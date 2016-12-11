unit VirtualFS;

{$include opts.inc}

interface

uses
	USystem, Errors, UClasses, UMath, Streams, Utils, Algo {$ifdef Debug}, ULog {$endif};

type
	pPack = ^Pack;
	Pack = object(&Object)
	type
		FileFlag  = (Compressed);
		FileFlags = set of FileFlag;

		ppFileDesc = ^pFileDesc;
		pFileDesc = ^FileDesc;
		FileDesc = record
			nName, nExt: sint;
			name, ext: pChar;
			flags: FileFlags;

			// Для сжатых файлов block задаёт индекс блока, offset — смещение в блоке.
			// Для несжатых: block не используется, offset — смещение в исходном файле.
			// size — всегда исходный размер файла.
			offset: FilePos;
			size: FileSize;
			block: sint;
		end;

		ppFolderDesc = ^pFolderDesc;
		pFolderDesc = ^FolderDesc;
		FolderDesc = record
			nName: sint;
			name: pChar;
			firstFolder, nFolders: sint;
			firstFile, nFiles: sint;
		end;

		BlockDesc = record
			offset: FilePos;
			size: FileSize;
		end;

		FolderFound = function(var v: Pack; var h: FolderDesc; param: pointer): boolean;
		FileFound = function(var v: Pack; var h: FileDesc; param: pointer): boolean;
	var
		stream: pStream;
		dataStart: FilePos;
		folders: array of FolderDesc;
		files: array of FileDesc;
		blocks: array of BlockDesc;
		naems, exts: string;

		constructor Init(f: pStream);
		destructor Done; virtual;
		function Root: pFolderDesc;
		function ToStringTree: StringTree;
		procedure Iterate(var folder: FolderDesc; onFolder: FolderFound; onFile: FileFound; param: pointer);
		procedure Lookup(var parent: FolderDesc; path: pChar; nPath: sint; folder: ppFolderDesc; vfile: ppFileDesc);
		procedure Lookup(var parent: FolderDesc; const path: string; folder: ppFolderDesc; vfile: ppFileDesc);
		function GetFileAttributes(const path: string): FileAttributes;
		function GetFileAttributes(path: pChar; nPath: sint): FileAttributes;
		function GetFile(var f: FileDesc): pStream;
		function GetFile(const path: string; flags: USystem.FileFlags): pStream;
		function GetFile(path: pChar; nPath: sint; flags: USystem.FileFlags): pStream;
		function FileName(var f: FileDesc): string;
		function FolderName(var f: FolderDesc): string;
	const
		Extension = 'pk';
		AutoreleaseTimeout = 4 * ResourcePool.DefaultTimeout;

	private
		procedure LookupOne(var parent: FolderDesc; name: pChar; nName: sint; folder: ppFolderDesc; vfile: ppFileDesc);
		procedure Deserialize(s: pStream);
	const
		Signature = 'ROFS' + EOL;
		EndOfHeader = EOL + '^_~' + EOL;

	public type
		ScanProc = function(const path: string; param: pointer): Folder.Enumerator;
		GetFileInfoProc = procedure(const path: string; out dataType: sint; param: pointer);
		GetStreamProc = function(const path: string; param: pointer): pStream;
		LogMessageProc = procedure(const msg: string; param: pointer);

		pStatistics = ^Statistics;
		Statistics = object
			nameStringsReused, nameCharsReused, extStringsReused, extCharsReused: sint;
			srcDataSize, compDataSize, compSrcDataSize, uncompSrcDataSize: FileSize;
			nFilesTotal, nFilesCompressed, nFilesUncompressed: sint;
			nBlocks, nBlockFiles: sint;
			srcBlocksSize, compBlocksSize: FileSize;
			z: array[ZStream.Method] of record
				nFiles: sint;
				src, compressed: FileSize;
			end;
		const
			Zero: Statistics =
			(
				nameStringsReused: 0; nameCharsReused: 0; extStringsReused: 0; extCharsReused: 0;
				srcDataSize: (value: 0); compDataSize: (value: 0); compSrcDataSize: (value: 0); uncompSrcDataSize: (value: 0);
				nFilesTotal: 0; nFilesCompressed: 0; nFilesUncompressed: 0;
				nBlocks: 0; nBlockFiles: 0; srcBlocksSize: (value: 0); compBlocksSize: (value: 0);
			{$define one := (nFiles: 0; src: (value: 0); compressed: (value: 0))}
				z: (one, one, one, one, one)
			{$undef one}
			);
		end;
	var
		function Pack(output: pStream; scan: ScanProc; getInfo: GetFileInfoProc; getStream: GetStreamProc; message: LogMessageProc; param: pointer): Statistics; static;
	end;

implementation

	constructor Pack.Init(f: pStream);
	begin
		inherited Init;
		stream := MakeRef(f);
		try
		{$ifdef Debug} LogR('Открываю VFS {0}... ', StreamPath.Log(f^.path)); {$endif}
			Deserialize(f);
			f^.DisableBuffering;
		{$ifdef Debug} Log('VFS {0} открыта, смещение данных {1}', StreamPath.Log(f^.path), ToString(dataStart), logOK); {$endif}
		except
			Done;
			raise;
		end;
	end;

	destructor Pack.Done;
	{$ifdef Debug} var strm: string; {$endif}
	begin
	{$ifdef Debug} strm := StreamPath.Log(stream^.path); LogR('Закрываю VFS {0}... ', strm); {$endif}
		Release(stream);
	{$ifdef Debug} Log('VFS {0} закрыта', strm, logOK); {$endif}
		inherited Done;
	end;

	function Pack.Root: pFolderDesc;
	begin
		result := @folders[0];
	end;

	function Pack.ToStringTree: StringTree;

		procedure Traverse(var folder: FolderDesc; var tree: StringTree);
		var
			i: sint;
		begin
			for i := folder.firstFolder to folder.firstFolder + folder.nFolders - 1 do
				Traverse(folders[i], tree.Add(USystem.ToString(folders[i].name, folders[i].nName))^);

			for i := folder.firstFile to folder.firstFile + folder.nFiles - 1 do
				tree.Add(USystem.ToString(files[i].name, files[i].nName));
		end;

	begin
		(@result)^.Init('');
		Traverse(folders[0], result);
	end;

	procedure Pack.Iterate(var folder: FolderDesc; onFolder: FolderFound; onFile: FileFound; param: pointer);
	var
		i: sint;
	begin
		if Assigned(onFolder) and (folder.nFolders > 0) then
			for i := folder.firstFolder to folder.firstFolder + folder.nFolders - 1 do
				if not onFolder(self, folders[i], param) then
					break;

		if Assigned(onFile) and (folder.nFiles > 0) then
			for i := folder.firstFile to folder.firstFile + folder.nFiles - 1 do
				if not onFile(self, files[i], param) then
					break;
	end;

	procedure Pack.Lookup(var parent: FolderDesc; path: pChar; nPath: sint; folder: ppFolderDesc; vfile: ppFileDesc);
	var
		i: sint;
		nf: pFolderDesc;
	begin
		if Assigned(folder) then folder^ := nil;
		if Assigned(vfile) then vfile^ := nil;

		if nPath = 0 then
		begin
			if Assigned(folder) then folder^ := @parent;
			exit;
		end;

		for i := 0 to nPath - 1 do
			if path[i] = FileSeparator then
			begin
				LookupOne(parent, path, i, @nf, nil);
				if Assigned(nf) then Lookup(nf^, path + i + 1, nPath - i - 1, folder, vfile);
				exit;
			end;
		LookupOne(parent, path, nPath, folder, vfile);
	end;

	procedure Pack.Lookup(var parent: FolderDesc; const path: string; folder: ppFolderDesc; vfile: ppFileDesc);
	begin
		Lookup(parent, pChar(path), length(path), folder, vfile);
	end;

	function Pack.GetFileAttributes(const path: string): FileAttributes;
	begin
		result := GetFileAttributes(pChar(path), length(path));
	end;

	function Pack.GetFileAttributes(path: pChar; nPath: sint): FileAttributes;
	var
		folder: pFolderDesc;
		vfile: pFileDesc;
	begin
		Lookup(Root^, path, nPath, @folder, @vfile);
		result := [];
		if Assigned(folder) then result += [file_Folder];
		if Assigned(vfile) then result += [file_JustFile];
	end;

	function Pack.GetFile(var f: FileDesc): pStream;
	var
		fn: string;
	{$ifdef Debug} t: Ticks; {$endif}
	begin
		fn := FileName(f);
		if Compressed in f.flags then
		begin
			result := new(pZStream, Init(stream^.Window(dataStart + blocks[f.block].offset, blocks[f.block].size, fn), [AllowAbandon]));
		{$ifdef Debug} if f.offset > FilePos.Zero then t := Ticks.Get; {$endif}
			result^.Skip(f.offset);
		{$ifdef Debug} if f.offset > FilePos.Zero then LogR('Время пропуска: ' + ToString(Ticks.Get - t) + '; ', logDebug); {$endif}

			result^.size := f.offset + f.size;
		{$ifdef Debug}
			Log('Сжатый файл {0} размером {1} в блоке {2} + {3}, смещение в блоке = {4}',
				fn, ToString(f.size), ToString(dataStart), ToString(blocks[f.block].offset), ToString(f.offset));
		{$endif}
		end else
		begin
			result := stream^.Window(dataStart + f.offset, f.size, fn);
		{$ifdef Debug} Log('Несжатый файл {0} по смещению {1} + {2} размером {3}', fn, ToString(dataStart), ToString(f.offset), ToString(f.size)); {$endif}
		end;
	end;

	function Pack.GetFile(const path: string; flags: USystem.FileFlags): pStream;
	begin
		result := GetFile(pChar(path), length(path), flags);
	end;

	function Pack.GetFile(path: pChar; nPath: sint; flags: USystem.FileFlags): pStream;
	var
		vfile: pFileDesc;
	begin
		Lookup(Root^, path, nPath, nil, @vfile);
		if Assigned(vfile) then result := GetFile(vfile^) else
			if file_JustTry in flags then result := nil else
				raise Error('VFS {0}: файл {1} не найден.', StreamPath.Human(stream^.path), StreamPath.Human(USystem.ToString(path, nPath)));
	end;

	function Pack.FileName(var f: FileDesc): string;
	begin
		result := USystem.ToString(f.name, f.nName);
		if f.nExt > 0 then result += ExtensionSeparator + USystem.ToString(f.ext, f.nExt);
	end;

	function Pack.FolderName(var f: FolderDesc): string;
	begin
		result := USystem.ToString(f.name, f.nName);
	end;

	procedure Pack.LookupOne(var parent: FolderDesc; name: pChar; nName: sint; folder: ppFolderDesc; vfile: ppFileDesc);
	var
		ext: pChar;
		nExt, i: sint;
	begin
		if Assigned(folder) then folder^ := nil;
		if Assigned(vfile) then vfile^ := nil;

		if Assigned(folder) then
			for i := parent.firstFolder to parent.firstFolder + parent.nFolders - 1 do
				if StrEq(folders[i].name, folders[i].nName, name, nName) then
				begin
					folder^ := @folders[i];
					exit;
				end;

		if Assigned(vfile) then
		begin
			nExt := 0; ext := nil;
			for i := nName - 1 downto 0 do
				if name[i] = ExtensionSeparator then
				begin
					ext := name + i + 1;
					nExt := nName - i - 1;
					nName := i;
					break;
				end;

			for i := parent.firstFile to parent.firstFile + parent.nFiles - 1 do
				if StrEq(files[i].name, files[i].nName, name, nName) and StrEq(files[i].ext, files[i].nExt, ext, nExt) then
				begin
					vfile^ := @files[i];
					break;
				end;
		end;
	end;

	procedure Pack.Deserialize(s: pStream);
		function ReadShift(n: uint; fmt: UiBinaryFormat; var len: uint): pChar;
		var
			t: PtrUint;
		begin
			if n = 0 then exit;
			t := Deserialize_ui(s, fmt);
			pPtrUint(@result)^ := t;
			if t + n > len then len := t + n;
		end;

		function UiBinaryFormat(fmts: uint32; start, count: uint): UiBinaryFormat;
		begin
			result := Streams.UiBinaryFormat(RangeCheck(bits(fmts, start, count), ord(High(result)), 'UiFormat'));
		end;
	var
		i: sint;
		nChildsFmt, nNameFmt, nameshFmt, nExtFmt, extshFmt, offsetFmt, fileSizeFmt, blockSizeFmt, blockIndexFmt: UiBinaryFormat;
		nNaems, nExts: uint;
		fmts: uint32;
	begin
		nNaems := 0;
		nExts := 0;

		Deserialize_signature(s, Signature, no);
		fmts := Deserialize_ui32(s);
		nChildsFmt := UiBinaryFormat(fmts, 0, 3);
		nNameFmt   := UiBinaryFormat(fmts, 3, 3);
		nameshFmt  := UiBinaryFormat(fmts, 6, 3);
		nExtFmt    := UiBinaryFormat(fmts, 9, 3);
		extshFmt   := UiBinaryFormat(fmts, 12, 3);
		offsetFmt  := UiBinaryFormat(fmts, 15, 3);
		fileSizeFmt := UiBinaryFormat(fmts, 18, 3);
		blockSizeFmt := UiBinaryFormat(fmts, 21, 3);
		blockIndexFmt := UiBinaryFormat(fmts, 24, 3);

		SetLength(folders, 1 + VarInt.Read(s));
		for i := 0 to High(folders) do
		begin
			if i > 0 then
			begin
				folders[i].nName := Deserialize_ui(s, nNameFmt); folders[i].name := ReadShift(folders[i].nName, nameshFmt, nNaems);
				folders[i].firstFolder := folders[i - 1].firstFolder + folders[i - 1].nFolders;
				folders[i].firstFile := folders[i - 1].firstFile + folders[i - 1].nFiles;
			end else
			begin
				folders[i].nName := 0;
				folders[0].firstFolder := 1;
				folders[0].firstFile := 0;
			end;

			folders[i].nFolders := Deserialize_ui(s, nChildsFmt);
			folders[i].nFiles := Deserialize_ui(s, nChildsFmt);
		end;

		SetLength(blocks, VarInt.Read(s));
		for i := 0 to High(blocks) do
		begin
			blocks[i].offset := FilePos.Explicit(Deserialize_ui(s, offsetFmt));
			blocks[i].size := FileSize.Explicit(Deserialize_ui(s, blockSizeFmt));
		end;

		SetLength(files, VarInt.Read(s));
		for i := 0 to High(files) do
		begin
			files[i].flags := [];
			files[i].nName := Deserialize_ui(s, nNameFmt); files[i].name := ReadShift(files[i].nName, nameshFmt, nNaems);
			files[i].nExt  := Deserialize_ui(s, nExtFmt);  files[i].ext  := ReadShift(files[i].nExt,  extshFmt,  nExts);
			files[i].size := FileSize.Explicit(Deserialize_ui(s, fileSizeFmt));
			files[i].offset := FilePos.Explicit(Deserialize_ui(s, offsetFmt));
			files[i].block := sint(Deserialize_ui(s, blockIndexFmt)) - 1;
			if files[i].block >= 0 then files[i].flags += [Compressed];
		end;

		naems := Deserialize_conststring(s, nNaems);
		exts := Deserialize_conststring(s, nExts);
		Deserialize_signature(s, EndOfHeader, no);

		for i := 0 to High(folders) do
			if folders[i].nName > 0 then folders[i].name := pChar(naems) + pPtrUint(@folders[i].name)^;

		for i := 0 to High(files) do
		begin
			if files[i].nName > 0 then files[i].name := pChar(naems) + pPtrUint(@files[i].name)^;
			if files[i].nExt > 0 then files[i].ext := pChar(exts) + pPtrUint(@files[i].ext)^;
		end;
		dataStart := s^.Position;
	end;

type
	GetString = function(id: sint; param: pointer): string;
	SetStringStart = procedure(id: sint; start: size_t; param: pointer);
	NotifyStringReuse = procedure(strs, chars: sint; param: pointer);

	function PackStrings(count: sint; getString: GetString; setStart: SetStringStart; notifyReused: NotifyStringReuse; param: pointer): string;
	type
		tElem = record
			id: sint;
			s: string;
		end;

		function CommonAffix(const a, b: string): sint;
		var
			i, last: sint;
		begin
			last := min(length(a), length(b));
			for i := 1 to last + 1 do
				if (i > last) or not StrEq(pChar(a) + length(a) - i, pChar(b), i) then
					exit(i - 1);
			Assert(no);
		end;

		function HeuristicalLess(const a, b: string): boolean;
		var
			adif: sint;
		begin
			adif := CommonAffix(a, b) - CommonAffix(b, a);
			result := (adif > 0) or ((adif = 0) and (length(a) > length(b))) or ((adif = 0) and (length(a) = length(b)) and (a < b));
		end;

		{$define elem := tElem} {$define less := HeuristicalLess(_1.s, _2.s)} {$define openarray} {$define less_is_consciously_intransitive}
		{$include sort.inc}
	var
		e: array of tElem;
		i, p, reuse: sint;
	begin
		SetLength(e, count);
		for i := 0 to count - 1 do
		begin
			e[i].id := i;
			e[i].s := getString(i, param);
		end;
		sort(e);

		result := '';
		for i := 0 to count - 1 do
		begin
			p := Pos(e[i].s, result);
			if p > 0 then
			begin
				if Assigned(notifyReused) then notifyReused(1, 0, param);
				setStart(e[i].id, p - 1, param);
				continue;
			end;

			reuse := min(length(result), length(e[i].s));
			repeat
				if StrEq(pChar(result) + length(result) - reuse, pChar(e[i].s), reuse) then
				begin
					if Assigned(notifyReused) then notifyReused(0, reuse, param);
					p := length(result);
					SetLength(result, p + length(e[i].s) - reuse);
					memcpy(pChar(e[i].s) + reuse, pChar(result) + p, (length(e[i].s) - reuse) * sizeof(char));
					setStart(e[i].id, p - reuse, param);
					break;
				end;
				Assert(reuse > 0);
				dec(reuse);
			until no;
		end;
	end;

type
	pFlatFolder = ^FlatFolder;
	FlatFolder = record
		name: string;
		namesh: sint;
		firstFolder, nFolders: sint;
		firstFile, nFiles: sint;
	end;

	pFlatFile = ^FlatFile;
	FlatFile = object
		path: string;
		name, ext: string;
		namesh, extsh: sint;
		size: FileSize;
		dataType: sint;
		offset: FilePos;
		block: sint;
	end;

	pBlock = ^Block;
	Block = record
		sumSize: FileSize;
		minFileID: sint;
		voffset: FilePos;
		vsize: FileSize;
		files: array of pFlatFile;
	end;
	pBlocks = ^Blocks;
	Blocks = array of Block;

	pFlat = ^Flat;
	Flat = object
	type
		BuildQueueItem = object
			folderId: sint;
			path: string;
			function Make(newFolderId: sint; const newPath: string): BuildQueueItem; static;
		end;
	{$define classname := tBuildQueue} {$define item_type := BuildQueueItem} {$include queue.h.inc}
	var
		folders: array of FlatFolder;
		files: array of FlatFile;
		naems, exts: string;
		function Build(scan: Pack.ScanProc; getInfo: Pack.GetFileInfoProc; param: pointer; var stat: Pack.Statistics): Flat; static;
		procedure Build(folderId: sint; const path: string; scan: Pack.ScanProc; getInfo: Pack.GetFileInfoProc; param: pointer;
		                var qu: tBuildQueue; var stat: Pack.Statistics);
		function EstimateBlocks: Blocks;
	end;

type
	pPackParam = ^PackParam;
	PackParam = record
		f: pFlat;
		stat: Pack.pStatistics;
	end;

	function GetName(id: sint; p: pPackParam): string;
	begin
		if id < length(p^.f^.folders) then
			result := p^.f^.folders[id].name
		else
			result := p^.f^.files[id - length(p^.f^.folders)].name;
	end;

	procedure SetNameSh(id: sint; start: size_t; p: pPackParam);
	begin
		if id < length(p^.f^.folders) then
			p^.f^.folders[id].namesh := start
		else
			p^.f^.files[id - length(p^.f^.folders)].namesh := start;
	end;

	procedure NotifyNameReused(strs, chars: sint; p: pPackParam);
	begin
		p^.stat^.nameStringsReused += strs;
		p^.stat^.nameCharsReused += chars;
	end;

	function GetExt(id: sint; p: pPackParam): string;
	begin
		result := p^.f^.files[id].ext;
	end;

	procedure SetExtSh(id: sint; start: size_t; p: pPackParam);
	begin
		p^.f^.files[id].extsh := start;
	end;

	procedure NotifyExtReused(strs, chars: sint; p: pPackParam);
	begin
		p^.stat^.extStringsReused += strs;
		p^.stat^.extCharsReused += chars;
	end;

	function Flat.Build(scan: Pack.ScanProc; getInfo: Pack.GetFileInfoProc; param: pointer; var stat: Pack.Statistics): Flat;
	var
		flat: Flat absolute result;
		pp: PackParam;
		qu: tBuildQueue;
		item: BuildQueueItem;
	begin
		SetLength(flat.folders, 1);
		qu.Init;
		qu.Put(BuildQueueItem.Make(0, ''));
		while qu.Get(item) do
			flat.Build(item.folderId, item.path, scan, getInfo, param, qu, stat);
		qu.Done;

		pp.f    := @flat;
		pp.stat := @stat;
		flat.naems := PackStrings(length(flat.folders) + length(flat.files), GetString(@GetName), SetStringStart(@SetNameSh), NotifyStringReuse(@NotifyNameReused), @pp);
		flat.exts := PackStrings(length(flat.files), GetString(@GetExt), SetStringStart(@SetExtSh), NotifyStringReuse(@NotifyExtReused), @pp);
	end;

	function Flat.BuildQueueItem.Make(newFolderId: sint; const newPath: string): BuildQueueItem;
	begin
		result.folderId := newFolderId;
		result.path := newPath;
	end;

{$define classname := Flat.tBuildQueue} {$include queue.pp.inc}

	procedure Flat.Build(folderId: sint; const path: string; scan: Pack.ScanProc; getInfo: Pack.GetFileInfoProc; param: pointer;
	                      var qu: tBuildQueue; var stat: Pack.Statistics);
	var
		f: FoundFile;
		i: sint;
		folder: pFlatFolder;
		&file: pFlatFile;
	begin
		folder := @folders[folderId];
		folder^.firstFolder := length(folders);
		folder^.firstFile   := length(files);
		folder^.nFolders    := 0;
		folder^.nFiles      := 0;
		for f in scan(path, param) do
		begin
			if f.IsFolder then
			begin
				SetLength(folders, length(folders) + 1);
				folder := @folders[folderId];
				folders[folder^.firstFolder + folder^.nFolders].name := f.name;
				inc(folder^.nFolders);
			end
			else if f.IsFile then
			begin
				SetLength(files, length(files) + 1);
				&file := @files[High(files)];
				&file^.path := StreamPath.Navigate(f.name, path);
				&file^.name := StreamPath.CutExtension(f.name, @&file^.ext);
				&file^.size := f.size;
				stat.srcDataSize += &file^.size;
				if Assigned(getInfo) then getInfo(&file^.path, &file^.dataType, param) else &file^.dataType := -1;
				inc(folder^.nFiles);
			end;
		end;

		for i := folder^.firstFolder to folder^.firstFolder + folder^.nFolders - 1 do
			qu.Put(BuildQueueItem.Make(i, StreamPath.ForcePath(path) + folders[i].name));
	end;

	function Flat.EstimateBlocks: Blocks;
	const
		MaxBlockSize = 128 * 1024;
		MaxFileSize = (3 * MaxBlockSize) div 2;

		{$define procname := SortByTypeAndSize} {$define elem := pFlatFile}
			{$define less := (_1^.dataType < _2^.dataType) or ((_1^.dataType = _2^.dataType) and (_1^.size < _2^.size))} {$define openarray} {$include sort.inc}

		{$define procname := SortByMinFileID} {$define elem := Block}
			{$define less := _1.minFileID < _2.minFileID} {$define openarray} {$include sort.inc}

	var
		sorted: array of pFlatFile;
		i, fid: sint;
		cb: pBlock;
	begin
		SetLength(sorted, length(files));
		for i := 0 to High(files) do
			sorted[i] := @files[i];
		SortByTypeAndSize(sorted);

		result := nil;
		for i := 0 to High(sorted) do
		begin
			cb := nil;
			if sorted[i]^.size.value <= MaxFileSize then
				if (length(result) > 0) and (result[High(result)].files[0]^.dataType = sorted[i]^.dataType)
					and ((result[High(result)].sumSize + files[i].size).value <= MaxBlockSize)
				then
					cb := @result[High(result)];

			if not Assigned(cb) then
			begin
				SetLength(result, length(result) + 1);
				cb := @result[High(result)];
				cb^.sumSize := FileSize.Zero;
			end;

			SetLength(cb^.files, length(cb^.files) + 1);
			cb^.files[High(cb^.files)] := sorted[i];
			cb^.sumSize += sorted[i]^.size;
			fid := sorted[i] - pFlatFile(files);
			if (length(cb^.files) = 1) or (fid < cb^.minFileID) then cb^.minFileID := fid;
		end;
		SortByMinFileID(result);
	end;

type
	pPacker = ^Packer;
	Packer = object
		flat: Flat;
		getStream: Pack.GetStreamProc;
		message: Pack.LogMessageProc;
		param: pointer;
		tempFileSep: pStream;
		tempFileForMethod: array[ZStream.Method] of pStream;
		function Create(const tempBase: string; const newFlat: Flat; newGetStream: Pack.GetStreamProc;
		                newMessage: Pack.LogMessageProc; newParam: pointer): Packer; static;
		procedure Done;
		procedure Pack(output: pStream; var stat: Pack.Statistics);
		procedure Note(const what: string);
	{$define func:=procedure Note(const fmt: string; const _ARGS_: string);} {$include variadic.inc}
		function Reopen(var f: FlatFile): pStream;
		function Percentage(const src, compressed: FileSize; zm: sint): string;
		function TrySeparate(const streams: array of pStream; method: ZStream.Method): FileSize;
		function DoAtYourBest(const streams: array of pStream; const summarySize: FileSize; target: pStream; out method: ZStream.Method): boolean;
		function TryCompress(const files: array of pFlatFile; target: pStream; orWriteUncompressed: boolean; var stat: Pack.Statistics): boolean;
		procedure WriteHeader(target: pStream; const blocks: Blocks);
	end;

	function Packer.Create(const tempBase: string; const newFlat: Flat; newGetStream: Pack.GetStreamProc; newMessage: Pack.LogMessageProc;
	                       newParam: pointer): Packer;
	var
		z: ZStream.Method;
	begin
		result.flat    := newFlat;
		result.getStream := newGetStream;
		result.message := newMessage;
		result.param   := newParam;
		result.tempFileSep := nil;
		for z in ZStream.Method do
			result.tempFileForMethod[z] := nil;

		try
			result.tempFileSep := FileStream.CreateTemp(StreamPath.Relative('vfs', tempBase));
			for z in ZStream.Method do
				result.tempFileForMethod[z] := FileStream.CreateTemp(StreamPath.Relative('vfs', tempBase));
		except
			result.Done;
			raise;
		end;
	end;

	procedure Packer.Done;
	var
		z: ZStream.Method;
	begin
		for z in ZStream.Method do
			Release(tempFileForMethod[z]);
		Release(tempFileSep);
		System.Finalize(flat);
	end;

	procedure Packer.Note(const what: string);
	begin
		if Assigned(message) then message(what, param);
	end;

{$define func:=
	procedure Packer.Note(const fmt: string; const _ARGS_: string);
	begin
		if Assigned(message) then message(Format(fmt, _ARGS_), param);
	end;} {$include variadic.inc}

	function Packer.Reopen(var f: FlatFile): pStream;
	begin
		result := getStream(f.path, param)^.NewRef;
		if result^.size <> f.size then
		begin
			Release(result);
			raise Error('Файл {0} не такой, каким я его запомнил.', StreamPath.Human(f.path));
		end;
	end;

const
	MethodEstimation: array[ZStream.Method] of float = (1.0, 0.85, 1.1, 1.05, 0.83);

	function Packer.Percentage(const src, compressed: FileSize; zm: sint): string;
	begin
		if (src = FileSize.Zero) or (compressed = FileSize.Zero) then
			result := Format('{0} -> {1}', ToString(src), ToString(compressed))
		else
			if src = compressed then result := Format('{0} -> размер не изменился', ToString(src)) else
				if src > compressed then
					if (zm >= 0) and (MethodEstimation[ZStream.Method(zm)] <> 1.0) then
						result := Format('{0} -> {1}, {2}x, с поправкой — {3}x', ToString(src), ToString(compressed),
						                 ToString(src / compressed), ToString(src / compressed / MethodEstimation[ZStream.Method(zm)]))
					else
						result := Format('{0} -> {1}, {2}x', ToString(src), ToString(compressed), ToString(src / compressed))
				else
					result := Format('{0} -> {1}, +{2}%', ToString(src), ToString(compressed), ToString((compressed - src) / src * 100.0));
	end;

	function Packer.TrySeparate(const streams: array of pStream; method: ZStream.Method): FileSize;
	var
		tmp, tz: pStream;
		i: sint;
	begin
		result := FileSize.Zero;
		tmp := MakeRef(tempFileSep);
		try
			for i := 0 to High(streams) do
			begin
				tmp^.Position := FilePos.Zero;
				tz := MakeRef(new(pZStream, Init(tmp, method)));
				try
					streams[i]^.Position := FilePos.Zero;
					tz^.AppendFrom(streams[i]);
				finally
					Release(tz);
				end;
				result += tmp^.Position;
			end;
		finally
			tmp^.Size := FileSize.Zero;
			Release(tmp);
		end;
	end;

	function Packer.DoAtYourBest(const streams: array of pStream; const summarySize: FileSize; target: pStream; out method: ZStream.Method): boolean;
	const
		MaxRatio   = 0.925;
		MaxBlockToSeparateRatioBase = 0.95;
		MaxBlockToSeparateRatioExpMin = 0.8;
		ExpK = 0.03;
	var
		tmp: array[ZStream.Method] of pStream;
		compressedSize, cssep: FileSize;
		ratio, bestRatio, maxBlockToSeparateRatio: float;
		zs: pStream;
		zm: ZStream.Method;
		i: sint;
		size: FileSize;
	begin
		result := no;
		if summarySize = FileSize.Zero then
		begin
			Note('Да он пустой лол.' + EOL);
			exit;
		end;
		bestRatio := 0.0;

		for zm in ZStream.Method do
			tmp[zm] := MakeRef(tempFileForMethod[zm]);
		try
			for zm in ZStream.Method do
			begin
				tmp[zm]^.Position := FilePos.Zero;
				zs := MakeRef(new(pZStream, Init(tmp[zm], zm)));
				try
					for i := 0 to High(streams) do
					begin
						streams[i]^.Position := FilePos.Zero;
						zs^.AppendFrom(streams[i]);
					end;
				finally
					Release(zs);
				end;

				compressedSize := tmp[zm]^.Position;
				if compressedSize.value >= summarySize.value then
				begin
					Note('{0}: сжатие не дало эффекта ({1})', ZStream.MethodIds[zm], Percentage(summarySize, compressedSize, -1));
					continue;
				end;

				if compressedSize / summarySize >= MaxRatio then
				begin
					Note('{0}: сжатие недостаточно ({1})', ZStream.MethodIds[zm], Percentage(summarySize, compressedSize, -1));
					continue;
				end;

				ratio := compressedSize / summarySize * MethodEstimation[zm];
				if ratio > 1.0 then
				begin
					Note('{0}: сжатие посчитано неэффективным ({1})', ZStream.MethodIds[zm], Percentage(summarySize, compressedSize, ord(zm)));
					continue;
				end;

				if result and (ratio >= bestRatio) then
				begin
					Note('{0}: {1}; отброшено', ZStream.MethodIds[zm], Percentage(summarySize, compressedSize, ord(zm)));
					continue;
				end;

				if length(streams) > 1 then
				begin
					maxBlockToSeparateRatio := MaxBlockToSeparateRatioExpMin + (MaxBlockToSeparateRatioBase - MaxBlockToSeparateRatioExpMin) * exp(ExpK * (2 - length(streams)));
					cssep := TrySeparate(streams, zm);
					if compressedSize / cssep > maxBlockToSeparateRatio then
					begin
						Note('{0}: сжатие блоком недостаточно эффективно ({1}) (ожидается не менее {2}x)', ZStream.MethodIds[zm], Percentage(cssep, compressedSize, -1), ToString(1.0 / maxBlockToSeparateRatio));
						continue;
					end else
						Note('{0}: блоком эффективнее ({1})', ZStream.MethodIds[zm], Percentage(cssep, compressedSize, -1));
				end;

				result := yes;
				method := zm;
				bestRatio := ratio;
				Note('{0}: {1}; принято', ZStream.MethodIds[zm], Percentage(summarySize, compressedSize, ord(zm)));
			end;

			if result then
			begin
				size := tmp[method]^.Position;
				tmp[method]^.Position := FilePos.Zero;
				target^.AppendFrom(tmp[method], size);
				Note('Вердикт: {0}', ZStream.MethodIds[method]);
			end else
				Note('Вердикт: без сжатия');
			Note('');
		finally
			for zm in ZStream.Method do
			begin
				tmp[zm]^.Size := FileSize.Zero;
				Release(tmp[zm]);
			end;
		end;
	end;

	function Packer.TryCompress(const files: array of pFlatFile; target: pStream; orWriteUncompressed: boolean; var stat: Pack.Statistics): boolean;
	const
		MaxPrettyFiles = 5;
	var
		hb: string;
		streams: array of pStream;
		i: sint;
		method: ZStream.Method;
		summarySize, compSize: FileSize;
		start: FilePos;
	begin
		summarySize := FileSize.Zero;
		for i := 0 to High(files) do
			summarySize += files[i]^.size;

		if length(files) > 1 then
		begin
			hb := '';
			if length(files) <= MaxPrettyFiles then
				for i := 0 to High(files) do
				begin
					if i > 0 then hb += ', ';
					hb += StreamPath.Human(files[i]^.path);
				end
			else
			begin
				for i := 0 to MaxPrettyFiles div 2 - 1 do
				begin
					if i > 0 then hb += ', ';
					hb += StreamPath.Human(files[i]^.path);
				end;
				hb += ', ...';
				for i := length(files) - (MaxPrettyFiles + 1) div 2 to High(files) do
					hb += ', ' + StreamPath.Human(files[i]^.path);
			end;
			Note('Попытка сжатия блока ({0}) {{ {1} }...', ToString(length(files)), hb);
		end else
			Note('Попытка сжатия файла ' + StreamPath.Human(files[0]^.path) + '...');
		Note('');

		try
			SetLength(streams, length(files));
			for i := 0 to High(streams) do streams[i] := nil;
			for i := 0 to High(files) do streams[i] := Reopen(files[i]^);
			start := target^.Position;
			result := DoAtYourBest(streams, summarySize, target, method);
			if result then
			begin
				compSize := target^.Position - start;
				start := FilePos.Zero;
				for i := 0 to High(files) do
				begin
					files[i]^.offset := start;
					start += files[i]^.size;
				end;
				stat.compSrcDataSize += summarySize;
				stat.compDataSize += compSize;
				stat.z[method].nFiles += length(streams);
				stat.z[method].src += summarySize;
				stat.z[method].compressed += compSize;

				stat.nFilesTotal += length(files);
				stat.nFilesCompressed += length(files);
				if length(files) > 1 then
				begin
					inc(stat.nBlocks);
					stat.nBlockFiles += length(files);
					stat.srcBlocksSize += summarySize;
					stat.compBlocksSize += compSize;
				end;
			end else
				if orWriteUncompressed then
				begin
					stat.nFilesUncompressed += length(files);
					stat.uncompSrcDataSize += summarySize;
					for i := 0 to High(streams) do
					begin
						streams[i]^.Position := FilePos.Zero;
						target^.AppendFrom(streams[i]);
					end;
				end;
		finally
			for i := 0 to High(streams) do
				Release(streams[i]);
		end;
	end;

type
	pEstimateParam = ^EstimateParam;
	EstimateParam = record
		p: pPacker;
		b: pBlocks;
	end;

	function EstimateChildId(id: uint; param: pointer): uint;
	var
		ff: pFlatFolder absolute param;
	begin
		result := max(ff[id].nFolders, ff[id].nFiles);
	end;

	function EstimateNameLen(id: uint; param: pointer): uint;
	var
		f: pFlat absolute param;
	begin
		if id < uint(length(f^.folders)) then exit(length(f^.folders[id].name)) else id -= uint(length(f^.folders));
		result := length(f^.files[id].name);
	end;

	function EstimateNameShift(id: uint; param: pointer): uint;
	var
		f: pFlat absolute param;
	begin
		if id < uint(length(f^.folders)) then exit(f^.folders[id].namesh) else id -= uint(length(f^.folders));
		result := f^.files[id].namesh;
	end;

	function EstimateExtLen(id: uint; param: pointer): uint;
	var
		f: pFlatFile absolute param;
	begin
		result := length(f[id].ext);
	end;

	function EstimateExtShift(id: uint; param: pointer): uint;
	var
		f: pFlatFile absolute param;
	begin
		result := f[id].extsh;
	end;

	function EstimateOffset(id: uint; param: pointer): FilePos.ValueType;
	var
		p: pEstimateParam absolute param;
	begin
		if id < uint(length(p^.p^.flat.files)) then
			result := p^.p^.flat.files[id].offset.value
		else
		begin
			id -= uint(length(p^.p^.flat.files));
			result := p^.b^[id].voffset.value;
		end;
	end;

	function EstimateFileSize(id: uint; param: pointer): FileSize.ValueType;
	var
		f: pFlatFile absolute param;
	begin
		result := f[id].size.value;
	end;

	function EstimateBlockSize(id: uint; param: pointer): FileSize.ValueType;
	var
		f: pBlock absolute param;
	begin
		result := f[id].vsize.value;
	end;

	function EstimateBlockIndex(id: uint; param: pointer): uint;
	var
		f: pFlatFile absolute param;
	begin
		result := 1 + f[id].block;
	end;

	procedure Packer.WriteHeader(target: pStream; const blocks: Blocks);
	var
		i: sint;
		ep: EstimateParam;
		nChildsFmt, nNameFmt, nameshFmt, nExtFmt, extshFmt, offsetFmt, fileSizeFmt, blockSizeFmt, blockIndexFmt: UiBinaryFormat;
	begin
		Serialize_conststring(target, VirtualFS.Pack.Signature);
		ep.p := @self;
		ep.b := @blocks;
		nChildsFmt := UiBinaryFormatChooser.Choose(length(flat.folders), @EstimateChildId, pFlatFolder(flat.folders), 'количеств файлов и подпапок');
		nNameFmt := UiBinaryFormatChooser.Choose(length(flat.folders) + length(flat.files), @EstimateNameLen, @flat, 'длин имён');
		nameshFmt := UiBinaryFormatChooser.Choose(length(flat.folders) + length(flat.files), @EstimateNameShift, @flat, 'смещений имён');
		nExtFmt := UiBinaryFormatChooser.Choose(length(flat.files), @EstimateExtLen, pFlatFile(flat.files), 'длин расширений');
		extshFmt := UiBinaryFormatChooser.Choose(length(flat.files), @EstimateExtShift, pFlatFile(flat.files), 'смещений расширений');
		offsetFmt := UiBinaryFormatChooser.Choose(length(flat.files) + length(blocks), @EstimateOffset, @ep, 'смещений данных');
		fileSizeFmt := UiBinaryFormatChooser.Choose(length(flat.files), @EstimateFileSize, pFlatFile(flat.files), 'размеров файлов');
		blockSizeFmt := UiBinaryFormatChooser.Choose(length(blocks), @EstimateBlockSize, pBlock(blocks), 'размеров блоков');
		blockIndexFmt := UiBinaryFormatChooser.Choose(length(flat.files), @EstimateBlockIndex, pFlatFile(flat.files), 'индексов блоков');
		Note('Выбранные форматы:' + EOL +
			'  количества файлов и подпапок — {0}' + EOL +
			'  длины имён — {1}' + EOL +
			'  смещения имён — {2}' + EOL +
			'  длины расширений — {3}' + EOL +
			'  смещения расширений — {4}' + EOL +
			'  смещения данных — {5}' + EOL +
			'  размеры файлов — {6}' + EOL +
			'  размеры блоков — {7}' + EOL +
			'  индексы блоков — {8}', UiBinaryFormatIds[nChildsFmt], UiBinaryFormatIds[nNameFmt], UiBinaryFormatIds[nameshFmt],
			UiBinaryFormatIds[nExtFmt], UiBinaryFormatIds[extshFmt], UiBinaryFormatIds[offsetFmt], UiBinaryFormatIds[fileSizeFmt],
			UiBinaryFormatIds[blockSizeFmt], UiBinaryFormatIds[blockIndexFmt]);

		Serialize_ui32(target, bitpack(
			[ord(nChildsFmt), 3, ord(nNameFmt), 3, ord(nameshFmt), 3, ord(nExtFmt), 3, ord(extshFmt), 3, ord(offsetFmt), 3, ord(fileSizeFmt), 3,
			 ord(blockSizeFmt), 3, ord(blockIndexFmt), 3]));

		VarInt.Write(target, length(flat.folders) - 1);
		for i := 0 to High(flat.folders) do
		begin
			if i > 0 then
			begin
				Serialize_ui(target, length(flat.folders[i].name), nNameFmt);
				if flat.folders[i].name <> '' then Serialize_ui(target, flat.folders[i].namesh, nameshFmt);

				if flat.folders[i].firstFolder <> flat.folders[i - 1].firstFolder + flat.folders[i - 1].nFolders then
					raise Error('Ошибка перепроверки: firstFolder = {0} ({1}) <> ({2} + {3} + 1)',
						ToString(flat.folders[i].firstFolder), flat.folders[i].name, ToString(flat.folders[i - 1].firstFolder), ToString(flat.folders[i - 1].nFolders));

				if flat.folders[i].firstFile <> flat.folders[i - 1].firstFile + flat.folders[i - 1].nFiles then
					raise Error('Ошибка перепроверки: firstFile = {0} <> ({1} + {2} + 1)',
						ToString(flat.folders[i].firstFile), ToString(flat.folders[i - 1].firstFile), ToString(flat.folders[i - 1].nFiles));
			end;
			Serialize_ui(target, flat.folders[i].nFolders, nChildsFmt);
			Serialize_ui(target, flat.folders[i].nFiles, nChildsFmt);
		end;

		VarInt.Write(target, length(blocks));
		for i := 0 to High(blocks) do
		begin
			Serialize_ui(target, blocks[i].voffset.value, offsetFmt);
			Serialize_ui(target, blocks[i].vsize.value, blockSizeFmt);
		end;

		VarInt.Write(target, length(flat.files));
		for i := 0 to High(flat.files) do
		begin
			Serialize_ui(target, length(flat.files[i].name), nNameFmt); if flat.files[i].name <> '' then Serialize_ui(target, flat.files[i].namesh, nameshFmt);
			Serialize_ui(target, length(flat.files[i].ext), nExtFmt); if flat.files[i].ext <> '' then Serialize_ui(target, flat.files[i].extsh, extshFmt);
			Serialize_ui(target, flat.files[i].size.value, fileSizeFmt);
			Serialize_ui(target, flat.files[i].offset.value, offsetFmt);
			Serialize_ui(target, 1 + flat.files[i].block, blockIndexFmt);
		end;

		Serialize_conststring(target, flat.naems);
		Serialize_conststring(target, flat.exts);
		Serialize_conststring(target, VirtualFS.Pack.EndOfHeader);
	end;

	procedure Packer.Pack(output: pStream; var stat: Pack.Statistics);

		{$define procname := SortByFileID} {$define elem := pFlatFile} {$define less := _1 - pFlatFile(flat.files) < _2 - pFlatFile(flat.files)} {$define openarray}
		{$include sort.inc}

	var
		b: Blocks;
		realBlocks: Blocks;
		byOne, realOnes: array of pFlatFile;
		cb: pBlock;
		cf: pFlatFile;
		data: pStream;
		i, j, start: sint;
		fstart: FilePos;
	begin
		b := flat.EstimateBlocks;
		realBlocks := nil;
		byOne := nil;
		realOnes := nil;

		data := FileStream.CreateTemp(StreamPath.Relative('vfs', output^.path));
		try
			// Сначала пытаемся сжать блоки из более чем одного файла.
			// Те, для которых это не даёт нужного эффекта, разделяем на отдельные файлы и обрабатываем вместе с остальными позже.
			for i := 0 to High(b) do
			begin
				fstart := data^.Position;
				if (length(b[i].files) > 1) and TryCompress(b[i].files, data, no, stat) then
				begin
					SetLength(realBlocks, length(realBlocks) + 1);
					cb := @realBlocks[High(realBlocks)];
					cb^ := b[i];
					cb^.voffset := fstart;
					cb^.vsize   := data^.Position - fstart;
				end else
				begin
					start := length(byOne);
					SetLength(byOne, length(byOne) + length(b[i].files));
					for j := 0 to High(b[i].files) do
						byOne[start + j] := b[i].files[j];
				end;
			end;

			// Теперь обрабатываем оставшиеся файлы по одному, предварительно отсортировав по ID (они могли сбиться после разделения блоков) ради большей локальности.
			SortByFileID(byOne);
			for i := 0 to High(byOne) do
			begin
				fstart := data^.Position;
				if TryCompress(byOne[i], data, yes, stat) then
				begin
					SetLength(realBlocks, length(realBlocks) + 1);
					cb := @realBlocks[High(realBlocks)];
					cb^.sumSize := byOne[i]^.size;
					cb^.voffset := fstart;
					cb^.vsize   := data^.Position - fstart;
					SetLength(cb^.files, 1);
					cb^.files[0] := byOne[i];
				end else
				begin
					SetLength(realOnes, length(realOnes) + 1);
					realOnes[High(realOnes)] := byOne[i];
					cf := byOne[i];
					cf^.offset := fstart;
					Assert(cf^.size = data^.Position - fstart, 'что-то не так, размер файла не совпал с записанным в несжатом виде');
				end;
			end;

			// Выставляем индексы блоков.
			for i := 0 to High(realBlocks) do
				for j := 0 to High(realBlocks[i].files) do
					realBlocks[i].files[j]^.block := i;
			for i := 0 to High(realOnes) do
				realOnes[i]^.block := -1;

			Assert(data^.Position = data^.Size, 'что-то не так с сырым упакованным файлом');
			Note('Размер данных архива: ' + ToString(data^.Size));
			Note('Пишу заголовок...');
			fstart := output^.Position;
			WriteHeader(output, realBlocks);
			Note('Размер заголовка: ' + ToString(output^.Position - fstart));

			Note('Пишу данные...');
			data^.Position := FilePos.Zero;
			output^.AppendFrom(data);
			Note('OK!');
			Note('');
		finally
			Release(data);
		end;
	end;

	function Pack.Pack(output: pStream; scan: ScanProc; getInfo: GetFileInfoProc; getStream: GetStreamProc; message: LogMessageProc; param: pointer): Statistics;
	var
		flat: VirtualFS.Flat;
		packer: VirtualFS.Packer;
	begin
		result := Statistics.Zero;
		flat   := Flat.Build(scan, getInfo, param, result);
		packer := Packer.Create(output^.path, flat, getStream, message, param);
		try
			packer.Pack(output, result);
		finally
			packer.Done;
		end;
	end;

	function LoadVirtualFS(s: pStream): pObject; begin result := new(pPack, Init(s)); end;

	procedure Init;
	begin
		ResourcePool.Shared^.Register(TypeOf(Pack), @LoadVirtualFS)^.Flags([file_RandomAccess])^.Timeout(Pack.AutoreleaseTimeout);
	end;

initialization
	&Unit('VirtualFS').Initialize(@Init);
end.
