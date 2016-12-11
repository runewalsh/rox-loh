{$include opts.inc}
unit BlobCaches;

interface

uses
	USystem, Errors, Streams, Algo, UMath, Utils {$ifdef Debug}, ULog {$endif};

type
	pBlobCache = ^BlobCache;
	BlobCache = object
	type
		procedure Open(out bc: BlobCache; const fn: string); static; // при неудаче — записывает Invalid И бросает исключение
		procedure Close;
		procedure Drop;
		function OK: boolean;
		function Invalid: BlobCache; static;

	type
		BlockData = object
			ptr: pointer;
			size: size_t;
			function Make(ptr: pointer; size: size_t): BlockData; static;
		end;

	var
		procedure Put(const ID: string; const data: BlockData);
		function TryPut(const ID: string; const data: BlockData): boolean; // игнорирует дубликат, а не бросает исключение (в остальном аналогично Put)
		function TryGet(const ID: string): pStreamImage;
		function ItemsCount: uint;

	private type
		pItemDesc = ^ItemDesc;
		pBlockDesc = ^BlockDesc;

		pItemRef = ^ItemRef;
		ItemRef = object
			blockIndex, itemIndex: uint;
			function Make(blockIndex, itemIndex: uint): ItemRef; static; cinline
			function Get(var bc: BlobCache): pItemDesc; cinline
		end;

		ItemDesc = object
			ID: string;
			affixes: array[Affix] of record
				len: size_t;
				takeFromBlock: uint;
			end;
			dataSize: size_t;
			offset: FilePos;
			function Make(const ID: string; dataSize: size_t; var bc: BlobCache): ItemDesc; static;
			function CalculateHeaderSize(const offsetFromHeaderEnd: FilePos): size_t;
			procedure WriteHeader(s: pStream; const headerEnd: FilePos);
			function CalculateExtra: uint;
		end;

		FitResult = object
		type
			HeaderDirtiness = (Clean, StartChanged, StartLenChanged);
		var
			// какую часть заголовка перезаписывать?
			// Если OK: достаточно записать заголовок нового элемента.
			// Если StartChanged: перезаписать начало заголовка и заголовок нового элемента.
			// Если StartLenChanged: перезаписать весь заголовок.
			dirty: HeaderDirtiness;

			// смещение заголовка только что помещённой записи.
			lastItemHeaderOffset: FilePos;

			// Для параноидальной проверки.
			offsetFromHeaderEnd: FilePos;
		end;

		BlockDesc = object
			headerOfs: FilePos;
			summaryItemHeaderSize, headerReserved, preciseHeader: size_t;
			items: array of ItemDesc;

			// длина заголовка до заголовков отдельных предметов (сейчас там три VarInt). Если она изменилась,
			// придётся перезаписать весь заголовок.
			headerStartLen: size_t;

			function Make(const newOfs: FilePos): BlockDesc; static;
			function Fit(blockIndex: uint; const item: ItemDesc; const offsetFromHeaderEnd: FilePos; out fr: FitResult): boolean;
			function HasData: boolean;
			function AddItem(const item: ItemDesc): pItemDesc;
			procedure WriteHeader(blockIndex: uint; s: pStream; const offsetToNext: FilePos; const fr: FitResult);
			function HeaderEnd: FilePos;
		end;

		{$define classname := ItemSet} {$define key_type := ItemRef} {$define inline_key := string}
		{$define user_param := pBlobCache}
		{$include hash.h.inc}

	const
		Signature       = 'BLOBS' + EOL;
		MaxHeaderLen    = size_t(64 * 1024);
		HeaderSizeQuant = size_t(4096);
		MinAffixLength = 6;
		ReasonableItemsCountLimit = 100000;

	type
		ItemAffix = object
		const
			EXTRA_BITS = 2;
			HAS_PREFIX_BITN = 0;
			HAS_SUFFIX_BITN = 1;
		end;

	var
		s: pStream;
		blocks: array of BlockDesc;
		items: ItemSet;
		procedure EnsureOK;
		procedure Init;
		procedure TrustedClose(emergency: boolean);
		procedure TrustedPut(const ID: string; const data: BlockData);
		procedure Read;
		function TryFitToExistingBlock(const item: ItemDesc; const streamEnd: FilePos; out fr: FitResult): uint;
		function FindBestAffix(const ID: string; out affixLen: size_t; what: Affix; limit: size_t): uint;
		function PrevHeaderReserved: size_t;
		function ItemFromAbsoluteIndex(absIdx: uint): pItemDesc;
	end;

	operator :=(const s: string): BlobCache.BlockData;
	operator :=(const s: BlobCache.BlockData): string;

implementation

uses
	Human;

	{$define classname := BlobCache.ItemSet} {$define get_key := _1.Get(param^)^.ID}
	{$define hash_func := Hash.OfString}
	{$include hash.pp.inc}

	function BlobCache.ItemRef.Make(blockIndex, itemIndex: uint): ItemRef;
	begin
		result.blockIndex := blockIndex;
		result.itemIndex  := itemIndex;
	end;

	function BlobCache.ItemRef.Get(var bc: BlobCache): pItemDesc;
	begin
		result := @bc.blocks[blockIndex].items[itemIndex];
	end;

	procedure BlobCache.Open(out bc: BlobCache; const fn: string);
	var
		ownFile: boolean;
	begin
		bc := Invalid;
		bc.Init;

		// Попробовать открыть существующий.
		bc.s := FileStream.Open(fn, [file_Read, file_Write, file_Existing, file_JustTry]);
		if Assigned(bc.s) then
			try
				bc.Read;
			{$ifdef Debug} Log('Кэш {0} открыт, {1}.', StreamPath.Log(fn), lang_amount(bc.ItemsCount, '{N} запис{ь/и/ей}'), logOK); {$endif}
			except
			{$ifdef Debug} Log(Exception.Message + ' Попытаюсь пересоздать кэш.', logWarning); {$endif}
				bc.TrustedClose(yes);
				bc.Init;
			end;

		// Файл будет пересоздан и если не существует, и если не прочитался.
		if not Assigned(bc.s) then
		begin
			ownFile := no;
			try
				bc.s := FileStream.Open(fn, [file_Read, file_Write, file_New]);
				ownFile := yes;
				Serialize_conststring(bc.s, Signature + GetExecVersion + EOL);
			{$ifdef Debug} Log('Создан кэш {0}.', StreamPath.Log(fn), logOK); {$endif}
			except
				bc.TrustedClose(ownFile);
				raise;
			end;
		end;
	end;

	procedure BlobCache.Close;
	begin
		if OK then TrustedClose(no);
	end;

	procedure BlobCache.Drop;
	begin
		if OK then TrustedClose(yes);
	end;

	function BlobCache.OK: boolean;
	begin
		result := Assigned(s);
	end;

	function BlobCache.Invalid: BlobCache;
	begin
	FPC_3_BUG System.Initialize(result);
		result.s      := nil;
		result.blocks := nil;
	end;

	function BlobCache.BlockData.Make(ptr: pointer; size: size_t): BlockData;
	begin
		result.ptr  := ptr;
		result.size := size;
	end;

	procedure BlobCache.Put(const ID: string; const data: BlockData);
	begin
		EnsureOK;
		if Assigned(items.Find(ID)) then raise Error('ID "{0}" уже существует в кэше.', PrintableString(ID));
		TrustedPut(ID, data);
	end;

	function BlobCache.TryPut(const ID: string; const data: BlockData): boolean;
	begin
		EnsureOK;
		result := not Assigned(items.Find(ID));
		if result then TrustedPut(ID, data);
	end;

	function BlobCache.TryGet(const ID: string): pStreamImage;
	var
		ref: pItemRef;
		item: pItemDesc;
	begin
		EnsureOK;
		ref := items.Find(ID);
		if Assigned(ref) then
			try
				item := ref^.Get(self);
				result := s^.GetImage(item^.offset, item^.dataSize);
			except
				TrustedClose(yes);
				raise Error('Не удалось получить "{0}" из кэша.', PrintableString(ID));
			end
		else
			result := nil;
	end;

	function BlobCache.ItemsCount: uint;
	var
		i: sint;
	begin
		result := 0;
		for i := 0 to High(blocks) do
			result += uint(length(blocks[i].items));
	end;

	function BlobCache.ItemDesc.Make(const ID: string; dataSize: size_t; var bc: BlobCache): ItemDesc;
	var
		a: Affix;
		lenLimit: size_t;
	begin
		result.ID                  := ID;
		result.dataSize            := dataSize;
		result.offset              := FilePos.Not0;

		lenLimit := length(result.ID);
		for a in Affix do
		begin
			result.affixes[a].takeFromBlock := bc.FindBestAffix(ID, result.affixes[a].len, a, lenLimit);
			lenLimit -= result.affixes[a].len;
		end;
	end;

	function BlobCache.ItemDesc.CalculateHeaderSize(const offsetFromHeaderEnd: FilePos): size_t;
	var
		a: Affix;
	begin
		result := 0;
		for a in Affix do
			Assert((affixes[a].len = 0) = (0 = not affixes[a].takeFromBlock), Format('{0}/{1}', [affixes[a].len, affixes[a].takeFromBlock]));

		// Формат заголовка item:
		// string_xbits строка-суффикс, nExtra = EXTRA_BITS,
		// если выставлен бит HAS_PREFIX_BITN — есть префикс, если выставлен HAS_SUFFIX_BITN — есть суффикс.
		// для префикса и суффикса, если есть: VarInt индекс предыдущего блока, VarInt длина аффикса.
		// VarInt размер данных.
		// если размер > 0: VarInt смещение данных от конца заголовка.
		result += Serialize_string_xbits_bytes(size_t(length(ID)) - affixes[Prefix].len - affixes[Suffix].len, ItemAffix.EXTRA_BITS,
		                                       CalculateExtra);
		for a in Affix do
			if affixes[a].len > 0 then result += VarInt.Bytes(affixes[a].takeFromBlock) + VarInt.Bytes(affixes[a].len);
		result += VarInt.Bytes(dataSize);
		if dataSize > 0 then result += VarInt.Bytes(offsetFromHeaderEnd.value);
	end;

	procedure BlobCache.ItemDesc.WriteHeader(s: pStream; const headerEnd: FilePos);
	var
		a: Affix;
	begin
		Serialize_string_xbits(s, Copy(ID, 1 + affixes[Prefix].len, length(ID) - affixes[Prefix].len - affixes[Suffix].len),
			ItemAffix.EXTRA_BITS, CalculateExtra);
		for a in Affix do
			if affixes[a].len > 0 then
			begin
				VarInt.Write(s, affixes[a].takeFromBlock);
				VarInt.Write(s, affixes[a].len);
			end;
		VarInt.Write(s, dataSize);
		if dataSize > 0 then VarInt.Write(s, (offset - headerEnd).value);
	end;

	function BlobCache.ItemDesc.CalculateExtra: uint;
	begin
		result := ord(affixes[Prefix].len > 0) shl ItemAffix.HAS_PREFIX_BITN or ord(affixes[Suffix].len > 0) shl ItemAffix.HAS_SUFFIX_BITN;
	end;

	function BlobCache.BlockDesc.Make(const newOfs: FilePos): BlockDesc;
	begin
		result.headerOfs             := newOfs;
		result.summaryItemHeaderSize := 0;
		result.headerReserved        := 0;
		result.preciseHeader         := 0;
		result.items                 := nil;
		result.headerStartLen        := 0;
	end;

	function BlobCache.BlockDesc.Fit(blockIndex: uint; const item: ItemDesc; const offsetFromHeaderEnd: FilePos; out fr: FitResult): boolean;
	var
		allowDynamic: boolean;
		curHeaderReserved, curPreciseHeader, prevHeaderReserved, itemHeaderSize, prevItemHeaderSize, curHeaderStartSize: size_t;
		tryId, tries: uint;
	begin
		unused_args blockIndex end_list
		allowDynamic := not HasData;
		if allowDynamic then Assert(offsetFromHeaderEnd = FilePos.Zero);
		curHeaderReserved := headerReserved;
		if allowDynamic then tries := 3 else tries := 1;
		fr.offsetFromHeaderEnd := offsetFromHeaderEnd + item.dataSize;

		// заткнуть компилятор, используются только на tryId > 1
		prevHeaderReserved  := 0;
		prevItemHeaderSize := 0;

		for tryId := 1 to tries do
		begin
			itemHeaderSize := item.CalculateHeaderSize(offsetFromHeaderEnd);
			// подсчёт длины данных, записываемых в WriteHeader

			curHeaderStartSize :=
				// зарезервированная длина заголовка
				VarInt.Bytes(curHeaderReserved)
				// смещение до следующего заголовка относительно конца этого
				+ VarInt.Bytes(fr.offsetFromHeaderEnd.value)
				// число записей
				+ VarInt.Bytes(length(items) + 1); // существующие + этот

			curPreciseHeader := curHeaderStartSize
				+ summaryItemHeaderSize
				+ itemHeaderSize;

			if allowDynamic then
			begin
				if curPreciseHeader <= headerReserved then
					curHeaderReserved := headerReserved
				else
					curHeaderReserved := // align(curPreciseHeader, HeaderSizeQuant);
						HeaderSizeQuant * ((curPreciseHeader + (HeaderSizeQuant - 1)) div HeaderSizeQuant);
			end;

			result := (not allowDynamic and (curPreciseHeader <= headerReserved))
			       or (allowDynamic and (curHeaderReserved <= MaxHeaderLen));
			result := result and (length(items) < ReasonableItemsCountLimit);
			if not result then break;

			if allowDynamic then
			begin
				result := (tryId > 1) and (prevHeaderReserved = curHeaderReserved) and (prevItemHeaderSize = itemHeaderSize);
				if not result and (tryId = tries) then raise Error(lang_amount(tries, 'Fit не стабилизировалась с {N} попыт{ки/ок/ок}.'));

				prevHeaderReserved := curHeaderReserved;
				prevItemHeaderSize := itemHeaderSize;
			end;
			if result then break;
		end;

		if result then
		begin
			fr.lastItemHeaderOffset := headerOfs + curHeaderStartSize + summaryItemHeaderSize;
			if curHeaderStartSize <> headerStartLen then
			begin
				fr.dirty := StartLenChanged;
				headerStartLen := curHeaderStartSize;
			end else
				if allowDynamic and (headerReserved <> curHeaderReserved) or (item.dataSize > 0) then
					fr.dirty := StartChanged
				else
					fr.dirty := Clean;

			summaryItemHeaderSize += itemHeaderSize;
			preciseHeader         := curPreciseHeader;
			if allowDynamic then headerReserved := curHeaderReserved;
		end;
	end;

	function ItemHasData(id: uint; items: pointer): boolean;
	begin
		result := BlobCache.pItemDesc(items)[id].dataSize > 0;
	end;

	function BlobCache.BlockDesc.HasData: boolean;
	begin
		result := Range.Open(length(items)).Any(@ItemHasData, pItemDesc(items));
	end;

	function BlobCache.BlockDesc.AddItem(const item: ItemDesc): pItemDesc;
	begin
		SetLength(items, length(items) + 1);
		result := @items[High(items)];
		result^ := item;
		Assert(summaryItemHeaderSize <= preciseHeader, Format('{0}/{1}/{2}', [summaryItemHeaderSize, preciseHeader, headerReserved]));
	end;

	procedure BlobCache.BlockDesc.WriteHeader(blockIndex: uint; s: pStream; const offsetToNext: FilePos; const fr: FitResult);
	var
		i: sint;
	{$ifdef Debug} chkStart: FilePos; {$endif}
	begin
		unused_args blockIndex end_list
		if offsetToNext <> fr.offsetFromHeaderEnd then
			raise Error('Смещения из Fit и WriteHeader не совпадают ({0} vs. {1}).', [fr.offsetFromHeaderEnd.value, offsetToNext.value]);

		// длина данных подсчитывается в Fit
		if fr.dirty <> Clean then
		begin
			s^.Position := headerOfs;
			VarInt.Write(s, headerReserved);
			VarInt.Write(s, offsetToNext.value);
			VarInt.Write(s, length(items));
		end;

		if fr.dirty <> StartLenChanged then
		begin
			s^.Position := fr.lastItemHeaderOffset;
			items[High(items)].WriteHeader(s, HeaderEnd);
		end;

		if fr.dirty = StartLenChanged then
		begin
		{$ifdef Debug} chkStart := s^.Position; {$endif}
			for i := 0 to High(items) do
				items[i].WriteHeader(s, HeaderEnd);
		{$ifdef Debug}
			if (s^.Position - chkStart).value <> summaryItemHeaderSize then
				raise Error('summaryItemHeaderSize рассчитана неверно (N = {0}): summarySize = {1}, chkStart = {2}, Position = {3}.',
				            ToString(length(items)), ToString(summaryItemHeaderSize), ToString(chkStart.value), ToString(s^.Position.value));
		{$endif}

		{$ifdef Debug}
			if (s^.Position - headerOfs).AsSizeT > preciseHeader then
				raise Error('WriteHeader превысила рассчитанный размер: headerOfs = {0}, headerLen = {1}, Position = {2} (+{3}).',
				            ToString(headerOfs.value), ToString(preciseHeader), ToString(s^.Position.value),
				            ToString((s^.Position - headerOfs - preciseHeader).value));
		{$endif}
		end;
	end;

	function BlobCache.BlockDesc.HeaderEnd: FilePos;
	begin
		result := headerOfs + headerReserved;
	end;

	procedure BlobCache.EnsureOK;
	begin
		if not OK then raise Error('Кэш не открыт.');
	end;

	procedure BlobCache.Init;
	begin
		items.Init(@self);
	end;

	procedure BlobCache.TrustedClose(emergency: boolean);
	var
		erase: boolean;
		fn: string;
	begin
		erase := emergency and Assigned(s);
		if erase then fn := s^.path;

		Release(s);
		items.Done;
	FPC_3_BUG System.Finalize(self);
		self := Invalid;

		if erase then &File.Erase(fn);
	end;

	procedure BlobCache.TrustedPut(const ID: string; const data: BlockData);
	var
		item: ItemDesc;
		ip: pItemDesc;
		b: uint;
		offsetToNext: FilePos;
		wasDyn: boolean;
		fr: FitResult;
	begin
		try
			// если есть место в одном из блоков, данные будут дописаны в конец файла
			// (на самом деле пока допускается расширение только последнего, а не любого)
			item := ItemDesc.Make(ID, data.size, self);
			b    := TryFitToExistingBlock(item, s^.Size, fr);

		{$ifdef Debug}
			if 0 <> not b then
				Log('"{0}" размещён в блоке {1}, смещение в заголовке {2}/{3}, смещение данных {4}',
				    [ID, b, (fr.lastItemHeaderOffset - blocks[b].headerOfs).value, blocks[b].headerReserved, ToString(FilePos(s^.Size))], logDebug);
		{$endif}

			if 0 = not b then
			begin
				b := length(blocks);
				SetLength(blocks, b + 1);
				blocks[b] := BlockDesc.Make(s^.Size);
				blocks[b].headerReserved := min(2 * PrevHeaderReserved, MaxHeaderLen); // этот резерв не имеет принципиального значения,
				                                                                       // т. к. блок растянется за счёт allowDynamic.
				if not blocks[b].Fit(b, item, FilePos.Zero, fr) then
					raise Error('Ключ не умещается в заголовок блока.');

			{$ifdef Debug}
				Log('"{0}" размещён в новом блоке {1}, резерв заголовка {2}, смещение данных {3}',
				    [ID, b, blocks[b].headerReserved, ToString(blocks[b].headerOfs + blocks[b].headerReserved)], logDebug);
			{$endif}
			end;

			wasDyn := not blocks[b].HasData; // строго говоря, and b = High(blocks), но см. выше, пока это невозможно
			ip := blocks[b].AddItem(item);
			if wasDyn then ip^.offset := blocks[b].headerOfs + blocks[b].headerReserved else ip^.offset := s^.Size;
			offsetToNext := ip^.offset + ip^.dataSize - blocks[b].headerEnd;
			blocks[b].WriteHeader(b, s, offsetToNext, fr);

			s^.Position := ip^.offset;
			s^.Write(data.ptr, data.size);
			items.Add(ItemRef.Make(b, High(blocks[b].items)), ip^.ID);
		except
			TrustedClose(yes);
			raise Error('Не удалось поместить "{0}" в кэш.', PrintableString(ID));
		end;
	end;

	procedure BlobCache.Read;
	var
		b: pBlockDesc;
		a: Utils.Affix;
		it, takeFrom: pItemDesc;
		offsetToNext, itemsStart: FilePos;
		nItems: uint;
		itemIndex, absoluteItemIndex: sint;
		extra: uint;
	begin
		Deserialize_signature(s, Signature + GetExecVersion + EOL);
		absoluteItemIndex := 0;
		while s^.Position < s^.Size do
		begin
			SetLength(blocks, length(blocks) + 1);
			b                  := @blocks[High(blocks)];
			b^.headerOfs       := s^.Position;
			b^.headerReserved  := RangeCheck(VarInt.Read(s), (s^.Size - s^.Position).value, 'BlobCache.HeaderReserved');
			offsetToNext.value := RangeCheck(VarInt.Read(s), (s^.Size - b^.headerOfs).value - b^.headerReserved, 'BlobCache.OffsetToNext');
			nItems             := RangeCheck(VarInt.Read(s), 1, ReasonableItemsCountLimit, 'BlobCache.ItemsCount');
			itemsStart         := s^.Position;

			SetLength(b^.items, nItems);
			for itemIndex := 0 to High(b^.items) do
			begin
				it := @b^.items[itemIndex];

				it^.ID := Deserialize_string_xbits(s, ItemAffix.EXTRA_BITS, extra);
				for a in Utils.Affix do
					if (a = Prefix) and (extra and (1 shl ItemAffix.HAS_PREFIX_BITN) <> 0)
						or (a = Suffix) and (extra and (1 shl ItemAffix.HAS_SUFFIX_BITN) <> 0) then
					begin
						it^.affixes[a].takeFromBlock := RangeCheckOpen(VarInt.Read(s), absoluteItemIndex, 'BlobCache.TakeAffixFromBlock');
						takeFrom                     := ItemFromAbsoluteIndex(it^.affixes[a].takeFromBlock);
						it^.affixes[a].len           := RangeCheck(VarInt.Read(s), 1, length(takeFrom^.ID), 'BlobCache.AffixLen');
						AppendAffixTo(it^.ID, GetAffix(takeFrom^.ID, it^.affixes[a].len, a), a);
					end else
					begin
						it^.affixes[a].takeFromBlock := not uint(0);
						it^.affixes[a].len           := 0;
					end;

				it^.dataSize               := VarInt.Read(s);
				it^.offset                 := b^.headerOfs + b^.headerReserved + VarInt.Read(s);
				if it^.offset + it^.dataSize > s^.Size then
					raise Error('Неверные размер-смещение записи {0}: +{1}:{2}', it^.ID, ToString(it^.offset.value), ToString(it^.dataSize));
				inc(absoluteItemIndex);
				items.Add(ItemRef.Make(High(blocks), itemIndex), it^.ID);
			end;
			b^.preciseHeader         := (s^.Position - b^.headerOfs).AsSizeT;
			b^.summaryItemHeaderSize := (s^.Position - itemsStart).AsSizeT;
			s^.Position              := b^.headerOfs + b^.headerReserved + offsetToNext;
		end;
	end;

	function BlobCache.TryFitToExistingBlock(const item: ItemDesc; const streamEnd: FilePos; out fr: FitResult): uint;
	var
		b: pBlockDesc;
		offsetFromHeaderEnd: FilePos;
	begin
		result := not uint(0);
		if length(blocks) > 0 then
		begin
			b := @blocks[High(blocks)];
			if not b^.HasData then
				offsetFromHeaderEnd := FilePos.Zero
			else
				offsetFromHeaderEnd := streamEnd - b^.HeaderEnd;

			if b^.Fit(High(blocks), item, offsetFromHeaderEnd, fr) then
				result := High(blocks);
		end;
	end;

	function BlobCache.FindBestAffix(const ID: string; out affixLen: size_t; what: Affix; limit: size_t): uint;
	var
		ib, ii, absii: sint;
		curAffixLen: size_t;
	begin
		result    := not uint(0);
		affixLen := 0;
		absii     := 0;

		for ib := 0 to High(blocks) do
			for ii := 0 to High(blocks[ib].items) do
			begin
				curAffixLen := CommonAffixLength(ID, blocks[ib].items[ii].ID, what, limit);
				if (curAffixLen > affixLen) and (curAffixLen >= MinAffixLength) then
				begin
					affixLen := curAffixLen;
					result    := absii;
				end;
				inc(absii);
			end;
	end;

	function BlobCache.PrevHeaderReserved: size_t;
	begin
		if length(blocks) <= 1 then result := 0 else result := blocks[High(blocks) - 1].headerReserved;
	end;

	function BlobCache.ItemFromAbsoluteIndex(absIdx: uint): pItemDesc;
	var
		i: sint;
	begin
		for i := 0 to High(blocks) do
			if absIdx < uint(length(blocks[i].items)) then
				exit(@blocks[i].items[absIdx])
			else
				absIdx -= uint(length(blocks[i].items));
		raise Error('Записи с абсолютным индексом {0} не существует.', ToString(absIdx));
	end;

	operator :=(const s: string): BlobCache.BlockData; begin result := result.Make(pointer(s), length(s) * sizeof(s[1])); end;
	operator :=(const s: BlobCache.BlockData): string; begin result := USystem.ToString(s.ptr, s.size); end;

end.
