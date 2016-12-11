unit Streams;

{$include opts.inc}
{$ifdef Debug}
	{-$define ExtDebug}
	{-$define DebugZStreamWithTinyBlocks}
	{-$define DebugZSteps}
	{-$define DebugStreamPath}
{$endif}
{$include all_numbers.inc}

interface

uses
	ctypes, ZlibHeaders, USystem, Errors, UMath, QuickLZ;

const
	DefaultFileFlags = [file_Read];

type
	StreamPath = object
		function Relative(const src, base: string): string; static;
		function Navigate(const src, base: string): string; static;
		function Resolve(const src: string): string; static;
		function Resolve(const src, base: string): string; static;
		function CutExtension(const src: string; ext: pString = nil): string; static;
		function System(const src: string): string; static;
		function Human(const src: string): string; static;
		function Log(const src: string): string; static;
		function Path(const src: string): string; static;
		function ForcePath(const src: string): string; static; cinline
		function ForceNoPath(const src: string): string; static; cinline
		function Filename(const src: string): string; static;
		function FilenameNoExt(const src: string): string; static;
		function FilenameNoExt(const src: string; ext: pString): string; static;
		function Extension(const src: string): string; static;
		function Extension(const src: string; id: sint): string; static;
	private
		function Human(const src: string; quote: boolean): string; static;
	end;

	pStream = ^Stream;

	pStreamImage = ^StreamImage;
	StreamImage = object(&Object)
	private
		_native: pMapping;
		_data: pointer;
		_size: size_t;
		ownDataPtr: boolean;
	{$ifdef Debug} srcfn: string; {$endif}
	public
		constructor Emulate(newData: pointer; newSize: size_t; ownDataPtr: boolean {$ifdef Debug}; const newSrcfn: string {$endif});
		constructor Native(var f: &File; const ofs: FilePos; newSize: size_t; flags: FileFlags {$ifdef Debug}; const newSrcfn: string {$endif});
		destructor Done; virtual;
		property Data: pointer read _data;
		property Size: size_t read _size;
		function AsString: string;
	end;

	Stream = object(&Object)
	private const
		TempBufferSize = &File.TempBufferSize;
	private
		_flags: FileFlags;
		_size: FileSize;
		_position: FilePos;
		procedure _SetSize(const newSize: FileSize);
		procedure _SetPosition(const newPos: FilePos);
	protected
		function _Resize(const newSize: FileSize): FileSize; virtual; abstract;
		function _Seek(const newPos: FilePos): FilePos; virtual;
	public
		path: string;
		constructor Init(newFlags: FileFlags; const newPath: string);
		destructor Done; virtual;
		function CanSeek: boolean; virtual;
		function TryRead(dest: pointer; count: size_t): size_t; virtual; abstract;
		function TryWrite(source: pointer; count: size_t): size_t; virtual; abstract;
		function Read(dest: pointer; count: size_t): pStream;
		function Write(source: pointer; count: size_t): pStream;
		function Write(const s: string): pStream;
		procedure AppendFrom(s: pStream);
		procedure AppendFrom(s: pStream; count: FileSize);
		procedure AppendFrom(s: pStream; const start: FilePos; const count: FileSize);
		function Skip(n: FileSize): pStream;
		function Window(const pos: FilePos; const wsize: FileSize; const newPath: string): pStream; virtual;
		function GetImage(const ofs: FilePos; imsize: size_t): pStreamImage; virtual;
		procedure DisableBuffering; virtual;
		procedure Flush; virtual;
	public
		property Flags: FileFlags read _flags;
		property Position: FilePos read _position write _SetPosition;
		property Size: FileSize read _size write _SetSize;
	end;

	pFileStream = ^FileStream;
	FileStream = object(Stream)
	private const
		MmapSizeLimit = size_t(256 * 1024);
	private
		f: &File;
		mmap: Mapping;
		mmapStart, mmapEnd: FilePos;
		mmapSize, maxMmapSize: size_t;
		physicalFileSize: FileSize;
		allowMmapGrow: boolean;
	{$ifdef Debug} physFn: string; {$endif}
		function IO(target: pointer; count: size_t; isRead: boolean): size_t;
		procedure CloseMmap;
	protected
		function _Resize(const newSize: FileSize): FileSize; virtual;
	public
		constructor Init(const fileName: string; newFlags: FileFlags = DefaultFileFlags);
		constructor TrustedInit(const newPath {$ifdef Debug}, newPhysFn {$endif}: string; const opened: &File; newFlags: FileFlags;
		                        fromCtr: boolean; const physicalSize: FileSize);
		function Open(const fileName: string; newFlags: FileFlags = DefaultFileFlags): pStream; static;
		destructor Done; virtual;

		function CreateTemp(const base: string = ''; mode: &File.TempMode = CreateAndOpenTemp): pFileStream; static;
		function TryRead(dest: pointer; count: size_t): size_t; virtual;
		function TryWrite(source: pointer; count: size_t): size_t; virtual;
		function Window(const pos: FilePos; const wsize: FileSize; const newPath: string): pStream; virtual;
		function GetImage(const ofs: FilePos; imsize: size_t): pStreamImage; virtual;
		procedure DisableBuffering; virtual;
		procedure Flush; virtual;
	end;

	pMemoryStream = ^MemoryStream;
	MemoryStream = object(Stream)
	private
		ptr: pointer;
		allocated: size_t;
		ownsPtr: boolean;
	protected
		function _Resize(const newSize: FileSize): FileSize; virtual;
	public
		constructor InitReadOnly(newP: pointer; newSize: size_t; newOwnsPtr: boolean = yes);
		constructor InitWriteOnly(newSize: size_t = 0);
		constructor InitRW;
		destructor Done; virtual;
		function TryRead(dest: pointer; count: size_t): size_t; virtual;
		function TryWrite(source: pointer; count: size_t): size_t; virtual;
		function Window(const pos: FilePos; const wsize: FileSize; const newPath: string): pStream; virtual;
		function GetImage(const ofs: FilePos; imsize: size_t): pStreamImage; virtual;
		property Data: pointer read ptr;
	end;

type
	EndBehaviourFlag = (AllowAbandon);
	EndBehaviour = set of EndBehaviourFlag;

	pBlockStream = ^BlockStream;
	BlockStream = object
	private const
		DefaultBlockSize = {$ifdef DebugZStreamWithTinyBlocks} {$note BlockStream.BlockSize = 1 Kb} 1 {$else} 1024 {$endif} * 1024;
	private
		base: pStream;
		reading, headerReady, broken, breakAfterThis: boolean;
		buf: pointer;
		pos, blockSize: size_t;
		endb: EndBehaviour;
	public
		function Open(newBase: pStream; mode: FileFlag; newEndb: EndBehaviour): BlockStream; static;
		procedure Close;
		function Read(data: pointer; size: size_t): size_t;
		procedure Write(data: pointer; size: size_t);
		procedure EnsureHeader(last: boolean);
		procedure WriteBuffer(last: boolean);
	end;

	pZStream = ^ZStream;
	ZStream = object(Stream)
	type
		Method = {$push} {$scopedenums on} (Deflate, Lzo, Bzip2, Lzham, QuickLZ) {$pop};
	public
		constructor Init(base: pStream; endb: EndBehaviour);
		constructor Init(base: pStream; aMethod: Method);
		destructor Done; virtual;
		function TryRead(dest: pointer; count: size_t): size_t; virtual;
		function TryWrite(source: pointer; count: size_t): size_t; virtual;
		function CanSeek: boolean; virtual;
	private type
		ModeEnum = (Compress, Decompress);

		pImplBase = ^ImplBase;
		ImplBase = object
			base: pStream;
			mode: ModeEnum;
			endb: EndBehaviour;
			constructor Init(newBase: pStream; newMode: ModeEnum; newEndb: EndBehaviour);
			destructor Done; virtual;
			function Read(dest: pointer; count: size_t): size_t; virtual; abstract;
			function Write(source: pointer; count: size_t): size_t; virtual; abstract;
		end;
	protected
		function _Resize(const newSize: FileSize): FileSize; virtual;
		function _Seek(const newPos: FilePos): FilePos; virtual;
	private
	{$ifdef Debug} srcMode: ModeEnum; {$endif}
		impl: pImplBase;
		sizeSet: boolean;
		function _Init(base: pStream; aMethod: Method; mode: ModeEnum; endb: EndBehaviour): boolean;
		function _NRE: Exception;
		function _InitFailed(base: pStream; aMethod: Method; mode: ModeEnum): Exception;

	private type
		pStreamAdapter = ^StreamAdapter;
		StreamAdapter = object(BaseObject)
		public type
			AdvanceResult = (advance_Ok, advance_Error, advance_Finished);
		public
			srcMode: ModeEnum;
			constructor Init(newMode: ModeEnum);
			destructor Done; virtual;
			function Advance(mode: ModeEnum; input: pointer; nInput: size_t; output: pointer; nOutput: size_t; out consumed, written: size_t): AdvanceResult;
			function AdvanceImpl(mode: ModeEnum; input: pointer; nInput: size_t; output: pointer; nOutput: size_t; out consumed, written: size_t): AdvanceResult; virtual; abstract;
			function Bound(const uncompressed: size_t): size_t; virtual; abstract;
		end;

		pStreamed = ^Streamed;
		Streamed = object(ImplBase)
		private const
			MaxCompressBufferSize = size_t({$ifdef DebugZStreamWithTinyBlocks} {$note Streamed.MaxCompressBufferSize = 1} 1 {$else} 256 * 1024 {$endif});
			MaxDecompressBufferSize = size_t({$ifdef DebugZStreamWithTinyBlocks} {$note Streamed.MaxDecompressBufferSize = 1} 1 {$else} 256 * 1024 {$endif});
		private
			function _Read(dest: pointer; count: size_t): size_t;
			function _Write(source: pointer; count: size_t): size_t;
		public
			bs: BlockStream;
			zs: pStreamAdapter;
			buf, bufPos: pointer;
			bufAvail, bufAllocated: size_t;
			broken, finished: boolean;
			constructor Init(newBase: pStream; newMode: ModeEnum; newZs: pStreamAdapter; newEndb: EndBehaviour);
			destructor Done; virtual;
			function Read(dest: pointer; count: size_t): size_t; virtual;
			function Write(source: pointer; count: size_t): size_t; virtual;
		end;

		pDeflate = ^Deflate;
		Deflate = object(StreamAdapter)
			zs: z.stream;
			constructor Init(newMode: ModeEnum);
			destructor Done; virtual;
			function AdvanceImpl(mode: ModeEnum; input: pointer; nInput: size_t; output: pointer; nOutput: size_t; out consumed, written: size_t): AdvanceResult; virtual;
			function Bound(const uncompressed: size_t): size_t; virtual;
		{$ifdef Debug} procedure Error(const where: string; code: sint); {$endif}
		end;

		pBlockAdapter = ^BlockAdapter;
		BlockAdapter = object(BaseObject)
			constructor Init;
			destructor Done; virtual;
			function Bound(const uncompressed: size_t): size_t; virtual; abstract;
			procedure Compress(block: pointer; size: size_t; output: pointer; outputSize: size_t; out compressedSize: size_t); virtual; abstract;
			procedure Decompress(block: pointer; size: size_t; output: pointer; outputSize: size_t; out decompressedSize: size_t); virtual; abstract;
		end;

		pBlocked = ^Blocked;
		Blocked = object(ImplBase)
		private const
			BlockSize = size_t({$ifdef DebugZStreamWithTinyBlocks} {$note Blocked.BlockSize = 1} 1 {$else} 4 * 1024 * 1024 {$endif});
			ReasonableBlockSizeLimit = size_t(10 * 1024 * 1024);
		public
			constructor Init(newBase: pStream; newMode: ModeEnum; newZs: pBlockAdapter; newEndb: EndBehaviour);
			destructor Done; virtual;
			procedure EnsureHeader(last: boolean);
			function Read(dest: pointer; count: size_t): size_t; virtual;
			function Write(source: pointer; count: size_t): size_t; virtual;
			function ReadBuffer(last: boolean): boolean;
			function WriteBuffer(last: boolean): boolean;
		strict private
			zs: pBlockAdapter;
			inbuf, outbuf: pointer;
			inpos, insize, inalloc, outalloc: size_t;
			broken, finished, headerReady: boolean;
		end;

		pLzo = ^LzoAdapter;
		LzoAdapter = object(BlockAdapter)
			constructor Init;
			destructor Done; virtual;
			function Bound(const uncompressed: size_t): size_t; virtual;
			procedure Compress(block: pointer; size: size_t; output: pointer; outputSize: size_t; out compressedSize: size_t); virtual;
			procedure Decompress(block: pointer; size: size_t; output: pointer; outputSize: size_t; out decompressedSize: size_t); virtual;
			function Error(const where: string; code: cint): Exception; static;
		end;

		pBzip2 = ^Bzip2Adapter;
		Bzip2Adapter = object(StreamAdapter)
			bz: bzip2.stream;
			constructor Init(newMode: ModeEnum);
			destructor Done; virtual;
			function AdvanceImpl(mode: ModeEnum; input: pointer; nInput: size_t; output: pointer; nOutput: size_t; out consumed, written: size_t): AdvanceResult; virtual;
			function Bound(const uncompressed: size_t): size_t; virtual;
		{$ifdef Debug} procedure Error(const where: string; code: sint); {$endif}
		end;

		pLzham = ^LzhamAdapter;
		LzhamAdapter = object(StreamAdapter)
			state: pointer;
			constructor Init(newMode: ModeEnum);
			destructor Done; virtual;
			function AdvanceImpl(mode: ModeEnum; input: pointer; nInput: size_t; output: pointer; nOutput: size_t; out consumed, written: size_t): AdvanceResult; virtual;
			function Bound(const uncompressed: size_t): size_t; virtual;
		{$ifdef Debug} procedure Error(const where: string; code: sint); {$endif}
		end;

		pQuickLZ = ^QuickLZAdapter;
		QuickLZAdapter = object(BlockAdapter)
			function Bound(const uncompressed: size_t): size_t; virtual;
			procedure Compress(block: pointer; size: size_t; output: pointer; outputSize: size_t; out compressedSize: size_t); virtual;
			procedure Decompress(block: pointer; size: size_t; output: pointer; outputSize: size_t; out decompressedSize: size_t); virtual;
		end;

	public const
		MethodIds: array[Method] of string = ('Deflate', 'LZO', 'BZip2', 'Lzham', 'QuickLZ');
	end;

	pBitStream = ^BitStream;
	BitStream = object
	private type
		tmp_t = PtrUint;
	var
		base: pStream;
		reading: boolean;
		pending: tmp_t;
		nPending: uint;
		procedure FlushWrite(fully: boolean);
	public
		function Open(newBase: pStream; mode: FileFlag): BitStream; static;
		procedure Close;
		function Read(n: uint): uint;
		procedure Write(data, n: uint);
	end;

	procedure MarkAsReadOnly(const folder: string);
	function MarkedAsReadOnly(const path: string): boolean;
	function GetStreamAttributes(const info: string): FileAttributes;
	function GetStream(const info: string; flags: FileFlags): pStream;
	function GetStream(const info: string): pStream;
	function GetStreamRef(const info: string; flags: FileFlags): pStream;
	function GetStreamRef(const info: string): pStream;
	function TryGetStream(const info: string): pStream;

	// TODO: сформулировать, что эта хрень делает. Применяется в tLocale.SetFsBase.)
	function PrefetchPath(const info: string): string;

type
	ReadWholeAsStringOptsDesc = object
		function Binary(isbin: boolean = yes): ReadWholeAsStringOptsDesc;
		function StoreSignature(&to: pString): ReadWholeAsStringOptsDesc;
	private
		bin: boolean;
		storeSig: pString;
	end;
const
	ReadWholeAsStringOpts: ReadWholeAsStringOptsDesc = (bin: no; storeSig: nil);
	function SkipUTF8BOM(s: pStream; storeSig: pString = nil): boolean;
	function ReadWholeAsString(f: pStream): string;
	function ReadWholeAsString(f: pStream; const opts: ReadWholeAsStringOptsDesc): string;

	procedure Serialize_f64(s: pStream; const f: hp_float); function Deserialize_f64(s: pStream): hp_float;
	procedure Serialize_f32(s: pStream; const f: float);    function Deserialize_f32(s: pStream): float;
	procedure Serialize_f16(s: pStream; const f: float);    function Deserialize_f16(s: pStream): float;

	procedure Serialize_vec2f32(s: pStream; const v: Vec2); function Deserialize_vec2f32(s: pStream): Vec2;
	procedure Serialize_vec3f32(s: pStream; const v: Vec3); function Deserialize_vec3f32(s: pStream): Vec3;
	procedure Serialize_vec4f32(s: pStream; const v: Vec4); function Deserialize_vec4f32(s: pStream): Vec4;

	procedure Serialize_vec2f16(s: pStream; const v: Vec2); function Deserialize_vec2f16(s: pStream): Vec2;
	procedure Serialize_vec3f16(s: pStream; const v: Vec3); function Deserialize_vec3f16(s: pStream): Vec3;
	procedure Serialize_vec4f16(s: pStream; const v: Vec4); function Deserialize_vec4f16(s: pStream): Vec4;

	procedure Serialize_fN8(s: pStream; const f, a, b: float);    function Deserialize_fN8(s: pStream; const a, b: float): float;
	procedure Serialize_fN16(s: pStream; const f, a, b: float);   function Deserialize_fN16(s: pStream; const a, b: float): float;
	procedure Serialize_vec2N8(s: pStream; const v, a, b: Vec2);  function Deserialize_vec2N8(s: pStream; const a, b: Vec2): Vec2;
	procedure Serialize_vec2N16(s: pStream; const v, a, b: Vec2); function Deserialize_vec2N16(s: pStream; const a, b: Vec2): Vec2;
	procedure Serialize_vec3N8(s: pStream; const v, a, b: Vec3);  function Deserialize_vec3N8(s: pStream; const a, b: Vec3): Vec3;
	procedure Serialize_vec3N16(s: pStream; const v, a, b: Vec3); function Deserialize_vec3N16(s: pStream; const a, b: Vec3): Vec3;
	procedure Serialize_vec4N8(s: pStream; const v, a, b: Vec4);  function Deserialize_vec4N8(s: pStream; const a, b: Vec4): Vec4;
	procedure Serialize_vec4N16(s: pStream; const v, a, b: Vec4); function Deserialize_vec4N16(s: pStream; const a, b: Vec4): Vec4;

	procedure Serialize_IQuat8(s: pStream; const uq: Quaternion);    function Deserialize_IQuat8(s: pStream): Quaternion;
	procedure Serialize_IQuat16(s: pStream; const uq: Quaternion);   function Deserialize_IQuat16(s: pStream): Quaternion;
	procedure Serialize_tf32r8(s: pStream; const t: Transform);      function Deserialize_tf32r8(s: pStream): Transform;
	procedure Serialize_tf32r16(s: pStream; const t: Transform);     function Deserialize_tf32r16(s: pStream): Transform;
	procedure Serialize_planeN8d32(s: pStream; const plane: Plane);  function Deserialize_planeN8d32(s: pStream): Plane;

	function Serialize_string_bytes(len: size_t): size_t;
	function Serialize_string_bytes(const s: string): size_t;
	procedure Serialize_string(s: pStream; const st: string);        function Deserialize_string(s: pStream): string;
	function Serialize_string_xbits_bytes(len: size_t; nExtra, extra: uint): size_t;
	function Serialize_string_xbits_bytes(const s: string; nExtra, extra: uint): size_t;
	procedure Serialize_string_xbits(s: pStream; const st: string; nExtra, extra: uint);
	function Deserialize_string_xbits(s: pStream; nExtra: uint; out extra: uint): string;
	procedure Serialize_conststring(s: pStream; const st: string); function Deserialize_conststring(s: pStream; len: uint): string;
	procedure Serialize_signature(s: pStream; const sig: string);  function Deserialize_signature(s: pStream; const sig: string; mayFail: boolean = no): boolean;

type
	UiBinaryFormat = (se_ui8, se_ui16, se_ui24, se_ui32, se_ui64, se_ui_v8);
	VarInt = object
		function Bound(bytes: size_t): size_t;
	{$define intf :=
		function Bytes(const x: typ): size_t; static; {$ifdef signed} cinline {$endif}
		procedure Write(s: pStream; {$ifndef unsigned} const {$endif} x: typ); static; {$ifdef signed} cinline {$endif}} all_ints
		function Read(s: pStream): uint; static;
		function ReadLong(s: pStream): ulong; static;
		function StoreOne(x: uint; p: pointer; n: size_t): size_t; static;
		procedure Store(x: uint; var p: pointer; var n: size_t); static;
		function LoadOne(p: pointer; n: size_t; out len: size_t): uint; static;
		function Load(var p: pointer; var n: size_t): uint; static;
	private type
		pBase = ^Base;
		Base = uint8;
	const
		N_BITS = bitsizeof(Base) - 1;
		MASK = High(Base) shr 1;
		HAS_NEXT_BIT = 1 shl N_BITS;
	end;

	procedure Serialize_ui8(s: pStream; const x: uint);    function Deserialize_ui8(s: pStream): uint;
	procedure Serialize_ui16(s: pStream; const x: uint);   function Deserialize_ui16(s: pStream): uint;
	procedure Serialize_ui24(s: pStream; const x: uint);   function Deserialize_ui24(s: pStream): uint;
	procedure Serialize_ui32(s: pStream; const x: uint);   function Deserialize_ui32(s: pStream): uint;
	procedure Serialize_ui64(s: pStream; const x: uint64); function Deserialize_ui64(s: pStream): uint64;
{$define intf :=
	function Serialize_ui_bytes(const x: typ; fmt: UiBinaryFormat): size_t;
	procedure Serialize_ui(s: pStream; const x: typ; fmt: UiBinaryFormat);} all_ints
	function Deserialize_ui(s: pStream; fmt: UiBinaryFormat): uint;
	function Deserialize_ui_long(s: pStream; fmt: UiBinaryFormat): ulong;
	procedure Store_ui32(x: uint32; var p: pointer; var n: size_t); function Load_ui32(var p: pointer; var n: size_t): uint32;
	function Serialize_enum_bytes(x: uint; const prefixCodes: array of string): size_t;
	procedure Serialize_enum(s: pStream; x: uint; const prefixCodes: array of string);
	function Deserialize_enum(s: pStream; const prefixCodes: array of string): uint;

	procedure Serialize_colorRGB8(s: pStream; const color: Color); function Deserialize_colorRGB8(s: pStream): Color;
	procedure Serialize_datetime(s: pStream; const dt: DateTime);  function Deserialize_datetime(s: pStream): DateTime;

const
	UiBinaryFormatIds: array[UiBinaryFormat] of string = ('Fixed-8', 'Fixed-16', 'Fixed-24', 'Fixed-32', 'Fixed-64', 'V-8');
	UiBinaryFormatPrefixCodes: array[UiBinaryFormat] of string = ('b', 'w', '3', 'L', 'Q', 'v');
	UiBinaryFormatInfo: array[UiBinaryFormat] of record
		bytes: size_t;
	end =
	(
		(bytes: sizeof(uint8)),     // se_ui8
		(bytes: sizeof(uint16)),    // se_ui16
		(bytes: 3 * sizeof(uint8)), // se_ui24
		(bytes: sizeof(uint32)),    // se_ui32
		(bytes: sizeof(uint64)),    // se_ui64
		(bytes: 0)                  // se_ui_v8
	);

implementation

uses
	Utils, VirtualFS, UClasses, Random, Algo {$ifdef selftest}, Tests {$endif}
{$ifdef Debug}, ULog {$endif};

{$ifdef DebugZSteps}
	{$define zstep_dpr := writeln}
{$else}
	{$define zstep_dpr := //}
{$endif}

	function StreamPath.Relative(const src, base: string): string;
	begin
		result := Path(base) + src;
	end;

	function StreamPath.Navigate(const src, base: string): string;
	begin
		result := ForcePath(base) + src;
	end;

	function StreamPath.Resolve(const src: string): string;
	var
		i, sep, prevSep, removePrevSep, removeNextSep: sint;
		r: string absolute result;
	begin
		result := src;
		i := 1;
		sep := 0;
		while i <= length(r) do
		begin
			prevSep := sep;
			sep := Pos(FileSeparator, r, i + ord(i = 0));
			if sep = 0 then exit;
			i := sep + 1;

			if (sep > 1) and Prefixed('..', r, sep + 1) and ((sep + 3 > length(r)) or (r[sep + 3] = FileSeparator))
				and not ((sep = prevSep + 3) and (r[prevSep + 1] = '.') and (r[prevSep + 2] = '.'))
			then
			begin
				removePrevSep := ord((prevSep > 1) and (sep + 3 > length(r)));
				removeNextSep := ord((sep + 3 <= length(r)) and (r[sep + 3] = FileSeparator));
				delete(r, prevSep + 1 - removePrevSep, sep + length('..') - prevSep + removePrevSep + removeNextSep);
				i := prevSep;
				sep := Rpos(FileSeparator, r, prevSep - 1);
			end;
		end;
	end;

	function StreamPath.Resolve(const src, base: string): string;
	begin
		result := Resolve(Relative(src, base));
	end;

	function StreamPath.CutExtension(const src: string; ext: pString = nil): string;
	var
		i: sint;
	begin
		for i := length(src) downto 1 do
			case src[i] of
				FileSeparator: break;
				ExtensionSeparator:
					begin
						if Assigned(ext) then ext^ := Copy(src, i + 1, length(src) - i);
						exit(Copy(src, 1, i - 1));
					end;
			end;

		result := src;
		if Assigned(ext) then ext^ := '';
	end;

	function StreamPath.System(const src: string): string;      begin result := ToSystemFileName(src); end;
	function StreamPath.Human(const src: string): string;       begin result := Human(src, no); end;
	function StreamPath.Log(const src: string): string;         begin result := Human(src, yes); end;
	function StreamPath.Path(const src: string): string;        begin result := Folder.Path(src); end;
	function StreamPath.ForcePath(const src: string): string;   begin result := Folder.AppendSeparator(src); end;
	function StreamPath.ForceNoPath(const src: string): string; begin result := Folder.RemoveSeparator(src); end;
	function StreamPath.Filename(const src: string): string;    begin result := Folder.Filename(src); end;
	function StreamPath.FilenameNoExt(const src: string): string; begin result := Folder.FilenameNoExt(src); end;
	function StreamPath.FilenameNoExt(const src: string; ext: pString): string; begin result := CutExtension(Filename(src), ext); end;
	function StreamPath.Extension(const src: string): string;   begin result := Extension(src, 0); end;

	function StreamPath.Extension(const src: string; id: sint): string;
	var
		i, last: sint;
	begin
		last := length(src);
		for i := length(src) downto 1 do
			case src[i] of
				ExtensionSeparator:
					begin
						dec(id);
						if id >= 0 then last := i - 1 else exit(Copy(src, i + 1, last - i));
					end;
				FileSeparator: break;
			end;
		result := '';
	end;

	function StreamPath.Human(const src: string; quote: boolean): string;
	begin
		result := System(src);
		if quote then result := '"' + result + '"' else result := MaybeQuote(result);
	end;

	constructor StreamImage.Emulate(newData: pointer; newSize: size_t; ownDataPtr: boolean {$ifdef Debug}; const newSrcfn: string {$endif});
	begin
		inherited Init;
		_native := nil;
		_data := newData;
		_size := newSize;
		self.ownDataPtr := ownDataPtr;
	{$ifdef Debug} srcfn := newSrcfn; {$endif}
		NewRef;
	end;

	constructor StreamImage.Native(var f: &File; const ofs: FilePos; newSize: size_t; flags: FileFlags {$ifdef Debug}; const newSrcfn: string {$endif});
	begin
		inherited Init;
		_data := nil;
		new(_native);
		if not f.Mmap(_native^, ofs, newSize, flags) then ConstructorFailed;
		_data := _native^.Data;
		_size := newSize;
	{$ifdef Debug} srcfn := newSrcfn; {$endif}
		NewRef;
	end;

	destructor StreamImage.Done;
	begin
		if Assigned(_native) then
		begin
			_native^.Close;
			dispose(_native);
		{$ifdef Debug} if Assigned(_data) then Log('Отображение {0} ({1}) уничтожено', [StreamPath.Log(srcfn), _data]); {$endif}
		end else
			if ownDataPtr then FreeMem(_data);
		inherited Done;
	end;

	function StreamImage.AsString: string;
	begin
		result := USystem.ToString(Data, Size);
	end;

	constructor Stream.Init(newFlags: FileFlags; const newPath: string);
	begin
		inherited Init;
		_size := FileSize.Zero;
		_position := FilePos.Zero;
		path := newPath;
		_flags := newFlags;
	end;

	destructor Stream.Done;
	begin
		inherited Done;
	end;

	function Stream.CanSeek: boolean;
	begin
		result := yes;
	end;

	function Stream.Read(dest: pointer; count: size_t): pStream;
	var
		rd: size_t;
	begin
		result := @self;
		rd := TryRead(dest, count);
		if rd <> count then
			raise Error('Ошибка чтения из {0} (запрошено {1}, прочитано {2}).', StreamPath.Human(path), ToString(FileSize.Explicit(count)), ToString(FileSize.Explicit(rd)));
	end;

	function Stream.Write(source: pointer; count: size_t): pStream;
	var
		wt: size_t;
	begin
		result := @self;
		wt := TryWrite(source, count);
		if wt <> count then
			raise Error('Ошибка записи в {0} (запрошено {1}, записано {2}).', StreamPath.Human(path), ToString(FileSize.Explicit(count)), ToString(FileSize.Explicit(wt)));
	end;

	procedure Stream._SetSize(const newSize: FileSize);
	begin
		if _size <> newSize then
		begin
			_size := _Resize(newSize);
			_position.value := Min(_position.value, _size.value);
		end;
	end;

	procedure Stream._SetPosition(const newPos: FilePos);
	begin
		if _position <> newPos then
		begin
		{$ifdef Debug} Assert(CanSeek, path + ': этот поток не поддерживает Seek (' + ToString(_position) + ' -> ' + ToString(newPos) + ')'); {$endif}
			_position := _Seek(FilePos.Explicit(newPos.value)); // здесь был min с size, но в BlobCaches мешал.
		end;
	end;

	function Stream._Seek(const newPos: FilePos): FilePos;
	begin
		result := newPos;
	end;

	function Stream.Write(const s: string): pStream;
	begin
		result := Write(pointer(s), length(s) * sizeof(s[1]));
	end;

	procedure Stream.AppendFrom(s: pStream);
	begin
		AppendFrom(s, s^.Size - s^.Position);
	end;

	procedure Stream.AppendFrom(s: pStream; count: FileSize);
	var
		buf: pointer;
		sz: size_t;
	begin
		buf := nil;
		MakeRef(s);

		try
			buf := GetMem(min(count.value, TempBufferSize));
			while count.value > 0 do
			begin
				sz := min(count.value, TempBufferSize);
				s^.Read(buf, sz);
				self.Write(buf, sz);
				count -= sz;
			end;
		finally
			FreeMem(buf);
			Release(s);
		end;
	end;

	procedure Stream.AppendFrom(s: pStream; const start: FilePos; const count: FileSize);
	var
		op: FilePos;
	begin
		op := s^.Position;
		s^.Position := start;
		try
			AppendFrom(s, count);
		finally
			s^.Position := op;
		end;
	end;

	function Stream.Skip(n: FileSize): pStream;
	var
		buf: pointer;
		sz: size_t;
		static_buf: array[0 .. 7] of pointer;
	begin
		result := @self;
		if n.value <= FileSize.ValueType(sizeof(static_buf)) then
			Read(@static_buf, n.value)
		else
		begin
			buf := GetMem(min(n.value, TempBufferSize));
			try
				repeat
					sz := min(n.value, TempBufferSize);
					Read(buf, sz);
					n -= sz;
				until n = FileSize.Zero;
			finally
				FreeMem(buf);
			end;
		end;
	end;

	function Stream.Window(const pos: FilePos; const wsize: FileSize; const newPath: string): pStream;
	var
		data: pointer;
		op: FilePos;
	begin
		data := nil;
		result := nil;
		op := Position;
		Position := pos;
		try
			try
				data := GetMem(wsize.AsSizeT);
				Read(data, wsize.value);
				result := new(pMemoryStream, InitReadOnly(data, wsize.value))
			except
				FreeMem(data);
				raise;
			end;
			result^.path := newPath;
		finally
			if CanSeek then Position := op;
		end;
	end;

	function Stream.GetImage(const ofs: FilePos; imsize: size_t): pStreamImage;
	var
		data: pointer;
		op: FilePos;
	begin
		op := Position;
		Position := ofs;
		result := nil;
		data := GetMem(imsize);
		try
			Read(data, imsize);
			result := new(pStreamImage, Emulate(data, imsize, yes {$ifdef Debug}, path {$endif}));
		finally
			if not Assigned(result) then FreeMem(data);
		end;
		if CanSeek then Position := op;
	end;

	procedure Stream.DisableBuffering; begin end;
	procedure Stream.Flush; begin end;

	constructor FileStream.Init(const fileName: string; newFlags: FileFlags = DefaultFileFlags);
	var
		resolved: string;
{$ifdef Debug}
	var
		what: string;
{$endif}
	begin
		resolved := StreamPath.Resolve(fileName);
		mmap := Mapping.Invalid;
		inherited Init(newFlags, resolved);

		try
			f := &File.Open(path, _flags);
		except
			f := &File.Invalid;
			if file_JustTry in _flags then
			begin
			{$ifdef Debug} Log(Exception.Message + ' — так и задумано', logDebug); {$endif}
				ConstructorFailed;
			end else
			begin
			{$ifdef Debug} Exception.Log; {$endif}
				raise;
			end;
		end;

	{$ifdef Debug}
		what := '';
		if file_Read in _flags then
			if file_Write in _flags then what += 'на чтение и запись' else what += 'на чтение'
		else
			if file_Write in _flags then what += 'на запись' else what += 'не для чтения и не для записи';
		if file_SequentialAccess in _flags then
			if file_RandomAccess in _flags then what += ' с противоречивой информацией о последовательности доступа' else what += ' с последовательным доступом'
		else
			if file_RandomAccess in _flags then what += ' со случайным доступом';
		if what <> '' then what := ' ' + what;

		Log('Файл ' + StreamPath.Log(path) + ' открыт' + what + '.');
	{$endif}
		TrustedInit(path, {$ifdef Debug} path, {$endif} f, newFlags, yes, f.Size);
	end;

	constructor FileStream.TrustedInit(const newPath {$ifdef Debug}, newPhysFn {$endif}: string;
		const opened: &File; newFlags: FileFlags; fromCtr: boolean; const physicalSize: FileSize);
	begin
		if not fromCtr then inherited Init(newFlags, newPath);
		f := opened;
	{$ifdef Debug} physFn := newPhysFn; {$endif}
		physicalFileSize := physicalSize;
		_size := physicalSize;

		mmap := Mapping.Invalid;
		if file_Write in _flags then
			maxMmapSize := SystemInfo.pageSize
		else
			maxMmapSize := min(physicalFileSize.value, MmapSizeLimit);
		allowMmapGrow := yes;
		maxMmapSize := align(maxMmapSize, SystemInfo.pageSize);
	end;

	function FileStream.Open(const fileName: string; newFlags: FileFlags = DefaultFileFlags): pStream;
	begin
		result := MakeRef(new(pFileStream, Init(fileName, newFlags)));
	end;

	destructor FileStream.Done;
	begin
		CloseMmap;
		if f.OK then
		begin
			{$ifdef ExtDebug} LogR('Закрываю ' + StreamPath.Log(info) + '... ', logDebug); {$endif}
			if (file_Write in _flags) and (physicalFileSize > size) then
				try
					f.Size := Size;
				except
				{$ifdef Debug} Exception.Show; {$endif}
				end;
			f.Close;
		end;
	{$ifdef Debug} physFn := ''; {$endif}
		inherited Done;
	end;

	function FileStream.CreateTemp(const base: string; mode: &File.TempMode = CreateAndOpenTemp): pFileStream;
	var
		t: &File;
		fn: string;
	begin
		t  := &File.CreateTemp(base, fn, mode);
		fn := StreamPath.Resolve(fn);
		result := new(pFileStream, TrustedInit(fn {$ifdef Debug}, fn {$endif}, t, [file_Read, file_Write, file_Temp], no, t.Size))^.NewRef;
	end;

	function FileStream.IO(target: pointer; count: size_t; isRead: boolean): size_t;

		procedure DoMmapIo(count: size_t);
		begin
			if isRead then
				memcpy(mmap.Data + (_position - mmapStart).value, target, count)
			else
				memcpy(target, mmap.Data + (_position - mmapStart).value, count);
			_position += count;
			result += count;
		end;

	var
		mmapRest, proceed, refMmapSize: size_t;
		refMmapStart, refMmapEnd: FilePos;
		useRaw: boolean;
	begin
		result := 0;
		// [    map   ]
		//        [    query    ]
		if mmap.OK and (_position >= mmapStart) and (_position <= mmapEnd) then // «<=» — пропускает count = 0 в DoMmapIo, где тот ничего не делает.
		begin
			mmapRest := (mmapEnd - _position).value;
			if mmapRest >= count then
			begin
				DoMmapIo(count);
				exit;
			end else
			begin
				DoMmapIo(mmapRest);
				target += mmapRest;
				count -= mmapRest;
			end;
		end;

		useRaw := maxMmapSize = 0;
		if not useRaw then
		begin
			CloseMmap;
			repeat
				refMmapStart := FilePos.Explicit(align_left(_position.value, SystemInfo.allocationGranularity));
				refMmapEnd := FilePos.Explicit(min(align((_position + maxMmapSize).value, SystemInfo.pageSize), physicalFileSize.value));
				refMmapSize := (refMmapEnd - refMmapStart).value;
				useRaw := _position + count >= refMmapEnd; // то же с >=
				if useRaw then break;

				if f.Mmap(mmap, refMmapStart, refMmapSize, _flags) then
				begin
					mmapStart := refMmapStart;
					mmapSize := refMmapSize;
					mmapEnd := mmapStart + mmapSize;
				{$ifdef Debug}
					Log('Сегмент файла ' + StreamPath.Log(physFn) + ' (начало: ' + ToString(mmapStart) + ', размер: ' + ToStringSuff_b(mmapSize) + ') за''mmap''лен в ' + ToString(mmap.data) + ' (position = ' + ToString(_position) + ')', logOK); {$endif}
					break;
				end else
				begin
				{$ifdef Debug} LogR('Не удалось за''mmap''ить сегмент файла ' + StreamPath.Log(physFn) + ' (начало: ' + ToString(refMmapStart) + ', размер: ' + ToStringSuff_b(refMmapSize) + '). ', logError); {$endif}
					allowMmapGrow := no;
					refMmapSize := refMmapSize div 2;
					if refMmapSize < SystemInfo.pageSize then
					begin
					{$ifdef Debug} Log('Безнадёжно. Впредь будет использован обычный I/O.', logError); {$endif}
						maxMmapSize := 0;
						useRaw := yes;
						break;
					end else
					begin
						maxMmapSize := align(refMmapSize, SystemInfo.pageSize);
					{$ifdef Debug} Log('Попробую ещё раз.', logError); {$endif}
					end;
				end;
			until no;
		end;

		if useRaw then
		begin
			if isRead then proceed := f.Read(_position, target, count) else proceed := f.Write(_position, target, count);
			_position += proceed;
			result += proceed;
		end else
			DoMmapIo(count);
	end;

	procedure FileStream.CloseMmap;
	begin
		if mmap.OK then
		begin
		{$ifdef Debug} Log('Отображение ' + StreamPath.Log(physFn) + ' (' + ToString(mmap.data) + ', начало: ' + ToString(mmapStart) + ', размер: ' + ToStringSuff_b(mmapSize) + ') уничтожено', logDebug); {$endif}
			mmap.Close;
		end;
	end;

	function FileStream.TryRead(dest: pointer; count: size_t): size_t;
	begin
		if not f.OK or not (file_Read in _flags) then exit(0);
		if _position + count > _size then count := (_size - _position).value;
		result := IO(dest, count, yes);
	end;

	function FileStream.TryWrite(source: pointer; count: size_t): size_t;
	var
		np: FilePos;
		newMaxMap, bonus: size_t;
	{$ifdef Debug} msg: string; {$endif}
	begin
		if not f.OK or not (file_Write in _flags) then exit(0);
	{$ifdef Debug} msg := ''; {$endif}

		if maxMmapSize > 0 then
		begin
			np := _position + count;
			if np > physicalFileSize then
			begin
				bonus := 2 * SystemInfo.pageSize;
				physicalFileSize := FileSize.Explicit(align(np.value + maxMmapSize + bonus, SystemInfo.pageSize));
				if allowMmapGrow and (maxMmapSize < physicalFileSize.value div 2) then
				begin
					newMaxMap := align(min(physicalFileSize.value div 2, MmapSizeLimit), SystemInfo.pageSize);
					physicalFileSize := FileSize.Explicit(align(max(physicalFileSize.value, np.value + newMaxMap + bonus), SystemInfo.pageSize));
				{$ifdef Debug} if maxMmapSize <> newMaxMap then msg := ' (отображение до ' + ToStringSuff_b(newMaxMap) + ')'; {$endif}
					allowMmapGrow := (maxMmapSize <> newMaxMap) and (newMaxMap < MmapSizeLimit);
					maxMmapSize := newMaxMap;
				end;
			{$ifdef Debug}
				msg := StreamPath.Log(physFn) + ': размер увеличен с ' + ToString(f.Size) + ' до ' + ToString(physicalFileSize) + msg;
				msg += '; ';
				LogR(msg, logDebug);
			{$endif}
				f.Size := physicalFileSize;
			end;
		end;

		result := IO(source, count, no);
		if _position > _size then _size := _position;
	end;

	function FileStream.Window(const pos: FilePos; const wsize: FileSize; const newPath: string): pStream;
	begin
	{$ifdef Debug}
		Log('Сегмент файла ' + StreamPath.Log(physFn) + ' (начало: ' + ToString(pos) + ', размер: ' + ToString(wsize) + ') запрошен' +
		    ' отдельным потоком под видом ' + StreamPath.Log(newPath) + '.', logDebug);
	{$endif}
		result := new(pFileStream, TrustedInit(newPath, {$ifdef Debug} physFn, {$endif} f.Duplicate, [file_Read], no, physicalFileSize));
		if Assigned(result) then
		begin
			result^.Position := pos;
			result^.Size := pos + wsize;
		end;
	end;

	function FileStream.GetImage(const ofs: FilePos; imsize: size_t): pStreamImage;
	begin
		result := new(pStreamImage, Native(f, ofs, imsize, [file_Read] {$ifdef Debug}, physFn {$endif}));
		if Assigned(result) then
		begin
		{$ifdef Debug} Log('Сегмент файла ' + StreamPath.Log(physFn) + ' (начало: ' + ToString(ofs) + ', размер: ' + ToStringSuff_b(imsize) + ') за''mmap''лен в ' + ToString(result^.data), logOK); {$endif}
		end else
		begin
		{$ifdef Debug} Log('Не удалось за''mmap''ить кусок ' + StreamPath.Log(physFn) + ' (начало: ' + ToString(ofs) + ', размер: ' + ToStringSuff_b(imsize) + '). Попробую эмулировать.', logError); {$endif}
			result := inherited GetImage(ofs, imsize);
		end;
	end;

	procedure FileStream.DisableBuffering; begin CloseMmap; end;
	procedure FileStream.Flush; begin CloseMmap; end;

	function FileStream._Resize(const newSize: FileSize): FileSize;
	begin
		if mmapEnd > newSize then CloseMmap;
		if file_Write in _flags then
		begin
			f.Size := newSize;
			result := f.Size;
		end else
			result := FileSize.Explicit(min(f.Size.value, newSize.value));
		physicalFileSize := result;
	end;

	constructor MemoryStream.InitReadOnly(newP: pointer; newSize: size_t; newOwnsPtr: boolean = yes);
	begin
		inherited Init([file_Read], '');
		ptr := newP;
		_size := FileSize.Explicit(newSize);
		ownsPtr := newOwnsPtr;
	end;

	constructor MemoryStream.InitWriteOnly(newSize: size_t = 0);
	begin
		inherited Init([file_Write], '');
		allocated := newSize;
		_size := FileSize.Zero;
		ptr := GetMem(newSize);
		ownsPtr := yes;
	end;

	constructor MemoryStream.InitRW;
	begin
		inherited Init([file_Read, file_Write], '');
		allocated := 0;
		_size := FileSize.Zero;
		ptr := nil;
		ownsPtr := yes;
	end;

	destructor MemoryStream.Done;
	begin
		if ownsPtr then FreeMem(ptr);
		inherited Done;
	end;

	function MemoryStream.TryRead(dest: pointer; count: size_t): size_t;
	begin
		Assert(file_Read in _flags);
		result := min((_size - _position).value, count);
		memcpy(ptr + _position.value, dest, result);
		_position += result;
	end;

	function MemoryStream.TryWrite(source: pointer; count: size_t): size_t;
	var
		newPos: size_t;
	begin
		Assert(file_Write in _flags);
		newPos := _position.value + count;
		if newPos > allocated then
		begin
			allocated := 2 * newPos;
			ReallocMem(ptr, allocated);
		end;
		memcpy(source, ptr + _position.value, count);
		_position := FilePos.Explicit(newPos);
		if _position > _size then _size := _position;
		result := count;
	end;

	function MemoryStream.Window(const pos: FilePos; const wsize: FileSize; const newPath: string): pStream;
	begin
		result := new(pMemoryStream, InitReadOnly(ptr + pos.AsSizeT, wsize.AsSizeT, no));
		result^.path := newPath;
	end;

	function MemoryStream.GetImage(const ofs: FilePos; imsize: size_t): pStreamImage;
	begin
		result := new(pStreamImage, Emulate(ptr + ofs.AsSizeT, imsize, no {$ifdef Debug}, '<mem>' {$endif}));
	end;

	function MemoryStream._Resize(const newSize: FileSize): FileSize;
	begin
		ReallocMem(ptr, newSize.value);
		allocated := newSize.value;
		result := newSize;
	end;

	function BlockStream.Open(newBase: pStream; mode: FileFlag; newEndb: EndBehaviour): BlockStream;
	var
		bs: BlockStream absolute result;
		ok: boolean;
	begin
		Assert(mode in [file_Read, file_Write]);
		ok := no;
		bs.base := MakeRef(newBase);
		bs.endb := newEndb;
		bs.buf := nil;
		bs.broken := no;

		try
			if not (mode in bs.base^.flags) then raise Error('Режим потока блоков не совместим с подлежащим потоком.');
			bs.reading := mode = file_Read;

			if bs.reading then
			begin
				bs.blockSize := VarInt.Read(bs.base);
				if bs.blockSize = 0 then raise Error('Неверный размер блока в потоке блоков.');
				bs.pos := 0;
				bs.breakAfterThis := no;
			end else
			begin
				bs.blockSize := DefaultBlockSize;
				bs.buf := GetMem(bs.blockSize);
				bs.pos := 0;
				bs.headerReady := no;
			end;
			ok := yes;
		finally
			if not ok then
			begin
				bs.broken := yes;
				bs.Close;
			end;
		end;
	end;

	procedure BlockStream.Close;
	var
		tmp: pointer;
		sz: size_t;
	begin
		if Assigned(base) then
		begin
			if reading then
			begin
				if (not broken) and not (AllowAbandon in endb) then
				begin
				{$ifdef Debug} Log('BlockStream: поток не завершён (' + ToString(pos) + '). Завершаю руками.', logError); {$endif}
					sz := Stream.TempBufferSize;
					tmp := GetMem(sz);
					try while Read(tmp, sz) = sz do; finally FreeMem(tmp); end;
				end;
			end else
				if not broken then WriteBuffer(yes);
		end;
		FreeMem(buf);
		Release(base);
	end;

	function BlockStream.Read(data: pointer; size: size_t): size_t;
	var
		n: size_t;
	begin
		Assert(reading);
		result := 0;
		while (size > 0) and not broken do
		begin
			if size < pos then n := size else n := pos;
			try
				base^.Read(data, n);
			except
				broken := yes;
				raise;
			end;
			result += n;
			pos -= n;
			size -= n;
			data += n;
			if pos = 0 then
				if breakAfterThis then broken := yes else
				begin
					n := VarInt.Read(base);
					breakAfterThis := n and 1 <> 0;
					pos := n shr 1;
					if pos = 0 then pos := blockSize;
				end;
		end;
	end;

	procedure BlockStream.Write(data: pointer; size: size_t);
	var
		n: size_t;
	begin
		Assert(not reading);
		while size > 0 do
			if pos < blockSize then
			begin
				n := min(size, blockSize - pos);
				memcpy(data, buf + pos, n);
				pos += n;
				data += n;
				size -= n;
			end else
				WriteBuffer(no);
	end;

	procedure BlockStream.EnsureHeader(last: boolean);
	var
		sz: size_t;
	begin
		if headerReady then exit;
		if last then sz := pos else sz := blockSize;
		VarInt.Write(base, sz);
		headerReady := yes;
	end;

	procedure BlockStream.WriteBuffer(last: boolean);
	var
		n: size_t;
	begin
		EnsureHeader(last);
		Assert((pos > 0) or last);
		if pos = blockSize then n := 0 else
		begin
			Assert(last);
			n := pos;
		end;
		VarInt.Write(base, (n shl 1) or size_t(last));
		base^.Write(buf, pos);
		pos := 0;
	end;

	constructor ZStream.Init(base: pStream; endb: EndBehaviour);
	begin
		if not Assigned(MakeRef(base)) then raise _NRE;
		sizeSet := no;
		try
			inherited Init(base^.Flags, base^.path);
			if not _Init(base, Method(0), Decompress, endb) then raise _InitFailed(base, Method(0), Decompress);
		finally
			Release(base);
		end;
	end;

	constructor ZStream.Init(base: pStream; aMethod: Method);
	begin
		if not Assigned(MakeRef(base)) then raise _NRE;
		try
			inherited Init(base^.Flags, base^.path);
			if not _Init(base, aMethod, Compress, []) then raise _InitFailed(base, aMethod, Compress);
		finally
			Release(base);
		end;
	end;

	destructor ZStream.Done;
	begin
		if Assigned(impl) then dispose(impl, Done);
		inherited Done;
	end;

	function ZStream.TryRead(dest: pointer; count: size_t): size_t;
	begin
	{$ifdef Debug} Assert(srcMode = Decompress); {$endif}
		if sizeSet and (position + count > size) then count := (size - position).value;
		result := impl^.Read(dest, count);
		_position += result;
	end;

	function ZStream.TryWrite(source: pointer; count: size_t): size_t;
	begin
	{$ifdef Debug} Assert(srcMode = Compress); {$endif}
		result := impl^.Write(source, count);
		_position += result;
	end;

	function ZStream.CanSeek: boolean;
	begin
		result := no;
	end;

	function ZStream._Resize(const newSize: FileSize): FileSize;
	begin
		Assert(impl^.mode = Decompress);
		if sizeSet then raise Error('Размер потока распаковки уже задан.');
		result := newSize;
		sizeSet := yes;
	end;

	function ZStream._Seek(const newPos: FilePos): FilePos;
	begin
		Assert(no);
		result := inherited _Seek(newPos);
	end;

	function ZStream._Init(base: pStream; aMethod: Method; mode: ModeEnum; endb: EndBehaviour): boolean;
	begin
		result := no;
		impl := nil;
	{$ifdef Debug} if TypeOf(base^) = TypeOf(ZStream) then Log('Вряд ли заворачивать Z-stream в Z-stream — хорошая идея...', logWarning); {$endif}

		if (file_Write in base^.flags) and (mode = Compress) then
		begin
		{$ifdef Debug} LogR('Открываю поток сжатия из ' + StreamPath.Log(base^.path) + ' (метод: ' + MethodIds[aMethod] + ')... '); {$endif}
			mode := Compress;
			Serialize_ui8(base, ord(aMethod));
		end else
			if (file_Read in base^.flags) and (mode = Decompress) then
			begin
				mode := Decompress;
			{$ifdef Debug} LogR('Открываю поток распаковки из ' + StreamPath.Log(base^.path) + '... '); {$endif}
				try
					aMethod := Method(RangeCheck(Deserialize_ui8(base), ord(High(Method)), 'ZStream.Method'));
				except
				{$ifdef Debug} Log('Данные повреждены', logError); {$endif}
					exit;
				end;
			{$ifdef Debug} LogR('Метод: ' + MethodIds[aMethod] + '; ', logOK); {$endif}
			end else
			begin
			{$ifdef Debug} Log('Режим неверен.', logError); {$endif}
				exit;
			end;
		{$ifdef Debug} srcMode := mode; {$endif}

		case aMethod of
			Method.Deflate: impl := new(pStreamed, Init(base, mode, new(pDeflate, Init(mode)), endb));
			Method.Lzo:     impl := new(pBlocked, Init(base, mode, new(pLzo, Init), endb));
			Method.Bzip2:   impl := new(pStreamed, Init(base, mode, new(pBzip2, Init(mode)), endb));
			Method.Lzham:   impl := new(pStreamed, Init(base, mode, new(pLzham, Init(mode)), endb));
			Method.QuickLZ: impl := new(pBlocked, Init(base, mode, new(pQuickLZ, Init), endb));
		end;

		result := Assigned(impl);
	end;

	constructor ZStream.ImplBase.Init(newBase: pStream; newMode: ModeEnum; newEndb: EndBehaviour);
	begin
		base := MakeRef(newBase);
		mode := newMode;
		endb := newEndb;
	end;

	destructor ZStream.ImplBase.Done;
	begin
		Release(base);
	end;

	function ZStream._NRE: Exception;
	begin
		result := Error('Поток не задан.');
	end;

	function ZStream._InitFailed(base: pStream; aMethod: Method; mode: ModeEnum): Exception;
	begin
		if mode = Compress then
			result := Error('Не удаётся открыть поток сжатия в {0} ({1}).', StreamPath.Human(base^.path), MethodIds[aMethod])
		else
			result := Error('Не удаётся открыть поток распаковки из {0}.', StreamPath.Human(base^.path));
		Done;
	end;

	constructor ZStream.StreamAdapter.Init(newMode: ModeEnum);
	begin
		srcMode := newMode;
	end;

	destructor ZStream.StreamAdapter.Done;
	begin
	end;

	function ZStream.StreamAdapter.Advance(mode: ModeEnum; input: pointer; nInput: size_t; output: pointer; nOutput: size_t; out consumed, written: size_t): AdvanceResult;
	begin
		Assert(mode = srcMode);
		result := AdvanceImpl(mode, input, nInput, output, nOutput, consumed, written);
	end;

	constructor ZStream.Streamed.Init(newBase: pStream; newMode: ModeEnum; newZs: pStreamAdapter; newEndb: EndBehaviour);
		function fmode(cmode: ModeEnum): FileFlag;
		begin
			if cmode = Compress then result := file_Write else result := file_Read;
		end;
	begin
		if not Assigned(newZs) then Fail;
		bs := BlockStream.Open(newBase, fmode(newMode), newEndb);
		inherited Init(newBase, newMode, newEndb);
		zs := newZs;
		buf := nil;
		case mode of
			Compress: bufAllocated := MaxCompressBufferSize;
			Decompress:
				bufAllocated := min(zs^.Bound(min((base^.Size - base^.Position).value, MaxDecompressBufferSize)), MaxDecompressBufferSize);
		end;
		buf := GetMem(bufAllocated);
		bufAvail := 0;
		broken := no;
		finished := no;
	end;

	destructor ZStream.Streamed.Done;
	const
		DummyPtr: pointer = @MethodIds;
	begin
		case mode of
			Compress: if not finished then _Write(DummyPtr, 0);
			Decompress:
				begin
					if (not finished) and not (AllowAbandon in endb) then
					begin
						_Read(DummyPtr, 0);
					{$ifdef Debug} if (not (broken or finished)) and not (AllowAbandon in endb) then Log('Z-stream не прочитан полностью', logWarning); {$endif}
					end;
				end;
		end;

		bs.Close;
		dispose(zs, Done);
		FreeMem(buf);
		inherited Done;
	end;

	function ZStream.Streamed._Read(dest: pointer; count: size_t): size_t;
	var
		consumed, written: size_t;
		shouldFinish: boolean;
	begin
		result := 0;
		if broken or finished then exit;
		shouldFinish := count = 0;

		repeat
			if bufAvail = 0 then
			begin
				bufAvail := bs.Read(buf, bufAllocated);
				bufPos   := buf;
			end;
			case zs^.Advance(Decompress, bufPos, bufAvail, dest, count, consumed, written) of
				advance_Ok: ;
				advance_Finished:
					begin
						if bufAvail > consumed then
						begin
							// base^.Position := base^.Position - (bufAfail - consumed);
						{$ifdef Debug} Log('ZStream.Streamed.Read: лишние данные (' + ToString(bufAvail - consumed) + '). В текущей реализации это невозможно и, должно быть, означает ошибку декодера. Так и запишем.', logError); {$endif}
							broken := yes;
						end;
						finished := yes;
					end;
				else broken := yes;
			end;
			bufPos += consumed;
			bufAvail -= consumed;
			dest += written;
			count -= written;
			result += written;
		until ((count = 0) and not shouldFinish) or finished or broken;
	end;

	function ZStream.Streamed._Write(source: pointer; count: size_t): size_t;
	var
		consumed, written: size_t;
		shouldFinish: boolean;
	begin
	{$define broken_and_break:=begin broken := yes; break; end}
		result := 0;
		Assert(not finished);
		if broken then exit;
		shouldFinish := count = 0;

		repeat
			case zs^.Advance(Compress, source, count, buf, bufAllocated, consumed, written) of
				advance_Ok: ;
				advance_Finished: finished := yes;
				else broken_and_break;
			end;
			bs.Write(buf, written);
			source += consumed;
			count -= consumed;
			result += consumed;
		until ((count = 0) and not shouldFinish) or finished or broken;
	{$undef broken_and_break}
	end;

	function ZStream.Streamed.Read(dest: pointer; count: size_t): size_t;
	begin
		if count > 0 then result := _Read(dest, count) else result := 0;
	end;

	function ZStream.Streamed.Write(source: pointer; count: size_t): size_t;
	begin
		if count > 0 then result := _Write(source, count) else result := 0;
	end;

	constructor ZStream.Deflate.Init(newMode: ModeEnum);
	var
		r: cint;
	begin
		try
			z.loader.Load;
		except
			instant_reraise_from_constructor;
		end;

		try
			zs := z.EmptyStream;
			case newMode of
				Compress:
					begin
						r := z.deflateInit2(zs, z.BEST_COMPRESSION, z.DEFLATED, -15, 9, z.DEFAULT_STRATEGY, z.VERSION, sizeof(zs));
					{$ifdef Debug} if r <> z.OK then Error('deflateInit', r); {$endif}
					end;
				Decompress:
					begin
						r := z.inflateInit2(zs, -15, z.VERSION, sizeof(zs));
					{$ifdef Debug} if r <> z.OK then Error('inflateInit', r); {$endif}
					end;
				else Assert(no);
			end;
			if r <> z.OK then raise USystem.Error(z.ErrorMessage(r, zs));
		except
			z.loader.Unload;
			instant_reraise_from_constructor;
		end;
		inherited Init(newMode);
	end;

	destructor ZStream.Deflate.Done;
	var
		r: cint;
	begin
		if instantly_reraised_from_constructor then exit;
		case srcMode of
			Compress:
				begin
					r := z.deflateEnd(zs);
					if r <> z.OK then {$ifdef Debug} Error('deflateEnd', r) {$endif};
				end;
			Decompress:
				begin
					r := z.inflateEnd(zs);
					if r <> z.OK then {$ifdef Debug} Error('inflateEnd', r) {$endif};
				end;
		end;
		inherited Done;
		z.loader.Unload;
	end;

	function ZStream.Deflate.AdvanceImpl(mode: ModeEnum; input: pointer; nInput: size_t; output: pointer; nOutput: size_t;
		out consumed, written: size_t): AdvanceResult;
	var
		r: cint;
	begin
		zs.next_in := input; zs.avail_in := nInput;
		zs.next_out := output; zs.avail_out := nOutput;
		zstep_dpr('deflate before adv: avail_in = ', zs.avail_in, ', avail_out = ', zs.avail_out);

		case mode of
			Compress:
				begin
					r := z.deflate(zs, IfThen(nInput = 0, z.FINISH, z.NO_FLUSH));
					if r = z.OK then result := advance_Ok else
						if (r = z.STREAM_END) and (zs.avail_in = 0) then result := advance_Finished else
						begin
						{$ifdef Debug} Error('deflate', r); {$endif}
							result := advance_Error;
						end;
				end;
			Decompress:
				begin
					r := z.inflate(zs, z.NO_FLUSH);
					if r = z.OK then result := advance_Ok else
						if r = z.STREAM_END then result := advance_Finished else
						begin
						{$ifdef Debug} Error('inflate', r); {$endif}
							result := advance_Error;
						end;
				end;
		end;

		consumed := nInput - zs.avail_in;
		written  := nOutput - zs.avail_out;
		zstep_dpr('deflate adv: avail_in = ', zs.avail_in, ', avail_out = ', zs.avail_out, ', r = ', r);
	end;

	function ZStream.Deflate.Bound(const uncompressed: size_t): size_t;
	begin
		result := z.deflateBound(zs, uncompressed);
	end;

{$ifdef Debug}
	procedure ZStream.Deflate.Error(const where: string; code: sint);
	begin
		Log('Ошибка Deflate.' + where + '. ' + z.ErrorMessage(code, zs), logError);
	end;
{$endif}

	constructor ZStream.BlockAdapter.Init; begin end;
	destructor  ZStream.BlockAdapter.Done; begin end;

	constructor ZStream.Blocked.Init(newBase: pStream; newMode: ModeEnum; newZs: pBlockAdapter; newEndb: EndBehaviour);
	begin
		inherited Init(newBase, newMode, newEndb);
		zs := newZs;
		inbuf := nil;
		outbuf := nil;
		broken := yes;
		finished := no;
		headerReady := no;
		case mode of
			Compress:
				begin
					inalloc := BlockSize;
					outalloc := zs^.Bound(inalloc);
				end;
			Decompress:
				begin
					inalloc := VarInt.Read(base);
					if inalloc <= ReasonableBlockSizeLimit then
					begin
					{$ifdef Debug} LogR('Размер блока: ' + ToStringSuff_b(inalloc) + '; ', logDebug); {$endif}
					end else
					begin
					{$ifdef Debug} Log('Неправдоподобный размер блока; данные повреждены', logError); {$endif}
						ConstructorFailed;
					end;
					outalloc := zs^.Bound(inalloc);
				end;
			else raise ExhaustiveCase(ord(mode), 'Blocked.mode');
		end;
		inbuf := GetMem(inalloc + outalloc);
		outbuf := inbuf + inalloc;
		inpos := 0;
		insize := 0;
		broken := no;
	end;

	destructor ZStream.Blocked.Done;
	begin
		case mode of
			Compress: if not broken then WriteBuffer(yes);
			Decompress:
				begin
					if (not broken) and (not finished) and not (AllowAbandon in endb) then ReadBuffer(yes);
				{$ifdef Debug} if not (broken or finished) then Log('Поток блочного сжатия не прочитан полностью.', logWarning); {$endif}
				end;
		end;
		dispose(zs, Done);
		FreeMem(inbuf);
		inherited Done;
	end;

	procedure ZStream.Blocked.EnsureHeader(last: boolean);
	var
		sz: size_t;
	begin
		Assert(mode = Compress);
		if headerReady then exit;

		if last then sz := inpos else sz := inalloc;
		VarInt.Write(base, sz);
		headerReady := yes;
	end;

	function ZStream.Blocked.Read(dest: pointer; count: size_t): size_t;
	var
		n: size_t;
	begin
		result := 0;
		if broken then exit;
		while count > 0 do
		begin
			if inpos = insize then
				if finished or not ReadBuffer(no) then break;
			n := min(count, insize - inpos);
			memcpy(inbuf + inpos, dest, n);
			inpos += n;
			dest += n;
			result += n;
			count -= n;
		end;
	end;

	function ZStream.Blocked.Write(source: pointer; count: size_t): size_t;
	var
		n: size_t;
	begin
		result := 0;
		while (count > 0) and not broken do
			if (inpos < inalloc) or WriteBuffer(no) then
			begin
				n := min(count, inalloc - inpos);
				memcpy(source, inbuf + inpos, n);
				inpos += n;
				source += n;
				result += n;
				count -= n;
			end;
	end;

	function ZStream.Blocked.ReadBuffer(last: boolean): boolean;
	label _finally_;
	var
		compressedSize: size_t;
	begin
		Assert(not finished);
		result := no;
		inpos := 0;
		insize := 0;
		compressedSize := VarInt.Read(base);
		finished := compressedSize and 1 <> 0;
		compressedSize := compressedSize shr 1;
		if (compressedSize > outalloc) or ((compressedSize = 0) and not last) then
		begin
		{$ifdef Debug} Log('Неверный размер сжатого блока (' + ToString(compressedSize) + '; выделено ' + ToString(outalloc) + '), данные повреждены', logError); {$endif}
			goto _finally_;
		end;
		base^.Read(outbuf, compressedSize);
		if compressedSize > 0 then
			try
				zs^.Decompress(outbuf, compressedSize, inbuf, inalloc, insize);
				result := yes;
			except
				// потом придумаю что-нибудь ._.
			end;
	_finally_:
		if not result then broken := yes;
	end;

	function ZStream.Blocked.WriteBuffer(last: boolean): boolean;
	var
		compressed: size_t;
	begin
		try
			result := no;
			EnsureHeader(last);
			Assert((inpos > 0) or last);
			if inpos > 0 then
				zs^.Compress(inbuf, inpos, outbuf, outalloc, compressed)
			else
				compressed := 0;
			VarInt.Write(base, (compressed shl 1) or size_t(last));
			base^.Write(outbuf, compressed);
			inpos := 0;
			result := yes;
		finally
			if not result then broken := yes;
		end;
	end;

	constructor ZStream.LzoAdapter.Init;
	begin
		try
			lzo.loader.Load;
		except
			instant_reraise_from_constructor;
		end;
		inherited Init;
	end;

	destructor ZStream.LzoAdapter.Done;
	begin
		if instantly_reraised_from_constructor then exit;
		inherited Done;
		lzo.loader.Unload;
	end;

	function ZStream.LzoAdapter.Bound(const uncompressed: size_t): size_t;
	begin
		result := lzo.bound(uncompressed);
	end;

	procedure ZStream.LzoAdapter.Compress(block: pointer; size: size_t; output: pointer; outputSize: size_t; out compressedSize: size_t);
	var
		r: cint;
		outsize: cuint;
		mem: packed array[0 .. lzo.COMPRESS_WORK_MEM - 1] of byte;
	begin
		outsize := outputSize;
		r := lzo.compress(block, size, output, outsize, @mem);
		compressedSize := outsize;
		if r <> lzo.OK then raise Error('Compress', r);
	end;

	procedure ZStream.LzoAdapter.Decompress(block: pointer; size: size_t; output: pointer; outputSize: size_t; out decompressedSize: size_t);
	var
		r: cint;
		outsize: cuint;
	begin
		outsize := outputSize;
		r := lzo.decompress(block, size, output, outsize, nil);
		decompressedSize := outsize;
		if r <> lzo.OK then raise Error('Decompress(size = ' + ToString(size) + ', outputSize = ' + ToString(outputSize) + ')', r);
	end;

	function ZStream.LzoAdapter.Error(const where: string; code: cint): Exception;
	begin
		result := USystem.Error('Ошибка Lzo.' + where + '. ' + lzo.ErrorMessage(code));
	{$ifdef Debug} Log(result.RawMessage, logError); {$endif}
	end;

	constructor ZStream.Bzip2Adapter.Init(newMode: ModeEnum);
	var
		r: cint;
	begin
		try
			bzip2.loader.Load;
		except
			instant_reraise_from_constructor;
		end;

		try
			bz := bzip2.EmptyStream;
			case newMode of
				Compress:
					begin
						r := bzip2.CompressInit(bz, 9, 0, 0);
					{$ifdef Debug} if r <> bzip2.OK then Error('CompressInit', r); {$endif}
					end;
				Decompress:
					begin
						r := bzip2.DecompressInit(bz, 0, 0);
					{$ifdef Debug} if r <> bzip2.OK then Error('DecompressInit', r); {$endif}
					end;
				else Assert(no);
			end;

			if r <> bzip2.OK then raise USystem.Error(bzip2.ErrorMessage(r));
		except
			bzip2.loader.Unload;
			instant_reraise_from_constructor;
		end;
		inherited Init(newMode);
	end;

	destructor ZStream.Bzip2Adapter.Done;
	var
		r: cint;
	begin
		if instantly_reraised_from_constructor then exit;
		case srcMode of
			Compress:
				begin
					r := bzip2.CompressEnd(bz);
					if r <> bzip2.OK then {$ifdef Debug} Error('CompressEnd', r) {$endif};
				end;
			Decompress:
				begin
					r := bzip2.DecompressEnd(bz);
					if r <> bzip2.OK then {$ifdef Debug} Error('DecompressEnd', r) {$endif};
				end;
		end;
		inherited Done;
		bzip2.loader.Unload;
	end;

	function ZStream.Bzip2Adapter.AdvanceImpl(mode: ModeEnum; input: pointer; nInput: size_t; output: pointer; nOutput: size_t;
		out consumed, written: size_t): AdvanceResult;
	var
		r: cint;
	begin
		bz.next_in  := input;  bz.avail_in := nInput;
		bz.next_out := output; bz.avail_out := nOutput;
		zstep_dpr('bzip2 before adv: avail_in = ', bz.avail_in, ', avail_out = ', bz.avail_out);

		case mode of
			Compress:
				begin
					r := bzip2.Compress(bz, IfThen(nInput = 0, bzip2.FINISH, bzip2.RUN));
					if (r = bzip2.RUN_OK) or (r = bzip2.FINISH_OK) then result := advance_Ok else
						if (r = bzip2.STREAM_END) and (bz.avail_in = 0) then result := advance_Finished else
						begin
						{$ifdef Debug} Error('Compress', r); {$endif}
							result := advance_Error;
						end;
				end;
			Decompress:
				begin
					r := bzip2.Decompress(bz);
					if r = bzip2.OK then result := advance_Ok else
						if r = bzip2.STREAM_END then result := advance_Finished else
						begin
						{$ifdef Debug} Error('Decompress', r); {$endif}
							result := advance_Error;
						end;
				end;
		end;

		consumed := nInput - bz.avail_in;
		written  := nOutput - bz.avail_out;
		zstep_dpr('bzip2 adv: avail_in = ', bz.avail_in, ', avail_out = ', bz.avail_out, ', r = ', r);
	end;

	function ZStream.Bzip2Adapter.Bound(const uncompressed: size_t): size_t;
	begin
		result := bzip2.Bound(uncompressed);
	end;

{$ifdef Debug}
	procedure ZStream.Bzip2Adapter.Error(const where: string; code: sint);
	begin
		Log('Ошибка Bzip2.' + where + '. ' + bzip2.ErrorMessage(code), logError);
	end;
{$endif}

	constructor ZStream.LzhamAdapter.Init(newMode: ModeEnum);
	var
		cs: lzham.compress_state absolute state;
		ds: lzham.decompress_state absolute state;
	begin
		try
			lzham.loader.Load;
		except
			instant_reraise_from_constructor;
		end;

		try
			state := nil;
			case newMode of
				Compress:
					begin
						cs := lzham.compress_init(@lzham.DefaultCompressionParams);
					{$ifdef Debug} if not Assigned(state) then Log('Ошибка Lzham.compress_init', logError); {$endif}
					end;
				Decompress:
					begin
						ds := lzham.BorrowDecompressionState;
					{$ifdef Debug} if not Assigned(state) then Log('Ошибка Lzham.decompress_init', logError); {$endif}
					end;
				else Assert(no);
			end;

			if not Assigned(state) then raise USystem.Error('Ошибка инициализации LZHAM.');
		except
      	lzham.loader.Unload;
			instant_reraise_from_constructor;
		end;
		inherited Init(newMode);
	end;

	destructor ZStream.LzhamAdapter.Done;
	var
		cs: lzham.compress_state absolute state;
		ds: lzham.decompress_state absolute state;
	begin
		if instantly_reraised_from_constructor then exit;
		case srcMode of
			Compress: lzham.compress_deinit(cs);
			Decompress: lzham.ReturnDecompressionState(ds);
		end;

		inherited Done;
		lzham.loader.Unload;
	end;

	function ZStream.LzhamAdapter.AdvanceImpl(mode: ModeEnum; input: pointer; nInput: size_t; output: pointer; nOutput: size_t;
		out consumed, written: size_t): AdvanceResult;
	var
		r: cuint;
		cs: lzham.compress_state absolute state;
		ds: lzham.decompress_state absolute state;
		isz, osz: csize_t;
	begin
		isz := nInput;
		osz := nOutput;
		zstep_dpr('lzham before adv: avail_in = ', isz, ', avail_out = ', osz);

		case mode of
			Compress:
				begin
					r := lzham.compress(cs, input, isz, output, osz, uint(nInput = 0));
					if (r = lzham.COMP_STATUS_NOT_FINISHED) or (r = lzham.COMP_STATUS_NEEDS_MORE_INPUT) or (r = lzham.COMP_STATUS_HAS_MORE_OUTPUT) then
						result := advance_Ok
					else
						if (r = lzham.COMP_STATUS_SUCCESS) and (nInput = isz) then result := advance_Finished else
						begin
						{$ifdef Debug} Error('compress', r); {$endif}
							result := advance_Error;
						end;
				end;
			Decompress:
				begin
					r := lzham.decompress(ds, input, isz, output, osz, uint(nInput = 0));
					if (r = lzham.DECOMP_STATUS_NOT_FINISHED) or (r = lzham.DECOMP_STATUS_NEEDS_MORE_INPUT) or (r = lzham.DECOMP_STATUS_HAS_MORE_OUTPUT) then
						result := advance_Ok
					else
						if r = lzham.DECOMP_STATUS_SUCCESS then result := advance_Finished else
						begin
						{$ifdef Debug} Error('decompress', r); {$endif}
							result := advance_Error;
						end;
				end;
		end;

		consumed := isz;
		written := osz;
		zstep_dpr('lzham adv: consumed = ', consumed, ', written = ', written, ', r = ', r);
	end;

	function ZStream.LzhamAdapter.Bound(const uncompressed: size_t): size_t;
	begin
		result := lzham.bound(uncompressed);
	end;

{$ifdef Debug}
	procedure ZStream.LzhamAdapter.Error(const where: string; code: sint);
	begin
		Log('Ошибка Lzham.' + where + '. ' + lzham.ErrorMessage(code, srcMode = Compress), logError);
	end;
{$endif}

	function ZStream.QuickLZAdapter.Bound(const uncompressed: size_t): size_t;
	begin
		result := QuickLZ.CompressBound(uncompressed);
	end;

	procedure ZStream.QuickLZAdapter.Compress(block: pointer; size: size_t; output: pointer; outputSize: size_t; out compressedSize: size_t);
	begin
		compressedSize := QuickLZ.Compress(block, size, output, outputSize);
		if compressedSize = 0 then raise Error('Ошибка QuickLZ.Compress(inSize = {0}, outSize = {1})', ToString(size), ToString(outputSize));
	end;

	procedure ZStream.QuickLZAdapter.Decompress(block: pointer; size: size_t; output: pointer; outputSize: size_t; out decompressedSize: size_t);
	begin
		decompressedSize := QuickLZ.Decompress(block, size, output, outputSize);
		if decompressedSize = 0 then raise Error('Ошибка QuickLZ.Decompress(inSize = {0}, outSize = {1})', ToString(size), ToString(outputSize));
	end;

	function BitStream.Open(newBase: pStream; mode: FileFlag): BitStream;
	var
		bs: BitStream absolute result;
		ok: boolean;
	begin
		Assert(mode in [file_Read, file_Write]);
		bs.base := MakeRef(newBase);
		ok := no;
		try
			if not Assigned(bs.base) then raise Error('Для битового потока не задан подлежащий поток.');
			if not (mode in bs.base^.flags) then raise Error('Режим битового потока не совместим с подлежащим потоком.');
			ok := yes;
		finally
			if not ok then Release(bs.base);
		end;

		bs.reading := mode = file_Read;
		bs.pending := 0;
		if bs.reading then bs.nPending := 0 else bs.nPending := bitsizeof(bs.pending);
	end;

	procedure BitStream.Close;
	begin
		if not reading then FlushWrite(yes);
		Release(base);
	end;

	function BitStream.Read(n: uint): uint;

		procedure ReadPending;
		var
			bytes: size_t;
		begin
			if n >= bitsizeof(pending) then bytes := sizeof(pending) else bytes := (n + (bitsizeof(byte) - 1)) div bitsizeof(byte);
			pending := 0;
			base^.Read(@pending, bytes);
			pending := BEtoN(pending) shr (bitsizeof(byte) * (sizeof(pending) - bytes));
			nPending := bytes * bitsizeof(byte);
		end;

	begin
		Assert(reading);
		result := 0;
		if nPending = 0 then ReadPending;

		while n > nPending do
		begin
			result := (result shl nPending) or pending;
			n -= nPending;
			ReadPending;
		end;

		//                  nPending - n
		//      xxxxxxx OOOOOOOOOOOOOOOOOOO
		//      |  n  |
		//      |  |  |
		//      V  V  V
		// OOOO xxxxxxx

		nPending -= n;
		result := (result shl n) or (pending shr nPending);
		pending := pending and (1 shl nPending - 1);
	end;

	procedure BitStream.Write(data, n: uint);
	begin
		// nPending — количество СВОБОДНЫХ бит.
		Assert(not reading);
		Assert((n >= bitsizeof(data)) or (data = (data and (uint(1) shl n - 1))));
		if nPending = 0 then FlushWrite(no);

		while n > nPending do
		begin
			//                       n - nPending
			//         xxxxxxxxxxxx OOOOOOOOOOOOOO
			//         |          |
			//         V nPending V
			// OOOOOOO xxxxxxxxxxxx
			n -= nPending;
			pending := (pending shl nPending) or (data shr n);
			nPending := 0;
			data := data and (1 shl n - 1);
			FlushWrite(no);
		end;

		pending := (pending shl n) or data;
		nPending -= n;
	end;

	procedure BitStream.FlushWrite(fully: boolean);
	var
		t: tmp_t;
		bits, bytes: uint;
	begin
		if nPending = 0 then
		begin
			t := NtoBE(pending);
			base^.Write(@t, sizeof(t));
		end else
		begin
			Assert(fully);
			bits := bitsizeof(pending) - nPending;
			bytes := (bits + (bitsizeof(byte) - 1)) div bitsizeof(byte);

			//          bits
			// xxxxxxxx xxxxxxxx xxxxx000
			//                         ^
			//                        000 = (bitsizeof(pending) - bits) mod 8
			t := NtoBE(pending shl (nPending mod bitsizeof(byte)));
			base^.Write(pointer(@t) + (sizeof(t) - bytes), bytes);
		end;

		pending := 0;
		nPending := bitsizeof(pending);
	end;

var
	ros: array of string;

	procedure MarkAsReadOnly(const folder: string);
	begin
		Assert(not MarkedAsReadOnly(foldeR));
		SetLength(ros, length(ros) + 1);
		ros[High(ros)] := folder;
	end;

	function MarkedAsReadOnly(const path: string): boolean;
	var
		i: sint;
	begin
		for i := 0 to High(ros) do
			if Prefixed(ros[i], path) then
				exit(yes);
		result := no;
	end;

type
	StreamRequisites = object
		ok: boolean;
		vfs: pPack;
		fullPath, fullRealPath: string;
		pathStart, realPathStart: sint;
		function Empty: StreamRequisites; static;
		function Get(const info: string; ro: boolean): StreamRequisites; static;
		procedure Done;
	end;

	function StreamRequisites.Empty: StreamRequisites;
	begin
		result.vfs := nil;
		result.fullPath := '';
		result.fullRealPath := '';
		result.pathStart := 1;
		result.realPathStart := 1;
		result.ok := yes;
	end;

	procedure StreamRequisites.Done;
	begin
		Release(vfs);
		Finalize(self);
		ok := no;
	end;

type
	GetAttributesParam = object
		what: FileAttributes;
		id: sint;
	const
		Empty: GetAttributesParam = (what: []; id: -1);
	end;

	procedure GetAttributes(var node: FilesystemCache.Node; id: sint; param: pointer);
	var
		p: ^GetAttributesParam absolute param;
	begin
		Assert(@id = @id);
		p^.what := node.what;
		p^.id := id;
	end;

	function MayBePk(p: pChar; len: sint): boolean;
	begin
		while len > 0 do
		begin
			dec(len);
			if p[len] = ExtensionSeparator then exit(no);
		end;
		result := yes;
	end;

	// TODO: переделать o_O
	function StreamRequisites.Get(const info: string; ro: boolean): StreamRequisites;
	const
		AlteredVfsTag = '+PK';
	var
		p, np, namelen, q, nq: sint;
		last: boolean;
		a: GetAttributesParam;
		altered, realName: string;
		reset: boolean;
		vfsnow: boolean;
	begin
		p := 1;
		q := FilesystemCache.IROOT;
		result := StreamRequisites.Empty;
		altered := '';
		reset := no;
		ro := ro and MarkedAsReadOnly(info);

		if not ro then
		begin
			result.fullPath := info;
			result.fullRealPath := info;
			exit;
		end;

		while p <= length(info) do
		begin
			np := Pos(FileSeparator, info, p);
			last := np = 0;
			if last then namelen := length(info) - p + 1 else namelen := np - p;
			a := GetAttributesParam.Empty;
			nq := FsCache^.Query(q, @info[p], namelen, @GetAttributes, @a);

			vfsnow := ((nq < 0) or FsCache^.TaggedWith(nq, AlteredVfsTag)) and MayBePk(@info[p], namelen);
			if vfsnow then
			begin
				altered := Copy(info, p, namelen) + ExtensionSeparator + Pack.Extension;
				if nq < 0 then
				begin
					nq := a.id; Assert(nq >= 0);
					if (FsCache^.Query(q, altered, @GetAttributes, @a) >= 0) and (file_JustFile in a.what) then
					begin
						FsCache^.ForceCorporeality(nq);
						FsCache^.TagWith(nq, AlteredVfsTag);
					end else
						nq := -1;
				end;
			end else
				vfsnow := file_JustFile in a.what;

			if altered <> '' then
			begin
				realName := altered;
				altered := '';
			end else
				realName := Copy(info, p, namelen);
			result.fullPath := StreamPath.ForcePath(result.fullPath);
			result.fullRealPath := StreamPath.ForcePath(result.fullRealPath);
			if reset then
			begin
				result.pathStart := length(result.fullPath) + 1;
				result.realPathStart := length(result.fullRealPath) + 1;
				reset := no;
			end;
			result.fullPath += Copy(info, p, namelen);
			result.fullRealPath += realName;

			if nq < 0 then
			begin
				result.Done;
				exit;
			end;

			q := nq;
			p += namelen + 1;

			if vfsnow and not last then
			begin
				Release(result.vfs);
				result.vfs := ResourcePool.Shared^.LoadRef(TypeOf(Pack), result.fullRealPath);
				reset := yes;
			end;
		end;
	end;

	function GetStreamAttributes(const info: string): FileAttributes;
{$ifdef DebugStreamPath}
	const
		What: array[boolean, boolean] of string = (('N/A', 'файл'), ('папка', 'папка И файл'));
{$endif}
	var
		req: StreamRequisites;
	begin
		req := StreamRequisites.Get(info, yes);
		try
			if req.ok then
				if Assigned(req.vfs) then
					result := req.vfs^.GetFileAttributes(pChar(req.fullRealPath) + req.realPathStart - 1, length(req.fullRealPath) - req.realPathStart + 1)
				else
					result := GetFileAttributes(Copy(req.fullRealPath, req.realPathStart, length(req.fullRealPath) - req.realPathStart + 1))
			else
				result := [];
		finally
			req.Done;
		end;
	{$ifdef DebugStreamPath}
		Log('GetStreamAttributes(' + info + ') = (' + StreamTypeIds[st] + ', ' + What[file_Folder in result, file_JustFile in result] + ')', logDebug);
	{$endif}
	end;

	function GetStream(const info: string; flags: FileFlags): pStream;
	var
		req: StreamRequisites;
		fn: string;
	begin
		req := StreamRequisites.Get(info, not (file_Write in flags));
		fn := '';
		try
			if req.ok then
				if Assigned(req.vfs) then
					result := req.vfs^.GetFile(pChar(req.fullRealPath) + req.realPathStart - 1, length(req.fullRealPath) - req.realPathStart + 1, flags)
				else
				begin
					fn := Copy(req.fullRealPath, req.realPathStart, length(req.fullRealPath) - req.realPathStart + 1);
					result := new(pFileStream, Init(fn, flags));
				end
			else
				result := new(pFileStream, Init(info, flags));
		finally
			fn := '';
			req.Done;
		end;
		if Assigned(result) then result^.path := info;
	end;

	function GetStream(const info: string): pStream; begin result := GetStream(info, DefaultFileFlags); end;
	function GetStreamRef(const info: string): pStream; begin result := GetStream(info)^.NewRef; end;
	function GetStreamRef(const info: string; flags: FileFlags): pStream; begin result := MakeRef(GetStream(info, flags)); end;

	function TryGetStream(const info: string): pStream;
	begin
		result := GetStream(info, DefaultFileFlags + [file_JustTry]);
	end;

	function PrefetchPath(const info: string): string;
	begin
		StreamRequisites.Get(info, yes).Done;
		result := info;
	end;

	function ReadWholeAsStringOptsDesc.Binary(isbin: boolean = yes): ReadWholeAsStringOptsDesc;
	begin result := self;
		result.bin := isbin;
	end;

	function ReadWholeAsStringOptsDesc.StoreSignature(&to: pString): ReadWholeAsStringOptsDesc;
	begin result := self;
		result.storeSig := &to;
	end;

	function SkipUTF8BOM(s: pStream; storeSig: pString = nil): boolean;
	var
		sig: packed array[1 .. length(UTF8.BOM)] of ansichar;
		spos: FilePos;
	begin
		if Assigned(storeSig) then storeSig^ := '';
		spos := s^.Position;
		result := (s^.TryRead(@sig[1], sizeof(sig)) = sizeof(sig)) and (sig = UTF8.BOM);
		if result then
		begin
			if Assigned(storeSig) then storeSig^ := USystem.ToString(@sig[Low(sig)], length(sig));
		end else
		begin
			s^.Position := spos;
		{$ifdef Debug} LogR(StreamPath.Log(s^.path) + ' - UTF-8 без BOM; ', logWarning); {$endif}
		end;
	end;

	function ReadWholeAsString(f: pStream): string; begin result := ReadWholeAsString(f, ReadWholeAsStringOpts); end;
	function ReadWholeAsString(f: pStream; const opts: ReadWholeAsStringOptsDesc): string;
	var
		restSize: size_t;
	begin
		if Assigned(opts.storeSig) then opts.storeSig^ := '';
		try
			MakeRef(f);
			if not opts.bin then SkipUTF8BOM(f, opts.storeSig);
			restSize := (f^.size - f^.position).AsSizeT;
			SetLength(result, (restSize + (sizeof(char) - 1)) div sizeof(char));
			f^.Read(pointer(result), restSize);
		finally
			Release(f);
		end;
	end;

{$define impl :=
	procedure serialize(s: pStream; const f: _PARAM_);
	var
		t: _T_;
	begin
		t := f;
		s^.Write(@t, sizeof(t));
	end;

	function deserialize(s: pStream): _PARAM_;
	var
		t: _T_;
	begin
		s^.Read(@t, sizeof(t));
		result := {$ifdef _T2PARAM_} _T2PARAM_ {$endif} (t);
	end; {$undef serialize} {$undef deserialize} {$undef _T_} {$undef _PARAM_} {$undef _T2PARAM}}
	{$define serialize := Serialize_f64} {$define deserialize := Deserialize_f64} {$define _T_ := float64} {$define _PARAM_ := hp_float} impl
	{$define serialize := Serialize_f32} {$define deserialize := Deserialize_f32} {$define _T_ := float32} {$define _PARAM_ := float} impl
	{$define serialize := Serialize_f16} {$define deserialize := Deserialize_f16} {$define _T_ := float16} {$define _PARAM_ := float} {$define _T2PARAM_:=float32} impl
{$undef impl}

{$define simpl:=
	var
		i: uint;
	begin
		for i := 0 to High(v.data) do
			se(s, v.data[i]);
	end;}

{$define dimpl:=
	var
		i: uint;
	begin
		for i := 0 to High(result.data) do
			result.data[i] := de(s);
	end;}
{$define se:=Serialize_f32} {$define de:=Deserialize_f32}
	procedure Serialize_vec2f32(s: pStream; const v: Vec2); simpl
	function Deserialize_vec2f32(s: pStream): Vec2; dimpl
	procedure Serialize_vec3f32(s: pStream; const v: Vec3); simpl
	function Deserialize_vec3f32(s: pStream): Vec3; dimpl
	procedure Serialize_vec4f32(s: pStream; const v: Vec4); simpl
	function Deserialize_vec4f32(s: pStream): Vec4; dimpl
{$define se:=Serialize_f16} {$define de:=Deserialize_f16}
	procedure Serialize_vec2f16(s: pStream; const v: Vec2); simpl
	function Deserialize_vec2f16(s: pStream): Vec2; dimpl
	procedure Serialize_vec3f16(s: pStream; const v: Vec3); simpl
	function Deserialize_vec3f16(s: pStream): Vec3; dimpl
	procedure Serialize_vec4f16(s: pStream; const v: Vec4); simpl
	function Deserialize_vec4f16(s: pStream): Vec4; dimpl
{$undef se} {$undef de} {$undef simpl} {$undef dimpl}

	procedure Serialize_fN8(s: pStream; const f, a, b: float);
	begin
	{$ifdef Debug} Assert((f >= a) and (f <= b)); {$endif}
		Serialize_ui8(s, round(remap(f, a, b, 0.0, High(uint8))));
	end;

	function Deserialize_fN8(s: pStream; const a, b: float): float;
	begin
		result := a + (b - a) * (Deserialize_ui8(s) * (1.0 / High(uint8)));
	end;

	procedure Serialize_fN16(s: pStream; const f, a, b: float);
	begin
	{$ifdef Debug} Assert((f >= a) and (f <= b)); {$endif}
		Serialize_ui16(s, round(remap(f, a, b, 0.0, High(uint16))));
	end;

	function Deserialize_fN16(s: pStream; const a, b: float): float;
	begin
		result := a + (b - a) * (Deserialize_ui16(s) * (1.0 / High(uint16)));
	end;

{$define se8impl:=
	var i: uint;
	begin
		for i := 0 to High(v.data) do
			Serialize_fN8(s, v.data[i], a.data[i], b.data[i]);
	end;}
{$define de8impl:=
	var i: uint;
	begin
		for i := 0 to High(result.data) do
			result.data[i] := Deserialize_fN8(s, a.data[i], b.data[i]);
	end;}
{$define se16impl:=
	var i: uint;
	begin
		for i := 0 to High(v.data) do
			Serialize_fN16(s, v.data[i], a.data[i], b.data[i]);
	end;}
{$define de16impl:=
	var i: uint;
	begin
		for i := 0 to High(result.data) do
			result.data[i] := Deserialize_fN16(s, a.data[i], b.data[i]);
	end;}
	procedure Serialize_vec2N8(s: pStream; const v, a, b: Vec2); se8impl
	function Deserialize_vec2N8(s: pStream; const a, b: Vec2): Vec2; de8impl
	procedure Serialize_vec2N16(s: pStream; const v, a, b: Vec2); se16impl
	function Deserialize_vec2N16(s: pStream; const a, b: Vec2): Vec2; de16impl

	procedure Serialize_vec3N8(s: pStream; const v, a, b: Vec3); se8impl
	function Deserialize_vec3N8(s: pStream; const a, b: Vec3): Vec3; de8impl
	procedure Serialize_vec3N16(s: pStream; const v, a, b: Vec3); se16impl
	function Deserialize_vec3N16(s: pStream; const a, b: Vec3): Vec3; de16impl

	procedure Serialize_vec4N8(s: pStream; const v, a, b: Vec4); se8impl
	function Deserialize_vec4N8(s: pStream; const a, b: Vec4): Vec4; de8impl
	procedure Serialize_vec4N16(s: pStream; const v, a, b: Vec4); se16impl
	function Deserialize_vec4N16(s: pStream; const a, b: Vec4): Vec4; de16impl
{$undef se8impl} {$undef de8impl} {$undef se16impl} {$undef de16impl}

// TODO: сохранять как угол + ось?

{$define se:=
	var
		q: Quaternion;
	begin
		q := uq.Normalized;
		if q.w < 0.0 then q := -q;
		snv(s, pVec3(@q)^, Vec3.MinusOnes, Vec3.Ones)
	end; {$undef snv}}

{$define de:=
	var
		v: Vec3;
		sqw: float;
	begin
		v := dnv(s, Vec3.MinusOnes, Vec3.Ones);
		sqw := 1.0 - v.SqrLength;
		if sqw >= 0.0 then
			result := Quaternion.Make(v, sqrt(sqw))
		else
			result := Quaternion.Make(v.Normalized, 0.0);
	end; {$undef dnv}}

	procedure Serialize_IQuat8(s: pStream; const uq: Quaternion); {$define snv:=Serialize_vec3N8} se
	function Deserialize_IQuat8(s: pStream): Quaternion; {$define dnv:=Deserialize_vec3N8} de
	procedure Serialize_IQuat16(s: pStream; const uq: Quaternion); {$define snv:=Serialize_vec3N16} se
	function Deserialize_IQuat16(s: pStream): Quaternion; {$define dnv:=Deserialize_vec3N16} de
{$undef se} {$undef de}

const
	TRANSFORM_HAS_TF_BIT    = 1 shl 0;
	TRANSFORM_HAS_ROT_BIT   = 1 shl 1;
	TRANSFORM_HAS_SCALE_BIT = 1 shl 2;

{$define simpl:=
	var
		flags: uint;
		hasTf, hasRot, hasScale: boolean;
	begin
		flags := 0;
		hasTf  := not t.tr.IsZero; if hasTf then flags := flags or TRANSFORM_HAS_TF_BIT;
		hasRot := not SameRotation(t.rot, Quaternion.Identity); if hasRot then flags := flags or TRANSFORM_HAS_ROT_BIT;
		hasScale := not Equals(t.scale, 1.0); if hasScale then flags := flags or TRANSFORM_HAS_SCALE_BIT;
		Serialize_ui8(s, flags);
		if hasTf then Serialize_vec3f32(s, t.tr);
		if hasRot then _seiq_(s, t.rot);
		if hasScale then Serialize_f32(s, t.scale);
	end; {$undef _seiq_}}

{$define dimpl:=
	var
		flags: uint;
	begin
		flags := Deserialize_ui8(s);
		if (flags and TRANSFORM_HAS_TF_BIT) <> 0 then
			result.tr := Deserialize_vec3f32(s)
		else
			result.tr := Vec3.Zero;

		if (flags and TRANSFORM_HAS_ROT_BIT) <> 0 then
			result.rot := _deiq_(s)
		else
			result.rot := Quaternion.Identity;

		if (flags and TRANSFORM_HAS_SCALE_BIT) <> 0 then
			result.scale := Deserialize_f32(s)
		else
			result.scale := 1.0;
	end; {$undef _deiq_}}

	procedure Serialize_tf32r8(s: pStream; const t: Transform); {$define _seiq_:=Serialize_IQuat8} simpl
	function Deserialize_tf32r8(s: pStream): Transform; {$define _deiq_:=Deserialize_IQuat8} dimpl
	procedure Serialize_tf32r16(s: pStream; const t: Transform); {$define _seiq_:=Serialize_IQuat16} simpl
	function Deserialize_tf32r16(s: pStream): Transform; {$define _deiq_:=Deserialize_IQuat16} dimpl
{$undef simpl} {$undef dimpl}

	procedure Serialize_planeN8d32(s: pStream; const plane: Plane);
	begin
		Serialize_vec3N8(s, plane.v3, Vec3.MinusOnes, Vec3.Ones);
		Serialize_f32(s, plane.d);
	end;

	function Deserialize_planeN8d32(s: pStream): Plane;
	begin
		result.data.v3 := Deserialize_vec3N8(s, Vec3.MinusOnes, Vec3.Ones);
		result.d := Deserialize_f32(s);
	end;

	procedure Serialize_string(s: pStream; const st: string);
	begin
		VarInt.Write(s, length(st));
		s^.Write(pointer(st), length(st) * sizeof(st[1]));
	end;

	function Serialize_string_bytes(len: size_t): size_t;
	begin
		result := VarInt.Bytes(len) + len * sizeof(char);
	end;

	function Serialize_string_bytes(const s: string): size_t;
	begin
		result := Serialize_string_bytes(length(s));
	end;

	function Deserialize_string(s: pStream): string;
	var
		len: uint;
	begin
		len := VarInt.Read(s);
		SetLength(result, len);
		s^.Read(pointer(result), length(result) * sizeof(result[1]));
	end;

	function Serialize_string_xbits_bytes(len: size_t; nExtra, extra: uint): size_t;
	begin
		result := VarInt.Bytes(RangeCheck(len, High(uint) shr nExtra, 'Serialize_string_xbits') shl nExtra or extra)
		        + len * sizeof(char);
	end;

	function Serialize_string_xbits_bytes(const s: string; nExtra, extra: uint): size_t;
	begin
		result := Serialize_string_xbits_bytes(length(s), nExtra, extra);
	end;

	procedure Serialize_string_xbits(s: pStream; const st: string; nExtra, extra: uint);
	begin
		Assert(extra = extra and (1 shl nExtra - 1), Format('more than {0} bits in {1}', ToString(nExtra), ToString(extra)));
		VarInt.Write(s, RangeCheck(length(st), High(uint) shr nExtra, 'Serialize_string_xbits') shl nExtra or extra);
		s^.Write(pointer(st), length(st) * sizeof(st[1]));
	end;

	function Deserialize_string_xbits(s: pStream; nExtra: uint; out extra: uint): string;
	var
		lenx: uint;
	begin
		lenx := VarInt.Read(s);
		extra := lenx and (1 shl nExtra - 1);
		SetLength(result, lenx shr nExtra);
		s^.Read(pointer(result), length(result) * sizeof(result[1]));
	end;

	procedure Serialize_conststring(s: pStream; const st: string);
	begin
		s^.Write(st);
	end;

	function Deserialize_conststring(s: pStream; len: uint): string;
	begin
		SetLength(result, len);
		s^.Read(pointer(result), len * sizeof(result[1]));
	end;

	procedure Serialize_signature(s: pStream; const sig: string);
	begin
		s^.Write(sig);
	end;

	function Deserialize_signature(s: pStream; const sig: string; mayFail: boolean): boolean;
	var
		spos: FilePos;
		st: string;
	begin
		spos := s^.Position;
		try
			st := Deserialize_conststring(s, length(sig));
			result := st = sig;
			if not result then
				raise Error('Неверная сигнатура' {$ifdef Debug} + ': {0}, ожидается {1}' {$endif} + '.' {$ifdef Debug}, PrintableString(st), PrintableString(sig) {$endif});
		except
			result := no;
			s^.Position := spos;
			if not mayFail then raise;
		end;
	end;

	function VarInt.Bound(bytes: size_t): size_t;
	begin
		result := bytes + (bytes + 1) div 2;
	end;

{$define intf :=
	function VarInt.Bytes(const x: typ): size_t;
	begin
		result := 1 + Log2(x + ord(x = 0)) div N_BITS;
	end;

	procedure VarInt.Write(s: pStream; x: typ);
	var
		fb: uint;
		b: packed array[0 .. (bitsizeof(x) + N_BITS - 1) div N_BITS - 1] of Base;
	begin
		fb := High(b) + 1;
		repeat
			Assert(fb > 0);
			dec(fb);
			b[fb] := x and MASK;
			if fb < High(b) then b[fb] := b[fb] or HAS_NEXT_BIT;
			x := x shr N_BITS;
		until x = 0;
		s^.Write(@b[fb], (High(b) - fb + 1) * sizeof(Base));
	end;} all_uints

{$define intf :=
	function VarInt.Bytes(const x: typ): size_t;
	begin
		Assert(x >= 0, 'VarInt не может быть отрицательным.');
		result := Bytes(uint_pair(x));
	end;

	procedure VarInt.Write(s: pStream; const x: typ);
	begin
		Assert(x >= 0, 'VarInt не может быть отрицательным.');
		Write(s, uint_pair(x));
	end;} all_sints

{$define impl :=
	const
		ErrorBase = 'Ошибка чтения VarInt';
	var
		b: packed array[0 .. (bitsizeof(result) + N_BITS - 1) div N_BITS - 1] of Base;
		start: FilePos;
		i, rd, cb: uint;
	begin
		result := 0;
		if s^.CanSeek then
		begin
			start := s^.Position;
			rd := s^.TryRead(@b, sizeof(b)) div sizeof(Base);
			if rd < sizeof(Base) then raise Error('{0}.', ErrorBase);

			result := 0;
			for i := 0 to uint(rd div sizeof(Base) - 1) do
			begin
				cb := b[i];
				result := (result shl N_BITS) or (cb and MASK);
				if cb and HAS_NEXT_BIT = 0 then
				begin
					if i + 1 < rd div sizeof(Base) then s^.Position := start + (i + 1) * sizeof(Base);
					exit;
				end;
			end;
		end else
			for i := High(b) downto 0 do
			begin
				s^.Read(@b[0], sizeof(b[0]));
				cb := b[0];
				result := (result shl N_BITS) or (cb and MASK);
				if cb and HAS_NEXT_BIT = 0 then exit;
			end;

		raise Error('{0}: не укладывается в {1}x{2}.', ErrorBase, ToString(length(b)), ToString(sizeof(Base)));
	end;}
	function VarInt.Read(s: pStream): uint; impl
	function VarInt.ReadLong(s: pStream): ulong; impl
{$undef impl}

	function VarInt.StoreOne(x: uint; p: pointer; n: size_t): size_t;
	var
		b: pBase absolute p;
		cells: uint;
		t: uint;
	begin
		cells := 0;
		t     := x;
		repeat
			t := t shr N_BITS;
			inc(cells);
		until t = 0;

		result := cells * sizeof(Base);
	{$ifdef Debug}
		if result > n then
			raise Error('VarInt({0}) занимает {1}b и не помещается в буфер на {2}b.', [x, cells * sizeof(Base), n]);
	{$else}
		Assert(@n = @n);
	{$endif}

		dec(cells);
		b[cells] := x and MASK;
		while cells > 0 do
		begin
			dec(cells); x := x shr N_BITS;
			b[cells] := x and MASK or HAS_NEXT_BIT;
		end;
	end;

	procedure VarInt.Store(x: uint; var p: pointer; var n: size_t);
	var
		sz: size_t;
	begin
		sz := StoreOne(x, p, n);
		p += sz; n -= sz;
	end;

	function VarInt.LoadOne(p: pointer; n: size_t; out len: size_t): uint;
	const
		ErrorBase = 'Не удалось прочитать VarInt: ';
	var
		b: pBase absolute p;
	begin
		result := 0;
		len    := sizeof(Base);
		while (len <= n) and (result <= High(result) shr N_BITS) do
		begin
			result := result shl N_BITS or (b[len div sizeof(Base) - 1] and uint(MASK));
			if b[len div sizeof(Base) - 1] and HAS_NEXT_BIT = 0 then exit;
			inc(len, sizeof(Base));
		end;

		if len > n then
			raise Error('{0}недостаточный размер входных данных ({1}b).', [ErrorBase, n])
		else
			raise Error('{0}переполнение uint{1}.', [ErrorBase, bitsizeof(result)]);
	end;

	function VarInt.Load(var p: pointer; var n: size_t): uint;
	var
		sz: size_t;
	begin
		result := LoadOne(p, n, sz);
		p += sz; n -= sz;
	end;

{$define impl :=
	procedure serialize(s: pStream; const x: {$ifdef precise_type} _T_ {$else} uint {$endif});
	var
		t: _T_;
	begin
		t := NtoLE(x);
		s^.Write(@t, {$ifdef rep_size} rep_size {$else} sizeof(t) {$endif});
	end;

	function deserialize(s: pStream): {$ifdef precise_type} _T_ {$else} uint {$endif};
	var
		t: _T_;
	begin
	{$ifdef rep_size} t := 0; {$endif}
		s^.Read(@t, {$ifdef rep_size} rep_size {$else} sizeof(t) {$endif});
		result := LEtoN(t);
	end; {$undef _T_} {$undef serialize} {$undef deserialize} {$undef precise_type} {$undef rep_size}}

{$define _T_ := uint8} {$define serialize := Serialize_ui8} {$define deserialize := Deserialize_ui8} impl
{$define _T_ := uint16} {$define serialize := Serialize_ui16} {$define deserialize := Deserialize_ui16} impl
{$define _T_ := uint32} {$define rep_size := 3 * sizeof(uint8)} {$define serialize := Serialize_ui24} {$define deserialize := Deserialize_ui24} impl
{$define _T_ := uint32} {$define serialize := Serialize_ui32} {$define deserialize := Deserialize_ui32} impl
{$define _T_ := uint64} {$define serialize := Serialize_ui64} {$define deserialize := Deserialize_ui64} {$define precise_type} impl
{$undef impl}

{$define intf :=
	function Serialize_ui_bytes(const x: typ; fmt: UiBinaryFormat): size_t;
	begin
		case fmt of
			se_ui8:  result := sizeof(uint8);
			se_ui16: result := sizeof(uint16);
			se_ui24: result := 3 * sizeof(uint8);
			se_ui32: result := sizeof(uint32);
			se_ui64: result := sizeof(uint64);
			se_ui_v8: result := VarInt.Bytes(x);
			else raise ExhaustiveCase(ord(fmt), 'Serialize_ui_bytes.format');
		end;
	end;

	procedure Serialize_ui(s: pStream; const x: typ; fmt: UiBinaryFormat);
	begin
		case fmt of
			se_ui8: Serialize_ui8(s, x);
			se_ui16: Serialize_ui16(s, x);
			se_ui24: Serialize_ui24(s, x);
			se_ui32: Serialize_ui32(s, x);
			se_ui64: Serialize_ui64(s, x);
			se_ui_v8: VarInt.Write(s, x);
			else raise ExhaustiveCase(ord(fmt), 'Serialize_ui.format');
		end;
	end;} all_uints

{$define intf :=
	function Serialize_ui_bytes(const x: typ; fmt: UiBinaryFormat): size_t;
	begin
		result := Serialize_ui_bytes(uint_pair(x), fmt);
	end;

	procedure Serialize_ui(s: pStream; const x: typ; fmt: UiBinaryFormat);
	begin
		Serialize_ui(s, uint_pair(x), fmt);
	end;} all_sints

{$define impl :=
	function func(s: pStream; fmt: UiBinaryFormat): typ;
	begin
		case fmt of
			se_ui8: result := Deserialize_ui8(s);
			se_ui16: result := Deserialize_ui16(s);
			se_ui24: result := Deserialize_ui24(s);
			se_ui32: result := Deserialize_ui32(s);
			se_ui64: result := Deserialize_ui64(s);
			se_ui_v8: result := vi_read(s);
			else raise ExhaustiveCase(ord(fmt), 'Deserialize_ui.format');
		end;
	end; {$undef func} {$undef typ} {$undef vi_read}}
	{$define func := Deserialize_ui} {$define typ := uint} {$define vi_read := VarInt.Read} impl
	{$define func := Deserialize_ui_long} {$define typ := ulong} {$define vi_read := VarInt.ReadLong} impl
{$undef impl}

	function StoreLoadBufferOverflow(const what: string; req, got: size_t): Exception;
	begin
		result := Error('{0} ({1}) не умещается в предоставленный буфер на {2}.', what, ToStringSuff_b(req), ToStringSuff_b(got));
	end;

	procedure Store_ui32(x: uint32; var p: pointer; var n: size_t);
	begin
	{$ifdef Debug} if n < sizeof(uint32) then raise StoreLoadBufferOverflow('Uint32', sizeof(uint32), n); {$endif}
		pUint32(unaligned(p))^ := NtoBE(x);
		p += sizeof(uint32); n -= sizeof(uint32);
	end;

	function Load_ui32(var p: pointer; var n: size_t): uint32;
	begin
		if n < sizeof(uint32) then raise StoreLoadBufferOverflow('Uint32', sizeof(uint32), n);
		result := BEtoN(pUint32(unaligned(p))^);
		p += sizeof(uint32); n -= sizeof(uint32);
	end;

	function Serialize_enum_bytes(x: uint; const prefixCodes: array of string): size_t;
	begin
		result := length(prefixCodes[x]) * sizeof(char);
	end;

	procedure Serialize_enum(s: pStream; x: uint; const prefixCodes: array of string);
	begin
		if x >= uint(length(prefixCodes)) then raise Error('Энум вне диапазона ({0}).', SeparatedList.Join(prefixCodes, ', '));
		Serialize_conststring(s, prefixCodes[x]);
	end;

	function Deserialize_enum(s: pStream; const prefixCodes: array of string): uint;
	var
		vs: array[0 .. 31] of sint;
		sym: char;
		nVs, nSym, i: sint;
	begin
		nSym := 0;
		repeat
			sym := chr(Deserialize_ui8(s));
			if nSym = 0 then
			begin
				nVs := 0;
				for i := 0 to High(prefixCodes) do
					if prefixCodes[i][1] = sym then
					begin
						if length(prefixCodes[i]) = 1 then exit(i);
						if nVs = length(vs) then raise Error('Буфер энумов переполнен.');
						vs[nVs] := i;
						inc(nVs);
					end;
			end else
				for i := nVs - 1 downto 0 do
					if prefixCodes[vs[i]][1 + nSym] <> sym then
					begin
						vs[i] := vs[nVs - 1];
						dec(nVs);
					end else
						if length(prefixCodes[vs[i]]) = 1 + nSym then
							exit(vs[i]);
			inc(nSym);
		until nVs = 0;

		raise Error('Энум не соответствует ни одному из известных ({0}).', SeparatedList.Join(prefixCodes, ', '));
	end;

	procedure Serialize_colorRGB8(s: pStream; const color: Color);
	begin
		if (color.r >= 0.0) and (color.r <= 1.0) and (color.g >= 0.0) and (color.g <= 1.0) and (color.b >= 0.0) and (color.b <= 1.0) then
		begin
			Serialize_ui8(s, 1);
			Serialize_ui8(s, round(color.r * High(uint8)));
			Serialize_ui8(s, round(color.g * High(uint8)));
			Serialize_ui8(s, round(color.b * High(uint8)));
		end else
		begin
			Serialize_ui8(s, 0);
			Serialize_f32(s, color.r);
			Serialize_f32(s, color.g);
			Serialize_f32(s, color.b);
		end;
	end;

	function Deserialize_colorRGB8(s: pStream): Color;
	var
		r, g, b: float;
	begin
		case Deserialize_ui8(s) of
			1:
				begin
					r := Deserialize_ui8(s) / High(uint8);
					g := Deserialize_ui8(s) / High(uint8);
					b := Deserialize_ui8(s) / High(uint8);
				end;
			else
				begin
					r := Deserialize_f32(s);
					g := Deserialize_f32(s);
					b := Deserialize_f32(s);
				end;
		end;
		result := Color.RGB(r, g, b);
	end;

	// 0–5 — секунда, 6–11 — минута, 12–17 — час, 18–22 — день, 23–26 — месяц, 27–31 — 5 младших бит года, затем 8 старших
	procedure Serialize_datetime(s: pStream; const dt: DateTime);
	begin
		Serialize_ui32(s, bitpack([dt.sec, 6, dt.min, 6, dt.hour, 6, dt.day, 5, dt.month, 4, dt.year and %11111, 5]));
		Serialize_ui8(s, dt.year shr 5);
	end;

	function Deserialize_datetime(s: pStream): DateTime;
	var
		x: uint32;
	begin
		x := Deserialize_ui32(s);
		result := DateTime.YMDHMSMS(
			{year} bits(x, 27, 5) or Deserialize_ui8(s) shl 5, {month} bits(x, 23, 4), {day} bits(x, 18, 5),
			{hour} bits(x, 12, 6), {min} bits(x, 6, 6), {sec} bits(x, 0, 6), 0);
		if not result.Validate then raise Error('Прочитано недопустимое значение даты и времени.');
	end;

{$if defined(selftest)}
	function ResolveTestCase(const input: string): string;
	begin
		result := StreamPath.Resolve(input);
	end;

	procedure Test;
	begin
		TestSuite.Start
		.Feature('Path.Resolve', @ResolveTestCase)
		.&Case('../a/../../../b/c/', '../../../b/c/').&Case('a/b/../c/d/../../../', '').&Case('a/b/c/../..', 'a').&Case('a/b/c/../../', 'a/')
		.&Case('/a/..', '/').&Case('/../../b/..', '/../..').&Case('/../../b/../', '/../../')
		.Done;
	end;
{$endif}

initialization
{$if defined(selftest)} &Unit('Streams').Test(@Test); {$endif}
end.
