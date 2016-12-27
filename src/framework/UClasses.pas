unit UClasses;

{$include opts.inc}
{$ifdef Debug}
	{-$define DebugStrPool}
	{-$define DebugDelayedRelease}
	{-$define DebugModifiableValues}
	{-$define DebugMemoryPool}
	{-$define DebugFsCache}
{$endif}
{-$define sb_fine}

interface

uses
	USystem, Errors, Streams, UMath, Utils, Algo;

type
	ValueDynamics = object
	private
		controlPeriod, _curStartTime: Ticks;
		_curStartFrame: sint;
		_curSum: size_t;
		_avgPerSec, _avgPerFrame: float;
		_periodActive: boolean;
	public
		procedure Init;
		procedure Done;
		procedure NewValue(value: size_t; frameNo: sint);

		property AveragePerSec: float read _avgPerSec;
		property AveragePerFrame: float read _avgPerFrame;
	end;

type
	pPoolString = ^PoolString;
	PoolString = object
		internal: string;
		function Hash: Hash.Value; cinline
		function ToIndex: pointer; cinline
		function Empty: boolean; cinline
	end;
	operator =(const a, b: PoolString): boolean; cinline
	operator =(const a: string; b: PoolString): boolean; cinline
	operator =(const a: PoolString; b: string): boolean; cinline
	operator :=(const s: PoolString): string; cinline
	operator :=(const s: string): PoolString; cinline
	operator +(const s: string; a: PoolString): string; cinline

	function InternedPoolSize: size_t;
	procedure CleanupInternedPool(n: uint = High(uint));
	function DumpInternedStrings(out count: uint): string;

type
	pMemoryPool = ^MemoryPool;
	MemoryPool = object
		procedure Init(const name: string; blockSize: size_t);
		procedure Done;
		function GetBlock: pointer;
		procedure ReturnBlock(p: pointer);
		function GetMem(size: size_t): pointer;
		procedure FreeMem(p: pointer; size: size_t);
		function ReallocMem(p: pointer; osize, nsize: size_t): pointer;

	private const
		CleanupPeriod = 5000;
	type
		CleanupReport = record
			opsForPeriod: sint;
		end;
	var
		lock: ThreadLock;
		blockSize: size_t;
		blocks: array of pointer;
		nBlocks, minFreeBlocksForPeriod, opsForPeriod: sint;
		timer: ThreadTimer;
	{$ifdef Debug}
		nAllocated: sint;
		name: string;
		function MessagePrefix: string;
	{$endif}
		function Cleanup: CleanupReport;
	end;

	pStringBuilder = ^StringBuilder;
	StringBuilder = object
		procedure Init;
		procedure Done;
		procedure Append(ch: pChar; count: size_t);
	{$define func:=
		procedure Append(const _ARGS_: char);
		procedure Append(const _ARGS_: string);} {$include variadic.inc}
		function ToString: string;
		function DestructiveToString: string;
		function Len: size_t;
		procedure InstanceMovedFrom(var old: StringBuilder);
		procedure Clear;

	private type
		pChain = ^Chain;
		Chain = record
			count: size_t;
			next: pChain;
			data: array[0 .. 8 * sizeof(pointer) - 1] of char;
		end;
	var
		chWoLast: size_t;
		cp, cend: pChar;
		chLast: pChain;
		chStatic: Chain;
	{$ifndef sb_fine} function Grow(by: size_t): pChar; {$endif}
		function AllocateBlock(apsize: size_t): pChar;
		function GetSym(id: size_t): char;
	{$ifdef sb_fine} procedure FineAppend(ch: pChar; count: size_t); {$endif}
	public
		property Syms[id: size_t]: char read GetSym; default;
	end;

	pBitfield = ^Bitfield;
	Bitfield = object
	private const
		Base = bitsizeof(byte);
	private var
		_size: uint;
		_dataSize: size_t;
		_data: pointer;
		staticData: {$ifdef Debug} uint16 {$else} uint64 {$endif};
		function GetBit(id: uint): boolean;
		procedure SetBit(id: uint; newValue: boolean);
		procedure SetSize(newSize: uint);
	public
		procedure Init(newSize: uint);
		procedure Done;
		function Dump(const str0: string = '0'; const str1: string = '1'): string;
		function ValidateIndex(idx: uint): boolean;
		function Ones: boolean;
		procedure FillWithOnes;

		property Bits[id: uint]: boolean read GetBit write SetBit; default;
		property Size: uint read _size write SetSize;
		property Data: pointer read _data;
		property DataSize: size_t read _dataSize;
	end;

	pBitfield2D = ^Bitfield2D;
	Bitfield2D = object
	private type
		BoolArray = array of boolean;
	private var
		_size: UintVec2;
		procedure AllocateEmpty(const newSize: UintVec2; fromScratch: boolean);
		function ValidateIndex(x, y: uint): boolean;
		function BitAt(x, y: uint): boolean;
		procedure SetBitAt(x, y: uint; newValue: boolean);
		function Extract(const pos, size: UintVec2): BoolArray;
		procedure Trim;
	public type
		Rects = array of UintRect;
	var
		raw: Bitfield;
		procedure Init(const newSize: UintVec2);
		procedure Init(const data: string);
		procedure Done;
		function Dump(const str0: string = '0'; const str1: string = '1'): string;
		procedure Serialize(s: pStream; packsxy, justsizes: boolean);
		procedure Deserialize(s: pStream; packsxy, justsizes: boolean);

		function Bit(const pos: UintVec2): boolean;
		procedure SetBit(const pos: UintVec2; newValue: boolean = yes);
		function ValidatePoint(const pos: UintVec2): boolean;

		procedure Resize(const newSize: UintVec2);
		function FitsWithOnes(const b: Bitfield2D; const ofs: UintVec2): boolean;
		procedure InplaceOr(const b: Bitfield2D; const ofs: UintVec2);
		procedure InplaceXor(const b: Bitfield2D; const ofs: UintVec2);
		procedure InplaceAndNot(const b: Bitfield2D; const ofs: UintVec2);
		function ToRects: Rects;

		property Size: UintVec2 read _size;
		property Bits[x, y: uint]: boolean read BitAt write SetBitAt; default;
	private type
		CombineRowCallback = function(var a: Bitfield; const b: Bitfield; astart, bstart, n: uint): boolean;
		function Combine(const b: Bitfield2D; const ofs: UintVec2; cb: CombineRowCallback): boolean;
	end;

	pBlob = ^Blob;
	Blob = object
	public type
		DestructorProc = procedure(var obj);
		GetSizeProc = function(var obj): size_t;
		Predicate = function(var obj; param: pointer): boolean;
	private var
		_data: pointer;
		_size: size_t;
		_count: uint;
	public
		procedure Init;
		procedure Done;
		procedure Done(destruct: DestructorProc; getSize: GetSizeProc);
		function Add(newSize: size_t): pointer;
		function Next(var ptr: pointer; getSize: GetSizeProc): boolean;
		function Find(getSize: GetSizeProc; pred: Predicate; param: pointer): pointer;

		property Raw: pointer read _data;
		property DataSize: size_t read _size;
		property Count: uint read _count;
	end;

{$define classname := HeterogenousQueue} {$include heterogenous_queue.h.inc}
{$define classname := ThreadedHeterogenousQueue} {$define threading} {$include heterogenous_queue.h.inc}

	pMultiDelegate = ^MultiDelegate;
	MultiDelegate = object
	public type
		pSingleInfo = ^SingleInfo;
		SingleInfo = object
			proc: pointer;
			user: pObject;
			procedure Stop;
		private
			md: pMultiDelegate;
		end;
		DestructProc = procedure(const info: SingleInfo);
		CallerProc = procedure(const info: SingleInfo; param: pointer);
		OnEmptinessInversedProc = procedure(var md: MultiDelegate; param: pointer);
	protected type
		pSingleRec = ^SingleRec;
		SingleRec = record
			info: SingleInfo;
			name: PoolString;
			priority: sint;
			destruct: DestructProc;
		end;
	protected var
		list: array of SingleRec;
		onEmptinessInversed: OnEmptinessInversedProc;
		userParam: pObject;
		stopped: boolean;
		function FindPos(priority: sint): sint;
		procedure RemoveAt(id: sint);
	public
		procedure Init;
		procedure Done;
		procedure Add(newProc: pointer; newParam: pObject; newPriority: sint = 0);
		procedure Add(newProc: pointer; newParam: pObject; newDestruct: DestructProc; newPriority: sint = 0);
		function Remove(rmProc: pointer; rmParam: pObject): boolean;
		function SetNamed(const newName: PoolString; newProc: pointer; newParam: pObject; newDestruct: DestructProc; newPriority: sint = 0): pSingleInfo;
		function FindNamed(const aName: PoolString): pSingleInfo;
		function RemoveNamed(const rmName: PoolString): boolean;
		function Count: sint;
		function Empty: boolean; cinline
		procedure Clear;
		procedure Call(caller: CallerProc; param: pointer);
		procedure SetCallbacks(onEmptinessInversed: OnEmptinessInversedProc; param: pObject);
	end;

const
	ObjType_MultiDelegate: pointer = @MultiDelegate.Init;

type
	pSingleDelegateInfo = MultiDelegate.pSingleInfo;
	SingleDelegateInfo = MultiDelegate.SingleInfo;

	pModifiableValue = ^ModifiableValue;
	ModifiableValue = object
	type
		OpEnum = (op_Set, op_Add, op_Sub, op_Mul);
		OnChangeProc = procedure(var v: ModifiableValue; param: pointer);
	private type
		ModifierRec = record
			name: PoolString;
			op: OpEnum;
			x: float;
			priority: sint;
		end;
	var
		base, v: float;
		modifiers: array of ModifierRec;
		onChange: OnChangeProc;
		onChangeParam: pointer;
		function FindModifier(const name: PoolString): sint;
		procedure Recalculate;
	public
		procedure Init(const newBase: float);
		procedure Done;
		procedure Invalidate;
		function Valid: boolean;
		procedure Serialize(stream: pStream);
		procedure Deserialize(out v: ModifiableValue; stream: pStream); static;
		procedure DeserializeInplace(stream: pStream);

		procedure SetModifier(const name: PoolString; op: OpEnum; const x: float; priority: sint);
		procedure RemoveModifier(const name: PoolString; expectExisting: boolean = yes);
		procedure SetChangeCallback(proc: OnChangeProc; param: pointer);
		function Empty: boolean;

		property Value: float read v;

	const
		OpIds: array[OpEnum] of string = ('set', 'add', 'sub', 'mul');
	private const
		InvalidOnChangeParam = NULL + 1;
		HAS_BASE_BITN      = 0;
		HAS_MODIFIERS_BITN = 1;
	{$define max := ord(High(OpEnum))} {$define nbits := OP_ENUM_BITS} {$define mask := OP_ENUM_MASK} {$include bits_to_store.inc}
		MODNAME_EXTRA_BITS = OP_ENUM_BITS + 1;
		MODNAMEX_HAS_PRIORITY_BITN = OP_ENUM_BITS;
	end;

const
	ObjType_ModifiableValue: pointer = @ModifiableValue.Done;

type
	InterpolationMode = (erp_Linear, erp_Smooth, erp_CatmullRom, erp_BSpline);

	// TODO: пул?
	pDimensionalPath = ^DimensionalPath;
	DimensionalPath = object(&Object)
	type
		FlagEnum = (LoopedPath);
		FlagSet = set of FlagEnum;
	private type
		pKey = ^tKey;
		tKey = record
			v: pFloat;
			time: float;
			erp_in, erp_out: InterpolationMode;
		end;
	private var
		dims: sint;
		flags: FlagSet;
		keys: array of tKey;
		values: array of float;
		_len: float;
		function GetFlag(flag: FlagEnum): boolean;
		procedure SetFlag(flag: FlagEnum; newValue: boolean);
		function StoreValues(const v: array of float): pFloat;
		function FindKey(const time: float): sint;
		function Interpolate(key, idx: sint; const x: float; method: InterpolationMode): float;
	public
		constructor Init(newDims: sint);
		destructor Done; virtual;
	{$define hdr:=
		procedure AddKey(const v: ty; const newTime: float; erIn, erOut: InterpolationMode);
		procedure AddAUA(const a, b, velA, velB: ty; const start, time: float; const kTimeA, kTimeB: float);}
	{$define ty:=array of float} hdr
	{$define ty:=Vec2} hdr
	{$define ty:=Vec3} hdr
	{$define ty:=Vec4} hdr
	{$define ty:=Quaternion} hdr
	{$undef hdr} {$undef ty}
		procedure Query(const time: float; v: pFloat);

		property Dimensions: sint read dims;
		property Len: float read _len write _len;
		property Looped: boolean index LoopedPath read GetFlag write SetFlag;
	end;

	pDimensionalMove = ^DimensionalMove;
	DimensionalMove = object
	private
		_path: pDimensionalPath;
		_done: boolean;
		_time: float;
		procedure _Init(newDims: sint; newPath: pDimensionalPath);
		procedure _SetPath(newPath: pDimensionalPath);
	public
		procedure Init(newDims: sint);
		procedure Init(newPath: pDimensionalPath);
		procedure Done;
		procedure Process(const dt: float);
		function CurrentF: float;
		function CurrentV2: Vec2;
		function CurrentV3: Vec3;
		function CurrentV4: Vec4;
		function CurrentQ: Quaternion;

		property Path: pDimensionalPath read _path write _SetPath;
		property Finished: boolean read _done;
	end;

const
	ObjType_DimensionalMove: pointer = @DimensionalMove.Done;

type
	EntityActionStopReason = (reason_Done, reason_Abort, reason_Stall);

const
	InterpolationIds: array[InterpolationMode] of string = ('linear', 'smooth', 'catmull-rom', 'b-spline');
	StopReasonIds: array[EntityActionStopReason] of string = ('done', 'abort', 'stall');

type
	pEntityAction = ^EntityAction;
	EntityAction = object(&Object)
	public type
		OnProcessProc = function(action: pEntityAction; const dt: float; const info: SingleDelegateInfo): boolean;
		OnDoneProc = procedure(action: pEntityAction; reason: EntityActionStopReason; const info: SingleDelegateInfo);

		OnProcessArgs = record
			action: pEntityAction;
			dt: pFloat;
			ret: boolean;
		end;

		OnDoneArgs = record
			action: pEntityAction;
			reason: EntityActionStopReason;
		end;
	var
		_stopped: boolean;
	protected
		procedure _Process(entity: pObject; const dt: float); virtual; abstract;
		function _Conflicts(var ac: EntityAction): boolean; virtual;
	public
		onProcess,
		onDone: MultiDelegate;
		constructor Init;
		destructor Done; virtual;
		procedure Process(entity: pObject; const dt: float);
		procedure Stop(reason: EntityActionStopReason);
		property Stopped: boolean read _stopped;
	end;

	pEntityActions = ^EntityActions;
	EntityActions = object
		list: array of pEntityAction;

		procedure Init;
		procedure Done;
		function Add(entity: pObject; ac: pEntityAction): boolean;
		function Process(entity: pObject; const dt: float): boolean;

		function Count: sint;
		function Empty: boolean;
	end;

const
	ObjType_EntityActions: pointer = @EntityActions.Init;

type
	pSlide = ^Slide;
	Slide = object(EntityAction)
	private
		id: PoolString;
	protected
		procedure _Process(entity: pObject; const dt: float); virtual;
		function _Conflicts(var ac: EntityAction): boolean; virtual;
	public
		dm: DimensionalMove;
		constructor Init(path: pDimensionalPath; const newId: PoolString);
		destructor Done; virtual;
	end;

	pDistribution = ^Distribution;
	Distribution = object
	type
		KindEnum = (dis_Constant, dis_Uniform, dis_Bell);
	public
		kind: KindEnum;
		function Constant(const value: float): Distribution; static;
		function Uniform(const min, max: float): Distribution; static;
		function Bell(const min, avg, max: float): Distribution; static;
		function Copy: Distribution;
		function Equals(const a, b: Distribution): boolean; static;
		procedure Done;
		function GenerateValue: float;
		function EstimateMaximum: float;
		function GetParam(id: sint): float;
		property Params[id: sint]: float read GetParam;

	private
		static: array[0 .. {$ifdef Debug} 0 {$else} 2 {$endif}] of float;
		dynamic: pFloat;
	{$ifdef Debug} nParams: sint; {$endif}
		function Create(newKind: KindEnum; const v: array of float): Distribution; static;
	const
		KindIds: array[KindEnum] of string = ('constant', 'uniform', 'bell');
		KindPrefixCodes: array[KindEnum] of string = ('c', 'u', 'b');
		KindInfo: array[KindEnum] of record
			nParams: sint;
		end =
		(
			(nParams: 1),
			(nParams: 2),
			(nParams: 3)
		);
		TypeOf: pointer = @KindIds;
	end;

type
	pResourcePool = ^ResourcePool;
	ResourcePool = object
	type
		pSelf = pResourcePool;

		StreamLoadProc = function(s: pStream): pObject;
		StringLoadProc = function(const stream: string): pObject;
		scoped_enum_ LoadProcKind = (Stream, &String); _end
		LoadProc = record
		case kind: LoadProcKind of
			LoadProcKind.Stream:  (stream: StreamLoadProc);
			LoadProcKind.&String: (&string: StringLoadProc);
		end;

		StartLoadProc = function(const stream: string; param: pointer): pointer;
		EndLoadProc   = procedure(startResult, param: pointer);
		UserReleaseProc = procedure(obj: pObject; param: pointer);
	private type
		pTypeDesc = ^TypeDesc;
		TypeDesc = object
			ctr: LoadProc;
			flags: FileFlags;
			timeout: uint;
			tag: sint;
			function Create(ctr: LoadProc; flags: FileFlags; timeout: uint; tag: sint): pTypeDesc; static;
			function ToFileFlags(justTry: boolean): FileFlags;
			procedure Release(obj: pObject; var rp: ResourcePool);
		end;

		pResKey = ^ResKey;

		pTimerParam = ^TimerParam;
		TimerParam = record
			rp: pResourcePool;
			key: pResKey;
			timer: ThreadTimer;
			cancel: boolean;
		end;

		ResKey = object
			stream: string;
			typ: pointer;
			td: pTypeDesc;
			obj: pObject;
			releasing: pTimerParam;
			procedure Init(const stream: string; typ: pointer; td: pTypeDesc; obj: pObject);
			{$define ResKey_constructor_args := const stream: string; typ: pointer; td: pTypeDesc; obj: pObject}
			{$define pSelf := pResKey} {$define constructor_args := ResKey_constructor_args} {$include dyn_obj.h.inc}
		end;

		{$define classname := VMT2Type} {$define key_type := pointer} {$define value_type := pTypeDesc} {$define null_value := nil} {$include hash.h.inc}
		{$define classname := KeySet} {$define key_type := pResKey} {$define dont_replace} {$define null_value := nil} {$include hash.h.inc}

		{$define classname := ObjectSet} {$define key_type := pResKey} {$define inline_key := pObject} {$define null_value := nil}
		{$include hash.h.inc}

		TagDesc = object
			name: string;
			objs: ObjectSet;
			active: boolean;
			userRelease: UserReleaseProc; // nil — обычное освобождение
			userParam: pointer;
			procedure Init(const name: string);
			procedure Done;
		end;
	private
		loadLock, timerLock: ThreadLock;
		types: VMT2Type;
		keys: KeySet;
		objs: ObjectSet;
		startLoad: StartLoadProc;
		endLoad: EndLoadProc;
		userParam: pointer;
		tags: array of TagDesc;
		lastRegistered: pTypeDesc;
		function FindType(typ: pointer): pTypeDesc;
		function TagIndex(const tag: string; allowCreate: boolean): sint;
		procedure FullLock;
		procedure FullUnlock;
		procedure CancelTimer(k: pResKey);
		procedure Unregister(k: pResKey);
		procedure UnregisterObjectSet(var objs: ObjectSet);
	public
		procedure Init;
		procedure Done; {$include dyn_obj.h.inc}
		function Shared: pSelf; static;
		function Register(typ: pointer; ctr: LoadProc): pSelf;
		function LoadRef(otyp: pointer; const stream: string; justTry: boolean = no): pointer;
	{$ifdef Debug} function Dump(out count: uint): string; {$endif}

		function Registered(typ: pointer): boolean;
		function Loaded(obj: pObject; stream: pString = nil): boolean;
		// работают с последним зарегистрированным типом
		function Timeout(timeout: uint): pSelf;
		function Tag(const tag: string): pSelf;
		function Flags(flags: FileFlags): pSelf;

		// Сейчас не используется, но на всякий случай оставлю.
		procedure OverrideRelease(const tag: string; release: UserReleaseProc; param: pointer);

		// Функция специально для ресурсов, использующих OpenGL, т. е. которые должны выгрузиться раньше, чем будет уничтожено окно :\
		// Им всем выставляется тег GLResourceTag и перед закрытием окна вызывается Deactivate(GLResourceTag).
		// Deactivate(tag) запрещает впредь загружать помеченные этим тегом ресурсы. Сейчас попытка сделать это бросает ошибку. Если всё же
		// понадобится, можно под капотом загружать по-обычному, минуя пул.
		procedure Deactivate(const tag: string);

		procedure SetCallbacks(startLoad: StartLoadProc; endLoad: EndLoadProc; param: pointer);
		procedure ResetCallbacks;
	const
		DefaultTimeout = 30000;
	type
		_SizeofIsTimeout = array[0 .. DefaultTimeout - 1] of byte;
	{$if sizeof(_SizeofIsTimeout) < 30000} {$note Low resource pool timeout!} {$endif}
	end;
	operator :=(f: ResourcePool.StreamLoadProc): ResourcePool.LoadProc;
	operator :=(f: ResourcePool.StringLoadProc): ResourcePool.LoadProc;

type
	pStringTree = ^StringTree;
	StringTree = object
		data: string;
		childs: array of StringTree;
		procedure Init(const newData: string);
		procedure Done;
		function Add(const what: string): pStringTree;
		function Find(const what: string): pStringTree;
		function Dump: string;
		function DestructiveDump: string;
	end;

	pFilesystemCache = ^FilesystemCache;
	FilesystemCache = object
	type
		ChildList = array of sint;

		pNode = ^Node;
		Node = object
			name: PoolString;
			what: FileAttributes;
			corporeal: Tribool;
			parent: sint;
			childs: ChildList;
			tags: array of PoolString;
			procedure AddTag(const tag: PoolString);
			procedure RemoveTag(const tag: PoolString);
			function TaggedWith(const tag: PoolString): boolean;
		{$ifdef Debug} function ToString: string; {$endif}
		private
			function TagIndex(const tag: PoolString): sint;
		end;

		QueryCallback = procedure(var node: Node; id: sint; param: pointer);

	const
		IROOT = 0;

	private
		lock: ThreadLock;
		nodes: array of Node;
		function _Trace(nid: sint): string;
	{$ifdef Debug} function _ToStringTree: StringTree; {$endif}
	public
		procedure Init;
		procedure Done; {$define pSelf := pFilesystemCache} {$include dyn_obj.h.inc}
		function Query(parent: sint; const name: string; cb: QueryCallback = nil; param: pointer = nil): sint;
		function Query(parent: sint; name: pChar; nameLen: sint; cb: QueryCallback = nil; param: pointer = nil): sint;
		function Trace(nid: sint): string;
		procedure TagWith(id: sint; const tag: string);
		function TaggedWith(id: sint; const tag: string): boolean;
		procedure ForceCorporeality(id: sint);
	end;
	function FsCache: pFilesystemCache;

type
	Dir4 = object
	type
		Enum = (_Left, _Up, _Right, _Down);

	private
		_value: Enum;
	public
		property value: Enum read _value;
		function CW: Dir4; cinline
		function CCW: Dir4; cinline
		function Reverse: Dir4; cinline
		function Dx: sint; cinline
		function Dy: sint; cinline
	const
		Left:  Dir4 = (_value: _Left);
		Right: Dir4 = (_value: _Right);
		Up:    Dir4 = (_value: _Up);
		Down:  Dir4 = (_value: _Down);
		All = [Low(Enum) .. High(Enum)];

		EnumCount = uint(High(Enum)) + 1;
		Delta: array[Enum] of record
			x, y: sint;
		end =
		(
			(x: -1; y: 0),
			(x: 0;  y: -1),
			(x: 1;  y: 0),
			(x: 0;  y: 1)
		);
	end;
	operator :=(const value: Dir4.Enum): Dir4; cinline
	operator =(const a, b: Dir4): boolean; cinline

type
	pLoaderSuite = ^LoaderSuite;
	LoaderSuite = object
	type
		PlainLoad = procedure(obj: pointer; stream: pStream);
		SizedLoad = procedure(obj: pointer; stream: pStream; const size: FileSize);
		PlainSave = procedure(obj: pointer; stream: pStream);
		StringOptsSave = procedure(obj: pointer; stream: pStream; const opts: string);

		scoped_enum_ LoadInterface = (Plain, Sized); _end
		LoadProc = record
		case typ: LoadInterface of
			LoadInterface.Plain: (plain: PlainLoad);
			LoadInterface.Sized: (sized: SizedLoad);
		end;

		scoped_enum_ SaveInterface = (None, Plain, StringOpts); _end
		SaveProc = record
		case typ: SaveInterface of
      	SaveInterface.None: (funcPtr: CodePointer);
			SaveInterface.Plain: (plain: PlainSave);
			SaveInterface.StringOpts: (stringOpts: StringOptsSave);
		end;

		pLoaderDesc = ^LoaderDesc;
		LoaderDesc = record
			ext: string;
			load: LoadProc;
			save: SaveProc;
		end;

		Proxy = object
			function Opts(const value: string): Proxy;
			function ForceLoader(const value: string): Proxy;
			function Size(const value: FileSize): Proxy;

			procedure Load(obj: pointer; stream: pStream);
			procedure Save(obj: pointer; stream: pStream);
			procedure Save(obj: pointer; const stream: string);
		private
			suite: pLoaderSuite;
			useOpts: string;
			useForceLoader: string;
			useSize: FileSize;
		end;

		function Register(const exts: string; const load: LoadProc): pLoaderSuite;
		function Register(const exts: string; const load: LoadProc; const save: SaveProc): pLoaderSuite;
		procedure Load(obj: pointer; stream: pStream);
		procedure Save(obj: pointer; stream: pStream);
		procedure Save(obj: pointer; const stream: string);
		function GetSaveInterface(const loader: string): SaveInterface;
		function Dump: string;

		function Opts(const value: string): Proxy;
		function ForceLoader(const value: string): Proxy;
		function Size(const value: FileSize): Proxy;
	private
		loaders: array of LoaderDesc;
		function CreateProxy: Proxy;
		procedure Load(obj: pointer; stream: pStream; const extra: Proxy);
		procedure Save(obj: pointer; stream: pStream; const extra: Proxy);
		procedure Save(obj: pointer; const stream: string; const extra: Proxy);
		function Find(const ext: string): pLoaderDesc;
		function Find(const spath, forced: string; isSave: boolean): pLoaderDesc;
	end;
	operator :=(f: LoaderSuite.PlainLoad): LoaderSuite.LoadProc;
	operator :=(f: LoaderSuite.SizedLoad): LoaderSuite.LoadProc;
	operator :=(f: LoaderSuite.PlainSave): LoaderSuite.SaveProc;
	operator :=(f: LoaderSuite.StringOptsSave): LoaderSuite.SaveProc;

type
	{$define classname := ObjectsList} {$define item_type := pObject}
	{$include vector-link.h.inc}

implementation

uses
	Random, TextProcessing
{$ifdef use_serialization}, Serialization {$endif}
{$ifdef selftest}, Tests {$endif}
{$ifdef Debug}, ULog {$endif}
{$ifdef Profile}, Profile{$endif};

{$ifdef DebugDelayedRelease}
	{$define rp_dpr := Con.WriteLine}
{$else}
	{$define rp_dpr := //}
{$endif}

	{$define classname := ResourcePool.VMT2Type} {$define hash_func := Hash.OfPointer} {$define finalize_value := dispose(_1)}
	{$include hash.pp.inc}

	{$define classname := ResourcePool.KeySet} {$define inline_hash := Hash.OfPointer(_1^.typ) xor Hash.OfString(_1^.stream)}
	{$define inline_eq := (_1 = _2) or ((_1^.typ = _2^.typ) and (_1^.stream = _2^.stream))} {$define finalize_key := _1^.Free}
	{$include hash.pp.inc}

	{$define classname := ResourcePool.ObjectSet} {$define get_key := _1^.obj} {$define hash_func := Hash.OfPointer}
	{$include hash.pp.inc}

	{$define classname := ObjectsList} {$include vector-link.pp.inc}

	procedure ValueDynamics.Init;
	begin
		controlPeriod := Ticks.FromSeconds(0.2);
		_avgPerSec := 0.0;
		_avgPerFrame := 0.0;
		_periodActive := no;
	end;

	procedure ValueDynamics.Done;
	begin
	end;

	procedure ValueDynamics.NewValue(value: size_t; frameNo: sint);
	var
		ct, t: Ticks;
		seconds: hp_float;
		frames: uint;
	begin
		ct := Ticks.Get;
		if not _periodActive then
		begin
			_curStartTime := ct;
			_curStartFrame := frameNo;
			_curSum := 0;
			_periodActive := yes;
			exit;
		end;

		_curSum := _curSum + value;
		t := ct - _curStartTime;
		if t >= controlPeriod then
		begin
			seconds := t.ToSeconds;
			if seconds <> 0.0 then _avgPerSec := _curSum / seconds else _avgPerSec := 0.0;
			frames := abs(frameNo - _curStartFrame);
			if frames <> 0 then _avgPerFrame := _curSum / frames else _avgPerFrame := 0.0;
			_periodActive := no;
		end;
	end;

type
	pInternedStrings = ^InternedStrings;
	InternedStrings = object
		procedure Init;
		procedure Done;
		function Intern(const s: string): string;
		function Interned(const s: string): boolean;
		function StoredCount: size_t;

	private type
		pListItem = ^ListItem;
		{$define classname := ItemSet} {$define key_type := pListItem} {$define inline_key := string}
		{$define huge_keys} {$define dont_replace} {$define delayed_key} {$define on_new_ret} {$define store_iter}
		{$include hash.h.inc}

		ListItem = record
			s: string;
			stsIt: ItemSet.Iterator;
		end;

		{$define classname := ItemList} {$define item_type := ListItem} {$define store_ptr} {$define user_param := pInternedStrings}
		{$include vector-link.h.inc}

	var
		lock: ThreadLock;
		sts: ItemSet;
		list: ItemList;
		sweepPos: ItemList.Iterator;
		procedure Sweep(amount: uint);
	end;

	{$define classname := InternedStrings.ItemSet} {$define hash_func := Hash.OfString} {$define get_key := _1^.s}
	{$define store_iter := _1^.stsIt}
	{$include hash.pp.inc}

	{$define classname := InternedStrings.ItemList}
	{$define store_ptr := param^.sts.GetKey(item.stsIt)^ := ptr}
	{$include vector-link.pp.inc}

	procedure InternedStrings.Init;
	begin
		lock.Init;
		sts.Init;
		list.Init(@self);
		sweepPos := list.GetIterator;
	end;

	procedure InternedStrings.Done;
	begin
		list.Done;
		sts.Done;
		lock.Done;
	end;

	function InternedStrings.Intern(const s: string): string;
	var
		found: ^pListItem;
		new: boolean;
		it: pListItem;
		stsIt: ItemSet.Iterator;
	begin
		lock.Enter;
		found := sts.Add(s, new, stsIt);
		if new then
		begin
			it                 := list.Grow(1);
			found^             := it;
			it^.s              := s;
			it^.stsIt          := stsIt;

		{$ifdef Debug}
	  	{$ifdef DebugStrPool} Log('Interned ++ "{0}", теперь {1}.', s, ToString(list.ItemsCount), logDebug); {$endif}
			stat.Note(max_strpool_len, list.Count);
		{$endif}
		end;
		result := found^^.s;

		Assert(not new or (pointer(found^^.s) = pointer(s)));
		Sweep(1 + ord(new));
		lock.Leave;
	end;

	function InternedStrings.Interned(const s: string): boolean;
	var
		p: ^pListItem;
	begin
		lock.Enter;
		p := sts.Find(s);
		lock.Leave;
		result := Assigned(p) and (pointer(p^^.s) = pointer(s));
	end;

	function InternedStrings.StoredCount: size_t;
	begin
		lock.Enter;
		result := list.Count;
		lock.Leave;
	end;

	procedure InternedStrings.Sweep(amount: uint);
	var
		refs: PtrInt;
	begin
		if amount > list.Count then amount := list.Count;
		while amount > 0 do
		begin
			if not list.Valid(sweepPos) then
			begin
				sweepPos := list.GetIterator;
			{$ifdef Debug} if not list.Valid(sweepPos) then Assert(no); {$endif}
			end;

			refs := StringRefCount(list.GetItemPtr(sweepPos)^.s);
		{$ifdef Debug} stat.Note(max_strpool_refcount, max(0, refs)); {$endif}
			if refs = 1 then
			begin
			{$ifdef DebugStrPool} Log('Interned -- "{0}", теперь {1}.', list.GetItemPtr(sweepPos)^.s, ToString(list.ItemsCount), logDebug); {$endif}
				sts.Remove(list.GetItemPtr(sweepPos)^.stsIt);
				list.RemoveReplace(sweepPos);
			end else
				list.Next(sweepPos);
			dec(amount);
		end;
	end;

var
	internedPool: InternedStrings;

	function InternedPoolSize: size_t;
	begin
		result := internedPool.StoredCount;
	end;

	function GetInternedString(id: uint; param: pointer): string;
	begin
		result := MaybeQuote(InternedStrings(param^).list.ItemPtrFromIndex(id)^.s);
	end;

	function DumpInternedStrings(out count: uint): string;
	begin
		internedPool.lock.Enter;
		count := internedPool.list.Count;
		result := SeparatedList.Join(internedPool.list.Count, @GetInternedString, @internedPool,
			', ' + SeparatedList.Empty + '(пусто)');
		internedPool.lock.Leave;
	end;

	procedure CleanupInternedPool(n: uint = High(uint));
	begin
		internedPool.lock.Enter;
		if n = High(uint) then n := internedPool.list.Count;
		internedPool.Sweep(n);
		internedPool.lock.Leave;
	end;

	function PoolString.Hash: Hash.Value;               begin result := Algo.Hash.OfPointer(pChar(internal)); end;
	function PoolString.ToIndex: pointer;               begin result := pChar(internal); end;
	function PoolString.Empty: boolean;                 begin result := internal = ''; end;
	operator =(const a, b: PoolString): boolean;        begin result := pChar(a.internal) = pChar(b.internal); end;
	operator =(const a: string; b: PoolString): boolean; begin result := a = b.internal; end;
	operator =(const a: PoolString; b: string): boolean; begin result := a.internal = b; end;
	operator :=(const s: PoolString): string;           begin result := s.internal; end;
	operator :=(const s: string): PoolString;           begin result.internal := internedPool.Intern(s); end;
	operator +(const s: string; a: PoolString): string; begin result := s + a.internal; end;

	procedure MemoryPool.Init(const name: string; blockSize: size_t);
	begin
	{$ifdef Debug} self.name := name; {$else} unused_args name end_list {$endif}
		self.blockSize := blockSize;
		lock.Init;
		blocks := nil;
		nBlocks := 0;
		timer.Invalidate;
	{$ifdef Debug} nAllocated := 0; {$endif}
	end;

	procedure FreeBlock(index: uint; param: pointer);
	begin
		USystem.FreeMem(pMemoryPool(param)^.blocks[index]);
	end;

	procedure MemoryPool.Done;
	begin
	{$ifdef Debug}
		if nAllocated <> 0 then
			Log('{0}утечка ({1}))!', MessagePrefix, ToString(nAllocated), logError);
	{$endif}
		timer.Close;
		Range.Open(nBlocks).Each(@FreeBlock, @self);
		lock.Done;
	end;

	function MemoryPool.Cleanup: CleanupReport;
	begin
		Assert(minFreeBlocksForPeriod <= nBlocks);
		if minFreeBlocksForPeriod > 0 then
		begin
		{$ifdef DebugMemoryPool} Log(MessagePrefix + 'очистка с ' + ToString(nBlocks) + ' до ' + ToString(nBlocks - minFreeBlocksForPeriod) + ' (выделено: ' + ToString(nAllocated) + ')', logDebug); {$endif}
			Range.Open(nBlocks - minFreeBlocksForPeriod, nBlocks).Each(@FreeBlock, @self);
			nBlocks -= minFreeBlocksForPeriod;
			if nBlocks < length(blocks) div 4 then SetLength(blocks, 2 * nBlocks);
		end else
		{$ifdef DebugMemoryPool} Log(MessagePrefix + 'делать нечего (свободных блоков: ' + ToString(nBlocks) + ', выделенных: ' + ToString(nAllocated) + ', операций за период: ' + ToString(opsForPeriod) + ')', logDebug) {$endif};
		minFreeBlocksForPeriod := nBlocks;
		result.opsForPeriod := opsForPeriod;
		opsForPeriod := 0;
	end;

{$ifdef Debug} function MemoryPool.MessagePrefix: string; begin result := 'Пул памяти ' + name + ': '; end; {$endif}

	procedure MemoryPoolCleanupTimer(param: pointer; var instance: ThreadTimer.CallbackInstance);
	var
		pool: pMemoryPool absolute param;
		rep: MemoryPool.CleanupReport;
	begin
		pool^.lock.Enter;
		rep := pool^.Cleanup;
		if (rep.opsForPeriod = 0) and (pool^.nBlocks = 0) then
		begin
		{$ifdef DebugMemoryPool} Log(pool^.MessagePrefix + 'таймер уничтожен', logDebug); {$endif}
			instance.Close;
		end;
		pool^.lock.Leave;
	end;

	function MemoryPool.GetBlock: pointer;
	begin
		lock.Enter;
	{$ifdef Debug} inc(nAllocated); {$endif}
		inc(opsForPeriod);
		if nBlocks > 0 then
		begin
			dec(nBlocks);
			result := blocks[nBlocks];
			if nBlocks < minFreeBlocksForPeriod then minFreeBlocksForPeriod := nBlocks;
			lock.Leave;
		end else
		begin
			minFreeBlocksForPeriod := 0;
			lock.Leave;
			result := USystem.GetMem(blockSize);
		end;
	end;

	procedure MemoryPool.ReturnBlock(p: pointer);
	begin
		Assert(Assigned(p));
		lock.Enter;
		inc(opsForPeriod);
		inc(nBlocks);
		if nBlocks >= length(blocks) then SetLength(blocks, 2 * nBlocks);
		blocks[nBlocks - 1] := p;
	{$ifdef Debug} dec(nAllocated); {$endif}
		if not timer.Valid then
		begin
		{$ifdef DebugMemoryPool} Log(MessagePrefix + 'таймер создан', logDebug); {$endif}
			minFreeBlocksForPeriod := nBlocks;
			ThreadTimer.Open(timer, @MemoryPoolCleanupTimer, @self, CleanupPeriod, CleanupPeriod);
		end;
		lock.Leave;
	end;

	function MemoryPool.GetMem(size: size_t): pointer;
	begin
		if size = 0 then exit(nil);
		if size <= blockSize then
			result := GetBlock
		else
			result := USystem.GetMem(size);
	end;

	procedure MemoryPool.FreeMem(p: pointer; size: size_t);
	begin
		if Assigned(p) and (size <= blockSize) then
			ReturnBlock(p)
		else
			USystem.FreeMem(p);
	end;

	function MemoryPool.ReallocMem(p: pointer; osize, nsize: size_t): pointer;
	begin
		if not Assigned(p) then exit(self.GetMem(nsize));
		if nsize = 0 then
		begin
			Assert(osize <> 0);
			self.FreeMem(p, osize);
			exit(nil);
		end;
		if osize <= blockSize then
		begin
			if nsize <= blockSize then exit(p) else
			begin
				result := USystem.GetMem(nsize);
				if osize < nsize then memcpy(p, result, osize) else memcpy(p, result, nsize);
				ReturnBlock(p);
			end;
		end else
		begin
			if nsize <= blockSize then
			begin
				result := GetBlock;
				if osize < nsize then memcpy(p, result, osize) else memcpy(p, result, nsize);
				USystem.FreeMem(p);
			end else
				result := System.ReallocMem(p, nsize);
		end;
	end;

	procedure StringBuilder.Init;
	begin
		chStatic.next := nil;
		chLast := @chStatic;
		cp := @chStatic.data[0];
		cend := @chStatic.data[0] + length(chStatic.data);
		chWoLast := 0;
	end;

	procedure StringBuilder.Done;
	var
		c, t: pChain;
	begin
		c := chStatic.next;
		while Assigned(c) do
		begin
			t := c;
			c := c^.next;
			FreeMem(t);
		end;
	end;

	procedure StringBuilder.Append(ch: pChar; count: size_t);
	begin
	{$ifdef sb_fine}
		if cp + count <= cend then
		begin
			memcpy(ch, cp, count);
			cp += count;
		end else
			FineAppend(ch, count);
	{$else}
		memcpy(ch, Grow(count), count);
	{$endif}
	end;

{$define func:=
	procedure StringBuilder.Append(const _ARGS_: char);
	var
		p: pChar;
	begin
	{$ifdef sb_fine}
		if cp + _NARGS_ <= cend then
		begin
			p := cp;
		{$define iterate := p[_IARG_] := _ARG_;} _FOREACH_
			cp += _NARGS_;
		end else
		begin
		{$define iterate := FineAppend(@_ARG_, 1);} _FOREACH_
		end;
	{$else}
		p := Grow(_NARGS_);
		{$define iterate := p[_IARG_] := _ARG_;} _FOREACH_
	{$endif}
	end;

	procedure StringBuilder.Append(const _ARGS_: string);
	var
		p: pChar;
		totalLen: size_t;
	begin
		totalLen := {$define conv := length(_ARG_)} {$define sep := +} _ARGS_;
	{$ifdef sb_fine}
		if cp + totalLen <= cend then
		begin
			p := cp;
		{$define iterate:=memcpy(pChar(_ARG_), p, length(_ARG_) * sizeof(char)); p += length(_ARG_);} _FOREACH_
			cp += totalLen;
		end else
		begin
		{$define iterate:=FineAppend(pChar(_ARG_), length(_ARG_));} _FOREACH_
		end;
	{$else}
		p := Grow(totalLen);
	{$define iterate:=memcpy(pChar(_ARG_), p, length(_ARG_) * sizeof(char)); p += length(_ARG_);} _FOREACH_
	{$endif}
	end;} {$include variadic.inc}

	function StringBuilder.ToString: string;
	var
		pos, count: size_t;
		c: pChain;
	begin
		SetLength(result, Len);
		pos := 0;
		c := @chStatic;
		while Assigned(c) do
		begin
			if c = chLast then count := cp - @chLast^.data[0] else count := c^.count;
			memcpy(@c^.data[0], pChar(result) + pos, count);
			pos += count;
			c := c^.next;
		end;
	end;

	function StringBuilder.DestructiveToString: string;
	begin
		result := ToString;
		Done;
	end;

	function StringBuilder.Len: size_t;
	begin
		result := chWoLast + size_t(cp - @chLast^.data[0]);
	end;

	procedure StringBuilder.InstanceMovedFrom(var old: StringBuilder);
	begin
		Assert((chLast = @old.chStatic) or (chLast = old.chLast));
		if chLast = @old.chStatic then
		begin
			chLast := @chStatic;
			cp := @chStatic.data[0] + (cp - @old.chStatic.data[0]);
			cend := @chStatic.data[0] + (cend - @old.chStatic.data[0]);
		end;
	end;

	procedure StringBuilder.Clear;
	begin
		Done;
		Init;
	end;

{$ifndef sb_fine}
	function StringBuilder.Grow(by: size_t): pChar;
	var
		n: pChar;
	begin
		result := cp;
		n := cp + by;
		if n <= cend then
			cp := n
		else
			result := AllocateBlock(by);
	end;
{$endif}

	function StringBuilder.AllocateBlock(apsize: size_t): pChar;
	const
		Header = size_t(sizeof(Chain) - sizeof(Chain.data));
	var
		c: pChain;
		wasAllocated, allocate: size_t;
	begin
		wasAllocated := cend - @chLast^.data[0];
		chLast^.count := cp - @chLast^.data[0]; Assert((chLast = @chStatic) or (chLast^.count > 0));
		chWoLast += chLast^.count;

		allocate := UpToPow2(Header + 4 * wasAllocated);
		if allocate < Header + apsize then allocate := UpToPow2(Header + 4 * wasAllocated + apsize);
		c := GetMem(allocate);
		c^.next := nil;
		chLast^.next := c;
		chLast := c;
		result := @c^.data[0];
		cp := result + apsize;
		cend := result + (allocate - Header);
	end;

	function StringBuilder.GetSym(id: size_t): char;
	var
		c: pChain;
	begin
		Assert(id < Len);
		if id >= chWoLast then result := (@chLast^.data[0] + (id - chWoLast))^ else
		begin
			c := @chStatic;
			while id >= c^.count do
			begin
				id -= c^.count;
				c := c^.next;
				Assert(Assigned(c));
			end;
			result := c^.data[id];
		end;
	end;

{$ifdef sb_fine}
	procedure StringBuilder.FineAppend(ch: pChar; count: size_t);
	var
		n: size_t;
	begin
		if cp + count <= cend then
		begin
			memcpy(ch, cp, count);
			cp += count;
		end else
		begin
			n := cend - cp;
			memcpy(ch, cp, n); cp += n; ch += n; count -= n;
			memcpy(ch, AllocateBlock(count), count);
		end;
	end;
{$endif}

	function Bitfield.GetBit(id: uint): boolean;
	begin
		Assert(ValidateIndex(id));
		result := pByte(_data)[id div Base] and (1 shl (id mod Base)) <> 0;
	end;

	procedure Bitfield.SetBit(id: uint; newValue: boolean);
	begin
		Assert(ValidateIndex(id));
		if newValue then
			pByte(_data)[id div bitsizeof(byte)] := pByte(_data)[id div bitsizeof(byte)] or (byte(1) shl (id mod Base))
		else
			pByte(_data)[id div bitsizeof(byte)] := pByte(_data)[id div bitsizeof(byte)] and not (byte(1) shl (id mod Base));
	end;

	procedure Bitfield.SetSize(newSize: uint);
	var
		newDataSize: size_t;
		i: sint;
	begin
		if _size = newSize then exit;
		newDataSize := (newSize + (Base - 1)) div Base;
		if _dataSize <> newDataSize then
		begin
			if _dataSize <= sizeof(staticData) then
				if newDataSize <= sizeof(staticData) then
				begin
					// всё в порядке
				end else
				begin
					_data := GetMem(newDataSize);
					memcpy(@staticData, _data, _dataSize);
				end
			else
				if newDataSize <= sizeof(staticData) then
				begin
					memcpy(_data, @staticData, newDataSize);
					FreeMem(_data);
					_data := @staticData;
				end else
				begin
					ReallocMem(_data, newDataSize);
				end;
			_dataSize := newDataSize;
		end;
		if newSize < _size then
		begin
			i := (Base - newSize mod Base) mod Base;
			if _dataSize > 0 then
				pByte(_data)[_dataSize - 1] := byte((pByte(_data)[_dataSize - 1] shl i)) shr i;
		end else
			Zero(_data + (_size + (Base - 1)) div Base, (newSize + (Base - 1)) div Base - (_size + (Base - 1)) div Base);
		_size := newSize;
	end;

	procedure Bitfield.Init(newSize: uint);
	begin
		_size := 0;
		_dataSize := 0;
		_data := @staticData;
		Size := newSize;
	end;

	procedure Bitfield.Done;
	begin
		if _dataSize > sizeof(staticData) then FreeMem(_data);
	end;

	function Bitfield.Dump(const str0: string = '0'; const str1: string = '1'): string;
	var
		i: uint;
		strs: array[boolean] of string;
		sb: StringBuilder;
	begin
		strs[no] := str0;
		strs[yes] := str1;

		sb.Init;
		i := 0;
		while i < Size do
		begin
			sb.Append(strs[GetBit(i)]);
			inc(i);
		end;
		result := sb.DestructiveToString;
	end;

	function Bitfield.ValidateIndex(idx: uint): boolean;
	begin
		result := idx < _size;
	end;

	function Bitfield.Ones: boolean;
	var
		i, r: uint;
	begin
		i := 0;
		r := Size div Base;
		while i < r do
		begin
			if pByte(_data)[i] <> 1 shl Base - 1 then exit(no);
			inc(i);
		end;

		i := Size - Size mod Base;
		while i < Size do
		begin
			if not GetBit(i) then exit(no);
			inc(i);
		end;
		result := yes;
	end;

	procedure Bitfield.FillWithOnes;
	var
		i, r: uint;
	begin
		i := 0;
		r := Size div Base;
		while i < r do
		begin
			pByte(_data)[i] := 1 shl Base - 1;
			inc(i);
		end;

		i := Size - Size mod Base;
		while i < Size do
		begin
			SetBit(i, yes);
			inc(i);
		end;
	end;

	procedure Bitfield2D.AllocateEmpty(const newSize: UintVec2; fromScratch: boolean);
	begin
		if not fromScratch then raw.Done;
		raw.Init(newSize.Product);
		_size := newSize;
	end;

	function Bitfield2D.BitAt(x, y: uint): boolean;
	begin
		Assert(ValidateIndex(x, y));
		result := raw.GetBit(y * _size.x + x);
	end;

	procedure Bitfield2D.SetBitAt(x, y: uint; newValue: boolean);
	begin
		Assert(ValidateIndex(x, y), Format('{0}, {1} / {2}, {3}', [x, y, _size.x, _size.y]));
		raw.SetBit(y * _size.x + x, newValue);
	end;

	function Bitfield2D.Extract(const pos, size: UintVec2): BoolArray;
	var
		x, y: uint;
	begin
		SetLength(result, size.Product);
		y := pos.y;
		while y < pos.y + size.y do
		begin
			x := pos.x;
			while x < pos.x + size.x do
			begin
				result[(y - pos.y) * size.x + (x - pos.x)] := BitAt(x, y);
				inc(x);
			end;
			inc(y);
		end;
	end;

	procedure Bitfield2D.Init(const newSize: UintVec2);
	begin
		AllocateEmpty(newSize, yes);
	end;

	procedure Bitfield2D.Init(const data: string);
	const
		AltEOL = '|';
		EmptyCell = [' ', TabSym, '.'];
	var
		thenTrim: boolean;
		rows: array of record
			data: array of boolean;
			spaces, leftEdge, nData: uint;
		end;
		xscale, minSpaces, maxWidth: uint;
		x, y, p, row {$ifdef Debug}, i {$endif}: sint;
		neol: size_t;
		t: StringTokenizer;
		cp: t.Guard;
	begin
		xscale := 2;
		thenTrim := yes;

		p := 1;
		t := data;
		try
			if t.Maybe('[') then
			begin
				repeat
					case t.ScanTokenEndingWith(['=', ',', ']'], cp) of
						'dont-trim': thenTrim := no;
						'xscale': begin t.Expect('='); xscale := RangeCheck(t.ScanUintToken, 1, 2, 'xscale'); end;
						else raise t.UnknownIdentifier(cp);
					end;

					if t.Maybe(',') then else begin t.Expect(']'); break; end;
				until no;
				p += sint(t.Consumed);
			end;
		finally
			t.Done;
		end;

		if UTF8.IsEOL(pChar(data) + (p-1), length(data) - p + 1, neol) then
			uint(p) += neol;

		rows := nil;
		minSpaces := High(minSpaces);
		row := -1;
		repeat
			inc(row);
			if row >= length(rows) then SetLength(rows, 2 * (row + 1));
			rows[row].nData := 0;
			rows[row].spaces := 0;
			while (rows[row].spaces < minSpaces) and (p <= length(data)) and (data[p] in [' ', TabSym]) do
			begin
				rows[row].spaces += 1;
				inc(p);
			end;

			while p <= length(data) do
			begin
				if UTF8.IsEOL(pChar(data) + (p - 1), length(data) - p + 1, neol) then begin uint(p) += neol; break; end;
				if data[p] = AltEOL then begin uint(p) += 1; break; end;

				inc(rows[row].nData); if rows[row].nData > uint(length(rows[row].data)) then SetLength(rows[row].data, 2 * rows[row].nData);
				rows[row].data[rows[row].nData - 1] := not (data[p] in EmptyCell);
			{$ifdef Debug}
				for i := 2 to xscale do
					if data[p + i - 2] <> data[p] then
						raise Error('{0}:{1}: ожидается дублирующийся символ к {2}, прочитан ''{3}''.', [row, rows[row].nData-1, data[p], data[p + i - 1]]);
			{$endif}
				uint(p) += xscale;
			end;

			if rows[row].nData > 0 then minSpaces := min(minSpaces, rows[row].spaces) else rows[row].spaces := 0;
		until p > length(data);

		maxWidth := 0;
		for y := 0 to row do
		begin
			if rows[y].spaces > minSpaces then rows[y].leftEdge := (rows[y].spaces - minSpaces) div xscale else rows[y].leftEdge := 0;
			maxWidth := max(maxWidth, rows[y].leftEdge + rows[y].nData);
		end;

		AllocateEmpty(UintVec2.Make(maxWidth, 1 + row), yes);
		for y := 0 to row do
			for x := 1 to rows[y].nData do
				raw[maxWidth * uint(y) + rows[y].leftEdge + uint(x - 1)] := rows[y].data[uint(x - 1)];
		if thenTrim then Trim;
	end;

	procedure Bitfield2D.Done;
	begin
		raw.Done;
	end;

	procedure Bitfield2D.Resize(const newSize: UintVec2);
	var
		tmp: BoolArray;
		x, y: uint;
		tmpSize: UintVec2;
	begin
		if _size.x = newSize.x then
		begin
			if _size.y <> newSize.y then
			begin
				_size.y := newSize.y;
				raw.SetSize(_size.Product);
			end;
			exit;
		end;
		tmpSize := min(_size, newSize);
		tmp := Extract(UintVec2.Zero, tmpSize);

		AllocateEmpty(newSize, no);
		y := 0;
		while y < tmpSize.y do
		begin
			x := 0;
			while x < tmpSize.x  do
			begin
				SetBitAt(x, y, tmp[y * tmpSize.x + x]);
				inc(x);
			end;
			inc(y);
		end;
	end;

	procedure Bitfield2D.Trim;
	var
		min, max, cur: UintVec2;
		first: boolean;
		x, y: uint;
		tmpSize: UintVec2;
		tmp: BoolArray;
	begin
		min := UintVec2.Ones;
		max := UintVec2.Zero;
		first := yes;
		y := 0;
		while y < _size.y do
		begin
			x := 0;
			while x < _size.x do
			begin
				if BitAt(x, y) then
				begin
					cur := UintVec2.Make(x, y);
					if first then
					begin
						min := cur;
						max := cur;
						first := no;
					end else
					begin
						min := UMath.Min(min, cur);
						max := UMath.Max(max, cur);
					end;
				end;
				inc(x);
			end;
			inc(y);
		end;

		tmpSize := max + UintVec2.Ones - min;
		tmp := Extract(min, tmpSize);
		AllocateEmpty(tmpSize, no);
		y := 0;
		while y < tmpSize.y do
		begin
			x := 0;
			while x < tmpSize.x do
			begin
				if tmp[y * tmpSize.x + x] then
					SetBitAt(x, y, yes);
				inc(x);
			end;
			inc(y);
		end;
	end;

	function Bitfield2D.Dump(const str0: string = '0'; const str1: string = '1'): string;
	var
		strs: array[boolean] of string;
		sb: StringBuilder;
		x, y: uint;
	begin
		strs[no] := str0;
		strs[yes] := str1;

		sb.Init;
		y := 0;
		while y < _size.y do
		begin
			x := 0;
			while x < _size.x do
			begin
				sb.Append(strs[BitAt(x, y)]);
				inc(x);
			end;
			if y + 1 < _size.y then
				sb.Append(EOL);
			inc(y);
		end;
		result := sb.DestructiveToString;
	end;

	procedure Bitfield2D.Serialize(s: pStream; packsxy, justsizes: boolean);
	begin
		if packsxy then
			Serialize_ui8(s, (_size.x shl (bitsizeof(uint8) div 2)) or _size.y)
		else
		begin
			Serialize_ui8(s, _size.x);
			Serialize_ui8(s, _size.y);
		end;
		if justsizes then exit;
		s^.Write(raw.data, raw.dataSize);
	end;

	procedure Bitfield2D.Deserialize(s: pStream; packsxy, justsizes: boolean);
	var
		t: uint;
		sz: UintVec2;
	begin
		if packsxy then
		begin
			t := Deserialize_ui8(s);
			sz := UintVec2.Make(t shr (bitsizeof(uint8) div 2), t and (1 shl (bitsizeof(uint8) div 2) - 1));
		end else
		begin
			sz.x := Deserialize_ui8(s);
			sz.y := Deserialize_ui8(s);
		end;
		if not sz.Positive then raise Error('Маска повреждена.');

		Resize(sz);
		if justsizes then exit;
		s^.Read(raw.data, raw.dataSize);
	end;

	function Bitfield2D.Bit(const pos: UintVec2): boolean;
	begin
		result := BitAt(pos.x, pos.y);
	end;

	procedure Bitfield2D.SetBit(const pos: UintVec2; newValue: boolean = yes);
	begin
		SetBitAt(pos.x, pos.y, newValue);
	end;

	function Bitfield2D.ValidatePoint(const pos: UintVec2): boolean;
	begin
		result := ValidateIndex(pos.x, pos.y);
	end;

	function Bitfield2D.ValidateIndex(x, y: uint): boolean;
	begin
		result := (x < _size.x) and (y < _size.y);
	end;

{$define combine_impl :=
	function internal_func(var a: Bitfield; const b: Bitfield; astart, bstart, n: uint): boolean;
	var
		i: uint;
	begin
		i := 0;
		while i < n do
		begin
			{$define ia := astart + i} {$define ib := bstart + i} body; {$undef ia} {$undef ib}
			inc(i);
		end;
		result := yes;
	end;

	{$ifdef bool_result} function {$else} procedure {$endif} Bitfield2D.interface_func(
		const b: Bitfield2D; const ofs: UintVec2) {$ifdef bool_result}: boolean {$endif};
	begin
	{$ifdef bool_result} result := (ofs + b._size).FitsClosed(size) and {$endif} Combine(b, ofs, @internal_func);
	end;
	{$undef body} {$undef internal_func} {$undef interface_func} {$undef bool_result}}

	{$define interface_func := FitsWithOnes} {$define internal_func := RowFitsWithOnes} {$define body := if b[ib] and a[ia] then exit(no)} {$define bool_result} combine_impl
	{$define interface_func := InplaceOr} {$define internal_func := InplaceOrRow} {$define body := if b[ib] then a[ia] := yes} combine_impl
	{$define interface_func := InplaceXor} {$define internal_func := InplaceXorRow} {$define body := if b[ib] then a[ia] := not a[ia]} combine_impl
	{$define interface_func := InplaceAndNot} {$define internal_func := InplaceAndNotRow} {$define body := if b[ib] then a[ia] := no} combine_impl
{$undef combine_impl}

	function Bitfield2D.ToRects: Rects;
	var
		r: Rects absolute result;

		function Covered(const point: UintVec2): boolean;
		var
			i: sint;
		begin
			for i := 0 to High(r) do
				if r[i].Contains(point) then
					exit(yes);
			result := no;
		end;

		function GetSizeY(const pos: UintVec2; sizeX: uint): uint;
		var
			x, y, ofs: uint;
		begin
			result := 0;
			ofs := pos.y * _size.x + pos.x;
			y := pos.y;
			while y < _size.y do
			begin
				x := pos.x;
				while x < pos.x + sizeX do
				begin
					if (not raw[ofs]) or Covered(UintVec2.Make(x, y)) then exit;
					inc(ofs);
					inc(x);
				end;
				inc(result);
				inc(y);
				ofs += _size.x - sizeX;
			end;
		end;

	var
		x, y, curSquare, bestSquare: uint;
		curRect, bestRect: UintRect;

	begin
		r := nil;

		y := 0;
		while y < _size.y do
		begin
			x := 0;
			while x < _size.x do
			begin
				if BitAt(x, y) and not Covered(UintVec2.Make(x, y)) then
				begin
					curRect := UintRect.Make(UintVec2.Make(x, y), UintVec2.Zero);
					bestSquare := 0;

					repeat
						inc(curRect.size.data[0]);
						curRect.size.y := GetSizeY(curRect.pos, curRect.size.x);
						if curRect.size.y = 0 then break;
						curSquare := curRect.Square;
						if curSquare > bestSquare then
						begin
							bestSquare := curSquare;
							bestRect := curRect;
						end;
					until (curRect.pos.x + curRect.size.x = _size.x) or not BitAt(curRect.pos.x + curRect.size.x, curRect.pos.y);

					Assert(bestSquare > 0);
					SetLength(r, length(r) + 1);
					r[High(r)] := bestRect;
				end;
				inc(x);
			end;
			inc(y);
		end;
	end;

	function Bitfield2D.Combine(const b: Bitfield2D; const ofs: UintVec2; cb: CombineRowCallback): boolean;
	var
		rowI, by, bRowI: uint;
	begin
		Assert((b._size + ofs).FitsClosed(_size), ToString(b._size) + ' -> ' + ToString(_size) + '@' + ToString(ofs));
		rowI := ofs.y * _size.x + ofs.x;
		bRowI := 0;
		by := 0;
		while by < b._size.y do
		begin
			if not cb(self.raw, b.raw, rowI, bRowI, b._size.x) then exit(no);
			rowI += _size.x;
			bRowI += b._size.x;
			inc(by);
		end;
		result := yes;
	end;

	procedure Blob.Init;
	begin
		_data := nil;
		_size := 0;
		_count := 0;
	end;

	procedure Blob.Done;
	begin
		Done(nil, nil);
	end;

	procedure Blob.Done(destruct: DestructorProc; getSize: GetSizeProc);
	var
		cur: pointer;
		sz: size_t;
	begin
		if Assigned(destruct) then
		begin
			cur := _data;
			while size_t(cur - _data) < _size do
			begin
				sz := getSize(cur^);
				destruct(cur^);
				cur += sz;
			end;
		end;
		FreeMem(_data);
	end;

	function Blob.Add(newSize: size_t): pointer;
	var
		ofs: size_t;
	begin
		inc(_count);
		ofs := _size;
		_size += newSize;
		ReallocMem(_data, _size);
		result := _data + ofs;
	end;

	function Blob.Next(var ptr: pointer; getSize: GetSizeProc): boolean;
	begin
		if not Assigned(ptr) then
		begin
			result := _size > 0;
			if result then ptr := _data;
		end else
		begin
			ptr += getSize(ptr^);
			result := size_t(ptr - _data) < _size;
		end;
	end;

	function Blob.Find(getSize: GetSizeProc; pred: Predicate; param: pointer): pointer;
	begin
		result := nil;
		while Next(result, getSize) do
			if pred(result^, param) then exit;
		result := nil;
	end;

{$define classname := HeterogenousQueue} {$include heterogenous_queue.pp.inc}
{$define classname := ThreadedHeterogenousQueue} {$include heterogenous_queue.pp.inc}

	procedure MultiDelegate.SingleInfo.Stop;
	begin
		md^.stopped := yes;
	end;

	procedure MultiDelegate.Init;
	begin
		System.Initialize(self);
		onEmptinessInversed := nil;
		userParam := nil;
	end;

	procedure MultiDelegate.Done;
	begin
		Clear;
		System.Finalize(self);
	end;

	procedure MultiDelegate.Clear;
	var
		i: sint;
	begin
		for i := 0 to High(list) do
			if Assigned(list[i].destruct) then list[i].destruct(list[i].info);
		list := nil;
	end;

	procedure MultiDelegate.Add(newProc: pointer; newParam: pObject; newDestruct: DestructProc; newPriority: sint = 0);
	var
		i, p: sint;
	begin
		Assert(Assigned(newProc), 'use Remove instead Add(nil)');
		for i := 0 to High(list) do
			if (list[i].info.proc = newProc) and (list[i].info.user = newParam) and (list[i].name.internal = '') then
			begin
			{$ifdef Debug} Log('MultiDelegate: повторное добавление (proc, param), не делай так', logWarning); {$endif}
				exit;
			end;

		p := FindPos(newPriority);
		SetLength(list, length(list) + 1);
		for i := High(list) downto p + 1 do
			list[i] := list[i - 1];

		list[p].info.md   := @self;
		list[p].info.proc := newProc;
		list[p].info.user := newParam;
		list[p].name     := '';
		list[p].priority := newPriority;
		list[p].destruct := newDestruct;

		if Assigned(onEmptinessInversed) and (length(list) = 1) then
			onEmptinessInversed(self, userParam);
	{$ifdef Debug} stat.Note(max_functions_in_delegate, length(list)); {$endif}
	end;

	procedure MultiDelegate.Add(newProc: pointer; newParam: pObject; newPriority: sint = 0);
	begin
		Add(newProc, newParam, nil, newPriority);
	end;

	function MultiDelegate.Remove(rmProc: pointer; rmParam: pObject): boolean;
	var
		i: sint;
	begin
		for i := 0 to High(list) do
			if (list[i].info.proc = rmProc) and (list[i].info.user = rmParam) and (list[i].name.internal = '') then
			begin
				RemoveAt(i);
				if Assigned(onEmptinessInversed) and (length(list) = 0) then
					onEmptinessInversed(self, userParam);
				exit(yes);
			end;
	{$ifdef Debug} Log('MultiDelegate: удаление несуществующего, так и задумано?', logWarning); {$endif}
		result := no;
	end;

	function MultiDelegate.FindPos(priority: sint): sint;
	var
		i: sint;
	begin
		for i := High(list) downto 0 do
			if priority > list[i].priority then
				exit(i + 1);
		result := 0;
	end;

	procedure MultiDelegate.RemoveAt(id: sint);
	var
		i: sint;
	begin
		if Assigned(list[id].destruct) then
			list[id].destruct(list[id].info);
		for i := id to High(list) - 1 do
			list[i] := list[i + 1];
		SetLength(list, length(list) - 1);
	end;

	function MultiDelegate.SetNamed(const newName: PoolString; newProc: pointer; newParam: pObject; newDestruct: DestructProc; newPriority: sint = 0): pSingleInfo;
	var
		i, j, p: sint;
		t: SingleRec;
	begin
		Assert(Assigned(newProc), 'use RemoveNamed instead SetNamed(name, nil)');
		for i := 0 to High(list) do
			if list[i].name = newName then
			begin
				if Assigned(list[i].destruct) then list[i].destruct(list[i].info);
				list[i].info.proc := newProc;
				list[i].info.user := newParam;
				list[i].destruct := newDestruct;
				if list[i].priority <> newPriority then
				begin
					list[i].priority := newPriority;
					p := FindPos(newPriority);
					if p <> i then
					begin
						t := list[i];
						if p > i then
							for j := i to p - 2 do
								list[j] := list[j + 1]
						else
							for j := i downto p + 1 do
								list[j] := list[j - 1];
						list[p] := t;
					end;
					result := @list[p].info;
				end else
					result := @list[i].info;
				exit;
			end;

		p := FindPos(newPriority);
		SetLength(list, length(list) + 1);
		for i := High(list) downto p + 1 do
			list[i] := list[i - 1];

		with list[p] do
		begin
			info.md   := @self;
			info.proc := newProc;
			info.user := newParam;
			name      := newName;
			priority  := newPriority;
			destruct  := newDestruct;
			result := @info;
		end;
		if Assigned(onEmptinessInversed) and (length(list) = 1) then
			onEmptinessInversed(self, userParam);
	{$ifdef Debug} stat.Note(max_functions_in_delegate, length(list)); {$endif}
	end;

	function MultiDelegate.FindNamed(const aName: PoolString): pSingleInfo;
	var
		i: sint;
	begin
		for i := 0 to High(list) do
			if list[i].name = aName then
				exit(@list[i].info);
		result := nil;
	end;

	function MultiDelegate.RemoveNamed(const rmName: PoolString): boolean;
	var
		i: sint;
	begin
		for i := 0 to High(list) do
			if list[i].name = rmName then
			begin
				RemoveAt(i);
				exit(yes);
			end;
	{$ifdef Debug} Log('MultiDelegate: удаление несуществующего ("' + rmName + '"), так и задумано?', logWarning); {$endif}
		result := no;
	end;

	function MultiDelegate.Count: sint;
	begin
		result := length(list);
	end;

	function MultiDelegate.Empty: boolean;
	begin
		result := length(list) = 0;
	end;

	procedure MultiDelegate.Call(caller: CallerProc; param: pointer);
	var
		i: sint;
	begin
		stopped := no;
		// TO-DO: элементы могут работать с этим же делегатом!!!
		// в простейшем случае — при удалении делегатом самого себя — достаточно и итерирования наоборот,
		// но стоит запилить более безопасный механизм.
		for i := High(list) downto 0 do
		begin
			caller(list[i].info, param);
			if stopped then break;
		end;
	end;

	procedure MultiDelegate.SetCallbacks(onEmptinessInversed: OnEmptinessInversedProc; param: pObject);
	begin
		self.onEmptinessInversed := onEmptinessInversed;
		self.userParam           := param;
	end;

	function ModifiableValue.FindModifier(const name: PoolString): sint;
	begin
		result := Index(name.ToIndex, pointer(modifiers) + fieldoffset ModifierRec _ name _, length(modifiers), sizeof(ModifierRec));
	end;

	procedure ModifiableValue.Recalculate;
	var
		i: sint;
		nv: float;
	begin
		nv := base;
		for i := 0 to High(modifiers) do
			with modifiers[i] do
				case op of
					op_Set:    nv := x;
					op_Add:    nv += x;
					op_Sub:    nv -= x;
					op_Mul:    nv *= x;
				{$ifdef Debug} else Assert(no); {$endif}
				end;
		if not Equals(v, nv) then
		begin
			v := nv;
			if Assigned(onChange) then onChange(self, onChangeParam);
		end;
	end;

	procedure ModifiableValue.Init(const newBase: float);
	begin
		base := newBase;
		v := newBase;
		onChange := nil;
		modifiers := nil;
	end;

	procedure ModifiableValue.Done;
	begin
		Invalidate;
	end;

	procedure ModifiableValue.Invalidate; begin onChangeParam := InvalidOnChangeParam; end;
	function ModifiableValue.Valid: boolean; begin result := onChangeParam <> InvalidOnChangeParam; end;

	procedure ModifiableValue.Serialize(stream: pStream);
	var
		i: sint;
	begin
		Serialize_ui8(stream,
			   uint(base <> 0)              shl HAS_BASE_BITN or
			   uint(length(modifiers) <> 0) shl HAS_MODIFIERS_BITN);

		if base <> 0.0 then Serialize_f16(stream, base);
		if length(modifiers) <> 0 then
		begin
			Serialize_ui8(stream, RangeCheck(length(modifiers), High(uint8), 'nModifiers') - 1);
			for i := 0 to High(modifiers) do
				with modifiers[i] do
				begin
					Serialize_string_xbits(stream, name, MODNAME_EXTRA_BITS, uint(op) or uint(priority <> 0) shl MODNAMEX_HAS_PRIORITY_BITN);
					Serialize_f16(stream, x);
					if priority <> 0 then VarInt.Write(stream, design(priority));
				end;
		end;
	end;

	procedure ModifiableValue.DeserializeInplace(stream: pStream);
	var
		flags, nameExtra: uint;
		i: sint;
	begin
		try
			flags := Deserialize_ui8(stream);
			if flags and (1 shl HAS_BASE_BITN) <> 0 then base := Deserialize_f16(stream);
			if flags and (1 shl HAS_MODIFIERS_BITN) <> 0 then
			begin
				SetLength(modifiers, 1 + Deserialize_ui8(stream));
				for i := 0 to High(modifiers) do
				begin
					modifiers[i].name := Deserialize_string_xbits(stream, MODNAME_EXTRA_BITS, nameExtra);
					modifiers[i].op := OpEnum(RangeCheck(nameExtra and OP_ENUM_MASK, High(ord(OpEnum)), 'ModifiableValue.op'));
					modifiers[i].x := Deserialize_f16(stream);
					if nameExtra and (1 shl MODNAMEX_HAS_PRIORITY_BITN) <> 0 then modifiers[i].priority := ensign(VarInt.Read(stream)) else modifiers[i].priority := 0;
				end;
			end;
			Recalculate;
		except
			Done;
			raise;
		end;
	end;

	procedure ModifiableValue.Deserialize(out v: ModifiableValue; stream: pStream);
	begin
		v.Init(0.0);
		v.DeserializeInplace(stream);
	end;

	procedure ModifiableValue.SetModifier(const name: PoolString; op: OpEnum; const x: float; priority: sint);
	var
		i, id: sint;
	begin
	trace_call('ModifiableValue.SetModifier');
		id := FindModifier(name);
		if id < 0 then
		begin
			id := 0;
			while (id < length(modifiers)) and (priority >= modifiers[id].priority) do
				inc(id);

			SetLength(modifiers, length(modifiers) + 1);
			for i := High(modifiers) downto id + 1 do
				modifiers[i] := modifiers[i - 1];
			modifiers[id].name := name;
			modifiers[id].op   := op;
			modifiers[id].x    := x;
			modifiers[id].priority := priority;
		{$ifdef DebugModifiableValues} Log('Добавлен модификатор "' + name + '" (всего ' + ToString(length(modifiers)) + ')', logDebug); {$endif}
			Recalculate;
		end else
		begin
			Assert(priority = modifiers[id].priority, 'Cannot change priority of existing modifier');
			Assert(op = modifiers[id].op, Format('Cannot change operation of existing modifier ({0} {1} {2} -> {3} {4} {5})',
			                                     modifiers[id].name, OpIds[modifiers[id].op], ToString(modifiers[id].x), name, OpIds[op], ToString(x)));
			if not Equals(x, modifiers[id].x) then
			begin
				modifiers[id].x := x;
				Recalculate;
			end;
		end;
	leave_call
	end;

	procedure ModifiableValue.RemoveModifier(const name: PoolString; expectExisting: boolean = yes);
	var
		i, id: sint;
	begin
		id := FindModifier(name);
		if id < 0 then
		begin
			if not expectExisting then {$ifdef Debug} Log('Модификатор "' + name + '" не найден', logWarning) {$endif};
			exit;
		end;

		for i := id to High(modifiers) - 1 do
			modifiers[i] := modifiers[i + 1];
		SetLength(modifiers, length(modifiers) - 1);
	{$ifdef Debug} Log('Модификатор "' + name + '" удалён', logDebug); {$endif}
		Recalculate;
	end;

	procedure ModifiableValue.SetChangeCallback(proc: OnChangeProc; param: pointer);
	begin
		Assert(param <> InvalidOnChangeParam);
		onChange := proc;
		onChangeParam := param;
	end;

	function ModifiableValue.Empty: boolean;
	begin
		result := (base = 0) and (length(modifiers) = 0);
	end;

	constructor DimensionalPath.Init(newDims: sint);
	begin
		Assert(newDims > 0);
		inherited Init;
		dims   := newDims;
		keys   := nil;
		values := nil;
		_len    := 0.0;
		flags  := [];
	end;

	destructor DimensionalPath.Done;
	begin
		inherited Done;
	end;

	function DimensionalPath.FindKey(const time: float): sint;
	var
		L, R: sint;
	begin
		if length(keys) = 0 then exit(-1);
		L := 0;
		R := High(keys);
		repeat
			result := L + (R - L + 1) div 2;
			if time < keys[result].time then
			begin
				dec(result);
				R := result;
				if L > R then break;
			end else
			begin
				L := result;
				if L = R then break;
			end;
		until no;
	end;

	function DimensionalPath.Interpolate(key, idx: sint; const x: float; method: InterpolationMode): float;
		function radjust(key: sint): pKey;
		begin
			if key > High(keys) then
				if Looped then
					repeat
						key -= length(keys);
					until key < length(keys)
				else
					key := High(keys);
			result := @keys[key];
		end;
		function ladjust(key: sint): pKey;
		begin
			if key < 0 then
				if Looped then
					repeat
						key += length(keys);
					until key >= 0
				else
					key := 0;
			result := @keys[key];
		end;
	var
		aa, a, b, bb: float;
	begin
		a := keys[key].v[idx];
		b := radjust(key + 1)^.v[idx];
		if method in [erp_BSpline, erp_CatmullRom] then
		begin
			aa := ladjust(key - 1)^.v[idx];
			bb := radjust(key + 2)^.v[idx];
		end;
		case method of
			erp_Linear: result := lerp(a, b, x);
			erp_Smooth: result := lerp(a, b, smoothstep(x));
			erp_CatmullRom: result := CatmullRomSpline(aa, a, b, bb, x);
			erp_BSpline: result := BSpline(aa, a, b, bb, x); // TODO: что-то не так... возможно, лерпать c началом и концом, если key = 0 | key + 1 = High(keys)
			else Assert(no);
		end;
	end;

	function DimensionalPath.GetFlag(flag: FlagEnum): boolean;
	begin
		result := flag in flags;
	end;

	procedure DimensionalPath.SetFlag(flag: FlagEnum; newValue: boolean);
	begin
		if newValue then Include(flags, flag) else Exclude(flags, flag);
	end;

	function DimensionalPath.StoreValues(const v: array of float): pFloat;
	var
		eq: boolean;
		i, j, start: sint;
		kshift: array of sint;
	begin
		for i := 0 to High(values) + 1 do
		begin
			eq := yes;
			for j := i to min(i + dims - 1, High(values)) do
				if not Equals(values[j], v[j - i]) then
				begin
					eq := no;
					break;
				end;
			if eq then
			begin
				if i + dims > length(values) then
				begin
					SetLength(kshift, length(keys));
					for j := 0 to High(kshift) do kshift[j] := keys[j].v - pFloat(values);
					start := length(values);
					SetLength(values, i + dims);
					for j := start to High(values) do
						values[j] := v[dims - (High(values) - j + 1)];
					for j := 0 to High(kshift) do keys[j].v := pFloat(values) + kshift[j];
				end;
				exit(@values[i]);
			end;
		end;
		Assert(no);
	end;

	procedure DimensionalPath.AddKey(const v: array of float; const newTime: float; erIn, erOut: InterpolationMode);
	var
		vp: pFloat;
		i, at: sint;
	begin
		Assert(dims = length(v));
		at := FindKey(newTime) + 1;
		vp := StoreValues(v);

		SetLength(keys, length(keys) + 1);
		for i := High(keys) downto at + 1 do
			keys[i] := keys[i - 1];
		with keys[at] do
		begin
			v       := vp;
			time    := newTime;
			erp_in  := erIn;
			erp_out := erOut;
		end;
		_len := max(_len, newTime);
	end;

	procedure DimensionalPath.AddAUA(const a, b, velA, velB: array of float; const start, time: float; const kTimeA, kTimeB: float);
	const
		Erp1 = erp_Linear;
		Erp2 = erp_CatmullRom;
	var
		n: array[0 .. 3] of array of float;
		k, timeA, timeU, timeB: float;
		distance, vA, vB, aA: float;
		x: hp_float;
		d, stage: sint;
	begin
		Assert((length(a) = dims) and (length(b) = dims));
		Assert((length(velA) <= 1) or (length(velA) = dims));
		Assert((length(velB) <= 1) or (length(velB) = dims));
		Assert((kTimeA >= 0.0) and (kTimeA <= 1.0) and (kTimeB >= 0.0) and (kTimeB <= 1.0) and LessThanEqual(kTimeA + kTimeB, 1.0));
		if kTimeA <> 0.0 then k := kTimeA else
			if kTimeB <> 0.0 then k := 1.0 - kTimeB else
				k := 0.5;
		timeA := k * time;
		if kTimeB <> 0.0 then k := kTimeB else
			if k > 0.5 then k := 1.0 - k; // else k := k
		timeB := k * time;
		timeU := time - (timeA + timeB);

		for stage := 0 to High(n) do
			SetLength(n[stage], dims);

		if length(velA) = 0 then vA := 0.0;
		if length(velB) = 0 then vB := 0.0;
		for d := 0 to dims - 1 do
		begin
			if length(velA) > 1 then vA := velA[d] else
				if length(velA) = 1 then vA := velA[0];
			if length(velB) > 1 then vB := velB[d] else
				if length(velB) = 1 then vB := velB[0];
			distance := b[d] - a[d];
			if solveLinear(0.5 * (timeA + timeB) + timeU, 0.5 * (vA * timeA + vB * timeB) - distance, x) then
			begin
				aA := (x - vA) / timeA;
				// aB := (vB - x) / timeB;
				n[0, d] := a[d];
				n[1, d] := n[0, d] + vA * timeA + 0.5 * aA * sqr(timeA);
				n[2, d] := n[1, d] + x * timeU;
				n[3, d] := b[d];
			end else
			begin
			{$ifdef Debug}
				Log('Уравнение не решилось: dist = ' + ToString(distance) + ', timeA = ' + ToString(timeA) + ', timeB =' +
					ToString(timeB) + ', timeU = ' + ToString(timeU), logWarning);
			{$endif}
				for stage := 0 to 3 do
					if stage = 0 then
						n[stage, d] := a[stage]
					else
						n[stage, d] := b[stage];
			end;
		end;
		AddKey(n[0], start, Erp1, Erp2);
		AddKey(n[1], start + timeA, Erp2, Erp2);
		if GreaterThan(timeU, 0.1 * time) then AddKey(n[2], start + timeA + timeU, Erp2, Erp2);
		AddKey(n[3], start + time, Erp2, Erp1);
	end;

{$define impl:=
	procedure DimensionalPath.AddKey(const v: ty; const newTime: float; erIn, erOut: InterpolationMode);
	begin
		AddKey(v.data, newTime, erIn, erOut);
	end;

	procedure DimensionalPath.AddAUA(const a, b, velA, velB: ty; const start, time: float; const kTimeA, kTimeB: float);
	begin
		AddAUA(a.data, b.data, velA.data, velB.data, start, time, kTimeA, kTimeB);
	end;}
{$define ty:=Vec2} impl
{$define ty:=Vec3} impl
{$define ty:=Vec4} impl
{$undef impl} {$undef ty}

	procedure DimensionalPath.AddKey(const v: Quaternion; const newTime: float; erIn, erOut: InterpolationMode);
	begin
		AddKey(v.v4.data, newTime, erIn, erOut);
	end;

	procedure DimensionalPath.AddAUA(const a, b, velA, velB: Quaternion; const start, time: float; const kTimeA, kTimeB: float);
	begin
		AddAUA(a.v4.data, b.v4.data, velA.v4.data, velB.v4.data, start, time, kTimeA, kTimeB);
	end;

	procedure DimensionalPath.Query(const time: float; v: pFloat);
	var
		t2: float;
		key, i, k2: sint;
		x, cv: float;
	begin
		if length(keys) = 0 then
		begin
			Assert(no);
			for i := 0 to dims - 1 do v[i] := 0.0;
			exit;
		end;
		Assert((not Looped) or (time <= _len));

		key := FindKey(time);
		Assert(key <= High(keys));

		if key < 0 then
		begin
			for i := 0 to dims - 1 do
				v[i] := keys[0].v[i];
			exit;
		end;

		if (key = High(keys)) and (not Looped) then
		begin
			for i := 0 to dims - 1 do
				v[i] := keys[key].v[i];
			exit;
		end;

		k2 := key + 1;
		if k2 >= length(keys) then
		begin
			t2 := _len;
			k2 := 0;
		end else
			t2 := keys[key + 1].time;
		x := (time - keys[key].time) / (t2 - keys[key].time);
		Assert(GreaterThanEqual(x, 0.0) and LessThanEqual(x, 1.0));

		for i := 0 to dims - 1 do
		begin
			cv := Interpolate(key, i, x, keys[key].erp_out);
			if keys[k2].erp_in <> keys[key].erp_out then
				cv := lerp(cv, Interpolate(key, i, x, keys[k2].erp_in), x);
			v[i] := cv;
		end;
	end;

	procedure DimensionalMove._Init(newDims: sint; newPath: pDimensionalPath);
	begin
		if Assigned(newPath) then
		begin
			Assert((newDims = 0) or (newDims = newPath^.Dimensions));
			_path := MakeRef(newPath);
		end else
			_path := MakeRef(new(pDimensionalPath, Init(newDims)));
		_done    := no;
		_time    := 0.0;
	end;

	procedure DimensionalMove._SetPath(newPath: pDimensionalPath);
	begin
		if Assigned(newPath) then SetRef(_path, newPath);
	end;

	procedure DimensionalMove.Init(newDims: sint);
	begin
		System.Initialize(self);
		_Init(newDims, nil);
	end;

	procedure DimensionalMove.Init(newPath: pDimensionalPath);
	begin
		System.Initialize(self);
		_Init(0, newPath);
	end;

	procedure DimensionalMove.Done;
	begin
		Release(_path);
		System.Finalize(self);
	end;

	procedure DimensionalMove.Process(const dt: float);
	begin
		Assert(not _done);
		_time += dt;
		if _time >= _path^.Len then
			if _path^.Looped then
			begin
				_time -= _path^.Len;
				if _time > _path^.Len then _time := modf(_time, _path^.Len);
			end else
			begin
				_time := _path^.Len;
				_done := yes;
			end;
	end;

	function DimensionalMove.CurrentF: float;
	begin
		Assert(_path^.Dimensions = 1);
		_path^.Query(_time, @result);
	end;

{$define impl:=
	begin
		Assert(_path^.Dimensions = length(result.data));
		_path^.Query(_time, @result.data[0]);
	end;}
	function DimensionalMove.CurrentV2: Vec2; impl
	function DimensionalMove.CurrentV3: Vec3; impl
	function DimensionalMove.CurrentV4: Vec4; impl
{$undef impl}

	function DimensionalMove.CurrentQ: Quaternion;
	begin
		result := Quaternion(CurrentV4).Normalized;
	end;

	function EntityAction._Conflicts(var ac: EntityAction): boolean;
	begin
		Assert(@ac = @ac);
		result := no;
	end;

	constructor EntityAction.Init;
	begin
		inherited Init;
		_stopped := no;
		onProcess.Init;
		onDone.Init;
	end;

	destructor EntityAction.Done;
	begin
		onProcess.Done;
		onDone.Done;
		inherited Done;
	end;

	procedure _CallOnProcess(const info: SingleDelegateInfo; param: pointer);
	var
		args: ^EntityAction.OnProcessArgs absolute param;
	begin
		args^.ret := EntityAction.OnProcessProc(info.proc)(args^.action, args^.dt^, info);
	end;

	procedure _CallOnDone(const info: SingleDelegateInfo; param: pointer);
	var
		args: ^EntityAction.OnDoneArgs absolute param;
	begin
		EntityAction.OnDoneProc(info.proc)(args^.action, args^.reason, info);
	end;

	procedure EntityAction.Process(entity: pObject; const dt: float);
	var
		args: OnProcessArgs;
	begin
		if _stopped then exit;

		_Process(entity, dt);
		if not onProcess.Empty then
		begin
			args.action := @self;
			args.dt     := @dt;
			onProcess.Call(@_CallOnProcess, @args);
			if not args.ret then Stop(reason_Abort);
		end;
	end;

	procedure EntityAction.Stop(reason: EntityActionStopReason);
	var
		args: OnDoneArgs;
	begin
		if _stopped then exit;
		_stopped := yes;
		if not onDone.Empty then
		begin
			args.action := @self;
			args.reason := reason;
			onDone.Call(@_CallOnDone, @args);
		end;
	end;

	procedure EntityActions.Init;
	begin
		list := nil;
	end;

	procedure EntityActions.Done;
	begin
		ReleaseArray(USystem.ObjectsList(list));
	end;

	function EntityActions.Add(entity: pObject; ac: pEntityAction): boolean;
	var
		i: sint;
		ok: boolean;
		t: pEntityAction;
	begin
		result := no;
		Assert(@entity = @entity);

	{$ifdef Debug} Assert(Index(ac, pointer(list), length(list)) < 0); {$endif}
		repeat
			ok := yes;
			for i := High(list) downto 0 do
				if ac^._Conflicts(list[i]^) then
				begin
				{$if defined(Debug) and defined(use_serialization)}
					Log('Удаляю конфликтующее действие ' + SerializationDB.Shared^.TypeName(TypeOf(list[i]^))
						+ ' в пользу ' + SerializationDB.Shared^.TypeName(TypeOf(list[i]^)), logDebug);
				{$endif}

					t := list[i];
					list[i] := list[High(list)];
					SetLength(list, length(list) - 1);
					t^.Stop(reason_Abort);
					Release(t);
					ok := no;
				end;
		until ok;
		SetLength(list, length(list) + 1);
		list[High(list)] := MakeRef(ac);
	{$ifdef Debug} stat.Note(max_entity_actions, length(list)); {$endif}
		result := yes;
	end;

	function EntityActions.Process(entity: pObject; const dt: float): boolean;
	label _finally_;
	var
		i, removed: sint;
	begin
		MakeRef(entity);
		if length(list) = 0 then
		begin
			result := no;
			goto _finally_;
		end;

		i := 0;
		while i < length(list) do
		begin
			list[i]^.Process(entity, dt);
			inc(i);
		end;

		removed := 0;
		for i := High(list) downto 0 do
			if list[i]^._stopped then
			begin
				Release(list[i]);
				list[i] := list[High(list) - removed];
				inc(removed);
			end;
		if removed > 0 then SetLength(list, length(list) - removed);

		result := length(list) > 0;
	_finally_:
		Release(entity);
	end;

	function EntityActions.Count: sint;
	begin
		result := length(list);
	end;

	function EntityActions.Empty: boolean;
	begin
		result := length(list) = 0;
	end;

	procedure Slide._Process(entity: pObject; const dt: float);
	begin
		Assert(@entity = @entity);
		dm.Process(dt);
		if dm.Finished then Stop(reason_Done);
	end;

	function Slide._Conflicts(var ac: EntityAction): boolean;
	begin
		result := inherited _Conflicts(ac) or ((TypeOf(ac) = TypeOf(self)) and (pSlide(@ac)^.id = id));
	end;

	constructor Slide.Init(path: pDimensionalPath; const newId: PoolString);
	begin
		inherited Init;
		if not Assigned(path) then Fail;
		dm.Init(path);
		id := newId;
	end;

	destructor Slide.Done;
	begin
		dm.Done;
		inherited Done;
	end;

	function Distribution.Constant(const value: float): Distribution;
	begin
		result := Create(dis_Constant, [value]);
	end;

	function Distribution.Uniform(const min, max: float): Distribution;
	begin
		result := Create(dis_Uniform, [min, max - min]);
	end;

	function Distribution.Bell(const min, avg, max: float): Distribution;
	begin
		result := Create(dis_Bell, [min, avg, max]);
	end;

	function Distribution.Copy: Distribution;
	var
		sz: size_t;
	begin
		result.kind := kind;
		result.static := static;
		if Assigned(dynamic) then
		begin
			sz := MemSize(dynamic);
			result.dynamic := GetMem(sz);
			memcpy(dynamic, result.dynamic, sz);
		end else
			result.dynamic := nil;
	{$ifdef Debug} result.nParams := nParams; {$endif}
	end;

	function Distribution.Equals(const a, b: Distribution): boolean;
	begin
		if a.kind <> b.kind then exit(no);

		case a.kind of
			dis_Constant: result := UMath.Equals(a.params[0], b.params[0]);
			dis_Uniform: result := UMath.Equals(a.params[0], b.params[0]) and UMath.Equals(a.params[1], b.params[1]);
			dis_Bell: result := UMath.Equals(a.params[0], b.params[0]) and UMath.Equals(a.params[1], b.params[1]) and UMath.Equals(a.params[2], b.params[2]);
		{$ifdef Debug} else raise ExhaustiveCase(ord(a.kind), 'Distribution.Equals'); {$endif}
		end;
	end;

	procedure Distribution.Done;
	begin
		FreeMem(dynamic);
	end;

	function Distribution.GenerateValue: float;
	begin
		case kind of
			dis_Constant: result := params[0];
			dis_Uniform: result := params[0] + GlobalRNG.GetFloat * params[1];
			dis_Bell: result := GlobalRNG.Bell(params[0], params[1], params[2]);
			else Assert(no);
		end;
	end;

	function Distribution.EstimateMaximum: float;
	begin
		case kind of
			dis_Constant: result := params[0];
			dis_Uniform: result := params[0] + params[1];
			dis_Bell: result := params[2];
			else Assert(no);
		end;
	end;

	function Distribution.GetParam(id: sint): float;
	begin
	{$ifdef Debug} Assert(id < nParams); {$endif}
		if id <= High(static) then result := static[id] else result := dynamic[id - length(static)];
	end;

	function Distribution.Create(newKind: KindEnum; const v: array of float): Distribution;
	const
		nStatic = length(result.static);
	var
		i: sint;
	begin
		result.kind := newKind;
	{$ifdef Debug} result.nParams := length(v); {$endif}
		if length(v) > nStatic then
			result.dynamic := GetMem((length(v) - nStatic) * sizeof(float))
		else
			result.dynamic := nil;

		for i := 0 to min(nStatic - 1, High(v)) do
			result.static[i] := v[i];
		for i := 0 to length(v) - nStatic - 1 do
			result.dynamic[i] := v[nStatic + i];
	end;

	procedure DelayedReleaseTimer(param: pointer; var instance: ThreadTimer.CallbackInstance);
	var
	{$ifdef DebugDelayedRelease} stream: string; {$endif}
		rp: pResourcePool;
		p: ResourcePool.pTimerParam absolute param;
		td: ResourcePool.pTypeDesc;
		obj: pObject;
	begin
	{$ifdef DebugDelayedRelease} stream := p^.key^.stream; {$endif}
		rp_dpr('Таймер: ' + stream);
		rp := p^.rp;
		rp^.timerLock.Enter;
		Assert(p^.cancel = not Assigned(p^.key^.releasing), YesNo[p^.cancel] + '/' + ToString(p^.key^.releasing));
		if p^.cancel then
		begin
			rp_dpr('Отмена: ' + stream);
			rp^.timerLock.Leave;
			exit;
		end;

		rp_dpr('Удаление: ' + stream);
		td := p^.key^.td;
		obj := p^.key^.obj;
		rp^.Unregister(p^.key);
		rp^.timerLock.Leave;

		if not p^.cancel then
		begin
			instance.Close;
			dispose(p);
		end;

		rp_dpr('Освобождение: ' + stream);
		td^.Release(obj, rp^); // может вызвать HandleZeroRefCount
		rp_dpr('Освобождён: ' + stream);
	end;

	procedure HandleZeroRefCount(obj: pObject; param: pointer);
	var
		rp: pResourcePool absolute param;
		k: ResourcePool.pResKey;
		t: ResourcePool.pTimerParam;
	begin
		rp^.timerLock.Enter;
		k := rp^.objs.Find(obj);
		Assert(Assigned(k), ToString(obj));
		Assert(k^.obj = obj, k^.stream);
		Assert(not Assigned(k^.releasing), k^.stream);

		MakeRef(obj);
		new(t); k^.releasing := t;
		t^.rp  := rp;
		t^.key := k;
		t^.cancel := no;
		ThreadTimer.Open(t^.timer, @DelayedReleaseTimer, t, k^.td^.timeout, 0, [ThreadTimer.NonCritical]);
		rp_dpr('Открыт таймер ' + k^.stream + ' на ' + ToString(k^.td^.timeout) + ' с');
		rp^.timerLock.Leave;
	end;

	function ResourcePool.TypeDesc.Create(ctr: LoadProc; flags: FileFlags; timeout: uint; tag: sint): pTypeDesc;
	begin
		new(result);
		result^.ctr     := ctr;
		result^.flags   := flags + [file_Read];
		result^.timeout := timeout;
		result^.tag     := tag;
	end;

	function ResourcePool.TypeDesc.ToFileFlags(justTry: boolean): FileFlags;
	begin
		result := flags;
		if justTry then result += [file_JustTry];
	end;

	procedure ResourcePool.TypeDesc.Release(obj: pObject; var rp: ResourcePool);
	begin
		if (tag >= 0) and Assigned(rp.tags[tag].userRelease) and Assigned(obj) then
			rp.tags[tag].userRelease(obj, rp.tags[tag].userParam)
		else
			USystem.Release(obj);
	end;

	procedure ResourcePool.ResKey.Init(const stream: string; typ: pointer; td: pTypeDesc; obj: pObject);
	begin
		self.stream := stream;
		self.typ    := typ;
		self.td     := td;
		self.obj    := obj;
		self.releasing := nil;
	end;
	{$define classname := ResourcePool.ResKey} {$define pSelf := pResKey} {$define no_done}
	{$define constructor_args := ResKey_constructor_args} {$define pass_constructor_args := stream, typ, td, obj}
	{$include dyn_obj.pp.inc}

	procedure ResourcePool.TagDesc.Init(const name: string);
	begin
		self.name   := name;
		active      := yes;
		userRelease := nil;
		userParam   := nil;
		objs.Init;
	end;

	procedure ResourcePool.TagDesc.Done;
	begin
		objs.Done;
	end;

	function ResourcePool.FindType(typ: pointer): pTypeDesc;
	begin
		result := types.Find(typ);
	end;

	function ResourcePool.TagIndex(const tag: string; allowCreate: boolean): sint;
	begin
		if (tag = '') and allowCreate then exit(-1);
		result := Index(tag, pointer(tags) + fieldoffset TagDesc _ name _, length(tags), sizeof(TagDesc));
		if result >= 0 then exit;
		if not allowCreate then raise Error('Тег {0} не найден.', tag);

		result := length(tags);
		SetLength(tags, result + 1);
		tags[result].Init(tag);
	end;

	procedure ResourcePool.FullLock;
	begin
		loadLock.Enter;
		timerLock.Enter;
	end;

	procedure ResourcePool.FullUnlock;
	begin
		timerLock.Leave;
		loadLock.Leave;
	end;

	procedure ResourcePool.CancelTimer(k: pResKey);
	var
		t: pTimerParam;
	{$ifdef DebugDelayedRelease} times: uint; {$endif}
	begin
		Assert(loadLock.AcquiredAssert and timerLock.AcquiredAssert);
		Assert(Assigned(k^.releasing));
		t := k^.releasing; k^.releasing := nil;
		Assert(not t^.cancel, k^.stream);
		t^.cancel := yes;
		timerLock.Leave;

		rp_dpr('Остановка таймера ' + k^.stream + '...');
	{$ifdef DebugDelayedRelease} times := {$endif} t^.timer.Close;
		rp_dpr('Таймер ' + k^.stream + ' остановлен (' + ToString(times) + ')');

		timerLock.Enter;
		dispose(t);
	end;

	procedure ResourcePool.Unregister(k: pResKey);
	begin
		if not objs.Remove(k^.obj) then Assert(no, k^.stream);
		if (k^.td^.tag >= 0) and not tags[k^.td^.tag].objs.Remove(k^.obj) then Assert(no, k^.stream);
		keys.Remove(k); // это освобождает k, так что в последнюю очередь
	end;

	procedure ResourcePool.UnregisterObjectSet(var objs: ObjectSet);
	var
		it: ObjectSet.Iterator;
		k: pResKey;
	begin
		while objs.AnyElement(it) do
		begin
			k := objs.GetKey(it)^;
			if Assigned(k^.releasing) then
			begin
				CancelTimer(k);
				k^.td^.Release(k^.obj, self);
			end else
				if Assigned(k^.obj) then
					k^.obj^.RemoveOnDestroyProc(@HandleZeroRefCount, @self);
			Unregister(k);
		end;
	end;

	procedure ResourcePool.Init;
	begin
		loadLock.Init; timerLock.Init;
		types.Init;
		keys.Init;
		objs.Init;
		ResetCallbacks;
	end;

	procedure ResourcePool.Done;
	var
		i: sint;
	begin
		FullLock;
		UnregisterObjectSet(objs);
		FullUnlock;

		for i := 0 to High(tags) do tags[i].Done;
		objs.Done;
		keys.Done;
		types.Done;
		timerLock.Done; loadLock.Done;
	end; {$define classname := ResourcePool} {$include dyn_obj.pp.inc}

{$define accessor := ResourcePool.Shared} {$define instance_type := pResourcePool}
{$define create_instance := ResourcePool.Create} {$define destroy_instance := instance^.Free(instance)} {$define unitname := 'Classes'}
{$include lazy_singleton.inc}

	function ResourcePool.Register(typ: pointer; ctr: LoadProc): pSelf;
	var
		t: pTypeDesc;
	begin
		result := @self;
		Assert(not Assigned(FindType(typ)), 'duplicate type!');
		t := TypeDesc.Create(ctr, [], DefaultTimeout, -1);
		types.Add(typ, t);
		lastRegistered := t;
	end;

	function ResourcePool.LoadRef(otyp: pointer; const stream: string; justTry: boolean = no): pointer;
		function FindKey(otyp: pointer; const stream: string): pResKey;
		var
			tk: ResKey;
		begin
			tk.typ    := otyp;
			tk.stream := stream;
			result    := keys.Find(@tk);
		end;
	var
		typ: pTypeDesc;
		k, k2: pResKey;
		s: pStream;
		startResult: pointer;
	begin
		FullLock;
		try
			typ := FindType(otyp);
			if not Assigned(typ) then
				raise Error('Тип {0} ({1}) неизвестен.', [StreamPath.Human(stream), otyp]);

			if (typ^.tag >= 0) and not tags[typ^.tag].active then
				raise Error('Тип {0} ({1}) деактивирован по тегу {2}.', [StreamPath.Human(stream), otyp, tags[typ^.tag].name]);

			k := FindKey(otyp, stream);
			if Assigned(k) then
			begin
				// Ресурс уже был загружен.
				result := k^.obj;

				// Если взведён таймер выгрузки: отменить таймер, перевыставить обработчик дропа счётчика ссылок до 0 и вернуть сам объект
				// (ссылка уже держалась, чтобы быть освобождённой в таймере)
				if Assigned(k^.releasing) then
				begin
					CancelTimer(k);
					pObject(result)^.AddOnDestroyProc(@HandleZeroRefCount, @self);
				end else
					// Если таймер выгрузки не работает — вернуть новую ссылку.
					MakeRef(result);
			end else
			begin
				// Это первая загрузка. Ресурсы могут быть вложенными, поэтому блокировку нужно отпустить.
				if Assigned(startLoad) then startResult := startLoad(stream, userParam) else startResult := nil;
				FullUnlock;

				result := nil;
				try
					case typ^.ctr.kind of
						LoadProcKind.Stream:
							begin
								s := GetStreamRef(stream, typ^.ToFileFlags(justTry));
								if not Assigned(s) then exit(nil);
								try
									result := MakeRef(typ^.ctr.stream(s));
								finally
									Release(s);
								end;
							end;
						LoadProcKind.&String:
							begin
								try
									result := MakeRef(typ^.ctr.&string(stream));
								except
									if justTry then {$ifdef Debug} Log(Exception.Message, logWarning) {$endif} else raise;
								end;
							end;
					{$ifdef Debug} else raise ExhaustiveCase(ord(typ^.ctr.kind), 'LoadProcKind'); {$endif}
					end;
				finally
					FullLock;

					k  := ResKey.Create(stream, otyp, typ, result);
					k2 := keys.Add(k)^;
					if k = k2 then
					begin
						// Всё в порядке, конфликтов не возникло. Регистрируем результат во вспомогательных структурах: objs и tag.objs,
						// и добавляем обработчик нуля ссылок.
						if Assigned(result) then
						begin
							rp_dpr(stream + ' загружен.');
							objs.Add(k, result);
							if typ^.tag >= 0 then tags[typ^.tag].objs.Add(k, result);
							pObject(result)^.AddOnDestroyProc(@HandleZeroRefCount, @self);
						end else
						begin
							rp_dpr(stream + ' запомнен как незагружающийся.');
						end;
					end else
					begin
						// Пока блокировка была отпущена, этот ресурс был загружен кем-то другим. Придётся тут же выгрузить, работа была напрасной.
						// TODO: сразу добавить ключ с пометкой и ждать, пока тот, кто начал, не загрузит.
						rp_dpr('Конфликт: ' + stream + ' загружен дважды.');
						Release(result);
						result := MakeRef(k2^.obj);
					end;
					if Assigned(endLoad) then endLoad(startResult, userParam);
				end;
			end;

			if not Assigned(result) and not justTry then raise Error('{0} не удалось загрузить ранее.', StreamPath.Human(stream));
		finally
			FullUnlock;
		end;
	end;

{$ifdef Debug}
	function ResourcePool.Dump(out count: uint): string;
	var
		it: KeySet.Iterator;
		k: pResKey;
	begin
		FullLock;
		count := keys.Count;
		result := '';
		it := keys.GetIterator;
		while keys.Next(it) do
		begin
			k := keys.GetKey(it)^;
			if result <> '' then result += EOL;
			result += k^.stream;
			if Assigned(k^.releasing) then result += ' (таймер)' else
				if Assigned(k^.obj) then result += ' (' + ToString(k^.obj^.ReferenceCount) + ')' else
					result += '(фейл)';
		end;
		FullUnlock;
	end;
{$endif}

	function ResourcePool.Registered(typ: pointer): boolean;
	begin
		result := Assigned(types.Find(typ));
	end;

	function ResourcePool.Loaded(obj: pObject; stream: pString): boolean;
	var
		key: pResKey;
	begin
		FullLock;
		key    := objs.Find(obj);
		result := Assigned(key);
		if result and Assigned(stream) then stream^ := key^.stream;
		FullUnlock;
	end;

	function ResourcePool.Timeout(timeout: uint): pSelf;
	begin
		result := @self;
		lastRegistered^.timeout := timeout;
	end;

	function ResourcePool.Tag(const tag: string): pSelf;
	begin
		result := @self;
		lastRegistered^.tag := TagIndex(tag, yes);
	end;

	function ResourcePool.Flags(flags: FileFlags): pSelf;
	begin
		result := @self;
		lastRegistered^.flags := flags + [file_Read];
	end;

	procedure ResourcePool.OverrideRelease(const tag: string; release: UserReleaseProc; param: pointer);
	var
		id: sint;
	begin
		id := TagIndex(tag, no);
		tags[id].userRelease := release;
		tags[id].userParam   := param;
	end;

	procedure ResourcePool.Deactivate(const tag: string);
	var
		tid: sint;
	begin
		FullLock;
		try
			tid              := TagIndex(tag, no);
			tags[tid].active := no;
			UnregisterObjectSet(tags[tid].objs);
		finally
			FullUnlock;
		end;
	end;

	procedure ResourcePool.SetCallbacks(startLoad: StartLoadProc; endLoad: EndLoadProc; param: pointer);
	begin
		FullLock;
		self.startLoad := startLoad;
		self.endLoad   := endLoad;
		self.userParam := param;
		FullUnlock;
	end;

	procedure ResourcePool.ResetCallbacks;
	begin
		SetCallbacks(nil, nil, nil);
	end;

	operator :=(f: ResourcePool.StreamLoadProc): ResourcePool.LoadProc; begin result.kind := ResourcePool.LoadProcKind.Stream; result.stream := f; end;
	operator :=(f: ResourcePool.StringLoadProc): ResourcePool.LoadProc; begin result.kind := ResourcePool.LoadProcKind.&String; result.&string := f; end;

	procedure StringTree.Init(const newData: string);
	begin
		data := newData;
		childs := nil;
	end;

	procedure StringTree.Done;
	var
		i: sint;
	begin
		for i := 0 to High(childs) do
			childs[i].Done;
	end;

	function StringTree.Add(const what: string): pStringTree;
	begin
		SetLength(childs, length(childs) + 1);
		result := @childs[High(childs)];
		result^.Init(what);
	end;

	function StringTree.Find(const what: string): pStringTree;
	var
		i: sint;
	begin
		for i := 0 to High(childs) do
			if childs[i].data = what then
				exit(@childs[i]);
		result := nil;
	end;

	function StringTree.Dump: string;
	const
		SoftLimit = 150;

		function _Small(var n: StringTree; const prefix: string): boolean;
		var
			i, len: sint;
		begin
			result := no;
			len := 0;
			for i := 0 to High(n.childs) do
			begin
				if length(n.childs[i].childs) > 0 then exit;
				len += length(n.childs[i].data);
				if len + length(prefix) > SoftLimit then exit;
			end;
			result := yes;
		end;

		procedure _Dump(var sb: StringBuilder; var n: StringTree; const aprefix: string);
		var
			i: sint;
			small: boolean;
			lineStart: sint;
			prefix: string;
		begin
			sb.Append(n.data, ': {');
			prefix := aprefix + '　　';
			small := _Small(n, prefix);
			if small then sb.Append(' ') else sb.Append(EOL, prefix);
			lineStart := sb.Len;

			for i := 0 to High(n.childs) do
			begin
				if (length(n.childs[i].childs) > 0) and (i > 0) then sb.Append(EOL, prefix);
				if length(n.childs[i].childs) > 0 then
				begin
					_Dump(sb, n.childs[i], prefix);
					lineStart := sb.Len;
				end else
				begin
					sb.Append(n.childs[i].data);
				end;

				if i < High(n.childs) then
				begin
					sb.Append(',');
					if (((length(n.childs[i].childs) > 0) and (length(n.childs[i + 1].childs) = 0)) or (sb.Len - lineStart + length(prefix) > SoftLimit))
						and not small then
					begin
						sb.Append(EOL, prefix);
						lineStart := sb.Len;
					end else
						sb.Append(' ');
				end;
			end;
			if (length(n.childs) > 0) then
				if small then sb.Append(' ') else sb.Append(EOL, aprefix);
			sb.Append('}');
		end;

	var
		sb: StringBuilder;
	begin
		sb.Init;
		_Dump(sb, self, '');
		result := sb.DestructiveToString;
	end;

	function StringTree.DestructiveDump: string;
	begin
		result := Dump;
		Done;
	end;

{$ifdef Debug}
	function FilesystemCache.Node.ToString: string;
	begin
		if file_Folder in what then result := 'папка' else
			if file_JustFile in what then result := 'файл' else
				result := 'симулякр';
	end;
{$endif}

	procedure FilesystemCache.Node.AddTag(const tag: PoolString);
	begin
		Assert(not TaggedWith(tag), 'тег ' + tag + ' уже задан');
		SetLength(tags, length(tags) + 1);
		tags[High(tags)] := tag;
	end;

	procedure FilesystemCache.Node.RemoveTag(const tag: PoolString);
	var
		i: sint;
	begin
		i := TagIndex(tag);
		Assert(i >= 0, 'тег ' + tag + ' не задан');
		tags[i] := tags[High(tags)];
		SetLength(tags, length(tags) - 1);
	end;

	function FilesystemCache.Node.TaggedWith(const tag: PoolString): boolean;
	begin
		result := TagIndex(tag) >= 0;
	end;

	function FilesystemCache.Node.TagIndex(const tag: PoolString): sint;
	begin
		result := Index(tag.ToIndex, pPoolString(tags), length(tags));
	end;

	procedure FilesystemCache.Init;
	begin
		lock.Init;
		SetLength(nodes, 1);
		nodes[0].name   := '';
		nodes[0].what   := [file_Folder];
		nodes[0].corporeal := yes;
		nodes[0].parent := -1;
		nodes[0].childs := nil;
		nodes[0].tags   := nil;
	end;

	procedure FilesystemCache.Done;
	begin
	{$ifdef DebugFsCache} Log(_ToStringTree.DestructiveDump, logDebug); {$endif}
		lock.Done;
	end; {$define classname := FilesystemCache} {$define pSelf := pFilesystemCache} {$include dyn_obj.pp.inc}

	function FilesystemCache.Query(parent: sint; const name: string; cb: QueryCallback = nil; param: pointer = nil): sint;
	begin
		result := Query(parent, pChar(name), length(name), cb, param);
	end;

	function FilesystemCache.Query(parent: sint; name: pChar; nameLen: sint; cb: QueryCallback = nil; param: pointer = nil): sint;
	label _finally_;
	var
		childs: ^ChildList;
		idx: sint;
		n: pNode;
		i: sint;
		what: FileAttributes;
	begin
	{$ifdef DebugFsCache} Log('FsCache.Query: ' + Copy(name, 0, nameLen), logDebug); {$endif}
		result := -1;
		if parent < 0 then exit;

		// forceFolder := no;
		for i := 0 to nameLen - 1 do
			if name[i] = FileSeparator then
				if i = nameLen - 1 then
				begin
					// forceFolder := yes;
					dec(nameLen);
				end else
				begin
					if i > 0 then
					begin
						parent := Query(parent, name, i);
						result := Query(parent, name + i + 1, nameLen - i - 1);
					end;
					exit;
				end;

		lock.Enter;
		if nodes[parent].corporeal.StrictNo then goto _finally_;
		childs := @nodes[parent].childs;

		for idx in childs^ do
			if StrEq(pChar(nodes[idx].name.internal), length(nodes[idx].name.internal), name, nameLen) then
			begin
				result := idx;
				goto _finally_;
			end;

		result := length(nodes);
		SetLength(childs^, length(childs^) + 1);
		childs^[High(childs^)] := result;

		SetLength(nodes, result + 1);
		n := @nodes[result];
		n^.name := USystem.ToString(name, nameLen);
		n^.parent := parent;
		n^.childs := nil;
		n^.what := [];
		n^.corporeal := Tribool.Unknown;
		lock.Leave;

		what := GetStreamAttributes(_Trace(result)); // Внимание! Может рекурсивно вызвать эту же Query. В частности, левая часть станет НЕ ВАЛИДНОЙ,
		                                             // если является ссылкой на nodes.

		lock.Enter;
		n := @nodes[result];
		n^.what := what;
		if n^.corporeal.Undefined then n^.corporeal := what * [file_Folder, file_JustFile] <> [];

	{$ifdef DebugFsCache} Log('FsCache ++ ' + StreamPath.Log(_Trace(result)) + ' (' + n^.ToString + ') (#' + ToString(result) + ')', logDebug); {$endif}
	_finally_:
		if result >= 0 then
		begin
			if Assigned(cb) then cb(nodes[result], result, param);
			if nodes[result].corporeal.StrictNo then result := -1;
		end;
		lock.Leave;
	end;

	function FilesystemCache.Trace(nid: sint): string;
	begin
		if nid < 0 then exit('');
		lock.Enter;
		result := _Trace(nid);
		lock.Leave;
	end;

	procedure FilesystemCache.TagWith(id: sint; const tag: string);
	begin
		lock.Enter;
		nodes[id].AddTag(tag);
		lock.Leave;
	end;

	function FilesystemCache.TaggedWith(id: sint; const tag: string): boolean;
	begin
		lock.Enter;
		result := nodes[id].TaggedWith(tag);
		lock.Leave;
	end;

	procedure FilesystemCache.ForceCorporeality(id: sint);
	begin
		lock.Enter;
		nodes[id].corporeal := yes;
		lock.Leave;
	end;

	function FilesystemCache._Trace(nid: sint): string;
	begin
		if nodes[nid].parent > 0 then result := _Trace(nodes[nid].parent) + FileSeparator else result := '';
		result += nodes[nid].name;
	end;

{$ifdef Debug}
	function FsNodeToString(id: uint; param: pointer): string;
	begin
		result := FilesystemCache.pNode(param)[id].name;
	end;

	function FilesystemCache._ToStringTree: StringTree;

		procedure Traverse(id: sint; var tree: StringTree);
		var
			name: string;
		begin
			for id in nodes[id].childs do
			begin
				name := nodes[id].name + SeparatedList.Join(length(nodes[id].tags), @FsNodeToString, pNode(nodes),
				                                            ', ' + SeparatedList.Prefix + ' {' + SeparatedList.Suffix + '}');
				if [file_Folder, file_JustFile] * nodes[id].what = [] then name := '[[' + name + ']]';
				Traverse(id, tree.Add(name)^);
			end;
		end;

	begin
		pStringTree(@result)^.Init('Дамп FsCache');
		Traverse(0, result);
	end;
{$endif}

	{$define accessor := FsCache} {$define instance_type := pFilesystemCache}
	{$define create_instance := FilesystemCache.Create} {$define destroy_instance := instance^.Free(instance)} {$define unitname := 'Classes'}
	{$include lazy_singleton.inc}

	function Dir4.CW: Dir4;      begin result._value := Enum((uint(value) + 1) mod EnumCount); end;
	function Dir4.CCW: Dir4;     begin result._value := Enum((uint(value) + 3) mod EnumCount); end;
	function Dir4.Reverse: Dir4; begin result._value := Enum((uint(value) + 2) mod EnumCount); end;
	function Dir4.Dx: sint;      begin result := Delta[value].x; end;
	function Dir4.Dy: sint;      begin result := Delta[value].y; end;
	operator :=(const value: Dir4.Enum): Dir4; begin result._value := value; end;
	operator =(const a, b: Dir4): boolean; begin result := a.value = b.value; end;

	function LoaderSuite.Proxy.Opts(const value: string): Proxy;        begin useOpts := value;        result := self; end;
	function LoaderSuite.Proxy.ForceLoader(const value: string): Proxy; begin useForceLoader := value; result := self; end;
	function LoaderSuite.Proxy.Size(const value: FileSize): Proxy;      begin useSize := value;        result := self; end;
	procedure LoaderSuite.Proxy.Load(obj: pointer; stream: pStream);    begin suite^.Load(obj, stream, self); end;
	procedure LoaderSuite.Proxy.Save(obj: pointer; stream: pStream);    begin suite^.Save(obj, stream, self); end;
	procedure LoaderSuite.Proxy.Save(obj: pointer; const stream: string); begin suite^.Save(obj, stream, self); end;

	function LoaderSuite.Register(const exts: string; const load: LoadProc): pLoaderSuite;
	const
		NoSave: SaveProc = (typ: SaveInterface.None; funcPtr: nil);
	begin
		result := Register(exts, load, NoSave);
	end;

	function LoaderSuite.Register(const exts: string; const load: LoadProc; const save: SaveProc): pLoaderSuite;
	var
		t: StringTokenizer;
		l: pLoaderDesc;
		ext: string;
	begin
		result := @self;
		t := exts;
		try
			repeat
				ext := t.ScanTokenEndingWith(['|']);
			{$ifdef Debug} if Assigned(Find(ext)) then raise Error('Загрузчик для {0} уже существует.', ext); {$endif}
				SetLength(loaders, length(loaders) + 1);
				l := @loaders[High(loaders)];
				l^.ext := ext;
				l^.load := load;
				l^.save := save;
				if t.Maybe('|') then else begin t.ExpectEnd; break; end;
			until no;
		finally
			t.Done;
		end;
	end;

	procedure LoaderSuite.Load(obj: pointer; stream: pStream); begin Load(obj, stream, CreateProxy); end;
	procedure LoaderSuite.Save(obj: pointer; stream: pStream); begin Save(obj, stream, CreateProxy); end;
	procedure LoaderSuite.Save(obj: pointer; const stream: string); begin Save(obj, stream, CreateProxy); end;

	function LoaderSuite.GetSaveInterface(const loader: string): SaveInterface;
	var
		l: pLoaderDesc;
	begin
		l := Find(loader);
		if not Assigned(l) then raise Error('Неизвестный формат: {0}.', loader);
		result := l^.save.typ;
	end;

	function DescribeLoader(id: uint; param: pointer): string;
	begin
		result := '".' + LoaderSuite.pLoaderDesc(param)[id].ext + '"';
	end;

	function LoaderSuite.Dump: string;
	begin
		result := SeparatedList.Join(length(loaders), @DescribeLoader, pLoaderDesc(loaders), ', ' + SeparatedList.Empty + '(нет)');
	end;

	function LoaderSuite.Opts(const value: string): Proxy;        begin result := CreateProxy.Opts(value); end;
	function LoaderSuite.ForceLoader(const value: string): Proxy; begin result := CreateProxy.ForceLoader(value); end;
	function LoaderSuite.Size(const value: FileSize): Proxy;      begin result := CreateProxy.Size(value); end;

	function LoaderSuite.CreateProxy: Proxy;
	begin
	FPC_3_BUG System.Initialize(result);
		result.suite := @self;
		result.useOpts := '';
		result.useForceLoader := '';
		result.useSize := FileSize.Not0;
	end;

	procedure LoaderSuite.Load(obj: pointer; stream: pStream; const extra: Proxy);
	var
		l: pLoaderDesc;
		sz: FileSize;
	begin
		try
			l := Find(stream^.path, extra.useForceLoader, no);
			case l^.load.typ of
				LoadInterface.Plain:
					begin
						if (0 <> not extra.useSize.value) and (extra.useSize <> stream^.Size - stream^.Position) then
							raise Error('SizedLoad({0}): размер не может быть проигнорирован ({1} <> {2}).',
								StreamPath.Human(stream^.path), ToString(extra.useSize), ToString(stream^.Size - stream^.Position));
						l^.load.plain(obj, stream);
					end;
				LoadInterface.Sized:
					begin
						if 0 <> not extra.useSize.value then
							sz := extra.useSize
						else
							sz := stream^.Size - stream^.Position;
						l^.load.sized(obj, stream, sz);
					end;
			{$ifdef Debug} else ExhaustiveCase(ord(l^.load.typ), l^.ext + '.LoadInterface'); {$endif}
			end;
		finally
			FPC_3_BUG System.Finalize(extra);
		end;
	end;

	procedure LoaderSuite.Save(obj: pointer; stream: pStream; const extra: Proxy);
	var
		ldr: pLoaderDesc;
	begin
		try
			ldr := Find(stream^.path, extra.useForceLoader, yes);
			case ldr^.save.typ of
				SaveInterface.Plain:
					begin
						if extra.useOpts <> '' then raise Error('Опции не могут быть проигнорированы ({0}).', extra.useOpts);
						ldr^.save.plain(obj, stream);
					end;
				SaveInterface.StringOpts:
					begin
						ldr^.save.stringOpts(obj, stream, extra.useOpts);
					end;
			end;
		finally
			FPC_3_BUG System.Finalize(extra);
		end;
	end;

	procedure LoaderSuite.Save(obj: pointer; const stream: string; const extra: Proxy);
	var
		s: pStream;
	begin
		// Так ошибка неизвестного формата не удалит исходный файл.
		// Хотя вообще есть смысл сохранять во временный файл и заменять существующий последним действием, конечно.
		try
			Find(stream, extra.useForceLoader, yes);
		except
			FPC_3_BUG System.Finalize(extra);
			raise;
		end;

		s := GetStreamRef(stream, [file_Write]);
		try2
			Save(obj, s, extra);
		finally2
			Release(s);
		except2
			&File.Erase(stream);
			raise;
		end;
	end;

	function LoaderSuite.Find(const ext: string): pLoaderDesc;
	var
		index: sint;
	begin
		index := USystem.Index(ext, pointer(pLoaderDesc(loaders)) + fieldoffset LoaderDesc _ ext _, length(loaders), sizeof(LoaderDesc));
		if index >= 0 then result := @loaders[index] else result := nil;
	end;

	function LoaderSuite.Find(const spath, forced: string; isSave: boolean): pLoaderDesc;
	var
		ext: string;
	begin
		if forced <> '' then ext := forced else ext := StreamPath.Extension(spath);
		if ext = '' then raise Error('{0}: формат не определён.', StreamPath.Human(spath));
		result := Find(ext);
		if not Assigned(result) then raise Error('{0}: неизвестный формат файла ({1}).', StreamPath.Human(spath), ext);
		if isSave and (result^.save.typ = SaveInterface.None) then
			raise Error('{0}: сохранение {1} не поддерживается.', StreamPath.Human(spath), ext);
	end;

	operator :=(f: LoaderSuite.PlainLoad): LoaderSuite.LoadProc; begin result.typ := LoaderSuite.LoadInterface.Plain; result.plain := f; end;
	operator :=(f: LoaderSuite.SizedLoad): LoaderSuite.LoadProc; begin result.typ := LoaderSuite.LoadInterface.Sized; result.sized := f; end;
	operator :=(f: LoaderSuite.PlainSave): LoaderSuite.SaveProc; begin result.typ := LoaderSuite.SaveInterface.Plain; result.plain := f; end;
	operator :=(f: LoaderSuite.StringOptsSave): LoaderSuite.SaveProc; begin result.typ := LoaderSuite.SaveInterface.StringOpts; result.stringOpts := f; end;

{$ifdef use_serialization}
const
	MD_ITEM_HAS_USER_BIT = 1 shl 0;
	MD_ITEM_HAS_NAME_BIT = 1 shl 1;
	MD_ITEM_HAS_PRIORITY_BIT = 1 shl 2;
	MD_ITEM_HAS_DESTRUCT_BIT = 1 shl 3;

	procedure SerializeMultiDelegate(se: pSerializer; obj: pointer);
	var
		md: pMultiDelegate absolute obj;
		one: MultiDelegate.pSingleRec;
		flags: uint;
		i: sint;
	begin
		with se^ do
		begin
			VarInt.Write(stream, length(md^.list));
			for i := 0 to High(md^.list) do
			begin
				flags := 0;
				one := @md^.list[i];
				if Assigned(one^.info.user) then flags := flags or MD_ITEM_HAS_USER_BIT;
				if one^.name.internal <> '' then flags := flags or MD_ITEM_HAS_NAME_BIT;
				if one^.priority <> 0 then flags := flags or MD_ITEM_HAS_PRIORITY_BIT;
				if Assigned(one^.destruct) then flags := flags or MD_ITEM_HAS_DESTRUCT_BIT;
				Serialize_ui8(stream, flags);
				SeFunction(one^.info.proc);
				if (flags and MD_ITEM_HAS_USER_BIT) <> 0 then SeObject(one^.info.user);
				if (flags and MD_ITEM_HAS_NAME_BIT) <> 0 then Serialize_string(stream, one^.name);
				if (flags and MD_ITEM_HAS_PRIORITY_BIT) <> 0 then VarInt.Write(stream, design(one^.priority));
				if (flags and MD_ITEM_HAS_DESTRUCT_BIT) <> 0 then SeFunction(one^.destruct);
			end;
		end;
	end;

	procedure DeserializeMultiDelegate(de: pDeserializer; obj: pointer);
	var
		md: pMultiDelegate absolute obj;
		one: MultiDelegate.pSingleRec;
		i: sint;
		flags: uint;
	begin
		with de^ do
		begin
			SetLength(md^.list, VarInt.Read(stream));
			for i := 0 to High(md^.list) do
			begin
				one := @md^.list[i];
				flags := Deserialize_ui8(stream);
				one^.info.proc := DeFunction();
				if (flags and MD_ITEM_HAS_USER_BIT) <> 0 then DeWeakA(one^.info.user) else one^.info.user := nil;
				if (flags and MD_ITEM_HAS_NAME_BIT) <> 0 then one^.name := Deserialize_string(stream) else one^.name := '';
				if (flags and MD_ITEM_HAS_PRIORITY_BIT) <> 0 then one^.priority := ensign(VarInt.Read(stream)) else one^.priority := 0;
				if (flags and MD_ITEM_HAS_DESTRUCT_BIT) <> 0 then one^.destruct := MultiDelegate.DestructProc(DeFunction()) else one^.destruct := nil;
			end;
		end;
	end;

	procedure MultiDelegateDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		md: pMultiDelegate absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: md^.Init;
		end;
	end;

	procedure SerializeModifiableValue(se: pSerializer; obj: pointer);
	var
		v: pModifiableValue absolute obj;
	begin
		v^.Serialize(se^.stream);
	end;

	procedure DeserializeModifiableValue(de: pDeserializer; obj: pointer);
	var
		v: pModifiableValue absolute obj;
	begin
		v^.DeserializeInplace(de^.stream);
	end;

	procedure ModifiableValueDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		v: pModifiableValue absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: v^.Init(0.0);
		end;
	end;

const
	DIPATH_DIM_BITS   = 3;
	DIPATH_LOOPED_BIT = 1 shl (DIPATH_DIM_BITS + 0);

	procedure SerializeDimensionalPath(se: pSerializer; obj: pointer);
	var
		path: pDimensionalPath absolute obj;
		flags: uint;
		i: sint;
		vmin, vmax: float;
	begin
		vmin := path^.values[0];
		vmax := path^.values[0];
		for i := 1 to High(path^.values) do
		begin
			vmin := min(vmin, path^.values[i]);
			vmax := max(vmax, path^.values[i]);
		end;

		with se^ do
		begin
			Assert(path^.dims and (1 shl DIPATH_DIM_BITS - 1) = path^.dims);
			flags := uint(path^.dims);
			if path^.Looped then flags := flags or DIPATH_LOOPED_BIT;

			Serialize_ui8(stream, flags);
			Serialize_f32(stream, path^._len);
			VarInt.Write(stream, length(path^.values));
			Serialize_f32(stream, vmin);
			Serialize_f32(stream, vmax);
			for i := 0 to High(path^.values) do
				Serialize_fN16(stream, path^.values[i], vmin, vmax);

			VarInt.Write(stream, length(path^.keys));
			for i := 0 to High(path^.keys) do
			begin
				VarInt.Write(stream, path^.keys[i].v - pFloat(path^.values));
				Serialize_fN16(stream, path^.keys[i].time, 0.0, path^.len);
				Serialize_ui8(stream, (ord(path^.keys[i].erp_in) shl 4) or ord(path^.keys[i].erp_out));
			end;
		end;
	end;

	procedure DeserializeDimensionalPath(de: pDeserializer; obj: pointer);
	var
		path: pDimensionalPath absolute obj;
		flags: uint;
		i: sint;
		vmin, vmax: float;
		t: uint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			path^.dims := flags and (1 shl DIPATH_DIM_BITS - 1);
			path^.Looped := (flags and DIPATH_LOOPED_BIT) <> 0;

			path^._len := Deserialize_f32(stream);
			SetLength(path^.values, VarInt.Read(stream));
			vmin := Deserialize_f32(stream);
			vmax := Deserialize_f32(stream);
			for i := 0 to High(path^.values) do
				path^.values[i] := Deserialize_fN16(stream, vmin, vmax);

			SetLength(path^.keys, VarInt.Read(stream));
			for i := 0 to High(path^.keys) do
			begin
				path^.keys[i].v := pFloat(path^.values) + VarInt.Read(stream);
				if path^.keys[i].v + path^.dims - 1 > @path^.values[High(path^.values)] then
					raise Error('Прочитанный указатель на координаты точки неверен.');
				path^.keys[i].time := Deserialize_fN16(stream, 0.0, path^._len);
				t := Deserialize_ui8(stream);
				path^.keys[i].erp_in  := InterpolationMode(t shr 4);
				path^.keys[i].erp_out := InterpolationMode(t and (1 shl 4 - 1));
			end;
		end;
	end;

	procedure DimensionalPathDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		path: pDimensionalPath absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: path^.DeseInit;
		end;
	end;

const
	DIMOVE_STARTED_BIT = 1 shl 0;
	DIMOVE_DONE_BIT    = 1 shl 1;

	procedure SerializeDimensionalMove(se: pSerializer; obj: pointer);
	var
		dm: pDimensionalMove absolute obj;
		flags: uint;
	begin
		with se^ do
		begin
			flags := 0;
			if dm^._done then flags := flags or DIMOVE_DONE_BIT;
			if NotZero(dm^._time) then flags := flags or DIMOVE_STARTED_BIT;
			Serialize_ui8(stream, flags);
			SeObject(dm^._path);
			if NotZero(dm^._time) then Serialize_f32(stream, dm^._time);
		end;
	end;

	procedure DeserializeDimensionalMove(de: pDeserializer; obj: pointer);
	var
		dm: pDimensionalMove absolute obj;
		flags: uint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			dm^._done := (flags and DIMOVE_DONE_BIT) <> 0;
			DeObjectR(dm^._path);
			if (flags and DIMOVE_STARTED_BIT) <> 0 then dm^._time := Deserialize_f32(stream) else dm^._time := 0.0;
		end;
	end;

	procedure DimensionalMoveDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		dm: pDimensionalMove absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: System.Initialize(dm^);
		end;
	end;

	const
	ACTION_STOPPED_BIT       = 1 shl 0;
	ACTION_HAS_ONPROCESS_BIT = 1 shl 1;
	ACTION_HAS_ONDONE_BIT    = 1 shl 2;

	procedure SerializeEntityAction(se: pSerializer; obj: pointer);
	var
		ac: pEntityAction absolute obj;
		flags: uint;
	begin
		with se^ do
		begin
			flags := 0;
			if ac^._stopped then flags := flags or ACTION_STOPPED_BIT;
			if not ac^.onProcess.Empty then flags := flags or ACTION_HAS_ONPROCESS_BIT;
			if not ac^.onDone.Empty then flags := flags or ACTION_HAS_ONDONE_BIT;
			Serialize_ui8(stream, flags);
			if not ac^.onProcess.Empty then SeObject(@ac^.onProcess, ObjType_MultiDelegate);
			if not ac^.onDone.Empty then SeObject(@ac^.onDone, ObjType_MultiDelegate);
		end;
	end;

	procedure DeserializeEntityAction(de: pDeserializer; obj: pointer);
	var
		ac: pEntityAction absolute obj;
		flags: uint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			ac^._stopped := (flags and ACTION_STOPPED_BIT) <> 0;
			if (flags and ACTION_HAS_ONPROCESS_BIT) <> 0 then DeWeakAtR(ac^.onProcess) else ac^.onProcess.Init;
			if (flags and ACTION_HAS_ONDONE_BIT) <> 0 then DeWeakAtR(ac^.onDone) else ac^.onDone.Init;
		end;
	end;

	procedure SerializeEntityActions(se: pSerializer; obj: pointer);
	var
		a: pEntityActions absolute obj;
		i: sint;
	begin
		with se^ do
		begin
			VarInt.Write(stream, a^.Count);
			for i := 0 to High(a^.list) do
				SeObject(a^.list[i]);
		end;
	end;

	procedure DeserializeEntityActions(de: pDeserializer; obj: pointer);
	var
		a: pEntityActions absolute obj;
		i: sint;
	begin
		with de^ do
		begin
			SetLength(a^.list, VarInt.Read(stream));
			for i := 0 to High(a^.list) do
				DeObjectA(a^.list[i]);
		end;
	end;

	procedure EntityActionsDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		a: pEntityActions absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: a^.Init;
		end;
	end;

	procedure SerializeSlide(se: pSerializer; obj: pointer);
	var
		slide: pSlide absolute obj;
	begin
		with se^ do
		begin
			Serialize_string(stream, slide^.id);
			SeObject(@slide^.dm, ObjType_DimensionalMove);
		end;
	end;

	procedure DeserializeSlide(de: pDeserializer; obj: pointer);
	var
		slide: pSlide absolute obj;
	begin
		with de^ do
		begin
			slide^.id := Deserialize_string(stream);
			DeObjectAtR(slide^.dm);
		end;
	end;

	procedure SlideDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		slide: pSlide absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: slide^.DeseInit;
		end;
	end;

	procedure SerializeDistribution(se: pSerializer; obj: pointer);
	var
		dis: pDistribution absolute obj;
		i: sint;
	begin
		with se^ do
		begin
			Serialize_enum(stream, ord(dis^.kind), Distribution.KindPrefixCodes);
			for i := 0 to Distribution.KindInfo[dis^.kind].nParams - 1 do
				Serialize_f16(stream, dis^.params[i]);
		end;
	end;

	procedure DeserializeDistribution(de: pDeserializer; obj: pointer);
	var
		dis: pDistribution absolute obj;
		p: array of float;
		i: sint;
	begin
		with de^ do
		begin
			dis^.kind := Distribution.KindEnum(Deserialize_enum(stream, Distribution.KindPrefixCodes));
			SetLength(p, Distribution.KindInfo[dis^.kind].nParams);
			for i := 0 to High(p) do
				p[i] := Deserialize_f16(stream);
			dis^ := Distribution.Create(dis^.kind, p);
		end;
	end;
{$endif use_serialization}

{$ifdef selftest}
	function ParseBitfieldCase(const input: string): string;
	var
		b: Bitfield2D;
	begin
		b.Init(input);
		result := b.Dump('....', '##');
		b.Done;
	end;

	procedure Test;
	begin
		TestSuite.Start
		.Feature('ParseBitfield', @ParseBitfieldCase)
		.&Case('	##..' + EOL + '	####' + EOL + '	####', '##....' + EOL + '####' + EOL + '####')
		.&Case('[dont-trim]' + EOL + EOL + '		##' + Carriage + EOL + '				####', '............' + EOL + '##........' + EOL + '....####')
		.Done;
	end;
{$endif}

	procedure Init;
	begin
		internedPool.Init;

	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Multi delegate', ObjType_MultiDelegate, nil, sizeof(MultiDelegate), no,
			@SerializeMultiDelegate, @DeserializeMultiDelegate, nil, @MultiDelegateDeSpecial)
		^.RegisterType('Modifiable value', ObjType_ModifiableValue, nil, sizeof(ModifiableValue), no,
			@SerializeModifiableValue, @DeserializeModifiableValue, nil, @ModifiableValueDeSpecial)
		^.RegisterType('Dimensional path', TypeOf(DimensionalPath), nil, sizeof(DimensionalPath), yes,
			@SerializeDimensionalPath, @DeserializeDimensionalPath, nil, @DimensionalPathDeSpecial)
		^.RegisterType('Dimensional move', ObjType_DimensionalMove, nil, sizeof(DimensionalMove), no,
			@SerializeDimensionalMove, @DeserializeDimensionalMove, nil, @DimensionalMoveDeSpecial)
		^.RegisterType('Entity action', TypeOf(EntityAction), nil, sizeof(EntityAction), yes,
			@SerializeEntityAction, @DeserializeEntityAction, nil, nil)
		^.RegisterType('Entity actions', ObjType_EntityActions, nil, sizeof(EntityActions), no,
			@SerializeEntityActions, @DeserializeEntityActions, nil, @EntityActionsDeSpecial)
		^.RegisterType('Slide', TypeOf(Slide), TypeOf(EntityAction), sizeof(Slide), yes,
			@SerializeSlide, @DeserializeSlide, nil, @SlideDeSpecial)
		^.RegisterType('Distribution', Distribution.TypeOf, nil, sizeof(Distribution), no,
			@SerializeDistribution, @DeserializeDistribution, nil, nil);
	{$endif}
	end;

	procedure Done;
	begin
		internedPool.Done;
	end;

initialization
	&Unit('Classes').Initialize(@Init, @Done) {$ifdef selftest}.Test(@Test) {$endif} .Priority(+2);
end.
