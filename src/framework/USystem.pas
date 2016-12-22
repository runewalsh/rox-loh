unit USystem;

{$include opts.inc}
{$ifdef Debug}
	{$define NotifySpuriousWakeups}
	{-$define DebugAsyncIO}
	{-$define DisableSRW}
	{-$define DisableCV}
	{-$define DisableVistaTP}
	{-$define DebugFibers}
{$endif}
{$include all_numbers.inc}

interface

uses
	signals, ctypes {$ifdef Debug}, SysUtils {$endif} {$ifdef Windows}, Windows {$endif};

type
	Paths = object
	const
		Platform = {$if defined(cpu32)} 'x86'
		           {$elseif defined(cpu64)} 'x64'
		           {$else} {$error unknown platform} {$endif};

		DllExt   = {$if defined(Windows)} 'dll'
		           {$else} {$error unknown platform} {$endif};

		Libs        = 'lib/';
		MiscLibs    = Libs + 'etc/';
		Data        = 'data/';
		function DLL(const name: string): string; static;
		function Logs: string; static;
		function Cache: string; static;

	private const
		LogsFolder  = 'logs/';
		CacheFolder = 'cache/';

		function TempBase: string; static;
	end;

type
	pBoolean = ^boolean;
	sbyte  = type shortint; pSbyte  = ^sbyte;
	sint8  = type shortint; pSint8  = ^sint8;
	uint8  = type byte;     pUint8  = ^uint8;
	sint16 = type smallint; pSint16 = ^sint16;
	uint16 = type word;     pUint16 = ^uint16;
	sint32 = type longint;  pSint32 = ^sint32;
	uint32 = type longword; pUint32 = ^uint32;
	sint64 = type int64;    pSint64 = ^sint64;
	uint64 = type qword;    pUint64 = ^uint64;
	sint   = type sint32;   pSint   = ^sint;
	uint   = type uint32;   pUint = ^uint;
	ilong  = type sint64;   pIlong = ^ilong;
	ulong  = type uint64;   pUlong = ^ulong;
	integer = record end;   pInteger = record end;
	size_t   = type SizeUint; pSize_t = ^size_t;

	float16 = record
	case byte of
		0: (u: uint16);
	end;                        pFloat16 = ^float16;
{$if sizeof(float16) <> sizeof(uint16)} {$error что-то не так...} {$endif}
	float32  = type single;     pFloat32 = ^float32;
	float64  = type double;     pFloat64 = ^float64;
	float80  = type extended;   pFloat80 = ^float80;

	float = type {$if sizeof(pointer) > sizeof(float32)} float64 {$else} float32 {$endif}; // можно менять
	pFloat = ^float;

	hp_float = type {$ifdef Debug} float80 {$else} float64 {$endif}; // -"-
	pHp_float = ^hp_float;

const
	yes = true;
	no  = false;
	NULL = pointer(0); // для преобразований между целыми и указателями через NULL + <число> или <указатель> - NULL

	CloseToZeroEps = float(
		{$if sizeof(float) >= 16} 3e-31
		{$elseif sizeof(float) >= 10} 3e-19
		{$elseif sizeof(float) >= 8} 3e-15
		{$elseif sizeof(float) >= 4} 3e-7
		{$else} {$error can't determine epsilon} {$endif});

	Infinity    =  1.0 / 0.0;
	NegInfinity = -1.0 / 0.0;
	NaN         =  0.0 / 0.0;

	operator :=(const x: float32): float16;
	operator :=(const x: float16): float32;

type
	FloatIs = class
	  	class function NaN(const x: float32): boolean;
		class function Finite(const x: float32): boolean;
		class function NaN(const x: float64): boolean;
		class function Finite(const x: float64): boolean;
	end;

const
	FileSeparator = '/';
	ExtensionSeparator = '.';
	ExeExtension = {$if defined(Windows)} '.exe'
	               {$else} {$error ExeExtension undefined} {$endif};

	EOL       = #10;
	Carriage  = #13;
	Backspace = #8;
	TabSym    = #9;

type
	char = ansichar; pChar = ^char; ppChar = ^pChar;
	charset_t = set of char;
	pString = ^string;
	Strings = array of string;
	widestring = unicodestring;
	UTFchar = type uint32; pUTFchar = ^UTFchar;
	Exception = class;

const
	UTFInvalid = High(UTFchar);

type
	UTF8 = object
	const
		MaxCharLen = 4;
		BOM = #$EF#$BB#$BF;
	type
		CharBytes = type string[MaxCharLen];
		Statistics = record
			n: array[1 .. MaxCharLen] of uint;
		end;
	var
		function Validate(const s: string): boolean; static;
		function Validate(s: pChar; len: size_t): boolean; static;
		function GetStatistics(const s: string): Statistics; static;
		function Codepoints(s: pChar; len: size_t): uint; static;
		function Codepoints(const s: string): uint; static;
		function Next(const s: string; var pos: sint): UTFchar; static;
		function Next(const s: string; var pos: sint; out sym: UTFchar): UTFchar; static; cinline
		function Next(var s: pChar; var cb: size_t): UTFchar; static;
		function Prev(const s: string; var pos: sint): UTFchar; static;
		function Prev(const s: string; var pos: sint; out sym: UTFchar): UTFchar; static; cinline
		function Peek(const s: string; pos: sint): UTFchar; static; cinline
		function Peek(const s: string; pos: sint; out len: size_t): UTFchar; static; cinline
		function Peek(const s: string; pos: sint; out sym: UTFchar): UTFchar; static; cinline
		function Peek(const s: string; pos: sint; out len: size_t; out sym: UTFchar): UTFchar; static; cinline
		function CodepointToString(const char: UTFchar; syms: pChar): sint; static;
		function CodepointToString(const char: UTFchar): CharBytes; static; cinline
		function IsEOL(s: pChar; len: size_t; out eolen: size_t): boolean; static;
	private
		function ToStringError(const char: UTFchar; const what: string): Exception; static;
	end;

	UTF16 = object
	const
		MaxCharLen = 2;
	var
		function CodepointToString(const char: UTFchar; syms: pWideChar): sint;
	end;

{$define intf := function ToString(const value: typ): string;} all_ints
	function ToString(buf: pChar; count: uint): string;
	function ToString(buf: pWideChar; count: uint): widestring;
	function StringRefCount(const s: string): PtrInt;
	function UTF8Encode(const s: widestring): rawbytestring;
	function Prefixed(const prefix, s: string): boolean;
	function Prefixed(const prefix, s: string; start: sint): boolean;
	function Prefixed(const prefix: string; ps: pChar; ns: sint): boolean;
	function Prefixed(pp: pChar; np: sint; ps: pChar; ns: sint): boolean;
	function CutPrefix(const prefix, s: string; rest: pString): boolean;
	function CutPrefix(const prefix, s: string): string;
	procedure ContinueString(var text: string; const value, sep: string);
	function Continued(const text, sep, value: string): string;
	procedure Append(var strs: Strings; const s: string);
	procedure Append(var strs: Strings; const s: array of string);

type
	pMemoryChunk = ^MemoryChunk;
	MemoryChunk = object
	type
		scoped_enum_ Operation = (GetData, Destroy, QueryWrite); _end
		HandlerProc = function(op: Operation; param: pointer): pointer;
	var
		param: pointer;
		size: size_t;
		handler: HandlerProc;
		function Make(param: pointer; size: size_t; handler: HandlerProc): MemoryChunk; static;
		function Plain(data: pointer; size: size_t): MemoryChunk; static;
		function TakeIt(data: pointer; size: size_t): MemoryChunk; static;
		function data: pointer;
		procedure Done;
		function OK: boolean;
		function AsPlain: MemoryChunk;
		procedure RequestWrite;
	const
		Empty: MemoryChunk = (param: nil; size: 0; handler: nil);
		GetData = Operation.GetData;
		Destroy = Operation.Destroy;
		QueryWrite = Operation.QueryWrite;
	end;
	operator :=(const s: string): MemoryChunk;

type
	Ticks = object
	type
		ValueType = sint64;
	public
		function Get: Ticks; static;
		function Elapsed: Ticks;
		function FromSeconds(const sec: hp_float): Ticks; static;
		function FromMilliseconds(ms: uint): Ticks; static;
		function ToSeconds: hp_float;
		function ToMilliseconds: hp_float;
		function ToMicroseconds: hp_float;
		function ToNanoseconds: hp_float;
		function ToIMilliseconds: uint;
		function ToIMicroseconds: uint;
		function ToINanoseconds: uint;
		function InternalFrequency: ValueType; static;
		function Overhead(count: uint): Ticks; static;
	private
		value: ValueType;
		frequency, msecIFrequency: ValueType; static;
		secFFrequency, toSec, toMsec, toMcsec, toNsec: hp_float; static;
		CalculatedMulOverhead: ValueType {= 0}; static;
		CalculatedMul: uint {= 0}; static;
		procedure Initialize; static;
		procedure CalculateOverhead(out mulOverhead: ValueType; out mul: uint); static;
	public
		property Internal: ValueType read value;
	public const
		Zero: Ticks = (value: 0);
	end;
	operator +(const a, b: Ticks): Ticks; cinline
	operator -(const a, b: Ticks): Ticks; cinline
	operator /(const a: Ticks; b: uint): hp_float;
	{$define typ := Ticks} {$include comparison.h.inc}

type
	pThread = ^Thread;
	Thread = object
	type
		ID = System.TThreadID;

		Body = object
		type
			scoped_enum_ Signature = (Parametrized, Unparametrized); _end
			ParametrizedSignature = procedure(param: pointer);
			UnparametrizedSignature = procedure;
		var
			proc: record
			case sig: Signature of
				Signature.Parametrized: (para: ParametrizedSignature);
				Signature.Unparametrized: (unpara: UnparametrizedSignature);
			end;

			procedure Run(param: pointer);
			procedure ShowFatal(const where: string);
		end;

		HumanFormat = (FullOf, BracketedInfo);
	var
		nameOf: string;
		function Invalid: Thread; static;
		function OK: boolean;
		procedure Start(const nameOf: string; out thrd: Thread; const proc: Body; param: pointer); static;
		procedure Close;
		function Running: boolean;
		procedure Join;
		procedure Sleep(const ms: uint); static;
		procedure Sleep(const time: Ticks); static;
		function Current: ID; static; cinline
		function Human(format: HumanFormat): string;
		procedure TerminateSelf; static;
	private const
		DefaultStackSize = 256 * 1024;
	var
		handle: {$ifdef Windows} Windows.HANDLE {$else} {$error Thread.handle: ???} {$endif};
		stackSize: size_t;
		proc: Body;
		param: pointer;
	{$ifdef Debug} guard: pointer; {$endif}
	{$if defined(Debug) and defined(Windows)} sysId: ID; {$endif}
	end;
	operator :=(const proc: Thread.Body.ParametrizedSignature): Thread.Body;
	operator :=(const proc: Thread.Body.UnparametrizedSignature): Thread.Body;

type
	casint_t = longint;

	pRecursiveThreadLock = ^RecursiveThreadLock;
	RecursiveThreadLock = object
		procedure Init;
		procedure Done; {$define pSelf := pRecursiveThreadLock} {$include dyn_obj.h.inc}
		procedure Invalidate;
		function Valid: boolean;
		function TryEnter: boolean; cinline
		procedure Enter; cinline
		procedure Leave; cinline
		function TryEnterShared: boolean; cinline
		procedure EnterShared; cinline
		procedure LeaveShared; cinline
		function AcquiredAssert: boolean;
	private
		cs: TRTLCriticalSection;
	{$ifdef Debug} guard: pointer; {$endif}
	const
		IMPOSSIBLE_FOOTPRINT = pointer($FEFEFEFE);
	end;
{$if sizeof(RecursiveThreadLock) < sizeof(pointer)} {$error IMPOSSIBLE_FOOTPRINT won't fit in RecursiveThreadLock} {$endif}

	pThreadLock = ^ThreadLock;
	ThreadLock = object
		procedure Init;
		procedure Done; {$define pSelf := pThreadLock} {$include dyn_obj.h.inc}
		procedure Invalidate;
		function Valid: boolean;
		function TryEnter: boolean;
		procedure Enter;
		procedure Leave;
		function TryEnterShared: boolean;
		procedure EnterShared;
		procedure LeaveShared;
		function AcquiredAssert: boolean;
	private
		ptr: pointer; // Если поддерживаются SRW (WinSRWSupported) — это SRW. Иначе это динамически выделенная RecursiveThreadLock.
	{$ifdef Debug} guard: pointer; owner: Thread.ID; {$endif}
	const
		IMPOSSIBLE_PTR = pointer($FEFEFEFE);
	end;

	ThreadLockReference = object
	private
		nonrec: pThreadLock;
		rec: pRecursiveThreadLock;
		function Make(newNonrec: pThreadLock; newRec: pRecursiveThreadLock): ThreadLockReference; static;
		procedure Enter; cinline
		procedure Leave; cinline
		function AcquiredAssert: boolean;
	end;
	operator :=(var lock: ThreadLock): ThreadLockReference;
	operator :=(var lock: RecursiveThreadLock): ThreadLockReference;

type
	pThreadEvent = ^ThreadEvent;
	ThreadEvent = object
	type
		InitFlag = (AutoReset, InitiallySet, AllowMultiWait);
		InitFlags = set of InitFlag;
		WaitFlag = (OrMessage);
		WaitFlags = set of WaitFlag;
	var
		procedure Init(flags: InitFlags = []);
		procedure Done; {$define pSelf := pThreadEvent} {$define constructor_args := flags: InitFlags = []} {$include dyn_obj.h.inc}
		procedure SetEvent;
		procedure ResetEvent;
		procedure Wait; cinline
		function Wait(timeoutMs: uint): boolean;
		function WaitAny(const ev: array of pThreadEvent; flags: WaitFlags = []): sint; static;
		function WaitAny(const ev: array of pThreadEvent; timeoutMs: uint; flags: WaitFlags = []): sint; static;
		function State: boolean;
	private
	{$ifdef Windows}
		union: record
		case uint of
			0: (handle: Windows.HANDLE);
			1: (cvep: pointer);
		end;
	{$endif}
		multiwaitable: boolean;
	end;

	pThreadCV = ^ThreadCV;
	ThreadCV = object
	type
		Predicate = function(param: pointer): boolean;
	var
		procedure Init;
		procedure Done; {$define pSelf := pThreadCV} {$include dyn_obj.h.inc}
		procedure StartWait; cinline
		procedure EndWait; cinline
		procedure Wait(const lock: ThreadLockReference);
		function Wait(const lock: ThreadLockReference; timeoutMs: uint): boolean;
		procedure Wait(const lock: ThreadLockReference; condition: Predicate; param: pointer);
		procedure WakeOne;
		procedure WakeAll;
	private
		ptr: pointer;
	{$ifdef NotifySpuriousWakeups}
	type
		SpuriousRec = record
			thrd: Thread.ID;
			really: uint;
		end;
	var
		spurious: array of SpuriousRec;
		spuriLock: ThreadLock;
		procedure NotifySpuriousWakeups;
		function SpuriousIndex(thrd: Thread.ID): sint;
	{$endif}
	end;

	PendingSync = object
	private
		fin: pThreadCV;
		pending: casint_t;
	public
		lock: ThreadLock;
		procedure Init;
		procedure Done;
		function AddOne: sint;
		function KillOne: sint;
	end;

	pThreadPool = ^ThreadPool;
	pThreadTimer = ^ThreadTimer;
	ThreadTimer = object
	type
		scoped_enum_ Flag = (TinyWork, HardWork, NonCritical); _end
		FlagSet = set of Flag;

		pCallbackInstance = ^CallbackInstance;
		CallbackInstance = object
			procedure Close;
			function Reset(due, period: uint): uint;
		private
			timer: pThreadTimer;
		end;

		AdvancedCallback = procedure(param: pointer; var instance: CallbackInstance);
		UnparametrizedAdvancedCallback = procedure(var instance: CallbackInstance);

		scoped_enum_ CallbackKind = (Unparametrized, Simple, Advanced, UnparametrizedAdvanced); _end
		Callback = record
		case kind: CallbackKind of
			CallbackKind.Unparametrized: (unparametrized: Thread.Body.UnparametrizedSignature);
			        CallbackKind.Simple: (simple: Thread.Body.ParametrizedSignature);
			      CallbackKind.Advanced: (advanced: AdvancedCallback);
			CallbackKind.UnparametrizedAdvanced: (unparametrizedAdvanced: UnparametrizedAdvancedCallback);
		end;
	var
		procedure Invalidate;
		function Valid: boolean;
		procedure Open(out timer: ThreadTimer; const proc: Callback; param: pointer; due, period: uint; flags: FlagSet = []; tp: pThreadPool = nil); static;
		function Close: uint;
		function Reset(due, period: uint): uint;
		function SelfReset(due, period: uint): uint;
	private
		tp: pThreadPool;
	{$ifdef Windows}
		wh: record
		case uint of
			0: (xpHandle: Windows.HANDLE);
			1: (tp_timer: pointer); // PTP_TIMER
		end;
	{$endif}
		lock: ThreadLock;
		proc: Callback;
		param: pointer;
		times: uint;
		flags: FlagSet;
		period: uint;
		procedure InternalClose(wait: boolean);
		procedure CompleteClosing;

	public const
		TinyWork    = Flag.TinyWork;
		HardWork    = Flag.HardWork;
		NonCritical = Flag.NonCritical;
	end;

	pTask = ^Task;
	Task = object
	type
		scoped_enum_ Flag = (HardWork, _Dynamic, _WillWait); _end
		FlagSet = set of Flag;
	var
		procedure Close;

	private
		tp: pThreadPool;
		proc: Thread.Body;
		param: pointer;
		flags: FlagSet;
		xc: Exception;
	{$ifdef Debug} guard: pointer; {$endif}

	{$ifdef Windows}
		// finished создаётся только когда а) используется XP-пул (там нет API ожидания) -И- б) вызывающий получил экземпляр таска
		// (а получать он, в свою очередь, должен только таски с выставленным флагом WillWait).
		// tp_work — тогда и только тогда, когда используется Vista-пул.
		ex: record
		case uint of
			0: (finished: pThreadEvent);
			1: (tp_work: pointer);
		end;
	{$endif}
		procedure Run;
		procedure InternalClose;

	public const
		HardWork = Flag.HardWork;
	end;

	ThreadPool = object
		procedure Queue(const proc: Thread.Body; param: pointer; flags: Task.FlagSet = []);
		procedure Queue(out task: Task; const proc: Thread.Body; param: pointer; flags: Task.FlagSet = []);
		procedure Queue(out task: pTask; const proc: Thread.Body; param: pointer; flags: Task.FlagSet = []);
	private
		sync: PendingSync;

	{$ifdef Windows} xpTimerQueue: Windows.HANDLE; {$endif}
		procedure Init;
		procedure Done;
		procedure PlaceTask;
		procedure RunTask;
		procedure EndTask;
		procedure TrustedQueue(out task: Task; const proc: Thread.Body; param: pointer; flags: Task.FlagSet);
	end;
	operator :=(proc: Thread.Body.UnparametrizedSignature): ThreadTimer.Callback;
	operator :=(proc: Thread.Body.ParametrizedSignature): ThreadTimer.Callback;
	operator :=(proc: ThreadTimer.AdvancedCallback): ThreadTimer.Callback;
	operator :=(proc: ThreadTimer.UnparametrizedAdvancedCallback): ThreadTimer.Callback;

var
	Work: ThreadPool;

	procedure GetCPULoad(out full, kernel, user: float);

type
	pFiber = ^Fiber;
	Fiber = object
	type
		ResumeResult = (FiberFinished);
		ResumeResults = set of ResumeResult;
	var
		finished: boolean;
		function Split: pFiber; static;
		function Create(const newNameOf: string; const proc: Thread.Body; param: pointer): pFiber; static;
		function Ref: pFiber;
		procedure Done;
		function Resume: ResumeResults;
		procedure Yield;
	type
		HumanFormat = Thread.HumanFormat;
	var
		function Human(format: HumanFormat): string;
	{$ifdef Debug} function GlobalDump: string; static; {$endif}
	private
		h: pointer;
		passControl, passFinal: pFiber;
		refCount: sint;
	{$ifdef Debug} id: sint; {$endif}
		proc: Thread.Body;
		param: pointer;
		nameOf: string;
		function CreateInstance(const newNameOf: string): pFiber; static;
		function NonrefSplit: pFiber; static;
		procedure Switch(forever: boolean);
		procedure Delete;
		function FromThread: pointer; static;
		procedure ToThread;
	const
		StackSize = 64 * 1024;
	end;

type
	FileSize = object
	type
		ValueType = uint64;
	var
		value: ValueType;
		function Explicit(const v: ValueType): FileSize; static; cinline
		function AsSizeT: size_t; cinline
	const
		Zero: FileSize = (value: 0);
		Not0: FileSize = (value: not ValueType(0));
	end;
	FilePos = FileSize;
	{$define typ := FileSize} {$include comparison.h.inc}
	operator +(const p: FileSize; const delta: size_t): FileSize; operator +(const p, delta: FileSize): FileSize;
	operator -(const p: FileSize; const delta: size_t): FileSize; operator -(const p, delta: FileSize): FileSize;
	operator /(const a, b: FileSize): hp_float;

type
	FileFlag = (file_Read, file_Write, file_New, file_Existing, file_RandomAccess, file_SequentialAccess, file_JustTry, file_Sync, file_Temp,
		file_ShareReadWrite);
	FileFlags = set of FileFlag;

type
	pFileRef    = ^FileRef;
	pMappingRef = ^MappingRef;

	MappingRef = object
		f: pFileRef;
		handle: {$ifdef Windows} Windows.HANDLE {$else} {$error MappingRef.Handle: ???} {$endif};
		src: pointer;
		srcPos: FilePos;
		srcSize: size_t;
		srcFlags: FileFlags;
		refcount: casint_t;
		function Create(af: pFileRef): pMappingRef; static;
		procedure KillOne;
	end;

	pMapping = ^Mapping;
	Mapping = object
	private
		ref: pMappingRef;
		_data: pointer;
	{$ifdef Debug} guard: pointer; {$endif}
	public
		function Invalid: Mapping; static;
		function OK: boolean;
		procedure Close;
		property Data: pointer read _data;
	end;

	FileAttribute = (file_JustFile, file_Folder);
	FileAttributes = set of FileAttribute; pFileAttributes = ^FileAttributes;

	AsioStatus = object
	type
		scoped_enum_ Fundamental = (Completed, Aborted, Failed); _end
	var
		what: Fundamental;
		error: Exception;
		transferred: size_t;
		function Completed: boolean;
		function Aborted: boolean;
		function Failed: boolean;
	private
		function Create(what: Fundamental; error: Exception; transferred: size_t): AsioStatus; static;
		function Create(transferred: size_t): AsioStatus; static;
		function CreateAborted(transferred: size_t): AsioStatus; static;
		function Create(error: Exception; transferred: size_t = 0): AsioStatus; static;
		procedure Done;
	end;

	pFileHandle = ^FileHandle;
	FileHandle = object
	{$ifdef Windows}
		handle: Windows.HANDLE;
		tp_io: pointer; // PTP_IO
	{$endif}
	{$ifdef Debug} fn: string; {$endif}
		procedure Init({$ifdef Debug} const fn: string; {$endif} {$ifdef Windows} handle: Windows.HANDLE; {$endif} async: boolean);
		procedure Close(fromCompletionCallback: boolean);
	end;

	FileRef = object(FileHandle)
		async: boolean;
		apos: FilePos;
		refcount: casint_t;
		lock: ThreadLock;
		mmaps: array of pMappingRef;
		function Create({$ifdef Debug} const fn: string; {$endif} {$ifdef Windows} handle: Windows.HANDLE; {$endif} async: boolean): pFileRef; static;
		function Ref: pFileRef;
		procedure KillOne(fromCompletionCallback: boolean);
		function NoteSyncIo(const n: size_t): FilePos;
		function TryReuseMmap(const apos: FilePos; asize: size_t; flags: FileFlags): pMappingRef;
	const
		MmapAffectingFlags = [file_Read, file_Write];
	end;

	pFile = ^&File;
	&File = object
	private
		ref: pFileRef;
		function _GetSize: FileSize;
		procedure _SetSize(const newSize: FileSize);
	public
		function Invalid: &File; static;
		function Open(const filename: string; flags: FileFlags): &File; static;
		function OK: boolean;
		procedure Close;
		function Duplicate: &File;
		// TODO: Read(n, callback)
		function Read(buf: pointer; n: size_t): size_t;
		function Read(at: FilePos; buf: pointer; n: size_t): size_t;
		function Write(buf: pointer; n: size_t): size_t;
		function Write(const s: string): uint;
		function Write(at: FilePos; buf: pointer; n: size_t): size_t;
		function Write(const at: FilePos; const s: string): uint;
		function Seek(const pos: FilePos): FilePos;
		property Size: FileSize read _GetSize write _SetSize;
		function Mmap(out m: Mapping; pos: FilePos; msize: size_t; flags: FileFlags): boolean;
		function Copy(var from, &to: &File; n: FileSize): FileSize; static;

	type
		TempMode = (CreateAndOpenTemp, CreateAndOpenSharedTemp, OnlyCreate);
		function CreateTemp(const base: string; out fn: string; mode: TempMode = CreateAndOpenTemp): &File; static;

	type
		ProgressCallbackResult = (ContinueOperation, CancelOperation);
		ProgressCallback = function(const done, total: FileSize; param: pointer): ProgressCallbackResult;
		OpFlag = (AllowReplace, ToSameLocation, Throw);
		OpFlags = set of OpFlag;
	var
		function Exists(const filename: string): boolean; static;
		function Erase(const filename: string): boolean; static;
		function MoveCopy(move: boolean; const from, &to: string; flags: OpFlags; progress: ProgressCallback = nil; param: pointer = nil): boolean; static;
		function Move(const from, &to: string; flags: OpFlags; progress: ProgressCallback = nil; param: pointer = nil): boolean; static;
		function Copy(const from, &to: string; flags: OpFlags; progress: ProgressCallback = nil; param: pointer = nil): boolean; static;
		function Rename(const fullFrom, toName: string; flags: OpFlags = []): string; static;
	const
		TempBufferSize = size_t(1 * 1024 * 1024);
	end;

	Folder = object
	type
		Enumerator = class;
		WhatToSearch = (Anything, OnlyFiles, OnlyFolders);
	var
		function Exists(const folder: string): boolean; static;
		function EraseOne(const folder: string): boolean; static;
		function EraseTree(const folder: string): boolean; static;
		function Scan(const folder: string; const mask: string; filter: WhatToSearch = Anything): Enumerator; static;
		function Scan(const folder: string; filter: WhatToSearch = Anything): Enumerator; static;
		function Create(const folder: string; firstCreated: pString = nil): boolean; static;
		function AppendSeparator(const folder: string): string; static;
		function RemoveSeparator(const folder: string): string; static;
		function Path(const fn: string; stripSeparator: boolean): string; static;
		function Path(const fn: string): string; static; // a/b/c -> a/b/
		function Parent(const fn: string): string; static; // a/b/c -> a/b
		function Filename(const fn: string): string; static;
		function Extension(const fn: string): string; static;
		function FilenameNoExt(const fn: string): string; static;
		function Temp: string; static;
		function Working: string; static;

	type
		Enumerator = class
			name: string;
			what: FileAttributes;
			function IsFolder: boolean;
			function IsFile: boolean;
			function SearchedName: string;
			function Size: FileSize;

			destructor Destroy; override;
			function GetEnumerator: Enumerator;
			function GetCurrent: Enumerator;
			function MoveNext: boolean;
			property Current: Enumerator read GetCurrent;
		private
			folder, mask: string;
			filter: WhatToSearch;
		{$ifdef Windows}
			handle: Windows.HANDLE;
			data: WIN32_FIND_DATAW;
		{$endif}
		end;
	end;
	FoundFile = Folder.Enumerator;

	pFileWatch = ^FileWatch;
	FileWatch = object
	type
		Callback = procedure(const fn: string; param: pointer);
	// fnOrDir = 'a/b/c' следит за одним файлом (или одной папкой, без содержимого) a/b/c.
	// fnOrDir = 'a/b/c/' следит за содержимым папки a/b/c — нерекурсивно.
	var
		procedure Open(out w: FileWatch; const fnOrDir: string; cb: Callback; param: pointer); static;
		procedure Close;
		function Invalid: FileWatch; static;
		function OK: boolean;

	private type
		pReference = ^Reference;
		Reference = object
			cb: Callback;
			param: pointer;
			fnOrDir: string;
			removed: boolean;
			idInFolder, folderIdInRegistry: sint;
		{$ifdef Windows} wfn: widestring; {$endif} // если слежение за всей папкой, wfn = ''.
		end;

		FolderLockEnum = (FolderNotLocked, FolderLockedYetNothingRemoved, FolderLockedAndHasRemovedEntries, FolderRemoved);

		pFolderRec = ^FolderRec;
		FolderRec = object
			locked: FolderLockEnum;
			dir: string;
			watches: array of pReference;
			f: FileHandle;
			aw: pointer; // pAsyncWrite
			id: sint;
			procedure PostReadChangesRequest;
			function RemoveWatch(w: pReference; wi: sint): sint;
		end;

		pFolderRegistry = ^FolderRegistry;
		FolderRegistry = object
			lock: RecursiveThreadLock;
			folders: array of pFolderRec;
			procedure Init;
			procedure Done; {$define pSelf := pFolderRegistry} {$include dyn_obj.h.inc}
			procedure Add(w: FileWatch.pReference);
			procedure RemoveFolder(f: pFolderRec; fi: sint; fromCompletionCallback: boolean);
		end;

		function Registry: pFolderRegistry; static;
	var
		ref: pReference;
	end;
	function GetFileAttributes(const filename: string): FileAttributes;
	function ToSystemFileName(const fileName: string): string;
	function FromSystemFileName(const fileName: string): string;
	function ToShortSystemFileName(const fileName: string): string;

type
	FileVersionEnum = (FileVersion, CodeName);

	function GetFileVersion(const fn: string; what: FileVersionEnum = FileVersion): string;
	function GetExecVersion(what: FileVersionEnum = FileVersion): string;

const
	DefaultLanguage = 'ru';
	function GetSystemLanguage(const fallback: string): string;

type
	Process = object
	public type
		OpenFlag = (Silent);
		OpenFlags = set of OpenFlag;
	private
		handle: {$ifdef Windows} Windows.HANDLE {$else} {$error Process.handle: ???} {$endif};
		mainThread: {$ifdef Windows} Windows.HANDLE {$else} {$error Process.mainThread: ???} {$endif};
	public
		procedure Open(out process: Process; const exe: string; const args: array of string; flags: OpenFlags = []); static;
		procedure Close;
		procedure Wait;
		function ExitCode: sint;
	end;

	DynamicLibrary = object
	type
		FunctionDesc = object
			fn: pointer; {pCodePointer}
			name: string;
			function Load(var lib: DynamicLibrary): boolean;
			function Load(const desc: array of FunctionDesc; var lib: DynamicLibrary; const fail: string): boolean; static;
		end;

		function Invalid: DynamicLibrary; static;
		procedure Open(out lib: DynamicLibrary; const filename: string); static;
		function TryOpen(out lib: DynamicLibrary; const filename: string): boolean; static;
		function OK: boolean;
		function Close: boolean;
		function FindProc(const name: string): pointer;
	private
		handle: {$ifdef Windows} Windows.HANDLE {$else} {$error DynamicLibrary.handle: ???} {$endif};
	{$ifdef Debug} fn: string; {$endif}
		procedure Open(out lib: DynamicLibrary; const filename: string; throw: boolean); static;
	end;

type
	pBaseObject = ^BaseObject;
	BaseObject = object
		constructor Init;
		destructor Done; virtual;
		procedure ReplaceVMT(newVmt: pointer);
		function VMTWasReplacedWith(vmt: pointer): boolean;
	private
		function VMTPtr: pPointer;
	end;

	ppObject = ^pObject;
	pObject = ^&Object;
	&Object = object(BaseObject)
	type
		Callback = procedure(obj: pObject; param: pointer);
	var
		constructor Init;
		constructor DeseInit;
		destructor Done; virtual;
		procedure MakeStatic;
		function Static: boolean;
		function NewRef: pointer;
		procedure KillRef;
		procedure AddOnDestroyProc(proc: Callback; param: pointer);
		procedure RemoveOnDestroyProc(proc: Callback; param: pointer);
	const
		MAX_REFCOUNT    = High(uint) - 1;
		STATIC_REFCOUNT = not uint(0);
	type
		CallbackRec = object
			proc: Callback;
			param: pointer;
			function Make(proc: Callback; param: pointer): CallbackRec;
		end;

		pCallbacksList = ^CallbacksList;
		CallbacksList = object
		type
			pRawContainer = ^RawContainer;

			{$define classname := RawContainer} {$define item_type := CallbackRec} {$define extra_predicate_param := CallbackRec}
			{$include vector-link.h.inc}

			procedure Init;
			procedure Done; {$define pSelf := pCallbacksList} {$include dyn_obj.h.inc}
			procedure Add(proc: Callback; param: pointer);
			procedure Remove(proc: Callback; param: pointer);
			function Find(proc: Callback; param: pointer; out it: RawContainer.Iterator): boolean;
			procedure Execute(obj: pObject; clear: boolean);
			function Count: size_t;
			function Empty: boolean;
		private
			raw: pRawContainer;
		end;
	private
		refCount: uint;
		onDestroy: CallbacksList;
		procedure ExecuteOnDestroyChain;
	{$ifdef Debug}
	private
		_sizeof, _not_sizeof_minus_42: size_t;
		procedure Validate; cinline
	{$endif}
	public
		function ReferenceCount: uint;
	protected
		procedure SilentUnref;
	end;
	ObjectsList = array of pObject;

	procedure Free(var obj: pObject);
	procedure FreeWeak(obj: pObject); cinline
	procedure Release(var r: pObject);
	procedure ReleaseArray(var r: ObjectsList);
	procedure ReleaseWeak(r: pObject); cinline
	procedure SetRef(var r: pObject; o2: pObject);
	function MakeRef(obj: pObject): pointer;
	procedure PumpRef(var obj: pObject);
	function InheritsFrom(typ, parent: pointer): boolean;
	function ParentTypeOf(typ: pointer): pointer; cinline

	procedure Zero(mem: pointer; size: size_t); cinline
	procedure memcpy(source: pointer; dest: pointer; count: size_t); external name 'FPC_MOVE';
	function memfind(needle: pointer; nNeedle: size_t; haystack: pointer; nHaystack: size_t): pointer;
{$define index_overloads :=
	function func(const x: typ; buf: pointer; len: size_t): rettype; {$if defined(straight) and not defined(nonpod)} cinline {$endif}
	function func(const x: typ; buf: pointer; len: size_t; stride: size_t): rettype;
	{$undef func} {$undef rettype} {$undef straight} // typ и nonpod хэндлятся в index_header}
{$define index_header :=
	{$define func := Index}     {$define rettype := SizeInt} {$define straight} index_overloads
	{$define func := IndexRev}  {$define rettype := SizeInt} index_overloads
	{$define func := Index1}    {$define rettype := size_t}  index_overloads
	{$define func := Index1Rev} {$define rettype := size_t}  index_overloads
	{$undef typ} {$undef nonpod}}

	// TODO: для всех, как выше
	function IndexIndirect(const x: string; buf: pPointer; len: size_t; offset: size_t): sint;

	{$define typ := char} index_header
	{$define typ := uint32} index_header {$define typ := sint32} index_header
	{$define typ := uint64} index_header {$define typ := sint64} index_header
	{$define typ := pointer} index_header
	{$define typ := string} {$define nonpod} index_header
{$undef index_header} {$undef index_overloads}
	procedure memxor(a, b, target: pointer; size: size_t);
	procedure SwapMem(a, b: pointer; size: size_t);
	procedure Swap(var a, b: pointer);
	function GetMem(size: size_t): pointer; cinline
	procedure FreeMem(var p: pointer); cinline
	procedure FreeMemWeak(p: pointer); cinline

type
	GenericAlignedType = pointer; pGenericAlignedType = ^GenericAlignedType;

{$define intf :=
	function align(x, alignment: typ): typ;
	function align_howmuch(x, alignment: typ): typ;
	function align_left(x, alignment: typ): typ;
	function align_left_howmuch(x, alignment: typ): typ;} all_uints
	function aligned(p: pointer; alignment: size_t): boolean;

type
	AssumedErrorSource = (SoftwareError, SoftwareOrDriverError, HardwareError, OsError);

	procedure Fatal(const msg: string = ''; skipTrace: uint = 0); noreturn;
	procedure Fatal(const msg: string; source: AssumedErrorSource); noreturn;

type
	pColor = ^Color;
	Color = object
	private
		_r, _g, _b: float;
	public
		function RGB(const newR, newG, newB: float): Color; static;
		function Intensity: float;
		property R: float read _r;
		property G: float read _g;
		property B: float read _b;
	end;
	operator =(const a, b: Color): boolean;

type
	KeyboardKey = (
		key_Esc, key_LCtrl, key_LAlt, key_LShift, key_RCtrl, key_RAlt, key_RShift,
		key_Enter, key_Tab, key_Space, key_Insert, key_Delete, key_Home, key_End,
		key_Left, key_Right, key_Up, key_Down,
		key_A, key_B, key_C, key_D, key_E, key_F, key_G, key_H, key_I, key_J, key_K, key_L, key_M,
		key_N, key_O, key_P, key_Q, key_R, key_S, key_T, key_U, key_V, key_W, key_X, key_Y, key_Z,
		key_0, key_1, key_2, key_3, key_4, key_5, key_6, key_7, key_8, key_9,
		key_Tilde, key_Minus, key_Equal, key_Backslash, key_Backspace,
		key_OpenSq, key_CloseSq, key_Semicolon, key_Quote, key_Comma, key_Period, key_Slash,
		key_NumPlus, key_NumMinus, key_NumStar, key_NumDivide,
		key_Num0, key_Num1, key_Num2, key_Num3, key_Num4, key_Num5, key_Num6, key_Num7, key_Num8, key_Num9,
		key_F1, key_F2, key_F3, key_F4, key_F5, key_F6, key_F7, key_F8, key_F9, key_F10, key_F11, key_F12,
		key_AltEnter);
	KeyboardKeys = set of KeyboardKey;

const
	CharKeys = [key_Backspace,
		key_Space, key_Tab,
		key_A, key_B, key_C, key_D, key_E, key_F, key_G, key_H, key_I, key_J, key_K, key_L, key_M,
		key_N, key_O, key_P, key_Q, key_R, key_S, key_T, key_U, key_V, key_W, key_X, key_Y, key_Z,
		key_0, key_1, key_2, key_3, key_4, key_5, key_6, key_7, key_8, key_9,
		key_Tilde, key_Minus, key_Equal, key_Backslash,
		key_OpenSq, key_CloseSq, key_Semicolon, key_Quote, key_Comma, key_Period, key_Slash,
		key_NumPlus, key_NumMinus, key_NumStar, key_NumDivide,
		key_Num0, key_Num1, key_Num2, key_Num3, key_Num4, key_Num5, key_Num6, key_Num7, key_Num8, key_Num9];

{$ifdef use_console}
type
	Console = object
	private
		lock: ThreadLock;
	{$ifdef Windows} hIn, hOut: Windows.HANDLE; {$endif}
		procedure Initialize;
		procedure Finalize;
		function GetSize(id: sint): sint;
		procedure SetSize(id: sint; value: sint);
		function GetSizeX: sint; procedure SetSizeX(newSize: sint);
		function GetSizeY: sint; procedure SetSizeY(newSize: sint);
	public
		procedure GetSizes(out w, h: sint);
		procedure SetSizes(w, h: sint);
		procedure SetPosition(x, y: sint);
		procedure SetColor(const color: Color);
		procedure Write(const s: string);
		procedure Write(const c: char; n: uint);
		procedure Write(x, y: sint; nChars: sint; stride: size_t; sym: pUTFchar; col: pColor);
		procedure Write(x, y: sint; sym: UTFchar; count: uint);
		procedure WriteLine(const s: string = '');
		procedure Input(const s: string);
		function ReadLine: string;
		function ScanKey(out key: KeyboardKey): boolean;
		procedure ScanKey;

		property SizeX: sint read GetSizeX write SetSizeX;
		property SizeY: sint read GetSizeY write SetSizeY;
	end;

var
	Con: Console;
{$endif}

{$ifdef Windows}
type
	WindowsSpecific = object
		function DecryptKey(code: uint; msg: PMSG; out key: KeyboardKey): boolean; static;
		function CheckVersion(major, minor: sint): boolean; static;
		function DescribeError(code: dword): string; static;
		function OperationFailedMessage(const what: string; code: dword = 0): string; static;
		function OperationFailed(const what: string; code: dword = 0): Exception; static;
		function FunctionFailed(const what: string; code: dword = 0): Exception; static;
		function ToWideFileName(const fn: string): widestring;
		function FromWideFileName(const fn: widestring): string;
	type
		Version = (Vista, Seven);
	const
		Versions: array[Version] of record
			major, minor: sint;
		end =
		(
			(major: 6; minor: 0), // Vista
			(major: 6; minor: 1)  // Seven
		);
		function CheckVersion(ver: Version): boolean; static;
	end;
{$endif}

	function GetEnv(const name: string): string;
	function ExecFileName: string;

type
	CommandLine = object
		function Get: CommandLine; static;
		procedure Done;
		function Count: uint;
		function Param(id: uint): string;
		property Params[id: uint]: string read Param; default;
		function Raw: string; static;
		function ToStrings: Strings;
	private
	{$ifdef Debug} guard: pointer; {$endif}
	{$ifdef Windows}
		argv: ^pWideChar;
		nargv: uint;
	{$endif}
	end;

type
	Heap = object
	type
		scoped_enum_ Flag = (Lock); _end
		FlagSet = set of Flag;
	var
		procedure Create(out h: Heap; const name: string; flags: FlagSet = []); static;
		procedure Close;
		function Invalid: Heap; static;
		function OK: boolean;
		function Alloc(size: size_t): pointer;
		procedure Free(ptr: pointer);
		function Realloc(ptr: pointer; size: size_t): pointer;
	private
		handle: {$ifdef Windows} Windows.HANDLE {$else} {$error Heap.handle: ???} {$endif};
	{$ifdef Windows} wflags: dword; {$endif}
	{$ifdef Debug} name: string; {$endif}
	public const
		Lock = Flag.Lock;
	end;

	HeapDump = object
	type
		pBlock = ^Block;
		Block = record
			address: PtrUint;
			size: size_t;
		end;
	var
		tmpHeap: Heap;
		nBlocks: sint;
		blocks: pBlock;
		function Get: HeapDump; static;
		procedure Close;
	end;

	Crypt = class
		class procedure Random(buf: pointer; len: size_t);
		class function Random32: uint32;
		class function Random64: uint64;
	private
		class procedure Initialize;
		class procedure Finalize;

	{$ifdef Windows}
	private type
		HCRYPTPROV = type pointer;
	const
		CRYPT_VERIFYCONTEXT = $F0000000;
		PROV_RSA_FULL = 1;
	class var
		Provider: HCRYPTPROV;
		class function GetProvider: HCRYPTPROV;
	{$endif}
	end;
	procedure NonCrucialRandom(buf: pointer; len: size_t);

type
	Dialog = object
	type
		AnswerEnum = (TaskCancel, TaskOK, TaskYes, TaskNo, TaskV1, TaskV2, TaskV3);
		scoped_enum_ Semantics = (Error, Warning, Info, Message); _end
	const
		FirstVariantAnswer = TaskV1;
		LastVariantAnswer = TaskV3;

		function Show: AnswerEnum;
		function Show(const withText: string): AnswerEnum;
		function Text(const value: string): Dialog;
		function Title(const value: string): Dialog;
		function Footer(const value: string): Dialog;
		function Expanded(const value: string): Dialog;
		function Variant(const value: string): Dialog;
		function OkCancel: Dialog;
		function YesNo: Dialog;
		function YesNoCancel: Dialog;
		function ContinueOrStopVariants: Dialog;
		function ForcePlain: Dialog;

	private type
	scoped_enum_ Answers = (OK, OkCancel, YesNo, YesNoCancel, UserVariants); _end
		LinkReplaceFunc = function(const s: string; param: pointer): string;

		pInternal = ^Internal;
		Internal = record
			ans: Answers;
			sem: Semantics;
			text, title, footer, expanded: string;
			variants: array of string;
			forcePlain: boolean;
		end;
	var
		int: pInternal;

		function Create(sem: Semantics): Dialog; static;
		function SetString(var s: string; const value: string; const sep: string = ''): Dialog;
		function SetAnswers(out ans: Answers; value: Answers): Dialog;
		function ReplaceLinks(const s: string; replace: LinkReplaceFunc; param: pointer): string; static;
	end;
	function Error: Dialog;
	function Warning: Dialog;
	function Info: Dialog;
	function Message: Dialog;

type
	DateTime = object
		year, month, day, hour, min, sec, msec: uint;
		function YMDHMSMS(y, mo, d, h, mi, s, ms: uint): DateTime; static;
		function GetLocal: DateTime; static;
		function Validate: boolean;
		function ToCode: string;
		function Start: DateTime; static;
	const
		Zero: DateTime = (year: 0; month: 0; day: 0; hour: 0; min: 0; sec: 0; msec: 0);
	end;

type
	// AcquireExceptionObject не извлекает объект исключения из лап RTL (хотя в простых случаях работает), поэтому пусть будет так.
	pExceptionRef = ^ExceptionRef;
	ExceptionRef = object
		refcount: uint;
		message: string;
		inner: pExceptionRef;
	{$ifdef Debug} logged: boolean; {$endif}
		function Create(const message: string; inner: pExceptionRef): pExceptionRef; static;
		function Ref: pExceptionRef;
		procedure Kill;
	end;

	Exception = class
	public
		constructor Create(xc: pExceptionRef);
		constructor Create(const newMessage: string);
		constructor Create(const newMessage: string; newInner: pExceptionRef);
		constructor Acquire;
		constructor CreateBlank;
		destructor Destroy; override;
		function Human: string;
		class function Current: System.TObject;
		function RawMessage: string;

		class function Message: string;  class function Message(E: System.TObject): string;
		class procedure Show;            class procedure Show(E: System.TObject);
		class procedure Fatal;           class procedure Fatal(E: System.TObject);
	{$ifdef Debug}
		class procedure Log;             class procedure Log(E: System.TObject);
		function Logged: boolean;
	{$endif}
	{$ifdef use_console}
		class procedure Print;           class procedure Print(E: System.TObject);
		class procedure More;            class procedure More(E: System.TObject);
	{$endif}
	private
		ref: pExceptionRef;
		class function ExtractInnerExceptionRef: pExceptionRef;

{$ifdef Debug}
	public type
		LogProc = procedure(const message: string);
		class procedure SetLogger(logProc: LogProc);
	private class var
		logger: record
			proc: LogProc;
		end;
{$endif}
	end;
	function Error(const message: string): Exception;

type
	TerminateThread = class(System.TObject) end;

type
	UnitRegistry = object
	type
		InitFinalProc = procedure;
	{$ifdef selftest} TestProc = procedure; {$endif}

		SingletonRec = record
			name: string;
			finalize: InitFinalProc;
		end;

		pItemRec = ^ItemRec;
		ItemRec = object
			name: string;
			initialize, finalize: InitFinalProc;
			priority: sint;
			singletons: array of SingletonRec;
		{$ifdef selftest} test: TestProc; {$endif}
		const
			Empty: ItemRec = (name: ''; initialize: nil; finalize: nil; priority: 0; singletons: nil);
		end;

		UnitRef = object
			function Initialize(proc: InitFinalProc): UnitRef;
			function Initialize(initProc, finalProc: InitFinalProc): UnitRef;
			function Finalize(proc: InitFinalProc): UnitRef;
			function Priority(value: sint): UnitRef;
		{$ifdef selftest}  function Test(newTest: TestProc): UnitRef; {$endif}
		private
			It: pItemRec;
		end;

	var
		procedure Init;
		procedure Done;
		function Add(const name: string): UnitRef;
		procedure AddSingleton(const unitname: string; finalize: InitFinalProc);
		procedure InitializeAll(autoFinalize: boolean = yes);
		procedure FinalizeAll;
	{$ifdef selftest} procedure TestAll; {$endif}

	type
		{$define classname := ItemsList} {$define item_type := ItemRec} {$include vector.h.inc}
	var
		list: ItemsList;
		initialized: boolean;
	end;
	function &Unit(const name: string): UnitRegistry.UnitRef;

var
	units: UnitRegistry;

{$ifdef Debug}
type
	Statistics = object
	type
		MaxEnum = (max_object_refcount, max_ondestroy_chain_len, max_real_ondestroy_chain_len, max_threadpool_pending_tasks,
			max_io_pending, max_mmaps, max_hash_nelems, max_chain_hash_chain_length, max_open_hash_chain_length,
			max_strpool_len, max_strpool_refcount,
			max_kd_depth, max_rigid_contacts, max_uniforms_in_shader, max_edges_incident_to_waypoint_vertex, max_uniforms_per_instance,
			max_drawables_in_mesh, max_functions_in_delegate, max_script_delegate_uid, max_entity_actions,
			max_gl_materials_in_pool, max_phys_primitives_in_pool, max_particles_in_system, max_normal_random_retries, max_random_uint_retries);
		TotalEnum = (total_objects_created, spurious_wakeups, total_wakeups, total_threadpool_tasks, truly_async_writes, async_writes_completed_synchronously,
			reused_mmaps, total_timers, duplicate_timers, passed_chain_hash_elems, chain_hash_searches, passed_open_hash_elems, open_hash_searches,
			relocated_open_hash_elems, n_lua_blocks, n_way_searches, qsorts, qsort_fallbacks, maybenormalized_vectors, maybenormalized_denorm_vectors,
			maybenormalized_quaternions, maybenormalized_denorm_quaternions, dyn_obj_frees, dyn_obj_nil_frees, ticks_overhead_estimation_retries);
		CurrentAndMax = (alive_objects, threadpool_simultaneous_pending_tasks, opened_files);
		TotalSizeEnum = (lua_total_blocks_mem, lua_strings_mem, lua_tables_mem, lua_functions_mem, lua_udatas_mem, lua_threads_mem, lua_other_mem);
		TotalHpFloat = (way_searches_time);
		JustValue = (rigid_contacts_limit, kd_depth_limit);
	public
		lock: ThreadLock;
		max: array[MaxEnum] of uint;
		total: array[TotalEnum] of casint_t;
		cam: array[CurrentAndMax] of record
			current, max: casint_t;
		end;
		totalSize: array[TotalSizeEnum] of size_t;
		totalHpf: array[TotalHpFloat] of hp_float;
		jv: array[JustValue] of sint;
		procedure Init;
		procedure Done;
		function Note(what: MaxEnum; value: uint): boolean;
		procedure Increment(what: TotalEnum);
		procedure Increment(what: CurrentAndMax);
		procedure Decrement(what: CurrentAndMax);
		procedure Increment(what: TotalSizeEnum; by: size_t);
		procedure Increment(what: TotalHpFloat; const by: hp_float);
		procedure Note(what: JustValue; value: sint);
	end;

var
	stat: Statistics;
{$endif}

type
	SystemFeature =
	(
		system_SlimRWLocks,
		system_ConditionVariables
	);
	SystemFeatures = set of SystemFeature;

{$ifdef Debug}
var
	stderr: record end; // я так понимаю, что системный stderr — thread-local, это не очень ок.
	procedure ToStderr(const s: string; linebreak: boolean = yes);
{$endif}

var
	SystemInfo: record
		nCPUs: uint;
		features: SystemFeatures;
		allocationGranularity, pageSize: size_t;
	end =
	(
		nCPUs: 1;
		features: [];
		allocationGranularity: 1;
		pageSize: 1
	);
	SingletonLock: RecursiveThreadLock;

	AppInfo: record
		Feedback: string;
	end =
	(
		Feedback: ''
	);

	// http://stackoverflow.com/questions/10535950/forcing-nvidia-gpu-programmatically-in-optimus-laptops
	// http://developer.download.nvidia.com/devzone/devcenter/gamegraphics/files/OptimusRenderingPolicies.pdf
{$ifdef use_discrete_gpu}
	optimus_ty_zaebal_vkluchaysya_suka: uint32 = 1; export name 'NvOptimusEnablement';
{$endif}

implementation

uses
	Errors {$ifdef Debug}, Debug {$endif};

{$ifdef use_discrete_gpu}
exports
	optimus_ty_zaebal_vkluchaysya_suka;
{$endif}

type
	Hacks = object
	type
		PAnsiRec = ^TAnsiRec;
		TAnsiRec = record
			cpes: record
			case byte of
				0: (CodePage: TSystemCodePage; ElementSize: Word);
				1: (Padding: SizeInt);
			end;
			ref: SizeInt;
			len: SizeInt;
		end;

		pVMT = ^VMT;
		VMT = record
			size, msize: PtrInt;
			parent: pVMT;
		end;
	end;

	function Paths.DLL(const name: string): string; begin result := Libs + Platform + '/' + name + '.' + DllExt; end;
	function Paths.Logs: string;                    begin result := TempBase + LogsFolder; end;
	function Paths.Cache: string;                   begin result := TempBase + CacheFolder; end;

	function Paths.TempBase: string;
	begin
		result := Folder.Temp + 'rr/' + Folder.FilenameNoExt(ExecFileName) + '/';
	end;

{$ifdef Windows}
type
	Win = class
	class var
		Initialized: boolean;
		version: Windows.OSVERSIONINFO;
		class procedure Initialize;
		class procedure Finalize;
		class procedure AlterCD;
		class function DescribeError(code: dword): string;
		class function Coord(x, y: sint): Windows.COORD;
		class function XPTimerPeriod(period: uint): Windows.ULONG;
		class function TextAttribute(const color: Color): WORD;
		class function ModuleFileNameW(path: boolean): widestring;
		class function TimeoutOrInfinite(timeoutMs: uint): dword;
		class function ToDateTime(const dt: Windows.SYSTEMTIME): DateTime;
		class function ToFileAttributes(attrs: dword): FileAttributes;

	type
		QueryStringCallback = procedure(buf: pWideChar; nBuf: size_t; out len: size_t; param: pointer);
		UnparaQueryStringCallback = procedure(buf: pWideChar; nBuf: size_t; out len: size_t);
	const
		QUERY_STRING_LENGTH_UNKNOWN = High(size_t);

		class function Error(const msgBase: string; code: dword = 0): Exception;
		class function FunctionFailed(const fn: string; code: dword = 0): Exception;
		class function OperationFailed(const what: string; code: dword = 0): Exception;
		class function OperationFailedMessage(const what: string; code: dword = 0): string;
		class function FileLoadError(const fn: string; code: dword): Exception;
		class function LowerCase(const s: string): string;
		class function LowerCaseFirst(const s: string): string;
		class function ToWideFileName(const fn: string): widestring;
		class function FromWideFileName(const fn: widestring): string;
		// cb получает длину буфера С нулевым символом, возвращает количество символов без нулевого (< nBuf), если буфера хватает, или
		// требуемую длину буфера с нулевым (> nBuf), если нет. QUERY_STRING_LENGTH_UNKNOWN — если известен только факт, что не хватает.
		class function QueryString(cb: QueryStringCallback; param: pointer; const ofWhat: string): widestring;
		class function QueryString(cb: UnparaQueryStringCallback; const ofWhat: string): widestring;

	const
		NullDevice = 'nul';
	{$ifdef Debug} TimeQuantMs = 3; {$endif}
		STILL_ALIVE = 259;
		PROGRESS_CONTINUE = 0;
		PROGRESS_CANCEL   = 1;
		COPY_FILE_FAIL_IF_EXISTS = $1;

	type
		FILE_NOTIFY_INFORMATION = record
			NextEntryOffset: DWORD;
			Action: DWORD;
			FileNameLength: DWORD;
			FileName: array[0 .. 0] of widechar;
		end;

	const
		FILE_NOTIFY_CHANGE_LAST_WRITE = $10;
		FILE_ACTION_MODIFIED          = $3;

	type
		SRWLOCK = pointer;
	class var
		SRW: record
		{$define WinSRWSupported := Assigned(Win.SRW.InitializeSRWLock)}
			InitializeSRWLock: procedure(out lock: Win.SRWLOCK); stdcall;
			TryAcquireSRWLockExclusive: function(var lock: Win.SRWLOCK): byte; stdcall;
			AcquireSRWLockExclusive: procedure(var lock: Win.SRWLOCK); stdcall;
			ReleaseSRWLockExclusive: procedure(var lock: Win.SRWLOCK); stdcall;
			TryAcquireSRWLockShared: function(var lock: Win.SRWLOCK): byte; stdcall;
			AcquireSRWLockShared: procedure(var lock: Win.SRWLOCK); stdcall;
			ReleaseSRWLockShared: procedure(var lock: Win.SRWLOCK); stdcall;
		end;

	type
		CONDITION_VARIABLE = pointer;
	class var
		CV: record
		{$define WinCVSupported := Assigned(Win.CV.InitializeConditionVariable)}
			InitializeConditionVariable: procedure(out cv: Win.CONDITION_VARIABLE); stdcall;
			WakeAllConditionVariable: procedure(var cv: Win.CONDITION_VARIABLE); stdcall;
			WakeConditionVariable: procedure(var cv: Win.CONDITION_VARIABLE); stdcall;
			SleepConditionVariableCS: function(var cv: Win.CONDITION_VARIABLE; var cs: CRITICAL_SECTION; dwMilliseconds: dword): Windows.BOOL; stdcall;
			SleepConditionVariableSRW: function(var cv: Win.CONDITION_VARIABLE; var lock: Win.SRWLOCK; dwMilliseconds: dword; flags: Windows.ULONG): Windows.BOOL; stdcall;
		end;

	const
		CRITICAL_SECTION_NO_DEBUG_INFO = $01000000;
	class var
		CSX: record
			InitializeCriticalSectionEx: function(out cs: CRITICAL_SECTION; dwSpinCount: dword; flags: dword): Windows.BOOL; stdcall;
		end;

	type
		PTP_CALLBACK_INSTANCE = ^TP_CALLBACK_INSTANCE;    TP_CALLBACK_INSTANCE = record end;
		PTP_WORK              = ^TP_WORK;                 TP_WORK              = record end;
		PTP_CALLBACK_ENVIRON  = ^TP_CALLBACK_ENVIRON;     TP_CALLBACK_ENVIRON  = record end;
		PTP_TIMER             = ^TP_TIMER;                TP_TIMER             = record end;
		PTP_IO                = ^TP_IO;                   TP_IO                = record end;

		TP_WORK_CALLBACK = procedure(Instance: PTP_CALLBACK_INSTANCE; Context: pointer; Work: PTP_WORK); stdcall;
		TP_TIMER_CALLBACK = procedure(Instance: PTP_CALLBACK_INSTANCE; Context: pointer; Timer: PTP_TIMER); stdcall;
		TP_IO_CALLBACK = procedure(Instance: PTP_CALLBACK_INSTANCE; Context: pointer;
			Overlapped: LPOVERLAPPED; IoResult: Windows.ULONG; NumberOfBytesTransferred: Windows.ULONG_PTR; Io: PTP_IO); stdcall;

	class var
		VistaTP: record
		{$define WinVistaTPSupported := Assigned(Win.VistaTP.CreateThreadpoolWork)}
			CreateThreadpoolWork: function(pfnwk: TP_WORK_CALLBACK; pv: pointer; pcbe: PTP_CALLBACK_ENVIRON): PTP_WORK; stdcall;
			SubmitThreadpoolWork: procedure(pwk: PTP_WORK); stdcall;
			CloseThreadpoolWork: procedure(pwk: PTP_WORK); stdcall;
			WaitForThreadpoolWorkCallbacks: procedure(pwk: PTP_WORK; fCancelPendingCallbacks: Windows.BOOL); stdcall;

			CreateThreadpoolTimer: function(pfnti: TP_TIMER_CALLBACK; pv: pointer; pcbe: PTP_CALLBACK_ENVIRON): PTP_TIMER; stdcall;
			SetThreadpoolTimer: procedure(pti: PTP_TIMER; pftDueTime: Windows.PFILETIME; msPeriod: DWORD; msWindowLength: DWORD); stdcall;
			WaitForThreadpoolTimerCallbacks: procedure(pti: PTP_TIMER; fCancelPendingCallbacks: Windows.BOOL); stdcall;
			CloseThreadpoolTimer: procedure(pti: PTP_TIMER); stdcall;

			CreateThreadpoolIo: function(fl: Windows.HANDLE; pfnio: TP_IO_CALLBACK; pv: pointer; pcbe: PTP_CALLBACK_ENVIRON): PTP_IO; stdcall;
			StartThreadpoolIo: procedure(pio: PTP_IO); stdcall;

			// To prevent memory leaks, you must call the CancelThreadpoolIo function for either of the following scenarios:
         // An overlapped (asynchronous) I/O operation fails (that is, the asynchronous I/O function call returns failure with an error code
			// other than ERROR_IO_PENDING).
			CancelThreadpoolIo: procedure(pio: PTP_IO); stdcall;
			WaitForThreadpoolIoCallbacks: procedure(pio: PTP_IO; fCancelPendingCallbacks: Windows.BOOL); stdcall;
			CloseThreadpoolIo: procedure(pio: PTP_IO); stdcall;

			CallbackMayRunLong: function(pci: PTP_CALLBACK_INSTANCE): Windows.BOOL; stdcall;
		end;

	type
		LPPROGRESS_ROUTINE = function(TotalFileSize, TotalBytesTransferred, StreamSize, StreamBytesTransferred: Windows.LARGE_INTEGER;
			dwStreamNumber, dwCallbackReason: DWORD; hSourceFile, hDestinationFile: Windows.HANDLE; lpData: pointer): DWORD; stdcall;
		LPFIBER_START_ROUTINE = procedure(lpParameter: pointer); stdcall;

		WaitOrTimerCallback = procedure(param: pointer; isTimer: ByteBool); stdcall;
	const
		WT_EXECUTEDEFAULT       = 0;
		WT_EXECUTEINTIMERTHREAD = $20;
		WT_EXECUTELONGFUNCTION  = $10;
	class var
		XPTp: record
			BindIoCompletionCallback: function(FileHandle: Windows.HANDLE; func: LPOVERLAPPED_COMPLETION_ROUTINE; flags: Windows.ULONG): Windows.BOOL; stdcall;
			QueueUserWorkItem: function(func: LPTHREAD_START_ROUTINE; Context: pointer; Flags: Windows.ULONG): Windows.BOOL; stdcall;

			CreateTimerQueue: function: Windows.HANDLE; stdcall;
			DeleteTimerQueueEx: function(TimerQueue: Windows.HANDLE; CompletionEvent: Windows.HANDLE): Windows.BOOL; stdcall;
			CreateTimerQueueTimer: function(out phNewTimer: Windows.HANDLE; TimerQueue: Windows.HANDLE; Callback: WaitOrTimerCallback; Parameter: pointer;
				DueTime, Period: dword; Flags: Windows.ULONG): Windows.BOOL; stdcall;
			DeleteTimerQueueTimer: function(TimerQueue, Timer: Windows.HANDLE; CompletionEvent: Windows.HANDLE): Windows.BOOL; stdcall;
			ChangeTimerQueueTimer: function(TimerQueue, Timer: Windows.HANDLE; DueTime, Period: Windows.ULONG): Windows.BOOL; stdcall;
		end;

	type
		PROCESS_HEAP_ENTRY = record
			lpData: pointer;
			cbData: dword;
			cbOverhead: byte;
			iRegionIndex: byte;
			wFlags: word;
		case byte of
			0: (hMem: Windows.HANDLE; dwReserved: array[0 .. 2] of dword);
			1: (dwCommittedSize: dword; dwUnCommittedSize: dword; lpFirstBlock: pointer; lpLastBlock: pointer);
		end;

	const
		COINIT_MULTITHREADED = $0;
		COINIT_DISABLE_OLE1DDE   = $4;

	type
		HINSTANCE = Windows.HMODULE;

	{$push} {$packrecords c}
		SHELLEXECUTEINFO = record
			cbSize:         DWORD;
			fMask:          Windows.ULONG;
			hwnd:           HWND;
			lpVerb, lpFile, lpParameters, lpDirectory: PCWSTR;
			nShow:          cint;
			hInstApp:       HINSTANCE;
			lpIDList:       pointer;
			lpClass:        PCWSTR;
			hkeyClass:      HKEY;
			dwHotKey:       DWORD;
			hIconOrMonitor: HANDLE;
			hProcess:       HANDLE;
		end;
	{$pop}

	const
		SEE_MASK_NOASYNC = $00000100;
		SEE_MASK_UNICODE = $00004000;
	{$ifndef Debug}
		SEE_MASK_FLAG_NO_UI = $00000400; // Do not display an error message box if an error occurs.
	{$endif}

	type
		TASKDIALOG_FLAGS               = dword;
		TASKDIALOG_COMMON_BUTTON_FLAGS = dword;
		PFTASKDIALOGCALLBACK = function(hwnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM; lpRefData: LONG_PTR): HRESULT; stdcall;

		PTASKDIALOG_BUTTON             = ^TASKDIALOG_BUTTON;
		TASKDIALOG_BUTTON              = record
			nButtonID: cint;
			pszButtonText: Windows.PCWSTR;
		end;

	{$push} {$packrecords c}
		TASKDIALOGCONFIG = record
			cbSize:                  Windows.UINT;
			hwndParent:              Windows.HWND;
			hInstance:               HINSTANCE;
			dwFlags:                 TASKDIALOG_FLAGS;
			dwCommonButtons:         TASKDIALOG_COMMON_BUTTON_FLAGS;
			pszWindowTitle:          Windows.PCWSTR;
			mainIcon: record
			case uint of
				0: (hMainIcon:        HICON);
				1: (pszMainIcon:      PCWSTR);
			end;
			pszMainInstruction, pszContent: Windows.PCWSTR;
			cButtons:                Windows.UINT;
			pButtons:                PTASKDIALOG_BUTTON;
			nDefaultButton:          cint;
			cRadioButtons:           Windows.UINT;
			pRadioButtons:           PTASKDIALOG_BUTTON;
			nDefaultRadioButton:     cint;
			pszVerificationText, pszExpandedInformation, pszExpandedControlText, pszCollapsedControlText: Windows.PCWSTR;
			footerIcon: record
			case uint of
				0: (hFooterIcon:      Windows.HICON);
				1: (pszFooterIcon:    Windows.PCWSTR);
			end;
			pszFooter:               Windows.PCWSTR;
			pfCallback:              PFTASKDIALOGCALLBACK;
			lpCallbackData:          Windows.LONG_PTR;
			cxWidth:                 Windows.UINT;
		end;
	{$pop}

	const
		TDF_ENABLE_HYPERLINKS         = $0001; // pszContent, pszExpandedInformation, pszFooter: <A HREF="http://...">text</A>
		TDF_ALLOW_DIALOG_CANCELLATION = $0008;
		TDF_USE_COMMAND_LINKS         = $0010;
		TDF_EXPAND_FOOTER_AREA        = $0040;
		TDF_SIZE_TO_CONTENT           = $1000000;

		TDN_HYPERLINK_CLICKED       = 3; // lParam = (LPCWSTR)pszHREF

		TD_WARNING_ICON     = MAKEINTRESOURCEW(Word(-1));
		TD_ERROR_ICON       = MAKEINTRESOURCEW(Word(-2));
		TD_INFORMATION_ICON = MAKEINTRESOURCEW(Word(-3));

		TDCBF_OK_BUTTON     = $0001;  // ->IDOK
		TDCBF_YES_BUTTON    = $0002;  // ->IDYES
		TDCBF_NO_BUTTON     = $0004;  // ->IDNO
		TDCBF_CANCEL_BUTTON = $0008;  // ->IDCANCEL

		class function EnsureLib(var lib: DynamicLibrary; const filename: string; throw: boolean): boolean;
	class var
		comctl32: record
			lib: DynamicLibrary;

			TaskDialogIndirectTried: boolean;
			TaskDialogIndirect: function(constref pTaskConfig: TASKDIALOGCONFIG; pnButton, pnRadioButton: pcint;
				pfVerificationFlagChecked: Windows.PBOOL): HRESULT; stdcall;
		end;
		class function EnsureTaskDialogIndirect: boolean;

	class var
		ole32: record
			lib: DynamicLibrary;
			CoInitializeEx: function(pvReserved: pointer; dwCoInit: dword): HRESULT; stdcall;
			CoUninitialize: procedure; stdcall;
		end;
		class procedure EnsureCoInit;

	class var
		shell32: record
			lib: DynamicLibrary;
			ShellExecuteExW: function(var info: Win.SHELLEXECUTEINFO): Windows.BOOL; stdcall;
		end;
		class procedure EnsureShellExecuteExW;

	class var
		advapi32: record
			lib: DynamicLibrary;
			CryptAcquireContextW: function(out phProv: Crypt.HCRYPTPROV; pszContainer, pszProvider: LPCWSTR; dwProvType, dwFlags: DWORD): Windows.BOOL; stdcall;
			CryptReleaseContext: function(hProv: Crypt.HCRYPTPROV; dwFlags: DWORD): Windows.BOOL; stdcall;
			CryptGenRandom: function(hProv: Crypt.HCRYPTPROV; dwLen: DWORD; pbBuffer: pointer): Windows.BOOL; stdcall;
		end;
		class procedure EnsureCryptoAPI;

	type
		pEventOnCV = ^EventOnCV;
		EventOnCV = object
			lock: ThreadLock;
			cv: CONDITION_VARIABLE;
			autoReset, state: boolean;
			procedure Init(flags: ThreadEvent.InitFlags);
			procedure Done;
			procedure SetEvent; cinline
			procedure ResetEvent; cinline
			function Wait(timeoutMs: uint): boolean; cinline
			function GetState: boolean; cinline
		end;

		pCVOnEvent = ^CVOnEvent;
		CVOnEvent = object
		private const
			SIGNAL = 0;
			BROADCAST = 1;
		public var
			ev: array[0 .. 1] of Windows.HANDLE;
			lock: ThreadLock;
			nWaiters: uint;
			procedure Init;
			procedure Done;
			function Wait(const ext_lock: ThreadLockReference; timeoutMs: uint): boolean; cinline
			procedure WakeOne; cinline
			procedure WakeAll; cinline
		end;
	end;

	function SetFilePointerEx(hFile: HANDLE; liDistanceToMove: LARGE_INTEGER; lpNewFilePointer: PLARGE_INTEGER; dwMoveMethod: DWORD): Windows.BOOL; stdcall; external kernel32;
	function GetFileSizeEx(hFile: HANDLE; lpFileSize: PLARGE_INTEGER): Windows.BOOL; stdcall; external kernel32;
	function ReadDirectoryChangesW(hDirectory: Windows.HANDLE; lpBuffer: pointer; nBufferLength: dword; bWatchSubtree: Windows.BOOL;
		dwNotifyFilter: dword; lpBytesReturned: pDword; lpOverlapped: LPOVERLAPPED; lpCompletionRoutine: LPOVERLAPPED_COMPLETION_ROUTINE): Windows.BOOL; stdcall; external kernel32;

	function ConvertThreadToFiber(lpParameter: pointer): pointer; stdcall; external kernel32;
	function ConvertFiberToThread: Windows.BOOL; stdcall; external kernel32;
	procedure DeleteFiber(lpFiber: pointer); stdcall; external kernel32;
	function CreateFiberEx(dwStackCommitSize, dwStackReserveSize: csize_t; dwFlags: DWORD; lpStartAddress: Win.LPFIBER_START_ROUTINE; lpParameter: pointer): pointer; stdcall; external kernel32;
	procedure SwitchToFiber(lpFiber: pointer); stdcall; external kernel32;

	function CommandLineToArgvW(lpCmdLine: LPCWSTR; out pNumArgs: sint32): pointer; stdcall; external 'shell32';
	function GetSystemTimes(lpIdleTime, lpKernelTime, lpUserTime: LPFILETIME): Windows.BOOL; stdcall; external kernel32;
	function GetUserDefaultUILanguage: Windows.LANGID; stdcall; external kernel32;

{$ifdef Debug}
	function timeBeginPeriod(uPeriod: Windows.UINT): Windows.UINT; stdcall; external 'winmm';
	function timeEndPeriod(uPeriod: Windows.UINT): Windows.UINT; stdcall; external 'winmm';
{$endif}
	function HeapWalk(hHeap: Windows.HANDLE; var lpEntry: Win.PROCESS_HEAP_ENTRY): Windows.BOOL; stdcall; external kernel32;
	function CopyFileExW(lpExistingFileName, lpNewFileName: LPCWSTR; lpProgressRoutine: Win.LPPROGRESS_ROUTINE; lpData: pointer; pbCancel: Windows.LPBOOL;
		dwCopyFlags: dword): Windows.BOOL; stdcall; external kernel32;
	function MoveFileWithProgressW(lpExistingFileName, lpNewFileName: LPCWSTR; lpProgressRoutine: Win.LPPROGRESS_ROUTINE; lpData: pointer;
		dwFlags: dword): Windows.BOOL; stdcall; external kernel32;

	function WindowsSpecific.DecryptKey(code: uint; msg: PMSG; out key: KeyboardKey): boolean;
	begin
		result := yes;
		case code of
			VK_RETURN:
				if Assigned(msg) and (HIWORD(msg^.lParam) and KF_ALTDOWN = KF_ALTDOWN) then
					key := key_AltEnter
				else
					key := key_Enter;
			VK_SHIFT, VK_LSHIFT: key := key_LShift; VK_RSHIFT: key := key_RShift;
			VK_CONTROL, VK_LCONTROL: key := key_LCtrl; VK_RCONTROL: key := key_RCtrl;
			VK_MENU: key := key_LAlt;
			VK_ESCAPE: key := key_Esc; VK_SPACE: key := key_Space; VK_TAB: key := key_Tab; VK_BACK: key := key_Backspace;
			VK_LEFT: key := key_Left; VK_UP: key := key_Up; VK_RIGHT: key := key_Right; VK_DOWN: key := key_Down;
			$30 .. $39: key := KeyboardKey(ord(key_0) + (code - $30));
			$41 .. $5a: key := KeyboardKey(ord(key_A) + (code - $41));
			$60 .. $69: key := KeyboardKey(ord(key_Num0) + (code - $60));
			VK_ADD: key := key_NumPlus; VK_SUBTRACT: key := key_NumMinus; VK_MULTIPLY: key := key_NumStar; VK_DIVIDE: key := key_NumDivide;
			VK_INSERT: key := key_Insert; VK_DELETE: key := key_Delete; VK_HOME: key := key_Home; VK_END: key := key_End;
			VK_F1 .. VK_F12: key := KeyboardKey(ord(key_F1) + (code - VK_F1));
			192: key := key_Tilde; 189: key := key_Minus; 187: key := key_Equal; 220: key := key_Backslash;
			219: key := key_OpenSq; 221: key := key_CloseSq; 186: key := key_Semicolon; 222: key := key_Quote;
			188: key := key_Comma; 190: key := key_Period; 191: key := key_Slash;
			else
				result := no;
		end;
	end;

	function WindowsSpecific.CheckVersion(major, minor: sint): boolean;
	begin
		result :=
			(Win.version.dwMajorVersion > uint(major)) or
			((Win.version.dwMajorVersion = uint(major)) and (Win.version.dwMinorVersion >= uint(minor)));
	end;

	function WindowsSpecific.CheckVersion(ver: Version): boolean; begin result := CheckVersion(Versions[ver].major, Versions[ver].minor); end;
	function WindowsSpecific.DescribeError(code: dword): string;  begin result := Win.DescribeError(code); end;
	function WindowsSpecific.OperationFailed(const what: string; code: dword = 0): Exception; begin result := Win.OperationFailed(what, code); end;
	function WindowsSpecific.OperationFailedMessage(const what: string; code: dword = 0): string; begin result := Win.OperationFailedMessage(what, code); end;
	function WindowsSpecific.FunctionFailed(const what: string; code: dword = 0): Exception; begin result := Win.FunctionFailed(what, code); end;
	function WindowsSpecific.ToWideFileName(const fn: string): widestring; begin result := Win.ToWideFileName(fn); end;
	function WindowsSpecific.FromWideFileName(const fn: widestring): string; begin result := Win.FromWideFileName(fn); end;
{$endif}

type
	UserCompletionCallback = procedure(const status: AsioStatus; buf: pointer; param: pointer);

	pAsyncWrite = ^AsyncWrite;
	AsyncWrite = record
	{$ifdef Windows} op: Windows.OVERLAPPED; {$endif}
		ref: pFileHandle;
		cb: UserCompletionCallback;
		param: pointer;
		unref: boolean;
	{$ifdef Debug} magicSelfPtr: pAsyncWrite; {$endif}
		data: array[0 .. 0] of GenericAlignedType;
	end;

	AsyncIORegistry = object
		pending: PendingSync;

		procedure Init;
		procedure Done;
		function Add(ref: pFileHandle; cb: UserCompletionCallback; param: pointer; extraDataSize: size_t): pAsyncWrite;
		function Add(ref: pFileRef; dataSize: size_t {$ifdef Windows}; const overlappedOffset: qword {$endif}): pAsyncWrite;
		procedure Close(aw: pAsyncWrite; fromCompletionCallback: boolean; const status: AsioStatus);
	private
		function InternalAdd(ref: pFileHandle; cb: UserCompletionCallback; param: pointer; extraDataSize: size_t; unref: boolean): pAsyncWrite;
	end;

	procedure AsyncIORegistry.Init;
	begin
		pending.Init;
	end;

	procedure AsyncIORegistry.Done;
	begin
		pending.Done;
	end;

	function AsyncIORegistry.Add(ref: pFileHandle; cb: UserCompletionCallback; param: pointer; extraDataSize: size_t): pAsyncWrite;
	begin
		result := InternalAdd(ref, cb, param, extraDataSize, no);
	end;

	function AsyncIORegistry.Add(ref: pFileRef; dataSize: size_t {$ifdef Windows}; const overlappedOffset: qword {$endif}): pAsyncWrite;
	begin
		// Assert(MemSize(ref) = sizeof(FileRef), ToString(MemSize(ref)) + ' <-> ' + ToString(sizeof(FileRef)));
		result := InternalAdd(ref^.Ref, nil, nil, dataSize, yes);

	{$ifdef Windows}
		result^.op.Offset       := Lo(overlappedOffset);
		result^.op.OffsetHigh   := Hi(overlappedOffset);
	{$endif}
	end;

	procedure AsyncIORegistry.Close(aw: pAsyncWrite; fromCompletionCallback: boolean; const status: AsioStatus);
	var
		unref: boolean;
	begin
	{$ifdef Debug} Assert(aw^.magicSelfPtr = aw, 'Asio.Close(' + HexStr(aw) + '), похоже, не была добавлена.'); {$endif}
		if not fromCompletionCallback and WinVistaTPSupported then Win.VistaTP.CancelThreadpoolIo(aw^.ref^.tp_io);
		try
			if Assigned(aw^.cb) then
				aw^.cb(status, pGenericAlignedType(aw^.data), aw^.param)
			else
			{$ifdef Debug} if status.Failed then status.error.Show {$endif};
		finally
			status.Done;
		end;

		unref := aw^.unref;
		if unref then
		begin
			// Assert(MemSize(aw^.ref) = sizeof(FileRef));
			pFileRef(aw^.ref)^.KillOne(fromCompletionCallback);
		end;
		FreeMem(aw);
		if unref then pending.KillOne;
	end;

	function AsyncIORegistry.InternalAdd(ref: pFileHandle; cb: UserCompletionCallback; param: pointer; extraDataSize: size_t; unref: boolean): pAsyncWrite;
	{$ifdef Debug} var n: casint_t; {$endif}
	begin
		if unref then
		begin
		{$ifdef Debug} n := {$endif} pending.AddOne; {$ifdef Debug} stat.Note(max_io_pending, n); {$endif}
		end;

		result := GetMem(sizeof(AsyncWrite) - sizeof(AsyncWrite.data) + extraDataSize);
		result^.ref := ref;
		result^.cb := cb;
		result^.param := param;
		result^.unref := unref;
	{$ifdef Debug} result^.magicSelfPtr := result; {$endif}

	{$ifdef Windows}
		result^.op.Internal     := 0;
		result^.op.InternalHigh := 0;
		result^.op.hEvent       := 0;
		if WinVistaTPSupported then Win.VistaTP.StartThreadpoolIo(ref^.tp_io);
	{$endif}
	end;

var
	asio: AsyncIORegistry;

	constructor BaseObject.Init; begin end;
	destructor BaseObject.Done;  begin end;

	function BaseObject.VMTPtr: pPointer;
	begin
		result := pPointer(pointer(@self) + sizeof(BaseObject)) - 1;
	end;

	procedure BaseObject.ReplaceVMT(newVmt: pointer);
	begin
		VMTPtr^ := newVmt;
	end;

	function BaseObject.VMTWasReplacedWith(vmt: pointer): boolean;
	begin
		result := VMTPtr^ = vmt;
	end;

{$define classname := &Object.CallbacksList.RawContainer} {$include vector-link.pp.inc}

	procedure &Object.CallbacksList.Init;
	begin
		raw := nil;
	end;

	procedure &Object.CallbacksList.Done;
	begin
		if Assigned(raw) then raw^.Free;
	end; {$define classname := &Object.CallbacksList} {$define pSelf := pCallbacksList} {$include dyn_obj.pp.inc}

	procedure &Object.CallbacksList.Add(proc: Callback; param: pointer);
	var
		c: ^CallbackRec;
	{$ifdef assert} it: RawContainer.Iterator; {$endif}
	begin
	{$ifdef assert} Assert(not Find(proc, param, it), 'Двойной обработчик!'); {$endif}
		if not Assigned(raw) then raw := RawContainer.Create(1);
		c        := raw^.Grow(1);
		c^.proc  := proc;
		c^.param := param;
	end;

	procedure &Object.CallbacksList.Remove(proc: Callback; param: pointer);
	var
		id: RawContainer.Iterator;
	begin
		if not Find(proc, param, id) then Assert(no, 'Обработчик не найден.');
		raw^.RemoveReplace(id);
	end;

	function SameCallback(const a, b: &Object.CallbackRec): boolean;
	begin
		result := (a.proc = b.proc) and (a.param = b.param);
	end;

	function &Object.CallbacksList.Find(proc: Callback; param: pointer; out it: RawContainer.Iterator): boolean;
	begin
		if Assigned(raw) then
			result := raw^.Find(@SameCallback, CallbackRec.Make(proc, param), it)
		else
			result := no;
	end;

	procedure CallCallback(const cb: &Object.CallbackRec; obj: pointer);
	begin
		cb.proc(obj, cb.param);
	end;

	procedure &Object.CallbacksList.Execute(obj: pObject; clear: boolean);
	var
		t: pRawContainer;
	begin
		t := raw;
		if Assigned(t) then
		begin
			if clear then raw := nil;
			t^.ForEach(@CallCallback, obj);
			if clear then t^.Free;
		end;
	end;

	function &Object.CallbacksList.Count: size_t;
	begin
		if Assigned(raw) then result := raw^.Count else result := 0;
	end;

	function &Object.CallbacksList.Empty: boolean;
	begin
		result := not Assigned(raw) or (raw^.Count = 0);
	end;

	constructor &Object.Init;
	begin
	{$ifdef Debug}
		_sizeof := sizeof(self);
		_not_sizeof_minus_42 := not size_t(sizeof(self)) - 42;
		stat.Increment(total_objects_created);
		stat.Increment(alive_objects);
	{$endif}
		refCount := 0;
		onDestroy.Init;
	end;

	constructor &Object.DeseInit;
	begin
		Init;
	end;

	destructor &Object.Done;
	begin
		Assert((refCount = 0) or (0 = not refCount));
	{$ifdef Debug}
		Validate;
		stat.Decrement(alive_objects);
		_sizeof := 0;
		_not_sizeof_minus_42 := 0;
	{$endif}
		ExecuteOnDestroyChain; // не очень красиво, что в этом месте объект уничтожен, но пока необходимо :(
		onDestroy.Done;
	end;

	procedure &Object.MakeStatic;
	begin
	{$ifdef Debug} Validate; {$endif}
		Assert(refCount = 0, 'cannot make static object with non-zero refcount');
		refCount := STATIC_REFCOUNT;
	end;

	function &Object.Static: boolean;
	begin
		result := 0 = not refcount;
	end;

	function &Object.NewRef: pointer;
	begin
		result := @self;
		if Assigned(result) then
		begin
		{$ifdef Debug} Validate; {$endif}
			if 0 <> not refCount then
			begin
				inc(refcount);
			{$ifdef Debug} stat.Note(max_object_refcount, refCount); {$endif}
			end;
		end;
	end;

	procedure &Object.KillRef;
	begin
	{$ifdef Debug} Validate; {$endif}
		if 0 <> not refCount then
		begin
			Assert(refCount > 0, 'KillRef: refcount = 0.');
			dec(refCount);
			if refCount = 0 then
			begin
				ExecuteOnDestroyChain;
				if refCount = 0 then dispose(@self, Done);
			end;
		end;
	end;

	procedure &Object.AddOnDestroyProc(proc: Callback; param: pointer);
	begin
		onDestroy.Add(proc, param);
	{$ifdef Debug} stat.Note(max_ondestroy_chain_len, onDestroy.Count); {$endif}
	end;

	procedure &Object.RemoveOnDestroyProc(proc: Callback; param: pointer);
	begin
		onDestroy.Remove(proc, param);
	end;

	function &Object.CallbackRec.Make(proc: Callback; param: pointer): CallbackRec;
	begin
		result.proc  := proc;
		result.param := param;
	end;

	procedure &Object.ExecuteOnDestroyChain;
	begin
	{$ifdef Debug} if onDestroy.Count > 0 then stat.Note(max_real_ondestroy_chain_len, onDestroy.Count); {$endif}
		onDestroy.Execute(@self, yes);
	end;

{$ifdef Debug}
	procedure &Object.Validate;
	begin
		if _sizeof <> not size_t(_not_sizeof_minus_42 + 42) then
			Fatal('Object.Validate провалилась!');
	end;
{$endif}

	function &Object.ReferenceCount: uint;
	begin
		result := refCount;
	end;

	procedure &Object.SilentUnref;
	begin
		Assert(refCount > 0, 'SilentUnref: refCount = 0');
		dec(refCount);
	end;

	procedure Free(var obj: pObject);
	var
		t: pObject;
	begin
		if Assigned(obj) then
		begin
			t := obj;
			obj := nil;
		{$ifdef Debug} t^.Validate; {$endif}
			Assert(t^.refCount = 0, 'Free: refcount = ' + ToString(t^.refCount) + '.');
			t^.ExecuteOnDestroyChain;
			dispose(t, Done);
		end;
	end;

	procedure FreeWeak(obj: pObject);
	begin
		Free(obj);
	end;

	procedure Release(var r: pObject);
	var
		t: pObject;
	begin
		if Assigned(r) then
		begin
			t := r;
			r := nil;
			t^.KillRef;
		end;
	end;

	procedure ReleaseArray(var r: ObjectsList);
	var
		trash: ObjectsList;
		i: sint;
	begin
		trash := r;
		r := nil;
		for i := 0 to High(trash) do Release(trash[i]);
	end;

	procedure ReleaseWeak(r: pObject);
	begin
		Release(r);
	end;

	procedure SetRef(var r: pObject; o2: pObject);
	var
		t: pObject;
	begin
		t := r;
		if t <> o2 then
		begin
			if Assigned(o2) then r := o2^.NewRef else r := nil;
			Release(t);
		end;
	end;

	function MakeRef(obj: pObject): pointer;
	begin
		if Assigned(obj) then
			result := obj^.NewRef
		else
			result := nil;
	end;
	
	procedure PumpRef(var obj: pObject);
	begin
		MakeRef(obj);
		Release(obj);
	end;

	function InheritsFrom(typ, parent: pointer): boolean;
	begin
		while Assigned(typ) and (typ <> parent) do
			typ := ParentTypeOf(typ);
		result := typ = parent;
	end;

	function ParentTypeOf(typ: pointer): pointer; begin result := Hacks.pVMT(typ)^.parent; end;
	procedure Zero(mem: pointer; size: size_t);   begin fillchar(mem^, size, 0); end;

	function memfind(needle: pointer; nNeedle: size_t; haystack: pointer; nHaystack: size_t): pointer;
	var
		i: size_t;
	begin
		if nNeedle <= nHaystack then
			for i := 0 to nHaystack - nNeedle do
			begin
				if CompareByte(needle^, haystack^, nNeedle) = 0 then
					exit(haystack);
				inc(haystack);
			end;
		result := nil;
	end;

{$define one_index_impl :=
{$ifdef onebased} {$define rettype := size_t} {$else} {$define rettype := SizeInt} {$endif}
	function func_direct(const x: typ; buf: pointer; len: size_t): rettype;
	{$ifdef nonpod}
		type
			ptyp = ^typ;
		var
			i: size_t;
		begin
			i := 0;
			while i < len do
			begin
				if ptyp(buf)[i] = x then exit(i {$ifdef onebased} + 1 {$endif});
				inc(i); if (i < len) and (ptyp(buf)[i] = x) then exit(i {$ifdef onebased} + 1 {$endif});
				inc(i); if (i < len) and (ptyp(buf)[i] = x) then exit(i {$ifdef onebased} + 1 {$endif});
				inc(i); if (i < len) and (ptyp(buf)[i] = x) then exit(i {$ifdef onebased} + 1 {$endif});
				inc(i);
			end;
			result := {$ifdef onebased} 0 {$else} -1 {$endif};
		end;
	{$else}
		begin
			result :=
			{$if sizeof(x) = sizeof(byte)} IndexByte
			{$elseif sizeof(x) = sizeof(word)} IndexWord
			{$elseif sizeof(x) = sizeof(dword)} IndexDWord
			{$elseif sizeof(x) = sizeof(qword)} IndexQWord
			{$else} {$error} {$endif}
				(buf^, len, {$ifdef conv} conv {$else} x {$endif}) {$ifdef onebased} + 1 {$endif};
		end;
	{$endif}

	function func_rev(const x: typ; buf: pointer; len: size_t): rettype;
	type
		ptyp = ^typ;
	var
		i: size_t;
	begin
		i := len;
		while i > 0 do
		begin
			dec(i);
			if ptyp(buf)[i] = x then exit(i {$ifdef onebased} + 1 {$endif});
		end;
		result := {$ifdef onebased} 0 {$else} -1 {$endif}
	end;

	function func_direct(const x: typ; buf: pointer; len: size_t; stride: size_t): rettype;
	var
		i: size_t;
	begin
		i := len;
		while i > 0 do
		begin
			if typ(buf^) = x then exit(len - i {$ifdef onebased} + 1 {$endif});
			dec(i);
			buf += stride;
		end;
		result := {$ifdef onebased} 0 {$else} -1 {$endif};
	end;

	function func_rev(const x: typ; buf: pointer; len: size_t; stride: size_t): rettype;
	var
		i: size_t;
	begin
		i := len;
		buf += len * stride;
		while i > 0 do
		begin
			buf -= stride;
			dec(i);
			if typ(buf^) = x then exit(i {$ifdef onebased} + 1 {$endif});
		end;
		result := {$ifdef onebased} 0 {$else} -1 {$endif};
	end;

{$ifdef pair}
	function func_direct(const x: pair; buf: pointer; len: size_t): rettype;                 begin result := func_direct(typ(x), buf, len); end;
	function func_rev(const x: pair; buf: pointer; len: size_t): rettype;                    begin result := func_rev(typ(x), buf, len); end;
	function func_direct(const x: pair; buf: pointer; len: size_t; stride: size_t): rettype; begin result := func_direct(typ(x), buf, len, stride); end;
	function func_rev(const x: pair; buf: pointer; len: size_t; stride: size_t): rettype;    begin result := func_rev(typ(x), buf, len, stride); end;
{$endif}
	{$undef func_direct} {$undef func_rev} {$undef onebased} {$undef rettype}
	// pair и nonpod хэндлятся в index_impl}

{$define index_impl :=
	{$define func_direct := Index} {$define func_rev := IndexRev} one_index_impl
	{$define func_direct := Index1} {$define func_rev := Index1Rev} {$define onebased} one_index_impl
	{$undef typ} {$undef conv} {$undef pair} {$undef nonpod}}

	{$define typ := char} {$define conv := uint8(x)} index_impl
	{$define typ := uint32} {$define pair := sint32} index_impl
	{$define typ := uint64} {$define pair := sint64} index_impl
	{$define typ := pointer} {$define conv := pPtrUint(@x)^} index_impl
	{$define typ := string} {$define nonpod} index_impl
{$undef index_impl}
{$undef one_index_impl}

	function IndexIndirect(const x: string; buf: pPointer; len: size_t; offset: size_t): sint;
	var
		i: size_t;
	begin
		i := 0;
		while i < len do
		begin
			if x = pString(buf[i] + offset)^ then exit(i);
			inc(i);
		end;
		result := -1;
	end;

	procedure memxor(a, b, target: pointer; size: size_t);
	{$define op := R := _1 xor _2} {$include over_memory.inc}

	procedure SwapMem(a, b: pointer; size: size_t);
	var
		t: PtrUint;
		tb: byte;
	{$define inplace_op := {$ifdef byte} {$define t := tb} {$endif} t := _1; _1 := _2; _2 := t {$undef t}} {$include over_memory.inc}

{$define swapimpl:=procedure Swap(var a, b: typ); var t: typ; begin t := a; a := b; b := t; end;}
	{$define typ := pointer} swapimpl
	{$undef typ}

	function GetMem(size: size_t): pointer;
	begin
		if size > 0 then
			result := System.GetMem(size)
		else
			result := nil;
	end;

	procedure FreeMem(var p: pointer);
	begin
		if Assigned(p) then
		begin
			System.FreeMem(p);
			p := nil;
		end;
	end;

	procedure FreeMemWeak(p: pointer);
	begin
		if Assigned(p) then System.FreeMem(p);
	end;

{$define intf :=
	function align(x, alignment: typ): typ;
	begin
	{$ifdef Debug} CheckPow2(alignment, 'alignment'); {$endif}
		result := (x + typ(alignment - 1)) and not typ(alignment - 1);
	end;

	function align_howmuch(x, alignment: typ): typ;
	begin
	{$ifdef Debug} CheckPow2(alignment, 'alignment'); {$endif}
		result := (alignment - (x and (alignment - 1))) and (alignment - 1);
	end;

	function align_left(x, alignment: typ): typ;
	begin
	{$ifdef Debug} CheckPow2(alignment, 'alignment'); {$endif}
		result := x and not typ(alignment - 1);
	end;

	function align_left_howmuch(x, alignment: typ): typ;
	begin
	{$ifdef Debug} CheckPow2(alignment, 'alignment'); {$endif}
		result := x and (alignment - 1);
	end;} all_uints

	function aligned(p: pointer; alignment: size_t): boolean;
	begin
	{$ifdef Debug} CheckPow2(alignment, 'aligned'); {$endif}
		result := PtrUint(p - pointer(nil)) and (alignment - 1) = 0;
	end;

	procedure Fatal(const msg: string = ''; skipTrace: uint = 0);
	begin
		unused_args skipTrace end_list
		if msg <> '' then Error {$ifdef Debug} .Expanded('Стек вызовов:' + EOL + GetBackTrace(1 + skipTrace)) {$endif}.Show(msg);
	{$if defined(Windows)}
		TerminateProcess(GetCurrentProcess, 1);
		Sleep(Windows.INFINITE);
	{$else} {$warning Fatal will use System.Halt, you may want to use OS-specific function instead}
	{$endif}
		System.Halt;
	end;

	procedure Fatal(const msg: string; source: AssumedErrorSource);
		function Epilogue(source: AssumedErrorSource): string;
		begin
			case source of
				SoftwareOrDriverError: result := 'Возможно, баг приложения или конфигурации / драйверов.';
				SoftwareError: result := 'Вероятно, баг приложения.';
				HardwareError, OsError:
					begin
						result := 'По-видимому, ';
						if source = HardwareERror then result += 'платформа не подходит' else result += 'ОС или её настройки не подходят';
						result += ' для работы.'
					end;
				else result := '';
			end;
			if result <> '' then result += EOL + EOL;
			if source in [HardwareError, OsError] then result += 'В' else result += 'В любом случае, в';
			result += 'ыполение будет прекращено немедленно.' + EOL + 'Прощай.';
		end;
	begin
		Fatal(msg + EOL + Epilogue(source), 1);
	end;

	operator :=(const x: float32): float16;
	var
		i: uint32 absolute x;
		u: uint16 absolute result.u;
		e, m: sint32;
	begin
		e := ((i shr 23) and $FF) - $70;
		m := i and $7FFFFF;

		u := (i shr 16) and $8000;
		if (e <= 0) then // Denorm
		begin
			m := ((m or $800000) shr (1 - e)) + $1000;
			u := u or (m shr 13);
		end else
			if e = $8F then
			begin
				u := u or $7C00;
				if m <> 0 then // NAN
				begin
					m := m shr 13;
					if m = 0 then m := 1;
					u := u or m;
				end;
			end else
			begin
				m := m + $1000;
				if (m and $800000) <> 0 then // переполнение мантиссы
				begin
					m := 0;
					inc(e);
				end;
				if e < 31 then
					u := u or (e shl 10) or (m shr 13)
				else // переполнение экспоненты
					u := u or $7C00;
			end;
	end;

	operator :=(const x: float16): float32;
	var
		s: uint32;
		e, m: sint32;
	begin
		s := (x.u and $8000) shl 16;
		e := (x.u shr 10) and $1F;
		m := x.u and $3FF;

		if e = 0 then
		begin
			if m = 0 then
			begin
				result := pFloat32(@s)^;
				exit; // +/- 0
			end;

			while (m and $400) = 0 do // денормализованный флоат?
			begin
				m := m + m;
				dec(e);
			end;
			inc(e);
			m := m and not uint32($400);
		end else
			if e = 31 then // INF / NAN
			begin
				s := s or $7F800000 or uint32(m shl 13);
				result := pFloat32(@s)^;
				exit;
			end;
		s := s or uint32((e + $70) shl 23) or uint32(m shl 13);
		result := pFloat32(@s)^;
	end;

	class function FloatIs.NaN(const x: float32): boolean;
	var
		u: uint32 absolute x;
	begin
		// Все биты порядка (23–30) выставлены, хотя бы один бит мантиссы (0–22) выставлен
		result := u and (uint32(1 shl 31) - 1) > uint32($FF shl 23);
	end;

	class function FloatIs.Finite(const x: float32): boolean;
	var
		u: uint32 absolute x;
	begin
		// Не все биты порядка (23–30) выставлены
		result := u shr 23 and $FF <> $FF;
	end;

	class function FloatIs.NaN(const x: float64): boolean;
	var
		u: uint64 absolute x;
		a, b: uint64;
	begin
		// Все биты порядка (52–62) выставлены, хотя бы один бит мантиссы (0–51) выставлен
		a := (uint64(1 shl 63) - 1);
		b := uint64(1 shl 11 - 1) shl 52;
		result := u and a > b;
	end;

	class function FloatIs.Finite(const x: float64): boolean;
	var
		u: uint64 absolute x;
	begin
		// Не все биты порядка (52–62) выставлены
		result := u shr 52 and (1 shl 11 - 1) <> (1 shl 11 - 1);
	end;

	function UTF8.Validate(const s: string): boolean; begin result := Validate(pChar(s), length(s)); end;
	function UTF8.Validate(s: pChar; len: size_t): boolean;
	var
		pos: size_t;
		c: uint;
	begin
		result := no;
		pos := 0;
		while pos < len do
		begin
			c := ord(s[pos]);
			case c of
				%00000000 .. %01111111: inc(pos);
			{$define multibyte :=
				begin
					if (pos + 1 + n > len) {$define times := n} {$define rep := or (ord(s[pos + (1 + repid)]) shr 6 <> %10)} {$include repeat.inc} then exit;
					pos += 1 + n;
				end; {$undef n}}
				%11000000 .. %11011111: {$define n := 1} multibyte
				%11100000 .. %11101111: {$define n := 2} multibyte
				%11110000 .. %11110111: {$define n := 3} multibyte
				else exit;
			{$undef multibyte}
			end;
		end;
		result := yes;
	end;

	function UTF8.GetStatistics(const s: string): Statistics;
	const
		Empty: Statistics = (n: (0, 0, 0, 0));
	var
		pos: size_t;
	begin
		result := Empty;
		pos := 1;
		while pos <= uint(length(s)) do
			case ord(s[pos]) of
				%00000000 .. %01111111: begin inc(result.n[1]); pos += 1; end;
				%11000000 .. %11011111: begin inc(result.n[2]); pos += 2; end;
				%11100000 .. %11101111: begin inc(result.n[3]); pos += 3; end;
				%11110000 .. %11110111: begin inc(result.n[4]); pos += 4; end;
				else exit(Empty);
			end;
	end;

	function UTF8.Codepoints(s: pChar; len: size_t): uint;
	var
		ed: pChar;
	begin
		result := 0;
		ed := s + len;
		while s < ed do
		begin
			if ord(s^) and %11000000 <> %10000000 then inc(result);
			inc(s);
		end;
	end;
	function UTF8.Codepoints(const s: string): uint; begin result := Codepoints(pChar(s), length(s)); end;

	function UTF8.Next(const s: string; var pos: sint): UTFchar;
	begin
		if length(s) - pos + 1 > 0 then result := UTFchar(s[pos]) else exit(UTFInvalid);
		if result <= %01111111 then inc(pos)
		{$define n_more :=
			(1 + n <= length(s) - pos + 1) {$define times := n} {$define rep := and (ord(s[pos + (1 + repid)]) shr 6 = %10)} {$include repeat.inc} then
			begin
				result := (result and (%00011111 shr (n-1))) shl (6*n)
						{$define rep := or (UTFchar(ord(s[pos + (1+repid)]) and %00111111) shl (6*(n-1-repid)))} {$define times := n} {$include repeat.inc};
				pos += n + 1;
			end {$undef n}}
		else if (result >= %11000000) and (result <= %11011111) and {$define n := 1} n_more
		else if (result >= %11100000) and (result <= %11101111) and {$define n := 2} n_more
		else if (result >= %11110000) and (result <= %11110111) and {$define n := 3} n_more
	{$undef n_more} else exit(UTFInvalid);
	end;

	function UTF8.Next(const s: string; var pos: sint; out sym: UTFchar): UTFchar;
	begin
		result := Next(s, pos);
		sym := result;
	end;

	function UTF8.Next(var s: pChar; var cb: size_t): UTFchar;
	begin
		if cb > 0 then result := UTFchar(s[0]) else exit(UTFInvalid);
		if result <= %01111111 then begin s += 1; cb -= 1; end
		{$define n_more :=
			(1 + n <= cb) {$define times := n} {$define rep := and (ord(s[1 + repid]) shr 6 = %10)} {$include repeat.inc} then
			begin
				result := (result and (%00011111 shr (n-1))) shl (6*n)
						{$define rep := or (UTFchar(ord(s[1 + repid]) and %00111111) shl (6*(n-1-repid)))} {$define times := n} {$include repeat.inc};
				s  += 1 + n; cb -= 1 + n;
			end {$undef n}}
		else if (result >= %11000000) and (result <= %11011111) and {$define n := 1} n_more
		else if (result >= %11100000) and (result <= %11101111) and {$define n := 2} n_more
		else if (result >= %11110000) and (result <= %11110111) and {$define n := 3} n_more
	{$undef n_more} else exit(UTFInvalid);
	end;

	function UTF8.Prev(const s: string; var pos: sint): UTFchar;
	begin
		while pos >= 2 do
		begin
			dec(pos);
			if ord(s[pos]) shr 6 <> %10 then exit(Peek(s, pos));
		end;
		result := UTFInvalid;
	end;

	function UTF8.Prev(const s: string; var pos: sint; out sym: UTFchar): UTFchar;
	begin
		result := Prev(s, pos);
		sym    := result;
	end;

	function UTF8.Peek(const s: string; pos: sint): UTFchar; begin result := Next(s, pos); end;
	function UTF8.Peek(const s: string; pos: sint; out len: size_t): UTFchar;
	var
		p: sint;
	begin
		p := pos;
		result := Next(s, p);
		len := p - pos;
	end;

	function UTF8.Peek(const s: string; pos: sint; out sym: UTFchar): UTFchar;
	begin
		result := Peek(s, pos);
		sym    := result;
	end;

	function UTF8.Peek(const s: string; pos: sint; out len: size_t; out sym: UTFchar): UTFchar;
	begin
		result := Peek(s, pos, len);
		sym    := result;
	end;

	function UTF8.CodepointToString(const char: UTFchar; syms: pChar): sint;
	begin
		case char of
			0 .. $7f:
				begin
					syms[0] := chr(char);
					result := 1;
				end;
			$80 .. $7ff:
				begin
					syms[0] := chr(%11000000 or (char shr 6));
					syms[1] := chr(%10000000 or (char and %00111111));
					result := 2;
				end;
			$800 .. $ffff:
				begin
					syms[0] := chr(%11100000 or (char shr 12));
					syms[1] := chr(%10000000 or ((char shr 6) and %00111111));
					syms[2] := chr(%10000000 or (char and %00111111));
					result := 3;
				end;
			$10000 .. $1fffff:
				begin
					syms[0] := chr(%11110000 or (char shr 18));
					syms[1] := chr(%10000000 or ((char shr 12) and %00111111));
					syms[2] := chr(%10000000 or ((char shr 6) and %00111111));
					syms[3] := chr(%10000000 or (char and %00111111));
					result := 4;
				end;
			else raise ToStringError(char, 'UTF-8');
		end;
	end;
	function UTF8.CodepointToString(const char: UTFchar): CharBytes; begin result[0] := chr(CodepointToString(char, @result[1])); end;

	function UTF8.IsEOL(s: pChar; len: size_t; out eolen: size_t): boolean;
	begin
		if (len = 0) or ((s[0] <> #13) and (s[0] <> #10)) then
		begin
			result := len = 0;
			eolen := 0;
		end else
		begin
			result := yes;
			eolen := 1 + uint((s[0] = #13) and (len > 1) and (s[1] = #10));
		end;
	end;

	function UTF8.ToStringError(const char: UTFchar; const what: string): Exception;
	begin
		result := Error('Не удалось преобразовать символ #' + ToString(char) + ' в ' + what + '.');
	end;

	function UTF16.CodepointToString(const char: UTFchar; syms: pWideChar): sint;
	var
		c2: uint16;
	begin
		case char of
			0 .. $ffff:
				begin
					syms[0] := widechar(char);
					result := 1;
				end;
			$10000 .. $10ffff:
				begin
					c2 := char - $10000;
					syms[0] := widechar(%1101100000000000 or (c2 and %0000001111111111));
					syms[1] := widechar(%1101110000000000 or ((c2 shr 10) and %0000001111111111));
					result := 2;
				end;
			else raise UTF8.ToStringError(char, 'UTF-16');
		end;
	end;

{$define intf := function ToString(const value: typ): string; begin System.str(value, result); end;} all_ints

	function ToString(buf: pChar; count: uint): string;
	begin
		SetLength(result, count * sizeof(buf^));
		memcpy(buf, pointer(result), count * sizeof(buf^));
	end;

	function ToString(buf: pWideChar; count: uint): widestring;
	begin
		SetLength(result, count * sizeof(buf^));
		memcpy(buf, pointer(result), count * sizeof(buf^));
	end;

	function StringRefCount(const s: string): PtrInt;
	begin
		if Assigned(pointer(s)) then result := (Hacks.PAnsiRec(s) - 1)^.ref else result := -1;
	end;

	// Ну тупые.
	function UTF8Encode(const s: widestring): rawbytestring;
	begin
		result := System.UTF8Encode(s);
		if Assigned(pointer(result)) then (Hacks.PAnsiRec(result) - 1)^.cpes.CodePage := CP_ACP;
	end;

	{$define impl:= {$ifndef np_cmp} {$define np_cmp := np} {$endif}
	begin {$ifdef prologue} prologue {$endif}
		result := ((ns) >= (np_cmp)) and (CompareByte((ps)^, (pp)^, (np) * sizeof(char)) = 0);
	end; {$undef ns} {$undef np} {$undef np_cmp} {$undef ps} {$undef pp} {$undef prologue}}
	function Prefixed(const prefix, s: string): boolean; {$define ns:=length(s)} {$define np:=length(prefix)} {$define ps:=pChar(s)} {$define pp:=pChar(prefix)} impl
	function Prefixed(const prefix, s: string; start: sint): boolean; {$define prologue:=Assert(start > 0, 'start = ' + ToString(start));}
		{$define ns:=length(s)} {$define np:=length(prefix)} {$define np_cmp:=length(prefix) + (start-1)}
		{$define ps:=pChar(s)+(start-1)} {$define pp:=pChar(prefix)} impl
	function Prefixed(const prefix: string; ps: pChar; ns: sint): boolean; {$define np:=length(prefix)} {$define pp:=pChar(prefix)} impl
	function Prefixed(pp: pChar; np: sint; ps: pChar; ns: sint): boolean; impl
{$undef impl}

	function CutPrefix(const prefix, s: string; rest: pString): boolean;
	begin
		result := Prefixed(prefix, s);
		if result then rest^ := Copy(s, length(prefix) + 1, length(s) - length(prefix)) else rest^ := s;
	end;

	function CutPrefix(const prefix, s: string): string;
	begin
		CutPrefix(prefix, s, @result);
	end;

	procedure ContinueString(var text: string; const value, sep: string);
	begin
		if value <> '' then
		begin
			if text <> '' then text += sep;
			text += value;
		end;
	end;

	function Continued(const text, sep, value: string): string;
	begin
		if (text <> '') and (value <> '') then
			result := text + sep + value
		else
			result := text + value;
	end;

	procedure Append(var strs: Strings; const s: string);
	begin
		SetLength(strs, length(strs) + 1);
		strs[High(strs)] := s;
	end;

	procedure Append(var strs: Strings; const s: array of string);
	var
		i: sint;
	begin
		SetLength(strs, length(strs) + length(s));
		for i := 0 to High(s) do strs[length(strs) - length(s) + i] := s[i];
	end;

	function MemoryChunk.Make(param: pointer; size: size_t; handler: HandlerProc): MemoryChunk;
	begin
		result.param   := param;
		result.size    := size;
		result.handler := handler;
	end;

	function MemoryChunk.Plain(data: pointer; size: size_t): MemoryChunk;
	begin
		result.param := data;
		result.size  := size;
		result.handler := nil;
	end;

	function OwnedMemoryChunkHandler(op: MemoryChunk.Operation; param: pointer): pointer;
	begin
		case op of
			MemoryChunk.GetData: result := param;
			MemoryChunk.Destroy: FreeMem(param);
			MemoryChunk.QueryWrite: result := param;
			else Assert(no);
		end;
	end;

	function MemoryChunk.TakeIt(data: pointer; size: size_t): MemoryChunk;
	begin
		result := Make(data, size, @OwnedMemoryChunkHandler);
	end;

	function MemoryChunk.data: pointer;
	begin
		if Assigned(handler) then result := handler(GetData, param) else result := param;
	end;

	procedure MemoryChunk.Done;
	begin
		if Assigned(param) then
		begin
			if Assigned(handler) then handler(Destroy, param);
			param := nil;
		end;
	end;

	function MemoryChunk.OK: boolean;
	begin
		result := Assigned(param);
	end;

	function MemoryChunk.AsPlain: MemoryChunk;
	begin
		result := Plain(data, size);
	end;

	procedure MemoryChunk.RequestWrite;
	var
		n: MemoryChunk;
	begin
		if not Assigned(handler) or not Assigned(handler(QueryWrite, param)) then
		begin
			n := MemoryChunk.TakeIt(GetMem(size), size);
			memcpy(data, n.data, size);
			Done;
			self := n;
		end;
	end;

	function StringMemoryChunkHandler(op: MemoryChunk.Operation; param: pointer): pointer;
	begin
		case op of
			MemoryChunk.GetData: result := param;
			MemoryChunk.Destroy: System.Finalize(string(param));
			MemoryChunk.QueryWrite: begin UniqueString(string(param)); result := param; end;
			else Assert(no);
		end;
	end;

	operator :=(const s: string): MemoryChunk;
	begin
		System.Initialize(string(result.param)); string(result.param) := s;
		result.size := length(s) * sizeof(char);
		result.handler := @StringMemoryChunkHandler;
	end;

	function ExceptionRef.Create(const message: string; inner: pExceptionRef): pExceptionRef;
	begin
		new(result);
		result^.message := message;
		if Assigned(inner) then result^.inner := inner^.Ref else result^.inner := nil;
		result^.refcount := 0;
	{$ifdef Debug} result^.logged := no; {$endif}
	end;

	function ExceptionRef.Ref: pExceptionRef;
	begin
		inc(refcount);
		result := @self;
	end;

	procedure ExceptionRef.Kill;
	begin
		dec(refcount);
		if refcount = 0 then
		begin
			if Assigned(inner) then inner^.Kill;
			dispose(@self);
		end;
	end;

	constructor Exception.Create(xc: pExceptionRef);
	begin
		inherited Create;
		ref := xc^.Ref;
	end;

	constructor Exception.Create(const newMessage: string);
	begin
		Create(newMessage, ExtractInnerExceptionRef);
	end;

	constructor Exception.Create(const newMessage: string; newInner: pExceptionRef);
	begin
		Create(ExceptionRef.Create(newMessage, newInner));
	end;

	constructor Exception.Acquire;
	begin
		Assert(Assigned(RaiseList));
		Create(ExtractInnerExceptionRef);
	end;

	constructor Exception.CreateBlank;
	begin
		Create('(Вы не должны этого видеть.)');
	end;

	destructor Exception.Destroy;
	begin
		ref^.Kill;
		inherited Destroy;
	end;

	function Exception.Human: string;
	var
		t: pExceptionRef;
	begin
		result := ref^.message;
		t := ref^.inner;
		while Assigned(t) do
		begin
			result += EOL + t^.message;
			t := t^.inner;
		end;
	end;

	class function Exception.Current: System.TObject;
	begin
		if Assigned(RaiseList) then
			result := RaiseList[0].FObject
		else
			USystem.Fatal('Exception.Current вызвана вне блока обработки исключения.');
	end;

	function Exception.RawMessage: string;
	begin
		result := ref^.message;
	end;

	class function Exception.Message: string; begin result := Message(Current); end;
	class function Exception.Message(E: System.TObject): string;
	var
		err: Exception absolute E;
	{$ifdef Debug} exc: SysUtils.Exception absolute E; {$endif}
	begin
		if E is Exception then result := err.Human else
	{$ifdef Debug} if E is SysUtils.Exception then result := exc.ClassName + ': ' + exc.Message else {$endif}
		if E is System.TObject then result := 'Исключение ' + e.ClassName + '!' else
			result := 'Неизвестная ошибка.';
	end;

	class procedure Exception.Show;                     begin Show(Current); end;
	class procedure Exception.Show(E: System.TObject);  begin Error.Show(Message(E)); end;
	class procedure Exception.Fatal;                    begin Fatal(Current); end;
	class procedure Exception.Fatal(E: System.TObject); begin USystem.Fatal(Message(E)); end;

{$ifdef Debug}
	class procedure Exception.Log;                      begin Log(Current); end;
	class procedure Exception.Log(E: System.TObject);
	begin
		if E is Exception then
		begin
			if Exception(E).ref^.logged then exit;
			Exception(E).ref^.logged := yes;
		end;
		SingletonLock.Enter;
		if Assigned(logger.proc) then logger.proc(Message(E)) else {$ifdef Debug} Show(E) {$endif};
		SingletonLock.Leave;
	end;
	function Exception.Logged: boolean; begin result := ref^.logged; end;
{$endif}
{$ifdef use_console}
	class procedure Exception.Print;                    begin Print(Current); end;
	class procedure Exception.Print(E: System.TObject); begin Con.WriteLine(Message(E)); end;
	class procedure Exception.More;                     begin More(Current); end;
	class procedure Exception.More(E: System.TObject);  begin Con.WriteLine(Message(E)); Con.Write('<ENTER>'); Con.ReadLine; end;
{$endif}

	class function Exception.ExtractInnerExceptionRef: pExceptionRef;
	var
		raised: System.TObject;
	begin
		if Assigned(RaiseList) then
		begin
			raised := Current;
			if raised is Exception then
				result := Exception(raised).ref
			else
				result := ExceptionRef.Create(Message(raised), nil);
		end else
			result := nil;
	end;

{$ifdef Debug}
	class procedure Exception.SetLogger(logProc: LogProc);
	begin
		SingletonLock.Enter;
		logger.proc  := logProc;
		SingletonLock.Leave;
	end;
{$endif}

	function Error(const message: string): Exception;
	begin
		result := Exception.Create(message);
	end;

{$ifdef Windows}
	function TaskDialogCallback(hwnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM; lpRefData: LONG_PTR): HRESULT; stdcall;
		procedure OpenHyperlink(const link: widestring);
		var
			ci: HRESULT;
			se: Win.SHELLEXECUTEINFO;
		begin
			Win.EnsureCoInit;
			Win.EnsureShellExecuteExW;

			ci := Win.ole32.CoInitializeEx(nil, Win.COINIT_MULTITHREADED or Win.COINIT_DISABLE_OLE1DDE);
			if (ci <> S_OK) and (ci <> S_FALSE) then
			{$ifdef Debug} Warning.Show('Не удаётся инициализировать COM (' + ToString(ci) + ').') {$endif}
			else
			begin
				Zero(@se, sizeof(se));
				se.cbSize := sizeof(se);
				se.fMask := Win.SEE_MASK_UNICODE or Win.SEE_MASK_NOASYNC {$ifNdef Debug} or Win.SEE_MASK_FLAG_NO_UI {$endif};
				se.lpVerb := nil;
				se.lpFile := pWideChar(link);
				if not Win.shell32.ShellExecuteExW(se) then ; // оно само покажет ошибку, если не задана SEE_MASK_FLAG_NO_UI.
				Win.ole32.CoUninitialize;
			end;
		end;
	begin
		unused_args hwnd _ wParam _ lpRefData end_list
		case msg of
			Win.TDN_HYPERLINK_CLICKED: OpenHyperlink(pWideChar(NULL + lParam));
		end;
		result := S_OK;
	end;

	function PrepareTaskDialogLink(const link: string; param: pointer): string;
	begin
		unused_args param end_list
		result := '<a href="' + link + '">' + CutPrefix('http://', link) + '</a>';
	end;
{$endif}

	function Dialog.Show: AnswerEnum;
	{$ifdef Windows}
		procedure CommonIDToAnswer(id: uint; var answer: AnswerEnum);
		begin
			case id of
				IDYES: answer := TaskYes;
				IDOK:  answer := TaskOk;
				IDNO:  answer := TaskNo;
			end;
		end;

		procedure ShowWithMessageBox(int: pInternal; var answer: AnswerEnum);
			procedure AppendVariants(const buttons: array of string; const mb: Windows.UINT; var text: string; var flags: Windows.UINT);
			var
				i: sint;
			begin
				flags := flags or mb;
				for i := 0 to High(buttons) do
				begin
					if text <> '' then begin text += EOL; if i = 0 then text += EOL; end;
					if length(buttons) = length(int^.variants) then
					begin
						text += buttons[i] + ' — ' + Win.LowerCaseFirst(int^.variants[i]);
						if Index(EOL, pChar(int^.variants[i]), length(int^.variants[i])) >= 0 then text += EOL;
					end else
						text += buttons[i];
				end;
			end;

		var
			text, title: string;
			flags: Windows.UINT;
			ret: cint;
		begin
			flags := MB_SYSTEMMODAL or MB_TOPMOST or MB_SETFOREGROUND;

			case int^.sem of
				Semantics.Error:   flags := flags or MB_ICONERROR;
				Semantics.Warning: flags := flags or MB_ICONWARNING;
				Semantics.Info:    flags := flags or MB_ICONINFORMATION;
			end;

			text := int^.text;
			case int^.ans of
				Answers.OkCancel: flags := flags or MB_OKCANCEL;
				Answers.YesNo:    flags := flags or MB_YESNO;
				Answers.YesNoCancel: flags := flags or MB_YESNOCANCEL;
				Answers.UserVariants:
					begin
						case length(int^.variants) of
							1: AppendVariants(['OK'], MB_OK, text, flags);
							2: AppendVariants(['OK', 'Отмена'], MB_OKCANCEL, text, flags);
							3: AppendVariants(['«Да»', '«Нет»', '«Отмена»'], MB_YESNOCANCEL, text, flags);
							else AppendVariants(['(варианты ответа недоступны)'], MB_OK, text, flags);
						end;
					end;
				else flags := flags or MB_OK;
			end;

			ContinueString(text, int^.footer, EOL + '------' + EOL);
			ContinueString(text, int^.expanded, EOL + '------' + EOL);

			if int^.title <> '' then title := int^.title else title := Folder.Filename(ExecFileName);

			ret := Windows.MessageBoxW(0, pWideChar(UTF8Decode(text)), pWideChar(UTF8Decode(title)), flags);
			if int^.ans = Answers.UserVariants then
				case length(int^.variants) of
					1: if ret = IDOK then answer := TaskV1;
					2: case ret of IDOK: answer := TaskV1; IDCANCEL: answer := TaskV2; end;
					3: case ret of IDYES: answer := TaskV1; IDNO: answer := TaskV2; IDCANCEL: answer := TaskV3; end;
				end
			else
				CommonIDToAnswer(ret, answer);
		end;

		function ShowWithTaskDialog(int: pInternal; var answer: AnswerEnum): boolean;
		writeable_const_ Recursion: uint = 0; _end
		const
			FirstUserButtonID = IDHELP + 1;
		var
			nw: sint;
			w: array of widestring;

			function AddW(const s: string): pWideChar;
			begin
				if s = '' then exit(nil);
				w[nw] := UTF8Decode(s);
				result := pWideChar(w[nw]);
				inc(nw);
			end;

		var
			d: Win.TASKDIALOGCONFIG;
			buttons: array of Win.TASKDIALOG_BUTTON;
			i: sint;
			status: HRESULT;
			button: cint;
		begin
			result := no;
			if Recursion > 0 then exit;
			inc(Recursion);
			try
				if not Win.EnsureTaskDialogIndirect then exit;
				nw := 0;
				SetLength(w, 1 + ord(int^.title <> '') + ord(int^.text <> '') + ord(int^.footer <> '') + ord(int^.expanded <> '') +
					length(int^.variants));

				Zero(@d, sizeof(d));
				d.cbSize         := sizeof(d);
				d.dwFlags        := Win.TDF_ALLOW_DIALOG_CANCELLATION or Win.TDF_ENABLE_HYPERLINKS or Win.TDF_SIZE_TO_CONTENT or
					Win.TDF_EXPAND_FOOTER_AREA;
				d.pszWindowTitle := AddW(int^.title);
				d.pszContent     := AddW(ReplaceLinks(int^.text, @PrepareTaskDialogLink, nil));
				d.pszFooter      := AddW(ReplaceLinks(int^.footer, @PrepareTaskDialogLink, nil));
				d.pszExpandedInformation := AddW(ReplaceLinks(int^.expanded, @PrepareTaskDialogLink, nil));
				d.pfCallback     := @TaskDialogCallback;

				case int^.sem of
					Semantics.Error:   d.mainIcon.pszMainIcon := Win.TD_ERROR_ICON;
					Semantics.Warning: d.mainIcon.pszMainIcon := Win.TD_WARNING_ICON;
					Semantics.Info:    d.mainIcon.pszMainIcon := Win.TD_INFORMATION_ICON;
				end;

				case int^.ans of
					Answers.OkCancel: d.dwCommonButtons := Win.TDCBF_OK_BUTTON or Win.TDCBF_CANCEL_BUTTON;
					Answers.YesNo: d.dwCommonButtons := Win.TDCBF_YES_BUTTON or Win.TDCBF_NO_BUTTON;
					Answers.YesNoCancel: d.dwCommonButtons := Win.TDCBF_YES_BUTTON or Win.TDCBF_NO_BUTTON or Win.TDCBF_CANCEL_BUTTON;

					Answers.UserVariants:
						begin
							d.dwFlags := d.dwFlags or Win.TDF_USE_COMMAND_LINKS;
							SetLength(buttons, length(int^.variants));
							for i := 0 to High(int^.variants) do
							begin
								buttons[i].nButtonID     := FirstUserButtonID + i;
								buttons[i].pszButtonText := AddW(int^.variants[i]);
							end;

							d.cButtons := length(buttons);
							d.pButtons := Win.PTASKDIALOG_BUTTON(buttons);
						end;

					else d.dwCommonButtons := Win.TDCBF_OK_BUTTON;
				end;

				status := Win.comctl32.TaskDialogIndirect(d, @button, {radioButton} nil, {verification} nil);
				if status <> S_OK then
				begin
					Text('(Не удалось отобразить этот диалог через TaskDialog. Код ' + ToString(status) + ').');
					exit;
				end;

				case button of
					FirstUserButtonID + 0 .. FirstUserButtonID + (ord(LastVariantAnswer) - ord(FirstVariantAnswer)):
						answer := AnswerEnum(ord(FirstVariantAnswer) + (button - FirstUserButtonID));
					else CommonIDToAnswer(button, answer);
				end;

				result := yes;
			finally
				dec(Recursion);
			end;
		end;
	{$endif}
	begin
		try
			result := TaskCancel;
		{$ifdef Windows}
			if int^.forcePlain or not ShowWithTaskDialog(int, result) then ShowWithMessageBox(int, result);
		{$else} {$error TaskDialog.Show unimplemented} {$endif}
		finally
			dispose(int);
		end;
	end;

	function Dialog.Show(const withText: string): AnswerEnum; begin result := Text(withText).Show; end;
	function Dialog.Text(const value: string): Dialog;        begin result := SetString(int^.text, value, EOL + EOL); end;
	function Dialog.Title(const value: string): Dialog;       begin result := SetString(int^.title, value); end;
	function Dialog.Footer(const value: string): Dialog;      begin result := SetString(int^.footer, value, EOL + EOL); end;
	function Dialog.Expanded(const value: string): Dialog;    begin result := SetString(int^.expanded, value, EOL + EOL); end;
	function Dialog.Variant(const value: string): Dialog;
	begin
		SetLength(int^.variants, length(int^.variants) + 1);
		result := SetString(int^.variants[High(int^.variants)], value).SetAnswers(int^.ans, Answers.UserVariants);
	end;

	function Dialog.OkCancel: Dialog;    begin result := SetAnswers(int^.ans, Answers.OkCancel); end;
	function Dialog.YesNo: Dialog;       begin result := SetAnswers(int^.ans, Answers.YesNo); end;
	function Dialog.YesNoCancel: Dialog; begin result := SetAnswers(int^.ans, Answers.YesNoCancel); end;
	function Dialog.ContinueOrStopVariants: Dialog; begin result := Variant('Продолжить').Variant('Остановить выполнение'); end;
	function Dialog.ForcePlain: Dialog;  begin int^.forcePlain := yes; result := self; end;

	function Dialog.Create(sem: Semantics): Dialog;
	begin
		new(result.int);
		result.int^.ans := Answers.OK;
		result.int^.sem := sem;
		result.int^.forcePlain := no;
		result := result.Footer(AppInfo.Feedback);
	end;

	function Dialog.SetString(var s: string; const value: string; const sep: string = ''): Dialog;
	begin
		if sep <> '' then ContinueString(s, value, sep) else s := value;
		result := self;
	end;

	function Dialog.SetAnswers(out ans: Answers; value: Answers): Dialog;
	begin
		ans := value;
		result := self;
	end;

	function Dialog.ReplaceLinks(const s: string; replace: LinkReplaceFunc; param: pointer): string;
	const
		ProtocolSample = '://';
		WwwSample  = 'www.';

		function ReverseScanForProtocolStart(const s: string; p, start: sint): sint;
		begin
			Assert(Prefixed(ProtocolSample, s, p));
			result := p;
			while (result > start) and (s[result - 1] in ['a' .. 'z', 'A' .. 'Z', '-', '+', '.', '0' .. '9']) do
				dec(result);
			while (result < p) and (s[result] in ['-', '+', '.', '0' .. '9']) do inc(result);
		end;

		function ScanForLinkEnd(const s: string; p, pstart: sint): sint;
		var
			paren: boolean;
			i: sint;
		begin
			result := p;
			while (result <= length(s)) and not (s[result] in [' ', '"', '<', '>', #0 .. #31]) do inc(result);
			while (result > p) and (s[result - 1] in [' ', ',', ';']) do dec(result);

			if (result > p) and (s[result - 1] = ')') then
			begin
				paren := no;
				for i := pstart - 1 downto 1 do
				begin
					if s[i] = '(' then begin paren := yes; break; end;
					if s[i] = ')' then break;
				end;
				if paren then dec(result);
			end;
		end;

	var
		start, protocolStart, ed, p: sint;
	begin
		result := '';
		start := 1;
		p     := 1;
		while p <= length(s) do
		begin
			if Prefixed(ProtocolSample, s, p) then
			begin
				protocolStart := max(start, ReverseScanForProtocolStart(s, p, start));
				if (protocolStart < p) and ((protocolStart = 1) or not (s[protocolStart - 1] in ['a' .. 'z', 'A' .. 'Z'])) then
				begin
					ed := ScanForLinkEnd(s, p + length(ProtocolSample), protocolStart);
					if ed > p + length(ProtocolSample) then
					begin
						result += Copy(s, start, protocolStart - start) + replace(Copy(s, protocolStart, ed - protocolStart), param);
						start := ed; p := start;
						continue;
					end;
				end;
			end;

			if Prefixed(WwwSample, s, p) and ((p = 1) or not (s[p - 1] in ['a' .. 'z', 'A' .. 'Z'])) then
			begin
				ed := ScanForLinkEnd(s, p + length(WwwSample), p);
				if ed > p + length(WwwSample) then
				begin
					result += Copy(s, start, p - start) + replace(Copy(s, p, ed - p), param);
					start := ed; p := start;
					continue;
				end;
			end;

			inc(p);
		end;
		if start = 1 then result := s else result += Copy(s, start, p - start);
	end;

	function Error: Dialog;   begin result := Dialog.Create(Dialog.Semantics.Error); end;
	function Warning: Dialog; begin result := Dialog.Create(Dialog.Semantics.Warning); end;
	function Info: Dialog;    begin result := Dialog.Create(Dialog.Semantics.Info); end;
	function Message: Dialog; begin result := Dialog.Create(Dialog.Semantics.Message); end;

	function DateTime.YMDHMSMS(y, mo, d, h, mi, s, ms: uint): DateTime;
	begin
		result.year  := y;
		result.month := mo;
		result.day   := d;
		result.hour  := h;
		result.min   := mi;
		result.sec   := s;
		result.msec  := ms;
	end;

	function DateTime.GetLocal: DateTime;
{$ifdef Windows}
	var
		time: Windows.SYSTEMTIME;
	begin
		GetLocalTime((@time)^);
		result := Win.ToDateTime(time);
	end;
{$else} {$error DateTime.GetLocal unimplemented unimplemented} {$endif}

	function DateTime.Validate: boolean;
		function LeapYear(year: uint): boolean;
		begin
			result := ((year mod 4 = 0) and not (year mod 100 = 0)) or (year mod 400 = 0);
		end;
	const
		DaysInMonth: array[0 .. 11] of uint = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
	begin
		result := (month < 12) and (day < DaysInMonth[month] + uint(LeapYear(year) and (month = 1))) and (hour < 24)
				and (min < 60) and (sec < 60) and (msec < 1000);
	end;

	function DateTime.ToCode: string;
		function Pad(x: uint; n: uint): string;
		begin
			result := ToString(x);
			while uint(length(result)) < n do result := '0' + result;
		end;
	begin
		result := ToString(year) + Pad(1 + month, 2) + Pad(1 + day, 2) + '-' + Pad(hour, 2) + Pad(min, 2) + Pad(sec, 2);
	end;

	function DateTime.Start: DateTime;
{$ifdef Windows}
	var
		creationUtc, creationLocal, unused: Windows.FILETIME;
		creationSystem: Windows.SYSTEMTIME;
	begin
		if not GetProcessTimes(GetCurrentProcess, (@creationUtc)^, (@unused)^, (@unused)^, (@unused)^) or
			not FileTimeToLocalFileTime(creationUtc, (@creationLocal)^) or
			not FileTimeToSystemTime(creationLocal, (@creationSystem)^) then raise Win.OperationFailed('получить дату запуска процесса');
		result := Win.ToDateTime(creationSystem);
	end;
{$else} {$error DateTime.Start unimplemented} {$endif}

	procedure Thread.Body.Run(param: pointer);
	begin
		case proc.sig of
			Signature.Parametrized: proc.para(param);
			Signature.Unparametrized: proc.unpara();
			else Assert(no);
		end;
	end;

	procedure Thread.Body.ShowFatal(const where: string);
	begin
		Fatal(Exception.Message + EOL + '(' + where + ').');
	end;

	function Thread.Invalid: Thread;
	begin
		result.nameOf := '';
		result.handle := 0;
	{$ifdef Debug}
		result.guard := nil;
		{$ifdef Windows} result.sysId := 0; {$endif}
	{$endif}
	end;

	function Thread.OK: boolean;
	begin
		result := handle <> 0;
	end;

	procedure Thread.Close;
	begin
		if handle <> 0 then
		begin
			Join;
		{$ifdef Windows} CloseHandle(handle);
		{$else} {$error Thread.Close unimplemented}
		{$endif}
		end;
	{$ifdef Debug} FreeMem(guard); {$endif}
		self := Invalid;
	end;

	procedure SetupSignals; forward;

	procedure RunThread(t: pThread);
	begin
		SetupSignals;
		try
			t^.proc.Run(t^.param);
		except
			on TerminateThread do {nothing};
			else t^.proc.ShowFatal('в потоке ' + t^.Human(FullOf));
		end;
	end;

{$ifdef Windows}
	function ThreadProc(param: pointer): dword; stdcall;
	begin
		RunThread(param);
		result := 0;
	end;
{$else} {$error ThreadProc unimplementeD} {$endif}

	procedure Thread.Start(const nameOf: string; out thrd: Thread; const proc: Body; param: pointer);
	const
		STACK_SIZE_PARAM_IS_A_RESERVATION = $00010000;
	{$ifdef Windows} {$ifdef Debug} var tid: dword; {$endif} {$endif}
	begin
		thrd := Invalid;
		try
			thrd.nameOf := nameOf;
		{$ifdef Debug} thrd.guard := GetMem(1); {$endif}
			thrd.stackSize := DefaultStackSize;
			thrd.proc := proc;
			thrd.param := param;

		{$ifdef Windows}
			thrd.handle := CreateThread(nil, thrd.stackSize, @ThreadProc, @thrd,
				STACK_SIZE_PARAM_IS_A_RESERVATION {$ifdef Debug} or CREATE_SUSPENDED {$endif}, {$ifdef Debug} pdword(@tid)^ {$else} dword(nil^) {$endif});
			if thrd.handle = 0 then
				raise Win.OperationFailed('создать поток ' + thrd.Human(FullOf));
		{$ifdef Debug} thrd.sysId := tid; ResumeThread(thrd.handle); {$endif}
		{$else} {$error Thread.Start unimplemented} {$endif}
		except
			thrd.Close;
			raise;
		end;
	end;

	function Thread.Running: boolean;
	var
		code: dword;
	begin
		if handle = 0 then exit(no);
	{$ifdef Windows}
		code := 0;
		result := GetExitCodeThread(handle, code) and (code = Win.STILL_ALIVE);
	{$else} {$error Thread.Running unimplemented} {$endif}
	end;

	procedure Thread.Join;
	begin
		if handle <> 0 then
			WaitForSingleObject(handle, Windows.INFINITE);
	end;

	procedure Thread.Sleep(const ms: uint);
{$ifdef Windows}
	begin
		Windows.Sleep(ms);
	end;
{$else} {$error Thread.Sleep unimplemented} {$endif}

	procedure Thread.Sleep(const time: Ticks);
	begin
		Sleep(time.ToIMilliseconds);
	end;

	function Thread.Current: ID;
	begin
		result := System.ThreadID;
	end;

	function Thread.Human(format: HumanFormat): string;
	begin
		if (format = FullOf) and (nameOf <> '') then result := nameOf + ' ' else result := '';
		result += '(';
		if handle <> 0 then
		begin
			result += 'handle = ' + ToString(handle);
		{$if defined(Windows) and defined(Debug)} result += ', id = ' + ToString(sysId); {$endif}
		end else
			result += '---';
		result += ')';
	end;

	procedure Thread.TerminateSelf;
	begin
		raise TerminateThread.Create;
	end;

	operator :=(const proc: Thread.Body.ParametrizedSignature): Thread.Body; begin result.proc.sig := result.Signature.Parametrized; result.proc.para := proc; end;
	operator :=(const proc: Thread.Body.UnparametrizedSignature): Thread.Body; begin result.proc.sig := result.Signature.Unparametrized; result.proc.unpara := proc; end;

	procedure RecursiveThreadLock.Init;
	begin
	{$ifdef Debug} guard := GetMem(1); {$endif}
	{$ifdef Windows}
		if Assigned(Win.CSX.InitializeCriticalSectionEx) then
		begin
			Win.CSX.InitializeCriticalSectionEx(cs, 0, 0 {$ifndef Debug} or Win.CRITICAL_SECTION_NO_DEBUG_INFO {$endif});
			exit;
		end;
	{$endif}
		InitCriticalSection(cs);
	{$ifdef Windows} Assert(cs.OwningThread = 0); {$endif}
	end;

	procedure RecursiveThreadLock.Done;
	begin
		if not Valid then exit;
	{$ifdef Windows} Assert(cs.OwningThread = 0); {$endif}
		DoneCriticalSection(cs);
	{$ifdef Debug} FreeMem(guard); {$endif}
		Invalidate;
	end;
{$define classname := RecursiveThreadLock} {$define pSelf := pRecursiveThreadLock} {$include dyn_obj.pp.inc}

	procedure RecursiveThreadLock.Invalidate; begin pPointer(@cs)^ := IMPOSSIBLE_FOOTPRINT; end;
	function RecursiveThreadLock.Valid: boolean; begin result := pPointer(@cs)^ <> IMPOSSIBLE_FOOTPRINT; end;

	function RecursiveThreadLock.TryEnter: boolean;
	begin
		result := System.TryEnterCriticalSection(cs) <> 0;
	{$ifdef Windows} Assert(not result or (cs.OwningThread = Thread.Current)); {$endif}
	end;

	procedure RecursiveThreadLock.Enter;
	begin
		EnterCriticalSection(cs);
	{$ifdef Windows} Assert(cs.OwningThread = Thread.Current); {$endif}
	end;

	procedure RecursiveThreadLock.Leave;
	begin
		LeaveCriticalSection(cs);
	end;

	function RecursiveThreadLock.TryEnterShared: boolean; begin result := TryEnter; end;
	procedure RecursiveThreadLock.EnterShared;            begin Enter; end;
	procedure RecursiveThreadLock.LeaveShared;            begin Leave; end;
	function RecursiveThreadLock.AcquiredAssert: boolean; begin result := {$ifdef Windows} cs.OwningThread = Thread.Current {$else} yes {$endif}; end;

	procedure ThreadLock.Init;
	begin
	{$ifdef Debug} guard := GetMem(1); owner := 0; {$endif}
	{$ifdef Windows}
		if WinSRWSupported then
			Win.SRW.InitializeSRWLock(Win.SRWLOCK(ptr))
		else
			ptr := RecursiveThreadLock.Create;
	{$else} {$error ThreadLock.Init unimplemented} {$endif}
	end;

	procedure ThreadLock.Done;
	begin
	{$ifdef Windows}
		if not WinSRWSupported then pRecursiveThreadLock(ptr)^.Free;
		ptr := nil;
	{$else} {$error ThreadLock.Done unimplemented} {$endif}
	{$ifdef Debug} Assert(owner = 0, 'owner = ' + ToString(owner)); FreeMem(guard); {$endif}
	end;
{$define classname := ThreadLock} {$define pSelf := pThreadLock} {$include dyn_obj.pp.inc}

	procedure ThreadLock.Invalidate; begin ptr := IMPOSSIBLE_PTR; end;
	function ThreadLock.Valid: boolean; begin result := ptr <> IMPOSSIBLE_PTR; end;

	function ThreadLock.TryEnter: boolean;
	begin
	{$ifdef Windows}
		if WinSRWSupported then
			result := Win.SRW.TryAcquireSRWLockExclusive(Win.SRWLOCK(ptr)) <> 0
		else
			result := pRecursiveThreadLock(ptr)^.TryEnter;
	{$else} {$error ThreadLock.TryEnter unimplemented} {$endif}
	{$ifdef Debug} if result then owner := Thread.Current; {$endif}
	end;

	procedure ThreadLock.Enter;
	begin
	{$ifdef Windows}
		if WinSRWSupported then
			Win.SRW.AcquireSRWLockExclusive(Win.SRWLOCK(ptr))
		else
			pRecursiveThreadLock(ptr)^.Enter;
	{$else} {$error ThreadLock.Enter unimplemented} {$endif}
	{$ifdef Debug} Assert(owner = 0, 'owner = ' + ToString(owner)); owner := Thread.Current; {$endif}
	end;

	procedure ThreadLock.Leave;
	begin
	{$ifdef Debug} Assert(owner = Thread.Current, 'owner = ' + ToString(owner)); owner := 0; {$endif}
	{$ifdef Windows}
		if WinSRWSupported then
			Win.SRW.ReleaseSRWLockExclusive(Win.SRWLOCK(ptr))
		else
			pRecursiveThreadLock(ptr)^.Leave;
	{$else} {$error ThreadLock.Leave unimplemented} {$endif}
	end;

	function ThreadLock.TryEnterShared: boolean;
{$ifdef Windows}
	begin
		if WinSRWSupported then
			result := Win.SRW.TryAcquireSRWLockShared(Win.SRWLOCK(ptr)) <> 0
		else
			result := pRecursiveThreadLock(ptr)^.TryEnterShared;
	end;
{$else} {$error ThreadLock.TryEnterShared unimplemented} {$endif}

	procedure ThreadLock.EnterShared;
{$ifdef Windows}
	begin
		if WinSRWSupported then
			Win.SRW.AcquireSRWLockShared(Win.SRWLOCK(ptr))
		else
			pRecursiveThreadLock(ptr)^.EnterShared;
	end;
{$else} {$error ThreadLock.Enter unimplemented} {$endif}

	procedure ThreadLock.LeaveShared;
{$ifdef Windows}
	begin
		if WinSRWSupported then
			Win.SRW.ReleaseSRWLockShared(Win.SRWLOCK(ptr))
		else
			pRecursiveThreadLock(ptr)^.LeaveShared;
	end;
{$else} {$error ThreadLock.Leave unimplemented} {$endif}

	function ThreadLock.AcquiredAssert: boolean;
	begin
		result := {$ifdef Debug} owner = Thread.Current {$else} yes {$endif};
	end;

	function ThreadLockReference.Make(newNonrec: pThreadLock; newRec: pRecursiveThreadLock): ThreadLockReference;
	begin
		result.nonrec := newNonrec;
		result.rec    := newRec;
	end;

	procedure ThreadLockReference.Enter;
	begin
		if Assigned(nonrec) then nonrec^.Enter else rec^.Enter;
	end;

	procedure ThreadLockReference.Leave;
	begin
		if Assigned(nonrec) then nonrec^.Leave else rec^.Leave;
	end;

	function ThreadLockReference.AcquiredAssert: boolean;
	begin
		if Assigned(nonrec) then result := nonrec^.AcquiredAssert else result := rec^.AcquiredAssert;
	end;

	operator :=(var lock: ThreadLock): ThreadLockReference;          begin result := ThreadLockReference.Make(@lock, nil); end;
	operator :=(var lock: RecursiveThreadLock): ThreadLockReference; begin result := ThreadLockReference.Make(nil, @lock); end;

	procedure ThreadEvent.Init(flags: InitFlags = []);
{$ifdef Windows} var cve: Win.pEventOnCV absolute union.cvep; {$endif}
	begin
		multiwaitable := AllowMultiWait in flags;
	{$ifdef Windows}
		if WinCVSupported and not multiwaitable then
		begin
			new(cve);
			cve^.Init(flags);
		end else
			union.handle := CreateEventW(nil, not (AutoReset in flags), InitiallySet in flags, nil);
	{$else} {$error ThreadEvent.Init unimplemented} {$endif}
	end;

	procedure ThreadEvent.Done;
{$ifdef Windows} var cve: Win.pEventOnCV absolute union.cvep; {$endif}
	begin
	{$ifdef Windows}
		if WinCVSupported and not multiwaitable then
		begin
			Assert(Assigned(cve));
			cve^.Done;
			dispose(cve);
		end else
		begin
			Assert(union.handle <> 0);
			CloseHandle(union.handle);
		end;
	{$else} {$error ThreadEvent.Done unimplemented} {$endif}
	end;
{$define classname := ThreadEvent} {$define pSelf := pThreadEvent}
{$define constructor_args := flags: InitFlags = []} {$define pass_constructor_args := flags}
{$include dyn_obj.pp.inc}

	procedure ThreadEvent.SetEvent;
{$ifdef Windows}
	var
		cve: Win.pEventOnCV absolute union.cvep;
	begin
		if WinCVSupported and not multiwaitable then
			cve^.SetEvent
		else
			Windows.SetEvent(union.handle);
	end;
{$else} {$error ThreadEvent.SetEvent unimplemented} {$endif}

	procedure ThreadEvent.ResetEvent;
{$ifdef Windows}
	var
		cve: Win.pEventOnCV absolute union.cvep;
	begin
		if WinCVSupported and not multiwaitable then
			cve^.ResetEvent
		else
			Windows.ResetEvent(union.handle)
	end;
{$else} {$error ThreadEvent.ResetEvent unimplemented} {$endif}

	procedure ThreadEvent.Wait;
	begin
		Wait(not uint(0));
	end;

	function ThreadEvent.Wait(timeoutMs: uint): boolean;
{$ifdef Windows}
	var
		cve: Win.pEventOnCV absolute union.cvep;
		r: dword;
	begin
		if WinCVSupported and not multiwaitable then
			cve^.Wait(timeoutMs)
		else
		begin
			r := Windows.WaitForSingleObject(union.handle, Win.TimeoutOrInfinite(timeoutMs));
			if r = WAIT_OBJECT_0 then result := yes
			else if (r = WAIT_TIMEOUT) and (0 <> not timeoutMs) then result := no
			else raise Win.FunctionFailed('WaitForSingleObject');
		end;
	end;
{$else} {$error ThreadEvent.Wait(timeout) unimplemented} {$endif}

	function ThreadEvent.State: boolean;
{$ifdef Windows}
	var
		cve: Win.pEventOnCV absolute union.cvep;
	begin
		if WinCVSupported and not multiwaitable then
			result := cve^.GetState
		else
			case Windows.WaitForSingleObject(union.handle, 0) of
				WAIT_OBJECT_0: result := yes;
				WAIT_TIMEOUT: result := no;
				else raise Win.FunctionFailed('WaitForSingleObject(0)');
			end;
	end;
{$else} {$error ThreadEvent.State unimplemented} {$endif}

	function ThreadEvent.WaitAny(const ev: array of pThreadEvent; flags: WaitFlags = []): sint;
	begin
		result := WaitAny(ev, not uint(0), flags);
	end;

	function ThreadEvent.WaitAny(const ev: array of pThreadEvent; timeoutMs: uint; flags: WaitFlags = []): sint;
{$ifdef Windows}
	const ZeroWaitObject0 = WAIT_OBJECT_0 = 0;
	type ZeroWaitObject0IfGp = array[0 .. sint(ZeroWaitObject0)] of pointer;
	var
		h: tWOHandleArray;
		i: sint;
		r, winTimeout: dword;
		succeed: boolean;
	begin
		Assert(length(ev) + 1 <= length(h));
		for i := 0 to High(ev) do
		begin
			Assert(ev[i]^.multiwaitable, 'WaitAny на событии, инициализированном без AllowMultiWait');
			h[i] := ev[i]^.union.handle;
		end;
		winTimeout := Win.TimeoutOrInfinite(timeoutMs);
		if OrMessage in flags then
			r := MsgWaitForMultipleObjectsEx(length(ev), h, winTimeout, QS_ALLEVENTS, 0)
		else
			r := WaitForMultipleObjects(length(ev), @h, no, winTimeout);

		succeed := {$if sizeof(ZeroWaitObject0IfGp) <= sizeof(pointer)} (r >= WAIT_OBJECT_0) and {$endif} (r < WAIT_OBJECT_0 + uint(length(ev)));
		if succeed then result := r - WAIT_OBJECT_0 else
		if (0 <> not timeoutMs) and (r = WAIT_TIMEOUT) then result := -1 else
		if (OrMessage in flags) and (r = WAIT_OBJECT_0 + uint(length(ev))) then result := length(ev) else
				raise Win.FunctionFailed('' {$ifdef _msg_} + 'Msg' {$endif} + 'WaitForMultipleObjects');
	end;
{$else} {$error ThreadEvent.WaitAny unimplemented} {$endif}

	procedure ThreadCV.Init;
	begin
	{$ifdef NotifySpuriousWakeups}
		spurious := nil;
		spuriLock.Init;
	{$endif}
	{$ifdef Windows}
		if WinCVSupported then Win.CV.InitializeConditionVariable(Win.CONDITION_VARIABLE(ptr)) else
		begin
			ptr := new(Win.pCVOnEvent);
			Win.pCVOnEvent(ptr)^.Init;
		end;
	{$else} {$error ThreadCV.Init unimplemented} {$endif}
	end;

	procedure ThreadCV.Done;
	begin
	{$ifdef Windows}
		if WinCVSupported then
		begin
			// do nothing
		end else
		begin
			Win.pCVOnEvent(ptr)^.Done;
			dispose(Win.pCVOnEvent(ptr));
		end;
	{$else} {$error ThreadCV.Done unimplemented} {$endif}

	{$ifdef NotifySpuriousWakeups}
		Assert(length(spurious) = 0);
		spuriLock.Done;
	{$endif}
	end; {$define classname := ThreadCV} {$define pSelf := pThreadCV} {$include dyn_obj.pp.inc}

	procedure ThreadCV.StartWait;
	{$ifdef NotifySpuriousWakeups} var th: Thread.ID; {$endif}
	begin
	{$ifdef NotifySpuriousWakeups}
		th := Thread.Current;
		spuriLock.Enter;
		Assert(SpuriousIndex(th) < 0, 'EndWait missing');
		SetLength(spurious, length(spurious) + 1);
		spurious[High(spurious)].thrd := th;
		spurious[High(spurious)].really := 0;
		spuriLock.Leave;
	{$endif}
	end;

	procedure ThreadCV.EndWait;
{$ifdef NotifySpuriousWakeups}
	var
		i: sint;
{$endif}
	begin
	{$ifdef NotifySpuriousWakeups}
		spuriLock.Enter;
		i := SpuriousIndex(Thread.Current);
		Assert(i >= 0, 'StartWait missing');
		spurious[i] := spurious[High(spurious)];
		SetLength(spurious, length(spurious) - 1);
		spuriLock.Leave;
	{$endif}
	end;

	procedure ThreadCV.Wait(const lock: ThreadLockReference);
	begin
		Wait(lock, not uint(0));
	end;

	function ThreadCV.Wait(const lock: ThreadLockReference; timeoutMs: uint): boolean;
{$ifdef Windows}
	var
		winTimeout: dword;
		csp: ^CRITICAL_SECTION;
	begin
		Assert(lock.AcquiredAssert);
		if WinCVSupported then
		begin
			winTimeout := Win.TimeoutOrInfinite(timeoutMs);
		{$ifdef Debug} if Assigned(lock.nonrec) then lock.nonrec^.owner := 0; {$endif}
			if Assigned(lock.rec) or not WinSRWSupported then
			begin
				if Assigned(lock.rec) then csp := @lock.rec^.cs else csp := lock.nonrec^.ptr;
				result := Win.CV.SleepConditionVariableCS(Win.CONDITION_VARIABLE(ptr), csp^, winTimeout);
			end else
				result := Win.CV.SleepConditionVariableSRW(Win.CONDITION_VARIABLE(ptr), Win.SRWLOCK(lock.nonrec^.ptr), winTimeout, 0);
		{$ifdef Debug} if Assigned(lock.nonrec) then lock.nonrec^.owner := Thread.Current; {$endif}
		end else
			result := Win.pCVOnEvent(ptr)^.Wait(lock, timeoutMs);
	{$ifdef NotifySpuriousWakeups} NotifySpuriousWakeups; {$endif}
	end;
{$else} {$error ThreadCV.Wait unimplemented} {$endif}

	procedure ThreadCV.Wait(const lock: ThreadLockReference; condition: Predicate; param: pointer);
	begin
		if not condition(param) then
		begin
			StartWait;
			while not condition(param) do Wait(lock);
			EndWait;
		end;
	end;

	procedure ThreadCV.WakeOne;
	begin
	{$ifdef Windows}
		if WinCVSupported then
			Win.CV.WakeConditionVariable(Win.CONDITION_VARIABLE(ptr))
		else
			Win.pCVOnEvent(ptr)^.WakeOne;
	{$else} {$error ThreadCV.WakeOne unimplemented} {$endif}
	end;

	procedure ThreadCV.WakeAll;
	begin
	{$ifdef Windows}
		if WinCVSupported then
			Win.CV.WakeAllConditionVariable(Win.CONDITION_VARIABLE(ptr))
		else
			Win.pCVOnEvent(ptr)^.WakeAll;
	{$else} {$error ThreadCV.WakeAll unimplemented} {$endif}
	end;

{$ifdef NotifySpuriousWakeups}
	procedure ThreadCV.NotifySpuriousWakeups;
	var
		i: sint;
	begin
		spuriLock.Enter;
		i := SpuriousIndex(Thread.Current);
		Assert(i >= 0, 'StartWait missing');
		stat.Increment(total_wakeups);
		if spurious[i].really > 0 then
		begin
			stat.Increment(spurious_wakeups);
			if spurious[i].really > 1 then stat.Increment(spurious_wakeups);
			spurious[i].really := 1;
		end else
			spurious[i].really := 2;
		spuriLock.Leave;
	end;

	function ThreadCV.SpuriousIndex(thrd: Thread.ID): sint;
	begin
		result := Index(thrd, pointer(spurious) + fieldoffset SpuriousRec _ thrd _, length(spurious), sizeof(SpuriousRec));
	end;
{$endif}

	procedure PendingSync.Init;
	begin
		lock.Init;
		pending := 1;
		fin := nil;
	end;

	function AllPendingCompleted(param: pointer): boolean;
	begin
		result := PendingSync(param^).pending = 0;
	end;

	procedure PendingSync.Done;
	begin
		if InterlockedDecrement(pending) > 0 then
		begin
			lock.Enter;
			fin := ThreadCV.Create;
			fin^.Wait(lock, @AllPendingCompleted, @self);
			fin^.Free;
			lock.Leave;
		end;
		lock.Done;
	end;

	function PendingSync.AddOne: sint;
	begin
		result := InterlockedIncrement(pending);
		Assert(result > 1, ToString(result));
	end;

	function PendingSync.KillOne: sint;
	begin
		result := InterlockedDecrement(pending);
		if result = 0 then
		begin
			lock.Enter;
			if Assigned(fin) then fin^.WakeAll;
			lock.Leave;
		end;
	end;

	procedure RunTimerProc(param: pointer);
	label &finally;
	var
		t: pThreadTimer absolute param;
		tp: pThreadPool;
		instance: ThreadTimer.CallbackInstance;
	begin
	{$ifdef Debug} stat.Increment(total_timers); {$endif}
		t^.lock.Enter;
		if (t^.period = 0) and (t^.times > 0) or not Assigned(t^.tp) then
		begin
			// period = 0 and times > 0
			// Я подозреваю, что может быть гонка, когда таймер ресетается ровно в момент срабатывания, и в итоге «одноразовый» таймер
			// сможет отработать более одного раза. Это — второй и последний рубеж защиты. Первый — остановка и перезадание таймера в Reset,
			// вместо просто перезадания.

			// not Assigned(tp)
			// Таймер был остановлен. Из-за батчинга вызовов с NonCritical это происходит чаще, чем можно подумать.
			goto &finally;
		end;
		inc(t^.times);

		tp := t^.tp;
		tp^.PlaceTask; tp^.RunTask;
		try2
			case t^.proc.kind of
				ThreadTimer.CallbackKind.Unparametrized: t^.proc.unparametrized();
            ThreadTimer.CallbackKind.Simple:         t^.proc.simple(t^.param);
				ThreadTimer.CallbackKind.Advanced, ThreadTimer.CallbackKind.UnparametrizedAdvanced:
					begin
						instance.timer := t;
						case t^.proc.kind of
							ThreadTimer.CallbackKind.UnparametrizedAdvanced: t^.proc.unparametrizedAdvanced(instance);
							ThreadTimer.CallbackKind.Advanced: t^.proc.advanced(t^.param, instance);
							else Assert(no);
						end;
						if not Assigned(instance.timer) then exit; // вызвана instance.Close, освободившая (да и уничтожившая) блокировку
					end;
				else Assert(no);
			end;
		finally2
			tp^.EndTask;
		except2
			Fatal(Exception.Message + EOL + '(в обработчике таймера)');
		end;

	&finally:
		t^.lock.Leave;
	end;

{$ifdef Windows}
	procedure TimerQueueTimerProc(param: pointer; isTimer: ByteBool); stdcall;
	begin
		Assert(isTimer);
		RunTimerProc(param);
	end;

	procedure VistaThreadpoolTimerProc(Instance: Win.PTP_CALLBACK_INSTANCE; Context: pointer; Timer: Win.PTP_TIMER); stdcall;
	var
		t: pThreadTimer absolute Context;
	begin
		unused_args Timer end_list
		if t^.HardWork in t^.flags then Win.VistaTP.CallbackMayRunLong(Instance);
      RunTimerProc(t);
	end;

	procedure CreateXPTimer(var timer: ThreadTimer; out handle: Windows.HANDLE; due, period: uint);
	var
		wflags: Windows.ULONG;
	begin
		if timer.HardWork in timer.flags then wflags := Win.WT_EXECUTELONGFUNCTION else
		if timer.TinyWork in timer.flags then wflags := Win.WT_EXECUTEINTIMERTHREAD else
			wflags := Win.WT_EXECUTEDEFAULT;

		if not Win.XPTp.CreateTimerQueueTimer(handle, timer.tp^.xpTimerQueue, @TimerQueueTimerProc, @timer, due,
			Win.XPTimerPeriod(period), wflags)
		then
         raise Win.FunctionFailed('CreateTimerQueueTimer');
	end;

	procedure SetVistaThreadpoolTimer(var timer: ThreadTimer; due, period: uint);
	var
		timeWindow: DWORD;
		ftCompatDue: uint64;
		ftDue: Windows.FILETIME;
	begin
		// Due в SetThreadpoolTimer задаётся как FILETIME, которая считает время 100-наносекундными интервалами (т. о. миллисекунды
		// необходимо домножить на 10'000). Причём если Due положительна, она задаёт абсолютное время (!), если отрицательна — относительное.
		// Т. о., чтобы таймер сработал через три секунды, в FILETIME необходимо передать -300000000.
		// Не уверен, как поместить в FILETIME отрицательное значение, если он беззнаковый лол. Один из примеров в MSDN использует
		// промежуточный LARGE_INTEGER.
		ftCompatDue := uint64(-sint64(due) * 10000);
		ftDue.dwLowDateTime  := Lo(ftCompatDue);
		ftDue.dwHighDateTime := Hi(ftCompatDue);

		// WindowLength в SetThreadpoolTimer разрешает системе вызывать коллбэки пачками, задерживая их на время не более указанного.
		if timer.NonCritical in timer.flags then timeWindow := 4 * (due + period) else timeWindow := 0;

		Win.VistaTP.SetThreadpoolTimer(timer.wh.tp_timer, @ftDue, period, timeWindow);
	end;

	procedure DisableVistaThreadpoolTimer(var timer: ThreadTimer; wait: boolean);
	begin
		Win.VistaTP.SetThreadpoolTimer(timer.wh.tp_timer, nil, 0, 0);
		if wait then Win.VistaTP.WaitForThreadpoolTimerCallbacks(timer.wh.tp_timer, yes);
	end;
{$endif}

	procedure ThreadTimer.CallbackInstance.Close;
	begin
		timer^.InternalClose(no);
		timer^.lock.Leave;
		timer^.CompleteClosing;
		timer := nil;
	end;

	function ThreadTimer.CallbackInstance.Reset(due, period: uint): uint;
	begin
		result := timer^.SelfReset(due, period);
	end;

	procedure ThreadTimer.Invalidate;
	begin
		lock.Invalidate;
	end;

	function ThreadTimer.Valid: boolean;
	begin
		result := lock.Valid;
	end;

	procedure ThreadTimer.Open(out timer: ThreadTimer; const proc: Callback; param: pointer; due, period: uint; flags: FlagSet = []; tp: pThreadPool = nil);
	begin
		if not Assigned(tp) then tp := @Work;
		timer.proc := proc;
		timer.param := param;
		timer.times := 0;
		timer.lock.Init;
		timer.tp := tp;
		timer.flags := flags;
		timer.period := period;

		try
		{$ifdef Windows}
			if WinVistaTPSupported then
			begin
				timer.wh.tp_timer := Win.VistaTP.CreateThreadpoolTimer(@VistaThreadpoolTimerProc, @timer, nil);
				if not Assigned(timer.wh.tp_timer) then raise Win.FunctionFailed('CreateThreadpoolTimer');
				SetVistaThreadpoolTimer(timer, due, period);
			end else
				CreateXPTimer(timer, timer.wh.xpHandle, due, period);
		{$else} {$error OpenTimer unimplemented} {$endif}
		except
			timer.CompleteClosing;
			raise;
		end;
	end;

	procedure ThreadTimer.InternalClose(wait: boolean);
	{$ifdef Windows} var evh: Windows.HANDLE; {$endif}
	begin
	{$ifdef Windows}
		if WinVistaTPSupported then
		begin
			DisableVistaThreadpoolTimer(self, wait);
			Win.VistaTP.CloseThreadpoolTimer(wh.tp_timer);
		end else
		begin
			if wait then evh := INVALID_HANDLE_VALUE else evh := 0;
			if (not Win.XPTp.DeleteTimerQueueTimer(tp^.xpTimerQueue, wh.xpHandle, evh)) and (GetLastError <> ERROR_IO_PENDING) then
			{$ifdef Debug} raise Win.FunctionFailed('DeleteTimerQueueTimer') {$endif};
		end;
	{$else} {$error Timer.InternalClose unimplemented} {$endif}
	end;

	function ThreadTimer.Close: uint;
	begin
		if not Valid then exit(0);
		InternalClose(yes);
		result := times;
		CompleteClosing;
	end;

	function ThreadTimer.Reset(due, period: uint): uint;
	begin
	{$ifdef Windows}
		if WinVistaTPSupported then
			DisableVistaThreadpoolTimer(self, yes)
		else
			InternalClose(yes);
	{$else} {$error ThreadTimer.Reset stop part unimplemented} {$endif}

		result := times;
		times := 0;
		self.period := period;

	{$ifdef Windows}
		if WinVistaTPSupported then
			SetVistaThreadpoolTimer(self, due, period)
		else
			// Альтернативно, сначала создавать новый таймер (под блокировкой, либо неактивный и активировать в последнюю очередь) и только
			// потом удалять старый, чтобы оставить таймер в согласованном состоянии, если вдруг Close отработала, а создание нового провалилось.
			try
				CreateXPTimer(self, wh.xpHandle, due, period);
			except
				Fatal(Exception.Message + EOL + '(Это не было предусмотрено.)', SoftwareError);
			end;
	{$else} {$error ThreadTimer.Reset setup part unimplemented} {$endif}
	end;

	function ThreadTimer.SelfReset(due, period: uint): uint;
	begin
		result        := times;
		times  := 0;
		period := period;

	{$ifdef Windows}
		if WinVistaTPSupported then
			SetVistaThreadpoolTimer(self, due, period)
		else
			if not Win.XPTp.ChangeTimerQueueTimer(tp^.xpTimerQueue, wh.xpHandle, due, Win.XPTimerPeriod(period)) then
				raise Win.FunctionFailed('ChangeTimerQueueTimer');
	{$else} {$error Timer.Reset unimplemented} {$endif}
	end;

	procedure ThreadTimer.CompleteClosing;
	begin
		lock.Done;
		Invalidate;
	end;

	procedure Task.Close;
	var
		x: Exception;
	begin
		Assert(Flag._WillWait in flags);
	{$ifdef Debug} Assert(Assigned(guard)); {$endif}
	{$ifdef Windows}
		if WinVistaTPSupported then
			Win.VistaTP.WaitForThreadpoolWorkCallbacks(ex.tp_work, no)
		else
			pThreadEvent(ex.finished)^.Wait;
	{$else} {$error Task.Wait unimplemented} {$endif}

		x := xc;
		InternalClose;
		if Flag._Dynamic in flags then dispose(@self);
		if Assigned(x) then raise x;
	end;

	procedure Task.Run;
	var
		saveTp: pThreadPool;
	begin
		tp^.RunTask;
		try
			proc.Run(param);
		except
			if Flag._WillWait in flags then
				xc := Exception.Acquire
			else
				proc.ShowFatal('в рабочем потоке');
		end;

	{$ifdef Windows} if not WinVistaTPSupported and (Flag._WillWait in flags) then pThreadEvent(ex.finished)^.SetEvent; {$endif}
		saveTp := self.tp;
		if not (Flag._WillWait in flags) then
		begin
			InternalClose;
			if Flag._Dynamic in flags then dispose(@self);
		end;
		saveTp^.EndTask;
	end;

	procedure Task.InternalClose;
	begin
	{$ifdef Debug} Assert(Assigned(guard)); FreeMem(guard); {$endif}
	{$ifdef Windows}
		if WinVistaTPSupported then
			Win.VistaTP.CloseThreadpoolWork(ex.tp_work)
		else
			if Flag._WillWait in flags then
				pThreadEvent(ex.finished)^.Free;
	{$endif}
	end;

	procedure ThreadPool.Queue(const proc: Thread.Body; param: pointer; flags: Task.FlagSet = []);
	var
		t: pTask;
	begin
		new(t);
		TrustedQueue(t^, proc, param, flags + [Task.Flag._Dynamic] - [Task.Flag._WillWait]);
	end;

	procedure ThreadPool.Queue(out task: Task; const proc: Thread.Body; param: pointer; flags: Task.FlagSet = []);
	begin
		TrustedQueue(task, proc, param, flags - [Task.Flag._Dynamic] + [Task.Flag._WillWait]);
	end;

	procedure ThreadPool.Queue(out task: pTask; const proc: Thread.Body; param: pointer; flags: Task.FlagSet = []);
	begin
		new(task);
		TrustedQueue(task^, proc, param, flags + [task^.Flag._Dynamic, task^.Flag._WillWait]);
	end;

	procedure ThreadPool.Init;
	begin
		sync.Init;
	{$ifdef Windows}
		if not WinVistaTPSupported then
		begin
			xpTimerQueue := Win.XPTp.CreateTimerQueue();
			if xpTimerQueue = 0 then raise Win.FunctionFailed('CreateTimerQueue');
		end;
	{$endif}
	end;

	procedure ThreadPool.Done;
	begin
	{$ifdef Windows}
		if not WinVistaTPSupported then
			if not Win.XPTp.DeleteTimerQueueEx(xpTimerQueue, INVALID_HANDLE_VALUE) then
				{$ifdef Debug} raise Win.FunctionFailed('DeleteTimerQueueEx') {$endif};
	{$endif}
		sync.Done;
	end;

	procedure ThreadPool.PlaceTask;
	{$ifdef Debug} var n: uint; {$endif}
	begin
	{$ifdef Debug} stat.Increment(total_threadpool_tasks); {$endif}
	{$ifdef Debug} n := {$endif} sync.AddOne;
	{$ifdef Debug} stat.Note(max_threadpool_pending_tasks, n); {$endif}
	end;

	procedure ThreadPool.RunTask;
	begin
	{$ifdef Debug} stat.Increment(threadpool_simultaneous_pending_tasks); {$endif}
	end;

	procedure ThreadPool.EndTask;
	begin
	{$ifdef Debug} stat.Decrement(threadpool_simultaneous_pending_tasks); {$endif}
		sync.KillOne;
	end;

{$ifdef Windows}
	function QueueUserWorkItemWorker(param: pointer): dword; stdcall;
	begin
		pTask(param)^.Run;
		result := 0;
	end;

	procedure VistaThreadpoolWorker(Instance: Win.PTP_CALLBACK_INSTANCE; Context: pointer; Work: Win.PTP_WORK); stdcall;
	var
		task: pTask absolute Context;
	begin
		unused_args Work end_list
		if USystem.Task.HardWork in task^.flags then Win.VistaTP.CallbackMayRunLong(Instance);
		task^.Run;
	end;
{$endif}

	procedure ThreadPool.TrustedQueue(out task: Task; const proc: Thread.Body; param: pointer; flags: Task.FlagSet);
	{$ifdef Windows} var wflags: Windows.ULONG; {$endif}
	begin
	{$ifdef Debug} task.guard := GetMem(1); {$endif}
		task.tp := @self;
		task.proc := proc;
		task.param := param;
		task.flags := flags;
		task.xc := nil;
		PlaceTask;

	{$ifdef Windows}
		if WinVistaTPSupported then
		begin
			task.ex.tp_work := Win.VistaTP.CreateThreadpoolWork(@VistaThreadpoolWorker, @task, nil);
			if Assigned(task.ex.tp_work) then
			begin
				Win.VistaTP.SubmitThreadpoolWork(task.ex.tp_work);
				exit;
			end;
		{$ifdef Debug} ToStderr('CreateThreadpoolWork error (' + Win.DescribeError(GetLastError) + '), executing immediately.'); {$endif}
		end else
		begin
			if Task.Flag._WillWait in flags then task.ex.finished := ThreadEvent.Create;
			if Task.HardWork in flags then wflags := Win.WT_EXECUTELONGFUNCTION else wflags := Win.WT_EXECUTEDEFAULT;
			if Win.XPTp.QueueUserWorkItem(@QueueUserWorkItemWorker, @task, wflags) then exit;
		{$ifdef Debug} ToStderr('QueueUserWorkItem error (' + Win.DescribeError(GetLastError) + '), executing immediately.'); {$endif}
		end;
	{$else} {$error ThreadPool.Queue unimplemented} {$endif}

		task.Run;
	end;

	operator :=(proc: Thread.Body.UnparametrizedSignature): ThreadTimer.Callback; begin result.kind := ThreadTimer.CallbackKind.Unparametrized; result.unparametrized := proc; end;
	operator :=(proc: Thread.Body.ParametrizedSignature): ThreadTimer.Callback;   begin result.kind := ThreadTimer.CallbackKind.Simple;         result.simple := proc; end;
	operator :=(proc: ThreadTimer.AdvancedCallback): ThreadTimer.Callback;        begin result.kind := ThreadTimer.CallbackKind.Advanced;       result.advanced := proc; end;
	operator :=(proc: ThreadTimer.UnparametrizedAdvancedCallback): ThreadTimer.Callback; begin result.kind := ThreadTimer.CallbackKind.UnparametrizedAdvanced; result.unparametrizedAdvanced := proc; end;

	procedure Ticks.Initialize;
	begin
	{$ifdef Windows}
		Windows.QueryPerformanceFrequency(frequency);
	{$else} {$error Ticks.Initialize unimplemented} {$endif}

		secFFrequency := frequency;
		msecIFrequency := (frequency + 999) div 1000;
		if msecIFrequency = 0 then msecIFrequency := 1;
		toSec := 1.0 / frequency;
		toMsec := 1.0e3 * toSec;
		toMcsec := 1.0e6 * toSec;
		toNsec := 1.0e9 * toSec;
	end;

	function Ticks.Get: Ticks;
	begin
	{$ifdef Windows}
		QueryPerformanceCounter((@result.value)^);
	{$else} {$error Ticks.Get unimplemented} {$endif}
	end;

	function Ticks.Elapsed: Ticks;
	begin
		result := Get - self;
	end;

	function Ticks.FromSeconds(const sec: hp_float): Ticks;
	begin
		result.value := round(secFFrequency * sec);
		if (result.value = 0) and (sec <> 0.0) then result.value := 1;
	end;

	function Ticks.FromMilliseconds(ms: uint): Ticks;
	begin
		result.value := msecIFrequency * ms;
	end;

	function Ticks.ToSeconds: hp_float; begin result := value * toSec; end;
	function Ticks.ToMilliseconds: hp_float; begin result := value * toMsec; end;
	function Ticks.ToMicroseconds: hp_float; begin result := value * toMcsec; end;
	function Ticks.ToNanoseconds: hp_float; begin result := value * toNsec; end;
	function Ticks.ToIMilliseconds: uint; begin if 0 = not value then result := High(result) else result := round(ToMilliseconds); end;
	function Ticks.ToIMicroseconds: uint; begin if 0 = not value then result := High(result) else result := round(ToMicroseconds); end;
	function Ticks.ToINanoseconds: uint; begin if 0 = not value then result := High(result) else result := round(ToNanoseconds); end;

	function Ticks.InternalFrequency: ValueType;
	begin
		result := frequency;
	end;

	function Ticks.Overhead(count: uint): Ticks;
	begin
		if CalculatedMul = 0 then CalculateOverhead(CalculatedMulOverhead, CalculatedMul);
		result.value := (count * CalculatedMulOverhead) div CalculatedMul;
	end;

	procedure Ticks.CalculateOverhead(out mulOverhead: ValueType; out mul: uint);

		procedure Setup(out mulOverhead: ValueType; out mul: uint);
		begin
			mul         := 1;
			mulOverhead := 0;
		end;

	const
		MaxRetries = 10;
		EnoughTicks = 15000;
		SmallTicksAllowance = 3000;
	var
		i, retries: uint;
		prevOh: ValueType;
	begin
		retries := 0;
		Setup(mulOverhead, mul);

		repeat
			prevOh  := mulOverhead;

			mulOverhead := Get.value;
         for i := 2 to mul do Get;
         mulOverhead := Ticks(mulOverhead).Elapsed.value;
			if (mulOverhead + SmallTicksAllowance >= prevOh) and (mulOverhead <= (5 * prevOh) div 2 + SmallTicksAllowance) then
			begin
				if (mul > High(mul) div 2) or (mulOverhead >= EnoughTicks) then break;
				mul *= 2;
			end else
			begin
				inc(retries);
				Setup(mulOverhead, mul);
				if retries > MaxRetries then break;
			{$ifdef Debug} stat.Increment(ticks_overhead_estimation_retries); {$endif}
			end;
		until no;
	end;

	operator +(const a, b: Ticks): Ticks; begin result.value := a.value + b.value; end;

	operator -(const a, b: Ticks): Ticks;
	begin
	{$push} {$overflowchecks off} result.value := a.value - b.value; {$pop}
		if result.value < 0 then result.value := High(result.value);
	end;

	operator /(const a: Ticks; b: uint): hp_float; begin result := a.ToSeconds / b; end;
	{$define typ := Ticks} {$define ref := _1.value} {$include comparison.pp.inc}

	procedure GetCPULoad(out full, kernel, user: float);

		procedure Impl(out kernel, user: float);
	{$ifdef Windows}
			function decrypt(const ts: FILETIME): Ticks.ValueType;
			begin
				result := Ticks.ValueType(ts.dwHighDateTime) shl bitsizeof(ts.dwLowDateTime) or Ticks.ValueType(ts.dwLowDateTime);
			end;
		writeable_const_
			LastSysKernel:  Ticks.ValueType = 0;
			LastSysUser:    Ticks.ValueType = 0;
			LastProcKernel: Ticks.ValueType = 0;
			LastProcUser:   Ticks.ValueType = 0; _end
		var
			k, u, unused: FILETIME;
			sysKernel, sysUser, procKernel, procUser, sysKernelDelta, sysUserDelta, procKernelDelta, procUserDelta: Ticks.ValueType;
		begin
			if not GetSystemTimes(nil, @k, @u) then raise Win.FunctionFailed('GetSystemTimes');
			sysKernel := decrypt(k);
			sysUser   := decrypt(u);
			if not GetProcessTimes(GetCurrentProcess, (@unused)^, (@unused)^, k, u) then raise Win.FunctionFailed('GetProcessTimes');
			procKernel := decrypt(k);
			procUser  := decrypt(u);

			sysKernelDelta  := sysKernel - LastSysKernel;
			sysUserDelta    := sysUser - LastSysUser;
			procKernelDelta := procKernel - LastProcKernel;
			procUserDelta   := procUser - LastProcUser;
			if (LastSysKernel <> 0) and (sysKernelDelta <> 0) and (sysUserDelta <> 0) then
			begin
				kernel := procKernelDelta / (sysKernelDelta + sysUserDelta);
				user   := procUserDelta / (sysKernelDelta + sysUserDelta);
			end else
			begin
				kernel := 0.0;
				user := 0.0;
			end;
			LastSysKernel  := sysKernel;
			LastSysUser    := sysUser;
			LastProcKernel := procKernel;
			LastProcUser   := procUser;
		end;
	{$else} {$error GetCPULoad.Impl undefined} {$endif}

	const
		UpdatePeriodMs = 750;
	writeable_const_
		LastUpdate: Ticks = (value: 0);
		LastFull:   float = 0.0;
		LastKernel: float = 0.0;
		LastUser:   float = 0.0; _end
	var
		time, dt: Ticks;
	begin
		time := Ticks.Get;
		dt := time - LastUpdate;
		if dt < Ticks.FromMilliseconds(UpdatePeriodMs) then
		begin
			full := LastFull;
			kernel := LastKernel;
			user := LastUser;
		end else
		begin
			Impl(kernel, user);
			full := kernel + user;
			LastUpdate := time;
			LastFull := full;
			LastKernel := kernel;
			LastUser := user;
		end;
	end;

threadvar
{$ifdef Debug} fibers: array of pFiber; {$endif}
	runningFiber: pFiber;

	procedure RunFiber(f: pFiber);

		function Run: pFiber;
		begin
		{$ifdef Debug} if runningFiber <> f then Fatal('RunningFiber неадекватна.'); {$endif}
			try
				f^.proc.Run(f^.param);
			except
				f^.proc.ShowFatal('в волокне ' + f^.Human(FullOf) + ' потока ' + ToString(Thread.Current));
			end;
			result := f^.passControl;
		{$ifdef Debug} if not Assigned(result) then Fatal('Не задана нить, породившая выполняемую.'); {$endif}
			f^.passControl := nil;
			f^.finished := yes;
		{$ifdef DebugFibers} Con.WriteLine('Нить ' + f^.Human(FullOf) + ' завершена, переключение на ' + result^.Human(FullOf)); {$endif}
		end;

	begin
		Run^.Switch(yes);
	end;

{$ifdef Windows}
	procedure FiberProc(param: pointer); stdcall;
	begin
		RunFiber(param);
	end;
{$else} {$error FiberProc unimplemented} {$endif}

	function Fiber.CreateInstance(const newNameOf: string): pFiber;
	begin
		new(result);
		result^.h := nil;
		result^.passControl := nil;
		result^.passFinal := nil;
		result^.refCount := 0;
		result^.nameOf := newNameOf;
		result^.finished := no;

	{$ifdef Debug}
		SetLength(fibers, length(fibers) + 1);
		result^.id := High(fibers);
		fibers[result^.id] := result;
	{$endif}
	end;

	function Fiber.NonrefSplit: pFiber;
	begin
		if Assigned(runningFiber) then result := runningFiber else
		begin
			result := CreateInstance('Main');
			result^.h := FromThread;
			runningFiber := result;
		end;
	end;

	function Fiber.Ref: pFiber;
	begin
		result := @self;
		inc(refCount);
	end;

	function Fiber.Split: pFiber;
	begin
		result := NonrefSplit^.Ref;
	end;

	function Fiber.Create(const newNameOf: string; const proc: Thread.Body; param: pointer): pFiber;
	begin
		result := CreateInstance(newNameOf)^.Ref;
		result^.proc := proc;
		result^.param := param;
		result^.passFinal := Split;
		try
		{$ifdef Windows}
			result^.h := CreateFiberEx(0, StackSize, 0, @FiberProc, result);
			if not Assigned(result^.h) then raise Win.FunctionFailed('CreateFiber');
		{$else} {$error Fiber.Create unimplemented} {$endif}

		{$ifdef DebugFibers} Con.WriteLine('CreateFiber() = ' + HexStr(result^.h)); {$endif}
		except
			result^.Done;
			raise;
		end;
	end;

	procedure Fiber.Done;
{$ifdef Debug} var i: sint; {$endif}
	begin
		dec(refCount);
		if refCount > 0 then exit;

	{$ifdef Debug}
		fibers[id] := fibers[High(fibers)];
		fibers[id]^.id := id;
		SetLength(fibers, length(fibers) - 1);
	{$endif}

		if Assigned(h) then
		begin
			if runningFiber = @self then
			begin
			{$ifdef Debug}
				for i := 0 to High(fibers) do
					if not fibers[i]^.finished then
						Fatal('В потоке есть помимо уничтожаемой нити ' + Human(FullOf) + ' есть незаконченная ' + fibers[i]^.Human(FullOf) + '.');
			{$endif}
				runningFiber := nil;
				ToThread;
			end else
			begin
			{$ifdef Debug}
				if Assigned(passControl) then
					Fatal('Уничтожаемая нить ' + Human(FullOf) + ' приостановлена и должна передать управление в нить ' + passControl^.Human(FullOf) + '.');
				for i := 0 to High(fibers) do
					if fibers[i]^.passControl = @self then
						Fatal('Уничтожаемой нити ' + Human(FullOf) + ' собиралась передать управление приостановленная нить ' + fibers[i]^.Human(FullOf) + '.');
			{$endif}
				Delete;
			end;
		end;

		if Assigned(passFinal) then passFinal^.Done;
		dispose(@self);
	end;

	function Fiber.Resume: ResumeResults;
	begin
		if not finished then Switch(no);
		result := [];
		if finished then result += [FiberFinished];
	end;

	procedure Fiber.Yield;
	begin
		passControl^.Resume;
	end;

	function Fiber.Human(format: HumanFormat): string;
	begin
		if (format = FullOf) and (nameOf <> '') then result := nameOf + ' ' else result := '';
		result += '(' + HexStr(h) + ')';
	end;

{$ifdef Debug}
	function Fiber.GlobalDump: string;
	var
		i: sint;
	begin
		if length(fibers) = 0 then exit('(поток не разделен на волокна)');
		result := '';
		for i := 0 to High(fibers) do
		begin
			if fibers[i] = runningFiber then result += 'now>';
			result += fibers[i]^.Human(FullOf) + '(' + ToString(fibers[i]^.refcount) + ')';
			if i < High(fibers) then result += '; ';
		end;
	end;
{$endif}

	procedure Fiber.Switch(forever: boolean);
	begin
	{$ifdef Debug} if not Assigned(runningFiber) then raise Error('Текущая нить выполнения не определена.'); {$endif}
		if forever then passControl := nil else passControl := runningFiber;
		runningFiber := @self;

	{$ifdef Windows}
		SwitchToFiber(h);
	{$else} {$error Fiber.Switch unimplemented} {$endif}
	end;

	procedure Fiber.Delete;
	begin
	{$ifdef Windows}
		DeleteFiber(h);
	{$else} {$error Fiber.Delete unimplementeD} {$endif}
	{$ifdef DebugFibers} Con.WriteLine('DeleteFiber(' + HexStr(h) + ')'); {$endif}
	end;

	function Fiber.FromThread: pointer; static;
	begin
	{$ifdef Windows}
		result := ConvertThreadToFiber(nil);
	{$ifdef DebugFibers} Con.WriteLine('ConvertThreadToFiber() = ' + HexStr(result)); {$endif}
		if not Assigned(result) then raise Win.FunctionFailed('ConvertThreadToFiber');
	{$else} {$error Fiber.FromThread unimplemented} {$endif}
	end;

	procedure Fiber.ToThread;
	begin
	{$ifdef DebugFibers} Con.WriteLine('ConvertFiberToThread(' + HexStr(h) + ')'); {$endif}
	{$ifdef Windows}
		ConvertFiberToThread;
	{$else} {$error Fiber.ToThread unimplemented} {$endif}
	end;

	function FileSize.Explicit(const v: ValueType): FileSize; begin result.value := v; end;
	function FileSize.AsSizeT: size_t; begin result := RangeCheck(value, High(size_t), 'Размер файла'); end;
	{$define typ := FileSize} {$define ref := _1.value} {$include comparison.pp.inc}

	operator +(const p: FileSize; const delta: size_t): FileSize; begin result.value := p.value + delta; end;
	operator +(const p, delta: FileSize): FileSize;               begin result.value := p.value + delta.value; end;
	operator -(const p: FileSize; const delta: size_t): FileSize; begin result.value := p.value - delta; end;
	operator -(const p, delta: FileSize): FileSize;               begin result.value := p.value - delta.value; end;
	operator /(const a, b: FileSize): hp_float;                   begin result := a.value / b.value; end;

	function Mapping.Invalid: Mapping;
	begin
		result.ref := nil;
		result._data := nil;
	{$ifdef Debug} result.guard := nil; {$endif}
	end;

	function Mapping.OK: boolean;
	begin
		result := Assigned(_data);
	end;

	procedure Mapping.Close;
	begin
		if Assigned(ref) then ref^.KillOne;

	{$ifdef Debug} FreeMem(guard); {$endif}
		self := Invalid;
	end;

	function MappingRef.Create(af: pFileRef): pMappingRef;
	begin
		Assert(af^.lock.AcquiredAssert);
		new(result);
		SetLength(af^.mmaps, length(af^.mmaps) + 1);
		af^.mmaps[High(af^.mmaps)] := result;

		result^.f := af;
		result^.handle := 0;
		result^.src := nil;
		result^.refcount := 1;
	end;

	procedure MappingRef.KillOne;
	var
		i: sint;
	begin
		if Assigned(f) then f^.lock.Enter;
		if InterlockedDecrement(refcount) > 0 then
		begin
			if Assigned(f) then f^.lock.Leave;
			exit;
		end;

	{$ifdef Windows}
		if Assigned(src) then
			if not UnmapViewOfFile(src) then {$ifdef Debug} Fatal('Ошибка UnmapViewOfFile.') {$endif};
		if handle <> 0 then
			if not CloseHandle(handle) then {$ifdef Debug} Fatal('Ошибка CloseHandle(mmap).') {$endif};
	{$else} {$error Mapping.Close unimplemented} {$endif}

		if Assigned(f) then
		begin
			i := Index(@self, pPointer(f^.mmaps), length(f^.mmaps)); Assert(i >= 0);
			f^.mmaps[i] := f^.mmaps[High(f^.mmaps)];
			SetLength(f^.mmaps, length(f^.mmaps) - 1);

			f^.lock.Leave;
		end;
		dispose(@self);
	end;

	function AsioStatus.Create(what: Fundamental; error: Exception; transferred: size_t): AsioStatus;
	begin
		result.what := what;
		result.error := error;
		result.transferred := transferred;
	end;
	function AsioStatus.Create(transferred: size_t): AsioStatus;                       begin result := Create(Fundamental.Completed, nil, transferred); end;
	function AsioStatus.CreateAborted(transferred: size_t): AsioStatus;                begin result := Create(Fundamental.Aborted, nil, transferred); end;
	function AsioStatus.Create(error: Exception; transferred: size_t = 0): AsioStatus; begin result := Create(Fundamental.Failed, error, transferred); end;
	function AsioStatus.Completed: boolean; begin result := what = Fundamental.Completed; end;
	function AsioStatus.Aborted: boolean;   begin result := what = Fundamental.Aborted; end;
	function AsioStatus.Failed: boolean;    begin result := what = Fundamental.Failed; end;

	procedure AsioStatus.Done;
	begin
		error.Free;
	end;

{$ifdef Windows}
	function CompletionToStatus(ioResult: dword; transferred: size_t): AsioStatus;
	begin
		case ioResult of
			0: result := AsioStatus.Create(transferred);
			ERROR_OPERATION_ABORTED: result := AsioStatus.CreateAborted(transferred);
			else result := AsioStatus.Create(Error(Win.DescribeError(ioResult)), transferred);
		end;
	end;

	procedure XPIoCompletionCallback(dwErrorCode: dword; dwNumberOfBytesTransfered: dword; lpOverlapped: Windows.LPOVERLAPPED); stdcall;
	begin
	{$ifdef DebugAsyncIO} ToStderr('I/O completion: ' + ToString(dwNumberOfBytesTransfered) + '; code ' + ToString(dwErrorCode));
	{$else} {$ifdef Debug} if dwErrorCode <> 0 then ToStderr('I/O completion error, code ' + ToString(dwErrorCode)); {$endif} {$endif}
		asio.Close(pAsyncWrite(lpOverlapped), yes, CompletionToStatus(dwErrorCode, dwNumberOfBytesTransfered));
	end;

	procedure VistaThreadpoolIoCallback(Instance: Win.PTP_CALLBACK_INSTANCE; Context: pointer;
		Overlapped: LPOVERLAPPED; IoResult: Windows.ULONG; NumberOfBytesTransferred: Windows.ULONG_PTR; Io: Win.PTP_IO); stdcall;
	begin
	{$ifdef DebugAsyncIO} ToStderr('I/O completion (V): ' + ToString(NumberOfBytesTransfered) + '; result ' + ToString(IoResult));
	{$else} {$ifdef Debug} if IoResult <> 0 then ToStderr('I/O completion error, code ' + ToString(IoResult)); {$endif} {$endif}
		unused_args Instance _ Context _ Io end_list
		asio.Close(pAsyncWrite(Overlapped), yes, CompletionToStatus(IoResult, NumberOfBytesTransferred));
	end;
{$endif}

	procedure FileHandle.Init({$ifdef Debug} const fn: string; {$endif} {$ifdef Windows} handle: Windows.HANDLE; {$endif} async: boolean);
	begin
		try
		{$ifdef Windows}
			self.handle := handle;
			self.tp_io := nil;
			if async then
				if WinVistaTPSupported then
				begin
					self.tp_io := Win.VistaTP.CreateThreadpoolIo(handle, @VistaThreadpoolIoCallback, nil, nil);
					if not Assigned(self.tp_io) then raise Win.FunctionFailed('CreateThreadpoolIo');
				end else
					Win.XPTp.BindIoCompletionCallback(handle, @XPIoCompletionCallback, 0);
		{$else} {$error FileHandle.Init unimplemented}
		{$endif}
		{$ifdef Debug} self.fn := ToSystemFileName(fn); {$endif}
		except
			Close(no);
			raise;
		end;
	end;

	procedure FileHandle.Close(fromCompletionCallback: boolean);
	{$if defined(Windows) and defined(Debug)} var code: dword; {$endif}
	begin
	{$ifdef Windows}
		if handle <> INVALID_HANDLE_VALUE then
		begin
		{$ifdef Debug} if not {$endif}
			CloseHandle(handle)
		{$ifdef Debug} then
			begin
				code := GetLastError;
				Warning.Expanded('handle = $' + HexStr(handle, bitsizeof(handle) div 4) + EOL + Win.DescribeError(code)).Show('Не удалось закрыть ' + fn + '.');
			end
		{$endif};
			handle := INVALID_HANDLE_VALUE;
		end;

		if Assigned(tp_io) then
		begin
			if not fromCompletionCallback then Win.VistaTP.WaitForThreadpoolIoCallbacks(tp_io, no);
			// You should close the associated file handle and wait for all outstanding overlapped I/O operations to complete
			// before calling this function
			Win.VistaTP.CloseThreadpoolIo(tp_io);
			tp_io := nil;
		end;
	{$else} {$error File.Close unimplemented} {$endif}
	end;

	function FileRef.Create({$ifdef Debug} const fn: string; {$endif} {$ifdef Windows} handle: Windows.HANDLE; {$endif} async: boolean): pFileRef;
	var
		rollback: uint;
	begin
		Assert(handle <> INVALID_HANDLE_VALUE);
		rollback := 0;
		try
			new(result); rollback := 1;
			result^.Init({$ifdef Debug} fn, {$endif} {$ifdef Windows} handle, {$endif} async); rollback := 2;
			result^.async := async;
			result^.refcount := 1;
			result^.lock.Init; rollback := 3;
			result^.apos := FilePos.Zero;
		except
			if rollback >= 3 then result^.lock.Done;
			if rollback >= 2 then pFileHandle(result)^.Close(no);
		{$ifdef Windows} if rollback = 0 then CloseHandle(handle); {$endif}
			if rollback >= 1 then dispose(result);
			raise;
		end;
	end;

	function FileRef.Ref: pFileRef;
	begin
		InterlockedIncrement(refcount);
		result := @self;
	end;

	procedure FileRef.KillOne(fromCompletionCallback: boolean);
	var
		i: sint;
	begin
		if InterlockedDecrement(refcount) > 0 then exit;
		inherited Close(fromCompletionCallback);

		lock.Enter;
		for i := 0 to High(mmaps) do
			mmaps[i]^.f := nil;
		lock.Leave;
		lock.Done;

	{$ifdef Debug} stat.Decrement(opened_files); {$endif}
		dispose(@self);
	end;

	function FileRef.NoteSyncIo(const n: size_t): FilePos;
	begin
		result := apos;
		apos += n;
	end;

	function FileRef.TryReuseMmap(const apos: FilePos; asize: size_t; flags: FileFlags): pMappingRef;
	var
		i: sint;
	begin
		result := nil;
		Assert(flags = flags * MmapAffectingFlags);
		Assert(lock.AcquiredAssert);

		for i := 0 to High(mmaps) do
			if (mmaps[i]^.srcPos <= apos) and (mmaps[i]^.srcPos + mmaps[i]^.srcSize >= apos + asize) and (mmaps[i]^.srcFlags >= flags) then
			begin
				result := mmaps[i];
				InterlockedIncrement(result^.refcount);
			{$ifdef Debug} stat.Increment(reused_mmaps); {$endif}
				exit;
			end;
	end;

	function &File.Invalid: &File;
	begin
		result.ref := nil;
	end;

	function &File.Open(const filename: string; flags: FileFlags): &File;

	{$ifdef Windows}
		function CreatePath(out err: dword): boolean;
		begin
			result := Folder.Create(Folder.Path(filename));
			if not result then err := GetLastError;
		end;
	{$endif}

	var
		r: pFileRef absolute result.ref;
		async: boolean;
	{$ifdef Windows}
		handle: Windows.HANDLE;
		wfn: widestring;
		access, share, disp, attrs, err: dword;
		tryId: sint;
	{$endif}
	begin
		async := (file_Write in flags) and not (file_Sync in flags);
	{$ifdef Windows}
		wfn := Win.ToWideFileName(filename);
		access := 0;
		if file_Read in flags then access := access or GENERIC_READ;
		if file_Write in flags then access := access or GENERIC_READ or GENERIC_WRITE; // без GENERIC_READ не работает mmap на запись :(
		share := 0;
		if (file_Read in flags) and not (file_Write in flags) then share := share or FILE_SHARE_READ;
		if file_ShareReadWrite in flags then share := share or FILE_SHARE_READ or FILE_SHARE_WRITE;
		if (file_Write in flags) and not (file_Existing in flags) then
			if file_New in flags then disp := CREATE_NEW
			else if file_Read in flags then disp := OPEN_ALWAYS
			else disp := CREATE_ALWAYS
		else
			disp := OPEN_EXISTING;
		attrs := 0;
		if file_RandomAccess in flags then attrs := attrs or FILE_FLAG_RANDOM_ACCESS;
		if file_SequentialAccess in flags then attrs := attrs or FILE_FLAG_SEQUENTIAL_SCAN;
		if async then attrs := attrs or FILE_FLAG_OVERLAPPED;
		if file_Temp in flags then attrs := attrs or FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE;

		tryId := 0;
		repeat
			handle := CreateFileW(pWideChar(wfn), access, share, nil, disp, attrs, 0);
			inc(tryId);
			if handle = INVALID_HANDLE_VALUE then err := GetLastError;
		until (handle <> INVALID_HANDLE_VALUE) or (tryId > 1)
			or not ((file_Write in flags) and (err = ERROR_PATH_NOT_FOUND) and CreatePath(err));

		if handle = INVALID_HANDLE_VALUE then raise Win.FileLoadError(filename, err);
	{$else} {$error &File.Open unimplemented} {$endif}
		try
			r := FileRef.Create({$ifdef Debug} filename, {$endif} handle, async);;
		except
			r := nil;
			raise;
		end;
	{$ifdef Debug} stat.Increment(opened_files); {$endif}
	end;

	function &File.OK: boolean;
	begin
		result := Assigned(ref);
	end;

	procedure &File.Close;
	begin
		if Assigned(ref) then ref^.KillOne(no);
		self := Invalid;
	end;

	function &File.Duplicate: &File;
	begin
		Assert(OK);
		result.ref := ref^.Ref;
	end;

	function &File.Read(buf: pointer; n: size_t): size_t;
	begin
		result := Read(FilePos.Not0, buf, n);
	end;

	function &File.Read(at: FilePos; buf: pointer; n: size_t): size_t;
{$ifdef Windows}
	var
		readed: dword;
		aw: pAsyncWrite;
		p: LARGE_INTEGER;
		rfOk: boolean;
{$endif}
	begin
		if ref^.async and (0 = not at.value) then at := ref^.NoteSyncIo(n);

	{$ifdef Windows}
		Assert(@readed = @readed);
		if ref^.async then
		begin
			aw := asio.Add(ref, 0, at.value);

			rfOk := Windows.ReadFile(ref^.handle, buf^, n, readed, @aw^.op);
			if rfOk or (GetLastError = ERROR_IO_PENDING) then
			begin
				if not rfOk then GetOverlappedResult(ref^.handle, aw^.op, readed, yes);
				result := readed;
			end else
			begin
				result := 0;
				asio.Close(aw, no, AsioStatus.Create(Error(Win.DescribeError(GetLastError))));
			end;
		end else
		begin
			if 0 <> not at.value then
			begin
				p.QuadPart := at.value;
				if (not SetFilePointerEx(ref^.handle, p, @p, FILE_BEGIN)) or (p.QuadPart <> at.value) then exit(0);
			end;

			if Windows.ReadFile(ref^.handle, buf^, n, readed, nil) then
				result := readed
			else
				result := 0;
		end;
	{$else} {$error &File.Read unimplemented} {$endif}
	end;

	function &File.Write(buf: pointer; n: size_t): size_t;
	begin
		result := Write(FilePos.Not0, buf, n);
	end;

	function &File.Write(const s: string): uint;
	begin
		result := Write(FilePos.Not0, s);
	end;

	function &File.Write(at: FilePos; buf: pointer; n: size_t): size_t;
{$ifdef Windows}
	var
		written: dword;
		p: LARGE_INTEGER;
		aw: pAsyncWrite;
		ok1, ok2: boolean;
{$endif}

	begin
		Assert(@written = @written);
		if ref^.async then
		begin
			if 0 = not at.value then at := ref^.NoteSyncIo(n);

			aw := asio.Add(ref, n {$ifdef Windows}, at.value {$endif});
			memcpy(buf, pGenericAlignedType(aw^.data), n);
		end;

	{$ifdef Windows}
		if ref^.async then
		begin
			ok1 := WriteFile(ref^.handle, pGenericAlignedType(aw^.data)^, n, dword(nil^), @aw^.op);
			ok2 := ok1 or (GetLastError = ERROR_IO_PENDING);
			if ok2 then result := n else
			begin
				result := 0;

				// Асинхронные Read/WriteFile не вызывают коллбэк тогда и только тогда, когда завершаются с ошибкой помимо IO_PENDING.
				// Предположительно.
				asio.Close(aw, no, AsioStatus.Create(Error(Win.DescribeError(GetLastError))));
			end;

		{$ifdef Debug}
			if ok1 then stat.Increment(async_writes_completed_synchronously) else
				if ok2 then stat.Increment(truly_async_writes);
		{$endif}
		end else
		begin
			if 0 <> not at.value then
			begin
				p.QuadPart := at.value;
				if (not SetFilePointerEx(ref^.handle, p, @p, FILE_BEGIN)) or (p.QuadPart <> at.value) then exit(0);
			end;

			if Windows.WriteFile(ref^.handle, buf^, n, written, nil) then
				result := written
			else
				result := 0;
		end;
	{$else} {$error &File.Write unimplemented} {$endif}
	end;

	function &File.Write(const at: FilePos; const s: string): uint;
	begin
		result := Write(at, pointer(s), length(s));
	end;

	function &File.Seek(const pos: FilePos): FilePos;
{$ifdef Windows}
	var
		p, r: LARGE_INTEGER;
{$endif}
	begin
	{$ifdef Windows}
		p.QuadPart := pos.value;
		if SetFilePointerEx(ref^.handle, p, @r, FILE_BEGIN) then
			result := FilePos.Explicit(r.QuadPart)
		else
			result := FilePos.Not0;
	{$else} {$error &File.Seek unimplemented} {$endif}

		if 0 <> not result.value then ref^.apos := result;
	end;

	function &File._GetSize: FileSize;
{$ifdef Windows}
	var
		sz: LARGE_INTEGER;
	begin
		if GetFileSizeEx(ref^.handle, @sz) then
			result := FileSize.Explicit(sz.QuadPart)
		else
			result := FileSize.Zero;
	end;
{$else} {$error &File.GetSize unimplemented} {$endif}

	procedure &File._SetSize(const newSize: FileSize);
{$ifdef Windows}
	var
		oldfp, fp: LARGE_INTEGER;
	begin
		fp.QuadPart := 0;
		if (not SetFilePointerEx(ref^.handle, fp, @oldfp, FILE_CURRENT)) or (fp.QuadPart <> 0) then begin {$ifdef Debug} raise Win.FunctionFailed('SetFilePointerEx(current, 0)'); {$endif} exit; end;
		fp.QuadPart := newSize.value;
		if (not SetFilePointerEx(ref^.handle, fp, @fp, FILE_BEGIN)) or (fp.QuadPart <> newSize.value) then begin {$ifdef Debug} raise Win.FunctionFailed('SetFilePointerEx(new)'); {$endif} exit; end;
		if not SetEndOfFile(ref^.handle) then begin {$ifdef Debug} raise Error('Ошибка SetEndOfFile.'); {$endif} exit; end;
		if (not SetFilePointerEx(ref^.handle, oldfp, @fp, FILE_BEGIN)) or (fp.QuadPart <> oldfp.QuadPart) then {$ifdef Debug} raise Win.FunctionFailed('SetFilePointer(restore_old)') {$endif};
	end;
{$else} {$error &File.SetSize unimplemented} {$endif}

	function &File.Mmap(out m: Mapping; pos: FilePos; msize: size_t; flags: FileFlags): boolean;
	label _finally_;
	var
		fix: size_t;
{$ifdef Windows}
		access: dword;
		mstest: size_t;
{$endif}
	begin
		flags *= FileRef.MmapAffectingFlags;
		m := Mapping.Invalid;
	{$ifdef Debug} m.guard := GetMem(1); {$endif}
		result := no;

		ref^.lock.Enter;
		m.ref := ref^.TryReuseMmap(pos, msize, flags);
		if Assigned(m.ref) then
		begin
			m._data := m.ref^.src + (pos - m.ref^.srcPos).value;
			ref^.lock.Leave;
			exit(yes);
		end;

		m.ref := MappingRef.Create(ref);
		fix := align_left_howmuch(pos.value, SystemInfo.allocationGranularity);
		msize += fix;
		pos -= fix;
		mstest := align(msize, SystemInfo.pageSize);
		if FileSize.Explicit(pos.value) + mstest <= _GetSize then msize := mstest;
	{$ifdef Windows}
		if file_Write in flags then access := PAGE_READWRITE else access := PAGE_READONLY;
		m.ref^.handle := CreateFileMapping(ref^.handle, nil, access, Hi(qword(pos) + msize), Lo(qword(pos) + msize), nil);
		if m.ref^.handle = 0 then goto _finally_;

		if file_Write in flags then access := FILE_MAP_WRITE else access := FILE_MAP_READ;
		m.ref^.src := MapViewOfFile(m.ref^.handle, access, Hi(qword(pos)), Lo(qword(pos)), msize);
	{$else} {$error &File.Mmap unimplemented} {$endif}
		result := Assigned(m.ref^.src);

	_finally_:
		if result then
		begin
			m._data := m.ref^.src + fix;
			m.ref^.srcPos  := pos;
			m.ref^.srcSize := msize;
			m.ref^.srcFlags := flags;
		{$ifdef Debug} stat.Note(max_mmaps, length(ref^.mmaps)); {$endif}
		end;
		ref^.lock.Leave;
		if not result then m.Close;
	end;

	function &File.Copy(var from, &to: &File; n: FileSize): FileSize;
	label _finally_;
	var
		buf: pointer;
		bufSize, sz: size_t;
	begin
		result := FileSize.Zero;
		bufSize := TempBufferSize; if n.value < bufSize then bufSize := n.value;
		buf := GetMem(bufSize);

		repeat
			if n.value > bufSize then sz := bufSize else sz := n.value;
			if from.Read(buf, sz) <> sz then goto _finally_;
			if &to.Write(buf, sz) <> sz then goto _finally_;
			result += sz;
			n -= sz;
		until n = FileSize.Zero;

	_finally_:
		FreeMem(buf);
	end;

	function &File.CreateTemp(const base: string; out fn: string; mode: TempMode = CreateAndOpenTemp): &File;
		function ExtraTempFlags: FileFlags;
		begin
			if mode = CreateAndOpenSharedTemp then result := [file_ShareReadWrite] else result := [];
		end;

	{$ifdef Windows}
		procedure Impl(const path, prefix, ext: string);
		var
			buf: widestring;
			err: dword;
		begin
			SetLength(buf, Windows.MAX_PATH);
			if GetTempFileNameW(pWideChar(Win.ToWideFileName(path)), pWideChar(Win.ToWideFileName(prefix)), 0, pWideChar(buf)) = 0 then
			begin
				err := GetLastError;
				raise Error('Не удалось создать временный файл ' + ToSystemFileName(path + prefix) + '. ' + Win.DescribeError(err));
			end;
			fn := Win.FromWideFileName(pWideChar(buf));
			try
				if ext <> '' then fn := Rename(fn, Folder.Filename(fn) + '.' + ext);
				if mode <> OnlyCreate then result := Open(fn, [file_Read, file_Write, file_Existing, file_Temp] + ExtraTempFlags);
			except
				Erase(fn);
				raise;
			end;
		end;
	{$else} {$error File.CreateTemp unimplemented} {$endif}
	var
		path, prefix, ext, createdFolder: string;
	begin
		path   := Folder.Path(base);     if path = '' then path := Folder.Temp;
		prefix := Folder.Filename(base); if prefix = '' then prefix := Folder.FilenameNoExt(ExecFileName);
		ext    := Folder.Extension(base);
		Folder.Create(Folder.RemoveSeparator(path), @createdFolder);
		try
			Impl(path, prefix, ext);
		except
			if createdFolder <> '' then Folder.EraseTree(createdFolder);
			raise;
		end;
	end;

	function &File.Exists(const filename: string): boolean;
	begin
		result := file_JustFile in GetFileAttributes(filename);
	end;

	function &File.Erase(const filename: string): boolean;
	begin
	{$ifdef Windows}
		result := DeleteFileW(pWideChar(Win.ToWideFileName(filename)));
	{$else} {$error &File.Erase unimplemented} {$endif}
	end;

type
	pProgressParam = ^ProgressParam;
	ProgressParam = record
		progress: &File.ProgressCallback;
		param: pointer;
	end;

{$ifdef Windows}
	function MoveCopyFileProgress(TotalFileSize, TotalBytesTransferred, StreamSize, StreamBytesTransferred: Windows.LARGE_INTEGER;
		dwStreamNumber, dwCallbackReason: DWORD; hSourceFile, hDestinationFile: Windows.HANDLE; lpData: pointer): dword; stdcall;
	var
		p: pProgressParam absolute lpData;
		whattodo: &File.ProgressCallbackResult;
	begin
		Assert((@StreamSize = @StreamSize) and (@StreamBytesTransferred = @StreamBytesTransferred) and (@dwStreamNumber = @dwStreamNumber)
			and (@dwCallbackReason = @dwCallbackReason) and (@hSourceFile = @hSourceFile) and (@hDestinationFile = @hDestinationFile));

		whattodo := p^.progress(FileSize.Explicit(TotalBytesTransferred.QuadPart), FileSize.Explicit(TotalFileSize.QuadPart), p^.param);
		case whattodo of
			CancelOperation: result := Win.PROGRESS_CANCEL;
			ContinueOperation: result := Win.PROGRESS_CONTINUE;
			else raise ExhaustiveCase(ord(whattodo), 'MoveCopy.progress');
		end;
	end;
{$endif}

	function &File.MoveCopy(move: boolean; const from, &to: string; flags: OpFlags; progress: ProgressCallback = nil; param: pointer = nil): boolean;
	var
		p: ProgressParam;
	{$ifdef Windows} wprogress: Win.LPPROGRESS_ROUTINE; wflags, code: dword; wfrom, wto: widestring; {$endif}
	begin
		if Assigned(progress) then
		begin
			p.progress := progress;
			p.param := param;
		end;

	{$ifdef Windows}
		if Assigned(progress) then wprogress := @MoveCopyFileProgress else wprogress := nil;
		wfrom := Win.ToWideFileName(from);
		wto   := Win.ToWideFileName(&to);

		wflags := 0;
		if move then
		begin
			if not (ToSameLocation in flags) then wflags := MOVEFILE_COPY_ALLOWED;
			if AllowReplace in flags then wflags := wflags or MOVEFILE_REPLACE_EXISTING;
			result := MoveFileWithProgressW(pWideChar(wfrom), pWideChar(wto), wprogress, @p, wflags);
		end else
		begin
			if not (AllowReplace in flags) then wflags := wflags or Win.COPY_FILE_FAIL_IF_EXISTS;
			result := CopyFileExW(pWideChar(wfrom), pWideChar(wto), wprogress, @p, nil, wflags);
		end;

		if not result and (Throw in flags) then
		begin
			code := GetLastError;
			raise Error(from + ' -> ' + &to + ': ' + Win.LowercaseFirst(Win.DescribeError(code)));
		end;
	{$else} {$error &File.MoveCopy unimplemented} {$endif}
	end;

	function &File.Move(const from, &to: string; flags: OpFlags; progress: ProgressCallback = nil; param: pointer = nil): boolean;
	begin
		result := MoveCopy(yes, from, &to, flags, progress, param);
	end;

	function &File.Copy(const from, &to: string; flags: OpFlags; progress: ProgressCallback = nil; param: pointer = nil): boolean;
	begin
		result := MoveCopy(no, from, &to, flags, progress, param);
	end;

	function &File.Rename(const fullFrom, toName: string; flags: OpFlags = []): string;
	begin
		result := Folder.Path(fullFrom) + toName;
		Move(fullFrom, result, flags + [ToSameLocation, Throw]);
	end;

	function Folder.Exists(const folder: string): boolean;
	begin
		result := file_Folder in GetFileAttributes(folder);
	end;

	function Folder.EraseOne(const folder: string): boolean;
	begin
	{$ifdef Windows}
		result := RemoveDirectoryW(pWideChar(Win.ToWideFileName(folder)));
	{$else} {$error Folder.EraseOne undefined} {$endif}
	end;

	function Folder.EraseTree(const folder: string): boolean;
	var
		f: FoundFile;
	begin
		if folder = '' then raise Error('Пустое имя папки в Folder.EraseTree.');
		result := yes;
		if EraseOne(folder) then exit;

		for f in Scan(folder) do
		begin
			if f.IsFolder then result := result and EraseTree(f.SearchedName)
			else if f.IsFile then result := result and &File.Erase(f.SearchedName);
		end;
		result := result and EraseOne(folder);
	end;

	function Folder.Scan(const folder: string; const mask: string; filter: WhatToSearch = Anything): Enumerator;
	begin
		result        := Enumerator.Create;
	{$ifdef Windows} result.handle := INVALID_HANDLE_VALUE; {$endif}
		result.folder := folder;
		result.mask   := mask;
		result.filter := filter;
	end;
   function Folder.Scan(const folder: string; filter: WhatToSearch = Anything): Enumerator; begin result := Scan(folder, '', filter); end;

	function Folder.Create(const folder: string; firstCreated: pString = nil): boolean;
	{$ifdef Windows}
		function CreateDirectory(const dir: string; out reallyCreated: boolean): boolean;
		begin
			if (length(dir) > 0) and (dir[length(dir)] = ':') then // E:
			begin
				reallyCreated := no;
				exit(yes);
			end;

			reallyCreated := CreateDirectoryW(pWideChar(Win.ToWideFileName(dir)), nil);
			result := reallyCreated or (GetLastError = ERROR_ALREADY_EXISTS);
		end;
	{$else} {$error CreateDirectory unimplemented} {$endif}
	var
		p: sint;
		cur, first: string;
		reallyCreated: boolean;
	begin
		first := '';
		if Assigned(firstCreated) then firstCreated^ := '';

		for p := 1 to length(folder) do
		begin
			if p = length(folder) then cur := folder else
				if folder[p] = FileSeparator then cur := Copy(folder, 1, p - 1) else
					continue;

			if CreateDirectory(cur, reallyCreated) then
			begin
				if reallyCreated and (first = '') then first := cur;
			end else
			begin
				if first <> '' then EraseTree(first);
				exit(no);
			end;
		end;

		if Assigned(firstCreated) then firstCreated^ := first;
		result := yes;
	end;

	function Folder.AppendSeparator(const folder: string): string;
	begin
		if (length(folder) > 0) and (folder[length(folder)] <> FileSeparator) then
			result := folder + FileSeparator
		else
			result := folder;
	end;

	function Folder.RemoveSeparator(const folder: string): string;
	begin
		if (length(folder) > 1 {'/' → '/'}) and (folder[length(folder)] = FileSeparator) then
			result := Copy(folder, 1, length(folder) - 1)
		else
			result := folder;
	end;

	function Folder.Path(const fn: string; stripSeparator: boolean): string;
	var
		p: size_t;
	begin
		p := Index1Rev(FileSeparator, pointer(fn), length(fn) * sizeof(char));
		if p > 0 then result := Copy(fn, 1, p - size_t(stripSeparator)) else result := '';
	end;
	function Folder.Path(const fn: string): string; begin result := Path(fn, no); end;
	function Folder.Parent(const fn: string): string; begin result := Path(fn, yes); end;

	function Folder.Filename(const fn: string): string;
	var
		p: size_t;
	begin
		p := Index1Rev(FileSeparator, pointer(fn), length(fn) * sizeof(char));
		if p > 0 then result := Copy(fn, p + 1, length(fn) - p) else result := fn;
	end;

	function Folder.Extension(const fn: string): string;
	var
		i: sint;
	begin
		for i := length(fn) downto 1 do
			case fn[i] of
				ExtensionSeparator: exit(Copy(fn, i + 1, length(fn) - i));
				FileSeparator: break;
			end;
		result := '';
	end;

	function Folder.FilenameNoExt(const fn: string): string;
	var
		p, e: size_t;
	begin
		p := Index1Rev(FileSeparator, pointer(fn), length(fn) * sizeof(char));
		e := p + Index1Rev(ExtensionSeparator, pointer(fn) + p, (length(fn) - p) * sizeof(char));
		if e = p then e := length(fn) else dec(e);
		if (p = 0) and (e = size_t(length(fn))) then result := fn else result := Copy(fn, p+1, e-p);
	end;

{$ifdef Windows}
	procedure QueryTempFolderPath(buf: pWideChar; nBuf: size_t; out len: size_t);
	begin
		len := GetTempPathW(nBuf, buf);
		if len = 0 then raise Win.FunctionFailed('GetTempPath');
	end;
{$endif}

FPC_3_BUG writeable_const_ CachedTempFolder: string = ''; _end // рапортуется heaptrc, если объявить внутри Temp

	function Folder.Temp: string;
	begin
		result := CachedTempFolder; if result <> '' then exit;
	{$ifdef Windows}
		result := Folder.AppendSeparator(Win.FromWideFileName(Win.QueryString(@QueryTempFolderPath, 'имени временной папки')));
	{$else} {$error Folder.Temp unimplemented} {$endif}
		CachedTempFolder := result;
	end;

{$ifdef Windows}
	procedure QueryWorkingFolderPath(buf: pWideChar; nBuf: size_t; out len: size_t);
	begin
		len := GetCurrentDirectoryW(nBuf, buf);
		if len = 0 then raise Win.FunctionFailed('GetCurrentDirectory');
	end;
{$endif}

	function Folder.Working: string;
	begin
	{$ifdef Windows}
		result := Folder.AppendSeparator(Win.FromWideFileName(Win.QueryString(@QueryWorkingFolderPath, 'имени текущего каталога')));
	{$else} {$error Folder.Working unimplemented} {$endif}
	end;

	function Folder.Enumerator.IsFolder: boolean; begin result := file_Folder in what; end;
	function Folder.Enumerator.IsFile: boolean;   begin result := file_JustFile in what; end;

	function Folder.Enumerator.SearchedName: string;
	begin
		result := USystem.Folder.AppendSeparator(folder) + name;
	end;

	function Folder.Enumerator.Size: FileSize;
	begin
	{$ifdef Windows}
		result := FileSize.Explicit(QWORD(data.nFileSizeHigh) shl bitsizeof(DWORD) + data.nFileSizeLow);
	{$else} {$error Folder.Enumerator.Size unimplemented} {$endif}
	end;

	destructor Folder.Enumerator.Destroy;
	begin
	{$ifdef Windows} if handle <> INVALID_HANDLE_VALUE then begin FindClose(handle); handle := INVALID_HANDLE_VALUE; end;
	{$else} {$error Folder.Enumerator.Destroy unimplemented} {$endif}
		inherited Destroy;
	end;

	function Folder.Enumerator.GetEnumerator: Enumerator; begin result := self; end;
	function Folder.Enumerator.GetCurrent: Enumerator;    begin result := self; end;

	function Folder.Enumerator.MoveNext: boolean;
	{$ifdef Windows}
		function FindFirst(const folder, mask: string; out data: WIN32_FIND_DATAW): Windows.HANDLE;
		var
			m: string;
		begin
			if mask <> '' then m := mask else m := '*';
			result := FindFirstFileExW(pWideChar(Win.ToWideFileName(USystem.Folder.AppendSeparator(folder) + m)),
			                           FindExInfoBasic, @data, FindExSearchNameMatch, nil, 0);
		end;

		function Decrypt(const data: WIN32_FIND_DATAW; found: FoundFile; filter: WhatToSearch): boolean;
		const
			CheckMask: array[WhatToSearch] of FileAttributes = ([file_JustFile, file_Folder], [file_JustFile], [file_Folder]);
		begin
			result := no;
			found.what := Win.ToFileAttributes(data.dwFileAttributes);
			if (found.what = []) or (CheckMask[filter] * found.what <> found.what) then exit;

			found.name := Win.FromWideFileName(data.cFileName);
			if (found.name = '.') or (found.name = '..') then exit;
			result := yes;
		end;
	{$endif}

	begin
	{$ifdef Windows}
		repeat
			if handle = INVALID_HANDLE_VALUE then
			begin
				handle := FindFirst(folder, mask, data);
				result := handle <> INVALID_HANDLE_VALUE;
			end else
				result := FindNextFileW(handle, data);
		until (not result) or Decrypt(data, self, filter);
	{$else} {$error Folder.Enumerator.MoveNext unimplemented}
	{$endif}
	end;

	procedure FileWatch.Open(out w: FileWatch; const fnOrDir: string; cb: Callback; param: pointer);
	begin
		w := Invalid;
		new(w.ref);
		try
			w.ref^.cb := cb;
			w.ref^.param := param;
			w.ref^.fnOrDir := fnOrDir;
			w.ref^.removed := no;
			w.ref^.idInFolder := -1;

		{$ifdef Windows}
			if (length(fnOrDir) > 0) and (fnOrDir[length(fnOrDir)] = FileSeparator) then
				w.ref^.wfn := ''
			else
				w.ref^.wfn := UTF8Decode(Folder.Filename(fnOrDir));
		{$endif}

			Registry^.Add(w.ref);
		except
			w.Close;
			raise;
		end;
	end;

	procedure FileWatch.Close;
	var
		reg: pFolderRegistry;
		fi: sint;
	begin
		if OK then
		begin
			if ref^.idInFolder >= 0 then
			begin
				reg := Registry;
				reg^.lock.Enter;
				fi := ref^.folderIdInRegistry;
				if reg^.folders[fi]^.locked = FolderNotLocked then
					if reg^.folders[fi]^.RemoveWatch(ref, ref^.idInFolder) = 0 then
						reg^.RemoveFolder(reg^.folders[fi], fi, no)
					else
				else
				begin
					Assert(reg^.folders[fi]^.locked in [FolderLockedYetNothingRemoved, FolderLockedAndHasRemovedEntries]);
					ref^.removed := yes;
					reg^.folders[fi]^.locked := FolderLockedAndHasRemovedEntries;
				end;
				reg^.lock.Leave;
			end else
				dispose(ref);
		end;
		self := Invalid;
	end;

	function FileWatch.Invalid: FileWatch; begin result.ref := nil; end;
	function FileWatch.OK: boolean; begin result := Assigned(ref); end;

	procedure OnReadChanges(const status: AsioStatus; buf: pointer; param: pointer);
	var
		reg: FileWatch.pFolderRegistry;
		d: FileWatch.pFolderRec absolute param;
		ok, locked: boolean;
		i: sint;
		w: FileWatch.pReference;
	{$ifdef Windows} notify: ^Win.FILE_NOTIFY_INFORMATION; {$endif}
	begin
		ok := status.Completed;
		reg := FileWatch.Registry;
		locked := ok;
		if locked then reg^.lock.Enter;
		ok := ok and (d^.locked <> FolderRemoved);
		if ok then
			try
				Assert(Assigned(d^.aw));
				Assert(d^.locked = FolderNotLocked);
				d^.locked := FolderLockedYetNothingRemoved;

			{$ifdef Windows}
				notify := buf;
				repeat
					case notify^.Action of
						Win.FILE_ACTION_MODIFIED:
							for i := 0 to High(d^.watches) do // внимание, cb() могут добавить правее новые
							begin
								w := d^.watches[i];
								if Assigned(w^.cb) then
									if (notify^.FileNameLength = sizeof(widechar) * uint(length(w^.wfn)))
										and (CompareByte(pWideChar(w^.wfn)^, (@notify^.FileName[0])^, notify^.FileNameLength) = 0)
									then
										w^.cb(w^.fnOrDir, w^.param)
									else if w^.wfn = '' then // слежение за всей папкой
										w^.cb(UTF8Encode(ToString(pWideChar(notify^.FileName), notify^.FileNameLength div sizeof(widechar))), param);
							end;
					end;

					pointer(notify) += notify^.NextEntryOffset;
				until notify^.NextEntryOffset = 0;
			{$endif}

				if d^.locked = FolderLockedAndHasRemovedEntries then
					for i := High(d^.watches) downto 0 do
						if d^.watches[i]^.removed then
							ok := (d^.RemoveWatch(d^.watches[i], i) > 0) and ok;

				d^.aw := nil;
				if ok then d^.PostReadChangesRequest; // может бросить исключение, d^.locked останется в странном состоянии, но RemoveFolder это неважно
				d^.locked := FolderNotLocked;
			except
			{$ifdef Debug} Exception.Show; {$endif}
				ok := no;
			end;

		if not ok then reg^.RemoveFolder(d, d^.id, yes);
		if locked then reg^.lock.Leave;
	end;

	procedure FileWatch.FolderRec.PostReadChangesRequest;
	const
		BufferSize = 4096 - (sizeof(AsyncWrite) - sizeof(AsyncWrite.data));
	var
		awt: pAsyncWrite absolute aw;
	begin
		Assert(not Assigned(aw));
		Assert(FileWatch.Registry^.lock.AcquiredAssert);
		awt := asio.Add(@f, @OnReadChanges, @self, BufferSize);

	{$ifdef Windows}
		if not ReadDirectoryChangesW(f.handle, pGenericAlignedType(awt^.data), BufferSize, no, FILE_NOTIFY_CHANGE_LAST_WRITE, nil, @awt^.op, nil) then
		begin
			awt := nil;
			try
				raise Win.FunctionFailed('ReadDirectoryChanges');
			finally
				asio.Close(aw, no, AsioStatus.Create(Exception.CreateBlank));
			end;
		end;
	{$else} {$error ReadDirectoryChanges unimplemented} {$endif}
	end;

	function FileWatch.FolderRec.RemoveWatch(w: pReference; wi: sint): sint;
	begin
		Assert((w^.idInFolder = wi) and (w^.folderIdInRegistry = self.id));
		Assert(locked in [FolderNotLocked, FolderLockedAndHasRemovedEntries]);

		watches[wi] := watches[High(watches)];
		watches[wi]^.idInFolder := wi;
		result := length(watches) - 1;
		SetLength(watches, result);
		dispose(w);
	end;

	procedure FileWatch.FolderRegistry.Init;
	begin
		lock.Init;
	end;

	procedure FileWatch.FolderRegistry.Done;
	begin
		lock.Done;
	end; {$define classname := FileWatch.FolderRegistry} {$define pSelf := pFolderRegistry} {$include dyn_obj.pp.inc}

	procedure FileWatch.FolderRegistry.Add(w: FileWatch.pReference);
		procedure AddToFolder(fi: uint);
		var
			wi: uint;
		begin
			// Это OK, если папка locked. Цикл вычислит правую границу заранее, новые вотчи добавятся после неё.
			wi := length(folders[fi]^.watches);
			SetLength(folders[fi]^.watches, wi + 1);
			folders[fi]^.watches[wi] := w;
			w^.idInFolder := wi;
			w^.folderIdInRegistry := fi;
		end;
	var
		dir: string;
		i: sint;
		rollback: uint;
		f: pFolderRec;
	{$ifdef Windows} h: Windows.HANDLE; {$endif}
	begin
		dir := Folder.Parent(w^.fnOrDir);
		lock.Enter;
		try
			for i := 0 to High(folders) do
				if folders[i]^.dir = dir then
				begin
					AddToFolder(i);
					exit;
				end;

			// Папка не найдена — нужно создать новую.
			new(f); rollback := 0;
			f^.locked := FolderNotLocked;
			f^.dir := dir;
			f^.aw := nil;
			try
				h := CreateFileW(pWideChar(Win.ToWideFileName(dir)),
					FILE_LIST_DIRECTORY, FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_EXISTING,
					FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED, 0);
				if h = INVALID_HANDLE_VALUE then raise Win.FileLoadError(dir, GetLastError);
				f^.f.Init({$ifdef Debug} dir, {$endif} {$ifdef Windows} h, {$endif} yes); rollback := 1;
				SetLength(folders, length(folders) + 1); rollback := 2;
				folders[High(folders)] := f; f^.id := High(folders);
				AddToFolder(High(folders)); rollback := 3;
				f^.PostReadChangesRequest;
			except
				if rollback >= 2 then SetLength(folders, length(folders) - 1);
				if rollback >= 1 then f^.f.Close(no);
				dispose(f);
				raise;
			end;
		finally
			lock.Leave;
		end;
	end;

	procedure FileWatch.FolderRegistry.RemoveFolder(f: pFolderRec; fi: sint; fromCompletionCallback: boolean);
	var
		watch: pReference;
	begin
		Assert(lock.AcquiredAssert);
		if not fromCompletionCallback and Assigned(f^.aw) then
		begin
			f^.locked := FolderRemoved;
		{$ifdef Windows} CancelIoEx(f^.f.handle, @pAsyncWrite(f^.aw)^.op); {$endif}
		end else
		begin
			Assert(f^.id = fi, 'folder.id = ' + ToString(f^.id) + ', должно быть ' + ToString(fi));
			Assert(folders[fi] = f);
			folders[fi] := folders[High(folders)];
			folders[fi]^.id := fi;
			for watch in folders[fi]^.watches do
				watch^.folderIdInRegistry := fi;
			SetLength(folders, length(folders) - 1);
			f^.f.Close(fromCompletionCallback);
			dispose(f);
		end;
	end;

{$define accessor := FileWatch.Registry} {$define instance_type := FileWatch.pFolderRegistry}
{$define instance_name := FolderRegistryInstance} {$define create_instance := FileWatch.FolderRegistry.Create} {$define destroy_instance := instance^.Free}
{$include lazy_singleton.inc}

	function GetFileAttributes(const filename: string): FileAttributes;
{$ifdef Windows}
	begin
		result := Win.ToFileAttributes(GetFileAttributesW(pWideChar(Win.ToWideFileName(filename))));
	end;
{$else} {$error GetFileAttributes unimplemented} {$endif}

const
	SystemSeparator = {$if defined(Windows)} '\' {$else} '/' {$endif};

	function ToSystemFileName(const fileName: string): string;
	var
		i: sint;
	begin
		result := fileName;
		for i := 1 to length(result) do
			case result[i] of
				FileSeparator: result[i] := SystemSeparator;
			{$if defined(Debug) and defined(Windows)}
				'\': raise Error('Ожидается разделитель файлов ' + FileSeparator + ' (' + fileName + ').' + EOL + GetBackTrace);
			{$endif}
			end;
	end;

	function FromSystemFileName(const fileName: string): string;
	var
		i: sint;
	begin
		result := fileName;
		if (length(result) >= 2) and (result[1] = '"') and (result[length(result)] = '"') then result := Copy(result, 2, length(result) - 2);
		for i := 1 to length(result) do
			case result[i] of
				SystemSeparator: result[i] := FileSeparator;
			{$if defined(Debug) and defined(Windows)}
				FileSeparator: raise Error('В системных именах файлов ожидается разделитель ' + SystemSeparator + '.');
			{$endif}
			end;
	end;

{$ifdef Windows}
	procedure QueryShortPathName(buf: pWideChar; nBuf: size_t; out len: size_t; param: pointer);
	begin
		len := GetShortPathNameW(pWideChar(param), buf, nBuf);
		if len = 0 then raise Win.FunctionFailed('GetShortPathName');
	end;
{$endif}

	function ToShortSystemFileName(const fileName: string): string;
	{$ifdef Windows} var wfn: widestring; {$endif}
	begin
	{$ifdef Windows}
		wfn := Win.ToWideFileName(fileName);
		result := UTF8Encode(Win.QueryString(@QueryShortPathName, pWideChar(wfn), 'короткого имени файла'));
	{$else}
		{$note Dummy ToShortSystemFileName}
		result := ToSystemFileName(fileName);
	{$endif}
	end;

	function GetFileVersion(const fn: string; what: FileVersionEnum = FileVersion): string;
{$ifdef Windows}
	const
		LangCodepage = widestring('041904B0');  // concatenation of language and encoding
		                                        // (http://msdn.microsoft.com/en-us/library/windows/desktop/ms647464%28v=vs.85%29.aspx)
		                                        // 0C00 — neutral language; 0419 — russian; 04B0 — UTF-16
		                                        // (http://msdn.microsoft.com/en-us/library/windows/desktop/dd318693%28v=vs.85%29.aspx)
		StringVerId: array[FileVersionEnum] of widestring = ('FileVersion', 'InternalName');
	var
		wfn: widestring;
		sz: size_t;
		block, ptr: pointer;
		ptrlen: Windows.UINT;
	begin
		result := '';
		wfn := Win.ToWideFileName(fn);
		sz := GetFileVersionInfoSizeW(pWideChar(wfn), dword(nil^));
		if sz = 0 then exit;

		block := GetMem(sz);
		if GetFileVersionInfoW(pWideChar(wfn), 0, sz, block) then
		begin
			if StringVerId[what] <> '' then
			begin
				if VerQueryValueW(block, pWideChar(widestring('\StringFileInfo\' + LangCodepage + '\' + StringVerId[what])), (@ptr)^, (@ptrlen)^) and (ptrlen > 0) then
					result := UTF8Encode(widestring(pWideChar(ptr)));
			end;
		end;
		FreeMem(block);
	end;
{$else}
	begin
		Assert((@fn = @fn) and (@what = @what));
		result := '';
	end;
	{$error GetFileVersion unimplemented}
{$endif}

	function GetExecVersion(what: FileVersionEnum = FileVersion): string;
	begin
		result := GetFileVersion(ExecFileName, what);
	end;

	function GetSystemLanguage(const fallback: string): string;
	{$ifdef Windows}
	const
		PrimaryLanguageMask = 1 shl 10 - 1;
		LANG_RUSSIAN    = $19;
		LANG_BELARUSIAN = $23;
		LANG_FRENCH     = $C;
		LANG_ENGLISH    = $9;
		LANG_JAPANESE   = $11;
	var
		langID: dword;
	{$endif}
	begin
		result := fallback;
	{$ifdef Windows}
		langID := GetUserDefaultUILanguage();

		case langID and PrimaryLanguageMask of
			LANG_RUSSIAN:    result := 'ru';
			LANG_BELARUSIAN: result := 'be';
			LANG_FRENCH:     result := 'fr';
			LANG_ENGLISH:    result := 'en';
			LANG_JAPANESE:   result := 'jp';
		end;
	{$else} {$error GetSystemLanguage unimplemented} {$endif}
	end;

	procedure Process.Open(out process: Process; const exe: string; const args: array of string; flags: OpenFlags = []);
	{$ifdef Windows}
		function Quote(const arg: string): string;
		var
			slashes: uint;
			i: sint;
			marks: boolean;
		begin
			result := '';
			marks := no;
			i := 1;
			repeat
				slashes := 0;
				while (i <= length(arg)) and (arg[i] = '\') do begin inc(i); inc(slashes); end;
				if (i > length(arg)) or (arg[i] = '"') then slashes := 2*slashes + ord((i <= length(arg)) and (arg[i] = '"'));
				while slashes > 0 do begin result += '\'; dec(slashes); end;
				if i > length(arg) then break;
				result += arg[i];

				marks := marks or (arg[i] in [' ', TabSym, #$B {\v, vertical tab}, EOL, '"']);
				inc(i);
			until no;
			if marks then result := '"' + result + '"';
		end;

		procedure _Impl;
		var
			cmdline: widestring;
			i: sint;
			startup: Windows.STARTUPINFOW;
			info: Windows.PROCESS_INFORMATION;
			wf: Windows.DWORD;
			code: dword;
		begin
			cmdline := UTF8Decode(Quote(ToSystemFileName(exe)));
			for i := 0 to High(args) do
				cmdline += UTF8Decode(' ' + Quote(args[i]));
			Zero(@startup, sizeof(startup));
			startup.cb := sizeof(startup);
			wf := CREATE_UNICODE_ENVIRONMENT;
			if Silent in flags then wf := wf or CREATE_NO_WINDOW;

			if not CreateProcessW(pWideChar(Win.ToWideFileName(exe)), pWideChar(cmdline), nil, nil, no, wf, nil, nil, startup, (@info)^) then
			begin
				code := GetLastError;
				raise Win.Error('Не удалось создать процесс ' + Folder.Filename(exe) + '.', code);
			end;
			process.handle := info.hProcess;
			process.mainThread := info.hThread;
		end;
	{$else} {$error Process.Open unimplemented} {$endif}
	begin
		process.handle := 0;
		process.mainThread := 0;
		_Impl;
	end;

	procedure Process.Close;
	begin
		if handle <> 0 then
		begin
		{$ifdef Windows}
			CloseHandle(handle);
			CloseHandle(mainThread);
		{$else} {$error Process.Close unimplemented} {$endif}
			handle := 0;
			mainThread := 0;
		end;
	end;

	procedure Process.Wait;
	begin
	{$ifdef Windows}
		WaitForSingleObject(handle, Windows.INFINITE);
	{$else} {$error Process.Wait unimplemented} {$endif}
	end;

	function Process.ExitCode: sint;
	{$ifdef Windows}
		procedure _Impl;
		var
			code: dword;
		begin
			Wait;
			if GetExitCodeProcess(handle, (@code)^) and (code <> Windows.STILL_ACTIVE) then result := code;
		end;
	{$else} {$error Process.ExitCode unimplemented} {$endif}
	begin
		result := -1;
		if handle <> 0 then _Impl;
	end;

	function DynamicLibrary.FunctionDesc.Load(var lib: DynamicLibrary): boolean;
	begin
		pPointer(fn)^ := lib.FindProc(name);
		result := Assigned(pPointer(fn)^);
	end;

	function DynamicLibrary.FunctionDesc.Load(const desc: array of FunctionDesc; var lib: DynamicLibrary; const fail: string): boolean;
	var
		i, j: sint;
	begin
		result := yes;
		for i := 0 to High(desc) do
		begin
			result := result and desc[i].Load(lib);

			if not result then
			begin
				for j := 0 to High(desc) do
					pPointer(desc[j].fn)^ := nil;
				if fail <> '' then raise Error('Не удалось загрузить ' + fail + ' (' + desc[i].name + ').');
			end;
		end;
	end;

	function DynamicLibrary.Invalid: DynamicLibrary;
	begin
		result.handle := 0;
	end;

	procedure DynamicLibrary.Open(out lib: DynamicLibrary; const filename: string);
	begin
		Open(lib, filename, yes);
	end;

	function DynamicLibrary.TryOpen(out lib: DynamicLibrary; const filename: string): boolean;
	begin
		Open(lib, filename, no);
		result := lib.OK;
	end;

	function DynamicLibrary.OK: boolean;
	begin
		result := handle <> 0;
	end;

	function DynamicLibrary.Close: boolean;
	{$if defined(Windows) and defined(Debug)} var code: dword; {$endif}
	begin
		result := OK;
		if result then
		begin
		{$ifdef Windows}
			result := FreeLibrary(handle);
		{$ifdef Debug}
			if not result then
			begin
				code := GetLastError;
				Warning.Expanded('handle = $' + HexStr(handle, bitsizeof(handle) div 4) + EOL + Win.DescribeError(code))
					.Show('Не удалось закрыть библиотеку ' + fn + '.');
			end;
		{$endif}
		{$else} {$error tDynamicLibrary.Close unimplemented} {$endif}
		FPC_3_BUG System.Finalize(self);
			self := Invalid;
		end;
	end;

	function DynamicLibrary.FindProc(const name: string): pointer;
	begin
		Assert(OK, 'библиотека не открыта (' + name + ')');
	{$ifdef Windows}
		result := Windows.GetProcAddress(handle, pChar(name));
	{$else} {$error tDynamicLibrary.FindProc unimplemented} {$endif}
	end;

	procedure DynamicLibrary.Open(out lib: DynamicLibrary; const filename: string; throw: boolean);
	begin
	FPC_3_BUG System.Initialize(lib);
	{$ifdef Debug} lib.fn := ToSystemFileName(filename); {$endif}
	{$ifdef Windows}
		lib.handle := LoadLibraryW(pWideChar(Win.ToWideFileName(filename)));
		if (lib.handle = 0) and throw then raise Win.FileLoadError(filename, GetLastError);
	{$else} {$error tDynamicLibrary.Open unimplemented} {$endif}
	end;

	function Color.RGB(const newR, newG, newB: float): Color;
	begin
		result._r := newR;
		result._g := newG;
		result._b := newB;
	end;

	function Color.Intensity: float;
	begin
		result := _r * 0.39 + _g * 0.5 + _b * 0.11;
	end;

{$ifdef Windows}
	procedure QueryEnv(buf: pWideChar; nBuf: size_t; out len: size_t; param: pointer);
	begin
		len := Windows.GetEnvironmentVariableW(pWideChar(param), buf, nBuf)
	end;
{$endif}

	function GetEnv(const name: string): string;
{$ifdef Windows}
	var
		wname: widestring;
	begin
		wname := UTF8Decode(name);
		try
			result := UTF8Encode(Win.QueryString(@QueryEnv, pWideChar(wname), 'имени переменной окружения'));
		except
			result := '';
		end;
	end;
{$else} {$error GetEnv unimplemented} {$endif}

	function ExecFileName: string;
	writeable_const_ Cached: string = ''; _end
	begin
		result := Cached; if result <> '' then exit;
	{$ifdef Windows}
		result := Win.FromWideFileName(Win.ModuleFileNameW(no));
	{$else} {$error ExecFileName unimplemented} {$endif}
		Cached := result;
	end;

	function CommandLine.Get: CommandLine;
{$ifdef Windows}
	var
		cmd: pWideChar;
		nParams: sint32;
{$endif}
	begin
	{$ifdef Debug} result.guard := GetMem(1); {$endif}
	{$ifdef Windows}
		result.argv := nil;
		result.nargv := 0;

		cmd := GetCommandLineW;
		if Assigned(cmd) then
		begin
			result.argv := CommandLineToArgvW(cmd, nParams);
			if nParams > 0 then result.nargv := nParams - 1 else result.nargv := 0;
		end;
	{$else} {$error CommandLine.Get unimplemented} {$endif}
	end;

	procedure CommandLine.Done;
	begin
	{$ifdef Debug}
		Assert(Assigned(guard));
		FreeMem(guard);
	{$endif}
	{$ifdef Windows}
		if Assigned(argv) then LocalFree(pPtrUint(@argv)^);
	{$else} {$error CommandLine.Done unimplemented} {$endif}
	end;

	function CommandLine.Count: uint;
	begin
		result := {$ifdef Windows} nargv {$else} {$error CommandLine.Count unimplemented} {$endif};
	end;

	function CommandLine.Param(id: uint): string;
	begin
		if id >= Count then exit('');
		result := {$ifdef Windows} UTF8Encode(widestring(argv[1 + id])) {$else} {$error CommandLine.Param unimplemented} {$endif};
	end;

	function CommandLine.Raw: string;
	begin
		result := {$ifdef Windows} UTF8Encode(widestring(GetCommandLineW)) {$else} {$error CommandLine.Raw unimplemented} {$endif}
	end;

	function CommandLine.ToStrings: Strings;
	var
		i: uint;
	begin
		SetLength(result, Count);
		for i := 1 to length(result) do
			result[i - 1] := self[i - 1];
	end;

	procedure Heap.Create(out h: Heap; const name: string; flags: FlagSet = []);
	begin
	FPC_3_BUG System.Initialize(h);
	{$ifdef Windows}
		h.wflags := 0; if not (Lock in flags) then h.wflags := h.wflags or HEAP_NO_SERIALIZE;
		h.handle := HeapCreate(h.wflags, 0, 0);
		if h.handle = 0 then raise Win.FunctionFailed('HeapCreate');
	{$else} {$error Heap.Create unimplemented} {$endif}
	{$ifdef Debug} h.name := name; {$else} unused_args name end_list {$endif}
	end;

	procedure Heap.Close;
{$if defined(Windows) and defined(Debug)} var code: dword; {$endif}
	begin
		if not OK then exit;
	{$ifdef Windows}
		if not HeapDestroy(handle) then
		begin
		{$ifdef Debug}
			code := GetLastError;
			Warning.Expanded(Win.DescribeError(code)).Show('Не удалось уничтожить кучу ' + name + '.');
		{$endif}
		end;
	{$else} {$error Heap.Close unimplemented} {$endif}
	FPC_3_BUG System.Finalize(self);
		self := Invalid;
	end;

	function Heap.Invalid: Heap; begin result.handle := 0; end;
	function Heap.OK: boolean; begin result := handle <> 0; end;

	function Heap.Alloc(size: size_t): pointer;
	begin
	{$ifdef Windows}
		if size = 0 then result := nil else result := HeapAlloc(handle, wflags, size);
	{$else} {$error Heap.Alloc unimplemented} {$endif}
	end;

	procedure Heap.Free(ptr: pointer);
{$if defined(Windows) and defined(Debug)} var code: dword; {$endif}
	begin
	{$ifdef Windows}
		if Assigned(ptr) and not HeapFree(handle, wflags, ptr) then
		begin
		{$ifdef Debug}
			code := GetLastError;
			Warning.Expanded(Win.DescribeError(code)).Show('Не удалось освободить блок $' + HexStr(ptr) + ' (куча ' + name + ').');
		{$endif}
		end;
	{$else} {$error Heap.Alloc unimplemented} {$endif}
	end;

	function Heap.Realloc(ptr: pointer; size: size_t): pointer;
	begin
	{$ifdef Windows}
		if Assigned(ptr) and (size > 0) then result := HeapReAlloc(handle, wflags, ptr, size)
		else if not Assigned(ptr) then result := Alloc(size)
		else begin Free(ptr); result := nil; end;
	{$else} {$error Heap.Alloc unimplemented} {$endif}
	end;

	function HeapDump.Get: HeapDump;
	var
		procHeap: Windows.HANDLE;
		entry: Win.PROCESS_HEAP_ENTRY;
		blocksAllocated: sint;
		t: pointer;
	begin
		result.nBlocks := 0;
		result.blocks  := nil;
		result.tmpHeap := Heap.Invalid;
		blocksAllocated := 0;

		try
			Heap.Create(result.tmpHeap, 'HeapDump');
		{$ifdef Windows}
			procHeap := GetProcessHeap;
			HeapLock(procHeap);
			try
				entry.lpData := nil;
				while HeapWalk(procHeap, entry) do
				begin
					inc(result.nBlocks);
					if result.nBlocks > blocksAllocated then
					begin
						blocksAllocated := 2 * result.nBlocks;
						t := result.tmpHeap.Realloc(result.blocks, blocksAllocated * sizeof(Block));
						if not Assigned(t) then raise Error('Не удалось распределить дамп кучи.');
						result.blocks := t;
					end;

					pPointer(@result.blocks[result.nBlocks - 1].address)^ := entry.lpData;
					result.blocks[result.nBlocks - 1].size := entry.cbData;
				end;
				if GetLastError <> ERROR_NO_MORE_ITEMS then
					raise Win.FunctionFailed('HeapWalk');
			finally
				HeapUnlock(procHeap);
			end;
		{$else} {$error HeapDump.Get unimplementeD} {$endif}
		except
			result.Close;
			raise;
		end;
	end;

	procedure HeapDump.Close;
	begin
		tmpHeap.Free(blocks); blocks := nil;
		tmpHeap.Close;
	end;

	class procedure Crypt.Random(buf: pointer; len: size_t);
	{$ifdef Windows} var prov: HCRYPTPROV; {$endif}
	begin
	{$ifdef Windows}
		prov := GetProvider;
		if not Win.advapi32.CryptGenRandom(prov, len, buf) then raise Win.FunctionFailed('CryptoGenRandom');
	{$else} {$error Crypt.Random unimplemented} {$endif}
	end;

{$define impl:= begin Random(@result, sizeof(result)); end;}
	class function Crypt.Random32: uint32; impl
	class function Crypt.Random64: uint64; impl
{$undef impl}

	class procedure Crypt.Initialize;
	begin
	{$ifdef Windows} Provider := nil; {$endif}
	end;

	class procedure Crypt.Finalize;
	begin
	{$ifdef Windows}
		if Assigned(Provider) then
		begin
			if not Win.advapi32.CryptReleaseContext(Provider, 0) then
		{$ifdef Debug} try raise Win.FunctionFailed('CryptReleaseContext'); except Exception.Show; end {$endif};
			Provider := nil;
		end;
	{$endif}
	end;

{$ifdef Windows}
	class function Crypt.GetProvider: HCRYPTPROV;
	begin
		result := Provider; if Assigned(result) then exit;
		SingletonLock.Enter;
		try
			result := Provider; if Assigned(result) then exit;
			Win.EnsureCryptoAPI;
			if not Win.advapi32.CryptAcquireContextW(result, nil, nil, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT) or not Assigned(result) then
				raise Win.FunctionFailed('CryptAcquireContext.');
			Provider := result;
		finally
			SingletonLock.Leave;
		end;
	end;
{$endif}

unchecked
	procedure NonCrucialRandom(buf: pointer; len: size_t);
	var
		x: uint32;
	begin
		x := uint32(Ticks.Get.value);
		while len > sizeof(x) do
		begin
			uint32(buf^) := x;
			buf += sizeof(x); len -= sizeof(x);
			if len > 0 then x := x * 48271 mod 2147483647; // C++ std::minstd_rand
		end;

		while len > 0 do
		begin
			pByte(buf)^ := x and High(byte);
			buf += sizeof(byte); len -= sizeof(byte); x := x shr bitsizeof(byte);
		end;
	end;
end_unchecked

	operator =(const a, b: Color): boolean;
	begin
		result := (abs(a._r - b._r) < CloseToZeroEps) and (abs(a._g - b._g) < CloseToZeroEps) and (abs(a._b - b._b) < CloseToZeroEps);
	end;

{$ifdef use_console}
	procedure Console.Initialize;
	begin
		lock.Init;
	{$ifdef Windows}
		hIn := CreateFile('CONIN$',  GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
		if hIn = INVALID_HANDLE_VALUE then raise Win.OperationFailed('открыть дескриптор консоли для ввода');
		hOut := CreateFile('CONOUT$',  GENERIC_READ or GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
		if hOut = INVALID_HANDLE_VALUE then raise Win.OperationFailed('открыть дескриптор консоли для вывода');
	{$else} {$error Console.Initialize unimplemented} {$endif}
	end;

	procedure Console.Finalize;
	begin
	{$ifdef Windows}
		CloseHandle(hIn);
		CloseHandle(hOut);
	{$endif}
		lock.Done;
	end;

	procedure Console.GetSizes(out w, h: sint);
	{$ifdef Windows} var info: CONSOLE_SCREEN_BUFFER_INFO; {$endif}
	begin
	{$ifdef Windows}
		GetConsoleScreenBufferInfo(hOut, (@info)^);
		w := info.dwSize.X;
		h := info.dwSize.Y;
	{$else} {$error Console.GetSizes unimlemented} {$endif}
	end;

	procedure Console.SetSizes(w, h: sint);
{$ifdef Windows}
	begin
		SetConsoleScreenBufferSize(hOut, Win.Coord(w, h));
	end;
{$else} {$error Console.SetSizes unimplemented} {$endif}

	function Console.GetSize(id: sint): sint;
	var
		w, h: sint;
	begin
		GetSizes(w, h);
		if id = 0 then result := w else result := h;
	end;

	procedure Console.SetSize(id: sint; value: sint);
	var
		w, h: sint;
	begin
		GetSizes(w, h);
		if id = 0 then w := value else h := value;
		SetSizes(w, h);
	end;

	function Console.GetSizeX: sint;           begin result := GetSize(0); end;
	procedure Console.SetSizeX(newSize: sint); begin SetSize(0, newSize); end;
	function Console.GetSizeY: sint;           begin result := GetSize(1); end;
	procedure Console.SetSizeY(newSize: sint); begin SetSize(1, newSize); end;

	procedure Console.SetPosition(x, y: sint);
{$ifdef Windows}
	begin
		SetConsoleCursorPosition(hOut, Win.Coord(x, y));
	end;
{$else} {$error ConSetPosition unimplemented} {$endif}

	procedure Console.SetColor(const color: Color);
{$ifdef Windows}
	writeable_const_ OldF: WORD = High(WORD); _end
	var
		f: WORD;
	begin
		lock.Enter;
		f := Win.TextAttribute(color);
		if f <> OldF then
		begin
			SetConsoleTextAttribute(hOut, f);
			OldF := f;
		end;
		lock.Leave;
	end;
{$else} {$error ConSetColor unimplemented} {$endif}

	procedure Console.Write(const s: string);
{$ifdef Windows}
	const
		BlockSize = 4096;
	var
		ws: widestring;
		p: pWideChar;
		n, rest: size_t;
		written: dword;
	begin
		ws := UTF8Decode(s);
		p := pWideChar(ws);
		rest := length(ws);
		lock.Enter;
		while rest > 0 do
		begin
			n := rest;
			if n > BlockSize then n := BlockSize;

			WriteConsoleW(hOut, p, n, (@written)^, nil);
			if written <> n then break;
			p += n;
			rest -= n;
		end;
		lock.Leave;
	end;
{$else} {$error ConWriteUTF8 unimplemented} {$endif}

	procedure Console.Write(const c: char; n: uint);
	begin
		while n > 0 do begin Write(c); dec(n); end;
	end;

	procedure Console.WriteLine(const s: string = '');
	begin
		Write(s + EOL);
	end;

	procedure Console.Write(x, y: sint; nChars: sint; stride: size_t; sym: pUTFchar; col: pColor);
	var
		syms: array of {$ifdef Windows} widechar {$else} {$error} {$endif};
		cols: array of {$ifdef Windows} WORD {$else} {$error} {$endif};
		i: sint;
	{$ifdef Windows}
		symid: sint;
		written: DWORD;
	{$endif}
	begin
		if nChars <= 0 then exit;
		if Assigned(sym) then SetLength(syms, UTF16.MaxCharLen * nChars);
		if Assigned(col) then SetLength(cols, nChars);
		if Assigned(sym) then
		begin
			symid := 0;
			for i := 0 to nChars - 1 do
			begin
				symid += UTF16.CodepointToString(sym^, @syms[symid]);
				pointer(sym) += stride;
			end;
		end;
		if Assigned(col) then
			for i := 0 to nChars - 1 do
			begin
				cols[i] := Win.TextAttribute(col^);
				pointer(col) += stride;
			end;
	{$ifdef Windows}
		written := 0;
		lock.Enter;
		if Assigned(col) then WriteConsoleOutputAttribute(hOut, pointer(cols), nChars, Win.Coord(x, y), written);
		if Assigned(sym) then WriteConsoleOutputCharacterW(hOut, pointer(syms), symid, Win.Coord(x, y), written);
		lock.Leave;
	{$else} {$error ConWrite unimplemented} {$endif}
	end;

	procedure Console.Write(x, y: sint; sym: UTFchar; count: uint);
	{$ifdef Windows}
	var
		u16: array[0 .. UTF16.MaxCharLen - 1] of widechar;
		written: dword;
	begin
		UTF16.CodepointToString(sym, u16);
		FillConsoleOutputCharacterW(hOut, u16[0], count, Win.Coord(x, y), (@written)^);
	end;
	{$else} {$error Console.Write(char, count) unimplemented} {$endif}

	procedure Console.Input(const s: string);
{$ifdef Windows}
	var
		ws: widestring;
		inp: array of INPUT_RECORD;
		i: sint;
		written: dword;
	begin
		ws := UTF8Decode(s);
		if ws = '' then exit;

		SetLength(inp, length(ws));
		for i := 0 to High(inp) do
		begin
			inp[i].EventType := KEY_EVENT;
			with inp[i].Event.KeyEvent do
			begin
				bKeyDown          := yes;
				wRepeatCount      := 0;
				wVirtualKeyCode   := 0;
				wVirtualScanCode  := 0;
				UnicodeChar       := ws[1 + i];
				dwControlKeyState := 0;
			end;
		end;
		written := 0;
		WriteConsoleInputW(hIn, inp[0], length(inp), written);
	end;
{$else} {$error ConInputUTF8 unimplemented} {$endif}

	function Console.ReadLine: string;
{$ifdef Windows}
	var
		buf: packed array[0 .. 19] of widechar;
		t: widestring;
		readed, uptoline: dword;
	begin
		t := '';
		readed := 0;
		// W... T... F?! Без этого после первой после WriteConsoleInput ReadConsole в buf оказывается мусор, и только если его размер был недостаточным.
		// FlushConsoleInputBuffer перед WriteConsoleInput бесполезна.
		ReadConsoleW(hIn, nil, 0, readed, nil);

		while ReadConsoleW(hIn, @buf, length(buf), readed, nil) and (readed > 0) do
		begin
			uptoline := readed;
			while (uptoline > 0) and (buf[uptoline - 1] in [#13, #10]) do
				dec(uptoline);
			t += Copy(buf, 0, uptoline);
			if buf[readed - 1] = #10 then break; // Если, скажем, дочитать до #13 и бросить, то в буфере останется #10.
		end;

		result := UTF8Encode(t);
	end;
{$endif}

	function Console.ScanKey(out key: KeyboardKey): boolean;
{$ifdef Windows}
	var
		ir: INPUT_RECORD;
		readed: DWORD;
	begin
		FlushConsoleInputBuffer(hIn);
		readed := 0;

		while ReadConsoleInputW(hIn, (@ir)^, 1, readed) do
			// key press
			if (ir.EventType = KEY_EVENT) and (ir.Event.KeyEvent.bKeyDown)
				and (ir.Event.KeyEvent.wVirtualKeyCode <> VK_SHIFT)
				and (ir.Event.KeyEvent.wVirtualKeyCode <> VK_MENU)
				and (ir.Event.KeyEvent.wVirtualKeyCode <> VK_CONTROL)
			then
			begin
				result := WindowsSpecific.DecryptKey(ir.Event.KeyEvent.wVirtualKeyCode, nil, key);
				// cChar = irInputRecord.Event.KeyEvent.uChar.AsciiChar;
				// ReadConsoleInputW(winInput, ir, 1, readed); // key release
				exit;
			end;
		result := no;
	end;
{$else} {$error ConScanKey unimplemented} {$endif}

	procedure Console.ScanKey;
	var
		key: KeyboardKey;
	begin
		ScanKey(key);
	end;
{$endif}

	function UnitRegistry.UnitRef.Initialize(proc: InitFinalProc): UnitRef; begin result := self; It^.initialize := proc; end;
	function UnitRegistry.UnitRef.Initialize(initProc, finalProc: InitFinalProc): UnitRef; begin result := Initialize(initProc).Finalize(finalProc); end;
	function UnitRegistry.UnitRef.Finalize(proc: InitFinalProc): UnitRef;   begin result := self; It^.finalize := proc;   end;
	function UnitRegistry.UnitRef.Priority(value: sint): UnitRef;           begin result := self; It^.priority := value;  end;
{$ifdef selftest}
	function UnitRegistry.UnitRef.Test(newTest: TestProc): UnitRef;         begin result := self; It^.test := newTest;    end;
{$endif}

	procedure UnitRegistry.Init;
	begin
		initialized := no;
		list.Init;
	end;

	procedure UnitRegistry.Done;
	begin
		list.Done;
	end;

	function NameMatches(const item: UnitRegistry.ItemRec; param: pointer): boolean; begin result := item.name = pString(param)^; end;

	function UnitRegistry.Add(const name: string): UnitRef;
	var
		id: uint;
		item: ^ItemRec;
	begin
		Assert(not initialized, 'Модули уже инициализированы.');
		Assert(not list.Find(@NameMatches, @name, id));
		item := list.Grow(1);
		item^ := ItemRec.Empty;
		item^.name := name;
		result.It := item;
	end;

	procedure UnitRegistry.AddSingleton(const unitname: string; finalize: InitFinalProc);
	var
		id: uint;
		ns: SingletonRec;
	begin
		if not list.Find(@NameMatches, @unitname, id) then
		begin
		{$ifdef Debug} Error.Show('Модуль ' + unitname + ' не найден.'); {$endif}
			exit;
		end;

		ns.name     := 'TODO';
		ns.finalize := finalize;
		SetLength(list.items[id].singletons, length(list.items[id].singletons) + 1);
		list.items[id].singletons[High(list.items[id].singletons)] := ns;
	end;

	procedure InitializeUnit(const item: UnitRegistry.ItemRec);
	begin
		if Assigned(item.initialize) then item.initialize();
	end;

	procedure FinalizeUnitSingletons(const item: UnitRegistry.ItemRec);
	var
		i: sint;
	begin
		for i := High(item.singletons) downto 0 do
			if Assigned(item.singletons[i].finalize) then
				item.singletons[i].finalize();
	end;

	procedure FinalizeUnit(const item: UnitRegistry.ItemRec);
	begin
		if Assigned(item.finalize) then item.finalize();
	end;

	procedure FinalizeUnits; begin units.FinalizeAll; end;

	procedure UnitRegistry.InitializeAll(autoFinalize: boolean = yes);
		{$define elem := ItemRec} {$define no_math} {$define less := _1.priority > _2.priority} {$include sort.inc}
	begin
		Assert(@self = @units);
		Assert(not initialized, 'Модули уже инициализированы.');
		initialized := yes;
		sort(list.items, list.n);
		try
			list.ForEach(@InitializeUnit);
		except
			Exception.Fatal;
		end;
		list.Pack;
		if autoFinalize then AddExitProc(@FinalizeUnits);
	{$ifdef selftest} TestAll; {$endif}
	end;

	procedure UnitRegistry.FinalizeAll;
	begin
		Assert(initialized, 'Модули не инициализированы.');
		initialized := no;
		list.ForEach(@FinalizeUnitSingletons);
		list.ForEach(@FinalizeUnit, [list.ReversedOrder]);
		list.Clear;
	end;
	{$define classname := UnitRegistry.ItemsList} {$include vector.pp.inc}

{$ifdef selftest}
	procedure TestUnit(const item: UnitRegistry.ItemRec);
	begin
		if Assigned(item.test) then item.test();
	end;

	procedure UnitRegistry.TestAll;
	begin
		list.ForEach(@TestUnit);
	end;
{$endif}

	function &Unit(const name: string): UnitRegistry.UnitRef;
	begin
		result := units.Add(name);
	end;

{$ifdef Debug}
	procedure Statistics.Init;
	begin
		lock.Init;
	end;

	procedure Statistics.Done;
	begin
		lock.Done;
	end;

	function Statistics.Note(what: MaxEnum; value: uint): boolean;
	begin
		lock.Enter;
		result := max[what] < value;
		if result then max[what] := value;
		lock.Leave;
	end;

	procedure Statistics.Increment(what: TotalEnum);
	begin
		InterlockedIncrement(total[what]);
	end;

	procedure Statistics.Increment(what: CurrentAndMax);
	var
		v: casint_t;
	begin
		v := InterlockedIncrement(cam[what].current);
		lock.Enter;
		if v > cam[what].max then cam[what].max := v;
		lock.Leave;
	end;

	procedure Statistics.Decrement(what: CurrentAndMax);
	begin
		InterlockedDecrement(cam[what].current);
	end;

	procedure Statistics.Note(what: JustValue; value: sint);
	begin
		jv[what] := value;
	end;

{$define impl:=
	begin
		lock.Enter;
		total[what] += by;
		lock.Leave;
	end;}
	procedure Statistics.Increment(what: TotalSizeEnum; by: size_t); {$define total := totalSize} impl
	procedure Statistics.Increment(what: TotalHpFloat; const by: hp_float); {$define total := totalHpf} impl
{$undef total} {$undef impl}
{$endif}

{$ifdef Windows}
	class procedure Win.Initialize;
	const
		SRWLockFns: array[0 .. 6] of DynamicLibrary.FunctionDesc =
		(
			(fn: @SRW.InitializeSRWLock;          name: 'InitializeSRWLock' {$ifdef DisableSRW} {$note SRW locks are disabled} + '-' {$endif}),
			(fn: @SRW.TryAcquireSRWLockExclusive; name: 'TryAcquireSRWLockExclusive'),
			(fn: @SRW.AcquireSRWLockExclusive;    name: 'AcquireSRWLockExclusive'),
			(fn: @SRW.ReleaseSRWLockExclusive;    name: 'ReleaseSRWLockExclusive'),
			(fn: @SRW.TryAcquireSRWLockShared;    name: 'TryAcquireSRWLockShared'),
			(fn: @SRW.AcquireSRWLockShared;       name: 'AcquireSRWLockShared'),
			(fn: @SRW.ReleaseSRWLockShared;       name: 'ReleaseSRWLockShared')
		);
		CVFns: array[0 .. 4] of DynamicLibrary.FunctionDesc =
		(
			(fn: @CV.InitializeConditionVariable; name: 'InitializeConditionVariable' {$ifdef DisableCV} {$note Condition variables are disabled} + '-' {$endif}),
			(fn: @CV.SleepConditionVariableCS;    name: 'SleepConditionVariableCS'),
			(fn: @CV.SleepConditionVariableSRW;   name: 'SleepConditionVariableSRW'),
			(fn: @CV.WakeAllConditionVariable;    name: 'WakeAllConditionVariable'),
			(fn: @CV.WakeConditionVariable;       name: 'WakeConditionVariable')
		);
		VistaTPFns: array[0 .. 13] of DynamicLibrary.FunctionDesc =
		(
			(fn: @VistaTP.CreateThreadpoolWork;   name: 'CreateThreadpoolWork' {$ifdef DisableVistaTP} {$note Vista threadpool API disabled} + '-' {$endif}),
			(fn: @VistaTP.SubmitThreadpoolWork;   name: 'SubmitThreadpoolWork'),
			(fn: @VistaTP.CloseThreadpoolWork;    name: 'CloseThreadpoolWork'),
			(fn: @VistaTP.WaitForThreadpoolWorkCallbacks; name: 'WaitForThreadpoolWorkCallbacks'),

			(fn: @VistaTP.CreateThreadpoolTimer;  name: 'CreateThreadpoolTimer'),
			(fn: @VistaTP.SetThreadpoolTimer;     name: 'SetThreadpoolTimer'),
			(fn: @VistaTP.WaitForThreadpoolTimerCallbacks; name: 'WaitForThreadpoolTimerCallbacks'),
			(fn: @VistaTP.CloseThreadpoolTimer;   name: 'CloseThreadpoolTimer'),

			(fn: @VistaTP.CreateThreadpoolIo;     name: 'CreateThreadpoolIo'),
			(fn: @VistaTP.StartThreadpoolIo;      name: 'StartThreadpoolIo'),

			(fn: @VistaTP.CancelThreadpoolIo;     name: 'CancelThreadpoolIo'),
			(fn: @VistaTP.WaitForThreadpoolIoCallbacks; name: 'WaitForThreadpoolIoCallbacks'),
			(fn: @VistaTP.CloseThreadpoolIo;      name: 'CloseThreadpoolIo'),

			(fn: @VistaTP.CallbackMayRunLong;     name: 'CallbackMayRunLong')
		);
		XPTpFns: array[0 .. 6] of DynamicLibrary.FunctionDesc =
		(
			(fn: @XPTp.BindIoCompletionCallback; name: 'BindIoCompletionCallback'),
			(fn: @XPTp.QueueUserWorkItem;        name: 'QueueUserWorkItem'),

			(fn: @XPTp.CreateTimerQueue;         name: 'CreateTimerQueue'),
			(fn: @XPTp.DeleteTimerQueueEx;       name: 'DeleteTimerQueueEx'),
			(fn: @XPTp.CreateTimerQueueTimer;    name: 'CreateTimerQueueTimer'),
			(fn: @XPTp.DeleteTimerQueueTimer;    name: 'DeleteTimerQueueTimer'),
			(fn: @XPTp.ChangeTimerQueueTimer;    name: 'ChangeTimerQueueTimer')
		);

	var
		ke32: DynamicLibrary;
		sysinfo: SYSTEM_INFO;
	begin
		version.dwOSVersionInfoSize := sizeof(version);
		if not GetVersionEx(version) then raise FunctionFailed('GetVersionEx');
	{$ifdef Debug} timeBeginPeriod(TimeQuantMs); {$endif}

		sysinfo.dwNumberOfProcessors := 0;
		GetSystemInfo(sysinfo);
		if sysinfo.dwNumberOfProcessors > 0 then SystemInfo.nCPUs := sysinfo.dwNumberOfProcessors;
		if sysinfo.dwAllocationGranularity > 0 then SystemInfo.allocationGranularity := sysinfo.dwAllocationGranularity;
		if sysinfo.dwPageSize > 0 then SystemInfo.pageSize := sysinfo.dwPageSize;

		if DynamicLibrary.TryOpen(ke32, kernel32) then
		begin
			DynamicLibrary.FunctionDesc.Load(SRWLockFns, ke32, '');
			if WinSRWSupported then Include(SystemInfo.features, system_SlimRWLocks);

			DynamicLibrary.FunctionDesc.Load(CVFns, ke32, '');
			if WinCVSupported then Include(SystemInfo.features, system_ConditionVariables);

			pointer(CSX.InitializeCriticalSectionEx) := ke32.FindProc('InitializeCriticalSectionEx');

			DynamicLibrary.FunctionDesc.Load(VistaTPFns, ke32, '');
			if not WinVistaTPSupported then
				DynamicLibrary.FunctionDesc.Load(XPTpFns, ke32, 'API пула потоков');
			ke32.Close;
		end;
		Initialized := yes;
	end;

	class procedure Win.Finalize;
	begin
		advapi32.lib.Close;
		comctl32.lib.Close;
		shell32.lib.Close;
		ole32.lib.Close;
	{$ifdef Debug} timeEndPeriod(TimeQuantMs); {$endif}
	end;

	class procedure Win.AlterCD;
	begin
		if not SetCurrentDirectoryW(pWideChar(ModuleFileNameW(yes))) then
			raise OperationFailed('выставить рабочий каталог');
	end;

	class function Win.DescribeError(code: dword): string;
	var
		fmflags: dword;
		werr: widestring;
		ptr: pointer;
	begin
		if code = 0 then result := 'Причина неизвестна.' else
		begin
			fmflags := FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS or FORMAT_MESSAGE_MAX_WIDTH_MASK;
			if FormatMessageW(fmflags, nil, code, 0, pWideChar(@ptr), 0, nil) > 0 then
			begin
				werr := pWideChar(ptr);
				if Assigned(ptr) then HeapFree(GetProcessHeap, 0, ptr);
				result := UTF8Encode(werr);
				while (length(result) > 0) and (result[length(result)] = ' ') do SetLength(result, length(result) - 1);
			end else
				result := '';

			if result = '' then exit('Нет текстового описания системной ошибки, код ' + USystem.ToString(code) + '.');
		end;
	{$ifdef Debug} result += ' (' + USystem.ToString(code) + ')'; {$endif}
	end;

	class function Win.Coord(x, y: sint): Windows.COORD;
	begin
		result.x := x;
		result.y := y;
	end;

	class function Win.XPTimerPeriod(period: uint): Windows.ULONG;
	begin
		// Период 0 в Create/ChangeTimerQueueTimer имеет тот же эффект, НО:
		// если такой одноразовый таймер сработал и был перезадан, больше он не сработает. А с INFINITE — сработает.
		// Если INFINITE на самом деле не поддерживается и воспринимается как обычное значение, период будет 2^32 миллисекунд = 49,7 дней.
		// (У API из Висты такой проблемы нет, а вообще на данный момент у одноразовых таймеров одноразовость явно форсируется в обработчике).
		if period > 0 then result := period else result := Windows.INFINITE;
	end;

	class function Win.TextAttribute(const color: Color): WORD;
	var
		m, intensity: float;
	begin
		result := 0;
		intensity := color.Intensity;
		if color.r > color.g then
			if color.r > color.b then m := color.r else m := color.b
		else
			if color.g > color.b then m := color.g else m := color.b;
		if (color.r > 0.2) and (m - color.r < 0.1) then result := result or FOREGROUND_RED;
		if (color.g > 0.2) and (m - color.g < 0.1) then result := result or FOREGROUND_GREEN;
		if (color.b > 0.2) and (m - color.b < 0.1) then result := result or FOREGROUND_BLUE;
		if intensity > 0.51 then result := result or FOREGROUND_INTENSITY;
	end;

	procedure QueryModuleFileName(buf: pWideChar; nBuf: size_t; out len: size_t);
	var
		r: dword;
	begin
		r := GetModuleFileNameW(0, buf, nBuf);
		if (r = 0) and (nBuf > 0) then raise Win.FunctionFailed('GetModuleFileName');
		if r >= nBuf then len := Win.QUERY_STRING_LENGTH_UNKNOWN else len := r;
	end;

	class function Win.ModuleFileNameW(path: boolean): widestring;
	var
		len: SizeInt;
	begin
		result := QueryString(@QueryModuleFileName, 'имени исполняемого файла');

		if path then
		begin
			len := length(result);
			while (len > 0) and (result[len] <> '\') do dec(len);
			SetLength(result, len);
		end;
	end;

	class function Win.TimeoutOrInfinite(timeoutMs: uint): dword;
	begin
		if 0 = not timeoutMs then result := Windows.INFINITE else result := timeoutMs;
	end;

	class function Win.ToDateTime(const dt: Windows.SYSTEMTIME): DateTime;
	begin
		result := DateTime.YMDHMSMS(dt.Year, dt.Month - 1, dt.Day - 1, dt.Hour, dt.Minute, dt.Second, dt.Millisecond);
	end;

	class function Win.ToFileAttributes(attrs: dword): FileAttributes;
	begin
		if (attrs = dword(-1)) then result := [] else
		if attrs and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY then result := [file_Folder] else
			result := [file_JustFile];
	end;

	class function Win.Error(const msgBase: string; code: dword = 0): Exception;
	begin
		if code = 0 then code := GetLastError;
		result := USystem.Error(Continued(msgBase, ' ', DescribeError(code)));
	end;

   class function Win.FunctionFailed(const fn: string; code: dword = 0): Exception;
	begin
		if code = 0 then code := GetLastError;
		result := Error('Ошибка ' + fn + '.', code);
	end;

	class function Win.OperationFailedMessage(const what: string; code: dword = 0): string;
	begin
		if code = 0 then code := GetLastError;
		result := 'Не удалось ' + what + '. ' + DescribeError(code);
	end;
	class function Win.OperationFailed(const what: string; code: dword = 0): Exception; begin result := USystem.Error(OperationFailedMessage(what, code)); end;

	class function Win.FileLoadError(const fn: string; code: dword): Exception;
	begin
		result := USystem.Error(ToSystemFileName(fn) + ': ' + LowerCaseFirst(DescribeError(code)));
	end;

	class function Win.LowerCase(const s: string): string;
	var
		ws: widestring;
	begin
		ws := UTF8Decode(s);
		CharLowerW(pWideChar(ws));
		result := UTF8Encode(ws);
	end;

	class function Win.LowerCaseFirst(const s: string): string;
	var
		p: sint;
	begin
		p := 1;
		if UTF8.Next(s, p) <> UTFInvalid then result := LowerCase(Copy(s, 1, p - 1)) + Copy(s, p, length(s) - p + 1) else result := s;
	end;

	class function Win.ToWideFileName(const fn: string): widestring;   begin result := UTF8Decode(ToSystemFileName(fn)); end;
	class function Win.FromWideFileName(const fn: widestring): string; begin result := FromSystemFileName(UTF8Encode(fn)); end;

	class function Win.QueryString(cb: QueryStringCallback; param: pointer; const ofWhat: string): widestring;
	const
		ReasonableLimit = 65535;
	var
		len, report: size_t;
	begin
		len := 64;
		repeat
			SetLength(result, len);
			cb(pWideChar(result), len + ord(len > 0), report, param);
			if report <= len then exit(Copy(result, 1, report));
			if report = QUERY_STRING_LENGTH_UNKNOWN then
			begin
				if len = ReasonableLimit then raise USystem.Error('Неправдоподобная длина ' + ofWhat + '.');
				len := 2 * len; if len > ReasonableLimit then len := ReasonableLimit;
			end else
			begin
				if (report = len + 1) or (report - 1 > ReasonableLimit) then
					raise USystem.Error('Получена неправдоподобная длина ' + ofWhat + ' (' + USystem.ToString(len) + ').');
				len := report - 1;
			end;
		until no;
	end;

	procedure UnparaQueryString(buf: pWideChar; nBuf: size_t; out len: size_t; param: pointer);
	begin
		Win.UnparaQueryStringCallback(param)(buf, nBuf, len);
	end;

	class function Win.QueryString(cb: UnparaQueryStringCallback; const ofWhat: string): widestring;
	begin
		result := QueryString(@UnparaQueryString, cb, ofWhat);
	end;

	class function Win.EnsureLib(var lib: DynamicLibrary; const filename: string; throw: boolean): boolean;
	var
		temp: DynamicLibrary;
	begin
		if lib.OK then exit(yes);
		if Initialized then SingletonLock.Enter;
		try
			if lib.OK then begin SingletonLock.Leave; exit(yes); end;
			DynamicLibrary.Open(temp, filename, throw);
			result := temp.OK;
			lib := temp;
		finally
			if Initialized then SingletonLock.Leave;
		end;
	end;

	class function Win.EnsureTaskDialogIndirect: boolean;
	const
		Funcs: array[0 .. 0] of DynamicLibrary.FunctionDesc =
		(
			(fn: @comctl32.TaskDialogIndirect; name: 'TaskDialogIndirect')
		);
	begin
		if comctl32.TaskDialogIndirectTried then exit(Assigned(comctl32.TaskDialogIndirect));
		result := EnsureLib(comctl32.lib, 'Comctl32.dll', no) and DynamicLibrary.FunctionDesc.Load(Funcs, comctl32.lib, '');
		comctl32.TaskDialogIndirectTried := yes;
	end;

	class procedure Win.EnsureCoInit;
	const
		Funcs: array[0 .. 1] of DynamicLibrary.FunctionDesc =
		(
			(fn: @ole32.CoInitializeEx; name: 'CoInitializeEx'),
			(fn: @ole32.CoUninitialize; name: 'CoUninitialize')
		);
	begin
		if Assigned(ole32.CoInitializeEx) then exit;
		EnsureLib(ole32.lib, 'Ole32.dll', yes);
		DynamicLibrary.FunctionDesc.Load(Funcs, ole32.lib, 'функции COM');
	end;

	class procedure Win.EnsureShellExecuteExW;
	const
		Funcs: array[0 .. 0] of DynamicLibrary.FunctionDesc =
		(
			(fn: @shell32.ShellExecuteExW; name: 'ShellExecuteExW')
		);
	begin
		if Assigned(shell32.ShellExecuteExW) then exit;
		EnsureLib(shell32.lib, 'Shell32.dll', yes);
		DynamicLibrary.FunctionDesc.Load(Funcs, shell32.lib, 'ShellExecute API');
	end;

	class procedure Win.EnsureCryptoAPI;
	const
		Funcs: array[0 .. 2] of DynamicLibrary.FunctionDesc =
		(
			(fn: @advapi32.CryptAcquireContextW; name: 'CryptAcquireContextW'),
			(fn: @advapi32.CryptReleaseContext;  name: 'CryptReleaseContext'),
			(fn: @advapi32.CryptGenRandom;       name: 'CryptGenRandom')
		);
	begin
		if Assigned(advapi32.CryptAcquireContextW) then exit;
		EnsureLib(advapi32.lib, 'Advapi32.dll', yes);
		DynamicLibrary.FunctionDesc.Load(Funcs, advapi32.lib, 'CryptoAPI');
	end;

	procedure Win.EventOnCV.Init(flags: ThreadEvent.InitFlags);
	begin
		autoReset := ThreadEvent.InitFlag.AutoReset in flags;
		state := InitiallySet in flags;
		lock.Init;
		Win.CV.InitializeConditionVariable(cv);
	end;

	procedure Win.EventOnCV.Done;
	begin
		lock.Done;
	end;

	procedure Win.EventOnCV.SetEvent;
	begin
		lock.Enter;
		if not state then
		begin
			state := yes;
			lock.Leave;
			if autoReset then
				Win.CV.WakeConditionVariable(cv)
			else
				Win.CV.WakeAllConditionVariable(cv);
		end else
			lock.Leave;
	end;

	procedure Win.EventOnCV.ResetEvent;
	begin
		lock.Enter;
		state := no;
		lock.Leave;
	end;

	function Win.EventOnCV.Wait(timeoutMs: uint): boolean;
	const
		Allowance = 20;
	var
		startTime, t: Ticks;
		passed: uint;
		winTimeout: dword;
	{$ifdef NotifySpuriousWakeups} spurious: boolean; {$endif}
	begin
		{$ifdef NotifySpuriousWakeups} spurious := no; {$endif}
		if 0 <> not timeoutMs then startTime := Ticks.Get;
		lock.Enter;
		while not state do
		begin
			winTimeout := Win.TimeoutOrInfinite(timeoutMs);
			if WinSRWSupported then
				Win.CV.SleepConditionVariableSRW(cv, SRWLOCK(lock.ptr), winTimeout, 0)
			else
				Win.CV.SleepConditionVariableCS(cv, pRecursiveThreadLock(lock.ptr)^.cs, winTimeout);
		{$ifdef NotifySpuriousWakeups}
			if spurious then stat.Increment(spurious_wakeups) else spurious := yes;
			stat.Increment(total_wakeups);
		{$endif}

			if 0 <> not timeoutMs then
			begin
				t := Ticks.Get;
				passed := (t - startTime).ToIMilliseconds;
				if passed + Allowance >= timeoutMs div 2 then break;
				timeoutMs -= passed;
				startTime := t;
			end;
		end;

		result := state;
		if result and autoReset then state := no;
		lock.Leave;
	end;

	function Win.EventOnCV.GetState: boolean;
	begin
		lock.Enter; // ?
		result := state;
		lock.Leave;
	end;

	procedure Win.CVOnEvent.Init;
	begin
		ev[SIGNAL] := CreateEventW(nil, no, no, nil);
		ev[BROADCAST] := CreateEventW(nil, yes, no, nil);
		nWaiters := 0;
		lock.Init;
	end;

	procedure Win.CVOnEvent.Done;
	begin
		lock.Done;
		CloseHandle(ev[SIGNAL]);
		CloseHandle(ev[BROADCAST]);
	end;

	function Win.CVOnEvent.Wait(const ext_lock: ThreadLockReference; timeoutMs: uint): boolean;
	var
		lastWaiter: boolean;
		r: dword;
	begin
		lock.Enter;
		inc(nWaiters);
		lock.Leave;

		ext_lock.Leave;
		r := WaitForMultipleObjects(2, pWOHandleArray(@ev), no, Win.TimeoutOrInfinite(timeoutMs));
		lastWaiter := no;
		result := yes;

		lock.Enter;
		dec(nWaiters);
		case r of
			WAIT_OBJECT_0 + SIGNAL: ;
			WAIT_OBJECT_0 + BROADCAST: lastWaiter := nWaiters = 0;
			else
				if (r = WAIT_TIMEOUT) and (0 <> not timeoutMs) then
					result := no
				else
					raise USystem.Error('Ошибка CVOnEvent.Wait.WFMO (r = ' + USystem.ToString(r) + ').');
		end;
		lock.Leave;

		if lastWaiter then
			ResetEvent(ev[BROADCAST]);
		ext_lock.Enter;
	end;

	procedure Win.CVOnEvent.WakeOne;
	var
		hasWaiters: boolean;
	begin
		lock.EnterShared;
		hasWaiters := nWaiters > 0;
		lock.LeaveShared;

		if hasWaiters then
			SetEvent(ev[SIGNAL]);
	end;

	procedure Win.CVOnEvent.WakeAll;
	var
		hasWaiters: boolean;
	begin
		lock.EnterShared;
		hasWaiters := nWaiters > 0;
		lock.LeaveShared;

		if hasWaiters then
			SetEvent(ev[BROADCAST]);
	end;
{$endif windows}

	function HandleSignal(v: longint): longint; cdecl;
	var
		human: string;
	begin
		case v of
			SIGFPE:  human := 'SIGFPE';
			SIGABRT: human := 'SIGABRT';
			SIGILL:  human := 'SIGILL';
			SIGSEGV: human := 'AV (SIGSEGV)';
			SIGTERM: human := 'SIGTERM';
			SIGINT:  human := 'SIGINT';
			else     human := 'сигнал (' + ToString(v) + ')';
		end;

		Fatal('Словил ' + human + '.', SoftwareOrDriverError);
		result := 1;
	end;

	procedure SetupSignals;
	begin
		signal(SIGFPE,  @HandleSignal);
		signal(SIGABRT, @HandleSignal);
		signal(SIGILL,  @HandleSignal);
		signal(SIGSEGV, @HandleSignal);
		signal(SIGTERM, @HandleSignal);
		signal(SIGINT,  @HandleSignal);

		// 15-13 reserved
		// 12    IC Infinity control. (That is for 8087 and 80287 only).
		// 11-10 RC Rounding control. 00 - to nearest, 01 - down (toward -inf), 10 - up (toward +inf), 11 - toward zero.
		// 9-8   PC Precision control. 11 - extended precision, 10 - double precision, 01 - reserved, 00 - single precision.
		// 7-6   reserved
		// 5     PM Precision (inexact result) mask
		// 4     UM Underflow mask
		// 3     OM Overflow mask
		// 2     ZM Zero-divide mask
		// 1     DM Denormalized operand mask
		// 0     IM Invalid operation mask
		System.Set8087CW(System.Get8087CW or %111111);

		// 15    FZ Flush To Zero
		// 14-13 10 R+ Round Positive, 01 R- Round Negative, 11 RZ Round To Zero, 00 RN Round To Nearest
		// 12    PM Precision Mask
		// 11    UM Underflow Mask
		// 10    OM Overflow Mask
		// 9     ZM Divide By Zero Mask
		// 8     DM Denormal Mask
		// 7     IM Invalid Operation Mask
		// 6     DAZ Denormals Are Zero
		// 5     PE Precision Flag
		// 4     UE Underflow Flag
		// 3     OE Overflow Flag
		// 2     ZE Divide By Zero Flag
		// 1     DE Denormal Flag
		// 0     IE Invalid Operation Flag
		System.SetMXCSR(GetMXCSR or %1111110000000);
	end;

type
	CrucialSelfTests = object
	type
		ReplaceVMT = object
		type
			DestructorCalled = (ParentDestructorCalled, ChildDestructorCalled);
			DestructorsCalled = set of DestructorCalled;
			pDestructorsCalled = ^DestructorsCalled;

			pParent = ^Parent;
			Parent = object(BaseObject)
				dtrs: pDestructorsCalled;
				constructor Init(dtrs: pDestructorsCalled);
				destructor Done; virtual;
			end;

			pChild = ^Child;
			Child = object(Parent)
				constructor Init(dtrs: pDestructorsCalled);
				destructor Done; virtual;
			end;

			function ToString(dc: DestructorsCalled): string; static; overload;
			procedure Test(var warn: string); static;
		end;

		procedure TestIndexStringAsPointer(var warn: string); static;
		procedure TestAnsiRecHack(var warn: string); static;
		procedure TestVMTHack(var warn: string); static;
		procedure Run; static;
	private
		procedure Expect(value, expected: PtrInt; const what: string; var warn: string); static;
	end;

	constructor CrucialSelfTests.ReplaceVMT.Parent.Init(dtrs: pDestructorsCalled);
	begin
		inherited Init;
		self.dtrs := dtrs;
	end;

	destructor CrucialSelfTests.ReplaceVMT.Parent.Done;
	begin
		if Assigned(dtrs) then dtrs^ += [ParentDestructorCalled];
		inherited Done;
	end;

	constructor CrucialSelfTests.ReplaceVMT.Child.Init(dtrs: pDestructorsCalled);
	begin
		inherited Init(dtrs);
	end;

	destructor CrucialSelfTests.ReplaceVMT.Child.Done;
	begin
		if Assigned(dtrs) then dtrs^ += [ChildDestructorCalled];
		inherited Done;
	end;

	function CrucialSelfTests.ReplaceVMT.ToString(dc: DestructorsCalled): string;
	begin
		result := '';
		if ParentDestructorCalled in dc then result += 'P' else result += '-';
		if ChildDestructorCalled in dc then result += 'C' else result += '-';
	end;

	procedure CrucialSelfTests.ReplaceVMT.Test(var warn: string);
		procedure Expect(const got, exp: DestructorsCalled);
		begin
			if exp <> got then warn += EOL + 'Ожидались деструкторы ' + ToString(exp) + ', на деле вызваны ' + ToString(got);
		end;
	var
		c: ^BaseObject;
		dp: DestructorsCalled;
		_c: Child;
	begin
		// чтобы WPO не выбросила методы
		c := @_c;
		pBaseObject(c)^.Init; c^.Done;
		pParent(c)^.Init(nil); c^.Done;

		dp := []; pChild(c)^.Init(@dp); c^.Done;
		Expect(dp, [ParentDestructorCalled, ChildDestructorCalled]);

		dp := []; pChild(c)^.Init(@dp); c^.ReplaceVMT(TypeOf(Parent)); c^.Done;
		Expect(dp, [ParentDestructorCalled]);

		dp := []; pChild(c)^.Init(@dp); c^.ReplaceVMT(TypeOf(BaseObject)); c^.Done;
		Expect(dp, []);
	end;

	procedure CrucialSelfTests.TestIndexStringAsPointer(var warn: string);
	var
		strings: array[0 .. 4] of string;
	begin
		strings[0] := chr(Thread.Current and ord(High(char)));
		strings[1] := strings[0] + strings[0];
		strings[2] := strings[1] + strings[1];
		strings[3] := strings[0] + strings[0];
		strings[4] := strings[2];

		Expect(Index(pointer(strings[4]), pString(strings), length(strings)), 2, 'Index(p(4) = p(2))', warn);
		Expect(Index(pointer(strings[3]), pString(strings), length(strings)), 3, 'Index(3 = 1 <> p(1))', warn);
		Expect(Index(pointer(strings[0] + strings[0]), pString(strings), length(strings)), -1, 'Index(X = 2 <> p(2))', warn);
	end;

	procedure CrucialSelfTests.TestAnsiRecHack(var warn: string);
	var
		s, link: string;
		ar: Hacks.PAnsiRec;
	begin
		SetLength(s, 12);
		(@link)^ := s;
		ar := Hacks.PAnsiRec(s) - 1;
		Expect(ar^.cpes.CodePage,    CP_ACP, 'CodePage',    warn);
		Expect(ar^.cpes.ElementSize, 1,      'ElementSize', warn);
		Expect(ar^.ref,              2,      'RefCount',    warn);
		Expect(ar^.len,              12,     'Length',      warn);
	end;

	procedure CrucialSelfTests.TestVMTHack(var warn: string);
	var
		v: Hacks.pVMT;
		expectedPtr: pointer;
	begin
		v := TypeOf(ReplaceVMT.Child);
		Expect(v^.size, sizeof(ReplaceVMT.Child), 'size', warn);
		Expect(v^.msize, -sizeof(ReplaceVMT.Child), 'msize', warn);

		expectedPtr := TypeOf(ReplaceVMT.Parent);
		if v^.parent <> expectedPtr then warn += EOL + 'Parent = ' + HexStr(v^.parent) + ', ожидается ' + HexStr(expectedPtr);
	end;

	procedure CrucialSelfTests.Run;
	var
		warn: string;
		procedure Start; begin warn := ''; end;
		procedure Finish(const msg: string); begin if warn <> '' then Warning.Title('Тест провален').Show('Внимание, ' + msg + warn); end;
	begin
		Start; ReplaceVMT.Test(warn);          Finish('ReplaceVMT неадекватна.');
		Start; TestIndexStringAsPointer(warn); Finish('Index(PChar) не работает как задумано. Пул строк сломается.');
		Start; TestAnsiRecHack(warn);          Finish('Hacks.AnsiRec не соответствует структуре строки.');
		Start; TestVMTHack(warn);              Finish('Hacks.VMT не соответствует структуре VMT.');
	end;

	procedure CrucialSelfTests.Expect(value, expected: PtrInt; const what: string; var warn: string);
	begin
		if value <> expected then warn += EOL + 'От ' + what + ' ожидалось ' + ToString(expected) + ', получено ' + ToString(value) + '.';
	end;

type
	Platform = {$ifdef Windows} Win {$endif};

{$ifdef Debug}
	procedure CreateStderr(var f: text);
		function TrySystemOpen(var f: text; const fn: string): boolean;
		begin
			assign(f, fn); rewrite(f);
			result := IOResult = 0;
		end;
	var
		fn, t: string;
		extStart: sint;
	begin
		t := Folder.Filename(ExecFileName);
		extStart := Index1Rev(ExtensionSeparator, pointer(t), length(t) * sizeof(char));
		if extStart = 0 then extStart := length(t) + 1;

		fn := Paths.Logs + Copy(t, 1, extStart - 1) + '-' + DateTime.Start.ToCode + '.stderr';
		if not (Folder.Create(Folder.Path(fn)) and TrySystemOpen(f, fn)) then
		begin
			if Warning.Text('Не удалось создать ' + ToSystemFileName(fn) + '.')
				.Variant('Использовать stderr в рабочей папке (' + Folder.RemoveSeparator(Folder.Working) + ')')
				.Variant('Не вести stderr').Show = TaskV1
			then
				fn := 'stderr'
			else
				fn := Platform.NullDevice;
			TrySystemOpen(f, fn);
		end;

		write(f, UTF8.BOM);
		writeln(f, 'Started.'); Flush(f);
	end;

var
	main_stderr: ^text = nil;

	procedure ToStderr(const s: string; linebreak: boolean = yes);
	begin
		SingletonLock.Enter;
		try
			if linebreak then writeln(main_stderr^, s) else write(main_stderr^, s);
		finally
			SingletonLock.Leave;
		end;
	end;
{$endif}

{$ifdef assert}
	procedure HandleFailedAssert(const message: shortstring; const fname: shortstring; lineno: longint; erroraddr: pointer);
	var
		msg: string;
	begin
		unused_args erroraddr end_list
		msg := 'Ассерт не сработал (' + fname + ':' + ToString(lineno) + ').';
		if message <> '' then msg += EOL + message;
		Fatal(msg, 2 {ассерт + этот обработчик});
	end;
{$endif}

	procedure ShowUnhandledException(Obj: TObject; Addr: CodePointer; FrameCount: LongInt; Frame: PCodePointer);
	{$ifdef Debug}
		function BuildTrace: string;
		var
			i: sint;
		begin
			result := '';
			for i := 0 to FrameCount - 1 do
			begin
				if i > 0 then result += EOL;
				result += AddressLineInfo(Frame[i]);
			end;
		end;
	{$endif}
	begin
		unused_args addr _ FrameCount _ Frame end_list
		Error.Title('Критическая ошибка').Text(Exception.Message(Obj)){$ifdef Debug}.Expanded(BuildTrace){$endif}.Show;
		Fatal;
	end;

	function Quote(const arg: string): string;
		var
			slashes: uint;
			i: sint;
			quote: boolean;
		begin
			result := '';
			quote := no;
			i := 1;
			repeat
				slashes := 0;
				while (i <= length(arg)) and (arg[i] = '\') do begin inc(i); inc(slashes); end;
				if (i > length(arg)) or (arg[i] = '"') then slashes := 2*slashes + ord((i <= length(arg)) and (arg[i] = '"'));
				while slashes > 0 do begin result += '\'; dec(slashes); end;
				if i > length(arg) then break;
				result += arg[i];

				quote := quote or (arg[i] in [' ', TabSym, #$B {\v, vertical tab}, EOL, '"']);
				inc(i);
			until no;
			if quote then result := '"' + result + '"';
		end;

initialization
{$ifdef assert} AssertErrorProc := @HandleFailedAssert; {$endif}
	ExceptProc := @ShowUnhandledException;

	// Поведение DefaultSystemCodePage = CP_ACP не определено, но на практике это отключит самодеятельность RTL.
	// Так, строковым литералам прописывается кодировка CP_ACP, когда в исходнике не задана ни BOM, ни {$codepage}.
	// DefaultSystemCodePage, в свою очередь, прописывается динамически создаваемым строкам.
	// Сама CP_ACP численно равна нулю и имеет эффект, де-факто аналогичный CP_NONE (я использую CP_ACP только из-за литералов).
	DefaultSystemCodePage := CP_ACP;

	// Все исключения попадут в ShowUnhandledException. Наверное.
	Ticks.Initialize;
	if not System.IsMultiThread
		and (Warning.Title('Самоконтроль').ContinueOrStopVariants.Show('IsMultiThread = false — потоки могут работать нестабильно.') <> TaskV1)
		then Fatal;
	SetupSignals;
	Platform.Initialize;
{$ifdef alter_cd} Platform.AlterCD; {$endif}
	SingletonLock.Init;

{$ifdef Debug}
	main_stderr := @system.stderr;
	CreateStderr(main_stderr^);
{$endif}

{$ifdef Debug} stat.Init; {$endif}
	if IsConsole <> {$ifdef use_console} yes {$else} no {$endif} then
		Warning.Show('Дефайн use_console выставлен неверно (' + chr(ord('+') * uint(IsConsole) + ord('-') * uint(not IsConsole)) + ').');
{$ifdef use_console} Con.Initialize; {$endif}
	asio.Init;
	Work.Init;
	CrucialSelfTests.Run;
   units.Init;

finalization
{$ifdef Debug}
	if stat.cam[alive_objects].current > 0 then
		ToStderr('Leaked objects: ' + ToString(stat.cam[alive_objects].current) + '!');
{$endif}

	units.Done;
	Work.Done;
	asio.Done;
	Crypt.Finalize;
{$ifdef use_console} Con.Finalize; {$endif}
{$ifdef Debug} stat.Done; {$endif}
	SingletonLock.Done;
	try
		Platform.Finalize;
	{$ifdef Debug} writeln(main_stderr^, 'Terminated normally.'); Flush(main_stderr^); {$endif}
	except
	{$ifdef Debug} Exception.Fatal; {$endif}
	end;
end.
