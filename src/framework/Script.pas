unit Script;

// Изменения в исходниках:
// — lua_Integer'ы — 32-битные на 32-битных платформах.
// — lua_Reader, lua_Writer, lua_Hook, lua_CFunction, lua_KFunction, lua_Alloc следуют соглашению о вызове для остального API.

{$include opts.inc}
{$define pss_by_ls:=ppScriptState(pPointer(ls) - 1) {$undef ls}}
{$define ss_by_ls:=(pss_by_ls^)}
{$ifdef Debug}
	{-$define DebugFibers}
{$endif}
{$define GuardLoading}
{$define DetectSerializableArrays}
{$define DetectSerializableRecords}

{$ifndef GuardLoading} {$warning GuardLoading undefined, please make sure that's intended} {$endif}

{$include all_vectors.inc}
{$define start_vec_ids :=
	{$ifdef end_vec_ids} {$error end_vec_ids missing} {$endif}

	{$if veclen = 2}     {$define isvec := IsVec2} {$define asvec := ToVec2} {$define pushvec := PushVec2} {$define getvecfield := GetVec2Field}
	                     {$define bindingctr := Script_Vec2} {$define bindinglen := Script_Vec2_length} {$define bindinggetnorm := Script_Vec2_normalized}
	                     {$define bindingop := Script_Vec2_Op} {$define bindingr := Script_Vec2_R} {$define bindingw := Script_Vec2_W}
	                     {$define bindingtypeofconst := vec2_typeof_const} {$define bindingtypeof := vec2_typeof}
	{$elseif veclen = 3} {$define isvec := IsVec3} {$define asvec := ToVec3} {$define pushvec := PushVec3} {$define getvecfield := GetVec3Field}
	                     {$define bindingctr := Script_Vec3} {$define bindinglen := Script_Vec3_length} {$define bindinggetnorm := Script_Vec3_normalized}
	                     {$define bindingop := Script_Vec3_Op} {$define bindingr := Script_Vec3_R} {$define bindingw := Script_Vec3_W}
	                     {$define bindingtypeofconst := vec3_typeof_const} {$define bindingtypeof := vec3_typeof}
	{$elseif veclen = 4} {$define isvec := IsVec4} {$define asvec := ToVec4} {$define pushvec := PushVec4} {$define getvecfield := GetVec4Field}
	                     {$define bindingctr := Script_Vec4} {$define bindinglen := Script_Vec4_length} {$define bindinggetnorm := Script_Vec4_normalized}
	                     {$define bindingop := Script_Vec4_Op} {$define bindingr := Script_Vec4_R} {$define bindingw := Script_Vec4_W}
	                     {$define bindingtypeofconst := vec4_typeof_const} {$define bindingtypeof := vec4_typeof}
	{$else} {$error wrong veclen} {$endif}
	{$define end_vec_ids := {$undef isvec} {$undef asvec} {$undef pushvec} {$undef getvecfield}
	                        {$undef bindingctr} {$undef bindinglen} {$undef bindinggetnorm} {$undef bindingop} {$undef bindingr} {$undef bindingw}
	                        {$undef bindingtypeofconst} {$undef bindingtypeof}
	                        {$undef end_vec_ids}}}

interface

uses
	ctypes, USystem, UClasses, UMath, Random, Streams, Utils, Algo, LuaLib
{$ifdef use_serialization}, Serialization {$endif}
{$ifdef Debug}, ULog, Errors, Debug {$endif};

type
	ScriptType =
	(
		script_Nil, script_Boolean, script_Number,
		script_String, script_Pointer, script_Table, script_Function
	);

const
	script_Object = script_Pointer;
	UPVALUE = lua.REGISTRYINDEX;

type
	ScriptOperatorEnum = (script_Add, script_Sub, script_Mul, script_Div, script_Pow, script_Unm);
	ExecuteResult = (exec_Ok, exec_Incomplete, exec_CompilationError, exec_RunError);

	pStackTrace = ^StackTrace;
	StackTrace = object
	type
		HumanMode = (OneLine, MultiLine, MultiLineWithoutMessage);
	var
		message, title: string;
		items: array of record
			item: string;
			reps: sint;
		end;
		thread: lua.State;
		procedure Init(const newMessage: string; newThread: lua.State);
		procedure Done; {$define pSelf := pStackTrace} {$define free_only} {$include dyn_obj.h.inc}
		procedure Add(const item: string);
		procedure Add(const item: string; reps: sint);
		function Human(mode: HumanMode): string;
		function DestructiveHuman(mode: HumanMode): string;
		procedure Append(const trace: StackTrace);
		procedure DestructiveAppendTo(var trace: StackTrace);
	const
		WindowTitle = '/title=';
	end;

	pScriptPath = ^ScriptPath;
	ppScriptState = ^pScriptState;
	pScriptState = ^ScriptState;
	ScriptGC = object
		procedure Collect;
		function Allocated: size_t;
		procedure SetPause(pause: size_t);
		procedure SetStepMul(sm: size_t);
	private
		ls: lua.State;
	end;

	ScriptState = object(&Object)
	public type
		FunctionBody = function(var ss: ScriptState): sint;
		R0FunctionBody = procedure(var ss: ScriptState);
		R1FunctionBody = procedure(var ss: ScriptState);
		OperatorBody = procedure(var ss: ScriptState; op: ScriptOperatorEnum);
		ROPropertyBody = procedure(var ss: ScriptState);
		RWPropertyBody = procedure(var ss: ScriptState; read: boolean);
		WOPropertyBody = procedure(var ss: ScriptState);
		UnparaErrorMessageFunc = function(var ss: ScriptState): string;
		UnstateErrorMessageFunc = function(param: pointer): string;
		ErrorMessageFunc = function(var ss: ScriptState; param: pointer): string;

		StuffDesc = record
			s: string;
			p: pointer;
		end;

		pNativeFiber = ^NativeFiber;
		NativeFiber = object
		type
			MainProc = procedure(var fiber: NativeFiber; var ss: ScriptState; param: pointer);
			SingleYieldOutcome = (Discarded);
			YieldOutcome = set of SingleYieldOutcome;
		private var
			thisFiber: pFiber;
			body: MainProc;
			ss: pScriptState;
			param: pointer;
			finalizing: boolean;
			yieldResults: sint;
		public
			procedure Init(const newNameOf: string; newSS: pScriptState; newBody: MainProc; newParam: pointer);
			procedure Done;
			function Resume(out finished: boolean): sint;
			function Yield(nResults: sint): YieldOutcome;
		{$ifdef Debug} function HumanOf: string; {$endif}
		end;

		pEnvIndex = ^EnvIndex;
		EnvIndex = object
		{$ifdef Debug} magic: uint; {$endif}
			index: R1FunctionBody;
			newIndex: R0FunctionBody;
			param: pointer;
			function Make(index: R1FunctionBody; newIndex: R0FunctionBody; newParam: pointer): EnvIndex; static;
		const
		{$ifdef Debug} CorrectMagic = $E5715DE7; {$endif}
			ParamUpvalue = UPVALUE-2;
		end;
	private
		ls: lua.State;
		_assertions: boolean;
	public
		constructor Init;
		destructor Done; virtual;
		function DoFile(const fileName: string; pushEnv: boolean; nArg, nRet: sint; userEnv: sint = 0; userIndex: pEnvIndex = nil): sint;
		function LoadModule(const stream: string; nArg, nRet: sint; keepFor: uint = 0): sint;
		function Execute(const code, chunkname: string; env: sint; errmsg: pString): ExecuteResult;
		procedure Throw(const msg: string); noreturn;
	{$define func := procedure Throw(const fmt: string; const _ARGS_: string); noreturn;} {$include variadic.inc}
		procedure UnknownIdentifier(const id: string); noreturn;
		procedure WrongArgc(const fn: string); noreturn;
	{$ifdef Debug}
		function DumpDelegates(obj: pObject): string;
	{$endif}
		function StackTrace(const message: string): StackTrace;
		procedure SetupGuard(const name: string);
		procedure ResetGuard;
		procedure RemoveGuard;
		procedure Bookkeep; // TODO: КАК-НИБУДЬ сделать нормальную многопоточную работу со скриптом?..

		procedure AddStuff(const stuff: array of StuffDesc);

		procedure SetGlobal(const name: string);
		procedure SetGlobal(const name: string; o: pObject);
		procedure SetGlobal(const name: string; const s: string);
		procedure SetGlobal(const name: string; x: lua.Number);
		procedure GetGlobal(const name: string);

		procedure SetDelegate(obj: pObject; md: pMultiDelegate; func: pointer; const newName: string);
		function GetDelegate(md: pMultiDelegate; const name: string): boolean;

		procedure Associate(obj: pObject; id: sint = 0);
		function GetAssociated(obj: pObject; id: sint = 0): boolean;
		function HasUV(obj: pObject): boolean;
		function KillUV(obj: pObject): boolean;
		procedure RestoreUV(obj: pObject);

		procedure DontSerialize(obj: pObject);
		function Serializable(idx: sint): boolean;

		function Call(nArg, nRet: sint): sint;
		function Call(nArg: sint): sint;

		function Top: sint; cinline
		procedure SetTop(nt: sint); cinline
		function AbsIdx(idx: sint): sint; cinline
		function AdjustIdx(idx: sint; pushed: sint): sint; cinline
		function Typ(idx: sint): ScriptType; cinline
		function InternalValueTypeName(idx: sint): string; cinline
		function IsNil(idx: sint): boolean; cinline
		function NonNil(idx: sint): boolean; cinline
		function IsNumber(idx: sint): boolean; cinline
		function IsString(idx: sint): boolean; cinline
		function IsTable(idx: sint): boolean; cinline
		function IsFunction(idx: sint): boolean; cinline
		procedure PushNil; cinline
		procedure PushNil(n: uint); cinline
		procedure PushFloat(const x: lua.Number); cinline
		procedure PushSint(x: sint); cinline
		procedure PushBool(b: boolean); cinline
		procedure PushString(const s: string); cinline
		procedure PushString(const s: StringView); cinline
		procedure PushStringOrNil(const s: string); cinline
		procedure PushPChar(s: PChar); cinline
		procedure PushObject(obj: pObject);
		procedure PushObject(obj: pObject; isref: boolean);
		function PushExisting(obj: pObject): boolean; cinline
		procedure PushTable; cinline
		procedure PushTable(narr, nrec: sint); cinline
		procedure PushCopy(idx: sint); cinline
		procedure Pop; cinline
		procedure Pop(n: uint); cinline
		procedure Insert(at: sint); cinline
		procedure Insert(at, n: sint); cinline
		procedure Remove(idx: sint); cinline
		procedure Remove(idx, n: sint); cinline
		procedure Replace(idx: sint); cinline
		procedure SetCopy(fromidx, toidx: sint); cinline
		procedure PushFiber(fnidx: sint);
		function IsFiber(idx: sint): boolean;
		procedure PushFiber(const nameOf: string; body: NativeFiber.MainProc; param: pointer);
		procedure PushPtr(p: pointer);

	{$define vecf := start_vec_ids procedure PushVec(const v: vec); cinline end_vec_ids} all_float_vectors
		procedure PushQuaternion(const q: Quaternion); cinline
		procedure PushTransform(const tf: Transform); cinline

		function ToFloat(idx: sint): lua.Number; cinline
		function ToFloat(idx: sint; const def: lua.Number): lua.Number; cinline
		function ToSint(idx: sint): sint; cinline
		function ToSint(idx: sint; def: sint): sint; cinline
		function ToBool(idx: sint): boolean; cinline
		function ToTribool(idx: sint): Tribool;
		function ToPChar(idx: sint): PChar; cinline
		function ToStringView(idx: sint): StringView; cinline
		function ToString(idx: sint): string;
		function RawLen(idx: sint): sint; cinline
		function ObjType(idx: sint): pointer;
		function IsPOD(idx: sint): boolean;
		function ToObject(idx: sint; ot: pointer): pointer;
		function ToSelf: pointer; cinline
		function IsStream(idx: sint): boolean;
		function ToStream(const fn: string): string;
		function ToStream(idx: sint): string;
		function ToData(idx: sint; ot: pointer): pointer; cinline
		function ToData(idx: sint): pointer; cinline

	{$define vecf := start_vec_ids
		function isvec(idx: sint): boolean;
		function asvec(idx: sint): vec; end_vec_ids} all_float_vectors
		function VecLen(idx: sint): sint;
		function ToUintVec2(idx: sint): UintVec2;
		function IsQuaternion(idx: sint): boolean; function ToQuaternion(idx: sint): Quaternion;
		function IsTransform(idx: sint): boolean;  function ToTransform(idx: sint): Transform;

		procedure GetTable(idx: sint); cinline
		procedure GetWithKey(tidx, kidx: sint);
		procedure GetTableI(idx: sint; key: sint); cinline
		function TryGetTableI(idx: sint; key: sint): boolean;
		function GetTableS(idx: sint; const key: string): boolean;
		procedure ForceGetTableS(idx: sint; const key: string);
	{$define part:=
		function HasField(idx: sint; const key: key_type): boolean;
		function FieldType(idx: sint; const key: key_type): ScriptType;
		function GetFloatField(idx: sint; const key: key_type): lua.Number;
		function GetFloatField(idx: sint; const key: key_type; const def: lua.Number): lua.Number;
		procedure SetFloatField(idx: sint; const key: key_type; const value: lua.Number);
		function GetSintField(idx: sint; const key: key_type): sint;
		function GetSintField(idx: sint; const key: key_type; def: sint): sint;
		function GetBoolField(idx: sint; const key: key_type): boolean;
		function GetBoolField(idx: sint; const key: key_type; def: boolean): boolean;
		function GetStringField(idx: sint; const key: key_type): string;
		function GetStringField(idx: sint; const key: key_type; const def: string): string;
		function GetStreamField(idx: sint; const key: key_type): string;
	{$define vecf := start_vec_ids
		function getvecfield(idx: sint; const key: key_type): vec;
		function getvecfield(idx: sint; const key: key_type; const def: vec): vec; end_vec_ids} all_float_vectors
		function GetQuaternionField(idx: sint; const key: key_type): Quaternion;
		function GetTransformField(idx: sint; const key: key_type): Transform;
		function GetObjectField(idx: sint; const key: key_type; ot: pointer): pointer;
	{$undef key_type}}
	{$define key_type:=string} part
	{$define key_type:=sint} part
	{$undef part}
		procedure SetTable(idx: sint); cinline
		procedure SetWithKey(tidx, kidx: sint);
		procedure SetWithValue(tidx, vidx: sint);
		procedure SetKeyValue(tidx, kidx, vidx: sint);
		procedure SetTableI(idx: sint; key: sint); cinline
		procedure SetTableS(idx: sint; const key: string); cinline
		function ConsecutiveKeys(idx: sint): uint;
		function ConsecutiveSerializableKeys(idx: sint): uint;

		function Next(idx: sint): boolean; cinline
		function GetMeta(idx: sint): boolean; cinline
		procedure ExpectMeta(idx: sint);
		function EqualMetatables(a, b: sint): boolean;

		function GC: ScriptGC;
		function MemoryEaten: size_t;
		property Assertions: boolean read _assertions write _assertions;

	{$ifdef use_serialization}
		procedure Serialize(idx: sint; se: pSerializer);
		function Deserialize(de: pDeserializer): boolean;
	{$endif}
	private type
		BuiltinFunctionDesc = record
			main: lua.CFunction;
			underlying: pointer;
		end;
		{$define classname:=Name2BuiltinFunction} {$define key_type:=string} {$define inline_key:=StringView} {$define value_type:=BuiltinFunctionDesc}
		{$include hash.h.inc}

		Guard = record
			name: string;
			setupTime, timeout: Ticks;
		end;

		pTimedLink = ^TimedLink;
		TimedLink = record
			timer: ThreadTimer;
			ss: pScriptState;
			id, expiredId: sint;
		{$ifdef Debug} renews: sint; name: string; {$endif}
		end;

		pInternalPropertyDesc = ^InternalPropertyDesc;
		InternalPropertyDesc = record
			both: boolean;
		case uint of
			0: (ro: ROPropertyBody);
			1: (wo: WOPropertyBody);
			1: (rw: RWPropertyBody);
			2: (funcPtr: pointer);
		end;

		pInternalPropertyBlock = ^InternalPropertyBlock;
		InternalPropertyBlock = record
			next: pInternalPropertyBlock;
			count: sint;
			props: array[0 .. 0] of InternalPropertyDesc;
		end;

		LazyEnum = (lazy_NativeFiberMeta, lazy_PathMeta, lazy_LocalizedMeta);
		LoadFlag = (Share, PushError, DontPushEnv, SupplyEnv, SupplyIndex);
		LoadFlags = set of LoadFlag;
	const
		MemoryPoolBlockSize = 64;
		CodeTimeout = 5.0;
		ContinuationTimeout = 10.0;
		GlobalFilenameStart = {$ifdef Debug} '~G~' {$else} #0 {$endif};
	var
		memPool: MemoryPool;
		envDepFns: Name2BuiltinFunction;
		guards: array of Guard;
		timedLinks: array of pTimedLink;
		expiredTLs: array of pTimedLink;
		expiredTLsLock: ThreadLock;
		propsMem: RegionAllocator;
		_priorityToBinaries, _showErrors: boolean;
	{$ifdef Debug} _insideCall: uint; {$endif}
		function CopyTable(srcIdx, dstIdx: sint; doreplace: boolean): sint;
		procedure _CompleteEnv(const stream: string; env, ep: sint; index: pEnvIndex);
		function LoadModule(const stream: string; nArg, nRet: sint; flags: LoadFlags; keepFor: uint = 0): sint;
		function _GenericLoad(const stream: string; nArg, nRet: sint; flags: LoadFlags; keepFor: uint): sint;
		function _GetValuesTable(obj: pObject; force: boolean): boolean;
		function _GetObjectValueI(obj: pObject; valueId: sint): boolean;
		procedure _SetObjectValueI(obj: pObject; valueId: sint);
		procedure _RemoveObjectValueI(obj: pObject; valueId: sint);
		procedure _PushData(const data; size: size_t; otype: pointer);
		function _HookRequired: boolean;
		procedure _MaybeSetHook;
		procedure _MaybeRemoveHook;
		procedure SetupTimedLink(due: uint {$ifdef Debug}; const name: string {$endif});
		procedure PushLazy(what: LazyEnum);
		function PushPath(newBase: pScriptPath; newInPlace: boolean): pScriptPath;
		function GetQuaternionFromTable(tidx, start: sint; out readed: sint): Quaternion;
		function HumanStackTrace(const message: string; mode: StackTrace.HumanMode): string;
		procedure ShowError(const trace: StackTrace; allowBox: boolean);
		procedure ShowError(const msg: string);
		function GetFinalLocalized(idx: sint): string;
{$ifdef Debug}
	private
		startStackTop: sint;
		procedure _Dump(baseTable, idx: sint; var sb: StringBuilder; indent: sint; var nextUid: sint);
		function _Dump(idx: sint): string;
		function _Dump: string;
		function DumpKeys(idx: sint): string;
		function DumpLocalStack: string;
{$endif}
	end;

	ScriptStuffDesc = ScriptState.StuffDesc;

	// button. >>onClick<< .blabla = function() end
	pScriptDelegateWrapper = ^ScriptDelegateWrapper;
	ScriptDelegateWrapper = object(&Object)
	private
		obj: pObject;
		md: pMultiDelegate;
		func: pointer;
	public
		constructor Init(newObj: pObject; newMd: pMultiDelegate; newFunc: pointer);
		destructor Done; virtual;
		procedure SetNamed(var ss: ScriptState; const newName: string);
		function GetNamed(var ss: ScriptState; const name: string): boolean;
	end;

	pScriptDelegate = ^ScriptDelegate;
	ScriptDelegate = object(&Object)
		ss: pScriptState;
		obj: pObject;
		md: pMultiDelegate;
		name: PoolString;
		uid: uint;
		constructor Init(newSS: pScriptState; newObj: pObject; newMd: pMultiDelegate; const newName: PoolString);
		destructor Done; virtual;
		function GetFunction {$ifdef Debug}(const dbgname: string){$endif} : boolean;
	end;

	ScriptPath = object
		base: pScriptPath;
		value: string;
		inPlace: boolean;
		procedure Init(newBase: pScriptPath; newInPlace: boolean);
		procedure Done;
		procedure Append(const fn: string);
		function Final: string;
	end;

const
	{$define vec2_typeof_const := @Vec2.SqrLength} {$define vec2_typeof := pointer(vec2_typeof_const)}
	{$define vec3_typeof_const := @Vec3.SqrLength} {$define vec3_typeof := pointer(vec3_typeof_const)}
	{$define vec4_typeof_const := @Vec4.SqrLength} {$define vec4_typeof := pointer(vec4_typeof_const)}
	{$define quaternion_typeof_const := @Quaternion.Normalized} {$define quaternion_typeof := pointer(quaternion_typeof_const)}
	{$define transform_typeof_const := @Transform.Inversed} {$define transform_typeof := pointer(transform_typeof_const)}
	ObjType_Vec: array[2 .. 4] of pointer = (vec2_typeof_const, vec3_typeof_const, vec4_typeof_const);
	ObjSize_Vec: array[2 .. 4] of size_t = (sizeof(Vec2), sizeof(Vec3), sizeof(Vec4));
	ObjType_Quaternion: pointer = quaternion_typeof_const;
	ObjType_Transform: pointer = transform_typeof_const;
	ObjType_Path: pointer = @ScriptPath.Init;
	ObjType_Any = nil;
	ScriptTypeNames: array[ScriptType] of string =
		('nil', 'boolean', 'number', 'string', 'pointer', 'table', 'function');
	RequireEnv = '/F';
	Writeable  = '/W';
	WriteOnly = '/w';
	PODType    = '/#';
	StartDescMagic = '/';
	FunctionsDesc = StartDescMagic + 'F';
	PrefixedFunctions = '>';
	TypeDesc = StartDescMagic + 'T';
	TuneTypeMagic = ':';
	TypeOp = TuneTypeMagic + 'O';
	TypeR = TuneTypeMagic + 'R';
	TypeW = TuneTypeMagic + 'W';
	TypeCall = TuneTypeMagic + 'C';

	function UnescapeInterpolationParam(const s: string): string;
	function Preprocess(const source: string; assertions: boolean): string;

implementation

uses
	BinaryScripts, Human {$ifdef Profile}, Profile {$endif};

	{$define classname:=ScriptState.Name2BuiltinFunction} {$define hash_func:=Hash.OfString} {$define get_key:=@_1}
	{$include hash.pp.inc}

type
	Reg = object
	const
		Start         = lua.RIDX_LAST;
		WeakBoth      = Start + 0;
		ApiMt         = Start + 1;
		Modules       = Start + 2;
		ObjUserdatas  = Start + 3;
		CFunctionPtr  = Start + 4;
		DontSerialize = Start + 5;
		TimedLinks    = Start + 6;
		Lazy          = ObjUserdatas;

		ApiMt_TypeOf  = 0;
	end;

const
	EnvMt_StreamBase = 0;
	EnvMt_Results = 1;
	EnvMt_This = '_META';
	EnvMt_Here = 'here';

	ObjValue_Assoc = 0;
	ObjValue_Delegates = 1;
	ObjValue_Assoc2 = 2;

	LuaID_Index = '__index';
	LuaID_NewIndex = '__newindex';
	LuaID_GC = '__gc';
	LuaID_TableMode = '__mode';
	LuaID_WeakBoth = 'vk';
	LuaID_GlobalsVar = '_G';
	LuaID_Call = '__call';
	LuaID_Concat = '__concat';
	LuaOps: array[ScriptOperatorEnum] of record
		binary: boolean;
		id: string;
	end =
	(
		(binary: yes; id: '__add'),
		(binary: yes; id: '__sub'),
		(binary: yes; id: '__mul'),
		(binary: yes; id: '__div'),
		(binary: yes; id: '__pow'),
		(binary: no; id: '__unm')
	);

type
	BuiltinFunction = (func_Generic, func_ZeroResults, func_OneResult, func_Custom);

	procedure HandleStackTraceOption(id: uint; const value: StringView; param: pointer);
	begin
		case id of
			0 {Title}: pStackTrace(param)^.title := value.ToString;
		{$ifdef Debug} else raise ExhaustiveCase(id, 'StackTrace.option'); {$endif}
		end;
	end;

	procedure StackTrace.Init(const newMessage: string; newThread: lua.State);
	const
		Options: array[0 .. 0] of string = (WindowTitle);
	begin
		title := '';
		thread := newThread;
		items := nil;
		message := StringOptionals.Split(newMessage, Options, @HandleStackTraceOption, @self).ToString;
	end;

	procedure StackTrace.Done;
	begin
	end; {$define classname := StackTrace} {$define pSelf := pStackTrace} {$define free_only} {$include dyn_obj.pp.inc}

	procedure StackTrace.Add(const item: string; reps: sint);
	var
		i: sint;
	begin
		if (item = '') or ((length(items) = 0) and Prefixed(item, message)) then exit;
		if (length(items) > 0) and (item = items[High(items)].item) then
			items[High(items)].reps += reps
		else
		begin
			i := length(items);
			SetLength(items, i + 1);
			items[i].item := item;
			items[i].reps := reps;
		end;
	end;

	procedure StackTrace.Add(const item: string);
	begin
		Add(item, 1);
	end;

	function StackTrace.Human(mode: HumanMode): string;

		function MaybeAppendArrow(const s: string): string;
		begin
			result := s;
			if result <> '' then result += ' <- ';
		end;

	var
		i: sint;
	begin
		if mode = MultiLineWithoutMessage then result := '' else result := FixupSentence(message);
		for i := 0 to High(items) do
		begin
			case mode of
				OneLine: result := MaybeAppendArrow(result);
				MultiLine, MultiLineWithoutMessage: result += IfThen(result <> '', EOL) + '↑';
			end;
			result += items[i].item;
			if items[i].reps > 1 then result += ' x' + ToString(items[i].reps);
		end;

		if length(items) = 0 then
		begin
			if result = '' then result := 'стек пуст';
			if mode in [MultiLine, MultiLineWithoutMessage] then result := FixupSentence(result);
		end;
	end;

	function StackTrace.DestructiveHuman(mode: HumanMode): string;
	begin
		result := Human(mode);
		Done;
	end;

	procedure StackTrace.Append(const trace: StackTrace);
	var
		i: sint;
	begin
		if message = '' then message := trace.message else Add(trace.message);
		if title = '' then title := trace.title;
		for i := 0 to High(trace.items) do
			Add(trace.items[i].item, trace.items[i].reps);
		thread := trace.thread;
	end;

	procedure StackTrace.DestructiveAppendTo(var trace: StackTrace);
	begin
		trace.Append(self);
		Done;
	end;

	procedure ScriptGC.Collect;                 begin lua.gc(ls, lua.GCCOLLECT, 0); end;
	function ScriptGC.Allocated: size_t;        begin result := lua.gc(ls, lua.GCCOUNT, 0) * 1024 + lua.gc(ls, lua.GCCOUNTB, 0); end;
	procedure ScriptGC.SetPause(pause: size_t); begin lua.gc(ls, lua.GCSETPAUSE, pause); end;
	procedure ScriptGC.SetStepMul(sm: size_t);  begin lua.gc(ls, lua.GCSETSTEPMUL, sm); end;

	procedure RunNativeFiber(param: pointer);
	var
		fiber: ScriptState.pNativeFiber absolute param;
	begin
		fiber^.body(fiber^, fiber^.ss^, fiber^.param);
	end;

	procedure ScriptState.NativeFiber.Init(const newNameOf: string; newSS: pScriptState; newBody: MainProc; newParam: pointer);
	begin
		System.Initialize(self);
	{$ifdef Debug} yieldResults := -1; {$endif}
		thisFiber := Fiber.Create(newNameOf, @RunNativeFiber, @self);
		body := newBody;
		ss := newSS;
		param := newParam;
		finalizing := no;
	{$ifdef DebugFibers}
		Log('Создана сопрограмма ' + Human, logDebug);
	{$endif}
	end;

	procedure ScriptState.NativeFiber.Done;
	var
		finished: boolean;
	{$ifdef Debug} extraResumes: sint; {$endif}
	begin
		if not thisFiber^.finished then
		begin
		{$ifdef Debug}
			LogR('Принудительно завершаю сопрограмму {0}... ', HumanOf, logDebug);
			extraResumes := 0;
		{$endif}
			finalizing := yes;

			repeat
			{$ifdef Debug} inc(extraResumes); {$endif}
				ss^.Pop(Resume(finished));
			until finished;

		{$ifdef Debug} Log('Сопрограмма {0} завершена с {1}', HumanOf, lang_amount(extraResumes, '{N} попыт{ки/ок/ок}'), logOK); {$endif}
		end;

		thisFiber^.Done;
		System.Finalize(self);
	end;

unsafe_exceptions
	function ScriptState.NativeFiber.Resume(out finished: boolean): sint;
	var
		top, nResults: sint;
	begin
		finished := no;
	{$ifdef Debug} if yieldResults >= 0 then ss^.Throw('C-Lua-волокно возобновлено дважды'); {$endif}
		top := ss^.Top;
		finished := FiberFinished in thisFiber^.Resume;
		nResults := ss^.Top - top;
	{$ifdef Debug}
		Assert((yieldResults = nResults) or (thisFiber^.finished and (yieldResults < 0)), 'yieldResults = ' + Utils.ToString(yieldResults) + ', nResults = ' + Utils.ToString(nResults) + ', при этом волокно не завершено');
		yieldResults := -1;
	{$endif}
		Assert(nResults >= 0);
		result := nResults;
	end;
_end

	function ScriptState.NativeFiber.Yield(nResults: sint): YieldOutcome;
	begin
		Assert(nResults >= 0);
		yieldResults := nResults;
		thisFiber^.Yield;
		result := [];
		if finalizing then result += [Discarded];
	end;

{$ifdef Debug}
	function ScriptState.NativeFiber.HumanOf: string;
	begin
		result := thisFiber^.Human(FullOf);
	end;
{$endif}

	function ScriptState.EnvIndex.Make(index: R1FunctionBody; newIndex: R0FunctionBody; newParam: pointer): EnvIndex;
	begin
	{$ifdef Debug} result.magic := CorrectMagic; {$endif}
		result.index    := index;
		result.newIndex := newIndex;
		result.param    := newParam;
	end;

{$ifdef Debug}
	procedure ScriptState._Dump(baseTable, idx: sint; var sb: StringBuilder; indent: sint; var nextUid: sint);

		function AlreadySeen(idx: sint): boolean;
		begin
			lua.pushvalue(ls, idx);
			lua.rawget(ls, baseTable);
			result := NonNil(-1);
			Pop;
		end;
		function GetUid(idx: sint; out seen: boolean): sint;
		begin
			lua.pushvalue(ls, idx);
			lua.rawget(ls, baseTable);
			seen := NonNil(-1);
			if seen then
			begin
				result := ToSint(-1);
				Pop;
			end else
			begin
				Pop;
				lua.pushvalue(ls, idx);
				PushSint(nextUid);
				lua.rawset(ls, baseTable);
				result := nextUid;
				inc(nextUid);
			end;
		end;
		function UidStr(idx: sint; out seen: boolean): string;
		begin
			result := '(#' + Utils.ToString(GetUid(idx, seen)) + ')';
		end;
		function NewLine: string;
		begin
			result := EOL + StrDup('　', 4 * indent);
		end;

	var
		seen, first, hasname, hasnamewhat, hassource: boolean;
		info: lua.Debug;
		t: string;
		o, ot: pointer;
		uvname: pChar;
		i: sint;
	begin
		lua.checkstack(ls, 20);
		baseTable := lua.absindex(ls, baseTable);
		idx := AbsIdx(idx);
		case lua.&type(ls, idx) of
			lua.TNIL, lua.TBOOLEAN, lua.TNUMBER: sb.Append(ToString(idx));
			lua.TLIGHTUSERDATA: sb.Append('ptr: ', Utils.ToString(lua.touserdata(ls, idx)));
			lua.TSTRING: sb.Append('"', ToString(idx), '"');
			lua.TTABLE:
				begin
					sb.Append(UidStr(idx, seen));
					if not seen then
					begin
						sb.Append(NewLine, '{');
						inc(indent);
						lua.pushnil(ls);
						first := yes;
						while lua.next(ls, idx) <> 0 do
						begin
							if first then first := no else sb.Append(',');
							sb.Append(NewLine, '[');
							_Dump(baseTable, -2, sb, indent, nextUid);
							sb.Append('] => ');
							_Dump(baseTable, -1, sb, indent, nextUid);
							Pop;
						end;
						if lua.getmetatable(ls, idx) <> 0 then
						begin
							sb.Append(',', NewLine, '(meta) = ');
							_Dump(baseTable, -1, sb, indent, nextUid);
							Pop;
						end;
						dec(indent);
						sb.Append(NewLine, '}');
					end;
				end;
			lua.TFUNCTION:
				begin
					sb.Append(UidStr(idx, seen) + ' function');
					if not seen then
					begin
						lua.pushvalue(ls, idx);
						if lua.getinfo(ls, '>nSu', (@info)^) <> 0 then
						begin
							hasname := info.name <> nil;
							hasnamewhat := length(string(info.namewhat)) > 0;
							hassource := info.source <> nil;
							if hassource and (string(info.source) = '=[C]') then
							begin
								hassource := no;
								sb.Append('(native)');
							end else
							begin
								sb.Append('(', Utils.ToString(info.nparams));
								if info.isvararg <> 0 then sb.Append(', ...');
								sb.Append(')');
							end;
							if info.nups > 0 then sb.Append(', ' + Utils.ToString(info.nups) + ' ups');
							if hasname or hasnamewhat or hassource or (info.nups > 0) then
							begin
								sb.Append(NewLine, '{');
								inc(indent);
								if hasname then sb.Append(NewLine, 'name = ', string(info.name));
								if hasnamewhat then sb.Append(NewLine, 'what = ', string(info.namewhat));
								if hassource then
									sb.Append(NewLine, 'source = ', string(info.source), ':' + Utils.ToString(info.linedefined) + '-' + Utils.ToString(info.lastlinedefined));
								for i := 1 to info.nups do
								begin
									uvname := lua.getupvalue(ls, idx, i);
									sb.Append(NewLine, 'up' + Utils.ToString(i));
									t := string(uvname);
									if length(t) > 0 then sb.Append(' "' + t + '"');
									sb.Append(' = ');
									_Dump(baseTable, -1, sb, indent, nextUid);
									Pop;
								end;
								dec(indent);
								sb.Append(NewLine, '}');
							end;
						end;
					end;
				end;
			lua.TUSERDATA:
				begin
					sb.Append(UidStr(idx, seen) + ' ');
					ot := ObjType(idx);
					if IsPOD(idx) then o := ToData(idx) else o := ToObject(idx, ObjType_Any);
					if ot = ObjType_Vec[2] then sb.Append('vec2' + Utils.ToString(pVec2(o)^)) else
					if ot = ObjType_Vec[3] then sb.Append('vec3' + Utils.ToString(pVec3(o)^)) else
					if ot = ObjType_Vec[4] then sb.Append('vec4' + Utils.ToString(pVec4(o)^)) else
					if ot = ObjType_Quaternion then sb.Append('quaternion(' + Utils.ToString(pQuaternion(o)^) + ')') else
					if ot = ObjType_Transform then sb.Append('transform{ ' + Utils.ToString(pTransform(o)^) + '}') else
						sb.Append('object');
				{$ifdef use_serialization} if SerializationDB.Shared^.TypeName(ot) <> '' then sb.Append(' (', SerializationDB.Shared^.TypeName(ot), ')'); {$endif}
					lua.getuservalue(ls, idx);
					if NonNil(-1) then
					begin
						sb.Append(', assoc = ');
						_Dump(baseTable, -1, sb, indent, nextUid);
					end;
					Pop;
					if lua.getmetatable(ls, idx) <> 0 then
					begin
						sb.Append(', (meta) = ');
						_Dump(baseTable, -1, sb, indent, nextUid);
						Pop;
					end;
				end;
			lua.TTHREAD: sb.Append(UidStr(idx, seen) + ' thread');
			else
				sb.Append('???');
		end;
	end;

	function ScriptState._Dump(idx: sint): string;
	var
		sb: StringBuilder;
		uid: sint;
	begin
		sb.Init;
		uid := 0;
		lua.newtable(ls);
		_Dump(-1, AdjustIdx(idx, 1), sb, 0, uid);
		Pop;
		result := sb.DestructiveToString;
	end;

	function ScriptState._Dump: string;
	begin
		lua.pushvalue(ls, lua.REGISTRYINDEX);
		result := 'Дамп Lua Registry: ' + _Dump(-1);
		Pop;
	end;

	function ScriptState.DumpKeys(idx: sint): string;
	var
		first: boolean;
	begin
		result := '';
		first := yes;
		PushNil;
		while Next(AdjustIdx(idx, 1)) do
		begin
			if first then first := no else result += ', ';
			result += ToString(-2);
			Pop;
		end;
	end;

	function GetLocalStackItemString(id: uint; param: pointer): string;
	begin
		result := pScriptState(param)^.ToString(1 + id);
	end;

	function ScriptState.DumpLocalStack: string;
	begin
		result := SeparatedList.Join(Top, @GetLocalStackItemString, @self, ', ');
	end;
{$endif}

	function WrapError(ls: lua.State): pStackTrace;
	begin
		case lua.&type(ls, -1) of
			lua.TLIGHTUSERDATA:
				begin
					result := lua.touserdata(ls, -1);
					if result^.thread.p <> ls.p then
					begin
					{$ifdef Debug} Log('Поймана Lua-ошибка, уже завёрнутая в StackTrace. Дополняю стек (' + ss_by_ls^.StackTrace('').DestructiveHuman(OneLine) + ').', logDebug); {$endif}
						ss_by_ls^.StackTrace('').DestructiveAppendTo(result^);
					end;
				end;
			lua.TSTRING:
				begin
					new(result); result^ := ss_by_ls^.StackTrace(lua.tostring(ls, -1));
					lua.pop(ls);
					lua.pushlightuserdata(ls, result);
				end;
			else Assert(no);
		end;
	end;

	function LuaPanic(ls: lua.State): cint; cdecl;
		function WrapToException: Exception;
		var
			trace: pStackTrace;
		begin
			trace := WrapError(ls);
			result := Error(trace^.Human(MultiLine));
			trace^.Free;
		end;
	begin
		Assert(@result = @result);
		raise WrapToException;
	end;

	function PCallErrorHandler(ls: lua.State): cint; cdecl;
	begin
		WrapError(ls);
		result := 1;
	end;

	function _pcall(ls: lua.State; narg, nret: sint; errmsg: pString; leaveErrorObject: boolean): sint; cinline
	var
		msghidx, oldtop: sint;
		trace: pStackTrace;
	begin
		lua.pushcfunction(ls, @PCallErrorHandler); msghidx := -narg-2; lua.insert(ls, msghidx); msghidx := lua.absindex(ls, msghidx);

		if nret < 0 then oldtop := lua.gettop(ls);
		if lua.pcall(ls, narg, nret, msghidx) = lua.OK then
			if nret < 0 then result := lua.gettop(ls) - (oldtop - narg - 1) else result := nret
		else
		begin
			Assert(lua.&type(ls, -1) = lua.TLIGHTUSERDATA, 'трюк с указателем, переданным через обработчик Lua, не удался');
			if not leaveErrorObject then
			begin
				trace := lua.touserdata(ls, -1);
				if Assigned(errmsg) then errmsg^ := trace^.Human(OneLine);
				ss_by_ls^.ShowError(trace^, not Assigned(errmsg));
				trace^.Free;
				lua.pop(ls);
			end;
			result := -1;
		end;
		if msghidx <> 0 then lua.remove(ls, msghidx);
	end;

	procedure rawgetfield(ls: lua.State; idx: cint; const name: string);
	begin
		idx := lua.absindex(ls, idx);
		lua.pushlstring(ls, pointer(name), length(name));
		lua.rawget(ls, idx);
	end;

	procedure rawsetfield(ls: lua.State; idx: cint; const name: string);
	begin
		idx := lua.absindex(ls, idx);
		lua.pushstring(ls, name);
		lua.insert(ls, -2);
		lua.rawset(ls, idx);
	end;

	procedure _registerIn(ls: lua.State; idx: cint; const name: string; fn: lua.CFunction; nUpvals: cint = 0; remember: sint = -1);
	begin
		idx := lua.absindex(ls, idx);
		lua.pushcclosure(ls, fn, nUpvals);
		if remember >= 0 then
		begin
			lua.rawgeti(ls, lua.REGISTRYINDEX, Reg.CFunctionPtr);
			lua.pushvalue(ls, -2);
			if remember = ord(func_Custom) then lua.pushlightuserdata(ls, fn) else lua.pushinteger(ls, remember);
			lua.rawset(ls, -3);
			lua.pop(ls);
		end;
		rawsetfield(ls, idx, name);
	end;

	function _getApiMt(ls: lua.State; resType: pointer): boolean;
	begin
		lua.rawgeti(ls, lua.REGISTRYINDEX, Reg.ApiMt);
		lua.rawgetp(ls, -1, resType);
		result := not lua.isnil(ls, -1);
		if result then
			lua.remove(ls, -2)
		else
			lua.pop(ls, 2);
	end;

	function GCObject(ls: lua.State): cint; cdecl;
	begin
		result := 0;
		Assert(lua.&type(ls, 1) = lua.TUSERDATA);
		Release(pObject(pPointer(lua.touserdata(ls, 1))^));
	end;

	procedure InvalidIndex(var ss: ScriptState);
	begin
		ss.Throw('прочитано несуществующее поле {0}', ss.ToString(2));
	end;

	procedure InvalidNewIndex(var ss: ScriptState);
	begin
		ss.Throw('записано несуществующее поле {0}', ss.ToString(2));
	end;

	function IndexObject(ls: lua.State): cint; cdecl;
	var
		prop: ScriptState.pInternalPropertyDesc;
	begin
		result := 1; // obj key
		lua.pushvalue(ls, 2); // obj key key
		lua.rawget(ls, UPVALUE-1); // obj key fn
		case lua.&type(ls, -1) of
			lua.TFUNCTION: ;
			lua.TLIGHTUSERDATA:
				begin
					prop := lua.touserdata(ls, -1);
					if prop^.both then
						prop^.rw(ss_by_ls^, yes)
					else
						prop^.ro(ss_by_ls^);
				end;
			else
				begin
					lua.pop(ls, 1);
					ScriptState.R1FunctionBody(lua.touserdata(ls, UPVALUE-2))(ss_by_ls^);
				end;
		end;
	end;

	function NewIndexObject(ls: lua.State): cint; cdecl;
	var
		prop: ScriptState.pInternalPropertyDesc;
	begin
		result := 0; // obj key value
		lua.pushvalue(ls, 2); // obj key value key
		lua.rawget(ls, UPVALUE-1); // obj key value fn
		if lua.&type(ls, -1) = lua.TLIGHTUSERDATA then
		begin
			prop := lua.touserdata(ls, -1);
			lua.pop(ls, 1);
			if prop^.both then
				prop^.rw(ss_by_ls^, no)
			else
				prop^.wo(ss_by_ls^);
		end else
		begin
			lua.pop(ls, 1);
			ScriptState.R0FunctionBody(lua.touserdata(ls, UPVALUE-2))(ss_by_ls^);
		end;
	end;

	function GenericLuaFunction(ls: lua.State): cint; cdecl;
	begin
	trace_call('Script.GenericFunction');
		result := ScriptState.FunctionBody(lua.touserdata(ls, UPVALUE-1)) (ss_by_ls^);
	leave_call
	end;

	function GenericLuaFunctionR0(ls: lua.State): cint; cdecl;
	begin
	trace_call('Script.GenericFunctionR0');
		ScriptState.R0FunctionBody(lua.touserdata(ls, UPVALUE-1)) (ss_by_ls^);
		result := 0;
	leave_call
	end;

	function GenericLuaFunctionR1(ls: lua.State): cint; cdecl;
	begin
	trace_call('Script.GenericFunctionR1');
		ScriptState.R1FunctionBody(lua.touserdata(ls, UPVALUE-1)) (ss_by_ls^);
		result := 1;
	leave_call
	end;

	function BuiltinFunctionPointer(id: BuiltinFunction): lua.CFunction;
	begin
		case id of
			func_Generic: result := @GenericLuaFunction;
			func_ZeroResults: result := @GenericLuaFunctionR0;
			func_OneResult: result := @GenericLuaFunctionR1;
			else result := nil;
		end;
	end;

	function GenericLuaOperator(ls: lua.State): cint; cdecl;
	begin
		ScriptState.OperatorBody(lua.touserdata(ls, UPVALUE-1)) (ss_by_ls^, ScriptOperatorEnum(lua.tointegerx(ls, UPVALUE-2, nil)));
		result := 1;
	end;

	function GenericLuaROProperty(ls: lua.State): cint; cdecl;
	begin
	trace_call('Script.GenericROProperty');
		Assert(lua.gettop(ls) = 2);
		result := 1;
		ScriptState.ROPropertyBody(lua.touserdata(ls, UPVALUE-1)) (ss_by_ls^);
		Assert(lua.gettop(ls) = 3, 'неверный Lua-геттер');
	leave_call
	end;

	function GenericLuaRWProperty(ls: lua.State): cint; cdecl;
	begin
	trace_call('Script.GenericRWProperty');
		result := 3 - lua.gettop(ls);
		Assert((result = 0) or (result = 1));
		ScriptState.RWPropertyBody(lua.touserdata(ls, UPVALUE-1)) (ss_by_ls^, result <> 0);
	{$ifdef Debug}
		Assert((lua.gettop(ls) = 3) or ((result = 0) and (lua.gettop(ls) = 2)),
			Format('неверное Lua-свойство (ожидается результатов: {0}, стек: {1}', Utils.ToString(result), ss_by_ls^.DumpLocalStack));
	{$endif}
	leave_call
	end;

	function GenericLuaWOProperty(ls: lua.State): cint; cdecl;
	begin
	trace_call('Script.GenericWOProperty');
		Assert(lua.gettop(ls) = 3);
		result := 0;
		ScriptState.WOPropertyBody(lua.touserdata(ls, UPVALUE-1)) (ss_by_ls^);
		Assert(lua.gettop(ls) in [2, 3], 'неверный Lua-сеттер');
	leave_call
	end;

	function DummyLuaFunction(ls: lua.State): cint; cdecl;
	begin
		Assert(@ls = @ls);
		result := 0;
	end;

	function IndexLuaENV(ls: lua.State): cint; cdecl;
		procedure ThrowUnknownGlobalVariable; noreturn;
		begin
			ss_by_ls^.Throw('неизвестная глобальная переменная {0}', ss_by_ls^.ToString(2));
		end;
	var
		ss: pScriptState;
		fn: ^ScriptState.BuiltinFunctionDesc;
		what: StringView;
	begin
		result := 1;
		lua.rawgeti(ls, lua.REGISTRYINDEX, lua.RIDX_GLOBALS);
		lua.pushvalue(ls, 2);
		lua.rawget(ls, -2);
		if not lua.isnil(ls, -1) then exit;

		ss := ss_by_ls;
		if lua.&type(ls, UPVALUE-1) = lua.TLIGHTUSERDATA then
		begin
			ScriptState.R1FunctionBody(lua.touserdata(ls, UPVALUE-1))(ss_by_ls^);
			exit;
		end;

		case lua.&type(ls, 2) of
			lua.TSTRING:
				begin
					what := ss^.ToStringView(2);
					if ss^.envDepFns.Find(what, fn) then
					begin
						lua.pushlightuserdata(ls, fn^.underlying);
						ss^.ExpectMeta(1);
						lua.pushcclosure(ls, fn^.main, 2);
					end
					else if (what = EnvMt_This) and (lua.getmetatable(ls, 1) <> 0) then
					else if what = EnvMt_Here then
					begin
						ss^.ExpectMeta(1);
						ss^.PushPath(nil, yes)^.Append(ss^.GetStringField(-2, EnvMt_StreamBase));
						ss^.Remove(-2);
						exit;
					end
					else ThrowUnknownGlobalVariable;
				end;
			lua.TNUMBER:
				begin
					ss^.ExpectMeta(1);
					if ss^.TryGetTableI(-1, EnvMt_Results) then
						lua.rawgeti(ls, -1, ss^.ToSint(2))
					else
						ThrowUnknownGlobalVariable;
				end;
			else ThrowUnknownGlobalVariable;
		end;
	end;

	function ScriptState.CopyTable(srcIdx, dstIdx: sint; doreplace: boolean): sint;
	begin
		result := 0;
		lua.pushnil(ls);
		while lua.next(ls, AdjustIdx(srcIdx, 1)) <> 0 do
		begin
			if not doreplace then
			begin
				GetWithKey(AdjustIdx(dstIdx, 2), -2);
				if not lua.isnil(ls, -1) then
				begin
					Pop(2);
					continue;
				end else
					Pop;
			end;
			SetKeyValue(AdjustIdx(dstIdx, 2), -2, -1);
			Pop;
			inc(result);
		end;
	end;

	procedure ScriptState.AddStuff(const stuff: array of StuffDesc);
		function CutOutFunctionClass(var name: string): BuiltinFunction;
		begin
			if CutSuffix(':0', name, @name) then result := func_ZeroResults else
			if CutSuffix(':1', name, @name) then result := func_OneResult else
				result := func_Generic;
		end;
		procedure ReadFunctions(const stuff: array of StuffDesc; var istuff: sint);
		var
			name, prefix: string;
			fn: FunctionBody;
			fc: BuiltinFunction;
			efn: BuiltinFunctionDesc;
			first, reqEnv: boolean;
			p: sint;
		begin
			lua.rawgeti(ls, lua.REGISTRYINDEX, lua.RIDX_GLOBALS); // _G(functable)

			name := stuff[istuff].s;
			if not CutPrefix(FunctionsDesc, name, @name) then Assert(no);

			p := Pos(PrefixedFunctions, name);
			if p > 0 then
			begin
				prefix := Copy(name, 1, p - 1);
				delete(name, 1, p + length(PrefixedFunctions) - 1);
				if not GetTableS(-1, prefix) then
				begin
					PushTable; // _G functable
					PushCopy(-1);
					SetTableS(-3, prefix);
				end; // _G functable
				Remove(-2);
			end;
			Assert(name <> '');

			first := yes;
			while first or ((istuff < length(stuff)) and not Prefixed(StartDescMagic, stuff[istuff].s)) do
			begin
				if first then first := no else name := stuff[istuff].s;
				fn := FunctionBody(stuff[istuff].p);
				reqEnv := CutSuffix(RequireEnv, name, @name);
				fc := CutOutFunctionClass(name);
				if reqEnv then
				begin
					efn.underlying := fn;
					efn.main := BuiltinFunctionPointer(fc);
					envDepFns.Add(name, efn);
				end else
				begin
					if Assigned(fn) then
					begin
						lua.pushlightuserdata(ls, fn); // functable fn_ptr
						_registerIn(ls, -2, name, BuiltinFunctionPointer(fc), 1, ord(fc));
					end else
						_registerIn(ls, -1, name, @DummyLuaFunction, 0, ord(func_Custom));
				end;
			{$ifdef use_serialization} if Assigned(fn) then SerializationDB.Shared^.RegisterFunc(fn); {$endif}
				inc(istuff);
			end;
			Pop;
		end;

		procedure InheritTableFromFirstUpvalue(target: sint; {$ifdef Debug} var desc: string; const descMod: string; {$endif} var n: sint);
		begin
			if not Assigned(lua.getupvalue(ls, -1, 1)) then Assert(no);
			if n <> 0 then CopyTable(-1, AdjustIdx(target, 1), no) else begin SetCopy(-1, AdjustIdx(target, 1)); {$ifdef Debug} desc += descMod; {$endif} n := -1; end;
			Pop(2);
		end;

		function CountProps(const stuff: array of StuffDesc; istuff: sint): sint;
		begin
			result := 0;
			while (istuff < length(stuff)) and not Prefixed(StartDescMagic, stuff[istuff].s) do
			begin
				if not Prefixed(TuneTypeMagic, stuff[istuff].s) and not (stuff[istuff].s[1] in ['A' .. 'Z']) then
					inc(result);
				inc(istuff);
			end;
		end;

		procedure ReadType(const stuff: array of StuffDesc; var istuff: sint);
		var
			typ, parentType: pointer;
			name: string;
			raccess, waccess, wonly, isPOD: boolean;
			fn: FunctionBody;
			fc: BuiltinFunction;
			readFunc: R1FunctionBody;
			writeFunc: R0FunctionBody;
			callFunc: FunctionBody;
			op: OperatorBody;
			opk: ScriptOperatorEnum;
			nIndex, nNewIndex, totalProps: sint;
			prop: pInternalPropertyDesc;
		{$ifdef Debug} discarded: string; {$endif}
		begin
			isPOD := IsSuffix(PODType, stuff[istuff].s);
			typ := stuff[istuff].p;
			parentType := nil;
			op := nil;
			readFunc := nil;
			writeFunc := nil;
			callFunc := nil;
			inc(istuff);
			nIndex := 0; nNewIndex := 0;
			totalProps := CountProps(stuff, istuff);
			prop := propsMem.Allocate(totalProps * sizeof(InternalPropertyDesc), [propsMem.AllocateFlag.Precise]);

			lua.rawgeti(ls, lua.REGISTRYINDEX, Reg.ApiMt); // meta_reg
		{$ifdef Debug}
			lua.rawgetp(ls, -1, typ);
			Assert(IsNil(-1), 'RegisterType: тип уже зарегистрирован');
			Pop;
		{$endif}
			if not isPOD then
			begin
				lua.rawgetp(ls, -1, ParentTypeOf(typ));
				if NonNil(-1) then parentType := ParentTypeOf(typ);
				Pop;
			{$if defined(Debug) and defined(use_serialization)}
				if (parentType = nil) and (ParentTypeOf(typ) <> TypeOf(&Object)) and Assigned(ParentTypeOf(typ)) then
					Log('Не удалось вычислить предка ' + SerializationDB.Shared^.TypeName(typ) + '.', logWarning);
			{$endif}
			end;
			lua.newtable(ls); // meta_reg meta
			lua.pushlightuserdata(ls, typ); SetTableI(-2, Reg.ApiMt_TypeOf);

			raccess := yes;
			PushTable; PushTable; // meta_reg meta index newindex
			while (istuff < length(stuff)) and not Prefixed(StartDescMagic, stuff[istuff].s) do
			begin
				if Prefixed(TuneTypeMagic, stuff[istuff].s) then
				begin
					if stuff[istuff].s = TypeOp then op := OperatorBody(stuff[istuff].p) else
					if stuff[istuff].s = TypeR then readFunc := R1FunctionBody(stuff[istuff].p) else
					if stuff[istuff].s = TypeW then writeFunc := R0FunctionBody(stuff[istuff].p) else
					if stuff[istuff].s = TypeCall then callFunc := FunctionBody(stuff[istuff].p)
					{$ifdef Debug} else Assert(no, 'Неожиданно: ' + stuff[istuff].s) {$endif};
				end else
				begin
					name := stuff[istuff].s;
					pointer(fn) := stuff[istuff].p;
					if name[1] in ['A' .. 'Z'] then
					begin
						fc := CutOutFunctionClass(name);
						lua.pushlightuserdata(ls, fn); // meta_reg meta index newindex method_fn_ptr
						_registerIn(ls, -3, name, BuiltinFunctionPointer(fc), 1);
						inc(nIndex);
					end else
					begin
						wonly := CutSuffix(WriteOnly, name, @name);
						raccess := not wonly;
						waccess := wonly or CutSuffix(Writeable, name, @name);

						prop^.both := raccess and waccess;
						prop^.funcPtr := fn;
						if raccess then
						begin
							lua.pushlightuserdata(ls, prop); // meta_reg meta index newindex prop_desc
							SetTableS(-3, name);
							inc(nIndex);
						end;
						if waccess then
						begin
							lua.pushlightuserdata(ls, prop); // meta_reg meta index newindex prop_desc
							SetTableS(-2, name);
							inc(nNewIndex);
						end;
						inc(prop); {$ifdef Debug} dec(totalProps); {$endif}
					end;
				end;
				inc(istuff);
			end; // meta_reg meta index newindex
		{$ifdef Debug} Assert(totalProps = 0); {$endif}

			if not isPOD then
			begin
				_registerIn(ls, -3, LuaID_GC, @GCObject);
				if Assigned(parentType) then
				begin
					lua.rawgetp(ls, -4, parentType); // meta_reg meta index newindex parent_meta
					if lua.istable(ls, -1) then
					begin
					{$ifdef Debug} discarded := ''; {$endif}
						if GetTableS(-1, LuaID_Index) then InheritTableFromFirstUpvalue(-4, {$ifdef Debug} discarded, 'R', {$endif} nIndex);
						if GetTableS(-1, LuaID_NewIndex) then InheritTableFromFirstUpvalue(-3, {$ifdef Debug} discarded, 'W', {$endif} nNewIndex);
					{$if defined(Debug) and defined(use_serialization)}
						if discarded <> '' then LogR('{0} ← {1} ({2}); ', SerializationDB.Shared^.TypeName(typ), SerializationDB.Shared^.TypeName(ParentTypeOf(typ)), discarded, logDebug);
					{$endif}
					end {$ifdef Debug} else Log('AddStuff: предок должен быть добавлен до потомков', logError) {$endif};
					Pop; // meta_reg meta index newindex
				end;
			end;

			if Assigned(op) then
				for opk in ScriptOperatorEnum do
				begin
					lua.pushlightuserdata(ls, op);
					lua.pushinteger(ls, ord(opk)); // meta_reg meta index newindex C1(fn_ptr) C2(op_id)
					_registerIn(ls, -5, LuaOps[opk].id, @GenericLuaOperator, 2);
				end;

			if Assigned(callFunc) then
			begin
				lua.pushlightuserdata(ls, callFunc);
				_registerIn(ls, -4, LuaID_Call, @GenericLuaFunction, 1);
			end;

			if (nIndex <> 0) or Assigned(readFunc) then
			begin
				if not Assigned(readFunc) then readFunc := @InvalidIndex;
				PushCopy(-2); // meta_reg meta index newindex C1(index)
				lua.pushlightuserdata(ls, readFunc); // meta_reg meta index newindex C1(index) C2(index_fn_ptr)
				_registerIn(ls, -5, LuaID_Index, @IndexObject, 2);
			end;

			if (nNewIndex <> 0) or Assigned(writeFunc) then
			begin
				if not Assigned(writeFunc) then writeFunc := @InvalidNewIndex;
				PushCopy(-1);
				lua.pushlightuserdata(ls, writeFunc);
				_registerIn(ls, -5, LuaID_NewIndex, @NewIndexObject, 2);
			end;

			Pop(2);
			lua.rawsetp(ls, -2, typ); // meta_reg
			Pop;
		end;

	var
		istuff: sint;
	begin
		istuff := 0;
		while istuff < length(stuff) do
			if Prefixed(FunctionsDesc, stuff[istuff].s) then ReadFunctions(stuff, istuff) else
			if Prefixed(TypeDesc, stuff[istuff].s) then ReadType(stuff, istuff)
		{$ifdef Debug} else Assert(no, Format('ScriptState.AddStuff: не разобрано #{0} "{1}"', Utils.ToString(istuff), stuff[istuff].s)) {$endif};
	end;

	function Script_USE(var ss: ScriptState): sint;
	var
		i, ia, nArg: sint;
	begin
		result := ss.Top;
		for i := 1 to result do
			case ss.Typ(i) of
				script_String, script_Pointer:
					if ss.LoadModule(ss.ToStream(i), 0, 0, [PushError], 0) < 0 then
						lua.error(ss.ls);
				script_Table:
					begin
						nArg := max(0, ss.RawLen(1) - 1);
						for ia := 1 to nArg do
							ss.GetTableI(i, ia + 1);
						if ss.LoadModule(ss.GetStreamField(1, 1), nArg, 0, [PushError], 0) < 0 then
							lua.error(ss.ls);
					end;
				else ss.Throw('недопустимый аргумент #{0} ({1})', ToString(i), ss.ToString(i));
			end;
	end;

	procedure Script_sin(var ss: ScriptState); begin ss.PushFloat(sin(ss.ToFloat(1))); end;
	procedure Script_cos(var ss: ScriptState); begin ss.PushFloat(cos(ss.ToFloat(1))); end;
	procedure Script_rad(var ss: ScriptState); begin ss.PushFloat(ss.ToFloat(1) * Deg2Rad); end;

{$define all :=
	var
		i: cint;
		r: lua.Number;
	begin
		r := ss.ToFloat(1);
		for i := 2 to ss.Top do r := func(r, ss.ToFloat(i));
		ss.PushFloat(r);
	end; {$undef func}}
	procedure Script_max(var ss: ScriptState); {$define func := max} all
	procedure Script_min(var ss: ScriptState); {$define func := min} all
{$undef all}

	procedure Script_clamp(var ss: ScriptState); begin ss.PushFloat(Clamp(ss.ToFloat(1), ss.ToFloat(2), ss.ToFloat(3))); end;
	procedure Script_abs(var ss: ScriptState); begin ss.PushFloat(abs(ss.ToFloat(1))); end;
	procedure Script_sign(var ss: ScriptState); begin ss.PushSint(IntSign(ss.ToFloat(1))); end;
	procedure Script_round(var ss: ScriptState); begin ss.PushFloat(round(ss.ToFloat(1))); end;
	procedure Script_floor(var ss: ScriptState); begin ss.PushFloat(floor(ss.ToFloat(1))); end;
	procedure Script_modf(var ss: ScriptState); begin ss.PushFloat(modf(ss.ToFloat(1), ss.ToFloat(2))); end;
	procedure Script_nangle(var ss: ScriptState); begin ss.PushFloat(NormalizeAngle(ss.ToFloat(1))); end;
	procedure Script_dot(var ss: ScriptState); begin ss.PushFloat(ss.ToVec3(1) ** ss.ToVec3(2)); end;

	procedure Script_angle(var ss: ScriptState);
	var
		ot: pointer;
		v: Vec3;
	begin
		ot := ss.ObjType(1);
		if ot = ObjType_Vec[2] then ss.PushFloat(Angle(ss.ToVec2(1), ss.ToVec2(2))) else
		if ot = ObjType_Vec[3] then ss.PushFloat(AngleN(ss.ToVec3(1), ss.ToVec3(2))) else
		if ot = ObjType_Quaternion then
		begin
			v := ss.ToVec3(3);
			ss.PushFloat(AngleN(ss.ToQuaternion(1) * v, ss.ToQuaternion(2) * v));
		end else
			ss.Throw('ожидается двух- или трёхмерный вектор');
	end;

	procedure Script_cross(var ss: ScriptState); begin ss.PushVec3(ss.ToVec3(1) >< ss.ToVec3(2)); end;

	procedure Script_smoothstep(var ss: ScriptState);
	begin
		case ss.Top of
			1: ss.PushFloat(SmoothStep(ss.ToFloat(1)));
			3: ss.PushFloat(SmoothStep(ss.ToFloat(1), ss.ToFloat(2), ss.ToFloat(3)));
			else ss.PushNil;
		end;
	end;

	procedure Script_remap(var ss: ScriptState);
	begin
		ss.PushFloat(remap(ss.ToFloat(1), ss.ToFloat(2), ss.ToFloat(3), ss.ToFloat(4), ss.ToFloat(5)));
	end;

	procedure Script_remapc(var ss: ScriptState);
	begin
		ss.PushFloat(remapc(ss.ToFloat(1), ss.ToFloat(2), ss.ToFloat(3), ss.ToFloat(4), ss.ToFloat(5)));
	end;

	procedure Script_tostring(var ss: ScriptState);
	var
		ff: FloatFormatDesc;
		i: sint;
		r: string;
	begin
		case ss.Typ(1) of
			script_Number:
				begin
					ff := FloatFormat;
					r := '';
					for i := 2 to ss.Top do
						case ss.Typ(i) of
							script_Number: ff := ff.AfterPoint(ss.ToSint(i));
							script_String:
								begin
									case ss.ToPChar(i)[0] of
										'b': r := ToStringSuff_b(ss.ToFloat(1), ff);
										's': r := ToStringSuff(ss.ToFloat(1), ff);
									end;
								end;
						end;
					if r = '' then r := ff.Apply(ss.ToFloat(1));
				end;
			else
				r := ss.ToString(1);
		end;
		ss.PushString(r);
	end;

	function GetFormatArgument(id: uint; param: pointer): StringView;
	var
		ss: pScriptState absolute param;
	begin
		id := 2 + id;
		if ss^.Typ(id) <> script_String then
		begin
			ss^.PushString(ss^.ToString(id));
			ss^.Replace(id);
		end;
		result := ss^.ToStringView(id);
	end;

	procedure Script_format(var ss: ScriptState);
	begin
	trace_call('Script.Format');
		ss.PushString(Format(ss.ToString(1), ss.Top - 1, @GetFormatArgument, @ss));
	leave_call
	end;

	function IndexLocalized(ls: lua.State): sint; cdecl;
	begin
		lua.rawseti(ls, 1, lua.rawlen(ls, 1) + 1);
		result := 1;
	end;

	procedure Script_Localized(var ss: ScriptState);
	begin
		ss.PushTable; // localized
		ss.PushString(ss.ToString(1)); ss.SetTableI(-2, 1);
		ss.PushLazy(lazy_LocalizedMeta); lua.setmetatable(ss.ls, -2);
		// lazy[localized] = ord(LocalizedMeta)
		lua.rawgeti(ss.ls, lua.REGISTRYINDEX, Reg.Lazy); ss.PushSint(ord(lazy_LocalizedMeta)); ss.SetWithKey(-2, -3); ss.Pop;
	end;

	procedure Script_fiber(var ss: ScriptState);      begin ss.PushFiber(1); end;
	function Script_yield(var ss: ScriptState): sint; begin result := lua.yield(ss.ls, ss.Top); end;

{$ifdef Debug}
	procedure Script_Internal(var ss: ScriptState);
	var
		count: uint;
	begin
		case ss.ToString(1) of
			'interned':
				if (ss.Typ(2) = script_String) and (ss.ToString(2) = 'cleanup') then
					CleanupInternedPool
				else
					Message.Text(DumpInternedStrings(count)).Title(Format('Пул строк ({0})', [count])).Show;
			'rff': Message.Text(ResourcePool.Shared^.Dump(count)).Title(Format('Пул ресурсов ({0})', [count])).Show;
			else ss.UnknownIdentifier(ss.ToString(1));
		end;
	end;

	function Script_crash(var ss: ScriptState): sint;
	begin
		pPointer(nil)^ := @ss;
		result := 1 div (@ss - @ss);
	end;
{$endif}

{$ifdef Debug}
{$define log_impl :=
var
	i: sint;
	s: string;
begin
	s := '';
	for i := 1 to ss.Top do
		s := s + ss.ToString(i);
	LogR(s, logstyle);
end;}
	procedure Script_Log(var ss: ScriptState);   {$define logstyle:=logPlain}   log_impl
	procedure Script_LogW(var ss: ScriptState);  {$define logstyle:=logWarning} log_impl
	procedure Script_LogOK(var ss: ScriptState); {$define logstyle:=logOk}      log_impl
	procedure Script_LogD(var ss: ScriptState);  {$define logstyle:=logDebug}   log_impl
{$undef logstyle} {$undef log_impl}

	procedure Script_Dump(var ss: ScriptState);
	begin
		if ss.Top = 0 then
			ss.PushString(ss._Dump)
		else
			ss.PushString(ss._Dump(1));
	end;
{$endif Debug}

	procedure Script_Ticks(var ss: ScriptState); begin ss.PushFloat(Ticks.Get.ToSeconds); end;
	procedure Script_MessageBox(var ss: ScriptState); begin Info.Show(ss.ToString(1)); end;

{$define vecf := start_vec_ids
	procedure bindingctr(var ss: ScriptState);
	var
		r: vec;
	begin
		case ss.Top of
			0: r := vec.Zero;
			1: r := ss.asvec(1);
		{$if veclen = 4} 2: r := vec.Make(ss.ToVec3(1), ss.ToFloat(2)); {$endif}
			veclen: r := vec.Make({$define one := ss.ToFloat(1 + itemid)} comma_separated);
		{$ifdef Debug} else ss.WrongArgc(vectypename); {$endif}
		end;
		ss.pushvec(r);
	end;

	procedure bindinglen(var ss: ScriptState; read: boolean);
	begin
		if read then
			ss.PushFloat(pvec(ss.ToData(1))^.Length)
		else
			pvec(ss.ToData(1))^.Length := ss.ToFloat(3);
	end;

	procedure bindinggetnorm(var ss: ScriptState); begin ss._PushData(pvec(ss.ToData(1))^.Normalized, ObjSize_Vec[veclen], bindingtypeof); end;

	procedure bindingop(var ss: ScriptState; op: ScriptOperatorEnum);
	var
		v: pvec;
		v2: vec;
		n: float;
	begin
		if ss.Typ(1) <> script_Object then ss.Insert(1);
		if LuaOps[op].binary and (ss.Typ(2) = script_Number) then
		begin
			v := ss.ToData(1);
			n := ss.ToFloat(2);
			case op of
				script_Add: ss._PushData(v^ + vec.Make(n), sizeof(v^), bindingtypeof);
				script_Sub: ss._PushData(v^ - vec.Make(n), sizeof(v^), bindingtypeof);
				script_Mul: ss._PushData(v^ * n, sizeof(v^), bindingtypeof);
				script_Div: ss._PushData(v^ / n, sizeof(v^), bindingtypeof);
				else ss.Throw('неверная операция вектор x число: {0}', LuaOps[op].id);
			end;
		end else
		begin
		{$ifdef Debug}
			if (LuaOps[op].binary) and (veclen <> ss.{$macro off} VecLen {$macro on} (2)) then
				ss.Throw('несовместимые векторы для {0}: {1} x {2} ({3})', LuaOps[op].id, Utils.ToString(veclen), Utils.ToString(ss.{$macro off} VecLen {$macro on} (2)), ss.ToString(2));
		{$endif}
			v := ss.ToData(1);
			if LuaOps[op].binary then
				v2 := ss.asvec(2);
			case op of
				script_Add: ss._PushData(v^ + v2, sizeof(v^), bindingtypeof);
				script_Sub: ss._PushData(v^ - v2, sizeof(v^), bindingtypeof);
				script_Mul: ss._PushData(v^ * v2, sizeof(v^), bindingtypeof);
				script_Div: ss._PushData(v^ / v2, sizeof(v^), bindingtypeof);
				script_Unm: ss._PushData(-v^, sizeof(v^), bindingtypeof);
				else ss.Throw('неверная операция "вектор x вектор"');
			end;
		end;
	end;

	procedure bindingr(var ss: ScriptState);
	var
		c: pChar;
		i, len: sint;
		v: pvec;
		r: Vec4;
	begin
		v := pvec(ss.ToData(1));
		case ss.Typ(2) of
			script_Number: ss.PushFloat(v^.data[ss.ToSint(2)]);
			script_String:
				begin
					c := ss.ToPChar(2);
					len := ss.RawLen(2);
					for i := 0 to len-1 do
						case c[i] of
							'x', 'y' {$if veclen >= 3}, 'z'{$endif}: r.data[i] := v^.data[ord(c[i]) - ord('x')];
						{$if veclen >= 4} 'w': r.data[i] := v^.w; {$endif}
							'X', 'Y' {$if veclen >= 3}, 'Z'{$endif}: r.data[i] := -v^.data[ord(c[i]) - ord('X')];
						{$if veclen >= 4} 'W': r.data[i] := -v^.w; {$endif}
							'1': r.data[i] := 1.0;
							'0': r.data[i] := 0.0;
							else ss.Throw('неверный компонент вектора: {0}', c[i]);
						end;
					case len of
						1: ss.PushFloat(r.x);
						2, 3, 4: ss._PushData(r, ObjSize_Vec[len], ObjType_Vec[len]);
					end;
				end;
			else ss.Throw('неверный индекс Vec*: ["{0}"]', ss.ToString(2));
		end;
	end;

	procedure bindingw(var ss: ScriptState);
	var
		c: ansichar;
		v: pvec;
		r: vec;
		pc: pChar;
		i, len: sint;
	begin
		v := pvec(ss.ToData(1));
		case ss.Typ(2) of
			script_Number: v^.data[ss.ToSint(2)] := ss.ToFloat(3);
			script_String:
				begin
					len := ss.RawLen(2);
					pc := ss.ToPChar(2);
					case len of
						1: pFloat(@r)^ := ss.ToFloat(3);
						2: pVec2(@r)^ := ss.ToVec2(3);
						3: pVec3(@r)^ := ss.ToVec3(3);
						4: pVec4(@r)^ := ss.ToVec4(3);
						else ss.Throw('неверный индекс Vec*: "{0}"', pc);
					end;
					for i := 0 to len - 1 do
					begin
						c := pc[i];
						case c of
							'x', 'y' {$if veclen >= 3}, 'z'{$endif}: v^.data[ord(c) - ord('x')] := r.data[i];
						{$if veclen >= 4} 'w': v^.w := ss.ToFloat(3); {$endif}
							else ss.Throw('неверный индекс Vec*: {0}', c);
						end;
					end;
				end;
			else ss.Throw('неверный индекс Vec*: [{0}]', ss.ToString(2));
		end;
	end; end_vec_ids}
	all_float_vectors

	procedure Script_Distance(var ss: ScriptState); begin ss.PushFloat(Distance(ss.ToVec3(1), ss.ToVec3(2))); end;
	procedure Script_closer(var ss: ScriptState); begin ss.PushFloat(closer(ss.ToFloat(1), ss.ToFloat(2), ss.ToFloat(3))); end;

	procedure Script_plane_X_line(var ss: ScriptState);
	begin
		ss.PushVec3(Plane.Make(ss.ToVec3(1), ss.ToVec3(2), ss.ToVec3(3)).IntersectLine(ss.ToVec3(4), ss.ToVec3(5)));
	end;

	procedure Script_Transform_Call(var ss: ScriptState);
	begin
		if ss.Top = 1 then
			ss.PushTransform(Transform.Identity)
		else
			ss.PushTransform(ss.ToTransform(2));
	end;

	procedure Script_Transform_Scale(var ss: ScriptState); begin ss.PushTransform(Scale(ss.ToFloat(1))); end;

	procedure Script_Transform_Translate(var ss: ScriptState);
	begin
		case ss.Top of
			0: ss.PushTransform(Transform.Identity);
			1: ss.PushTransform(Translate(ss.ToVec3(1)));
			3: ss.PushTransform(Translate(Vec3.Make(ss.ToFloat(1), ss.ToFloat(2), ss.ToFloat(3))));
			else ss.PushNil;
		end;
	end;

	procedure Script_Transform_Rotate(var ss: ScriptState);
	begin
		case ss.Top of
			1: ss.PushTransform(Rotate(ss.ToQuaternion(1)));
			2:
				if ss.IsNumber(1) then
					ss.PushTransform(Rotate(ss.ToFloat(1), ss.ToVec3(2)))
				else
					ss.PushTransform(Rotate(Quaternion.Rotation(ss.ToVec3(1), ss.ToVec3(2))));
			4: ss.PushTransform(Rotate(ss.ToFloat(1), Vec3.Make(ss.ToFloat(2), ss.ToFloat(3), ss.ToFloat(4))));
			else ss.PushNil;
		end;
	end;

	procedure Script_Transform_OP(var ss: ScriptState; op: ScriptOperatorEnum);
	begin
		if op = script_Mul then
		begin
			if ss.ObjType(2) = ObjType_Vec[3] then
				ss.PushVec3(ss.ToTransform(1) * ss.ToVec3(2))
			else
				ss.PushTransform(ss.ToTransform(1) * ss.ToTransform(2));
		end else
			ss.Throw('неверная операция над Transform');
	end;

	procedure Script_Transform_inversed(var ss: ScriptState); begin ss.PushTransform(ss.ToTransform(1).Inversed); end;

	procedure Script_QRot(var ss: ScriptState);
	begin
		case ss.Top of
			0: ss.PushQuaternion(Quaternion.Identity);
			1: ss.PushQuaternion(ss.ToQuaternion(1));
			2:
				if ss.IsNumber(1) then
					ss.PushQuaternion(Quaternion.Rotation(ss.ToFloat(1), ss.ToVec3(2)))
				else
					ss.PushQuaternion(Quaternion.Rotation(ss.ToVec3(1), ss.ToVec3(2)));
			4: ss.PushQuaternion(Quaternion.Rotation(ss.ToFloat(1), Vec3.Make(ss.ToFloat(2), ss.ToFloat(3), ss.ToFloat(4))));
			{$ifdef Debug} else ss.WrongArgc('QRot'); {$endif}
		end;
	end;

	procedure Script_gra(var ss: ScriptState); begin ss.PushQuaternion(AlignToGravity(ss.ToVec3(1), ss.ToVec3(2), ss.ToVec3(3))); end;
	procedure Script_Quaternion_getInverse(var ss: ScriptState); begin ss.PushQuaternion(ss.ToQuaternion(1).Inversed); end;

	procedure Script_Quaternion_OP(var ss: ScriptState; op: ScriptOperatorEnum);
	begin
		if op = script_Mul then
		begin
			if ss.IsVec3(2) then ss.PushVec3(ss.ToQuaternion(1) * ss.ToVec3(2)) else
				if ss.IsQuaternion(2) then
					ss.PushQuaternion(ss.ToQuaternion(1) * ss.ToQuaternion(2))
				else
					ss.PushNil;
		end else
			ss.PushNil;
	{$ifdef Debug} if ss.IsNil(-1) then ss.Throw('неверная операция над кватернионами'); {$endif}
	end;

	procedure Script_ScriptDelegateWrapper_R(var ss: ScriptState);
	var
		md: pScriptDelegateWrapper;
	begin
		md := ss.ToSelf;
		if not md^.GetNamed(ss, ss.ToString(2)) then ss.PushNil;
	end;

	procedure Script_ScriptDelegateWrapper_W(var ss: ScriptState);
	var
		md: pScriptDelegateWrapper;
	begin
		md := ss.ToSelf;
		md^.SetNamed(ss, ss.ToString(2));
	end;

	procedure Script_CreateRNG(var ss: ScriptState);
	const
		Default = Tiny;
	var
		rng: pRNG;
		id: string;
		i: sint;
	begin
		id := ss.ToString(1);
		if id = '' then i := ord(Default) else i := FindStr(id, Random.RNG.AlgorithmFlagIds);

		if i >= 0 then rng := new(pRNG, Init(Random.RNG.AlgorithmFlag(i))) else
		begin
			i := FindStr(id, Random.RNG.AlgorithmIds);
			if i < 0 then ss.UnknownIdentifier(id);
			rng := new(pRNG, Init(Random.RNG.Algorithm(i), ss.ToString(2)));
		end;
		ss.PushObject(rng);
	end;

	function Script_RNG_Get(var ss: ScriptState): sint;
	var
		rng: pRNG;
	begin
		rng := ss.ToSelf;
		case ss.Top of
			1: ss.PushFloat(rng^.GetFloat);
			2: ss.PushFloat(rng^.GetFloat(ss.ToFloat(2)));
			3: ss.PushFloat(rng^.GetFloat(ss.ToFloat(2), ss.ToFloat(3)));
		{$ifdef Debug} else ss.WrongArgc('RNG.Get'); {$endif}
		end;
		result := 1;
	end;

	procedure Script_RNG_Int(var ss: ScriptState);
	var
		rng: pRNG;
	begin
		rng := ss.ToSelf;
		case ss.Top of
			2: ss.PushSint(rng^.GetInt(ss.ToSint(2)));
			3: ss.PushSint(rng^.GetInt(ss.ToSint(2), ss.ToSint(3)));
		{$ifdef Debug} else ss.WrongArgc('RNG.Int'); {$endif}
		end;
	end;

	procedure Script_RNG_Normal(var ss: ScriptState);
	var
		rng: pRNG;
	begin
		rng := ss.ToSelf;
		case ss.Top of
			1: ss.PushFloat(rng^.Normal);
			2: ss.PushFloat(rng^.Normal(ss.ToFloat(2)));
		{$ifdef Debug} else ss.WrongArgc('RNG.Normal'); {$endif}
		end;
	end;

	procedure Script_RNG_Bell(var ss: ScriptState);
	var
		rng: pRNG;
	begin
		rng := ss.ToSelf;
		case ss.Top of
			2: ss.PushFloat(rng^.Bell(ss.ToFloat(2)));
			4: ss.PushFloat(rng^.Bell(ss.ToFloat(2), ss.ToFloat(3), ss.ToFloat(4)));
		{$ifdef Debug} else ss.WrongArgc('RNG.Bell'); {$endif}
		end;
	end;

type
	pChooseParam = ^tChooseParam;
	tChooseParam = record
		ss: pScriptState;
	case uint of
		0: (nResults, nTempResults: sint);
		1: (ridx, getWeightFunc: sint);
	end;

	function NextWeightFromTable(param: pointer): float;
	var
		p: pChooseParam absolute param;
	begin
		if p^.ss^.Next(2) then
		begin
			if p^.getWeightFunc <> 0 then // передана функция, возвращающая настоящее число-вес из значения таблицы
			begin
				p^.ss^.PushCopy(p^.getWeightFunc); // k v fn
				p^.ss^.PushCopy(-2);
				p^.ss^.Call(1, 1);
			end;
			result := p^.ss^.ToFloat(-1);
			if p^.getWeightFunc <> 0 then
				p^.ss^.Pop; // на стеке ключ и значение, в этом состоянии стек получают Beats и Dont
		end else
		begin
			p^.ss^.PushNil; // для согласованности с ситуацией, когда Choose вообще ничего не вызвала
			result := -1.0;
		end;
	end;

	procedure BeatsWeightFromTable(param: pointer);
	var
		p: pChooseParam absolute param;
	begin
		p^.ss^.Replace(p^.ridx + 1);
		p^.ss^.SetCopy(-1, p^.ridx);
	end;

	procedure DontBeatsWeightFromTable(param: pointer);
	var
		p: pChooseParam absolute param;
	begin
		p^.ss^.Pop(1);
	end;

	function GetWeightFromFunc(id: sint; param: pointer): float;
	var
		ss: pScriptState absolute param;
	begin
		ss^.PushCopy(2);
		ss^.PushSint(1 + id);
		ss^.Call(1, 1);
		result := ss^.ToFloat(-1, -1.0);
		ss^.Pop;
	end;

	function NextWeightFromFiber(param: pointer): float;
	var
		p: pChooseParam absolute param;
		nr: sint;
	begin
		p^.ss^.PushCopy(2);
		nr := p^.ss^.Call(0);
		if nr = 0 then
		begin
			result := -1.0;
			p^.nTempResults := 0;
		end else
		begin
			result := p^.ss^.ToFloat(-nr);
			p^.ss^.Remove(-nr);
			p^.nTempResults := nr - 1;
		end;
	end;

	procedure BeatsWeightFromFiber(param: pointer);
	var
		p: pChooseParam absolute param;
	begin
		p^.ss^.Remove(-p^.nTempResults - p^.nResults, p^.nResults);
		p^.nResults := p^.nTempResults;
	end;

	procedure DontBeatsWeightFromFiber(param: pointer);
	var
		p: pChooseParam absolute param;
	begin
		p^.ss^.Pop(p^.nTempResults);
	end;

	function Script_RNG_Choose(var ss: ScriptState): sint;
	var
		rng: pRNG;
		p: tChooseParam;
	begin
		rng := ss.ToSelf;
		case ss.Typ(2) of
			script_Table:
				begin
					p.ss := @ss;
					p.getWeightFunc := IfThen(ss.IsFunction(3), 3, 0);
					p.ridx := ss.Top + 1; // В (p.ridx) и (p.ridx + 1) ключ и значение-результаты
					ss.PushNil(3); // ключ значение итератор
					rng^.Choose(@NextWeightFromTable, @BeatsWeightFromTable, @DontBeatsWeightFromTable, @p);
					ss.Pop;
					result := 2;
				end;
			script_Function:
				if ss.IsFiber(2) then
				begin
					// Начиная с 3-го индекса идёт nResults лучших результатов из волокна и nTempResults текущих
					p.ss := @ss;
					p.nResults := 0;
					rng^.Choose(@NextWeightFromFiber, @BeatsWeightFromFiber, @DontBeatsWeightFromFiber, @p);
					result := p.nResults;
				end else
				begin
					ss.PushSint(1 + rng^.Choose(@GetWeightFromFunc, @ss));
					result := 1;
				end;
			else
			begin
			{$ifdef Debug} ss.Throw('неверный аргумент RNG.Choose: {0}.', ss.ToString(2)); {$endif}
				result := 0;
			end;
		end;
	end;

	procedure Script_RNG_Direction2(var ss: ScriptState);
	begin
		ss.PushVec2(pRNG(ss.ToSelf)^.Direction2);
	end;

	procedure Script_RNG_Direction3(var ss: ScriptState);
	begin
		ss.PushVec3(pRNG(ss.ToSelf)^.Direction3);
	end;

	procedure Script_Global(var ss: ScriptState);
	type
		tTraverseResult = set of (Changed);

		function Traverse(idx: sint): tTraverseResult;
		begin
			result := [];

			case ss.Typ(idx) of
				script_String:
					begin
						ss.PushString(ScriptState.GlobalFilenameStart + ss.ToStream(idx));
						ss.Replace(ss.AdjustIdx(idx, 1));
						result += [Changed];
					end;
				script_Pointer:
					if lua.getmetatable(ss.ls, idx) <> 0 then
					begin
						ss.PushLazy(lazy_PathMeta);
						if lua.rawequal(ss.ls, -2, -1) then
							pScriptPath(ss.ToData(ss.AdjustIdx(idx, 2)))^.inPlace := no;
						ss.Pop(2);
					end;
				script_Table:
					begin
						ss.PushNil;
						while ss.Next(ss.AdjustIdx(idx, 1)) do
						begin
							if Changed in Traverse(-1) then
								ss.SetKeyValue(ss.AdjustIdx(idx, 2), -2, -1);
							ss.Pop;
						end;
					end;
			end;
		end;

	begin
		Traverse(1);
	end;

	function trusted_next(ls: lua.State): cint; cdecl;
	begin
		Assert(lua.istable(ls, 1));
		Assert(lua.gettop(ls) = 2);
		if lua.next(ls, 1) <> 0 then result := 2 else result := 0;
	end;

	function _api_error(ls: lua.State): cint; cdecl;

		procedure PushMessage;
		begin
			if (lua.gettop(ls) >= 1) and (lua.&type(ls, -1) = lua.TSTRING) then
				lua.pushstring(ls, lua.tostring(ls, -1))
			else
				lua.pushstring(ls, 'error() без объяснений');
		end;

	begin
		PushMessage;
		exit(lua.error(ls));
	end;

	function _api_assert(ls: lua.State): cint; cdecl;
	{$ifdef Debug}
		procedure PushMessage(idx: sint);
		begin
			if lua.&type(ls, idx) = lua.TSTRING then
				lua.pushstring(ls, lua.tostring(ls, idx))
			else
				lua.pushstring(ls, 'Ассерт не выполнен.');
		end;
	{$endif}
	begin
	{$ifdef Debug}
		if not lua.toboolean(ls, 1) then
		begin
			PushMessage(-1);
			exit(lua.error(ls));
		end;
	{$endif}
		lua.settop(ls, 1);
		result := 1;
	end;

	function _api_rawget(ls: lua.State): cint; cdecl;
	var
		i, argc: sint;
	begin
		if not lua.istable(ls, 1) then exit(0);
		argc := lua.gettop(ls);
		result := argc - 1;
		lua.checkstack(ls, result);
		for i := 2 to argc do
		begin
			lua.pushvalue(ls, i);
			lua.rawget(ls, 1);
		end;
	end;

	function _api_rawset(ls: lua.State): cint; cdecl;
	var
		i, npairs: sint;
	begin
		result := 0;
		if not lua.istable(ls, 1) then exit;
		npairs := (lua.gettop(ls) - 1) div 2;
		for i := 1 to npairs do
		begin
			if lua.isnil(ls, 2*i) then continue;
			lua.pushvalue(ls, 2*i);
			lua.pushvalue(ls, 2*i + 1);
			lua.rawset(ls, 1);
		end;
	end;

	function _api_pairs(ls: lua.State): cint; cdecl;
	begin
		if lua.&type(ls, 1) = lua.TTABLE then
		begin
			lua.pushcfunction(ls, @trusted_next);
			lua.pushvalue(ls, 1);
			lua.pushnil(ls);
			result := 3;
		end else
		begin
			lua.pushcfunction(ls, @DummyLuaFunction);
			result := 1;
		end;
	end;

	function trusted_inext(ls: lua.State): cint; cdecl;
	var
		i: sint;
	begin
		Assert(lua.istable(ls, 1));
		Assert(lua.gettop(ls) = 2); // t k
		i := lua.tointegerx(ls, 2, nil) + 1;
		lua.pushinteger(ls, i);     // t k new_k
		lua.rawgeti(ls, 1, i);      // t k new_k value
		if lua.&type(ls, 4) <> lua.TNIL then result := 2 else result := 0;
	end;

	function _api_ipairs(ls: lua.State): cint; cdecl;
	begin
		if lua.&type(ls, 1) = lua.TTABLE then
		begin
			lua.pushcfunction(ls, @trusted_inext);
			lua.pushvalue(ls, 1);
			lua.pushinteger(ls, 0);
			result := 3;
		end else
		begin
			lua.pushcfunction(ls, @DummyLuaFunction);
			result := 1;
		end;
	end;

	function _api_collectgarbage(ls: lua.State): cint; cdecl;
	var
		what: StringView;
	begin
		result := 0;
		if lua.gettop(ls) >= 1 then what := ss_by_ls^.ToStringView(1) else what := StringView.Make('collect');
		if what = 'collect' then lua.gc(ls, lua.GCCOLLECT, 0) else
		if what = 'strpool' then CleanupInternedPool else
		if what = 'stop' then lua.gc(ls, lua.GCSTOP, 0) else
		if what = 'restart' then lua.gc(ls, lua.GCRESTART, 0) else
		if what = 'isrunning' then
		begin
			lua.pushboolean(ls, lua.gc(ls, lua.GCISRUNNING, 0) <> 0);
			result := 1;
		end else
		if what = 'count' then
		begin
			lua.pushnumber(ls, lua.gc(ls, lua.GCCOUNT, 0) + lua.gc(ls, lua.GCCOUNTB, 0) / 1024.0);
			result := 1;
		end else
			ss_by_ls^.UnknownIdentifier(what.ToString);
	end;

	function _api_type(ls: lua.State): cint; cdecl;
	begin
		lua.pushstring(ls, lua.typename(ls, lua.&type(ls, 1)));
		result := 1;
	end;

	function _api_setmetatable(ls: lua.State): cint; cdecl;
	begin
		result := 1;
		if (lua.&type(ls, 1) in [lua.TTABLE, lua.TUSERDATA]) and (lua.&type(ls, 2) in [lua.TTABLE, lua.TNIL]) then
			lua.setmetatable(ls, 1);
		lua.pushvalue(ls, 1);
	end;

	function _api_getmetatable(ls: lua.State): cint; cdecl;
	begin
		result := cint((lua.&type(ls, 1) in [lua.TTABLE, lua.TUSERDATA]) and (lua.getmetatable(ls, 1) <> 0));
	end;

	function _api_unpack(ls: lua.State): cint; cdecl;
	var
		i: sint;
	begin
		if not lua.istable(ls, 1) then exit(0);
		result := lua.rawlen(ls, 1);
		lua.checkstack(ls, result);
		for i := 1 to result do
			lua.rawgeti(ls, 1, i);
	end;

const
	Wheels: array[0 .. 11] of record
		name: string;
		fn: lua.CFunction;
	end =
	(
		(name: 'collectgarbage'; fn: @_api_collectgarbage),
		(name: 'GC'; fn: @_api_collectgarbage),
		(name: 'type'; fn: @_api_type),
		(name: 'error'; fn: @_api_error),
		(name: 'assert'; fn: @_api_assert),
		(name: 'rawget'; fn: @_api_rawget),
		(name: 'rawset'; fn: @_api_rawset),
		(name: 'pairs'; fn: @_api_pairs),
		(name: 'setmetatable'; fn: @_api_setmetatable),
		(name: 'getmetatable'; fn: @_api_getmetatable),
		(name: 'unpack'; fn: @_api_unpack),
		(name: 'ipairs'; fn: @_api_ipairs)
	);

	function IndexString(ls: lua.State): cint; cdecl;
	var
		s: pChar;
		index, n: sint;
	begin
		case lua.&type(ls, 2) of
			lua.TNUMBER:
				begin
					s := lua.topchar(ls, 1);
					n := lua.rawlen(ls, 1);
					index := lua.tointegerx(ls, 2, nil);
					result := 1;
					if (index > 0) and (index <= n) then lua.pushlstring(ls, s + index - 1, 1) else
					if (index < 0) and (index >= -n) then lua.pushlstring(ls, s + n + index, 1) else
						result := 0;
				end;
			else
			begin
				lua.rawget(ls, UPVALUE-1);
				result := 1;
			end;
		end;
	end;

	function ConcatAnything(ls: lua.State): cint; cdecl;
	var
		ss: pScriptState;
	begin
		ss := ss_by_ls;
		lua.pushstring(ls, ss^.ToString(1) + ss^.ToString(2));
		result := 1;
	end;

	function IndexBoolean(ls: lua.State): cint; cdecl;
	begin
		Assert(not lua.toboolean(ls, 1));
		result := 0;
	end;

	function IndexFunction(ls: lua.State): cint; cdecl;
	begin
		lua.call(ls, 1, 1);
		result := 1;
	end;

	function ScriptState.StackTrace(const message: string): StackTrace;
	const
		MaxLevels = 16;
	var
		d: lua.Debug;
		level: sint;
		t: string;
	begin
		(@result)^.Init(message, ls);

		for level := 0 to MaxLevels - 1 do
		begin
			if lua.getstack(ls, level, d) = 0 then break;

			if lua.getinfo(ls, 'lS', d) = 0 then
			begin
				result.Add('(отладочная информация на уровне ' + Utils.ToString(level) + ' недоступна)');
				break;
			end;
			if d.what = 'C' then continue;

			t := d.short_src;
			if d.currentline > 0 then t += ':' + Utils.ToString(d.currentline);
			result.Add(t);
		end;
	end;

	function ScriptState.HumanStackTrace(const message: string; mode: StackTrace.HumanMode): string;
	begin
		result := StackTrace(message).DestructiveHuman(mode);
	end;

	procedure ScriptState.ShowError(const trace: StackTrace; allowBox: boolean);
	begin
	{$ifdef Debug} Log(trace.Human(OneLine), logError); {$endif}
		if _showErrors and allowBox then
		begin
			case Warning.Text(FixupSentence(trace.message)).Expanded(trace.Human(MultiLineWithoutMessage))
				.Title(IfThen(trace.title <> '', trace.title, 'Ошибка скрипта'))
				.ContinueOrStopVariants.Variant('Игнорировать эти ошибки').Show
			of
				TaskV1: ;
				TaskV3: _showErrors := no;
				else Fatal;
			end;
		end;
	end;

	procedure ScriptState.ShowError(const msg: string);
	var
		trace: StackTrace;
	begin
		trace := StackTrace(msg);
		ShowError(trace, yes);
		trace.Done;
	end;

	function ScriptState.GetFinalLocalized(idx: sint): string;
	var
		i, ir, ncp, n, nch: sint;
	begin
		n := RawLen(idx);
		nch := 0;
		for i := 1 to n do
		begin
			GetTableI(AdjustIdx(idx, i - 1), i);
			nch += RawLen(-1);
		end;

		SetLength(result, nch + n - 1);
		ir := 0;
		for i := 1 to n do
		begin
			if ir > 0 then
			begin
				pChar(result)[ir] := '.';
				inc(ir);
			end;
			ncp := RawLen(-n + (i-1));
			memcpy(ToPChar(-n + (i-1)), pChar(result) + ir, ncp * sizeof(char));
			ir += ncp;
		end;
		Assert(ir = length(result));
		result := Locale.Localized(result);
		Pop(n);
	end;

{$define func :=
	procedure ScriptState.Throw(const fmt: string; const _ARGS_: string);
	begin
		Throw(Format(fmt, _ARGS_));
	end;} {$include variadic.inc}

	procedure ScriptState.Throw(const msg: string);
	var
		trace: pStackTrace;
	begin
		new(trace);
		trace^ := StackTrace(msg);
		lua.pushlightuserdata(ls, trace);
		lua.error(ls);
	end;

{$ifdef Debug}
	function ScriptState.DumpDelegates(obj: pObject): string;
	var
		n: sint;
	begin
		if _GetObjectValueI(obj, ObjValue_Delegates) then
		begin
			n := 0;
			result := '';
			PushNil;
			while Next(-2) do
			begin
				inc(n);
				if result <> '' then result += ', ';
				result += ToString(-2);
				Pop;
			end;
			result := Utils.ToString(n) + ': (' + result + ')';
			Pop;
		end else
			result := 'N/A';
	end;
{$endif}

	procedure ScriptState.UnknownIdentifier(const id: string);
	begin
		Throw('Неизвестный идентификатор: {0}.', id);
	end;

	procedure ScriptState.WrongArgc(const fn: string);
	begin
		Throw('Неверное число аргументов {0} ({1})', fn, Utils.ToString(Top));
	end;

	function PoolAllocator(ud: pointer; ptr: pointer; osize, nsize: csize_t): pointer; cdecl;
	const
		MaxSize = 16 * 1024 * 1024;
{$ifdef Debug}
	var
		ty: sint;
		sty: Statistics.TotalSizeEnum;
{$endif}
	begin
	trace_call('Script.Pool.Realloc');
		if (nsize > MaxSize) and ((not Assigned(ptr)) or (nsize > osize)) then
		begin
		{$ifdef Debug} Log('Скрипт попытался выделить СЛИШКОМ большой блок памяти (' + ToStringSuff_b(nsize) + '!)', logError); {$endif}
			exit(nil);
		end;
	{$ifdef Debug}
		if Assigned(ptr) then
		begin
			ptr -= sizeof(pointer);
			osize += sizeof(pointer);
		end;
		if nsize <> 0 then
		begin
			stat.Increment(lua_total_blocks_mem, nsize);
			stat.Increment(n_lua_blocks);
			if Assigned(ptr) then ty := pPtrUint(ptr)^ else ty := osize;
			case ty of
				lua.TSTRING: sty := lua_strings_mem;
				lua.TTABLE: sty := lua_tables_mem;
				lua.TFUNCTION: sty := lua_functions_mem;
				lua.TUSERDATA: sty := lua_udatas_mem;
				lua.TTHREAD: sty := lua_threads_mem;
				else begin assert((ty = 10) or (ty = 9) or (ty = 0), Utils.ToString(ty)); sty := lua_other_mem; end;
			end;
			if Assigned(ptr) then
			begin
				if nsize > osize then stat.Increment(sty, nsize - osize);
			end else
				stat.Increment(sty, nsize);
			nsize += sizeof(pointer);
		end;
	{$endif}
		result := pMemoryPool(ud)^.ReallocMem(ptr, osize, nsize);
	{$ifdef Debug}
		if Assigned(result) then
		begin
			if not Assigned(ptr) then pPtrUint(result)^ := ty;
			result += sizeof(pointer);
		end;
	{$endif}
	leave_call
	end;

	constructor ScriptState.Init;
	const
		Stuff: array[0 .. 80 {$ifdef Debug} + 3 {$endif}] of ScriptStuffDesc =
		(
		{$define vecf := start_vec_ids
			(s: TypeDesc + PODType; p: bindingtypeofconst),
			(s: TypeOp; p: @bindingop),
			(s: TypeR; p: @bindingr),
			(s: TypeW; p: @bindingw),
			(s: 'length' + Writeable; p: @bindinglen),
			(s: 'normalized'; p: @bindinggetnorm),
			end_vec_ids} all_float_vectors

			(s: TypeDesc + PODType; p: quaternion_typeof_const),
			(s: TypeOp; p: @Script_Quaternion_OP),
			(s: TypeR; p: @Script_Vec4_R),
			(s: TypeW; p: @Script_Vec4_W),
			(s: 'inversed'; p: @Script_Quaternion_getInverse),

			(s: TypeDesc + PODType; p: transform_typeof_const),
			(s: TypeOp; p: @Script_Transform_OP),
			(s: 'inversed'; p: @Script_Transform_inversed),

			(s: FunctionsDesc + 'math' + PrefixedFunctions + 'sin:1'; p: @Script_sin),
			(s: 'cos:1'; p: @Script_cos),
			(s: 'rad:1'; p: @Script_rad),
			(s: 'max:1'; p: @Script_max),
			(s: 'min:1'; p: @Script_min),
			(s: 'clamp:1'; p: @Script_clamp),
			(s: 'abs:1'; p: @Script_abs),
			(s: 'sign:1'; p: @Script_sign),
			(s: 'round:1'; p: @Script_round),
			(s: 'floor:1'; p: @Script_floor),
			(s: 'modf:1'; p: @Script_modf),
			(s: 'nangle:1'; p: @Script_nangle),
			(s: 'dot:1'; p: @Script_dot),
			(s: 'cross:1'; p: @Script_cross),
			(s: 'angle:1'; p: @Script_angle),
			(s: 'smoothstep:1'; p: @Script_smoothstep),
			(s: 'remap:1'; p: @Script_remap),
			(s: 'remapc:1'; p: @Script_remapc),
			(s: 'distance:1'; p: @Script_Distance),
			(s: 'closer:1'; p: @Script_closer),
			(s: 'plane_X_line:1'; p: @Script_plane_X_line),
			(s: 'gra:1'; p: @Script_Gra),

			(s: FunctionsDesc + 'Transform' + PrefixedFunctions + 'Translate:1'; p: @Script_Transform_Translate),
			(s: 'Rotate:1'; p: @Script_Transform_Rotate),
			(s: 'Scale:1'; p: @Script_Transform_Scale),

			(s: TypeDesc; p: TypeOf(ScriptDelegateWrapper)),
			(s: TypeR; p: @Script_ScriptDelegateWrapper_R),
			(s: TypeW; p: @Script_ScriptDelegateWrapper_W),

			(s: FunctionsDesc + 'CreateRNG:1'; p: @Script_CreateRNG),

			(s: TypeDesc; p: TypeOf(RNG)),
			(s: TypeCall; p: @Script_RNG_Get),
			(s: 'Get'; p: @Script_RNG_Get),
			(s: 'Int:1'; p: @Script_RNG_Int),
			(s: 'Normal:1'; p: @Script_RNG_Normal),
			(s: 'Bell:1'; p: @Script_RNG_Bell),
			(s: 'Choose'; p: @Script_RNG_Choose),
			(s: 'Direction2:1'; p: @Script_RNG_Direction2),
			(s: 'Direction3:1'; p: @Script_RNG_Direction3),

			(s: FunctionsDesc + 'Global' + RequireEnv; p: @Script_Global),
			(s: 'Use' + RequireEnv; p: @Script_USE),
			(s: 'QRot:1'; p: @Script_QRot),
			(s: 'Vec2:1'; p: @Script_Vec2),
			(s: 'Vec3:1'; p: @Script_Vec3),
			(s: 'Vec4:1'; p: @Script_Vec4),
			(s: 'Log:0'; p: {$ifdef Debug} @Script_Log {$else} nil {$endif}),
			(s: 'LogW:0'; p: {$ifdef Debug} @Script_LogW {$else} nil {$endif}),
			(s: 'LogOK:0'; p: {$ifdef Debug} @Script_LogOK {$else} nil {$endif}),
			(s: 'LogD:0'; p: {$ifdef Debug} @Script_LogD {$else} nil {$endif}),
		{$ifdef Debug}
			(s: 'Dump:1'; p: @Script_Dump),
			(s: 'Internal:0'; p: @Script_Internal),
			(s: 'crash'; p: @Script_crash),
		{$endif}
			(s: 'Ticks:1'; p: @Script_Ticks),
			(s: 'MessageBox:0'; p: @Script_MessageBox),
			(s: 'tostring:1'; p: @Script_tostring),
			(s: 'fiber:1'; p: @Script_fiber),
			(s: 'yield'; p: @Script_yield),
			(s: 'format:1'; p: @Script_format),
			(s: 'Localized:1'; p: @Script_Localized)
		);
	var
		i: sint;
	begin
		try
			lua.loader.Load;
		except
			instant_reraise_from_constructor;
		end;
		inherited Init;

		memPool.Init('Lua', MemoryPoolBlockSize);
		ls := lua.newstate(@PoolAllocator, @memPool);
		pss_by_ls^ := @self;
		lua.atpanic(ls, @LuaPanic);
		lua.onthrow(ls, lua.DefaultThrowFunc, lua.DefaultPCallFunc);
	{$ifdef Debug}
		i := round(lua.version(ls)^);
		Log('Версия Lua: ' + Utils.ToString(i div 100) + '.' + Utils.ToString(i mod 100), logDebug);
	{$endif}
		_priorityToBinaries := no;
		_assertions := {$ifdef Debug} yes {$else} no {$endif};
		guards := nil;
		envDepFns.Init;
		timedLinks := nil;
		expiredTLs := nil;
		propsMem.Init;
		expiredTLsLock.Init;
	{$ifdef Debug} _insideCall := 0; {$endif}
		_showErrors := yes;

	{$ifdef Debug}
		startStackTop := lua.gettop(ls);
		LogR('Загрузка общего скриптового интерфейса... ');
	{$endif}

		PushTable; SetTableI(lua.REGISTRYINDEX, Reg.ApiMt);

		// метатаблица для nil'ов. Индексирование nil'а даёт nil.
		PushNil;
		PushTable(0, 1);
			PushTable; SetTableS(-2, LuaID_Index);
			lua.setmetatable(ls, -2);
		Pop;

		// Индексировать yes нельзя. Индексирование no аналогично индексированию nil'а.
		PushBool(no);
		PushTable(0, 1);
		{$ifdef Debug} lua.pushcfunction(ls, @IndexBoolean); {$else} PushNil; ExpectMeta(-1); Remove(-2); {$endif} SetTableS(-2, LuaID_Index);
			lua.setmetatable(ls, -2);
		Pop;

		// Индексирование функции вызывает её с аргументом.
		lua.pushcfunction(ls, @_api_getmetatable);
		PushTable(0, 1);
			lua.pushcfunction(ls, @IndexFunction); SetTableS(-2, LuaID_Index);
			lua.setmetatable(ls, -2);
		Pop;

		PushTable(0, 1);
			PushString(LuaID_WeakBoth); SetTableS(-2, LuaID_TableMode);
		SetTableI(lua.REGISTRYINDEX, Reg.WeakBoth);

		PushTable;
			GetTableI(lua.REGISTRYINDEX, Reg.WeakBoth); lua.setmetatable(ls, -2);
		SetTableI(lua.REGISTRYINDEX, Reg.ObjUserdatas);
		PushTable;
			GetTableI(lua.REGISTRYINDEX, Reg.WeakBoth); lua.setmetatable(ls, -2);
		SetTableI(lua.REGISTRYINDEX, Reg.Modules);
		PushTable;
			GetTableI(lua.REGISTRYINDEX, Reg.WeakBoth); lua.setmetatable(ls, -2);
		SetTableI(lua.REGISTRYINDEX, Reg.CFunctionPtr);
		PushTable;
			GetTableI(lua.REGISTRYINDEX, Reg.WeakBoth); lua.setmetatable(ls, -2);
		SetTableI(lua.REGISTRYINDEX, Reg.DontSerialize);

		lua.rawgeti(ls, lua.REGISTRYINDEX, lua.RIDX_GLOBALS);
		for i := 0 to High(Wheels) do
			_registerIn(ls, -1, Wheels[i].name, Wheels[i].fn, 0, ord(func_Custom));
		SetGlobal(LuaID_GlobalsVar);

		PushBool({$ifdef Debug} yes {$else} no {$endif}); SetGlobal('DEBUG');
		GetTableI(lua.REGISTRYINDEX, Reg.Modules); SetGlobal('_U');

		AddStuff(Stuff);

		// метатаблица для строк: индексирование [-len .. -1, 1 .. len], конкатенация (строка .. что угодно)
		PushString('');
		PushTable;
			PushTable;
				GetGlobal('format'); SetTableS(-2, 'format');
			lua.pushcclosure(ls, @IndexString, 1); SetTableS(-2, LuaID_Index);
			lua.pushcfunction(ls, @ConcatAnything); SetTableS(-2, LuaID_Concat);
			lua.setmetatable(ls, -2);
		Pop;

		GetGlobal('math');
			SetFloatField(-1, 'pi', Pi);
			SetFloatField(-1, 'twopi', TwoPi);
			SetFloatField(-1, 'halfpi', HalfPi);
		Pop;
		GetGlobal('Transform');
			PushTable(0, 1); lua.pushlightuserdata(ls, @Script_Transform_Call); _registerIn(ls, -2, LuaID_Call, @GenericLuaFunctionR1, 1);
			lua.setmetatable(ls, -2);
		Pop;
		PushObject(@GlobalRNG); SetGlobal('RNG');
	{$ifdef Debug} Log('Общий скриптовый интерфейс загружен', logOK); {$endif}
	end;

	destructor ScriptState.Done;
	var
		i: sint;
	begin
		if instantly_reraised_from_constructor then exit;
		propsMem.Done;
		for i := 0 to High(timedLinks) do
		begin
			timedLinks[i]^.timer.Close;
			dispose(timedLinks[i]);
		end;
		timedLinks := nil;
		expiredTLs := nil;
		expiredTLsLock.Done;

	{$ifdef Debug}
		if startStackTop <> lua.gettop(ls) then
			Log('Утечка стека Lua: вершина в ' + Utils.ToString(lua.gettop(ls)) + ', должна быть ' + ToString(startStackTop), logError);
	{$endif}
		Assert(length(guards) = 0);
	{$ifdef Debug} LogR('Закрываю состояние скрипта... '); {$endif}
		lua.close(ls);
		envDepFns.Done;
		memPool.Done;
	{$ifdef Debug} Log('Состояние скрипта закрыто', logOK); {$endif}
		lua.loader.Unload;
		inherited Done;
	end;

	function ScriptState.DoFile(const fileName: string; pushEnv: boolean; nArg, nRet: sint; userEnv: sint = 0; userIndex: pEnvIndex = nil): sint;
	var
		flags: LoadFlags;
	begin
		flags := []; if not pushEnv then flags += [DontPushEnv];
		if userEnv <> 0 then
		begin
			flags += [SupplyEnv];
			lua.rotate(ls, userEnv, -1); // ...args... userEnv
			Insert(-nArg - 1);
		end;
		if Assigned(userIndex) then
		begin
			flags += [SupplyIndex];
			lua.pushlightuserdata(ls, userIndex);
			Insert(-nArg - 1);
		end;
		result := _GenericLoad(fileName, nArg, nRet, flags, 0);
	end;

	function ScriptState.LoadModule(const stream: string; nArg, nRet: sint; keepFor: uint = 0): sint;
	begin
		result := _GenericLoad(stream, nArg, nRet, [Share], keepFor);
	end;

	function ScriptState._HookRequired: boolean;
	begin
		result := length(guards) > 0;
	end;

	procedure _Hook(ls: lua.State; var ar: lua.Debug); cdecl;
	var
		ss: pScriptState;
		g: ^ScriptState.Guard;
	{$ifdef Debug} useful: boolean; {$endif}
	begin
	{$ifdef Debug} useful := no; {$endif}
		Assert(@ar = @ar);
		ss := ss_by_ls;

		if length(ss^.guards) > 0 then
		begin
		{$ifdef Debug} useful := yes; {$endif}
			g := @ss^.guards[High(ss^.guards)];
			if Ticks.Get - g^.setupTime >= g^.timeout then
				if Warning.Text('Скрипт выполняется слишком долго — возможно, завис.').Title('Lua hook')
					.Variant('Продолжить')
					.Variant('Остановить').Show = TaskV2 then
				begin
					lua.pushstring(ls, 'Cкрипт завис.');
					lua.error(ls);
				end else
				begin
					g^.setupTime := Ticks.Get;
					g^.timeout := Ticks.FromSeconds(ss^.ContinuationTimeout);
				end;
		end;
	{$ifdef Debug} if not useful then Log('Lua-хуку нечего делать.', logWarning); {$endif}
	end;

	procedure ScriptState._MaybeSetHook;
	begin
		if not _HookRequired then lua.sethook(ls, @_Hook, 1 shl lua.HOOKCOUNT, 99999);
	end;

	procedure ScriptState._MaybeRemoveHook;
	begin
		if not _HookRequired then lua.sethook(ls, nil, 0, 0);
	end;

	procedure _TimedLink(param: pointer);
	var
		tl: ScriptState.pTimedLink absolute param;
		ss: pScriptState;
		id: sint;
	begin
		ss := tl^.ss;
		ss^.expiredTLsLock.Enter;
		Assert(tl^.expiredId < 0);

		id := length(ss^.expiredTLs);
		SetLength(ss^.expiredTLs, id + 1);
		ss^.expiredTLs[id] := tl;
		tl^.expiredId := id;
		ss^.expiredTLsLock.Leave;
	end;

	procedure ScriptState.SetupTimedLink(due: uint {$ifdef Debug}; const name: string {$endif});
	var
		tl: pTimedLink;
		id: sint;
	begin
		if not TryGetTableI(lua.REGISTRYINDEX, Reg.TimedLinks) then
		begin
			PushTable;
			PushCopy(-1); SetTableI(lua.REGISTRYINDEX, Reg.TimedLinks);
		end;

		GetWithKey(-1, -2); // obj TLs TLs[obj]
		if Typ(-1) = script_Pointer then
		begin
			tl := ToData(-1);
			Pop(3);

		{$ifdef Debug} inc(tl^.renews); {$endif}
			if tl^.timer.Reset(due, 0) > 0 then
			begin
				expiredTLsLock.Enter;
				Assert(tl^.expiredId >= 0);
				Assert(expiredTLs[tl^.expiredId] = tl);

				expiredTLs[tl^.expiredId] := expiredTLs[High(expiredTLs)];
				expiredTLs[High(expiredTLs)]^.expiredId := High(expiredTLs);
				SetLength(expiredTLs, length(expiredTLs) - 1);
				tl^.expiredId := -1;
				expiredTLsLock.Leave;
			end;
		end else
		begin
		{$ifdef Debug} Log('Запрошено удержание ссылки "' + name + '" в течение ' + Utils.TimeToString(due * 1e-3), logDebug); {$endif}
			Pop; // obj TLs

			id := length(timedLinks);
			SetLength(timedLinks, id + 1);
			new(tl);
			tl^.ss := @self;
			tl^.id := id;
			tl^.expiredId := -1;
		{$ifdef Debug} tl^.name := name; tl^.renews := 0; {$endif}
			timedLinks[id] := tl;

			PushCopy(-2); lua.rawsetp(ls, -2, tl); // TLs[tl] = obj
			lua.pushlightuserdata(ls, tl); SetWithKey(-2, -3); // TLs[obj] = tl
			Pop(2);

			ThreadTimer.Open(tl^.timer, @_TimedLink, tl, due, 0, [ThreadTimer.TinyWork]);
		end;
	end;

	procedure ScriptState.SetupGuard(const name: string);
	begin
		_MaybeSetHook;
		SetLength(guards, length(guards) + 1);
		guards[High(guards)].name := name;
		ResetGuard;
	end;

	procedure ScriptState.ResetGuard;
	var
		g: ^Guard;
	begin
		g := @guards[High(guards)];
		g^.timeout := Ticks.FromSeconds(CodeTimeout);
		g^.setupTime := Ticks.Get;
	end;

	procedure ScriptState.RemoveGuard;
	begin
		SetLength(guards, length(guards) - 1);
		if length(guards) > 0 then
			guards[High(guards)].setupTime := Ticks.Get
		else
			_MaybeRemoveHook;
	end;

	procedure ScriptState.Bookkeep;
	var
		i, id: sint;
		tl: pTimedLink;
	begin
		expiredTLsLock.Enter;
		if length(expiredTLs) > 0 then
		begin
			GetTableI(lua.REGISTRYINDEX, Reg.TimedLinks); // TLs
			Assert(IsTable(-1));
			for i := 0 to High(expiredTLs) do
			begin
				tl := expiredTLs[i]; Assert(tl = expiredTLs[tl^.expiredId]);
				id := tl^.id;         Assert(tl = timedLinks[id]);
				lua.rawgetp(ls, -1, tl); PushNil; SetTable(-3); // TLs[obj] = nil
				PushNil; lua.rawsetp(ls, -2, tl); // TLs[tl] = nil

				timedLinks[id] := timedLinks[High(timedLinks)];
				timedLinks[id]^.id := id;
				SetLength(timedLinks, length(timedLinks) - 1);
				tl^.timer.Close;
			{$ifdef Debug} Log('Ссылка "' + tl^.name + '" больше не удерживается (обновлялась ' + lang_amount(tl^.renews, '{N} раз{/а/}') + ')', logDebug); {$endif}
				dispose(tl);
			end;
			Pop;
			expiredTLs := nil;
		end;
		expiredTLsLock.Leave;
	end;

	function ScriptState.Execute(const code, chunkname: string; env: sint; errmsg: pString): ExecuteResult;
	var
		lmsg: string;
	begin
		if env = 0 then
			PushTable
		else
			PushCopy(env);

		if Lua.LoadString(ls, Preprocess(code, _assertions), chunkname, @lmsg) then
		begin
			_CompleteEnv('', -2, -1, nil);
			SetupGuard(chunkname);
			try
				if _pcall(ls, 0, 0, errmsg, no) = 0 then
					result := exec_Ok
				else
					result := exec_RunError;
			finally
				RemoveGuard;
			end;
		end else
			if IsSuffix('<eof>', lmsg) then
			begin
				result := exec_Incomplete;
				if Assigned(errmsg) then errmsg^ := lmsg;
			end else
			begin
				result := exec_CompilationError;
				if Assigned(errmsg) then errmsg^ := lmsg;
			end;
		Pop;
	end;

	function ScriptState.GC: ScriptGC;
	begin
		result.ls := ls;
	end;

	function ScriptState.MemoryEaten: size_t;
	begin
		result := size_t(lua.gc(ls, lua.GCCOUNT, 0)) * 1024 + size_t(lua.gc(ls, lua.GCCOUNTB, 0));
	end;

	procedure ScriptState._CompleteEnv(const stream: string; env, ep: sint; index: pEnvIndex);
	var
		ups: uint;
	begin
		Assert(IsTable(env));
		Assert(IsFunction(ep));

		lua.newtable(ls);
			PushString(StreamPath.Path(stream)); SetTableI(-2, EnvMt_StreamBase);
			ups := 0;
			if Assigned(index) and Assigned(index^.index) then
			begin
				lua.pushlightuserdata(ls, index^.index); inc(ups);
				if Assigned(index^.param) then begin lua.pushlightuserdata(ls, index^.param); inc(ups); end;
			end;
			_registerIn(ls, -1 - ups, LuaID_Index, @IndexLuaENV, ups);
			if Assigned(index) and Assigned(index^.newIndex) then
			begin
				lua.pushlightuserdata(ls, index^.newIndex); ups := 1;
				if Assigned(index^.param) then begin lua.pushlightuserdata(ls, index^.param); inc(ups); end;
				lua.pushcclosure(ls, @GenericLuaFunctionR0, ups);
				rawsetfield(ls, -2, LuaID_NewIndex);
			end;
		{$ifdef Debug} if stream <> '' then begin PushString(StreamPath.Filename(stream)); SetTableS(-2, 'filename'); end; {$endif}
		lua.setmetatable(ls, AdjustIdx(env, 1));

		lua.pushvalue(ls, env);
		lua.setupvalue(ls, AdjustIdx(ep, 1), 1);
	end;

	function ScriptState.LoadModule(const stream: string; nArg, nRet: sint; flags: LoadFlags; keepFor: uint = 0): sint;
	begin
		result := _GenericLoad(stream, nArg, nRet, [Share] + flags, keepFor);
	end;

	// TODO: переделать
	function ScriptState._GenericLoad(const stream: string; nArg, nRet: sint; flags: LoadFlags; keepFor: uint): sint;
	label _finally_;

		procedure MMenvCleanup; // (__M) m_env
		begin
			if Share in flags then
			begin
				lua.pushnil(ls); // __M m_env nil
				SetTableS(-3, stream); // __M[name] := nil; __M m_env
				lua.pushnil(ls);
				lua.rawset(ls, -3); // __M[m_env] = nil; __M
			end; // __M либо m_env
			Pop;
		end;

		function AdjustResults(curN, targetN: sint): sint;
		begin
			if (curN < 0) or (targetN < 0) then result := curN else
			begin
				if curN < targetN then PushNil(targetN - curN);
				if curN > targetN then Pop(curN - targetN);
				result := targetN;
			end;
		end;

	var
		realStream, errmsg: string;
		code: Strings;
		s: pStream;
		binary, errorOnStack: boolean;
		tryId, nTries, i: sint;
		sflags: FileFlags;
		userIndex: pEnvIndex;
	{$ifdef Debug}
		time: Ticks;
		what, epilogue: string;
		nExport: sint;
	{$endif}
	begin
		result := -1;
		errmsg := '';
		errorOnStack := no;

		if Share in flags then
		begin
			lua.rawgeti(ls, lua.REGISTRYINDEX, Reg.Modules); // ...args... __M
			if GetTableS(-1, stream) then // ...args... __M m_env
			begin
				Remove(-2);
				ExpectMeta(-1);

				if (nRet <> 0) and TryGetTableI(-1, EnvMt_Results) then // ...args... m_env meta results
				begin
					Remove(-2);
					result := RawLen(-1);
					if (nRet >= 0) and (nRet < result) then result := nRet;
					for i := 1 to result do
						GetTableI(-i, i); // ...args... m_env results ...results...
					Remove(-result - 1);
				end else
				begin
					Pop;
					result := 0;
				end;
				goto _finally_;
			end;
			Pop;
		end;

		s := nil;
		try
			tryId := 1;
			nTries := 2;
			repeat
				binary := _priorityToBinaries;
				realStream := stream + ExtensionSeparator + IfThen(binary, 'luac', 'lua');

				sflags := DefaultFileFlags;
				if tryId < nTries then sflags += [file_JustTry];
				s := GetStream(realStream, sflags);
				if not Assigned(s) then _priorityToBinaries := not _priorityToBinaries;
				inc(tryId);
			until Assigned(s);
		except
			Assert(not Assigned(s));
			errmsg := Exception.Message;
		end;

		if not Assigned(s) then
		begin
			errmsg += Script.StackTrace.WindowTitle + 'Ошибка загрузки скрипта';
			goto _finally_;
		end;
	{$ifdef Debug} LogR('Загрузка скрипта ' + StreamPath.Log(realStream) + '... '); {$endif}

		code := BinaryScript.Load(s, {$ifdef Debug} yes {$else} no {$endif});
		if not binary then
			for i := 0 to High(code) do
				code[i] := Preprocess(code[i], _assertions);

		if Share in flags then
			lua.rawgeti(ls, lua.REGISTRYINDEX, Reg.Modules); // ...args... __M
		if SupplyIndex in flags then
		begin
			lua.rotate(ls, -nArg - uint(Share in flags) - 1, -1);
			userIndex := lua.touserdata(ls, -1); {$ifdef Debug} Assert(userIndex^.magic = userIndex^.CorrectMagic, 'EnvIndex невалиден.'); {$endif}
			Pop;
			flags -= [SupplyIndex];
		end else
			userIndex := nil;
		if SupplyEnv in flags then
		begin
			lua.rotate(ls, -nArg - uint(Share in flags) - 1, -1);
			flags -= [SupplyEnv];
		end else
			PushTable; // ...args... (__M) m_env
		if Share in flags then
		begin
			PushString(stream);      // ...args... __M m_env name
			SetKeyValue(-3, -1, -2); // __M[name] = m_env
			SetKeyValue(-3, -2, -1); // __M[m_env] = name
			Pop;
		end; // ...args... (__M) m_env

	{$ifdef Debug} time := Ticks.Get; {$endif}
		if not Lua.LoadString(ls, code, stream, @errmsg) then
		begin
			errmsg += Script.StackTrace.WindowTitle + 'Ошибка ' + IfThen(binary, 'загрузки', 'компиляции') + ' скрипта';
			MMenvCleanup;
			goto _finally_;
		end;
		// ...args... __M m_env m_ep
	{$ifdef Debug} LogR('время {0}: {1}; ', IfThen(binary, 'загрузки', 'компиляции'), Utils.ToString(Ticks.Get - time)); {$endif}

	{$ifdef Debug} Log(StreamPath.Log(realStream) + ' загружен, выполняю...', logOK); {$endif}
		_CompleteEnv(stream, -2 {env}, -1 {ep}, userIndex);
	{$ifdef GuardLoading} SetupGuard(StreamPath.System(stream)); try {$endif}
		// ...args... (__M) m_env m_ep
		Insert(-2 - uint(Share in flags) - nArg, 2 + uint(Share in flags)); // (__M) m_env m_ep ...args...
		result := _pcall(ls, nArg, -1, nil, PushError in flags); // (__M) m_env ...results...
		nArg := 0;
	{$ifdef GuardLoading} finally RemoveGuard; end; {$endif}

		if result < 0 then // (__M) m_env (error)
		begin
			if PushError in flags then
			begin
				Insert(-2 - sint(Share in flags));
				errorOnStack := yes;
			end;
			MMenvCleanup;
			goto _finally_;
		end;

		if Share in flags then
		begin
			if result > 0 then
			begin
				ExpectMeta(-result - 1); // __M m_env ...results... meta
				PushTable(result, 0); // __M m_env ...results... meta R
				for i := 1 to result do
				begin
					PushCopy(-2 - result + (i - 1));
					SetTableI(-2, i);
				end;
				SetTableI(-2, EnvMt_Results); // __M m_env ...results... meta
				Pop;
			end;
			lua.remove(ls, -result - 2); // m_env ...results...
		end;

	{$ifdef Debug}
		epilogue := StreamPath.Log(realStream) + ' завершён, ';
		if result > 0 then epilogue += lang_amount(result, 'вернул {N} значени{е/я/й}, ');
		nExport := 0;
		lua.pushnil(ls);
		while lua.next(ls, -2 - result) <> 0 do
		begin
			if nExport = 0 then epilogue += 'экспортирует ' else epilogue += ', ';
			what := ToString(-2);
			if Typ(-2) = script_String then what := '"' + what + '"';
			epilogue += what;
			Pop;
			inc(nExport);
		end;
		if result > 0 then
		begin
			if nExport = 0 then epilogue += 'экспортирует ' else epilogue += ' и ';
			epilogue += lang_amount(result, '{N} значени{е/я/й}');
		end;
		if nExport + result = 0 then epilogue += 'ничего не экспортирует';
		Log(epilogue, logDebug);
	{$endif}
	_finally_:
		result := AdjustResults(result, nRet);
		Remove(-max(result, 0) - nArg, nArg);
		if SupplyIndex in flags then Remove(-max(result, 0) - 1);
		if SupplyEnv in flags then Remove(-max(result, 0) - 1);

		if result >= 0 then
		begin
			if (Share in flags) and (keepFor > 0) then
			begin
				PushCopy(-result - 1);
				SetupTimedLink(keepFor {$ifdef Debug}, stream {$endif});
			end;
			if DontPushEnv in flags then
				Remove(-result - 1);
		end else
		begin
			if (PushError in flags) and not errorOnStack then PushString(errmsg);
			if not (PushError in flags) and (errmsg <> '') then ShowError(errmsg);
		end;
	end;

	procedure ScriptState.SetGlobal(const name: string);
	begin
		lua.rawgeti(ls, lua.REGISTRYINDEX, lua.RIDX_GLOBALS);
		lua.insert(ls, -2);
		SetTableS(-2, name);
		Pop;
	end;

	procedure ScriptState.SetGlobal(const name: string; o: pObject);      begin PushObject(o); SetGlobal(name); end;
	procedure ScriptState.SetGlobal(const name: string; const s: string); begin PushString(s); SetGlobal(name); end;
	procedure ScriptState.SetGlobal(const name: string; x: lua.Number);   begin PushFloat(x);  SetGlobal(name); end;

	procedure ScriptState.GetGlobal(const name: string);
	begin
		lua.rawgeti(ls, lua.REGISTRYINDEX, lua.RIDX_GLOBALS);
		rawgetfield(ls, -1, name);
		lua.remove(ls, -2);
	end;

	function ScriptState.Call(narg, nret: sint): sint;
	var
		oldTop: sint;
	begin
		if nret < 0 then nret := lua.MULTRET;
	{$ifdef Debug}
		if not lua.isfunction(ls, -narg - 1) then
			Throw('ScriptState.Call требует функцию, получено: ' + ToString(-narg - 1));
	{$endif}

	{$ifdef Debug} if _insideCall > 0 then {$endif}
		begin
			if nret < 0 then oldTop := lua.gettop(ls);
			lua.call(ls, narg, nret);
			if nret < 0 then result := lua.gettop(ls) - (oldTop - narg - 1) else result := nret;
		end
	{$ifdef Debug}
		else
		begin
			inc(_insideCall);
			result := _pcall(ls, narg, nret, nil, no);
			dec(_insideCall);
		end
	{$endif};
	end;
	function ScriptState.Call(nArg: sint): sint; begin result := Call(nArg, -1); end;

	function ScriptState._GetValuesTable(obj: pObject; force: boolean): boolean;
	begin
		if not PushExisting(obj) then
		begin
		{$ifdef Debug} if force then Throw('GetValuesTable: нет ссылок на этот объект'); {$endif}
			exit(no);
		end;
		lua.getuservalue(ls, -1);
		if lua.&type(ls, -1) = lua.TTABLE then
		begin
			lua.remove(ls, -2);
			result := yes;
		end else
		begin
			result := force;
			if force then
			begin
				Pop;
				lua.newtable(ls);
				lua.pushvalue(ls, -1);
				lua.setuservalue(ls, -3);
				lua.remove(ls, -2);
			end else
				Pop(2);
		end;
	end;

	function CollectNativeFiber(ls: lua.State): sint; cdecl;
	var
		fiber: ScriptState.pNativeFiber;
	begin
		fiber := ss_by_ls^.ToData(1);
	{$ifdef DebugFibers} Log(Format('Сопрограмма {0} попалась GC', fiber^.Human), logDebug); {$endif}
		fiber^.Done;
		result := 0;
	end;

	function CollectPath(ls: lua.State): sint; cdecl; forward;
	function IndexConcatPath(ls: lua.State): sint; cdecl; forward;
	function CallPath(ls: lua.State): sint; cdecl; forward;

	procedure ScriptState.PushLazy(what: LazyEnum);
	begin
		GetTableI(lua.REGISTRYINDEX, Reg.Lazy);
		if TryGetTableI(-1, ord(what)) then
		begin
			Remove(-2);
			exit;
		end;

		case what of
			lazy_NativeFiberMeta:
				begin
					PushTable(0, 1);
					_registerIn(ls, -1, LuaID_GC, @CollectNativeFiber);
				end;
			lazy_PathMeta:
				begin
					PushTable(1, 4);
					_registerIn(ls, -1, LuaID_GC, @CollectPath);
					_registerIn(ls, -1, LuaID_Index, @IndexConcatPath);
					_registerIn(ls, -1, LuaID_Concat, @IndexConcatPath);
					_registerIn(ls, -1, LuaID_Call, @CallPath);
					lua.pushlightuserdata(ls, ObjType_Path); SetTableI(-2, Reg.ApiMt_TypeOf);
				end;
			lazy_LocalizedMeta:
				begin
					PushTable(0, 1);
					_registerIn(ls, -1, LuaID_Index, @IndexLocalized);
				end;
			else Assert(no);
		end;
		PushCopy(-1); // lazy value value
		SetTableI(-3, ord(what));
		Remove(-2);
	end;

	function ScriptState.PushPath(newBase: pScriptPath; newInPlace: boolean): pScriptPath;
	begin
		result := lua.newuserdata(ls, sizeof((@result)^^));
		PushLazy(lazy_PathMeta); lua.setmetatable(ls, -2);
		result^.Init(newBase, newInPlace);
	end;

	constructor ScriptDelegate.Init(newSS: pScriptState; newObj: pObject; newMd: pMultiDelegate; const newName: PoolString);
	begin
		inherited Init;
		ss := newSS;
		obj := newObj;
		md := newMd;
		name := newName;
		if ss^._GetObjectValueI(obj, ObjValue_Delegates) then
		begin
			uid := ss^.ConsecutiveKeys(-1) + 1;
			ss^.Pop;
		end else
			uid := 1;
	{$ifdef Debug} stat.Note(max_script_delegate_uid, uid); {$endif}
	end;

	destructor ScriptDelegate.Done;
	begin
		inherited Done;
	end;

	function ScriptDelegate.GetFunction {$ifdef Debug}(const dbgname: string){$endif}: boolean;
	begin
		result := ss^._GetObjectValueI(obj, ObjValue_Delegates);
		if not result then
		begin
		{$ifdef Debug}
			ss^.Throw('таблица делегатов для функции {0} не найдена. Возможно, на объект не осталось ссылок из скрипта.' + EOL + GetBackTrace, dbgname);
		{$else}
			Fatal('ScriptDelegate.GetFunction: что-то не так...');
		{$endif}
			exit;
		end;
		ss^.GetTableI(-1, uid);
		ss^.Remove(-2);
	{$ifdef Debug} if not ss^.IsFunction(-1) then ss^.Throw('{0} — не функция ({1})', dbgname, ss^.ToString(-1)); {$endif}
	end;

	procedure HandleScriptStateDestroying(obj: pObject; param: pointer); forward;

	procedure HandleObjectDestroying(obj: pObject; param: pointer);
	var
		info: pScriptDelegate absolute param;
	begin
		Assert(info^.obj = obj);
		info^.obj := nil;
		with info^.ss^ do
		begin
			RemoveOnDestroyProc(@HandleScriptStateDestroying, param);
			if _GetObjectValueI(obj, ObjValue_Delegates) then
			begin
			{$ifdef Debug} Log('ЗДЕСЬ ОКАЗАТЬСЯ НЕ ОЧЕНЬ-ТО И ВЕРОЯТНО. БУДЬ ОСТОРОЖЕН', logWarning); {$endif}
				PushNil;
				SetTableI(-2, info^.uid);
				Pop;
			end;
		end;
		info^.ss := nil;
	end;

	procedure HandleScriptStateDestroying(obj: pObject; param: pointer);
	var
		info: pScriptDelegate absolute param;
		ss: pScriptState absolute obj;
	begin
		Assert(info^.ss = ss);
		info^.ss := nil;
		info^.obj^.RemoveOnDestroyProc(@HandleObjectDestroying, param);
		info^.obj := nil;
		info^.md^.RemoveNamed(info^.name);
	end;

	procedure DestroySingleDelegate(const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		if Assigned(sd^.obj) then sd^.obj^.RemoveOnDestroyProc(@HandleObjectDestroying, sd);
		if Assigned(sd^.ss) then
			with sd^.ss^ do
			begin
				RemoveOnDestroyProc(@HandleScriptStateDestroying, sd);
				if _GetObjectValueI(sd^.obj, ObjValue_Delegates) then
				begin
					PushNil;
					SetTableI(-2, sd^.uid);
					Pop;
				end;
			end;
		dispose(sd, Done);
	end;

	procedure ScriptState.SetDelegate(obj: pObject; md: pMultiDelegate; func: pointer; const newName: string);
	var
		info: pSingleDelegateInfo;
		sd: pScriptDelegate;
		ok: boolean;
	begin
		Assert(Assigned(obj));
	{$ifdef Debug}
		if not (Typ(-1) in [script_Nil, script_Boolean, script_Function]) then
			Throw('Ожидается функция или NIL, получено: ' + ToString(-1));
	{$endif}

		if IsFunction(-1) then
		begin
			info := md^.FindNamed(newName);
			if Assigned(info) then
				sd := pScriptDelegate(info^.user)
			else
			begin
				sd := new(pScriptDelegate, Init(@self, obj, md, newName));
				md^.SetNamed(newName, func, sd, @DestroySingleDelegate);
				obj^.AddOnDestroyProc(@HandleObjectDestroying, sd);
				self.AddOnDestroyProc(@HandleScriptStateDestroying, sd);
			end;
			ok := _GetObjectValueI(obj, ObjValue_Delegates);
			if not ok then
			begin
				PushTable;
				_SetObjectValueI(obj, ObjValue_Delegates);
				ok := _GetObjectValueI(obj, ObjValue_Delegates);
			end;
			if ok then
			begin
				Insert(-2);
				SetTableI(-2, sd^.uid);
			end;
			Pop; // если ok — таблицу делегатов, иначе функцию-аргумент SetDelegate
		end else
		begin
			if Assigned(md^.FindNamed(newName)) then
				md^.RemoveNamed(newName);
			Pop;
		end;
	end;

	function ScriptState.GetDelegate(md: pMultiDelegate; const name: string): boolean;
	var
		info: pSingleDelegateInfo;
	begin
		info := md^.FindNamed(name);
		result := Assigned(info) and pScriptDelegate(info^.user)^.GetFunction {$ifdef Debug} ('(GetDelegate "' + name + '")') {$endif};
	end;

{$define impl:=
	function ScriptState._GET_(obj: pObject; valueId: _T_): boolean;
	begin
		result := _GetValuesTable(obj, no);
		if not result then exit;
		_LUAGET_(ls, -1, _LUAID_);
		Remove(-2);
		result := NonNil(-1);
		if not result then Pop;
	end;

	procedure ScriptState._SET_(obj: pObject; valueId: _T_);
	begin
		if not _GetValuesTable(obj, no) then
			if (lua.&type(ls, -1) = lua.TNIL) or (not _GetValuesTable(obj, yes)) then
			begin
				Pop;
				exit;
			end;
		lua.insert(ls, -2);
		_LUASET_(ls, -2, _LUAID_);
		Pop;
	end;

	procedure ScriptState._REMOVE_(obj: pObject; valueId: _T_);
	begin
		if not _GetValuesTable(obj, no) then exit;
		lua.pushnil(ls);
		_LUASET_(ls, -2, _LUAID_);
		Pop;
	end; {$undef _T_} {$undef _GET_} {$undef _SET_} {$undef _REMOVE_} {$undef _LUAGET_} {$undef _LUASET_} {$undef _LUAID_}}

{$define _T_:=sint} {$define _GET_:=_GetObjectValueI} {$define _SET_:=_SetObjectValueI} {$define _REMOVE_:=_RemoveObjectValueI}
{$define _LUAGET_:=lua.rawgeti} {$define _LUASET_:=lua.rawseti} {$define _LUAID_:=valueId}
	impl
{$undef impl}

	function id2ass(id: sint): sint;
	begin
		case id of
			0: result := ObjValue_Assoc;
			1: result := ObjValue_Assoc2;
			else Assert(no);
		end;
	end;

	procedure ScriptState.Associate(obj: pObject; id: sint = 0);
	begin
		_SetObjectValueI(obj, id2ass(id));
	end;

	function ScriptState.GetAssociated(obj: pObject; id: sint = 0): boolean;
	begin
		result := _GetObjectValueI(obj, id2ass(id));
		if result then
		begin
			result := not lua.isnil(ls, -1);
			if not result then Pop;
		end;
	end;

	function ScriptState.HasUV(obj: pObject): boolean;
	begin
		result := PushExisting(obj);
		if result then
		begin
			lua.getuservalue(ls, -1);
			result := not lua.isnil(ls, -1);
			Pop(2);
		end;
	end;

	function ScriptState.KillUV(obj: pObject): boolean;
	begin
		result := PushExisting(obj);
		if result then
		begin
			lua.getuservalue(ls, -1);
			result := not lua.isnil(ls, -1);
			if result then
			begin
				PushNil;
				lua.setuservalue(ls, -3);
				Remove(-2);
			end else
				Pop(2);
		end;
	end;

	procedure ScriptState.RestoreUV(obj: pObject);
	begin
		if PushExisting(obj) then
		begin
			Insert(-2);
			lua.setuservalue(ls, -2);
			Pop;
		end else
			Assert(no);
	end;

	procedure ScriptState.DontSerialize(obj: pObject);
	begin
		GetTableI(lua.REGISTRYINDEX, Reg.DontSerialize);
		if not PushExisting(obj) then Assert(no);
		PushBool(yes);
		SetTable(-3);
		Pop;
	end;

	function ScriptState.Serializable(idx: sint): boolean;
	begin
		if Typ(idx) = script_Pointer then
		begin
			GetTableI(lua.REGISTRYINDEX, Reg.DontSerialize);
			GetWithKey(-1, AdjustIdx(idx, 1));
			result := not ToBool(-1);
			Pop(2);
		end else
			result := yes;
	end;

	function ScriptState.Top: sint;               begin result := lua.gettop(ls); end;
	procedure ScriptState.SetTop(nt: sint);       begin lua.settop(ls, nt); end;
	function ScriptState.AbsIdx(idx: sint): sint; begin result := lua.absindex(ls, idx); end;

	function ScriptState.AdjustIdx(idx: sint; pushed: sint): sint;
	begin
		if (idx > lua.FIRSTPSEUDOIDX) and (idx < 0) then result := idx - pushed else result := idx;
	end;

	function ScriptState.Typ(idx: sint): ScriptType;
	const
		Straight: array[-1 .. 8] of ScriptType = ({lua.TNONE} script_Nil,
			{lua.TNIL} script_Nil, {lua.TBOOLEAN} script_Boolean, {lua.TLIGHTUSERDATA} script_Pointer, {lua.TNUMBER} script_Number,
			{lua.TSTRING} script_String, {lua.TTABLE} script_Table, {lua.TFUNCTION} script_Function,
			{lua.TUSERDATA} script_Pointer, {lua.TTHREAD} script_Nil);
	var
		lt: sint;
	begin
		lt := lua.&type(ls, idx);
		Assert((lt >= Low(Straight)) and (lt <= High(Straight)));
		result := Straight[lt];
	end;

	function ScriptState.InternalValueTypeName(idx: sint): string; begin result := lua.typename(ls, lua.&type(ls, idx)); end;
	function ScriptState.IsNil(idx: sint): boolean;      begin result := lua.&type(ls, idx) =  lua.TNIL; end;
	function ScriptState.NonNil(idx: sint): boolean;     begin result := lua.&type(ls, idx) <> lua.TNIL; end;
	function ScriptState.IsNumber(idx: sint): boolean;   begin result := lua.&type(ls, idx) =  lua.TNUMBER; end;
	function ScriptState.IsString(idx: sint): boolean;   begin result := lua.&type(ls, idx) =  lua.TSTRING; end;
	function ScriptState.IsTable(idx: sint): boolean;    begin result := lua.&type(ls, idx) =  lua.TTABLE; end;
	function ScriptState.IsFunction(idx: sint): boolean; begin result := lua.&type(ls, idx) =  lua.TFUNCTION; end;
	procedure ScriptState.PushNil;                          begin lua.pushnil(ls); end;
	procedure ScriptState.PushNil(n: uint);                 begin lua.settop(ls, uint(lua.gettop(ls)) + n); end;
	procedure ScriptState.PushFloat(const x: lua.Number);   begin lua.pushnumber(ls, x); end;
	procedure ScriptState.PushSint(x: sint);                begin lua.pushinteger(ls, x); end;
	procedure ScriptState.PushBool(b: boolean);             begin lua.pushboolean(ls, b); end;
	procedure ScriptState.PushString(const s: string);      begin lua.pushlstring(ls, PChar(s), length(s)); end;
	procedure ScriptState.PushString(const s: StringView);  begin lua.pushlstring(ls, s.p, s.n); end;
	procedure ScriptState.PushStringOrNil(const s: string); begin if length(s) > 0 then PushString(s) else PushNil; end;
	procedure ScriptState.PushPChar(s: PChar);              begin lua.pushstring(ls, s); end;

	procedure ScriptState._PushData(const data; size: size_t; otype: pointer);
	var
		lud: pointer;
	begin
		lud := lua.newuserdata(ls, size);
		memcpy(@data, lud, size);
		if _getApiMt(ls, otype) then lua.setmetatable(ls, -2);
	end;

	procedure ScriptState.PushObject(obj: pObject);
	begin
		PushObject(obj, no);
	end;

	procedure ScriptState.PushObject(obj: pObject; isref: boolean);
	var
		t: pointer;
	begin
		if not Assigned(obj) then
		begin
			lua.pushnil(ls);
			exit;
		end;
		lua.rawgeti(ls, lua.REGISTRYINDEX, Reg.ObjUserdatas);
		lua.rawgetp(ls, -1, obj);
		if IsNil(-1) then
		begin
			lua.pop(ls);
			t := TypeOf(obj^);
			if not isref then obj^.NewRef;
			pPointer(lua.newuserdata(ls, sizeof(pointer)))^ := obj;
			if _getApiMt(ls, t) then lua.setmetatable(ls, -2);
			lua.pushvalue(ls, -1);
			lua.rawsetp(ls, -3, obj);
			lua.remove(ls, -2);
		end else
		begin
			if isref then Release(obj);
			lua.remove(ls, -2);
		end;
	end;

	function ScriptState.PushExisting(obj: pObject): boolean;
	begin
		lua.rawgeti(ls, lua.REGISTRYINDEX, Reg.ObjUserdatas);
		lua.rawgetp(ls, -1, obj);
		result := not lua.isnil(ls, -1);
		if result then
			lua.remove(ls, -2)
		else
			lua.pop(ls, 2);
	end;

	procedure ScriptState.PushTable;                   begin lua.newtable(ls); end;
	procedure ScriptState.PushTable(narr, nrec: sint); begin lua.createtable(ls, narr, nrec); end;
	procedure ScriptState.PushCopy(idx: sint);         begin lua.pushvalue(ls, idx); end;
	procedure ScriptState.Pop;                         begin lua.settop(ls, -2); end;
	procedure ScriptState.Pop(n: uint);                begin lua.settop(ls, -1 - n); end;
	procedure ScriptState.Insert(at: sint);            begin lua.insert(ls, at); end;
	procedure ScriptState.Insert(at, n: sint);         begin lua.rotate(ls, at, n); end;
	procedure ScriptState.Remove(idx: sint);           begin lua.remove(ls, idx); end;
	procedure ScriptState.Remove(idx, n: sint);        begin lua.rotate(ls, idx, -n); lua.pop(ls, n); end;
	procedure ScriptState.Replace(idx: sint);          begin lua.replace(ls, idx); end;
	procedure ScriptState.SetCopy(fromidx, toidx: sint); begin lua.copy(ls, fromidx, toidx); end;

	function _FiberFunc(ls: lua.State): cint; cdecl;
	{$define RestoreLS := ss_by_ls^.ls := ls}
	var
		fiber: lua.State;
		narg, nres, r: sint;
		finished: boolean;
	begin
		fiber := lua.tothread(ls, UPVALUE-1);
		r := lua.status(fiber);
		finished := (r = lua.OK) and (lua.gettop(fiber) = 0);

		if ((r <> lua.OK) and (r <> lua.YIELDED)) or finished then
			ss_by_ls^.Throw('волокно завершилось (status = {0}, top(fiber) = {1})', Utils.ToString(r), Utils.ToString(lua.gettop(fiber)));

		narg := lua.gettop(ls);
		lua.xmove(ls, fiber, narg);

		ss_by_ls^.ls := fiber;
		r := lua.resume(fiber, ls, narg);

		case r of
			lua.OK, lua.YIELDED:
				begin
					nres := lua.gettop(fiber);
					lua.xmove(fiber, ls, nres);
					result := nres;
					RestoreLS;
				end;
			else
				begin
					WrapError(fiber)^.Add('[fiber.resume]');
					lua.xmove(fiber, ls, 1);
					RestoreLS;
					result := lua.error(ls);
				end;
		end;
	{$undef RestoreLS}
	end;

	procedure ScriptState.PushFiber(fnidx: sint);
	var
		fiber: lua.State;
	begin
		fiber := lua.newthread(ls);
	{$define ls := fiber} pss_by_ls^ := @self;
		PushCopy(AdjustIdx(fnidx, 1)); lua.xmove(ls, fiber, 1); // на стеке волокна — yield'ящаяся функция
		lua.pushcclosure(ls, @_FiberFunc, 1);                   // функция, каждый вызов которой возвращает результат очередного yield'а
	end;

	function ScriptState.IsFiber(idx: sint): boolean;
	begin
		if Assigned(lua.getupvalue(ls, idx, 1)) then
		begin
			result := lua.&type(ls, -1) = lua.TTHREAD;
			Pop;
		end else
			result := no;
	end;

	function _NativeFiberFunc(ls: lua.State): cint; cdecl;
	var
		fiber: ScriptState.pNativeFiber;
		finished: boolean;
	begin
		fiber := ss_by_ls^.ToData(UPVALUE-1);
		result := fiber^.Resume(finished);
		if finished then
		begin
			ss_by_ls^.PushNil; lua.setmetatable(ls, UPVALUE-1);
		{$ifdef DebugFibers} Log(Format('Сопрограмма {0} завершена, уничтожаю сразу.', fiber^.HumanOf), logDebug); {$endif}
			fiber^.Done;
		end;
	end;

	procedure ScriptState.PushFiber(const nameOf: string; body: NativeFiber.MainProc; param: pointer);
	var
		fiber: pNativeFiber;
	begin
		fiber := lua.newuserdata(ls, sizeof(NativeFiber));
		PushLazy(lazy_NativeFiberMeta); lua.setmetatable(ls, -2);
		fiber^.Init(nameOf, @self, body, param);
		lua.pushcclosure(ls, @_NativeFiberFunc, 1);
	end;

	procedure ScriptState.PushPtr(p: pointer);
	begin
		lua.pushlightuserdata(ls, p);
	end;

{$define vecf := start_vec_ids procedure ScriptState.pushvec(const v: vec); begin _PushData(v, sizeof(v), bindingtypeof); end; end_vec_ids} all_float_vectors
	procedure ScriptState.PushQuaternion(const q: Quaternion); begin _PushData(q, sizeof(q), ObjType_Quaternion); end;
	procedure ScriptState.PushTransform(const tf: Transform);  begin _PushData(tf, sizeof(tf), ObjType_Transform); end;

	function ScriptState.ToFloat(idx: sint): lua.Number;
{$ifdef Debug} var ok: cint; {$endif}
	begin
		result := lua.tonumberx(ls, idx, {$ifdef Debug} @ok {$else} nil {$endif});
	{$ifdef Debug} if ok = 0 then Throw('Неверный тип в #' + Utils.ToString(idx) + ', ожидается число, получено: ' + ToString(idx)); {$endif}
	end;

	function ScriptState.ToFloat(idx: sint; const def: lua.Number): lua.Number;
	var
		ok: cint;
	begin
		result := lua.tonumberx(ls, idx, @ok);
		if ok = 0 then result := def;
	end;

	function ScriptState.ToSint(idx: sint): sint;
{$ifdef Debug} var ok: cint; {$endif}
	begin
		result := lua.tointegerx(ls, idx, {$ifdef Debug} @ok {$else} nil {$endif});
	{$ifdef Debug} if ok = 0 then Throw('Неверный тип в #' + Utils.ToString(idx) + ', ожидается целое число, получено: ' + ToString(idx)); {$endif}
	end;

	function ScriptState.ToSint(idx: sint; def: sint): sint;
	var
		ok: cint;
	begin
		result := lua.tointegerx(ls, idx, @ok);
		if ok = 0 then result := def;
	end;

	function ScriptState.ToBool(idx: sint): boolean; begin result := lua.toboolean(ls, idx); end;

	function ScriptState.ToTribool(idx: sint): Tribool;
	begin
		if IsNumber(idx) then
			result := ToSint(idx)
		else
			result := ToBool(idx);
	end;

	function ScriptState.ToPChar(idx: sint): PChar;
	begin
	{$ifdef Debug}
		if lua.&type(ls, idx) <> lua.TSTRING then
			Throw('Неверный тип в #' + Utils.ToString(idx) + ', ожидается строка, получено: ' + ToString(idx) + EOL + GetBackTrace);
	{$endif}
		result := lua.topchar(ls, idx);
	end;

	function ScriptState.ToStringView(idx: sint): StringView; begin result := StringView.Make(lua.topchar(ls, idx), lua.rawlen(ls, idx)); end;

	function ScriptState.ToString(idx: sint): string;
		procedure Fallback(ptr: pointer);
		begin
			result := ScriptTypeNames[Typ(idx)];
			if Assigned(ptr) then result += ': ' + Utils.ToString(ptr);
		end;
	var
		ptr, ot: pointer;
		id: sint;
	begin
		case Typ(idx) of
			script_Nil: result := 'nil';
			script_Number: result := Utils.ToString(ToFloat(idx));
			script_String: result := lua.tostring(ls, idx);
			script_Boolean: result := YesNoEn[ToBool(idx)];
			script_Table:
				begin
					idx := AbsIdx(idx);
					lua.rawgeti(ls, lua.REGISTRYINDEX, Reg.Lazy);
					GetWithKey(-1, idx); Remove(-2);
					if NonNil(-1) then id := ToSint(-1) else id := -1;
					Pop;
					case id of
						ord(lazy_LocalizedMeta): result := GetFinalLocalized(idx);
						else Fallback(nil);
					end;
				end;
			else
				begin
					ptr := ToData(idx);
					ot := ObjType(idx);
					if ot = ObjType_Vec[2] then result := Utils.ToString(pVec2(ptr)^) else
					if ot = ObjType_Vec[3] then result := Utils.ToString(pVec3(ptr)^) else
					if ot = ObjType_Vec[4] then result := Utils.ToString(pVec4(ptr)^) else
					if ot = ObjType_Quaternion then result := 'Quat' + Utils.ToString(pQuaternion(ptr)^) else
					if ot = ObjType_Transform then result := Utils.ToString(pTransform(ptr)^) else
					if ot = ObjType_Path then result := GlobalFilenameStart + pScriptPath(ptr)^.Final else
						Fallback(ptr);
				end;
		end;
	end;

	function ScriptState.RawLen(idx: sint): sint; begin result := lua.rawlen(ls, idx); end;

	function ScriptState.ToData(idx: sint; ot: pointer): pointer;
	begin
	{$ifdef Debug}
		if (lua.&type(ls, idx) <> lua.TUSERDATA) or ((ObjType(idx) <> ot) and (ot <> ObjType_Any)) then
		begin
			Throw('Неверный тип в #' + Utils.ToString(idx) + ', ожидается некий POD, получено: ' + ToString(idx) + EOL + GetBackTrace);
			exit(nil);
		end;
	{$else}
		Assert(ot = ot);
	{$endif}
		result := lua.touserdata(ls, idx);
	end;

	function ScriptState.ToData(idx: sint): pointer;
	begin
		result := lua.touserdata(ls, idx);
	end;

	function ScriptState.ObjType(idx: sint): pointer;
	begin
		if lua.getmetatable(ls, idx) <> 0 then
		begin
			lua.rawgeti(ls, -1, Reg.ApiMt_TypeOf);
			result := lua.touserdata(ls, -1);
			Pop(2);
		end else
			result := nil;
	end;

	function ScriptState.IsPOD(idx: sint): boolean;
	begin
		if lua.getmetatable(ls, idx) <> 0 then
		begin
			rawgetfield(ls, -1, LuaID_GC);
			result := IsNil(-1);
			Pop(2);
		end else
			result := no;
	end;

	function ScriptState.ToObject(idx: sint; ot: pointer): pointer;
{$ifdef Debug} var objt: pointer; {$endif}
	begin
		if lua.&type(ls, idx) = lua.TUSERDATA then
		begin
		{$ifdef Debug}
			objt := ObjType(idx);
			if (objt <> ot) and (ot <> ObjType_Any) and (not InheritsFrom(objt, ot)) then
			begin
				Throw('Неверный тип объекта в #' + Utils.ToString(idx));
				exit(nil);
			end;
		{$else}
			Assert(ot = ot);
		{$endif}
			result := pPointer(lua.touserdata(ls, idx))^;

		{$ifdef Debug}
			GetTableI(lua.REGISTRYINDEX, Reg.ObjUserdatas);
			lua.rawgetp(ls, -1, result);
			if lua.&type(ls, -1) <> lua.TUSERDATA then Fatal(HumanStackTrace('Это НЕ объект!', MultiLine));
			Pop(2);
		{$endif}
		end else
		begin
		{$ifdef Debug}
			if lua.toboolean(ls, idx) then
				Throw('Неверный тип в #' + Utils.ToString(idx) + ', ожидается некий объект (' + Utils.ToString(ot) + ') или хотя бы nil, получено: ' + ToString(idx) + EOL + GetBackTrace);
		{$endif}
			result := nil;
		end;
	end;

	function ScriptState.ToSelf: pointer;
	begin
		result := ToObject(1, ObjType_Any);
	{$ifdef Debug} if not Assigned(result) then Throw('Ожидается объект (вероятно, пропущено двоеточие)'); {$endif}
	end;

	function ScriptState.ToStream(const fn: string): string;
	const
		EnvMt = UPVALUE-2;
	begin
		if Prefixed(GlobalFilenameStart, fn) then
			result := Copy(fn, length(GlobalFilenameStart) + 1, length(fn) - length(GlobalFilenameStart))
		else
		begin
			Assert(IsTable(EnvMt), HumanStackTrace('Видимо, функции не указан флаг RequireEnv: ' + fn, MultiLine));
			result := StreamPath.Resolve(fn, GetStringField(EnvMt, EnvMt_StreamBase));
		end;
	end;

	function ScriptState.ToStream(idx: sint): string;
	begin
		if Typ(idx) = script_Pointer then
		begin
		{$ifdef Debug} if not IsStream(idx) then Throw('Ожидается путь.'); {$endif}
			result := pScriptPath(ToData(idx))^.Final;
		end else
			result := ToStream(ToString(idx));
	end;

	function ScriptState.IsStream(idx: sint): boolean;
	begin
		result := no;
		case Typ(idx) of
			script_String: result := yes;
			script_Pointer:
				if GetMeta(idx) then
				begin
					PushLazy(lazy_PathMeta);
					result := lua.rawequal(ls, -2, -1);
					Pop(2);
				end;
		end;
	end;

{$define vecf := start_vec_ids
	function ScriptState.isvec(idx: sint): boolean;
	begin
		case Typ(idx) of
			script_Number: result := yes;
			script_Table: result := RawLen(idx) = veclen;
			script_Object: result := ObjType(idx) = bindingtypeof;
		{$if veclen = 3} script_String: result := yes; {$endif}
			else result := no;
		end;
	end;

	function ScriptState.asvec(idx: sint): vec;
	{$ifdef Debug} procedure ThrowWrong; begin Throw('Ожидается ' + vectypename + ', получено: {0}.', self.ToString(idx)); end; {$endif}
	begin
	{$ifdef Debug} if not isvec(idx) then ThrowWrong; {$endif}
		case Typ(idx) of
			script_Number: result := vec.Make(ToFloat(idx));
			script_Table: result := vec.Make({$define one := GetFloatField(idx, 1 + itemid)} comma_separated);
			script_Object: result := vec(ToData(idx {$ifdef Debug}, bindingtypeof {$endif})^);
		{$if veclen = 3}
			script_String:
				case ToPChar(idx)[0] of
					'x': result := vec.PositiveX;
					'y': result := vec.PositiveY;
					'z': result := vec.PositiveZ;
					'X': result := vec.NegativeX;
					'Y': result := vec.NegativeY;
					'Z': result := vec.NegativeZ;
					'0': result := vec.Zero;
				{$ifdef Debug} else ThrowWrong; {$endif}
				end;
		{$endif}
		{$ifdef Debug} else ThrowWrong; {$endif}
		end;
	end; end_vec_ids}
	all_float_vectors

	function ScriptState.VecLen(idx: sint): sint;
	var
		ot: pointer;
	begin
		case Typ(idx) of
			script_Table:
				begin
					result := RawLen(idx);
					if (result < 2) or (result > 4) then result := -1;
				end;
			script_Number: result := -1;
			script_Object:
				begin
					ot := ObjType(idx);
					if ot = ObjType_Vec[3] then result := 3 else
					if ot = ObjType_Vec[2] then result := 2 else
					if ot = ObjType_Vec[4] then result := 4 else
						result := -1;
				end;
			else result := -1;
		end;
	end;

	function ScriptState.ToUintVec2(idx: sint): UintVec2; begin result := UintTrunc(ToVec2(idx)); end;

	function ScriptState.GetQuaternionFromTable(tidx, start: sint; out readed: sint): Quaternion;
	var
		t: ScriptType;
		angle, x: float;
		axis: Vec3;
	begin
		angle := GetFloatField(tidx, start);
		GetTableI(tidx, start + 1);
		t := Typ(-1);
		case t of
			script_Number: // { angle, x, y, z }
				begin        //          ^
					x := ToFloat(-1);
					Pop;
					result := Quaternion.Rotation(angle, Vec3.Make(x, GetFloatField(tidx, start + 2), GetFloatField(tidx, start + 3)));
					readed := 4;
				end;
			script_String: // { angle, 'axis' }
				begin        //            ^
					case ToPChar(-1)[0] of
						'x': axis := Vec3.PositiveX; 'y': axis := Vec3.PositiveY; 'z': axis := Vec3.PositiveZ;
						'X': axis := Vec3.NegativeX; 'Y': axis := Vec3.NegativeY; 'Z': axis := Vec3.NegativeZ;
						else Throw('Ожидается ось "x", "y" или "z"');
					end;
					Pop;
					result := Quaternion.Rotation(angle, axis);
					readed := 2;
				end;
			else     // { angle, axis }
				begin  //            ^
					axis := ToVec3(-1);
					Pop;
					result := Quaternion.Rotation(angle, axis);
					readed := 2;
				end;
		end;
	end;

	function ScriptState.IsQuaternion(idx: sint): boolean;
	begin
		case Typ(idx) of
			script_Table: result := yes;
			script_Object: result := ObjType(idx) = ObjType_Quaternion;
			script_String: result := yes;
			else result := no;
		end;
	end;

	function ScriptState.ToQuaternion(idx: sint): Quaternion;
	{$ifdef Debug} procedure ThrowWrong; begin Throw('Ожидается кватернион, получено: {0}.', self.ToString(idx)); end; {$endif}
	var
		i, len, d: sint;
		current: Quaternion;
		first: boolean;
	begin
	{$ifdef Debug} if not IsQuaternion(idx) then ThrowWrong; {$endif}
		case Typ(idx) of
			script_Table:
				begin
					len := RawLen(idx);
					i := 1;
					first := yes;
					while i <= len do
					begin
						current := GetQuaternionFromTable(idx, i, d);
						i += d;

						if first then
						begin
							result := current;
							first := no;
						end else
							result *= current;
					end;
				{$ifdef Debug} if first then ThrowWrong; {$endif}
				end;

			script_Object: result := pQuaternion(ToData(idx {$ifdef Debug}, ObjType_Quaternion {$endif}))^;
			script_String:
				case ToPChar(idx)[0] of
					'I': result := Quaternion.Identity;
					{$ifdef Debug} else ThrowWrong; {$endif}
				end;
		{$ifdef Debug} else ThrowWrong; {$endif}
		end;
	end;

	function ScriptState.IsTransform(idx: sint): boolean;
	var
		ot: pointer;
	begin
		case Typ(idx) of
			script_Table: result := yes;
			script_Object:
				begin
					ot := ObjType(idx);
					result := (ot = ObjType_Transform) or (ot = ObjType_Vec[3]) or (ot = ObjType_Quaternion);
				end
			else result := no;
		end;
	end;

	function ScriptState.ToTransform(idx: sint): Transform;
	{$ifdef Debug} procedure ThrowWrong; begin Throw('Ожидается Transform, получено: {0}.', self.ToString(idx)); end; {$endif}
	var
		t: ScriptType;
		first: boolean;
		current: Transform;
		i, len, d: sint;
		x: float;
		ot, o: pointer;
	begin
	{$ifdef Debug} if not IsTransform(idx) then ThrowWrong; {$endif}

		case Typ(idx) of
			script_Table:
				begin
					len := RawLen(idx);
					i := 1;
					first := yes;
					while i <= len do
					begin
						GetTableI(idx, i);
						t := Typ(-1);
						if t = script_Number then
						begin
							// translate { x, y, z }
							x := ToFloat(-1);
							Pop;
							current := Translate(x, GetFloatField(idx, i + 1), GetFloatField(idx, i + 2));
							i += 3;
						end else
							if (t = script_String) and (ToPChar(-1)[0] = 'r') then
							begin
								Assert(RawLen(-1) = 1, 'ожидается строка "r" от "r"otate');
								Pop;
								i += 1;
								current := Rotate(GetQuaternionFromTable(idx, i, d));
								i += d;
							end else
							begin
								if IsVec3(-1) then current := Translate(ToVec3(-1)) else current := Rotate(ToQuaternion(-1));
								Pop;
								i += 1;
							end;

						if first then
						begin
							result := current;
							first := no;
						end else
							result *= current;
					end;
				{$ifdef Debug} if first then ThrowWrong; {$endif}
				end;
			script_Object:
				begin
					ot := ObjType(idx);
					o := ToData(idx);
					if ot = ObjType_Transform then result := pTransform(o)^ else
						if ot = ObjType_Vec[3] then result := Translate(pVec3(o)^) else
						begin
							Assert(ot = ObjType_Quaternion);
							result := Rotate(pQuaternion(o)^);
						end;
				end;
		end;
	end;

	procedure ScriptState.GetTable(idx: sint); begin lua.rawget(ls, idx); end;

	procedure ScriptState.GetWithKey(tidx, kidx: sint);
	begin
		tidx := AbsIdx(tidx);
		PushCopy(kidx);
		GetTable(tidx);
	end;

	procedure  ScriptState.GetTableI(idx: sint; key: sint); begin lua.rawgeti(ls, idx, key); end;

	function ScriptState.TryGetTableI(idx: sint; key: sint): boolean;
	begin
		lua.rawgeti(ls, idx, key);
		result := not lua.isnil(ls, -1);
		if not result then Pop;
	end;

	function ScriptState.GetTableS(idx: sint; const key: string): boolean;
	begin
		rawgetfield(ls, idx, key);
		result := not lua.isnil(ls, -1);
		if not result then Pop;
	end;

	procedure ScriptState.ForceGetTableS(idx: sint; const key: string); begin rawgetfield(ls, idx, key); end;

{$define get_impl :=
	begin
		_LUAGET_(ls, idx, key);
		result := conv;
		Pop;
	end; {$undef conv}}

{$define getdef_impl :=
	begin
		if _TRYGET_(idx, key) then
		begin
			result := conv;
			Pop;
		end else
			result := def;
	end; {$undef conv} {$undef def}}

{$define field_impl:=
	function ScriptState.HasField(idx: sint; const key: key_type): boolean;                     {$define conv := lua.&type(ls, -1) <> lua.TNIL} get_impl
	function ScriptState.FieldType(idx: sint; const key: key_type): ScriptType;                 {$define conv := Typ(-1)} get_impl
	function ScriptState.GetFloatField(idx: sint; const key: key_type): lua.Number;             {$define conv := lua.tonumberx(ls, -1, nil)} get_impl
	function ScriptState.GetFloatField(idx: sint; const key: key_type; const def: lua.Number): lua.Number; {$define conv := ToFloat(-1)} getdef_impl

	procedure ScriptState.SetFloatField(idx: sint; const key: key_type; const value: lua.Number);
	begin
		idx := AbsIdx(idx);
		PushFloat(value);
		_LUASET_(ls, idx, key);
	end;

	function ScriptState.GetSintField(idx: sint; const key: key_type): sint;                   {$define conv := lua.tointegerx(ls, -1, nil)} get_impl
	function ScriptState.GetSintField(idx: sint; const key: key_type; def: sint): sint;        {$define conv := ToSint(-1)} getdef_impl
	function ScriptState.GetBoolField(idx: sint; const key: key_type): boolean;                {$define conv := lua.toboolean(ls, -1)} get_impl
	function ScriptState.GetBoolField(idx: sint; const key: key_type; def: boolean): boolean;  {$define conv := ToBool(-1)} getdef_impl
	function ScriptState.GetStringField(idx: sint; const key: key_type): string;               {$define conv := lua.tostring(ls, -1)} get_impl
	function ScriptState.GetStringField(idx: sint; const key: key_type; const def: string): string; {$define conv := ToString(-1)} getdef_impl
	function ScriptState.GetStreamField(idx: sint; const key: key_type): string;               {$define conv := ToStream(-1)} {$define def := ''} getdef_impl
{$define vecf := start_vec_ids
	function ScriptState.getvecfield(idx: sint; const key: key_type): vec; begin result := getvecfield(idx, key, vec.Zero); end;
	function ScriptState.getvecfield(idx: sint; const key: key_type; const def: vec): vec; {$define conv := asvec(-1)} getdef_impl
	end_vec_ids} all_float_vectors
	function ScriptState.GetQuaternionField(idx: sint; const key: key_type): Quaternion;       {$define conv := ToQuaternion(-1)} {$define def := Quaternion.Identity} getdef_impl
	function ScriptState.GetTransformField(idx: sint; const key: key_type): Transform;         {$define conv := ToTransform(-1)} {$define def := Transform.Identity} getdef_impl
	function ScriptState.GetObjectField(idx: sint; const key: key_type; ot: pointer): pointer; {$define conv := ToObject(-1, ot)} {$define def := nil} getdef_impl
	{$undef key_type} {$undef _TRYGET_} {$undef _LUAGET_} {$undef _LUASET_}}

	{$define key_type:=string} {$define _TRYGET_:=GetTableS} {$define _LUAGET_:=rawgetfield} {$define _LUASET_:=rawsetfield} field_impl
	{$define key_type:=sint} {$define _TRYGET_:=TryGetTableI} {$define _LUAGET_:=lua.rawgeti} {$define _LUASET_:=lua.rawseti} field_impl
{$undef field_impl}
{$undef get_impl} {$undef getdef_impl} {$undef remap2def_impl}

	procedure ScriptState.SetTable(idx: sint);
	begin
	{$ifdef Debug} Assert(lua.&type(ls, idx) = lua.TTABLE, HumanStackTrace('Ожилается таблица', OneLine)); {$endif}
		lua.rawset(ls, idx);
	end;

	procedure ScriptState.SetWithKey(tidx, kidx: sint);
	begin
		PushCopy(kidx);
		Insert(-2);
		SetTable(AdjustIdx(tidx, 1));
	end;

	procedure ScriptState.SetWithValue(tidx, vidx: sint);
	begin
		PushCopy(vidx);
		SetTable(AdjustIdx(tidx, 1));
	end;

	procedure ScriptState.SetKeyValue(tidx, kidx, vidx: sint);
	begin
		PushCopy(kidx);
		PushCopy(AdjustIdx(vidx, 1));
		SetTable(AdjustIdx(tidx, 2));
	end;

	procedure ScriptState.SetTableI(idx: sint; key: sint);
	begin
	{$ifdef Debug} Assert(lua.&type(ls, idx) = lua.TTABLE, HumanStackTrace('Ожидается таблица', OneLine)); {$endif}
		lua.rawseti(ls, idx, key);
	end;

	procedure ScriptState.SetTableS(idx: sint; const key: string);
	begin
	{$ifdef Debug} Assert(lua.&type(ls, idx) = lua.TTABLE, HumanStackTrace('Ожидается таблица', OneLine)); {$endif}
		rawsetfield(ls, idx, key);
	end;

	function ScriptState.ConsecutiveKeys(idx: sint): uint;
	begin
		result := 0;
		while HasField(idx, result + 1) do inc(result);
	end;

	function ScriptState.ConsecutiveSerializableKeys(idx: sint): uint;
	var
		nx: boolean;
	begin
		result := 0;
		nx := yes;
		while nx and TryGetTableI(idx, result + 1) do
		begin
			if Serializable(-1) then
				inc(result)
			else
			begin
			{$ifdef Debug} Log('Обнаружен несериализуемый элемент в таблице-последовательности.', logWarning); {$endif}
				nx := no;
			end;
			Pop;
		end;
	end;

	function ScriptState.Next(idx: sint): boolean;    begin result := lua.next(ls, idx) <> 0; end;
	function ScriptState.GetMeta(idx: sint): boolean; begin result := lua.getmetatable(ls, idx) <> 0; end;

	procedure ScriptState.ExpectMeta(idx: sint);
	begin
		if not GetMeta(idx) then
			Throw('Нет метатаблицы у #{0} {1}.', Utils.ToString(idx), ToString(idx));
	end;

	function ScriptState.EqualMetatables(a, b: sint): boolean;
	var
		ma, mb: boolean;
	begin
		b := AbsIdx(b);
		ma := GetMeta(a);
		mb := GetMeta(b);
		result := (not ma and not mb) or (ma and mb and lua.rawequal(ls, -2, -1));
		Pop(uint(ma) + uint(mb));
	end;

{$ifdef use_serialization}
// Пишу подробно, чтобы самому всё понять :)
//
// В общем виде алгоритм сериализации такой же, как у графа нативных объектов.
// При этом ВСЕ значения, кроме nil и boolean, записываются по ID. (number можно по значению, но индексов пока что выше крыши, да и sizeof(lua.Number) > 4).
// [СДЕЛАЛ для чего угодно, даже для ссылочных типов с единственной ссылкой].
//
// Отдельная проблема — замыкания.
// Они записываются не значениями, а индексами из отдельного пространства, т. к. могут быть общими для нескольких функций. (lua.upvalueid, lua.upvaluejoin)
// Внимание, индексация замыканий с 1.
// [СДЕЛАЛ]
//
// Типы lua.TTHREAD и lua.TLIGHTUSERDATA НЕ поддерживаются, а lua.TUSERDATA жёстко регламентирован.
// Лёгкие юзердаты предположительно применяются только в рантаймовых структурах (ну, C-функции ещё), а полные хранят либо объекты, либо POD-тип —
// в любом случае, они уже принадлежат не только скрипту, так что он ими не занимается.
//
// Что НЕЛЬЗЯ сериализовывать из упомянутого рантайма. (По-хорошему надо воткнуть и проверки на несериализуемость внутренних объектов, но мне лень).
// Reg.ApiMt — метатаблицы зарегистрированных типов. Все они должны быть зарегистрированы до сериализации.
// Reg.ObjUserdatas — массив указатель -> объект, восстанавливается^W дополняется после десериализации.
// _G — просто так.
// Теоретически, ссылки из скрипта могут быть только на _G и подтаблицы Reg.ApiMt. [РЕШЕНО]
//
// Что НЕ СТОИТ сериализовывать:
// Reg.Modules — таблица модулей... можно восстановить, в случае частичной загрузки это "исключит" повторную загрузку модулей.
// Стоп. Их СТОИТ сериализовывать. Но не как таблицы! А как имена файлов. Аналогично ресурсам-из-файлов. [СДЕЛАЛ ^_^]
//
// Что НУЖНО сериализовать:
// — Сишные функции и замыкания, которые могут быть, в общем-то, где угодно (local cos = math.cos).
//   [НЕВЕРНО] [НЕТ, ВЕРНО]
//   Предполагается, что такая функция всегда указывает на GenericLuaFunction, а её первым замыканием является указатель на исходную функцию.
//   [Сделал, как и для случая без замыканий / с замыканиями не на функцию]
//   [единственный частный случай — IndexLuaENV — ликвидирован]
//   [Сделал для любых функций (Reg.CFunctionPtr)]
//
// А теперь вопрос века:
// Имею ли я право рассчитывать на инвариантность обхода всяких Reg.ApiMt?!
// [Решено: ни Reg.ApiMt, ни её элементы не сериализуются, метатаблицы восстанавливаются исходя из типа]

const
	Signature = '~ Script state serializer v1.06 ~';
	UpvalueSig = '~Up!~';
	StartStructuresSig = 'Structs';
	EndStructuresSig = 'EndStructs';
	SE_NILBOOL      = 0;
	SE_NUMBER       = 1;
	SE_UINT         = 2;
	SE_STRING       = 3;
	SE_TABLE        = 4;
	SE_CFUNCTION    = 5;
	SE_LUAFUNCTION  = 6;
	SE_OBJECT       = 7;
	SE_VEC2         = 8;
	SE_VEC3         = 9;
	SE_VEC4         = 10;
	SE_QUATERNION   = 11;
	SE_TRANSFORM    = 12;
	SE_MODULE       = 13;
	SE_FIRST        = SE_NILBOOL;
	SE_LAST         = SE_MODULE;
	SE_SIG_PREFIX: array[boolean] of string = (EOL + '~', '~:');
	SE_SIG_SUFFIX: array[boolean] of string = ('~' + EOL, ':~');
	SE_SIG: array[SE_FIRST .. SE_LAST] of string =
	(
		'nil/tf', 'number', 'uint', 'string', 'table', 'C function', 'Lua function', 'native object',
		'vec2', 'vec3', 'vec4', 'quaternion', 'transform', 'module'
	);
	{$define max := High(SE_SIG)} {$define nbits := SE_FLAG} {$define mask := SE_TYPE_MASK} {$include bits_to_store.inc}

	SE_NILBOOL_TRUE_BIT                           = 1 shl (SE_FLAG + 0);
	SE_NILBOOL_FALSE_BIT                          = 1 shl (SE_FLAG + 1);
	SE_NUMBER_16_BIT                              = 1 shl (SE_FLAG + 0);
	SE_NUMBER_32_BIT                              = 1 shl (SE_FLAG + 1);
	SE_UINT_PREDEFINED_BIT                        = 1 shl (SE_FLAG + 0);
	SE_UINT_I_BIT                                 = 1 shl (SE_FLAG + 1);
	SE_UINT_II_BIT                                = 1 shl (SE_FLAG + 2);
	SE_TABLE_STRUCT_BIT                           = 1 shl (SE_FLAG + 0);
	SE_TABLE_META_BIT                             = 1 shl (SE_FLAG + 1);
	SE_TABLE_ARRAY_BIT                            = 1 shl (SE_FLAG + 2);
	SE_CFUNCTION_FIRST_UPVALUE_IS_A_CFUNCTION_BIT = 1 shl (SE_FLAG + 0);
	SE_CFUNCTION_ID_I                             = 1 shl (SE_FLAG + 1);
	SE_CFUNCTION_ID_II                            = 1 shl (SE_FLAG + 2);
	SE_OBJECT_HAS_UV_BIT                          = 1 shl (SE_FLAG + 0);
	SE_QUAT_IDENTITY_BIT                          = 1 shl (SE_FLAG + 0);

	EXTRA_STRUCT_BITS = 2;
	STRUCT_HAS_SEQUENCE_BIT = 1 shl 0;
	STRUCT_HAS_META_BIT     = 1 shl 1;

	// может быть вызвана с byvalueidx = 0 для «черновых» результатов
	function preferbyvalue(ls: lua.State; idx: sint; byvalueidx: sint): boolean;
	var
		checkbv: boolean;
	begin
		checkbv := no;
		case lua.&type(ls, idx) of
			lua.TNIL, lua.TBOOLEAN, lua.TNUMBER: result := yes;
			lua.TTABLE, lua.TFUNCTION, lua.TUSERDATA: checkbv := yes;
			lua.TSTRING:
				begin
					result := lua.rawlen(ls, idx) <= 8;
					checkbv := not result;
				end;
			else
				result := no;
		end;
		if checkbv then
			if byvalueidx = 0 then
				result := no
			else
			begin
				lua.pushvalue(ls, idx);
				lua.rawget(ls, byvalueidx);
				result := lua.toboolean(ls, -1);
				lua.pop(ls);
			end;
	end;

type
	ReservedFunctionDumps = object
	const
		Prefix = 0;
		Suffix = 1;
		Count  = 2;
	end;

type
{$define classname := StringList} {$define item_type := string} {$include vector.h.inc}
{$define classname := StringList} {$include vector.pp.inc}

{$ifdef DetectSerializableRecords}
type
	pStructurer = ^Structurer;
	Structurer = object
	type
		pStructDesc = ^StructDesc;
		StructDesc = object
			first: uint;
			anothers: array of uint;
			ngeneric, nconsec: uint;
			function Create(idx: sint; st: pStructurer; out viable: boolean): StructDesc; static;
			procedure AddAnother(uid: uint);
			procedure Push(st: pStructurer);

			function CalculateHash(st: pStructurer): Hash.Value;
			function Equals(const another: StructDesc; st: pStructurer): boolean;
			function HashOne(idx: sint; ss: pScriptState): Hash.Value; static;
			function GenericKeyValuePair(a, b: sint; ss: pScriptState): boolean;
			function CountsAsConsecutiveKey(idx: sint; ss: pScriptState): boolean;
		{$ifdef Debug}
			procedure Dump(st: pStructurer; var sb: StringBuilder);
			function Dump(st: pStructurer): string;
		{$endif}
		end;

		{$define classname := StructSet} {$define key_type := StructDesc} {$define user_param := pStructurer}
		{$define huge_keys} {$define dont_replace}
		{$include hash.h.inc}
	var
		ss: pScriptState;
		structs: StructSet;
		uid2table, tempIdx, {$ifdef Debug} tempIdx2, {$endif} sdescs, table2sdescid: uint;
		nextUid, confirmed: uint;
		procedure Init(newSS: pScriptState);
		procedure Done;
		procedure Add(idx: sint);
		procedure Flatten;
	{$ifdef Debug} function Dump: string; {$endif}
	const
		StructDescPtr = 0; // lightuserdata-указатель на исходную структуру в sdescs
		StructMeta    = -1;
	end;

	{$define classname := Structurer.StructSet} {$define inline_hash := _1.CalculateHash(param)} {$define inline_eq := _1.Equals(_2, param)}
	{$include hash.pp.inc}

	function Structurer.StructDesc.Create(idx: sint; st: pStructurer; out viable: boolean): StructDesc;
	const
		NonTrivialLimit = 4;
	var
		ss: pScriptState;
		nNonTrivial: uint;
	begin
		ss := st^.ss;
		result.first := 0;
		result.anothers := nil;
		result.ngeneric := 0;
		result.nconsec := ss^.ConsecutiveSerializableKeys(idx);
		nNonTrivial := 0;
		viable := yes;

		idx := ss^.AbsIdx(idx);
		ss^.PushNil;
		while viable and ss^.Next(idx) do
		begin
			if result.GenericKeyValuePair(-2, -1, ss) then
			begin
				inc(result.ngeneric);
				if ss^.Typ(-2) in [script_Pointer, script_Table, script_Function] then
				begin
					inc(nNonTrivial);
					viable := nNonTrivial <= NonTrivialLimit;
				end;
			end;
			ss^.Pop(1 + uint(not viable));
		end;
		viable := viable and (result.ngeneric > 0);
	end;

	procedure Structurer.StructDesc.AddAnother(uid: uint);
	begin
		SetLength(anothers, length(anothers) + 1);
		anothers[High(anothers)] := uid;
	end;

	procedure Structurer.StructDesc.Push(st: pStructurer);
	begin
		st^.ss^.GetTableI(st^.uid2table, first);
		Assert(st^.ss^.IsTable(-1));
	end;

	function Structurer.StructDesc.CalculateHash(st: pStructurer): Hash.Value;
	var
		ss: pScriptState;
	begin
		result := 0;
		ss := st^.ss;

		Push(st);
		ss^.PushNil;
		while ss^.Next(-2) do
		begin
			if GenericKeyValuePair(-2, -1, ss) then
				result := result xor HashOne(-2, ss);
			ss^.Pop;
		end;
		if ss^.GetMeta(-1) then
		begin
			result := Hash.Combine(result, HashOne(-1, ss));
			ss^.Pop;
		end;
		if nconsec > 0 then result := Hash.Combine(result, Hash.OfUint(nconsec));
		ss^.Pop;
	end;

	function Structurer.StructDesc.Equals(const another: StructDesc; st: pStructurer): boolean;
		function IsSerializableKeySubset(a, b: sint; ss: pScriptState): boolean;
		begin
			a := ss^.AbsIdx(a); b := ss^.AbsIdx(b);
			ss^.PushNil;          // a_it0
			while ss^.Next(a) do // a_it a_value
			begin
				if GenericKeyValuePair(-2, -1, ss) then
				begin
					ss^.GetWithKey(b, -2);   // a_it a_value b_value
					if ss^.IsNil(-1) or not ss^.Serializable(-1) then
					begin
						ss^.Pop(3);
						exit(no);
					end;
					ss^.Pop;
				end;
				ss^.Pop;
			end;
			result := yes;
		end;

		function SerializableKeySetsAndMetatablesAreEqual(const a, b: StructDesc; st: pStructurer): boolean;
		var
			ss: pScriptState;
		begin
			ss := st^.ss;
			a.Push(st);
			b.Push(st);
			result := IsSerializableKeySubset(-2, -1, ss) and IsSerializableKeySubset(-1, -2, ss) and ss^.EqualMetatables(-2, -1);
			ss^.Pop(2)
		end;
	begin
		self.Push(st);
		another.Push(st);
		result := (self.ngeneric = another.ngeneric) and (self.nconsec = another.nconsec) and SerializableKeySetsAndMetatablesAreEqual(self, another, st);
		st^.ss^.Pop(2);
	end;

	function Structurer.StructDesc.HashOne(idx: sint; ss: pScriptState): Hash.Value;
	begin
		case ss^.Typ(idx) of
			script_Boolean: result := Hash.OfUint(uint(High(uint) - uint(ss^.ToBool(idx))));
			script_Number: result := Hash.OfUint(uint(ss^.ToSint(idx)));
			script_String: result := Hash.OfString(ss^.ToStringView(idx));
			script_Pointer, script_Table, script_Function: result := Hash.OfPointer(lua.topointer(ss^.ls, idx));
			else Assert(no, ss^.InternalValueTypeName(idx));
		end;
	end;

	function Structurer.StructDesc.GenericKeyValuePair(a, b: sint; ss: pScriptState): boolean;
	begin
		result := ss^.Serializable(a) and ss^.Serializable(b) and not CountsAsConsecutiveKey(a, ss);
	end;

	function Structurer.StructDesc.CountsAsConsecutiveKey(idx: sint; ss: pScriptState): boolean;
	var
		x: lua.Number;
	begin
		result := ss^.IsNumber(idx);
		if result then
		begin
			x := ss^.ToFloat(idx);
			result := (x > 0) and (x = int(x)) and (int(x) <= nconsec);
		end;
	end;

{$ifdef Debug}
	procedure Structurer.StructDesc.Dump(st: pStructurer; var sb: StringBuilder);
	var
		hasSomething: boolean;
		ss: pScriptState;

		function SomethingNew: pString;
		const
			First: string = '[';
			Next: string = ', ';
		begin
			if hasSomething then result := @Next else
			begin
				result := @First;
				hasSomething := yes;
			end;
		end;

	begin
		ss := st^.ss;
		Push(st);
		ss^.PushNil;
		hasSomething := no;
		while ss^.Next(-2) do
		begin
			if not CountsAsConsecutiveKey(-2, ss) then
				sb.Append(SomethingNew^, ss^.ToString(-2));
			ss^.Pop;
		end;

		if ss^.GetMeta(-1) then
		begin
			sb.Append(SomethingNew^, 'meta = ', Utils.ToString(lua.topointer(ss^.ls, -1)));
			ss^.Pop;
		end;

		if nconsec > 0 then
		begin
			sb.Append(SomethingNew^, '1 .. ', Utils.ToString(nconsec));
		end;
		if hasSomething then sb.Append(']') else sb.Append('пустая структура');
		if length(anothers) > 0 then sb.Append(' (', lang_amount(uint(1 + length(anothers)), '{N} экземпляр{/а/ов}'), ')');
		ss^.Pop;
	end;

	function Structurer.StructDesc.Dump(st: pStructurer): string;
	var
		sb: StringBuilder;
	begin
		sb.Init;
		Dump(st, sb);
		result := sb.DestructiveToString;
	end;
{$endif}

	procedure Structurer.Init(newSS: pScriptState);
	begin
		ss := newSS;
		structs.Init(@self);
		ss^.PushTable; uid2table := ss^.AbsIdx(-1);
		ss^.PushNil;   tempIdx   := ss^.AbsIdx(-1);
	{$ifdef Debug} ss^.PushNil;   tempIdx2  := ss^.AbsIdx(-1); {$endif}
		nextUid := 1;
		confirmed := 0;
	end;

	procedure Structurer.Done;
	begin
		ss^.Pop(2 {$ifdef Debug} + 1 {$endif});
		structs.Done;
	end;

	procedure Structurer.Add(idx: sint);
	var
		tmpStruct: StructDesc;
		struct: pStructDesc;
		viable: boolean;
	begin
		Assert(uid2table <> 0);
		ss^.PushCopy(idx); ss^.SetTableI(uid2table, nextUid);
		tmpStruct := StructDesc.Create(idx, @self, viable);
		tmpStruct.first := nextUid;
		if not viable then exit;

		struct := structs.Add(tmpStruct);
		if struct^.first <> nextUid then
		begin
			struct^.AddAnother(tmpStruct.first);
			if length(struct^.anothers) = 1 then inc(confirmed);
		end;
		inc(nextUid)
	end;

	procedure Structurer.Flatten;
	var
		it: StructSet.Iterator;
		struct: pStructDesc;
		another, fid, sid: uint;
	begin
		Assert(uid2table <> 0);
		ss^.PushTable; sdescs := ss^.AbsIdx(-1);                            // sdescs
		ss^.PushTable; table2sdescid := ss^.AbsIdx(-1);                     // sdescs table2sdescid

		it := structs.GetIterator;
		sid := 1;
		while structs.Next(it) do
		begin
			struct := structs.GetKey(it);
			if length(struct^.anothers) = 0 then
				continue; // запоминание структур имеет смысл, только когда структура встретилась больше одного раза

			ss^.PushTable;                                                    // sdescs table2sdescid struct_desc
			lua.pushlightuserdata(ss^.ls, struct);
			ss^.SetTableI(-2, StructDescPtr); // struct_desc[StructDescPtr] = @struct
			fid := 1;

			ss^.GetTableI(uid2table, struct^.first); Assert(ss^.IsTable(-1)); // sdescs table2sdescid struct_desc reftable
			ss^.PushNil;
			while ss^.Next(-2) do
			begin
				if struct^.GenericKeyValuePair(-2, -1, ss) then                  // sdescs table2sdescid struct_desc reftable refkey _
				begin
					ss^.PushCopy(-2);                                              // sdescs table2sdescid struct_desc reftable refkey _ refkey
					ss^.SetTableI(-5, fid); // struct_desc[fieldIndex] = fieldName // sdescs table2sdescid struct_desc reftable refkey _
					inc(fid);
				end;
				ss^.Pop;
			end;                                                               // sdescs table2sdescid struct_desc reftable
			Assert(fid = struct^.ngeneric + 1);

			// struct_desc[StructMeta] = metatable:
			if ss^.GetMeta(-1) then                                            // sdescs table2sdescid struct_desc reftable refmeta
				ss^.SetTableI(-3, StructMeta);                                   // sdescs table2sdescid struct_desc reftable

			ss^.PushSint(sid);                                                 // sdescs table2sdescid struct_desc reftable struct_id
			ss^.SetTable(table2sdescid); // table2sdescid[first] = struct_id   // sdescs table2sdescid struct_desc

			for another in struct^.anothers do
			begin
				ss^.GetTableI(uid2table, another); Assert(ss^.IsTable(-1));      // sdescs table2sdescid struct_desc another_struct
				ss^.PushSint(sid);
				ss^.SetTable(table2sdescid); // table2sdescid[another] = struct_id // sdescs table2sdesc struct_desc
			end;
			ss^.SetTableI(sdescs, sid);                                        // sdescs table2sdescid
			inc(sid);
		end;
		Assert(sid = confirmed + 1);
	{$ifdef Debug} {$define _NEWIDX_ := tempIdx2} {$else} {$define _NEWIDX_ := uid2table} {$endif}
		ss^.Replace(_NEWIDX_);  table2sdescid := _NEWIDX_; _NEWIDX_ := 0;
		ss^.Replace(tempIdx);   sdescs        := tempIdx;  tempidx  := 0;
	{$undef _NEWIDX_}
	end;

{$ifdef Debug}
	function Structurer.Dump: string;
	var
		it: StructSet.Iterator;
		nopp: uint;
	begin
		if structs.count = 0 then exit('Структур не найдено.');
		it := structs.GetIterator;
		nopp := 0;
		while structs.Next(it) do
		begin
			if nopp > 0 then result += EOL;
			inc(nopp);
			result += Format('Структура #{0}: {1}', [nopp, structs.GetKey(it)^.Dump(@self)]);
		end;
	end;
{$endif}
{$endif DetectSerializableRecords}

	// TODO: рефакторнуть
	procedure ScriptState.Serialize(idx: sint; se: pSerializer);
	type
		PrepassState = record
			dump2id: sint;
		end;

	var
		stream: pStream;
		v2id, id2v, f2dumpid, byvalues: sint;
		v2id_next: PtrUint;
		lastSe: PtrUint;
		nextUp: PtrUint;
		up2id: Ptr2ID;
		fdumps: StringList;
	{$ifdef DetectSerializableRecords} structures: Structurer; {$endif}

		procedure init_state;
		begin
			up2id.Init;
			nextUp := 1;
			lua.checkstack(ls, 20);
			PushTable; v2id    := AbsIdx(-1);
			PushTable; id2v    := AbsIdx(-1);
			PushTable; f2dumpid := AbsIdx(-1);
			PushTable; byvalues := AbsIdx(-1);
		{$ifdef DetectSerializableRecords} structures.Init(@self); {$endif}
			v2id_next := 1;
			lastSe := 0;
			fdumps.Init(ReservedFunctionDumps.Count);
			fdumps.Grow(ReservedFunctionDumps.Count);
		end;

		procedure done_state;
		begin
		{$ifdef DetectSerializableRecords} structures.Done; {$endif}
			Pop(4);
			up2id.Done;
		end;

		procedure prepass(idx: sint; const state: PrepassState; pass: uint);
		var
			lty, i: sint;
			info: lua.Debug;
			fdump: string;
			fdumpid: PtrUint;
			dump2id: sint absolute state.dump2id;
			iscfunc: boolean;
		{$ifdef DetectSerializableRecords} struct: Structurer.pStructDesc; {$endif}
		begin
			if preferbyvalue(ls, idx, 0) then exit;
			lty := lua.&type(ls, idx);
			lua.checkstack(ls, 20);
			idx := AbsIdx(idx);
			GetWithKey(byvalues, idx);
			if IsNil(-1) then
			begin
				PushBool(yes);
				SetWithKey(byvalues, idx);
				case lty of
					lua.TSTRING: ;
					lua.TTABLE:
						begin
							lua.rawgeti(ls, lua.REGISTRYINDEX, Reg.Modules);
							GetWithKey(-1, idx);
							if IsNil(-1) then
							begin
							{$ifdef DetectSerializableRecords}
								if pass = 0 then structures.Add(idx);
								struct := nil;
								if pass = 1 then
								begin
									GetWithKey(structures.table2sdescid, idx);
									if NonNil(-1) then
									begin
										GetTable(structures.sdescs);
										GetTableI(-1, Structurer.StructDescPtr); struct := lua.touserdata(ls, -1); Pop; Assert(Assigned(struct));
										for i := 1 to struct^.ngeneric do
										begin
											GetTableI(-1, i); Assert(NonNil(-1));
											GetTable(idx);    Assert(NonNil(-1));
											prepass(-1, state, pass);
											Pop;
										end;
										for i := 1 to struct^.nconsec do
										begin
											GetTableI(idx, i);
											prepass(-1, state, pass);
											Pop;
										end;
									end;
									Pop;
								end;

								if not Assigned(struct) then
								begin
							{$endif}
									PushNil;
									while Next(idx) do
									begin
										if Serializable(-2) and Serializable(-1) then
										begin
											prepass(-2, state, pass);
											prepass(-1, state, pass);
										end;
										Pop;
									end;
									if lua.getmetatable(ls, idx) <> 0 then
									begin
										prepass(-1, state, pass);
										Pop;
									end;
							{$ifdef DetectSerializableRecords}
								end;
							{$endif}
							end;
							Pop(2);
						end;
					lua.TFUNCTION:
						begin
							if pass = 1 then iscfunc := lua.iscfunction(ls, idx);
							PushCopy(idx);
							info.nups := 0;
							lua.getinfo(ls, '>u', info);
							for i := 1 to info.nups do
							begin
								lua.getupvalue(ls, idx, i);
								if not ((i = 1) and (lua.&type(ls, -1) = lua.TLIGHTUSERDATA)) then
									prepass(-1, state, pass);
								Pop;
							end;

							if (pass = 1) and not iscfunc then
							begin
								fdump := Lua.Dump(ls, idx, yes);
								PushString(fdump);
								GetWithKey(dump2id, -1);
								if IsNil(-1) then
								begin
									Pop;
									fdumps.Push(fdump);
									fdumpid := fdumps.n - ReservedFunctionDumps.Count;
									lua.pushlightuserdata(ls, NULL + fdumpid);
									SetTable(dump2id);
								end else
								begin
									fdumpid := lua.touserdata(ls, -1) - NULL;
									Pop(2);
								end;
								lua.pushlightuserdata(ls, NULL + fdumpid);
								SetWithKey(f2dumpid, idx);
							end;
						end;
					lua.TUSERDATA:
						begin
							lua.getuservalue(ls, idx);
							prepass(-1, state, pass);
							Pop;
						end;
					else Assert(no, lua.typename(ls, lty));
				end;
			end else
				if (pass = 1) and lua.toboolean(ls, -1) then
				begin
					PushBool(no);
					SetWithKey(byvalues, idx);
				end else
				begin
					// ничего
				end;
			Pop;
		end;

	{$ifdef DetectSerializableRecords}
		procedure prepass_structs(const state: PrepassState; pass: uint);
		var
			istruct, ifield: uint;
			struct: Structurer.pStructDesc;
		begin
			for istruct := 1 to structures.confirmed do
			begin
				GetTableI(structures.sdescs, istruct);   // struct
				GetTableI(-1, Structurer.StructDescPtr);
				struct := lua.touserdata(ls, -1);
				Pop;                                     // struct

				for ifield := 1 to struct^.ngeneric do
				begin
					GetTableI(-1, ifield);
					prepass(-1, state, pass);
					Pop;
				end;
				if TryGetTableI(-1, Structurer.StructMeta) then
				begin
					prepass(-1, state, pass);
					Pop;
				end;
				Pop;
			end;
		end;
	{$endif}

		procedure prepass(idx: sint; pass: uint);
		var
			state: PrepassState;
		begin
			Assert(idx = AbsIdx(idx));
			if pass = 1 then
			begin
				PushTable; state.dump2id := AbsIdx(-1);
			{$ifdef DetectSerializableRecords} prepass_structs(state, pass); {$endif}
			end;
			prepass(idx, state, pass);
			if pass = 1 then Pop;
		end;

		function v2id_getindex(idx: sint): PtrUint;
		begin
			GetWithKey(v2id, idx);
			result := lua.touserdata(ls, -1) - NULL;
			Pop;

			if NULL + result = nil then
			begin
				result := v2id_next;
				lastSe := result;
				inc(v2id_next);
				idx := AbsIdx(idx);
				lua.pushlightuserdata(ls, pPointer(@result)^); SetWithKey(v2id, idx);
				PushCopy(idx); lua.rawseti(ls, id2v, result);
			end;
		end;

		procedure id2v_getvalue(index: PtrUint);
		begin
			lua.rawgeti(ls, id2v, index);
		end;

		procedure serialize_lv_body(idx: sint; byvalue: boolean); forward;

		procedure serialize_lv(idx: sint);
		begin
			if preferbyvalue(ls, idx, byvalues) then
				serialize_lv_body(idx, yes)
			else
				VarInt.Write(stream, v2id_getindex(idx) shl 1);
		end;

		procedure serialize_up(upid: pointer; vidx: sint);
		var
			id: uint;
			isnew: boolean;
		begin
			id := up2id.Find(upid);
			isnew := id = 0;
			if isnew then
			begin
				id := nextUp;
				inc(nextUp);
				up2id.Add(upid, id);
			end;
			VarInt.Write(stream, id);
			if isnew then
			begin
				if se^.Verbose then Serialize_conststring(stream, UpvalueSig);
				serialize_lv(vidx);
			end;
		end;

		procedure serialize_lv_body(idx: sint; byvalue: boolean);
		const
			NilBoolValueBit: array[boolean] of uint = (SE_NILBOOL_FALSE_BIT, SE_NILBOOL_TRUE_BIT);
		{$ifdef Debug} CLua: array[boolean] of string = ('C', 'Lua'); {$endif}
		var
			ltype: sint;
			i, ty, n, tary, itary {$ifdef DetectSerializableRecords}, structId {$endif}: uint;
			lnum: lua.Number;
			info: lua.Debug;
			module: string;
			fdumpid: PtrUint;
			ot: pointer;
			fn: lua.CFunction;
			fc: BuiltinFunction;
		{$ifdef DetectSerializableRecords} struct: Structurer.pStructDesc; {$endif}
		begin
			idx := AbsIdx(idx);
			ltype := lua.&type(ls, idx);
			case ltype of
				lua.TNIL: ty := SE_NILBOOL;
				lua.TBOOLEAN: ty := SE_NILBOOL or NilBoolValueBit[lua.toboolean(ls, idx)];
			{$ifdef Debug} lua.TLIGHTUSERDATA: SerializationError('не умею сериализовывать Light Userdata. Вообще в скрипте они появляться не должны.'); {$endif}
				lua.TNUMBER:
					begin
						lnum := ToFloat(idx);
						if (lnum > 0) and (int(lnum) = lnum) and (int(lnum) <= High(uint32)) then
						begin
							ty := SE_UINT;
							i := round(lnum);
							if i and (1 shl 0) <> 0 then ty := ty or SE_UINT_I_BIT;
							if i and (1 shl 1) <> 0 then ty := ty or SE_UINT_II_BIT;
							i := i shr 2;
							if i = 0 then ty := ty or SE_UINT_PREDEFINED_BIT;
						end else
						begin
							ty := SE_NUMBER;
							if Equals(float32(float16(lnum)), lnum, 4 * CloseToZeroEps) then ty := ty or SE_NUMBER_16_BIT else
								if float32(lnum) = lnum then ty := ty or SE_NUMBER_32_BIT;
						end;
					end;
				lua.TSTRING: ty := SE_STRING;
				lua.TTABLE:
					begin
						lua.rawgeti(ls, lua.REGISTRYINDEX, Reg.Modules);
						GetWithKey(-1, idx);
						if NonNil(-1) then
						begin
							ty := SE_MODULE;
							module := ToString(-1);
						end else
						begin
							ty := SE_TABLE;
						{$ifdef DetectSerializableRecords}
							GetWithKey(structures.table2sdescid, idx);
							if NonNil(-1) then
							begin
								ty := ty or SE_TABLE_STRUCT_BIT;
								structId := ToSint(-1);
							end else
						{$endif}
							begin
								if GetMeta(idx) then
								begin
									ty := ty or SE_TABLE_META_BIT;
									Pop;
								end;
							{$ifdef DetectSerializableArrays}
								tary := ConsecutiveSerializableKeys(idx);
								if tary > 2 then
									ty := ty or SE_TABLE_ARRAY_BIT
								else
									tary := 0;
							{$else}
								tary := 0;
							{$endif}
							end;
						{$ifdef DetectSerializableRecords} Pop; {$endif}
						end;
						Pop(2);
					end;
				lua.TFUNCTION:
					begin
						if lua.iscfunction(ls, idx) then
						begin
							ty := SE_CFUNCTION;
							lua.rawgeti(ls, lua.REGISTRYINDEX, Reg.CFunctionPtr);
							GetWithKey(-1, idx);
							if lua.&type(ls, -1) = lua.TNUMBER then
							begin
								i := lua.tointegerx(ls, -1, nil);
								Assert((i <= Ord(High(BuiltinFunction))) and (i <> ord(func_Custom)),
									'Если функция зарегистрирована в Reg.CFunctionPtr как число, то оно соответствует одной из встроенных (GenericLuaFunction, etc.)');
								fc := BuiltinFunction(i);
							end else
							begin
								if lua.&type(ls, -1) <> lua.TLIGHTUSERDATA then Throw('Функция не зарегистрирована');
								pointer(fn) := lua.touserdata(ls, -1);
								Assert(Assigned(fn));
								Assert((fn <> @GenericLuaFunction) and (fn <> @GenericLuaFunctionR0) and (fn <> @GenericLuaFunctionR1));
								fc := func_Custom;
							end;
							if ord(fc) and 1 <> 0 then ty := ty or SE_CFUNCTION_ID_I;
							if ord(fc) and (1 shl 1) <> 0 then ty := ty or SE_CFUNCTION_ID_II;
							Assert(uint(ty and SE_CFUNCTION_ID_II <> 0) shl 1 or uint(ty and SE_CFUNCTION_ID_I <> 0) = uint(ord(fc)), 'BuiltinFunction не поместился в биты SE_CFUNCTION_ID.');
							Pop(2);
						end else
						begin
							ty := SE_LUAFUNCTION;
							GetWithKey(f2dumpid, idx);
							Assert(lua.&type(ls, -1) = lua.TLIGHTUSERDATA, 'Эта функция не попадалась в prepass!');
							pPointer(@fdumpid)^ := lua.touserdata(ls, -1);
							Pop;
						end;
						PushCopy(idx);
						info.nups := 0;
						if lua.getinfo(ls, '>u', info) = 0 then
						begin
							{$ifdef Debug} Assert(no, 'не удалось запросить информацию о ' + CLua[ty = SE_LUAFUNCTION] + '-функции'); {$endif}
						end;
						if info.nups > 0 then
						begin
							lua.getupvalue(ls, idx, 1);
							if lua.&type(ls, -1) = lua.TLIGHTUSERDATA then
							begin
								Assert(ty and SE_TYPE_MASK = SE_CFUNCTION);
								ty := ty or SE_CFUNCTION_FIRST_UPVALUE_IS_A_CFUNCTION_BIT;
							end;
							Pop;
						end;
					end;
				lua.TUSERDATA:
					begin
						if IsPOD(idx) then
						begin
							ot := ObjType(idx);
							if ot = ObjType_Vec[2] then ty := SE_VEC2 else
							if ot = ObjType_Vec[3] then ty := SE_VEC3 else
							if ot = ObjType_Vec[4] then ty := SE_VEC4 else
							if ot = ObjType_Quaternion then
							begin
								ty := SE_QUATERNION;
								if SameRotation(ToQuaternion(idx), Quaternion.Identity) then ty := ty or SE_QUAT_IDENTITY_BIT;
							end else
							if ot = ObjType_Transform then ty := SE_TRANSFORM
						{$ifdef Debug} else SerializationError('неизвестный P.O.D.-тип!') {$endif};
						end else
						begin
							ty := SE_OBJECT;
							lua.getuservalue(ls, idx);
							if lua.istable(ls, -1) then ty := ty or SE_OBJECT_HAS_UV_BIT;
							Pop;
						end;
					end;

			{$ifdef Debug}
				lua.TTHREAD: SerializationError('не умею сериализовывать Lua Thread, просто не умею.');
				else SerializationError('странный Lua-тип (код ' + Utils.ToString(ltype) + ')');
			{$endif}
			end;
			if byvalue then VarInt.Write(stream, (ty shl 1) or 1) else Serialize_ui8(stream, ty);
			if se^.verbose then
				Serialize_conststring(stream, SE_SIG_PREFIX[byvalue] + SE_SIG[ty and SE_TYPE_MASK] + SE_SIG_SUFFIX[byvalue]);

			case ty and SE_TYPE_MASK of
				SE_NILBOOL: ;
				SE_NUMBER:
					if (ty and SE_NUMBER_32_BIT) <> 0 then Serialize_f32(stream, lnum) else
						if (ty and SE_NUMBER_16_BIT) <> 0 then Serialize_f16(stream, lnum) else
							Serialize_f64(stream, lnum);
				SE_UINT: if ty and SE_UINT_PREDEFINED_BIT = 0 then VarInt.Write(stream, i);
				SE_STRING: Serialize_string(stream, ToString(idx));
				SE_TABLE:
				{$ifdef DetectSerializableRecords}
					if ty and SE_TABLE_STRUCT_BIT <> 0 then
					begin
						VarInt.Write(stream, structId);
						GetTableI(structures.sdescs, structId); Assert(IsTable(-1));
						GetTableI(-1, Structurer.StructDescPtr); struct := lua.touserdata(ls, -1); Pop; Assert(Assigned(struct));
						for i := 1 to struct^.ngeneric do
						begin
							GetTableI(-1, i); Assert(NonNil(-1));
							GetTable(idx);    Assert(NonNil(-1));
							serialize_lv(-1);
							Pop;
						end;
						for i := 1 to struct^.nconsec do
						begin
							GetTableI(idx, i); Assert(NonNil(-1));
							serialize_lv(-1);
							Pop;
						end;
						Pop;
					end else
				{$endif}
					begin
						n := 0;
						PushNil;
						while Next(idx) do
						begin
							if Serializable(-2) and Serializable(-1) then inc(n);
							Pop;
						end;
						Assert(n >= tary);
						n -= tary;
						VarInt.Write(stream, n);
						if tary > 0 then VarInt.Write(stream, tary);

						for itary := 1 to tary do
						begin
							GetTableI(idx, itary);
							serialize_lv(-1);
							Pop;
						end;

						PushNil;
						while Next(idx) do
						begin
							if (tary > 0) and (Typ(-2) = script_Number) then
							begin
								lnum := ToFloat(-2);
								if (int(lnum) = lnum) and (lnum >= 1) and (lnum <= tary) then
								begin
									Pop;
									continue;
								end;
							end;

							if Serializable(-2) and Serializable(-1) then
							begin
								serialize_lv(-2);
								serialize_lv(-1);
							end;
							Pop;
						end;

						if GetMeta(idx) then
						begin
							serialize_lv(-1);
							Pop;
						end;
					end;
				SE_CFUNCTION, SE_LUAFUNCTION:
					begin
						Serialize_ui8(stream, info.nups);
						if ty and SE_TYPE_MASK = SE_LUAFUNCTION then
							VarInt.Write(stream, fdumpid)
						else
							if fc = func_Custom then
								se^.SeFunction(fn);
						for i := 1 to info.nups do
						begin
							if (i = 1) and (ty and SE_TYPE_MASK = SE_CFUNCTION) and ((ty and SE_CFUNCTION_FIRST_UPVALUE_IS_A_CFUNCTION_BIT) <> 0) then
							begin
								lua.getupvalue(ls, idx, 1);
								se^.SeFunction(lua.touserdata(ls, -1));
								Pop;
								continue;
							end;
							lua.getupvalue(ls, idx, i);
							serialize_up(lua.upvalueid(ls, idx, i), -1);
							Pop;
						end;
					end;
				SE_OBJECT:
					begin
						se^.SeObject(ToObject(idx, ObjType_Any));
						if (ty and SE_OBJECT_HAS_UV_BIT) <> 0 then
						begin
							lua.getuservalue(ls, idx);
							serialize_lv(-1);
							Pop;
						end;
						// метатаблица НЕ сериализуется. Восстанавливается из типа.
					end;
				SE_VEC2: Serialize_vec2f32(stream, ToVec2(idx));
				SE_VEC3: Serialize_vec3f32(stream, ToVec3(idx));
				SE_VEC4: Serialize_vec4f32(stream, ToVec4(idx));
				SE_QUATERNION: if ty = (ty and SE_TYPE_MASK) then Serialize_IQuat16(stream, ToQuaternion(idx));
				SE_TRANSFORM: Serialize_tf32r16(stream, ToTransform(idx));
				SE_MODULE:
					begin
						Assert(Prefixed(Paths.Data, module));
						Serialize_string(stream, CutPrefix(Paths.Data, module));
					end;
				else Assert(no, Utils.ToString(ty));
			end;
		end;

	{$ifdef DetectSerializableRecords}
		procedure serialize_(const structures: Structurer);
		var
			istruct, ifield, ngeneric_x, pass: uint;
			struct: Structurer.pStructDesc;
		begin
			for pass := 1 to 2 do
			begin
				if pass = 1 then VarInt.Write(stream, structures.confirmed);
				if se^.Verbose and (pass = 1) and (structures.confirmed > 0) then serialize_conststring(stream, EOL + StartStructuresSig + EOL);
				for istruct := 1 to structures.confirmed do
				begin
					GetTableI(structures.sdescs, istruct);   // struct
					GetTableI(-1, Structurer.StructDescPtr);
					struct := lua.touserdata(ls, -1);
					Pop;                                     // struct

					if pass = 1 then
					begin
						ngeneric_x := struct^.ngeneric shl EXTRA_STRUCT_BITS;
						if struct^.nconsec <> 0 then ngeneric_x := ngeneric_x or STRUCT_HAS_SEQUENCE_BIT;
						if TryGetTableI(-1, Structurer.StructMeta) then
						begin
							ngeneric_x := ngeneric_x or STRUCT_HAS_META_BIT;
							Pop;
						end;
						VarInt.Write(stream, ngeneric_x);
						if struct^.nconsec <> 0 then
							VarInt.Write(stream, struct^.nconsec);
					end;

					if pass = 2 then
					begin
						for ifield := 1 to struct^.ngeneric do
						begin
							GetTableI(-1, ifield);
							serialize_lv(-1);
							Pop;
						end;
						if TryGetTableI(-1, Structurer.StructMeta) then
						begin
							serialize_lv(-1);
							Pop;
						end;
					end;
					Pop;
				end;
				if se^.Verbose and (pass = 2) and (structures.confirmed > 0) then serialize_conststring(stream, EOL + EndStructuresSig + EOL);
			end;
		end;
	{$endif}

	var
		i: sint;
		z: pStream;
		fdprefix, fdsuffix: string;
	{$ifdef Debug} start: FilePos; sumsize, compsize: FileSize; {$endif}
		nextSe: PtrUint;
	begin
		if not Assigned(se) then exit;
		idx := AbsIdx(idx);
		stream := se^.stream;
		se^.DelayAfter;
		GC.Collect;

		init_state;
	{$ifdef DetectSerializableRecords}
		prepass(idx, 0);
		structures.Flatten;
		PushTable; Replace(byvalues);
	{$endif}
		prepass(idx, 1);

		Serialize_conststring(stream, EOL + Signature + EOL);

		fdprefix := CommonAffix(fdumps.items + ReservedFunctionDumps.Count, fdumps.n - ReservedFunctionDumps.Count, Prefix);
		fdsuffix := CommonAffix(fdumps.items + ReservedFunctionDumps.Count, fdumps.n - ReservedFunctionDumps.Count, Suffix);
		fdumps.items[ReservedFunctionDumps.Prefix] := fdprefix;
		fdumps.items[ReservedFunctionDumps.Suffix] := fdsuffix;
	{$ifdef Debug}
		if fdprefix <> '' then Log('Обнаружен общий префикс дампов функций длиной ' + Utils.ToString(length(fdprefix)), logDebug);
		if fdsuffix <> '' then Log('Обнаружен общий суффикс дампов функций длиной ' + Utils.ToString(length(fdsuffix)), logDebug);
	{$endif}
		if (fdprefix <> '') or (fdsuffix <> '') then
			for i := ReservedFunctionDumps.Count to fdumps.n - 1 do
				fdumps.items[i] := Copy(fdumps.items[i], 1 + length(fdprefix), length(fdumps.items[i]) - length(fdprefix) - length(fdsuffix));

		VarInt.Write(stream, fdumps.n - ReservedFunctionDumps.Count);
		if fdumps.n > ReservedFunctionDumps.Count then
		begin
			for i := 0 to fdumps.n - 1 do
				VarInt.Write(stream, length(fdumps.items[i]));
		{$ifdef Debug} start := stream^.position; sumsize := FileSize.Zero; {$endif}
			z := MakeRef(new(pZStream, Init(stream, ZStream.Method.QuickLZ)));
			for i := 0 to fdumps.n - 1 do
			begin
				z^.Write(fdumps.items[i]);
			{$ifdef Debug} sumsize += uint(length(fdumps.items[i])); {$endif}
			end;
			Release(z);
		end;

	{$ifdef Debug}
		compsize := stream^.Position - start;
		if sumsize > FileSize.Zero then
			if sumsize > compsize then
				Log('Дампы функций ({0} + {1}) сжаты с {2} до {3} ({4}%)',
						Utils.ToString(fdumps.n - ReservedFunctionDumps.Count), Utils.ToString(ReservedFunctionDumps.Count),
						Utils.ToString(sumsize), Utils.ToString(compsize), Utils.ToString(100.0 * (compsize / sumsize - 1.0)), logDebug)
			else
				Log('Дампы функций ({0} + {1}) не сжались',
					Utils.ToString(fdumps.n - ReservedFunctionDumps.Count), Utils.ToString(ReservedFunctionDumps.Count), Utils.ToString(sumsize), logWarning);
	{$endif}
		fdumps.Done;

	{$if defined(Debug) and defined(DetectSerializableRecords)}
		if structures.confirmed = 0 then Log('Структур не обнаружено.') else Log('Обнаруженные структуры:');
		for i := 1 to structures.confirmed do
		begin
			GetTableI(structures.sdescs, i);
			GetTableI(-1, 0);
			Log('#{0}: {1}', Utils.ToString(i), Structurer.pStructDesc(lua.touserdata(ls, -1))^.Dump(@structures));
			Pop(2);
		end;
	{$endif}

		nextSe := v2id_next;
	{$ifdef DetectSerializableRecords} serialize_(structures); {$else} VarInt.Write(stream, 0); {$endif}

		serialize_lv(idx);
		while nextSe <= lastSe do
		begin
			id2v_getvalue(nextSe);
			serialize_lv_body(-1, no);
			Pop;
			inc(nextSe);
		end;

		done_state;
		se^.ContinueAfter;
	end;

type
	LinkEnum = (ln_TableIntKey, ln_TablePair, ln_StructField, ln_StructMeta, ln_Meta, ln_UV, ln_Stack);
	Link = record
	case what: LinkEnum of
		ln_TableIntKey: (ik: record table, index, value: PtrUint; end);
		ln_TablePair: (kv: record table, key, value: PtrUint; end);
		ln_StructField: (sf: record table, struct, field, value: PtrUint; end);
		ln_StructMeta: (sm: record table, struct: PtrUint; end);
		ln_Meta, ln_UV: (ap: record target, app: PtrUint; end);
		ln_Stack: (st: record obj: PtrUint; slot: sint; end);
	end;
	{$define classname := LinkList} {$define item_type := Link} {$include vector.h.inc}
	{$define classname := LinkList} {$include vector.pp.inc}

	function ScriptState.Deserialize(de: pDeserializer): boolean;
	label _finally_;
	const
		BYVALUE_INDEX_BIT = PtrUint(1) shl (bitsizeof(PtrUint) - 1);
	type
		UpvalueFn = record
			fn: PtrUint;
			index: sint;
		end;
		UpvalueRec = record
			fns: array of UpvalueFn;
			value: PtrUint;
		end;
		StructInfo = record
			nseq: uint;
			fields: array of PtrUint;
			meta: PtrUint;
		end;
	var
		stream: pStream;
		id2o, values: sint;
		current, last, nextvalue: PtrUint;
		links: LinkList;
		objs: array of PtrUint;
		ups: array of UpvalueRec;
		fdumps: array of string;
		structures: array of StructInfo;

		procedure init_state;
		begin
			lua.checkstack(ls, 20);
			PushTable; id2o := AbsIdx(-1);
			PushTable; values := AbsIdx(-1);
			current := 0; last := 0;
			nextvalue := 1;
			links.Init;
		end;

		procedure done_state;
		begin
			links.Done;
			Pop(2);
		end;

		function deserialize_lv_body(id, tyhiddeninref: PtrUint): boolean; forward;

		function deserialize_lv: PtrUint;
		var
			cv, idx: PtrUint;
		begin
			idx := VarInt.Read(stream);
			if idx and 1 <> 0 then
			begin
				cv := nextvalue;
				result := cv or BYVALUE_INDEX_BIT;
				inc(nextvalue);
				if not deserialize_lv_body(result, idx) then exit(0);
				lua.rawsetp(ls, values, pPointer(@cv)^);
			end else
			begin
				result := idx shr 1;
				if last < result then last := result;
			end;
		end;

		procedure getobject(id: PtrUint);
		begin
			if id and BYVALUE_INDEX_BIT <> 0 then
			begin
				id := id xor BYVALUE_INDEX_BIT;
				lua.rawgetp(ls, values, pPointer(@id)^);
			end else
				lua.rawgetp(ls, id2o, pPointer(@id)^);
		end;

		function make_table_int_key_link(t, i, v: PtrUint): Link;
		begin
			result.what     := ln_TableIntKey;
			result.ik.table := t;
			result.ik.index := i;
			result.ik.value := v;
		end;

		procedure link_table_int_key(t, i, v: PtrUint);
		begin
			links.Grow(1)^ := make_table_int_key_link(t, i, v);
		end;

		function make_table_pair_link(t, k, v: PtrUint): Link;
		begin
			result.what     := ln_TablePair;
			result.kv.table := t;
			result.kv.key   := k;
			result.kv.value := v;
		end;

		procedure link_table_pair(t, k, v: PtrUint);
		begin
			links.Grow(1)^ := make_table_pair_link(t, k, v);
		end;

		function make_struct_field_link(t, s, f, v: PtrUint): Link;
		begin
			result.what := ln_StructField;
			result.sf.table  := t;
			result.sf.struct := s;
			result.sf.field  := f;
			result.sf.value  := v;
		end;

		procedure link_struct_field(t, s, f, v: PtrUint);
		begin
			links.Grow(1)^ := make_struct_field_link(t, s, f, v);
		end;

		function make_struct_meta_link(t, s: PtrUint): Link;
		begin
			result.what := ln_StructMeta;
			result.sm.table  := t;
			result.sm.struct := s;
		end;

		procedure link_struct_meta(t, s: PtrUint);
		begin
			links.Grow(1)^ := make_struct_meta_link(t, s);
		end;

		function make_meta_link(aTarget, meta: PtrUint): Link;
		begin
			result.what      := ln_Meta;
			result.ap.target := aTarget;
			result.ap.app    := meta;
		end;

		procedure link_meta(aTarget, meta: PtrUint);
		begin
			links.Grow(1)^ := make_meta_link(aTarget, meta);
		end;

		function make_uv_link(aTarget, uv: PtrUint): Link;
		begin
			result.what      := ln_UV;
			result.ap.target := aTarget;
			result.ap.app    := uv;
		end;

		procedure link_uv(aTarget, uv: PtrUint);
		begin
			links.Grow(1)^ := make_uv_link(aTarget, uv);
		end;

		function make_stack_link(aObj: PtrUint; aSlot: sint): Link;
		begin
			result.what    := ln_Stack;
			result.st.obj  := aObj;
			result.st.slot := aSlot;
		end;

		procedure link_stack(aObj: PtrUint; aSlot: sint);
		begin
			links.Grow(1)^ := make_stack_link(aObj, aSlot);
		end;

		procedure deserialize_up(aFn: PtrUint; aIndex: sint);
		var
			uid: PtrUint;
			isnew: boolean;
		begin
			uid := VarInt.Read(stream);
			if uid = 0 then SerializationError {$ifdef Debug} ('невозможный (нулевой) индекс замыкания') {$endif};
			dec(uid);
			isnew := uid >= uint(length(ups));
			if isnew then
			begin
				if uid <> uint(length(ups)) then SerializationError {$ifdef Debug} ('невозможный индекс замыкания') {$endif};
				if de^.Verbose then Deserialize_signature(stream, UpvalueSig, no);
				SetLength(ups, uid + 1);
				ups[uid].value := deserialize_lv();
			end;
			with ups[uid] do
			begin
				SetLength(fns, length(fns) + 1);
				with fns[High(fns)] do
				begin
					fn := aFn;
					index := aIndex;
				end;
			end;
		end;

		function deserialize_lv_body(id, tyhiddeninref: PtrUint): boolean;
		const SeFirstIsZero = SE_FIRST = 0;
		type SeFirstIsZeroIfGp = array[0 .. sint(SeFirstIsZero)] of pointer;
		var
			byvalue: boolean;
			ty: uint;
			i, n, tary, key, structId: uint;
			f: lua.Number;
			errmsg: string;
			fn: lua.CFunction;
			fc: BuiltinFunction;
			o: ppObject;
		begin
			result := no;
			byvalue := tyhiddeninref <> 0;
			if byvalue then ty := tyhiddeninref shr 1 else ty := Deserialize_ui8(stream);
			if {$if sizeof(SeFirstIsZeroIfGp) <= sizeof(pointer)} (ty and SE_TYPE_MASK < SE_FIRST) or {$endif} (ty and SE_TYPE_MASK > SE_LAST) then SerializationError('неверный тип');
			if de^.verbose then Deserialize_signature(stream, SE_SIG_PREFIX[byvalue] + SE_SIG[ty and SE_TYPE_MASK] + SE_SIG_SUFFIX[byvalue], no);
			lua.checkstack(ls, 20);

			case ty and SE_TYPE_MASK of
				SE_NILBOOL:
					if ty and (SE_NILBOOL_TRUE_BIT or SE_NILBOOL_FALSE_BIT) = 0 then
						PushNil
					else
						PushBool(ty and SE_NILBOOL_TRUE_BIT <> 0);
				SE_NUMBER:
					begin
						if (ty and SE_NUMBER_32_BIT) <> 0 then f := Deserialize_f32(stream) else
							if (ty and SE_NUMBER_16_BIT) <> 0 then f := Deserialize_f16(stream) else
								f := Deserialize_f64(stream);
						PushFloat(f);
					end;
				SE_UINT:
					begin
						if (ty and SE_UINT_PREDEFINED_BIT) <> 0 then n := 0 else n := VarInt.Read(stream) shl 2;
						if (ty and SE_UINT_I_BIT) <> 0 then n := n or (1 shl 0);
						if (ty and SE_UINT_II_BIT) <> 0 then n := n or (1 shl 1);
						lua.pushinteger(ls, n);
					end;
				SE_STRING: PushString(Deserialize_string(stream));
				SE_TABLE:
					begin
						PushTable;
						if ty and SE_TABLE_STRUCT_BIT <> 0 then
						begin
							structId := VarInt.Read(stream);
							if (structId <= 0) or (structId > uint(length(structures))) then
								SerializationError {$ifdef Debug} ('неверный индекс структуры') {$endif};
							dec(structId);

							for i := 1 to length(structures[structId].fields) do
								link_struct_field(id, structId, i - 1, deserialize_lv());
							for i := 1 to structures[structId].nseq do
								link_table_int_key(id, i, deserialize_lv());
							if structures[structId].meta <> 0 then link_struct_meta(id, structId);
						end else
						begin
							n := VarInt.Read(stream);
							if ty and SE_TABLE_ARRAY_BIT <> 0 then tary := VarInt.Read(stream) else tary := 0;
							for i := 1 to tary do
								link_table_int_key(id, i, deserialize_lv());

							for i := 1 to n do
							begin
								key := deserialize_lv();
								link_table_pair(id, key, deserialize_lv());
							end;

							if (ty and SE_TABLE_META_BIT) <> 0 then
								link_meta(id, deserialize_lv());
						end;
					end;
				SE_CFUNCTION, SE_LUAFUNCTION:
					begin
						n := Deserialize_ui8(stream);
						case ty and SE_TYPE_MASK of
							SE_LUAFUNCTION:
								begin
									i := VarInt.Read(stream);
									if (i < 1) or (ReservedFunctionDumps.Count + i > uint(length(fdumps))) then SerializationError('неверный индекс дампа функции');
									if not Lua.LoadString(ls, [fdumps[ReservedFunctionDumps.Prefix], fdumps[ReservedFunctionDumps.Count + (i - 1)], fdumps[ReservedFunctionDumps.Suffix]], '', @errmsg) then
										raise USystem.Error('Не удалось прочитать Lua-функцию. ' + errmsg);
								end;
							SE_CFUNCTION:
								begin
									i := uint(ty and SE_CFUNCTION_ID_II <> 0) shl 1 or uint(ty and SE_CFUNCTION_ID_I <> 0);
									if i > ord(High(BuiltinFunction)) then SerializationError('Неверный индекс встроенной функции');
									fc := BuiltinFunction(i);
									fn := BuiltinFunctionPointer(fc);
									if not Assigned(fn) then fn := lua.CFunction(de^.DeFunction);
									if n <> 0 then
									begin
										lua.checkstack(ls, n);
										PushNil(n);
									end;
									lua.pushcclosure(ls, fn, n);

									// и запомнить её в Reg.CFunctionPtr
									lua.rawgeti(ls, lua.REGISTRYINDEX, Reg.CFunctionPtr);
									if fc = func_Custom then lua.pushlightuserdata(ls, fn) else lua.pushinteger(ls, ord(fc));
									SetWithKey(-2, -3);
									Pop;
								end;
						end;
						for i := 1 to n do
						begin
							if (i = 1) and (ty and SE_TYPE_MASK = SE_CFUNCTION) and ((ty and SE_CFUNCTION_FIRST_UPVALUE_IS_A_CFUNCTION_BIT) <> 0) then
							begin
								lua.pushlightuserdata(ls, de^.DeFunction());
								lua.setupvalue(ls, -2, i);
								continue;
							end;
							deserialize_up(id, i);
						end;
					end;
				SE_OBJECT:
					begin
						SetLength(objs, length(objs) + 1);
						objs[High(objs)] := id;
						o := lua.newuserdata(ls, sizeof(pointer));
						o^ := nil;
						de^.DeObjectA(o^);
						if Assigned(pointer(o^)) and ((ty and SE_OBJECT_HAS_UV_BIT) = 0) then
						begin
							lua.rawgeti(ls, lua.REGISTRYINDEX, Reg.ObjUserdatas); // obj uds
							lua.rawgetp(ls, -1, o^); // obj uds exobj?
							if NonNil(-1) then
							begin
							{$ifdef Debug} Log('Восстановлена старая ссылка: ' + SerializationDB.Shared^.TypeName(TypeOf(o^^))); {$endif}
								Release(o^);
								lua.insert(ls, -3);
							end;
							Pop(2);
						end;
						if (ty and SE_OBJECT_HAS_UV_BIT) <> 0 then link_uv(id, deserialize_lv()){ else
						begin
							lua.pushnil(ls);
							lua.setuservalue(ls, -2);
						end};
					end;
				SE_VEC2: PushVec2(Deserialize_vec2f32(stream));
				SE_VEC3: PushVec3(Deserialize_vec3f32(stream));
				SE_VEC4: PushVec4(Deserialize_vec4f32(stream));
				SE_QUATERNION:
					if (ty and SE_QUAT_IDENTITY_BIT) <> 0 then
						PushQuaternion(Quaternion.Identity)
					else
						PushQuaternion(Deserialize_IQuat16(stream));
				SE_TRANSFORM: PushTransform(Deserialize_tf32r16(stream));
				SE_MODULE: if LoadModule(Paths.Data + Deserialize_string(stream), 0, 0) < 0 then PushNil;
				else
					SerializationError {$ifdef Debug} ('неверный тип') {$endif};
			end;
			result := yes;
		end;

		procedure read_structures;
		var
			istruct, ifield, ngeneric_x: uint;
			struct: ^StructInfo;
		begin
			SetLength(structures, VarInt.Read(stream));
			if de^.Verbose and (length(structures) > 0) then Deserialize_signature(stream, EOL + StartStructuresSig + EOL, no);
			for istruct := 1 to length(structures) do
			begin
				struct := @structures[istruct - 1];
				ngeneric_x := VarInt.Read(stream);
				SetLength(struct^.fields, ngeneric_x shr EXTRA_STRUCT_BITS);
				if ngeneric_x and STRUCT_HAS_SEQUENCE_BIT <> 0 then struct^.nseq := VarInt.Read(stream) else struct^.nseq := 0;
				if ngeneric_x and STRUCT_HAS_META_BIT <> 0 then struct^.meta := not PtrUint(0) else struct^.meta := 0;
			end;

			for istruct := 1 to length(structures) do
			begin
				struct := @structures[istruct - 1];
				for ifield := 1 to length(struct^.fields) do
					struct^.fields[ifield - 1] := deserialize_lv();
				if 0 = not struct^.meta then struct^.meta := deserialize_lv();
			end;
			if de^.Verbose and (length(structures) > 0) then Deserialize_signature(stream, EOL + EndStructuresSig + EOL, no);
		end;

	{$ifdef Debug}
		function CountUpvalueInstances: uint;
		var
			i: sint;
		begin
			result := 0;
			for i := 0 to High(ups) do
				result += uint(length(ups[i].fns));
		end;
	{$endif}

	var
		i, j, retidx: uint;
		o: pObject;
		z: pStream;

	begin
		result := no;
		if not Assigned(de) then exit;
		stream := de^.stream;
		de^.DelayAfter;

		PushNil; retidx := AbsIdx(-1);
		init_state;
		Deserialize_signature(stream, EOL + Signature + EOL, no);

		i := VarInt.Read(stream);
		fdumps := nil;
		if i > 0 then
		begin
			SetLength(fdumps, ReservedFunctionDumps.Count + i);
			for i := 0 to High(fdumps) do
				SetLength(fdumps[i], VarInt.Read(stream));

			z := MakeRef(new(pZStream, Init(stream, [])));
			for i := 0 to High(fdumps) do
				z^.Read(pointer(fdumps[i]), length(fdumps[i]) * sizeof(fdumps[i, 1]));
			Release(z);
		end;

		read_structures();
		link_stack(deserialize_lv(), retidx);
		while current < last do
		begin
			inc(current);
			if not deserialize_lv_body(current, 0) then goto _finally_;
			lua.rawsetp(ls, id2o, pPointer(@current)^);
		end;

	{$ifdef Debug} if links.n > 0 then Log('Восстановление {0}...', lang_amount(links.n, '{N} ссыл{ки/ок/ок}')); {$endif}
		i := 0;
		while i < links.n do
		begin
			with links.items[i] do
				case what of
					ln_TableIntKey:
						begin
							getobject(ik.table);
							getobject(ik.value);
							lua.rawseti(ls, -2, ik.index);
							Pop;
						end;
					ln_TablePair:
						begin
							getobject(kv.table);
							getobject(kv.key);
							getobject(kv.value);
							lua.rawset(ls, -3);
							Pop;
						end;
					ln_StructField:
						begin
							getobject(sf.table);
							getobject(structures[sf.struct].fields[sf.field]);
							getobject(sf.value);
							lua.rawset(ls, -3);
							Pop;
						end;
					ln_StructMeta:
						begin
							getobject(sf.table);
							getobject(structures[sf.struct].meta);
							lua.setmetatable(ls, -2);
							Pop;
						end;
					ln_Meta:
						begin
							getobject(ap.target);
							getobject(ap.app);
							lua.setmetatable(ls, -2);
							Pop;
						end;
					ln_UV:
						begin
							getobject(ap.target);
							getobject(ap.app);
							lua.setuservalue(ls, -2);
							Pop;
						end;
					ln_Stack:
						begin
							getobject(st.obj);
							lua.replace(ls, st.slot);
						end;
					else Assert(no);
			end;
			inc(i);
		end;

	{$ifdef Debug}
		if length(ups) > 0 then Log('Восстановление {0} {1}...', lang_amount(CountUpvalueInstances, '{N} экземпляр{а/ов/ов}'),
		                            lang_amount(length(ups), '{N} замыкан{ия/ий/ий}'));
	{$endif}

		for i := 0 to High(ups) do
			with ups[i] do
			begin
				getobject(fns[0].fn);
				getobject(value);
				if not Assigned(lua.setupvalue(ls, -2, fns[0].index)) then SerializationError('не удалось восстановить замыкание');
				for j := 1 to High(fns) do
				begin
					getobject(fns[j].fn);
					lua.upvaluejoin(ls, -1, fns[j].index, -2, fns[0].index);
					Pop;
				end;
				Pop;
			end;

	{$ifdef Debug} if length(ups) > 0 then Log('Восстановление {0}...', lang_amount(length(objs), '{N} объект{а/ов/ов}')); {$endif}
		lua.rawgeti(ls, lua.REGISTRYINDEX, Reg.ObjUserdatas);
		for i := 0 to High(objs) do
		begin
			getobject(objs[i]);
			o := pPointer(lua.touserdata(ls, -1))^;

			{lua.rawgetp(ls, -2, o);
			if NonNil(-1) then
			begin
				Pop;
				continue;
			end else
				Pop;}

			PushCopy(-1);
			lua.rawsetp(ls, -3, o);

			if Assigned(o) then
			begin
				if _getApiMt(ls, TypeOf(o^)) then
					lua.setmetatable(ls, -2)
				else
					SerializationError('неизвестный тип скриптового объекта');
			end else
				SerializationError('нулевой объект в скрипте?');
			Pop;
		end;
		Pop;

		result := yes;
	_finally_:
		de^.ContinueAfter;
		done_state;
		GC.Collect;
	end;
{$endif}

	constructor ScriptDelegateWrapper.Init(newObj: pObject; newMd: pMultiDelegate; newFunc: pointer);
	begin
		Assert(Assigned(newObj));
		inherited Init;
		obj := newObj;
		md := newMd;
		func := newFunc;
	end;

	destructor ScriptDelegateWrapper.Done;
	begin
		inherited Done;
	end;

	function ScriptDelegateWrapper.GetNamed(var ss: ScriptState; const name: string): boolean;
	begin
		result := ss.GetDelegate(md, name);
	end;

	procedure ScriptDelegateWrapper.SetNamed(var ss: ScriptState; const newName: string);
	begin
		ss.SetDelegate(obj, md, func, newName);
	end;

	procedure ScriptPath.Init(newBase: pScriptPath; newInPlace: boolean);
	begin
		System.Initialize(self);
		base := newBase;
		value := '';
		inPlace := newInPlace;
	end;

	procedure ScriptPath.Done;
	begin
		System.Finalize(self);
	end;

	procedure ScriptPath.Append(const fn: string);
	begin
		value := StreamPath.ForcePath(value) + fn;
	end;

	function ScriptPath.Final: string;
	begin
		if Assigned(base) then result := base^.Final else result := '';
		if value <> '' then result := StreamPath.Resolve(value, StreamPath.ForcePath(result));
	end;

	function CollectPath(ls: lua.State): sint; cdecl;
	begin
		pScriptPath(ss_by_ls^.ToData(1))^.Done;
		result := 0;
	end;

	function IndexConcatPathTable(ls: lua.State): sint; cdecl; forward;

	procedure TurnIntoPathTable(ls: lua.State; var ss: ScriptState; path: pScriptPath; pathidx: sint);
	begin
		Assert(pathidx > 0);
		path^.inPlace := no;
		ss.PushTable(0, 2);
		_registerIn(ls, -1, LuaID_Index, @IndexConcatPathTable);
		_registerIn(ls, -1, LuaID_Concat, @IndexConcatPathTable);
		lua.setmetatable(ls, -2);

		ss.PushNil;
		while ss.Next(-2) do
		begin
			case ss.Typ(-1) of
				script_String:
					begin
						ss.PushPath(path, no)^.Append(ss.ToString(-2));
						ss.SetWithKey(-4, -3);
					end;
				else Assert(no, ss.ToString(-1));
			end;
			ss.Pop;
		end;
		ss.PushCopy(pathidx); ss.SetTableI(-2, 0);
	end;

	function IndexConcatPath(ls: lua.State): sint; cdecl;
	var
		ss: pScriptState;
		path: pScriptPath;
	begin
		ss := ss_by_ls;
		path := ss^.ToData(1);
		result := 1;

		if ss^.IsTable(-1) then TurnIntoPathTable(ls, ss^, path, 1) else
		begin
			if path^.inPlace then ss^.PushCopy(1) else path := ss^.PushPath(path, yes);
			if ss^.ToStringView(2) = 'up' then
				path^.Append('..')
			else
				path^.Append(ss^.ToString(2));
		end;
	end;

	function IndexConcatPathTable(ls: lua.State): sint; cdecl;
	begin
		lua.rawgeti(ls, 1, 0);
		lua.replace(ls, 1);
		result := IndexConcatPath(ls);
	end;

	function CallPath(ls: lua.State): sint; cdecl;

		function Run(ls: lua.State): sint;
		var
			ss: pScriptState;
			path: pScriptPath;
		begin
			ss := ss_by_ls;
			path := ss^.ToData(1);
			result := ss^.LoadModule(path^.Final, ss^.Top - 1, -1, [PushError]);
		end;

	begin
		result := Run(ls);
		if result < 0 then
			result := lua.error(ls)
		else
			inc(result); // окружение перед результатами
	end;

	function UnescapeInterpolationParam(const s: string): string;
	var
		i, start: sint;
		_sb: StringBuilder;
		sbOk: boolean;

		function Sb: pStringBuilder;
		begin
			if not sbOk then
			begin
				sbOk := yes;
				_sb.Init;
			end;
			result := @_sb;
		end;

	begin
		sbOk := no;
		i := 1;
		start := 1;
		while i <= length(s) do
		begin
			case s[i] of
				'\':
					if i < length(s) then
					begin
						Sb^.Append(pChar(s) + start - 1, i - start);
						inc(i);
						start := i;
					end;
			end;
			inc(i);
		end;
		if sbOk then
		begin
			_sb.Append(pChar(s) + start - 1, length(s) - start + 1);
			result := _sb.DestructiveToString;
		end else
			result := s;
	end;

	function Preprocess(const source: string; assertions: boolean): string;
	type
		tTokenType = (tok_Nothing, tok_EOF, tok_Assert, tok_Error, tok_Yes, tok_No, tok_BlockStart, tok_BlockEnd, tok_Colon, tok_Identifier,
			tok_Op, tok_NonOp, tok_Format, tok_String);
	const
		tok_Unknown = tok_Nothing;

	type
		tBlockKind = (blk_Brackets, blk_DoEnd);

		tToken = record
			sp, ep: sint;
		case typ: tTokenType of
			tok_BlockStart: (bk: tBlockKind);
		end;

		function ReadIdentifier(var pos: sint): sint;
		var
			p2: sint;
		begin
			Assert(source[pos] in ['a' .. 'z', 'A' .. 'Z', '_']);
			p2 := pos + 1;
			while (p2 <= length(source)) and (source[p2] in ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_']) do
				inc(p2);
			result := p2 - pos;
			pos := p2;
		end;

		procedure SkipString(var pos: sint);
		var
			quote: char;
		begin
			quote := source[pos]; Assert(quote in ['"', '''']);
			inc(pos);
			while pos < length(source) do
			begin
				if source[pos] = quote then break;
				if source[pos] = '\' then inc(pos); // следующий инкремент скипнет символ после "\"
				inc(pos);
			end;
			inc(pos);
		end;

		function ReadLongStringStart(var pos: sint): sint;
		var
			cp: sint;
		begin
			Assert(source[pos] = '[');
			cp := pos + 1;
			result := 0;
			while cp <= length(source) do
			begin
				case source[cp] of
					'=': inc(result);
					'[':
						begin
							pos := cp + 1;
							exit;
						end;
					else break;
				end;
				inc(cp);
			end;
			result := -1;
		end;

		function NEqualSigns(s: pChar; n: sint): boolean;
		var
			i: sint;
		begin
			for i := 0 to n - 1 do
				if s[i] <> '=' then
					exit(no);
			result := yes;
		end;

		function IsLongStringEnding(s: pChar; long: sint): boolean;
		begin
			result := (s[0] = ']') and (s[long + 1] = ']') and NEqualSigns(s + 1, long);
		end;

		procedure SkipLongString(var pos: sint; long: sint);
		var
			i: sint;
		begin
			for i := pos to length(source) - long - 2 + 1 do
				if IsLongStringEnding(pChar(source) + i - 1, long) then
				begin
					pos += 1 + long + 1;
					exit;
				end;
			pos := length(source) + 1;
		end;

		function ScanToken(var pos: sint): tToken;
		var
			ls, idlen: sint;
		begin
			repeat
				result.typ := tok_Unknown;
				if pos > length(source) then result.typ := tok_EOF else
					if Symbol.IsWhitespace(source[pos]) then inc(pos) else
					begin
						result.sp := pos;
						case source[pos] of
							'a'..'z', 'A'..'Z', '_':
								begin
									idlen := ReadIdentifier(pos);
									if StrEq(@source[result.sp], idlen, 'end') then result.typ := tok_BlockEnd else
									if StrEq(@source[result.sp], idlen, 'yes') then result.typ := tok_Yes else
									if StrEq(@source[result.sp], idlen, 'no') then result.typ := tok_No else
									if StrEq(@source[result.sp], idlen, 'assert') then result.typ := tok_Assert else
									if StrEq(@source[result.sp], idlen, 'error') then result.typ := tok_Error else
									if StrEq(@source[result.sp], idlen, 'function') or StrEq(@source[result.sp], idlen, 'do') or StrEq(@source[result.sp], idlen, 'then') then
									begin
										result.typ := tok_BlockStart;
										result.bk := blk_DoEnd;
									end else
									if StrEq(@source[result.sp], idlen, 'format') then result.typ := tok_Format else
										result.typ := tok_Identifier;
								end;
							'"', '''':
								begin
									result.typ := tok_String;
									SkipString(pos);
								end;
							'-':
								if (pos + 1 <= length(source)) and (source[pos + 1] = '-') then
								begin
									inc(pos, 2);
									if (pos <= length(source)) and (source[pos] = '[') then ls := ReadLongStringStart(pos) else ls := -1;
									if ls >= 0 then SkipLongString(pos, ls) else
										while (pos <= length(source)) and not Symbol.IsNewline(ord(source[pos])) do
											inc(pos);
								end else
								begin
									result.typ := tok_Op;
									inc(pos);
								end;
							'[':
								begin
									ls := ReadLongStringStart(pos);
									if ls >= 0 then
									begin
										result.typ := tok_String;
										SkipLongString(pos, ls);
									end else
									begin
										result.typ := tok_BlockStart;
										result.bk := blk_Brackets;
										inc(pos);
									end;
								end;
							'{', '(':
								begin
									result.typ := tok_BlockStart;
									result.bk := blk_Brackets;
									inc(pos);
								end;
							']', ')', '}':
								begin
									inc(pos);
									result.typ := tok_BlockEnd;
								end;
							',':
								begin
									result.typ := tok_Colon;
									inc(pos);
								end;
							'+', '*', '/', '%', '^', '#', '=', '<', '>', '.':
								begin
									result.typ := tok_Op;
									inc(pos);
								end;
							else
							begin
								result.typ := tok_NonOp;
								inc(pos);
							end;
						end;
					end;
			until result.typ <> tok_Unknown;
			result.ep := pos;
		end;

	var
		nt: sint;
		t: array of tToken;

		procedure CutRange(a, b: sint);
		var
			i: sint;
		begin
			for i := a to b do
				t[i].typ := tok_Nothing;
		end;

		function FindBlockEnd(start: sint; colon: pSint = nil): sint;
		var
			i, b: sint;
		begin
			Assert(t[start].typ = tok_BlockStart);
			b := 1;
			if Assigned(colon) then colon^ := -1;
			for i := start + 1 to nt - 1 do
				case t[i].typ of
					tok_BlockStart: inc(b);
					tok_BlockEnd:
						begin
							dec(b);
							if b = 0 then exit(i);
						end;
					tok_Colon:
						if (b = 1) and Assigned(colon) and (colon^ < 0) then colon^ := i;
				end;
			result := -1;
		end;

		function PrevTokenEnd(i: sint): sint;
		begin
			if i > 0 then
				result := t[i - 1].ep
			else
				result := 1;
		end;

		function NextTokenStart(i: sint): sint;
		begin
			if i + 1 < nt then
				result := t[i + 1].sp
			else
				result := length(source) + 1;
		end;

		procedure Append(var sb: StringBuilder; tokA, tokB: sint);
		begin
			if tokB >= tokA then
				sb.Append(pChar(source) + (PrevTokenEnd(tokA) - 1), NextTokenStart(tokB) - PrevTokenEnd(tokA));
		end;

		procedure Append(var sb: StringBuilder; token: sint);
		begin
			Append(sb, token, token);
		end;

		procedure Flush(var sb: StringBuilder; var startToken: sint; curToken: sint);
		begin
			Append(sb, startToken, curToken - 1);
			startToken := curToken + 1;
		end;

	{$ifdef Debug}
		function DebugDump(const outerResult: string): string;
		const
			TokenTypeIds: array[tTokenType] of string = ('---', 'EOF', 'assert', 'error', 'yes', 'no', 'block start', 'block end', ',', 'identifier',
				'op', 'non-op', 'format', 'string');
		var
			b: StringBuilder;
			i: sint;
		begin
			b.Init;
			b.Append(source, EOL, '======', EOL);
			for i := 0 to nt - 1 do
			begin
				b.Append(TokenTypeIds[t[i].typ]);
				if i + 1 < nt then b.Append('(', Copy(source, t[i].sp, min(t[i].ep - t[i].sp, length(source) - t[i].sp + 1)), ') ');
			end;
			b.Append(EOL, '===R==');
			b.Append(outerResult);
			result := b.DestructiveToString;
		end;
	{$endif}

		function GetRawTokenData(token: sint): string;
		begin
			result := Copy(source, PrevTokenEnd(token), NextTokenStart(token) - PrevTokenEnd(token));
		end;

		function SplitInterpolableString(const fms: string; out items: Strings; inlineVersion: boolean): string;
		var
			i, nPotential, nItems, nOpened, startAt: sint;
			sb: StringBuilder;
		begin
			nOpened := 0;
			nPotential := 0;
			i := 1;
			while i <= length(fms) do
			begin
				case fms[i] of
					'\': inc(i); // скипнуть следующий символ
					'{':
						if (not inlineVersion) or ((i + 1 <= length(fms)) and (fms[i + 1] = '=')) then
						begin
							if nOpened = 0 then inc(nPotential);
							inc(nOpened);
							if inlineVersion then inc(i);
						end;
					'}': dec(nOpened);
				end;
				inc(i);
			end;

			if nPotential = 0 then
			begin
				items := nil;
				exit(fms);
			end;

			SetLength(items, nPotential);
			nItems := 0;
			nOpened := 0;
			sb.Init;
			i := 1;
			startAt := 1;
			while i <= length(fms) do
			begin
				case fms[i] of
					'\': inc(i); // скипнуть следующий символ
					'{':
						if (not inlineVersion) or ((i + 1 <= length(fms)) and (fms[i + 1] = '=')) then
						begin
							if nOpened = 0 then
							begin
								sb.Append(pChar(fms) + (startAt - 1), i - startAt + 1); // вместе со скобкой
								if inlineVersion then inc(i);
								startAt := i + 1;
							end;
							inc(nOpened);
						end;
					'}':
						begin
							dec(nOpened);
							if nOpened = 0 then
							begin
								Assert(nItems < length(items));
								items[nItems] := Preprocess(UnescapeInterpolationParam(Copy(fms, startAt, i - startAt)), assertions);
								sb.Append(Utils.ToString(nItems));
								inc(nItems);
								startAt := i; // вместе со скобкой
							end;
						end;
				end;
				inc(i);
			end;
		{$ifdef Debug} if nOpened > 0 then Log('Похоже, строка "' + fms + '" содержит незакрытые блоки формата.', logError); {$endif}
			sb.Append(pChar(fms) + (startAt - 1), length(fms) - startAt + 1);
			SetLength(items, nItems);
			result := sb.DestructiveToString;
		end;

		procedure HandleFormatInterpolation(var sb: StringBuilder; var startToken, curToken: sint);
		var
			fms: string;
			items: Strings;
			lastToken: sint;
		begin
			if t[curToken].typ = tok_String then
			begin
				lastToken := curToken;
				fms := GetRawTokenData(curToken);
			end else
				if (curToken + 3 < nt) and (t[curToken + 1].typ = tok_BlockStart) and (t[curToken + 1].bk = blk_Brackets)
					and (t[curToken + 2].typ = tok_String)
					and (t[curToken + 3].typ = tok_BlockEnd)
				then
				begin
					lastToken := curToken + 3;
					fms := GetRawTokenData(curToken + 2);
				end
				else
					if (curToken + 1 < nt) and (t[curToken + 1].typ = tok_String) then
					begin
						lastToken := curToken + 1;
						fms := GetRawTokenData(curToken + 1);
					end else
						exit;

			fms := SplitInterpolableString(fms, items, t[curToken].typ = tok_String);
			if length(items) > 0 then
			begin
				Flush(sb, startToken, curToken);
				sb.Append('format(', fms, ', ', SeparatedList.Join(items, ', '), ')');
				curToken := lastToken;
				startToken := lastToken + 1;
			end;
		end;

	var
		i, pos, start, ed, colon: sint;
		rsb: StringBuilder;
	{$ifdef Debug} nAss, nErr, nTf: sint; parseTime, cutTime, sbTime: Ticks; msg: string; {$endif}
	begin
	{$ifdef Debug} nAss := 0; nErr := 0; nTf := 0; parseTime := Ticks.Get; {$endif}
		nt  := 0;
		t := nil;
		pos := 1;
		repeat
			inc(nt);
			if nt > length(t) then SetLength(t, 2 * nt);
			t[nt - 1] := ScanToken(pos);
			if t[nt - 1].typ = tok_EOF then break;
			if (nt > 1) and (t[nt - 1].typ in [tok_Op, tok_NonOp]) and (t[nt - 2].typ = t[nt - 1].typ) then
			begin
				t[nt - 2].ep := t[nt - 1].ep;
				dec(nt);
			end;
		until no;
	{$ifdef Debug} parseTime := Ticks.Get - parseTime; {$endif}

		if assertions then
		begin
			// не вырезать assert/error
		end else
		begin
		{$ifdef Debug} cutTime := Ticks.Get; nAss := 0; nErr := 0; {$endif}

			for i := 0 to nt - 1 do
				case t[i].typ of
					tok_Assert:
						if (i + 1 < nt) and (t[i + 1].typ = tok_BlockStart) then
						begin
						{$ifdef Debug} inc(nAss); {$endif}
							ed := FindBlockEnd(i + 1, @colon);
							if ed >= 0 then
								if (i > 0) and (((t[i - 1].typ = tok_BlockStart) and (t[i - 1].bk = blk_Brackets)) or (t[i - 1].typ in [tok_Colon, tok_Op])) then
								begin
									CutRange(i, i + 1);
									if colon >= 0 then CutRange(colon, ed) else CutRange(ed, ed);
								end else
									CutRange(i, ed);
						end;
					tok_Error:
						if (i + 1 < nt) and (t[i + 1].typ = tok_BlockStart) then
						begin
						{$ifdef Debug} inc(nErr); {$endif}
							ed := FindBlockEnd(i + 1, nil);
							if ed >= 0 then CutRange(i, ed);
						end;
				end;
		{$ifdef Debug} cutTime := Ticks.Get - cutTime; {$endif}
		end;

	{$ifdef Debug} sbTime := Ticks.Get; {$endif}
		rsb.Init;
		i := 0;
		start := i;
		while i < nt do
		begin
			case t[i].typ of
				tok_Nothing: Flush(rsb, start, i);
				tok_Yes, tok_No:
					begin
						Flush(rsb, start, i);
					{$ifdef Debug} inc(nTf); {$endif}
						rsb.Append(TrueFalse[t[i].typ = tok_Yes]);
					end;
				tok_Format: HandleFormatInterpolation(rsb, start, i);
				tok_String: HandleFormatInterpolation(rsb, start, i);
			end;
			inc(i);
		end;
		Flush(rsb, start, i);
		result := rsb.DestructiveToString;
	{$ifdef Debug} sbTime := Ticks.Get - sbTime; {$endif}

	{$ifdef Debug}
		msg := 'разбор: ' + Utils.ToString(parseTime) + '; ';
		if not assertions then msg += Format('assert/error: {0}/{1} ({2}); ', Utils.ToString(nAss), Utils.ToString(nErr), Utils.ToString(cutTime));
		if nTf > 0 then msg += 't/f: ' + Utils.ToString(nTf) + '; ';
		msg += 'построение: ' + Utils.ToString(sbTime) + '; ';
		LogR(msg, logDebug);
	{$endif}
	end;

{$ifdef use_serialization}
	procedure SerializeScriptDelegate(se: pSerializer; obj: pointer);
	var
		sd: pScriptDelegate absolute obj;
	begin
		with se^ do
		begin
			SeObject(sd^.ss);
			SeObject(sd^.obj);
			SeObject(sd^.md, ObjType_MultiDelegate);
			Serialize_string(stream, sd^.name);
			VarInt.Write(stream, sd^.uid);
		end;
	end;

	procedure DeserializeScriptDelegate(de: pDeserializer; obj: pointer);
	var
		sd: pScriptDelegate absolute obj;
	begin
		with de^ do
		begin
			DeWeakR(sd^.ss);
			DeWeakR(sd^.obj);
			DeWeakR(sd^.md);
			sd^.name := Deserialize_string(stream);
			sd^.uid := VarInt.Read(stream);
		end;
	end;

	procedure ScriptDelegateDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		sd: pScriptDelegate absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: sd^.DeseInit;
			de_After:
				begin
					sd^.obj^.AddOnDestroyProc(@HandleObjectDestroying, sd);
					sd^.ss^.AddOnDestroyProc(@HandleScriptStateDestroying, sd);
				end;
		end;
	end;

	procedure RegisterWheel(index: uint; param: pointer);
	begin
		pSerializationDB(param)^.RegisterFunc(Wheels[index].fn);
	end;
{$endif}

	procedure Init;
	begin
	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Script state', TypeOf(ScriptState), nil, sizeof(ScriptState), yes, nil, nil, nil, nil)
		^.RegisterType('Script delegate', TypeOf(ScriptDelegate), nil, sizeof(ScriptDelegate), yes,
			@SerializeScriptDelegate, @DeserializeScriptDelegate, nil, @ScriptDelegateDeSpecial)
		^.RegisterFunc(@DestroySingleDelegate);
		Range.Open(length(Wheels)).Each(@RegisterWheel, SerializationDB.Shared);
	{$endif}
	end;

initialization
	&Unit('Script').Initialize(@Init);
end.
