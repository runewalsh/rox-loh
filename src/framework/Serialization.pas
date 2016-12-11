unit Serialization;

{$include opts.inc}
{$ifdef Debug}
	{-$define ExtDebug}
{$endif}
{$ifNdef use_serialization} {$error serialization disabled, so don't use this} {$endif}

interface

uses
	USystem, Errors, Streams, Algo, UMath, Utils {$ifdef Debug}, ULog, Debug {$endif};

const
	Signature = '>--(X_X)-->';
	SerializerVersionPrefix = 'Serializer version: ';
	SerializerVersion = '1.06';
	FileVersionPrefix = 'File version: ';
	FileVersionSuffix = {$ifdef Debug} '-D' {$else} '' {$endif};
	VerbosePrefix = '<<';
	VerboseSuffix = '>>';

type
	pSerializer = ^Serializer;
	pDeserializer = ^Deserializer;
	SerializeProc = procedure(se: pSerializer; obj: pointer);
	DeserializeProc = procedure(de: pDeserializer; obj: pointer);

	SeSpecial = (se_Before, se_After);
	SeSpecialProc = procedure(se: pSerializer; what: SeSpecial; obj: pointer);

	DeSpecial = (de_Initialize, de_FixLink, de_After, de_After2);
	DeSpecialProc = procedure(de: pDeserializer; what: DeSpecial; var obj: pointer);

	// TODO: универсальная сериализация-десериализация
	UniversalHandler = class
		se: boolean;
		ser: pSerializer;
		der: pDeserializer;

		{procedure SeDe(obj: pointer); virtual;
		procedure Special(what: UniversalSpecial; obj: pointer); virtual;}
	end;

	pTypeInfo = ^TypeInfo;
	TypeInfo = object
		name: string;
		typeof: pointer;
		parent_type: pointer;
		parent: pTypeInfo;
		instanceSize: size_t;
		refcounted, rffAllowed: boolean;
		serialize: SerializeProc;
		deserialize: DeserializeProc;
		seSpecial: SeSpecialProc;
		deSpecial: DeSpecialProc;
		procedure Init(const newName: string;
			newTypeof, newParent: pointer; const newInstanceSize: size_t; newRefcounted: boolean;
			newSerialize:   SerializeProc;
			newDeserialize: DeserializeProc;
			newSeSpecial:   SeSpecialProc;
			newDeSpecial:   DeSpecialProc);
		procedure Done;
	end;

{$define classname:=Ptr2ID} {$define key_type:=pointer} {$define value_type:=uint} {$define null_value:=0}
{$include hash.h.inc}

	PtrPair = object
		a, b: pointer;
		function Make(newA, newB: pointer): PtrPair; static;
	end;

{$define classname:=PtrPair2ID} {$define key_type:=PtrPair} {$define value_type:=uint} {$define null_value:=0}
{$include hash.h.inc}

	pSerializationDB = ^SerializationDB;
	SerializationDB = object
	type
		pSelf = pSerializationDB;

		function Shared: pSelf; static;
		function RegisterType(const newName: string;
			newTypeof, newParent: pointer; const newInstanceSize: size_t; newRefcounted: boolean;
			newSerialize:   SerializeProc;
			newDeserialize: DeserializeProc;
			newSeSpecial:   SeSpecialProc;
			newDeSpecial:   DeSpecialProc): pSelf;
		function FindType(ot: pointer): pTypeInfo;
		function TypeName(ot: pointer): string;
		function Type2ID(ot: pointer): uint;
		function TypeInfo2ID(ti: pTypeInfo): uint;
		function ID2TypeInfo(id: uint): pTypeInfo;

		function RegisterFunc(fn: pointer): pSelf;
		function RegisterFuncs(const fns: array of pointer): pSelf;
		function Function2ID(fn: pointer): uint;
		function ID2Function(id: uint): pointer;

		function AddEnv(newObj: pointer; newTypeof: pointer): pSelf;
		function AddEnv(newObj: pObject): pSelf;
		function AddEnv(const newObjs: array of pObject): pSelf;
		function RemEnv(aObj: pointer; aTypeof: pointer): pSelf;
		function RemEnv(aObj: pObject): pSelf;
		function RemEnv(const aObjs: array of pObject): pSelf;
	private var
		_t2id: Ptr2ID;
		_tis: array of TypeInfo;
		_fn2id: Ptr2ID;
		_fns: array of pointer;
		_env: array of record
			obj: pointer;
			ty: pointer;
			tyid: sint;
		end;
		_completed: boolean;
		procedure Init;
		procedure Done; {$include dyn_obj.h.inc}
		procedure Complete;
	end;

	Serializer = object
	private type
		pSe = ^tSe;
		tSe = object
		public
			obj: pointer;
			typ: pTypeInfo;
			next: pSe;
			constructor Init(newObj: pointer; newTyp: pTypeInfo);
			destructor Done;
		end;

	private var
		_db: pSerializationDB;
		_stream: pStream;
		_verbose, ruined: boolean;
		_typeidFmt, _fnidFmt: UiBinaryFormat;
		_busy, _afterDelayed: boolean;
		_seFirst, _seLast: pSe;
		_s2id: PtrPair2ID;
		_nextSeId: uint;
		_nAfter: sint;
		_after: array of tSe;
		procedure _Init(newStream: pStream; const version: string);
		procedure _AddEnv(newObj: pointer; newTypeof: pointer);
		function _SeObject(obj: pointer; ot: pointer): uint;
		function _GetSe(out se: tSe): boolean;
		procedure TrustedProcessSe;
		procedure ProcessSe;
	public
		constructor Init(newStream: pStream; const version: string);
		constructor Init(const newFn: string; const version: string);
		destructor Done;
		property Stream: pStream read _stream;
		procedure ChangeStream(newStream: pStream);

		procedure SeObject(obj: pObject);
		procedure SeObject(obj: pointer; ot: pointer);
		procedure SeFunction(fn: pointer);
		property Verbose: boolean read _verbose;
		procedure DelayAfter;
		procedure ContinueAfter;
	end;

	Deserializer = object
	private type
		DeFlag = (de_InPlace, de_Resolved, de_RFF);
		DeFlags = set of DeFlag;

		pDe = ^tDe;
		tDe = object
			typ: pTypeInfo;
			obj: pointer;

			flags: DeFlags;
			links: array of record
				ofs: PtrUint;
				target: uint;
				strong, absolute: boolean;
			end;
			procedure Initialize;
			procedure Finalize;
			procedure Place(var des: Deserializer; newParent: uint; newOfs: PtrUint);
			procedure SetType(newTyp: pTypeInfo);
			procedure AddLink(newOfs: PtrUint; newTarget: uint; newStrong, newAbsolute: boolean);
			procedure ResolveLinks(var des: Deserializer; to_: uint);
		end;

	private var
		_db: pSerializationDB;
		_stream: pStream;
		_verbose: boolean;
		_typeidFmt, _fnidFmt: UiBinaryFormat;

		_de: array of tDe;
		_current, _last, _afterStart: uint;
		_busy, _afterDelayed: boolean;
		procedure _Init(newStream: pStream; const version: string; justTry: boolean);
		procedure _AddEnv(newObj: pointer; newTypeof: pointer);
	{$ifdef Debug} function _HumanObject(id: uint): string; {$endif}
		procedure _EnsureLast(newLast: uint);
		procedure _ValidateIndex(id: uint);
		function _ID2De(id: uint): pDe;
		procedure _DeObject(out raw; inplace, strong, absolute: boolean);
		procedure _ResolveLink(var link; target: pDe; strong, allowRelease: boolean);
		procedure _ProcessDe;
	public
		date: DateTime;
		constructor Init(newStream: pStream; const version: string; justTry: boolean = no);
		constructor Init(const newFn: string; const version: string; justTry: boolean = no);
		destructor Done;
		property Stream: pStream read _stream;

		procedure DeObjectA(out ref: pObject);
		procedure DeObjectR(out ref: pObject);
		procedure DeWeakA(out ref: pointer);
		procedure DeWeakR(out ref: pointer);
		procedure DeObjectAtA(out obj);
		procedure DeObjectAtR(out obj);
		procedure DeWeakAtA(out obj);
		procedure DeWeakAtR(out obj);
		procedure ChangeLink(old, typeof, new: pointer); // o_o"
		function DeFunction: pointer;
		property Verbose: boolean read _verbose;
		procedure DelayAfter;
		procedure ContinueAfter;
	type
		VersionMismatch = class(Exception) end;
	end;

	procedure SerializationError(const msg: string = '');

var
	Config: record
		verbose: boolean;
	end =
	(
		verbose: no;
	);

implementation

uses
	UClasses;

const
	RFF_BIT = 1 shl 0;

	TYPEID_NFLAGS = 1;
	TYPEID_HAS_FLAGS_BIT = 1 shl 0;

	{$define classname:=Ptr2ID} {$define hash_func:=Hash.OfPointer} {$include hash.pp.inc}

	{$define classname:=PtrPair2ID}
	{$define inline_hash:=Hash.OfPointer(_1.a) xor Hash.OfPointer(_1.b)}
	{$define inline_eq:=(_1.a = _2.a) and (_1.b = _2.b)}
	{$include hash.pp.inc}

	procedure SerializationError(const msg: string = '');
	var
		m2: string;
	begin
		m2 := 'Ошибка (де)сериализации';
		if msg <> '' then m2 += ': ' + msg;
	{$ifdef Debug} m2 += EOL + GetBackTrace; {$endif}
		raise Error(m2 + '.');
	end;

	function PtrPair.Make(newA, newB: pointer): PtrPair;
	begin
		result.a := newA;
		result.b := newB;
	end;

{$ifdef ExtDebug}
	function LinkToStr(ofs: PtrUint; absolute: boolean): string;
	begin
		if absolute then
			result := ToString(pPointer(@ofs)^)
		else
			result := '+' + ToString(ofs);
	end;
{$endif}

	procedure TypeInfo.Init(const newName: string;
		newTypeof, newParent: pointer; const newInstanceSize: size_t; newRefcounted: boolean;
		newSerialize:   SerializeProc;
		newDeserialize: DeserializeProc;
		newSeSpecial:   SeSpecialProc;
		newDeSpecial:   DeSpecialProc);
	begin
		name := newName;
		typeof       := newTypeof;
		parent_type  := newParent;
		parent       := nil;
		instanceSize := newInstanceSize;
		refcounted   := newRefcounted;
		rffAllowed   := refcounted and ResourcePool.Shared^.Registered(typeof);
		serialize    := newSerialize;
		deserialize  := newDeserialize;
		seSpecial    := newSeSpecial;
		deSpecial    := newDeSpecial;
	end;

	procedure TypeInfo.Done;
	begin
	end;

{$define instance_type := pSerializationDB} {$define accessor := SerializationDB.Shared}
{$define create_instance := SerializationDB.Create} {$define destroy_instance := instance^.Free(instance)}
{$include lazy_singleton.inc}
{$define chainable := result := @self;}

	function SerializationDB.RegisterType(const newName: string;
		newTypeof, newParent: pointer; const newInstanceSize: size_t; newRefcounted: boolean;
		newSerialize:   SerializeProc;
		newDeserialize: DeserializeProc;
		newSeSpecial:   SeSpecialProc;
		newDeSpecial:   DeSpecialProc): pSelf;
	var
		nid: uint;
	{$ifdef ExtDebug} msg: string; {$endif}
	begin chainable
	{$ifdef ExtDebug}
		Log('SerializationDB.RegisterType(name = ' + newName + ', typeof = ' + ToString(newTypeof) + ', parent = ' + ToString(newParent) + ', size = ' + ToString(newInstanceSize) + ', refcounted = ' + BoolToStr[newRefcounted], logDebug);
	{$endif}

		Assert(_t2id.Find(newTypeof) = 0, 'Повторная регистрация типа: ' + newName + ' (typeof = ' + ToString(newTypeof) + ')');
		SetLength(_tis, length(_tis) + 1);
		_tis[High(_tis)].Init(newName, newTypeof, newParent, newInstanceSize, newRefcounted, newSerialize, newDeserialize, newSeSpecial, newDeSpecial);
		nid := 1 + High(_tis);
		_t2id.Add(newTypeof, nid);
		_completed := no;

	{$ifdef ExtDebug}
		msg := 'Зарегистрирован тип "' + newName + '" (ID: ' + ToString(nid) + ', sizeof = ' + ToString(newInstanceSize) + ', typeof: ' + ToString(newTypeof) + ', parent: ' + ToString(newParent) + ')';
		if _tis[High(_tis)].rffAllowed then
		begin
			msg += '. Его экземпляры ';
			if Assigned(newDeserialize) then msg += 'могут' else msg += 'ДОЛЖНЫ';
			msg += ' быть ресурсами-из-файлов';
		end;
		Log(msg, logDebug);
	{$endif}
	end;

	function SerializationDB.FindType(ot: pointer): pTypeInfo;
	begin
		result := @_tis[Type2ID(ot) - 1];
	end;

	function SerializationDB.TypeName(ot: pointer): string;
	var
		id: sint;
	begin
		id := _t2id.Find(ot);
		if id > 0 then result := _tis[id-1].name else result := '(Type ' + ToString(ot) + ')';
	end;

	function SerializationDB.Type2ID(ot: pointer): uint;
	begin
		result := _t2id.Find(ot);
		if result = 0 then SerializationError {$ifdef Debug} ('неизвестный тип (' + ToString(ot) + ')' + EOL + GetBackTrace) {$endif};
	end;

	function SerializationDB.TypeInfo2ID(ti: pTypeInfo): uint;
	begin
		result := 1 + (ti - @_tis[0]);
		assert((result >= 1) and (result - 1 <= High(_tis)), 'invalid typeinfo ptr!');
	end;

	function SerializationDB.ID2TypeInfo(id: uint): pTypeInfo;
	begin
		if (id < 1) or (id - 1 > High(_tis)) then
			SerializationError {$ifdef Debug} ('ID ' + ToString(id) + ' не соответствует ни одному из известных типов') {$endif};
		result := @_tis[id - 1];
	end;

	function SerializationDB.RegisterFunc(fn: pointer): pSelf;
	begin chainable
		if _fn2id.Find(fn) <> 0 then exit;
		SetLength(_fns, length(_fns) + 1);
		_fns[High(_fns)] := fn;
		_fn2id.Add(fn, 1 + High(_fns));
	end;

	function SerializationDB.RegisterFuncs(const fns: array of pointer): pSelf;
	var
		fn: pointer;
	begin chainable
		for fn in fns do RegisterFunc(fn);
	end;

	function SerializationDB.Function2ID(fn: pointer): uint;
	begin
		result := _fn2id.Find(fn);
		if result = 0 then SerializationError {$ifdef Debug} ('неизвестная функция (' + ToString(fn) + ')') {$endif};
	end;

	function SerializationDB.ID2Function(id: uint): pointer;
	begin
		if (id < 1) or (id - 1 > High(_fns)) then
			SerializationError {$ifdef Debug} ('ID ' + ToString(id) + ' не соответствует ни одной из известных функций') {$endif};
		result := _fns[id - 1];
	end;

	function SerializationDB.AddEnv(newObj: pointer; newTypeof: pointer): pSelf;
{$ifdef Debug} var i: sint; {$endif}
	begin chainable
		if not Assigned(newObj) then exit;
	{$ifdef Debug}
		for i := 0 to High(_env) do
			Assert((_env[i].obj <> newObj) or (_env[i].ty <> newTypeof), 'Duplicate environment registration');
	{$endif}
		SetLength(_env, length(_env) + 1);
		with _env[High(_env)] do
		begin
			obj  := newObj;
			ty   := newTypeof;
			tyid := 0;
		end;
		_completed := no;
	end;

	function SerializationDB.AddEnv(newObj: pObject): pSelf;
	begin chainable
		if Assigned(newObj) then AddEnv(newObj, TypeOf(newObj^));
	end;

	function SerializationDB.AddEnv(const newObjs: array of pObject): pSelf;
	var
		i: sint;
	begin chainable
		for i := 0 to High(newObjs) do AddEnv(newObjs[i]);
	end;

	function SerializationDB.RemEnv(aObj: pointer; aTypeof: pointer): pSelf;
	var
		i: sint;
	begin chainable
		for i := 0 to High(_env) do
			if (_env[i].obj = aObj) and (_env[i].ty = aTypeof) then
			begin
				_env[i] := _env[High(_env)];
				SetLength(_env, length(_env) - 1);
				exit;
			end;
		Assert(no, 'Attempt to remove non-existing environment');
	end;

	function SerializationDB.RemEnv(aObj: pObject): pSelf;
	begin chainable
		if Assigned(aObj) then RemEnv(aObj, TypeOf(aObj^));
	end;

	function SerializationDB.RemEnv(const aObjs: array of pObject): pSElf;
	var
		i: sint;
	begin chainable
		for i := 0 to High(aObjs) do RemEnv(aObjs[i]);
	end;

	procedure SerializationDB.Init;
	begin
		_t2id.Init;
		_tis := nil;
		_fn2id.Init;
		_fns := nil;
		_completed := yes;
	end;

	procedure SerializationDB.Done;
	var
		i: sint;
	begin
		_t2id.Done;
		for i := 0 to High(_tis) do
			_tis[i].Done;
		_tis := nil;
		_fn2id.Done;
		_fns := nil;
	end;

{$define classname := SerializationDB} {$include dyn_obj.pp.inc}

	procedure SerializationDB.Complete;
	var
		i: sint;
	begin
		if _completed then exit;
		_completed := yes;
		for i := 0 to High(_tis) do
			if Assigned(_tis[i].parent_type) then
				_tis[i].parent := FindType(_tis[i].parent_type)
			else
				_tis[i].parent := nil;
		for i := 0 to High(_env) do
			if _env[i].tyid = 0 then
				_env[i].tyid := Type2ID(_env[i].ty);
	end;

	constructor Serializer.tSe.Init(newObj: pointer; newTyp: pTypeInfo);
	begin
		obj   := newObj;
		typ   := newTyp;
		next  := nil;
	end;

	destructor Serializer.tSe.Done;
	begin
	end;

	procedure special_r(se: pSerializer; ty: pTypeInfo; what: SeSpecial; obj: pointer);
	begin
		if Assigned(ty^.parent) then special_r(se, ty^.parent, what, obj);
		if Assigned(ty^.seSpecial) then ty^.seSpecial(se, what, obj);
	end;

	function Serializer._SeObject(obj: pointer; ot: pointer): uint;
	var
		pair: PtrPair;
		se: pSe;
	begin
		pair := PtrPair.Make(obj, ot);
		result := _s2id.Find(pair);
		if result <> 0 then exit;

		se := new(pSe, Init(obj, _db^.FindType(ot)));
	{$ifdef Debug} if not _busy then Log('Сохраняю ' + se^.typ^.name + ' в ' + StreamPath.Log(_stream^.path) + '...'); {$endif}
		special_r(@self, se^.typ, se_Before, obj);
		if Assigned(_seLast) then
		begin
			_seLast^.next := se;
			_seLast := se;
		end else
		begin
			_seFirst := se;
			_seLast := se;
		end;
		result := _nextSeId;
		_s2id.Add(pair, result);
		inc(_nextSeId);
		if _nextSeId = 0 then SerializationError {$ifdef Debug} ('индексы объектов переполнились o_O"') {$endif};
	end;

	function Serializer._GetSe(out se: tSe): boolean;
	var
		t: pSe;
	begin
		result := Assigned(_seFirst);
		if result then
		begin
			t := _seFirst;
			_seFirst := _seFirst^.next;
			if not Assigned(_seFirst) then _seLast := nil;
			se := t^;
			dispose(t, Done);
		end;
	end;

	function serialize_r(se: pSerializer; ty: pTypeInfo; obj: pointer): uint;
	begin
		result := 0;
		if Assigned(ty^.parent) then
			result += serialize_r(se, ty^.parent, obj);

		if Assigned(ty^.serialize) then
		begin
			ty^.serialize(se, obj);
			result += 1;
		end;
	end;

	procedure Serializer.TrustedProcessSe;
	var
		se: tSe;
		typeid, typeid_w, flags: uint;
		rff: boolean;
		rffStream: string;
		i: sint;
	begin
		while _GetSe(se) do
		begin
		{$ifdef ExtDebug} Log('Пишу объект #' + ToString(_s2id.Find(PtrPair(se.obj, se.typ^.typeof))) + ' (' + se.typ^.name + ')', logDebug); {$endif}
			typeid := _db^.TypeInfo2ID(se.typ);
			rff := se.typ^.rffAllowed and ResourcePool.Shared^.Loaded(se.obj, @rffStream);
			Assert((not rff) or Prefixed(Paths.Data, rffStream));

			flags := 0;
			if rff then flags := flags or RFF_BIT;

			typeid_w := typeid shl TYPEID_NFLAGS;
			if flags <> 0 then typeid_w := typeid_w or TYPEID_HAS_FLAGS_BIT;
			Serialize_ui(_stream, typeid_w, _typeidFmt);
			if flags <> 0 then Serialize_ui8(_stream, flags);
			if _verbose then Serialize_conststring(_stream, EOL + VerbosePrefix + se.typ^.name + VerboseSuffix + EOL);
		{$ifdef ExtDebug} Log('Записан индекс типа: ' + ToString(typeid) + ' (' + se.typ^.name + '), флаги: ' + ToString(flags), logDebug); {$endif}
			if rff then
				Serialize_string(_stream, CutPrefix(Paths.Data, rffStream))
			else
				if serialize_r(@self, se.typ, se.obj) = 0 then
					raise Error('{0} нельзя сохранить{1}.', se.typ^.name, IfThen(se.typ^.rffAllowed, ' таким образом', ''));
			inc(_nAfter);
			if _nAfter > length(_after) then SetLength(_after, 2 * _nAfter);
			_after[_nAfter - 1] := se;
		end;
		if not _afterDelayed then
		begin
			for i := _nAfter - 1 downto 0 do
				special_r(@self, _after[i].typ, se_After, _after[i].obj);
			_after := nil;
			_nAfter := 0;
		end;
	end;

	procedure Serializer.ProcessSe;
	begin
		if ruined then raise Error('Сериализация уже была запорота.');
		_busy := yes;
		try
			TrustedProcessSe;
		except
			ruined := yes;
			raise;
		end;
		_busy := no;
	end;

	procedure Serializer._AddEnv(newObj: pointer; newTypeof: pointer);
	begin
		_s2id.Add(PtrPair.Make(newObj, newTypeof), _nextSeId);
		inc(_nextSeId);
	end;

{$ifdef Debug}
	function Deserializer._HumanObject(id: uint): string;
	var
		typename: string;
	begin
		if (id <= _last) and Assigned(_ID2De(id)^.typ) then typename := _ID2De(id)^.typ^.name else typename := 'чёрт-знает-что';
		result := typename + ' #' + ToString(id);
	end;
{$endif}

	procedure Serializer._Init(newStream: pStream; const version: string);
	var
		i: sint;
	begin
		_db     := SerializationDB.Shared;
		_stream := MakeRef(newStream);
		_busy   := no;
		_seFirst := nil;
		_seLast  := nil;
		_s2id.Init;
		_nextSeId := 1;
		_after := nil;
		_nAfter := 0;
		_afterDelayed := no;
		ruined := no;

		Serialize_conststring(_stream, Signature + EOL + SerializerVersionPrefix + SerializerVersion + EOL);
		if version <> '' then Serialize_conststring(_stream, FileVersionPrefix + version + FileVersionSuffix + EOL);
		_verbose := Config.verbose;
		Serialize_ui8(_stream, uint(_verbose));

		_typeidFmt := UiBinaryFormatChooser.Choose(uint(max(High(_db^._tis), 0)) shl 1, 'индексов типов');
		_fnidFmt := UiBinaryFormatChooser.Choose(uint(max(High(_db^._fns), 0)), 'индексов функций');
		Serialize_ui8(_stream, bitpack([ord(_typeidFmt), 3, ord(_fnidFmt), 3]));
		Serialize_datetime(_stream, DateTime.GetLocal);

		_db^.Complete;
		for i := 0 to High(_db^._env) do
			with _db^._env[i] do
				_AddEnv(obj, _db^.ID2TypeInfo(tyid)^.typeof);
	end;

	constructor Serializer.Init(newStream: pStream; const version: string);
	begin
		_Init(newStream, version);
	end;

	constructor Serializer.Init(const newFn: string; const version: string);
	var
		strm: pStream;
	begin
		strm := GetStream(newFn, [file_Write]);
		try
			_Init(strm, version);
		except
			&File.Erase(newFn);
			raise;
		end;
	end;

	destructor Serializer.Done;
	begin
		_s2id.Done;
		Assert(not Assigned(_seFirst) or ruined);
		Release(_stream);
	end;

	procedure Serializer.ChangeStream(newStream: pStream);
	begin
		SetRef(_stream, newStream);
	end;

	procedure Serializer.SeObject(obj: pObject);
	begin
		if Assigned(obj) then
			SeObject(obj, TypeOf(obj^))
		else
			SeObject(nil, nil);
	end;

	procedure Serializer.SeObject(obj: pointer; ot: pointer);
	var
		id: uint;
	begin
		if not Assigned(obj) then
		begin
			VarInt.Write(_stream, 0);
		{$ifdef ExtDebug} Log('Записана нулевая ссылка', logDebug); {$endif}
			exit;
		end;

		id := _SeObject(obj, ot);
		VarInt.Write(_stream, id);
	{$ifdef ExtDebug} Log('Записан индекс объекта: ' + ToString(id), logDebug); {$endif}

		if not _busy then ProcessSe;
	end;

	procedure Serializer.SeFunction(fn: pointer);
	begin
		if Assigned(fn) then
			Serialize_ui(_stream, _db^.Function2ID(fn), _fnidFmt)
		else
			Serialize_ui(_stream, 0, _fnidFmt);
	end;

	procedure Serializer.DelayAfter;
	begin
		Assert(not _afterDelayed);
		_afterDelayed := yes;
	end;

	procedure Serializer.ContinueAfter;
	begin
		Assert(_afterDelayed);
		_afterDelayed := no;
		ProcessSe;
	end;

	procedure Deserializer.tDe.Initialize;
	begin
		obj := nil;
		typ := nil;
		flags := [];
		links := nil;
	end;

	procedure Deserializer.tDe.Finalize;
	begin
		if de_RFF in flags then Release(obj);
		links := nil;
	end;

	procedure Deserializer.tDe.Place(var des: Deserializer; newParent: uint; newOfs: PtrUint);
	var
		parent: pDe;
	begin
		if de_InPlace in flags then
			SerializationError {$ifdef Debug} ('объект ' + typ^.name + ' уже был зафиксирован in-place в другом месте') {$endif};
		Include(flags, de_InPlace);
		Assert(not Assigned(obj), 'Place: object shouldn''t be allocated here!'); // вообще-то может... если перестанет работать, верну memcpy в новый регион

		if newParent <> 0 then
		begin
			parent := des._ID2De(newParent);
			Assert(Assigned(parent^.typ), 'Place: parent should be allocated, i. e. its type required to be known!');
			obj := parent^.obj + newOfs;
		{$ifdef ExtDebug} Log('Объект встроен в данные ' + des._HumanObject(newParent) + ', +' + ToString(newOfs), logDebug); {$endif}
		end else
		begin
			obj := pPointer(@newOfs)^;
		{$ifdef ExtDebug} Log('Объект встроен не пойми куда (' + ToString(obj) + ')', logDebug); {$endif}
		end;
	end;

	procedure Deserializer.tDe.SetType(newTyp: pTypeInfo);
	begin
		Assert(not Assigned(typ) and (not Assigned(obj) or (de_InPlace in flags)));
		typ := newTyp;
		if (not Assigned(obj)) and not (de_RFF in flags) then
			obj := GetMem(typ^.instanceSize);
	end;

	procedure Deserializer.tDe.AddLink(newOfs: PtrUint; newTarget: uint; newStrong, newAbsolute: boolean);
	begin
		SetLength(links, length(links) + 1);
		with links[High(links)] do
		begin
			ofs := newOfs;
			target := newTarget;
			strong := newStrong;
			absolute := newAbsolute;
		end;
	end;

	procedure Deserializer.tDe.ResolveLinks(var des: Deserializer; to_: uint);
	var
		i: sint;
		link: pointer;
	begin
		if de_RFF in flags then exit;
		for i := 0 to High(links) do
		begin
			if (to_ <> 0) and (links[i].target <> to_) then continue;

			if links[i].absolute then
				link := pPointer(@links[i].ofs)^
			else
				link := obj + links[i].ofs;
			des._ResolveLink(link^, des._ID2De(links[i].target), links[i].strong, to_ <> 0);
		end;
	end;

	procedure Deserializer._AddEnv(newObj: pointer; newTypeof: pointer);
	var
		de: pDe;
	begin
		inc(_current);
		_EnsureLast(_current);
		de := _ID2De(_current);
		de^.obj := newObj;
		de^.typ := _db^.FindType(newTypeof);
		de^.flags += [de_Resolved];
	end;

	procedure Deserializer._Init(newStream: pStream; const version: string; justTry: boolean);
	var
		t: uint;
		i: sint;
	begin
		_db := SerializationDB.Shared;
		_stream := MakeRef(newStream);
		_de := nil;
		_current := 0;
		_afterStart := 0;
		_afterDelayed := no;
		_last := 0;
		Deserialize_signature(_stream, Signature, no);

		try
			Deserialize_signature(_stream, EOL + SerializerVersionPrefix + SerializerVersion + EOL, no);
			if version <> '' then Deserialize_signature(_stream, FileVersionPrefix + version + FileVersionSuffix + EOL, no);
		except
			raise VersionMismatch.Create('Несовместимая версия файла.');
		end;

		_verbose := Deserialize_ui8(_stream) <> 0;
		t := Deserialize_ui8(stream);
		_typeidFmt := UiBinaryFormat(RangeCheck(bits(t, 0, 3), ord(High(_typeidFmt)), 'Deserializer.TypeIDFmt'));
		_fnidFmt := UiBinaryFormat(RangeCheck(bits(t, 3, 3), ord(High(_fnidFmt)), 'Deserializer.FnIDFmt'));
		date := Deserialize_datetime(_stream);
		if justTry then exit;

		_db^.Complete;
		for i := 0 to High(_db^._env) do
			with _db^._env[i] do
				_AddEnv(obj, _db^.ID2TypeInfo(tyid)^.typeof);
	end;

	constructor Deserializer.Init(newStream: pStream; const version: string; justTry: boolean);
	begin
		_Init(newStream, version, justTry);
	end;

	constructor Deserializer.Init(const newFn: string; const version: string; justTry: boolean);
	begin
		_Init(GetStream(newFn), version, justTry);
	end;

	destructor Deserializer.Done;
	var
		i: sint;
	begin
		_ProcessDe;
		for i := 0 to sint(_last) - 1 do
			_de[i].Finalize;
		_de := nil;
		Release(_stream);
	end;

	procedure Deserializer._EnsureLast(newLast: uint);
	var
		nold, i: sint;
	begin
		if newLast < 1 then SerializationError {$ifdef Debug} ('неверный индекс: ' + ToString(newLast)) {$endif};

		if newLast > _last then
		begin
			nold := _last;
			_last := newLast;
			if _last > uint(length(_de)) then SetLength(_de, 2 * _last);
			for i := nold to _last - 1 do
				_de[i].Initialize;
		end;
	end;

	procedure Deserializer._ValidateIndex(id: uint);
	begin
		if (id < 1) or (id > _last) then
			SerializationError {$ifdef Debug} ('неверный индекс: ' + ToString(id)) {$endif};
	end;

	function Deserializer._ID2De(id: uint): pDe;
	begin
		_ValidateIndex(id);
		result := @_de[id - 1];
	end;

	procedure deserialize_r(de: pDeserializer; ty: pTypeInfo; obj: pointer);
	begin
		if Assigned(ty^.parent) then deserialize_r(de, ty^.parent, obj);
		if Assigned(ty^.deserialize) then ty^.deserialize(de, obj);
	end;

	procedure special_r(de: pDeserializer; ty: pTypeInfo; what: DeSpecial; var obj: pointer);
	begin
		if Assigned(ty^.parent) then special_r(de, ty^.parent, what, obj);
		if Assigned(ty^.deSpecial) then ty^.deSpecial(de, what, obj);
	end;

	procedure Deserializer._ProcessDe;
	var
		first, typeid, typeid_r, flags: uint;
		rff: boolean;
		de: pDe;
		ty: pTypeInfo;
		i, j: sint;
		rffStream: string;
		oo: pointer;
		sp: DeSpecial;
	begin
		_busy := yes;
		first := _current + 1;
		while _current < _last do
		begin
			inc(_current);
			de := _ID2De(_current);
		{$ifdef ExtDebug} Log('Читаю объект #' + ToString(_current), logDebug); {$endif}
			typeid_r := Deserialize_ui(_stream, _typeidFmt);
			if (typeid_r and TYPEID_HAS_FLAGS_BIT) <> 0 then flags := Deserialize_ui8(_stream) else flags := 0;
			typeid := typeid_r shr TYPEID_NFLAGS;
			rff := (flags and RFF_BIT) <> 0;

			ty := _db^.ID2TypeInfo(typeid);
		{$ifdef ExtDebug} Log('> Индекс типа: ' + ToString(typeid) + ' (' + ty^.name + ')', logDebug); {$endif}
			if _verbose then Deserialize_signature(_stream, EOL + VerbosePrefix + ty^.name + VerboseSuffix + EOL, no);
			if rff then
			begin
				rffStream := Paths.Data + Deserialize_string(_stream);
			{$ifdef ExtDebug} Log('Это ресурс-из-файла (' + StreamPath.Log(rffStream) + ')', logDebug); {$endif}
				Include(de^.flags, de_RFF);
				de^.SetType(ty);
				de^.obj := ResourcePool.Shared^.LoadRef(ty^.typeof, rffStream);
			end else
			begin
				de^.SetType(ty);
				if Assigned(ty^.deSpecial) then ty^.deSpecial(@self, de_Initialize, de^.obj);
				try
					deserialize_r(@self, ty, de^.obj);
				except
					raise Error('Не удалось прочитать объект ' + ty^.name + '; вероятно, данные повреждены.');
				end;

				de := _ID2De(_current); // _de: array of tDe мог реаллоцироваться. Лол.
				if ty^.refcounted and (de_InPlace in de^.flags) then
				begin
					if not Assigned(de^.obj) then SerializationError('Невозможно встроить нулевой объект в данные другого!');
					pObject(de^.obj)^.MakeStatic;
				end;
			end;
		end;
		_busy := no;

		for i := first - 1 to _last - 1 do
			if (not (de_RFF in _de[i].flags)) and _de[i].typ^.refcounted and Assigned(_de[i].typ^.deSpecial) and not (de_InPlace in _de[i].flags) then
				_de[i].typ^.deSpecial(@self, de_FixLink, _de[i].obj);
		for i := first - 1 to _last - 1 do
			_de[i].ResolveLinks(self, 0);

		if not _afterDelayed then
		begin
			for sp := de_After to de_After2 do
				for i := _last - 1 downto _afterStart do
				begin
					if de_RFF in _de[i].flags then continue;

					oo := _de[i].obj;
					special_r(@self, _de[i].typ, sp, _de[i].obj);
					if oo <> _de[i].obj then
					begin
						Assert(sp = de_After, 'After2+ cannot change link');
					{$ifdef Debug}
						Log('AfterDeserialization изменила ссылку на ' + _HumanObject(1 + i) + ' (' + ToString(oo) + ' -> ' + ToString(_de[i].obj) + ').', logDebug);
					{$endif}
						// Когда-нибудь это всё сломает, ага.
						// Сломало, лол =\ Собрался было выпиливать возможность замены объекта, но исправилось обходом в обратном порядке. Надолго ли?
						// Кстати, с этим должны сбиться ссылки на относительные in-place объекты, но я их не использую.
						// TODO: позволить явно указывать зависимости между объектами и выстраивать порядок AfterDeserialization согласно им.
						for j := _afterStart to _last - 1 do
							_de[j].ResolveLinks(self, 1 + i);
					end;
				end;
			_afterStart := _last;
		end;
	end;

	procedure Deserializer._ResolveLink(var link; target: pDe; strong, allowRelease: boolean);
	begin
		if allowRelease and strong and target^.typ^.refcounted then Release(pObject(link));
		pointer(link) := target^.obj;

		if strong then
		begin
			if not target^.typ^.refcounted then SerializationError {$ifdef Debug} ('сильная ссылка на объект без счётчика ссылок как такового (' + target^.typ^.name + ')') {$endif};
			MakeRef(pObject(link));
		end;
	end;

	procedure Deserializer._DeObject(out raw; inplace, strong, absolute: boolean);
	var
		de: pDe;
		id, parent: uint;
		ofs: PtrUint;
	begin
	{$ifdef Debug} if not _busy then Log('Загружаю невесть что из ' + StreamPath.Log(_stream^.path) + '...'); {$endif}
		id := VarInt.Read(_stream);
	{$ifdef ExtDebug} Log('> Индекс объекта: ' + ToString(id), logDebug); {$endif}

		if id = 0 then
		begin
			if inplace then
				SerializationError {$ifdef Debug} ('in-place объект невозможно десериализовать пустотой!') {$endif}
			else
				pointer(raw) := nil;
			exit;
		end;
		_EnsureLast(id);

		de := _ID2De(id);
		if de_Resolved in de^.flags then
		begin
			Assert(not inplace);
			_ResolveLink(raw, de, strong, no);
		{$ifdef ExtDebug} if _busy then Log('Ссылка на ' + _HumanObject(id) + ' разрешена немедленно', logDebug); {$endif}
			exit;
		end;

		Assert(absolute or _busy);
		if inplace then
		begin
			if absolute then
			begin
				parent := 0;
				ofs := pointer(@raw) - pointer(nil);
			end else
			begin
				parent := _current;
				ofs := pointer(@raw) - _ID2De(_current)^.obj;
			end;
			de^.Place(self, parent, ofs);
		end else
			if _busy then
			begin
				de := _ID2De(_current);
				if absolute then
					ofs := pointer(@raw) - pointer(nil)
				else
					ofs := pointer(@raw) - _ID2De(_current)^.obj;
				de^.AddLink(ofs, id, strong, absolute);

			{$ifdef ExtDebug}
				Log('Добавлена ссылка на ' + _HumanObject(id) + ' (' + LinkToStr(ofs, absolute) + ', strong = ' + BoolToStr[strong] + ')', logDebug);
			{$endif}
			end;

		if not _busy then
		begin
			_ProcessDe;
			if not inplace then _ResolveLink(raw, _ID2De(id), strong, no);
		end;
	end;

	procedure Deserializer.DeObjectA(out ref: pObject);
	begin
		_DeObject(ref, no, yes, yes);
	end;

	procedure Deserializer.DeObjectR(out ref: pObject);
	begin
		_DeObject(ref, no, yes, no);
	end;

	procedure Deserializer.DeWeakA(out ref: pointer);
	begin
		_DeObject(ref, no, no, yes);
	end;

	procedure Deserializer.DeWeakR(out ref: pointer);
	begin
		_DeObject(ref, no, no, no);
	end;

	procedure Deserializer.DeObjectAtA(out obj);
	begin
		_DeObject(obj, yes, yes, yes);
	end;

	procedure Deserializer.DeObjectAtR(out obj);
	begin
		_DeObject(obj, yes, yes, no);
	end;

	procedure Deserializer.DeWeakAtA(out obj);
	begin
		_DeObject(obj, yes, no, yes);
	end;

	procedure Deserializer.DeWeakAtR(out obj);
	begin
		_DeObject(obj, yes, no, no);
	end;

	procedure Deserializer.ChangeLink(old, typeof, new: pointer);
	var
		ty: pTypeInfo;
		i, j: sint;
	begin
		ty := _db^.FindType(typeof);
		for i := 0 to _last - 1 do
			if (_de[i].obj = old) and (_de[i].typ = ty) then
			begin
				_de[i].obj := new;
				for j := 0 to _last - 1 do
					_de[j].ResolveLinks(self, 1 + i);
				{$ifdef Debug} Log('Принудительно изменена ссылка на ' + _HumanObject(1 + i) + ' (' + ToString(old) + ' -> ' + ToString(_de[i].obj) + ').', logDebug); {$endif}
				exit;
			end;
		Assert(no, 'ChangeLink: no such link (typeof = ' + ty^.name + ')');
	end;

	function Deserializer.DeFunction: pointer;
	var
		id: uint;
	begin
		id := Deserialize_ui(_stream, _fnidFmt);
		if id <> 0 then
			result := _db^.ID2Function(id)
		else
			result := nil;
	end;

	procedure Deserializer.DelayAfter;
	begin
		Assert(not _afterDelayed);
		_afterDelayed := yes;
	end;

	procedure Deserializer.ContinueAfter;
	begin
		Assert(_afterDelayed);
		_afterDelayed := no;
		_ProcessDe;
	end;

end.
