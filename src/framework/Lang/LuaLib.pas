{$include opts.inc}
unit LuaLib;

interface

uses
	ctypes, USystem, DynamicLoader;

type
	lua = class
	const
		MULTRET   = -1;
		FIRSTPSEUDOIDX = -1001000;
		REGISTRYINDEX = FIRSTPSEUDOIDX;

		RIDX_MAINTHREAD   = 1;
		RIDX_GLOBALS      = 2;
		RIDX_LAST         = RIDX_GLOBALS;

		OK        = 0;
		YIELDED   = 1;
		ERRRUN    = 2;
		ERRSYNTAX = 3;
		ERRMEM    = 4;
		ERRGCMM   = 5;
		ERRERR    = 6;

		IDSIZE = 60;

	type
		State = record
			p: pointer;
		end;

		CFunction = function(L: State): cint; cdecl;
		KContext = PtrInt;
		KFunction = function(L: State; status: cint; ctx: KContext): cint; cdecl;
		Chunkreader = function(L: State; ud: pointer; out sz: csize_t): PChar; cdecl;
		Chunkwriter = function(L: State; p: pointer; sz: csize_t; ud: pointer): cint; cdecl;
		Alloc = function(ud: pointer; ptr: pointer; osize, nsize: csize_t): pointer; cdecl;
		Throw = procedure; cdecl;
		PFunc = procedure(L: State; ud: pointer); cdecl;
		PCallf = function(f: PFunc; L: State; ud: pointer): cint; cdecl;

		Number = type double; pNumber = ^Number;
		Integer = {$ifdef CPU32} cint {$else} clonglong {$endif};

		Debug = record
			event: cint;
			name: PChar; // (n)
			namewhat: PChar; // (n) 'global', 'local', 'field', 'method'
			what: PChar; // (S) 'Lua', 'C', 'main', 'tail'
			source: PChar; // (S)
			currentline: cint; // (l)
			linedefined: cint; // (S)
			lastlinedefined: cint; // (S)
			nups: cuchar; // (u) number of upvalues
			nparams: cuchar; // (u) number of parameters
			isvararg: cschar; // (u)
			istailcall: cuchar; // (t)
			short_src: array[0 .. IDSIZE-1] of ansichar; // (S)
			&private: pointer;
		end;

		Hook = procedure(L: State; var ar: Debug); cdecl;

	const
		TNONE          = -1;
		TNIL           = 0;
		TBOOLEAN       = 1;
		TLIGHTUSERDATA = 2;
		TNUMBER        = 3;
		TSTRING        = 4;
		TTABLE         = 5;
		TFUNCTION      = 6;
		TUSERDATA      = 7;
		TTHREAD        = 8;

		GCSTOP        = 0;
		GCRESTART     = 1;
		GCCOLLECT     = 2;
		GCCOUNT       = 3;
		GCCOUNTB      = 4;
		GCSTEP        = 5;
		GCSETPAUSE    = 6;
		GCSETSTEPMUL  = 7;
		GCSETMAJORINC = 8;
		GCISRUNNING   = 9;
		GCGEN         = 10;
		GCINC         = 11;

		HOOKCALL     = 0;
		HOOKRET      = 1;
		HOOKLINE     = 2;
		HOOKCOUNT    = 3;
		HOOKTAILCALL = 4;

	class var
		newstate: function(f: Alloc; ud: pointer): State; cdecl;
		close: procedure(L: State); cdecl;
		version: function(L: State): pNumber; cdecl;
		atpanic: function(L: State; panicf: CFunction): CFunction; cdecl;
		onthrow: procedure(L: State; throwf: Throw; pcallf: PCallf); cdecl;

		gettop: function(L: State): cint; cdecl;
		settop: procedure(L: State; idx: cint); cdecl;
		pushvalue: procedure(L: State; idx: cint); cdecl;
		rotate: procedure(L: State; idx, n: cint); cdecl;
		copy: procedure(L: State; fromidx, toidx: cint); cdecl;
		absindex: function(L: State; idx: cint): cint; cdecl;
		checkstack: function(L: State; extra: cint): cint; cdecl;

		isnumber: function(L: State; idx: cint): LongBool; cdecl;
		isinteger: function(L: State; idx: cint): LongBool; cdecl;
		isuserdata: function(L: State; idx: cint): LongBool; cdecl;
		iscfunction: function(L: State; idx: cint): LongBool; cdecl;
		&type: function(L: State; idx: cint): cint; cdecl;
		typename: function(L: State; tp: cint): PChar; cdecl;
		tonumberx: function(L: State; idx: cint; isnum: pcint): Number; cdecl;
		tointegerx: function(L: State; idx: cint; isnum: pcint): cint; cdecl;
		toboolean: function(L: State; idx: cint): LongBool; cdecl;
		tolstring: function(L: State; idx: cint; len: pcsize_t): PChar; cdecl;
		touserdata: function(L: State; idx: cint): pointer; cdecl;
		tothread: function(L: State; idx: cint): State; cdecl;
		topointer: function(L: State; idx: cint): pointer; cdecl;
		rawlen: function(L: State; idx: cint): csize_t; cdecl;
		rawequal: function(l: State; index1, index2: cint): LongBool; cdecl;

		pushnil: procedure(L: State); cdecl;
		pushnumber: procedure(L: State; n: Number); cdecl;
		pushinteger: procedure(L: State; n: cint); cdecl;
		pushlstring: procedure(L: State; s: PChar; ls: csize_t); cdecl;
		pushcclosure: procedure(L: State; fn: CFunction; n: cint); cdecl;
		pushboolean: procedure(L: State; b: LongBool); cdecl;
		pushlightuserdata: procedure(L: State; p: pointer); cdecl;
		createtable: procedure(l: State; narr, nrec: cint); cdecl;
		newuserdata: function(L: State; sz: csize_t): pointer; cdecl;

		// gettable: procedure(L: State; idx: cint); cdecl;
		getfield: procedure(l: State; index: cint; k: pChar); cdecl;
		rawget: procedure(L: State; idx: cint); cdecl;
		rawgeti: procedure(L: State; idx, n: cint); cdecl;
		rawgetp: procedure(L: State; idxn: cint; p: pointer); cdecl;
		getmetatable: function(L: State; objindex: cint): cint; cdecl;
		getuservalue: procedure(L: State; index: cint); cdecl;

		// settable: procedure(L: State; idx: cint); cdecl;
		setfield: procedure(l: State; index: cint; k: pChar); cdecl;
		rawset: procedure(L: State; idx: cint); cdecl;
		rawseti: procedure(L: State; idx, n: cint); cdecl;
		rawsetp: procedure(L: State; idx: cint; p: pointer); cdecl;
		setmetatable: function(L: State; objindex: cint): LongBool; cdecl;
		setuservalue: procedure(L: State; index: cint); cdecl;

		callk: procedure(L: State; nargs, nresults: cint; ctx: KContext; k: KFunction); cdecl;
		pcallk: function(L: State; nargs, nresults, errfunc: cint; ctx: KContext; k: KFunction): cint; cdecl;

		load_: function(L: State; reader: Chunkreader; dt: pointer; chunkname, mode: PChar): cint; cdecl;
		dump_: function(L: State; writer: Chunkwriter; data: pointer; strip: cint): cint; cdecl;

		getupvalue: function(L: State; funcindex, n: cint): PChar; cdecl;
		setupvalue: function(L: State; funcindex, n: cint): PChar; cdecl;
		upvalueid: function(L: State; funcindex, n: cint): pointer; cdecl;
		upvaluejoin: procedure(L: State; fidx1, n1, fidx2, n2: cint); cdecl;

		error: function(L: State): cint; cdecl;
		next: function(L: State; idx: cint): cint; cdecl;
		gc: function(l: State; what: cint; data: cint): cint; cdecl;

		getstack: function(L: State; level: cint; out ar: Debug): cint; cdecl;
		getinfo: function(L: State; what: PChar; var ar: Debug): cint; cdecl;
		sethook: function(L: State; func: Hook; mask, count: cint): cint; cdecl;

		newthread: function(L: State): State; cdecl;
		resume: function(L: State; from: State; narg: cint): cint; cdecl;
		yieldk: function(L: State; nresults: cint; ctx: KContext; k: KFunction): cint; cdecl;
		xmove: procedure(from, to_: State; n: cint); cdecl;
		status: function(L: State): cint; cdecl;

		class procedure pop(L: State); cinline
		class procedure pop(L: State; n: cint); cinline
		class procedure remove(L: State; idx: cint); cinline
		class procedure insert(L: State; idx: cint); cinline
		class procedure replace(L: State; idx: cint); cinline
		class procedure pushcfunction(L: State; f: CFunction); cinline

		class function topchar(L: State; idx: cint): PChar; cinline
		class function tostring(L: State; idx: cint): string; reintroduce; cinline
		class function tonumber(L: State; idx: cint): Number; cinline
		class function isfunction(L: State; n: cint): Boolean; cinline
		class function istable(L: State; n: cint): Boolean; cinline
		class function islightuserdata(L: State; n: cint): Boolean; cinline
		class function isnil(L: State; n: cint): Boolean; cinline
		class function isboolean(L: State; n: cint): Boolean; cinline

		class procedure newtable(L: State); cinline

		class function yield(L: State; nresults: cint): cint; cinline
		class procedure call(L: State; nargs, nresults: cint); cinline
		class function pcall(L: State; nargs, nresults, errfunc: cint): cint; cinline

		class procedure pushstring(L: State; const s: string); cinline
		class function LoadString(ls: State; const parts: array of string; const namae: string; errmsg: pString = nil): boolean;
		class function Dump(ls: State; idx: sint; strip: boolean): string;

		class function DefaultAllocator: Alloc;
		class function DefaultThrowFunc: Throw;
		class function DefaultPCallFunc: PCallf;

	class var
		loader: DLLoader;
	end;

implementation

type
	InternalThrow = class end;

	class function lua.tostring(L: State; idx: cint): string;
	var
		ch: pchar;
		len: csize_t;
	begin
		ch := tolstring(L, idx, @len);
		Assert(Assigned(ch) or (len = 0));
		SetLength(result, len);
		System.Move(ch^, pointer(result)^, len);
	end;

	class function lua.topchar(L: State; idx: cint): PChar;
	begin
		Result := tolstring(L,idx,nil);
	end;

	class procedure lua.pop(L: State);
	begin
		settop(L, -2);
	end;

	class procedure lua.pop(L: State; n: cint);
	begin
		settop(L, -n - 1);
	end;

	class procedure lua.remove(L: State; idx: cint);
	begin
		rotate(L, idx, -1);
		settop(L, -2);
	end;

	class procedure lua.insert(L: State; idx: cint);
	begin
		rotate(L, idx, 1);
	end;

	class procedure lua.replace(L: State; idx: cint);
	begin
		copy(L, -1, idx);
		settop(L, -2);
	end;

	class procedure lua.pushcfunction(L: State; f: CFunction);
	begin
		pushcclosure(L, f, 0);
	end;

	class procedure lua.newtable(L: State);
	begin
		createtable(L, 0, 0);
	end;

	class function lua.isfunction(L: State; n: cint): Boolean;
	begin
		Result := &type(L, n) = TFUNCTION;
	end;

	class function lua.istable(L: State; n: cint): Boolean;
	begin
		Result := &type(L, n) = TTABLE;
	end;

	class function lua.islightuserdata(L: State; n: cint): Boolean;
	begin
		Result := &type(L, n) = TLIGHTUSERDATA;
	end;

	class function lua.isnil(L: State; n: cint): Boolean;
	begin
		Result := &type(L, n) = TNIL;
	end;

	class function lua.isboolean(L: State; n: cint): Boolean;
	begin
		Result := &type(L, n) = TBOOLEAN;
	end;

	class function lua.yield(L: State; nresults: cint): cint;
	begin
		result := yieldk(L, nresults, 0, nil);
	end;

	class procedure lua.call(L: State; nargs, nresults: cint);
	begin
		callk(L, nargs, nresults, 0, nil);
	end;

	class function lua.pcall(L: State; nargs, nresults, errfunc: cint): cint;
	begin
		result := pcallk(L, nargs, nresults, errfunc, 0, nil);
	end;

	class function lua.tonumber(L: State; idx: cint): Number;
	begin
		result := tonumberx(L, idx, nil);
	end;

	class procedure lua.pushstring(L: State; const s: string);
	begin
		pushlstring(L, pchar(s), length(s));
	end;

type
	ReaderRec = record
		parts: pString;
		next, total: sint;
	end;

	function Reader(ls: lua.State; ud: pointer; out sz: csize_t): PChar; cdecl;
	var
		r: ^ReaderRec absolute ud;
		cur: sint;
	begin
		Assert(@ls = @ls);
		while r^.next < r^.total do
		begin
			cur := r^.next;
			inc(r^.next);
			if length(r^.parts[cur]) > 0 then
			begin
				sz := length(r^.parts[cur]) * sizeof(r^.parts[cur, 1]);
				exit(pointer(r^.parts[cur]));
			end;
		end;

		sz := 0;
		result := nil;
	end;

	class function lua.LoadString(ls: State; const parts: array of string; const namae: string; errmsg: pString = nil): boolean;
	var
		rr: ReaderRec;
	begin
		rr.parts := pString(parts);
		rr.next := 0;
		rr.total := length(parts);
		result := load_(ls, @Reader, @rr, pChar('=' + namae), nil) = OK;
		if not result then
		begin
			if Assigned(errmsg) then errmsg^ := tostring(ls, -1);
			pop(ls);
		end;
	end;

type
	WriterParams = record
		s: pString;
		len: size_t;
	end;

	function Writer(ls: lua.State; p: pointer; sz: csize_t; ud: pointer): cint; cdecl;
	var
		newlen: size_t;
		etc: ^WriterParams absolute ud;
		s: pString;
	begin
		Assert(@ls = @ls);
		s := etc^.s;
		newlen := etc^.len + sz;
		if newlen > size_t(length(s^)) then SetLength(s^, 2 * newlen * sizeof(ansichar));
		memcpy(p, pointer(s^) + etc^.len, sz);
		etc^.len := newlen;
		result := 0;
	end;

	class function lua.Dump(ls: State; idx: sint; strip: boolean): string;
	var
		p: WriterParams;
	begin
		if idx <> -1 then pushvalue(ls, idx);
		result := '';
		p.s := @result;
		p.len := 0;
		if dump_(ls, @Writer, @p, cint(strip)) <> 0 then
		begin
			p.len := 0;
			Assert(no);
		end;
		SetLength(result, p.len);
		if idx <> -1 then pop(ls);
	end;

	function DefaultLuaAllocator(ud: pointer; ptr: pointer; osize, nsize: csize_t): pointer; cdecl;
	begin
		unused_args ud _ osize end_list
		result := ReallocMem(ptr, nsize);
	end;

	procedure DefaultLuaThrow; cdecl;
	begin
		raise InternalThrow.Create;
	end;

	class function lua.DefaultAllocator: Alloc;
	begin
		result := @DefaultLuaAllocator;
	end;

	class function lua.DefaultThrowFunc: Throw;
	begin
		result := @DefaultLuaThrow;
	end;

	function DefaultLuaPCall(f: lua.PFunc; L: lua.State; ud: pointer): cint; cdecl;
	begin
		try
			f(L, ud);
			result := 1;
		except
			on InternalThrow do result := 0;
		end;
	end;

	class function lua.DefaultPCallFunc: PCallf;
	begin
		result := @DefaultLuaPCall;
	end;

	procedure DescribeLuaFunctions(var fns: DLLoader.FunctionsList);
	begin
		fns
		.Func(@lua.newstate,          'newstate')^
		.Func(@lua.close,             'close')^
		.Func(@lua.version,           'version')^
		.Func(@lua.atpanic,           'atpanic')^
		.Func(@lua.onthrow,           'onthrow')^
		.Func(@lua.gettop,            'gettop')^
		.Func(@lua.settop,            'settop')^
		.Func(@lua.pushvalue,         'pushvalue')^
		.Func(@lua.rotate,            'rotate')^
		.Func(@lua.copy,              'copy')^
		.Func(@lua.absindex,          'absindex')^
		.Func(@lua.checkstack,        'checkstack')^
		.Func(@lua.isnumber,          'isnumber')^
		.Func(@lua.isinteger,         'isinteger')^
		.Func(@lua.isuserdata,        'isuserdata')^
		.Func(@lua.iscfunction,       'iscfunction')^
		.Func(@lua.&type,             'type')^
		.Func(@lua.typename,          'typename')^
		.Func(@lua.tonumberx,         'tonumberx')^
		.Func(@lua.tointegerx,        'tointegerx')^
		.Func(@lua.toboolean,         'toboolean')^
		.Func(@lua.tolstring,         'tolstring')^
		.Func(@lua.touserdata,        'touserdata')^
		.Func(@lua.tothread,          'tothread')^
		.Func(@lua.topointer,         'topointer')^
		.Func(@lua.rawlen,            'rawlen')^
		.Func(@lua.rawequal,          'rawequal')^
		.Func(@lua.pushnil,           'pushnil')^
		.Func(@lua.pushnumber,        'pushnumber')^
		.Func(@lua.pushinteger,       'pushinteger')^
		.Func(@lua.pushlstring,       'pushlstring')^
		.Func(@lua.pushcclosure,      'pushcclosure')^
		.Func(@lua.pushboolean,       'pushboolean')^
		.Func(@lua.pushlightuserdata, 'pushlightuserdata')^
		.Func(@lua.createtable,       'createtable')^
		.Func(@lua.newuserdata,       'newuserdata')^
	// .Func(@lua.gettable,          'gettable')^
		.Func(@lua.getfield,          'getfield')^
		.Func(@lua.rawget,            'rawget')^
		.Func(@lua.rawgeti,           'rawgeti')^
		.Func(@lua.rawgetp,           'rawgetp')^
		.Func(@lua.getmetatable,      'getmetatable')^
		.Func(@lua.getuservalue,      'getuservalue')^
	// .Func(@lua.settable,          'settable')^
		.Func(@lua.setfield,          'setfield')^
		.Func(@lua.rawset,            'rawset')^
		.Func(@lua.rawseti,           'rawseti')^
		.Func(@lua.rawsetp,           'rawsetp')^
		.Func(@lua.setmetatable,      'setmetatable')^
		.Func(@lua.setuservalue,      'setuservalue')^
		.Func(@lua.callk,             'callk')^
		.Func(@lua.pcallk,            'pcallk')^
		.Func(@lua.load_,             'load')^
		.Func(@lua.dump_,             'dump')^
		.Func(@lua.getupvalue,        'getupvalue')^
		.Func(@lua.setupvalue,        'setupvalue')^
		.Func(@lua.upvalueid,         'upvalueid')^
		.Func(@lua.upvaluejoin,       'upvaluejoin')^
		.Func(@lua.error,             'error')^
		.Func(@lua.next,              'next')^
		.Func(@lua.gc,                'gc')^
		.Func(@lua.getstack,          'getstack')^
		.Func(@lua.getinfo,           'getinfo')^
		.Func(@lua.sethook,           'sethook')^
		.Func(@lua.newthread,         'newthread')^
		.Func(@lua.resume,            'resume')^
		.Func(@lua.yieldk,            'yieldk')^
		.Func(@lua.xmove,             'xmove')^
		.Func(@lua.status,            'status');
	end;

	procedure Init;
	begin
		lua.loader.Init('lua(prefix = lua_)', @DescribeLuaFunctions);
	end;

	procedure Done;
	begin
		lua.loader.Done;
	end;

initialization
	&Unit('Lua').Initialize(@Init, @Done);
end.
