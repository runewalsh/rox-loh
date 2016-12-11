unit DynamicLoader;

{$include opts.inc}

interface

uses
	USystem, Utils;

type
	pDLLoader = ^DLLoader;
	DLLoader = object
	type
		ParaLoadHook = procedure(dl: pDLLoader);
		UnparaLoadHook = procedure;
		scoped_enum_ LoadHookKind = (NotSet, Parametrized, Unparametrized); _end
		LoadHook = record
      case kind: LoadHookKind of
			LoadHookKind.Parametrized:   (para: ParaLoadHook);
			LoadHookKind.Unparametrized: (unpara: UnparaLoadHook);
		end;

		FunctionDesc = record
			fn: pointer; {pCodePointer}
			namex: string;
		end;

		pFunctionsList = ^FunctionsList;
		FunctionsList = object
		type
			Callback = procedure(const namex: string; fieldPtr: pPointer; param: pointer);

			function Func(fieldPtr: pointer; const namex: string): pFunctionsList;
			function Funcs(const f: array of FunctionDesc): pFunctionsList;
		private
			cb: Callback;
			param: pointer;
			procedure Init(cb: Callback; param: pointer);
			procedure Done;
		end;

		GetFunctions = procedure(var fns: FunctionsList);

		scoped_enum_ FindFunctionInterface = (NotSet, Plain, Renaming); _end
		PlainFindFunction = function(const name: string): pointer;
		RenamingFindFunction = function(var name: string): pointer;
		FindFunction = record
		case kind: FindFunctionInterface of
			FindFunctionInterface.Plain: (plain: PlainFindFunction);
			FindFunctionInterface.Renaming: (renaming: RenamingFindFunction);
		end;

		pHookDesc = ^HookDesc;
		HookDesc = object
			afterLoad, beforeUnload, afterUnload: LoadHook;
			priority: sint;
			afterLoadSucceed: boolean;
		end;

		HookRef = object
			function AfterLoad(const fn: LoadHook): HookRef;
			function BeforeUnload(const fn: LoadHook): HookRef;
			function AfterUnload(const fn: LoadHook): HookRef;
		private
			h: pHookDesc;
		end;

	const
		SkipIfPresented = '/<';
		Optional        = '/?';
		LoadOptionIds: array[0 .. 1] of string = (SkipIfPresented, Optional);

	var
		procedure Init(const namex: string; getf: GetFunctions);
		procedure Init(const namex: string; getf: GetFunctions; const findf: FindFunction);
		procedure Done;
	{$define constructor_args := const namex: string; getf: GetFunctions}
	{$define constructor_args2 := const namex: string; getf: GetFunctions; const findf: FindFunction}
	{$define pSelf := pDLLoader} {$include dyn_obj.h.inc}
		procedure Load; // бросает исключение при неудаче
		procedure Unload;
		function HumanName(log: boolean): string;
		function HumanName: string;
		function LogName: string;
		function IsLoaded: boolean;

		// afterLoad может бросить исключение, в этом случае соответствующие before/afterUnload, а также последующие хуки, не будут вызваны.
		function Hook(priority: sint): HookRef;

	private type
		scoped_enum_ FlagEnum = (Permanent, Lock); _end
		FlagSet = set of FlagEnum;

	var
		flags: FlagSet;
		lib: string;      // Для DLL — аргумент Paths.DLL(), иначе просто имя.
		fnPrefix: string;
		loaded: sint;     // 0 — не загружена, >0 — загружена loaded раз, <0 — загрузка провалилась.
		dl: DynamicLibrary;
		getf: GetFunctions;
		findf: FindFunction; // если задана — полагается библиотекой с пользовательской загрузкой, иначе — DLL.
		hooks: array of HookDesc; // Отсортированы по убыванию приоритета (т. о. больше — раньше).
		loadLock: ThreadLock;
		heyUnloaded: pThreadCV;

		procedure ParseNamex(const namex: string; out libName, fnPrefix: string; out flags: FlagSet);
		function LoadProc(var name: string): pointer;
		procedure DoLoad;
		procedure DoUnload(zeroify: boolean);
	const
		Permanent = FlagEnum.Permanent;
		Lock      = FlagEnum.Lock;
	end;

	operator :=(fn: DLLoader.ParaLoadHook): DLLoader.LoadHook;
	operator :=(fn: DLLoader.UnparaLoadHook): DLLoader.LoadHook;
	operator :=(fn: DLLoader.PlainFindFunction): DLLoader.FindFunction;
	operator :=(fn: DLLoader.RenamingFindFunction): DLLoader.FindFunction;

implementation

uses
	Streams, TextProcessing {$ifdef Debug}, ULog{$endif};

	function DLLoader.HookRef.AfterLoad(const fn: LoadHook): HookRef;
	begin
		h^.afterLoad := fn;
		result := self;
   end;

	function DLLoader.HookRef.BeforeUnload(const fn: LoadHook): HookRef;
	begin
		h^.beforeUnload := fn;
		result := self;
   end;

	function DLLoader.HookRef.AfterUnload(const fn: LoadHook): HookRef;
	begin
		h^.afterUnload := fn;
		result := self;
   end;

   procedure DLLoader.Init(const namex: string; getf: GetFunctions);
   var
   	noFindf: FindFunction;
   begin
   	noFindf.kind := FindFunctionInterface.NotSet;
   	Init(namex, getf, noFindf);
   end;

	procedure DLLoader.Init(const namex: string; getf: GetFunctions; const findf: FindFunction);
	begin
		self.loaded   := 0;
		self.dl       := DynamicLibrary.Invalid;
		self.getf     := getf;
		self.findf    := findf;
		hooks         := nil;
		ParseNamex(namex, self.lib, self.fnPrefix, self.flags);
		if Lock in self.flags then begin loadLock.Init; heyUnloaded := nil; end;
	end;

	function AllUnloaded(param: pointer): boolean;
	var
		dl: pDLLoader absolute param;
	begin
		result := dl^.loaded <= 0 + ord(dl^.Permanent in dl^.flags);
	end;

	procedure DLLoader.Done;
	begin
		if Lock in flags then
		begin
			loadLock.Enter;
			if not AllUnloaded(@self) then
			begin
				heyUnloaded := ThreadCV.Create;
				heyUnloaded^.Wait(loadLock, @AllUnloaded, @self);
				heyUnloaded^.Free(heyUnloaded);
			end;
			loadLock.Leave;
			loadLock.Done;
			flags -= [Lock];
		end;

		if loaded > 0 then
		begin
			Assert(loaded = 1, HumanName);
			Assert(Permanent in flags, HumanName);
			DoUnload(no);
		end;
	end;

{$define constructor_args := const namex: string; getf: GetFunctions}
{$define constructor_args2 := const namex: string; getf: GetFunctions; const findf: FindFunction}
{$define pass_constructor_args := namex, getf}
{$define pass_constructor_args2 := namex, getf, findf}
{$define pSelf := pDLLoader} {$define classname := DLLoader} {$include dyn_obj.pp.inc}

	procedure DLLoader.Load;

		procedure Unlocked;
		begin
			if loaded > 0 then inc(loaded)
			else if loaded < 0 then raise Error(HumanName + ' не загрузилась ранее.')
			else
			begin
				try
					DoLoad;
					loaded := 1 + ord(Permanent in flags);
				except
					loaded := -1;
					raise;
				end;
			end;
		end;

		procedure Locked;
		begin
			loadLock.Enter;
			try
				Unlocked;
			finally
				loadLock.Leave;
			end;
		end;

	begin
		if Lock in flags then Locked else Unlocked;
	end;

	procedure DLLoader.Unload;

		procedure Unlocked;
		begin
			Assert(loaded > ord(Permanent in flags), 'Не сбалансированы Load/Unload');
			dec(loaded);
			if loaded = 0 then DoUnload(yes);
		end;

		procedure Locked;
		begin
			loadLock.Enter;
			try
				Unlocked;
				if Assigned(heyUnloaded) and AllUnloaded(@self) then heyUnloaded^.WakeAll;
			finally
				loadLock.Leave;
			end;
		end;

	begin
		if Lock in flags then Locked else Unlocked;
	end;

	function DLLoader.HumanName(log: boolean): string;
	begin
		if findf.kind = FindFunctionInterface.NotSet then
		begin
			result := Paths.DLL(lib);
			if log then result := StreamPath.Log(result) else result := StreamPath.Human(result);
		end else
			result := lib;
	end;

	function DLLoader.HumanName: string; begin result := HumanName(no); end;
	function DLLoader.LogName: string;   begin result := HumanName(yes); end;

	function DLLoader.IsLoaded: boolean;
	begin
		result := loaded > 0;
	end;

	function DLLoader.Hook(priority: sint): HookRef;
		{$define elem := HookDesc} {$define EmplaceInSorted} {$define less := _1 > _2}
		{$define key := sint} {$define get_key := elem.priority} {$include sorted.inc}
	begin
		Assert(loaded = 0);
		SetLength(hooks, length(hooks) + 1);
		result.h                       := EmplaceInSorted(pHookDesc(hooks), length(hooks) - 1, priority);
		result.h^.priority             := priority;
		result.h^.afterLoad.kind       := LoadHookKind.NotSet;
		result.h^.beforeUnload.kind    := LoadHookKind.NotSet;
		result.h^.afterUnload.kind     := LoadHookKind.NotSet;
		result.h^.afterLoadSucceed     := no;
	end;

	function DLLoader.FunctionsList.Func(fieldPtr: pointer; const namex: string): pFunctionsList;
	begin result := @self;
		cb(namex, fieldPtr, param);
	end;

	function DLLoader.FunctionsList.Funcs(const f: array of FunctionDesc): pFunctionsList;
	var
		i: sint;
	begin result := @self;
		for i := 0 to High(f) do
			cb(f[i].namex, f[i].fn, param);
	end;

	procedure DLLoader.FunctionsList.Init(cb: Callback; param: pointer);
	begin
		self.cb    := cb;
		self.param := param;
	end;

	procedure DLLoader.FunctionsList.Done;
	begin
	end;

	procedure DLLoader.ParseNamex(const namex: string; out libName, fnPrefix: string; out flags: FlagSet);
	var
		t: StringTokenizer;
		cp: t.Guard;
		id: string;
	begin
		t := namex;
		try
			libName  := t.ScanTokenEndingWith(['(']);
			fnPrefix := '';
			flags    := [];

			if t.Maybe('(') then
			begin
				while t.MaybeTokenEndingWith(id, ['=', ',', ')'], cp) do
				begin
					case id of
						'prefix': begin t.Expect('='); fnPrefix := t.ScanTokenEndingWith([',', ')']); end;
						'perm':   flags += [Permanent];
						'lock':   flags += [Lock];
						else raise t.UnknownIdentifier(cp);
					end;
					if t.Maybe(',') then else break;
				end;
				t.Expect(')');
			end;
		finally
			t.Done;
		end;		
	end;

	function DLLoader.LoadProc(var name: string): pointer;
	begin
		case findf.kind of
			FindFunctionInterface.NotSet:
				begin
					Assert(dl.OK);
					result := dl.FindProc(name);
				end;
			FindFunctionInterface.Plain: result := findf.plain(name);
			FindFunctionInterface.Renaming: result := findf.renaming(name);
		end;
	end;

type
	LoadContext = record
		self: pDLLoader;
		curFn: record mayFail, skip: boolean; end;
		ok: boolean;
	{$ifdef Debug} nOk, nTotal: uint; {$endif}
		lastFailed: string;
	end;

	procedure HandleLoadOption2(id: uint; const value: StringView; param: pointer);
	var
		ctx: ^LoadContext absolute param;

		function Presented(const proc: string): boolean;
		var
			name: string;
		begin
			name := ctx^.self^.fnPrefix + proc;
			result := Assigned(ctx^.self^.LoadProc(name));
		end;

	begin
		case id of
			0 {SkipIfPresented}: ctx^.curFn.skip    := ctx^.curFn.skip or Presented(value.ToString);
			1 {Optional}:        ctx^.curFn.mayFail := yes;
		end;
	end;

	procedure LoadFunction(const namex: string; fieldPtr: pPointer; param: pointer);

	{$ifdef Debug}
		function FromFinalName(const name: string; self: pDLLoader): string;
		begin
			result := CutPrefix(self^.fnPrefix, name);
		end;
	{$endif}

	var
		ctx: ^LoadContext absolute param;
		name: StringView;
		queryName, actualName: string;
	{$ifdef Debug}
		desc, comment: string;
		logStyle: LogMessageStyle;
	{$endif}
	begin
		ctx^.curFn.mayFail := no;
		ctx^.curFn.skip    := no;
		name := StringOptionals.Split(namex, DLLoader.LoadOptionIds, @HandleLoadOption2, ctx);
		if ctx^.curFn.skip then
		begin
		{$ifdef Debug} LogR('(({0})), ', name.ToString, logDisabled); {$endif}
			fieldPtr^ := nil;
			exit;
		end;

	{$ifdef Debug} inc(ctx^.nTotal); {$endif}
		queryName  := ctx^.self^.fnPrefix + name.ToString;
		actualName := queryName;
		fieldPtr^ := ctx^.self^.LoadProc(actualName);
		if (not Assigned(fieldPtr^)) and (not ctx^.curFn.mayFail) and ctx^.ok then
		begin
			ctx^.lastFailed := actualName;
			ctx^.ok := no;
		end;

	{$ifdef Debug}
		if Assigned(fieldPtr^) then inc(ctx^.nOk);
		if Assigned(fieldPtr^) then      begin logStyle := logOK; comment := ''; end
		else if ctx^.curFn.mayFail then  begin logStyle := logWarning; comment := ' — fail (неважно)'; end
		else                             begin logStyle := logError; comment := ' — fail'; end;

		desc := FromFinalName(queryName, ctx^.self);
		if actualName <> queryName then
		begin
			if logStyle = logOK then logStyle := logWarning;
			desc += ' → ' + FromFinalName(actualName, ctx^.self);
		end;
		LogR(desc + comment + ', ', logStyle);
	{$endif}
	end;

	procedure DLLoader.DoLoad;
	var
		fns: FunctionsList;
		i: sint;
		ctx: LoadContext;
	begin
	{$ifdef Debug} LogR('{0}: DLLoader ', LogName, logOK); {$endif}
		if findf.kind = FindFunctionInterface.NotSet then
		begin
			Assert(not dl.OK);
		{$ifdef Debug} LogR('Загрузка {0}... ', LogName); {$endif}
			DynamicLibrary.Open(dl, Paths.DLL(lib));
		end;

		ctx.self   := @self;
      ctx.ok     := yes;
	{$ifdef Debug}
		ctx.nOk    := 0;
		ctx.nTotal := 0;
	{$endif}
		ctx.lastFailed := '';

		fns.Init(@LoadFunction, @ctx);
		getf(fns);

	{$ifdef Debug}
		if ctx.ok then
			if ctx.nOk = ctx.nTotal then Log('{0} загружена', LogName, logOK) else
				if ctx.nOk = 0 then Log('Попытка загрузить {0} - полный фейл', LogName, logError) else
					Log('{0}: загружено {1}/{2}', [LogName, ctx.nOk, ctx.nTotal], logWarning)
		else
			Log('Загрузка ' + LogName + ': технический фейл', logError);
	{$endif}
		fns.Done;

		try
			if not ctx.ok then raise Error('Не удалось импортировать нужные функции {0} ({1}).', HumanName, ctx.lastFailed);

			for i := 0 to High(hooks) do
			begin
				case hooks[i].afterLoad.kind of
					LoadHookKind.Parametrized: hooks[i].afterLoad.para(@self);
					LoadHookKind.Unparametrized: hooks[i].afterLoad.unpara();
				end;
	         hooks[i].afterLoadSucceed := yes;
	      end;
		except
			DoUnload(yes);
			raise;
		end;
	end;

	procedure ZeroifyFunction(const namex: string; fieldPtr: pPointer; param: pointer);
	begin
		unused_args namex _ param end_list
		fieldPtr^ := nil;
	end;

	procedure DLLoader.DoUnload(zeroify: boolean);
	var
		fns: FunctionsList;
		i: sint;
	begin
		for i := High(hooks) downto 0 do
			if hooks[i].afterLoadSucceed then
				case hooks[i].beforeUnload.kind of
					LoadHookKind.Parametrized: hooks[i].beforeUnload.para(@self);
					LoadHookKind.Unparametrized: hooks[i].beforeUnload.unpara();
				end;

		if zeroify then
		begin
			fns.Init(@ZeroifyFunction, nil);
			getf(fns);
			fns.Done;
		end;

		if findf.kind = FindFunctionInterface.NotSet then
		begin
			Assert(dl.OK);
		{$ifdef Debug} LogR('Выгрузка {0}... ', LogName); {$endif}
			dl.Close;
		{$ifdef Debug} Log('{0} выгружена', LogName, logOK); {$endif}
		end;

		for i := 0 to High(hooks) do
			if hooks[i].afterLoadSucceed then
			begin
				case hooks[i].afterUnload.kind of
					LoadHookKind.Parametrized: hooks[i].afterUnload.para(@self);
					LoadHookKind.Unparametrized: hooks[i].afterUnload.unpara();
				end;
				hooks[i].afterLoadSucceed := no;
			end;
	end;

	operator :=(fn: DLLoader.ParaLoadHook): DLLoader.LoadHook; begin result.kind := DLLoader.LoadHookKind.Parametrized; result.para := fn; end;
	operator :=(fn: DLLoader.UnparaLoadHook): DLLoader.LoadHook; begin result.kind := DLLoader.LoadHookKind.Unparametrized; result.unpara := fn; end;
	operator :=(fn: DLLoader.PlainFindFunction): DLLoader.FindFunction; begin result.kind := DLLoader.FindFunctionInterface.Plain; result.plain := fn; end;
	operator :=(fn: DLLoader.RenamingFindFunction): DLLoader.FindFunction; begin result.kind := DLLoader.FindFunctionInterface.Renaming; result.renaming := fn; end;

end.
