unit Profile;

{$include opts.inc}
{$ifndef Profile} {$error don't include this} {$endif}

interface

uses
	USystem, Algo, UMath, Utils, ULog, Streams;

	procedure TraceCall(const name: string);
	procedure LeaveCall;

implementation

type
	pFuncInfo = ^FuncInfo;
	FuncInfo = object
		name: string;
		time: Ticks;
		ncalls: uint;
		procedure Init(const newName: string);
		procedure Done; {$define pSelf := pFuncInfo} {$define constructor_args := const newName: string} {$include dyn_obj.h.inc}
	end;

	procedure FuncInfo.Init(const newName: string);
	begin
		name := newName;
		time := Ticks.Zero;
		ncalls := 0;
	end;

	procedure FuncInfo.Done;
	begin
	end;
{$define constructor_args := const newName: string} {$define pass_constructor_args := newName}
{$define classname := FuncInfo} {$define pSelf := pFuncInfo} {$include dyn_obj.pp.inc}

type
	{$define classname:=Addr2FuncInfo} {$define key_type:=pointer} {$define value_type:=pFuncInfo} {$define null_value := nil}
	{$include hash.h.inc}

	{$define classname:=Addr2FuncInfo} {$define hash_func:=Hash.OfPointer} {$define finalize_value := _1^.Free}
	{$include hash.pp.inc}

type
	CallProfiler = object
	private type
		pStackRec = ^tStackRec;
		tStackRec = record
			func: pFuncInfo;
			addr: pointer;
			startTime: Ticks;
		end;

		pStack = ^tStack;
		tStack = record
			items: array of tStackRec;
			n: sint;
		end;
	private var
		_fnid: Addr2FuncInfo;
		_stacks: array of record
			thread: Thread.ID;
			stack: tStack;
		end;
		_lock: ThreadLock;
		function _ForceStack: pStack;
		procedure _KillStack;
	public
		constructor Init;
		destructor Done;
		procedure Call(const name: string);
		procedure Leave;
		procedure Dump(const filename: string);
	end;

	constructor CallProfiler.Init;
	begin
		_fnid.Init;
		_stacks := nil;
		_lock.Init;
	end;

	destructor CallProfiler.Done;
	begin
		_lock.Done;
		Assert(length(_stacks) = 0);
		_fnid.Done;
	end;

	function CallProfiler._ForceStack: pStack;
	var
		th: Thread.ID;
		i: sint;
	begin
		th := Thread.Current;
		for i := 0 to High(_stacks) do
			if _stacks[i].thread = th then
				exit(@_stacks[i].stack);

		SetLength(_stacks, length(_stacks) + 1);
		_stacks[High(_stacks)].thread := th;
		result := @_stacks[High(_stacks)].stack;
		result^.n := 0;
		result^.items := nil;
	end;

	procedure CallProfiler._KillStack;
	var
		th: Thread.ID;
		i: sint;
	begin
		th := Thread.Current;
		for i := 0 to High(_stacks) do
			if _stacks[i].thread = th then
			begin
				_stacks[i] := _stacks[High(_stacks)];
				SetLength(_stacks, length(_stacks) - 1);
				exit;
			end;
		Assert(no);
	end;

	procedure CallProfiler.Call(const name: string);
	var
		st: pStack;
		stk: pStackRec;
		time, dt: Ticks;
	begin
		time := Ticks.Get;
		_lock.Enter;
		st := _ForceStack;

		if st^.n > 0 then
		begin
			stk := @st^.items[st^.n - 1];
			dt := time - stk^.startTime;
			stk^.func^.time += dt;
		end;

		inc(st^.n);
		if st^.n > length(st^.items) then SetLength(st^.items, 2 * st^.n);
		stk := @st^.items[st^.n - 1];
		stk^.addr := get_caller_addr(get_caller_frame(get_frame));
		stk^.func := _fnid.Find(stk^.addr);
		if not Assigned(stk^.func) then
		begin
			stk^.func := FuncInfo.Create(name);
			_fnid.Add(stk^.addr, stk^.func);
		end;
		inc(stk^.func^.ncalls);
		stk^.startTime := Ticks.Get;
		_lock.Leave;
	end;

	procedure CallProfiler.Leave;
	var
		st: pStack;
		stk: pStackRec;
		time, dt: Ticks;
	begin
		time := Ticks.Get;
		_lock.Enter;
		st := _ForceStack;

		stk := @st^.items[st^.n - 1];
		dt := time - stk^.startTime;
		stk^.func^.time += dt;
		dec(st^.n);
		if st^.n > 0 then
		begin
			stk := @st^.items[st^.n - 1];
			stk^.startTime := Ticks.Get;
		end else
			_KillStack;
		_lock.Leave;
	end;

{$define procname:=sort_by_time} {$define elem:=pFuncInfo} {$define less := _1^.time > _2^.time} {$define openarray}
{$include sort.inc}

	procedure CallProfiler.Dump(const filename: string);
	label _finally_;
	var
		f: &File;
		it: Addr2FuncInfo.Iterator;
		func: pFuncInfo;
		s: array[0 .. 1] of shortstring;
		fns: array of pFuncInfo;
		i, n: sint;
		sumTime: Ticks;
		invSumTime100: hp_float;
	begin
		_lock.Enter;
		SetLength(fns, _fnid.Count);
		n := 0;

		sumTime := Ticks.Zero;
		it := _fnid.GetIterator;
		while _fnid.Next(it) do
		begin
			fns[n] := _fnid.GetValue(it)^;
			sumTime += fns[n]^.time;
			inc(n);
		end;
		sort_by_time(fns);

		invSumTime100 := sumTime.ToSeconds;
		if NotZero(invSumTime100) then invSumTime100 := 100.0 / invSumTime100;
		try2
			try
				// TODO: &File.Open(out file) как у BlobsCache, чтобы можно было гарантировать невалидность.
				f := &File.Open(filename, [file_Write]);
			except
				f := &File.Invalid;
				raise;
			end;
			for i := 0 to High(fns) do
			begin
				s[0] := '';
				s[1] := '';
				func := fns[i];
				func^.time -= func^.time.Overhead(4 * func^.ncalls);
				WriteStr(s[0], func^.name, ' (', func^.ncalls, '): ');
				WriteStr(s[1], func^.time.ToSeconds:0:4, 's (', func^.time.ToSeconds * invSumTime100:0:1, '%)', EOL);
				f.Write(string(s[0]) + StrDup(' ', 40 - length(s[0])) + s[1]);
			end;
		finally2
			f.Close;
		except2
			Exception.Show;
		end;
	_finally_:
		_lock.Leave;
	end;

var
	cp: CallProfiler;

	procedure TraceCall(const name: string);
	begin
		cp.Call(name);
	end;

	procedure LeaveCall;
	begin
		cp.Leave;
	end;

	procedure Init;
	begin
		cp.Init;
	end;

	procedure Done;
	begin
		cp.Dump(Paths.Logs + StreamPath.FilenameNoExt(ExecFileName) + '-' + DateTime.Start.ToCode + '.profile');
		cp.Done;
	end;

initialization
	&Unit('Profile').Initialize(@Init, @Done).Priority(+4);
end.
