{$include opts.inc}
unit rox_state;

interface

uses
	USystem, UMath, UClasses, Audio, Utils, GLUtils, rox_ui, rox_timer;

const
	PrettyTimeCycle = 4000.0;

type
	pStateManager = ^StateManager;

	pState = ^State;
	State = object
		mgr: pStateManager;
		id: string;
		timers: array of pTimer;
		constructor Init(const id: string);
		destructor Done; virtual;

		procedure HandleActivation; virtual;
		procedure HandleDeactivation; virtual;

		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;

		function QueryDeactivate: boolean; virtual;

		procedure HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra); virtual;
		procedure HandleKeyboard(action: KeyboardAction; key: KeyboardKey; var extra: HandlerExtra); virtual;
		function QuerySwitchOff: boolean; virtual;

		function AddTimer(const timeout: float; onProcess: Timer.ProcessCallback; const onDone: Timer.DoneCallback; param: pointer): pTimer;
	private
		procedure RemoveTimer(id: sint);
	public const
		MuteMainTheme = 'mute';
	end;

	StateManager = object
		win: pointer {pWindow};
		state: pState;
		switching: record
			&to: pState; // nil — не активно, в push мусор
			push: boolean;
		end;
		previous: array of pState;

		bgm: MusicPlayer;
		ui: UserInterface;
		nvp, invp: Vec2;
		viewportAp: AspectPair;
		switchedDuringLastUpdate: boolean;

		procedure Invalidate;
		procedure Verify;
		procedure Init(win: pointer);
		procedure Done;

		procedure Switch(another: pState);
		procedure Push(another: pState);
		procedure Pop;

		procedure Update(const dt: float);
		procedure Draw;

		procedure HandleViewportChange(const viewport: UintVec2);
		procedure HandleMouse(action: MouseAction; const pos: Vec2);
		procedure HandleKeyboard(action: KeyboardAction; key: KeyboardKey);
	private
		magic: array[0 .. 3] of char;
		procedure CheckCanSwitch;
		function InternalTrySwitch(another: pState; pushing: boolean): boolean;
		procedure ExternalSwitch(another: pState; pushing: boolean);
	const
		CorrectMagic = 'SMGR';
		IncorrectMagic = '!smg';
	end;

implementation

uses
	rox_win, rox_state_mainmenu;

	constructor State.Init(const id: string);
	begin
		self.mgr := nil;
		self.id := id;
	end;

	destructor State.Done;
	var
		i: sint;
	begin
		for i := High(timers) downto 0 do
			RemoveTimer(i);
	end;

	procedure State.HandleActivation;
	var
		priority: pModifiableValue;
	begin
		priority := mgr^.bgm.Priority(id, no);
		if Assigned(priority) then priority^.SetModifier('mgr', op_Add, +1, 0);
	end;

	procedure State.HandleDeactivation;
	var
		priority: pModifiableValue;
	begin
		priority := mgr^.bgm.Priority(id, no);
		if Assigned(priority) then
		begin
			priority^.RemoveModifier('mgr');
			priority^.RemoveModifier(MuteMainTheme, no);
		end;
	end;

	procedure State.HandleUpdate(const dt: float);
	var
		i: sint;
	begin
		for i := High(timers) downto 0 do
		begin
			timers[i]^.Update(dt);
			if timers[i]^.Dead then RemoveTimer(i);
		end;
	end;

	procedure State.HandleDraw; begin end;
	function State.QueryDeactivate: boolean; begin result := yes; end;
	procedure State.HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra); begin Assert((@action = @action) and (@pos = @pos) and (@extra = @extra)); end;
	procedure State.HandleKeyboard(action: KeyboardAction; key: KeyboardKey; var extra: HandlerExtra); begin Assert((@action = @action) and (@key = @key) and (@extra = @extra)); end;
	function State.QuerySwitchOff: boolean; begin result := yes; end;

	function State.AddTimer(const timeout: float; onProcess: Timer.ProcessCallback; const onDone: Timer.DoneCallback; param: pointer): pTimer;
	begin
		result := new(pTimer, Init(timeout, onProcess, onDone, param));
		SetLength(timers, length(timers) + 1);
		timers[High(timers)] := result^.NewRef;
	end;

	procedure State.RemoveTimer(id: sint);
	begin
		timers[id]^.Emergency;
		Release(timers[id]);
		timers[id] := timers[High(timers)];
		SetLength(timers, length(timers) - 1);
	end;

	procedure StateManager.Invalidate;
	begin
		state := pointer(@self);
		magic := IncorrectMagic;
	end;

	procedure StateManager.Verify;
	begin
		if not Assigned(@self) or (magic <> CorrectMagic) then raise Error('Менеджер состояний не валиден.');
	end;

	procedure StateManager.Init(win: pointer);
	begin
		Invalidate;
		magic := CorrectMagic;
		self.win := Window.FromPointer(win);
		state := nil;
		switching.&to := nil;
		previous := nil;
		viewportAp := AspectPair.Empty;
		bgm.Init;
		ui.Init(@self);

		state := new(pState, Init('IDLE'));
		state^.mgr := @self;
	end;

	procedure StateManager.Done;
	var
		i: sint;
	begin
		if state = pointer(@self) then exit;
		if Assigned(state) then begin dispose(state, Done); state := nil; end;
		if Assigned(switching.&to) then begin dispose(switching.&to, Done); switching.&to := nil; end;
		for i := 0 to High(previous) do dispose(previous[i], Done);
		ui.Done;
		bgm.Done;
		Invalidate;
	end;

	procedure StateManager.Switch(another: pState);
	begin
		ExternalSwitch(another, no);
	end;

	procedure StateManager.Push(another: pState);
	begin
		ExternalSwitch(another, yes);
	end;

	procedure StateManager.Pop;
	var
		top: pState;
	begin
		CheckCanSwitch;
		if length(previous) = 0 then
		begin
			Window(win^).RequestClose;
			exit;
		end;

		top := previous[High(previous)];
		SetLength(previous, length(previous) - 1);
		Switch(top);
	end;

	procedure StateManager.Update(const dt: float);
	begin
		switchedDuringLastUpdate := no;
		if Assigned(switching.&to) then
		begin
			switching.&to^.HandleUpdate(dt);
			if InternalTrySwitch(switching.&to, switching.push) then
			begin
				switching.&to := nil;
				exit;
			end;
		end;

		state^.HandleUpdate(dt);
		ui.Update(dt);
	end;

	procedure StateManager.Draw;
	begin
		state^.HandleDraw;
		ui.Draw;
	end;

	procedure StateManager.HandleViewportChange(const viewport: UintVec2);
	begin
		viewportAp := AspectPair.Make(viewport);
		nvp := viewportAp.Aspect2(asp2_min1, 1);
		invp := 1.0 / nvp;
	end;

	procedure StateManager.HandleMouse(action: MouseAction; const pos: Vec2);
	var
		x: HandlerExtra;
	begin
		x.SetupTry;
		ui.HandleMouse(action, pos * nvp, x);
		if x.WasHandled then
		begin
			x.SetupReal;
			ui.HandleMouse(action, pos * nvp, x);
		end else
		begin
			x.SetupReal;
			state^.HandleMouse(action, pos * nvp, x);
		end;
	end;

	procedure StateManager.HandleKeyboard(action: KeyboardAction; key: KeyboardKey);
	var
		x: HandlerExtra;
	begin
		x.SetupReal;
		case action of
			KeyClick:
				case key of
					key_NumPlus: if x.Handle then bgm.Rewind(+5);
					key_NumMinus: if x.Handle then bgm.Rewind(-5);
					key_Esc: if (state^.id <> MainMenu.StateID) and x.Handle then Switch(new(pMainMenu, Init));
				end;
		end;
		state^.HandleKeyboard(action, key, x);
	end;

	procedure StateManager.CheckCanSwitch;
	begin
		if Assigned(switching.&to) then raise Error('Уже выполняется переключение на {0}.', switching.&to^.id);
	end;

	function StateManager.InternalTrySwitch(another: pState; pushing: boolean): boolean;
	var
		id: string;
	begin
		result := no;
		id := state^.id;
		if pushing then
			if state^.QueryDeactivate then
			begin
				SetLength(previous, length(previous) + 1);
				previous[High(previous)] := state;
				result := yes;
			end else
		else
			if state^.QuerySwitchOff then
			begin
				dispose(state, Done);
				result := yes;
			end;

		if result then
		begin
			ui.RemoveGroup(id);
			state := another;
			another^.mgr := @self;
			another^.HandleActivation;
		end;
	end;

	procedure StateManager.ExternalSwitch(another: pState; pushing: boolean);
	var
		ok: boolean;
	begin
		try
			CheckCanSwitch;
			state^.HandleDeactivation;
			ok := InternalTrySwitch(another, pushing);
			switchedDuringLastUpdate := yes;
		except
			dispose(another, Done);
			raise;
		end;
		another^.HandleUpdate(0);

		if not ok then
		begin
			switching.&to := another;
			switching.push := pushing;
		end;
	end;

end.
