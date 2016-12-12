{$include opts.inc}
unit rox_state;

interface

uses
	USystem, UMath, UClasses, Audio, Utils, GLUtils, rox_ui;

const
	PrettyTimeCycle = 4000.0;

type
	pStateManager = ^StateManager;

	pState = ^State;
	State = object
		mgr: pStateManager;
		id: string;
		constructor Init(const id: string);
		destructor Done; virtual;

		procedure HandleActivation; virtual;

		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;

		function QueryDeactivate: boolean; virtual;
		procedure HandleReactivation; virtual;

		procedure HandleMouse(action: MouseAction; const pos: Vec2); virtual;
		function QuerySwitchOff: boolean; virtual;
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
	rox_win;

	constructor State.Init(const id: string);
	begin
		self.mgr := nil;
		self.id := id;
	end;

	destructor State.Done;
	begin
	end;

	procedure State.HandleActivation; begin end;
	procedure State.HandleUpdate(const dt: float); begin Assert(@dt = @dt); end;
	procedure State.HandleDraw; begin end;
	function State.QueryDeactivate: boolean; begin result := yes; end;
	procedure State.HandleReactivation; begin end;
	procedure State.HandleMouse(action: MouseAction; const pos: Vec2); begin Assert((@action = @action) and (@pos = @pos)); end;
	function State.QuerySwitchOff: boolean; begin result := yes; end;

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
		pWindow(win)^.Verify;
		self.win := win;
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
	begin
		ui.HandleMouse(action, pos * nvp);
		if not ui.mouseHandled then state^.HandleMouse(action, pos * nvp);
	end;

	procedure StateManager.CheckCanSwitch;
	begin
		if Assigned(switching.&to) then raise Error('Уже выполняется переключение на {0}.', switching.&to^.id);
	end;

	function StateManager.InternalTrySwitch(another: pState; pushing: boolean): boolean;
	var
		priority: pModifiableValue;
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
			bgm.ResetTheme(state^.id);
			priority := bgm.Priority(state^.id, no);
			if Assigned(priority) then priority^.SetModifier('mgr', op_Add, +1, 0);
			another^.HandleActivation;
		end;
	end;

	procedure StateManager.ExternalSwitch(another: pState; pushing: boolean);
	var
		priority: pModifiableValue;
		ok: boolean;
	begin
		try
			CheckCanSwitch;
			priority := bgm.Priority(state^.id, no);
			if Assigned(priority) then priority^.RemoveModifier('mgr');
			ok := InternalTrySwitch(another, pushing);
		except
			dispose(another, Done);
			raise;
		end;

		if not ok then
		begin
			switching.&to := another;
			switching.push := pushing;
		end;
	end;

end.