unit Input;

{$include opts.inc}

interface

uses
	USystem, Utils, UMath, UClasses;

type
	ButtonEvent = (button_Click, button_Press, button_Release);

const
	ButtonEventIds: array[ButtonEvent] of string = ('click', 'press', 'release');
	KeyboardKeyIds: array[KeyboardKey] of string =
	(
		'esc', 'lctrl', 'lalt', 'lshift', 'rctrl', 'ralt', 'rshift',
		'enter', 'tab', 'space', 'ins', 'del', 'home', 'end',
		'left', 'right', 'up', 'down',
		'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
		'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
		'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
		'~', '-', '=', '\', 'backspace',
		'[', ']', ':', '"', ',', '.', '/',
		'num+', 'num-', 'num*', 'num/',
		'num0', 'num1', 'num2', 'num3', 'num4', 'num5', 'num6', 'num7', 'num8', 'num9',
		'f1', 'f2', 'f3', 'f4', 'f5', 'f6', 'f7', 'f8', 'f9', 'f10', 'f11', 'f12',
		'alt-enter'
	);

type
	pKeyboardInput = ^KeyboardInput;
	KeyboardInput = object
	public type
		tOnKeyProc = procedure(key: KeyboardKey; ev: ButtonEvent; const info: SingleDelegateInfo);
		pOnKeyArgs = ^tOnKeyArgs;
		tOnKeyArgs = record
			key: KeyboardKey;
			ev: ButtonEvent;
		end;
	private var
		_pressedList: array of KeyboardKey;
	public var
		k: array[KeyboardKey] of boolean;
		keyEvents: array[KeyboardKey, ButtonEvent] of MultiDelegate;
		constructor Init;
		destructor Done;
		procedure PressKey(key: KeyboardKey);
		procedure ReleaseKey(key: KeyboardKey);
		procedure Update;
	end;

type
	MouseButton = (mouse_Left, mouse_Right, mouse_Middle);
	pMouseInput = ^MouseInput;
	MouseInput = object
	public type
		OnButtonProc = procedure(btn: MouseButton; ev: ButtonEvent; const info: SingleDelegateInfo);
		pOnButtonArgs = ^OnButtonArgs;
		OnButtonArgs = record
			btn: MouseButton;
			ev: ButtonEvent;
		end;
		OnMoveProc = procedure(const delta: Vec2; const info: SingleDelegateInfo);
		OnScrollProc = procedure(const delta: float; const info: SingleDelegateInfo);
	var
		buttonEvents: array[MouseButton, ButtonEvent] of MultiDelegate;
		onMove,
		onScroll: MultiDelegate;
		maxPos: UintVec2;
		constructor Init;
		destructor Done;
		procedure Update;
		procedure Move(const pos: UintVec2; silent: boolean = no);
		procedure ClickButton(btn: MouseButton);
		procedure ReleaseButton(btn: MouseButton);
		procedure Scroll(const delta: float);
	private
		_hasOld: boolean;
		_pos, _oldPos: UintVec2;
		_b: array[MouseButton] of boolean;
	public const
		ButtonIds: array[MouseButton] of string = ('L', 'R', 'M');
	end;

type
	pGamepadInput = ^GamepadInput;
	GamepadInput = object
	public type
		pStickState = ^StickState;
		StickState = Vec2;

		Button =
		(
			btn_Triangle, btn_Circle, btn_X, btn_Square,
			btn_LUp, btn_LDown, btn_RUp, btn_RDown,
			btn_Select, btn_Start,
			btn_LStick, btn_RStick,
			btn_Left, btn_Right, btn_Up, btn_Down,
			btn_LStickLeft, btn_LStickRight, btn_LStickUp, btn_LStickDown,
			btn_RStickLeft, btn_RStickRight, btn_RStickUp, btn_RStickDown
		);

		Stick = (LeftStick, RightStick);

		OnButtonProc = procedure(btn: Button; event: ButtonEvent; const info: SingleDelegateInfo);
		OnStickProcessProc = procedure(stk: Stick; const state: StickState; const info: SingleDelegateInfo);

	public const
		PseudoButtonThreshold = 0.9;
		ButtonIds: array[Button] of string =
		(
			'triangle', 'circle', 'X', 'square',
			'L-up', 'L-down', 'R-up', 'R-down',
			'select', 'start',
			'L-stick', 'R-stick',
			'left', 'right', 'up', 'down',
			'lstick.left', 'lstick.right', 'lstick.up', 'lstick.down',
			'rstick.left', 'rstick.right', 'rstick.up', 'rstick.down'
		);
		StickIds: array[Stick] of string = ('left', 'right');
		Stick2Buttons: array[Stick, 0 .. High(StickState.data), boolean] of Button =
		(
			// stick_L
			(
				(btn_LStickLeft, btn_LStickRight),
				(btn_LStickDown, btn_LStickUp)
			),
			// stick_R
			(
				(btn_RStickLeft, btn_RStickRight),
				(btn_RStickDown, btn_RStickUp)
			)
		);

	public var
		buttons: array[Button] of record
			events: array[ButtonEvent] of MultiDelegate;
		end;
		sticks: array[Stick] of record
			onProcess: MultiDelegate;
		end;

		constructor Init;
		destructor Done;
		procedure Update;
		procedure SetButtonState(btn: Button; state: boolean);
		procedure SetButtonState(id: sint; state: boolean);
		procedure SetStickState(stk: Stick; const state: StickState);

		procedure ResetMapping;
		procedure MapButton(id: sint; btn: Button);
		procedure MapButton(id: sint; const name: string);
		function DecryptButton(id: sint; out btn: Button): boolean;

	public type
		pOnStickProcessArgs = ^tOnStickProcessArgs;
		tOnStickProcessArgs = record
			stk: Stick;
			state: pStickState;
		end;

		pOnButtonArgs = ^tOnButtonArgs;
		tOnButtonArgs = record
			btn: Button;
			event: ButtonEvent;
		end;

	private
		_b: array[Button] of boolean;
		_s: array[Stick] of record
			active: boolean;
			state: StickState;
		end;
		_pressed: array of Button;
		_active: array of Stick;
		_bMap: array of record
			id: sint;
			btn: Button;
		end;

		procedure _CallOnStickProcess(stk: Stick; const state: StickState);
		procedure _CallOnButton(btn: Button; ev: ButtonEvent);
	end;

implementation

	procedure _CallOnKeyboardKey_impl(const info: SingleDelegateInfo; param: pointer);
	var
		args: KeyboardInput.pOnKeyArgs absolute param;
	begin
		with args^ do KeyboardInput.tOnKeyProc(info.proc)(key, ev, info);
	end;

	procedure _CallOnKeyboardKey(var proc: MultiDelegate; key: KeyboardKey; ev: ButtonEvent);
	var
		args: KeyboardInput.tOnKeyArgs;
	begin
		args.key := key;
		args.ev := ev;
		proc.Call(@_CallOnKeyboardKey_impl, @args);
	end;

	procedure _CallOnMouseButton_impl(const info: SingleDelegateInfo; param: pointer);
	var
		args: MouseInput.pOnButtonArgs absolute param;
	begin
		with args^ do MouseInput.OnButtonProc(info.proc)(btn, ev, info);
	end;

	procedure _CallOnMouseButton(var proc: MultiDelegate; button: MouseButton; ev: ButtonEvent);
	var
		args: MouseInput.OnButtonArgs;
	begin
		args.btn := button;
		args.ev := ev;
		proc.Call(@_CallOnMouseButton_impl, @args);
	end;

	constructor KeyboardInput.Init;
	var
		key: KeyboardKey;
		ev: ButtonEvent;
	begin
		for key in KeyboardKey do
		begin
			k[key] := no;
			for ev in ButtonEvent do
				keyEvents[key, ev].Init;
		end;
		_pressedList := nil;
	end;

	destructor KeyboardInput.Done;
	var
		key: KeyboardKey;
		ev: ButtonEvent;
	begin
		for key in KeyboardKey do
			for ev in ButtonEvent do
				keyEvents[key, ev].Done;
	end;

	procedure KeyboardInput.PressKey(key: KeyboardKey);
	begin
		if not k[key] then
		begin
			if not keyEvents[key, button_Press].Empty then
			begin
				SetLength(_pressedList, length(_pressedList) + 1);
				_pressedList[High(_pressedList)] := key;
			end;
			k[key] := yes;
		end;
		if not keyEvents[key, button_Click].Empty then
			_CallOnKeyboardKey(keyEvents[key, button_Click], key, button_Click);
	end;

	procedure KeyboardInput.ReleaseKey(key: KeyboardKey);
	var
		i: sint;
	begin
		if k[key] then
		begin
			for i := 0 to High(_pressedList) do
				if _pressedList[i] = key then
				begin
					_pressedList[i] := _pressedList[High(_pressedList)];
					SetLength(_pressedList, length(_pressedList) - 1);
					break;
				end;
			k[key] := no;
			if not keyEvents[key, button_Release].Empty then
				_CallOnKeyboardKey(keyEvents[key, button_Release], key, button_Release);
		end;
	end;

	procedure KeyboardInput.Update;
	var
		i: sint;
		key: KeyboardKey;
	begin
		for i := 0 to High(_pressedList) do
		begin
			key := _pressedList[i];
			if not keyEvents[key, button_Press].Empty then
				_CallOnKeyboardKey(keyEvents[key, button_Press], key, button_Press);
		end;
	end;

	constructor MouseInput.Init;
	var
		bt: MouseButton;
		bev: ButtonEvent;
	begin
		_hasOld := no;
		for bt in MouseButton do
		begin
			_b[bt] := no;
			for bev in ButtonEvent do
				buttonEvents[bt, bev].Init;
		end;
		onMove.Init;
		onScroll.Init;
	end;

	destructor MouseInput.Done;
	var
		bt: MouseButton;
		bev: ButtonEvent;
	begin
		for bt in MouseButton do
			for bev in ButtonEvent do
				buttonEvents[bt, bev].Done;
		onMove.Done;
		onScroll.Done;
	end;

	procedure _CallOnMove(const info: SingleDelegateInfo; param: pointer);
	var
		delta: pVec2 absolute param;
	begin
		MouseInput.OnMoveProc(info.proc)(delta^, info);
	end;

	procedure MouseInput.Update;
	var
		bt: MouseButton;
		deltaN: Vec2;
	begin
		if not _hasOld then
		begin
			_hasOld := yes;
			_oldPos := _pos;
		end;
		if (not onMove.Empty) and (_pos <> _oldPos) then
		begin
			deltaN := (IntVec2(_pos) - IntVec2(_oldPos)) / max(1, min(maxPos.x, maxPos.y));
			onMove.Call(@_CallOnMove, @deltaN);
		end;
		for bt in MouseButton do
		begin
			if _b[bt] and (not buttonEvents[bt, button_Press].Empty) then
				_CallOnMouseButton(buttonEvents[bt, button_Press], bt, button_Press);
		end;
	end;

	procedure MouseInput.Move(const pos: UintVec2; silent: boolean = no);
	begin
		_pos := pos;
		if silent then _oldPos := _pos;
	end;

	procedure MouseInput.ClickButton(btn: MouseButton);
	begin
		if not _b[btn] then
		begin
			_b[btn] := yes;
			if not buttonEvents[btn, button_Click].Empty then
				_CallOnMouseButton(buttonEvents[btn, button_Click], btn, button_Click);
		end;
	end;

	procedure MouseInput.ReleaseButton(btn: MouseButton);
	begin
		if _b[btn] then
		begin
			_b[btn] := no;
			if not buttonEvents[btn, button_Release].Empty then
				_CallOnMouseButton(buttonEvents[btn, button_Release], btn, button_Release);
		end;
	end;

	procedure _CallOnScroll(const info: SingleDelegateInfo; param: pointer);
	var
		delta: pFloat absolute param;
	begin
		MouseInput.OnScrollProc(info.proc)(delta^, info);
	end;

	procedure MouseInput.Scroll(const delta: float);
	begin
		if (not onScroll.Empty) and (delta <> 0.0) then
			onScroll.Call(@_CallOnScroll, @delta);
	end;

	constructor GamepadInput.Init;
	var
		b: Button;
		ev: ButtonEvent;
		s: Stick;
	begin
		for b in Button do
		begin
			for ev in ButtonEvent do
				buttons[b].events[ev].Init;
			_b[b] := no;
		end;

		for s in Stick do
		begin
			sticks[s].onProcess.Init;
			_s[s].state  := Vec2.Zero;
			_s[s].active := no;
		end;

		_pressed := nil;
		_active  := nil;
		ResetMapping;
	end;

	destructor GamepadInput.Done;
	var
		b: Button;
		ev: ButtonEvent;
		s: Stick;
	begin
		for b in Button do
		begin
			for ev in ButtonEvent do
				buttons[b].events[ev].Done;
		end;

		for s in Stick do
		begin
			sticks[s].onProcess.Done;
		end;
	end;

	procedure GamepadInput.Update;
	var
		i: sint;
	begin
		for i := 0 to High(_pressed) do
			_CallOnButton(_pressed[i], button_Press);
		for i := 0 to High(_active) do
			_CallOnStickProcess(_active[i], _s[_active[i]].state);
	end;

	procedure GamepadInput.SetButtonState(btn: Button; state: boolean);
	var
		i: sint;
	begin
		if _b[btn] = state then exit;
		_b[btn] := state;
		if state then
		begin
			SetLength(_pressed, length(_pressed) + 1);
			_pressed[High(_pressed)] := btn;

			_CallOnButton(btn, button_Click);
		end else
		begin
			for i := 0 to High(_pressed) do
				if _pressed[i] = btn then
				begin
					_pressed[i] := _pressed[High(_pressed)];
					SetLength(_pressed, length(_pressed) - 1);
					break;
				end;

			_CallOnButton(btn, button_Release);
		end;
	end;

	procedure GamepadInput.SetButtonState(id: sint; state: boolean);
	var
		btn: Button;
	begin
		if DecryptButton(id, btn) then SetButtonState(btn, state);
	end;

	procedure GamepadInput.SetStickState(stk: Stick; const state: StickState);
	var
		na: boolean;
		i: sint;
	begin
		if _s[stk].state = state then exit;
		_s[stk].state := state;

		na := not state.IsZero;
		if _s[stk].active <> na then
		begin
			_s[stk].active := na;
			if na then
			begin
				SetLength(_active, length(_active) + 1);
				_active[High(_active)] := stk;
			end else
			begin
				for i := 0 to High(_active) do
					if _active[i] = stk then
					begin
						_active[i] := _active[High(_active)];
						SetLength(_active, length(_active) - 1);
						break;
					end;
			end;
		end;

		for i := 0 to High(StickState.data) do
		begin
			SetButtonState(Stick2Buttons[stk, i, no], state.data[i] <= -PseudoButtonThreshold);
			SetButtonState(Stick2Buttons[stk, i, yes], state.data[i] >= PseudoButtonThreshold);
		end;
	end;

	procedure GamepadInput.ResetMapping;
	begin
		_bMap := nil;
	end;

	procedure GamepadInput.MapButton(id: sint; btn: GamepadInput.Button);
	begin
	{$ifdef Debug} Assert(Index(id, first_field _bMap _ id _, length(_bMap), sizeof(_bMap[0])) < 0, 'duplicate button mapping'); {$endif}
		SetLength(_bMap, length(_bMap) + 1);
		_bMap[High(_bMap)].id := id;
		_bMap[High(_bMap)].btn := btn;
	end;

	procedure GamepadInput.MapButton(id: sint; const name: string);
	var
		enum: sint;
	begin
		enum := FindStr(name, ButtonIds, length(ButtonIds));
		if enum < length(ButtonIds) then MapButton(id, Button(enum));
	end;

	function GamepadInput.DecryptButton(id: sint; out btn: GamepadInput.Button): boolean;
	var
		i: sint;
	begin
		i := Index(id, first_field _bMap _ id _, length(_bMap), sizeof(_bMap[0]));
		result := i >= 0;
		if result then btn := _bMap[i].btn;
	end;

	procedure _CallOnStickProcess_impl(const info: SingleDelegateInfo; param: pointer);
	var
		args: GamepadInput.pOnStickProcessArgs absolute param;
	begin
		with args^ do GamepadInput.OnStickProcessProc(info.proc)(stk, state^, info);
	end;

	procedure GamepadInput._CallOnStickProcess(stk: Stick; const state: StickState);
	var
		md: pMultiDelegate;
		args: tOnStickProcessArgs;
	begin
		md := @sticks[stk].onProcess;
		if md^.Empty then exit;

		args.stk := stk;
		args.state := @state;
		md^.Call(@_CallOnStickProcess_impl, @args);
	end;

	procedure _CallOnGamepadButton_impl(const info: SingleDelegateInfo; param: pointer);
	var
		args: GamepadInput.pOnButtonArgs absolute param;
	begin
		with args^ do GamepadInput.OnButtonProc(info.proc)(btn, event, info);
	end;

	procedure GamepadInput._CallOnButton(btn: Button; ev: ButtonEvent);
	var
		md: pMultiDelegate;
		args: tOnButtonArgs;
	begin
		md := @buttons[btn].events[ev];
		if md^.Empty then exit;

		args.btn := btn;
		args.event := ev;
		md^.Call(@_CallOnGamepadButton_impl, @args);
	end;

end.
