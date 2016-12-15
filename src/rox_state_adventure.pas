{$include opts.inc}
unit rox_state_adventure;

interface

uses
	USystem, Errors, UMath, UClasses, Utils, rox_state, rox_gl, rox_ui, rox_actor, rox_location, rox_decoration, rox_dialogue, rox_paths;

type
	pAdventure = ^Adventure;
	Adventure = object(State)
		controls: set of Dir4.Enum;
		shift: boolean;
		view: Transform2;
		player: pActor;
		location: pLocation;
		textBox: pTextBox;
		state: (Setup, WatchLimits, Idle);
		constructor Init(location: pLocation);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;
		procedure HandleMouse(action: MouseAction; const pos: Vec2); virtual;
		procedure HandleKeyboard(action: KeyboardAction; key: KeyboardKey); virtual;
	const
		StateID = 'adventure';
		DialogueBorder = 0.03;
		RunningVelocity = 0.8;
		WalkingVelocity = 0.3;
	private
		function DirectionKeyToDir4(k: KeyboardKey): Dir4;
	end;

implementation

uses
	rox_state_mainmenu;

	constructor Adventure.Init(location: pLocation);
	var
		d: pDecoration;
	begin
		self.location := MakeRef(location);
		inherited Init(StateID);
		view := Transform2.Identity;
		player := new(pActor, Init(Vec2.Make(0.14, 0.28), Character('player', 'model.png'), Vec2.Make(1/4, 1/8)))^.NewRef;
		player^.AddState('idle', Vec2.Make(0, 0), 4, 8, 0.0, 'idle', []);
		player^.AddState('walk', Vec2.Make(0, 0), 4, 8, 0.6, 'walk', [MovingState]);

		if not Assigned(self.location) then
			self.location := new(pLocation, Init)^.NewRef;

		self.location^.AddWall(new(pDecoration, Init(Environment('bar_door.png'), Translate2(1, 0), Vec2.Make(0.3, 0.3/1*1.3))), Vec2.Zero, Vec2.Make(0, 0.2/1*1.3));

		d := new(pDecoration, Init(Environment('brick.png'), Translate2(0, 0.02), Vec2.Make(1.5, 0.3)))^.NewRef;
		try
			d^.texRect := Rect.Make(Vec2.Zero, Vec2.Make(5, 1));
			self.location^.AddWall(d, Vec2.Zero, Vec2.Make(0, 0.2/1*1.3));
		finally
			Release(d);
		end;

		textBox := new(pTextBox, Init(Dialogue('player', '0.png'), Face('player', 'indifferent.png')))^.NewRef;
		state := Setup;
	end;

	destructor Adventure.Done;
	begin
		Release(textBox);
		Release(player);
		Release(location);
		inherited Done;
	end;

	procedure ResetLimitsWhenPlayerWalksIn(reason: Actor.MoveCallbackReason; ac: pActor; param: pointer);
	begin
		unused_args reason _ ac end_list
		pAdventure(param)^.location^.limits.A := -pAdventure(param)^.mgr^.nvp;
		pAdventure(param)^.state := Idle;
	end;

	procedure Adventure.HandleUpdate(const dt: float);
	begin
		inherited HandleUpdate(dt);
		case state of
			Setup:
				begin
					location^.limits := Rect.Make(-mgr^.nvp - Vec2.Make(0.4, 0), mgr^.nvp);
					textBox^.size := mgr^.nvp.x - 2*DialogueBorder;
					textBox^.local.trans := -mgr^.nvp + Vec2.Make(DialogueBorder);
					textBox^.local.trans.y := max(textBox^.local.trans.y, -mgr^.nvp.y + DialogueBorder - textBox^.CalculateRawSize.y + 0.3);
					mgr^.ui.Add(textBox^.NewRef, id);

					player^.local.trans := Vec2.Make(location^.limits.A.x, -0.3);
					player^.SwitchToState('walk');
					player^.MoveTo(Vec2.Make(-0.2, -0.3), WalkingVelocity, @ResetLimitsWhenPlayerWalksIn, @self);
					self.location^.Add(player);
					state := WatchLimits;
				end;
		end;

		if controls <> [] then
		begin
			if state = WatchLimits then
			begin
				ResetLimitsWhenPlayerWalksIn(MovingCanceled, player, @self);
				state := Idle;
			end;
			player^.SwitchToState('walk');
			player^.rtMethod := NotRotating;
			player^.MoveBy(0.3 * Vec2.Make(sint(_Right in controls) - sint(_Left in controls), sint(_Up in controls) - sint(_Down in controls)).Normalized,
				IfThen(shift, RunningVelocity, WalkingVelocity));
		end;
		location^.Update(dt);
	end;

	procedure Adventure.HandleDraw;
	begin
		inherited HandleDraw;
		location^.Draw(view);
	end;

	procedure Adventure.HandleMouse(action: MouseAction; const pos: Vec2);
	begin
		case action of
			MouseLClick:
				begin
					player^.rtMethod := NotRotating;
					player^.SwitchToState('walk');
					player^.MoveTo(view.Inversed * pos, WalkingVelocity, nil, nil);
				end;
			MouseMove: if player^.mvMethod = NotMoving then player^.RotateTo(view.Inversed * pos);
		end;
		inherited HandleMouse(action, pos);
	end;

	procedure Adventure.HandleKeyboard(action: KeyboardAction; key: KeyboardKey);
	var
		handled: boolean;
	begin
		handled := no;
		case action of
			KeyClick:
				case key of
					key_Up, key_Down, key_Left, key_Right: controls += [DirectionKeyToDir4(key).value];
					key_LShift: shift := yes;
					key_Esc: begin mgr^.Switch(new(pMainMenu, Init)); handled := yes; end;
				end;
			KeyRelease:
				case key of
					key_Up, key_Down, key_Left, key_Right: controls -= [DirectionKeyToDir4(key).value];
					key_LShift: shift := no;
				end;
		end;
		if not handled then inherited HandleKeyboard(action, key);
	end;

	function Adventure.DirectionKeyToDir4(k: KeyboardKey): Dir4;
	begin
		case k of
			key_Up: result := Dir4.Up;
			key_Down: result := Dir4.Down;
			key_Left: result := Dir4.Left;
			key_Right: result := Dir4.Right;
			else raise ExhaustiveCase(ord(k), 'DirKey');
		end;
	end;

end.
