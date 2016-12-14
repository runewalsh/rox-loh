{$include opts.inc}
unit rox_state_adventure;

interface

uses
	USystem, Errors, UMath, UClasses, Utils, rox_state, rox_gl, rox_ui, rox_actor;

type
	pAdventure = ^Adventure;
	Adventure = object(State)
		controls: set of Dir4.Enum;
		shift: boolean;
		player: pActor;
		constructor Init;
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;
		procedure HandleMouse(action: MouseAction; const pos: Vec2); virtual;
		procedure HandleKeyboard(action: KeyboardAction; key: KeyboardKey); virtual;
	const
		StateID = 'adventure';
	private
		function DirectionKeyToDir4(k: KeyboardKey): Dir4;
	end;

implementation

uses
	rox_state_mainmenu;

	constructor Adventure.Init;
	begin
		inherited Init(StateID);
		player := new(pActor, Init(Vec2.Make(0.12, 0.24), 'player.png', Vec2.Make(1/4, 1)))^.NewRef;
		player^.AddState('idle', Vec2.Make(0, 0), 1, 1, 0.0, 'idle', []);
		player^.AddState('walk', Vec2.Make(0, 0), 4, 1, 0.75, 'walk', [MovingState]);
		player^.pos := Vec2.Make(0.1);
	end;

	destructor Adventure.Done;
	begin
		Release(player);
		inherited Done;
	end;

	procedure Adventure.HandleUpdate(const dt: float);
	begin
		inherited HandleUpdate(dt);
		if controls <> [] then
		begin
			player^.SwitchToState('walk');
			player^.MoveBy(0.3 * Vec2.Make(sint(_Right in controls) - sint(_Left in controls), sint(_Up in controls) - sint(_Down in controls)).Normalized,
				IfThen(shift, 0.6, 0.25));
		end;
		player^.Update(dt);
	end;

	procedure Adventure.HandleDraw;
	begin
		inherited HandleDraw;
		player^.Draw(Vec2.Zero);
	end;

	procedure Adventure.HandleMouse(action: MouseAction; const pos: Vec2);
	begin
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
