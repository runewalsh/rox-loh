{$include opts.inc}
unit rox_state_adventure;

interface

uses
	USystem, Errors, UMath, UClasses, Utils, rox_state, rox_gl, rox_ui, rox_actor, rox_location, rox_dialogue, rox_trigger, rox_win;

type
	pAdventure = ^Adventure;
	Adventure = object(State)
		controls: set of Dir4.Enum;
		shift: boolean;
		view: Transform2;
		player: pActor;
		location: pLocation;
		dlg: Dialogue;
		mouseOverTrigger: boolean;
		constructor Init(const id: string);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;
		procedure HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra); virtual;
		procedure HandleKeyboard(action: KeyboardAction; key: KeyboardKey); virtual;

	const
		RunningVelocity = 0.8;
		WalkingVelocity = 0.3;
	private
		function DirectionKeyToDir4(k: KeyboardKey): Dir4;
	end;

implementation

uses
	rox_state_mainmenu;

	constructor Adventure.Init(const id: string);
	begin
		dlg.Invalidate;
		inherited Init(id);
		view := Transform2.Identity;
	end;

	destructor Adventure.Done;
	begin
		Release(player);
		Release(location);
		dlg.Done;
		inherited Done;
	end;

	procedure Adventure.HandleUpdate(const dt: float);
	begin
		if not Assigned(player) then raise Error('Не назначен игрок.');
		if not Assigned(location) then raise Error('Не назначена локация.');
		inherited HandleUpdate(dt);
		if controls <> [] then
		begin
			player^.SwitchToState('walk');
			player^.rtMethod := NotRotating;
			player^.MoveBy(0.3 * Vec2.Make(sint(_Right in controls) - sint(_Left in controls), sint(_Up in controls) - sint(_Down in controls)).Normalized,
				IfThen(shift, RunningVelocity, WalkingVelocity));
		end;
		location^.Update(dt);
		if dlg.Valid then
		begin
			dlg.Update(dt);
			if dlg.Finished then begin dlg.Done; dlg.Init(@self, 'player [indifferent.png]: 0.png'); dlg.Update(dt); end;
		end;
		view.trans := -player^.local.trans;
	end;

	procedure Adventure.HandleDraw;
	begin
		inherited HandleDraw;
		location^.Draw(view);
	end;

	procedure Adventure.HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra);
	var
		i: sint;
		mot: boolean;
	begin
		case action of
			MouseLClick:
				if extra.Handle then
				begin
					player^.rtMethod := NotRotating;
					player^.SwitchToState('walk');
					player^.MoveTo(view.Inversed * pos, WalkingVelocity, nil, nil);
				end;
			MouseMove:
				begin
					// подсветка курсора на триггерах
					mot := no;
					for i := 0 to High(location^.triggers) do
						if InheritsFrom(TypeOf(location^.triggers[i]^), TypeOf(SpatialTrigger)) and pSpatialTrigger(location^.triggers[i])^.highlight and
							Rect.MakeSize(view * location^.triggers[i]^.local.trans, location^.triggers[i]^.size).Contains(pos) and extra.HandleSilent then
						begin
							Window.FromPointer(mgr^.win)^.cursor := Cursor1;
							mot := yes;
						end;

						if not mot and mouseOverTrigger and extra.HandleSilent then
							Window.FromPointer(mgr^.win)^.cursor := Cursor0;
					mouseOverTrigger := mot;

					if extra.Handle then
						if player^.mvMethod = NotMoving then player^.RotateTo(view.Inversed * pos);
				end;
		end;
		inherited HandleMouse(action, pos, extra);
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
