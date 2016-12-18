{$include opts.inc}
unit rox_state_adventure;

interface

uses
	USystem, Errors, UMath, UClasses, Utils, GLUtils,
	rox_state, rox_gl, rox_ui, rox_actor, rox_location, rox_dialogue, rox_win, rox_world;

type
	pCamera = ^Camera;
	Camera = object
		viewTransform: Transform2;
		target: Vec2;
		restart: boolean;
		procedure Init;
		procedure Update(const dt: float);
		function Unproject(const visiblePos: Vec2): Vec2;
	end;

	pAdventure = ^Adventure;
	Adventure = object(State)
		controls: set of Dir4.Enum;
		shift: boolean;
		camera: Camera;
		cameraMode: (LookAfterPlayer, LookPredefined);
		world: pWorld;
		player: pActor;
		lastMovementDirection: Vec2;
		playerControlMode: (PlayerControlEnabled, PlayerControlDisabled);
		location: pLocation;
		dlg: Dialogue;
		triggerHighlighted: boolean;
		constructor Init(const id: string; world: pWorld);
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

	procedure Camera.Init;
	begin
		viewTransform := Transform2.Identity;
		target := Vec2.Zero;
		restart := yes;
	end;

	procedure Camera.Update(const dt: float);
	var
		delta: Vec2;
	begin
		if restart then
		begin
			viewTransform.trans := -target;
			restart := no;
		end else
		begin
			delta := (-target - viewTransform.trans) * min(1.5*dt, 1.0);
			viewTransform.trans := viewTransform.trans + delta;
		end;
	end;

	function Camera.Unproject(const visiblePos: Vec2): Vec2;
	begin
		result := viewTransform.Inversed * visiblePos;
	end;

	constructor Adventure.Init(const id: string; world: pWorld);
	begin
		dlg.Invalidate;
		inherited Init(id);
		camera.Init;
		if Assigned(world) then self.world := world^.NewRef else self.world := new(pWorld, Init)^.NewRef;
		player := self.world^.player^.NewRef;
		if Assigned(player^.location) then player^.Detach;
	end;

	destructor Adventure.Done;
	begin
		if Assigned(mgr) then Window.FromPointer(mgr^.win)^.cursor := Cursor0;
		Release(player);
		Release(location);
		Release(world);
		dlg.Done;
		inherited Done;
	end;

	procedure Adventure.HandleUpdate(const dt: float);
	var
		delta, a, b: Vec2;
	begin
		if not Assigned(player) then raise Error('Не назначен игрок.');
		if not Assigned(location) then raise Error('Не назначена локация.');

		inherited HandleUpdate(dt);
		if (controls <> []) and (playerControlMode = PlayerControlEnabled) then
		begin
			player^.SwitchToState('walk');
			player^.rtMethod := NotRotating;
			delta := Vec2.Make(sint(_Right in controls) - sint(_Left in controls), sint(_Up in controls) - sint(_Down in controls));
			player^.MoveBy(0.3 * delta.Normalized, IfThen(shift, RunningVelocity, WalkingVelocity));
			lastMovementDirection := lastMovementDirection + (delta - lastMovementDirection) * dt;
		end;
		location^.Update(dt);
		if dlg.Valid then dlg.Update(dt);

		case cameraMode of
			LookAfterPlayer:
				begin
					a := min(location^.limits.A + 0.75 * mgr^.nvp, location^.limits.B);
					b := max(location^.limits.B - 0.75 * mgr^.nvp, location^.limits.A);
					camera.target := clamp(player^.local.trans + 0.6 * lastMovementDirection * mgr^.nvp, a, max(a, b));
				end;
		end;
		camera.Update(dt);
	end;

	procedure Adventure.HandleDraw;
	begin
		inherited HandleDraw;
		location^.Draw(camera.viewTransform);
	end;

	procedure Adventure.HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra);
	var
		ht: boolean;
	begin
		case action of
			MouseLClick:
				begin
					if dlg.Valid and not dlg.Finished and extra.Handle then dlg.Skip;
					if extra.HandleSilent and location^.ActivateTriggerAt(camera.Unproject(pos), player) then extra.Handle;

					if (playerControlMode = PlayerControlEnabled) and extra.Handle then
					begin
						player^.rtMethod := NotRotating;
						player^.MoveTo(camera.Unproject(pos), WalkingVelocity, nil, nil);
						lastMovementDirection := (camera.Unproject(pos) - player^.local.trans).Normalized;
					end;
				end;
			MouseMove:
				begin
					ht := location^.ShouldHighlightTrigger(camera.Unproject(pos));
					if ht and extra.HandleSilent then Window.FromPointer(mgr^.win)^.cursor := Cursor1;
					if not ht and triggerHighlighted and extra.HandleSilent then Window.FromPointer(mgr^.win)^.cursor := Cursor0;
					triggerHighlighted := ht;

					if (playerControlMode = PlayerControlEnabled) and (player^.mvMethod = NotMoving) and extra.Handle then
						player^.RotateTo(camera.Unproject(pos));
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
					key_Z: location^.ActivateTriggerFor(player);
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
