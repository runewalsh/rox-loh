{$include opts.inc}
unit rox_state_adventure;

interface

uses
	USystem, Errors, UMath, UClasses, Utils, GLUtils,
	rox_state, rox_gl, rox_ui, rox_actor, rox_location, rox_dialogue, rox_win, rox_world, rox_gfx;

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
		lastCursorPos: Vec2;
		fxPhase: float;
		constructor Init(const id: string; world: pWorld);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;
		procedure HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra); virtual;
		procedure HandleKeyboard(action: KeyboardAction; key: KeyboardKey; var extra: HandlerExtra); virtual;

	const
		RunningVelocity = 0.8;
		WalkingVelocity = 0.3;
	private
		function DirectionKeyToDir4(k: KeyboardKey): Dir4;
		procedure UpdateCursor(const pos: Vec2; force: boolean);
		procedure UnwieldWeapon;
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
			if not player^.wieldingWeapon then player^.rtMethod := NotRotating;
			delta := Vec2.Make(sint(_Right in controls) - sint(_Left in controls), sint(_Up in controls) - sint(_Down in controls));
			player^.MoveBy(0.3 * delta.Normalized, IfThen(shift, RunningVelocity, WalkingVelocity));
			lastMovementDirection := lastMovementDirection + (delta - lastMovementDirection) * dt;
		end;
		location^.Update(dt);
		if dlg.Valid then
		begin
			dlg.Update(dt);
			if dlg.Finished then dlg.Done;
		end;

		case cameraMode of
			LookAfterPlayer:
				begin
					a := min(location^.limits.A + 0.75 * mgr^.nvp, location^.limits.B);
					b := max(location^.limits.B - 0.75 * mgr^.nvp, location^.limits.A);
					camera.target := clamp(player^.local.trans + 0.6 * lastMovementDirection * mgr^.nvp, a, max(a, b));
				end;
		end;
		camera.Update(dt);
		fxPhase := modf(fxPhase + dt, PrettyTimeCycle);
	end;

	procedure Adventure.HandleDraw;
	var
		acAsNode: pNode;
		ac: pActor absolute acAsNode;
		q: Quad;
		rc: Location.RaycastResult;
		dist, angle: float;
	begin
		inherited HandleDraw;
		location^.Draw(camera.viewTransform);

		for acAsNode in location^.actors do
		begin
			if ac^.wieldingWeapon then
			begin
				dist := 1.5;
				angle := ac^.angle;
				if location^.Raycast(ac^.HeartPos, Rotate(Vec2.PositiveX, ac^.angle), rc, ac) then
					dist := clamp(sqrt(rc[0].sqrDistance) + (Distance(ac^.AimOrigin, rc[0].point) - Distance(ac^.HeartPos, rc[0].point)), 0, dist);


				// красная линия прицела
				q.fields := [q.Field.Transform, q.Field.ColorAB];
				q.transform := camera.viewTransform * Translate(ac^.AimOrigin) * Rotate(angle - HalfPi);
				q.colorA := Vec4.Make(1, 0, 0, 0.35 + 0.15 * 2.0 * abs(0.5 - frac(10*fxPhase)) * min(1.0, 0.5*dist));
				q.colorB := Vec4.Make(1, 0, 0, 0.1);
				q.Draw(nil, Vec2.Make(-0.004, 0), Vec2.Make(0.008, dist), Vec2.Zero, Vec2.Ones);
			end;
		end;
	end;

	procedure Adventure.HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra);
	begin
		case action of
			MouseLClick:
				begin
					if dlg.Valid and not dlg.Finished and extra.Handle then dlg.Skip; {if Assigned(dlg.active) then dlg.active^.lettertimeout:=0;}
					if playerControlMode = PlayerControlEnabled then
					begin
						if player^.wieldingWeapon and extra.Handle then player^.Fire;
						if extra.HandleSilent and location^.ActivateTriggerAt(camera.Unproject(pos), player) then extra.Handle;

						if extra.Handle then
						begin
							player^.rtMethod := NotRotating;
							player^.MoveTo(camera.Unproject(pos), WalkingVelocity, nil, nil);
							lastMovementDirection := (camera.Unproject(pos) - player^.local.trans).Normalized;
						end;
					end;
				end;
			MouseRClick:
				if (playerControlMode = PlayerControlEnabled) and player^.wieldingWeapon and extra.Handle then UnwieldWeapon;
			MouseMove:
				begin
					if extra.HandleSilent then UpdateCursor(pos, no);

					if (playerControlMode = PlayerControlEnabled) and ((player^.mvMethod = NotMoving) or player^.wieldingWeapon) and extra.Handle then
						player^.RotateTo(camera.Unproject(pos));
				end;
		end;
		inherited HandleMouse(action, pos, extra);
	end;

	procedure Adventure.HandleKeyboard(action: KeyboardAction; key: KeyboardKey; var extra: HandlerExtra);
	begin
		case action of
			KeyClick:
				case key of
					key_Up, key_Down, key_Left, key_Right: if extra.Handle then controls += [DirectionKeyToDir4(key).value];
					key_LShift: if extra.Handle then shift := yes;
					key_Esc:
						if extra.Handle then
						begin
							mgr^.Switch(new(pMainMenu, Init));
							exit;
						end;
					key_Z:
						if extra.Handle then
							if dlg.Valid and not dlg.Finished then
								dlg.Skip
							else
								if playerControlMode = PlayerControlEnabled then
									location^.ActivateTriggerFor(player);
					key_1:
						if playerControlMode = PlayerControlEnabled then
							if player^.wieldingWeapon then UnwieldWeapon else
							begin
								player^.WieldWeapon;
								UpdateCursor(lastCursorPos, yes);
							end;
				end;
			KeyRelease:
				case key of
					key_Up, key_Down, key_Left, key_Right: if extra.Handle then controls -= [DirectionKeyToDir4(key).value];
					key_LShift: if extra.Handle then shift := no;
				end;
		end;
		inherited HandleKeyboard(action, key, extra);
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

	procedure Adventure.UpdateCursor(const pos: Vec2; force: boolean);
	var
		ht: boolean;
	begin
		lastCursorPos := pos;
		ht := location^.ShouldHighlightTrigger(camera.Unproject(pos));
		if ht then Window.FromPointer(mgr^.win)^.cursor := Cursor1;
		if not ht and (triggerHighlighted or force) then Window.FromPointer(mgr^.win)^.cursor := Cursor0;
		triggerHighlighted := ht;
	end;

	procedure Adventure.UnwieldWeapon;
	begin
		player^.UnwieldWeapon;
	end;

end.
