{$include opts.inc}
unit rox_state_adventure;

interface

uses
	USystem, Errors, UMath, UClasses, Utils, GLUtils,
	rox_state, rox_gl, rox_ui, rox_actor, rox_location, rox_dialogue, rox_win, rox_world, rox_gfx, rox_paths, rox_timer;

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
		fxPhase, deathPhase: float;
		display: boolean;
		bullet: pTexture;
		hint: pControl;
		hintTimer: pTimer;
		constructor Init(const id: string; world: pWorld);
		destructor Done; virtual;
		procedure HandleDeactivation; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;
		procedure HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra); virtual;
		procedure HandleKeyboard(action: KeyboardAction; key: KeyboardKey; var extra: HandlerExtra); virtual;
		procedure OpenHint(const src: string; const timeout: float; process: Timer.ProcessCallback; const size: float = 0; const y: float = 0);

	const
		RunningVelocity = 0.8;
		WalkingVelocity = 0.3;
		ShotDistance = 1.5;
	protected
		procedure UnwieldWeapon; virtual;
	private
		function DirectionKeyToDir4(k: KeyboardKey): Dir4;
		procedure UpdateCursor(const pos: Vec2; force: boolean);
		function PlayerDied: boolean;
		procedure PostDeathClick;
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
		result := -viewTransform * visiblePos;
	end;

	constructor Adventure.Init(const id: string; world: pWorld);
	begin
		dlg.Invalidate;
		inherited Init(id);
		camera.Init;
		if Assigned(world) then self.world := world^.NewRef else self.world := new(pWorld, Init)^.NewRef;
		player := self.world^.player^.NewRef;
		player^.Cleanup;
		display := yes;
		location := new(pLocation, Init(@self))^.NewRef;
	end;

	destructor Adventure.Done;
	begin
		if Assigned(mgr) then Window.FromPointer(mgr^.win)^.cursor := Cursor0;
		Release(player);
		Release(location);
		Release(world);
		Release(bullet);
		Release(hintTimer); Release(hint);
		dlg.Done;
		inherited Done;
	end;

	procedure Adventure.HandleDeactivation;
	begin
		mgr^.bgm.Priority('over')^.RemoveModifier(id, no);
		inherited HandleDeactivation;
	end;

	procedure Adventure.HandleUpdate(const dt: float);
	var
		delta, a, b: Vec2;
		i: sint;
	begin
		if not Assigned(player) then raise Error('Не назначен игрок.');
		if not Assigned(location) then raise Error('Не назначена локация.');
		// writeln(tostring(player^.pointon(vec2.make(0.5, 0))));

		inherited HandleUpdate(dt);
		if (controls <> []) and (playerControlMode = PlayerControlEnabled) then
		begin
			// if player^.wieldingWeapon then player^.RotateTo(camera.Unproject(lastCursorPos));
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
					for i := 0 to High(a.data) do
						if a.data[i] > b.data[i] then
						begin
							a.data[i] := 0.5 * (a.data[i] + b.data[i]);
							b.data[i] := a.data[i];

						end;
					camera.target := clamp(player^.local.trans + 0.6 * lastMovementDirection * mgr^.nvp, a, max(a, b));
				end;
		end;
		camera.Update(dt);
		fxPhase := modf(fxPhase + dt, PrettyTimeCycle);

		if PlayerDied then
		begin
			if (deathPhase = 0) and (dt > 0) then
			begin
				mgr^.bgm.ResetTheme('over');
				mgr^.bgm.Priority('over')^.SetModifier(id, op_Add, +1, 0);
			end;
			deathPhase += dt;
			if deathPhase > 3 then display := no;
		end;
	end;

	procedure Adventure.HandleDraw;
	var
		acAsNode: pNode;
		ac: pActor absolute acAsNode;
		q: Quad;
		rc: Location.RaycastResult;
		dist, angle, blink: float;
		i: uint;
		sz: Vec2;
	begin
		inherited HandleDraw;
		if display then
		begin
			location^.Draw(camera.viewTransform);

			for acAsNode in location^.actors do
			begin
				if ac^.wieldingWeapon then
				begin
					dist := ShotDistance;
					angle := ac^.angle;
					if location^.Raycast(ac^.HeartPos, Rotation2(ac^.angle).ToDirection, rc, ac) then
						dist := clamp(sqrt(rc[0].sqrDistance) + (Distance(ac^.AimOrigin, rc[0].point) - Distance(ac^.HeartPos, rc[0].point)), 0, dist);

					// красная линия прицела
					q.fields := [q.Field.Transform, q.Field.ColorAB];
					q.transform := camera.viewTransform * Translate(ac^.AimOrigin) * Rotate(angle - HalfPi);
					blink := 0.5 + 0.5*2*abs(0.5 - frac(10*fxPhase));
					q.colorA := Vec4.Make(1, 0, 0, blink * 0.2 * clamp((1.5 - dist) / 1.5, 0, 1));
					q.colorB := Vec4.Make(1, 0, 0, blink * 0.2);
					q.Draw(nil, Vec2.Make(-0.005, 0), Vec2.Make(0.01, dist), Vec2.Zero, Vec2.Ones);
					gl.BlendFunc(gl.SRC_ALPHA, gl.ONE);
					q.Draw(nil, Vec2.Make(-0.002, 0), Vec2.Make(0.004, dist), Vec2.Zero, Vec2.Ones);
					gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
				end;
			end;
		end;

		if PlayerDied then
		begin
			q.fields := [q.Field.Color];
			q.color := Vec4.Make(0.75 * smoothstep(5, 3, deathPhase), 0, 0, smoothstep(1, 3, deathPhase) * smoothstep(5, 3, deathPhase));
			q.Draw(nil, -mgr^.nvp, 2 * mgr^.nvp, Vec2.Zero, Vec2.Ones);
		end;

		for i := 1 to min(player^.bullets, 5) do
		begin
			if not Assigned(bullet) then bullet := Texture.Load(UI('bullet.png'));
			sz := bullet^.ap.Aspect2(asp2_y1, 0.1);
			Quad.DrawPlain(bullet, Vec2.Make(-mgr^.nvp.x, mgr^.nvp.y) + sz * Vec2.Make((i - 1) * 0.5, -1), sz, Vec2.Zero, Vec2.Ones);
		end;
	end;

	procedure Adventure.HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra);
	begin
		case action of
			MouseLClick:
				begin
					if dlg.Valid and not dlg.Finished and dlg.Skippable and extra.Handle then dlg.Skip;
					if PlayerDied and extra.Handle then PostDeathClick;
					if playerControlMode = PlayerControlEnabled then
					begin
						if player^.wieldingWeapon and (player^.bullets > 0) and extra.Handle then
						begin
							player^.Fire;
							dec(player^.bullets);
							if player^.bullets = 0 then UnwieldWeapon;

							// Внимание, стрелять нужно по тому же лучу, по какому рисовался лазер.
							// (Это может быть очевидно, но HeartPos ↔ AimOrigin... последнее логичнее, но даёт проблемки с упиранием пистолета в стены)
							location^.Shot(player, player^.HeartPos, player^.HeartPos + ShotDistance * Rotation2(player^.angle).ToDirection);
						end;
						if extra.HandleSilent and location^.ActivateTriggerAt(camera.Unproject(pos), player) then extra.Handle;

						if extra.Handle then
						begin
							// player^.StopRotating;
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

					if (playerControlMode = PlayerControlEnabled) and ((controls = []) and (player^.mvMethod = NotMoving) or player^.wieldingWeapon)
						and extra.Handle
					then
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
					key_Z:
						if extra.Handle then
							if dlg.Valid and dlg.Skippable and not dlg.Finished then dlg.Skip
							else if PlayerDied then PostDeathClick
							else if playerControlMode = PlayerControlEnabled then location^.ActivateTriggerFor(player);
					key_1:
						if playerControlMode = PlayerControlEnabled then
							if player^.wieldingWeapon then UnwieldWeapon else
								if player^.bullets > 0 then
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

	procedure CloseHintTimer(reason: Timer.DoneReason; param: pointer);
	var
		e: pAdventure absolute param;
	begin
		if reason in [Timeout, Stopped] then
		begin
			Release(e^.hintTimer);
			e^.hint^.Detach;
			Release(e^.hint);
		end;
	end;

	procedure Adventure.OpenHint(const src: string; const timeout: float; process: Timer.ProcessCallback; const size: float = 0; const y: float = 0);
	begin
		if Assigned(hintTimer) then begin hintTimer^.Stop; Release(hintTimer); end;
		if Assigned(hint) then begin hint^.Detach; Release(hint); end;
		hint := new(pControl, Init(Texture.Load(UI(src)), []))^.NewRef;
		hint^.size := IfThen(size <> 0, size, 0.5);
		hint^.local := Translate(-mgr^.nvp.x, mgr^.nvp.y - hint^.CalculateRawSize.y + y);
		mgr^.ui.Add(hint^.NewRef, id);
		hintTimer := AddTimer(timeout, process, @CloseHintTimer, @self)^.NewRef;
	end;

	procedure Adventure.UnwieldWeapon;
	begin
		player^.UnwieldWeapon;
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

	function Adventure.PlayerDied: boolean;
	begin
		result := player^.states[player^.state].name = 'death';
	end;

	procedure Adventure.PostDeathClick;
	begin
		if deathPhase > 4 then mgr^.Switch(new(pMainMenu, Init));
	end;

end.
