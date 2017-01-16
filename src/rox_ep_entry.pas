{$include opts.inc}
unit rox_ep_entry;

interface

uses
	USystem, UMath, Utils, Random, UClasses,
	rox_decoration, rox_actor, rox_ui, rox_gfx, rox_paths, rox_state_adventure, rox_location, rox_timer, rox_world, rox_gl,
	rox_ep_bar, rox_mv_flight;

type
	pEp_Entry = ^Ep_Entry;
	Ep_Entry = object(Adventure)
		hints: boolean;
		wall, door, ship, shipFire: pDecoration;
		doorTrig: pTrigger;

		smoke: array of record
			d: pDecoration;
			v: Vec2;
		end;
		smokeTimeout, flash, camVel, stateTime: float;

		constructor Init(world: pWorld);
		destructor Done; virtual;
		procedure HandleActivation; virtual;
		procedure HandleDeactivation; virtual;

		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;
		procedure HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra); virtual;
		procedure HandleKeyboard(action: KeyboardAction; key: KeyboardKey; var extra: HandlerExtra); virtual;

	private
		state: (Setup, SetupRe, SetupDepart, Idle, MovingToBarRequested, MoveToShip, Flying, Flied, CameraUp);
		shipVel, shipAcc: float;
		procedure PostSmokeToDoor;

	const
		EntryStateID = 'ep_entry';
		DepartStateID = 'ep_depart';
	end;

implementation

	function DoorTest(n: pNode; const pos: Vec2; t: pTrigger; param: pointer): boolean;
	var
		e: pEp_Entry absolute param;
	begin
		Assert(@param = @param);
		result := (n = pNode(e^.player)) and (n^.local.trans.y - 0.05 < e^.door^.local.trans.y) and
			(abs(AngleDiff(ArcTan2(t^.HeartPos - pos), pActor(n)^.angle)) < Pi/6);
	end;

	procedure ProcessDoorHint(timer: pTimer; const dt: float; param: pointer);
	var
		e: pEp_Entry absolute param;
	begin
		Assert(dt = dt);
		if not e^.doorTrig^.HasInside(e^.player) then timer^.Stop;
	end;

	procedure DoorTrigger(n: pNode; reason: Trigger.Reason; param: pointer);
	var
		e: pEp_Entry absolute param;
	begin
		Assert(n = n);
		case reason of
			Entered:
				begin
					e^.door^.texRect := Rect.MakeSize(1/3, 0, 1/3, 1);
					if e^.hints then e^.OpenHint('hint-act.png', 20, @ProcessDoorHint);
				end;
			Leaved: e^.door^.texRect := Rect.MakeSize(0, 0, 1/3, 1);
		end;
	end;

	procedure DoorActivate(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Entry absolute param;
	begin
		Assert(t = t);
		if activator <> pNode(e^.player) then exit;
		if e^.doorTrig^.HasInside(e^.player) then e^.state := MovingToBarRequested;
	end;

	constructor Ep_Entry.Init(world: pWorld);
	var
		stateId: string;
	begin
		stateId := EntryStateID;
		if Assigned(world) then
			if world^.spaceshipBrought then
			begin
				state := SetupDepart;
				stateId := DepartStateID;
			end else
				state := SetupRe
		else
		begin
			hints := yes;
			state := Setup;
		end;
		inherited Init(stateId, world);

		door := new(pDecoration, Init(Environment('bar_door.png'), Translate(1, 0), Vec2.Make(0.3, 0.3*1.3)))^.SetTexRect(Rect.MakeSize(0, 0, 1/3, 1))^.NewRef;
		location^.AddWall(door, Vec2.Zero, Vec2.Make(0, 0.2/1*1.3));

		doorTrig := new(pTrigger, Init(Translate(0, -0.1) * door^.local, door^.size))^.WithCallbacks(@DoorTest, @DoorTrigger, @DoorActivate, @self)^.
			AddTo(location)^.NewRef;

		wall := new(pDecoration, Init(Environment('brick.png'), Translate(0, 0.02), Vec2.Make(1.5, 0.3)))^.SetTexRect(Rect.Make(0, 0, 5, 1))^.NewRef;
		self.location^.AddWall(wall, Vec2.Zero, Vec2.Make(0, 0.2/1*1.3));

		if state <> Setup then player^.local := door^.local * Translate(0.5 * (door^.size.x - player^.size.x), -0.15);
		location^.Add(player);
	end;

	destructor Ep_Entry.Done;
	begin
		Release(doorTrig); Release(door);
		Release(wall);
		Release(shipFire); Release(ship);
		inherited Done;
	end;

	procedure Ep_Entry.HandleActivation;
	var
		name: string;
		time: float;
	begin
		inherited HandleActivation;
		if mgr^.bgm.CurrentTrack(@name, @time, nil) and (name = 'restoration1') and (time < 1.0) then
			mgr^.bgm.Rewind(1.0 - time);
	end;

	procedure Ep_Entry.HandleDeactivation;
	begin
		mgr^.bgm.Priority(id)^.RemoveModifier('mute', no);
		inherited HandleDeactivation;
	end;

	procedure ProcessMovementHint(timer: pTimer; const dt: float; param: pointer);
	var
		e: pEp_Entry absolute param;
	begin
		Assert(dt = dt);
		if (e^.controls <> []) or (e^.player^.mvMethod <> NotMoving) then timer^.left := min(timer^.left, 1.0);
	end;

	procedure MonologueFinished(param: pointer);
	var
		e: pEp_Entry absolute param;
	begin
		e^.cameraMode := LookAfterPlayer;
		e^.playerControlMode := PlayerControlEnabled;
		e^.player^.idclip := no;
		e^.OpenHint('hint-move.png', 8, @ProcessMovementHint);
	end;

	procedure PlayerWalkedIn(param: pointer);
	var
		e: pEp_Entry absolute param;
	begin
		e^.dlg.Init(e, 'rox [face = indifferent.png]: 0.png >>' +
			'rox [face = saliva.png]: 1.png >>' +
			'rox [face = scared.png, delay = 0.5]: - >>' +
			'rox [face = scared-refl.png, delay = 0.5]: - >>' +
			'rox [face = scared.png, delay = 1]: 2.png >>' +
			'rox: 3.png')^.Callbacks(nil, @MonologueFinished, e);
	end;

	procedure ShipActivate(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Entry absolute param;
	begin
		if activator <> pNode(e^.player) then exit;
		t^.Detach;
		e^.state := MoveToShip;
	end;

	procedure Depart_3(param: pointer);
	var
		e: pEp_Entry absolute param;
	begin
		e^.state := Flying;
		e^.location^.Add(e^.shipFire);
		e^.PostSmokeToDoor;
	end;

	procedure Depart_2(param: pointer);
	// Валера в корабле.
	var
		e: pEp_Entry absolute param;
	begin
		Assert(not e^.dlg.Valid);
		e^.dlg.Init(e, 'valera [face = shine.png]: 3.png')^.Callbacks(nil, @Depart_3, e);
	end;

	procedure Depart_1(reason: Actor.MoveCallbackReason; ac: pActor; param: pointer);
	// Рокс вошёл в корабль, задержка.
	var
		e: pEp_Entry absolute param;
	begin
		if reason <> TargetReached then exit;
		ac^.Detach;
		e^.AddTimer(2.0, nil, @Depart_2, e);
	end;

	procedure Depart_5_ShiftCameraUp(t: pTimer; const dt: float; param: pointer);
	var
		e: pEp_Entry absolute param;
	begin
		Assert(t = t);
		e^.camVel += 2*dt;
		e^.camera.target := e^.camera.target + Vec2.Make(0, e^.camVel * dt);
	end;

	procedure Depart_4_WaitForShiftCameraUp(param: pointer);
	var
		e: pEp_Entry absolute param;
	begin
		e^.cameraMode := LookPredefined;
		e^.mgr^.bgm.Priority(e^.id)^.SetModifier('mute', op_Set, 0, +999);
		e^.state := CameraUp; e^.stateTime := 0;
		e^.AddTimer(2, @Depart_5_ShiftCameraUp, nil, e);
	end;

	procedure Ep_Entry.HandleUpdate(const dt: float);
	const
		Flames: array[0 .. 1] of record pos: Vec2; size: float; end = ((pos: (data: (0.15, 0.22)); size: 0.15), (pos: (data: (-0.03, 0)); size: 0.18));
		MaxShipAngle = 0.9 * HalfPi;
	var
		i: sint;
	begin
		stateTime += dt;
		case state of
			Setup, SetupRe, SetupDepart:
				begin
					location^.limits := Rect.Make(-mgr^.nvp, mgr^.nvp);
					case state of
						Setup:
							begin
								cameraMode := LookPredefined;
								camera.target := Vec2.Make(-0.1, 0.2);

								playerControlMode := PlayerControlDisabled;
								player^.idclip := yes;
								player^.local.trans := Vec2.Make(camera.target.x - mgr^.nvp.x - player^.size.x, -0.3);
								player^.MoveTo(Vec2.Make(-0.4, -0.3), WalkingVelocity, @PlayerWalkedIn, @self);
							end;
						SetupRe: ;
						SetupDepart:
							begin
								location^.limits.B.data[0] += 2.2;
								ship := new(pDecoration, Init(Environment('ship.png'), Translate(mgr^.nvp.x + 0.8, -0.5), Vec2.Make(0.9, Deduce)))^.NewRef;
								shipFire := new(pDecoration, Init(Environment('ship-fire.png'), Translate(ship^.size * Vec2.Make((-9-91)/283, (-23+10)/177)),
									Vec2.Make(0.9 * (168/283), Deduce)))^.SetAnim(Rect.MakeSize(Vec2.Zero, Vec2.Make(1/3, 1)), 3, 0.3, [])^.SetLayer(+1)^.SetParent(ship)^.
									NewRef;
								(new(pDecoration, Init(Character('valera', 'in_ship.png'), Translate(ship^.size * Vec2.Make(239/283, 90/177)),
									Vec2.Make(0.9 * (21/283), Deduce))))^.SetParent(ship)^.AddTo(location);
								location^.AddWall(ship, Vec2.Make(0, 0.05), Vec2.Make(0, 0.3));
								(new(pTrigger, Init(ship^.local, ship^.size)))^.WithCallbacks(nil, nil, @ShipActivate, @self)^.AddTo(location);
							end;
					end;
					state := Idle;
				end;
		end;

		inherited HandleUpdate(dt);

		case state of
			MovingToBarRequested: mgr^.Switch(new(pEp_Bar, Init(world)));
			MoveToShip:
				begin
					playerControlMode := PlayerControlDisabled;
					player^.idclip := yes;
					player^.MoveTo(ship^.local.trans + Vec2.Make(0.3, 0.25), WalkingVelocity, @Depart_1, @self);
				end;
			Flying, Flied, CameraUp:
				begin
					shipAcc := min(shipAcc + (4*shipAcc+0.01)*dt, 1000);
					shipVel := min(shipVel + shipAcc * dt, 10);
					ship^.local := ship^.local * Translate(shipVel * dt, 0);
					if ship^.local.rot.ToAngle < MaxShipAngle then ship^.local.rot *= Rotation2(min(0.2*shipVel*dt, MaxShipAngle - ship^.local.rot.ToAngle));
					for i := High(smoke) downto 0 do
					begin
						smoke[i].d^.local := smoke[i].d^.local * Translate(dt * smoke[i].v);
						smoke[i].v *= Pow(0.52, dt);
						smoke[i].d^.local := smoke[i].d^.local * Translate(0.5 * smoke[i].d^.size);
						smoke[i].d^.local.scale += dt;
						smoke[i].d^.local := smoke[i].d^.local * Translate(-0.5 * smoke[i].d^.size);
						smoke[i].d^.alpha -= 0.2 * dt;
						if smoke[i].d^.alpha <= 0 then
						begin
							smoke[i].d^.Detach;
							smoke[i] := smoke[High(smoke)];
							SetLength(smoke, length(smoke) - 1);
						end;
					end;

					case state of
						Flying:
							if ship^.local.trans.y > location^.limits.B.y + 10 then
							begin
								for i := 0 to High(Flames) do
									(new(pDecoration, Init(Environment('flame.png'), door^.local * Translate(Flames[i].pos), Vec2.Make(Flames[i].size))))^.
										SetAnim(Rect.MakeSize(0, 0, 1/3, 1), 3, 0.3, [])^.SetLayer(+1)^.AddTo(location);
								door^.texRect := Rect.MakeSize(2/3, 0, 1/3, 1);
								Release(wall^.tex);
								wall^.tex := Texture.Load(Environment('brick_burnt.png'));
								AddTimer(3.5, nil, @Depart_4_WaitForShiftCameraUp, @self);
								state := Flied;
							end;
						Flied: flash := Max(flash - 0.5 * dt, 0);
						CameraUp: if stateTime >= 4 then mgr^.Switch(new(pMv_Flight, Init(world)));
					end;
				end;
		end;
	end;

	procedure Ep_Entry.HandleDraw;
	var
		q: Quad;
		tex: pTexture;
	begin
		inherited HandleDraw;
		case state of
			Flying, Flied, CameraUp:
				begin
					q.fields := [q.Field.Color];
					if state in [Flying, Flied] then
					begin
						if state = Flying then flash := min(1, 0.1 * shipVel) * clamp(0.5 * (ship^.local.trans.x - location^.limits.B.x + 0.5), 0, 1);
						q.color := Vec4.Make(1, 1, 1, flash);
						q.Draw(nil, -mgr^.nvp, 2 * mgr^.nvp, Vec2.Zero, Vec2.Ones);
					end else
					begin
						q.color := Vec4.Make(1, 1, 1, 0.3 * smoothstep(1.5, 2.5, stateTime) * smoothstep(4, 3, stateTime));
						tex := Texture.Load(Fx('up.png'));
						gl.BlendFunc(gl.SRC_ALPHA, gl.ONE);
						q.Draw(tex, -mgr^.nvp, 2 * mgr^.nvp, Vec2.Make(0, -sqr(0.5*stateTime)), Vec2.Make(1, 0.2));
						gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
						Release(tex);
					end;
				end;
		end;
	end;

	procedure Ep_Entry.HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra);
	begin
		inherited HandleMouse(action, pos, extra);
	end;

	procedure Ep_Entry.HandleKeyboard(action: KeyboardAction; key: KeyboardKey; var extra: HandlerExtra);
	begin
		inherited HandleKeyboard(action, key, extra);
	end;

	procedure SmokeTimer(param: pointer);
	var
		e: pEp_Entry absolute param;
	begin
		if (e^.state <> Flying) or (e^.ship^.local.trans.x > 4 * e^.mgr^.nvp.x) then exit;

		repeat
			e^.smokeTimeout -= 0.15 / clamp(10 * e^.shipVel, 1, 4);
			SetLength(e^.smoke, length(e^.smoke) + 1);
			pNode(e^.smoke[High(e^.smoke)].d) := new(pDecoration, Init(Environment('smoke.png'),
				e^.ship^.local * Translate(0, GlobalRNG.GetFloat(0, 0.2)), Vec2.Make(GlobalRNG.GetFloat(0.2, 0.25))))^.
				SetLayer(e^.shipFire^.layer + 1)^.
				AddTo(e^.location);
			e^.smoke[High(e^.smoke)].v := (e^.door^.HeartPos + GlobalRNG.Direction2 * Vec2.Make(0.8, 0.3) - e^.ship^.HeartPos).Normalized *
				GlobalRNG.GetFloat(0.62, 0.72);
			e^.smoke[High(e^.smoke)].d^.additive := yes;
		until e^.smokeTimeout <= 0;
		e^.PostSmokeToDoor;
	end;

	procedure Ep_Entry.PostSmokeToDoor;
	begin
		smokeTimeout += 0.15;
		AddTimer(0.15 / clamp(10 * shipVel, 1, 4), nil, @SmokeTimer, @self);
	end;

end.

