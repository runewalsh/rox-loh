{$include opts.inc}
unit rox_ep_mars;

interface

uses
	USystem, UMath, Utils, U_GL, UClasses, Random,
	rox_decoration, rox_location, rox_world, rox_state_adventure, rox_paths, rox_gfx, rox_gl, rox_timer, rox_actor, rox_dialogue;

type
	pIris = ^Iris;
	Iris = object(Decoration)
		dir: Vec2;
		procedure HandleDraw(const view: Transform2); virtual;
	end;

	pFootprint = ^Footprint;
	Footprint = object(Decoration)
		life, baseAlpha: float;
		procedure HandleUpdate(const dt: float); virtual;
	end;

	pEp_Mars = ^Ep_Mars;
	Ep_Mars = object(Adventure)
	type
		StateEnum = (_1_SetupShip, _1_ShipFlying, _1_ShipFlied, _1_Fadeout, _2_SetupShip, FadeoutMovingToShip, MoveToShip, RedFlash, BlueFlash,
			FadeIn, Idle, FadeoutToEnding);
	var
		ship, shipFire, eye: pDecoration;
		iris: pIris;
		baseIrisSize: Vec2;
		state: StateEnum;
		stateTime: float;

		blooddropTrigger: pTrigger;
		bloodDropsOnFeet: uint;
		lastBloodyFootprint: Vec2; // мусор при bloodDropsOnFeet = 0
		nextFootprintL, fadeoutMovingToShipThisTime, doFadeIn: boolean;
		nextSentenceAboutEye: uint;

		valera, twinkle, kazah: pActor;
		roxLocalBase, valeraLocalBase, twinkleLocalBase, kazahLocalBase: Transform2;
		jump: DimensionalMove;
		shipFirePhase, shipVel, flightTime, shipFireAlpha: float;
		shipFireStretch: Vec2;

		constructor Init(world: pWorld; doFadeIn: boolean);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;
		procedure HandleDeactivation; virtual;
	private
		procedure CreateShipFire;
		procedure UpdateIris;
		function ShipArrivalOrigin: Vec2;
		function ShipStanding: Vec2;
		function EyePos: Vec2;
		function ReferencePlayerPositionForBloodyFootprint: Vec2;
		procedure PlaceExplodedEyeLeftover;
		function BaseEyeRect: Rect;
		function ExplodedEyeRect: Rect;
		procedure RotateFour(const at: Vec2; ignore: pNode);
		procedure DrawShipFireBacklight(const view: Transform2; const latPhase, longPhase: float; pass: uint; const stretch: Vec2; const alphaK: float);
		procedure ApplyActorsOffset(const delta: Vec2);
		procedure SwitchState(state: StateEnum);
	const
		StartingStateID = 'ep_mars_starting';
		EyeExplodedStateID = 'ep_mars_eye_exloded';
		AllOutsideStateID = 'ep_mars_all_outside';
		Fadeout1 = 1.3;
		PlayerPosAtExit: Vec2 = (data: (0.4, 0.23));
		EyeCollision = 'eye';
		BaseEyeSize: Vec2 = (data: (0.27, 0.27/100*60));
		RedFlashTime = 3.0;
		ShipFireLatPhaseVelocity = 6.0;
		ShipFireLongPhaseVelocity = 18.0;
		ShipFireShowingTime = 2.0;
		BlueFlashTime = 3.0;
	end;

implementation

uses
	rox_ep_ship, rox_ep_bar;

	procedure Iris.HandleDraw(const view: Transform2);
	var
		rotateInDirection, finalRotated: Transform;
		frame, lfix: float;
	begin
		if dir.y > 0 then
			rotateInDirection := Rotate(slerp(Quaternion.Identity, Quaternion.Rotation(Vec3.PositiveZ, Vec3.Make(dir.x, dir.y, 0).Normalized), min((Vec2.Make(1, 4) * dir).length, 1)))
		else
			rotateInDirection := Rotate(slerp(Quaternion.Identity, Quaternion.Rotation(Vec3.PositiveZ, Vec3.Make(dir.x, 0, -2*dir.y).Normalized), min((Vec2.Make(1, 2) * dir).length, 1)));
		frame := pDecoration(parent)^.CurrentFrame/4;
		finalRotated := rotateInDirection * Translate(0, 0, 0.42 * parent^.size.x) * Rotate(min(abs(dir.x), 1)*(-0.6+0.15*frame), Vec3.PositiveX);
		if finalRotated.tr.y > 0 then
		begin
			lfix := min(1000*finalRotated.tr.y*sqr(finalRotated.tr.x), 1);
			finalRotated.tr.y := -0.02*lfix + finalRotated.tr.y * (0.48-0.1*lfix + (0.13+0.03*lfix)*frame);
		end;
		finalRotated.tr.y := finalRotated.tr.y + 0.2*size.y*frame;
		finalRotated := Rotate(20 * Deg2Rad, Vec3.PositiveX) * finalRotated;
		gl.L.LoadMatrixf(
			Matrix4.Scaling(1/location^.state^.mgr^.nvp.x, 1/location^.state^.mgr^.nvp.y, 1) *
			((view * parent^.local * local).To3 * finalRotated).ToMatrix *
			Matrix4.Scaling(location^.state^.mgr^.nvp.x, location^.state^.mgr^.nvp.y, 1));
		Quad.DrawPlain(tex, -0.5*size, size, texRect.A, texRect.Size);
		gl.L.LoadIdentity;
	end;

	procedure Footprint.HandleUpdate(const dt: float);
	begin
		inherited HandleUpdate(dt);
		life -= dt;
		if life <= 0 then
		begin
			Detach;
			exit;
		end;
		alpha := baseAlpha * min(1, life/30);
	end;

	procedure MovingToShipProceed(param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		if e^.fadeoutMovingToShipThisTime then
		begin
			e^.playerControlMode := PlayerControlDisabled;
			e^.SwitchState(FadeoutMovingToShip);
		end else
			e^.SwitchState(MoveToShip);
	end;

	procedure ActivateShipTrigger(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		if (activator <> pNode(e^.player)) or not t^.HasInside(activator) then exit;
		e^.playerControlMode := PlayerControlDisabled;
		e^.player^.MoveTo(e^.ShipStanding + e^.PlayerPosAtExit, e^.WalkingVelocity, @MovingToShipProceed, e);
		t^.Detach;
	end;

	procedure EyeReceiveHit(ac: pNode; param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		if ac <> pNode(e^.player) then exit;
		Assert(Assigned(e^.iris));
		e^.location^.Add((new(pDecoration, Init(Environment('explosion.png'),
			Translate(e^.EyePos + Vec2.Make(-0.5*(0.27 * (232/100) - 0.27), 0)),
			Vec2.Make(0.27 * (232/100), Deduce))))^.
			SetAnim(Rect.Make(0, 0, 1/3, 1), 3, 0.5, [Oneshot]));
		e^.eye^.Detach; e^.eye := nil;
		e^.iris^.Detach; e^.iris := nil;
		e^.location^.RemoveWall(e^.EyeCollision);

		e^.mgr^.bgm.Priority(e^.id)^.SetModifier('mute', op_Set, 0, +999);
		e^.world^.eyeExploded := yes;
		e^.fadeoutMovingToShipThisTime := yes;
		e^.PlaceExplodedEyeLeftover;

		if e^.state = Idle then
			e^.SwitchState(RedFlash);
	end;

	function TestCommentOnEyeTrigger(n: pNode; const pos: Vec2; t: pTrigger; param: pointer): boolean;
	var
		e: pEp_Mars absolute param;
	begin
		Assert((pos = pos) and (t = t));
		result := (n = pNode(e^.player)) and (not Assigned(e^.blooddropTrigger) or e^.blooddropTrigger^.HasInside(n));
	end;

	procedure Dialogue_1_CommentOnEye(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Mars absolute param;
		scenario: string;
	begin
		Assert(t = t);
		if (activator <> pNode(e^.player)) or e^.dlg.Valid then exit;
		if e^.world^.eyeExploded then scenario := 'rox: 12.png' else
		begin
			case e^.nextSentenceAboutEye of
				0: scenario := 'rox [face = scared.png]: 10.png';
				else scenario := 'rox: 11.png';
			end;
			e^.nextSentenceAboutEye := (e^.nextSentenceAboutEye + 1) mod 2;
		end;
		e^.dlg.Init(e, scenario);
	end;

	procedure DrawShipFireBacklightBeforeShipItself(const view: Transform2; param: pointer);
	const
		Steps = 6;
	var
		e: pEp_Mars absolute param;
		pass, i: uint;
	begin
		if e^.shipFireAlpha <= 0 then
		begin
			e^.ship^.drawBefore := nil;
			exit;
		end;

		for pass := 0 to 1 do
		begin
			e^.DrawShipFireBacklight(view, e^.shipFirePhase, e^.shipFirePhase, pass, e^.shipFireStretch, smoothstep(e^.shipFireAlpha));
			for i := 1 to Steps - 1 do
				if e^.shipFirePhase > i*Pi/Steps/e^.ShipFireLatPhaseVelocity then
					e^.DrawShipFireBacklight(view, e^.shipFirePhase - i*Pi/Steps/e^.ShipFireLatPhaseVelocity,
						e^.shipFirePhase - i*Pi/Steps/e^.ShipFireLongPhaseVelocity, pass, e^.shipFireStretch, smoothstep(e^.shipFireAlpha));
		end;
	end;

	procedure Scene_2_Fadeout(param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		e^.SwitchState(FadeoutToEnding);
	end;

	procedure Scene_2_InsertKazahSentence(timer: pTimer; const dt: float; param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		Assert(dt = dt);
		if not e^.dlg.Valid then
		begin
			e^.dlg.Init(e, 'kazah [face = suspicious.png]: 9.png')^.Callbacks(nil, @Scene_2_Fadeout, e);
			timer^.Stop;
		end;
	end;

	procedure Scene_2_StartWaitingForKazahSentence(param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		e^.AddTimer(99.0, @Scene_2_InsertKazahSentence, nil, e);
	end;

	procedure Scene_2_RotateAfterShip(reason: Actor.MoveCallbackReason; ac: pActor; param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		Assert(e = e);
		if reason <> TargetReached then exit;
		ac^.RotateTo(ac^.HeartPos - Vec2.Make(1, 1));

		if ac = e^.kazah then
			e^.AddTimer(2.0, nil, @Scene_2_StartWaitingForKazahSentence, e);
	end;

	procedure Scene_2_JumpProcess(timer: pTimer; const dt: float; param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		Assert(e^.jump.Valid and (timer = timer));
		if not e^.jump.Finished then e^.jump.Process(dt);
		e^.ApplyActorsOffset(Vec2.Make(0, 0.15 * e^.jump.CurrentF));
	end;

	procedure Scene_2_JumpDone(param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		Assert(e^.jump.Valid and not e^.dlg.Valid);
		e^.jump.Done;
		e^.dlg.Init(e, 'valera [face = gloomy.png]: 13.png');
		e^.ApplyActorsOffset(Vec2.Zero);
		e^.valera^.idclip := yes;
		e^.valera^.MoveTo(Vec2.Make(0, -0.1), e^.RunningVelocity, @Scene_2_RotateAfterShip, e);

		e^.player^.idclip := yes;
		e^.player^.MoveTo(Vec2.Make(0.14, -0.14), e^.RunningVelocity, @Scene_2_RotateAfterShip, e);

		e^.twinkle^.idclip := yes;
		e^.twinkle^.MoveTo(Vec2.Make(0.28, -0.18), e^.RunningVelocity, @Scene_2_RotateAfterShip, e);

		e^.kazah^.idclip := yes;
		e^.kazah^.MoveTo(Vec2.Make(0.42, -0.22), e^.RunningVelocity, @Scene_2_RotateAfterShip, e);
	end;

	procedure Scene_2_Jump(param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		e^.roxLocalBase := e^.player^.local;
		e^.valeraLocalBase := e^.valera^.local;
		e^.twinkleLocalBase := e^.twinkle^.local;
		e^.kazahLocalBase := e^.kazah^.local;
		Assert(not e^.jump.Valid);
		e^.jump.Init(1);
		e^.jump.path^.AddAUA(0.0, 0.0, 10.0, -5.0, 0.0, 0.3, 0, 0);
		e^.AddTimer(e^.jump.path^.len, @Scene_2_JumpProcess, @Scene_2_JumpDone, e);
	end;

	procedure Scene_2_RotateAtFlash(param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		e^.SwitchState(BlueFlash);
		e^.RotateFour(e^.ship^.PointOn(Vec2.Make(1, 0.5)), nil);
	end;

	procedure Scene_2_ProcessFlight(timer: pTimer; const dt: float; param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		e^.shipVel := min(e^.shipVel + 3.0 * dt, 10.0);
		if e^.ship^.local.trans.x > max(-1, -e^.mgr^.nvp.x) - e^.ship^.size.x - e^.shipFire^.size.x - 8.0 then
		begin
			e^.ship^.local *= Translate(-e^.shipVel*dt, 0);
			e^.shipFireStretch.data[0] += e^.shipVel*dt;
		end;
		e^.shipFireStretch.data[0] += 0.5 * dt;
		e^.shipFireStretch.data[1] += 0.5 * dt;
		e^.flightTime += dt;
		if e^.flightTime > 2.0 then
		begin
			e^.shipFireAlpha := max(e^.shipFireAlpha - 0.5*dt, 0);
			if e^.shipFireAlpha = 0 then timer^.Stop;
		end;
	end;

	procedure Scene_2_StartFlight(param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		e^.CreateShipFire;
		e^.AddTimer(99.0, @Scene_2_ProcessFlight, nil, e);
	end;

	procedure Scene_2_ProcessShipFire(timer: pTimer; const dt: float; param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		e^.shipFirePhase += dt;
		if e^.shipFireAlpha = 0 then timer^.Stop;
	end;

	procedure Scene_2_ShipFliesAway(param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		e^.ship^.drawBefore := @DrawShipFireBacklightBeforeShipItself;
		e^.ship^.drawBeforeParam := e;
		e^.shipFireAlpha := 1.0;
		e^.AddTimer(99.0, @Scene_2_ProcessShipFire, nil, e);
		e^.AddTimer(e^.ShipFireShowingTime, nil, @Scene_2_StartFlight, e);
		e^.AddTimer(max(0.1, (e^.ShipFireShowingTime - 0.5) - 0.3 - 0.3), nil, @Scene_2_RotateAtFlash, e);
		e^.AddTimer(max(0.1, (e^.ShipFireShowingTime - 0.5) - 0.3), nil, @Scene_2_Jump, e);
		e^.mgr^.bgm.Priority(e^.id)^.SetModifier('mute', op_Set, 0, +999);
	end;

	procedure Scene_2_DialogueItem(id: uint; what: Dialogue.ItemEvent; param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		case what of
			ItemStart:
				case id of
					0:
						begin
							e^.RotateFour(e^.player^.HeartPos, e^.player);
							e^.player^.RotateTo((e^.valera^.HeartPos + e^.twinkle^.HeartPos + e^.kazah^.HeartPos) / 3);
						end;
					1: e^.RotateFour((e^.valera^.HeartPos + e^.player^.HeartPos) / 2, e^.valera);
					2, 5: e^.RotateFour((e^.twinkle^.HeartPos + e^.player^.HeartPos) / 2, e^.twinkle);
					4: e^.RotateFour((e^.kazah^.HeartPos + e^.player^.HeartPos) / 2, e^.kazah);
				end;
		end;
	end;

	procedure Dialogue_2(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		Assert(t = t);
		if (activator <> pNode(e^.player)) or e^.dlg.Valid then exit;
		e^.playerControlMode := PlayerControlDisabled;
		e^.dlg.Init(e,
			'rox [face = x-eyes.png]: 13.png >>' +
			'valera: 12.png >>' +
			'twinkle: 8.png >>' +
			'twinkle [face = eyes-closed.png]: 9.png >>' +
			'kazah: 8.png >>' +
			'twinkle: 10.png')^.
			Callbacks(@Scene_2_DialogueItem, @Scene_2_ShipFliesAway, e);
	end;

	constructor Ep_Mars.Init(world: pWorld; doFadeIn: boolean);
	const
		CommentOnEyeTriggerBias: Vec2 = (data: (0.02, 0.03));
	var
		stateId: string;
	begin
		jump.Invalidate;
		if world^.everyoneFled then stateId := AllOutsideStateID
		else if world^.eyeExploded then stateId := EyeExplodedStateID
		else stateId := StartingStateID;
		inherited Init(stateId, world);

		(new(pDecoration, Init(Environment('land.png'), Translate(-0.4, -0.5), Vec2.Make(2.7, Deduce))))^.SetLayer(-2)^.AddTo(location);
		location^.AddWall(Rect.Make(-0.2, -0.1, 1.5, 0), 27.5 * Deg2Rad, [NotObstacleForBullets]);
		location^.AddWall(Rect.Make(1.17, 0.65, 2.3, 0.7), 0.08, [NotObstacleForBullets]);
		location^.AddWall(Rect.Make(2.2, -1, 2.3, 0.7), [NotObstacleForBullets]);
		location^.AddWall(Rect.Make(Vec2.Make(0.475, -0.388) + Rotate((22 - 90) * Deg2Rad) * Vec2.PositiveX, Vec2.Make(0.475 + 2.2, -0.288)),
			22 * Deg2Rad, [NotObstacleForBullets]);
		location^.AddWall(Rect.Make(-0.35, -0.3, 1, 0), -23 * Deg2Rad, [NotObstacleForBullets]);
		location^.AddObstacle(Circle.Make(0.67, 0.61, 0.3));
		location^.AddObstacle(Circle.Make(2.06, 0.77, 0.11));
		location^.AddWall(Rect.Make(2.1, 0.65, 2.3, 0.7), [NotObstacleForBullets]);

		if world^.eyeExploded then
			PlaceExplodedEyeLeftover
		else
		begin
			pNode(eye) := new(pDecoration, Init(Environment('land-eye.png'), Translate(EyePos), BaseEyeSize))^.
				SetAnim(Rect.Make(0, 0, 1/4, 1), 4, 0.5, [Mirror])^.AddTo(location);
			eye^.relHeart := Vec2.Make(0.5, 0.1);

			baseIrisSize := Vec2.Make(0.05, 0.05);
			pNode(iris) := new(pIris, Init(Environment('iris.png'), Translate(Vec2.Make(0.52, 0.5) * BaseEyeSize), baseIrisSize))^.AddTo(location);
			iris^.SetParent(eye);

			location^.AddWall(BaseEyeRect)^.OnHit(@EyeReceiveHit, @self)^.WithUid(EyeCollision);
		end;
		(new(pTrigger, Init(Translate(EyePos + CommentOnEyeTriggerBias), BaseEyeSize - 2*CommentOnEyeTriggerBias)))^.
			WithCallbacks(@TestCommentOnEyeTrigger, nil, @Dialogue_1_CommentOnEye, @self)^.AddTo(location);

		if self.world^.spaceshipArrivedOnMars then
		begin
			location^.limits := Rect.Make(-0.6, -0.5, 2.4, 1.3);
			self.player^.local := Translate(ShipStanding + PlayerPosAtExit - (self.player^.relHeart * self.player^.size));
			self.player^.angle := HalfPi;
			self.player^.AddTo(location);
			(new(pTrigger, Init(Translate(ShipStanding + Vec2.Make(0.2, 0.2)), Vec2.Make(0.5, 0.15))))^.WithCallbacks(nil, nil, @ActivateShipTrigger, @self)^.AddTo(location);
			SwitchState(_2_SetupShip);
			self.doFadeIn := doFadeIn;
		end else
		begin
			cameraMode := LookPredefined;
			location^.limits := Rect.Make(-5, -3, 5, 3);
			SwitchState(_1_SetupShip);
		end;

		if self.world^.everyoneFled then
		begin
			valera := CreateKolobok('valera')^.OnHit(@ValeraHit, @self);
			valera^.local := Translate(EyePos + Vec2.Make(-0.12, -0.03));
			valera^.angle := 0;
			location^.Add(valera);
			(new(pTrigger, Init(valera^.local, valera^.size)))^.WithCallbacks(nil, nil, @Dialogue_2, @self)^.AddTo(location);

			twinkle := CreateKolobok('twinkle')^.OnHit(@TwinkleHit, @self);
			twinkle^.local := Translate(EyePos + Vec2.Make(-0.05, 0.09));
			twinkle^.angle := -Pi/4;
			location^.Add(twinkle);
			(new(pTrigger, Init(twinkle^.local, twinkle^.size)))^.WithCallbacks(nil, nil, @Dialogue_2, @self)^.AddTo(location);

			kazah := CreateKolobok('kazah')^.OnHit(@KazahHit, @self);
			kazah^.local := Translate(EyePos + Vec2.Make(0.22, 0.09));
			kazah^.angle := -Pi*(3/4);
			location^.Add(kazah);
			(new(pTrigger, Init(kazah^.local, kazah^.size)))^.WithCallbacks(nil, nil, @Dialogue_2, @self)^.AddTo(location);
		end;
	end;

	destructor Ep_Mars.Done;
	begin
		jump.Done;
		Release(valera); Release(twinkle); Release(kazah);
		Release(ship);
		inherited Done;
	end;

	procedure Dialogue_1_1(param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		e^.SwitchState(_1_Fadeout);
	end;

	procedure Dialogue_1(param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		Assert(not e^.dlg.Valid);
		e^.dlg.Init(e,
			'rox [face = scared.png]: 7.png >>' +
			'twinkle: 2.png >>' +
			'kazah: 2.png >>' +
			'valera: 4.png >>' +
			'valera: 5.png');
		e^.dlg.Callbacks(nil, @Dialogue_1_1, e);
	end;

	procedure Ep_Mars.HandleUpdate(const dt: float);
	label again;
	var
		sp, spy: float;
		pos: Vec2;
		foot: pFootprint;
	begin
		stateTime += dt;
		again: case state of
			_1_SetupShip, _2_SetupShip:
				begin
					if state = _1_SetupShip then pos := ShipArrivalOrigin else pos := ShipStanding;
					ship := new(pDecoration, Init(Environment('ship-refl.png'), Translate(pos), Vec2.Make(0.9, Deduce)))^.AddTo(location)^.NewRef;

					if not world^.everyoneFled then
						(new(pDecoration, Init(Character('valera', 'in_ship.png'), Translate(ship^.size * Vec2.Make(1-239/283, 90/177)),
							Vec2.Make(-0.9 * (21/283), Deduce))))^.SetParent(ship)^.AddTo(location);

					if state = _1_SetupShip then
					begin
						CreateShipFire;
						SwitchState(_1_ShipFlying);
						AddTimer(5.5, nil, @Dialogue_1, @self);
					end else
					begin
						location^.AddWall(Rect.Make(ship^.local.trans + Vec2.Make(0, 0.1), ship^.local.trans + Vec2.Make(0.7, 0.15)));
						if doFadeIn then SwitchState(FadeIn) else SwitchState(Idle);
					end;
					goto again;
				end;
			_1_ShipFlying:
				begin
					sp := stateTime/6;
					if sp >= 1 then
					begin
						sp := 1;
						SwitchState(_1_ShipFlied);
					end;
					if (sp >= 0.7) and Assigned(shipFire) and Assigned(shipFire^.location) then begin shipFire^.Detach; shipFire := nil; end;
					spy := clamp(stateTime/4, 0, 1);
					ship^.local := Translate(lerp(ShipArrivalOrigin, ShipStanding, Vec2.Make(-2*sqr(sp)/2+2*sp, -2*sqr(spy)/2+2*spy)));
					camera.target := ship^.HeartPos + Vec2.Make((-mgr^.nvp.x - 0.5*ship^.size.x) * (1-sp), 0.5 - 1*(1-sp));
				end;
			RedFlash: if stateTime >= RedFlashTime then SwitchState(Idle);
			BlueFlash: if stateTime >= BlueFlashTime then SwitchState(Idle);
		end;

		if Assigned(iris) then UpdateIris;
		if (bloodDropsOnFeet > 0) and (SqrDistance(lastBloodyFootprint, ReferencePlayerPositionForBloodyFootprint) > sqr(0.04)) then
		begin
			foot := new(pFootprint, Init(Environment('footprint.png'),
				Translate(ReferencePlayerPositionForBloodyFootprint) *
				Rotate(player^.angle + HalfPi + GlobalRNG.GetFloat(-0.25, 0.25)) *
				Translate(Vec2.Make(0.11*player^.size.x*IfThen(nextFootprintL, -1, 1), 0)
					+ Vec2.Make(GlobalRNG.GetFloat(-0.006, 0.006), GlobalRNG.GetFloat(-0.006, 0.006))
					- Vec2.Make(0.008)),
				Vec2.Make(0.018, Deduce)));
			foot^.life := 40.0;
			foot^.baseAlpha := min(1, bloodDropsOnFeet / 15);
			foot^.SetLayer(-1)^.AddTo(location);
			lastBloodyFootprint := ReferencePlayerPositionForBloodyFootprint;
			dec(bloodDropsOnFeet);
			nextFootprintL := not nextFootprintL;
		end;
		inherited HandleUpdate(dt);

		case state of
			_1_Fadeout, FadeoutMovingToShip, FadeoutToEnding:
				if stateTime > Fadeout1 then
					case state of
						_1_Fadeout:
							begin
								world^.spaceshipArrivedOnMars := yes;
								mgr^.Switch(new(pEp_Ship, Init(world, AutoTransition)));
							end;
						FadeoutToEnding:
							begin
								// mgr^.Switch(new(pEnding, Init(world)))
							end;
						else {FadeoutMovingToShip} mgr^.Switch(new(pEp_Ship, Init(world, EnterThroughDoorWithFadeIn)));
					end;
			MoveToShip: mgr^.Switch(new(pEp_Ship, Init(world, EnterThroughDoor)));
		end;
	end;

	procedure Ep_Mars.HandleDraw;
	var
		q: Quad;
		k: float;
	begin
		inherited HandleDraw;
		case state of
			_1_Fadeout, FadeoutMovingToShip, FadeoutToEnding, RedFlash, BlueFlash, FadeIn:
				begin
					q.fields := [q.Field.Color];
					case state of
						_1_Fadeout, FadeoutMovingToShip, FadeoutToEnding: q.color := Vec4.Make(0, 0, 0, smoothstep(0, Fadeout1, stateTime));
						RedFlash:
							begin
								k := min(1.0, 40.0 * stateTime / RedFlashTime) * smoothstep(1.0 - stateTime / RedFlashTime);
								q.color := Vec4.Make(0.6 - 0.2 * (1.0 - k), 0.0, 0.0, 0.5 * k);
							end;
						BlueFlash:
							begin
								gl.BlendFunc(gl.SRC_ALPHA, gl.ONE);
								k := smoothstep(0.0, 0.5, stateTime / BlueFlashTime) * smoothstep(1.0, 0.5, stateTime / BlueFlashTime);
								k := lerp(k, min(k + 0.3, 1.0), 0.1 + 0.1 * cos(31*stateTime));
								q.color := Vec4.Make(0.5 * k, 0.5 * k, k, lerp(k, 1, 0.5));
							end;
						else {FadeIn} q.color := Vec4.Make(0, 0, 0, clamp(1 - stateTime / Fadeout1, 0, 1));
					end;
					q.Draw(nil, -mgr^.nvp, 2 * mgr^.nvp, Vec2.Zero, Vec2.Ones);
					gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
				end;
		end;
	end;

	procedure Ep_Mars.HandleDeactivation;
	var
		priority: pModifiableValue;
	begin
		inherited HandleDeactivation;
		priority := mgr^.bgm.Priority(id, no);
		if Assigned(priority) then priority^.RemoveModifier('mute', no);
	end;

	procedure Ep_Mars.CreateShipFire;
	begin
		Assert(not Assigned(shipFire));
		pNode(shipFire) := new(pDecoration, Init(Environment('ship-fire.png'), Translate(ship^.size * Vec2.Make(1-(-9-91)/283, (-23+10)/177)),
			Vec2.Make(-0.9 * (168/283), Deduce)))^.SetAnim(Rect.MakeSize(Vec2.Zero, Vec2.Make(1/3, 1)), 3, 0.3, [])^.SetLayer(+1)^.SetParent(ship)^.
			AddTo(location);
	end;

	procedure Ep_Mars.UpdateIris;
	var
		eyeTarget: Vec2;
	begin
		Assert(Assigned(iris));
		case state of
			_1_ShipFlying, _1_ShipFlied, _1_Fadeout: eyeTarget := ship^.HeartPos;
			else eyeTarget := player^.local.trans + Vec2.Make(0.5, 0.9) * player^.size;
		end;
		iris^.dir := eyeTarget - eye^.HeartPos;
	end;

	function Ep_Mars.ShipArrivalOrigin: Vec2;
	begin
		result := Vec2.Make(eye^.local.trans.x + mgr^.nvp.x + 4, 1);
	end;

	function Ep_Mars.ShipStanding: Vec2;
	begin
		result := Vec2.Make(0.15, -0.35);
	end;

	function Ep_Mars.EyePos: Vec2;
	begin
		result := Vec2.Make(-0.4 + 2.7*0.59, -0.5 + 2.7*(616/873)*0.47);
	end;

	function Ep_Mars.ReferencePlayerPositionForBloodyFootprint: Vec2;
	begin
		result := player^.PointOn(Vec2.Make(0.5, 0.12));
	end;

	function BlooddropTriggerTest(n: pNode; const pos: Vec2; t: pTrigger; param: pointer): boolean;
	var
		e: pEp_Mars absolute param;
	begin
		Assert((pos = pos) and (t = t));
		result := (n = pNode(e^.player)) and
			(((e^.eye^.HeartPos - e^.ReferencePlayerPositionForBloodyFootprint) * Vec2.Make(1, 3)).SqrLength < 0.046);
	end;

	procedure BlooddropTriggerEnterLeave(n: pNode; reason: Trigger.Reason; param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		if n <> pNode(e^.player) then exit;
		case reason of
			Entered:
				begin
					if e^.bloodDropsOnFeet = 0 then e^.lastBloodyFootprint := e^.ReferencePlayerPositionForBloodyFootprint;
					e^.bloodDropsOnFeet := max(e^.bloodDropsOnFeet, 25);
				end;
		end;
	end;

	procedure Ep_Mars.PlaceExplodedEyeLeftover;
	begin
		Assert(not Assigned(eye));
		pNode(eye) := new(pDecoration, Init(Environment('hole.png'), Translate(EyePos + Vec2.Make(-0.5*(0.27*(120/100) - 0.27), -0.02)),
			Vec2.Make(0.27 * (135/100), Deduce)))^.AddTo(location)^.SetLayer(-1);
		location^.AddWall(ExplodedEyeRect)^.WithUid(EyeCollision);

		Assert(not Assigned(blooddropTrigger));
		pNode(blooddropTrigger) := new(pTrigger, Init(Translate(EyePos), Vec2.Make(0.3, 0.1)))^.
			WithCallbacks(@BlooddropTriggerTest, @BlooddropTriggerEnterLeave, nil, @self)^.AddTo(location);
	end;

	function Ep_Mars.BaseEyeRect: Rect;
	begin
		result := Rect.Make(EyePos + Vec2.Make(0.1, 0.45) * BaseEyeSize, EyePos + Vec2.Make(0.9, 0.55) * BaseEyeSize);
	end;

	function Ep_Mars.ExplodedEyeRect: Rect;
	begin
		result := Rect.MakeSize(BaseEyeRect.A + Vec2.Make(0.03, -0.002), BaseEyeRect.Size * Vec2.Make(0.85, 0.005));
	end;

	procedure Ep_Mars.RotateFour(const at: Vec2; ignore: pNode);
		procedure Maybe(actor: pActor);
		begin
			if pNode(actor) <> ignore then
				actor^.RotateTo(at);
		end;
	begin
		Maybe(player);
		Maybe(kazah);
		Maybe(twinkle);
		Maybe(valera);
	end;

	procedure Ep_Mars.DrawShipFireBacklight(const view: Transform2; const latPhase, longPhase: float; pass: uint; const stretch: Vec2; const alphaK: float);
	const
		BaseColor: Vec3 = (data: (0.3, 0.6, 0.9));
	var
		point, invp, a, b, d: Vec2;
		vertices: array[0 .. 4] of Vec2f;
		colors: array[0 .. 4] of Vec4f;
		color: Vec4;
		dynamicWeight: float;
		i: uint;
	begin
		invp := mgr^.invp;
		a := ship^.PointOn(Vec2.Make(0.9, 0.9));
		b := ship^.PointOn(Vec2.Make(0.9, 0.05));
		dynamicWeight := max(0, 1-1.0*abs(stretch.x));
		d := Rotation2(Pi/2 * smoothstep(ShipFireShowingTime, 0, latPhase) +
			0.2*smoothstep(0.5, 0.5 + ShipFireShowingTime, latPhase)*sin(ShipFireLatPhaseVelocity*latPhase)*(1-0.5*pass)*dynamicWeight +
			0.1*(1-pass)) *
			Vec2.Make(0.8 + 0.15*sin(ShipFireLongPhaseVelocity*longPhase) * dynamicWeight, 0);

		for i := 0 to 4 do
		begin
			case i of
				0: begin point := ship^.PointOn(Vec2.Make(0.75, 0.4)); color := Vec4.Make(BaseColor.x, BaseColor.y, BaseColor.z, 0.4); end;
				1: begin point := a; color := Vec4.Make(BaseColor.x, BaseColor.y, BaseColor.z, 0.4); end;
				2: begin point := b; color := Vec4.Make(BaseColor.x, BaseColor.y, BaseColor.z, 0.4); end;
				3: begin point := a + d + stretch; color := Vec4.Make(BaseColor.x, BaseColor.y, BaseColor.z, 0); end;
				4: begin point := b + Vec2.Make(d.x + stretch.x, -d.y - stretch.y); color := Vec4.Make(BaseColor.x, BaseColor.y, BaseColor.z, 0); end;
			end;

			vertices[i] := view * point * invp;
			colors[i] := color;
		end;
		if latPhase < 1.5 then
			for i := 0 to 4 do
				colors[i, 3] *= smoothstep(0, 1.5, latPhase);
		for i := 0 to 4 do
			colors[i, 3] *= alphaK;

		gl.Disable(gl.TEXTURE_2D);
		gl.L.DisableClientState(gl.L.TEXTURE_COORD_ARRAY);
		gl.L.EnableClientState(gl.L.COLOR_ARRAY);

		gl.L.VertexPointer(2, gl.FLOAT_TYPE, sizeof(vertices[0]), @vertices[0, 0]);
		gl.L.ColorPointer(4, gl.FLOAT_TYPE, sizeof(colors[0]), @colors[0, 0]);

		if pass = 0 then
			gl.DrawArrays(gl.TRIANGLE_STRIP, 0, 5)
		else
		begin
			gl.BlendFunc(gl.SRC_ALPHA, gl.ONE);
			gl.DrawArrays(gl.TRIANGLE_STRIP, 0, 5);
			gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
		end;

		gl.L.DisableClientState(gl.L.COLOR_ARRAY);
		gl.L.EnableClientState(gl.L.TEXTURE_COORD_ARRAY);
		gl.Enable(gl.TEXTURE_2D);
	end;

	procedure Ep_Mars.ApplyActorsOffset(const delta: Vec2);
	begin
		player^.local := roxLocalBase * Translate(delta);
		valera^.local := valeraLocalBase * Translate(delta);
		twinkle^.local := twinkleLocalBase * Translate(delta);
		kazah^.local := kazahLocalBase * Translate(delta);
	end;

	procedure Ep_Mars.SwitchState(state: StateEnum);
	begin
		self.state := state;
		self.stateTime := 0;
	end;

end.

