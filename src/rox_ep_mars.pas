{$include opts.inc}
unit rox_ep_mars;

interface

uses
	USystem, UMath, Utils, U_GL, UClasses, Random,
	rox_decoration, rox_location, rox_world, rox_state_adventure, rox_paths, rox_gfx, rox_gl, rox_timer, rox_actor;

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
		ship, shipFire, eye: pDecoration;
		iris: pIris;
		baseIrisSize: Vec2;
		state: (_1_SetupShip, _1_ShipFlying, _1_ShipFlied, _1_Fadeout, _2_SetupShip, MoveToShip, RedFlash, Idle);
		stateTime: float;

		blooddropTrigger: pTrigger;
		bloodDropsOnFeet: uint;
		lastBloodyFootprint: Vec2; // мусор при bloodDropsOnFeet = 0
		nextFootprintL: boolean;
		nextSentenceAboutEye: uint;

		constructor Init(world: pWorld);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;
		procedure HandleDeactivation; virtual;
	private
		procedure UpdateIris;
		function ShipArrivalOrigin: Vec2;
		function ShipStanding: Vec2;
		function EyePos: Vec2;
		function ReferencePlayerPositionForBloodyFootprint: Vec2;
		procedure PlaceExplodedEyeLeftover;
		function BaseEyeRect: Rect;
		function ExplodedEyeRect: Rect;
	const
		StartingStateID = 'ep_mars_starting';
		EyeExplodedStateID = 'ep_mars_eye_exloded';
		AllOutsideStateID = 'ep_mars_all_outside';
		Fadeout1 = 1.0;
		PlayerPosAtExit: Vec2 = (data: (0.4, 0.23));
		EyeCollision = 'eye';
		BaseEyeSize: Vec2 = (data: (0.27, 0.27/100*60));
		RedFlashTime = 3.0;
	end;

implementation

uses
	rox_ep_ship;

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

	procedure MovingToShipProceed(reason: Actor.MoveCallbackReason; param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		if reason <> TargetReached then exit;
		e^.state := MoveToShip;
	end;

	procedure ActivateShipTrigger(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		if (activator <> pNode(e^.player)) or not t^.HasInside(activator) then exit;
		e^.playerControlMode := PlayerControlDisabled;
		e^.player^.MoveTo(e^.ShipStanding + e^.PlayerPosAtExit, e^.WalkingVelocity, @MovingToShipProceed, e);
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
			SetAnim(Rect.Make(0, 0, 1/3, 1), 3, 0.5, yes));
		e^.eye^.Detach; e^.eye := nil;
		e^.iris^.Detach; e^.iris := nil;
		e^.location^.RemoveWall(e^.EyeCollision);

		e^.mgr^.bgm.Priority(e^.id)^.SetModifier('mute', op_Set, 0, +999);
		e^.world^.eyeExploded := yes;
		e^.PlaceExplodedEyeLeftover;

		if e^.state = Idle then
		begin
			e^.state := RedFlash;
			e^.stateTime := 0;
		end;
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
		if e^.world^.eyeExploded then scenario := 'rox [sizeX = 50/800]: 12.png' else
		begin
			case e^.nextSentenceAboutEye of
				0: scenario := 'rox [face = scared.png, sizeX = 75/800]: 10.png';
				else scenario := 'rox [sizeX = 190/800]: 11.png';
			end;
			e^.nextSentenceAboutEye := (e^.nextSentenceAboutEye + 1) mod 2;
		end;
		e^.dlg.Init(e, scenario);
	end;

	constructor Ep_Mars.Init(world: pWorld);
	const
		CommentOnEyeTriggerBias: Vec2 = (data: (0.05, 0.03));
	var
		stateId: string;
	begin
		if world^.eyeExploded then stateId := EyeExplodedStateID
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
				SetAnim(Rect.Make(0, 0, 1/4, 1), 4, 0.5, no)^.AddTo(location);
			eye^.mirroredLoop := yes;
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
			state := _2_SetupShip;
		end else
		begin
			cameraMode := LookPredefined;
			location^.limits := Rect.Make(-5, -3, 5, 3);
			state := _1_SetupShip;
		end;
	end;

	destructor Ep_Mars.Done;
	begin
		Release(ship); Release(shipFire);
		inherited Done;
	end;

	procedure Dialogue_1_1(param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		e^.state := _1_Fadeout;
		e^.stateTime := 0;
	end;

	procedure Dialogue_1(reason: Timer.DoneReason; param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		if reason <> Timeout then exit;
		Assert(not e^.dlg.Valid);
		e^.dlg.Init(e,
			'rox [face = scared.png, sizeX = 250/800]: 7.png >>' +
			'twinkle [sizeX = 260/800]: 2.png >>' +
			'kazah [sizeX = 230/800]: 2.png >>' +
			'valera [sizeX = 730/800]: 4.png >>' +
			'valera [sizeX = 310/800]: 5.png');
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
					(new(pDecoration, Init(Character('valera', 'in_ship.png'), Translate(ship^.size * Vec2.Make(1-239/283, 90/177)),
						Vec2.Make(-0.9 * (21/283), Deduce))))^.SetParent(ship)^.AddTo(location);

					if state = _1_SetupShip then
					begin
						shipFire := new(pDecoration, Init(Environment('ship-fire.png'), Translate(ship^.size * Vec2.Make(1-(-9-91)/283, (-23+10)/177)),
							Vec2.Make(-0.9 * (168/283), Deduce)))^.SetAnim(Rect.MakeSize(Vec2.Zero, Vec2.Make(1/3, 1)), 3, 0.3, no)^.SetLayer(+1)^.SetParent(ship)^.
							AddTo(location)^.NewRef;
						state := _1_ShipFlying;
						stateTime := 0;
						mgr^.AddTimer(new(pTimer, Init(5.5, nil, @Dialogue_1, @self)), id);
					end else
					begin
						location^.AddWall(Rect.Make(ship^.local.trans + Vec2.Make(0, 0.1), ship^.local.trans + Vec2.Make(0.7, 0.15)));
						state := Idle;
					end;
					goto again;
				end;
			_1_ShipFlying:
				begin
					sp := stateTime/6;
					if sp >= 1 then
					begin
						sp := 1;
						state := _1_ShipFlied;
					end;
					if (sp >= 0.7) and Assigned(shipFire^.location) then shipFire^.Detach;
					spy := clamp(stateTime/4, 0, 1);
					ship^.local := Translate(lerp(ShipArrivalOrigin, ShipStanding, Vec2.Make(-2*sqr(sp)/2+2*sp, -2*sqr(spy)/2+2*spy)));
					camera.target := ship^.HeartPos + Vec2.Make((-mgr^.nvp.x - 0.5*ship^.size.x) * (1-sp), 0.5 - 1*(1-sp));
				end;
			RedFlash: if stateTime >= RedFlashTime then state := Idle;
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
			_1_Fadeout:
				if stateTime > Fadeout1 then
				begin
					world^.spaceshipArrivedOnMars := yes;
					mgr^.Switch(new(pEp_Ship, Init(world, AutoTransition)));
				end;
			MoveToShip: mgr^.Switch(new(pEp_Ship, Init(world, EnterThroughDoor)));
		end;
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

	procedure Ep_Mars.HandleDraw;
	var
		q: Quad;
	begin
		inherited HandleDraw;
		case state of
			_1_Fadeout, RedFlash:
				begin
					q.fields := [q.Field.Color];
					case state of
						_1_Fadeout: q.color := Vec4.Make(0, 0, 0, clamp(stateTime / Fadeout1, 0, 1));
						else {RedFlash} q.color := Vec4.Make(0.8 - 0.4 * min(1.0, 3.0 * stateTime / RedFlashTime), 0.0, 0.0,
							0.8 * min(1.0, 40.0 * stateTime / RedFlashTime) * smoothstep(1.0 - stateTime / RedFlashTime));
					end;
					q.Draw(nil, -mgr^.nvp, 2 * mgr^.nvp, Vec2.Zero, Vec2.Ones);
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

end.

