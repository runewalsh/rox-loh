{$include opts.inc}
unit rox_ep_mars;

interface

uses
	USystem, UMath, Utils, U_GL,
	rox_decoration, rox_location, rox_world, rox_state_adventure, rox_paths, rox_gfx, rox_gl, rox_timer;

type
	pIris = ^Iris;
	Iris = object(Decoration)
		dir: Vec2;
		procedure HandleDraw(const view: Transform2); virtual;
	end;

	pEp_Mars = ^Ep_Mars;
	Ep_Mars = object(Adventure)
		ship, shipFire, eye: pDecoration;
		iris: pIris;
		baseIrisSize: Vec2;
		state: (_1_SetupShip, _1_ShipFlying, _1_ShipFlied, _1_Fadeout, _2_SetupShip, MoveToShip, Idle);
		stateTime: float;
		constructor Init(world: pWorld);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;
	private
		procedure UpdateIris;
		function ShipArrivalOrigin: Vec2;
		function ShipStanding: Vec2;
	const
		StateID = 'ep_mars';
		Fadeout1 = 1.0;
		FASTER = 1/6;
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

	procedure ActivateShipTrigger(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Mars absolute param;
	begin
		if (activator <> pNode(e^.player)) or not t^.HasInside(activator) then exit;
		e^.state := MoveToShip;
	end;

	constructor Ep_Mars.Init(world: pWorld);
	begin
		inherited Init(StateID, world);
		(new(pDecoration, Init(Environment('land.png'), Translate(-0.4, -0.5), Vec2.Make(2.7, Deduce))))^.SetLayer(-2)^.AddTo(location);
		location^.AddWall(Rect.Make(-0.2, -0.1, 1.5, 0), 27.5 * Deg2Rad, [NotObstacleForBullets]);
		location^.AddWall(Rect.Make(1.17, 0.65, 2.3, 0.7), [NotObstacleForBullets]);
		location^.AddWall(Rect.Make(2.2, -1, 2.3, 0.7), [NotObstacleForBullets]);
		location^.AddWall(Rect.Make(Vec2.Make(0.475, -0.388) + Rotate((22 - 90) * Deg2Rad) * Vec2.PositiveX, Vec2.Make(0.475 + 2.2, -0.288)),
			22 * Deg2Rad, [NotObstacleForBullets]);
		location^.AddWall(Rect.Make(-0.35, -0.3, 1, 0), -23 * Deg2Rad, [NotObstacleForBullets]);
		location^.AddObstacle(Circle.Make(0.67, 0.61, 0.3));
		location^.AddObstacle(Circle.Make(2.05, 0.72, 0.1));

		pNode(eye) := new(pDecoration, Init(Environment('land-eye.png'), Translate(-0.4 + 2.7*0.59, -0.5 + 2.7*(616/873)*0.47), Vec2.Make(0.27, Deduce)))^.
			SetAnim(Rect.Make(0, 0, 1/4, 1), 4, 0.5, no)^.AddTo(location);
		eye^.mirroredLoop := yes;
		eye^.relHeart := Vec2.Make(0.5, 0.1);

		baseIrisSize := Vec2.Make(0.05, 0.05);
		pNode(iris) := new(pIris, Init(Environment('iris.png'), Translate(Vec2.Make(0.52, 0.5) * eye^.size), baseIrisSize))^.AddTo(location);
		iris^.SetParent(eye);
		location^.AddWall(Rect.Make(eye^.local.trans + Vec2.Make(0.1, 0.25) * eye^.size, eye^.local.trans + Vec2.Make(0.9, 0.35) * eye^.size));

		if self.world^.spaceshipArrivedOnMars then
		begin
			location^.limits := Rect.Make(-0.6, -0.5, 2.4, 1.3);
			self.player^.local := Translate(0.54, -0.146);
			self.player^.angle := HalfPi;
			self.player^.AddTo(location);
			(new(pTrigger, Init(Translate(ShipStanding + Vec2.Make(0.2, 0.2)), Vec2.Make(0.5, 0.1))))^.WithCallbacks(nil, nil, @ActivateShipTrigger, @self)^.AddTo(location);
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
						mgr^.AddTimer(new(pTimer, Init(FASTER*5.5, nil, @Dialogue_1, @self)), id);
					end else
					begin
						location^.AddWall(Rect.Make(ship^.local.trans + Vec2.Make(0, 0.1), ship^.local.trans + Vec2.Make(0.7, 0.2)));
						state := Idle;
					end;
					goto again;
				end;
			_1_ShipFlying:
				begin
					sp := stateTime/(FASTER*6);
					if sp >= 1 then
					begin
						sp := 1;
						state := _1_ShipFlied;
					end;
					if (sp >= 0.7) and Assigned(shipFire^.location) then shipFire^.Detach;
					spy := clamp(stateTime/(FASTER*4), 0, 1);
					ship^.local := Translate(lerp(ShipArrivalOrigin, ShipStanding, Vec2.Make(-2*sqr(sp)/2+2*sp, -2*sqr(spy)/2+2*spy)));
					camera.target := ship^.HeartPos + Vec2.Make((-mgr^.nvp.x - 0.5*ship^.size.x) * (1-sp), 0.5 - 1*(1-sp));
				end;
		end;

		UpdateIris;
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

	procedure Ep_Mars.HandleDraw;
	var
		q: Quad;
	begin
		inherited HandleDraw;
		case state of
			_1_Fadeout:
				begin
					q.fields := [q.Field.Color];
					q.color := Vec4.Make(0, 0, 0, clamp(stateTime / Fadeout1, 0, 1));
					q.Draw(nil, -mgr^.nvp, 2 * mgr^.nvp, Vec2.Zero, Vec2.Ones);
				end;
		end;
	end;

	procedure Ep_Mars.UpdateIris;
	var
		eyeTarget: Vec2;
	begin
		case state of
			_1_ShipFlying, _1_ShipFlied, _1_Fadeout: eyeTarget := ship^.HeartPos;
			else eyeTarget := player^.local.trans + Vec2.Make(0.5, 0.9) * player^.size;
		end;
		iris^.dir := eyeTarget - eye^.HeartPos;
	end;

end.

