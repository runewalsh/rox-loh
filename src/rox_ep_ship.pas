{$include opts.inc}
unit rox_ep_ship;

interface

uses
	USystem, UMath, Utils,
	rox_state_adventure, rox_location, rox_actor, rox_decoration, rox_paths, rox_world, rox_timer, rox_gfx;

type
	pEp_Ship = ^Ep_Ship;
	Ep_Ship = object(Adventure)
	type
		EnterMode = (AutoTransition, EnterThroughDoor);
	var
		door: pDecoration;
		valera, twinkle, kazah: pActor;
		state: (Idle, MovingOutsideRequested);
		valeraDlg1Stage: (ObzhoryEbaniye, RoxEsliTakHocheshIdiPervyi);
		fadeMode: (NoFade, FadeIn);
		fade: float;
		constructor Init(world: pWorld; enter: EnterMode);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;
	const
		StateID = 'ep_ship';

		TwinkleAngle1 = -Pi/4;
		KazahAngle1 = -3*Pi/4;
		ValeraAngle1 = Pi;
		AmmoLoad = 5;
	end;

	procedure GenericFakeHit(ac: pActor; shooter: pNode; e: pAdventure; const dlg: string);

implementation

uses
	rox_ep_bar, rox_ep_mars;

	function DoorTest(n: pNode; const pos: Vec2; t: pTrigger; param: pointer): boolean;
	var
		e: pEp_Ship absolute param;
	begin
		Assert(@param = @param);
		result := (n = pNode(e^.player)) and
			Rect.Make(t^.local.trans, t^.local.trans + t^.size).Contains(pos) and
			(abs(Angle(t^.PointOn(Vec2.Make(0.5, 0.5)) - pos, Rotation2(pActor(n)^.angle).ToDirection)) < Pi/4);
	end;

	procedure DoorTrigger(n: pNode; reason: Trigger.Reason; param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		Assert(n = n);
		case reason of
			Entered: e^.door^.texRect := Rect.MakeSize(1/2, 0, 1/2, 1);
			Leaved: e^.door^.texRect := Rect.MakeSize(0, 0, 1/2, 1);
		end;
	end;

	procedure DoorActivate(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		Assert(t = t);
		if activator <> pNode(e^.player) then exit;
		if t^.HasInside(activator) then e^.state := MovingOutsideRequested;
	end;

	procedure RotateValeraBack(param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		e^.valera^.RotateTo(e^.valera^.HeartPos + Rotation2(Ep_Ship.ValeraAngle1).ToDirection);
	end;

	procedure RotateTwinkleBack(param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		e^.twinkle^.RotateTo(e^.twinkle^.HeartPos + Rotation2(Ep_Ship.TwinkleAngle1).ToDirection);
	end;

	procedure RotateKazahBack(param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		e^.kazah^.RotateTo(e^.kazah^.HeartPos + Rotation2(Ep_Ship.KazahAngle1).ToDirection);
	end;

	function NonCriticalDialoguePrologue(e: pEp_Ship; t: pTrigger; activator: pNode): boolean;
	begin
		Assert(t = t);
		result := (activator = pNode(e^.player)) and not e^.dlg.Valid;
	end;

	procedure Dialogue_Valera(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Ship absolute param;
		scenario: string;
	begin
		if not NonCriticalDialoguePrologue(e, t, activator) then exit;

		case e^.valeraDlg1Stage of
			ObzhoryEbaniye, RoxEsliTakHocheshIdiPervyi:
				begin
					e^.valera^.RotateTo(e^.player^.HeartPos);
					case e^.valeraDlg1Stage of
						ObzhoryEbaniye:
							begin
								scenario := 'valera [sizeX = 180/800]: 6.png';
								e^.valeraDlg1Stage := RoxEsliTakHocheshIdiPervyi;
							end;
						else
							begin
								scenario := 'valera [sizeX = 360/800]: 7.png';
								e^.valeraDlg1Stage := ObzhoryEbaniye;
							end;
					end;

					e^.dlg.Init(e, scenario)^.Callbacks(nil, @RotateValeraBack, e);
				end;
		end;
	end;

	procedure Dialogue_Twinkle(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		if not NonCriticalDialoguePrologue(e, t, activator) then exit;
		e^.twinkle^.RotateTo(e^.player^.HeartPos);
		e^.dlg.Init(e, 'twinkle [face = eating.png, sizeX = 140/800]: 3.png')^.Callbacks(nil, @RotateTwinkleBack, e);
	end;

	procedure Dialogue_Kazah(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		if not NonCriticalDialoguePrologue(e, t, activator) then exit;
		e^.kazah^.RotateTo(e^.player^.HeartPos);
		e^.dlg.Init(e, 'kazah [face = eating.png, sizeX = 140/800]: 3.png')^.Callbacks(nil, @RotateKazahBack, e);
	end;

	procedure AddAmmo(e: pEp_Ship);
	begin
		e^.player^.bullets := Max(e^.player^.bullets, e^.AmmoLoad);
	end;

	procedure ProcessShootHint(timer: pTimer; const dt: float; param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		Assert(dt = dt);
		if e^.player^.bullets < e^.AmmoLoad then timer^.left := min(timer^.left, 2.0);
	end;

	procedure AddAmmoAndShowShootingHintAfterMonologue(param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		AddAmmo(e);
		e^.OpenHint('hint-shoot.png', 20, @ProcessShootHint, 0.6, -0.1);
	end;

	procedure AmmoActivate(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		Assert(t = t);
		if (activator <> pNode(e^.player)) or e^.dlg.Valid or (e^.player^.bullets >= Ep_Ship.AmmoLoad) then exit;
		if e^.world^.firstAmmoProceed then
			AddAmmo(e)
		else
		begin
			e^.dlg.Init(e,
				'rox [sizeX = 260/800]: 8.png >>' +
				'rox [face = saliva.png, sizeX = 560/800]: 9.png')^.Callbacks(nil, @AddAmmoAndShowShootingHintAfterMonologue, e);
			e^.world^.firstAmmoProceed := yes;
		end;
	end;

	procedure GenericFakeHit(ac: pActor; shooter: pNode; e: pAdventure; const dlg: string);
	begin
		ac^.ForceState('hit');
		if shooter = pNode(e^.player) then
		begin
			if not e^.dlg.Valid then
				e^.dlg.Init(e, dlg);
		end;
	end;

	procedure ValeraHit(ac: pActor; shooter: pNode; param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		GenericFakeHit(ac, shooter, e, 'valera [face = gloomy.png, sizeX = 350/800]: 8.png');
	end;

	procedure TwinkleHit(ac: pActor; shooter: pNode; param: pointer);
	var
		e: pEp_Ship absolute param;
		dlg: string;
	begin
		case e^.world^.nextTwinkleShotReaction of
			TwinkleShotReaction1: begin dlg := 'twinkle [face = x-eyes.png, sizeX = 300/800]: 4.png'; e^.world^.nextTwinkleShotReaction := TwinkleShotReaction2; end;
			else begin dlg := 'twinkle [face = x-eyes.png, sizeX = 90/800]: 5.png'; e^.world^.nextTwinkleShotReaction := TwinkleShotReaction1; end;
		end;
		GenericFakeHit(ac, shooter, e, dlg);
	end;

	procedure KazahHit(ac: pActor; shooter: pNode; param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		GenericFakeHit(ac, shooter, e, 'kazah [face = hit.png, sizeX = 140/800]: 4.png');
	end;

	constructor Ep_Ship.Init(world: pWorld; enter: EnterMode);
	var
		floor, seat, ammo: pDecoration;
	begin
		inherited Init(StateID, world);
		location^.limits := Rect.Make(0.15, -0, 1.8, 0.6);
		location^.AddWall(Rect.Make(0, 0.5, 2, 0.6));
		location^.AddWall(Rect.Make(1.1, 0, 2.1, 0.1), 1.12);

		pNode(floor) := (new(pDecoration, Init(Environment('ship-floor.png'), Transform2.Identity, Vec2.Make(1.2, Deduce))))^.SetLayer(-2)^.AddTo(location);
		// панель
		location^.AddWall(Rect.Make(0.11, 0.3, 0.55, 0.4), 1.1);
		location^.AddWall(Rect.Make(0.21, 0, 0.5, 0.1), 1.8);

		(new(pDecoration, Init(Environment('ship-extern.png'), Translate(Vec2.Make(66/311, -98/147) * floor^.size), Vec2.Make(floor^.size.x * (508/311), Deduce))))^.SetLayer(+1)^.AddTo(location);

		pNode(door) := (new(pDecoration, Init(Environment('ship-door.png'), Translate(Vec2.Make(117/311, 91/147) * floor^.size), Vec2.Make(floor^.size.x * (84/311), Deduce))))^.
			SetTexRect(Rect.Make(0, 0, 1/2, 1))^.AddTo(location);
		location^.AddWall(Rect.Make(0.57, 0.4, 0.75, 0.5));
		location^.AddWall(Rect.Make(0.53, 0.4, 0.65, 0.45), 1.1);
		(new(pTrigger, Init(door^.local * Translate(0, 0.05), door^.size - Vec2.Make(0, 0.05))))^.
			WithCallbacks(@DoorTest, @DoorTrigger, @DoorActivate, @self)^.AddTo(location);

		pNode(ammo) := (new(pDecoration, Init(Environment('ammo.png'), Translate(Vec2.Make(215/311, 86/147) * floor^.size), Vec2.Make(floor^.size.x * (74/311), Deduce))))^.AddTo(location);
		location^.AddWall(Rect.Make(0.92, 0.38, 1.1, 0.45));
		location^.AddWall(Rect.Make(0.86+0.02, 0.38, 0.97, 0.38+0.02), 1.12);
		location^.Add((new(pTrigger, Init(ammo^.local, ammo^.size)))^.WithCallbacks(nil, nil, @AmmoActivate, @self));

		pNode(seat) := (new(pDecoration, Init(Environment('seat.png'), Translate(Vec2.Make(59/311, 67/147) * floor^.size), Vec2.Make(floor^.size.x * (61/311), Deduce))))^.AddTo(location);
		location^.AddWall(Rect.Make(0.25, 0.25, 0.42, 0.3), [NotObstacleForBullets]);

		(new(pDecoration, Init(Environment('leg.png'), Translate(Vec2.Make(148/311, 30/147) * floor^.size), Vec2.Make(floor^.size.x * (60/311), Deduce))))^.AddTo(location);
		location^.AddObstacle(Circle.Make(0.72, 0.17, 0.06));

		valera := CreateKolobok('valera');
		valera^.angle := ValeraAngle1;
		valera^.SetParent(seat);
		valera^.local := seat^.local * Translate(0.0, 0.035);
		valera^.OnHit(@ValeraHit, @self);
		location^.Add(valera);
		(new(pTrigger, Init(valera^.local, valera^.size + Vec2.Make(0.05, 0))))^.WithCallbacks(nil, nil, @Dialogue_Valera, @self)^.AddTo(location);

		twinkle := CreateKolobok('twinkle');
		twinkle^.angle := TwinkleAngle1;
		twinkle^.local := Translate(0.57, 0.18);
		twinkle^.OnHit(@TwinkleHit, @self);
		location^.Add(twinkle);
		(new(pTrigger, Init(twinkle^.local, twinkle^.size)))^.WithCallbacks(nil, nil, @Dialogue_Twinkle, @self)^.AddTo(location);

		kazah := CreateKolobok('kazah');
		kazah^.angle := KazahAngle1;
		kazah^.local := Translate(0.77, 0.2);
		kazah^.OnHit(@KazahHit, @self);
		location^.Add(kazah);
		(new(pTrigger, Init(kazah^.local, kazah^.size)))^.WithCallbacks(nil, nil, @Dialogue_Kazah, @self)^.AddTo(location);

		case enter of
			AutoTransition:
				begin
					player^.local := Translate(0.35, 0.11);
					player^.angle := -HalfPi;
					fadeMode := FadeIn;
				end;
			EnterThroughDoor:
				begin
					player^.local := Translate(0.4, 0.2);
					player^.angle := -HalfPi;
				end;
		end;
		location^.Add(player);
		world^.spaceshipArrivedOnMars := yes; {$note}
	end;

	destructor Ep_Ship.Done;
	begin
		Release(valera); Release(twinkle); Release(kazah);
		inherited Done;
	end;

	procedure Ep_Ship.HandleUpdate(const dt: float);
	begin
		if state = MovingOutsideRequested then
		begin
			mgr^.Switch(new(pEp_Mars, Init(world)));
			exit;
		end;

		case fadeMode of
			NoFade: ;
			FadeIn: begin fade := min(fade + dt, 1.0); if fade >= 1.0 then fadeMode := NoFade; end;
		end;

		inherited HandleUpdate(dt);
	end;

	procedure Ep_Ship.HandleDraw;
	var
		q: Quad;
	begin
		inherited HandleDraw;
		case fadeMode of
			FadeIn:
				begin
					q.fields := [q.Field.Color];
					q.color := Vec4.Make(0, 0, 0, 1 - fade);
					q.Draw(nil, -mgr^.nvp, 2 * mgr^.nvp, Vec2.Zero, Vec2.Ones);
				end;
		end;
	end;

end.

