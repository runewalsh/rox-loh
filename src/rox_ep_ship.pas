{$include opts.inc}
unit rox_ep_ship;

interface

uses
	USystem, UMath, Utils, UClasses,
	rox_state_adventure, rox_location, rox_actor, rox_decoration, rox_paths, rox_world, rox_timer, rox_gfx, rox_gl;

type
	pEp_Ship = ^Ep_Ship;
	Ep_Ship = object(Adventure)
	type
		EnterMode = (AutoTransition, EnterThroughDoor, EnterThroughDoorWithFadeIn);
	var
		door, floor, secondSplat: pDecoration;
		valera, twinkle, kazah: pActor;
		valeraTrig, twinkleTrig, kazahTrig: pTrigger;
		valeraLocalBase, twinkleLocalBase, kazahLocalBase: Transform2;
		state: (Setup, Idle, MovingOutsideRequested, FadingOutForMovingOutside);
		valeraDlgStage, kazahDlgStage: uint;
		fadeMode: (NoFade, FadeIn, RedFlash, FadeOut);
		fade: float;
		closeHintOnUnwield, doorOpened: boolean;
		jump: DimensionalMove;
		constructor Init(world: pWorld; enter: EnterMode);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;
	const
		NormalStateID = 'ep_ship';
		BloodyStateID = 'ep_ship_bloody';
		AmmoLoad = 5;
	protected
		procedure UnwieldWeapon; virtual;
	private
		function PredefinedValeraAngle: float;
		function PredefinedTwinkleAngle: float;
		function PredefinedKazahAngle: float;
		procedure PlaceSecondSplat;
		procedure CheckDialogueEnd(lastSpeakedWith: pActor);
		procedure ApplyActorsOffset(const delta: Vec2);
		procedure SetDoorState(opened: boolean);
		procedure OpenDoorFor(const time: float);
	end;

	procedure ValeraHit(ac: pActor; shooter: pNode; param: pointer);
	procedure TwinkleHit(ac: pActor; shooter: pNode; param: pointer);
	procedure KazahHit(ac: pActor; shooter: pNode; param: pointer);

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
		e^.SetDoorState(reason = Entered);
	end;

	procedure DoorActivate(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		Assert(t = t);
		if activator <> pNode(e^.player) then exit;
		if t^.HasInside(activator) then
			if e^.world^.everyoneFled and not e^.world^.shipEyeBlinkProceed then
			begin
				e^.playerControlMode := PlayerControlDisabled;
				e^.world^.shipEyeBlinkProceed := yes;
				e^.state := FadingOutForMovingOutside;
				e^.fadeMode := FadeOut; e^.fade := 0;
				(new(pDecoration, Init(Environment('eye.png'), Translate(0.24, 0.09), Vec2.Make(0.08, Deduce))))^.
					SetAnim(Rect.MakeSize(0, 0, 1/5, 1), 5, 1.0, [DontLoop])^.AddTo(e^.location);
			end else
				e^.state := MovingOutsideRequested;
	end;

	procedure RotateValeraBack(param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		e^.valera^.RotateTo(e^.valera^.HeartPos + Rotation2(e^.PredefinedValeraAngle).ToDirection);
		e^.CheckDialogueEnd(e^.valera);
	end;

	procedure RotateTwinkleBack(param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		e^.twinkle^.RotateTo(e^.twinkle^.HeartPos + Rotation2(e^.PredefinedTwinkleAngle).ToDirection);
		e^.CheckDialogueEnd(e^.twinkle);
	end;

	procedure RotateKazahBack(param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		e^.kazah^.RotateTo(e^.kazah^.HeartPos + Rotation2(e^.PredefinedKazahAngle).ToDirection);
		e^.CheckDialogueEnd(e^.kazah);
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
		e^.valera^.RotateTo(e^.player^.HeartPos);
		if e^.world^.eyeExploded then
		begin
			case e^.valeraDlgStage of
				0: scenario := 'valera [face = gloomy.png]: 9.png';
				else scenario := 'valera: 10.png';
			end;
			e^.world^.shipSplatValeraDlgProceed := max(e^.world^.shipSplatValeraDlgProceed, e^.valeraDlgStage + 1);
			e^.valeraDlgStage := (e^.valeraDlgStage + 1) mod 2;
		end else
		begin
			case e^.valeraDlgStage of
				0: scenario := 'valera: 6.png';
				else scenario := 'valera: 7.png';
			end;
			e^.valeraDlgStage := (e^.valeraDlgStage + 1) mod 2;
		end;
		e^.dlg.Init(e, scenario)^.Callbacks(nil, @RotateValeraBack, e);
	end;

	procedure Dialogue_Twinkle(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		if not NonCriticalDialoguePrologue(e, t, activator) then exit;
		e^.twinkle^.RotateTo(e^.player^.HeartPos);
		if e^.world^.eyeExploded then
		begin
			e^.dlg.Init(e, 'twinkle [face = x-eyes.png]: 6.png')^.Callbacks(nil, @RotateTwinkleBack, e);
			e^.world^.shipSplatTwinkleDlgProceed := 1;
		end else
			e^.dlg.Init(e, 'twinkle [face = eating.png]: 3.png')^.Callbacks(nil, @RotateTwinkleBack, e);
	end;

	procedure Dialogue_Kazah(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Ship absolute param;
		scenario: string;
	begin
		if not NonCriticalDialoguePrologue(e, t, activator) then exit;
		e^.kazah^.RotateTo(e^.player^.HeartPos);
		if e^.world^.eyeExploded then
		begin
			case e^.kazahDlgStage of
				0: scenario := 'kazah: 5.png';
				else scenario := 'kazah: 6.png';
			end;
			e^.world^.shipSplatKazahDlgProceed := max(e^.world^.shipSplatKazahDlgProceed, e^.kazahDlgStage + 1);
			e^.kazahDlgStage := (e^.kazahDlgStage + 1) mod 2;
		end else
			scenario := 'kazah [face = eating.png]: 3.png';
		e^.dlg.Init(e, scenario)^.Callbacks(nil, @RotateKazahBack, e);
	end;

	procedure AddAmmo(e: pEp_Ship);
	begin
		e^.player^.bullets := Max(e^.player^.bullets, e^.AmmoLoad);
	end;

	procedure AddAmmoAndShowShootingHintAfterMonologue(param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		AddAmmo(e);
		e^.OpenHint('hint-shoot.png', 20, nil, 0.6, -0.1);
		e^.closeHintOnUnwield := yes;
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
				'rox: 8.png >>' +
				'rox [face = saliva.png]: 9.png')^.Callbacks(nil, @AddAmmoAndShowShootingHintAfterMonologue, e);
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
		e: pAdventure absolute param;
	begin
		GenericFakeHit(ac, shooter, e, 'valera [face = gloomy.png]: 8.png');
	end;

	procedure TwinkleHit(ac: pActor; shooter: pNode; param: pointer);
	var
		e: pAdventure absolute param;
		dlg: string;
	begin
		case e^.world^.nextTwinkleShotReaction of
			TwinkleShotReaction1: begin dlg := 'twinkle [face = x-eyes.png]: 4.png'; e^.world^.nextTwinkleShotReaction := TwinkleShotReaction2; end;
			else begin dlg := 'twinkle [face = x-eyes.png]: 5.png'; e^.world^.nextTwinkleShotReaction := TwinkleShotReaction1; end;
		end;
		GenericFakeHit(ac, shooter, e, dlg);
	end;

	procedure KazahHit(ac: pActor; shooter: pNode; param: pointer);
	var
		e: pAdventure absolute param;
	begin
		GenericFakeHit(ac, shooter, e, 'kazah [face = hit.png]: 4.png');
	end;

	procedure Dialogue_SeenEye(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		if not NonCriticalDialoguePrologue(e, t, activator) then exit;
		e^.dlg.Init(e, 'rox: 12.png');
	end;

	constructor Ep_Ship.Init(world: pWorld; enter: EnterMode);
	var
		seat, ammo, d: pDecoration;
		stateId: string;
	begin
		jump.Invalidate;
		if world^.eyeExploded then
			stateId := BloodyStateID
		else
			stateId := NormalStateID;
		inherited Init(stateID, world);
		location^.limits := Rect.Make(0.15, -0, 1.8, 0.6);
		location^.AddWall(Rect.Make(0, 0.5, 2, 0.6));
		location^.AddWall(Rect.Make(1.1, 0, 2.1, 0.1), 1.12);

		pNode(floor) := (new(pDecoration, Init(Environment('ship-floor.png'), Transform2.Identity, Vec2.Make(1.2, Deduce))))^.SetLayer(-2)^.AddTo(location);
		// панель
		location^.AddWall(Rect.Make(0.11, 0.3, 0.55, 0.4), 1.1);
		location^.AddWall(Rect.Make(0.21, 0, 0.5, 0.1), 1.8);

		(new(pDecoration, Init(Environment('ship-extern.png'), Translate(Vec2.Make(66/311, -98/147) * floor^.size), Vec2.Make(floor^.size.x * (508/311), Deduce))))^.
			SetLayer(+1)^.AddTo(location);

		pNode(door) := (new(pDecoration, Init(Environment('ship-door.png'), Translate(Vec2.Make(117/311, 91/147) * floor^.size), Vec2.Make(floor^.size.x * (84/311), Deduce))))^.
			SetTexRect(Rect.Make(0, 0, 1/2, 1))^.AddTo(location);
		location^.AddWall(Rect.Make(0.57, 0.4, 0.75, 0.5));
		location^.AddWall(Rect.Make(0.53, 0.4, 0.65, 0.45), 1.1);
		(new(pTrigger, Init(door^.local * Translate(0, -0.02), door^.size - Vec2.Make(0, -0.02))))^.
			WithCallbacks(@DoorTest, @DoorTrigger, @DoorActivate, @self)^.AddTo(location);

		pNode(ammo) := (new(pDecoration, Init(Environment('ammo.png'), Translate(Vec2.Make(215/311, 86/147) * floor^.size), Vec2.Make(floor^.size.x * (74/311), Deduce))))^.AddTo(location);
		location^.AddWall(Rect.Make(0.92, 0.39, 1.1, 0.45));
		location^.AddWall(Rect.Make(0.86+0.02, 0.39, 0.97, 0.39+0.02), 1.12);
		location^.Add((new(pTrigger, Init(ammo^.local, ammo^.size)))^.WithCallbacks(nil, nil, @AmmoActivate, @self));

		pNode(seat) := (new(pDecoration, Init(Environment('seat.png'), Translate(Vec2.Make(59/311, 67/147) * floor^.size), Vec2.Make(floor^.size.x * (61/311), Deduce))))^.AddTo(location);
		location^.AddWall(Rect.Make(0.25, 0.25, 0.42, 0.3), [NotObstacleForBullets]);

		if self.world^.eyeExploded then
		begin
			pNode(d) := (new(pDecoration, Init(Environment('bone.png'), Translate(Vec2.Make(173/311, 30/147) * floor^.size),
				Vec2.Make(floor^.size.x * (51/311), Deduce))))^.SetLayer(-1)^.AddTo(location);
			location^.AddWall(Rect.MakeSize(d^.local.trans + Vec2.Make(0.02), Vec2.Make(d^.size.x - 0.04, 0.001)), 0.5);
			(new(pDecoration, Init(Environment('splat3.png'), Translate(Vec2.Make(110/311, 32/147) * floor^.size), Vec2.Make(floor^.size.x * (68/311), Deduce))))^.
				SetLayer(-1)^.AddTo(location);

			if self.world^.everyoneFled then
				PlaceSecondSplat;
			if self.world^.shipEyeBlinkProceed then
				(new(pTrigger, Init(secondSplat^.local, Vec2.Make(0.16, 0.13))))^.WithCallbacks(nil, nil, @Dialogue_SeenEye, @self)^.AddTo(location);
		end else
		begin
			(new(pDecoration, Init(Environment('leg.png'), Translate(Vec2.Make(148/311, 30/147) * floor^.size), Vec2.Make(floor^.size.x * (60/311), Deduce))))^.AddTo(location);
			location^.AddObstacle(Circle.Make(0.72, 0.17, 0.06));
		end;

		if not self.world^.everyoneFled then
		begin
			pNode(valera) := CreateKolobok('valera')^.OnHit(@ValeraHit, @self)^.AddTo(location);
			valera^.angle := PredefinedValeraAngle;
			valera^.SetParent(seat);
			valera^.local := seat^.local * Translate(0.0, 0.04);
			pNode(valeraTrig) := (new(pTrigger, Init(valera^.local, valera^.size + Vec2.Make(0.05, 0))))^.WithCallbacks(nil, nil, @Dialogue_Valera, @self)^.AddTo(location);

			pNode(twinkle) := CreateKolobok('twinkle')^.OnHit(@TwinkleHit, @self)^.AddTo(location);
			twinkle^.angle := PredefinedTwinkleAngle;
			if self.world^.eyeExploded then twinkle^.local := Translate(0.7, 0.1) else twinkle^.local := Translate(0.57, 0.18);
			pNode(twinkleTrig) := (new(pTrigger, Init(twinkle^.local, twinkle^.size)))^.WithCallbacks(nil, nil, @Dialogue_Twinkle, @self)^.AddTo(location);

			pNode(kazah) := CreateKolobok('kazah')^.OnHit(@KazahHit, @self)^.AddTo(location);
			kazah^.angle := PredefinedKazahAngle;
			if self.world^.eyeExploded then kazah^.local := Translate(0.85, 0.15) else kazah^.local := Translate(0.77, 0.2);
			pNode(kazahTrig) := (new(pTrigger, Init(kazah^.local, kazah^.size)))^.WithCallbacks(nil, nil, @Dialogue_Kazah, @self)^.AddTo(location);
		end;

		case enter of
			AutoTransition:
				begin
					player^.local := Translate(0.35, 0.11);
					player^.angle := -HalfPi;
					fadeMode := FadeIn;
				end;
			EnterThroughDoor, EnterThroughDoorWithFadeIn:
				begin
					player^.local := Translate(0.51, 0.28);
					player^.angle := -HalfPi;
					if enter = EnterThroughDoorWithFadeIn then fadeMode := FadeIn;
				end;
		end;
		location^.Add(player);
	end;

	destructor Ep_Ship.Done;
	begin
		Release(valera); Release(twinkle); Release(kazah);
		jump.Done;
		inherited Done;
	end;

	procedure Ep_Ship.HandleUpdate(const dt: float);
	label again;
	var
		fadeVel: float;
	begin
	again: case state of
			Setup: state := Idle;
		end;

		if (state = MovingOutsideRequested) or (state = FadingOutForMovingOutside) and (fadeMode = NoFade) then
		begin
			mgr^.Switch(new(pEp_Mars, Init(world, state = FadingOutForMovingOutside)));
			exit;
		end;

		if fadeMode <> NoFade then
		begin
			case fadeMode of
				FadeIn: fadeVel := 0.75;
				RedFlash: fadeVel := 3.0;
				else {FadeOut} fadeVel := 0.5;
			end;
			fade := min(fade + fadeVel*dt, 1.0);
			if fade >= 1.0 then
			begin
				fadeMode := NoFade;
				goto again;
			end;
		end;

		inherited HandleUpdate(dt);
	end;

	procedure Ep_Ship.HandleDraw;
	var
		q: Quad;
	begin
		inherited HandleDraw;
		case fadeMode of
			FadeIn, RedFlash, FadeOut:
				begin
					q.fields := [q.Field.Color];
					case fadeMode of
						FadeIn: q.color := Vec4.Make(0, 0, 0, 1 - fade);
						RedFlash: q.color := Vec4.Make(0.6, 0, 0, 0.5 * (1 - min(10*fade, 1) * fade));
						else {FadeOut} q.color := Vec4.Make(0, 0, 0, fade);
					end;

					q.Draw(nil, -mgr^.nvp, 2 * mgr^.nvp, Vec2.Zero, Vec2.Ones);
				end;
		end;
	end;

	procedure Ep_Ship.UnwieldWeapon;
	begin
		inherited UnwieldWeapon;
		if closeHintOnUnwield then
		begin
			closeHintOnUnwield := no;
			if Assigned(hintTimer) then hintTimer^.left := min(hintTimer^.left, 3.0);
		end;
	end;

	function Ep_Ship.PredefinedValeraAngle: float; begin if world^.eyeExploded then result := -Pi/4 else result := Pi; end;
	function Ep_Ship.PredefinedTwinkleAngle: float; begin if world^.eyeExploded then result := 3*Pi/4 else result := -Pi/4; end;
	function Ep_Ship.PredefinedKazahAngle: float; begin if world^.eyeExploded then result := 3*Pi/4 else result := -3*Pi/4; end;

	procedure Ep_Ship.PlaceSecondSplat;
	begin
		pNode(secondSplat) := (new(pDecoration, Init(Environment('splat2.png'), Translate(Vec2.Make(47/311, 15/147) * floor^.size), Vec2.Make(floor^.size.x * (232/311), Deduce))))^.
			SetLayer(-1)^.AddTo(location);
	end;

	procedure Scene_Ao_11_ValeraDisappears(param: pointer);
	var e: pEp_Ship absolute param;
	begin
		e^.valera^.Detach; Release(e^.valera);
		e^.world^.everyoneFled := yes;
		e^.playerControlMode := PlayerControlEnabled;
	end;

	procedure Scene_Ao_10_ValeraOpensDoor(param: pointer);
	var e: pEp_Ship absolute param;
	begin
		e^.OpenDoorFor(0.5);
		e^.AddTimer(0.25, nil, @Scene_Ao_11_ValeraDisappears, e);
	end;

	procedure Scene_Ao_9_ValeraRunsAway(param: pointer);
	var e: pEp_Ship absolute param;
	begin
		e^.valera^.idclip := yes;
		e^.valera^.SetParent(nil);
		e^.valeraTrig^.Detach; e^.valeraTrig := nil;
		e^.valera^.MoveTo(e^.door^.PointOn(Vec2.Make(0.5, 0)), e^.RunningVelocity, @Scene_Ao_10_ValeraOpensDoor, e);
	end;

	procedure Scene_Ao_8_ValeraComplaints(param: pointer);
	var e: pEp_Ship absolute param;
	begin
		Assert(not e^.dlg.Valid);
		e^.valera^.RotateTo(e^.player^.HeartPos);
		e^.dlg.Init(e, 'valera: 11.png')^.Callbacks(nil, @Scene_Ao_9_ValeraRunsAway, e);
	end;

	procedure Scene_Ao_7_KazahDisappears(param: pointer);
	var e: pEp_Ship absolute param;
	begin
		e^.kazah^.Detach; Release(e^.kazah);
		e^.AddTimer(0.8, nil, @Scene_Ao_8_ValeraComplaints, e);
	end;

	procedure Scene_Ao_6_KazahOpensDoor(param: pointer);
	var e: pEp_Ship absolute param;
	begin
		e^.OpenDoorFor(0.5);
		e^.AddTimer(0.25, nil, @Scene_Ao_7_KazahDisappears, e);
	end;

	procedure Scene_Ao_5_KazahRunsAway(param: pointer);
	var e: pEp_Ship absolute param;
	begin
		e^.kazah^.idclip := yes;
		e^.kazahTrig^.Detach; e^.kazahTrig := nil;
		e^.kazah^.MoveTo(e^.door^.PointOn(Vec2.Make(0.5, 0)), e^.RunningVelocity, @Scene_Ao_6_KazahOpensDoor, e);
	end;

	procedure Scene_Ao_4_KazahComplaints(param: pointer);
	var e: pEp_Ship absolute param;
	begin
		Assert(not e^.dlg.Valid);
		e^.dlg.Init(e, 'kazah: 7.png')^.Callbacks(nil, @Scene_Ao_5_KazahRunsAway, e);
	end;

	procedure Scene_Ao_3_TwinkleDisappears(param: pointer);
	var e: pEp_Ship absolute param;
	begin
		e^.twinkle^.Detach; Release(e^.twinkle);
		e^.AddTimer(0.8, nil, @Scene_Ao_4_KazahComplaints, e);
	end;

	procedure Scene_Ao_2_TwinkleOpensDoor(param: pointer);
	var e: pEp_Ship absolute param;
	begin
		e^.OpenDoorFor(0.5);
		e^.AddTimer(0.25, nil, @Scene_Ao_3_TwinkleDisappears, e);
	end;

	procedure Scene_Ao_1_TwinkleRunsAway(param: pointer);
	var e: pEp_Ship absolute param;
	begin
		e^.twinkle^.idclip := yes;
		e^.twinkleTrig^.Detach; e^.twinkleTrig := nil;
		e^.twinkle^.MoveTo(e^.door^.PointOn(Vec2.Make(0.5, 0)), e^.RunningVelocity, @Scene_Ao_2_TwinkleOpensDoor, e);
	end;

	procedure Scene_Ao_1_JumpProcess(timer: pTimer; const dt: float; param: pointer);
	var
		e: pEp_Ship absolute param;
	begin
		Assert(e^.jump.Valid and (timer = timer));
		if not e^.jump.Finished then e^.jump.Process(dt);
		e^.ApplyActorsOffset(Vec2.Make(0, 0.15 * e^.jump.CurrentF));
	end;

	procedure Scene_Ao_1_JumpDone(param: pointer);
	var e: pEp_Ship absolute param;
	begin
		Assert(e^.jump.Valid);
		e^.jump.Done;
		e^.ApplyActorsOffset(Vec2.Zero);
		Assert(not e^.dlg.Valid);
		e^.dlg.Init(e, 'twinkle [face = scared.png, delay = 0.5]: 7.png')^.Callbacks(nil, @Scene_Ao_1_TwinkleRunsAway, e);
	end;

	procedure Ep_Ship.CheckDialogueEnd(lastSpeakedWith: pActor);
	begin
		if not Assigned(secondSplat) and
			(lastSpeakedWith = twinkle) and (world^.shipSplatValeraDlgProceed > 0) and (world^.shipSplatKazahDlgProceed > 0) or
			(lastSpeakedWith <> twinkle) and (world^.shipSplatValeraDlgProceed >= 2) and (world^.shipSplatKazahDlgProceed >= 2) and (world^.shipSplatTwinkleDlgProceed >= 1) then
		begin
			Assert(not jump.Valid);
			fadeMode := RedFlash;
			fade := 0;
			PlaceSecondSplat;
			playerControlMode := PlayerControlDisabled;
			valeraLocalBase := valera^.local;
			twinkleLocalBase := twinkle^.local;
			kazahLocalBase := kazah^.local;
			valera^.RotateTo(secondSplat^.HeartPos);
			twinkle^.RotateTo(secondSplat^.HeartPos);
			kazah^.RotateTo(secondSplat^.HeartPos);
			player^.RotateTo(secondSplat^.HeartPos);
			jump.Init(1);
			jump.path^.AddAUA(0.0, 0.0, 10.0, -5.0, 0.0, 0.3, 0, 0);
			AddTimer(jump.path^.len, @Scene_Ao_1_JumpProcess, @Scene_Ao_1_JumpDone, @self);
		end;
	end;

	procedure Ep_Ship.ApplyActorsOffset(const delta: Vec2);
	begin
		valera^.local := valeraLocalBase * Translate(delta);
		twinkle^.local := twinkleLocalBase * Translate(delta);
		kazah^.local := kazahLocalBase * Translate(delta);
	end;

	procedure Ep_Ship.SetDoorState(opened: boolean);
	begin
		if opened <> doorOpened then
		begin
			door^.texRect := Rect.MakeSize(IfThen(opened, 1/2, 0), 0, 1/2, 1);
			doorOpened := opened;
		end;
	end;

	procedure CloseDoorTimer(param: pointer);
	var e: pEp_Ship absolute param;
	begin
		e^.SetDoorState(no);
	end;

	procedure Ep_Ship.OpenDoorFor(const time: float);
	begin
		if not doorOpened then
		begin
			SetDoorState(yes);
			AddTimer(time, nil, @CloseDoorTimer, @self);
		end;
	end;

end.

