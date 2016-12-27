{$include opts.inc}
unit rox_ep_bar;

interface

uses
	USystem, UMath, Utils, UClasses, Random,
	rox_state_adventure, rox_actor, rox_location, rox_decoration, rox_paths, rox_world, rox_dialogue, rox_timer;

type
	pEp_Bar = ^Ep_Bar;
	Ep_Bar = object(Adventure)
		door: pDecoration;
		valera, twinkle, kazah, obrub: pActor;

		obrubState: (ObrubStanding, ObrubStandingAndWatching, ObrubWalking, ObrubFiring);
		obrubMood: (ObrubIndifferent, ObrubAnnoyed, ObrubGettingAngry, ObrubAngry);
		obrubTimer: pTimer;
		constructor Init(world: pWorld);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDeactivation; virtual;
	private
		state: (Setup, Idle, MovingOutsideRequested);
		procedure SetupObrubWandering;
		procedure StopObrubWandering;
	const
		StateID = 'ep_bar';
	end;

	function CreateKolobok(const char: string): pActor;
	procedure RotateActors(const actors: array of pActor; const target: Vec2);

implementation

uses
	rox_ep_entry;

	function DoorTest(n: pNode; const pos: Vec2; t: pTrigger; param: pointer): boolean;
	var
		e: pEp_Bar absolute param;
	begin
		Assert(@param = @param);
		result := (n = pNode(e^.player)) and (n^.local.trans.y > t^.local.trans.y) and
			(abs(Angle(t^.local.trans + Vec2.Make(0.5 * t^.size.x, 0) - pos, Rotation2(pActor(n)^.angle).ToDirection)) < Pi/5);
	end;

	procedure DoorTrigger(n: pNode; reason: Trigger.Reason; param: pointer);
	var
		e: pEp_Bar absolute param;
	begin
		Assert(n = n);
		case reason of
			Entered: e^.door^.texRect := Rect.MakeSize(1/3, 0, 1/3, 1);
			Leaved: e^.door^.texRect := Rect.MakeSize(0, 0, 1/3, 1);
		end;
	end;

	procedure DoorActivate(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Bar absolute param;
	begin
		Assert(t = t);
		if activator <> pNode(e^.player) then exit;
		if t^.HasInside(e^.player) then e^.state := MovingOutsideRequested;
	end;

	procedure Dialogue_4(reason: Timer.DoneReason; param: pointer);
	var
		e: pEp_Bar absolute param;
	begin
		if reason = Timeout then
		begin
			e^.world^.spaceshipArrived := yes;
			if e^.obrubMood = ObrubGettingAngry then e^.obrubMood := ObrubAngry;
			e^.playerControlMode := PlayerControlEnabled;
			e^.player^.idclip := no;

			if not e^.dlg.Valid then e^.dlg.Init(e, 'rox [sizeX = 630/800]: 6.png');
		end;
	end;

	procedure Dialogue_4_QuitScene(reason: Actor.MoveCallbackReason; ac: pActor; param: pointer);
	begin
		Assert(param = param);
		if reason = TargetReached then ac^.Detach;
	end;

	procedure RollKoloboks(t: pTimer; const dt: float; param: pointer);
		function Roll(var ac: Actor; const vel: float): boolean;
		var
			dAn, newDAn: float;
			h: Vec2;
		begin
			if ac.mvMethod = MovingTo then dAn := AngleDiff(ArcTan2(ac.mvPointOrDelta - ac.HeartPos), ac.angle);
			h := ac.local.trans + ac.local.rot * (0.5 * ac.size);
			ac.local := Translate(h) * (Rotate(-vel * dt) * (Translate(-h) * ac.local));

			if ac.mvMethod = MovingTo then
			begin
				newDAn := AngleDiff(ArcTan2(ac.mvPointOrDelta - ac.HeartPos), ac.angle);
				if abs(dAn) < abs(newDAn) then ac.angle := NormalizeAngle(ac.angle + AngleDiff(newDAn, dAn));
			end;
			result := Assigned(ac.location);
		end;
	var
		e: pEp_Bar absolute param;
		done: boolean;
	begin
		done := yes;
		done := not Roll(e^.valera^, 6) and done;
		if done then t^.Stop;
	end;

	procedure StartRolling(reason: Timer.DoneReason; param: pointer);
	var
		e: pEp_Bar absolute param;
	begin
		if reason <> Timeout then exit;
		e^.mgr^.AddTimer(new(pTimer, Init(99, @RollKoloboks, nil, e)), e^.id);
	end;

	procedure Dialogue_3(param: pointer);
	// Конец диалога: все убегают, Рокс поворачивается вслед и выдаёт последнюю фразу.
	var
		e: pEp_Bar absolute param;
		t: pTimer;
	begin
		e^.twinkle^.idclip := yes;
		e^.twinkle^.MoveTo(e^.door^.HeartPos + Vec2.Make(0.03, 0.03), lerp(Ep_Bar.WalkingVelocity, Ep_Bar.RunningVelocity, 0.45), @Dialogue_4_QuitScene, nil);

		e^.kazah^.idclip := yes;
		e^.kazah^.MoveTo(e^.door^.HeartPos + Vec2.Make(-0.04, 0), lerp(Ep_Bar.WalkingVelocity, Ep_Bar.RunningVelocity, 0.44), @Dialogue_4_QuitScene, nil);

		e^.valera^.idclip := yes;
		e^.valera^.MoveTo(e^.door^.HeartPos, lerp(Ep_Bar.WalkingVelocity, Ep_Bar.RunningVelocity, 0.37), @Dialogue_4_QuitScene, nil);
		// e^.mgr^.AddTimer(new(pTimer, Init(1, nil, @StartRolling, e)), e^.id);
		StartRolling(Timeout, e);

		e^.player^.RotateTo(e^.door^.HeartPos);

		t := new(pTimer, Init(1.6, nil, @Dialogue_4, e));
		e^.mgr^.AddTimer(t, e^.id);
	end;

	procedure RotateFour(at: pActor; e: pEp_Bar);
		procedure Maybe(actor: pActor);
		begin
			if actor <> at then actor^.RotateTo(at^.HeartPos);
		end;
	begin
		Maybe(e^.player);
		Maybe(e^.valera);
		Maybe(e^.twinkle);
		Maybe(e^.kazah);
	end;

	procedure Dialogue_2_Item(id: uint; what: Dialogue.ItemEvent; param: pointer);
	var
		e: pEp_Bar absolute param;
	begin
		case what of
			ItemStart:
				case id of
					2, 8: RotateFour(e^.valera, e);
					3: RotateFour(e^.player, e);
					4, 6: RotateFour(e^.twinkle, e);
					5, 7: RotateFour(e^.kazah, e);
				end;
		end;
	end;

	procedure Dialogue_2(reason: Actor.MoveCallbackReason; ac: pActor; param: pointer);
	// Рокс пришёл, начало диалога.
	var
		e: pEp_Bar absolute param;
	begin
		Assert(ac = ac);
		Assert(reason = TargetReached);

		e^.player^.RotateTo(e^.player^.HeartPos + Vec2.PositiveY);
		if e^.dlg.Valid then e^.dlg.Done;
		e^.dlg.Init(e,
			{0} 'valera [sizeX = 220/800]: 0.png >>' +
			{1} 'rox [sizeX = 560/800]: 4.png >>' +
			{2} 'valera [sizeX = 620/800]: 1.png >>' +
			{3} 'rox [face = x-eyes.png, sizeX = 230/800]: 5.png >>' +
			{4} 'twinkle [face = eyes-closed.png, sizeX = 540/800]: 0.png >>' +
			{5} 'kazah [face = suspicious.png, sizeX = 470/800]: 0.png >>' +
			{6} 'twinkle [face = x-eyes.png, sizeX = 348/800]: 1.png >>' +
			{7} 'kazah [sizeX = 108/800]: 1.png >>' +
			{8} 'valera [sizeX = 200/800]: 2.png')^.Callbacks(@Dialogue_2_Item, @Dialogue_3, e);
		RotateFour(e^.player, e);
	end;

	procedure Dialogue_1(t: pTrigger; activator: pNode; param: pointer);
	// Активация диалога между командой.
	// Сначала Рокс идёт за стол.
	var
		e: pEp_Bar absolute param;
	begin
		if activator <> pNode(e^.player) then exit;
		t^.Detach;
		e^.playerControlMode := PlayerControlDisabled;
		e^.player^.idclip := yes;
		e^.player^.MoveTo(Vec2.Make(-0.85, 0.34), Ep_Bar.WalkingVelocity, @Dialogue_2, e);
	end;

	procedure Dialogue_Obrub_RestartWandering(param: pointer);
	var
		e: pEp_Bar absolute param;
	begin
		e^.SetupObrubWandering;
	end;

	procedure Obrub_Aim(t: pTimer; const dt: float; param: pointer);
	var
		e: pEp_Bar absolute param;
	begin
		Assert((t = t) and (dt = dt));
		e^.obrub^.RotateTo(e^.player^.HeartPos);
	end;

	procedure Obrub_Shot(reason: Timer.DoneReason; param: pointer);
	var
		e: pEp_Bar absolute param;
		v: Vec2;
	begin
		if reason <> Timeout then exit;
		e^.obrub^.Fire;

		v := e^.player^.HeartPos - e^.obrub^.AimOrigin;
		(new(pDecoration, Init(Environment('splat.png'),
			Translate(e^.player^.HeartPos + 0.1 * v.Normalized) * Rotate(ArcTan2(v)) * Translate(0, -0.5 * 0.2),
			Vec2.Make(Deduce, 0.2))))^.SetLayer(-1)^.AddTo(e^.location);

		e^.player^.StopMoving;
		e^.playerControlMode := PlayerControlDisabled;
		e^.player^.SwitchToState('death');

		e^.obrub^.UnwieldWeapon;
		e^.SetupObrubWandering;
	end;

	procedure Dialogue_Obrub_2(param: pointer);
	// Обрубенс окончательно разозлился
	var
		e: pEp_Bar absolute param;
	begin
		e^.obrubState := ObrubFiring;
		e^.obrub^.WieldWeapon;
		e^.mgr^.AddTimer(new(pTimer, Init(2, @Obrub_Aim, @Obrub_Shot, e)), e^.id);
	end;

	procedure Dialogue_Obrub_1(t: pTrigger; activator: pNode; param: pointer);
	// реплики Обрубенса за стойкой
	var
		e: pEp_Bar absolute param;
	begin
		if (activator <> pNode(e^.player)) or e^.dlg.Valid then exit;
		e^.StopObrubWandering;
		e^.obrub^.RotateTo(e^.player^.HeartPos);

		case e^.obrubMood of
			ObrubIndifferent:
				begin
					e^.dlg.Init(e, 'obrubens [face = teasing.png, sizeX = 240/800]: 0.png')^.Callbacks(nil, @Dialogue_Obrub_RestartWandering, e);
					e^.obrubMood := ObrubAnnoyed;
				end;
			ObrubAnnoyed, ObrubGettingAngry:
				begin
					e^.dlg.Init(e, 'obrubens [face = annoyed.png, sizeX = 58/800, letterTimeout = 0.3]: 1.png')^.Callbacks(nil, @Dialogue_Obrub_RestartWandering, e);
					if e^.world^.spaceshipArrived then
						e^.obrubMood := ObrubAngry
					else
						e^.obrubMood := ObrubGettingAngry;
				end;
			ObrubAngry:
				begin
					t^.Detach;
					e^.mgr^.bgm.Priority(e^.id)^.SetModifier('mute', op_Set, 0, +999);
					e^.dlg.Init(e, 'obrubens [face = mad.png, sizeX = 100/800, letterTimeout = 0.2, delay = 1]: 2.png')^.Callbacks(nil, @Dialogue_Obrub_2, e);
					e^.dlg.skippable := no;
				end;
		end;
	end;

	constructor Ep_Bar.Init(world: pWorld);
	var
		d: pDecoration;
	begin
		inherited Init(StateID, world);
		if not Assigned(world) then player^.angle := HalfPi;

		location := new(pLocation, Init(@self))^.NewRef;
		location^.limits := Rect.Make(-1.2, -0.7, 1.2, 0.7);
		(new(pDecoration, Init(Environment('parquet.png'), Translate(-1.2, -0.7), Vec2.Make(2.4, 1.4))))^.
			SetTexRect(Rect.Make(Vec2.Zero, Vec2.Make(12, 7) * 0.5))^.SetLayer(-2)^.AddTo(location);

		door := new(pDecoration, Init(Environment('bar_door.png'), Translate(0.3, -0.9), Vec2.Make(0.3, 0.3*1.3)))^.
			SetTexRect(Rect.MakeSize(0, 0, 1/3, 1))^.NewRef;
		location^.Add(door);

		(new(pTrigger, Init(door^.local, door^.size))^.WithCallbacks(@DoorTest, @DoorTrigger, @DoorActivate, @self))^.AddTo(location);

		d := new(pDecoration, Init(Environment('bar_counter.png'), Translate(0.5, 0.5), Vec2.Make(0.3, Deduce)));
		location^.AddWall(d, Vec2.Make(0.02, 0), Vec2.Make(0.02, 0), [NotObstacleForBullets]);

		(new(pDecoration, Init(Environment('bottles.png'), Translate(0.36, 0.75), Vec2.Make(0.58, Deduce))))^.AddTo(location);

		obrub := new(pActor, Init(Vec2.Make(0.14, 0.28), Character('obrubens', 'model.png'), Vec2.Make(1/9, 1/8)))^.NewRef;
		obrub^.AddState('idle', Vec2.Make(0, 0), 4, 8, 0.0, 'idle', []);
		obrub^.AddState('walk', Vec2.Make(0, 0), 4, 8, 0.6, 'walk', [MovingState]);
		obrub^.AddState('idle-wpn', Vec2.Make(4/9, 0), 4, 8, 0.0, 'idle-wpn', []);
		obrub^.AddState('walk-wpn', Vec2.Make(4/9, 0), 4, 8, 0.6, 'walk-wpn', [MovingState]);
		obrub^.AddState('firing', Vec2.Make(8/9, 0), 1, 8, 0.2, 'idle-wpn', [MovingState]);
		obrub^.SetupAimOrigins([
			Vec2.Make(49/50, 1-44/100), Vec2.Make(1-2/50, 1-27/100), Vec2.Make(23/50,1-11/100), Vec2.Make(2/50, 1-27/100),
			Vec2.Make(1-49/50, 1-44/100), Vec2.Make(1-45/50, 1-58/100), Vec2.Make(25/50, 1-61/100), Vec2.Make(45/50, 1-58/100)]);

		obrub^.idclip := yes;
		obrub^.local := Translate(0.64, 0.6);
		location^.Add(obrub);

		d := new(pDecoration, Init(Environment('table.png'), Translate(-1.0, 0.4), Vec2.Make(0.3, Deduce)));
		location^.AddWall(d, Vec2.Make(0.02, 0.0), Vec2.Make(0.0, 0.17));

		d := new(pDecoration, Init(Environment('table2.png'), Translate(-0.8, -0.5), Vec2.Make(0.3, Deduce)));
		location^.AddWall(d, Vec2.Make(0.02, 0.0), Vec2.Make(0.0, 0.17));

		d := new(pDecoration, Init(Environment('table3.png'), Translate(0.75, -0.15), Vec2.Make(0.3, Deduce)));
		location^.AddWall(d, Vec2.Make(0.02, 0.0), Vec2.Make(0.0, 0.12));

		if not self.world^.spaceshipArrived then
		begin
			valera := CreateKolobok('valera');
			valera^.local := Translate(-0.93, 0.54);
			valera^.angle := -HalfPi;
			location^.Add(valera);

			twinkle := CreateKolobok('twinkle');
			twinkle^.local := Translate(-0.7, 0.42);
			twinkle^.angle := Pi;
			location^.Add(twinkle);

			kazah := CreateKolobok('kazah');
			kazah^.local := Translate(-1.12, 0.42);
			kazah^.angle := 0;
			location^.Add(kazah);

			(new(pTrigger, Init(Translate(-1.17, 0.37), Vec2.Make(0.65, 0.32)))^.WithCallbacks(nil, nil, @Dialogue_1, @self))^.AddTo(location);
		end;

		(new(pTrigger, Init(Translate(0.45, 0.45), Vec2.Make(0.4, 0.4)))^.WithCallbacks(nil, nil, @Dialogue_Obrub_1, @self))^.AddTo(location);

		player^.local.trans := Vec2.Make(door^.local.trans.x + 0.5 * (door^.size.x - player^.size.x), location^.limits.A.y);
		location^.Add(player);

		state := Setup;
		obrubState := ObrubStandingAndWatching;
		obrubMood := {ObrubAngry}ObrubIndifferent;
	end;

	destructor Ep_Bar.Done;
	begin
		Release(obrubTimer);
		Release(obrub);
		Release(kazah);
		Release(twinkle);
		Release(valera);
		Release(door);
		inherited Done;
	end;

	procedure ObrubTimerProcess(timer: pTimer; const dt: float; param: pointer); forward;
	procedure ObrubTimerShot(reason: Timer.DoneReason; param: pointer); forward;

	procedure ObrubMoved(reason: Actor.MoveCallbackReason; ac: pActor; param: pointer);
	var
		e: pEp_Bar absolute param;
	begin
		Assert(ac = ac);
		if reason <> TargetReached then exit;
		case e^.obrubState of
			ObrubStanding, ObrubStandingAndWatching: ;
			ObrubWalking: e^.SetupObrubWandering;
			ObrubFiring: ;
		end;
	end;

	procedure ObrubTimerProcess(timer: pTimer; const dt: float; param: pointer);
	var
		e: pEp_Bar absolute param;
	begin
		Assert((timer = timer) and (dt = dt));
		case e^.obrubState of
			ObrubStandingAndWatching: e^.obrub^.RotateTo(e^.player^.HeartPos);
		end;
	end;

	procedure ObrubTimerShot(reason: Timer.DoneReason; param: pointer);
	var
		e: pEp_Bar absolute param;
		target: Vec2;
	begin
		if reason <> Timeout then exit;
		case e^.obrubState of
			ObrubStanding, ObrubStandingAndWatching:
				begin
					if e^.obrub^.HeartPos.x > 0.64 then
						target := Vec2.Make(GlobalRNG.GetFloat(0.61, 0.63), GlobalRNG.GetFloat(0.64, 0.69))
					else
						target := Vec2.Make(GlobalRNG.GetFloat(0.66, 0.7), GlobalRNG.GetFloat(0.64, 0.69));
					e^.obrubState := ObrubWalking;
					e^.obrub^.StopRotating;
					e^.obrub^.MoveTo(target, e^.WalkingVelocity, @ObrubMoved, e);
				end;
			ObrubWalking: ;
			ObrubFiring: ;
		end;
	end;

	procedure Ep_Bar.HandleUpdate(const dt: float);
	begin
		inherited HandleUpdate(dt);
		case state of
			Setup:
				begin
					SetupObrubWandering;
					state := Idle;
				end;
			MovingOutsideRequested: mgr^.Switch(new(pEp_Entry, Init(world)));
		end;
	end;

	procedure Ep_Bar.HandleDeactivation;
	begin
		mgr^.bgm.Priority(id)^.RemoveModifier('mute', no);
		inherited HandleDeactivation;
	end;

	procedure Ep_Bar.SetupObrubWandering;
	begin
		if abs(AngleDiff(obrub^.angle, ArcTan2(player^.HeartPos - obrub^.HeartPos))) < HalfPi then
			obrubState := ObrubStandingAndWatching
		else
			obrubState := ObrubStanding;
		StopObrubWandering;
		obrubTimer := new(pTimer, Init(GlobalRNG.GetFloat(4.0, 9.0), @ObrubTimerProcess, @ObrubTimerShot, @self))^.NewRef;
		mgr^.AddTimer(obrubTimer, id);
	end;

	procedure Ep_Bar.StopObrubWandering;
	begin
		obrub^.StopMoving;
		if Assigned(obrubTimer) then obrubTimer^.Stop;
		Release(obrubTimer);
	end;

	function CreateKolobok(const char: string): pActor;
	begin
		result := new(pActor, Init(Vec2.Make(0.14, 0.14), Character(char, 'model.png'), Vec2.Make(1/2, 1/8)))^.NewRef;
		result^.AddState('idle', Vec2.Make(0, 0), 2, 8, 0.6, 'idle', []);
		result^.AddState('walk', Vec2.Make(0, 0), 2, 8, 0.6, 'idle', [MovingState]);
	end;

	procedure RotateActors(const actors: array of pActor; const target: Vec2);
	var
		ac: pActor;
	begin
		for ac in actors do
			ac^.RotateTo(target);
	end;

end.

