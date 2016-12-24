{$include opts.inc}
unit rox_ep_bar;

interface

uses
	USystem, UMath, Utils, Random,
	rox_state_adventure, rox_actor, rox_location, rox_decoration, rox_paths, rox_world, rox_dialogue, rox_timer;

type
	pEp_Bar = ^Ep_Bar;
	Ep_Bar = object(Adventure)
		door: pDecoration;
		valera, twinkle, kazah, obrub: pActor;

		obrubState: (ObrubStanding, ObrubStandingAndWatching, ObrubWalking, ObrubFiring);
		obrubMood: (ObrubIndifferent, ObrubAnnoyed, ObrubAngry);
		obrubTimer: pTimer;
		constructor Init(world: pWorld);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
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
			Entered: e^.door^.texRect.A := Vec2.Make(0.5, 0);
			Leaved: e^.door^.texRect.A := Vec2.Make(0, 0);
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

	procedure Dialogue_3(param: pointer);
	// Конец диалога: все убегают, Рокс поворачивается вслед и выдаёт последнюю фразу.
	var
		e: pEp_Bar absolute param;
		t: pTimer;
	begin
		e^.twinkle^.idclip := yes;
		e^.twinkle^.MoveTo(e^.door^.HeartPos, lerp(Ep_Bar.WalkingVelocity, Ep_Bar.RunningVelocity, 0.45), @Dialogue_4_QuitScene, nil);

		e^.kazah^.idclip := yes;
		e^.kazah^.MoveTo(e^.door^.HeartPos, lerp(Ep_Bar.WalkingVelocity, Ep_Bar.RunningVelocity, 0.47), @Dialogue_4_QuitScene, nil);

		e^.valera^.idclip := yes;
		e^.valera^.MoveTo(e^.door^.HeartPos, lerp(Ep_Bar.WalkingVelocity, Ep_Bar.RunningVelocity, 0.5), @Dialogue_4_QuitScene, nil);

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
			{8} 'valera [sizeX = 200/800]: 2.png');
		e^.dlg.onItem := @Dialogue_2_Item;
		e^.dlg.onDone := @Dialogue_3;
		e^.dlg.param := e;
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

	procedure Dialogue_Obrub_1(t: pTrigger; activator: pNode; param: pointer);
	var
		e: pEp_Bar absolute param;
	begin
		if (activator <> pNode(e^.player)) or e^.dlg.Valid then exit;
		e^.StopObrubWandering;
		e^.obrub^.RotateTo(e^.player^.HeartPos);

		case e^.obrubMood of
			ObrubIndifferent:
				begin
					e^.dlg.Init(e, 'obrubens [face = teasing.png, sizeX = 224/800, letterTimeout = 1.1]: 0.png');
					e^.dlg.onDone := @Dialogue_Obrub_RestartWandering;
					e^.dlg.param := e;
					e^.obrubMood := ObrubAnnoyed;
				end;
			ObrubAnnoyed:
				begin
					e^.dlg.Init(e, 'obrubens [face = annoyed.png, sizeX = 58/800, letterTimeout = 0.3]: 1.png');
					if e^.world^.spaceshipArrived then
					begin
						e^.obrubMood := ObrubAngry;
					end else
					begin
						e^.dlg.onDone := @Dialogue_Obrub_RestartWandering;
						e^.dlg.param := e;
					end;
				end;
			ObrubAngry:
				begin

				end;
		end;
	end;

	constructor Ep_Bar.Init(world: pWorld);
	var
		d: pDecoration;
		t: pTrigger;
	begin
		inherited Init(StateID, world);
		if not Assigned(world) then player^.angle := HalfPi;

		location := new(pLocation, Init(@self))^.NewRef;
		location^.limits := Rect.Make(-1.2, -0.7, 1.2, 0.7);
		d := new(pDecoration, Init(Environment('parquet.png'), Translate(-1.2, -0.7), Vec2.Make(2.4, 1.4)));
		d^.texRect := Rect.Make(Vec2.Zero, Vec2.Make(12, 7) * 0.5);
		d^.layer := -2;
		location^.Add(d);

		door := new(pDecoration, Init(Environment('bar_door.png'), Translate(0.3, -0.9), Vec2.Make(0.3, 0.3*1.3)))^.NewRef;
		door^.texRect := Rect.Make(0, 0, 0.5, 1);
		location^.Add(door);

		t := new(pTrigger, Init(door^.local, door^.size))^.WithCallbacks(@DoorTest, @DoorTrigger, @DoorActivate, @self);
		self.location^.Add(t);

		d := new(pDecoration, Init(Environment('bar_counter.png'), Translate(0.5, 0.5), Vec2.Make(0.3, 0.3*0.75)));
		location^.AddWall(d, Vec2.Make(0.02, 0), Vec2.Make(0.02, 0), [NotObstacleForBullets]);

		d := new(pDecoration, Init(Environment('bottles.png'), Translate(0.36, 0.75), Vec2.Make(0.58, 0.58*(1/3))));
		location^.Add(d);

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

		d := new(pDecoration, Init(Environment('table.png'), Translate(-1.0, 0.4), Vec2.Make(0.3, 0.3*(67/92))));
		location^.AddWall(d, Vec2.Make(0.02, 0.0), Vec2.Make(0.0, 0.17));

		d := new(pDecoration, Init(Environment('table2.png'), Translate(-0.8, -0.5), Vec2.Make(0.3, 0.3*(67/89))));
		location^.AddWall(d, Vec2.Make(0.02, 0.0), Vec2.Make(0.0, 0.17));

		d := new(pDecoration, Init(Environment('table3.png'), Translate(0.75, -0.15), Vec2.Make(0.3, 0.3*(57/92))));
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

			t := new(pTrigger, Init(Translate(-1.17, 0.37), Vec2.Make(0.65, 0.32)))^.WithCallbacks(nil, nil, @Dialogue_1, @self);
			location^.Add(t);
		end;

		t := new(pTrigger, Init(Translate(0.45, 0.45), Vec2.Make(0.4, 0.4)))^.WithCallbacks(nil, nil, @Dialogue_Obrub_1, @self);
		location^.Add(t);

		player^.local.trans := Vec2.Make(door^.local.trans.x + 0.5 * (door^.size.x - player^.size.x), location^.limits.A.y);
		location^.Add(player);

		state := Setup;
		obrubState := ObrubStandingAndWatching;
		obrubMood := ObrubIndifferent;
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
					e^.obrub^.rtMethod := NotRotating;
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

	procedure Ep_Bar.SetupObrubWandering;
	begin
		Assert(obrubMood in [ObrubIndifferent, ObrubAnnoyed]);
		if GlobalRNG.XInY(5, 6) then obrubState := ObrubStandingAndWatching else obrubState := ObrubStanding;
		StopObrubWandering;
		obrubTimer := new(pTimer, Init(GlobalRNG.GetFloat(5.0, 10.0), @ObrubTimerProcess, @ObrubTimerShot, @self))^.NewRef;
		mgr^.AddTimer(obrubTimer, id);
	end;

	procedure Ep_Bar.StopObrubWandering;
	begin
		obrub^.StopMove;
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

