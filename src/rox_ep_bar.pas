{$include opts.inc}
unit rox_ep_bar;

interface

uses
	USystem, UMath, Utils,
	rox_state_adventure, rox_actor, rox_location, rox_decoration, rox_paths, rox_world, rox_dialogue, rox_timer;

type
	pEp_Bar = ^Ep_Bar;
	Ep_Bar = object(Adventure)
		door: pDecoration;
		doorTrig: pTrigger;
		dlgTrig: pTrigger;
		valera, twinkle, kazah: pActor;
		constructor Init(world: pWorld);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
	private
		state: (Idle, MovingOutsideRequested);
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
			(abs(Angle(t^.local.trans + Vec2.Make(0.5 * t^.size.x, 0) - pos, Rotate(Vec2.PositiveX, pActor(n)^.angle))) < Pi/5);
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
		if e^.doorTrig^.HasInside(e^.player) then e^.state := MovingOutsideRequested;
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
		Assert(t = t);
		if activator <> pNode(e^.player) then exit;
		e^.dlgTrig^.Detach; Release(e^.dlgTrig);
		e^.playerControlMode := PlayerControlDisabled;
		e^.player^.idclip := yes;
		e^.player^.MoveTo(Vec2.Make(-0.85, 0.3), Ep_Bar.WalkingVelocity, @Dialogue_2, e);
	end;

	constructor Ep_Bar.Init(world: pWorld);
	var
		d: pDecoration;
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

		doorTrig := new(pTrigger, Init(door^.local, door^.size))^.NewRef;
		doorTrig^.onTest := @DoorTest;
		doorTrig^.onTrigger := @DoorTrigger;
		doorTrig^.onActivate := @DoorActivate;
		doorTrig^.param := @self;
		self.location^.Add(doorTrig);

		d := new(pDecoration, Init(Environment('bar_counter.png'), Translate(0.5, 0.5), Vec2.Make(0.3, 0.3*0.75)));
		location^.AddWall(d, Vec2.Make(0.02, 0), Vec2.Make(0.02, 0), [NotObstacleForBullets]);

		d := new(pDecoration, Init(Environment('bottles.png'), Translate(0.36, 0.75), Vec2.Make(0.58, 0.58*(1/3))));
		location^.Add(d);

		d := new(pDecoration, Init(Environment('table.png'), Translate(-1.0, 0.4), Vec2.Make(0.3, 0.3*(67/92))));
		location^.AddWall(d, Vec2.Make(0.02, 0.0), Vec2.Make(0.0, 0.17));

		d := new(pDecoration, Init(Environment('table2.png'), Translate(-0.8, -0.5), Vec2.Make(0.3, 0.3*(67/89))));
		location^.AddWall(d, Vec2.Make(0.02, 0.0), Vec2.Make(0.0, 0.17));

		d := new(pDecoration, Init(Environment('table3.png'), Translate(0.75, -0.15), Vec2.Make(0.3, 0.3*(57/92))));
		location^.AddWall(d, Vec2.Make(0.02, 0.0), Vec2.Make(0.0, 0.12));

		if not self.world^.spaceshipArrived then
		begin
			valera := CreateKolobok('valera');
			valera^.local := Translate(-0.93, 0.55);
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

			dlgTrig := new(pTrigger, Init(Translate(-1.17, 0.37), Vec2.Make(0.65, 0.32)))^.NewRef;
			dlgTrig^.onActivate := @Dialogue_1;
			dlgTrig^.param := @self;
			location^.Add(dlgTrig);
		end;

		player^.local.trans := Vec2.Make(door^.local.trans.x + 0.5 * (door^.size.x - player^.size.x), location^.limits.A.y);
		location^.Add(player);
	end;

	destructor Ep_Bar.Done;
	begin
		Release(dlgTrig);
		Release(kazah);
		Release(twinkle);
		Release(valera);
		Release(doorTrig);
		Release(door);
		inherited Done;
	end;

	procedure Ep_Bar.HandleUpdate(const dt: float);
	begin
		inherited HandleUpdate(dt);
		{valera^.angle := NormalizeAngle(valera^.angle + 0.8*dt);
		twinkle^.angle := NormalizeAngle(twinkle^.angle + 0.85*dt);
		kazah^.angle := NormalizeAngle(twinkle^.angle + 0.9*dt);}
		case state of
			MovingOutsideRequested: mgr^.Switch(new(pEp_Entry, Init(world)));
		end;
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

