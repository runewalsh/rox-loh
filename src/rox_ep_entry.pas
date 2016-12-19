{$include opts.inc}
unit rox_ep_entry;

interface

uses
	USystem, UMath, Utils,
	rox_decoration, rox_actor, rox_ui, rox_gfx, rox_paths, rox_state_adventure, rox_location, rox_timer, rox_world,
	rox_ep_bar;

type
	pEp_Entry = ^Ep_Entry;
	Ep_Entry = object(Adventure)
		hints: boolean;
		door: pDecoration;
		doorTrig: pTrigger;

		hint: pControl;
		hintTimer: pTimer;

		constructor Init(world: pWorld);
		destructor Done; virtual;
		procedure HandleActivation; virtual;

		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra); virtual;
		procedure HandleKeyboard(action: KeyboardAction; key: KeyboardKey); virtual;

	private
		state: (Setup, SetupRe, SetupDepart, Monologue, Idle, MovingToBarRequested);

	const
		EntryStateID = 'ep_entry';
		DepartStateID = 'ep_depart';
	end;

implementation

	procedure OpenHint(e: pEp_Entry; const src: string);
	begin
		if Assigned(e^.hintTimer) then begin e^.hintTimer^.Stop; Release(e^.hintTimer); end;
		if Assigned(e^.hint) then begin e^.hint^.Detach; Release(e^.hint); end;
		e^.hint := new(pControl, Init(Texture.Load(UI(src)), []))^.NewRef;
		e^.hint^.size := 0.5;
		e^.hint^.local := Translate(-e^.mgr^.nvp.x, e^.mgr^.nvp.y - e^.hint^.CalculateRawSize.y);
		e^.mgr^.ui.Add(e^.hint^.NewRef, e^.id);
	end;

	procedure CloseHintTimer(reason: Timer.DoneReason; param: pointer);
	var
		e: pEp_Entry absolute param;
	begin
		if reason in [Timeout, Stopped] then
		begin
			Release(e^.hintTimer);
			e^.hint^.Detach;
			Release(e^.hint);
		end;
	end;

	function DoorTest(n: pNode; const pos: Vec2; t: pTrigger; param: pointer): boolean;
	var
		e: pEp_Entry absolute param;
	begin
		Assert(@param = @param);
		result := (n = pNode(e^.player)) and (t^.local.trans.y > n^.local.trans.y) and
			(abs(Angle(t^.HeartPos - pos, Rotate(Vec2.PositiveX, pActor(n)^.angle))) < Pi/6);
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
					e^.door^.texRect.A := Vec2.Make(0.5, 0);
					if e^.hints then
					begin
						OpenHint(e, 'hint-act.png');
						e^.hintTimer := new(pTimer, Init(20, @ProcessDoorHint, @CloseHintTimer, e))^.NewRef;
						e^.mgr^.AddTimer(e^.hintTimer, e^.id);
					end;
				end;
			Leaved: e^.door^.texRect.A := Vec2.Make(0, 0);
		end;
	end;

	procedure DoorActivate(t: pTrigger; param: pointer);
	var
		e: pEp_Entry absolute param;
	begin
		Assert(t = t);
		if e^.doorTrig^.HasInside(e^.player) then e^.state := MovingToBarRequested;
	end;

	constructor Ep_Entry.Init(world: pWorld);
	var
		stateId: string;
		d: pDecoration;
	begin
		stateId := EntryStateID;
		if Assigned(world) then
			if world^.spaceshipArrived then
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

		location := new(pLocation, Init(@self))^.NewRef;
		door := new(pDecoration, Init(Environment('bar_door.png'), Translate(1, 0), Vec2.Make(0.3, 0.3*1.3)))^.NewRef;
		door^.texRect := Rect.Make(0, 0, 0.5, 1);
		location^.AddWall(door, Vec2.Zero, Vec2.Make(0, 0.2/1*1.3));

		doorTrig := new(pTrigger, Init(door^.local, door^.size))^.NewRef;
		doorTrig^.onTest := @DoorTest;
		doorTrig^.onTrigger := @DoorTrigger;
		doorTrig^.onActivate := @DoorActivate;
		doorTrig^.param := @self;
		doorTrig^.highlight := yes;
		self.location^.Add(doorTrig);

		d := new(pDecoration, Init(Environment('brick.png'), Translate(0, 0.02), Vec2.Make(1.5, 0.3)));
		d^.texRect := Rect.Make(Vec2.Zero, Vec2.Make(5, 1));
		self.location^.AddWall(d, Vec2.Zero, Vec2.Make(0, 0.2/1*1.3));

		if state <> Setup then player^.local := door^.local * Translate(0.5 * (door^.size.x - player^.size.x), -0.15);
		location^.Add(player);
	end;

	destructor Ep_Entry.Done;
	begin
		Release(hintTimer);
		Release(hint);
		Release(doorTrig);
		Release(door);
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

		OpenHint(e, 'hint-move.png');
		e^.hintTimer := new(pTimer, Init(8.0, @ProcessMovementHint, @CloseHintTimer, e))^.NewRef;
		e^.mgr^.AddTimer(e^.hintTimer, e^.id);
	end;

	procedure PlayerWalkedIn(reason: Actor.MoveCallbackReason; ac: pActor; param: pointer);
	var
		e: pEp_Entry absolute param;
	begin
		Assert(ac = ac);
		Assert(reason = TargetReached);
		e^.state := Idle;

		e^.state := Monologue;
		e^.dlg.Init(e, 'rox [face = indifferent.png, sizeX = 0.5]: 0.png >>' +
			'rox [face = saliva.png, sizeX = 0.6]: 1.png >>' +
			'rox [face = scared.png, sizeX = 0, delay = 0.5]: empty.png >>' +
			'rox [face = scared-refl.png, sizeX = 0, delay = 0.5]: empty.png >>' +
			'rox [face = scared.png, sizeX = 0.15, delay = 1]: 2.png >>' +
			'rox [sizeX = 0.4]: 3.png');
		e^.dlg.onDone := @MonologueFinished;
		e^.dlg.param := e;
	end;

	procedure Ep_Entry.HandleUpdate(const dt: float);
	begin
		case state of
			Setup, SetupRe, SetupDepart:
				begin
					location^.limits := Rect.Make(-mgr^.nvp, mgr^.nvp);
					if state = Setup then
					begin
						cameraMode := LookPredefined;
						camera.target := Vec2.Make(-0.1, 0.2);
						state := Monologue;

						playerControlMode := PlayerControlDisabled;
						player^.idclip := yes;
						player^.local.trans := Vec2.Make(camera.target.x - mgr^.nvp.x - player^.size.x, -0.3);
						player^.MoveTo(Vec2.Make(-0.4, -0.3), WalkingVelocity, @PlayerWalkedIn, @self);
					end else
						state := Idle;
				end;
		end;

		inherited HandleUpdate(dt);

		case state of
			MovingToBarRequested: mgr^.Switch(new(pEp_Bar, Init(world)));
		end;
	end;

	procedure Ep_Entry.HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra);
	begin
		inherited HandleMouse(action, pos, extra);
	end;

	procedure Ep_Entry.HandleKeyboard(action: KeyboardAction; key: KeyboardKey);
	begin
		inherited HandleKeyboard(action, key);
	end;

end.

