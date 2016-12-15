{$include opts.inc}
unit rox_ep1_entry;

interface

uses
	USystem, UMath, Utils, rox_decoration, rox_actor, rox_ui, rox_gfx, rox_paths, rox_state_adventure, rox_location, rox_trigger, rox_timer;

type
	pEp1_Entry = ^Ep1_Entry;
	Ep1_Entry = object(Adventure)
		nearDoor: boolean;
		door: pDecoration;

		hint: pControl;
		hintState: (WaitingForMovementHint, MovementHint, NoHint);
		hintTimer: pTimer;
		constructor Init;
		destructor Done; virtual;

		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra); virtual;

	private
		state: (Setup, WatchLimits, Idle);

	const
		StateID = 'ep1_entry';
	end;

implementation

	function DoorTest(n: pNode; t: pSpatialTrigger; param: pointer): boolean;
	var
		e: pEp1_Entry absolute param;
	begin
		Assert(@param = @param);
		result := (n = pNode(e^.player)) and (t^.local.trans.y > n^.local.trans.y) and
			(abs(Angle(t^.HeartPos - n^.HeartPos, Rotate(Vec2.PositiveX, pActor(n)^.angle))) < Pi/6);
	end;

	procedure DoorTrigger(n: pNode; reason: SpatialTrigger.Reason; param: pointer);
	var
		e: pEp1_Entry absolute param;
	begin
		case reason of
			Entered:
				begin
					e^.nearDoor := yes;
					e^.door^.texRect.A := Vec2.Make(0.5, 0);
				end;
			Leaved:
				begin
					e^.nearDoor := no;
					e^.door^.texRect.A := Vec2.Make(0, 0);
				end;
		end;
	end;

	constructor Ep1_Entry.Init;
	var
		d: pDecoration;
		st: pSpatialTrigger;
	begin
		inherited Init(StateID);
		player := new(pActor, Init(Vec2.Make(0.14, 0.28), Character('player', 'model.png'), Vec2.Make(1/4, 1/8)))^.NewRef;
		player^.AddState('idle', Vec2.Make(0, 0), 4, 8, 0.0, 'idle', []);
		player^.AddState('walk', Vec2.Make(0, 0), 4, 8, 0.6, 'walk', [MovingState]);

		location := new(pLocation, Init(@self))^.NewRef;
		door := new(pDecoration, Init(Environment('bar_door.png'), Translate2(1, 0), Vec2.Make(0.3, 0.3/1*1.3)))^.NewRef;
		door^.texRect := Rect.Make(0, 0, 0.5, 1);
		location^.AddWall(door, Vec2.Zero, Vec2.Make(0, 0.2/1*1.3));

		st := new(pSpatialTrigger, Init(door^.local, door^.size));
		st^.onTest := @DoorTest;
		st^.onTrigger := @DoorTrigger;
		st^.param := @self;
		st^.highlight := yes;
		self.location^.Add(st);

		d := new(pDecoration, Init(Environment('brick.png'), Translate2(0, 0.02), Vec2.Make(1.5, 0.3)));
		d^.texRect := Rect.Make(Vec2.Zero, Vec2.Make(5, 1));
		self.location^.AddWall(d, Vec2.Zero, Vec2.Make(0, 0.2/1*1.3));

		dlg.Init(@self, 'player [indifferent.png]: 0.png');
		state := Setup;
	end;

	destructor Ep1_Entry.Done;
	begin
		Release(hintTimer);
		Release(hint);
		Release(door);
		inherited Done;
	end;

	procedure CloseHint(reason: Timer.DoneReason; param: pointer);
	var
		e: pEp1_Entry absolute param;
	begin
		if reason = Timeout then
		begin
			e^.hint^.Detach;
			Release(e^.hint);
			e^.hintState := NoHint;
			Release(e^.hintTimer);
		end;
	end;

	procedure ResetLimitsWhenPlayerWalksIn(reason: Actor.MoveCallbackReason; ac: pActor; param: pointer);
	var
		e: pEp1_Entry absolute param;
	begin
		Assert(ac = ac);
		e^.location^.limits.A := -pAdventure(param)^.mgr^.nvp;
		e^.state := Idle;
		writeln(reason,' ',e^.hintstate);

		case e^.hintState of
			WaitingForMovementHint:
				if reason = TargetReached then
				begin
					e^.hint := new(pControl, Init(Texture.Load(rox_paths.UI('hint-0.png')), []))^.NewRef;
					e^.hint^.size := 0.5;
					e^.hint^.local := Translate2(-e^.mgr^.nvp.x, e^.mgr^.nvp.y - e^.hint^.CalculateRawSize.y);
					e^.mgr^.ui.Add(e^.hint^.NewRef, e^.id);
					e^.hintState := MovementHint;
					e^.hintTimer := new(pTimer, Init(4.0, nil, @CloseHint, e))^.NewRef;
					e^.mgr^.AddTimer(e^.hintTimer, e^.id);
				end else
					e^.hintState := NoHint;
			MovementHint: e^.hintTimer^.left *= 0.3;
		end;
	end;

	procedure Ep1_Entry.HandleUpdate(const dt: float);
	begin
		inherited HandleUpdate(dt);
		case state of
			Setup:
				begin
					location^.limits := Rect.Make(-mgr^.nvp - Vec2.Make(0.4, 0), mgr^.nvp);
					player^.local.trans := Vec2.Make(location^.limits.A.x, -0.3);
					player^.SwitchToState('walk');
					player^.MoveTo(Vec2.Make(-0.2, -0.3), WalkingVelocity, @ResetLimitsWhenPlayerWalksIn, @self);
					location^.Add(player);
					state := WatchLimits;
				end;
		end;

		if dlg.Valid and dlg.Finished then begin dlg.Done; dlg.Init(@self, 'player [indifferent.png]: 0.png'); dlg.Update(dt); end;
	end;

	procedure Ep1_Entry.HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra);
	begin
		inherited HandleMouse(action, pos, extra);
	end;

end.

