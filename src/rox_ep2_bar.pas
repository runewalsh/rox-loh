{$include opts.inc}
unit rox_ep2_bar;

interface

uses
	USystem, UMath,
	rox_state_adventure, rox_actor, rox_location, rox_decoration, rox_paths, rox_world;

type
	pEp2_Bar = ^Ep2_Bar;
	Ep2_Bar = object(Adventure)
		door: pDecoration;
		doorTrig: pTrigger;
		constructor Init(world: pWorld);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
	private
		state: (Idle, MovingOutsideRequested);
	const
		StateID = 'ep2_bar';
	end;

implementation

uses
	rox_ep1_entry;

	function DoorTest(n: pNode; const pos: Vec2; t: pTrigger; param: pointer): boolean;
	var
		e: pEp2_Bar absolute param;
	begin
		Assert(@param = @param);
		result := (n = pNode(e^.player)) and (n^.local.trans.y > t^.local.trans.y) and
			(abs(Angle(t^.HeartPos - pos, Rotate(Vec2.PositiveX, pActor(n)^.angle))) < Pi/6);
	end;

	procedure DoorTrigger(n: pNode; reason: Trigger.Reason; param: pointer);
	var
		e: pEp2_Bar absolute param;
	begin
		Assert(n = n);
		case reason of
			Entered: e^.door^.texRect.A := Vec2.Make(0.5, 0);
			Leaved: e^.door^.texRect.A := Vec2.Make(0, 0);
		end;
	end;

	procedure DoorActivate(t: pTrigger; param: pointer);
	var
		e: pEp2_Bar absolute param;
	begin
		Assert(t = t);
		if e^.doorTrig^.HasInside(e^.player) then e^.state := MovingOutsideRequested;
	end;

	constructor Ep2_Bar.Init(world: pWorld);
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
		doorTrig^.highlight := yes;
		self.location^.Add(doorTrig);

		d := new(pDecoration, Init(Environment('bar_counter.png'), Translate(0.5, 0.5), Vec2.Make(0.3, 0.3*0.75)));
		location^.AddWall(d, Vec2.Make(0.02, 0), Vec2.Make(0.02, 0));

		d := new(pDecoration, Init(Environment('bottles.png'), Translate(0.36, 0.75), Vec2.Make(0.58, 0.58*(1/3))));
		location^.Add(d);

		d := new(pDecoration, Init(Environment('table.png'), Translate(-1.0, 0.4), Vec2.Make(0.35, 0.35*(67/92))));
		location^.AddWall(d, Vec2.Make(0.02, 0.0), Vec2.Make(0.02, 0.2));

		d := new(pDecoration, Init(Environment('table2.png'), Translate(-0.6, -0.4), Vec2.Make(0.37, 0.35*(67/89))));
		location^.AddWall(d, Vec2.Make(0.02, 0.0), Vec2.Make(0.02, 0.2));

		player^.local.trans := Vec2.Make(door^.local.trans.x + 0.5 * (door^.size.x - player^.size.x), location^.limits.A.y);
		location^.Add(player);
	end;

	destructor Ep2_Bar.Done;
	begin
		Release(doorTrig);
		Release(door);
		inherited Done;
	end;

	procedure Ep2_Bar.HandleUpdate(const dt: float);
	begin
		inherited HandleUpdate(dt);
		case state of
			MovingOutsideRequested: mgr^.Switch(new(pEp1_Entry, Init(world)));
		end;
	end;

end.

