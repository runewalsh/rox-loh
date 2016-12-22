{$include opts.inc}
unit rox_location;

interface

uses
	USystem, UMath, Utils, rox_gl, rox_collision, rox_state;

type
	pLocation = ^Location;

	pNode = ^Node;
	Node = object(&Object)
		location: pLocation;
		local: Transform2;
		size: Vec2;
		layer: sint;
		constructor Init(const local: Transform2; const size: Vec2);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw(const view: Transform2); virtual; abstract;
		function HeartPos: Vec2; virtual;
		procedure Detach;
	end;

	pNodesArray = ^NodesArray;
	NodesArray = array of pNode;

	pTrigger = ^Trigger;
	Trigger = object(&Node)
	type
		Reason = (Entered, Leaved);
		TestProc = function(n: pNode; const pos: Vec2; t: pTrigger; param: pointer): boolean;
		TriggerProc = procedure(n: pNode; reason: Reason; param: pointer);
		ActivateProc = procedure(t: pTrigger; activator: pNode; param: pointer);

		pInnerDesc = ^InnerDesc;
		InnerDesc = record
			n: pNode;
			confirmed: boolean;
		end;
	var
		onTest: TestProc;
		onTrigger: TriggerProc;
		onActivate: ActivateProc;
		param: pointer;
		inside: array of InnerDesc;

		constructor Init(const local: Transform2; const size: Vec2);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw(const view: Transform2); virtual;
		procedure Dismiss;
		function HasInside(n: pNode): boolean;
	private
		function DoTest(n: pNode; const pos: Vec2): boolean;
		procedure HandleEnter(n: pNode);
		procedure HandleLeave(n: pNode);
		function FindInner(n: pNode): sint;
	end;

	Location = object(&Object)
		state: pState;
		nodes: array of pNode;
		walls: array of record
			rect: Rect;
			angle: float;
		end;
		obstacles: array of Circle;
		triggers: array of pTrigger;
		actors: array of pNode {pActor};
		limits: Rect;
		constructor Init(state: pState);
		destructor Done; virtual;
		procedure Update(const dt: float);
		procedure Draw(const view: Transform2);

		procedure Add(n: pNode);
		procedure Remove(n: pNode);
		function Collide(const obj: Circle; var move: Vec2; ignore: pNode): boolean;
		procedure AddObstacle(const obj: Circle);

		procedure AddWall(const w: Rect; const angle: float = 0);
		procedure AddWall(n: pNode; const dA, dB: Vec2);

		function ActivateTriggerAt(const pos: Vec2; activator: pNode): boolean;
		function ActivateTriggerFor(activator: pNode): boolean;
		function ShouldHighlightTrigger(const pos: Vec2): boolean;
	private
		procedure AddOrRemove(n: pNode; add: boolean);
		procedure AddOrRemove(n: pNode; var ary: NodesArray; add: boolean);
		function IndexNode(n: pNode; const ary: array of pNode): sint;
	end;

implementation

uses
	rox_actor;

	constructor Node.Init(const local: Transform2; const size: Vec2);
	begin
		inherited Init;
		self.local := local;
		self.size := size;
	end;

	destructor Node.Done;
	begin
		inherited Done;
	end;

	procedure Node.HandleUpdate(const dt: float);
	begin
		Assert(@dt = @dt);
	end;

	function Node.HeartPos: Vec2;
	begin
		result := local.trans + 0.5 * size;
	end;

	procedure Node.Detach;
	begin
		if not Assigned(location) then raise Error('Объект не в локации.');
		location^.Remove(@self);
	end;

	constructor Trigger.Init(const local: Transform2; const size: Vec2);
	begin
		inherited Init(local, size);
	end;

	destructor Trigger.Done;
	begin
		Dismiss;
		inherited Done;
	end;

	procedure Trigger.HandleDraw(const view: Transform2);
	begin
		Assert(@view = @view);
		raise Error('Триггер не должен получать запросы на отрисовку.');
	end;

	procedure Trigger.HandleUpdate(const dt: float);
	var
		i, inner: sint;
		rect: UMath.Rect;
	begin
		inherited HandleUpdate(dt);
		for i := 0 to High(inside) do
		begin
			Assert(Assigned(inside[i].n^.location));
			inside[i].confirmed := no;
		end;

		rect := UMath.Rect.Make(local.trans, local.trans + size);
		for i := 0 to High(location^.nodes) do
			if InheritsFrom(TypeOf(location^.nodes[i]^), TypeOf(Actor)) and
				rect.Intersects(UMath.Rect.MakeSize(location^.nodes[i]^.local.trans, location^.nodes[i]^.size)) and
				DoTest(location^.nodes[i], location^.nodes[i]^.HeartPos) then
			begin
				inner := FindInner(location^.nodes[i]);
				if inner < 0 then
				begin
					inner := length(inside);
					SetLength(inside, inner + 1);
					inside[inner].n := location^.nodes[i]^.NewRef;
					HandleEnter(location^.nodes[i]);
				end;
				inside[inner].confirmed := yes;
			end;

		for i := High(inside) downto 0 do
			if not inside[i].confirmed then
			begin
				HandleLeave(inside[i].n);
				Release(inside[i].n);
				inside[i] := inside[High(inside)];
				SetLength(inside, length(inside) - 1);
			end;
	end;

	procedure Trigger.Dismiss;
	var
		i: sint;
	begin
		for i := 0 to High(inside) do Release(inside[i].n);
		inside := nil;
	end;

	function Trigger.HasInside(n: pNode): boolean;
	begin
		result := FindInner(n) >= 0;
	end;

	function Trigger.DoTest(n: pNode; const pos: Vec2): boolean;
	begin
		result := not Assigned(onTest) or onTest(n, pos, @self, param);
	end;

	procedure Trigger.HandleEnter(n: pNode);
	begin
		if Assigned(onTrigger) then onTrigger(n, Entered, param);
	end;

	procedure Trigger.HandleLeave(n: pNode);
	begin
		if Assigned(onTrigger) then onTrigger(n, Leaved, param);
	end;

	function Trigger.FindInner(n: pNode): sint;
	begin
		result := Index(n, pointer(pInnerDesc(inside)) + fieldoffset InnerDesc _ n _, length(inside), sizeof(InnerDesc));
	end;

	constructor Location.Init(state: pState);
	begin
		inherited Init;
		self.state := state;
		limits := Rect.Make(0, 0, 10, 10);
	end;

	destructor Location.Done;
	begin
		ReleaseArray(USystem.ObjectsList(nodes));
		ReleaseArray(USystem.ObjectsList(triggers));
		ReleaseArray(USystem.ObjectsList(actors));
		inherited Done;
	end;

	procedure Location.Update(const dt: float);
	var
		i: sint;
	begin
		for i := High(nodes) downto 0 do
			nodes[i]^.HandleUpdate(dt);
		for i := High(triggers) downto 0 do
			if i < length(triggers) then
				triggers[i]^.HandleUpdate(dt);
	end;

	procedure Location.Draw(const view: Transform2);
		{$define elem := pNode} {$define procname := SortByDrawOrder}
		{$define less := (_1^.layer < _2^.layer) or (_1^.layer = _2^.layer) and (_1^.local.trans.y > _2^.local.trans.y)}
		{$define openarray} {$include sort.inc}
	var
		sorted: array of pNode;
		i: sint;
	begin
		SetLength(sorted, length(nodes));
		for i := 0 to High(sorted) do sorted[i] := nodes[i];
		SortByDrawOrder(sorted);
		for i := 0 to High(sorted) do sorted[i]^.HandleDraw(view);
	end;

	procedure Location.Add(n: pNode);
	begin
		AddOrRemove(n, yes);
	end;

	procedure Location.Remove(n: pNode);
	begin
		AddOrRemove(n, no);
	end;

	function Location.Collide(const obj: Circle; var move: Vec2; ignore: pNode): boolean;
	var
		i: sint;
		nm: Vec2;
	begin
		result := no;
		result := CircleVsEnclosingRect(obj, limits, move) or result;

		for i := 0 to High(obstacles) do
			result := CircleVsCircle(obj, obstacles[i], move) or result;

		for i := 0 to High(walls) do
			if walls[i].angle = 0 then
				result := CircleVsRect(obj, walls[i].rect, move) or result
			else
			begin
				nm := Rotate(move, -walls[i].angle);
				if CircleVsRect(Circle.Make(Rotate(obj.center - walls[i].rect.A, -walls[i].angle), obj.radius), Rect.Make(Vec2.Zero, walls[i].rect.Size), nm) then
				begin
					result := yes;
					move := Rotate(nm, walls[i].angle);
				end;
			end;

		for i := 0 to High(actors) do
			if (actors[i] <> ignore) and not pActor(actors[i])^.idclip then
				result := CircleVsCircle(obj, pActor(actors[i])^.Collision, move) or result;
	end;

	procedure Location.AddObstacle(const obj: Circle);
	begin
		SetLength(obstacles, length(obstacles) + 1);
		obstacles[High(obstacles)] := obj;
	end;

	procedure Location.AddWall(const w: Rect; const angle: float = 0);
	begin
		SetLength(walls, length(walls) + 1);
		walls[High(walls)].rect := w;
		walls[High(walls)].angle := angle;
	end;

	procedure Location.AddWall(n: pNode; const dA, dB: Vec2);
	begin
		Add(n);
		AddWall(Rect.Make(n^.local.trans + dA, n^.local.trans + n^.size - dA - dB));
	end;

	function Location.ActivateTriggerAt(const pos: Vec2; activator: pNode): boolean;
	var
		i: sint;
	begin
		for i := 0 to High(triggers) do
			if Assigned(triggers[i]^.onActivate) and
				Rect.MakeSize(triggers[i]^.local.trans, triggers[i]^.size).Contains(pos) and
				triggers[i]^.HasInside(activator)
			then
			begin
				triggers[i]^.onActivate(triggers[i], activator, triggers[i]^.param);
				exit(yes);
			end;
		result := no;
	end;

	function Location.ActivateTriggerFor(activator: pNode): boolean;
	var
		i: sint;
	begin
		for i := 0 to High(triggers) do
			if Assigned(triggers[i]^.onActivate) and triggers[i]^.HasInside(activator) then
			begin
				triggers[i]^.onActivate(triggers[i], activator, triggers[i]^.param);
				exit(yes);
			end;
		result := no;
	end;

	function Location.ShouldHighlightTrigger(const pos: Vec2): boolean;
	var
		i: sint;
	begin
		for i := 0 to High(triggers) do
			if Rect.MakeSize(triggers[i]^.local.trans, triggers[i]^.size).Contains(pos) then
				exit(yes);
		result := no;
	end;

	procedure Location.AddOrRemove(n: pNode; add: boolean);
	begin
		if add then
			if Assigned(n^.location) then raise Error('Объект ужа принадлежит другой локации.') else
		else
			Assert(n^.location = @self);

		if InheritsFrom(TypeOf(n^), TypeOf(Trigger)) then
			AddOrRemove(n, NodesArray(triggers), add)
		else
			AddOrRemove(n, nodes, add);

		if InheritsFrom(TypeOf(n^), TypeOf(Actor)) then
			AddOrRemove(n, NodesArray(actors), add);

		if add then n^.location := @self else n^.location := nil;
	end;

	procedure Location.AddOrRemove(n: pNode; var ary: NodesArray; add: boolean);
	var
		id: sint;
	begin
		if add then
		begin
			if IndexNode(n, ary) >= 0 then raise Error('Объект уже добавлен в локацию.');
			SetLength(ary, length(ary) + 1);
			ary[High(ary)] := n^.NewRef;
		end else
		begin
			id := IndexNode(n, ary);
			if id < 0 then raise Error('Объекта нет в локации.');
			Release(n); // = Release(ary[id])
			ary[id] := ary[High(ary)];
			SetLength(ary, length(ary) - 1);
		end;
	end;

	function Location.IndexNode(n: pNode; const ary: array of pNode): sint;
	type
		ppNode = ^pNode;
	begin
		result := Index(n, ppNode(ary), length(ary));
	end;

end.

