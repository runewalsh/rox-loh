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
		parent: pNode;
		constructor Init(const local: Transform2; const size: Vec2);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw(const view: Transform2); virtual; abstract;
		function HeartPos: Vec2; virtual;
		procedure Detach;
		function SetLayer(layer: sint): pNode;
		function SetParent(parent: pNode): pNode;
		function AddTo(loc: pLocation): pNode;
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
		function WithCallbacks(onTest: TestProc; onTrigger: TriggerProc; onActivate: ActivateProc; param: pointer): pTrigger;
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
	type
		WallFlag = (NotObstacleForBullets);
		WallFlags = set of WallFlag;

		RaycastResultItem = record
			n: pNode;
			point: Vec2;
			sqrDistance: float;
		end;
		RaycastResult = array of RaycastResultItem;
	var
		state: pState;
		nodes: array of pNode;
		walls: array of record
			rect: Rect;
			flags: WallFlags;
			rot: Rotation2;
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
		function Raycast(const origin, direction: Vec2; out r: RaycastResult; ignore: pNode): boolean;
		procedure AddObstacle(const obj: Circle);

		procedure AddWall(const w: Rect; const flags: WallFlags = []);
		procedure AddWall(const w: Rect; const rot: Rotation2; const flags: WallFlags = []);
		procedure AddWall(n: pNode; const dA, dB: Vec2; const flags: WallFlags = []);

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

	function Node.SetLayer(layer: sint): pNode;
	begin
		self.layer := layer;
		result := @self;
	end;

	function Node.SetParent(parent: pNode): pNode;
	begin
		self.parent := parent;
		result := @self;
	end;

	function Node.AddTo(loc: pLocation): pNode;
	begin
		loc^.Add(@self);
		result := @self;
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

	function Trigger.WithCallbacks(onTest: TestProc; onTrigger: TriggerProc; onActivate: ActivateProc; param: pointer): pTrigger;
	begin
		self.onTest := onTest;
		self.onTrigger := onTrigger;
		self.onActivate := onActivate;
		self.param := param;
		result := @self;
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
			if i < length(nodes) then
				nodes[i]^.HandleUpdate(dt);
		for i := High(triggers) downto 0 do
			if i < length(triggers) then
				triggers[i]^.HandleUpdate(dt);
	end;

	procedure Location.Draw(const view: Transform2);
		function DrawBefore(a, b: pNode): boolean;
		begin
			if Assigned(b^.parent) then
				if b^.parent = a then exit(yes) else b := b^.parent;
			if Assigned(a^.parent) then
				if a^.parent = b then exit(no) else a := a^.parent;
			result := (a^.layer < b^.layer) or (a^.layer = b^.layer) and (a^.local.trans.y > b^.local.trans.y);
		end;

		{$define elem := pNode} {$define procname := SortByDrawOrder}
		{$define less := DrawBefore(_1, _2)}
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
			if walls[i].rot.IsIdentity then
				result := CircleVsRect(obj, walls[i].rect, move) or result
			else
			begin
				nm := -walls[i].rot * move;
				if CircleVsRect(Circle.Make(-walls[i].rot * (obj.center - walls[i].rect.A), obj.radius), Rect.Make(Vec2.Zero, walls[i].rect.Size), nm) then
				begin
					result := yes;
					move := walls[i].rot * nm;
				end;
			end;

		for i := 0 to High(actors) do
			if (actors[i] <> ignore) and not pActor(actors[i])^.idclip then
				result := CircleVsCircle(obj, pActor(actors[i])^.Collision, move) or result;
	end;

	function Location.Raycast(const origin, direction: Vec2; out r: RaycastResult; ignore: pNode): boolean;
		procedure Push(n: pNode; const point: Vec2);
		begin
			SetLength(r, length(r) + 1);
			r[High(r)].n := n;
			r[High(r)].point := point;
			r[High(r)].sqrDistance := SqrDistance(origin, point);
		end;

		{$define procname := SortByDistance} {$define elem := RaycastResultItem} {$define ref := _1.sqrDistance} {$define openarray}
		{$include sort.inc}

	var
		i: sint;
		point: Vec2;
	begin
		r := nil;
		for i := 0 to High(walls) do
			if not (NotObstacleForBullets in walls[i].flags) then
				if walls[i].rot.IsIdentity then
				begin
					if RayVsRect(origin, direction, walls[i].rect, @point) then
						Push(nil, point);
				end else
				begin
					if RayVsRect(-walls[i].rot * (origin - walls[i].rect.A), -walls[i].rot * direction, Rect.Make(Vec2.Zero, walls[i].rect.Size), @point) then
						Push(nil, walls[i].rect.A + walls[i].rot * point);
				end;

		for i := 0 to High(actors) do
			if (actors[i] <> ignore) and RayVsCircle(origin, direction, pActor(actors[i])^.Collision, nil, @point) then
				Push(actors[i], point);

		if (length(r) = 0) and RayVsRect(origin, direction, limits, @point) then
			Push(nil, point);
		SortByDistance(r);
		result := length(r) > 0;
	end;

	procedure Location.AddObstacle(const obj: Circle);
	begin
		SetLength(obstacles, length(obstacles) + 1);
		obstacles[High(obstacles)] := obj;
	end;

	procedure Location.AddWall(const w: Rect; const flags: WallFlags = []);
	begin
		AddWall(w, Rotation2.Identity, flags);
	end;

	procedure Location.AddWall(const w: Rect; const rot: Rotation2; const flags: WallFlags = []);
	begin
		SetLength(walls, length(walls) + 1);
		walls[High(walls)].rect := w;
		walls[High(walls)].flags := flags;
		walls[High(walls)].rot := rot;
	end;

	procedure Location.AddWall(n: pNode; const dA, dB: Vec2; const flags: WallFlags = []);
	begin
		Add(n);
		AddWall(Rect.Make(n^.local.trans + dA, n^.local.trans + n^.size - dA - dB), n^.local.rot, flags);
	end;

	function Location.ActivateTriggerAt(const pos: Vec2; activator: pNode): boolean;
	var
		i: sint;
	begin
		for i := 0 to High(triggers) do
			if (i < length(triggers)) and Assigned(triggers[i]^.onActivate) and
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

	function ShouldHighlightThisOne(id: uint; param, param2: pointer): boolean;
	begin
		result := Rect.MakeSize(pLocation(param)^.triggers[id]^.local.trans, pLocation(param)^.triggers[id]^.size).Contains(pVec2(param2)^);
	end;

	function Location.ShouldHighlightTrigger(const pos: Vec2): boolean;
	begin
		result := Range.Open(length(triggers)).Any(@ShouldHighlightThisOne, @self, @pos);
	end;

	procedure Location.AddOrRemove(n: pNode; add: boolean);
	begin
		if add then
			if Assigned(n^.location) then raise Error('Объект ужа принадлежит другой локации.') else
		else
			Assert(n^.location = @self);

		MakeRef(n);
		try
			if InheritsFrom(TypeOf(n^), TypeOf(Trigger)) then
				AddOrRemove(n, NodesArray(triggers), add)
			else
				AddOrRemove(n, nodes, add);

			if InheritsFrom(TypeOf(n^), TypeOf(Actor)) then
				AddOrRemove(n, NodesArray(actors), add);

			if add then n^.location := @self else n^.location := nil;
		finally
			Release(n);
		end;
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

