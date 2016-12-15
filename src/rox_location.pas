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
		constructor Init(const local: Transform2; const size: Vec2);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw(const view: Transform2); virtual; abstract;
		function HeartPos: Vec2; virtual;
	end;

	pNodesArray = ^NodesArray;
	NodesArray = array of pNode;

	pTrigger = ^Trigger;
	Trigger = object(&Node)
		constructor Init(const local: Transform2; const size: Vec2);
		destructor Done; virtual;
		procedure HandleDraw(const view: Transform2); virtual;
	end;

	Location = object(&Object)
		state: pState;
		nodes: array of pNode;
		walls: array of Rect;
		obstacles: array of Circle;
		triggers: array of pTrigger;
		limits: Rect;
		constructor Init(state: pState);
		destructor Done; virtual;
		procedure Update(const dt: float);
		procedure Draw(const view: Transform2);

		procedure Add(n: pNode);
		procedure Remove(n: pNode);
		function Collide(const obj: Circle; var move: Vec2): boolean;
		procedure AddObstacle(const obj: Circle);

		procedure AddWall(const w: Rect);
		procedure AddWall(n: pNode; const dA, dB: Vec2);
	private
		function SuitableArray(n: pNode): pNodesArray;
		procedure Add(n: pNode; var ary: NodesArray);
		procedure Remove(n: pNode; var ary: NodesArray);
		function IndexNode(n: pNode; const ary: array of pNode): sint;
	end;

implementation

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

	constructor Trigger.Init(const local: Transform2; const size: Vec2);
	begin
		inherited Init(local, size);
	end;

	destructor Trigger.Done;
	begin
		inherited Done;
	end;

	procedure Trigger.HandleDraw(const view: Transform2);
	begin
		Assert(@view = @view);
		raise Error('Триггер не должен получать запросы на отрисовку.');
	end;

	constructor Location.Init(state: pState);
	begin
		inherited Init;
		self.state := state;
		limits := Rect.Make(0, 0, 10, 10);
	end;

	destructor Location.Done;
	var
		i: sint;
	begin
		for i := 0 to High(nodes) do
			Release(nodes[i]);
		for i := 0 to High(triggers) do
			Release(triggers[i]);
		inherited Done;
	end;

	procedure Location.Update(const dt: float);
	var
		i: sint;
	begin
		for i := 0 to High(nodes) do
			nodes[i]^.HandleUpdate(dt);
		for i := 0 to High(triggers) do
			triggers[i]^.HandleUpdate(dt);
	end;

	procedure Location.Draw(const view: Transform2);
		{$define elem := pNode} {$define procname := SortByDrawOrder} {$define less := _1^.local.trans.y > _2^.local.trans.y}
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
		Add(n, SuitableArray(n)^);
	end;

	procedure Location.Remove(n: pNode);
	begin
		Remove(n, SuitableArray(n)^);
	end;

	function Location.Collide(const obj: Circle; var move: Vec2): boolean;
	var
		i: sint;
	begin
		result := no;
		result := CircleVsEnclosingRect(obj, limits, move) or result;

		for i := 0 to High(obstacles) do
			result := CircleVsCircle(obj, obstacles[i], move) or result;

		for i := 0 to High(walls) do
			result := CircleVsRect(obj, walls[i], move) or result;
	end;

	procedure Location.AddObstacle(const obj: Circle);
	begin
		SetLength(obstacles, length(obstacles) + 1);
		obstacles[High(obstacles)] := obj;
	end;

	procedure Location.AddWall(const w: Rect);
	begin
		SetLength(walls, length(walls) + 1);
		walls[High(walls)] := w;
	end;

	procedure Location.AddWall(n: pNode; const dA, dB: Vec2);
	begin
		Add(n);
		AddWall(Rect.Make(n^.local.trans + dA, n^.local.trans + n^.size - dB));
	end;

	function Location.SuitableArray(n: pNode): pNodesArray;
	begin
		if InheritsFrom(TypeOf(n^), TypeOf(Trigger)) then result := @NodesArray(triggers) else result := @nodes;
	end;

	procedure Location.Add(n: pNode; var ary: NodesArray);
	begin
		if Assigned(n^.location) then raise Error('Объект ужа принадлежит другой локации.');
		if IndexNode(n, ary) >= 0 then raise Error('Объект уже добавлен в локацию.');
		SetLength(ary, length(ary) + 1);
		ary[High(ary)] := n^.NewRef;
		n^.location := @self;
	end;

	procedure Location.Remove(n: pNode; var ary: NodesArray);
	var
		id: sint;
	begin
		id := IndexNode(n, ary);
		if id < 0 then raise Error('Объекта нет в локации.');
		Assert(ary[id]^.location = @self);
		ary[id]^.location := nil;
		Release(ary[id]);
		ary[id] := ary[High(ary)];
		SetLength(ary, length(ary) - 1);
	end;

	function Location.IndexNode(n: pNode; const ary: array of pNode): sint;
	type
		ppNode = ^pNode;
	begin
		result := Index(n, ppNode(ary), length(ary));
	end;

end.

