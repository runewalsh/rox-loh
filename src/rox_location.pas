{$include opts.inc}
unit rox_location;

interface

uses
	USystem, UMath, Utils, rox_gl, rox_collision;

type
	pLocation = ^Location;

	pNode = ^Node;
	Node = object(&Object)
		location: pLocation;
		local: Transform2;
		size: Vec2;
		constructor Init(const local: Transform2; const size: Vec2);
		destructor Done; virtual;
		procedure Update(const dt: float); virtual; abstract;
		procedure Draw(const view: Transform2); virtual; abstract;
		function HeartPos: Vec2; virtual;
	end;

	Location = object(&Object)
		nodes: array of pNode;
		walls: array of Rect;
		obstacles: array of Circle;
		limits: Rect;
		constructor Init;
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
		function IndexNode(n: pNode): sint;
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

	function Node.HeartPos: Vec2;
	begin
		result := local.trans;
	end;

	constructor Location.Init;
	begin
		inherited Init;
		limits := Rect.Make(0, 0, 10, 10);
	end;

	destructor Location.Done;
	var
		i: sint;
	begin
		for i := 0 to High(nodes) do
			Release(nodes[i]);
		inherited Done;
	end;

	procedure Location.Update(const dt: float);
	var
		i: sint;
	begin
		for i := 0 to High(nodes) do
			nodes[i]^.Update(dt);
	end;

	procedure Location.Draw(const view: Transform2);
		{$define elem := pNode} {$define procname := SortByDrawOrder} {$define less := _1^.HeartPos.y > _2^.HeartPos.y}
		{$define openarray} {$include sort.inc}
	var
		sorted: array of pNode;
		i: sint;
	begin
		gl.Disable(gl.DEPTH_TEST);
		SetLength(sorted, length(nodes));
		for i := 0 to High(sorted) do sorted[i] := nodes[i];
		SortByDrawOrder(sorted);
		for i := 0 to High(sorted) do sorted[i]^.Draw(view);
		gl.Enable(gl.DEPTH_TEST);
	end;

	procedure Location.Add(n: pNode);
	begin
		if Assigned(n^.location) then raise Error('Объект ужа принадлежит другой локации.');
		if IndexNode(n) >= 0 then raise Error('Объект уже добавлен в локацию.');
		SetLength(nodes, length(nodes) + 1);
		nodes[High(nodes)] := n^.NewRef;
		n^.location := @self;
	end;

	procedure Location.Remove(n: pNode);
	var
		id: sint;
	begin
		id := IndexNode(n);
		if id < 0 then raise Error('Объекта нет в локации.');
		Assert(nodes[id]^.location = @self);
		nodes[id]^.location := nil;
		Release(nodes[id]);
		nodes[id] := nodes[High(nodes)];
		SetLength(nodes, length(nodes) - 1);
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

	function Location.IndexNode(n: pNode): sint;
	begin
		result := Index(n, pPointer(nodes), length(nodes));
	end;

end.

