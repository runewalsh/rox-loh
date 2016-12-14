{$include opts.inc}
unit rox_collision;

interface

uses
	USystem, UMath;

	function CircleVsEnclosingRect(const obj: Circle; const enclosing: Rect; var move: Vec2): boolean;
	function CircleVsCircle(const obj, obstacle: Circle; var move: Vec2): boolean;
	function CircleVsRect(const obj: Circle; const obstacle: Rect; var move: Vec2): boolean;

implementation

	function CircleVsEnclosingRect(const obj: Circle; const enclosing: Rect; var move: Vec2): boolean;
	var
		dim: uint;
	begin
		result := no;
		for dim := 0 to 1 do
			if (obj.center.data[dim] - obj.radius < enclosing.A.data[dim]) and (move.data[dim] < 0) or
				(obj.center.data[dim] + obj.radius > enclosing.B.data[dim]) and (move.data[dim] > 0) then
			begin
				result := yes;
				move.data[dim] := 0;
			end;
	end;

	function CircleVsCircle(const obj, obstacle: Circle; var move: Vec2): boolean;
	var
		a, b: Vec2;
	begin
		result := (SqrDistance(obj.center, obstacle.center) < sqr(obj.radius + obstacle.radius)) and (move ** (obstacle.center - obj.center) > 0);
		if result then
		begin
			a := (obstacle.center - obj.center).YX.Normalized;
			a.y := -a.y;
			b := -a;
			if move ** a > move ** b then move := a * (move ** a) else move := b * (move ** b);
		end else
			result := no;
	end;

	function CircleVsRect(const obj: Circle; const obstacle: Rect; var move: Vec2): boolean;
	var
		moved: Vec2;
	begin
		result := no;
		moved := obj.center + move;
		if (moved.x + obj.radius < obstacle.A.x) or (moved.x - obj.radius > obstacle.B.x) or
			(moved.y + obj.radius < obstacle.A.y) or (moved.y - obj.radius > obstacle.B.y)
		then
			exit;

		if (obj.center.x < obstacle.B.x) and (move.x > 0) and (moved.y >= obstacle.A.y) and (moved.y <= obstacle.B.y) and (moved.x + obj.radius > obstacle.A.x) then
		begin
			result := yes;
			move.x := obstacle.A.x - obj.center.x - obj.radius;
		end;

		if (obj.center.x > obstacle.A.x) and (move.x < 0) and (moved.y >= obstacle.A.y) and (moved.y <= obstacle.B.y) and (moved.x - obj.radius < obstacle.B.x) then
		begin
			result := yes;
			move.x := obstacle.B.x - obj.center.x + obj.radius;
		end;

		if (obj.center.y < obstacle.B.y) and (move.y > 0) and (moved.x >= obstacle.A.x) and (moved.x <= obstacle.B.x) and (moved.y + obj.radius > obstacle.A.y) then
		begin
			result := yes;
			move.y := obstacle.A.y - obj.center.y - obj.radius;
		end;

		if (obj.center.y > obstacle.A.y) and (move.y < 0) and (moved.x >= obstacle.A.x) and (moved.x <= obstacle.B.x) and (moved.y - obj.radius < obstacle.B.y) then
		begin
			result := yes;
			move.y := obstacle.B.y - obj.center.y + obj.radius;
		end;

		if (obj.center.x < obstacle.B.x) and (obj.center.y < obstacle.B.y) and ((move.x > 0) or (move.y > 0)) then
			result := CircleVsCircle(obj, Circle.Make(obstacle.A, 0), move) or result;

		if (obj.center.x < obstacle.B.x) and (obj.center.y > obstacle.A.y) and ((move.x > 0) or (move.y < 0)) then
			result := CircleVsCircle(obj, Circle.Make(obstacle.A.x, obstacle.B.y, 0), move) or result;

		if (obj.center.x > obstacle.A.x) and (obj.center.y < obstacle.B.y) and ((move.x < 0) or (move.y > 0)) then
			result := CircleVsCircle(obj, Circle.Make(obstacle.B.x, obstacle.A.y, 0), move) or result;

		if (obj.center.x > obstacle.A.x) and (obj.center.y > obstacle.A.y) and ((move.x < 0) or (move.y < 0)) then
			result := CircleVsCircle(obj, Circle.Make(obstacle.B.x, obstacle.B.y, 0), move) or result;
	end;

end.

