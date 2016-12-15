{$include opts.inc}
unit rox_trigger;

interface

uses
	USystem, UMath, Utils, rox_gfx, rox_location;

type
	pSpatialTrigger = ^SpatialTrigger;
	SpatialTrigger = object(Trigger)
	type
		Reason = (Entered, Leaved);
		TestProc = function(n: pNode; t: pSpatialTrigger; param: pointer): boolean;
		TriggerProc = procedure(n: pNode; reason: Reason; param: pointer);

		pInnerDesc = ^InnerDesc;
		InnerDesc = record
			n: pNode;
			confirmed: boolean;
		end;
	var
		onTest: TestProc;
		onTrigger: TriggerProc;
		param: pointer;
		inside: array of InnerDesc;
		highlight: boolean;

		constructor Init(const local: Transform2; const size: Vec2);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure Dismiss;

	private
		procedure HandleEnter(n: pNode); virtual;
		procedure HandleLeave(n: pNode); virtual;
	end;

implementation

	constructor SpatialTrigger.Init(const local: Transform2; const size: Vec2);
	begin
		inherited Init(local, size);
	end;

	destructor SpatialTrigger.Done;
	begin
		Dismiss;
		inherited Done;
	end;

	procedure SpatialTrigger.HandleUpdate(const dt: float);
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
			if rect.Intersects(UMath.Rect.MakeSize(location^.nodes[i]^.local.trans, location^.nodes[i]^.size)) and
				(not Assigned(onTest) or onTest(location^.nodes[i], @self, param)) then
			begin
				inner := Index(location^.nodes[i], pointer(pInnerDesc(inside)) + fieldoffset InnerDesc _ n _, length(inside), sizeof(InnerDesc));
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

	procedure SpatialTrigger.Dismiss;
	var
		i: sint;
	begin
		for i := 0 to High(inside) do Release(inside[i].n);
		inside := nil;
	end;

	procedure SpatialTrigger.HandleEnter(n: pNode);
	begin
		if Assigned(onTrigger) then onTrigger(n, Entered, param);
	end;

	procedure SpatialTrigger.HandleLeave(n: pNode);
	begin
		if Assigned(onTrigger) then onTrigger(n, Leaved, param);
	end;

end.

