{$include opts.inc}
unit rox_actor;

interface

uses
	USystem, UMath, Utils, rox_gfx, rox_location;

type

	pActor = ^Actor;
	Actor = object(Node)
	type
		StateFlag = (MovingState);
		StateFlags = set of StateFlag;

		pStateDesc = ^StateDesc;
		StateDesc = record
			name: string;
			base: Vec2;
			frames: uint;
			angles: uint;
			len: float;
			next: string;
			flags: StateFlags;
			phase: float;
		end;

		MoveCallbackReason = (MovingCanceled, TargetReached);
		MoveCallback = procedure(reason: MoveCallbackReason; ac: pActor; param: pointer);
		MoveTargeter = (NotMoving, MovingBy, MovingTo);
	var
		tex: pTexture;
		texSize: Vec2;
		states: array of StateDesc;

		state: uint;
		angle: float;

		mvMethod: MoveTargeter;
		mvPointOrDelta: Vec2;
		mvVel, realMovementVel: float;
		mvCb: MoveCallback;
		mvParam: pointer;

		rtMethod: (NotRotating, RotatingToPoint);
		rtPoint: Vec2;
		idclip: boolean;

		constructor Init(const size: Vec2; const tex: string; const texSize: Vec2);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw(const view: Transform2); virtual;
		function Collision: Circle;

		function AddState(const name: string; const base: Vec2; frames, angles: uint; const len: float; const next: string; flags: StateFlags): pStateDesc;
		function TryFindState(const name: string): sint;
		function FindState(const name: string): uint;
		function TrySwitchToState(const name: string): boolean;
		procedure SwitchToState(const name: string);
		procedure SwitchToState(id: uint);

		procedure MoveBy(const delta: Vec2; velocity: float);
		procedure MoveTo(const target: Vec2; velocity: float; cb: MoveCallback; param: pointer);
		function HeartPos: Vec2; virtual;

		procedure RotateTo(const point: Vec2);
	private
		procedure SwitchMove(method: MoveTargeter);
		function MoveByStep(const delta: Vec2; const by: float; moved: pVec2): boolean;
		function RotateStep(const target: float; const by: float): boolean;
	end;

implementation

	constructor Actor.Init(const size: Vec2; const tex: string; const texSize: Vec2);
	begin
		inherited Init(Transform2.Identity, size);
		self.tex := Texture.Load(tex);
		self.texSize := texSize;
		angle := -HalfPi;
	end;

	destructor Actor.Done;
	begin
		Release(tex);
		inherited Done;
	end;

	procedure Actor.HandleUpdate(const dt: float);
	var
		moved: Vec2;
		detached: boolean;
	begin
		if length(states) = 0 then raise Error('Актору не заданы состояния.');
		realMovementVel := 0;

		case mvMethod of
			NotMoving:
				if MovingState in states[state].flags then SwitchToState('idle');
			MovingBy:
				begin
					if RotateStep(ArcTan2(mvPointOrDelta.y, mvPointOrDelta.x), 10.0 * dt) then
						MoveByStep(mvPointOrDelta, mvVel * dt, @moved);
					mvMethod := NotMoving;
					// location^.ActivateTriggerAt(HeartPos, @self);
				end;
			MovingTo:
				begin
					if RotateStep(ArcTan2(mvPointOrDelta - HeartPos), 10.0 * dt) then
						if MoveByStep(mvPointOrDelta - HeartPos, mvVel * dt, nil) then
						begin
							if Assigned(mvCb) then
							begin
								MakeRef(@self);
								mvCb(TargetReached, @self, mvParam);
								detached := not Assigned(location);
								ReleaseWeak(@self);
								if detached then exit;
								mvCb := nil;
							end;
							mvMethod := NotMoving;
						end;
					if mvMethod = NotMoving then location^.ActivateTriggerAt(mvPointOrDelta, @self);
				end;
		end;

		case rtMethod of
			NotRotating: ;
			RotatingToPoint:
				begin
					if RotateStep(ArcTan2(rtPoint - HeartPos), 10.0 * dt) then
						rtMethod := NotRotating;
				end;
		end;

		if states[state].len > 0 then
		begin
			if MovingState in states[state].flags then
				states[state].phase += dt * (mvVel / size.x) * states[state].len * (1/1.4)
			else
				states[state].phase += dt;
			if states[state].phase >= states[state].len then
			begin
				states[state].phase := modf(states[state].phase, states[state].len);
				SwitchToState(states[state].next);
			end;
		end;
	end;

	procedure Actor.HandleDraw(const view: Transform2);
	var
		q: Quad;
		an, anStep, frame: float;
	begin
		if length(states) = 0 then raise Error('Актору не заданы состояния.');
		Assert(AngleNormalized(angle), ToString(angle));

		an := angle; if an < 0 then an += TwoPi;
		anStep := floor(states[state].angles * an * (1/TwoPi) + 0.5); if anStep = states[state].angles then anStep := 0;
		Assert((anStep >= 0) and (anStep < states[state].angles), Format('{0}/{1}', [anStep, states[state].angles]));

		frame := floor(states[state].frames * states[state].phase / max(0.1, states[state].len) + 0.5); if frame = states[state].frames then frame := 0;
		Assert((frame >= 0) and (frame < states[state].frames), Format('{0}/{1}', [frame, states[state].frames]));

		q.fields := [q.Field.Transform];
		q.transform := view * self.local;
		q.Draw(tex, Vec2.Zero, size, states[state].base + Vec2.Make(frame * texSize.x, anStep * texSize.y), texSize);
	end;

	function Actor.Collision: Circle;
	begin
		result := Circle.Make(HeartPos, 0.4 * size.x);
	end;

	function Actor.AddState(const name: string; const base: Vec2; frames, angles: uint; const len: float; const next: string; flags: StateFlags): pStateDesc;
	begin
		SetLength(states, length(states) + 1);
		result := @states[High(states)];
		result^.name := name;
		result^.base := base;
		result^.frames := frames;
		result^.angles := angles;
		result^.len := len;
		result^.next := next;
		result^.flags := flags;
		result^.phase := 0;
		if length(states) = 1 then state := 0;
	end;

	function Actor.TryFindState(const name: string): sint;
	var
		i: sint;
	begin
		for i := 0 to High(states) do
			if states[i].name = name then
				exit(i);
		result := -1;
	end;

	function Actor.FindState(const name: string): uint;
	var
		id: sint;
	begin
		id := TryFindState(name);
		if id < 0 then raise Error('Состояние ''' + name + ''' не найдено.');
		result := id;
	end;

	procedure Actor.SwitchToState(const name: string);
	begin
		SwitchToState(FindState(name));
	end;

	function Actor.TrySwitchToState(const name: string): boolean;
	var
		id: sint;
	begin
		id := TryFindState(name);
		result := id >= 0;
		if result then SwitchToState(id);
	end;

	procedure Actor.SwitchToState(id: uint);
	begin
		Assert(id < uint(length(states)));
		if state <> id then
		begin
			state := id;
			if not (MovingState in states[state].flags) then
				states[state].phase := 0;
		end;
	end;

	procedure Actor.MoveBy(const delta: Vec2; velocity: float);
	begin
		SwitchMove(MovingBy);
		mvPointOrDelta := delta;
		mvVel := velocity;
	end;

	procedure Actor.MoveTo(const target: Vec2; velocity: float; cb: MoveCallback; param: pointer);
	begin
		SwitchMove(MovingTo);
		mvPointOrDelta := target;
		mvVel := velocity;
		mvCb := cb;
		mvParam := param;
	end;

	function Actor.HeartPos: Vec2;
	begin
		result := Vec2.Make(local.trans.x + 0.5 * size.x, local.trans.y + 0.2 * size.y);
	end;

	procedure Actor.RotateTo(const point: Vec2);
	begin
		rtMethod := RotatingToPoint;
		rtPoint := point;
	end;

	procedure Actor.SwitchMove(method: MoveTargeter);
	begin
		if (mvMethod = MovingTo) and Assigned(mvCb) then
		begin
			mvCb(MovingCanceled, @self, mvParam);
			mvCb := nil;
		end;
		if not TrySwitchToState('walk') then SwitchToState('idle');
		mvMethod := method;
	end;

	function Actor.MoveByStep(const delta: Vec2; const by: float; moved: pVec2): boolean;
	var
		m: Vec2;
		sql: float;
	begin
		sql := delta.SqrLength;
		result := sqr(by) >= sql;
		if result then m := delta else m := delta * (by / sqrt(sql));
		if not idclip and location^.Collide(Collision, m, @self) and (m.SqrLength < 0.001*by) then mvMethod := NotMoving;
		local.trans += m;
		if Assigned(moved) then moved^ := m;
		realMovementVel := m.Length;
	end;

	function Actor.RotateStep(const target: float; const by: float): boolean;
	var
		nt: float;
	begin
		Assert(AngleNormalized(target), ToString(target));
		if abs(target - angle) <= Pi then nt := target else nt := target - FloatSign(target - angle, TwoPi);
		result := abs(nt - angle) <= by;
		if result then angle := nt else angle += FloatSign(nt - angle, by);
		angle := NormalizeAngle(angle);
	end;

end.

