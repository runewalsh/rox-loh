{$include opts.inc}
unit rox_actor;

interface

uses
	USystem, UMath, Utils, rox_gfx;

type

	pActor = ^Actor;
	Actor = object(&Object)
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

		MoveCallbackReason = (MovingTargetReached);
		MoveCallback = procedure(reason: MoveCallbackReason; ac: pActor; param: pointer);
	var
		// шаблон
		tex: pTexture;
		size, texSize: Vec2;
		states: array of StateDesc;

		// конкретные параметры
		pos: Vec2;
		state: uint;
		angle: float;

		mvMethod: (NotMoving, MovingBy, MovingTo);
		mvPointOrDelta: Vec2;
		mvVel: float;
		mvCb: MoveCallback;
		mvParam: pointer;

		constructor Init(const size: Vec2; const tex: string; const texSize: Vec2);
		destructor Done; virtual;
		procedure Update(const dt: float);
		procedure Draw(const camera: Vec2);

		function AddState(const name: string; const base: Vec2; frames, angles: uint; const len: float; const next: string; flags: StateFlags): pStateDesc;
		function TryFindState(const name: string): sint;
		function FindState(const name: string): uint;
		procedure SwitchToState(const name: string);
		procedure SwitchToState(id: uint);

		procedure MoveBy(const delta: Vec2; velocity: float);
		procedure MoveTo(const target: Vec2; velocity: float; cb: MoveCallback; param: pointer);
	private
		function MoveByStep(const delta: Vec2; const by: float; moved: pVec2): boolean;
		function RotateStep(const target: float; const by: float): boolean;
	end;

implementation

	constructor Actor.Init(const size: Vec2; const tex: string; const texSize: Vec2);
	begin
		inherited Init;
		self.size := size;
		self.tex := Texture.Load(tex);
		self.texSize := texSize;
	end;

	destructor Actor.Done;
	begin
		Release(tex);
		inherited Done;
	end;

	procedure Actor.Update(const dt: float);
	var
		moved: Vec2;
	begin
		if length(states) = 0 then raise Error('Актору не заданы состояния.');

		case mvMethod of
			NotMoving:
				if MovingState in states[state].flags then SwitchToState('idle');
			MovingBy:
				begin
					if RotateStep(ArcTan2(mvPointOrDelta.y, mvPointOrDelta.x), 12.0 * dt) then MoveByStep(mvPointOrDelta, mvVel * dt, @moved);
					mvMethod := NotMoving;
				end;
			MovingTo:
				if RotateStep(ArcTan2(mvPointOrDelta.y - pos.y, mvPointOrDelta.x - pos.x), 12.0 * dt) then
					if MoveByStep(mvPointOrDelta - pos, mvVel * dt, nil) then
						mvMethod := NotMoving;
		end;

		if states[state].len > 0 then
		begin
			states[state].phase += dt;
			if states[state].phase >= states[state].len then
			begin
				states[state].phase := modf(states[state].phase, states[state].len);
				SwitchToState(states[state].next);
			end;
		end;
	end;

	procedure Actor.Draw(const camera: Vec2);
	var
		visiblePos: Vec2;
		an, anStep, frame: float;
	begin
		if length(states) = 0 then raise Error('Актору не заданы состояния.');
		visiblePos := self.pos - camera;
		Assert(AngleNormalized(angle), ToString(angle));

		an := angle; if an < 0 then an += TwoPi;
		anStep := floor(states[state].angles * an * (1/TwoPi)); if anStep = states[state].angles then anStep := 0;
		Assert((anStep >= 0) and (anStep < states[state].angles), Format('{0}/{1}', [anStep, states[state].angles]));

		frame := floor(states[state].frames * states[state].phase / max(0.1, states[state].len)); if frame = states[state].frames then frame := 0;
		Assert((frame >= 0) and (frame < states[state].frames), Format('{0}/{1}', [frame, states[state].frames]));

		Quad.DrawPlain(tex, visiblePos, size, states[state].base + Vec2.Make(frame * texSize.x, anStep * texSize.y), texSize);
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
		mvMethod := MovingBy;
		mvPointOrDelta := delta;
		mvVel := velocity;
	end;

	procedure Actor.MoveTo(const target: Vec2; velocity: float; cb: MoveCallback; param: pointer);
	begin
		mvMethod := MovingTo;
		mvPointOrDelta := target;
		mvVel := velocity;
		mvCb := cb;
		mvParam := param;
	end;

	function Actor.MoveByStep(const delta: Vec2; const by: float; moved: pVec2): boolean;
	var
		m: Vec2;
		sql: float;
	begin
		sql := delta.SqrLength;
		result := sqr(by) >= sql;
		if result then m := delta else m := delta * (by / sqrt(sql));
		pos += m;
		if Assigned(moved) then moved^ := m;
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

