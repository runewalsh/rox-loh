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

		MoveCallbackWoActor = procedure(reason: MoveCallbackReason; param: pointer);
		MoveCallbackWithActor = procedure(reason: MoveCallbackReason; ac: pActor; param: pointer);
		MoveCallbackKind = (MoveCallbackNotSet, UseMoveCallbackWoActor, UseMoveCallbackWithActor);
		MoveCallback = record
		case kind: MoveCallbackKind of
			UseMoveCallbackWoActor: (woa: MoveCallbackWoActor);
			UseMoveCallbackWithActor: (wa: MoveCallbackWithActor);
		end;

		MoveTargeter = (NotMoving, MovingBy, MovingTo);
	var
		tex: pTexture;
		texSize: Vec2;
		states: array of StateDesc;
		forceState: sint;
		forceStateTimeout: float;

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

		wieldingWeapon: boolean;
		aimOrigins: array of Vec2;

		constructor Init(const size: Vec2; const tex: string; const texSize: Vec2);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw(const view: Transform2); virtual;
		function Collision: Circle;
		procedure Cleanup;

		function AddState(const name: string; const base: Vec2; frames, angles: uint; const len: float; const next: string; flags: StateFlags): pStateDesc;
		function TryFindState(const name: string): sint;
		function FindState(const name: string): uint;
		function TrySwitchToState(const name: string): boolean;
		procedure SwitchToState(const name: string);
		procedure SwitchToState(id: uint);

		procedure MoveBy(const delta: Vec2; velocity: float);
		procedure MoveTo(const target: Vec2; velocity: float; const cb: MoveCallback; param: pointer);
		procedure StopMoving;
		function HeartPos: Vec2; virtual;
		function AimOrigin: Vec2;

		procedure RotateTo(const point: Vec2);
		procedure StopRotating;

		procedure WieldWeapon;
		procedure UnwieldWeapon;
		procedure SetupAimOrigins(const orig: array of Vec2);
		procedure Fire;
	private
		procedure SwitchMove(method: MoveTargeter);
		function MoveByStep(const delta: Vec2; const by: float; moved: pVec2): boolean;
		function RotateStep(const target: float; const by: float): boolean;
		procedure ShotMoveCallback(reason: MoveCallbackReason);
	end;
	operator :=(null: pointer): Actor.MoveCallback;
	operator :=(woa: Actor.MoveCallbackWoActor): Actor.MoveCallback;
	operator :=(wa: Actor.MoveCallbackWithActor): Actor.MoveCallback;

implementation

	constructor Actor.Init(const size: Vec2; const tex: string; const texSize: Vec2);
	begin
		inherited Init(Transform2.Identity, size);
		self.tex := Texture.Load(tex);
		self.texSize := texSize;
		angle := -HalfPi;
		forceState := -1;
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
		if forceState >= 0 then
		begin
			forceStateTimeout -= dt;
			if forceStateTimeout <= 0 then forceState := -1;
		end;

		case mvMethod of
			NotMoving:
				if MovingState in states[state].flags then
					if wieldingWeapon then SwitchToState('idle-wpn') else SwitchToState('idle');
			MovingBy:
				begin
					if wieldingWeapon or (rtMethod <> NotRotating) or RotateStep(ArcTan2(mvPointOrDelta.y, mvPointOrDelta.x), 10.0 * dt) then
						MoveByStep(mvPointOrDelta, mvVel * dt, @moved);
					mvMethod := NotMoving;

					// location^.ActivateTriggerAt(HeartPos, @self);
					// ↑ это активирует триггеры при входе через стрелки (в дверь, например)
					// иногда удобно, иногда нежелательно, так что лучше настраивать для каждого, а пока убрал
				end;
			MovingTo:
				begin
					if wieldingWeapon or (rtMethod <> NotRotating) or RotateStep(ArcTan2(mvPointOrDelta - HeartPos), 10.0 * dt) then
						if MoveByStep(mvPointOrDelta - HeartPos, mvVel * dt, nil) then
						begin
							if mvCb.kind <> MoveCallbackNotSet then
							begin
								MakeRef(@self);
								try
									ShotMoveCallback(TargetReached);
									detached := not Assigned(location);
								finally
									ReleaseWeak(@self);
								end;
								if detached then exit;
							end;
							mvMethod := NotMoving;
						end;

					// а вот здесь безусловное автосрабатывание уже получше смотрится
					// (но всё равно для чего-то, что игрок может активировать случайно, лучше настраивать)
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
				if states[state].next <> '' then
				begin
					states[state].phase := modf(states[state].phase, states[state].len);
					SwitchToState(states[state].next);
				end else
					states[state].phase := states[state].len;
		end;
	end;

	procedure Actor.HandleDraw(const view: Transform2);
		procedure Draw(const state: StateDesc);
		var
			q: Quad;
			an, anStep, frame: float;
		begin
			Assert(AngleNormalized(angle), ToString(angle));
			an := angle; if an < 0 then an += TwoPi;
			anStep := floor(state.angles * an * (1/TwoPi) + 0.5); if anStep = state.angles then anStep := 0;
			Assert((anStep >= 0) and (anStep < state.angles), Format('{0}/{1}', [anStep, state.angles]));

			frame := floor(state.frames * state.phase / max(0.1, state.len) {+ IfThen(state.next <> '', 0.5, 0)});
			if frame >= state.frames then
				if state.next = '' then frame := state.frames - 1 else frame := 0;
			Assert((frame >= 0) and (frame < state.frames), Format('{0}/{1}', [frame, state.frames]));

			q.fields := [q.Field.Transform];
			q.transform := view * self.local;
			q.Draw(tex, Vec2.Zero, size, state.base + Vec2.Make(frame * texSize.x, anStep * texSize.y), texSize);
		end;

	begin
		if length(states) = 0 then raise Error('Актору не заданы состояния.');
		if forceState >= 0 then Draw(states[forceState]) else Draw(states[state]);
	end;

	function Actor.Collision: Circle;
	begin
		result := Circle.Make(HeartPos, 0.4 * size.x);
	end;

	procedure Actor.Cleanup;
	begin
		mvCb.kind := MoveCallbackNotSet;
		idclip := no;
		if Assigned(location) then Detach;
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

	procedure Actor.MoveTo(const target: Vec2; velocity: float; const cb: MoveCallback; param: pointer);
	begin
		SwitchMove(MovingTo);
		mvPointOrDelta := target;
		mvVel := velocity;
		mvCb := cb;
		mvParam := param;
	end;

	procedure Actor.StopMoving;
	begin
		if mvMethod = MovingTo then ShotMoveCallback(MovingCanceled);
		mvMethod := NotMoving;
	end;

	function Actor.HeartPos: Vec2;
	begin
		result := local.trans + local.rot * Vec2.Make(0.5, 0.2) * size;
	end;

	function Actor.AimOrigin: Vec2;
	var
		an: float;
		step: sint;
	begin
		if length(aimOrigins) = 0 then
			result := HeartPos
		else
		begin
			an := NormalizeAngle(angle); if an < 0 then an += TwoPi;
			step := round(an * (1/TwoPi) * length(aimOrigins)); if step = length(aimOrigins) then step := 0;
			Assert((step >= 0) and (step < length(aimOrigins)), Format('{0}/{1}/{2}', [an, step, length(aimOrigins)]));
			result := local * (aimOrigins[step] * size);
		end;
	end;

	procedure Actor.RotateTo(const point: Vec2);
	begin
		rtMethod := RotatingToPoint;
		rtPoint := point;
	end;

	procedure Actor.StopRotating;
	begin
		rtMethod := NotRotating;
	end;

	procedure Actor.WieldWeapon;
	begin
		if wieldingWeapon then exit;
		wieldingWeapon := yes;
		SwitchToState(states[state].name + '-wpn');
	end;

	procedure Actor.UnwieldWeapon;
	begin
		if not wieldingWeapon then exit;
		wieldingWeapon := no;
		SwitchToState(CutSuffix('-wpn', states[state].name));
	end;

	procedure Actor.SetupAimOrigins(const orig: array of Vec2);
	var
		i: sint;
	begin
		SetLength(aimOrigins, length(orig));
		for i := 0 to High(aimOrigins) do
			aimOrigins[i] := orig[i];
	end;

	procedure Actor.Fire;
	begin
		forceState := FindState('firing');
		forceStateTimeout := states[forceState].len;
	end;

	procedure Actor.SwitchMove(method: MoveTargeter);
	begin
		StopMoving;
		if wieldingWeapon then
			if not TrySwitchToState('walk-wpn') then SwitchToState('idle-wpn') else
		else
			if not TrySwitchToState('walk') then SwitchToState('idle') else;
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
		dAngle: float;
	begin
		Assert(AngleNormalized(target), ToString(target));
		dAngle := AngleDiff(target, angle);
		result := abs(dAngle) <= by;
		if result then angle := target else angle := NormalizeAngle(angle + clamp(dAngle, -by, by));
	end;

	procedure Actor.ShotMoveCallback(reason: MoveCallbackReason);
	begin
		case mvCb.kind of
			UseMoveCallbackWoActor: mvCb.woa(reason, mvParam);
			UseMoveCallbackWithActor: mvCb.wa(reason, @self, mvParam);
		end;
		mvCb.kind := MoveCallbackNotSet;
	end;

	operator :=(null: pointer): Actor.MoveCallback; begin Assert(not Assigned(null)); result.kind := MoveCallbackNotSet; end;
	operator :=(woa: Actor.MoveCallbackWoActor): Actor.MoveCallback;   begin result.kind := UseMoveCallbackWoActor; result.woa := woa; end;
	operator :=(wa: Actor.MoveCallbackWithActor): Actor.MoveCallback; begin result.kind := UseMoveCallbackWithActor; result.wa := wa; end;

end.

