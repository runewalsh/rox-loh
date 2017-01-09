unit Character;

// tolerance modifiers — хитрые значения.
// Если >= 0, toleranceCont = tolerance + toleranceContModifier.
// Если < 0, toleranceCont = tolerance * (-toleranceContModifier + 1.0).
// См. ApplyToleranceModifier_GTE().

{$include opts.inc}
{$ifdef Debug}
	{-$define ExtDebug}
{$endif}

interface

uses
	USystem, UMath, Random, {$ifdef Debug} ULog, {$endif} Streams, UClasses, Utils, USkeleton, Script,
	SceneGraph, Physics, PathFinding, GUI, Inventories;

type
	pVitalParameter = ^VitalParameter;
	VitalParameter = object(&Object)
	public type
		OnChangeProc = procedure(vp: pVitalParameter; const newValue: float; const info: SingleDelegateInfo);
		OnTrailBeginEndProc = procedure(vp: pVitalParameter; const info: SingleDelegateInfo);
		OnTrailProcessProc = procedure(vp: pVitalParameter; const trail: float; const info: SingleDelegateInfo);

		pOnChangeArgs = ^OnChangeArgs;
		OnChangeArgs = record
			vp: pVitalParameter;
			newValue: pFloat;
		end;
		pOnTrailProcessArgs = ^OnTrailProcessArgs;
		OnTrailProcessArgs = record
			vp: pVitalParameter;
			trail: pFloat;
		end;
	public const
		DefaultEffectTime = 0.1;
	private var
		_owner: pObject;
		_reversed, _changed: boolean;
		_value, _max, _trail: float;
		overTime: array of record
			rest, perSec: float;
		end;
		bindings: array of IndicatorGroup.pBinding;
		procedure _SetValue(const newValue: float);
		function _GetNormalized: float;
		procedure _SetNormalized(const newValue: float);
		procedure _AddOverTime(const x: float; const time: float);
		function _ProcessOverTime(const dt: float): float;
		procedure _OnChange;
	public
		onChange,
		onTrailBegin,
		onTrailProcess,
		onTrailEnd: MultiDelegate;
		regen: ModifiableValue;

		constructor Init(newOwner: pObject; const newMax: float; const baseRegen: float; reversed: boolean);
		destructor Done; virtual;
		procedure Process(const dt: float);
		procedure Affect(x: float; const time: float = 0.0);
		procedure Damage(const x: float; const time: float = 0.0);
		procedure Heal(const x: float; const time: float = 0.0);
		procedure ForceUpdate;
		procedure Bind(group: pIndicatorGroup; const name: string);

		property Owner: pObject read _owner;
		property Value: float read _value write _SetValue;
		property NValue: float read _GetNormalized write _SetNormalized;
	end;

type
	DollFlag =
	(
		doll_UserControlled,
		doll_DisableRotationLock,
		doll_CanWalk, doll_CanRun, doll_CanFly, doll_CanFire, doll_CanJump, doll_CanDie,
		doll_CanWait, doll_CanMelee
	);
	DollFlags = set of DollFlag;

	DollStat = (doll_HP, doll_MP, doll_Conf, doll_Pois);
	DollStats = set of DollStat;

	DollMovementMode = (move_Offset, move_Straight, move_Way);
	DollMovementMethod = (move_Walk, move_Run, move_Fly, move_Auto);
	ActualDollMovementMethod = move_Walk .. move_Fly;
	DollMovementAngle = (move_Forward, move_LStrafe, move_Backward, move_RStrafe);

	DollRotateMode = (rotate_Offset, rotate_Target);

	DollState =
	(
		doll_Idle, doll_IdleFly, doll_Walk, doll_Run, doll_Fly, doll_Death, doll_Wait,
		doll_Rotate,
		doll_Fire, doll_Melee
	);
	DollStates = set of DollState;

type
	DollAnimationID = (anim_0, anim_1, anim_2, anim_3, anim_4);
	DollAnimationIDs = set of DollAnimationID;

const
	anim_Base    = anim_0;
	anim_Forward = anim_1;
	anim_Backward = anim_2;
	anim_Left    = anim_3;
	anim_Right   = anim_4;

type
	pConfusionEffects = ^ConfusionEffects;
	ConfusionEffects = object
	private
		_phaseA, _phaseB, _phaseC: float;
		_moveDistortion, _automatism: Vec3;
	public
		constructor Init;
		destructor Done;
		procedure Serialize(stream: pStream);
		procedure Deserialize(stream: pStream);
		procedure Process(const value: float; const dt: float);
		property MoveDistortion: Vec3 read _moveDistortion;
		property Automatism: Vec3 read _automatism;
	end;

	pStateTransitionInfo = ^tStateTransitionInfo;
	tStateTransitionInfo = object
		stateA, stateB: DollState;
		handle: SkeletonNameIdPair;
		procedure Initialize(newStateA, newStateB: DollState);
		procedure Finalize;
	end;

	StateTransitions = object
		list: array of tStateTransitionInfo;
		procedure Initialize;
		procedure Finalize;
		procedure SetTransition(stateA, stateB: DollState; const animName: string; skel: pSkeletonNode);
		function FindTransition(stateA, stateB: DollState): pStateTransitionInfo;
		function ForceTransition(stateA, stateB: DollState): pStateTransitionInfo;
	end;

	pFireBone = ^FireBone;
	FireBone = object
		handle: SkeletonNameIdPair;
		tf: Transform;
		periodDis: Distribution;
		curPeriod, curTimer: float;
		callback: pMultiDelegate;
		constructor Init(skel: pSkeletonNode; const newName: string; const newTransform: Transform; const newPeriodDis: Distribution);
		destructor Done;
		procedure GeneratePeriod;
	end;

	tMoveMethodsInfo = array[ActualDollMovementMethod] of record
		maxVelocity: float;
		toleranceK: float;
		phaseVelocity: float;
	end;

	pDoll = ^Doll;

	pStateOpts = ^tStateOpts;
	tStateOpts = object
		state: DollState;
		onInit, onProcess, onDone: MultiDelegate;
		anims: array[DollAnimationID] of SkeletonNameIdPair;
		lastPhase: float;
		phases: array of record
			phase: float;
			callback: pMultiDelegate;
		end;
		constructor Init(aState: DollState);
		destructor Done;
		procedure Update(doll: pDoll);
		function PhaseCallback(const aPhase: float): pMultiDelegate;
	end;

	Doll = object(RigidBody)
	public type
		OnStateInitDoneProc = procedure(doll: pDoll; state: DollState; const info: SingleDelegateInfo);
		OnStateInitDoneArgs = record
			doll: pDoll;
			state: DollState;
		end;

		OnStateProcessProc = procedure(doll: pDoll; state: DollState; const dt: float; const info: SingleDelegateInfo);
		OnStateProcessArgs = record
			doll: pDoll;
			state: DollState;
			dt: pFloat;
		end;

		OnCanFlyCheckProc = function(doll: pDoll; up: boolean; const info: SingleDelegateInfo): boolean;
		OnCanFlyCheckArgs = record
			doll: pDoll;
			up: boolean;
			ret: boolean;
		end;

		OnLandCheckProc = function(doll: pDoll; const info: SingleDelegateInfo): boolean;
		OnLandCheckArgs = record
			doll: pDoll;
			ret: boolean;
		end;

		OnFireProc = procedure(doll: pDoll; boneId: sint; const tf: Transform; const info: SingleDelegateInfo);
		OnFireArgs = record
			doll: pDoll;
			boneId: sint;
			tf: pTransform;
		end;

		OnStatePhaseProc = procedure(doll: pDoll; state: DollState; const phase: float; const info: SingleDelegateInfo);
		OnStatePhaseArgs = record
			doll: pDoll;
			state: DollState;
			phase: pFloat;
		end;

	private const
		AngleValues: array[DollMovementAngle] of float = (0.0, -HalfPi, Pi, HalfPi);
		MinWalkTolerance = 0.1;
		DefaultWalkTolerance = 0.3;
		DefaultWalkToleranceContModifier = -1.0;
		MinRotateTolerance = 0.04;
		DefaultRotateTolerance = 0.1;
		DefaultRotateToleranceContModifier = 0.1 * Pi;
		MovingStates = [doll_Walk, doll_Run, doll_Fly];
		FlyingStates = [doll_Fly, doll_IdleFly];
		RelRadiusXZ = 0.25; // TODO: не константа, вообще-то
	public const
		FlagIds: array[DollFlag] of string = ('user_controlled', 'disable_rotation_lock',
			'can_walk', 'can_run', 'can_fly', 'can_fire', 'can_jump', 'can_die', 'can_wait', 'can_melee');
		StateIds: array[DollState] of string =
		(
			'idle', 'idle-fly', 'walk', 'run', 'fly', 'death', 'wait',
			'rotate',
			'fire', 'melee'
		);
		AngleIds: array[DollMovementAngle] of string = ('f', 'l', 'b', 'r');
		MoveMethodIds: array[DollMovementMethod] of string = ('walk', 'run', 'fly', 'auto');
		StatIds: array[DollStat] of string = ('hp', 'mp', 'conf', 'pois');
	private var
		_flags: DollFlags;
		_skeleton: pSkeletonNode;
		_stateTransitions: StateTransitions;
		_activeTransition: pStateTransitionInfo;
		_heartShift: Vec3;

		_acStates: array of DollState;
		_acStatesSet, _possibleStates: DollStates;

		_moveMode: DollMovementMode;
		_moveParam: Vec3;
		_moveTolerance, _moveToleranceCont: float;
		_moveAngle, _moveVelocity: float;
		_way: pWay;
		_waySeg: sint;
		_wayCalcTime, _checkLandTime: float;
		_lastCheckLand: boolean;

		_rotateMode: DollRotateMode;
		_rotateTarget: Quaternion;
		_rotateTolerance, _rotateToleranceCont: float;

		_strafePenalty, _backwardPenalty: float;
		_moveMethods: tMoveMethodsInfo;

		_fire_bones: array of FireBone;
		_meleeVariation: DollAnimationID;
		_confusionEffects: pConfusionEffects;
		_inventory: pInventory;

		procedure _Initialize;
		function _GetFlag(flag: DollFlag): boolean;
		procedure _SetFlag(flag: DollFlag; value: boolean);
		procedure _SetSkeleton(newSkel: pSkeletonNode);
		procedure _UpdateSkeleton(skel: pSkeletonNode);
		function _uniform2walk(const uni: Vec3): Vec3;
		function _walk2uniform(const walk: Vec3): Vec3;
		function _AlignToGravity(const rot: Quaternion): Quaternion;
		function _GetState(var method: DollMovementMethod): DollState;
		function _WayIsOutOfDate(const temporalTolerance, spatialTolerance: float): boolean;
		function _PrepareMove(method: ActualDollMovementMethod; const irot: Quaternion; out v: Vec3): boolean;
		function _PrepareRotate(out vel: float): boolean;
		procedure _HandleStateInit(state: DollState);
		procedure _HandleStateProcess(state: DollState; const dt: float);
		procedure _HandleStateDone(state: DollState);
		function _EnableState(aState: DollState; allowTransition: boolean): boolean;
		function _EnableState(aState: DollState): boolean;
		function _DisableState(aState: DollState): boolean;
		function _GetStateAnimWeightCoef(state: DollState): float;
		procedure _UseWalkAnimation(state: DollState; const vel: Vec3);
		procedure _UseFlyAnimation(state: DollState; const vel: Vec3);
		procedure _ProcessIdle(const dt: float);
		procedure _ProcessIdleFly;
		procedure _ProcessWalk(state: DollState; const dt: float);
		procedure _ProcessFly;
		procedure _ProcessRotate(const dt: float);
		procedure _ProcessFire(const dt: float);
		procedure _ProcessMelee(state: DollState);

	protected
		procedure _OnUpdate(const dt: float); virtual;
		function _SuitsTo(know: SceneKnowledge): boolean; virtual;
	public
		states: array[DollState] of pStateOpts;
		onCanFlyCheck, onLandCheck: MultiDelegate;
		stats: array[DollStat] of pVitalParameter;

		constructor Init(const newFlags: DollFlags);
		destructor Done; virtual;
		function HeartPos: Vec3;
		procedure SwitchStatOn(stat: DollStat; const max, regen: float; reversed: boolean);
		procedure ForceUpdate;
		procedure SetStateTransition(stateA, stateB: DollState; const animName: string);

		procedure RotateBy(const delta: Quaternion);
		procedure RotateTo(const target: Quaternion; const tolerance: float = DefaultRotateTolerance;
			const toleranceContModifier: float = DefaultRotateToleranceContModifier);
		procedure MoveBy(const wasd: Vec3; method: DollMovementMethod);
		procedure MoveTo(const target: Vec3; const vel, angle: float; const tolerance, toleranceContModifier: float; method: DollMovementMethod);
		procedure MoveTo(const target: Vec3; const vel: float = 0.0; angle: DollMovementAngle = move_Forward;
			const tolerance: float = DefaultWalkTolerance; const toleranceContModifier: float = DefaultWalkToleranceContModifier;
			method: DollMovementMethod = move_Auto);
		function FireCallback(const bone: string; const tf: Transform; const periodDis: Distribution): pMultiDelegate;
		function CanFly(up: boolean): boolean;
		function CheckLand: boolean;
		function FlyUp: boolean;
		function Moves: boolean;
		function Flies: boolean;
		procedure Land;
		procedure ContinuousFire(fire: boolean);
		procedure Melee;
		procedure Stop;
		procedure Die;
		procedure EnableInventory(const size: UintVec2);

		property HeartShift: Vec3 read _heartShift write _heartShift;
		property Skeleton: pSkeletonNode read _skeleton write _SetSkeleton;
		property Way: pWay read _way;
		property UserControlled: boolean index doll_UserControlled read _GetFlag write _SetFlag;
		property Inventory: pInventory read _inventory;
	end;

	function ApplyToleranceModifier_GTE(const x: float; const modifier: float): float;
	procedure OpenScript(var script: ScriptState);

implementation

uses
	Scene, MMSystem, Script_EngineAPI
{$ifdef use_serialization}, Serialization {$endif}
{$ifdef Profile}, Profile {$endif};

const
	StatesT = [doll_Idle .. doll_Wait];
	StatesR = [doll_Rotate .. doll_Rotate];
	StatesA = [doll_Fire .. doll_Melee];

	DollStateInfo: array[DollState] of record
		conflicts: DollStates;
		anims: array[DollAnimationID] of string;
		ref: DollAnimationID;
	end =
	(
		( // doll_Idle
			conflicts: StatesT;
			anims: ('stand', 'walk', 'walk-b', 'strafe-l', 'strafe-r');
			ref: anim_Forward
		),
		(// doll_IdleFly
			conflicts: StatesT;
			anims: ('fly', 'fly-f', 'fly-b', 'fly-l', 'fly-r');
			ref: anim_Base
		),
		( // doll_Walk
			conflicts: StatesT;
			anims: ('stand', 'walk', 'walk-b', 'strafe-l', 'strafe-r');
			ref: anim_Forward
		),
		( // doll_Run
			conflicts: StatesT;
			anims: ('stand', 'run', 'run-b', 'run-l', 'run-r');
			ref: anim_Forward
		),
		( // doll_Fly
			conflicts: StatesT;
			anims: ('fly', 'fly-f', 'fly-b', 'fly-l', 'fly-r');
			ref: anim_Forward
		),
		( // doll_Death
			conflicts: StatesT + StatesR + StatesA;
			anims: ('death', '', '', '', '');
			ref: anim_Base
		),
		( // doll_Wait
			conflicts: StatesT;
			anims: ('wait', '', '', '', '');
			ref: anim_Base
		),
		( // doll_Rotate
			conflicts: StatesR;
			anims: ('', '', '', '', '');
			ref: anim_Base
		),
		( // doll_Fire
			conflicts: StatesA;
			anims: ('fire', '', '', '', '');
			ref: anim_Base
		),
		( // doll_Melee
			conflicts: StatesA;
			anims: ('melee', 'melee2', 'melee3', 'melee4', 'melee5');
			ref: anim_Base
		)
	);

	procedure VitalParameter._SetValue(const newValue: float);
	var
		nv: float;
	begin
		nv := Clamp(newValue, 0.0, _max);
		if (not Equals(_value, nv)) or (((nv = 0.0) or (nv = _max)) and (_value <> nv)) then
		begin
			_changed := yes;
			_value := nv;
		end;
	end;

	function VitalParameter._GetNormalized: float;
	begin
		result := _value / _max;
	end;

	procedure VitalParameter._SetNormalized(const newValue: float);
	begin
		_SetValue(newValue * _max);
	end;

	constructor VitalParameter.Init(newOwner: pObject; const newMax: float; const baseRegen: float; reversed: boolean);
	begin
		inherited Init;
		if Assigned(newOwner) then
			_owner := newOwner
		else
			_owner := @self;
		_max := newMax;
		_reversed := reversed;
		if _reversed then
			_value := 0.0
		else
			_value := newMax;
		_changed := yes;
		_trail := 0.0;
		regen.Init(baseRegen);

		bindings := nil;
		onChange.Init;
		onTrailBegin.Init;
		onTrailProcess.Init;
		onTrailEnd.Init;
	end;

	destructor VitalParameter.Done;
	begin
		ReleaseArray(USystem.ObjectsList(bindings));

		onTrailEnd.Done;
		onTrailProcess.Done;
		onTrailBegin.Done;
		onChange.Done;
		regen.Done;
		inherited Done;
	end;

	function VitalParameter._ProcessOverTime(const dt: float): float;
	var
		i: sint;
		delta: float;
		expired: boolean;
	begin
		result := 0.0;
		for i := High(overTime) downto 0 do
		begin
			delta := overTime[i].perSec * dt;
			expired := GreaterThanEqual(abs(delta), abs(overTime[i].rest));
			if expired then delta := overTime[i].rest;
			result += delta;
			if expired then
			begin
				overTime[i] := overTime[High(overTime)];
				SetLength(overTime, length(overTime) - 1);
			end else
				overTime[i].rest -= delta;
		end;
	end;

	procedure CallOnChange(const info: SingleDelegateInfo; param: pointer);
	var
		args: VitalParameter.pOnChangeArgs absolute param;
	begin
		VitalParameter.OnChangeProc(info.proc)(args^.vp, args^.newValue^, info);
	end;

	procedure CallOnTrailBeginEnd(const info: SingleDelegateInfo; param: pointer);
	var
		vp: pVitalParameter absolute param;
	begin
		VitalParameter.OnTrailBeginEndProc(info.proc)(vp, info);
	end;

	procedure CallOnTrailProcess(const info: SingleDelegateInfo; param: pointer);
	var
		args: VitalParameter.pOnTrailProcessArgs absolute param;
	begin
		VitalParameter.OnTrailProcessProc(info.proc)(args^.vp, args^.trail^, info);
	end;

	procedure VitalParameter._OnChange;
	var
		args: OnChangeArgs;
		i: sint;
	begin
		if not onChange.Empty then
		begin
			args.vp       := @self;
			args.newValue := @_value;
			onChange.Call(@CallOnChange, @args);
		end;

		for i := 0 to High(bindings) do
			bindings[i]^.ChangeValue(NValue);
	end;

	procedure VitalParameter.Process(const dt: float);
	var
		args: OnTrailProcessArgs;
	begin
		Affect(_ProcessOverTime(dt) + regen.Value * dt);
		if _changed then
		begin
			_changed := no;
			_OnChange;
		end;

		if _trail <> 0.0 then
		begin
			if not onTrailProcess.Empty then
			begin
				args.vp    := @self;
				args.trail := @_trail;
				onTrailProcess.Call(@CallOnTrailProcess, @args);
			end;
			_trail -= 1.5 * dt;
			if _trail <= 0.0 then
			begin
				_trail := 0.0;
				if not onTrailEnd.Empty then onTrailEnd.Call(@CallOnTrailBeginEnd, @self);
			end;
		end;
	end;

	procedure VitalParameter.Affect(x: float; const time: float = 0.0);
	var
		delta: float;
	begin
		if (x < 0.0) and not (onTrailBegin.Empty and onTrailProcess.Empty and onTrailEnd.Empty) then
		begin
			delta := min(20.0 * (-x)/_max, 1.0);
			if _trail = 0.0 then
				if not onTrailBegin.Empty then onTrailBegin.Call(@CallOnTrailBeginEnd, @self);
			_trail := min(_trail + delta, 1.0);
		end;
		if time = 0.0 then
		begin
			if _reversed then x := -x;
			_SetValue(_value + x)
		end else
			_AddOverTime(x, time);
	end;

	procedure VitalParameter.Damage(const x: float; const time: float = 0.0);
	begin
		Assert(x >= 0.0);
		Affect(-x, time);
	end;

	procedure VitalParameter.Heal(const x: float; const time: float = 0.0);
	begin
		Assert(x >= 0.0);
		Affect(+x, time);
	end;

	procedure VitalParameter._AddOverTime(const x: float; const time: float);
	begin
		Assert(NotZero(time));
		SetLength(overTime, length(overTime) + 1);
		overTime[High(overTime)].rest := x;
		overTime[High(overTime)].perSec := x / time;
	end;

	procedure VitalParameter.ForceUpdate;
	var
		args: OnTrailProcessArgs;
	begin
		if (_trail <> 0.0) and (not onTrailBegin.Empty) then onTrailBegin.Call(@CallOnTrailBeginEnd, @self);
		if (_trail <> 0.0) and (not onTrailProcess.Empty) then
		begin
			args.vp    := @self;
			args.trail := @_trail;
			onTrailProcess.Call(@CallOnTrailProcess, @args);
		end;
		if (_trail = 0.0) and (not onTrailEnd.Empty) then onTrailEnd.Call(@CallOnTrailBeginEnd, @self);
		_OnChange;
	end;

	procedure UnbindIndicator(binding: IndicatorGroup.pBinding; param: pObject);
	var
		vp: pVitalParameter absolute param;
		i: sint;
	begin
		i := Index(binding, pPointer(vp^.bindings), length(vp^.bindings));
		Assert(i >= 0);
		Release(vp^.bindings[i]);
		vp^.bindings[i] := vp^.bindings[High(vp^.bindings)];
		SetLength(vp^.bindings, length(vp^.bindings) - 1);
	end;

	procedure VitalParameter.Bind(group: pIndicatorGroup; const name: string);
	var
		binding: IndicatorGroup.pBinding;
	begin
		binding := group^.Bind(name, @UnbindIndicator, @self);
		if not Assigned(binding) then exit;

		SetLength(bindings, length(bindings) + 1);
		bindings[High(bindings)] := MakeRef(binding);
		binding^.ChangeValue(NValue);
	end;

	constructor ConfusionEffects.Init;
	begin
		_phaseA := GlobalRNG.GetFloat(TwoPi);
		_phaseB := GlobalRNG.GetFloat(TwoPi);
		_phaseC := GlobalRNG.GetFloat(TwoPi);
		_moveDistortion := Vec3.Zero;
		_automatism := Vec3.Zero;
	end;

	destructor ConfusionEffects.Done;
	begin
	end;

	procedure ConfusionEffects.Serialize(stream: pStream);
	begin
		Serialize_f32(stream, _phaseA);
		Serialize_f32(stream, _phaseB);
		Serialize_f32(stream, _phaseC);
		Serialize_vec3f32(stream, _moveDistortion);
		Serialize_vec3f32(stream, _automatism);
	end;

	procedure ConfusionEffects.Deserialize(stream: pStream);
	begin
		_phaseA := Deserialize_f32(stream);
		_phaseB := Deserialize_f32(stream);
		_phaseC := Deserialize_f32(stream);
		_moveDistortion := Deserialize_vec3f32(stream);
		_automatism := Deserialize_vec3f32(stream);
	end;

	procedure ConfusionEffects.Process(const value: float; const dt: float);
	const
		MinToAutomatism = 0.4;
		InvMinToAutomatism = 1.0 / MinToAutomatism;
	begin
		_phaseA += 1.5 * dt;
		_phaseB += 1.5 * dt;
		_phaseC += 1.5 * dt;
		_moveDistortion := value * Vec3.Make(sin(_phaseA), sin(_phaseB), sin(_phaseC));

		if value > MinToAutomatism then
			_automatism := (value - MinToAutomatism) * InvMinToAutomatism * _moveDistortion
		else
			_automatism := Vec3.Zero;
	end;

	procedure _CallOnStateInitDone(const info: SingleDelegateInfo; param: pointer);
	var
		args: ^Doll.OnStateInitDoneArgs absolute param;
	begin
		Doll.OnStateInitDoneProc(info.proc)(args^.doll, args^.state, info);
	end;

	procedure _CallOnStateProcess(const info: SingleDelegateInfo; param: pointer);
	var
		args: ^Doll.OnStateProcessArgs absolute param;
	begin
		Doll.OnStateProcessProc(info.proc)(args^.doll, args^.state, args^.dt^, info);
	end;

	procedure Doll._HandleStateInit(state: DollState);
	const
		ScrewCast: Vec3 = (data: (0.0, -2.0, 0.0));
	var
		args: OnStateInitDoneArgs;
		i, n: sint;
		aid: DollAnimationID;
	begin
		case state of
			doll_IdleFly: self.UnaffectedByGravity := yes;
			doll_Fly: self.UnaffectedByGravity := yes;
			doll_Death:
				begin
					if not (doll_DisableRotationLock in _flags) then
					begin
						self.InfiniteRotIX := no;
						self.InfiniteRotIZ := no;
					end;
					if Assigned(inventory) then
					begin
						inventory^.Screw(_heartShift, ScrewCast, @self);
						Release(_inventory);
					end;
				end;
			doll_Fire:
				for i := 0 to High(_fire_bones) do
				begin
					_fire_bones[i].GeneratePeriod;
					_fire_bones[i].curTimer := 0.0;
				end;
			doll_Melee:
				begin
					n := 0;
					for aid in DollAnimationID do
						if states[doll_Melee]^.anims[aid].id >= 0 then inc(n) else break;
					if n > 0 then
					begin
						_meleeVariation := DollAnimationID(GlobalRNG.GetUint(n));
						_skeleton^.SetAnimPhase(states[doll_Melee]^.anims[_meleeVariation].id, 0.0);
					end else
					begin
					{$ifdef Debug} Log('Рукопашная невозможна ввиду отсутствия анимаций.', logError); {$endif}
						_meleeVariation := anim_0;
					end;
				end;
		end;
		if not states[state]^.onInit.Empty then
		begin
			args.doll := @self;
			args.state := state;
			states[state]^.onInit.Call(@_CallOnStateInitDone, @args);
		end;
	end;

	procedure Doll._HandleStateProcess(state: DollState; const dt: float);
	var
		args: OnStateProcessArgs;
	begin
		if not states[state]^.onProcess.Empty then
		begin
			args.doll := @self;
			args.state := state;
			args.dt := @dt;
			states[state]^.onProcess.Call(@_CallOnStateProcess, @args);
		end;
		if (state in StatesT) and (_moveMode = move_Offset) then
			_moveParam := Vec3.Zero;
	end;

	procedure Doll._HandleStateDone(state: DollState);
	var
		args: OnStateInitDoneArgs;
		om: Vec3;
	begin
		if not states[state]^.onDone.Empty then
		begin
			args.doll := @self;
			args.state := state;
			states[state]^.onDone.Call(@_CallOnStateInitDone, @args);
		end;

		case state of
			doll_IdleFly: self.UnaffectedByGravity := _acStatesSet * FlyingStates <> [];
			doll_Fly: self.UnaffectedByGravity := _acStatesSet * FlyingStates <> [];
			doll_Rotate:
				begin
					om := self.Omega;
					om.y := 0.0;
					self.Omega := om;
				end;
		end;
		if state in StatesT then
			Release(_way);
	end;

	function Doll._EnableState(aState: DollState; allowTransition: boolean): boolean;
	var
		i: sint;
		transition: pStateTransitionInfo;
	begin
		if not (aState in _possibleStates) then
		begin
		{$ifdef Debug} Log('Doll.EnableState: состояние ' + StateIds[aState] + ' для этой куклы недоступно', logError); {$endif}
			exit(no);
		end;
		if aState in _acStatesSet then
		begin
		{$ifdef Debug} Log('Избыточный Doll.EnableState: ' + StateIds[aState], logWarning); {$endif}
			exit(yes);
		end;
		if allowTransition and Assigned(_activeTransition) then
			exit;
		if doll_Death in _acStatesSet then exit(no); // ;_;

		if allowTransition then
		begin
			transition := nil;
			for i := 0 to High(_acStates) do
				if _acStates[i] in DollStateInfo[aState].conflicts then
				begin
					transition := _stateTransitions.FindTransition(_acStates[i], aState);
					if Assigned(transition) then break;
				end;

			if Assigned(transition) then
			begin
				_activeTransition := transition;
				_skeleton^.SetAnimPhase(_activeTransition^.handle.id, 0.0);
				Assert(aState = _activeTransition^.stateB);
				if _activeTransition^.handle.id >= 0 then exit;
			end;
		end;

		_HandleStateInit(aState);
		SetLength(_acStates, length(_acStates) + 1);
		_acStates[High(_acStates)] := aState;
		Include(_acStatesSet, aState);
		states[aState]^.lastPhase := -1.0;

		for i := High(_acStates) downto 0 do
			if (_acStates[i] <> aState) and (_acStates[i] in DollStateInfo[aState].conflicts) then
				_DisableState(_acStates[i]);
		result := yes;
	end;

	function Doll._EnableState(aState: DollState): boolean;
	begin
		result := _EnableState(aState, yes);
	end;

	function Doll._DisableState(aState: DollState): boolean;
	var
		i: sint;
	begin
		if not (aState in _acStatesSet) then
		begin
		{$ifdef Debug} Log('Избыточный Doll.DisableState: ' + StateIds[aState], logWarning); {$endif}
			exit(yes);
		end;

		if Assigned(_activeTransition) and (_activeTransition^.stateB = aState) then
		begin
			_activeTransition := nil;
			exit;
		end;

		for i := 0 to High(_acStates) do
			if _acStates[i] = aState then
			begin
				_acStates[i] := _acStates[High(_acStates)];
				SetLength(_acStates, length(_acStates) -1 );
				break;
			end;
		Exclude(_acStatesSet, aState);
		_HandleStateDone(aState);
		result := yes;
	end;

	function Doll._GetStateAnimWeightCoef(state: DollState): float;
	begin
		Assert(@state = @state);
		result := 1.0;
	end;

	procedure Doll._UseWalkAnimation(state: DollState; const vel: Vec3);
	const
		StandToWalkThreshold = 0.4;
	var
		velxz: Vec3;
		walk_wt, phaseVel, ak: float;
	begin
		velxz := Vec3.Make(vel.x, clamp(0.5 * vel.y, -1.0, 1.0), vel.z);
		walk_wt := min(velxz.Length, 0.75);
		phaseVel := _moveMethods[move_Walk].phaseVelocity * min(velxz.Length, 6.0);
		velxz := velxz.Normalized;

		ak := _GetStateAnimWeightCoef(state);
		_skeleton^.UseAnim(states[state]^.anims[anim_Base].id, ak * (StandToWalkThreshold - walk_wt), 1.0);
		_skeleton^.UseAnim(states[state]^.anims[anim_Forward].id, ak * walk_wt * velxz.z, phaseVel);
		_skeleton^.UseAnim(states[state]^.anims[anim_Backward].id, ak * walk_wt * -velxz.z, phaseVel);
		_skeleton^.UseAnim(states[state]^.anims[anim_Left].id, ak * walk_wt * velxz.x, phaseVel);
		_skeleton^.UseAnim(states[state]^.anims[anim_Right].id, ak * walk_wt * -velxz.x, phaseVel);
	end;

	procedure Doll._UseFlyAnimation(state: DollState; const vel: Vec3);
	const
		FlyWt = 1.5;
	var
		v, vxz: Vec3;
		phaseVel, ak: float;
	begin
		phaseVel := _moveMethods[move_Fly].phaseVelocity;
		ak := _GetStateAnimWeightCoef(state);
		v := vel.Clamped(1.0);
		vxz := v;
		vxz.y := 0.0;

		_skeleton^.UseAnim(states[state]^.anims[anim_Base].id, ak * max(0.4 - 1.0*vxz.Length + FlyWt*abs(v.y), 0), phaseVel);
		_skeleton^.UseAnim(states[state]^.anims[anim_Forward].id, ak * FlyWt * v.z, phaseVel);
		_skeleton^.UseAnim(states[state]^.anims[anim_Backward].id, ak * FlyWt * -v.z, phaseVel);
		_skeleton^.UseAnim(states[state]^.anims[anim_Left].id, ak * FlyWt * v.x, phaseVel);
		_skeleton^.UseAnim(states[state]^.anims[anim_Right].id, ak * FlyWt * -v.x, phaseVel);
	end;

	procedure Doll._ProcessIdle(const dt: float);
	var
		vel, nv: Vec3;
	begin
		if (not (doll_UserControlled in _flags)) and CanFly(yes) and (not CheckLand) then
		begin
			_EnableState(doll_IdleFly);
			exit;
		end;

		if Assigned(_confusionEffects) then
			if self.Velocity.sqrLength < 2.0 * _confusionEffects^.Automatism.SqrLength then
			begin
				vel := self.Velocity;
				nv := 0.7 * _confusionEffects^.Automatism;
				self.Velocity := Vec3.Make(nv.x, vel.y, nv.z);
			end;

		if doll_CanWalk in _flags then
			_UseWalkAnimation(doll_Idle, WorldRot.Inversed * self.Velocity)
		else
			_skeleton^.UseAnim(states[doll_Idle]^.anims[anim_Base].id, _GetStateAnimWeightCoef(doll_Idle), 1.0);

		self.Omega := self.Omega * pow(0.2, dt);
	end;

	procedure Doll._ProcessIdleFly;
	const
		DirK: Vec3 = (data: (0.75, 1.0, 0.75));
	begin
		_UseFlyAnimation(doll_IdleFly, WorldRot.Inversed * self.Velocity * DirK);
		if (not CanFly(no)) or (([doll_CanWalk, doll_UserControlled] * _flags = [doll_CanWalk]) and CheckLand) then Land;

		if Assigned(_confusionEffects) then
			self.selfForce := self.selfForce + self.Mass * _confusionEffects^.Automatism;
	end;

	procedure Doll._ProcessWalk(state: DollState; const dt: float);
	const
		Acceleration = 50.0;
	var
		method: DollMovementMethod;
		irot: Quaternion;
		maxvel: float;
		tv, tv_XZ, tv_XZt: Vec3;
		oldvel, oldvel_XZ, oldvel_XZt, newvel_XZt, newvel_XZ, newvel: Vec3;
		walkDir: Vec3;
	begin
		if (not (doll_UserControlled in _flags)) and CanFly(yes) and (not CheckLand) then
		begin
			_EnableState(doll_Fly);
			exit;
		end;

		irot := WorldRot.Inversed;
		if state = doll_Run then method := move_Run else method := move_Walk;
		if not _PrepareMove(method, irot, tv) then
		begin
			Stop;
			exit;
		end;
		if tv.SqrLength > 1.0 then tv.Length := 1.0;

		if Assigned(_confusionEffects) then
			tv := tv + _confusionEffects^.MoveDistortion;

		tv_XZ := tv;
		tv_XZ.y := 0.0;

		tv_XZt := _uniform2walk(tv_XZ);

		oldvel := irot * self.Velocity;
		oldvel_XZ := oldvel;
		oldvel_XZ.y := 0.0;
		oldvel_XZt := _uniform2walk(oldvel_XZ);

		newvel_XZt := oldvel_XZt + Acceleration * dt * tv_XZt;

		maxvel := _moveMethods[method].maxVelocity;
		if _moveVelocity <> 0.0 then maxvel := _moveVelocity;

		if newvel_XZt.SqrLength > sqr(maxvel) then
			if oldvel_XZt.SqrLength <= sqr(maxvel) then
				newvel_XZt.Length := maxvel
			else
				newvel_XZt := newvel_XZt.Clamped(oldvel_XZt.Length);

		newvel_XZ := _walk2uniform(newvel_XZt);
		newvel := irot * self.Velocity;
		newvel.X := newvel_XZ.x;
		newvel.Z := newvel_XZ.z;
		self.Velocity := WorldRot * newvel * self.AsRigid^.EntangleCoef;

		case _moveMode of
			move_Offset: walkDir := tv.Normalized;
			else
				walkDir := (newvel_XZ.Normalized + (irot * (_moveParam - HeartPos).Normalized)).Normalized;
		end;
		_UseWalkAnimation(state, walkDir * newvel_XZt.Length);
	end;

	procedure Doll._ProcessFly;
	var
		irot: Quaternion;
		tv, t, force, flyDir: Vec3;
		maxvel: float;
	begin
		if not CanFly(no) then
		begin
			Land;
			exit;
		end;

		irot := WorldRot.Inversed;
		if _PrepareMove(move_Fly, irot, tv) then
		begin
			if tv.SqrLength > 1.0 then tv.Length := 1.0;
			if Assigned(_confusionEffects) then
				tv := tv + _confusionEffects^.MoveDistortion;

			t := tv + Vec3.Make(0, 0, 0.02);
			force := 50.0 * t;
			self.selfForce := self.selfForce + WorldRot * force;
			maxvel := _moveMethods[move_Fly].maxVelocity;
			if _moveVelocity <> 0.0 then maxvel := _moveVelocity;
			self.Velocity := self.Velocity.Clamped(maxvel);

			flyDir := irot * self.Velocity.Clamped(2.0) + 3.0*t;
			_UseFlyAnimation(doll_Fly, flyDir);
		end else
			Stop;
	end;

	procedure Doll._ProcessRotate(const dt: float);
	var
		vel: float;
		an, dAn: float;
	begin
		Assert(@dt = @dt);
		if not _PrepareRotate(vel) then
		begin
			_DisableState(doll_Rotate);
			exit;
		end;

		an := 0.1 + AngleUN(LocalRot * Vec3.PositiveZ, _rotateTarget * Vec3.PositiveZ);
		dAn := an * vel * dt;
		if dAn < an then
			self.LocalRot := slerp(self.LocalRot, _rotateTarget, dAn / an)
		else
			self.LocalRot := _rotateTarget;
		self.Omega := Vec3.Zero;
	end;

	constructor FireBone.Init(skel: pSkeletonNode; const newName: string; const newTransform: Transform; const newPeriodDis: Distribution);
	begin
		handle.Initialize(newName);
		handle.Update(skel, id_Bone);
		new(callback); callback^.Init;
		tf := newTransform;
		periodDis := newPeriodDis;
		GeneratePeriod;
	end;

	destructor FireBone.Done;
	begin
		periodDis.Done;
		callback^.Done; dispose(callback);
		handle.Finalize;
	end;

	procedure FireBone.GeneratePeriod;
	begin
		curPeriod := max(periodDis.GenerateValue, 0.01);
	end;

	constructor tStateOpts.Init(aState: DollState);
	var
		anim: DollAnimationID;
	begin
		state := aState;
		onInit.Init;
		onProcess.Init;
		onDone.Init;
		for anim in DollAnimationID do
			anims[anim].Initialize(DollStateInfo[state].anims[anim]);
		lastPhase := -1.0;
		phases := nil;
	end;

	destructor tStateOpts.Done;
	var
		anim: DollAnimationID;
		i: sint;
	begin
		onInit.Done;
		onProcess.Done;
		onDone.Done;
		for i := 0 to High(phases) do
		begin
			phases[i].callback^.Done;
			dispose(phases[i].callback);
		end;
		for anim in DollAnimationID do
			anims[anim].Finalize;
	end;

	procedure CallOnStatePhase(const info: SingleDelegateInfo; param: pointer);
	var
		args: ^Doll.OnStatePhaseArgs absolute param;
	begin
		Doll.OnStatePhaseProc(info.proc)(args^.doll, args^.state, args^.phase^, info);
	end;

	procedure tStateOpts.Update(doll: pDoll);
	var
		curPhase: float;
		i: sint;
		args: Doll.OnStatePhaseArgs;
	begin
		if anims[DollStateInfo[state].ref].id >= 0 then
		begin
			curPhase := doll^.skeleton^.GetAnimPhase(anims[DollStateInfo[state].ref].id);
			if lastPhase >= 0.0 then
			begin
				for i := 0 to High(phases) do
					if (not phases[i].callback^.Empty) and GreaterThan(curPhase, phases[i].phase) and LessThanEqual(lastPhase, phases[i].phase) then
					begin
						args.doll := doll;
						args.state := state;
						args.phase := @curPhase;
						phases[i].callback^.Call(@CallOnStatePhase, @args);
					end;
			end;
			lastPhase := curPhase;
		end;
	end;

	function tStateOpts.PhaseCallback(const aPhase: float): pMultiDelegate;
	var
		i: sint;
	begin
		for i := 0 to High(phases) do
			if Equals(phases[i].phase, aPhase) then
				exit(phases[i].callback);
		SetLength(phases, length(phases) + 1);
		with phases[High(phases)] do
		begin
			phase := aPhase;
			new(callback); callback^.Init;
			exit(callback);
		end;
	end;

	procedure _CallOnFire(const info: SingleDelegateInfo; param: pointer);
	var
		args: ^Doll.OnFireArgs absolute param;
	begin
		Doll.OnFireProc(info.proc)(args^.doll, args^.boneId, args^.tf^, info);
	end;

	procedure Doll._ProcessFire(const dt: float);
	const
		MaxPerFrame = 3;
	var
		i, j: sint;
		args: OnFireArgs;
		tf: Transform;
	begin
		_skeleton^.UseAnim(states[doll_Fire]^.anims[anim_Base].id, _GetStateAnimWeightCoef(doll_Fire) * 2.0, 1.0);

		for i := 0 to High(_fire_bones) do
			if (_fire_bones[i].handle.id >= 0) and not _fire_bones[i].callback^.Empty then
			begin
				_fire_bones[i].curTimer += dt;
				for j := 1 to MaxPerFrame do
					if _fire_bones[i].curTimer >= _fire_bones[i].curPeriod then
					begin
						tf := _skeleton^.bones[_fire_bones[i].handle.id].WorldTransform(_skeleton^) * _fire_bones[i].tf;
						args.doll := @self;
						args.boneId := _fire_bones[i].handle.id;
						args.tf := @tf;
						_fire_bones[i].callback^.Call(@_CallOnFire, @args);

						_fire_bones[i].curTimer -= _fire_bones[i].curPeriod;
						_fire_bones[i].GeneratePeriod;
					end else
						break;
				if _fire_bones[i].curTimer > _fire_bones[i].curPeriod then _fire_bones[i].curTimer := 0.0;
			end;
	end;

	procedure Doll._ProcessMelee(state: DollState);
	begin
		_skeleton^.UseAnim(states[state]^.anims[_meleeVariation].id, _GetStateAnimWeightCoef(state), 1.0);
		if _skeleton^.AnimCompleted(states[state]^.anims[_meleeVariation].id, 0.0) then
		begin
			_DisableState(state);
			exit;
		end;
	end;

	procedure Doll._OnUpdate(const dt: float);
	var
		state: DollState;
		stat_it: DollStat;
		i: sint;
	begin
	trace_call('Doll._OnUpdate');
		inherited _OnUpdate(dt);
		// if (length(_states) = 0) and (_curTransition < 0) then _EnableState(doll_Idle);
		if (not (doll_Death in _acStatesSet)) and (doll_CanDie in _flags) and Assigned(stats[doll_HP]) and (stats[doll_HP]^.Value = 0.0) then
			Die;

		for stat_it in DollStat do
			if Assigned(stats[stat_it]) then
				stats[stat_it]^.Process(dt);
		if Assigned(_confusionEffects) then _confusionEffects^.Process(stats[doll_Conf]^.Value, dt);

		if Assigned(_activeTransition) then
			if _skeleton^.AnimCompleted(_activeTransition^.handle.id, 0.2) then
			begin
				state := _activeTransition^.stateB;
				_activeTransition := nil;
				_EnableState(state, no);
			end else
				_skeleton^.UseAnim(_activeTransition^.handle.id, 1.0, 1.0);

		if not Assigned(_activeTransition) then
		begin
			i := 0;
			while i <= High(_acStates) do
			begin
				state := _acStates[i];
				case state of
					doll_Idle: _ProcessIdle(dt);
					doll_IdleFly: _ProcessIdleFly;
					doll_Walk, doll_Run: _ProcessWalk(state, dt);
					doll_Fly: _ProcessFly;
					doll_Rotate: _ProcessRotate(dt);
					doll_Fire: _ProcessFire(dt);
					doll_Melee: _ProcessMelee(state);
					else
						_skeleton^.UseAnim(states[state]^.anims[anim_Base].id, _GetStateAnimWeightCoef(state), 1.0);
				end;
				if (i > High(_acStates)) or (_acStates[i] <> state) then continue;
				_HandleStateProcess(state, dt);
				states[state]^.Update(@self);
				inc(i);
			end;
		end;
leave_call
	end;

	function Doll._SuitsTo(know: SceneKnowledge): boolean;
	begin
		case know of
			scene_Update: result := yes;
			else
				result := inherited _SuitsTo(know);
		end;
	end;

	procedure EnsureConfusion(vp: pVitalParameter; const newValue: float; const info: SingleDelegateInfo);
	var
		doll: pDoll absolute info.user;
	begin
		Assert(VitalParameter.OnChangeProc(@EnsureConfusion) = @EnsureConfusion);
		Assert(@vp = @vp);
		if (newValue = 0.0) xor not Assigned(doll^._confusionEffects) then
			if newValue = 0.0 then
			begin
				dispose(doll^._confusionEffects, Done);
				doll^._confusionEffects := nil;
			end else
				doll^._confusionEffects := new(pConfusionEffects, Init);
	end;

	procedure Doll._Initialize;
	const
		DefaultMoveMethods: tMoveMethodsInfo =
		(
			(maxVelocity: 1.0; toleranceK: 1.0; phaseVelocity: 1.0),
			(maxVelocity: 2.0; toleranceK: 1.3; phaseVelocity: 1.0),
			(maxVelocity: 3.0; toleranceK: 1.6; phaseVelocity: 1.0)
		);
	var
		state_it: DollState;
		stat_it: DollStat;
	begin
		self.Sleepless := yes;
		if not (doll_DisableRotationLock in _flags) then
		begin
			self.InfiniteRotIX := yes;
			self.InfiniteRotIZ := yes;
		end;
		self.ForceInertiaMoment1 := yes;

		for stat_it in DollStat do
			stats[stat_it] := nil;

		_acStates := nil;
		_acStatesSet := [];
		_possibleStates := [doll_Idle, doll_Rotate];
		if doll_CanFly   in _flags then _possibleStates += [doll_IdleFly, doll_Fly];
		if doll_CanWalk  in _flags then _possibleStates += [doll_Walk];
		if doll_CanRun   in _flags then _possibleStates += [doll_Run];
		if doll_CanDie   in _flags then _possibleStates += [doll_Death];
		if doll_CanFire  in _flags then _possibleStates += [doll_Fire];
		if doll_CanWait  in _flags then _possibleStates += [doll_Wait];
		if doll_CanMelee in _flags then _possibleStates += [doll_Melee];

		_moveParam := Vec3.Zero;

		_strafePenalty := 0.8;
		_backwardPenalty := 0.6;
		_moveMethods := DefaultMoveMethods;

		_stateTransitions.Initialize;
		_activeTransition := nil;
		_skeleton := nil;

		for state_it in DollState do
			if state_it in _possibleStates then
				states[state_it] := new(pStateOpts, Init(state_it))
			else
				states[state_it] := nil;
		onCanFlyCheck.Init;
		onLandCheck.Init;
		_checkLandTime := 0.0;
		_lastCheckLand := yes;

		_way := nil;
		_fire_bones := nil;
		_wayCalcTime := 0.0;
		_inventory := nil;
	end;

	constructor Doll.Init(const newFlags: DollFlags);
	begin
		inherited Init(nil, 1.0);
		_flags := newFlags;
		_Initialize;
	end;

	destructor Doll.Done;
	var
		state_it: DollState;
		stat_it: DollStat;
		i: sint;
	begin
		Release(_inventory);
		onLandCheck.Done;
		onCanFlyCheck.Done;
		for i := 0 to High(_fire_bones) do
			_fire_bones[i].Done;

		for state_it in DollState do
			if Assigned(states[state_it]) then
				dispose(states[state_it], Done);
		_stateTransitions.Finalize;

		if Assigned(_confusionEffects) then
			dispose(_confusionEffects, Done);
		for stat_it in DollStat do
			Release(stats[stat_it]);
		Release(_way);
		Release(_skeleton);
		inherited Done;
	end;

	function Doll.HeartPos: Vec3;
	begin
	trace_call('Doll.HeartPos');
		result := globalTransform * _heartShift;
	leave_call
	end;

	procedure Doll.SwitchStatOn(stat: DollStat; const max, regen: float; reversed: boolean);
	begin
		SetRef(stats[stat], new(pVitalParameter, Init(@self, max, regen, reversed)));

		if stat = doll_Conf then
			stats[stat]^.onChange.Add(@EnsureConfusion, @self);
	end;

	procedure Doll.ForceUpdate;
	var
		stat: DollStat;
	begin
		for stat in DollStat do
			if Assigned(stats[stat]) then
				stats[stat]^.ForceUpdate;
	end;

	procedure Doll.SetStateTransition(stateA, stateB: DollState; const animName: string);
	begin
		_stateTransitions.SetTransition(stateA, stateB, animName, _skeleton);
	end;

	procedure Doll.RotateBy(const delta: Quaternion);
	begin
		if not (doll_Rotate in _acStatesSet) then
		begin
			_EnableState(doll_Rotate);
			_rotateTarget := _AlignToGravity(WorldRot);
		end;
		_rotateMode := rotate_Offset;
		_rotateTarget := _AlignToGravity(_rotateTarget * delta);
		_rotateTolerance := 0.0;
		_rotateToleranceCont := 0.0;
	end;

	procedure Doll.RotateTo(const target: Quaternion; const tolerance: float = DefaultRotateTolerance;
		const toleranceContModifier: float = DefaultRotateToleranceContModifier);
	var
		vel: float;
	begin
		_rotateMode := rotate_Target;
		_rotateTarget := _AlignToGravity(target);
		_rotateTolerance := max(tolerance, MinRotateTolerance);
		_rotateToleranceCont := ApplyToleranceModifier_GTE(_rotateTolerance, toleranceContModifier);
		if _PrepareRotate(vel) then
			if not (doll_Rotate in _acStatesSet) then
				_EnableState(doll_Rotate);
	end;

	procedure Doll.MoveTo(const target: Vec3; const vel, angle: float; const tolerance, toleranceContModifier: float;
		method: DollMovementMethod);
	var
		desiredState: DollState;
		v: Vec3;
	begin
		if (doll_Death in _acStatesSet) then exit;
		desiredState := _GetState(method);

		_moveMode := {move_Straight}move_Way;
		_moveParam := target;
		_moveAngle := angle;
		_moveVelocity := vel;
		_moveTolerance := max(MinWalkTolerance, tolerance);
		_moveToleranceCont := ApplyToleranceModifier_GTE(_moveTolerance, toleranceContModifier);

		if _PrepareMove(method, WorldRot.Inversed, v) then
		begin
			if not (desiredState in _acStatesSet) then
				_EnableState(desiredState);
		end else
			Stop;

		RotateTo(Quaternion.Rotation(Vec3.PositiveZ, (target - HeartPos).Normalized) * Quaternion.Rotation(angle, Vec3.PositiveY));
	end;

	procedure Doll.MoveTo(const target: Vec3; const vel: float = 0.0; angle: DollMovementAngle = move_Forward;
	                       const tolerance: float = DefaultWalkTolerance;
	                       const toleranceContModifier: float = DefaultWalkToleranceContModifier;
	                       method: DollMovementMethod = move_Auto);
	begin
		MoveTo(target, vel, AngleValues[angle], tolerance, toleranceContModifier, method);
	end;

	procedure Doll.MoveBy(const wasd: Vec3; method: DollMovementMethod);
	var
		desiredState: DollState;
	begin
		desiredState := _GetState(method);
		if not (desiredState in _acStatesSet) then
			_EnableState(desiredState);

		if _moveMode <> move_Offset then
		begin
			_moveMode := move_Offset;
			_moveParam := wasd;
		end else
			_moveParam += wasd;
		_moveAngle := 0.0;
		_moveVelocity := 0.0;
	end;

	function Doll.FlyUp: boolean;
	begin
		result := (not Flies) and (CanFly(yes)) and _EnableState(doll_Fly);
	end;

	function Doll.Moves: boolean;
	begin
		result := MovingStates * _acStatesSet <> [];
	end;

	function Doll.Flies: boolean;
	begin
		result := [doll_IdleFly, doll_Fly] * _acStatesSet <> [];
	end;

	procedure Doll.Land;
	begin
		if Flies then
		begin
			_checkLandTime := mm.SceneTime;
			_lastCheckLand := yes;
			if doll_CanWalk in _flags then
				_EnableState(doll_Idle);
		end;
	end;

	procedure Doll.ContinuousFire(fire: boolean);
	begin
		if not (doll_CanFire in _flags) then exit;
		if fire then
		begin
			if not (doll_Fire in _acStatesSet) then _EnableState(doll_Fire);
		end else
		begin
			if (doll_Fire in _acStatesSet) then _DisableState(doll_Fire);
		end;
	end;

	procedure Doll.Melee;
	begin
		if doll_Melee in _acStatesSet then _DisableState(doll_Melee);
		_EnableState(doll_Melee);
	end;

	procedure Doll.Stop;
	var
		desiredState: DollState;
	begin
		if Flies then
			desiredState := doll_IdleFly
		else
			desiredState := doll_Idle;
		if not (desiredState in _acStatesSet) then _EnableState(desiredState);
	end;

	procedure Doll.Die;
	begin
		if not (doll_Death in _acStatesSet) then _EnableState(doll_Death);
	end;

	procedure Doll.EnableInventory(const size: UintVec2);
	begin
		Assert(not Assigned(_inventory));
		_inventory := MakeRef(new(pInventory, Init(size)));
		_inventory^.Owner := @self;
	end;

	function Doll.FireCallback(const bone: string; const tf: Transform; const periodDis: Distribution): pMultiDelegate;
	var
		bid, i, id: sint;
	begin
		Assert(doll_CanFire in _flags);
		if Assigned(_skeleton) then
		begin
			bid := _skeleton^.GetBoneID(bone);
			if bid < 0 then exit;
		end else
			exit;

		for i := 0 to High(_fire_bones) do
			if (_fire_bones[i].handle.id = bid) and (_fire_bones[i].tf = tf) and Distribution.Equals(_fire_bones[i].periodDis, periodDis) then
				exit(_fire_bones[i].callback);

		id := length(_fire_bones);
		SetLength(_fire_bones, id + 1);
		_fire_bones[id].Init(_skeleton, bone, tf, periodDis.Copy);
		result := _fire_bones[id].callback;
	end;

	function Doll._GetFlag(flag: DollFlag): boolean;
	begin
		result := flag in _flags;
	end;

	procedure Doll._SetFlag(flag: DollFlag; value: boolean);
	begin
		Assert(flag in [doll_UserControlled]);
		if value <> (flag in _flags) then
		begin
			if value then Include(_flags, flag) else Exclude(_flags, flag);
		end;
	end;

	procedure Doll._SetSkeleton(newSkel: pSkeletonNode);
	begin
		if _skeleton = newSkel then exit;
		if Assigned(_skeleton) then _skeleton^.Detach;
		SetRef(_skeleton, newSkel);
		Attach(_skeleton);
		_UpdateSkeleton(_skeleton);
	end;

	procedure Doll._UpdateSkeleton(skel: pSkeletonNode);
	var
		state: DollState;
		aid: DollAnimationID;
		i: sint;
	begin
		for state in DollState do
			if state in _possibleStates then
				for aid in DollAnimationID do
					if DollStateInfo[state].anims[aid] <> '' then
						states[state]^.anims[aid].Update(skel, id_Anim);

		for i := 0 to High(_stateTransitions.list) do
			_stateTransitions.list[i].handle.Update(skel, id_Anim);

		if doll_CanFire in _flags then
			for i := 0 to High(_fire_bones) do
				_fire_bones[i].handle.Update(skel, id_Bone);
	end;

	procedure tStateTransitionInfo.Initialize(newStateA, newStateB: DollState);
	begin
		stateA := newStateA;
		stateB := newStateB;
		handle.Initialize('');
	end;

	procedure tStateTransitionInfo.Finalize;
	begin
		handle.Finalize;
	end;

	procedure StateTransitions.Initialize;
	begin
		list := nil;
	end;

	procedure StateTransitions.Finalize;
	var
		i: sint;
	begin
		for i := 0 to High(list) do
			list[i].Finalize;
		list := nil;
	end;

	procedure StateTransitions.SetTransition(stateA, stateB: DollState; const animName: string; skel: pSkeletonNode);
	var
		t: pStateTransitionInfo;
	begin
		t := ForceTransition(stateA, stateB);
		t^.handle.Rename(animName, skel, id_Anim);
	end;

	function StateTransitions.FindTransition(stateA, stateB: DollState): pStateTransitionInfo;
	var
		i: sint;
	begin
		for i := 0 to High(list) do
			if (list[i].stateA = stateA) and (list[i].stateB = stateB) then
				exit(@list[i]);
		result := nil;
	end;

	function StateTransitions.ForceTransition(stateA, stateB: DollState): pStateTransitionInfo;
	begin
		result := FindTransition(stateA, stateB);
		if not Assigned(result) then
		begin
			SetLength(list, length(list) + 1);
			result := @list[High(list)];
			result^.Initialize(stateA, stateB);
		end;
	end;

	function Doll._uniform2walk(const uni: Vec3): Vec3;
	begin
		result := uni;
		if result.z < 0.0 then result.z := result.z / _backwardPenalty;
		result.x := result.x / _strafePenalty;
	end;

	function Doll._walk2uniform(const walk: Vec3): Vec3;
	begin
		result := walk;
		if result.z < 0.0 then result.z := result.z * _backwardPenalty;
		result.x := result.x * _strafePenalty;
	end;

	function Doll._AlignToGravity(const rot: Quaternion): Quaternion;
	var
		t: Vec3;
	begin
		if doll_DisableRotationLock in _flags then
			result := AlignToGravity(rot * Vec3.PositiveZ, rot * Vec3.PositiveY, Vec3.PositiveY) * rot
		else
		begin
			t := rot * Vec3.PositiveZ;
			t.y := 0.0;
			if t.IsZero then result := rot else result := Quaternion.Rotation(Vec3.PositiveZ, t.Normalized);
		end;
	end;

	function Doll._GetState(var method: DollMovementMethod): DollState;
	begin
		if method = move_Auto then
			if Flies then
			begin
					if doll_CanFly in _flags then method := move_Fly else
						if doll_CanRun in _flags then method := move_Run else
							if doll_CanWalk in _flags then method := move_Walk else
								Assert(no);
			end else
			if (doll_Run in _acStatesSet) then
			begin
				if doll_CanRun in _flags then method := move_Run else
					if doll_CanFly in _flags then method := move_Fly else
						if doll_CanWalk in _flags then method := move_Walk else
							Assert(no);
			end else
			begin
				if doll_CanWalk in _flags then method := move_Walk else
					if doll_CanFly in _flags then method := move_Fly else
						if doll_CanRun in _flags then method := move_Run else
							Assert(no);
			end;

		case method of
			move_Walk: result := doll_Walk;
			move_Run: result := doll_Run;
			move_Fly: result := doll_Fly;
		{$ifdef Debug} else Fatal('unknown movement method'); {$endif}
		end;
		if (result = doll_Fly) and (not CanFly(not Flies)) then
			if doll_CanWalk in _flags then result := doll_Walk else
				if doll_CanRun in _flags then result := doll_Run else
					Assert(no);
	end;

	function Doll._WayIsOutOfDate(const temporalTolerance, spatialTolerance: float): boolean;
	var
		timeDelta: float;
	begin
		timeDelta := mm.SceneTimeSince(_wayCalcTime);
		if Assigned(_way) then
			result :=
				(timeDelta > temporalTolerance) or
				GreaterThan(SqrDistance(_moveParam, _way^.Target), sqr(spatialTolerance * (1.0 - timeDelta / temporalTolerance)))
		else
			result := timeDelta > temporalTolerance;
	end;

	function Doll._PrepareMove(method: ActualDollMovementMethod; const irot: Quaternion; out v: Vec3): boolean;
	const
		MaxWayLen = 150.0;
		WayTolerance = 0.3;
	var
		target: Vec3;
		tolerance: float;
	begin
		case _moveMode of
			move_Straight, move_Way:
				begin
					if _acStatesSet * MovingStates <> [] then tolerance := _moveTolerance else tolerance := _moveToleranceCont;

					case _moveMode of
						move_Straight: target := _moveParam;
						move_Way:
							if Assigned(_way) then
							begin
								target := _way^.GetSegmentB(_waySeg);
								if method <> move_Fly then
									target.y := lerp(target.y, HeartPos.y, 0.75)
								else
									target.y := lerp(target.y, HeartPos.y, 0.2);
						// log('сегмент пути ' + ToString(_wayseg)+'/'+ToString(_way^.getnsegments) + ' (допуск: ' + ToString(tolerance) + ', позиция: ['+ToString(WorldPos)+'], цель: ['+ToString(target)+'])');
							end else
								target := _moveParam;
					end;

					tolerance := (tolerance + RelRadiusXZ) * _moveMethods[method].toleranceK;
					result := SqrDistance(HeartPos, target) > sqr(tolerance);
					if result and (_moveMode = move_Way) and (Assigned(_way)) then
						tolerance := WayTolerance + RelRadiusXZ;

					if result then
					begin
						if _moveMode = move_Way then
						begin
							if _WayIsOutOfDate(3.5, 2.0 * (_moveToleranceCont + RelRadiusXZ)) then
							begin
								SetRef(_way, pScene(Root)^.ways.FindWay(HeartPos, _moveParam, MaxWayLen, search_Default));
								if Assigned(_way) then
								begin
									// _way^.Shift(2.0 * _relRadiusXZ);
									_waySeg := 0;
								end else
									target := _moveParam;
								_wayCalcTime := mm.SceneTime;
							end;
						end;
						v := irot * (target - HeartPos);
						RotateTo(Quaternion.Rotation(Vec3.PositiveZ, (target - HeartPos).Normalized) * Quaternion.Rotation(_moveAngle, Vec3.PositiveY));
					end else
					begin
						if (_moveMode = move_Way) and Assigned(_way) and (_waySeg + 1 < _way^.GetNSegments) then
						begin
							inc(_waySeg);
							result := _PrepareMove(method, irot, v);
						end else
							result := no;
					end;
				end;
			move_Offset:
				begin
					result := NotZero(_moveParam.SqrLength);
					if result then v := _moveParam;
				end;
		end;
		// if _moveVelocity <> 0.0 then
		//   v := v.GetClamp(_moveVelocity);
	end;

	function Doll._PrepareRotate(out vel: float): boolean;
	const
		Params: array[DollRotateMode] of record
			velAngleK: float;
		end =
		(
			(velAngleK: 20.0),
			(velAngleK: 2.5)
		);
	var
		diff: float;
		tolerance: float;
	begin
		if doll_Rotate in _acStatesSet then tolerance := _rotateTolerance else tolerance := _rotateToleranceCont;
		diff := AngleUN(_rotateTarget, WorldRot);
		result := abs(diff) > tolerance;
		if result then
		begin
			vel := Params[_rotateMode].velAngleK;

			if (not (doll_UserControlled in _flags)) and (_moveVelocity <> 0.0) then
				vel *= _moveVelocity / _moveMethods[move_Fly].maxVelocity;
		end;
	end;

	procedure _CallOnCanFlyCheck(const info: SingleDelegateInfo; param: pointer);
	var
		args: ^Doll.OnCanFlyCheckArgs absolute param;
	begin
		args^.ret := Doll.OnCanFlyCheckProc(info.proc)(args^.doll, args^.up, info);
	end;

	function Doll.CanFly(up: boolean): boolean;
	var
		args: OnCanFlyCheckArgs;
	begin
		if doll_CanFly in _flags then
		begin
			if onCanFlyCheck.Empty then
				result := yes
			else
			begin
				args.doll := @self;
				args.up := up;
				onCanFlyCheck.Call(@_CallOnCanFlyCheck, @args);
				result := args.ret;
			end;
		end else
			result := no;
	end;

	procedure _CallOnLandCheck(const info: SingleDelegateInfo; param: pointer);
	var
		args: ^Doll.OnLandCheckArgs absolute param;
	begin
		args^.ret := Doll.OnLandCheckProc(info.proc)(args^.doll, info);
	end;

	function Doll.CheckLand: boolean;
	const
		TimeTolerance = 1.5;
	var
		args: OnLandCheckArgs;
	begin
		if not onLandCheck.Empty then
		begin
			if mm.SceneTimeSince(_checkLandTime) > TimeTolerance then
			begin
				args.doll := @self;
				onLandCheck.Call(@_CallOnLandCheck, @args);
				_lastCheckLand := args.ret;
				_checkLandTime := mm.SceneTime;
			end;
			result := _lastCheckLand;
		end else
			result := no;
	end;

	function ApplyToleranceModifier_GTE(const x: float; const modifier: float): float;
	begin
		if modifier >= 0.0 then
			result := x + modifier
		else
			result := x * (-modifier + 1.0);
	end;

	procedure Script_VitalParameter_Damage(var ss: ScriptState);
	begin
		pVitalParameter(ss.ToSelf)^.Damage(ss.ToFloat(2), ss.ToFloat(3, VitalParameter.DefaultEffectTime));
	end;

	procedure Script_VitalParameter_Heal(var ss: ScriptState);
	begin
		pVitalParameter(ss.ToSelf)^.Heal(ss.ToFloat(2), ss.ToFloat(3, VitalParameter.DefaultEffectTime));
	end;

	procedure Script_VitalParameter_ModifyRegen(var ss: ScriptState);
	begin
		Script_modifiable(ss, 2, pVitalParameter(ss.ToSelf)^.regen);
	end;

{$define fname := Script_VitalParameter_value} {$define otype := VitalParameter} {$define field := Value} {$define prop_float}
{$include script_prop.inc}

{$define fname := Script_VitalParameter_nvalue} {$define otype := VitalParameter} {$define field := NValue} {$define prop_float}
{$include script_prop.inc}

	procedure OnVitalParameterChange(vp: pVitalParameter; const newValue: float; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(VitalParameter.OnChangeProc(@OnVitalParameterChange) = @OnVitalParameterChange);
		if not sd^.GetFunction {$ifdef Debug}('OnVitalParameterChange'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(vp);
			PushFloat(newValue);
			Call(2, 0);
		end;
	end;

	procedure OnVitalParameterTrailBeginEnd(vp: pVitalParameter; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(VitalParameter.OnTrailBeginEndProc(@OnVitalParameterTrailBeginEnd) = @OnVitalParameterTrailBeginEnd);
		if not sd^.GetFunction {$ifdef Debug}('OnVitalParameterTrailBeginEnd'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(vp);
			Call(1, 0);
		end;
	end;

	procedure OnVitalParameterTrailProcess(vp: pVitalParameter; const trail: float; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(VitalParameter.OnTrailProcessProc(@OnVitalParameterTrailProcess) = @OnVitalParameterTrailProcess);
		if not sd^.GetFunction {$ifdef Debug}('[VitalParameter] OnTrailProcess'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(vp);
			PushFloat(trail);
			Call(2, 0);
		end;
	end;

	procedure Script_VitalParameter_onChange(var ss: ScriptState);
	var
		vp: pVitalParameter;
	begin
		vp := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(vp^.Owner, @vp^.onChange, @OnVitalParameterChange)));
	end;

	procedure Script_VitalParameter_onTrailBegin(var ss: ScriptState);
	var
		vp: pVitalParameter;
	begin
		vp := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(vp^.Owner, @vp^.onTrailBegin, @OnVitalParameterTrailBeginEnd)));
	end;

	procedure Script_VitalParameter_onTrailProcess(var ss: ScriptState);
	var
		vp: pVitalParameter;
	begin
		vp := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(vp^.Owner, @vp^.onTrailProcess, @OnVitalParameterTrailProcess)));
	end;

	procedure Script_VitalParameter_onTrailEnd(var ss: ScriptState);
	var
		vp: pVitalParameter;
	begin
		vp := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(vp^.Owner, @vp^.onTrailEnd, @OnVitalParameterTrailBeginEnd)));
	end;

	procedure DollStatePhaseCallback(doll: pDoll; state: DollState; const phase: float; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(doll^.OnStatePhaseProc(@DollStatePhaseCallback) = @DollStatePhaseCallback);
		Assert(@state = @state);
		if not sd^.GetFunction {$ifdef Debug}('DollStatePhaseCallback'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(doll);
			PushFloat(phase);
			Call(2, 0);
		end;
	end;

	procedure DollFireCallback(doll: pDoll; boneId: sint; const tf: Transform; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(doll^.OnFireProc(@DollFireCallback) = @DollFireCallback);
		if not sd^.GetFunction {$ifdef Debug}('DollFireCallback'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(doll);
			PushString(doll^.Skeleton^.Skeleton^.bones[boneId].name);
			PushTransform(tf);
			Call(3, 0);
		end;
	end;

	function DollCanFly(doll: pDoll; up: boolean; const info: SingleDelegateInfo): boolean; forward;
	function DollCheckLand(doll: pDoll; const info: SingleDelegateInfo): boolean; forward;

	function DollCanFly(doll: pDoll; up: boolean; const info: SingleDelegateInfo): boolean;
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(doll^.OnLandCheckProc(@DollCheckLand) = @DollCheckLand);
		if not sd^.GetFunction {$ifdef Debug}('[Doll] CanFly'){$endif} then exit(yes);
		with sd^.ss^ do
		begin
			PushObject(doll);
			PushBool(up);
			Call(2, 1);
			result := ToBool(-1);
			Pop;
		end;
	end;

	function DollCheckLand(doll: pDoll; const info: SingleDelegateInfo): boolean;
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(doll^.OnCanFlyCheckProc(@DollCanFly) = @DollCanFly);
		if not sd^.GetFunction {$ifdef Debug}('[Doll] CheckLand'){$endif} then exit(yes);
		with sd^.ss^ do
		begin
			PushObject(doll);
			Call(1, 1);
			result := ToBool(-1);
			Pop;
		end;
	end;

	procedure OnInitDoneDollState(doll: pDoll; state: DollState; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(doll^.OnStateInitDoneProc(@OnInitDoneDollState) = @OnInitDoneDollState);
		Assert(@state = @state);
		if not sd^.GetFunction {$ifdef Debug}('OnInitDoneDollState'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(doll);
			Call(1, 0);
		end;
	end;

	procedure OnProcessDollState(doll: pDoll; state: DollState; const dt: float; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(doll^.OnStateProcessProc(@OnProcessDollState) = @OnProcessDollState);
		Assert(@state = @state);
		if not sd^.GetFunction {$ifdef Debug}('OnProcessDollState'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(doll);
			PushFloat(dt);
			Call(2, 0);
		end;
	end;

	procedure Script_CreateDoll(var ss: ScriptState);
	var
		flags: DollFlags;
		sk: pSkeletonSource;
		pf: DollFlag;
		doll: pDoll;
		stat: DollStat;
		state_id, state2_id: DollState;
		dis: Distribution;
	begin
		flags := [];
		for pf in DollFlag do
			if ss.GetBoolField(1, Character.Doll.FlagIds[pf]) then
				Include(flags, pf);

		doll := new(pDoll, Init(flags));
		ss.PushObject(doll);

		if ss.GetTableS(1, 'skeleton') then
		begin
			sk := ResourcePool.Shared^.LoadRef(TypeOf(SkeletonSource), ss.ToStream(-1));
			doll^.Skeleton := new(pSkeletonNode, Init(sk));
			Release(sk);
			ss.Pop;
		end;
		if ss.GetTableS(1, 'fire_bones') then
		begin
			ss.PushNil;
			while ss.Next(-2) do
			begin
				if ss.GetTableS(-1, 'callback') then
				begin
					if ss.GetTableS(-2, 'periodDis') then
					begin
						dis := Script_distribution(ss, -1);
						ss.Pop;
					end else
						dis := Distribution.Constant(1.0);
					ss.SetDelegate(doll, doll^.FireCallback(ss.ToString(-3), ss.GetTransformField(-2, 'tf'), dis), @DollFireCallback, '');
					dis.Done;
				end else 
					ss.Throw('не задан fire_bones.callback');
				ss.Pop;
			end;
			ss.Pop;
		end;
		if ss.GetTableS(1, 'can_fly_proc') then
			ss.SetDelegate(doll, @doll^.onCanFlyCheck, @DollCanFly, '');
		if ss.GetTableS(1, 'check_land_proc') then
			ss.SetDelegate(doll, @doll^.onLandCheck, @DollCheckLand, '');
		if ss.GetTableS(1, 'max_fly_velocity') then
		begin
			doll^._moveMethods[move_Fly].maxVelocity := ss.ToFloat(-1);
			ss.Pop;
		end;

		if ss.GetTableS(1, 'states') then
		begin
			ss.PushNil;
			while ss.Next(-2) do
			begin
				state_id := DollState(FindStr(ss.ToString(-2), doll^.StateIds, ord(doll_Idle)));
				if ss.GetTableS(-1, 'onInit') then
					ss.SetDelegate(doll, @doll^.states[state_id]^.onInit, @OnInitDoneDollState, '');
				if ss.GetTableS(-1, 'onProcess') then
					ss.SetDelegate(doll, @doll^.states[state_id]^.onProcess, @OnProcessDollState, '');
				if ss.GetTableS(-1, 'onDone') then
					ss.SetDelegate(doll, @doll^.states[state_id]^.onDone, @OnInitDoneDollState, '');
				if ss.GetTableS(-1, 'transitions') then
				begin
					ss.PushNil;
					while ss.Next(-2) do
					begin
						state2_id := DollState(FindStr(ss.ToString(-2), doll^.StateIds, ord(doll_Idle)));
						doll^.SetStateTransition(state_id, state2_id, ss.ToString(-1));
						ss.Pop;
					end;
					ss.Pop;
				end;
				if ss.GetTableS(-1, 'phases') then
				begin
					ss.PushNil;
					while ss.Next(-2) do
					begin
						if ss.IsTable(-2) then // phases_table phase_proc
						begin
							ss.PushNil;
							while ss.Next(-3) do // phases_table phase_proc phases_table_key phase
							begin
								ss.PushCopy(-3); // phases_table phase_proc phases_table_key phase phase_proc
								ss.SetDelegate(doll, doll^.states[state_id]^.PhaseCallback(ss.ToFloat(-2)), @DollStatePhaseCallback, '');
								ss.Pop;
							end;
							ss.Pop;
						end else
							ss.SetDelegate(doll, doll^.states[state_id]^.PhaseCallback(ss.ToFloat(-2)), @DollStatePhaseCallback, '');
					end;
					ss.Pop;
				end;
				ss.Pop;
			end;
			ss.Pop;
		end;

		if ss.GetTableS(1, 'stats') then
		begin
			ss.PushNil;
			while ss.Next(-2) do
			begin
				stat := DollStat(FindStr(ss.ToString(-2), doll^.StatIds, ord(doll_Pois)));
				doll^.SwitchStatOn(stat, ss.GetFloatField(-1, 'max', 1.0), ss.GetFloatField(-1, 'regen'), ss.GetBoolField(-1, 'reverse'));
				ss.Pop;
			end;
			ss.Pop;
		end;

		if ss.GetTableS(1, 'inventory') then
		begin
			doll^.EnableInventory(UintTrunc(ss.GetVec2Field(-1, 'size')));
			ss.Pop;
		end;
	end;

	procedure Script_Doll_SwitchState(var ss: ScriptState);
	var
		doll: pDoll;
		state: DollState;
		on: boolean;
	begin
		doll := ss.ToSelf;
		state := DollState(FindStr(ss.ToString(2), doll^.StateIds, ord(doll_Idle)));
		on := (ss.Typ(3) = script_Nil) or ss.ToBool(3);
		if on <> (state in doll^._acStatesSet) then
			if on then
				doll^._EnableState(state)
			else
				doll^._DisableState(state);
	end;

	procedure Script_Doll_Move(var ss: ScriptState);
	var
		doll: pDoll;
		target, offset: Vec3;
		angle: float;
		method: DollMovementMethod;
	begin
		doll := ss.ToSelf;

		if ss.GetTableS(2, 'method') then
		begin
			method := DollMovementMethod(FindStr(ss.ToString(-1), doll^.MoveMethodIds, ord(move_Auto)));
			ss.Pop;
		end else
			method := move_Auto;

		if ss.GetTableS(2, 'target') then
		begin
			target := ss.ToVec3(-1);
			ss.Pop;
			if ss.GetTableS(2, 'angle') then
			begin
				case ss.Typ(-1) of
					script_String: angle := doll^.AngleValues[DollMovementAngle(FindStr(ss.ToString(-1), doll^.AngleIds, ord(move_Forward)))];
					script_Number: angle := ss.ToFloat(-1);
					else angle := 0.0;
				end;
				ss.Pop;
			end else
				angle := 0.0;
			doll^.MoveTo(target, ss.GetFloatField(-1, 'velocity'), angle,
				ss.GetFloatField(-1, 'tolerance', doll^.DefaultWalkTolerance) + ss.GetFloatField(-1, 'target_rxz'),
				ss.GetFloatField(-1, 'tolerance_modifier', doll^.DefaultWalkToleranceContModifier), method);
		end else
			if ss.GetTableS(2, 'offset') then
			begin
				offset := ss.ToVec3(-1);
				ss.Pop;
				doll^.MoveBy(offset, method);
			end else
				ss.Throw('кукла может либо двигаться в точку (target), либо сдвинуться по смещению (offset)');
	end;

	procedure Script_Doll_Rotate(var ss: ScriptState);
	var
		doll: pDoll;
		target, offset: Quaternion;
	begin
		doll := ss.ToSelf;

		if ss.GetTableS(2, 'target') then
		begin
			case ss.Typ(-1) of
				script_Number: target := Quaternion.Rotation(ss.ToFloat(-1), Vec3.PositiveY);
				script_Pointer: target := ss.ToQuaternion(-1);
				else ss.Throw('ожидается число или кватернион');
			end;
			ss.Pop;
			doll^.RotateTo(target, ss.GetFloatField(2, 'tolerance', doll^.DefaultRotateTolerance),
				ss.GetFloatField(2, 'tolerance_modifier', doll^.DefaultRotateToleranceContModifier));
		end else
			if ss.GetTableS(2, 'offset') then
			begin
				case ss.Typ(-1) of
					script_Number: offset := Quaternion.Rotation(ss.ToFloat(-1), Vec3.PositiveY);
					script_Pointer: offset := ss.ToQuaternion(-1);
					else ss.Throw('ожидается число или кватернион');
				end;
				ss.Pop;
				doll^.RotateBy(offset);
			end else
				ss.Throw('кукла может повернуться либо до конкретного угла (target), либо по смещению (offset)');
	end;

	procedure Script_Doll_ContinuousFire(var ss: ScriptState);
	begin
		pDoll(ss.ToSelf)^.ContinuousFire(ss.ToBool(2));
	end;

	procedure Script_Doll_FlyUp(var ss: ScriptState);
	begin
		ss.PushBool(pDoll(ss.ToSelf)^.FlyUp);
	end;

	procedure Script_Doll_Land(var ss: ScriptState);
	begin
		pDoll(ss.ToSelf)^.Land;
	end;

	procedure Script_Doll_Melee(var ss: ScriptState);
	begin
		pDoll(ss.ToSelf)^.Melee;
	end;

	procedure Script_Doll_Stop(var ss: ScriptState);
	var
		doll: pDoll;
	begin
		doll := ss.ToSelf;
		doll^.Stop;
	end;

	procedure Script_Doll_CanFly(var ss: ScriptState);
	begin
		ss.PushBool(pDoll(ss.ToSelf)^.CanFly(ss.ToBool(2)));
	end;

	procedure Script_Doll_ForceUpdate(var ss: ScriptState);
	begin
		pDoll(ss.ToSelf)^.ForceUpdate;
	end;

	procedure Script_Doll_BindIndicators(var ss: ScriptState);
	var
		doll: pDoll;
		group: pIndicatorGroup;
		id: sint;
	begin
		doll := ss.ToSelf;
		group := ss.ToObject(2, TypeOf(IndicatorGroup));
		ss.PushNil;
		while ss.Next(3) do
		begin
			id := FindStr(ss.ToString(-2), doll^.StatIds);
			if id >= 0 then
				if Assigned(doll^.stats[DollStat(id)]) then
					doll^.stats[DollStat(id)]^.Bind(group, ss.ToString(-1))
				else
					group^.Unbind(ss.ToString(-1))
			else
				ss.UnknownIdentifier(ss.ToString(-2));
		end;
	end;

{$define doll_prop := {$define otype := Doll} {$include script_prop.inc}}
{$define fname:=Script_Doll_skeleton} {$define field:=skeleton} {$define prop_object:=SkeletonNode} doll_prop
{$define fname:=Script_Doll_way} {$define field:=_way} {$define prop_object:=tWay} {$define readonly} doll_prop
{$define fname:=Script_Doll_hp} {$define field:=stats[doll_HP]} {$define prop_object:=VitalParameter} {$define readonly} doll_prop
{$define fname:=Script_Doll_mp} {$define field:=stats[doll_MP]} {$define prop_object:=VitalParameter} {$define readonly} doll_prop
{$define fname:=Script_Doll_conf} {$define field:=stats[doll_Conf]} {$define prop_object:=VitalParameter} {$define readonly} doll_prop
{$define fname:=Script_Doll_pois} {$define field:=stats[doll_Pois]} {$define prop_object:=VitalParameter} {$define readonly} doll_prop
{$define fname:=Script_Doll_heartPos} {$define field:=HeartPos} {$define prop_vec3} {$define readonly} doll_prop
{$define fname:=Script_Doll_heartShift} {$define field:=HeartShift} {$define prop_vec3} doll_prop
{$define fname:=Script_Doll_userControlled} {$define field:=UserControlled} {$define prop_bool} doll_prop
{$define fname:=Script_Doll_flies} {$define field:=Flies} {$define prop_bool} {$define readonly} doll_prop
{$define fname:=Script_Doll_moves} {$define field:=Moves} {$define prop_bool} {$define readonly} doll_prop
{$undef doll_prop}

	procedure Script_Doll_alive(var ss: ScriptState);
	begin
		ss.PushBool(not (doll_Death in pDoll(ss.ToSelf)^._acStatesSet));
	end;

	procedure Script_Doll_inventory(var ss: ScriptState);
	begin
		ss.PushObject(pDoll(ss.ToSelf)^.Inventory);
	end;

	procedure OpenScript(var script: ScriptState);
	const
		Stuff: array[0 .. 35] of ScriptStuffDesc =
		(
			(s: TypeDesc; p: TypeOf(VitalParameter)),
			(s: 'Damage:0'; p: @Script_VitalParameter_Damage),
			(s: 'Heal:0'; p: @Script_VitalParameter_Heal),
			(s: 'ModifyRegen:0'; p: @Script_VitalParameter_ModifyRegen),

			(s: 'value' + Writeable; p: @Script_VitalParameter_value),
			(s: 'nvalue' + Writeable; p: @Script_VitalParameter_nvalue),
			(s: 'onChange'; p: @Script_VitalParameter_onChange),
			(s: 'onTrailBegin'; p: @Script_VitalParameter_onTrailBegin),
			(s: 'onTrailProcess'; p: @Script_VitalParameter_onTrailProcess),
			(s: 'onTrailEnd'; p: @Script_VitalParameter_onTrailEnd),

			(s: TypeDesc; p: TypeOf(Doll)),
			(s: 'SwitchState:0'; p: @Script_Doll_SwitchState),
			(s: 'Move:0'; p: @Script_Doll_Move),
			(s: 'Rotate:0'; p: @Script_Doll_Rotate),
			(s: 'ContinuousFire:0'; p: @Script_Doll_ContinuousFire),
			(s: 'FlyUp:1'; p: @Script_Doll_FlyUp),
			(s: 'Land:0'; p: @Script_Doll_Land),
			(s: 'Melee:0'; p: @Script_Doll_Melee),
			(s: 'Stop:0'; p: @Script_Doll_Stop),
			(s: 'CanFly:1'; p: @Script_Doll_CanFly),
			(s: 'BindIndicators:0'; p: @Script_Doll_BindIndicators),
			(s: 'ForceUpdate:0'; p: @Script_Doll_ForceUpdate),

			(s: 'heartShift' + Writeable; p: @Script_Doll_heartShift),
			(s: 'heartPos'; p: @Script_Doll_heartPos),
			(s: 'userControlled' + Writeable; p: @Script_Doll_userControlled),
			(s: 'skeleton' + Writeable; p: @Script_Doll_skeleton),
			(s: 'way'; p: @Script_Doll_way),
			(s: 'hp'; p: @Script_Doll_hp),
			(s: 'mp'; p: @Script_Doll_mp),
			(s: 'conf'; p: @Script_Doll_conf),
			(s: 'pois'; p: @Script_Doll_pois),
			(s: 'flies'; p: @Script_Doll_flies),
			(s: 'moves'; p: @Script_Doll_moves),
			(s: 'alive'; p: @Script_Doll_alive),
			(s: 'inventory'; p: @Script_Doll_inventory),

			(s: FunctionsDesc + 'CreateDoll:1' + RequireEnv; p: @Script_CreateDoll)
		);
	begin
		script.AddStuff(Stuff);
	end;

{$ifdef use_serialization}
const
	VP_HAS_OWNER_BIT           = 1 shl 0;
	VP_REVERSED_BIT            = 1 shl 1;
	VP_CHANGED_BIT             = 1 shl 2;
	VP_HAS_TRAIL_BIT           = 1 shl 3;
	VP_MAXED_BIT               = 1 shl 4;
	VP_HAS_OVER_TIME_BIT       = 1 shl 5;
	VP_HAS_BINDINGS_BIT        = 1 shl 6;
	VP_HAS_ONCHANGE_BIT        = 1 shl 7;
	VP_HAS_ONTRAILBEGIN_BIT    = 1 shl 8;
	VP_HAS_ONTRAILPROCESS_BIT  = 1 shl 9;
	VP_HAS_ONTRAILEND_BIT      = 1 shl 10;
	VP_HAS_REGEN_BIT           = 1 shl 11;

	procedure SerializeVitalParameter(se: pSerializer; obj: pointer);
	var
		vp: pVitalParameter absolute obj;
		flags: uint;
		i: sint;
	begin
		with se^ do
		begin
			flags := 0;
			if Assigned(vp^._owner) and (vp^._owner <> pObject(vp)) then flags := flags or VP_HAS_OWNER_BIT;
			if vp^._reversed then flags := flags or VP_REVERSED_BIT;
			if vp^._changed then flags := flags or VP_CHANGED_BIT;
			if NotZero(vp^._trail) then flags := flags or VP_HAS_TRAIL_BIT;
			if Equals(vp^._value, vp^._max) then flags := flags or VP_MAXED_BIT;
			if length(vp^.overTime) > 0 then flags := flags or VP_HAS_OVER_TIME_BIT;
			if length(vp^.bindings) > 0 then flags := flags or VP_HAS_BINDINGS_BIT;
			if not vp^.onChange.Empty then flags := flags or VP_HAS_ONCHANGE_BIT;
			if not vp^.onTrailBegin.Empty then flags := flags or VP_HAS_ONTRAILBEGIN_BIT;
			if not vp^.onTrailProcess.Empty then flags := flags or VP_HAS_ONTRAILPROCESS_BIT;
			if not vp^.onTrailEnd.Empty then flags := flags or VP_HAS_ONTRAILEND_BIT;
			if not vp^.regen.Empty then flags := flags or VP_HAS_REGEN_BIT;
			Serialize_ui16(stream, flags);

			if (flags and VP_HAS_OWNER_BIT) <> 0 then SeObject(vp^._owner);
			Serialize_f32(stream, vp^._max);
			if (flags and VP_MAXED_BIT) = 0 then Serialize_fN16(stream, vp^._value, 0.0, vp^._max);
			if (flags and VP_HAS_TRAIL_BIT) <> 0 then Serialize_f16(stream, vp^._trail);
			if (flags and VP_HAS_OVER_TIME_BIT) <> 0 then
			begin
				Serialize_ui8(stream, length(vp^.overTime));
				for i := 0 to High(vp^.overTime) do
				begin
					Serialize_f32(stream, vp^.overTime[i].rest);
					Serialize_f32(stream, vp^.overTime[i].perSec);
				end;
			end;
			if (flags and VP_HAS_BINDINGS_BIT) <> 0 then
			begin
				Serialize_ui8(stream, length(vp^.bindings));
				for i := 0 to High(vp^.bindings) do
					SeObject(vp^.bindings[i]);
			end;
			if (flags and VP_HAS_ONCHANGE_BIT) <> 0 then SeObject(@vp^.onChange, ObjType_MultiDelegate);
			if (flags and VP_HAS_ONTRAILBEGIN_BIT) <> 0 then SeObject(@vp^.onTrailBegin, ObjType_MultiDelegate);
			if (flags and VP_HAS_ONTRAILPROCESS_BIT) <> 0 then SeObject(@vp^.onTrailProcess, ObjType_MultiDelegate);
			if (flags and VP_HAS_ONTRAILEND_BIT) <> 0 then SeObject(@vp^.onTrailEnd, ObjType_MultiDelegate);
			if (flags and VP_HAS_REGEN_BIT) <> 0 then SeObject(@vp^.regen, ObjType_ModifiableValue);
		end;
	end;

	procedure DeserializeVitalParameter(de: pDeserializer; obj: pointer);
	var
		vp: pVitalParameter absolute obj;
		flags: uint;
		i: sint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui16(stream);
			vp^._reversed := (flags and VP_REVERSED_BIT) <> 0;
			vp^._changed := (flags and VP_CHANGED_BIT) <> 0;

			if (flags and VP_HAS_OWNER_BIT) <> 0 then DeWeakR(vp^._owner) else vp^._owner := nil;
			vp^._max := Deserialize_f32(stream);
			if (flags and VP_MAXED_BIT) = 0 then vp^._value := Deserialize_fN16(stream, 0.0, vp^._max) else vp^._value := vp^._max;
			if (flags and VP_HAS_TRAIL_BIT) <> 0 then vp^._trail := Deserialize_f16(stream) else vp^._trail := 0.0;
			if (flags and VP_HAS_OVER_TIME_BIT) <> 0 then
			begin
				SetLength(vp^.overTime, Deserialize_ui8(stream));
				for i := 0 to High(vp^.overTime) do
				begin
					vp^.overTime[i].rest := Deserialize_f32(stream);
					vp^.overTime[i].perSec := Deserialize_f32(stream);
				end;
			end;
			if (flags and VP_HAS_BINDINGS_BIT) <> 0 then
			begin
				SetLength(vp^.bindings, Deserialize_ui8(stream));
				for i := 0 to High(vp^.bindings) do
					DeObjectA(vp^.bindings[i]);
			end;
			if (flags and VP_HAS_ONCHANGE_BIT) <> 0 then DeWeakAtR(vp^.onChange) else vp^.onChange.Init;
			if (flags and VP_HAS_ONTRAILBEGIN_BIT) <> 0 then DeWeakAtR(vp^.onTrailBegin) else vp^.onTrailBegin.Init;
			if (flags and VP_HAS_ONTRAILPROCESS_BIT) <> 0 then DeWeakAtR(vp^.onTrailProcess) else vp^.onTrailProcess.Init;
			if (flags and VP_HAS_ONTRAILEND_BIT) <> 0 then DeWeakAtR(vp^.onTrailEnd) else vp^.onTrailEnd.Init;
			if (flags and VP_HAS_REGEN_BIT) <> 0 then DeWeakAtR(vp^.regen) else vp^.regen.Init(0.0);
		end;
	end;

	procedure VitalParameterDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		vp: pVitalParameter absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: vp^.DeseInit;
			de_After: if not Assigned(vp^._owner) then vp^._owner := vp;
		end;
	end;

const
	DOLL_HAS_INVENTORY_BIT = 1 shl 0;

	procedure SerializeDoll(se: pSerializer; obj: pointer);
	var
		doll: pDoll absolute obj;
		bits, flags: uint;
		i: sint;
		transition: pStateTransitionInfo;
		mvMethod: ActualDollMovementMethod;
		fb: pFireBone;
		state_it: DollState;
		state: pStateOpts;
		stat: DollStat;
	begin
		with se^ do
		begin
			bits := 0;
			for i := ord(Low(DollFlag)) to ord(High(DollFlag)) do
				if DollFlag(i) in doll^._flags then
					bits := bits or uint(1 shl uint(i));
			Serialize_ui32(stream, bits);

			flags := 0;
			if Assigned(doll^.inventory) then flags := flags or DOLL_HAS_INVENTORY_BIT;
			Serialize_ui8(stream, flags);

			SeObject(doll^._skeleton);
			Serialize_ui8(stream, length(doll^._stateTransitions.list));
			for i := 0 to High(doll^._stateTransitions.list) do
			begin
				transition := @doll^._stateTransitions.list[i];
				Serialize_ui8(stream, ord(transition^.stateA));
				Serialize_ui8(stream, ord(transition^.stateB));
				Serialize_string(stream, transition^.handle.name);
			end;
			if Assigned(doll^._activeTransition) then
				Serialize_ui8(stream, 1 + (doll^._activeTransition - @doll^._stateTransitions.list[0]))
			else
				Serialize_ui8(stream, 0);
			Serialize_vec3f32(stream, doll^._heartShift);

			Serialize_ui8(stream, length(doll^._acStates));
			for i := 0 to High(doll^._acStates) do
				Serialize_ui8(stream, ord(doll^._acStates[i]));

			if doll^.Moves then
			begin
				Serialize_ui8(stream, ord(doll^._moveMode));
				Serialize_vec3f32(stream, doll^._moveParam);
				Serialize_f32(stream, doll^._moveTolerance);
				Serialize_f32(stream, doll^._moveToleranceCont);
				Serialize_f32(stream, doll^._moveAngle);
				Serialize_f32(stream, doll^._moveVelocity);
			end;

			if doll_Rotate in doll^._acStatesSet then
			begin
				Serialize_ui8(stream, ord(doll^._rotateMode));
				Serialize_IQuat8(stream, doll^._rotateTarget);
				Serialize_f32(stream, doll^._rotateTolerance);
				Serialize_f32(stream, doll^._rotateToleranceCont);
			end;

			Serialize_f32(stream, doll^._strafePenalty);
			Serialize_f32(stream, doll^._backwardPenalty);
			for mvMethod in ActualDollMovementMethod do
			begin
				Serialize_f32(stream, doll^._moveMethods[mvMethod].maxVelocity);
				Serialize_f32(stream, doll^._moveMethods[mvMethod].toleranceK);
				Serialize_f32(stream, doll^._moveMethods[mvMethod].phaseVelocity);
			end;

			Serialize_ui8(stream, length(doll^._fire_bones));
			for i := 0 to High(doll^._fire_bones) do
			begin
				fb := @doll^._fire_bones[i];
				Serialize_string(stream, fb^.handle.name);
				Serialize_tf32r8(stream, fb^.tf);
				SeObject(@fb^.periodDis, Distribution.TypeOf);
				Serialize_f32(stream, fb^.curPeriod);
				Serialize_f32(stream, fb^.curTimer);
				SeObject(fb^.callback, ObjType_MultiDelegate);
			end;
			if doll_CanMelee in doll^._flags then Serialize_ui8(stream, ord(doll^._meleeVariation));
			Serialize_ui8(stream, uint(Assigned(doll^._confusionEffects)));
			if Assigned(doll^._confusionEffects) then doll^._confusionEffects^.Serialize(stream);

			for state_it in DollState do
			begin
				state := doll^.states[state_it];
				Assert(Assigned(state) = (state_it in doll^._possibleStates));
				if Assigned(state) then
				begin
					SeObject(@state^.onInit, ObjType_MultiDelegate);
					SeObject(@state^.onProcess, ObjType_MultiDelegate);
					SeObject(@state^.onDone, ObjType_MultiDelegate);
					Serialize_f32(stream, state^.lastPhase);
					Serialize_ui8(stream, length(state^.phases));
					for i := 0 to High(state^.phases) do
					begin
						Serialize_f32(stream, state^.phases[i].phase);
						SeObject(state^.phases[i].callback, ObjType_MultiDelegate);
					end;
				end;
			end;
			SeObject(@doll^.onCanFlyCheck, ObjType_MultiDelegate);
			SeObject(@doll^.onLandCheck, ObjType_MultiDelegate);

			bits := 0;
			for i := ord(Low(DollStat)) to ord(High(DollStat)) do
				if Assigned(doll^.stats[DollStat(i)]) then
					bits := bits or uint(1 shl uint(i));
			Serialize_ui16(stream, bits);
			for stat in DollStat do
				if Assigned(doll^.stats[stat]) then SeObject(doll^.stats[stat]);

			if (flags and DOLL_HAS_INVENTORY_BIT) <> 0 then SeObject(doll^.inventory);
		end;
	end;

	procedure DeserializeDoll(de: pDeserializer; obj: pointer);
	var
		doll: pDoll absolute obj;
		bits, flags, t: uint;
		i: sint;
		transition: pStateTransitionInfo;
		mvMethod: ActualDollMovementMethod;
		fb: pFireBone;
		state_it: DollState;
		state: pStateOpts;
		stat: DollStat;
	begin
		with de^ do
		begin
			doll^._flags := [];
			bits := Deserialize_ui32(stream);
			for i := ord(Low(DollFlag)) to ord(High(DollFlag)) do
				if (bits and uint(1 shl uint(i))) <> 0 then
					Include(doll^._flags, DollFlag(i));
			doll^._Initialize;

			flags := Deserialize_ui8(stream);

			DeObjectR(doll^._skeleton);
			SetLength(doll^._stateTransitions.list, Deserialize_ui8(stream));
			for i := 0 to High(doll^._stateTransitions.list) do
			begin
				transition := @doll^._stateTransitions.list[i];
				transition^.stateA := DollState(Deserialize_ui8(stream));
				transition^.stateB := DollState(Deserialize_ui8(stream));
				transition^.handle.name := Deserialize_string(stream);
			end;
			t := Deserialize_ui8(stream);
			if t <> 0 then
				doll^._activeTransition := @doll^._stateTransitions.list[0] + (t - 1)
			else
				doll^._activeTransition := nil;
			doll^._heartShift := Deserialize_vec3f32(stream);

			SetLength(doll^._acStates, Deserialize_ui8(stream));
			doll^._acStatesSet := [];
			for i := 0 to High(doll^._acStates) do
			begin
				doll^._acStates[i] := DollState(Deserialize_ui8(stream));
				Include(doll^._acStatesSet, doll^._acStates[i]);
			end;

			if doll^.Moves then
			begin
				doll^._moveMode := DollMovementMode(Deserialize_ui8(stream));
				doll^._moveParam := Deserialize_vec3f32(stream);
				doll^._moveTolerance := Deserialize_f32(stream);
				doll^._moveToleranceCont := Deserialize_f32(stream);
				doll^._moveAngle := Deserialize_f32(stream);
				doll^._moveVelocity := Deserialize_f32(stream);
			end;

			if doll_Rotate in doll^._acStatesSet then
			begin
				doll^._rotateMode := DollRotateMode(Deserialize_ui8(stream));
				doll^._rotateTarget := Deserialize_IQuat8(stream);
				doll^._rotateTolerance := Deserialize_f32(stream);
				doll^._rotateToleranceCont := Deserialize_f32(stream);
			end;

			doll^._strafePenalty := Deserialize_f32(stream);
			doll^._backwardPenalty := Deserialize_f32(stream);
			for mvMethod in ActualDollMovementMethod do
			begin
				doll^._moveMethods[mvMethod].maxVelocity := Deserialize_f32(stream);
				doll^._moveMethods[mvMethod].toleranceK := Deserialize_f32(stream);
				doll^._moveMethods[mvMethod].phaseVelocity := Deserialize_f32(stream);
			end;

			SetLength(doll^._fire_bones, Deserialize_ui8(stream));
			for i := 0 to High(doll^._fire_bones) do
			begin
				fb := @doll^._fire_bones[i];
				fb^.handle.name := Deserialize_string(stream);
				fb^.tf := Deserialize_tf32r8(stream);
				DeWeakAtA(fb^.periodDis);
				fb^.curPeriod := Deserialize_f32(stream);
				fb^.curTimer := Deserialize_f32(stream);
				DeWeakA(fb^.callback);
			end;
			if doll_CanMelee in doll^._flags then doll^._meleeVariation := DollAnimationID(Deserialize_ui8(stream));
			if Deserialize_ui8(stream) <> 0 then
			begin
				doll^._confusionEffects := new(pConfusionEffects, Init);
				doll^._confusionEffects^.Deserialize(stream);
			end;

			for state_it in DollState do
			begin
				state := doll^.states[state_it];
				if Assigned(state) then
				begin
					DeWeakAtA(state^.onInit);
					DeWeakAtA(state^.onProcess);
					DeWeakAtA(state^.onDone);
					state^.lastPhase := Deserialize_f32(stream);
					SetLength(state^.phases, Deserialize_ui8(stream));
					for i := 0 to High(state^.phases) do
					begin
						state^.phases[i].phase := Deserialize_f32(stream);
						DeWeakA(state^.phases[i].callback);
					end;
				end;
			end;
			DeWeakAtR(doll^.onCanFlyCheck);
			DeWeakAtR(doll^.onLandCheck);

			bits := Deserialize_ui16(stream);
			for stat in DollStat do
				if (bits and uint(1 shl ord(stat))) <> 0 then
					DeObjectR(doll^.stats[stat]);

			if (flags and DOLL_HAS_INVENTORY_BIT) <> 0 then DeObjectR(doll^._inventory);
		end;
	end;

	procedure DollDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		doll: pDoll absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: doll^.DeseInit;
			de_After: doll^._UpdateSkeleton(doll^._skeleton);
		end;
	end;
{$endif}

	procedure Init;
	begin
	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Vital parameter', TypeOf(VitalParameter), nil, sizeof(VitalParameter), yes,
		               @SerializeVitalParameter, @DeserializeVitalParameter, nil, @VitalParameterDeSpecial)
		^.RegisterFuncs([@UnbindIndicator, @OnVitalParameterChange, @OnVitalParameterTrailBeginEnd, @OnVitalParameterTrailProcess, @EnsureConfusion])
		^.RegisterType('Doll', TypeOf(Doll), TypeOf(RigidBody), sizeof(Doll), yes,
		               @SerializeDoll, @DeserializeDoll, nil, @DollDeSpecial)
		^.RegisterFuncs([@DollStatePhaseCallback, @DollFireCallback, @DollCanFly, @DollCheckLand, @OnInitDoneDollState, @OnProcessDollState]);
	{$endif}
	end;

initialization
	&Unit('Character').Initialize(@Init);
end.
