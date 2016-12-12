unit Physics;

// События фантомов (Trigger Volume) вызываются с определённым порядком тел:
// в фантоме — (фантом, тело),
// в теле — (тело, фантом).

{$include opts.inc}

interface

uses
	ctypes, USystem, UMath {$ifdef Debug}, ULog {$endif}, Utils, Human, SceneGraph, UClasses, Streams, Algo,
	SpatialIndex, Script, NewtonHeaders, DynamicLoader;

const
	MIN_PHYS_DT = 1/300;
	MAX_PHYS_DT = 1/60;
	MAX_PHYS_ITERATIONS = round(0.5 + (1/10)/MAX_PHYS_DT);
	INFINITE_MASS = -9.5;
	MaxContacts = 36;

	// не обязаны совпадать с Newton'овскими
	DefaultStaticFriction = 0.9;
	DefaultKineticFriction = 0.5;

type
	pPhysWorld = ^PhysWorld;

	RigidPrimitiveKind =
	(
		rigid_Null,
		rigid_Box,
		rigid_Sphere,
		rigid_Capsule,
		rigid_Cone,
		rigid_Cylinder,
		rigid_Convex,
		rigid_Mesh,
		rigid_HeightMap,
		rigid_Compound
	);

	pRigidPrimitive = ^RigidPrimitive;
	RigidPrimitive = object(&Object)
	private const
		MASS_PROPERTIES_BIT = 1 shl 0;
		DefaultInertiaMoment: Vec3 = (data: (1.0, 1.0, 1.0));
		// http://en.wikipedia.org/wiki/List_of_moments_of_inertia
		// лол, не пригодилось, Newton всё сам считает =)
		PoolEps = 0.025;
	private type
		{$define classname:=tPool} {$define key_type:=pRigidPrimitive} {$define null_value:=nil} {$include hash.h.inc}
	private var
		_nInstances: sint; static;
		_pool: tPool; static;
		function _Hash(prim: pRigidPrimitive): Hash.Value; static;
		function _NewtonHash(coll: Newton.pCollision): Hash.Value; static;
		function _Equals(a, b: pRigidPrimitive): boolean; static;
		function _NewtonEquals(a, b: Newton.pCollision): boolean; static;
	private
		_kind: RigidPrimitiveKind;
		_newt: Newton.pCollision;
		_inertiaMoment, _massCenter: Vec3;
		_inPool: boolean;
		procedure _Deserialize(s: pStream);
		function _NewtonHumanInfo(coll: Newton.pCollision): string; static;
		procedure _MakeUnique;
	public
		constructor Init(newKind: RigidPrimitiveKind);
		constructor InitNull;
		constructor InitBox(sx, sy, sz: float; const newTransform: Transform; partOfCompound: boolean);
		constructor InitBox(const sizes: Vec3; const newTransform: Transform; partOfCompound: boolean);
		constructor InitSphere(rx, ry, rz: float; const newTransform: Transform; partOfCompound: boolean);
		constructor InitSphere(const rads: Vec3; const newTransform: Transform; partOfCompound: boolean);
		constructor InitSphere(r: float; const newTransform: Transform; partOfCompound: boolean);
		constructor InitCapsule(newRadius, newHeight: float; const newTransform: Transform; partOfCompound: boolean);
		constructor InitCone(const newRadius, newHeight: float; const newTransform: Transform; partOfCompound: boolean);
		constructor InitCylinder(newRadius, newHeight: float; const newTransform: Transform; partOfCompound: boolean);
		constructor InitCompound(const newPrims: array of pRigidPrimitive);
		constructor InitConvex(const newCloud: array of Vec3; const newTransform: Transform; partOfCompound: boolean);
		constructor InitMesh(const verts: array of Vec3; const inds: array of sint; const newTransform: Transform);
		constructor InitHeightMap(sizeX, sizeY: sint; const map: array of float; const matID: array of tMaterialID; const cellSize: float;
			const newTransform: Transform);
		destructor Done; virtual;
		function CalculateMassProperties: boolean;
		function Static: boolean;
		function HumanInfo: string;

		procedure Serialize(s: pStream);
		function Deserialize(s: pStream): pRigidPrimitive; static;
		function Merge(prim: pRigidPrimitive; allowFree: boolean): pRigidPrimitive; static;
		procedure Merge(var prim: pRigidPrimitive); static;
		function RayCast(rctransform: pTransform; const p1, p2: Vec3; x: pFloat; point, normal: pVec3): boolean;
		function RayCast(const p1, p2: Vec3): boolean;
		function ClosestPoint(world: pPhysWorld; const cptransform: Transform; const p: Vec3; out point, normal: Vec3): boolean;

		property Kind: RigidPrimitiveKind read _kind;
		property InertiaMoment: Vec3 read _inertiaMoment write _inertiaMoment;
		property MassCenter: Vec3 read _massCenter write _massCenter;
	end;

	pRigidBody = ^RigidBody;
	pWater = ^tWater;
	pRigidInteraction = ^tRigidInteraction;
	tRayCastPrefilter = function(body: pRigidBody; param: pointer): boolean;
	tRayCastCallback = function(body: pRigidBody; const x: float; const point, normal: Vec3; param: pointer): float;

	pContactInfo = ^ContactInfo;
	ContactInfo = object
		pos, norm, tang, binorm: Vec3;
		nSpeed, tSpeed, bSpeed: float;
		procedure Reset;
	end;

	tOnCollideProc = procedure(b0, b1: pRigidBody; const contact: ContactInfo; const info: SingleDelegateInfo);
	pOnCollideArgs = ^tOnCollideArgs;
	tOnCollideArgs = record
		b0, b1: pRigidBody;
		contact: pContactInfo;
	end;

	tOnPhantomProc = procedure(b0, b1: pRigidBody; const info: SingleDelegateInfo);
	pOnPhantomArgs = ^tOnPhantomArgs;
	tOnPhantomArgs = record
		b0, b1: pRigidBody;
	end;

	pPhysMaterialDB = ^PhysMaterialDB;
	PhysMaterialDB = object(&Object)
	public type
		pMaterial = ^Material;
		Material = record
			name: PoolString;
		end;

		pInteraction = ^Interaction;
		Interaction = object
			code: uint;
			collidable: boolean;
			staticFriction, kineticFriction: float;
			procedure Reset(newCode: uint; cp: pInteraction);
			function IdA(var db: PhysMaterialDB): sint;
			function IdB(var db: PhysMaterialDB): sint;
			function MaterialA(var db: PhysMaterialDB): pMaterial;
			function MaterialB(var db: PhysMaterialDB): pMaterial;
		end;

	private type
		{$define classname := Name2ID} {$define key_type := PoolString} {$define value_type := sint} {$define null_value:=-1} {$include hash.h.inc}
		{$define classname := InteractionSet} {$define key_type := Interaction} {$define inline_key := uint} {$include hash.h.inc}
	private const
		Signature = 'PhysMaterialDB 1.0... ';
		COLLIDABLE_BIT = 1 shl 0;
	private var
		_m: array of Material;
		_n2id: Name2ID;
		_ints: InteractionSet;
		procedure _Code2Mats(code: uint; out a, b: sint);
		function _Mats2Code(a, b: sint): uint;
		function _Deserialize(s: pStream): boolean;
	public
		constructor Init;
		constructor Init(s: pStream);
		destructor Done; virtual;
		function Serialize(s: pStream): boolean;

		function CreateMaterial(const name: PoolString): boolean;
		function RemoveMaterial(id: sint): boolean;
		function FindMaterial(const name: PoolString): pMaterial;
		function GetMaterial(id: sint): pMaterial;
		function GetMaterialID(const name: PoolString): sint;
		function Rename(id: sint; const newName: PoolString): boolean;
		function MaterialsCount: sint;
		function ValidateMaterial(id: sint): boolean;

		function FindInteraction(a, b: sint): pInteraction;
		function SuitableInteraction(a, b: sint): pInteraction;
		function ForceInteraction(a, b: sint): pInteraction;
		function RemoveInteraction(a, b: sint): boolean;
		function InteractionsCount: sint;
	end;

	tRigidInteraction = object(&Object)
	private
		_collidable: boolean;
	public
		constructor Init;
		destructor Done; virtual;
		property Collidable: boolean read _collidable write _collidable;
	end;

	pPhysJoint = ^tPhysJoint;

	RigidBodyFlag =
	(
		rigid_Sleepless,
		rigid_UnaffectedByGravity,
		rigid_InfiniteRotIX,
		rigid_InfiniteRotIY,
		rigid_InfiniteRotIZ,
		rigid_ForceInertiaMoment1
	);
	RigidBodyFlags = set of RigidBodyFlag;

	{$define classname:=tHash_Body2Interaction} {$define key_type:=pRigidBody} {$define value_type:=pRigidInteraction} {$define null_value:=nil}
	{$include hash.h.inc}

	RigidBody = object(SceneNode)
	public type
		tKind =
		(
			rigid_DynamicBody,
			rigid_Phantom
		);
	const
		KindPrefixCodes: array[tKind] of string = ('d', 'p');
		DefaultMaterial = 'default';
	private type
		pTmpNewtonOpts = ^tTmpNewtonOpts;
		tTmpNewtonOpts = object
			velocity: Vec3;
			omega: Vec3;
			constructor Init;
			destructor Done;
		end;
		pRigidSpecific = ^tRigidSpecific;
		tRigidSpecific = object
			matName: PoolString;
			mass: float;
			tmpNewt: pTmpNewtonOpts;
			joints: array of pPhysJoint;
			selfForce, selfTorque: Vec3;
			oldRotEul: Vec3;
			onCollide, onEnterPhantom, onExitPhantom: MultiDelegate;
			entangle: ModifiableValue;
			phantoms: array of pRigidBody;
			water: array of pWater;
			procedure Initialize(fully: boolean);
			procedure Finalize(var body: RigidBody);
			function EnsureTmpNewt: pTmpNewtonOpts;
			procedure UpdateMaterial(var body: RigidBody; world: pPhysWorld);
			procedure CallOnCollide(var body: RigidBody; b1: pRigidBody; const contact: ContactInfo);
			function EntangleCoef: float;
			procedure AddPhantom(ph: pRigidBody);
			procedure RemovePhantom(ph: pRigidBody);
		end;
		PhantomInnerRec = record
			ensured: boolean;
			body: pRigidBody;
		end;
	{$define classname := InnerList} {$define item_type := PhantomInnerRec} {$include vector.h.inc}
		pPhantomSpecific = ^tPhantomSpecific;
		tPhantomSpecific = object
			onEnter, onExit: MultiDelegate;
			inner: InnerList;
			procedure Initialize(fully: boolean);
			procedure Finalize(var body: RigidBody);
			procedure CallOnPhantom(what: pMultiDelegate; b0, b1: pRigidBody); static;
			procedure InvalidateInners;
			procedure ConfirmInner(var body: RigidBody; world: pPhysWorld; b2: pRigidBody);
			procedure RemoveUnconfirmedInners(var body: RigidBody; world: pPhysWorld);
			procedure ForceInnersToQuit(var body: RigidBody; emergency: boolean = no);
			procedure Pump(var body: RigidBody; b2: pRigidBody);
		end;
		tKindSpecific = record
		case tKind of
			rigid_DynamicBody: (asRigid: pRigidSpecific);
			rigid_Phantom: (asPhantom: pPhantomSpecific);
		end;
	private var
		_world: pPhysWorld;
		worldId1: uint;
		_kind: tKind;
		_newt: Newton.pBody;
		_primitive: pRigidPrimitive;
		_interaction: tHash_Body2Interaction;
		_specific: tKindSpecific;
		_flags: RigidBodyFlags;
		procedure _CommonInit(newKind: tKind; newPrim: pRigidPrimitive);
	{$ifdef Debug}
		function _GetRigidSpecific: pRigidSpecific;
		function _GetPhantomSpecific: pPhantomSpecific;
	{$endif}
		procedure _SetWorld(theWorld: pPhysWorld);
		procedure _CreateNewt;
		procedure _DestroyNewt;
		procedure _PreUpdate;
		procedure _PostUpdate;
		procedure _SetPrimitive(newPrim: pRigidPrimitive);
		function _GetFlag(flag: RigidBodyFlag): boolean;
		procedure _SetFlag(flag: RigidBodyFlag; newValue: boolean);
	private
		// rigid-only
		function _GetMaterial: PoolString;
		procedure _SetMaterial(const newMat: PoolString);
		procedure _UpdateSpecials(const targetFlags: RigidBodyFlags);
		procedure _AddJoint(j: pPhysJoint);
		procedure _RemoveJoint(j: pPhysJoint);
		function _GetVelocity: Vec3;
		procedure _SetVelocity(const vel: Vec3);
		function _GetOmega: Vec3;
		procedure _SetOmega(const om: Vec3);
		function _GetMass: float;
		procedure _SetMass(const newMass: float);
		procedure _UpdateNewtonMass;
		procedure _UpdateDamping;
		function _GetSelfForce: Vec3;
		procedure _SetSelfForce(const newForce: Vec3);
		function _GetSelfTorque: Vec3;
		procedure _SetSelfTorque(const newTorque: Vec3);
	protected
		procedure _AfterAttach; virtual;
		procedure _BeforeDetach; virtual;
		procedure _OnApplyTransform; virtual;
	public
		constructor Init(newPrim: pRigidPrimitive; const newMass: float);
		constructor InitPhantom(newPrim: pRigidPrimitive);
		destructor Done; virtual;
		function Interaction(b2: pRigidBody): pRigidInteraction;
		function ClosestPoint(const p: Vec3; out point, normal: Vec3): boolean;
		procedure PumpPhantoms;

		property Primitive: pRigidPrimitive read _primitive write _SetPrimitive;
		property AsRigid: pRigidSpecific read {$ifdef Debug} _GetRigidSpecific {$else} _specific.asRigid {$endif};
		property AsPhantom: pPhantomSpecific read {$ifdef Debug} _GetPhantomSpecific {$else} _specific.asPhantom {$endif};

		property Kind: tKind read _kind;
		property Material: PoolString read _GetMaterial write _SetMaterial;
		property Mass: float read _GetMass write _SetMass;
		property Velocity: Vec3 read _GetVelocity write _SetVelocity;
		property Omega: Vec3 read _GetOmega write _SetOmega;
		property SelfForce: Vec3 read _GetSelfForce write _SetSelfForce;
		property SelfTorque: Vec3 read _GetSelfTorque write _SetSelfTorque;

		property InfiniteRotIX: boolean index rigid_InfiniteRotIX read _GetFlag write _SetFlag;
		property InfiniteRotIY: boolean index rigid_InfiniteRotIY read _GetFlag write _SetFlag;
		property InfiniteRotIZ: boolean index rigid_InfiniteRotIZ read _GetFlag write _SetFlag;
		property Sleepless: boolean index rigid_Sleepless read _GetFlag write _SetFlag;
		property UnaffectedByGravity: boolean index rigid_UnaffectedByGravity read _GetFlag write _SetFlag;
		property ForceInertiaMoment1: boolean index rigid_ForceInertiaMoment1 read _GetFlag write _SetFlag;
	end;

	tPhysJointKind =
	(
		joint_Generic,
		joint_Ball
	);

	tPhysJoint = object(&Object)
	private
		_world: pPhysWorld;
		_newt: Newton.pJoint;
		_kind: tPhysJointKind;
		_parent, _child: pRigidBody;
		_pivot: Vec3;
		procedure _CreateNewt_impl; virtual; abstract;
		procedure _CreateNewt;
		procedure _DestroyNewt;
	public
		constructor Init(newKind: tPhysJointKind; newParent, newChild: pRigidBody; const newPivot: Vec3);
		destructor Done; virtual;
		procedure Break;

		property Kind: tPhysJointKind read _kind;
		property Parent: pRigidBody read _parent;
		property Child: pRigidBody read _child;
		property Pivot: Vec3 read _pivot;
	end;

	pPhysBallJoint = ^tPhysBallJoint;
	tPhysBallJoint = object(tPhysJoint)
	private
		_axis: Vec3;
		_maxCone, _maxTwist: float;
		procedure _CreateNewt_impl; virtual;
		procedure _UpdateNewt;
		procedure _SetAxis(const newAxis: Vec3);
		procedure _SetMaxCone(newMaxCone: float);
		procedure _SetMaxTwist(newMaxTwist: float);
	public
		constructor Init(newParent, newChild: pRigidBody; const newPivot: Vec3);
		destructor Done; virtual;

		property Axis: Vec3 read _axis write _SetAxis;
		property MaxCone: float read _maxCone write _SetMaxCone;
		property MaxTwist: float read _maxTwist write _SetMaxTwist;
	end;

	tCallbackKind = (callback_Collide, callback_Phantom);
	tCallbackRec = record
		proc: pMultiDelegate;
		b0, b1: pRigidBody;
	case kind: tCallbackKind of
		callback_Collide: (contact: ContactInfo; interaction: pObject);
	end;

	tWater = object(RigidBody)
	private
		_planeNormal: Vec3;
		_density, _linearViscosity, _angularViscosity: float;
	protected
		function _SuitsTo(know: SceneKnowledge): boolean; virtual;
	public
		constructor Init(newPrim: pRigidPrimitive);
		destructor Done; virtual;
		function Contains(const pt: Vec3; const radius: float): boolean;
		function Plane: UMath.Plane;

		property PlaneNormal: Vec3 read _planeNormal write _planeNormal;
		property Density: float read _density write _density;
		property LinearViscosity: float read _linearViscosity write _linearViscosity;
		property AngularViscosity: float read _angularViscosity write _angularViscosity;
	end;

	CallbacksQueue = object
	private type
		{$define classname:=tQueue} {$define item_type:=tCallbackRec}
		{$include queue.h.inc}
	private var
		qu: tQueue;
	public
		procedure Init;
		procedure Done;
		procedure AddCollide(proc: pMultiDelegate; b0, b1: pRigidBody; const contact: ContactInfo);
		procedure AddPhantom(proc: pMultiDelegate; b0, b1: pRigidBody);
		procedure ProcessAll;
	end;

	RigidBodyProc = procedure(body: pRigidBody; param: pointer);

	{$define classname:=BodiesList} {$define item_type:=pRigidBody} {$include vector.h.inc}

	{$define classname:=tHash_Code2OnCollide} {$define key_type:=uint} {$define value_type:=pMultiDelegate} {$define null_value:=nil}
	{$include hash.h.inc}

	PhysWorld = object(&Object)
	private
		_newt: Newton.pWorld;
		_materialDb: pPhysMaterialDB;
		_mid2newt: array of cint;
		_int2onCollide: tHash_Code2OnCollide;
	{$ifdef Debug}
		_nQueries: sint;
		_queriesTimeSum, _lastQueryReport: Ticks;
	{$endif}
		_callbacksQueue: CallbacksQueue;
		_prevTime: Ticks;
		_accDt: hp_float;
		bodies: BodiesList;
		_envResistance: float;
		procedure AddBody(b: pRigidBody);
		procedure RemoveBody(b: pRigidBody);
		procedure _SetEnvResistance(newResistance: float);
		procedure _SetMaterialDB(newDb: pPhysMaterialDB);
		procedure _PreUpdate;
		procedure _PostUpdate;
	public
		gravityEnabled: boolean;
		gravity: Vec3;
		constructor Init;
		destructor Done; virtual;
		procedure Update;
		procedure Resize(const aabb: AABB);
		procedure Query(const ibnd: Bounding; proc: RigidBodyProc; param: pointer);
		procedure RayCast(const p1, p2: Vec3; prefilter: tRayCastPrefilter; proc: tRayCastCallback; param: pointer);
		property EnvResistance: float read _envResistance write _SetEnvResistance;

		property MaterialDB: pPhysMaterialDB read _materialDB write _SetMaterialDB;
		function OnMaterialCollide(const a, b: string): pMultiDelegate;
	end;

	function PhysMemoryEaten: size_t;

const
	RigidPrimitiveKindIds: array[RigidPrimitiveKind] of string =
	(
		'null', 'box', 'sphere', 'capsule', 'cone', 'cylinder', 'convex', 'mesh', 'heightmap', 'compound'
	);

var
	Config: record
		idclip: boolean;
		solverModel: sint;
		frictionModel: sint;
		fpext: sint;
		nThreads: sint
	end =
	(
		idclip: no;
		solverModel: 1;
		frictionModel: 1;
		fpext: 0;
		nThreads: 0
	);

	procedure OpenScript(var script: ScriptState);
	function Script_create_rigid_prim_ref(var ss: ScriptState; idx: sint; partOfCompound: boolean = no): pRigidPrimitive;

implementation

uses
	Scene, MMSystem, Script_EngineAPI
{$ifdef use_serialization}, Serialization {$endif}
{$ifdef Profile}, Profile{$endif};

{$if sizeof(float) > sizeof(Newton.Float)}
	{$define RenormalizeVectors}
{$endif}

	{$define classname:=RigidPrimitive.tPool} {$define hash_func:=RigidPrimitive._Hash} {$define inline_eq := RigidPrimitive._Equals(_1, _2)}
	{$include hash.pp.inc}

	{$define classname:=BodiesList} {$include vector.pp.inc}
	{$define classname := RigidBody.InnerList} {$include vector.pp.inc}
	{$define classname:=CallbacksQueue.tQueue} {$include queue.pp.inc}

	{$define classname:=tHash_Body2Interaction} {$define hash_func:=Hash.OfPointer} {$define finalize_value := Release(_1)}
	{$include hash.pp.inc}

	{$define classname := PhysMaterialDB.Name2ID} {$define inline_hash := _1.Hash} {$include hash.pp.inc}

	{$define classname:=PhysMaterialDB.InteractionSet} {$define get_key:=_1.code} {$define hash_func:=Hash.OfUint} {$define inline_eq := _1.code = _2}
	{$include hash.pp.inc}

	{$define classname:=tHash_Code2OnCollide} {$define hash_func:=Hash.OfUint} {$define finalize_value := _1^.Done; dispose(_1);}
	{$include hash.pp.inc}

const
	NearInfinity = Newton.Float(1.0e+5);

type
	RigidPrimitiveKindFlag = (rpk_Static, rpk_SaveMassProps, rpk_DontPool);
	RigidPrimitiveKindFlags = set of RigidPrimitiveKindFlag;

const
	PrimitiveKindInfo: array[RigidPrimitiveKind] of record
		flags: RigidPrimitiveKindFlags;
	end =
	(
		(flags: [rpk_Static]), // rigid_Null
		(flags: []),           // rigid_Box,
		(flags: []),           // rigid_Sphere,
		(flags: []),           // rigid_Capsule,
		(flags: []),           // rigid_Cone,
		(flags: []),           // rigid_Cylinder,
		(flags: [rpk_DontPool, rpk_SaveMassProps]), // rigid_Convex,
		(flags: [rpk_DontPool, rpk_Static]),        // rigid_Mesh,
		(flags: [rpk_DontPool, rpk_Static]),        // rigid_HeightMap,
		(flags: [rpk_DontPool, rpk_SaveMassProps])  // rigid_Compound
	);

var
	TempWorld: Newton.pWorld = nil;

	function _NewtonAlloc(sizeInBytes: cint): pointer; cdecl;
	begin
		result := GetMem(sizeInBytes);
		fillchar(result^, sizeInBytes, 0);
	end;

	procedure _NewtonFree(ptr: pointer; sizeInBytes: cint); cdecl;
	begin
		Assert(@sizeInBytes = @sizeInBytes);
		FreeMem(ptr);
	end;

	procedure AfterLoad;
	begin
		Newton.SetMemorySystem(@_NewtonAlloc, @_NewtonFree);
		TempWorld := Newton.WorldCreate();
	end;

	procedure BeforeUnload;
	begin
		if Assigned(TempWorld) then Newton.WorldDestroy(TempWorld);
		TempWorld := nil;
	end;

	function NewtonGetAABB(body: Newton.pBody): AABB;
	var
		p1, p2: Newton.Vec3;
	begin
		Newton.BodyGetAABB(body, p1, p2);
		result := AABB.Make(Newton.From(p1), Newton.From(p2));
	end;

	function _NewtonPreCollide(material: Newton.pMaterial; body0, body1: Newton.pBody; threadIndex: cint): cint; cdecl;
	var
		b0, b1: pRigidBody;
	{$ifdef Debug} mInt: PhysMaterialDB.pInteraction; {$endif}
		int: pRigidInteraction;
	begin
	trace_call('NewtonPreCollide');
		Assert(@threadIndex = @threadIndex);
		if Config.idclip then exit(0);
	{$ifdef Debug}
		mInt := Newton.MaterialGetMaterialPairUserData(material);
		Assert(Assigned(mInt) and mInt^.collidable);
	{$else}
		Assert(@material = @material);
	{$endif}

		b0 := Newton.BodyGetUserData(body0);
		b1 := Newton.BodyGetUserData(body1);
		Assert(Assigned(b0) and Assigned(b1));

		int := b0^._interaction.Find(b1);
		if Assigned(int) then result := uint(int^.Collidable) else result := 1;
	leave_call
	end;

	procedure _NewtonCollide(contactJ: Newton.pJoint; timestep: Newton.Float; threadIndex: cint); cdecl;
	var
		nContacts: sint;
		contacts: array[0 .. MaxContacts] of record // без -1: терминальный NULL
			contact: pointer;
			material: pointer;
		end;
		world: pPhysWorld;
		b0, b1: pRigidBody;
		mInt: PhysMaterialDB.pInteraction;
		mOnCollide: pMultiDelegate;
		info: ContactInfo;
		nv3, nv3_2: Newton.Vec3;
		i: sint;
		hasMaterialCallback, hasBody0Callback, hasBody1Callback: boolean;
	begin
	trace_call('NewtonCollide');
		Assert(@timestep = @timestep);
		Assert(@threadIndex = @threadIndex);
		b0 := Newton.BodyGetUserData(Newton.JointGetBody0(contactJ));
		b1 := Newton.BodyGetUserData(Newton.JointGetBody1(contactJ));
		if (b0^._kind <> rigid_DynamicBody) and (b1^._kind <> rigid_DynamicBody) then
			exit
		else
		begin
			if b0^._kind = rigid_DynamicBody then
				Swap(pointer(b0), pointer(b1));
			// столкновения каких бы то ни было фантомов со статическими телами не обрабатываются (хотя это может измениться)
			if (b0^._kind = rigid_Phantom) and (b1^.asRigid^.mass = INFINITE_MASS) then exit;
		end;

		world := b0^._world;

		Newton.WorldCriticalSectionLock(world^._newt);
		nContacts := 0;
		contacts[0].contact := Newton.ContactJointGetFirstContact(contactJ);
		while Assigned(contacts[nContacts].contact) do
		begin
			if nContacts = MaxContacts then break;
			inc(nContacts);
			contacts[nContacts].contact := Newton.ContactJointGetNextContact(contactJ, contacts[nContacts - 1].contact);
		end;
		i := 0;
		while i < nContacts do
		begin
			contacts[i].material := Newton.ContactGetMaterial(contacts[i].contact);
			if Newton.MaterialGetContactFaceAttribute(contacts[i].material) = MaterialID_Transparent then
			begin
				Newton.ContactJointRemoveContact(contactJ, contacts[i].contact);
				contacts[i] := contacts[nContacts - 1];
				dec(nContacts);
				continue;
			end;
			inc(i);
		end;
		Newton.WorldCriticalSectionUnlock(world^._newt);
		if nContacts = 0 then exit;
	{$ifdef Debug} stat.Note(max_rigid_contacts, nContacts); {$endif}

		Newton.WorldCriticalSectionLock(world^._newt); // TODO: нужно ли лочить что-то, кроме очереди и RemoveContact?
		case b0^._kind of
			rigid_DynamicBody:
				begin
					hasBody0Callback := not b0^.asRigid^.onCollide.Empty;
					hasBody1Callback := not b1^.asRigid^.onCollide.Empty;
					for i := 0 to nContacts - 1 do
					begin
						mInt := Newton.MaterialGetMaterialPairUserData(contacts[i].material);
						mOnCollide := world^._int2onCollide.Find(mInt^.code);
						hasMaterialCallback := Assigned(mOnCollide) and not mOnCollide^.Empty;
						if hasMaterialCallback or hasBody0Callback or hasBody1Callback then
						begin
							Newton.MaterialGetContactPositionAndNormal(contacts[i].material, b0^._newt, nv3, nv3_2);
							info.pos := Newton.From(nv3);
							info.norm := Newton.From(nv3_2) {$ifdef RenormalizeVectors}.MaybeNormalized{$endif};
							Newton.MaterialGetContactTangentDirections(contacts[i].material, b0^._newt, nv3, nv3_2);
							info.tang := Newton.From(nv3) {$ifdef RenormalizeVectors}.MaybeNormalized{$endif};
							info.binorm := Newton.From(nv3_2) {$ifdef RenormalizeVectors}.MaybeNormalized{$endif};
							info.nSpeed := Newton.MaterialGetContactNormalSpeed(contacts[i].material);
							info.tSpeed := Newton.MaterialGetContactTangentSpeed(contacts[i].material, 0);
							info.bSpeed := Newton.MaterialGetContactTangentSpeed(contacts[i].material, 1);
							if hasMaterialCallback then
								world^._callbacksQueue.AddCollide(mOnCollide, b0, b1, info);
							if hasBody0Callback then
								world^._callbacksQueue.AddCollide(@b0^.asRigid^.onCollide, b0, b1, info);
							if hasBody1Callback then
								world^._callbacksQueue.AddCollide(@b1^.asRigid^.onCollide, b1, b0, info);
						end;
					end;
				end;
			rigid_Phantom:
				begin
					// столкновения (тело, объём) не обрабатываются, только (объём, тело)
					b0^.asPhantom^.ConfirmInner(b0^, world, b1);
					for i := 0 to nContacts - 1 do
						Newton.ContactJointRemoveContact(contactJ, contacts[i].contact);
				end;
		end;
		Newton.WorldCriticalSectionUnlock(world^._newt);
	leave_call
	end;

	procedure ContactInfo.Reset;
	begin
		self.pos := Vec3.Zero;
		self.norm := Vec3.Zero;
		self.tang := Vec3.Zero;
		self.binorm := Vec3.Zero;
		self.nSpeed := 0.0;
		self.tSpeed := 0.0;
		self.bSpeed := 0.0;
	end;

	procedure PhysMaterialDB.Interaction.Reset(newCode: uint; cp: pInteraction);
	begin
		code := newCode;
		if Assigned(cp) then
		begin
			collidable      := cp^.collidable;
			staticFriction  := cp^.staticFriction;
			kineticFriction := cp^.kineticFriction;
		end else
		begin
			collidable      := yes;
			staticFriction  := DefaultStaticFriction;
			kineticFriction := DefaultKineticFriction;
		end;
	end;

	function PhysMaterialDB.Interaction.IdA(var db: PhysMaterialDB): sint;
	var
		a, b: sint;
	begin
		db._Code2Mats(code, a, b);
		result := a;
	end;

	function PhysMaterialDB.Interaction.IdB(var db: PhysMaterialDB): sint;
	var
		a, b: sint;
	begin
		db._Code2Mats(code, a, b);
		result := b;
	end;

	function PhysMaterialDB.Interaction.MaterialA(var db: PhysMaterialDB): pMaterial;
	begin
		result := db.GetMaterial(IdA(db));
	end;

	function PhysMaterialDB.Interaction.MaterialB(var db: PhysMaterialDB): pMaterial;
	begin
		result := db.GetMaterial(IdB(db));
	end;

	function PhysMaterialDB.ValidateMaterial(id: sint): boolean;
	begin
		result := ((id > 0) and (id < length(_m))) or (id = 0);
	end;

	procedure PhysMaterialDB._Code2Mats(code: uint; out a, b: sint);
	begin
		a := code div (1 shl 16);
		b := code mod (1 shl 16);
		Assert(ValidateMaterial(a) and ValidateMaterial(b));
	end;

	function PhysMaterialDB._Mats2Code(a, b: sint): uint;
	begin
		Assert(ValidateMaterial(a) and ValidateMaterial(b));
		if a <= b then
			result := a * (1 shl 16) + b
		else
			result := b * (1 shl 16) + a;
	end;

	function PhysMaterialDB._Deserialize(s: pStream): boolean;
	label _finally_;
	var
		n, i, a: sint;
		flags: uint;
		int: Interaction;
	begin
		result := no;
		if not Assigned(MakeRef(s)) then exit;

	{$ifdef Debug} LogR('Загрузка коллекции физических материалов ' + StreamPath.Log(s^.path) + '... '); {$endif}
		Deserialize_signature(s, Signature, no);
		n := Deserialize_ui16(s);
		SetLength(_m, n);
		for i := 1 to High(_m) do
		begin
			_m[i].name := Deserialize_string(s);
			_n2id.Add(_m[i].name, i);
		end;

		n := Deserialize_ui32(s);
		_ints.Clear;
		for i := 0 to n - 1 do
		begin
			flags := Deserialize_ui8(s);
			int.collidable := (flags and COLLIDABLE_BIT) <> 0;
			a := Deserialize_ui16(s);
			int.code := _Mats2Code(a, Deserialize_ui16(s));
			int.staticFriction := Deserialize_f16(s);
			int.kineticFriction := Deserialize_f16(s);
			_ints.Add(int);
		end;

	{$ifdef Debug} Log('OK (материалов: ' + ToString(length(_m)) + '; взаимодействий: ' + ToString(_ints.Count) + ')', logOK); {$endif}
		result := yes;
	_finally_:
		Release(s);
	end;

	constructor PhysMaterialDB.Init;
	begin
		inherited Init;
		_m := nil;
		_n2id.Init;
		_ints.Init;
		CreateMaterial(RigidBody.DefaultMaterial);
		ForceInteraction(0, 0);
	end;

	constructor PhysMaterialDB.Init(s: pStream);
	var
		ok: boolean;
	begin
		Init;
		ok := _Deserialize(MakeRef(s));
		Release(s);
		if not ok then ConstructorFailed;
	end;

	destructor PhysMaterialDB.Done;
	begin
		_n2id.Done;
		_ints.Done;
		inherited Done;
	end;

	function PhysMaterialDB.Serialize(s: pStream): boolean;
	label _finally_;
	var
		i: sint;
		int_it: InteractionSet.Iterator;
		int: pInteraction;
		flags: uint;
	begin
		result := no;
		if not Assigned(MakeRef(s)) then exit;

		Serialize_conststring(s, Signature);
		Serialize_ui16(s, length(_m));
		for i := 1 to High(_m) do
			Serialize_string(s, _m[i].name);

		Serialize_ui32(s, _ints.Count);
		int_it := _ints.GetIterator;
		while _ints.Next(int_it) do
		begin
			int := _ints.GetKey(int_it);
			flags := 0;
			if int^.collidable then flags := flags or COLLIDABLE_BIT;
			Serialize_ui8(s, flags);
			Serialize_ui16(s, int^.IdA(self));
			Serialize_ui16(s, int^.IdB(self));
			Serialize_f16(s, int^.staticFriction);
			Serialize_f16(s, int^.kineticFriction);
		end;

		result := yes;
	_finally_:
		Release(s);
	end;

	function PhysMaterialDB.CreateMaterial(const name: PoolString): boolean;
	begin
		result := _n2id.Find(name) < 0;
		if result then
		begin
			SetLength(_m, length(_m) + 1);
			_m[High(_m)].name := name;
			_n2id.Add(name, High(_m));
		end;
	end;

	function PhysMaterialDB.RemoveMaterial(id: sint): boolean;
	var
		i, a, b: sint;
		oi: array of Interaction;
		it: InteractionSet.Iterator;
		int: pInteraction;
	begin
		Assert(ValidateMaterial(id));
		result := no;
		if id = 0 then exit;

		oi := nil;
		it := _ints.GetIterator;
		while _ints.Next(it) do
		begin
			int := _ints.GetKey(it);
			_Code2Mats(int^.code, a, b);
			if (int^.IdA(self) >= id) or (int^.IdB(self) >= id) then
			begin
				SetLength(oi, length(oi) + 1);
				oi[High(oi)] := int^;
			end;
		end;

		for i := 0 to High(oi) do
		begin
			_ints.Remove(oi[i].code);
			if (oi[i].IdA(self) = id) or (oi[i].IdB(self) = id) then
				continue
			else
			begin
				_Code2Mats(oi[i].code, a, b);
				if a > id then dec(a);
				if b > id then dec(b);
				oi[i].code := _Mats2Code(a, b);
				_ints.Add(oi[i]);
			end;
		end;

		_n2id.Remove(_m[id].name);
		for i := id to High(_m) - 1 do
		begin
			_m[i] := _m[i + 1];
			_n2id.Add(_m[i].name, i);
		end;
		SetLength(_m, length(_m) - 1);
		result := yes;
	end;

	function PhysMaterialDB.FindMaterial(const name: PoolString): pMaterial;
	var
		id: sint;
	begin
		id := GetMaterialID(name);
		if id >= 0 then result := GetMaterial(id) else result := nil;
	end;

	function PhysMaterialDB.GetMaterial(id: sint): pMaterial;
	begin
		Assert(ValidateMaterial(id));
		result := @_m[id];
	end;

	function PhysMaterialDB.GetMaterialID(const name: PoolString): sint;
	begin
		result := _n2id.Find(name);
	end;

	function PhysMaterialDB.Rename(id: sint; const newName: PoolString): boolean;
	var
		m: pMaterial;
	begin
		result := GetMaterialID(newName) < 0;
		if result then
		begin
			m := GetMaterial(id);
			_n2id.Remove(m^.name);
			m^.name := newName;
			_n2id.Add(m^.name, id);
		end;
	end;

	function PhysMaterialDB.MaterialsCount: sint;
	begin
		result := length(_m);
	end;

	function PhysMaterialDB.FindInteraction(a, b: sint): pInteraction;
	begin
		result := _ints.Find(_Mats2Code(a, b));
	end;

	function PhysMaterialDB.SuitableInteraction(a, b: sint): pInteraction;
	begin
		result := FindInteraction(a, b);
		if Assigned(result) then exit;
		result := FindInteraction(a, 0);
		if Assigned(result) then exit;
		result := FindInteraction(0, b);
		if Assigned(result) then exit;
		result := FindInteraction(0, 0);
	end;

	function PhysMaterialDB.ForceInteraction(a, b: sint): pInteraction;
	var
		nint: Interaction;
	begin
		result := FindInteraction(a, b);
		if not Assigned(result) then
		begin
			nint.Reset(_Mats2Code(a, b), SuitableInteraction(a, b));
			_ints.Add(nint);
			result := FindInteraction(a, b);
			Assert(Assigned(result));
		end;
	end;

	function PhysMaterialDB.RemoveInteraction(a, b: sint): boolean;
	begin
		if (a = 0) and (b = 0) then exit(no);
		result := _ints.Remove(_Mats2Code(a, b));
	end;

	function PhysMaterialDB.InteractionsCount: sint;
	begin
		result := _ints.Count;
	end;

	constructor RigidPrimitive.Init(newKind: RigidPrimitiveKind);
	begin
		try
			Newton.loader.Load;
		except
			instant_reraise_from_constructor;
		end;
		if _nInstances = 0 then _pool.Init;
		inc(_nInstances);

		inherited Init;
		_inPool := no;
		_kind := newKind;
		_newt := nil;
		_inertiaMoment := DefaultInertiaMoment;
		_massCenter := Vec3.Zero;
	end;

	constructor RigidPrimitive.InitNull;
	begin
		Init(rigid_Null);
		_newt := Newton.CreateNull(TempWorld);
	end;

	constructor RigidPrimitive.InitBox(sx, sy, sz: float; const newTransform: Transform; partOfCompound: boolean);
	begin
		Init(rigid_Box);
		_newt := Newton.CreateBox(TempWorld, sx, sy, sz, 0, newTransform.ToMatrixWoScale);
		if not partOfCompound then _MakeUnique;
		CalculateMassProperties;
	end;

	constructor RigidPrimitive.InitBox(const sizes: Vec3; const newTransform: Transform; partOfCompound: boolean);
	begin
		InitBox(sizes.x, sizes.y, sizes.z, newTransform, partOfCompound);
	end;

	constructor RigidPrimitive.InitSphere(rx, ry, rz: float; const newTransform: Transform; partOfCompound: boolean);
	begin
		Init(rigid_Sphere);
		_newt := Newton.CreateSphere(TempWorld, rx, ry, rz, 0, newTransform.ToMatrixWoScale);
		if not partOfCompound then _MakeUnique;
		CalculateMassProperties;
	end;

	constructor RigidPrimitive.InitSphere(const rads: Vec3; const newTransform: Transform; partOfCompound: boolean);
	begin
		InitSphere(rads.x, rads.y, rads.z, newTransform, partOfCompound);
	end;

	constructor RigidPrimitive.InitSphere(r: float; const newTransform: Transform; partOfCompound: boolean);
	begin
		InitSphere(r, r, r, newTransform, partOfCompound);
	end;

	constructor RigidPrimitive.InitCapsule(newRadius, newHeight: float; const newTransform: Transform; partOfCompound: boolean);
	begin
		Init(rigid_Capsule);
		_newt := Newton.CreateCapsule(TempWorld, newRadius, newHeight, 0, newTransform.ToMatrixWoScale);
		if not partOfCompound then _MakeUnique;
		CalculateMassProperties;
	end;

	constructor RigidPrimitive.InitCone(const newRadius, newHeight: float; const newTransform: Transform; partOfCompound: boolean);
	begin
		Init(rigid_Cone);
		_newt := Newton.CreateCone(TempWorld, newRadius, newHeight, 0, newTransform.ToMatrixWoScale);
		if not partOfCompound then _MakeUnique;
		CalculateMassProperties;
	end;

	constructor RigidPrimitive.InitCylinder(newRadius, newHeight: float; const newTransform: Transform; partOfCompound: boolean);
	begin
		Init(rigid_Cylinder);
		_newt := Newton.CreateCylinder(TempWorld, newRadius, newHeight, 0, newTransform.ToMatrixWoScale);
		if not partOfCompound then _MakeUnique;
		CalculateMassProperties;
	end;

	constructor RigidPrimitive.InitCompound(const newPrims: array of pRigidPrimitive);
	var
		cc: Newton.ppCollision;
		i: sint;
		ok: boolean;
	begin
		ok := length(newPrims) > 0;
		for i := 0 to High(newPrims) do
		begin
			MakeRef(newPrims[i]);
			ok := ok and Assigned(newPrims[i]) and not (rpk_Static in PrimitiveKindInfo[newPrims[i]^.kind].flags);
		end;
		if ok then
		begin
			Init(rigid_Compound);
			cc := GetMem(length(newPrims) * sizeof(Newton.pCollision));
			for i := 0 to High(newPrims) do
				cc[i] := newPrims[i]^._newt;
			_newt := Newton.CreateCompoundCollision(TempWorld, length(newPrims), cc, 0);
			_MakeUnique;
			CalculateMassProperties;
			FreeMem(cc);
		end;
		for i := 0 to High(newPrims) do
			ReleaseWeak(newPrims[i]);
		if not ok then Fail;
	end;

	constructor RigidPrimitive.InitConvex(const newCloud: array of Vec3; const newTransform: Transform; partOfCompound: boolean);
	var
		i: sint;
		ncloud: Newton.pVec3;
	begin
		Init(rigid_Convex);
		ncloud := GetMem(length(newCloud) * sizeof(Newton.Vec3));
		for i := 0 to High(newCloud) do
			ncloud[i] := newCloud[i];
		_newt := Newton.CreateConvexHull(TempWorld, length(newCloud), Newton.pFloat(ncloud), sizeof(Newton.Vec3), 0.03, 0, newTransform.ToMatrixWoScale);
		if not partOfCompound then _MakeUnique;
		CalculateMassProperties;
		FreeMem(ncloud);
	end;

	constructor RigidPrimitive.InitMesh(const verts: array of Vec3; const inds: array of sint; const newTransform: Transform);
	var
		i, j: sint;
		face: array[0 .. 2] of Newton.Vec3;
	begin
		Init(rigid_Mesh);
	{$ifdef Debug} LogR('Построение Tree Collision... '); {$endif}
		_newt := Newton.CreateTreeCollision(TempWorld, 0);
		Newton.TreeCollisionBeginBuild(_newt);
		for i := 0 to length(inds) div 3 - 1 do
		begin
			for j := 0 to 2 do
				face[j] := newTransform * verts[inds[3*i + j]];
			Newton.TreeCollisionAddFace(_newt, 3, @face[0], sizeof(Newton.Vec3), 0);
		end;
		Newton.TreeCollisionEndBuild(_newt, 1);
		_MakeUnique;
	{$ifdef Debug} Log('Завершено (' + ToString(length(inds) div 3) + ' трисов)', logOK); {$endif}
	end;

	constructor RigidPrimitive.InitHeightMap(sizeX, sizeY: sint; const map: array of float; const matID: array of tMaterialID; const cellSize: float;
		const newTransform: Transform);
	var
		i, x, y: sint;
		mid: pCuchar;
		elev: pCushort;
		a, b: float;
	begin
		Assert(@newTransform = @newTransform);
		Init(rigid_HeightMap);
		mid := GetMem(sizeX * sizeY * sizeof(cuchar));
		for i := 0 to sizeX * sizeY - 1 do
			mid[i] := matID[i];

		a := map[0];
		b := map[0];
		for i := 1 to sizeX * sizeY - 1 do
		begin
			if map[i] < a then a := map[i];
			if map[i] > b then b := map[i];
		end;
		elev := GetMem(sizeX * sizeY * sizeof(cushort));
		for y := 0 to sizeY-1 do
			for x := 0 to sizeX-1 do
				pCushort(elev)[y * sizeX + x] := round(remap(map[y * sizeX + x], a, b, 0.0, High(cushort)));

		_newt := Newton.CreateHeightFieldCollision(TempWorld, sizeX, sizeY, 0, elev, mid, cellSize, (b-a) / High(cushort), 0);
		_MakeUnique;
		FreeMem(elev);
		FreeMem(mid);
	end;

	destructor RigidPrimitive.Done;
	begin
		if instantly_reraised_from_constructor then exit;
		if _inPool then _pool.Remove(@self);
		if Assigned(_newt) then Newton.ReleaseCollision(TempWorld, _newt);
		inherited Done;

		dec(_nInstances);
		if _nInstances = 0 then _pool.Done;
		Newton.loader.Unload;
	end;

	function RigidPrimitive.RayCast(rctransform: pTransform; const p1, p2: Vec3; x: pFloat; point, normal: pVec3): boolean;
	var
		itf: Transform;
		nnorm: Newton.Vec3;
		matID: cint;
		cast: float;
	begin
		if Assigned(rctransform) then
		begin
			itf := rctransform^.Inversed;
			cast := Newton.CollisionRayCast(_newt, itf * p1, itf * p2, nnorm, @matID);
		end else
			cast := Newton.CollisionRayCast(_newt, p1, p2, nnorm, @matID);

		result := (matID <> MaterialID_Transparent) and (cast <= 1.0);
		if result then
		begin
			if Assigned(x) then x^ := cast;
			if Assigned(rctransform) then
			begin
				if Assigned(point) then point^ := rctransform^ * (p1 * (1.0 - cast) + p2 * cast);
				if Assigned(normal) then normal^ := (rctransform^.rot * Newton.From(nnorm)) {$ifdef RenormalizeVectors}.MaybeNormalized{$endif};
			end else
			begin
				if Assigned(point) then point^ := p1 * (1.0 - cast) + p2 * cast;
				if Assigned(normal) then normal^ := Newton.From(nnorm) {$ifdef RenormalizeVectors}.MaybeNormalized{$endif};
			end;
		end;
	end;

	function RigidPrimitive.RayCast(const p1, p2: Vec3): boolean;
	begin
		result := RayCast(nil, p1, p2, nil, nil, nil);
	end;

	function RigidPrimitive.ClosestPoint(world: pPhysWorld; const cptransform: Transform; const p: Vec3; out point, normal: Vec3): boolean;
	var
		itf: Transform;
		npt, nnorm: Newton.Vec3;
		r: cint;
	begin
		itf := cptransform.Inversed;
		r := Newton.CollisionPointDistance(world^._newt, itf * p, _newt, Newton.IdentityMat4, npt, nnorm, 0);
		result := r <> 0;
		if result then
		begin
			point := cptransform * Newton.From(npt);
			normal := cptransform.rot * Newton.From(nnorm);
		end else
		begin
			point := p;
			normal := Vec3.Zero;
		end;
	end;

	procedure NewtSerializeCallback(s: pointer; buffer: pointer; size: csize_t); cdecl;
	begin
		pStream(s)^.Write(buffer, size);
	end;

	procedure RigidPrimitive.Serialize(s: pStream);
	var
		flags: uint16;
		oldInertiaMoment, oldMassCenter: Vec3;
	begin
		if not Assigned(s) then exit;
	{$ifdef Debug} LogR('Сохранение коллижна — ' + HumanInfo + ' — в ' + StreamPath.Log(s^.path) + '... '); {$endif}
		MakeRef(s);
		flags := 0;
		if rpk_SaveMassProps in PrimitiveKindInfo[_kind].flags then
			flags := flags or MASS_PROPERTIES_BIT
		else
		begin
			oldInertiaMoment := _inertiaMoment;
			oldMassCenter := _massCenter;
			if CalculateMassProperties and ((_inertiaMoment <> oldInertiaMoment) or (_massCenter <> oldMassCenter)) then
			begin
				flags := flags or MASS_PROPERTIES_BIT;
				_inertiaMoment := oldInertiaMoment;
				_massCenter := oldMassCenter;
			end;
		end;

		Serialize_ui16(s, ord(_kind));
		Serialize_ui16(s, flags);
		Newton.CollisionSerialize(TempWorld, _newt, @NewtSerializeCallback, s);
		if (flags and MASS_PROPERTIES_BIT) <> 0 then
		begin
			Serialize_vec3f32(s, _massCenter);
			Serialize_vec3f32(s, _inertiaMoment);
		end;
	{$ifdef Debug} Log('Коллижн сохранён в ' + StreamPath.Log(s^.path), logOK); {$endif}
		Release(s);
	end;

	function RigidPrimitive.Deserialize(s: pStream): pRigidPrimitive;
	begin
		result := new(pRigidPrimitive, Init(rigid_Null));
		try
			result^._Deserialize(s);
			result := Merge(result, yes);
		except
			Free(result);
			raise;
		end;
	end;

	procedure NewtDeserializeCallback(s: pointer; buffer: pointer; size: csize_t); cdecl;
	begin
		pStream(s)^.Read(buffer, size);
	end;

	procedure RigidPrimitive._Deserialize(s: pStream);
	var
		flags: uint16;
	begin
		MakeRef(s);
		try
		{$ifdef Debug} LogR('Загрузка физического примитива из ' + StreamPath.Log(s^.path) + '... '); {$endif}
			_kind := RigidPrimitiveKind(Deserialize_ui16(s));
			flags := Deserialize_ui16(s);
			_newt := Newton.CreateCollisionFromSerialization(TempWorld, @NewtDeserializeCallback, s);
			if not Assigned(_newt) then
				raise Error('Не удалось загрузить физический примитив из ' + StreamPath.Human(s^.path) + '.');
			_MakeUnique;
			if flags and MASS_PROPERTIES_BIT <> 0 then
			begin
				_massCenter    := Deserialize_vec3f32(s);
				_inertiaMoment := Deserialize_vec3f32(s);
			end else
				CalculateMassProperties;
		{$ifdef Debug} Log('Примитив ' + StreamPath.Log(s^.path) + ' — ' + HumanInfo + ' — загружен', logOK); {$endif}
		finally
			Release(s);
		end;
	end;

	function RigidPrimitive.CalculateMassProperties: boolean;
	var
		nim, ncom: Newton.Vec3;
	begin
		result := not Static;
		if not result then exit;
		Newton.ConvexCollisionCalculateInertialMatrix(_newt, nim, ncom);
		_inertiaMoment := Newton.From(nim);
		_massCenter := Newton.From(ncom);
	end;

	function RigidPrimitive.Static: boolean;
	begin
		result := rpk_Static in PrimitiveKindInfo[_kind].flags;
	end;

	function RigidPrimitive._NewtonHumanInfo(coll: Newton.pCollision): string;
	type
		SID = Newton.SERIALIZE_ID;
	var
		ninfo: Newton.CollisionInfoRecord;
		tf: Transform;
		angle: float;
		axis: Vec3;
		i: sint;
	begin
		Newton.CollisionGetInfo(coll, ninfo);
		with ninfo do
		begin
			case m_collisionType of
				SID.BOX:  with asbox do result := 'Box: размеры ' + ToString(Vec3.Make(m_x, m_y, m_z));
				SID.CONE: with ascone do result := 'Cone: радиус = ' + ToString(m_r) + ', высота = ' + ToString(m_height);
				SID.SPHERE:
					with assphere do
					begin
						result := 'Sphere: радиус';
						if Equals(m_r0, m_r1) and Equals(m_r1, m_r2) and Equals(m_r0, m_r2) then
							result += ' = ' + ToString(m_r0)
						else
							result += 'ы ' + ToString(Vec3.Make(m_r0, m_r1, m_r2));
					end;
				SID.CAPSULE:
					with ascapsule do
					begin
						result := 'Capsule: радиус';
						if Equals(m_r0, m_r1) then result += ' = ' + ToString(m_r0) else result += 'ы ' + ToString(Vec2.Make(m_r0, m_r1));
						result += ', высота = ' + ToString(m_height);
					end;
				SID.CYLINDER:
					with ascylinder do
					begin
						result := 'Cylinder: радиус';
						if Equals(m_r0, m_r1) then result += ' = ' + ToString(m_r0) else result += 'ы ' + ToString(Vec2.Make(m_r0, m_r1));
						result += ', высота = ' + ToString(m_height);
					end;
				SID.COMPOUND:
					begin
						with ascompound do
						begin
							result := 'Compound (' + ToString(m_childrenCount) + '):' + EOL + '(';
							for i := 0 to m_childrenCount - 1 do
							begin
								if i > 0 then result += ', ';
								result += EOL + '　　　　' + _NewtonHumanInfo(m_children[i]);
							end;
							result += EOL + ')';
						end;
					end;
				SID.CONVEXHULL:
					with asconvexhull do
						result := 'Convex Hull: ' + lang_amount(m_vertexCount, '{N} вершин{а/ы/}') + ', ' + lang_amount(m_faceCount, '{N} гран{ь/и/ей}');
				SID.CONVEXMODIFIER: result := 'Convex Hull Modifier: ???';
				SID.CHAMFERCYLINDER: result := 'Chamfer Cylinder: ???';
				SID.TREE:
					with astree do
						result := 'Tree: ' + lang_amount(m_vertexCount, '{N} вершин{а/ы/}') + ', ' + lang_amount(m_indexCount, '{N} индекс{/а/ов}');
				SID.NULL: result := 'Null';
				SID.HEIGHTFIELD:
					with asheightfield do
					begin
						result := 'Height Field: sizeX = ' + ToString(m_width) + ', sizeY = ' + ToString(m_height) + ', масштаб';
						if Equals(m_horizontalScale, m_verticalScale) then
							result += ' = ' + ToString(m_horizontalScale)
						else
							result += 'ы: ' + ToString(Vec2.Make(m_horizontalScale, m_verticalScale));
					end;
				SID.USERMESH: result := 'User Mesh: ???';
				SID.SCENE: result := 'Scene: ???';
				else
					result += '[неизвестный примитив, код ' + ToString(m_collisionType) + ']';
			end;

			tf := Transform.FromMatrix(Newton.From(m_offsetMatrix));
			if tf.tr <> Vec3.Zero then result += ', смещение: ' + ToString(tf.tr);
			if not SameRotation(tf.rot, Quaternion.Identity) then
			begin
				tf.rot.GetRotation(angle, axis);
				result += ', поворот: ' + ToString(Rad2Deg * angle) + '° ' + ToString(axis);
			end;
		end;
	end;

	procedure RigidPrimitive._MakeUnique;
	begin
		if _kind <> rigid_Compound then
			Newton.CollisionMakeUnique(TempWorld, _newt);
	end;

	function RigidPrimitive.HumanInfo: string;
	begin
		if not Assigned(_newt) then exit('[НЕТ ПРИМИТИВА]');
		result := _NewtonHumanInfo(_newt);

		if not (rpk_Static in PrimitiveKindInfo[_kind].flags) then
		begin
			result += ';';
			if Pos(EOL, result) > 0 then result += EOL else result += ' ';
			result += 'центр тяжести: ' + ToString(_massCenter) + ', '
			          + 'момент инерции: ' + ToString(_inertiaMoment);
		end;
	end;

	function RigidPrimitive._Hash(prim: pRigidPrimitive): Hash.Value;
	begin
		result := Hash.OfUint(uint(ord(prim^.kind)));
		if prim^.kind <> rigid_Null then result := result xor _NewtonHash(prim^._newt);
	end;

	function RigidPrimitive._NewtonHash(coll: Newton.pCollision): Hash.Value;
	type
		SID = Newton.SERIALIZE_ID;
	var
		info: Newton.CollisionInfoRecord;
		tf: Transform;
		angle: float;
		axis: Vec3;
		i: sint;
	begin
		Newton.CollisionGetInfo(coll, info);
		tf := Transform.FromMatrix(Newton.From(info.m_offsetMatrix));
		tf.rot.GetRotation(angle, axis);

		with info do
			case m_collisionType of
				SID.BOX: result := Hash.OfFloatEps(asbox.m_x, PoolEps) xor Hash.OfFloatEps(asbox.m_y, PoolEps) xor Hash.OfFloatEps(asbox.m_z, PoolEps);
				SID.CONE: result := Hash.OfFloatEps(ascone.m_r, PoolEps) xor Hash.OfFloatEps(ascone.m_height, PoolEps);
				SID.SPHERE: result := Hash.OfFloatEps(assphere.m_r0, PoolEps) xor Hash.OfFloatEps(assphere.m_r1, PoolEps) xor Hash.OfFloatEps(assphere.m_r2, PoolEps);
				SID.CAPSULE: result := Hash.OfFloatEps(ascapsule.m_r0, PoolEps) xor Hash.OfFloatEps(ascapsule.m_r1, PoolEps) xor Hash.OfFloatEps(ascapsule.m_height, PoolEps);
				SID.CYLINDER: result := Hash.OfFloatEps(ascylinder.m_r0, PoolEps) xor Hash.OfFloatEps(ascylinder.m_r1, PoolEps) xor Hash.OfFloatEps(ascylinder.m_height, PoolEps);
				SID.COMPOUND:
					begin
						result := ascompound.m_childrenCount;
						for i := 0 to ascompound.m_childrenCount - 1 do
							result := result xor _NewtonHash(ascompound.m_children[i]);
					end;
				else
					Assert(no, ToString(m_collisionType));
			end;
	end;

	function RigidPrimitive._Equals(a, b: pRigidPrimitive): boolean;
	begin
		if (a = b) or (a^._inPool and b^._inPool) then
			result := a = b
		else
			result := (a^.kind = b^.kind) and ((a^.kind = rigid_Null) or _NewtonEquals(a^._newt, b^._newt));
	end;

	function RigidPrimitive._NewtonEquals(a, b: Newton.pCollision): boolean;
	type
		SID = Newton.SERIALIZE_ID;
	var
		infoA, infoB: Newton.CollisionInfoRecord;
		tfA, tfB: Transform;
		boxA: Newton.BoxParam           absolute infoA.asbox;          boxB: Newton.BoxParam           absolute infoB.asbox;
		coneA: Newton.ConeParam         absolute infoA.ascone;         coneB: Newton.ConeParam         absolute infoB.ascone;
		sphereA: Newton.SphereParam     absolute infoA.assphere;       sphereB: Newton.SphereParam     absolute infoB.assphere;
		capsuleA: Newton.CapsuleParam   absolute infoA.ascapsule;      capsuleB: Newton.CapsuleParam   absolute infoB.ascapsule;
		cylinderA: Newton.CylinderParam absolute infoA.ascylinder;     cylinderB: Newton.CylinderParam absolute infoB.ascylinder;
		compoundA: Newton.CompoundCollisionParam absolute infoA.ascompound;
		compoundB: Newton.CompoundCollisionParam absolute infoB.ascompound;
		i: sint;
	begin
		if a = b then exit(yes);
		result := no;

		Newton.CollisionGetInfo(a, infoA);
		Newton.CollisionGetInfo(b, infoB);
		if infoA.m_collisionType <> infoB.m_collisionType then exit;
		tfA := Transform.FromMatrix(Newton.From(infoA.m_offsetMatrix));
		tfB := Transform.FromMatrix(Newton.From(infoB.m_offsetMatrix));
		if not Equals(tfA, tfB, PoolEps) then exit;

		case infoA.m_collisionType of
			SID.BOX: result := Equals(boxA.m_x, boxB.m_x, PoolEps) and Equals(boxA.m_y, boxB.m_y, PoolEps) and Equals(boxA.m_z, boxB.m_z, PoolEps);
			SID.CONE: result := Equals(coneA.m_r, coneB.m_r, PoolEps) and Equals(coneA.m_height, coneB.m_height, PoolEps);
			SID.SPHERE: result := Equals(sphereA.m_r0, sphereB.m_r0, PoolEps) and Equals(sphereA.m_r1, sphereB.m_r1, PoolEps) and Equals(sphereA.m_r2, sphereB.m_r2, PoolEps);
			SID.CAPSULE: result := Equals(capsuleA.m_r0, capsuleB.m_r0, PoolEps) and Equals(capsuleA.m_r1, capsuleB.m_r1, PoolEps) and Equals(capsuleA.m_height, capsuleB.m_height, PoolEps);
			SID.CYLINDER: result := Equals(cylinderA.m_r0, cylinderB.m_r0, PoolEps) and Equals(cylinderA.m_r1, cylinderB.m_r1, PoolEps) and Equals(cylinderA.m_height, cylinderB.m_height, PoolEps);
			SID.COMPOUND:
				begin
					if compoundA.m_childrenCount <> compoundB.m_childrenCount then exit;
					for i := 0 to compoundA.m_childrenCount - 1 do
						if not _NewtonEquals(compoundA.m_children[i], compoundB.m_children[i]) then exit;
					result := yes;
				end;
			else
				Assert(no, ToString(infoA.m_collisionType));
		end;
	end;

	function RigidPrimitive.Merge(prim: pRigidPrimitive; allowFree: boolean): pRigidPrimitive;
	begin
		if (prim^._inPool) or (rpk_DontPool in PrimitiveKindInfo[prim^._kind].flags) then exit(prim);
		result := _pool.Find(prim);
		if Assigned(result) then
		begin
			Assert(result <> prim);
			if allowFree then Free(prim);
		end else
		begin
			_pool.Add(prim);
		{$ifdef Debug} stat.Note(max_phys_primitives_in_pool, _pool.Count); {$endif}
			prim^._inPool := yes;
			result := prim;
		end;
	end;

	procedure RigidPrimitive.Merge(var prim: pRigidPrimitive);
	begin
		SetRef(prim, Merge(prim, no));
	end;

	constructor tRigidInteraction.Init;
	begin
		inherited Init;
		_collidable := yes;
	end;

	destructor tRigidInteraction.Done;
	begin
		inherited Done;
	end;

{$hints off}
	function _NewtonGetBuoyancyPlane(collisionID: cint; context: pointer; constref globalSpaceMatrix: Newton.Mat4;
		out globalSpacePlane: Newton.Plane): cint; cdecl;
{$hints on}
	var
		water: pWater absolute context;
		plane: UMath.Plane;
		i: sint;
	begin
		Assert((@globalSpaceMatrix = @globalSpaceMatrix) and (collisionID = collisionID));
		plane := water^.Plane;
		for i := 0 to High(globalSpacePlane) do
			globalSpacePlane[i] := -plane.data.coefs[i];
		result := 1;
	end;

	procedure _NewtonApplyForceAndTorque(body: Newton.pBody; timestep: Newton.Float; threadIndex: cint); cdecl;
	var
		b: pRigidBody;
		rig: RigidBody.pRigidSpecific;
		w: pPhysWorld;
		v3: Newton.Vec3;
		force, torque, nv: Vec3;
		k, vabs: float;
		i: sint;
		aabb: UMath.AABB;
	begin
		Assert((@timestep = @timestep) and (@threadIndex = @threadIndex));
		b := Newton.BodyGetUserData(body);
		Assert(Assigned(b), 'ApplyTransform: newton body with unassigned userdata');
		Assert(b^._kind = rigid_DynamicBody, 'non-static non-DynamicBody!');
		w := b^._world;

		rig := b^.asRigid;
		force := Vec3.Zero;
		torque := Vec3.Zero;

		for i := 0 to High(rig^.water) do
		begin
			aabb := NewtonGetAABB(body);
			nv := -w^.gravity.Normalized;
			k := (aabb.B - aabb.A) ** nv;
			if NotZero(k) then k := 1.0 / k else k := 1.0;
			force += 200.0 * rig^.water[i]^.Density * Newton.ConvexCollisionCalculateVolume(b^.primitive^._newt) * nv *
				Clamp(1.0 - Max(rig^.water[i]^.Plane.SignedDistance(aabb.A), rig^.water[i]^.Plane.SignedDistance(aabb.B)) * k, 0.0, 1.0);
			{NewtonBodyAddBuoyancyForce(body, rig^.water[i]^.Density, rig^.water[i]^.LinearViscosity, rig^.water[i]^.AngularViscosity,
				NewtonVec3(-w^.gravity.Normalized),  @_NewtonGetBuoyancyPlane, rig^.water[i]);}
		end;

		if b^.selfForce <> Vec3.Zero then
			force += b^.selfForce;
		if b^.selfTorque <> Vec3.Zero then
			torque += b^.selfTorque;

		if [rigid_InfiniteRotIX .. rigid_InfiniteRotIZ] * b^._flags <> [] then
		begin
			Newton.BodyGetOmega(body, v3);
			if rigid_InfiniteRotIX in b^._flags then v3[0] := 0;
			if rigid_InfiniteRotIY in b^._flags then v3[1] := 0;
			if rigid_InfiniteRotIZ in b^._flags then v3[2] := 0;
			Newton.BodySetOmega(body, v3);
		end;

		if w^.gravityEnabled and not (rigid_UnaffectedByGravity in b^._flags) then
			force += w^.gravity * rig^.mass;

		if NotZero(rig^.entangle.Value) then
		begin
			k := rig^.EntangleCoef;
			nv := b^.Velocity.Normalized(vabs);
			b^.Velocity := nv * max(vabs - rig^.entangle.Value * timestep, 0) * pow(k, timestep);
			force *= k;
			torque *= k;
		end;
		if force <> Vec3.Zero then
			Newton.BodyAddForce(body, force);
		if torque <> Vec3.Zero then
			Newton.BodyAddTorque(body, torque);
	end;

{$hints off}
	procedure _NewtonBodyTransformCallback(body: Newton.pBody; constref matrix: Newton.Mat4; threadIndex: cint); cdecl;
{$hints on}
	var
		b: pRigidBody;
		t: Transform;
	begin
		Assert(@threadIndex = @threadIndex);
		b := Newton.BodyGetUserData(body);
		Assert(Assigned(b));

		t := Transform.FromMatrix(Newton.From(matrix));
		b^.GlobalTransform := t;
	end;

	constructor RigidBody.tTmpNewtonOpts.Init;
	begin
		velocity := Vec3.Zero;
		omega := Vec3.Zero;
	end;

	destructor RigidBody.tTmpNewtonOpts.Done;
	begin
	end;

	procedure _CallOnCollide(const info: SingleDelegateInfo; param: pointer);
	var
		args: pOnCollideArgs absolute param;
	begin
		with args^ do tOnCollideProc(info.proc)(b0, b1, contact^, info);
	end;

	procedure _CallOnPhantom(const info: SingleDelegateInfo; param: pointer);
	var
		args: pOnPhantomArgs absolute param;
	begin
		with args^ do tOnPhantomProc(info.proc)(b0, b1, info);
	end;

	procedure RigidBody.tRigidSpecific.Initialize(fully: boolean);
	begin
		selfForce := Vec3.Zero;
		selfTorque := Vec3.Zero;
		mass := INFINITE_MASS;
		joints := nil;
		tmpNewt := nil;
		phantoms := nil;
		water := nil;
		if fully then
		begin
			matName := DefaultMaterial;
			onCollide.Init;
			onEnterPhantom.Init;
			onExitPhantom.Init;
			entangle.Init(0.0);
		end;
	end;

	procedure RigidBody.tRigidSpecific.Finalize(var body: RigidBody);
	var
		i: sint;
	begin
		entangle.Done;
		onEnterPhantom.Done;
		onExitPhantom.Done;
		onCollide.Done;
		for i := 0 to High(joints) do
		begin
			if joints[i]^._parent = @body then
				joints[i]^._parent := nil
			else
				joints[i]^._child := nil;
			joints[i]^.Break;
			Release(joints[i]);
		end;
		joints := nil;
		phantoms := nil;
		water := nil;
		if Assigned(tmpNewt) then dispose(tmpNewt, Done);
	end;

	function RigidBody.tRigidSpecific.EnsureTmpNewt: pTmpNewtonOpts;
	begin
		result := tmpNewt;
		if not Assigned(result) then
		begin
			result := new(pTmpNewtonOpts, Init);
			tmpNewt := result;
		end;
	end;

	procedure RigidBody.tRigidSpecific.UpdateMaterial(var body: RigidBody; world: pPhysWorld);
	var
		id: sint;
	begin
		if not Assigned(world) then exit;
		if Assigned(world^._materialDb) then id := world^._materialDb^.GetMaterialID(matName) else id := -1;
		if id >= 0 then
		begin
			if Assigned(body._newt) then
				Newton.BodySetMaterialGroupID(body._newt, id);
		end
	{$ifdef Debug} else Log('Физический материал "' + matName + '" не найден', logError) {$endif};
	end;

	procedure RigidBody.tRigidSpecific.CallOnCollide(var body: RigidBody; b1: pRigidBody; const contact: ContactInfo);
	var
		args: tOnCollideArgs;
	begin
		Assert(not onCollide.Empty);
		args.b0 := @body;
		args.b1 := b1;
		args.contact := @contact;
		onCollide.Call(@_CallOnCollide, @args);
	end;

	function RigidBody.tRigidSpecific.EntangleCoef: float;
	begin
		result := 1.0 / (1.0 + entangle.Value);
	end;

	procedure RigidBody.tRigidSpecific.AddPhantom(ph: pRigidBody);
	begin
		SetLength(phantoms, length(phantoms) + 1);
		phantoms[High(phantoms)] := ph;
		if TypeOf(ph^) = TypeOf(tWater) then
		begin
			SetLength(water, length(water) + 1);
			water[High(water)] := pWater(ph);
		end;
	end;

	procedure RigidBody.tRigidSpecific.RemovePhantom(ph: pRigidBody);
	var
	{$ifdef Debug} ok: boolean; {$endif}
		i: sint;
	begin
	{$ifdef Debug} ok := no; {$endif}
		for i := 0 to High(phantoms) do
			if phantoms[i] = ph then
			begin
				phantoms[i] := phantoms[High(phantoms)];
				SetLength(phantoms, length(phantoms) - 1);
			{$ifdef Debug} ok := yes; {$endif}
				break;
			end;

		if TypeOf(ph^) = TypeOf(tWater) then
		begin
		{$ifdef Debug} ok := no; {$endif}
			for i := 0 to High(water) do
				if pRigidBody(water[i]) = ph then
				begin
					water[i] := water[High(water)];
					SetLength(water, length(water) - 1);
				{$ifdef Debug} ok := yes; {$endif}
					break;
				end;
		end;

	{$ifdef Debug} Assert(ok); {$endif}
	end;

	procedure RigidBody.tPhantomSpecific.CallOnPhantom(what: pMultiDelegate; b0, b1: pRigidBody);
	var
		args: tOnPhantomArgs;
	begin
		Assert(not what^.Empty);
		args.b0 := b0;
		args.b1 := b1;
		what^.Call(@_CallOnPhantom, @args);
	end;

	procedure RigidBody.tPhantomSpecific.Initialize(fully: boolean);
	begin
		if fully then
		begin
			onEnter.Init;
			onExit.Init;
		end;
		inner.Init;
	end;

	procedure RigidBody.tPhantomSpecific.Finalize(var body: RigidBody);
	begin
		ForceInnersToQuit(body, yes);
		inner.Done;
		onEnter.Done;
		onExit.Done;
	end;

	procedure InvalidateInner(var inner: RigidBody.PhantomInnerRec);
	begin
		inner.ensured := no;
	end;

	procedure RigidBody.tPhantomSpecific.InvalidateInners;
	begin
		inner.ForEach(@InvalidateInner);
	end;

	function BodyMatches(const inner: RigidBody.PhantomInnerRec; param: pointer): boolean;
	begin
		result := inner.body = param;
	end;

	procedure RigidBody.tPhantomSpecific.ConfirmInner(var body: RigidBody; world: pPhysWorld; b2: pRigidBody);
	var
		i: uint;
		e: ^PhantomInnerRec;
	begin
		Assert(b2^._kind = rigid_DynamicBody);
		if inner.Find(@BodyMatches, b2, i) then
		begin
			inner.items[i].ensured := yes;
			exit;
		end;

		e := inner.Grow(1);
		e^.body := MakeRef(b2);
		e^.ensured := yes;
		b2^.asRigid^.AddPhantom(@body);
		if not onEnter.Empty then
			world^._callbacksQueue.AddPhantom(@onEnter, @body, b2);
		if (b2^._kind = rigid_DynamicBody) and (not b2^.asRigid^.onEnterPhantom.Empty) then
				world^._callbacksQueue.AddPhantom(@b2^.asRigid^.onEnterPhantom, b2, @body);
	end;

	procedure RigidBody.tPhantomSpecific.RemoveUnconfirmedInners(var body: RigidBody; world: pPhysWorld);
	var
		i: sint;
	begin
		i := inner.n;
		while i > 0 do
		begin
			dec(i);
			if not inner.items[i].ensured then
			begin
				if not onExit.Empty then
					world^._callbacksQueue.AddPhantom(@onExit, @body, inner.items[i].body);
				if (inner.items[i].body^._kind = rigid_DynamicBody) and (not inner.items[i].body^.asRigid^.onExitPhantom.Empty) then
					world^._callbacksQueue.AddPhantom(@inner.items[i].body^.asRigid^.onExitPhantom, inner.items[i].body, @body);
				inner.items[i].body^.asRigid^.RemovePhantom(@body);
				Release(inner.items[i].body);
				inner.RemoveReplace(i);
			end;
		end;
	end;

	procedure RigidBody.tPhantomSpecific.ForceInnersToQuit(var body: RigidBody; emergency: boolean = no);
	var
		i: uint;
	begin
		if not emergency then
		begin
			i := inner.n;
			while i > 0 do
			begin
				dec(i);
				if not onExit.Empty then
					CallOnPhantom(@onExit, @body, inner.items[i].body);
				if (inner.items[i].body^._kind = rigid_DynamicBody) and not (inner.items[i].body^.asRigid^.onExitPhantom.Empty) then
					CallOnPhantom(@inner.items[i].body^.asRigid^.onExitPhantom, inner.items[i].body, @body);
			end;
		end;
		for i := 1 to inner.n do
		begin
			inner.items[i-1].body^.asRigid^.RemovePhantom(@body);
			Release(inner.items[i-1].body);
		end;
		inner.Clear;
	end;

	procedure RigidBody.tPhantomSpecific.Pump(var body: RigidBody; b2: pRigidBody);
	begin
		if not onExit.Empty then
			CallOnPhantom(@onExit, @body, b2);
		if not onEnter.Empty then
			CallOnPhantom(@onEnter, @body, b2);
	end;

	procedure RigidBody._CommonInit(newKind: tKind; newPrim: pRigidPrimitive);
	begin
		_kind := newKind;
		_newt := nil;
		_world := nil;
		worldId1 := 0;
		_primitive := MakeRef(newPrim);
		_interaction.Init;
		_flags := [];
		case _kind of
			rigid_DynamicBody:
				begin
					new(_specific.asRigid);
					_specific.asRigid^.Initialize(yes);
				end;
			rigid_Phantom:
				begin
					new(_specific.asPhantom);
					_specific.asPhantom^.Initialize(yes);
				end;
		end;
	end;

	constructor RigidBody.Init(newPrim: pRigidPrimitive; const newMass: float);
	begin
		if not Assigned(newPrim) then newPrim := new(pRigidPrimitive, InitNull);
		inherited Init;
		_CommonInit(rigid_DynamicBody, newPrim);
		mass := newMass;
		Assert(NotZero(mass), 'use INFINITE_MASS instead of zero');
	end;

	constructor RigidBody.InitPhantom(newPrim: pRigidPrimitive);
	begin
		if not Assigned(newPrim) then Fail;
		inherited Init;
		_CommonInit(rigid_Phantom, newPrim);
	end;

	destructor RigidBody.Done;
	var
		it: tHash_Body2Interaction.Iterator;
	begin
		it := _interaction.GetIterator;
		while _interaction.Next(it) do
			_interaction.GetKey(it)^^._interaction.Remove(@self);
		_interaction.Done;

		case _kind of
			rigid_DynamicBody:
				begin
					_specific.asRigid^.Finalize(self);
					dispose(_specific.asRigid);
				end;
			rigid_Phantom:
				begin
					_specific.asPhantom^.Finalize(self);
					dispose(_specific.asPhantom);
				end;
		end;
		_DestroyNewt;
		Release(_primitive);
		inherited Done;
	end;

	procedure RigidBody._SetWorld(theWorld: pPhysWorld);
	begin
		Assert(Assigned(theWorld));
		Assert((_world = nil) or (_world = theWorld), 'attempt to move body between different worlds');
		if _world = nil then
		begin
			_world := theWorld;
			case _kind of
				rigid_DynamicBody: asRigid^.UpdateMaterial(self, _world);
			end;
		end;
	end;

{$ifdef Debug}
	function RigidBody._GetRigidSpecific: pRigidSpecific;
	begin
		Assert(_kind = rigid_DynamicBody);
		result := _specific.asRigid;
	end;

	function RigidBody._GetPhantomSpecific: pPhantomSpecific;
	begin
		Assert(_kind = rigid_Phantom);
		result := _specific.asPhantom;
	end;
{$endif}

	procedure RigidBody._CreateNewt;
	begin
		if Assigned(_newt) then exit;
		_newt := Newton.CreateBody(_world^._newt, _primitive^._newt, globalTransform.ToMatrixWoScale);
		Newton.BodySetUserData(_newt, @self);
		case _kind of
			rigid_DynamicBody:
				begin
					asRigid^.UpdateMaterial(self, _world);
					_UpdateNewtonMass;
					_UpdateDamping;
					_UpdateSpecials([Low(RigidBodyFlag) .. High(RigidBodyFlag)]);
					if Assigned(asRigid^.tmpNewt) then
					begin
						Velocity := asRigid^.tmpNewt^.velocity;
						Omega := asRigid^.tmpNewt^.omega;
						dispose(asRigid^.tmpNewt, Done); asRigid^.tmpNewt := nil;
					end;
				end;
			rigid_Phantom: ;
		end;
	end;

	procedure RigidBody._DestroyNewt;
	begin
		if Assigned(_newt) then
		begin
			Newton.DestroyBody(_world^._newt, _newt);
			_newt := nil;
		end;
	end;

	procedure RigidBody._PreUpdate;
	begin
		case _kind of
			rigid_DynamicBody:
				if [rigid_InfiniteRotIX .. rigid_InfiniteRotIZ] * _flags <> [] then
					asRigid^.oldRotEul := LocalRot.ToEuler;
			rigid_Phantom: asPhantom^.InvalidateInners;
		end;
	end;

	procedure RigidBody._PostUpdate;
	var
		eul: Vec3;
	begin
		case _kind of
			rigid_DynamicBody:
				begin
					if [rigid_InfiniteRotIX .. rigid_InfiniteRotIZ] * _flags <> [] then
					begin
						eul := LocalRot.ToEuler;
						if rigid_InfiniteRotIX in _flags then eul.y := asRigid^.oldRotEul.y;
						if rigid_InfiniteRotIY in _flags then eul.x := asRigid^.oldRotEul.x;
						if rigid_InfiniteRotIZ in _flags then eul.z := asRigid^.oldRotEul.z;
						LocalRot := Quaternion.FromEuler(eul);
					end;
					asRigid^.selfForce := Vec3.Zero;
					asRigid^.selfTorque := Vec3.Zero;
				end;
			rigid_Phantom: asPhantom^.RemoveUnconfirmedInners(self, _world);
		end;
	end;

	function RigidBody._GetMaterial: PoolString;
	begin
		result := asRigid^.matName;
	end;

	procedure RigidBody._SetMaterial(const newMat: PoolString);
	begin
		if asRigid^.matName <> newMat then
		begin
			asRigid^.matName := newMat;
			asRigid^.UpdateMaterial(self, _world);
		end;
	end;

	procedure RigidBody._SetPrimitive(newPrim: pRigidPrimitive);
	begin
		if not Assigned(newPrim) then newPrim := RigidPrimitive.Merge(new(pRigidPrimitive, InitNull), yes);;
		if newPrim <> _primitive then
		begin
			SetRef(_primitive, RigidPrimitive.Merge(newPrim, no));
			if Assigned(_newt) then
			begin
				Newton.BodySetCollision(_newt, _primitive^._newt);
				_UpdateNewtonMass;
			end;
		end;
	end;

	function RigidBody._GetFlag(flag: RigidBodyFlag): boolean;
	begin
		result := flag in _flags;
	end;

	procedure RigidBody._SetFlag(flag: RigidBodyFlag; newValue: boolean);
	begin
		if newValue <> (flag in _flags) then
		begin
			if newValue then
				Include(_flags, flag)
			else
				Exclude(_flags, flag);
			_UpdateSpecials([flag]);
		end;
	end;

	procedure RigidBody._UpdateSpecials(const targetFlags: RigidBodyFlags);
	begin
		if not Assigned(_newt) then exit;
		if rigid_Sleepless in targetFlags then
			Newton.BodySetAutoSleep(_newt, uint(not (rigid_Sleepless in _flags)));
		if [rigid_InfiniteRotIX .. rigid_InfiniteRotIZ] * targetFlags <> [] then
			_UpdateNewtonMass;
	end;

	procedure RigidBody._UpdateDamping;
	var
		lin: float;
		ang: Vec3;
		i: sint;
	begin
		lin := _world^.EnvResistance;
		ang := Vec3.Make(_world^.EnvResistance);
		for i := 0 to High(asRigid^.water) do
		begin
			lin += asRigid^.water[i]^.LinearViscosity;
			ang += Vec3.Make(asRigid^.water[i]^.AngularViscosity);
		end;

		Newton.BodySetLinearDamping(_newt, lin);
		Newton.BodySetAngularDamping(_newt, ang);
	end;

	function RigidBody.Interaction(b2: pRigidBody): pRigidInteraction;
	begin
		Assert(b2 <> @self, 'Rigid body cannot interact with itself');
		Assert((not Assigned(_world)) or (not Assigned(b2^._world)) or (_world = b2^._world),
			'bodies belonging to the different worlds');
		result := _interaction.Find(b2);
		if Assigned(result) then exit;
		result := new(pRigidInteraction, Init);
		_interaction.Add(b2, result^.NewRef);
		b2^._interaction.Add(@self, result^.NewRef);
	end;

	function RigidBody.ClosestPoint(const p: Vec3; out point, normal: Vec3): boolean;
	begin
		result := _primitive^.ClosestPoint(_world, globalTransform, p, point, normal);
	end;

	procedure RigidBody.PumpPhantoms;
	var
		i: sint;
	begin
		for i := 0 to High(asRigid^.phantoms) do
			asRigid^.phantoms[i]^.asPhantom^.Pump(asRigid^.phantoms[i]^, @self);
	end;

	procedure RigidBody._AddJoint(j: pPhysJoint);
	begin
		if not Assigned(j) then exit;
		Assert(_kind = rigid_DynamicBody, 'only dynamic bodies may be jointed');
		SetLength(asRigid^.joints, length(asRigid^.joints) + 1);
		asRigid^.joints[High(asRigid^.joints)] := j^.NewRef;
	end;

	procedure RigidBody._RemoveJoint(j: pPhysJoint);
	var
		i: sint;
	begin
		Assert(_kind = rigid_DynamicBody);
		if _kind <> rigid_DynamicBody then exit;
		for i := 0 to High(asRigid^.joints) do
			if asRigid^.joints[i] = j then
			begin
				Release(asRigid^.joints[i]);
				asRigid^.joints[i] := asRigid^.joints[High(asRigid^.joints)];
				SetLength(asRigid^.joints, length(asRigid^.joints) - 1);
				break;
			end;
	end;

	function RigidBody._GetVelocity: Vec3;
	var
		nv: Newton.Vec3;
	begin
		if Assigned(_newt) then
		begin
			Newton.BodyGetVelocity(_newt, nv);
			result := Newton.From(nv);
		end else
			if Assigned(asRigid^.tmpNewt) then
				result := asRigid^.tmpNewt^.velocity
			else
				result := Vec3.Zero;
	end;

	procedure RigidBody._SetVelocity(const vel: Vec3);
	begin
		if Assigned(_newt) then
			Newton.BodySetVelocity(_newt, vel)
		else
			if Assigned(asRigid^.tmpNewt) or (vel <> Vec3.Zero) then
				asRigid^.EnsureTmpNewt^.velocity := vel;
	end;

	function RigidBody._GetOmega: Vec3;
	var
		nv: Newton.Vec3;
	begin
		if Assigned(_newt) then
		begin
			Newton.BodyGetOmega(_newt, nv);
			result := Newton.From(nv);
		end else
			if Assigned(asRigid^.tmpNewt) then
				result := asRigid^.tmpNewt^.omega
			else
				result := Vec3.Zero;
	end;

	procedure RigidBody._SetOmega(const om: Vec3);
	begin
		if Assigned(_newt) then
			Newton.BodySetOmega(_newt, om)
		else
			if Assigned(asRigid^.tmpNewt) or (om <> Vec3.Zero) then
				asRigid^.EnsureTmpNewt^.omega := om;
	end;

	function RigidBody._GetMass: float;
	begin
		result := asRigid^.mass;
	end;

	procedure RigidBody._SetMass(const newMass: float);
	begin
		if not Equals(asRigid^.mass, newMass) then
		begin
			asRigid^.mass := newMass;
			if Assigned(_newt) then _UpdateNewtonMass;
		end;
	end;

	procedure RigidBody._UpdateNewtonMass;
	var
		nmass: float;
		t: Vec3;
	begin
		if asRigid^.mass <> INFINITE_MASS then
		begin
			nmass := asRigid^.mass;
			t := _primitive^.InertiaMoment;
			if rigid_ForceInertiaMoment1 in _flags then t := Vec3.Make(1.0);
			t *= nmass;
			if rigid_InfiniteRotIX in _flags then t.x := NearInfinity;
			if rigid_InfiniteRotIY in _flags then t.y := NearInfinity;
			if rigid_InfiniteRotIZ in _flags then t.z := NearInfinity;
		end else
		begin
			nmass := 0.0;
			t := Vec3.Make(NearInfinity);
		end;
		Newton.BodySetMassMatrix(_newt, nmass, t.x, t.y, t.z);
		Newton.BodySetCentreOfMass(_newt, _primitive^._massCenter);
	end;

	function RigidBody._GetSelfForce: Vec3;
	begin
		result := asRigid^.selfForce;
	end;

	procedure RigidBody._SetSelfForce(const newForce: Vec3);
	begin
		asRigid^.selfForce := newForce;
	end;

	function RigidBody._GetSelfTorque: Vec3;
	begin
		result := asRigid^.selfTorque;
	end;

	procedure RigidBody._SetSelfTorque(const newTorque: Vec3);
	begin
		asRigid^.selfTorque := newTorque;
	end;

	procedure RigidBody._AfterAttach;
	var
		i: sint;
	begin
		inherited _AfterAttach;
		_SetWorld(pScene(parent^.Root)^.phys);
		_world^.AddBody(@self);
		case _kind of
			rigid_DynamicBody:
				for i := 0 to High(asRigid^.joints) do
					asRigid^.joints[i]^._CreateNewt;
		end;
	end;

	procedure RigidBody._BeforeDetach;
	var
		i: sint;
		tn: pTmpNewtonOpts;
	begin
		case _kind of
			rigid_DynamicBody:
				begin
					if (Velocity <> Vec3.Zero) or (Omega <> Vec3.Zero) then
					begin
						tn := asRigid^.EnsureTmpNewt;
						tn^.velocity := Velocity;
						tn^.omega := Omega;
					end;
					for i := 0 to High(asRigid^.joints) do
						asRigid^.joints[i]^._DestroyNewt;
				end;
			rigid_Phantom: asPhantom^.ForceInnersToQuit(self);
		end;
		_world^.RemoveBody(@self);
		inherited _BeforeDetach;
	end;

	procedure RigidBody._OnApplyTransform;
	begin
		inherited _OnApplyTransform;
		if Assigned(_newt) then
			Newton.BodySetMatrix(_newt, globalTransform.ToMatrixWoScale);
	end;

	procedure tPhysJoint._CreateNewt;
	begin
		if (not Assigned(_newt)) and (Assigned(_parent^.parent)) and (Assigned(_child^.parent)) then
			_CreateNewt_impl;
	end;

	procedure tPhysJoint._DestroyNewt;
	begin
		if Assigned(_newt) then
		begin
			Newton.DestroyJoint(_world^._newt, _newt);
			_newt := nil;
			end;
	end;

	constructor tPhysJoint.Init(newKind: tPhysJointKind; newParent, newChild: pRigidBody; const newPivot: Vec3);
	begin
		if (not Assigned(newParent)) or (not Assigned(newChild)) or (not Assigned(newParent^._world)) then Fail;
		Assert(newParent^._world = newChild^._world, 'bodies belonging to the different worlds');
		inherited Init;
		_world := newParent^._world;
		self._parent := newParent;
		self._child := newChild;
		self._kind := newKind;
		self._pivot := newPivot;
		self._newt := nil;
		_parent^._AddJoint(@self);
		_child^._AddJoint(@self);
		_CreateNewt;
	end;

	destructor tPhysJoint.Done;
	begin
		inherited Done;
	end;

	procedure tPhysJoint.Break;
	begin
		if Assigned(_parent) then _parent^._RemoveJoint(@self);
		if Assigned(_child) then _child^._RemoveJoint(@self);
		_DestroyNewt;
	end;

	procedure tPhysBallJoint._CreateNewt_impl;
	begin
		_newt := Newton.ConstraintCreateBall(_world^._newt, _parent^.globalTransform * _pivot, _child^._newt, _parent^._newt);
		_UpdateNewt;
	end;

	procedure tPhysBallJoint._UpdateNewt;
	begin
		if Assigned(_newt) then
			Newton.BallSetConeLimits(_newt, {_parent^.WorldRot * }_axis, _maxCone, _maxTwist);
	end;

	procedure tPhysBallJoint._SetAxis(const newAxis: Vec3);
	begin
		if _axis <> newAxis then
		begin
			_axis := newAxis;
			_UpdateNewt;
		end;
	end;

	procedure tPhysBallJoint._SetMaxCone(newMaxCone: float);
	begin
		if _maxCone <> newMaxCone then
		begin
			_maxCone := newMaxCone;
			_UpdateNewt;
		end;
	end;

	procedure tPhysBallJoint._SetMaxTwist(newMaxTwist: float);
	begin
		if _maxTwist <> newMaxTwist then
		begin
			_maxTwist := newMaxTwist;
			_UpdateNewt;
		end;
	end;

	constructor tPhysBallJoint.Init(newParent, newChild: pRigidBody; const newPivot: Vec3);
	begin
		inherited Init(joint_Ball, newParent, newChild, newPivot);
		self._axis := Vec3.PositiveX;
		self._maxCone := 0.0;
		self._maxTwist := 0.0;
	end;

	destructor tPhysBallJoint.Done;
	begin
		inherited Done;
	end;

	function tWater._SuitsTo(know: SceneKnowledge): boolean;
	begin
		case know of
			scene_Water: result := yes;
			else
				result := inherited _SuitsTo(know);
		end;
	end;

	constructor tWater.Init(newPrim: pRigidPrimitive);
	begin
		inherited InitPhantom(newPrim);
		_planeNormal := Vec3.PositiveY;
		_density := 0.8;
		_linearViscosity := 0.8;
		_angularViscosity := 0.8;
	end;

	destructor tWater.Done;
	begin
		inherited Done;
	end;

	function tWater.Contains(const pt: Vec3; const radius: float): boolean;
	var
		aabb: UMath.AABB;
	begin
		aabb := NewtonGetAABB(_newt);
		aabb.Grow(1.0, radius);
		result := aabb.Contains(pt);
	end;

	function tWater.Plane: Plane;
	begin
		result := UMath.Plane.Make(globalTransform * Vec3.Zero, globalTransform.rot * _planeNormal);
	end;

	procedure CallbacksQueue.AddCollide(proc: pMultiDelegate; b0, b1: pRigidBody; const contact: ContactInfo);
	var
		c: tCallbackRec;
	begin
		c.kind := callback_Collide;
		c.proc := proc;
		c.b0 := MakeRef(b0);
		c.b1 := MakeRef(b1);
		c.contact := contact;
		qu.Put(c);
	end;

	procedure CallbacksQueue.AddPhantom(proc: pMultiDelegate; b0, b1: pRigidBody);
	var
		c: tCallbackRec;
	begin
		c.kind := callback_Phantom;
		c.proc := proc;
		c.b0 := MakeRef(b0);
		c.b1 := MakeRef(b1);
		qu.Put(c);
	end;

	procedure CallbacksQueue.Init;
	begin
		qu.Init(0);
	end;

	procedure CallbacksQueue.Done;
	var
		c: tCallbackRec;
	begin
		while qu.Get(c) do
		begin
			Release(c.b0);
			Release(c.b1);
		end;
		qu.Done;
	end;

	procedure CallbacksQueue.ProcessAll;
	var
		c: tCallbackRec;
	begin
	trace_call('CallbacksQueue.ProcessAll');
		while qu.Get(c) do
		begin
			case c.Kind of
				callback_Collide: c.b0^.asRigid^.CallOnCollide(c.b0^, c.b1, c.contact);
				callback_Phantom: RigidBody.tPhantomSpecific.CallOnPhantom(c.proc, c.b0, c.b1);
			end;
			Release(c.b1);
			Release(c.b0);
		end;
leave_call
	end;

	procedure PhysWorld.AddBody(b: pRigidBody);
	begin
		Assert(Assigned(b) and (b^.worldId1 = 0));
		bodies.Push(b^.NewRef);
		b^.worldId1 := bodies.n;
		b^._CreateNewt;
		Newton.BodySetForceAndTorqueCallback(b^._newt, @_NewtonApplyForceAndTorque);
		Newton.BodySetTransformCallback(b^._newt, @_NewtonBodyTransformCallback);
	end;

	procedure PhysWorld.RemoveBody(b: pRigidBody);
	begin
		Assert(Assigned(b) and (b^.worldId1 >= 1) and (b^.worldId1 - 1 < bodies.n));
		Assert(bodies.items[b^.worldId1 - 1] = b);
		b^._DestroyNewt;
		bodies.Last^.worldId1 := b^.worldId1;
		bodies.RemoveReplace(b^.worldId1 - 1);
		b^.worldId1 := 0;
		Release(b);
	end;

	procedure UpdateBodyEnvResistance(body: pRigidBody);
	begin
		if body^.Kind = rigid_DynamicBody then body^._UpdateDamping;
	end;

	procedure PhysWorld._SetEnvResistance(newResistance: float);
	begin
		if Equals(_envResistance, newResistance) then exit;
		_envResistance := newResistance;
		bodies.ForEach(@UpdateBodyEnvResistance);
	end;

	procedure UpdateBodyMaterial(body: pRigidBody; world: pPhysWorld);
	begin
		if body^.kind = rigid_DynamicBody then body^.asRigid^.UpdateMaterial(body^, world);
	end;

	procedure PhysWorld._SetMaterialDB(newDb: pPhysMaterialDB);
	var
		i: uint;
		int_it: PhysMaterialDB.InteractionSet.Iterator;
		int: PhysMaterialDB.pInteraction;
		a, b: sint;
	begin
		if not Assigned(newDb) then exit;
		Assert(not Assigned(_materialDb), 'Cannot change material DB twice.');
		SetRef(_materialDb, newDb);

		SetLength(_mid2newt, _materialDb^.MaterialsCount);
		_mid2newt[0] := Newton.MaterialGetDefaultGroupID(_newt);
		i := 1;
		while i < uint(length(_mid2newt)) do
		begin
			_mid2newt[i] := Newton.MaterialCreateGroupID(_newt);
			inc(i);
		end;

		int_it := _materialDB^._ints.GetIterator;
		while _materialDB^._ints.Next(int_it) do
		begin
			int := _materialDB^._ints.GetKey(int_it);
			a := _mid2newt[int^.IdA(_materialDB^)];
			b := _mid2newt[int^.IdB(_materialDB^)];
			Newton.MaterialSetDefaultCollidable(_newt, a, b, uint(int^.collidable));
			Newton.MaterialSetDefaultFriction  (_newt, a, b, int^.staticFriction, int^.kineticFriction);
		end;
		for a := 0 to High(_mid2newt) do
			for b := 0 to High(_mid2newt) do
			begin
				int := _materialDB^.SuitableInteraction(a, b);
				Assert(Assigned(int));
				Newton.MaterialSetCollisionCallback(_newt, _mid2newt[a], _mid2newt[b], int, @_NewtonPreCollide, @_NewtonCollide);
			end;

		bodies.ForEach(BodiesList.ValueItemProc(@UpdateBodyMaterial), @self);
	end;

	function PhysWorld.OnMaterialCollide(const a, b: string): pMultiDelegate;
	var
		ia, ib: sint;
		code: uint;
	begin
		result := nil;
		if not Assigned(_materialDb) then exit;
		ia := _materialDb^.GetMaterialID(a); if ia < 0 then exit;
		ib := _materialDb^.GetMaterialID(b); if ib < 0 then exit;
		code := PhysMaterialDB._Mats2Code(ia, ib);
		result := _int2onCollide.Find(code);
		if not Assigned(result) then
		begin
			new(result); result^.Init;
			_int2onCollide.Add(code, result);
		end;
	end;

	constructor PhysWorld.Init;
	var
		nThreadsMax: sint;
	begin
		try
			Newton.loader.Load;
		except
			instant_reraise_from_constructor;
		end;

		inherited Init;
	{$ifdef Debug} LogR('Создание Ньютоновского мира... '); {$endif}
		_newt := Newton.WorldCreate();
		if not Assigned(_newt) then raise Error('Не удалось создать Ньютоновский мир');

		Newton.SetSolverModel(_newt, Config.solverModel);
		Newton.SetFrictionModel(_newt, Config.frictionModel);
		Newton.SetPlatformArchitecture(_newt, Config.fpext);
		nThreadsMax := Newton.GetMaxThreadsCount(_newt);
		if Config.nThreads >= 1 then
		begin
			Config.nThreads := min(Config.nThreads, nThreadsMax);
			Newton.SetThreadsCount(_newt, Config.nThreads);
		end;
		Config.nThreads := Newton.GetThreadsCount(_newt);

	{$ifdef Debug}
		LogR('Версия Newton: ' + ToString(Newton.WorldGetVersion(_newt)) + '; ' +
			ToString(8*sizeof(Newton.Float)) + '-битная точность; модель солвера: ' +
			ToString(Config.solverModel) + '; модель трения: ' + ToString(Config.frictionModel) + '; ' +
			'архитектура платформы: ' + ToString(Config.fpext) + '; потоков: ' + ToString(Config.nThreads) + '; ' +
			'макс. потоков: ' + ToString(nThreadsMax) + '; ', logDebug);
		Log('Создан Ньютоновский мир', logOK);
	{$endif}

		Resize(AABB.Make(Vec3.Make(-1e6), Vec3.Make(1e6)));

		_prevTime := Ticks.Zero;
		bodies.Init;
	{$ifdef Debug}
		_nQueries := 0;
		_queriesTimeSum := Ticks.Zero;
		_lastQueryReport := Ticks.Get;
	{$endif}
		_materialDb := nil;
		_mid2newt := nil;
		_int2onCollide.Init;

		_accDt := 0.0;
		_envResistance := 0.1;
		_callbacksQueue.Init;

		gravity := Vec3.Zero;
		gravityEnabled := yes;
	end;

	destructor PhysWorld.Done;
	begin
		if instantly_reraised_from_constructor then exit;
		_callbacksQueue.Done;

		_int2onCollide.Done;
		Release(_materialDb);

		bodies.Done;
	{$ifdef Debug} LogR('Уничтожение Ньютоновского мира... '); {$endif}
		if TempWorld = _newt then TempWorld := nil;
		Newton.WorldDestroy(_newt);
	{$ifdef Debug} Log('Ньютоновский мир уничтожен', logOK); {$endif}

		inherited Done;
		Newton.loader.Unload;
	end;

	procedure PhysWorld._PreUpdate;
	const
		Dniwe = -300.0;
	var
		i: uint;
		bb, curBB: AABB;
		first: boolean;
	begin
	trace_call('PhysWorld._PreUpdate');
		first := yes;
		for i := 1 to bodies.n do
		begin
			bodies.items[i-1]^._PreUpdate;
			curBB := NewtonGetAABB(bodies.items[i-1]^._newt);
			if first then
			begin
				first := no;
				bb := curBB;
			end else
				bb.Enlarge(curBB);
		end;

		if not first then
		begin
			bb.Grow(1.5, 5.0);
			bb.A.y := max(bb.A.y, Dniwe);
			bb.B.y := max(bb.B.y, Dniwe);
			Resize(bb);
		end;
	leave_call
	end;

	procedure PhysWorld._PostUpdate;
	var
		i: uint;
	begin
	trace_call('PhysWorld._PostUpdate');
		for i := 1 to bodies.n do
			bodies.items[i-1]^._PostUpdate;
	leave_call
	end;

	procedure PhysWorld.Update;
	var
		dt: float;
		i: sint;
		newTime: Ticks;
	begin
	trace_call('PhysWorld.Update');
		gravityEnabled := not Config.idclip;

		newTime := Ticks.Get;
		if _prevTime <> Ticks.Zero then
			dt := min((newTime - _prevTime).ToSeconds, MultimediaSystem.MaxFrameDt)
		else
			dt := 0.0;
		_prevTime := newTime;
		_accDt += dt;

		if _accDt >= MIN_PHYS_DT then
		begin
			_PreUpdate;
			if _accDt >= MAX_PHYS_DT then
			begin
				for i := 1 to min(trunc(_accDt / MAX_PHYS_DT), MAX_PHYS_ITERATIONS) do
					Newton.Update(_newt, MAX_PHYS_DT);
				_accDt := modf(_accDt, MAX_PHYS_DT);
			end;
			if _accDt >= MIN_PHYS_DT then
			begin
				Newton.Update(_newt, _accDt);
				_accDt := 0.0;
			end;
			_PostUpdate;
			_callbacksQueue.ProcessAll;
		end;
	leave_call
	end;

	procedure PhysWorld.Resize(const aabb: AABB);
	begin
		Newton.SetWorldSize(_newt, aabb.A, aabb.B);
	end;

type
	pNewtonBodyIteratorParams = ^tNewtonBodyIteratorParams;
	tNewtonBodyIteratorParams = record
		proc: RigidBodyProc;
		param: pointer;
	end;

	procedure __NewtonBodyIterator(body: Newton.pBody; userData: pointer); cdecl;
	var
		p: pNewtonBodyIteratorParams absolute userData;
	begin
		p^.proc(Newton.BodyGetUserData(body), p^.param);
	end;

	procedure PhysWorld.Query(const ibnd: Bounding; proc: RigidBodyProc; param: pointer);
	var
		p: tNewtonBodyIteratorParams;
	{$ifdef Debug} dt, time: Ticks; {$endif}
	begin
	{$ifdef Debug} time := Ticks.Get; {$endif}
		p.proc := proc;
		p.param := param;
		Newton.WorldForEachBodyInAABBDo(_newt, ibnd.AABB.A, ibnd.AABB.B, @__NewtonBodyIterator, @p);
	{$ifdef Debug}
		_queriesTimeSum += Ticks.Get - time;
		inc(_nQueries);
		dt := Ticks.Get - _lastQueryReport;
		if dt >= Ticks.FromSeconds(4.0) then
		begin
			Log('PhysWorld.Query: ' + ToString(_nQueries) + ' шт. в течение ' +
				ToString(dt.ToSeconds) + ' с, в среднем по ' +
				ToString(_queriesTimeSum.ToMicroseconds / _nQueries) + ' мкс', logDebug);
			_lastQueryReport := Ticks.Get;
			_nQueries := 0;
			_queriesTimeSum := Ticks.Zero;
		end;
	{$endif}
	end;

type
	pRayFilterRec = ^tRayFilterRec;
	tRayFilterRec = record
		p1, p2: Vec3;
		prefilter: tRayCastPrefilter;
		proc: tRayCastCallback;
		param: pointer;
	end;

	function _NewtRayPrefilter(body: Newton.pBody; collision: Newton.pCollision; userData: pointer): cuint; cdecl;
	var
		r: pRayFilterRec absolute userData;
	begin
		Assert(@collision = @collision);
		result := uint(r^.prefilter(Newton.BodyGetUserData(body), r^.param));
	end;

{$hints off}
	function _NewtRayFilter(body: Newton.pBody; constref hitNormal: Newton.Vec3; collisionID: cint; userData: pointer; intersetParam: Newton.Float): Newton.Float; cdecl;
{$hints on}
	var
		r: pRayFilterRec absolute userData;
		b: pRigidBody;
	begin
		if collisionID <> MaterialID_Transparent then
		begin
			b := Newton.BodyGetUserData(body);
			result := r^.proc(b, intersetParam, r^.p1 * (1.0 - intersetParam) + r^.p2 * intersetParam,
				Vec3.Make(hitNormal[0], hitNormal[1], hitNormal[2]) {$ifdef RenormalizeVectors}.MaybeNormalized{$endif},
				r^.param);
		end;
	end;

	procedure PhysWorld.RayCast(const p1, p2: Vec3; prefilter: tRayCastPrefilter; proc: tRayCastCallback; param: pointer);
	var
		r: tRayFilterRec;
		newtPrefilter: Newton.WorldRayPrefilterCallback;
	begin
		r.p1 := p1;
		r.p2 := p2;
		r.proc := proc;
		r.prefilter := prefilter;
		r.param := param;
		if Assigned(prefilter) then
			newtPrefilter := @_NewtRayPrefilter
		else
			newtPrefilter := nil;
		Newton.WorldRayCast(_newt, p1, p2, @_NewtRayFilter, @r, newtPrefilter);
	end;

	function PhysMemoryEaten: size_t;
	begin
		if Assigned(Newton.GetMemoryUsed) then
			result := Newton.GetMemoryUsed()
		else
			result := 0;
	end;

	procedure Script_CreatePhysJoint(var ss: ScriptState);
	var
		s: string;
		parent, child: pRigidBody;
		pivot: Vec3;
		jo: pPhysJoint;
		ball: pPhysBallJoint absolute jo;
	begin
		s := ss.ToString(1);
		parent := ss.ToObject(2, TypeOf(RigidBody));
		child := ss.ToObject(3, TypeOf(RigidBody));
		if (ss.Top >= 4) and (ss.ObjType(4) = ObjType_Vec[3]) then pivot := ss.ToVec3(4) else pivot := Vec3.Zero;
		jo := nil;
		if s = 'ball' then
		begin
			ball := new(pPhysBallJoint, Init(parent, child, pivot));
			if ss.Top >= 5 then ball^.Axis := ss.ToVec3(5);
			if ss.Top >= 6 then ball^.MaxCone := ss.ToFloat(6);
			if ss.Top >= 7 then ball^.MaxTwist := ss.ToFloat(7);
		end else
			ss.Throw('неизвестный тип джоинта: {0}', s);
		ss.PushObject(jo);
	end;

	procedure Script_PhysJoint_Break(var ss: ScriptState); begin pPhysJoint(ss.ToSelf)^.Break; end;

	procedure Script_CreateRigidBody(var ss: ScriptState);
	var
		prim: pRigidPrimitive;
		body: pRigidBody;
		kind: string;
	begin
		prim := nil;
		if ss.TryGetTableI(1, 1) then
		begin
			prim := Script_create_rigid_prim_ref(ss, -1);
			ss.Pop;
		end else
			prim := nil;
		if (not Assigned(prim)) and (ss.GetTableS(1, 'primitive')) then
		begin
			prim := Script_create_rigid_prim_ref(ss, -1);
			ss.Pop;
		end;
		if not Assigned(prim) then
			ss.Throw('для физического тела нужен примитив (prim)');
		kind := ss.GetStringField(1, 'kind');

		if kind = '' then body := new(pRigidBody, Init(prim, ss.GetFloatField(1, 'mass', INFINITE_MASS))) else
		if kind = 'phantom' then body := new(pRigidBody, InitPhantom(prim)) else
			ss.UnknownIdentifier(kind);
		Release(prim);

		body^.LocalTransform := ss.GetTransformField(1, 'tf');
		if ss.GetTableS(1, 'material') then
		begin
			body^.material := ss.ToString(-1);
			ss.Pop;
		end;
		if ss.GetTableS(1, 'gravity') then
		begin
			body^.UnaffectedByGravity := not ss.ToBool(-1);
			ss.Pop;
		end;
		if ss.GettableS(1, 'velocity') then
		begin
			body^.Velocity := ss.ToVec3(-1);
			ss.Pop;
		end;
		Script_common_create_scene_node(ss, body);
		ss.PushObject(body);
	end;

	procedure Script_CreateWater(var ss: ScriptState);
	var
		water: pWater;
		prim: pRigidPrimitive;
	begin
		if ss.GetTableS(1, 'primitive') then
		begin
			prim := Script_create_rigid_prim_ref(ss, -1);
			ss.Pop;
		end else
			ss.Throw('не задан primitive');

		water := new(pWater, Init(prim));
		Release(prim);
		if not Assigned(water) then
		begin
			ss.PushNil;
			exit;
		end;

		water^.PlaneNormal := ss.GetVec3Field(1, 'plane_normal', water^.PlaneNormal);
		water^.Density     := ss.GetFloatField(1, 'density', water^.Density);
		water^.LinearViscosity := ss.GetFloatField(1, 'linear_viscosity', water^.LinearViscosity);
		water^.AngularViscosity := ss.GetFloatField(1, 'angular_viscosity', water^.AngularViscosity);

		Script_common_create_scene_node(ss, water);
		ss.PushObject(water);
	end;

	procedure Script_RigidBody_mass(var ss: ScriptState; read: boolean);
	var
		rb: pRigidBody;
	begin
		rb := ss.ToSelf;
		if read then
			ss.PushFloat(rb^.Mass)
		else
			if ss.Typ(3) <> script_Nil then
				rb^.mass := ss.ToFloat(3)
			else
				rb^.Mass := INFINITE_MASS;
	end;

	procedure Script_RigidBody_force(var ss: ScriptState; read: boolean);
	begin
		if read then
			ss.PushVec3(pRigidBody(ss.ToSelf)^.selfForce)
		else
			pRigidBody(ss.ToSelf)^.selfForce := ss.ToVec3(3);
	end;

	procedure Script_RigidBody_torque(var ss: ScriptState; read: boolean);
	begin
		if read then
			ss.PushVec3(pRigidBody(ss.ToSelf)^.selfTorque)
		else
			pRigidBody(ss.ToSelf)^.selfTorque := ss.ToVec3(3);
	end;

	procedure Script_RigidBody_velocity(var ss: ScriptState; read: boolean);
	var
		rb: pRigidBody;
	begin
		rb := pRigidBody(ss.ToSelf);
		if read then
			ss.PushVec3(rb^.Velocity)
		else
			rb^.Velocity := ss.ToVec3(3);
	end;

	procedure Script_RigidBody_omega(var ss: ScriptState; read: boolean);
	var
		rb: pRigidBody;
	begin
		rb := pRigidBody(ss.ToSelf);
		if read then
			ss.PushVec3(rb^.Omega)
		else
			rb^.Omega := ss.ToVec3(3);
	end;

	procedure Script_RigidBody_material(var ss: ScriptState; read: boolean);
	var
		b: pRigidBody;
	begin
		b := ss.ToSelf;
		if read then
			ss.PushString(b^.Material)
		else
			b^.Material := ss.ToString(3);
	end;

	procedure Script_RigidBody_static(var ss: ScriptState);
	begin
		ss.PushBool(pRigidBody(ss.ToSelf)^.Mass = INFINITE_MASS);
	end;

	procedure Script_RigidBody_rigid(var ss: ScriptState);
	begin
		ss.PushBool(pRigidBody(ss.ToSelf)^.Kind = rigid_DynamicBody);
	end;

	procedure Script_RigidBody_sleepless(var ss: ScriptState; read: boolean);
	begin
		if read then
			ss.PushBool(pRigidBody(ss.ToSelf)^.Sleepless)
		else
			pRigidBody(ss.ToSelf)^.Sleepless := ss.ToBool(3);
	end;

	procedure Script_RigidBody_prim(var ss: ScriptState; read: boolean);
	var
		prim: pRigidPrimitive;
	begin
		if read then
			ss.PushObject(pRigidBody(ss.ToSelf)^.Primitive)
		else
		begin
			prim := Script_create_rigid_prim_ref(ss, 3);
			pRigidBody(ss.ToSelf)^.Primitive := prim;
			Release(prim);
		end;
	end;

	procedure Script_RigidBody_gravity(var ss: ScriptState; read: boolean);
	begin
		if read then
			ss.PushBool(not pRigidBody(ss.ToSelf)^.UnaffectedByGravity)
		else
			pRigidBody(ss.ToSelf)^.UnaffectedByGravity := not ss.ToBool(3);
	end;

	procedure Script_RigidBody_Interaction(var ss: ScriptState);
	begin
		ss.PushObject(pRigidBody(ss.ToSelf)^.Interaction(pRigidBody(ss.ToObject(2, TypeOf(RigidBody)))));
	end;

	function Script_RigidBody_LocalRayCast(var ss: ScriptState): sint;
	var
		pos, norm: Vec3;
		x: float;
	begin
		if pRigidBody(ss.ToSelf)^.Primitive^.RayCast(nil, ss.ToVec3(2), ss.ToVec3(3), @x, @pos, @norm) then
		begin
			ss.PushFloat(x);
			ss.PushVec3(pos);
			ss.PushVec3(norm);
			result := 3;
		end else
			result := 0;
	end;

	function Script_RigidBody_ClosestPoint(var ss: ScriptState): sint;
	var
		pos, norm: Vec3;
		r: boolean;
	begin
		r := pRigidBody(ss.ToSelf)^.ClosestPoint(ss.ToVec3(2), pos, norm);
		ss.PushVec3(pos);
		ss.PushVec3(norm);
		ss.PushBool(r);
		result := 3;
	end;

	procedure Script_RigidBody_ModifyEntangle(var ss: ScriptState);
	begin
		Script_modifiable(ss, 2, pRigidBody(ss.ToSelf)^.AsRigid^.entangle);
	end;

	procedure Script_RigidBody_PumpPhantoms(var ss: ScriptState); begin pRigidBody(ss.ToSelf)^.PumpPhantoms; end;

	procedure __OnCollide(b0, b1: pRigidBody; const contact: ContactInfo; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(tOnCollideProc(@__OnCollide) = @__OnCollide);
		if not sd^.GetFunction {$ifdef Debug}('OnCollide'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(b0);
			PushObject(b1);
			with contact do
			begin
				PushVec3(pos);
				PushVec3(norm);
				PushFloat(nSpeed);
				PushVec3(tang);
				PushFloat(tSpeed);
				PushVec3(binorm);
				PushFloat(bSpeed);
				Call(9, 0);
			end;
		end;
	end;

	procedure __OnPhantom(b0, b1: pRigidBody; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(tOnPhantomProc(@__OnPhantom) = @__OnPhantom);
		if not sd^.GetFunction {$ifdef Debug}('OnPhantom'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(b0);
			PushObject(b1);
			Call(2, 0);
		end;
	end;

	procedure Script_RigidBody_onCollide(var ss: ScriptState);
	var
		body: pRigidBody;
	begin
		body := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(body, @body^.asRigid^.onCollide, @__OnCollide)));
	end;

	procedure Script_RigidBody_onEnterPhantom(var ss: ScriptState);
	var
		body: pRigidBody;
	begin
		body := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(body, @body^.asRigid^.onEnterPhantom, @__OnPhantom)));
	end;

	procedure Script_RigidBody_onExitPhantom(var ss: ScriptState);
	var
		body: pRigidBody;
	begin
		body := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(body, @body^.asRigid^.onExitPhantom, @__OnPhantom)));
	end;

	procedure Script_RigidBody_onEnter(var ss: ScriptState);
	var
		body: pRigidBody;
	begin
		body := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(body, @body^.asPhantom^.onEnter, @__OnPhantom)));
	end;

	procedure Script_RigidBody_onExit(var ss: ScriptState);
	var
		body: pRigidBody;
	begin
		body := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(body, @body^.asPhantom^.onExit, @__OnPhantom)));
	end;

	procedure Script_RigidInteraction_collidable(var ss: ScriptState; read: boolean);
	begin
		if read then
			ss.PushBool(pRigidInteraction(ss.ToSelf)^.Collidable)
		else
			pRigidInteraction(ss.ToSelf)^.Collidable := ss.ToBool(3);
	end;

	procedure Script_PhysWorld_OnMaterialCollide(var ss: ScriptState);
	var
		world: pPhysWorld;
		cb: pMultiDelegate;
	begin
		world := ss.ToSelf;
		cb := world^.OnMaterialCollide(ss.ToString(2), ss.ToString(3));
		if Assigned(cb) then
			ss.PushObject(new(pScriptDelegateWrapper, Init(world, cb, @__OnCollide)))
		else
			ss.PushNil;
	end;

	procedure Script_PhysWorld_SetMaterialDB(var ss: ScriptState);
	var
		db: pPhysMaterialDB;
	begin
		db := ResourcePool.Shared^.LoadRef(TypeOf(PhysMaterialDB), ss.ToStream(3));
		pPhysWorld(ss.ToSelf)^.MaterialDB := db;
		Release(db);
	end;

	procedure Script_PhysWorld_gravity(var ss: ScriptState; read: boolean);
	begin
		if read then
			ss.PushVec3(pPhysWorld(ss.ToSelf)^.gravity)
		else
			pPhysWorld(ss.ToSelf)^.gravity := ss.ToVec3(3);
	end;

	procedure Script_PhysWorld_envResistance(var ss: ScriptState; read: boolean);
	begin
		if read then
			ss.PushFloat(pPhysWorld(ss.ToSelf)^.EnvResistance)
		else
			pPhysWorld(ss.ToSelf)^.EnvResistance := ss.ToFloat(3);
	end;

	function _RayCastPrefilter(body: pRigidBody; param: pointer): boolean;
	var
		ss: pScriptState absolute param;
	begin
		if not ss^.PushExisting(body) then exit(no);
		ss^.PushCopy(-2);
		ss^.Insert(-2);
		ss^.Call(1, 1);
		case ss^.Typ(-1) of
			script_Boolean: result := ss^.ToBool(-1);
			else ss^.Throw('RayCast.Prefilter должна вернуть yes/no');
		end;
		ss^.Pop(1);
	end;

	function _RayCastCallback(body: pRigidBody; const x: float; const point, normal: Vec3; param: pointer): float;
	var
		ss: pScriptState absolute param;
	begin
		if not ss^.PushExisting(body) then exit(x);
		ss^.PushCopy(-3);
		ss^.Insert(-2);
		ss^.PushFloat(x);
		ss^.PushVec3(point);
		ss^.PushVec3(normal);
		ss^.Call(4, 1);
		case ss^.Typ(-1) of
			script_Boolean: if ss^.ToBool(-1) then result := 1.0 else result := x;
			else ss^.Throw('RayCast.Callback должна вернуть yes/no');
		end;
		ss^.Pop(1);
	end;

	function Script_PhysWorld_RayCast(var ss: ScriptState): sint;
	var
		prefilter: tRayCastPrefilter;
	begin
		if ss.IsFunction(5) then
			prefilter := @_RayCastPrefilter
		else
			prefilter := nil;
		ss.PushCopy(4);
		if Assigned(prefilter) then ss.PushCopy(5) else ss.PushNil;
		pPhysWorld(ss.ToSelf)^.RayCast(ss.ToVec3(2), ss.ToVec3(3), prefilter, @_RayCastCallback, @ss);
		result := 0;
	end;

	function Script_create_rigid_prim_ref(var ss: ScriptState; idx: sint; partOfCompound: boolean = no): pRigidPrimitive;
	var
		kind: string;
		tf: Transform;
		prims: array of pRigidPrimitive;
		curPrim: pRigidPrimitive;
		i: sint;
	begin
		idx := ss.AbsIdx(idx);
		case ss.Typ(idx) of
			script_Table:
				begin
					tf := ss.GetTransformField(idx, 'transform');
					kind := ss.GetStringField(idx, 'kind');
					if kind = 'null' then result := new(pRigidPrimitive, InitNull) else
					if kind = 'box' then result := new(pRigidPrimitive, InitBox(ss.GetVec3Field(idx, 'sizes'), tf, partOfCompound)) else
					if kind = 'sphere' then
						if ss.HasField(idx, 'radius3') then
							result := new(pRigidPrimitive, InitSphere(ss.GetVec3Field(idx, 'radius3'), tf, partOfCompound))
						else
							result := new(pRigidPrimitive, InitSphere(ss.GetFloatField(idx, 'radius'), tf, partOfCompound))
					else
					if kind = 'capsule' then result := new(pRigidPrimitive, InitCapsule(ss.GetFloatField(idx, 'radius'), ss.GetFloatField(idx, 'height'), tf, partOfCompound)) else
					if kind = 'cylinder' then result := new(pRigidPrimitive, InitCylinder(ss.GetFloatField(idx, 'radius'), ss.GetFloatField(idx, 'height'), tf, partOfCompound)) else
					if kind = 'cone' then result := new(pRigidPrimitive, InitCone(ss.GetFloatField(idx, 'radius'), ss.GetFloatField(idx, 'height'), tf, partOfCompound)) else
					if kind = 'compound' then
					begin
						prims := nil;
						for i := 1 to ss.RawLen(idx) do
						begin
							ss.GetTableI(idx, i);
							curPrim := Script_create_rigid_prim_ref(ss, -1, yes);
							ss.Pop;
							if Assigned(curPrim) then
							begin
								SetLength(prims, length(prims) + 1);
								prims[High(prims)] := curPrim;
							end;
						end;
						result := new(pRigidPrimitive, InitCompound(prims));
						for i := 0 to High(prims) do
							Release(prims[i]);
					end else
					begin
					{$ifdef Debug} Log('Неизвестный примитив в CreateRigidPrim: kind = "' + kind + '"', logError); {$endif}
						exit;
					end;
					MakeRef(result);
				end;
			script_String: result := ResourcePool.Shared^.LoadRef(TypeOf(RigidPrimitive), ss.ToStream(idx));
			script_Object: result := MakeRef(ss.ToObject(idx, TypeOf(RigidPrimitive)));
			else ss.Throw('неверный RigidPrimitive');
		end;
		if not partOfCompound then RigidPrimitive.Merge(result);
	end;

	procedure OpenScript(var script: ScriptState);
	const
		Stuff: array[0 .. 37] of ScriptStuffDesc =
		(
			(s: TypeDesc; p: TypeOf(RigidPrimitive)),

			(s: TypeDesc; p: TypeOf(RigidBody)),
			(s: 'material' + Writeable; p: @Script_RigidBody_material),
			(s: 'mass' + Writeable; p: @Script_RigidBody_mass),
			(s: 'force' + Writeable; p: @Script_RigidBody_force),
			(s: 'torque' + Writeable; p: @Script_RigidBody_torque),
			(s: 'velocity' + Writeable; p: @Script_RigidBody_velocity),
			(s: 'omega' + Writeable; p: @Script_RigidBody_omega),
			(s: 'static'; p: @Script_RigidBody_static),
			(s: 'rigid'; p: @Script_RigidBody_rigid),
			(s: 'sleepless' + Writeable; p: @Script_RigidBody_sleepless),
			(s: 'prim' + Writeable; p: @Script_RigidBody_prim),
			(s: 'gravity' + Writeable; p: @Script_RigidBody_gravity),
			(s: 'onCollide'; p: @Script_RigidBody_onCollide),
			(s: 'onEnterPhantom'; p: @Script_RigidBody_onEnterPhantom),
			(s: 'onExitPhantom'; p: @Script_RigidBody_onExitPhantom),
			(s: 'onEnter'; p: @Script_RigidBody_onEnter),
			(s: 'onExit'; p: @Script_RigidBody_onExit),

			(s: 'Interaction:1'; p: @Script_RigidBody_Interaction),
			(s: 'LocalRayCast'; p: @Script_RigidBody_LocalRayCast),
			(s: 'ClosestPoint'; p: @Script_RigidBody_ClosestPoint),
			(s: 'ModifyEntangle:0'; p: @Script_RigidBody_ModifyEntangle),
			(s: 'PumpPhantoms:0'; p: @Script_RigidBody_PumpPhantoms),

			(s: TypeDesc; p: TypeOf(tRigidInteraction)),
			(s: 'collidable' + Writeable; p: @Script_RigidInteraction_collidable),

			(s: TypeDesc; p: TypeOf(PhysWorld)),
			(s: 'materialDB' + WriteOnly; p: @Script_PhysWorld_SetMaterialDB),
			(s: 'gravity' + Writeable; p: @Script_PhysWorld_gravity),
			(s: 'envResistance' + Writeable; p: @Script_PhysWorld_envResistance),

			(s: 'OnMaterialCollide:1'; p: @Script_PhysWorld_OnMaterialCollide),
			(s: 'RayCast'; p: @Script_PhysWorld_RayCast),

			(s: TypeDesc; p: TypeOf(tPhysJoint)),
			(s: 'Break:0'; p: @Script_PhysJoint_Break),

			(s: TypeDesc; p: TypeOf(tPhysBallJoint)),

			(s: TypeDesc; p: TypeOf(tWater)),

			(s: FunctionsDesc + 'CreatePhysJoint:1'; p: @Script_CreatePhysJoint),
			(s: 'CreateRigidBody:1' + RequireEnv; p: @Script_CreateRigidBody),
			(s: 'CreateWater:1' + RequireEnv; p: @Script_CreateWater)
		);
	begin
		script.AddStuff(Stuff);

	{$if defined(RenormalizeVectors) and defined(Debug)}
		Log('sizeof(float) > sizeof(Newton.Float) => полученные из Newton векторы будут ренормализованы!', logWarning);
	{$endif}
	end;

{$ifdef use_serialization}
	procedure SerializeRigidPrimitive(se: pSerializer; obj: pointer);
	var
		prim: pRigidPrimitive absolute obj;
	begin
		prim^.Serialize(se^.stream);
	end;

	procedure DeserializeRigidPrimitive(de: pDeserializer; obj: pointer);
	var
		prim: pRigidPrimitive absolute obj;
	begin
		prim^._Deserialize(de^.stream);
	end;

	procedure RigidPrimitiveDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		prim: pRigidPrimitive absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: prim^.Init(rigid_Null);
			de_After: prim := RigidPrimitive.Merge(prim, no);
		end;
	end;

const
	// Все биты занял D:
	RIGID_SLEEPLESS_BIT              = 1 shl 0;
	RIGID_UNAFFECTED_BY_GRAVITY_BIT  = 1 shl 1;
	RIGID_INFINITE_ROT_IX_BIT        = 1 shl 2;
	RIGID_INFINITE_ROT_IY_BIT        = 1 shl 3;
	RIGID_INFINITE_ROT_IZ_BIT        = 1 shl 4;
	RIGID_FORCE_INERTIA_MOMENT_1_BIT = 1 shl 5;
	RIGID_HAS_INTERACTIONS_BIT       = 1 shl 6;

	RIGID_RIGID_INFINITE_MASS_BIT      = 1 shl 7;       RIGID_PHANTOM_HAS_ONENTER_BIT = 1 shl 7;
	RIGID_RIGID_HAS_VELOCITY_BIT       = 1 shl 8;       RIGID_PHANTOM_HAS_ONEXIT_BIT  = 1 shl 8;
	RIGID_RIGID_HAS_OMEGA_BIT          = 1 shl 9;
	RIGID_RIGID_HAS_JOINTS_BIT         = 1 shl 10;
	RIGID_RIGID_HAS_ONCOLLIDE_BIT      = 1 shl 11;
	RIGID_RIGID_HAS_ONENTERPHANTOM_BIT = 1 shl 12;
	RIGID_RIGID_HAS_ONEXITPHANTOM_BIT  = 1 shl 13;
	RIGID_RIGID_HAS_ENTANGLE_BIT       = 1 shl 14;
	RIGID_RIGID_HAS_MATERIAL           = 1 shl 15;

	RigidF2F: array[RigidBodyFlag] of uint = (RIGID_SLEEPLESS_BIT, RIGID_UNAFFECTED_BY_GRAVITY_BIT,
		RIGID_INFINITE_ROT_IX_BIT, RIGID_INFINITE_ROT_IY_BIT, RIGID_INFINITE_ROT_IZ_BIT,
		RIGID_FORCE_INERTIA_MOMENT_1_BIT);

type
	pTmpInteractionInfo = ^tTmpInteractionInfo;
	tTmpInteractionInfo = array of record
		b2: pRigidBody;
		int: pRigidInteraction;
	end;

	procedure SerializeRigidBody(se: pSerializer; obj: pointer);
	var
		body: pRigidBody absolute obj;
		asRigid: RigidBody.pRigidSpecific;
		asPhantom: RigidBody.pPhantomSpecific;
		flags: uint;
		flag: RigidBodyFlag;
		int_it: tHash_Body2Interaction.Iterator;
		i: sint;
	begin
		with se^ do
		begin
			flags := 0;
			for flag in RigidBodyFlag do
				if (flag in body^._flags) and (RigidF2F[flag] <> 0) then flags := flags or RigidF2F[flag];
			if body^._interaction.Count > 0 then flags := flags or RIGID_HAS_INTERACTIONS_BIT;

			case body^._kind of
				rigid_DynamicBody:
					begin
						asRigid := body^.AsRigid;
						if asRigid^.mass = INFINITE_MASS then
							flags := flags or RIGID_RIGID_INFINITE_MASS_BIT
						else
						begin
							if body^.Velocity <> Vec3.Zero      then flags := flags or RIGID_RIGID_HAS_VELOCITY_BIT;
							if body^.Omega <> Vec3.Zero         then flags := flags or RIGID_RIGID_HAS_OMEGA_BIT;
						end;
						if length(asRigid^.joints) > 0       then flags := flags or RIGID_RIGID_HAS_JOINTS_BIT;
						if not asRigid^.onCollide.Empty      then flags := flags or RIGID_RIGID_HAS_ONCOLLIDE_BIT;
						if not asRigid^.onEnterPhantom.Empty then flags := flags or RIGID_RIGID_HAS_ONENTERPHANTOM_BIT;
						if not asRigid^.onExitPhantom.Empty  then flags := flags or RIGID_RIGID_HAS_ONEXITPHANTOM_BIT;
						if not asRigid^.entangle.Empty       then flags := flags or RIGID_RIGID_HAS_ENTANGLE_BIT;
						if asRigid^.matName <> RigidBody.DefaultMaterial then flags := flags or RIGID_RIGID_HAS_MATERIAL;
					end;
				rigid_Phantom:
					begin
						asPhantom := body^.AsPhantom;
						if not asPhantom^.onEnter.Empty then flags := flags or RIGID_PHANTOM_HAS_ONENTER_BIT;
						if not asPhantom^.onExit.Empty then flags := flags or RIGID_PHANTOM_HAS_ONEXIT_BIT;
					end;
			end;

			Serialize_enum(stream, ord(body^._kind), RigidBody.KindPrefixCodes);
			Serialize_ui16(stream, flags);
			SeObject(body^._primitive);

			if body^._interaction.Count > 0 then
			begin
				Serialize_ui16(stream, body^._interaction.Count);
				int_it := body^._interaction.GetIterator;
				while body^._interaction.Next(int_it) do
				begin
					SeObject(body^._interaction.GetKey(int_it)^);
					SeObject(body^._interaction.GetValue(int_it)^);
				end;
			end;

			case body^._kind of
				rigid_DynamicBody:
					begin
						if asRigid^.matName <> RigidBody.DefaultMaterial then Serialize_string(stream, asRigid^.matName);
						if asRigid^.mass <> INFINITE_MASS then
						begin
							Serialize_f16(stream, asRigid^.mass);
							if body^.Velocity <> Vec3.Zero then Serialize_vec3f32(stream, body^.Velocity);
							if body^.Omega <> Vec3.Zero then Serialize_vec3f32(stream, body^.Omega);
						end;
						if length(asRigid^.joints) > 0 then
						begin
							Serialize_ui8(stream, length(asRigid^.joints));
							for i := 0 to High(asRigid^.joints) do
								SeObject(asRigid^.joints[i]);
						end;
						if not asRigid^.onCollide.Empty then SeObject(@asRigid^.onCollide, ObjType_MultiDelegate);
						if not asRigid^.onEnterPhantom.Empty then SeObject(@asRigid^.onEnterPhantom, ObjType_MultiDelegate);
						if not asRigid^.onExitPhantom.Empty then SeObject(@asRigid^.onExitPhantom, ObjType_MultiDelegate);
						if not asRigid^.entangle.Empty then SeObject(@asRigid^.entangle, ObjType_ModifiableValue);
					end;
				rigid_Phantom:
					begin
						if not asPhantom^.onEnter.Empty then SeObject(@asPhantom^.onEnter, ObjType_MultiDelegate);
						if not asPhantom^.onExit.Empty then SeObject(@asPhantom^.onExit, ObjType_MultiDelegate);
					end;
			end;
		end;
	end;

	// Внимание, координаты asRigid и asPhantom в памяти — абсолютные.
	procedure DeserializeRigidBody(de: pDeserializer; obj: pointer);
	var
		body: pRigidBody absolute obj;
		flags: uint;
		flag: RigidBodyFlag;
		ints: pTmpInteractionInfo;
		i: sint;
		asRigid: RigidBody.pRigidSpecific;
		asPhantom: RigidBody.pPhantomSpecific;
	begin
		with de^ do
		begin
			body^._kind := RigidBody.tKind(Deserialize_enum(stream, RigidBody.KindPrefixCodes));
			flags := Deserialize_ui16(stream);
			body^._flags := [];
			for flag in RigidBodyFlag do
				if (RigidF2F[flag] and flags) <> 0 then Include(body^._flags, flag);

			body^.worldId1 := 0;
			DeObjectR(body^._primitive);

			// Хак. Место _interaction используется для массива с тем же содержанием. После десериализации он переписывается в _interaction.
			Assert(sizeof(tHash_Body2Interaction) >= sizeof(tTmpInteractionInfo));
			ints := pointer(@body^._interaction);
			System.Initialize(ints^);
			if (flags and RIGID_HAS_INTERACTIONS_BIT) <> 0 then
			begin
				SetLength(ints^, Deserialize_ui16(stream));
				for i := 0 to High(ints^) do
				begin
					DeWeakA(ints^[i].b2);
					DeObjectA(ints^[i].int);
				end;
			end;

			case body^._kind of
				rigid_DynamicBody:
					begin
						new(body^._specific.asRigid); body^._specific.asRigid^.Initialize(no);
						asRigid := body^.AsRigid;
						if flags and RIGID_RIGID_HAS_MATERIAL <> 0 then asRigid^.matName := Deserialize_string(stream) else asRigid^.matName := body^.DefaultMaterial;
						if (flags and RIGID_RIGID_INFINITE_MASS_BIT) = 0 then
						begin
							asRigid^.mass := Deserialize_f16(stream);
							if (flags and RIGID_RIGID_HAS_VELOCITY_BIT) <> 0 then
								body^.Velocity := Deserialize_vec3f32(stream);
							if (flags and RIGID_RIGID_HAS_OMEGA_BIT) <> 0 then
								body^.Omega := Deserialize_vec3f32(stream);
						end else
							asRigid^.mass := INFINITE_MASS;

						if (flags and RIGID_RIGID_HAS_JOINTS_BIT) <> 0 then
						begin
							SetLength(asRigid^.joints, Deserialize_ui8(stream));
							for i := 0 to High(asRigid^.joints) do
								DeObjectA(asRigid^.joints[i]);
						end;
						if (flags and RIGID_RIGID_HAS_ONCOLLIDE_BIT) <> 0 then DeWeakAtA(asRigid^.onCollide) else asRigid^.onCollide.Init;
						if (flags and RIGID_RIGID_HAS_ONENTERPHANTOM_BIT) <> 0 then DeWeakAtA(asRigid^.onEnterPhantom) else asRigid^.onEnterPhantom.Init;
						if (flags and RIGID_RIGID_HAS_ONEXITPHANTOM_BIT) <> 0 then DeWeakAtA(asRigid^.onExitPhantom) else asRigid^.onExitPhantom.Init;
						if (flags and RIGID_RIGID_HAS_ENTANGLE_BIT) <> 0 then DeWeakAtA(asRigid^.entangle) else asRigid^.entangle.Init(0.0);
					end;
				rigid_Phantom:
					begin
						new(body^._specific.asPhantom); body^._specific.asPhantom^.Initialize(no);
						asPhantom := body^.AsPhantom;
						if (flags and RIGID_PHANTOM_HAS_ONENTER_BIT) <> 0 then DeWeakAtA(asPhantom^.onEnter) else asPhantom^.onEnter.Init;
						if (flags and RIGID_PHANTOM_HAS_ONEXIT_BIT) <> 0 then DeWeakAtA(asPhantom^.onExit) else asPhantom^.onExit.Init;
					end;
			end;
		end;
	end;

	procedure RigidBodyDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		body: pRigidBody absolute obj;
		ints: tTmpInteractionInfo;
		i: sint;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: body^.DeseInit;
			de_After:
				begin
					ints := pTmpInteractionInfo(@body^._interaction)^;
					System.Finalize(pTmpInteractionInfo(@body^._interaction)^);
					body^._interaction.Init;
					for i := 0 to High(ints) do
						body^._interaction.Add(ints[i].b2, ints[i].int);
				end;
		end;
	end;

const
	RINT_COLLIDABLE_BIT = 1 shl 0;

	procedure SerializeRigidInteraction(se: pSerializer; obj: pointer);
	var
		int: pRigidInteraction absolute obj;
		flags: uint;
	begin
		with se^ do
		begin
			flags := 0;
			if int^._collidable then flags := flags or RINT_COLLIDABLE_BIT;
			Serialize_ui8(stream, flags);
		end;
	end;

	procedure DeserializeRigidInteraction(de: pDeserializer; obj: pointer);
	var
		int: pRigidInteraction absolute obj;
		flags: uint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			int^._collidable := (flags and RINT_COLLIDABLE_BIT) <> 0;
		end;
	end;

	procedure RigidInteractionDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		int: pRigidInteraction absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: int^.Init;
		end;
	end;

const
	WATER_USER_NORMAL  = 1 shl 0;
	WATER_USER_DENSITY = 1 shl 1;
	WATER_USER_LINEAR_VISCOSITY = 1 shl 2;
	WATER_USER_ANGULAR_VISCOSITY = 1 shl 3;

	procedure SerializeWater(se: pSerializer; obj: pointer);
	var
		water: pWater absolute obj;
		flags: uint;
	begin
		with se^ do
		begin
			flags := 0;
			if water^.PlaneNormal <> Vec3.PositiveY then flags := flags or WATER_USER_NORMAL;
			if not Equals(water^.Density, 1.0) then flags := flags or WATER_USER_NORMAL;
			if not Equals(water^.LinearViscosity, 1.0) then flags := flags or WATER_USER_LINEAR_VISCOSITY;
			if not Equals(water^.AngularViscosity, 1.0) then flags := flags or WATER_USER_ANGULAR_VISCOSITY;
			Serialize_ui8(stream, flags);
			if (flags and WATER_USER_NORMAL) <> 0 then Serialize_vec3N8(stream, water^.PlaneNormal, Vec3.MinusOnes, Vec3.Ones);
			if (flags and WATER_USER_DENSITY) <> 0 then Serialize_f16(stream, water^.Density);
			if (flags and WATER_USER_LINEAR_VISCOSITY) <> 0 then Serialize_f16(stream, water^.LinearViscosity);
			if (flags and WATER_USER_ANGULAR_VISCOSITY) <> 0 then Serialize_f16(stream, water^.AngularViscosity);
		end;
	end;

	procedure DeserializeWater(de: pDeserializer; obj: pointer);
	var
		water: pWater absolute obj;
		flags: uint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			if (flags and WATER_USER_NORMAL) <> 0 then water^.PlaneNormal := Deserialize_vec3N8(stream, Vec3.MinusOnes, Vec3.Ones).Normalized else water^.PlaneNormal := Vec3.PositiveY;
			if (flags and WATER_USER_DENSITY) <> 0 then water^.Density := Deserialize_f16(stream) else water^.Density := 1.0;
			if (flags and WATER_USER_LINEAR_VISCOSITY) <> 0 then water^.LinearViscosity := Deserialize_f16(stream) else water^.LinearViscosity := 1.0;
			if (flags and WATER_USER_ANGULAR_VISCOSITY) <> 0 then water^.AngularViscosity := Deserialize_f16(stream) else water^.AngularViscosity := 1.0;
		end;
	end;

	procedure WaterDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		water: pWater absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: water^.DeseInit;
		end;
	end;

	procedure SerializeWorld(se: pSerializer; obj: pointer);
	var
		world: pPhysWorld absolute obj;
		flags: uint;
		it: tHash_Code2OnCollide.Iterator;
	begin
		with se^ do
		begin
			flags := 0;
			Serialize_ui8(stream, flags);
			SeObject(world^._materialDb);

			Serialize_ui32(stream, world^._int2onCollide.Count);
			it := world^._int2onCollide.GetIterator;
			while world^._int2onCollide.Next(it) do
			begin
				Serialize_ui32(stream, world^._int2onCollide.GetKey(it)^);
				SeObject(world^._int2onCollide.GetValue(it)^, ObjType_MultiDelegate);
			end;

			Serialize_f16(stream, world^._envResistance);
			Serialize_vec3f16(stream, world^.gravity);
		end;
	end;

	procedure DeserializeWorld(de: pDeserializer; obj: pointer);
	var
		world: pPhysWorld absolute obj;
		md: pMultiDelegate;
		n, i: sint;
	begin
		with de^ do
		begin
			if Deserialize_ui8(stream) <> 0 then raise Error('Флаги мира полагаются нулевыми.');
			DeObjectR(world^._materialDb);

			n := Deserialize_ui32(stream);
			for i := 1 to n do
			begin
				new(md); md^.Init;
				world^._int2onCollide.Add(Deserialize_ui32(stream), md);
				md^.Done;
				DeWeakAtA(md^);
			end;

			world^._envResistance := Deserialize_f16(stream);
			world^.gravity := Deserialize_vec3f16(stream);
		end;
	end;

	procedure WorldDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		world: pPhysWorld absolute obj;
		ndb: pPhysMaterialDB;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: world^.Init;
			de_After:
				begin
					ndb := world^._materialDb;
					world^._materialDb := nil;
					world^._SetMaterialDB(ndb);
					Release(ndb);
				end;
		end;
	end;
{$endif use_serialization}

	function LoadRigidPrimitive(s: pStream): pObject; begin result := RigidPrimitive.Deserialize(s); end;
	function LoadPhysMaterialDB(s: pStream): pObject; begin result := new(pPhysMaterialDB, Init(s)); end;

	procedure Init;
	begin
		RigidPrimitive._nInstances := 0;
		Newton.loader.Hook(+0).AfterLoad(@AfterLoad).BeforeUnload(@BeforeUnload);
	{$ifdef Debug} stat.Note(rigid_contacts_limit, MaxContacts); {$endif}

		ResourcePool.Shared
		^.Register(TypeOf(RigidPrimitive), @LoadRigidPrimitive)
		^.Register(TypeOf(PhysMaterialDB), @LoadPhysMaterialDB);

	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Rigid primitive', TypeOf(RigidPrimitive), nil, sizeof(RigidPrimitive), yes,
		               @SerializeRigidPrimitive, @DeserializeRigidPrimitive, nil, @RigidPrimitiveDeSpecial)
		^.RegisterType('Rigid body', TypeOf(RigidBody), TypeOf(SceneNode), sizeof(RigidBody), yes,
		               @SerializeRigidBody, @DeserializeRigidBody, nil, @RigidBodyDeSpecial)
		^.RegisterFuncs([@__OnCollide, @__OnPhantom])
		^.RegisterType('Water', TypeOf(tWater), TypeOf(RigidBody), sizeof(tWater), yes,
		               @SerializeWater, @DeserializeWater, nil, @WaterDeSpecial)
		^.RegisterType('Rigid interaction', TypeOf(tRigidInteraction), nil, sizeof(tRigidInteraction), yes,
		               @SerializeRigidInteraction, @DeserializeRigidInteraction, nil, @RigidInteractionDeSpecial)
		^.RegisterType('Physical material database', TypeOf(PhysMaterialDB), nil, sizeof(PhysMaterialDB), yes, nil, nil, nil, nil)
		^.RegisterType('Physical world', TypeOf(PhysWorld), nil, sizeof(PhysWorld), yes,
		               @SerializeWorld, @DeserializeWorld, nil, @WorldDeSpecial);
	{$endif}
	end;

initialization
	&Unit('Physics').Initialize(@Init);
end.
