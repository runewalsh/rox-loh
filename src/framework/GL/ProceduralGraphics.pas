unit ProceduralGraphics;

{$include opts.inc}

interface

uses
	USystem, Errors, UMath, Random, UClasses, Utils, U_GL, GLBase, GLClasses, SceneGraph, Script, RenderLists, GUI, SpatialIndex, Streams, GLUtils
{$ifdef Debug}, Human, ULog {$endif};

type
	pTrail = ^Trail;
	Trail = object(SceneNode)
	public type
		FlagEnum = (trail_Textured);
		FlagSet = set of FlagEnum;
		Topology = (topology_Star, topology_Tube);
		IndSample = array of uint;
		OfsSample = array of Vec3;
		TexXSample = array of float;

		pRingRec = ^tRingRec;
		tRingRec = record
			spawnTime: float;
			pos: Vec3;
			rot: Quaternion;
			texY: float;
		end;
	public const
		TopologyIds: array[Topology] of string = ('star', 'tube');
		DefaultSegs: array[Topology] of sint = (3, 6);
		FlagIds: array[FlagEnum] of string = ('textured');
		FLAG_STAR     = UserShaderFlags.PRIVATE_BUILTIN + 0;
		FLAG_TUBE     = UserShaderFlags.PRIVATE_BUILTIN + 1;
		FLAG_TEXTURED = UserShaderFlags.PRIVATE_BUILTIN + 2;
	private var
		_flags: FlagSet;
		_bnd: Bounding;
		_mat: pGLMaterial;
		_ro: pRenderObject;
		_active: boolean;
		_glmesh: tGLMesh;
		_batch: pBatch;
		_vaCenterLife, _vaSegRot, _vaOfs, _vaTex: pNativeGLValue;
		_ref: pSceneNode;
		_refTransform: Transform;
		_persistence, _texYFactor, _moveTolerance, _angleTolerance: float;
		_recalcBndAndSweepTimeout, _nextRingTimeout: float;
		_refPos, _refRotCheckPos: Vec3;
		_refRot: Quaternion;
		_topology: Topology;
		_nRings, _nSegs, _dv, _di: sint;
		_rings: array of tRingRec;
		function _Initialize(dese: boolean): boolean;
		procedure _PrepareTopology;
		function _GetInd: IndSample;
		function _GetOfs: OfsSample;
		function _GetTexX: TexXSample;
		function _GetGL: pGLEntityParams;
		procedure _SetRef(aRef: pSceneNode);
		procedure _SpawnRings(n: sint; explicit: pRingRec); // в explicit поле spawnTime на самом деле равно разнице между spawnTime и mm.time
		procedure _UpdateRings(start, count: sint; explicit: pRingRec);
		procedure _KillFirstRings(n: sint);
		function _HasPrev: boolean;
		function _PrevPos: Vec3;
		function _PrevRot: Quaternion;
		procedure _RecalculateBoundingAndSweep;
		procedure _SetPersistence(const newPersistence: float);
		procedure _UpdatePersistence;
		procedure _SetTexYFactor(const newFactor: float);
		procedure _UpdateTexYFactor;
	protected
		procedure _OnUpdate(const dt: float); virtual;
		function _SuitsTo(know: SceneKnowledge): boolean; virtual;
	public
		constructor Init(newMat: pGLMaterial; newTopology: Topology; newSegs: sint; newFlags: FlagSet);
		destructor Done; virtual;
		procedure Break;

		property Ref: pSceneNode read _ref write _SetRef;
		property RefTransform: Transform read _refTransform write _refTransform;
		property gl: pGLEntityParams read _GetGL;
		property Persistence: float read _persistence write _SetPersistence;
		property MoveTolerance: float read _moveTolerance write _moveTolerance;
		property AngleTolerance: float read _angleTolerance write _angleTolerance;
		property TexYFactor: float read _texYFactor write _SetTexYFactor;
		property Flags: FlagSet read _flags;
	end;

	ParticleSystemFlag = (particles_Local, particles_ConstraintWithPlane, particles_UseAcceleration);
	ParticleSystemFlags = set of ParticleSystemFlag;

	pParticleSystem = ^ParticleSystem;
	ParticleSystem = object(SceneNode)
	public type
		pParticle = ^Particle;
		Particle = object
			variation: sint;
			seed: float;
			spawnPos, spawnVel: Vec3;
			spawnSize, endSize: float;
			spawnTime, life: float;
			constraint: Plane;
			procedure Initialize(var sys: ParticleSystem; dese: boolean);
			function EstimatePosition(var sys: ParticleSystem; const aDt: float): Vec3;
		end;

	{$define classname := ParticlesList} {$define item_type := Particle} {$include vector.h.inc}

		Variation = object
			texA, texB: Vec2;
		end;

		OnSpawnProc = procedure(var sys: ParticleSystem; var particle: Particle; const info: SingleDelegateInfo);
		pOnSpawnArgs = ^OnSpawnArgs;
		OnSpawnArgs = record
			sys: pParticleSystem;
			particle: pParticle;
		end;

	private const
		RecalcBndAndSweepTimeout = 4.0;
		LocalFactor = 2.0;
		FLAG_ACCELERATION     = UserShaderFlags.PRIVATE_BUILTIN + 0;
		FLAG_PLANE_CONSTRAINT = UserShaderFlags.PRIVATE_BUILTIN + 1;
		FLAG_LOCAL            = UserShaderFlags.PRIVATE_BUILTIN + 2;

	private var
		_vtsPerParticle, _idsPerParticle: uint; static;
		procedure _InitStatic; static;
	private
		_flags: ParticleSystemFlags;
		_variations: array of Variation;
		_variationWeights: array of float;

		_bnd: Bounding;
		_mat: pGLMaterial;
		_ro: pRenderObject;
		_glmesh: tGLMesh;
		_batch: pBatch;

		// TSVIL — Spawn Time, Size delta, Variation (& seed), Inverse Lifetime
		_vaSpawnPosAndSize, _vaSpawnVel, _vaTSVIL, _vaConstraintPlane, _vaCornerID: pNativeGLValue;

		_ref: pSceneNode;
		_active: boolean;
		_lifeDis, _sizeDis, _endSizeDis: Distribution;
		_hasPrev: boolean;
		_prevPos, _prevVel: Vec3;
		particles: ParticlesList;
		_recalcBndAndSweepTimeout, _spawnTimeout, _temporalTolerance, _temporalFrequency, _spatialTolerance, _spatialFrequency: float;
		_acceleration: Vec3;
		_lastRefPos: Vec3;

		function _Initialize(dese: boolean): boolean;
		procedure _SetRef(aRef: pSceneNode);
		procedure _Spawn(n: uint; local: boolean = no; explicit: pParticle = nil);
		procedure _SetTemporalFrequency(const newFreq: float);
		procedure _SetSpatialFrequency(const newFreq: float);
		procedure _SetAcceleration(const newAcc: Vec3);
		procedure CallOnSpawn(var p: Particle);
		procedure _Assign(to_, from: uint);
		procedure _RecalculateBoundingAndSweep;
		function _GetGL: pGLEntityParams;
		procedure _UpdateVariations;
		procedure _UpdateTemporalFrequency;
		procedure _UpdateSpatialFrequency;
		procedure _SetupLocal;
	protected
		procedure _OnUpdate(const dt: float); virtual;
		function _SuitsTo(know: SceneKnowledge): boolean; virtual;
	public
		onSpawn: MultiDelegate;
		constructor Init(newFlags: ParticleSystemFlags; newMat: pGLMaterial);
		destructor Done; virtual;
		procedure Break;
		procedure AddVariation(const newTexA, newTexB: Vec2; const weight: float);

		property Flags: ParticleSystemFlags read _flags;
		property Ref: pSceneNode read _ref write _SetRef;
		property GL: pGLEntityParams read _GetGL;
		property LifeDis: Distribution read _lifeDis write _lifeDis;
		property SizeDis: Distribution read _sizeDis write _sizeDis;
		property EndSizeDis: Distribution read _endSizeDis write _endSizeDis;
		property TemporalFrequency: float read _temporalFrequency write _SetTemporalFrequency;
		property SpatialFrequency: float read _spatialFrequency write _SetSpatialFrequency;
		property Acceleration: Vec3 read _acceleration write _SetAcceleration;
	end;

	pMazePattern = ^MazePattern;
	MazePattern = object
	type
		CellFlag = (CellIsProhibited, CellIsGrowSource, CellIsForced,
		            CellHasPrev, CellPrevBit0, CellPrevBit1, HasWallRight, HasWallDown, CellPassed);
		CellFlags = set of CellFlag;
		StartCell = type CellFlags;
		StartProc = procedure(x, y, i: uint; var cell: StartCell; param: pointer);
		StepResult = set of (FrozenMaze);
		pCell = ^Cell_t;
		Cell_t = object
			flags: CellFlags;
		const
			Empty: Cell_t = (flags: []);
		end;

		CellRef = object
			x, y: uint;
			cell: pCell;
			function Make(newX, newY: uint; newCell: pCell): CellRef; static;
			function First(var mp: MazePattern): CellRef; static;
			function Next(var mp: MazePattern): boolean;
			function ShiftLeft(var mp: MazePattern): CellRef;
			function ShiftRight(var mp: MazePattern): CellRef;
			function ShiftUp(var mp: MazePattern): CellRef;
			function ShiftDown(var mp: MazePattern): CellRef;
			function Shift(const dir: Dir4; var mp: MazePattern): CellRef;
			function Obstructed(var mp: MazePattern): boolean;
			function HasWall(const dir: Dir4; var mp: MazePattern): boolean;
			function WallLeft(var mp: MazePattern): boolean;
			function WallUp(var mp: MazePattern): boolean;
			function WallRight(var mp: MazePattern): boolean;
			function WallDown(var mp: MazePattern): boolean;
			function WallPseudographics(var mp: MazePattern): StringView;
			function PrevPseudographics: StringView;
		end;
		UpdateCallback = procedure(const cell: CellRef; var mp: MazePattern; const dir: Dir4; prevStage, newStage: uint; param: pointer);

		function Create(newSx, newSy, newMaxStage: uint; start: StartProc; startParam: pointer; update: UpdateCallback; updateParam: pointer): MazePattern; static;
		procedure Done;
		function Step(amount: uint; update: UpdateCallback; param: pointer): StepResult;

	type
		DumpProc = procedure(const ref: CellRef; var sym: StringView; var mp: MazePattern; param: pointer);
		function Dump(getsym: DumpProc; param: pointer): string;

	private type
		CellPredicate = object
			flags: CellFlags;
			neg: boolean;
			function Make(newFlags: CellFlags; newNeg: boolean): CellPredicate; static;
			function Match(const c: Cell_t): boolean;
		end;

		GrowingPoint = object
			cell, target: CellRef;
			dir: Dir4;
			delay: sint;
			stage: array[Dir4.Enum] of uint;
			function Create(const newCell, newTarget: CellRef; const newDir: Dir4): GrowingPoint; static;
			function TryMove(nd: Dir4; const on: CellPredicate; var mp: MazePattern; out moved: GrowingPoint): boolean;
		end;
		GrowingPointsArray = array of GrowingPoint; pGrowingPointsArray = ^GrowingPointsArray;

		BreadthGp = object
			cell: CellRef;
			dir: Dir4;
			delay, progress: uint;
			function Make(const newCell: CellRef; const newDir: Dir4; newDelay, newProgress: sint): BreadthGp; static;
		end;
	{$define classname := GrowingPointsQueue} {$define item_type := BreadthGp} {$include queue.h.inc}
	const
		MainGrow: CellPredicate = (flags: [CellIsProhibited, CellIsForced, CellIsGrowSource]; neg: yes);
	var
		sx, sy, maxstg: uint;
		cells: array of Cell_t;
		rng: RNG;
		q: GrowingPointsQueue;
		function GetCellRef(x, y: uint): CellRef;
		function ValidatePoint(x, y: uint): boolean;
		function ValidMove(x, y: uint; const dir: Dir4): boolean;
		procedure PlaceWalls(const from, on: CellPredicate; const frequency: float; startGps: pGrowingPointsArray; update: UpdateCallback; param: pointer);
		function PlaceGrowingPoints(const frequency: float; const from, on: CellPredicate): GrowingPointsArray;
		function GrowStep(const on: CellPredicate; var gps: GrowingPointsArray; var nGps: uint; update: UpdateCallback; param: pointer): boolean;
		procedure PropagateAround(const cell: CellRef; const cameBy: Dir4; delay: sint);
		procedure SetStage(const cell: CellRef; const dir: Dir4; prev, stage: uint; update: UpdateCallback; param: pointer);

	public
		property SizeX: uint read sx;
		property SizeY: uint read sy;
		property MaxStage: uint read maxstg;
	end;

	pGLMazePattern = ^GLMazePattern;
	GLMazePattern = object(&Object)
		constructor Init(sx, sy: uint; start: MazePattern.StartProc; startParam: pointer);
		destructor Done; virtual;
		procedure Bind(v: pGLEntityParams; const id: PoolString);
		function Step(amount: uint): MazePattern.StepResult;
	const
		MaxStage = 2;
	private
		mp: MazePattern;
		tex: pTexture;
		texData: StringView;
		texDirty: boolean;
		procedure UpdateTexture(force: boolean);
	end;

	procedure OpenScript(var script: ScriptState);
	procedure GenerateMengerSponge(depth: sint; const size: float; out gmain, gholes: pGLMesh);

implementation

uses
	MMSystem, Script_EngineAPI
{$ifdef Profile}, Profile {$endif}
{$ifdef use_serialization}, Serialization {$endif};

{$define classname := ParticleSystem.ParticlesList} {$include vector.pp.inc}
{$define classname := MazePattern.GrowingPointsQueue} {$include queue.pp.inc}

const
	ParticleSystemFlagIds: array[ParticleSystemFlag] of string = ('local_space', 'plane_constraint', 'use_acceleration');

	function Trail._GetGL: pGLEntityParams;
	begin
		result := @_ro^.gl;
	end;

	procedure Trail._SetRef(aRef: pSceneNode);
	begin
		if _ref <> aRef then
		begin
			if not Assigned(_ref) then _refPos := (aRef^.GlobalTransform * _refTransform).tr;
			SetRef(_ref, aRef);
		end;
	end;

	procedure Trail._SpawnRings(n: sint; explicit: pRingRec);
	var
		i: sint;
		sr, sv, si, cv, ci: uint;
		inds: pMeshIndices;
		sample: IndSample;
	begin
		if n <= 0 then exit;
		sr := _nRings;
		inc(_nRings, n);
		if _nRings > length(_rings) then SetLength(_rings, 2 * _nRings);

		inds := @_batch^.inds;
		sv := _batch^.VerticesCount;

		_batch^.VerticesCount := _nRings * _dv;
		if _nRings > 1 then
		begin
			cv := sv; if sr = 0 then inc(cv, uint(_dv));
			si := inds^.Count;
			inds^.Count := (_nRings - 1) * _di;
			sample := _GetInd;
			ci := si;
			while ci < uint(inds^.Count) do
			begin
				for i := 0 to _di - 1 do
					if sample[i] <> MeshIndices.RestartIndex then
						inds^[ci + uint(i)] := ulong(ulong(cv) + sample[i]) - _dv
					else
						inds^[ci + uint(i)] := MeshIndices.RestartIndex;
				inc(cv, uint(_dv));
				inc(ci, uint(_di));
			end;
			_glmesh.PartialChangedInds(0, 0, si, ci - si);
		end else
			inds^.Count := 0;

		_UpdateRings(sr, n, explicit);
		if not Assigned(explicit) then _nextRingTimeout := _persistence;
	end;

	procedure Trail._UpdateRings(start, count: sint; explicit: pRingRec);
	var
		ring: pRingRec;
		newBnd: Bounding;
		d: float;
		cur, acur, i, v: sint;
		ofs: OfsSample;
		texx: TexXSample;
	begin
		if Assigned(_ro^.bnd) then newBnd := _bnd;
		for cur := 0 to count - 1 do
		begin
			acur := start + cur;
			ring := @_rings[acur];
			if Assigned(explicit) then
			begin
				ring^.pos := explicit[cur].pos;
				ring^.rot := explicit[cur].rot;
				ring^.spawnTime := mm.SceneTime - explicit[cur].spawnTime;
			end else
			begin
				Assert(count = 1);
				if Assigned(_ref) then
				begin
					_refPos := (_ref^.globalTransform * _refTransform).tr;
					d := SqrDistance(_refRotCheckPos, _refPos);
					if d > 0.01 then
					begin
						_refRot := Quaternion.RotationThroughX0Z(Vec3.PositiveZ, (_refPos - _refRotCheckPos).Normalized);
						_refRotCheckPos := _refPos;
					end;

					ring^.spawnTime := mm.SceneTime;
					ring^.pos := _refPos;
					ring^.rot := _refRot;
				end else
					exit;
			end;

			if trail_Textured in _flags then
				if acur > 0 then
					ring^.texY := _rings[acur - 1].texY + Distance(_rings[acur - 1].pos, ring^.pos) * _texYFactor
				else
					ring^.texY := 0.0;

			if (not Assigned(_ro^.bnd)) and (cur = 0) then
				newBnd := Bounding.ByPoint(ring^.pos)
			else
				newBnd.Enlarge(ring^.pos);

			v := acur * _dv;
			if Assigned(_vaOfs) then ofs := _GetOfs;
			if Assigned(_vaTex) then texx := _GetTexX;
			for i := 0 to _dv - 1 do
			begin
				_vaCenterLife^.RawVec4[v] := Vec4.Make(ring^.pos, ring^.spawnTime);
				if Assigned(_vaSegRot) then _vaSegRot^.RawVec4[v] := ring^.rot.v4;
				if Assigned(_vaOfs) then _vaOfs^.RawVec3[v] := _refRot * ofs[i];
				if Assigned(_vaTex) then _vaTex^.RawVec2[v] := Vec2.Make(texx[i], ring^.texY);
				inc(v);
			end;
		end;
		if Assigned(_ro^.bnd) then _ro^.ChangeBounding(newBnd) else
			if count > 0 then
			begin
				_bnd := newBnd;
				_ro^.Bnd := @_bnd;
			end;

		_glmesh.PartialChangedVA(0, _vaCenterLife, start * _dv, count * _dv);
		if Assigned(_vaSegRot) then _glmesh.PartialChangedVA(0, _vaSegRot, start * _dv, count * _dv);
		if Assigned(_vaOfs) then _glmesh.PartialChangedVA(0, _vaOfs, start * _dv, count * _dv);
		if Assigned(_vaTex) then _glmesh.PartialChangedVA(0, _vaTex, start * _dv, count * _dv);
	end;

	procedure Trail._KillFirstRings(n: sint);
	var
		i, v2: sint;
		inds: pMeshIndices;
	begin
		if n <= 0 then exit;
		Assert(n <= _nRings);
		dec(_nRings, n);
		for i := 0 to _nRings - 1 do
			_rings[i] := _rings[i + n];
		inds := @_batch^.inds;

		v2 := _dv * n;
		for i := 0 to _nRings * _dv - 1 do
		begin
			_vaCenterLife^[i] := _vaCenterLife^[v2];
			if Assigned(_vaSegRot) then _vaSegRot^[i] := _vaSegRot^[v2];
			if Assigned(_vaOfs) then _vaOfs^[i] := _vaOfs^[v2];
			if Assigned(_vaTex) then _vaTex^[i] := _vaTex^[v2];
			inc(v2);
		end;
		if _nRings > 1 then inds^.Count := (_nRings - 1) * _di else inds^.Count := 0;
		_batch^.VerticesCount := _nRings * _dv;
		_glmesh.PartialChangedVA(0, _vaCenterLife, 0, _batch^.VerticesCount);
		if Assigned(_vaSegRot) then _glmesh.PartialChangedVA(0, _vaSegRot, 0, _batch^.VerticesCount);
		if Assigned(_vaOfs) then _glmesh.PartialChangedVA(0, _vaOfs, 0, _batch^.VerticesCount);
		if Assigned(_vaTex) then _glmesh.PartialChangedVA(0, _vaTex, 0, _batch^.VerticesCount);
	end;

	function Trail._HasPrev: boolean;
	begin
		result := _nRings > 1;
	end;

	function Trail._PrevPos: Vec3;
	begin
		result := _rings[_nRings - 2].pos;
	end;

	function Trail._PrevRot: Quaternion;
	begin
		result := _rings[_nRings - 2].rot;
	end;

	procedure Trail._RecalculateBoundingAndSweep;
	const
		RecalcBndAndSweepTimeout = 4.0;
	var
		i, n: sint;
		newBnd: Bounding;
	begin
		n := _nRings;
		for i := 0 to _nRings - 1 do
			if mm.SceneTimeSince(_rings[i].spawnTime) <= _persistence then
			begin
				n := i - 1; // кольцо i — живое, а (i-1) придётся оставить: оно участвует в плавном исчезновении хвоста
				System.Break; // ух, сколько же я этот break ловил... :D
			end;
		_KillFirstRings(n);
		_recalcBndAndSweepTimeout := RecalcBndAndSweepTimeout;

		if _nRings > 0 then
		begin
			newBnd := Bounding.ByPoint(_rings[0].pos);
			for i := 1 to _nRings - 1 do
				newBnd.Enlarge(_rings[i].pos);
			if Assigned(_ro^.Bnd) then
				_ro^.ChangeBounding(newBnd)
			else
			begin
				_bnd := newBnd;
				_ro^.Bnd := @_bnd;
			end;
		end;
	end;

	procedure Trail._SetPersistence(const newPersistence: float);
	begin
		if _persistence <> newPersistence then
		begin
			_persistence := newPersistence;
			_UpdatePersistence;
		end;
	end;

	procedure Trail._UpdatePersistence;
	begin
		gl^.values.Value('invPersistence', GLType.Float, 1, [NativeGLValueFlag.NonSerializable])^.SetFloat(1.0 / _persistence);
	end;

	procedure Trail._SetTexYFactor(const newFactor: float);
	begin
		if newFactor <> _texYFactor then
		begin
			_texYFactor := newFactor;
			_UpdateTexYFactor;
		end;
	end;

	procedure Trail._UpdateTexYFactor;
	begin
		Assert(trail_Textured in _flags);
		gl^.values.Value('texYFactor', GLType.Float, 1, [NativeGLValueFlag.NonSerializable])^.SetFloat(_texYFactor);
	end;

	procedure Trail._PrepareTopology;
	var
		i: sint;
		x: float;
		p: pNativeGLValue;
	begin
		if MMSystem.gl.GeometryShaderSupported then
		begin
			gl^.values.Value('nSegs', GLType.Int32, 1, [NativeGLValueFlag.NonSerializable])^.RawInt32[0] := _nSegs;
			p := gl^.values.Value('segSinCos', GLType.Vec2, _nSegs, [NativeGLValueFlag.NonSerializable]);
			for i := 0 to _nSegs - 1 do
			begin
				x := Pi * i / _nSegs;
				p^.RawVec2[i] := Vec2.Make(sin(x), cos(x));
			end;
			_dv := 1;
			_di := 1;
			exit;
		end;
		case _topology of
			topology_Star:
				begin
					_dv := 2 * _nSegs;
					if MMSystem.gl.PrimitiveRestartSupported then
					begin
						_glmesh.topology := GLtopology_TriStrip;
						_di := 5 * _nSegs;
					end else
					begin
						_glmesh.topology := GLtopology_Tris;
						_di := 6 * _nSegs;
					end;
				end;
			topology_Tube:
				begin
					_glmesh.topology := GLtopology_TriStrip;
					_dv := _nSegs;
					_di := 2 * (_nSegs + 1) + 1 + sint(not MMSystem.gl.PrimitiveRestartSupported);
				end;
			else Assert(no);
		end;
	end;

	function Trail._GetInd: IndSample;
	var
		i, ind, v: sint;
	begin
		SetLength(result, _di);
		if MMSystem.gl.GeometryShaderSupported then
			result[0] := +_dv
		else
		begin
			ind := 0;
			v := 0;
			case _topology of
				topology_Star:
					if MMSystem.gl.PrimitiveRestartSupported then
						for i := 0 to _nSegs - 1 do
						begin
							result[ind + 0] := v + _dv;
							result[ind + 1] := v;
							result[ind + 2] := v + _dv + 1;
							result[ind + 3] := v + 1;
							result[ind + 4] := MeshIndices.RestartIndex;
							inc(ind, 5);
							inc(v, 2);
						end
					else
						for i := 0 to _nSegs - 1 do
						begin
							result[ind + 0] := v + _dv;
							result[ind + 1] := v;
							result[ind + 2] := v + _dv + 1;
							result[ind + 3] := v + _dv + 1;
							result[ind + 4] := v;
							result[ind + 5] := v + 1;
							inc(ind, 6);
							inc(v, 2);
						end;
				topology_Tube:
					begin
						// дегенерат
						if not MMSystem.gl.PrimitiveRestartSupported then
						begin
							result[ind] := v + _dv;
							inc(ind);
						end;

						// кольцо (TriStrip)
						for i := 0 to _nSegs - 1 do
						begin
							result[ind + 0] := v + _dv;
							result[ind + 1] := v;
							inc(v);
							inc(ind, 2);
						end;
						// замыкание
						result[ind + 0] := _dv;
						result[ind + 1] := 0;
						inc(ind, 2);

						// дегенерат или Restart
						if MMSystem.gl.PrimitiveRestartSupported then
							result[ind] := MeshIndices.RestartIndex
						else
							result[ind] := result[ind - 1];
						inc(ind);
					end;
			end;
			Assert(ind = _di);
		end;
	end;

	function Trail._GetOfs: OfsSample;
	var
		i: sint;
		ins, x: float;
	begin
		Assert(not MMSystem.gl.GeometryShaderSupported);
		SetLength(result, _dv);
		case _topology of
			topology_Star:
				begin
					ins := Pi / _nSegs;
					for i := 0 to _nSegs - 1 do
					begin
						x := i * ins;
						result[2 * i + 0] := Vec3.Make(cos(x), sin(x), 0.0);
						result[2 * i + 1] := Vec3.Make(cos(Pi + x), sin(Pi + x), 0.0);
					end;
				end;
			topology_Tube:
				begin
					ins := TwoPi / _nSegs;
					for i := 0 to _nSegs - 1 do
					begin
						x := i * ins;
						result[i] := Vec3.Make(cos(x), sin(x), 0.0);
					end;
				end;
			else Assert(no);
		end;
	end;

	function Trail._GetTexX: TexXSample;
	var
		i: sint;
		ins: float;
	begin
		Assert(not MMSystem.gl.GeometryShaderSupported);
		SetLength(result, _dv);
		case _topology of
			topology_Star:
				begin
					for i := 0 to _nSegs - 1 do
					begin
						result[2 * i + 0] := 0.0;
						result[2 * i + 1] := 1.0;
					end;
				end;
			topology_Tube:
				begin
					ins := 1.0 / _nSegs;
					for i := 0 to _nSegs - 1 do
						result[i] := i * ins;
				end;
			else Assert(no);
		end;
	end;

	procedure Trail._OnUpdate(const dt: float);
	begin
	trace_call('Trail.OnUpdate');
		inherited _OnUpdate(dt);
		if _active then
		begin
			while _nRings < 2 do _SpawnRings(1, nil);
			_nextRingTimeout -= dt;
			if (_nextRingTimeout <= 0.0) or (_HasPrev and (SqrDistance(_refPos, _PrevPos) / sqr(_moveTolerance) + AngleN(_PrevRot, _refRot) / _angleTolerance > 1.0)) then
				_SpawnRings(1, nil)
			else
				_UpdateRings(_nRings - 1, 1, nil);
			if (not Assigned(_ref)) or (not Assigned(_ref^.parent)) then Break;
		end;
		if _active or (_nRings > 0) then
		begin
			_recalcBndAndSweepTimeout -= dt;
			if _recalcBndAndSweepTimeout < 0.0 then _RecalculateBoundingAndSweep;
		end else
			Detach;
	leave_call
	end;

	function Trail._SuitsTo(know: SceneKnowledge): boolean;
	begin
		case know of
			scene_Update: result := yes;
			else
				result := inherited _SuitsTo(know);
		end;
	end;

	function Trail._Initialize(dese: boolean): boolean;
	var
		mesh: pMesh;
		rings: array of tRingRec;
	begin
		result := no;
		_nextRingTimeout := 0.0;
		_refPos := Vec3.Zero;
		_refRotCheckPos := Vec3.Zero;
		_refRot := Quaternion.Identity;

		mesh := new(pMesh, Init({$ifdef Debug}'trail'{$endif}));
		_batch := mesh^.AddBatch('');
		_batch^.AddVA('centerLife', GLType.Vec4);
		if MMSystem.gl.GeometryShaderSupported then
		begin
			_batch^.AddVA('segRot', GLType.Vec4);
			if trail_Textured in _flags then _batch^.AddVA('tex', GLType.Float);
		end else
		begin
			_batch^.AddVA('ofs', GLType.Vec3);
			if trail_Textured in _flags then _batch^.AddVA('tex', GLType.Vec2);
		end;
		_vaCenterLife := _batch^.FindVA('centerLife');
		if MMSystem.gl.GeometryShaderSupported then
			_vaSegRot := _batch^.FindVA('segRot')
		else
			_vaOfs := _batch^.FindVA('ofs');
		if trail_Textured in _flags then _vaTex := _batch^.FindVA('tex');

		_glmesh.Init(mesh, yes); _glmesh.MakeStatic;
		if MMSystem.gl.GeometryShaderSupported then
			_glmesh.topology := GLtopology_LineStrip
		else
			_glmesh.topology := GLtopology_TriStrip;

		_recalcBndAndSweepTimeout := 0.0;
		_ro := MakeRef(new(pRenderObject, Init(@_glmesh, _mat, [robj_NoStartBnd])));
		if not Assigned(_ro) then exit;
		_ro^.Serializable := no;
		Attach(_ro);

		case _topology of
			topology_Star: gl^.flags[FLAG_STAR] := yes;
			topology_Tube: gl^.flags[FLAG_TUBE] := yes;
		end;
		if trail_Textured in _flags then gl^.flags[FLAG_TEXTURED] := yes;
		_PrepareTopology;

		if dese then
		begin
			rings := _rings;
			SetLength(rings, _nRings);
			_rings := nil;
			_nRings := 0;
			_SpawnRings(length(rings), pRingRec(rings));
		end else
		begin
			_ref := nil;
			_refTransform := Transform.Identity;
			_nRings := 0;
			_rings := nil;
			_moveTolerance := 2.0;
			_angleTolerance := 0.2;
			_active := yes;
			_persistence := 3.0;
			if trail_Textured in _flags then _texYFactor := 1.0;
		end;
		_UpdatePersistence;
		if trail_Textured in _flags then _UpdateTexYFactor;
		result := yes;
	end;

	constructor Trail.Init(newMat: pGLMaterial; newTopology: Topology; newSegs: sint; newFlags: FlagSet);
	begin
		inherited Init;
		_mat := MakeRef(newMat);
		_topology := newTopology;
		_nSegs := newSegs;
		_flags := newFlags;
		if not _Initialize(no) then ConstructorFailed;
	end;

	destructor Trail.Done;
	begin
		Release(_ro);
		Release(_ref);
		Release(_mat);
		inherited Done;
		_glmesh.Done;
	end;

	procedure Trail.Break;
	begin
		_active := no;
		Release(_ref);
	end;

	procedure ParticleSystem.Particle.Initialize(var sys: ParticleSystem; dese: boolean);
	begin
		variation := GlobalRNG.Choose(sys._variationWeights);
		seed := GlobalRNG.GetFloat;
		spawnSize := sys._sizeDis.GenerateValue;
		endSize   := sys._endSizeDis.GenerateValue;
		life      := sys._lifeDis.GenerateValue;
		if not dese then
		begin
			spawnTime := mm.SceneTime;
			spawnPos := Vec3.Zero;
			spawnVel := Vec3.Zero;
			constraint := Plane.None;
		end;
	end;

	function ParticleSystem.Particle.EstimatePosition(var sys: ParticleSystem; const aDt: float): Vec3;
	var
		dt, dist: float;
	begin
		dt := min(aDt, life);
		result := spawnPos + spawnVel * dt;
		if particles_UseAcceleration in sys._flags then result := result + sys._acceleration * sqr(dt);
		if particles_ConstraintWithPlane in sys._flags then
		begin
			dist := constraint.SignedDistance(result) - 0.5 * spawnSize;
			if dist < 0.0 then result := result - constraint.normal * dist;
		end;
	end;

	procedure ParticleSystem._SetRef(aRef: pSceneNode);
	begin
		if _ref <> aRef then
		begin
			if not Assigned(_ref) then _lastRefPos := aRef^.WorldPos;
			SetRef(_ref, aRef);
		end;
	end;

	procedure ParticleSystem._Spawn(n: uint; local: boolean = no; explicit: pParticle = nil);
	var
		sv, si: uint;
		i, j, v, ind: uint;
		p: pParticle;
		t: float;
		newBnd: Bounding;
	begin
		if n <= 0 then exit;
		if Assigned(_ro^.bnd) then newBnd := _bnd;

		sv := _batch^.VerticesCount;
		si := _batch^.inds.Count;
		_batch^.VerticesCount := _batch^.VerticesCount + sint(_vtsPerParticle * n);
		_batch^.inds.Count := _batch^.inds.Count + sint(_idsPerParticle * n);
		v := sv;
		ind := si;

		if local then
		begin
			Assert(particles.n = 0);
			particles.SetExactLength(n);
			p := particles.items;
		end else
			p := particles.Grow(n);
	{$ifdef Debug} stat.Note(max_particles_in_system, particles.n); {$endif}

		for i := 0 to n - 1 do
		begin
			if Assigned(explicit) then
			begin
				p[i] := explicit[i];
				p[i].Initialize(self, yes);
			end else
			begin
				p[i].Initialize(self, no);
				CallOnSpawn(p[i]);
			end;

			if (not local) and _hasPrev and (i < n - 1) and not Assigned(explicit) then
			begin
				p[i].spawnPos := lerp(_prevPos, p[i].spawnPos, i / (n-1));
				p[i].spawnVel := lerp(_prevVel, p[i].spawnVel, i / (n-1));
			end;

			if (i = 0) and (not Assigned(_ro^.bnd) or (particles.n = n)) then
				newBnd := Bounding.ByAABB(p[i].spawnPos - Vec3.Make(p[i].spawnSize), p[i].spawnPos + Vec3.Make(p[i].spawnSize))
			else
				newBnd.Enlarge(p[i].spawnPos, p[i].spawnSize);

			if local then t := p[i].life else t := RecalcBndAndSweepTimeout;
			newBnd.Enlarge(p[i].EstimatePosition(self, t), p[i].endSize);

			if local then
				p[i].spawnTime := i / n * LocalFactor * _lifeDis.EstimateMaximum
			else
				if Assigned(explicit) then p[i].spawnTime := mm.SceneTime - p[i].spawnTime;

			for j := 0 to _vtsPerParticle - 1 do
			begin
				_vaSpawnPosAndSize^.RawVec4[v + j] := Vec4.Make(p[i].spawnPos, p[i].spawnSize);
				_vaSpawnVel^.RawVec3[v + j] := p[i].spawnVel;
				_vaTSVIL^.RawVec4[v + j] := Vec4.Make(p[i].spawnTime, p[i].endSize - p[i].spawnSize, p[i].variation + 0.5 * p[i].seed, 1.0 / p[i].life);
				if Assigned(_vaConstraintPlane) then
					_vaConstraintPlane^.RawVec4[v + j] := p[i].constraint.v4;
				if Assigned(_vaCornerID) then
					_vaCornerID^.RawFloat[v + j] := j;
			end;

			if _idsPerParticle = 1 then
				_batch^.inds[ind + 0] := uint(v)
			else
				for j := 0 to _idsPerParticle - 1 do
					_batch^.inds[ind + j] := uint(v) + TriQuadInds[j];

			v += _vtsPerParticle;
			ind += _idsPerParticle;
		end;

		if local then
		begin
			_ro^.StartBnd := nil;
			_bnd := newBnd;
			_ro^.StartBnd := @_bnd;
		{$ifdef Debug} Log(lang_amount(n, 'Создан{а/ы/о} {N} частиц{а/ы/} статической системы'), logDebug); {$endif}
		end else
		begin
			if not Assigned(explicit) then
			begin
				_hasPrev := yes;
				_prevPos := p[n-1].spawnPos;
				_prevVel := p[n-1].spawnVel;
			end;

			_glmesh.PartialChangedVA(0, _vaSpawnPosAndSize, sv, _vtsPerParticle * n);
			_glmesh.PartialChangedVA(0, _vaSpawnVel, sv, _vtsPerParticle * n);
			_glmesh.PartialChangedVA(0, _vaTSVIL,    sv, _vtsPerParticle * n);
			if Assigned(_vaConstraintPlane) then
				_glmesh.PartialChangedVA(0, _vaConstraintPlane, sv, _vtsPerParticle * n);
			if Assigned(_vaCornerID) then _glmesh.PartialChangedVA(0, _vaCornerID, sv, _vtsPerParticle * n);
			_glmesh.PartialChangedInds(0, 0, si, _idsPerParticle * n);
			if Assigned(_ro^.bnd) then _ro^.ChangeBounding(newBnd) else
			begin
				_bnd := newBnd;
				_ro^.Bnd := @_bnd;
			end;
		end;
	end;

	procedure _CallOnSpawn(const info: SingleDelegateInfo; param: pointer);
	var
		args: ParticleSystem.pOnSpawnArgs absolute param;
	begin
		with args^ do ParticleSystem.OnSpawnProc(info.proc)(sys^, particle^, info);
	end;

	procedure ParticleSystem.CallOnSpawn(var p: Particle);
	var
		args: OnSpawnArgs;
	begin
		args.sys := @self;
		args.particle := @p;
		onSpawn.Call(@_CallOnSpawn, @args);
	end;

	procedure ParticleSystem._Assign(to_, from: uint);
		procedure AssignOne(va: pNativeGLValue);
		begin
			va^.CopyRange(_vtsPerParticle * from, _vtsPerParticle * to_, _vtsPerParticle);
			_glmesh.PartialChangedVA(0, va, _vtsPerParticle * to_, _vtsPerParticle);
		end;
	begin
		if to_ = from then exit;
		AssignOne(_vaSpawnPosAndSize);
		AssignOne(_vaSpawnVel);
		AssignOne(_vaTSVIL);
		if Assigned(_vaConstraintPlane) then AssignOne(_vaConstraintPlane);
		if Assigned(_vaCornerID       ) then AssignOne(_vaCornerID);
	end;

	procedure ParticleSystem._RecalculateBoundingAndSweep;
	var
		dt: float;
		i: uint;
		nb: sint;
		newBnd: Bounding;
		np: Vec3;
	begin
		Assert(not (particles_Local in flags));
		nb := 0;

		i := particles.n;
		while i > 0 do
		begin
			dec(i);
			dt := mm.SceneTimeSince(particles.items[i].spawnTime);
			if dt < particles.items[i].life then
			begin
				np := particles.items[i].EstimatePosition(self, dt);
				if nb > 0 then
					newBnd.Enlarge(np, particles.items[i].spawnSize)
				else
					newBnd := Bounding.BySphere(np, particles.items[i].spawnSize);
				newBnd.Enlarge(particles.items[i].EstimatePosition(self, dt + RecalcBndAndSweepTimeout));
				inc(nb);
			end else
			begin
				_Assign(i, particles.n - 1);
				particles.RemoveReplace(i);
				_batch^.VerticesCount := _batch^.VerticesCount - _vtsPerParticle;
				_batch^.inds.Count := _batch^.inds.Count - _idsPerParticle;
			end;
		end;

		if nb > 0 then
			if Assigned(_ro^.Bnd) then
				_ro^.ChangeBounding(newBnd)
			else
			begin
				_bnd := newBnd;
				_ro^.Bnd := @_bnd;
			end;
		_recalcBndAndSweepTimeout := RecalcBndAndSweepTimeout;
	end;

	function ParticleSystem._GetGL: pGLEntityParams;
	begin
		result := @_ro^.gl;
	end;

	procedure ParticleSystem._OnUpdate(const dt: float);
	const
		MaxSpawnAtATime = 80;
	var
		d: float;
		n: sint;
	begin
		inherited _OnUpdate(dt);
		if (particles_Local in _flags) then
		begin
			if particles.n = 0 then
			begin
				_Spawn(1 + round(LocalFactor * _lifeDis.EstimateMaximum * _temporalFrequency), yes);
				_SetupLocal;
			end;
			exit;
		end;

		if _active then
		begin
			_spawnTimeout -= dt;
			if Assigned(_ref) then
				if _hasPrev then
				begin
					d := SqrDistance(_ref^.WorldPos, _lastRefPos);
					if d > sqr(_spatialTolerance) then
					begin
						n := round(min(sqrt(d) / _spatialTolerance, MaxSpawnAtATime));
						_Spawn(n);
						_lastRefPos := _ref^.WorldPos;
					end;
				end else
					_lastRefPos := _ref^.WorldPos;

			if _spawnTimeout <= 0.0 then
			begin
				n := round(min(1 - _spawnTimeout * _temporalFrequency, MaxSpawnAtATime));
				_Spawn(n);
				_spawnTimeout += n * _temporalTolerance;
				if _spawnTimeout <= 0.0 then _spawnTimeout := _temporalTolerance;
			end;

			if (not Assigned(_ref)) or (not Assigned(_ref^.parent)) then Break;
		end;

		if _active or (particles.n > 0) then
		begin
			_recalcBndAndSweepTimeout -= dt;
			if _recalcBndAndSweepTimeout <= 0.0 then _RecalculateBoundingAndSweep;
		end else
			Detach;
	end;

	function ParticleSystem._SuitsTo(know: SceneKnowledge): boolean;
	begin
		case know of
			scene_Update: result := (inherited _SuitsTo(know)) or (not (particles_Local in _flags)) or (particles.n = 0);
			else
				result := inherited _SuitsTo(know);
		end;
	end;

	procedure ParticleSystem._InitStatic;
	begin
		if MMSystem.gl.GeometryShaderSupported then
		begin
			_vtsPerParticle := 1;
			_idsPerParticle := 1;
		end else
		begin
			_vtsPerParticle := 4;
			_idsPerParticle := 6;
		end;
	end;

	constructor ParticleSystem.Init(newFlags: ParticleSystemFlags; newMat: pGLMaterial);
	begin
		inherited Init;
		_flags := newFlags;
		_mat   := MakeRef(newMat);
		if not _Initialize(no) then ConstructorFailed;
	end;

	function ParticleSystem._Initialize(dese: boolean): boolean;
	var
		mesh: pMesh;
		ps: ParticlesList;
	begin
		result := no;
		if _vtsPerParticle = 0 then _InitStatic;

		_hasPrev := no;
		_spawnTimeout := 0.0;
		_lastRefPos := Vec3.Zero;

		mesh := new(pMesh, Init({$ifdef Debug}'particles'{$endif}));
		_batch := mesh^.AddBatch('');

		_batch^.AddVA('spawnPosAndSize', GLType.Vec4);
		_batch^.AddVA('spawnVel', GLType.Vec3);
		_batch^.AddVA('TSVIL', GLType.Vec4);
		if particles_ConstraintWithPlane in _flags then
			_batch^.AddVA('constraintPlane', GLType.Vec4);
		_batch^.AddVA('cornerID', GLType.Float);
		_vaSpawnPosAndSize := _batch^.FindVA('spawnPosAndSize');
		_vaSpawnVel := _batch^.FindVA('spawnVel');
		_vaTSVIL    := _batch^.FindVA('TSVIL');
		if particles_ConstraintWithPlane in _flags then
			_vaConstraintPlane := _batch^.FindVA('constraintPlane')
		else
			_vaConstraintPlane := nil;
		_vaCornerID := _batch^.FindVA('cornerID');

		_recalcBndAndSweepTimeout := 0.0;
		_glmesh.Init(mesh, yes); _glmesh.MakeStatic;
		_ro := MakeRef(new(pRenderObject, Init(@_glmesh, _mat, [robj_NoStartBnd])));
		if not Assigned(_ro) then exit;
		_ro^.Serializable := no;
		Attach(_ro);

		if particles_UseAcceleration in _flags then
		begin
			gl^.flags[FLAG_ACCELERATION] := yes;
			gl^.values.Value('acceleration', GLType.Vec3, 1, [NativeGLValueFlag.NonSerializable])^.SetVec3(_acceleration);
		end;
		if particles_ConstraintWithPlane in _flags then
			gl^.flags[FLAG_PLANE_CONSTRAINT] := yes;
		if particles_Local in _flags then
			gl^.flags[FLAG_LOCAL] := yes;

		if MMSystem.gl.GeometryShaderSupported then
			_glmesh.topology := GLtopology_Points;

		if dese then
		begin
			_UpdateVariations;
			_UpdateTemporalFrequency;
			_UpdateSpatialFrequency;
			ps := particles;
			particles.Init;
			_Spawn(ps.n, particles_Local in _flags, ps.items);
			if (particles_Local in _flags) and (particles.n > 0) then _SetupLocal;
			ps.Done;
		end else
		begin
			_variations := nil;
			particles.Init;
			_lifeDis := Distribution.Bell(0.1, 0.5, 1.0);
			_sizeDis := Distribution.Bell(0.1, 0.5, 1.0);
			_endSizeDis := _sizeDis.Copy;
			_active := yes;
			onSpawn.Init;
			_temporalFrequency := 1.0; _UpdateTemporalFrequency;
			_spatialFrequency  := 1.0; _UpdateSpatialFrequency;
			_acceleration := Vec3.Zero;
			_ref := nil;
		end;
		result := yes;
	end;

	destructor ParticleSystem.Done;
	begin
		_lifeDis.Done;
		_sizeDis.Done;
		_endSizeDis.Done;
		Release(_ref);
		Release(_ro);
		Release(_mat);
		onSpawn.Done;
		particles.Done;
		inherited Done;
		_glmesh.Done;
	end;

	procedure ParticleSystem.Break;
	begin
		_active := no;
		Release(_ref);
	end;

	procedure ParticleSystem.AddVariation(const newTexA, newTexB: Vec2; const weight: float);
	begin
		SetLength(_variations, length(_variations) + 1);
		with _variations[High(_variations)] do
		begin
			texA := newTexA;
			texB := newTexB;
		end;
		SetLength(_variationWeights, length(_variationWeights) + 1);
		_variationWeights[High(_variationWeights)] := weight;
		_UpdateVariations;
	end;

	procedure ParticleSystem._SetTemporalFrequency(const newFreq: float);
	begin
		if not Equals(_temporalFrequency, newFreq) then
		begin
			_temporalFrequency := newFreq;
			_UpdateTemporalFrequency;
		end;
	end;

	procedure ParticleSystem._SetSpatialFrequency(const newFreq: float);
	begin
		if not Equals(_spatialFrequency, newFreq) then
		begin
			_spatialFrequency := newFreq;
			_UpdateSpatialFrequency;
		end;
	end;

	procedure ParticleSystem._SetupLocal;
	begin
		gl^.values.Value('period', GLType.Float, 1, [NativeGLValueFlag.NonSerializable])^.SetFloat(LocalFactor * _lifeDis.EstimateMaximum);
		_glmesh.ConvertToStatic;
		KnowledgeMayChange(scene_Update, no);
	end;

	procedure ParticleSystem._SetAcceleration(const newAcc: Vec3);
	begin
		Assert(particles_UseAcceleration in _flags);
		if _acceleration <> newAcc then
		begin
			_acceleration := newAcc;
			gl^.values.Value('acceleration')^.SetVec3(_acceleration);
		end;
	end;

	procedure ParticleSystem._UpdateVariations;
	var
		v: pNativeGLValue;
		i: sint;
	begin
		v := gl^.values.Value('variations', GLType.Vec4, length(_variations), [NativeGLValueFlag.NonSerializable]);
		for i := 0 to High(_variations) do
			v^.SetVec4(Vec4.Make(_variations[i].texA.x, _variations[i].texA.y, _variations[i].texB.x, _variations[i].texB.y), i);
	end;

	procedure ParticleSystem._UpdateTemporalFrequency;
	begin
		if NotZero(_temporalFrequency) then
			_temporalTolerance := 1.0 / _temporalFrequency
		else
			_temporalTolerance := 1.0;
	end;

	procedure ParticleSystem._UpdateSpatialFrequency;
	begin
		if NotZero(_spatialFrequency) then
			_spatialTolerance := 1.0 / _spatialFrequency
		else
			_spatialTolerance := 1.0;
	end;

	// очень неоптимальная сетка, не использовать!!!
	// TODO: запилить через прямоугольники, а не только квадраты
	procedure GenerateMengerSponge(depth: sint; const size: float; out gmain, gholes: pGLMesh);
	type
		tSides = set of byte;
	var
		main, holes: pMesh;
		mainAB, holesAB: PosABConverter;
		b_main, b_holes: pBatch;
		i_main, i_holes: pMeshIndices;
		main_posAB, main_normal, main_color: pNativeGLValue;
		hole_posAB, hole_normal, hole_color: pNativeGLValue;

		procedure generate(depth: sint; const A, B: Vec3; include: tSides);
		const
			// 0-fw, 1-bk, 2-lt, 3-rt, 4-up, 5-dn
			Includes: array[0 .. 2, 0 .. 2, 0 .. 2] of tSides =
			(
				(
					([1, 2, 5],    [2, 3, 4, 5], [0, 2, 5]),
					([0, 1, 2, 3], [],           [0, 1, 2, 3]),
					([1, 2, 4],    [2, 3, 4, 5], [0, 2, 4])
				),
				(
					([0, 1, 4, 5], [],           [0, 1, 4, 5]),
					([],           [],           []),
					([0, 1, 4, 5], [],           [0, 1, 4, 5])
				),
				(
					([1, 3, 5],    [2, 3, 4, 5], [0, 3, 5]),
					([0, 1, 2, 3], [],           [0, 1, 2, 3]),
					([1, 3, 4],    [2, 3, 4, 5], [0, 3, 4])
				)
			);
			Sides: array[0 .. 5] of record
				normal, mainColor, holeColor: Vec3;
			end =
			(
				(normal: (data: (0.0, 0.0, 1.0));  mainColor: (data: (1.0, 0.0, 0.0)); holeColor: (data: (0.0, 1.0, 1.0))),
				(normal: (data: (0.0, 0.0, -1.0)); mainColor: (data: (0.0, 1.0, 0.0)); holeColor: (data: (1.0, 0.0, 1.0))),
				(normal: (data: (-1.0, 0.0, 0.0)); mainColor: (data: (0.0, 0.0, 1.0)); holeColor: (data: (1.0, 1.0, 0.0))),
				(normal: (data: (1.0, 0.0, 0.0));  mainColor: (data: (1.0, 1.0, 0.0)); holeColor: (data: (0.0, 0.0, 1.0))),
				(normal: (data: (0.0, 1.0, 0.0));  mainColor: (data: (1.0, 0.0, 1.0)); holeColor: (data: (0.0, 1.0, 0.0))),
				(normal: (data: (0.0, -1.0, 0.0)); mainColor: (data: (0.0, 1.0, 1.0)); holeColor: (data: (1.0, 0.0, 0.0)))
			);
			Holes: array[0 .. 5, 0 .. 3, 0 .. 2] of sint =
			(
				((1, 2, 3), (1, 1, 3), (2, 1, 3), (2, 2, 3)),
				((2, 2, 0), (2, 1, 0), (1, 1, 0), (1, 2, 0)),
				((0, 2, 1), (0, 1, 1), (0, 1, 2), (0, 2, 2)),
				((3, 2, 2), (3, 1, 2), (3, 1, 1), (3, 2, 1)),
				((1, 3, 1), (1, 3, 2), (2, 3, 2), (2, 3, 1)),
				((1, 0, 2), (1, 0, 1), (2, 0, 1), (2, 0, 2))
			);
		var
			d: array[0 .. 3] of Vec3;
			i, j, x, y, z, sv, si: sint;
			t: Vec3;
		begin
			d[0] := A;
			d[1] := A + (B - A) * 0.33;
			d[2] := A + (B - A) * 0.66;
			d[3] := B;

			if depth > 0 then
			begin
				for x := 0 to 2 do
					for y := 0 to 2 do
						for z := 0 to 2 do
							if uint(x <> 1) + uint(y <> 1) + uint(z <> 1) >= 2 then
								generate(depth - 1, Vec3.Make(d[x].x, d[y].y, d[z].z), Vec3.Make(d[x + 1].x, d[y + 1].y, d[z + 1].z), includes[x, y, z]);

				sv := b_holes^.VerticesCount;
				si := i_holes^.Count;
				b_holes^.VerticesCount := sv + length(Holes) * length(Holes[0]);
				i_holes^.Count := si + length(Holes) * length(TriQuadInds);

				for i := 0 to High(Holes) do
				begin
					for j := 0 to High(Holes[0]) do
					begin
						hole_posAB^.AsVec3[sv + length(Holes[0])*i + j] := holesAB.AB(d[Holes[i, j, x]]);
						hole_normal^.AsVec3[sv + length(Holes[0])*i + j] := Sides[i].normal;
						hole_color^.AsVec3[sv + length(Holes[0])*i + j] := Sides[i].holeColor;
					end;

					for j := 0 to High(TriQuadInds) do
						i_holes^[si + i * length(TriQuadInds) + j] := uint(sv + 4*i) + TriQuadInds[j];
				end;
			end else
			begin
				for i := 0 to 5 do
					if i in include then
					begin
						sv := b_main^.VerticesCount;
						si := i_main^.Count;
						b_main^.VerticesCount := sv + 4;
						i_main^.Count := si + length(TriQuadInds);

						for j := 0 to 3 do
						begin
							for x := 0 to 2 do
								t.data[x] := d[0 + 3*(Holes[i, j, x] div 2)].data[x];
							main_posAB^.AsVec3[sv + j] := mainAB.AB(t);
							main_normal^.AsVec3[sv + j] := Sides[i].normal;
							main_color^.AsVec3[sv + j] := Sides[i].mainColor;
						end;

						for j := 0 to High(TriQuadInds) do
							i_main^[si + j] := uint(sv) + TriQuadInds[j];
					end;
			end;
		end;

	begin
		main  := new(pMesh, Init({$ifdef Debug}'sponge-main'{$endif}));
		holes := new(pMesh, Init({$ifdef Debug}'sponge-holes'{$endif}));

		main^.Bounding := Bounding.ByAABB(Vec3.Make(-0.5 * size), Vec3.Make(0.5 * size));
		mainAB := PosABConverter.Prepare(main^.Bounding);
		holes^.Bounding := main^.Bounding;
		holesAB := PosABConverter.Prepare(holes^.Bounding);

		b_main := main^.AddBatch('');
		b_main^.AddVA('posAB', GLType.Vec3Nui16);
		b_main^.AddVA('normal', GLType.Vec3Ni8);
		b_main^.AddVA('color', GLType.Vec3Nui8);
		main_posAB  := b_main^.FindVA('posAB');
		main_normal := b_main^.FindVA('normal');
		main_color  := b_main^.FindVA('color');
		i_main := @b_main^.inds;

		b_holes := holes^.AddBatch('');
		b_holes^.AddVA('posAB', GLType.Vec3Nui16);
		b_holes^.AddVA('normal', GLType.Vec3Ni8);
		b_holes^.AddVA('color', GLType.Vec3Nui8);
		hole_posAB  := b_holes^.FindVA('posAB');
		hole_normal := b_holes^.FindVA('normal');
		hole_color  := b_holes^.FindVA('color');
		i_holes := @b_holes^.inds;

		generate(depth, Vec3.Make(-0.5 * size), Vec3.Make(0.5 * size), [0 .. 5]);
		gmain := new(pGLMesh, Init(main, no));
		gholes := new(pGLMesh, Init(holes, no));
	end;

	function MazePattern.CellRef.Make(newX, newY: uint; newCell: pCell): CellRef;
	begin
		result.x := newX;
		result.y := newY;
		result.cell := newCell;
	end;

	function MazePattern.CellRef.First(var mp: MazePattern): CellRef; begin result := Make(0, 0, pCell(mp.cells)); end;

	function MazePattern.CellRef.Next(var mp: MazePattern): boolean;
	begin
		if x + 1 < mp.sizeX then
		begin
			inc(x); inc(cell);
			result := yes;
		end else if y + 1 < mp.sizeY then
		begin
			x := 0; inc(y); inc(cell);
			result := yes;
		end else
			result := no;
	end;

	function MazePattern.CellRef.ShiftLeft(var mp: MazePattern): CellRef ; begin result := Make(x - 1, y, cell - 1); Assert(mp.ValidatePoint(result.x, result.y)); end;
	function MazePattern.CellRef.ShiftRight(var mp: MazePattern): CellRef; begin result := Make(x + 1, y, cell + 1); Assert(mp.ValidatePoint(result.x, result.y)); end;
	function MazePattern.CellRef.ShiftUp(var mp: MazePattern): CellRef   ; begin result := Make(x, y - 1, cell - mp.sizeX); Assert(mp.ValidatePoint(result.x, result.y)); end;
	function MazePattern.CellRef.ShiftDown(var mp: MazePattern): CellRef ; begin result := Make(x, y + 1, cell + mp.sizeX); Assert(mp.ValidatePoint(result.x, result.y)); end;

	function MazePattern.CellRef.Shift(const dir: Dir4; var mp: MazePattern): CellRef;
	begin
		case dir.value of
			_Left:  result := Make(x - 1, y, cell - 1);
			_Right: result := Make(x + 1, y, cell + 1);
			_Up:    result := Make(x, y - 1, cell - mp.sizeX);
			else    result := Make(x, y + 1, cell + mp.sizeX);
		end;
		Assert(mp.ValidatePoint(result.x, result.y));
	end;

	function MazePattern.CellRef.Obstructed(var mp: MazePattern): boolean;
	begin
		result := (HasWallRight in cell^.flags) or (HasWallDown in cell^.flags)
		          or ((x > 0) and (HasWallRight in ShiftLeft(mp).cell^.flags))
		          or ((y > 0) and (HasWallDown in ShiftUp(mp).cell^.flags));
	end;

	function MazePattern.CellRef.HasWall(const dir: Dir4; var mp: MazePattern): boolean;
	begin
		case dir.value of
			_Left:  result := WallLeft(mp);
			_Up:    result := WallUp(mp);
			_Right: result := WallRight(mp);
			else    result := WallDown(mp);
		end;
	end;

	function MazePattern.CellRef.WallLeft(var mp: MazePattern): boolean ; begin result := (x > 0) and (HasWallRight in ShiftLeft(mp).cell^.flags); end;
	function MazePattern.CellRef.WallUp(var mp: MazePattern): boolean   ; begin result := (y > 0) and (HasWallDown in ShiftUp(mp).cell^.flags); end;
	function MazePattern.CellRef.WallRight(var mp: MazePattern): boolean; begin result := (HasWallRight in cell^.flags); Assert(@mp = @mp); end;
	function MazePattern.CellRef.WallDown(var mp: MazePattern): boolean ; begin result := (HasWallDown in cell^.flags); Assert(@mp = @mp); end;

	function MazePattern.CellRef.WallPseudographics(var mp: MazePattern): StringView;
	type
		s = string;
	const
		Walls: array[boolean, boolean, boolean, boolean] of s = // left, up, right, down
		({!L} ({!U} ({!R} ({!D} s(''),  {D} s('│')), {R} ({!D} s('─'), {D} s('┌'))), {U} ({!R} ({!D} s('│'), {D} s('│')), {R} ({!D} s('└'), {D} s('├')))),
			{L} ({!U} ({!R} ({!D} s('─'), {D} s('┐')), {R} ({!D} s('─'), {D} s('┬'))), {U} ({!R} ({!D} s('┘'), {D} s('┤')), {R} ({!D} s('┴'), {D} s('┼')))));
	begin
		result := @Walls[WallLeft(mp), WallUp(mp), WallRight(mp), WallDown(mp)];
	end;

	function MazePattern.CellRef.PrevPseudographics: StringView;
	const
		Syms: array[Dir4.Enum] of string = (string('<'), string('^'), string('>'), string('v'));
	begin
		if CellHasPrev in cell^.flags then
			result := @Syms[Dir4.Enum(uint(CellPrevBit0 in cell^.flags) or (uint(CellPrevBit1 in cell^.flags) shl 1))]
		else
			result := StringView.Empty;
	end;

	function MazePattern.Create(newSx, newSy, newMaxStage: uint; start: StartProc; startParam: pointer; update: UpdateCallback; updateParam: pointer): MazePattern;
	const
		Frequency = 0.2;
	var
		c: CellRef;
		gps: GrowingPointsArray;
		i: sint;
	begin
	FPC_3_BUG System.Initialize(result);
		result.sx := RangeCheckMin(newSx, 1, 'MazePattern.sizeX');
		result.sy := RangeCheckMin(newSy, 1, 'MazePattern.sizeY');
		result.maxstg := RangeCheck(newMaxStage, 1, 1000, 'MazePattern.maxStage');
		result.rng.Init(Crawl);
		SetLength(result.cells, result.sizeX * result.sizeY);
		c := CellRef.First(result);
		repeat
			c.cell^ := Cell_t.Empty;
			start(c.x, c.y, c.cell - pCell(result.cells), c.cell^.flags, startParam);
			Assert(c.cell^.flags - [CellIsProhibited, CellIsGrowSource, CellIsForced] = []);
		until not c.Next(result);

		result.q.Init;
		result.PlaceWalls(CellPredicate.Make([CellIsForced], yes), CellPredicate.Make([CellIsForced], no), Frequency, nil, update, updateParam);
		result.PlaceWalls(CellPredicate.Make([CellIsGrowSource], no), MainGrow, Frequency, @gps, nil, nil);
		for i := 0 to High(gps) do
			result.PropagateAround(gps[i].cell, gps[i].dir, gps[i].delay * sint(result.maxStage));
	end;

	procedure MazePattern.Done;
	begin
		rng.Done;
		q.Done;
	FPC_3_BUG System.Finalize(self);
	end;

	function MazePattern.Step(amount: uint; update: UpdateCallback; param: pointer): StepResult;
	var
		count, op: uint;
		p: BreadthGp;
	begin
		result := [];
		if q.Count = 0 then result += [FrozenMaze];

		while amount > 0 do
		begin
			dec(amount);
			count := q.Count;
			while (count > 0) and q.Get(p) do
			begin
				dec(count);
				if p.delay > 0 then
				begin
					dec(p.delay);
					q.Put(p);
					continue;
				end;

				op := p.progress;
				inc(p.progress);
				SetStage(p.cell, p.dir, op, p.progress, update, param);

				if uint(p.progress) >= maxStage then
				begin
					p.cell := p.cell.Shift(p.dir, self);
					PropagateAround(p.cell, p.dir, 0);
				end else
					q.Put(p);
			end;
		end;
	end;

	function MazePattern.Dump(getsym: DumpProc; param: pointer): string;
	var
		sb: StringBuilder;
		cell: CellRef;
		sym: StringView;
	begin
		sb.Init;
		cell := CellRef.First(self);
		repeat
			sym.n := 0;
			if Assigned(getsym) then getsym(cell, sym, self, param);
			if sym.n = 0 then sym := cell.WallPseudographics(self);
			if (sym.n = 0) and (CellIsProhibited in cell.cell^.flags) then sym := StringView.Make('#');
			if (sym.n = 0) and (CellIsGrowSource in cell.cell^.flags) then sym := StringView.Make('X');
			if (sym.n = 0) and (CellIsForced in cell.cell^.flags) then sym := StringView.Make('~');
			if sym.n > 0 then sb.Append(sym.p, sym.n) else sb.Append(' ');
			if (cell.x + 1 = sizeX) and (cell.y + 1 < sizeY) then sb.Append(EOL);
		until not cell.Next(self);
		result := sb.DestructiveToString;
	end;

	function MazePattern.CellPredicate.Make(newFlags: CellFlags; newNeg: boolean): CellPredicate;
	begin
		result.flags := newFlags;
		result.neg  := newNeg;
	end;

	function MazePattern.CellPredicate.Match(const c: Cell_t): boolean; begin result := neg xor (flags * c.flags <> []); end;

	function MazePattern.GetCellRef(x, y: uint): CellRef;
	begin
		Assert(ValidatePoint(x, y));
		result := CellRef.Make(x, y, @cells[y * sizeX + x]);
	end;

	function MazePattern.GrowingPoint.Create(const newCell, newTarget: CellRef; const newDir: Dir4): GrowingPoint;
	begin
		result.cell   := newCell;
		result.target := newTarget;
		result.dir    := newDir;
		result.delay  := 0;
	end;

	function MazePattern.GrowingPoint.TryMove(nd: Dir4; const on: CellPredicate; var mp: MazePattern; out moved: GrowingPoint): boolean;
	var
		nt: CellRef;
	begin
		result := mp.ValidMove(cell.x, cell.y, nd);
		if result then
		begin
			nt := cell.Shift(nd, mp);
			result := on.Match(nt.cell^) and not nt.Obstructed(mp);
			if result then moved := Create(cell, nt, nd);
		end;
	end;

	function MazePattern.BreadthGp.Make(const newCell: CellRef; const newDir: Dir4; newDelay, newProgress: sint): BreadthGp;
	begin
		result.cell := newCell;
		result.dir  := newDir;
		result.delay := newDelay;
		result.progress := newProgress;
	end;

	function MazePattern.ValidatePoint(x, y: uint): boolean;
	begin
		result := (x < sizeX) and (y < sizeY);
	end;

	function MazePattern.ValidMove(x, y: uint; const dir: Dir4): boolean;
	begin
		result := ((dir.value = _Left) and (x >= 1)) or ((dir.value = _Right) and (x + 1 < sizeX))
				or ((dir.value = _Up) and (y >= 1)) or ((dir.value = _Down) and (y + 1 < sizeY));
	end;

type
	PickPointContext = record
		all, sel: MazePattern.pGrowingPointsArray;
		n: uint;
	end;

	procedure PickPoint(id: uint; param: pointer);
	var
		ctx: ^PickPointContext absolute param;
	begin
		ctx^.sel^[ctx^.n] := ctx^.all^[id];
		inc(ctx^.n);
	end;

	procedure MazePattern.PlaceWalls(const from, on: CellPredicate; const frequency: float; startGps: pGrowingPointsArray; update: UpdateCallback; param: pointer);
	var
		gps: GrowingPointsArray;
		nGps: uint;
	begin
		gps := PlaceGrowingPoints(frequency, from, on);
		nGps := length(gps);
		if Assigned(startGps) then startGps^ := Copy(gps, 0, nGps);
		repeat until not GrowStep(on, gps, nGps, update, param);
	end;

	function MazePattern.PlaceGrowingPoints(const frequency: float; const from, on: CellPredicate): GrowingPointsArray;

		procedure Test(const a, b: CellRef; const dir: Dir4; var all: GrowingPointsArray; var nAll: uint);
		begin
			if on.Match(b.cell^) then
			begin
				inc(nAll);
				if nAll > uint(length(all)) then SetLength(all, 2 * nAll);
				all[nAll - 1] := GrowingPoint.Create(a, b, dir);
			end;
		end;

	var
		nAll: uint;
		i: sint;
		t: CellRef;
		all: GrowingPointsArray;
		c: CellRef;
		pickCtx: PickPointContext;
	begin
		nAll := 0;
		SetLength(all, 2 * max(sizeX, sizeY));
		c := CellRef.First(self);
		repeat
			if from.Match(c.cell^) then
			begin
				//nprev := nAll;
				if c.x > 0         then begin t := c.ShiftLeft(self);  Test(c, t, Dir4.Left,  all, nAll); end;
				if c.x + 1 < sizeX then begin t := c.ShiftRight(self); Test(c, t, Dir4.Right, all, nAll); end;
				if c.y > 0         then begin t := c.ShiftUp(self);    Test(c, t, Dir4.Up,    all, nAll); end;
				if c.y + 1 < sizeY then begin t := c.ShiftDown(self);  Test(c, t, Dir4.Down,  all, nAll); end;
				//if nAll > nprev + 1 then
				//begin
				//  all[nprev + 1] := all[rng.GetUint(nprev + 1, nAll - 1)];
				//  nAll := nprev + 1;
				//end;
			end;
		until not c.Next(self);

		SetLength(result, uint(nAll > 0) + round((nAll - uint(nAll > 0)) * clamp(frequency, 0, 1)));
		pickCtx.n := 0;
		pickCtx.all := @all;
		pickCtx.sel := @result;
		SetLength(result, rng.RandomElements(nAll, length(result), @PickPoint, @pickCtx));
		for i := 0 to High(result) do
			result[i].delay := (min(uint(length(result)), 8) * i) div length(result);
	end;

	// Отрицательный GrowingPoint.delay пропускает запоминание ссылки на предыдущую клетку — выставляется отвевившимся,
	// чтобы не проходить корень по 100 раз. Инкрементируется назад перед циклом и проверяется в цикле, поэтому, чтобы пропустить 1 раз,
	// выставляется в -2.
	function MazePattern.GrowStep(const on: CellPredicate; var gps: GrowingPointsArray; var nGps: uint; update: UpdateCallback; param: pointer): boolean;
	// Точки роста могут дублироваться — дубликаты постепенно самовыпиливаются. Проверять дубликаты в TryMove?

		function TryBacktrack(var gp: GrowingPoint): boolean; // при любом результате портит gp
		var
			np: GrowingPoint;
			prevd: Dir4;
			d: Dir4.Enum;
		begin
			while CellHasPrev in gp.cell.cell^.flags do
			begin
				prevd := Dir4.Enum(uint(CellPrevBit0 in gp.cell.cell^.flags) or (uint(CellPrevBit1 in gp.cell.cell^.flags) shl 1));
				// gp.cell.cell^.flags -= [CellHasPrev, CellPrevBit0, CellPrevBit1];
				gp.cell := gp.cell.Shift(prevd, self);
				for d in Dir4.All - [prevd.Reverse.value] do
					if gp.TryMove(d, on, self, np) then
					begin
						gp := np;
						exit(yes);
					end;
			end;
			result := no;
		end;

	const
		BranchChance = 0.4;
		DieAfterBranchingChance = 0.96;
	var
		nd: Dir4;
		ngp, straight: GrowingPoint;
		i, origGps, rot: uint;
		canMoveStraight, ob, removed: boolean;
	begin
		result := no;
		i := 0;
		origGps := nGps;
		while i < origGps do
		begin
			if gps[i].delay > 0 then
			begin
				dec(gps[i].delay);
				inc(i);
				continue;
			end;
			if gps[i].delay < 0 then inc(gps[i].delay);
			ob := gps[i].target.Obstructed(self);

			if not ob then
			begin
				Assert(gps[i].target.cell^.flags * [HasWallRight, HasWallDown, CellHasPrev, CellPrevBit0, CellPrevBit1] = [],
						ToString(gps[i].target.x) + ', ' + ToString(gps[i].target.y) + ': ' +
						'R = ' + YesNoEn[HasWallRight in gps[i].target.cell^.flags] + ', D = ' + YesNoEn[HasWallDown in gps[i].target.cell^.flags] + ', '
						+ 'P = ' + YesNoEn[CellHasPrev in gps[i].target.cell^.flags] + ', P0 = ' + YesNoEn[CellPrevBit0 in gps[i].target.cell^.flags] + ', '
						+ 'P1 = ' + YesNoEn[CellPrevBit0 in gps[i].target.cell^.flags]);
				case gps[i].dir.value of
					_Right: gps[i].cell.cell^.flags += [HasWallRight];
					_Down:  gps[i].cell.cell^.flags += [HasWallDown];
					_Left:  gps[i].target.cell^.flags += [HasWallRight];
					else    gps[i].target.cell^.flags += [HasWallDown];
				end;
				if Assigned(update) then SetStage(gps[i].cell, gps[i].dir, 0, maxStage, update, param);
				result := yes;
				if gps[i].delay = 0 then
				begin
					gps[i].target.cell^.flags += [CellHasPrev];
					if ord(gps[i].dir.Reverse.value) and (1 shl 0) <> 0 then gps[i].target.cell^.flags += [CellPrevBit0];
					if ord(gps[i].dir.Reverse.value) and (1 shl 1) <> 0 then gps[i].target.cell^.flags += [CellPrevBit1];
				end;
				gps[i].cell := gps[i].target;
			end;

			canMoveStraight := not ob and gps[i].TryMove(gps[i].dir, on, self, straight);
			removed := no;
			if not canMoveStraight or rng.Roll(BranchChance) then
			begin
				nd := gps[i].dir;
				if rng.Coinflip then nd := nd.CCW else nd := nd.CW;
				for rot := 0 to 1 do
				begin
					if gps[i].TryMove(nd, on, self, ngp) then
					begin
						if rng.Roll(DieAfterBranchingChance) then
						begin
							gps[i] := ngp;
							removed := yes;
						end else
						begin
							ngp.delay := -2;
							inc(nGps);
							if nGps > uint(length(gps)) then SetLength(gps, 2 * nGps);
							gps[nGps - 1] := ngp;
						end;
						break;
					end else
						if rot = 0 then nd := nd.Reverse;
				end;
			end;

			if removed then inc(i)
			else if canMoveStraight then
			begin
				gps[i] := straight;
				inc(i);
			end else
				if TryBacktrack(gps[i]) then
					// трекнулось — повторить итерацию
				else
				begin
					gps[i] := gps[origGps - 1];
					gps[origGps - 1] := gps[nGps - 1];
					dec(nGps); dec(origGps);
				end;
		end;
	end;

	procedure MazePattern.PropagateAround(const cell: CellRef; const cameBy: Dir4; delay: sint);
	const
		StartStage = 0;
	var
		d: Dir4.Enum;
	begin
		for d in Dir4.All - [cameBy.Reverse.value] do
			if cell.HasWall(d, self) then
			begin
				cell.cell^.flags += [CellPassed];
				q.Put(MazePattern.BreadthGp.Make(cell, d, delay, StartStage));
			end;
	end;

	procedure MazePattern.SetStage(const cell: CellRef; const dir: Dir4; prev, stage: uint; update: UpdateCallback; param: pointer);
	begin
		if Assigned(update) then update(cell, self, dir, prev, stage, param);
	end;

	procedure UpdateWall(const cell: MazePattern.CellRef; var mp: MazePattern; const dir: Dir4; prevStage, newStage: uint; param: pointer);
	const
		WallColor = (%111 shl 5) or (%111 shl 2) or (%11 shl 0); // RGB332
		MaxStage = GLMazePattern.MaxStage;
	var
		gm: pGLMazePattern absolute param;
		x, y, sx, sy: uint;
	begin
		Assert(@mp = @mp);
		Assert(newStage > prevStage);

		case dir.value of
			_Left:  begin x := sint(MaxStage * cell.x) - newStage; y := MaxStage * cell.y; sx := newStage - prevStage; sy := 1; end;
			_Right: begin x := MaxStage * cell.x + prevStage + 1; y := MaxStage * cell.y; sx := newStage - prevStage; sy := 1; end;
			_Up:    begin x := MaxStage * cell.x; y := sint(MaxStage * cell.y) - newStage; sx := 1; sy := newStage - prevStage; end;
			_Down:  begin x := MaxStage * cell.x; y := MaxStage * cell.y + prevStage + 1; sx := 1; sy := newStage - prevStage; end;
		end;
		Assert((x + sx <= uint(gm^.tex^.Size.X)) and (y + sy <= uint(gm^.tex^.Size.Y)));
		repeat
			pUint8(gm^.texData.p)[(gm^.tex^.Size.Y - y - 1) * gm^.tex^.Size.X + x] := WallColor;
			if sx > 1 then begin inc(x); dec(sx); end else begin inc(y); dec(sy); end;
		until (sx = 0) or (sy = 0);
		gm^.texDirty := yes;
	end;

	constructor GLMazePattern.Init(sx, sy: uint; start: MazePattern.StartProc; startParam: pointer);
	begin
		inherited Init;
		tex := new(pTexture, Init(GLtexture_2D, UintVec2.Make(sint(MaxStage * sx) - 1, sint(MaxStage * sy) - 1), GLformat_RGB332, [{texture_DontFilter}], texture_Dynamic))^.NewRef;
		tex^.Wrap := GLwrap_Clamp;
		texData.n := tex^.info.GetLevelDataSize(0);
		texData.p := GetMem(texData.n);
		Zero(texData.p, texData.n);
		mp := MazePattern.Create(sx, sy, MaxStage, start, startParam, @UpdateWall, @self);
		UpdateTexture(yes);
	end;

	destructor GLMazePattern.Done;
	begin
		Release(tex);
		FreeMem(texData.p);
		mp.Done;
		inherited Done;
	end;

	procedure GLMazePattern.Bind(v: pGLEntityParams; const id: PoolString);
	begin
		v^.values.Value(id, GLType.Sampler)^.SetTex(tex);
	end;

	function GLMazePattern.Step(amount: uint): MazePattern.StepResult;
	begin
		result := mp.Step(amount, @UpdateWall, @self);
		UpdateTexture(no);
	end;

	procedure GLMazePattern.UpdateTexture(force: boolean);
	begin
		if not texDirty and not force then exit;
		tex^.SubImage(UintVec2.Zero, tex^.Size.XY, GLformat_RGB332, texData.n, texData.p);
		texDirty := no;
	end;

	procedure Script_CreateTrail(var ss: ScriptState);
	var
		t: pTrail;
		mat: pGLMaterial;
		topology: Trail.Topology;
		segs: sint;
		flags: Trail.FlagSet;
		flag: Trail.FlagEnum;
	begin
		flags := [];
		for flag in Trail.FlagEnum do
			if ss.GetBoolField(1, Trail.FlagIds[flag]) then
				flags += [flag];
		if ss.GetTableS(1, 'material') then
		begin
			mat := MakeRef(Script_create_material(ss, -1));
			ss.Pop;
		end else
			mat := nil;
		if ss.GetTableS(1, 'topology') then
		begin
			topology := Trail.Topology(FindStr(ss.ToString(-1), Trail.TopologyIds, ord(topology_Tube)));
			ss.Pop;
		end else
			topology := topology_Tube;
		if ss.GetTableS(1, 'segs') then
		begin
			segs := ss.ToSint(-1);
			ss.Pop;
		end else
			segs := Trail.DefaultSegs[topology];

		t := new(pTrail, Init(mat, topology, segs, flags));
		Release(mat);
		ss.PushObject(t);
		if not Assigned(t) then exit;

		if ss.GetTableS(1, 'ref') then
		begin
			t^.Ref := ss.ToObject(-1, TypeOf(SceneNode));
			ss.Pop;
		end;
		if ss.GetTableS(1, 'ref_transform') then
		begin
			t^.RefTransform := ss.ToTransform(-1);
			ss.Pop;
		end;
		if ss.GetTableS(1, 'gl') then
		begin
			Script_modify_gl(ss, -1, t^.gl^);
			ss.Pop;
		end;
		if ss.GetTableS(1, 'persistence') then
		begin
			t^.Persistence := ss.ToFloat(-1);
			ss.Pop;
		end;
		if ss.GetTableS(1, 'moveTolerance') then
		begin
			t^.MoveTolerance := ss.ToFloat(-1);
			ss.Pop;
		end;
		if ss.GetTableS(1, 'angleTolerance') then
		begin
			t^.AngleTolerance := ss.ToFloat(-1);
			ss.Pop;
		end;
		Script_common_create_scene_node(ss, t);
	end;

	procedure Script_Trail_Break(var ss: ScriptState);
	begin
		pTrail(ss.ToSelf)^.Break;
	end;

	function Script_Trail_GL(var ss: ScriptState): sint;
	begin
		result := Script_modify_gl_and_also_query(ss, 2, pTrail(ss.ToSelf)^.gl^);
	end;

{$define fname := Script_Trail_ref} {$define otype := Trail} {$define field := Ref} {$define prop_object := SceneNode} {$include script_prop.inc}

	function Script_GenerateMengerSponge(var ss: ScriptState): sint;
	var
		main, holes: pGLMesh;
	begin
		GenerateMengerSponge(ss.GetSintField(1, 'depth', 1), ss.GetFloatField(1, 'size', 1.0), main, holes);
		ss.PushObject(main);
		ss.PushObject(holes);
		result := 2;
	end;

	procedure OnSpawn(var sys: ParticleSystem; var particle: ParticleSystem.Particle; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(ParticleSystem.OnSpawnProc(@OnSpawn) = @OnSpawn);
		if not sd^.GetFunction {$ifdef Debug}('ParticleSystem.OnSpawn'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(sys.ref);
			PushFloat(particle.spawnSize);
			PushFloat(particle.endSize);
			PushSint(particle.variation);
			Call(4, 1);
			if GetTableS(-1, 'pos') then
			begin
				particle.spawnPos := ToVec3(-1);
				Pop;
			end;
			if GetTableS(-1, 'vel') then
			begin
				particle.spawnVel := ToVec3(-1);
				Pop;
			end;
			if (particles_ConstraintWithPlane in sys._flags) and GetTableS(-1, 'constraint_plane') then
			begin
				particle.constraint := Plane.Make(ToVec4(-1)).Normalized;
				Pop;
			end;
			Pop;
		end;
	end;

	procedure Script_CreateParticleSystem(var ss: ScriptState);
	var
		flag: ParticleSystemFlag;
		flags: ParticleSystemFlags;
		ps: pParticleSystem;
		mat: pGLMaterial;
		v4: Vec4;
		i: sint;
	begin
		flags := [];
		for flag in ParticleSystemFlag do
			if ss.GetBoolField(1, ParticleSystemFlagIds[flag]) then
				flags += [flag];
		if ss.HasField(1, 'acceleration') then flags += [particles_UseAcceleration];

		if ss.GetTableS(1, 'material') then
		begin
			mat := MakeRef(Script_create_material(ss, -1));
			ss.Pop;
		end else
			mat := nil;

		ps := new(pParticleSystem, Init(flags, mat));
		Release(mat);
		ss.PushObject(ps);
		if not Assigned(ps) then exit;

		if ss.GetTableS(1, 'ref') then
		begin
			ps^.Ref := ss.ToObject(-1, TypeOf(SceneNode));
			ss.Pop;
		end;
		if ss.GetTableS(1, 'gl') then
		begin
			Script_modify_gl(ss, -1, ps^.gl^);
			ss.Pop;
		end;
		if ss.GetTableS(1, 'onSpawn') then
			ss.SetDelegate(ps, @ps^.onSpawn, ParticleSystem.OnSpawnProc(@OnSpawn), '');

		if ss.GetTableS(1, 'lifeDis') then
		begin
			ps^.LifeDis.Done; ps^.LifeDis := Script_distribution(ss, -1);
			ss.Pop;
		end;
		if ss.GetTableS(1, 'sizeDis') then
		begin
			ps^.SizeDis.Done; ps^.SizeDis := Script_distribution(ss, -1);
			ss.Pop;
		end;
		if ss.GetTableS(1, 'endSizeDis') then
		begin
			ps^.EndSizeDis.Done; ps^.EndSizeDis := Script_distribution(ss, -1);
			ss.Pop;
		end;
		ps^.TemporalFrequency := ss.GetFloatField(1, 'temporal_frequency', ps^.TemporalFrequency);
		ps^.SpatialFrequency := ss.GetFloatField(1, 'spatial_frequency', ps^.SpatialFrequency);
		if ss.GetTableS(1, 'acceleration') then
		begin
			ps^.Acceleration := ss.ToVec3(-1);
			ss.Pop;
		end;

		if ss.GetTableS(1, 'variations') then
		begin
			for i := 1 to ss.RawLen(-1) do
			begin
				ss.GetTableI(-1, i);
				v4 := ss.GetVec4Field(-1, 'rect');
				ps^.AddVariation(Vec2.Make(v4.x, v4.y), Vec2.Make(v4.z, v4.w), ss.GetFloatField(-1, 'weight', 1.0));
				ss.Pop;
			end;
			ss.Pop;
		end else
			ps^.AddVariation(Vec2.Zero, Vec2.Ones, 1.0);
		Script_common_create_scene_node(ss, ps);
	end;

	procedure Script_ParticleSystem_Break(var ss: ScriptState);
	begin
		pParticleSystem(ss.ToSelf)^.Break;
	end;

	procedure Script_ParticleSystem_ref(var ss: ScriptState; read: boolean);
	var
		ps: pParticleSystem;
	begin
		ps := ss.ToSelf;
		if read then
			ss.PushObject(ps^.Ref)
		else
			ps^.Ref := ss.ToObject(3, TypeOf(SceneNode));
	end;

	procedure CellFromImage(x, y, i: uint; var cell: MazePattern.StartCell; param: pointer);
	var
		img: pTextureImage absolute param;
		px: pUint8;
	begin
		Assert((@x = @x) and (@y = @y) and (@i = @i));
		px := img^.PixelPtr(i);
		if px[0] >= 250 then cell += [CellIsProhibited];
		if px[1] >= 128 then cell += [CellIsGrowSource];
		if px[2] >= 128 then cell += [CellIsForced];
	end;

	procedure Script_CreateMazePattern(var ss: ScriptState);
	var
		img: TextureImage;
		mp: pGLMazePattern;
		ctl: pControl;
	begin
		img.Init(GetStream(ss.GetStreamField(1, 'src')));
		Assert(img.format in [GLformat_RGB, GLformat_RGBA], GLImageFormatIds[img.format]);
		mp := new(pGLMazePattern, Init(img.Size.X, img.Size.Y, @CellFromImage, @img));
		img.Done;

		if ss.GetTableS(1, 'bind') then
		begin
			ctl := ss.GetObjectField(-1, 'gui', TypeOf(Control));
			if Assigned(ctl) then mp^.Bind(@ctl^.gl, ss.GetStringField(-1, 'id')) else ss.Throw('не задан биндинг лабиринто-узора');
			ss.Pop;
		end;

		ss.PushObject(mp);
	end;

	procedure Script_MazePattern_Step(var ss: ScriptState);
	var
		mp: pGLMazePattern;
	begin
		mp := ss.ToSelf;
		if FrozenMaze in mp^.Step(1) then ss.PushString('frozen') else ss.PushNil;
	end;

	procedure OpenScript(var script: ScriptState);
	const
		Stuff: array[0 .. 12] of ScriptStuffDesc =
		(
			(s: TypeDesc; p: TypeOf(Trail)),
			(s: 'ref' + Writeable; p: @Script_Trail_ref),
			(s: 'Break:0'; p: @Script_Trail_Break),
			(s: 'GL'; p: @Script_Trail_GL),

			(s: TypeDesc; p: TypeOf(ParticleSystem)),
			(s: 'ref' + Writeable; p: @Script_ParticleSystem_ref),
			(s: 'Break:0'; p: @Script_ParticleSystem_Break),

			(s: TypeDesc; p: TypeOf(GLMazePattern)),
			(s: 'Step:1'; p: @Script_MazePattern_Step),

			(s: FunctionsDesc + 'CreateTrail:1' + RequireEnv; p: @Script_CreateTrail),
			(s: 'GenerateMengerSponge'; p: @Script_GenerateMengerSponge),
			(s: 'CreateParticleSystem:1' + RequireEnv; p: @Script_CreateParticleSystem),
			(s: 'CreateMazePattern:1' + RequireEnv; p: @Script_CreateMazePattern)
		);
	begin
		script.AddStuff(Stuff);
	end;

{$ifdef use_serialization}
const
	TRAIL_HAS_REF_TRANSFORM_BIT = 1 shl 0;
	TRAIL_TEXTURED_BIT          = 1 shl 1;
	TRAIL_ACTIVE_BIT            = 1 shl 2;

	procedure SerializeTrail(se: pSerializer; obj: pointer);
	var
		trail: pTrail absolute obj;
		flags: uint;
		i: sint;
	begin
		with se^ do
		begin
			flags := 0;
			if trail^._refTransform <> Transform.Identity then flags := flags or TRAIL_HAS_REF_TRANSFORM_BIT;
			if trail_Textured in trail^.flags then flags := flags or TRAIL_TEXTURED_BIT;
			if trail^._active then flags := flags or TRAIL_ACTIVE_BIT;

			Serialize_ui8(stream, flags);
			SeObject(trail^._ref);
			if trail^._refTransform <> Transform.Identity then Serialize_tf32r8(stream, trail^._refTransform);
			SeObject(trail^._mat);
			SeObject(trail^.gl, TypeOf(GLEntityParams));
			Serialize_ui8(stream, ord(trail^._topology));
			Serialize_ui8(stream, trail^._nSegs);

			Serialize_f16(stream, trail^.persistence);
			Serialize_f16(stream, trail^.moveTolerance);
			Serialize_f16(stream, trail^.angleTolerance);
			if trail_Textured in trail^.flags then Serialize_f16(stream, trail^.texYFactor);

			Serialize_ui16(stream, trail^._nRings);
			if trail^._nRings > 0 then
			begin
				Serialize_vec3f32(stream, trail^._bnd.aabb.A);
				Serialize_vec3f32(stream, trail^._bnd.aabb.B);
				for i := 0 to trail^._nRings - 1 do
				begin
					Serialize_f16(stream, mm.SceneTimeSince(trail^._rings[i].spawnTime));
					Serialize_vec3N8(stream, trail^._rings[i].pos, trail^._bnd.aabb.A, trail^._bnd.aabb.B);
					Serialize_IQuat8(stream, trail^._rings[i].rot);
				end;
			end;
		end;
	end;

	procedure DeserializeTrail(de: pDeserializer; obj: pointer);
	var
		t: pTrail absolute obj;
		flags: uint;
		i: sint;
		aabb: UMath.AABB;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			t^._flags := [];
			if (flags and TRAIL_TEXTURED_BIT) <> 0 then t^._flags += [trail_Textured];
			t^._active := (flags and TRAIL_ACTIVE_BIT) <> 0;

			DeObjectR(t^._ref);
			if (flags and TRAIL_HAS_REF_TRANSFORM_BIT) <> 0 then t^._refTransform := Deserialize_tf32r8(stream) else t^._refTransform := Transform.Identity;
			DeObjectA(t^._mat);
			pGLEntityParams(t^._ro)^.Done;
			DeWeakAtA(pGLEntityParams(t^._ro)^);
			t^._topology := Trail.Topology(Deserialize_ui8(stream));
			t^._nSegs := Deserialize_ui8(stream);

			t^._persistence := Deserialize_f16(stream);
			t^._moveTolerance := Deserialize_f16(stream);
			t^._angleTolerance := Deserialize_f16(stream);
			if (flags and TRAIL_TEXTURED_BIT) <> 0 then t^._texYFactor := Deserialize_f16(stream);

			t^._nRings := Deserialize_ui16(stream);
			if t^._nRings > 0 then
			begin
				aabb.A := Deserialize_vec3f32(stream);
				aabb.B := Deserialize_vec3f32(stream);
				SetLength(t^._rings, t^._nRings);
				for i := 0 to t^._nRings - 1 do
				begin
					t^._rings[i].spawnTime := Deserialize_f16(stream);
					t^._rings[i].pos := Deserialize_vec3N8(stream, aabb.A, aabb.B);
					t^._rings[i].rot := Deserialize_IQuat8(stream);
				end;
			end;
		end;
	end;

	procedure TrailSeSpecial(se: pSerializer; what: SeSpecial; obj: pointer);
	var
		trail: pTrail absolute obj;
	begin
		Assert(@se = @se);
		case what of
			se_Before: trail^._RecalculateBoundingAndSweep;
		end;
	end;

	procedure TrailDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		trail: pTrail absolute obj;
		gl: pGLEntityParams;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize:
				begin
					trail^.DeseInit;
					gl := new(pGLEntityParams, Init);
					pGLEntityParams(trail^._ro) := gl;
				end;
			de_After2: // After занят материалом и GLEntityParams.
				begin
					gl := pGLEntityParams(trail^._ro);
					trail^._Initialize(yes);
					trail^.gl^.Merge(gl^);
					dispose(gl, Done);
				end;
		end;
	end;

const
	PARTICLES_LOCAL_BIT            = 1 shl 0;
	PARTICLES_PLANE_CONSTRAINT_BIT = 1 shl 1;
	PARTICLES_ACCELERATION_BIT     = 1 shl 2;
	PARTICLES_ACTIVE_BIT           = 1 shl 3;

	procedure SerializeParticleSystem(se: pSerializer; obj: pointer);
	var
		ps: pParticleSystem absolute obj;
		spawnPosAABB, spawnVelAABB: AABB;
		flags: uint;
		i: uint;
	begin
		with se^ do
		begin
			flags := 0;
			if particles_Local               in ps^.flags then flags := flags or PARTICLES_LOCAL_BIT;
			if particles_ConstraintWithPlane in ps^.flags then flags := flags or PARTICLES_PLANE_CONSTRAINT_BIT;
			if particles_UseAcceleration     in ps^.flags then flags := flags or PARTICLES_ACCELERATION_BIT;
			if ps^._active then flags := flags or PARTICLES_ACTIVE_BIT;

			Serialize_ui8(stream, flags);
			SeObject(ps^._ref);
			SeObject(ps^._mat);
			SeObject(ps^.gl, TypeOf(GLEntityParams));
			SeObject(@ps^._lifeDis, Distribution.TypeOf);
			SeObject(@ps^._sizeDis, Distribution.TypeOf);
			SeObject(@ps^._endSizeDis, Distribution.TypeOf);
			Serialize_f16(stream, ps^._temporalFrequency);
			Serialize_f16(stream, ps^._spatialFrequency);
			if particles_UseAcceleration in ps^._flags then Serialize_vec3f16(stream, ps^._acceleration);
			SeObject(@ps^.onSpawn, ObjType_MultiDelegate);

			Serialize_ui8(stream, length(ps^._variations));
			for i := 0 to High(ps^._variations) do
			begin
				Serialize_vec2N16(stream, ps^._variations[i].texA, Vec2.Zero, Vec2.Ones);
				Serialize_vec2N16(stream, ps^._variations[i].texB, Vec2.Zero, Vec2.Ones);
				Serialize_f16(stream, ps^._variationWeights[i]);
			end;

			if not (particles_Local in ps^.flags) then
			begin
				Serialize_ui32(stream, ps^.particles.n);
				if ps^.particles.n > 0 then
				begin
					spawnPosAABB := AABB.Make(ps^.particles.items[0].spawnPos);
					spawnVelAABB := AABB.Make(ps^.particles.items[0].spawnVel);
					i := 0;
					while i < ps^.particles.n do
					begin
						spawnPosAABB.Enlarge(ps^.particles.items[i].spawnPos);
						spawnVelAABB.Enlarge(ps^.particles.items[i].spawnVel);
						inc(i);
					end;
					Serialize_vec3f32(stream, spawnPosAABB.A); Serialize_vec3f32(stream, spawnPosAABB.B);
					Serialize_vec3f16(stream, spawnVelAABB.A); Serialize_vec3f16(stream, spawnVelAABB.B);
				end;
				i := 0;
				while i < ps^.particles.n do
				begin
					// Seed, Variation, SpawnSize, EndSize, Life — генерируются по новой.
					Serialize_vec3N8(stream, ps^.particles.items[i].spawnPos, spawnPosAABB.A, spawnPosAABB.B);
					Serialize_vec3N8(stream, ps^.particles.items[i].spawnVel, spawnVelAABB.A, spawnVelAABB.B);
					if not (particles_Local in ps^.flags) then Serialize_f16(stream, mm.SceneTimeSince(ps^.particles.items[i].spawnTime));
					if particles_ConstraintWithPlane in ps^.flags then Serialize_planeN8d32(stream, ps^.particles.items[i].constraint);
					inc(i);
				end;
			end;
		end;
	end;

	procedure DeserializeParticleSystem(de: pDeserializer; obj: pointer);
	var
		ps: pParticleSystem absolute obj;
		spawnPosAABB, spawnVelAABB: AABB;
		flags: uint;
		i, n: sint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			ps^._flags := [];
			if (flags and PARTICLES_LOCAL_BIT) <> 0 then            ps^._flags += [particles_Local];
			if (flags and PARTICLES_PLANE_CONSTRAINT_BIT) <> 0 then ps^._flags += [particles_ConstraintWithPlane];
			if (flags and PARTICLES_ACCELERATION_BIT) <> 0 then     ps^._flags += [particles_UseAcceleration];
			ps^._active := (flags and PARTICLES_ACTIVE_BIT) <> 0;

			DeObjectR(ps^._ref);
			DeObjectR(ps^._mat);
			pGLEntityParams(ps^._ro)^.Done;
			DeWeakAtA(pGLEntityParams(ps^._ro)^);
			DeWeakAtR(ps^._lifeDis);
			DeWeakAtR(ps^._sizeDis);
			DeWeakAtR(ps^._endSizeDis);
			ps^._temporalFrequency := Deserialize_f16(stream);
			ps^._spatialFrequency := Deserialize_f16(stream);
			if particles_UseAcceleration in ps^._flags then ps^._acceleration := Deserialize_vec3f16(stream);
			DeWeakAtR(ps^.onSpawn);
			ps^.onSpawn.Init;

			SetLength(ps^._variations, Deserialize_ui8(stream));
			SetLength(ps^._variationWeights, length(ps^._variations));
			for i := 0 to High(ps^._variations) do
			begin
				ps^._variations[i].texA := Deserialize_vec2N16(stream, Vec2.Zero, Vec2.Ones);
				ps^._variations[i].texB := Deserialize_vec2N16(stream, Vec2.Zero, Vec2.Ones);
				ps^._variationWeights[i] := Deserialize_f16(stream);
			end;

			ps^.particles.Init;
			if (flags and PARTICLES_LOCAL_BIT) <> 0 then
			begin
			end else
			begin
				n := Deserialize_ui32(stream);
				ps^.particles.Grow(n);
				if n > 0 then
				begin
					spawnPosAABB.A := Deserialize_vec3f32(stream); spawnPosAABB.B := Deserialize_vec3f32(stream);
					spawnVelAABB.A := Deserialize_vec3f16(stream); spawnVelAABB.B := Deserialize_vec3f16(stream);
				end;
				for i := 0 to n - 1 do
				begin
					ps^.particles.items[i].spawnPos := Deserialize_vec3N8(stream, spawnPosAABB.A, spawnPosAABB.B);
					ps^.particles.items[i].spawnVel := Deserialize_vec3N8(stream, spawnVelAABB.A, spawnVelAABB.B);
					if not (particles_Local in ps^.flags) then ps^.particles.items[i].spawnTime := Deserialize_f16(stream);
					if particles_ConstraintWithPlane in ps^.flags then ps^.particles.items[i].constraint := Deserialize_planeN8d32(stream) else ps^.particles.items[i].constraint := Plane.None;
				end;
			end;
		end;
	end;

	procedure ParticleSystemSeSpecial(se: pSerializer; what: SeSpecial; obj: pointer);
	var
		ps: pParticleSystem absolute obj;
	begin
		Assert(@se = @se);
		case what of
			se_Before: if not (particles_Local in ps^._flags) then ps^._RecalculateBoundingAndSweep;
		end;
	end;

	procedure ParticleSystemDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		ps: pParticleSystem absolute obj;
		gl: pGLEntityParams;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize:
				begin
					ps^.DeseInit;
					gl := new(pGLEntityParams, Init);
					pGLEntityParams(ps^._ro) := gl;
				end;
			de_After2: // After занят материалом и GLEntityParams
				begin
					gl := pGLEntityParams(ps^._ro);
					ps^._Initialize(yes);
					ps^.gl^.Merge(gl^);
					dispose(gl, Done);
				end;
		end;
	end;
{$endif}

	procedure Init;
	begin
		with ShaderDefines^ do
		begin
			AddFlag('TRAIL_STAR',     Trail.FLAG_STAR);
			AddFlag('TRAIL_TUBE',     Trail.FLAG_TUBE);
			AddFlag('TRAIL_TEXTURED', Trail.FLAG_TEXTURED);

			AddFlag('PARTICLES_ACCELERATION',     ParticleSystem.FLAG_ACCELERATION);
			AddFlag('PARTICLES_PLANE_CONSTRAINT', ParticleSystem.FLAG_PLANE_CONSTRAINT);
			AddFlag('PARTICLES_LOCAL',            ParticleSystem.FLAG_LOCAL);
		end;
		ParticleSystem._vtsPerParticle := 0;

	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Trail', TypeOf(Trail), TypeOf(SceneNode), sizeof(Trail), yes,
		               @SerializeTrail, @DeserializeTrail, @TrailSeSpecial, @TrailDeSpecial)
		^.RegisterType('Particle system', TypeOf(ParticleSystem), TypeOf(SceneNode), sizeof(ParticleSystem), yes,
		              @SerializeParticleSystem, @DeserializeParticleSystem, @ParticleSystemSeSpecial, @ParticleSystemDeSpecial)
		^.RegisterFunc(@OnSpawn);
	{$endif}
	end;

initialization
	&Unit('ProceduralGraphics').Initialize(@Init);
end.
