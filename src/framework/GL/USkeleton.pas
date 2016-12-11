unit USkeleton;

{$include opts.inc}

interface

uses
	USystem, Errors, UMath, UClasses, Streams, SceneGraph
{$ifdef Debug}, Utils, ULog, Human {$endif}
{$ifdef use_serialization}, Serialization {$endif};

const
	BaseBoneVector: Vec3 = (data: (1.0, 0.0, 0.0));

type
	pBone = ^tBone;
	tBone = object
	public
		name: PoolString;
		offset: Vec3;
		rotation: Quaternion;
		invBind: Transform;
		attachedBones: array of pBone;
		parent: pBone;
		constructor Init(const newName: PoolString);
		destructor Done;
		procedure AttachBone(newBone: pBone);
		procedure BuildInverseBind;
	end;

	tBoneAnimationFlag = (boneanim_Looped, boneanim_HasRot, boneanim_HasOfs);
	tBoneAnimationFlags = set of tBoneAnimationFlag;

	pBoneAnimation = ^tBoneAnimation;
	tBoneAnimation = object
	private
		_flags: tBoneAnimationFlags;
		_nKeys: sint;
		procedure _SetNKeyframes(n: sint);
		function _GetFlag(f: tBoneAnimationFlag): boolean;
		procedure _SetFlag(f: tBoneAnimationFlag; value: boolean);
		procedure _SetFlags(newFlags: tBoneAnimationFlags);
	public
		bone: pBone;
		weight: float;
		maxPhase: float;
		kfPhase: pFloat;
		kfRot: pQuaternion;
		kfOfs: pVec3;
		constructor Init(newBone: pBone);
		destructor Done;
		procedure AddKeyframe(const phase: float; const boneRot: Quaternion; const boneOfs: Vec3);
		function RemoveKeyframe(id: sint): boolean;
		procedure GetBoneState(const phase: float; out boneRot: Quaternion; out boneOfs: Vec3);

		property nKeyframes: sint read _nKeys write _SetNKeyframes;
		property Flags: tBoneAnimationFlags read _flags write _SetFlags;
		property Flag[f: tBoneAnimationFlag]: boolean read _GetFlag write _SetFlag;
		property Looped: boolean index boneanim_Looped read _GetFlag write _SetFlag;
		property HasRot: boolean index boneanim_HasRot read _GetFlag write _SetFlag;
		property HasOfs: boolean index boneanim_HasOfs read _GetFlag write _SetFlag;
	end;

	SkeletonAnimationKind =
	(
		anim_Single,
		anim_Looped
	);

const
	BoneAnimationFlagIds: array[tBoneAnimationFlag] of string = ('loop', 'rot', 'ofs');
	SkeletonAnimationKindIds: array[SkeletonAnimationKind] of string = ('single', 'loop');

type
	pSkeletonSource = ^SkeletonSource;

	pSkeletonAnimation = ^SkeletonAnimation;
	SkeletonAnimation = object
	{$ifdef Debug}
	type
		DeserializationDebug = object
			fixedBones, totalBones: uint;
		end;
	{$endif}
	private
		_length: float;
		_kind: SkeletonAnimationKind;
	public
		skel: pSkeletonSource;
		name: PoolString;
		// той же длины, что и в скелете! Неиспользуемые кости — nil'ы.
		bones: array of pBoneAnimation;
		defaultInVel, defaultOutVel: float;
		links: array of sint;
		transitions: array of record
			a2, trans: sint;
		end;
		constructor Init(const newName: PoolString; newSkeleton: pSkeletonSource);
		destructor Done;
		procedure Deserialize(s: pStream; const offsetsAABB: AABB {$ifdef Debug}; out d: DeserializationDebug {$endif});
		function BoneAnim(id: sint): pBoneAnimation;
		function RemoveBone(id: sint): boolean;

		property Len: float read _length write _length;
		property Kind: SkeletonAnimationKind read _kind write _kind;
	end;

	SkeletonSource = object(&Object)
		bones: array of tBone;
		anims: array of SkeletonAnimation;
		constructor Init;
		destructor Done; virtual;
		function Deserialize(s: pStream): pSkeletonSource; static;
		function CreateBone(const name: PoolString): pBone;
		function FindBone(const name: PoolString): pBone;
		function GetBoneID(const name: PoolString): sint;
		function GetBoneID(var bone: tBone): sint;
		procedure JointBones(const &to, who: PoolString);
		function CreateAnimation(const name: PoolString): pSkeletonAnimation;
		function FindAnimation(const name: PoolString): pSkeletonAnimation;
		procedure RemoveAnimation(id: sint);
		function GetAnimationID(const name: PoolString): sint;

	var
		Loaders: LoaderSuite; static;

	type
		RawSkel = object
		const
			Signature = 'skel';
			BONE_HAS_CHILDS = 1 shl 0;

			{$define max := ord(High(SkeletonAnimationKind))} {$define mask := KIND_MASK} {$include bits_to_store.inc}
			ANIM_HAS_LINKS = 1 shl (bitsizeof(uint8) - 1);
			ANIM_HAS_TRANSITIONS = 1 shl (bitsizeof(uint8) - 2);

			BONEANIM_LOOPED = 1 shl 0;
			BONEANIM_HAS_ROT = 1 shl 1;
			BONEANIM_HAS_OFS = 1 shl 2;
			BONEANIM_USER_WEIGHT = 1 shl 3;
			BONEANIM_HAS_MAX_PHASE = 1 shl 4;
		end;
	end;

	pSkeletonNode = ^SkeletonNode;
	SkeletonNode = object(SceneNode)
	private type
		pAnimation = ^tAnimation;
		tAnimation = object
			weight: float;
			phase, phaseVel: float;
			active: boolean;
			requiredWeight: float;
			inSkel: pSkeletonAnimation;
			procedure Initialize(newInSkel: pSkeletonAnimation);
			procedure Finalize;
		end;

		pBoneInstance = ^tBoneInstance;

		pInverseKinematics = ^tInverseKinematics;

		tBoneInstance = object
			tLocal, tBind: Transform;
			ik: pInverseKinematics;
			dirty: boolean;
			procedure Initialize;
			procedure Finalize;
			function EnsureIK(var skel: SkeletonNode): pInverseKinematics;
			procedure KillIK;
			function ID(var skel: SkeletonNode): sint;
			function GetParent(var skel: SkeletonNode): pBoneInstance;
			procedure SetLocalTransform(const newTf: Transform; var skel: SkeletonNode);
			procedure InvalidateBind(var skel: SkeletonNode);
			function BindTransform(var skel: SkeletonNode): Transform;
			function WorldTransform(var skel: SkeletonNode): Transform;
		end;

		tInverseKinematics = object
		private type
			tMode = (ik_None, ik_ToNode, ik_ToTarget);
		private var
			_velocity, _timeout: float;

			_mode: tMode;
			_node: pSceneNode;
			_nodeTf: Transform;
			_target: Vec3;
			_localConeAxis: Vec3;
			procedure _Reset;
			procedure _NodeLost;
			procedure _Initialize(var skel: SkeletonNode; bone: pBoneInstance; dese: boolean);
		public
			constructor Init(var skel: SkeletonNode; bone: pBoneInstance);
			constructor DeseInit;
			destructor Done;
			procedure SetNode(newNode: pSceneNode; const newNodeTransform: Transform; const newVelocity, newTimeout: float);
			procedure SetTarget(const newTarget: Vec3; const newVelocity, newTimeout: float);
			function Update(var skel: SkeletonNode; var bone: tBoneInstance; const dt: float): boolean;
		end;

		BoneChild = record
			bone: pBoneInstance;
			node: pSceneNode;
		end;

	private var
		_skel: pSkeletonSource;
		anims: array of tAnimation;
		ignoreAccumNextTime: boolean;
	protected
		procedure _OnUpdate(const dt: float); virtual;
		function _SuitsTo(know: SceneKnowledge): boolean; virtual;
		function TransformBaseFor(node: pSceneNode): Transform; virtual;
		procedure _NotifyDetach(node: pSceneNode); virtual;
	public
		bones: array of tBoneInstance;
		boneChilds: array of BoneChild;
		constructor Init(newSkel: pSkeletonSource);
		destructor Done; virtual;
		procedure AttachToBone(id: sint; node: pSceneNode);
		function FindBone(const name: PoolString): pBoneInstance;
		function GetBoneID(const name: PoolString): sint;
		function GetAnimationID(const name: PoolString): sint;
		procedure SetIK(bone: sint; newNode: pSceneNode; const newNodeTransform: Transform; const newVelocity, newTimeout: float);
		procedure SetIK(bone: sint; const newTarget: Vec3; const newVelocity, newTimeout: float);

		function UseAnim(animId: sint; const weight: float; const phaseVelocity: float): boolean;
		function GetAnimPhase(animId: sint): float;
		function AnimCompleted(animId: sint; const tolerance: float): boolean;
		procedure SetAnimPhase(animId: sint; const aPhase: float; alwaysPropagateToLinks: boolean = yes);
		function EstimateBoneLength(id: sint): float;

		property Skeleton: pSkeletonSource read _skel;
	private
		function IndexBoneChild(node: pSceneNode): sint;
	end;

	tSkeletonIdKind = (id_Bone, id_Anim);

	SkeletonNameIdPair = object
		name: PoolString;
		id: sint;
		procedure Initialize(const newName: PoolString);
		procedure Finalize;
		procedure Update(skel: pSkeletonNode; what: tSkeletonIdKind);
		procedure Rename(const newName: PoolString; skel: pSkeletonNode; what: tSkeletonIdKind);
	end;

implementation

{$ifdef Profile}
uses
	Profile;
{$endif}

	constructor tBone.Init(const newName: PoolString);
	begin
		parent := nil;
		attachedBones := nil;
		offset := Vec3.Zero;
		rotation := Quaternion.Identity;
		name := newName;
	end;

	destructor tBone.Done;
	begin
	end;

	procedure tBone.AttachBone(newBone: pBone);
	begin
		SetLength(attachedBones, system.length(attachedBones) + 1);
		attachedBones[High(attachedBones)] := newBone;
		newBone^.parent := @self;
	end;

	procedure tBone.BuildInverseBind;
	var
		t: Transform;
		i: sint;
	begin
		t := RotateTranslate(rotation.Inversed, -offset);
		if Assigned(parent) then
			invBind := t * parent^.invBind
		else
			invBind := t;
		for i := 0 to High(attachedBones) do
			attachedBones[i]^.BuildInverseBind;
	end;

	procedure tBoneAnimation._SetNKeyframes(n: sint);
	begin
		if n <> _nKeys then
		begin
			_nKeys := n;
			ReallocMem(kfPhase, nKeyframes * sizeof(kfPhase[0]));
			if boneanim_HasRot in _flags then ReallocMem(kfRot, _nKeys * sizeof(kfRot[0]));
			if boneanim_HasOfs in _flags then ReallocMem(kfOfs, _nKeys * sizeof(kfOfs[0]));
		end;
	end;

	function tBoneAnimation._GetFlag(f: tBoneAnimationFlag): boolean;
	begin
		result := f in _flags;
	end;

	procedure tBoneAnimation._SetFlag(f: tBoneAnimationFlag; value: boolean);
	var
		i: sint;
	begin
		if value = (f in _flags) then exit;
		if value then
			Include(_flags, f)
		else
			Exclude(_flags, f);
		case f of
			boneanim_HasRot:
				if value then
				begin
					kfRot := GetMem(nKeyframes * sizeof(kfRot[0]));
					for i := 0 to nKeyframes - 1 do
						kfRot[i] := bone^.rotation;
				end else
					FreeMem(kfRot);
			boneanim_HasOfs:
				if value then
				begin
					kfOfs := GetMem(nKeyframes * sizeof(kfOfs[0]));
					for i := 0 to nKeyframes - 1 do
						kfOfs[i] := bone^.offset;
				end else
					FreeMem(kfOfs);
		end;
	end;

	procedure tBoneAnimation._SetFlags(newFlags: tBoneAnimationFlags);
	var
		f: tBoneAnimationFlag;
	begin
		for f in tBoneAnimationFlags do
			Flag[f] := f in newFlags;
	end;

	constructor tBoneAnimation.Init(newBone: pBone);
	begin
		maxPhase := 0.0;
		weight := 1.0;
		_nKeys := 0;
		kfPhase := nil;
		kfRot := nil;
		kfOfs := nil;
		bone := newBone;
		_flags := [];
	end;

	destructor tBoneAnimation.Done;
	begin
		FreeMem(kfOfs);
		FreeMem(kfRot);
		FreeMem(kfPhase);
	end;

	procedure tBoneAnimation.AddKeyframe(const phase: float; const boneRot: Quaternion; const boneOfs: Vec3);
	begin
		if boneRot <> bone^.rotation then HasRot := yes;
		if boneOfs <> bone^.offset then HasOfs := yes;

		nKeyframes := nKeyframes + 1;
		kfPhase[_nKeys - 1] := phase;
		if boneanim_HasRot in _flags then kfRot[_nKeys - 1] := boneRot;
		if boneanim_HasOfs in _flags then kfOfs[_nKeys - 1] := boneOfs;
	end;

	function tBoneAnimation.RemoveKeyframe(id: sint): boolean;
	var
		i: sint;
	begin
		result := (id >= 0) and (id < nKeyframes);
		if not result then exit;

		for i := id to nKeyframes - 2 do
		begin
			kfPhase[i] := kfPhase[i + 1];
			if boneanim_HasRot in _flags then kfRot[i] := kfRot[i + 1];
			if boneanim_HasOfs in _flags then kfOfs[i] := kfOfs[i + 1];
		end;
		nKeyframes := nKeyframes - 1;
	end;

	procedure tBoneAnimation.GetBoneState(const phase: float; out boneRot: Quaternion; out boneOfs: Vec3);
	var
		i0, i1, i2, i3: sint;
		i, b: sint;
		w, phaseDiff: float;
	begin
	trace_call('tBoneAnimation.GetBoneState');
		if nKeyframes = 0 then
		begin
			boneRot := bone^.rotation;
			boneOfs := bone^.offset;
			exit;
		end;
		if not (boneanim_HasRot in _flags) then boneRot := bone^.rotation;
		if not (boneanim_HasOfs in _flags) then boneOfs := bone^.offset;

		if not (boneanim_Looped in _flags) then
		begin
			if phase <= kfPhase[0] then
			begin
				if boneanim_HasRot in _flags then boneRot := kfRot[0];
				if boneanim_HasOfs in _flags then boneOfs := kfOfs[0];
				exit;
			end;
			if phase >= kfPhase[nKeyframes - 1] then
			begin
				i := nKeyframes - 1;
				if boneanim_HasRot in _flags then boneRot := kfRot[i];
				if boneanim_HasOfs in _flags then boneOfs := kfOfs[i];
				exit;
			end;
		end;

		b := nKeyframes - 1;
		for i := 0 to nKeyframes - 2 do
			if (phase >= kfPhase[i]) and (phase <= kfPhase[i + 1]) then
			begin
				b := i;
				break;
			end;

		if boneanim_Looped in _flags then
		begin
			i0 := (b - 1 + nKeyframes) mod nKeyframes;
			i1 := b;
			i2 := (b + 1) mod nKeyframes;
			i3 := (b + 2) mod nKeyframes;
			if i1 + 1 = i2 then phaseDiff := kfPhase[i2] - kfPhase[i1] else phaseDiff := maxPhase - kfPhase[nKeyframes - 1] + kfPhase[0];
		end else
		begin
			if b > 1 then i0 := b - 1 else i0 := 0;
			i1 := b;
			if b + 1 < nKeyframes then i2 := b + 1 else i2 := nKeyframes - 1;
			if b + 2 < nKeyframes then i3 := b + 2 else i3 := nKeyframes - 1;
			if i1 < i2 then phaseDiff := kfPhase[i2] - kfPhase[i1] else phaseDiff := maxPhase - kfPhase[i1];
		end;
		if phase > kfPhase[i1] then
			w := (phase - kfPhase[i1]) / phaseDiff
		else
			w := (maxPhase + phase - kfPhase[i1]) / phaseDiff;

		if boneanim_HasRot in _flags then
			boneRot := CatmullRomSpline(kfRot[i0], kfRot[i1], kfRot[i2], kfRot[i3], w);
		if boneanim_HasOfs in _flags then
			boneOfs := CatmullRomSpline(kfOfs[i0], kfOfs[i1], kfOfs[i2], kfOfs[i3], w);

	leave_call
	end;

	procedure SkeletonAnimation.Deserialize(s: pStream; const offsetsAABB: AABB {$ifdef Debug}; out d: DeserializationDebug {$endif});
	type
		RawSkel = SkeletonSource.RawSkel;
	var
		usedBones: sint;
		cb, ckf, i: sint;
		curBone: pBoneAnimation;
		kindx, animFlags: uint;
	begin
	{$ifdef Debug} d.fixedBones := 0; d.totalBones := 0; {$endif}
		name := Deserialize_string(s);
		kindx := Deserialize_ui8(s);
		_kind := SkeletonAnimationKind(RangeCheck(kindx and RawSkel.KIND_MASK, ord(High(_kind)), 'SkeletonAnimation.kind'));
		_length := Deserialize_f16(s);
		defaultInVel := Deserialize_f16(s);
		defaultOutVel := Deserialize_f16(s);

		usedBones := RangeCheck(VarInt.Read(s), 1, length(skel^.bones), 'SkeletonAnimation.usedBones');
	{$ifdef Debug} d.totalBones := usedBones; {$endif}
		for cb := 0 to usedBones - 1 do
		begin
			curBone := BoneAnim(RangeCheck(VarInt.Read(s), High(skel^.bones), 'SkeletonAnimation.boneID'));
			animFlags := Deserialize_ui8(s);
			curBone^.Looped := (animFlags and RawSkel.BONEANIM_LOOPED) <> 0;
			curBone^.HasRot := (animFlags and RawSkel.BONEANIM_HAS_ROT) <> 0;
			curBone^.HasOfs := (animFlags and RawSkel.BONEANIM_HAS_OFS) <> 0;
			if (animFlags and RawSkel.BONEANIM_USER_WEIGHT) <> 0 then curBone^.weight := Deserialize_f16(s);
			if (animFlags and RawSkel.BONEANIM_HAS_MAX_PHASE) <> 0 then curBone^.maxPhase := Deserialize_f16(s);
			ckf := VarInt.Read(s);
			curBone^._SetNKeyframes(ckf);
			for i := 0 to ckf-1 do
				curBone^.kfPhase[i] := Deserialize_f16(s);
			if curBone^.HasRot then
				for i := 0 to ckf-1 do
				begin
					curBone^.kfRot[i] := Deserialize_IQuat16(s);
					if (i = 0) and (curBone^.kfRot[i] ** curBone^.bone^.rotation < 0) or
						(i > 0) and (curBone^.kfRot[i] ** curBone^.kfRot[i - 1] < 0) then
					begin
						curBone^.kfRot[i] := -curBone^.kfRot[i];
					{$ifdef Debug} d.fixedBones += 1; {$endif}
					end;
				end;
			if curBone^.HasOfs then
				for i := 0 to ckf-1 do
					curBone^.kfOfs[i] := Deserialize_vec3N16(s, offsetsAABB.A, offsetsAABB.B);
		end;

		if kindx and RawSkel.ANIM_HAS_LINKS <> 0 then
		begin
			SetLength(links, VarInt.Read(s));
			for i := 0 to High(links) do
				links[i] := VarInt.Read(s);
		end;

		if kindx and RawSkel.ANIM_HAS_TRANSITIONS <> 0 then
		begin
			SetLength(transitions, VarInt.Read(s));
			for i := 0 to High(transitions) do
			begin
				transitions[i].a2 := VarInt.Read(s);
				transitions[i].trans := VarInt.Read(s);
			end;
		end;
	end;

	constructor SkeletonAnimation.Init(const newName: PoolString; newSkeleton: pSkeletonSource);
	var
		i: sint;
	begin
		skel := newSkeleton;
		name := newName;
		SetLength(bones, length(skel^.bones));
		for i := 0 to High(bones) do bones[i] := nil;
		_kind := anim_Single;
		_length := 0.0;
		defaultInVel := 1.0;
		defaultOutVel := 1.0;
		links := nil;
		transitions := nil;
	end;

	destructor SkeletonAnimation.Done;
	var
		i: sint;
	begin
		for i := 0 to High(bones) do
			if Assigned(bones[i]) then
				dispose(bones[i], Done);
	end;

	function SkeletonAnimation.BoneAnim(id: sint): pBoneAnimation;
	begin
		if id <> -1 then
		begin
			if not Assigned(bones[id]) then bones[id] := new(pBoneAnimation, Init(@skel^.bones[id]));
			result := bones[id];
			result^.maxPhase := Len;
		end else
			result := nil;
	end;

	function SkeletonAnimation.RemoveBone(id: sint): boolean;
	begin
		result := (id >= 0) and (id < length(bones)) and Assigned(bones[id]);
		if result then
		begin
			dispose(bones[id], DOnE);
			bones[id] := nil;
		end;
	end;

	constructor SkeletonSource.Init;
	begin
		inherited Init;
		bones := nil;
		anims := nil;
	end;

	destructor SkeletonSource.Done;
	var
		i: sint;
	begin
		for i := 0 to High(bones) do bones[i].Done;
		for i := 0 to High(anims) do anims[i].Done;
		inherited Done;
	end;

	function SkeletonSource.CreateBone(const name: PoolString): pBone;
	begin
		result := FindBone(name);
		if Assigned(result) then
		begin
		{$ifdef Debug} Log('Кость "' + name + '" уже существует в скелете', logError); {$endif}
			exit(result);
		end;
		SetLength(bones, length(bones) + 1);
		bones[High(bones)].Init(name);
		result := @bones[High(bones)];
	end;

	function SkeletonSource.FindBone(const name: PoolString): pBone;
	var
		i: sint;
	begin
		i := GetBoneID(name);
		if i >= 0 then result := @bones[i] else result := nil;
	end;

	function SkeletonSource.GetBoneID(const name: PoolString): sint;
	begin
		result := Index(name.ToIndex, pointer(bones) + fieldoffset tBone _ name _, length(bones), sizeof(tBone));
	end;

	function SkeletonSource.GetBoneID(var bone: tBone): sint;
	begin
		result := @bone - @bones[0];
		Assert((result >= 0) and (result < length(bones)));
	end;

	procedure SkeletonSource.JointBones(const &to, who: PoolString);
	var
		t, w: pBone;
	begin
		t := FindBone(&to);
		w := FindBone(who);
		if (Assigned(t)) and (Assigned(w)) then t^.AttachBone(w);
	end;

	function SkeletonSource.Deserialize(s: pStream): pSkeletonSource;
	var
		i: sint;
	begin
		MakeRef(s);
		try
		{$ifdef Debug} LogR('Загрузка скелета из ' + StreamPath.Log(s^.path) + '... '); {$endif}
			result := new(pSkeletonSource, Init);
			try
				Loaders.Load(result, s);
			except
				Free(result);
				raise;
			end;
		{$ifdef Debug} Log('Скелет {0} загружен, {1}, {2}.', StreamPath.Log(s^.path), lang_amount(length(result^.bones), '{N} кост{ь/и/ей}'), lang_amount(length(result^.anims), '{N} анимац{ия/ии/ий}'), logOK); {$endif}
		finally
			Release(s);
		end;

		for i := 0 to High(result^.bones) do
			if not Assigned(result^.bones[i].parent) then
				result^.bones[i].BuildInverseBind;
	end;

	procedure LoadRawskel(var skel: SkeletonSource; s: pStream);
	type
		RawSkel = SkeletonSource.RawSkel;
	var
		i, j, nBones, nAnims: sint;
		curBone: pBone;
		boneFlags: uint8;
		offsetsAABB: AABB;
	{$ifdef Debug} fixedList: string; ad: SkeletonAnimation.DeserializationDebug; {$endif}
	begin
		Deserialize_signature(s, RawSkel.Signature, no);
		offsetsAABB.A := Deserialize_vec3f32(s);
		offsetsAABB.B := Deserialize_vec3f32(s);

		nBones := VarInt.Read(s);
		SetLength(skel.bones, nBones);
		for i := 0 to nBones - 1 do
			skel.bones[i].Init('');
		for i := 0 to nBones - 1 do
		begin
			curBone := @skel.bones[i];
			curBone^.name := Deserialize_string(s);
			boneFlags := Deserialize_ui8(s);
			curBone^.offset := Deserialize_vec3N16(s, offsetsAABB.A, offsetsAABB.B);
			curBone^.rotation := Deserialize_IQuat16(s);
			if RawSkel.BONE_HAS_CHILDS and boneFlags <> 0 then
				for j := 1 to RangeCheck(VarInt.Read(s), uint(nBones - 1), 'bone.nChilds') do
					curBone^.AttachBone(@skel.bones[RangeCheck(VarInt.Read(s), uint(nBones - 1), 'bone.child')]);
		end;

		nAnims := VarInt.Read(s);
	{$ifdef Debug} fixedList := ''; {$endif}
		for i := 1 to nAnims do
		begin
			skel.CreateAnimation('')^.Deserialize(s, offsetsAABB {$ifdef Debug}, ad {$endif});
		{$ifdef Debug}
			if ad.fixedBones > 0 then
			begin
				fixedList += IfThen(fixedList <> '', ', ', '') +
				             skel.anims[High(skel.anims)].name + ' (' + ToString(ad.fixedBones) + '/' + ToString(ad.totalBones) + ')';
			end;
		{$endif}
		end;
	{$ifdef Debug} if fixedList <> '' then Log('Исправленные кости: ' + fixedList, logDebug); {$endif}
	end;

	function SkeletonSource.CreateAnimation(const name: PoolString): pSkeletonAnimation;
	begin
		result := FindAnimation(name);
		if Assigned(result) then
		begin
		{$ifdef Debug} Log('Анимация "' + name + '" уже существует в скелете', logWarning); {$endif}
			result^.Done;
			result^.Init(name, @self);
			exit(result);
		end;
		SetLength(anims, length(anims) + 1);
		anims[High(anims)].Init(name, @self);
		result := @anims[High(anims)];
	end;

	function SkeletonSource.FindAnimation(const name: PoolString): pSkeletonAnimation;
	var
		i: sint;
	begin
		i := GetAnimationID(name);
		if i >= 0 then result := @anims[i] else result := nil;
	end;

	procedure SkeletonSource.RemoveAnimation(id: sint);
		function fix(var aid: sint): boolean;
		begin
			result := aid <> id;
			if aid > id then dec(aid);
		end;
	var
		i, j: sint;
		anim: pSkeletonAnimation;
	begin
		if id < 0 then exit;
		anims[id].Done;
		for i := id to High(anims) - 1 do
			anims[i] := anims[i + 1];
		SetLength(anims, length(anims) - 1);

		for i := 0 to High(anims) do
		begin
			anim := @anims[i];
			for j := High(anim^.links) downto 0 do
				if not fix(anim^.links[j]) then
				begin
					anim^.links[j] := anim^.links[High(anim^.links)];
					SetLength(anim^.links, length(anim^.links) - 1);
				end;
			for j := High(anim^.transitions) downto 0 do
				if (not fix(anim^.transitions[j].a2)) or
					(not fix(anim^.transitions[j].trans)) then
				begin
					anim^.transitions[j] := anim^.transitions[High(anim^.transitions)];
					SetLength(anim^.transitions, length(anim^.transitions) - 1);
				end;
		end;
	end;

	function SkeletonSource.GetAnimationID(const name: PoolString): sint;
	begin
		result := Index(name.ToIndex, pointer(anims) + fieldoffset SkeletonAnimation _ name _, length(anims), sizeof(SkeletonAnimation));
	end;

	procedure SkeletonNode.tAnimation.Initialize(newInSkel: pSkeletonAnimation);
	begin
		phase := 0.0;
		weight := 0.0;
		inSkel := newInSkel;
	end;

	procedure SkeletonNode.tAnimation.Finalize;
	begin
	end;

	procedure SkeletonNode.tBoneInstance.Initialize;
	begin
		tLocal := Transform.Identity;
		dirty := yes;
		ik := nil;
	end;

	procedure SkeletonNode.tBoneInstance.Finalize;
	begin
		KillIK;
	end;

	function SkeletonNode.tBoneInstance.EnsureIK(var skel: SkeletonNode): pInverseKinematics;
	begin
		result := ik;
		if not Assigned(result) then
		begin
			result := new(pInverseKinematics, Init(skel, @self));
			ik := result;
		end;
	end;

	procedure SkeletonNode.tBoneInstance.KillIK;
	begin
		if Assigned(ik) then dispose(ik, Done);
	end;

	function SkeletonNode.tBoneInstance.ID(var skel: SkeletonNode): sint;
	begin
		result := @self - @skel.bones[0];
		Assert((result >= 0) and (result < length(skel.bones)));
	end;

	function SkeletonNode.tBoneInstance.GetParent(var skel: SkeletonNode): pBoneInstance;
	var
		pb: pBone;
	begin
		pb := skel._skel^.bones[ID(skel)].parent;
		if Assigned(pb) then
			result := @skel.bones[skel._skel^.GetBoneID(pb^)]
		else
			result := nil;
	end;

	procedure SkeletonNode.tBoneInstance.SetLocalTransform(const newTf: Transform; var skel: SkeletonNode);
	begin
		if newTf <> tLocal then
		begin
			tLocal := newTf;
			InvalidateBind(skel);
		end;
	end;

	procedure SkeletonNode.tBoneInstance.InvalidateBind(var skel: SkeletonNode);
	var
		bone: pBone;
		ss: pSkeletonSource;
	begin
		if not dirty then
		begin
			dirty := yes;
			ss := skel._skel;
			for bone in ss^.bones[ID(skel)].attachedBones do
				skel.bones[ss^.GetBoneID(bone^)].InvalidateBind(skel);
		end;
	end;

	function SkeletonNode.tBoneInstance.BindTransform(var skel: SkeletonNode): Transform;
	var
		parent: pBoneInstance;
	begin
		if dirty then
		begin
			parent := GetParent(skel);
			if Assigned(parent) then
				result := parent^.BindTransform(skel) * tLocal
			else
				result := tLocal;
			tBind := result;
			dirty := no;
		end else
			result := tBind;
	end;

	function SkeletonNode.tBoneInstance.WorldTransform(var skel: SkeletonNode): Transform;
	begin
		result := skel.GlobalTransform * BindTransform(skel);
	end;

	constructor SkeletonNode.Init(newSkel: pSkeletonSource);
	var
		i: sint;
	begin
		inherited Init;
		anims := nil;
		bones := nil;
		boneChilds := nil;

		_skel := MakeRef(newSkel);
		if not Assigned(newSkel) then ConstructorFailed;

		SetLength(bones, length(_skel^.bones));
		for i := 0 to High(bones) do
		begin
			bones[i].Initialize;
			bones[i].SetLocalTransform(TranslateRotate(_skel^.bones[i].offset, _skel^.bones[i].rotation), self);
		end;

		SetLength(anims, length(_skel^.anims));
		for i := 0 to High(anims) do
			anims[i].Initialize(@_skel^.anims[i]);
		ignoreAccumNextTime := yes;
	end;

	destructor SkeletonNode.Done;
	var
		i: sint;
	begin
		boneChilds := nil;
		for i := 0 to High(bones) do
			bones[i].Finalize;
		bones := nil;
		for i := 0 to High(anims) do
			anims[i].Finalize;
		anims := nil;
		Release(_skel);
		inherited Done;
	end;

	procedure SkeletonNode.AttachToBone(id: sint; node: pSceneNode);
	begin
		if (id < 0) or (id > High(bones)) then exit;
		SetLength(boneChilds, length(boneChilds) + 1);
		boneChilds[High(boneChilds)].bone := @bones[id];
		boneChilds[High(boneChilds)].node := node;
		Attach(node);
	{$ifdef Debug} Log('К кости "{0}" прицеплен объект', _skel^.bones[id].name, logDebug); {$endif}
	end;

	function SkeletonNode.TransformBaseFor(node: pSceneNode): Transform;
	var
		i: sint;
	begin
		i := IndexBoneChild(node);
		if i >= 0 then
			result := boneChilds[i].bone^.WorldTransform(self)
		else
			result := inherited TransformBaseFor(node);
	end;

	procedure SkeletonNode._NotifyDetach(node: pSceneNode);
	var
		i: sint;
	begin
		i := IndexBoneChild(node);
		if i >= 0 then
		begin
		{$ifdef Debug} Log('От кости "{0}" отцеплен объект', _skel^.bones[boneChilds[i].bone^.ID(self)].name, logDebug); {$endif}
			boneChilds[i] := boneChilds[High(boneChilds)];
			SetLength(boneChilds, length(boneChilds) - 1);
		end;
		inherited _NotifyDetach(node);
	end;

	function SkeletonNode.FindBone(const name: PoolString): pBoneInstance;
	var
		i: sint;
	begin
		i := _skel^.GetBoneID(name);
		if i >= 0 then result := @bones[i] else result := nil;
	end;

	function SkeletonNode.GetBoneID(const name: PoolString): sint;
	begin
		result := _skel^.GetBoneID(name);
	end;

	function SkeletonNode.GetAnimationID(const name: PoolString): sint;
	begin
		result := _skel^.GetAnimationID(name);
	end;

	procedure SkeletonNode.SetIK(bone: sint; newNode: pSceneNode; const newNodeTransform: Transform; const newVelocity, newTimeout: float);
	begin
		if bone < 0 then exit;
		bones[bone].EnsureIK(self)^.SetNode(newNode, newNodeTransform, newVelocity, newTimeout);
	end;

	procedure SkeletonNode.SetIK(bone: sint; const newTarget: Vec3; const newVelocity, newTimeout: float);
	begin
		if bone < 0 then exit;
		bones[bone].EnsureIK(self)^.SetTarget(newTarget, newVelocity, newTimeout);
	end;

	function SkeletonNode.UseAnim(animId: sint; const weight: float; const phaseVelocity: float): boolean;
	var
		i: sint;
	begin
		result := (animId >= 0) and GreaterThan(weight, 0.0);
		if not result then exit;
		if not anims[animId].active then
		begin
			anims[animId].active := yes;
			anims[animId].phaseVel := phaseVelocity;
			for i := 0 to High(anims[animId].inSkel^.links) do
				anims[anims[animId].inSkel^.links[i]].phaseVel := phaseVelocity;
		end;
		anims[animId].requiredWeight := weight;
	end;

	function SkeletonNode.GetAnimPhase(animId: sint): float;
	begin
		if animId < 0 then exit(0.0);
		result := anims[animId].phase;
	end;

	function SkeletonNode.AnimCompleted(animId: sint; const tolerance: float): boolean;
	begin
		result := (animId < 0) or (_skel^.anims[animId].Kind = anim_Single) and GreaterThanEqual(anims[animId].phase + tolerance, _skel^.anims[animId].Len);
	end;

	procedure SkeletonNode.SetAnimPhase(animId: sint; const aPhase: float; alwaysPropagateToLinks: boolean = yes);
	var
		i: sint;
	begin
		if animId < 0 then exit;
		with anims[animId] do
		begin
			case inSkel^.Kind of
				anim_Single: anims[animId].phase := clamp(aPhase, 0.0, anims[animId].inSkel^.Len);
				anim_Looped: anims[animId].phase := modf(aPhase, anims[animId].inSkel^.Len);
			end;
			for i := 0 to High(inSkel^.links) do
				if alwaysPropagateToLinks or not anims[inSkel^.links[i]].active then
					anims[inSkel^.links[i]].phase := phase;
		end;
	end;

	function SkeletonNode.EstimateBoneLength(id: sint): float;
	var
		b: pBone;
		i, n: sint;
	begin
		b := @_skel^.bones[id];
		n := 0;
		result := 0.0;
		for i := 0 to High(b^.attachedBones) do
			if bones[_skel^.GetBoneID(b^.attachedBones[i]^)].tLocal.tr.x > 0.0 then
			begin
				result := result + bones[_skel^.GetBoneID(b^.attachedBones[i]^)].tLocal.tr.x;
				inc(n);
			end;
		if n > 0 then
			result := result / n
		else
			if Assigned(b^.parent) then
				result := 0.5 * EstimateBoneLength(_skel^.GetBoneID(b^.parent^))
			else
				result := 0.5 * WorldScale;
	end;

	function SkeletonNode.IndexBoneChild(node: pSceneNode): sint;
	begin
		result := Index(node, pointer(boneChilds) + fieldoffset BoneChild _ node _, length(boneChilds), sizeof(BoneChild));
	end;

	procedure SkeletonNode._OnUpdate(const dt: float);
	const
		MIN_WT_SUM = 0.2;
		INV_MIN_WT_SUM = 1.0 / MIN_WT_SUM;
	var
		wtSum, t: float;
		curRot, animRot: Quaternion;
		curOfs, animOfs: Vec3;
		i, j, nActiveAnims: sint;
		nWtAnims: sint;
		wtAnims: array[1 .. 10] of pAnimation;
		tr: Transform;
	begin
	trace_call('SkeletonNode._OnUpdate');
		inherited _OnUpdate(dt);
		wtSum := 0.0;
		nWtAnims := 0;
		nActiveAnims := 0;
		for i := 0 to High(anims) do
			if anims[i].active then
				inc(nActiveAnims);

		if ignoreAccumNextTime then
		begin
			for i := 0 to High(anims) do
				if anims[i].active then
					anims[i].weight := anims[i].requiredWeight;
			ignoreAccumNextTime := no;
		end;

		for i := 0 to High(anims) do
		begin
			if not (anims[i].active or (anims[i].weight > 0.0)) then continue;
			SetAnimPhase(i, anims[i].phase + anims[i].phaseVel * dt, no);

			if anims[i].active then
			begin
				if anims[i].weight > anims[i].requiredWeight then
					anims[i].weight := max(anims[i].weight - anims[i].inSkel^.defaultOutVel * dt, anims[i].requiredWeight)
				else
					anims[i].weight := min(anims[i].weight + anims[i].inSkel^.defaultInVel * dt, anims[i].requiredWeight)
			end else
				anims[i].weight := max(anims[i].weight - (5.0 * anims[i].weight + 1.0) * dt, 0.0);

			if anims[i].weight > 0.0 then
			begin
				if nWtAnims < High(wtAnims) then
				begin
					wtSum := wtSum + anims[i].weight;
					inc(nWtAnims);
					wtAnims[nWtAnims] := @anims[i];
				end
			{$ifdef Debug}
				else
				begin
					Log('SkeletonNode._UpdateTree: превышен предел wtAnims (' + ToString(High(wtAnims)) + ')', logWarning);
					break;
				end
			{$endif};
			end;
		end;

		for j := 0 to High(bones) do
		begin
			curRot.v4 := Vec4.Zero;
			curOfs := Vec3.Zero;
			wtSum := 0.0;
			for i := 1 to nWtAnims do
				if Assigned(wtAnims[i]^.inSkel^.bones[j]) then
				begin
					t := wtAnims[i]^.weight * wtAnims[i]^.inSkel^.bones[j]^.weight;
					wtAnims[i]^.inSkel^.bones[j]^.GetBoneState(wtAnims[i]^.phase, animRot, animOfs);
					if IsZero(wtSum) then
						curRot := animRot
					else
						curRot := lerp(curRot, animRot, t / (wtSum + t));
					curOfs := curOfs + animOfs * t;
					wtSum += t;
				end;

			if wtSum < MIN_WT_SUM then
			begin
				wtSum := wtSum * INV_MIN_WT_SUM;
				curRot := curRot * INV_MIN_WT_SUM + (1.0 - wtSum) * _skel^.bones[j].rotation;
				curOfs := curOfs * INV_MIN_WT_SUM + (1.0 - wtSum) * _skel^.bones[j].offset;
			end else
			begin
				wtSum := 1.0 / wtSum;
				curOfs := curOfs * wtSum;
			end;

			if Assigned(bones[j].ik) then
			begin
				tr := bones[j].tLocal;
				tr.tr := curOfs;
				bones[j].SetLocalTransform(tr, self);
				if bones[j].ik^.Update(self, bones[j], dt) then
					continue
				else
					bones[j].KillIK;
			end;

			bones[j].SetLocalTransform(TranslateRotate(curOfs, curRot.Normalized), self);
		end;

		for i := 0 to High(anims) do anims[i].active := no;
	leave_call
	end;

	function SkeletonNode._SuitsTo(know: SceneKnowledge): boolean;
	begin
		case know of
			scene_Update: result := yes;
			else
				result := inherited _SuitsTo(know);
		end;
	end;

	procedure _UnlinkNode(obj: pObject; param: pointer);
	var
		ik: SkeletonNode.pInverseKinematics absolute param;
	begin
		Assert(obj = pObject(ik^._node));
		ik^._NodeLost;
	end;

	procedure SkeletonNode.tInverseKinematics._Reset;
	begin
		case _mode of
			ik_ToNode: _node^.RemoveOnDestroyProc(@_UnlinkNode, @self);
		end;
		_mode := ik_None;
	end;

	procedure SkeletonNode.tInverseKinematics._NodeLost;
	begin
		Assert(_mode = ik_ToNode);
		_mode := ik_None;
	end;

	procedure SkeletonNode.tInverseKinematics._Initialize(var skel: SkeletonNode; bone: pBoneInstance; dese: boolean);
	begin
		_localConeAxis := (skel._skel^.bones[bone^.ID(skel)].rotation * BaseBoneVector).Normalized;
		if dese and (_mode = ik_ToNode) then
		begin
			Assert(Assigned(_node));
			_node^.AddOnDestroyProc(@_UnlinkNode, @self);
		end;
	end;

	constructor SkeletonNode.tInverseKinematics.Init(var skel: SkeletonNode; bone: pBoneInstance);
	begin
		_velocity := 0.0;
		_mode := ik_None;
		_node := nil;
		_target := Vec3.Zero;
		_Initialize(skel, bone, no);
	end;

	constructor SkeletonNode.tInverseKinematics.DeseInit;
	begin
	end;

	destructor SkeletonNode.tInverseKinematics.Done;
	begin
		_Reset;
	end;

	procedure SkeletonNode.tInverseKinematics.SetNode(newNode: pSceneNode; const newNodeTransform: Transform; const newVelocity, newTimeout: float);
	begin
		_Reset;
		if not Assigned(newNode) then exit;
		_mode := ik_ToNode;
		_node := newNode;
		_node^.AddOnDestroyProc(@_UnlinkNode, @self);
		_nodeTf := newNodeTransform;
		_velocity := newVelocity;
		Assert(GreaterThan(_velocity, 0.0));
		_timeout := newTimeout;
	end;

	procedure SkeletonNode.tInverseKinematics.SetTarget(const newTarget: Vec3; const newVelocity, newTimeout: float);
	begin
		_Reset;
		_mode := ik_ToTarget;
		_target := newTarget;
		_velocity := newVelocity;
		Assert(GreaterThan(_velocity, 0.0));
		_timeout := newTimeout;
	end;

	function SkeletonNode.tInverseKinematics.Update(var skel: SkeletonNode; var bone: tBoneInstance; const dt: float): boolean;
	const
		Constraint = Pi / 2.5;
	var
		k: float;
		target: Vec3;
		targetLocal: Vec3;
		tf: Transform;
		pwrot: Quaternion;
		parent: pBoneInstance;
	begin
		_timeout -= dt;
		case _mode of
			ik_None: result := no;
			ik_ToNode, ik_ToTarget:
				begin
					if _mode = ik_ToNode then
					begin
						Assert(Assigned(_node));
						target := (_node^.globalTransform * _nodeTf).tr;
					end else
						target := _target;

					// TODO: подобрать нормальную формулу
					k := min(dt * _velocity, 1.0);

					// TODO: отдельные ограничения на каждую кость
					parent := bone.GetParent(skel);
					if Assigned(parent) then pwrot := skel.WorldRot * parent^.BindTransform(skel).rot else pwrot := skel.WorldRot;
					targetLocal := LimitByCone(
						(
							pwrot.Inversed *
							(target - (skel.GlobalTransform * bone.BindTransform(skel).tr))
						).Normalized, _localConeAxis, Constraint);
					tf := bone.tLocal;
					tf.rot := slerp(tf.rot, Quaternion.Rotation(BaseBoneVector, targetLocal), k);
					bone.SetLocalTransform(tf, skel);
					result := (k < 1.0) or (_timeout > 0.0);
				end;
		{$ifdef Debug} else Assert(no); {$endif}
		end;
	end;

	procedure SkeletonNameIdPair.Initialize(const newName: PoolString);
	begin
		name := newName;
		id := -1;
	end;

	procedure SkeletonNameIdPair.Finalize;
	begin
	end;

	procedure SkeletonNameIdPair.Update(skel: pSkeletonNode; what: tSkeletonIdKind);
	begin
		if Assigned(skel) then
			case what of
				id_Bone: id := skel^.GetBoneID(name);
				id_Anim: id := skel^.GetAnimationID(name);
				else Assert(no);
			end
		else
			id := -1;
	end;

	procedure SkeletonNameIdPair.Rename(const newName: PoolString; skel: pSkeletonNode; what: tSkeletonIdKind);
	begin
		name := newName;
		Update(skel, what);
	end;

{$ifdef use_serialization}
const
	SKEL_IGNORE_ACCUM_ONCE_BIT = 1 shl 0;
	SKEL_HAS_BONE_CHILDS_BIT   = 1 shl 1;

	BONE_HAS_IK_BIT = 1 shl 0;
	BONE_FLAGS = 1;

	ANIM_ACTIVE_BIT       = 1 shl 0;
	ANIM_HAS_WEIGHT_BIT   = 1 shl 1;
	ANIM_HAS_REQWT_BIT    = 1 shl 2;
	ANIM_HAS_PHASE_BIT    = 1 shl 3;
	ANIM_HAS_PHASEVEL_BIT = 1 shl 4;
	ANIM_FLAGS = 5;

	procedure SerializeSkeletonNode(se: pSerializer; obj: pointer);
	var
		sk: pSkeletonNode absolute obj;
		flags, cb: uint;
		i: sint;
		ik: SkeletonNode.pInverseKinematics;
		anim: SkeletonNode.pAnimation;
		bits: BitStream;
	begin
		with se^ do
		begin
			flags := 0;
			if sk^.ignoreAccumNextTime then flags := flags or SKEL_IGNORE_ACCUM_ONCE_BIT;
			if length(sk^.boneChilds) > 0 then flags := flags or SKEL_HAS_BONE_CHILDS_BIT;
			Serialize_ui8(stream, flags);
			SeObject(sk^._skel);

			VarInt.Write(stream, length(sk^.bones));
			bits := BitStream.Open(stream, file_Write);
			for i := 0 to High(sk^.bones) do
			begin
				cb := 0;
				if Assigned(sk^.bones[i].ik) then cb := cb or BONE_HAS_IK_BIT;
				bits.Write(cb, BONE_FLAGS);
			end;
			bits.Close;

			for i := 0 to High(sk^.bones) do
			begin
				ik := sk^.bones[i].ik;
				if Assigned(ik) then
				begin
					Serialize_tf32r8(stream, sk^.bones[i].tLocal);
					Serialize_f32(stream, ik^._velocity);
					Serialize_f32(stream, ik^._timeout);
					Serialize_ui8(stream, ord(ik^._mode));
					case ik^._mode of
						ik_ToNode:
							begin
								SeObject(ik^._node);
								Serialize_tf32r8(stream, ik^._nodeTf);
							end;
						ik_ToTarget: Serialize_vec3f32(stream, ik^._target);
						else Assert(no);
					end;
				end;
			end;

			VarInt.Write(stream, length(sk^.anims));
			bits := BitStream.Open(stream, file_Write);
			for i := 0 to High(sk^.anims) do
			begin
				anim := @sk^.anims[i];
				cb := 0;
				if anim^.active then cb := cb or ANIM_ACTIVE_BIT;
				if NotZero(anim^.weight) then cb := cb or ANIM_HAS_WEIGHT_BIT;
				if NotZero(anim^.requiredWeight) then cb := cb or ANIM_HAS_REQWT_BIT;
				if NotZero(anim^.phase) then cb := cb or ANIM_HAS_PHASE_BIT;
				if NotZero(anim^.phaseVel) then cb := cb or ANIM_HAS_PHASEVEL_BIT;
				bits.Write(cb, ANIM_FLAGS);
			end;
			bits.Close;

			for i := 0 to High(sk^.anims) do
			begin
				anim := @sk^.anims[i];
				if NotZero(anim^.weight) then Serialize_f16(stream, anim^.weight);
				if NotZero(anim^.phase) then Serialize_f32(stream, anim^.phase);
				if NotZero(anim^.phaseVel) then Serialize_f16(stream, anim^.phaseVel);
				if NotZero(anim^.requiredWeight) then Serialize_f16(stream, anim^.requiredWeight);
			end;

			if (flags and SKEL_HAS_BONE_CHILDS_BIT) <> 0 then
			begin
				VarInt.Write(stream, length(sk^.boneChilds));
				for i := 0 to High(sk^.boneChilds) do
				begin
					VarInt.Write(stream, sk^.boneChilds[i].bone^.ID(sk^));
					SeObject(sk^.boneChilds[i].node);
				end;
			end;
		end;
	end;

	procedure DeserializeSkeletonNode(de: pDeserializer; obj: pointer);
	var
		sk: pSkeletonNode absolute obj;
		flags: uint;
		i: sint;
		ik: SkeletonNode.pInverseKinematics;
		anim: SkeletonNode.pAnimation;
		bflags, aflags: array of uint;
		bits: BitStream;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);;
			sk^.ignoreAccumNextTime := (flags and SKEL_IGNORE_ACCUM_ONCE_BIT) <> 0;
			DeObjectR(sk^._skel);

			SetLength(sk^.bones, VarInt.Read(stream));
			SetLength(bflags, length(sk^.bones));
			bits := BitStream.Open(stream, file_Read);
			for i := 0 to High(bflags) do
				bflags[i] := bits.Read(BONE_FLAGS);
			bits.Close;

			for i := 0 to High(sk^.bones) do
			begin
				sk^.bones[i].dirty := yes;
				if (BONE_HAS_IK_BIT and bflags[i]) <> 0 then
				begin
					sk^.bones[i].tLocal := Deserialize_tf32r8(stream);
					ik := new(SkeletonNode.pInverseKinematics, DeseInit);
					ik^._velocity := Deserialize_f32(stream);
					ik^._timeout := Deserialize_f32(stream);
					ik^._mode := SkeletonNode.tInverseKinematics.tMode(Deserialize_ui8(stream));
					case ik^._mode of
						ik_ToNode:
							begin
								DeWeakA(ik^._node);
								ik^._nodeTf := Deserialize_tf32r8(stream);
							end;
						ik_ToTarget: ik^._target := Deserialize_vec3f32(stream);
						else raise ExhaustiveCase(ord(ik^._mode), 'Skeleton.IK.mode');
					end;
				end else
					ik := nil;
				sk^.bones[i].ik := ik;
			end;

			SetLength(sk^.anims, VarInt.Read(stream));
			SetLength(aflags, length(sk^.anims));
			bits := BitStream.Open(stream, file_Read);
			for i := 0 to High(aflags) do
				aflags[i] := bits.Read(ANIM_FLAGS);
			bits.Close;

			for i := 0 to High(sk^.anims) do
			begin
				anim := @sk^.anims[i];
				if (ANIM_HAS_WEIGHT_BIT and aflags[i]) <> 0 then anim^.weight := Deserialize_f16(stream) else anim^.weight := 0.0;
				if (ANIM_HAS_PHASE_BIT and aflags[i]) <> 0 then anim^.phase := Deserialize_f32(stream) else anim^.phase := 0.0;
				if (ANIM_HAS_PHASEVEL_BIT and aflags[i]) <> 0 then anim^.phaseVel := Deserialize_f16(stream) else anim^.phaseVel := 0.0;
				anim^.active := (ANIM_ACTIVE_BIT and aflags[i]) <> 0;
				if (ANIM_HAS_REQWT_BIT and aflags[i]) <> 0 then anim^.requiredWeight := Deserialize_f16(stream) else anim^.requiredWeight := 0.0;
			end;

			if (flags and SKEL_HAS_BONE_CHILDS_BIT) <> 0 then
			begin
				SetLength(sk^.boneChilds, VarInt.Read(stream));
				for i := 0 to High(sk^.boneChilds) do
				begin
					pPtrUint(@sk^.boneChilds[i].bone)^ := VarInt.Read(stream);
					DeWeakA(sk^.boneChilds[i].node);
				end;
			end;
		end;
	end;

	procedure SkeletonNodeDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		sk: pSkeletonNode absolute obj;
		i: sint;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: sk^.DeseInit;
			de_After:
				begin
					if length(sk^.bones) <> length(sk^._skel^.bones) then
						SerializationError( {$ifdef Debug} 'Количество костей скелета не совпадает' {$endif} );
					if length(sk^.anims) <> length(sk^._skel^.anims) then
						SerializationError( {$ifdef Debug} 'Количество анимаций скелета не совпадает' {$endif} );
					for i := 0 to High(sk^.bones) do
						if Assigned(sk^.bones[i].ik) then
							sk^.bones[i].ik^._Initialize(sk^, @sk^.bones[i], yes);
					for i := 0 to High(sk^.anims) do
						sk^.anims[i].inSkel := @sk^._skel^.anims[i];
					for i := 0 to High(sk^.boneChilds) do
						sk^.boneChilds[i].bone := @sk^.bones[pPtrUint(@sk^.boneChilds[i].bone)^];
				end;
		end;
	end;
{$endif}

	procedure SuiteLoadRawskel(obj: pointer; s: pStream); begin LoadRawskel(pSkeletonSource(obj)^, s); end;
	function LoadSkeletonSource(s: pStream): pObject; begin result := SkeletonSource.Deserialize(s); end;

	procedure Init;
	begin
		SkeletonSource.Loaders.Register('skel', @SuiteLoadRawskel);
		ResourcePool.Shared^.Register(TypeOf(SkeletonSource), @LoadSkeletonSource);

	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Skeleton source', TypeOf(SkeletonSource), nil, sizeof(SkeletonSource), yes, nil, nil, nil, nil)
		^.RegisterType('Skeleton node', TypeOf(SkeletonNode), TypeOf(SceneNode), sizeof(SkeletonNode), yes,
		               @SerializeSkeletonNode, @DeserializeSkeletonNode, nil, @SkeletonNodeDeSpecial);
	{$endif}
	end;

initialization
	&Unit('Skeleton').Initialize(@Init);
end.
