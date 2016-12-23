unit RenderLists;

{$include opts.inc}
{$ifdef Debug}
	{-$define ExtDebug}
{$endif}

interface

uses
	USystem, UClasses, UMath, Algo, U_GL, GLBase, GLClasses, Lights, SceneGraph, USkeleton, Cameras, SpatialIndex
{$ifdef use_serialization}, Streams {$endif}
{$ifdef Debug}, Utils, ULog {$endif};

type
	pRenderObjectLevelData = ^RenderObjectLevelData;
	RenderObjectLevelData = object
	private
		mesh: pGLMeshLevel;
		materials: array of pGLMaterialLevel;
		minLOD: float;
		blend: GLBlendMode;
		function Initialize(const newMinLod: float; newMesh: pGLMesh; const newMats: array of pGLMaterial): boolean;
		procedure Finalize;
	end;

type
	RenderObjectFlag = (robj_AllowInsta, robj_NoStartBnd, __robj_removing_from_pool_and_should_be_equal_only_to_itself);
	RenderObjectFlags = set of RenderObjectFlag;

	pRenderObjectData = ^RenderObjectData;
	RenderObjectData = object(&Object)
	private
		lods: array of RenderObjectLevelData;
		flags: RenderObjectFlags;
		instaGL: pGLEntityParams;
		function GetLOD(lod: float): sint;
		function _InstaParamID(const namae: PoolString; typ: GLType): sint;
	{$ifdef Debug} procedure _CheckMaxInsta; {$endif}
		function _Hash(desc: pRenderObjectData): Hash.Value; static;
		function _Equals(a, b: pRenderObjectData): boolean; static;
		function _MakeFromSrc: boolean;
	public
		srcMesh: pGLMesh;
		srcMats: array of pGLMaterial;
		constructor Init(newMesh: pGLMesh; const newMats: array of pGLMaterial; const newFlags: RenderObjectFlags);
		constructor DeseInit;
		destructor Done; virtual;
	end;

	pRenderObject = ^RenderObject;

	ppRenderRec = ^pRenderRec;
	pRenderRec = ^RenderRec;
	RenderRec = object
		node: pRenderObject;
		ro: pRenderObjectLevelData;
		lodID: sint;
		sqrDistToCamera: float;
		passed: boolean;
		procedure Render(var rp: tParams4Renderable; instances: ppRenderRec);
	end;

	{$define classname:=tSet_RobjData} {$define key_type:=pRenderObjectData} {$define null_value:=nil} {$include hash.h.inc}

	RenderObject = object(SceneNode)
	private
		_nInstances: sint; static;
		_robjDatas: tSet_RobjData; static;
		procedure _InitInstance; static;
	private
		_data: pRenderObjectData;
		_instaParams: array of record
			v: pNativeGLValue;
			data_id: sint;
		end;
		_skeleton: pSkeletonNode;
		procedure _SetSkeleton(newSkel: pSkeletonNode);
		procedure _AddInstaParam(v: pNativeGLValue);
		procedure _RelocateInstaParam(v_old, v_new: pNativeGLValue);
		procedure _SetupRuntime(dese: boolean);
	protected
		procedure _AfterAttach; virtual;
		procedure _OnUpdate(const dt: float); virtual;
		function _SuitsTo(know: SceneKnowledge): boolean; virtual;
	public
		gl: GLEntityParams;
		ltOmniA, ltOmniS: LightsSet;
		constructor Init(newMesh: pGLMesh; const newMats: array of pGLMaterial; const newFlags: RenderObjectFlags);
		constructor DeseInit;
		destructor Done; virtual;
		function GetInstance(lod: float): RenderRec;
		function AnyBatch: pGLBatch;

		property Data: pRenderObjectData read _data;
		property Skeleton: pSkeletonNode read _skeleton;
	end;

type
	pRenderList = ^RenderList;
	RenderList = object
	private
	type
		RenderListKind = (rl_Generic, rl_TranspaZ, rl_Transparent);

		ListByKind = object
		private
			_kind: RenderListKind;
			n: sint;
			v: array of RenderRec;
		public
			constructor Init(newKind: RenderListKind);
			destructor Done;
			procedure Add(const ro: RenderRec);
			procedure Prepare(var cam: Camera);
			procedure Render(var rp: tParams4Renderable; var cam: Camera; transp, zMask, byLights: boolean; var lights: SceneLights);
			procedure Clear;
		end;
	const
		RenderListsInfo: array[RenderListKind] of record
			transparent: boolean;
			zMask: boolean;
		end =
		(
			(transparent: no; zMask: no),
			(transparent: yes;  zMask: no),
			(transparent: yes;  zMask: yes)
		);
	var
		_list: array[RenderListKind] of ListByKind;
	public
		constructor Init;
		destructor Done;
		procedure Add(const ro: RenderRec);
		procedure Clear;
		procedure Render(const RT: GLRenderTarget; var cam: Camera; scn: pRenderScenario; var lights: SceneLights);
	end;

const
	RenderObjectFlagIds: array[RenderObjectFlag] of string = ('insta', 'nobnd', '???');

implementation

uses
	MMSystem, Character
{$ifdef use_serialization}, Serialization {$endif}
{$ifdef Profile}, Profile {$endif};

	{$define classname:=tSet_RobjData} {$define hash_func:=RenderObjectData._Hash} {$define inline_eq := RenderObjectData._Equals(_1, _2)}
	{$include hash.pp.inc}

	function RenderObjectLevelData.Initialize(const newMinLod: float; newMesh: pGLMesh; const newMats: array of pGLMaterial): boolean;
	var
		i: sint;
		mat: pGLMaterialLevel;
	begin
		mesh := newMesh^.GetLevel(newMinLod);
		materials := nil;
		minLod := newMinLod;
		blend := GLblend_Off;
		if Assigned(mesh) then
			SetLength(materials, length(mesh^.mesh^.batches))
		else
			materials := nil;
		result := Assigned(mesh);
	{$ifdef Debug} if not result then Log('Меш не задан', logError); {$endif}
		for i := 0 to High(materials) do
		begin
			if (i < length(newMats)) and Assigned(newMats[i]) then
				mat := newMats[i]^.GetLevel(newMinLod)
			else
				if not Assigned(mat) then
				begin
				{$ifdef Debug} Log('Материал для батча #' + ToString(i) + ' не задан', logError); {$endif}
					mat := nil;
					result := no;
				end {$ifdef Debug} else Log('Материал для батча #' + ToString(i) + ' не задан, использю предыдущий', logWarning); {$endif};
			if Assigned(mat) and (blend = GLblend_Off) and (mat^.blend <> GLblend_Off) then blend := mat^.blend;
			materials[i] := mat;
		end;
		if not result then Finalize;
	end;

	procedure RenderObjectLevelData.Finalize;
	begin
		materials := nil;
	end;

	function RenderObjectData._Hash(desc: pRenderObjectData): Hash.Value;
	begin
		result := Hash.OfPointer(desc^.srcMesh);
		if length(desc^.srcMats) > 0 then
			result := result xor Hash.OfPointer(desc^.srcMats[0]);
	end;

	function RenderObjectData._Equals(a, b: pRenderObjectData): boolean;
	var
		i: sint;
	begin
		result := a = b;
		if result or (a^.flags <> b^.flags) or (a^.srcMesh <> b^.srcMesh) or (length(a^.srcMats) <> length(b^.srcMats)) then
			exit;
		for i := 0 to High(a^.srcMats) do
			if a^.srcMats[i] <> b^.srcMats[i] then
				exit;
		result := yes;
	end;

	function RenderObjectData._MakeFromSrc: boolean;
		{$define procname:=sort_lod_values} {$define elem:=float} {$define less := _1 > _2} {$define openarray} {$include sort.inc}
	type
		tFloatArray = array of float;

		procedure add_value(var arr: tFloatArray; v: float);
		var
			i: sint;
		begin
			for i := 0 to High(arr) do
				if Equals(arr[i], v) then
					exit;
			SetLength(arr, length(arr) + 1);
			arr[High(arr)] := v;
		end;

	var
		i, j, n: sint;
		lodValues: tFloatArray;
	begin
		result := Assigned(srcMesh);
		if not result then
		begin
			{$ifdef Debug} Log('Нет меша :(', logError); {$endif}
			exit;
		end;

		for i := 0 to High(srcMats) do
		begin
			result := Assigned(srcMats[i]);
			if not result then
			begin
			{$ifdef Debug} Log('Не определён материал для батча #' + ToString(i), logError); {$endif}
				exit(no);
			end;
		end;

		if robj_AllowInsta in flags then instaGL := new(pGLEntityParams, Init) else instaGL := nil;

		lodValues := nil;
		for i := 0 to High(srcMesh^.mesh^.levels) do
			add_value(lodValues, srcMesh^.mesh^.levels[i].minLod);
		for j := 0 to High(srcMats) do
			for i := 0 to High(srcMats[j]^.lods) do
				add_value(lodValues, srcMats[j]^.lods[i].minLod);
		sort_lod_values(lodValues);

		n := 0;
		SetLength(lods, length(lodValues));
		for i := 0 to High(lods) do
			if lods[n].Initialize(lodValues[i], srcMesh, srcMats) then
				inc(n);
		SetLength(lods, n);
		if n = 0 then
		begin
		{$ifdef Debug} Log(srcMesh^.mesh^.name + ': нет активных уровней', logError); {$endif}
			result := no;
		end;
	end;

	constructor RenderObjectData.Init(newMesh: pGLMesh; const newMats: array of pGLMaterial; const newFlags: RenderObjectFlags);
	var
		i: sint;
	begin
		inherited Init;
		srcMesh := MakeRef(newMesh);
		SetLength(srcMats, length(newMats));
		for i := 0 to High(newMats) do
			srcMats[i] := MakeRef(newMats[i]);
		flags := newFlags;
		lods := nil;
		instaGL := nil;
		if not _MakeFromSrc then ConstructorFailed;
	end;

	constructor RenderObjectData.DeseInit;
	begin
		inherited DeseInit;
	end;

	destructor RenderObjectData.Done;
	var
	{$ifdef Debug} etc: string; {$endif}
		i: sint;
	begin
		Include(flags, __robj_removing_from_pool_and_should_be_equal_only_to_itself);
		if RenderObject._robjDatas.Remove(@self) then
		begin
		{$ifdef Debug}
			etc := '';
			if robj_AllowInsta in flags then etc += '[I+]';
			if etc <> '' then etc := ' ' + etc;
			LogR('Уничтожена RobjData: "' + srcMesh^.mesh^.name + '"' + etc + '; ', logDebug);
		{$endif}
		end;
		Exclude(flags, __robj_removing_from_pool_and_should_be_equal_only_to_itself);

		if Assigned(instaGL) then dispose(instaGL, Done);

		for i := 0 to High(lods) do
			lods[i].Finalize;
		for i := 0 to High(srcMats) do
			Release(srcMats[i]);
		Release(srcMesh);
		inherited Done;
	end;

	function RenderObjectData.GetLOD(lod: float): sint;
	var
		i: sint;
	begin
		for i := 0 to High(lods) do
			if lod >= lods[i].minLOD then
				exit(i);
		result := -1;
	end;

	function RenderObjectData._InstaParamID(const namae: PoolString; typ: GLType): sint;
	var
		v: NativeGLValue;
	begin
		Assert(typ <> GLType.Sampler);
		result := instaGL^.values.GetID(namae);
		if result >= 0 then
		begin
			Assert(instaGL^.values.raw[result].type_ = typ);
			exit;
		end;

		v.Initialize(namae, typ, 0, [NativeGLValueFlag.InstaPack]);
		instaGL^.values.AddRaw(v);
		result := instaGL^.values.GetID(namae);
	{$ifdef Debug} _CheckMaxInsta; {$endif}
	end;

{$ifdef Debug}
	procedure RenderObjectData._CheckMaxInsta;
	var
		i: sint;
		nUniforms: sint;
	begin
		nUniforms := 0;
		for i := 0 to High(instaGL^.values.raw) do
			nUniforms += UniformComponents(instaGL^.values.raw[i].Type_);
		stat.Note(USystem.max_uniforms_per_instance, nUniforms);
		Assert(BASE_UNIFORMS_PER_INSTANCE + nUniforms <= MAX_UNIFORMS_PER_INSTANCE,
			'Too many instance uniforms (' + ToString(nUniforms) + ', max. ' + ToString(MAX_UNIFORMS_PER_INSTANCE - BASE_UNIFORMS_PER_INSTANCE) +
			') in "' + srcMesh^.mesh^.name + '"');
	end;
{$endif}

	procedure RenderRec.Render(var rp: tParams4Renderable; instances: ppRenderRec);
	var
		i, j: sint;
		mv, t: Transform;
		mesh: pGLMesh;
		skel: pSkeletonNode;
		bnd: pBounding;
	begin
		if rp.nInsta = 1 then
			rp.roParams := @node^.gl
		else
		begin
			node^._data^.instaGL^.flags := node^.gl.flags;
			rp.roParams := node^._data^.instaGL;
		end;

		skel := node^.skeleton;
		if Assigned(skel) then
			for i := 0 to High(skel^.bones) do
			begin
				t := skel^.skeleton^.bones[i].invBind;
				UInvBonePos^.SetVec3(t.tr, i);
				UInvBoneRot^.SetVec4(t.rot.v4, i);
				t := skel^.bones[i].BindTransform(skel^);
				UBonePos^.SetVec3(t.tr, i);
				UBoneRot^.SetVec4(t.rot.v4, i);
			end;
		for i := 0 to rp.nInsta-1 do
		begin
			mv := rp.view^ * instances[i]^.node^.globalTransform;
			UModelRot^.SetVec4(instances[i]^.node^.WorldRot.v4, i);
			UModelTrans^.SetVec4(Vec4.Make(instances[i]^.node^.WorldPos, instances[i]^.node^.WorldScale), i);
			UMVRot^.SetVec4(mv.rot.v4, i);
			UMVTrans^.SetVec4(Vec4.Make(mv.tr, mv.scale), i);
			if rp.nInsta > 1 then
				for j := 0 to High(instances[i]^.node^._instaParams) do
					node^._data^.instaGL^.values.raw[instances[i]^.node^._instaParams[j].data_id][i] := instances[i]^.node^._instaParams[j].v^[0];
		end;

		mesh := ro^.mesh^.mesh;
		bnd := mesh^.mesh^.BoundingPtr;
		if Assigned(bnd) then
		begin
			UAABB^.SetVec3(bnd^.aabb.A, 0);
			UAABB^.SetVec3(bnd^.aabb.Sizes, 1);
		end;
		ro^.mesh^.Draw(rp, ro^.materials);
	end;

	procedure AddInstaParam(v: pNativeGLValue; param: pObject);
	var
		ro: pRenderObject absolute param;
	begin
		if v^.type_ <> GLType.Sampler then
			ro^._AddInstaParam(v);
	end;

	procedure RelocateInstaParam(old, new: pNativeGLValue; param: pObject);
	var
		ro: pRenderObject absolute param;
	begin
		ro^._RelocateInstaParam(old, new);
	end;

	procedure RenderObject._InitInstance;
	begin
		if _nInstances = 0 then _robjDatas.Init;
		inc(_nInstances);
	end;

	constructor RenderObject.Init(newMesh: pGLMesh; const newMats: array of pGLMaterial; const newFlags: RenderObjectFlags);
	var
		test: pRenderObjectData;
	begin
		inherited Init;
		_InitInstance;

		_data := nil;
		_skeleton := nil;
		ltOmniA := [];
		ltOmniS := [];
		gl.Init;
		_instaParams := nil;

		test := new(pRenderObjectData, Init(newMesh, newMats, newFlags));
		if not Assigned(test) then ConstructorFailed;
		_data := MakeRef(_robjDatas.Find(test));
	{$ifdef ExtDebug}
		if Assigned(_data) then
			Log('Данные рендеробжекта [' + _data^.lods[0].mesh^.mesh^.namae + '] объединены с уже созданными.', logDebug);
	{$endif}

		// ВНИМАНИЕ!
		// Если освободить test ДО добавления его копии в RobjDatas, внутренние данные (меш, материалы) МОГУТ быть
		// уничтожены сборкой мусора! При передаче объектов из скрипта этого никогда не произойдёт, но это возможно.
		if not Assigned(_data) then
		begin
			_data := MakeRef(new(pRenderObjectData, Init(newMesh, newMats, newFlags)));
			if Assigned(_data) then
			begin
			{$ifdef ExtDebug} LogR('Создана RobjData [' + _data^.lods[0].mesh^.mesh^.namae + ']; ', logDebug); {$endif}
				_robjDatas.Add(_data);
			end;
		end;
		Free(test);
		if not Assigned(_data) then ConstructorFailed;

		_SetupRuntime(no);
	end;

	procedure RenderObject._SetupRuntime(dese: boolean);
	var
		i: sint;
	begin
		if not (robj_NoStartBnd in _data^.flags) then
		begin
			StartBnd := _data^.srcMesh^.mesh^.BoundingPtr;
		{$ifdef Debug}
			if not Assigned(StartBnd) then
				Log(_data^.srcMesh^.mesh^.name + ' - нет описанного объёма', logWarning);
		{$endif}
		end;

		if robj_AllowInsta in _data^.flags then
		begin
			if dese then
				for i := 0 to High(gl.values.raw) do
					_AddInstaParam(@gl.values.raw[i]);
			gl.values.SetCallbacks(@AddInstaParam, @RelocateInstaParam, @self);
		end;
	end;

	constructor RenderObject.DeseInit;
	begin
		inherited DeseInit;
		_InitInstance;
	end;

	destructor RenderObject.Done;
	begin
		gl.Done;
		_SetSkeleton(nil);
		Release(_data);
		dec(_nInstances);
		if _nInstances = 0 then _robjDatas.Done;
		inherited Done;
	end;

	procedure RenderObject._AfterAttach;
	begin
		inherited _AfterAttach;
		if TypeOf(parent^) = TypeOf(Doll) then
			_SetSkeleton(pDoll(parent)^.Skeleton);
	end;

	procedure RenderObject._OnUpdate(const dt: float);
	begin
		inherited _OnUpdate(dt);
		if Cullable then
		begin
			ltOmniA := [];
			ltOmniS := [];
		end else
		begin
			ltOmniA := AllLights;
			ltOmniS := AllLights;
		end;
	end;

	function RenderObject._SuitsTo(know: SceneKnowledge): boolean;
	begin
		case know of
			scene_Update, scene_OmniVisibleWithoutCulling: result := yes;
			else
				result := inherited _SuitsTo(know);
		end;
	end;

	procedure RenderObject._SetSkeleton(newSkel: pSkeletonNode);
	begin
		if _skeleton <> newSkel then
		begin
			if Assigned(_skeleton) and (_skeleton^.parent = pSceneNode(@self)) then
				_skeleton^.Detach;
			SetRef(_skeleton, newSkel);
			if Assigned(_skeleton) and (not Assigned(_skeleton^.parent)) then
				Attach(_skeleton);
		end;
	end;

	procedure RenderObject._AddInstaParam(v: pNativeGLValue);
	var
		id: sint;
	begin
		id := length(_instaParams);
		SetLength(_instaParams, id + 1);
		_instaParams[id].v := v;
		_instaParams[id].data_id := _data^._InstaParamID(v^.namae, v^.type_);
	end;

	procedure RenderObject._RelocateInstaParam(v_old, v_new: pNativeGLValue);
	var
		i: sint;
	begin
		for i := 0 to High(_instaParams) do
			if _instaParams[i].v = v_old then
			begin
				_instaParams[i].v := v_new;
				break;
			end;
	end;

	function RenderObject.GetInstance(lod: float): RenderRec;
	var
		lodID: sint;
	begin
		lodID := _data^.GetLOD(lod);
		if lodID >= 0 then
		begin
			result.lodID := lodID;
			result.node := @self;
			result.ro := @_data^.lods[lodID];
		end else
			result.ro := nil;
	end;

	function RenderObject.AnyBatch: pGLBatch;
	begin
		if length(_data^.lods) > 0 then
			result := @_data^.lods[0].mesh^.mesh^.batches[0]
		else
			result := nil;
	end;

	constructor RenderList.ListByKind.Init(newKind: RenderListKind);
	begin
		_kind := newKind;
		n := 0;
		v := nil;
	end;

	destructor RenderList.ListByKind.Done;
	begin
	end;

	procedure RenderList.ListByKind.Add(const ro: RenderRec);
	begin
		inc(n);
		if n > length(v) then SetLength(v, 2 * n);
		v[n - 1] := ro;
	end;

	procedure RenderList.ListByKind.Clear;
	begin
		n := 0;
	end;

	function TranspaLess(const a, b: RenderRec): boolean;
	begin
		if (abs(a.sqrDistToCamera - b.sqrDistToCamera) > 1.0) or (a.ro^.blend = b.ro^.blend) then
			result := a.sqrDistToCamera < b.sqrDistToCamera
		else
			result := a.ro^.blend < b.ro^.blend;
	end;

{$define procname:=sort_transparent} {$define elem:=RenderRec} {$define less := TranspaLess(_1, _2)} {$include sort.inc}

{$define procname:=sort_by_material_and_distance} {$define elem:=RenderRec}
{$define less := (_1.ro < _2.ro) or ((_1.ro = _2.ro) and (_1.sqrDistToCamera < _2.sqrDistToCamera))}
{$include sort.inc}

	procedure RenderList.ListByKind.Prepare(var cam: Camera);
	var
		i: sint;
		d: float;
	begin
	trace_call('RenderList.Prepare');
		for i := 0 to n - 1 do
		begin
			d := SqrDistance(v[i].node^.WorldPos, cam.pos);
			if d = Infinity then
			begin
			{$ifdef Debug} Log('Неверное расстояние: ' + v[i].ro^.mesh^.mesh^.mesh^.name + ' (слишком удалённый объект?)', logWarning); {$endif}
				d := 1.0e10;
			end;
			v[i].sqrDistToCamera := d;
		end;
		if (RenderListsInfo[_kind].transparent) and (RenderListsInfo[_kind].zMask) then
			sort_transparent(pRenderRec(v), n)
		else
			sort_by_material_and_distance(pRenderRec(v), n);
	leave_call
	end;

	procedure RenderList.ListByKind.Render(var rp: tParams4Renderable; var cam: Camera; transp, zMask, byLights: boolean; var lights: SceneLights);
	var
		nInsta: uint;
		insta: array[0 .. MAX_INSTA_LIMIT - 1] of pRenderRec;
		prev: pRenderRec;

		procedure FlushInsta;
		begin
			if nInsta = 0 then exit;
			rp.nInsta := nInsta;
			insta[0]^.Render(rp, @insta[0]);
			nInsta := 0;
			prev := nil;
		end;

		procedure AddInsta(id: sint);
		var
			inst: pRenderRec;
		begin
			inst := @v[id];
			if (nInsta >= gl.MaxInstances) or (not (robj_AllowInsta in inst^.node^._data^.flags)) or
				((Assigned(prev)) and (inst^.ro <> prev^.ro))
			then
				FlushInsta
			else
				Assert((not Assigned(prev)) or (inst^.node^._data = prev^.node^._data));

			insta[nInsta] := inst;
			inc(nInsta);
			prev := inst;
		end;

	var
		i, j, k: sint;
		lp: tLightingPassesInfo;
		curOmniA, curOmniS: LightsSet;
		ld: tLightDetail;
		rast: GLRasterizerState;
	begin
		if n = 0 then exit;
		nInsta := 0;
		prev := nil;
		if zMask then
		begin
			rast.depthMask := yes;
			gl.SetRasterizerState(rast, [GLrast_DepthMask]);
		end;

		if byLights then
		begin
			rp.useMatBlend := transp;
			// освещение - аддитивный блендинг
			// в Z пишет только первый проход
			// TODO: это может пригодиться не только для освещения
			for i := 0 to n - 1 do
				v[i].passed := no;
			for i := 0 to n - 1 do
			begin
				if v[i].passed then continue;
				curOmniA := v[i].node^.ltOmniA;
				curOmniS := v[i].node^.ltOmniS;
				GetLightingPasses(lights, curOmniA, curOmniS, lp);
				if lp.count > 0 then
					UInvNPasses^.SetFloat(1.0 / lp.count)
				else
					UInvNPasses^.SetFloat(1.0);
				for j := 0 to lp.count - 1 do
				begin
					if (not transp) and (j <= 1) then
					begin
						rast.depthMask := j > 0;
						if j = 0 then rast.blend := GLblend_Off else rast.blend := GLblend_AddWa;
						gl.SetRasterizerState(rast, [GLrast_DepthMask, GLrast_Blend]);
					end;
					PrepareLightingPass(lp, j, cam);
					for ld in tLightDetail do
						rp.sh.NLights[ld] := lp.passes[j].byDetail[ld].n;
					for k := i to n  - 1 do
						if (not v[k].passed) and (v[k].node^.ltOmniA = curOmniA) and (v[k].node^.ltOmniS = curOmniS) then
							AddInsta(k);
					FlushInsta;
				end;
				for k := i to n - 1 do
					if (not v[k].passed) and (v[k].node^.ltOmniA = curOmniA) and (v[k].node^.ltOmniS = curOmniS) then
						v[k].passed := yes;
			end;
		end else
		begin
			rp.useMatBlend := yes;
			if transp then
			begin
				for i := n - 1 downto 0 do
					AddInsta(i);
			end else
			begin
				for i := 0 to n - 1 do
					AddInsta(i);
			end;
			FlushInsta;
		end;

		if zMask then
		begin
			rast.depthMask := no;
			gl.SetRasterizerState(rast, [GLrast_DepthMask]);
		end;
	end;

	constructor RenderList.Init;
	var
		rl: RenderListKind;
	begin
		for rl in RenderListKind do
			_list[rl].Init(rl);
	end;

	destructor RenderList.Done;
	var
		rl: RenderListKind;
	begin
		for rl in RenderListKind do
			_list[rl].Done;
	end;

	procedure RenderList.Add(const ro: RenderRec);
	begin
		if not Assigned(ro.ro) then exit;
		case ro.ro^.blend of
			GLblend_Off: _list[rl_Generic].Add(ro);
			GLblend_MixZ: _list[rl_TranspaZ].Add(ro);
			else _list[rl_Transparent].Add(ro);
		end;
	end;

	procedure RenderList.Clear;
	var
		rl: RenderListKind;
	begin
		for rl in RenderListKind do
			_list[rl].Clear;
	end;

	procedure RenderList.Render(const RT: GLRenderTarget; var cam: Camera; scn: pRenderScenario; var lights: SceneLights);
	var
		pm, invPm: Matrix4;
		passNo, i, j: sint;
		rp: tParams4Renderable;
		rl: RenderListKind;
		useLts: boolean;
		v2: Vec2;
	begin
	trace_call('RenderList.Render');
		pm := cam.Projection;
		invPm := cam.InversedProjection;
		UProjMatrix^.SetMat4(pm);
		for i := 0 to 1 do
		begin
			for j := 0 to 1 do
				v2.data[j] := invPm.m[2 + j, 2 + i];
			UInvProjZW^.SetVec2(v2, i);
		end;
		UCamPos^.SetVec3(cam.pos);

		for rl in RenderListKind do
			_list[rl].Prepare(cam);

		rp.Reset;
		rp.rt := @RT;
		rp.view := @cam.viewTransform;
		for rl in RenderListKind do
			for passNo := 0 to High(scn^.passes) do
			begin
				SetRef(rp.pass, scn^.passes[passNo]);
				useLts := scn^.passes[passNo]^.special = pass_UseLights;
				_list[rl].Render(rp, cam, RenderListsInfo[rl].transparent, RenderListsInfo[rl].zMask, useLts, lights);
			end;
		rp.Finalize;
	leave_call
	end;

{$ifdef use_serialization}
const
	ROD_ALLOW_INSTA_BIT  = 1 shl 0;
	ROD_NO_START_BND_BIT = 1 shl 1;
	ROD_MULTIPLE_MATERIALS = 1 shl 2;

	procedure SerializeRobjData(se: pSerializer; obj: pointer);
	var
		rd: pRenderObjectData absolute obj;
		flags: uint;
		i: sint;
	begin
		with se^ do
		begin
			flags := 0;
			if robj_AllowInsta in rd^.flags then flags := flags or ROD_ALLOW_INSTA_BIT;
			if robj_NoStartBnd in rd^.flags then flags := flags or ROD_NO_START_BND_BIT;
			if length(rd^.srcMats) <> 1 then flags := flags or ROD_MULTIPLE_MATERIALS;
			Serialize_ui8(stream, flags);
			SeObject(rd^.srcMesh);
			if length(rd^.srcMats) <> 1 then Serialize_ui8(stream, length(rd^.srcMats));
			for i := 0 to High(rd^.srcMats) do
				SeObject(rd^.srcMats[i]);
		end;
	end;

	procedure DeserializeRobjData(de: pDeserializer; obj: pointer);
	var
		rd: pRenderObjectData absolute obj;
		flags, n: uint;
		i: sint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			rd^.flags := [];
			if (flags and ROD_ALLOW_INSTA_BIT) <> 0 then Include(rd^.flags, robj_AllowInsta);
			if (flags and ROD_NO_START_BND_BIT) <> 0 then Include(rd^.flags, robj_NoStartBnd);
			DeObjectR(rd^.srcMesh);
			if (flags and ROD_MULTIPLE_MATERIALS) <> 0 then n := Deserialize_ui8(stream) else n := 1;
			SetLength(rd^.srcMats, n);
			for i := 0 to High(rd^.srcMats) do
				DeObjectA(rd^.srcMats[i]);
		end;
	end;

	procedure RobjDataDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		rd: pRenderObjectData absolute obj;
		d2: pRenderObjectData;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: rd^.DeseInit;
			de_After:
				begin
					d2 := RenderObject._robjDatas.Find(rd);
					if Assigned(d2) then
						rd := d2
					else
					begin
						rd^._MakeFromSrc;
						RenderObject._robjDatas.Add(rd);
					end;
				end;
		end;
	end;

const
	RO_HAS_SKELETON_BIT = 1 shl 0;

	procedure SerializeRenderObject(se: pSerializer; obj: pointer);
	var
		ro: pRenderObject absolute obj;
		flags: uint;
	begin
		with se^ do
		begin
			flags := 0;
			if Assigned(ro^.skeleton) then flags := flags or RO_HAS_SKELETON_BIT;
			Serialize_ui8(stream, flags);
			SeObject(ro^._data);
			if Assigned(ro^.skeleton) then SeObject(ro^.skeleton);
			SeObject(@ro^.gl, TypeOf(GLEntityParams));
		end;
	end;

	procedure DeserializeRenderObject(de: pDeserializer; obj: pointer);
	var
		ro: pRenderObject absolute obj;
		flags: uint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			DeObjectR(ro^._data);
			if (flags and RO_HAS_SKELETON_BIT) <> 0 then DeObjectR(ro^._skeleton);
			DeWeakAtR(ro^.gl);
		end;
	end;

	procedure RenderObjectDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		ro: pRenderObject absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: ro^.DeseInit;
			de_After2: ro^._SetupRuntime(yes); // After занят Data
		end;
	end;
{$endif}

	procedure Init;
	begin
		RenderObject._nInstances := 0;

	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Render object data', TypeOf(RenderObjectData), nil, sizeof(RenderObjectData), yes,
		               @SerializeRobjData, @DeserializeRobjData, nil, @RobjDataDeSpecial)
		^.RegisterType('Render object', TypeOf(RenderObject), TypeOf(SceneNode), sizeof(RenderObject), yes,
		               @SerializeRenderObject, @DeserializeRenderObject, nil, @RenderObjectDeSpecial)
		^.RegisterFuncs([@AddInstaParam, @RelocateInstaParam]);
	{$endif}
	end;

initialization
	&Unit('RenderList').Initialize(@Init);
end.
