unit Scene;

{$include opts.inc}

interface

uses
	USystem, UMath, UClasses, SpatialIndex, U_GL, GLBase, GLClasses, SceneGraph, Cameras, Audio,
	Physics, Lights, PathFinding, RenderLists, Inventories
{$ifdef use_serialization}, Streams {$endif}
{$ifdef Debug}, ULog, Utils{$endif};

type
	SceneFlag = (scene_Main, scene_Phys);
	SceneFlags = set of SceneFlag;

	SceneNodeProc = function(const obj: pSceneNode; param: pointer): boolean;

	pSceneKnowledgePart = ^SceneKnowledgePart;
	SceneKnowledgePart = record
		list: SceneNode.List;
		kd: SceneKdTree;
	end;

	pScene = ^SceneRoot;
	SceneRoot = object(SceneNode)
	public type
		OnAttachDetachProc = procedure(scene: pScene; node: pSceneNode; attach: boolean; const info: SingleDelegateInfo);
		OnAttachDetachArgs = record
			scene: pScene;
			node: pSceneNode;
			attach: boolean;
		end;

		RayCastReason = (cast_ForCamera, cast_ForItem);
		RayCastProc = function(reason: RayCastReason; node: pSceneNode; const info: SingleDelegateInfo): boolean;
		RayCastArgs = record
			reason: RayCastReason;
			node: pSceneNode;
			result: boolean;
		end;

	const
		RayCastReasonIds: array[RayCastReason] of string = ('camera', 'item');
	private var
		_flags: SceneFlags;
		_lastKdRebuild: float;
		_kd: SceneKdTree;
		_rlStackTop: sint;
		_rlStack: array[0 .. 1] of RenderList;
		_nObjs: sint;
		_neutralized: boolean;
	{$ifdef Debug}
		_queryables: sint;
		_queryStart, _queriesTimeSum, _lastQueryReport: Ticks;
		_nQueries: sint;
		procedure _StartQuery;
		procedure _EndQuery;
	{$endif}
		procedure _Initialize;
		procedure _CreateRenderList(var cam: Camera; sc: pRenderScenario; const lod: float; var rl: RenderList; exclude: pSceneNode; clipPlane: pPlane);
		procedure _CallOnCreateDestroyOutdoorItem(create: boolean; item: pItem; node: pSceneNode);
	public
		phys: pPhysWorld;
		lights: SceneLights;
		ways: Waypoints;
		camera: Camera;
		knowledge: array[SceneKnowledge] of SceneKnowledgePart;
		onAttachDetach, onRayCast, onCreateOutdoorItem, onDestroyOutdoorItem: MultiDelegate;

		constructor Init(newFlags: SceneFlags);
		constructor DeseInit;
		destructor Done; virtual;
		procedure Update(const dt: float);
		procedure Render(var cam: Camera; sc: pRenderScenario; lod: float = 1.0; exclude: pSceneNode = nil; clipPlane: pPlane = nil; clear: GLRenderBuffers = [GLbuffer_Color, GLbuffer_Depth]);
		procedure Neutralize;
		property Neutralized: boolean read _neutralized;
		property Flags: SceneFlags read _flags;

		procedure MakeQueryable(obj: pSceneNode);
		procedure QueryableChanged(obj: pSceneNode; const oldBnd: Bounding);
		procedure UnmakeQueryable(obj: pSceneNode);
		procedure Query(var ibnd: Bounding; proc: SceneNodeProc; param: pointer);
		procedure Query(const frustum: Frustum; proc: SceneNodeProc; param: pointer);
		function GetAABB: AABB;
		function GetKD: pSceneKdTree;

		procedure NotifyAtDt(node: pSceneNode; isattach: boolean);

		procedure Remember(obj: pSceneNode; know: SceneKnowledge);
		function Remembered(obj: pSceneNode; know: SceneKnowledge): boolean;
		function RememberedID(obj: pSceneNode; know: SceneKnowledge): sint;
		procedure Forget(obj: pSceneNode; know: SceneKnowledge);
		procedure Forget(obj: pSceneNode);
		property NObjs: sint read _nObjs;

		function RayCast(reason: RayCastReason; const a, b: Vec3; x: pFloat = nil): Vec3;
		function PlaceOutdoorItem(item: pItem; const wtf: Transform; velocity: pVec3): pOutdoorItem;
		procedure DestroyOutdoorItem(node: pOutdoorItem);
	end;

const
	SceneFlagIds: array[SceneFlag] of string = ('main', 'phys');

implementation

uses
	MMSystem, EnvironmentMaps
{$ifdef use_serialization}, Serialization {$endif}
{$ifdef Profile}, Profile{$endif};

const
	KnowledgeInfo: array[SceneKnowledge] of record
		list: boolean;
		kd: boolean;
	end =
	(
		(list: yes; kd: no),  // scene_Update
		(list: yes; kd: no),  // scene_DelayedUpdate
		(list: yes; kd: no),  // scene_OmniVisibleWithoutCulling
		(list: yes; kd: yes), // scene_Light
		(list: yes; kd: no)   // scene_Water
	);
	KnowKd: array[0 .. 0] of SceneKnowledge = (scene_Light);

	procedure SceneRoot._Initialize;
	var
		know: SceneKnowledge;
		i: sint;
	begin
		scene := @self;
		_neutralized := no;
	{$ifdef Debug}
		_nQueries := 0;
		_queryables := 0;
	{$endif}

		for i := 0 to High(_rlStack) do
			_rlStack[i].Init;
		_rlStackTop := -1;

		_kd.Init;
		_lastKdRebuild := -999.0;
		lights.Initialize;
		_nobjs := 0;

		for know in SceneKnowledge do
		begin
			knowledge[know].list.Init;
			if KnowledgeInfo[know].kd then
				knowledge[know].kd.Init;
		end;
	end;

	constructor SceneRoot.Init(newFlags: SceneFlags);
	var
	{$ifdef Debug} i: sint; {$endif}
		cflags: CameraFlags;
	begin
		inherited Init;
		_flags := newFlags;

		cflags := [];
		if scene_Main in _flags then cflags += [camera_Main];
		camera.Init(cflags); camera.MakeStatic;
		camera.DenyProjectionUpdateOnFly; // изменение размеров окна. TODO: обрабатывать по-нормальному.
		camera.Owner := @self;
	{$ifdef Debug}
		for i := 0 to GLBase.Config.nCsmSplits-1 do
			Log('CSM сплит #' + ToString(i) + ': Z = ' +
				ToString(camera.GetCSMSplitZ(i, GLBase.Config.nCsmSplits, GLBase.Config.csmLogK)));
	{$endif}

		if scene_Phys in _flags then
			phys := MakeRef(new(pPhysWorld, Init))
		else
			phys := nil;
		ways.Init(yes); ways.MakeStatic;

		onAttachDetach.Init;
		onRayCast.Init;
		onCreateOutdoorItem.Init;
		onDestroyOutdoorItem.Init;

		_Initialize;
		_AfterAttach;
	end;

	constructor SceneRoot.DeseInit;
	begin
		inherited DeseInit;
		_neutralized := no;
	end;

	destructor SceneRoot.Done;
	var
		know: SceneKnowledge;
		i: sint;
	begin
		if not _neutralized then
		begin
		{$ifdef Debug} Log('Сцена не "нейтрализована", хмм...', logWarning); {$endif}
			_BeforeDetach;
		end {$ifdef Debug} else Log('Сцена была "нейтрализована" до уничтожения', logDebug); {$endif};
		onAttachDetach.Clear;
		inherited Done; // должно быть до остального! отцепляемые узлы могут работать со сценой.
		onRayCast.Done;
		onDestroyOutdoorItem.Done;
		onCreateOutdoorItem.Done;
		onAttachDetach.Done;
		for know in SceneKnowledge do
		begin
			Assert(knowledge[know].list.n = 0);
			knowledge[know].list.Done;
			if KnowledgeInfo[know].kd then
			begin
				Assert(knowledge[know].kd.ObjectsCount = 0);
				knowledge[know].kd.Done;
			end;
		end;

		ways.Done;
		lights.Finalize;

		for i := 0 to High(_rlStack) do
			_rlStack[i].Done;
	{$ifdef Debug}
		if _queryables <> 0 then
			Log('SceneRoot: несбалансированы MakeQueryable / UnmakeQueryable (ост. ' + ToString(_queryables) + ')', logWarning);
	{$endif}
		Release(phys);
		_kd.Done;
		camera.Done;
	end;

	procedure SceneRoot.Update(const dt: float);
	var
		i: uint;
		ir: Quaternion;
		t: Matrix4;
		v4: Vec4;
		know: SceneKnowledge;
	begin
	trace_call('SceneRoot.Update');
		if Assigned(phys) then phys^.Update;

		i := 0;
		while i < knowledge[scene_Update].list.n do // внимание, n может измениться в ходе цикла!
		begin
			knowledge[scene_Update].list.items[i]^.Update(dt);
			inc(i);
		end;

		if (mm.SceneTimeSince(_lastKdRebuild) > 3.01) then
		begin
			_kd.Divide;
			for know in KnowKd do knowledge[know].kd.Divide;
			_lastKdRebuild := mm.SceneTime;
		end;

		camera.Update(dt);
		if scene_Main in _flags then
		begin
			for i := 0 to Camera.NSplits - 1 do
				USplits^.SetFloat(camera.Split(i), i);
			ir := camera.viewTransform.rot.Inversed;
			Sound.SetViewer(camera.Pos, ir * Vec3.PositiveY, ir * Vec3.NegativeZ);
			t := camera.viewTransform.ToMatrix;
			for i := 0 to 3 do
				v4.data[i] := -t.m[i, 2];
			UMainCamViewZv4^.SetVec4(v4);
			UpdateVirtualLights(@self, dt);
		end;

		i := 0;
		while i < knowledge[scene_DelayedUpdate].list.n do // здесь n тоже может измениться в ходе цикла
		begin
			knowledge[scene_DelayedUpdate].list.items[i]^.Update(dt);
			inc(i);
		end;
	leave_call
	end;

type
	pTraverse2RLParams = ^tTraverse2RLParams;
	tTraverse2RLParams = record
		update: boolean;
		rl: pRenderList;
		lod: float;
		cam: pCamera;
		exclude: pSceneNode;
		clipPlane: pPlane;
		nDupd: uint;
		dupd: array[0 .. 63] of pSceneNode;
	end;

	function __traverse_update(const obj: pSceneNode; param: pointer): boolean;
	var
		p: pTraverse2RLParams absolute param;
	begin
		result := yes;
		if obj = p^.exclude then exit;
		if Assigned(p^.clipPlane) and (p^.clipPlane^.SignedDistance(obj^.bnd^.sphere.center) < -obj^.bnd^.sphere.radius) then exit;
		if p^.update then
			if (TypeOf(obj^) <> TypeOf(RTTOperator)) or (p^.nDupd = High(p^.dupd)) then
				obj^.UpdateVis
			else
			begin
				p^.dupd[p^.nDupd] := obj;
				inc(p^.nDupd);
			end;
		if TypeOf(obj^) = TypeOf(RenderObject) then
			p^.rl^.Add(pRenderObject(obj)^.GetInstance(obj^.CalculateLOD(p^.cam^.pos) * p^.lod));
	end;

	procedure TraverseUpdateForEachWrapper(obj: pSceneNode; param: pointer);
	begin
		__traverse_update(obj, param);
	end;

	procedure SceneRoot._CreateRenderList(var cam: Camera; sc: pRenderScenario; const lod: float; var rl: RenderList; exclude: pSceneNode; clipPlane: pPlane);
	var
		frustum: UMath.Frustum;
		p: tTraverse2RLParams;
		i: uint;
		o: Vec3;
		p2: Plane;
		water: pWater;
	begin
	trace_call('SceneRoot._CreateRenderList');
		frustum := Frustum.FromMatrix(cam.ProjectionMatrix * cam.viewTransform.ToMatrix);
		p.update := (@cam = @camera) and (scenario_Uvis in sc^.flags);
		p.cam := @cam;
		p.rl := @rl;
		p.lod := lod;
		p.exclude := exclude;
		p.nDupd := 0;
		p.clipPlane := clipPlane;

		Query(frustum, @__traverse_update, @p);
		p.clipPlane := nil;
		knowledge[scene_OmniVisibleWithoutCulling].list.ForEach(@TraverseUpdateForEachWrapper, @p);

		for i := 1 to p.nDupd do
			p.dupd[i-1]^.UpdateVis;

		if Assigned(cam.postprocess) then
		begin
			for i := 1 to knowledge[scene_Water].list.n do
			begin
				water := pWater(knowledge[scene_Water].list.items[i-1]);
				if water^.Contains(cam.pos, cam.zNear) then
				begin
					o := PlanesIntersection(frustum.NearPlane, frustum.LeftPlane, frustum.BottomPlane);
					p2 := water^.Plane.ToBasis(o, Matrix3.Columns(
						PlanesIntersection(frustum.NearPlane, frustum.RightPlane, frustum.BottomPlane) - o,
						PlanesIntersection(frustum.NearPlane, frustum.LeftPlane, frustum.TopPlane) - o,
						cam.viewTransform.Inversed * Vec3.PositiveZ));
					cam.gl.flags[GL_CAMERA_FLAG_UNDERWATER] := yes;
					cam.gl.values.Value('water_plane', GLType.Vec3, 1, [NativeGLValueFlag.NonSerializable])^.SetVec3(Vec3.Make(p2.a, p2.b, p2.d));
					break;
				end;
			end;
		end;
	leave_call
	end;

	procedure SceneRoot.Render(var cam: Camera; sc: pRenderScenario; lod: float = 1.0; exclude: pSceneNode = nil; clipPlane: pPlane = nil; clear: GLRenderBuffers = [GLbuffer_Color, GLbuffer_Depth]);
	var
		rt: pRenderTarget;
		plane_mv: Plane;
	begin
	trace_call('SceneRoot.Render');
		Assert(_rlStackTop < High(_rlStack), 'renderlist stack overflow');
		inc(_rlStackTop);
		if cam.ClampToScene then cam.AdjustZToAABB(_kd.GetAABB);
		_CreateRenderList(cam, sc, lod, _rlStack[_rlStackTop], exclude, clipPlane);

		rt := cam.BeginDraw;
		rt^.Clear(clear);

		if Assigned(clipPlane) then
		begin
			GlobalGL.flags[GL_FLAG_CLIP_PLANE] := yes;
			plane_mv := cam.viewTransform * clipPlane^;
			UClipPlane^.SetVec4(plane_mv.v4);
		end;

		if rt^.size.Positive then
			UInvDestRTSizes^.SetVec2(1 / rt^.size);
		_rlStack[_rlStackTop].Render(rt^.inGL^, cam, sc, lights);

		if Assigned(clipPlane) then
			GlobalGL.flags[GL_FLAG_CLIP_PLANE] := no;
		cam.EndDraw;
		if Assigned(cam.postprocess) then
			cam.gl.flags[GL_CAMERA_FLAG_UNDERWATER] := no;
		_rlStack[_rlStackTop].Clear;
		dec(_rlStackTop);
	leave_call
	end;

	procedure SceneRoot.Neutralize;
	begin
		Assert(not _neutralized);
		_BeforeDetach;
		if scene_Main in _flags then Sound.ResetViewer;
		_neutralized := yes;
	end;

	procedure SceneRoot.MakeQueryable(obj: pSceneNode);
	var
		know: SceneKnowledge;
	begin
	trace_call('SceneRoot.MakeQueryable');
		Assert(Assigned(obj^.bnd));
		Assert(not obj^.scp.queryable);
		obj^.scp.queryable := yes;
		_kd.Add(obj, yes);

		for know in KnowKd do
			if Remembered(obj, know) then
				knowledge[know].kd.Add(obj, yes);
	{$ifdef Debug} inc(_queryables); {$endif}
	leave_call
	end;

	procedure SceneRoot.QueryableChanged(obj: pSceneNode; const oldBnd: Bounding);
	var
		know: SceneKnowledge;
	begin
	trace_call('SceneRoot.ChangeQueryable');
		Assert(obj^.scp.queryable);
		_kd.Changed(obj, oldBnd);

		for know in KnowKd do
			if Remembered(obj, know) then
				knowledge[know].kd.Changed(obj, oldBnd);
	leave_call
	end;

	procedure SceneRoot.UnmakeQueryable(obj: pSceneNode);
	var
		know: SceneKnowledge;
	begin
	trace_call('SceneRoot.UnmakeQueryable');
		Assert(Assigned(obj^.bnd));
		Assert(obj^.scp.queryable);
		obj^.scp.queryable := no;
		_kd.Remove(obj);

		for know in KnowKd do
			if Remembered(obj, know) then
				knowledge[know].kd.Remove(obj);
	{$ifdef Debug} dec(_queryables); {$endif}
	leave_call
	end;

	procedure SceneRoot.Query(var ibnd: Bounding; proc: SceneNodeProc; param: pointer);
	begin
	trace_call('SceneRoot.Query(bnd)');
	{$ifdef Debug} _StartQuery; {$endif}
		_kd.ForEachIntersected(ibnd, proc, param);
	{$ifdef Debug} _EndQuery; {$endif}
	leave_call
	end;

	procedure SceneRoot.Query(const frustum: Frustum; proc: SceneNodeProc; param: pointer);
	begin
	trace_call('SceneRoot.Query(frustrum)');
	{$ifdef Debug} _StartQuery; {$endif}
		_kd.ForEachIntersected(frustum, proc, param);
	{$ifdef Debug} _EndQuery; {$endif}
	leave_call
	end;

	function SceneRoot.GetAABB: AABB;
	begin
		result := _kd.GetAABB;
	end;

	function SceneRoot.GetKD: pSceneKdTree;
	begin
		result := @_kd;
	end;

{$ifdef Debug}
	procedure SceneRoot._StartQuery;
	begin
		if _nQueries = 0 then
		begin
			_queriesTimeSum := Ticks.Zero;
			_lastQueryReport := Ticks.Get;
		end;
		_queryStart := Ticks.Get;
	end;

	procedure SceneRoot._EndQuery;
	var
		dt: Ticks;
	begin
		inc(_nQueries);
		_queriesTimeSum += Ticks.Get - _queryStart;
		dt := Ticks.Get - _lastQueryReport;
		if dt >= Ticks.FromSeconds(4.0) then
		begin
			Log('SceneRoot.Query: ' + ToString(_nQueries) + ' шт. в течение ' + ToString(dt.ToSeconds) + ' с, в среднем по ' + ToString(_queriesTimeSum.ToMicroseconds / _nQueries) + ' мкс', logDebug);
			_lastQueryReport := Ticks.Get;
			_nQueries := 0;
		end;
	end;
{$endif}

	procedure _CallOnAttachDetach(const info: SingleDelegateInfo; param: pointer);
	var
		args: ^SceneRoot.OnAttachDetachArgs absolute param;
	begin
		SceneRoot.OnAttachDetachProc(info.proc)(args^.scene, args^.node, args^.attach, info);
	end;

	procedure SceneRoot.NotifyAtDt(node: pSceneNode; isattach: boolean);
	var
		args: OnAttachDetachArgs;
	begin
		if not onAttachDetach.Empty then
		begin
			args.scene := @self;
			args.node := node;
			args.attach := isattach;
			onAttachDetach.Call(@_CallOnAttachDetach, @args);
		end;
		if isattach then inc(_nObjs) else dec(_nObjs);
	end;

	procedure SceneRoot.Remember(obj: pSceneNode; know: SceneKnowledge);
	var
		id: sint;
	begin
		Assert(not Remembered(obj, know), 'duplicate knowledge');

		with knowledge[know] do
		begin
			id := list.n;
			obj^.scp.knowledgeId[know] := id;

			if KnowledgeInfo[know].list then
				list.Push(obj);

			if KnowledgeInfo[know].kd and obj^.scp.queryable then
				knowledge[know].kd.Add(obj, yes);
		end;
	end;

	function SceneRoot.Remembered(obj: pSceneNode; know: SceneKnowledge): boolean;
	begin
		result := obj^.scp.knowledgeId[know] >= 0;
	end;

	function SceneRoot.RememberedID(obj: pSceneNode; know: SceneKnowledge): sint;
	begin
		result := obj^.scp.knowledgeId[know];
	end;

	procedure SceneRoot.Forget(obj: pSceneNode; know: SceneKnowledge);
	var
		id: sint;
	begin
		with knowledge[know] do
		begin
			id := obj^.scp.knowledgeId[know];
			Assert(id >= 0, 'knowledge missing');
			if KnowledgeInfo[know].list then
			begin
				list.Last^.scp.knowledgeId[know] := id; // внимание, если это поставить ниже, оно сможет затереть -1
				list.RemoveReplace(id);
			end;
			obj^.scp.knowledgeId[know] := -1;

			if KnowledgeInfo[know].kd and obj^.scp.queryable then
				knowledge[know].kd.Remove(obj);
		end;
	end;

	procedure SceneRoot.Forget(obj: pSceneNode);
	var
		know: SceneKnowledge;
	begin
		for know in SceneKnowledge do
			if Remembered(obj, know) then
				Forget(obj, know);
	end;

	procedure _CallOnRayCast(const info: SingleDelegateInfo; param: pointer);
	var
		args: ^SceneRoot.RayCastArgs absolute param;
	begin
		with args^ do
			result := result and SceneRoot.RayCastProc(info.proc)(reason, node, info);
	end;

type
	pRayCastParams = ^tRayCastParams;
	tRayCastParams = record
		reason: SceneRoot.RayCastReason;
		scene: pScene;
		result: pVec3;
		resultx: pFloat;
		found: boolean;
	end;

	function _RayCastImpl(body: pRigidBody; const x: float; const point, normal: Vec3; param: pointer): float;
	var
		p: pRayCastParams absolute param;
		args: SceneRoot.RayCastArgs;
	begin
		Assert(@normal = @normal);
		args.reason := p^.reason;
		args.node   := body;
		args.result := yes;
		p^.scene^.onRayCast.Call(@_CallOnRayCast, @args);
		if args.result then
		begin
			p^.result^ := point;
			if Assigned(p^.resultx) then p^.resultx^ := x;
			p^.found := yes;
			result := x;
		end else
			result := 1.0;
	end;

	function SceneRoot.RayCast(reason: RayCastReason; const a, b: Vec3; x: pFloat = nil): Vec3;
	var
		params: tRaycastParams;
	begin
		if (not Assigned(phys)) or (onRayCast.Empty) then exit(b);
		params.reason := reason;
		params.scene  := @self;
		params.result := @result;
		params.resultx := x;
		params.found := no;
		phys^.RayCast(a, b, nil, @_RayCastImpl, @params);
		if not params.found then
		begin
			result := b;
			if Assigned(x) then x^ := 1.0;
		end;
	end;

	procedure __CallOnCreateDestroyOutdoorItem(const info: SingleDelegateInfo; param: pointer);
	var
		args: pOnCreateDestroyOutdoorItemArgs absolute param;
	begin
		with args^ do OnCreateDestroyOutdoorItemProc(info.proc)(item, node, info);
	end;

	procedure SceneRoot._CallOnCreateDestroyOutdoorItem(create: boolean; item: pItem; node: pSceneNode);
	var
		md: pMultiDelegate;
		args: OnCreateDestroyOutdoorItemArgs;
	begin
		if create then md := @onCreateOutdoorItem else md := @onDestroyOutdoorItem;
		Assert(not md^.Empty);
		args.item := item;
		args.node := node;
		md^.Call(@__CallOnCreateDestroyOutdoorItem, @args);
	end;

	function SceneRoot.PlaceOutdoorItem(item: pItem; const wtf: Transform; velocity: pVec3): pOutdoorItem;
	begin
		result := nil;
	{$ifdef Debug}
		if onCreateOutdoorItem.Empty then
		begin
			Log('"' + item^.id + '": не задан обработчик onCreateOutdoor.', logError);
			exit;
		end;
	{$endif}

		result := new(pOutdoorItem, Init(item));
		if not Assigned(result) then
		begin
		{$ifdef Debug} Log('"' + item^.id + '": не удалось создать представление этого предмета в сцене.', logWarning); {$endif}
			exit;
		end;
		result^.GlobalTransform := wtf;
		if Assigned(velocity) then result^.Velocity := velocity^;
		_CallOnCreateDestroyOutdoorItem(yes, item, result);

	{$ifdef Debug}
		if not Assigned(result^.Root) then
		begin
			Log('"' + item^.id + '": onCreateOutdoor не добавил предмет в сцену.', logError);
			PumpRef(result);
		end;
	{$endif}
	end;

	procedure SceneRoot.DestroyOutdoorItem(node: pOutdoorItem);
	begin
	{$ifdef Debug} MakeRef(node); {$endif}
		Assert(Assigned(node) and (node^.Root = pSceneNode(@self)), 'ожидается предмет в сцене');
		_CallOnCreateDestroyOutdoorItem(no, node^.item, node);
		Assert(not Assigned(node^.Root), 'onDestroyOutdoor должна удалить предмет из сцены');
	{$ifdef Debug} Release(node); {$endif}
	end;

{$ifdef use_serialization}
const
	SCENE_MAIN_BIT = 1 shl 0;
	SCENE_PHYS_BIT = 1 shl 1;

	procedure SerializeScene(se: pSerializer; obj: pointer);
	var
		scene: pScene absolute obj;
		flags: uint;
	begin
		Assert(not scene^._neutralized);
		with se^ do
		begin
			flags := 0;
			if scene_Main in scene^._flags then flags := flags or SCENE_MAIN_BIT;
			if scene_Phys in scene^._flags then flags := flags or SCENE_PHYS_BIT;
			Serialize_ui8(stream, flags);

			Assert(not scene^._neutralized);
			if scene_Phys in scene^._flags then SeObject(scene^.phys);
			SeObject(@scene^.ways, TypeOf(Waypoints));
			SeObject(@scene^.camera, TypeOf(Camera));
			SeObject(@scene^.onAttachDetach, ObjType_MultiDelegate);
			SeObject(@scene^.onRayCast, ObjType_MultiDelegate);
			SeObject(@scene^.onCreateOutdoorItem, ObjType_MultiDelegate);
			SeObject(@scene^.onDestroyOutdoorItem, ObjType_MultiDelegate);
		end;
	end;

	procedure DeserializeScene(de: pDeserializer; obj: pointer);
	var
		scene: pScene absolute obj;
		flags: uint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			scene^._flags := [];
			if (flags and SCENE_MAIN_BIT) <> 0 then Include(scene^._flags, scene_Main);
			if (flags and SCENE_PHYS_BIT) <> 0 then Include(scene^._flags, scene_Phys);

			if (flags and SCENE_PHYS_BIT) <> 0 then DeObjectR(scene^.phys);
			DeObjectAtR(scene^.ways);
			DeObjectAtR(scene^.camera);
			DeWeakAtR(scene^.onAttachDetach);
			DeWeakAtR(scene^.onRayCast);
			DeWeakAtR(scene^.onCreateOutdoorItem);
			DeWeakAtR(scene^.onDestroyOutdoorItem);
			scene^._Initialize;
		end;
	end;

	procedure SceneDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		scene: pScene absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: scene^.DeseInit;
			de_After2: scene^._AfterAttach;
		end;
	end;
{$endif}

	procedure Init;
	begin
	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Scene root', TypeOf(SceneRoot), TypeOf(SceneNode), sizeof(SceneRoot), yes,
		               @SerializeScene, @DeserializeScene, nil, @SceneDeSpecial);
	{$endif}
	end;

initialization
	&Unit('Scene').Initialize(@Init);
end.
