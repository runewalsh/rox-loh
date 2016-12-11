unit Script_EngineAPI;

{$include opts.inc}

interface

uses
	USystem, Script, Utils, UClasses, UMath, Streams, SceneGraph, GLBase, GLClasses
{$ifdef Debug}, ULog {$endif};

type
	Config2 = object
	type scoped_enum_
		Classification = (Unknown, Intermediate, Leaf, Got); _end
	const
		Unknown      = Classification.Unknown;
		Intermediate = Classification.Intermediate;
		Leaf         = Classification.Leaf;
		Got          = Classification.Got;
		TEST_IDX     = Low(sint) + 1;
		GET_IDX      = Low(sint);
		function Apply(var ss: ScriptState; const opt: array of StringView; idx: sint): Classification; static;
	end;

	procedure OpenScript(var script: ScriptState);
	procedure Script_handle_assoc(var ss: ScriptState; read: boolean);
	procedure Script_common_create_scene_node(var script: ScriptState; n: pSceneNode);
	procedure Script_modify_gl(var ss: ScriptState; idx: sint; var gl: GLEntityParams);
	function Script_modify_gl_and_also_query(var ss: ScriptState; idx: sint; var gl: GLEntityParams): sint;
	function Script_add_action(var ss: ScriptState; entity: pObject; ac: pEntityAction): sint;
	function Script_Slide(var ss: ScriptState): sint;
	function Script_SlideGL(var ss: ScriptState; entity: pObject; var gl: GLEntityParams): sint;
	function Script_AddTimer(var ss: ScriptState): sint;
	procedure Script_modifiable(var ss: ScriptState; start: sint; var v: ModifiableValue);
	function Script_create_material(var ss: ScriptState; idx: sint): pGLMaterial;
	function Script_dimensional_path_ref(var ss: ScriptState; idx: sint; dim: sint): pDimensionalPath;
	function Script_distribution(var ss: ScriptState; idx: sint): Distribution;

implementation

uses
	MMSystem, U_GL, GLUtils, USkeleton, Scene, Cameras,
	EnvironmentMaps, Lights, RenderLists,
	GUI, SpatialIndex, Physics, Audio, PathFinding, Character, Labyrinth, ProceduralGraphics,
	Inventories, TeamRelations
{$ifdef use_serialization}, Serialization {$endif}
{$ifdef Profile}, Profile {$endif};

	function Config2.Apply(var ss: ScriptState; const opt: array of StringView; idx: sint): Classification;
	var
		applyResult: Classification absolute result;
		lv: sint;
		test: boolean;

		function Descend(const name: string): boolean;
		begin
			result := opt[lv] = name;
			if result then inc(lv);
		end;

		function Last(const name: string): boolean;
		begin
			result := opt[lv] = name;
			if result then
			begin
				applyResult := Classification.Leaf;
				result := lv = High(opt);
			end;
		end;

		function Final: boolean;
		begin
			result := lv >= length(opt);
			if result then applyResult := Intermediate;
		end;

		function Get: boolean;
		begin
			result := idx = GET_IDX;
			if result then applyResult := Got;
		end;

	begin
		lv := 0;
		test := (idx = GET_IDX) or (idx = TEST_IDX);
		result := Unknown;
		if Final then
		else if Descend('window') then
			if Final then
			else if Last('pos') then        if test then else mm.window.SetPos(ss.ToVec2(idx))
			else if Last('sizes') then      if test then else mm.window.SetSizes(ss.ToVec2(idx))
			else if Last('centered') then   if test then else mm.window.Centered      := ss.ToBool(idx)
			else if Last('fullscreen') then if test then else mm.window.Fullscreen    := ss.ToBool(idx)
			else if Last('forwardGL') then  if test then else mm.window.ForwardGL     := ss.ToBool(idx)
			else if Last('debugGL') then    if test then else {$ifdef Debug} mm.window.DebugGL   := ss.ToBool(idx) {$endif}
			else if Last('VSync') then      if test then else mm.window.VSync         := ss.ToBool(idx)
			else if Last('AA') then         if test then else mm.window.msaaSamples   := ss.ToSint(idx)
			else if Last('fpsLimit') then   if test then else mm.FPSLimit             := ss.ToFloat(idx)
			else if Last('allowMT') then    if test then else MMSystem.Config.allowMT := ss.ToTribool(idx)
			else
		else if Descend('GL') then
			if Final then
			else if Last('textureAnisotropy') then if test then else GLBase.Config.textureAnisotropy := ss.ToFloat(idx)
			else if Last('allowAdvFloats') then    if test then else GLBase.Config.allowAdvFloats    := ss.ToTribool(idx)
			else if Last('allowUbyteIndices') then if test then else GLBase.Config.allowUbyteIndices := ss.ToTribool(idx)
			else if Last('vaAlignment') then    if test then else GLBase.Config.vaAlignment       := ss.ToSint(idx)
			else if Last('csmLogK') then        if test then else GLBase.Config.csmLogK           := clamp(ss.ToFloat(idx), 0.0, 1.0)
			else if Last('zNear') then          if test then else GLBase.Config.zNear             := ss.ToFloat(idx)
			else if Last('abyssColor') then     if test then else GLBase.Config.abyssColor        := ss.ToVec4(idx)
			else if Last('maxZFar') then        if test then else GLBase.Config.maxZFar           := max(ss.ToFloat(idx), 10.0 * GLBase.Config.zNear)
			else if Last('allowMT') then        if test then else GLBase.Config.allowMT           := ss.ToBool(idx)
			else if Last('forceMTfail') then    if test then else GLBase.Config.forceMTfail       := ss.ToBool(idx)
			else if Last('allowInsta') then     if test then else GLBase.Config.allowInstancing   := ss.ToBool(idx)
			else if Last('allowUbo') then       if test then else GLBase.Config.allowUbo          := ss.ToBool(idx)
			else if Last('allowGS') then        if test then else GLBase.Config.allowGeometryShader := ss.ToBool(idx)
			else if Last('allowPrimitiveRestart') then if test then else GLBase.Config.allowPrimitiveRestart := ss.ToBool(idx)
			else if Last('allowDSA') then              if test then else GLBase.Config.allowDSA              := ss.ToBool(idx)
			else if Last('allowBinaryShaders') then    if test then else GLBase.Config.allowBinaryShaders    := ss.ToBool(idx)
			else if Last('nCsmSplits') then     if test then else GLBase.Config.nCsmSplits        := clamp(ss.ToSint(idx), 1, MAX_CSM_SPLITS)
			else if Last('mergeLights') then    if Get then ss.PushBool(GLBase.Config.mergeLights) else if test then else GLBase.Config.mergeLights := ss.ToBool(idx)
			else if Last('keepZeroUniFor') then if test then else GLBase.Config.keepZeroUniFor    := ss.ToBool(idx)
			else if Last('keepOneInstance') then if test then else GLBase.Config.keepOneInstance  := ss.ToBool(idx)
			else if Last('forceGLver') then     if test then else GLBase.Config.forceGLver        := ss.ToString(idx)
			else if Last('forceSLver') then     if test then else GLBase.Config.forceSLver        := ss.ToString(idx)
			else
		else if Descend('phys') then
			if Final then
			else if Last('idclip') then        if Get then ss.PushBool(Physics.Config.idclip) else if test then else Physics.Config.idclip := ss.ToBool(idx)
			else if Last('solverModel') then   if test then else Physics.Config.solverModel   := ss.ToSint(idx)
			else if Last('frictionModel') then if test then else Physics.Config.frictionModel := ss.ToSint(idx)
			else if Last('fpext') then         if test then else Physics.Config.fpext         := ss.ToSint(idx)
			else if Last('nThreads') then      if test then else Physics.Config.nThreads      := ss.ToSint(idx)
			else
		else if Descend('audio') then
			if Final then
			else if Last('device') then if test then else Audio.Config.device := ss.ToSint(idx)
			else
		else if Descend('script') then
			if Final then
			else if Last('entry') then if test then else mm.scriptEntry := ss.ToString(idx)
			else if Descend('gc') then
				if Final then
				else if Last('pause') then if test then else ss.GC.SetPause(ss.ToSint(idx))
				else if Last('stepMul') then if test then else ss.GC.SetStepMul(ss.ToSint(idx))
				else
			else if Last('assertions') then if test then else ss.Assertions := ss.ToBool(idx)
			else
		else if Descend('serialization') then
			if Final then
			else
		{$ifdef use_serialization}
			if Last('verbose') then if test then else Serialization.Config.verbose := ss.ToBool(-1)
			else if Last('previewSize') then if test then else MMSystem.Config.previewSize := ss.ToUintVec2(-1)
			else
		{$endif}
		else if Descend('gamepad') then
			if Final then
			else if Last('buttons') then if test then else
			begin
				ss.PushNil;
				while ss.Next(ss.AdjustIdx(idx, 1)) do
				begin
					if ss.Typ(-1) = script_Table then
					begin                    // start_id name_list
						ss.PushNil;           // start_id name_list name_k
						while ss.Next(-2) do
						begin                 // start_id name_list name_k name_v
							mm.gamepad.MapButton(ss.ToSint(-4) + ss.ToSint(-2) - 1, ss.ToString(-1));
							ss.Pop;
						end;
					end else                 // id name
						mm.gamepad.MapButton(ss.ToSint(-2), ss.ToString(-1));
					ss.Pop;
				end;
			end
			else
		else if Last('language') then if test then else mm.locale.Language := ss.ToString(idx)
		else;
	end;

	function Script_Memory(var ss: ScriptState): sint;
	var
		s: string;
		heap: tFPCHeapStatus;
		r: hp_float;
	begin
		s := ss.ToString(1);
		if s = 'System'  then
		begin
			heap := GetFPCHeapStatus;
			if heap.CurrHeapUsed = heap.CurrHeapSize then r := heap.CurrHeapUsed else
			begin
				ss.PushFloat(heap.CurrHeapUsed);
				ss.PushFloat(heap.CurrHeapSize);
				exit(2);
			end;
		end else
		if s = 'Lua'     then r := ss.MemoryEaten else
		if s = 'Newton'  then r := PhysMemoryEaten else
		if s = 'GL'      then r := gl.MemoryUsed else
		if s = 'GL.GPU'  then r := gl.MemoryUsed(GLmem_GPU) else
		if s = 'GL.aux'  then r := gl.MemoryUsed(GLmem_Aux) else
			ss.UnknownIdentifier(ss.ToString(1));
		ss.PushFloat(r);
		result := 1;
	end;

	procedure Script_System(var ss: ScriptState);
	var
		s: string;
	begin
		s := ss.ToString(1);
		ss.PushNil;
		if s = 'nCPUs' then ss.PushSint(SystemInfo.nCPUs) else
		if s = 'ExecVersion' then ss.PushStringOrNil(GetExecVersion()) else
		if s = 'ExecCodeName' then ss.PushStringOrNil(GetExecVersion(CodeName))
	{$ifdef Debug} else
		if s = 'GL.changes/f' then ss.PushFloat(gl.stateChangesDynamics.AveragePerFrame) else
		if s = 'GL.batches/f' then ss.PushFloat(gl.drawCallsDynamics.AveragePerFrame) else
		if s = 'GL.tris/f' then ss.PushFloat(gl.trisDynamics.AveragePerFrame) else
		if s = 'GL.sent/f' then ss.PushFloat(gl.sendedDynamics.AveragePerFrame) else
			ss.UnknownIdentifier(s)
	{$endif};
	end;

	procedure Script_VisualizeKD(var ss: ScriptState);
	var
		p: string;
		sc: pScene;
		b: pGLBatch;
	begin
		sc := ss.ToSelf;
		p := ss.ToString(2);
		b := pRenderObject(ss.ToObject(3, TypeOf(RenderObject)))^.AnyBatch;
		if not Assigned(b) then exit;

		if p = 'scene' then VisualizeKD(no, sc^.GetKd, b) else
		if p = 'sceneO' then VisualizeKD(yes, sc^.GetKd, b) else
		if p = 'ways' then VisualizeWaypointsKD(sc^.ways.GetKD, b) else
			ss.UnknownIdentifier(p);
	end;

	procedure Script_VisualizeSkeleton(var ss: ScriptState);
	begin
		VisualizeSkeleton(pSkeletonNode(ss.ToSelf), pRenderObject(ss.ToObject(2, TypeOf(RenderObject)))^.AnyBatch);
	end;

	procedure Script_VisualizeWaypoints(var ss: ScriptState);
	var
		s: string;
		edges: boolean;
	begin
		s := ss.ToString(2);
		if s = 'edges' then edges := yes else
		if s = 'points' then edges := no
			else ss.Throw('WTF: {0}', s);
		VisualizeWaypoints(pWaypoints(ss.ToObject(1, TypeOf(Waypoints))), pRenderObject(ss.ToObject(3, TypeOf(RenderObject)))^.AnyBatch, edges);
	end;

	procedure Script_VisualizeWay(var ss: ScriptState);
	var
		o: pObject;
		batch: pGLBatch;
	begin
		o := ss.ToSelf;
		batch := pRenderObject(ss.ToObject(2, TypeOf(RenderObject)))^.AnyBatch;

		if TypeOf(o^) = TypeOf(Doll) then VisualizeWay(pDoll(o)^.way, batch)
			else ss.Throw('нужна кукла');
	end;

	procedure Script_CreatePostprocess(var ss: ScriptState);
	var
		pp: pPostprocess;
	begin
		pp := ResourcePool.Shared^.LoadRef(TypeOf(Postprocess), ss.ToStream(1));
		ss.PushObject(pp);
		Release(pp);
	end;

{$ifdef Debug}
	procedure Script_Postprocess_Dump(var ss: ScriptState);
	begin
		pPostprocess(ss.ToSelf)^.Dump(ss.ToString(2));
	end;
{$endif}

	function Script_modify_gl_and_also_query(var ss: ScriptState; idx: sint; var gl: GLEntityParams): sint;

		procedure setdefine(const name: PoolString; value: boolean);
		var
			def: pShaderDefine;
		begin
			def := ShaderDefines^.Find(name);
			if not Assigned(def) then ss.Throw('шейдерный дефайн {0} не найден', name);
			case def^.kind of
				Flag: gl.flags[def^.id] := value;
				Special: gl.flags.special[def^.id] := value;
				else Assert(no);
			end;
		end;

		function GuessGLType(idx: sint): sint;
		var
			vl: sint;
		begin
			case ss.Typ(idx) of
				script_Number: result := ord(GLType.Float);
				script_Table, script_Pointer:
					begin
						vl := ss.VecLen(-1) - 2;
						if vl >= 0 then
						begin
							Assert(vl <= 2);
							result := ord(GLType.Vec2) + vl;
						end else
							if ss.ObjType(-1) = TypeOf(tTexture) then result := ord(GLType.Sampler) else
								ss.Throw('странный тип');
					end;
				script_String: result := ord(GLType.Sampler);
				else ss.Throw('странный объект, ожидается вектор или текстура');
			end;
		end;

	var
		name: PoolString;
		def: pShaderDefine;
		v: pNativeGLValue;
		tex: pTexture;
		gt: sint;
	begin
	trace_call('Script_modify_gl_and_also_query');
		result := 0;
		idx := ss.AbsIdx(idx);
		case ss.Typ(idx) of
			script_Table:
				begin
					ss.PushNil;
					while ss.Next(idx) do
					begin
						if ss.Typ(-2) = script_String then
						begin
							name := ss.ToString(-2);
							if ss.Typ(-1) = script_Boolean then
								if name.internal[1] in ['A' .. 'Z'] then
									setdefine(name, ss.ToBool(-1))
								else
								begin
									Assert(not ss.ToBool(-1), 'Implying you want to remove value; so use "no"');
									gl.values.Remove(name);
									v := nil;
								end
							else
							begin
								v := gl.values.Value(name);
								if Assigned(v) then
								begin
								{$ifdef Debug}
									gt := GuessGLType(-1);
									if ord(v^.type_) <> gt then
									begin
										if gt >= 0 then ss.Throw('тип GL-значения ({0}) выглядит как {1}, но оно уже существует под типом {2}', v^.namae, GLTypeIds[GLType(gt)], GLTypeIds[v^.type_]);
										v := nil;
									end;
								{$endif}
								end else
								begin
									gt := GuessGLType(-1);
									if gt >= 0 then v := gl.values.Value(name, GLType(gt));
								end;

								if Assigned(v) then
									case v^.type_ of
										GLType.Float: v^.SetFloat(ss.ToFloat(-1));
										GLType.Vec3: v^.SetVec3(ss.ToVec3(-1));
										GLType.Vec4: v^.SetVec4(ss.ToVec4(-1));
										GLType.Vec2: v^.SetVec2(ss.ToVec2(-1));
										GLType.Sampler:
											begin
												if ss.Typ(-1) = script_String then
													tex := ResourcePool.Shared^.LoadRef(TypeOf(tTexture), ss.ToStream(-1))
												else
													tex := MakeRef(ss.ToObject(-1, TypeOf(tTexture)));
												v^.SetTex(tex);
												Release(tex);
											end;
										else Assert(no);
									end;
							end;
						end else
							setdefine(ss.ToString(-1), yes);

						ss.Pop;
					end;
				end;
			script_String:
				begin
					name := ss.ToString(idx);
					def := ShaderDefines^.Find(name);
					if Assigned(def) then
					begin
						result := 1;
						case def^.kind of
							Flag: ss.PushBool(gl.flags[def^.id]);
							Special: ss.PushBool(gl.flags.special[def^.id]);
							else Assert(no);
						end;
					end else
					begin
						v := gl.values.Value(name);
						if Assigned(v) then
						begin
							result := 1;
							case v^.Type_ of
								GLType.Float: ss.PushFloat(v^.ToFloat(0));
								GLType.Vec2: ss.PushVec2(v^.ToVec2(0));
								GLType.Vec3: ss.PushVec3(v^.ToVec3(0));
								GLType.Vec4: ss.PushVec4(v^.ToVec4(0));
								else Assert(no, GLTypeIds[v^.Type_]);
							end;
						end else
						begin
							// Запрошенный параметр не найден — штатная ситуация.
							result := 0;
						end;
					end;
				end;
		end;
leave_call
	end;

	procedure Script_modify_gl(var ss: ScriptState; idx: sint; var gl: GLEntityParams);
	begin
		ss.Pop(Script_modify_gl_and_also_query(ss, idx, gl));
	end;

	procedure Script_modifiable(var ss: ScriptState; start: sint; var v: ModifiableValue);
	var
		name: string;
	begin
		name := ss.ToString(start);
		if ss.Typ(start + 1) = script_String then
		begin
			v.SetModifier(name, ModifiableValue.OpEnum(FindStr(ss.ToString(start + 1), ModifiableValue.OpIds, ord(op_Set))),
			              ss.ToFloat(start + 2), ss.ToSint(start + 3, 0));
		end else
			v.RemoveModifier(name);
	end;

	function Script_Save(var ss: ScriptState): sint;
	var
		o: pObject;
		ot: pointer;
		f: pStream;
	begin
		result := 0;
		o := ss.ToSelf;
		ot := ss.ObjType(1);
		f := MakeRef(GetStream(ss.ToStream(2), [file_Write]));
		if not Assigned(f) then exit;
		if ot = TypeOf(RigidPrimitive) then pRigidPrimitive(o)^.Serialize(f) else
		if ot = TypeOf(RigidBody) then pRigidBody(o)^.primitive^.Serialize(f) else
		if ot = TypeOf(Waypoints) then pWaypoints(o)^.Serialize(f)
	{$ifdef Debug} else Log('Нельзя сохранить этот объект', logError) {$endif};
		Release(f);
	end;

	procedure Script_CreateMaterial(var ss: ScriptState);
	begin
		ss.PushObject(Script_create_material(ss, 1));
	end;

	procedure Script_CreateSkeletonNode(var ss: ScriptState);
	var
		sks: pSkeletonSource;
		sk: pSkeletonNode;
	begin
		sks := ResourcePool.Shared^.LoadRef(TypeOf(SkeletonSource), ss.ToStream(1));
		try
			sk := new(pSkeletonNode, Init(sks));
		finally
			Release(sks);
		end;

		Script_common_create_scene_node(ss, sk);
		ss.PushObject(sk);
	end;

	procedure Script_SkeletonNode_AttachToBone(var ss: ScriptState);
	var
		sk: pSkeletonNode;
		bone: sint;
		i: sint;
	begin
		sk := ss.ToSelf;
		bone := sk^.GetBoneID(ss.ToString(2));
		if bone >= 0 then
			for i := 3 to ss.Top do
				sk^.AttachToBone(bone, ss.ToObject(i, TypeOf(SceneNode)));
	end;

	procedure Script_SkeletonNode_IK(var ss: ScriptState);
	var
		skel: pSkeletonNode;
		bone: sint;
		velocity, timeout: float;
	begin
		skel := ss.ToSelf;
		bone := skel^.GetBoneID(ss.GetStringField(2, 'bone'));
		velocity := ss.GetFloatField(2, 'velocity', 1.0);
		timeout := ss.GetFloatFIeld(2, 'timeout');

		if ss.GetTableS(2, 'node') then
			skel^.SetIK(bone, ss.ToObject(-1, TypeOf(SceneNode)), ss.GetTransformField(2, 'nodeTf'), velocity, timeout)
		else
			skel^.SetIK(bone, ss.GetVec3Field(-1, 'target'), velocity, timeout);
	end;

	procedure Script_CreateRTTOperator(var ss: ScriptState);
	var
		size: array of float;
		sizeMode: array of RTTOperatorSizeMode;
		newFlags: RTTOperatorFlags;
		rs: pRenderScenario;
		rop: pRTTOperator;
		bind: pObject;
		bindv: pGLEntityParams;
		i: sint;
	begin
		rs := nil;
		size := nil;
		sizeMode := nil;
		rs := FindRenderScenario(ss.GetStringField(1, 'scenario', 'main'));
		if ss.GetTableS(1, 'size') then
		begin
			case ss.Typ(-1) of
				script_Number:
					begin
						SetLength(size, 1);
						size[0] := ss.ToFloat(-1);
					end;
				script_Pointer:
					if ss.ObjType(-1) = ObjType_Vec[2] then
					begin
						SetLength(size, length(size) + 2);
						size[High(size) - 1] := ss.ToVec2(-1).x;
						size[High(size)]     := ss.ToVec2(-1).y;
					end;
				script_Table:
					for i := 1 to ss.RawLen(-1) do
					case ss.FieldType(-1, i) of
							script_Number:
								begin
									SetLength(size, length(size) + 1);
									size[High(size)] := ss.GetFloatField(-1, i);
								end;
							script_String:
								begin
									SetLength(sizeMode, length(sizeMode) + 1);
									sizeMode[High(sizeMode)] := RTTOperatorSizeMode(FindStr(ss.GetStringField(-1, i), RTTOpSizeModeIds, ord(rtop_PredefSize)));
								end;
						end;
			end;
			ss.Pop;
		end;

		newFlags := [];
		if rs = @MainScenario then Include(newFlags, rtop_Endless);
		if ss.GetBoolField(1, 'main_camera') then newFlags += [rtop_MainCamera];
		if ss.GetBoolField(1, 'clip_plane') then newFlags += [rtop_ClipPlane];
		if ss.GetBoolField(1, 'reflect_plane') then newFlags += [rtop_ReflectPlane];

		rop := new(pRTTOperator, Init(ss.GetStringField(1, 'name'), rs,
			GLTextureTarget(FindStr(ss.GetStringField( 1, 'target', GLTextureTargetIds[GLtexture_Cube]), GLTextureTargetIds, ord(GLtexture_Cube))),
			[GLImageFormat(FindStr(ss.GetStringField(1, 'format', GLImageFormatIds[GLformat_RGB]), GLImageFormatIds, ord(GLformat_RGB)))],
			size, sizeMode, newFlags));

		rop^.Quality := ss.GetFloatField(1, 'quality', rop^.Quality);
		rop^.BaseRefreshPeriod := ss.GetFloatField(1, 'refresh_period', rop^.BaseRefreshPeriod);
		if ss.GetTableS(1, 'bind') then
		begin
			if ss.GetTableS(-1, 'ro') then
			begin
				bind := MakeRef(ss.ToObject(-1, TypeOf(RenderObject)));
				bindv := @pRenderObject(bind)^.gl;
				ss.Pop;
			end else
			if ss.GetTableS(-1, 'gui') then
			begin
				bind := MakeRef(ss.ToObject(-1, TypeOf(Control)));
				bindv := @pControl(bind)^.gl;
				ss.Pop;
			end else
				bind := nil;
			if Assigned(bind) then rop^.Bind(bind, bindv, ss.GetStringField(-1, 'name', 'env'));
			Release(bind);
			ss.Pop;
		end;
		if ss.GetTableS(1, 'plane_point') then
		begin
			rop^.PlanePoint := ss.ToVec3(-1);
			ss.Pop;
		end;
		if ss.GetTableS(1, 'plane_normal') then
		begin
			rop^.PlaneNormal := ss.ToVec3(-1);
			ss.Pop;
		end;
		if ss.GetTableS(1, 'clip_plane_shift') then
		begin
			rop^.ClipPlaneShift := ss.ToFloat(-1);
			ss.Pop;
		end;
		Script_common_create_scene_node(ss, rop);
		ss.PushObject(rop);
	end;

{$define fname:=Script_Light_color} {$define otype:=Light} {$define field:=color} {$define prop_vec3} {$i script_prop.inc}
{$define fname:=Script_Light_radius} {$define otype:=Light} {$define field:=radius} {$define prop_float} {$i script_prop.inc}
{$define fname:=Script_Light_fadeK} {$define otype:=Light} {$define field:=fadeK} {$define prop_float} {$i script_prop.inc}
{$define fname:=Script_Light_k} {$define otype:=Light} {$define field:=coeff} {$define prop_float} {$i script_prop.inc}
{$define fname:=Script_Light_position} {$define otype:=Light} {$define field:=position} {$define prop_vec3} {$i script_prop.inc}
{$define fname:=Script_VirtualLight_color} {$define otype:=VirtualLight} {$define field:=basecolor} {$define prop_vec3} {$i script_prop.inc}
{$define fname:=Script_VirtualLight_radius} {$define otype:=VirtualLight} {$define field:=baseradius} {$define prop_float} {$i script_prop.inc}
{$define fname:=Script_VirtualLight_fadeK} {$define otype:=VirtualLight} {$define field:=fadeK} {$define prop_float} {$i script_prop.inc}
{$define fname:=Script_VirtualLight_k} {$define otype:=VirtualLight} {$define field:=coeff} {$define prop_float} {$i script_prop.inc}

	procedure Script_CreateLight(var ss: ScriptState);
	var
		n: pSceneNode;
		light: pLight;
		vlight: pVirtualLight;
		kind: tLightBase;
		shadow: sint;
		radius, fadeK: float;
		color: Vec3;
	begin
		if ss.Top = 0 then ss.PushTable;
		kind := tLightBase(FindStr(ss.GetStringField(1, 'kind', LightBaseIds[light_Omni]), LightBaseIds, ord(light_Omni)));
		shadow := ss.GetSintField(1, 'shadow');
		radius := ss.GetFloatField(1, 'radius', -1);
		color := ss.GetVec3Field(1, 'color', Vec3.MinusOnes);
		fadeK := ss.GetFloatField(1, 'fadeK', -1);

		if ss.GetBoolField(1, 'virtual') then
		begin
			vlight := new(pVirtualLight, Init(kind, shadow));
			if radius <> -1 then vlight^.BaseRadius := radius;
			if color.x <> -1 then vlight^.BaseColor := color;
			if fadeK <> -1 then vlight^.FadeK := fadeK;
			n := vlight;
		end else
		begin
			light := new(pLight, Init(kind, shadow));
			if radius <> -1 then light^.radius := radius;
			if color.x <> -1 then light^.color := color;
			if fadeK <> -1 then light^.FadeK := fadeK;
			n := light;
		end;
		Script_common_create_scene_node(ss, n);
		ss.PushObject(n);
	end;

	procedure Script_SceneNode_tf(var ss: ScriptState; read: boolean);
	begin
		if read then
			ss.PushTransform(pSceneNode(ss.ToSelf)^.LocalTransform)
		else
			if ss.ToBool(3) then
				pSceneNode(ss.ToSelf)^.LocalTransform := ss.ToTransform(3);
	end;

	procedure Script_SceneNode_wtf(var ss: ScriptState; read: boolean);
	begin
		if read then
			ss.PushTransform(pSceneNode(ss.ToSelf)^.globalTransform)
		else
			if ss.ToBool(3) then
				pSceneNode(ss.ToSelf)^.globalTransform := ss.ToTransform(3);
	end;

	procedure Script_SceneNode_pos(var ss: ScriptState; read: boolean);
	begin
		if read then
			ss.PushVec3(pSceneNode(ss.ToSelf)^.LocalPos)
		else
			if ss.ToBool(3) then
				pSceneNode(ss.ToSelf)^.LocalPos := ss.ToVec3(3);
	end;

	procedure Script_SceneNode_wpos(var ss: ScriptState);
	begin
		ss.PushVec3(pSceneNode(ss.ToSelf)^.WorldPos);
	end;

	procedure Script_SceneNode_rot(var ss: ScriptState; read: boolean);
	begin
		if read then
			ss.PushQuaternion(pSceneNode(ss.ToSelf)^.LocalRot)
		else
			if ss.ToBool(3) then
				pSceneNode(ss.ToSelf)^.LocalRot := ss.ToQuaternion(3);
	end;

	procedure Script_SceneNode_wrot(var ss: ScriptState);
	begin
		ss.PushQuaternion(pSceneNode(ss.ToSelf)^.WorldRot);
	end;

	procedure Script_SceneNode_scale(var ss: ScriptState; read: boolean);
	begin
		if read then
			ss.PushFloat(pSceneNode(ss.ToSelf)^.LocalScale)
		else
			if ss.ToBool(3) then
				pSceneNode(ss.ToSelf)^.LocalScale := ss.ToFloat(3);
	end;

	procedure Script_SceneNode_wscale(var ss: ScriptState);
	begin
		ss.PushFloat(pSceneNode(ss.ToSelf)^.WorldScale);
	end;

	procedure Script_handle_assoc(var ss: ScriptState; read: boolean);
	var
		sc: pSceneNode;
	begin
		sc := ss.ToSelf;
		if read then
		begin
			if not ss.GetAssociated(sc) then ss.PushNil;
		end else
			ss.Associate(sc);
	end;

	procedure Script_SceneNode_Attach(var ss: ScriptState);
	var
		i: sint;
	begin
		for i := 2 to ss.Top do
			if ss.Typ(i) <> script_Nil then
				pSceneNode(ss.ToSelf)^.Attach(pSceneNode(ss.ToObject(i, TypeOf(SceneNode))));
	end;

	procedure Script_SceneNode_Detach(var ss: ScriptState);
	begin
		pSceneNode(ss.ToSelf)^.Detach;
	end;

	function Script_SceneNode_GetLocalAABB(var ss: ScriptState): sint;
	var
		n: pSceneNode;
	begin
		n := ss.ToSelf;
		if not Assigned(n^.StartBnd) then exit(0);
		ss.PushVec3(n^.StartBnd^.AABB.A);
		ss.PushVec3(n^.StartBnd^.AABB.B);
		result := 2;
	end;

	function EntityAction_OnProcess(ac: pEntityAction; const dt: float; const info: SingleDelegateInfo): boolean;
	var
		sd: pScriptDelegate absolute info.user;
	begin
		if not sd^.GetFunction {$ifdef Debug}('[EntityAction] OnProcess'){$endif} then exit(yes);
		with sd^.ss^ do
		begin
			PushObject(ac);
			PushFloat(dt);
			Call(2, 1);
			result := (Typ(-1) <> script_Boolean) or (ToBool(-1));
			Pop;
		end;
	end;

	procedure EntityAction_OnDone(ac: pEntityAction; reason: EntityActionStopReason; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(EntityAction.OnProcessProc(@EntityAction_OnProcess) = @EntityAction_OnProcess);
		Assert(EntityAction.OnDoneProc(@EntityAction_OnDone) = @EntityAction_OnDone);
		if not sd^.GetFunction {$ifdef Debug}('[EntityAction] OnDone'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(ac);
			PushString(StopReasonIds[reason]);
			Call(2, 0);
		end;
	end;

	procedure SceneNodeTimer_OnTimer(ac: pSceneNodeTimer; const dt: float; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
	trace_call('Script.Node.OnTimer');
		Assert(SceneNodeTimer.OnTimerProc(@SceneNodeTimer_OnTimer) = @SceneNodeTimer_OnTimer);
		if not sd^.GetFunction {$ifdef Debug}('[EntityAction] OnTimer'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(ac);
			PushFloat(dt);
			Call(2, 0);
		end;
	leave_call
	end;

	function Script_add_action(var ss: ScriptState; entity: pObject; ac: pEntityAction): sint;
	begin
		if not Assigned(ac) then exit(0);
		if InheritsFrom(TypeOf(entity^), TypeOf(SceneNode)) then pSceneNode(entity)^.AddAction(ac) else
		if InheritsFrom(TypeOf(entity^), TypeOf(Control)) then pControl(entity)^.AddAction(ac)
			else ss.Throw('допускается добавление действия к узлу сцены или GUI');
		ss.PushObject(ac);
		result := 1;

		if ss.GetTableS(2, 'onProcess') then
			ss.SetDelegate(entity, @ac^.onProcess, @EntityAction_OnProcess, '');
		if ss.GetTableS(2, 'onDone') then
			ss.SetDelegate(entity, @ac^.onDone, @EntityAction_OnDone, '');
	end;

	function Script_Slide(var ss: ScriptState): sint;
	var
		entity: pObject;
		path: pDimensionalPath;
		ac: pEntityAction;
	begin
		entity := ss.ToSelf;
		if ss.GetTableS(2, 'path') then
		begin
			path := Script_dimensional_path_ref(ss, -1, 0);
			ss.Pop;
		end else
			path := nil;

		ac := new(pSlide, Init(path, ss.GetStringField(2, 'id')));
		Release(path);
		result := Script_add_action(ss, entity, ac);
	end;

	function Script_SlideGL(var ss: ScriptState; entity: pObject; var gl: GLEntityParams): sint;
	var
		what: string;
		v: pNativeGLValue;
		path: pDimensionalPath;
		ac: pEntityAction;
	begin
		result := 0;
		what := ss.GetStringField(2, 'what');
		v := gl.values.Value(what);
		if not Assigned(v) then ss.Throw('параметр {0} не найден', what);

		if ss.GetTableS(2, 'path') then
		begin
			path := Script_dimensional_path_ref(ss, -1, GLTypeInfo[v^.Type_].baseDim);
			ss.Pop;
		end else
			path := nil;

		ac := new(pSlideGL, Init(@gl, what, path));
		Release(path);
		result := Script_add_action(ss, entity, ac);
	end;

	function Script_SceneNode_Move(var ss: ScriptState): sint;
	var
		node: pSceneNode;
		ac: pEntityAction;
		path: pDimensionalPath;
	begin
		node := pSceneNode(ss.ToSelf);
		if ss.GetTableS(2, 'path') then
		begin
			path := Script_dimensional_path_ref(ss, -1, 3);
			ss.Pop;
		end else
			ss.Throw('не задан path');
		ac := new(pSceneNodeMove, Init(path));
		Release(path);
		result := Script_add_action(ss, node, ac);
	end;

	function Script_SceneNode_Rotate(var ss: ScriptState): sint;
	var
		node: pSceneNode;
		ac: pEntityAction;
		path: pDimensionalPath;
	begin
		node := pSceneNode(ss.ToSelf);
		if ss.GetTableS(2, 'path') then
		begin
			path := Script_dimensional_path_ref(ss, -1, 1);
			ss.Pop;
		end else
			path := nil;
		ac := new(pSceneNodeRotate, Init(node, ss.GetQuaternionField(2, 'target'), path));
		Release(path);
		result := Script_add_action(ss, node, ac);
	end;

	function Script_AddTimer(var ss: ScriptState): sint;
	var
		entity: pObject;
		ac: pSceneNodeTimer;
	begin
		entity := ss.ToSelf;
		ac := new(pSceneNodeTimer, Init(ss.GetFloatField(2, 'period')));
		ac^.Single := ss.GetBoolField(2, 'single');
		ac^.Now := ss.GetBoolField(2, 'now');
		if ss.GetTableS(2, 'onTimer') then
			ss.SetDelegate(entity, @ac^.onTimer, @SceneNodeTimer_OnTimer, '')
		else
			ss.Throw('для таймера нужен onTimer');

		result := Script_add_action(ss, entity, ac);
	end;

	procedure OnSceneNodeUpdate(node: pSceneNode; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
	trace_call('Script.Node.OnUpdate');
		Assert(SceneNode.OnUpdateProc(@OnSceneNodeUpdate) = @OnSceneNodeUpdate);
		if not sd^.GetFunction {$ifdef Debug}('[SceneNode] OnUpdate'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(node);
			PushFloat(mm.FrameDt);
			Call(2, 0);
		end;
	leave_call
	end;

	procedure Script_SceneNode_onUpdate(var ss: ScriptState);
	var
		node: pSceneNode;
	begin
		node := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(node, @node^.onUpdate, @OnSceneNodeUpdate)));
	end;

	procedure Script_EntityAction_Stop(var ss: ScriptState);
	begin
		pEntityAction(ss.ToSelf)^.Stop(reason_Abort);
	end;

	procedure Script_SceneNodeTimer_Now(var ss: ScriptState);
	begin
		pSceneNodeTimer(ss.ToSelf)^.Now := yes;
	end;

	procedure Script_SceneNodeTimer_period(var ss: ScriptState; read: boolean);
	var
		timer: pSceneNodeTimer;
	begin
		timer := ss.ToSelf;
		if read then
			ss.PushFloat(timer^.Period)
		else
			timer^.Period := ss.ToFloat(3);
	end;

	procedure Script_Slide_current(var ss: ScriptState);
	var
		slide: pSlide;
	begin
		slide := ss.ToSelf;
		case slide^.dm.path^.Dimensions of
			1: ss.PushFloat(slide^.dm.CurrentF);
			2: ss.PushVec2(slide^.dm.CurrentV2);
			3: ss.PushVec3(slide^.dm.CurrentV3);
			4: ss.PushVec4(slide^.dm.CurrentV4);
			else Assert(no);
		end;
	end;

	function Script_RenderObject_GL(var ss: ScriptState): sint;
	begin
		result := Script_modify_gl_and_also_query(ss, 2, pRenderObject(ss.ToSelf)^.gl);
	end;

	function Script_RenderObject_SlideGL(var ss: ScriptState): sint;
	var
		ro: pRenderObject;
	begin
		ro := ss.ToSelf;
		result := Script_SlideGL(ss, ro, ro^.gl);
	end;

	procedure Script_CreateRenderObject(var ss: ScriptState);
		function CreateDynamicMesh(var ss: ScriptState; idx: sint): pGLMesh;
		const
			KnownAttrs: array[2 .. 2] of record
				name: string;
				typ: GLType;
			end =
			(
				(name: 'pos'; typ: GLType.Vec3)
			);
		var
			m: pMesh;
			i: sint;
		begin
			m := new(pMesh, Init({$ifdef Debug} ss.GetStringField(idx, 'name', '---') {$endif}));
			m^.AddBatch('');
			for i := Low(KnownAttrs) to High(KnownAttrs) do
				if ss.GetTableS(idx, KnownAttrs[i].name) then
				begin
					m^.batches[0].AddVA(KnownAttrs[i].name, KnownAttrs[i].typ);
					ss.Pop;
				end;
			result := new(pGLMesh, Init(m, yes));
			if ss.GetTableS(idx, 'prim') then
			begin
				result^.topology := GLTopology(FindStr(ss.ToString(-1), GLTopologyIds, ord(GLtopology_Tris)));
				ss.Pop;
			end;
		end;
	var
		mesh: pGLMesh;
		mats: array of pGLMaterial;
		batchID: sint;
		flags: RenderObjectFlags;
		ro: pRenderObject;
		rof: RenderObjectFlag;
	var
		i: sint;
	begin
		flags := [];

		mesh := nil;
		if ss.GetTableS(1, 'mesh') then
		begin
			case ss.Typ(-1) of
				script_Table: mesh := MakeRef(CreateDynamicMesh(ss, -1));
				script_String: mesh := ResourcePool.Shared^.LoadRef(TypeOf(tGLMesh), ss.ToStream(-1));
				script_Object: mesh := MakeRef(ss.ToObject(-1, TypeOf(tGLMesh)));
				else ss.Throw('на месте mesh принимаются объект, файл или описание динамического меша');
			end;
			ss.Pop;
		end;

		mats := nil;
		if ss.GetTableS(1, 'material') then
		begin
			SetLength(mats, 1);
			mats[0] := MakeRef(Script_create_material(ss, -1));
			ss.Pop;
		end else
		if ss.GetTableS(1, 'materials') then
		begin
			SetLength(mats, length(mesh^.batches));
			for i := 0 to High(mats) do mats[i] := nil;
			ss.PushNil;
			while ss.Next(-2) do
			begin
				case ss.Typ(-2) of
					script_Number: batchID := ss.ToSint(-2) - 1;
					script_String: batchID := mesh^.mesh^.GetBatchID(ss.ToString(-2));
					else batchID := -1;
				end;
				if batchID >= 0 then mats[batchID] := MakeRef(Script_create_material(ss, -1));
				ss.Pop;
			end;
			ss.Pop;
		end else
			mats := nil;

		for rof in RenderObjectFlag do
			if ss.GetBoolField(1, RenderObjectFlagIds[rof]) then
				Include(flags, rof)
			else
				Exclude(flags, rof);

		ro := new(pRenderObject, Init(mesh, mats, flags));
		Release(mesh);
		for i := 0 to High(mats) do Release(mats[i]);

		Script_common_create_scene_node(ss, ro);
		ss.PushObject(ro);
		if not Assigned(ro) then exit;
		if ss.GetTableS(1, 'gl') then
		begin
			Script_modify_gl(ss, -1, ro^.gl);
			ss.Pop;
		end;
	end;

	function Script_Config(var ss: ScriptState): sint;
	begin
		result := ord(ss.Top = 1);
		if Config2.Apply(ss, SplitIntoViews(ss.ToString(1), ['.']), IfThen(result = 1, Config2.GET_IDX, 2))
			<> Config2.Classification(IfThen(result = 1, ord(Config2.Got), ord(Config2.Leaf)))
		then
			ss.Throw('Неизвестная опция {0}.', ss.ToString(1));
	end;

	procedure Script_common_create_scene_node(var script: ScriptState; n: pSceneNode);
	begin
		if not script.IsTable(1) then exit;
		if script.GetTableS(1, 'parent') then
		begin
			pSceneNode(script.ToObject(-1, TypeOf(SceneNode)))^.Attach(n);
			script.Pop;
		end;
		if script.GetTableS(1, 'childs') then
		begin
			script.PushNil;
			while script.Next(-2) do
			begin
				n^.Attach(pSceneNode(script.ToObject(-1, TypeOf(SceneNode))));
				script.Pop;
			end;
			script.Pop;
		end;
		if script.GetTableS(1, 'pos') then
		begin
			n^.LocalPos := script.ToVec3(-1);
			script.Pop;
		end;
		if script.GetTableS(1, 'rot') then
		begin
			n^.LocalRot := script.ToQuaternion(-1);
			script.Pop;
		end;
	end;

	procedure Script_CreateSceneNode(var ss: ScriptState);
	var
		s: pSceneNode;
		i: sint;
	begin
		s := new(pSceneNode, Init);
		for i := 1 to ss.Top do
			if ss.Typ(i) = script_Object then
				s^.Attach(pSceneNode(ss.ToObject(i, TypeOf(SceneNode))));
		Script_common_create_scene_node(ss, s);
		ss.PushObject(s);
	end;

	procedure OnAttachDetachNode(scene: pScene; node: pSceneNode; attach: boolean; const info: SingleDelegateInfo);
		function clear(node: pSceneNode): boolean;
		begin
			result := node^.Serializable and (not node^.Static) and ((not Assigned(node^.parent)) or clear(node^.parent));
		end;
	var
		ss: pScriptState absolute info.user;
		ok: boolean;
	begin
		Assert(SceneRoot.OnAttachDetachProc(@OnAttachDetachNode) = @OnAttachDetachNode);
		Assert(@scene = @scene);
		if not clear(node) then exit;

		if not ss^.GetAssociated(scene) then
		begin
			Assert(no, 'потерялась scene.assoc');
			exit;
		end;
		if attach then
		begin
			ok := ss^.HasUV(node);
			if ok then
			begin
				ss^.PushObject(node);
				if not node^.Serializable then ss^.DontSerialize(node);
			end;
		end else
			ok := ss^.PushExisting(node);
		if ok then
		begin
			if attach then ss^.PushBool(yes) else ss^.PushNil;
			ss^.SetTable(-3);
		end;
		ss^.Pop;
	end;

	procedure Script_CreateScene(var ss: ScriptState);
	var
		flag: SceneFlag;
		flags: SceneFlags;
		s: string;
		sc: pScene;
	begin
		flags := [];
		if ss.Top >= 1 then
		begin
			s := ss.ToString(1);
			for flag in SceneFlags do
				if Pos(SceneFlagIds[flag], s) > 0 then Include(flags, flag);
		end;

		sc := new(pScene, Init(flags));
		ss.PushObject(sc);
		ss.PushTable; ss.Associate(sc);
		sc^.onAttachDetach.Add(@OnAttachDetachNode, @ss);
		// TODO: внимание, если сцена может существовать дольше скрипта, в котором создана, то понадобится отдельный объект и обработчики OnDestroy
	end;

	procedure OnCreateDestroyOutdoorItem(item: pItem; node: pSceneNode; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(OnCreateDestroyOutdoorItemProc(@OnCreateDestroyOutdoorItem) = @OnCreateDestroyOutdoorItem);
		if not sd^.GetFunction {$ifdef Debug}('Scene.OnCreate/DestroyOutdoorItem'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(item);
			PushObject(node);
			Call(2, 0);
		end;
	end;

	procedure Script_Scene_onCreateOutdoorItem(var ss: ScriptState);
	var
		sc: pScene;
	begin
		sc := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(sc, @sc^.onCreateOutdoorItem, @OnCreateDestroyOutdoorItem)));
	end;

	procedure Script_Scene_onDestroyOutdoorItem(var ss: ScriptState);
	var
		sc: pScene;
	begin
		sc := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(sc, @sc^.onDestroyOutdoorItem, @OnCreateDestroyOutdoorItem)));
	end;

	function OnSceneRayCast(reason: SceneRoot.RayCastReason; node: pSceneNode; const info: SingleDelegateInfo): boolean;
	var
		sd: pScriptDelegate absolute info.user;
	begin
		if not sd^.GetFunction {$ifdef Debug}('Scene.OnRayCast'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushString(SceneRoot.RayCastReasonIds[reason]);
			PushObject(node);
			Call(2, 1);
			result := (Typ(-1) = script_Nil) or ToBool(-1);
			Pop;
		end;
	end;

	procedure Script_Scene_onRayCast(var ss: ScriptState);
	var
		sc: pScene;
	begin
		Assert(SceneRoot.RayCastProc(@OnSceneRayCast) = @OnSceneRayCast);
		sc := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(sc, @sc^.onRayCast, @OnSceneRayCast)));
	end;

	procedure Script_Scene_camera(var ss: ScriptState);
	begin
		ss.PushObject(@pScene(ss.ToSelf)^.Camera);
	end;

	procedure Script_Scene_phys(var ss: ScriptState; read: boolean);
	var
		sc: pScene;
	begin
		sc := ss.ToSelf;
		if read then ss.PushObject(sc^.phys) else SetRef(sc^.phys, ss.ToObject(3, TypeOf(PhysWorld)));
	end;

	procedure Script_Scene_ways(var ss: ScriptState);
	begin
		ss.PushObject(@pScene(ss.ToSelf)^.ways);
	end;

	procedure Script_Scene_nobjs(var ss: ScriptState);
	begin
		ss.PushSint(pScene(ss.ToSelf)^.nobjs);
	end;

type
	pObjectsInVolumeParam = ^tObjectsInVolumeParam;
	tObjectsInVolumeParam = object
		scene: pScene;
		bounding: Bounding;
		ss: pScriptState;
		fiber: ScriptState.pNativeFiber;
	end;

	function YieldSceneNode(const data: pSceneNode; param: pointer): boolean;
	var
		p: pObjectsInVolumeParam absolute param;
	begin
		if p^.ss^.PushExisting(data) then
			result := not (Discarded in p^.fiber^.Yield(1))
		else
			result := yes;
	end;

	procedure TraverseObjectsInVolume(var fiber: ScriptState.NativeFiber; var ss: ScriptState; param: pointer);
	var
		p: pObjectsInVolumeParam absolute param;
	begin
		p^.fiber := @fiber;
		p^.ss    := @ss;
		p^.scene^.Query(p^.bounding, @YieldSceneNode, p);
		dispose(p);
	end;

	procedure Script_Scene_ObjectsInRadius(var ss: ScriptState);
	var
		p: pObjectsInVolumeParam;
	begin
		new(p);
		p^.scene    := ss.ToSelf;
		p^.bounding := Bounding.BySphere(ss.ToVec3(2), ss.ToFloat(3));
		ss.PushFiber('Scene.ObjectsInRadius', @TraverseObjectsInVolume, p);
	end;

{$define cam_prop := {$define otype := Camera} {$include script_prop.inc}}
	function Script_Camera_GL(var ss: ScriptState): sint;
	begin
		result := Script_modify_gl_and_also_query(ss, 2, pCamera(ss.ToSelf)^.gl);
	end;

	procedure Script_Camera_tTarget(var ss: ScriptState; read: boolean);
	var
		cam: pCamera;
	begin
		cam := ss.ToSelf;
		if read then
			ss.PushVec3(cam^.tTarget)
		else
			case ss.Typ(3) of
				script_Pointer:
					begin
						cam^.TTarget := ss.ToVec3(3);
						cam^.TTargetCastShift := Vec3.Zero;
					end;
				script_Table:
					begin
						cam^.tTarget := ss.GetVec3Field(3, 1);
						cam^.TTargetCastShift := ss.GetVec3Field(3, 'cast_shift', cam^.TTargetCastShift);
					end;
			end;
	end;

{$define fname:=Script_Camera_fov} {$define field:=FOV} {$define prop_float} cam_prop
{$define fname:=Script_Camera_pos} {$define field:=Pos} {$define prop_vec3} {$define readonly} cam_prop
{$define fname:=Script_Camera_tPos} {$define field:=tPos} {$define prop_vec3} cam_prop
{$define fname:=Script_Camera_qrot} {$define field:=QRot} {$define prop_quat} cam_prop
{$define fname:=Script_Camera_target} {$define field:=Target} {$define prop_vec3} {$define readonly} cam_prop
{$define fname:=Script_Camera_tTargetCastShift} {$define field:=tTargetCastShift} {$define prop_vec3} cam_prop
{$define fname:=Script_Camera_tZrot} {$define field:=tZRot} {$define prop_float} cam_prop
{$define fname:=Script_Camera_accK} {$define field:=accK} {$define prop_float} cam_prop
{$define fname:=Script_Camera_cinematic} {$define field:=cinematic} {$define prop_bool} cam_prop
{$define fname:=Script_Camera_postprocess} {$define field:=postprocess} {$define prop_object := Postprocess} cam_prop

	procedure Script_Camera_UnProject(var ss: ScriptState);
	var
		v: Vec3;
	begin
		if ss.Top = 2 then
			v := ss.ToVec3(2)
		else
			v := Vec3.Make(ss.ToFloat(2), ss.ToFloat(3), ss.ToFloat(4));
		ss.PushVec3(pCamera(ss.ToSelf)^.UnProject(v));
	end;
{$undef cam_prop}

	function Script_Load(var ss: ScriptState): sint;
	var
		s: pStream;
		ext: string;
	begin
		result := 0;
		s := GetStream(ss.ToStream(1));
		if not Assigned(s) then exit;
		ext := StreamPath.Extension(s^.path);
		if ext = ShaderFilenameExtension then ShaderTemplates.Load(s) else
			ss.Throw('Неизвестное расширение в Load: .{0}', ext);
	end;

	function Script_create_material(var ss: ScriptState; idx: sint): pGLMaterial;
		procedure read_level(idx: sint; var level: GLMaterialLevel);
		var
			pass: pRenderPass;
			prog: pShaderProgram;
		begin
			idx := ss.AbsIdx(idx);
			if ss.GetTableS(idx, 'gl') then
			begin
				Script_modify_gl(ss, -1, level.gl);
				ss.Pop;
			end;
			level.blend := GLBlendMode(FindStr(ss.GetStringField(idx, 'blend', GLBlendModeIds[level.blend]), GLBlendModeIds, ord(level.blend)));
			level.cull := ss.GetBoolField(idx, 'cull', level.cull);

			ss.PushNil;
			while ss.Next(idx) do
			begin
				case ss.Typ(-2) of
					script_String:
						begin
							pass := FindRenderPass(ss.ToString(-2));
							if Assigned(pass) then
							begin
								prog := ResourcePool.Shared^.LoadRef(TypeOf(ShaderProgram), tShader.FileName(ss.ToStream(-1)));
								level.Progs[pass] := prog;
								Release(prog);
							end;
						end;
				end;
				ss.Pop;
			end;
		end;
	type
		tTmpLevelRec = record
			lod: float;
			index: sint;
		end;
		{$define procname:=sort_levels} {$define elem:=tTmpLevelRec} {$define less := _1.lod < _2.lod} {$define openarray} {$include sort.inc}
	var
		mat, base: pGLMaterial;
		lvs: GLMaterial.LevelsList;
		curLod: float;
		tmp_lv: array of tTmpLevelRec;
		i, lv: sint;
	begin
	trace_call('Script_create_material');
		if not ss.IsTable(idx) then
		begin
			case ss.Typ(idx) of
				script_Object: result := ss.ToObject(idx, TypeOf(GLMaterial));
				else ss.Throw('ожидается материал');
			end;
			exit;
		end;

		idx := ss.AbsIdx(idx);
		mat := nil;
		if ss.GetTableS(idx, 'base') then
		begin
			base := MakeRef(Script_create_material(ss, -1));
			ss.Pop;
		end else
			base := nil;
	{$ifdef Debug} if not ss.HasField(idx, 'name') then ss.Throw('безымянный материал :('); {$endif}
		mat := new(pGLMaterial, Init(ss.GetStringField(idx, 'name'), base));
		Release(base);
		if not Assigned(mat) then exit(nil);

		ss.PushTable;
		i := 0;
		tmp_lv := nil;

		ss.PushNil;
		while ss.Next(idx) do
		begin
			case ss.Typ(-2) of
				script_Number: curLod := ss.ToFloat(-2);
				script_Table:
					begin
						Assert(no, 'ты ебанутый?');
						curLod := ss.GetFloatField(-2, 'level', -1.0);
					end
				else
					curLod := -1.0;
			end;
			if curLod >= 0.0 then
			begin
				SetLength(tmp_lv, length(tmp_lv) + 1);
				with tmp_lv[High(tmp_lv)] do
				begin
					lod := curLod;
					index := i;
				end;
				ss.SetTableI(-3, i);
				inc(i);
			end else
				ss.Pop;
		end;
		sort_levels(tmp_lv);

		for i := 0 to High(tmp_lv) do
		begin
			lvs := mat^.FetchLevels(tmp_lv[i].lod);
			ss.GetTableI(-1, tmp_lv[i].index);
			for lv := 0 to High(lvs) do
				read_level(-1, lvs[lv]^);
			ss.Pop;
		end;
		ss.Pop;
		tmp_lv := nil;

		if ss.GetTableS(idx, 'gl') then
		begin
			Script_modify_gl(ss, -1, mat^.gl);
			ss.Pop;
		end;

		result := GLMaterial.Merge(mat);
	leave_call
	end;

	function Script_dimensional_path_ref(var ss: ScriptState; idx: sint; dim: sint): pDimensionalPath;
	type
		vector = array of float;
	var
		defErp: InterpolationMode;
		ext_result: pDimensionalPath absolute result;

		function ndvalue(var ss: ScriptState; idx: sint): vector;
		var
			i, n: sint;
			ot: pointer;
			v: pVec4;
		begin
			result := nil;
			case ss.Typ(idx) of
				script_Number:
					begin
						SetLength(result, 1);
						result[0] := ss.ToFloat(idx);
					end;
				script_Table:
					begin
						idx := ss.AbsIdx(idx);
						ss.GetTableI(idx, 1);
						if ss.IsNumber(-1) then
						begin
							SetLength(result, ss.RawLen(idx));
							result[0] := ss.ToFloat(-1);
							for i := 1 to High(result) do
								result[i] := ss.GetFloatField(idx, 1 + i);
						end else
							result := ndvalue(ss, -1);
						ss.Pop;
					end;
				script_Object:
					begin
						ot := ss.ObjType(idx);
						if ot = ObjType_Vec[3] then n := 3 else
						if ot = ObjType_Vec[2] then n := 2 else
						if ot = ObjType_Vec[4] then n := 4 else
							exit(nil);
						SetLength(result, n);
						v := pVec4(ss.ToData(idx));
						for i := 0 to High(result) do
							result[i] := v^.data[i];
					end;
			end;
			if dim = 0 then
			begin
				Assert(not Assigned(ext_result));
				dim := length(result);
				ext_result := MakeRef(new(pDimensionalPath, Init(dim)));
			end;
		{$ifdef Debug}
			if length(result) <> dim then
			begin
				ss.Throw('несовпадение размерностей: ожидается {0}, получена {1} ({2})', ToString(dim), ToString(length(result)), ss.ToString(idx));
				n := length(result);
				SetLength(result, dim);
				for i := n to High(result) do result[i] := 0.0;
			end;
		{$endif}
		end;

		procedure scan_generic_frame(var ss: ScriptState; idx: sint; const start: float);
		var
			erp_in, erp_out: InterpolationMode;
			v: vector;
		begin
			erp_in  := defErp;
			erp_out := defErp;
			if ss.IsTable(idx) then
			begin
				if ss.GetTableS(idx, 'inout') then
				begin
					erp_in := InterpolationMode(FindStr(ss.ToString(-1), InterpolationIds, ord(erp_in)));
					erp_out := erp_in;
					ss.Pop;
				end else
				begin
					if ss.GetTableS(idx, 'IN') then
					begin
						erp_in := InterpolationMode(FindStr(ss.ToString(-1), InterpolationIds, ord(erp_in)));
						ss.Pop;
					end;
					if ss.GetTableS(idx, 'out') then
					begin
						erp_out := InterpolationMode(FindStr(ss.ToString(-1), InterpolationIds, ord(erp_out)));
						ss.Pop;
					end;
				end;
			end;
			v := ndvalue(ss, idx);
			result^.AddKey(v, start, erp_in, erp_out);
		end;

		function ndvaluefieldornil(var ss: ScriptState; idx: sint; const field: string): vector;
		begin
			if ss.GetTableS(idx, field) then
			begin
				result := ndvalue(ss, -1);
				ss.Pop;
			end else
				result := nil;
		end;

		procedure scan_aua(var ss: ScriptState; idx: sint; const start: float);
		var
			a, b: vector;
		begin
			ss.GetTableS(idx, 'a'); a := ndvalue(ss, -1); ss.Pop;
			ss.GetTableS(idx, 'b'); b := ndvalue(ss, -1); ss.Pop;
			result^.AddAUA(a, b, ndvaluefieldornil(ss, idx, 'velA'), ndvaluefieldornil(ss, idx, 'velB'),
				start, ss.GetFloatField(idx, 'time', 1.0), ss.GetFloatField(idx, 'kA'), ss.GetFloatField(idx, 'kB'));
		end;

	var
		special: string;
		start: float;
	begin
		idx := ss.AbsIdx(idx);
		case ss.Typ(idx) of
			script_Table:
				begin
					if dim > 0 then result := MakeRef(new(pDimensionalPath, Init(dim))) else result := nil;
					defErp := erp_Linear;
					if ss.GetTableS(idx, 'inout') then
					begin
						defErp := InterpolationMode(FindStr(ss.ToString(-1), InterpolationIds, ord(defErp)));
						ss.Pop;
					end;

					ss.PushNil;
					while ss.Next(idx) do
					begin
						if ss.IsNumber(-2) then
						begin
							start := ss.ToFloat(-2);
							if ss.IsTable(-1) then special := ss.GetStringField(-1, 'special') else special := '';
							if special = '' then scan_generic_frame(ss, -1, start) else
							if special = 'aua' then scan_aua(ss, -1, start) else
								ss.UnknownIdentifier(special);
						end;
						ss.Pop;
					end;

					if ss.GetTableS(idx, 'len') then
					begin
						result^.Len := ss.ToFloat(-1);
						ss.Pop;
					end;
					if ss.GetBoolField(idx, 'looped') then result^.Looped := yes;
				end;
			else ss.Throw('ожидается таблица');
		end;
	end;

	function Script_distribution(var ss: ScriptState; idx: sint): Distribution;
	var
		id: string;
		pidx: sint;
		kind: Distribution.KindEnum;
	begin
		idx := ss.AbsIdx(idx);
		id := ss.GetStringField(idx, 1);
		pidx := 1;
		if id = '' then kind := dis_Uniform else
		begin
			kind := Distribution.KindEnum(FindStr(id, Distribution.KindIds, ord(dis_Uniform)));
			inc(pidx);
		end;

		case kind of
			dis_Uniform: result := Distribution.Uniform(ss.GetFloatField(idx, pidx + 0, 0.0), ss.GetFloatField(idx, pidx + 1, 1.0));
			dis_Bell: result := Distribution.Bell(ss.GetFloatField(idx, pidx + 0), ss.GetFloatField(idx, pidx + 1), ss.GetFloatField(idx, pidx + 2));
			else Assert(no);
		end;
	end;

	procedure OpenScript(var script: ScriptState);
	const
		Stuff: array[0 .. 89 {$ifdef Debug} + 1 {$endif}] of ScriptStuffDesc =
		(
			(s: TypeDesc; p: TypeOf(Camera)),
			(s: 'fov' + Writeable; p: @Script_Camera_fov),
			(s: 'pos'; p: @Script_Camera_pos),
			(s: 'tPos' + Writeable; p: @Script_Camera_tPos),
			(s: 'qrot' + Writeable; p: @Script_Camera_qrot),
			(s: 'target'; p: @Script_Camera_target),
			(s: 'tTarget' + Writeable; p: @Script_Camera_tTarget),
			(s: 'tTargetCastShift' + Writeable; p: @Script_Camera_tTargetCastShift),
			(s: 'tZrot' + Writeable; p: @Script_Camera_tZrot),
			(s: 'accK' + Writeable; p: @Script_Camera_accK),
			(s: 'cinematic' + Writeable; p: @Script_Camera_cinematic),
			(s: 'postprocess' + Writeable; p: @Script_Camera_postprocess),

			(s: 'UnProject:1'; p: @Script_Camera_UnProject),
			(s: 'GL'; p: @Script_Camera_GL),

			(s: TypeDesc; p: TypeOf(Postprocess)),
		{$ifdef Debug} (s: 'Dump:0'; p: @Script_Postprocess_Dump), {$endif}
			(s: TypeDesc; p: TypeOf(tGLMesh)),
			(s: TypeDesc; p: TypeOf(GLMaterial)),
			(s: TypeDesc; p: TypeOf(tTexture)),

			(s: TypeDesc; p: TypeOf(SceneNode)),
			(s: 'tf' + Writeable; p: @Script_SceneNode_tf),
			(s: 'wtf' + Writeable; p: @Script_SceneNode_wtf),
			(s: 'pos' + Writeable; p: @Script_SceneNode_pos),
			(s: 'wpos'; p: @Script_SceneNode_wpos),
			(s: 'rot' + Writeable; p: @Script_SceneNode_rot),
			(s: 'wrot'; p: @Script_SceneNode_wrot),
			(s: 'scale' + Writeable; p: @Script_SceneNode_scale),
			(s: 'wscale'; p: @Script_SceneNode_wscale),
			(s: 'assoc' + Writeable; p: @Script_handle_assoc),
			(s: 'onUpdate'; p: @Script_SceneNode_onUpdate),

			(s: 'Attach:0'; p: @Script_SceneNode_Attach),
			(s: 'Detach:0'; p: @Script_SceneNode_Detach),
			(s: 'GetLocalAABB'; p: @Script_SceneNode_GetLocalAABB),
			(s: 'Move'; p: @Script_SceneNode_Move),
			(s: 'Rotate'; p: @Script_SceneNode_Rotate),
			(s: 'AddTimer'; p: @Script_AddTimer),
			(s: 'Slide'; p: @Script_Slide),

			(s: TypeDesc; p: TypeOf(SceneRoot)),
			(s: 'camera'; p: @Script_Scene_camera),
			(s: 'phys' + Writeable; p: @Script_Scene_phys),
			(s: 'ways'; p: @Script_Scene_ways),
			(s: 'nobjs'; p: @Script_Scene_nobjs),
			(s: 'onRayCast'; p: @Script_Scene_onRayCast),
			(s: 'onCreateOutdoorItem'; p: @Script_Scene_onCreateOutdoorItem),
			(s: 'onDestroyOutdoorItem'; p: @Script_Scene_onDestroyOutdoorItem),

			(s: 'ObjectsInRadius:1'; p: @Script_Scene_ObjectsInRadius),

			(s: TypeDesc; p: TypeOf(EntityAction)),
			(s: 'Stop:0'; p: @Script_EntityAction_Stop),

			(s: TypeDesc; p: TypeOf(Slide)),
			(s: 'current'; p: @Script_Slide_current),

			(s: TypeDesc; p: TypeOf(SceneNodeMove)),
			(s: TypeDesc; p: TypeOf(SceneNodeRotate)),

			(s: TypeDesc; p: TypeOf(SceneNodeTimer)),
			(s: 'period' + Writeable; p: @Script_SceneNodeTimer_period),
			(s: 'Now:0'; p: @Script_SceneNodeTimer_Now),

			(s: TypeDesc; p: TypeOf(SlideGL)),
			(s: TypeDesc; p: TypeOf(RTTOperator)),

			(s: TypeDesc; p: TypeOf(Light)),
			(s: 'color' + Writeable; p: @Script_Light_color),
			(s: 'radius' + Writeable; p: @Script_Light_radius),
			(s: 'fadeK' + Writeable; p: @Script_Light_fadeK),
			(s: 'k' + Writeable; p: @Script_Light_k),
			(s: 'position' + Writeable; p: @Script_Light_position),

			(s: TypeDesc; p: TypeOf(VirtualLight)),
			(s: 'color' + Writeable; p: @Script_VirtualLight_color),
			(s: 'radius' + Writeable; p: @Script_VirtualLight_radius),
			(s: 'fadeK' + Writeable; p: @Script_VirtualLight_fadeK),
			(s: 'k' + Writeable; p: @Script_VirtualLight_k),

			(s: TypeDesc; p: TypeOf(RenderObject)),
			(s: 'GL'; p: @Script_RenderObject_GL),
			(s: 'SlideGL'; p: @Script_RenderObject_SlideGL),

			(s: TypeDesc; p: TypeOf(SkeletonNode)),
			(s: 'AttachToBone:0'; p: @Script_SkeletonNode_AttachToBone),
			(s: 'IK:0'; p: @Script_SkeletonNode_IK),

			(s: FunctionsDesc + 'CreatePostprocess:1' + RequireEnv; p: @Script_CreatePostprocess),
			(s: 'CreateMaterial:1' + RequireEnv; p: @Script_CreateMaterial),
			(s: 'Load' + RequireEnv; p: @Script_Load),
			(s: 'Memory'; p: @Script_Memory),
			(s: 'System:1'; p: @Script_System),
			(s: 'VisualizeKD:0'; p: @Script_VisualizeKD),
			(s: 'VisualizeSkeleton:0'; p: @Script_VisualizeSkeleton),
			(s: 'VisualizeWaypoints:0'; p: @Script_VisualizeWaypoints),
			(s: 'VisualizeWay:0'; p: @Script_VisualizeWay),
			(s: 'Config' + RequireEnv; p: @Script_Config),
			(s: 'CreateRTTOperator:1'; p: @Script_CreateRTTOperator),
			(s: 'CreateLight:1'; p: @Script_CreateLight),
			(s: 'CreateRenderObject:1' + RequireEnv; p: @Script_CreateRenderObject),
			(s: 'CreateSceneNode:1'; p: @Script_CreateSceneNode),
			(s: 'CreateScene:1'; p: @Script_CreateScene),
			(s: 'CreateSkeletonNode:1' + RequireEnv; p: @Script_CreateSkeletonNode),
			(s: 'Save' + RequireEnv; p: @Script_Save)
		);
	begin
	{$ifdef Debug} LogR('Загрузка скриптового интерфейса движка... '); {$endif}
		script.AddStuff(Stuff);
		MMSystem.OpenScript(script);
		GLClasses.OpenScript(script);
		GUI.OpenScript(script);
		Audio.OpenScript(script);
		Physics.OpenScript(script);
		PathFinding.OpenScript(script);
		Labyrinth.OpenScript(script);
		Character.OpenScript(script);
		ProceduralGraphics.OpenScript(script);
		Inventories.OpenScript(script);
		TeamRelations.OpenScript(script);
	{$ifdef Debug} Log('Скриптовый интерфейс движка загружен', logOK); {$endif}
	end;

	procedure Init;
	begin
	{$ifdef use_serialization}
		SerializationDB.Shared^
		.RegisterFuncs([@OnAttachDetachNode, @OnSceneRayCast, @OnCreateDestroyOutdoorItem, @OnSceneNodeUpdate,
		                @EntityAction_OnProcess, @EntityAction_OnDone, @SceneNodeTimer_OnTimer]);
	{$endif}
	end;

initialization
	&Unit('Script_EngineAPI').Initialize(@Init);
end.
