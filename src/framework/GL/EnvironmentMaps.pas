unit EnvironmentMaps;

{$include opts.inc}

interface

uses
	USystem, UMath, UClasses, SceneGraph, Cameras, U_GL, GLBase, GLClasses, SpatialIndex {$ifdef use_serialization}, Streams {$endif}
{$ifdef Debug}, ULog {$endif};

type
	RTTOperatorFlag = (rtop_Endless, rtop_ExternalCameraControl, rtop_Precise, rtop_MainCamera, rtop_ClipPlane, rtop_ReflectPlane);
	RTTOperatorFlags = set of RTTOperatorFlag;
	RTTOperatorSizeMode = (rtop_PredefSize, rtop_WindowSize, rtop_WindowSizeX, rtop_WindowSizeY);

	pRTTOperator = ^RTTOperator;
	RTTOperator = object(SceneNode)
	private type
		tBind = object
			container: pObject;
			v: pGLEntityParams;
			color: array of PoolString;
			depth: PoolString;
			procedure Initialize(newContainer: pObject; newV: pGLEntityParams; const newBaseName: string; nColors: sint);
			procedure Finalize;
			procedure Discard;
		end;
	private var
		_bnd: Bounding;
		_flags: RTTOperatorFlags;
		_scenario: pRenderScenario;
		_target: GLTextureTarget;
		_sizeMode: array[0 .. 1] of RTTOperatorSizeMode;
		_baseSize: array[0 .. 1] of uint;
		_switchingProgress: sint;
		_baseRefreshPeriod, _refreshPeriod, _lastRefreshTime, _lastSwitchTime: float;
		_rt: tRenderTarget;
		_lastRefreshedSide: GLCubeSide;
		_texChanged, _visible: boolean;
		_binds: array of tBind;
		_quality, _invQuality: float;
		_planePoint, _planeNormal: Vec3;
		_clipPlaneShift: float;
		procedure _Initialize(const newName: PoolString);
		procedure SetQuality(const newQuality: float); procedure UpdateQuality;
		function _GetSidesWhichNeedsRefresh(timePassed: float): GLCubeSides;
		procedure _Discard(just4switch: boolean);
		function _GetBaseSize(id: uint): uint;
		function _MakeBeauty(x: sint; const coef: float): sint;
	private const
		RelSizeDiscrete = 512;
	protected
		procedure _OnUpdate(const dt: float); virtual;
		procedure _OnUpdateVis; virtual;
		function _SuitsTo(know: SceneKnowledge): boolean; virtual;
	public
		camera: Camera;
		colors: array of record
			tex: pTexture;
			format: GLImageFormat;
		end;
		constructor Init(const newName: PoolString;
			newScenario: pRenderScenario; newTarget: GLTextureTarget; newFormats: array of GLImageFormat;
			const newSizes: array of float; const newSizeModes: array of RTTOperatorSizeMode;
			newFlags: RTTOperatorFlags = []);
		destructor Done; virtual;
		procedure Bind(container: pObject; v: pGLEntityParams; const baseName: string);
		procedure Unbind(container: pObject);
		function GetRT: pRenderTarget;
		procedure SetSize(id: sint; const newSize: float);

		property Quality: float read _quality write SetQuality;
		property BaseRefreshPeriod: float read _baseRefreshPeriod write _baseRefreshPeriod;
		property PlanePoint: Vec3 read _planePoint write _planePoint;
		property PlaneNormal: Vec3 read _planeNormal write _planeNormal;
		property ClipPlaneShift: float read _clipPlaneShift write _clipPlaneShift;
	end;

const
	RTTOpSizeModeIds: array[RTTOperatorSizeMode] of string = ('predef', 'win', 'winx', 'winy');

implementation

uses
	Scene, MMSystem
{$ifdef use_serialization}, Serialization {$endif}
{$ifdef Profile}, Profile {$endif};

	function RTTOperator._MakeBeauty(x: sint; const coef: float): sint;
	var
		x2, k: float;
	begin
		if x <= 1 then exit(1);
		if rtop_Precise in _flags then exit(x);
		x2 := max(x * coef, 1.0);
		k := pow(2, uint(round(max(logn(2, x2) - 2.0, 0.0))));
		result := max(round(k*round(x2/k)), 1);
		if result < 8 then result := pow(2, uint(round(logn(2, result))));
		result := min(result, x);
	end;

	procedure RTTOperator._Initialize(const newName: PoolString);
	var
		i: sint;
		fmts: array of GLImageFormat;
	begin
		camera.Init([], @_rt);
		camera.ZNear := 0.05;
		camera.FOV := 90.0;
		camera.ClampToScene := rtop_Endless in _flags;
		UpdateQuality;
		_refreshPeriod := -1.0;
		_lastRefreshedSide := Low(_lastRefreshedSide);
		_lastRefreshTime := -1000.0;

		_switchingProgress := -1;
		_lastSwitchTime := -1000.0;

		for i := 0 to High(colors) do
			colors[i].tex := nil;
		SetLength(fmts, length(colors));
		for i := 0 to High(fmts) do
			fmts[i] := colors[i].format;
		_rt.Init(newName, _target, fmts, UintVec2.Ones, rt_SharedDepth);

		_texChanged := yes;
		_visible := yes;
		_binds := nil;
	end;

	constructor RTTOperator.Init(const newName: PoolString;
		newScenario: pRenderScenario; newTarget: GLTextureTarget; newFormats: array of GLImageFormat;
		const newSizes: array of float; const newSizeModes: array of RTTOperatorSizeMode;
		newFlags: RTTOperatorFlags = []);
	var
		i: sint;
	begin
		Assert(length(newFormats) > 0);
		inherited Init;
		_scenario  := MakeRef(newScenario);
		_target    := newTarget;
		for i := 0 to TextureTargetsInfo[_target].determinativeDims - 1 do
		begin
			if i < length(newSizeModes) then
				_sizeMode[i] := newSizeModes[i]
			else
				if i > 0 then
					_sizeMode[i] := _sizeMode[0]
				else
					_sizeMode[i] := rtop_PredefSize;
			if i < length(newSizes) then
				SetSize(i, newSizes[i])
			else
				if length(newSizes) > 0 then
					SetSize(i, newSizes[0])
				else
					SetSize(i, 1);
		end;
		SetLength(colors, length(newFormats));
		for i := 0 to High(colors) do
			colors[i].format := newFormats[i];
		_flags     := newFlags;
		_baseRefreshPeriod := 1.0 / 20.0;
		_quality   := 1.0;
		_planePoint := Vec3.Zero;
		_planeNormal := Vec3.PositiveY;
		_clipPlaneShift := 0.0;
		_Initialize(newName);
	end;

	procedure __UnbindOnDestroy(obj: pObject; param: pointer);
	begin
		pRTTOperator(param)^.Unbind(obj);
	end;

	destructor RTTOperator.Done;
	var
		i: sint;
	begin
		for i := 0 to High(colors) do
			Release(colors[i].tex);
		for i := 0 to High(_binds) do
		begin
			_binds[i].container^.RemoveOnDestroyProc(@__UnbindOnDestroy, @self);
			_binds[i].Finalize;
		end;
		Release(_scenario);
		camera.Done;
		_rt.Done;
		inherited Done;
	end;

	procedure RTTOperator.tBind.Initialize(newContainer: pObject; newV: pGLEntityParams; const newBaseName: string; nColors: sint);
	var
		i: sint;
	begin
		container := newContainer;
		v := newV;
		SetLength(color, nColors);
		for i := 0 to High(color) do
		begin
			color[i] := newBaseName + ToString(i);
			v^.values.Value(color[i], GLType.Sampler, 1, [NativeGLValueFlag.NonSerializable]);
		end;
		depth := newBaseName + '_depth';
		v^.values.Value(depth, GLType.Sampler, 1, [NativeGLValueFlag.NonSerializable]);
	end;

	procedure RTTOperator.tBind.Finalize;
	begin
		if Assigned(container) then Discard;
		color := nil;
	end;

	procedure RTTOperator.tBind.Discard;
	var
		i: sint;
	begin
		for i := 0 to High(color) do
			v^.values.Value(color[i])^.SetTex(nil);
		v^.values.Value(depth)^.SetTex(nil);
	end;

	procedure RTTOperator.Bind(container: pObject; v: pGLEntityParams; const baseName: string);
{$ifdef Debug} var i: sint; {$endif}
	begin
	{$ifdef Debug}
		for i := 0 to High(_binds) do
			if _binds[i].v = v then
			begin
				Log('Дублирующийся бинд RTT оператора "' + _rt.name + '": ' + baseName, logError);
			end;
	{$endif}
		SetLength(_binds, length(_binds) + 1);
		_binds[High(_binds)].Initialize(container, v, baseName, length(_rt.color));
		container^.AddOnDestroyProc(@__UnbindOnDestroy, @self);
	end;

	procedure RTTOperator.Unbind(container: pObject);
	var
		i: sint;
	begin
		for i := 0 to High(_binds) do
			if _binds[i].container = container then
			begin
				_binds[i].container := nil;
				_binds[i].Finalize;
				_binds[i] := _binds[High(_binds)];
				SetLength(_binds, length(_binds) - 1);
				break;
			end;
	end;

	function RTTOperator.GetRT: pRenderTarget;
	begin
		result := @_rt;
	end;

	procedure RTTOperator.SetSize(id: sint; const newSize: float);
	begin
		case _sizeMode[id] of
			rtop_PredefSize: _baseSize[id] := round(newSize);
			rtop_WindowSize, rtop_WindowSizeX, rtop_WindowSizeY: _baseSize[id] := round(newSize * RelSizeDiscrete);
			else Assert(no);
		end;
	end;

	procedure RTTOperator.SetQuality(const newQuality: float);
	begin
		if not Equals(_quality, newQuality) then
		begin
			_quality := newQuality;
			UpdateQuality;
		end;
	end;

	procedure RTTOperator.UpdateQuality;
	begin
		if _quality <> 0 then
			_invQuality := 1.0 / _quality
		else
			_invQuality := 0.0;
	end;

	function RTTOperator._GetSidesWhichNeedsRefresh(timePassed: float): GLCubeSides;
	var
		startSide: GLCubeSide;
	begin
		result := [];
		startSide := _lastRefreshedSide;
		while timePassed > 0 do
		begin
			timePassed := timePassed - _refreshPeriod;
			if _lastRefreshedSide = High(_lastRefreshedSide) then
				_lastRefreshedSide := Low(_lastRefreshedSide)
			else
				inc(_lastRefreshedSide);
			Include(result, _lastRefreshedSide);
			if _lastRefreshedSide = startSide then break;
		end;
	end;

	procedure RTTOperator._Discard(just4switch: boolean);
	var
		i: sint;
	begin
		if Assigned(_rt.color[0].tex) then
		begin
			_rt.Discard;
			_texChanged := yes;
			if just4switch then _switchingProgress := 0 else
			begin
				for i := 0 to High(_binds) do
					_binds[i].Discard;
				for i := 0 to High(colors) do
					Release(colors[i].tex);
				_switchingProgress := -1;
			end;
		end;
	end;

	function RTTOperator._GetBaseSize(id: uint): uint;
	begin
		case _sizeMode[id] of
			rtop_PredefSize: result := _baseSize[id];
			rtop_WindowSize:
				case id of
					0: result := (MainRT.Size.X * _baseSize[id]) div RelSizeDiscrete;
					1: result := (MainRT.Size.Y * _baseSize[id]) div RelSizeDiscrete;
					else Assert(no);
				end;
			rtop_WindowSizeX: result := (MainRT.Size.X * _baseSize[id]) div RelSizeDiscrete;
			rtop_WindowSizeY: result := (MainRT.Size.Y * _baseSize[id]) div RelSizeDiscrete;
			else Assert(no);
		end;
	end;
{$undef impl}

	procedure RTTOperator._OnUpdate(const dt: float);
		function _GetMinDelay(const lod: float): float;
		begin
			if lod < 1.0 then result := 2.5 else result := 0.5;
		end;
	const
		Threshold = 0.9;
	var
		resk, k, lod: float;
		bsx, newRes, newResY: uint;
	begin
		Assert(@dt = @dt);
		inherited _OnUpdate(dt);

		if Assigned(parent^.Bnd) then
			if Assigned(Bnd) then ChangeBounding(parent^.Bnd^) else
			begin
				_bnd := parent^.Bnd^;
				Bnd := @_bnd;
			end
		else
			Bnd := nil;
		lod := CalculateLOD(pScene(scene)^.camera.pos);

		if NotZero(lod) then
		begin
			_refreshPeriod := _baseRefreshPeriod / TextureTargetsInfo[_rt.target].faces / pow(lod, 3.5 * _invQuality);
			if not (rtop_Precise in _flags) then _refreshPeriod *= _invQuality;
			_refreshPeriod := min(_refreshPeriod, 3.5);
		end else
			_refreshPeriod := Infinity;

		if _visible then
			_visible := no
		else
			_Discard(no);

		if mm.SceneTimeSince(_lastSwitchTime) >= _GetMinDelay(lod) then
		begin
			bsx := _GetBaseSize(0);
			if not (rtop_Precise in _flags) then
			begin
				resk := pow(lod, 2.5 * _invQuality);
				newRes := _MakeBeauty(bsx, resk);
				k := newRes / _rt.size.X;
			end else
				newRes := max(bsx, 1);
			if ((rtop_Precise in _flags) and (newRes <> _rt.size.X)) or
				((not (rtop_Precise in _flags)) and ((k < Threshold) or (k > 1.0 / Threshold) or ((newRes >= bsx) and (_rt.size.X < bsx)))) then
			begin
				Assert(newRes <> _rt.size.X);
				if TextureTargetsInfo[_rt.target].determinativeDims > 1 then newResY := _MakeBeauty(_GetBaseSize(1), resk) else newResY := 1;
				_Discard(yes);
				_rt.Resize(UintVec2.Make(newRes, newResY));
			end;
			_lastSwitchTime := mm.SceneTime;
		end;
	end;

	procedure RTTOperator._OnUpdateVis;
		procedure EnsureTextures;
		var
			i, j: sint;
		begin
			if _texChanged then
			begin
				_rt.Prepare;
				if _switchingProgress < 0 then
				begin
					for j := 0 to High(colors) do
						SetRef(colors[j].tex, _rt.color[j].tex);
					for i := 0 to High(_binds) do
					begin
						for j := 0 to High(_binds[i].color) do
							_binds[i].v^.values.Value(_binds[i].color[j])^.SetTex(colors[j].tex);
						_binds[i].v^.values.Value(_binds[i].depth)^.SetTex(_rt.depth);
					end;
					_texChanged := no;
				end;
			end;
		end;

		procedure Render(var camera: Camera; const lod: float; clipPlanep: pPlane);
		var
			rast: GLRasterizerState;
			clear: GLRenderBuffers;
		begin
			if rtop_ReflectPlane in _flags then
			begin
				rast.frontFace := GLcircuit_CW;
				gl.SetRasterizerState(rast, [GLrast_FrontFace]);
			end;

			clear := [GLbuffer_Depth];
			if (_target = GLtexture_2D) and ([rtop_ClipPlane, rtop_ReflectPlane] * _flags <> []) then
			begin
			end else
				Include(clear, GLbuffer_Color);
			pScene(Root)^.Render(camera, _scenario, lod * _quality, parent, clipPlanep, clear);

			if rtop_ReflectPlane in _flags then
			begin
				rast.frontFace := GLcircuit_CCW;
				gl.SetRasterizerState(rast, [GLrast_FrontFace]);
			end;
		end;

	const
		Rsq2 = 1.0 / sqrt(2);
		CamAngles: array[GLCubeSide] of Quaternion =
		(
			(data: (v: (-Rsq2, 0, -Rsq2,    0))),
			(data: (v: ( Rsq2, 0, -Rsq2,    0))),
			(data: (v: (-Rsq2, 0,     0, Rsq2))),
			(data: (v: ( Rsq2, 0,     0, Rsq2))),
			(data: (v: (    0, 0,    -1,    0))),
			(data: (v: (    1, 0,     0,    0)))
		);
	var
		timePassed, lod: float;
		refreshIt: GLCubeSides;
		curSide: GLCubeSide;
		texCh: boolean;
		refPlane, clipPlane: Plane;
		clipPlanep: pPlane;
		cc: pCamera;
	begin
	trace_call('RTTOperator._OnUpdateVis');
		inherited _OnUpdateVis;
		_visible := yes;
		timePassed := mm.SceneTimeSince(_lastRefreshTime);
		if timePassed < _refreshPeriod then exit;
		texCh := _texChanged;
		EnsureTextures;
		lod := CalculateLOD(pScene(scene)^.camera.pos);

		clipPlanep := nil;
		if [rtop_ClipPlane, rtop_ReflectPlane] * _flags <> [] then
		begin
			refPlane := Plane.Make(globalTransform * _planePoint, WorldRot * _planeNormal);
			if rtop_ClipPlane in _flags then
			begin
				clipPlane := refPlane;
				clipPlane.d := clipPlane.d - _clipPlaneShift;
				clipPlanep := @clipPlane;
			end;
		end;

		if rtop_MainCamera in _flags then
		begin
			cc := @pScene(Root)^.camera;
			camera.forceAspect := MainRT.size.Aspect;
			if (rtop_ClipPlane in _flags) and (((rtop_ReflectPlane in _flags)) xor (refPlane.SignedDistance(cc^.pos) > 0)) then
				clipPlane := refPlane.Reflect(clipPlane);

			if rtop_ReflectPlane in _flags then
			begin
				camera.pos := refPlane.ReflectPoint(cc^.pos);
				camera.FOV := -cc^.FOV;
				camera.qrot := refPlane.Reflect(cc^.qrot);
			end else
			begin
				camera.pos := cc^.pos;
				camera.FOV := cc^.FOV;
				camera.qrot := cc^.qrot;
			end;
			camera.ZNear := cc^.ZNear;
			camera.ZFar := cc^.ZFar;
			camera.ClampToScene := cc^.ClampToScene;
		end else
		begin
			camera.tPos := WorldPos;
			if not (rtop_ExternalCameraControl in _flags) then
				camera.QRot := WorldRot;
		end;
		if rtop_Endless in _flags then
		begin
			if Assigned(bnd) then camera.ZNear := pScene(Root)^.camera.ZNear;
		end else
		begin
			if Assigned(bnd) then camera.ZFar := bnd^.Radius;
		end;

		if _target = GLtexture_2D then
		begin
			camera.Update(mm.FrameDt);
			Render(camera, lod, clipPlanep);
			_lastRefreshTime := mm.SceneTime;
			if _switchingProgress >= 0 then _switchingProgress := -1;
			exit;
		end;

		if texCh then refreshIt := [Low(GLCubeSide) .. High(GLCubeSide)] else
			if _refreshPeriod > 0.0 then refreshIt := _GetSidesWhichNeedsRefresh(timePassed) else
				begin
				{$ifdef Debug} if _refreshPeriod <> -1.0 then Log(_rt.name + ': refreshPeriod <= 0?!', logWarning); {$endif}
					refreshIt := [Low(GLCubeSide) .. High(GLCubeSide)];
					_refreshPeriod := 0.0;
				end;
	{$ifdef Debug} Assert(refreshIt <> []); {$endif}

		for curSide := Low(curSide) to High(curSide) do
			if curSide in refreshIt then
			begin
				camera.QRot := CamAngles[curSide];
				_rt.SwitchToSublevel(ord(curSide));
				camera.UpdateStatic;
				Render(camera, lod, clipPlanep);
				if _switchingProgress >= 0 then
				begin
					inc(_switchingProgress); Assert(_switchingProgress >= 0);
					if uint(_switchingProgress) >= TextureTargetsInfo[_rt.target].faces then _switchingProgress := -1;
				end;
			end;
		_lastRefreshTime := mm.SceneTime;
	leave_call
	end;

	function RTTOperator._SuitsTo(know: SceneKnowledge): boolean;
	begin
		case know of
			scene_Update: result := no;
			scene_DelayedUpdate, scene_OmniVisibleWithoutCulling: result := yes;
			else
				result := inherited _SuitsTo(know);
		end;
	end;

{$ifdef use_serialization}
const
	RTOP_ENDLESS_BIT         = 1 shl 0;
	RTOP_MAIN_CAMERA_BIT     = 1 shl 1;
	RTOP_MRT_BIT             = 1 shl 2;
	RTOP_CLIP_PLANE_BIT      = 1 shl 3;
	RTOP_REFLECT_PLANE_BIT   = 1 shl 4;

	procedure SerializeRTTOperator(se: pSerializer; obj: pointer);
	var
		rt: pRTTOperator absolute obj;
		flags: uint;
		i: sint;
		sizemode: uint;
	begin
		with se^ do
		begin
			flags := 0;
			if rtop_Endless in rt^._flags then flags := flags or RTOP_ENDLESS_BIT;
			if rtop_MainCamera in rt^._flags then flags := flags or RTOP_MAIN_CAMERA_BIT;
			if length(rt^.colors) > 1 then flags := flags or RTOP_MRT_BIT;
			if rtop_ClipPlane in rt^._flags then flags := flags or RTOP_CLIP_PLANE_BIT;
			if rtop_ReflectPlane in rt^._flags then flags := flags or RTOP_REFLECT_PLANE_BIT;

			Serialize_ui8(stream, flags);
			Serialize_string(stream, rt^._rt.name);
			Serialize_ui8(stream, ord(rt^._target));
			if (flags and RTOP_MRT_BIT) <> 0 then Serialize_ui8(stream, length(rt^.colors));
			for i := 0 to High(rt^.colors) do
				Serialize_ui8(stream, ord(rt^.colors[i].format));
			SeObject(rt^._scenario);
			sizemode := ord(rt^._sizeMode[0]) or (ord(rt^._sizeMode[1]) shl 4);
			Serialize_ui8(stream, sizemode);
			for i := 0 to TextureTargetsInfo[rt^._target].determinativeDims - 1 do
				Serialize_ui16(stream, rt^._baseSize[i]);
			Serialize_f16(stream, rt^._quality);
			Serialize_f16(stream, rt^._baseRefreshPeriod);

			if [rtop_ClipPlane, rtop_ReflectPlane] * rt^._flags <> [] then
			begin
				Serialize_vec3f32(stream, rt^._planePoint);
				Serialize_vec3f32(stream, rt^._planeNormal);
				if rtop_ClipPlane in rt^._flags then Serialize_f32(stream, rt^._clipPlaneShift);
			end;

			Serialize_ui8(stream, length(rt^._binds));
			for i := 0 to High(rt^._binds) do
			begin
				SeObject(rt^._binds[i].container);
				SeObject(rt^._binds[i].v, TypeOf(GLEntityParams));
				Serialize_string(stream, Copy(rt^._binds[i].color[0].internal, 1, length(rt^._binds[i].color[0].internal) - 1));
			end;
		end;
	end;

	procedure DeserializeRTTOperator(de: pDeserializer; obj: pointer);
	var
		rt: pRTTOperator absolute obj;
		flags: uint;
		i, nTex: sint;
		sizemode: uint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			if (flags and RTOP_ENDLESS_BIT) <> 0 then Include(rt^._flags, rtop_Endless);
			if (flags and RTOP_MAIN_CAMERA_BIT) <> 0 then Include(rt^._flags, rtop_MainCamera);
			if (flags and RTOP_CLIP_PLANE_BIT) <> 0 then Include(rt^._flags, rtop_ClipPlane);
			if (flags and RTOP_REFLECT_PLANE_BIT) <> 0 then Include(rt^._flags, rtop_ReflectPlane);
			rt^._rt.name := Deserialize_string(stream);

			rt^._target := GLTextureTarget(Deserialize_ui8(stream));
			if (flags and RTOP_MRT_BIT) <> 0 then nTex := Deserialize_ui8(stream) else nTex := 1;
			SetLength(rt^.colors, nTex);
			for i := 0 to High(rt^.colors) do
			begin
				rt^.colors[i].format := GLImageFormat(Deserialize_ui8(stream));
			end;
			DeObjectR(rt^._scenario);
			sizemode := Deserialize_ui8(stream);
			rt^._sizeMode[0] := RTTOperatorSizeMode(sizemode and (1 shl 4 - 1));
			rt^._sizeMode[1] := RTTOperatorSizeMode(sizemode shr 4);
			for i := 0 to TextureTargetsInfo[rt^._target].determinativeDims - 1 do
				rt^._baseSize[i] := Deserialize_ui16(stream);
			rt^._quality := Deserialize_f16(stream);
			rt^._baseRefreshPeriod := Deserialize_f16(stream);

			if [rtop_ClipPlane, rtop_ReflectPlane] * rt^._flags <> [] then
			begin
				rt^._planePoint := Deserialize_vec3f32(stream);
				rt^._planeNormal := Deserialize_vec3f32(stream);
				if rtop_ClipPlane in rt^._flags then rt^._clipPlaneShift := Deserialize_f32(stream);
			end;

			SetLength(rt^._binds, Deserialize_ui8(stream));
			for i := 0 to High(rt^._binds) do
			begin
				DeWeakA(rt^._binds[i].container);
				DeWeakA(rt^._binds[i].v);
				rt^._binds[i].depth := Deserialize_string(stream);
			end;
		end;
	end;

	procedure RTTOperatorDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		rt: pRTTOperator absolute obj;
		name: PoolString;
		baseName: string;
		binds: array of RTTOperator.tBind;
		i: sint;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: rt^.DeseInit;
			de_After:
				begin
					binds := rt^._binds;
					name := rt^._rt.name; Finalize(rt^._rt.name);
					rt^._Initialize(name);
					for i := 0 to High(binds) do
					begin
						baseName := binds[i].depth;
						rt^.Bind(binds[i].container, binds[i].v, baseName);
					end;
				end;
		end;
	end;
{$endif}

	procedure Init;
	begin
	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Render-to-texture operator', TypeOf(RTTOperator), TypeOf(SceneNode), sizeof(RTTOperator), yes,
			@SerializeRTTOperator, @DeserializeRTTOperator, nil, @RTTOperatorDeSpecial);
	{$endif}
	end;

initialization
	&Unit('EnvironmentMaps').Initialize(@Init);
end.
