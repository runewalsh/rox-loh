unit Lights;

{$include opts.inc}

interface

uses
	USystem, UClasses, UMath, U_GL, GLBase, GLClasses, SceneGraph, EnvironmentMaps, Cameras, SpatialIndex
{$ifdef use_serialization}, Streams {$endif}
{$ifdef Debug}, Utils, ULog {$endif};

type
	LightID = type byte;
	LightsSet = set of LightID;

const
	AllLights: LightsSet = [Low(LightID) .. High(LightID)];
	ShadowFormat = GLformat_RGBA;

type
	pVirtualLightGroup = ^VirtualLightGroup;
	ppLight = ^pLight;
	pLight = ^Light;

	pLightAttributes = ^LightAttributes;
	LightAttributes = object
		base: tLightBase;
		position: Vec3;
		color: Vec3;
		radius: float;
		fadeK: float;
		shadowQuality: sint;
		procedure Add(const a: LightAttributes);
		function Mix(const a, b: LightAttributes; const x: float): LightAttributes; static;

		procedure A2O_Add(const a: LightAttributes; const weight: float);
		procedure A2O_Fix(const invWtSum: float);
		function O_Extract(const weight, invWtSum: float; const src: LightAttributes): LightAttributes;

	const
		Zero: LightAttributes = (base: light_Omni; position: (data: (0.0, 0.0, 0.0)); color: (data: (0.0, 0.0, 0.0)); radius: 0.0; fadeK: 0.0; shadowQuality: 0);
	end;

	pVirtualLight = ^VirtualLight;
	VirtualLight = object(SceneNode)
	private type
		tPart = (part_Color, part_Radius, part_Coeff);
	private var
		_base: tLightBase;
		_shadowQuality: sint;
		_baseRadius, _finalRadius: float;
		_fadeK: float;
		_baseColor, _finalColor: Vec3;
		_coeff: float;
		_startBnd: Bounding;
		_group: pVirtualLightGroup;
		procedure _Recalculate(what: tPart);
		procedure _SetBaseRadius(const newRadius: float);
		procedure _SetBaseColor(const newColor: Vec3);
		procedure _SetCoeff(const newCoeff: float);
		procedure _AbandonGroup(counterpartRef: ppLight);
	protected
		function _SuitsTo(know: SceneKnowledge): boolean; virtual;
		procedure _BeforeDetach; virtual;
	public
		constructor Init(newBase: tLightBase; newShadowQuality: sint = 0);
		constructor DeseInit;
		destructor Done; virtual;
		function GetAttributes: LightAttributes;
		function Mergeable(var a, b: VirtualLight; const lodA, lodB: float): boolean; static;

		property Base: tLightBase read _base;
		property BaseRadius: float read _baseRadius write _SetBaseRadius;
		property FinalRadius: float read _finalRadius;
		property BaseColor: Vec3 read _baseColor write _SetBaseColor;
		property FinalColor: Vec3 read _finalColor;
		property Coeff: float read _coeff write _SetCoeff;
		property FadeK: float read _fadeK write _fadeK;
		property ShadowQuality: sint read _shadowQuality;
	end;

	VirtualLightGroup = object(&Object)
	private type
		tMode = (mode_NA, mode_All, mode_One);
	private var
		_scene: pSceneNode;
		_mode: tMode;
		_mergeDegree: float;
		_lts: array of record
			v: pVirtualLight;
			r: pLight;
			startA: pLightAttributes;
			weight: float;
		end;
		_invCount, _invWtSum: float;
		_one: pLight;
		_corrupted: boolean;
		function _CalculateBoundingSphere: Sphere;
	public
		constructor Init(aScene: pSceneNode);
		destructor Done; virtual;
		procedure Add(light: pVirtualLight; counterpart: pLight);
		procedure Remove(light: pVirtualLight; counterpartRef: ppLight = nil);
		procedure Update(const dmerge: float);
		function Matches(const list: array of pVirtualLight): boolean;
	end;

	Light = object(SceneNode)
	private
	type
		pCSMLazyUpdateRecs = ^tCSMLazyUpdateRecs;
		tCSMLazyUpdateRecs = array[0 .. MAX_CSM_SPLITS-1] of record
			lastFrustum: Frustum;
			lastCamPos: Vec3;
			lastCamRot: Quaternion;
			frustumRelSize: float;
		end;
	var
		_base: tLightBase;
		_baseShadowResolution: sint;
		_transformedPosition: Vec3;
		_sm: array[0 .. MAX_CSM_SPLITS-1] of pRTTOperator;
		_radius, _realRadius, _fadeK: float;
		_csmLazyUpd: pCSMLazyUpdateRecs;
		_first: boolean;
		_realColor: Vec3;
		_startBnd: Bounding;
		procedure _Initialize(newBase: tLightBase; newShadowResolution: sint; des: boolean);
		function _GetNShadows: sint;
		procedure _SetRadius(newRadius: float);
		procedure _UpdateCSM;
		function _GetShadow(id: sint): pRTTOperator;
		procedure _SetShadowQuality;
	protected
		procedure _OnUpdateVis; virtual;
		function _SuitsTo(know: SceneKnowledge): boolean; virtual;
		procedure _AfterAttach; virtual;
		procedure _BeforeDetach; virtual;
	public
		id: LightID;
		position: Vec3;
		color: Vec3;
		coeff: float;
		constructor Init(newBase: tLightBase; newShadowResolution: sint = 0);
		constructor Init(const a: LightAttributes);
		destructor Done; virtual;
		function GetAttributes: LightAttributes;
		procedure ApplyAttributes(const a: LightAttributes);
		function Detail: tLightDetail;

		property Base: tLightBase read _base;
		property Radius: float read _radius write _SetRadius;
		property FadeK: float read _fadeK write _fadeK;
		property Shadow: pRTTOperator read _sm[0];
		property Shadows[i: sint]: pRTTOperator read _GetShadow;
	end;

	SceneLights = object
		v: array[tLightDetail] of array of pLight;
		procedure Initialize;
		procedure Finalize;
		procedure Add(newItem: pLight);
		procedure Remove(item: pLight);
	end;

	tLightingPassList = array[0 .. MAX_ANY_LIGHTS-1] of pLight;
	tLightingPassInfo = object
	public
		estimation: uint;
		byDetail: array[tLightDetail] of record
			n: sint;
			list: tLightingPassList;
		end;
		property nOmniA: sint read byDetail[light_OmniA].n;
		property omniA: tLightingPassList read byDetail[light_OmniA].list;
		property nOmniS: sint read byDetail[light_OmniS].n;
		property omniS: tLightingPassList read byDetail[light_OmniS].list;
		property nTargA: sint read byDetail[light_TargetedA].n;
		property targA: tLightingPassList read byDetail[light_TargetedA].list;
		property nTargS: sint read byDetail[light_TargetedS].n;
		property targS: tLightingPassList read byDetail[light_TargetedS].list;
	end;

	tLightingPassesInfo = record
		count: sint;
		passes: array[0 .. MaxLightingPasses - 1] of tLightingPassInfo;
	end;

const
	LightBaseIds: array[tLightBase] of string = ('omni', 'targeted');

	procedure UpdateVirtualLights(aScene: pSceneNode; const dt: float);
	procedure GetLightingPasses(var lights: SceneLights; const omniA, omniS: LightsSet; out lp: tLightingPassesInfo);
	procedure PrepareLightingPass(const lp: tLightingPassesInfo; passNo: sint; var cam: Camera);
	function CanUseUniFor(ld: tLightDetail): boolean;

implementation

uses
	RenderLists, MMSystem, Scene
{$ifdef use_serialization}, Serialization {$endif}
{$ifdef Profile}, Profile {$endif};

	procedure LightAttributes.Add(const a: LightAttributes);
	begin
		Assert(base = a.base);
		position += a.position;
		color += a.color;
		radius += a.radius;
		fadeK += a.fadeK;
		shadowQuality := max(shadowQuality, a.shadowQuality);
	end;

	function LightAttributes.Mix(const a, b: LightAttributes; const x: float): LightAttributes;
	begin
		Assert(a.base = b.base);
		result.base     := a.base;
		result.position := lerp(a.position, b.position, x);
		result.color    := lerp(a.color,    b.color,    x);
		result.radius   := lerp(a.radius,   b.radius,   x);
		result.fadeK    := lerp(a.fadeK,    b.fadeK,    x);
		result.shadowQuality := max(a.shadowQuality, b.shadowQuality);
	end;

	procedure LightAttributes.A2O_Add(const a: LightAttributes; const weight: float);
	begin
		color += weight * a.color;
		fadeK += weight * a.fadeK;
		shadowQuality := max(shadowQuality, a.shadowQuality);
	end;

	procedure LightAttributes.A2O_Fix(const invWtSum: float);
	begin
		fadeK *= invWtSum;
	end;

	function LightAttributes.O_Extract(const weight, invWtSum: float; const src: LightAttributes): LightAttributes;
	begin
		result := self;
		result.color *= weight * invWtSum;
		result.shadowQuality := src.shadowQuality;
	end;

	procedure VirtualLight._Recalculate(what: tPart);
	var
		newBnd: Bounding;
	begin
		case what of
			part_Color: _finalColor := _baseColor * _coeff;
			part_Radius:
				if _base = light_Omni then
				begin
					_finalRadius := _baseRadius * _coeff;
					newBnd := Bounding.BySphere(Vec3.Zero, _finalRadius);
					ChangeStartBounding(newBnd);
					_finalRadius *= WorldScale;
				end;
			part_Coeff:
				begin
					_Recalculate(part_Color);
					_Recalculate(part_Radius);
				end;
		end;
	end;

	procedure VirtualLight._SetBaseRadius(const newRadius: float);
	begin
		if not Equals(_baseRadius, newRadius) then
		begin
			_baseRadius := newRadius;
			_Recalculate(part_Radius);
		end;
	end;

	procedure VirtualLight._SetBaseColor(const newColor: Vec3);
	begin
		if not Equals(_baseColor, newColor) then
		begin
			_baseColor := newColor;
			_Recalculate(part_Color);
		end;
	end;

	procedure VirtualLight._SetCoeff(const newCoeff: float);
	begin
		if not Equals(_coeff, newCoeff) then
		begin
			_coeff := newCoeff;
			_Recalculate(part_Coeff);
		end;
	end;

	procedure VirtualLight._AbandonGroup(counterpartRef: ppLight);
	begin
		if Assigned(_group) then
		begin
			_group^.Remove(@self, counterpartRef);
			Release(_group);
		end else
			if Assigned(counterpartRef) then
				counterpartRef^ := nil;
	end;

	function VirtualLight._SuitsTo(know: SceneKnowledge): boolean;
	begin
		case know of
			scene_Light: result := yes;
			else
				result := inherited _SuitsTo(know);
		end;
	end;

	procedure VirtualLight._BeforeDetach;
	begin
		_AbandonGroup(nil);
		inherited _BeforeDetach;
	end;

	constructor VirtualLight.Init(newBase: tLightBase; newShadowQuality: sint = 0);
	begin
		inherited Init;
		_base := newBase;
		_shadowQuality := newShadowQuality;
		_baseRadius := 0.0;
		_fadeK := 0.0;
		_baseColor := Vec3.Ones;
		_coeff := 1.0;
		if _base = light_Omni then
		begin
			_startBnd := Bounding.BySphere(Vec3.Zero, 0.0);
			StartBnd := @_startBnd;
		end;
		_Recalculate(part_Color);
		_Recalculate(part_Radius);
		_group := nil;
	end;

	constructor VirtualLight.DeseInit;
	begin
		inherited DeseInit;
		_group := nil;
	end;

	destructor VirtualLight.Done;
	begin
		Release(_group);
		inherited Done;
	end;

	function VirtualLight.GetAttributes: LightAttributes;
	begin
		result          := LightAttributes.Zero;
		result.base     := Base;
		result.position := WorldPos;
		result.color    := FinalColor;
		result.radius   := FinalRadius;
		result.fadeK    := FadeK;
		result.shadowQuality := _shadowQuality;
	end;

	function VirtualLight.Mergeable(var a, b: VirtualLight; const lodA, lodB: float): boolean;
	begin
		result := ((a.shadowQuality > 0) = (b.shadowQuality > 0)) and
			(SqrDistance(a.FinalColor, b.FinalColor) < 1.0 - 0.4 * (lodA + lodB)) and
			(abs(a.FinalRadius - b.FinalRadius) < (a.FinalRadius * (1.0 - 0.5 * lodA) + b.FinalRadius * (1.0 - 0.5 * lodB)));
	end;

	function VirtualLightGroup._CalculateBoundingSphere: Sphere;
	var
		sp: array of Sphere;
		i: sint;
	begin
		SetLength(sp, length(_lts));
		for i := 0 to High(sp) do
			sp[i] := _lts[i].v^.Bnd^.sphere;
		result := Sphere.Bound(sp);
	end;

	constructor VirtualLightGroup.Init(aScene: pSceneNode);
	begin
		inherited Init;
		_mode := mode_NA;
		_lts := nil;
		_one := nil;
		_scene := aScene;
		_corrupted := no;
		_invWtSum := 1.0;
	end;

	destructor VirtualLightGroup.Done;
	var
		i: sint;
	begin
		for i := 0 to High(_lts) do
			if Assigned(_lts[i].startA) then
				dispose(_lts[i].startA);
		inherited Done;
	end;

	procedure VirtualLightGroup.Add(light: pVirtualLight; counterpart: pLight);
{$ifdef Debug}
	var
		i: sint;
{$endif}
	begin
		Assert(_mode = mode_NA);
	{$ifdef Debug}
		for i := 0 to High(_lts) do
			Assert(_lts[i].v <> light);
	{$endif}

		SetLength(_lts, length(_lts) + 1);
		_lts[High(_lts)].v := light;
		_lts[High(_lts)].r := counterpart;
		_lts[High(_lts)].weight := 0.0;
		_invCount := 1.0 / length(_lts);
	end;

	procedure VirtualLightGroup.Remove(light: pVirtualLight; counterpartRef: ppLight = nil);
	var
		cp: pLight;
		i: sint;
	begin
		_corrupted := yes;
		for i := 0 to High(_lts) do
			if _lts[i].v = light then
			begin
				case _mode of
					mode_NA: cp := nil;
					mode_One:
						begin
							if Assigned(counterpartRef) then
							begin
								cp := new(pLight, Init(_one^.GetAttributes.O_Extract(_lts[i].weight, _invWtSum, light^.GetAttributes)));
								cp^.Serializable := no;
								_scene^.Attach(cp);
							end else
								cp := nil;
							if length(_lts) = 1 then
							begin
								_one^.Detach;
								_one := nil;
							end;
						end;
					mode_All: cp := _lts[i].r;
					else Assert(no);
				end;

				if Assigned(_lts[i].startA) then
				begin
					dispose(_lts[i].startA);
					_lts[i].startA := nil;
				end;

				_lts[i] := _lts[High(_lts)];
				SetLength(_lts, length(_lts) - 1);
				if length(_lts) = 0 then _mode := mode_NA;
				// _invCount/_invWtSum не меняется — она нужна для mul_Extract.

				if Assigned(counterpartRef) then
					counterpartRef^ := cp
				else
					if Assigned(cp) then
						cp^.Detach;

				exit;
			end;

		Assert(no);
	end;

	procedure VirtualLightGroup.Update(const dmerge: float);

		procedure _SwitchToOne(const a: LightAttributes);
		var
			i: sint;
		begin
			_mode := mode_One;

			if length(_lts) = 1 then
			begin
				_one := _lts[0].r;
				_lts[0].r := nil;
			end else
			begin
				for i := 0 to High(_lts) do
				begin
					_lts[i].r^.Detach;
					_lts[i].r := nil;
				end;

				_one := new(pLight, Init(a));
				_one^.Serializable := no;
				_scene^.Attach(_one);
			end;
		end;

	var
		bs: Sphere;
		i: sint;
		a, b: LightAttributes;
		invRadius, wtSum, weight: float;
		aime: boolean;
	begin
		Assert(length(_lts) > 0);

		if length(_lts) = 1 then
		begin
			a := _lts[0].v^.GetAttributes;
			_lts[0].weight := 1.0;
			_invWtSum := 1.0;
		end else
		begin
			bs := _CalculateBoundingSphere;
			if NotZero(bs.radius) then invRadius := 1.0 / bs.radius else invRadius := 1.0;

			wtSum := 0.0;
			a := LightAttributes.Zero;
			a.base := _lts[0].v^.base;
			a.position := bs.center;
			a.radius   := bs.radius;
			for i := 0 to High(_lts) do
			begin
				b := _lts[i].v^.GetAttributes;
				weight := sqr(b.radius * invRadius);
				wtSum += weight;

				_lts[i].weight := weight;
				a.A2O_Add(b, weight);
			end;
			if NotZero(wtSum) then _invWtSum := 1.0 / wtSum else _invWtSum := 1.0;
			a.A2O_Fix(_invWtSum);
		end;

		if _mode = mode_NA then
		begin
			for i := 0 to High(_lts) do
				if Assigned(_lts[i].r) then
				begin
					new(_lts[i].startA);
					_lts[i].startA^ := _lts[i].r^.GetAttributes;
				end else
				begin
					_lts[i].startA := nil;
					_lts[i].r := new(pLight, Init(_lts[i].v^.GetAttributes));
					_lts[i].r^.Serializable := no;
					_scene^.Attach(_lts[i].r);
				end;
			_mode := mode_All;
			_mergeDegree := 0.0;

			aime := yes;
			for i := 0 to High(_lts) do
				if Assigned(_lts[0].startA) then
				begin
					aime := no;
					break;
				end;

			if aime then
			begin
				_SwitchToOne(a);
				exit;
			end;
		end;

		case _mode of
			mode_All:
				begin
					if _mergeDegree < 1.0 then
					begin
						_mergeDegree := min(_mergeDegree + dmerge, 1.0);
						if _mergeDegree = 1.0 then
						begin
							_SwitchToOne(a);
							exit;
						end;
					end;

				// log('md = ' + ToString(_mergedegree));

					for i := 0 to High(_lts) do
					begin
						if Assigned(_lts[i].startA) then
							b := _lts[i].startA^
						else
							b := _lts[i].v^.GetAttributes;

						_lts[i].r^.ApplyAttributes(LightAttributes.Mix(b, a.O_Extract(_lts[i].weight, _invWtSum, b), smoothstep(_mergeDegree)));
						// Log('ALL' + ToString(i) + ': ' + ToString(_lts[i].r^.color));
					end;
				end;
			mode_One:
				begin
					_one^.ApplyAttributes(a);
					// Log('ONE: ' + ToString(_one^.color));
				end;
		end;
	end;

	function VirtualLightGroup.Matches(const list: array of pVirtualLight): boolean;
	var
		i, j: sint;
		ok: boolean;
	begin
		result := no;
		if length(list) <> length(_lts) then exit;

		for i := 0 to High(list) do
		begin
			ok := no;
			for j := 0 to High(_lts) do
				if _lts[j].v = list[i] then
				begin
					ok := yes;
					break;
				end;
			if not ok then exit;
		end;
		result := yes;
	end;

	procedure Light._Initialize(newBase: tLightBase; newShadowResolution: sint; des: boolean);
	const
		Targets: array[tLightBase] of GLTextureTarget = (GLtexture_Cube, GLtexture_2D);
	var
		i: sint;
		nsh: sint;
		scenario: pRenderScenario;
	begin
		_baseShadowResolution := newShadowResolution;
		_base := newBase;

		if (_base = light_Targeted) and (newShadowResolution <> 0) and not gl.CSMRecommended then
		begin
		{$ifdef Debug} Log('Targeted @ ' + ToString(newShadowResolution) + ': тень выключена', logWarning); {$endif}
			newShadowResolution := 0;
		end;

		coeff := 1.0;
		color := Vec3.Make(1.0);
		case _base of
			light_Omni: position := Vec3.Zero;
			light_Targeted: position := Vec3.PositiveY;
		end;

		id := 0;
		for i := 0 to High(_sm) do _sm[i] := nil;
		if (_base = light_Omni) and (not des) then
		begin
			_startBnd := Bounding.BySphere(Vec3.Zero, 0.0);
			StartBnd := @_startBnd;
			_radius := -1.0; Radius := 1.0;
			_fadeK := 0.0;
		end;
		_csmLazyUpd := nil;

		if newShadowResolution > 0 then
		begin
			case _base of
				light_Omni:
					begin
						nsh := 1;
						scenario := @ShadowScenario;
					end;
				light_Targeted:
					begin
						nsh := GLBase.Config.nCsmSplits;
						scenario := @TargShadowScenario;
					end;
				else Assert(no);
			end;
			for i := 0 to nsh-1 do
			begin
				_sm[i] := MakeRef(new(pRTTOperator, Init(LightBaseIds[_base] + '_shadow' + ToString(i),
					scenario, Targets[_base], ShadowFormat,
					[newShadowResolution, newShadowResolution], [rtop_PredefSize])));
				_sm[i]^.Serializable := no;
				Attach(_sm[i]);
				if _base = light_Targeted then
				begin
					_sm[i]^.camera.ProjectionMode := proj_Ortho;
					_sm[i]^.camera.DenyProjectionUpdateOnFly;
					if nsh > 1 then _sm[i]^.Quality := 1.0 - 0.5 * i / (nsh - 1);
				end;
			end;
			if _base = light_Targeted then new(_csmLazyUpd);
		end;
		_first := yes;
	end;

	constructor Light.Init(newBase: tLightBase; newShadowResolution: sint = 0);
	begin
		inherited Init;
		_Initialize(newBase, newShadowResolution, no);
	end;

	constructor Light.Init(const a: LightAttributes);
	begin
		Init(a.base, a.shadowQuality);
		ApplyAttributes(a);
	end;

	destructor Light.Done;
	var
		i: sint;
	begin
		for i := 0 to High(_sm) do Release(_sm[i]);
		if Assigned(_csmLazyUpd) then dispose(_csmLazyUpd);
		inherited Done;
	end;

	function Light.GetAttributes: LightAttributes;
	begin
		result          := LightAttributes.Zero;
		result.base     := _base;
		result.position := WorldPos;
		result.color    := color;
		result.radius   := radius;
		result.fadeK    := FadeK;
		result.shadowQuality := _baseShadowResolution;
	end;

	procedure Light.ApplyAttributes(const a: LightAttributes);
	var
		t: Transform;
	begin
		Assert(_base = a.base);

		if GlobalTransform.tr <> a.position then
		begin
			t := GlobalTransform;
			t.tr := a.position;
			GlobalTransform := t;
		end;
		color := a.color;
		radius := a.radius;
		fadeK := a.fadeK;
	end;

	function Light.Detail: tLightDetail;
	begin
		case _base of
			light_Omni:
				if Assigned(_sm[0]) then
					result := light_OmniS
				else
					result := light_OmniA;
			light_Targeted:
				if Assigned(_sm[0]) then
					result := light_TargetedS
				else
					result := light_TargetedA;
		end;
	end;

	function __AddToLightArea(const obj: pSceneNode; param: pointer): boolean;
	var
		lt: pLight absolute param;
	begin
		result := yes;
		if TypeOf(obj^) = TypeOf(RenderObject) then
			case lt^.Detail of
				light_OmniA: Include(pRenderObject(obj)^.ltOmniA, lt^.id);
				light_OmniS: Include(pRenderObject(obj)^.ltOmniS, lt^.id);
			end;
	end;

	procedure Light._OnUpdateVis;
	begin
		inherited _OnUpdateVis;
		case _base of
			light_Omni:
				begin
					_transformedPosition := globalTransform * position;
					_realRadius := _radius * coeff * WorldScale;
					pScene(Root)^.Query(bnd^, @__AddToLightArea, @self);
				end;
			light_Targeted:
				begin
					_transformedPosition := (WorldRot * position).MaybeNormalized;
					position := position.MaybeNormalized;
					if Assigned(_sm[0]) then _UpdateCSM;
				end
		{$ifdef Debug} else Assert(no); {$endif}
		end;
		_realColor := color * coeff;
	end;

	function Light._SuitsTo(know: SceneKnowledge): boolean;
	begin
		case know of
			scene_OmniVisibleWithoutCulling: result := yes;
			else
				result := inherited _SuitsTo(know);
		end;
	end;

	procedure Light._UpdateCSM;
	const
		MinDistK = 0.005;
	var
		cam, c2: pCamera;
		bias: float;
		fpts, spts: Frustum.EightPoints;
		fmin, fmax, smin, smax: Vec3;
		f: Frustum;
		it: Transform;
		si, i: sint;
		xn, xf, yn, yf, zn, zf, camZFar: float;
	begin
		cam := @pScene(Root)^.camera;
		camZFar := cam^.ZFar;

		for si := 0 to cam^.NSplits - 1 do
		begin
			if (_first) or
				(SqrDistance(cam^.pos, _csmLazyUpd^[si].lastCamPos) > sqr(_csmLazyUpd^[si].frustumRelSize * MinDistK)) or
				((cam^.viewTransform.rot.v4 - _csmLazyUpd^[si].lastCamRot.v4).SqrLength > 0.004 + 0.008*si/GLBase.Config.nCsmSplits)
			then
			begin
				_csmLazyUpd^[si].lastCamPos := cam^.pos;
				_csmLazyUpd^[si].lastFrustum := Frustum.FromMatrix(cam^.ProjectionMatrix * cam^.viewTransform.ToMatrix);
				_csmLazyUpd^[si].lastCamRot := cam^.viewTransform.rot;
			end;
			f := _csmLazyUpd^[si].lastFrustum;

			if si > 0 then
				f.planes[Near].d := f.planes[Near].d + cam^.Split(si-1);
			if si < GLBase.Config.nCsmSplits-1 then
				f.planes[Far].d := f.planes[Far].d - camZFar + cam^.Split(si);

			fpts := f.GetEightPoints;
			spts := pScene(Root)^.GetAABB.GetEightPoints;

			_sm[si]^.LocalRot := Quaternion.RotationThroughX0Z(Vec3.NegativeZ, -position);
			it := (globalTransform * Rotate(_sm[si]^.LocalRot)).Inversed;

			for i := 0 to High(fpts) do
			begin
				spts[i] := it * spts[i];
				fpts[i] := it * fpts[i];
				if i = 0 then
				begin
					smin := spts[i];
					smax := spts[i];
					fmin := fpts[i];
					fmax := fpts[i];
				end else
				begin
					fmin := min(fmin, fpts[i]);
					fmax := max(fmax, fpts[i]);
					smin := min(smin, spts[i]);
					smax := max(smax, spts[i]);
				end;
			end;
			xn := max(fmin.x, smin.x);
			xf := min(fmax.x, smax.x);
			zf := max(fmin.z, smin.z);
			zn := smax.z;
			yn := max(fmin.y, smin.y);
			yf := min(fmax.y, smax.y);

			_csmLazyUpd^[si].frustumRelSize := 0.5 *(abs(xn - xf) + abs(yn - yf));

			c2 := @_sm[si]^.camera;
			bias := _csmLazyUpd^[si].frustumRelSize * MinDistK;
			c2^.zNear := zn + bias;
			c2^.zFar := zf - bias;
			c2^.OrthoRect := Rect.Make(Vec2.Make(xn - bias, yn - bias), Vec2.Make(xf + bias, yf + bias));
		end;
		_first := no;
	end;

	function Light._GetShadow(id: sint): pRTTOperator;
	begin
		result := _sm[id];
	end;

	function Light._GetNShadows: sint;
	var
		i: sint;
	begin
		result := 0;
		for i := 0 to High(_sm) do
			if Assigned(_sm[i]) then
				inc(result)
			else
				break;
	end;

	procedure Light._SetShadowQuality;
	const
		BaseQuality = 0.5;
	var
		i: sint;
		n: sint;
	begin
		n := _GetNShadows;
		for i := 0 to n - 1 do
			_sm[i]^.Quality := BaseQuality * (0.5 + 0.5 * (1.0 - i/n));
	end;

	procedure Light._AfterAttach;
	begin
		inherited _AfterAttach;
		pScene(Root)^.lights.Add(@self);
	end;

	procedure Light._BeforeDetach;
	begin
		pScene(Root)^.lights.Remove(@self);
		inherited _BeforeDetach;
	end;

	procedure SceneLights.Initialize;
	var
		ld: tLightDetail;
	begin
		for ld in tLightDetail do
			v[ld] := nil;
	end;

	procedure SceneLights.Finalize;
	var
		ld: tLightDetail;
	begin
		for ld in tLightDetail do
			v[ld] := nil;
	end;

	procedure SceneLights.Add(newItem: pLight);
	var
		ld: tLightDetail;
		i: sint;
	begin
		ld := newItem^.Detail;
		for i := 0 to High(v[ld]) do
			if v[ld, i] = newItem then exit;
		newItem^.id := length(v[ld]) mod (sint(High(LightID)) + 1);
		SetLength(v[ld], length(v[ld]) + 1);
		v[ld][High(v[ld])] := newItem;
	end;

	procedure SceneLights.Remove(item: pLight);
	var
		ld: tLightDetail;
		i: sint;
	begin
		ld := item^.Detail;
		for i := High(v[ld]) downto 0 do
			if v[ld, i] = item then
			begin
				v[ld, i] := v[ld, High(v[ld])];
				SetLength(v[ld], length(v[ld]) - 1);
				if i < length(v[ld]) then
					v[ld, i]^.id := i mod (sint(High(LightID)) + 1);
				break;
			end;
	end;

	procedure Light._SetRadius(newRadius: float);
	begin
		if (_base <> light_Omni) or (newRadius = _radius) then exit;
		_radius := newRadius;
		ChangeStartBounding(Bounding.BySphere(Vec3.Zero, radius));
	end;

type
	pLiQuBuf = ^tLiQuBuf;
	tLiQuBuf = record
		n: sint;
		buf: array of pVirtualLight;
		exclude: pVirtualLight;
	end;

	function __TraverseLight(const node: pSceneNode; param: pointer): boolean;
	var
		light: pVirtualLight absolute node;
		buf: pLiQuBuf absolute param;
	begin
		result := yes;
		Assert(TypeOf(light^) = TypeOf(VirtualLight));
		if light = buf^.exclude then exit;
		buf^.buf[buf^.n] := light;
		inc(buf^.n);
	end;

	// TODO: оптимизировать, блеать!!!
	procedure UpdateVirtualLights(aScene: pSceneNode; const dt: float);
	var
		scene: pScene;
		know: pSceneKnowledgePart;

		lights: array of record
			mergeRadius, lod: float;
			group: sint;
		end;

		groups: array of record
			representation: array of pVirtualLight;
		end;

		procedure traverse(light: pVirtualLight; lid, gid: sint);
		var
			bnd: Bounding;
			buf: tLiQuBuf;
			i, kid: sint;
		begin
			lights[lid].group := gid;
			SetLength(groups[gid].representation, length(groups[gid].representation) + 1);
			groups[gid].representation[High(groups[gid].representation)] := light;

			if not GLBase.Config.mergeLights then exit;
			bnd := Bounding.BySphere(light^.WorldPos, lights[lid].mergeRadius);
			SetLength(buf.buf, know^.list.n);
			buf.n := 0;
			buf.exclude := light;
			know^.kd.ForEachIntersected(bnd, @__TraverseLight, @buf);
			for i := 0 to buf.n - 1 do
			begin
				kid := scene^.RememberedID(buf.buf[i], scene_Light);
				Assert(kid >= 0);
				if SqrDistance(buf.buf[i]^.WorldPos, light^.WorldPos) > sqr(lights[lid].mergeRadius + lights[kid].mergeRadius) then continue;
				if not VirtualLight.Mergeable(light^, buf.buf[i]^, lights[lid].lod, lights[kid].lod) then continue;
				if lights[kid].group < 0 then
					traverse(pVirtualLight(know^.list.items[kid]), kid, gid);
			end;
		end;

	var
		i, j, gid: sint;
		light: pVirtualLight;
		cg: pVirtualLightGroup;
		counterpart: pLight;
		matches: boolean;
	begin
		scene := pScene(aScene);
		know  := @scene^.knowledge[scene_Light];

		SetLength(lights, know^.list.n);
		groups := nil;

		for i := 0 to sint(know^.list.n) - 1 do
		begin
			light := pVirtualLight(know^.list.items[i]);
			lights[i].lod := light^.CalculateLOD(scene^.camera.pos);
			lights[i].mergeRadius := 0.3 + 0.4 * light^.FinalRadius / max(lights[i].lod, 0.1);
			lights[i].group := -1;
		end;

		for i := 0 to sint(know^.list.n) - 1 do
		begin
			if lights[i].group >= 0 then continue;
			gid := length(groups);
			SetLength(groups, gid + 1);
			groups[gid].representation := nil;

			light := pVirtualLight(know^.list.items[i]);
			traverse(light, i, gid);
		end;

		// Log('Groups: ' + ToString(length(groups)));
		for i := 0 to High(groups) do
		begin
			cg := groups[i].representation[0]^._group;
			matches := Assigned(cg) and (not cg^._corrupted) and cg^.Matches(groups[i].representation);
			if matches then
				for j := 1 to High(groups[i].representation) do
					if groups[i].representation[j]^._group <> cg then
					begin
						matches := no;
						break;
					end;

			if matches then
			begin
				// Log('G' + ToString(i) + ' matches!');
			end else
			begin
				// Log('CNG!');
				cg := new(pVirtualLightGroup, Init(scene));
				for j := 0 to High(groups[i].representation) do
				begin
					light := groups[i].representation[j];
					light^._AbandonGroup(@counterpart);
					light^._group := MakeRef(cg);
					cg^.Add(light, counterpart);
				end;
			end;

			cg^.Update(0.8 * dt);
		end;
	end;

	procedure GetLightingPasses(var lights: SceneLights; const omniA, omniS: LightsSet; out lp: tLightingPassesInfo);
		function MakeNewPass: boolean;
		var
			ld: tLightDetail;
		begin
			result := lp.count < MaxLightingPasses;
			if not result then exit;
			for ld in tLightDetail do
			begin
				lp.passes[lp.count].byDetail[ld].n := 0;
			end;
			lp.passes[lp.count].estimation := 0;
			inc(lp.count);
		end;
	var
		i, j, pass: sint;
		ld: tLightDetail;
		ne: uint;
	begin
		lp.count := 0;
		MakeNewPass;
		for ld in tLightDetail do
		begin
			for i := 0 to High(lights.v[ld]) do
			begin
				case ld of
					light_OmniA: if not (lights.v[ld, i]^.id in omniA) then continue;
					light_OmniS: if not (lights.v[ld, i]^.id in omniS) then continue;
				end;
				pass := -1;
				for j := 0 to lp.count do
				begin
					if j = lp.count then
						if not MakeNewPass then break;
					ne := lp.passes[j].estimation + GLBase.Config.lightEstimation[ld];
					if (ne <= GLBase.Config.maxLightsEstimation) and (lp.passes[j].byDetail[ld].n < MAX_LIGHTS[ld]) then
					begin
						pass := j;
						lp.passes[j].estimation := ne;
						break;
					end;
				end;
				if pass = -1 then break;
				inc(lp.passes[pass].byDetail[ld].n);
				lp.passes[pass].byDetail[ld].list[lp.passes[pass].byDetail[ld].n - 1] := lights.v[ld, i];
			end;
		end;
	end;

	procedure PrepareLightingPass(const lp: tLightingPassesInfo; passNo: sint; var cam: Camera);

		procedure set_sfif(var u: NativeGLValue; var light: Light; i: sint);
		var
			startFade, invFade: float;
		begin
			startFade := light._realRadius * light.FadeK;
			invFade := 1.0 / (CloseToZeroEps + light._realRadius * (1.0 - light.FadeK));
			u.SetVec2(Vec2.Make(startFade, invFade), i);
		end;

	var
		i, j, n: sint;
		curLt: pLight;
		ld: tLightDetail;
		lu: tLightUniform;
		ldus: tLightUniforms;
		ids: array[tLightUniform] of sint;
	begin
		if passNo = 0 then
			UFirstLtPassFactor^.SetFloat(1.0)
		else
			UFirstLtPassFactor^.SetFloat(0.0);
		for lu in tLightUniform do
			ids[lu] := -1;
		ULights.NOmni^.SetInt(lp.passes[passNo].byDetail[light_OmniA].n);
		ULights.NTarg^.SetInt(lp.passes[passNo].byDetail[light_TargetedA].n);

		for ld in tLightDetail do
		begin
			n := lp.passes[passNo].byDetail[ld].n;

			ldus := LightDetailInfo[ld].u;
			for i := 0 to n - 1 do
			begin
				curLt := lp.passes[passNo].byDetail[ld].list[i];
				curLt^.UpdateVis;

				for lu in tLightUniform do
					if lu in ldus then
						inc(ids[lu]);

				case curLt^.base of
					light_Omni: ULights.Pos^.SetVec3(cam.viewTransform * curLt^._transformedPosition, ids[ulight_All]);
					light_Targeted: ULights.Pos^.SetVec3(cam.viewTransform.rot * curLt^._transformedPosition, ids[ulight_All]);
					else Assert(no);
				end;
				ULights.Color^.SetVec3(curLt^._realColor, ids[ulight_All]);

				if ulight_Omni in ldus then
					set_sfif(UOmni.StartFadeInvFade^, curLt^, ids[ulight_Omni]);

				if ulight_OmniS in ldus then
				begin
					UOmniS.MPos^.SetVec3(curLt^._transformedPosition, ids[ulight_OmniS]);
					UOmniS.SM[ids[ulight_OmniS]]^.SetTex(curLt^._sm[0]^.colors[0].tex);
				end;

				if ulight_TargetedS in ldus then
				begin
					for j := 0 to GLBase.Config.nCsmSplits-1 do
					begin
						UTargS.PVmat[j]^.SetMat4(curLt^._sm[j]^.camera.ProjectionMatrix * curLt^._sm[j]^.camera.viewTransform.ToMatrix, ids[ulight_TargetedS]);
						UTargS.SM[ids[ulight_TargetedS], j]^.SetTex(curLt^._sm[j]^.colors[0].tex);
					end;
				end;
			end;

			if CanUseUniFor(ld) and (n < MAX_LIGHTS[ld]) and ((n > 0) or GLBase.Config.keepZeroUniFor) then
				for lu in tLightUniform do
					if lu in ldus then
						inc(ids[lu], MAX_LIGHTS[ld] - n);
		end;
	end;

	function CanUseUniFor(ld: tLightDetail): boolean;
	begin
		case ld of
			light_OmniA, light_TargetedA: result := gl.UniForSupported;
			else result := no;
		end;
	end;

{$ifdef use_serialization}
	procedure SerializeVirtualLight(se: pSerializer; obj: pointer);
	var
		lt: pVirtualLight absolute obj;
	begin
		with se^ do
		begin
			Serialize_ui8(stream, ord(lt^._base));
			Serialize_ui16(stream, lt^._shadowQuality);
			Serialize_f32(stream, lt^.BaseRadius);
			Serialize_f16(stream, lt^.FadeK);
			Serialize_vec3f32(stream, lt^.BaseColor);
			Serialize_f16(stream, lt^.Coeff);
		end;
	end;

	procedure DeserializeVirtualLight(de: pDeserializer; obj: pointer);
	var
		lt: pVirtualLight absolute obj;
	begin
		with de^ do
		begin
			lt^._base := tLightBase(Deserialize_ui8(stream));
			lt^._shadowQuality := Deserialize_ui16(stream);
			lt^._baseRadius := Deserialize_f32(stream);
			lt^._fadeK := Deserialize_f16(stream);
			lt^._baseColor := Deserialize_vec3f32(stream);
			lt^._coeff := Deserialize_f16(stream);
		end;
	end;

	procedure VirtualLightDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		lt: pVirtualLight absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: lt^.DeseInit;
			de_After:
				begin
					lt^._startBnd := Bounding.ByPoint(Vec3.Zero);
					lt^.StartBnd := @lt^._startBnd;
					lt^._Recalculate(part_Coeff);
				end;
		end;
	end;

	procedure SerializeLight(se: pSerializer; obj: pointer);
	var
		lt: pLight absolute obj;
	begin
		with se^ do
		begin
			Serialize_ui8(stream, ord(lt^._base));
			Serialize_ui16(stream, lt^._baseShadowResolution);
			Serialize_f32(stream, lt^._radius);
			Serialize_f16(stream, lt^._fadeK);
			Serialize_vec3f32(stream, lt^.position);
			Serialize_vec3f32(stream, lt^.color);
			Serialize_f16(stream, lt^.coeff);
		end;
	end;

	procedure DeserializeLight(de: pDeserializer; obj: pointer);
	var
		lt: pLight absolute obj;
	begin
		with de^ do
		begin
			lt^._base := tLightBase(Deserialize_ui8(stream));
			lt^._baseShadowResolution := Deserialize_ui16(stream);
			lt^._Initialize(lt^._base, lt^._baseShadowResolution, yes);
			lt^._radius  := Deserialize_f32(stream);
			lt^._fadeK   := Deserialize_f16(stream);
			lt^.position := Deserialize_vec3f32(stream);
			lt^.color    := Deserialize_vec3f32(stream);
			lt^.coeff    := Deserialize_f16(stream);
		end;
	end;

	procedure LightDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		lt: pLight absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: lt^.DeseInit;
		end;
	end;
{$endif}

	procedure Init;
	begin
	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Virtual Light', TypeOf(VirtualLight), TypeOf(SceneNode), sizeof(VirtualLight), yes,
		               @SerializeVirtualLight, @DeserializeVirtualLight, nil, @VirtualLightDeSpecial)
		^.RegisterType('Light', TypeOf(Light), TypeOf(SceneNode), sizeof(Light), yes,
		               @SerializeLight, @DeserializeLight, nil, @LightDeSpecial);
	{$endif}
	end;

initialization
	&Unit('Lights').Initialize(@Init);
end.
