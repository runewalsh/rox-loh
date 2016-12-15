unit GLClasses;

{$include opts.inc}
{-$define DebugImageFlip}
{$ifdef Debug}
	{-$define ExtDebug}
	{-$define DebugDrawables}
	{$define DebugInterleaving}
{$endif}

interface

uses
	USystem, Errors, UClasses, UMath, Utils, Tokenizer, Streams, Script, Algo,
	U_GL, SpatialIndex, GLBase, Human
{$ifdef Debug}, ULog {$endif};

const
	DefaultMinLOD = 0.001;
	MaxLightingPasses = 6;
	GeometryShaderDefine = 'GEOMETRY';
	VAOLifetime = 33.0;
	ShaderFilenameExtension = '影';
	ShaderVaPrefix = 'a_';
	GL_FLAG_CLIP_PLANE = UserShaderFlags.PUBLIC_BUILTIN + 0;
	GL_FLAG_UNDERWATER = UserShaderFlags.PUBLIC_BUILTIN + 1;

type
	TextureMode = (texture_Static, texture_Dynamic, texture_External);

	pTexture = ^tTexture;
	tTexture = object(&Object)
	private
		_mode: TextureMode;
		_loaded: boolean;
		_wrap: GLTextureWrapMode;
		_swizzle: SwizzlePack;
		procedure _Init(newIm: pTextureImage; newTarget: GLTextureTarget; const newSize: UintVec3; newFormat: GLImageFormat; imageFlags: TextureImageFlags; newMode: TextureMode);
		procedure _Load2vmem(const data: array of pointer);
		procedure _SetWrap(newWrap: GLTextureWrapMode);
		procedure _SetSwizzle(const newSwizzle: SwizzlePack);
	public
		info: TextureImageInfo;
		inGL: pGLTexture;
		constructor Init(s: pStream; flags: TextureImageFlags = []; const forceLoader: string = ''; size: size_t = 0);
		constructor Init(newIm: pTextureImage);
		constructor Init(newTarget: GLTextureTarget; const newSize: UintSize3; newFormat: GLImageFormat; imageFlags: TextureImageFlags; newMode: TextureMode);
		destructor Done; virtual;
		procedure Resize(const newSize: UintSize3);
		procedure EnsureLoaded;
		procedure SubImage(var im: TextureImage; const offset: UintOffset3; level: sint = 0);
		procedure SubImage(const offset: UintOffset3; const size: UintSize3; format: GLImageFormat; dataSize: size_t; data: pointer; level: sint = 0; takeThisData: boolean = no);
		procedure Save(const fileName: string; level: sint);

		property Target: GLTextureTarget read info.target write info.target;
		property Format: GLImageFormat read info.format write info.format;
		property Size: UintVec3 read info.size;
		property nLevels: uint read info.nLevels;

		property Wrap: GLTextureWrapMode read _wrap write _SetWrap;
		property Swizzle: SwizzlePack read _swizzle write _SetSwizzle;

	public
	{$ifdef DebugImageFlip} FlipsTime: Ticks; static; FlipsCount: uint; static; {$endif}
	end;

type
	PoolTextureRec = record
		tex: pTexture;
		nQueries: sint;
	end;
	PoolTexturesArray = array of PoolTextureRec;

	RenderTexturesPool = object
	private
		_share: PoolTexturesArray;
	{$ifdef Debug}
		_maxQueries: sint;
		procedure _LogAboutNewTexture(tex: pTexture; const msgPrefix: string);
	{$endif}
	public
		procedure Init;
		procedure Done;
		function QueryShared(target: GLTextureTarget; sizeX, sizeY: uint; format: GLImageFormat): pTexture;
		procedure Return(var tex: pTexture);
	end;

	RenderTargetDepthBehaviour =
	(
		rt_NoDepth,
		rt_ExclusiveDepth,
		rt_SharedDepth
	);

const
	RenderTargetDepthBehaviourIds: array[RenderTargetDepthBehaviour] of string =
		('none', 'exclusive', 'shared');

type
	pRenderTarget = ^tRenderTarget;
	tRenderTarget = object
	private
		_target: GLTextureTarget;
		_size: UintVec2;
		_main: boolean;
		_depthBehaviour: RenderTargetDepthBehaviour;
		_activeSublevel: uint;
		_clearColor: Vec4;
	{$ifdef Debug} function _Validate(alsoReportIfOk: boolean): boolean; {$endif}
	public
		inGL: pGLRenderTarget;
		color: array of record
			tex: pTexture;
			format: GLImageFormat;
		end;
		depth: pTexture;
		name: PoolString;
		constructor Init(const newName: PoolString; newTarget: GLTextureTarget; newFormats: array of GLImageFormat; const newSize: UintVec2; newDb: RenderTargetDepthBehaviour);
		constructor InitMain(const newSize: UintVec2);
		destructor Done;
		procedure Resize(const newSize: UintVec2);
		procedure SwitchToSublevel(sublv: uint);
		procedure Prepare;
		procedure Discard;
		function Buffers: GLRenderBuffers;
		procedure Clear;
		procedure Clear(what: GLRenderBuffers);

		property Target: GLTextureTarget read _target;
		property Main: boolean read _main;
		property Size: UintVec2 read _size;
		property ClearColor: Vec4 read _clearColor write _clearColor;
	end;

	tParams4Renderable = object
		pass: pRenderPass;
		view: pTransform;
		rt: pGLRenderTarget;
		mtlParams, matLevelParams, roParams: pGLEntityParams;
		useMatBlend: boolean;
		sh: ShaderFlags;
		nInsta: uint;
		procedure Reset;
		procedure Finalize;
	end;

	ppShader = ^pShader;
	pShader = ^tShader;

	pSingleShader = ^tSingleShader;
	tSingleShader = object(&Object)
	public type
		tSourceHash = object
			len: uint;
			crc64: uint64;
			function Create(const source: string): tSourceHash; static;
			function Hash(const h: tSourceHash): Hash.Value; static;
			function Equals(const a, b: tSourceHash): boolean; static;
		end;

		tPrepared = record
			parent: pShader;
			name, source: string;
			hash: tSourceHash;
			params: ShaderFlags;
		end;
	private
		_inGL: pGLShader;
		_params: array of ShaderFlags;
		_hash: tSourceHash;
		_parent: pShader;
	{$ifdef Debug} _name: string; {$endif}
	public
		function Prepare(parent: pShader; const params: ShaderFlags; chainId: sint): tPrepared; static;
		constructor Init(const prep: tPrepared);
		destructor Done; virtual;
	end;

	tShader = object(&Object)
	private type
		{$define classname:=tHash_Flags2SingleShader} {$define key_type:=ShaderFlags} {$define value_type:=pSingleShader}
		{$include hash.h.inc}

		{$define classname:=tHash_Source2SingleShader} {$define key_type:=tSingleShader.tSourceHash} {$define value_type:=pSingleShader}
		{$include hash.h.inc}
	private var
		_st: ShaderType;
		name: PoolString;
		_sh: tHash_Flags2SingleShader;
		_s2sh: tHash_Source2SingleShader;
	public
		tok: ShaderParser;
		constructor Init(const name, source: string; aType: sint = -1);
		constructor Init(s: pStream);
		destructor Done; virtual;
		function Request(const f: ShaderFlags; const prep: tSingleShader.tPrepared): pSingleShader;
		function FileName(const base: string): string; static;

		property Type_: ShaderType read _st;
	end;

	pShaderProgram = ^ShaderProgram;

	pSingleShaderProgram = ^SingleShaderProgram;
	SingleShaderProgram = object(&Object)
	private type
		pUboUniform = ^tUboUniform;
		tUboUniform = object
			name: PoolString;
			ver: NativeGLValue.tVersion;
			inGL: GLUboUniform;
		end;

		tUbo = object
			inGL: pGLBuffer;
			dataSize: size_t;
			data, oldData: pointer;
			uni: array of tUboUniform;
			procedure Init(const ubo: GLUboDesc);
			procedure Done;
			procedure Send;
		end;
	private var
		_parent: pShaderProgram;
		_inGL: pGLProgram;
		shaders: array of pSingleShader;
		_uniforms: array of record
			inGL: GLUniform;
			name: PoolString;
			ver: NativeGLValue.tVersion;
		end;
		_ubos: array of tUbo;
		_params: array of ShaderFlags;
	{$ifdef Debug} _name: PoolString; {$endif}
		function _FindUniform(var rp: tParams4Renderable; const name: PoolString): pNativeGLValue;
		procedure _SendUniforms(var rp: tParams4Renderable);
	public
		attrs: array of record
			namae: PoolString;
			index: sint;
		{$ifdef Debug} typ: GLType; {$endif}
		end;
		constructor Init(theParent: pShaderProgram; const params: ShaderFlags; const prep: array of tSingleShader.tPrepared);
		destructor Done; virtual;
		procedure Bind(var rp: tParams4Renderable);
	end;

	ShaderProgram = object(&Object)
	private type
		{$define classname:=tHash_Flags2SingleShaderProgram} {$define key_type:=ShaderFlags} {$define value_type:=pSingleShaderProgram}
		{$include hash.h.inc}
	private var
		name: PoolString;
		shaders: array of pShader;
		_mask: ShaderFlags;
		_p: tHash_Flags2SingleShaderProgram;
		function _Request(const p: ShaderFlags): pSingleShaderProgram;
	public
		constructor Init(const newName: PoolString; const sh: array of pShader);
		constructor Init(s: pStream);
		destructor Done; virtual;
		function Bind(var rp: tParams4Renderable): pSingleShaderProgram;
		function Has(typ: ShaderType): boolean;
		function HasUbo: boolean;
	end;

	pGLMaterial = ^GLMaterial;
	pGLMaterialLevel = ^GLMaterialLevel;
	GLMaterialLevel = object
	type
		ProgForPass = record
			pass: pRenderPass;
			prog: pShaderProgram;
		end;
	private
		procedure _SetProg(pass: pRenderPass; prog: pShaderProgram);
		function _GetProg(pass: pRenderPass): pShaderProgram;
	public
		base: pGLMaterial;
		minLod: float;
		pp: array of ProgForPass;
		cull: boolean;
		blend: GLBlendMode;
		gl: GLEntityParams;
		procedure Initialize(newBase: pGLMaterial; const aMinLod: float; cp: pGLMaterialLevel);
		procedure Finalize;
		function GetProgID(pass: pRenderPass): sint;
		property Progs[pass: pRenderPass]: pShaderProgram read _GetProg write _SetProg;
	end;

	GLMaterial = object(&Object)
	private type
		{$define classname:=tPool} {$define key_type:=pGLMaterial} {$define null_value:=nil} {$include hash.h.inc}
	private var
		_nInstances: sint; static;
		_pool: tPool; static;
		function _Hash(m: pGLMaterial): Hash.Value; static;
		function _Equals(a, b: pGLMaterial): boolean; static;
	private var
		_inPool: boolean;
		procedure _InitInstance;
	public type
		LevelsList = array of pGLMaterialLevel;
	public var
		name: PoolString;
		gl: GLEntityParams;
		lods: array of GLMaterialLevel;
		constructor Init(const newName: PoolString; base: pGLMaterial = nil);
		constructor DeseInit;
		destructor Done; virtual;
		function Merge(m: pGLMaterial): pGLMaterial; static;
		procedure ClearLevels;
		function AddLevel(const aMinLod: float; cp: pGLMaterialLevel = nil): pGLMaterialLevel;
		function GetLevel(const lod: float): pGLMaterialLevel;
		function FetchLevels(const lod: float): LevelsList;
		function AnyLevel: pGLMaterialLevel;
	end;

type
	pGLMesh = ^tGLMesh;

	pGLBatch = ^tGLBatch;
	tGLBatch = object
	private
	type
		tDrawable = object
		private type
			tVatMap = array[0 .. MaxVertexAttributes - 1] of sint;
		private var
			progs: array of pSingleShaderProgram;
			GLvd: pGLVertexDeclaration;
			mapVAT: tVatMap;
			lastTimeDrawn: float;
			function Query(aProg: pSingleShaderProgram; batch: pGLBatch): pGLVertexDeclaration; static;
			procedure Finalize;
			procedure Update(glBatch: pGLBatch);
		{$ifdef Debug} function HumanName(batch: pGLBatch; const dprogs: array of pSingleShaderProgram): string; static; {$endif}
		end;
	var
		_mesh: pGLMesh;
		_batch: pBatch;
		_drawables: array of tDrawable;
		_sweepPos: sint;
		vaVb: array of record
			ofs: size_t;
			vtsAllocated: uint;
		end;
		levelInds: array of record
			ibOffset: size_t;
		end;
		indsLoaded: uint;
		indsLoadedType: GLType;
		_ibOffset: PtrUint;

		_interleave: boolean;
		_vertexLayout: array of record
			offset: size_t;
		end;
		_vbOffset, _vertexSize: size_t;
		_vtsAllocated: uint;

		function _InterleavingRequired: boolean;
		procedure _InterleavingOpen(interleave: boolean);
		procedure _InterleavingClose(interleave: boolean);
		procedure _GetInterleaved(buf: pointer; start: uint; count: sint);
		procedure _Load(vts, ids: boolean);
		procedure _PartialUpdateVA(id: sint; start, count: uint);
		procedure _PartialUpdateInds(start, count: uint);
		procedure _Sweep;
	public
		constructor Init(newBatch: pBatch; newMesh: pGLMesh);
		destructor Done;
		procedure Draw(var rp: tParams4Renderable; level: sint; mat: pGLMaterialLevel);
	{$ifdef Debug} function Human: string; {$endif}

		property Mesh: pGLMesh read _mesh;
		property Batch: pBatch read _batch;
	end;

	pGLMeshLevel = ^tGLMeshLevel;
	tGLMeshLevel = object
		mesh: pGLMesh;
		id: sint;
		procedure Draw(var rp: tParams4Renderable; const mat: array of pGLMaterialLevel);
	end;

	tGLMesh = object(&Object)
	private type
		tUpdateReq = (upd_None, upd_Partial, upd_Whole);

		tPartialUpdateV = record
			batch, iva: sint;
			start, count: uint;
		end;

		tPartialUpdateI = record
			batch: sint;
			start, count: uint;
		end;

	private var
		_mesh: pMesh;
		_dynamic: boolean;
		_glVB, _glIB: pGLBuffer;
		_glVertexBufferSize, _glIndexBufferSize, _loadedGLVertexBufferSize, _loadedGLIndexBufferSize: size_t;
		_levels: array of tGLMeshLevel;

		_updateV, _updateI: tUpdateReq;
		_partialUpdateV: array of tPartialUpdateV;
		_partialUpdateI: array of tPartialUpdateI;

		procedure _FreeData;
		procedure _CalculateOffsets(vts, ids: boolean);
		procedure _LoadInVBO;
		procedure _RefreshInVBO(vts, ids: boolean);
		procedure _EnsureLoaded;
		procedure _AddPartialUpdateV(newBatch, newIva: sint; newStart, newCount: uint);
		procedure _AddPartialUpdateI(newBatch: sint; newStart, newCount: uint);
		procedure _ClearPartialUpdateV;
		procedure _ClearPartialUpdateI;
		procedure _Draw(var rp: tParams4Renderable; const mat: array of pGLMaterialLevel; level: sint);
	public
		batches: array of tGLBatch;
		topology: GLTopology;

		constructor Init(newMesh: pMesh; newDynamic: boolean = no);
		constructor Init(s: pStream; newDynamic: boolean = no);
		destructor Done; virtual;
		function FirstLevel: pGLMeshLevel;
		function GetLevel(const lod: float): pGLMeshLevel;
		procedure Draw(var rp: tParams4Renderable; const mat: array of pGLMaterialLevel);
		procedure PartialChangedVA(batch: sint; v: pNativeGLValue; start, count: uint);
		procedure PartialChangedInds(batch: sint; level: uint; start, count: uint);
		procedure ConvertToStatic;

		procedure Changed;
		property Mesh: pMesh read _mesh;
		property Dynamic: boolean read _dynamic;
	end;

	pPostprocess = ^Postprocess;
	Postprocess = object(&Object)
	private const
		DEST_BASERT = Low(sint);
	private type
		pRTRec = ^RTRec;
		RTRec = object
			name: PoolString;
			size: UintVec2;
			sizeK: float;
			colors: array of record
				format: GLImageFormat;
			end;
			depth: RenderTargetDepthBehaviour;
			depthFrom: sint;
			instance: pRenderTarget;
			procedure Initialize(const newName: PoolString);
			procedure Finalize;
			function ReadFromTokens(var s: tTokenizer; pp: pPostprocess): boolean;
			procedure SetNColors(newN: uint);
		end;

		pPassRec = ^PassRec;
		PassRec = object
			mat: pGLMaterial;
			pvs: array of record
				color: array of pNativeGLValue;
				depth: pNativeGLValue;
			end;
			sources: array of sint;
			dest: sint;
			procedure Initialize(pp: pPostprocess; const newName: PoolString);
			procedure Finalize;
			function ReadFromTokens(var s: tTokenizer; pp: pPostprocess): boolean;
			procedure Complete(pp: pPostprocess);
		end;
	private var
		_rts: array of RTRec;
		_passes: array of PassRec;
	{$ifdef Debug} _name: PoolString; {$endif}
		function _ReadFromTokens(var s: tTokenizer): boolean;
		function _NewRt(const newName: PoolString): pRTRec;
		function _NewPass(const newName: PoolString): pPassRec;
		function _GetRTIdByName(const name: PoolString {$ifdef Debug}; warn: boolean = no {$endif}): sint;
		procedure _Prepare;
		procedure _Discard;
	public
		constructor Init(s: pStream);
		destructor Done; virtual;
		function PreparePass(const size: UintVec2): pRenderTarget;
		procedure Draw(rt: pGLRenderTarget; gl: pGLEntityParams);
	{$ifdef Debug} procedure Dump(const baseFn: string); {$endif}
	end;

	PostprocessRegistry = object
	private
		_reg: array of record
			pp: pPostprocess;
			lastFrameUsed: sint;
		end;
	public
		constructor Init;
		destructor Done;
		procedure Add(newPP: pPostprocess);
		procedure Remove(pp: pPostprocess);
		procedure Pump(pp: pPostprocess);
		procedure Cleanup;
	end;

	procedure GLClasses_Init;
	procedure GLClasses_Done;
	procedure GLOnEndFrame;
	procedure OpenScript(var script: ScriptState);

var
	MainRT: tRenderTarget;
	Quad01, Quad11, Quad01CW: tGLMesh;
	RTpool: RenderTexturesPool;
	Postprocesses: PostprocessRegistry;

	GlobalGL: GLEntityParams;
	UTime, UGUITime, UCamPos, UProjMatrix, UInvProjZW, UMainCamViewZv4,
	USplits,
	UFirstLtPassFactor, UInvNPasses,
	UBonePos, UBoneRot, UInvBonePos, UInvBoneRot,
	UMVTrans, UMVRot, UModelTrans, UModelRot,
	UAABB,
	UClipPlane, UFrameDt: pNativeGLValue;
	UInvGUISizes, UInvDestRTSizes: pNativeGLValue;

	ULights: record
		NOmni, NTarg: pNativeGLValue;
		Pos, Color: pNativeGLValue;
	end;

	UOmni: record
		StartFadeInvFade: pNativeGLValue;
	end;

	UOmniS: record
		MPos: pNativeGLValue;
		SM: array[0 .. MAX_OMNI_S-1] of pNativeGLValue;
	end;

	UTargS: record
		SM: array[0 .. MAX_TARG_S-1] of array[0 .. MAX_CSM_SPLITS-1] of pNativeGLValue;
		PVmat: array[0 .. MAX_CSM_SPLITS-1] of pNativeGLValue;
	end;

implementation

uses
	GLUtils, MMSystem, Script_EngineAPI, Windowing
{$ifdef use_serialization}, Serialization {$endif}
{$ifdef Profile}, Profile {$endif};

	{$define classname:=tShader.tHash_Flags2SingleShader} {$define hash_func:=ShaderFlags.Hash} {$define inline_eq := ShaderFlags.Equals(_1, _2)}
	{$include hash.pp.inc}

	{$define classname:=tShader.tHash_Source2SingleShader}
	{$define hash_func:=tSingleShader.tSourceHash.Hash} {$define inline_eq := tSingleShader.tSourceHash.Equals(_1, _2)}
	{$include hash.pp.inc}

	{$define classname:=ShaderProgram.tHash_Flags2SingleShaderProgram}
	{$define hash_func:=ShaderFlags.Hash} {$define inline_eq := ShaderFlags.Equals(_1, _2)}
	{$include hash.pp.inc}

	{$define classname:=GLMaterial.tPool}
	{$define hash_func:=GLMaterial._Hash} {$define inline_eq := GLMaterial._Equals(_1, _2)}
	{$include hash.pp.inc}

const
	GlobalGLRelocations: array[0 ..
		22 +             // common
		4 +              // ULight
		1 +              // UOmni
		1 + MAX_OMNI_S + // UOmniS
		MAX_TARG_S * MAX_CSM_SPLITS + MAX_CSM_SPLITS // UTargS
		- 1] of record
		reloc: ^pNativeGLValue;
		name: string;
		typ: GLType;
		count: uint;
		flags: NativeGLValueFlags;
	end =
	(
		(reloc: @UTime;              name: 'time';              typ: GLType.Float; count: 1; flags: []),
		(reloc: @UGUITime;           name: 'gui_time';          typ: GLType.Float; count: 1; flags: []),
		(reloc: @UCamPos;            name: 'camPos';            typ: GLType.Vec3;  count: 1; flags: []),
		(reloc: @UProjMatrix;        name: 'projectionMatrix';  typ: GLType.Mat4;  count: 1; flags: []),
		(reloc: @UInvProjZW;         name: 'invProjZW';         typ: GLType.Vec2;  count: 2; flags: []),
		(reloc: @UMainCamViewZv4;    name: 'mainCamViewZv4';    typ: GLType.Vec4;  count: 1; flags: []),
		(reloc: @USplits;            name: 'splits';            typ: GLType.Float; count: MAX_CSM_SPLITS; flags: []),
		(reloc: @UFirstLtPassFactor; name: 'firstLtPassFactor'; typ: GLType.Float; count: 1; flags: []),
		(reloc: @UInvNPasses;        name: 'invNPasses';        typ: GLType.Float; count: 1; flags: []),
		(reloc: @UBonePos;           name: 'bonePos';           typ: GLType.Vec3;  count: MaxGLBones; flags: []),
		(reloc: @UBoneRot;           name: 'boneRot';           typ: GLType.Vec4;  count: MaxGLBones; flags: []),
		(reloc: @UInvBonePos;        name: 'iBonePos';          typ: GLType.Vec3;  count: MaxGLBones; flags: []),
		(reloc: @UInvBoneRot;        name: 'iBoneRot';          typ: GLType.Vec4;  count: MaxGLBones; flags: []),
		(reloc: @UMVTrans;           name: 'mvTrans';           typ: GLType.Vec4;  count: 0; flags: [NativeGLValueFlag.InstaPack]),
		(reloc: @UMVRot;             name: 'mvRot';             typ: GLType.Vec4;  count: 0; flags: [NativeGLValueFlag.InstaPack]),
		(reloc: @UModelTrans;        name: 'modelTrans';        typ: GLType.Vec4;  count: 0; flags: [NativeGLValueFlag.InstaPack]),
		(reloc: @UModelRot;          name: 'modelRot';          typ: GLType.Vec4;  count: 0; flags: [NativeGLValueFlag.InstaPack]),
		(reloc: @UAABB;              name: 'aabb';              typ: GLType.Vec3;  count: 2; flags: []),
		(reloc: @UClipPlane;         name: 'clipPlane';         typ: GLType.Vec4;  count: 1; flags: []),
		(reloc: @UFrameDt;           name: 'frame_dt';          typ: GLType.Float; count: 1; flags: []),
		(reloc: @UInvGUISizes;       name: 'invGUIsizes';       typ: GLType.Vec2;  count: 1; flags: []),
		(reloc: @UInvDestRTSizes;    name: 'invDestRT';         typ: GLType.Vec2;  count: 1; flags: []),

		(reloc: @ULights.NOmni;        name: 'nOmni';           typ: GLType.Int32;  count: 1; flags: []),
		(reloc: @ULights.NTarg;        name: 'nTarg';           typ: GLType.Int32;  count: 1; flags: []),
		(reloc: @ULights.Pos;          name: 'light_pos';       typ: GLType.Vec3;  count: MAX_OMNI_A + MAX_OMNI_S + MAX_TARG_A + MAX_TARG_S; flags: []),
		(reloc: @ULights.Color;        name: 'light_color';     typ: GLType.Vec3;  count: MAX_OMNI_A + MAX_OMNI_S + MAX_TARG_A + MAX_TARG_S; flags: []),

		(reloc: @UOmni.StartFadeInvFade; name: 'omni_sfif';  typ: GLType.Vec2; count: MAX_OMNI_A + MAX_OMNI_S; flags: []),

		(reloc: @UOmniS.MPos;      name: 'omniS_mPos';      typ: GLType.Vec3;    count: MAX_OMNI_S; flags: []),
		(reloc: @UOmniS.SM[0];     name: 'omniS_SM0';       typ: GLType.Sampler; count: 1; flags: []),
		(reloc: @UOmniS.SM[1];     name: 'omniS_SM1';       typ: GLType.Sampler; count: 1; flags: []),
		(reloc: @UOmniS.SM[2];     name: 'omniS_SM2';       typ: GLType.Sampler; count: 1; flags: []),
		(reloc: @UOmniS.SM[3];     name: 'omniS_SM3';       typ: GLType.Sampler; count: 1; flags: []),

		(reloc: @UTargS.SM[0, 0]; name: 'targS_0_SM0';  typ: GLType.Sampler; count: 1; flags: []),
		(reloc: @UTargS.SM[0, 1]; name: 'targS_0_SM1';  typ: GLType.Sampler; count: 1; flags: []),
		(reloc: @UTargS.SM[0, 2]; name: 'targS_0_SM2';  typ: GLType.Sampler; count: 1; flags: []),
		(reloc: @UTargS.SM[0, 3]; name: 'targS_0_SM3';  typ: GLType.Sampler; count: 1; flags: []),
		(reloc: @UTargS.SM[1, 0]; name: 'targS_1_SM0';  typ: GLType.Sampler; count: 1; flags: []),
		(reloc: @UTargS.SM[1, 1]; name: 'targS_1_SM1';  typ: GLType.Sampler; count: 1; flags: []),
		(reloc: @UTargS.SM[1, 2]; name: 'targS_1_SM2';  typ: GLType.Sampler; count: 1; flags: []),
		(reloc: @UTargS.SM[1, 3]; name: 'targS_1_SM3';  typ: GLType.Sampler; count: 1; flags: []),
		(reloc: @UTargS.PVmat[0]; name: 'targS_PVmat0'; typ: GLType.Mat4; count: MAX_TARG_S; flags: []),
		(reloc: @UTargS.PVmat[1]; name: 'targS_PVmat1'; typ: GLType.Mat4; count: MAX_TARG_S; flags: []),
		(reloc: @UTargS.PVmat[2]; name: 'targS_PVmat2'; typ: GLType.Mat4; count: MAX_TARG_S; flags: []),
		(reloc: @UTargS.PVmat[3]; name: 'targS_PVmat3'; typ: GLType.Mat4; count: MAX_TARG_S; flags: [])
	);

	function tSingleShader.tSourceHash.Create(const source: string): tSourceHash;
	begin
		result.len := length(source);
		result.crc64 := Algo.Hash.Crc64(0, pointer(source), length(source) * sizeof(char));
	end;

	function tSingleShader.tSourceHash.Hash(const h: tSourceHash): Hash.Value;
	begin
		result := Algo.Hash.OfUint(h.len) xor Algo.Hash.Value(h.crc64);
	end;

	function tSingleShader.tSourceHash.Equals(const a, b: tSourceHash): boolean;
	begin
		result := (a.len = b.len) and (a.crc64 = b.crc64);
	end;

	function tSingleShader.Prepare(parent: pShader; const params: ShaderFlags; chainId: sint): tPrepared; static;
	var
		s: StringBuilder;
	{$ifdef Debug} msg: string; {$endif}
	begin
		Assert(ShaderFlags.Equals(params * parent^.tok.mask, params), 'примени маску');
		result.parent := parent;
		result.params := params;

		result.name := '"' + parent^.name + '" (флаги: ' + params.Human + ')';
	{$ifdef Debug} LogR('Подготовка шейдера ' + result.name + '... '); {$endif}

		s.Init;
	{$ifdef Debug} LogR('Версия: ' + parent^.tok.version + '; ', logDebug); {$endif}
		s.Append('#version ', parent^.tok.version, EOL);

		if gl.ShouldSetPrecisionExplicitly then s.Append('precision mediump float;', EOL);
		if (parent^.type_ = GLshader_Fragment) and gl.InOutSupported then
			s.Append('out vec4 ', FragColorVariable, ';', EOL);

		parent^.tok.BuildSourceAndMask([], s, parent^.type_, chainId, result.params);
		result.source := s.DestructiveToString;
		result.hash := tSourceHash.Create(result.source);

	{$ifdef Debug}
		msg := 'Шейдер ' + result.name + ' готов, хэш: ' + ToString(result.hash.len) + '/' + ToString(result.hash.crc64, IntFormat.Hex);
		if not ShaderFlags.Equals(result.params, params) then msg += ', реальные флаги: ' + result.params.Human;
		Log(msg, logOK);
	{$endif}
	end;

	constructor tSingleShader.Init(const prep: tPrepared);
	begin
		inherited Init;
		_parent := prep.parent;
		SetLength(_params, 1);
		_params[0] := prep.params;
		_hash   := prep.hash;
	{$ifdef Debug} _name := prep.name; {$endif}
		_inGL := gl.CreateShader(_parent^._st, prep.name, prep.source);
	end;

	destructor tSingleShader.Done;
	var
		i: sint;
	begin
		if Assigned(_parent) then
		begin
			for i := 0 to High(_params) do
				if not _parent^._sh.Remove(_params[i]) then Assert(no);
			if not _parent^._s2sh.Remove(_hash) then Assert(no);
		end;
		gl.DeleteShader(_inGL);
	{$ifdef Debug} LogR('Шейдер ' + _name + ' уничтожен; ', logDebug); {$endiF}
		inherited Done;
	end;

	constructor tShader.Init(s: pStream);
	var
		nm, code: string;
	begin
		if not Assigned(s) then Fail;
		nm := StreamPath.FilenameNoExt(s^.path);
		code := ReadWholeAsString(s);
		Init(nm, code);
	end;

	constructor tShader.Init(const name, source: string; aType: sint = -1);
	var
		defs: array of PoolString;

		procedure AppendDefine(const def: PoolString);
		begin
			SetLength(defs, length(defs) + 1);
			defs[High(defs)] := def;
		end;

	var
		id: sint;
		sample: string;
	begin
		if source = '' then Fail;

		if aType < 0 then
		begin
			sample := StreamPath.Extension(name);
			if sample = ShaderFilenameExtension then sample := StreamPath.Extension(name, 1);
			id := FindStr(sample, ShaderTypeAbbrevs);
			if id <> -1 then _st := ShaderType(id) else
			begin
			{$ifdef Debug} Log('Не удалось определить тип шейдера "' + name + '"', logError); {$endif}
				Fail;
			end;
		end else
			_st := ShaderType(aType);

		if (_st = GLshader_Geometry) and (not gl.GeometryShaderSupported) then
		begin
		{$ifdef Debug} Log('Геометрический шейдер "' + name + '" отброшен.', logWarning); {$endif}
			Fail;
		end;

		inherited Init;
		_sh.Init;
		_s2sh.Init;
		self.name := name;

		defs := nil;
		if gl.GeometryShaderSupported then AppendDefine(GeometryShaderDefine);

		tok.Init(defs, source);
		if tok.version.Empty then tok.version := gl.SLVersion
		      {$ifdef Debug} else LogR('"' + name + '" - пользовательская версия: ' + tok.version + '; ') {$endif};
	end;

	destructor tShader.Done;
	var
		it: tHash_Flags2SingleShader.Iterator;
		sh: pSingleShader;
	begin
		it := _sh.GetIterator;
		while _sh.Next(it) do
		begin
			sh := _sh.GetValue(it)^;
			if Assigned(sh) then sh^._parent := nil;
		end;

		tok.Done;
		_s2sh.Done;
		_sh.Done;
		inherited Done;
	end;

	function tShader.Request(const f: ShaderFlags; const prep: tSingleShader.tPrepared): pSingleShader;
	var
		flags: ShaderFlags;
		r: ^pSingleShader;
	begin
		flags := f * tok.mask;
		r := _sh.Find(flags);
		if Assigned(r) then exit(r^);

		Assert(ShaderFlags.Equals(prep.params, prep.params * tok.mask), 'маска вычислена неверно');
		r := _sh.Find(prep.params);
		if Assigned(r) then
		begin
		{$ifdef Debug} if Assigned(r^) then Log('Шейдер ' + prep.name + ' объединён с ' + r^^._name + ', т. к. разницы на уровне реально заюзанных флагов не замечено', logDebug); {$endif}
		end else
		begin
			r := _s2sh.Find(prep.hash);
		{$ifdef Debug}
			if Assigned(r) and Assigned(r^) then
				Log('Шейдер ' + prep.name + ' объединён с ' + r^^._name + ', т. к. разницы на уровне исходника не замечено', logDebug);
		{$endif}
		end;

		if Assigned(r) then result := r^ else
		begin
			result := new(pSingleShader, Init(prep));
			_s2sh.Add(prep.hash, result);
			if not ShaderFlags.Equals(prep.params, flags) then _sh.Add(prep.params, result);
		end;

		if Assigned(result) and (Assigned(r) or not ShaderFlags.Equals(prep.params, flags)) then
		begin
			SetLength(result^._params, length(result^._params) + 1);
			result^._params[High(result^._params)] := flags;
		end;

		// Даже если шейдер не скомпилировался (result = nil), это стоит запомнить
		_sh.Add(flags, result);
	end;

	function tShader.FileName(const base: string): string;
	begin
		if StreamPath.Extension(base) = ShaderFilenameExtension then
			result := base
		else
			result := base + ExtensionSeparator + ShaderFilenameExtension;
	end;

	function SingleShaderProgram._FindUniform(var rp: tParams4Renderable; const name: PoolString): pNativeGLValue;
	begin
		result := nil;
		if Assigned(rp.roParams) then result := rp.roParams^.values.Value(name);
		if not Assigned(result) then
		begin
			if Assigned(rp.mtlParams) then result := rp.mtlParams^.values.Value(name);
			if not Assigned(result) then
			begin
				result := GlobalGL.values.Value(name);
			{$ifdef Debug} if not Assigned(result) then Log(_name + ': шейдерная константа "' + name + '" не найдена', logError); {$endif}
			end;
		end;
	end;

	procedure SingleShaderProgram._SendUniforms(var rp: tParams4Renderable);
	{$ifdef Debug}
		procedure CheckType(cp: pNativeGLValue; reqType: GLType);
		begin
			if cp^.Type_ <> reqType then
				Log('Тип юниформа "' + cp^.namae + '" не совпадает: требуется ' + GLTypeIds[reqType] + ', получен ' + GLTypeIds[cp^.Type_], logError);
		end;
	{$endif}
	var
		i, j, count: sint;
		cp: pNativeGLValue;
		uu: pUboUniform;
		tex: pTexture;
		ch: boolean;
	begin
	trace_call('SingleShaderProgram._SendUniforms');
		for i := 0 to High(_uniforms) do
		begin
			cp := _FindUniform(rp, _uniforms[i].name);
			if (not Assigned(cp)) or (cp^.Version = _uniforms[i].ver) then continue;
		{$ifdef Debug} CheckType(cp, _uniforms[i].inGL.type_); {$endif}
			if NativeGLValueFlag.InstaPack in cp^.flags then
				count := rp.nInsta
			else
				count := cp^.count;
			_uniforms[i].ver := cp^.Version;
			if cp^.Type_ <> GLType.Sampler then
				gl.SetUniform(_inGL, _uniforms[i].inGL, cp^.Ptr, count)
			else
			begin
				tex := pTexture(cp^.RawTex);
				if Assigned(tex) then
				begin
					tex^.EnsureLoaded;
					gl.SetUniform(_inGL, _uniforms[i].inGL, tex^.inGL, count);
				end else
					gl.SetUniform(_inGL, _uniforms[i].inGL, nil);
			end;
		end;
		for i := 0 to High(_ubos) do
		begin
			ch := no;
			for j := 0 to High(_ubos[i].uni) do
			begin
				uu := @_ubos[i].uni[j];
				cp := _FindUniform(rp, uu^.name);
				if (not Assigned(cp)) or (cp^.Version = uu^.ver) then continue;
			{$ifdef Debug} CheckType(cp, uu^.inGL.typ); {$endif}
				if NativeGLValueFlag.InstaPack in cp^.flags then
					count := rp.nInsta
				else
					count := cp^.count;
				uu^.ver := cp^.Version;
				gl.PlaceUboData(cp^.Ptr, uu^.inGL, _ubos[i].data + uu^.inGL.bufOffset, count);
				ch := yes;
			end;
			if ch then _ubos[i].Send;
		end;
	leave_call
	end;

	procedure SingleShaderProgram.tUbo.Init(const ubo: GLUboDesc);
	var
		i: sint;
	begin
		dataSize := ubo.dataSize;
		data := GetMem(dataSize);
		fillchar(data^, dataSize, 0);
		oldData := GetMem(dataSize);
		fillchar(oldData^, dataSize, 0);
		inGL := gl.CreateBuffer(GLbuffer_Uniform);
		gl.BufferData(inGL, dataSize, data);

		SetLength(uni, length(ubo.uniforms));
		for i := 0 to High(uni) do
		begin
			uni[i].name := ubo.uniforms[i].name;
			uni[i].inGL := ubo.uniforms[i].u;
			uni[i].ver := 0;
		end;
	end;

	procedure SingleShaderProgram.tUbo.Done;
	begin
		uni := nil;
		FreeMem(oldData);
		FreeMem(data);
		gl.DeleteBuffer(inGL);
	end;

	procedure _SendBufferRange(src: pointer; offset, size: size_t; param: pointer);
	var
		ubo: ^SingleShaderProgram.tUbo absolute param;
	begin
		gl.BufferSubData(ubo^.inGL, offset, size, src + offset);
		System.Move((src + offset)^, (ubo^.oldData + offset)^, size);
	end;

	procedure SingleShaderProgram.tUbo.Send;
	begin
		UpdateRanges.Update(data, oldData, dataSize, 96, @_SendBufferRange, @self);
	end;

type
	BinaryShaderVersionInfo = object
	type
		OneVersion = record
			ver: PoolString;
			sh: set of ShaderType;
		end;
	var
		vers: array of OneVersion;
		procedure Init;
		procedure Add(nst: ShaderType; const ver: PoolString);
		function ToString: string;
	end;

	procedure BinaryShaderVersionInfo.Init;
	begin
		vers := nil;
	end;

	procedure BinaryShaderVersionInfo.Add(nst: ShaderType; const ver: PoolString);
	var
		id: sint;
	begin
		id := Index(ver.ToIndex, pointer(vers) + fieldoffset OneVersion _ ver _, length(vers), sizeof(OneVersion));
		if id >= 0 then Include(vers[id].sh, nst) else
		begin
			SetLength(vers, length(vers) + 1);
			vers[High(vers)].ver := ver;
			vers[High(vers)].sh := [nst];
		end;
	end;

	function GetBinaryShaderVersionPart(id: uint; param: pointer): string;
	var
		st: ShaderType;
	begin
		result := '';
		for st in BinaryShaderVersionInfo(param^).vers[id].sh do
			result += ShaderTypeAbbrevs[st];
		result += BinaryShaderVersionInfo(param^).vers[id].ver;
	end;

	function BinaryShaderVersionInfo.ToString: string;
	begin
		result := SeparatedList.Join(length(vers), @GetBinaryShaderVersionPart, @self, '-' + SeparatedList.Prefix + '-');
	end;

	constructor SingleShaderProgram.Init(theParent: pShaderProgram; const params: ShaderFlags; const prep: array of tSingleShader.tPrepared);
	var
		i, n: sint;
		sh: array of pGLShader;
		fn, name, gname: string;
		info: ShaderEntrails;
		bufs: array of pGLBuffer;
		ver: BinaryShaderVersionInfo;
	{$ifdef Debug} j, k, nComponents: sint; {$endif}
		capNote: WindowCaption.Cookie;
	begin
	trace_call('SingleShaderProgram.Init');
		inherited Init;
	{$ifdef Debug} _name := ''; {$endif}
		_uniforms := nil;
		_ubos := nil;
		shaders := nil;
		attrs := nil;
		SetLength(_params, 1);
		_params[0] := params;
		_parent := nil;
		_inGL := nil;

		name := '"' + theParent^.name + '" (флаги: ' + params.Human + ')';
	{$ifdef Debug}
		_name := name;
		nComponents := 0;
	{$endif}
		gname := name;
		fn := '';

		if ResourcePool.Shared^.Loaded(theParent, @fn) then
			capNote := mm.window.caption.SetNote('загрузка шейдера ' + StreamPath.Human(fn) + ', флаги ' + params.Human)
		else
			capNote := WindowCaption.Cookie.Empty;

		if gl.BinaryShadersSupported and ResourcePool.Shared^.Loaded(theParent, @fn) then
		begin
			gname += BinaryShader.Uid + fn + '; ' + params.Base32;
			(@ver)^.Init;
			for i := 0 to High(theParent^.shaders) do
				ver.Add(theParent^.shaders[i]^.type_, theParent^.shaders[i]^.tok.version);
			gname += ver.ToString;
			if gl.UBOSupported and theParent^.HasUbo then
				gname += '-U';
		end;

		if fn <> '' then
			_inGL := gl.CreateProgram(gname, [], info);

		if not Assigned(_inGL) then
		begin
			if gl.BinaryShadersSupported then gname += BinaryShader.SuppressLoad;

			n := 0;
			SetLength(shaders, length(theParent^.shaders));
			SetLength(sh, length(theParent^.shaders));
			for i := 0 to High(theParent^.shaders) do
			begin
				shaders[n] := MakeRef(theParent^.shaders[i]^.Request(params, prep[i]));
				if Assigned(shaders[n]) then
				begin
					sh[n] := shaders[n]^._inGL;
					inc(n);
				end;
			end;
			if n <> length(theParent^.shaders) then
			begin
				SetLength(shaders, n);
				ConstructorFailed;
			end;

			SetLength(sh, n);
			_inGL := gl.CreateProgram(gname, sh, info);
		end;

		mm.window.caption.RemoveNote(capNote);

		if not Assigned(_inGL) then ConstructorFailed;

		SetLength(_uniforms, length(info.u));
		for i := 0 to High(info.u) do
		begin
			_uniforms[i].inGL := info.u[i].uniform;
			_uniforms[i].name := info.u[i].name;
			_uniforms[i].ver := High(_uniforms[i].ver);
		{$ifdef Debug} nComponents += UniformComponents(info.u[i].uniform.type_) * sint(info.u[i].uniform.count); {$endif}
		end;
		SetLength(_ubos, length(info.ub));
		SetLength(bufs, length(info.ub));
		for i := 0 to High(_ubos) do
		begin
			_ubos[i].Init(info.ub[i]);
			bufs[i] := _ubos[i].inGL;
		{$ifdef Debug}
			for j := 0 to High(info.ub[i].uniforms) do
				nComponents += UniformComponents(info.ub[i].uniforms[j].u.typ) * sint(info.ub[i].uniforms[j].u.count);
		{$endif}
		end;
		gl.SetUbos(_inGL, bufs);
		SetLength(attrs, length(info.va));
		for i := 0 to High(info.va) do
		begin
			attrs[i].namae := CutPrefix(ShaderVaPrefix, info.va[i].name);
			attrs[i].index := info.va[i].index;
		{$ifdef Debug} attrs[i].typ := info.va[i].type_; {$endif}
		end;

	{$ifdef Debug}
		Log('Компонент юниформов: ' + ToString(nComponents), logDebug);
		stat.Note(max_uniforms_in_shader, nComponents);

		for i := 0 to High(_uniforms) do
			for j := 0 to High(_ubos) do
				for k := 0 to High(_ubos[j].uni) do
					if _uniforms[i].name = _ubos[j].uni[k].name then
						Log('Юниформ ' + _uniforms[i].name + ' объявлен одновременно в дефолтном и не-дефолтном блоке. Исправь.', logError);

		for i := 0 to High(_ubos) - 1 do
				for j := 0 to High(_ubos[i].uni) do
					for k := i + 1 to High(_ubos) do
						for n := 0 to High(_ubos[k].uni) do
							if _ubos[i].uni[j].name = _ubos[k].uni[n].name then
								Log('Юниформ ' + _ubos[i].uni[j].name + ' объявлен одновременно в нескольких блоках. Исправь.', logError);
	{$endif}
		_parent := theParent;
	leave_call
	end;

	destructor SingleShaderProgram.Done;
	var
		i: sint;
	begin
		if Assigned(_parent) then
		begin
			for i := 0 to High(_params) do
				if not _parent^._p.Remove(_params[i]) then Assert(no);
		end;
		if Assigned(_inGL) then gl.DeleteProgram(_inGL);
		for i := 0 to High(_ubos) do _ubos[i].Done;
		ReleaseArray(shaders);
	{$ifdef Debug} LogR('Шейдерная программа ' + _name + ' уничтожена; ', logDebug); {$endif}
		inherited Done;
	end;

	procedure SingleShaderProgram.Bind(var rp: tParams4Renderable);
	begin
		_SendUniforms(rp);
	end;

	function ShaderProgram._Request(const p: ShaderFlags): pSingleShaderProgram;
		procedure Reload(s: pStream);
		label stop;
		const
			Decoration = '===>';
			EndToken = '%end%';
			FileSample = '#include ';
			PrologueSample = '%prologue%';
			ShaderTypeSamples: array[ShaderType] of string = ('%vertex%',  '%geometry%', '%fragment%');
		var
			sh: array of pShader;

			procedure AddShader(sha: pShader);
			begin
				if Assigned(sha) then
				begin
					SetLength(sh, length(sh) + 1);
					sh[High(sh)] := sha;
				end;
			end;

			function GetBlock(const source: string; var p: sint; const sample: string; out block: string): boolean;
			var
				p2: sint;
			begin
				result := Prefixed(sample, @source[p], length(source) - p + 1);
				if not result then exit;

				p2 := Pos(EndToken, source, p + length(sample));
				if p2 <= 0 then
				begin
				{$ifdef Debug} Log(sample + ' не закрыт', logError); {$endif}
					p := length(source) + 1;
					exit(no);
				end;

				block := Copy(source, p + length(sample), p2 - p - length(sample));
				p := p2 + length(EndToken);
			end;

		var
			p, p2: sint;
			neol: size_t;
			st: ShaderType;
			prologue, source, block, fn: string;
			i, n: sint;
			base: string;
			ok: boolean;
			name: string;
			it: tHash_Flags2SingleShaderProgram.Iterator;
			sp: pSingleShaderProgram;
		begin
			it := _p.GetIterator;
			while _p.Next(it) do
			begin
				sp := _p.GetValue(it)^;
				if Assigned(sp) then sp^._parent := nil;
			end;
			_p.Done;
			ReleaseArray(shaders);

			name := StreamPath.FilenameNoExt(s^.path);
			base := s^.path;
			source := ReadWholeAsString(s);
			prologue := '';

			sh := nil;
			p := 1;
			while p <= length(source) do
			begin
				if Symbol.IsWhitespace(source[p]) then
				begin
					inc(p);
					continue;
				end;

				case source[p] of
					'/':
						if (p < length(source)) and (source[p + 1] = '/') then
						begin
							p += 2;
							while (p <= length(source)) and (source[p] <> EOL) do
								inc(p);
							continue;
						end;
					'=':
						if Prefixed(Decoration, @source[p], length(source) - p + 1) then
						begin
							inc(p, length(Decoration));
							continue;
						end;
					'#':
						if Prefixed(FileSample, @source[p], length(source) - p + 1) then
						begin
							p2 := length(source) + 1;
							neol := 0;
							for i := p + length(FileSample) to length(source) do
								if UTF8.IsEOL(pChar(source) + i - 1, length(source) - i + 1, neol) then
								begin
									p2 := i;
									break;
								end;

							fn := tShader.FileName(Copy(source, p + length(FileSample), p2 - p - length(FileSample)));
							AddShader(ResourcePool.Shared^.LoadRef(TypeOf(tShader), StreamPath.Resolve(fn, base)));
							p := p2 + sint(neol);
							continue;
						end else
						begin
						{$ifdef Debug} Log('# не в тему (' + Copy(source, p, 5) + ')', logError); {$endif}
							goto stop;
						end;
					'%':
						begin
							ok := no;
							for st in ShaderType do
								if GetBlock(source, p, ShaderTypeSamples[st], block) then
								begin
									AddShader(MakeRef(new(pShader, Init(name + ExtensionSeparator + ShaderTypeAbbrevs[st],
										prologue + block, ord(st)))));
									ok := yes;
									break;
								end;
							if ok then continue;
							if GetBlock(source, p, PrologueSample, block) then
							begin
								prologue := block;
								if (length(prologue) > 0) and (prologue[length(prologue)] <> EOL) then prologue += EOL;
								continue;
							end else
							begin
							{$ifdef Debug} Log('% не в тему (' + Copy(source, p, 5) + ')', logError); {$endif}
								goto stop;
							end;
						end;
				end;

			{$ifdef Debug} Log('Неожиданный символ: #' + ToString(ord(source[p])) + ' (' + Copy(source, p, 5) + ')', logError); {$endif}
				inc(p);
			end;

		stop:
			_mask := ShaderFlags.Zero;
			_p.Init;

			SetLength(shaders, length(sh));
			n := 0;
			for i := 0 to High(sh) do
			begin
				shaders[n] := MakeRef(sh[i]);
				if Assigned(sh[i]) then
				begin
					_mask := _mask + sh[i]^.tok.mask;
					inc(n);
				end;
			end;
			SetLength(shaders, n);
			ReleaseArray(sh);
		end;
	var
		prep: array of tSingleShader.tPrepared;
		flags, rflags: ShaderFlags;
		r: ^pSingleShaderProgram;
		i: sint;
	begin
		flags := p * _mask;
		r := _p.Find(flags);
		if Assigned(r) then exit(r^);

		SetLength(prep, length(shaders));
		rflags := ShaderFlags.Zero;
		for i := 0 to High(shaders) do
		begin
			prep[i] := tSingleShader.Prepare(shaders[i], flags * shaders[i]^.tok.mask, i);
			rflags += prep[i].params;
		end;

		r := _p.Find(rflags);
		if Assigned(r) then
		begin
			result := r^;
		{$ifdef Debug}
			if Assigned(result) then
				Log('Шейдерные программы объединены за отсутствием видимой разницы: ' + name + ', флаги ' + flags.Human + ' <-> ' + rflags.Human, logDebug);
		{$endif}
		end else
		begin
		{$ifdef Debug}
			if not ShaderFlags.Equals(flags, rflags) then
				LogR('Реальные флаги шейдерной программы: "' + name + '": ' + flags.Human + ' -> ' + rflags.Human + '; ', logDebug);
		{$endif}
			result := new(pSingleShaderProgram, Init(@self, rflags, prep));
			if not ShaderFlags.Equals(rflags, flags) then _p.Add(rflags, result);
		end;

		if Assigned(result) and (Assigned(r) or not ShaderFlags.Equals(rflags, flags)) then
		begin
			SetLength(result^._params, length(result^._params) + 1);
			result^._params[High(result^._params)] := flags;
		end;

		// Даже если шейдер не скомпилировался, это стоит запомнить
		_p.Add(flags, result);
	end;

	constructor ShaderProgram.Init(const newName: PoolString; const sh: array of pShader);
	var
		i, n: sint;
	begin
		inherited Init;
		_mask := ShaderFlags.Zero;
		_p.Init;

		SetLength(shaders, length(sh));
		n := 0;
		self.name := newName;
		for i := 0 to High(sh) do
		begin
			shaders[n] := MakeRef(sh[i]);
			if Assigned(sh[i]) then
			begin
				_mask := _mask + sh[i]^.tok.mask;
				inc(n);
			end;

			if (self.name = '') and Assigned(sh[i]) then
				self.name := StreamPath.FilenameNoExt(sh[i]^.name);
		end;
		SetLength(shaders, n);
	end;

	constructor ShaderProgram.Init(s: pStream);
	label stop;
	const
		Decoration = '===>';
		EndToken = '%end%';
		FileSample = '#include ';
		PrologueSample = '%prologue%';
		ShaderTypeSamples: array[ShaderType] of string = ('%vertex%',  '%geometry%', '%fragment%');
	var
		sh: array of pShader;

		procedure AddShader(sha: pShader);
		begin
			if Assigned(sha) then
			begin
				SetLength(sh, length(sh) + 1);
				sh[High(sh)] := sha;
			end;
		end;

		function GetBlock(const source: string; var p: sint; const sample: string; out block: string): boolean;
		var
			p2: sint;
		begin
			result := Prefixed(sample, @source[p], length(source) - p + 1);
			if not result then exit;

			p2 := Pos(EndToken, source, p + length(sample));
			if p2 <= 0 then
			begin
			{$ifdef Debug} Log(sample + ' не закрыт', logError); {$endif}
				p := length(source) + 1;
				exit(no);
			end;

			block := Copy(source, p + length(sample), p2 - p - length(sample));
			p := p2 + length(EndToken);
		end;

	var
		p, p2: sint;
		neol: size_t;
		st: ShaderType;
		prologue, source, block, fn: string;
		i: sint;
		base: string;
		ok: boolean;
		newName: string;
	begin
		newName := StreamPath.FilenameNoExt(s^.path);
		base := s^.path;
		source := ReadWholeAsString(s);
		prologue := '';

		sh := nil;
		p := 1;
		while p <= length(source) do
		begin
			if Symbol.IsWhitespace(source[p]) then
			begin
				inc(p);
				continue;
			end;

			case source[p] of
				'/':
					if (p < length(source)) and (source[p + 1] = '/') then
					begin
						p += 2;
						while (p <= length(source)) and (source[p] <> EOL) do
							inc(p);
						continue;
					end;
				'=':
					if Prefixed(Decoration, @source[p], length(source) - p + 1) then
					begin
						inc(p, length(Decoration));
						continue;
					end;
				'#':
					if Prefixed(FileSample, @source[p], length(source) - p + 1) then
					begin
						p2 := length(source) + 1;
						neol := 0;
						for i := p + length(FileSample) to length(source) do
							if UTF8.IsEOL(pChar(source) + i - 1, length(source) - i + 1, neol) then
							begin
								p2 := i;
								break;
							end;

						fn := tShader.FileName(Copy(source, p + length(FileSample), p2 - p - length(FileSample)));
						AddShader(ResourcePool.Shared^.LoadRef(TypeOf(tShader), StreamPath.Resolve(fn, base)));
						p := p2 + sint(neol);
						continue;
					end else
					begin
					{$ifdef Debug} Log('# не в тему (' + Copy(source, p, 5) + ')', logError); {$endif}
						goto stop;
					end;
				'%':
					begin
						ok := no;
						for st in ShaderType do
							if GetBlock(source, p, ShaderTypeSamples[st], block) then
							begin
								AddShader(MakeRef(new(pShader, Init(newName + ExtensionSeparator + ShaderTypeAbbrevs[st],
									prologue + block, ord(st)))));
								ok := yes;
								break;
							end;
						if ok then continue;
						if GetBlock(source, p, PrologueSample, block) then
						begin
							prologue := block;
							if (length(prologue) > 0) and (prologue[length(prologue)] <> EOL) then prologue += EOL;
							continue;
						end else
						begin
						{$ifdef Debug} Log('% не в тему (' + Copy(source, p, 5) + ')', logError); {$endif}
							goto stop;
						end;
					end;
			end;

		{$ifdef Debug} Log('Неожиданный символ: #' + ToString(ord(source[p])) + ' (' + Copy(source, p, 5) + ')', logError); {$endif}
			inc(p);
		end;

	stop:
		Init(newName, sh);
		ReleaseArray(sh);
	end;

	destructor ShaderProgram.Done;
	var
		it: tHash_Flags2SingleShaderProgram.Iterator;
		sp: pSingleShaderProgram;
	begin
		it := _p.GetIterator;
		while _p.Next(it) do
		begin
			sp := _p.GetValue(it)^;
			if Assigned(sp) then sp^._parent := nil;
		end;
		_p.Done;
		ReleaseArray(shaders);
		inherited Done;
	end;

	function ShaderProgram.Bind(var rp: tParams4Renderable): pSingleShaderProgram;
	var
		sh: ShaderFlags;
	begin
		sh := rp.sh;
		sh.user += GlobalGL.flags + rp.matLevelParams^.flags + rp.mtlParams^.flags + rp.roParams^.flags;
		sh.Insta := (rp.nInsta > 1) or ((gl.MaxInstances > 1) and GLBase.Config.keepOneInstance);
		result := _Request(sh);
		if Assigned(result) then result^.Bind(rp);
	end;

	function ShaderTypeIs(index: uint; ref, list: pointer): boolean; begin result := ppShader(list)[index]^.type_ = ShaderType(ref^); end;
	function ShaderProgram.Has(typ: ShaderType): boolean;
	begin
		result := Range.Open(length(shaders)).Any(@ShaderTypeIs, @typ, ppShader(shaders));
	end;

	function ShaderHasUbo(index: uint; list: pointer): boolean; begin result := flag_Ubo in ppShader(list)[index]^.tok.flags; end;
	function ShaderProgram.HasUbo: boolean;
	begin
		result := Range.Open(length(shaders)).Any(@ShaderHasUbo, ppShader(shaders));
	end;

	function GLMaterial._Hash(m: pGLMaterial): Hash.Value;
	var
		i: sint;
	begin
		result := 0;
		if length(m^.lods) > 0 then
		begin
			for i := 0 to High(m^.lods[0].pp) do
				result := result xor Hash.OfPointer(m^.lods[0].pp[i].prog);
			result := result xor m^.gl.Hash xor m^.lods[0].gl.Hash;
		end;
	end;

	function GLMaterial._Equals(a, b: pGLMaterial): boolean;
	var
		level, i: sint;
		LA, LB: pGLMaterialLevel;
	begin
		if (a^._inPool and b^._inPool) or (a = b) then exit(a = b);

		result := length(a^.lods) = length(b^.lods);
		if result then
			for level := 0 to High(a^.lods) do
			begin
				LA := @a^.lods[level];
				LB := @b^.lods[level];
				result := (LA^.cull = LB^.cull) and (LA^.blend = LB^.blend) and (length(LA^.pp) = length(LB^.pp));
				if result then
					for i := 0 to High(LA^.pp) do
						if (LA^.pp[i].pass <> LB^.pp[i].pass) or (LA^.pp[i].prog <> LB^.pp[i].prog) then
						begin
							result := no;
							break;
						end;
				if result then result := LA^.gl.Equals(LB^.gl);
				if not result then break;
			end;
		result := result and a^.gl.Equals(b^.gl);
	end;

	procedure GLMaterial._InitInstance;
	begin
		if _nInstances = 0 then _pool.Init;
		inc(_nInstances);
		_inPool := no;
	end;

	constructor GLMaterial.Init(const newName: PoolString; base: pGLMaterial = nil);
	var
		i: sint;
	begin
		inherited Init;
		_InitInstance;
		name := newName;

		if Assigned(base) then
		begin
			SetLength(lods, length(base^.lods));
			gl.Init(base^.gl);
			for i := 0 to High(lods) do
				lods[i].Initialize(@self, base^.lods[i].minLod, @base^.lods[i]);
		end else
		begin
			AddLevel(DefaultMinLOD);
			gl.Init;
		end;
	end;

	constructor GLMaterial.DeseInit;
	begin
		inherited DeseInit;
		_InitInstance;
	end;

	destructor GLMaterial.Done;
	begin
		if _inPool then _pool.Remove(@self);
		ClearLevels;
		gl.Done;
		dec(_nInstances);
		if _nInstances = 0 then _pool.Done;
		inherited Done;
	end;

	procedure GLMaterial.ClearLevels;
	var
		i: sint;
	begin
		for i := 0 to High(lods) do
			lods[i].Finalize;
		lods := nil;
	end;

	procedure GLMaterialLevel.Initialize(newBase: pGLMaterial; const aMinLod: float; cp: pGLMaterialLevel);
	var
		i: sint;
	begin
		base := newBase;
		minLod := aMinLod;
		if Assigned(cp) then
		begin
			gl.Init(cp^.gl);
			SetLength(pp, length(cp^.pp));
			for i := 0 to High(pp) do
			begin
				pp[i].pass := MakeRef(cp^.pp[i].pass);
				pp[i].prog := MakeRef(cp^.pp[i].prog);
			end;
			cull := cp^.cull;
			blend := cp^.blend;
		end else
		begin
			gl.Init;
			pp := nil;
			cull := yes;
			blend := GLblend_Off;
		end;
	end;

	procedure GLMaterialLevel.Finalize;
	var
		i: sint;
	begin
		for i := 0 to High(pp) do
		begin
			Release(pp[i].prog);
			Release(pp[i].pass);
		end;
		gl.Done;
		pp := nil;
	end;

	procedure GLMaterialLevel._SetProg(pass: pRenderPass; prog: pShaderProgram);
	var
		i, id: sint;
	begin
		id := GetProgID(pass);
		if id >= 0 then
		begin
			if Assigned(prog) then
				SetRef(pp[id].prog, prog)
			else
			begin
				Release(pp[id].prog);
				Release(pp[id].pass);
				for i := id to High(pp) - 1 do pp[i] := pp[i + 1];
				SetLength(pp, length(pp) - 1);
			end;
			exit;
		end;
		MakeRef(prog);
		if (Assigned(prog)) and (Assigned(pass)) then
		begin
			id := length(pp);
			SetLength(pp, id + 1);
			pp[id].pass := pass^.NewRef;
			pp[id].prog := prog^.NewRef;
			while (id > 0) and (pp[id].pass < pp[id - 1].pass) do
			begin
				SwapMem(@pp[id], @pp[id - 1], sizeof(pp[id]));
				dec(id);
			end;
		end;
		Release(prog);
	end;

	function GLMaterialLevel._GetProg(pass: pRenderPass): pShaderProgram;
	var
		i: sint;
	begin
		for i := 0 to High(pp) do
			if pp[i].pass = pass then
				exit(pp[i].prog);
		result := nil;
	end;

	function GLMaterialLevel.GetProgID(pass: pRenderPass): sint;
	begin
		result := Index(pass, pointer(pp) + fieldoffset ProgForPass _ pass _, length(pp), sizeof(ProgForPass));
	end;

	function GLMaterial.Merge(m: pGLMaterial): pGLMaterial;
	begin
		if (not Assigned(m)) or (m^._inPool) then exit(m);
		result := _pool.Find(m);
		if Assigned(result) then
		begin
			Assert(result <> m);
		{$ifdef ExtDebug} Log('Материал "' + m^.name + '" не объявлен уникальным и объединился с уже созданным (' + result^.name + ')', logDebug); {$endif}
			PumpRef(m);
		end else
		begin
			_pool.Add(m);
		{$ifdef Debug} stat.Note(max_gl_materials_in_pool, _pool.Count); {$endif}
			m^._inPool := yes;
			result := m;
		end;
	end;

{$define procname:=sort_lods} {$define elem:=GLMaterialLevel} {$define less := _1.minLod > _2.minLod} {$define openarray} {$include sort.inc}

	function GLMaterial.AddLevel(const aMinLod: float; cp: pGLMaterialLevel = nil): pGLMaterialLevel;
	var
		i: sint;
		n: GLMaterialLevel;
	begin
		for i := 0 to High(lods) do
			if Equals(lods[i].minLod, aMinLod) then
			begin
			{$ifdef Debug} Log('Дублирующийся LOD материала: ' + ToString(aMinLod), logWarning); {$endif}
				exit(@lods[i]);
			end;
		n.Initialize(@self, aMinLod, cp);
		SetLength(lods, length(lods) + 1);
		lods[High(lods)] := n;
		sort_lods(lods);
		result := GetLevel(aMinLod);
	end;

	function GLMaterial.GetLevel(const lod: float): pGLMaterialLevel;
	var
		i: sint;
	begin
		for i := 0 to High(lods) do
			if lod >= lods[i].minLod then
				exit(@lods[i]);
		result := nil;
	end;

	function GLMaterial.FetchLevels(const lod: float): LevelsList;
		function Fetch(n: sint): LevelsList;
		var
			i: sint;
		begin
			SetLength(result, n);
			for i := 0 to n - 1 do
				result[i] := @lods[i];
		end;
	const
		MinSplitDistance = 0.05;
	var
		i, nearest: sint;
	begin
		// Случай A, особый: lod = 0.0
		if lod = 0.0 then exit(Fetch(length(lods)));

		// Случай B, маргинальный: уровней вообще нет (правда, на данный момент такого быть не может в принципе)
		if length(lods) = 0 then
		begin
			if GreaterThanEqual(1.0 - lod, MinSplitDistance) then AddLevel(lod);
			exit(Fetch(length(lods)));
		end;

		// Случай C: lod меньше либо равен миниальному
		if LessThanEqual(lod, lods[High(lods)].minLod) then
		begin
			if GreaterThanEqual(lods[High(lods)].minLod - lod, MinSplitDistance) then AddLevel(lod, @lods[High(lods)]);
			exit(Fetch(length(lods)));
		end;

		// Случай D: lod больше либо равен максимальному
		if GreaterThanEqual(lod, lods[0].minLod) then
		begin
			if GreaterThanEqual(lod - lods[0].minLod, MinSplitDistance) then
				AddLevel(lod, @lods[0]);
			exit(Fetch(1));
		end;

		// Случай E: lod между двумя существующими
		for i := 1 to High(lods) do
			if lod > lods[i].minLod then
			begin
				if LessThan(lods[i - 1].minLod - lod, MinSplitDistance) then
					exit(Fetch(i));
				if LessThan(lod - lods[i].minLod, MinSplitDistance) then
					exit(Fetch(i + 1));
				if LessThan(lods[i - 1].minLod - lod, lod - lods[i].minLod) then
					nearest := i - 1
				else
					nearest := i;
				AddLevel(lod, @lods[nearest]);
				Assert(lods[i].minLod = lod);
				exit(Fetch(i + 1));
			end;

		// ухуху...
		Assert(no, 'You shouldn''t be here. Please go back.');
	end;

	function GLMaterial.AnyLevel: pGLMaterialLevel;
	begin
		if length(lods) > 0 then
			result := @lods[0]
		else
			result := nil;
	end;

	procedure RenderTexturesPool.Init;
	begin
		_share := nil;
	{$ifdef Debug} _maxQueries := 0; {$endif}
	end;

	procedure RenderTexturesPool.Done;
	begin
	{$ifdef Debug}
		Log('Макс. ссылок на расшаренную текстуру в пуле: ' + ToString(_maxQueries));
		if length(_share) <> 0 then
			Log('Не возвращено текстур в пул: ' + ToString(length(_share)));
	{$endif}
	end;

{$ifdef Debug}
	procedure RenderTexturesPool._LogAboutNewTexture(tex: pTexture; const msgPrefix: string);
	begin
		Log(msgPrefix + GLTextureTargetIds[tex^.target] + ' ' + SizeToString(tex^.size) + ' @ ' + GLImageFormatIds[tex^.format] + ' ' +
			'(теперь в пуле: ' + ToString(length(_share)) + ')', logDebug);
	end;
{$endif}

	function RenderTexturesPool.QueryShared(target: GLTextureTarget; sizeX, sizeY: uint; format: GLImageFormat): pTexture;
	var
		i: sint;
		nt: pTexture;
	begin
		for i := 0 to High(_share) do
			if (_share[i].tex^.target = target) and (_share[i].tex^.size.xy = UintVec2.Make(sizeX, sizeY)) and (_share[i].tex^.format = format) then
			begin
				result := _share[i].tex;
				inc(_share[i].nQueries);
			{$ifdef Debug} _maxQueries := max(_maxQueries, _share[i].nQueries); {$endif}
				exit;
			end;
		nt := new(pTexture, Init(target, UintVec2.Make(sizeX, sizeY), format, [], texture_External));
		if not Assigned(nt) then exit(nil);
		SetLength(_share, length(_share) + 1);
		_share[High(_share)].tex := MakeRef(nt);
		_share[High(_share)].nQueries := 1;
		result := nt;
	{$ifdef Debug}
		_maxQueries := max(_maxQueries, 1);
		_LogAboutNewTexture(nt, 'Новая расшаренная текстура в пуле: ');
	{$endif}
	end;

	procedure RenderTexturesPool.Return(var tex: pTexture);
	var
		i: sint;
	begin
		if not Assigned(tex) then exit;
		for i := 0 to High(_share) do
			if _share[i].tex = tex then
			begin
				Assert(_share[i].nQueries > 0);
				dec(_share[i].nQueries);
				if _share[i].nQueries = 0 then
				begin
					Release(_share[i].tex);
					_share[i] := _share[High(_share)];
					SetLength(_share, length(_share) - 1);
				end;
				break;
			end;
		Release(tex);
	end;

	constructor tRenderTarget.Init(const newName: PoolString;
		newTarget: GLTextureTarget; newFormats: array of GLImageFormat;
		const newSize: UintVec2; newDb: RenderTargetDepthBehaviour);
	var
		i: sint;
	begin
		_main := no;
		Assert(length(newFormats) > 0);
		name := newName;
		inGL := gl.CreateRenderTarget;
		_size := newSize;
		_target := newTarget;
		_depthBehaviour := newDb;
		gl.ResizeRenderTarget(inGL, _size);
		SetLength(color, length(newFormats));
		for i := 0 to High(color) do
		begin
			color[i].tex := nil;
			color[i].format := newFormats[i];
		end;
		depth := nil;
		_activeSublevel := 0;
		_clearColor := GLBase.Config.abyssColor;
	end;

	constructor tRenderTarget.InitMain(const newSize: UintVec2);
	begin
		_main := yes;
		name := 'main';
		inGL := @gl.ScreenRT;
		_target := GLtexture_2D;
		SetLength(color, 1);
		color[0].tex := nil;
		color[0].format := GLformat_RGB;
		depth := nil;
		_size := newSize;
		_clearColor := GLBase.Config.abyssColor;
		_depthBehaviour := rt_ExclusiveDepth;
	end;

	destructor tRenderTarget.Done;
	begin
		Discard;
		if not _main then gl.DeleteRenderTarget(inGL);
	end;

{$ifdef Debug}
	function tRenderTarget._Validate(alsoReportIfOk: boolean): boolean;
	var
		status: GLRenderTargetStatus;
	begin
		status := gl.ValidateRenderTarget(inGL);
		if (status <> GLrt_Complete) or (alsoReportIfOk) then
		begin
			LogR('Динамический ' + GLTextureTargetIds[_target] + ' рендертаргет "' + name + '" (' + SizeToString(_size) + '): ');
			case status of
				GLrt_Complete: Log('годен', logOK);
				GLrt_Incomplete: Log('не годен', logError);
				GLrt_Unsupported: Log('не поддерживается', logError);
			end;
		end;
		result := status = GLrt_Complete;
	end;
{$endif}

	procedure tRenderTarget.Resize(const newSize: UintVec2);
	var
		i: sint;
	{$ifdef Debug} oldSizeStr: string; {$endif}
	begin
		if (_size.X = newSize.X) and ((TextureTargetsInfo[_target].determinativeDims < 2) or (_size.Y = newSize.Y)) then exit;
	{$ifdef Debug} oldSizeStr := SizeToString(_size); {$endif}
		_size.X := newSize.x;
		if TextureTargetsInfo[_target].determinativeDims > 1 then _size.Y := newSize.Y else
			if _target = GLtexture_Cube then _size.Y := _size.X else _size.Y := 1;
	{$ifdef Debug} Log('RT "' + name + '": ' + oldSizeStr + ' -> ' + SizeToString(_size), logDebug); {$endif}
		gl.ResizeRenderTarget(inGL, _size);
		if _main or (not Assigned(color[0].tex)) then exit;

		for i := 0 to High(color) do
			color[i].tex^.Resize(_size);
		case _depthBehaviour of
			rt_SharedDepth:
				begin
					RTpool.Return(depth);
					SetRef(depth, RTpool.QueryShared(GLtexture_2D, _size.X, _size.Y, GLformat_Depth));
					gl.AttachRenderTexture(inGL, depth^.inGL, _activeSublevel div TextureTargetsInfo[_target].faces, GLbuffer_Depth);
				end;
			rt_ExclusiveDepth: depth^.Resize(_size);
		end;
	{$ifdef Debug} _Validate(yes); {$endif}
	end;

	procedure tRenderTarget.SwitchToSublevel(sublv: uint);
	var
		i: sint;
	begin
		Assert(_target = GLtexture_Cube);
		Assert(not _main, 'main RT is 2d');
		if _activeSublevel = sublv then exit;
		_activeSublevel := sublv;
		if not Assigned(color[0].tex) then exit;
		for i := 0 to High(color) do
			gl.AttachRenderTexture(inGL, color[i].tex^.inGL, sublv, GLbuffer_Color, i);
		gl.AttachRenderTexture(inGL, depth^.inGL, sublv div TextureTargetsInfo[_target].faces, GLbuffer_Depth);
	end;

	procedure tRenderTarget.Prepare;
	var
		i: sint;
		p: GLTextureParamsRec;
	begin
		if main or Assigned(color[0].tex) then exit;
		p.wrap := GLwrap_Clamp;
		for i := 0 to High(color) do
		begin
			color[i].tex := MakeRef(new(pTexture, Init(_target, _size, color[i].format, [], texture_External)));
			gl.SetTextureParams(color[i].tex^.inGL, p, [GLtexparam_Wrap]);
			gl.AttachRenderTexture(inGL, color[i].tex^.inGL, _activeSublevel, GLbuffer_Color, i);
		end;
		case _depthBehaviour of
			rt_ExclusiveDepth: SetRef(depth, new(pTexture, Init(_target, _size, GLformat_Depth, [], texture_External)));
			rt_SharedDepth: SetRef(depth, RTpool.QueryShared(GLtexture_2D, _size.x, _size.y, GLformat_Depth));
		end;
		if _depthBehaviour <> rt_NoDepth then
		begin
			gl.SetTextureParams(depth^.inGL, p, [GLtexparam_Wrap]);
			gl.AttachRenderTexture(inGL, depth^.inGL, _activeSublevel div TextureTargetsInfo[_target].faces, GLbuffer_Depth);
		end;
	{$ifdef Debug} _Validate(yes); {$endif}
	end;

	procedure tRenderTarget.Discard;
	var
		i: sint;
	begin
		if main or not Assigned(color[0].tex) then exit;
		for i := 0 to High(color) do
			if Assigned(color[i].tex) then
			begin
				gl.DetachRenderTexture(inGL, GLbuffer_Color, i);
				Release(color[i].tex);
			end;
		if (_depthBehaviour <> rt_NoDepth) and Assigned(depth) then gl.DetachRenderTexture(inGL, GLbuffer_Depth);
		case _depthBehaviour of
			rt_SharedDepth: RTpool.Return(depth);
			rt_ExclusiveDepth: Release(depth);
		end;
	end;

	function tRenderTarget.Buffers: GLRenderBuffers;
	begin
		result := [GLbuffer_Color];
		if _depthBehaviour <> rt_NoDepth then Include(result, GLbuffer_Depth);
	end;

	procedure tRenderTarget.Clear;
	begin
		Clear([Low(GLRenderBuffer) .. High(GLRenderBuffer)]);
	end;

	procedure tRenderTarget.Clear(what: GLRenderBuffers);
	begin
		gl.Clear(inGL, what * Buffers, _clearColor);
	end;

	procedure tParams4Renderable.Reset;
	begin
		pass := MakeRef(@MainPass);
		view := nil;
		rt := nil;
		mtlParams := nil;
		matLevelParams := nil;
		roParams := nil;
		sh := ShaderFlags.FromUser(GlobalGL.flags);
		useMatBlend := yes;
		nInsta := 1;
	end;

	procedure tParams4Renderable.Finalize;
	begin
		Release(pass);
	end;

	constructor tGLBatch.Init(newBatch: pBatch; newMesh: pGLMesh);
	var
		i: sint;
	begin
		_drawables := nil;
		_batch := newBatch;
		_mesh := newMesh;
		vaVb := nil;
		SetLength(levelInds, length(_mesh^.mesh^.levels) - 1);
		for i := 0 to High(levelInds) do
			levelInds[i].ibOffset := 0;
		_interleave := _InterleavingRequired;
	{$ifdef DebugInterleaving}
		if not newMesh^.dynamic then
			if _interleave then
				Log('Для батча ' + Human + ' включено хранение атрибутов вперемешку (выравнивание: ' + ToString(gl.VaAlignment) + ')', logDebug)
			else
				Log('Хранение атрибутов вперемешку в батче ' + Human + ' выключено', logDebug);
	{$endif}
		_InterleavingOpen(_interleave);
	end;

	destructor tGLBatch.Done;
	var
		i: sint;
	begin
		for i := 0 to High(_drawables) do
			_drawables[i].Finalize;
		_InterleavingClose(_interleave);
	end;

{$ifdef Debug}
	function tGLBatch.Human: string;
	begin
		result := _batch^.Human;
	end;
{$endif}

	function tGLBatch._InterleavingRequired: boolean;
	var
		i: sint;
		sz: size_t;
	begin
		if gl.VaAlignment > 1 then
			for i := 0 to High(_batch^.va) do
			begin
				sz := GLTypeInfo[_batch^.va[i].type_].sizeof;
				if align(sz, gl.VaAlignment) <> sz then
					exit(yes);
			end;
		result := no;
	end;

	procedure tGLBatch._InterleavingOpen(interleave: boolean);
	type
		tVaDesc = record
			id: sint;
			sizeof: size_t;
		end;

	{$define procname:=sort} {$define elem:=tVaDesc} {$define less := _1.sizeof > _2.sizeof} {$define openarray} {$include sort.inc}

	var
		desc: array of tVaDesc;
		i, va: sint;
		nextOffset: size_t;
	{$ifdef DebugInterleaving}
		msg: string;
		unalignedSize: size_t;
	{$endif}
	begin
		if interleave then
		begin
			_vbOffset := 0;
			_vertexSize := 0;
			_vtsAllocated := 0;
		{$ifdef DebugInterleaving}
			msg := 'Структура вершины ' + Human + ': (';
			unalignedSize := 0;
		{$endif}

			SetLength(desc, length(_batch^.va));
			for i := 0 to High(_batch^.va) do
			begin
				desc[i].id := i;
				desc[i].sizeof := GLTypeInfo[_batch^.va[i].type_].sizeof;
			{$ifdef DebugInterleaving} unalignedSize += desc[i].sizeof; {$endif}
			end;
			sort(desc);

			SetLength(_vertexLayout, length(desc));
			nextOffset := 0;
			for i := 0 to High(_vertexLayout) do
			begin
				va := desc[i].id;
				_vertexLayout[va].offset := nextOffset;
				nextOffset := align(nextOffset + desc[i].sizeof, {LCM(} gl.VaAlignment {, desc[(i + 1) mod length(desc)].sizeof)});
			{$ifdef DebugInterleaving}
				if i > 0 then msg += ', ';
				msg += '+' + ToString(_vertexLayout[va].offset) + ': ' + GLTypeIds[_batch^.va[va].type_] + ' ' + _batch^.va[va].namae;
			{$endif}
			end;
			_vertexSize := nextOffset;
		{$ifdef DebugInterleaving}
			msg += '), байт: ' + ToString(_vertexSize);
			if unalignedSize <> _vertexSize then msg += ' (потрачено на выравнивание: ' + ToString(_vertexSize - unalignedSize) + ')';
			Log(msg, logDebug);
		{$endif}
		end else
		begin
			SetLength(vaVb, length(_batch^.va));
			for i := 0 to High(vaVb) do
			begin
				vaVb[i].ofs := 0;
				vaVb[i].vtsAllocated := 0;
			end;
		end;
	end;

	procedure tGLBatch._InterleavingClose(interleave: boolean);
	begin
		if interleave then
		begin
			_vertexLayout := nil;
		end else
		begin
			vaVb := nil;
		end;
	end;

	procedure tGLBatch._GetInterleaved(buf: pointer; start: uint; count: sint);
	var
		i, va: sint;
		cv: pNativeGLValue;
		src, dst: pointer;
		sz: size_t;
	begin
		Assert(count > 0);
		for va := 0 to High(_vertexLayout) do
		begin
			cv := @_batch^.va[va];
			sz := GLTypeInfo[cv^.type_].sizeof;
			src := cv^.ptr + start * sz;
			dst := buf + _vertexLayout[va].offset;
			for i := 0 to count - 1 do
			begin
				memcpy(src, dst, sz);
				src += sz;
				dst += _vertexSize;
			end;
		end;
	end;

	procedure tGLBatch._Sweep;
	begin
		if length(_drawables) = 0 then exit;
		inc(_sweepPos);
		if _sweepPos > High(_drawables) then _sweepPos := 0;
		if mm.SceneTimeSince(_drawables[_sweepPos].lastTimeDrawn) > VAOLifetime then
		begin
		{$ifdef DebugDrawables} Log('Батч+прога удалено: ' + tDrawable.HumanName(@self, _drawables[_sweepPos].progs), logDebug); {$endif}
			_drawables[_sweepPos].Finalize;
			_drawables[_sweepPos] := _drawables[High(_drawables)];
			SetLength(_drawables, length(_drawables) - 1);
		end;
	end;

	function tGLBatch.tDrawable.Query(aProg: pSingleShaderProgram; batch: pGLBatch): pGLVertexDeclaration;
	var
		i, j {$ifdef Debug}, na, nb {$endif}: sint;
	{$ifdef Debug} exmsg: string; {$endif}
		vb, ib: pGLBuffer;
		d: ^tDrawable;
		map: tVatMap;
		ok: boolean;
	begin
		d := nil;
		for i := 0 to High(batch^._drawables) do
		begin
			for j := 0 to High(batch^._drawables[i].progs) do
				if batch^._drawables[i].progs[j] = aProg then
				begin
					d := @batch^._drawables[i];
					break;
				end;
			if Assigned(d) then break;
		end;

		if not Assigned(d) then
		begin
			ok := yes;
			for j := 0 to High(aProg^.attrs) do
			begin
				map[j] := -1;
				for i := 0 to High(batch^.batch^.va) do
					if batch^.batch^.va[i].Namae = aProg^.attrs[j].namae then
					begin
					{$ifdef Debug}
						na := GLTypeInfo[batch^.batch^.va[i].Type_].baseDim;
						nb := GLTypeInfo[aProg^.attrs[j].typ].baseDim;
						if na <> nb then
						begin
							if na > nb then exmsg := ', будет урезан.' else exmsg := ', будет дополнен до (0, 0, 0, 1).';
							Log(HumanName(batch, aProg) + ': атрибут "' + aProg^.attrs[j].namae + '" в программе задан как ' + GLTypeIds[aProg^.attrs[j].typ] +
								', а в модели — как ' + GLTypeIds[batch^.batch^.va[i].Type_] + exmsg, logWarning);
						end;
					{$endif}
						map[j] := i;
						break;
					end;
				if map[j] < 0 then
				begin
				{$ifdef Debug} Log(HumanName(batch, aProg) + ': вершинный атрибут "' + aProg^.attrs[j].namae + '" не найден', logError); {$endif}
					ok := no;
				end;
			end;

			for i := 0 to High(batch^._drawables) do
				if (length(aProg^.attrs) = length(batch^._drawables[i].progs[0]^.attrs)) and
					(CompareByte(map, batch^._drawables[i].mapVat, length(aProg^.attrs) * sizeof(map[0])) = 0) then
				begin
					d := @batch^._drawables[i];
				{$ifdef DebugDrawables} Log('Батч+прога объединено: ' + tDrawable.HumanName(batch, d^.progs) + ' += ' + aProg^.name^.Ansi, logDebug); {$endif}
					SetLength(d^.progs, length(d^.progs) + 1);
					d^.progs[High(d^.progs)] := MakeRef(aProg);
				end;

			if not Assigned(d) then
			begin
				SetLength(batch^._drawables, length(batch^._drawables) + 1);
				d := @batch^._drawables[High(batch^._drawables)];
				SetLength(d^.progs, 1);
				d^.progs[0] := MakeRef(aProg);
				vb := batch^.Mesh^._glVB;
				ib := batch^.Mesh^._glIB;
				if ok then
				begin
					d^.GLvd := gl.CreateVertexDeclaration(vb, ib, batch^._mesh^.topology);
					d^.mapVat := map;
					d^.Update(batch);
				end else
					d^.GLvd := nil;

			{$ifdef Debug} stat.Note(max_drawables_in_mesh, length(batch^._drawables)); {$endif}
			{$ifdef DebugDrawables} Log('Батч+прога создано: ' + tDrawable.HumanName(batch, d^.progs), logDebug); {$endif}
			end;
		end;
		d^.lastTimeDrawn := mm.SceneTime;
		result := d^.GLvd;
	end;

	procedure tGLBatch.tDrawable.Finalize;
	var
		i: sint;
	begin
		if Assigned(GLvd) then
			gl.DeleteVertexDeclaration(GLvd);
		for i := 0 to High(progs) do
			Release(progs[i]);
		progs := nil;
	end;

	procedure tGLBatch.tDrawable.Update(glBatch: pGLBatch);
	var
		i, vatIdx: sint;
		batch: pBatch;
		prog: pSingleShaderProgram;
	begin
		if not Assigned(GLvd) then exit;
		batch := glBatch^.batch;
		prog := progs[0];

		for i := 0 to High(prog^.attrs) do
		begin
			vatIdx := mapVAT[i];
			if vatIdx >= 0 then
				if glBatch^._interleave then
					gl.SetVertexAttribute(GLvd, prog^.attrs[i].index, batch^.va[vatIdx].Type_, glBatch^._vertexSize, glBatch^._vbOffset + glBatch^._vertexLayout[vatIdx].offset)
				else
					gl.SetVertexAttribute(GLvd, prog^.attrs[i].index, batch^.va[vatIdx].Type_, 0, glBatch^.vaVb[vatIdx].ofs)
			else
				gl.SetVertexAttribute(GLvd, prog^.attrs[i].index, GLType.Float, 0, 0);
		end;
	end;

{$ifdef Debug}
	function tGLBatch.tDrawable.HumanName(batch: pGLBatch; const dprogs: array of pSingleShaderProgram): string;
	var
		i: sint;
	begin
		result := batch^.mesh^.mesh^.name + '[' + batch^.batch^.name + '] x ';
		for i := 0 to High(dprogs) do
		begin
			result += dprogs[i]^._name;
			if i < High(dprogs) then result += ', ';
		end;
	end;
{$endif}

	procedure tGLBatch._Load(vts, ids: boolean);
	var
		i: sint;
		inds: pMeshIndices;
		data: pointer;
		dataSize: size_t;
	begin
		if vts and (batch^.VerticesCount > 0) then
		begin
			if _interleave then
			begin
				dataSize := _vertexSize * size_t(_batch^.VerticesCount);
				data := GetMem(dataSize);
				_GetInterleaved(data, 0, _batch^.VerticesCount);
				gl.BufferSubData(_mesh^._glVB, _vbOffset, dataSize, data, yes);
			end else
				for i := 0 to High(_batch^.va) do
					gl.BufferSubData(_mesh^._glVB, vaVb[i].ofs, _batch^.va[i].DataSize, _batch^.va[i].Ptr);
		end;

		if ids then
		begin
			inds := @_batch^.inds;
			gl.BufferSubData(_mesh^._glIB, _ibOffset, inds^.Raw.DataSize, inds^.Raw.Ptr);

			for i := 1 to High(_mesh^._levels) do
				levelInds[i - 1].ibOffset := _ibOffset + GLTypeInfo[inds^.Typ].sizeof * _batch^.IndexOffset(i);
			indsLoaded := inds^.Raw.ReservedSize div GLTypeInfo[inds^.Raw.Type_].sizeof;
			indsLoadedType := inds^.Typ;
		end;

		// То, что происходит без (_batch^.VerticesCount > 0) с мешами, внезапно оставшимися с 0 вершинами, подозрительно похоже на багофичу ATI.
		if vts and (_batch^.VerticesCount > 0) then
			for i := 0 to High(_drawables) do
				_drawables[i].Update(@self);
	end;

	procedure tGLBatch._PartialUpdateVA(id: sint; start, count: uint);
	var
		v: pNativeGLValue;
		oneSize: size_t;
		data: pointer;
		dataSize: size_t;
	begin
		if start >= uint(batch^.VerticesCount) then exit;
		if start + count > uint(batch^.VerticesCount) then
			count := batch^.VerticesCount - start;

		if _interleave then
		begin
			Assert(id < 0);
			dataSize := count * _vertexSize;
			data := GetMem(dataSize);
			_GetInterleaved(data, start, count);
			gl.BufferSubData(_mesh^._glVB, _vbOffset + start * _vertexSize, _vertexSize * count, data, yes);
		end else
		begin
			v := @_batch^.va[id];
			oneSize := GLTypeInfo[v^.type_].sizeof;
			gl.BufferSubData(_mesh^._glVB, vaVb[id].ofs + start * oneSize, oneSize * count, v^.Ptr + start * oneSize);
		end;
	end;

	procedure tGLBatch._PartialUpdateInds(start, count: uint);
	var
		oneSize: size_t;
	begin
		if start >= uint(batch^.inds.Count) then exit;
		if start + count > uint(batch^.inds.Count) then
			count := batch^.inds.Count - start;

		oneSize := GLTypeInfo[_batch^.inds.Typ].sizeof;
		gl.BufferSubData(_mesh^._glIB, start * oneSize, oneSize * count, _batch^.inds.Raw.Ptr + start * oneSize);
	end;

	procedure tGLBatch.Draw(var rp: tParams4Renderable; level: sint; mat: pGLMaterialLevel);
	var
		vd: pGLVertexDeclaration;
		progID: sint;
		prog: pShaderProgram;
		sp: pSingleShaderProgram;
		rast: GLRasterizerState;
		rastf: GLRasterizerParams;
		ofs, count: uint;
	begin
		if (not Assigned(mat)) or (batch^.VerticesCount = 0) then exit;
		count := _batch^.IndicesCount[level];
		if count = 0 then exit;
		ofs := _ibOffset;
		if level > 0 then ofs += levelInds[level - 1].ibOffset;

		progID := mat^.GetProgID(rp.pass);
		if progID = -1 then exit;
		prog := mat^.pp[progID].prog;
		rp.matLevelParams := @mat^.gl;
		rp.mtlParams := @mat^.base^.gl;
		sp := prog^.Bind(rp);
		if not Assigned(sp) then exit;

		vd := tDrawable.Query(sp, @self);
		if not Assigned(vd) then exit;

		rastf := [GLrast_Cull];
		rast.cull := mat^.cull;
		if rp.useMatBlend then
		begin
			rast.blend := mat^.blend;
			Include(rastf, GLrast_Blend);
		end;
		gl.SetRasterizerState(rast, rastf);

		gl.DrawBatch(rp.rt, vd, sp^._inGL, ofs, count, _batch^.inds.Raw.Type_, rp.nInsta);
		_Sweep;
	end;

	constructor tGLMesh.Init(newMesh: pMesh; newDynamic: boolean = no);
	var
		i, j {$ifdef Debug}, nConvTotal {$endif} : sint;
	begin
		if not Assigned(newMesh) then Fail;
		inherited Init;
		_mesh := MakeRef(newMesh);
		_glVB := gl.CreateBuffer(GLbuffer_Vertex);
		_glIB := gl.CreateBuffer(GLbuffer_Index);
		_glVertexBufferSize := 0;
		_glIndexBufferSize := 0;
		_loadedGLVertexBufferSize := 0;
		_loadedGLIndexBufferSize := 0;

		batches := nil;
		topology := GLtopology_Tris;
		_dynamic := newDynamic;
		_partialUpdateV := nil;
		_partialUpdateI := nil;
		_updateV := upd_Whole;
		_updateI := upd_Whole;

		SetLength(_levels, length(_mesh^.levels));
		for i := 0 to High(_levels) do
		begin
			_levels[i].mesh := @self;
			_levels[i].id := i;
		end;

		if not dynamic then
		begin
		{$ifdef Debug} nConvTotal := 0; {$endif}
			for i := 0 to High(_mesh^.batches) do
				for j := 0 to High(_mesh^.batches[i].va) do
					if _mesh^.batches[i].va[j].VaFix then
					begin
					{$ifdef Debug} inc(nConvTotal); {$endif}
					end;
		{$ifdef Debug} if nConvTotal > 0 then Log('"' + _mesh^.name + '": ' + ToString(nConvTotal) + ' шт. VATs сконвертировано', logWarning); {$endif}
		end;
		for i := 0 to High(_mesh^.batches) do
			_mesh^.batches[i].inds.Typ := _mesh^.batches[i].inds.raw.IdFix(_mesh^.batches[i].inds.typ);

		SetLength(batches, length(_mesh^.batches));
		for i := 0 to High(_mesh^.batches) do
			batches[i].Init(@_mesh^.batches[i], @self);

		if not dynamic then
		begin
			_dynamic := yes;
			ConvertToStatic;
		end;
	end;

	constructor tGLMesh.Init(s: pStream; newDynamic: boolean = no);
	begin
		Init(new(pMesh, Init(s)), newDynamic);
		if not Assigned(_mesh) then Fail;
	end;

	destructor tGLMesh.Done;
	var
		i: sint;
	begin
		_ClearPartialUpdateV;
		_ClearPartialUpdateI;
		for i := 0 to High(batches) do batches[i].Done;
		Release(_mesh);
		gl.DeleteBuffer(_glIB);
		gl.DeleteBuffer(_glVB);
		inherited Done;
	end;

	function tGLMesh.FirstLevel: pGLMeshLevel;
	begin
		result := @_levels[0];
	end;

	function tGLMesh.GetLevel(const lod: float): pGLMeshLevel;
	var
		id: sint;
	begin
		id := _mesh^.GetLevelID(lod);
		if id >= 0 then
			result := @_levels[id]
		else
			result := nil;
	end;

	procedure tGLMesh._AddPartialUpdateV(newBatch, newIva: sint; newStart, newCount: uint);
	begin
		SetLength(_partialUpdateV, length(_partialUpdateV) + 1);
		with _partialUpdateV[High(_partialUpdateV)] do
		begin
			batch := newBatch;
			iva   := newIva;
			start := newStart;
			count := newCount;
		end;
	end;

	procedure tGLMesh._AddPartialUpdateI(newBatch: sint; newStart, newCount: uint);
	begin
		SetLength(_partialUpdateI, length(_partialUpdateI) + 1);
		with _partialUpdateI[High(_partialUpdateI)] do
		begin
			batch := newBatch;
			start := newStart;
			count := newCount;
		end;
	end;

	procedure tGLMesh._ClearPartialUpdateV;
	begin
		_partialUpdateV := nil;
	end;

	procedure tGLMesh._ClearPartialUpdateI;
	begin
		_partialUpdateI := nil;
	end;

	procedure tGLMesh._EnsureLoaded;
	var
		i: sint;
	begin
		case _updateV of
			upd_None: ;
			upd_Partial:
				begin
					for i := 0 to High(_partialUpdateV) do
						batches[_partialUpdateV[i].batch]._PartialUpdateVA(_partialUpdateV[i].iva, _partialUpdateV[i].start, _partialUpdateV[i].count);
					_ClearPartialUpdateV;
					_updateV := upd_None;
				end;
			upd_Whole:
				begin
					if (_loadedGLVertexBufferSize = 0) and (_loadedGLIndexBufferSize = 0) and (not dynamic) then
						_LoadInVBO
					else
						_RefreshInVBO(yes, _updateI = upd_Whole);
					_updateV := upd_None;
					if _updateI = upd_Whole then _updateI := upd_None;
				end;
		end;

		case _updateI of
			upd_None: ;
			upd_Partial:
				begin
					for i := 0 to High(_partialUpdateI) do
						batches[_partialUpdateI[i].batch]._PartialUpdateInds(_partialUpdateI[i].start, _partialUpdateI[i].count);
					_ClearPartialUpdateI;
					_updateI := upd_None;
				end;
			upd_Whole:
				begin
					Assert(_updateV <> upd_Whole);
					_RefreshInVBO(no, yes);
					_updateI := upd_None;
				end;
		end;
	end;

	procedure tGLMesh.Changed;
	begin
	{$ifdef Debug}
		if not dynamic then
		begin
			Log(_mesh^.name + ': невозможно обновить статический меш', logError);
			exit;
		end;
	{$endif}
		if _updateV = upd_Partial then _ClearPartialUpdateV;
		if _updateI = upd_Partial then _ClearPartialUpdateI;
		_updateV := upd_Whole;
		_updateI := upd_Whole;
	end;

	procedure tGLMesh.PartialChangedVA(batch: sint; v: pNativeGLValue; start, count: uint);
	var
		i, id: sint;
		oev: uint;
	begin
	{$ifdef Debug} Assert(dynamic, string(_mesh^.name)); {$endif}
		if batches[batch]._interleave then id := -1 else id := v - pNativeGLValue(batches[batch].batch^.va);

		if _updateV = upd_None then
		begin
			_updateV := upd_Partial;
			Assert(length(_partialUpdateV) = 0);
		end;
		case _updateV of
			upd_Partial:
				begin
					if (batches[batch]._interleave and (start + count < batches[batch]._vtsAllocated)) or
						((not batches[batch]._interleave) and (start + count < batches[batch].vaVb[id].vtsAllocated)) then
					begin
						for i := 0 to High(_partialUpdateV) do
							if (batch = _partialUpdateV[i].batch) and (batches[batch]._interleave or (id = _partialUpdateV[i].iva)) then
								if RangeIntersects(start, start + count, _partialUpdateV[i].start, _partialUpdateV[i].start + _partialUpdateV[i].count) then
								begin
									oev := _partialUpdateV[i].start + _partialUpdateV[i].count;
									_partialUpdateV[i].start := min(_partialUpdateV[i].start, start);
									_partialUpdateV[i].count := max(ilong(start) + count, oev) - _partialUpdateV[i].start;
									exit;
								end;
						_AddPartialUpdateV(batch, id, start, count);
					end else
					begin
						_ClearPartialUpdateV;
						_updateV := upd_Whole;
					end;
				end;
			upd_Whole: ;
		end;
	end;

	procedure tGLMesh.PartialChangedInds(batch: sint; level: uint; start, count: uint);
	var
		i: sint;
		oev: uint;
	begin
	{$ifdef Debug} Assert(dynamic, string(_mesh^.name)); {$endif}
		start += batches[batch].batch^.IndexOffset(level);

		if _updateI = upd_None then
		begin
			_updateI := upd_Partial;
			Assert(length(_partialUpdateI) = 0);
		end;
		case _updateI of
			upd_Partial:
				begin
					if (start + count < batches[batch].indsLoaded) and (batches[batch].batch^.inds.typ = batches[batch].indsLoadedType) then
					begin
						for i := 0 to High(_partialUpdateI) do
							if (batch = _partialUpdateI[i].batch) then
								if RangeIntersects(start, start + count, _partialUpdateI[i].start, _partialUpdateI[i].start + _partialUpdateI[i].count) then
								begin
									oev := _partialUpdateI[i].start + _partialUpdateI[i].count;
									_partialUpdateI[i].start := min(_partialUpdateI[i].start, start);
									_partialUpdateI[i].count := max(ilong(start) + count, oev) - _partialUpdateI[i].start;
									exit;
								end;
						_AddPartialUpdateI(batch, start, count);
					end else
					begin
						_ClearPartialUpdateI;
						_updateI := upd_Whole;
					end;
				end;
			upd_Whole: ;
		end;
	end;

	procedure tGLMesh.ConvertToStatic;
	var
		i, j: sint;
	begin
		if _dynamic then
		begin
			for i := 0 to High(_mesh^.batches) do
			begin
				_mesh^.batches[i].inds.raw.Pack;
				for j := 0 to High(_mesh^.batches[i].va) do
					_mesh^.batches[i].va[j].Pack;
			end;
			Changed;
			_dynamic := no;
			_EnsureLoaded;
		end;
	end;

	procedure tGLMesh.Draw(var rp: tParams4Renderable; const mat: array of pGLMaterialLevel);
	begin
		_Draw(rp, mat, 0);
	end;

	procedure tGLMeshLevel.Draw(var rp: tParams4Renderable; const mat: array of pGLMaterialLevel);
	begin
		mesh^._Draw(rp, mat, id);
	end;

	procedure tGLMesh._Draw(var rp: tParams4Renderable; const mat: array of pGLMaterialLevel; level: sint);
	var
		i: sint;
	begin
		_EnsureLoaded;
		for i := 0 to High(batches) do
			batches[i].Draw(rp, level, mat[i]);
	end;

	procedure tGLMesh._CalculateOffsets(vts, ids: boolean);
	var
	{$ifdef DebugInterleaving} prev, {$endif} curVbOffset, curIbOffset: PtrUint;
		allocated: size_t;
		i, j: sint;
	begin
		if vts then curVbOffset := 0;
		if ids then curIbOffset := 0;
		for i := Low(batches) to High(batches) do
		begin
			if vts then
			begin
				if batches[i]._interleave then
				begin
					curVbOffset := align(curVbOffset, gl.VaAlignment);
					allocated := batches[i].batch^.VerticesCount;
					if _dynamic then allocated := UpToPow2(allocated);
					batches[i]._vbOffset := curVbOffset;
					batches[i]._vtsAllocated := allocated;
					curVbOffset += allocated * batches[i]._vertexSize;
				end else
					for j := 0 to High(batches[i].batch^.va) do
					begin
					{$ifdef DebugInterleaving} prev := curVbOffset; {$endif}
						curVbOffset := align(curVbOffset, gl.VaAlignment);
					{$ifdef DebugInterleaving}
						if prev <> curVbOffset then
							Log('Смещение ' + batches[i].Human + ' (' + GLTypeIds[batches[i].batch^.va[j].type_] + ') выровнено: ' + ToString(prev) + ' -> ' + ToString(curVbOffset), logDebug);
					{$endif}
						allocated := batches[i].batch^.va[j].ReservedSize;
						batches[i].vaVb[j].ofs := curVbOffset;
						batches[i].vaVb[j].vtsAllocated := allocated div GLTypeInfo[batches[i].batch^.va[j].type_].sizeof;
						curVbOffset += allocated;
					end;
			end;
			if ids then
			begin
			{$ifdef DebugInterleaving} prev := curIbOffset; {$endif}
				curIbOffset := align(curIbOffset, gl.VaAlignment);
			{$ifdef DebugInterleaving}
				if prev <> curIbOffset then
					Log('Смещение индексов ' + batches[i].Human + ' (' + GLTypeIds[batches[i].batch^.inds.typ] + ') выровнено: ' + ToString(prev) + ' -> ' + ToString(curIbOffset), logDebug);
			{$endif}
				batches[i]._ibOffset := curIbOffset;
				curIbOffset += batches[i].batch^.inds.Raw.ReservedSize;
			end;
		end;
		if vts then _glVertexBufferSize := curVbOffset;
		if ids then _glIndexBufferSize := curIbOffset;
	end;

	procedure tGLMesh._LoadInVBO;
	begin
		_RefreshInVBO(yes, yes);
	{$IFDEF Debug}
		Log('"' + _mesh^.name + '", VRAM: vts = ' + ToStringSuff_b(_glVertexBufferSize) + ', ids = ' + ToStringSuff_b(_glIndexBufferSize) + '; ', logDebug);
	{$ENDIF}
		if not dynamic then _FreeData;
	end;

	procedure tGLMesh._RefreshInVBO(vts, ids: boolean);
	const
		BufUsages: array[boolean] of GLBufferUsage = (GLusage_StaticDraw, GLusage_DynamicDraw);
	var
		i: sint;
	begin
		Assert((_loadedGLVertexBufferSize = 0) or (dynamic), 'Cannot update static meshes');
		_CalculateOffsets(vts, ids);

		if vts and (_loadedGLVertexBufferSize <> _glVertexBufferSize) then
		begin
			gl.BufferData(_glVB, _glVertexBufferSize, nil, BufUsages[dynamic]);
			_loadedGLVertexBufferSize := _glVertexBufferSize;
		end;
		if ids and (_loadedGLIndexBufferSize <> _glIndexBufferSize) then
		begin
			gl.BufferData(_glIB, _glIndexBufferSize, nil, BufUsages[dynamic]);
			_loadedGLIndexBufferSize := _glIndexBufferSize;
		end;

		for i := 0 to High(batches) do
			batches[i]._Load(vts, ids);
	end;

	procedure tGLMesh._FreeData;
	begin
		_mesh^.FreeData;
	end;

	procedure tTexture._SetWrap(newWrap: GLTextureWrapMode);
	var
		p: GLTextureParamsRec;
	begin
		if _loaded then
		begin
			p.wrap := newWrap;
			gl.SetTextureParams(inGL, p, [GLtexparam_Wrap]);
		end;
		_wrap := newWrap;
	end;

	procedure tTexture._SetSwizzle(const newSwizzle: SwizzlePack);
	var
		p: GLTextureParamsRec;
	begin
		if _loaded then
		begin
			p.swizzle := newSwizzle;
			gl.SetTextureParams(inGL, p, [GLtexparam_Swizzle]);
		end;
		_swizzle := newSwizzle;
	end;

	procedure tTexture._Init(newIm: pTextureImage; newTarget: GLTextureTarget;
		const newSize: UintVec3; newFormat: GLImageFormat; imageFlags: TextureImageFlags; newMode: TextureMode);
	var
		data: array of pointer;
		i: sint;
	{$ifdef DebugImageFlip} t: Ticks; {$endif}
	begin
		if Assigned(newIm) then
		begin
			info := newIm^.info;
			SetLength(data, newIm^.nLevels);
			for i := 0 to High(data) do
				data[i] := newIm^.LevelPtr(i);

			if not (texture_Flipped in imageFlags) then
			begin
			{$ifdef DebugImageFlip} t := Ticks.Get; {$endif}
				for i := 0 to High(data) do
					FlipY(data[i], data[i], info.format, info.LevelSize(i));
			{$ifdef DebugImageFlip} FlipsTime += t.Elapsed; inc(FlipsCount) {$endif}
			end;
		end else
		begin
			Assert(newMode in [texture_Dynamic, texture_External]);
			info.Init(newTarget, newSize, newFormat, imageFlags);
			data := nil;
		end;
		inGL := gl.CreateTexture(target);

		_mode := newMode;
		_loaded := no;
		_wrap := GLwrap_Repeat;
		_swizzle := NoSwizzle;
		_Load2vmem(data);
		newIm^.Free(newIm);
	end;

	constructor tTexture.Init(s: pStream; flags: TextureImageFlags = []; const forceLoader: string = ''; size: size_t = 0);
	begin
		inherited Init;
		_Init(TextureImage.Create(s, forceLoader, size), GLTextureTarget(0), UintVec3.Zero, GLImageFormat(0), flags, texture_Static);
	end;

	constructor tTexture.Init(newIm: pTextureImage);
	begin
		inherited Init;
		_Init(newIm, GLTextureTarget(0), UintVec3.Zero, GLImageFormat(0), [], texture_Static);
	end;

	constructor tTexture.Init(newTarget: GLTextureTarget; const newSize: UintSize3; newFormat: GLImageFormat; imageFlags: TextureImageFlags; newMode: TextureMode);
	begin
		inherited Init;
		_Init(nil, newTarget, newSize, newFormat, imageFlags, newMode);
	end;

	destructor tTexture.Done;
	begin
		if Assigned(inGL) then gl.DeleteTexture(inGL);
		inherited Done;
	end;

	procedure tTexture.Resize(const newSize: UintSize3);
	begin
		if Size = newSize then exit;
		if _mode = texture_Static then
		begin
		{$ifdef Debug} Log('Статическую текстуру ресайзнуть нельзя', logError); {$endif}
			exit;
		end;

		info.Init(target, newSize, format, info.flags);
		_loaded := no;
		if _mode = texture_External then EnsureLoaded;
	end;

	procedure tTexture._Load2vmem(const data: array of pointer);
	var
		i: sint;
		curDataSize: size_t;
		cp: pointer;
		p: GLTextureParamsRec;
	begin
		for i := 0 to nLevels - 1 do
		begin
			if i < length(data) then
			begin
				curDataSize := info.GetLevelDataSize(i);
				cp := data[i];
			end else
			begin
				curDataSize := 0;
				cp := nil;
			end;
			gl.TexImage(inGL, info.LevelSize(i), format, curDataSize, cp, i);
		end;
		if target = GLtexture_Cube then _wrap := GLwrap_Clamp;
		p.wrap := _wrap;
		if texture_DontFilter in info.flags then
		begin
			p.magFilter := GLfilter_Nearest;
			p.minFilter := GLfilter_Nearest;
		end else
		begin
			p.magFilter := GLfilter_Linear;
			p.minFilter := GLfilter_Linear;
		end;
		if (target = GLtexture_2D) and (p.minFilter <> GLfilter_Nearest) then p.anisotropy := GLBase.Config.textureAnisotropy else p.anisotropy := 0.0;
		p.swizzle := _swizzle;
		gl.SetTextureParams(inGL, p, [GLtexparam_Wrap, GLtexparam_MagFilter, GLtexparam_MinFilter,
			GLtexparam_Aniso, GLtexparam_Swizzle]);
		_loaded := yes;
	end;

	procedure tTexture.EnsureLoaded;
	begin
		if not _loaded then _Load2vmem([]);
	end;

	procedure tTexture.SubImage(var im: TextureImage; const offset: UintOffset3; level: sint = 0);
	begin
		SubImage(offset, im.Size, im.format, im.info.GetLevelDataSize(0), im.FirstLevel, level);
	end;

	procedure tTexture.SubImage(const offset: UintOffset3; const size: UintSize3; format: GLImageFormat; dataSize: size_t; data: pointer; level: sint = 0; takeThisData: boolean = no);
	begin
		EnsureLoaded;
		gl.TexSubImage(inGL, offset, size, format, dataSize, data, level, takeThisData);
	end;

	procedure tTexture.Save(const fileName: string; level: sint);
	var
		mem: pointer;
		fmt: GLImageFormat;
	begin
		EnsureLoaded;
		fmt := ImageFormat8[max(3, GLImageFormatsInfo[format].nChannels)];
		mem := gl.GetTexImage(inGL, level, fmt);
		if not Assigned(mem) then raise Error('Не удалось получить изображение текстуры для {0}.', StreamPath.Human(fileName));
		try
			if _mode = texture_External then
				FlipY(mem, mem, fmt, info.LevelSize(level));

			try
				TextureImage.Save(fileName, info.LevelSizeXY(level), fmt, mem, yes);
			finally
				mem := nil;
			end;
		finally
			FreeMem(mem);
		end;
	end;

	function Postprocess._ReadFromTokens(var s: tTokenizer): boolean;
	var
		id: string;
	begin
		result := no;
		if s.ReadIdentifier <> 'postprocess' then exit;
		if not s.ReadBlockStart then exit;

		repeat
			case s.NextTokenType of
				token_Identifier: id := s.ReadIdentifier;
				token_BlockEnd: break;
				else exit;
			end;

			case id of
				'rt': if not _NewRt(s.ReadString)^.ReadFromTokens(s, @self) then exit;
				'pass': if not _NewPass(s.ReadString)^.ReadFromTokens(s, @self) then exit;
				else raise UnknownIdentifier(id, 'Postprocess');
			end;
		until no;

		s.ReadBlockEnd;
		result := yes;
	end;

	function Postprocess._NewRt(const newName: PoolString): pRTRec;
	var
		id: sint;
	begin
		id := _GetRTIdByName(newName);
		if id <> -1 then
		begin
		{$ifdef Debug} Log('Дублирующаяся RT: "' + newName + '"', logWarning); {$endif}
			exit(@_rts[id]);
		end;
		SetLength(_rts, length(_rts) + 1);
		result := @_rts[High(_rts)];
		result^.Initialize(newName);
	end;

	function Postprocess._NewPass(const newName: PoolString): pPassRec;
	begin
		SetLength(_passes, length(_passes) + 1);
		result := @_passes[High(_passes)];
		result^.Initialize(@self, newName);
	end;

	function Postprocess._GetRTIdByName(const name: PoolString {$ifdef Debug}; warn: boolean = no {$endif}): sint;
	begin
		result := Index(name.ToIndex, pRTRec(_rts) + fieldoffset RTRec _ name _, length(_rts), sizeof(RTRec));
	{$ifdef Debug} if (result < 0) and warn then Log('Рендертаргет "' + name + '" не найден.', logError); {$endif}
	end;

	constructor Postprocess.Init(s: pStream);
	var
		ts: pTokenizer;
		ok: boolean;
		i: sint;
	begin
		if not Assigned(s) then Fail;
		inherited Init;
	{$ifdef Debug}
		_name := StreamPath.Filename(s^.path);
		LogR('Загружаю постпроцесс из ' + StreamPath.Log(s^.path) + '... ');
	{$endif}
		ts := MakeRef(new(pTokenizer, Init(s)));
		ok := Assigned(ts) and _ReadFromTokens(ts^) and (length(_rts) > 0);
	{$ifdef Debug}
		if ok then
			Log('Постпроцесс "' + _name + '" загружен', logOK)
		else
			Log('Не удалось загрузить постпроцесс "' + _name + '"', logError);
	{$endif}
		Release(ts);
		if not ok then ConstructorFailed;
		for i := 0 to High(_passes) do _passes[i].Complete(@self);
		Postprocesses.Add(@self);
	end;

	destructor Postprocess.Done;
	var
		i: sint;
	begin
		Postprocesses.Remove(@self);
		_Discard;
		for i := 0 to High(_passes) do
			_passes[i].Finalize;
		for i := 0 to High(_rts) do
			_rts[i].Finalize;
		inherited Done;
	end;

	procedure Postprocess._Prepare;
	var
		i, j: sint;
		fmts: array of GLImageFormat;
	begin
		if not Assigned(_rts[0].instance) then
		begin
			for i := 0 to High(_rts) do
			begin
				Assert(not Assigned(_rts[i].instance));
				SetLength(fmts, length(_rts[i].colors));
				for j := 0 to High(fmts) do
					fmts[j] := _rts[i].colors[j].format;
				_rts[i].instance := new(pRenderTarget, Init({$ifdef Debug} _name + ': ' + {$endif} _rts[i].name,
					GLtexture_2D, fmts, _rts[i].size, _rts[i].depth));
			end;
		{$ifdef Debug} Log('Созданы рендертаргеты для постпроцесса "' + _name + '"', logDebug); {$endif}
		end;
	end;

	procedure Postprocess._Discard;
	var
		i: sint;
	begin
		if Assigned(_rts[0].instance) then
		begin
			for i := 0 to High(_rts) do
			begin
				Assert(Assigned(_rts[i].instance));
				dispose(_rts[i].instance, Done);
				_rts[i].instance := nil;
			end;
		{$ifdef Debug} Log('Уничтожены рендертаргеты постпроцесса "' + _name + '"', logDebug); {$endif}
		end;
	end;

	function Postprocess.PreparePass(const size: UintVec2): pRenderTarget;
	var
		i: sint;
	begin
		_Prepare;
		Postprocesses.Pump(@self);
		for i := 0 to High(_rts) do
		begin
			if _rts[i].sizeK <> 0.0 then
				_rts[i].instance^.Resize(max(UintVec2.Ones, UintRound(size * _rts[i].sizeK)));
			_rts[i].instance^.Prepare;
		end;
		result := _rts[0].instance;
	end;

	procedure Postprocess.Draw(rt: pGLRenderTarget; gl: pGLEntityParams);
	var
		i, j, pass, rtid: sint;
		rp: tParams4Renderable;
		rast: GLRasterizerState;
	begin
		MMSystem.gl.PushRasterizerState;
		rast.wire := [];
		rast.depthTest := no;
		rast.blend := GLblend_Off;
		MMSystem.gl.SetRasterizerState(rast, [GLrast_Wire, GLrast_DepthTest, GLrast_Blend]);

		rp.Reset;
		rp.roParams := gl;
		rp.useMatBlend := no;

		for pass := 0 to High(_passes) do
		begin
			for i := 0 to High(_passes[pass].pvs) do
			begin
				rtid := _passes[pass].sources[i];
				if rtid = -1 then continue;
				for j := 0 to High(_rts[rtid].colors) do
					_passes[pass].pvs[i].color[j]^.SetTex(_rts[rtid].instance^.color[j].tex);
				if Assigned(_passes[pass].pvs[i].depth) then
					if _rts[rtid].depthFrom < 0 then
						_passes[pass].pvs[i].depth^.SetTex(_rts[rtid].instance^.depth)
					else
						_passes[pass].pvs[i].depth^.SetTex(_rts[_rts[rtid].depthFrom].instance^.depth)
			end;

			if _passes[pass].dest <> DEST_BASERT then
				rp.rt := _rts[_passes[pass].dest].instance^.inGL
			else
				rp.rt := rt;

			if rp.rt^.size.Positive then
				UInvDestRTSizes^.SetVec2(1.0 / rp.rt^.size);

			Quad11.Draw(rp, _passes[pass].mat^.AnyLevel);

			for i := 0 to High(_passes[pass].pvs) do
			begin
				for j := 0 to High(_passes[pass].pvs[i].color) do
					_passes[pass].pvs[i].color[j]^.SetTex(nil);
				if Assigned(_passes[pass].pvs[i].depth) then
					_passes[pass].pvs[i].depth^.SetTex(nil);
			end;
		end;

		rp.Finalize;
		MMSystem.gl.PopRasterizerState;
	end;

{$ifdef Debug}
	procedure Postprocess.Dump(const baseFn: string);
	var
		i, j: sint;
		path, name, ext, fn: string;
	begin
		path := StreamPath.Path(baseFn);
		ext := StreamPath.Extension(baseFn);
		if ext = '' then ext := 'bmp';
		name := StreamPath.FilenameNoExt(baseFn);
		if name = '' then name := _name;
		if name = '' then name := 'pp';
		for i := 0 to High(_rts) do
		begin
			if not Assigned(_rts[i].instance) then
			begin
			{$ifdef Debug} Log('Дамп постпроцесса "' + baseFn + '": рендертаргет ' + _rts[i].name + ' недоступен', logWarning); {$endif}
				continue;
			end;
			for j := 0 to High(_rts[i].instance^.color) do
				if Assigned(_rts[i].instance^.color[j].tex) then
				begin
					fn := path + name + '_' + _rts[i].name + '_color' + ToString(j) + ExtensionSeparator + ext;
					_rts[i].instance^.color[j].tex^.Save(fn, 0);
				end;
		end;
	end;
{$endif}

	procedure Postprocess.RTRec.Initialize(const newName: PoolString);
	begin
		name := newName;
		size := UintVec2.Ones;
		sizeK := 1.0;
		colors := nil;
		SetNColors(1);
		depth := rt_NoDepth;
		depthFrom := -1;
		instance := nil;
	end;

	procedure Postprocess.RTRec.Finalize;
	begin
		if Assigned(instance) then dispose(instance, Done);
		colors := nil;
	end;

	procedure Postprocess.RTRec.SetNColors(newN: uint);
	var
		i, oldN: sint;
	begin
		oldN := length(colors);
		SetLength(colors, newN);
		for i := oldN to High(colors) do
			colors[i].format := GLformat_RGB;
	end;

	function Postprocess.RTRec.ReadFromTokens(var s: tTokenizer; pp: pPostprocess): boolean;
	var
		id: string;
		iv: sint;
	begin
		result := no;
		if not s.ReadBlockStart then exit;
		repeat
			case s.NextTokenType of
				token_Identifier: id := s.ReadIdentifier;
				token_BlockEnd: break;
				else exit;
			end;
			case id of
				'sizeX':
					begin
						size.X := s.ReadInteger;
						sizeK := 0.0;
					end;
				'sizeY': size.Y := s.ReadInteger;
				'sizeK': sizeK := s.ReadFloat;
				'depth': depth := RenderTargetDepthBehaviour(FindStr(s.ReadIdentifier, RenderTargetDepthBehaviourIds, ord(rt_NoDepth)));
				'depth_from': depthFrom := pp^._GetRTIdByName(s.ReadString);
				'tex':
					begin
						iv := s.ReadInteger;
						if iv >= length(colors) then SetNColors(iv + 1);
						if not s.ReadBlockStart then exit;
						repeat
							case s.NextTokenType of
								token_Identifier: id := s.ReadIdentifier;
								token_BlockEnd: break;
								else exit;
							end;
							case id of
								'format': colors[iv].format := GLImageFormat(FindStr(s.ReadIdentifier, GLImageFormatIds, ord(colors[iv].format)));
								else raise UnknownIdentifier(id, 'Postprocess.RT.tex');
							end;
						until no;
						s.ReadBlockEnd;
					end;
				else raise UnknownIdentifier(id, 'Postprocess.RT');
			end;
		until no;
		s.ReadBlockEnd;
		result := yes;
	end;

	procedure __RelocatePvs(old, new: pNativeGLValue; param: pObject);
	var
		pp: pPostprocess absolute param;
		pass, i, j: sint;
	begin
		for pass := 0 to High(pp^._passes) do
			for i := 0 to High(pp^._passes[pass].pvs) do
				with pp^._passes[pass].pvs[i] do
				begin
					for j := 0 to High(color) do
						if color[j] = old then
						begin
							color[j] := new;
							break;
						end;
					if depth = old then
					begin
						depth := new;
						break;
					end;
				end;
	end;

	procedure Postprocess.PassRec.Initialize(pp: pPostprocess; const newName: PoolString);
	begin
		mat := MakeRef(new(pGLMaterial, Init(newName)));
		mat^.gl.values.SetCallbacks(nil, @__RelocatePvs, pp);
		pvs := nil;
		SetLength(sources, 1);
		sources[0] := 0;
		dest := 0;
	end;

	procedure Postprocess.PassRec.Finalize;
	begin
		SetLength(sources, 0);
		SetLength(pvs, 0);
		Release(mat);
	end;

	function Postprocess.PassRec.ReadFromTokens(var s: tTokenizer; pp: pPostprocess): boolean;
	var
		id, nm: string;
		iv: sint;
		prog: pShaderProgram;
	begin
		result := no;
		if not s.ReadBlockStart then exit;
		repeat
			case s.NextTokenType of
				token_Identifier: id := s.ReadIdentifier;
				token_BlockEnd: break;
				else exit;
			end;
			if id = 'prog' then
			begin
				prog := ResourcePool.Shared^.LoadRef(TypeOf(ShaderProgram), StreamPath.Resolve(tShader.FileName(s.ReadString), s.StreamPath));
				mat^.AnyLevel^.Progs[@MainPass] := prog;
				Release(prog);
			end else
			if id = 'in' then
			begin
				iv := s.ReadInteger;
				if iv >= length(sources) then SetLength(sources, iv + 1);
				nm := s.ReadString;
				sources[iv] := pp^._GetRTIdByName(nm {$ifdef Debug}, yes {$endif});
			end else
			if id = 'out' then
				dest := pp^._GetRTIdByName(s.ReadString {$ifdef Debug}, yes {$endif})
			else
			if id = 'final' then
				dest := DEST_BASERT
			else
				raise UnknownIdentifier(id, 'Postprocess.Pass');
		until no;
		s.ReadBlockEnd;
		result := yes;
	end;

	procedure Postprocess.PassRec.Complete(pp: pPostprocess);
	var
		i, j, rtid: sint;
	begin
		SetLength(pvs, length(sources));
		for i := 0 to High(pvs) do
			pvs[i].depth := nil;

		for i := 0 to High(pvs) do
		begin
			rtid := sources[i];
			if rtid = -1 then continue;
			SetLength(pvs[i].color, length(pp^._rts[rtid].colors));
			for j := 0 to High(pvs[i].color) do
				pvs[i].color[j] := mat^.gl.values.Value('rt' + ToString(i) + '_color' + ToString(j), GLType.Sampler);
			if (pp^._rts[rtid].depth <> rt_NoDepth) or (pp^._rts[rtid].depthFrom >= 0) then
				pvs[i].depth := mat^.gl.values.Value('rt' + ToString(i) + '_depth', GLType.Sampler)
			else
				pvs[i].depth := nil;
		end;
	end;

	constructor PostprocessRegistry.Init;
	begin
		_reg := nil;
	end;

	destructor PostprocessRegistry.Done;
	begin
	end;

	procedure PostprocessRegistry.Add(newPP: pPostprocess);
	begin
		SetLength(_reg, length(_reg) + 1);
		_reg[High(_reg)].pp := newPP;
		_reg[High(_reg)].lastFrameUsed := mm.FrameNo;
	end;

	procedure PostprocessRegistry.Remove(pp: pPostprocess);
	var
		i: sint;
	begin
		for i := 0 to High(_reg) do
			if _reg[i].pp = pp then
			begin
				_reg[i] := _reg[High(_reg)];
				SetLength(_reg, length(_reg) - 1);
				break;
			end;
	end;

	procedure PostprocessRegistry.Pump(pp: pPostprocess);
	var
		i: sint;
	begin
		for i := 0 to High(_reg) do
			if _reg[i].pp = pp then
			begin
				_reg[i].lastFrameUsed := mm.FrameNo;
				break;
			end;
	end;

	procedure PostprocessRegistry.Cleanup;
	var
		i: sint;
	begin
		for i := 0 to High(_reg) do
			if abs(_reg[i].lastFrameUsed - mm.FrameNo) > 100 then _reg[i].pp^._Discard;
	end;

	procedure _InitQuad01(out q: tGLMesh; is11: boolean; rotate: boolean);
	const
		AI = Low(sbyte); BI = High(sbyte);
		AU = Low(byte);  BU = High(byte);
		Quad01Verts: array[0..3] of
		record
			pos2_01: Vec2u8;
			pos2_11: Vec2s8;
			tex, tex2: Vec2u8;
			rotatex: sint;
		end =
		(
			(pos2_01: (AU, BU); pos2_11: (AI, BI); tex: (AU, AU); tex2: (AU, BU); rotatex: 2),
			(pos2_01: (AU, AU); pos2_11: (AI, AI); tex: (AU, BU); tex2: (AU, AU); rotatex: 0),
			(pos2_01: (BU, BU); pos2_11: (BI, BI); tex: (BU, AU); tex2: (BU, BU); rotatex: 3),
			(pos2_01: (BU, AU); pos2_11: (BI, AI); tex: (BU, BU); tex2: (BU, AU); rotatex: 1)
		);
		Quad01Inds: array[0..3] of sint = (0, 1, 2, 3);
		Quad01Norm: Vec3u8 = (0, 0, 255);
		function sd(s, d: GLType): GLType;
		begin
			if is11 then result := s else result := d;
		end;
	var
		i, ti: sint;
		b: pBatch;
		m: pMesh;
		v_pos2, v_tex, v_tex2, v_norm: pNativeGLValue;
	begin
		m := new(pMesh, Init({$ifdef Debug}'Quad' + ToString(uint(is11)) + '1'{$endif}));
		b := m^.AddBatch('');
		b^.VerticesCount := length(Quad01Verts);
		b^.AddVA('pos2d', sd(GLType.Vec2Ni8, GLType.Vec2Nui8));
		b^.AddVA('tex', GLType.Vec2Nui8);
		b^.AddVA('tex2d', GLType.Vec2Nui8);
		b^.AddVA('normal', GLType.Vec3Nui8);
		v_pos2 := b^.FindVA('pos2d');
		v_tex := b^.FindVA('tex');
		v_tex2 := b^.FindVA('tex2d');
		v_norm := b^.FindVA('normal');

		if is11 then m^.Bounding := Bounding.BySphere(Vec3.Zero, 1.0) else m^.Bounding := Bounding.BySphere(Vec3.Make(0.5, 0.5, 0.0), 0.5);
		for i := 0 to High(Quad01Verts) do
		begin
			if is11 then
				v_pos2^.RawVec2i8[i] := Quad01Verts[i].pos2_11
			else
				v_pos2^.RawVec2ui8[i] := Quad01Verts[i].pos2_01;
			if rotate then ti := Quad01Verts[i].rotatex else ti := i;
			v_tex^.RawVec2ui8[i] := Quad01Verts[ti].tex;
			v_tex2^.RawVec2ui8[i] := Quad01Verts[ti].tex2;
			v_norm^.RawVec3ui8[i] := Quad01Norm;
		end;
		b^.inds.Count := length(Quad01Inds);
		for i := 0 to High(Quad01Inds) do
			b^.inds[i] := Quad01Inds[i];
		q.Init(m); q.MakeStatic;
		q.topology := GLtopology_TriStrip;
	end;

	procedure GLClasses_Init;
	var
		i: sint;
	begin
		if not GLBase.Config.allowMT then
		begin
		{$ifdef Debug}
			Log('Рендер в отдельном потоке выключен. ResourcePool не имеет права удалять GL-объекты извне.', logWarning);
		{$endif}
			// с текущей реализацией AddToGLGraveyard это всегда так.
		end;
	{$ifdef Debug} Log('Загрузчики изображений: {0}.', TextureImage.Loaders.Dump); {$endif}
	{$ifdef Debug} Log('Загрузчики моделей: {0}.', Mesh.Loaders.Dump); {$endif}

		try
			MainRT.InitMain(UintVec2.Make(mm.window.sizeX, mm.window.sizeY));
			_InitQuad01(Quad01, no, no);
			_InitQuad01(Quad11, yes, no);
			_InitQuad01(Quad01CW, no, yes);
		{$ifdef use_serialization}
			SerializationDB.Shared^
			.AddEnv([@Quad01, @Quad11]);
		{$endif}

			RTpool.Init;
			Postprocesses.Init;
			ShaderDefines^.AddFlag('CLIP_PLANE', GL_FLAG_CLIP_PLANE);
			ShaderDefines^.AddFlag('UNDERWATER', GL_FLAG_UNDERWATER);

			for i := 0 to High(GlobalGLRelocations) do
				with GlobalGLRelocations[i] do
					reloc^ := GlobalGL.values.Value(name, typ, count, flags);
		except
			Exception.Fatal;
		end;
	end;

	procedure GLClasses_Done;
	begin
	{$ifdef DebugImageFlip}
		if tTexture.FlipsCount > 0 then
		begin
		{$ifdef Debug} Log {$else} Info.Show {$endif}
			(Format('{0} за {1}.', lang_amount(tTexture.FlipsCount, '{N} текстур{а/ы/} перевёрнуты'), ToString(tTexture.FlipsTime))
			{$ifdef Debug}, logDebug {$endif});
		end;
	{$endif}
		Postprocesses.Done;
		RTpool.Done;

		Quad01CW.Done;
		Quad11.Done;
		Quad01.Done;
		MainRT.Done;
	end;

	procedure GLOnEndFrame;
	begin
		Postprocesses.Cleanup;
	end;

	procedure Script_GL_RegShaderFlags(var ss: ScriptState);
	var
		what: string;
	begin
		ss.PushNil;
		while ss.Next(1) do
		begin
			if ss.IsTable(-1) then
			begin
				what := ss.ToString(-2);
				if what = 'specials' then
				begin
					ss.PushNil;
					while ss.Next(-2) do
					begin
						ShaderDefines^.AddSpecial(ss.ToString(-2), ss.ToSint(-1));
						ss.Pop;
					end;
				end else
					ss.UnknownIdentifier(what);
			end else
				ShaderDefines^.AddFlag(ss.ToString(-2), ss.ToSint(-1));
			ss.Pop;
		end;
	end;

	function Script_GL_Wire(var ss: ScriptState): sint;
	var
		rast: GLRasterizerState;
	begin
		rast := gl.RasterizerState;
		if ss.Top > 0 then
		begin
			if ss.IsString(1) and (ss.ToString(1) = 'toggle') then
			begin
				if rast.wire = [] then rast.wire := [GLface_Front, GLface_Back] else
				if rast.wire = [GLface_Front, GLface_Back] then
					if gl.WiresMustMatch then
						rast.wire := []
					else
						rast.wire := [GLface_Back]
				else
					rast.wire := [];
			end else
			begin
				if ss.ToBool(1) then Include(rast.wire, GLface_Front);
				if ss.ToBool(2) then Include(rast.wire, GLface_Back);
			end;
			gl.SetRasterizerState(rast, [GLrast_Wire]);
		end;
		ss.PushBool(GLface_Front in rast.wire);
		ss.PushBool(GLface_Back in rast.wire);
		result := 2;
	end;

var
	simultaneousScreenshots: casint_t = 0;
	erroneousScreenshots: casint_t = 0;

type
	ScreenshotTaskParam = object
		format: GLImageFormat;
		size: UintVec2;
		stream: string;
		chunk: pointer;
		procedure Close;
	end;

	procedure ScreenshotTaskParam.Close;
	begin
		FreeMem(chunk);
		dispose(@self);
		InterlockedDecrement(simultaneousScreenshots);
	end;

	procedure _ScreenshotTask(param: pointer);
	var
		p: ^ScreenshotTaskParam absolute param;
		fn, path, ext, timestamp: string;
	begin
		timestamp := ToString(DateTime.GetLocal, '{D.M.Y} {h}：{m}：{s}.{ms}');
		path := StreamPath.Path(p^.stream);
		ext := StreamPath.Extension(p^.stream); if ext = '' then ext := 'png';
		fn := StreamPath.FilenameNoExt(p^.stream); if fn = '' then fn := timestamp;
		fn := path + StrReplace(fn, '%TIMESTAMP%', timestamp) + ExtensionSeparator + ext;

		FlipY(p^.chunk, p^.chunk, p^.format, p^.size);
		try
			try
				TextureImage.Save(fn, p^.size, p^.format, p^.chunk, yes);
			finally
				p^.chunk := nil;
			end;
		except
		{$ifdef Debug} Log(Exception.Message, logError); {$endif}
			if InterlockedIncrement(erroneousScreenshots) = 1 then
				Warning.Show('Не удалось сохранить скриншот.' + EOL + Exception.Message);
			InterlockedDecrement(erroneousScreenshots);
		end;
		p^.Close;
	end;

	procedure Script_GL_Screenshot(var ss: ScriptState);
	const
		Format = GLformat_RGB;
		MaxSimultaneousScreenshots = 2;
	var
		p: ^ScreenshotTaskParam;
	begin
		if InterlockedIncrement(simultaneousScreenshots) > MaxSimultaneousScreenshots then
		begin
		{$ifdef Debug} Log('Превышено максимальное количество одновременно сохраняемых скриншотов (' + ToString(MaxSimultaneousScreenshots) + ')!', logWarning); {$endif}
			InterlockedDecrement(simultaneousScreenshots);
			exit;
		end;

		new(p);
		p^.stream := ss.ToString(1);
		p^.size := MainRT.Size;
		p^.format := Format;
		p^.chunk := gl.GetRTImage(MainRT.inGL, Format);
		if Assigned(p^.chunk) then
			Work.Queue(@_ScreenshotTask, p)
		else
			p^.Close;
	end;

	procedure Script_GL_Quad01(var ss: ScriptState);
	begin
		ss.PushObject(@Quad01);
	end;

	procedure Script_GL_Quad11(var ss: ScriptState);
	begin
		ss.PushObject(@Quad11);
	end;

	function Script_GL_U(var ss: ScriptState): sint;
	begin
		result := Script_modify_gl_and_also_query(ss, 1, GlobalGL);
	end;

	procedure OpenScript(var script: ScriptState);
	const
		Stuff: array[0 .. 5] of ScriptStuffDesc =
		(
			(s: FunctionsDesc + 'GL' + PrefixedFunctions + 'Wire'; p: @Script_GL_Wire),
			(s: 'Screenshot:0'; p: @Script_GL_Screenshot),
			(s: 'RegShaderFlags:0'; p: @Script_GL_RegShaderFlags),
			(s: 'Quad01:1'; p: @Script_GL_Quad01),
			(s: 'Quad11:1'; p: @Script_GL_Quad11),
			(s: 'U'; p: @Script_GL_U)
		);
	begin
		script.AddStuff(Stuff);
	end;

{$ifdef use_serialization}
const
	NLODS_HAS_GL_BIT = 1 shl 7;
	NPP_CULL_BIT     = 1 shl 7;
	NPP_HAS_GL_BIT   = 1 shl 6;

	procedure SerializeGLMaterial(se: pSerializer; obj: pointer);
	var
		m: pGLMaterial absolute obj;
		lv: pGLMaterialLevel;
		i, j: sint;
		u: uint;
	begin
		with se^, m^ do
		begin
			u := length(lods);
			if not gl.Empty then u := u or NLODS_HAS_GL_BIT;
			Serialize_ui8(stream, u);
			if not gl.Empty then SeObject(@gl, TypeOf(gl));

			for i := 0 to High(lods) do
			begin
				lv := @lods[i];
				u := length(lv^.pp);
				if lv^.cull then u := u or NPP_CULL_BIT;
				if not lv^.gl.Empty then u := u or NPP_HAS_GL_BIT;

				Serialize_ui8(stream, u);
				Serialize_fN8(stream, lv^.minLod, 0.0, 1.0);
				for j := 0 to High(lv^.pp) do
				begin
					SeObject(lv^.pp[j].pass);
					SeObject(lv^.pp[j].prog);
				end;
				Serialize_ui8(stream, ord(lv^.blend));
				if not lv^.gl.Empty then SeObject(@lv^.gl, TypeOf(lv^.gl));
			end;
		end;
	end;

	procedure DeserializeGLMaterial(de: pDeserializer; obj: pointer);
	var
		m: pGLMaterial absolute obj;
		lv: pGLMaterialLevel;
		i, j: sint;
		u: uint;
		hasGL: boolean;
	begin
		with de^, m^ do
		begin
			u := Deserialize_ui8(stream);
			hasGL := (NLODS_HAS_GL_BIT and u) <> 0; if hasGL then u := u xor NLODS_HAS_GL_BIT;
			if hasGL then DeWeakAtR(gl) else gl.Init;

			SetLength(lods, u);
			for i := 0 to High(lods) do
			begin
				lv := @lods[i];
				u := Deserialize_ui8(stream);
				lv^.cull := (u and NPP_CULL_BIT) <> 0; if lv^.cull then u := u xor NPP_CULL_BIT;
				hasGL := (u and NPP_HAS_GL_BIT) <> 0; if hasGL then u := u xor NPP_HAS_GL_BIT;

				lv^.minLod := Deserialize_fN8(stream, 0.0, 1.0);
				SetLength(lv^.pp, u);
				for j := 0 to High(lv^.pp) do
				begin
					DeObjectA(lv^.pp[j].pass);
					DeObjectA(lv^.pp[j].prog);
				end;
				lv^.blend := GLBlendMode(Deserialize_ui8(stream));
				if hasGL then DeWeakAtA(lv^.gl) else lv^.gl.Init;
			end;
		end;
	end;

	procedure GLMaterialDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		m: pGLMaterial absolute obj;
		m2: pGLMaterial;
		i: sint;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: m^.DeseInit;
			de_After:
				begin
					m2 := GLMaterial.Merge(m);
					if m2 = m then
					begin
						for i := 0 to High(m^.lods) do
							m^.lods[i].base := m;
					end;
					m := m2;
				end;
		end;
	end;
{$endif}

	procedure __RelocateGlobalGL(old, new: pNativeGLValue; param: pObject);
	var
		i: sint;
	begin
		Assert(@param = @param);
		for i := 0 to High(GlobalGLRelocations) do
			if GlobalGLRelocations[i].reloc^ = old then
			begin
				GlobalGLRelocations[i].reloc^ := new;
				exit;
			end;
	{$ifdef Debug} Log('Relocate пользовательского глобального GL-параметра: "' + new^.namae + '"', logDebug); {$endif}
	end;
	
	function LoadTexture(s: pStream): pObject;
	var
		tex: pTexture absolute result;
		i, j, t: sint;
		fn: string;
		ok: boolean;
		swz: SwizzlePack;
	begin
		result := nil;
		if not Assigned(s) then exit;
		fn := StreamPath.FilenameNoExt(s^.path);
		tex := new(pTexture, Init(s));
		if Pos('[c]', fn) > 0 then tex^.Wrap := GLwrap_Clamp;
		for i := 1 to length(fn) - 1 do
			if fn[i] = '[' then
			begin
				swz := NoSwizzle;
				ok := yes;
				for j := i + 1 to min(i + 4, length(fn)) do
				begin
					if fn[j] = ']' then break;
					t := FindStr(fn[j], SwizzleIds);
					if t = -1 then
					begin
						ok := no;
						break;
					end;
					swz[j - i - 1] := Swizzle(t);
				end;
				if ok then
					tex^.Swizzle := swz;
			end;
	end;
	function LoadShader(s: pStream): pObject;        begin result := new(pShader, Init(s)); end;
	function LoadShaderProgram(s: pStream): pObject; begin result := new(pShaderProgram, Init(s)); end;
	function LoadGLMesh(s: pStream): pObject;        begin result := new(pGLMesh, Init(s, no)); end;
	function LoadPostprocess(s: pStream): pObject;   begin result := new(pPostprocess, Init(s)); end;

	procedure Init;
	begin
		GLMaterial._nInstances := 0;
		ResourcePool.Shared
		^.Register(TypeOf(tTexture), @LoadTexture)^.Tag(GLResourceTag)
		^.Register(TypeOf(tShader), @LoadShader)^.Tag(GLResourceTag)
		^.Register(TypeOf(ShaderProgram), @LoadShaderProgram)^.Tag(GLResourceTag)
		^.Register(TypeOf(tGLMesh), @LoadGLMesh)^.Tag(GLResourceTag)
		^.Register(TypeOf(Postprocess), @LoadPostprocess)^.Tag(GLResourceTag);

		GlobalGL.Init;
		GlobalGL.values.SetCallbacks(nil, @__RelocateGlobalGL, nil);

	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterFunc(@__RelocateGlobalGL)
		^.RegisterType('Texture', TypeOf(tTexture), nil, sizeof(tTexture), yes, nil, nil, nil, nil)
		^.RegisterType('Shader program', TypeOf(ShaderProgram), nil, sizeof(ShaderProgram), yes, nil, nil, nil, nil)
		^.RegisterType('GL material', TypeOf(GLMaterial), nil, sizeof(GLMaterial), yes,
		               @SerializeGLMaterial, @DeserializeGLMaterial, nil, @GLMaterialDeSpecial)
		^.RegisterType('GL mesh', TypeOf(tGLMesh), nil, sizeof(tGLMesh), yes, nil, nil, nil, nil)
		^.RegisterType('Postprocess', TypeOf(Postprocess), nil, sizeof(Postprocess), yes, nil, nil, nil, nil);
	{$endif}
	end;

	procedure Done;
	begin
		GlobalGL.Done;
	end;

initialization
	&Unit('GLClasses').Initialize(@Init, @Done);
end.
