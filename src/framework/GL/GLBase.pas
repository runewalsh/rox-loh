unit GLBase;

{$include opts.inc}
{$ifdef Debug}
	{-$define DebugNativeGLValues}
	{-$define DebugSlideGL}
{$endif}

interface

uses
	USystem, Errors, UClasses, UMath, Utils, Tokenizer, Streams, U_GL, SpatialIndex, Algo, Human
{$ifdef Debug}, ULog {$endif};

type
	tLightBase = (light_Omni, light_Targeted);
	tLightDetail = (light_OmniA, light_OmniS, light_TargetedA, light_TargetedS);
	tLightDetails = set of tLightDetail;
	tLightUniform = (ulight_All, ulight_Omni, ulight_OmniS, ulight_Targeted, ulight_TargetedS);
	tLightUniforms = set of tLightUniform;

const
	MAX_OMNI_A = 8;
	MAX_OMNI_S = 4;
	MAX_TARG_A = 2;
	MAX_TARG_S = 2;
	MAX_ANY_LIGHTS = MAX_OMNI_A;
	MAX_LIGHTS: array[tLightDetail] of sint = (MAX_OMNI_A, MAX_OMNI_S, MAX_TARG_A, MAX_TARG_S);
	MAX_INSTA_LIMIT = 64;

	MaxGLBones = 80;
	MAX_CSM_SPLITS = 4;
	NoClipPlane: Vec4 = (data: (0.0, 0.0, 0.0, 1.0));

	UNIFORMS_SUPPLY = MaxGLBones * (3 + 4 + 3 + 4) + MAX_ANY_LIGHTS * 16 + 128 + 64;
	BASE_UNIFORMS_PER_INSTANCE = 4 * 4; // mvpos + mvtrans + modelpos + modeltrans
	MAX_UNIFORMS_PER_INSTANCE = BASE_UNIFORMS_PER_INSTANCE + 12;

type
	pNativeGLValueData = ^tNativeGLValueData;
	tNativeGLValueData = record
	case uint8 of
		0: (_ptr: pointer);
		1: (_float: pGLFloat); 2: (_v2f: pVec2f); 3: (_v3f: pVec3f); 4: (_v4f: pVec4f);
		5: (_mat4: pMat4f);
		6: (_obj: pObject);
		7: (_uint: pUint32); 8: (_ushort: pUint16); 9: (_ubyte: pByte);
		10: (_i16: pSint16); 11: (_v2i16: pVec2s16); 12: (_v3i16: pVec3s16); 13: (_v4i16: pVec4s16);
		14: (_i8: pSbyte); 15: (_v2i8: pVec2s8); 16: (_v3i8: pVec3s8); 17: (_v4i8: pVec4s8);
		18: (_ui16: pUint16); 19: (_v2ui16: pVec2u16); 20: (_v3ui16: pVec3u16); 21: (_v4ui16: pVec4u16);
		22: (_ui8: pByte); 23: (_v2ui8: pVec2u8); 24: (_v3ui8: pVec3u8); 25: (_v4ui8: pVec4u8);
		26: (_half: pHalf); 27: (_v2h: pVec2h); 28: (_v3h: pVec3h); 29: (_v4h: pVec4h);
		30: (_int32: pSint32); 31: (_int16: pSint16); 32: (_int8: pSint8);
	end;

	scoped_enum_ NativeGLValueFlag = (InstaPack, NonSerializable); _end
	NativeGLValueFlags = set of NativeGLValueFlag;

	// TODO: вынести версии в NativeGLValues, запилить взамен какой-нибудь onChange.
	pNativeGLValue = ^NativeGLValue;
	NativeGLValue = object
	public type
		tVersion = type uint;
	private
		_namae: PoolString;
		_type: GLType;
		_count, _allocated: sint;
		_flags: NativeGLValueFlags;
		data: tNativeGLValueData;
		_version: tVersion;
		_nextVersion: tVersion; static;
		function _RawGet(i: sint): pointer;
		procedure _RawSet(i: sint; p: pointer);
		procedure _SetCount(newCount: sint);
		function _GetDataSize: size_t;
		function _GetReservedSize: size_t;
		function _NewVersion: tVersion; static;
		procedure _SetFloat(id: sint; const x: float);
		procedure _SetVec2(id: sint; const x: Vec2);
		procedure _SetVec3(id: sint; const x: Vec3);
		procedure _SetVec4(id: sint; const x: Vec4);
	public
		procedure Initialize(const newName: PoolString; newType: GLType; newCount: sint = 1; newFlags: NativeGLValueFlags = []);
		procedure Initialize(const newName: PoolString; newType: GLType; newFlags: NativeGLValueFlags);
		procedure Initialize(const v2: NativeGLValue);
		procedure Finalize;
		procedure ConvertTo(t2: GLType);
		function Equals(const v2: NativeGLValue; maxCount: sint = -1): boolean;
		procedure FreeData;
		procedure Pack;
		procedure CopyRange(from, to_, n: sint);
		procedure SetFloat(const x: float; id: sint = 0);
		procedure SetVec2(const v: Vec2; id: sint = 0);
		procedure SetVec3(const v: Vec3; id: sint = 0);
		procedure SetVec4(const v: Vec4; id: sint = 0);
		procedure SetMat4(const m: Matrix4; id: sint = 0);
		procedure SetTex(newTex: pObject; id: sint = 0);
		procedure SetInt(x: sint; id: sint = 0);
		function ToFloat(id: sint): float;
		function ToVec2(id: sint): Vec2;
		function ToVec3(id: sint): Vec3;
		function ToVec4(id: sint): Vec4;
		function Hash: Hash.Value;
		function VaFix: boolean;
		function VaFix(typ: GLType): GLType; static;
		function IdFix(typ: GLType): GLType; static;

		property RawI[i: sint]: pointer read _RawGet write _RawSet; default;
	public
		property Namae: PoolString read _namae;
		property Type_: GLType read _type;
		property Count: sint read _count write _SetCount;
		property DataSize: size_t read _GetDataSize;
		property ReservedCount: sint read _allocated;
		property ReservedSize: size_t read _GetReservedSize;
		property Flags: NativeGLValueFlags read _flags;

		property Version: tVersion read _version;
		property Ptr: pointer read data._ptr;
		property AsFloat[id: sint]: float read ToFloat write _SetFloat;
		property AsVec2[id: sint]: Vec2 read ToVec2 write _SetVec2;
		property AsVec3[id: sint]: Vec3 read ToVec3 write _SetVec3;
		property AsVec4[id: sint]: Vec4 read ToVec4 write _SetVec4;
		property RawFloat: pGLFloat read data._float; property RawVec2: pVec2f read data._v2f; property RawVec3: pVec3f read data._v3f; property RawVec4: pVec4f read data._v4f;
		property RawMat4: pMat4f read data._mat4;
		property RawTex: pObject read data._obj;
		property RawUbyte: pByte read data._ubyte; property RawUshort: pUint16 read data._ushort; property RawUint: pUint32 read data._uint;
		property RawVec2i16: pVec2s16 read data._v2i16; property RawVec3i16: pVec3s16 read data._v3i16; property RawVec4i16: pVec4s16 read data._v4i16;
		property RawVec2i8: pVec2s8 read data._v2i8; property RawVec3i8: pVec3s8 read data._v3i8; property RawVec4i8: pVec4s8 read data._v4i8;
		property RawVec2ui16: pVec2u16 read data._v2ui16; property RawVec3ui16: pVec3u16 read data._v3ui16; property RawVec4ui16: pVec4u16 read data._v4ui16;
		property RawVec2ui8: pVec2u8 read data._v2ui8; property RawVec3ui8: pVec3u8 read data._v3ui8; property RawVec4ui8: pVec4u8 read data._v4ui8;
		property RawHalf: pHalf read data._half; property RawVec2Half: pVec2h read data._v2h;
			property RawVec3Half: pVec3h read data._v3h; property RawVec4Half: pVec4h read data._v4h;
		property RawInt32: pSint32 read data._int32; property RawInt16: pSint16 read data._int16; property RawInt8: pSint8 read data._int8;
	end;

	{$define classname := Name2NativeGLValue} {$define key_type := PoolString} {$define value_type := pNativeGLValue} {$define null_value:=nil}
	{$include hash.h.inc}

	pNativeGLValues = ^NativeGLValues;
	NativeGLValues = object
	public type
		OnAddProc = procedure(v: pNativeGLValue; param: pObject);
		OnRelocateProc = procedure(old, new: pNativeGLValue; param: pObject);
	private var
		_hash: Name2NativeGLValue;
		_onAdd: OnAddProc;
		_onRelocate: OnRelocateProc;
		_userParam: pObject;
		procedure _ReallocRaw(newSize: sint);
		function _GetHash(v: pNativeGLValues): Hash.Value; static;
		function _Equals(a, b: pNativeGLValues): boolean; static;
	public var
		raw: array of NativeGLValue;
		procedure Initialize;
		procedure Initialize(var cp: NativeGLValues);
		procedure Finalize;
		procedure SetCallbacks(add: OnAddProc; relocate: OnRelocateProc; param: pObject);
		procedure AddRaw(var v: NativeGLValue);
		procedure Remove(const name: PoolString);
		function Value(const name: PoolString): pNativeGLValue;
		function Value(const name: PoolString; typ: GLType; count: sint = 1; flags: NativeGLValueFlags = []): pNativeGLValue;
		function GetID(const name: PoolString): sint;
		procedure ForceCleanup;
		function Hash: Hash.Value;
		function Equals(var b: NativeGLValues): boolean;
	end;

	RenderPassSpecial = (pass_Generic, pass_UseLights);

	pRenderPass = ^RenderPass;
	RenderPass = object(&Object)
	private
		_static: boolean;
	public
		name: PoolString;
		special: RenderPassSpecial;
		constructor Init(const newName: PoolString; newSpecial: RenderPassSpecial; static: boolean = no);
		destructor Done; virtual;
	end;

	RenderScenarioFlag = (scenario_Uvis, scenario_Static);
	RenderScenarioFlags = set of RenderScenarioFlag;

	pRenderScenario = ^RenderScenario;
	RenderScenario = object(&Object)
		name: PoolString;
		passes: array of pRenderPass;
		flags: RenderScenarioFlags;
		constructor Init(const newName: PoolString; const newPasses: array of pRenderPass; newFlags: RenderScenarioFlags);
		destructor Done; virtual;
	end;

	{$define classname := RenderPassesSet} {$define key_type := pRenderPass} {$define inline_key := PoolString} {$define null_value := nil}
	{$include hash.h.inc}

	{$define classname := RenderScenariosSet} {$define key_type := pRenderScenario} {$define inline_key := PoolString} {$define null_value := nil}
	{$include hash.h.inc}

	pMesh = ^Mesh;
	pBatch = ^Batch;

	pMeshIndices = ^MeshIndices;
	MeshIndices = object
	public const
		RestartIndex = High(uint);
	type
		TypeEnum = (Uint8Indices, Uint16Indices, Uint32Indices);
	private var
		_batch: pBatch;
		function _GetIndex(n: uint): uint;
		procedure _SetIndex(n: uint; newInd: uint);
		procedure _SetCount(newCount: sint);
		procedure _Convert(nt: GLType);
	public
		raw: NativeGLValue;
		procedure Initialize(newBatch: pBatch = nil);
		procedure Finalize;
		procedure Deserialize(s: pStream);
		procedure Rebuild;
		function TypeToGL(ty: TypeEnum): GLType; static;
		function TypeFromGL(ty: GLType): TypeEnum; static;

		property Indices[n: uint]: uint read _GetIndex write _SetIndex; default;
		property Count: sint read raw._count write _SetCount;
		property Typ: GLType read raw._type write _Convert;
	end;

	Batch = object
	private
		_mesh: pMesh;
		_meshId: sint;
		_verticesCount: sint;
		procedure _SetVerticesCount(newVerticesCount: sint);
		function _GetIndicesCount(level: sint): uint;
		procedure _SetIndicesCount(level: sint; newCount: uint);
		procedure EnsureConsistency;
		procedure FreeData;
	public
		name: PoolString;
		va: array of NativeGLValue;
		inds: MeshIndices;
		procedure Init(newMesh: pMesh; newMeshId: sint; const newName: PoolString);
		procedure Done;
	{$ifdef Debug} function Human: string; {$endif}

		function AddVA(const newName: PoolString; type_: GLType): sint;
		function GetVAId(const vaName: PoolString): sint;
		function FindVA(const vaName: PoolString): pNativeGLValue;
		procedure RemoveVA(id: sint);
		function RemoveVA(const vaName: PoolString): boolean;
		function RenameVA(id: sint; const newName: PoolString): boolean;

		function GetVertexDataSize: size_t;
		function GetVertexReserved: size_t;
		function GetDataSize: size_t;
		function IndexOffset(level: sint): size_t;
		procedure IndicesRangeRemoved(start, count: uint);

		property VerticesCount: sint read _verticesCount write _SetVerticesCount;
		property IndicesCount[level: sint]: uint read _GetIndicesCount write _SetIndicesCount;
		property Mesh: pMesh read _mesh;
	end;

	Mesh = object(&Object)
	type
		pLevelData = ^tLevelData;
		tLevelData = object
			minLod: float;
			lvInds: array of record
				count: uint;
			end;
			constructor Initialize(mesh: pMesh; const newMinLod: float);
			destructor Finalize;
		end;
	private
		_bnd: pBounding;
		function _GetBounding: Bounding;
		procedure _SetBounding(const newBounding: Bounding);
	public
		batches: array of Batch;
		levels: array of tLevelData;
	{$ifdef Debug} name: PoolString; {$endif}

		constructor Init({$ifdef Debug} const newName: string = '' {$endif});
		constructor Init(s: pStream);
		destructor Done; virtual;
		function GetDataSize: size_t;
		procedure FreeData;

		function GetBatchID(const batchName: PoolString): sint;
		function AddBatch(const batchName: string): pBatch;
		function FindBatch(const batchName: PoolString): pBatch;

		function HasLevels: boolean;
		function AddLevel(const aMinLod: float): sint;
		function GetLevelID(const lod: float): sint;
		function ChangeLevel(id: sint; const newLod: float): boolean;
		function RemoveLevel(id: sint): boolean;

		function HasBounding: boolean;
		procedure ClearBounding;
		property Bounding: Bounding read _GetBounding write _SetBounding;
		property BoundingPtr: pBounding read _bnd;

	var
		Loaders: LoaderSuite; static;
	type
		RawMesh = object
		const
			HAS_BND  = 1 shl 0;
			HAS_LODS = 1 shl 1;
			OldSignature = 'rwbinmesh';
			Signature = 'mesh';
			{$define max := ord(High(MeshIndices.TypeEnum))} {$define nbits := INDICES_TYPE_NBITS} {$define mask := INDICES_TYPE_MASK} {$include bits_to_store.inc}
		end;
	end;

	pShaderDefine = ^ShaderDefine;
	ShaderDefine = object
	public type
		tKind = (Flag, Special);
	private var
		_kind: tKind;
		_id: uint;
	public
		function Make(newKind: tKind; newId: uint): ShaderDefine; static;
		property Kind: tKind read _kind;
		property Id: uint read _id;
	end;

	pShaderDefinesManager = ^ShaderDefinesManager;
	ShaderDefinesManager = object
		procedure Init;
		procedure Done; {$define pSelf := pShaderDefinesManager} {$include dyn_obj.h.inc}
		procedure Add(const s: PoolString; kind: ShaderDefine.tKind; id: uint);
		procedure AddFlag(const s: PoolString; id: uint);
		procedure AddSpecial(const s: PoolString; id: uint);

		function Find(const s: PoolString): pShaderDefine;
		function FindFlag(const s: PoolString): sint;
		function FindSpecial(const s: PoolString): sint;
		
	private type
		{$define classname := Name2Define} {$define key_type := PoolString} {$define value_type := ShaderDefine}
		{$include hash.h.inc}
	var
		_h: Name2Define;
	end;

	pUserShaderFlags = ^UserShaderFlags;
	UserShaderFlags = object
	private type
		mask_t = type uint64;
	private var
		_bits: mask_t;
		_special: uint;
		function _GetFlag(id: sint): boolean;
		procedure _SetFlag(id: sint; newValue: boolean);
		function _GetFlagByName(const name: PoolString): boolean;
		procedure _SetFlagByName(const name: PoolString; newValue: boolean);
		function _GetSpecial(id: uint): boolean;
		procedure _SetSpecial(id: uint; newValue: boolean);
	public
		function Hash(const flags: UserShaderFlags): Hash.Value; static;
		function Equals(const a, b: UserShaderFlags): boolean; static;
		procedure Serialize(stream: pStream);
		function Deserialize(stream: pStream): UserShaderFlags; static;

		property Flag[id: sint]: boolean read _GetFlag write _SetFlag; default;
		property ByName[const name: PoolString]: boolean read _GetFlagByName write _SetFlagByName;
		property Special[id: uint]: boolean read _GetSpecial write _SetSpecial;
	public const
		Zero: UserShaderFlags = (_bits: 0; _special: 0);

		MAX_COUNT = bitsizeof(mask_t);
		MAX_PUBLIC_BUILTIN = 5;
		MAX_PRIVATE_BUILTIN = 5;

		MAX_USER = MAX_COUNT - MAX_PUBLIC_BUILTIN - MAX_PRIVATE_BUILTIN;
		USER_MASK = mask_t(1 shl MAX_USER - 1);
		HAS_SPECIAL_BIT = 1 shl MAX_USER;

		USER = 0;
		PUBLIC_BUILTIN = USER + MAX_USER;
		PRIVATE_BUILTIN = PUBLIC_BUILTIN + MAX_PUBLIC_BUILTIN;
	end;

	pShaderFlags = ^ShaderFlags;
	ShaderFlags = object
	private
		_nLights: array[tLightDetail] of uint;
		_insta: boolean;
		function _GetNLights(detail: tLightDetail): uint;
		procedure _SetNLights(detail: tLightDetail; newN: uint);
	public
		user: UserShaderFlags;
		function FromUser(const flags: UserShaderFlags): ShaderFlags; static;
		function Hash(const flags: ShaderFlags): Hash.Value; static;
		function Equals(const a, b: ShaderFlags): boolean; static;
		function Human: string;
		function Base32: string;
		procedure AppendMask(const def: ShaderDefine);
		procedure AppendMask(ld: tLightDetail);
		procedure AppendMask(lu: tLightUniform);
		function GetNLights(lu: tLightUniform): uint;
		function LocalizeLightUniform(global: uint; lu: tLightUniform): uint;
		function GlobalizeLightUniform(lu: tLightUniform; local: uint): uint;
		function GetLightDetail(global: uint): tLightDetail;

		property NLights[kind: tLightDetail]: uint read _GetNLights write _SetNLights;
		property Insta: boolean read _insta write _insta;
	public const
		Zero: ShaderFlags = (_nLights: (0, 0, 0, 0); _insta: no; user: (_bits: 0; _special: 0));
		LightAbbrevs: array[tLightDetail] of string = ('o', 'oS', 't', 'tS');
	end;

	operator +(const a, b: UserShaderFlags): UserShaderFlags;
	operator *(const a, b: UserShaderFlags): UserShaderFlags;
	operator +(const a, b: ShaderFlags): ShaderFlags;
	operator *(const a, b: ShaderFlags): ShaderFlags;

type
	pGLEntityParams = ^GLEntityParams;
	GLEntityParams = object
	public
		values: NativeGLValues;
		flags: UserShaderFlags;
		constructor Init;
		constructor Init(var cp: GLEntityParams);
		destructor Done;
		function Hash: Hash.Value;
		function Equals(var b: GLEntityParams): boolean;
		function Empty: boolean;
		procedure Merge(var cp: GLEntityParams);
	end;

	pSlideGL = ^SlideGL;
	SlideGL = object(EntityAction)
	private
		_dm: DimensionalMove;
		_gl: pGLEntityParams;
		_v: pNativeGLValue;
	protected
		procedure _Process(entity: pObject; const dt: float); virtual;
		function _Conflicts(var ac: EntityAction): boolean; virtual;
	public
		constructor Init(newGL: pGLEntityParams; const newName: string; newPath: pDimensionalPath);
		destructor Done; virtual;
	end;

	pShaderParser = ^ShaderParser;
	ShaderParser = object
	public type
		tTokenEnum =
		(
			tok_Unknown, tok_Raw, tok_Template,
			tok_Define, tok_IfDef, tok_Else, tok_EndIf,
			tok_NLights, tok_NBones, tok_NCsmSplits, tok_nInsta,
			tok_InstanceID,
			tok_Pi, tok_Percent,
			tok_ForLoopStart, tok_ForLoopEnd, tok_ForLoopVar, tok_ForLoopBracketedVar,
			tok_UboStart, tok_UboEnd,
			tok_TextureLookup,
			tok_UniqueIn, tok_UniqueOut, tok_In, tok_Out, tok_Flat, tok_NoPerspective, tok_FlatIn, tok_FlatOut,
			tok_FragColor
		);
		tForLoopKind = (for_OmniA, for_OmniS, for_TargA, for_TargS, for_Csm, for_Lights);

		DefineKind = (def_Name, def_Sf, def_Lide, def_Liu, def_LiuLoop);
		tDefine = object
			kind: DefineKind;
			name: PoolString;
			def: ShaderDefine;
			neg: boolean;
			function ByName(const newName: PoolString): tDefine; static;
			function ByDefine(const adef: ShaderDefine): tDefine; static;
			function ByLightDetail(aLide: tLightDetail): tDefine; static;
			function ByLightUniform(aLiu: tLightUniform; loopVar: pPoolString = nil): tDefine; static;
			function Make(const s: PoolString): tDefine; static;
			procedure Finalize;
		private const
			Empty: tDefine = (kind: def_Name; name: (internal: ''); def: (_kind: Flag; _id: 0); neg: no);
		end;
		tDefineList = array of tDefine;

		pForLoopRec = ^tForLoopRec;
		tForLoopRec = record
			kind: tForLoopKind;
			varName: PoolString;
			i, max, jmp, jend: sint;
			fair: boolean;
			start: sint;
		end;

		tContext = object
			fors: array of tForLoopRec;
			defs: array of PoolString;
			procedure Init(const newDefs: array of PoolString);
			procedure Done;
			function OpenForLoop(kind: tForLoopKind; const varName: PoolString; max, jmp, jend: sint): pForLoopRec;
			function FindForLoop(const varName: PoolString {$ifdef Debug}; allowFail: boolean = no {$endif}): pForLoopRec;
			function FindForLoopEnd(var sp: ShaderParser; start: sint): sint;
			function CloseForLoop: boolean;
			procedure Define(const def: tDefine; var params: ShaderFlags);
			function CheckDefines(var sp: ShaderParser; const pred: tDefineList; const params: ShaderFlags): boolean;
		end;

		pToken = ^tToken;
		tToken = record
			enum: tTokenEnum;
			str: string;
		case byte of
			0: (lu: tLightUniform);
			1: (id: sint);
			2: (tex: GLTextureTarget);
			3: (def: ^tDefine);
			4: (defs: ^tDefineList);
			5: (ps: pPoolString);
			6: (forKind: tForLoopKind; forVar: pPoolString);
			7: (forv: pPoolString; special: sint);
			8: (rawContainsSomething: boolean);
		end;
		tTokenList = array of tToken;

		tFlag = (flag_Ubo);
		tFlags = set of tFlag;
	public const
		LightDefines: array[tLightDetail] of string = ('OMNI_A', 'OMNI_S', 'TARG_A', 'TARG_S');
		LiuCounters: array[tLightUniform] of string = ('any', 'omni', 'omniS', 'targ', 'targS');
		LiuDefines: array[tLightUniform] of string = ('LIGHTS', 'OMNI', 'OMNI_S', 'TARG', 'TARG_S');
		UniformNLights: array[tLightDetail] of string = ('nOmni', '???', 'nTarg', '???');
	private var
		_gdefs: array of PoolString;
		function _Parse(const source: string; optimized: boolean): tTokenList;
		procedure _Append(var list: tTokenList; const tok: tToken);
		procedure _AppendRaw(var list: tTokenList; const str: string; optimized: boolean);
		function _Recognize(const t: string; var context: tContext): tToken;
		function _Optimize(const s: string): string;
		procedure _Finalize(var t: tToken);
		function _JmpNextBranch(start: sint): sint;
		procedure _BuildSource(const defs: array of PoolString; var s: StringBuilder; typ: ShaderType; chainId: sint; const aparams: ShaderFlags);
	public
		tok: tTokenList;
		mask: ShaderFlags;
		version: PoolString;
		flags: tFlags;
		constructor Init(const defs: array of PoolString; const source: string);
		destructor Done;
		procedure BuildSourceAndMask(const defs: array of PoolString; var s: StringBuilder; typ: ShaderType; chainId: sint; var params: ShaderFlags);
	end;

type
	ShaderSourceTemplates = object
	private
		_t: array of record
			name: PoolString;
			code: ShaderParser;
		end;
	public
		constructor Init;
		destructor Done;
		procedure Add(const name: PoolString; const code: string);
		procedure Load(s: pStream);
		function Find(const name: PoolString {$ifdef Debug}; warn: boolean = yes{$endif} ): pShaderParser;
	end;

	function FindRenderPass(const name: PoolString): pRenderPass;
	function FindRenderScenario(const name: PoolString): pRenderScenario;

type
	TextureImageFlag = (texture_Mips, texture_DontFilter, texture_ManualImgs, texture_Flipped);
	TextureImageFlags = set of TextureImageFlag;

	pTextureImageInfo = ^TextureImageInfo;
	TextureImageInfo = object
		target: GLTextureTarget;
		format: GLImageFormat;
		size: UintVec3;
		nLevels: uint;
		flags: TextureImageFlags;
		procedure Init(newTarget: GLTextureTarget; const newSize: UintVec3; newFormat: GLImageFormat; newFlags: TextureImageFlags);
		function GetLevelDimension(level, dim: uint): uint;
		function LevelSize(level: uint): UintVec3;
		function LevelSizeXY(level: uint): UintVec2;
		function GetPixelsCount(level: uint): uint;
		function GetLevelDataSize(level: uint): size_t;
		function DimsToStr(level: uint): string;
		function RowSize: size_t; cinline
		function PlaneSize(level: uint = 0): size_t; cinline
		function Defaced(level: uint): uint;
	end;

	ImageCombineMode = (img_Replace, img_Transparency);

const
	ImageCombineModeIds: array[ImageCombineMode] of string = ('replace', 'transparency');

type
	pTextureImage = ^TextureImage;
	TextureImage = object
		info: TextureImageInfo;
		FirstLevel: pointer;
		otherLevels: pPointer;
		procedure Init(const newSize: UintVec2; newFormat: GLImageFormat; newFlags: TextureImageFlags = []);
		procedure Init(const newSize: UintVec3; newFormat: GLImageFormat; newFlags: TextureImageFlags = []);
		procedure Init(newTarget: GLTextureTarget; const newSize: UintSize3; newFormat: GLImageFormat; newFlags: TextureImageFlags = []);
		procedure Init(s: pStream; const forceLoader: string = ''; size: size_t = 0);
		procedure Done;
	{$define texture_image_constructor_args := newTarget: GLTextureTarget; const newSize: UintSize3; newFormat: GLImageFormat; newFlags: TextureImageFlags = []}
	{$define texture_image_constructor_args2 := s: pStream; const forceLoader: string = ''; size: size_t = 0}
	{$define constructor_args := texture_image_constructor_args} {$define constructor_args2 := texture_image_constructor_args2}
	{$define pSelf := pTextureImage} {$include dyn_obj.h.inc}

		procedure Invalidate;
		function OK: boolean;
		procedure Prepare(newTarget: GLTextureTarget; const newSize: UintSize3; newFormat: GLImageFormat; newFlags: TextureImageFlags = []);
		procedure FreeData;
		procedure Save(const stream: string; const opts: string = '');
		procedure Save(const stream: string; const sizes: UintVec2; format: GLImageFormat; data: pointer; takeThisData: boolean = no; const opts: string = ''); static;
		procedure Save(const stream: string; level: uint);
		procedure CheckSaveTarget(supported: GLTextureTargets; const fmt: string);
		function ValidateCoord(x, y, z, level: uint): boolean;
		function ValidateCoord(x, y: uint): boolean;
		procedure ValidateCoordThrow(x, y, z, level: uint);
		procedure ValidateLevel(level: uint);
		function GetPixel(x, y, z, level: sint; out px: Vec4): boolean;
		function GetPixel(x, y, z, level: sint): Vec4;
		function GetPixel(ptr: pointer; out px: Vec4): boolean;
		function SetPixel(x, y, z, level: sint; const px: Vec4): boolean;
		procedure SetPixel(ptr: pointer; const px: Vec4);
		procedure CombinePixel(x, y, z, level: sint; const color: Vec4; mode: ImageCombineMode);
		procedure CombinePixel(ptr: pointer; const color: Vec4; mode: ImageCombineMode);
		procedure CombinePixel(ptr: pointer; const rgba: Vec4u8; mode: ImageCombineMode);
		procedure Blit(var from: TextureImage; fromX, fromY, fromZ: uint; fromLv: uint; sx, sy, sz: uint;
		               toX, toY, toZ: uint; toLv: uint; mode: ImageCombineMode);
		procedure RotateCwOZ;
		procedure Blit(var from: TextureImage; toX, toY, toZ: uint; toLv: uint; mode: ImageCombineMode);
		procedure Fill(const color: Vec4; level: sint = 0);
		function PixelSize: size_t; cinline
		function LevelPtr(level: uint): pointer;
		procedure ReplaceLevelWithOwnPointer(level: uint; data: pointer; silent: boolean = no);
		function PixelPtr(x, y, z, level: sint): pointer;
		function PixelPtr(x, y: sint): pointer;
		function PixelPtr(offset: uint): pointer;

		property Target: GLTextureTarget read info.target write info.target;
		property Format: GLImageFormat read info.format write info.format;
		property Size: UintVec3 read info.size;
		property nLevels: uint read info.nLevels;

	var
		Loaders: LoaderSuite; static;
	end;

const
	TriQuadInds: array[0 .. 5] of uint = (0, 1, 2, 0, 2, 3);

	function ShaderDefines: pShaderDefinesManager;

var
	MainPass, LightingPass, ShadowPass, TargShadowPass, MinimapPass: RenderPass;
	MainScenario, ShadowScenario, TargShadowScenario, MinimapScenario: RenderScenario;
	ShaderTemplates: ShaderSourceTemplates;

	Config: record
		maxLightsEstimation  : uint;
		lightEstimation      : array[tLightDetail] of uint;
		textureAnisotropy    : float;
		allowAdvFloats       : Tribool;
		allowUbyteIndices    : Tribool;
		nCsmSplits           : sint;
		csmLogK              : float;
		zNear, maxZFar       : float;
		abyssColor           : Vec4;
		allowMT              : boolean;
		forceMTFail          : boolean;
		allowInstancing      : boolean;
		allowPrimitiveRestart: boolean;
		allowUbo             : boolean;
		allowGeometryShader  : boolean;
		allowDSA             : boolean;
		allowBinaryShaders   : boolean;
		mergeLights          : boolean;
		vaAlignment          : sint;
		keepZeroUniFor       : boolean;
		keepOneInstance      : boolean;
		forceGLver           : string;
		forceSLver           : string;
	end =
	(
		maxLightsEstimation: 100;
		lightEstimation: (8, 16, 4, 40);
		textureAnisotropy: 4.0;
		allowAdvFloats: (value: 0);
		allowUbyteIndices: (value: 0);
		nCsmSplits: 3;
		csmLogK: 0.78;
		zNear: 0.1;
		maxZFar: 200.0;
		abyssColor: (data: (0.0, 0.0, 0.0, 0.0));
		allowMT              : yes;
		forceMTfail          : no;
		allowInstancing      : yes;
		allowPrimitiveRestart: yes;
		allowUbo             : yes;
		allowGeometryShader  : yes;
		allowDSA             : yes;
		allowBinaryShaders   : yes;
		mergeLights          : yes;
		vaAlignment          : 0;
		keepZeroUniFor       : yes;
		keepOneInstance      : yes;
		forceGLver           : '';
		forceSLver           : ''
	);

const
	LightDetailInfo: array[tLightDetail] of record
		u: tLightUniforms;
	end =
	(
		(u: [ulight_All, ulight_Omni]),                      // light_OmniA
		(u: [ulight_All, ulight_Omni, ulight_OmniS]),        // light_OmniS
		(u: [ulight_All, ulight_Targeted]),                  // light_TargetedA
		(u: [ulight_All, ulight_Targeted, ulight_TargetedS]) // light_TargetedS
	);

	LightUniformInfo: array[tLightUniform] of record
		start: tLightDetail;
		d: tLightDetails;
	end =
	(
		(start: light_OmniA; d: [light_OmniA, light_OmniS, light_TargetedA, light_TargetedS]), // ulight_All
		(start: light_OmniA; d: [light_OmniA, light_OmniS]),                                   // ulight_Omni
		(start: light_OmniS; d: [light_OmniS]),                                                // ulight_OmniS
		(start: light_TargetedA; d: [light_TargetedA, light_TargetedS]),                       // ulight_Targeted
		(start: light_TargetedS; d: [light_TargetedS])                                         // ulight_TargetedS
	);

implementation

uses
	Lights, GLUtils, MMSystem, DDS, PNG, Crunch, RawMesh, BMP, RawTex, Diffed3DTexture
{$ifdef use_serialization}, Serialization {$endif};

	{$define classname:=Name2NativeGLValue} {$define inline_hash := _1.Hash} {$define finalize_key :=}
	{$include hash.pp.inc}

	{$define classname := RenderPassesSet} {$define inline_hash := _1.Hash} {$define get_key := _1^.name} {$define finalize_key := _1^.Done}
	{$include hash.pp.inc}

	{$define classname := RenderScenariosSet} {$define inline_hash := _1.Hash} {$define get_key := _1^.name} {$define finalize_key := _1^.Done}
	{$include hash.pp.inc}

	{$define classname := ShaderDefinesManager.Name2Define} {$define inline_hash := _1.Hash} {$define finalize_key :=}
	{$include hash.pp.inc}

	function NativeGLValue._RawGet(i: sint): pointer;
	begin
		Assert(i < Count, ToString(i) + ' >= ' + ToString(Count));
		case _type of
			GLType.Sampler:
				begin
					Assert(i = 0);
					result := RawTex;
				end;
			else
				result := Ptr + GLTypeInfo[_type].sizeof * size_t(i);
		end;
	end;

	procedure NativeGLValue._RawSet(i: sint; p: pointer);
	var
		ofs, size: PtrUint;
	begin
		Assert(i < Count, ToString(i) + ' >= ' + ToString(Count));
		case _type of
			GLType.Sampler:
				begin
					Assert(i = 0);
					if data._obj <> p then
					begin
						SetRef(data._obj, p);
						_version := _NewVersion;
					end;
				end;
			else
			begin
				ofs := GLTypeInfo[_type].sizeof * size_t(i);
				size := GLTypeInfo[_type].sizeof;
				if CompareByte(p^, (Ptr + ofs)^, size) <> 0 then
				begin
					memcpy(p, Ptr + ofs, size);
					_version := _NewVersion;
				end;
			end;
		end;
	end;

	function NativeGLValue._GetDataSize: size_t;
	begin
		Assert(type_ <> GLType.Sampler);
		result := GLTypeInfo[_type].sizeof * size_t(Count);
	end;

	function NativeGLValue._GetReservedSize: size_t;
	begin
		Assert(type_ <> GLType.Sampler);
		result := GLTypeInfo[_type].sizeof * size_t(_allocated);
	end;

	function NativeGLValue._NewVersion: tVersion;
	begin
		result := _nextVersion;
		_nextVersion += 1;
	end;

	procedure NativeGLValue.Initialize(const newName: PoolString; newType: GLType; newCount: sint = 1; newFlags: NativeGLValueFlags = []);
{$ifdef Debug}
	{$push} {$writeableconst on} const FirstMaxInstances: uint = 0; {$pop}
{$endif}
	begin
		_version := 0;
		_namae := newName;
		_type := newType;
		_count := 0;
		_flags := newFlags;
		if NativeGLValueFlag.InstaPack in _flags then
		begin
			newCount := gl.MaxInstances;
		{$ifdef Debug}
			if FirstMaxInstances = 0 then FirstMaxInstances := newCount;
			Assert((newCount > 0) and (uint(newCount) = FirstMaxInstances), 'видимо, значение с glv_InstaPack создано до контекста OpenGL');
		{$endif}
		end;

		data._ptr := nil;
		_allocated := 0;
		Count := newCount;
	end;

	procedure NativeGLValue.Initialize(const newName: PoolString; newType: GLType; newFlags: NativeGLValueFlags);
	begin
		Initialize(newName, newType, 1, newFlags);
	end;

	procedure NativeGLValue.Initialize(const v2: NativeGLValue);
	begin
		Initialize(v2.Namae, v2.Type_, v2.Count, v2.flags);
		if Count <> 0 then
			if type_ <> GLType.Sampler then
				memcpy(v2.ptr, ptr, size_t(count) * GLTypeInfo[type_].sizeof)
			else
				SetTex(v2.RawTex);
	end;

	procedure NativeGLValue.Finalize;
	begin
		FreeData;
	end;

	procedure NativeGLValue.FreeData;
	begin
		case _type of
			GLType.Sampler: Release(data._obj);
			else
				FreeMem(data._ptr);
		end;
	end;

	procedure NativeGLValue.Pack;
	begin
		Assert(_type <> GLType.Sampler);
		if _allocated <> _count then
		begin
			_allocated := _count;
			ReallocMem(data._ptr, GLTypeInfo[type_].sizeof * size_t(_count));
		end;
	end;

	procedure NativeGLValue.CopyRange(from, to_, n: sint);
	var
		size: size_t;
	begin
		Assert(_type <> GLType.Sampler);
		size := GLTypeInfo[type_].sizeof;
		memcpy(data._ptr + size_t(from) * size, data._ptr + size_t(to_) * size, size_t(n) * size);
	end;

{$define impl:=
	var
		gx: gtype;
	begin
		case _type of
			gtypenum: rawprop[id] := x;
			else
			begin
				gx := x;
				Convert(@gx, gtypenum, data._ptr + size_t(id) * GLTypeInfo[type_].sizeof, type_);
				_version := _NewVersion;
			end;
		end;
	end; {$undef gtype} {$undef gtypenum} {$undef rawprop}}

	procedure NativeGLValue._SetFloat(id: sint; const x: float); {$define rawprop:=RawFloat} {$define gtype := GLfloat} {$define gtypenum := GLType.Float} impl
	procedure NativeGLValue._SetVec2(id: sint; const x: Vec2); {$define rawprop:=RawVec2} {$define gtype := Vec2f} {$define gtypenum := GLType.Vec2} impl
	procedure NativeGLValue._SetVec3(id: sint; const x: Vec3); {$define rawprop:=RawVec3} {$define gtype := Vec3f} {$define gtypenum := GLType.Vec3} impl
	procedure NativeGLValue._SetVec4(id: sint; const x: Vec4); {$define rawprop:=RawVec4} {$define gtype := Vec4f} {$define gtypenum := GLType.Vec4} impl
{$undef impl}

	procedure NativeGLValue.SetFloat(const x: float; id: sint = 0);
	var
		g: GLfloat;
		t: pGLfloat;
	begin
		Assert((_type = GLType.Float) and (id < _count));
		g := x;
		t := RawFloat + id;
		if (_version = 0) or (g <> t^) then
		begin
			t^ := g;
			_version := _NewVersion;
		end;
	end;

	procedure NativeGLValue.SetVec2(const v: Vec2; id: sint = 0);
	var
		g: Vec2f;
		t: pVec2f;
	begin
		Assert((_type = GLType.Vec2) and (id < _count));
		g := v;
		t := RawVec2 + id;
		if (_version = 0) or (g[0] <> t^[0]) or (g[1] <> t^[1]) then
		begin
			t^ := g;
			_version := _NewVersion;
		end;
	end;

	procedure NativeGLValue.SetVec3(const v: Vec3; id: sint = 0);
	var
		g: Vec3f;
		t: pVec3f;
	begin
		Assert((_type = GLType.Vec3) and (id < _count));
		g := v;
		t := RawVec3 + id;
		if (_version = 0) or (g[0] <> t^[0]) or (g[1] <> t^[1]) or (g[2] <> t^[2]) then
		begin
			t^ := g;
			_version := _NewVersion;
		end;
	end;

	procedure NativeGLValue.SetVec4(const v: Vec4; id: sint = 0);
	var
		g: Vec4f;
		t: pVec4f;
	begin
		Assert((_type = GLType.Vec4) and (id < _count));
		g := v;
		t := RawVec4 + id;
		if (_version = 0) or (g[0] <> t^[0]) or (g[1] <> t^[1]) or (g[2] <> t^[2]) or (g[3] <> t^[3]) then
		begin
			RawVec4[id] := g;
			_version := _NewVersion;
		end;
	end;

	procedure NativeGLValue.SetMat4(const m: Matrix4; id: sint = 0);
	var
		g: Mat4f;
		t: pMat4f;
	begin
		Assert((_type = GLType.Mat4) and (id < _count));
		g := m;
		t := RawMat4 + id;
		if (_version = 0) or (CompareByte(g, t^, sizeof(g)) <> 0) then
		begin
			t^ := g;
			_version := _NewVersion;
		end;
	end;

	procedure NativeGLValue.SetInt(x: sint; id: sint = 0);
	var
		chg: boolean;
	begin
		Assert((GLTypeFlag.Int in GLTypeInfo[_type].flags) and (id < _count));
		chg := _version = 0;
		case _type of
			GLType.Int32:
				begin
					chg := RawInt32[id] <> x;
					RawInt32[id] := x;
				end;
			GLType.Uint32:
				begin
					chg := RawUint[id] <> uint(x);
					RawUint[id] := x;
				end;
			GLType.Uint16:
				begin
					chg := RawUshort[id] <> x;
					RawUshort[id] := x;
				end;
			GLType.Ubyte:
				begin
					chg := RawUbyte[id] <> x;
					RawUbyte[id] := x;
				end;
			else Assert(no, GLTypeIds[_type]);
		end;
		if chg then _version := _NewVersion;
	end;

	procedure NativeGLValue.SetTex(newTex: pObject; id: sint = 0);
	begin
		Assert((_type = GLType.Sampler) and (id < _count));
		if (_version = 0) or (RawTex <> newTex) then
		begin
			SetRef(data._obj, newTex);
			_version := _NewVersion;
		end;
	end;

	function NativeGLValue.ToFloat(id: sint): float;
	var
		t: GLfloat;
	begin
		Convert(data._ptr + GLTypeInfo[type_].sizeof * size_t(id), type_, @t, GLType.Float);
		result := t;
	end;

	function NativeGLValue.ToVec2(id: sint): Vec2;
	var
		t: Vec2f;
	begin
		Convert(data._ptr + GLTypeInfo[type_].sizeof * size_t(id), type_, @t, GLType.Vec2);
		result := FromGL(t);
	end;

	function NativeGLValue.ToVec3(id: sint): Vec3;
	var
		t: Vec3f;
	begin
		Convert(data._ptr + GLTypeInfo[type_].sizeof * size_t(id), type_, @t, GLType.Vec3);
		result := FromGL(t);
	end;

	function NativeGLValue.ToVec4(id: sint): Vec4;
	var
		t: Vec4f;
	begin
		Convert(data._ptr + GLTypeInfo[type_].sizeof * size_t(id), type_, @t, GLType.Vec4);
		result := FromGL(t);
	end;

	procedure NativeGLValue.ConvertTo(t2: GLType);
	label _finally_;
	var
		newData: pointer;
	{$ifdef Debug} t: Ticks; {$endif}
	begin
		Assert((type_ <> GLType.Sampler) and (t2 <> GLType.Sampler));
		if type_ = t2 then exit;

	{$ifdef Debug}
		LogR('Конвертирование "' + namae + '": ' + GLTypeIds[type_] + ' -> ' + GLTypeIds[t2] + '... ' , logDebug);
		t := Ticks.Get;
	{$endif}
		newData := GetMem(GLTypeInfo[t2].sizeof * size_t(_allocated));
		Convert(data._ptr, type_, newData, t2, count);
		FreeMem(data._ptr);
		data._ptr := newData;
		_type := t2;

	_finally_:
	{$ifdef Debug} LogR('ОК, заняло ' + ToString((Ticks.Get - t).ToMicroseconds) + ' мкс; ', logDebug); {$endif}
	end;

	procedure NativeGLValue._SetCount(newCount: sint);
	var
		size, oneSize, oldSize: size_t;
		na: uint;
	begin
		if _count = newCount then exit;
		case _type of
			GLType.Sampler:
				case newCount of
					0: Release(data._obj);
					1: data._obj := nil;
				{$ifdef Debug} else Fatal('Sampler arrays don''t supported'); {$endif}
				end;
			else
			begin
				if (newCount > _allocated) or (newCount <= _allocated div 8) then
				begin
					oneSize := GLTypeInfo[type_].sizeof;
					if (_allocated = 0) then
					begin
						_allocated := newCount;
						size := uint(_allocated) * oneSize;
						data._ptr := GetMem(size);
						fillchar(data._ptr^, size, 0);
					end else
					begin
						na := 2 * newCount;
						size := na * oneSize;
						ReallocMem(data._ptr, size);
						if na > uint(_allocated) then
						begin
							oldSize := uint(_allocated) * oneSize;
							fillchar((data._ptr + oldSize)^, size - oldSize, 0);
						end;
						_allocated := na;
					end;
				end;
			end;
		end;
		_count := newCount;
		_version := _NewVersion;
	end;

	function NativeGLValue.Equals(const v2: NativeGLValue; maxCount: sint = -1): boolean;
	begin
		Assert(type_ = v2.type_);
		if (maxCount = -1) or (maxCount > count) then maxCount := count;
		if type_ <> GLType.Sampler then
			result := (CompareByte(Ptr^, v2.Ptr^, size_t(min(maxCount, v2.count)) * GLTypeInfo[type_].sizeof) = 0)
		else
			result := Ptr = v2.Ptr;
	end;

	function NativeGLValue.Hash: Hash.Value;
	begin
		result := Algo.Hash.OfUint(uint(count));
		if count > 0 then
			if type_ <> GLType.Sampler then
			begin
				if GLTypeInfo[type_].sizeof >= sizeof(uint) then
					result := result xor Algo.Hash.OfUint(pUint(ptr)^);
			end else
				result := result xor Algo.Hash.OfPointer(ptr);
	end;

	function NativeGLValue.VaFix: boolean;
	var
		nt: GLType;
	begin
		nt := VaFix(type_);
		result := nt <> type_;
		ConvertTo(nt);
	end;

	function NativeGLValue.VaFix(typ: GLType): GLType;
	begin
		result := typ;
		if (GLTypeFlag.AdvFloat in GLTypeInfo[typ].flags) and not gl.UseAdvFloats then
			result := GLVec[GLTypeInfo[typ].baseDim]
		else
			if not Config.allowAdvFloats.StrictYes then
				case typ of
					GLType.Vec3Half, GLType.Nui8: result := GLVec[GLTypeInfo[typ].baseDim];
					GLType.Vec2Ni8: result := GLType.Vec2Ni16;
					GLType.Vec2Nui8: result := GLType.Vec2Nui16;
				end;
	end;

	function NativeGLValue.IdFix(typ: GLType): GLType;
	begin
		result := typ;
		if (typ = GLType.Ubyte) and not gl.UseUbyteIds then result := GLType.Uint16;
	end;

	procedure NativeGLValues.Initialize;
	begin
		_hash.Init;
		_onAdd := nil;
		_onRelocate := nil;
		_userParam := nil;
	end;

	procedure NativeGLValues.Initialize(var cp: NativeGLValues);
	var
		i: sint;
		v: NativeGLValue;
	begin
		Initialize;
		for i := 0 to High(cp.raw) do
		begin
			v.Initialize(cp.raw[i]);
			AddRaw(v);
		end;
	end;

	procedure NativeGLValues.Finalize;
	var
		i: sint;
	begin
		_hash.Done;
		for i := 0 to High(raw) do
			raw[i].Finalize;
		raw := nil;
	end;

	procedure NativeGLValues.SetCallbacks(add: OnAddProc; relocate: OnRelocateProc; param: pObject);
	begin
		_onAdd := add;
		_onRelocate := relocate;
		_userParam := param;
	end;

	procedure NativeGLValues._ReallocRaw(newSize: sint);
	var
		oldSize: sint;
		oldPtr: pNativeGLValue;
		v: ^pNativeGLValue;
		i: sint;
		it: Name2NativeGLValue.Iterator;
	begin
		oldPtr := pointer(raw);
		oldSize := length(raw);
		SetLength(raw, newSize);
		if (oldPtr <> pointer(raw)) then
		begin
		{$ifdef DebugNativeGLValues} Log('NativeGLValues.Relocate: ' + ToString(oldSize) + ' -> ' + ToString(newSize)); {$endif}
			if Assigned(_onRelocate) then
			begin
				for i := 0 to min(oldSize, newSize) - 1 do
					_onRelocate(oldPtr + i, pNativeGLValue(raw) + i, _userParam);
			end;

			it := _hash.GetIterator;
			while _hash.Next(it) do
			begin
				v := _hash.GetValue(it);
				v^ := pNativeGLValue(raw) + (v^ - oldPtr);
			end;
		end;
	end;

	procedure NativeGLValues.AddRaw(var v: NativeGLValue);
	begin
		Assert(not Assigned(_hash.Find(v.namae)));
		_ReallocRaw(length(raw) + 1);
		raw[High(raw)] := v;
		_hash.Add(v.namae, @raw[High(raw)]);
		if Assigned(_onAdd) then
			_onAdd(@raw[High(raw)], _userParam);
	end;

	procedure NativeGLValues.Remove(const name: PoolString);
	var
		v: pNativeGLValue;
		i: sint;
	begin
	{$ifdef Debug} Log('Удаляю GL-параметр: "' + name + '"', logWarning); {$endif}
		v := _hash.Find(name);
		if not Assigned(v) then
		begin
		{$ifdef Debug} Log('GL-параметр "' + name + '" не найден', logError); {$endif}
			exit;
		end;

		_hash.Remove(name);
		i := v - @raw[0];
		v^.Finalize;
		if i <> High(raw) then
		begin
			raw[i] := raw[High(raw)];
			_hash.Add(raw[i].namae, @raw[i]);
			if Assigned(_onRelocate) then _onRelocate(@raw[High(raw)], @raw[i], _userParam);
		end;
		_ReallocRaw(length(raw) - 1);
	end;

	function NativeGLValues.Value(const name: PoolString; typ: GLType; count: sint = 1; flags: NativeGLValueFlags = []): pNativeGLValue;
	var
		v: NativeGLValue;
	begin
		result := Value(name);
		if Assigned(result) then
		begin
			Assert(result^.type_ = typ, 'GL type mismatch: ' + GLTypeIds[result^.type_] + ' <-> ' + GLTypeIds[typ]);
			if result^.count < count then result^.count := count;
		end else
		begin
			v.Initialize(name, typ, count, flags);
			AddRaw(v);
			result := Value(name);
		end;
	end;

	function NativeGLValues.Value(const name: PoolString): pNativeGLValue;
	begin
		result := _hash.Find(name);
	end;

	function NativeGLValues.GetID(const name: PoolString): sint;
	var
		v: pNativeGLValue;
	begin
		v := _hash.Find(name);
		if Assigned(v) then
			result := v - pNativeGLValue(raw)
		else
			result := -1;
	end;

	procedure NativeGLValues.ForceCleanup;
	var
		i: sint;
	begin
		for i := 0 to High(raw) do
			raw[i].Count := 0;
	end;

	function NativeGLValues.Hash: Hash.Value;
	var
		i: sint;
	begin
		result := Algo.Hash.OfUint(uint(length(raw)));
		for i := 0 to High(raw) do
			result := result xor raw[i].Hash;
	end;

	function NativeGLValues.Equals(var b: NativeGLValues): boolean;
	var
		i: sint;
		v, v2: pNativeGLValue;
	begin
		result := _hash.Count = b._hash.Count;
		if not result then exit;

		for i := 0 to High(raw) do
		begin
			v := @raw[i];
			v2 := b.Value(v^.Namae);
			if (not Assigned(v2)) or (v^.Type_ <> v2^.Type_) or (not v^.Equals(v2^)) then
				exit(no);
		end;
	end;

	function NativeGLValues._GetHash(v: pNativeGLValues): Hash.Value;
	begin
		result := v^.Hash;
	end;

	function NativeGLValues._Equals(a, b: pNativeGLValues): boolean;
	begin
		result := (a = b) or a^.Equals(b^);
	end;

var
	RenderPasses: RenderPassesSet;
	RenderScenarios: RenderScenariosSet;

	constructor RenderPass.Init(const newName: PoolString; newSpecial: RenderPassSpecial; static: boolean = no);
	begin
		inherited Init;
		self._static := static;
		name := newName;
		special := newSpecial;
		if self._static then MakeStatic;
		RenderPasses.Add(@self);
	end;

	destructor RenderPass.Done;
	begin
		if not _static then RenderPasses.Remove(name);
		inherited Done;
	end;

	function FindRenderPass(const name: PoolString): pRenderPass;
	begin
		result := RenderPasses.Find(name);
	end;

	function FindRenderScenario(const name: PoolString): pRenderScenario;
	begin
		result := RenderScenarios.Find(name);
	end;

	constructor RenderScenario.Init(const newName: PoolString; const newPasses: array of pRenderPass; newFlags: RenderScenarioFlags);
	var
		i: sint;
		n: sint;
	begin
		inherited Init;
		name := newName;
		SetLength(passes, length(newPasses));
		n := 0;
		for i := 0 to High(newPasses) do
			if Assigned(newPasses[i]) then
			begin
				passes[n] := MakeRef(newPasses[i]);
				inc(n);
			end;
		SetLength(passes, n);
		flags := newFlags;
		RenderScenarios.Add(@self);
		if scenario_Static in flags then MakeStatic;
	end;

	destructor RenderScenario.Done;
	begin
		if not (scenario_Static in flags) then RenderScenarios.Remove(name);
		ReleaseArray(USystem.ObjectsList(passes));
		inherited Done;
	end;

	function MeshIndices._GetIndex(n: uint): uint;
	begin
		case raw.type_ of
			GLType.Uint16: result := raw.RawUshort[n];
			GLType.Ubyte: result := raw.RawUbyte[n];
			GLType.Uint32: result := raw.RawUint[n];
		{$ifdef Debug} else Assert(no); {$endif}
		end;
	end;

	procedure MeshIndices._SetIndex(n: uint; newInd: uint);
	var
		nt: GLType;
	begin
		if newInd <> RestartIndex then
		begin
			if newInd + 1 > GLIntLims[raw.type_] then
			begin
				nt := GLType.Uint16;
				if newInd + 1 > GLIntLims[nt] then nt := GLType.Uint32;
				if newInd + 1 > GLIntLims[nt] then
				begin
				{$ifdef Debug} Log('OMG! Настолько большой индекс (' + ToString(newInd) + ') не влезает в GLuint32!', logError); {$endif}
					exit;
				end;
				_Convert(nt);
			end;
		end else
			newInd := GLIntLims[raw.Type_];
		case raw.type_ of
			GLType.Uint16: raw.RawUshort[n] := newInd;
			GLType.Ubyte: raw.RawUbyte[n] := newInd;
			GLType.Uint32: raw.RawUint[n] := newInd;
		end;
	end;

	procedure MeshIndices._SetCount(newCount: sint);
	begin
		if raw.Count <> newCount then
		begin
			raw.Count := newCount;
			if Assigned(_batch) then _batch^.EnsureConsistency;
		end;
	end;

	procedure MeshIndices._Convert(nt: GLType);
	var
		restart: uint;
		restarts: array of uint;
		i, nRestarts: sint;
	begin
		if nt = raw.type_ then exit;
		if gl.PrimitiveRestartSupported then
		begin
			restart := GLIntLims[raw.Type_];
			nRestarts := 0;
			SetLength(restarts, Count);
			for i := 0 to Count - 1 do
				if _GetIndex(i) = restart then
				begin
					restarts[nRestarts] := i;
					inc(nRestarts);
				end;
			SetLength(restarts, nRestarts);
		end;

		raw.ConvertTo(nt);
		if gl.PrimitiveRestartSupported then
		begin
			restart := GLIntLims[raw.Type_];
			case raw.type_ of
			{$define impl:=for i := 0 to nRestarts - 1 do raw.field[restarts[i]] := restart {$undef field}}
				GLType.Uint16: {$define field:=RawUshort} impl;
				GLType.Ubyte: {$define field:=RawUbyte} impl;
				GLType.Uint32: {$define field:=RawUint} impl;
			{$undef impl}
			end;
		end;
	end;

	procedure MeshIndices.Rebuild;
	var
		i: sint;
		nt: GLType;
	begin
		nt := GLType.Ubyte;
		for i := 0 to Count - 1 do
			if GLIntLims[nt] - 1 < self[i] then
				if GLIntLims[GLType.Uint16] - 1 >= self[i] then
					nt := GLType.Uint16
				else
					nt := GLType.Uint32;
		if nt <> raw.type_ then _Convert(nt);
	end;

	function MeshIndices.TypeToGL(ty: TypeEnum): GLType;
	const
		Conv: array[TypeEnum] of GLType = (GLType.Ubyte, GLType.Uint16, GLType.Uint32);
	begin
		result := Conv[ty];
	end;

	function MeshIndices.TypeFromGL(ty: GLType): TypeEnum;
	begin
		case ty of
			GLType.Ubyte: result := Uint8Indices;
			GLType.Uint16: result := Uint16Indices;
			GLType.Uint32: result := Uint32Indices;
			else raise Error('Неверный тип индексов: {0}.', GLTypeIds[ty]);
		end;
	end;

	procedure MeshIndices.Initialize(newBatch: pBatch = nil);
	begin
		_batch := newBatch;
		raw.Initialize('inds', GLType.Ubyte, 0);
	end;

	procedure MeshIndices.Finalize;
	begin
		raw.Finalize;
	end;

	procedure MeshIndices.Deserialize(s: pStream);
	type
		RawMesh = Mesh.RawMesh;
	var
		cntx: uint;
	begin
		cntx := VarInt.Read(s);
		raw.Finalize;
		raw.Initialize('inds', TypeToGL(TypeEnum(RangeCheck(cntx and RawMesh.INDICES_TYPE_MASK, ord(High(TypeEnum)), 'Indices.type'))), 0);
		Count := cntx shr RawMesh.INDICES_TYPE_NBITS;
		s^.Read(raw.Ptr, raw.DataSize);
	end;

	procedure Batch._SetVerticesCount(newVerticesCount: sint);
	var
		i: sint;
	begin
		if _verticesCount <> newVerticesCount then
		begin
			_verticesCount := newVerticesCount;
			for i := 0 to High(va) do
				va[i].Count := _verticesCount;
		end;
	end;

	function Batch.AddVA(const newName: PoolString; type_: GLType): sint;
	begin
		result := GetVAId(newName);
		if result >= 0 then
		begin
			if va[result].Type_ = type_ then
				exit
			else
				va[result].Finalize;
		end else
		begin
			result := length(va);
			SetLength(va, result + 1);
		end;
		va[result].Initialize(newName, type_, _verticesCount);
	end;

	function Batch.GetVAId(const vaName: PoolString): sint;
	begin
		result := Index(vaName.ToIndex, first_field va _ _namae _, length(va), sizeof(va[0]));
	end;

	function Batch.FindVA(const vaName: PoolString): pNativeGLValue;
	var
		id: sint;
	begin
		id := GetVAId(vaName);
		if id >= 0 then result := @va[id] else result := nil;
	end;

	procedure Batch.RemoveVA(id: sint);
	begin
		va[id].Finalize;
		va[id] := va[High(va)];
		SetLength(va, length(va) - 1);
	end;

	function Batch.RemoveVA(const vaName: PoolString): boolean;
	var
		i: sint;
	begin
		i := GetVAId(vaName);
		if i >= 0 then RemoveVA(i);
		result := i >= 0;
	end;

	function Batch.RenameVA(id: sint; const newName: PoolString): boolean;
	begin
		va[id]._namae := newName;
		result := yes;
	end;

	procedure Batch.Init(newMesh: pMesh; newMeshId: sint; const newName: PoolString);
	begin
		_mesh := newMesh;
		_meshId := newMeshId;
		name := newName;
		VerticesCount := 0;
		va := nil;
		inds.Initialize(@self);
	end;

	procedure Batch.Done;
	var
		i: sint;
	begin
		inds.Finalize;
		for i := 0 to High(va) do
			va[i].Finalize;
	end;

{$ifdef Debug}
	function Batch.Human: string;
	begin
		result := '"' + IfThen(not _mesh^.name.Empty, _mesh^.name, '<?>') + '.' + IfThen(not name.Empty, name, '<?>') + '"';
	end;
{$endif}

	procedure Batch.EnsureConsistency;
	begin
		if length(_mesh^.levels) = 1 then
			_mesh^.levels[0].lvInds[_meshId].count := inds.Count;
	end;

	function Batch.GetVertexDataSize: size_t;
	var
		i: sint;
	begin
		result := 0;
		for i := 0 to High(va) do
			result += va[i].DataSize;
	end;

	function Batch.GetVertexReserved: size_t;
	var
		i: sint;
	begin
		result := 0;
		for i := 0 to High(va) do
			result += va[i].ReservedSize;
	end;

	function Batch.GetDataSize: size_t;
	begin
		result := GetVertexDataSize + inds.raw.DataSize;
	end;

	function Batch.IndexOffset(level: sint): size_t;
	var
		i: sint;
	begin
		result := 0;
		for i := 0 to level - 1 do
			result += _mesh^.levels[i].lvInds[_meshId].count;
	end;

	procedure Batch.IndicesRangeRemoved(start, count: uint);
	var
		lv: sint;
	begin
		lv := 0;
		while (lv < High(_mesh^.levels)) and (start >= IndicesCount[lv]) do
		begin
			start -= IndicesCount[lv];
			inc(lv);
		end;
		while (lv < length(_mesh^.levels)) and (count > 0) do
		begin
			if IndicesCount[lv] > count then
			begin
				IndicesCount[lv] := IndicesCount[lv] - count;
				count := 0;
			end else
			begin
				count -= IndicesCount[lv];
				IndicesCount[lv] := 0;
			end;
			inc(lv);
		end;
	end;

	function Batch._GetIndicesCount(level: sint): uint;
	begin
		result := _mesh^.levels[level].lvInds[_meshId].count;
	end;

	procedure Batch._SetIndicesCount(level: sint; newCount: uint);
	begin
		_mesh^.levels[level].lvInds[_meshId].count := newCount;
	end;

	procedure Batch.FreeData;
	var
		i: sint;
	begin
		for i := 0 to High(va) do
			va[i].FreeData;
		inds.raw.FreeData;
	end;

	constructor Mesh.tLevelData.Initialize(mesh: pMesh; const newMinLod: float);
	var
		i: sint;
	begin
		minLod := newMinLod;
		SetLength(lvInds, length(mesh^.batches));
		for i := 0 to High(lvInds) do
			lvInds[i].count := 0;
	end;

	destructor Mesh.tLevelData.Finalize;
	begin
		lvInds := nil;
	end;

	function Mesh.AddBatch(const batchName: string): pBatch;
	var
		i: sint;
		optr: pointer;
	begin
		optr := pointer(batches);
		SetLength(batches, length(batches) + 1);
		if Assigned(optr) and (optr <> pointer(batches)) then
			for i := 0 to High(batches) - 1 do
				batches[i].inds._batch := @batches[i];

		result := @batches[High(batches)];
		result^.Init(@self, High(batches), batchName);
		for i := 0 to High(levels) do
		begin
			SetLength(levels[i].lvInds, length(levels[i].lvInds) + 1);
			Assert(High(levels[i].lvInds) = High(batches));
			with levels[i].lvInds[High(levels[i].lvInds)] do
				count := 0;
		end;
	end;

	constructor Mesh.Init({$ifdef Debug} const newName: string = '' {$endif});
	begin
		inherited Init;
		_bnd := nil;
		levels := nil;
		AddLevel(0.0);
	{$ifdef Debug} name := newName; {$endif}
	end;

	constructor Mesh.Init(s: pStream);
{$ifdef Debug} var bbDesc: string; {$endif}
	begin
		Init({$ifdef Debug}StreamPath.FilenameNoExt(s^.path){$endif});
		s^.NewRef;
		try
		{$ifdef Debug} LogR('Загрузка модели из ' + StreamPath.Log(s^.path) + '... '); {$endif}
			Loaders.Load(@self, s);
		{$ifdef Debug}
			if Assigned(_bnd) then bbDesc := ', ' + ToString(_bnd^) else bbDesc := '';
			Log('Модель ' + StreamPath.Log(s^.path) + ' загружена, ' + lang_amount(length(levels), '{N} уров{ень/ня/ней}') +
				', ' + lang_amount(length(batches), '{N} батч{/а/ей}') + ', размер данных: ' + ToStringSuff_b(GetDataSize) + bbDesc + '; ', logOK)
		{$endif}
		finally
			Release(s);
		end;
	end;

	destructor Mesh.Done;
	var
		i: sint;
	begin
		ClearBounding;
		for i := 0 to High(levels) do
			levels[i].Finalize;
		for i := 0 to High(batches) do
			batches[i].Done;
		inherited Done;
	end;

	function Mesh.GetDataSize: size_t;
	var
		i: sint;
	begin
		result := 0;
		for i := 0 to High(batches) do
			result += batches[i].GetDataSize;
	end;

	procedure Mesh.FreeData;
	var
		i: sint;
	begin
		for i := 0 to High(batches) do
			batches[i].FreeData;
	end;

	function Mesh.HasLevels: boolean;
	begin
		result := (length(levels) > 1) or (levels[0].minLod <> 0.0);
	end;

	function Mesh.AddLevel(const aMinLod: float): sint;
	begin
		SetLength(levels, length(levels) + 1);
		levels[High(levels)].Initialize(@self, aMinLod);
		result := High(levels);
		Assert(GetLevelID(aMinLod) = result);
	end;

	function Mesh.GetLevelID(const lod: float): sint;
	var
		i: sint;
	begin
		for i := 0 to High(levels) do
			if lod >= levels[i].minLod then
				exit(i);
		result := -1;
	end;

	function Mesh.ChangeLevel(id: sint; const newLod: float): boolean;
	begin
		result := (id >= 0) and (id < length(levels)) and (newLod >= 0.0) and (newLod <= 1.0) and not Equals(levels[id].minLod, newLod);
		if result then
			levels[id].minLod := newLod;
	end;

	function Mesh.RemoveLevel(id: sint): boolean;
	var
		bid: sint;
		binds: pMeshIndices;
		i, start, n: sint;
	begin
		result := (id >= 0) and (id < length(levels)) and (length(levels) > 1);
		if result then
		begin
			for bid := 0 to High(batches) do
			begin
				n := batches[bid].IndicesCount[id];
				if n = 0 then continue;
				start := batches[bid].IndexOffset(id);
				binds := @batches[bid].inds;
				for i := start to binds^.Count - 1 - n do
					binds^[i] := binds^[i + n];
				binds^.Count := binds^.Count - n;
			end;

			levels[id].Finalize;
			for i := id to High(levels) - 1 do
				levels[i] := levels[i + 1];
			SetLength(levels, length(levels) - 1);
		end;
	end;

	function Mesh.GetBatchID(const batchName: PoolString): sint;
	begin
		result := Index(batchName.ToIndex, first_field batches _ name _, length(batches), sizeof(batches[0]));
	end;

	function Mesh.FindBatch(const batchName: PoolString): pBatch;
	var
		id: sint;
	begin
		id := GetBatchID(batchName);
		if id >= 0 then result := @batches[id] else result := nil;
	end;

	function Mesh._GetBounding: Bounding;
	begin
		Assert(Assigned(_bnd));
		result := _bnd^;
	end;

	procedure Mesh._SetBounding(const newBounding: Bounding);
	begin
		if not Assigned(_bnd) then new(_bnd);
		_bnd^ := newBounding;
	end;

	function Mesh.HasBounding: boolean;
	begin
		result := Assigned(_bnd);
	end;

	procedure Mesh.ClearBounding;
	begin
		if Assigned(_bnd) then
		begin
			dispose(_bnd);
			_bnd := nil;
		end;
	end;

	function ShaderDefine.Make(newKind: tKind; newId: uint): ShaderDefine;
	begin
		result._kind := newKind;
		result._id   := newId;
	end;

	procedure ShaderDefinesManager.Init;
	begin
		_h.Init;
	end;

	procedure ShaderDefinesManager.Done;
	begin
		_h.Done;
	end; {$define classname := ShaderDefinesManager} {$define pSelf := pShaderDefinesManager} {$include dyn_obj.pp.inc}

	procedure ShaderDefinesManager.Add(const s: PoolString; kind: ShaderDefine.tKind; id: uint);
	begin
	{$ifdef Debug}
		if Assigned(_h.Find(s)) then Log('Шейдерный дефайн "' + s + '" уже существует.', logWarning);
		case kind of
			Flag:
				if id >= UserShaderFlags.MAX_COUNT then
				begin
					Log('ID шейдерного флага "' + s + '" (' + ToString(id) + ') вне границ.', logError);
					exit;
				end;
			Special:
				if id = 0 then
				begin
					Log('0 — неподходящий индекс СПЕШОЛА.', logError);
					exit;
				end;
		end;
	{$endif}

		_h.Add(s, ShaderDefine.Make(kind, id));
	end;

	procedure ShaderDefinesManager.AddFlag(const s: PoolString; id: uint);
	begin
		Add(s, Flag, id);
	end;

	procedure ShaderDefinesManager.AddSpecial(const s: PoolString; id: uint);
	begin
		Add(s, Special, id);
	end;

	function ShaderDefinesManager.Find(const s: PoolString): pShaderDefine;
	begin
		result := _h.Find(s);
	end;

	function ShaderDefinesManager.FindFlag(const s: PoolString): sint;
	var
		def: pShaderDefine;
	begin
		def := Find(s);
		if Assigned(def) and (def^.kind = Flag) then result := def^.id else result := -1;
	end;

	function ShaderDefinesManager.FindSpecial(const s: PoolString): sint;
	var
		def: pShaderDefine;
	begin
		def := Find(s);
		if Assigned(def) and (def^.kind = Special) then result := def^.id else result := -1;
	end;

	function UserShaderFlags._GetFlag(id: sint): boolean;
	begin
		if (id < 0) or (id >= MAX_COUNT) then exit(no);
		result := (_bits and (mask_t(1) shl id)) <> 0;
	end;

	procedure UserShaderFlags._SetFlag(id: sint; newValue: boolean);
	var
		combine: mask_t;
	begin
		if (id < 0) or (id >= MAX_COUNT) then exit;
		combine := mask_t(1) shl id;
		if newValue then
			_bits := _bits or combine
		else
			_bits := _bits and (not combine);
	end;

	function UserShaderFlags._GetFlagByName(const name: PoolString): boolean;
	begin
		result := _GetFlag(ShaderDefines^.FindFlag(name));
	end;

	procedure UserShaderFlags._SetFlagByName(const name: PoolString; newValue: boolean);
	begin
		_SetFlag(ShaderDefines^.FindFlag(name), newValue);
	end;

	function UserShaderFlags._GetSpecial(id: uint): boolean;
	begin
		result := _special = id;
	end;

	procedure UserShaderFlags._SetSpecial(id: uint; newValue: boolean);
	begin
		if newValue then
			_special := id
		else
			if _special = id then _special := 0;
	end;

	function UserShaderFlags.Hash(const flags: UserShaderFlags): Hash.Value;
	begin
		result := Algo.Hash.OfUint(flags._bits);
		if flags._special <> 0 then result := result xor Algo.Hash.OfUint(flags._special);
	end;

	function UserShaderFlags.Equals(const a, b: UserShaderFlags): boolean;
	begin
		result := (a._bits = b._bits) and (a._special = b._special);
	end;

	procedure UserShaderFlags.Serialize(stream: pStream);
	var
		x: uint64;
	begin
		x := _bits and USER_MASK;
		if _special <> 0 then x := x or HAS_SPECIAL_BIT;
		Serialize_ui64(stream, x);
		if _special <> 0 then VarInt.Write(stream, _special);
	end;

	function UserShaderFlags.Deserialize(stream: pStream): UserShaderFlags;
	var
		x: uint64;
	begin
		x := Deserialize_ui64(stream);
		if (x and HAS_SPECIAL_BIT) <> 0 then
		begin
			x := x xor HAS_SPECIAL_BIT;
			result._special := VarInt.Read(stream);
		end else
			result._special := 0;
		result._bits := x;

		Assert(result._bits = (result._bits and USER_MASK));
	end;

	function ShaderFlags.FromUser(const flags: UserShaderFlags): ShaderFlags;
	begin
		result := Zero;
		result.user := flags;
	end;

	function ShaderFlags.Hash(const flags: ShaderFlags): Hash.Value;
	begin
		result := UserShaderFlags.Hash(flags.user) xor Algo.Hash.OfUint(
			(flags._nLights[light_OmniA] shl 24) or
			(flags._nLights[light_OmniS] shl 16) or
			(flags._nLights[light_TargetedA] shl 8) or
			(flags._nLights[light_TargetedS] shl 0));
		if flags._insta then result := result xor 1;
	end;

	function ShaderFlags.Equals(const a, b: ShaderFlags): boolean;
	begin
		result := UserShaderFlags.Equals(a.user, b.user) and
			(a._nLights[light_OmniA] = b._nLights[light_OmniA]) and
			(a._nLights[light_OmniS] = b._nLights[light_OmniS]) and
			(a._nLights[light_TargetedA] = b._nLights[light_TargetedA]) and
			(a._nLights[light_TargetedS] = b._nLights[light_TargetedS]) and
			(a._insta = b._insta);
	end;

	function ShaderFlags._GetNLights(detail: tLightDetail): uint;
	begin
		result := _nLights[detail];
	end;

	procedure ShaderFlags._SetNLights(detail: tLightDetail; newN: uint);
	begin
		if CanUseUniFor(detail) and ((newN > 0) or GLBase.Config.keepZeroUniFor) then newN := MAX_LIGHTS[detail];
		_nLights[detail] := newN;
	end;

	function ShaderFlags.Human: string;
	var
		i: sint;
		n: uint;
		ld: tLightDetail;
	begin
		result := '';
		for i := 0 to UserShaderFlags.MAX_COUNT-1 do
			if user[i] then
			begin
				if result <> '' then result := result + ';';
				result += ToString(i);
			end;
		for ld in tLightDetail do
		begin
			n := _nLights[ld];
			if n > 0 then
			begin
				if result <> '' then result += '-';
				result += '[' + LightAbbrevs[ld] + ']';
				if CanUseUniFor(ld) then result += 'F';
				if (not n) <> 0 then result += ToString(n);
			end;
		end;
		if user._special <> 0 then
		begin
			if result <> '' then result += '-';
			result += 'Sp' + ToString(user._special);
		end;
		if _insta then
		begin
			if result <> '' then result += '-';
			result += 'I';
		end;
		if result = '' then result := 'Ø';
	end;

	function ShaderFlags.Base32: string;
	var
		a, b: string;
		ld: tLightDetail;
		i: sint;
	begin
		result := '';
		a := Algo.Base32(@user._bits, sizeof(user._bits));
		b := '';
		for i := 0 to UserShaderFlags.MAX_COUNT-1 do
			if user[i] then
			begin
				if b <> '' then b += '-';
				b += ToString(i);
				if length(b) > length(a) then break;
			end;

		if length(b) <= length(a) then result += b else result += a;
		for ld in tLightDetail do
			if _nLights[ld] > 0 then
			begin
				if result <> '' then result += '-';
				result += LightAbbrevs[ld];
				if CanUseUniFor(ld) then result += 'F';
				if (not _nLights[ld]) <> 0 then result += ToString(_nLights[ld]);
			end;
		if user._special <> 0 then
		begin
			if result <> '' then result += '-';
			result += 'Sp' + ToString(user._special);
		end;
		if _insta then
		begin
			if result <> '' then result += '-';
			result += 'I';
		end;
		if result = '' then result := 'Ø';
	end;

	procedure ShaderFlags.AppendMask(const def: ShaderDefine);
	begin
		case def.kind of
			Flag: user[def.id] := yes;
			Special: user._special := High(user._special);
			else Assert(no);
		end;
	end;

	procedure ShaderFlags.AppendMask(lu: tLightUniform);
	var
		ld: tLightDetail;
	begin
		for ld in tLightDetail do
			if ld in LightUniformInfo[lu].d then
				NLights[ld] := not uint(0);
	end;

	procedure ShaderFlags.AppendMask(ld: tLightDetail);
	begin
		NLights[ld] := not uint(0);
	end;

	function ShaderFlags.GetNLights(lu: tLightUniform): uint;
	var
		ld: tLightDetail;
	begin
		result := 0;
		for ld in tLightDetail do
			if ld in LightUniformInfo[lu].d then
				result += NLights[ld];
	end;

	function ShaderFlags.LocalizeLightUniform(global: uint; lu: tLightUniform): uint;
	var
		ld: tLightDetail;
		n: uint;
	begin
		result := 0;
		for ld in tLightDetail do
		begin
			n := NLights[ld];
			if lu in LightDetailInfo[ld].u then
			begin
				if global < n then
				begin
					result += global;
					exit;
				end;
				result += n;
			end;
			Assert(global >= n, 'Неверное использование счётчика "' + ShaderParser.LiuCounters[lu] + '"');
			dec(global, n);
		end;
		Assert(no, 'Неверное использование счётчика "' + ShaderParser.LiuCounters[lu] + '"');
	end;

	function ShaderFlags.GlobalizeLightUniform(lu: tLightUniform; local: uint): uint;
	var
		ld: tLightDetail;
		n: uint;
	begin
		result := 0;
		for ld in tLightDetail do
		begin
			n := NLights[ld];
			if lu in LightDetailInfo[ld].u then
			begin
				if local < n then
				begin
					result += local;
					exit;
				end;
				local -= n;
			end;
			result += n;
		end;
		Assert(no);
	end;

	function ShaderFlags.GetLightDetail(global: uint): tLightDetail;
	var
		ld: tLightDetail;
		n: uint;
	begin
		for ld in tLightDetail do
		begin
			n := NLights[ld];
			if global < n then exit(ld);
			dec(global, n);
		end;
		Assert(no, 'завышен индекс источника');
	end;

	operator +(const a, b: UserShaderFlags): UserShaderFlags;
	begin
		result._bits := a._bits or b._bits;
		result._special := a._special or b._special;
		Assert((result._special = a._special) or (result._special = b._special));
	end;

	operator *(const a, b: UserShaderFlags): UserShaderFlags;
	begin
		result._bits := a._bits and b._bits;
		result._special := a._special and b._special;
		Assert((a._special = 0) or ((not a._special) = 0) or (b._special = 0) or ((not b._special) = 0));
	end;

	operator +(const a, b: ShaderFlags): ShaderFlags;
	var
		ld: tLightDetail;
	begin
		result.user := a.user + b.user;
		for ld in tLightDetail do
			result._nLights[ld] := a._nLights[ld] or b._nLights[ld];
		result._insta := a._insta or b._insta;
	end;

	operator *(const a, b: ShaderFlags): ShaderFlags;
	var
		ld: tLightDetail;
	begin
		result.user := a.user * b.user;
		for ld in tLightDetail do
			result._nLights[ld] := a._nLights[ld] and b._nLights[ld];
		result._insta := a._insta and b._insta;
	end;

	constructor GLEntityParams.Init;
	begin
		values.Initialize;
		flags := UserShaderFlags.Zero;
	end;

	constructor GLEntityParams.Init(var cp: GLEntityParams);
	begin
		values.Initialize(cp.values);
		flags := cp.flags;
	end;

	destructor GLEntityParams.Done;
	begin
		values.Finalize;
	end;

	function GLEntityParams.Hash: Hash.Value;
	begin
		result := UserShaderFlags.Hash(flags) xor values.Hash;
	end;

	function GLEntityParams.Equals(var b: GLEntityParams): boolean;
	begin
		result := UserShaderFlags.Equals(flags, b.flags) and values.Equals(b.values);
	end;

	function GLEntityParams.Empty: boolean;
	var
		i: sint;
	begin
		result := UserShaderFlags.Equals(flags, UserShaderFlags.Zero);
		if result then
		begin
			result := values._hash.Count = 0;
			if not result then
			begin
				result := yes;
				for i := 0 to High(values.raw) do
					if not (NativeGLValueFlag.NonSerializable in values.raw[i].flags) then
					begin
						result := no;
						break;
					end;
			end;
		end;
	end;

	procedure GLEntityParams.Merge(var cp: GLEntityParams);
	var
		i: sint;
		v: NativeGLValue;
	begin
		flags := flags + cp.flags;
		for i := 0 to High(cp.values.raw) do
		begin
			v.Initialize(cp.values.raw[i]);
			values.AddRaw(v);
		end;
	end;

	constructor SlideGL.Init(newGL: pGLEntityParams; const newName: string; newPath: pDimensionalPath);
	begin
		if not Assigned(newPath) then Fail;
		inherited Init;
		_dm.Init(newPath);
		_gl := newGL;
		_v := _gl^.values.Value(newName);
		if not Assigned(_v) then
		begin
		{$ifdef Debug} Log('SlideGL: параметр "' + newName + '" не существует... а должен', logError); {$endif}
			ConstructorFailed;
		end;
		Assert(GLTypeInfo[_v^.Type_].baseDim = newPath^.Dimensions, 'Dimension mismatch: expected ' + ToString(GLTypeInfo[_v^.Type_].baseDim) + ', got ' + ToString(newPath^.Dimensions));
	end;

	destructor SlideGL.Done;
	begin
		_dm.Done;
		inherited Done;
	end;

	procedure SlideGL._Process(entity: pObject; const dt: float);
	begin
		Assert(@entity = @entity);
		_dm.Process(dt);
		case _v^.Type_ of
			GLType.Float: _v^.SetFloat(_dm.CurrentF);
			GLType.Vec2: _v^.SetVec2(_dm.CurrentV2);
			GLType.Vec3: _v^.SetVec3(_dm.CurrentV3);
			GLType.Vec4: _v^.SetVec4(_dm.CurrentV4);
			else Assert(no);
		end;
		if _dm.Finished then Stop(reason_Done);
	end;

	function SlideGL._Conflicts(var ac: EntityAction): boolean;
	begin
		result := inherited _Conflicts(ac) or ((TypeOf(ac) = TypeOf(SlideGL)) and (pSlideGL(@ac)^._v = _v));
	end;

	constructor ShaderParser.Init(const defs: array of PoolString; const source: string);
	var
		i: sint;
		src: string;
	begin
		mask := ShaderFlags.Zero;
		version := '';
		flags := [];
		SetLength(_gdefs, length(defs));
		for i := 0 to High(defs) do
			_gdefs[i] := defs[i];

		src := source;
	{$ifNdef Debug} src := _Optimize(src); {$endif}
		tok := _Parse(src, {$ifdef Debug} no {$else} yes {$endif});
	end;

	destructor ShaderParser.Done;
	var
		i: sint;
	begin
		for i := 0 to High(tok) do _Finalize(tok[i]);
	end;

	function ShaderParser._Parse(const source: string; optimized: boolean): tTokenList;
		function PosWoComments(ch: char; const str: string; ofs: sint): sint;
		var
			i: sint;
			eo: size_t;
		begin
			i := ofs;
			while i <= length(str) do
			begin
				if (str[i] = '/') and (i < length(str)) and (str[i + 1] = '/') then
				begin
					i += 2;
					while not UTF8.IsEOL(@str[i], length(str) - i + 1, eo) do inc(i);
					i += sint(eo);
					continue;
				end;

				if (str[i] = '/') and (i < length(str)) and (str[i + 1] = '*') then
				begin
					eo := Pos('*/', str, i + 2);
					if eo > 0 then
					begin
						i := eo + 2;
						continue;
					end;
				end;

				if str[i] = ch then exit(i);
				inc(i);
			end;
			result := 0;
		end;
	var
		next, L, R: sint;
		id: string;
		ctx: tContext;
	begin
		ctx.Init([]);
		result := nil;
		next := 1;
		repeat
			if optimized then L := Pos('%', source, next) else L := PosWoComments('%', source, next);
			if L = 0 then
			begin
				if next <= length(source) then _AppendRaw(result, Copy(source, next, length(source) - next + 1), optimized);
				break;
			end;
			if L > next then _AppendRaw(result, Copy(source, next, L - next), optimized);
			R := Pos('%', source, L + 1);
			if R = 0 then break;
			id := Copy(source, L + 1, R - L - 1);
			next := R + 1;
			_Append(result, _Recognize(id, ctx));
		until no;
		ctx.Done;
	end;

	procedure ShaderParser._Append(var list: tTokenList; const tok: tToken);
	begin
		if tok.enum = tok_Unknown then exit;
		SetLength(list, length(list) + 1);
		list[High(list)] := tok;
	end;

	procedure ShaderParser._AppendRaw(var list: tTokenList; const str: string; optimized: boolean);
	begin
		if length(str) = 0 then exit;
		SetLength(list, length(list) + 1);
		list[High(list)].enum := tok_Raw;
		list[High(list)].str := str;
		list[High(list)].rawContainsSomething := optimized or (length(_Optimize(str)) > 0);
	end;

	function ShaderParser._Recognize(const t: string; var context: tContext): tToken;
	const
		VersionSample = 'ver:';
		UboSample = 'Ubo:';
		ForLoopSample = 'For:';
		InSample = 'in.';
		OutSample = 'out.';
		ForLoopKindIds: array[tForLoopKind] of string = ('OmniA', 'OmniS', 'TargA', 'TargS', 'Csm', 'Lights');
		NuLightsTokens: array[tLightUniform] of string = ('[nLights]', '[nOmni]', '[nOmniS]', '[nTarg]', '[nTargS]');
		TextureLookups: array[GLTextureTarget] of string = ('tex1d', 'tex2d', 'tex3d', 'texCube');
		DefineSyms: charset_t = ['A'..'Z', 'a'..'z', '0'..'9', '_', '~', '*'];
		Known: array[0 .. 14] of record
			t: string;
			enum: tTokenEnum;
		end =
		(
			(t: '[nInsta]'; enum: tok_NInsta),  (t: '[IID]'; enum: tok_InstanceID),
			(t: '[nBones]'; enum: tok_NBones),
			(t: 'pi'; enum: tok_Pi),
			(t: 'percent'; enum: tok_Percent),
			(t: 'nCsmSplits'; enum: tok_NCsmSplits),
			(t: '/Ubo'; enum: tok_UboEnd),
			(t: '/For'; enum: tok_ForLoopEnd),
			(t: 'in'; enum: tok_In),
			(t: 'out'; enum: tok_Out),
			(t: 'flat'; enum: tok_Flat),
			(t: 'flatin'; enum: tok_FlatIn),
			(t: 'flatout'; enum: tok_FlatOut),
			(t: 'noperspective'; enum: tok_NoPerspective),
			(t: FragColorVariable; enum: tok_FragColor)
		);
	var
		id, i, j: sint;
		s, ns, ts: string;
		tex: GLTextureTarget;
		neg, bracket: boolean;
		ps: PoolString;
		forLoop: pForLoopRec;
		handled: boolean;
	begin
		s := '';
		result.enum := tok_Unknown;
		if CutPrefix(InSample, t, @s) then
		begin
			result.enum := tok_UniqueIn;
			new(result.ps); result.ps^ := s;
			exit;
		end;
		if CutPrefix(OutSample, t, @s) then
		begin
			result.enum := tok_UniqueOut;
			new(result.ps); result.ps^ := s;
			exit;
		end;
		for i := 0 to High(Known) do
			if t = Known[i].t then
			begin
				result.enum := Known[i].enum;
				case result.enum of
					tok_NInsta: mask.Insta := yes;
					tok_ForLoopEnd: context.CloseForLoop;
				end;
				exit;
			end;
		if CutPrefix(VersionSample, t, @s) then
		begin
			version := s;
			exit;
		end;
		if CutSuffix('?', t, @s) then
		begin
			result.enum := tok_IfDef;
			new(result.defs); result.defs^ := nil;
			i := 1;
			repeat
				ns := ScanToken(s, i, DefineSyms);
				if ns <> '' then
				begin
					neg := CutPrefix('~', ns, @ns);

					id := length(result.defs^);
					SetLength(result.defs^, id + 1);
					handled := no;

					ps := ns;
					forLoop := context.FindForLoop(ps {$ifdef Debug}, yes {$endif});
					if Assigned(forLoop) then
					begin
						ts := ScanToken(s, i, DefineSyms);
						j := FindStr(ts, LiuCounters);
						if j >= 0 then
						begin
							handled := yes;
							result.defs^[id] := tDefine.ByLightUniform(tLightUniform(j), @ps);
						end;
					end;

					if not handled then
						result.defs^[id] := tDefine.Make(ns);

					case result.defs^[id].kind of
						def_Sf: mask.AppendMask(result.defs^[id].def);
						def_Lide: mask.AppendMask(tLightDetail(result.defs^[id].def.id));
						def_Liu: mask.AppendMask(tLightUniform(result.defs^[id].def.id));
					end;
					result.defs^[id].neg := neg;
				end else
					break;
			until no;
			exit;
		end;
		if CutPrefix('+', t, @s) then
		begin
			result.enum := tok_Define;
			new(result.def);
			result.def^ := tDefine.Make(s);
			exit;
		end;
		if t = ':' then
		begin
			result.enum := tok_Else;
			exit;
		end;
		if (t = '/') or (t = '/2') or (t = '/3') or (t = '/4') then
		begin
			result.enum := tok_EndIf;
			if t = '/' then result.id := 1 else result.id := StrToInt(Copy(t, 2, length(t) - 1));
			exit;
		end;
		id := FindStr(t, NuLightsTokens);
		if id >= 0 then
		begin
			result.enum := tok_NLights;
			result.lu := tLightUniform(id);
			// mask.AppendMask(result.lu);
			exit;
		end;
		if CutPrefix(UboSample, t, @s) then
		begin
			Include(flags, flag_Ubo);
			result.enum := tok_UboStart;
			new(result.ps); result.ps^ := s;
			exit;
		end;
		if CutPrefix(ForLoopSample, t, @s) then
		begin
			i := 1;
			ns := ScanToken(s, i);
			result.enum := tok_ForLoopStart;
			result.forKind := tForLoopKind(FindStr(ScanToken(s, i), ForLoopKindIds, ord(for_OmniA)));
			new(result.forVar); result.forVar^ := ns;
			context.OpenForLoop(result.forKind, result.forVar^, 0, 0, 0);
			exit;
		end;
		if CutAffixes('<', t, '>', @s) then
		begin
			result.enum := tok_Template;
			new(result.ps); result.ps^ := s;
			exit;
		end;
		for tex in GLTextureTarget do
			if t = TextureLookups[tex] then
			begin
				result.enum := tok_TextureLookup;
				result.tex := tex;
				exit;
			end;

		bracket := CutAffixes('[', t, ']', @s);
		i := 1;
		ns := ScanToken(s, i, DefaultTokenSyms - ['.'] + ['*']);
		s := ScanToken(s, i, DefaultTokenSyms - ['.'] + ['*']);
		ps := ns;
		forLoop := context.FindForLoop(ps {$ifdef Debug}, yes {$endif});
		if Assigned(forLoop) then
		begin
			if bracket then result.enum := tok_ForLoopBracketedVar else result.enum := tok_ForLoopVar;
			new(result.forv); result.forv^ := ps;
			result.special := -1;
			if s <> '' then
				case forLoop^.kind of
					for_Lights:
						begin
							id := FindStr(s, LiuCounters);
							if id >= 0 then result.special := id {$ifdef Debug} else Log('For.Lights: неизвестный особый счётчик "' + s + '"', logError) {$endif};
						end;
				{$ifdef Debug} else Log('For-цикл ' + ForLoopKindIds[forLoop^.kind] + ' не допускает особых счётчиков', logError); {$endif}
				end;
			exit;
		end;
	{$ifdef Debug} if result.enum = tok_Unknown then Log('Неизвестный токен: "' + t + '"', logError); {$endif}
	end;

	function ShaderParser._Optimize(const s: string): string;
	var
		i, n: sint;
		comment, rowcomment: boolean;
	begin
		SetLength(result, length(s));
		n := 0;
		comment := no;
		rowcomment := no;
		for i := 1 to length(s) do
		begin
			if comment then
				if (s[i - 2] = '*') and (s[i - 1] = '/') then comment := no else continue;
			if rowcomment then
				if s[i - 1] = EOL then rowcomment := no else continue;
			if (i + 1 <= length(s)) and (s[i] = '/') and (s[i + 1] in ['*', '/']) then
			begin
				if (s[i + 1] = '*') then comment := yes else rowcomment := yes;
				continue;
			end;

			if Symbol.IsWhitespace(s[i]) then
			begin
				if (n > 0) and (result[n] in IdentifierSyms) and (i < length(s)) and (s[i+1] in IdentifierSyms) then
				begin
					inc(n);
					result[n] := ' ';
				end;
				continue;
			end;

			inc(n);
			result[n] := s[i];
		end;
		SetLength(result, n);
	end;

	procedure ShaderParser._Finalize(var t: tToken);
	var
		i: sint;
	begin
		case t.enum of
			tok_IfDef:
				begin
					for i := 0 to High(t.defs^) do
						t.defs^[i].Finalize;
					dispose(t.defs);
				end;
			tok_Define:
				begin
					t.def^.Finalize;
					dispose(t.def);
				end;
			tok_Template, tok_UboStart, tok_UniqueIn, tok_UniqueOut: dispose(t.ps);
			tok_ForLoopVar, tok_ForLoopBracketedVar: dispose(t.forv);
			tok_ForLoopStart: dispose(t.forVar);
		end;
	end;

	function ShaderParser.tDefine.ByName(const newName: PoolString): tDefine;
	begin
		Assert(not Assigned(ShaderDefines^.Find(newName)));
		result := Empty;
		result.kind := def_Name;
		result.name := newName;
	end;

	function ShaderParser.tDefine.ByDefine(const adef: ShaderDefine): tDefine;
	begin
		result := Empty;
		result.kind := def_Sf;
		result.def  := adef;
	end;

	function ShaderParser.tDefine.ByLightDetail(aLide: tLightDetail): tDefine;
	begin
		result := Empty;
		result.kind := def_Lide;
		result.def := ShaderDefine.Make(Special, ord(aLide));
	end;

	function ShaderParser.tDefine.ByLightUniform(aLiu: tLightUniform; loopVar: pPoolString = nil): tDefine;
	begin
		result := Empty;
		if Assigned(loopVar) then
		begin
			result.kind := def_LiuLoop;
			result.name := loopVar^;
		end else
			result.kind := def_Liu;
		result.def := ShaderDefine.Make(Special, ord(aLiu));
	end;

	function ShaderParser.tDefine.Make(const s: PoolString): tDefine;
	var
		adef: pShaderDefine;
		id: sint;
	begin
		adef := ShaderDefines^.Find(s);
		if Assigned(adef) then exit(ByDefine(adef^));
		id := FindStr(s, LightDefines);
		if id >= 0 then exit(ByLightDetail(tLightDetail(id)));
		id := FindStr(s, LiuDefines);
		if id >= 0 then exit(ByLightUniform(tLightUniform(id)));
		result := ByName(s);
	end;

	procedure ShaderParser.tDefine.Finalize;
	begin
	end;

	procedure ShaderParser.tContext.Init(const newDefs: array of PoolString);
	var
		i: sint;
	begin
		fors := nil;
		SetLength(defs, length(newDefs));
		for i := 0 to High(newDefs) do
			defs[i] := newDefs[i];
	end;

	procedure ShaderParser.tContext.Done;
	begin
		if length(fors) > 0 then
		begin
		{$ifdef Debug} Log('Незакрытые for''ы (' + ToString(length(fors)) + ')!', logError); {$endif}
			while CloseForLoop do;
		end;
	end;

	function ShaderParser.tContext.OpenForLoop(kind: tForLoopKind; const varName: PoolString; max, jmp, jend: sint): pForLoopRec;
	var
		n: pForLoopRec;
	begin
	{$ifdef Debug} if Assigned(FindForLoop(varName, yes)) then Log('Цикл по той же переменной (' + varName + ')', logError); {$endif}
		SetLength(fors, length(fors) + 1);
		n := @fors[High(fors)];
		n^.kind := kind;
		n^.varName := varName;
		n^.i := 0;
		n^.max := max;
		n^.jmp := jmp;
		n^.jend := jend;
		n^.fair := no;
		result := n;
	end;

	function ShaderParser.tContext.FindForLoop(const varName: PoolString {$ifdef Debug}; allowFail: boolean = no {$endif}): pForLoopRec;
	var
		i: sint;
	begin
		for i := 0 to High(fors) do
			if fors[i].varName = varName then
				exit(@fors[i]);
	{$ifdef Debug} if not allowFail then Log('Цикл по ' + varName + ' не найден, сейчас всё рухнет', logError); {$endif}
		result := nil;
	end;

	function ShaderParser.tContext.FindForLoopEnd(var sp: ShaderParser; start: sint): sint;
	var
		op: sint;
	begin
		op := 0;
		while start < length(sp.tok) do
		begin
			case sp.tok[start].enum of
				tok_ForLoopStart: inc(op);
				tok_ForLoopEnd:
					begin
						dec(op);
						if op <= 0 then exit(start);
					end;
			end;
			inc(start);
		end;
		result := -1;
	end;

	function ShaderParser.tContext.CloseForLoop: boolean;
	begin
		result := length(fors) > 0;
		if result then
			SetLength(fors, length(fors) - 1);
	end;

	procedure ShaderParser.tContext.Define(const def: tDefine; var params: ShaderFlags);
	begin
		case def.kind of
			def_Name:
				begin
					SetLength(defs, length(defs) + 1);
					defs[High(defs)] := def.name;
				end;
			def_Sf:
				case def.def.Kind of
					Flag: params.user[def.def.id] := yes;
					else Assert(no, 'вручную допускается определять только флаги');
				end;
			else Assert(no, 'это определять нельзя');
		end;
	end;

	function ShaderParser.tContext.CheckDefines(var sp: ShaderParser; const pred: tDefineList; const params: ShaderFlags): boolean;
	var
		defined: boolean;
		i, j: sint;
		forLoop: pForLoopRec;
	begin
		result := no;
		for i := 0 to High(pred) do
		begin
			defined := no;

			case pred[i].kind of
				def_Name:
					begin
						for j := 0 to High(sp._gdefs) do
							if sp._gdefs[j] = pred[i].name then
							begin
								defined := yes;
								break;
							end;

						if not defined then
							for j := 0 to High(defs) do
								if defs[j] = pred[i].name then
								begin
									defined := yes;
									break;
								end;
					end;
				def_Sf:
					case pred[i].def.Kind of
						Flag: defined := params.user[pred[i].def.id];
						Special: defined := params.user.special[pred[i].def.id];
						else Assert(no);
					end;
				def_Lide: defined := params.NLights[tLightDetail(pred[i].def.id)] > 0;
				def_Liu: defined := params.GetNLights(tLightUniform(pred[i].def.id)) > 0;
				def_LiuLoop:
					begin
						forLoop := FindForLoop(pred[i].name);
						if Assigned(forLoop) then
							defined := tLightUniform(pred[i].def.id) in LightDetailInfo[params.GetLightDetail(forLoop^.i)].u;
					end;
				else Assert(no);
			end;

			result := result or (defined xor pred[i].neg);
			if result then exit;
		end;
	end;

	function ShaderParser._JmpNextBranch(start: sint): sint;
	var
		idskip: sint;
	begin
		result := start + 1;
		idskip := 1;
		while result <= High(tok) do
		begin
			case tok[result].enum of
				tok_IfDef: inc(idskip);
				tok_Else:
					begin
						Assert(idskip >= 1);
						if idskip = 1 then exit(result + 1);
					end;
				tok_EndIf:
					begin
						dec(idskip, tok[result].id);
						if idskip <= 0 then exit(result + 1);
					end;
			end;
			inc(result);
		end;
	end;

	procedure ShaderParser._BuildSource(const defs: array of PoolString; var s: StringBuilder; typ: ShaderType; chainId: sint; const aparams: ShaderFlags);
	const
		TextureLookupFunctions: array[GLTextureTarget] of string = ('texture1D', 'texture2D', 'texture3D', 'textureCube');

		procedure _SepId;
		begin
			if (s.Len > 0) and (s[s.Len - 1] in IdentifierSyms) then s.Append(' ');
		end;

	var
		params: ShaderFlags;
		tp: sint;
		t: pToken;
		max: sint;
		temp: pShaderParser;
		ctx: tContext;
		forLoop: pForLoopRec;
		completeUbo: string;
		suppressSepId, forFirst: boolean;
		n: uint;
		i, iend, shift: sint;
		ld: tLightDetail;
		lu: tLightUniform;
	begin
		params := aparams;
		ctx.Init(defs);
		completeUbo := '';
		suppressSepId := no;

		tp := 0;
		while tp <= High(tok) do
		begin
			t := @tok[tp];
			case t^.enum of
				tok_Unknown: ;
				tok_Raw:
					begin
						if (completeUbo <> '') and t^.rawContainsSomething then
						begin
							s.Append(completeUbo);
							completeUbo := '';
						end;
						if suppressSepId then
							suppressSepId := no
						else
							if t^.str[1] in IdentifierSyms then
								_SepId;
						s.Append(t^.str);
					end;
				tok_Template:
					begin
						temp := ShaderTemplates.Find(t^.ps^);
						if Assigned(temp) then temp^._BuildSource([], s, typ, chainId, params);
					end;
				tok_Define: ctx.Define(t^.def^, params);
				tok_IfDef:
					begin
						if not ctx.CheckDefines(self, t^.defs^, params) then
						begin
							tp := _JmpNextBranch(tp);
							continue;
						end;
					end;
				tok_Else:
					begin
						tp := _JmpNextBranch(tp);
						continue;
					end;
				tok_EndIf: ;
				tok_Pi: s.Append(ToString(pi, FloatFormat.MostPrecise));
				tok_Percent: s.Append('%');
				tok_NLights:
					begin
						n := params.GetNLights(t^.lu);
						Assert(n > 0);
						if n > 1 then s.Append('[', ToString(n), ']');
					end;
				tok_nInsta:
					if params.Insta then
						s.Append('[', ToString(gl.MaxInstances), ']');
				tok_InstanceID: if params.Insta then s.Append('[gl_InstanceID]');
				tok_NBones: s.Append('[', ToString(MaxGLBones), ']');
				tok_NCsmSplits: s.Append(ToString(Config.nCsmSplits));
				tok_UboStart: if gl.UBOSupported then completeUbo := 'layout(packed)uniform ' + t^.ps^ + '{';
				tok_UboEnd:
					if gl.UBOSupported then
						if completeUbo = '' then
							s.Append('};')
						else
						begin
						// {$ifdef Debug} Log('Пустой UBO — ' + completeUbo + '} — выброшен', logDebug); {$endif}
							completeUbo := '';
						end;
				tok_TextureLookup:
					if gl.OverloadedTextureFetch then s.Append('texture') else s.Append(TextureLookupFunctions[t^.tex]);
				tok_UniqueIn:
					begin
						_SepId;
						s.Append('VaR', ToString(chainId - 1), '_', t^.ps^);
					end;
				tok_UniqueOut:
					begin
						_SepId;
						s.Append('VaR', ToString(chainId), '_', t^.ps^);
					end;
				tok_In: if gl.InOutSupported then s.Append('in ') else
					if typ = GLshader_Vertex then s.Append('attribute ') else s.Append('varying ');
				tok_Out: if gl.InOutSupported then s.Append('out ') else s.Append('varying ');
				tok_Flat: if gl.InOutSupported then s.Append('flat ');
				tok_NoPerspective: if gl.InOutSupported then s.Append('noperspective ');
				tok_FragColor: if gl.InOutSupported then s.Append(FragColorVariable) else s.Append('gl_FragColor');
				tok_ForLoopStart:
					begin
						forFirst := (length(ctx.fors) = 0) or (ctx.fors[High(ctx.fors)].varName <> t^.forVar^);
						if forFirst then
						begin
							case t^.forKind of
								for_Lights: max := params.GetNLights(ulight_All);
								for_OmniA, for_OmniS, for_TargA, for_TargS: max := params.NLights[tLightDetail(ord(t^.forKind) - ord(for_OmniA))];
								for_Csm: max := Config.nCsmSplits;
								else Assert(no);
							end;
							forLoop := ctx.OpenForLoop(t^.forKind, t^.forVar^, max, tp, ctx.FindForLoopEnd(self, tp) + 1);
						end else
						begin
							forLoop := @ctx.fors[High(ctx.fors)];
							inc(forLoop^.i);
						end;

						if forLoop^.i >= forLoop^.max then
						begin
							tp := forLoop^.jend;
							ctx.CloseForLoop;
							continue;
						end;

						case forLoop^.kind of
							for_Lights:
								begin
									ld := params.GetLightDetail(forLoop^.i);
									if CanUseUniFor(ld) then
									begin
										forLoop^.fair := yes;
										forLoop^.start := forLoop^.i;
										forLoop^.i += sint(params.NLights[ld]) - 1;
										s.Append('for(int ', forLoop^.varName, '=0;', forLoop^.varName, '<', UniformNLights[ld], ';', forLoop^.varName, '++){');
									end;
								end;
						end;
					end;
				tok_ForLoopEnd:
					begin
						forLoop := @ctx.fors[High(ctx.fors)];
						if forLoop^.fair then
						begin
							s.Append('}');
							forLoop^.fair := no;
						end;
						Assert(forLoop^.jend = tp + 1);
						tp := forLoop^.jmp;
						continue;
					end;
				tok_ForLoopVar, tok_ForLoopBracketedVar:
					begin
						forLoop := ctx.FindForLoop(t^.forv^);
						i := forLoop^.i;
						iend := forLoop^.max;
						shift := 0;
						case forLoop^.kind of
							for_Lights:
								if (t^.special >= 0) or (forLoop^.fair) then
								begin
									if t^.special >= 0 then lu := tLightUniform(t^.special) else lu := ulight_All;

									if forLoop^.fair then
									begin
										shift := forLoop^.start - sint(params.GlobalizeLightUniform(lu, 0));
										Assert(shift >= 0, ToString(shift));
									end else
									begin
										i := params.LocalizeLightUniform(i, lu);
										iend := params.GetNLights(lu);
									end;
								end;
						end;

						if t^.enum = tok_ForLoopVar then
						begin
							Assert(not forLoop^.fair);
							s.Append(ToString(i));
							suppressSepId := yes; // это может быть частью идентификатора
						end else
						begin
							if forLoop^.fair then
							begin
								s.Append('[');
								if shift > 0 then s.Append(ToString(shift), '+');
								s.Append(forLoop^.varName, ']');
							end else
								if iend > 1 then s.Append('[', ToString(i), ']');
						end;
					end;
			end;
			inc(tp);
		end;
		ctx.Done;
	end;

	procedure ShaderParser.BuildSourceAndMask(const defs: array of PoolString; var s: StringBuilder; typ: ShaderType; chainId: sint; var params: ShaderFlags);
		function check(var ref: string; force: boolean = no): boolean;
		var
			sb: StringBuilder;
			r2: string;
		begin
			sb.Init;
			_BuildSource(defs, sb, typ, chainId, params);
			r2 := sb.DestructiveToString;
			result := r2 = ref;
			if result or force then ref := r2;
		end;

	var
		ref: string;
		i: sint;
		t: uint;
		ld: tLightDetail;
	begin
		ref := '';
		check(ref, yes);

		for i := 0 to UserShaderFlags.MAX_COUNT - 1 do
			if params.User[i] then
			begin
				params.User[i] := no;
				if not check(ref) then params.User[i] := yes;
			end;

		if params.user._special <> 0 then
		begin
			t := params.user._special;
			params.user._special := 0;
			if not check(ref) then params.user._special := t;
		end;

		for ld in tLightDetail do
			if params.NLights[ld] <> 0 then
			begin
				t := params.NLights[ld];
				params.NLights[ld] := 0;
				if not check(ref) then params.NLights[ld] := t;
			end;

		if params.Insta then
		begin
			params.Insta := no;
			if not check(ref) then params.Insta := yes;
		end;
		s.Append(ref);
	end;

	constructor ShaderSourceTemplates.Init;
	begin
		_t := nil;
	end;

	destructor ShaderSourceTemplates.Done;
	var
		i: sint;
	begin
		for i := 0 to High(_t) do
			_t[i].code.Done;
	end;

	procedure ShaderSourceTemplates.Add(const name: PoolString; const code: string);
	begin
		if Assigned(Find(name {$ifdef Debug}, no{$endif})) then
		begin
		{$ifdef Debug} LogR('Дублирующийся шейдерный темплейт "' + name + '" проигнорирован; ', logWarning); {$endif}
			exit;
		end;
	{$ifdef Debug} LogR('"' + name + '"; ', logDebug); {$endif}
		SetLength(_t, length(_t) + 1);
		_t[High(_t)].name := name;
		_t[High(_t)].code.Init([], code);
	end;

	procedure ShaderSourceTemplates.Load(s: pStream);
	var
		ts: pTokenizer;
		id, name: string;
		code: string;
	{$ifdef Debug} oldCount: sint; {$endif}
	begin
		if not Assigned(s) then exit;
		ts := MakeRef(new(pTokenizer, Init(s)));
		if not Assigned(ts) then exit;

	{$ifdef Debug}
		LogR('Загрузка шейдерных темплейтов из "' + StreamPath.FilenameNoExt(ts^.StreamPath) + '"... ');
		oldCount := length(_t);
	{$endif}
		repeat
			case ts^.NextTokenType of
				token_Identifier: id := ts^.ReadIdentifier;
				else break;
			end;
			case id of
				'template':
					begin
						name := ts^.ReadString;
						code := ts^.ReadString;
						Add(name, code);
					end;
				else raise UnknownIdentifier(id, 'Shader.Templates');
			end;
		until no;
	{$ifdef Debug} Log(lang_amount(length(_t) - oldCount, '{N} темплейт{/а/ов} загружен{/о/о}.'), logDebug); {$endif}
		Release(ts);
	end;

	function ShaderSourceTemplates.Find(const name: PoolString {$ifdef Debug}; warn: boolean = yes{$endif} ): pShaderParser;
	var
		i: sint;
	begin
		for i := 0 to High(_t) do
			if _t[i].name = name then
				exit(@_t[i].code);
		result := nil;
	{$ifdef Debug} if warn then Log('Неизвестный шейдерный темплейт："{0}".', name, logError); {$endif}
	end;

	procedure TextureImageInfo.Init(newTarget: GLTextureTarget; const newSize: UintVec3; newFormat: GLImageFormat; newFlags: TextureImageFlags);
	var
		i: uint;
	begin
		if newSize.Positive then ValidateImageSize(newSize);
	{$ifdef Debug} if not newSize.Positive then Log('{0}-текстура нулевого размера: {1}.', GLTextureTargetIds[newTarget], ToString(newSize), logWarning); {$endif}
		target := newTarget;
		for i := 0 to High(newSize.data) do
			if i < TextureTargetsInfo[target].determinativeDims then
			begin
				size.data[i] := newSize.data[i];
			end else if (target = GLtexture_Cube) and (i = 1) then
				size.data[i] := size.data[0]
			else
			begin
				Assert(newSize.data[i] = 1, GLTextureTargetIds[newTarget] + ': ' + ToString(newSize));
				size.data[i] := 1;
			end;

		flags := newFlags;
		format := newFormat;
		nLevels := TextureTargetsInfo[target].faces;
		if texture_Mips in flags then
			nLevels += nLevels * Log2(uint(max(size.x, size.y, size.z)));
	end;

	procedure TextureImage.Init(const newSize: UintVec2; newFormat: GLImageFormat; newFlags: TextureImageFlags = []);
	begin
		Init(GLtexture_2D, newSize, newFormat, newFlags);
	end;

	procedure TextureImage.Init(const newSize: UintVec3; newFormat: GLImageFormat; newFlags: TextureImageFlags = []);
	begin
		Init(GLtexture_3D, newSize, newFormat, newFlags);
	end;

	procedure TextureImage.Init(newTarget: GLTextureTarget; const newSize: UintSize3; newFormat: GLImageFormat; newFlags: TextureImageFlags = []);
	begin
		Prepare(newTarget, newSize, newFormat, newFlags);
	end;

	procedure TextureImage.Prepare(newTarget: GLTextureTarget; const newSize: UintSize3; newFormat: GLImageFormat; newFlags: TextureImageFlags = []);
	var
		i: uint;
	begin
		FirstLevel := nil;
		otherLevels := nil;
		info.Init(newTarget, newSize, newFormat, newFlags - [texture_ManualImgs]);
		if not (texture_ManualImgs in newFlags) then FirstLevel := GetMem(info.GetLevelDataSize(0));

		if nLevels = 1 then otherLevels := nil else
		begin
			otherLevels := GetMem((nLevels - 1) * sizeof(pointer));
			for i := 1 to nLevels - 1 do
				if texture_ManualImgs in newFlags then otherLevels[i - 1] := nil else otherLevels[i - 1] := GetMem(info.GetLevelDataSize(i));
		end;
	end;

	procedure TextureImage.Init(s: pStream; const forceLoader: string = ''; size: size_t = 0);
	{$ifdef Debug}
		function _exinfo: string;
		begin
			result := '';
			if s^.Position > FilePos.Zero then
				result += ' (+' + ToString(s^.Position) + ', размер данных: ' + ToStringSuff_b(size) + ')';
		end;
	{$endif}
	{$ifdef Debug} var name: string; {$endif}
	begin
		Invalidate;
		if size = 0 then size := (s^.Size - s^.Position).AsSizeT;
		try2
			s^.NewRef;
		{$ifdef Debug}
			name := StreamPath.FilenameNoExt(s^.path);
			LogR('Загрузка текстуры из ' + StreamPath.Log(s^.path) + _exinfo + '... ');
		{$endif}
			Loaders.Size(FileSize.Explicit(size)).ForceLoader(forceLoader).Load(@self, s);
		{$ifdef Debug} Log('Текстура "' + name + '" загружена [' + info.DimsToStr(0) + ']; ', logOK); {$endif}
		finally2
			Release(s);
		except2
			Done;
			raise;
		end;
	end;

	procedure TextureImage.Done;
	begin
		if not OK then exit;
		FreeData;
		Invalidate;
	end;	
{$define classname := TextureImage} {$define pSelf := pTextureImage}
{$define constructor_args := texture_image_constructor_args} {$define pass_constructor_args := newTarget, newSize, newFormat, newFlags}
{$define constructor_args2 := texture_image_constructor_args2} {$define pass_constructor_args2 := s, forceLoader, size}
{$include dyn_obj.pp.inc}

	procedure TextureImage.Invalidate;
	begin
		info.nLevels := 0;
	end;

	function TextureImage.OK: boolean;
	begin
		result := info.nLevels > 0;
	end;

	function TextureImageInfo.GetLevelDimension(level, dim: uint): uint;
	begin
		result := Max(1, size.data[dim] shr Defaced(level));
	end;

	function TextureImageInfo.LevelSize(level: uint): UintVec3;
	begin
		result := Max(UintVec3.Ones, size shr Defaced(level));
	end;

	function TextureImageInfo.LevelSizeXY(level: uint): UintVec2;
	begin
		result := Max(UintVec2.Ones, size.XY shr Defaced(level));
	end;

	function TextureImageInfo.GetPixelsCount(level: uint): uint;
	begin
		result := LevelSize(level).Product;
	end;

	function TextureImageInfo.GetLevelDataSize(level: uint): size_t;
	begin
		result := GetTextureDataSize(LevelSize(level), format);
	end;

	function TextureImageInfo.DimsToStr(level: uint): string;
	begin
		result := SizeToString(LevelSize(level)) + ' @ ' + GLImageFormatIds[format];
	end;

	function TextureImageInfo.RowSize: size_t;
	begin
		result := GLImageFormatsInfo[format].pixelSize * size_t(size.x);
	end;

	function TextureImageInfo.PlaneSize(level: uint = 0): size_t;
	begin
		result := GetTextureDataSize(LevelSizeXY(level), format);
	end;

	function TextureImageInfo.Defaced(level: uint): uint;
	begin
		if target = GLtexture_Cube then result := level div 6 else result := level;
		Assert(level < TextureTargetsInfo[target].faces * (1 + result));
	end;

	procedure TextureImage.FreeData;
	var
		i: uint;
	begin
		FreeMem(FirstLevel);
		if Assigned(otherLevels) then
		begin
			for i := 1 to nLevels - 1 do
				FreeMem(otherLevels[i - 1]);
			FreeMem(otherLevels);
		end;
	end;

	procedure TextureImage.Save(const stream: string; const opts: string = '');
	begin
	{$ifdef Debug} LogR('Сохранение изображения: ' + StreamPath.Log(stream) + '... '); {$endif}
		Loaders.Opts(opts).Save(@self, stream);
	{$ifdef Debug} Log('Изображение сохранено в ' + StreamPath.Log(stream), logOK); {$endif}
	end;

	procedure TextureImage.Save(const stream: string; const sizes: UintVec2; format: GLImageFormat; data: pointer; takeThisData: boolean = no; const opts: string = '');
	var
		flags: TextureImageFlags;
		temp: TextureImage;
	begin
		flags := [];
		if not takeThisData then flags += [texture_ManualImgs];
		temp.Init(GLtexture_2D, sizes, format, flags);
		temp.ReplaceLevelWithOwnPointer(0, data);
		try
			temp.Save(stream, opts);
		finally
			if not takeThisData then temp.ReplaceLevelWithOwnPointer(0, nil, yes);
			temp.Done;
		end;
	end;

	procedure TextureImage.Save(const stream: string; level: uint);
	begin
		ValidateLevel(level);
		Save(stream, info.LevelSizeXY(level), format, LevelPtr(level), no);
	end;

	procedure TextureImage.CheckSaveTarget(supported: GLTextureTargets; const fmt: string);
	begin
		if not (target in supported) then
			raise Error('Сохранение {0}-текстуры в {1} не поддерживается.', GLTextureTargetIds[target], fmt);
	end;

	function TextureImage.ValidateCoord(x, y: uint): boolean;
	begin
		result := (x < size.X) and (y < size.Y);
	end;

	function TextureImage.ValidateCoord(x, y, z, level: uint): boolean;
	begin
		result := (x < info.GetLevelDimension(level, 0)) and (y < info.GetLevelDimension(level, 1)) and (z < info.GetLevelDimension(level, 2)) and
			(level < nLevels);
	end;

	procedure TextureImage.ValidateCoordThrow(x, y, z, level: uint);
	begin
		if (x < info.GetLevelDimension(level, 0)) and (y < info.GetLevelDimension(level, 1)) and (z < info.GetLevelDimension(level, 2)) and (level < nLevels) then
		else
			raise Error('Точка текстуры вне границ.');
	end;

	procedure TextureImage.ValidateLevel(level: uint);
	begin
		if level >= nLevels then raise Error('Текстура не содержит уровня {0}.', [level]);
	end;

	function TextureImage.GetPixel(x, y, z, level: sint; out px: Vec4): boolean;
	var
		ptr: pointer;
	begin
		px := DefaultTextureSample;
		ptr := PixelPtr(x, y, z, level);
		result := Assigned(ptr) and GetPixel(ptr, px);
	end;

	function TextureImage.GetPixel(x, y, z, level: sint): Vec4;
	begin
		GetPixel(x, y, z, level, result);
	end;

	function TextureImage.GetPixel(ptr: pointer; out px: Vec4): boolean;
	const
		InvByte = 1.0 / High(uint8);
	var
		u8: pUint8 absolute ptr;
		i: sint;
	begin
		px := DefaultTextureSample;
		result := yes;
		case format of
			GLformat_R .. GLformat_RGBA:
				for i := 0 to ord(format) - ord(GLformat_R) do
					px.data[i] := u8[i] * InvByte;
			GLformat_BGR:
				for i := 0 to 2 do
					px.data[i] := u8[2 - i] * InvByte;
			GLformat_BGRA:
				begin
					for i := 0 to 2 do
						px.data[i] := u8[2 - i] * InvByte;
					px.w := u8[3] * InvByte;
				end;
			else
				result := no;
		end;
	end;

	function TextureImage.SetPixel(x, y, z, level: sint; const px: Vec4): boolean;
	var
		ptr: pointer;
	begin
		ptr := PixelPtr(x, y, z, level);
		result := Assigned(ptr);
		if result then SetPixel(ptr, px);
	end;

	procedure TextureImage.SetPixel(ptr: pointer; const px: Vec4);
	const
		U8K = float(High(uint8));
	var
		u8: pUint8 absolute ptr;
		i: sint;
	begin
		case format of
			GLformat_R .. GLformat_RGBA:
				for i := 0 to GLImageFormatsInfo[format].nChannels - 1 do
					u8[i] := clamp(round(px.data[i] * U8K), 0, High(uint8));
			GLformat_BGR:
				for i := 0 to 2 do
					u8[2 - i] := clamp(round(px.data[i] * U8K), 0, High(uint8));
			GLformat_BGRA:
				begin
					for i := 0 to 2 do
						u8[2 - i] := clamp(round(px.data[i] * U8K), 0, High(uint8));
					u8[3] := clamp(round(px.w * U8K), 0, High(uint8));
				end;
		end;
	end;

	procedure TextureImage.CombinePixel(x, y, z, level: sint; const color: Vec4; mode: ImageCombineMode);
	var
		ptr: pointer;
	begin
		ptr := PixelPtr(x, y, z, level);
		if Assigned(ptr) then CombinePixel(ptr, color, mode);
	end;

	procedure TextureImage.CombinePixel(ptr: pointer; const color: Vec4; mode: ImageCombineMode);
	var
		px: Vec4;
	begin
		if mode = img_Replace then SetPixel(ptr, color) else
			if GetPixel(ptr, px) then
			begin
				case mode of
					img_Transparency: px := px * (1.0 - color.w) + color * color.w;
					else
						Assert(no);
				end;
				SetPixel(ptr, px);
			end;
	end;

{$define impl8:=
begin
	case mode of

		img_Replace:
			begin
			{$if nch=1} pUint8(ptr)^ := rgba[0];
			{$elseif nch=2} pUint16(ptr)^ := pUint16(@rgba)^;
			{$elseif (nch=3) or (nch=4)}
				{$if defined(bgr)}
					pUint8(ptr)[0] := rgba[2];
					pUint8(ptr)[1] := rgba[1];
					pUint8(ptr)[2] := rgba[0];
					{$if nch=4} pUint8(ptr)[3] := rgba[3]; {$endif}
				{$elseif nch=3}
					pVec3u8(ptr)^ := pVec3u8(@rgba)^;
				{$else}
					pVec4u8(ptr)^ := pVec4u8(@rgba)^;
				{$endif}
			{$else} {$error wrong nch} {$endif}
			end;

		img_Transparency:
			begin
			{$define imp2:=pUint8(ptr)[c] := (uint(pUint8(ptr)[c]) * uint(High(uint8) - rgba[3]) + uint(rgba[c]) * rgba[3] + (High(uint8) - 1) div 2) div High(uint8) {$undef c}}
				{$define c:=0} imp2;
				{$if nch>=2} {$define c:=1} imp2; {$endif}
				{$if nch>=3} {$define c:=2} imp2; {$endif}
				{$if nch>=4} {$define c:=3} imp2; {$endif}
			{$undef imp2}
			end;

		else
			Assert(no);
	end;
end {$undef nch} {$undef bgr}}
	procedure TextureImage.CombinePixel(ptr: pointer; const rgba: Vec4u8; mode: ImageCombineMode);
	const
		InvU8 = 1.0 / High(uint8);
	begin
		case format of
			GLformat_R: {$define nch:=1} impl8;
			GLformat_RG: {$define nch:=2} impl8;
			GLformat_RGB: {$define nch:=3} impl8;
			GLformat_RGBA: {$define nch:=4} impl8;
			GLformat_BGR: {$define nch:=3} {$define bgr} impl8;
			GLformat_BGRA: {$define nch:=4} {$define bgr} impl8;
			else
				begin
				{$ifdef Debug} Log('CombinePixel: нерационально (' + GLImageFormatIds[format] + ')', logWarning); {$endif}
					CombinePixel(ptr, Vec4.Make(rgba[0] * InvU8, rgba[1] * InvU8, rgba[2] * InvU8, rgba[3] * InvU8), mode);
				end;
		end;
	end;
{$undef impl8}

	function TextureImage.PixelSize: size_t;
	begin
		result := GLImageFormatsInfo[format].pixelSize;
	end;

	function TextureImage.LevelPtr(level: uint): pointer;
	begin
		Assert(level < nLevels);
		if level = 0 then result := FirstLevel else result := otherLevels[level - 1];
	end;

	procedure TextureImage.ReplaceLevelWithOwnPointer(level: uint; data: pointer; silent: boolean = no);
	var
		lvp: pPointer;
	begin
		Assert(level < nLevels);
		if level = 0 then lvp := @FirstLevel else lvp := @otherLevels[level - 1];
		if not silent then FreeMem(lvp^);
		lvp^ := data;
	end;

	function TextureImage.PixelPtr(x, y, z, level: sint): pointer;
	var
		shift: size_t;
	begin
		if ValidateCoord(x, y, z, level) then
		begin
			shift := PixelSize * (size_t(info.GetLevelDimension(level, 0)) * (size_t(info.GetLevelDimension(level, 1)) * size_t(z) + size_t(y)) + size_t(x));
			result := LevelPtr(level) + shift;
		end else
			result := nil;
	end;

	function TextureImage.PixelPtr(x, y: sint): pointer;
	begin
		Assert(ValidateCoord(x, y));
		result := FirstLevel + PixelSize * (size_t(Size.X) * size_t(y) + size_t(x));
	end;

	function TextureImage.PixelPtr(offset: uint): pointer;
	begin
		result := FirstLevel + PixelSize * offset;
	end;

	procedure TextureImage.Blit(var from: TextureImage; fromX, fromY, fromZ: uint; fromLv: uint; sx, sy, sz: uint; toX, toY, toZ: uint; toLv: uint; mode: ImageCombineMode);

		function adjust_coord(fid, tid: uint; var cfrom, cto, sz: uint): boolean;
		var
			maxFrom, maxTo: uint;
		begin
			maxFrom := from.info.GetLevelDimension(fromLv, fid);
			maxTo := info.GetLevelDimension(toLv, tid);
			result := (maxFrom > cfrom) and (maxTo > cto);
			if result then sz := min(sz, maxFrom - cfrom, maxTo - cto);
		end;

	var
		x, y, z: sint;
		px: Vec4;
		handled: boolean;
		from_psz, to_psz, from_rowsz, to_rowsz, from_dnexty, to_dnexty, from_dnextz, to_dnextz: size_t;
		from_p, to_p: pointer;
	begin
		handled := no;
		if (not adjust_coord(0, 0, fromX, toX, sx)) or
			(not adjust_coord(1, 1, fromY, toY, sy)) or
			(not adjust_coord(2, 2, fromZ, toZ, sz))
		then
			exit;
		Assert(
			(toX + sx <= info.GetLevelDimension(toLv, 0)) and
			(toY + sy <= info.GetLevelDimension(toLv, 1)) and
			(toZ + sz <= info.GetLevelDimension(toLv, 2)));

		from_psz := GLImageFormatsInfo[from.format].pixelSize;
		to_psz := GLImageFormatsInfo[format].pixelSize;
		from_rowsz := size_t(sx) * from_psz;
		to_rowsz := size_t(sx) * to_psz;
		from_dnexty := size_t(from.size.X - sx) * from_psz;
		to_dnexty := size_t(size.X - sx) * to_psz;
		from_dnextz := from_dnexty + size_t(from.size.Y - sy) * size_t(from.size.X) * from_psz;
		to_dnextz := to_dnexty + size_t(size.Y - sy) * size_t(size.X) * to_psz;

		if not handled then
		begin
		// {$ifdef Debug} Log('TextureImage.Blit: fallback на медленный алгоритм (' + GLImageFormatIds[from.format] + ' -> ' + GLImageFormatIds[format] + ', mode = ' + ImageCombineModeIds[mode] + ')', logWarning); {$endif}
			from_p := from.PixelPtr(fromX, fromY, fromZ, fromLv);
			to_p := PixelPtr(toX, toY, toZ, toLv);
			for z := 0 to sz - 1 do
			begin
				for y := 0 to sy - 1 do
				begin
					if (mode = img_Replace) and (from.format = format) then
					begin
						memcpy(from_p, to_p, from_rowsz);
						from_p += from_rowsz;
						to_p += to_rowsz;
					end else
						for x := 0 to sx - 1 do
						begin
							if from.GetPixel(from_p, px) then
								CombinePixel(to_p, px, mode);
							from_p += from_psz;
							to_p += to_psz;
						end;
					from_p += from_dnexty;
					to_p += to_dnexty;
				end;
				from_p += from_dnextz;
				to_p += to_dnextz;
			end;
		end;
	end;

	procedure TextureImage.RotateCwOZ;
	var
		lv, x, y, z, sx, sy, syx, sz: sint;
		dz, dn, dp: size_t;
		p, np: pointer;
	begin
		for lv := 0 to nLevels - 1 do
		begin
			p := LevelPtr(lv);
			np := GetMem(info.GetLevelDataSize(lv));

			sx := info.GetLevelDimension(lv, 0);
			sy := info.GetLevelDimension(lv, 1);
			sz := info.GetLevelDimension(lv, 2);
			syx := sx * sy;

		{$define impl:=
			begin
				dz := 0;
				dp := 0;
				for z := 0 to sz - 1 do
				begin
					for y := 0 to sy - 1 do
					begin
						dn := dz + size_t(sy - 1 - y);
						for x := 0 to sx - 1 do
						begin
							_TYPTR_(np)[dn] := _TYPTR_(p)[dp];
							dn += size_t(sy);
							dp += 1;
						end;
					end;
					dz += size_t(syx);
				end;
			end
		{$undef _TYPTR_}}

			case GLImageFormatsInfo[format].pixelSize of
				1: {$define _TYPTR_:=pUint8} impl;
				2: {$define _TYPTR_:=pUint16} impl;
				3: {$define _TYPTR_:=pVec3u8} impl;
				4: {$define _TYPTR_:=pUint32} impl;
				else
					begin
					{$ifdef Debug} Log('TextureImage.RotateCwOZ: размер пиксела ' + ToString(GLImageFormatsInfo[format].pixelSize) + ' (' + GLImageFormatIds[format] + ') не поддерживается', logError); {$endif}
						FreeMem(np);
						exit;
					end;
			end;
		{$undef impl}

			ReplaceLevelWithOwnPointer(lv, np);
		end;
		info.Init(target, UintVec3.Make(Size.Y, Size.X, Size.Z), format, info.flags);
	end;

	procedure TextureImage.Blit(var from: TextureImage; toX, toY, toZ, toLv: uint; mode: ImageCombineMode);
	begin
		Blit(from, 0, 0, 0, 0, from.Size.X, from.Size.Y, from.Size.Z, toX, toY, toZ, toLv, mode);
	end;

	procedure TextureImage.Fill(const color: Vec4; level: sint = 0);
	var
		i, x, y, z, nPx: sint;
		p: pointer;
		c8: array[0 .. 3] of uint8;
	begin
		{$define fill_8:=
		begin
			{$if nch=1} FillByte(p^, nPx, pUint8(@c8)^);
			{$elseif nch=2} FillWord(p^, nPx, pUint16(@c8)^);
			{$elseif nch=4} FillDWord(p^, nPx, pUint32(@c8)^);
			{$elseif nch=3}
				for i := 0 to nPx - 1 do
				begin
					pVec3u8(p)^ := pVec3u8(@c8)^; pVec3u8(p) += 1;
				end;
			{$else} {$error wrong nch} {$endif}
		end
		{$undef nch}}

		p := LevelPtr(level);
		nPx := info.GetPixelsCount(level);
		case format of
			GLformat_R .. GLformat_RGBA:
				for i := 0 to GLImageFormatsInfo[format].nChannels - 1 do
					c8[i] := clamp(round(High(uint8) * color.data[i]), 0, High(uint8));
			GLformat_BGR, GLformat_BGRA:
				begin
					for i := 0 to 2 do
						c8[2 - i] := clamp(round(High(uint8) * color.data[i]), 0, High(uint8));
					if format = GLformat_BGRA then
						c8[3] := clamp(round(High(uint8) * color.w), 0, High(uint8));
				end;
		end;

		case format of
			GLformat_R: {$define nch:=1} fill_8;
			GLformat_RG: {$define nch:=2} fill_8;
			GLformat_RGB, GLformat_BGR: {$define nch:=3} fill_8;
			GLformat_RGBA, GLformat_BGRA: {$define nch:=4} fill_8;
			else
			begin
			{$ifdef Debug} Log('TextureImage.Fill: fallback на медленный алгоритм (' + GLImageFormatIds[format] + ')', logWarning); {$endif}
				for z := 0 to Size.Z - 1 do
					for y := 0 to Size.Y - 1 do
						for x := 0 to Size.X - 1 do
							SetPixel(x, y, z, level, color);
			end;
		end;

		{$undef fill_8}
	end;

{$define accessor := ShaderDefines} {$define instance_type := pShaderDefinesManager}
{$define create_instance := ShaderDefinesManager.Create} {$define destroy_instance := instance^.Free} {$define unitname := 'GLBase'}
{$include lazy_singleton.inc}

{$ifdef use_serialization}
const
	COUNT_HAS_CALLBACKS_BIT = 1 shl 0;
	COUNT_NBITS             = 1;

	procedure SerializeGLEntityParams(se: pSerializer; obj: pointer);
	var
		gl: pGLEntityParams absolute obj;
		v: pNativeGLValues;
		cv: pNativeGLValue;
		hasCallbacks: boolean;
		i: sint;
		flags: uint;
	begin
		with se^ do
		begin
			gl^.flags.Serialize(stream);
			v := @gl^.values;

			hasCallbacks := Assigned(v^._onAdd) or Assigned(v^._onRelocate);
			flags := 0;
			for i := 0 to High(v^.raw) do
			begin
				Assert(v^.raw[i].flags * [NativeGLValueFlag.InstaPack] = [], '' + v^.raw[i].namae);
				if not (NativeGLValueFlag.NonSerializable in v^.raw[i].flags) then
					inc(flags);
			end;
			flags := flags shl 1;
			if hasCallbacks then flags := flags or COUNT_HAS_CALLBACKS_BIT;
			VarInt.Write(stream, flags);

			for i := 0 to High(v^.raw) do
			begin
				cv := @v^.raw[i];
				if NativeGLValueFlag.NonSerializable in cv^.flags then continue;

				Serialize_string(stream, cv^._namae);
				Serialize_ui8(stream, ord(cv^._type));
				VarInt.Write(stream, cv^._count);
				if cv^._count > 0 then
					if cv^.type_ = GLType.Sampler then
						SeObject(cv^.RawTex)
					else
						stream^.Write(cv^.Ptr, cv^.DataSize);
			end;

			if hasCallbacks then
			begin
				SeFunction(v^._onAdd);
				SeFunction(v^._onRelocate);
				SeObject(v^._userParam);
			end;
		end;
	end;

	procedure DeserializeGLEntityParams(de: pDeserializer; obj: pointer);
	var
		gl: pGLEntityParams absolute obj;
		v: pNativeGLValues;
		cv: pNativeGLValue;
		i: sint;
		flags: uint;
		hasCallbacks: boolean;
	begin
		with de^ do
		begin
			gl^.flags := UserShaderFlags.Deserialize(stream);
			v := @gl^.values;

			flags := VarInt.Read(stream);
			hasCallbacks := (flags and COUNT_HAS_CALLBACKS_BIT) <> 0;
			SetLength(v^.raw, flags shr COUNT_NBITS);
			for i := 0 to High(v^.raw) do
			begin
				cv := @v^.raw[i];
				cv^._version := NativeGLValue._NewVersion;
				cv^._flags := [];

				cv^._namae := Deserialize_string(stream);
				cv^._type := GLType(Deserialize_ui8(stream));
				cv^._count := VarInt.Read(stream);
				cv^._allocated := cv^._count;
				if cv^._type <> GLType.Sampler then
				begin
					cv^.data._ptr := GetMem(cv^.DataSize);
					stream^.Read(cv^.Ptr, cv^.DataSize);
				end else
					if cv^._count > 0 then
						DeObjectA(cv^.data._ptr);
			end;

			if hasCallbacks then
			begin
				pointer(v^._onAdd) := DeFunction();
				pointer(v^._onRelocate) := DeFunction();
				DeWeakR(v^._userParam);
			end else
			begin
				v^._onAdd := nil;
				v^._onRelocate := nil;
				v^._userParam := nil;
			end;
		end;
	end;

	procedure GLEntityParamsDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		gl: pGLEntityParams absolute obj;
		i: sint;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: gl^.Init;
			de_After:
				begin
					for i := 0 to High(gl^.values.raw) do
						gl^.values._hash.Add(gl^.values.raw[i].namae, @gl^.values.raw[i]);
				end;
		end;
	end;

	procedure SerializeSlideGL(se: pSerializer; obj: pointer);
	var
		ac: pSlideGL absolute obj;
	begin
		with se^ do
		begin
		{$ifdef DebugSlideGL} Log('Сохраняю SlideGL: "' + ac^._v^.namae^.Ansi + '" (/' + ToString(ac^._gl^.values._hash.count) + ')', logDebug); {$endif}
			SeObject(@ac^._dm, ObjType_DimensionalMove);
			SeObject(ac^._gl, TypeOf(GLEntityParams));
			Serialize_string(stream, ac^._v^.namae);
		end;
	end;

	procedure DeserializeSlideGL(de: pDeserializer; obj: pointer);
	var
		ac: pSlideGL absolute obj;
	begin
		with de^ do
		begin
			DeWeakAtR(ac^._dm);
			DeWeakR(ac^._gl);
			new(pPoolString(ac^._v)); pPoolString(ac^._v)^ := Deserialize_string(stream);
		end;
	end;

	procedure SlideGLDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		ac: pSlideGL absolute obj;
		nm: PoolString;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: ac^.DeseInit;
			de_After2: // After занят gl.values.hash
				begin
					nm := pPoolString(ac^._v)^; dispose(pPoolString(ac^._v));
				{$ifdef Debug} Log('Загружаю SlideGL: "' + nm + '" (/' + ToString(ac^._gl^.values._hash.count) + ')', logDebug); {$endif}
					ac^._v := ac^._gl^.values.Value(nm);
				end;
		end;
	end;
{$endif}

	procedure Init;
	begin
		Assert(UNIFORMS_SUPPLY <= MinUniforms);
		NativeGLValue._nextVersion := 0;

	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Render pass', TypeOf(RenderPass), nil, sizeof(RenderPass), yes, nil, nil, nil, nil)
		^.RegisterType('Render scenario', TypeOf(RenderScenario), nil, sizeof(RenderScenario), yes, nil, nil, nil, nil)
		^.RegisterType('GL entity params', TypeOf(GLEntityParams), nil, sizeof(GLEntityParams), no,
			@SerializeGLEntityParams, @DeserializeGLEntityParams, nil, @GLEntityParamsDeSpecial)
		^.RegisterType('Slide GL', TypeOf(SlideGL), TypeOf(EntityAction), sizeof(SlideGL), yes,
			@SerializeSlideGL, @DeserializeSlideGL, nil, @SlideGLDeSpecial);
	{$endif}

		ShaderTemplates.Init;

		RenderPasses.Init;
		RenderScenarios.Init;
		MainPass.Init('main', pass_Generic, yes);
		ShadowPass.Init('shadow', pass_Generic, yes);
		TargShadowPass.Init('targ_shadow', pass_Generic, yes);
		LightingPass.Init('light', pass_UseLights, yes);
		MinimapPass.Init('minimap', pass_Generic, yes);
		MainScenario.Init('main', [@LightingPass, @MainPass], [scenario_Uvis, scenario_Static]);
		ShadowScenario.Init('shadow', [@ShadowPass], [scenario_Static]);
		TargShadowScenario.Init('targ_shadow', [@TargShadowPass], [scenario_Static]);
		MinimapScenario.Init('minimap', [@MinimapPass], [scenario_Static]);

	{$ifdef use_serialization}
		SerializationDB.Shared^
		.AddEnv([@MainPass, @LightingPass, @ShadowPass, @TargShadowPass, @MinimapPass,
				@MainScenario, @ShadowScenario, @TargShadowScenario, @MinimapScenario]);
	{$endif}
	end;

	procedure Done;
	begin
		ShaderTemplates.Done;
		RenderScenarios.Done;
		RenderPasses.Done;
	end;

initialization
	&Unit('GLBase').Initialize(@Init, @Done);
end.
