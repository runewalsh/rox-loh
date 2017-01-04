unit U_GL;

{$include opts.inc}
{$define pure_virtual := virtual; abstract}

interface

uses
	USystem, OpenGL_Headers, UMath, UClasses, Utils {$ifdef Debug}, ULog, Errors {$endif} {$ifdef Profile}, Profile {$endif};

type
	// Это просто для удобства передачи параметров нескольких размерностей, например, размеров текстуры.
	// UintSize добивается до вектора единицами, UintOffset — нулями.
	UintSize3 = object(UintVec3) end;
	UintOffset3 = object(UintVec3) end;

	operator :=(const v: UintVec2): UintSize3;
	operator :=(const v: UintVec3): UintSize3;
	operator :=(const v: UintVec2): UintOffset3;
	operator :=(const v: UintVec3): UintOffset3;

const
	MaxMRTSlots = 4;
	MaxTextureUnits = 12;
	MaxVertexAttributes = 8;
	MaxUniformBuffers = 8;

	MinUniforms = 1536;
	MinVertexOutputs = 48;
	MinFragmentInputs = 48;
	FragColorVariable = 'fragColor';
	BinaryShadersCacheFile = 'shaders.kv';
	GLResourceTag = 'GL';

type
	GLfloat = float32; pGLfloat = ^GLfloat;
	Vec2f = packed array[0 .. 1] of GLfloat;  pVec2f = ^Vec2f;
	Vec3f = packed array[0 .. 2] of GLfloat;  pVec3f = ^Vec3f;
	Vec4f = packed array[0 .. 3] of GLfloat;  pVec4f = ^Vec4f;
	Mat4f = packed array[0 .. 15] of GLfloat; pMat4f = ^Mat4f;
	half = float16; pHalf = ^half;
	Vec2h = packed array[0 .. 1] of half; pVec2h = ^Vec2h;
	Vec3h = packed array[0 .. 2] of half; pVec3h = ^Vec3h;
	Vec4h = packed array[0 .. 3] of half; pVec4h = ^Vec4h;
	Vec2u8 = packed array[0 .. 1] of byte; pVec2u8 = ^Vec2u8;
	Vec3u8 = packed array[0 .. 2] of byte; pVec3u8 = ^Vec3u8;
	Vec4u8 = packed array[0 .. 3] of byte; pVec4u8 = ^Vec4u8;
	Vec2u16 = packed array[0 .. 1] of uint16; pVec2u16 = ^Vec2u16;
	Vec3u16 = packed array[0 .. 2] of uint16; pVec3u16 = ^Vec3u16;
	Vec4u16 = packed array[0 .. 3] of uint16; pVec4u16 = ^Vec4u16;
	Vec2s8 = packed array[0 .. 1] of sbyte; pVec2s8 = ^Vec2s8;
	Vec3s8 = packed array[0 .. 2] of sbyte; pVec3s8 = ^Vec3s8;
	Vec4s8 = packed array[0 .. 3] of sbyte; pVec4s8 = ^Vec4s8;
	Vec2s16 = packed array[0 .. 1] of sint16; pVec2s16 = ^Vec2s16;
	Vec3s16 = packed array[0 .. 2] of sint16; pVec3s16 = ^Vec3s16;
	Vec4s16 = packed array[0 .. 3] of sint16; pVec4s16 = ^Vec4s16;

scoped_enum_
	GLType =
	(
		Float, Vec2, Vec3, Vec4,
		Mat4,
		Uint32, Ubyte, Uint16,
		Sampler,
		Ni16, Vec2Ni16, Vec3Ni16, Vec4Ni16,
		Ni8, Vec2Ni8, Vec3Ni8, Vec4Ni8,
		Half, Vec2Half, Vec3Half, Vec4Half,
		Nui16, Vec2Nui16, Vec3Nui16, Vec4Nui16,
		Nui8, Vec2Nui8, Vec3Nui8, Vec4Nui8,
		Int32, Int16, Int8,
		Vec2ui8, Vec3ui8, Vec4ui8,
		Vec2ui16, Vec3ui16, Vec4ui16,
		Vec2i8, Vec3i8, Vec4i8,
		Vec2i16, Vec3i16, Vec4i16
	);
_end
	GLTypes = set of GLType;

type
	GLHandle = GL.uint;

	pGLImageFormat = ^GLImageFormat;
	GLImageFormat = (
		GLformat_R, GLformat_RG, GLformat_RGB, GLformat_RGBA,
		GLformat_RGB_DXT1, GLformat_RGBA_DXT1, GLformat_RGBA_DXT5,
		GLformat_R_RGTC1, GLformat_RG_RGTC2,
		GLformat_R16f, GLformat_RG16f, GLformat_RGB16f, GLformat_RGBA16f,
		GLformat_R32f, GLformat_RG32f, GLformat_RGB32f, GLformat_RGBA32f,
		GLformat_RGBA4, GLformat_RGB565, GLformat_RGB5, GLformat_RGB5A1, GLformat_A1RGB5,
		GLformat_BGR, GLformat_BGRA,
		GLformat_RGB332, GLformat_Depth);
	GLImageFormats = set of GLImageFormat;

	GLRenderBuffer = (GLbuffer_Color, GLbuffer_Depth);
	GLRenderBuffers = set of GLRenderBuffer;

	GLBufferTarget = (GLbuffer_Vertex, GLbuffer_Index, GLbuffer_Uniform);
	pGLBuffer = ^GLBuffer;
	GLBuffer = record
		target: GLBufferTarget;
		id: GLHandle;
	end;

	GLBlendMode =
	(
		GLblend_Off,
		GLblend_Mix,
		GLblend_MixZ,
		GLblend_Add,
		GLblend_AddWa,
		GLblend_MixAdd
	);

	GLTopology = (GLtopology_Points, GLtopology_Lines, GLtopology_LineStrip, GLtopology_Tris, GLtopology_TriStrip);
	GLPolygonMode = (GLpoly_Fill, GLpoly_Lines);
	GLFace = (GLface_Front, GLface_Back);
	GLFaces = set of GLFace;

	GLTextureTarget = (GLtexture_1D, GLtexture_2D, GLtexture_3D, GLtexture_Cube);
	GLTextureTargets = set of GLTextureTarget;
	GLCubeSide = (GLcube_XN, GLcube_XP, GLcube_YN, GLcube_YP, GLcube_ZN, GLcube_ZP);
	GLCubeSides = set of GLCubeSide;

	pGLShader = ^GLShader;
	GLShader = record
		id: GLHandle;
		compiled: boolean;
	end;
	GLShaderList = array of pGLShader;

	pGLTexture = ^tGLTexture;
	tGLTexture = record
		target: GLTextureTarget;
		id: GLHandle;
		fmt: GLImageFormat;
		nLevels: uint;
		bad: boolean;
	end;

	pGLProgram = ^GLProgram;
	GLProgram = record
		id: GLHandle;
		nSamplers: sint;
		samplers: array[0 .. MaxTextureUnits - 1] of record
			target: GLTextureTarget;
			id: GLHandle;
		end;
		nUbos: sint;
		ubos: array[0 .. MaxUniformBuffers - 1] of record
			id: GLHandle;
		end;
	end;

	pGLUniform = ^GLUniform;
	GLUniform = record
		loc: sint;
		count: uint;
		type_: GLType;
		texUnit: sint; // for samplers
	end;

	GLUniformDesc = record
		name: string;
		uniform: GLUniform;
	end;
	GLUniformsList = array of GLUniformDesc;

	pGLUboUniform = ^GLUboUniform;
	GLUboUniform = record
		count: uint;
		typ: GLType;
		bufOffset, arrStride, matStride: size_t;
		stridesArePacked: boolean;
	end;

	pGLUboUniformDesc = ^GLUboUniformDesc;
	GLUboUniformDesc = record
		name: string;
		u: GLUboUniform;
	end;

	GLUboDesc = record
		name: string;
		dataSize: size_t;
		uniforms: array of GLUboUniformDesc;
	end;
	GLUboList = array of GLUboDesc;

	GLVertexAttribDesc = record
		name: string;
		index: sint;
		type_: GLType;
	end;
	GLVertexAttribsList = array of GLVertexAttribDesc;

	pShaderEntrails = ^ShaderEntrails;
	ShaderEntrails = record
		u: GLUniformsList;
		ub: GLUboList;
		va: GLVertexAttribsList;
	end;

	pGLVertexDeclaration = ^GLVertexDeclaration;
	GLVertexDeclaration = record
		id: GLHandle;
		vb: pGLBuffer;
		topology: GLTopology;
	end;

	GLCircuit = (GLcircuit_CW, GLcircuit_CCW);

	GLBufferUsage = (GLusage_StaticDraw, GLusage_DynamicDraw);
	ShaderType = (GLshader_Vertex, GLshader_Geometry, GLshader_Fragment);

	GLTextureWrapMode = (GLwrap_Repeat, GLwrap_Clamp);
	GLTextureFilter = (GLfilter_Nearest, GLfilter_Linear);

	scoped_enum_ Swizzle = (R, G, B, A, Zero, One); _end
	SwizzlePack = array[0 .. 3] of Swizzle;

	GLTextureParam =
	(
		GLtexparam_Wrap,
		GLtexparam_MagFilter, GLtexparam_MinFilter,
		GLtexparam_Aniso, GLtexparam_Swizzle
	);
	GLTextureParamSet = set of GLTextureParam;

	GLTextureParamsRec = record
		wrap: GLTextureWrapMode;
		magFilter, minFilter: GLTextureFilter;
		anisotropy: float;
		swizzle: SwizzlePack;
	end;

	GLRenderTargetStatus = (GLrt_Complete, GLrt_Incomplete, GLrt_Unsupported);
	pGLRenderTarget = ^GLRenderTarget;
	GLRenderTarget = record
		id: GLHandle;
		size: UintVec2;
		colorRTs: set of 0 .. MaxMRTSlots - 1;
		bad: boolean;
	end;

	GLRasterizerParam =
	(
		GLrast_FrontFace,
		GLrast_Cull,
		GLrast_Blend,
		GLrast_Wire,
		GLrast_DepthTest,
		GLrast_DepthMask
	);
	GLRasterizerParams = set of GLRasterizerParam;
	pGLRasterizerState = ^GLRasterizerState;
	GLRasterizerState = record
		frontFace: GLCircuit;
		cull: boolean;
		blend: GLBlendMode;
		wire: GLFaces;
		depthTest: boolean;
		depthMask: boolean;
	end;

	GLMemoryUsage = (GLmem_Total, GLmem_GPU, GLmem_Aux);

	pGL = ^tGL;
	pGraveyardItem = ^GraveyardItem;
	GraveyardItem = object
	type
		TypeEnum = (DeadVD, DeadBuffer, DeadShader, DeadProgram, DeadTexture, DeadRT);

		function Make(&type: TypeEnum; obj: pointer): GraveyardItem; static;
		procedure Purge(gl: pGL);
	private
		&type: TypeEnum;
		obj: pointer;
	end;

	Command = object
	type
		scoped_enum_ Opcode =
		(
			CtxInit, CtxDone,
			MemStat,
			FrameStart, FrameEnd, Clear, Purge,
			BufInit, BufLoad, BufSub,
			VDInit, VDSetVA, Batch,
			ShInit, ShpInit, ShpDone, SendUni, BindUbos,
			TexInit, TexLoad, TexSub, TexOpt, TexQuIm,
			RTInit, RTSize, RTTexAt, RTTexDt, RTValid, RTQuIm,
			RastPush, RastPop, RastOpt,
			Terminate, _CommandCompleted
		); _end

		DynamicData = object
		type
			pStruct = ^Struct;
			Struct = record
				sizex: size_t;
				data: array[0 .. 0] of GenericAlignedType;
			end;
		const
			IN_PLACE  = size_t(1) shl (bitsizeof(size_t) - 1); // структура выделена по размеру данных и они хранятся непосредственно с data[0]
			OWN_PTR   = size_t(1) shl (bitsizeof(size_t) - 2); // data[0] должен быть освобождён
			SIZE_MASK = High(size_t) shr 2;
		end;

	type
		pBase = ^Base;
		Base = object
			op: Opcode;
		end;

		pCtxInit = ^CtxInit;
		CtxInit = object(Base)
			winSize: UintVec2;
			result: pSint;
		end;

		pCtxDone = ^CtxDone; CtxDone = object(Base) end;

		pMemStat = ^MemStat;
		MemStat = object(Base)
			what: GLMemoryUsage;
			result: pSize_t;
		end;

		pFrameStart = ^FrameStart; FrameStart = object(Base) end;

		pFrameEnd = ^FrameEnd;
		FrameEnd = object(Base)
			frameNo: sint;
		end;

		OnRenderTarget = object(Base)
			rt: pGLRenderTarget;
		end;

		pClear = ^Clear;
		Clear = object(OnRenderTarget)
			bufs: GLRenderBuffers;
			color: Vec4;
		end;

		pPurge = ^Purge;
		Purge = object(Base)
			grave: GraveyardItem;
		end;

		OnBuffer = object(Base)
			buf: pGLBuffer;
		end;

		pBufInit = ^BufInit; BufInit = object(OnBuffer) end;

		pBufLoad = ^BufLoad;
		BufLoad = object(OnBuffer)
			usage: GLBufferUsage;
			data: DynamicData.Struct;
		end;

		pBufSub = ^BufSub;
		BufSub = object(OnBuffer)
			offset: PtrUint;
			data: DynamicData.Struct;
		end;

		OnVertexDeclaration = object(Base)
			vd: pGLVertexDeclaration;
		end;

		pVDInit = ^VDInit;
		VDInit = object(OnVertexDeclaration)
			ib: pGLBuffer;
		end;

		pVDSetVA = ^VDSetVA;
		VDSetVA = object(OnVertexDeclaration)
			index: sint;
			&type: GLType;
			stride: size_t;
			vbOffset: PtrUint;
		end;

		pBatch = ^Batch;
		Batch = object(OnRenderTarget)
			vd: pGLVertexDeclaration;
			prog: pGLProgram;
			ibOffset: PtrUint;
			nIndices: uint;
			indexType: GLType;
			nInstances: uint;
		end;

		OnShader = object(Base)
			sh: pGLShader;
		end;

		pShInit = ^ShInit;
		ShInit = object(OnShader)
			&type: ShaderType;
			name, source: string;
		end;

		OnProgram = object(Base)
			prog: pGLProgram;
		end;

		pShpInit = ^ShpInit;
		ShpInit = object(OnProgram)
			namex: string;
			shaders: array of pGLShader;
			info: pShaderEntrails;
			result: pBoolean;
		end;

		pShpDone = ^ShpDone; ShpDone = object(OnProgram) end;

		pSendUni = ^SendUni;
		SendUni = object(OnProgram)
			u: pGLUniform;
			count: uint;
			data: DynamicData.Struct;
		end;

		pBindUbos = ^BindUbos;
		BindUbos = object(OnProgram)
			buffers: array of pGLBuffer;
		end;

		OnTexture = object(Base)
			tex: pGLTexture;
		end;

		pTexInit = ^TexInit;
		TexInit = object(OnTexture)
			target: GLTextureTarget;
		end;

		pTexLoad = ^TexLoad;
		TexLoad = object(OnTexture)
			size: UintVec3;
			format: GLImageFormat;
			level: uint;
			data: DynamicData.Struct;
		end;

		pTexSub = ^TexSub;
		TexSub = object(OnTexture)
			offset, size: UintVec3;
			format: GLImageFormat;
			level: uint;
			data: DynamicData.Struct;
		end;

		pTexOpt = ^TexOpt;
		TexOpt = object(OnTexture)
			params: GLTextureParamsRec;
			paramFields: GLTextureParamSet;
		end;

		pTexQuIm = ^TexQuIm;
		TexQuIm = object(OnTexture)
			result: pPointer;
			level: sint;
			format: GLImageFormat;
		end;

		pRTInit = ^RTInit; RTInit = object(OnRenderTarget) end;

		pRTSize = ^RTSize;
		RTSize = object(OnRenderTarget)
			size: UintVec2;
		end;

		pRTTexAt = ^RTTexAt;
		RTTexAt = object(OnRenderTarget)
			tex: pGLTexture;
			level: uint;
			target: GLRenderBuffer;
			targetN: uint;
		end;

		pRTTexDt = ^RTTexDt;
		RTTexDt = object(OnRenderTarget)
			target: GLRenderBuffer;
			targetN: uint;
		end;

		pRTValid = ^RTValid;
		RTValid = object(OnRenderTarget)
			result: ^GLRenderTargetStatus;
		end;

		pRTQuIm = ^RTQuIm;
		RTQuIm = object(OnRenderTarget)
			format: GLImageFormat;
			mrtN: uint;
			result: pPointer;
		end;

		pRastPush = ^RastPush; RastPush = object(Base) end;
		pRastPop  = ^RastPop;  RastPop  = object(Base) end;

		pRastOpt = ^RastOpt;
		RastOpt = object(Base)
			state: GLRasterizerState;
			fields: GLRasterizerParams;
		end;

		pTerminate = ^Terminate; Terminate = object(Base) end;
	end;
	Opcode = Command.Opcode;

	tGL = object
	private
		_ctxInited: boolean;
	{$ifdef Debug} procedure _LogAbout(instancingSupported: boolean); {$endif}
	protected
		_maxUniforms, _maxInstances: uint;
		_UBOSupported, _primitiveRestartSupported: boolean;
		_geometryShaderSupported, _csmRecommended, _advFloatsRecommended, _ubyteIdsRecommended, _wiresMustMatch, _uniForSupported, _binaryShadersSupported,
			_inoutSupported, _overloadedTextureFetch, _shouldSetPrecisionExplicitly, _intelVendor: boolean;
		_vaAlignment: uint;
		_slVersion: string;
		_rastStack: array[0 .. 3] of GLRasterizerState;
		_rastStackTop: sint;
		_rast: GLRasterizerState;
		_notifyErrors: boolean;
	{$ifdef Debug}
		procedure _LogSupUnsup(supported: boolean; const name, msgOk, msgFail: string); static;
		procedure _LogSupUnsup(enabled, supported: boolean; const name, msgOk, msgFail, msgDisabled, msgDisabledAndFail: string); static;
	{$endif}
		procedure _Error(const msg: string);
	public
		ScreenRT: GLRenderTarget;
	{$ifdef Debug}
		drawCallsDynamics, stateChangesDynamics, trisDynamics, sendedDynamics: ValueDynamics;
	{$endif}
		constructor Init;
		destructor Done; virtual;

		function InitContext(const winSize: UintVec2): boolean;
		procedure DoneContext;
		function MemoryUsed(what: GLMemoryUsage = GLmem_Total): size_t;

		procedure BeginFrame;
		procedure EndFrame(frameNo: sint);
		procedure Clear(rt: pGLRenderTarget; bufs: GLRenderBuffers; const clearColor: Vec4);

		procedure DrawBatch(rt: pGLRenderTarget; vd: pGLVertexDeclaration; prog: pGLProgram; ibOffset: PtrUint; nIndices: uint; indexType: GLType; nInstances: uint = 1);

		function CreateVertexDeclaration(vb, ib: pGLBuffer; topology: GLTopology): pGLVertexDeclaration;
		procedure DeleteVertexDeclaration(var vd: pGLVertexDeclaration);
		procedure SetVertexAttribute(vd: pGLVertexDeclaration; index: sint; type_: GLType; stride: size_t; vbOffset: PtrUint);

		function CreateBuffer(target: GLBufferTarget): pGLBuffer;
		procedure DeleteBuffer(var buf: pGLBuffer);
		procedure BufferData(buf: pGLBuffer; size: size_t; data: pointer; usage: GLBufferUsage = GLusage_StaticDraw);
		procedure BufferSubData(buf: pGLBuffer; offset: PtrUint; size: size_t; data: pointer; takeThisData: boolean = no);

		function CreateShader(type_: ShaderType; const name, source: string): pGLShader;
		procedure DeleteShader(var sh: pGLShader);

		function CreateProgram(const namex: string; const sh: array of pGLShader; out info: ShaderEntrails): pGLProgram;
		procedure DeleteProgram(var prog: pGLProgram);
		procedure SetUniform(prog: pGLProgram; var u: GLUniform; value: pointer; count: uint = High(uint));
		procedure SetUbos(prog: pGLProgram; const bufs: array of pGLBuffer);
		procedure PlaceUboData(src: pointer; var u: GLUboUniform; dst: pointer; count: uint = High(uint)); static;

		function CreateTexture(target: GLTextureTarget): pGLTexture;
		procedure DeleteTexture(var tex: pGLTexture);
		procedure TexImage(tex: pGLTexture; const size: UintSize3; format: GLImageFormat; dataSize: size_t; data: pointer; level: uint = 0; takeThisData: boolean = no);
		procedure TexSubImage(tex: pGLTexture; const offset: UintOffset3; const size: UintSize3; format: GLImageFormat; dataSize: size_t; data: pointer; level: uint = 0; takeThisData: boolean = no);
		procedure SetTextureParams(tex: pGLTexture; const params: GLTextureParamsRec; paramFields: GLTextureParamSet);
		function GetTexImage(tex: pGLTexture; level: uint; format: GLImageFormat): pointer;

		function CreateRenderTarget: pGLRenderTarget;
		procedure DeleteRenderTarget(var fb: pGLRenderTarget);
		procedure ResizeRenderTarget(fb: pGLRenderTarget; const size: UintVec2);
		procedure AttachRenderTexture(fb: pGLRenderTarget; tex: pGLTexture; level: uint; target: GLRenderBuffer; targetN: uint = 0);
		procedure DetachRenderTexture(fb: pGLRenderTarget; target: GLRenderBuffer; targetN: uint = 0);
		function ValidateRenderTarget(fb: pGLRenderTarget): GLRenderTargetStatus;
		function GetRTImage(fb: pGLRenderTarget; format: GLImageFormat; mrtN: uint = 0): pointer;

		procedure SetRasterizerState(var state: GLRasterizerState; fields: GLRasterizerParams);
		function RasterizerState: GLRasterizerState; // TODO: костыль. Используется в Script_GL_Wire.
		procedure PushRasterizerState;
		procedure PopRasterizerState;

		property MaxInstances: uint read _maxInstances;
		property MaxUniforms: uint read _maxUniforms;
		property UBOSupported: boolean read _UBOSupported;
		property PrimitiveRestartSupported: boolean read _primitiveRestartSupported;
		property GeometryShaderSupported: boolean read _geometryShaderSupported;

		property CSMRecommended: boolean read _csmRecommended;
		property WiresMustMatch: boolean read _wiresMustMatch;
		property UniForSupported: boolean read _UniForSupported;
		property BinaryShadersSupported: boolean read _binaryShadersSupported;
		property UseAdvFloats: boolean read _advFloatsRecommended;
		property UseUbyteIds: boolean read _ubyteIdsRecommended;
		property VaAlignment: uint read _vaAlignment;
		property SLVersion: string read _slVersion;
		property InOutSupported: boolean read _inoutSupported;
		property OverloadedTextureFetch: boolean read _overloadedTextureFetch;
		property ShouldSetPrecisionExplicitly: boolean read _shouldSetPrecisionExplicitly;
		property IntelVendor: boolean read _intelVendor;
	protected
	{$ifdef Debug}
		_curDrawCalls, _curStateChanges, _curTris, _curSended, allocatedInPlace, allocatedDynamically: ulong;
		maxGraveyardSize: sint;
	{$endif}
	protected
		function _InitContext(const winSize: UintVec2): sint;
		procedure _DoneContext;
		procedure _BeginFrame;
		procedure _EndFrame(frameNo: sint);

		function _InitGL: boolean; pure_virtual;
		procedure _DoneGL; pure_virtual;
		procedure _OnEndFrame; pure_virtual;
		function _MemoryUsed(what: GLMemoryUsage): size_t; pure_virtual;

		procedure _Clear(var rt: GLRenderTarget; bufs: GLRenderBuffers; const clearColor: Vec4); pure_virtual;
		procedure _DrawBatch(var rt: GLRenderTarget; var vd: GLVertexDeclaration; var prog: GLProgram; ibOffset: PtrUint; nIndices: uint; indexType: GLType; nInstances: uint); pure_virtual;

		procedure _CreateVertexDeclaration(var vd: GLVertexDeclaration; var ib: GLBuffer); pure_virtual;
		procedure _DeleteVertexDeclaration(var vd: GLVertexDeclaration); pure_virtual;
		procedure _SetVertexAttribute(var vd: GLVertexDeclaration; index: sint; type_: GLType; stride: size_t; vbOffset: PtrUint); pure_virtual;

		procedure _CreateBuffer(var buf: GLBuffer); pure_virtual;
		procedure _DeleteBuffer(var buf: GLBuffer); pure_virtual;
		procedure _BufferData(var buf: GLBuffer; size: size_t; data: pointer; usage: GLBufferUsage); pure_virtual;
		procedure _BufferSubData(var buf: GLBuffer; offset: PtrUint; size: size_t; data: pointer); pure_virtual;

		procedure _CreateShader(var sh: GLShader; type_: ShaderType; const name, source: string); pure_virtual;
		procedure _DeleteShader(var sh: GLShader); pure_virtual;

		function _CreateProgram(var prog: GLProgram; const namex: string; const sh: array of pGLShader; out info: ShaderEntrails): boolean; pure_virtual;
		procedure _DeleteProgram(var prog: GLProgram); pure_virtual;
		procedure _SetUniform(var prog: GLProgram; var u: GLUniform; value: pointer; count: uint); pure_virtual;
		procedure _SetUbos(var prog: GLProgram; const bufs: array of pGLBuffer); pure_virtual;

		procedure _CreateTexture(out tex: tGLTexture; target: GLTextureTarget); pure_virtual;
		procedure _DeleteTexture(var tex: tGLTexture); pure_virtual;
		procedure _TexImage(var tex: tGLTexture; const size: UintVec3; format: GLImageFormat; dataSize: size_t; data: pointer; level: uint); pure_virtual;
		procedure _TexSubImage(var tex: tGLTexture; const offset, size: UintVec3; format: GLImageFormat; dataSize: size_t; data: pointer; level: uint); pure_virtual;
		procedure _SetTextureParams(var tex: tGLTexture; const params: GLTextureParamsRec; paramFields: GLTextureParamSet); pure_virtual;
		function _GetTexImage(var tex: tGLTexture; level: uint; format: GLImageFormat): pointer; pure_virtual;

		procedure _CreateRenderTarget(var fb: GLRenderTarget); pure_virtual;
		procedure _DeleteRenderTarget(var fb: GLRenderTarget); pure_virtual;
		procedure _ResizeRenderTarget(var fb: GLRenderTarget; const size: UintVec2); pure_virtual;
		procedure _AttachRenderTexture(var fb: GLRenderTarget; var tex: tGLTexture; level: uint; target: GLRenderBuffer; targetN: uint); pure_virtual;
		procedure _DetachRenderTexture(var fb: GLRenderTarget; target: GLRenderBuffer; targetN: uint); pure_virtual;
		function _ValidateRenderTarget(var fb: GLRenderTarget): GLRenderTargetStatus; pure_virtual;
		function _GetRTImage(var fb: GLRenderTarget; format: GLImageFormat; mrtN: uint): pointer; pure_virtual;

		procedure _PushRasterizerState;
		procedure _PopRasterizerState;
		procedure _SetRasterizerState(var state: GLRasterizerState; fields: GLRasterizerParams);

	private const
		StoreInPlaceUpTo = size_t(256 * sizeof(pointer));
	private var
		cmds: ThreadedHeterogenousQueue;
		immediateLock, graveyardLock: ThreadLock;
		cmdThread: Thread;
		cmdFinished: ThreadCV;
		graveyard: array of GraveyardItem;

		function LockPut(op: Opcode; size: size_t): Command.pBase;
		function LockPutDynData(op: Opcode; size: size_t; data: pointer; dataSize: size_t; ownDataPtr: boolean): Command.pBase;
		procedure UnlockPut;
		procedure UnlockPutImmediate(cmd: Command.pBase);
		procedure ExecuteNext;
		procedure KillCmdThread;
		procedure AddToGraveyard(&type: GraveyardItem.TypeEnum; obj: pointer);
		procedure CleanupGraveyard;
		procedure GenericPurge(&type: GraveyardItem.TypeEnum; var obj: pointer);
	end;

const
	GLIntLims: array[GLType.Uint32 .. GLType.Uint16] of uint =
	(
		High(uint32) div 2, High(byte), (2 * High(uint16)) div 3
	);

	GLImageFormatIds: array[GLImageFormat] of string =
	(
		'r', 'rg', 'rgb', 'rgba',
		'rgb_dxt', 'rgba_dxt1', 'rgba_dxt5', 'r_rgtc', 'rg_rgtc',
		'r16f', 'rg16f', 'rgb16f', 'rgba16f',
		'r32f', 'rg32f', 'rgb32f', 'rgba32f',
		'rgba4', 'rgb565', 'rgb5', 'rgb5a1', 'a1rgb5', 'bgr', 'bgra',
		'rgb332', 'depth'
	);

	GLTextureTargetIds: array[GLTextureTarget] of string = ('1d', '2d', '3d', 'cube');
	GLWrapModeIds: array[GLTextureWrapMode] of string = ('repeat', 'clamp');
	GLTopologyIds: array[GLTopology] of string = ('point', 'line', 'linestrip', 'tris', 'tristrip');
	GLBlendModeIds: array[GLBlendMode] of string = ('off', 'mix', 'mixZ', 'add', 'addwa', 'mixadd');
	NoSwizzle: SwizzlePack = (Swizzle.R, Swizzle.G, Swizzle.B, Swizzle.A);

type
	scoped_enum_ GLTypeFlag = (Matrix, AdvFloat, Int); _end
	GLTypeFlags = set of GLTypeFlag;

const
	GLTypeInfo: array[GLType] of
		record
			sizeof: size_t;
			baseDim: sint;
			baseType: GLType;
			flags: GLTypeFlags;
		end =
	(
		(sizeof: sizeof(GLfloat); baseDim: 1; baseType: GLType.Float;  flags: []), // GLType.Float
		(sizeof: sizeof(Vec2f); baseDim: 2; baseType: GLType.Float;  flags: []), // GLType.Vec2
		(sizeof: sizeof(Vec3f); baseDim: 3; baseType: GLType.Float;  flags: []), // GLType.Vec3
		(sizeof: sizeof(Vec4f); baseDim: 4; baseType: GLType.Float;  flags: []), // GLType.Vec4
		(sizeof: sizeof(Mat4f); baseDim: 4; baseType: GLType.Vec4;  flags: [GLTypeFlag.Matrix]), // GLType.Mat4
		(sizeof: sizeof(uint32); baseDim: 1; baseType: GLType.Uint32; flags: [GLTypeFlag.Int]), // GLType.Uint32
		(sizeof: sizeof(byte);   baseDim: 1; baseType: GLType.Ubyte;  flags: [GLTypeFlag.Int]), // GLType.Ubyte
		(sizeof: sizeof(uint16); baseDim: 1; baseType: GLType.Uint16; flags: [GLTypeFlag.Int]), // GLType.Uint16
		(sizeof: 0; baseDim: 0;  baseType: GLType.Sampler; flags: []), // GLType.Sampler
		(sizeof: sizeof(sint16);  baseDim: 1; baseType: GLType.Ni16; flags: [GLTypeFlag.AdvFloat]), // GLType.Ni16
		(sizeof: sizeof(Vec2s16); baseDim: 2; baseType: GLType.Ni16; flags: [GLTypeFlag.AdvFloat]), // GLType.Vec2Ni16
		(sizeof: sizeof(Vec3s16); baseDim: 3; baseType: GLType.Ni16; flags: [GLTypeFlag.AdvFloat]), // GLType.Vec3Ni16
		(sizeof: sizeof(Vec4s16); baseDim: 4; baseType: GLType.Ni16; flags: [GLTypeFlag.AdvFloat]), // GLType.Vec4Ni16
		(sizeof: sizeof(sbyte);   baseDim: 1; baseType: GLType.Ni8; flags: [GLTypeFlag.AdvFloat]), // GLType.Ni8
		(sizeof: sizeof(Vec2s8);  baseDim: 2; baseType: GLType.Ni8; flags: [GLTypeFlag.AdvFloat]), // GLType.Vec2Ni8
		(sizeof: sizeof(Vec3s8);  baseDim: 3; baseType: GLType.Ni8; flags: [GLTypeFlag.AdvFloat]), // GLType.Vec3Ni8
		(sizeof: sizeof(Vec4s8);  baseDim: 4; baseType: GLType.Ni8; flags: [GLTypeFlag.AdvFloat]), // GLType.Vec4Ni8
		(sizeof: sizeof(half);  baseDim: 1; baseType: GLType.Half; flags: [GLTypeFlag.AdvFloat]), // GLType.Half
		(sizeof: sizeof(Vec2h); baseDim: 2; baseType: GLType.Half; flags: [GLTypeFlag.AdvFloat]), // GLType.Vec2Half
		(sizeof: sizeof(Vec3h); baseDim: 3; baseType: GLType.Half; flags: [GLTypeFlag.AdvFloat]), // GLType.Vec3Half
		(sizeof: sizeof(Vec4h); baseDim: 4; baseType: GLType.Half; flags: [GLTypeFlag.AdvFloat]), // GLType.Vec4Half
		(sizeof: sizeof(uint16);  baseDim: 1; baseType: GLType.Nui16; flags: [GLTypeFlag.AdvFloat]), // GLType.Nui16
		(sizeof: sizeof(Vec2u16); baseDim: 2; baseType: GLType.Nui16; flags: [GLTypeFlag.AdvFloat]), // GLType.Vec2Nui16
		(sizeof: sizeof(Vec3u16); baseDim: 3; baseType: GLType.Nui16; flags: [GLTypeFlag.AdvFloat]), // GLType.Vec3Nui16
		(sizeof: sizeof(Vec4u16); baseDim: 4; baseType: GLType.Nui16; flags: [GLTypeFlag.AdvFloat]), // GLType.Vec4Nui16
		(sizeof: sizeof(byte);   baseDim: 1; baseType: GLType.Nui8; flags: [GLTypeFlag.AdvFloat]), // GLType.Nui8
		(sizeof: sizeof(Vec2u8); baseDim: 2; baseType: GLType.Nui8; flags: [GLTypeFlag.AdvFloat]), // GLType.Vec2Nui8
		(sizeof: sizeof(Vec3u8); baseDim: 3; baseType: GLType.Nui8; flags: [GLTypeFlag.AdvFloat]), // GLType.Vec3Nui8
		(sizeof: sizeof(Vec4u8); baseDim: 4; baseType: GLType.Nui8; flags: [GLTypeFlag.AdvFloat]), // GLType.Vec4Nui8
		(sizeof: sizeof(sint32); baseDim: 1; baseType: GLType.Int32; flags: [GLTypeFlag.Int]), // GLType.Int32
		(sizeof: sizeof(sint16); baseDim: 1; baseType: GLType.Int16; flags: [GLTypeFlag.Int]), // GLType.Int16
		(sizeof: sizeof(sint8);  baseDim: 1; baseType: GLType.Int8; flags: [GLTypeFlag.Int]),  // GLType.Int8
		(sizeof: sizeof(Vec2u8);  baseDim: 2; baseType: GLType.Ubyte; flags: []), // GLType.Vec2ui8
		(sizeof: sizeof(Vec3u8);  baseDim: 3; baseType: GLType.Ubyte; flags: []), // GLType.Vec3ui8
		(sizeof: sizeof(Vec4u8);  baseDim: 4; baseType: GLType.Ubyte; flags: []), // GLType.Vec4ui8
		(sizeof: sizeof(Vec2u16); baseDim: 2; baseType: GLType.Uint16; flags: []), // GLType.Vec2ui16
		(sizeof: sizeof(Vec3u16); baseDim: 3; baseType: GLType.Uint16; flags: []), // GLType.Vec3ui16
		(sizeof: sizeof(Vec4u16); baseDim: 4; baseType: GLType.Uint16; flags: []), // GLType.Vec4ui16
		(sizeof: sizeof(Vec2s8);  baseDim: 2; baseType: GLType.Int8; flags: []), // GLType.Vec2i8
		(sizeof: sizeof(Vec3s8);  baseDim: 3; baseType: GLType.Int8; flags: []), // GLType.Vec3i8
		(sizeof: sizeof(Vec4s8);  baseDim: 4; baseType: GLType.Int8; flags: []), // GLType.Vec4i8
		(sizeof: sizeof(Vec2s16); baseDim: 2; baseType: GLType.Int16; flags: []), // GLType.Vec2i16
		(sizeof: sizeof(Vec3s16); baseDim: 3; baseType: GLType.Int16; flags: []), // GLType.Vec3i16
		(sizeof: sizeof(Vec4s16); baseDim: 4; baseType: GLType.Int16; flags: [])  // GLType.Vec4i16
	);
	GLTypeIds: array[GLType] of string = (
		'f', 'v2f', 'v3f', 'v4f', 'm4f', 'ui', 'ub', 'us', 'tex', 'ni16', 'v2ni16', 'v3ni16', 'v4ni16',
		'ni8', 'v2ni8', 'v3ni8', 'v4ni8', 'h', 'v2h', 'v3h', 'v4h', 'nu16', 'v2nu16', 'v3nu16', 'v4nu16',
		'nu8', 'v2nu8', 'v3nu8', 'v4nu8',
		'i32', 'i16', 'i8',
		'v2ui8', 'v3ui8', 'v4ui8', 'v2ui16', 'v3ui16', 'v4ui16', 'v2i8', 'v3i8', 'v4i8', 'v2i16', 'v3i16', 'v4i16');
	GLVec: array[1 .. 4] of GLType = (GLType.Float, GLType.Vec2, GLType.Vec3, GLType.Vec4);
	GLVecHalf: array[1 .. 4] of GLType = (GLType.Half, GLType.Vec2Half, GLType.Vec3Half, GLType.Vec4Half);
	GLVecNi8: array[1 .. 4] of GLType = (GLType.Ni8, GLType.Vec2Ni8, GLType.Vec3Ni8, GLType.Vec4Ni8);
	GLVecNi16: array[1 .. 4] of GLType = (GLType.Ni16, GLType.Vec2Ni16, GLType.Vec3Ni16, GLType.Vec4Ni16);
	GLVecNui8: array[1 .. 4] of GLType = (GLType.Nui8, GLType.Vec2Nui8, GLType.Vec3Nui8, GLType.Vec4Nui8);
	GLVecNui16: array[1 .. 4] of GLType = (GLType.Nui16, GLType.Vec2Nui16, GLType.Vec3Nui16, GLType.Vec4Nui16);

	ShaderTypeAbbrevs: array[ShaderType] of string = ('v', 'g', 'f');
	PolyModeIds: array[GLPolygonMode] of string = ('fill', 'line');

type
	GLImageFormatFlag = (GLformat_Compressed);
	GLImageFormatFlags = set of GLImageFormatFlag;

const
	MaxColorChannels = 4;
	GLImageFormatsInfo: array[GLImageFormat] of
		record
			pixelSize: size_t;
			nChannels: uint;
			flags: GLImageFormatFlags;
		end =
	(
		(pixelSize: 1 * SizeOf(byte); nChannels: 1; flags: []), // GLformat_R
		(pixelSize: 2 * SizeOf(byte); nChannels: 2; flags: []), // GLformat_RG
		(pixelSize: 3 * SizeOf(byte); nChannels: 3; flags: []), // GLformat_RGB
		(pixelSize: 4 * SizeOf(byte); nChannels: 4; flags: []), // GLformat_RGBA
		(pixelSize: 0; nChannels: 3; flags: [GLformat_Compressed]), // GLformat_RGB_DXT1
		(pixelSize: 0; nChannels: 4; flags: [GLformat_Compressed]), // GLformat_RGBA_DXT1
		(pixelSize: 0; nChannels: 4; flags: [GLformat_Compressed]), // GLformat_RGBA_DXT5
		(pixelSize: 0; nChannels: 1; flags: [GLformat_Compressed]), // GLformat_R_RGTC1
		(pixelSize: 0; nChannels: 2; flags: [GLformat_Compressed]), // GLformat_RG_RGTC2
		(pixelSize: 1 * SizeOf(float16); nChannels: 1; flags: []), // GLformat_R16F
		(pixelSize: 2 * SizeOf(float16); nChannels: 2; flags: []), // GLformat_RG16F
		(pixelSize: 3 * SizeOf(float16); nChannels: 3; flags: []), // GLformat_RGB16F
		(pixelSize: 4 * SizeOf(float16); nChannels: 4; flags: []), // GLformat_RGBA16F
		(pixelSize: 1 * SizeOf(float32); nChannels: 1; flags: []), // GLformat_R32F
		(pixelSize: 2 * SizeOf(float32); nChannels: 2; flags: []), // GLformat_RG32F
		(pixelSize: 3 * SizeOf(float32); nChannels: 3; flags: []), // GLformat_RGB32F
		(pixelSize: 4 * SizeOf(float32); nChannels: 4; flags: []), // GLformat_RGBA32F
		(pixelSize: 2; nChannels: 4; flags: []), // GLformat_RGBA4
		(pixelSize: 2; nChannels: 3; flags: []), // GLformat_RGB565
		(pixelSize: 2; nChannels: 3; flags: []), // GLformat_RGB5
		(pixelSize: 2; nChannels: 4; flags: []), // GLformat_RGB5A1
		(pixelSize: 2; nChannels: 4; flags: []), // GLformat_A1RGB5
		(pixelSize: 3 * SizeOf(byte); nChannels: 3; flags: []), // GLformat_BGR
		(pixelSize: 4 * SizeOf(byte); nChannels: 4; flags: []), // GLformat_BGRA
		(pixelSize: SizeOf(byte); nChannels: 3; flags: []),  // GLformat_RGB332
		(pixelSize: 0; nChannels: 1; flags: []) // GLformat_Depth
	);

	ImageFormat8: array[1 .. 4] of GLImageFormat = (GLformat_R, GLformat_RG, GLformat_RGB, GLformat_RGBA);

	// determinitiveDims — количество размерностей, достаточное для описания текстуры, т. о. для кубической карты determinitiveDims = 1.
	TextureTargetsInfo: array[GLTextureTarget] of record
		determinativeDims, faces: uint;
	end =
	(
		(determinativeDims: 1; faces: 1), // 1d
		(determinativeDims: 2; faces: 1), // 2d
		(determinativeDims: 3; faces: 1), // 3d
		(determinativeDims: 1; faces: 6) // cube
	);

	CubeSideIds: array[GLCubeSide] of string = ('X-', 'X+', 'Y-', 'Y+', 'Z-', 'Z+');
	SwizzleIds: array[Swizzle] of string = ('r', 'g', 'b', 'a', '0', '1');
	DefaultTextureSample: Vec4 = (data: (0.0, 0.0, 0.0, 1.0));

type
	BinaryShader = object
	const
		Uid          = '|uid=';
		SuppressLoad = '|nobin';
		Options: array[0 .. 1] of string = (Uid, SuppressLoad);
	end;

	function FromGL(const v: Vec2f): Vec2;
	function FromGL(const v: Vec3f): Vec3;
	function FromGL(const v: Vec4f): Vec4;

	operator :=(const v: Vec2): Vec2f;
	operator :=(const v: Vec3): Vec3f;
	operator :=(const v: Vec4): Vec4f;
	operator :=(const v: Matrix4): Mat4f;

	function DenormI16(x: GLfloat): sint16; function NormI16(x: sint16): GLfloat;
	function DenormI8(x: GLfloat): sint8;   function NormI8(x: sint8): GLfloat;
	function DenormU16(x: GLfloat): uint16; function NormU16(x: uint16): GLfloat;
	function DenormU8(x: GLfloat): uint8;   function NormU8(x: uint8): GLfloat;
	procedure Convert(inp: pointer; in_type: GLType; outp: pointer; out_type: GLType; count: sint = 1);
	function GetTextureDataSize(const size: UintVec2; format: GLImageFormat): size_t;
	function GetTextureDataSize(const size: UintVec3; format: GLImageFormat): size_t;
	function GetTextureDataSize(w, h: uint; format: GLImageFormat): size_t;
	function GetTextureDataSize(w, h, d: uint; format: GLImageFormat): size_t;
	function UniformComponents(ty: GLType): sint;
	function RGB332(r, g, b: uint): uint8; cinline
	function RGB8(r, g, b: uint): Vec3u8;
	procedure Convert(inp: pointer; inFormat: GLImageFormat; outp: pointer; outFormat: GLImageFormat; const size: UintVec3);
	procedure Convert(inp: pointer; inFormat: GLImageFormat; outp: pointer; outFormat: GLImageFormat; const size: UintVec2);
	procedure Convert(inp: pointer; inFormat: GLImageFormat; outp: pointer; outFormat: GLImageFormat; w, h, d: uint);
	procedure FlipY(src, dst: pointer; format: GLImageFormat; const size: UintSize3);

implementation

uses
	MMSystem, GLBase, GLClasses, DXT;

	operator :=(const v: UintVec2): UintSize3; begin result.x := v.x; result.y := v.y; result.z := 1; end;
	operator :=(const v: UintVec3): UintSize3; begin result.data := UintSize3.LinearData(v.data); end;
	operator :=(const v: UintVec2): UintOffset3; begin result.x := v.x; result.y := v.y; result.z := 0; end;
	operator :=(const v: UintVec3): UintOffset3; begin result.data := UintOffset3.LinearData(v.data); end;

const
	DefaultRenderTarget: GLRenderTarget =
	(
		id: 0;
		size: (data: (0, 0));
		colorRTs: [];
		bad: no
	);

	function GraveyardItem.Make(&type: TypeEnum; obj: pointer): GraveyardItem;
	begin
		result.&type := &type;
		result.obj   := obj;
	end;

	procedure GraveyardItem.Purge(gl: pGL);
	var
		vd: pGLVertexDeclaration absolute obj;
		buf: pGLBuffer absolute obj;
		sh: pGLShader absolute obj;
		prog: pGLProgram absolute obj;
		tex: pGLTexture absolute obj;
		rt: pGLRenderTarget absolute obj;
	begin
		case &type of
			DeadVD:      begin gl^._DeleteVertexDeclaration(vd^); dispose(vd);  end;
			DeadBuffer:  begin gl^._DeleteBuffer(buf^);           dispose(buf); end;
			DeadShader:  begin gl^._DeleteShader(sh^);            dispose(sh);  end;
			DeadProgram: begin gl^._DeleteProgram(prog^);         dispose(prog); end;
			DeadTexture: begin gl^._DeleteTexture(tex^);          dispose(tex); end;
			DeadRT:      begin gl^._DeleteRenderTarget(rt^);      dispose(rt);  end;
		{$ifdef Debug} else raise ExhaustiveCase(ord(&type), 'GraveyardItem.type'); {$endif}
		end;
	end;

	procedure GLThread(param: pointer);
	begin
		try
			repeat
				pGL(param)^.ExecuteNext;
			until no;
		except
			on TerminateThread do begin {$ifdef Debug} Log('Поток рендера завершён', logOK); {$endif} raise; end;
		end;
	end;

	constructor tGL.Init;
	const
		DefaultRasterizer: GLRasterizerState =
		(
			frontFace: GLcircuit_CW;
			cull: yes;
			blend: GLblend_Off;
			wire: [];
			depthTest: yes;
			depthMask: no;
		);
	begin
		immediateLock.Init;
		graveyardLock.Init;
		cmds.Init;
		cmdThread := Thread.Invalid;
		cmdFinished.Init;

		_ctxInited := no;
	{$ifdef Debug}
		drawCallsDynamics.Init;
		stateChangesDynamics.Init;
		trisDynamics.Init;
		sendedDynamics.Init;
		_curDrawCalls := 0;
		_curStateChanges := 0;
		_curTris := 0;
		_curSended := 0;
		allocatedInPlace     := 0;
		allocatedDynamically := 0;
		maxGraveyardSize := 0;
	{$endif}

		_rast := DefaultRasterizer;
		_rastStackTop := 0;
		ScreenRT := DefaultRenderTarget;

		_maxUniforms := MinUniforms;
		_maxInstances := 1;
		_UBOSupported := no;
		_primitiveRestartSupported := no;
		_geometryShaderSupported := no;

		_csmRecommended := no;
		_advFloatsRecommended := no;
		_ubyteIdsRecommended := no;
		_vaAlignment := 1;
		_wiresMustMatch := no;
		_uniForSupported := no;
		_binaryShadersSupported := no;
		_inoutSupported := no;
		_overloadedTextureFetch := no;
		_shouldSetPrecisionExplicitly := no;
		_intelVendor := no;
		_slVersion := '100';
		_notifyErrors := yes;
	end;

{$ifdef Debug}
	function DescribeSummaryAllocated(id: uint; param: pointer): string;
	begin
		case id of
			0: if pGL(param)^.allocatedInPlace > 0 then result := 'в очереди: ' + ToStringSuff_b(pGL(param)^.allocatedInPlace);
			else if pGL(param)^.allocatedDynamically > 0 then result := 'в куче: ' + ToStringSuff_b(pGL(param)^.allocatedDynamically);
		end;
	end;
{$endif}

	destructor tGL.Done;
	begin
		DoneContext;
		KillCmdThread;

	{$ifdef Debug}
		sendedDynamics.Done;
		trisDynamics.Done;
		stateChangesDynamics.Done;
		drawCallsDynamics.Done;
	{$endif}

	{$ifdef Debug} if _rastStackTop <> 0 then Log('tGL.Done: стек рендерстейтов не сбалансирован', logError); {$endif}
		cmdFinished.Done;
	{$ifdef Debug}
		if cmds.stats.allocations > 0 then
			Log('Аллокаций в очереди GL-команд: {0}, макс. блоков: {1}, макс. элементов: {2}, макс. длина: {3}, макс. выделено: {4}.',
			    ToString(cmds.stats.allocations), ToString(cmds.stats.maxBlocks), ToString(cmds.stats.maxItems),
				 ToStringSuff_b(cmds.stats.maxSumSize), ToStringSuff_b(cmds.stats.maxSumAlloc), logDebug);
		if (allocatedInPlace > 0) or (allocatedDynamically > 0) then
			Log('Суммарно выделено буферов {0}.', SeparatedList.Join(2, @DescribeSummaryAllocated, @self, ', '), logDebug);
		if maxGraveyardSize > 0 then Log('Максимальный размер GL-«кладбища»: {0}.', [maxGraveyardSize], logDebug);
	{$endif}
		cmds.Done;
		graveyardLock.Done;
		immediateLock.Done;

		_slVersion := '';
	end;

	procedure tGL.KillCmdThread;
	begin
		if cmdThread.OK then
		begin
		{$ifdef Debug} LogR('Остановка рендера... '); {$endif}
			LockPut(Opcode.Terminate, sizeof(Command.Terminate));
			UnlockPut;
			cmdThread.Close;
		end;
	end;

	procedure tGL.AddToGraveyard(&type: GraveyardItem.TypeEnum; obj: pointer);
	begin
		graveyardLock.Enter;
		SetLength(graveyard, length(graveyard) + 1);
		graveyard[High(graveyard)] := GraveyardItem.Make(&type, obj);
		graveyardLock.Leave;
	end;

	procedure tGL.CleanupGraveyard;
	var
		t: array of GraveyardItem;
		i: sint;
	begin
		repeat
			graveyardLock.Enter;
			if length(graveyard) = 0 then break;
			t := graveyard;
			graveyard := nil;
		{$ifdef Debug} maxGraveyardSize := max(maxGraveyardSize, length(t)); {$endif}
			graveyardLock.Leave;

			for i := 0 to High(t) do t[i].Purge(@self);
		until no;
		graveyardLock.Leave;
	end;

	procedure tGL.GenericPurge(&type: GraveyardItem.TypeEnum; var obj: pointer);
	begin
		if Config.allowMT then
		begin
			Command.pPurge(LockPut(Opcode.Purge, sizeof(Command.Purge)))^.grave := GraveyardItem.Make(&type, obj);
			UnlockPut;
		end else
			AddToGraveyard(&type, obj);
		obj := nil;
	end;

	procedure tGL._Error(const msg: string);
	begin
		if _notifyErrors then
		begin
			case Error.Text(msg).ContinueOrStopVariants.Show of
				TaskV1: ;
				TaskV3: _notifyErrors := no;
				else Fatal;
			end;
		end;
	end;

{$ifdef Debug}
	procedure tGL._LogSupUnsup(supported: boolean; const name, msgOk, msgFail: string);
	begin
		LogR(name);
		if supported then
			Log(msgOk, logOk)
		else
			Log(msgFail, logWarning);
	end;

	procedure tGL._LogSupUnsup(enabled, supported: boolean; const name, msgOk, msgFail, msgDisabled, msgDisabledAndFail: string);
	begin
		Log(name + IfThen(enabled, IfThen(supported, msgOk, msgFail), IfThen(supported, msgDisabled, msgDisabledAndFail)),
			LogMessageStyle(IfThen(enabled and supported, ord(logOk), ord(logWarning))));
	end;

	procedure tGL._LogAbout(instancingSupported: boolean);
	begin
		Log('Макс. проходов освещения: ' + ToString(MaxLightingPasses));
		Log('Макс. пользовательских шейдерных флагов: ' + ToString(UserShaderFlags.MAX_USER));
		Log('Макс. на проход: ' +
			ToString(min(Config.maxLightsEstimation div Config.lightEstimation[light_OmniA], MAX_OMNI_A)) + ' omni, ' +
			ToString(min(Config.maxLightsEstimation div Config.lightEstimation[light_OmniS], MAX_OMNI_S)) + ' omni с тенями, ' +
			ToString(min(Config.maxLightsEstimation div Config.lightEstimation[light_TargetedA], MAX_TARG_A)) + ' targeted или ' +
			ToString(min(Config.maxLightsEstimation div Config.lightEstimation[light_TargetedS], MAX_TARG_S)) + ' targeted с тенями.');
		Log('Макс. CSM сплитов: ' + ToString(MAX_CSM_SPLITS));
		Log('GLSL: #version ' + _slVersion);
		_LogSupUnsup(UniForSupported, 'For по юниформу: ', 'OK', 'выключен от греха подальше');
		_LogSupUnsup(InOutSupported, 'Квалификаторы in/out: ', 'поддерживаются', 'не поддерживаются');
		_LogSupUnsup(Config.allowInstancing and (_maxInstances > 1), instancingSupported, 'Инстансинг: ',
			'включен, МОЩЬ ' + ToString(round(100.0 * MaxInstances / MAX_INSTA_LIMIT)) + '% (' + ToString(MaxInstances) + '/' + ToString(MAX_INSTA_LIMIT) +
			')' + EOL +
			'Резерв юниформов (не задействован в инстансинге): ' + ToString(UNIFORMS_SUPPLY) + ' / ' + ToString(MaxUniforms) + ' (' +
			ToString(round(100.0 * UNIFORMS_SUPPLY / MaxUniforms)) + '%)',
			'не поддерживается', 'выключен', 'выключен, да и не поддерживается');
		_LogSupUnsup(Config.textureAnisotropy > 1.0, 'Анизотропная фильтрация: ', ToString(Config.textureAnisotropy) + 'x', 'выкл.');
		_LogSupUnsup(Config.allowMT, 'Рендер в отдельном потоке: ', 'включен', 'выключен');
		_LogSupUnsup(Config.allowUbo, UBOSupported, 'UBO: ', 'включены', 'не поддерживаются', 'выключены', 'выключены, да и не поддерживаются');
		_LogSupUnsup(Config.allowPrimitiveRestart, PrimitiveRestartSupported, 'Primitive Restart: ', 'включен', 'не поддерживается', 'выключен', 'выключен, да и не поддерживается');
		if Config.allowAdvFloats.Defined then
			_LogSupUnsup(Config.allowAdvFloats.StrictYes, _advFloatsRecommended, 'Half / Normalized Integer атрибуты: ', 'включены', 'не рекомендуются, но включены', 'выключены', 'выключены, да и не рекомендуются')
		else
			_LogSupUnsup(_advFloatsRecommended, 'Half / Normalized Integer атрибуты: ', 'включены (автоматически)', 'выключены (автоматически)');
		if Config.allowUbyteIndices.Defined then
			_LogSupUnsup(Config.allowUbyteIndices.StrictYes, _ubyteIdsRecommended, 'Однобайтовые индексы: ', 'включены', 'не рекомендуются, но включены', 'выключены', 'выключены, да и не рекомендуются')
		else
			_LogSupUnsup(_ubyteIdsRecommended, 'Однобайтовые индексы: ', 'включены (автоматически)', 'выключены (автоматически)');
		if Config.vaAlignment <> 0 then
			_LogSupUnsup(Config.vaAlignment < 0, _vaAlignment = 1, 'Выравнивание данных в VBO: ',
				'не используется', 'выключено (рек. ' + ToString(_vaAlignment) + ')', 'включено (' + ToString(Config.vaAlignment) + ')', 'включено (запрошено ' + ToString(Config.vaAlignment) + ', рек.' + ToString(_vaAlignment) + ')')
		else
			_LogSupUnsup(_vaAlignment = 1, 'Выравнивание данных в VBO: ', 'выключено (автоматически)', 'включено (автоматически; ' + ToString(_vaAlignment) + ')');
		_LogSupUnsup(Config.allowGeometryShader, GeometryShaderSupported, 'Геометрические шейдеры: ', 'включены', 'не поддерживаются', 'выключены', 'выключены, да и не поддерживаются');
		_LogSupUnsup(Config.allowBinaryShaders, BinaryShadersSupported, 'Binary Shaders: ', 'включены', 'не поддерживаются', 'выключены', 'выключены, да и не поддерживаются');
	end;
{$endif}

	function tGL._InitContext(const winSize: UintVec2): sint;
	var
		instancingSupported: boolean;
	begin
		Assert(not _ctxInited);
		result := sint(mm.window.BindGLContext and (not (Config.allowMT and Config.forceMTfail)) and _InitGL);
		if (result > 0) and (_maxUniforms < MinUniforms) and
			(Warning
			.Text(Format('Если верить драйверу видеокарты, оно даже не запустится: MaxUniforms = {0}, требуется минимум {1}.', [_maxUniforms, MinUniforms]))
			.Variant('Продолжить').Variant('Провалить инициализацию').Show <> TaskV1)
		then
		begin
			_DoneGL;
			result := -1;
		end;
		if result <= 0 then
		begin
			mm.window.UnbindGLContext;
			exit;
		end;

		instancingSupported := _maxInstances > 1;
		if instancingSupported then
			if _maxUniforms >= UNIFORMS_SUPPLY then
				_maxInstances := clamp((_maxUniforms - UNIFORMS_SUPPLY) div MAX_UNIFORMS_PER_INSTANCE, 1, MAX_INSTA_LIMIT)
			else
				_maxInstances := 1;
	{$ifdef Debug} _LogAbout(instancingSupported); {$endif}

		if not Config.allowInstancing then _maxInstances := 1;

		_UBOSupported := _UBOSupported and Config.allowUbo;
		_primitiveRestartSupported := _primitiveRestartSupported and Config.allowPrimitiveRestart;
		_geometryShaderSupported := _geometryShaderSupported and Config.allowGeometryShader;
		_binaryShadersSupported := _binaryShadersSupported and Config.allowBinaryShaders;
		_advFloatsRecommended := GLBase.Config.allowAdvFloats.Decide(_advFloatsRecommended);
		_ubyteIdsRecommended := GLBase.Config.allowUbyteIndices.Decide(_ubyteIdsRecommended);
		if Config.vaAlignment > 0 then _vaAlignment := Config.vaAlignment;
		if Config.vaAlignment < 0 then _vaAlignment := 1;

		_ResizeRenderTarget(ScreenRT, winSize);
		_ctxInited := yes;
	end;

	procedure tGL._DoneContext;
	begin
		if not _ctxInited then exit;
		CleanupGraveyard;
		_DoneGL;
		mm.window.UnbindGLContext;
		_ctxInited := no;
	end;

	procedure tGL._BeginFrame;
	begin
	{$ifdef Debug}
		_curDrawCalls := 0;
		_curStateChanges := 0;
		_curTris := 0;
		_curSended := 0;
	{$endif}
	end;

	procedure tGL._EndFrame(frameNo: sint);
	begin
		mm.window.SwapBuffers;
		GLOnEndFrame;
		_OnEndFrame;
		CleanupGraveyard;

	{$ifdef Debug}
		drawCallsDynamics.NewValue(_curDrawCalls, frameNo);
		stateChangesDynamics.NewValue(_curStateChanges, frameNo);
		trisDynamics.NewValue(_curTris, frameNo);
		sendedDynamics.NewValue(_curSended, frameNo);
	{$else}
		Assert(frameNo = frameNo);
	{$endif}
	end;

	function tGL.LockPut(op: Opcode; size: size_t): Command.pBase;
	begin
		result := cmds.LockPut(size);
		result^.op := op;
	end;

	function tGL.LockPutDynData(op: Opcode; size: size_t; data: pointer; dataSize: size_t; ownDataPtr: boolean): Command.pBase;
	var
		storeInPlace: boolean;
		dd: Command.DynamicData.pStruct;
	begin
		// если Assigned(data) и dataSize = 0, команда передаёт произвольный указатель как есть.
	{$ifdef Debug}
		if dataSize <> dataSize and Command.DynamicData.SIZE_MASK then
			Fatal(Format('Слишком большой размер данных команды: {0}.', [dataSize]));
	{$endif}

		storeInPlace := not ownDataPtr and (dataSize > 0) and (dataSize <= StoreInPlaceUpTo);
		if storeInPlace then
		begin
		{$ifdef Debug} immediateLock.Enter; allocatedInPlace += dataSize; immediateLock.Leave; {$endif}
			result := cmds.LockPut(size - sizeof(Command.DynamicData.Struct.data) + dataSize);
		end else
			result := cmds.LockPut(size - sizeof(Command.DynamicData.Struct.data) + sizeof(pointer));
		result^.op := op;

		// Предполагается, что в конце команды будет DynamicData.Struct.
		dd := pointer(result) + (size - sizeof(Command.DynamicData.Struct));
		dd^.sizex := dataSize;
		if storeInPlace then
		begin
			dd^.sizex := dd^.sizex or Command.DynamicData.IN_PLACE;
			memcpy(data, @(dd^.data), dataSize);
		end else
		begin
			if dataSize > 0 then dd^.sizex := dd^.sizex or Command.DynamicData.OWN_PTR;
			if ownDataPtr or (dataSize = 0) then pPointer(@dd^.data[0])^ := data else
			begin
			{$ifdef Debug} immediateLock.Enter; allocatedDynamically += dataSize; immediateLock.Leave; {$endif}
				pPointer(@dd^.data[0])^ := GetMem(dataSize);
				memcpy(data, pPointer(@dd^.data[0])^, dataSize);
			end;
		end;
	end;

	procedure tGL.UnlockPut;
	begin
		cmds.UnlockPut;
	end;

	function CommandCompleted(param: pointer): boolean;
	begin
		result := Command.pBase(param)^.op = Opcode._CommandCompleted;
	end;

	procedure tGL.UnlockPutImmediate(cmd: Command.pBase);
	begin
		cmds.UnlockPut;
		immediateLock.Enter;
		cmdFinished.Wait(immediateLock, @CommandCompleted, cmd);
		immediateLock.Leave;
	end;

	procedure tGL.ExecuteNext;
	type
		Op = Opcode;
		function DynamicSize(const dd: Command.DynamicData.Struct): size_t;
		begin
			result := dd.sizex and Command.DynamicData.SIZE_MASK;
		end;

		function DynamicPtr(const dd: Command.DynamicData.Struct): pointer;
		begin
			if Command.DynamicData.IN_PLACE and dd.sizex <> 0 then
				result := @dd.data
			else
				result := pPointer(@dd.data[0])^;
		end;

		function FreeDynamic(size: size_t; const dd: Command.DynamicData.Struct): size_t;
		begin
			result := size;
			if Command.DynamicData.IN_PLACE and dd.sizex <> 0 then
				result := result - sizeof(dd.data) + DynamicSize(dd)
			else
			begin
				result := result - sizeof(dd.data) + sizeof(pointer);
				if Command.DynamicData.OWN_PTR and dd.sizex <> 0 then FreeMemWeak(pPointer(@dd.data[0])^);
			end;
		end;

		procedure NoteCompleted(cmd: Command.pBase);
		begin
			immediateLock.Enter;
			cmd^.op := Opcode._CommandCompleted;
			immediateLock.Leave;
			cmdFinished.WakeOne;
		end;
	var
		cmd: Command.pBase;
		size: size_t;
		CtxInit:    Command.pCtxInit absolute cmd;
		CtxDone:    Command.pCtxDone absolute cmd;
		MemStat:    Command.pMemStat absolute cmd;
		FrameEnd:   Command.pFrameEnd absolute cmd;
		ClearRT:    Command.pClear absolute cmd;
		Purge:      Command.pPurge absolute cmd;
		Batch:      Command.pBatch absolute cmd;
		VDInit:     Command.pVDInit absolute cmd;
		VDSetVA:    Command.pVDSetVA absolute cmd;
		BufInit:    Command.pBufInit absolute cmd;
		BufLoad:    Command.pBufLoad absolute cmd;
		BufSub:     Command.pBufSub absolute cmd;
		ShInit:     Command.pShInit absolute cmd;
		ShpInit:    Command.pShpInit absolute cmd;
		ShpDone:    Command.pShpDone absolute cmd;
		SendUni:    Command.pSendUni absolute cmd;
		BindUbos:   Command.pBindUbos absolute cmd;
		TexInit:    Command.pTexInit absolute cmd;
		TexLoad:    Command.pTexLoad absolute cmd;
		TexSub:     Command.pTexSub absolute cmd;
		TexOpt:     Command.pTexOpt absolute cmd;
		TexQuIm:    Command.pTexQuIm absolute cmd;
		RTInit:     Command.pRTInit absolute cmd;
		RTSize:     Command.pRTSize absolute cmd;
		RTTexAt:    Command.pRTTexAt absolute cmd;
		RTTexDt:    Command.pRTTexDt absolute cmd;
		RTValid:    Command.pRTValid absolute cmd;
		RTQuIm:     Command.pRTQuIm absolute cmd;
		RastOpt:    Command.pRastOpt absolute cmd;
	begin
		cmd := cmds.LockGet(yes);
		case cmd^.op of
			Op.CtxInit:
				begin
					size := sizeof(Command.CtxInit);
					CtxInit^.result^ := _InitContext(CtxInit^.winSize);
					NoteCompleted(CtxInit);
				end;
			Op.CtxDone:
				begin
					size := sizeof(Command.CtxDone);
					_DoneContext;
					NoteCompleted(CtxDone);
				end;
			Op.MemStat:
				begin
					size := sizeof(Command.MemStat);
					MemStat^.result^ := _MemoryUsed(MemStat^.what);
					NoteCompleted(MemStat);
				end;
			Op.FrameStart:
				begin
					size := sizeof(Command.FrameStart);
					_BeginFrame;
				end;
			Op.FrameEnd:
				begin
					size := sizeof(Command.FrameEnd);
					_EndFrame(FrameEnd^.frameNo);
					NoteCompleted(FrameEnd);
				end;
			Op.Clear:
				begin
					size := sizeof(Command.Clear);
					_Clear(ClearRT^.rt^, ClearRT^.bufs, ClearRT^.color);
				end;
			Op.Purge:
				begin
					size := sizeof(Command.Purge);
					Purge^.grave.Purge(@self);
				end;
			Op.Batch:
				begin
					size := sizeof(Command.Batch);
					_DrawBatch(Batch^.rt^, Batch^.vd^, Batch^.prog^, Batch^.ibOffset, Batch^.nIndices, Batch^.indexType, Batch^.nInstances);
				end;
			Op.VDInit:
				begin
					size := sizeof(Command.VDInit);
					_CreateVertexDeclaration(VDInit^.vd^, VDInit^.ib^);
				end;
			Op.VDSetVA:
				begin
					size := sizeof(Command.VDSetVA);
					_SetVertexAttribute(VDSetVA^.vd^, VDSetVA^.index, VDSetVA^.&type, VDSetVA^.stride, VDSetVA^.vbOffset);
				end;
			Op.BufInit:
				begin
					size := sizeof(Command.BufInit);
					_CreateBuffer(BufInit^.buf^);
				end;
			Op.BufLoad:
				begin
					_BufferData(BufLoad^.buf^, DynamicSize(BufLoad^.data), DynamicPtr(BufLoad^.data), BufLoad^.usage);
					size := FreeDynamic(sizeof(Command.BufLoad), BufLoad^.data);
				end;
			Op.BufSub:
				begin
					_BufferSubData(BufSub^.buf^, BufSub^.offset, DynamicSize(BufSub^.data), DynamicPtr(BufSub^.data));
					size := FreeDynamic(sizeof(Command.BufSub), BufSub^.data);
				end;
			Op.ShInit:
				begin
					size := sizeof(Command.ShInit);
					_CreateShader(ShInit^.sh^, ShInit^.&type, ShInit^.name, ShInit^.source);
					System.Finalize(ShInit^);
				end;
			Op.ShpInit:
				begin
					size := sizeof(Command.ShpInit);
					ShpInit^.result^ := _CreateProgram(ShpInit^.prog^, ShpInit^.namex, ShpInit^.shaders, ShpInit^.info^);
					NoteCompleted(ShpInit);
					System.Finalize(ShpInit^);
				end;
			Op.ShpDone:
				begin
					size := sizeof(Command.ShpDone);
					_DeleteProgram(ShpDone^.prog^);
					NoteCompleted(ShpDone);
					dispose(ShpDone^.prog);
				end;
			Op.SendUni:
				begin
					_SetUniform(SendUni^.prog^, SendUni^.u^, DynamicPtr(SendUni^.data), SendUni^.count);
					size := FreeDynamic(sizeof(Command.SendUni), SendUni^.data);
				end;
			Op.BindUbos:
				begin
					size := sizeof(Command.BindUbos);
					_SetUbos(BindUbos^.prog^, BindUbos^.buffers);
					System.Finalize(BindUbos^);
				end;
			Op.TexInit:
				begin
					size := sizeof(Command.TexInit);
					_CreateTexture(TexInit^.tex^, TexInit^.target);
				end;
			Op.TexLoad:
				begin
					_TexImage(TexLoad^.tex^, TexLoad^.size, TexLoad^.format, DynamicSize(TexLoad^.data), DynamicPtr(TexLoad^.data), TexLoad^.level);
					size := FreeDynamic(sizeof(Command.TexLoad), TexLoad^.data);
				end;
			Op.TexSub:
				begin
					_TexSubImage(TexSub^.tex^, TexSub^.offset, TexSub^.size, TexSub^.format, DynamicSize(TexSub^.data), DynamicPtr(TexSub^.data), TexSub^.level);
					size := FreeDynamic(sizeof(Command.TexSub), TexSub^.data);
				end;
			Op.TexOpt:
				begin
					size := sizeof(Command.TexOpt);
					_SetTextureParams(TexOpt^.tex^, TexOpt^.params, TexOpt^.paramFields);
				end;
			Op.TexQuIm:
				begin
					size := sizeof(Command.TexQuIm);
					TexQuIm^.result^ := _GetTexImage(TexQuIm^.tex^, TexQuIm^.level, TexQuIm^.format);
					NoteCompleted(TexQuIm);
				end;
			Op.RTInit:
				begin
					size := sizeof(Command.RTInit);
					_CreateRenderTarget(RTInit^.rt^);
				end;
			Op.RTSize:
				begin
					size := sizeof(Command.RTSize);
					_ResizeRenderTarget(RTSize^.rt^, RTSize^.size);
				end;
			Op.RTTexAt:
				begin
					size := sizeof(Command.RTTexAt);
					_AttachRenderTexture(RTTexAt^.rt^, RTTexAt^.tex^, RTTexAt^.level, RTTexAt^.target, RTTexAt^.targetN);
				end;
			Op.RTTexDt:
				begin
					size := sizeof(Command.RTTexDt);
					_DetachRenderTexture(RTTexDt^.rt^, RTTexDt^.target, RTTexDt^.targetN);
				end;
			Op.RTValid:
				begin
					size := sizeof(Command.RTValid);
					RTValid^.result^ := _ValidateRenderTarget(RTValid^.rt^);
					NoteCompleted(RTValid);
				end;
			Op.RTQuIm:
				begin
					size := sizeof(Command.RTQuIm);
					RTQuIm^.result^ := _GetRTImage(RTQuIm^.rt^, RTQuIm^.format, RTQuIm^.mrtN);
					NoteCompleted(RTQuIm);
				end;
			Op.RastPush:
				begin
					size := sizeof(Command.RastPush);
					_PushRasterizerState;
				end;
			Op.RastPop:
				begin
					size := sizeof(Command.RastPop);
					_PopRasterizerState;
				end;
			Op.RastOpt:
				begin
					size := sizeof(Command.RastOpt);
					_SetRasterizerState(RastOpt^.state, RastOpt^.fields);
				end;
			Op.Terminate:
				begin
					cmds.UnlockGet(sizeof(Command.Terminate));
					Thread.TerminateSelf;
				end;
		{$ifdef Debug} else raise ExhaustiveCase(ord(cmd^.op), 'GLCommand.op'); {$endif}
		end;
		cmds.UnlockGet(size);
	end;

	function tGL.InitContext(const winSize: UintVec2): boolean;
	var
		cmd: Command.pCtxInit;
		r: sint;
	begin
		if Config.allowMT then
		begin
			Thread.Start('OpenGL', cmdThread, @GLThread, @self);
		{$ifdef Debug} Log('GL туредо стаато! ' + cmdThread.Human(BracketedInfo), logOK); {$endif}

			cmd          := Command.pCtxInit(LockPut(Opcode.CtxInit, sizeof(Command.CtxInit)));
			cmd^.winSize := winSize;
			cmd^.result  := @r;
			UnlockPutImmediate(cmd);
			r             := cmd^.result^;
			if r = 0 then
			begin
				Warning.Text('Не удалось загрузить OpenGL из другого потока.' + EOL +
					'Для чистоты эксперимента попробую отключить рендер в отдельном потоке.').Show;
			{$ifdef Debug} Log('Попробую выключить рендер в отдельном потоке.', logWarning); {$endif}
				KillCmdThread;
				Config.allowMT := no;
			end;
		end else
			r := 0;
		if (r = 0) and (not Config.allowMT) then r := _InitContext(winSize);
		result := r > 0;

		if result then
		begin
			GLClasses_Init;
		end else
			Error.Show(
				'Не удалось загрузить OpenGL.' + EOL + EOL +
				'Одно из ' + IfThen(IntelVendor, 'трёх', 'двух') + ':' + EOL +
				'— На этом компьютере играть невозможно' + IfThen(IntelVendor, ' (Intel)') + '.' + EOL +
				'— Не подхватился / не установлен драйвер видеокарты.' +
				IfThen(IntelVendor, EOL + '— Двойная видеокарта не переключилась на дискретную.'));
	end;

	procedure tGL.DoneContext;
	begin
		if _ctxInited then
			GLClasses_Done;

		if cmdThread.OK then
		begin
			UnlockPutImmediate(LockPut(Opcode.CtxDone, sizeof(Command.CtxDone)));
		end else
			_DoneContext;
	end;

	function tGL.MemoryUsed(what: GLMemoryUsage = GLmem_Total): size_t;
	var
		cmd: Command.pMemStat;
	begin
		if Config.allowMT then
		begin
			cmd         := Command.pMemStat(LockPut(Opcode.MemStat, sizeof(Command.MemStat)));
			cmd^.what   := what;
			cmd^.result := @result;
			UnlockPutImmediate(cmd);
			result      := cmd^.result^;
		end else
			result := _MemoryUsed(what);
	end;

	procedure tGL.BeginFrame;
	begin
		UTime^.SetFloat(mm.SceneTime);
		UFrameDt^.SetFloat(mm.FrameDt);
		UGUITime^.SetFloat(mm.GUITime);

		if Config.allowMT then
		begin
			LockPut(Opcode.FrameStart, sizeof(Command.FrameStart));
			UnlockPut;
		end else
			_BeginFrame;
	end;

	procedure tGL.EndFrame(frameNo: sint);
	var
		cmd: Command.pFrameEnd;
	begin
		if Config.allowMT then
		begin
			cmd          := Command.pFrameEnd(LockPut(Opcode.FrameEnd, sizeof(Command.FrameEnd)));
			cmd^.frameNo := frameNo;
			UnlockPutImmediate(cmd);
		end else
			_EndFrame(frameNo);
	end;

	procedure tGL.Clear(rt: pGLRenderTarget; bufs: GLRenderBuffers; const clearColor: Vec4);
	var
		cmd: Command.pClear;
	begin
		Assert(bufs <> [], 'Clear.bufs = []');
		if Config.allowMT then
		begin
			cmd        := Command.pClear(LockPut(Opcode.Clear, sizeof(Command.Clear)));
			cmd^.rt    := rt;
			cmd^.bufs  := bufs;
			cmd^.color := clearColor;
			UnlockPut;
		end else
			_Clear(rt^, bufs, clearColor);
	end;

	procedure tGL.DrawBatch(rt: pGLRenderTarget; vd: pGLVertexDeclaration; prog: pGLProgram; ibOffset: PtrUint; nIndices: uint; indexType: GLType; nInstances: uint = 1);
	var
		cmd: Command.pBatch;
	begin
		Assert((indexType <> GLType.Ubyte) or _ubyteIdsRecommended, 'Нужно сконвертировать ubyte-индексы во что-нибудь ещё.');
		if Config.allowMT then
		begin
			cmd             := Command.pBatch(LockPut(Opcode.Batch, sizeof(Command.Batch)));
			cmd^.rt         := rt;
			cmd^.vd         := vd;
			cmd^.prog       := prog;
			cmd^.ibOffset   := ibOffset;
			cmd^.nIndices   := nIndices;
			cmd^.indexType  := indexType;
			cmd^.nInstances := nInstances;
			UnlockPut;
		end else
			_DrawBatch(rt^, vd^, prog^, ibOffset, nIndices, indexType, nInstances);
	end;

	function tGL.CreateVertexDeclaration(vb, ib: pGLBuffer; topology: GLTopology): pGLVertexDeclaration;
	const
		Empty: GLVertexDeclaration = (id: 0; vb: nil; topology: GLtopology_Tris);
	var
		cmd: Command.pVDInit;
	begin
		new(result);
		result^ := Empty;
		result^.vb := vb;
		result^.topology := topology;
		if Config.allowMT then
		begin
			cmd := Command.pVDInit(LockPut(Opcode.VDInit, sizeof(Command.VDInit)));
			cmd^.vd := result;
			cmd^.ib := ib;
			UnlockPut;
		end else
			_CreateVertexDeclaration(result^, ib^);
	end;

	procedure tGL.DeleteVertexDeclaration(var vd: pGLVertexDeclaration);
	begin
		GenericPurge(DeadVD, pointer(vd));
	end;

	procedure tGL.SetVertexAttribute(vd: pGLVertexDeclaration; index: sint; type_: GLType; stride: size_t; vbOffset: PtrUint);
	var
		cmd: Command.pVDSetVA;
	begin
		if Config.allowMT then
		begin
			cmd           := Command.pVDSetVA(LockPut(Opcode.VDSetVA, sizeof(Command.VDSetVA)));
			cmd^.vd       := vd;
			cmd^.index    := index;
			cmd^.&type    := type_;
			cmd^.stride   := stride;
			cmd^.vbOffset := vbOffset;
			UnlockPut;
		end else
			_SetVertexAttribute(vd^, index, type_, stride, vbOffset);
	end;

	function tGL.CreateBuffer(target: GLBufferTarget): pGLBuffer;
	const
		Empty: GLBuffer = (target: GLbuffer_Vertex; id: 0);
	var
		cmd: Command.pBufInit;
	begin
		new(result);
		result^ := Empty;
		result^.target := target;
		if Config.allowMT then
		begin
			cmd      := Command.pBufInit(LockPut(Opcode.BufInit, sizeof(Command.BufInit)));
			cmd^.buf := result;
			UnlockPut;
		end else
			_CreateBuffer(result^);
	end;

	procedure tGL.DeleteBuffer(var buf: pGLBuffer);
	begin
		GenericPurge(DeadBuffer, pointer(buf));
	end;

	procedure tGL.BufferData(buf: pGLBuffer; size: size_t; data: pointer; usage: GLBufferUsage = GLusage_StaticDraw);
	var
		cmd: Command.pBufLoad;
	begin
		if Config.allowMT then
		begin
			cmd        := Command.pBufLoad(LockPutDynData(Opcode.BufLoad, sizeof(Command.BufLoad), data, size, not Assigned(data)));
			cmd^.buf   := buf;
			cmd^.usage := usage;
			UnlockPut;
		end else
			_BufferData(buf^, size, data, usage);
	end;

	procedure tGL.BufferSubData(buf: pGLBuffer; offset: PtrUint; size: size_t; data: pointer; takeThisData: boolean = no);
	var
		cmd: Command.pBufSub;
	begin
		// Assert(size > 0, 'SubData.size = 0'); — легально, просто не очень красиво
		if Config.allowMT then
		begin
			cmd         := Command.pBufSub(LockPutDynData(Opcode.BufSub, sizeof(Command.BufSub), data, size, takeThisData));
			cmd^.buf    := buf;
			cmd^.offset := offset;
			UnlockPut;
		end else
		begin
			_BufferSubData(buf^, offset, size, data);
			if takeThisData then FreeMem(data);
		end;
	end;

	function tGL.CreateShader(type_: ShaderType; const name, source: string): pGLShader;
	const
		Empty: GLShader = (id: 0; compiled: no);
	var
		cmd: Command.pShInit;
	begin
		new(result);
		result^ := Empty;
		if Config.allowMT then
		begin
			cmd         := Command.pShInit(LockPut(Opcode.ShInit, sizeof(Command.ShInit)));
			Initialize(cmd^.name); Initialize(cmd^.source);
			cmd^.sh     := result;
			cmd^.&type  := type_;
			cmd^.name   := name;
			cmd^.source := source;
			UnlockPut;
		end else
			_CreateShader(result^, type_, name, source);
	end;

	procedure tGL.DeleteShader(var sh: pGLShader);
	begin
		GenericPurge(DeadShader, pointer(sh));
	end;

	function tGL.CreateProgram(const namex: string; const sh: array of pGLShader; out info: ShaderEntrails): pGLProgram;
	const
		EmptyInfo: ShaderEntrails = (u: nil; ub: nil; va: nil);
		Empty: GLProgram =
		(
			id: 0;
			nSamplers: 0;
			samplers:
			(
				(target: GLtexture_2D; id: 0), (target: GLtexture_2D; id: 0), (target: GLtexture_2D; id: 0),
				(target: GLtexture_2D; id: 0), (target: GLtexture_2D; id: 0), (target: GLtexture_2D; id: 0),
				(target: GLtexture_2D; id: 0), (target: GLtexture_2D; id: 0), (target: GLtexture_2D; id: 0),
				(target: GLtexture_2D; id: 0), (target: GLtexture_2D; id: 0), (target: GLtexture_2D; id: 0)
			);
			nUbos: 0;
			ubos: ((id: 0), (id: 0), (id: 0), (id: 0), (id: 0), (id: 0), (id: 0), (id: 0))
		);
	var
		cmd: Command.pShpInit;
		i: sint;
		ok: boolean;
	begin
		info := EmptyInfo;
		new(result);
		result^ := Empty;
		if Config.allowMT then
		begin
			cmd          := Command.pShpInit(LockPut(Opcode.ShpInit, sizeof(Command.ShpInit)));
			Initialize(cmd^.namex); Initialize(cmd^.shaders);
			SetLength(cmd^.shaders, length(sh));
			for i := 0 to High(sh) do cmd^.shaders[i] := sh[i];

			cmd^.prog    := result;
			cmd^.namex   := namex;
			cmd^.info    := @info;
			cmd^.result  := @ok;
			UnlockPutImmediate(cmd);
			ok           := cmd^.result^;
		end else
			ok := _CreateProgram(result^, namex, sh, info);

		if not ok then
		begin
			info := EmptyInfo;
			dispose(result);
			result := nil;
		end;
	end;

	procedure tGL.DeleteProgram(var prog: pGLProgram);
	var
		cmd: Command.pShpDone;
	begin
		if Config.allowMT then
		begin
			// Именно immediate. Или создавать копию юниформа в SetUniform.
			cmd       := Command.pShpDone(LockPut(Opcode.ShpDone, sizeof(Command.ShpDone)));
			cmd^.prog := prog;
			UnlockPutImmediate(cmd);
		end else
			AddToGraveyard(DeadProgram, prog);
		prog := nil;
	end;

	procedure tGL.SetUniform(prog: pGLProgram; var u: GLUniform; value: pointer; count: uint = High(uint));
	var
		cmd: Command.pSendUni;
		size: size_t;
	begin
		if count > u.count then count := u.count;
		Assert(count > 0, 'SetUniform(0)');

		if Config.allowMT then
		begin
			size := GLTypeInfo[u.type_].sizeof; if count <> 1 then size *= count; // для Sampler сработает ownDataPtr
			Assert((u.type_ = GLType.Sampler) = (size = 0), Format('UniformType = {0}, size = {1}', [ord(u.type_), size]));

			cmd        := Command.pSendUni(LockPutDynData(Opcode.SendUni, sizeof(Command.SendUni), value, size, no));
			cmd^.prog  := prog;
			cmd^.u     := @u;
			cmd^.count := count;
			UnlockPut;
		end else
			_SetUniform(prog^, u, value, count);
	end;

	procedure tGL.SetUbos(prog: pGLProgram; const bufs: array of pGLBuffer);
	var
		cmd: Command.pBindUbos;
		i: sint;
	begin
		if Config.allowMT then
		begin
			cmd       := Command.pBindUbos(LockPut(Opcode.BindUbos, sizeof(Command.BindUbos)));
			Initialize(cmd^.buffers);
			SetLength(cmd^.buffers, length(bufs));
			for i := 0 to High(bufs) do cmd^.buffers[i] := bufs[i];
			cmd^.prog := prog;
			UnlockPut;
		end else
			_SetUbos(prog^, bufs);
	end;

	procedure tGL.PlaceUboData(src: pointer; var u: GLUboUniform; dst: pointer; count: uint = High(uint));
	var
		i, j: uint;
		unitSize: size_t;
		t: pointer;
	begin
		trace_call ('GL.PlaceUboData');
		if count > u.count then count := u.count;
		Assert(count > 0, 'PlaceUboData(0)');
		if u.stridesArePacked then
		begin
			memcpy(src, dst, count * GLTypeInfo[u.typ].sizeof);
			exit;
		end;

		if GLTypeFlag.Matrix in GLTypeInfo[u.typ].flags then
		begin
			unitSize := GLTypeInfo[GLTypeInfo[u.typ].baseType].sizeof;
			for i := 1 to count do
			begin
				t := dst;
				for j := 1 to GLTypeInfo[u.typ].baseDim do
				begin
					memcpy(src, dst, unitSize);
					src += unitSize;
					dst += u.matStride;
				end;
				dst := t + u.arrStride;
			end;
		end else
		begin
			unitSize := GLTypeInfo[u.typ].sizeof;
			for i := 1 to count do
			begin
				memcpy(src, dst, unitSize);
				src += unitSize;
				dst += u.arrStride;
			end;
		end;
		leave_call;
	end;

	function tGL.CreateTexture(target: GLTextureTarget): pGLTexture;
	var
		cmd: Command.pTexInit;
	begin
		new(result);
		if Config.allowMT then
		begin
			cmd         := Command.pTexInit(LockPut(Opcode.TexInit, sizeof(Command.TexInit)));
			cmd^.tex    := result;
			cmd^.target := target;
			UnlockPut;
		end else
			_CreateTexture(result^, target);
	end;

	procedure tGL.DeleteTexture(var tex: pGLTexture);
	begin
		GenericPurge(DeadTexture, pointer(tex));
	end;

	procedure tGL.TexImage(tex: pGLTexture; const size: UintSize3; format: GLImageFormat; dataSize: size_t; data: pointer; level: uint = 0; takeThisData: boolean = no);
	var
		cmd: Command.pTexLoad;
	begin
		if Config.allowMT then
		begin
			cmd         := Command.pTexLoad(LockPutDynData(Opcode.TexLoad, sizeof(Command.TexLoad), data, dataSize, not Assigned(data) or takeThisData));
			cmd^.tex    := tex;
			cmd^.size   := size;
			cmd^.format := format;
			cmd^.level  := level;
			UnlockPut;
		end else
		begin
			_TexImage(tex^, size, format, dataSize, data, level);
			if takeThisData then FreeMem(data);
		end;
	end;

	procedure tGL.TexSubImage(tex: pGLTexture; const offset: UintOffset3; const size: UintSize3; format: GLImageFormat; dataSize: size_t; data: pointer; level: uint = 0; takeThisData: boolean = no);
	var
		cmd: Command.pTexSub;
	begin
		if Config.allowMT then
		begin
			cmd         := Command.pTexSub(LockPutDynData(Opcode.TexSub, sizeof(Command.TexSub), data, dataSize, takeThisData));
			cmd^.tex    := tex;
			cmd^.offset := offset;
			cmd^.size   := size;
			cmd^.format := format;
			cmd^.level  := level;
			UnlockPut;
		end else
		begin
			_TexSubImage(tex^, offset, size, format, dataSize, data, level);
			if takeThisData then FreeMem(data);
		end;
	end;

	procedure tGL.SetTextureParams(tex: pGLTexture; const params: GLTextureParamsRec; paramFields: GLTextureParamSet);
	var
		cmd: Command.pTexOpt;
	begin
		if Config.allowMT then
		begin
			cmd              := Command.pTexOpt(LockPut(Opcode.TexOpt, sizeof(Command.TexOpt)));
			cmd^.tex         := tex;
			cmd^.params      := params;
			cmd^.paramFields := paramFields;
			UnlockPut;
		end else
			_SetTextureParams(tex^, params, paramFields);
	end;

	function tGL.GetTexImage(tex: pGLTexture; level: uint; format: GLImageFormat): pointer;
	var
		cmd: Command.pTexQuIm;
	begin
		if Config.allowMT then
		begin
			cmd         := Command.pTexQuIm(LockPut(Opcode.TexQuIm, sizeof(Command.TexQuIm)));
			cmd^.tex    := tex;
			cmd^.level  := level;
			cmd^.format := format;
			cmd^.result := @result;
			UnlockPutImmediate(cmd);
			result      := cmd^.result^;
		end else
			result := _GetTexImage(tex^, level, format);
	end;

	function tGL.CreateRenderTarget: pGLRenderTarget;
	var
		cmd: Command.pRTInit;
	begin
		new(result);
		result^ := DefaultRenderTarget;
		if Config.allowMT then
		begin
			cmd     := Command.pRTInit(LockPut(Opcode.RTInit, sizeof(Command.RTInit)));
			cmd^.rt := result;
			UnlockPut;
		end else
			_CreateRenderTarget(result^);
	end;

	procedure tGL.DeleteRenderTarget(var fb: pGLRenderTarget);
	begin
		GenericPurge(DeadRT, pointer(fb));
	end;

	procedure tGL.ResizeRenderTarget(fb: pGLRenderTarget; const size: UintVec2);
	var
		cmd: Command.pRTSize;
	begin
		if Config.allowMT then
		begin
			cmd       := Command.pRTSize(LockPut(Opcode.RTSize, sizeof(Command.RTSize)));
			cmd^.rt   := fb;
			cmd^.size := size;
			UnlockPut;
		end else
			_ResizeRenderTarget(fb^, size);
	end;

	procedure tGL.AttachRenderTexture(fb: pGLRenderTarget; tex: pGLTexture; level: uint; target: GLRenderBuffer; targetN: uint);
	var
		cmd: Command.pRTTexAt;
	begin
		if Config.allowMT then
		begin
			cmd          := Command.pRTTexAt(LockPut(Opcode.RTTexAt, sizeof(Command.RTTexAt)));
			cmd^.rt      := fb;
			cmd^.tex     := tex;
			cmd^.level   := level;
			cmd^.target  := target;
			cmd^.targetN := targetN;
			UnlockPut;
		end else
			_AttachRenderTexture(fb^, tex^, level, target, targetN);
	end;

	procedure tGL.DetachRenderTexture(fb: pGLRenderTarget; target: GLRenderBuffer; targetN: uint);
	var
		cmd: Command.pRTTexDt;
	begin
		if Config.allowMT then
		begin
			cmd          := Command.pRTTexDt(LockPut(Opcode.RTTexDt, sizeof(Command.RTTexDt)));
			cmd^.rt      := fb;
			cmd^.target  := target;
			cmd^.targetN := targetN;
			UnlockPut;
		end else
			_DetachRenderTexture(fb^, target, targetN);
	end;

	function tGL.ValidateRenderTarget(fb: pGLRenderTarget): GLRenderTargetStatus;
	var
		cmd: Command.pRTValid;
	begin
		if Config.allowMT then
		begin
			cmd         := Command.pRTValid(LockPut(Opcode.RTValid, sizeof(Command.RTValid)));
			cmd^.rt     := fb;
			cmd^.result := @result;
			UnlockPutImmediate(cmd);
			result      := cmd^.result^;
		end else
			result := _ValidateRenderTarget(fb^);
	end;

	function tGL.GetRTImage(fb: pGLRenderTarget; format: GLImageFormat; mrtN: uint = 0): pointer;
	var
		cmd: Command.pRTQuIm;
	begin
		if Config.allowMT then
		begin
			cmd         := Command.pRTQuIm(LockPut(Opcode.RTQuIm, sizeof(Command.RTQuIm)));
			cmd^.rt     := fb;
			cmd^.format := format;
			cmd^.mrtN   := mrtN;
			cmd^.result := @result;
			UnlockPutImmediate(cmd);
			result      := cmd^.result^;
		end else
			result := _GetRTImage(fb^, format, mrtN);
	end;

	function tGL.RasterizerState: GLRasterizerState;
	begin
		result := _rast;
	end;

	procedure tGL.PushRasterizerState;
	begin
		if Config.allowMT then
		begin
			LockPut(Opcode.RastPush, sizeof(Command.RastPush));
			UnlockPut;
		end else
			_PushRasterizerState;
	end;

	procedure tGL.PopRasterizerState;
	begin
		if Config.allowMT then
		begin
			LockPut(Opcode.RastPop, sizeof(Command.RastPop));
			UnlockPut;
		end else
			_PopRasterizerState;
	end;

	procedure tGL.SetRasterizerState(var state: GLRasterizerState; fields: GLRasterizerParams);
	var
		cmd: Command.pRastOpt;
	begin
		if Config.allowMT then
		begin
			cmd         := Command.pRastOpt(LockPut(Opcode.RastOpt, sizeof(Command.RastOpt)));
			cmd^.state  := state;
			cmd^.fields := fields;
			UnlockPut;
		end else
			_SetRasterizerState(state, fields);
	end;

	procedure tGL._PushRasterizerState;
	begin
	{$ifdef Debug}
		if _rastStackTop >= High(_rastStack) then Fatal('Стек состояний растеризатора переполнен, см. tGL._rastStack');
	{$endif}
		inc(_rastStackTop);
		_rastStack[_rastStackTop] := _rast;
	end;

	procedure tGL._PopRasterizerState;
	var
		fields: GLRasterizerParams;
		r2: pGLRasterizerState;
	begin
	{$ifdef Debug} if _rastStackTop < 0 then Fatal('Антипереполнение стека состояний растеризатора'); {$endif}
		r2 := @_rastStack[_rastStackTop];
		fields := [];
		if _rast.frontFace <> r2^.frontFace then Include(fields, GLrast_FrontFace);
		if _rast.cull <> r2^.cull then Include(fields, GLrast_Cull);
		if _rast.blend <> r2^.blend then Include(fields, GLrast_Blend);
		if _rast.wire <> r2^.wire then Include(fields, GLrast_Wire);
		if _rast.depthTest <> r2^.depthTest then Include(fields, GLrast_DepthTest);
		if _rast.depthMask <> r2^.depthMask then Include(fields, GLrast_DepthMask);
		if fields <> [] then _SetRasterizerState(r2^, fields);
		dec(_rastStackTop);
	end;

	procedure tGL._SetRasterizerState(var state: GLRasterizerState; fields: GLRasterizerParams);
	begin
		if GLrast_FrontFace in fields then _rast.frontFace := state.frontFace;
		if GLrast_Cull in fields then _rast.cull := state.cull;
		if GLrast_Blend in fields then _rast.blend := state.blend;
		if GLrast_Wire in fields then _rast.wire := state.wire;
		if GLrast_DepthTest in fields then _rast.depthTest := state.depthTest;
		if (GLrast_DepthMask in fields) and (_rast.depthMask <> state.depthMask) then _rast.depthMask := state.depthMask;
	end;

	function FromGL(const v: Vec3f): Vec3;
	begin
	{$if sizeof(v) = sizeof(Vec3)}
		result.data := Vec3.LinearData(v);
	{$else} {$note Vec3f size mismatch}
		result := Vec3.Make(v[0], v[1], v[2]);
	{$endif}
	end;

	function FromGL(const v: Vec2f): Vec2;
	begin
	{$if sizeof(v) = sizeof(Vec2)}
		result.data := Vec2.LinearData(v);
	{$else} {$note Vec2f size mismatch}
		result := Vec2.Make(v[0], v[1]);
	{$endif}
	end;

	function FromGL(const v: Vec4f): Vec4;
	begin
	{$if sizeof(v) = sizeof(Vec4)}
		result.data := Vec4.LinearData(v);
	{$else} {$note Vec4f size mismatch}
		result := Vec4.Make(v[0], v[1], v[2], v[3]);
	{$endif}
	end;

	operator :=(const v: Vec2): Vec2f;
	begin
	{$if sizeof(v) = sizeof(Vec2f)}
		result := Vec2f(v.data);
	{$else} {$note Vec2f size mismatch}
		result[0] := v.data[0];
		result[1] := v.data[1];
	{$endif}
	end;

	operator :=(const v: Vec3): Vec3f;
	begin
	{$if sizeof(v) = sizeof(Vec3f)}
		result := Vec3f(v.data);
	{$else} {$note Vec3f size mismatch}
		result[0] := v.data[0];
		result[1] := v.data[1];
		result[2] := v.data[2];
	{$endif}
	end;

	operator :=(const v: Vec4): Vec4f;
	begin
	{$if sizeof(v) = sizeof(Vec4f)}
		result := Vec4f(v.data);
	{$else} {$note Vec4f size mismatch}
		result[0] := v.data[0];
		result[1] := v.data[1];
		result[2] := v.data[2];
		result[3] := v.data[3];
	{$endif}
	end;

	operator :=(const v: Matrix4): Mat4f;
	{$if sizeof(Matrix4) <> sizeof(Mat4f)} var i: uint; {$endif}
	begin
	{$if sizeof(v) = sizeof(Mat4f)}
		result := Mat4f(v.data);
	{$else} {$note GLMat4 size mismatch}
		for i := 0 to 15 do
			result[i] := v.data.l[i];
	{$endif}
	end;

	// OpenGL 4.2+ использует универсально -127 .. 127.
	// Прежние версии в некоторых случаях (таких как данные вершинных атрибутов) используют -128 .. 127.
	// В этом случае формула нормализации становится (2x + 1) / (2^b - 1) (b — бит в результате),
	//                       денормализации — (x * (2^b - 1) - 1) / 2.
	//
	// Т. о. формулы, используемые ниже — неверные. Я использую их, чтобы обойти следующие проблемы:
	// а) если скормить OpenGL 3 данные, предназначенные OpenGL 4.2, исходная -1 превратится в -0.99, иногда это критично
	//    (края текстур, полноэкранные квады)
	// б) формулами OpenGL 3.2 невозможно представить 0.
	{$define denorm_impl:=
	begin
	{$ifdef Debug} if abs(x) > 1.0 then Log('Переполнение денормализации', logWarning); {$endif}
		if x > 0.0 then
			result := round(min(x, 1.0) * High(result))
		else
			result := round(max(x, -1.0) * -sint(Low(result)));
		end;}
	{$define norm_impl:=
	const
		GtK = 1.0 / High(x);
		LtK = 1.0 / -sint(Low(x));
	begin
		if x >= 0 then
			result := x * GtK
		else
			result := x * LtK;
	end;}
	function DenormI16(x: GLfloat): sint16; denorm_impl
	function NormI16(x: sint16): GLfloat; norm_impl
	function DenormI8(x: GLfloat): sint8; denorm_impl
	function NormI8(x: sint8): GLfloat; norm_impl
	{$undef norm_impl}
	{$undef denorm_impl}

	{$define denorm_impl:=
	begin
	{$ifdef Debug} if (x < 0.0) or (x > 1.0) then Log('Переполнение денормализации', logWarning); {$endif}
		result := clamp(round(x * High(result)), 0, High(result));
	end;}
	{$define norm_impl:=
	begin
		result := x * (1.0 / High(x));
	end;}
	function DenormU16(x: GLfloat): uint16; denorm_impl
	function NormU16(x: uint16): GLfloat; norm_impl
	function DenormU8(x: GLfloat): uint8; denorm_impl
	function NormU8(x: uint8): GLfloat; norm_impl
	{$undef norm_impl}
	{$undef denorm_impl}

	procedure Int2Int(inp: pointer; in_type: GLType; outp: pointer; out_type: GLType; count: sint = 1);
	var
		uiBuf: pUint32;
		i: sint;
	begin
		uiBuf := GetMem(count * sizeof(uint32));
		case in_type of
			GLType.Uint32: for i := 0 to count-1 do uiBuf[i] := pUint32(inp)[i];
			GLType.Uint16: for i := 0 to count-1 do uiBuf[i] := pUint16(inp)[i];
			GLType.Ubyte: for i := 0 to count-1 do uiBuf[i] := pByte(inp)[i];
		end;
		case out_type of
			GLType.Uint32: for i := 0 to count-1 do pUint32(outp)[i] := uiBuf[i];
			GLType.Uint16: for i := 0 to count-1 do pUint16(outp)[i] := uiBuf[i];
			GLType.Ubyte: for i := 0 to count-1 do pByte(outp)[i] := uiBuf[i];
		end;
		FreeMem(uiBuf);
	end;

	procedure Vec2Vec(inp: pointer; in_type: GLType; outp: pointer; out_type: GLType; count: sint = 1);
	const
		Defaults: array[0 .. 3] of hp_float = (0.0, 0.0, 0.0, 1.0);
	var
		fBuf, curf: pHp_float;
		inLen, outLen, bufLen, i, j: sint;
	begin
		inLen := GLTypeInfo[in_type].baseDim;
		outLen := GLTypeInfo[out_type].baseDim;
		bufLen := 4 * count;
		fBuf := GetMem(bufLen * sizeof(hp_float));
		for i := 0 to bufLen - 1 do
			fBuf[i] := defaults[i mod length(defaults)];
	{$define i2f:=
		begin
			curf := fBuf;
			for j := 0 to count-1 do
			begin
				for i := 0 to inLen-1 do
					curf[i] := conv(typ(inp)[i]);
				curf := curf + 4;
				inp := typ(inp) + inLen;
			end;
		end {$undef conv} {$undef typ}}
		case in_type of
		                      GLType.Float .. GLType.Vec4: {$define conv := }        {$define typ := pGLfloat} i2f;
		                   GLType.Ni16 .. GLType.Vec4Ni16: {$define conv := NormI16} {$define typ := pSint16}  i2f;
		                     GLType.Ni8 .. GLType.Vec4Ni8: {$define conv := NormI8}  {$define typ := pSint8}   i2f;
		                   GLType.Half .. GLType.Vec4Half: {$define conv := float32} {$define typ := pHalf}  i2f;
		                 GLType.Nui16 .. GLType.Vec4Nui16: {$define conv := NormU16} {$define typ := pUint16}  i2f;
		                   GLType.Nui8 .. GLType.Vec4Nui8: {$define conv := NormU8}  {$define typ := pUint8}   i2f;
		  GLType.Ubyte,  GLType.Vec2ui8 .. GLType.Vec4ui8: {$define conv := }        {$define typ := pUint8}   i2f;
		GLType.Uint16, GLType.Vec2ui16 .. GLType.Vec4ui16: {$define conv := }        {$define typ := pUint16}  i2f;
		    GLType.Int8,   GLType.Vec2i8 .. GLType.Vec4i8: {$define conv := }        {$define typ := pSint8}   i2f;
		  GLType.Int16,  GLType.Vec2i16 .. GLType.Vec4i16: {$define conv := }        {$define typ := pSint16}  i2f;
		                              {$ifdef Debug} else  raise UnsupportedCase(GLTypeIds[in_type], 'Vec2Vec.In'); {$endif}
		end;
	{$undef i2f}
	{$define f2o:=
		begin
			curf := fBuf;
			for j := 0 to count-1 do
			begin
				for i := 0 to outLen-1 do
					typ(outp)[i] := conv(curf[i]);
				curf := curf + 4;
				outp := typ(outp) + outLen;
			end;
		end {$undef conv} {$undef typ}}
		case out_type of
		                      GLType.Float .. GLType.Vec4: {$define conv := }          {$define typ := pGLfloat} f2o;
		                   GLType.Ni16 .. GLType.Vec4Ni16: {$define conv := DenormI16} {$define typ := pSint16}  f2o;
		                     GLType.Ni8 .. GLType.Vec4Ni8: {$define conv := DenormI8}  {$define typ := pSint8}   f2o;
		                   GLType.Half .. GLType.Vec4Half: {$define conv := float16}   {$define typ := pHalf}  f2o;
		                 GLType.Nui16 .. GLType.Vec4Nui16: {$define conv := DenormU16} {$define typ := pUint16}  f2o;
		                   GLType.Nui8 .. GLType.Vec4Nui8: {$define conv := DenormU8}  {$define typ := pUint8}   f2o;
		  GLType.Ubyte,  GLType.Vec2ui8 .. GLType.Vec4ui8: {$define conv := round}     {$define typ := pUint8}   f2o;
		GLType.Uint16, GLType.Vec2ui16 .. GLType.Vec4ui16: {$define conv := round}     {$define typ := pUint16}  f2o;
		    GLType.Int8,   GLType.Vec2i8 .. GLType.Vec4i8: {$define conv := round}     {$define typ := pSint8}   f2o;
		  GLType.Int16,  GLType.Vec2i16 .. GLType.Vec4i16: {$define conv := round}     {$define typ := pSint16}  f2o;
		                              {$ifdef Debug} else  raise UnsupportedCase(GLTypeIds[out_type], 'Vec2Vec.Out'); {$endif}
		end;
	{$undef f2o}
		FreeMem(fBuf);
	end;

	procedure Convert(inp: pointer; in_type: GLType; outp: pointer; out_type: GLType; count: sint = 1);
	begin
		Assert((in_type <> GLType.Sampler) and (out_type <> GLType.Sampler));
		if in_type = out_type then
		begin
			memcpy(inp, outp, size_t(count) * GLTypeInfo[in_type].sizeof);
			exit;
		end;
		if (in_type in [GLType.Uint32 .. GLType.Uint16]) and (out_type in [GLType.Uint32 .. GLType.Uint16]) then
			Int2Int(inp, in_type, outp, out_type, count)
		else
			if (GLTypeInfo[in_type].baseDim <> 0) and (GLTypeInfo[out_type].baseDim <> 0) then
				Vec2Vec(inp, in_type, outp, out_type, count)
		{$ifdef Debug} else raise UnsupportedCase(GLTypeIds[in_type] + '→' + GLTypeIds[out_type], 'GL.Convert') {$endif}
		;
	end;

	function GetTextureDataSize(const size: UintVec2; format: GLImageFormat): size_t;
	begin
		if not (GLformat_Compressed in GLImageFormatsInfo[format].flags) then
			result := size.Product * GLImageFormatsInfo[format].pixelSize
		else
			case format of
				GLformat_RGB_DXT1, GLformat_RGBA_DXT1: result := 8 * max(1, (size.x+3) div 4) * max(1, (size.y+3) div 4);
				GLformat_RGBA_DXT5: result := 16 * max(1, (size.x+3) div 4) * max(1, (size.y+3) div 4);
				else raise Error('Для {0} не реализован расчёт размера.', GLImageFormatIds[format]);
			end;
	end;

	function GetTextureDataSize(const size: UintVec3; format: GLImageFormat): size_t;
	begin
		result := GetTextureDataSize(size.xy, format) * size.z;
	end;

	function GetTextureDataSize(w, h: uint; format: GLImageFormat): size_t;
	begin
		result := GetTextureDataSize(UintVec2.Make(w, h), format);
	end;

	function GetTextureDataSize(w, h, d: uint; format: GLImageFormat): size_t;
	begin
		result := GetTextureDataSize(w, h, format) * d;
	end;

	function UniformComponents(ty: GLType): sint; begin result := GLTypeInfo[ty].baseDim * GLTypeInfo[GLTypeInfo[ty].baseType].baseDim; end;
	function RGB332(r, g, b: uint): uint8;        begin result := (r shl 5) or (g shl 2) or b; end;
	function RGB8(r, g, b: uint): Vec3u8;     begin result[0] := r; result[1] := g; result[2] := b; end;

	procedure Convert(inp: pointer; inFormat: GLImageFormat; outp: pointer; outFormat: GLImageFormat; const size: UintVec3);
	var
		z: uint;
	begin
		z := size.z;
		repeat
			Convert(inp, inFormat, outp, outFormat, size.xy);
			inp += GetTextureDataSize(size.xy, inFormat);
			outp += GetTextureDataSize(size.xy, outFormat);
			dec(z);
		until z = 0;
	end;

	// TODO:
	// либо автоматически строить граф для де-факто возможных, хотя и не заданных явно преобразований
	// либо преобразовывать всё через универсальный формат вроде RGBA32f
	// (эти два варианта могут работать вместе)
	procedure Convert(inp: pointer; inFormat: GLImageFormat; outp: pointer; outFormat: GLImageFormat; const size: UintVec2);

		function Unsupported: Exception;
		begin
			result := Error('Преобразование из ' + GLImageFormatIds[inFormat] + ' в ' + GLImageFormatIds[outFormat] + ' не поддерживается.');
		end;

	{$define impl8 :=
		var
			srcp, srcLast, dstp: pointer;
			typed_srcp: ^src_type absolute srcp; {$define src := typed_srcp^}
			typed_dstp: ^dst_type absolute dstp; {$define dst := typed_dstp^}
		begin
			srcp := inp;
			srcLast := inp + sizeof(src) * size.Product;
			dstp := outp;

			while srcp < srcLast do
			begin
				begin work end;
				srcp += sizeof(src);
				dstp += sizeof(dst);
			end;
		end; {$undef work} {$undef dst_type} {$undef src} {$undef dst}}

	{$define src_type := Vec3u8}
		procedure RGB_to_RGBA; {$define dst_type := Vec4u8} {$define work := Vec3u8(dstp^) := src; dst[3] := 255;} impl8
		procedure RGB_to_BGR; {$define dst_type := Vec3u8} {$define work := dst[0] := src[2]; dst[1] := src[1]; dst[2] := src[0];} impl8
		procedure RGB_to_BGRA; {$define dst_type := Vec4u8} {$define work := dst[0] := src[2]; dst[1] := src[1]; dst[2] := src[0]; dst[3] := 255;} impl8
		procedure RGB_to_RGB565; {$define dst_type := uint16}
			{$define work := dst := trunc(src[0]/255*31 + 0.5) shl 11 or trunc(src[1]/255*63 + 0.5) shl 5 or trunc(src[2]/255*31 + 0.5);} impl8
		procedure RGB_to_RGB332; {$define dst_type := uint8}
			{$define work := dst := RGB332(trunc(src[0]/255*7 + 0.5), trunc(src[1]/255*7 + 0.5), trunc(src[2]/255*3))} impl8

	{$define src_type := Vec4u8}
		procedure RGBA_to_RGB; {$define dst_type := Vec3u8} {$define work := dst := pVec3u8(srcp)^;} impl8
		procedure RGBA_to_BGR; {$define dst_type := Vec3u8} {$define work := dst[0] := src[2]; dst[1] := src[1]; dst[2] := src[0];} impl8
		procedure RGBA_to_BGRA; {$define dst_type := Vec4u8} {$define work := dst[0] := src[2]; dst[1] := src[1]; dst[2] := src[0]; dst[3] := src[3];} impl8
		procedure RGBA_to_RGBA4; {$define dst_type := uint16}
			{$define work := dst := trunc(src[0]/255*15 + 0.5) shl 8 or trunc(src[1]/255*15 + 0.5) shl 4 or trunc(src[2]/255*15 + 0.5) or
				trunc(src[3]/255*15 + 0.5) shl 12;} impl8

	{$define src_type := Vec2u8}
		procedure RG_to_RGB; {$define dst_type := Vec3u8} {$define work := Vec2u8(dstp^) := src; dst[2] := 0;} impl8
		procedure RG_to_BGR; {$define dst_type := Vec3u8} {$define work := dst[0] := 0; dst[1] := src[1]; dst[2] := src[0];} impl8
	{$define src_type := uint8}
		procedure R_to_RGB; {$define dst_type := Vec3u8} {$define work := dst[0] := src; dst[1] := 0; dst[2] := 0;} impl8
		procedure R_to_BGR; {$define dst_type := Vec3u8} {$define work := dst[0] := 0; dst[1] := 0; dst[2] := src;} impl8
	{$define src_type := uint16}
		procedure RGB565_to_RGB; {$define dst_type := Vec3u8} {$define work := DXT.RGB565_to_RGB8(src, dst);} impl8
		procedure RGBA4_to_RGBA; {$define dst_type := Vec4u8}
			{$define work :=
				dst[0] := trunc(uint(src shr 8 and %1111)/15*255+0.5);
				dst[1] := trunc(uint(src shr 4 and %1111)/15*255+0.5);
				dst[2] := trunc(uint(src and %1111)/15*255+0.5);
				dst[3] := trunc(uint(src shr 12)/15*255+0.5);} impl8
	{$undef src_type}
	{$undef impl8}

		procedure ConvertDXT;
		var
			decomp: GLImageFormat;
			tmp: pointer;
		begin
			decomp := ImageFormat8[GLImageFormatsInfo[inFormat].nChannels];
			if outFormat = decomp then tmp := outp else tmp := GetMem(GetTextureDataSize(size, decomp));
			try
				DXT.Decompress(inp, inFormat, size, tmp);
				if tmp <> outp then Convert(tmp, decomp, outp, outFormat, size);
			finally
				if tmp <> outp then FreeMem(tmp);
			end;
		end;

	begin
		Assert(inp <> outp);
		Assert(inFormat <> outFormat);
		case inFormat of
			GLformat_RGB:
				case outFormat of
					GLformat_RGBA: RGB_to_RGBA; GLformat_BGR: RGB_to_BGR; GLformat_BGRA: RGB_to_BGRA;
					GLformat_RGB565: RGB_to_RGB565;
					GLformat_RGB332: RGB_to_RGB332;
					else raise Unsupported;
				end;
			GLformat_RGBA:
				case outFormat of
					GLformat_RGB: RGBA_to_RGB; GLformat_BGR: RGBA_to_BGR; GLformat_BGRA: RGBA_to_BGRA;
					GLformat_RGBA4: RGBA_to_RGBA4;
					else raise Unsupported;
				end;
			GLformat_BGR: case outFormat of GLformat_RGB: RGB_to_BGR; GLformat_RGBA: RGB_to_BGRA; GLformat_BGRA: RGB_to_RGBA; else raise Unsupported; end;
			GLformat_BGRA: case outFormat of GLformat_RGB: RGBA_to_BGR; GLformat_BGR: RGBA_to_RGB; GLformat_RGBA: RGBA_to_BGRA; else raise Unsupported; end;
			GLformat_RG: case outFormat of GLformat_RGB: RG_to_RGB; GLformat_BGR: RG_to_BGR; else raise Unsupported; end;
			GLformat_R: case outFormat of GLformat_RGB: R_to_RGB; GLformat_BGR: R_to_BGR; else raise Unsupported; end;
			GLformat_RGB_DXT1, GLformat_RGBA_DXT1, GLformat_RGBA_DXT5: ConvertDXT;
			GLformat_RGBA4: case outFormat of GLformat_RGBA: RGBA4_to_RGBA; else raise Unsupported; end;
			GLformat_RGB565: case outFormat of GLformat_RGB: RGB565_to_RGB; else raise Unsupported; end;
			else raise Unsupported;
		end;
	end;

	procedure FlipY(src, dst: pointer; format: GLImageFormat; const size: UintSize3);
	var
		d: uint;
		rowSize, planeSize: size_t;
		a, b, tmpRow: pointer;
	begin
		if GLImageFormatsInfo[format].pixelSize > 0 then
		begin
			rowSize := size.x * GLImageFormatsInfo[format].pixelSize;
			planeSize := rowSize * size.y;

			if src = dst then tmpRow := GetMem(rowSize);
			for d := 1 to size.z do
			begin
				a := src;
				b := dst + planeSize;
				repeat
					b -= rowSize;
					if (b < dst) or (a = b) then break;
					if src = dst then begin memcpy(a, tmpRow, rowSize); memcpy(b, a, rowSize); memcpy(tmpRow, b, rowSize); end else memcpy(a, b, rowSize);
					a += rowSize;
				until a = b;

				src += planeSize;
				dst += planeSize;
			end;
			if src = dst then FreeMem(tmpRow);
		end else
			case format of
				GLformat_RGB_DXT1, GLformat_RGBA_DXT1, GLformat_RGBA_DXT5:
					begin
						planeSize := GetTextureDataSize(size.xy, format);
						for d := 1 to size.z do
						begin
							DXT.Flip(src, dst, format, size.xy);
							src += planeSize;
							dst += planeSize;
						end;
					end;
				else raise Error('Вертикальное отражение {0} не поддерживается.', GLImageFormatIds[format]);
			end;
	end;

	procedure Convert(inp: pointer; inFormat: GLImageFormat; outp: pointer; outFormat: GLImageFormat; w, h, d: uint);
	begin
		Convert(inp, inFormat, outp, outFormat, UintVec3.Make(w, h, d));
	end;

end.
