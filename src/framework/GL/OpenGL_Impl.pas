unit OpenGL_Impl;

{$include opts.inc}
{$ifdef Debug}
	{-$define ExtDebug}
	// проверка ошибок OpenGL на каждый чих — может быть медленной
	{-$define ExhaustiveErrorcheck}
{$endif}

{$ifdef ExhaustiveErrorcheck}
	{$define paranoia := CheckError}
{$else}
	{$define paranoia := //}
{$endif}

interface

uses
	ctypes, USystem, Utils, UClasses, Streams, UMath, U_GL, OpenGL_Headers, BlobCaches, ZlibHeaders, Algo
{$ifdef Debug}, Human, ULog {$endif} {$ifdef Profile}, Profile {$endif};

{$ifdef Debug}
type
	ResCounterEnum = (rc_Shader, rc_Buffer, rc_VertexDeclaration, rc_Program, rc_Texture, rc_Framebuffer);
const
	ResCounterNames: array[ResCounterEnum] of string =
		('шейдеров', 'буферов', 'VAO', 'шейдерных программ', 'текстур', 'фреймбуферов');
{$endif}

type
	pOpenGL = ^tOpenGL;
	tOpenGL = object(tGL)
	const
		MIN_MAJOR_VERSION = 2;
		MIN_MINOR_VERSION = 1;
		REC_MAJOR_VERSION = 4;
		REC_MINOR_VERSION = 4;
		MaxBinaryShaderCacheFailures = 2;
		ReasonableBinaryShaderSizeLimit = 1 * 1024 * 1024;
	var
		constructor Init;
		destructor Done; virtual;

	{$ifdef Debug}
	private type
		DebugOutputUid = record
			source, typ: gl.enum;
			id: uint;
		end;
	private var
		resCounter: array[ResCounterEnum] of record
			count, max: sint;
		end;
		debugSync: PendingSync;
		nDebugSeen: sint;
		debugSeen: array[0 .. 20] of DebugOutputUid;
		procedure NoteCreate(typ: ResCounterEnum);
		procedure NoteDestroy(typ: ResCounterEnum);
		function SuspiciousLog(const s: string): boolean; static;
	{$endif}
	private type
		MemoryUsedFunc = function(what: GLMemoryUsage; gl: pOpenGL): size_t;
	private var
		ver: record major, minor: sint; end;
		maxTextureAnisotropy: float;
		dsaSupported {$ifdef Debug}, debugOutputSupported {$endif} : boolean;
		oldRast: GLRasterizerState;
		memUsed: record
			get: MemoryUsedFunc;
			startFreeGPU, startFreeAux: size_t;
		end;
		_activeFramebuffer: GLHandle;
		_activeTextureUnit: sint;
		_boundTextures: array[0 .. MaxTextureUnits - 1] of GLHandle;
		_activeProgPtr: pGLProgram;
		_boundVertexArray: GLHandle;
		_blendSrc, _blendDst: gl.enum;
		_bb: array[GLBufferTarget] of GLHandle;
		_boundUbos: array[0 .. MaxUniformBuffers - 1] of GLHandle;
		viewport: record
			x, y, w, h: sint;
		end;
		_clearColor: Vec4;
		_primitiveRestartIndex: gl.uint;
		bsCache: BlobCache;
		bsCacheFailures: uint;
		bsLock: ThreadLock;
		bsPending: PendingSync;
		lzoOk: boolean;

		function CheckError {$ifdef Debug}(const cmdName: string = ''; warn: boolean = yes){$endif}: boolean;
		procedure _TexImage(var tex: tGLTexture; const size: UintVec3; dataFormat, intFormat: GLImageFormat; dataSize: size_t; data: pointer; level: uint);
		function _CheckVendorSpecific: boolean;
		function _CheckVersion(major, minor: sint): boolean;
		procedure _glBindTexture(texUnit: sint; target: GLTextureTarget; texID: GLHandle);
		procedure _glUseProgram(prog: pGLProgram);
		procedure _UseDrawingProgram(var prog: GLProgram);
		procedure _BindUBO(index: sint; id: GLHandle);
		procedure _glBindVertexArray(id: GLHandle);
		function BindBufferToOperate(target: GLBufferTarget; id: GLHandle): gl.enum;
		procedure _glBindFramebuffer(var rt: GLRenderTarget);
		function _GetProgramLog(var prog: GLProgram): string;
		procedure _GetUniforms(var prog: GLProgram; out u: GLUniformsList; out ub: GLUboList);
		procedure _GetVertexAttribs(var prog: GLProgram; out a: GLVertexAttribsList);
		procedure _UpdateDrawBuffers(var rt: GLRenderTarget);
		procedure _glViewport(x, y, w, h: sint);
		procedure _ApplyRasterizerState;
		procedure HandleBinaryShaderCacheFailure(exception: boolean);
	protected
		function _InitGL: boolean; virtual;
		procedure _DoneGL; virtual;
		procedure _OnEndFrame; virtual;
		function _MemoryUsed(what: GLMemoryUsage): size_t; virtual;

		procedure _Clear(var rt: GLRenderTarget; bufs: GLRenderBuffers; const clearColor: Vec4); virtual;
		procedure _DrawBatch(var rt: GLRenderTarget; var vd: GLVertexDeclaration; var prog: GLProgram; ibOffset: PtrUint; nIndices: uint; indexType: GLType; nInstances: uint); virtual;

		procedure _CreateVertexDeclaration(var vd: GLVertexDeclaration; var ib: GLBuffer); virtual;
		procedure _DeleteVertexDeclaration(var vd: GLVertexDeclaration); virtual;
		procedure _SetVertexAttribute(var vd: GLVertexDeclaration; index: sint; type_: GLType; stride: size_t; vbOffset: PtrUint); virtual;

		procedure _CreateBuffer(var buf: GLBuffer); virtual;
		procedure _DeleteBuffer(var buf: GLBuffer); virtual;
		procedure _BufferData(var buf: GLBuffer; size: size_t; data: pointer; usage: GLBufferUsage); virtual;
		procedure _BufferSubData(var buf: GLBuffer; offset: PtrUint; size: size_t; data: pointer); virtual;

		procedure _CreateShader(var sh: GLShader; type_: ShaderType; const name, source: string); virtual;
		procedure _DeleteShader(var sh: GLShader); virtual;

		function _CreateProgram(var prog: GLProgram; const namex: string; const sh: array of pGLShader; out info: ShaderEntrails): boolean; virtual;
		procedure _DeleteProgram(var prog: GLProgram); virtual;
		procedure _SetUniform(var prog: GLProgram; var u: GLUniform; value: pointer; count: uint); virtual;
		procedure _SetUbos(var prog: GLProgram; const bufs: array of pGLBuffer); virtual;

		procedure _CreateTexture(out tex: tGLTexture; target: GLTextureTarget); virtual;
		procedure _DeleteTexture(var tex: tGLTexture); virtual;
		procedure _TexImage(var tex: tGLTexture; const size: UintVec3; format: GLImageFormat; dataSize: size_t; data: pointer; level: uint); virtual;
		procedure _TexSubImage(var tex: tGLTexture; const offset, size: UintVec3; format: GLImageFormat; dataSize: size_t; data: pointer; level: uint); virtual;
		procedure _SetTextureParams(var tex: tGLTexture; const params: GLTextureParamsRec; paramFields: GLTextureParamSet); virtual;
		function _GetTexImage(var tex: tGLTexture; level: uint; format: GLImageFormat): pointer; virtual;

		procedure _CreateRenderTarget(var fb: GLRenderTarget); virtual;
		procedure _DeleteRenderTarget(var fb: GLRenderTarget); virtual;
		procedure _ResizeRenderTarget(var fb: GLRenderTarget; const size: UintVec2); virtual;
		procedure _AttachRenderTexture(var fb: GLRenderTarget; var tex: tGLTexture; level: uint; target: GLRenderBuffer; targetN: uint); virtual;
		procedure _DetachRenderTexture(var fb: GLRenderTarget; target: GLRenderBuffer; targetN: uint); virtual;
		function _ValidateRenderTarget(var fb: GLRenderTarget): GLRenderTargetStatus; virtual;
		function _GetRTImage(var fb: GLRenderTarget; format: GLImageFormat; mrtN: uint): pointer; virtual;
	end;

const
	GLFormats: array[GLImageFormat] of
		record
			components, internalFormat: gl.enum;
			ctype: gl.enum; // для (Get)TexImage
		end =
	(
		( // GLformat_R
			components: gl.RED; internalFormat: gl.R8;
			ctype: gl.UNSIGNED_BYTE
		),
		( // GLformat_RG
			components: gl.RG; internalFormat: gl.RG8;
			ctype: gl.UNSIGNED_BYTE
		),
		( // GLformat_RGB
			components:  gl.RGB; internalFormat: gl.RGB8;
			ctype: gl.UNSIGNED_BYTE
		),
		( // GLformat_RGBA
			components: gl.RGBA; internalFormat: gl.RGBA8;
			ctype: gl.UNSIGNED_BYTE
		),
		( // GLformat_RGB_DXT1
			components: gl.RGB; internalFormat: gl.COMPRESSED_RGB_S3TC_DXT1;
			ctype: gl.UNSIGNED_BYTE
		),
		( // GLformat_RGBA_DXT1
			components: gl.RGBA; internalFormat: gl.COMPRESSED_RGBA_S3TC_DXT1;
			ctype: gl.UNSIGNED_BYTE
		),
		( // GLformat_RGBA_DXT5
			components: gl.RGBA; internalFormat: gl.COMPRESSED_RGBA_S3TC_DXT5;
			ctype: gl.UNSIGNED_BYTE
		),
		( // GLformat_R_RGTC1
			components: gl.RED; internalFormat: gl.COMPRESSED_RED_RGTC1;
			ctype: gl.UNSIGNED_BYTE
		),
		( // GLformat_RG_RGTC2
			components: gl.RG; internalFormat: gl.COMPRESSED_RG_RGTC2;
			ctype: gl.UNSIGNED_BYTE
		),
		( // GLformat_R16f
			components: gl.RED; internalFormat: gl.R16F;
			ctype: gl.HALF_FLOAT
		),
		( // GLformat_RG16f
			components: gl.RG; internalFormat: gl.RG16F;
			ctype: gl.HALF_FLOAT
		),
		( // GLformat_RGB16f
			components: gl.RGB; internalFormat: gl.RGB16F;
			ctype: gl.HALF_FLOAT
		),
		( // GLformat_RGBA16f
			components: gl.RGBA; internalFormat: gl.RGBA16F;
			ctype: gl.HALF_FLOAT
		),
		( // GLformat_R32f
			components: gl.RED; internalFormat: gl.R32F;
			ctype: gl.FLOAT_TYPE
		),
		( // GLformat_RG32f
			components: gl.RG; internalFormat: gl.RG32F;
			ctype: gl.FLOAT_TYPE
		),
		( // GLformat_RGB32f
			components: gl.RGB; internalFormat: gl.RGB32F;
			ctype: gl.FLOAT_TYPE
		),
		( // GLformat_RGBA32f
			components: gl.RGBA; internalFormat: gl.RGBA32F;
			ctype: gl.FLOAT_TYPE
		),
		( // GLformat_RGBA4
			components: gl.RGBA; internalFormat: gl.RGBA;
			ctype: gl.UNSIGNED_SHORT_4_4_4_4
		),
		( // GLformat_RGB565
			components: gl.RGB; internalFormat: gl.RGB;
			ctype: gl.UNSIGNED_SHORT_5_6_5
		),
		( // GLformat_RGB5
			components: gl.RGBA; internalFormat: gl.RGB5;
			ctype: gl.UNSIGNED_SHORT_5_5_5_1
		),
		( // GLformat_RGB5A1
			components: gl.RGBA; internalFormat: gl.RGB5_A1;
			ctype: gl.UNSIGNED_SHORT_5_5_5_1
		),
		( // GLformat_A1RGB5
			components: gl.BGRA; internalFormat: gl.RGB5_A1;
			ctype: gl.UNSIGNED_SHORT_1_5_5_5_REV
		),
		( // GLformat_BGR
			components:  gl.BGR; internalFormat: gl.RGB8;
			ctype: gl.UNSIGNED_BYTE
		),
		( // GLformat_BGRA
			components: gl.BGRA; internalFormat: gl.RGBA8;
			ctype: gl.UNSIGNED_BYTE
		),
		( // GLformat_RGB332
			components: gl.RGB; internalFormat: gl.R3_G3_B2;
			ctype: gl.UNSIGNED_BYTE_3_3_2
		),
		( // GLformat_Depth
			components: gl.DEPTH_COMPONENT; internalFormat: gl.DEPTH_COMPONENT24;
			ctype: gl.FLOAT_TYPE
		)
	);

	GLAsAttrib: array[GLType] of record
		enum: gl.enum;
		count: sint;
		normalized: gl.enum;
	end =
	(
		(enum: gl.FLOAT_TYPE;     count: 1; normalized: gl.FALSE), // GLtype_Float
		(enum: gl.FLOAT_TYPE;     count: 2; normalized: gl.FALSE), // GLtype_Vec2
		(enum: gl.FLOAT_TYPE;     count: 3; normalized: gl.FALSE), // GLtype_Vec3
		(enum: gl.FLOAT_TYPE;     count: 4; normalized: gl.FALSE), // GLtype_Vec4
		(enum: gl.FLOAT_MAT4;     count: 1; normalized: gl.FALSE), // GLtype_Mat4
		(enum: gl.UNSIGNED_INT;   count: 1; normalized: gl.FALSE), // GLtype_Uint32
		(enum: gl.UNSIGNED_BYTE;  count: 1; normalized: gl.FALSE), // GLtype_Ubyte
		(enum: gl.UNSIGNED_SHORT; count: 1; normalized: gl.FALSE), // GLtype_Uint16
		(enum: 0;                 count: -1; normalized: gl.FALSE), // GLtype_Sampler
		(enum: gl.SIGNED_SHORT;   count: 1; normalized: gl.TRUE), // GLtype_Ni16
		(enum: gl.SIGNED_SHORT;   count: 2; normalized: gl.TRUE), // GLtype_Vec2Ni16
		(enum: gl.SIGNED_SHORT;   count: 3; normalized: gl.TRUE), // GLtype_Vec3Ni16
		(enum: gl.SIGNED_SHORT;   count: 4; normalized: gl.TRUE), // GLtype_Vec4Ni16
		(enum: gl.SIGNED_BYTE;    count: 1; normalized: gl.TRUE), // GLtype_Ni8
		(enum: gl.SIGNED_BYTE;    count: 2; normalized: gl.TRUE), // GLtype_Vec2Ni8
		(enum: gl.SIGNED_BYTE;    count: 3; normalized: gl.TRUE), // GLtype_Vec3Ni8
		(enum: gl.SIGNED_BYTE;    count: 4; normalized: gl.TRUE), // GLtype_Vec4Ni8
		(enum: gl.HALF_FLOAT;     count: 1; normalized: gl.FALSE), // GLtype_Half
		(enum: gl.HALF_FLOAT;     count: 2; normalized: gl.FALSE), // GLtype_Vec2Half
		(enum: gl.HALF_FLOAT;     count: 3; normalized: gl.FALSE), // GLtype_Vec3Half
		(enum: gl.HALF_FLOAT;     count: 4; normalized: gl.FALSE), // GLtype_Vec4Half
		(enum: gl.UNSIGNED_SHORT; count: 1; normalized: gl.TRUE), // GLtype_Nui16
		(enum: gl.UNSIGNED_SHORT; count: 2; normalized: gl.TRUE), // GLtype_Vec2Nui16
		(enum: gl.UNSIGNED_SHORT; count: 3; normalized: gl.TRUE), // GLtype_Vec3Nui16
		(enum: gl.UNSIGNED_SHORT; count: 4; normalized: gl.TRUE), // GLtype_Vec4Nui16
		(enum: gl.UNSIGNED_BYTE;  count: 1; normalized: gl.TRUE), // GLtype_Nui8
		(enum: gl.UNSIGNED_BYTE;  count: 2; normalized: gl.TRUE), // GLtype_Vec2Nui8
		(enum: gl.UNSIGNED_BYTE;  count: 3; normalized: gl.TRUE), // GLtype_Vec3Nui8
		(enum: gl.UNSIGNED_BYTE;  count: 4; normalized: gl.TRUE), // GLtype_Vec4Nui8
		(enum: gl.SIGNED_INT;     count: 1; normalized: gl.FALSE), // GLtype_Int32
		(enum: gl.SIGNED_SHORT;   count: 1; normalized: gl.FALSE), // GLtype_Int16
		(enum: gl.SIGNED_BYTE;    count: 1; normalized: gl.FALSE), // GLtype_Int8
		(enum: gl.UNSIGNED_BYTE;  count: 2; normalized: gl.FALSE), // GLtype_Vec2ui8
		(enum: gl.UNSIGNED_BYTE;  count: 3; normalized: gl.FALSE), // GLtype_Vec3ui8
		(enum: gl.UNSIGNED_BYTE;  count: 4; normalized: gl.FALSE), // GLtype_Vec4ui8
		(enum: gl.UNSIGNED_SHORT; count: 2; normalized: gl.FALSE), // GLtype_Vec2ui16
		(enum: gl.UNSIGNED_SHORT; count: 3; normalized: gl.FALSE), // GLtype_Vec3ui16
		(enum: gl.UNSIGNED_SHORT; count: 4; normalized: gl.FALSE), // GLtype_Vec4ui16
		(enum: gl.SIGNED_BYTE;    count: 2; normalized: gl.FALSE), // GLtype_Vec2i8
		(enum: gl.SIGNED_BYTE;    count: 3; normalized: gl.FALSE), // GLtype_Vec3i8
		(enum: gl.SIGNED_BYTE;    count: 4; normalized: gl.FALSE), // GLtype_Vec4i8
		(enum: gl.SIGNED_SHORT;   count: 2; normalized: gl.FALSE), // GLtype_Vec2i16
		(enum: gl.SIGNED_SHORT;   count: 3; normalized: gl.FALSE), // GLtype_Vec3i16
		(enum: gl.SIGNED_SHORT;   count: 4; normalized: gl.FALSE)  // GLtype_Vec4i16
	);

	SwizzleEnums: array[Swizzle] of gl.enum = (gl.RED, gl.GREEN, gl.BLUE, gl.ALPHA, gl.ZERO, gl.ONE);

implementation

uses
	MMSystem, GLBase;

type
	gl = OpenGL_Headers.gl;

const
	GLBoolEnum: array[boolean] of gl.enum = (gl.FALSE, gl.TRUE);
	GLTypeEnums: array[GLType] of gl.enum =
	(
		gl.FLOAT_TYPE, gl.FLOAT_VEC2, gl.FLOAT_VEC3, gl.FLOAT_VEC4,
		gl.FLOAT_MAT4,
		gl.UNSIGNED_INT, gl.UNSIGNED_BYTE, gl.UNSIGNED_SHORT,
		0,
		gl.SIGNED_SHORT, gl.SIGNED_SHORT, gl.SIGNED_SHORT, gl.SIGNED_SHORT,
		gl.SIGNED_BYTE, gl.SIGNED_BYTE, gl.SIGNED_BYTE, gl.SIGNED_BYTE,
		gl.HALF_FLOAT, gl.HALF_FLOAT, gl.HALF_FLOAT, gl.HALF_FLOAT,
		gl.UNSIGNED_SHORT, gl.UNSIGNED_SHORT, gl.UNSIGNED_SHORT, gl.UNSIGNED_SHORT,
		gl.UNSIGNED_BYTE, gl.UNSIGNED_BYTE, gl.UNSIGNED_BYTE, gl.UNSIGNED_BYTE,
		gl.SIGNED_INT, gl.SIGNED_SHORT, gl.SIGNED_BYTE,
		gl.UNSIGNED_BYTE, gl.UNSIGNED_BYTE, gl.UNSIGNED_BYTE,
		gl.UNSIGNED_SHORT, gl.UNSIGNED_SHORT, gl.UNSIGNED_SHORT,
		gl.SIGNED_BYTE, gl.SIGNED_BYTE, gl.SIGNED_BYTE,
		gl.SIGNED_SHORT, gl.SIGNED_SHORT, gl.SIGNED_SHORT
	);

	GLTextureTargetEnums: array[GLTextureTarget] of gl.enum =
	(
		gl.TEXTURE_1D, gl.TEXTURE_2D, gl.TEXTURE_3D,
		gl.TEXTURE_CUBE_MAP
	);

	GLBufferTargetEnums: array[GLBufferTarget] of gl.enum =
	(
		gl.ARRAY_BUFFER, gl.ELEMENT_ARRAY_BUFFER, gl.UNIFORM_BUFFER
	);

	GLCubeSideEnums: array[GLCubeSide] of gl.enum =
	(
		gl.TEXTURE_CUBE_MAP_NEGATIVE_X, gl.TEXTURE_CUBE_MAP_POSITIVE_X,
		gl.TEXTURE_CUBE_MAP_NEGATIVE_Y, gl.TEXTURE_CUBE_MAP_POSITIVE_Y,
		gl.TEXTURE_CUBE_MAP_NEGATIVE_Z, gl.TEXTURE_CUBE_MAP_POSITIVE_Z
	);

	GLRenderBufferEnums: array[GLRenderBuffer] of gl.enum =
	(
		gl.COLOR_ATTACHMENT0, gl.DEPTH_ATTACHMENT
	);

	function GetInteger(enum: gl.enum): gl.int;
	var
		iv: gl.int;
	begin
		gl.GetError();
		iv := 0;
		gl.GetIntegerv(enum, @iv);
		if gl.GetError() = gl.NO_ERROR then
			result := iv
		else
			result := 0;
	end;

	function GetFloat(enum: gl.enum): float;
	var
		fv: GLfloat;
	begin
		gl.GetError;
		fv := 0;
		gl.GetFloatv(enum, @fv);
		if gl.GetError() = gl.NO_ERROR then
			result := fv
		else
			result := 0;
	end;

	function tOpenGL.CheckError {$ifdef Debug}(const cmdName: string = ''; warn: boolean = yes){$endif}: boolean;
	var
		errEnum: gl.enum;
	{$ifdef Debug} msg: string; {$endif}
	begin
		errEnum := gl.GetError();
		result := errEnum <> gl.NO_ERROR;
	{$ifdef Debug}
		if result and warn then
		begin
			msg := IfThen(length(cmdName) > 0, cmdName, 'OpenGL') + ' вернула ошибку ' +
				IfThen(length(cmdName) > 0, 'OpenGL ') + gl.DescribeErrorInline(errEnum) + '.';
			Log(msg, logError);
			_Error(msg);
		end;
	{$endif}
	end;

	constructor tOpenGL.Init;
	var
		i: sint;
		bt: GLBufferTarget;
	{$ifdef Debug} rc: ResCounterEnum; {$endif}
	begin
		inherited Init;
		dsaSupported := no;
	{$ifdef Debug} debugOutputSupported := no; {$endif}
		oldRast := _rast;
		memUsed.get := nil;
		_activeTextureUnit := 0;
		for i := Low(_boundTextures) to High(_boundTextures) do
			_boundTextures[i] := 0;
		_activeProgPtr := nil;
		_boundVertexArray := 0;
		_blendSrc := gl.ONE;
		_blendDst := gl.ZERO;
		for bt in GLBufferTarget do
			_bb[bt] := 0;
		for i := 0 to MaxUniformBuffers - 1 do
			_boundUbos[i] := 0;
		viewport.x := 0;
		viewport.y := 0;
		viewport.w := 1;
		viewport.h := 1;
		_clearColor := Vec4.Zero;
	{$ifdef Debug}
		for rc in ResCounterEnum do
		begin
			resCounter[rc].count := 0;
			resCounter[rc].max := 0;
		end;
	{$endif}
		bsCache := bsCache.Invalid;
		bsCacheFailures := 0;
		bsLock.Init;
		bsPending.Init;

		lzoOk := yes;
		try lzo.loader.Load; except lzoOk := no; end;
	end;

	destructor tOpenGL.Done;
	begin
		bsPending.Done;
		if lzoOk then lzo.loader.Unload;
		bsLock.Done;
		bsCache.Close;
		inherited Done;
	end;

	function MemoryUsed_NV(what: GLMemoryUsage; gl: pOpenGL): size_t;
	var
		avail: gl.int;
	begin
		result := 0;
		if what = GLmem_Aux then exit;
		avail := GetInteger(OpenGL_Headers.gl.GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX);
		if (avail < 0) or (gl^.memUsed.startFreeGPU < size_t(avail)) then exit;
		result := (gl^.memUsed.startFreeGPU - avail) * 1024;
	end;

// Из спецификации ATI_meminfo:
// GetIntegerv с *_FREE_MEMORY_ATI записывает 4-мерный вектор интов.
// param[0] - total memory free in the pool
// param[1] - largest available free block in the pool
// param[2] - total auxiliary memory free
// param[3] - largest auxiliary free block
// Единица измерения - килобайты.

	function MemoryUsed_ATI(what: GLMemoryUsage; gl: pOpenGL): size_t;
	var
		iv4: packed array[0 .. 3] of gl.int;
		t: size_t;
	begin
		OpenGL_Headers.gl.GetIntegerv(OpenGL_Headers.gl.TEXTURE_FREE_MEMORY_ATI, @iv4[0]);
		t := 0;
		result := 0;
		if what in [GLmem_Total, GLmem_GPU] then
		begin
			if (iv4[0] < 0) or (gl^.memUsed.startFreeGPU < size_t(iv4[0])) then exit;
			t := ilong(t) + gl^.memUsed.startFreeGPU - size_t(iv4[0]);
		end;
		if what in [GLmem_Total, GLmem_Aux] then
		begin
			if (iv4[2] < 0) or (gl^.memUsed.startFreeAux < size_t(iv4[2])) then exit;
			t := ilong(t) + gl^.memUsed.startFreeAux - size_t(iv4[2]);
		end;
		result := t * 1024;
	end;

	function tOpenGL._MemoryUsed(what: GLMemoryUsage): size_t;
	begin
		if Assigned(memUsed.get) then result := memUsed.get(what, @self) else result := 0;
	end;

	function tOpenGL._CheckVersion(major, minor: sint): boolean;
	begin
		result := (ver.major > major) or ((ver.major = major) and (ver.minor >= minor));
	end;

	function tOpenGL._CheckVendorSpecific: boolean;
	label _finally_;
{$ifdef Debug}
	const
		requiredValues: array[1 .. 12] of record
			valueEnum: gl.enum;
			name: string;
		end =
		(
			(valueEnum: gl.MAX_VERTEX_ATTRIBS;              name: 'MAX_VERTEX_ATTRIBS'),
			(valueEnum: gl.MAX_VERTEX_UNIFORM_COMPONENTS;   name: 'MAX_VERTEX_UNIFORM_COMPONENTS'),
			(valueEnum: gl.MAX_FRAGMENT_UNIFORM_COMPONENTS; name: 'MAX_FRAGMENT_UNIFORM_COMPONENTS'),
			(valueEnum: gl.MAX_VERTEX_OUTPUT_COMPONENTS;    name: 'MAX_VERTEX_OUTPUT_COMPONENTS'),
			(valueEnum: gl.MAX_FRAGMENT_INPUT_COMPONENTS;   name: 'MAX_FRAGMENT_INPUT_COMPONENTS'),
			(valueEnum: gl.MAX_TEXTURE_IMAGE_UNITS;         name: 'MAX_TEXTURE_IMAGE_UNITS'),
			(valueEnum: gl.MAX_DRAW_BUFFERS;                name: 'MAX_DRAW_BUFFERS'),
			(valueEnum: gl.MAX_UNIFORM_BUFFER_BINDINGS;     name: 'MAX_UNIFORM_BUFFER_BINDINGS'),
			(valueEnum: gl.MAX_UNIFORM_BLOCK_SIZE;          name: 'MAX_UNIFORM_BLOCK_SIZE'),
			(valueEnum: gl.MAX_GEOMETRY_OUTPUT_VERTICES;    name: 'MAX_GEOMETRY_OUTPUT_VERTICES'),
			(valueEnum: gl.MAX_GEOMETRY_SHADER_INVOCATIONS; name: 'MAX_GEOMETRY_SHADER_INVOCATIONS'),
			(valueEnum: gl.MAX_ARRAY_TEXTURE_LAYERS;        name: 'MAX_ARRAY_TEXTURE_LAYERS')
		);
{$endif}
		function ExtractFourDigits(const s: string): sint;
		const
			Digits = ['0'..'9'];
		var
			i: sint;
		begin
			for i := 1 to length(s) - 3 do
				if ((i = 1) or not (s[i - 1] in Digits)) and (s[i] in Digits) and (s[i + 1] in Digits) and (s[i + 2] in Digits) and (s[i + 3] in Digits)
					and ((i + 4 > length(s)) or not (s[i + 4] in Digits))
				then
					exit(StrToInt(copy(s, i, 4)));
			result := -1;
		end;
		function RadeonLt(const renderer: string; four: sint): boolean;
		begin
			result := (Pos('Radeon', renderer) > 0) and (ExtractFourDigits(renderer) < four);
		end;
		function Intel(const vendor, renderer: string): boolean;
		begin
			result := (Pos('Intel', vendor) > 0) or (Pos('Intel', renderer) > 0);
		end;
		function ThisCardHandlesNewFloatsTooSlow(const renderer: string): boolean;
		begin
			result := RadeonLt(renderer, 6000);
		end;
		function ThisCardDislikesUbyteIndices(const renderer: string): boolean;
		begin
			result := RadeonLt(renderer, 7000);
		end;
		function UboMaySuck(const vendor, renderer: string): boolean;
		begin
			result :=
				((not _CheckVersion(3, 2)) and RadeonLt(renderer, 6500)) or Intel(vendor, renderer);
		end;
		function CheckInteger(enum: gl.enum; atLeast: sint {$ifdef Debug}; const name: string {$endif}): boolean;
		var
			x: sint;
		begin
			x := GetInteger(enum);
			result := x >= atLeast;
		{$ifdef Debug}
			if not result then
				if x > 0 then
					Log('Недостаточно ' + name + ': ' + ToString(x) + ' (требуется как минимум ' + ToString(atLeast) + '). ' +
						'Апгрейднись или дай мне знать.', logError);
		{$endif}
		end;

		function FindSuffix(const exts: array of string; what: string): boolean;
		var
			i: sint;
		begin
			for i := 0 to High(exts) do
				if IsSuffix(what, exts[i]) then
					exit(yes);
			result := no;
		end;

	var
		extremelyOld, goddamnedVendor, ATI: boolean;
		exts: array of string;
		vendor, renderer, glverrep, glverdot, slverrep, slverdot, extss: string;
		iv4: packed array[0 .. 3] of gl.int;
		i, slvern {$ifdef Debug}, j {$endif}: sint;
	begin
		result := no;
		if not (Assigned(gl.GetString) and Assigned(gl.GetIntegerv) and Assigned(gl.GetFloatv) and Assigned(gl.GetError)) then
		begin
			{$ifdef Debug} Log('Нет даже базовых функций OpenGL', logError); {$endif}
			Error.Show('Нет даже базовых функций OpenGL. WTF?!');
			goto _finally_;
		end;
		glverrep := gl.GetString(gl.VERSION);
		slverrep := gl.GetString(gl.SHADING_LANGUAGE_VERSION);
		vendor := gl.GetString(gl.VENDOR);
		renderer := gl.GetString(gl.RENDERER);

		if Assigned(gl.GetStringi) then
		begin
			SetLength(exts, GetInteger(gl.NUM_EXTENSIONS));
			for i := 0 to High(exts) do
				exts[i] := gl.GetStringi(gl.EXTENSIONS, i);
		{$ifdef Debug} extss := SeparatedList.Join(exts, ' '); {$endif}
		end else
		begin
			exts := nil;
			extss := gl.GetString(gl.EXTENSIONS);
			i := 1;
			repeat
				SetLength(exts, length(exts) + 1);
				exts[High(exts)] := ScanToken(extss, i);
			until exts[High(exts)] = '';
			SetLength(exts, length(exts) - 1);
		end;

		if Config.forceGLver <> '' then
		begin
		{$ifdef Debug} Log('Форсирована версия OpenGL ' + Config.forceGLver, logWarning); {$endif}
			glverdot := Config.forceGLver;
		end else
			glverdot := glverrep;

		i := 1;
		ver.major := StrToInt(ScanToken(glverdot, i, ['0'..'9']), 1);
		ver.minor := StrToInt(ScanToken(glverdot, i, ['0'..'9']));

		if Config.forceSLver <> '' then
		begin
		{$ifdef Debug} Log('Форсирована версия GLSL ' + Config.forceSLver, logWarning); {$endif}
			slverdot := Config.forceSLver;
		end else
			slverdot := slverrep;

		i := 1;
		_slVersion := ScanToken(slverdot, i, ['0'..'9']);
		_slVersion += ScanToken(slverdot, i, ['0'..'9']);
		slvern := StrToInt(_slVersion, 100);

		extremelyOld := not _CheckVersion(MIN_MAJOR_VERSION, MIN_MINOR_VERSION);
		if extremelyOld then
		begin
		{$ifdef Debug} Log('СЛИШКОМ старая версия OpenGL (' + glverdot + ').', logError); {$endif}
			if Warning.Text('Твоя версия OpenGL (' + glverdot + ') СЛИШКОМ старая. Рекомендуется как минимум ' +
				ToString(MIN_MAJOR_VERSION) + '.' + ToString(MIN_MINOR_VERSION) + '.')
				.Variant('Продолжить').Variant('Провалить инициализацию').Show <> TaskV1
			then
				goto _finally_;
		end;

		goddamnedVendor := no;
		if Intel(vendor, renderer) then
		begin
		{$ifdef Debug} Log('Интел вместо видеокарты => никаких фокусов с новыми фичами', logWarning); {$endif}
			goddamnedVendor := yes;
			_intelVendor := yes;
		end;
		ATI := (Pos('ATI', vendor) > 0) or (Pos('AMD', vendor) > 0);

		_csmRecommended := _CheckVersion(3, 0) and not goddamnedVendor;
		_wiresMustMatch := mm.window.ForwardGL and _CheckVersion(3, 1);
		_uniForSupported := (slvern >= 140) and not goddamnedVendor;
		_inoutSupported := slvern >= 130;
		_overloadedTextureFetch := slvern >= 130;
		_shouldSetPrecisionExplicitly := ATI and _inoutSupported;
		_geometryShaderSupported := _CheckVersion(3, 2) or FindSuffix(exts, 'geometry_shader');
		_binaryShadersSupported := Assigned(gl.ProgramBinary);
		dsaSupported := FindSuffix(exts, 'direct_state_access') and Assigned(gl.ProgramUniform1i);
	{$ifdef Debug} debugOutputSupported := Assigned(gl.DebugMessageCallback); {$endif}

	{$ifdef Debug}
		Log('Вендор: ' + vendor);
		Log('Рендерер: ' + renderer);
		Log('Версия GL: ' + glverrep + ' (принята ' + ToString(ver.major) + '.' + ToString(ver.minor) + ')', logDebug);
		if (not _CheckVersion(REC_MAJOR_VERSION, REC_MINOR_VERSION)) and (not extremelyOld) then
			Log('Слишком старая версия OpenGL ' + ToString(ver.major) + '.' + ToString(ver.minor) + ', рекомендуется как минимум ' +
				ToString(REC_MAJOR_VERSION) + '.' + ToString(REC_MINOR_VERSION) + '. Некоторые фичи будут выключены, ' +
				'корректная работа не гарантируется! Обнови драйверы / проапгрейди конфигурацию.', logWarning);
		Log('Версия GLSL: ' + slverrep + ' (принята ' + ToString(slvern) + ')');
		for i := 1 to length(requiredValues) do
		begin
			j := GetInteger(requiredValues[i].valueEnum);
			if j > 0 then
				Log(requiredValues[i].name + ' = ' + ToString(j))
			else
				Log(requiredValues[i].name + ' = ' + ToString(j), logWarning);
		end;
		Log('Расширения (' + ToString(length(exts)) + '): ' + extss);

		_LogSupUnsup(GLBase.Config.allowDsa, dsaSupported, 'Direct State Access (DSA): ', 'включен', 'не поддерживается', 'выключен', 'выключен, да и не поддерживается');
		_LogSupUnsup(mm.window.DebugGL, debugOutputSupported, 'Debug Output: ', 'включен', 'не поддерживается', 'выключен', 'выключен, да и не поддерживается');
	{$endif}

		dsaSupported := dsaSupported and GLBase.Config.allowDsa;
	{$ifdef Debug} debugOutputSupported := debugOutputSupported and mm.window.DebugGL; {$endif}

		_maxUniforms := Min(GetInteger(gl.MAX_VERTEX_UNIFORM_COMPONENTS), GetInteger(gl.MAX_FRAGMENT_UNIFORM_COMPONENTS));
		CheckInteger(gl.MAX_VERTEX_ATTRIBS, MaxVertexAttributes {$ifdef Debug}, 'MAX_VERTEX_ATTRIBS' {$endif});
		CheckInteger(gl.MAX_TEXTURE_IMAGE_UNITS, MaxTextureUnits {$ifdef Debug}, 'MAX_TEXTURE_IMAGE_UNITS' {$endif});

		if Assigned(gl.DrawElementsInstanced) then _maxInstances := High(_maxInstances);
		_UBOSupported := Assigned(gl.UniformBlockBinding) and not goddamnedVendor;
		if _UBOSupported and UboMaySuck(vendor, renderer) then
		begin
		{$ifdef Debug} Log('От греха подальше отключаю UBO.', logWarning); {$endif}
			_UBOSupported := no;
		end;
		_primitiveRestartSupported := Assigned(gl.PrimitiveRestartIndex);
		maxTextureAnisotropy := GetFloat(gl.MAX_TEXTURE_MAX_ANISOTROPY);
	{$ifdef Debug} Log('Макс. анизотропия текстуры: ' + ToString(maxTextureAnisotropy) + 'x'); {$endif}

		if _UBOSupported then CheckInteger(gl.MAX_UNIFORM_BUFFER_BINDINGS, MaxUniformBuffers {$ifdef Debug}, 'MAX_UNIFORM_BUFFER_BINDINGS' {$endif});

		_advFloatsRecommended := not ThisCardHandlesNewFloatsTooSlow(renderer);
		_ubyteIdsRecommended := not ThisCardDislikesUbyteIndices(renderer);
		_vaAlignment := 4;

		if FindSuffix(exts, 'NVX_gpu_memory_info') then
		begin
		{$ifdef Debug} Log('Источник информации о видеопамяти - ''NVX_gpu_memory_info''', logOK); {$endif}
			gl.GetIntegerv(gl.GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX, @iv4[0]);
			memUsed.startFreeGPU := max(iv4[0], 0);
		{$ifdef Debug} Log('Видеопамяти свободно: ' + ToStringSuff_b(1024 * hp_float(memUsed.startFreeGPU)), logDebug); {$endif}
			memUsed.get := @MemoryUsed_NV;
		end else
		if FindSuffix(exts, 'ATI_meminfo') then
		begin
		{$ifdef Debug} Log('Источник информации о видеопамяти - ''ATI_meminfo''', logOK); {$endif}
			gl.GetIntegerv(gl.TEXTURE_FREE_MEMORY_ATI, @iv4[0]);
			memUsed.startFreeGPU := max(iv4[0], 0);
			memUsed.startFreeAux := max(iv4[2], 0);
		{$ifdef Debug}
			Log('Видеопамяти свободно: ' + ToStringSuff_b(1024 * hp_float(memUsed.startFreeGPU)) +
				'; + доп. ' + ToStringSuff_b(1024 * hp_float(memUsed.startFreeAux)), logDebug);
		{$endif}
			memUsed.get := @MemoryUsed_ATI;
		end {$ifdef Debug} else Log('Информация о видеопамяти недоступна', logWarning) {$endif};

		result := yes;
	{$ifdef ExhaustiveErrorcheck} Log('gl.GetError вызывается на каждый чих! Выключи, если не нужно.', logWarning); {$endif}
	_finally_:
		paranoia('CheckVendorSpecific');
	end;

{$ifdef Debug}
	procedure tOpenGL.NoteCreate(typ: ResCounterEnum);
	begin
		inc(resCounter[typ].count);
		if resCounter[typ].count > resCounter[typ].max then resCounter[typ].max := resCounter[typ].count;
	end;

	procedure tOpenGL.NoteDestroy(typ: ResCounterEnum);
	begin
		dec(resCounter[typ].count);
	end;

type
	pDebugTaskParam = ^tDebugTaskParam;
	tDebugTaskParam = record
		gl: pOpenGL;
		uid: tOpenGL.DebugOutputUid;
		sev: gl.enum;
		msg: string;
	end;

	procedure GLDebugTask(param: pointer);

		function SourceStr(source: gl.enum): string;
		begin
			case source of
				gl.DEBUG_SOURCE_API:             result := 'Api';
				gl.DEBUG_SOURCE_WINDOW_SYSTEM:   result := 'Win';
				gl.DEBUG_SOURCE_SHADER_COMPILER: result := 'Sha';
				gl.DEBUG_SOURCE_THIRD_PARTY:     result := 'Thp';
				gl.DEBUG_SOURCE_APPLICATION:     result := 'App';
				gl.DEBUG_SOURCE_OTHER:           result := 'Oth';
				else result := '(src?)';
			end;
		end;

		function TypeStr(typ: gl.enum): string;
		begin
			case typ of
				gl.DEBUG_TYPE_ERROR:               result := 'Err';
				gl.DEBUG_TYPE_DEPRECATED_BEHAVIOR: result := 'Dep';
				gl.DEBUG_TYPE_UNDEFINED_BEHAVIOR:  result := 'UB';
				gl.DEBUG_TYPE_PORTABILITY:         result := 'Port';
				gl.DEBUG_TYPE_PERFORMANCE:         result := 'Perf';
				gl.DEBUG_TYPE_OTHER:               result := 'Nf';
				else result := '(type?)';
			end;
		end;

		function SeverityStr(sev: gl.enum): string;
		begin
			case sev of
				gl.DEBUG_SEVERITY_HIGH:   result := '!';
				gl.DEBUG_SEVERITY_MEDIUM: result := '?';
				gl.DEBUG_SEVERITY_LOW:    result := '~';
				else result := '(sev?)';
			end;
		end;
	var
		p: pDebugTaskParam absolute param;
		style: LogMessageStyle;
	begin
		case p^.sev of
			gl.DEBUG_SEVERITY_HIGH:   style := logError;
			gl.DEBUG_SEVERITY_MEDIUM: style := logWarning;
			gl.DEBUG_SEVERITY_LOW:    style := logDebug;
			else style := logWarning;
		end;
		Log('OpenGL says [' + SourceStr(p^.uid.source) + TypeStr(p^.uid.typ) + SeverityStr(p^.sev) + ', #' + ToString(p^.uid.id) + ']: ' + p^.msg, style);
		p^.gl^.debugSync.KillOne;
		dispose(p);
	end;

	procedure GLDebug(source, typ: gl.enum; id: gl.uint; severity: gl.enum; len: gl.sizei; message: pChar; userParam: pointer); stdcall;
	var
		p: pDebugTaskParam;
		gl: pOpenGL absolute userParam;
		uid, t: tOpenGL.DebugOutputUid;
		i, j: sint;
	begin
		Assert(@len = @len);
		gl^.debugSync.AddOne;

		uid.source := source;
		uid.typ    := typ;
		uid.id     := id;

		if severity <> OpenGL_Headers.gl.DEBUG_SEVERITY_HIGH then
		begin
			gl^.debugSync.lock.Enter;
			for i := 0 to gl^.nDebugSeen - 1 do
				if (gl^.debugSeen[i].source = source) and (gl^.debugSeen[i].typ = typ) and (gl^.debugSeen[i].id = id) then
				begin
					t := gl^.debugSeen[i];
					for j := i downto 1 do
						gl^.debugSeen[j] := gl^.debugSeen[j - 1];
					gl^.debugSeen[0] := t;

					gl^.debugSync.lock.Leave;
					gl^.debugSync.KillOne;
					exit;
				end;

			if gl^.nDebugSeen < length(gl^.debugSeen) then inc(gl^.nDebugSeen);
			for i := gl^.nDebugSeen - 1 downto 1 do
				gl^.debugSeen[i] := gl^.debugSeen[i - 1];
			gl^.debugSeen[0] := uid;
			gl^.debugSync.lock.Leave;
		end;

		new(p);
		p^.gl := gl;
		p^.uid := uid;
		p^.sev := severity;
		p^.msg := string(message);
		Work.Queue(@GLDebugTask, p);
	end;
{$ENDIF Debug}

	function tOpenGL._InitGL: boolean;
	begin
		result := OpenGL_Headers.LoadOpenGL;
		if not result then exit;

		paranoia('(контекст)');
		result := _CheckVendorSpecific;
		if not result then
		begin
			OpenGL_Headers.UnloadOpenGL;
			exit;
		end;

		if BinaryShadersSupported then
			try
				BlobCache.Open(bsCache, Paths.Cache + BinaryShadersCacheFile);
			except
			{$ifdef Debug} Log(Exception.Message + ' Кэш шейдеров выключен.', logWarning); {$endif}
				bsCacheFailures := MaxBinaryShaderCacheFailures;
			end;

		gl.Enable(gl.DEPTH_TEST);
		gl.Enable(gl.CULL_FACE);
		gl.DepthFunc(gl.LEQUAL);
		gl.PixelStorei(gl.PACK_ALIGNMENT, 1);
		gl.PixelStorei(gl.UNPACK_ALIGNMENT, 1);
		if _primitiveRestartSupported then
		begin
			gl.Enable(gl.PRIMITIVE_RESTART);
			gl.PrimitiveRestartIndex(High(gl.uint));
			_primitiveRestartIndex := High(gl.uint);
		end;
		paranoia('(начальные настройки)');
	{$ifdef Debug}
		if debugOutputSupported then
		begin
			Log('Устанавливаю обработчик отладочных сообщений OpenGL...');
			debugSync.Init;
			nDebugSeen := 0;
			gl.DebugMessageCallback(@GLDebug, @self);
			paranoia('glDebugMessageCallback');
		end;
	{$endif}
		paranoia('InitContext');
	end;

	procedure tOpenGL._DoneGL;
{$ifdef Debug}
	var
		rc: ResCounterEnum;
{$endif}
	begin
	{$ifdef Debug}
		if debugOutputSupported then
		begin
			Log('Снимаю обработчик отладочных сообщений OpenGL...');
			gl.DebugMessageCallback(nil, nil);
			debugSync.Done;
		end;

		for rc in ResCounterEnum do
		begin
			LogR('Макс. ' + ResCounterNames[rc] + ' одновременно: ' + ToString(resCounter[rc].max), logDebug);
			if resCounter[rc].count <> 0 then
				Log(' (не удалено: ' + ToString(resCounter[rc].count) + '!)', logWarning)
			else
				Log('', logDebug);
		end;
	{$endif}
		UnloadOpenGL;
	end;

	procedure tOpenGL._OnEndFrame;
	begin
		CheckError;
	end;

	procedure tOpenGL._glBindTexture(texUnit: sint; target: GLTextureTarget; texID: GLHandle);
	begin
		if _boundTextures[texUnit] <> texID then
		begin
			if _activeTextureUnit <> texUnit then
			begin
				gl.ActiveTexture(gl.TEXTURE0 + texUnit);
				_activeTextureUnit := texUnit;
			{$ifdef Debug} _curStateChanges += 1; {$endif}
			end;
			gl.BindTexture(GLTextureTargetEnums[target], texID);
			_boundTextures[_activeTextureUnit] := texID;
		{$ifdef Debug} _curStateChanges += 1; {$endif}
		end;
	end;

	procedure tOpenGL._glUseProgram(prog: pGLProgram);
	begin
		if _activeProgPtr <> prog then
		begin
			if Assigned(prog) then
			begin
				gl.UseProgram(prog^.id);
				_activeProgPtr := prog;
			end else
			begin
				gl.UseProgram(0);
				_activeProgPtr := nil;
			end;
		end;
	end;

	procedure tOpenGL._UseDrawingProgram(var prog: GLProgram);
	var
		i: sint;
	begin
		_glUseProgram(@prog);
		for i := 0 to prog.nSamplers - 1 do
			_glBindTexture(i, prog.samplers[i].target, prog.samplers[i].id);
		for i := 0 to prog.nUbos - 1 do
			_BindUBO(i, prog.ubos[i].id);
		paranoia('UseDrawingProgram');
	end;

	procedure tOpenGL._BindUBO(index: sint; id: GLHandle);
	begin
		if _boundUbos[index] <> id then
		begin
			_boundUbos[index] := id;
			gl.BindBufferBase(gl.UNIFORM_BUFFER, index, id);
		end;
	end;

	procedure tOpenGL._glBindVertexArray(id: GLHandle);
	begin
		if _boundVertexArray <> id then
		begin
			gl.BindVertexArray(id);
			_boundVertexArray := id;
		{$ifdef Debug} _curStateChanges += 1; {$endif}
		end;
	end;

	function tOpenGL.BindBufferToOperate(target: GLBufferTarget; id: GLHandle): gl.enum;
	begin
		case target of
			GLbuffer_Index: _glBindVertexArray(0);
		end;
		result := GLBufferTargetEnums[target];

		if _bb[target] <> id then
		begin
			gl.BindBuffer(result, id);
			_bb[target] := id;
		{$ifdef Debug} _curStateChanges += 1; {$endif}
		end;
	end;

	procedure tOpenGL._glBindFramebuffer(var rt: GLRenderTarget);
	begin
		if _activeFramebuffer <> rt.id then
		begin
			gl.BindFramebuffer(gl.FRAMEBUFFER, rt.id);
			_activeFramebuffer := rt.id;
		{$ifdef Debug} _curStateChanges += 1; {$endif}
		end;
		_glViewport(0, 0, rt.size.X, rt.size.Y);
	end;

	procedure tOpenGL._Clear(var rt: GLRenderTarget; bufs: GLRenderBuffers; const clearColor: Vec4);
	const
		BufferBitfields: array[GLRenderBuffer] of gl.bitfield =
		(
			gl.COLOR_BUFFER_BIT,
			gl.DEPTH_BUFFER_BIT
	);
	var
		r: GLRenderBuffer;
		mask: gl.bitfield;
	begin
		if rt.bad then exit;
		mask := 0;
		if (GLbuffer_Color in bufs) and (clearColor <> _clearColor) then
		begin
			gl.ClearColor(clearColor.x, clearColor.y, clearColor.z, clearColor.w);
			_clearColor := clearColor;
		end;
		_glBindFramebuffer(rt);
		for r in GLRenderBuffer do
			if r in bufs then mask := mask OR BufferBitfields[r];

		// Багофича ОТИ: при включенном маскировании буфера глубины он не очищается, лол.
		if oldRast.depthMask then
		begin
			gl.DepthMask(gl.TRUE); {$ifdef Debug} _curStateChanges += 1; {$endif}
			oldRast.depthMask := no;
		end;

		gl.Clear(mask);
		paranoia('Clear');
	end;

	procedure tOpenGL._DrawBatch(var rt: GLRenderTarget; var vd: GLVertexDeclaration; var prog: GLProgram; ibOffset: PtrUint; nIndices: uint; indexType: GLType; nInstances: uint);
	const
		TopologyEnums: array[GLTopology] of gl.enum = (gl.POINTS, gl.LINES, gl.LINE_STRIP, gl.TRIANGLES, gl.TRIANGLE_STRIP);
	{$ifdef Debug}
		function TrisDrawn(var top: GLTopology; nIndices, nInstances: uint): uint;
		begin
			result := 0;
			case top of
				GLtopology_Tris: result := nInstances * (nIndices div 3);
				GLtopology_TriStrip: if nIndices > 2 then result := nInstances * (nIndices - 2);
			end;
		end;
	{$endif}
	begin
		if rt.bad then exit;
		_glBindFramebuffer(rt);
		_UseDrawingProgram(prog);
		_glBindVertexArray(vd.id);
		_ApplyRasterizerState;
		if _primitiveRestartSupported and (_primitiveRestartIndex <> GLIntLims[indexType]) then
		begin
			_primitiveRestartIndex := GLIntLims[indexType];
			gl.PrimitiveRestartIndex(_primitiveRestartIndex);
		end;

		if nInstances = 1 then
			gl.DrawElements(TopologyEnums[vd.topology], nIndices, GLTypeEnums[indexType], NULL + ibOffset)
		else
			gl.DrawElementsInstanced(TopologyEnums[vd.topology], nIndices, GLTypeEnums[indexType], NULL + ibOffset, nInstances);
	{$ifdef Debug}
		_curDrawCalls += 1;
		_curTris += TrisDrawn(vd.topology, nIndices, nInstances);
	{$endif}
		paranoia('DrawBatch');
	end;

	procedure tOpenGL._CreateVertexDeclaration(var vd: GLVertexDeclaration; var ib: GLBuffer);
	var
		GLid: gl.uint;
	begin
		gl.GenVertexArrays(1, @GLid);
		vd.id := GLid;
		_glBindVertexArray(GLid);
		gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, ib.id);
	{$ifdef Debug} NoteCreate(rc_VertexDeclaration); {$endif}
		paranoia('CreateVertexDeclaration');
	end;

	procedure tOpenGL._DeleteVertexDeclaration(var vd: GLVertexDeclaration);
	var
		GLid: gl.uint;
	begin
		if _boundVertexArray = vd.id then _glBindVertexArray(0);
		GLid := vd.id;
		gl.DeleteVertexArrays(1, @GLid);
	{$ifdef Debug} if vd.id > 0 then NoteDestroy(rc_VertexDeclaration); {$endif}
		paranoia('DeleteVertexDeclaration');
	end;

	procedure tOpenGL._SetVertexAttribute(var vd: GLVertexDeclaration; index: sint; type_: GLType; stride: size_t; vbOffset: PtrUint);
	begin
		_glBindVertexArray(vd.id);
		gl.EnableVertexAttribArray(index);
		BindBufferToOperate(GLbuffer_Vertex, vd.vb^.id);
		gl.VertexAttribPointer(index, GLAsAttrib[type_].count, GLAsAttrib[type_].enum, GLAsAttrib[type_].normalized, stride, pPointer(@vbOffset)^);
		paranoia('SetVertexAttribute');
	end;

	procedure tOpenGL._CreateBuffer(var buf: GLBuffer);
	var
		glbuf: gl.uint;
	begin
		if Assigned(gl.CreateBuffers) then gl.CreateBuffers(1, @glbuf) else
		begin
			gl.GenBuffers(1, @glbuf);
			// иначе DSA в OpenGL 4.5 фейлится, БУФЕР НЕ СОЗДАН КО-КО-КО
			BindBufferToOperate(buf.target, glbuf);
		end;
		buf.id := glbuf;

	{$ifdef Debug} NoteCreate(rc_Buffer); {$endif}
		paranoia('CreateBuffer');
	end;

	procedure tOpenGL._DeleteBuffer(var buf: GLBuffer);
	var
		glbuf: gl.uint;
		i: sint;
		bufTarget: GLBufferTarget;
	begin
		if buf.target = GLbuffer_Uniform then
			for i := 0 to High(_boundUbos) do
				if _boundUbos[i] = buf.id then _BindUBO(i, 0);
		for bufTarget in GLBufferTarget do
			if _bb[bufTarget] = buf.id then BindBufferToOperate(bufTarget, 0);
		glbuf := buf.id;
		gl.DeleteBuffers(1, @glbuf);
	{$ifdef Debug} if buf.id > 0 then NoteDestroy(rc_Buffer); {$endif}
		paranoia('DeleteBuffer');
	end;

	procedure tOpenGL._BufferData(var buf: GLBuffer; size: size_t; data: pointer; usage: GLBufferUsage);
	const
		UsageEnums: array[GLBufferUsage] of gl.enum =
		(
			gl.STATIC_DRAW,
			gl.DYNAMIC_DRAW
	);
	var
		usageEnum: gl.enum;
	begin
		case buf.target of
			GLbuffer_Uniform: usageEnum := gl.DYNAMIC_DRAW;
			else
				usageEnum := UsageEnums[usage];
		end;
		if dsaSupported then
			gl.NamedBufferData(buf.id, size, data, usageEnum)
		else
			gl.BufferData(BindBufferToOperate(buf.target, buf.id), size, data, usageEnum);
	{$ifdef Debug} if Assigned(data) then _curSended += size; {$endif}
		paranoia('BufferData');
	end;

	procedure tOpenGL._BufferSubData(var buf: GLBuffer; offset: PtrUint; size: size_t; data: pointer);
	begin
		if dsaSupported then
			gl.NamedBufferSubData(buf.id, offset, size, data)
		else
			gl.BufferSubData(BindBufferToOperate(buf.target, buf.id), offset, size, data);
	{$ifdef Debug} _curSended += size; {$endif}
		paranoia('BufferSubData');
	end;

{$ifdef Debug}
	function tOpenGL.SuspiciousLog(const s: string): boolean;
	var
		lc: string;
	begin
		lc := Lowercase(s);
		result := (Pos('warn', lc) > 0) or (Pos('err', lc) > 0) or (Pos('fail', lc) > 0) or (Pos('note', lc) > 0);
	end;
{$endif}

	procedure tOpenGL._CreateShader(var sh: GLShader; type_: ShaderType; const name, source: string);
	const
		ShaderTypeEnums: array[ShaderType] of gl.enum =
		(
			gl.VERTEX_SHADER,
			gl.GEOMETRY_SHADER,
			gl.FRAGMENT_SHADER
	);
	var
		cs: gl.int;
		sptr: pChar;
	{$ifdef Debug}
		shlog: string;
		len: gl.int;
	{$endif}
	begin
		Assert(@name = @name);
		Assert((type_ <> GLshader_Geometry) or _geometryShaderSupported);
		sh.id := gl.CreateShader(ShaderTypeEnums[type_]);
	{$ifdef Debug} NoteCreate(rc_Shader); {$endif}

		sptr := pChar(source);
		gl.ShaderSource(sh.id, 1, @sptr, nil);
	{$ifdef Debug} LogR('Компиляция шейдера ' + name  + '... '); {$endif}
		gl.CompileShader(sh.id);
		gl.GetShaderiv(sh.id, gl.COMPILE_STATUS, @cs);
		sh.compiled := cs = gl.TRUE;

	{$ifdef Debug}
		if sh.compiled then Log('Шейдер ' + name + ' скомпилирован', logOK) else
		begin
			Log('Ошибка компиляции ' + name +'!', logError);
			Log('***' + EOL +
					'Исходник:' + EOL +
					source, logDebug);
		end;

		len := 0;
		gl.GetShaderiv(sh.id, gl.INFO_LOG_LENGTH, @len);
		if len > 0 then
		begin
			SetLength(shlog, len); gl.GetShaderInfoLog(sh.id, len, @len, pointer(shlog)); SetLength(shlog, len);

			if (not sh.compiled) or (SuspiciousLog(shlog)) then
				Log('***' + EOL +
					'Лог компиляции ' + name + ':' + EOL +
					shlog + EOL +
					'***', logDebug);
		end;
	{$endif}
		if not sh.compiled then _Error('Ошибка компиляции шейдера ' + name + '.');
		paranoia('CreateShader');
	end;

	procedure tOpenGL._DeleteShader(var sh: GLShader);
	begin
		gl.DeleteShader(sh.id);
	{$ifdef Debug} if sh.id > 0 then NoteDestroy(rc_Shader); {$endif}
		paranoia('DeleteShader');
	end;

type
	ProgramOptions = record
		uid: StringView;
		suppressBinary: boolean;
	end;

	procedure HandleProgramOption(id: uint; const value: StringView; param: pointer);
	begin
		case id of
			0 {Uid}:          ProgramOptions(param^).uid := value;
			1 {SuppressLoad}: ProgramOptions(param^).suppressBinary := yes;
		end;
	end;

{$ifdef Debug}
	function DescribeVertexAttribute(id: uint; param: pointer): string;
	var
		va: ^GLVertexAttribsList absolute param;
	begin
		result := GLTypeIds[va^[id].type_] + ' ' + va^[id].name + ' @ ' + ToString(va^[id].index);
	end;
{$endif}

type
	CacheBinaryShaderTaskParam = object
		g: pOpenGL;
		uid: string;
		GLFormat: gl.enum;
		headerAndUncompressed: pointer;
		headerReserve, uncompressedSize: size_t;
		procedure Run;
	end;

	procedure CacheBinaryShaderTaskParam.Run;
	var
		headerAndCompressed, dataStart, headerPos, WorkingMemory: pointer;
		headerSize, dataSize, storedSize, headerRest: size_t;
		compressedSize: cuint;
		checksum: uint32;
	begin
		headerAndCompressed := nil;
		dataStart := headerAndUncompressed + headerReserve;
		dataSize := uncompressedSize;
		storedSize := 0;

		if g^.lzoOk then
		begin
			compressedSize := lzo.bound(uncompressedSize);
			WorkingMemory  := GetMem(lzo.COMPRESS_WORK_MEM);
			headerAndCompressed := GetMem(headerReserve + compressedSize);

			if lzo.compress(dataStart, dataSize, headerAndCompressed + headerReserve, compressedSize, WorkingMemory) = lzo.OK then
			begin
				if (compressedSize > 0) and (compressedSize < uncompressedSize div 2)
					and (compressedSize <= tOpenGL.ReasonableBinaryShaderSizeLimit) then
				begin
					dataStart := headerAndCompressed + headerReserve;
					dataSize := compressedSize;
					storedSize := uncompressedSize;
				end;

			{$ifdef Debug}
				LogR('"{0}" сжат до {1} (-{2}%) — {3}; ', uid, ToStringSuff_b(compressedSize),
					ToString(100 * (1 - compressedSize / uncompressedSize)), IfThen(storedSize > 0, 'принято', 'отброшено'), logDebug);
			{$endif}
			end;
			FreeMem(WorkingMemory);
		end;
		checksum := Hash.Murmur32(dataStart, dataSize);
		headerSize := VarInt.Bytes(GLFormat) + VarInt.Bytes(storedSize) + sizeof(checksum);
		Assert(headerSize <= headerReserve);

		headerPos := dataStart - headerSize;
		headerRest := headerSize;
		VarInt.Store(GLFormat, headerPos, headerRest);
		VarInt.Store(storedSize, headerPos, headerRest);
		Store_ui32(checksum, headerPos, headerRest);
		Assert((headerPos = dataStart) and (headerRest = 0));

		g^.bsLock.Enter;
		try
			try
				g^.bsCache.TryPut(uid, g^.bsCache.BlockData.Make(dataStart - headerSize, headerSize + dataSize));
			except
				g^.HandleBinaryShaderCacheFailure(yes);
			end;
		finally
			g^.bsLock.Leave;
		end;
		FreeMem(headerAndCompressed);
		FreeMem(headerAndUncompressed);
		g^.bsPending.KillOne;
		dispose(@self);
	end;

	procedure CacheBinaryShaderTask(param: pointer);
	begin
		CacheBinaryShaderTaskParam(param^).Run;
	end;

	function tOpenGL._CreateProgram(var prog: GLProgram; const namex: string; const sh: array of pGLShader; out info: ShaderEntrails): boolean;
	label _finally_;
	var
		name: StringView;

	{$ifdef Debug}
		procedure LogLog(var prog: GLProgram; force: boolean; const ofwhat: string);
		var
			plog: string;
		begin
			plog := _GetProgramLog(prog);
			if force or SuspiciousLog(plog) then
				Log('***' + EOL +
						'Лог ' + ofwhat + ' ' + name.ToString + ': ' + EOL +
						plog + EOL +
						'***', logDebug);
		end;
	{$endif}

		procedure CacheBinaryShader(const prog: GLProgram; const uid: string);
		var
			glSize: gl.int;
			GLFormat: gl.enum;
			headerAndUncompressed: pointer;
			headerReserve, uncompressedSize: size_t;
			c: ^CacheBinaryShaderTaskParam;
		begin
			paranoia('перед CacheBinaryShader');

		{$ifdef Debug} LogR('Кэширую бинарный шейдер {0}... ', name.ToString); {$endif}
			gl.GetProgramiv(prog.id, gl.PROGRAM_BINARY_LENGTH, @glSize);
			if glSize <= 0 then
			begin
			{$ifdef Debug} Log('Странный размер ({0}).', [glSize], logError); {$endif}
				exit;
			end;

			uncompressedSize := glSize;
			if uncompressedSize > ReasonableBinaryShaderSizeLimit then
				raise Error('Неправдоподобный размер шейдера ({0}).', ToStringSuff_b(uncompressedSize));
		{$ifdef Debug} LogR('Размер: ' + ToStringSuff_b(uncompressedSize) + '; ', logDebug); {$endif}

			headerReserve := Align(VarInt.Bound(sizeof(GLFormat)) + VarInt.Bound(sizeof(ReasonableBinaryShaderSizeLimit)) + sizeof(uint32),
			                       sizeof(GenericAlignedType));

			headerAndUncompressed := GetMem(headerReserve + uncompressedSize);
			gl.GetProgramBinary(prog.id, glSize, nil, GLFormat, headerAndUncompressed + headerReserve);
		{$ifdef Debug} LogR('Формат: ${0}; ', ToString(GLFormat, IntFormat.Hex), logDebug); {$endif}

			new(c);
			c^.g                     := @self;
			c^.uid                   := uid;
			c^.GLFormat              := GLFormat;
			c^.headerAndUncompressed := headerAndUncompressed;
			c^.headerReserve         := headerReserve;
			c^.uncompressedSize      := glSize;
			bsPending.AddOne;
			Work.Queue(@CacheBinaryShaderTask, c, [Task.HardWork]);
		end;

		function LoadBinaryShader(var prog: GLProgram; const uid: string): boolean;
		var
			im: pStreamImage;
			GLFormat: gl.enum;
			ls: gl.int;
			imp, shaderData: pointer;
			ownShaderData: boolean;
			imRest, storedSize, shaderDataSize: size_t;
			lzoSize: cuint;
			checksum: uint32;
		begin
			result := no;
			try
				bsLock.Enter;
				try
					im   := bsCache.TryGet(uid);
				finally
					bsLock.Leave;
				end;
				if not Assigned(im) then exit;
				ownShaderData := no;

				try
				{$ifdef Debug} LogR('Загружаю бинарный шейдер ' + name.ToString + '... '); {$endif}
					imp := im^.Data; imRest := im^.Size;
					GLFormat   := VarInt.Load(imp, imRest);
					storedSize := VarInt.Load(imp, imRest);
					checksum   := Load_ui32(imp, imRest);
					if Hash.Murmur32(imp, imRest) <> checksum then raise Error('Данные шейдера повреждены (Murmur3).');

					if storedSize = 0 then
					begin
						shaderData := imp;
						shaderDataSize := imRest;
					end else
					if storedSize > ReasonableBinaryShaderSizeLimit then
						raise Error('Неправдоподобный размер шейдера ({0}).', ToStringSuff_b(storedSize))
					else
					begin
						shaderData := GetMem(storedSize);
						ownShaderData := yes;
						lzoSize := storedSize;
						shaderDataSize := storedSize;
						if not lzoOk then raise Error('LZO не загружена.');
						if (lzo.decompress(imp, imRest, shaderData, lzoSize, nil) <> lzo.OK) or (lzoSize <> storedSize) then
							raise Error('Данные шейдера повреждены (LZO).');
					end;
				{$ifdef Debug}
					LogR('Формат: ${0}, размер данных: {1}{2}; ', ToString(GLFormat, IntFormat.Hex), ToStringSuff_b(shaderDataSize),
						IfThen(storedSize > 0, ' (сжатый ' + ToStringSuff_b(imRest) + ')'), logDebug);
				{$endif}

					gl.ProgramBinary(prog.id, GLFormat, shaderData, shaderDataSize);
					gl.GetProgramiv(prog.id, gl.LINK_STATUS, @ls);
					result := ls = gl.TRUE;
				{$ifdef Debug}
					if result then Log('Бинарный шейдер ' + name.ToString + ' загружен', logOK); // иначе логнется исключение ниже
					LogLog(prog, not result, 'загрузки бинарного шейдера');
				{$endif}
					if not result then raise Error('Не удалось загрузить бинарный шейдер {0}.', name.ToString);
				finally
					if ownShaderData then FreeMem(shaderData);
					Release(im);
				end;
			except
				bsLock.Enter;
				try
					HandleBinaryShaderCacheFailure(yes);
				finally
					bsLock.Leave;
				end;
			end;
		end;

		function Maybe(var uid: string; const sample: string): boolean;
		var
			p: sint;
		begin
			p := Pos(sample, uid);
			result := p > 0;
			if result then delete(uid, p, length(sample));
		end;

	var
		i: sint;
		ls: gl.int;
		bsOk, bsLoadOk: boolean;
		opts: ProgramOptions;
	begin
		for i := 0 to High(sh) do
			if not sh[i]^.compiled then
				exit(no);

		opts.uid            := StringView.Empty;
		opts.suppressBinary := no;
		name := StringOptionals.Split(namex, BinaryShader.Options, @HandleProgramOption, @opts);

		bsOk := BinaryShadersSupported and (opts.uid.n > 0) and (bsCacheFailures < MaxBinaryShaderCacheFailures);
		bsLoadOk := bsOk and not opts.suppressBinary;

		prog.id := gl.CreateProgram();
		result := bsLoadOk and LoadBinaryShader(prog, opts.uid.ToString);

		if (not result) and (length(sh) > 0) then
		begin
			for i := 0 to High(sh) do
				gl.AttachShader(prog.id, sh[i]^.id);

			if bsOk then gl.ProgramParameteri(prog.id, gl.PROGRAM_BINARY_RETRIEVABLE_HINT, gl.TRUE);
		{$ifdef Debug} LogR('Линковка ' + name.ToString + '... '); {$endif}
			gl.LinkProgram(prog.id);
			gl.GetProgramiv(prog.id, gl.LINK_STATUS, @ls);
			result := ls = gl.TRUE;

		{$ifdef Debug}
			if result then
				Log(name.ToString + ' слинкована', logOK)
			else
				Log('Ошибка линковки ' + name.ToString + '!', logError);
			LogLog(prog, not result, 'линковки');
		{$endif}

			if bsOk and result then CacheBinaryShader(prog, opts.uid.ToString);
		end;

	_finally_:
		if result then
		begin
			_GetUniforms(prog, info.u, info.ub);
			_GetVertexAttribs(prog, info.va);
			prog.nUbos := length(info.ub);

			if length(info.va) > MaxVertexAttributes then
			begin
				result := no;
			{$ifdef Debug}
				Log('Шейдер {0} содержит слишком много вершинных атрибутов ({1}, предел — {2}): {3}', name.ToString, ToString(length(info.va)),
						ToString(MaxVertexAttributes), SeparatedList.Join(length(info.va), @DescribeVertexAttribute, @info.va, ', '), logError);
			{$endif}
			end;
		end;

		if result then
		begin
		{$ifdef Debug}
			Log(name.ToString + ' готова', logOK);
			NoteCreate(rc_Program);
		{$endif}
		end else
		begin
			if length(sh) > 0 then _Error('Не удалось загрузить шейдерную программу ' + name.ToString + '.');
			_glUseProgram(nil);
			gl.DeleteProgram(prog.id);
		end;
		paranoia('CreateProgram');
	end;

	procedure tOpenGL._DeleteProgram(var prog: GLProgram);
	begin
		if _activeProgPtr = @prog then _glUseProgram(nil);
		gl.DeleteProgram(prog.id);
	{$ifdef Debug} if prog.id > 0 then NoteDestroy(rc_Program); {$endif}
		paranoia('DeleteProgram');
	end;

	function tOpenGL._GetProgramLog(var prog: GLProgram): string;
	var
		len: gl.int;
	begin
		len := 0;
		gl.GetProgramiv(prog.id, gl.INFO_LOG_LENGTH, @len);
		if len > 0 then
		begin
			SetLength(result, len); gl.GetProgramInfoLog(prog.id, len, @len, pointer(result)); SetLength(result, len);
		end else
			result := '';
		paranoia('GetProgramLog');
	end;

	procedure tOpenGL._GetUniforms(var prog: GLProgram; out u: GLUniformsList; out ub: GLUboList);
		function TranslateType(enum: gl.enum; out typ: GLType): boolean;
		begin
			result := yes;
			case enum of
				gl.FLOAT_TYPE: typ := GLType.Float;
				gl.FLOAT_VEC2: typ := GLType.Vec2;
				gl.FLOAT_VEC3: typ := GLType.Vec3;
				gl.FLOAT_VEC4: typ := GLType.Vec4;
				gl.FLOAT_MAT4: typ := GLType.Mat4;
				gl.SIGNED_INT: typ := GLType.Int32;
				gl.SAMPLER_1D, gl.SAMPLER_2D, gl.SAMPLER_3D, gl.SAMPLER_CUBE: typ := GLType.Sampler;
				else
				begin
				{$ifdef Debug} Log('Тип юниформа неизвестен или неверен (OGL: ' + ToString(enum) + ')', logError); {$endif}
					result := no;
				end;
			end;
		end;
		// имена массивов могут быть возвращены с "[0]", а могут и без
		function FixName(const name: string): string;
		var
			p: sint;
		begin
			p := pos('[', name);
			if p = 0 then result := name else result := Copy(name, 1, p - 1);
		end;
		function IsSuspiciousName(const name: string): boolean;
		begin
			result := name[1] = '_';
		{$ifdef Debug} if result then Log('Какой-то подозрительный юниформ "' + name + '" — пропускаю', logWarning); {$endif}
		end;
	{$ifdef ExtDebug}
		function DescribeUniform(const u: tGLUniformDesc): string;
		begin
			result := GLTypeIds[u.uniform.type_];
			if u.uniform.type_ = GLType.Sampler then result += '#' + ToString(u.uniform.texUnit);
			result += ' ' + u.name;
			if u.uniform.count > 1 then result += '[' + ToString(u.uniform.count) + ']';
			result += '@' + ToString(u.uniform.loc);
		end;
		function DescribeUboUniform(const u: tGLUboUniformDesc): string;
		begin
			result := ToString(u.u.bufOffset) + ': ' + GLTypeIds[u.u.type_] + ' ' + u.name;
			if u.u.count <> 1 then result += '[' + ToString(u.u.count) + ']';
			if u.u.matStride > 0 then result += ' (шаг матрицы: ' + ToString(u.u.matStride) + ')';
			if u.u.arrStride > 0 then result += ' (шаг массива:' + ToString(u.u.arrStride) + ')';
			if not u.u.stridesArePacked then result += ' (разрежен!)';
		end;
	{$endif}
	var
		curTexUnit: uint;
		i, j, nTotal, nU, nUb: sint;
		gln, maxNameLen: gl.int;
		gltype: gl.enum;
		indices: array of gl.uint;
		types, counts, blockIds, nameLens, offsets, arrStrides, matStrides: array of gl.int;
		len: gl.sizei;
		name: string;
		cu: GLUniform;
		cub: GLUboUniform;
	{$ifdef Debug} k: sint; {$endif}
	{$ifdef ExtDebug} s: string; {$endif}
	begin
		_glUseProgram(@prog);
		curTexUnit := 0;
		nU := 0;
		gl.GetProgramiv(prog.id, gl.ACTIVE_UNIFORMS, @gln);
		nTotal := gln;
		if nTotal = 0 then exit;

		SetLength(u, nTotal);
		SetLength(blockIds, nTotal);
		maxNameLen := 64;
		gl.GetProgramiv(prog.id, gl.ACTIVE_UNIFORM_MAX_LENGTH, @maxNameLen);

		if _UBOSupported then
		begin
			SetLength(counts, nTotal);
			SetLength(types, nTotal);
			SetLength(nameLens, nTotal);
			SetLength(offsets, nTotal);
			SetLength(arrStrides, nTotal);
			SetLength(matStrides, nTotal);
			SetLength(indices, nTotal);
			for i := 0 to nTotal - 1 do indices[i] := i;
			gl.GetActiveUniformsiv(prog.id, nTotal, pointer(indices), gl.UNIFORM_TYPE, pointer(types));
			gl.GetActiveUniformsiv(prog.id, nTotal, pointer(indices), gl.UNIFORM_SIZE, pointer(counts));
			gl.GetActiveUniformsiv(prog.id, nTotal, pointer(indices), gl.UNIFORM_BLOCK_INDEX, pointer(blockIds));
			gl.GetActiveUniformsiv(prog.id, nTotal, pointer(indices), gl.UNIFORM_NAME_LENGTH, pointer(nameLens));
			gl.GetActiveUniformsiv(prog.id, nTotal, pointer(indices), gl.UNIFORM_OFFSET, pointer(offsets));
			gl.GetActiveUniformsiv(prog.id, nTotal, pointer(indices), gl.UNIFORM_ARRAY_STRIDE, pointer(arrStrides));
			gl.GetActiveUniformsiv(prog.id, nTotal, pointer(indices), gl.UNIFORM_MATRIX_STRIDE, pointer(matStrides));
			nUb := 0;
			for i := 0 to nTotal - 1 do
				if blockIds[i] >= nUb then nUb := blockIds[i] + 1;
			SetLength(ub, nUb);
			for i := 0 to High(ub) do
			begin
				gl.GetActiveUniformBlockiv(prog.id, i, gl.UNIFORM_BLOCK_DATA_SIZE, @gln);
				ub[i].dataSize := gln;
				gl.GetActiveUniformBlockiv(prog.id, i, gl.UNIFORM_BLOCK_NAME_LENGTH, @gln);
				SetLength(name, gln - 1);
				gl.GetActiveUniformBlockName(prog.id, i, length(name) + 1, nil, pointer(name));
				ub[i].name := name;
				ub[i].uniforms := nil;
				gl.UniformBlockBinding(prog.id, i, i);
			end;
			for i := 0 to nTotal - 1 do
			begin
				j := blockIds[i];
				if j = -1 then continue;
				if nameLens[i] = 0 then continue;
				SetLength(name, nameLens[i] - 1);
				gl.GetActiveUniformName(prog.id, i, nameLens[i], nil, pointer(name));
				if not TranslateType(types[i], cub.typ) then continue;
				if IsSuspiciousName(name) then continue;
				if counts[i] <= 0 then
				begin
				{$ifdef Debug} LogR(name + ': count = ' + ToString(counts[i]) + ' — возможно, юниформ неактивен; ', logWarning); {$endif}
					continue;
				end;
				if ((counts[i] > 1) and (arrStrides[i] <= 0)) then
				begin
				{$ifdef Debug} LogR(name + ': arrStride = ' + ToString(arrStrides[i]) + ' — возможно, юниформ неактивен; ', logWarning); {$endif}
					continue;
				end;
				cub.count := counts[i];
				cub.bufOffset := offsets[i];
				cub.arrStride := arrStrides[i];
				cub.matStride := matStrides[i];
				cub.stridesArePacked :=
					((cub.count = 1) or (cub.arrStride = GLTypeInfo[cub.typ].sizeof)) and
					((not (GLTypeFlag.Matrix in GLTypeInfo[cub.typ].flags)) or (cub.matStride = GLTypeInfo[GLTypeInfo[cub.typ].baseType].sizeof));
				SetLength(ub[j].uniforms, length(ub[j].uniforms) + 1);
				ub[j].uniforms[High(ub[j].uniforms)].name := FixName(name);
				ub[j].uniforms[High(ub[j].uniforms)].u := cub;
			end;
		{$ifdef Debug}
			for i := 0 to High(ub) do
				for j := 0 to High(ub[i].uniforms) do
				begin
					for k := j + 1 to High(ub[i].uniforms) do
						if ub[i].uniforms[j].u.bufOffset = ub[i].uniforms[k].u.bufOffset then
							Log('Возможно, кто-то из ' + ub[i].uniforms[j].name + ' и ' + ub[i].uniforms[k].name + ' неактивен — а в дровах-то баги', logError);
					if ub[i].uniforms[j].u.bufOffset >= ub[i].dataSize then
						Log(ub[i].uniforms[j].name + ' вылез за пределы буфера — возможно, неактивен. Баги в дровах. ИСПРАВЬ НЕМЕДЛЕННО!', logError);
				end;
		{$endif}
		end else
			for j := 0 to nTotal - 1 do
				blockIds[j] := -1;

		for i := 0 to nTotal - 1 do
		begin
			if blockIds[i] <> -1 then continue;
			SetLength(name, maxNameLen);
			gl.GetActiveUniform(prog.id, i, maxNameLen + 1, @len, @gln, @gltype, pointer(name));
			if len = 0 then continue;
			SetLength(name, len);
			if IsSuspiciousName(name) then continue;

			cu.count := gln;
			cu.loc := gl.GetUniformLocation(prog.id, pointer(name));
		{$ifdef Debug}
			if cu.loc < 0 then Log('Юниформ "' + name + '" объявлен активным, но локейшон у него почему-то ' + ToString(cu.loc), logWarning);
		{$endif}
			if not TranslateType(gltype, cu.type_) then continue;
			if cu.type_ = U_GL.GLType.Sampler then
			begin
				cu.texUnit := curTexUnit;
				case gltype of
					gl.SAMPLER_1D: prog.samplers[curTexUnit].target := GLtexture_1D;
					gl.SAMPLER_2D: prog.samplers[curTexUnit].target := GLtexture_2D;
					gl.SAMPLER_3D: prog.samplers[curTexUnit].target := GLtexture_3D;
					gl.SAMPLER_CUBE: prog.samplers[curTexUnit].target := GLtexture_Cube;
				{$ifdef Debug} else Log('Неизвестный тип сэмплера, OGL: ' + ToString(gltype), logError); {$endif}
				end;
				gl.Uniform1i(cu.loc, cu.texUnit);
				inc(curTexUnit);
				inc(prog.nSamplers);
			end;
			u[nU].uniform := cu;
			u[nU].name := FixName(name);
			inc(nU);
		end;
		SetLength(u, nU);

	{$ifdef ExtDebug}
		s := 'Обычные юниформы (' + ToString(length(u)) + ' шт.): [';
		for i := 0 to High(u) do
		begin
			if i > 0 then s += ', ';
			s += _DescribeUniform(u[i]);
		end;
		s += ']; ';
		LogR(s, logDebug);
		for i := 0 to High(ub) do
		begin
			s := 'Блок юниформов ''' + ub[i].name + ''' (' + ToString(length(ub[i].uniforms)) + ' шт., байт: ' + ToString(ub[i].dataSize) + '): [';
			for j := 0 to High(ub[i].uniforms) do
			begin
				if j > 0 then s += ', ';
				s += _DescribeUboUniform(ub[i].uniforms[j]);
			end;
			s += ']; ';
			LogR(s, logDebug);
		end;
	{$endif}
		paranoia('GetUniforms');
	end;

	procedure tOpenGL._GetVertexAttribs(var prog: GLProgram; out a: GLVertexAttribsList);
	var
		len1: gl.int;
		len2: gl.sizei;
		glSize: gl.int; glType: gl.enum;
		name: string;
		i, n: sint;
	begin
		_glUseProgram(@prog);
		gl.GetProgramiv(prog.id, gl.ACTIVE_ATTRIBUTES, @len1);
		SetLength(a, len1);
		gl.GetProgramiv(prog.id, gl.ACTIVE_ATTRIBUTE_MAX_LENGTH, @len1);

		n := 0;
		for i := 0 to High(a) do
		begin
			SetLength(name, len1 + 1);
			gl.GetActiveAttrib(prog.id, i, len1 + 1, @len2, @glSize, @glType, @name[1]);
			SetLength(name, len2);
			a[n].name := name;
			a[n].index := gl.GetAttribLocation(prog.id, @name[1]);
			if a[n].index = -1 then
			begin
			{$ifdef Debug} LogR('[' + name + ']; ', logDebug); {$endif}
				continue;
			end;
			// по спеке здесь могут оказаться gl_InstanceID и прочий сброд
			if Prefixed('gl_', name) then continue;
			case gltype of
				gl.FLOAT_TYPE: a[n].type_ := U_GL.GLType.Float;
				gl.FLOAT_VEC2: a[n].type_ := U_GL.GLType.Vec2;
				gl.FLOAT_VEC3: a[n].type_ := U_GL.GLType.Vec3;
				gl.FLOAT_VEC4: a[n].type_ := U_GL.GLType.Vec4;
			{$ifdef Debug}
				else
					Fatal('Тип вершинного атрибута "' + name + '" неизвестен или неверен (OGL: ' + ToString(gltype) + ')');
			{$endif}
			end;
			inc(n);
		end;
		SetLength(a, n);
		paranoia('GetVertexAttribs');
	end;

	procedure tOpenGL._UpdateDrawBuffers(var rt: GLRenderTarget);
	var
		db: packed array[0 .. MaxMRTSlots - 1] of gl.enum;
		i, ndb: sint;
	begin
		Assert(_activeFramebuffer = rt.id);
		ndb := 0;
		for i := 0 to MaxMRTSlots - 1 do
			if i in rt.colorRTs then
			begin
				db[ndb] := gl.COLOR_ATTACHMENT0 + i;
				inc(ndb);
			end;
		gl.DrawBuffers(ndb, @db[0]);
	end;

	procedure tOpenGL._glViewport(x, y, w, h: sint);
	begin
		if (x <> viewport.x) or (y <> viewport.y) or (w <> viewport.w) or (h <> viewport.h) then
		begin
			viewport.x := x; viewport.y := y;
			viewport.w := w; viewport.h := h;
			gl.Viewport(x, y, w, h);
		end;
	end;

	procedure tOpenGL._ApplyRasterizerState;
	const
		BlendFactorEnums: array[GLBlendMode] of record
			src, dst: gl.enum;
		end =
		(
			(src: gl.ONE;       dst: gl.ZERO),
			(src: gl.SRC_ALPHA; dst: gl.ONE_MINUS_SRC_ALPHA),
			(src: gl.SRC_ALPHA; dst: gl.ONE_MINUS_SRC_ALPHA),
			(src: gl.SRC_ALPHA; dst: gl.ONE),
			(src: gl.ONE;       dst: gl.ONE),
			(src: gl.ONE;       dst: gl.ONE_MINUS_SRC_ALPHA)
		);
		WireEnums: array[boolean] of gl.enum = (gl.FILL, gl.LINE);
		CircuitEnums: array[GLcircuit] of gl.enum = (gl.CW, gl.CCW);
	begin
		if oldRast.depthTest <> _rast.depthTest then
		begin
			if _rast.depthTest then gl.Enable(gl.DEPTH_TEST) else gl.Disable(gl.DEPTH_TEST);
			oldRast.depthTest := _rast.depthTest;
		{$ifdef Debug} _curStateChanges += 1; {$endif}
		end;
		if oldRast.frontFace <> _rast.frontFace then
		begin
			gl.FrontFace(CircuitEnums[_rast.frontFace]);
			oldRast.FrontFace := _rast.frontFace;
		{$ifdef Debug} _curStateChanges += 1; {$endif}
		end;
		if oldRast.cull <> _rast.cull then
		begin
			if _rast.cull then gl.Enable(gl.CULL_FACE) else gl.Disable(gl.CULL_FACE);
			oldRast.cull := _rast.cull;
		{$ifdef Debug} _curStateChanges += 1; {$endif}
		end;
		if oldRast.blend <> _rast.blend then
		begin
			if _rast.blend = GLblend_Off then gl.Disable(gl.BLEND) else
			begin
				if oldRast.blend = GLblend_Off then
				begin
					gl.Enable(gl.BLEND);
				{$ifdef Debug} _curStateChanges += 1; {$endif}
				end;
				if (_blendSrc <> BlendFactorEnums[_rast.blend].src) or (_blendDst <> BlendFactorEnums[_rast.blend].dst) then
				begin
					_blendSrc := BlendFactorEnums[_rast.blend].src;
					_blendDst := BlendFactorEnums[_rast.blend].dst;
					gl.BlendFunc(_blendSrc, _blendDst);
				{$ifdef Debug} _curStateChanges += 1; {$endif}
				end;
			end;
			oldRast.blend := _rast.blend;
		end;
		if oldRast.wire <> _rast.wire then
		begin
			if self.WiresMustMatch then
			begin
				Assert((GLface_Front in _rast.wire) = (GLface_Back in _rast.wire));
				gl.PolygonMode(gl.FRONT_AND_BACK, WireEnums[GLface_Front in _rast.wire]);
			end else
			begin
				if (GLface_Front in oldRast.wire) <> (GLface_Front in _rast.wire) then
					gl.PolygonMode(gl.FRONT, WireEnums[GLface_Front in _rast.wire]);
				if (GLface_Back in oldRast.wire) <> (GLface_Back in _rast.wire) then
					gl.PolygonMode(gl.BACK, WireEnums[GLface_Back in _rast.wire]);
			end;
		{$ifdef Debug} _curStateChanges += 1; {$endif}
			oldRast.wire := _rast.wire;
		end;
		if oldRast.depthMask <> _rast.depthMask then
		begin
			gl.DepthMask(GLBoolEnum[not _rast.depthMask]);
			oldRast.depthMask := _rast.depthMask;
		end;
		paranoia('ApplyRasterizerState');
	end;

	procedure tOpenGL.HandleBinaryShaderCacheFailure(exception: boolean);
	begin
		unused_args exception end_list
	{$ifdef Debug} if exception then begin Log(USystem.Exception.Message, logError); USystem.Exception.Show; end; {$endif}
		if bsCache.OK then bsCache.Drop;
		inc(bsCacheFailures);
		try
			bsCache.Open(bsCache, Paths.Cache + BinaryShadersCacheFile)
		except
			bsCacheFailures := MaxBinaryShaderCacheFailures;
		end;
	end;

	// raw...
	procedure tOpenGL._SetUniform(var prog: GLProgram; var u: GLUniform; value: pointer; count: uint);
	var
		id: GLHandle;
	begin
		trace_call ('GL.SendUniform');
		if u.type_ <> GLType.Sampler then
		begin
			if dsaSupported then
			begin
				case u.type_ of
					GLType.Float: gl.ProgramUniform1fv(prog.id, u.loc, count, value);
					GLType.Vec2: gl.ProgramUniform2fv(prog.id, u.loc, count, value);
					GLType.Vec3: gl.ProgramUniform3fv(prog.id, u.loc, count, value);
					GLType.Vec4: gl.ProgramUniform4fv(prog.id, u.loc, count, value);
					GLType.Int32: gl.ProgramUniform1i(prog.id, u.loc, pSint32(value)^);
					GLType.Mat4: gl.ProgramUniformMatrix4fv(prog.id, u.loc, count, gl.FALSE, value);
				{$ifdef Debug} else Log('Необработанный тип юниформа: "' + GLTypeIds[u.type_] + '"', logError); {$endif}
				end;
			end else
			begin
				_glUseProgram(@prog);
				case u.type_ of
					GLType.Float: gl.Uniform1fv(u.loc, count, value);
					GLType.Vec2: gl.Uniform2fv(u.loc, count, value);
					GLType.Vec3: gl.Uniform3fv(u.loc, count, value);
					GLType.Vec4: gl.Uniform4fv(u.loc, count, value);
					GLType.Int32: gl.Uniform1i(u.loc, pSint32(value)^);
					GLType.Mat4: gl.UniformMatrix4fv(u.loc, count, gl.FALSE, value);
				{$ifdef Debug} else Log('Необработанный тип юниформа: "' + GLTypeIds[u.type_] + '"', logError); {$endif}
				end;
			end;
		{$ifdef Debug} _curSended += ulong(count) * GLTypeInfo[u.type_].sizeof; {$endif}
		end else
		begin
		{$ifdef Debug}
			if Assigned(value) and (prog.samplers[u.texUnit].target <> pGLTexture(value)^.target) then
				Log('Несовпадение типов сэмплера: нужен ' + GLTextureTargetIds[prog.samplers[u.texUnit].target] +
					', получен ' + GLTextureTargetIds[pGLTexture(value)^.target], logError);
			Assert(u.texUnit < prog.nSamplers);
		{$endif}
			if Assigned(value) then
				id := pGLTexture(value)^.id
			else
				id := 0;
			if prog.samplers[u.texUnit].id <> id then
			begin
				prog.samplers[u.texUnit].id := id;
				if _activeProgPtr = @prog then _glBindTexture(u.texUnit, prog.samplers[u.texUnit].target, id);
			end;
		end;
		paranoia('SetUniform');
		leave_call;
	end;

	procedure tOpenGL._SetUbos(var prog: GLProgram; const bufs: array of pGLBuffer);
	var
		i: sint;
	begin
		if length(bufs) <> prog.nUbos then
		begin
		{$ifdef Debug} Log('SetUbos: неверное количество UBO', logError); {$endif}
			exit;
		end;
		for i := 0 to prog.nUbos - 1 do
		begin
			prog.ubos[i].id := bufs[i]^.id;
			if _activeProgPtr = @prog then _BindUBO(i, bufs[i]^.id);
		end;
		paranoia('SetUbos');
	end;

	procedure tOpenGL._CreateTexture(out tex: tGLTexture; target: GLTextureTarget);
	var
		glID: gl.uint;
	begin
		gl.GenTextures(1, @glID);
		tex.target := target;
		tex.id := glID;
		tex.nLevels := 0;
		tex.bad := no;
	{$ifdef Debug} NoteCreate(rc_Texture); {$endif}
		paranoia('CreateTexture');
	end;

	procedure tOpenGL._DeleteTexture(var tex: tGLTexture);
	var
		glID: gl.uint;
		i: sint;
	begin
		for i := 0 to MaxTextureUnits - 1 do
			if _boundTextures[i] = tex.id then
				_glBindTexture(i, tex.target, 0);
		glID := tex.id;
		gl.DeleteTextures(1, @glID);
		tex.id := 0;
	{$ifdef Debug} if glID > 0 then NoteDestroy(rc_Texture); {$endif}
		paranoia('DeleteTexture');
	end;

	procedure tOpenGL._TexImage(
		var tex: tGLTexture; const size: UintVec3; dataFormat, intFormat: GLImageFormat; dataSize: size_t; data: pointer; level: uint);
	var
		texEnum: gl.enum;
		lv: sint;
		compressed: gl.int;
		{$ifdef Debug} uncompressedSize, compressedSize: gl.int; {$endif}
	begin
		CheckError {$ifdef Debug}('перед TexImage'){$endif};
		_glBindTexture(0, tex.target, tex.id);
		tex.fmt := intFormat;
		tex.nlevels := Max(tex.nlevels, level + 1);
		if tex.target = GLtexture_Cube then
			texEnum := GLCubeSideEnums[GLCubeSide(uint(level) mod TextureTargetsInfo[tex.target].faces)]
		else
			texEnum := GLTextureTargetEnums[tex.target];
		lv := uint(level) div TextureTargetsInfo[tex.target].faces;

		if (GLformat_Compressed in GLImageFormatsInfo[dataFormat].flags) and (intFormat = dataFormat) then
		begin
			case tex.target of
				GLtexture_2D, GLtexture_Cube :
					gl.CompressedTexImage2D(texEnum, lv, GLFormats[intFormat].internalFormat,
						size.X, size.Y, 0, dataSize, data);
				GLtexture_3D :
					gl.CompressedTexImage3D(texEnum, lv, GLFormats[intFormat].internalFormat,
						size.X, size.Y, size.Z, 0, dataSize, data);
			end;
		end else
		begin
			if (GLformat_Compressed in GLImageFormatsInfo[dataFormat].flags) and (intFormat <> dataFormat) then
			begin
			{$ifdef Debug} Log('OpenGL не поддерживает рекомпрессию в TexImage*, используй GetTexImage', logError); {$endif}
				exit;
			end;
			case tex.target of
				GLtexture_2D, GLtexture_Cube :
					begin
						gl.TexImage2D(texEnum, lv, GLFormats[intFormat].internalFormat,
							size.X, size.Y, 0, GLFormats[dataFormat].components, GLFormats[dataFormat].ctype,
							data);
					end;
				GLtexture_3D :
					gl.TexImage3D(texEnum, lv, GLFormats[intFormat].internalFormat,
						size.X, size.Y, size.Z, 0, GLFormats[dataFormat].components, GLFormats[dataFormat].ctype,
						data);
			end;
		end;
		if (not (GLformat_Compressed in GLImageFormatsInfo[dataFormat].flags)) and
			(GLformat_Compressed in GLImageFormatsInfo[intFormat].flags) then
		begin
			compressed := gl.FALSE;
			gl.GetTexLevelParameteriv(texEnum, lv, gl.TEXTURE_COMPRESSED, @compressed);
			if compressed <> gl.TRUE then
			begin
			{$ifdef Debug} LogR('Сжатый формат текстуры не поддерживается, попытаюсь загрузить как несжатый. ', logWarning); {$endif}
				_TexImage(tex, size, dataFormat, ImageFormat8[GLImageFormatsInfo[intFormat].nChannels], dataSize, data, level);
				exit;
			end;
		{$ifdef Debug}
			uncompressedSize := GLImageFormatsInfo[dataFormat].pixelSize * size.Product;
			gl.GetTexLevelParameteriv(texEnum, lv, gl.TEXTURE_COMPRESSED_IMAGE_SIZE, @compressedSize);
			Log('Сжато с ' + ToStringSuff(uncompressedSize) + ' до ' + ToStringSuff(compressedSize) + ', коэф. = ' + ToString(compressedSize / uncompressedSize * 100.0) + '%... ');
		{$endif}
		end;
		// return new format, if turned into uncompressed, for example
		tex.fmt := intFormat;

		if CheckError {$ifdef Debug}('TexImage'){$endif} then tex.bad := yes;
	end;

	procedure tOpenGL._TexImage(var tex: tGLTexture; const size: UintVec3; format: GLImageFormat; dataSize: size_t; data: pointer; level: uint);
	begin
		_TexImage(tex, size, format, format, dataSize, data, level);
	end;

	procedure tOpenGL._TexSubImage(var tex: tGLTexture; const offset, size: UintVec3; format: GLImageFormat; dataSize: size_t; data: pointer; level: uint);
	var
		texEnum: gl.enum;
		lv: sint;
	begin
		_glBindTexture(0, tex.target, tex.id);
		if tex.target = GLtexture_Cube then
			texEnum := GLCubeSideEnums[GLCubeSide(uint(level) mod TextureTargetsInfo[tex.target].faces)]
		else
			texEnum := GLTextureTargetEnums[tex.target];
		lv := uint(level) div TextureTargetsInfo[tex.target].faces;

		if GLformat_Compressed in GLImageFormatsInfo[tex.fmt].flags then
		begin
		{$ifdef Debug} if tex.fmt <> format then Log('TexSubImage: сжатые форматы не совпадают, ' + GLImageFormatIds[tex.fmt] + ' <-> ' + GLImageFormatIds[format], logError); {$endif}
			case tex.target of
				GLtexture_2D, GLtexture_Cube: gl.CompressedTexSubImage2D(texEnum, lv, offset.x, offset.y, size.x, size.y, GLFormats[format].internalFormat, dataSize, data);
				GLtexture_3D: gl.CompressedTexSubImage3D(texEnum, lv, offset.x, offset.y, offset.z, size.x, size.y, size.z, GLFormats[format].internalFormat, dataSize, data);
			end;
			paranoia('TexSubImage(compressed)');
		end else
		begin
			Assert(dataSize = size.Product * GLImageFormatsInfo[format].pixelSize);
			case tex.target of
				GLtexture_2D, GLtexture_Cube: gl.TexSubImage2D(texEnum, lv, offset.x, offset.y, size.x, size.y, GLFormats[format].components, GLFormats[format].ctype, data);
				GLtexture_3D: gl.TexSubImage3D(texEnum, lv, offset.x, offset.y, offset.z, size.x, size.y, size.z, GLFormats[format].components, GLFormats[format].ctype, data);
			end;
			paranoia('TexSubImage');
		end;
	end;

	procedure tOpenGL._SetTextureParams(var tex: tGLTexture; const params: GLTextureParamsRec; paramFields: GLTextureParamSet);
	const
		WrapModeEnums: array[GLTextureWrapMode] of gl.enum = (gl.&REPEAT, gl.CLAMP_TO_EDGE);
		FilterEnums: array[GLTextureFilter] of gl.enum = (gl.NEAREST, gl.LINEAR);
		FilterEnums_wMIP: array[GLTextureFilter] of gl.enum = (gl.NEAREST_MIPMAP_NEAREST, gl.LINEAR_MIPMAP_LINEAR);
	var
		i: sint;
		swizzle: array[0 .. 3] of gl.int;
	begin
		_glBindTexture(0, tex.target, tex.id);

		if GLtexparam_Wrap in paramFields then
		begin
			gl.TexParameteri(GLTextureTargetEnums[tex.target], gl.TEXTURE_WRAP_S, WrapModeEnums[params.wrap]);
			gl.TexParameteri(GLTextureTargetEnums[tex.target], gl.TEXTURE_WRAP_T, WrapModeEnums[params.wrap]);
			gl.TexParameteri(GLTextureTargetEnums[tex.target], gl.TEXTURE_WRAP_R, WrapModeEnums[params.wrap]);
		end;
		if GLtexparam_MagFilter in paramFields then
			gl.TexParameteri(GLTextureTargetEnums[tex.target], gl.TEXTURE_MAG_FILTER, FilterEnums[params.magFilter]);
		if GLtexparam_MinFilter in paramFields then
			if tex.nLevels > TextureTargetsInfo[tex.target].faces then
				gl.TexParameteri(GLTextureTargetEnums[tex.target], gl.TEXTURE_MIN_FILTER, FilterEnums_wMIP[params.minFilter])
			else
				gl.TexParameteri(GLTextureTargetEnums[tex.target], gl.TEXTURE_MIN_FILTER, FilterEnums[params.minFilter]);

		if GLtexparam_Aniso in paramFields then
			gl.TexParameterf(GLTextureTargetEnums[tex.target], gl.TEXTURE_MAX_ANISOTROPY, Clamp(params.anisotropy, 1.0, maxTextureAnisotropy));
		if GLtexparam_Swizzle in paramFields then
		begin
			for i := 0 to 3 do swizzle[i] := SwizzleEnums[params.swizzle[i]];
			gl.TexParameteriv(GLTextureTargetEnums[tex.target], gl.TEXTURE_SWIZZLE_RGBA, @swizzle[0]);
		end;
		paranoia('SetTextureParams');
	end;

var
	getTexImageRecDepth: sint = 0;

	function tOpenGL._GetTexImage(var tex: tGLTexture; level: uint; format: GLImageFormat): pointer;
	label _finally_;
		procedure DevilGet(sx, sy, sz: sint);
		var
			tt: tGLTexture;
			ttformat: GLImageFormat;
			ttMem: pointer;
		begin
		{$ifdef Debug} Log('DEVIL WORKS...', logWarning); {$endif}
			ttformat := tex.fmt;
			if GLformat_Compressed in GLImageFormatsInfo[ttformat].flags then
				ttformat := ImageFormat8[GLImageFormatsInfo[ttformat].nChannels];
			ttMem := _GetTexImage(tex, level, ttformat);
			_CreateTexture(tt, tex.target);
			_TexImage(tt, UintVec3.Make(sx, sy, sz), ttformat, format, GetTextureDataSize(sx, sy, sz, ttformat), ttMem, 0);
			FreeMem(ttMem);
			result := _GetTexImage(tt, 0, format);
			_DeleteTexture(tt);
		end;
	var
		neededDevil: boolean;
		texEnum: gl.enum;
		lv: sint;
		glsize, glsizex, glsizey, glsizez: gl.int;
	begin
		result := nil;
		inc(getTexImageRecDepth);
		if getTexImageRecDepth > 3 then goto _finally_;

		if tex.target = GLtexture_Cube then
			texEnum := GLCubeSideEnums[GLCubeSide(level mod 6)]
		else
			texEnum := GLTextureTargetEnums[tex.target];
		lv := level div TextureTargetsInfo[tex.target].faces;
		_glBindTexture(0, tex.target, tex.id);

		glsizex := 0;
		glsizey := 0;
		glsizez := 0;
		gl.GetTexLevelParameteriv(texEnum, lv, gl.TEXTURE_WIDTH, @glsizex);
		gl.GetTexLevelParameteriv(texEnum, lv, gl.TEXTURE_HEIGHT, @glsizey);
		gl.GetTexLevelParameteriv(texEnum, lv, gl.TEXTURE_DEPTH, @glsizez);
		if (glsizex <= 0) or (glsizey <= 0) or (glsizez <= 0) then goto _finally_;

		neededDevil := no;
		if (GLformat_Compressed in GLImageFormatsInfo[format].flags) and (tex.fmt <> format) then
			neededDevil := yes;
		if (GLformat_Compressed in GLImageFormatsInfo[tex.fmt].flags) and (tex.fmt <> format) then
			if ImageFormat8[GLImageFormatsInfo[tex.fmt].nChannels] <> format then
				neededDevil := yes;
		if neededDevil then
		begin
			DevilGet(glsizex, glsizey, glsizez);
			goto _finally_;
		end;

		if GLformat_Compressed in GLImageFormatsInfo[format].flags then
		begin
			glsize := 0;
			gl.GetTexLevelParameteriv(texEnum, lv, gl.TEXTURE_COMPRESSED_IMAGE_SIZE, @glsize);
			if glsize = 0 then goto _finally_;
			result := GetMem(glsize);
			gl.GetCompressedTexImage(texEnum, lv, result);
		end else
		begin
			result := GetMem(GetTextureDataSize(glsizex, glsizey, glsizez, format));
			gl.GetTexImage(texEnum, lv, GLFormats[format].components, GLFormats[format].ctype, result);
		end;
	_finally_ :
		dec(getTexImageRecDepth);
		paranoia('GetTexImage');
	end;

	procedure tOpenGL._CreateRenderTarget(var fb: GLRenderTarget);
	var
		glID: gl.uint;
	begin
		gl.GenFramebuffers(1, @glID);
		fb.id := glID;
	{$ifdef Debug} NoteCreate(rc_Framebuffer); {$endif}
		paranoia('CreateFramebuffer');
	end;

	procedure tOpenGL._DeleteRenderTarget(var fb: GLRenderTarget);
	var
		glID: gl.uint;
	begin
		if _activeFramebuffer = fb.id then _glBindFramebuffer(ScreenRT);
		glID := fb.id;
		gl.DeleteFramebuffers(1, @glID);
	{$ifdef Debug} if fb.id > 0 then NoteDestroy(rc_Framebuffer); {$endif}
		paranoia('DeleteFramebuffers');
	end;

	procedure tOpenGL._ResizeRenderTarget(var fb: GLRenderTarget; const size: UintVec2);
	begin
		fb.size := size;
	end;

	procedure tOpenGL._AttachRenderTexture(var fb: GLRenderTarget; var tex: tGLTexture; level: uint; target: GLRenderBuffer; targetN: uint);
	var
		texEnum: gl.enum;
	begin
		if tex.bad or fb.bad then exit;
		CheckError {$ifdef Debug}('перед AttachRenderTexture'){$endif};
	{$ifdef Debug}
		if tex.id = 0 then
		begin
			Log('Используй DetachRenderTexture вместо AttachRenderTexture с нулевой текстурой', logWarning);
			_DetachRenderTexture(fb, target, targetN);
			exit;
		end;
	{$endif}
		_glBindFramebuffer(fb);
		if tex.target = GLtexture_Cube then
			texEnum := GLCubeSideEnums[GLCubeSide(level mod 6)]
		else
			texEnum := GLTextureTargetEnums[tex.target];
		gl.FramebufferTexture2D(gl.FRAMEBUFFER, GLRenderBufferEnums[target] + targetN, texEnum, tex.id, level div TextureTargetsInfo[tex.target].faces);
		if (target = GLbuffer_Color) and (not (targetN in fb.colorRTs)) then
		begin
			Include(fb.colorRTs, targetN);
			_UpdateDrawBuffers(fb);
		end;
		if CheckError {$ifdef Debug}('AttachRenderTexture'){$endif} then fb.bad := yes;
	end;

	procedure tOpenGL._DetachRenderTexture(var fb: GLRenderTarget; target: GLRenderBuffer; targetN: uint);
	begin
		if fb.bad then exit;
		_glBindFramebuffer(fb);
		gl.FramebufferTexture2D(gl.FRAMEBUFFER, GLRenderBufferEnums[target] + targetN, gl.TEXTURE_2D, 0, 0);
		if (target = GLbuffer_Color) and (targetN in fb.colorRTs) then
		begin
			Exclude(fb.colorRTs, targetN);
			_UpdateDrawBuffers(fb);
		end;
		paranoia('DetachRenderTexture');
	end;

	function tOpenGL._ValidateRenderTarget(var fb: GLRenderTarget): GLRenderTargetStatus;
	var
		r: gl.enum;
	begin
		if fb.bad then exit(GLrt_Incomplete);
		_glBindFramebuffer(fb);
		r := gl.CheckFramebufferStatus(gl.FRAMEBUFFER);
		case r of
			gl.FRAMEBUFFER_COMPLETE,
			gl.FRAMEBUFFER_UNDEFINED: // возвращается для главной RT, т. е. окна
				result := GLrt_Complete;
			gl.FRAMEBUFFER_INCOMPLETE_ATTACHMENT, gl.FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT,
			gl.FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER, gl.FRAMEBUFFER_INCOMPLETE_READ_BUFFER,
			gl.FRAMEBUFFER_INCOMPLETE_MULTISAMPLE: result := GLrt_Incomplete;
			gl.FRAMEBUFFER_UNSUPPORTED: result := GLrt_Unsupported;
			else
				begin
				{$ifdef Debug} Log('Неизвестный результат ValidateRenderTarget (OGL: ' + ToString(r) + ')', logError); {$endif}
					result := GLrt_Incomplete;
				end;
		end;
		if result <> GLrt_Complete then fb.bad := yes;
		paranoia('ValidateRenderTarget');
	end;

	function tOpenGL._GetRTImage(var fb: GLRenderTarget; format: GLImageFormat; mrtN: uint): pointer;
	begin
		CheckError {$ifdef Debug}('перед GetRTImage') {$endif};
		result := nil;
		if fb.bad then exit;
		_glBindFramebuffer(fb);
		if fb.id = 0 then
			gl.ReadBuffer(gl.BACK)
		else
			gl.ReadBuffer(gl.COLOR_ATTACHMENT0 + mrtN);
		result := GetMem(GetTextureDataSize(fb.size, format));
		gl.ReadPixels(0, 0, fb.size.X, fb.size.Y, GLFormats[format].components, GLFormats[format].ctype, result);
		if CheckError {$ifdef Debug}('GetRTImage') {$endif} then FreeMem(result);
	end;

end.
