unit OpenGL_Headers;

{$include opts.inc}

interface

uses
	USystem, Errors, Windowing, DynamicLoader {$ifdef Debug}, Utils, ULog {$endif};

{$ifdef Debug}
	{-$define CrashVAO}
	{-$define CrashInsta}
	{-$define CrashUBO}
	{-$define CrashPrimitiveRestart}
	{-$define CrashDSA}
	{-$define CrashBinaryShaders}
	{-$define CrashDebug}
	{-$define CrashCreateBuffers}
	{-$define CrashVABinding}
{$endif}

	function LoadOpenGL: boolean;
	procedure UnloadOpenGL;

type
	gl = class
	type
		enum     = uint32;   pEnum    = ^enum;
		boolean  = uint8;    pBoolean = ^boolean;
		bitfield = uint32;
		byte     = sint8;    pByte    = ^byte;
		short    = sint16;   pShort   = ^short;
		int      = sint32;   pInt     = ^int;
		intptr   = PtrInt;
		sizei    = sint32;   pSizei   = ^sizei;
		sizeiptr = PtrInt;
		ubyte    = uint8;    pUbyte   = ^ubyte;
		ushort   = uint16;   pUshort  = ^ushort;
		uint     = uint32;   pUint    = ^uint;
		float    = float32;  pFloat   = ^float;
		clampf   = float32;
		clampd   = float64;
		ppchar   = ^pchar;
		sync     = PtrInt;   pSync    = ^sync;

	const
		FALSE                                                  = 0;
		TRUE                                                   = 1;

	// VERSION_1_0
	const
	// AttribMask
		DEPTH_BUFFER_BIT                                       = $00000100;
		STENCIL_BUFFER_BIT                                     = $00000400;
		COLOR_BUFFER_BIT                                       = $00004000;
	// BeginMode
		POINTS                                                 = $0000;
		LINES                                                  = $0001;
		LINE_LOOP                                              = $0002;
		LINE_STRIP                                             = $0003;
		TRIANGLES                                              = $0004;
		TRIANGLE_STRIP                                         = $0005;
		TRIANGLE_FAN                                           = $0006;
	// AlphaFunction
		NEVER                                                  = $0200;
		LESS                                                   = $0201;
		EQUAL                                                  = $0202;
		LEQUAL                                                 = $0203;
		GREATER                                                = $0204;
		NOTEQUAL                                               = $0205;
		GEQUAL                                                 = $0206;
		ALWAYS                                                 = $0207;
	// BlendingFactorDest
		ZERO                                                   = 0;
		ONE                                                    = 1;
		SRC_COLOR                                              = $0300;
		ONE_MINUS_SRC_COLOR                                    = $0301;
		SRC_ALPHA                                              = $0302;
		ONE_MINUS_SRC_ALPHA                                    = $0303;
		DST_ALPHA                                              = $0304;
		ONE_MINUS_DST_ALPHA                                    = $0305;
	// BlendingFactorSrc
		DST_COLOR                                              = $0306;
		ONE_MINUS_DST_COLOR                                    = $0307;
		SRC_ALPHA_SATURATE                                     = $0308;
	// DrawBufferMode
		NONE                                                   = 0;
		FRONT_LEFT                                             = $0400;
		FRONT_RIGHT                                            = $0401;
		BACK_LEFT                                              = $0402;
		BACK_RIGHT                                             = $0403;
		FRONT                                                  = $0404;
		BACK                                                   = $0405;
		LEFT                                                   = $0406;
		RIGHT                                                  = $0407;
		FRONT_AND_BACK                                         = $0408;
	// ErrorCode
		NO_ERROR                                               = 0;
		INVALID_ENUM                                           = $0500;
		INVALID_VALUE                                          = $0501;
		INVALID_OPERATION                                      = $0502;
		OUT_OF_MEMORY                                          = $0505;
	// FrontFaceDirection
		CW                                                     = $0900;
		CCW                                                    = $0901;
	// GetPName
		POINT_SIZE                                             = $0B11;
		POINT_SIZE_RANGE                                       = $0B12;
		POINT_SIZE_GRANULARITY                                 = $0B13;
		LINE_SMOOTH                                            = $0B20;
		LINE_WIDTH                                             = $0B21;
		LINE_WIDTH_RANGE                                       = $0B22;
		LINE_WIDTH_GRANULARITY                                 = $0B23;
		POLYGON_SMOOTH                                         = $0B41;
		CULL_FACE                                              = $0B44;
		CULL_FACE_MODE                                         = $0B45;
		FRONT_FACE                                             = $0B46;
		DEPTH_RANGE                                            = $0B70;
		DEPTH_TEST                                             = $0B71;
		DEPTH_WRITEMASK                                        = $0B72;
		DEPTH_CLEAR_VALUE                                      = $0B73;
		DEPTH_FUNC                                             = $0B74;
		STENCIL_TEST                                           = $0B90;
		STENCIL_CLEAR_VALUE                                    = $0B91;
		STENCIL_FUNC                                           = $0B92;
		STENCIL_VALUE_MASK                                     = $0B93;
		STENCIL_FAIL                                           = $0B94;
		STENCIL_PASS_DEPTH_FAIL                                = $0B95;
		STENCIL_PASS_DEPTH_PASS                                = $0B96;
		STENCIL_REF                                            = $0B97;
		STENCIL_WRITEMASK                                      = $0B98;
		VIEWPORT_                                              = $0BA2;
		DITHER                                                 = $0BD0;
		BLEND_DST                                              = $0BE0;
		BLEND_SRC                                              = $0BE1;
		BLEND                                                  = $0BE2;
		LOGIC_OP_MODE                                          = $0BF0;
		COLOR_LOGIC_OP                                         = $0BF2;
		DRAW_BUFFER                                            = $0C01;
		READ_BUFFER                                            = $0C02;
		SCISSOR_BOX                                            = $0C10;
		SCISSOR_TEST                                           = $0C11;
		COLOR_CLEAR_VALUE                                      = $0C22;
		COLOR_WRITEMASK                                        = $0C23;
		DOUBLEBUFFER                                           = $0C32;
		STEREO                                                 = $0C33;
		LINE_SMOOTH_HINT                                       = $0C52;
		POLYGON_SMOOTH_HINT                                    = $0C53;
		UNPACK_SWAP_BYTES                                      = $0CF0;
		UNPACK_LSB_FIRST                                       = $0CF1;
		UNPACK_ROW_LENGTH                                      = $0CF2;
		UNPACK_SKIP_ROWS                                       = $0CF3;
		UNPACK_SKIP_PIXELS                                     = $0CF4;
		UNPACK_ALIGNMENT                                       = $0CF5;
		PACK_SWAP_BYTES                                        = $0D00;
		PACK_LSB_FIRST                                         = $0D01;
		PACK_ROW_LENGTH                                        = $0D02;
		PACK_SKIP_ROWS                                         = $0D03;
		PACK_SKIP_PIXELS                                       = $0D04;
		PACK_ALIGNMENT                                         = $0D05;
		MAX_TEXTURE_SIZE                                       = $0D33;
		MAX_VIEWPORT_DIMS                                      = $0D3A;
		SUBPIXEL_BITS                                          = $0D50;
		TEXTURE_1D                                             = $0DE0;
		TEXTURE_2D                                             = $0DE1;
		POLYGON_OFFSET_UNITS                                   = $2A00;
		POLYGON_OFFSET_POINT                                   = $2A01;
		POLYGON_OFFSET_LINE                                    = $2A02;
		POLYGON_OFFSET_FILL                                    = $8037;
		POLYGON_OFFSET_FACTOR                                  = $8038;
		TEXTURE_BINDING_1D                                     = $8068;
		TEXTURE_BINDING_2D                                     = $8069;
	// GetTextureParameter
		TEXTURE_WIDTH                                          = $1000;
		TEXTURE_HEIGHT                                         = $1001;
		TEXTURE_INTERNAL_FORMAT                                = $1003;
		TEXTURE_BORDER_COLOR                                   = $1004;
		TEXTURE_RED_SIZE                                       = $805C;
		TEXTURE_GREEN_SIZE                                     = $805D;
		TEXTURE_BLUE_SIZE                                      = $805E;
		TEXTURE_ALPHA_SIZE                                     = $805F;
	// HintMode
		DONT_CARE                                              = $1100;
		FASTEST                                                = $1101;
		NICEST                                                 = $1102;
	// DataType
		{BYTE} SIGNED_BYTE                                     = $1400;
		UNSIGNED_BYTE                                          = $1401;
		{SHORT} SIGNED_SHORT                                   = $1402;
		UNSIGNED_SHORT                                         = $1403;
		{INT} SIGNED_INT                                       = $1404;
		UNSIGNED_INT                                           = $1405;
		FLOAT_TYPE                                             = $1406;
		DOUBLE_TYPE                                            = $140A;
	// LogicOp
		CLEAR_                                                 = $1500;
		&AND                                                   = $1501;
		AND_REVERSE                                            = $1502;
		COPY                                                   = $1503;
		AND_INVERTED                                           = $1504;
		NOOP                                                   = $1505;
		&XOR                                                   = $1506;
		&OR                                                    = $1507;
		NOR                                                    = $1508;
		EQUIV                                                  = $1509;
		INVERT                                                 = $150A;
		OR_REVERSE                                             = $150B;
		COPY_INVERTED                                          = $150C;
		OR_INVERTED                                            = $150D;
		NAND                                                   = $150E;
		&SET                                                   = $150F;
	// MatrixMode (for gl3.h, FBO attachment type)
		TEXTURE                                                = $1702;
	// PixelCopyType
		COLOR                                                  = $1800;
		DEPTH                                                  = $1801;
		STENCIL                                                = $1802;
	// PixelFormat
		STENCIL_INDEX                                          = $1901;
		DEPTH_COMPONENT                                        = $1902;
		RED                                                    = $1903;
		GREEN                                                  = $1904;
		BLUE                                                   = $1905;
		ALPHA                                                  = $1906;
		RGB                                                    = $1907;
		RGBA                                                   = $1908;
	// PolygonMode
		POINT                                                  = $1B00;
		LINE                                                   = $1B01;
		FILL                                                   = $1B02;
	// StencilOp
		KEEP                                                   = $1E00;
		REPLACE                                                = $1E01;
		INCR                                                   = $1E02;
		DECR                                                   = $1E03;
	// StringName
		VENDOR                                                 = $1F00;
		RENDERER                                               = $1F01;
		VERSION                                                = $1F02;
		EXTENSIONS                                             = $1F03;
	// TextureMagFilter
		NEAREST                                                = $2600;
		LINEAR                                                 = $2601;
	// TextureMinFilter
		NEAREST_MIPMAP_NEAREST                                 = $2700;
		LINEAR_MIPMAP_NEAREST                                  = $2701;
		NEAREST_MIPMAP_LINEAR                                  = $2702;
		LINEAR_MIPMAP_LINEAR                                   = $2703;
	// TextureParameterName
		TEXTURE_MAG_FILTER                                     = $2800;
		TEXTURE_MIN_FILTER                                     = $2801;
		TEXTURE_WRAP_S                                         = $2802;
		TEXTURE_WRAP_T                                         = $2803;
	// TextureTarget
		PROXY_TEXTURE_1D                                       = $8063;
		PROXY_TEXTURE_2D                                       = $8064;
	// TextureWrapMode
		&REPEAT                                                = $2901;
	// PixelInternalFormat
		R3_G3_B2                                               = $2A10;
		RGB4                                                   = $804F;
		RGB5                                                   = $8050;
		RGB8                                                   = $8051;
		RGB10                                                  = $8052;
		RGB12                                                  = $8053;
		RGB16                                                  = $8054;
		RGBA2                                                  = $8055;
		RGBA4                                                  = $8056;
		RGB5_A1                                                = $8057;
		RGBA8                                                  = $8058;
		RGB10_A2                                               = $8059;
		RGBA12                                                 = $805A;
		RGBA16                                                 = $805B;

	class var
		CullFace: procedure(mode: enum); stdcall;
		FrontFace: procedure(mode: enum); stdcall;
		// Hint: procedure(target: enum; mode: enum); stdcall;
		// LineWidth: procedure(width: float); stdcall;
		// PointSize: procedure(size: float); stdcall;
		PolygonMode: procedure(face: enum; mode: enum); stdcall;
		// Scissor: procedure(x, y: int; width, height: sizei); stdcall;
		TexParameterf: procedure(target: enum; pname: enum; param: float); stdcall;
		// TexParameterfv: procedure(target: enum; pname: enum; params: pFloat); stdcall;
		TexParameteri: procedure(target: enum; pname: enum; param: int); stdcall;
		TexParameteriv: procedure(target: enum; pname: enum; params: pInt); stdcall;
		// TexImage1D: procedure(target: enum; level: int; internalformat: int; width: sizei; border: int; format: enum; type_: enum; pixels: pointer); stdcall;
		TexImage2D: procedure(target: enum; level: int; internalformat: int; width: sizei; height: sizei; border: int; format: enum; type_: enum; pixels: pointer); stdcall;
		// DrawBuffer: procedure(mode: enum); stdcall;
		Clear: procedure(mask: bitfield); stdcall;
		ClearColor: procedure(red: clampf; green: clampf; blue: clampf; alpha: clampf); stdcall;
		// ClearStencil: procedure(s: int); stdcall;
		// ClearDepth: procedure(depth: clampd); stdcall;
		// StencilMask: procedure(mask: uint); stdcall;
		// ColorMask: procedure(red: boolean; green: boolean; blue: boolean; alpha: boolean); stdcall;
		DepthMask: procedure(flag: boolean); stdcall;
		Disable: procedure(cap: enum); stdcall;
		Enable: procedure(cap: enum); stdcall;
		Finish: procedure; stdcall;
		// Flush: procedure; stdcall;
		BlendFunc: procedure(sfactor, dfactor: enum); stdcall;
		// LogicOp: procedure(opcode: enum); stdcall;
		// StencilFunc: procedure(func: enum; ref: int; mask: uint); stdcall;
		// StencilOp: procedure(fail_: enum; zfail: enum; zpass: enum); stdcall;
		DepthFunc: procedure(func: enum); stdcall;
		// PixelStoref: procedure(pname: enum; param: float); stdcall;
		PixelStorei: procedure(pname: enum; param: int); stdcall;
		ReadBuffer: procedure(mode: enum); stdcall;
		ReadPixels: procedure(x: int; y: int; width: sizei; height: sizei; format: enum; type_: enum; pixels: pointer); stdcall;
		// GetBooleanv: procedure(pname: enum; params: pBoolean); stdcall;
		// GetDoublev: procedure(pname: enum; params: pDouble); stdcall;
		GetError: function: enum; stdcall;
		GetFloatv: procedure(pname: enum; params: pFloat); stdcall;
		GetIntegerv: procedure(pname: enum; params: pInt); stdcall;
		GetString: function(name: enum): pChar; stdcall;
		GetTexImage: procedure(target: enum; level: int; format: enum; type_: enum; pixels: pointer); stdcall;
		// GetTexParameterfv: procedure(target: enum; pname: enum; params: pFloat); stdcall;
		// GetTexParameteriv: procedure(target: enum; pname: enum; params: pInt); stdcall;
		// GetTexLevelParameterfv: procedure(target: enum; level: int; pname: enum; params: pFloat); stdcall;
		GetTexLevelParameteriv: procedure(target: enum; level: int; pname: enum; params: pInt); stdcall;
		// IsEnabled: function(cap: enum): boolean; stdcall;
		// DepthRange: procedure(near: clampd; far: clampd); stdcall;
		Viewport: procedure(x: int; y: int; width: sizei; height: sizei); stdcall;

	// VERSION_1_1
	class var
		// DrawArrays: procedure(mode: enum; first: int; count: sizei); stdcall;
		DrawElements: procedure(mode: enum; count: sizei; type_: enum; indices: pointer); stdcall;
		// GetPointerv: procedure(pname: enum; params: pointer); stdcall;
		// PolygonOffset: procedure(factor: float; units: float); stdcall;
		// CopyTexImage1D: procedure(target: enum; level: int; internalformat: enum; x: int; y: int; width: sizei; border: int); stdcall;
		// CopyTexImage2D: procedure(target: enum; level: int; internalformat: enum; x: int; y: int; width: sizei; height: sizei; border: int); stdcall;
		// CopyTexSubImage1D: procedure(target: enum; level: int; xoffset: int; x: int; y: int; width: sizei); stdcall;
		// CopyTexSubImage2D: procedure(target: enum; level: int; xoffset: int; yoffset: int; x: int; y: int; width: sizei; height: sizei); stdcall;
		// TexSubImage1D: procedure(target: enum; level: int; xoffset: int; width: sizei; format: enum; type_: enum; pixels: pointer); stdcall;
		TexSubImage2D: procedure(target: enum; level: int; xoffset, yoffset: int; width, height: sizei; format, type_: enum; pixels: pointer); stdcall;
		BindTexture: procedure(target: enum; texture: uint); stdcall;
		DeleteTextures: procedure(n: sizei; textures: pUint); stdcall;
		GenTextures: procedure(n: sizei; textures: pUint); stdcall;
		// IsTexture: function(texture: uint): boolean; stdcall;

	// VERSION_1_2
	const
		UNSIGNED_BYTE_3_3_2                                    = $8032;
		UNSIGNED_SHORT_4_4_4_4                                 = $8033;
		UNSIGNED_SHORT_5_5_5_1                                 = $8034;
		UNSIGNED_INT_8_8_8_8                                   = $8035;
		UNSIGNED_INT_10_10_10_2                                = $8036;
		TEXTURE_BINDING_3D                                     = $806A;
		PACK_SKIP_IMAGES                                       = $806B;
		PACK_IMAGE_HEIGHT                                      = $806C;
		UNPACK_SKIP_IMAGES                                     = $806D;
		UNPACK_IMAGE_HEIGHT                                    = $806E;
		TEXTURE_3D                                             = $806F;
		PROXY_TEXTURE_3D                                       = $8070;
		TEXTURE_DEPTH                                          = $8071;
		TEXTURE_WRAP_R                                         = $8072;
		MAX_3D_TEXTURE_SIZE                                    = $8073;
		UNSIGNED_BYTE_2_3_3_REV                                = $8362;
		UNSIGNED_SHORT_5_6_5                                   = $8363;
		UNSIGNED_SHORT_5_6_5_REV                               = $8364;
		UNSIGNED_SHORT_4_4_4_4_REV                             = $8365;
		UNSIGNED_SHORT_1_5_5_5_REV                             = $8366;
		UNSIGNED_INT_8_8_8_8_REV                               = $8367;
		UNSIGNED_INT_2_10_10_10_REV                            = $8368;
		BGR                                                    = $80E0;
		BGRA                                                   = $80E1;
		MAX_ELEMENTS_VERTICES                                  = $80E8;
		MAX_ELEMENTS_INDICES                                   = $80E9;
		CLAMP_TO_EDGE                                          = $812F;
		TEXTURE_MIN_LOD                                        = $813A;
		TEXTURE_MAX_LOD                                        = $813B;
		TEXTURE_BASE_LEVEL                                     = $813C;
		TEXTURE_MAX_LEVEL                                      = $813D;
		SMOOTH_POINT_SIZE_RANGE                                = $0B12;
		SMOOTH_POINT_SIZE_GRANULARITY                          = $0B13;
		SMOOTH_LINE_WIDTH_RANGE                                = $0B22;
		SMOOTH_LINE_WIDTH_GRANULARITY                          = $0B23;
		ALIASED_LINE_WIDTH_RANGE                               = $846E;

	class var
		// BlendColor: procedure(red: clampf; green: clampf; blue: clampf; alpha: clampf); stdcall;
		// BlendEquation: procedure(mode: enum); stdcall;
		// DrawRangeElements: procedure(mode: enum; start: uint; end_: uint; count: sizei; type_: enum; indices: pointer); stdcall;
		TexImage3D: procedure(target: enum; level: int; internalformat: int; width: sizei; height: sizei; depth: sizei; border: int; format: enum; type_: enum; pixels: pointer); stdcall;
		TexSubImage3D: procedure(target: enum; level: int; xoffset, yoffset, zoffset: int; width, height, depth: sizei; format, type_: enum; pixels: pointer); stdcall;
		// CopyTexSubImage3D: procedure(target: enum; level: int; xoffset: int; yoffset: int; zoffset: int; x: int; y: int; width: sizei; height: sizei); stdcall;

	// ARB_imaging
	const
		CONSTANT_COLOR                                         = $8001;
		ONE_MINUS_CONSTANT_COLOR                               = $8002;
		CONSTANT_ALPHA                                         = $8003;
		ONE_MINUS_CONSTANT_ALPHA                               = $8004;
		BLEND_COLOR                                            = $8005;
		FUNC_ADD                                               = $8006;
		MIN                                                    = $8007;
		MAX                                                    = $8008;
		BLEND_EQUATION                                         = $8009;
		FUNC_SUBTRACT                                          = $800A;
		FUNC_REVERSE_SUBTRACT                                  = $800B;

	// VERSION_1_3
	const
		TEXTURE0                                               = $84C0;
		TEXTURE1                                               = $84C1;
		TEXTURE2                                               = $84C2;
		TEXTURE3                                               = $84C3;
		TEXTURE4                                               = $84C4;
		TEXTURE5                                               = $84C5;
		TEXTURE6                                               = $84C6;
		TEXTURE7                                               = $84C7;
		TEXTURE8                                               = $84C8;
		TEXTURE9                                               = $84C9;
		TEXTURE10                                              = $84CA;
		TEXTURE11                                              = $84CB;
		TEXTURE12                                              = $84CC;
		TEXTURE13                                              = $84CD;
		TEXTURE14                                              = $84CE;
		TEXTURE15                                              = $84CF;
		TEXTURE16                                              = $84D0;
		TEXTURE17                                              = $84D1;
		TEXTURE18                                              = $84D2;
		TEXTURE19                                              = $84D3;
		TEXTURE20                                              = $84D4;
		TEXTURE21                                              = $84D5;
		TEXTURE22                                              = $84D6;
		TEXTURE23                                              = $84D7;
		TEXTURE24                                              = $84D8;
		TEXTURE25                                              = $84D9;
		TEXTURE26                                              = $84DA;
		TEXTURE27                                              = $84DB;
		TEXTURE28                                              = $84DC;
		TEXTURE29                                              = $84DD;
		TEXTURE30                                              = $84DE;
		TEXTURE31                                              = $84DF;
		ACTIVE_TEXTURE                                         = $84E0;
		MULTISAMPLE                                            = $809D;
		SAMPLE_ALPHA_TO_COVERAGE                               = $809E;
		SAMPLE_ALPHA_TO_ONE                                    = $809F;
		SAMPLE_COVERAGE                                        = $80A0;
		SAMPLE_BUFFERS                                         = $80A8;
		SAMPLES                                                = $80A9;
		SAMPLE_COVERAGE_VALUE                                  = $80AA;
		SAMPLE_COVERAGE_INVERT                                 = $80AB;
		TEXTURE_CUBE_MAP                                       = $8513;
		TEXTURE_BINDING_CUBE_MAP                               = $8514;
		TEXTURE_CUBE_MAP_POSITIVE_X                            = $8515;
		TEXTURE_CUBE_MAP_NEGATIVE_X                            = $8516;
		TEXTURE_CUBE_MAP_POSITIVE_Y                            = $8517;
		TEXTURE_CUBE_MAP_NEGATIVE_Y                            = $8518;
		TEXTURE_CUBE_MAP_POSITIVE_Z                            = $8519;
		TEXTURE_CUBE_MAP_NEGATIVE_Z                            = $851A;
		PROXY_TEXTURE_CUBE_MAP                                 = $851B;
		MAX_CUBE_MAP_TEXTURE_SIZE                              = $851C;
		COMPRESSED_RGB                                         = $84ED;
		COMPRESSED_RGBA                                        = $84EE;
		TEXTURE_COMPRESSION_HINT                               = $84EF;
		TEXTURE_COMPRESSED_IMAGE_SIZE                          = $86A0;
		TEXTURE_COMPRESSED                                     = $86A1;
		NUM_COMPRESSED_TEXTURE_FORMATS                         = $86A2;
		COMPRESSED_TEXTURE_FORMATS                             = $86A3;
		CLAMP_TO_BORDER                                        = $812D;

	class var
		ActiveTexture: procedure(texture: enum); stdcall;
		// SampleCoverage: procedure(value: clampf; invert: boolean); stdcall;
		CompressedTexImage3D: procedure(target: enum; level: int; internalformat: enum; width: sizei; height: sizei; depth: sizei; border: int; imageSize: sizei; data: pointer); stdcall;
		CompressedTexImage2D: procedure(target: enum; level: int; internalformat: enum; width: sizei; height: sizei; border: int; imageSize: sizei; data: pointer); stdcall;
		// CompressedTexImage1D: procedure(target: enum; level: int; internalformat: enum; width: sizei; border: int; imageSize: sizei; data: pointer); stdcall;
		CompressedTexSubImage3D: procedure(target: enum; level: int; xoffset: int; yoffset: int; zoffset: int; width: sizei; height: sizei; depth: sizei; format: enum; imageSize: sizei; data: pointer); stdcall;
		CompressedTexSubImage2D: procedure(target: enum; level: int; xoffset: int; yoffset: int; width: sizei; height: sizei; format: enum; imageSize: sizei; data: pointer); stdcall;
		// CompressedTexSubImage1D: procedure(target: enum; level: int; xoffset: int; width: sizei; format: enum; imageSize: sizei; data: pointer); stdcall;
		GetCompressedTexImage: procedure(target: enum; level: int; img: pointer); stdcall;

	// VERSION_1_4
	const
		BLEND_DST_RGB                                          = $80C8;
		BLEND_SRC_RGB                                          = $80C9;
		BLEND_DST_ALPHA                                        = $80CA;
		BLEND_SRC_ALPHA                                        = $80CB;
		POINT_FADE_THRESHOLD_SIZE                              = $8128;
		DEPTH_COMPONENT16                                      = $81A5;
		DEPTH_COMPONENT24                                      = $81A6;
		DEPTH_COMPONENT32                                      = $81A7;
		MIRRORED_REPEAT                                        = $8370;
		MAX_TEXTURE_LOD_BIAS                                   = $84FD;
		TEXTURE_LOD_BIAS                                       = $8501;
		INCR_WRAP                                              = $8507;
		DECR_WRAP                                              = $8508;
		TEXTURE_DEPTH_SIZE                                     = $884A;
		TEXTURE_COMPARE_MODE                                   = $884C;
		TEXTURE_COMPARE_FUNC                                   = $884D;

	class var
		// BlendFuncSeparate: procedure(sfactorRGB: enum; dfactorRGB: enum; sfactorAlpha: enum; dfactorAlpha: enum); stdcall;
		// MultiDrawArrays: procedure(mode: enum; first: pInt; count: pSizei; primcount: sizei); stdcall;
		// MultiDrawElements: procedure(mode: enum; count: pSizei; type_: enum; indices: pointer; primcount: sizei); stdcall;
		// PointParameterf: procedure(pname: enum; param: float); stdcall;
		// PointParameterfv: procedure(pname: enum; params: pFloat); stdcall;
		// PointParameteri: procedure(pname: enum; param: int); stdcall;
		// PointParameteriv: procedure(pname: enum; params: pInt); stdcall;

	// VERSION_1_5
	const
		BUFFER_SIZE                                            = $8764;
		BUFFER_USAGE                                           = $8765;
		QUERY_COUNTER_BITS                                     = $8864;
		CURRENT_QUERY                                          = $8865;
		QUERY_RESULT                                           = $8866;
		QUERY_RESULT_AVAILABLE                                 = $8867;
		ARRAY_BUFFER                                           = $8892;
		ELEMENT_ARRAY_BUFFER                                   = $8893;
		ARRAY_BUFFER_BINDING                                   = $8894;
		ELEMENT_ARRAY_BUFFER_BINDING                           = $8895;
		VERTEX_ATTRIB_ARRAY_BUFFER_BINDING                     = $889F;
		READ_ONLY                                              = $88B8;
		WRITE_ONLY                                             = $88B9;
		READ_WRITE                                             = $88BA;
		BUFFER_ACCESS                                          = $88BB;
		BUFFER_MAPPED                                          = $88BC;
		BUFFER_MAP_POINTER                                     = $88BD;
		STREAM_DRAW                                            = $88E0;
		STREAM_READ                                            = $88E1;
		STREAM_COPY                                            = $88E2;
		STATIC_DRAW                                            = $88E4;
		STATIC_READ                                            = $88E5;
		STATIC_COPY                                            = $88E6;
		DYNAMIC_DRAW                                           = $88E8;
		DYNAMIC_READ                                           = $88E9;
		DYNAMIC_COPY                                           = $88EA;
		SAMPLES_PASSED                                         = $8914;

	class var
		// GenQueries: procedure(n: sizei; ids: pUint); stdcall;
		// DeleteQueries: procedure(n: sizei; ids: pUint); stdcall;
		// IsQuery: function(id: uint): boolean; stdcall;
		// BeginQuery: procedure(target: enum; id: uint); stdcall;
		// EndQuery: procedure(target: enum); stdcall;
		// GetQueryiv: procedure(target: enum; pname: enum; params: pInt); stdcall;
		// GetQueryObjectiv: procedure(id: uint; pname: enum; params: pInt); stdcall;
		// GetQueryObjectuiv: procedure(id: uint; pname: enum; params: pUint); stdcall;
		BindBuffer: procedure(target: enum; buffer: uint); stdcall;
		DeleteBuffers: procedure(n: sizei; buffers: pUint); stdcall;
		GenBuffers: procedure(n: sizei; buffers: pUint); stdcall;
		// IsBuffer: function(buffer: uint): boolean; stdcall;
		BufferData: procedure(target: enum; size: sizeiptr; data: pointer; usage: enum); stdcall;
		BufferSubData: procedure(target: enum; offset: intptr; size: sizeiptr; data: pointer); stdcall;
		// GetBufferSubData: procedure(target: enum; offset: intptr; size: sizeiptr; data: pointer); stdcall;
		// MapBuffer: function(target: enum; access: enum): pointer; stdcall;
		// UnmapBuffer: function(target: enum): boolean; stdcall;
		// GetBufferParameteriv: procedure(target: enum; pname: enum; params: pInt); stdcall;
		// GetBufferPointerv: procedure(target: enum; pname: enum; params: pointer); stdcall;

	// VERSION_2_0
	const
		BLEND_EQUATION_RGB                                     = $8009;
		VERTEX_ATTRIB_ARRAY_ENABLED                            = $8622;
		VERTEX_ATTRIB_ARRAY_SIZE                               = $8623;
		VERTEX_ATTRIB_ARRAY_STRIDE                             = $8624;
		VERTEX_ATTRIB_ARRAY_TYPE                               = $8625;
		CURRENT_VERTEX_ATTRIB                                  = $8626;
		VERTEX_PROGRAM_POINT_SIZE                              = $8642;
		VERTEX_ATTRIB_ARRAY_POINTER                            = $8645;
		STENCIL_BACK_FUNC                                      = $8800;
		STENCIL_BACK_FAIL                                      = $8801;
		STENCIL_BACK_PASS_DEPTH_FAIL                           = $8802;
		STENCIL_BACK_PASS_DEPTH_PASS                           = $8803;
		MAX_DRAW_BUFFERS                                       = $8824;
		DRAW_BUFFER0                                           = $8825;
		DRAW_BUFFER1                                           = $8826;
		DRAW_BUFFER2                                           = $8827;
		DRAW_BUFFER3                                           = $8828;
		DRAW_BUFFER4                                           = $8829;
		DRAW_BUFFER5                                           = $882A;
		DRAW_BUFFER6                                           = $882B;
		DRAW_BUFFER7                                           = $882C;
		DRAW_BUFFER8                                           = $882D;
		DRAW_BUFFER9                                           = $882E;
		DRAW_BUFFER10                                          = $882F;
		DRAW_BUFFER11                                          = $8830;
		DRAW_BUFFER12                                          = $8831;
		DRAW_BUFFER13                                          = $8832;
		DRAW_BUFFER14                                          = $8833;
		DRAW_BUFFER15                                          = $8834;
		BLEND_EQUATION_ALPHA                                   = $883D;
		MAX_VERTEX_ATTRIBS                                     = $8869;
		VERTEX_ATTRIB_ARRAY_NORMALIZED                         = $886A;
		MAX_TEXTURE_IMAGE_UNITS                                = $8872;
		FRAGMENT_SHADER                                        = $8B30;
		VERTEX_SHADER                                          = $8B31;
		MAX_FRAGMENT_UNIFORM_COMPONENTS                        = $8B49;
		MAX_VERTEX_UNIFORM_COMPONENTS                          = $8B4A;
		MAX_VERTEX_TEXTURE_IMAGE_UNITS                         = $8B4C;
		MAX_COMBINED_TEXTURE_IMAGE_UNITS                       = $8B4D;
		SHADER_TYPE                                            = $8B4F;
		FLOAT_VEC2                                             = $8B50;
		FLOAT_VEC3                                             = $8B51;
		FLOAT_VEC4                                             = $8B52;
		INT_VEC2                                               = $8B53;
		INT_VEC3                                               = $8B54;
		INT_VEC4                                               = $8B55;
		BOOL                                                   = $8B56;
		BOOL_VEC2                                              = $8B57;
		BOOL_VEC3                                              = $8B58;
		BOOL_VEC4                                              = $8B59;
		FLOAT_MAT2                                             = $8B5A;
		FLOAT_MAT3                                             = $8B5B;
		FLOAT_MAT4                                             = $8B5C;
		SAMPLER_1D                                             = $8B5D;
		SAMPLER_2D                                             = $8B5E;
		SAMPLER_3D                                             = $8B5F;
		SAMPLER_CUBE                                           = $8B60;
		SAMPLER_1D_SHADOW                                      = $8B61;
		SAMPLER_2D_SHADOW                                      = $8B62;
		DELETE_STATUS                                          = $8B80;
		COMPILE_STATUS                                         = $8B81;
		LINK_STATUS                                            = $8B82;
		VALIDATE_STATUS                                        = $8B83;
		INFO_LOG_LENGTH                                        = $8B84;
		ATTACHED_SHADERS                                       = $8B85;
		ACTIVE_UNIFORMS                                        = $8B86;
		ACTIVE_UNIFORM_MAX_LENGTH                              = $8B87;
		SHADER_SOURCE_LENGTH                                   = $8B88;
		ACTIVE_ATTRIBUTES                                      = $8B89;
		ACTIVE_ATTRIBUTE_MAX_LENGTH                            = $8B8A;
		FRAGMENT_SHADER_DERIVATIVE_HINT                        = $8B8B;
		SHADING_LANGUAGE_VERSION                               = $8B8C;
		CURRENT_PROGRAM                                        = $8B8D;
		POINT_SPRITE_COORD_ORIGIN                              = $8CA0;
		LOWER_LEFT                                             = $8CA1;
		UPPER_LEFT                                             = $8CA2;
		STENCIL_BACK_REF                                       = $8CA3;
		STENCIL_BACK_VALUE_MASK                                = $8CA4;
		STENCIL_BACK_WRITEMASK                                 = $8CA5;

	class var
		// BlendEquationSeparate: procedure(modeRGB: enum; modeAlpha: enum); stdcall;
		DrawBuffers: procedure(n: sizei; bufs: pEnum); stdcall;
		// StencilOpSeparate: procedure(face: enum; sfail: enum; dpfail: enum; dppass: enum); stdcall;
		// StencilFuncSeparate: procedure(frontfunc: enum; backfunc: enum; ref: int; mask: uint); stdcall;
		// StencilMaskSeparate: procedure(face: enum; mask: uint); stdcall;
		AttachShader: procedure(program_: uint; shader: uint); stdcall;
		// BindAttribLocation: procedure(program_: uint; index: uint; name: pChar); stdcall;
		CompileShader: procedure(shader: uint); stdcall;
		CreateProgram: function: uint; stdcall;
		CreateShader: function(type_: enum): uint; stdcall;
		DeleteProgram: procedure(program_: uint); stdcall;
		DeleteShader: procedure(shader: uint); stdcall;
		DetachShader: procedure(program_: uint; shader: uint); stdcall;
		DisableVertexAttribArray: procedure(index: uint); stdcall;
		EnableVertexAttribArray: procedure(index: uint); stdcall;
		GetActiveAttrib: procedure(program_: uint; index: uint; bufSize: sizei; length: pSizei; size: pInt; type_: pEnum; name: pChar); stdcall;
		GetActiveUniform: procedure(program_: uint; index: uint; bufSize: sizei; length: pSizei; size: pInt; type_: pEnum; name: pChar); stdcall;
		// GetAttachedShaders: procedure(program_: uint; maxCount: sizei; count: pSizei; obj: pUint); stdcall;
		GetAttribLocation: function(program_: uint; name: pChar): int; stdcall;
		GetProgramiv: procedure(program_: uint; pname: enum; params: pInt); stdcall;
		GetProgramInfoLog: procedure(program_: uint; bufSize: sizei; length: pSizei; infoLog: pChar); stdcall;
		GetShaderiv: procedure(shader: uint; pname: enum; params: pInt); stdcall;
		GetShaderInfoLog: procedure(shader: uint; bufSize: sizei; length: pSizei; infoLog: pChar); stdcall;
		// GetShaderSource: procedure(shader: uint; bufSize: sizei; length: pSizei; source: pChar); stdcall;
		GetUniformLocation: function(program_: uint; name: pChar): int; stdcall;
		// GetUniformfv: procedure( program_: uint; location: int; params: pFloat); stdcall;
		// GetUniformiv: procedure(program_: uint; location: int; params: pInt); stdcall;
		// GetVertexAttribdv: procedure(index: uint; pname: enum; params: pDouble); stdcall;
		// GetVertexAttribfv: procedure(index: uint; pname: enum; params: pFloat); stdcall;
		// GetVertexAttribiv: procedure(index: uint; pname: enum; params: pInt); stdcall;
		// GetVertexAttribPointerv: procedure(index: uint; pname: enum; pointer: pointer); stdcall;
		// IsProgram: function(program_: uint): boolean; stdcall;
		// IsShader: function(shader: uint): boolean; stdcall;
		LinkProgram: procedure(program_: uint); stdcall;
		ShaderSource: procedure(shader: uint; count: sizei; pieces: ppchar; length: pInt); stdcall;
		UseProgram: procedure(program_: uint); stdcall;
		// Uniform1f: procedure(location: int; v0: float); stdcall;
		// Uniform2f: procedure(location: int; v0: float; v1: float); stdcall;
		// Uniform3f: procedure(location: int; v0: float; v1: float; v2: float); stdcall;
		// Uniform4f: procedure(location: int; v0: float; v1: float; v2: float; v3: float); stdcall;
		Uniform1i: procedure(location: int; v0: int); stdcall;
		// Uniform2i: procedure(location: int; v0: int; v1: int); stdcall;
		// Uniform3i: procedure(location: int; v0: int; v1: int; v2: int); stdcall;
		// Uniform4i: procedure(location: int; v0: int; v1: int; v2: int; v3: int); stdcall;
		Uniform1fv: procedure(location: int; count: sizei; value: pFloat); stdcall;
		Uniform2fv: procedure(location: int; count: sizei; value: pFloat); stdcall;
		Uniform3fv: procedure(location: int; count: sizei; value: pFloat); stdcall;
		Uniform4fv: procedure(location: int; count: sizei; value: pFloat); stdcall;
		Uniform1iv: procedure(location: int; count: sizei; value: pInt); stdcall;
		// Uniform2iv: procedure(location: int; count: sizei; value: pInt); stdcall;
		// Uniform3iv: procedure(location: int; count: sizei; value: pInt); stdcall;
		// Uniform4iv: procedure(location: int; count: sizei; value: pInt); stdcall;
		// UniformMatrix2fv: procedure(location: int; count: sizei; transpose: boolean; value: pFloat); stdcall;
		// UniformMatrix3fv: procedure(location: int; count: sizei; transpose: boolean; value: pFloat); stdcall;
		UniformMatrix4fv: procedure(location: int; count: sizei; transpose: boolean; value: pFloat); stdcall;
		ValidateProgram: procedure(program_: uint); stdcall;
		// VertexAttrib1d: procedure(index: uint; x: double); stdcall;
		// VertexAttrib1dv: procedure(index: uint; v: pDouble); stdcall;
		// VertexAttrib1f: procedure(index: uint; x: float); stdcall;
		// VertexAttrib1fv: procedure(index: uint; v: pFloat); stdcall;
		// VertexAttrib1s: procedure(index: uint; x: short); stdcall;
		// VertexAttrib1sv: procedure(index: uint; v: pShort); stdcall;
		// VertexAttrib2d: procedure(index: uint; x: double; y: double); stdcall;
		// VertexAttrib2dv: procedure(index: uint; v: pDouble); stdcall;
		// VertexAttrib2f: procedure(index: uint; x: float; y: float); stdcall;
		// VertexAttrib2fv: procedure(index: uint; v: pFloat); stdcall;
		// VertexAttrib2s: procedure(index: uint; x: short; y: short); stdcall;
		// VertexAttrib2sv: procedure(index: uint; v: pShort); stdcall;
		// VertexAttrib3d: procedure(index: uint; x: double; y: double; z: double); stdcall;
		// VertexAttrib3dv: procedure(index: uint; v: pDouble); stdcall;
		// VertexAttrib3f: procedure(index: uint; x: float; y: float; z: float); stdcall;
		// VertexAttrib3fv: procedure(index: uint; v: pFloat); stdcall;
		// VertexAttrib3s: procedure(index: uint; x: short; y: short; z: short); stdcall;
		// VertexAttrib3sv: procedure(index: uint; v: pShort); stdcall;
		// VertexAttrib4Nbv: procedure(index: uint; v: pByte); stdcall;
		// VertexAttrib4Niv: procedure(index: uint; v: pInt); stdcall;
		// VertexAttrib4Nsv: procedure(index: uint; v: pShort); stdcall;
		// VertexAttrib4Nub: procedure(index: uint; x: ubyte; y: ubyte; z: ubyte; w: ubyte); stdcall;
		// VertexAttrib4Nubv: procedure(index: uint; v: pUbyte); stdcall;
		// VertexAttrib4Nuiv: procedure(index: uint; v: pUint); stdcall;
		// VertexAttrib4Nusv: procedure(index: uint; v: pUshort); stdcall;
		// VertexAttrib4bv: procedure(index: uint; v: pByte); stdcall;
		// VertexAttrib4d: procedure(index: uint; x: double; y: double; z: double; w: double); stdcall;
		// VertexAttrib4dv: procedure(index: uint; v: pDouble); stdcall;
		// VertexAttrib4f: procedure(index: uint; x: float; y: float; z: float; w: float); stdcall;
		// VertexAttrib4fv: procedure(index: uint; v: pFloat); stdcall;
		// VertexAttrib4iv: procedure(index: uint; v: pInt); stdcall;
		// VertexAttrib4s: procedure(index: uint; x: short; y: short; z: short; w: short); stdcall;
		// VertexAttrib4sv: procedure(index: uint; v: pShort); stdcall;
		// VertexAttrib4ubv: procedure(index: uint; v: pUbyte); stdcall;
		// VertexAttrib4uiv: procedure(index: uint; v: pUint); stdcall;
		// VertexAttrib4usv: procedure(index: uint; v: pUshort); stdcall;
		VertexAttribPointer: procedure(index: uint; size: int; type_: enum; normalized: boolean; stride: sizei; pointer: pointer); stdcall;

	// VERSION_2_1
	const
		PIXEL_PACK_BUFFER                                      = $88EB;
		PIXEL_UNPACK_BUFFER                                    = $88EC;
		PIXEL_PACK_BUFFER_BINDING                              = $88ED;
		PIXEL_UNPACK_BUFFER_BINDING                            = $88EF;
		FLOAT_MAT2x3                                           = $8B65;
		FLOAT_MAT2x4                                           = $8B66;
		FLOAT_MAT3x2                                           = $8B67;
		FLOAT_MAT3x4                                           = $8B68;
		FLOAT_MAT4x2                                           = $8B69;
		FLOAT_MAT4x3                                           = $8B6A;
		SRGB                                                   = $8C40;
		SRGB8                                                  = $8C41;
		SRGB_ALPHA                                             = $8C42;
		SRGB8_ALPHA8                                           = $8C43;
		COMPRESSED_SRGB                                        = $8C48;
		COMPRESSED_SRGB_ALPHA                                  = $8C49;

	class var
		// UniformMatrix2x3fv: procedure(location: int; count: sizei; transpose: boolean; value: pFloat); stdcall;
		// UniformMatrix3x2fv: procedure(location: int; count: sizei; transpose: boolean; value: pFloat); stdcall;
		// UniformMatrix2x4fv: procedure(location: int; count: sizei; transpose: boolean; value: pFloat); stdcall;
		// UniformMatrix4x2fv: procedure(location: int; count: sizei; transpose: boolean; value: pFloat); stdcall;
		// UniformMatrix3x4fv: procedure(location: int; count: sizei; transpose: boolean; value: pFloat); stdcall;
		// UniformMatrix4x3fv: procedure(location: int; count: sizei; transpose: boolean; value: pFloat); stdcall;

	// VERSION_3_0
	const
		COMPARE_REF_TO_TEXTURE                                 = $884E;
		CLIP_DISTANCE0                                         = $3000;
		CLIP_DISTANCE1                                         = $3001;
		CLIP_DISTANCE2                                         = $3002;
		CLIP_DISTANCE3                                         = $3003;
		CLIP_DISTANCE4                                         = $3004;
		CLIP_DISTANCE5                                         = $3005;
		CLIP_DISTANCE6                                         = $3006;
		CLIP_DISTANCE7                                         = $3007;
		MAX_CLIP_DISTANCES                                     = $0D32;
		MAJOR_VERSION                                          = $821B;
		MINOR_VERSION                                          = $821C;
		NUM_EXTENSIONS                                         = $821D;
		CONTEXT_FLAGS                                          = $821E;
		DEPTH_BUFFER                                           = $8223;
		STENCIL_BUFFER                                         = $8224;
		COMPRESSED_RED                                         = $8225;
		COMPRESSED_RG                                          = $8226;
		CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT                    = $0001;
		RGBA32F                                                = $8814;
		RGB32F                                                 = $8815;
		RGBA16F                                                = $881A;
		RGB16F                                                 = $881B;
		VERTEX_ATTRIB_ARRAY_INTEGER                            = $88FD;
		MAX_ARRAY_TEXTURE_LAYERS                               = $88FF;
		MIN_PROGRAM_TEXEL_OFFSET                               = $8904;
		MAX_PROGRAM_TEXEL_OFFSET                               = $8905;
		CLAMP_READ_COLOR                                       = $891C;
		FIXED_ONLY                                             = $891D;
		MAX_VARYING_COMPONENTS                                 = $8B4B;
		TEXTURE_1D_ARRAY                                       = $8C18;
		PROXY_TEXTURE_1D_ARRAY                                 = $8C19;
		TEXTURE_2D_ARRAY                                       = $8C1A;
		PROXY_TEXTURE_2D_ARRAY                                 = $8C1B;
		TEXTURE_BINDING_1D_ARRAY                               = $8C1C;
		TEXTURE_BINDING_2D_ARRAY                               = $8C1D;
		R11F_G11F_B10F                                         = $8C3A;
		UNSIGNED_INT_10F_11F_11F_REV                           = $8C3B;
		RGB9_E5                                                = $8C3D;
		UNSIGNED_INT_5_9_9_9_REV                               = $8C3E;
		TEXTURE_SHARED_SIZE                                    = $8C3F;
		TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH                  = $8C76;
		TRANSFORM_FEEDBACK_BUFFER_MODE                         = $8C7F;
		MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS             = $8C80;
		TRANSFORM_FEEDBACK_VARYINGS                            = $8C83;
		TRANSFORM_FEEDBACK_BUFFER_START                        = $8C84;
		TRANSFORM_FEEDBACK_BUFFER_SIZE                         = $8C85;
		PRIMITIVES_GENERATED                                   = $8C87;
		TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN                  = $8C88;
		RASTERIZER_DISCARD                                     = $8C89;
		MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS          = $8C8A;
		MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS                = $8C8B;
		INTERLEAVED_ATTRIBS                                    = $8C8C;
		SEPARATE_ATTRIBS                                       = $8C8D;
		TRANSFORM_FEEDBACK_BUFFER                              = $8C8E;
		TRANSFORM_FEEDBACK_BUFFER_BINDING                      = $8C8F;
		RGBA32UI                                               = $8D70;
		RGB32UI                                                = $8D71;
		RGBA16UI                                               = $8D76;
		RGB16UI                                                = $8D77;
		RGBA8UI                                                = $8D7C;
		RGB8UI                                                 = $8D7D;
		RGBA32I                                                = $8D82;
		RGB32I                                                 = $8D83;
		RGBA16I                                                = $8D88;
		RGB16I                                                 = $8D89;
		RGBA8I                                                 = $8D8E;
		RGB8I                                                  = $8D8F;
		RED_INTEGER                                            = $8D94;
		GREEN_INTEGER                                          = $8D95;
		BLUE_INTEGER                                           = $8D96;
		RGB_INTEGER                                            = $8D98;
		RGBA_INTEGER                                           = $8D99;
		BGR_INTEGER                                            = $8D9A;
		BGRA_INTEGER                                           = $8D9B;
		SAMPLER_1D_ARRAY                                       = $8DC0;
		SAMPLER_2D_ARRAY                                       = $8DC1;
		SAMPLER_1D_ARRAY_SHADOW                                = $8DC3;
		SAMPLER_2D_ARRAY_SHADOW                                = $8DC4;
		SAMPLER_CUBE_SHADOW                                    = $8DC5;
		UNSIGNED_INT_VEC2                                      = $8DC6;
		UNSIGNED_INT_VEC3                                      = $8DC7;
		UNSIGNED_INT_VEC4                                      = $8DC8;
		INT_SAMPLER_1D                                         = $8DC9;
		INT_SAMPLER_2D                                         = $8DCA;
		INT_SAMPLER_3D                                         = $8DCB;
		INT_SAMPLER_CUBE                                       = $8DCC;
		INT_SAMPLER_1D_ARRAY                                   = $8DCE;
		INT_SAMPLER_2D_ARRAY                                   = $8DCF;
		UNSIGNED_INT_SAMPLER_1D                                = $8DD1;
		UNSIGNED_INT_SAMPLER_2D                                = $8DD2;
		UNSIGNED_INT_SAMPLER_3D                                = $8DD3;
		UNSIGNED_INT_SAMPLER_CUBE                              = $8DD4;
		UNSIGNED_INT_SAMPLER_1D_ARRAY                          = $8DD6;
		UNSIGNED_INT_SAMPLER_2D_ARRAY                          = $8DD7;
		QUERY_WAIT                                             = $8E13;
		QUERY_NO_WAIT                                          = $8E14;
		QUERY_BY_REGION_WAIT                                   = $8E15;
		QUERY_BY_REGION_NO_WAIT                                = $8E16;
		BUFFER_ACCESS_FLAGS                                    = $911F;
		BUFFER_MAP_LENGTH                                      = $9120;
		BUFFER_MAP_OFFSET                                      = $9121;

	class var
		// ColorMaski: procedure(index: uint; r: boolean; g: boolean; b: boolean; a: boolean); stdcall;
		// GetBooleani_v: procedure(target: enum; index: uint; data: pBoolean); stdcall;
		// GetIntegeri_v: procedure(target: enum; index: uint; data: pInt); stdcall;
		// Enablei: procedure(target: enum; index: uint); stdcall;
		// Disablei: procedure(target: enum; index: uint); stdcall;
		// IsEnabledi: function(target: enum; index: uint): boolean; stdcall;
		// BeginTransformFeedback: procedure(primitiveMode: enum); stdcall;
		// EndTransformFeedback: procedure; stdcall;
		// BindBufferRange: procedure(target: enum; index: uint; buffer: uint; offset: intptr; size: sizeiptr); stdcall;
		BindBufferBase: procedure(target: enum; index: uint; buffer: uint); stdcall;
		// TransformFeedbackVaryings: procedure(program_: uint; count: sizei; varyings: pChar; bufferMode: enum); stdcall;
		// GetTransformFeedbackVarying: procedure(program_: uint; index: uint; bufSize: sizei; length: pSizei; size: pSizei; type_: pEnum; name: pChar); stdcall;
		// ClampColor: procedure(target: enum; clamp: enum); stdcall;
		// BeginConditionalRender: procedure(id: uint; mode: enum); stdcall;
		// EndConditionalRender: procedure; stdcall;
		// VertexAttribIPointer: procedure(index: uint; size: int; type_: enum; stride: sizei; pointer: pointer); stdcall;
		// GetVertexAttribIiv: procedure(index: uint; pname: enum; params: pInt); stdcall;
		// GetVertexAttribIuiv: procedure(index: uint; pname: enum; params: pUint); stdcall;
		// VertexAttribI1i: procedure(index: uint; x: int); stdcall;
		// VertexAttrinI2i: procedure(index: uint; x: int; y: int); stdcall;
		// VertexAttribI3i: procedure(index: uint; x: int; y: int; z: int); stdcall;
		// VertexAttribI4i: procedure(index: uint; x: int; y: int; z: int; w: int); stdcall;
		// VertexAttribI1ui: procedure(index: uint; x: uint); stdcall;
		// VertexAttribI2ui: procedure(index: uint; x: uint; y: uint); stdcall;
		// VertexAttribI3ui: procedure(index: uint; x: uint; y: uint; z: uint); stdcall;
		// VertexAttribI4ui: procedure(index: uint; x: uint; y: uint; z: uint; w: uint); stdcall;
		// VertexAttribI1iv: procedure(index: uint; v: pInt); stdcall;
		// VertexAttribI2iv: procedure(index: uint; v: pInt); stdcall;
		// VertexAttribI3iv: procedure(index: uint; v: pInt); stdcall;
		// VertexAttribI4iv: procedure(index: uint; v: pInt); stdcall;
		// VertexAttribI1uiv: procedure(index: uint; v: pUint); stdcall;
		// VertexAttribI2uiv: procedure(index: uint; v: pUint); stdcall;
		// VertexAttribI3uiv: procedure(index: uint; v: pUint); stdcall;
		// VertexAttribI4uiv: procedure(index: uint; v: pUint); stdcall;
		// VertexAttribI4bv: procedure(index: uint; v: pByte); stdcall;
		// VertexAttribI4sv: procedure(index: uint; v: pShort); stdcall;
		// VertexAttribI4ubv: procedure(index: uint; v: pUbyte); stdcall;
		// VertexAttribI4usv: procedure(index: uint; v: pUshort); stdcall;
		// GetUniformuiv: procedure(program_: uint; location: int; params: pUint); stdcall;
		// BindFragDataLocation: procedure(program_: uint; color: uint; name: pChar); stdcall;
		// GetFragDataLocation: function(program_: uint; name: pChar): int; stdcall;
		// Uniform1ui: procedure(location: int; v0: uint); stdcall;
		// Uniform2ui: procedure(location: int; v0: uint; v1: uint); stdcall;
		// Uniform3ui: procedure(location: int; v0: uint; v1: uint; v2: uint); stdcall;
		// Uniform4ui: procedure(location: int; v0: uint; v1: uint; v2: uint; v3: uint); stdcall;
		// Uniform1uiv: procedure(location: int; count: sizei; value: pUint); stdcall;
		// Uniform2uiv: procedure(location: int; count: sizei; value: pUint); stdcall;
		// Uniform3uiv: procedure(location: int; count: sizei; value: pUint); stdcall;
		// Uniform4uiv: procedure(location: int; count: sizei; value: pUint); stdcall;
		// TexParameterIiv: procedure(target: enum; pname: enum; params: pInt); stdcall;
		// TexParameterIuiv: procedure(target: enum; pname: enum; params: pUint); stdcall;
		// GetTexParameterIiv: procedure(target: enum; pname: enum; params: pInt); stdcall;
		// GetTexParameterIuiv: procedure(target: enum; pname: enum; params: pUint); stdcall;
		// ClearBufferiv: procedure(buffer: enum; drawbuffer: int; value: pInt); stdcall;
		// ClearBufferuiv: procedure(buffer: enum; drawbuffer: int; value: pUint); stdcall;
		// ClearBufferfv: procedure(buffer: enum; drawbuffer: int; value: pFloat); stdcall;
		// ClearBufferi: procedure(buffer: enum; drawbuffer: int; depth: float; stencil: int); stdcall;
		GetStringi: function(name: enum; index: uint): pChar; stdcall;

	// ARB_depth_buffer_float
	const
		DEPTH_COMPONENT32F                                     = $8CAC;
		DEPTH32F_STENCIL8                                      = $8CAD;
		FLOAT_32_UNSIGNED_INT_24_8_REV                         = $8DAD;

	// ARB_framebuffer_object
	const
		INVALID_FRAMEBUFFER_OPERATION                          = $0506;
		FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING                  = $8210;
		FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE                  = $8211;
		FRAMEBUFFER_ATTACHMENT_RED_SIZE                        = $8212;
		FRAMEBUFFER_ATTACHMENT_GREEN_SIZE                      = $8213;
		FRAMEBUFFER_ATTACHMENT_BLUE_SIZE                       = $8214;
		FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE                      = $8215;
		FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE                      = $8216;
		FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE                    = $8217;
		FRAMEBUFFER_DEFAULT                                    = $8218;
		FRAMEBUFFER_UNDEFINED                                  = $8219;
		DEPTH_STENCIL_ATTACHMENT                               = $821A;
		MAX_RENDERBUFFER_SIZE                                  = $84E8;
		DEPTH_STENCIL                                          = $84F9;
		UNSIGNED_INT_24_8                                      = $84FA;
		DEPTH24_STENCIL8                                       = $88F0;
		TEXTURE_STENCIL_SIZE                                   = $88F1;
		TEXTURE_RED_TYPE                                       = $8C10;
		TEXTURE_GREEN_TYPE                                     = $8C11;
		TEXTURE_BLUE_TYPE                                      = $8C12;
		TEXTURE_ALPHA_TYPE                                     = $8C13;
		TEXTURE_DEPTH_TYPE                                     = $8C16;
		UNSIGNED_NORMALIZED                                    = $8C17;
		FRAMEBUFFER_BINDING                                    = $8CA6;
		DRAW_FRAMEBUFFER_BINDING                               = FRAMEBUFFER_BINDING;
		RENDERBUFFER_BINDING                                   = $8CA7;
		READ_FRAMEBUFFER                                       = $8CA8;
		DRAW_FRAMEBUFFER                                       = $8CA9;
		READ_FRAMEBUFFER_BINDING                               = $8CAA;
		RENDERBUFFER_SAMPLES                                   = $8CAB;
		FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE                     = $8CD0;
		FRAMEBUFFER_ATTACHMENT_OBJECT_NAME                     = $8CD1;
		FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL                   = $8CD2;
		FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE           = $8CD3;
		FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER                   = $8CD4;
		FRAMEBUFFER_COMPLETE                                   = $8CD5;
		FRAMEBUFFER_INCOMPLETE_ATTACHMENT                      = $8CD6;
		FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT              = $8CD7;
		FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER                     = $8CDB;
		FRAMEBUFFER_INCOMPLETE_READ_BUFFER                     = $8CDC;
		FRAMEBUFFER_UNSUPPORTED                                = $8CDD;
		MAX_COLOR_ATTACHMENTS                                  = $8CDF;
		COLOR_ATTACHMENT0                                      = $8CE0;
		COLOR_ATTACHMENT1                                      = $8CE1;
		COLOR_ATTACHMENT2                                      = $8CE2;
		COLOR_ATTACHMENT3                                      = $8CE3;
		COLOR_ATTACHMENT4                                      = $8CE4;
		COLOR_ATTACHMENT5                                      = $8CE5;
		COLOR_ATTACHMENT6                                      = $8CE6;
		COLOR_ATTACHMENT7                                      = $8CE7;
		COLOR_ATTACHMENT8                                      = $8CE8;
		COLOR_ATTACHMENT9                                      = $8CE9;
		COLOR_ATTACHMENT10                                     = $8CEA;
		COLOR_ATTACHMENT11                                     = $8CEB;
		COLOR_ATTACHMENT12                                     = $8CEC;
		COLOR_ATTACHMENT13                                     = $8CED;
		COLOR_ATTACHMENT14                                     = $8CEE;
		COLOR_ATTACHMENT15                                     = $8CEF;
		DEPTH_ATTACHMENT                                       = $8D00;
		STENCIL_ATTACHMENT                                     = $8D20;
		FRAMEBUFFER                                            = $8D40;
		RENDERBUFFER                                           = $8D41;
		RENDERBUFFER_WIDTH                                     = $8D42;
		RENDERBUFFER_HEIGHT                                    = $8D43;
		RENDERBUFFER_INTERNAL_FORMAT                           = $8D44;
		STENCIL_INDEX1                                         = $8D46;
		STENCIL_INDEX4                                         = $8D47;
		STENCIL_INDEX8                                         = $8D48;
		STENCIL_INDEX16                                        = $8D49;
		RENDERBUFFER_RED_SIZE                                  = $8D50;
		RENDERBUFFER_GREEN_SIZE                                = $8D51;
		RENDERBUFFER_BLUE_SIZE                                 = $8D52;
		RENDERBUFFER_ALPHA_SIZE                                = $8D53;
		RENDERBUFFER_DEPTH_SIZE                                = $8D54;
		RENDERBUFFER_STENCIL_SIZE                              = $8D55;
		FRAMEBUFFER_INCOMPLETE_MULTISAMPLE                     = $8D56;
		MAX_SAMPLES                                            = $8D57;

	class var
		// IsRenderbuffer: function(renderbuffer: uint): boolean; stdcall;
		// BindRenderbuffer: procedure(target: enum; renderbuffer: uint); stdcall;
		// DeleteRenderbuffers: procedure(n: sizei; renderbuffers: pUint); stdcall;
		// GenRenderbuffers: procedure(n: sizei; renderbuffers: pUint); stdcall;
		// RenderbufferStorage: procedure(target: enum; internalformat: enum; width: sizei; height: sizei); stdcall;
		// GetRenderbufferParameteriv: procedure(target: enum; pname: enum; params: pInt); stdcall;
		// IsFramebuffer: function(framebuffer: uint): boolean; stdcall;
		BindFramebuffer: procedure(target: enum; framebuffer: uint); stdcall;
		DeleteFramebuffers: procedure(n: sizei; framebuffers: pUint); stdcall;
		GenFramebuffers: procedure(n: sizei; framebuffers: pUint); stdcall;
		CheckFramebufferStatus: function(target: enum): enum; stdcall;
		// FramebufferTexture1D: procedure(target: enum; attachment: enum; textarget: enum; texture: uint; level: int); stdcall;
		FramebufferTexture2D: procedure(target: enum; attachment: enum; textarget: enum; texture: uint; level: int); stdcall;
		// FramebufferTexture3D: procedure(target: enum; attachment: enum; textarget: enum; texture: uint; level: int; zoffset: int); stdcall;
		// FramebufferRenderbuffer: procedure(target: enum; attachment: enum; renderbuffertarget: enum; renderbuffer: uint); stdcall;
		// GetFramebufferAttachmentParameteriv: procedure(target: enum; attachment: enum; pname: enum; params: pInt); stdcall;
		// GenerateMipmap: procedure(target: enum); stdcall;
		// BlitFramebuffer: procedure(srcX0: int; srcY0: int; srcX1: int; srcY1: int; dstX0: int; dstY0: int; dstX1: int; dstY1: int; mask: bitfield; filter: enum); stdcall;
		// RenderbufferStorageMultisample: procedure(target: enum; samples: sizei; internalformat: enum; width: sizei; height: sizei); stdcall;
		// FramebufferTextureLayer: procedure(target: enum; attachment: enum; texture: uint; level: int; layer: int); stdcall;

	// ARB_framebuffer_sRGB
	const
		FRAMEBUFFER_SRGB                                       = $8DB9;

	// ARB_half_float_vertex
	const
		HALF_FLOAT                                             = $140B;

	// ARB_map_buffer_range
	const
		MAP_READ_BIT                                           = $0001;
		MAP_WRITE_BIT                                          = $0002;
		MAP_INVALIDATE_RANGE_BIT                               = $0004;
		MAP_INVALIDATE_BUFFER_BIT                              = $0008;
		MAP_FLUSH_EXPLICIT_BIT                                 = $0010;
		MAP_UNSYNCHRONIZED_BIT                                 = $0020;

	class var
		// MapBufferRange: function(target: enum; offset: intptr; length: sizeiptr; access: bitfield): pointer; stdcall;
		// FlushMappedBufferRange: procedure(target: enum; offset: intptr; length: sizeiptr); stdcall;

	// ARB_texture_compression_rgtc
	const
		COMPRESSED_RED_RGTC1                                   = $8DBB;
		COMPRESSED_SIGNED_RED_RGTC1                            = $8DBC;
		COMPRESSED_RG_RGTC2                                    = $8DBD;
		COMPRESSED_SIGNED_RG_RGTC2                             = $8DBE;

	// ARB_texture_rg
	const
		RG                                                     = $8227;
		RG_INTEGER                                             = $8228;
		R8                                                     = $8229;
		R16                                                    = $822A;
		RG8                                                    = $822B;
		RG16                                                   = $822C;
		R16F                                                   = $822D;
		R32F                                                   = $822E;
		RG16F                                                  = $822F;
		RG32F                                                  = $8230;
		R8I                                                    = $8231;
		R8UI                                                   = $8232;
		R16I                                                   = $8233;
		R16UI                                                  = $8234;
		R32I                                                   = $8235;
		R32UI                                                  = $8236;
		RG8I                                                   = $8237;
		RG8UI                                                  = $8238;
		RG16I                                                  = $8239;
		RG16UI                                                 = $823A;
		RG32I                                                  = $823B;
		RG32UI                                                 = $823C;

	// ARB_vertex_array_object
	const
		VERTEX_ARRAY_BINDING                                   = $85B5;

	class var
		BindVertexArray: procedure(array_: uint); stdcall;
		DeleteVertexArrays: procedure(n: sizei; arrays: pUint); stdcall;
		GenVertexArrays: procedure(n: sizei; arrays: pUint); stdcall;
		// IsVertexArray: function(array_: uint): boolean; stdcall;

	// VERSION_3_1
	const
		SAMPLER_2D_RECT                                        = $8B63;
		SAMPLER_2D_RECT_SHADOW                                 = $8B64;
		SAMPLER_BUFFER                                         = $8DC2;
		INT_SAMPLER_2D_RECT                                    = $8DCD;
		INT_SAMPLER_BUFFER                                     = $8DD0;
		UNSIGNED_INT_SAMPLER_2D_RECT                           = $8DD5;
		UNSIGNED_INT_SAMPLER_BUFFER                            = $8DD8;
		TEXTURE_BUFFER                                         = $8C2A;
		MAX_TEXTURE_BUFFER_SIZE                                = $8C2B;
		TEXTURE_BINDING_BUFFER                                 = $8C2C;
		TEXTURE_BUFFER_DATA_STORE_BINDING                      = $8C2D;
		TEXTURE_BUFFER_FORMAT                                  = $8C2E;
		TEXTURE_RECTANGLE                                      = $84F5;
		TEXTURE_BINDING_RECTANGLE                              = $84F6;
		PROXY_TEXTURE_RECTANGLE                                = $84F7;
		MAX_RECTANGLE_TEXTURE_SIZE                             = $84F8;
		RED_SNORM                                              = $8F90;
		RG_SNORM                                               = $8F91;
		RGB_SNORM                                              = $8F92;
		RGBA_SNORM                                             = $8F93;
		R8_SNORM                                               = $8F94;
		RG8_SNORM                                              = $8F95;
		RGB8_SNORM                                             = $8F96;
		RGBA8_SNORM                                            = $8F97;
		R16_SNORM                                              = $8F98;
		RG16_SNORM                                             = $8F99;
		RGB16_SNORM                                            = $8F9A;
		RGBA16_SNORM                                           = $8F9B;
		SIGNED_NORMALIZED                                      = $8F9C;
		PRIMITIVE_RESTART                                      = $8F9D;
		PRIMITIVE_RESTART_INDEX                                = $8F9E;

	class var
		// DrawArraysInstanced: procedure(mode: enum; first: int; count: sizei; primcount: sizei); stdcall;
		DrawElementsInstanced: procedure(mode: enum; count: sizei; type_: enum; indices: pointer; primcount: sizei); stdcall;
		// TexBuffer: procedure(target: enum; internalformat: enum; buffer: uint); stdcall;
		PrimitiveRestartIndex: procedure(index: uint); stdcall;

	// ARB_copy_buffer
	const
		COPY_READ_BUFFER                                       = $8F36;
		COPY_WRITE_BUFFER                                      = $8F37;

	class var
		// CopyBufferSubData: procedure(readTarget: enum; writeTarget: enum; readOffset: intptr; writeOffset: intptr; size: sizeiptr); stdcall;

	// ARB_uniform_buffer_object
	const
		UNIFORM_BUFFER                                         = $8A11;
		UNIFORM_BUFFER_BINDING                                 = $8A28;
		UNIFORM_BUFFER_START                                   = $8A29;
		UNIFORM_BUFFER_SIZE                                    = $8A2A;
		MAX_VERTEX_UNIFORM_BLOCKS                              = $8A2B;
		MAX_GEOMETRY_UNIFORM_BLOCKS                            = $8A2C;
		MAX_FRAGMENT_UNIFORM_BLOCKS                            = $8A2D;
		MAX_COMBINED_UNIFORM_BLOCKS                            = $8A2E;
		MAX_UNIFORM_BUFFER_BINDINGS                            = $8A2F;
		MAX_UNIFORM_BLOCK_SIZE                                 = $8A30;
		MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS                 = $8A31;
		MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS               = $8A32;
		MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS               = $8A33;
		UNIFORM_BUFFER_OFFSET_ALIGNMENT                        = $8A34;
		ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH                   = $8A35;
		ACTIVE_UNIFORM_BLOCKS                                  = $8A36;
		UNIFORM_TYPE                                           = $8A37;
		UNIFORM_SIZE                                           = $8A38;
		UNIFORM_NAME_LENGTH                                    = $8A39;
		UNIFORM_BLOCK_INDEX                                    = $8A3A;
		UNIFORM_OFFSET                                         = $8A3B;
		UNIFORM_ARRAY_STRIDE                                   = $8A3C;
		UNIFORM_MATRIX_STRIDE                                  = $8A3D;
		UNIFORM_IS_ROW_MAJOR                                   = $8A3E;
		UNIFORM_BLOCK_BINDING                                  = $8A3F;
		UNIFORM_BLOCK_DATA_SIZE                                = $8A40;
		UNIFORM_BLOCK_NAME_LENGTH                              = $8A41;
		UNIFORM_BLOCK_ACTIVE_UNIFORMS                          = $8A42;
		UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES                   = $8A43;
		UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER              = $8A44;
		UNIFORM_BLOCK_REFERENCED_BY_GEOMETRY_SHADER            = $8A45;
		UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER            = $8A46;
		INVALID_INDEX                                          = $FFFFFFFF;

	class var
		// GetUniformIndices: procedure(program_: uint; uniformCount: sizei; uniformNames: pChar; uniformIndices: pUint); stdcall;
		GetActiveUniformsiv: procedure(program_: uint; uniformCount: sizei; uniformIndices: pUint; pname: enum; params: pInt); stdcall;
		GetActiveUniformName: procedure(program_: uint; uniformIndex: uint; bufSize: sizei; length: pSizei; uniformName: pChar); stdcall;
		// GetUniformBlockIndex: function(program_: uint; uniformBlockName: pChar): uint; stdcall;
		GetActiveUniformBlockiv: procedure(program_: uint; uniformBlockIndex: uint; pname: enum; params: pInt); stdcall;
		GetActiveUniformBlockName: procedure(program_: uint; uniformBlockIndex: uint; bufSize: sizei; length: pSizei; uniformBlockName: pChar); stdcall;
		UniformBlockBinding: procedure(program_: uint; uniformBlockIndex: uint; uniformBlockBinding: uint); stdcall;

	// VERSION_3_2
	const
		CONTEXT_CORE_PROFILE_BIT                               = $00000001;
		CONTEXT_COMPATIBILITY_PROFILE_BIT                      = $00000002;
		LINES_ADJACENCY                                        = $000A;
		LINE_STRIP_ADJACENCY                                   = $000B;
		TRIANGLES_ADJACENCY                                    = $000C;
		TRIANGLE_STRIP_ADJACENCY                               = $000D;
		PROGRAM_POINT_SIZE                                     = $8642;
		MAX_GEOMETRY_TEXTURE_IMAGE_UNITS                       = $8C29;
		FRAMEBUFFER_ATTACHMENT_LAYERED                         = $8DA7;
		FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS                   = $8DA8;
		GEOMETRY_SHADER                                        = $8DD9;
		GEOMETRY_VERTICES_OUT                                  = $8916;
		GEOMETRY_INPUT_TYPE                                    = $8917;
		GEOMETRY_OUTPUT_TYPE                                   = $8918;
		MAX_GEOMETRY_UNIFORM_COMPONENTS                        = $8DDF;
		MAX_GEOMETRY_OUTPUT_VERTICES                           = $8DE0;
		MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS                   = $8DE1;
		MAX_VERTEX_OUTPUT_COMPONENTS                           = $9122;
		MAX_GEOMETRY_INPUT_COMPONENTS                          = $9123;
		MAX_GEOMETRY_OUTPUT_COMPONENTS                         = $9124;
		MAX_FRAGMENT_INPUT_COMPONENTS                          = $9125;
		CONTEXT_PROFILE_MASK                                   = $9126;

	class var
		// GetInteger64i_v: procedure(target: enum; index: uint; data: pInt64); stdcall;
		// GetBufferParameteri64v: procedure(target: enum; pname: enum; params: pInt64); stdcall;
		// ProgramParameteri: procedure(program_: uint; pname: enum; value: int); stdcall;
		// FramebufferTexture: procedure(target: enum; attachment: enum; texture: uint; level: int); stdcall;

	// ARB_depth_clamp

	const
		DEPTH_CLAMP                                            = $864F;

	// ARB_draw_elements_base_vertex
	class var
		// DrawElementsBaseVertex: procedure(mode: enum; count: sizei; type_: enum; indices: pointer; basevertex: int); stdcall;
		// DrawRangeElementsBaseVertex: procedure(mode: enum; start: uint; end_: uint; count: sizei; type_: enum; indices: pointer; basevertex: int); stdcall;
		// DrawElementsInstancesBaseVertex: procedure(mode: enum; count: sizei; type_: enum; indices: pointer; primcount: sizei; basevertex: int); stdcall;
		// MultiDrawElementsBaseVertex: procedure(mode: enum; count: pSizei; type_: enum; indices: pointer; primcount: sizei; basevertex: pInt); stdcall;

	// ARB_fragment_coord_conventions

	// ARB_provoking_vertex
	const
		QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION               = $8E4C;
		FIRST_VERTEX_CONVENTION                                = $8E4D;
		LAST_VERTEX_CONVENTION                                 = $8E4E;
		PROVOKING_VERTEX                                       = $8E4F;

	class var
		// ProvokingVertex: procedure(mode: enum); stdcall;

	// ARB_seamless_cube_map
	const
		TEXTURE_CUBE_MAP_SEAMLESS                              = $884F;

	// ARB_sync
	const
		MAX_SERVER_WAIT_TIMEOUT                                = $9111;
		OBJECT_TYPE                                            = $9112;
		SYNC_CONDITION                                         = $9113;
		SYNC_STATUS                                            = $9114;
		SYNC_FLAGS                                             = $9115;
		SYNC_FENCE                                             = $9116;
		SYNC_GPU_COMMANDS_COMPLETE                             = $9117;
		UNSIGNALED                                             = $9118;
		SIGNALED                                               = $9119;
		ALREADY_SIGNALED                                       = $911A;
		TIMEOUT_EXPIRED                                        = $911B;
		CONDITION_SATISFIED                                    = $911C;
		WAIT_FAILED                                            = $911D;
		SYNC_FLUSH_COMMANDS_BIT                                = $00000001;
		TIMEOUT_IGNORED                                        = $FFFFFFFFFFFFFFFF; // originally $FFFFFFFFFFFFFFFFull, wtf?

	class var
		// FenceSync: function(condition: enum; flags: bitfield): sync; stdcall;
		// IsSync: function(sync: sync): boolean; stdcall;
		// DeleteSync: procedure(sync: sync); stdcall;
		// ClientWaitSync: function(sync: sync; flags: bitfield; timeout: uint64): enum; stdcall;
		// WaitSync: procedure(sync: sync; flags: bitfield; timeout: uint64); stdcall;
		// GetInteger64v: procedure(pname: enum; params: pInt64); stdcall;
		// GetSynciv: procedure(sync: sync; pname: enum; bufSize: sizei; length: pSizei; values: pInt); stdcall;

	// ARB_texture_multisample

	const
		SAMPLE_POSITION                                        = $8E50;
		SAMPLE_MASK                                            = $8E51;
		SAMPLE_MASK_VALUE                                      = $8E52;
		MAX_SAMPLE_MASK_WORDS                                  = $8E59;
		TEXTURE_2D_MULTISAMPLE                                 = $9100;
		PROXY_TEXTURE_2D_MULTISAMPLE                           = $9101;
		TEXTURE_2D_MULTISAMPLE_ARRAY                           = $9102;
		PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY                     = $9103;
		TEXTURE_BINDING_2D_MULTISAMPLE                         = $9104;
		TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY                   = $9105;
		TEXTURE_SAMPLES                                        = $9106;
		TEXTURE_FIXED_SAMPLE_LOCATIONS                         = $9107;
		SAMPLER_2D_MULTISAMPLE                                 = $9108;
		INT_SAMPLER_2D_MULTISAMPLE                             = $9109;
		UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE                    = $910A;
		SAMPLER_2D_MULTISAMPLE_ARRAY                           = $910B;
		INT_SAMPLER_2D_MULTISAMPLE_ARRAY                       = $910C;
		UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY              = $910D;
		MAX_COLOR_TEXTURE_SAMPLES                              = $910E;
		MAX_DEPTH_TEXTURE_SAMPLES                              = $910F;
		MAX_INTEGER_SAMPLES                                    = $9110;

	class var
		// TexImage2DMultisample: procedure(target: enum; samples: sizei; internalformat: int; width: sizei; height: sizei; fixedsamplelocations: boolean); stdcall;
		// TexImage3DMultisample: procedure(target: enum; samples: sizei; internalformat: int; width: sizei; height: sizei; depth: sizei; fixedsamplelocations: boolean); stdcall;
		// GetMultisamplefv: procedure(pname: enum; index: uint; val: pFloat); stdcall;
		// SampleMaski: procedure(index: uint; mask: bitfield); stdcall;

	// VERSION_3_3
	// ARB_blend_func_extended
	const
		SRC1_COLOR                                             = $88F9;
		ONE_MINUS_SRC1_COLOR                                   = $88FA;
		ONE_MINUS_SRC1_ALPHA                                   = $88FB;
		MAX_DUAL_SOURCE_DRAW_BUFFERS                           = $88FC;

	class var
		// BindFragDataLocationIndexed: procedure(program_: uint; colorNumber: uint; index: uint; name: pChar); stdcall;
		// GetFragDataIndex: function(program_: uint; name: pChar): int; stdcall;

	// ARB_explicit_attrib_location

	// ARB_occlusion_query2
	const
		ANY_SAMPLES_PASSED                                     = $8C2F;

	// ARB_sampler_objects
	const
		SAMPLER_BINDING                                        = $8919;

	class var
		// GenSamplers: procedure(count: sizei; samplers: pUint); stdcall;
		// DeleteSamplers: procedure(count: sizei; samplers: pUint); stdcall;
		// IsSampler: function(sampler: uint): boolean; stdcall;
		// BindSampler: procedure(unit_: enum; sampler: uint); stdcall;
		// SampInterpolatearameteri: procedure(sampler: uint; pname: enum; param: int); stdcall;
		// SampInterpolatearameteriv: procedure(sampler: uint; pname: enum; param: pInt); stdcall;
		// SampInterpolatearameterf: procedure(sampler: uint; pname: enum; param: float); stdcall;
		// SampInterpolatearameterfv: procedure(sampler: uint; pname: enum; param: pFloat); stdcall;
		// SampInterpolatearameterIiv: procedure(sampler: uint; pname: enum; param: pInt); stdcall;
		// SampInterpolatearameterIuiv: procedure(sampler: uint; pname: enum; param: pUint); stdcall;
		// GetSampInterpolatearameteriv: procedure(sampler: uint; pname: enum; params: pInt); stdcall;
		// GetSampInterpolatearameterIiv: procedure(sampler: uint; pname: enum; params: pInt); stdcall;
		// GetSampInterpolatearameterfv: procedure(sampler: uint; pname: enum; params: pFloat); stdcall;
		// GetSampInterpolatearameterIfv: procedure(sampler: uint; pname: enum; params: pFloat); stdcall;

	// ARB_shader_bit_encoding

	// ARB_texture_rgb10_a2ui
	const
		RGB10_A2UI                                             = $906F;

	// ARB_texture_swizzle
	const
		TEXTURE_SWIZZLE_R                                      = $8E42;
		TEXTURE_SWIZZLE_G                                      = $8E43;
		TEXTURE_SWIZZLE_B                                      = $8E44;
		TEXTURE_SWIZZLE_A                                      = $8E45;
		TEXTURE_SWIZZLE_RGBA                                   = $8E46;

	// ARB_timer_query
	const
		TIME_ELAPSED                                           = $88BF;
		TIMESTAMP                                              = $8E28;

	class var
		// QueryCounter: procedure(id: uint; target: enum); stdcall;
		// GetQueryObjecti64v: procedure(id: uint; pname: enum; params: pInt64); stdcall;
		// GetQueryObjectui64v: procedure(id: uint; pname: enum; params: pUint64); stdcall;

	// ARB_vertex_type_2_10_10_10_rev
	const
		INT_2_10_10_10_REV                                     = $8D9F;

	// VERSION_4_0
	const
		SAMPLE_SHADING                                         = $8C36;
		MIN_SAMPLE_SHADING_VALUE                               = $8C37;
		MIN_PROGRAM_TEXTURE_GATHER_OFFSET                      = $8E5E;
		MAX_PROGRAM_TEXTURE_GATHER_OFFSET                      = $8E5F;
		TEXTURE_CUBE_MAP_ARRAY                                 = $9009;
		TEXTURE_BINDING_CUBE_MAP_ARRAY                         = $900A;
		PROXY_TEXTURE_CUBE_MAP_ARRAY                           = $900B;
		SAMPLER_CUBE_MAP_ARRAY                                 = $900C;
		SAMPLER_CUBE_MAP_ARRAY_SHADOW                          = $900D;
		INT_SAMPLER_CUBE_MAP_ARRAY                             = $900E;
		UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY                    = $900F;

	// ARB_draw_buffers_blend
	class var
		// BlendEquationiARB: procedure(buf: uint; mode: enum); stdcall;
		// BlendEquationSeparateiARB: procedure(buf: uint; modeRGB: enum; modeAlpha: enum); stdcall;
		// BlendFunciARB: procedure(buf: uint; src: enum; dst: enum); stdcall;
		// BlendFuncSeparateiARB: procedure(buf: uint; srcRGB: enum; dstRGB: enum; srcAlpha: enum; dstAlpha: enum); stdcall;

	// ARB_sample_shading
	class var
		// MinSampleShadingARB: procedure(value: float); stdcall;

	// ARB_draw_indirect
	const
		DRAW_INDIRECT_BUFFER                                   = $8F3F;
		DRAW_INDIRECT_BUFFER_BINDING                           = $8F43;

	class var
		// DrawArraysIndirect: procedure(mode: enum; indirect: pointer); stdcall;
		// DrawElementsIndirect: procedure(mode: enum; type_: enum; indirect: pointer); stdcall;

	// ARB_gpu_shader5
	const
		GEOMETRY_SHADER_INVOCATIONS                            = $887F;
		MAX_GEOMETRY_SHADER_INVOCATIONS                        = $8E5A;
		MIN_FRAGMENT_INTERPOLATION_OFFSET                      = $8E5B;
		MAX_FRAGMENT_INTERPOLATION_OFFSET                      = $8E5C;
		FRAGMENT_INTERPOLATION_OFFSET_BITS                     = $8E5D;
		MAX_VERTEX_STREAMS                                     = $8E71;

	// ARB_gpu_shader_fp64
	const
		DOUBLE_VEC2                                            = $8FFC;
		DOUBLE_VEC3                                            = $8FFD;
		DOUBLE_VEC4                                            = $8FFE;
		DOUBLE_MAT2                                            = $8F46;
		DOUBLE_MAT3                                            = $8F47;
		DOUBLE_MAT4                                            = $8F48;
		DOUBLE_MAT2x3                                          = $8F49;
		DOUBLE_MAT2x4                                          = $8F4A;
		DOUBLE_MAT3x2                                          = $8F4B;
		DOUBLE_MAT3x4                                          = $8F4C;
		DOUBLE_MAT4x2                                          = $8F4D;
		DOUBLE_MAT4x3                                          = $8F4E;

	class var
		// Uniform1d: procedure(location: int; x: double); stdcall;
		// Uniform2d: procedure(location: int; x: double; y: double); stdcall;
		// Uniform3d: procedure(location: int; x: double; y: double; z: double); stdcall;
		// Uniform4d: procedure(location: int; x: double; y: double; z: double; w: double); stdcall;
		// Uniform1dv: procedure(location: int; count: sizei; value: pDouble); stdcall;
		// Uniform2dv: procedure(location: int; count: sizei; value: pDouble); stdcall;
		// Uniform3dv: procedure(location: int; count: sizei; value: pDouble); stdcall;
		// Uniform4dv: procedure(location: int; count: sizei; value: pDouble); stdcall;
		// UniformMatrix2dv: procedure(location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// UniformMatrix3dv: procedure(location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// UniformMatrix4dv: procedure(location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// UniformMatrix2x3dv: procedure(location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// UniformMatrix2x4dv: procedure(location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// UniformMatrix3x2dv: procedure(location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// UniformMatrix3x4dv: procedure(location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// UniformMatrix4x2dv: procedure(location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// UniformMatrix4x3dv: procedure(location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// GetUniformdv: procedure(program_: uint; location: int; params: pDouble); stdcall;
		// ProgramUniform1d: procedure(program_: uint; location: int; x: double); stdcall;
		// ProgramUniform2d: procedure(program_: uint; location: int; x: double; y: double); stdcall;
		// ProgramUniform3d: procedure(program_: uint; location: int; x: double; y: double; z: double); stdcall;
		// ProgramUniform4d: procedure(program_: uint; location: int; x: double; y: double; z: double; w: double); stdcall;
		// ProgramUniform1dv: procedure(program_: uint; location: int; count: sizei; value: pDouble); stdcall;
		// ProgramUniform2dv: procedure(program_: uint; location: int; count: sizei; value: pDouble); stdcall;
		// ProgramUniform3dv: procedure(program_: uint; location: int; count: sizei; value: pDouble); stdcall;
		// ProgramUniform4dv: procedure(program_: uint; location: int; count: sizei; value: pDouble); stdcall;
		// ProgramUniformMatrix2dv: procedure(program_: uint; location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// ProgramUniformMatrix3dv: procedure(program_: uint; location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// ProgramUniformMatrix4dv: procedure(program_: uint; location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// ProgramUniformMatrix2x3dv: procedure(program_: uint; location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// ProgramUniformMatrix2x4dv: procedure(program_: uint; location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// ProgramUniformMatrix3x2dv: procedure(program_: uint; location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// ProgramUniformMatrix3x4dv: procedure(program_: uint; location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// ProgramUniformMatrix4x2dv: procedure(program_: uint; location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;
		// ProgramUniformMatrix4x3dv: procedure(program_: uint; location: int; count: sizei; transpose: boolean; value: pDouble); stdcall;

	// ARB_shader_subroutine
	const
		ACTIVE_SUBROUTINES                                     = $8DE5;
		ACTIVE_SUBROUTINE_UNIFORMS                             = $8DE6;
		ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS                    = $8E47;
		ACTIVE_SUBROUTINE_MAX_LENGTH                           = $8E48;
		ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH                   = $8E49;
		MAX_SUBROUTINES                                        = $8DE7;
		MAX_SUBROUTINE_UNIFORM_LOCATIONS                       = $8DE8;
		NUM_COMPATIBLE_SUBROUTINES                             = $8E4A;
		COMPATIBLE_SUBROUTINES                                 = $8E4B;

	class var
		// GetSubroutineUniformLocation: function(program_: uint; shadertype: enum; name: pChar): int; stdcall;
		// GetSubroutineIndex: function(program_: uint; shadertype: enum; name: pChar): uint; stdcall;
		// GetActiveSubroutineUniformiv: procedure(program_: uint; shadertype: enum; index: uint; pname: enum; values: pInt); stdcall;
		// GetActiveSubroutineUniformName: procedure(program_: uint; shadertype: enum; index: uint; bufsize: sizei; length: pSizei; name: pChar); stdcall;
		// GetActiveSubroutineName: procedure(program_: uint; shadertype: enum; index: uint; bufsize: sizei; length: pSizei; name: pChar); stdcall;
		// UniformSubroutinesuiv: procedure(shadertype: enum; count: sizei; indices: pUint); stdcall;
		// GetUniformSubroutineuiv: procedure(shadertype: enum; location: int; params: pUint); stdcall;
		// GetProgramStageiv: procedure(program_: uint; shadertype: enum; pname: enum; values: pInt); stdcall;

	// ARB_tessellation_shader
	const
		PATCHES                                                = $000E;
		PATCH_VERTICES                                         = $8E72;
		PATCH_DEFAULT_INNER_LEVEL                              = $8E73;
		PATCH_DEFAULT_OUTER_LEVEL                              = $8E74;
		TESS_CONTROL_OUTPUT_VERTICES                           = $8E75;
		TESS_GEN_MODE                                          = $8E76;
		TESS_GEN_SPACING                                       = $8E77;
		TESS_GEN_VERTEX_ORDER                                  = $8E78;
		TESS_GEN_POINT_MODE                                    = $8E79;
		ISOLINES                                               = $8E7A;
		FRACTIONAL_ODD                                         = $8E7B;
		FRACTIONAL_EVEN                                        = $8E7C;
		MAX_PATCH_VERTICES                                     = $8E7D;
		MAX_TESS_GEN_LEVEL                                     = $8E7E;
		MAX_TESS_CONTROL_UNIFORM_COMPONENTS                    = $8E7F;
		MAX_TESS_EVALUATION_UNIFORM_COMPONENTS                 = $8E80;
		MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS                   = $8E81;
		MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS                = $8E82;
		MAX_TESS_CONTROL_OUTPUT_COMPONENTS                     = $8E83;
		MAX_TESS_PATCH_COMPONENTS                              = $8E84;
		MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS               = $8E85;
		MAX_TESS_EVALUATION_OUTPUT_COMPONENTS                  = $8E86;
		MAX_TESS_CONTROL_UNIFORM_BLOCKS                        = $8E89;
		MAX_TESS_EVALUATION_UNIFORM_BLOCKS                     = $8E8A;
		MAX_TESS_CONTROL_INPUT_COMPONENTS                      = $886C;
		MAX_TESS_EVALUATION_INPUT_COMPONENTS                   = $886D;
		MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS           = $8E1E;
		MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS        = $8E1F;
		UNIFORM_BLOCK_REFERENCED_BY_TESS_CONTROL_SHADER        = $84F0;
		UNIFORM_BLOCK_REFERENCED_BY_TESS_EVALUATION_SHADER     = $84F1;
		TESS_EVALUATION_SHADER                                 = $8E87;
		TESS_CONTROL_SHADER                                    = $8E88;

	class var
		// PatchParameteri: procedure(pname: enum; value: int); stdcall;
		// PatchParameterfv: procedure(pname: enum; values: pFloat); stdcall;

	// ARB_texture_buffer_object_rgb32

	// ARB_transform_feedback2
	const
		TRANSFORM_FEEDBACK                                     = $8E22;
		TRANSFORM_FEEDBACK_BUFFER_PAUSED                       = $8E23;
		TRANSFORM_FEEDBACK_BUFFER_ACTIVE                       = $8E24;
		TRANSFORM_FEEDBACK_BINDING                             = $8E25;

	class var
		// BindTransformFeedback: procedure(target: enum; id: uint); stdcall;
		// DeleteTransformFeedbacks: procedure(n: sizei; ids: pUint); stdcall;
		// GenTransformFeedbacks: procedure(n: sizei; ids: pUint); stdcall;
		// IsTransformFeedback: function(id: uint): boolean; stdcall;
		// PauseTransformFeedback: procedure; stdcall;
		// ResumeTransformFeedback: procedure; stdcall;
		// DrawTransformFeedback: procedure(mode: enum; id: uint); stdcall;

	// ARB_transform_feedback3
	const
		MAX_TRANSFORM_FEEDBACK_BUFFERS                         = $8E70;

	class var
		// DrawTransformFeedbackStream: procedure(mode: enum; id: uint; stream: uint); stdcall;
		// BeginQueryIndexed: procedure(target: enum; index: uint; id: uint); stdcall;
		// EndQueryIndexed: procedure(target: enum; index: uint); stdcall;
		// GetQueryIndexediv: procedure(target: enum; index: uint; pname: enum; params: pInt); stdcall;

	// EXT_texture_filter_anisotropic
	const
		TEXTURE_MAX_ANISOTROPY = $84FE;
		MAX_TEXTURE_MAX_ANISOTROPY = $84FF;

	// EXT_texture_compression_s3tc
	const
		COMPRESSED_RGB_S3TC_DXT1                       = $83F0;
		COMPRESSED_RGBA_S3TC_DXT1                      = $83F1;
		COMPRESSED_RGBA_S3TC_DXT3                      = $83F2;
		COMPRESSED_RGBA_S3TC_DXT5                      = $83F3;

	// ATI_meminfo
	const
		VBO_FREE_MEMORY_ATI                            = $87FB;
		TEXTURE_FREE_MEMORY_ATI                        = $87FC;
		RENDERBUFFER_FREE_MEMORY_ATI                   = $87FD;

	// NVX_gpu_memory_info
	const
		GPU_MEMORY_INFO_DEDICATED_VIDMEM_NVX              = $9047;
		GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX        = $9048;
		GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX      = $9049;
		GPU_MEMORY_INFO_EVICTION_COUNT_NVX                = $904A;
		GPU_MEMORY_INFO_EVICTED_MEMORY_NVX                = $904B;

	// EXT_direct_state_access
	// http://www.opengl.org/registry/specs/EXT/direct_state_access.txt
	class var
		// TextureImage2D: procedure(texture: uint; target: enum; level: int; internalformat: int; width, height: sizei; border: int; format: enum; &type: enum; pixels: pointer);
		// TextureImage3D: procedure(texture: uint; target: enum; level: int; internalformat: int; width, height, depth: sizei; border: int; format: enum; &type: enum; pixels: pointer);
		// CompressedTextureImage2D: procedure(texture: uint; target: enum; level: int; internalformat: enum; width, height: sizei; border: int; imageSize: sizei; data: pointer);
		// CompressedTextureImage3D: procedure(texture: uint; target: enum; level: int; internalformat: enum; width, height, depth: sizei; border: int; imageSize: sizei; data: pointer);
		// GetTextureLevelParameteriv: procedure(texture: uint; target: enum; level: int; pname: enum; params: pInt);
		ProgramUniform1i: procedure(&program: uint; location: int; v0: int); stdcall;
		ProgramUniform1fv,
		ProgramUniform2fv,
		ProgramUniform3fv,
		ProgramUniform4fv: procedure(&program: uint; location: int; count: sizei; value: pFloat); stdcall;
		ProgramUniformMatrix4fv: procedure(&program: uint; location: int; count: sizei; transpose: boolean; value: pFloat); stdcall;
		NamedBufferData: procedure(buffer: uint; size: sizeiptr; data: pointer; usage: enum); stdcall;
		NamedBufferSubData: procedure(buffer: uint; offset: intptr; size: sizeiptr; data: pointer); stdcall;

	// ARB_direct_state_access
	// https://www.opengl.org/registry/specs/ARB/direct_state_access.txt
	class var
		CreateBuffers: procedure(n: sizei; buffers: pUint); stdcall;

	// ARB_vertex_attrib_binding
	// https://www.opengl.org/registry/specs/ARB/vertex_attrib_binding.txt
	class var
		// VertexArrayBindVertexBuffer: procedure(vaobj: uint; bindingindex: uint; buffer: uint; offset: intptr; stride: sizei); stdcall;
		// VertexArrayVertexAttribFormat: procedure(vaobj: uint; attribindex: uint; size: int; &type: enum; normalized: boolean; relativeoffset: uint); stdcall;
		// VertexArrayVertexAttribBinding: procedure(vaobj: uint; attribindex: uint; bindingindex: uint); stdcall;

	// ARB_get_progam_binary
	// http://www.opengl.org/registry/specs/ARB/get_program_binary.txt
	const
		PROGRAM_BINARY_RETRIEVABLE_HINT = $8257;
		PROGRAM_BINARY_LENGTH           = $8741;
		NUM_PROGRAM_BINARY_FORMATS      = $87FE;
		PROGRAM_BINARY_FORMATS          = $87FF;

	class var
		ProgramParameteri: procedure(program_: uint; pname: enum; value: int); stdcall;
		GetProgramBinary: procedure(program_: uint; bufSize: sizei; length: pSizei; out binaryFormat: enum; binary: pointer); stdcall;
		ProgramBinary: procedure(program_: uint; binaryFormat: enum; binary: pointer; length: sizei); stdcall;

	{$ifdef Debug}
	// ARB_debug_output
	// http://www.opengl.org/registry/specs/ARB/debug_output.txt

	type
		DebugProc = procedure(source, typ: enum; id: uint; severity: enum; length: sizei; message: pChar; userParam: pointer); stdcall;

	const
		DEBUG_SOURCE_API             = $8246;
		DEBUG_SOURCE_WINDOW_SYSTEM   = $8247;
		DEBUG_SOURCE_SHADER_COMPILER = $8248;
		DEBUG_SOURCE_THIRD_PARTY     = $8249;
		DEBUG_SOURCE_APPLICATION     = $824A;
		DEBUG_SOURCE_OTHER           = $824B;

		DEBUG_TYPE_ERROR                = $824C;
		DEBUG_TYPE_DEPRECATED_BEHAVIOR  = $824D;
		DEBUG_TYPE_UNDEFINED_BEHAVIOR   = $824E;
		DEBUG_TYPE_PORTABILITY          = $824F;
		DEBUG_TYPE_PERFORMANCE          = $8250;
		DEBUG_TYPE_OTHER                = $8251;

		DEBUG_SEVERITY_HIGH    = $9146;
		DEBUG_SEVERITY_MEDIUM  = $9147;
		DEBUG_SEVERITY_LOW     = $9148;

	class var
		DebugMessageCallback: procedure(callback: DebugProc; userParam: pointer); stdcall;
	{$endif}

		class function DescribeErrorInline(enum: enum): string;
	end;

implementation

uses
	OpenGL_Hacks;

var
	loader: DLLoader;

	class function gl.DescribeErrorInline(enum: enum): string;
	begin
		case enum of
			INVALID_ENUM: result := 'INVALID_ENUM';
			INVALID_VALUE: result := 'INVALID_VALUE';
			INVALID_OPERATION: result := 'INVALID_OPERATION';
			OUT_OF_MEMORY: result := 'OUT_OF_MEMORY';
			INVALID_FRAMEBUFFER_OPERATION: result := 'INVALID_FRAMEBUFFER_OPERATION';
			else result := InlineUnknownErrorCodeMsg(enum);
		end;
	end;

	procedure DescribeOpenGLFunctions(var fns: DLLoader.FunctionsList);
	const
		Optional = DLLoader.Optional;
		Crash    = 'XUI';
	begin
		Assert(Crash = Crash);
		fns
		.Func(@gl.CullFace,                   'CullFace')^
		.Func(@gl.FrontFace,                  'FrontFace')^
		.Func(@gl.PolygonMode,                'PolygonMode')^
		.Func(@gl.TexParameterf,              'TexParameterf')^
		.Func(@gl.TexParameteri,              'TexParameteri')^
		.Func(@gl.TexParameteriv,             'TexParameteriv')^
		.Func(@gl.TexImage2D,                 'TexImage2D')^
		.Func(@gl.Clear,                      'Clear')^
		.Func(@gl.ClearColor,                 'ClearColor')^
		.Func(@gl.DepthMask,                  'DepthMask')^
		.Func(@gl.Disable,                    'Disable')^
		.Func(@gl.Enable,                     'Enable')^
		.Func(@gl.Finish,                     'Finish')^
		.Func(@gl.BlendFunc,                  'BlendFunc')^
		.Func(@gl.DepthFunc,                  'DepthFunc')^
		.Func(@gl.PixelStorei,                'PixelStorei')^
		.Func(@gl.ReadBuffer,                 'ReadBuffer')^
		.Func(@gl.ReadPixels,                 'ReadPixels')^
		.Func(@gl.GetError,                   'GetError')^
		.Func(@gl.GetFloatv,                  'GetFloatv')^
		.Func(@gl.GetIntegerv,                'GetIntegerv')^
		.Func(@gl.GetString,                  'GetString')^
		.Func(@gl.GetTexImage,                'GetTexImage')^
		.Func(@gl.GetTexLevelParameteriv,     'GetTexLevelParameteriv')^
		.Func(@gl.Viewport,                   'Viewport')^
		.Func(@gl.DrawElements,               'DrawElements')^
		.Func(@gl.TexSubImage2D,              'TexSubImage2D')^
		.Func(@gl.BindTexture,                'BindTexture')^
		.Func(@gl.DeleteTextures,             'DeleteTextures')^
		.Func(@gl.GenTextures,                'GenTextures')^
		.Func(@gl.TexImage3D,                 'TexImage3D')^
		.Func(@gl.TexSubImage3D,              'TexSubImage3D')^
		.Func(@gl.ActiveTexture,              'ActiveTexture')^
		.Func(@gl.CompressedTexImage3D,       'CompressedTexImage3D')^
		.Func(@gl.CompressedTexImage2D,       'CompressedTexImage2D')^
		.Func(@gl.CompressedTexSubImage3D,    'CompressedTexSubImage3D')^
		.Func(@gl.CompressedTexSubImage2D,    'CompressedTexSubImage2D')^
		.Func(@gl.GetCompressedTexImage,      'GetCompressedTexImage')^
		.Func(@gl.BindBuffer,                 'BindBuffer')^
		.Func(@gl.DeleteBuffers,              'DeleteBuffers')^
		.Func(@gl.GenBuffers,                 'GenBuffers' + DLLoader.SkipIfPresented + 'CreateBuffers' {$ifdef CrashCreateBuffers} + Crash {$endif})^
		.Func(@gl.BufferData,                 'BufferData')^
		.Func(@gl.BufferSubData,              'BufferSubData')^
		.Func(@gl.DrawBuffers,                'DrawBuffers')^
		.Func(@gl.AttachShader,               'AttachShader')^
		.Func(@gl.CompileShader,              'CompileShader')^
		.Func(@gl.CreateProgram,              'CreateProgram')^
		.Func(@gl.CreateShader,               'CreateShader')^
		.Func(@gl.DeleteProgram,              'DeleteProgram')^
		.Func(@gl.DeleteShader,               'DeleteShader')^
		.Func(@gl.DetachShader,               'DetachShader')^
		.Func(@gl.DisableVertexAttribArray,   'DisableVertexAttribArray')^
		.Func(@gl.EnableVertexAttribArray,    'EnableVertexAttribArray')^
		.Func(@gl.GetActiveAttrib,            'GetActiveAttrib')^
		.Func(@gl.GetActiveUniform,           'GetActiveUniform')^
		.Func(@gl.GetAttribLocation,          'GetAttribLocation')^
		.Func(@gl.GetProgramiv,               'GetProgramiv')^
		.Func(@gl.GetProgramInfoLog,          'GetProgramInfoLog')^
		.Func(@gl.GetShaderiv,                'GetShaderiv')^
		.Func(@gl.GetShaderInfoLog,           'GetShaderInfoLog')^
		.Func(@gl.GetUniformLocation,         'GetUniformLocation')^
		.Func(@gl.LinkProgram,                'LinkProgram')^
		.Func(@gl.ShaderSource,               'ShaderSource')^
		.Func(@gl.UseProgram,                 'UseProgram')^
		.Func(@gl.Uniform1i,                  'Uniform1i')^
		.Func(@gl.Uniform1fv,                 'Uniform1fv')^
		.Func(@gl.Uniform2fv,                 'Uniform2fv')^
		.Func(@gl.Uniform3fv,                 'Uniform3fv')^
		.Func(@gl.Uniform4fv,                 'Uniform4fv')^
		.Func(@gl.Uniform1iv,                 'Uniform1iv')^
		.Func(@gl.UniformMatrix4fv,           'UniformMatrix4fv')^
		.Func(@gl.ValidateProgram,            'ValidateProgram')^
		.Func(@gl.VertexAttribPointer,        'VertexAttribPointer')^
		.Func(@gl.BindBufferBase,             'BindBufferBase' {$ifdef CrashUBO} + Crash {$endif} + Optional)^
		// .Func(@gl.BindFragDataLocation,       'BindFragDataLocation' + Optional)^
		.Func(@gl.GetStringi,                 'GetStringi' + Optional)^
		.Func(@gl.BindFramebuffer,            'BindFramebuffer')^
		.Func(@gl.DeleteFramebuffers,         'DeleteFramebuffers')^
		.Func(@gl.GenFramebuffers,            'GenFramebuffers')^
		.Func(@gl.CheckFramebufferStatus,     'CheckFramebufferStatus')^
		.Func(@gl.FramebufferTexture2D,       'FramebufferTexture2D')^
		.Func(@gl.BindVertexArray,            'BindVertexArray' {$ifdef CrashVAO} + Crash {$endif} + Optional)^
		.Func(@gl.DeleteVertexArrays,         'DeleteVertexArrays' {$ifdef CrashVAO} + Crash {$endif} + Optional)^
		.Func(@gl.GenVertexArrays,            'GenVertexArrays' {$ifdef CrashVAO} + Crash {$endif} + Optional)^
		.Func(@gl.DrawElementsInstanced,      'DrawElementsInstanced' {$ifdef CrashInsta} + Crash {$endif} + Optional)^
		.Func(@gl.PrimitiveRestartIndex,      'PrimitiveRestartIndex' {$ifdef CrashPrimitiveRestart} + Crash {$endif} + Optional)^
		.Func(@gl.GetActiveUniformsiv,        'GetActiveUniformsiv' {$ifdef CrashUBO} + Crash {$endif} + Optional)^
		.Func(@gl.GetActiveUniformName,       'GetActiveUniformName' {$ifdef CrashUBO} + Crash {$endif} + Optional)^
		.Func(@gl.GetActiveUniformBlockiv,    'GetActiveUniformBlockiv' {$ifdef CrashUBO} + Crash {$endif} + Optional)^
		.Func(@gl.GetActiveUniformBlockName,  'GetActiveUniformBlockName' {$ifdef CrashUBO} + Crash {$endif} + Optional)^
		.Func(@gl.UniformBlockBinding,        'UniformBlockBinding' {$ifdef CrashUBO} + Crash {$endif} + Optional)^

		// .Func(@gl.TextureImage2D,             'TextureImage2D' {$ifdef CrashDSA} + Crash {$endif} + Optional)^
		// .Func(@gl.TextureImage3D,             'TextureImage3D' {$ifdef CrashDSA} + Crash {$endif} + Optional)^
		// .Func(@gl.CompressedTextureImage2D,   'CompressedTextureImage2D' {$ifdef CrashDSA} + Crash {$endif} + Optional)^
		// .Func(@gl.CompressedTextureImage3D,   'CompressedTextureImage3D' {$ifdef CrashDSA} + Crash {$endif} + Optional)^
		// .Func(@gl.GetTextureLevelParameteriv, 'GetTextureLevelParameteriv' {$ifdef CrashDSA} + Crash {$endif} + Optional)^
		.Func(@gl.ProgramUniform1i,           'ProgramUniform1i' {$ifdef CrashDSA} + Crash {$endif} + Optional)^
		.Func(@gl.ProgramUniform1fv,          'ProgramUniform1fv' {$ifdef CrashDSA} + Crash {$endif} + Optional)^
		.Func(@gl.ProgramUniform2fv,          'ProgramUniform2fv' {$ifdef CrashDSA} + Crash {$endif} + Optional)^
		.Func(@gl.ProgramUniform3fv,          'ProgramUniform3fv' {$ifdef CrashDSA} + Crash {$endif} + Optional)^
		.Func(@gl.ProgramUniform4fv,          'ProgramUniform4fv' {$ifdef CrashDSA} + Crash {$endif} + Optional)^
		.Func(@gl.ProgramUniformMatrix4fv,    'ProgramUniformMatrix4fv' {$ifdef CrashDSA} + Crash {$endif} + Optional)^
		.Func(@gl.NamedBufferData,            'NamedBufferData' {$ifdef CrashDSA} + Crash {$endif} + Optional)^
		.Func(@gl.NamedBufferSubData,         'NamedBufferSubData' {$ifdef CrashDSA} + Crash {$endif} + Optional)^
		.Func(@gl.CreateBuffers,              'CreateBuffers' {$ifdef CrashCreateBuffers} + Crash {$endif} + Optional)^

		// .Func(@gl.VertexArrayBindVertexBuffer,    'VertexArrayBindVertexBuffer' {$ifdef CrashVABinding} + Crash {$endif} + Optional)^
		// .Func(@gl.VertexArrayVertexAttribFormat,  'VertexArrayVertexAttribFormat' {$ifdef CrashVABinding} + Crash {$endif} + Optional)^
		// .Func(@gl.VertexArrayVertexAttribBinding, 'VertexArrayVertexAttribBinding' {$ifdef CrashVABinding} + Crash {$endif} + Optional)^

		.Func(@gl.ProgramParameteri,          'ProgramParameteri' {$ifdef CrashBinaryShaders} + Crash {$endif} + Optional)^
		.Func(@gl.GetProgramBinary,           'GetProgramBinary' {$ifdef CrashBinaryShaders} + Crash {$endif} + Optional)^
		.Func(@gl.ProgramBinary,              'ProgramBinary' {$ifdef CrashBinaryShaders} + Crash {$endif} + Optional)

	{$ifdef Debug}^
		.Func(@gl.DebugMessageCallback,       'DebugMessageCallback' {$ifdef CrashDebug} + Crash {$endif} + Optional)
	{$endif}
		;
	end;

	function FindGLProc(var procName: string): pointer;
	const
		Exts: array[0 .. 2 {$ifdef Debug} + 16 {$endif}] of string = ('', 'EXT', 'ARB' // Устаревшее расширение ARB_shader_objects конфликтует с ядром.
		{$ifdef Debug},                                                                // Можно предусмотреть, если вдруг окажется, что где-то это единственный
			'EXTX', 'NV', 'NVX', 'ATI', 'AMD', '3DFX', 'APPLE', 'HP', 'IBM',            // способ загрузиться.
			'INTEL', 'KTX', 'MESA', 'SGI', 'SGIS', 'SGIX', 'SUN'
		{$endif});
	var
		i: sint;
		curProc: string;
	begin
		for i := 0 to High(Exts) do
		begin
			curProc := procName + Exts[i];
			result := GetGLProcAddress(curProc);
			if Assigned(result) then
			begin
				procName := curProc;
				exit;
			end;
		end;
	end;

	function LoadOpenGL: boolean;
		function Check(const ext: array of pPointer): boolean;
		var
			i: sint;
		begin
			result := yes;
			for i := 0 to High(ext) do
				if not Assigned(ext[i]^) then
				begin
					result := no;
					break;
				end;

			if not result then
				for i := 0 to High(ext) do
					ext[i]^ := nil;
		end;

{$push} {$typedaddress off}
	const
		VAOFns: array[0 .. 2] of pPointer =
		(
			@gl.BindVertexArray,
			@gl.DeleteVertexArrays,
			@gl.GenVertexArrays
		);
		UBOFns: array[0 .. 1] of pPointer =
		(
			@gl.UniformBlockBinding,
			@gl.BindBufferBase
		);
		DSAFns: array[0 .. 7] of pPointer =
		(
			@gl.ProgramUniform1i,
			@gl.ProgramUniform1fv,
			@gl.ProgramUniform2fv,
			@gl.ProgramUniform3fv,
			@gl.ProgramUniform4fv,
			@gl.ProgramUniformMatrix4fv,
			@gl.NamedBufferData,
			@gl.NamedBufferSubData
		);
		BinaryShaderFns: array[0 .. 2] of pPointer =
		(
			@gl.ProgramParameteri,
			@gl.GetProgramBinary,
			@gl.ProgramBinary
		);
		{VABindingFns: array[0 .. 2] of pPointer =
		(
			@gl.VertexArrayBindVertexBuffer,
			@gl.VertexArrayVertexAttribFormat,
			@gl.VertexArrayVertexAttribBinding
		);}
{$pop}
{$ifdef Debug}
	var
		crashList: string;
{$endif}
	begin
	{$ifdef Debug}
		crashList := SeparatedList.Join([
			{$ifdef CrashVAO} 'VAO', {$endif}
			{$ifdef CrashInsta} 'инстансинг', {$endif}
			{$ifdef CrashUBO} 'UBO', {$endif}
			{$ifdef CrashPrimitiveRestart} 'Primitive Restart', {$endif}
			{$ifdef CrashDSA} 'Direct State Access', {$endif}
			{$ifdef CrashBinaryShaders} 'Binary Shaders', {$endif}
			{$ifdef CrashDebug} 'Debug Output', {$endif}
			{$ifdef CrashCreateBuffers} 'CreateBuffers', {$endif}
			// {$ifdef CrashVABinding} 'ARB_vertex_attrib_binding', {$endif}
			''], ', ' + SeparatedList.LastSep + ' и ');
		if length(crashList) > 0 then
			Log(crashList + ' — ломаю для отладки', logWarning);
	{$endif}

		try
			loader.Load;
		except
			exit(no);
		end;
		result := yes;

		if not Check(VAOFns) then
		begin
		{$ifdef Debug} Log('VAO (Vertex Array Objects) не поддерживаются! Включаю эмуляцию.', logWarning); {$endif}
			NoVAO_Replace2ImplicitImmediate;
		end
	{$ifdef Debug}
		else
			Log('VAO (Vertex Array Objects): OK', logOK)
	{$endif};

	{$ifdef Debug}
		if Assigned(gl.DrawElementsInstanced) then
			Log('Инстансинг: OK', logOK)
		else
			Log('У тебя настолько отстойная видеокарта, что не может в инстансинг? Ок, я выключу его.', logWarning);
	{$endif}

	{$ifdef Debug} if {$endif}
		Check(UBOFns)
	{$ifdef Debug} then Log('UBO (Uniform Buffer Objects): OK', logOK) else Log('UBO (Uniform Buffer Objects): не поддерживаются ;_;', logWarning) {$endif};

	{$ifdef Debug} if {$endif}
		Check(DSAFns)
	{$ifdef Debug} then Log('Direct State Access (DSA): OK', logOK) else Log('Direct State Access (DSA): не поддерживается, ну и ладно', logWarning) {$endif};

	{$ifdef Debug} if {$endif}
		Check(BinaryShaderFns)
	{$ifdef Debug} then Log('Binary Shaders: OK', logOK) else Log('Binary Shaders: не поддерживаются. Why?..', logWarning) {$endif};

	(*{$ifdef Debug} if {$endif}
		Check(VABindingFns)
	{$ifdef Debug} then Log('ARB_vertex_attrib_binding: OK', logOK) else Log('ARB_vertex_attrib_binding: N/A', logWarning) {$endif};*)
	end;

	procedure UnloadOpenGL;
	begin
		loader.Unload;
	end;

	procedure Init;
	begin
		loader.Init('OpenGL(prefix = gl)', @DescribeOpenGLFunctions, @FindGLProc);
	end;

	procedure Done;
	begin
		loader.Done;
	end;

initialization
	&Unit('OpenGL').Initialize(@Init, @Done);
end.
