{$include opts.inc}
unit rox_gl;

interface

uses
	Windows, USystem, Utils, DynamicLoader;

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
	class var
		GetError: function: enum; stdcall;
		Enable: procedure(cap: enum); stdcall;
		Disable: procedure(cap: enum); stdcall;

		PixelStorei: procedure(pname: enum; param: int); stdcall;
		GenTextures: procedure(n: sizei; textures: puint); stdcall;
		DeleteTextures: procedure(n: sizei; textures: puint); stdcall;
		BindTexture: procedure(target: enum; texture: uint); stdcall;
		TexImage2D: procedure(target: enum; level: int; internalformat: int; width: sizei; height: sizei; border: int; format: enum; &type: enum; data: pointer); stdcall;
		TexImage3D: procedure(target: enum; level: int; internalformat: int; width: sizei; height: sizei; depth: sizei; border: int; format: enum; type_: enum; pixels: pointer); stdcall;
		CompressedTexImage2D: procedure(target: enum; level: int; internalformat: enum; width: sizei; height: sizei; border: int; imageSize: sizei; data: pointer); stdcall;
		CompressedTexImage3D: procedure(target: enum; level: int; internalformat: enum; width: sizei; height: sizei; depth: sizei; border: int; imageSize: sizei; data: pointer); stdcall;
		TexSubImage2D: procedure(target: enum; level: int; xoffset, yoffset: int; width, height: sizei; format, type_: enum; pixels: pointer); stdcall;
		TexParameteri: procedure(target, pname: enum; param: int); stdcall;
		TexParameteriv: procedure(target, pname: enum; param: pInt); stdcall;
		TexParameterf: procedure(target, pname: enum; param: float); stdcall;

		Viewport: procedure(x: int; y: int; width: sizei; height: sizei); stdcall;
		ClearColor: procedure(red: clampf; green: clampf; blue: clampf; alpha: clampf); stdcall;
		Clear: procedure(mask: bitfield); stdcall;
		BlendFunc: procedure(sfactor, dfactor: enum); stdcall;
		DepthFunc: procedure(func: enum); stdcall;
		DepthMask: procedure(flag: boolean); stdcall;

		DrawArrays: procedure(mode: enum; first: int; count: sizei); stdcall;
		DrawElements: procedure(mode: enum; count: sizei; type_: enum; indices: pointer); stdcall;
		GenBuffers: procedure(n: sizei; buffers: pUint); stdcall;
		DeleteBuffers: procedure(n: sizei; buffers: pUint); stdcall;
		BindBuffer: procedure(target: enum; buffer: uint); stdcall;
		BufferData: procedure(target: enum; size: sizeiptr; data: pointer; usage: enum); stdcall;
		BufferSubData: procedure(target: enum; offset: intptr; size: sizeiptr; data: pointer); stdcall;

	const
		TRUE = 1;
		FALSE = 0;

		NO_ERROR          = 0;
		INVALID_ENUM      = $0500;
		INVALID_VALUE     = $0501;
		INVALID_OPERATION = $0502;
		OUT_OF_MEMORY     = $0505;

		DEPTH_BUFFER_BIT = $00000100;
		COLOR_BUFFER_BIT = $00004000;
		DEPTH_TEST = $0B71;

		UNPACK_ALIGNMENT = $0CF5;
		PACK_ALIGNMENT   = $0D05;

		BLEND = $0BE2;
		ZERO = 0;
		ONE  = 1;
		SRC_ALPHA = $0302;
		ONE_MINUS_SRC_ALPHA = $0303;

		LESS = $0201;
		LEQUAL = $0203;

		UNSIGNED_BYTE  = $1401;
		SIGNED_SHORT   = $1402;
		UNSIGNED_SHORT = $1403;
		SIGNED_INT     = $1404;
		UNSIGNED_INT   = $1405;
		FLOAT_TYPE     = $1406;
		DOUBLE_TYPE    = $140A;

		TEXTURE0 = $84C0;
		TEXTURE_2D = $0DE1;
		TEXTURE_3D = $806F;
		TEXTURE_MAG_FILTER = $2800;
		TEXTURE_MIN_FILTER = $2801;
		TEXTURE_WRAP_S     = $2802;
		TEXTURE_WRAP_T     = $2803;
		TEXTURE_WRAP_R     = $8072;
		NEAREST = $2600;
		LINEAR = $2601;
		LINEAR_MIPMAP_LINEAR = $2703;
		&REPEAT = $2901;
		CLAMP_TO_EDGE = $812F;
		TEXTURE_LOD_BIAS = $8501;

		RGB            = $1907;
		RGBA           = $1908;
		RGB8           = $8051;
		RGBA8          = $8058;

		TRIANGLES      = $0004;
		TRIANGLE_STRIP = $0005;

		STATIC_DRAW          = $88E4;
		DYNAMIC_DRAW         = $88E8;
		ARRAY_BUFFER         = $8892;
		ELEMENT_ARRAY_BUFFER = $8893;

	type
		L = class
		const
			VERTEX_ARRAY        = $8074;
			TEXTURE_COORD_ARRAY = $8078;
			COLOR_ARRAY         = $8076;

			MODULATE            = $2100;
			DECAL               = $2101;
			REPLACE             = $1E01;
			TEXTURE_ENV_MODE    = $2200;
			TEXTURE_ENV         = $2300;
		class var
			EnableClientState: procedure(cap: enum); stdcall;
			DisableClientState: procedure(cap: enum); stdcall;
			VertexPointer: procedure(size: int; &type: enum; stride: sizei; data: pointer); stdcall;
			TexCoordPointer: procedure(size: int; &type: enum; stride: sizei; data: pointer); stdcall;
			ColorPointer: procedure(size: int; &type: enum; stride: sizei; data: pointer); stdcall;
			TexEnvi: procedure(target, pname: enum; param: int); stdcall;
			Color4f: procedure(r, g, b, a: float); stdcall;
		end;

		DXT = class
		const
			RGB_S3TC_DXT1  = $83F0;
			RGBA_S3TC_DXT1 = $83F1;
			RGBA_S3TC_DXT5 = $83F3;
		end;

		class function InlineErrorDescription(err: enum): string;

		class procedure Load;
		class procedure Unload;
	private class var
		loader: DLLoader;
		oglLib: DynamicLibrary;
	end;

implementation

	class function gl.InlineErrorDescription(err: enum): string;
	begin
		case err of
			INVALID_ENUM     : result := 'об ошибке INVALID_ENUM';
			INVALID_VALUE    : result := 'об ошибке INVALID_VALUE';
			INVALID_OPERATION: result := 'об ошибке INVALID_OPERATION';
			OUT_OF_MEMORY    : result := 'о нехватке памяти';
			else               result := Format('об ошибке с неизвестным кодом ({0})', [err]);
		end;
	end;

	class procedure gl.Load;
	begin
		DynamicLibrary.Open(oglLib, 'opengl32');
		try
			loader.Load;
		except
			oglLib.Close;
			raise;
		end;
	end;

	class procedure gl.Unload;
	begin
		loader.Unload;
		oglLib.Close;
	end;

	function FindGLProc(const name: string): pointer;
	begin
		result := gl.oglLib.FindProc(name);
		if not Assigned(result) then result := wglGetProcAddress(pChar(name));
	end;

	procedure DescribeOpenGLFunctions(var fns: DLLoader.FunctionsList);
	begin
		fns
		.Func(@gl.GetError, 'GetError')^
		.Func(@gl.Enable, 'Enable')^
		.Func(@gl.Disable, 'Disable')^

		.Func(@gl.PixelStorei, 'PixelStorei')^
		.Func(@gl.GenTextures, 'GenTextures')^
		.Func(@gl.DeleteTextures, 'DeleteTextures')^
		.Func(@gl.BindTexture, 'BindTexture')^
		.Func(@gl.TexImage2D, 'TexImage2D')^
		.Func(@gl.TexImage3D, 'TexImage3D')^
		.Func(@gl.CompressedTexImage2D, 'CompressedTexImage2D')^
		.Func(@gl.CompressedTexImage3D, 'CompressedTexImage3D')^
		.Func(@gl.TexSubImage2D, 'TexSubImage2D')^
		.Func(@gl.TexParameteri, 'TexParameteri')^
		.Func(@gl.TexParameteriv, 'TexParameteriv')^
		.Func(@gl.TexParameterf, 'TexParameterf')^

		.Func(@gl.Viewport, 'Viewport')^
		.Func(@gl.ClearColor, 'ClearColor')^
		.Func(@gl.Clear, 'Clear')^
		.Func(@gl.BlendFunc, 'BlendFunc')^
		.Func(@gl.DepthFunc, 'DepthFunc')^
		.Func(@gl.DepthMask, 'DepthMask')^

		.Func(@gl.DrawArrays, 'DrawArrays')^
		.Func(@gl.DrawElements, 'DrawElements')^
		.Func(@gl.GenBuffers, 'GenBuffers')^
		.Func(@gl.DeleteBuffers, 'DeleteBuffers')^
		.Func(@gl.BindBuffer, 'BindBuffer')^
		.Func(@gl.BufferData, 'BufferData')^
		.Func(@gl.BufferSubData, 'BufferSubData')^

		.Func(@gl.L.EnableClientState, 'EnableClientState')^
		.Func(@gl.L.DisableClientState, 'DisableClientState')^
		.Func(@gl.L.VertexPointer, 'VertexPointer')^
		.Func(@gl.L.TexCoordPointer, 'TexCoordPointer')^
		.Func(@gl.L.ColorPointer, 'ColorPointer')^
		.Func(@gl.L.TexEnvi, 'TexEnvi')^
		.Func(@gl.L.Color4f, 'Color4f');
	end;

	procedure Init;
	begin
		gl.loader.Init('OpenGL(prefix = gl)', @DescribeOpenGLFunctions, @FindGLProc);
		gl.oglLib := DynamicLibrary.Invalid;
	end;

	procedure Done;
	begin
		gl.oglLib.Close;
		gl.loader.Done;
	end;

initialization
	&Unit('rox_gl').Initialize(@Init, @Done);
end.

