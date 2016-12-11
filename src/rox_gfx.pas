{$include opts.inc}
unit rox_gfx;

interface

uses
	USystem, UMath, UClasses, Utils, Streams, GLBase, GLUtils, U_GL,  {$ifdef Debug} ULog, {$endif} rox_mm, rox_gl;

const
	GLResourceTag = 'GL';

type
	pTexture = ^Texture;
	Texture = object(&Object)
		handle: gl.uint;
		size: UintVec2;
		ap: AspectPair;

		function Load(const id: string): pTexture; static;
		destructor Done; virtual;
	private
		function Load(s: pStream): pTexture; static;
	end;

	procedure DrawQuad(tex: pTexture; const pos, size, texPos, texSize: Vec2; const z: float);

	procedure InitGL;
	procedure DoneGL;
	procedure CleanupGLGraveyard;
	procedure DeactivateGLGraveyard;

implementation

uses
	rox_ui;

type
	GLGraveyard = object
	type
		ItemKind = (DeadTexture);
	var
		lock: ThreadLock;
		list: array of record
			kind: ItemKind;
			handle: gl.uint;
		end;

		procedure Init;
		procedure Done;
		procedure Add(kind: ItemKind; handle: gl.uint);
		procedure Cleanup;
		procedure DisposeOf(kind: ItemKind; handle: gl.uint);
	end;

	procedure GLGraveyard.Init;
	begin
		lock.Init;
		Assert(length(list) = 0);
	end;

	procedure GLGraveyard.Done;
	begin
		Assert(length(list) = 0, 'Нужно вызвать ResourcePool.Deactivate(GLResourceTag) & CleanupGraveyard напоследок.');
		lock.Done;
	end;

	procedure GLGraveyard.Add(kind: ItemKind; handle: gl.uint);
	begin
		if window.GLContext = 0 then
			// :(
		else
		if (handle = 0) or (Thread.Current = window.GLContextOwner) then
			DisposeOf(kind, handle)
		else
		begin
			lock.Enter;
			try
				SetLength(list, length(list) + 1);
				list[High(list)].kind := kind;
				list[High(list)].handle := handle;
			finally
				lock.Leave;
			end;
		end;
	end;

	procedure GLGraveyard.Cleanup;
	var
		i: sint;
	begin
		Assert((window.GLContext = 0) or (Thread.Current = window.GLContextOwner), Format(
			'Graveyard.Cleanup: поток не владеет GL-контекстом, {0} <-> {1}. Это нужно делать до уничтожения окна.',
			[Thread.Current, window.GLContextOwner]));

		lock.Enter;
		for i := 0 to High(list) do
			DisposeOf(list[i].kind, list[i].handle);
		list := nil;
		lock.Leave;
	end;

	procedure GLGraveyard.DisposeOf(kind: ItemKind; handle: gl.uint);
	begin
		if handle = 0 then exit;
		case kind of
			DeadTexture: gl.DeleteTextures(1, @handle);
			else Assert(no);
		end;
	end;

var
	rp: pResourcePool;
	glTrash: GLGraveyard;

	function Texture.Load(const id: string): pTexture;
	begin
		result := rp^.LoadRef(TypeOf(Texture), Paths.Data + id);
	end;

	destructor Texture.Done;
	begin
		glTrash.Add(DeadTexture, handle);
		inherited Done;
	end;

	function Texture.Load(s: pStream): pTexture;
	var
		im: TextureImage;
		components, intFormat, ctype: gl.enum;
		h: gl.uint;
	begin
		im.Init(s);
		result := nil;
		try
			try
				result := new(pTexture, Init);
				result^.handle := 0;

				case im.format of
					GLformat_RGB:
						begin
							components := gl.RGB;
							intFormat := gl.RGB8;
							ctype := gl.UNSIGNED_BYTE;
						end;
					GLformat_RGBA:
						begin
							components := gl.RGBA;
							intFormat := gl.RGBA8;
							ctype := gl.UNSIGNED_BYTE;
						end;
					else
						raise Error('{0}: формат {1} не поддерживается.', s^.path, GLImageFormatIds[im.format]);
				end;

				gl.GenTextures(1, @h);
				gl.BindTexture(gl.TEXTURE_2D, h);
				gl.TexImage2D(gl.TEXTURE_2D, 0, intFormat, im.size.x, im.size.y, 0, components, ctype, im.FirstLevel);
				gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
				gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
				if GLImageFormatsInfo[im.format].nChannels = 4 then
				begin
					gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
					gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
				end else
				begin
					gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.&REPEAT);
					gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.&REPEAT);
				end;
				gl.L.TexEnvi(gl.L.TEXTURE_ENV, gl.L.TEXTURE_ENV_MODE, gl.L.MODULATE);
			finally
				im.Done;
			end;
		except
			if Assigned(result) then dispose(result, Done);
			raise;
		end;

		result^.handle := h;
		result^.size := im.Size.XY;
		result^.ap := AspectPair.Make(result^.size);
	end;

	procedure DrawQuad(tex: pTexture; const pos, size, texPos, texSize: Vec2; const z: float);
	var
		data: array[0 .. 19] of gl.float;
	begin
		data[0] := pos.x;
		data[1] := pos.y;
		data[2] := z;
		data[3] := pos.x;
		data[4] := pos.y + size.y;
		data[5] := z;
		data[6] := pos.x + size.x;
		data[7] := pos.y;
		data[8] := z;
		data[9] := pos.x + size.x;
		data[10] := pos.y + size.y;
		data[11] := z;
		data[12] := texPos.x;
		data[13] := texPos.y + texSize.y;
		data[14] := texPos.x;
		data[15] := texPos.y;
		data[16] := texPos.x + texSize.x;
		data[17] := texPos.y + texSize.y;
		data[18] := texPos.x + texSize.x;
		data[19] := texPos.y;

		gl.BindTexture(gl.TEXTURE_2D, tex^.handle);
		gl.L.EnableClientState(gl.L.VERTEX_ARRAY);
		gl.L.EnableClientState(gl.L.TEXTURE_COORD_ARRAY);
		gl.L.VertexPointer(3, gl.FLOAT_TYPE, 0, @data[0]);
		gl.L.TexCoordPointer(2, gl.FLOAT_TYPE, 0, @data[12]);
		gl.DrawArrays(gl.TRIANGLE_STRIP, 0, 4);
	end;

	procedure InitGL;
	begin
		ui.Init;
	end;

	procedure DoneGL;
	begin
		ui.Done;
	end;

	procedure CleanupGLGraveyard;
	begin
		glTrash.Cleanup;
	end;

	procedure DeactivateGLGraveyard;
	begin
		ResourcePool.Shared^.Deactivate(GLResourceTag);
		CleanupGLGraveyard;
	end;

	function LoadTexture(s: pStream): pObject;
	begin
		result := Texture.Load(s);
	end;

	procedure Init;
	begin
		rp := ResourcePool.Shared;
		rp^.Register(TypeOf(Texture), @LoadTexture)^.Tag(GLResourceTag);
		glTrash.Init;
	end;

	procedure Done;
	begin
		glTrash.Done;
	end;

initialization
	&Unit('rox_gfx').Initialize(@Init, @Done);
end.
