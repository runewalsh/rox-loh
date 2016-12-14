{$include opts.inc}
unit rox_gfx;

interface

uses
	USystem, UMath, UClasses, Utils, Streams, GLBase, GLUtils, U_GL, OpenGL_Impl,  {$ifdef Debug} ULog, {$endif} rox_gl;

const
	GLResourceTag = 'GL';

type
	pTexture = ^Texture;
	Texture = object(&Object)
		handle: gl.uint;
		size: UintVec2;
		sizeZ: uint;
		ap: AspectPair;
		targetEnum: gl.enum;

		function Load(const id: string): pTexture; static;
		destructor Done; virtual;
	private
		function Load(s: pStream): pTexture; static;
	end;

	pImageResource = ^ImageResource;
	ImageResource = object(&Object)
		im: TextureImage;
		constructor Init(s: pStream);
		destructor Done; virtual;
	end;

type
	Quad = object
	type
	scoped_enum_ Field = (Z, TexZ, Color, Transform); _end
	var
		fields: set of Field;
		z: float;
		texZ: float;
		color: Vec4;
		transform: Transform2;

		procedure DrawPlain(tex: pTexture; const pos, size, texPos, texSize: Vec2); static;
		procedure Draw(tex: pTexture; const pos, size, texPos, texSize: Vec2);
	end;

	procedure InitGL(win: pointer {pWindow});
	procedure DoneGL;
	procedure CleanupGLGraveyard;

implementation

uses
	rox_win;

var
	window: pWindow;

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
		Assert(Assigned(window), 'Ресурс, зависимый от GL-контекста, пережил уничтожение окна.');

		if (handle = 0) or (Thread.Current = window^.GLContextOwner) then
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
		Assert(Thread.Current = window^.GLContextOwner, Format(
			'Graveyard.Cleanup: поток не владеет GL-контекстом, {0} <-> {1}. Это нужно делать до уничтожения окна.',
			[Thread.Current, window^.GLContextOwner]));

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
	glTrash: GLGraveyard;

	function Texture.Load(const id: string): pTexture;
	begin
		result := ResourcePool.Shared^.LoadRef(TypeOf(Texture), Paths.Data + id);
	end;

	destructor Texture.Done;
	begin
		glTrash.Add(DeadTexture, handle);
		inherited Done;
	end;

	function Texture.Load(s: pStream): pTexture;
		function UnsupportedTarget(target: GLTextureTarget): Exception;
		begin
			result := Error(StreamPath.Human(s^.path) + ': ' + GLTextureTargetIds[target] + '-текстуры не поддерживаются.');
		end;
	var
		im: TextureImage;
		h: gl.uint;
		lv: uint;
		mipped: boolean;
		levelSize: UintVec3;
	begin
		im.Init(s);
		result := nil;
		try
			try
				result := new(pTexture, Init);
				gl.GenTextures(1, @h);
				result^.handle := h;
				case im.target of
					GLtexture_2D: begin result^.targetEnum := gl.TEXTURE_2D; result^.sizeZ := 1; end;
					GLtexture_3D: begin result^.targetEnum := gl.TEXTURE_3D; result^.sizeZ := im.size.Z; end;
					else raise UnsupportedTarget(im.target);
				end;

				gl.BindTexture(result^.targetEnum, h);
				mipped := texture_Mips in im.info.flags;
				for lv := 0 to im.nLevels - 1 do
				begin
					levelSize := im.info.LevelSize(lv);
					case im.target of
						GLtexture_2D:
							if GLformat_Compressed in GLImageFormatsInfo[im.format].flags then
								gl.CompressedTexImage2D(gl.TEXTURE_2D, im.info.Defaced(lv), GLFormats[im.format].internalFormat, levelSize.x, levelSize.y, 0,
									im.info.GetLevelDataSize(lv), im.LevelPtr(lv))
							else
								gl.TexImage2D(gl.TEXTURE_2D, im.info.Defaced(lv), GLFormats[im.format].internalFormat, levelSize.x, levelSize.y, 0,
									GLFormats[im.format].components, GLFormats[im.format].ctype, im.LevelPtr(lv));
						GLtexture_3D:
							if GLformat_Compressed in GLImageFormatsInfo[im.format].flags then
								gl.CompressedTexImage3D(gl.TEXTURE_3D, im.info.Defaced(lv), GLFormats[im.format].internalFormat, levelSize.x, levelSize.y, levelSize.z, 0,
									im.info.GetLevelDataSize(lv), im.LevelPtr(lv))
							else
								gl.TexImage3D(gl.TEXTURE_3D, im.info.Defaced(lv), GLFormats[im.format].internalFormat, levelSize.x, levelSize.y, levelSize.z, 0,
									GLFormats[im.format].components, GLFormats[im.format].ctype, im.LevelPtr(lv));
						else raise UnsupportedTarget(im.target);
					end;
				end;

				gl.TexParameteri(result^.targetEnum, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
				if mipped then
					gl.TexParameteri(result^.targetEnum, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR)
				else
					gl.TexParameteri(result^.targetEnum, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
				if ((GLImageFormatsInfo[im.format].nChannels = 4) or (Pos('[c]', s^.path) > 0)) and not (Pos('[tile]', s^.path) > 0) then
				begin
					gl.TexParameteri(result^.targetEnum, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
					gl.TexParameteri(result^.targetEnum, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
					if im.target = GLtexture_3D then gl.TexParameteri(result^.targetEnum, gl.TEXTURE_WRAP_R, gl.CLAMP_TO_EDGE);
				end else
				begin
					gl.TexParameteri(result^.targetEnum, gl.TEXTURE_WRAP_S, gl.&REPEAT);
					gl.TexParameteri(result^.targetEnum, gl.TEXTURE_WRAP_T, gl.&REPEAT);
					if im.target = GLtexture_3D then gl.TexParameteri(result^.targetEnum, gl.TEXTURE_WRAP_R, gl.&REPEAT);
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

	constructor ImageResource.Init(s: pStream);
	begin
		inherited Init;
		im.Init(s);
	end;

	destructor ImageResource.Done;
	begin
		im.Done;
		inherited Done;
	end;

	procedure Quad.DrawPlain(tex: pTexture; const pos, size, texPos, texSize: Vec2);
	var
		q: Quad;
	begin
		q.fields := [];
		q.Draw(tex, pos, size, texPos, texSize);
	end;

	procedure Quad.Draw(tex: pTexture; const pos, size, texPos, texSize: Vec2);
	var
		data: array[0 .. 3*4 + 3*4] of gl.float;
		vp, n: uint;
		t, a, b, c, d: Vec2;
	begin
		a := Vec2.Make(pos.x, pos.y + size.y);
		b := pos;
		c := pos + size;
		d := Vec2.Make(pos.x + size.x, pos.y);

		if Field.Transform in fields then
		begin
			a := transform * a;
			b := transform * b;
			c := transform * c;
			d := transform * d;
		end;

		t := window^.state.invp;
		a *= t; b *= t; c *= t; d *= t;

		vp := 0;
		if Field.Z in fields then
		begin
			n := 3;
			data[0] := a.x;
			data[1] := a.y;
			data[2] := z;
			data[3] := b.x;
			data[4] := b.y;
			data[5] := z;
			data[6] := c.x;;
			data[7] := c.y;
			data[8] := z;
			data[9] := d.x;;
			data[10] := d.y;
			data[11] := z;
		end else
		begin
			n := 2;
			data[0] := a.x;
			data[1] := a.y;
			data[2] := b.x;
			data[3] := b.y;
			data[4] := c.x;;
			data[5] := c.y;
			data[6] := d.x;;
			data[7] := d.y;
		end;

		gl.L.VertexPointer(n, gl.FLOAT_TYPE, 0, gl.pfloat(data) + vp);
		vp += 4*n;

		if Field.TexZ in fields then
		begin
			n := 3;
			data[vp+0] := texPos.x;
			data[vp+1] := texPos.y;
			data[vp+2] := texZ;
			data[vp+3] := texPos.x;
			data[vp+4] := texPos.y + texSize.y;
			data[vp+5] := texZ;
			data[vp+6] := texPos.x + texSize.x;
			data[vp+7] := texPos.y;
			data[vp+8] := texZ;
			data[vp+9] := texPos.x + texSize.x;
			data[vp+10] := texPos.y + texSize.y;
			data[vp+11] := texZ;
		end else
		begin
			n := 2;
			data[vp+0] := texPos.x;
			data[vp+1] := texPos.y;
			data[vp+2] := texPos.x;
			data[vp+3] := texPos.y + texSize.y;
			data[vp+4] := texPos.x + texSize.x;
			data[vp+5] := texPos.y;
			data[vp+6] := texPos.x + texSize.x;
			data[vp+7] := texPos.y + texSize.y;
		end;
		gl.L.TexCoordPointer(n, gl.FLOAT_TYPE, 0, gl.pfloat(data) + vp);

		if tex^.targetEnum <> gl.TEXTURE_2D then
		begin
			gl.Disable(gl.TEXTURE_2D);
			gl.Enable(tex^.targetEnum);
		end;

		gl.BindTexture(tex^.targetEnum, tex^.handle);
		if Field.Color in fields then gl.L.Color4f(color.x, color.y, color.z, color.w);
		gl.DrawArrays(gl.TRIANGLE_STRIP, 0, 4);
		if Field.Color in fields then gl.L.Color4f(1, 1, 1, 1);

		if tex^.targetEnum <> gl.TEXTURE_2D then
		begin
			gl.Disable(tex^.targetEnum);
			gl.Enable(gl.TEXTURE_2D);
		end;
	end;

	function LoadTexture(s: pStream): pObject; begin result := Texture.Load(s); end;
	function LoadImageResource(s: pStream): pObject; begin result := new(pImageResource, Init(s)); end;

	procedure InitGL(win: pointer {pWindow});
	begin
		if Assigned(window) then raise Error('InitGL уже вызвана.');
		pWindow(win)^.Verify;
		window := win;

		ResourcePool.Shared^
		.Register(TypeOf(Texture), @LoadTexture)^.Tag(GLResourceTag)^
		.Register(TypeOf(ImageResource), @LoadImageResource);

		glTrash.Init;
	end;

	procedure DoneGL;
	begin
		if not Assigned(window) then raise Error('InitGL не вызвана.');
		ResourcePool.Shared^.Deactivate(GLResourceTag);
		CleanupGLGraveyard;
		glTrash.Done;
		window := nil;
	end;

	procedure CleanupGLGraveyard;
	begin
		glTrash.Cleanup;
	end;

end.
