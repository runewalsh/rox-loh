unit Crunch;

{$include opts.inc}

interface

uses
	USystem, Streams, UClasses, UMath, DynamicLoader, U_GL, GLBase, Utils, DXT, DDS {$ifdef Debug}, ULog {$endif};

implementation

uses
	ctypes;

	function CrunchPath: string;
	begin
		result := Paths.MiscLibs + 'crunch.exe';
	end;

type
	crnd = class
	const
		cCRNFmtDXT1 = 0;
		cCRNFmtDXT5 = 2;
	type
		cbool = ByteBool;
		realloc_func = function(p: pointer; size: csize_t; pActual_size: pcsize_t; movable: cbool; pUser_data: pointer): pointer; cdecl;
		msize_func = function(p: pointer; pUser_data: pointer): csize_t; cdecl;
		unpack_context = record internal: pointer; end;
		texture_info = record
			struct_size: uint32;
			width, height: uint32;
			levels, faces: uint32;
			bytes_per_block: uint32;
			userdata0, userdata1: uint32;
			format: uint32;
		end;

	class var
		set_memory_callbacks: procedure(pRealloc: realloc_func; pMSize: msize_func; pUser_data: pointer); cdecl;
		get_fundamental_dxt_format: function(fmt: uint32): uint32; cdecl;
		get_texture_info: function(pData: pointer; data_size: uint32; var pTexture_info: texture_info): cbool; cdecl;
		unpack_begin: function(pData: pointer; data_size: uint32): unpack_context; cdecl;
		unpack_level: function(pContext: unpack_context; ppDst: pPointer; dst_size_in_bytes, row_pitch_in_bytes, level_index: uint32): cbool; cdecl;
		unpack_end: function(pContext: unpack_context): cbool; cdecl;

	class var
		loader: DLLoader;
	end;

	procedure DescribeCrunchFunctions(var fns: DLLoader.FunctionsList);
	begin
		fns
		.Func(@crnd.set_memory_callbacks,       'set_memory_callbacks')^
		.Func(@crnd.get_fundamental_dxt_format, 'get_fundamental_dxt_format')^
		.Func(@crnd.get_texture_info,           'get_texture_info')^
		.Func(@crnd.unpack_begin,               'unpack_begin')^
		.Func(@crnd.unpack_level,               'unpack_level')^
		.Func(@crnd.unpack_end,                 'unpack_end');
	end;

	function CrndRealloc(p: pointer; size: csize_t; pActual_size: pcsize_t; movable: crnd.cbool; pUser_data: pointer): pointer; cdecl;
	var
		sz: size_t;
	begin
		Assert(@pUser_data = @pUser_data);
		if p = nil then
		begin
			result := GetMem(size);
			sz := size;
		end else
			if size = 0 then
			begin
				FreeMem(p);
				sz := 0;
				result := nil;
			end else
				if movable then
				begin
					result := ReallocMem(p, size);
					sz := size;
				end else
				begin
					result := nil;
					sz := MemSize(p);
				end;

		if Assigned(pActual_size) then pActual_size^ := sz;
	end;

	function CrndMemSize(p: pointer; pUser_data: pointer): csize_t; cdecl;
	begin
		Assert(@pUser_data = @pUser_data);
		if Assigned(p) then result := MemSize(p) else result := 0;
	end;

	procedure Save(var im: TextureImage; s: pStream);

		function Extension(out opts: string): string;
		begin
			opts := '';
			case im.format of
				GLformat_RGB_DXT1, GLformat_RGBA_DXT1, GLformat_RGBA_DXT5: result := 'dds';
				else
				case GLImageFormatsInfo[im.format].nChannels of
					4: begin result := 'png'; opts := 'qs=fast'; end;
					3: result := 'bmp';
					else result := 'dds';
				end;
			end;
		end;

	var
		tmpName, crnOutput, opts: string;
		crn: Process;
		args: Strings;
		openFlags: Process.OpenFlags;
	begin
		&File.CreateTemp('to_crn.' + Extension(opts), tmpName, OnlyCreate);
		try
			im.Save(tmpName, opts);
			&File.CreateTemp('from_crn.crn', crnOutput, OnlyCreate);
			try
				args := nil;
				Append(args, ['/file', ToSystemFileName(tmpName)]);
				Append(args, ['/out', ToSystemFileName(crnOutput)]);

				case GLImageFormatsInfo[im.format].nChannels of
					3: if not (GLformat_Compressed in GLImageFormatsInfo[im.format].flags) then Append(args, '/DXT1');
					4: if not (GLformat_Compressed in GLImageFormatsInfo[im.format].flags) then Append(args, '/DXT5');
				end;
				Append(args, ['/mipMode', 'UseSource']);
				Append(args, ['/quality', '255']); // это «качество» в смысле «потери», так что 255 — минимум потерь, как и сжатия (в DXT и так потери)
				Append(args, ['/dxtQuality', 'uber']);

				openFlags := []; if not IsConsole then openFlags += [Silent];
				Process.Open(crn, CrunchPath, args, openFlags);
				try
					crn.Wait;
					if crn.ExitCode <> 0 then raise Error('Crunch вернул код ошибки {0}.', [crn.ExitCode]);
					s^.AppendFrom(GetStream(crnOutput));
				finally
					crn.Close;
				end;
			finally
				&File.Erase(crnOutput);
			end;
		finally
			&File.Erase(tmpName);
		end;
	end;

	procedure Load(var im: TextureImage; s: pStream; size: size_t);
	var
		sim: pStreamImage;
		ctx: crnd.unpack_context;
		info: crnd.texture_info;
		format: GLImageFormat;
		target: GLTextureTarget;
		flags: TextureImageFlags;
		level, i: uint;
		faces: array[0 .. 5] of pointer;
	begin
		crnd.loader.Load;

		try
			sim := s^.GetImage(s^.Position, size);
			try
				ctx := crnd.unpack_begin(sim^.Data, size);
				if not Assigned(ctx.internal) then raise Error('Ошибка crnd_unpack_begin.');
				info.struct_size := sizeof(info);
				if not crnd.get_texture_info(sim^.Data, size, info) then raise Error('Ошибка crnd_get_texture_info.');

				case crnd.get_fundamental_dxt_format(info.format) of
					crnd.cCRNFmtDXT1: format := GLformat_RGB_DXT1;
					crnd.cCRNFmtDXT5: format := GLformat_RGBA_DXT5;
					else raise Error('Неверный формат crunch ({0}).', ToString(info.format));
				end;
			{$ifdef Debug} LogR(ToString(info.width) + ' x ' + ToString(info.height) + ', уровней: ' + ToString(info.levels) + ', граней: ' + ToString(info.faces) + '; ', logDebug); {$endif}

				flags := [];
				if info.levels > 1 then Include(flags, texture_Mips);
				case info.faces of
					1: target := GLtexture_2D;
					6: target := GLtexture_Cube;
					else raise Error('Неверное число граней ({0}).', ToString(info.faces));
				end;

				im.Prepare(target, UintVec2.Make(info.width, info.height), format, flags);
				if im.nLevels <> info.levels * info.faces then
					raise Error('Число уровней не совпадает: основной {0}x{1}, вычислено = {2}, сообщено = {3} ур. x {4} гр.',
						ToString(info.width), ToString(info.height), ToString(im.nLevels), ToString(info.levels), ToString(info.faces));

				level := 0;
				repeat
					for i := 0 to info.faces - 1 do
						faces[i] := im.LevelPtr(level * TextureTargetsInfo[target].faces + i);
					if not crnd.unpack_level(ctx, pPointer(faces), High(uint32), 0, level) then
						raise Error('Не удалось прочитать уровень {0}.', ToString(level));

					if (level = 0) and (im.format = GLformat_RGB_DXT1) and
						DetectDXT1A(im.LevelPtr(level), im.info.LevelSizeXY(level)) then
					begin
						im.info.format := GLformat_RGBA_DXT1;
					{$ifdef Debug} LogR('(RGBA); ', logDebug); {$endif}
					end;

					level += info.faces;
				until level = info.levels;
				crnd.unpack_end(ctx);
				if s^.CanSeek then s^.Position := s^.Position + size;
			finally
				Release(sim);
			end;
		finally
			crnd.loader.Unload;
		end;
	end;

	procedure AfterLoad;
	begin
		crnd.set_memory_callbacks(@CrndRealloc, @CrndMemSize, nil);
	end;

	procedure SuiteSave(obj: pointer; s: pStream); begin Save(pTextureImage(obj)^, s); end;
	procedure SuiteLoad(obj: pointer; s: pStream; const fsize: FileSize); begin Load(pTextureImage(obj)^, s, fsize.AsSizeT); end;
	procedure Init;
	begin
		crnd.loader.Init('crunch(prefix = crnd_, perm, lock)', @DescribeCrunchFunctions);
		crnd.loader.Hook(+0).AfterLoad(@AfterLoad);
		TextureImage.Loaders.Register('crn', @SuiteLoad, @SuiteSave);
	end;

	procedure Done;
	begin
		crnd.loader.Done;
	end;

initialization
	&Unit('Crunch').Initialize(@Init, @Done);
end.
