unit DDS;

{$include opts.inc}

interface

uses
	USystem, Streams, UClasses, UMath, U_GL, GLBase, Algo, Utils, DXT {$ifdef Debug}, ULog {$endif};

	function DumpHeader(s: pStream): string;
	procedure Save(var im: TextureImage; s: pStream);

implementation

const
	HEADER_MAGIC     = $20534444; // 'DDS '

	// DDS_HEADER dwFlags
	DDSD_CAPS         = $1; // Required in every .dds file.
	DDSD_HEIGHT       = $2; // Required in every .dds file.
	DDSD_WIDTH        = $4; // Required in every .dds file.
	DDSD_PITCH        = $8; // Required when pitch is provided for an uncompressed texture.
	DDSD_PIXELFORMAT  = $1000; // Required in every .dds file.
	DDSD_MIPMAPCOUNT  = $20000; // Required in a mipmapped texture.
	DDSD_LINEARSIZE   = $80000; // Required when pitch is provided for a compressed texture.
	DDSD_DEPTH        = $800000; // Required in a depth texture.
	_DDSD_REQUIRED    = DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or DDSD_PIXELFORMAT;

	// DDS_HEADER dwCaps
	DDSCAPS_COMPLEX   = $8; // Optional; must be used on any file that contains more than one surface (a mipmap, a cubic environment map, or mipmapped volume texture).
	DDSCAPS_MIPMAP    = $400000; // Optional; should be used for a mipmap.
	DDSCAPS_TEXTURE   = $1000; // Required.
	_DDSCAPS_REQUIRED = DDSCAPS_TEXTURE;

	// dwCaps2
	DDSCAPS2_CUBEMAP  = $200; // Required for a cube map.
	DDSCAPS2_CUBEMAP_POSITIVEX = $400; // Required when these surfaces are stored in a cube map.
	DDSCAPS2_CUBEMAP_NEGATIVEX = $800;
	DDSCAPS2_CUBEMAP_POSITIVEY = $1000;
	DDSCAPS2_CUBEMAP_NEGATIVEY = $2000;
	DDSCAPS2_CUBEMAP_POSITIVEZ = $4000;
	DDSCAPS2_CUBEMAP_NEGATIVEZ = $8000;
	DDSCAPS2_VOLUME            = $200000; // Required for a volume texture.

	// DDS_PIXELFORMAT dwFlags
	DDPF_ALPHAPIXELS = $1; // Texture contains alpha data; dwRGBAlphaBitMask contains valid data.
	// DDPF_ALPHA    = $2; // Used in some older DDS files for alpha channel only uncompressed data (dwRGBBitCount contains the alpha channel bitcount; dwABitMask contains valid data)
	DDPF_FOURCC      = $4; // Texture contains compressed RGB data; dwFourCC contains valid data.
	DDPF_RGB         = $40; // Texture contains uncompressed RGB data; dwRGBBitCount and the RGB masks (dwRBitMask, dwGBitMask, dwBBitMask) contain valid data.
	// DDPF_YUV      = $200; // Used in some older DDS files for YUV uncompressed data (dwRGBBitCount contains the YUV bit count; dwRBitMask contains the Y mask, dwGBitMask contains the U mask, dwBBitMask contains the V mask)
	DDPF_LUMINANCE   = $20000; // Used in some older DDS files for single channel color uncompressed data (dwRGBBitCount contains the luminance channel bit count; dwRBitMask contains the channel mask). Can be combined with DDPF_ALPHAPIXELS for a two channel DDS file.

type
	DDS_PIXELFORMAT = packed record
		dwSize: uint32;
		dwFlags: uint32;
		dwFourCC: packed array[0 .. 3] of char; // uint32
		dwRGBBitCount: uint32;
		dwRBitMask, dwGBitMask, dwBBitMask, dwABitMask: uint32;
	end; {$if sizeof(DDS_PIXELFORMAT) <> 32} {$error wrong DDS_PIXELFORMAT size, should be 32} {$endif}

	DDS_HEADER = packed record
		dwSize: uint32;
		dwFlags: uint32;
		dwHeight, dwWidth: uint32;
		dwPitchOrLinearSize: uint32;
		dwDepth: uint32;
		dwMipMapCount: uint32;
		dwReserved1: packed array[0 .. 10] of uint32;
		ddspf: DDS_PIXELFORMAT;
		dwCaps, dwCaps2, dwCaps3, dwCaps4: uint32;
		dwReserved2: uint32;
	end; {$if sizeof(DDS_HEADER) <> 124} {$error wrong DDS_HEADER size, should be 124} {$endif}

type
	pKnownPlainFormat = ^KnownPlainFormat;
	KnownPlainFormat = record
		rmask, gmask, bmask, amask: uint32;
		format: GLImageFormat;
	end;

const
	KnownPlain: array[0 .. 8] of KnownPlainFormat =
	(
		(rmask: $ff0000; gmask: $00ff00; bmask: $0000ff; amask: 0; format: GLformat_BGR),
		(rmask: $ff0000; gmask: $00ff00; bmask: $0000ff; amask: $ff000000; format: GLformat_BGRA),
		(rmask: $f00; gmask: $0f0; bmask: $00f; amask: 0; format: GLformat_RGB4),
		(rmask: $f00; gmask: $0f0; bmask: $00f; amask: $f000; format: GLformat_RGBA4),
		(rmask: $F800; gmask: $7E0; bmask: $1F; amask: 0; format: GLformat_RGB565),
		(rmask: $7C00; gmask: $3E0; bmask: $1F; amask: 0; format: GLformat_RGB5),
		(rmask: $7C00; gmask: $3E0; bmask: $1F; amask: $8000; format: GLformat_A1RGB5),
		(rmask: $ff; gmask: 0; bmask: 0; amask: $ff00; format: GLformat_RG),
		(rmask: $ff; gmask: 0; bmask: 0; amask: 0; format: GLformat_R)
	);

	function FindPlain(format: GLImageFormat): pKnownPlainFormat;
	var
		i: sint;
	begin
		for i := 0 to High(KnownPlain) do
			if KnownPlain[i].format = format then
				exit(@KnownPlain[i]);
		result := nil;
	end;

	procedure Save(var im: TextureImage; s: pStream);
		procedure SetFourCC(var f: DDS_PIXELFORMAT; const FourCC: string);
		begin
			f.dwFlags  := f.dwFlags or DDPF_FOURCC;
			f.dwFourCC := FourCC;
		end;
	var
		h: DDS_HEADER;
		refFormat: GLImageFormat;
		plain: pKnownPlainFormat;
		data, tmp: pointer;
		i, lv, p: sint;
	begin
		Zero(@h, sizeof(h));
		h.dwSize  := sizeof(h);
		h.dwFlags := _DDSD_REQUIRED or DDSD_LINEARSIZE;
		h.dwHeight := im.Size.Y;
		h.dwWidth  := im.Size.X;
		h.dwPitchOrLinearSize := im.info.GetLevelDataSize(0);
		if im.target = GLtexture_3D then
		begin
			h.dwFlags := h.dwFlags or DDSD_DEPTH;
			h.dwDepth := im.Size.Z;
		end;
		if texture_Mips in im.info.flags then h.dwMipMapCount := im.info.Defaced(im.nLevels);

		h.ddspf.dwSize := sizeof(h.ddspf);
		case im.format of
			GLformat_RGB: refFormat := GLformat_BGR;
			GLformat_RGBA: refFormat := GLformat_BGRA;
			else refFormat := im.format;
		end;
		case refFormat of
			GLformat_RGB_DXT1, GLformat_RGBA_DXT1: SetFourCC(h.ddspf, 'DXT1');
			GLformat_RGBA_DXT5: SetFourCC(h.ddspf, 'DXT5');
			else
			begin
				plain := FindPlain(refFormat);
				if not Assigned(plain) then raise Error('Сохранение ' + GLImageFormatIds[im.format] + ' в DDS не поддерживается.');
				if (plain^.rmask > 0) and (plain^.gmask = 0) and (plain^.bmask = 0) then h.ddspf.dwFlags := h.ddspf.dwFlags or DDPF_LUMINANCE;
				if plain^.amask > 0 then h.ddspf.dwFlags := h.ddspf.dwFlags or DDPF_ALPHAPIXELS;
				if (plain^.gmask > 0) or (plain^.bmask > 0) then h.ddspf.dwFlags := h.ddspf.dwFlags or DDPF_RGB;

				h.ddspf.dwRGBBitCount := bitsizeof(byte) * GLImageFormatsInfo[refFormat].pixelSize;
				h.ddspf.dwRBitMask    := plain^.rmask;
				h.ddspf.dwGBitMask    := plain^.gmask;
				h.ddspf.dwBBitMask    := plain^.bmask;
				h.ddspf.dwABitMask    := plain^.amask;
			end;
		end;

		h.dwCaps := _DDSCAPS_REQUIRED;
		if im.nLevels > 1 then h.dwCaps := h.dwCaps or DDSCAPS_COMPLEX;
		if texture_Mips in im.info.flags then
		begin
			h.dwFlags := h.dwFlags or DDSD_MIPMAPCOUNT;
			h.dwCaps := h.dwCaps or DDSCAPS_MIPMAP;
		end;

		case im.target of
			GLtexture_3D: h.dwCaps2 := h.dwCaps2 or DDSCAPS2_VOLUME;
			GLtexture_Cube: h.dwCaps2 := h.dwCaps2 or DDSCAPS2_CUBEMAP or DDSCAPS2_CUBEMAP_POSITIVEX or DDSCAPS2_CUBEMAP_NEGATIVEX or
				DDSCAPS2_CUBEMAP_POSITIVEY or DDSCAPS2_CUBEMAP_NEGATIVEY or DDSCAPS2_CUBEMAP_POSITIVEZ or DDSCAPS2_CUBEMAP_NEGATIVEZ;
		end;

		s^.Write('DDS ');
		s^.Write(@h, sizeof(h));
		for i := 0 to im.nLevels - 1 do
		begin
			if (im.target <> GLtexture_Cube) or (i <= 1) then
				lv := i
			else
				lv := i + 2 * sint(not odd(i)) - 1;

			data := im.LevelPtr(lv);
			case im.format of
				GLformat_RGB, GLformat_RGBA, GLformat_RGBA4:
					begin
						tmp := GetMem(im.info.GetLevelDataSize(lv));
						case im.format of
							GLformat_RGB, GLformat_RGBA:
								Convert(data, im.format, tmp, refFormat, im.info.GetLevelDimension(lv, 0), im.info.GetLevelDimension(lv, 1), im.info.GetLevelDimension(lv, 2));
							GLformat_RGBA4:
								for p := 0 to im.info.GetPixelsCount(lv) - 1 do
									pUint16(tmp)[p] := Ror(pUint16(data)[p], 4);
						end;
						data := tmp;
					end;
				else
					tmp := nil;
			end;

			try
				s^.Write(data, im.info.GetLevelDataSize(lv));
			finally
				FreeMem(tmp);
			end;
		end;
	end;

	function RecognizeFormat(const pf: DDS_PIXELFORMAT; out format: GLImageFormat): boolean;
	var
		i: sint;
	begin
		if pf.dwFourCC = 'DXT1' then begin format := GLformat_RGB_DXT1; exit(yes); end;
		if pf.dwFourCC = 'DXT5' then begin format := GLformat_RGBA_DXT5; exit(yes); end;

		for i := 0 to High(KnownPlain) do
			if (pf.dwRBitMask = KnownPlain[i].rmask) and (pf.dwGBitMask = KnownPlain[i].gmask) and (pf.dwBBitMask = KnownPlain[i].bmask) and
				(pf.dwABitMask = KnownPlain[i].amask)
			then
			begin
				format := KnownPlain[i].format;
				exit(yes);
			end;
		result := no;
	end;

	procedure ScanHeader(s: pStream; out hdr: DDS_HEADER);
	var
		magic: uint32;
	begin
		magic := Deserialize_ui32(s);
		if magic <> HEADER_MAGIC then raise Error('Неверная сигнатура DDS.');
		s^.Read(@hdr, sizeof(hdr));
	end;

	function DumpPixelFormatItem(id: uint; param: pointer): string;
	var
      f: ^DDS_PIXELFORMAT absolute param;
      recog: GLImageFormat;
	begin
		result := '';
		case id of
			0: result := 'Size = ' + ToString(f^.dwSize);
			1: result := 'Flags = ' + MaskToString(f^.dwFlags, ['ALPHAPIXELS', 'FOURCC', 'RGBMASK', 'LUMINANCE'],
				[DDPF_ALPHAPIXELS, DDPF_FOURCC, DDPF_RGB, DDPF_LUMINANCE]);
			2: if DDPF_FOURCC and f^.dwFlags <> 0 then result := 'FourCC = ' + USystem.ToString(pChar(f^.dwFourCC), length(f^.dwFourCC) * sizeof(char));
			3: if ((DDPF_RGB or DDPF_LUMINANCE) and f^.dwFlags <> 0) or (f^.dwRGBBitCount <> 0) then result := 'RGBBitCount = ' + ToString(f^.dwRGBBitCount);
			4: if ((DDPF_RGB or DDPF_LUMINANCE) and f^.dwFlags <> 0) or (f^.dwRBitMask <> 0) then result := 'RMask = $' + ToString(f^.dwRBitMask, IntFormat.Hex);
			5: if (DDPF_RGB and f^.dwFlags <> 0) or (f^.dwGBitMask <> 0) then result := 'GMask = $' + ToString(f^.dwGBitMask, IntFormat.Hex);
			6: if (DDPF_RGB and f^.dwFlags <> 0) or (f^.dwBBitMask <> 0) then result := 'BMask = $' + ToString(f^.dwBBitMask, IntFormat.Hex);
			7: if (DDPF_ALPHAPIXELS and f^.dwFlags <> 0) or (f^.dwABitMask <> 0) then result := 'AMask = $' + ToString(f^.dwABitMask, IntFormat.Hex);
			8: if RecognizeFormat(f^, recog) then result := 'Распознан как ' + GLImageFormatIds[recog] else result := 'Формат не распознан.';
		end;
	end;

	function DumpPixelFormat(const f: DDS_PIXELFORMAT): string;
	begin
		result := SeparatedList.Join(9, @DumpPixelFormatItem, @f, EOL);
	end;

	function Uint32Zeroed(id: uint; param: pointer): boolean;
	begin
		result := pUint32(param)[id] = 0;
	end;

	function GetUint32FromArray(id: uint; param: pointer): string;
	begin
		result := '$' + ToString(pUint32(param)[id], IntFormat.Hex);
	end;

	function DumpHeaderItem(id: uint; param: pointer): string;
		procedure Unused(value: uint32; const name: string);
		begin
			 if value <> 0 then result := name + ' = $' + ToString(value, IntFormat.Hex);
		end;
	var
		h: ^DDS_HEADER absolute param;
	begin
		result := '';
		case id of
			0: result := 'Size = ' + ToString(h^.dwSize);
			1: result := 'Flags = ' + MaskToString(h^.dwFlags, ['ETC', 'PITCH', 'MIPMAPCOUNT', 'LINEARSIZE', 'DEPTH'],
				[_DDSD_REQUIRED, DDSD_PITCH, DDSD_MIPMAPCOUNT, DDSD_LINEARSIZE, DDSD_DEPTH]);
			2: result := 'Height = ' + ToString(h^.dwHeight);
			3: result := 'Width = ' + ToString(h^.dwWidth);
			4:
				if (DDSD_PITCH and h^.dwFlags <> 0) or (DDSD_LINEARSIZE and h^.dwFlags = 0) and (h^.dwPitchOrLinearSize <> 0) then
					result := 'Pitch = ' + ToString(h^.dwPitchOrLinearSize);
			5: if DDSD_LINEARSIZE and h^.dwFlags <> 0 then result := 'LinearSize = ' + ToString(h^.dwPitchOrLinearSize);
			6: if (DDSD_DEPTH and h^.dwFlags <> 0) or (h^.dwDepth <> 0) then result := 'Depth = ' + ToString(h^.dwDepth);
			7: if (DDSD_MIPMAPCOUNT and h^.dwFlags <> 0) or (h^.dwMipMapCount <> 0) then result := 'MipMapCount = ' + ToString(h^.dwMipMapCount);
			8:
				if not Range.Closed(0, High(h^.dwReserved1)).All(@Uint32Zeroed, pUint32(h^.dwReserved1)) then
					result := 'Reserved = {' + SeparatedList.Join(length(h^.dwReserved1), @GetUint32FromArray, pUint32(h^.dwReserved1), ', ') + '}';
			9: result := 'Caps = ' + MaskToString(h^.dwCaps, ['ETC', 'MIPMAP', 'COMPLEX'], [_DDSCAPS_REQUIRED, DDSCAPS_MIPMAP, DDSCAPS_COMPLEX]);
			10:
				if h^.dwCaps2 <> 0 then
					result := 'Caps2 = ' + MaskToString(h^.dwCaps2,
						['CUBEMAP', 'CUBEMAP-X+', 'CUBEMAP-X-', 'CUBEMAP-Y+', 'CUBEMAP-Y-', 'CUBEMAP-Z+', 'CUBEMAP-Z-', 'VOLUME'], [DDSCAPS2_CUBEMAP,
						DDSCAPS2_CUBEMAP_POSITIVEX, DDSCAPS2_CUBEMAP_NEGATIVEX,
						DDSCAPS2_CUBEMAP_POSITIVEY, DDSCAPS2_CUBEMAP_NEGATIVEY,
						DDSCAPS2_CUBEMAP_POSITIVEZ, DDSCAPS2_CUBEMAP_NEGATIVEZ, DDSCAPS2_VOLUME]);
			11: Unused(h^.dwCaps3, 'Caps3');
			12: Unused(h^.dwCaps4, 'Caps4');
			13: Unused(h^.dwReserved2, 'Reserved2');
		end;
	end;

	function DumpHeader(const hdr: DDS_HEADER): string;
	begin
		result := SeparatedList.Join(14, @DumpHeaderItem, @hdr, EOL);
		result += EOL + EOL + 'PixelFormat:' + EOL + DumpPixelFormat(hdr.ddspf);
	end;

	function DumpHeader(s: pStream): string;
	var
		h: DDS_HEADER;
	begin
		s^.NewRef;
		try
			ScanHeader(s, h);
			result := DumpHeader(h);
		finally
			Release(s);
		end;
	end;

	procedure Load(var im: TextureImage; s: pStream);
	var
		hdr: DDS_HEADER;
		newTarget: GLTextureTarget;
		newSizes: UintVec3;
		format: GLImageFormat;
		flags: TextureImageFlags;
		i, lv, p: sint;
		u4: pUint16;
		t: Ticks;
	begin
		t := Ticks.Get;
		ScanHeader(s, hdr);
		if hdr.ddspf.dwFourCC = 'DX10' then raise Error('DDS версии DX10 не поддерживается.');
	{$ifdef Debug} if hdr.ddspf.dwFourCC <> '' then LogR('Найден FourCC ' + PrintableString(hdr.ddspf.dwFourCC) + '; ', logDebug) {$endif};

		flags := [];
		if DDSCAPS_MIPMAP and hdr.dwCaps <> 0 then Include(flags, texture_Mips);
	{$ifdef Debug} if texture_Mips in flags then LogR('Мипы: вкл; ', logDebug) else LogR('Мипы: выкл; ', logDebug); {$endif}
		if DDSCAPS2_VOLUME and hdr.dwCaps2 <> 0 then newTarget := GLtexture_3D else
		if DDSCAPS2_CUBEMAP and hdr.dwCaps2 <> 0 then newTarget := GLtexture_Cube else
			newTarget := GLtexture_2D;
	{$ifdef Debug} LogR('Тип: ' + GLTextureTargetIds[newTarget] + '; '); {$endif}

		if not RecognizeFormat(hdr.ddspf, format) then
			raise Error('Неподдерживаемая комбинация масок: ' + ToString(hdr.ddspf.dwRBitMask, IntFormat.Hex) + ' / ' +
				ToString(hdr.ddspf.dwGBitMask, IntFormat.Hex) + ' / ' + ToString(hdr.ddspf.dwBBitMask, IntFormat.Hex) + ' / ' +
				ToString(hdr.ddspf.dwABitMask, IntFormat.Hex));
	{$ifdef Debug} LogR('Формат: ' + GLImageFormatIds[format] + '; ', logDebug); {$endif}

		newSizes.x := hdr.dwWidth;
		newSizes.y := hdr.dwHeight;
		if newTarget = GLtexture_3D then newSizes.z := hdr.dwDepth else newSizes.z := 1;
	{$ifdef Debug} LogR('Размеры: ' + SizeToString(newSizes) + '; ', logDebug); {$endif}

		im.Prepare(newTarget, newSizes, format, flags);
		for i := 0 to im.nLevels - 1 do
		begin
			if (newTarget <> GLtexture_Cube) or (i <= 1) then
				lv := i
			else
				lv := i + 2 * sint(not odd(i)) - 1;
			s^.Read(im.LevelPtr(lv), im.info.GetLevelDataSize(lv));

			if (i = 0) and (im.format = GLformat_RGB_DXT1) and DetectDXT1A(im.LevelPtr(lv), im.info.LevelSizeXY(lv)) then
			begin
				im.info.format := GLformat_RGBA_DXT1;
			{$ifdef Debug} LogR('(RGBA); ', logDebug); {$endif}
			end;

			case format of
				GLformat_RGBA4:
					begin
						u4 := im.LevelPtr(lv);
						for p := 0 to im.info.GetPixelsCount(lv) - 1 do
							u4[p] := Rol(u4[p], 4);
					end;
			end;
		end;
		t := Ticks.Get - t;
		Con.WriteLine(ToString(t));
	end;

	procedure SuiteSave(obj: pointer; s: pStream); begin Save(pTextureImage(obj)^, s); end;
	procedure SuiteLoad(obj: pointer; s: pStream); begin Load(pTextureImage(obj)^, s); end;
	procedure Init;
	begin
		TextureImage.Loaders.Register('dds', @SuiteLoad, @SuiteSave);
	end;

initialization
	&Unit('DDS').Initialize(@Init);
end.
