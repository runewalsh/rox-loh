unit PNG;

{$include opts.inc}

interface

uses
	ctypes, USystem, UClasses, Streams, UMath, U_GL, GLBase, Utils, TextProcessing, DynamicLoader, GLUtils, BMP {$ifdef Debug}, ULog {$endif};

type
	PreSave = procedure(stream: pStream; size: size_t; param: pointer);
	SpeedVsRatio = (Fast, Good, Uber);

	function Save(data: pointer; const size: UintVec2; format: GLImageFormat; qs: SpeedVsRatio; out dataSize: size_t): pointer;
	procedure Save(stream: pStream; const size: UintVec2; format: GLImageFormat; data: pointer; qs: SpeedVsRatio; before: PreSave; param: pointer);

implementation

type
	lodepng = class
	type
	{$push} {$packrecords c}
		Allocator = record
			realloc: function(p: pointer; nsize: csize_t): pointer; cdecl;
		end;

		CompressSettings = record
			// LZ77 related settings
			btype: cuint; // the block type for LZ (0, 1, 2 or 3, see zlib standard). Should be 2 for proper compression.
			use_lz77: cuint; // whether or not to use LZ77. Should be 1 for proper compression.
			windowsize: cuint; // must be a power of two <= 32768. higher compresses more but is slower. Default value: 2048.
			minmatch: cuint; // mininum lz77 length. 3 is normally best, 6 can be better for some PNGs. Default: 0
			nicematch: cuint; // stop searching if >= this length found. Set to 258 for best compression. Default: 128
			lazymatching: cuint; // use lazy matching: better compression but a bit slower. Default: true
		end;
	{$pop}

	const
		CT_GREY = 0;       // greyscale: 1,2,4,8,16 bit
		CT_RGB = 2;        // RGB: 8,16 bit
		CT_PALETTE = 3;    // palette: 1,2,4,8 bit
		CT_GREY_ALPHA = 4; // greyscale with alpha: 8,16 bit
		CT_RGBA = 6;       // RGB with alpha: 8,16 bit

	class var
		decode_memory: function(out &out: pointer; out w, h: cuint; &in: pointer; insize: csize_t; colortype: cint; bitdepth: cuint;
			constref alloc: Allocator): cuint; cdecl;
		encode_memory: function(out &out: pointer; out outsize: csize_t; image: pointer; w, h: cuint; colortype: cint; bitdepth: cuint;
			constref settings: CompressSettings; constref alloc: Allocator): cuint; cdecl;

	class var
		loader: DLLoader;
	end;

	function LodeReAlloc(p: pointer; nsize: csize_t): pointer; cdecl;
	begin
		result := ReallocMem(p, nsize);
	end;

const
	Allocator: lodepng.Allocator = (realloc: @LodeReAlloc);
	Settings: array[SpeedVsRatio] of lodepng.CompressSettings =
	(
		( // Fast
			btype: 1;
			use_lz77: 1;
			windowsize: 1024;
			minmatch: 3;
			nicematch: 128;
			lazymatching: 1
		),
		( // Good
			btype: 2;
			use_lz77: 1;
			windowsize: 2048;
			minmatch: 3;
			nicematch: 128;
			lazymatching: 1
		),
		( // Uber
			btype: 2;
			use_lz77: 1;
			windowsize: 32768;
			minmatch: 3;
			nicematch: 258;
			lazymatching: 1
		)
	);

	procedure DescribeLodePNGFunctions(var fns: DLLoader.FunctionsList);
	begin
		fns
		.Func(@lodepng.decode_memory, 'decode_memory')^
		.Func(@lodepng.encode_memory, 'encode_memory')
	end;

	function Save(data: pointer; const size: UintVec2; format: GLImageFormat; qs: SpeedVsRatio; out dataSize: size_t): pointer;
		function UnsupportedFormat: Exception;
		begin
			result := Error('Сохранение ' + GLImageFormatIds[format] + ' в PNG не поддерживается. Точно поддерживается RGB(A)-8.');
		end;
	var
		lodetype: cint;
		lodebits: cuint;
		lodesize: csize_t;
		loderesult: cuint;
		prepared: pointer;
		preparedFormat: GLImageFormat;
		tryId: uint;
	begin
		prepared := data;
		preparedFormat := format;
		lodepng.loader.Load;
		try
			for tryId := 0 to 1 do
				case preparedFormat of
					GLformat_R:
						begin
							lodetype := lodepng.CT_GREY;
							lodebits := 8;
						end;
					GLformat_RG:
						begin
							lodetype := lodepng.CT_GREY_ALPHA;
							lodebits := 8;
						end;
					GLformat_RGB:
						begin
							lodetype := lodepng.CT_RGB;
							lodebits := 8;
						end;
					GLformat_RGBA:
						begin
							lodetype := lodepng.CT_RGBA;
							lodebits := 8;
						end;
					else
						if tryId = 0 then
						begin
							preparedFormat := ImageFormat8[GLImageFormatsInfo[preparedFormat].nChannels];
							if preparedFormat = format then raise UnsupportedFormat;
							prepared := GetMem(GetTextureDataSize(size, preparedFormat));
							Convert(data, format, prepared, preparedFormat, size);
						end else
							raise UnsupportedFormat;
				end;

			dataSize := 0;
			loderesult := lodepng.encode_memory(result, lodesize, prepared, size.x, size.y, lodetype, lodebits, Settings[qs], Allocator);
			if loderesult <> 0 then
				raise Error('Ошибка LodePNG, код {0}.', [loderesult]);
			dataSize := lodesize;
		finally
			if prepared <> data then FreeMem(prepared);
			lodepng.loader.Unload;
		end;
	end;

	procedure Save(stream: pStream; const size: UintVec2; format: GLImageFormat; data: pointer; qs: SpeedVsRatio; before: PreSave; param: pointer);
	var
		oim: pointer;
		osize: size_t;
	begin
	{$ifdef Debug} LogR('Сохраняю PNG ({0} x {1} @ {2}) в {3}... ', [size.x, size.y, GLImageFormatIds[format], StreamPath.Log(stream^.path)]); {$endif}
		oim := Save(data, size, format, qs, osize);
		try
			if Assigned(before) then before(stream, osize, param);
			stream^.Write(oim, osize);
		{$ifdef Debug} Log('PNG сохранён в ' + StreamPath.Log(stream^.path), logOK); {$endif}
		finally
			FreeMem(oim);
		end;
	end;

	procedure Save(var im: TextureImage; stream: pStream; const opts: string);
	var
		qs: SpeedVsRatio;
		ot: StringTokenizer;
		cp: ot.Guard;
		id: string;
	begin
		qs := Good;
		ot := opts;
		try
			while ot.MaybeTokenEndingWith(id, ['='], cp) do
			begin
				case id of
					'qs':
						begin
							ot.Expect('=');
							case ot.ScanTokenEndingWith([','], cp) of
								'fast': qs := Fast;
								'good': qs := Good;
								'uber': qs := Uber;
								else raise ot.UnknownIdentifier(cp);
							end;
						end;
					else raise ot.UnknownIdentifier(cp);
				end;

				if ot.Maybe(',') then else ot.ExpectEnd;
			end;
		finally
			ot.Done;
		end;

		Save(stream, im.Size.XY, im.format, im.FirstLevel, qs, nil, nil);
	end;

	procedure Load(var im: TextureImage; s: pStream; size: size_t);
	const
		PNGHeaderLen = 8;
		IHDR_offset = PNGHeaderLen;
	type
		pChunk_t = ^chunk_t;
		Chunk_t = packed record
			chunklen   : uint32;
			chunktype  : packed array[0 .. 3] of char;
		end;
		IHDR_data = packed record
			asChunk    : Chunk_t;
			width      : uint32;
			height     : uint32;
			bitDepth   : uint8;
			colorType  : uint8;
			compression: uint8;
			filter     : uint8;
			interlace  : uint8;
		end;

		function has_tRNS(data: pChunk_t; size: size_t): boolean;
		var
			len: size_t;
		begin
			result := no;
			repeat
				len := BEtoN(data^.chunklen) + sizeof(Chunk_t) + {CRC} sizeof(uint32);
				if len + sizeof(Chunk_t) > size then exit;

				pointer(data) += len;
				size -= len;
				if data^.chunktype = 'tRNS' then begin {$ifdef Debug} Log('tRNS; ', logDebug); {$endif} exit(yes); end
				else if data^.chunktype = 'PLTE' then // продолжить
				else exit(no);
			until no;
		end;

	const
		PLTE_BIT  = 1 shl 0;
		RGB_BIT   = 1 shl 1;
		ALPHA_BIT = 1 shl 2;
	var
		png: pStreamImage;
		decoded: pointer;
		ihdr: ^IHDR_data;
		targetFormat: GLImageFormat;
		lodePngColorType: cint;
		ihdrWidth, ihdrHeight, realWidth, realHeight: cuint;
	begin
		png := nil;
		lodepng.loader.Load;
		try
			if size < IHDR_offset + sizeof(((@ihdr)^^)) then raise Error('Размер PNG-файла неверен.');
			png := s^.GetImage(s^.Position, size);
			ihdr := png^.data + IHDR_offset;
			if ihdr^.asChunk.chunktype <> 'IHDR' then raise Error('Неверная сигнатура IHDR.');
			ihdrWidth := BEtoN(ihdr^.width);
			ihdrHeight := BEtoN(ihdr^.height);

		{$ifdef Debug}
			LogR('Данные IHDR: ширина = ' + ToString(ihdrWidth) + ', высота = ' + ToString(ihdrHeight) +
				', глубина цвета — ' + ToString(ihdr^.bitDepth) + ' бит; ', logDebug); {$endif}
			ValidateImageSize(UintVec2.Make(ihdrWidth, ihdrHeight));

			if RGB_BIT and ihdr^.colorType <> 0 then
				if (ALPHA_BIT and ihdr^.colorType <> 0) or ((ihdr^.colorType and PLTE_BIT <> 0) and has_tRNS(@ihdr^.asChunk, size - IHDR_offset)) then
				begin
					targetFormat := GLformat_RGBA;
					lodePngColorType := lodepng.CT_RGBA;
				end else
				begin
					targetFormat := GLformat_RGB;
					lodePngColorType := lodepng.CT_RGB;
				end
			else
				if (ALPHA_BIT and ihdr^.colorType <> 0) then
				begin
					targetFormat := GLformat_RG;
					lodePngColorType := lodepng.CT_GREY_ALPHA;
				end else
				begin
					targetFormat := GLformat_R;
					lodePngColorType := lodepng.CT_GREY;
				end;

			im.Prepare(GLtexture_2D, UintVec2.Make(ihdrWidth, ihdrHeight), targetFormat, [texture_ManualImgs]);
			if lodepng.decode_memory(decoded, realWidth, realHeight, png^.data, size, lodePngColorType, bitsizeof(uint8), Allocator) <> 0 then
				raise Error('Не удалось загрузить PNG.');
			try
				if (ihdrWidth <> realWidth) or (ihdrHeight <> realHeight) then
					raise Error('Данные IHDR расходятся с данными от LodePNG.');
			except
				FreeMem(decoded);
				raise;
			end;

			im.ReplaceLevelWithOwnPointer(0, decoded);
			if s^.CanSeek then s^.Position := s^.Position + size;
		finally
			Release(png);
			lodepng.loader.Unload;
		end;
	end;

	procedure SuiteSave(obj: pointer; s: pStream; const opts: string); begin Save(pTextureImage(obj)^, s, opts); end;
	procedure SuiteLoad(obj: pointer; s: pStream; const size: FileSize); begin Load(pTextureImage(obj)^, s, size.AsSizeT); end;
	procedure Init;
	begin
		lodepng.loader.Init('lodepng(prefix = lodepng_, perm, lock)', @DescribeLodePNGFunctions);
		TextureImage.Loaders.Register('png', @SuiteLoad, @SuiteSave);
	end;

	procedure Done;
	begin
		lodepng.loader.Done;
	end;

initialization
	&Unit('PNG').Initialize(@Init, @Done).Priority(-2);
end.
