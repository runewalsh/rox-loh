unit FreeType;

{$include opts.inc}
{$ifdef Debug}
	{-$define DebugPerformance}
{$endif}

interface

uses
	ctypes, USystem, UClasses, UMath, Utils, Streams, DynamicLoader, U_GL, GLBase
{$ifdef Debug}, ULog {$endif};

type
	RasterizationOptions = object
		baseColor, outlineColor: Vec4;
		outlineSize: float;
	{$ifdef Debug} guard: pointer; {$endif}
		function Create: RasterizationOptions; static;
		procedure Done;
	end;

	RasterizedSymbol = object
		sizes, imShift: Vec2;
		cursorShift: float;
		procedure Done;
		function Image: pTextureImage;
		procedure KillImage;
	private
		im: TextureImage;
	end;

	TrueType = object
	private const
		MaxImageSize = 5 * 1024 * 1024;
	private var
	{$ifdef Debug} _fn: string; {$endif}
		faceptr: pointer; // ^FT_Face
		ttfImage: pStreamImage;
		invUpEM, lineDist: float;
	public
		function Invalid: TrueType; static;
		function Load(const fn: string): TrueType; static;
		procedure Unload;
		function OK: boolean;
		function Rasterize(const sym: UTFchar; const resolution: float; const opts: RasterizationOptions; out ra: RasterizedSymbol): boolean;
		function Kerning(const a, b: UTFchar): float;
	{$ifdef Debug} property SourceFilename: string read _fn; {$endif}
		property LineDistance: float read lineDist;
	end;

implementation

	function RasterizationOptions.Create: RasterizationOptions;
	begin
		result.baseColor    := Vec4.Make(1.0, 0.0, 0.0, 1.0);
		result.outlineColor := Vec4.Make(0.5, 0.0, 0.0, 1.0);
		result.outlineSize  := 0.0;
	{$ifdef Debug} result.guard := GetMem(1); {$endif}
	end;

	procedure RasterizationOptions.Done;
	begin
	{$ifdef Debug} FreeMem(guard); {$endif}
	end;

	procedure RasterizedSymbol.Done;
	begin
		im.Done;
	end;

	function RasterizedSymbol.Image: pTextureImage;
	begin
		if im.OK then result := @im else result := nil;
	end;

	procedure RasterizedSymbol.KillImage;
	begin
		im.Done;
	end;

const
	FT_LOAD_NO_SCALE = 1 shl 0;
	FT_LOAD_RENDER   = 1 shl 2;

type
	FT_Result = cint;
	FT_Fixed = clong;

	pFT_MemoryRec = ^FT_MemoryRec;
	FT_AllocFunc = function(memory: pFT_MemoryRec; size: clong): pointer; cdecl;
	FT_FreeFunc = procedure(memory: pFT_MemoryRec; block: pointer); cdecl;
	FT_ReallocFunc = function(memory: pFT_MemoryRec; curSize, newSize: clong; block: pointer): pointer; cdecl;
	FT_MemoryRec = record
		user: pointer;
		alloc: FT_AllocFunc;
		free: FT_FreeFunc;
		realloc: FT_ReallocFunc;
	end;

	pFT_Library = ^FT_Library; FT_Library = record end;
	pFT_Bitmap_Size = ^FT_Bitmap_Size; FT_Bitmap_Size = record end;
	pFT_CharMap = ^FT_CharMap; FT_CharMap = record end;

	FT_Generic_Finalizer = procedure(data: pointer);
	FT_Generic = record
		data: pointer;
		finalizer: FT_Generic_Finalizer;
	end;

	FT_BBox = record
		xMin, yMin, xMax, yMax: clong;
	end;

	pFT_Glyph_Metrics = ^FT_Glyph_Metrics;
	FT_Glyph_Metrics = record
		width, height: clong;
		horiBearingX, horiBearingY, horiAdvance: clong;
		vertBearingX, vertBearingY, vertAdvance: clong;
	end;

	pFT_Vector = ^FT_Vector;
	FT_Vector = record
		x, y: clong;
	end;

	FT_Glyph_Format = record
		pamparampampam: clong;
	end;

	pFT_Bitmap = ^FT_Bitmap;
	FT_Bitmap = record
		rows, width: cint;
		pitch: cint;
		buffer: pointer;
		num_grays: cshort;
		pixel_mode: cchar;
		palette_mode: cchar;
		palette: pointer;
	end;

const
	// A monochrome bitmap, using 1 bit per pixel.
	// Note that pixels are stored in most-significant order (MSB), which means that the left-most pixel in a byte has value 128.
	// FT_PIXEL_MODE_MONO  = 1;

	// An 8-bit bitmap, generally used to represent anti-aliased glyph images. Each pixel is stored in one byte.
	// Note that the number of ‘gray’ levels is stored in the ‘num_grays’ field of the FT_Bitmap structure (it generally is 256).
	FT_PIXEL_MODE_GRAY  = 2;

	// A 2-bit per pixel bitmap, used to represent embedded anti-aliased bitmaps in font files according to the OpenType specification.
	// We haven't found a single font using this format, however.
	// FT_PIXEL_MODE_GRAY2 = 3;

	// A 4-bit per pixel bitmap, representing embedded anti-aliased bitmaps in font files according to the OpenType specification.
	// We haven't found a single font using this format, however.
	// FT_PIXEL_MODE_GRAY4 = 4;

	// An 8-bit bitmap, representing RGB or BGR decimated glyph images used for display on LCD displays;
	// the bitmap is three times wider than the original glyph image.
	// FT_PIXEL_MODE_LCD   = 5;

	// An 8-bit bitmap, representing RGB or BGR decimated glyph images used for display on rotated LCD displays;
	// the bitmap is three times taller than the original glyph image.
	// FT_PIXEL_MODE_LCD_V = 6;

	// An image with four 8-bit channels per pixel, representing a color image (such as emoticons) with alpha channel.
	// For each pixel, the format is BGRA, which means, the blue channel comes first in memory.
	// The color channels are pre-multiplied and in the sRGB colorspace.
	// For example, full red at half-translucent opacity will be represented as ‘00,00,80,80’, not ‘00,00,FF,80’.
	FT_PIXEL_MODE_BGRA  = 7;

{$ifdef Debug}
	FT_FACE_FLAG_SCALABLE         = 1 shl 0;
	FT_FACE_FLAG_FIXED_SIZES      = 1 shl 1;
	FT_FACE_FLAG_FIXED_WIDTH      = 1 shl 2;
	FT_FACE_FLAG_SFNT             = 1 shl 3;
	FT_FACE_FLAG_HORIZONTAL       = 1 shl 4;
{$endif}
	FT_FACE_FLAG_VERTICAL         = 1 shl 5;
{$ifdef Debug}
	FT_FACE_FLAG_KERNING          = 1 shl 6;
	FT_FACE_FLAG_MULTIPLE_MASTERS = 1 shl 8;
	FT_FACE_FLAG_GLYPH_NAMES      = 1 shl 9;
	FT_FACE_FLAG_HINTER           = 1 shl 11;
	FT_FACE_FLAG_CID_KEYED        = 1 shl 12;
	FT_FACE_FLAG_TRICKY           = 1 shl 13;
{$endif}

	// FT_KERNING_DEFAULT  = 0;
	// FT_KERNING_UNFITTED = 1;
	FT_KERNING_UNSCALED = 2;

type
	pFT_Face = ^FT_Face;

	pFT_Outline = ^FT_Outline;
	FT_Outline = record
		n_contours, n_points: cshort;
		points: pFT_Vector;
		tags: pChar;
		contours: pCShort;
		flags: cint;
	end;

	pFT_GlyphSlot = ^FT_GlyphSlot;
	FT_GlyphSlot = record
		lib: pFT_Library;
		face: pFT_Face;
		next: pFT_GlyphSlot;
		reserved: cuint;
		generic: FT_Generic;

		metrics: FT_Glyph_Metrics;
		linearHoriAdvance, linearVertAdvance: clong;
		advance: FT_Vector;

		format: FT_Glyph_Format;

		bitmap: FT_Bitmap;
		bitmap_left, bitmap_top: cint;

		outline: FT_Outline;
		// ...
	end;

	pFT_Size = ^FT_Size; FT_Size = record end;

	FT_Face = object
		num_faces: clong;
		face_index: clong;

		face_flags: clong;
		style_flags: clong;

		num_glyphs: clong;

		family_name: pChar;
		style_name: pChar;

		num_fixed_sizes: cint;
		available_sizes: pFT_Bitmap_Size;

		num_charmaps: cint;
		charmaps: pFT_CharMap;

		generic: FT_Generic;

		bbox: FT_BBox;

		units_per_EM: cushort;
		ascender: cshort;
		descender: cshort;
		height: cshort;

		max_advance_width: cshort;
		max_advance_height: cshort;

		underline_position: cshort;
		underline_thickness: cshort;

		glyph: pFT_GlyphSlot;
		size: pFT_Size;
		charmap: pFT_CharMap;
		function Vertical: boolean;
	{$ifdef Debug} function HumanFlags: string; {$endif}
	end;

	function FT_Face.Vertical: boolean;
	begin
		result := (face_flags and FT_FACE_FLAG_VERTICAL) <> 0;
	end;

{$ifdef Debug}
	function FT_Face.HumanFlags: string;
	const
		Flags: array[0 .. 11] of record
			flag: uint;
			desc: string;
		end =
		(
			(flag: FT_FACE_FLAG_SCALABLE; desc: 'вектор'),
			(flag: FT_FACE_FLAG_FIXED_SIZES; desc: 'растр'),
			(flag: FT_FACE_FLAG_FIXED_WIDTH; desc: 'моноширинный'),
			(flag: FT_FACE_FLAG_SFNT; desc: 'TrueType'),
			(flag: FT_FACE_FLAG_HORIZONTAL; desc: 'горизонтальный'),
			(flag: FT_FACE_FLAG_VERTICAL; desc: 'вертикальный'),
			(flag: FT_FACE_FLAG_KERNING; desc: 'кернинг'),
			(flag: FT_FACE_FLAG_MULTIPLE_MASTERS; desc: 'Multiple Masters'),
			(flag: FT_FACE_FLAG_GLYPH_NAMES; desc: 'имена символов'),
			(flag: FT_FACE_FLAG_HINTER; desc: 'подсказки'),
			(flag: FT_FACE_FLAG_CID_KEYED; desc: 'CID-ключи вместо индексов'),
			(flag: FT_FACE_FLAG_TRICKY; desc: 'хитрый ;3')
		);
	var
		i: sint;
	begin
		result := '';
		for i := 0 to High(Flags) do
			if (Flags[i].flag and face_flags) <> 0 then
			begin
				if result <> '' then result += ', ';
				result += Flags[i].desc;
			end;
		if result = '' then result := 'нет';
	end;
{$endif}

type
	pFT_Span = ^FT_Span;
	FT_Span = record
		x: cshort;
		len: cushort;
		coverage: cuchar;
	end;

	FT_SpanFunc = procedure(y, count: cint; spans: pFT_Span; user: pointer); cdecl;
	FT_Raster_BitTest_Func = function(y, x: cint; user: pointer): cint; cdecl;
	FT_Raster_BitSet_Func = function(y, x: cint; user: pointer): cint; cdecl;

	pFT_Raster_Params = ^FT_Raster_Params;
	FT_Raster_Params = record
		target: pFT_Bitmap;
		source: pointer;
		flags: cint;
		gray_spans: FT_SpanFunc;
		black_spans: FT_SpanFunc;         // doesn't work!
		bit_test: FT_Raster_BitTest_Func; // doesn't work!
		bit_set: FT_Raster_BitSet_Func;   // doesn't work!
		user: pointer;
		clip_box: FT_BBox;
	end;

const
	// FT_STROKER_LINECAP_BUTT = 0;
	FT_STROKER_LINECAP_ROUND = 1;
	// FT_STROKER_LINECAP_SQUARE = 2;

	FT_STROKER_LINEJOIN_ROUND          = 0;
	// FT_STROKER_LINEJOIN_BEVEL          = 1;
	// FT_STROKER_LINEJOIN_MITER_VARIABLE = 2;
	// FT_STROKER_LINEJOIN_MITER          = FT_STROKER_LINEJOIN_MITER_VARIABLE;
	// FT_STROKER_LINEJOIN_MITER_FIXED    = 3;

type
	pFT_Stroker = ^FT_Stroker; FT_Stroker = record end;
	pFT_Glyph_Class = ^FT_Glyph_Class; FT_Glyph_Class = record end;

	pFT_Glyph = ^FT_Glyph;
	FT_Glyph = object
		lib: pFT_Library;
		clazz: pFT_Glyph_Class;
		format: FT_Glyph_Format;
		advance: FT_Vector;
	end;

	pFT_BitmapGlyph = ^FT_BitmapGlyph;
	FT_BitmapGlyph = object(FT_Glyph)
		left, top: cint;
		bitmap: FT_Bitmap;
	end;

var
	FT_New_Library: function(mem: pFT_MemoryRec; out lib: pFT_Library): FT_Result; cdecl;
	FT_Add_Default_Modules: procedure(lib: pFT_Library); cdecl;
	FT_Done_Library: function(lib: pFT_Library): FT_Result; cdecl;
	FT_New_Face: function(lib: pFT_Library; fn: pChar; face_index: clong; out face: pFT_Face): FT_Result; cdecl;
	FT_New_Memory_Face: function(lib: pFT_Library; base: pointer; size: clong; face_index: clong; out face: pFT_Face): FT_Result; cdecl;
	FT_Done_Face: function(face: pFT_Face): FT_Result; cdecl;
	FT_Set_Pixel_Sizes: function(face: pFT_Face; pixel_width, pixel_height: cuint): FT_Result; cdecl;
	FT_Get_Char_Index: function(face: pFT_Face; charu32: culong): cuint; cdecl;
	FT_Load_Glyph: function(face: pFT_Face; index: cuint; flags: sint32): FT_Result; cdecl;
	FT_Outline_Render: function(lib: pFT_Library; outline: pFT_Outline; params: pFT_Raster_Params): FT_Result; cdecl;
	FT_Get_Glyph: function(slot: pFT_GlyphSlot; out glyph: pFT_Glyph): FT_Result; cdecl;
	FT_Done_Glyph: procedure(glyph: pFT_Glyph); cdecl;
	FT_Stroker_New: function(lib: pFT_Library; out stroker: pFT_Stroker): FT_Result; cdecl;
	FT_Stroker_Done: procedure(stroker: pFT_Stroker); cdecl;
	FT_Stroker_Set: procedure(stroker: pFT_Stroker; radius: FT_Fixed; line_cap, line_join: cint; miter_limit: FT_Fixed); cdecl;
	FT_Glyph_Stroke: function(var glyph: pFT_Glyph; stroker: pFT_Stroker; destroy: cuchar): FT_Result; cdecl;
	FT_Glyph_To_Bitmap: function(var glyph: pFT_Glyph; render_mode: cint; origin: pFT_Vector; destroy: cuchar): FT_Result; cdecl;
	FT_Get_Kerning: function(face: pFT_Face; left_glyph, right_glyph: cuint; kern_mode: cuint; out akerning: FT_Vector): FT_Result; cdecl;

var
	loader: DLLoader;
	FTh: pFT_Library;

	function FT_Alloc(memory: pFT_MemoryRec; size: clong): pointer; cdecl;
	begin
		Assert(@memory = @memory);
		result := GetMem(size);
	end;

	procedure FT_Free(memory: pFT_MemoryRec; block: pointer); cdecl;
	begin
		Assert(@memory = @memory);
		FreeMem(block);
	end;

	function FT_Realloc(memory: pFT_MemoryRec; curSize, newSize: clong; block: pointer): pointer; cdecl;
	begin
		Assert(@memory = @memory);
		Assert(@curSize = @curSize);
		result := ReallocMem(block, newSize);
	end;

const
	FTMem: FT_MemoryRec =
	(
		user: nil;
		alloc: @FT_Alloc;
		free: @FT_Free;
		realloc: @FT_Realloc
	);

	procedure AfterLoad;
	begin
	{$ifdef Debug} LogR('Инициализация FreeType... '); {$endif}

		if FT_New_Library(@FTmem, FTh) = 0 then
		begin
		{$ifdef Debug} LogR('Загрузка модулей FreeType... '); {$endif}
			FT_Add_Default_Modules(FTh);
		{$ifdef Debug} Log('FreeType инициализирована', logOk); {$endif}
		end else
		begin
		{$ifdef Debug} Log('Ошибка инициации FreeType', logError); {$endif}
			FTh := nil;
			raise Error('Не удалось инициализировать FreeType.');
		end;
	end;

	procedure BeforeUnload;
	begin
		if Assigned(FTh) then
		begin
		{$ifdef Debug} LogR('Финализация FreeType... '); {$endif}
			if FT_Done_Library(FTh) = 0 then
			begin
			{$ifdef Debug} Log('FreeType финализирована', logOk); {$endif}
			end else
			begin
			{$ifdef Debug} Log('Ошибка финализации FreeType', logWarning); {$endif}
			end;
		end;
	end;

	function FTErrorDesc(r: FT_Result): string;
	begin
		case r of
			$0: result := 'всё в порядке';
			$1: result := 'не удаётся открыть ресурс';
			$2: result := 'неизвестный формат файла';
			$3: result := 'файл повреждён';
			$4: result := 'неверная версия FreeType';
			$5: result := 'слишком старая версия модуля';
			$6: result := 'неверный аргумент';
			$7: result := 'возможность не реализована';
			$8: result := 'таблица повреждена';
			$9: result := 'странное смещение в таблице';
			$A: result := 'слишком большой размер массива';
			$B: result := 'модуль не загружен';
			$C: result := 'свойство отсутствует';

			$10: result := 'неверный индекс символа';
			$11: result := 'неверный код символа';
			$12: result := 'неподдерживаемый формат изображения символа';
			$13: result := 'не удаётся прочитать этот формат символа';
			$14: result := 'неверный контур';
			$15: result := 'неверный составной символ';
			$16: result := 'слишком много подсказок';
			$17: result := 'неверный размер пиксела';

			$20: result := 'неверный дескриптор';
			$21: result := 'неверный дескриптор либы';
			$22: result := 'неверный дескриптор модуля';
			$23: result := 'неверный дескриптор шрифта';
			$24: result := 'неверный дескриптор размера';
			$25: result := 'неверный дескриптор слота символа';
			$26: result := 'неверный дескриптор таблицы символов';
			$27: result := 'неверный дескриптор менеджера кэша';
			$28: result := 'неверный дескриптор потока';

			$30: result := 'слишком много модулей';
			$31: result := 'слишком много расширений';

			$40: result := 'недостаточно памяти';
			$41: result := 'неучтённый объект';

			$51: result := 'не удаётся открыть поток';
			$52: result := 'неверный seek в потоке';
			$53: result := 'неверный skip в потоке';
			$54: result := 'неверное чтение из потока';
			$55: result := 'неверная операция над потоком';
			$56: result := 'неверная операция над кадром';
			$57: result := 'доступ к вложенному кадру';
			$58: result := 'неверное чтение кадра';

			$60: result := 'растр не инициализирован';
			$61: result := 'растр повреждён';
			$62: result := 'переполнение растра';
			$63: result := 'отрицательная высота растра';

			$70: result := 'слишком много кэшей';

			$80: result := 'неверный опкод в шрифте';
			$81: result := 'недостаточно аргументов в шрифте';
			$82: result := 'переполнение стека';
			$83: result := 'переполнение кода';
			$84: result := 'неверный аргумент в шрифте';
			$85: result := 'поделил на 0 в шрифте лол';
			$86: result := 'неверная ссылка в шрифте';
			$87: result := 'отладочный опкод в шрифте';
			$88: result := 'инструкция ENDF в выполняющемся потоке';
			$89: result := 'вложенный DEFS';
			$8A: result := 'неверный CodeRange';
			$8B: result := 'слишком длинный контекст выполнения';
			$8C: result := 'слишком много функций';
			$8D: result := 'слишком много инструкций';
			$8E: result := 'нет таблицы SFNT';
			$8F: result := 'нет таблицы горизонтальных заголовков (hhea)';
			$90: result := 'нет таблицы locations (loca)';
			$91: result := 'нет таблицы имён';
			$92: result := 'нет таблицы символов (cmap)';
			$93: result := 'нет таблицы горизонтальных метрик (hmtx)';
			$94: result := 'нет таблицы PostScript (post)';
			$95: result := 'неверная горизонтальная метрика';
			$96: result := 'неверный формат таблицы символов (cmap)';
			$97: result := 'неверное значение ppem';
			$98: result := 'неверная вертикальная метрика';
			$99: result := 'контекст не найден';
			$9A: result := 'неверный формат таблицы PostScript (post)';
			$9B: result := 'неверная таблица PostScript (post)';

			$A0: result := 'ошибка в синтаксисе опкода';
			$A1: result := 'антипереполнение стека аргументов';
			$A2: result := '(игнорировать. wat?)';
			$A3: result := 'не найдено юникодовское имя символа';
			$A4: result := 'символ слишком большой для подсказок';

			$B0: result := 'нет поля STARTFONT';
			$B1: result := 'нет поля FONT';
			$B2: result := 'нет поля SIZE';
			$B3: result := 'нет поля FONTBOUNDINGBOX';
			$B4: result := 'нет поля CHARS';
			$B5: result := 'нет поля STARTCHAR';
			$B6: result := 'нет поля ENCODING';
			$B7: result := 'нет поля BBX';
			$B8: result := 'BBX слишком большой';
			$B9: result := 'повреждён заголовок шрифта';
			$BA: result := 'повреждены символы шрифта';
			else
				result := 'неизвестная ошибка (код ' + ToString(r) + ')';
		end;
	end;

	function TrueType.Invalid: TrueType;
	begin
		result.faceptr := nil;
	end;

	function TrueType.Load(const fn: string): TrueType;
	var
		r: FT_Result;
		face: pFT_Face absolute result.faceptr;
		s: pStream;
		imgSize: size_t;
	begin
		result := Invalid;
		loader.Load;
	{$ifdef Debug}
		result._fn := fn;
		LogR('Загрузка шрифта из ' + StreamPath.Log(fn) + '... ');
	{$endif}

		s := MakeRef(GetStream(fn));
		imgSize := (s^.Size - s^.Position).value;
		if imgSize <= MaxImageSize then
		begin
			result.ttfImage := s^.GetImage(s^.Position, imgSize);
			Release(s);
			if not Assigned(result.ttfImage) then exit;

		{$ifdef Debug} LogR('(образ замаплен в память); ', logDebug); {$endif}
			r := FT_New_Memory_Face(FTh, result.ttfImage^.data, result.ttfImage^.size, 0, face);
		end else
		begin
		{$ifdef Debug} LogR('(образ слишком большой, буду читать с диска по запросу); ', logDebug); {$endif}
			Release(s);
			r := FT_New_Face(FTh, pChar(StreamPath.System(fn)), 0, face);
		end;

		if r = 0 then
		begin
		{$ifdef Debug}
			Log('Шрифт ' + StreamPath.Log(fn) + ' загружен; семейство: "' + face^.family_name + '", стиль: "' + face^.style_name + '", ' +
				'подшрифтов: ' + ToString(face^.num_faces) + ', символов: ' + ToString(face^.num_glyphs) + ', фиксированных размеров: ' + ToString(face^.num_fixed_sizes) + ', ' +
				'карт символов: ' + ToString(face^.num_charmaps) + ', ед./EM: ' + ToString(face^.units_per_EM) + ', ' +
				'ascender - descender: ' + ToString(face^.ascender - face^.descender) + ', расстояние между строками: ' + ToString(face^.height) + ', ' +
				'макс. dx: ' + ToString(face^.max_advance_width) + ', макс.dy: ' + ToString(face^.max_advance_height) + ', ' +
				'флаги: ' + face^.HumanFlags, logOK);
		{$endif}
			result.invUpEM := 1.0 / face^.units_per_EM;
			result.lineDist := face^.height * result.invUpEM;
		end else
		begin
		{$ifdef Debug} Log('Не удалось загрузить шрифт ' + StreamPath.Log(fn) + ': ' + FTErrorDesc(r), logError); {$endif}
			result.Unload;
		end;
	end;

	procedure TrueType.Unload;
	begin
		if not OK then exit;
		if Assigned(faceptr) then
		begin
		{$ifdef Debug} LogR('Выгрузка шрифта ' + StreamPath.Log(_fn) + '... '); {$endif}

			if FT_Done_Face(pFT_Face(faceptr)) = 0 then
			begin
			{$ifdef Debug} Log('Шрифт ' + StreamPath.Log(_fn) + ' выгружен', logOK); {$endif}
			end else
			begin
			{$ifdef Debug} Log('Ошибка при выгрузке шрифта ' + StreamPath.Log(_fn), logWarning); {$endif}
			end;
		end;
		Release(ttfImage);
		loader.Unload;
		self := Invalid;
	end;

	function TrueType.OK: boolean;
	begin
		result := Assigned(faceptr);
	end;

	function _FTBitmap2Texture(bm: pFT_Bitmap; dx, dy: sint; var im: TextureImage; const color: Vec4; combine: ImageCombineMode): boolean;
	const
		InvU8 = 1.0 / High(uint8);
	var
		c8: Vec4u8;
		i, x, y, x1, y1, x2, y2: sint;
		row, p, im_p: pointer;
		im_ps: size_t;
		p8: pUint8 absolute p;
	{$ifdef DebugPerformance} t: tTimer; {$endif}
	begin
		result := no;
		row := bm^.buffer;
		x1 := dx;
		x2 := dx + bm^.width - 1;
		y1 := dy;
		y2 := dy + bm^.rows - 1;
		im_ps := im.PixelSize;
		for i := 0 to High(color.data) do
			c8[i] := DenormU8(color.data[i]);

		case bm^.pixel_mode of
			FT_PIXEL_MODE_GRAY:
				begin
				{$ifdef Debug} LogR('Формат: GRAY; ', logDebug); {$endif}
					if bm^.num_grays <> sint(High(uint8)) + 1 then
					begin
					{$ifdef Debug} Log('Неподдерживаемое количество уровней серого: ' + ToString(bm^.num_grays), logError); {$endif}
						exit;
					end;

				{$ifdef DebugPerformance} t.Start; {$endif}
					for y := y1 to y2 do
					begin
						p := row;
						im_p := im.PixelPtr(x1, y, 0, 0);
						for x := x1 to x2 do
						begin
							c8[3] := p8[0];
							im.CombinePixel(im_p, c8, combine);
							im_p += im_ps;
							p8 += 1;
						end;
						row += bm^.pitch;
					end;
				{$ifdef DebugPerformance} LogR('Время перевода в текстуру: ' + ToString(t.Time * 1.0e6) + ' мкс; ', logWarning); {$endif}
				end;
			FT_PIXEL_MODE_BGRA:
				begin
				{$ifdef Debug} LogR('Формат: BGRA; ', logDebug); {$endif}

					for y := y1 to y2 do
					begin
						p := row;
						im_p := im.PixelPtr(x1, y, 0, 0);
						for x := x1 to x2 do
						begin
							im.CombinePixel(im_p, color * Vec4.Make(p8[2], p8[1], p8[0], p8[3]) * InvU8, combine);
							im_p += im_ps;
							p8 += 4;
						end;
						row += bm^.pitch;
					end;
				end;
			else
				begin
				{$ifdef Debug} Log('Неподдерживаемый формат изображения, код ' + ToString(bm^.pixel_mode), logError); {$endif}
					exit;
				end;
		end;
		result := yes;
	end;

	function _RasterizeOutline(const opts: RasterizationOptions; glyph: pFT_GlyphSlot; const resolution: float; out gbm: pFT_BitmapGlyph): boolean;
	label _finally_;
	var
		r: FT_Result;
		stroker: pFT_Stroker;
		sglyph: pFT_Glyph;
	{$ifdef DebugPerformance} t: tTimer; {$endif}
	begin
		result := no;
		sglyph := nil;
		stroker := nil;

		r := FT_Get_Glyph(glyph, sglyph);
		if r <> 0 then
		begin
		{$ifdef Debug} Log('Ошибка при получении символа: ' + FTErrorDesc(r), logError); {$endif}
			sglyph := nil;
			goto _finally_;
		end;

		r := FT_Stroker_New(FTh, stroker);
		if r <> 0 then
		begin
	{$ifdef Debug} Log('Ошибка при создании обводчика: ' + FTErrorDesc(r), logError); {$endif}
		stroker := nil;
		goto _finally_;
		end;

	{$ifdef DebugPerformance} t.Start; {$endif}
		FT_Stroker_Set(stroker, round(26.6 * opts.outlineSize * resolution), FT_STROKER_LINECAP_ROUND, FT_STROKER_LINEJOIN_ROUND, 0);

		r := FT_Glyph_Stroke(sglyph, stroker, 1);
		if r <> 0 then
		begin
		{$ifdef Debug} Log('Ошибка при обводке символа: ' + FTErrorDesc(r), logError); {$endif}
			goto _finally_;
		end;

		r := FT_Glyph_To_Bitmap(sglyph, 0, nil, 1);
		if r <> 0 then
		begin
		{$ifdef Debug} Log('Ошибка при растеризации обведённого контура: ' + FTErrorDesc(r), logError); {$endif}
			goto _finally_;
		end;
	{$ifdef DebugPerformance} LogR('Время растеризации контура (freetype): ' + ToString(t.Time * 1.0e6) + ' мкс; ', logDebug); {$endif}

		gbm := pFT_BitmapGlyph(sglyph);
	{$ifdef Debug} if gbm^.bitmap.width <> 0 then LogR('Контур растеризован; ', logOK); {$endif}
		result := yes;
	_finally_:
		if not result and Assigned(sglyph) then FT_Done_Glyph(sglyph);
		if Assigned(stroker) then FT_Stroker_Done(stroker);
	end;

	function TrueType.Rasterize(const sym: UTFchar; const resolution: float; const opts: RasterizationOptions; out ra: RasterizedSymbol): boolean;
	label _finally_;
		function Quote(sym: UTFchar): string;
		begin
			if sym = ord('"') then result := '''' else result := '"';
		end;
	var
		face: pFT_Face absolute faceptr;
		r: FT_Result;
		glyphId: uint;
		glyph: pFT_GlyphSlot;
		gm: pFT_Glyph_Metrics;
		bm, sbm: pFT_Bitmap;
		sglyphbm: pFT_BitmapGlyph;
		dx, dy: sint;
	{$ifdef DebugPerformance} t: tTimer; {$endif}
	begin
		result := no;
		if not OK then exit;
		ra.im.Invalidate;
		sglyphbm := nil;

	{$ifdef Debug} LogR('Загрузка символа {0}{1}{2} (U+{3}) из {4}... ',
	                    Quote(sym), UTF8.CodepointToString(sym), Quote(sym), ToString(sym, IntFormat.Hex), StreamPath.Log(_fn)); {$endif}
		glyphId := FT_Get_Char_Index(face, sym);
		if glyphId = 0 then
		begin
		{$ifdef Debug} Log('Символ не найден.', logWarning); {$endif}
			goto _finally_;
		end {$ifdef Debug} else LogR('Индекс: ' + ToString(glyphId) + '; ', logDebug) {$endif};

	{$ifdef DebugPerformance} t.Start; {$endif}
		r := FT_Load_Glyph(face, glyphId, FT_LOAD_NO_SCALE);
		if r <> 0 then
		begin
		{$ifdef Debug} Log('Ошибка при загрузке символа (NO_SCALE): ' + FTErrorDesc(r), logError); {$endif}
			goto _finally_;
		end;
	{$ifdef DebugPerformance} LogR('Время первой (NO_SCALE) загрузки символа (freetype): ' + ToString(t.Time * 1.0e6) + ' мкс; ', logDebug); {$endif}

		glyph := face^.glyph;
		gm := @glyph^.metrics;

	{$ifdef Debug}
		LogR('Размеры = (' + ToString(gm^.width) + ', ' + ToString(gm^.height) + ') ед.; по горизонтали: ' +
			'смещение = (' + ToString(gm^.horiBearingX) + ', ' + ToString(gm^.horiBearingY) + ') ед., сдвиг курсора ' + ToString(gm^.horiAdvance) + ' ед.; ' +
			'по вертикали: ' +
			'смещение = (' + ToString(gm^.vertBearingX) + ', ' + ToString(gm^.vertBearingY) + ') ед., сдвиг курсора ' + ToString(gm^.vertAdvance) + ' ед.; ', logDebug);
	{$endif}
		ra.imShift := Vec2.Make(gm^.horiBearingX, gm^.horiBearingY) * invUpEM - 0.5 * Vec2.Make(opts.outlineSize);
		ra.cursorShift := gm^.horiAdvance * invUpEM;
		ra.sizes := Vec2.Make(gm^.width, gm^.height) * invUpEM + Vec2.Make(opts.outlineSize);

		r := FT_Set_Pixel_Sizes(face, iceil(resolution), 0);
		if r <> 0 then
		begin
		{$ifdef Debug} Log('Ошибка при установке размеров символа (разрешение: ' + ToString(resolution) + '): ' + FTErrorDesc(r), logError); {$endif}
			goto _finally_;
		end;

		sbm := nil;
		if opts.outlineSize > 0.0 then
		begin
			r := FT_Load_Glyph(face, glyphId, 0);
			if r <> 0 then
			begin
			{$ifdef Debug} Log('Ошибка при загрузке символа для контура: ' + FTErrorDesc(r), logError); {$endif}
				goto _finally_;
			end;
			if not _RasterizeOutline(opts, face^.glyph, resolution, sglyphbm) then goto _finally_;
			sbm := @sglyphbm^.bitmap;
		{$ifdef Debug} if sbm^.width <> 0 then LogR('размеры обводки: ' + ToString(sbm^.width) + ' x ' + ToString(sbm^.rows) + ' px; ', logDebug); {$endif}
		end;

	{$ifdef DebugPerformance} t.Start; {$endif}
		r := FT_Load_Glyph(face, glyphId, FT_LOAD_RENDER);
		if r <> 0 then
		begin
		{$ifdef Debug} Log('Ошибка при загрузке символа (RENDER): ' + FTErrorDesc(r), logError); {$endif}
			goto _finally_;
		end;
	{$ifdef DebugPerformance} LogR('Время второй (RENDER) загрузки символа (freetype): ' + ToString(t.Time * 1.0e6) + ' мкс; ', logDebug); {$endif}

		glyph := face^.glyph;
		gm := @glyph^.metrics;
		bm := @glyph^.bitmap;
		if not Assigned(sbm) then sbm := bm;

	{$ifdef Debug} if bm^.width <> 0 then LogR('размеры символа: ' + ToString(bm^.width) + ' x ' + ToString(bm^.rows) + ' px; ', logDebug); {$endif}

		if bm^.width <> 0 then
		begin
			ra.im.Init(GLtexture_2D, UintVec2.Make(max(bm^.width, sbm^.width), max(bm^.rows, sbm^.rows)), GLformat_RGBA);
			ra.im.Fill(Vec4.Zero);

			if sbm <> bm then
			begin
				if not _FTBitmap2Texture(sbm, 0, 0, ra.im, opts.outlineColor, img_Transparency) then goto _finally_;
				ra.imShift += 0.5 * Vec2.Make(bm^.width - sbm^.width, sbm^.rows - bm^.rows) / resolution;
			end;
			dx := (sbm^.width - bm^.width + 1) div 2;
			dy := (sbm^.rows - bm^.rows + 1) div 2;
			if not _FTBitmap2Texture(bm, dx, dy, ra.im, opts.baseColor, img_Transparency) then goto _finally_;
		end;

	{$ifdef Debug} Log('Cимвол "' + UTF8.CodepointToString(sym) + '" загружен', logOK); {$endif}
		result := yes;
	_finally_:
		if not result then ra.Done;
		if Assigned(sglyphbm) then FT_Done_Glyph(sglyphbm);
	end;

	function TrueType.Kerning(const a, b: UTFchar): float;
	var
		face: pFT_Face absolute faceptr;
		idA, idB: uint;
		v: FT_Vector;
		r: FT_Result;
	begin
		result := 0.0;
		idA := FT_Get_Char_Index(face, a); if idA = 0 then exit;
		idB := FT_Get_Char_Index(face, b); if idB = 0 then exit;
		r := FT_Get_Kerning(face, idA, idB, FT_KERNING_UNSCALED, v);
		if r <> 0 then
		begin
		{$ifdef Debug}
			Log('Не удалось получить кернинг для пары символов "' +
				UTF8.CodepointToString(a) + '" (#' + ToString(a) + ') и "' + UTF8.CodepointToString(b) + '" (#' + ToString(b) + '): ' + FTErrorDesc(r), logError);
		{$endif}
			exit;
		end;

		result := v.x * invUpEM;
	{$ifdef Debug} if result <> 0.0 then LogR('Кернинговая пара "' + UTF8.CodepointToString(a) + UTF8.CodepointToString(b) + '": ' + ToString(result) + '; ', logDebug); {$endif}
	end;

	procedure DescribeFreetypeFunctions(var fns: DLLoader.FunctionsList);
	begin
		fns
		.Func(@FT_New_Library,         'New_Library')^
		.Func(@FT_Add_Default_Modules, 'Add_Default_Modules')^
		.Func(@FT_Done_Library,        'Done_Library')^
		.Func(@FT_New_Face,            'New_Face')^
		.Func(@FT_New_Memory_Face,     'New_Memory_Face')^
		.Func(@FT_Done_Face,           'Done_Face')^
		.Func(@FT_Set_Pixel_Sizes,     'Set_Pixel_Sizes')^
		.Func(@FT_Get_Char_Index,      'Get_Char_Index')^
		.Func(@FT_Load_Glyph,          'Load_Glyph')^
		.Func(@FT_Outline_Render,      'Outline_Render')^
		.Func(@FT_Get_Glyph,           'Get_Glyph')^
		.Func(@FT_Done_Glyph,          'Done_Glyph')^
		.Func(@FT_Stroker_New,         'Stroker_New')^
		.Func(@FT_Stroker_Done,        'Stroker_Done')^
		.Func(@FT_Stroker_Set,         'Stroker_Set')^
		.Func(@FT_Glyph_Stroke,        'Glyph_Stroke')^
		.Func(@FT_Glyph_To_Bitmap,     'Glyph_To_Bitmap')^
		.Func(@FT_Get_Kerning,         'Get_Kerning');
	end;

	procedure Init;
	begin
		loader.Init('freetype(prefix = FT_, lock)', @DescribeFreetypeFunctions);
		loader.Hook(+0).AfterLoad(@AfterLoad).BeforeUnload(@BeforeUnload);
	end;

	procedure Done;
	begin
		loader.Done;
	end;

initialization
	&Unit('FreeType').Initialize(@Init, @Done);
end.
