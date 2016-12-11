{$include opts.inc}
unit DXT;

interface

uses
	USystem, Utils, UMath, U_GL;

	procedure Decompress(data: pointer; dxf: GLImageFormat; const size: UintVec2; output: pointer);
	procedure Flip(src, dst: pointer; dxf: GLImageFormat; const size: UintVec2);
	function DetectDXT1A(data: pointer; const size: UintVec2): boolean;
	function DumpBlock(data: pointer; x, y, sizeX: uint; format: GLImageFormat): string;

	procedure RGB565_to_RGB8(v565: uint; out rgb: Vec3u8);
	procedure RGB565_to_RGBA8(v565: uint; out rgba: Vec4u8);

implementation

	function NotADXT(format: GLImageFormat): Exception;
	begin
		result := Error('Не DXT: ' + GLImageFormatIds[format]);
	end;

	function CantFlip(const size: UintVec2): Exception;
	begin
		result := Error('Изображение {0}x{1} не может быть перевёрнуто без потерь.', [size.x, size.y]);
	end;

type
	FourRGB = array[0 .. 3] of Vec3u8;
	FourRGBA = array[0 .. 3] of Vec4u8;

	pDXT1Block = ^DXT1Block;
	DXT1Block = packed record
	case uint of
		0: (color0, color1: uint16);
		1: (colors: uint32;
			bits: uint32);
	end; {$if sizeof(DXT1Block) <> 8} {$error wrong DXT1 block size, should be 8 bytes} {$endif}

	DXT5AlphaIndices = packed array[0 .. 5] of byte;

	pDXT5Block = ^DXT5Block;
	DXT5Block = packed record
	case uint of
		0: (alpha0, alpha1: uint8);
		1: (alphas: uint16;
			alphaIndices: DXT5AlphaIndices;

   	case uint of
   		0: (color0, color1: uint16);
   		1: (colors: uint32;
   			bits: uint32));
	end; {$if sizeof(DXT5Block) <> 16} {$error wrong DXT5 block size, should be 16 bytes} {$endif}

	// 16 смещений пикселей относительно выходного указателя
	OutputOffsets = array[0 .. 15] of size_t;

	procedure BuildOffsets(out ofs: OutputOffsets; base: size_t; sizeX: uint);
	var
		x, y: uint;
	begin
		for y := 0 to 3 do
			for x := 0 to 3 do
				ofs[4*y+x] := base*(y*sizeX + x);
	end;

	procedure DecodeColors(a, b: uint; out colors: FourRGB);
	begin
		RGB565_to_RGB8(a, colors[0]);
		RGB565_to_RGB8(b, colors[1]);
		colors[2][0] := (2*colors[0][0] + colors[1][0]) div 3;
		colors[2][1] := (2*colors[0][1] + colors[1][1]) div 3;
		colors[2][2] := (2*colors[0][2] + colors[1][2]) div 3;
		colors[3][0] := (colors[0][0] + 2*colors[1][0]) div 3;
		colors[3][1] := (colors[0][1] + 2*colors[1][1]) div 3;
		colors[3][2] := (colors[0][2] + 2*colors[1][2]) div 3;
	end;

	procedure DecodeColors(a, b: uint; out colors: FourRGBA);
	begin
		RGB565_to_RGBA8(a, colors[0]);
		RGB565_to_RGBA8(b, colors[1]);
		colors[2][0] := (colors[0][0] + colors[1][0]) div 2;
		colors[2][1] := (colors[0][1] + colors[1][1]) div 2;
		colors[2][2] := (colors[0][2] + colors[1][2]) div 2;
		colors[2][3] := 255;
		colors[3][0] := colors[2][0];
		colors[3][1] := colors[2][1];
		colors[3][2] := colors[2][2];
		colors[3][3] := 0;
	end;

	procedure DecompressDXT1Block(const block: DXT1Block; output: pVec3u8; const offsets: OutputOffsets);
	var
		colors: FourRGB;
		i: uint;
	begin
		DecodeColors(block.color0, block.color1, colors);
		for i := 0 to 15 do
			pVec3u8(pointer(output) + offsets[i])^ := colors[block.bits shr (2*i) and $3]
	end;

	procedure DecompressDXT1ABlock(const block: DXT1Block; output: pVec4u8; const offsets: OutputOffsets);
	var
		colors: FourRGBA;
		i: uint;
	begin
		DecodeColors(block.color0, block.color1, colors);
		for i := 0 to 15 do
			pVec4u8(pointer(output) + offsets[i])^ := colors[block.bits shr (2*i) and $3];
	end;

{$define DecompressDXT := DecompressDXT1}
{$define BlockType := DXT1Block} {$define pBlockType := pDXT1Block}
{$define ColorType := Vec3u8} {$define pColorType := pVec3u8}
{$define DecompressBlock := DecompressDXT1Block}
{$include DecompressDXT.inc}

{$define DecompressDXT := DecompressDXT1A}
{$define BlockType := DXT1Block} {$define pBlockType := pDXT1Block}
{$define ColorType := Vec4u8} {$define pColorType := pVec4u8}
{$define DecompressBlock := DecompressDXT1ABlock}
{$include DecompressDXT.inc}

	function FlipLowHeightBits(bits: uint32; height: uint): uint32;
	begin
		bits := LEtoN(bits);
		case height of
			2: result := uint32(System.Swap(Lo(bits)) or Hi(bits) shl 16);
			3: result := bits and uint32($FF) shl 16 or bits and uint32($FF00) or bits and uint32($FF0000) shr 16 or bits and uint32($FF000000);
			else Assert(no);
		end;
		result := NtoLE(result);
	end;

	procedure FlipDXT1(src, dst: pointer; const size: UintVec2);
	// Цвета достаточно просто переставить в симметричный блок, а коэффициентам — ещё и свапнуть endianness как 4-байтному числу
	// (т. к. их ровно по байту на строку блока).
	//
	// sizeX mod 4 <> 0 флипается без изменений.
	// sizeY mod 4 <> 0 можно флипнуть, только если в высоту ровно 1 блок.

		procedure SwapRows(a, b: pDXT1Block; nBlocks: uint);
		var
			i: uint;
			t: uint32;
		begin
			for i := 0 to nBlocks - 1 do
			begin
				t := a[i].colors; a[i].colors := b[i].colors; b[i].colors := t;
				t := a[i].bits; a[i].bits := SwapEndian(b[i].bits); b[i].bits := SwapEndian(t);
			end;
		end;

		procedure FixupCenterRow(a: pDXT1Block; nBlocks: uint);
		var
			i: uint;
		begin
			for i := 0 to nBlocks - 1 do
				a[i].bits := SwapEndian(a[i].bits);
		end;

		procedure SwapTo(a, b: pDXT1Block; nBlocks: uint);
		var
			i: uint;
		begin
			for i := 0 to nBlocks - 1 do
			begin
				b[i].colors := a[i].colors;
				b[i].bits := SwapEndian(a[i].bits);
			end;
		end;

		procedure FlipLowHeight(a, b: pDXT1Block; nBlocks: uint; height: uint);
		var
			i: uint;
		begin
			for i := 0 to nBlocks - 1 do
			begin
				b[i].colors := a[i].colors;
				b[i].bits := FlipLowHeightBits(a[i].bits, height);
			end;
		end;

		procedure EvenFlip(a, b: pointer; nBlocks: uint; sizeY: uint);
		var
			rowSize: size_t;
		begin
			rowSize := nBlocks * sizeof(DXT1Block);
			b += rowSize * (sizeY div 4);
			repeat
				b -= rowSize;
				if (b < dst) or (a = b) then
				begin
					if a = b then FixupCenterRow(a, nBlocks);
					break;
				end;
				if src = dst then SwapRows(a, b, nBlocks) else SwapTo(a, b, nBlocks);
				a += rowSize;
			until a = b;
		end;

	begin
		if size.y mod 4 > 0 then
		begin
			if size.y >= 4 then raise CantFlip(size);
			if size.y > 1 then FlipLowHeight(src, dst, (size.x + 3) div 4, size.y);
		end else
			EvenFlip(src, dst, (size.x + 3) div 4, size.y);
	end;

	procedure DecompressDXT5Block(const block: DXT5Block; output: pointer; const offsets: OutputOffsets);
	var
		alphaIndices: array[0 .. 1] of uint;
		alphas: array[0 .. 7] of uint8;
		i: uint;
		colors: FourRGB;
		color: pVec4u8;
	begin
		alphas[0] := block.alpha0;
		alphas[1] := block.alpha1;
		if block.alpha0 > block.alpha1 then
		begin
			alphas[2] := (6*block.alpha0 + 1*block.alpha1) div 7;
			alphas[3] := (5*block.alpha0 + 2*block.alpha1) div 7;
			alphas[4] := (4*block.alpha0 + 3*block.alpha1) div 7;
			alphas[5] := (3*block.alpha0 + 4*block.alpha1) div 7;
			alphas[6] := (2*block.alpha0 + 5*block.alpha1) div 7;
			alphas[7] := (1*block.alpha0 + 6*block.alpha1) div 7;
		end else
		begin
			alphas[2] := (4*block.alpha0 + 1*block.alpha1) div 5;
			alphas[3] := (3*block.alpha0 + 2*block.alpha1) div 5;
			alphas[4] := (2*block.alpha0 + 3*block.alpha1) div 5;
			alphas[5] := (1*block.alpha0 + 4*block.alpha1) div 5;
			alphas[6] := 0;
			alphas[7] := 255;
		end;
		alphaIndices[0] := LEtoN(pUint32(unaligned(@block.alphaIndices[0]))^); // можно объявить alphaIndices в блоке как bitpacked[0 .. 15] of 0 .. 2
		alphaIndices[1] := LEtoN(pUint32(unaligned(@block.alphaIndices[3]))^); // но bitpacked почему-то медленнее :(
		// ^ внимание, получаются по 3 младших значащих байта и 1 с мусором
		DecodeColors(block.color0, block.color1, colors);

		for i := 0 to 15 do
		begin
			color := output + offsets[i];
			pVec3u8(color)^ := colors[block.bits shr (2 * i) and $3];
			color^[3] := alphas[alphaIndices[i div 8] shr (3 * (i mod 8)) and %111];
		end;
	end;

{$define DecompressDXT := DecompressDXT5}
{$define BlockType := DXT5Block} {$define pBlockType := pDXT5Block}
{$define ColorType := Vec4u8} {$define pColorType := pVec4u8}
{$define DecompressBlock := DecompressDXT5Block}
{$include DecompressDXT.inc}

	procedure FlipDXT5(src, dst: pointer; const size: UintVec2);

	// Не считая коэффициентов альфы, переворачивается по тому же принципу, что DXT1.
	//
	// Как должны перевернуться коэффициенты альфы?
	// Было (подчёркивания относятся к предшествуюшим цифрам, младшие биты — слева):
	//    0        1        2        3        4        5
	// (1-я  строка)(2-я  строка) (3-я  строка)(4-я  строка)
	// 00011122 23334445 55666777 88899910 _11_12_1 3_14_15_
	// XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX
	//
	// Должно стать:
	// 12_13_14 _15_8889 9910_11_ 44455566 67770001 11222333
	// XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX
	// (1-я  строка)(2-я  строка) (3-я  строка)(4-я  строка)
	//    0        1        2        3        4        5
	//
	// Правила для нестандартных сторон те же, что у DXT1.

	unchecked
		procedure SwapTo(const src: DXT5AlphaIndices; out dst: DXT5AlphaIndices);
		var
			a, b: uint32;
		begin
			a := LEtoN(pUint32(unaligned(@src[0]))^);
			b := LEtoN(pUint32(unaligned(@src[3]))^);
			pUint32(unaligned(@dst[0]))^ := NtoLE(b shr 12 and uint32(%111111111111) or b and uint32(%111111111111) shl 12 or a shr 12 shl 24);
			pUint16(unaligned(@dst[4]))^ := NtoLE(a shr 20 and uint32(%1111) or a shl 4);
		end;
	end_unchecked

		procedure SwapRows(a, b: pDXT5Block; nBlocks: uint);
		var
			i: uint;
			t: uint32;
			ta: DXT5AlphaIndices;
		begin
			for i := 0 to nBlocks - 1 do
			begin
				t := a[i].alphas; a[i].alphas := b[i].alphas; b[i].alphas := t;
				ta := a[i].alphaIndices; SwapTo(b[i].alphaIndices, a[i].alphaIndices); SwapTo(ta, b[i].alphaIndices);
				t := a[i].colors; a[i].colors := b[i].colors; b[i].colors := t;
				t := a[i].bits; a[i].bits := SwapEndian(b[i].bits); b[i].bits := SwapEndian(t);
			end;
		end;

		procedure FixupCenterRow(a: pDXT5Block; nBlocks: uint);
		var
			i: uint;
			ta: DXT5AlphaIndices;
		begin
			for i := 0 to nBlocks - 1 do
			begin
				SwapTo(a[i].alphaIndices, ta); a[i].alphaIndices := ta;
				a[i].bits := SwapEndian(a[i].bits);
			end;
		end;

		procedure SwapTo(a, b: pDXT5Block; nBlocks: uint);
		var
			i: uint;
		begin
			for i := 0 to nBlocks - 1 do
			begin
				b[i].alphas := a[i].alphas;
				SwapTo(a[i].alphaIndices, b[i].alphaIndices);
				b[i].colors := a[i].colors;
				b[i].bits := SwapEndian(a[i].bits);
			end;
		end;

		procedure EvenFlip(a, b: pointer; nBlocks: uint; sizeY: uint);
		var
			rowSize: size_t;
		begin
			rowSize := nBlocks * sizeof(DXT5Block);
			b += rowSize * (sizeY div 4);
			repeat
				b -= rowSize;
				if (b < dst) or (a = b) then
				begin
					if a = b then FixupCenterRow(a, nBlocks);
					break;
				end;
				if src = dst then SwapRows(a, b, nBlocks) else SwapTo(a, b, nBlocks);
				a += rowSize;
			until a = b;
		end;

		procedure FlipLowHeightAlphaIndices(const src: DXT5AlphaIndices; out dst: DXT5AlphaIndices; height: uint);
		var
			t, a, b: uint32;
		begin
			// 2 пикселя:                                               3 пикселя:
			//    0        1        2        3        4        5           0        1        2        3        4        5
			// (1-я  строка)(2-я  строка) (3-я  строка)(4-я  строка)    (1-я  строка)(2-я  строка) (3-я  строка)(4-я  строка)
			// 00011122 23334445 55666777 88899910 _11_12_1 3_14_15_    00011122 23334445 55666777 88899910 _11_12_1 3_14_15_
			// XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX    XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX
			//
			// 44455566 67770001 11222333 88899910 _11_12_1 3_14_15_    88899910 _11_4445 55666777 00011122 233312_1 3_14_15_
			// XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX    XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX
			// (1-я  строка)(2-я  строка) (3-я  строка)(4-я  строка)    (1-я  строка)(2-я  строка) (3-я  строка)(4-я  строка)
			//    0        1        2        3        4        5           0        1        2        3        4        5
			case height of
				2: begin
						t := LEtoN(pUint32(unaligned(@src[0]))^);
						pUint32(unaligned(@dst[0]))^ := NtoLE(t shr 12 and uint32(%111111111111) or t and uint32(%111111111111) shl 12 or t and uint32($FF000000));
					end;
				3: begin
						a := LEtoN(pUint32(unaligned(@src[0]))^);
						b := LEtoN(pUint32(unaligned(@src[3]))^);
						pUint32(unaligned(@dst[0]))^ := NtoLE(b and uint32(%111111111111) or a and uint32(%111111111111000000000000) or a shl 24);
						dst[4] := a shr 8 and $F or src[4] and $F0;
					end;
				else Assert(no);
			end;
		end;

		procedure FlipLowHeight(a, b: pDXT5Block; nBlocks: uint; height: uint);
		var
			i: uint;
		begin
			for i := 0 to nBlocks - 1 do
			begin
				b[i].alphas := a[i].alphas;
				FlipLowHeightAlphaIndices(a[i].alphaIndices, b[i].alphaIndices, height);
				b[i].colors := a[i].colors;
				b[i].bits := FlipLowHeightBits(a[i].bits, height);
			end;
		end;

	begin
		if size.y mod 4 > 0 then
		begin
			if size.y >= 4 then raise CantFlip(size);
			if size.y > 1 then FlipLowHeight(src, dst, (size.x + 3) div 4, size.y);
		end else
			EvenFlip(src, dst, (size.x + 3) div 4, size.y);
	end;

	procedure Decompress(data: pointer; dxf: GLImageFormat; const size: UintVec2; output: pointer);
	begin
		case dxf of
			GLformat_RGB_DXT1: DecompressDXT1(data, size, output);
			GLformat_RGBA_DXT1: DecompressDXT1A(data, size, output);
			GLformat_RGBA_DXT5: DecompressDXT5(data, size, output);
			else raise NotADXT(dxf);
		end;
	end;

	procedure Flip(src, dst: pointer; dxf: GLImageFormat; const size: UintVec2);
	begin
		case dxf of
			GLformat_RGB_DXT1, GLformat_RGBA_DXT1: FlipDXT1(src, dst, size);
			GLformat_RGBA_DXT5: FlipDXT5(src, dst, size);
			else raise NotADXT(dxf);
		end;
	end;

	function DetectDXT1A(data: pointer; const size: UintVec2): boolean;
		function DetectDXT1ABlock(block: pDXT1Block): boolean;
		begin
			result := block[0].color0 < block[0].color1;
		end;
	var
		b: pDXT1Block absolute data;
	begin
		result := (size.x >= 4) and (size.y >= 4) and
			// первая, средняя и последняя строчки
			// в общем случае нужно просканировать полностью, хранить информацию извне или просто забить хд.
			(
				DetectDXT1ABlock(data) or
				((size.y >= 8) and DetectDXT1ABlock(b + (size.x div 4) * ((size.y div 4) div 2))) or
				((size.y >= 16) and DetectDXT1ABlock(b + (size.x div 4) * ((size.y div 4) - 1)))
			);
	end;

	function DumpBlock(data: pointer; x, y, sizeX: uint; format: GLImageFormat): string;
	type
		Indices = array[0 .. 15] of uint;
		function DumpRGB565(v565: uint): string;
		var
			rgb: Vec3u8;
		begin
			RGB565_to_RGB8(v565, rgb);
			result := Utils.Format('{0}, {1}, {2}', [rgb[0], rgb[1], rgb[2]]);
		end;

		procedure UnpackColorIndices(bits: uint32; out unpacked: Indices);
		var
			i: uint;
		begin
			for i := 0 to 15 do
				unpacked[i] := bits shr (2*i) and %11;
		end;

		function Dump(const b: DXT1Block): string;
		var
			ci: Indices;
			i: uint;
		begin
			UnpackColorIndices(b.bits, ci);

			result :=
				Utils.Format('Color0 = {0}, Color1 = {1}', [DumpRGB565(b.color0), DumpRGB565(b.color1)]) + EOL +
				'CBits:';

			for i := 0 to 3 do
				result += EOL + Utils.Format('{0} {1} {2} {3}', [ci[4*i], ci[4*i+1], ci[4*i+2], ci[4*i+3]]);
		end;

		function Dump(const b: DXT5Block): string;
		var
			ai, ci: Indices;
			i, x: uint;
			t: uint32;
		begin
			for i := 0 to 1 do
			begin
				t := b.alphaIndices[3*i] or b.alphaIndices[3*i+1] shl 8 or b.alphaIndices[3*i+2] shl 16;
				for x := 0 to 7 do ai[8*i+x] := t shr (3*x) and %111;
			end;
			UnpackColorIndices(b.bits, ci);

			result :=
				Utils.Format('Color0 = {0}, Color1 = {1}' + EOL + 'A0 = {2}, A1 = {3}', [DumpRGB565(b.color0), DumpRGB565(b.color1), b.alpha0, b.alpha1]) + EOL +
				EOL +
				Utils.Format('CBits: {pad:20}ABits:', []);

			for i := 0 to 3 do
				result += EOL + Utils.Format('{0} {1} {2} {3} {pad:20}{4} {5} {6} {7}',
					[ci[4*i], ci[4*i+1], ci[4*i+2], ci[4*i+3], ai[4*i], ai[4*i+1], ai[4*i+2], ai[4*i+3]]);
		end;

	var
		blockIndex: uint;
	begin
		blockIndex := (y div 4) * ((sizeX + 3) div 4) + x div 4;
		result := '';
		if (x mod 4 <> 0) or (y mod 4 <> 0) then ContinueString(result, Utils.Format('X = {0}, Y = {1}', [4*(x div 4), 4*(y div 4)]), EOL);
		case format of
			GLformat_RGB_DXT1, GLformat_RGBA_DXT1: ContinueString(result, Dump(pDXT1Block(data)[blockIndex]), EOL);
			GLformat_RGBA_DXT5: ContinueString(result, Dump(pDXT5Block(data)[blockIndex]), EOL);
			else NotADXT(format);
		end;
	end;

	procedure RGB565_to_RGB8(v565: uint; out rgb: Vec3u8);
	begin
		rgb[0] := (v565 shr 11 * 527 + 23) shr 6;
		rgb[1] := (v565 and $7E0 shr 5 * 259 + 33) shr 6;
		rgb[2] := (v565 and $1F * 527 + 23) shr 6;
	end;

	procedure RGB565_to_RGBA8(v565: uint; out rgba: Vec4u8);
	begin
		rgba[0] := (v565 shr 11 * 527 + 23) shr 6;
		rgba[1] := (v565 and $7E0 shr 5 * 259 + 33) shr 6;
		rgba[2] := (v565 and $1F * 527 + 23) shr 6;
		rgba[3] := 255;
	end;

end.

