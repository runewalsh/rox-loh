	procedure DecompressDXT(data: pBlockType; const size: UintVec2; output: pColorType);

		procedure DecompressRest(const block: BlockType; output: pColorType; restX, restY: uint; const offsets, tempOffsets: OutputOffsets);
		type
			Color = ColorType;
		var
			temp: packed array[0 .. 15] of Color; {$if sizeof(temp) <> 4*4*sizeof(Color)} {$error wrong 4x4xRGB(A) size} {$endif}
			i: uint;
		begin
			DecompressBlock(block, @temp[0], tempOffsets);
			for i := 0 to 15 do
				if (i div 4 < restY) and (i mod 4 < restX) then
					pColorType(pointer(output) + offsets[i])^ := temp[i];
		end;

		procedure DecompressRest(data: pBlockType; sizeX, sizeY: uint; output: pColorType; const offsets: OutputOffsets);
		var
			tempOffsets: OutputOffsets;
			x, y: uint;
		begin
			// data и output указывают в место, отмеченное X:
			//  ---- ---- ??
			// |    |    |??
			// |    |    |??
			//  ---- ---- ??
			// |    |    |??
			// |    |    |??
			//  ---- ---- ??
			//  X ??????????
			// где квадраты — уже прочитанные полные блоки, а ? — недочитанные края.

			BuildOffsets(tempOffsets, sizeof(output^), 4);
			if sizeX mod 4 > 0 then
				for y := 1 to sizeY div 4 do
					DecompressRest((data - (sizeX + 3) div 4 * y)[sizeX div 4], output - 4*sizeX*y + 4*(sizeX div 4), sizeX mod 4, 4, offsets, tempOffsets);

			if sizeY mod 4 > 0 then
				for x := 1 to sizeX div 4 do
					DecompressRest(data[x-1], output + 4*(x-1), 4, sizeY mod 4, offsets, tempOffsets);

			if (sizeX mod 4 > 0) and (sizeY mod 4 > 0) then
				DecompressRest(data[sizeX div 4], output + 4*(sizeX div 4), sizeX mod 4, sizeY mod 4, offsets, tempOffsets);
		end;

	var
		y, x: uint;
		offsets: OutputOffsets;
	begin
		BuildOffsets(offsets, sizeof(output^), size.x);

		// Цикл по полным блокам.
		if (size.x >= 4) and (size.y >= 4) then
			for y := size.y div 4 - 1 downto 0 do
			begin
				for x := size.x div 4 - 1 downto 0 do
					DecompressBlock(data[x], output + 4*x, offsets);
				data += (size.x + 3) div 4;
				output += 4 * size.x;
			end;

		if (size.x mod 4 > 0) or (size.y mod 4 > 0) then
			DecompressRest(data, size.x, size.y, output, offsets);
	end;

{$undef DecompressDXT} {$undef BlockType} {$undef pBlockType} {$undef ColorType} {$undef pColorType} {$undef DecompressBlock}
