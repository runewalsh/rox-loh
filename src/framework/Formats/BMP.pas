unit BMP;

{$include opts.inc}

interface

uses
	USystem, Errors, UClasses, Streams, Utils, UMath, GLBase, U_GL, GLUtils;

implementation

const
	BI_RGB = 0;

type
	FileHeader = packed record
		Signature: array[0 .. 1] of char;
		FileSize: uint32;
		Reserved: uint32;
		ImageOffset: uint32;
	end; {$if sizeof(FileHeader) <> 14} {$error sizeof(FileHeader) <> 14} {$endif}

	BitmapHeaderV1 = packed record
		HeaderSize: uint32;
		Width, Height: uint32;
		Planes: uint16;
		BitCount: uint16;
		Compression: uint32; // BI_RGB
		SizeImage: uint32;
		XPelsPerMeter, YPelsPerMeter: uint32;
		ClrUsed, ClrImportant: uint32;
	end; {$if sizeof(BitmapHeaderV1) <> 40} {$error sizeof(BitmapHeaderV1) <> 40} {$endif}

	procedure Save(const im: TextureImage; stream: pStream);
	const
		Pad: array[0 .. 2] of byte = (0, 0, 0);
	var
		fileHeader: BMP.FileHeader;
		bitmapHeader: BitmapHeaderV1;
		bgr, row: pointer;
		rawBgrRowSize, alignedBgrRowSize: size_t;
	begin
		im.CheckSaveTarget([GLtexture_2D, GLtexture_3D, GLtexture_Cube], 'BMP');

		rawBgrRowSize     := GLImageFormatsInfo[GLformat_BGR].pixelSize * im.Size.X;
		alignedBgrRowSize := align(rawBgrRowSize, 4);

		fileHeader.Signature    := 'BM';
		fileHeader.FileSize     := sizeof(fileHeader) + sizeof(bitmapHeader) + alignedBgrRowSize * im.Size.Y;
		fileHeader.Reserved     := 0;
		fileHeader.ImageOffset  := sizeof(fileHeader) + sizeof(bitmapHeader);
		stream^.Write(@fileHeader, sizeof(fileHeader));

		bitmapHeader.HeaderSize := sizeof(bitmapHeader);
		bitmapHeader.Width      := im.size.X;
		bitmapHeader.Height     := im.size.Y;
		bitmapHeader.Planes     := 1;
		bitmapHeader.BitCount   := 24;
		bitmapHeader.Compression := BI_RGB;
		bitmapHeader.SizeImage  := 0;
		bitmapHeader.XPelsPerMeter := 72;
		bitmapHeader.YPelsPerMeter := 72;
		bitmapHeader.ClrUsed    := 0;
		bitmapHeader.ClrImportant := 0;
		stream^.Write(@bitmapHeader, sizeof(bitmapHeader));

		if im.format = GLformat_BGR then
			bgr := im.FirstLevel
		else
		begin
			bgr := GetMem(GetTextureDataSize(im.Size.XY, GLformat_BGR));
			Convert(im.FirstLevel, im.format, bgr, GLformat_BGR, im.Size.XY);
		end;

		try
			row := bgr + im.Size.Y * rawBgrRowSize;
			repeat
				row -= rawBgrRowSize;
				stream^.Write(row, rawBgrRowSize);
				if alignedBgrRowSize > rawBgrRowSize then stream^.Write(@Pad, alignedBgrRowSize - rawBgrRowSize);
			until row = bgr;
		finally
			if bgr <> im.FirstLevel then FreeMem(bgr);
		end;
	end;

	procedure Load(var im: TextureImage; s: pStream);
	var
		fileHeader: BMP.FileHeader;
		bitmapHeader: BitmapHeaderV1;
		rowSize: size_t;
		rowPtr: pointer;
		pad: array[0 .. 2] of byte;
	begin
		s^.Read(@fileHeader, sizeof(fileHeader));
		if fileHeader.Signature <> 'BM' then
			raise Error('Неверная сигнатура.');

		s^.Read(@bitmapHeader, sizeof(bitmapHeader));
		if bitmapHeader.HeaderSize < sizeof(bitmapHeader) then
			raise Error('Неверный размер заголовка.');
		ValidateImageSize(UintVec2.Make(bitmapHeader.Width, bitmapHeader.Height));

		if bitmapHeader.BitCount <> 24 then
			raise Error('{0}-битный формат не поддерживается.', [bitmapHeader.BitCount]);

		if bitmapHeader.Compression <> BI_RGB then
			raise Error('Сжатие не поддерживается.');

		im.Prepare(GLtexture_2D, UintVec2.Make(bitmapHeader.Width, bitmapHeader.Height), GLformat_BGR);
		rowSize := im.info.RowSize;
		rowPtr := im.FirstLevel + rowSize * bitmapHeader.Height;

		RangeCheck(fileHeader.ImageOffset, s^.Position.value, s^.Size.value, 'ImageOffset');
		s^.Position := FilePos.Explicit(fileHeader.ImageOffset);
		repeat
			rowPtr -= rowSize;
			s^.Read(rowPtr, rowSize);
			if rowSize mod 4 <> 0 then s^.Read(@pad, align_howmuch(rowSize, 4));
		until rowPtr = im.FirstLevel;
	end;

	procedure SuiteLoad(obj: pointer; s: pStream); begin Load(pTextureImage(obj)^, s); end;
	procedure SuiteSave(obj: pointer; s: pStream); begin Save(pTextureImage(obj)^, s); end;
	procedure Init;
	begin
		TextureImage.Loaders.Register('bmp', @SuiteLoad, @SuiteSave);
	end;

initialization
	&Unit('BMP').Initialize(@Init).Priority(-3);
end.

