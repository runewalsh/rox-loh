{$include opts.inc}
unit Diffed3DTexture;

interface

uses
	USystem, Errors, Utils, UMath, UClasses, Streams, U_GL, GLBase;

implementation

uses
	PNG;

const
	Signature = 'png-diff';
	FORMAT_EXTRA_BITS = 1;
	MIPS_BITN = 0;

	function Pack(x: sint): uint8;
	begin
		if x > 0 then
			result := 128 + x div 2
		else
			result := (x + 254) div 2;
	end;

	function Unpack(x: uint8): sint;
	begin
		if x > 128 then result := 2*(x-128) else
			if x < 127 then result := 2*x - 253 else
				if x = 128 then result := 1 else
					result := 0;
	end;

	procedure WriteSizeBeforeData(stream: pStream; size: size_t; param: pointer);
	begin
		Assert(@param = @param);
		Serialize_ui32(stream, RangeCheck(size, High(uint32), 'PNGSize'));
	end;

	procedure Save(const im: TextureImage; f: pStream);
		function Unsupported(const what: string): Exception;
		begin
			result := Error('Diffed3DTexture не поддерживает {0}.', what);
		end;

	var
		temp, replica: TextureImage;
		plane: size_t;
		lv, z, c, nc, planeItems: uint;
		srcp, dstp, replp: pointer;
	begin
		try
			if im.target <> GLtexture_3D then raise Unsupported(GLTextureTargetIds[im.target]);
			nc := GLImageFormatsInfo[im.format].nChannels;
			if GLImageFormatsInfo[im.format].pixelSize <> nc * sizeof(uint8) then raise Unsupported('не-8-битные форматы.');

			Serialize_conststring(f, Signature);
			VarInt.Write(f, im.Size.Z);
			Serialize_ui8(f, RangeCheck(uint(ord(im.format)), High(uint8) shr FORMAT_EXTRA_BITS, 'Format') shl FORMAT_EXTRA_BITS
				or uint(texture_Mips in im.info.flags) shl MIPS_BITN);
			for lv := 0 to im.nLevels - 1 do
			begin
				temp.Invalidate;
				replica.Invalidate;
				PNG.Save(f, im.info.LevelSizeXY(lv), ImageFormat8[nc], im.LevelPtr(lv), Uber, @WriteSizeBeforeData, f);
				if im.info.GetLevelDimension(lv, 2) = 1 then continue;

				try
					plane := im.info.PlaneSize(lv);
					planeItems := nc * im.info.LevelSizeXY(lv).Product;
					temp.Init(UintVec2.Make(im.info.GetLevelDimension(lv, 0), im.info.GetLevelDimension(lv, 1) * (im.info.GetLevelDimension(lv, 2) - 1)),
						im.format);
					replica.Init(im.info.LevelSizeXY(lv), im.Format);
					memcpy(im.LevelPtr(lv), replica.FirstLevel, plane);

					srcp := im.LevelPtr(lv) + plane;
					dstp := temp.FirstLevel;

					for z := 2 to im.info.GetLevelDimension(lv, 2) do
					begin
						replp := replica.FirstLevel;
						c := 0;
						while c < planeItems do
						begin
							pUint8(dstp)[c] := Pack(sint(pUint8(srcp)[c]) - pUint8(replp)[c]);
							pUint8(replp)[c] := sint(pUint8(replp)[c]) + Unpack(pUint8(dstp)[c]);
							inc(c);
						end;

						pUint8(srcp) += planeItems;
						pUint8(dstp) += planeItems;
						pUint8(replp) += planeItems;
					end;

					PNG.Save(f, temp.Size.XY, ImageFormat8[nc], temp.FirstLevel, Uber, @WriteSizeBeforeData, f);
				finally
					replica.Done;
					temp.Done;
				end;
			end;
		finally
			Release(f);
		end;
	end;

	procedure Load(const im: TextureImage; f: pStream);
		function ReadPngSize(f: pStream): size_t;
		begin
			result := Deserialize_ui32(f);
			if FileSize.Explicit(result) > f^.Size - f^.Position then raise Error('Неверный размер PNG.');
		end;
	var
		depth: uint;
		fmtx, lv, z, c, nc, planeItems: uint;
		fmt: GLImageFormat;
		src: TextureImage;
		plane: size_t;
		srcp, dstp, dstprev: pointer;
		expectedSize: UintVec2;
		flags: TextureImageFlags;
	begin
		try
			Deserialize_signature(f, Signature, no);
			depth := VarInt.Read(f); if depth = 0 then raise Error('Неверная глубина.');
			fmtx := Deserialize_ui8(f);
			fmt := GLImageFormat(RangeCheck(fmtx shr FORMAT_EXTRA_BITS, ord(High(GLImageFormat)), 'Format'));
			nc := GLImageFormatsInfo[fmt].nChannels;
			if GLImageFormatsInfo[fmt].pixelSize <> nc * sizeof(uint8) then raise Error('Недопустимый формат DiffedPNG.');
			flags := [];
			if fmtx and (1 shl MIPS_BITN) <> 0 then flags += [texture_Mips];

			lv := 0;
			repeat
				src.Init(f, 'png', ReadPngSize(f));

				try
					if GLImageFormatsInfo[src.format].pixelSize <> GLImageFormatsInfo[fmt].pixelSize then
						raise Error('Неверный формат картинки ({0} <-> {1}).', GLImageFormatIds[src.format], GLImageFormatIds[fmt]);
					if lv = 0 then
						im.Prepare(GLtexture_3D, UintVec3.Make(src.Size.X, src.Size.Y, depth), fmt, flags)
					else
						if src.size.XY <> im.info.LevelSizeXY(lv) then
							raise Error('Неверные размеры оригинальной картинки: {0}, ожидаются {1} (level = {2}).', [SizeToString(src.size), SizeToString(im.info.LevelSize(lv)), lv]);

					plane := im.info.PlaneSize(lv);
					memcpy(src.FirstLevel, im.LevelPtr(lv), plane);
				finally
					src.Done;
				end;

				if im.info.GetLevelDimension(lv, 2) > 1 then
				begin
					src.Init(f, 'png', ReadPngSize(f));
					try
						expectedSize := UintVec2.Make(im.info.GetLevelDimension(lv, 0), im.info.GetLevelDimension(lv, 1) * (im.info.GetLevelDimension(lv, 2) - 1));
						if src.Size.XY <> expectedSize then
							raise Error('Неверные размеры диффа для уровня {0}: ожидаются {1}, получены {2}', SizeToString(expectedSize), SizeToString(src.size));

						dstprev := im.LevelPtr(lv);
						srcp := src.FirstLevel;
						dstp := dstprev + plane;
						planeItems := nc * im.info.LevelSizeXY(lv).Product;
						for z := 2 to im.info.GetLevelDimension(lv, 2) do
						begin
							c := 0;
							while c < planeItems do
							begin
								pUint8(dstp)[c] := sint(pUint8(dstprev)[c]) + Unpack(pUint8(srcp)[c]);
								inc(c);
							end;
							pUint8(dstprev) += planeItems;
							pUint8(srcp) += planeItems;
							pUint8(dstp) += planeItems;
						end;
					finally
						src.Done;
					end;
				end;
				inc(lv);
			until lv >= im.nLevels;
		finally
			Release(f);
		end;
	end;

	procedure SuiteSave(obj: pointer; stream: pStream); begin Save(pTextureImage(obj)^, stream^.NewRef); end;
	procedure SuiteLoad(obj: pointer; stream: pStream); begin Load(pTextureImage(obj)^, stream^.NewRef); end;
	procedure Init;
	begin
		TextureImage.Loaders.Register('png-diff', @SuiteLoad, @SuiteSave);
	end;

initialization
	&Unit('Diffed3DTexture').Initialize(@Init);
end.
