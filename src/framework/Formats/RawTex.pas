{$include opts.inc}
unit RawTex;

interface

uses
	USystem, utils, Errors, UMath, Streams, UClasses, U_GL, GLBase;

implementation

const
	Signature = 'tex';
	TargetPrefixCodes: array[GLTextureTarget] of string = ('I', '+', 'Y', 'T');
	FMT_BITS = bitsizeof(uint8) - 1;  FMT_MASK = 1 shl FMT_BITS - 1;
	MULTILEVEL_BITN = FMT_BITS;       MULTILEVEL_BIT = 1 shl MULTILEVEL_BITN;

	procedure Save(var tex: TextureImage; s: pStream);
	var
		i: sint;
	begin
		MakeRef(s);
		try
			Serialize_signature(s, Signature);
			Serialize_enum(s, ord(tex.target), TargetPrefixCodes);
			Serialize_ui8(s,
				RangeCheck(uint(ord(tex.format)), FMT_MASK, 'Format') or
				uint(tex.nLevels > TextureTargetsInfo[tex.target].faces) shl MULTILEVEL_BITN);

			for i := 0 to TextureTargetsInfo[tex.target].determinativeDims-1 do
				VarInt.Write(s, tex.info.size.data[i]);

			for i := 0 to tex.nLevels - 1 do
				s^.Write(tex.LevelPtr(i), tex.info.GetLevelDataSize(i));
		finally
			Release(s);
		end;
	end;

	procedure Load(var im: TextureImage; s: pStream);
	var
		fmtx: uint;
		i: uint;
		target: GLTextureTarget;
		sizes: UintVec3;
		format: GLImageFormat;
		flags: TextureImageFlags;
	begin
		flags := [];
		Deserialize_signature(s, Signature, no);
		target := GLTextureTarget(Deserialize_enum(s, TargetPrefixCodes));
		fmtx := Deserialize_ui8(s);
		if fmtx and MULTILEVEL_BIT <> 0 then begin flags += [texture_Mips]; fmtx := fmtx xor MULTILEVEL_BIT; end;
		format := GLImageFormat(RangeCheck(fmtx and FMT_MASK, ord(High(format)), 'RawTex.format'));

		for i := 0 to High(sizes.data) do
			if i < TextureTargetsInfo[target].determinativeDims then sizes.data[i] := VarInt.Read(s) else sizes.data[i] := 1;

		im.Prepare(target, sizes, format, flags);
		for i := 0 to im.nLevels - 1 do
			s^.Read(im.LevelPtr(i), im.info.GetLevelDataSize(i));
	end;

	procedure SuiteSave(obj: pointer; s: pStream); begin Save(pTextureImage(obj)^, s); end;
	procedure SuiteLoad(obj: pointer; s: pStream); begin Load(pTextureImage(obj)^, s); end;
	procedure Init;
	begin
		TextureImage.Loaders.Register('tex', @SuiteLoad, @SuiteSave);
	end;

initialization
	&Unit('RawTex').Initialize(@Init).Priority(-1);
end.

