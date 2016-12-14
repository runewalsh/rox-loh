{$include opts.inc}
unit rox_dialogue;

interface

uses
	USystem, UClasses, U_GL, GLBase, GLUtils, rox_gfx, rox_ui;

type
	TextBox = object(Control)
		syms: array of TextureImage;
		built: TextureImage;

		constructor Init(const src: string);
		destructor Done; virtual;
		procedure Update(const dt: float);
		procedure Draw; virtual;
	private
		procedure Prepare(const src: TextureImage);
		procedure ScanRow(img: pUint8; x, y, w, h: uint);
		procedure FloodFill(img, result: pUint8; x, y, w, h: uint; out left, top, filledWidth, filledHeight: uint);
	end;

implementation

	constructor TextBox.Init(const src: string);
	var
		si: pImageResource;
	begin
		built.Invalidate;
		syms := nil;
		inherited Init(nil, []);

		si := ResourcePool.Shared^.LoadRef(TypeOf(ImageResource), Paths.Data + src);
		try
			Prepare(si^.im);
		finally
			Release(si);
		end;
	end;

	destructor TextBox.Done;
	var
		i: sint;
	begin
		for i := 0 to High(syms) do
			syms[i].Done;
		built.Done;
		inherited Done;
	end;

	procedure TextBox.Update(const dt: float);
	begin
		inherited Update(dt);
	end;

	procedure TextBox.Draw;
	begin
	end;

	procedure TextBox.Prepare(const src: TextureImage);
	var
		x, y: uint;
		img: pUint8;
	begin
		if src.format <> GLformat_R then
			raise Error('Текст ожидается в grayscale.');

		img := nil;
		try
			img := GetMem(src.info.PlaneSize(0));
			memcpy(src.FirstLevel, img, src.info.PlaneSize(0));

			y := 0;
			while y < src.size.y do
			begin
				x := 0;
				while x < src.size.x do
				begin
					if img[y*src.size.y + x] > 128 then
					begin
						ScanRow(img, x, y, src.size.x, src.size.y);
						break;
					end;
					inc(x);
				end;
				inc(y);
			end;
		finally
			FreeMem(img);
		end;
	end;

	procedure TextBox.ScanRow(img: pUint8; x, y, w, h: uint);
	begin

	end;

	procedure TextBox.FloodFill(img, result: pUint8; x, y, w, h: uint; out left, top, filledWidth, filledHeight: uint);
	begin
	end;

end.

