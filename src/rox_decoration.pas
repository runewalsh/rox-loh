{$include opts.inc}
unit rox_decoration;

interface

uses
	USystem, UMath, rox_gfx, rox_location;

type
	pDecoration = ^Decoration;
	Decoration = object(Node)
		tex: pTexture;
		texRect: Rect;
		constructor Init(const tex: string; const local: Transform2; const size: Vec2);
		destructor Done; virtual;
		procedure Update(const dt: float); virtual;
		procedure Draw(const view: Transform2); virtual;
	end;

implementation

	constructor Decoration.Init(const tex: string; const local: Transform2; const size: Vec2);
	begin
		inherited Init(local, size);
		self.tex := Texture.Load(tex);
		self.texRect := Rect.ZeroOnes;
	end;

	destructor Decoration.Done;
	begin
		Release(tex);
		inherited Done;
	end;

	procedure Decoration.Update(const dt: float);
	begin
		Assert(@dt = @dt);
	end;

	procedure Decoration.Draw(const view: Transform2);
	var
		q: Quad;
	begin
		q.fields := [q.Field.Transform];
		q.transform := view * local;
		q.Draw(tex, Vec2.Zero, size, texRect.A, texRect.B);
	end;

end.
