{$include opts.inc}
unit rox_decoration;

interface

uses
	USystem, UMath, GLUtils, rox_gfx, rox_gl, rox_location;

const
	Deduce = NegInfinity;

type
	pDecoration = ^Decoration;
	Decoration = object(Node)
		tex: pTexture;
		texRect: Rect;
		animFrames: uint;
		phase, animLen: float;
		oneshot, deducedX, deducedY, additive: boolean;
		alpha: float;
		constructor Init(const tex: string; const local: Transform2; const size: Vec2);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw(const view: Transform2); virtual;
		function SetTexRect(const rect: Rect): pDecoration;
		function SetAnim(const base: Rect; const frames: uint; const len: float; oneshot: boolean): pDecoration;
	private
		procedure ReDeduce;
	end;

implementation

	constructor Decoration.Init(const tex: string; const local: Transform2; const size: Vec2);
	begin
		inherited Init(local, size);
		self.tex := Texture.Load(tex);
		self.texRect := Rect.ZeroOnes;
		deducedX := self.size.x = Deduce;
		deducedY := self.size.y = Deduce;
		alpha := 1;
		ReDeduce;
	end;

	destructor Decoration.Done;
	begin
		Release(tex);
		inherited Done;
	end;

	procedure Decoration.HandleUpdate(const dt: float);
	begin
		if animFrames > 0 then
		begin
			phase += dt;
			if phase >= animLen then
				if oneshot then Detach else phase := modf(phase, animLen);
		end;
	end;

	procedure Decoration.HandleDraw(const view: Transform2);
	var
		q: Quad;
		shift, frame: float;
	begin
		q.fields := [q.Field.Transform];
		if Assigned(parent) then q.transform := view * parent^.local * local else q.transform := view * local;
		if additive then gl.BlendFunc(gl.SRC_ALPHA, gl.ONE);
		if alpha <> 1 then begin q.fields += [q.Field.Color]; q.color := Vec4.Make(1, 1, 1, alpha); end;
		if animFrames = 0 then
			q.Draw(tex, Vec2.Zero, size, texRect.A, texRect.Size)
		else
		begin
			frame := floor(phase / max(animLen, 0.1) * animFrames);
			if frame >= animFrames then
				if oneshot then frame := animFrames - 1 else frame := 0;
			shift := texRect.Size.X * frame;
			q.Draw(tex, Vec2.Zero, size, texRect.A + Vec2.Make(shift, 0), texRect.Size);
		end;
		if additive then gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
	end;

	function Decoration.SetTexRect(const rect: Rect): pDecoration;
	begin
		texRect := rect;
		ReDeduce;
		result := @self;
	end;

	function Decoration.SetAnim(const base: Rect; const frames: uint; const len: float; oneshot: boolean): pDecoration;
	begin
		texRect := base;
		animFrames := frames;
		animLen := len;
		self.oneshot := oneshot;
		ReDeduce;
		result := @self;
	end;

	procedure Decoration.ReDeduce;
	begin
		if deducedX then self.size.x := self.tex^.ap.Fixed(texRect.Aspect).Aspect2Item(asp2_y1, 0, size.y);
		if deducedY then self.size.y := self.tex^.ap.Fixed(texRect.Aspect).Aspect2Item(asp2_x1, 1, size.x);
	end;

end.
