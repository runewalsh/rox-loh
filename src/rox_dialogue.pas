{$include opts.inc}
unit rox_dialogue;

interface

uses
	USystem, UClasses, UMath, Algo, Human, Utils, Streams, U_GL, GLBase, {$ifdef Debug} ULog, {$endif} GLUtils, TextProcessing,
	rox_gl, rox_state, rox_gfx, rox_ui, rox_paths;

type
	pTextBox = ^TextBox;
	TextBox = object(Control)
	type
		SymDesc = object
			data: pUint8;
			pos, size: UintVec2;
			procedure Done;
		end;
	var
		pic: pTexture;
		syms: array of SymDesc;
		nextSym: sint;
		sum: pUint8;
		sumSize: UintVec2;
		letterTimeout: float;
		skip: boolean;

		constructor Init(const src, pic: string);
		destructor Done; virtual;
		procedure Update(const dt: float); virtual;
		procedure Draw; virtual;
		procedure Advance(n: uint);
		function Finished: boolean;

		procedure HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra); virtual;
	private
		procedure Prepare(const src: TextureImage; const errfn: string);
		procedure ScanRowForSymbols(img: pUint8; ya, yb: uint; const size: UintVec2);
		function GuessBorder: float;
		function CalculateTransparentRect(const nvp: Vec2; const border: float): Rect;
	public const
		StartingSymbolThreshold = High(uint8) div 2;
		FloodFillThreshold = (High(uint8) * 6) div 7;
		PicSize = 0.3;
	end;

	pDialogue = ^Dialogue;
	Dialogue = object
	type
		ItemDesc = object
			char, pic, sentence: string;
			size, delay: float;
		end;

		DoneCallback = procedure(param: pointer);
	var
		state: pState;
		items: array of ItemDesc;
		nextItem: sint;
		active: pTextBox;
		finalTimeout: float;
		onDone: DoneCallback;
		param: pointer;

		procedure Invalidate;
		function Valid: boolean;
		procedure Init(state: pState; const scenario: string);
		procedure Done;
		procedure Update(const dt: float);
		function Finished: boolean;
		procedure Skip;
	private
		procedure Parse(const s: string);
	const
		Border = 0.03;
	end;

implementation

	procedure TextBox.SymDesc.Done;
	begin
		FreeMem(data);
	end;

	constructor TextBox.Init(const src, pic: string);
	var
		si: pImageResource;
	begin
		inherited Init(nil, []);
		self.pic := Texture.Load(pic);

		si := ResourcePool.Shared^.LoadRef(TypeOf(ImageResource), src);
		try
			Prepare(si^.im, src);
			sumSize := si^.im.Size.XY;

			ChangeTexture(Texture.Dynamic(GLformat_R {обрабатывается особо, см. Texture.InternalFormat}, sumSize));
			tex^.Swizzle(Swizzle.One, Swizzle.One, Swizzle.One, Swizzle.R);

			sum := GetMem(sumSize.Product * sizeof(uint8));
			Zero(sum, sumSize.Product * sizeof(uint8));
			tex^.Sub(0, 0, sumSize.X, sumSize.Y, GLformat_R, sum);
		finally
			Release(si);
		end;
		letterTimeout := 0.2;
	end;

	destructor TextBox.Done;
	var
		i: sint;
	begin
		for i := 0 to High(syms) do
			syms[i].Done;
		FreeMem(sum);
		Release(pic);
		inherited Done;
	end;

	procedure TextBox.Update(const dt: float);
	begin
		letterTimeout -= dt * (1 + 9*ord(skip));
		while (nextSym < length(syms)) and (letterTimeout < 0) do
		begin
			Advance(1);
			letterTimeout += 0.06;
			if nextSym >= length(syms) then skip := no;
		end;
		inherited Update(dt);
	end;

	procedure TextBox.Draw;
	var
		q: Quad;
		rect: UMath.Rect;
	begin
		q.fields := [q.Field.Color];
		q.color := Vec4.Make(0, 0, 0, 0.5);
		rect := CalculateTransparentRect(pStateManager(ui^._mgr)^.nvp, GuessBorder);
		q.Draw(nil, rect.A, rect.Size, Vec2.Zero, Vec2.Ones);

		inherited Draw;

		q.fields := [q.Field.Transform];
		q.transform := local;
		q.Draw(pic, Vec2.Make(0, rawSize.y + 0.5 * GuessBorder), pic^.ap.Aspect2(asp2_x1, PicSize), Vec2.Zero, Vec2.Ones);
	end;

	procedure TextBox.Advance(n: uint);
	var
		sx, sy: uint;
		sym: ^SymDesc;
		b: uint8;
	begin
		while (n > 0) and (nextSym < length(syms)) do
		begin
			sym := @syms[nextSym]; inc(nextSym); dec(n);

			sy := 0;
			while sy < sym^.size.y do
			begin
				sx := 0;
				while sx < sym^.size.x do
				begin
					b := sym^.data[sy * sym^.size.x + sx];
					if b > 0 then sum[(sym^.pos.y + sy) * sumSize.x + sym^.pos.x + sx] := b;
					inc(sx);
				end;
				inc(sy);
			end;
			if n = 0 then tex^.Sub(0, 0, sumSize.X, sumSize.Y, GLformat_R, sum);
		end;
	end;

	function TextBox.Finished: boolean;
	begin
		result := nextSym >= length(syms);
	end;

	procedure TextBox.HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra);
	begin
		case action of
			MouseLClick:
				if CalculateTransparentRect(pStateManager(ui^._mgr)^.nvp, GuessBorder).Contains(pos) and extra.Handle then
					skip := yes;
		end;
		inherited HandleMouse(action, pos, extra);
	end;

type
	FloodParam = record
		img: pUint8;
		width: uint;
	end;

	function FloodTest(const point: UintVec2; param: pointer): boolean;
	var
		p: ^FloodParam absolute param;
	begin
		result := p^.img[point.y * p^.width + point.x] < TextBox.FloodFillThreshold;
	end;

	procedure TextBox.Prepare(const src: TextureImage; const errfn: string);
	var
		x, y: uint;
		img: pUint8;
		fp: FloodParam;
		f: FloodFill.Result;
	begin
		if src.format <> GLformat_R then
			raise Error('{0}: текст ожидается в grayscale.', StreamPath.Human(errfn));

		img := nil;
		try
			img := GetMem(src.info.PlaneSize(0));
			memcpy(src.FirstLevel, img, src.info.PlaneSize(0));

			// Просматриваем картинку построчно.
			// Если в строке найдено что-то похожее на кусок символа (отмечено >x<):
			//      >x<
			//       x
			//  xx   xxx   xx
			//   xx  x  x x
			//  x x  x  x x
			//  xxx  xxx   xx
			//
			//  то заливкой определяем границы этого символа
			//      >.<
			//       .
			//  xx   ...   xx
			//   xx  .  . x
			//  x x  .  . x
			//  xxx  ...   xx
			//
			// и проверяем всю строку его высоты слева направо.

			y := 0;
			while y < src.size.y do
			begin
				x := 0;
				while x < src.size.x do
				begin
					if img[y * src.size.x + x] <= StartingSymbolThreshold then
					begin
						fp.img := img;
						fp.width := src.size.x;
						FloodFill.FourWay(f, UintVec2.Make(x, y), src.size.XY, @FloodTest, @fp);
						ScanRowForSymbols(img, f.min.y, f.max.y, src.size.XY);
						y := f.max.y;
						f.Done;
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

	procedure TextBox.ScanRowForSymbols(img: pUint8; ya, yb: uint; const size: UintVec2);
	var
		x, y: uint;
		fp: FloodParam;
		f: FloodFill.Result;
		sym: ^SymDesc;
		it: FloodFill.PointSet.Iterator;
		p: pUintVec2;
	begin
		x := 0;
		while x < size.x do
		begin
			y := ya;
			while y <= yb do
			begin
				if img[y * size.x + x] <= StartingSymbolThreshold then
				begin
					fp.img := img;
					fp.width := size.x;
					FloodFill.FourWay(f, UintVec2.Make(x, y), size, @FloodTest, @fp);

					SetLength(syms, length(syms) + 1);
					sym := @syms[High(syms)];
					sym^.pos := f.min;
					sym^.size := f.max - f.min + UintVec2.Ones;
					sym^.data := GetMem(sym^.size.Product * sizeof(uint8));
					Zero(sym^.data, sym^.size.Product * sizeof(uint8));

					it := f.filled.GetIterator;
					while f.filled.Next(it) do
					begin
						p := f.filled.GetKey(it);
						sym^.data[(p^.y - f.min.y) * sym^.size.x + (p^.x - f.min.x)] := High(uint8) - img[p^.y * size.x + p^.x];
						img[p^.y * size.x + p^.x] := High(uint8);
					end;
					f.Done;
				end;
				inc(y);
			end;
			inc(x);
		end;
	end;

	function TextBox.GuessBorder: float;
	begin
		result := local.trans.x - (-pStateManager(ui^._mgr)^.nvp.x);
	end;

	function TextBox.CalculateTransparentRect(const nvp: Vec2; const border: float): Rect;
	begin
		result.A := Vec2.Make(local.trans.x, -nvp.y + border);
		result.B := result.A + Vec2.Make(2 * (nvp.x - border), local.trans.y - (-nvp.y) + rawSize.y - border);
	end;

	procedure Dialogue.Invalidate;
	begin
		nextItem := -1;
		active := nil;
	end;

	function Dialogue.Valid: boolean;
	begin
		result := nextItem >= 0;
	end;

	procedure Dialogue.Init(state: pState; const scenario: string);
	begin
		Invalidate;
		self.state := state;
		items := nil;
		nextItem := 0;
		Parse(scenario);
	end;

	procedure Dialogue.Done;
	begin
		Release(active);
		Invalidate;
	end;

	procedure Dialogue.Update(const dt: float);
	var
		na: pTextBox;
	begin
		if Assigned(active) then
		begin
			if active^.Finished then
			begin
				finalTimeout -= dt;
				if (finalTimeout < 0) or active^.skip then
				begin
					active^.Detach;
					Release(active);
					if Finished and Assigned(onDone) then onDone(param);
				end;
			end;
		end;

		if not Assigned(active) and not Finished then
		begin
			na := new(pTextBox, Init(
				rox_paths.Dialogue(items[nextItem].char, items[nextItem].sentence),
				Face(items[nextItem].char, items[nextItem].pic)))^.NewRef;
			finalTimeout := items[nextItem].delay;

			try
				na^.size := 2 * state^.mgr^.nvp.x * items[nextItem].size - 2 * Border;
				na^.local.trans := -state^.mgr^.nvp + Vec2.Make(Border);
				na^.local.trans.y := max(na^.local.trans.y, -state^.mgr^.nvp.y + Border - na^.CalculateRawSize.y + 0.3);
				inc(nextItem);
				state^.mgr^.ui.Add(na^.NewRef, state^.id);
				active := na;
			except
				Release(na);
				raise;
			end;
		end;
	end;

	function Dialogue.Finished: boolean;
	begin
		result := (nextItem >= length(items)) and not Assigned(active);
	end;

	procedure Dialogue.Skip;
	begin
		if Assigned(active) then active^.skip := yes;
	end;

	procedure Dialogue.Parse(const s: string);
	var
		t: StringTokenizer;
		n: size_t;
		ni: ItemDesc;
		cp: t.Guard;
		id: string;
	begin
		t := s;
		try
			repeat
				// персонаж: реплика
				// персонаж [эмоция]: реплика
				// персонаж [sizeX = размер по X]
				if not t.MaybeTokenEndingWith(ni.char, ['[']) then begin t.ExpectEnd; break; end;
				ni.pic := 'indifferent.png';
				ni.size := 1.0;
				ni.delay := 2.0;
				if t.Maybe('[') then
				begin
					while t.MaybeTokenEndingWith(id, [',', ']', '='], cp) do
					begin
						case id of
							'face': begin t.Expect('='); ni.pic := t.ScanTokenEndingWith([',', ']']); end;
							'sizeX': begin t.Expect('='); ni.size := t.ScanFloatToken; end;
							'delay': begin t.Expect('='); ni.delay := t.ScanFloatToken; end;
							else t.UnknownIdentifier(cp);
						end;
						if not t.Maybe(',') then break;
					end;
					t.Expect(']');
				end;
				t.Expect(':');

				t.SkipWhitespace;
				n := 0;
				while (n < t.Remaining) and not Prefixed('>>', t.Current + n, t.Remaining - n) do
					inc(n);
				while (n > 0) and Symbol.IsWhitespace(t.Current[n - 1]) do dec(n);
				ni.sentence := t.Read(n);
				if not t.Maybe('>>') then t.ExpectEnd;

				SetLength(items, length(items) + 1);
				items[High(items)] := ni;
			until t.Remaining = 0;
		finally
			t.Done;
		end;
	end;

end.

