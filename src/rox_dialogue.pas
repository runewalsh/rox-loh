{$include opts.inc}
unit rox_dialogue;

interface

uses
	USystem, UClasses, UMath, Algo, Human, Utils, Streams, U_GL, GLBase, GLUtils, TextProcessing,
	rox_gl, rox_state, rox_gfx, rox_ui, rox_paths, rox_win;

type
	pTextBox = ^TextBox;
	TextBox = object(Control)
	type
		SymFlags = set of (IsPunctuation, HasSpaceAfter, HasNewlineAfter);
		SymDesc = object
			data: pUint8;
			pos, size: UintVec2;
			flags: SymFlags;
			procedure Done;
		end;
	var
		pic, name: pTexture;
		syms: array of SymDesc;
		nextSym: sint;
		sum: pUint8;
		sumSize: UintVec2;
		nextLetterTimeout, letterTimeout, nameDx, newlineDelay: float;
		skip, sumDirty, newlined, skipMeNextTime: boolean;

		constructor Init(const char, pic, sentence: string);
		destructor Done; virtual;
		procedure Update(const dt: float); virtual;
		procedure Draw; virtual;
		procedure Advance(n: uint);
		function Finished: boolean;
		function Dump: string;

		procedure HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra); virtual;
	private
		procedure Prepare(const src: TextureImage; const errfn: string);
		procedure ScanRowForSymbols(img: pUint8; ya, yb: uint; const size: UintVec2);
		function GuessBorder: float;
		function CalculateTransparentRect(const nvp: Vec2; const border: float): Rect;
	public const
		StartingSymbolThreshold = High(uint8) div 2;
		FloodFillThreshold = (High(uint8) * 6) div 7;
		AssumedFullPixelWidth = 930;
		PicSize = 0.3;
		NameHeight = 0.1;
		FirstLetterTimeoutK = 3;
		DefaultLetterTimeout = 0.045;
		DefaultRelativePunctuationBonus = 5.0;
	end;

	pDialogue = ^Dialogue;
	Dialogue = object
	type
		ItemDesc = object
			char, pic, sentence: string;
			delay, letterTimeout, newlineDelay: float;
		end;

		ItemEvent = (ItemStart, ItemNewline);
		ItemCallback = procedure(id: uint; what: ItemEvent; param: pointer);
		DoneCallback = procedure(param: pointer);
	var
		state: pState;
		items: array of ItemDesc;
		nextItem: sint;
		active: pTextBox;
		finalTimeout: float;
		onItem: ItemCallback;
		onDone: DoneCallback;
		param: pointer;
		skippable: boolean;

		procedure Invalidate;
		function Valid: boolean;
		function Init(state: pState; const scenario: string): pDialogue;
		procedure Done;
		procedure Callbacks(onItem: ItemCallback; onDone: DoneCallback; param: pointer);
		procedure Update(const dt: float);
		function Finished: boolean;
		procedure Skip;
		procedure ForceFace(const char, pic: string);
	private
		procedure Parse(const s: string);
	const
		Border = 0.03;
		DefaultSentenceDelay = 1.5;
	end;

implementation

	function NameOffset(const name: string): float;
	begin
		Assert(@name = @name);
		result := 0;
	end;

	procedure TextBox.SymDesc.Done;
	begin
		FreeMem(data);
	end;

	constructor TextBox.Init(const char, pic, sentence: string);
	var
		si: pImageResource;
	begin
		inherited Init(nil, []);
		nameDx := NameOffset(char);
		self.pic := Texture.Load(Face(char, pic));
		self.name := Texture.Load(Character(char, 'name.png'));

		if sentence <> '-' then
		begin
			si := ResourcePool.Shared^.LoadRef(TypeOf(ImageResource), rox_paths.Dialogue(char, sentence));
			try
				Prepare(si^.im, rox_paths.Dialogue(char, sentence));
				sumSize := si^.im.Size.XY;
			finally
				Release(si);
			end;

			ChangeTexture(Texture.Dynamic(GLformat_R {обрабатывается особо, см. Texture.InternalFormat}, sumSize, DynamicTextureForTextBox));
			tex^.Swizzle(Swizzle.One, Swizzle.One, Swizzle.One, Swizzle.R);

			sum := GetMem(sumSize.Product * sizeof(uint8));
			Zero(sum, sumSize.Product * sizeof(uint8));
			tex^.Sub(0, 0, sumSize.X, sumSize.Y, GLformat_R, sum);
		end;

		letterTimeout := DefaultLetterTimeout;
		nextLetterTimeout := FirstLetterTimeoutK * letterTimeout;
		newlineDelay := DefaultLetterTimeout;
	end;

	destructor TextBox.Done;
	var
		i: sint;
	begin
		for i := 0 to High(syms) do
			syms[i].Done;
		FreeMem(sum);
		Release(name);
		Release(pic);
		inherited Done;
	end;

	procedure TextBox.Update(const dt: float);
	begin
		nextLetterTimeout -= dt * (1 + 7*ord(skip));
		while (nextSym < length(syms)) and (nextLetterTimeout < 0) do
		begin
			if ([IsPunctuation, HasSpaceAfter] * syms[nextSym].flags = [IsPunctuation, HasSpaceAfter]) and
				not ((nextSym + 1 > High(syms)) or (IsPunctuation in syms[nextSym + 1].flags))
			then
				nextLetterTimeout += letterTimeout * DefaultRelativePunctuationBonus
			else
				nextLetterTimeout += letterTimeout;

			if (nextSym > 0) and (HasNewlineAfter in syms[nextSym - 1].flags) and (nextSym < High(syms)) then
			begin
				newlined := yes;
				nextLetterTimeout := max(nextLetterTimeout, newlineDelay);
			end;
			Advance(1);
			if nextSym >= length(syms) then skip := no;
		end;

		if sumDirty then
		begin
			tex^.Sub(0, 0, sumSize.X, sumSize.Y, GLformat_R, sum);
			sumDirty := no;
		end;
		inherited Update(dt);
	end;

	procedure TextBox.Draw;
	var
		q: Quad;
		rect: UMath.Rect;
	begin
		q.fields := [q.Field.Color];
		q.color := Vec4.Make(0, 0, 0, 0.7);
		rect := CalculateTransparentRect(pStateManager(ui^._mgr)^.nvp, GuessBorder);
		q.Draw(nil, rect.A, rect.Size, Vec2.Zero, Vec2.Ones);

		inherited Draw;

		q.fields := [q.Field.Transform];
		q.transform := local;
		q.Draw(pic, Vec2.Make(0, rawSize.y + 0.0 * GuessBorder), pic^.ap.Aspect2(asp2_x1, PicSize), Vec2.Zero, Vec2.Ones);
		q.Draw(name,
			Vec2.Make(pic^.ap.Aspect2Item(asp2_x1, 0, PicSize) * (1 + nameDx), rawSize.y + 0.0 * GuessBorder), name^.ap.Aspect2(asp2_y1, NameHeight), Vec2.Zero, Vec2.Ones);
	end;

	procedure TextBox.Advance(n: uint);
	var
		sx, sy: uint;
		sym: ^SymDesc;
		b: uint8;
	begin
		sumDirty := sumDirty or (n > 0);
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
		end;
	end;

	function TextBox.Finished: boolean;
	begin
		result := nextSym >= length(syms);
	end;

	function GetSym(id: uint; param: pointer): string;
	var
		p: pTextBox absolute param;
	begin
		if IsPunctuation in p^.syms[id].flags then result := ',' else result := 'S';
		if id + 1 < uint(length(p^.syms)) then
			if HasNewlineAfter in p^.syms[id].flags then result += EOL else
				if HasSpaceAfter in p^.syms[id].flags then result += ' ';
	end;

	function TextBox.Dump: string;
	begin
		result := SeparatedList.Join(length(syms), @GetSym, @self, SeparatedList.Empty + '(-)');
	end;

	procedure TextBox.HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra);
	begin
		case action of
			MouseLClick:
				if CalculateTransparentRect(pStateManager(ui^._mgr)^.nvp, GuessBorder).Contains(pos) and extra.Handle then
					skipMeNextTime := yes;
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
			raise Error('{0}: текст ожидается в grayscale без альфы.', StreamPath.Human(errfn));

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

	// Прямоугольник сканируется по столбцам сверху вниз слева направо.
	// Если найден занятый пиксель, с него заливается весь символ.
	//
	// -ДИАКРИТИКА-
	// Такая заливка могла залить точку над «ё» (или, наоборот, её «е»-часть):
	//   .   x           x   x
	//
	//   xxxxx           .....
	//  x     x         .     .
	//  xxxxxxx   или   .......
	//  x               .
	//   xxxxx           .....
	//
	// На этот случай проверяем окрестность залитой части: прямоугольник с x-координатами по её ширине и y-координатами ya~yb.
	//   o   x          |X|||X|
	//   |              |||||||
	//   Xxxxx          |ooooo|
	//  x|    x         o|||||o
	//  xXxxxxx   или   ooooooo
	//  x|              o||||||
	//   Xxxxx          |ooooo|
	//
	// Если попались незалитые части оригинальной картинки (отмечены «Х»), пробуем залить с них тоже.
	// Если оказалось, что проекция новой залитой части на Ox полностью содержит проекцию исходной, или наоборот — новая включается в символ.
	// Условие с проекциями пропустит диакритику над и под символом, но отклонит «Д» в таком случае:
	//
	//  ||XXxxxx
	//  |X||    x
	//  ||||     x
	//  ooo|     x
	//  |||o     x
	//  |ooo  xxxx
	//  o||o x   x
	//  oooo  xxx
	//
	// Если часть включена в символ, повторяем процесс для неё (т. к. если залита одна точка «ё», вторая зальётся только на третьем проходе).
	//
	// Для оптимизации:
	// 1) Из рассмотрения полностью исключаются прямоугольники, описанные вокруг уже залитых частей.
	//    (Хвостик Й, залезающий внутрь «И»-части, всё равно будет обнаружен, пока торчит и наружу).
	//    Если этого не сделать, нужно будет отмечать залитые или сверять попиксельно, чтобы не войти в бесконечный цикл.
	// 2) Число проходов ограничивается 3 (Ё самая сложная).

	// -ПУНКТУАЦИЯ-
	// Если одна из сторон результирующего символа меньше 2/5 меньшей стороны «эталона»
	// (подпадают «.» (точка), «,» (запятая), «—» (тире))
	// (сейчас эталонной считается первая найденная в картинке буква)
	// или внизу символа была «точка» с двумя такими сторонами (подпадают «?» или «!»)
	// символ считается символом пунктуации — задержка после него увеличивается, имитируя произношение.

	procedure TextBox.ScanRowForSymbols(img: pUint8; ya, yb: uint; const size: UintVec2);
	var
		parts: array[0 .. 2] of FloodFill.Result;
		nParts: sint;
		boundMin, boundMax: UintVec2;

		function AlreadyFilled(x, y: uint; const also: array of FloodFill.Result): boolean;
		var
			i: sint;
		begin
			for i := 0 to nParts - 1 do
				if InRange(x, parts[i].min.x, parts[i].max.x) and InRange(y, parts[i].min.y, parts[i].max.y) then
					exit(yes);

			// по filled.contains медленно будет, наверное
			for i := 0 to High(also) do
				if InRange(x, also[i].min.x, also[i].max.x) and InRange(y, also[i].min.y, also[i].max.y) then
					exit(yes);

			result := no;
		end;

		function TestFellow(const srcMin, srcMax, fellowMin, fellowMax: UintVec2): boolean;
		var
			srcAllowance, fellowAllowance, sa, sb, fa, fb: uint;
		begin
			// проекция ранее залитых частей на Ox содержит проекцию новой, или наоборот
			// допуск — четверть ширины части, вхождение в которую проверяется
			srcAllowance := (srcMax.x - srcMin.x + 1 + 5) div 6;
			fellowAllowance := (fellowMax.x - fellowMin.x + 1 + 5) div 6;

			// srcMin.x ~ srcMax.x с учётом допуска
			sa := srcMin.x - min(srcMin.x, srcAllowance);
			sb := srcMax.x + srcAllowance;

			// fellowMin.x ~ fellowMax.x с учётом допуска
			fa := fellowMin.x - min(fellowMin.x, fellowAllowance);
			fb := fellowMax.x + fellowAllowance;

			result :=
				InRange(srcMin.x, fa, fb) and InRange(srcMax.x, fa, fb) or
				InRange(fellowMin.x, sa, sb) and InRange(fellowMax.x, sa, sb);
		end;

		function SearchForFellows: boolean;
		label &finally;
		var
			x, y, nIgnore: uint;
			fp: FloodParam;
			ignore: array[0 .. 1] of FloodFill.Result; // чтобы тысячу раз не проходиться заливкой ото всех засвеченных пикселей того «Д» из комментария
			referencePart, newPart: ^FloodFill.Result;
		begin
			Assert(@ignore = @ignore); // "does not seem to be initialized"
			nIgnore := 0;
			referencePart := @parts[nParts - 1];
			result := no;

			y := ya;
			while y <= yb do
			begin
				// оптимизация: Y исходной части пропускаются сразу (пользуясь тем, что вся диакритика выше/ниже)
				if (y < referencePart^.min.y) or (y > referencePart^.max.y) then
				begin
					x := referencePart^.min.x;
					while x <= referencePart^.max.x do
					begin
						if (img[y * size.x + x] <= StartingSymbolThreshold) and not AlreadyFilled(x, y, Slice(ignore, nIgnore)) then
						begin
							fp.img := img;
							fp.width := size.x;
							newPart := @parts[nParts];
							FloodFill.FourWay(newPart^, UintVec2.Make(x, y), size, @FloodTest, @fp);

							if TestFellow(boundMin, boundMax, newPart^.min, newPart^.max) then
							begin
								result := yes;
								boundMin := min(boundMin, newPart^.min);
								boundMax := max(boundMax, newPart^.max);
								inc(nParts);
								if nParts > High(parts) then goto &finally;
							end else
								if nIgnore <= High(ignore) then
								begin
									ignore[nIgnore] := newPart^;
									inc(nIgnore);
								end else
								begin
									Warning(Format('TextBox.ScanRowForSymbols.TryFindFellow: nIgnore > 2 (обработано символов: {0}, x = {1}, y = {2}, ' +
										'newPart.min = {3}, newPart.max = {4})', [length(syms), x, y, ToString(newPart^.min), ToString(newPart^.max)]));
									newPart^.Done;
								end;
						end;

						inc(x);
					end;
				end;
				inc(y);
			end;

		&finally:
			while nIgnore > 0 do
			begin
				dec(nIgnore);
				ignore[nIgnore].Done;
			end;
		end;

		function ReferenceSymbolSize: uint;
		begin
			result := min(syms[0].size.x, syms[0].size.y);
		end;

		function DetectPunctuationBySize(const symSize: UintVec2): boolean;
		begin
			result := (symSize.x < (2 * ReferenceSymbolSize + 4) div 5) or (symSize.y < (2 * ReferenceSymbolSize + 4) div 5);
		end;

		function DetectPunctuation(const sym: SymDesc): boolean;
		var
			i: uint;
		begin
			result := DetectPunctuationBySize(sym.size);
			if not result then
				for i := 0 to nParts - 1 do
					result := result or
						(parts[i].min.y > sym.pos.y + (sym.size.y * 3 + 3) div 4) and DetectPunctuationBySize(parts[i].max - parts[i].min + UintVec2.Ones);
		end;

		function DetectSpace(const a, b: SymDesc): boolean;
		begin
			result := (b.pos.x >= a.pos.x + a.size.x) and ((b.pos.x >= (a.pos.x + a.size.x) + (ReferenceSymbolSize + 3) div 4))
		end;

	var
		x, y, i: uint;
		fp: FloodParam;
		sym: ^SymDesc;
		it: FloodFill.PointSet.Iterator;
		p: pUintVec2;
		starting: sint;
	begin
		starting := length(syms);
		x := 0;
		while x < size.x do
		begin
			y := ya;
			while y <= yb do
			begin
				if img[y * size.x + x] <= StartingSymbolThreshold then
				begin
					nParts := 0;
					fp.img := img;
					fp.width := size.x;
					FloodFill.FourWay(parts[nParts], UintVec2.Make(x, y), size, @FloodTest, @fp);
					boundMin := parts[nParts].min;
					boundMax := parts[nParts].max;
					inc(nParts);
					while (nParts <= High(parts)) and SearchForFellows do ;

					SetLength(syms, length(syms) + 1);
					sym := @syms[High(syms)];
					sym^.pos := boundMin;
					sym^.size := boundMax - boundMin + UintVec2.Ones;
					sym^.data := GetMem(sym^.size.Product * sizeof(uint8));
					Zero(sym^.data, sym^.size.Product * sizeof(uint8));

					for i := 0 to nParts - 1 do
					begin
						it := parts[i].filled.GetIterator;
						while parts[i].filled.Next(it) do
						begin
							p := parts[i].filled.GetKey(it);
							sym^.data[(p^.y - boundMin.y) * sym^.size.x + (p^.x - boundMin.x)] := High(uint8) - img[p^.y * size.x + p^.x];
							img[p^.y * size.x + p^.x] := High(uint8);
						end;
						parts[i].Done;
					end;

					sym^.flags := [];
					if DetectPunctuation(sym^) then sym^.flags += [IsPunctuation];
					if (High(syms) > starting) and DetectSpace(syms[High(syms) - 1], sym^) then
						syms[High(syms) - 1].flags += [HasSpaceAfter];
				end;
				inc(y);
			end;
			inc(x);
		end;

		if High(syms) >= starting then syms[High(syms)].flags += [HasSpaceAfter, HasNewlineAfter];
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

	function Dialogue.Init(state: pState; const scenario: string): pDialogue;
	begin
		Invalidate;
		self.state := state;
		items := nil;
		nextItem := 0;
		onItem := nil;
		onDone := nil;
		Parse(scenario);
		result := @self;
		skippable := yes;
	end;

	procedure Dialogue.Done;
	begin
		Release(active);
		Invalidate;
	end;

	procedure Dialogue.Callbacks(onItem: ItemCallback; onDone: DoneCallback; param: pointer);
	begin
		self.onItem := onItem;
		self.onDone := onDone;
		self.param := param;
	end;

	procedure Dialogue.Update(const dt: float);
	var
		na: pTextBox;
		t: DoneCallback;
	begin
		if Assigned(active) then
		begin
			if active^.skipMeNextTime then
			begin
				active^.skipMeNextTime := no;
				Skip;
			end;

			if active^.Finished then
			begin
				finalTimeout -= dt;
				if (finalTimeout < 0) or active^.skip then
				begin
					active^.Detach;
					Release(active);
				end;
			end else
				if active^.newlined then
				begin
					if Assigned(onItem) then onItem(nextItem - 1, ItemNewline, param);
					active^.newlined := no;
				end;
		end;

		if not Assigned(active) and not Finished then
		begin
			if Assigned(onItem) then onItem(nextItem, ItemStart, param);
			na := new(pTextBox, Init(items[nextItem].char, items[nextItem].pic, items[nextItem].sentence))^.NewRef;
			if items[nextItem].letterTimeout > 0 then na^.letterTimeout := items[nextItem].letterTimeout;
			finalTimeout := items[nextItem].delay;

			try
				if not Assigned(na^.tex) then na^.size := 0 else
					na^.size := 2 * state^.mgr^.nvp.x * na^.sumSize.x / TextBox.AssumedFullPixelWidth - 2 * Border;
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

		if Finished and Assigned(onDone) then
		begin
			t := onDone;
			onDone := nil;
			t(param);
		end;
	end;

	function Dialogue.Finished: boolean;
	begin
		result := (nextItem >= length(items)) and not Assigned(active);
	end;

	procedure Dialogue.Skip;
	begin
		if not skippable then exit;
		if Assigned(active) and not active^.skip then
		begin
			active^.skip := yes;
			// увеличение времени паузы на часть пропускаемого времени (саму паузу тоже можно пропустить, отдельно)
			if not active^.Finished then finalTimeout += 0.5 * active^.letterTimeout * (length(active^.syms) - active^.nextSym);
		end;
	end;

	procedure Dialogue.ForceFace(const char, pic: string);
	begin
		if Assigned(active) then
		begin
			SetRef(active^.pic, Texture.Load(Face(char, pic)));
			ReleaseWeak(active^.pic);
		end;
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
				// персонаж [face = эмоция, delay = пауза после реплики]: реплика
				if not t.MaybeTokenEndingWith(ni.char, ['[', ':']) then begin t.ExpectEnd; break; end;
				ni.pic := 'indifferent.png';
				ni.delay := DefaultSentenceDelay;
				ni.letterTimeout := 0;
				ni.newlineDelay := 0;
				if t.Maybe('[') then
				begin
					while t.MaybeTokenEndingWith(id, [',', ']', '='], cp) do
					begin
						case id of
							'face': begin t.Expect('='); ni.pic := t.ScanTokenEndingWith([',', ']']); end;
							'delay': begin t.Expect('='); ni.delay := t.ScanFloatToken; end;
							'letterTimeout': begin t.Expect('='); ni.letterTimeout := t.ScanFloatToken; end;
							'newlineDelay': begin t.Expect('='); ni.newlineDelay := t.ScanFloatToken; end;
							else raise t.UnknownIdentifier(cp);
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

