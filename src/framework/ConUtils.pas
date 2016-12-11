unit ConUtils;

{$define arrayofconst} {$include opts.inc}
{$ifndef use_console} {$error Определи use_console} {$endif}

interface

uses
	USystem, Utils, Streams, UMath, Human;

const
	DefaultFilenameSample = #0#1'%DEF_FN%'#1#0;

	procedure disable_cmdline_params;
	function scan_yn(const msg: string; default: char = '-'): boolean;
	function scan_int(const msg: string; const default: ilong = Low(ilong)): ilong;
	function scan_float(const msg: string): hp_float;
	function scan_string(const msg: string; const base: string): string;
	function scan_string(const msg: string): string;
	function scan_string_or_default(const msg: string; const default: string): string;
	function scan_file(const msg: string; flags: FileFlags = DefaultFileFlags): pStream;
	function scan_file_ref(const msg: string; flags: FileFlags = DefaultFileFlags): pStream;
	function scan_filename(const msg: string; const base, defaultFileName: string): string;
	function scan_file(const msg: string; const base, defaultFileName: string; flags: FileFlags = DefaultFileFlags): pStream;
	function scan_float_or_default(const msg: string; default: hp_float = 0.0): hp_float;
	function Bar(const progress: float; divs: sint): string;

	procedure ConWrite(const fmt: string; const args: array of const);
	procedure ConLine(const fmt: string; const args: array of const);
{$define func:=
	procedure ConWrite(const fmt: string; const _ARGS_: string);
	procedure ConLine(const fmt: string; const _ARGS_: string);} {$include variadic.inc}

type
	TextAlignment = (align_A, align_B, align_Center);
	pConsoleBuffer = ^ConsoleBuffer;
	ConsoleBuffer = object
	private type
		Sym_t = record
			ch, oldCh: UTFchar;
			col, oldCol: Color;
		end;
	private var
		_sizeX, _sizeY: sint;
		_syms: array of Sym_t;
		function _ValidatePoint(x, y: sint): boolean;
		function _GetSym(x, y: sint): UTFchar;
		procedure _SetSym(x, y: sint; const newSym: UTFchar);
		function _GetColor(x, y: sint): Color;
		procedure _SetColor(x, y: sint; const newColor: Color);
	public const
		Line1 = High(UTFchar);
		Line2 = High(UTFchar) - 1;
	public
		procedure Init;
		procedure Init(newSizeX, newSizeY: sint);
		procedure Done;
		procedure Invalidate;
		procedure InvalidateRect(ax, ay, w, h: sint);
		procedure Clear;
		procedure InstantClear;
		procedure Plot(x, y: sint; const newSym: UTFchar; const newColor: Color);
		procedure Rect(ax, ay, w, h: sint; const ch: UTFchar; const color: Color);
		procedure Rect(ax, ay, w, h: sint; const ch, fillCh: UTFchar; const color, fillColor: Color);
		procedure Print(x, y: sint; const text: string; const color: Color);
		procedure TextRect(ax, ay, w, h: sint; const text: string; const color: Color; alignX, alignY: TextAlignment);
		procedure TextRect(ax, ay, w, h: sint; const text: string; const color: Color; border: sint; alignX, alignY: TextAlignment);
		procedure Line(x1, y1, x2, y2: sint; const ch: UTFchar; const color: Color);
		procedure Update;

		property SizeX: sint read _sizeX;
		property SizeY: sint read _sizeY;
		property Sym[x, y: sint]: UTFchar read _GetSym write _SetSym; default;
		property Color[x, y: sint]: Color read _GetColor write _SetColor;
	end;

const
	Line1V = $2502;
	Line1H = $2500;
	Line1Corners: array[0 .. 1, 0 .. 1] of UTFchar = (($250c, $2514), ($2510, $2518));
	Line2V = $2551;
	Line2H = $2550;
	Line2Corners: array[0 .. 1, 0 .. 1] of UTFchar = (($2554, $255a), ($2557, $255d));

implementation

var
	nextparam: sint = 0;
	cmdLine: ^CommandLine;

	procedure disable_cmdline_params;
	begin
		nextparam := -1;
	end;

	function scan_yn(const msg: string; default: char = '-'): boolean;
	var
		s: string;
	begin
		repeat
			s := scan_string(msg);
			if (s = '') and (default in ['y', 'Y', 'n', 'N']) then exit(default in ['y', 'Y']);
			if length(s) = 1 then
				case s[1] of
					'y', 'Y': exit(yes);
					'n', 'N': exit(no);
				end;
		until no;
	end;

	function scan_int(const msg: string; const default: ilong = Low(ilong)): ilong;
	var
		inp: string;
	begin
		repeat
			inp := scan_string(msg);
			if TryParse(inp, result) then break;
			if (inp = '') and (default <> Low(ilong)) then exit(default);
		until no;
	end;

	function scan_float(const msg: string): hp_float;
	begin
		repeat
			if TryParse(scan_string(msg), result) then break;
		until no;
	end;

	function scan_string(const msg: string; const base: string): string;
	begin
		Con.Write(msg);
		if nextparam >= 0 then
		begin
			if not Assigned(cmdLine) then
			begin
				new(cmdLine);
				cmdLine^ := CommandLine.Get;
			end;

			if uint(nextparam) < cmdLine^.Count then
			begin
				result := base + cmdLine^[nextparam];
				inc(nextparam);
				Con.WriteLine(result);
				exit;
			end;
		end;

		Con.Input(base);
		result := Con.ReadLine;
	end;

	function scan_string(const msg: string): string;
	begin
		result := scan_string(msg, '');
	end;

	function scan_string_or_default(const msg: string; const default: string): string;
	begin
		result := scan_string(msg);
		if result = '' then result := default;
	end;

	function scan_file(const msg: string; flags: FileFlags = DefaultFileFlags): pStream;
	var
		path: string;
	begin
		path := scan_string(msg);
		result := new(pFileStream, Init(FromSystemFileName(path), flags));
		if not Assigned(result) then
			Con.WriteLine('Ошибка при открытии файла.');
	end;

	function scan_file_ref(const msg: string; flags: FileFlags = DefaultFileFlags): pStream;
	begin
		result := MakeRef(scan_file(msg, flags));
	end;

	function scan_filename(const msg: string; const base, defaultFileName: string): string;
		function glue(const fn, ext: string): string;
		begin
			if ext = '' then
				result := fn
			else
				result := fn + ExtensionSeparator + ext;
		end;
	var
		inp, fn, ext, t: string;
	begin
		fn := StreamPath.FilenameNoExt(base);
		if fn = '' then fn := StreamPath.FilenameNoExt(defaultFileName);
		ext := StreamPath.Extension(base);
		if ext = '' then ext := StreamPath.Extension(defaultFileName);

		Con.Write(StrReplace(msg, DefaultFilenameSample, glue(fn, ext)));
		inp := FromSystemFileName(scan_string('', ToSystemFileName(StreamPath.Path(base))));

		t := StreamPath.FileNameNoExt(inp);
		if t <> '' then fn := t;
		t := StreamPath.Extension(inp);
		if t <> '' then ext := t;

		result := StreamPath.Resolve(StreamPath.Path(inp)) + glue(fn, ext);
	end;

	function scan_file(const msg: string; const base, defaultFileName: string; flags: FileFlags = DefaultFileFlags): pStream;
	var
		fn: string;
	begin
		fn := scan_filename(msg, base, defaultFileName);
		Con.WriteLine('Файл: ' + ToSystemFileName(fn));
		result := new(pFileStream, Init(fn, flags));

		if not Assigned(result) then
			Con.WriteLine('Ошибка при открытии файла.');
	end;

	function scan_float_or_default(const msg: string; default: hp_float = 0.0): hp_float;
	var
		s: string;
	begin
		repeat
			s := scan_string(msg);
			if (s = '') or (TryParse(s, result)) then
			begin
				if s = '' then result := default;
				break;
			end;
		until no;
	end;

	function Bar(const progress: float; divs: sint): string;
	const
		Sym: array[boolean] of char = ('.', '#');
	var
		i: sint;
		one, current: float;
	begin
		one := 1 / (divs + 1);
		current := 0.0;
		SetLength(result, divs);
		for i := 1 to divs do
		begin
			current += one;
			result[i] := Sym[(current < progress) or ((i = divs) and (progress = 1))];
		end;
	end;

	procedure ConWrite(const fmt: string; const args: array of const); begin Con.Write(Format(fmt, args)); end;
	procedure ConLine(const fmt: string; const args: array of const);  begin Con.WriteLine(Format(fmt, args)); end;

{$define func:=
	procedure ConWrite(const fmt: string; const _ARGS_: string); begin Con.Write(Format(fmt, [_ARGS_])); end;
	procedure ConLine(const fmt: string; const _ARGS_: string); begin Con.WriteLine(Format(fmt, [_ARGS_])); end;} {$include variadic.inc}

	function ConsoleBuffer._ValidatePoint(x, y: sint): boolean;
	begin
		result := (x >= 0) and (x < _sizeX) and (y >= 0) and (y < _sizeY);
	end;

	function ConsoleBuffer._GetSym(x, y: sint): UTFchar;
	begin
		if _ValidatePoint(x, y) then
			result := _syms[y * _sizeX + x].ch
		else
			result := UTFInvalid;
	end;

	procedure ConsoleBuffer._SetSym(x, y: sint; const newSym: UTFchar);
	begin
		if _ValidatePoint(x, y) then
			_syms[y * _sizeX + x].ch := newSym;
	end;

	procedure ConsoleBuffer.Plot(x, y: sint; const newSym: UTFchar; const newColor: Color);
	var
		id: sint;
	begin
		if not _ValidatePoint(x, y) then exit;
		id := y * _sizeX + x;
		_syms[id].ch := newSym;
		_syms[id].col := newColor;
	end;

	function ConsoleBuffer._GetColor(x, y: sint): Color;
	begin
		result := _syms[y * _sizeX + x].col;
	end;

	procedure ConsoleBuffer._SetColor(x, y: sint; const newColor: Color);
	begin
		_syms[y * _sizeX + x].col := newColor;
	end;

	procedure ConsoleBuffer.Init;
	var
		sx, sy: sint;
	begin
		Con.GetSizes(sx, sy);
		Init(sx, sy);
	end;

	procedure ConsoleBuffer.Init(newSizeX, newSizeY: sint);
	begin
		Con.SetSizes(max(1, newSizeX), max(1, newSizeY));
		Con.GetSizes(_sizeX, _sizeY);
		SetLength(_syms, _sizeX * _sizeY);
		Clear;
		Invalidate;
	end;

	procedure ConsoleBuffer.Done;
	begin
		_syms := nil;
	end;

	procedure ConsoleBuffer.Invalidate;
	var
		i: sint;
	begin
		for i := 0 to High(_syms) do
		begin
			_syms[i].oldCh := UTFInvalid;
			_syms[i].oldCol := USystem.Color.RGB(0.123, 0.456, 0.789);
		end;
	end;

	procedure ConsoleBuffer.InvalidateRect(ax, ay, w, h: sint);
	var
		x, y, x1, y1, x2, y2, base: sint;
	begin
		x1 := max(ax, 0);
		y1 := max(ay, 0);
		x2 := min(ax + w, _sizeX) - 1;
		y2 := min(ay + h, _sizeY) - 1;
		for y := y1 to y2 do
		begin
			base := y * _sizeX;
			for x := x1 to x2 do
				_syms[base + x].oldCh := UTFInvalid;
		end;
	end;

	procedure ConsoleBuffer.Clear;
	var
		i: sint;
	begin
		for i := 0 to High(_syms) do
		begin
			_syms[i].ch := ord(' ');
			_syms[i].col := USystem.Color.RGB(0.5, 0.5, 0.5);
		end;
	end;

	procedure ConsoleBuffer.InstantClear;
	begin
		Clear;
		Update;
		Con.SetPosition(0, 0);
	end;

	procedure ConsoleBuffer.Rect(ax, ay, w, h: sint; const ch: UTFchar; const color: Color);
		function c(const sym: UTFchar; x, y: sint): UTFchar;
		begin
			case sym of
				Line1: result := Line1Corners[x, y];
				Line2: result := Line2Corners[x, y];
				else result := sym;
			end;
		end;

		function horz(const sym: UTFchar): UTFchar;
		begin
			case sym of
				Line1: result := Line1H;
				Line2: result := Line2H;
				else result := sym;
			end;
		end;

		function vert(const sym: UTFchar): UTFchar;
		begin
			case sym of
				Line1: result := Line1V;
				Line2: result := Line2V;
				else result := sym;
			end;
		end;
	var
		x1, x2, y1, y2, x, y: sint;
		cs: UTFchar;
	begin
		x1 := ax;
		x2 := ax + w - 1;
		y1 := ay;
		y2 := ay + h - 1;

		Plot(x1, y1, c(ch, 0, 0), color);
		Plot(x2, y1, c(ch, 1, 0), color);
		Plot(x1, y2, c(ch, 0, 1), color);
		Plot(x2, y2, c(ch, 1, 1), color);
		cs := horz(ch);
		for x := x1 + 1 to x2 - 1 do
		begin
			Plot(x, y1, cs, color);
			Plot(x, y2, cs, color);
		end;
		cs := vert(ch);
		for y := y1 + 1 to y2 - 1 do
		begin
			Plot(x1, y, cs, color);
			Plot(x2, y, cs, color);
		end;
	end;

	procedure ConsoleBuffer.Rect(ax, ay, w, h: sint; const ch, fillCh: UTFchar; const color, fillColor: Color);
	var
		x, y: sint;
	begin
		Rect(ax, ay, w, h, ch, color);
		for y := ay + 1 to ay + h - 2 do
			for x := ax + 1 to ax + w - 2 do
				Plot(x, y, fillCh, fillColor);
	end;

	procedure ConsoleBuffer.Print(x, y: sint; const text: string; const color: Color);
	var
		p: sint;
		ch: UTFchar;
		sx: sint;
	begin
		sx := x;
		p := 1;
		repeat
			ch := UTF8.Next(text, p, ch);
			case ch of
				UTFInvalid: break;
				ord(EOL):
					begin
						x := sx;
						inc(y);
					end;
				else
					begin
						Plot(x, y, ch, color);
						inc(x);
					end;
			end;
		until no;
	end;

	procedure ConsoleBuffer.TextRect(ax, ay, w, h: sint; const text: string; const color: Color; alignX, alignY: TextAlignment);
	var
		strs: array of array of UTFchar;
		cs: array of UTFchar;
		csym: UTFchar;
		i, j, x, y, p, lastSep, lastSepId: sint;
	begin
		p := 1;
		strs := nil;
		while (p <= length(text)) and (length(strs) < h) do
		begin
			cs := nil;
			while (UTF8.Next(text, p, csym) <> UTFInvalid) and Symbol.IsWhitespace(csym) do
				if Symbol.IsNewline(csym) then
					if length(strs) < h then
						SetLength(strs, length(strs) + 1)
					else
						break;
			if length(strs) >= h then break;

			if csym = UTFInvalid then break;
			SetLength(cs, 1);
			cs[0] := csym;

			lastSepId := -1;
			while (length(cs) < w) and (UTF8.Next(text, p, csym) <> UTFInvalid) do
			begin
				SetLength(cs, length(cs) + 1);
				cs[High(cs)] := csym;
				if Symbol.IsWhitespace(csym) then
				begin
					lastSep := p;
					lastSepId := High(cs);
					if Symbol.IsNewline(csym) then break;
				end;
			end;

			if (length(cs) = w) and (lastSepId >= 0) then
			begin
				csym := UTF8.Peek(text, p);
				if (csym <> UTFInvalid) and Symbol.IsWhitespace(csym) then
				begin
					p := lastSep;
					SetLength(cs, lastSepId);
				end else
					UTF8.Next(text, p);
			end;
			while Symbol.IsWhitespace(cs[High(cs)]) do SetLength(cs, length(cs) - 1);

			SetLength(strs, length(strs) + 1);
			strs[High(strs)] := cs;
		end;

		case alignY of
			align_A: y := ay;
			align_B: y := ay + h - length(strs);
			align_Center: y := ay + (h - length(strs)) div 2;
		end;
		for i := 0 to High(strs) do
		begin
			case alignX of
				align_A: x := ax;
				align_B: x := ax + w - length(strs[i]);
				align_Center: x := ax + (w - length(strs[i])) div 2;
			end;
			for j := 0 to High(strs[i]) do
				Plot(x + j, y + i, strs[i][j], color);
		end;
	end;

	procedure ConsoleBuffer.TextRect(ax, ay, w, h: sint; const text: string; const color: Color; border: sint; alignX, alignY: TextAlignment);
	begin
		TextRect(ax + border, ay + border, w - 2*border, h - 2*border, text, color, alignX, alignY);
	end;

	procedure ConsoleBuffer.Line(x1, y1, x2, y2: sint; const ch: UTFchar; const color: Color);
	var
		dx, dy, i, sx, sy, e, x, y: sint;
		yalign: boolean;
	begin
		dx := abs(x1 - x2);  dy := abs(y1 - y2);
		yalign := dy > dx;
		if yalign then Swap(dx, dy);

		sx := IntSign(x2 - x1); sy := IntSign(y2 - y1);
		x := x1; y := y1;
		e := 2*dy - dx;

		for i := 0 to dx do
		begin
			Plot(x, y, ch, color);
			if e >= 0 then
			begin
				if yalign then inc(x, sx) else inc(y, sy);
				dec(e, 2*dx);
			end;
			if yalign then inc(y, sy) else inc(x, sx);
			inc(e, 2*dy);
		end;
	end;

	procedure ConsoleBuffer.Update;
	var
		i: sint;
		rowStart, rowEnd: sint;
		ud: sint;
	begin
		i := 0;
		ud := 0;
		while i <= High(_syms) do
		begin
			if (_syms[i].ch = _syms[i].oldCh) and (_syms[i].col = _syms[i].oldCol) then
			begin
				inc(i);
				continue;
			end;
			rowStart := i;
			rowEnd := rowStart;
			repeat
				_syms[rowEnd].oldCh := _syms[rowEnd].ch;
				_syms[rowEnd].oldCol := _syms[rowEnd].col;
				inc(rowEnd);
			until (rowEnd > High(_syms)) or ((_syms[rowEnd].ch = _syms[rowEnd].oldCh) and (_syms[rowEnd].col = _syms[rowEnd].oldCol));
			Con.Write(rowStart mod _sizeX, rowStart div _sizeX, rowEnd - rowStart, sizeof(Sym_t), @_syms[rowStart].ch, @_syms[rowStart].col);
			inc(ud, rowend - rowstart);
			i := rowEnd;
		end;
	end;

	procedure Done;
	begin
		if Assigned(cmdLine) then
		begin
			cmdLine^.Done;
			dispose(cmdLine);
		end;
	end;

initialization
	units.Add('ConUtils').Finalize(@Done);
end.
