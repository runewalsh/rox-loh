{$include opts.inc}
unit TextProcessing;

interface

uses
	USystem, Utils, UClasses;

type
	StringTokenizer = object
	type
		Guard = object
		private
			s: pChar;
		end;

		procedure Done;
		function Source: string;
		function Tail: string;
		function Consumed: size_t;
		function ScanTokenEndingWith(const seps: charset_t): string;
		function ScanTokenEndingWith(const seps: charset_t; out cp: Guard): string;
		function MaybeTokenEndingWith(out tok: string; const seps: charset_t): boolean;
		function MaybeTokenEndingWith(out tok: string; const seps: charset_t; out cp: Guard): boolean;
		function ScanUintToken: uint;
		function ScanFloatToken: float;
		function Maybe(const what: string): boolean;
		procedure Expect(const what: string);
		procedure ExpectEnd;
		procedure SkipWhitespace;
		function UnknownIdentifier(const cp: Guard): Exception;
		function Checkpoint: Guard;
		function Revert(const cp: Guard): size_t;

	private
		s: pChar;
		rest: size_t;
		src: pChar;
	{$ifdef Debug} initGuard: pointer; {$endif}
		function InternalTokenEndingWith(out tok: string; const seps: charset_t; throw: boolean): boolean;
		function Highlight(a, b: size_t): string;
		function ExpectedError(a, b: size_t; const what: string): Exception;
	public
		property Remaining: size_t read rest;
	end;
	operator :=(const s: string): StringTokenizer;

type
	ReplaceLines = object
	type
		ReplaceLineFunc = function(const s: string; param: pointer): string;
	var
		function Replace(const text: string; onLine: ReplaceLineFunc; param: pointer; utf8bom: boolean): string; static;
	end;

implementation

uses
	Human {$ifdef selftest}, Tests {$endif};

	procedure StringTokenizer.Done;
	begin
	{$ifdef Debug} Assert(Assigned(initGuard)); FreeMem(initGuard); {$endif}
	end;

	function StringTokenizer.Source: string;
	begin
		result := USystem.ToString(src, Consumed + rest);
	end;

	function StringTokenizer.Tail: string;
	begin
		result := USystem.ToString(s, rest);
	end;

	function StringTokenizer.Consumed: size_t;
	begin
		result := s - src;
	end;

	function CharFromCharset(id: uint; param: pointer): string;
	begin
		if char(id) in charset_t(param^) then
			if (id > 32) and (id <= 127) then
				result := char(id)
			else
				result := '#' + ToString(id)
		else
			result := '';
	end;

	function StringTokenizer.ScanTokenEndingWith(const seps: charset_t): string;
	begin
		InternalTokenEndingWith(result, seps, yes);
	end;

	function StringTokenizer.ScanTokenEndingWith(const seps: charset_t; out cp: Guard): string;
	begin
		cp := Checkpoint;
		InternalTokenEndingWith(result, seps, yes);
	end;

	function StringTokenizer.MaybeTokenEndingWith(out tok: string; const seps: charset_t): boolean;
	begin
		result := InternalTokenEndingWith(tok, seps, no);
	end;

	function StringTokenizer.MaybeTokenEndingWith(out tok: string; const seps: charset_t; out cp: Guard): boolean;
	begin
		cp := Checkpoint;
		result := InternalTokenEndingWith(tok, seps, no);
	end;

	function StringTokenizer.ScanUintToken: uint;
	var
		n: size_t;
	begin
		n := TryParse(s, rest, result);
		if n = 0 then raise ExpectedError(0, n + 1, Format('целое число ({0})', lang_amount(bitsizeof(result), '{N} бит{/а/}')));
		s += n; rest -= n;
	end;

	function StringTokenizer.ScanFloatToken: float;
	type
		StateEnum = (Whitespace, Sign, MantissaBeforePoint, MantissaAfterPoint, E, ExponentSign, Exponent);
	var
		state: StateEnum;
		n: size_t;
		valCode: word;
	begin
		state := Whitespace;
		n := 0;
		while n < rest do
			case state of
				Whitespace: if Symbol.IsWhitespace(s[n]) then inc(n) else state := Sign;
				Sign, ExponentSign: begin if s[n] in ['+', '-'] then inc(n); if state = Sign then state := MantissaBeforePoint else state := Exponent; end;
				MantissaBeforePoint:
					case s[n] of
						'0' .. '9': inc(n);
						'.': begin inc(n); state := MantissaAfterPoint; end;
						else state := E;
					end;
				MantissaAfterPoint:
					case s[n] of
						'0' .. '9': inc(n);
						else state := E;
					end;
				E: begin if s[n] in ['e', 'E'] then inc(n); state := ExponentSign; end;
				Exponent: case s[n] of '0' .. '9': inc(n); else break; end;
				else Assert(no);
			end;

		val(USystem.ToString(s, n), result, valCode);
		if (valCode > 0) or (n = 0) then raise ExpectedError(0, n + 1, 'ожидается вещественное число');
      s += n; rest -= n;
	end;

	function StringTokenizer.Maybe(const what: string): boolean;
	begin
		SkipWhitespace;
		result := Prefixed(what, s, rest);
		if result then
		begin
			s += length(what);
			rest -= size_t(length(what));
		end;
	end;

	procedure StringTokenizer.Expect(const what: string);
	begin
		if not Maybe(what) then raise ExpectedError(0, 1, what);
	end;

	procedure StringTokenizer.ExpectEnd;
	begin
		SkipWhitespace;
		if rest > 0 then raise ExpectedError(0, 1, 'конец строки');
	end;

	procedure StringTokenizer.SkipWhitespace;
	var
		n: size_t;
	begin
		n := 0;
		while (n < rest) and Symbol.IsWhitespace(s[n]) do inc(n);
		s += n; rest -= n;
	end;

	function StringTokenizer.UnknownIdentifier(const cp: Guard): Exception;
	begin
		result := Error(Highlight(0, Revert(cp)) + ': неизвестный идентификатор.');
	end;

	function StringTokenizer.Checkpoint: Guard;
	begin
		result.s := s;
	end;

	function StringTokenizer.Revert(const cp: Guard): size_t;
	begin
		Assert((cp.s >= src) and (cp.s <= s));
		result := s - cp.s;
		rest += result;
		s := cp.s;
	end;

	function StringTokenizer.InternalTokenEndingWith(out tok: string; const seps: charset_t; throw: boolean): boolean;
	var
		&end: size_t;
	begin
		SkipWhitespace;
		&end := 0;
		while (&end < rest) and not ((s[&end] in seps) or Symbol.IsWhitespace(s[&end])) do inc(&end);
		result := &end > 0;
		if result then
		begin
			tok := USystem.ToString(s, &end);
			s += &end; rest -= &end;
		end else
			if throw then raise ExpectedError(0, &end + 1,
				Continued('токен, заканчивающийся на ' + SeparatedList.Join(uint(High(char)) + 1, @CharFromCharset, @seps, ', '), ' или ', 'пробел'));
	end;

	function StringTokenizer.Highlight(a, b: size_t): string;
	begin
		result := StrStuff(StrStuff(Source, 1 + Consumed + b, 0, '<'), 1 + size_t(s - src) + a, 0, '>');
	end;

	function StringTokenizer.ExpectedError(a, b: size_t; const what: string): Exception;
	begin
		result := Error('{0}: ожидается {1}.', Highlight(a, b), what);
	end;

	operator :=(const s: string): StringTokenizer;
	begin
		result.s := pChar(s);
		result.rest := length(s);
		result.src := pChar(s);
	{$ifdef Debug} result.initGuard := GetMem(1); {$endif}
	end;

	function ReplaceLines.Replace(const text: string; onLine: ReplaceLineFunc; param: pointer; utf8bom: boolean): string;
	var
		sb: StringBuilder;
		stringStart, p, n: size_t;
	begin
		sb.Init;
		stringStart := 0; p := 0;
		if Prefixed(UTF8.BOM, text) then begin stringStart += length(UTF8.BOM); p += length(UTF8.BOM); end;
		if utf8bom then sb.Append(UTF8.BOM);

		repeat
			if UTF8.IsEOL(pChar(text) + p, size_t(length(text)) - p, n) then
			begin
				sb.Append(onLine(Copy(text, stringStart + 1, p - stringStart), param), Copy(text, p+1, n));
				p += n;
				stringStart := p;
				if n = 0 then break;
			end else
			begin
				if UTF8.Peek(text, p + 1, n) = UTFInvalid then begin Info.Show('brk at ' + tostring(p)); break; end;
				p += n;
			end;
		until no;
		if stringStart < p then sb.Append(onLine(Copy(text, stringStart + 1, p - stringStart), param));
		result := sb.DestructiveToString;
		while (length(result) > 0) and Symbol.IsNewline(result[length(result)]) do delete(result, length(result), 1);
	end;

{$ifdef selftest}
	function ReverseLine(const s: string; param: pointer): string;
	var
		i: sint;
	begin
		Assert(@param = @param);
		SetLength(result, length(s));
		for i := 1 to length(result) do
			result[i] := s[length(result) - i + 1];
		if s = '' then result := '(empty)';
	end;

	function ReplaceLinesCase(const input: string): string;
	begin
		result := ReplaceLines.Replace(input, @ReverseLine, nil, no);
	end;

	procedure Test;
	begin
		TestSuite.Start.Feature('ReplaceLines', @ReplaceLinesCase)
		.&Case('Line 1' + EOL + Carriage + EOL + 'Line 2' + Carriage + EOL + 'Line 3' + Carriage + 'Line 4' + Carriage + Carriage,
				'1 eniL' + EOL + '(empty)' + Carriage + EOL + '2 eniL' + Carriage + EOL + '3 eniL' + Carriage + '4 eniL' + Carriage + '(empty)' + Carriage + '(empty)')
		.Done;
	end;
{$endif}

initialization
	{$ifdef selftest} &Unit('TextProcessing').Test(@Test); {$endif}
end.
