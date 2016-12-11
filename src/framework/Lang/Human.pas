unit Human;

{$include opts.inc}
{$ifdef Debug}
	{-$define DebugLocalize}
{$endif}

interface

uses
	USystem, Utils, UClasses, Script, Streams;

type
	// scoped_enum &Case = (Nominative, Genitive, Dative, Accusative, Ablative, Prepositional);

	pLocale = ^Locale;
	Locale = object
	public type
		OnChangeProc = procedure(var loc: Locale; const info: SingleDelegateInfo);
	private
		ss: pScriptState;
		lang: string;
		fsStart: sint;
		procedure SetLang(const newLang: string);
		function GetFsBase: string;
		procedure SetFsBase(const newBase: string);
		function Localize(const s: string; depth: sint): string;
		function Interpret(const s: string; depth: sint): string;
		function QueryFs(parent: sint; name: string): sint;
		function ExtractLocalized(idx: sint; depth: sint): string;
	private const
		LocalizedPrefix = #0'{';
		LocalizedSuffix = '}'#1;
		MaxDepth = 3;
		Qfs_FileLoaded = -2;
		DontLoadTag = 'LDONTLOAD';
		KeepTimeoutMs = ResourcePool.DefaultTimeout;
	public
		onChange: MultiDelegate;
		procedure Init(const newLang: string; baseSS: pScriptState);
		procedure Done;
		function Localized(const s: string): string; static;
		function Localize(const s: string): string;
		function LocalizationRequired(const s: string): boolean; static;

		property Language: string read lang write SetLang;
		property FsBase: string read GetFsBase write SetFsBase;
	end;

	function lang_amount(amt: ulong; const s: string): string;
	function Hyphen(const text: string; start, pos: sint; out resumeAt: sint; out sym: UTFchar): boolean;
	function CapitalizeFirst(const text: string): string;
	function LowercaseFirst(const text: string): string;
	function FixupSentence(const text: string): string;

type
	Symbol = object
	type
		Flag = (Letter, Vowel, Consonant, Digit, Whitespace, Newline, Punctuation, Cyrillic, Latin, Diacritic);
		Classification = set of Flag;
	var
		function Classify(const sym: UTFchar): Classification; static;
		function IsWhitespace(const sym: UTFchar): boolean; static; function IsWhitespace(sym: char): boolean; static;
		function IsNewline(const sym: UTFchar): boolean; static; function IsNewline(sym: char): boolean; static;
		function IsDiacritic(const sym: UTFchar): boolean; static; cinline
		function IsIdentifier(sym: char): boolean; static; cinline
		function Capitalize(const sym: UTFchar): UTFchar; static;
		function LowerCase(const sym: UTFchar): UTFchar; static;
		function IsLowerCase(const sym: UTFchar): boolean; static;
		function IsUpperCase(const sym: UTFchar): boolean; static;
	const
		TAB                 = ord(TabSym);
		UNIX_EOL            = $a;
		MAC_EOL             = $d;
		SPACE               = $20;
		NON_BREAKING_SPACE  = $a0;
		EM_DASH             = $2014;
		IDEOGRAPHIC_SPACE   = $3000;
		FIRST_DIACRITIC = $300; LAST_DIACRITIC = $36F;
		WhitespaceChars     = [' ', TabSym, chr(UNIX_EOL), chr(MAC_EOL), chr(NON_BREAKING_SPACE)];
	type
		Cyrillics = object
		const
			CAP_A = $410; CAP_IE = $415; CAP_I = $418; CAP_O = $41E; CAP_U = $423; CAP_YERU = $42B; CAP_E = $42D; CAP_YU = $42E; CAP_YA = $42F;
			A     = $430;     IE = $435;     I = $438;     O = $43E;     U = $443;     YERU = $44B;     E = $44D;     YU = $44E;     YA = $44F;
			CAP_HARD_SIGN = $42A; CAP_SOFT_SIGN = $42C; HARD_SIGN = $44A; SOFT_SIGN = $44C;
			CAP_IO = $401; IO = $451;
		private
			function ClassifyAYA(const sym: UTFchar): Classification; static; cinline
		end;
	end;

const
	Months: array[0 .. 11] of record
		name, genitive, abbrev: string;
	end =
	(
		(name: 'январь';   genitive: 'января';  abbrev: 'jan'),
		(name: 'февраль';  genitive: 'февраля'; abbrev: 'feb'),
		(name: 'март';     genitive: 'марта';   abbrev: 'mar'),
		(name: 'апрель';   genitive: 'апреля';  abbrev: 'apr'),
		(name: 'май';      genitive: 'мая';     abbrev: 'may'),
		(name: 'июнь';     genitive: 'июня';    abbrev: 'jun'),
		(name: 'июль';     genitive: 'июля';    abbrev: 'jul'),
		(name: 'август';   genitive: 'августа'; abbrev: 'aug'),
		(name: 'сентябрь'; genitive: 'сентября'; abbrev: 'sep'),
		(name: 'октябрь';  genitive: 'октября';  abbrev: 'oct'),
		(name: 'ноябрь';   genitive: 'ноября';   abbrev: 'nov'),
		(name: 'декабрь';  genitive: 'декабря';  abbrev: 'dec')
	);

implementation

{$ifdef Debug} uses ULog; {$endif}

	procedure _CallOnChangeLocale(const info: SingleDelegateInfo; param: pointer);
	var
		loc: ^Locale absolute param;
	begin
		Locale.OnChangeProc(info.proc)(loc^, info);
	end;

	procedure Locale.SetLang(const newLang: string);
	begin
		if lang <> newLang then
		begin
		{$ifdef Debug} LogR('Переключение языка: ' + lang + ' -> ' + newLang + '... '); {$endif}
			lang := newLang;
			onChange.Call(@_CallOnChangeLocale, @self);
		{$ifdef Debug} Log('Язык переключен', logOK); {$endif}
		end;
	end;

	function Locale.GetFsBase: string;
	begin
		result := FsCache^.Trace(fsStart);
	end;

	procedure Locale.SetFsBase(const newBase: string);
	begin
		fsStart := FsCache^.Query(FilesystemCache.IROOT, PrefetchPath(newBase));

	{$ifdef Debug}
		if fsStart >= 0 then
			Log('Задан путь к файлам локализации: ' + StreamPath.Log(FsCache^.Trace(fsStart)))
		else
			Log('Не удаётся найти путь к файлам локализации: ' + StreamPath.Log(newBase), logError);
	{$endif}
	end;

	function Locale.Localize(const s: string; depth: sint): string;
	var
		p, pprefix, psuffix: sint;
		last: boolean;
	begin
		if depth >= MaxDepth then exit(s);
		result := '';
		p := 1;

		repeat
			pprefix := Pos(LocalizedPrefix, s, p);
			last := pprefix = 0;
			if last then
				result += Copy(s, p, length(s) + 1 - p)
			else
			begin
				result += Copy(s, p, pprefix - p);
				psuffix := Pos(LocalizedSuffix, s, pprefix + length(LocalizedPrefix));
				if psuffix <> 0 then p := psuffix + length(LocalizedSuffix) else
				begin
				{$ifdef Debug} Log('Непарные LocalizedPrefix/LocalizedSuffix: ' + s, logError); {$endif}
					psuffix := length(s) + 1;
					p := psuffix;
				end;
				result += Interpret(Copy(s, pprefix + length(LocalizedPrefix), psuffix - pprefix - length(LocalizedPrefix)), depth);
			end;
		until last;
	end;

	function Locale.Interpret(const s: string; depth: sint): string;
	label _finally_;
	{$ifdef Debug} const WrongFieldMsg: array[boolean] of string = ('найдено', 'является ни строкой, ни таблицей'); {$endif}
	var
		p, dotp, fsItem: sint;
		fsNow, last, ok: boolean;
		current: string;
	begin
		if not Assigned(ss) then exit(s);
		// s — строка вида a.b.c.d, разворачивается в, например, a/b.lua со структурой c = { d = { ru = "текст", be = "тэкст" } }
		// папки и таблицы взаимозаменяемы, т. е. в примере выше возможны a.lua: b = { c = { d = { ... } } } и a/b/c.lua: d = { ... }

		fsNow := yes;
		p := 1;
		fsItem := fsStart;
		ok := no;

		repeat
			dotp := Pos('.', s, p);
			last := dotp = 0;
			if last then current := Copy(s, p, length(s) + 1 - p) else
			begin
				current := Copy(s, p, dotp - p);
				p := dotp + 1;
			end;

			if fsNow then
			begin
				fsItem := QueryFs(fsItem, current);
				if fsItem = Qfs_FileLoaded then
				begin
					ok := yes; // только в первый раз
					fsNow := no;
				end else
					if fsItem < 0 then goto _finally_;
			end else
			begin
				ok := ss^.GetTableS(-1, current);
				if ok then ss^.Remove(-2);

				if (not ok) or not (ss^.Typ(-1) in [script_String, script_Table]) then
				begin
				{$ifdef Debug} if ok then Log('"' + s + '": поле "' + current + '" не ' + WrongFieldMsg[ok], logError); {$endif}
					ss^.Pop;
					ok := no;
				end;
				if not ok then goto _finally_;
			end;
		until last;

		if not ok then
		begin
		{$ifdef Debug} Log('"' + s + '" осталась в пределах ФС', logError); {$endif}
			goto _finally_;
		end;

	_finally_:
		if ok then
		begin
			result := ExtractLocalized(-1, depth);
			ss^.Pop;
		end else
			result := s;
	{$ifdef DebugLocalize} Log('Locale.Interpret: "' + s + '" => "' + result + '"', logDebug); {$endif}
	end;

type
	pFsQueryCheckParam = ^tFsQueryCheckParam;
	tFsQueryCheckParam = record
		id: sint;
		ok: boolean;
	end;

	procedure _FsQueryCheck(var node: FilesystemCache.Node; id: sint; param: pointer);
	var
		p: pFsQueryCheckParam absolute param;
	begin
		p^.ok := (not (file_Folder in node.what)) and not node.TaggedWith(Locale.DontLoadTag);
		p^.id := id;
	end;

	function Locale.QueryFs(parent: sint; name: string): sint;
	var
		p: tFsQueryCheckParam;
	begin
		p.id := -1;
		result := FsCache^.Query(parent, name, @_FsQueryCheck, @p);
		if (p.id >= 0) and p.ok then
			if ss^.LoadModule(FsCache^.Trace(p.id), 0, 0, KeepTimeoutMs) = 0 then exit(Qfs_FileLoaded) else
			begin
			{$ifdef Debug} Log('Файл помечен как незагружаемый: ' + StreamPath.Log(FsCache^.Trace(p.id)), logWarning); {$endif}
				FsCache^.TagWith(p.id, DontLoadTag);
				result := -1;
			end;
		if result < 0 then result := -1;
	end;

	function Locale.ExtractLocalized(idx: sint; depth: sint): string;
	begin
		Assert(ss^.Typ(idx) in [script_String, script_Table]);
		case ss^.Typ(idx) of
			script_String: result := ss^.ToString(idx);
			script_Table:
				begin
					idx := ss^.AbsIdx(idx);
					result := ss^.GetStringField(idx, lang);
					if result = '' then result := ss^.GetStringField(idx, 1);
					if (result = '') and (lang <> DefaultLanguage) then result := ss^.GetStringField(idx, DefaultLanguage);
					if result = '' then
					begin
						ss^.PushNil;
						if ss^.Next(idx) then
						begin
							result := ss^.ToString(-1);
							ss^.Pop(2);
						end;
					end;
				end;
			else Assert(no);
		end;
		result := Localize(result, depth + 1);
	end;

	procedure Locale.Init(const newLang: string; baseSS: pScriptState);
	begin
		ss    := baseSS;
		lang := newLang;
		onChange.Init;
		fsStart := FilesystemCache.IROOT;
	{$ifdef Debug} if not Assigned(ss) then Log('Локаль не связана со скриптом, поэтому работать не будет.', logWarning); {$endif}
	end;

	procedure Locale.Done;
	begin
		onChange.Done;
	end;

	function Locale.Localized(const s: string): string;             begin result := LocalizedPrefix + s + LocalizedSuffix; end;
	function Locale.Localize(const s: string): string;              begin result := Localize(s, 0); end;
	function Locale.LocalizationRequired(const s: string): boolean; begin result := Pos(LocalizedPrefix, s) > 0; end;

	function lang_amount(amt: ulong; const s: string): string;
	type
		ModeEnum = (Text, Read1, Read234, ReadMul);
	var
		i, p: size_t;
		refMode, mode: ModeEnum;
	begin
		if (amt div 10 mod 10 = 1) or (amt mod 10 = 0) or (amt mod 10 > 4) then refMode := ReadMul else
		if amt mod 10 = 1 then refMode := Read1 else
			refMode := Read234;

		result := '';
		i := 1; p := 0;
		mode := Text;
		while i <= size_t(length(s)) do
			case mode of
				Text:
					begin
						p := Pos('{', s, i);
						if (p = 0) or (p + 2 > size_t(length(s))) then
						begin
							p := size_t(length(s)) + 1;
							break;
						end;
						result += Copy(s, i, p - i);
						if (s[p + 1] = 'N') and (s[p + 2] = '}') then
						begin
							i := p + 3;
							result += ToString(amt);
						end else
						if s[p + 1] = '{' then
						begin
							result += s[p + 1];
							i := p + 2;
						end else
						begin
							i := p + 1; p := i;
							mode := Read1;
						end;
					end;
				Read1, Read234, ReadMul:
					while p <= size_t(length(s)) do
						case s[p] of
							'/':
								begin
									if mode = refMode then result += Copy(s, i, p - i);
									if mode < ReadMul then inc(mode) else Assert(no);
									i := p + 1; p := i;
								end;
							'}':
								begin
									Assert(mode = ReadMul);
									if mode = refMode then result += Copy(s, i, p - i);
									mode := Text;
									i := p + 1;
									break;
								end;
							else inc(p);
						end;
				else begin Assert(no); break; end;
			end;

		result += Copy(s, i, p - i);
	end;

	function Hyphen(const text: string; start, pos: sint; out resumeAt: sint; out sym: UTFchar): boolean;
	var
		m3, m2, m1, here, p1, p2: UTFchar;
		clsm3, clsm2, clsm1, clshere, clsp1, clsp2: Symbol.Classification;
		mp, pp: sint;
		neol: size_t;
	begin
		pp := pos;
		here := UTF8.Next(text, pp);
		if here = ord('-') then
		begin
			sym := ord('-');
			resumeAt := pp;
			exit(yes);
		end;
		clshere := Symbol.Classify(here);

		if Whitespace in clshere then
		begin
			sym := UTFInvalid;
			if UTF8.IsEOL(pChar(text) + pos - 1, length(text) - pos + 1, neol) then resumeAt := pos + sint(neol) else resumeAt := pp;
			exit(yes);
		end;

		mp := pos;
		if mp > start then m1 := UTF8.Prev(text, mp) else m1 := UTFInvalid;
		clsm1 := Symbol.Classify(m1);

		if Whitespace in clsm1 then
		begin
			sym := UTFInvalid;
			resumeAt := pos;
			exit(yes);
		end;

		if (Vowel in clshere) or (Consonant in clshere) then
		begin
			if mp > start then m2 := UTF8.Prev(text, mp) else m2 := UTFInvalid;
			clsm2 := Symbol.Classify(m2);
			if mp > start then m3 := UTF8.Prev(text, mp) else m3 := UTFInvalid;
			clsm3 := Symbol.Classify(m3);

			// До:
			// Не отрывать одну букву: !с-тилет, !е-жевика
			// Не оставлять две согласных или согласную + ьъ: !пр-екр-асно,
			//   если только перед ними не гласная, а после — согласная: раст-вор, доль-ка. Две гласные можно: Маю-ши.
			if not (Letter in clsm1) or not (Letter in clsm2)
				or ((Consonant in clsm2) and (Consonant in clsm1) and not ((Consonant in clshere) and (Vowel in clsm3)))
			then
				exit(no);

			// После:
			// Не отрывать одну букву. !лет-о, !Франци-я.
			// Не оставлять две согласных или согласную + ьъ: !ли-тр, !ме-сто, !до-лька, если только перед ними не согласная, а после — гласная: ос-трый.
			// Не отрывать от конца английские слоги, оканчивающиеся на согласный + e: !ba-se (может прочитаться как один; или забить?).
			// Не ставить гласную после согласной. !пер-енос.
			resumeAt := pos;
			p1 := UTF8.Next(text, pp);
			p2 := UTF8.Next(text, pp);
			clsp1 := Symbol.Classify(p1);
			clsp2 := Symbol.Classify(p2);
			if not (Letter in clsp1)
				or ((Consonant in clshere) and not (Vowel in clsp1) and not ((Vowel in clsp2) and (Consonant in clsm1)))
				or (([Consonant, Latin] - clshere = []) and ((p1 = ord('e')) or (p1 = ord('E'))) and not (Letter in clsp2))
				or ((Consonant in clsm1) and (Vowel in clshere))
			then
				exit(no);

			sym := ord('-');
			exit(yes);
		end;
		result := no;
	end;

	function CapitalizeFirst(const text: string): string;
	var
		sym, csym: UTFchar;
		len: size_t;
	begin
		csym := Symbol.Capitalize(UTF8.Peek(text, 1, len, sym));
		if sym <> csym then result := StrStuff(text, 1, len, UTF8.CodepointToString(csym)) else result := text;
	end;

	function LowerCaseFirst(const text: string): string;
	var
		sym, lsym: UTFchar;
		len: size_t;
	begin
		lsym := Symbol.LowerCase(UTF8.Peek(text, 1, len, sym));
		if (sym <> lsym) and Symbol.IsLowerCase(UTF8.Peek(text, 1 + len)) then
			result := StrStuff(text, 1, len, UTF8.CodepointToString(lsym))
		else
			result := text;
	end;

	function FixupSentence(const text: string): string;
	var
		pos: sint;
		sym: UTFchar;
	begin
		result := text;
		if length(result) > 0 then
		begin
			pos := 1;
			sym := UTF8.Next(result, pos);
			if Symbol.IsLowerCase(sym) and ([Letter, Whitespace, Punctuation] * Symbol.Classify(UTF8.Peek(result, pos)) <> []) then
				result := StrStuff(result, 1, pos - 1, UTF8.CodepointToString(Symbol.Capitalize(sym)));

			pos := length(result) + 1;
			sym := UTF8.Prev(result, pos);
			if not (Punctuation in Symbol.Classify(sym)) then
				result += '.';
		end;
	end;

	function Symbol.Classify(const sym: UTFchar): Symbol.Classification;
	begin
		case sym of
			SPACE, TAB, NON_BREAKING_SPACE, IDEOGRAPHIC_SPACE: result := [Whitespace];
			UNIX_EOL, MAC_EOL: result := [Whitespace, Newline];
			ord('0') .. ord('9'): result := [Digit];
			ord('a') .. ord('z'), ord('A') .. ord('Z'):
				begin
					result := [Letter, Latin];
					case sym of
						ord('a'), ord('e'), ord('i'), ord('o'), ord('u'), ord('y'), ord('A'), ord('E'), ord('I'), ord('O'), ord('U'), ord('Y'): result += [Vowel];
						else result += [Consonant];
					end;
				end;
			ord('.'), ord('!'), ord(','), ord('?'), ord(':'), ord(';'), ord('-'), EM_DASH: result := [Punctuation];
			Cyrillics.CAP_IO, Cyrillics.IO: result := [Letter, Cyrillic, Vowel];
			Cyrillics.CAP_A .. Cyrillics.YA: result := [Letter, Cyrillic] + Cyrillics.ClassifyAYA(sym);
			FIRST_DIACRITIC .. LAST_DIACRITIC: result := [Diacritic];
			else
				result := [];
		end;
	end;

{$define iswhitespace_char :=
	((ord(sym) = ord(' ')) or (ord(sym) = ord(TabSym)) or (ord(sym) = UNIX_EOL) or (ord(sym) = MAC_EOL) or (ord(sym) = NON_BREAKING_SPACE))}
{$define isnewline_char := ((ord(sym) = UNIX_EOL) or (ord(sym) = MAC_EOL))}
	function Symbol.IsWhitespace(const sym: UTFchar): boolean;  begin result := iswhitespace_char or (sym = IDEOGRAPHIC_SPACE);       end;
	function Symbol.IsWhitespace(sym: char): boolean;           begin result := iswhitespace_char;                                    end;
	function Symbol.IsNewline(const sym: UTFchar): boolean;     begin result := isnewline_char;                                       end;
	function Symbol.IsNewline(sym: char): boolean;              begin result := isnewline_char;                                       end;
	function Symbol.IsDiacritic(const sym: UTFchar): boolean;   begin result := (sym >= FIRST_DIACRITIC) and (sym <= LAST_DIACRITIC); end;
	function Symbol.IsIdentifier(sym: char): boolean;           begin result := sym in IdentifierSyms; end;
{$undef iswhitespace_char} {$undef isnewline_char}

	function Symbol.Capitalize(const sym: UTFchar): UTFchar;
	begin
		case sym of
			ord('a') .. ord('z'): result := sym - (ord('a') - ord('A'));
			Cyrillics.A .. Cyrillics.YA: result := sym - (Cyrillics.A - Cyrillics.CAP_A);
			Cyrillics.IO: result := Cyrillics.CAP_IO;
			else result := sym;
		end;
	end;

	function Symbol.LowerCase(const sym: UTFchar): UTFchar;
	begin
		case sym of
			ord('A') .. ord('Z'): result := sym + (ord('a') - ord('A'));
			Cyrillics.CAP_A .. Cyrillics.CAP_YA: result := sym + (Cyrillics.A - Cyrillics.CAP_A);
			Cyrillics.CAP_IO: result := Cyrillics.IO;
			else result := sym;
		end;
	end;

	function Symbol.IsLowerCase(const sym: UTFchar): boolean;
	begin
		case sym of
			ord('a') .. ord('z'), Cyrillics.A .. Cyrillics.YA, Cyrillics.IO: result := yes;
			else result := no;
		end;
	end;

	function Symbol.IsUpperCase(const sym: UTFchar): boolean;
	begin
		case sym of
			ord('A') .. ord('Z'), Cyrillics.CAP_A .. Cyrillics.CAP_YA, Cyrillics.CAP_IO: result := yes;
			else result := no;
		end;
	end;

	function Symbol.Cyrillics.ClassifyAYA(const sym: UTFchar): Classification;
	begin
		case sym of
			CAP_A, CAP_IE, CAP_I, CAP_O, CAP_U, CAP_YERU, CAP_E, CAP_YU, CAP_YA, A, IE, I, O, U, YERU, E, YU, YA: result := [Vowel];
			CAP_HARD_SIGN, CAP_SOFT_SIGN, HARD_SIGN, SOFT_SIGN: result := [];
			else result := [Consonant];
		end;
	end;

end.
