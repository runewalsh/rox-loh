unit Utils;

{$define arrayofconst} {$include opts.inc}
{$include all_numbers.inc}
{$include all_vectors.inc}

interface

uses
	USystem, Errors, UMath;

const
	YesNo: array[boolean] of string = ('нет', 'да');
	YesNoEn: array[boolean] of string = ('no', 'yes');
	TrueFalse: array[boolean] of string = ('false', 'true');
	DefaultTokenSyms = ['A'..'Z', 'a'..'z', '0'..'9', '.', '+', '-', '_'];
	FloatTokenSyms = ['0' .. '9', '.', '+', '-', 'e', 'E'];
	IdentifierSyms = ['a'..'z', 'A'..'Z', '0'..'9', '_'];

type
	Tribool = object
		function Decide(default: boolean): boolean; cinline
		function StrictYes: boolean; cinline
		function StrictNo: boolean; cinline
		function Defined: boolean; cinline
		function Undefined: boolean; cinline
	private
		value: sint;
	public const
		Unknown: Tribool = (value: 0);
	end;
	operator :=(const x: boolean): Tribool;
	operator :=(const x: sint): Tribool;
	operator =(const a, b: Tribool): boolean;

type
	pStringView = ^StringView;
	StringView = object
		p: pChar;
		n: size_t;
		function Make(newP: pChar; newN: size_t): StringView; static; cinline
		function Make(const s: string): StringView; static; cinline
		function ToString: string; cinline
	const
		Empty: StringView = (p: nil; n: 0);
	end;
	StringViews = array of StringView;
	operator :=(s: pString): StringView; cinline
	operator =(const a: StringView; const b: string): boolean; cinline
	operator =(const a: string; const b: StringView): boolean; cinline
	operator =(const a, b: StringView): boolean; cinline
{$define func := function Concat(const _ARGS_: StringView): string;} {$include variadic.inc}

	function IfThen(cond: boolean; const yes, no: string): string; cinline
	function IfThen(cond: boolean; const yes: string): string; cinline
{$define numberf := function IfThen(cond: boolean; const yes, no: typ): typ; cinline} all_numbers
	function StrToInt(const s: string; const def: ilong = 0): ilong;
	function StrToFloat(const s: string; const def: hp_float = 0.0): hp_float;
{$define numberf := function TryParse(const s: string; out value: typ): boolean;} all_numbers
{$define intf := function TryParse(s: pChar; n: size_t; out value: typ): size_t;} all_ints

type
	IntFormatDesc = object
		function Base(newBase: uint): IntFormatDesc;
		function Padding(newPad: uint): IntFormatDesc;
		function Hex: IntFormatDesc;
	{$define intf := function Apply(const value: typ): string;} all_ints
	private
		nbase, pad: uint;
	end;

const
	IntFormat: IntFormatDesc = (nbase: 10; pad: 0);
{$define intf :=
	function ToString(const value: typ): string; overload;
	function ToString(const value: typ; const fmt: IntFormatDesc): string; overload;} all_ints

	function ToString(const p: FileSize): string; overload;

type
	FloatFormatDesc = object
	type
		rfloat_t = type string[48];
	var
		function Significant(signif: uint): FloatFormatDesc;
		function AfterPoint(ap: uint): FloatFormatDesc;
		function MaxAfterPoint(maxAp: uint): FloatFormatDesc;
		function MostPrecise: FloatFormatDesc;
		function Pattern(const newFmt: string): FloatFormatDesc;
		function Pattern(const newFmt: StringView): FloatFormatDesc;
		function Apply(const value: float): rfloat_t;
	{$define vecf := function Apply(const v: vec): string;} all_floating_vectors
		function Apply(const quat: Quaternion): string;
		function Apply(const tf: Transform): string;

	private
		signif, minAp, maxAp: uint;
		vecfmt: StringView; // 'a = (\x: x, \y: y, \z: z, \w: w)' × (1, 2, 3, 4) ⇒ 'a = (x: 1, y: 2, z: 3, w: 4)'
		                    // '(/, /)' ⇒ '(1, 2, 3)' (левая скобка / разделитель / правая скобка)
	type
		DefaultFunc      = function(obj: pointer; const fmt: FloatFormatDesc): string;
		GetComponentFunc = function(obj: pointer; const id: StringView; out value: float): boolean;
		FormatLsrFunc    = function(obj: pointer; const start, sep, &end: StringView; const fmt: FloatFormatDesc): string;
	var
		function FormatWithVecfmt(obj: pointer; default: DefaultFunc; getComponent: GetComponentFunc; formatLsr: FormatLsrFunc): string;
		function FormatWithLsrVecfmt(obj: pointer; formatLsr: FormatLsrFunc): string;
		function FormatWithGenericVecfmt(obj: pointer; getComponent: GetComponentFunc): string;
		function WrongVecfmt(const why: string): Exception;
	end;

const
	FloatFormat: FloatFormatDesc = (signif: 3; minAp: 0; maxAp: High(uint); vecfmt: (p: nil; n: 0));

	function ToString(const value: float): string; function ToString(const value: float; const fmt: FloatFormatDesc): string; overload;
{$define vecf := function ToString(const v: vec): string; function ToString(const v: vec; const fmt: FloatFormatDesc): string; overload;} all_floating_vectors
	function ToString(const quat: Quaternion): string; overload;
	function ToString(const quat: Quaternion; const fmt: FloatFormatDesc): string; overload;
	function ToString(const aabb: AABB): string; overload;
	function ToString(const sph: Sphere): string; overload;
	function ToString(const tf: Transform): string; function ToString(const tf: Transform; const fmt: FloatFormatDesc): string; overload;

	function ToStringSuff(const value, base: hp_float; const suffixes: array of string; firstSuffixPow: sint): string;
	function ToStringSuff(const value, base: hp_float; const fmt: FloatFormatDesc; const suffixes: array of string; firstSuffixPow: sint): string;
	function ToStringSuff(const value: hp_float): string;
	function ToStringSuff(const value: hp_float; const fmt: FloatFormatDesc): string;
	function ToStringSuff_b(const value: hp_float): string;
	function ToStringSuff_b(const value: hp_float; const fmt: FloatFormatDesc): string;

	function ToString(const rect: Rect): string; overload;
	function TimeToString(const time: hp_float): string;
	function ToMilliseconds(const time: float): uint;
	function ToString(const time: Ticks): string; overload;
	function ToString(const dt: DateTime; const fmt: string): string; overload;
	function ToString(const dt: DateTime): string; overload;
	function ToString(p: pointer): string; overload;
	function SizeToString(const size: UintVec2): string;
	function SizeToString(const size: UintVec3): string;
	function PrintableString(const s: string; allowNewline: boolean = no): string;
	function ToString(const v: System.TVarRec): string; overload;
	function MaskToString(const mask: uint; const flags: array of string; const values: array of uint): string;

const
	RPOS_START = Low(sint);

	function Pos(const substr, s: string; offset: sint; default: sint = 0): sint; overload;
	function Pos(ch: char; const s: string; offset: sint; default: sint = 0): sint; overload;
	function Pos(const substr: string; s: pChar; ns: size_t): size_t; overload;
	function Pos(ch: char; s: pChar; ns: size_t): size_t; overload;
	function Rpos(const substr, s: string; offset: sint = RPOS_START): sint;
	function StrReplace(const s, find, replace: string): string;

type
	StrReplaceFunc = function(id: uint; param: pointer): string;
	function StrReplace(const s: string; const find: array of string; replace: StrReplaceFunc; param: pointer): string;
	function StrDup(const s: string; times: sint): string;
	function StrStuff(const s: string; at, remove: sint; const what: string): string;

	function FindStr(const s: string; const arr: array of string; default: sint = -1): sint;
	function FindStr(out id: sint; const s: string; const arr: array of string; default: sint = -1): boolean;
	function ScanToken(const s: string; var pos: sint; const syms: charset_t = DefaultTokenSyms): string;
	function ScanToken(out tok: string; const s: string; var pos: sint; const syms: charset_t = DefaultTokenSyms): boolean;
	function ScanFloatToken(const s: string; var pos: sint; const def: hp_float = 0.0): hp_float;

	function StrEq(a, b: pChar; len: size_t): boolean;
	function StrEq(s: pChar; len: size_t; const ref: string): boolean;
	function StrEq(a: pChar; na: size_t; b: pChar; nb: size_t): boolean;
	function IsSuffix(const suffix, s: string): boolean;
	function CutSuffix(const suffix, s: string; rest: pString): boolean;
	function CutSuffix(const suffix, s: string): string;
	function CutAffixes(const prefix, s, suffix: string; rest: pString): boolean;
	function CutAffixes(const prefix, s, suffix: string): string;
	function CutFragmentWithAffixes(var s: string; const prefix, suffix: string; out fragment: string): boolean;

type
	Affix = (Prefix, Suffix);
	function CommonAffixLength(const a, b: string; what: Affix; limit: size_t): size_t;
	function GetAffix(const s: string; len: size_t; what: Affix): string;
	procedure AppendAffixTo(var s: string; const affix: string; what: Affix);
	function CommonAffix(s: pString; count: sint; what: Affix): string;

type
	StringOptionals = object
	type
		HandlerProc = procedure(id: uint; const value: StringView; param: pointer);
		StringHandlerProc = procedure(id: uint; const value: string; param: pointer);
	var
		function Split(const src: string; const opts: array of string; handler: HandlerProc; param: pointer): StringView; static;
	end;

type
	GetIndexedString = function(id: uint; param: pointer): string;
	GetIndexedStringView = function(id: uint; param: pointer): StringView;

type
	SeparatedList = object
		function Join(const strs: array of string; const sepx: string): string; static;
		function Join(const strs: array of StringView; const sepx: string): string; static;
		function Join(n: sint; getString: GetIndexedString; param: pointer; const sepx: string): string; static;
		function Join(n: sint; getString: GetIndexedStringView; param: pointer; const sepx: string): string; static;

	const
		LastSep = '/l';
		Prefix  = '/b';
		Suffix  = '/a';
		Empty   = '/e';

	protected const
		_LASTSEP     = 0;
		_PREFIX      = 1;
		_SUFFIX      = 2;
		_EMPTY       = 3; LAST_OPTION = _EMPTY;
	type
		OptionID = 0 .. LAST_OPTION;

	public const
		Options: array[OptionID] of string = (LastSep, Prefix, Suffix, Empty);
	end;

	function GetStringViewFromStringArray(id: uint; param: pointer): StringView;
	function GetStringViewFromStringViewArray(id: uint; param: pointer): StringView;
	function GetStringFromSintArray(id: uint; param: pointer): string;
	function GetStringFromUintArray(id: uint; param: pointer): string;
	function GetStringFromArrayOfConst(id: uint; param: pointer): string;
	function PadLeft(const s: string; minLen: sint): string;
	function PadRight(const s: string; minLen: sint): string;
	function PadLeft(const s: string; pad: char; minLen: sint): string;
	function PadLeft(const s: string; const pad: string; minLen: sint): string;
	function MaybeQuote(const s: string): string;
	function Split(const s: string; const seps: charset_t): Strings;
	function SplitIntoViews(const s: string; const seps: charset_t): StringViews;
	function SafeIndex(const s: array of string; index: sint): string;
	function SafeIndex(const s: array of StringView; index: sint): StringView;
	function WrapNonempty(const s: string; const prefix, postfix: string): string;

	function Format(s: pChar; ns: size_t; nArgs: uint; getArg: GetIndexedStringView; param: pointer): string;
	function Format(s: pChar; ns: size_t; nArgs: uint; getArg: GetIndexedString; param: pointer): string;
	function Format(const s: string; nArgs: uint; getArg: GetIndexedStringView; param: pointer): string;
	function Format(const s: string; const args: array of string): string;
	function Format(const s: string; const args: array of const): string;
{$define func:=
	function Format(const s: string; const _ARGS_: string): string; overload;
	function Error(const s: string; const _ARGS_: string): Exception; overload;} {$include variadic.inc}
	function Error(const s: string; const args: array of const): Exception; overload;

type
	Range = object
		L, R: uint;
		function Open(L, R: uint): Range; static;
		function Closed(L, R: uint): Range; static;
		function Open(n: uint): Range; static;

	{$define range__maybe_accept_params := {$ifdef param}; param: pointer {$endif} {$ifdef param2}; param2: pointer {$endif}}
	{$define range__maybe_pass_params := {$ifdef param}, param {$endif} {$ifdef param2}, param2 {$endif}}

	{$define range__for_each_callback :=
		{$define callback_type := Callback} {$define param} one {$undef param}
		{$define callback_type := UnparametrizedCallback} one
		{$define callback_type := CallbackWithTwoParams} {$define param} {$define param2} one {$undef param} {$undef param2}
		{$undef callback_type} {$undef one}}

	{$define range__for_each_predicate :=
		{$define predicate_type := Predicate} {$define param} one {$undef param}
		{$define predicate_type := UnparametrizedPredicate} one
		{$define predicate_type := PredicateWithTwoParams} {$define param} {$define param2} one {$undef param} {$undef param2}
		{$undef predicate_type} {$undef one}}

	{$define one :=
	type
		callback_type = procedure(index: uint range__maybe_accept_params);
	var
		procedure Each(proc: callback_type range__maybe_accept_params);} range__for_each_callback

	{$define one :=
	type
		predicate_type = function(index: uint range__maybe_accept_params): boolean;
	var
		function Any(pred: predicate_type range__maybe_accept_params): boolean;
		function All(pred: predicate_type range__maybe_accept_params): boolean;
		function Find(pred: predicate_type range__maybe_accept_params): sint;
		function Matching(pred: predicate_type range__maybe_accept_params): uint;} range__for_each_predicate
	end;

	Range2D = object
		L, R: UintVec2;
		function Open(const L, R: UintVec2): Range2D; static;
		function Open(const R: UintVec2): Range2D; static;

	{$define range2d__for_each_callback :=
		{$define callback_type := Callback} {$define param} one {$undef param}
		{$define callback_type := ExtraIndexCallback} {$define param} {$define extra_index} one {$undef extra_index} {$undef param}
		{$define callback_type := UnparametrizedCallback} one
		{$undef callback_type} {$undef one}}

	{$define range2d__for_each_predicate :=
		{$define predicate_type := Predicate} {$define param} one {$undef param}
		{$define predicate_type := ExtraIndexPredicate} {$define param} {$define extra_index} one {$undef extra_index} {$undef param}
		{$define predicate_type := UnparametrizedPredicate} one
		{$undef predicate_type} {$undef one}}

	{$define one :=
	type
		callback_type = procedure(x, y {$ifdef extra_index}, i {$endif}: uint range__maybe_accept_params);
	var
		procedure Each(proc: callback_type range__maybe_accept_params);} range2d__for_each_callback

	{$define one :=
	type
		predicate_type = function(x, y {$ifdef extra_index}, i {$endif}: uint range__maybe_accept_params): boolean;
	var
		function Any(pred: predicate_type range__maybe_accept_params): boolean;
		function All(pred: predicate_type range__maybe_accept_params): boolean;
		function Matching(pred: predicate_type range__maybe_accept_params): uint;} range2d__for_each_predicate
	end;

implementation

uses
	Algo, UClasses, Human {$ifdef Profile}, Profile {$endif} {$ifdef selftest}, TextProcessing, Tests {$endif} {$ifdef Debug}, ULog {$endif};

	function Tribool.Decide(default: boolean): boolean; begin if value = 0 then result := default else result := value > 0; end;
	function Tribool.StrictYes: boolean;                begin result := value > 0; end;
	function Tribool.StrictNo: boolean;                 begin result := value < 0; end;
	function Tribool.Defined: boolean;                  begin result := value <> 0; end;
	function Tribool.Undefined: boolean;                begin result := value = 0; end;
	operator :=(const x: boolean): Tribool;             begin if x then result.value := 1 else result.value := -1; end;
	operator :=(const x: sint): Tribool;                begin if x = 0 then result := Tribool.Unknown else result := x > 0; end;
	operator =(const a, b: Tribool): boolean;           begin result := a.value = b.value; end;

	function StringView.Make(newP: pChar; newN: size_t): StringView;
	begin
		result.p := newP;
		result.n := newN;
	end;

	function StringView.Make(const s: string): StringView;     begin result := Make(pChar(s), length(s)); end;
	function StringView.ToString: string;                      begin result := USystem.ToString(p, n); end;
	operator :=(s: pString): StringView;                       begin result := StringView.Make(pChar(s^), length(s^)); end;
	operator =(const a: StringView; const b: string): boolean; begin result := StrEq(a.p, a.n, b); end;
	operator =(const a: string; const b: StringView): boolean; begin result := StrEq(b.p, b.n, a); end;
	operator =(const a, b: StringView): boolean;               begin result := StrEq(a.p, a.n, b.p, b.n); end;

{$define func :=
	function Concat(const _ARGS_: StringView): string;
	var
		p: size_t;
	begin
		SetLength(result, {$define conv := _ARG_.n} {$define sep := +} _ARGS_);
		p := 0; {$define iterate := memcpy(_ARG_.p, pChar(result) + p, _ARG_.n * sizeof(char)); {$ifndef _LAST_} p += _ARG_.n; {$endif}} _FOREACH_
	end;} {$include variadic.inc}

{$define impl :=
	function IfThen(cond: boolean; const yes {$ifndef no}, no {$endif}: typ): typ;
	begin
		if cond then result := yes else result := no;
	end; {$ifndef keep_typ} {$undef typ} {$endif} {$undef no}}
	{$define typ := string} impl
	{$define typ := string} {$define no := ''} impl
	{$define keep_typ} {$define numberf := impl} all_numbers {$undef keep_typ}
{$undef impl}

	function StrToInt(const s: string; const def: ilong = 0): ilong;
	var
		valCode: word;
	begin
		val(s, result, valCode);
		if valCode <> 0 then result := def;
	end;

	function StrToFloat(const s: string; const def: hp_float = 0.0): hp_float;
	var
		valCode: word;
	begin
		val(s, result, valCode);
		if valCode <> 0 then result := def;
	end;

	function IntFormatDesc.Base  (newBase: uint): IntFormatDesc; begin result := self; result.nbase := newBase; end;
	function IntFormatDesc.Padding(newPad: uint): IntFormatDesc; begin result := self; result.pad   := newPad;  end;
	function IntFormatDesc.Hex                  : IntFormatDesc; begin result := self; result.nbase := 16;      end;

{$define intf :=
	function IntFormatDesc.Apply(const value: typ): string;
	const
		Digits = string('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ');
	var
		d: array[0 .. bitsizeof(value) - 1] of char;
		dn, len, i, digit: uint;
		t: typ;
	{$ifdef signed} minus: boolean; {$endif}
	begin
	{$ifdef signed} minus := value < 0; {$endif}

		t := value;
		dn := 0;
	{$ifdef signed} d[High(d) - dn] := Digits[1 + abs(t mod typ(nbase))]; t := abs(t div typ(nbase)); inc(dn); if t > 0 then {$endif}
		repeat
			digit := t mod typ(nbase);
			d[High(d) - dn] := Digits[1 + digit]; inc(dn);
			t := t div typ(nbase);
		until t = 0;
		len := dn;
	{$ifdef signed} if minus then inc(len); {$endif}
		if pad > len then len := pad;
		SetLength(result, len);
		memcpy(@d[High(d) - dn + 1], @result[uint(length(result)) - dn + 1], dn * sizeof(char));
		i := 1;
	{$ifdef signed} if minus then begin result[i] := '-'; inc(i); end; {$endif}
		for i := i to uint(length(result)) - dn do
			result[i] := '0';
	end;

	function ToString(const value: typ): string;                           begin result := IntFormat.Apply(value); end;
	function ToString(const value: typ; const fmt: IntFormatDesc): string; begin result := fmt.Apply(value); end;}
	all_ints

	function ToString(const p: FileSize): string; begin result := ToStringSuff_b(1.0 * p.value); end;

	function FloatFormatDesc.Significant(signif: uint)    : FloatFormatDesc; begin result := self; result.signif := signif;     end;
	function FloatFormatDesc.AfterPoint(ap: uint)         : FloatFormatDesc; begin result := self; result.minAp  := RangeCheck(ap, 4, 'FloatFormat.MinAfterPoint'); result.maxAp := ap; end;
	function FloatFormatDesc.MaxAfterPoint(maxAp: uint)   : FloatFormatDesc; begin result := self; result.maxAp  := maxAp;      end;
	function FloatFormatDesc.MostPrecise                  : FloatFormatDesc; begin result := self; result.signif := High(uint); end;
	function FloatFormatDesc.Pattern(const newFmt: string): FloatFormatDesc; begin result := self; result.vecfmt := @newFmt;    end;
	function FloatFormatDesc.Pattern(const newFmt: StringView): FloatFormatDesc; begin result := self; result.vecfmt := newFmt; end;

	function FloatFormatDesc.Apply(const value: float): rfloat_t;
	var
		t: rfloat_t absolute result;
		pPos, ePos, pow, first, ed, lenLimit: sint;
		afterPt, sig, i: sint;

	begin
	trace_call('FloatFormat.Apply');
		System.Str(value, t);
		afterPt := minAp;

		// Ожидается специальное значение либо <минус?><X.YZWABC...>E±<XY...>
		pPos := 2 + Index('.', @t[2], length(t) - 1);
		ePos := Index1Rev('E', @t[1], length(t));
		if (pPos > 0) and (ePos > pPos) and (TryParse(@t[ePos + 1], length(t) - ePos, pow) = size_t(length(t) - ePos))
		// с начала до ePos - 1 — X.YZWABC..., причём X = 0 только при 0.000000... .
		then
		begin
			// Сдвинуть запятую вправо (pow > 0)?
			// Начальный предел — длина, предложенная самой экспоненциальной формой.
			// Например, 1.2345E+02: до 4-х позиций, ePos - pPos = 5, т. о. условие — pow < Limit(ePos - pPos), иначе оставить экспоненциальную.
			// Предельное число значащих цифр — ePos - pPos.

			// Сдвинуть влево (pow < 0)?
			// Например, 1.2345E-06 (вспомогательные значения те же).
			// Если возможно сдвинуть запятую влево так, чтобы осталось sig значащих цифр — сдвинуть, иначе откатиться на экспоненциальную.
			// Останется значащих цифр для заданного ограничения длины — Limit(ePos - pPos) + pow.
			// Если это значение меньше sig, т. е. Limit(ePos - pPos) < sig - pow, откатиться к экспоненциальной форме.
			sig := min(signif, uint(ePos - pPos));
			lenLimit := 2 + uint(ePos - pPos);

			if ((pow >= 0) and (pow < lenLimit)) or ((pow < 0) and (lenLimit >= sig - pow)) then
			begin
				if (pow < 0) or (sig > pow) then afterPt := min(uint(sig - pow - 1), maxAp);
				System.Str(value: 0: afterPt, t);
			end else
				t[ePos] := 'e';
		end;

		first := 1; ed := length(t) + 1;
		while (first <= length(t)) and (t[first] = ' ') do inc(first);

		for i := minAp to afterPt do
			if (i < afterPt) and (ed > first) and (t[ed - 1] = '0') then dec(ed) else
			begin
				if (ed > first) and (t[ed - 1] = '.') then dec(ed);
				break;
			end;
		delete(t, ed, length(t) - ed + 1);
		delete(t, 1, first - 1);
	leave_call
	end;

{$define vecf :=
	{$if veclen = 2}     {$define DefaultVec := DefaultVec2} {$define GetVecComponent := GetVec2Component} {$define LsrFormatVec := LsrFormatVec2}
	{$elseif veclen = 3} {$define DefaultVec := DefaultVec3} {$define GetVecComponent := GetVec3Component} {$define LsrFormatVec := LsrFormatVec3}
	{$elseif veclen = 4} {$define DefaultVec := DefaultVec4} {$define GetVecComponent := GetVec4Component} {$define LsrFormatVec := LsrFormatVec4}
	{$else} {$error wrong veclen} {$endif}

	function DefaultVec(v: pvec; const fmt: FloatFormatDesc): string;
	begin
		result := '(' + {$define one := fmt.Apply(v^.item)} {$define op := + ', ' +} reduce_vec + ')';
	end;

	function GetVecComponent(v: pvec; const id: StringView; out value: float): boolean;
	begin
	{$define iterate := if id = itemname then begin value := v^.item; result := yes; end else} foreach_component
		result := no;
	end;

	function LsrFormatVec(v: pvec; const start, sep, &end: StringView; const fmt: FloatFormatDesc): string;
	var
		{$define one := item} comma_separated: FloatFormatDesc.rfloat_t;
	begin
		{$define iterate := item := fmt.Apply(v^.item);} foreach_component
		result := Concat(start, {$define one := StringView.Make(@item[1], length(item))} {$define op :=, sep,} reduce_vec, &end);
	end;

	function FloatFormatDesc.Apply(const v: vec): string;
	begin
		result := FormatWithVecfmt(@v, DefaultFunc(@DefaultVec), GetComponentFunc(@GetVecComponent), FormatLsrFunc(@LsrFormatVec));
	end; {$undef DefaultVec} {$undef GetVecComponent} {$undef LsrFormatVec}}
	all_floating_vectors

	function FloatFormatDesc.Apply(const quat: Quaternion): string; begin result := Apply(quat.v4); end;

type
	GetTransformItemParam = record
		ff: ^FloatFormatDesc;
		tf: ^Transform;
	end;

	function GetTransformItem(id: uint; param: pointer): string;
	var
		p: ^GetTransformItemParam absolute param;
	begin
		case id of
			0: if p^.tf^.tr.IsZero                 then result := '' else result := 'translate = ' + p^.ff^.Apply(p^.tf^.tr);
			1: if p^.tf^.rot = Quaternion.Identity then result := '' else result := 'rotate = ' + p^.ff^.Apply(p^.tf^.rot);
			2: if Equals(p^.tf^.scale, 1)          then result := '' else result := 'scale = ' + p^.ff^.Apply(p^.tf^.scale);
		{$ifdef Debug} else raise ExhaustiveCase(id, 'GetTransformItem.id'); {$endif}
		end;
	end;

	function FloatFormatDesc.Apply(const tf: Transform): string;
	var
		p: GetTransformItemParam;
	begin
		p.ff := @self;
		p.tf := @tf;
		result := SeparatedList.Join(3, @GetTransformItem, @p, ', ' + SeparatedList.Empty + 'нет');
	end;

	function FloatFormatDesc.FormatWithVecfmt(obj: pointer; default: DefaultFunc; getComponent: GetComponentFunc; formatLsr: FormatLsrFunc): string;
	var
		i: uint;
	begin
		if vecfmt.n = 0 then exit(default(obj, self));
		// Строка может быть в форме начало/разделитель/конец либо "[R: x, G: y, B: z]".
		i := 0;
		while i < vecfmt.n do
		begin
			case vecfmt.p[i] of
				'\': inc(i);
				'/': exit(FormatWithLsrVecfmt(obj, formatLsr));
			end;
			inc(i);
		end;
		result := FormatWithGenericVecfmt(obj, getComponent);
	end;

	function FloatFormatDesc.FormatWithLsrVecfmt(obj: pointer; formatLsr: FormatLsrFunc): string;
	var
		i, bufPos: uint;
		buf: string;
		startSepEnd: array[0 .. 2] of StringView;
		cur: sint;
	begin
		i := 0;
		SetLength(buf, vecfmt.n); bufPos := 0;
		cur := 0;
		startSepEnd[cur] := StringView.Make(pChar(buf) + bufPos, 0);

		while i < vecfmt.n do
		begin
			case vecfmt.p[i] of
				'\':
					begin
						inc(i);
						if i = vecfmt.n then raise WrongVecfmt('висящая \');
					end;
				'/':
					begin
						inc(cur);
						if cur = length(startSepEnd) then raise WrongVecfmt('ожидается .../.../...');
						startSepEnd[cur] := StringView.Make(pChar(buf) + bufPos, 0);
						inc(i); continue;
					end;
			end;
			Assert(bufPos < uint(length(buf)));
			pChar(buf)[bufPos] := vecfmt.p[i]; inc(bufPos); inc(startSepEnd[cur].n);
			inc(i);
		end;
		for i := cur + 1 to High(startSepEnd) do startSepEnd[i] := StringView.Empty;
		result := formatLsr(obj, startSepEnd[0], startSepEnd[1], startSepEnd[2], self);
	end;

	function FloatFormatDesc.FormatWithGenericVecfmt(obj: pointer; getComponent: GetComponentFunc): string;
	var
		i, rp, np: uint;
		cid: StringView;

		procedure flushcid;
		var
			x: float;
			t: rfloat_t;
			np: uint;
			cp: StringView;
		begin
			if cid.n = 0 then exit;
			if getComponent(obj, cid, x) then
			begin
				t := Apply(x);
				cp := StringView.Make(@t[1], length(t));
			end else
				cp := cid;

			np := rp + cp.n; if np > uint(length(result)) then SetLength(result, 2 * np);
			memcpy(cp.p, pChar(result) + (rp - 1), cp.n * sizeof(char));
			rp := np;
			cid.n := 0;
		end;

	begin
		SetLength(result, vecfmt.n + 40); rp := 1;
		i := 0;
		cid.n := 0;
		while i < vecfmt.n do
		begin
			case vecfmt.p[i] of
				'\':
					begin
						flushcid;
						inc(i);
						if i = vecfmt.n then raise WrongVecfmt('висящая \');
					end;
				'a' .. 'z':
					begin
						if cid.n = 0 then cid.p := vecfmt.p + i;
						inc(cid.n);
						inc(i);
						continue;
					end;
				else
					flushcid;
			end;
			np := rp + 1; if np > uint(length(result)) then SetLength(result, 2 * np);
			pChar(result)[rp-1] := vecfmt.p[i];
			rp := np;
			inc(i);
		end;
		flushcid;
		SetLength(result, rp - 1);
	end;

	function FloatFormatDesc.WrongVecfmt(const why: string): Exception; begin result := Error('Неверный формат {0}: {1}.', vecfmt.ToString, why); end;
{$define ts :=
	function ToString(const param: typ): string; begin result := FloatFormat.Apply(param); end;
	function ToString(const param: typ; const fmt: FloatFormatDesc): string; begin result := fmt.Apply(param); end;
	{$undef typ} {$undef param}}

	{$define typ := float}   {$define param := value} ts
	{$define vecf := {$define param := v} {$define typ := vec} ts} all_floating_vectors
	{$define typ := Quaternion} {$define param := quat} ts
	{$define typ := Transform} {$define param := tf} ts
{$undef ts}

	function ToStringSuff(const value, base: hp_float; const suffixes: array of string; firstSuffixPow: sint): string;
	begin
		result := ToStringSuff(value, base, FloatFormat, suffixes, firstSuffixPow);
	end;

	function ToStringSuff(const value, base: hp_float; const fmt: FloatFormatDesc; const suffixes: array of string; firstSuffixPow: sint): string;
	var
		fpow: hp_float;
		suffixPow: sint;
		ok: boolean;
	begin
		Assert(length(suffixes) > 0, 'nsuffixes = 0');
		fpow := logn(base, value);
		ok := FloatIs.Finite(fpow); // логарифм выдаст что-то уровня бесконечности и на value = 0
		if ok then
		begin
			suffixPow := clamp(ifloor(0.05 + fpow) - firstSuffixPow, 0, High(suffixes));
			ok := ok and (suffixes[suffixPow] <> '');
		end;

		if ok then
			result := ToString(value * pow(base, -(suffixPow + firstSuffixPow)), fmt) + suffixes[suffixPow]
		else
			result := ToString(value, fmt);
	end;

	function ToStringSuff(const value: hp_float): string;
	begin
		result := ToStringSuff(value, FloatFormat);
	end;

	function ToStringSuff(const value: hp_float; const fmt: FloatFormatDesc): string;
	const
	{$define s := string}
		Suffixes: array[-3 .. 6] of string = (s('n'), s('mc'), s('m'), '', s('K'), s('M'), s('G'), s('T'), s('P'), s('E'));
	{$undef s}
	begin
		result := ToStringSuff(value, 1000, fmt, Suffixes, Low(Suffixes));
	end;

	function ToStringSuff_b(const value: hp_float): string;
	begin
		result := ToStringSuff_b(value, FloatFormat);
	end;

	function ToStringSuff_b(const value: hp_float; const fmt: FloatFormatDesc): string;
	const
	{$define s := string}
		Suffixes: array[1 .. 6] of string = (s('K'), s('M'), s('G'), s('T'), s('P'), s('E'));
	{$undef s}
	begin
		result := ToStringSuff(value, 1024, fmt, Suffixes, Low(Suffixes)) + 'b';
	end;

	function ToString(const aabb: AABB): string;
	begin
		result := ToString(aabb.A) + '~' + ToString(aabb.B);
	end;

	function ToString(const sph: Sphere): string;
	begin
		result := 'O' + ToString(sph.center) + ', R = ' + ToString(sph.radius);
	end;

	function ToString(const rect: Rect): string;
	begin
		result := ToString(rect.A) + ', ' + ToString(rect.B);
	end;

	function TimeToString(const time: hp_float): string;
	const
		Parts: array[0 .. 10] of record
			divisor: hp_float;
			suffix: string;
			partsLimit: sint;
		end =
		(
			(divisor: 86400 * 365.25 * 1000000000; suffix: 'млрд.л'; partsLimit: 3),
			(divisor: 86400 * 365.25 * 1000000; suffix: 'млн.л'; partsLimit: 3),
			(divisor: 86400 * 365.25 * 1000; suffix: 'тыс.л'; partsLimit: 3),
			(divisor: 86400 * 365.25;        suffix: 'г';  partsLimit: 3),
			(divisor: 86400;                 suffix: 'дн';  partsLimit: 3),
			(divisor: 3600;                  suffix: 'ч';  partsLimit: 3),
			(divisor: 60;                    suffix: 'мин';  partsLimit: 3),
			(divisor: 1;                     suffix: 'с';  partsLimit: 3),
			(divisor: 1e-3;                  suffix: 'мс'; partsLimit: 1),
			(divisor: 1e-6;                  suffix: 'мкс'; partsLimit: 1),
			(divisor: 1e-9;                  suffix: 'нс'; partsLimit: 1)
		);
	var
		t: hp_float;
		i, nParts, x: sint;
		last: boolean;
	begin
		result := '';
		t := time;
		nParts := 0;

		for i := 0 to High(Parts) do
		begin
			if t < Parts[i].divisor then continue;
			if nParts >= Parts[i].partsLimit then break;
			inc(nParts);
			last := (nParts = 1) and ((nParts >= Parts[i].partsLimit) or ((i < High(Parts)) and (nParts >= Parts[i + 1].partsLimit)));

			if result <> '' then result += ' ';
			if last then result += ToString(t / Parts[i].divisor) else
			begin
				x := trunc(t / Parts[i].divisor);
				result += ToString(x);
				t -= x * Parts[i].divisor;
			end;
			result += ' ' + Parts[i].suffix;
			if last then break;
		end;
		if nParts = 0 then result := ToString(t) + ' с';
	end;

	function ToMilliseconds(const time: float): uint;
	begin
		Assert((time >= 0) and (1000 * time <= High(result)), ToString(time));
		result := round(1000 * time);
	end;

	function ToString(const time: Ticks): string;
	begin
		result := TimeToString(time.ToSeconds);
	end;

	function DateTimeReplaceFunc(id: uint; param: pointer): string;
	const
		Fmt00: IntFormatDesc = (nbase: 10; pad: 2);
	var
		dt: ^DateTime absolute param;
	begin
		case id of
			0: {D.M.Y} result := ToString(1 + dt^.day, Fmt00) + '.' + ToString(1 + dt^.month, Fmt00) + '.' + ToString(dt^.year);
			1: {h:m:s} result := ToString(dt^.hour, Fmt00) + ':' + ToString(dt^.min, Fmt00) + ':' + ToString(dt^.sec, Fmt00);
			2: {D}     result := ToString(1 + dt^.day);
			3: {Y}     result := ToString(dt^.year);
			4: {h}     result := ToString(dt^.hour, Fmt00);
			5: {m}     result := ToString(dt^.min, Fmt00);
			6: {s}     result := ToString(dt^.sec, Fmt00);
			7: {ms}    result := ToString(dt^.msec, IntFormat.Padding(3));
		end;
	end;

	function ToString(const dt: DateTime; const fmt: string): string;
	const
		Reps: array[0 .. 7] of string = ('{D.M.Y}', '{h:m:s}', '{D}', '{Y}', '{h}', '{m}', '{s}', '{ms}');
	begin
		result := StrReplace(fmt, Reps, @DateTimeReplaceFunc, @dt);
	end;

	function ToString(const dt: DateTime): string; begin result := ToString(dt, '{D.M.Y} {h:m:s}'); end;

{$define numberf :=
	function TryParse(const s: string; out value: typ): boolean;
	var
		valCode: word;
	begin
		val(s, value, valCode);
		result := valCode = 0;
	end;} all_numbers

{$define intf :=
	function TryParse(s: pChar; n: size_t; out value: typ): size_t;
	type
		utype = {$ifdef signed} unsigned_pair {$else} typ {$endif};
	var
		digits: uint;
		u, nu: utype;
	{$ifdef signed} var neg: boolean; {$endif}
	begin
		result := 0;
		while (n > 0) and Symbol.IsWhitespace(s^) do begin inc(s); dec(n); inc(result); end;
	{$ifdef signed} neg := (n > 0) and (s[0] = '-'); if neg then begin inc(s); dec(n); inc(result); end; {$endif}
		if {$ifdef signed} not neg and {$endif} (n > 0) and (s[0] = '+') then begin inc(s); dec(n); inc(result); end;

		u := 0;
		digits := 0;
		while (n > 0) and (s[0] >= '0') and (s[0] <= '9') do
		begin
		unchecked nu := u * 10 + utype(ord(s[0]) - ord('0')); end_unchecked
			if nu < u then exit(0);
			u := nu;
			inc(s); dec(n); inc(result); inc(digits);
		end;

		if (digits = 0) {$ifdef signed} or (not neg and (u > utype(High(value)))) or (neg and (u > utype(-Low(value)))) {$endif} then exit(0);
	{$ifdef signed} unchecked if neg then value := -u else value := u; end_unchecked {$else} value := u; {$endif}
	end;} all_ints

	function ToString(p: pointer): string;
	begin
		result := '$' + HexStr(p);
	end;

	function SizeToString(const size: UintVec2): string;
	begin
		result := ToString(size.x) + ' x ' + ToString(size.y);
	end;

	function SizeToString(const size: UintVec3): string;
	begin
		result := ToString(size.x) + ' x ' + ToString(size.y);
		if size.z <> 1 then result += ' x ' + ToString(size.z);
	end;

	function PrintableString(const s: string; allowNewline: boolean = no): string;

		function Printable(sym: UTFchar): boolean;
		begin
			result := (sym >= 32) or (sym = ord(TabSym)) or (allowNewline and ((sym = ord(EOL)) or (sym = ord(Carriage))));
		end;

	var
		p, start, endp: sint;
		sym: UTFchar;
		sb: StringBuilder;
		bytechain: boolean;

		function Validate: boolean;
		begin
			result := UTF8.Validate(@s[endp], p - endp);
			if not result then
			begin
				p := endp;
				sym := UTFInvalid;
			end;
		end;
	begin
		if s = '' then exit('(пустая строка)');
		p := 1;
		start := 1;
		bytechain := no;
		sb.Init;
		repeat
			start := p;
			repeat
				endp := p;
			until (UTF8.Next(s, p, sym) = UTFInvalid) or not Validate or not Printable(sym);
			if endp > start then
			begin
				if (start = 1) and (endp > length(s)) then begin sb.Done; exit(s); end; // оптимизация случая, когда в строке всё в порядке
				bytechain := no;
				sb.Append(pChar(s) + (start - 1), endp - start);
			end;

			if sym <> UTFInvalid then
			begin
				bytechain := no;
				sb.Append('#', ToString(sym, IntFormat.Hex))
			end else
			begin
				if p > length(s) then break;
				if not bytechain then sb.Append('/');
				bytechain := yes;
				sb.Append(ToString(ord(s[p]), IntFormat.Hex.Padding(2)), '/');
				inc(p);
			end;
		until no;
		result := sb.DestructiveToString;
	end;

	function ToString(const v: System.TVarRec): string;
	begin
		case v.VType of
			vtInteger: result := ToString(v.VInteger);
			vtBoolean: result := YesNo[v.VBoolean];
			vtChar:    result := v.VChar;
			vtWideChar: result := '(wc) ' + UTF8Encode(widestring(v.VWideChar));
			vtExtended: result := ToString(v.VExtended^);
			vtString:   result := v.VString^;
			vtPointer:  result := ToString(v.VPointer);
			vtPChar:    result := '(pc) ' + string(v.VPChar);
			vtObject:   if Assigned(v.VObject) then result := v.VObject.ClassName else result := 'TObject(nil)';
			vtClass:    if Assigned(v.VClass) then result := v.VClass.ClassName else result := 'TClass(nil)';
			vtPWideChar: result := '(pwc) ' + UTF8Encode(widestring(v.VPWideChar));
			vtAnsiString: result := ansistring(v.VAnsiString);
			vtCurrency:   result := '(curr) ' + ToString(v.VCurrency);
			vtVariant:    result := '(variant)';
			vtInterface:  result := '(interface)';
			vtWideString: result := '(ws) ' + UTF8Encode(widestring(v.VWideString));
			vtInt64:      result := ToString(v.VInt64^);
			vtUnicodeString: result := '(us) ' + UTF8Encode(pUnicodeString(v.VUnicodeString)^);
			vtQWord:      result := ToString(v.VQWord^);
			else          result := '(VType ' + ToString(v.VType) + ')';
		end;
	end;

	function MaskToString(const mask: uint; const flags: array of string; const values: array of uint): string;
	var
		i: sint;
		m: uint;
	begin
		if length(flags) <> length(values) then
			raise Error('MaskToString({0}) не соответствует длине массива ({1}).', [SeparatedList.Join(flags, ', '), length(values)]);
		m := mask;
		result := '';
		for i := 0 to High(flags) do
			if values[i] and m <> 0 then
			begin
				m := m and not values[i];
				ContinueString(result, flags[i], ', ');
			end;

		if (result = '') or (m <> 0) then
			ContinueString(result, ToString(m, IntFormat.Hex), ' + ');
	end;

	function Pos(const substr, s: string; offset: sint; default: sint = 0): sint;
	var
		i: sint;
	begin
		if offset = 0 then offset := 1;
		for i := offset to length(s) - length(substr) + 1 do
			if CompareByte(s[i], pChar(substr)^, length(substr)) = 0 then
				exit(i);
		result := default;
	end;

	function Pos(ch: char; const s: string; offset: sint; default: sint = 0): sint;
	begin
		Assert((offset > 0) and (offset <= length(s) + 1), Format('Смещение ({0}) вне границ [1 .. {1}]', [offset, length(s) + 1]));
		result := Index(ch, pChar(s) + (offset - 1), length(s) + 1 - offset);
		if result >= 0 then result += offset else result := default;
	end;

	function Pos(const substr: string; s: pChar; ns: size_t): size_t;
	var
		last: size_t;
	begin
		if ns >= size_t(length(substr)) then
		begin
			last := ns - length(substr);
			result := 0;
			while result <= last do
			begin
				if CompareByte(s[result], pChar(substr)^, length(substr) * sizeof(char)) = 0 then exit;
				inc(result);
			end;
		end;
		result := not size_t(0);
	end;

	function Pos(ch: char; s: pChar; ns: size_t): size_t;
	begin
		result := size_t(Index(ch, s, ns));
		Assert((0 = not result) or (result < ns), 'pos returned ' + ToString(result) + ' from n = ' + ToString(ns));
	end;

	function Rpos(const substr, s: string; offset: sint = RPOS_START): sint;
	var
		i: sint;
	begin
		if offset = RPOS_START then offset := length(s) - length(substr) + 1;
		for i := offset downto 1 do
			if CompareByte(s[i], pChar(substr)^, length(substr)) = 0 then
				exit(i);
		result := 0;
	end;

	function StrReplace(const s, find, replace: string): string;
	var
		curStart, curPos: sint;
	begin
		result := '';
		curStart := 1;
		repeat
			curPos := Pos(find, s, curStart);
			if curPos = 0 then
			begin
				result := result + Copy(s, curStart, length(s) - curStart + 1);
				break;
			end;
			result := result + Copy(s, curStart, curPos - curStart) + replace;
			curStart := curPos + length(find);
		until no;
	end;

	function StrReplace(const s: string; const find: array of string; replace: StrReplaceFunc; param: pointer): string;
	type
		StrItem = record
			index: uint;
			hash: Hash.Value;
		end;
		LenItem = record
			len: sint;
			multiplier: uint;
			ongoingHash: Hash.Value;
			bloom: uint;
			nfh, fhAlloc: size_t;
			static_fh: array[0 .. 7] of StrItem;
			dynamic_fh: ^StrItem;
		end;
	var
		nLens, lensAlloc, len, minLen: sint;
		prevHash, h: Hash.Value;
		ifd, il, ih, ist, start: sint;
		sb: StringBuilder;
		static_lens: array[0 .. 7] of LenItem;
		dynamic_lens: ^LenItem;
		lp, flp: ^LenItem;
		sp: ^StrItem;
	begin
		nLens := 0; Assert(@static_lens = @static_lens);
		minLen := length(find[0]);
		lensAlloc := length(static_lens);
		dynamic_lens := nil;
		for ifd := 0 to High(find) do
		begin
			len := length(find[ifd]);
			if len < minLen then minLen := len;
			flp := nil;
			lp := static_lens;
			for il := 0 to nLens - 1 do
			begin
				if lp^.len = len then
				begin
					flp := lp;
					break;
				end;
				lp += 1; if il = High(static_lens) then lp := dynamic_lens;
			end;
			if not Assigned(flp) then
			begin
				inc(nLens);
				if nLens > lensAlloc then
				begin
					lensAlloc := 2 * nLens;
					ReallocMem(dynamic_lens, (lensAlloc - length(static_lens)) * sizeof(LenItem));
				end;
				if nLens <= length(static_lens) then flp := @static_lens[nLens - 1] else flp := @dynamic_lens[nLens - length(static_lens) - 1];
				flp^.len := len;
				flp^.multiplier := Pow(Hash.RollingStringBase, uint(len - 1));
				flp^.nfh := 0;
				flp^.bloom := 0;
				flp^.fhAlloc := length(flp^.static_fh);
				flp^.dynamic_fh := nil;
			end;
			inc(flp^.nfh);
			if flp^.nfh > flp^.fhAlloc then
			begin
				flp^.fhAlloc := 2 * flp^.nfh;
				ReallocMem(flp^.dynamic_fh, (flp^.fhAlloc - length(flp^.static_fh)) * sizeof(StrItem));
			end;
			if flp^.nfh <= length(flp^.static_fh) then sp := @flp^.static_fh[flp^.nfh - 1] else sp := @flp^.dynamic_fh[flp^.nfh - length(flp^.static_fh) - 1];
			sp^.index := ifd;
			h := Hash.OfStringRolling(0, pChar(find[ifd]), len);
			sp^.hash := h;
			flp^.bloom := flp^.bloom or uint(1) shl (h mod bitsizeof(flp^.bloom));
		end;
		lp := static_lens;
		h := Hash.OfStringRolling(0, pChar(s), minLen - 1);
		for il := 0 to nLens - 1 do
		begin
			lp^.ongoingHash := h;
			lp += 1; if il = High(static_lens) then lp := dynamic_lens;
		end;

		start := 1;
		sb.Init;
		for ist := minLen to length(s) do
		begin
			lp := static_lens;
			for il := 0 to nLens - 1 do
			begin
				unchecked
				if ist > lp^.len then prevHash := lp^.multiplier * Hash.Value(s[ist - lp^.len]) else prevHash := 0;
				lp^.ongoingHash := (lp^.ongoingHash - prevHash) * Hash.RollingStringBase + Hash.Value(s[ist]);
				end_unchecked

				if (start <> ist + 1) and (lp^.bloom and (1 shl (lp^.ongoingHash mod bitsizeof(lp^.bloom))) <> 0) then
				begin
					sp := lp^.static_fh;
					for ih := 0 to lp^.nfh - 1 do
					begin
						if (lp^.ongoingHash = sp^.hash)
							and (CompareByte(s[ist - lp^.len + 1], pChar(find[sp^.index])^, lp^.len) = 0)
						then
						begin
							sb.Append(@s[start], ist - lp^.len + 1 - start);
							sb.Append(replace(sp^.index, param));
							start := ist + 1;
						end;
						sp += 1; if ih = High(lp^.static_fh) then sp := lp^.dynamic_fh;
					end;
				end;
				lp += 1; if il = High(static_lens) then lp := dynamic_lens;
			end;
		end;
		sb.Append(pChar(s) + start - 1, length(s) - start + 1);

		lp := static_lens;
		for il := 0 to nLens - 1 do
		begin
			FreeMem(lp^.dynamic_fh);
			lp += 1; if il = High(static_lens) then lp := dynamic_lens;
		end;
		FreeMem(dynamic_lens);
		result := sb.DestructiveToString;
	end;

	function StrDup(const s: string; times: sint): string;
	var
		p: pChar;
		i: sint;
	begin
		if times <= 0 then exit('');
		if times = 1 then exit(s);
		SetLength(result, length(s) * times);
		case length(s) * sizeof(char) of
			sizeof(byte): FillByte(pointer(result)^, times, pByte(s)^);
			sizeof(word): FillWord(pointer(result)^, times, pWord(s)^);
			sizeof(dword): FillDWord(pointer(result)^, times, pDWord(s)^);
			sizeof(qword): FillQWord(pointer(result)^, times, pQWord(s)^);
			else
				begin
					p := pointer(result);
					for i := 1 to times do
					begin
						memcpy(pChar(s), p, length(s) * sizeof(char));
						p += length(s);
					end;
				end;
		end;
	end;

	function StrStuff(const s: string; at, remove: sint; const what: string): string;
	begin
		Assert((at > 0) and (at <= length(s) + 1), 'out of range');
		if at + remove - 1 > length(s) then remove := length(s) - at + 1;
		SetLength(result, length(s) - remove + length(what));
		memcpy(pointer(s), pointer(result), (at - 1) * sizeof(char));
		memcpy(pointer(what), pChar(result) + (at - 1), length(what) * sizeof(char));
		memcpy(pChar(s) + (at - 1) + remove, pChar(result) + (at - 1) + length(what), (length(s) - (at - 1) - remove) * sizeof(char));
	end;

	function FindStr(const s: string; const arr: array of string; default: sint = -1): sint;
	begin
		result := Index(s, pString(arr), length(arr));
		if result < 0 then
		begin
		{$ifdef Debug} if default >= 0 then Log('Строка "' + s + '" не найдена, заменена "' + arr[default] + '"', logWarning); {$endif}
			result := default;
		end;
	end;

	function FindStr(out id: sint; const s: string; const arr: array of string; default: sint = -1): boolean;
	begin
		id := FindStr(s, arr);
		result := id >= 0;
		if not result then id := default;
	end;

	function ScanToken(out tok: string; const s: string; var pos: sint; const syms: charset_t = DefaultTokenSyms): boolean;
	var
		start, &end: sint;
	begin
		start := pos;
		while (start <= length(s)) and not (s[start] in syms) do
			inc(start);
		&end := start;
		while (&end <= length(s)) and (s[&end] in syms) do
			inc(&end);
		result := &end > start;
		tok := Copy(s, start, &end - start);
		pos := &end;
	end;

	function ScanToken(const s: string; var pos: sint; const syms: charset_t = DefaultTokenSyms): string;
	begin
		ScanToken(result, s, pos, syms);
	end;

	function ScanFloatToken(const s: string; var pos: sint; const def: hp_float = 0.0): hp_float;
	var
		ts: string;
	begin
		ts := ScanToken(s, pos);
		result := StrToFloat(ts, def);
	end;

	function StrEq(a, b: pChar; len: size_t): boolean;
	begin
		result := CompareByte(a^, b^, len * sizeof(char)) = 0;
	end;

	function StrEq(s: pChar; len: size_t; const ref: string): boolean;
	begin
		result := (len = size_t(length(ref))) and ((s = pChar(ref)) or (CompareByte(s^, pChar(ref)^, len * sizeof(char)) = 0));
	end;

	function StrEq(a: pChar; na: size_t; b: pChar; nb: size_t): boolean;
	begin
		result := (na = nb) and ((a = b) or (CompareByte(a^, b^, na * sizeof(char)) = 0));
	end;

	function IsSuffix(const suffix, s: string): boolean;
	begin
		result := (length(s) >= length(suffix)) and
		          (CompareByte(pointer(suffix)^, (pChar(s) + (length(s) - length(suffix)))^, length(suffix) * sizeof(char)) = 0);
	end;

	function CutSuffix(const suffix, s: string; rest: pString): boolean;
	begin
		result := IsSuffix(suffix, s);
		if result then
			rest^ := Copy(s, 1, length(s) - length(suffix))
		else
			rest^ := s;
	end;

	function CutSuffix(const suffix, s: string): string;
	begin
		CutSuffix(suffix, s, @result);
	end;

	function CutAffixes(const prefix, s, suffix: string; rest: pString): boolean;
	begin
		result := Prefixed(prefix, s) and IsSuffix(suffix, s);
		if result then
			rest^ := Copy(s, 1 + length(prefix), length(s) - length(prefix) - length(suffix))
		else
			rest^ := s;
	end;

	function CutAffixes(const prefix, s, suffix: string): string;
	begin
		CutAffixes(prefix, s, suffix, @result);
	end;

	function CutFragmentWithAffixes(var s: string; const prefix, suffix: string; out fragment: string): boolean;
	var
		L, R: sint;
	begin
		result := no;
		fragment := '';
		L := Pos(prefix, s);
		if L = 0 then exit;
		R := Pos(suffix, s, L + length(prefix));
		if R = 0 then exit;
		fragment := Copy(s, L + length(prefix), R - L - length(prefix));
		delete(s, L, R - L + length(suffix));
		result := yes;
	end;

	function CommonAffixLength(const a, b: string; what: Affix; limit: size_T): size_t;
	var
		minLen: size_t;
	begin
		result := 0;
		minLen := min(length(a), length(b), limit);

		if what = Prefix then
			while (result < minLen) and (a[1 + result] = b[1 + result]) do
				inc(result)
		else
			while (result < minLen) and (a[length(a) - result] = b[length(b) - result]) do
				inc(result);
	end;

	function GetAffix(const s: string; len: size_t; what: Affix): string;
	var
		start: size_t;
	begin
		if len > size_t(length(s)) then len := length(s);
		if what = Prefix then start := 0 else start := length(s) - len;
		result := Copy(s, 1 + start, len);
	end;

	procedure AppendAffixTo(var s: string; const affix: string; what: Affix);
	begin
		if what = Prefix then s := affix + s else s := s + affix;
	end;

	function CommonAffix(s: pString; count: sint; what: Affix): string;
	var
		isym, istr: sint;
		ref: char;
	begin
		case count of
			0: exit('');
			1: exit(s[0]);
		end;

		if what = Prefix then
			for isym := 1 to length(s[0]) do
			begin
				ref := s[0, isym];
				for istr := 1 to count - 1 do
					if (length(s[istr]) < isym) or (s[istr, isym] <> ref) then
						exit(Copy(s[0], 1, isym - 1));
			end
		else
			for isym := 1 to length(s[0]) do
			begin
	   		ref := s[0, length(s[0]) - isym + 1];
				for istr := 1 to count - 1 do
					if (length(s[istr]) < isym) or (s[istr, length(s[istr]) - isym + 1] <> ref) then
						exit(Copy(s[0], length(s[0]) - isym + 2, isym - 1));
			end;
		result := s[0];
	end;

	function StringOptionals.Split(const src: string; const opts: array of string; handler: HandlerProc; param: pointer): StringView;
	var
		prevId, prevDataStart: sint;

		procedure CompletePrev(pend: sint);
		begin
			if prevId >= 0 then
				handler(prevId, StringView.Make(pChar(src) + (prevDataStart - 1), pend - prevDataStart), param)
			else
				result := StringView.Make(pChar(src), pend - 1);
		end;

	var
		optChar: char;
		spp, p, i: sint;
	begin
		optChar := opts[0][1];
		p := 1;
		prevId := -1;
		while p <= length(src) do
		begin
			spp := Pos(optChar, src, p); if spp = 0 then break;
			p := spp;
			for i := 0 to High(opts) do
			begin
				Assert(opts[i][1] = optChar);

				if Prefixed(opts[i], src, p) then
				begin
					CompletePrev(spp);
					prevId := i;
					p += length(opts[i]);
					prevDataStart := p;
					break;
				end;
			end;

			if p = spp then
			begin
			{$ifdef Debug} Log('StringOptionals.Split({0}): разделитель встретился как обычный символ', src, logWarning); {$endif}
				p += 1;
			end;
		end;
		CompletePrev(length(src) + 1);
	end;

	function SeparatedList.Join(const strs: array of string; const sepx: string): string;
	begin
		result := Join(length(strs), @GetStringViewFromStringArray, pString(strs), sepx);
	end;

	function SeparatedList.Join(const strs: array of StringView; const sepx: string): string;
	begin
		result := Join(length(strs), @GetStringViewFromStringViewArray, pStringView(strs), sepx);
	end;

type
	StringToTransientParam = record
		getString: GetIndexedString;
		param: pointer;
		lastString: string;
	end;

	function StringToTransient(id: uint; param: pointer): StringView;
	var
		p: ^StringToTransientParam absolute param;
	begin
		p^.lastString := p^.getString(id, p^.param);
		result := @p^.lastString;
	end;

	function SeparatedList.Join(n: sint; getString: GetIndexedString; param: pointer; const sepx: string): string;
	var
		p: StringToTransientParam;
	begin
		p.getString := getString;
		p.param := param;
		result := Join(n, @StringToTransient, @p, sepx);
	end;

type
	SeparatedListOptions = record
		&set: set of SeparatedList.OptionID;
		values: array[SeparatedList.OptionID] of StringView;
	end;

	procedure HandleSeparatedListOption(id: uint; const value: StringView; param: pointer);
	begin
		SeparatedListOptions(param^).values[id] := value;
		SeparatedListOptions(param^).&set += [id];
	end;

	function SeparatedList.Join(n: sint; getString: GetIndexedStringView; param: pointer; const sepx: string): string;
	var
		i, pass, handled, last: sint;
		pos: size_t;
		s, sep: StringView;
		opts: SeparatedListOptions;
		csep: ^StringView;
	begin
		opts.&set := [];
		sep := StringOptionals.Split(sepx, Options, @HandleSeparatedListOption, @opts);
		last := -1;

		// на первом проходе вычисляется длина, в конце распределяется строка, на втором строки memcpy'ятся в неё
		for pass := 0 to 1 do
		begin
			handled := 0;
			pos := 0;
			if _PREFIX in opts.&set then pos += opts.values[_PREFIX].n;
			i := 0;
			while (n < 0) or (i < n) do
			begin
				s := getString(i, param);
				if s.n > 0 then
				begin
					if handled > 0 then
					begin
						if (i <> last) or not (_LASTSEP in opts.&set) then csep := @sep else csep := @opts.values[_LASTSEP];
						if pass > 0 then memcpy(csep^.p, pChar((@result)^) + pos, csep^.n * sizeof(char));
						pos += csep^.n;
					end;
					if pass > 0 then memcpy(s.p, pChar(result) + pos, s.n * sizeof(char));
					pos += s.n;
					if pass = 0 then last := i;
					inc(handled);
				end else
					if n < 0 then break;
				inc(i);
			end;
			if _SUFFIX in opts.&set then pos += opts.values[_SUFFIX].n;

			if pass = 0 then
			begin
				if handled <> 0 then
				begin
					if (handled > 1) and (_LASTSEP in opts.&set) then pos := pos - sep.n + opts.values[_LASTSEP].n;
					SetLength(result, pos);
					if _PREFIX in opts.&set then
						memcpy(opts.values[_PREFIX].p, pChar(result), opts.values[_PREFIX].n * sizeof(char));
					if _SUFFIX in opts.&set then
					begin
						memcpy(opts.values[_SUFFIX].p, pChar(result) + pos - opts.values[_SUFFIX].n, opts.values[_SUFFIX].n * sizeof(char));
						pos -= opts.values[_SUFFIX].n;
					end;
				end else
					if _EMPTY in opts.&set then
						result := opts.values[_EMPTY].ToString
					else
						result := '';
			end;
		end;
	end;

	function GetStringViewFromStringArray(id: uint; param: pointer): StringView; begin result := @pString(param)[id]; end;
	function GetStringViewFromStringViewArray(id: uint; param: pointer): StringView; begin result := pStringView(param)[id]; end;
	function GetStringFromSintArray(id: uint; param: pointer): string;    begin result := ToString(pSint(param)[id]); end;
	function GetStringFromUintArray(id: uint; param: pointer): string;    begin result := ToString(pUint(param)[id]); end;
	function GetStringFromArrayOfConst(id: uint; param: pointer): string; begin result := ToString(System.PVarRec(param)[id]); end;
	function PadLeft(const s: string; minLen: sint): string;              begin result := PadLeft(s, ' ', minLen); end;
	function PadRight(const s: string; minLen: sint): string;             begin result := s + StrDup(' ', minLen - length(s)); end;

	function PadLeft(const s: string; pad: char; minLen: sint): string;
	begin
		if length(s) >= minLen then
			result := s
		else
			result := StrDup(pad, minLen - length(s)) + s;
	end;

	function PadLeft(const s: string; const pad: string; minLen: sint): string;
	begin
		if length(s) >= minLen then result := s else
		begin
			result := StrDup(pad, (minLen - length(s)) div length(pad)) + Copy(pad, 1, (minLen - length(s)) mod length(pad)) + s;
			Assert(length(result) = minLen);
		end;
	end;

	function PreferQuoting(id: uint; param: pointer): boolean;
	begin
		result := ord(pString(param)^[id]) <= 32;
	end;

	function MaybeQuote(const s: string): string;
	begin
		if (s = '') or Range.Closed(1, length(s)).Any(@PreferQuoting, @s) then result := '"' + s + '"' else result := s;
	end;

	function Split(const s: string; const seps: charset_t): Strings;
	var
		v: StringViews;
		i: sint;
	begin
		v := SplitIntoViews(s, seps);
		SetLength(result, length(v));
		for i := 0 to High(v) do
			result[i] := v[i].ToString;
	end;

	function SplitIntoViews(const s: string; const seps: charset_t): StringViews;
	var
		i, n: sint;
		was, now: boolean;
	begin
		now := no;
		n := 0;
		for i := 1 to length(s) do
		begin
			was := now;
			now := not (s[i] in seps);
			if now and not was then inc(n);
		end;

		SetLength(result, n);
		n := 0;
		now := no;
		for i := 1 to length(s) do
		begin
			was := now;
			now := not (s[i] in seps);
			if now and not was then
			begin
				inc(n);
				result[n - 1].p := @s[i];
			end;
			if was and not now then result[n - 1].n := @s[i] - result[n - 1].p;
		end;
		if now and was then result[n - 1].n := pChar(s) + length(s) - result[n - 1].p;
		Assert(n = length(result));
	end;

	function SafeIndex(const s: array of string; index: sint): string; begin if (index >= 0) and (index < length(s)) then result := s[index] else result := ''; end;
	function SafeIndex(const s: array of StringView; index: sint): StringView; begin if (index >= 0) and (index <= length(s)) then result := s[index] else result := StringView.Empty; end;

	function WrapNonempty(const s: string; const prefix, postfix: string): string;
	begin
		if s = '' then result := '' else result := prefix + s + postfix;
	end;

	function Format(s: pChar; ns: size_t; nArgs: uint; getArg: GetIndexedStringView; param: pointer): string;
	const
		PadSample = 'pad:';
		MaxPad = 200;
	var
		format_result: string absolute result;
		rpos, cpsEnd: size_t;
		cpsCalculated: uint;

		function Codepoints: uint;
		begin
			if rpos > cpsEnd then
			begin
				cpsCalculated += UTF8.Codepoints(pChar(format_result) + cpsend, rpos - cpsend);
				cpsEnd := rpos;
			end;
			result := cpsCalculated;
		end;

		function Grow(n: size_t): pChar;
		begin
			if rpos + n > size_t(length(format_result)) then SetLength(format_result, 2*(rpos + n));
			result := pChar(format_result) + rpos;
			rpos += n;
		end;

		procedure Append(s: pChar; n: size_t);
		begin
			memcpy(s, Grow(n), n * sizeof(char));
		end;

		function ParsePad(s: pChar; n: size_t): boolean;
		var
			cc, pad, fill: uint;
		begin
			result := Prefixed(PadSample, s, n);
			if result then
			begin
				s += length(PadSample); n -= length(PadSample);
				cc := TryParse(s, n, pad);
				result := (cc > 0) and (cc = n);
				if result then
				begin
					fill := min(pad - min(Codepoints, pad), MaxPad);
					fillchar(Grow(fill)^, fill, ' ');
				end;
			end;
		end;

	{$ifdef Debug} var lastUsedArg: sint; {$endif}

		function ParseArg(s: pChar; n: size_t): boolean;
		var
			cc: size_t;
			id: uint;
			arg: StringView;
		begin
			cc := TryParse(s, n, id);
			result := (cc > 0) and (cc = n) and (id < nArgs);
			if result then
			begin
				arg := getArg(id, param);
				Append(arg.p, arg.n);
			{$ifdef Debug} lastUsedArg := max(lastUsedArg, id); {$endif}
			end;
		end;

	var
		p: size_t;

	begin
	trace_call('Format');
		SetLength(result, ns + 8*nArgs);
		rpos := 0; cpsEnd := 0; cpsCalculated := 0;
	{$ifdef Debug} lastUsedArg := -1; {$endif}

		repeat
			p := Pos('{', s, ns);
			if 0 = not p then break;
			Append(s, p);
			s += p + size_t(length('{')); ns := ns - p - size_t(length('{'));

			if (ns > 0) and (s[0] = '{') then
			begin
				Append(pChar('{'), length('{')); s += length('{'); ns -= length('{');
			end else
			begin
				p := Pos('}', s, ns);
				if (0 <> not p) and (ParseArg(s, p) or ParsePad(s, p)) then
					// ok
				else
				begin
					s -= size_t(length('{')); ns += size_t(length('{'));
					if 0 = not p then break;
					p += size_t(length('{'));
					Append(s, p + size_t(length('}')));
				end;
				s += p + size_t(length('}')); ns := ns - p - size_t(length('}'));
			end;
		until no;

		SetLength(result, rpos + ns);
		memcpy(s, pChar(result) + rpos, ns * sizeof(char));
	{$ifdef Debug} if lastUsedArg <> sint(nArgs) - 1 then Log('Format({0}): не все строки запрошены.', USystem.ToString(s, ns), logWarning); {$endif}
	leave_call
	end;

	function Format(s: pChar; ns: size_t; nArgs: uint; getArg: GetIndexedString; param: pointer): string;
	var
		p: StringToTransientParam;
	begin
		p.getString := getArg;
		p.param := param;
		result := Format(s, ns, nArgs, @StringToTransient, @p);
	end;

	function Format(const s: string; nArgs: uint; getArg: GetIndexedStringView; param: pointer): string; begin result := Format(pChar(s), length(s), nArgs, getArg, param); end;
	function Format(const s: string; const args: array of string): string; begin result := Format(pChar(s), length(s), length(args), @GetStringViewFromStringArray, pString(args)); end;
	function Format(const s: string; const args: array of const): string;  begin result := Format(pChar(s), length(s), length(args), @GetStringFromArrayOfConst, @args); end;

{$define func:=
	function Format(const s: string; const _ARGS_: string): string;   begin result := Format(s, [_ARGS_]); end;
	function Error(const s: string; const _ARGS_: string): Exception; begin result := Error(Format(s, [_ARGS_])); end;}
	{$include variadic.inc}
	function Error(const s: string; const args: array of const): Exception; begin result := Error(Format(s, args)); end;

	function Range.Open(L, R: uint): Range;   begin result.L := L; result.R := R; end;
	function Range.Closed(L, R: uint): Range; begin result.L := L; result.R := R + 1; end;
	function Range.Open(n: uint): Range;      begin result.L := 0; result.R := n; end;

{$define traverse_range :=
	var
		i: uint;
	begin
	{$ifdef pre} pre; {$endif}
		i := L;
		while i < R do
		begin
			body;
			inc(i);
		end;
	{$ifdef post} post; {$endif}
	end; {$undef body} {$undef pre} {$undef post}}

{$define one :=
	procedure Range.Each(proc: callback_type range__maybe_accept_params);
		{$define body := proc(i range__maybe_pass_params)}
		traverse_range} range__for_each_callback

{$define one :=
	function Range.Any(pred: predicate_type range__maybe_accept_params): boolean;
		{$define body := if pred(i range__maybe_pass_params) then exit(yes)}
		{$define post := result := no}
		traverse_range

	function Range.All(pred: predicate_type range__maybe_accept_params): boolean;
		{$define body := if not pred(i range__maybe_pass_params) then exit(no)}
		{$define post := result := yes}
		traverse_range

	function Range.Find(pred: predicate_type range__maybe_accept_params): sint;
		{$define body := if pred(i range__maybe_pass_params) then exit(i)}
		{$define post := result := -1}
		traverse_range

	function Range.Matching(pred: predicate_type range__maybe_accept_params): uint;
		{$define pre := result := 0}
		{$define body := if pred(i range__maybe_pass_params) then inc(result)}
		traverse_range} range__for_each_predicate
{$undef traverse_range}

	function Range2D.Open(const L, R: UintVec2): Range2D; begin result.L := L; result.R := R; end;
	function Range2D.Open(const R: UintVec2): Range2D;    begin result.L := UintVec2.Zero; result.R := R; end;

{$define traverse_range :=
	var
		x, y {$ifdef extra_index}, i {$endif}: uint;
	begin
	{$ifdef pre} pre; {$endif}
	{$ifdef extra_index} i := 0; {$endif}
		y := L.y;
		while y < R.y do
		begin
			x := L.x;
			while x < R.x do
			begin
				body;
			{$ifdef extra_index} inc(i); {$endif}
				inc(x);
			end;
			inc(y);
		end;
	{$ifdef post} post; {$endif}
	end; {$undef body} {$undef pre} {$undef post}}

{$define one :=
	procedure Range2D.Each(proc: callback_type range__maybe_accept_params);
		{$define body := proc(x, y {$ifdef extra_index}, i {$endif} range__maybe_pass_params)}
		traverse_range} range2d__for_each_callback

{$define one :=
	function Range2D.Any(pred: predicate_type range__maybe_accept_params): boolean;
		{$define body := if pred(x, y {$ifdef extra_index}, i {$endif} range__maybe_pass_params) then exit(yes)}
		{$define post := result := no}
		traverse_range

	function Range2D.All(pred: predicate_type range__maybe_accept_params): boolean;
		{$define body := if not pred(x, y {$ifdef extra_index}, i {$endif} range__maybe_pass_params) then exit(no)}
		{$define post := result := yes}
		traverse_range

	function Range2D.Matching(pred: predicate_type range__maybe_accept_params): uint;
		{$define pre := result := 0}
		{$define body := if pred(x, y {$ifdef extra_index}, i {$endif} range__maybe_pass_params) then inc(result)}
		traverse_range} range2d__for_each_predicate
{$undef traverse_range}

{$ifdef selftest}
	function PrintableStringTestCase(const input: string): string;
	begin
		result := PrintableString(input);
	end;

	function SeparatedListCase(const input: array of string): string;
	const
		Specials: array[SeparatedList.OptionID] of string = ('lastsep', 'prefix', 'suffix', 'empty');
	var
		items: array of string;
		sepx: string;
		i, id: sint;
	begin
		SetLength(items, length(input));
		sepx := '';
		i := 0;

		while i < length(input) do
			if (i + 1 < length(input)) and (FindStr(id, input[i], Specials) or (input[i] = 'sep')) then
			begin
				if id >= 0 then sepx += SeparatedList.Options[id] + input[i + 1] else sepx := input[i + 1] + sepx;
				i += 2;
			end else
			begin
				SetLength(items, length(items) + 1); items[High(items)] := input[i];
				inc(i);
			end;
		result := SeparatedList.Join(items, sepx);
	end;

	function SignedUnsigned(const s, u: string): string; begin result := 'signed ' + s + '; unsigned ' + u; end;

	function TryParseCase(const input: string): string;
		function BuildResult(const v: string; n: size_t): string;
		begin
			result := IfThen(n > 0, v) + IfThen(n > 0, ', ') + 'n=' + ToString(n);
		end;
	var
		n: size_t;
		svalue: int64; sresult: string;
		uvalue: qword; uresult: string;
	begin
		n := TryParse(pChar(input), length(input), svalue);
		sresult := BuildResult(ToString(svalue), n);

		n := TryParse(pChar(input), length(input), uvalue);
		uresult := BuildResult(ToString(uvalue), n);
		if sresult = uresult then result := sresult else result := SignedUnsigned(sresult, uresult);
	end;

	function FormatCase(const input: array of string): string;
	begin
		result := Format(input[0], length(input) - 1, @GetStringViewFromStringArray, pString(input) + 1);
	end;

	function FloatFormatCase(const input: array of string): string;
	var
		t: float;
		i: sint;
		valCode: word;
		fmt: FloatFormatDesc;
	begin
		fmt := FloatFormat.MostPrecise;
		t := 0; valCode := 0;
		i := 0;
		while i < length(input) do
			if (i + 1 < length(input)) and (input[i] = 'signif') then
			begin
				fmt := fmt.Significant(StrToInt(input[i + 1]));
				i += 2;
			end else
			begin
				System.Val(input[0], t, valCode);
				inc(i);
			end;

		if valCode = 0 then result := fmt.Apply(t) else result := Format('(ValCode = {0})', [valCode]);
	end;

	function VectorFormatCase(const input: array of string): string;
	var
		v: Vec3;
		i, c, id: sint;
		f: FloatFormatDesc;
		t: StringTokenizer;
	begin
		f := FloatFormat;
		v := Vec3.Zero;
		i := 0;
      while i < length(input) do
      	if (i + 1 < length(input)) and FindStr(id, input[i], ['signif', 'vecfmt']) then
      	begin
      		case id of
      			0: f := f.Significant(StrToInt(input[i + 1]));
      			else f := f.Pattern(input[i + 1]);
      		end;
      		i += 2;
      	end else
      	begin
      		t := input[i];
				try
					t.Expect('(');
					for c := 0 to High(v.data) do
					begin
						v.data[c] := t.ScanFloatToken;
						if c < High(v.data) then t.Expect(',');
					end;
					t.Expect(')');
				finally
					t.Done;
				end;				
				inc(i);
      	end;
		result := f.Apply(v);
	end;

	procedure Test;
	begin
		TestSuite.Start
		.Feature('PrintableString', @PrintableStringTestCase)
		.&Case('Простой текст',                'Простой текст')
		.&Case(#0#1#2#255#254#253,             '#0#1#2/FF/FE/FD/')
		.&Case(#154 + 'Тек' + #155#156 + 'ст', '/9A/Тек/9B/9C/ст')

		.Feature('SeparatedList', @SeparatedListCase)
		.&Case(['', '', '', 'sep', '; ', 'lastsep', ' and ', 'empty', '-_-'], '-_-')
		.&Case(['', 'ABC', 'DEF', '', 'G', 'sep', '; ', 'prefix', '(', 'suffix', ')', 'lastsep', ' and '], '(ABC; DEF and G)')

		.Feature('TryParse', @TryParseCase).&Case('0', '0, n=1').&Case('-365xy', SignedUnsigned('-365, n=4', 'n=0'))
		.&Case('--', 'n=0').&Case('+-2', 'n=0')
		.&Case('-9223372036854775808', SignedUnsigned('-9223372036854775808, n=20', 'n=0'))
		.&Case('9223372036854775807', '9223372036854775807, n=19')
		.&Case('9223372036854775808', SignedUnsigned('n=0', '9223372036854775808, n=19'))
		.&Case('18446744073709551615', SignedUnsigned('n=0', '18446744073709551615, n=20'))
		.&Case('-9223372036854775809', 'n=0')
		.&Case('18446744073709551616', 'n=0')

		.Feature('Format', @FormatCase)
		.&Case('', '').&Case('{}}{', '{}}{').&Case('{0}{1}', '{0}{1}').&Case(
			['[{0}{pad:10}{1}{pad:20}{2}{pad:25}{3}]{pad:38}', '第一', 'second', 'loooooooong', 'last'],
			'[第一       second    loooooooonglast]  ')

		.Feature('FloatFormat', @FloatFormatCase)
		.&Case('0', '0').&Case(['1.2345', 'signif', '4'], '1.235').&Case(['102.3', 'signif', '1'], '102').&Case(['102.34', 'signif', '4'], '102.3')
		.&Case('-0', '-0').&Case(['-1.2345', 'signif', '4'], '-1.235').&Case(['-102.3', 'signif', '1'], '-102').&Case(['-102.34', 'signif', '4'], '-102.3')
		.&Case('Inf', '+Inf').&Case('NaN', 'Nan').&Case('-Inf', '-Inf').&Case('12,3', '(ValCode = 3)')

		.Feature('VectorFormat', @VectorFormatCase)
		.&Case(['(1.2, 3.4, 5.6)', 'signif', '2', 'vecfmt', '\[/;\ /\]'], '[1.2; 3.4; 5.6]')
		.&Case(['(1.234, 5.678, 9.123)', 'signif', '3', 'vecfmt', 'X\=x; Y=y; Z\\\=z'], 'X=1.23; Y=5.68; Z\=9.12')
		.&Case(['(1.2, 3.4, 5.6)', 'signif', '1', 'vecfmt', '\/\x=x \y=y \z=z \w=w\/'], '/x=1 y=3 z=6 w=w/')
		.&Case(['(1.1, 2.2, 3.3)', 'signif', '2', 'vecfmt', 'X=x; /; y; /; z'], 'X=x; 1.1; y; 2.2; y; 3.3; z')
		.Done;
	end;
{$endif}

initialization
	{$ifdef selftest} &Unit('Utils').Test(@Test); {$endif}
end.
