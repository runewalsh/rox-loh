{$include opts.inc}
{$include all_numbers.inc}
unit Errors;

interface

uses
	USystem;

	function ExhaustiveCase(value: sint; const where: string): Exception;
	function UnsupportedCase(const what, where: string): Exception;
{$define numberf :=
	function RangeCheck(const value, max: typ; const where: string): typ;
	function RangeCheckOpen(const value, max: typ; const where: string): typ;
	function RangeCheck(const value, min, max: typ; const where: string): typ;
	function RangeCheckMin(const value, min: typ; const where: string): typ;} all_numbers
{$define intf :=
	function CheckPow2(const value: typ; const where: string): typ;} all_uints
	function Unimplemented(const what: string): Exception;
	function UnknownIdentifier(const id, where: string): Exception;
	function UnknownErrorCodeMsg(code: sint): string;
	function InlineUnknownErrorCodeMsg(code: sint): string;

implementation

	function ToString(const x: float): string; overload;
	begin
		str(x:0:2, result);
	end;

	function ExhaustiveCase(value: sint; const where: string): Exception;
	begin
		result := Error(
			{$ifdef Debug} where + ': значение ' + ToString(value) + ' не предусмотрено.'
			{$else}        'Внутренняя ошибка (' + where + ' = ' + ToString(value) + ').' {$endif});
	end;

	function UnsupportedCase(const what, where: string): Exception;
	begin
		result := Error(where + ': ' + what + ' не поддерживается.');
	end;

type
	scoped_enum_ RangeSide = (Closed, Open, Infinite); _end
{$define numberf :=
	function RangeCheckError(const value: typ; const where: string; const min, max: typ; right: RangeSide): Exception;
		function Range: string;
		begin
			result := '[' + ToString(min) + '; ';
			if right = RangeSide.Infinite then result += '+∞' else result += ToString(max);
			if right in [RangeSide.Open, RangeSide.Infinite] then result += ')' else result += ']';
		end;
	begin
		result := Error(where + ' = ' + ToString(value) + ' — вне допустимого диапазона ' + Range + '.');
	end;

	function RangeCheck(const value, max: typ; const where: string): typ;
	begin
		result := value;
		if result > max then raise RangeCheckError(value, where, 0, max, RangeSide.Closed);
	end;

	function RangeCheckOpen(const value, max: typ; const where: string): typ;
	begin
		result := value;
		if result >= max then raise RangeCheckError(value, where, 0, max, RangeSide.Open);
	end;

	function RangeCheck(const value, min, max: typ; const where: string): typ;
	begin
		result := value;
		if (result < min) or (result > max) then raise RangeCheckError(value, where, min, max, RangeSide.Closed);
	end;

	function RangeCheckMin(const value, min: typ; const where: string): typ;
	begin
		result := value;
		if result < min then raise RangeCheckError(value, where, min, 0, RangeSide.Infinite);
	end;} all_numbers

{$define intf :=
	function CheckPow2(const value: typ; const where: string): typ;
	begin
		result := value;
		if result and (result - 1) <> 0 then
			raise Error('Ожидается ' + where + '-степень двойки, получено ' + ToString(result) + '.');
	end;} all_uints

	function Unimplemented(const what: string): Exception;
	begin
		result := Error(what + ' не реализована.');
	end;

	function UnknownIdentifier(const id, where: string): Exception;
	begin
		result := Error(where + ': неизвестный идентификатор ' + id + '.');
	end;

	function UnknownErrorCodeMsg(code: sint): string;
	begin
		result := 'Неизвестный код (' + ToString(code) + ').';
	end;

	function InlineUnknownErrorCodeMsg(code: sint): string;
	begin
		result := 'с неизвестным кодом (' + ToString(code) + ')';
	end;

end.
