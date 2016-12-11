unit Tokenizer;

{$include opts.inc}
{$ifdef Debug}
	{-$define ExtDebug}
{$endif}

interface

uses
	USystem, Streams, Utils, UMath, UClasses, Human
{$ifdef Debug}, ULog{$endif};

const
	BufferSize = 16 * 1024;

type
	tTokenType =
	(
		token_Unknown, token_Finale,
		token_BlockStart, token_BlockEnd,
		token_Identifier,
		token_Integer, token_Float,
		token_String,
		token_Implication, token_Colon, token_Comma
	);

	tToken = object
		typ: tTokenType;
		sb: StringBuilder;
		procedure Initialize;
		procedure Finalize;
		function Human: string;
	end;

	pTokenizer = ^tTokenizer;
	tTokenizer = object(&Object)
	public type
		tTokenFunc = function(const token: string; param: pointer): string;
	private
		_stream: pStream;
		_hasNextToken, _finished: boolean;
		_nextToken: tToken;
		_buf: packed array[0 .. BufferSize - 1] of char;
		_bufPos, _bufSize: size_t;
		_curSym: char;
		function _NextSym: char;
		procedure _SkipComment;
		procedure _SkipRowComment;
		procedure _ScanNumber(var token: tToken);
		procedure _ScanString(var token: tToken);
		procedure _ScanIdentifier(var token: tToken);
		function _ScanToken(out token: tToken): boolean;
	public
		constructor Init(newStream: pStream);
		destructor Done; virtual;
		function NextTokenType: tTokenType;
		function Read(what: tTokenType): boolean;
		function ReadBlockStart: boolean;
		function ReadBlockEnd: boolean;
		function ReadIdentifier: string;
		function ReadInteger: sint;
		function ReadFloat: hp_float;
		function ReadString: string;
		function ReadStream(tokenFunc: tTokenFunc = nil; param: pointer = nil): string;
		function ReadBoolean: boolean;
		procedure SkipToken(count: sint = 1);
		procedure SkipBlock;
		procedure UnexpectedToken;
		function StreamPath: string;

		function ReadVec2: Vec2;
		function ReadVec3: Vec3;
		function ReadVec4: Vec4;
	end;

const
	TokenTypeIds: array[tTokenType] of string = ('???', 'finale', 'block start', 'block end', 'identifier', 'integer', 'float', 'string', '=>', ':', ',');

implementation

{$ifdef Profile}
uses
	Profile;
{$endif}

	procedure tToken.Initialize;
	begin
		typ := token_Unknown;
		sb.Init;
	end;

	procedure tToken.Finalize;
	begin
		sb.Done;
	end;

	function tToken.Human: string;
	begin
		result := TokenTypeIds[typ];
		if sb.Len > 0 then result += ', "' + sb.ToString + '"';
	end;

	function tTokenizer.StreamPath: string;
	begin
		result := _stream^.path;
	end;

	constructor tTokenizer.Init(newStream: pStream);
	begin
		inherited Init;
		_stream := MakeRef(newStream);
		_hasNextToken := no;
		_finished := no;
		SkipUTF8BOM(_stream);
		_bufPos := 0;
		_bufSize := 0;
		_NextSym;
	end;

	destructor tTokenizer.Done;
	begin
		if _hasNextToken then _nextToken.Finalize;
		Release(_stream);
		inherited Done;
	end;

	function tTokenizer._NextSym: char;
	begin
		if (_bufPos = _bufSize) and not _finished then
		begin
			_bufPos := 0;
			_bufSize := _stream^.TryRead(@_buf, sizeof(_buf));
			_finished := _bufSize = 0;
		end;
		if _bufPos < _bufSize then
		begin
			result := _buf[_bufPos];
			_bufPos += 1;
		end else
			result := #0;
		_curSym := result;
	end;

	procedure tTokenizer._SkipComment;
	begin
		repeat
			case _curSym of
				'(':
					begin
						_NextSym;
						if _curSym = '*' then
						begin
							_NextSym;
							_SkipComment;
						end;
					end;
				'*':
					begin
						_NextSym;
						if _curSym = ')' then
						begin
							_NextSym;
							break;
						end;
					end;
				#0: break;
				else _NextSym;
			end;
		until no;
	end;

	procedure tTokenizer._SkipRowComment;
	begin
		repeat
			if _curSym in [EOL, #0] then break;
			_NextSym;
		until no;
	end;

	procedure tTokenizer._ScanNumber(var token: tToken);
	var
		dot, e: boolean;
	begin
		dot := no;
		e := no;
		token.sb.Append(_curSym);
		case _curSym of
			'0' .. '9', '+', '-': token.typ := token_Integer;
			else token.typ := token_Float;
		end;
		repeat
			_NextSym;
			case _curSym of
				'0' .. '9': token.sb.Append(_curSym);
				'.':
					if dot then exit else
					begin
						dot := yes;
						token.typ := token_Float;
						token.sb.Append(_curSym);
					end;
				'e', 'E':
					if e then exit else
					begin
						e := yes;
						_NextSym;
						if _curSym in ['+', '-', '0'..'9'] then
						begin
							token.sb.Append('e', _curSym);
							token.typ := token_Float;
						end else
							exit;
					end;
				else
					exit;
			end;
		until no;
	end;

	procedure tTokenizer._ScanString(var token: tToken);
	var
		bracket: char;
	begin
		bracket := _curSym;
		token.typ := token_String;
		repeat
			_NextSym;
			if _curSym = bracket then
			begin
				_NextSym;
				if _curSym = bracket then
					token.sb.Append(_curSym)
				else
					exit;
			end else
				token.sb.Append(_curSym);
		until no;
	end;

	procedure tTokenizer._ScanIdentifier(var token: tToken);
	begin
		token.typ := token_Identifier;
		repeat
			if _curSym in IdentifierSyms then
			begin
				token.sb.Append(_curSym);
				_NextSym;
			end else
				exit;
		until no;
	end;

	function tTokenizer._ScanToken(out token: tToken): boolean;
	label _RE_;
	begin
		if _hasNextToken then
		begin
			token := _nextToken;
			token.sb.InstanceMovedFrom(_nextToken.sb);
			_hasNextToken := no;
			exit(token.typ <> token_Unknown);
		end;

	_RE_:
		while Symbol.IsWhitespace(_curSym) do _NextSym;
		token.Initialize;
		case _curSym of
			#0: token.typ := token_Finale;
			'{':
				begin
					token.typ := token_BlockStart;
					_NextSym;
				end;
			'}':
				begin
					token.typ := token_BlockEnd;
					_NextSym;
				end;
			'(':
				begin
					_NextSym;
					if _curSym = '*' then
					begin
						_NextSym;
						_SkipComment;
						token.Finalize;
						goto _RE_;
					end;
				end;
			'/':
				begin
					_NextSym;
					if _curSym = '/' then
					begin
						_NextSym;
						_SkipRowComment;
						token.Finalize;
						goto _RE_;
					end;
				end;
			'0' .. '9', '+', '-', '.': _ScanNumber(token);
			'"', '''': _ScanString(token);
			'=':
				begin
					_NextSym;
					if _curSym = '>' then
					begin
						token.typ := token_Implication;
						_NextSym;
					end;
				end;
			':':
				begin
					token.typ := token_Colon;
					_NextSym;
				end;
			',':
				begin
					token.typ := token_Comma;
					_NextSym;
				end;
			else
				if _curSym in IdentifierSyms then
					_ScanIdentifier(token)
		end;
		result := token.typ <> token_Unknown;
	{$ifdef ExtDebug} Log(TokenTypeIds[token.typ] + ': "' + token.sb.ToString + '"', logDebug); {$endif}
	end;

	function tTokenizer.Read(what: tTokenType): boolean;
	var
		token: tToken;
	begin
		result := _ScanToken(token) and (token.typ = what);
		token.Finalize;
	{$ifdef Debug} if not result then Log('Ожидается "' + TokenTypeIds[what] + '"', logError); {$endif}
	end;

	function tTokenizer.ReadBlockStart: boolean; begin result := Read(token_BlockStart); end;
	function tTokenizer.ReadBlockEnd:   boolean; begin result := Read(token_BlockEnd);   end;

	function tTokenizer.NextTokenType: tTokenType;
	var
		tok: tToken;
	begin
		if _hasNextToken then result := _nextToken.typ else
		begin
			_ScanToken(tok);
			result := tok.typ;
			_nextToken := tok;
			_nextToken.sb.InstanceMovedFrom(tok.sb);
			_hasNextToken := yes;
		end;
	end;

	function tTokenizer.ReadIdentifier: string;
	var
		tok: tToken;
	begin
		_ScanToken(tok);
		if tok.typ = token_Identifier then
			result := tok.sb.ToString
		else
		begin
		{$ifdef Debug} Log('Ожидается идентификатор', logError); {$endif}
			result := '';
		end;
		tok.Finalize;
	end;

	function tTokenizer.ReadInteger: sint;
	var
		tok: tToken;
	begin
		_ScanToken(tok);
		if tok.typ = token_Integer then
			result := StrToInt(tok.sb.ToString)
		else
		begin
		{$ifdef Debug} Log('Ожидается целое число', logError); {$endif}
			result := 0;
		end;
		tok.Finalize;
	end;

	function tTokenizer.ReadFloat: hp_float;
	var
		tok: tToken;
	begin
		_ScanToken(tok);
		case tok.typ of
			token_Integer: result := StrToInt(tok.sb.ToString);
			token_Float: result := StrToFloat(tok.sb.ToString);
			else
			begin
			{$ifdef Debug} Log('Ожидается вещественное число', logError); {$endif}
				result := 0.0;
			end;
		end;
		tok.Finalize;
	end;

	function tTokenizer.ReadString: string;
	var
		tok: tToken;
	begin
		_ScanToken(tok);
		if tok.typ = token_String then
			result := tok.sb.ToString
		else
		begin
		{$ifdef Debug} Log('Ожидается строка', logError); {$endif}
			result := '';
		end;
		tok.Finalize;
	end;

	function tTokenizer.ReadStream(tokenFunc: tTokenFunc = nil; param: pointer = nil): string;
	var
		token, base: string;
	begin
		if NextTokenType = token_Identifier then token := ReadIdentifier else token := '';
		if token = 'global' then base := '' else
			if (token <> 'local') and Assigned(tokenFunc) then base := tokenFunc(token, param) else
				base := StreamPath;
		result := Streams.StreamPath.Resolve(ReadString, base);
	end;

	function tTokenizer.ReadBoolean: boolean;
	var
		tok: tToken;
		s: string;
	begin
		_ScanToken(tok);
		case tok.typ of
			token_Integer: result := StrToInt(tok.sb.ToString) > 0;
			token_Identifier:
				begin
					s := tok.sb.ToString;
					result := (s = 'yes') or (s = 'on');
				end;
			else
				begin
					{$ifdef Debug} Log('Ожидается булевское значение', logError); {$endif}
					result := no;
				end;
		end;
		tok.Finalize;
	end;

	procedure tTokenizer.SkipToken(count: sint = 1);
	var
		i: sint;
		tok: tToken;
	begin
		for i := 1 to count do
		begin
			_ScanToken(tok);
			tok.Finalize;
		end;
	end;

	procedure tTokenizer.SkipBlock;
	var
		tok: tToken;
	begin
		repeat
			_ScanToken(tok);
			case tok.typ of
				token_BlockStart: SkipBlock;
				token_BlockEnd, token_Finale: break;
			end;
			tok.Finalize;
		until no;
		tok.Finalize;
	end;

	procedure tTokenizer.UnexpectedToken;
	var
		msg: string;
	begin
		msg := Streams.StreamPath.Log(StreamPath) + ': неожиданный токен';
		if _hasNextToken then msg += ' ' + _nextToken.Human;
	{$ifdef Debug} Log(msg, logError); {$endif}
	end;

{$define impl:=
	var
		i: sint;
	begin
		for i := 0 to High(result.data) do
			result.data[i] := ReadFloat;
	end;}
	function tTokenizer.ReadVec2: Vec2; impl
	function tTokenizer.ReadVec3: Vec3; impl
	function tTokenizer.ReadVec4: Vec4; impl
{$undef impl}

end.