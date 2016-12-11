unit XML;

{$include opts.inc}

interface

uses
	USystem, Streams, Human;

type
	tXMLParam = record
		name, value: string;
	end;

	pXML = ^tXML;
	tXML = object(&Object)
	private
		procedure ReadParams(const s: string);
		function ParseFrom(const s: string; sstart: sint): sint; // returns length of parsed part
	public
		name: string;
		data: string;
		params: array of tXMLParam;
		childs: array of pXML;
		constructor Init(stream: pStream);
		destructor Done; virtual;
		function Param(const pname: string): string;
		function Node(const nname: string): pXML;
	end;

implementation

	function TrimCopy(const s: string; index, count: sint): string;
	begin
		while Symbol.IsWhitespace(s[index]) do
		begin
			if count <= 1 then break;
			inc(index);
			dec(count);
		end;
		while Symbol.IsWhitespace(s[index + count - 1]) do
		begin
			dec(count);
			if count = 0 then break;
		end;
		result := copy(s, index, count);
		if length(result) >= 2 then
		begin
			if result[1] = '"' then
				delete(result, 1, 1);
			if result[length(result)] = '"' then
				delete(result, length(result), 1);
		end;
	end;

	procedure tXML.ReadParams(const s: string);
	type
		tState = (state_Start, state_Pname, state_Value);
	var
		i, start: sint;
		state: tState;
		valueNow, textNow: boolean;
	begin
		state := state_Start;
		start := 1;
		valueNow := no;
		textNow := no;
		for i := 1 to length(s) do
			case state of
				state_Start :
					if not Symbol.IsWhitespace(s[i]) then
					begin
						SetLength(params, length(params) + 1);
						state := state_Pname;
						start := i;
					end;
				state_Pname :
					if s[i] = '=' then
					begin
						params[High(params)].Name := TrimCopy(s, start, i - start);
						state := state_Value;
						start := i + 1;
					end;
				state_Value :
					begin
						if s[i] = '"' then
							textNow := not textNow;
						if (not Symbol.IsWhitespace(s[i])) and not textNow then
							valueNow := yes
						else
							if valueNow then
							begin
								params[High(params)].Value := TrimCopy(s, start, i - start);
								state := state_Start;
								valueNow := no;
							end;
					end;
			end;
		if state <> state_Start then
			params[High(params)].Value := TrimCopy(s, start, length(s) - start + 1);
	end;

	function tXML.Param(const pname: string): string;
	var
		i: sint;
	begin
		for i := 0 to High(params) do
			if params[i].Name = pname then
				exit(params[i].value);
		result := '';
	end;

	function tXML.ParseFrom(const s: string; sstart: sint): sint;
	type
		tState = (state_StartTag, state_Name, state_Params, state_Data, state_End);
	var
		i, j, start: sint;
		state: tState;
		textNow: boolean;
	begin
		textNow := no;
		state := state_StartTag;
		i := sstart - 1;
		start := sstart;
		data := '';

		while i < length(s) do
		begin
			inc(i);
			case state of
				state_StartTag :
					if s[i] = '<' then
					begin
						state := state_Name;
						start := i + 1;
					end;
				state_Name :
					begin
						case s[i] of
							'>': state := state_Data;
							'/': state := state_End;
							'?', '!' :
								begin
									state := state_StartTag;
								end
						else
							if Symbol.IsWhitespace(s[i]) then
								state := state_Params
							else
								continue;
						end;
						self.name := TrimCopy(s, start, i - start);
						start := i + 1;
					end;
				state_Params :
					begin
						if s[i] = '"' then textNow := not textNow;
						if not textNow then
						begin
							case s[i] of
								'>': state := state_Data;
								'/': state := state_End;
							else
								continue;
							end;
							ReadParams(TrimCopy(s, start, i - start));
							start := i + 1;
						end;
					end;
				state_Data :
					begin
						case s[i] of
							'"': textNow := not textNow;
							'<' :
								if not textNow then
								begin
									data := data + TrimCopy(s, start, i - start);
									for j := i to length(s) do
									begin
										if s[j] = '>' then
										begin
											if TrimCopy(s, i + 1, j - i - 1) <> '/' + name then
											begin
												SetLength(childs, length(childs) + 1);
												childs[High(childs)] := new(pXML, Init(nil))^.NewRef;
												i := i + childs[High(childs)]^.ParseFrom(s, i - 1);
												start := i + 1;
											end else
												state := state_End;
											break;
										end;
									end;
								end
						end;
					end;
				state_End :
					if s[i] = '>' then
					begin
						result := i - sstart;
						break;
					end;
			end;
		end;
	end;

	constructor tXML.Init(stream: pStream);
	begin
		inherited Init;
		if Assigned(stream) then ParseFrom(ReadWholeAsString(stream), 1);
	end;

	destructor tXML.Done;
	begin
		ReleaseArray(childs);
		inherited Done;
	end;

	function tXML.Node(const nname: string): pXML;
	var
		i: sint;
	begin
		for i := 0 to High(childs) do
			if childs[i]^.name = nname then
			begin
				result := childs[i];
				exit;
			end;
		result := nil;
	end;

end.