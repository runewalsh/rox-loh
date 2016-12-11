unit Debug;

{$include opts.inc}
{$ifndef Debug} {$warning use in debug mode} {$endif}

interface

uses
	USystem;

type
	Sentinel = object
		function Setup(data: pointer; size: size_t): Sentinel; static;
		procedure Verify(data: pointer; size: size_t; const what: string);

	var
		garbage: byte;
		procedure Corrupted(address: pointer; const what: string);
	end;

	function AddressLineInfo(addr: CodePointer): string;
	function GetBackTrace(skip: uint = 0): string;
	function DumpMemory(p: pointer; n: size_t): string;

implementation

var
	nextSentinelGarbage: byte = 0;

	function Sentinel.Setup(data: pointer; size: size_t): Sentinel;
	var
		g: byte;
	begin
		g := nextSentinelGarbage;
		result.garbage := g;
		unchecked inc(nextSentinelGarbage); end_unchecked
		fillchar(data^, size, g);
	end;

	procedure Sentinel.Verify(data: pointer; size: size_t; const what: string);
	var
		g: byte;
		ed: pointer;
	begin
		g := garbage;
		ed := data + size;
		while data < ed do
		begin
			if pByte(data)^ <> g then Corrupted(data, what);
			data += sizeof(byte);
		end;
	end;

	procedure Sentinel.Corrupted(address: pointer; const what: string);
	begin
		Fatal('Память повреждена (' + HexStr(address) + ', ' + what + ').');
	end;

	function AddressLineInfo(addr: CodePointer): string;
		function ExtractFilename(const item: string): string;
		var
			i, last: sint;
		begin
			last := length(item) + 1;
			for i := length(item) downto 1 do
			begin
				if item[i] in ['/', '\'] then exit(Copy(item, i + 1, last - i - 1));
				if (item[i] = '.') and (last > length(item)) then last := i;
			end;
			result := item;
		end;

	const
		LinePrefix = ' line ';
		FnPrefix   = ' of ';
	var
		p_fn, p_line: sint;
	begin
		result := BacktraceStrFunc(addr);
		p_line := Pos(LinePrefix, result);
		p_fn   := Pos(FnPrefix, result);
		if (p_line > 0) and (p_fn > p_line) then
			result := Copy(result, 1, p_line - 1) + ' ' +
				ExtractFilename(Copy(result, p_fn + length(FnPrefix), length(result) - (p_fn + length(FnPrefix)) + 1)) +
				':' + Copy(result, p_line + length(LinePrefix), p_fn - (p_line + length(LinePrefix)));
	end;

	function GetBackTrace(skip: uint = 0): string;
	var
		bp, prevbp: pointer;
		addr: pointer;
	begin
		result := '';
		bp := get_frame; prevbp := bp - 1;
		while Assigned(bp) and (bp > prevbp) and (bp <= StackBottom + StackLength) do
		begin
			if skip > 0 then dec(skip) else
			begin
				addr := get_caller_addr(bp);
				if result <> '' then result += EOL;
				result += AddressLineInfo(addr);
			end;
			prevbp := bp;
			bp := get_caller_frame(bp);
		end;

		if result <> '' then result += EOL;
		result += ' ($' + HexStr(bp) + ')';
	end;

	function DumpMemory(p: pointer; n: size_t): string;
	begin
		result := '';
		while (n > 0) and (pPtrUint(@p)^ mod sizeof(pointer) <> 0) do
		begin
			result += HexStr(pByte(p)^, 2);
			p += 1; n -= 1;
		end;

		while n >= sizeof(pointer) do
		begin
			if result <> '' then result += ' ';
			result += HexStr(pPointer(p)^);
			p += sizeof(pointer); n -= sizeof(pointer);
		end;

		if n > 0 then
		begin
			if result <> '' then result += ' ';
			repeat
				result += HexStr(pByte(p)^, 2);
				p += 1; n -= 1;
			until n = 0;
		end;
	end;

end.
