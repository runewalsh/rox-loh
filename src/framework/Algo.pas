unit Algo;

{$include opts.inc}
{$include all_numbers.inc}

interface

uses
	USystem, Errors, UMath, Random, Utils, Streams;

type
	Hash = object
	type
		Value = type uint32; pValue = ^Value;
	var
		function OfUint(const x: uint32): Value;                                  static;
		function OfUint(const x: uint64): Value;                                  static;
		function OfPointer(const x: pointer): Value;                              static; cinline
		function OfString(s: pchar; len: size_t): Value;                          static; cinline
		function OfString(const s: string): Value;                                static; cinline
		function OfString(const s: StringView): Value;                            static; cinline
		function OfStringSeeded(s: pchar; len: size_t; const seed: Value): Value; static;
	const
		RollingStringBase = 101;
	var
		function OfStringRolling(const hash: Value; s: pchar; len: size_t): Value;    static;
		function OfFloat(const x: float): Value;                                      static;
		function OfFloatEps(const x, eps: float): Value;                              static;
		function OfMem(p: pointer; len: size_t): Value;                               static;
		function Combine(const seed, new: Value): Value;                              static;

		function Crc32(p: pointer; len: size_t): uint32;                              static;
		function Crc32(const crc: uint32; p: pointer; len: size_t): uint32;           static;

		function Crc64(p: pointer; len: size_t): uint64;                              static;
		function Crc64(const crc: uint64; p: pointer; len: size_t): uint64;           static;

		function Murmur32(p: pointer; len: size_t): uint32;                           static;
		function Murmur32Seeded(key: pointer; len: size_t; const seed: uint32): uint32; static;
	end;

{$define intf :=
	function Rol(const x: typ; by: uint): typ; cinline
	function Ror(const x: typ; by: uint): typ; cinline}
	all_uint_sizes

type
	UpdateRanges = object
	type
		Callback = procedure(aPtr: pointer; offset, size: size_t; param: pointer);
	var
		procedure Update(aPtr, bPtr: pointer; size: size_t; maxFreeCells: size_t; proc: Callback; param: pointer); static;
	end;

	function Base32(data: pointer; size: size_t): string;
	function design(value: sint): uint;
	function ensign(value: uint): sint;
	function bitpack(const vn: array of uint): uint;
{$define intf := function bits(const value: typ; start, n: uint): typ; cinline} all_uints

type
	UiBinaryFormatChooser = object
		minFixed: UiBinaryFormat;
		n: uint;
		bytesFromV: size_t;
	{$ifdef Debug} guard: pointer; what: string; {$endif}

		procedure Init(const nWhat: string);
		procedure Done;
	{$define intf := procedure Note(const x: typ);} all_uints
		function Choose: UiBinaryFormat;
		function DestructiveChoose: UiBinaryFormat;
		function Choose(const x: array of uint; const nWhat: string): UiBinaryFormat; static;
		function Choose(const x: array of ulong; const nWhat: string): UiBinaryFormat; static;
		function ChooseByCount(x: uint; const nWhat: string): UiBinaryFormat; static;
	type
		GetValueFunc = function(id: uint; param: pointer): uint;
		GetUlongValueFunc = function(id: uint; param: pointer): ulong;
	var
		function Choose(count: sint; getValue: GetValueFunc; param: pointer; const nWhat: string): UiBinaryFormat; static;
		function Choose(count: sint; getValue: GetUlongValueFunc; param: pointer; const nWhat: string): UiBinaryFormat; static;
	end;

	function PackCircle(const centerA: Vec2; const radiusA: float; const centerB: Vec2; const radiusB: float; const radius: float; out center, altCenter: Vec2): sint;
	procedure PackCircles(n: sint; radiuses: pFloat; centers: pVec2);

type
	EmptyRects = object
	type
		List = array of Rect;
	var
		function Get(const whole: Rect; const rects: array of Rect): List; static;
	end;

	SplitStream = object
	type
		StartProc = function(param: pointer): pointer;
		BlockProc = procedure(block: pointer; size: size_t; sparam: pointer);
		EndProc = procedure(sparam: pointer);
	var
		procedure Split(s: pStream; const signature: string; start: StartProc; block: BlockProc; ends: EndProc; param: pointer); static;
	end;

	RegionAllocator = object
	type
		scoped_enum_ AllocateFlag = (Precise); _end
		AllocateFlags = set of AllocateFlag;
	var
		procedure Init(preallocateHint: size_t = 0);
		procedure Done;
		function Allocate(size: size_t; flags: AllocateFlags = []): pointer;

	private type
		BaseDataType = GenericAlignedType; pBaseDataType = ^BaseDataType;

		pBlock = ^Block;
		Block = record
			used, allocated: size_t;
			prev: pBlock;
			data: array[0 .. 0] of BaseDataType;
		end;
	var
		last: pBlock;

	public const
		Precise = AllocateFlag.Precise;
	end;

	// Approx неоптимален, Precise неюзабелен, т. к. требует N*M памяти. Лень до ума доводить, пусть повисит так.
	Diff = object
	type
		elem = char; pElem = ^elem;

		Source = record
			a: pElem; na: size_t;
			b: pElem; nb: size_t;
		end;

		Options = record
			minBlock: size_t;
		end;

		pBased = ^Based;
		Based = object
		type
			OpDesc = record
				takeFromA: boolean;
				start, count: size_t;
			end;

			OpList = array of OpDesc;
			Method = (Precise, Approx);
		var
			ops: OpList;
			totalTakenFromA: size_t;

			function Build(a: pElem; na: size_t; b: pElem; nb: size_t; minBlock: size_t; aMethod: Method): Based; static;
			function ToString: string;
		private
			procedure Init;
			procedure BuildPrecise(const src: Source; const opts: Options);
			procedure BuildApprox(a: pElem; na: size_t; b: pElem; nb: size_t; minBlock: size_t);
			procedure SetOp(index: size_t; takeFromA: boolean; start, count: size_t);
			procedure AddOp(takeFromA: boolean; start, count: size_t; var nOps: size_t);

		public type
			SerializationState = record
				pass: uint;
				fmtChooser: ^UiBinaryFormatChooser;
				fmt: UiBinaryFormat;
			end;
			StartPassProc = procedure(param: pointer);
			SaveBProc = procedure(s: pStream; const op: OpDesc; const state: SerializationState; param: pointer);

			DeserializationState = record
				opIndex: uint;
				fmt: UiBinaryFormat;
			end;
			LoadCountProc = procedure(nOps: uint; param: pointer);
			LoadAProc = procedure(start, count: size_t; const state: DeserializationState; param: pointer);
			LoadBProc = procedure(s: pStream; count: size_t; const state: DeserializationState; param: pointer);
		const
			COUNT_EXTRA_BITS = 1;
			COUNTX_FROM_A_BIT = uint(1 shl 0);
			NOpsLimit = 10000;

			procedure Serialize(s: pStream; startPass: StartPassProc; saveB: SaveBProc; param: pointer);
			procedure Deserialize(s: pStream; onCount: LoadCountProc; loadA: LoadAProc; loadB: LoadBProc; param: pointer); static;
			procedure Serialize(s: pStream);
			function Deserialize(s: pStream): Based; static;
		end;

	private
		function RangeEquals(a, b: pElem; n: size_t): boolean; static;
		function EqualLen(a: pElem; na: size_t; b: pElem; nb: size_t): size_t; static;
	type
		PreciseBased = object
		const
			N_COUNT_MASK = 1 shl (bitsizeof(size_t) - 1) - 1;
			N_FROM_A_BIT = 1 shl (bitsizeof(size_t) - 1);
		type
			pChainOp = ^ChainOp;
			ChainOp = object
				start, n: size_t;
				next: pChainOp;
				takenFromA: size_t;
				function Create(start, n: size_t; next: pChainOp; takenFromA: size_t; var pc: PreciseBased): pChainOp; static;
			end;

			MemKey = object
				sa, sb: size_t;
				function Make(sa, sb: size_t): MemKey; static;
			end;
		var
			src: Source;
			opts: Options;
			mem: array of pChainOp;
			chainOpMem: RegionAllocator;
			procedure Init(const src: Source; const opts: Options);
			procedure Done;
			function Build(sa, sb: size_t): pChainOp;
			procedure Memoize(sa, sb: size_t; cd: pChainOp);
			function Memoized(sa, sb: size_t): pChainOp;
		end;
	end;

	{Wave = object
	type
		PointID = type uint;
		Distance = type uint;
		GetEdgesCount = function(vertexId: uint; param: pointer): uint;
		GetEdge = function(vertexId, edgeId: uint; param: pointer; out distance: Distance): uint;

		function FindWay(nVerts, from, &to: uint; getEdgesCount: GetEdgesCount; getEdge: GetEdge; param: pointer;
			maxIntermediateVertices: uint; intermediateVertices: pUint; out distance: Distance): uint; static;
	end;}

	FloodFill = object
	type
	{$define classname := PointSet} {$define key_type := UintVec2} {$define dont_replace} {$define on_new_ret} {$include hash.h.inc}
		TestPoint = function(const point: UintVec2; param: pointer): boolean;
		Result = object
			filled: PointSet;
			min, max: UintVec2;
			procedure Done;
		end;

		procedure FourWay(out r: Result; const start, size: UintVec2; test: TestPoint; param: pointer); static;
	end;

implementation

uses
	UClasses {$ifdef Debug}, ULog, Human{$endif};

(*type
{$define classname := PointIDQueue} {$define item_type := Wave.PointID} {$include queue.h.inc}
{$define classname := PointIDSet} {$define key_type := Wave.PointID} {$include hash.h.inc}
{$define classname := PointIDQueue} {$include queue.pp.inc}
{$define classname := PointIDSet} {$define hash_func := Hash.OfUint} {$include hash.pp.inc}*)

{$define classname := FloodFill.PointSet} {$define inline_hash := Hash.Combine(Hash.OfUint(_1.x), Hash.OfUint(_1.y))} {$include hash.pp.inc}

unchecked
	function Hash.OfUint(const x: uint32): Value;
	var
		h: Value absolute result;
	begin                         // Другой вариант:
		h := x;                    // h := x;
		h := h + not (h shl 15);   // h := (h + $7ed55d16) + (h shl 12);
		h := h xor (h shr 10);     // h := (h xor $c761c23c) xor (h shr 19);
		h := h + (h shl 3);        // h := (h + $165667b1) + (h shl 5);
		h := h xor (h shr 6);      // h := (h + $d3a2646c) xor (h shl 9);
		h := h + not (h shl 11);   // h := (h + $fd7046c5) + (h shl 3);
		h := h xor (h shr 16);     // h := (h xor $b55a4f09) xor (h shr 16);
	end;

	function Hash.OfUint(const x: uint64): Value;
	var
		t: uint64;
	begin
		t := x;
		t := not t + (t shl 18);
		t := t xor (t shr 31);
		t *= 21;
		t := t xor (t shr 11);
		t += t shl 6;
		t := t xor (t shr 22);
		result := t;
	end;

	function Hash.OfPointer(const x: pointer): Value;         begin result := OfUint(pPtrUint(@x)^); end;
	function Hash.OfString(s: pchar; len: size_t): Value;     begin result := OfStringSeeded(s, len, 0); end;
	function Hash.OfString(const s: string): Value;           begin result := OfString(pointer(s), length(s)); end;
	function Hash.OfString(const s: StringView): Value;  begin result := OfString(s.p, s.n); end;

	function Hash.OfStringSeeded(s: pchar; len: size_t; const seed: Value): Value;
	const
		MaxStepsPow2 = 5;
		StepBlock = 8;
	var
		i, step: size_t;
		sm: array[2 .. StepBlock] of size_t;
	begin
		result := seed xor len;
		step := len shr MaxStepsPow2 + 1;

		i := 0;
		sm[High(sm)] := StepBlock * step;
		if len > sm[High(sm)] then
		begin
			sm[Low(sm)] := Low(sm) * step;
			for i := Low(sm) + 1 to High(sm) - 1 do
				sm[i] := sm[i - 1] + step;

			while i + sm[StepBlock] < len do
			begin
				result := (((((((result * 101 + Value(s[i])) * 101 + Value(s[i + step])) * 101 + Value(s[i + sm[2]])) * 101 + Value(s[i + sm[3]]))
					* 101 + Value(s[i + sm[4]])) * 101 + Value(s[i + sm[5]])) * 101 + Value(s[i + sm[6]])) * 101 + Value(s[i + sm[7]]);
				i += sm[StepBlock];
			end;
		end;

		while i < len do
		begin
			result := result * 101 + Value(s[i]);
			// result := result xor ((result shl 5) + (result shr 2) + Value(s[i]));
			i += step;
		end;
	end;

	function Hash.OfStringRolling(const hash: Value; s: pchar; len: size_t): Value;
	var
		ed: pChar;
	begin
		result := hash;
		ed := s + len;
		while s < ed do
		begin
			result := result * RollingStringBase + Value(s^);
			inc(s);
		end;
	end;

{$if not (sizeof(float) in [sizeof(float32), sizeof(float64)])}
	{$define tmp_float_for_hash}
{$endif}
	function Hash.OfFloat(const x: float): Value;
{$ifdef tmp_float_for_hash} var t: float64; {$endif}
	begin
		if (x = 0.0) or FloatIs.NaN(x) then
			result := High(result)
		else
		begin
		{$ifdef tmp_float_for_hash} t := x; {$endif}
			result :=
				{$if     sizeof(float) = sizeof(uint32)} OfUint(pUint32(@x)^)
				{$elseif sizeof(float) = sizeof(uint64)} OfUint(pUint64(@x)^)
				{$elseif defined(tmp_float_for_hash)} OfUint(pUint64(@t)^)
				{$else} {$error Hash.OfFloat unimplemented}
				{$endif}
		end;
	end;

	// TODO: убрать, это вообще корректно?
	function Hash.OfFloatEps(const x, eps: float): Value;  begin result := OfFloat(eps * int(x / eps + 0.5)); end;
{$undef tmp_float_for_hash}

	function Hash.OfMem(p: pointer; len: size_t): Value;   begin result := Murmur32(p, len); end;
	function Hash.Combine(const seed, new: Value): Value;  begin result := seed xor (new + $9e3779b9 + (seed shl 6) + (seed shr 2)); end;
end_unchecked

{$define crc_impl :=
type
	table_ptr = ^table_type;
	table_type = array[byte] of bitness;

	function create_table: table_ptr;
	var
		i, j: sint;
		part: bitness;
	begin
		new(result);
		for i := 0 to High(result^) do
		begin
			part := i;
			for j := 0 to 7 do
			begin
				if part and $1 <> 0 then
					part := (part shr 1) xor bitness(poly)
				else
					part := part shr 1;
			end;

			result^[i] := part;
		end;
	end;

	{$define accessor := table_accessor} {$define instance_type := table_ptr}
	{$define create_instance := create_table} {$define destroy_instance := dispose(instance); instance := nil}
	{$include lazy_singleton.inc}

	function frontend(p: pointer; len: size_t): bitness;  begin result := frontend(0, p, len); end;

	function frontend(const crc: bitness; p: pointer; len: size_t): bitness;
	var
		table: table_ptr;
	begin
		table := table_accessor;
		result := crc;

		while len > 0 do
		begin
			result := table^[(result xor pByte(p)[0]) and $ff] xor (result shr 8);
			inc(p);
			dec(len);
		end;
	end;
	{$undef bitness} {$undef poly}
	{$undef table_type} {$undef table_ptr} {$undef table_accessor}
	{$undef create_table} {$undef frontend}}

	{$define bitness := uint32} {$define poly := $edb88320} // 0,1,2,4,5,7,8,10,11,12,16,22,23,26
	{$define table_type := Crc32TableData} {$define table_ptr := pCrc32TableData} {$define table_accessor := Crc32Table}
	{$define create_table := InitCrc32Table} {$define frontend := Hash.Crc32}
		crc_impl

	{$define bitness := uint64} {$define poly := $C96C5795D7870F42}
	{$define table_type := Crc64TableData} {$define table_ptr := pCrc64TableData} {$define table_accessor := Crc64Table}
	{$define create_table := InitCrc64Table} {$define frontend := Hash.Crc64}
		crc_impl
{$undef crc_impl}

	function Hash.Murmur32(p: pointer; len: size_t): uint32;  begin result := Murmur32Seeded(p, len, 0); end;

unchecked
	function Hash.Murmur32Seeded(key: pointer; len: size_t; const seed: uint32): uint32;
	const
		c1 = uint32($cc9e2d51);
		c2 = uint32($1b873593);
		r1 = 15;
		r2 = 13;
		m = 5;
		n = uint32($e6546b64);
	var
		h: uint32 absolute result;
		tail: pUint8;
		i, nChunks: size_t;
		k: uint32;
		rem: uint;
	begin
		nChunks := len div 4;
		h := seed;

		i := 0;
		while i < nChunks do
		begin
			h := Rol(uint32(h xor (Rol(uint32(pUint32(key)[i] * c1), r1) * c2)), r2) * m + n;
			inc(i);
		end;

		rem := len and 3;
		if rem >= 1 then
		begin
			tail := pointer(pUint32(key) + nChunks);
			k := 0;
			if rem >= 3 then k := k xor (tail[2] shl 16);
			if rem >= 2 then k := k xor (tail[1] shl 8);
			h := h xor (Rol(uint32((k xor tail[0]) * c1), r1) * c2);
		end;

		h := h xor len;
		h := (h xor (h shr 16)) * $85ebca6b;
		h := (h xor (h shr 13)) * $c2b2ae35;
		h := h xor (h shr 16);
	end;
end_unchecked

{$define define_funcs_from_x :=
	{$if sizeof(x) = sizeof(uint8)} {$define rol_func := RolByte} {$define ror_func := RorByte}
	{$elseif sizeof(x) = sizeof(uint16)} {$define rol_func := RolWord} {$define ror_func := RorWord}
	{$elseif sizeof(x) = sizeof(uint32)} {$define rol_func := RolDWord} {$define ror_func := RorDWord}
	{$elseif sizeof(x) = sizeof(uint64)} {$define rol_func := RolQWord} {$define ror_func := RorQWord}
	{$else} {$error Rol/Ror: unsupported size} {$endif}
	{$define end_funcs := {$undef rol_func} {$undef ror_func} {$undef end_funcs}}}

{$define intf :=
	function Rol(const x: typ;  by: uint): typ; begin result := define_funcs_from_x rol_func(x, by) end_funcs; end;
	function Ror(const x: typ;  by: uint): typ; begin result := define_funcs_from_x ror_func(x, by) end_funcs; end;}
	all_uint_sizes
{$undef define_funcs_from_x}

	procedure UpdateRanges.Update(aPtr, bPtr: pointer; size: size_t; maxFreeCells: size_t; proc: Callback; param: pointer);
	var
		a: pPtrUint absolute aPtr;
		b: pPtrUint absolute bPtr;
		i, n: sint;
		curOfs, curSize, nFrees: size_t;
	begin
		n := size div sizeof(PtrUint);
		curSize := 0;
		nFrees := 0;

		for i := 0 to n - 1 do
			if a[i] <> b[i] then
			begin
				if curSize <> 0 then
				begin
					if nFrees <> 0 then
					begin
						curSize += nFrees;
						nFrees := 0;
					end;
					inc(curSize, sizeof(PtrUint));
				end else
				begin
					curOfs := i * sizeof(PtrUint);;
					curSize := sizeof(PtrUint);
					nFrees := 0;
				end;
			end else
				if curSize <> 0 then
				begin
					inc(nFrees, sizeof(PtrUint));
					if nFrees > maxFreeCells then
					begin
						proc(aPtr, curOfs, curSize, param);
						curSize := 0;
					end;
				end;
		if (size mod sizeof(PtrUint) <> 0) and (CompareByte(a[n], b[n], size mod sizeof(PtrUint)) <> 0) then
			if curSize = 0 then
			begin
				curOfs := n * sizeof(PtrUint);
				curSize := size mod sizeof(PtrUint);
			end else
				curSize += nFrees + size mod sizeof(PtrUint);
		if curSize <> 0 then proc(aPtr, curOfs, curSize, param);
	end;

	function Base32(data: pointer; size: size_t): string;
	const
		Sample = 'abcdefghijklmnopqrstuvwxyz!+=_()';
	var
		rest, restBits: uint;
		n: sint;
	begin
		result := '';
		rest := 0;
		restBits := 0;
		n := 0;
		SetLength(result, (8 * size + 4) div 5);

		while size > 0 do
		begin
			dec(size);
			restBits += bitsizeof(byte);
			rest := (rest shl 8) or uint(pByte(data)^);
			data += 1;

			while restBits >= 5 do
			begin
				inc(n);
				result[n] := sample[1 + rest shr (restBits - 5)];
				restBits -= 5;
				rest := rest and (1 shl restBits - 1);
			end;
		end;

		if restBits > 0 then
		begin
			inc(n);
			result[n] := sample[1 + rest];
		end;
		SetLength(result, n);
	end;

	function design(value: sint): uint;
	begin
		if value >= 0 then
			result := 2 * uint(value)
		else
			result := 2 * uint(-(value + 1)) + 1; // 2 * -value - 1, устойчивое к Low(sint)
	end;

	function ensign(value: uint): sint;
	begin
		if value mod 2 = 0 then
			result := value div 2
		else
			result := -(value div 2) - 1;
	end;

	function bitpack(const vn: array of uint): uint;
	var
		i: sint;
		shift: uint;
	begin
		Assert(length(vn) mod 2 = 0);
		result := 0;
		shift := 0;
		i := 0;

		while i < length(vn) do
		begin
			result := result or vn[i] shl shift;
			shift += vn[i + 1];

			Assert(shift <= bitsizeof(result), Format('Биты не умещаются в uint{0}.', [bitsizeof(result)]));
			Assert(result shr (shift - vn[i + 1]) = vn[i], Format('Параметр #{0} не уместился в {1}.',
				[i div 2, {$ifdef Debug} lang_amount(vn[i + 1], '{N} выделенны{й/е/х} бит{/а/}') {$else} 'выделенные биты' {$endif}]));
			i += 2;
		end;
	end;

{$define intf :=
	function bits(const value: typ; start, n: uint): typ;
	begin
		result := (value shr start) and (1 shl n - 1);
	end;} all_uints

	procedure UiBinaryFormatChooser.Init(const nWhat: string);
	begin
		minFixed := se_ui8;
		n := 0;
		bytesFromV := 0;
	{$ifdef Debug}
		what := nWhat;
		guard := GetMem(1);
	{$else}
		Assert(@nWhat = @nWhat);
	{$endif}
	end;

	procedure UiBinaryFormatChooser.Done;
	begin
	{$ifdef Debug} FreeMem(guard); {$endif}
	end;

{$define intf :=
	procedure UiBinaryFormatChooser.Note(const x: typ);
	begin
		if (x > High(uint8)) and (minFixed <= se_ui8) then minFixed := se_ui16;
		if (x > High(uint16)) and (minFixed <= se_ui16) then minFixed := se_ui24;
		if (x >= 1 shl (3 * bitsizeof(uint8))) and (minFixed <= se_ui24) then minFixed := se_ui32;
	{$if sizeof(x) > sizeof(uint32)} if (x > High(uint32)) and (minFixed <= se_ui32) then minFixed := se_ui64; {$endif}
		inc(n);
		bytesFromV += VarInt.Bytes(x);
	end;} all_uints

	function UiBinaryFormatChooser.Choose: UiBinaryFormat;
	{$ifdef Debug} var msg: string; {$endif}
	begin
		Assert(UiBinaryFormatInfo[minFixed].bytes > 0);
		if bytesFromV < (3 * n * UiBinaryFormatInfo[minFixed].bytes) div 4 then
			result := se_ui_v8
		else
			result := minFixed;

	{$ifdef Debug}
		if n > 0 then
		begin
			msg := 'Для ' + what;
			if n > 1 then msg += lang_amount(n, ' по {N} значени{ю/ям/ям}');
			msg += ' выбран формат ' + UiBinaryFormatIds[result];
			Log(msg);
		end;
	{$endif}
	end;

	function UiBinaryFormatChooser.DestructiveChoose: UiBinaryFormat;
	begin
		result := Choose;
		Done;
	end;

	function GetUintFromArray(id: uint; param: pointer): uint;  begin result := pUint(param)[id]; end;
	function UiBinaryFormatChooser.Choose(const x: array of uint; const nWhat: string): UiBinaryFormat;
	begin
		result := Choose(length(x), @GetUintFromArray, pUint(x), nWhat);
	end;

	function GetUlongFromArray(id: uint; param: pointer): uint;  begin result := pUlong(param)[id]; end;
	function UiBinaryFormatChooser.Choose(const x: array of ulong; const nWhat: string): UiBinaryFormat;
	begin
		result := Choose(length(x), @GetUlongFromArray, pUlong(x), nWhat);
	end;

{$define impl :=
	var
		ch: UiBinaryFormatChooser;
		i: sint;
	begin
		ch.Init(nWhat);
		for i := 0 to count - 1 do
			ch.Note(getValue(i, param));
		result := ch.DestructiveChoose;
	end;}
	function UiBinaryFormatChooser.Choose(count: sint; getValue: GetValueFunc; param: pointer; const nWhat: string): UiBinaryFormat; impl
	function UiBinaryFormatChooser.Choose(count: sint; getValue: GetUlongValueFunc; param: pointer; const nWhat: string): UiBinaryFormat; impl
{$undef impl}

	function UiBinaryFormatChooser.ChooseByCount(x: uint; const nWhat: string): UiBinaryFormat;
	begin
		result := Choose([uint(x - min(x, 1))], nWhat);
	end;

	function PackCircle(const centerA: Vec2; const radiusA: float; const centerB: Vec2; const radiusB: float; const radius: float; out center, altCenter: Vec2): sint;
	var
		a, sqb, b, c, cosa, sina: float;
		toh, hbase: Vec2;
	begin
		a := radius + radiusA;
		if IsZero(a) then exit(0);

		sqb := SqrDistance(centerA, centerB);
		b := sqrt(sqb);
		if IsZero(b) then
		begin
			center := centerA + Vec2.PositiveX * (max(radiusA, radiusB) + radius);
			exit(1);
		end;

		c := radius + radiusB;
		cosa := (sqr(a) + sqb - sqr(c)) / (2.0 * a * b);
		if cosa > 1.0 then exit(0);
		sina := sqrt(1.0 - sqr(cosa));

		toh := (centerB - centerA).Normalized;
		hbase := centerA + toh * (a * cosa);
		toh := a * sina * Vec2.Make(-toh.y, toh.x);

		result := 1 + uint(not toh.IsZero);
		center := hbase + toh;
		altCenter := hbase - toh;
	end;

	procedure PackCircles(n: sint; radiuses: pFloat; centers: pVec2);

		function overlaps(const center: Vec2; const radius: float; n: sint): boolean;
		var
			i: sint;
		begin
			for i := 0 to n - 1 do
				if LessThan(SqrDistance(center, centers[i]), sqr(radius + radiuses[i])) then
					exit(yes);
			result := no;
		end;

	var
		angle: float;
		i, j, k, nalt: sint;
		nc: array[0 .. 1] of Vec2;
		hasBest: boolean;
		curSqrDistance, bestSqrDistance: float;
	begin
		if n = 0 then exit;
		centers[0] := Vec2.Zero;
		if n = 1 then exit;
		angle := GlobalRNG.GetFloat(TwoPi);
		centers[1] := centers[0] + (radiuses[0] + radiuses[1]) * Vec2.Make(cos(angle), sin(angle));

		for i := 2 to n - 1 do
		begin
			hasBest := no;
			bestSqrDistance := 0.0;

			for j := 0 to i - 2 do
				for k := j + 1 to i - 1 do
					for nalt := 0 to PackCircle(centers[j], radiuses[j], centers[k], radiuses[k], radiuses[i], nc[0], nc[1]) - 1 do
						if not overlaps(nc[nalt], radiuses[i], i) then
						begin
							curSqrDistance := nc[nalt].SqrLength;
							if (not hasBest) or (curSqrDistance < bestSqrDistance) then
							begin
								bestSqrDistance := curSqrDistance;
								centers[i] := nc[nalt];
								hasBest := yes;
							end;
						end;

			Assert(hasBest, 'не удалось разместить окружность ' + ToString(i) + '/' + ToString(n)); // TO-DO: это вообще может произойти? предусмотреть?
		end;
	end;

	function EmptyRects.Get(const whole: Rect; const rects: array of Rect): List;
	type
		Direction = (Left, Right, Up, Down);
	var
		nParts, nRes: sint;
		parts: array of Rect;

		function Grow(const srcRect: Rect; const seq: array of Direction): Rect;
		var
			dir: Direction;
			rect: UMath.Rect absolute result;
			border, nc, cc: float;
			i: sint;
		begin
			rect := srcRect;
			for dir in seq do
			begin
				repeat
					border := 0.0;
					for i := 0 to nParts - 1 do
						if
							(
								(
									((dir = Left) and LessThan(parts[i].A.x, rect.A.x) and GreaterThanEqual(parts[i].B.x, rect.A.x)) or
									((dir = Right) and GreaterThan(parts[i].B.x, rect.B.x) and LessThanEqual(parts[i].A.x, rect.B.x))
								) and RangeStrictlyIntersects(parts[i].A.y, parts[i].B.y, rect.A.y, rect.B.y)
							) or
							(
								(
									((dir = Up) and LessThan(parts[i].A.y, rect.A.y) and GreaterThanEqual(parts[i].B.y, rect.A.y)) or
									((dir = Down) and GreaterThan(parts[i].B.y, rect.B.y) and LessThanEqual(parts[i].A.y, rect.B.y))
								) and RangeStrictlyIntersects(parts[i].A.x, parts[i].B.x, rect.A.x, rect.B.x)
							)
						then
						begin
							case dir of
								Left:  cc := parts[i].A.x;
								Right: cc := parts[i].B.x;
								Up:    cc := parts[i].A.y;
								else   cc := parts[i].B.y;
							end;
							if border = 0 then nc := cc else
								if dir in [Left, Up] then nc := max(nc, cc) else nc := min(nc, cc);
							if dir in [Left, Right] then
								border += RangeIntersection(parts[i].A.y, parts[i].B.y, rect.A.y, rect.B.y)
							else
								border += RangeIntersection(parts[i].A.x, parts[i].B.x, rect.A.x, rect.B.x);
						end;

					if not Equals(border, rect.Size(uint(dir in [Left, Right]))) then break;
					case dir of
						Left:  begin Assert(nc < rect.A.x); rect.A.x := nc; end;
						Right: begin Assert(nc > rect.B.x); rect.B.x := nc; end;
						Up:    begin Assert(nc < rect.A.y); rect.A.y := nc; end;
						else   begin Assert(nc > rect.B.y); rect.B.y := nc; end;
					end;
				until no;
			end;
		end;

		procedure MaybeAdd(const rect: Rect);
		var
			i: sint;
		begin
			for i := 0 to nRes - 1 do
				if Equals(rect, result[i]) then
					exit;
			inc(nRes);
			if nRes > length(result) then SetLength(result, 2 * nRes);
			result[nRes - 1] := rect;
		end;

	var
		rect: UMath.Rect;
		subdiv: Rect.Subdivision;
		i, j, start: sint;

	begin
		nParts := 1;
		SetLength(parts, length(rects));
		parts[0] := whole;

		for rect in rects do
			for i := nParts - 1 downto 0 do
			begin
				subdiv := parts[i].Subdivide(rect);
				if subdiv.n > 0 then parts[i] := subdiv.rects[0];
				start := nParts;
				nParts += subdiv.n - 1; // может быть subdiv.n = 0
				if nParts > length(parts) then SetLength(parts, 2 * nParts);
				for j := 1 to subdiv.n - 1 do
					parts[start + j - 1] := subdiv.rects[j];
			end;

		SetLength(result, uint(nParts + 1) div 2 + uint(nParts + 3) div 4);
		nRes := 0;
		for i := 0 to nParts - 1 do
		begin
			MaybeAdd(Grow(parts[i], [Left, Right, Up, Down]));
			MaybeAdd(Grow(parts[i], [Up, Down, Left, Right]));
		end;
		SetLength(result, nRes);
	end;

	procedure SplitStream.Split(s: pStream; const signature: string; start: StartProc; block: BlockProc; ends: EndProc; param: pointer);
	label _finally_;
	const
		BufferSize = Stream.TempBufferSize;
	var
		buf, sbuf, sparam, sigPtr: pointer;
		sigSize, bufSize, sbufSize, bufSaved, appSize: size_t;
	begin
		sigSize := length(signature) * sizeof(char);
		Assert(sigSize > 0);
		if sigSize > BufferSize then
		begin
			Assert(no, 'слишком длинная сигнатура');
			exit;
		end;
		if not Assigned(MakeRef(s)) then exit;

		buf := GetMem(BufferSize);
		bufSaved := 0;
		sparam := nil;
		repeat
			appSize := s^.TryRead(buf + bufSaved, BufferSize - bufSaved);
			bufSize := bufSaved + appSize;
			// writeln('readed: ',appSize,', saved: ', bufSaved, ', buffer: ', BufferToString(buf, bufSize));
			if appSize > 0 then bufSaved := min(bufSize - 1, sigSize - 1) else bufSaved := 0;
			// writeln('*saved: ', bufSaveD);
			sbuf := buf;
			sbufSize := bufSize;

			repeat
				sigPtr := memfind(pointer(signature), sigSize, sbuf, sbufSize);
				// if assigned(sigPtr) then write('sigpos = ', sigPtr - sbuf) else write('sigpos = NONE'); writeln(', sbufSize = ', sbufSize);
				if not Assigned(sigPtr) then
				begin
					if Assigned(sparam) then
						block(sbuf, sbufSize - bufSaved, sparam);
					break;
				end else
				begin
					if Assigned(sparam) then
					begin
						block(sbuf, sigPtr - sbuf, sparam);
						if Assigned(ends) then ends(sparam);
					end;

					if Assigned(start) then sparam := start(param) else sparam := @sparam;
					if not Assigned(sparam) then goto _finally_;

					block(sigPtr, sigSize, sparam);
					sbufSize -= size_t(sigPtr - sbuf);
					sbufSize -= sigSize;
					sbuf := sigPtr + sigSize;
					bufSaved := min(bufSaved, sbufSize);
				end;
			until sbufSize = 0;
			memcpy(buf + bufSize - bufSaved, buf, bufSaved);
		until bufSize = 0;

	_finally_:
		if Assigned(sparam) and Assigned(ends) then ends(sparam);
		FreeMem(buf);
		Release(s);
	end;

	procedure RegionAllocator.Init(preallocateHint: size_t = 0);
	begin
		last := nil;
		if preallocateHint > 0 then
		begin
			Allocate(preallocateHint, [Precise]);
			last^.used := 0;
		end;
	end;

	procedure RegionAllocator.Done;
	var
		c, t: pBlock;
	begin
		c := last;
		while Assigned(c) do
		begin
			t := c;
			c := c^.prev;
			FreeMem(t);
		end;
	end;

	function RegionAllocator.Allocate(size: size_t; flags: AllocateFlags = []): pointer;
	var
		allocate: size_t;
		b: pBlock;
	begin
		b := last;
		if Assigned(b) and (b^.used + size <= b^.allocated) then
		begin
			result := pointer(pBaseDataType(b^.data)) + b^.used;
			b^.used += size;
		end else
			if size = 0 then result := nil else
			begin
				if Precise in flags then allocate := size else
					if Assigned(last) then allocate := 2 * last^.allocated else
						allocate := 8 * size;

				b            := GetMem(sizeof(b^) - sizeof(b^.data) + allocate);
				b^.allocated := allocate;
				b^.used      := size;
				b^.prev      := last;
				result       := pointer(pBaseDataType(b^.data));
				last         := b;
			end;
	end;

	function Diff.Based.Build(a: pElem; na: size_t; b: pElem; nb: size_t; minBlock: size_t; aMethod: Method): Based;
		function BuildSrc: Source;
		begin
			result.a  := a;
			result.na := na;
			result.b  := b;
			result.nb := nb;
		end;

		function BuildOpts: Options;
		begin
			result.minBlock := RangeCheckMin(minBlock, 1, 'Diff.minBlock');
		end;
	begin
		(@result)^.Init;

		case aMethod of
			Precise: result.BuildPrecise(BuildSrc, BuildOpts);
			Approx: result.BuildApprox(a, na, b, nb, minBlock);
			else raise ExhaustiveCase(ord(aMethod), 'Diff.Based.method');
		end;
	end;

	function GetBasedToStringItem(id: uint; param: pointer): string;
	var
		b: ^Diff.Based absolute param;

		function DescribeRange: string;
		begin
			if b^.ops[id].start <> b^.ops[id].count then
				result := ToString(b^.ops[id].start) + '-' + ToString(b^.ops[id].start + uint(b^.ops[id].count - 1))
			else
				result := ToString(b^.ops[id].start);
		end;

	begin
		if id < size_t(length(b^.ops)) then
			result := IfThen(b^.ops[id].takeFromA, 'A', 'B') + ': ' + DescribeRange
		else
			result := 'totalTakenFromA = ' + ToString(b^.totalTakenFromA);
	end;

	function Diff.Based.ToString: string;
	begin
		result := SeparatedList.Join(length(ops) + 1, @GetBasedToStringItem, @self, EOL);
	end;

	procedure Diff.Based.Init;
	begin
		totalTakenFromA := 0;
		ops := nil;
	end;

	procedure Diff.Based.BuildPrecise(const src: Source; const opts: Options);
	var
		d: PreciseBased;
		c: PreciseBased.pChainOp;
		nops: size_t;
	begin
		d.Init(src, opts);
		c := d.Build(0, 0);
		totalTakenFromA := c^.takenFromA;
		nops := 0;
		while Assigned(c) do
		begin
			if (nops > 0) and (ops[nops-1].takeFromA = (c^.n and d.N_FROM_A_BIT <> 0)) and (ops[nops-1].start + ops[nops-1].count = c^.start) then
				ops[nops-1].count += 1
			else
				AddOp(c^.n and d.N_FROM_A_BIT <> 0, c^.start, c^.n and d.N_COUNT_MASK, nops);
			c := c^.next;
		end;
		SetLength(ops, nops);
		d.Done;
	end;

	procedure Diff.Based.BuildApprox(a: pElem; na: size_t; b: pElem; nb: size_t; minBlock: size_t);
		function Find(a: pElem; na: size_t; b: pElem; nb: size_t; out ap, bp, count: size_t): boolean;
		var
			ia, ib: size_t;
			curLen, curPenalty, bestLen, bestPenalty: size_t;
		begin
			if (na < minBlock) or (nb < minBlock) then exit(no);

			// Найти общий фрагмент длины не менее minBlock.
			bestLen := minBlock - 1;
			bestPenalty := 0;
			ia := 0;
			while ia + bestLen < na do
			begin
				if (bestLen >= minBlock) and (ia > bestLen) then break;
				ib := 0;
				while ib + bestLen < nb do
				begin
					if (bestLen >= minBlock) and (ib > bestLen) then break;
					curLen := Diff.EqualLen(a + ia, na - ia, b + ib, nb - ib);
					if curLen >= minBlock then
					begin
						curPenalty := {ia + ib}0;
						if curLen + bestPenalty > bestLen + curPenalty then
						begin
							bestLen := curLen;
							bestPenalty := curPenalty;
							ap := ia;
							bp := ib;
						end;
					end;
					inc(ib);
				end;
				inc(ia);
			end;

			count := bestLen;
			result := bestLen >= minBlock;
		end;

	var
		nops: size_t;

		procedure Recurse(na, ap, nb, bp: size_t);
		var
			aofs, bofs, count: size_t;
		begin
			while Find(a+ap, na-ap, b+bp, nb-bp, aofs, bofs, count) do
			begin
				if bofs > 0 then Recurse(ap+aofs, ap, bp+bofs, bp);

				AddOp(yes, ap + aofs, count, nops);
				totalTakenFromA += count;
				ap += aofs + count;
				bp += bofs + count;
			end;

			if bp < nb then AddOp(no, bp, nb - bp, nops);
		end;

	begin
		nops := 0;
		Recurse(na, 0, nb, 0);
		SetLength(ops, nops);
	end;

	procedure Diff.Based.SetOp(index: size_t; takeFromA: boolean; start, count: size_t);
	begin
		ops[index].takeFromA := takeFromA;
		ops[index].start     := start;
		ops[index].count     := count;
	end;

	procedure Diff.Based.AddOp(takeFromA: boolean; start, count: size_t; var nOps: size_t);
	begin
		inc(nOps);
		if nOps > size_t(length(ops)) then SetLength(ops, 2*nOps);
		SetOp(nOps-1, takeFromA, start, count);
	end;

	procedure Diff.Based.Serialize(s: pStream; startPass: StartPassProc; saveB: SaveBProc; param: pointer);
	var
		state: SerializationState;
		fmtChooser: UiBinaryFormatChooser;
		fmt: UiBinaryFormat absolute state.fmt;
		ap: size_t;
		dStart, countx, pass: uint;
		fromA: boolean;
		i: sint;
	begin
		fmtChooser.Init('формата чисел в диффе'); state.fmtChooser := @fmtChooser;
		try
			for pass := 0 to 1 do
			begin
				ap := 0; state.pass := pass;
				if Assigned(startPass) then startPass(param);
				if pass = 1 then
				begin
					VarInt.Write(s, length(ops));
					if length(ops) > 0 then Serialize_enum(s, ord(fmt), UiBinaryFormatPrefixCodes);
				end;

				for i := 0 to High(ops) do
				begin
					fromA := ops[i].takeFromA;
					countx := ops[i].count shl COUNT_EXTRA_BITS or IfThen(fromA, COUNTX_FROM_A_BIT, 0);
					if pass = 0 then fmtChooser.Note(countx) else Serialize_ui(s, countx, fmt);
					if fromA then
					begin
						Assert(ops[i].start >= ap, Format('Дифф неупорядочен. fromA = {0}, start = {1}, минимум {2}.', [fromA, ops[i].start, ap]));
						dStart := ops[i].start - ap;
						if pass = 0 then fmtChooser.Note(dStart) else Serialize_ui(s, dStart, fmt);
						ap := ops[i].start + ops[i].count;
					end else
						saveB(s, ops[i], state, param);
				end;
				if pass = 0 then fmt := fmtChooser.Choose;
			end;
		finally
			fmtChooser.Done;
		end;
	end;

	procedure Diff.Based.Deserialize(s: pStream; onCount: LoadCountProc; loadA: LoadAProc; loadB: LoadBProc; param: pointer);
	var
		opIndex, nOps: uint;
		ap, countx, dStart, count: size_t;
		state: DeserializationState;
	begin
		ap := 0;
		nOps := RangeCheck(VarInt.Read(s), Diff.Based.NOpsLimit, 'Diff.Based.NOps'); if Assigned(onCount) then onCount(nOps, param);
		if nOps > 0 then state.fmt := UiBinaryFormat(Deserialize_enum(s, UiBinaryFormatPrefixCodes));
		opIndex := 0;

		while opIndex < nOps do
		begin
			state.opIndex := opIndex;
			countx := Deserialize_ui(s, state.fmt);
			count := countx shr COUNT_EXTRA_BITS;
			if countx and COUNTX_FROM_A_BIT <> 0 then
			begin
				dStart := Deserialize_ui(s, state.fmt);
				if (dStart > High(ap) - ap) or (count > High(ap) - ap - dStart) then raise Error('Неправдоподобный размер фрагмента диффа.');
				ap += dStart;
				loadA(ap, count, state, param);
				ap += count;
			end else
				loadB(s, count, state, param);
			inc(opIndex);
		end;
	end;

	procedure StartSavingBTheSameWay(param: pointer);
	var
		bp: ^size_t absolute param;
	begin
		bp^ := 0;
	end;

	procedure SaveBTheSameWay(s: pStream; const op: Diff.Based.OpDesc; const state: Diff.Based.SerializationState; param: pointer);
	var
		bp: ^size_t absolute param;
		dStart: size_t;
	begin
		Assert(not op.takeFromA);
		Assert(op.start >= bp^, Format('Дифф неупорядочен. bp = {0}, start = {1}.', [bp^, op.start]));
		dStart := op.start - bp^;
		if state.pass = 0 then state.fmtChooser^.Note(dStart) else Serialize_ui(s, dStart, state.fmt);
		bp^ := op.start + op.count;
	end;

	procedure Diff.Based.Serialize(s: pStream);
	var
		bp: size_t;
	begin
		Serialize(s, @StartSavingBTheSameWay, @SaveBTheSameWay, @bp);
	end;

type
	LoadingState = record
		d: Diff.pBased;
		bp: size_t;
	end;

	procedure LoadCountTheSameWay(nOps: uint; param: pointer);
	var
		state: ^LoadingState absolute param;
	begin
		SetLength(state^.d^.ops, nOps);
	end;

	procedure LoadATheSameWay(start, count: size_t; const dstate: Diff.Based.DeserializationState; param: pointer);
	var
		state: ^LoadingState absolute param;
	begin
		state^.d^.SetOp(dstate.opIndex, yes, start, count);
		state^.d^.totalTakenFromA += count;
	end;

	procedure LoadBTheSameWay(s: pStream; count: size_t; const dstate: Diff.Based.DeserializationState; param: pointer);
	var
		state: ^LoadingState absolute param;
		dStart: size_t;
	begin
		dStart := Deserialize_ui(s, dstate.fmt);
		if (dStart > High(state^.bp) - state^.bp) or (count > High(state^.bp) - state^.bp - dStart) then
			raise Error('Неправдоподобный размер фрагмента диффа.');

		state^.d^.SetOp(dstate.opIndex, no, state^.bp + dStart, count);
		state^.bp += dStart + count;
	end;

	function Diff.Based.Deserialize(s: pStream): Based;
	var
		state: LoadingState;
	begin
		state.d       := @result;
		state.bp      := 0;

		result.Init;
		try
			Deserialize(s, @LoadCountTheSameWay, @LoadATheSameWay, @LoadBTheSameWay, @state);
		except
			System.Finalize(result);
			raise;
		end;
	end;

	function Diff.RangeEquals(a, b: pElem; n: size_t): boolean;
	begin
		result := CompareByte(a^, b^, n * sizeof(elem)) = 0;
	end;

	function Diff.EqualLen(a: pElem; na: size_t; b: pElem; nb: size_t): size_t;
	var
		lim: size_t;
	begin
		result := 0;
		lim := min(na, nb);
		while (result < lim) and (a[result] = b[result]) do inc(result);
	end;

	function Diff.PreciseBased.ChainOp.Create(start, n: size_t; next: pChainOp; takenFromA: size_t; var pc: PreciseBased): pChainOp;
	begin
		if Assigned(next) then takenFromA += next^.takenFromA;
		result             := pc.chainOpMem.Allocate(sizeof(ChainOp));
		result^.start      := start;
		result^.n          := n;
		result^.next       := next;
		result^.takenFromA := takenFromA;
	end;

	function Diff.PreciseBased.MemKey.Make(sa, sb: size_t): MemKey;
	begin
		result.sa := sa;
		result.sb := sb;
	end;

	procedure Diff.PreciseBased.Init(const src: Source; const opts: Options);
	var
		i: sint;
	begin
		self.src  := src;
		self.opts := opts;
		SetLength(mem, (src.na + 1) * src.nb);
		for i := 0 to High(mem) do mem[i] := nil;
		chainOpMem.Init(max(src.na, src.nb) * sizeof(ChainOp));
	end;

	procedure Diff.PreciseBased.Done;
	begin
		chainOpMem.Done;
	end;

	function Diff.PreciseBased.Build(sa, sb: size_t): pChainOp;
		function TakenFromA(c: pChainOp): size_t;
		begin
			if Assigned(c) then result := c^.takenFromA else result := 0;
		end;
	var
		b1, b2: pChainOp;
		neq: size_t;
	begin
		if sb = src.nb then result := nil else
		begin
			result := Memoized(sa, sb); if Assigned(result) then exit;
			if sa = src.na then result := ChainOp.Create(sb, src.nb - sb, nil, 0, self) else
			begin
				neq := Diff.EqualLen(src.a + sa, src.na - sa, src.b + sb, src.nb - sb);
				if (neq >= opts.minBlock) then result := ChainOp.Create(sa, neq or N_FROM_A_BIT, Build(sa + neq, sb + neq), neq, self) else
				begin
					b1 := Build(sa + 1, sb);
					b2 := Build(sa, sb + 1);
					if Assigned(b1) and (not Assigned(b2) or (b1^.takenFromA > b2^.takenFromA)) then
						result := b1
					else
						result := ChainOp.Create(sb, 1, b2, 0, self);
				end;
			end;
			Memoize(sa, sb, result);
		end;
	end;

	procedure Diff.PreciseBased.Memoize(sa, sb: size_t; cd: pChainOp);
	begin
		mem[src.nb * sa + sb] := cd;
	end;

	function Diff.PreciseBased.Memoized(sa, sb: size_t): pChainOp;
	begin
		result := mem[src.nb * sa + sb];
	end;

	{function Wave.FindWay(nVerts, from, &to: uint; getEdgesCount: GetEdgesCount; getEdge: GetEdge; param: pointer;
		maxIntermediateVertices: uint; intermediateVertices: pUint; out distance: Distance): uint;
	var
		queue: PointIDQueue;
		visited: PointIDSet;
		a, b, edgeIndex: uint;
		abDist: Wave.Distance;
	begin
		queue.Init;
		visited.Init;
		try
			queue.Put(from);
			while queue.Get(a) do
			begin
				visited.Add(a);
				for edgeIndex := 1 to getEdgesCount(a, param) do
				begin
					b := getEdge(a, edgeIndex - 1, param, abDist);
					if 0 = not abDist then continue;
					if Assigned(visited.Find(b)) then continue;
					queue.Put(b);
				end;
			end;
		finally
			visited.Done;
			queue.Done;
		end;
	end;}

	procedure FloodFill.Result.Done;
	begin
		filled.Done;
	end;

	procedure FloodFill.FourWay(out r: Result; const start, size: UintVec2; test: TestPoint; param: pointer);
	var
		queue: HeterogenousQueue;
		p: pUintVec2;
		new: boolean;

		procedure TryFill(const point: UintVec2);
		var
			new: boolean;
		begin
			if test(point, param) then
			begin
				r.filled.Add(point, new);
				if new then
				begin
					r.min := Min(r.min, point);
					r.max := Max(r.max, point);
					pUintVec2(queue.Put(sizeof(UintVec2)))^ := point;
				end;
			end;
		end;

	begin
		r.min := start;
		r.max := start;
		r.filled.Init;

		queue.Init;
		pUintVec2(queue.Put(sizeof(UintVec2)))^ := start;
		r.filled.Add(start, new);
		repeat
			p := queue.LockGet;
			if not Assigned(p) then break;

			if p^.x > 0 then TryFill(UintVec2.Make(p^.x - 1, p^.y));
			if p^.x + 1 < size.x then TryFill(UintVec2.Make(p^.x + 1, p^.y));
			if p^.y > 0 then TryFill(UintVec2.Make(p^.x, p^.y - 1));
			if p^.y + 1 < size.y then TryFill(UintVec2.Make(p^.x, p^.y + 1));
			queue.UnlockGet(sizeof(UintVec2));
		until no;
		queue.Done;
	end;

end.
