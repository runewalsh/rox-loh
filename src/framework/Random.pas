unit Random;

{$include opts.inc}
{-$define AlwaysParanoiaModBias}
{$define ParanoiaInfiniteRetries}
{$ifdef Debug}
	{-$define TestRNG}
	{$define DebugChooseRandomElements}
{$endif}

interface

uses
	USystem, Errors, Streams, UMath, Utils;

type
	pRNG = ^RNG;
	RNG = object(&Object)
	public type
		Algorithm = (MT19937_32, MT19937_64, TinyMT_32, TinyMT_64, Crawl, Cryptographic {$ifdef TestRNG}, Test {$endif});
		AlgorithmFlag = (Deterministic, ExtremelySlow, Good, Tiny, Quick);
		AlgorithmFlags = set of AlgorithmFlag;

		InitFlag = (NeedLock, DontUseGlobalRNG, _Dummy, _ManualLock);
		InitFlags = set of InitFlag;
	var
		constructor Init(crit: AlgorithmFlag; flags: InitFlags = []);
		constructor Init(newAlgo: Algorithm; const seed: string = ''; flags: InitFlags = []);
		destructor Done; virtual;
		procedure Serialize(s: pStream);
		procedure Deserialize(out rng: RNG; s: pStream); static;
		function ChooseAlgorithm(crit: AlgorithmFlag; flags: InitFlags = []): Algorithm;
		function Describe: string;

		procedure Random(buf: pointer; size: size_t);
		function GetUint32: uint32;
		function GetUint64: uint64;
		function GetUint: uint;
		function GetUint(x: uint): uint;
		function GetUint(min, max: uint): uint;
		function GetBits(n: uint): uint;
		function GetInt(x: sint): sint;
		function GetInt(min, max: sint): sint;

		// без Open — [0; 1], [0; max], etc., с Open — [0; 1), [0, max).
		function GetFloat32: float32;                     function GetFloat32Open: float32;
		function GetFloat32(const max: float32): float32; function GetFloat32Open(const max: float32): float32;
		function GetFloat64: float64;                     function GetFloat64Open: float64;
		function GetFloat64(const max: float64): float64; function GetFloat64Open(const max: float64): float64;
		function GetFloat: float;                         function GetFloatOpen: float;
		function GetFloat(const max: float): float;       function GetFloatOpen(const max: float): float;
		function GetFloat(const min, max: float): float;  function GetFloatOpen(const min, max: float): float;
		function Direction2: Vec2;
		function Direction3: Vec3;

		function Normal: float;
		function Unormal: float;
		function Normal(const x: float): float;
		function Bell(const x: float): float;
		function Bell(const min, peak, max: float): float;
		function RnRound(const x: float): sint;
		function Dice(n, faces: uint): uint;
		function Fdice(n: sint; const max: float): float;
		function Coinflip: boolean;
		function OneIn(million: uint): boolean;
		function XInY(x, y: sint): boolean;
		function Roll(const chance: float): boolean;

	type
		GetWeightFunc = function(id: sint; param: pointer): float;
		NextWeightFunc = function(param: pointer): float;
		VerdictFunc = procedure(param: pointer);
		ChooseFlag = (MayChooseNothing);
		ChooseFlags = set of ChooseFlag;
	var
		function Choose(count: sint; getWeight: GetWeightFunc; param: pointer; flags: ChooseFlags = []): sint;
		function Choose(getWeight: GetWeightFunc; param: pointer; flags: ChooseFlags = []): sint;
		function Choose(nextWeight: NextWeightFunc; param: pointer; flags: ChooseFlags = []): sint;
		function Choose(nextWeight: NextWeightFunc; beats: VerdictFunc; param: pointer; flags: ChooseFlags = []): sint;
		function Choose(nextWeight: NextWeightFunc; beats, dont: VerdictFunc; param: pointer; flags: ChooseFlags = []): sint;
		function Choose(const weights: array of float; flags: ChooseFlags = []): sint;

	type
		PickElement = procedure(id: uint; param: pointer);
		function RandomElements(from, n: sint; pick: PickElement; param: pointer): uint;

	const
		AlgorithmPrefixCodes: array[Algorithm] of string = ('m', 'M', 't', 'T', '@', 'r' {$ifdef TestRNG}, 'D' {$endif});
		AlgorithmIds: array[Algorithm] of string = ('mt19937-32', 'mt19937-64', 'tinyMT-32', 'tinyMT-64', 'crawl', 'cryptographic' {$ifdef TestRNG}, 'test' {$endif});
		AlgorithmFlagIds: array[AlgorithmFlag] of string = ('det', 'slow', 'good', 'tiny', 'quick');
		AlgorithmInfo: array[Algorithm] of record
			flags: AlgorithmFlags;
		end =
		(
			(flags: [Deterministic, Good]),        // MT19937_32
			(flags: [Deterministic, Good]),        // MT19937_64
			(flags: [Deterministic, Tiny]),        // TinyMT_32
			(flags: [Deterministic, Tiny]),        // TinyMT_64
			(flags: [Deterministic, Tiny, Quick]), // Crawl
			(flags: [ExtremelySlow])               // Cryptographic
		{$ifdef TestRNG}, (flags: []) {$endif}    // Test
		);

	private type
		pImplBase = ^ImplBase;
		ImplBase = object
			constructor Init;
			destructor Done; virtual;
			procedure Initialize(const seeds: string; flags: InitFlags); virtual;
			function GetUint32: uint32; virtual; abstract;
			function GetUint64: uint64; virtual; abstract;
			procedure Serialize(s: pStream); virtual;
			procedure Deserialize(s: pStream); virtual;
		end;

		Uint32Seed = array of uint32;
		Uint64Seed = array of uint64;

	var
		impl: pImplBase;
		lock: pThreadLock;
		hasNextNormal: boolean;
		nextNormal: float;
		usedAlgo: Algorithm;
		function CreateImpl(algo: Algorithm): pImplBase; static;
		function Serializable(algo: Algorithm): boolean; static;
		function BellFallback: float;
		procedure SeedToBuf(const seed: string; buf: pointer; bufSize: size_t; flags: InitFlags); static;
		function SeedToUints32(const seed: string; minLen, maxLen: uint; flags: InitFlags): Uint32Seed; static;
		function SeedToUints64(const seed: string; minLen, maxLen: uint; flags: InitFlags): Uint64Seed; static;
		procedure MaybeNothingChosen(r: sint; flags: ChooseFlags);

	type
		Impl32 = object(ImplBase)
			constructor Init;
			function GetUint64: uint64; virtual;
			procedure Serialize(s: pStream); virtual;
			procedure Deserialize(s: pStream); virtual;
		end;

		Impl64 = object(ImplBase)
			constructor Init;
			function GetUint32: uint32; virtual;
			procedure Serialize(s: pStream); virtual;
			procedure Deserialize(s: pStream); virtual;
		strict private
			has32: boolean;
			x32: uint32;
		end;

		pMT19937_32 = ^MT19937_32Impl;
		MT19937_32Impl = object(Impl32)
			procedure Initialize(const seeds: string; flags: InitFlags); virtual;
			function GetUint32: uint32; virtual;
			procedure Serialize(s: pStream); virtual;
			procedure Deserialize(s: pStream); virtual;
		private const
			N = 624;
			M = 397;
		var
			mt: array[0 .. N - 1] of uint32;
			mti: uint;
			procedure Setup(seed: uint32);
		end;

		pMT19937_64 = ^MT19937_64Impl;
		MT19937_64Impl = object(Impl64)
			procedure Initialize(const seeds: string; flags: InitFlags); virtual;
			function GetUint64: uint64; virtual;
			procedure Serialize(s: pStream); virtual;
			procedure Deserialize(s: pStream); virtual;
		private const
			NN = 312;
			MM = 156;
		var
			mt: array[0 .. NN - 1] of uint64;
			mti: uint;
			procedure Setup(const seed: uint64);
		end;

		pTinyMT_32 = ^TinyMT_32Impl;
		TinyMT_32Impl = object(Impl32)
			procedure Initialize(const seeds: string; flags: InitFlags); virtual;
			function GetUint32: uint32; virtual;
			procedure Serialize(s: pStream); virtual;
			procedure Deserialize(s: pStream); virtual;
		private type
			pVariation = ^tVariation;
			tVariation = record
				mat1, mat2, tmat: uint32;
			end;
			State = array[0 .. 3] of uint32;
		const
			Variations: array[0 .. 19] of tVariation =
			(
				(mat1: $8f7011ee; mat2: $fc78ff1f; tmat: $3793fdff),
				(mat1: $877810ef; mat2: $fc38ff0f; tmat: $c7fb7fff),
				(mat1: $837c106f; mat2: $fc18ff07; tmat: $eeb9bdff),
				(mat1: $718e0e31; mat2: $fb88fee3; tmat: $11dbffff),
				(mat1: $50af0a15; mat2: $fa80fea1; tmat: $9ddc99ff),
				(mat1: $14eb029d; mat2: $f8a0fe29; tmat: $46f3ebff),
				(mat1: $0bf4017e; mat2: $f858fe17; tmat: $e8cfecfd),
				(mat1: $09f6013e; mat2: $f848fe13; tmat: $52a0f5ff),
				(mat1: $e51b1ca3; mat2: $f720fdc9; tmat: $f8ebffff),
				(mat1: $ab55156a; mat2: $f550fd55; tmat: $6f33bd7f),
				(mat1: $946a128d; mat2: $f4a8fd2b; tmat: $feac77ff),
				(mat1: $817f102f; mat2: $f400fd01; tmat: $90dbc3ff),
				(mat1: $50ae0a15; mat2: $f288fca3; tmat: $dd2c73ff),
				(mat1: $19e7033c; mat2: $f0c0fc31; tmat: $55e7fd7d),
				(mat1: $0ef001de; mat2: $f078fc1f; tmat: $3ccef3ff),
				(mat1: $e9141d22; mat2: $ef58fbd7; tmat: $ff3f3edf),
				(mat1: $d22f1a45; mat2: $ee80fba1; tmat: $90a5ffff),
				(mat1: $b34e1669; mat2: $ed88fb63; tmat: $6f6e75ff),
				(mat1: $8c71118e; mat2: $ec70fb1d; tmat: $97eeff7f),
				(mat1: $65980cb3; mat2: $eb38facf; tmat: $cc3b75ff)
			);

			Sh0 = 1;
			Sh1 = 10;
			Sh8 = 8;
			Mask1 = $7fffffff;
			EmergencyState: State = (ord('T'), ord('I'), ord('N'), ord('Y'));
		var
			variation: pVariation;
			st: State;
			procedure Setup(const seed: array of uint32);
			function Validate(fix: boolean): boolean;
			procedure NextState;
			function Ini1(x: uint32): uint32; static;
			function Ini2(x: uint32): uint32; static;
		end;

		pTinyMT_64 = ^TinyMT_64Impl;
		TinyMT_64Impl = object(Impl64)
			procedure Initialize(const seeds: string; flags: InitFlags); virtual;
			function GetUint64: uint64; virtual;
			procedure Serialize(s: pStream); virtual;
			procedure Deserialize(s: pStream); virtual;
		private type
			pVariation = ^tVariation;
			tVariation = record
				mat1, mat2: uint32;
				tmat: uint64;
			end;
			State = array[0 .. 1] of uint64;
		const
			Variations: array[0 .. 19] of tVariation =
			(
				(mat1: $fa051f40; mat2: $ffd0fff4; tmat: uint64($58d02ffeffbfffbc)),
				(mat1: $daa51b54; mat2: $fed47fb5; tmat: uint64($a853e7ffeffefffe)),
				(mat1: $c0bf1817; mat2: $fe047f81; tmat: uint64($7fc75ff6ffffffbc)),
				(mat1: $c03f1807; mat2: $fe00ff80; tmat: uint64($14727d7fff7f7ffe)),
				(mat1: $443b0887; mat2: $fa247e89; tmat: uint64($f0d0e77bef7fdffa)),
				(mat1: $275804eb; mat2: $f93c7e4f; tmat: uint64($776007f7fffdfffe)),
				(mat1: $22dd045b; mat2: $f910fe44; tmat: uint64($1de2bf7fefffdfbc)),
				(mat1: $077800ef; mat2: $f83c7e0f; tmat: uint64($7a0836fbbfff7ffe)),
				(mat1: $daa41b54; mat2: $f6dc7db7; tmat: uint64($5fdebffccf3ffffe)),
				(mat1: $bcc21798; mat2: $f5ec7d7b; tmat: uint64($60df36f7fffdfffe)),
				(mat1: $80fe101f; mat2: $f40c7d03; tmat: uint64($f81bf877ffbdbffa)),
				(mat1: $7a840f50; mat2: $f3d8fcf6; tmat: uint64($9746beffffbffffe)),
				(mat1: $611f0c23; mat2: $f3047cc1; tmat: uint64($d2faed7ffdfdfffe)),
				(mat1: $47b908f7; mat2: $f230fc8c; tmat: uint64($49903fffff7efffe)),
				(mat1: $443a0887; mat2: $f22c7c8b; tmat: uint64($b99d5afaffffeffe)),
				(mat1: $047a008f; mat2: $f02c7c0b; tmat: uint64($d9c6b6f7ffbfffbc)),
				(mat1: $e31e1c63; mat2: $ef08fbc2; tmat: uint64($7197ddf7dfffdfbc)),
				(mat1: $dc211b84; mat2: $eef0fbbc; tmat: uint64($9dda76fbdfffeffe)),
				(mat1: $ba471748; mat2: $edc0fb70; tmat: uint64($e4c03f77ffbefffe)),
				(mat1: $a0dd141b; mat2: $ed147b45; tmat: uint64($7da1b5dfedbffffe))
			);

			Sh0 = 12;
			Sh1 = 11;
			Sh8 = 8;
			Mask1 = uint64($7fffffffffffffff);
			EmergencyState: State = (ord('T'), ord('M'));
		var
			variation: pVariation;
			st: State;
			procedure Setup(const seed: array of uint64);
			function Validate(fix: boolean): boolean;
			procedure NextState;
			function Ini1(const x: uint64): uint64; static;
			function Ini2(const x: uint64): uint64; static;
		end;

		pCrawl = ^CrawlImpl;
		CrawlImpl = object(Impl32)
			procedure Initialize(const seeds: string; flags: InitFlags); virtual;
			function GetUint32: uint32; virtual;
			procedure Serialize(s: pStream); virtual;
			procedure Deserialize(s: pStream); virtual;
		private
			lcg, mwcm, mwcc, xorshift, lfsr: uint32;
		end;

		pCryptographic = ^CryptographicImpl;
		CryptographicImpl = object(ImplBase)
			function GetUint32: uint32; virtual;
			function GetUint64: uint64; virtual;
		end;

	{$ifdef TestRNG}
		pTest = ^TestImpl;
		TestImpl = object(ImplBase)
			ret: uint64;
			procedure Initialize(const seeds: string; flags: InitFlags); virtual;
			function GetUint32: uint32; virtual;
			function GetUint64: uint64; virtual;
		end;
	{$endif}
	const
		HAS_NEXT_NORMAL_BITN = 0;
	end;

var
	GlobalRNG: RNG;

implementation

{$if defined(use_serialization) or defined(Debug)}
uses
	{$ifdef use_serialization} Serialization {$ifdef Debug}, {$endif} {$endif}
	{$ifdef Debug} ULog {$endif};
{$endif}

{$define _Lock := if Assigned(lock) then lock^.Enter}
{$define _Unlock := if Assigned(lock) then lock^.Leave}

{$ifdef ParanoiaInfiniteRetries}
const
	InfiniteRetriesLimit = 64;
{$endif}

	constructor RNG.Init(crit: AlgorithmFlag; flags: InitFlags = []);
	begin
		Init(ChooseAlgorithm(crit, flags), '', flags);
	end;

	constructor RNG.Init(newAlgo: Algorithm; const seed: string; flags: InitFlags = []);
	begin
		inherited Init;
	{$ifdef Debug} if seed <> '' then Log('Запрошен ГПСЧ с явным зерном "' + seed + '".'); {$endif}
		usedAlgo := newAlgo;
		if _Dummy in flags then impl := nil else
		begin
			impl := CreateImpl(newAlgo);
			impl^.Initialize(seed, flags);
		end;
		hasNextNormal := no;

		lock := nil;
		if NeedLock in flags then lock := ThreadLock.Create;
	end;

	destructor RNG.Done;
	begin
		if Assigned(impl) then dispose(impl, Done);
		lock^.Free(lock);
		inherited Done;
	end;

	procedure RNG.Serialize(s: pStream);
	var
		flags: uint;
	begin
		if Assigned(lock) or not Serializable(usedAlgo) then
			raise Error('Состояние этого генератора случайных чисел не может быть сохранено.');
		// Вообще-то можно и сохранять (без состояния, только энум), но смысла в этом немного.

		Serialize_enum(s, ord(usedAlgo), AlgorithmPrefixCodes);
		flags := (uint(hasNextNormal) shl HAS_NEXT_NORMAL_BITN);
		Serialize_ui8(s, flags);
		impl^.Serialize(s);
		if hasNextNormal then Serialize_f32(s, nextNormal);
	end;

	procedure RNG.Deserialize(out rng: RNG; s: pStream);
	var
		flags: uint;
	begin
		rng.Init(Algorithm(0), '', [_Dummy]);
		try
			rng.usedAlgo := Algorithm(Deserialize_enum(s, AlgorithmPrefixCodes));
			if not Serializable(rng.usedAlgo) then raise Error('Восстановить состояние предлагаемого Г(П)СЧ невозможно.');
			flags := Deserialize_ui8(s);

			rng.impl := CreateImpl(rng.usedAlgo);
			rng.impl^.Deserialize(s);

			rng.hasNextNormal := flags and (1 shl HAS_NEXT_NORMAL_BITN) <> 0;
			if rng.hasNextNormal then rng.nextNormal := Deserialize_f32(s);
		except
			rng.Done;
			raise;
		end;
	end;

	function RNG.ChooseAlgorithm(crit: AlgorithmFlag; flags: InitFlags = []): Algorithm;
	var
		suit: array[0 .. ord(High(Algorithm))] of Algorithm;
		n, index: uint;
		algo: Algorithm;
	begin
		n := 0;
		for algo in Algorithm do
			if crit in AlgorithmInfo[algo].flags then
			begin
				suit[n] := algo;
				inc(n);
			end;

		case n of
			0: raise Error('Не удалось подобрать алгоритм под критерий "{0}".', AlgorithmFlagIds[crit]);
			1: index := 0;
			else
				if DontUseGlobalRNG in flags then
				begin
					NonCrucialRandom(@index, sizeof(index));
					index := index mod n;
				end else
					index := GlobalRNG.GetUint(n);
		end;
		result := suit[index];
	end;

	function RNG.Describe: string;
	begin
		result := AlgorithmIds[usedAlgo];
	end;

	procedure RNG.Random(buf: pointer; size: size_t);
	var
		u64: pUint64 absolute buf;
		b: pUint8 absolute buf;
		t: uint64;
	begin
		_Lock;
		while size >= sizeof(uint64) do
		begin
			u64[0] := impl^.GetUint64;
			inc(u64);
			dec(size, sizeof(u64[0]));
		end;

		if size > 0 then
		begin
			if size > sizeof(uint32) then t := impl^.GetUint64 else t := impl^.GetUint32;
			repeat
				b[0] := t and High(b[0]);
				t := t shr bitsizeof(b[0]);
				inc(b);
				dec(size, sizeof(b[0]));
			until size = 0;
		end;
		_Unlock;
	end;

	function RNG.GetUint32: uint32;
	begin
		_Lock;
		result := impl^.GetUint32;
		_Unlock;
	end;

	function RNG.GetUint64: uint64;
	begin
		_Lock;
		result := impl^.GetUint64;
		_Unlock;
	end;

	function RNG.GetUint: uint;
	begin
		result :=
		{$if sizeof(uint) = sizeof(uint32)} GetUint32
		{$elseif sizeof(uint) = sizeof(uint64)} GetUint64
		{$else} {$error RNG.GetUint: неподдерживаемый размер uint} {$endif};
	end;

	function RNG.GetUint(x: uint): uint;

	{$ifdef ParanoiaInfiniteRetries}
		function DontRetryAnymore(var retries: uint; var rand: uint): boolean;
		begin
			inc(retries);
		{$ifdef Debug} stat.Note(max_random_uint_retries, retries); {$endif}
			result := retries > InfiniteRetriesLimit;
			if result then
			begin
				Assert(no, 'RNG.GetUint зациклилась.');
				rand := 0;
			end;
		end;
	{$endif}

	var
		divisor, rand {$ifdef ParanoiaInfiniteRetries}, retries {$endif}: uint;
	begin
		if x <= 1 then exit(0);
	{$ifdef AlwaysParanoiaModBias}
		{$note Включена паранойя статистической погрешности mod'а. Это избавляется от эффекта ценой скорости RNG.GetUint(x).}
	{$else}
		if x <= High(result) shr (bitsizeof(result) div 2) then
			result := self.GetUint mod x
		else
	{$endif}
		begin
			divisor := High(result);
			divisor := {divisor - divisor mod x — O4 ругается, вернуть этот вариант, когда перестанет} x * (divisor div x);

		{$ifdef ParanoiaInfiniteRetries} retries := 0; {$endif}
			repeat
				rand := self.GetUint;
			until (rand < divisor) {$ifdef ParanoiaInfiniteRetries} or DontRetryAnymore(retries, rand) {$endif};
			result := rand mod x;
		end;
	end;

	function RNG.GetUint(min, max: uint): uint;
	begin
		Assert(not ((min = 0) and (max = High(uint))), 'Используй GetUint без параметров.');
		if min <= max then
			result := min + GetUint(max - min + 1)
		else
			result := GetUint(max, min);
	end;

	function RNG.GetBits(n: uint): uint;
	begin
		Assert((n > 0) and (n < bitsizeof(uint)));
		result := GetUint and (1 shl n - 1);
	end;

	function RNG.GetInt(x: sint): sint;
	begin
		if x >= 0 then
			result := GetUint(x)
		else
			result := -sint(GetUint(-x));
	end;

	function RNG.GetInt(min, max: sint): sint;
	begin
		if min <= max then
			result := min + sint(GetUint(max - min + 1))
		else
			result := GetInt(max, min);
	end;

{$define impl :=
	begin
		result := (GetUint32 * (1.0 / {$ifdef open} 4294967296.0 {$else} 4294967295.0 {$endif})) {$ifdef mul} * mul {$endif};
	end; {$undef mul} {$undef open}}
	function RNG.GetFloat32: float32; impl
	function RNG.GetFloat32Open: float32; {$define open} impl
	function RNG.GetFloat32(const max: float32): float32; {$define mul := max} impl
	function RNG.GetFloat32Open(const max: float32): float32; {$define open} {$define mul := max} impl
{$undef impl}

{$define impl :=
	var
		a, b: uint32;
	begin
		a := GetUint32 shr 5;
		b := GetUint32 shr 6;
		result := ((a * 67108864.0 + b) * (1.0 / {$ifdef open} 9007199254740992.0 {$else} 9007199254740991.0 {$endif})) {$ifdef mul} * mul {$endif};
	end; {$undef mul} {$undef open}}
	function RNG.GetFloat64: float64; impl
	function RNG.GetFloat64Open: float64; {$define open} impl
	function RNG.GetFloat64(const max: float64): float64; {$define mul := max} impl
	function RNG.GetFloat64Open(const max: float64): float64; {$define open} {$define mul := max} impl
{$undef impl}

{$define impl:=
	begin
		result :=
		{$if sizeof(float) = sizeof(float32)} {$ifdef open} GetFloat32Open {$else} GetFloat32 {$endif}
		{$else} {$if sizeof(float) <> sizeof(float64)} {$note RNG.GetFloat: float size mismatch, still using 'double'} {$endif}
			{$ifdef open} GetFloat64Open {$else} GetFloat64 {$endif}
		{$endif}
		{$ifdef ze_max} (ze_max) {$endif};
	end; {$undef ze_max} {$undef open}}
	function RNG.GetFloat: float; impl
	function RNG.GetFloatOpen: float; {$define open} impl
	function RNG.GetFloat(const max: float): float; {$define ze_max := max} impl
	function RNG.GetFloatOpen(const max: float): float; {$define open} {$define ze_max := max} impl
{$undef impl}

	function RNG.GetFloat(const min, max: float): float;
	begin
		result := min + self.GetFloat * (max - min);
	end;

	function RNG.GetFloatOpen(const min, max: float): float;
	begin
		result := min + self.GetFloatOpen * (max - min);
	end;

	function RNG.Direction2: Vec2;
	var
		angle: float;
	begin
		angle := GetFloat(TwoPi);
		result.x := cos(angle);
		result.y := sin(angle);
	end;

	function RNG.Direction3: Vec3;
	var
		z, len, angle: float;
	begin
		z := GetFloat(-1, 1);
		len := sqrt(1.0 - sqr(z));
		angle := GetFloat(TwoPi);
		result.x := cos(angle) * len;
		result.y := sin(angle) * len;
		result.z := z;
	end;

{$define impl:=
	function func: float;
	const
		Outer = {$if defined(orig)} 1.0 {$elseif defined(unsigned)} 1.0 {$else} 0.25 {$endif};

	{$ifdef ParanoiaInfiniteRetries}
		function DontRetryAnymore(var retries: uint; var v1, v2, s: float): boolean;
		begin
			inc(retries);
		{$ifdef Debug} stat.Note(max_normal_random_retries, retries); {$endif}
			result := retries > InfiniteRetriesLimit;
			if result then
			begin
				Assert(no, 'RNG.Normal зациклилась.');
				v1 := 0.0;
				v2 := 0.0;
				s  := Outer;
			end;
		end;
	{$endif}

	var
		v1, v2, s, k: float;
	{$ifdef ParanoiaInfiniteRetries} retries: uint; {$endif}
	begin
		if hasNext then
		begin
			hasNext := no;
			exit(next);
		end;

	{$ifdef ParanoiaInfiniteRetries} retries := 0; {$endif}
		repeat
		{$if defined(orig)}
			v1 := 2.0 * GetFloat - 1.0;
			v2 := 2.0 * GetFloat - 1.0;
		{$elseif defined(unsigned)}
			v1 := GetFloat;
			v2 := GetFloat;
		{$else}
			v1 := GetFloat - 0.5;
			v2 := GetFloat - 0.5;
		{$endif}
			s := sqr(v1) + sqr(v2);
		until (s > 0.0) and (s <= Outer) {$ifdef ParanoiaInfiniteRetries} or DontRetryAnymore(retries, v1, v2, s) {$endif};

	{$if defined(orig)} k := sqrt((-2.0 * ln(s)) / s);
	{$elseif defined(unsigned)} k := sqrt((-2.0 * ln(s)) / s);
	{$else}
		k := ln(4.0) + ln(s);
		k := sqrt(-2.0 * k / s);
	{$endif}
		result := v1 * k;
	{$if defined(orig) and defined(unsigned)} result := abs(result); {$endif}
		next := v2 * k;
		hasNext := yes;
	end; {$undef func} {$undef hasNext} {$undef next} {$undef unsigned} {$undef orig}}

{$define func := RNG.Normal} {$define hasNext := hasNextNormal} {$define next := nextNormal} impl
{$undef impl}

	function RNG.Unormal: float;
	begin
		result := abs(Normal);
	end;

	function RNG.Normal(const x: float): float;
	begin
		result := x * self.Normal;
	end;

	function RNG.Bell(const x: float): float;
	begin
		result := Normal(x * (1/3));
		if (result < -x) or (result > x) then
			result := x * BellFallback;
	end;

	function RNG.Bell(const min, peak, max: float): float;
	var
		k: float;
	begin
		Assert((min <= peak) and (peak <= max));
		k := Unormal * (1 / 3);
		if k > 1 then k := abs(BellFallback);
		if GetFloat * (max - min) < (peak - min) then
			result := peak + (min - peak) * k
		else
			result := peak + (max - peak) * k;
	end;

	function RNG.RnRound(const x: float): sint;
	var
		fr: float;
	begin
		fr := frac(x);
		result := trunc(x);
		if (fr > 0) and (GetFloat <= fr) then inc(result);
		if (fr < 0) and (-GetFloat >= fr) then dec(result);
	end;

	function RNG.Dice(n, faces: uint): uint;
	var
		i: uint;
	begin
		result := n;
		for i := 1 to n do
			result += GetUint(faces);
	end;

	function RNG.Fdice(n: sint; const max: float): float;
	var
		i: sint;
	begin
		result := 0.0;
		for i := 1 to n do
			result += GetFloat;
		result *= max;
	end;

	function RNG.Coinflip: boolean;
	begin
		result := boolean(GetBits(1));
	end;

	function RNG.OneIn(million: uint): boolean;
	begin
		result := GetUint(million) = 0;
	end;

	function RNG.XInY(x, y: sint): boolean;
	begin
		result := (x >= y) or ((x > 0) and (GetInt(y) < x));
	end;

	function RNG.Roll(const chance: float): boolean;
	begin
		result := GetFloatOpen < chance;
	end;

	function RNG.Choose(count: sint; getWeight: GetWeightFunc; param: pointer; flags: ChooseFlags = []): sint;
	var
		cw, sum: float;
		i: sint;
	begin
		sum := 0.0;
		i := 0;
		while (count < 0) or (i < count) do
		begin
			cw := getWeight(i, param);
			if cw > 0.0 then sum += cw else
				if (count < 0) and (cw < 0.0) then
				begin
					count := i;
					break;
				end;
			inc(i);
		end;
		sum := GetFloat(sum);

		result := -1;
		for i := 0 to count - 1 do
		begin
			cw := getWeight(i, param);
			if cw > 0.0 then
			begin
				sum -= cw;
				if sum < 0.0 then exit(i) else result := i;
			end;
		end;

		MaybeNothingChosen(result, flags);
	end;

	function RNG.Choose(getWeight: GetWeightFunc; param: pointer; flags: ChooseFlags = []): sint;
	begin
		result := Choose(-1, getWeight, param, flags);
	end;

	function RNG.Choose(nextWeight: NextWeightFunc; param: pointer; flags: ChooseFlags = []): sint;
	begin
		result := Choose(nextWeight, nil, param, flags);
	end;

	function RNG.Choose(nextWeight: NextWeightFunc; beats: VerdictFunc; param: pointer; flags: ChooseFlags = []): sint;
	begin
		result := Choose(nextWeight, beats, nil, param, flags);
	end;

	function RNG.Choose(nextWeight: NextWeightFunc; beats, dont: VerdictFunc; param: pointer; flags: ChooseFlags = []): sint;
	var
		i: sint;
		cw, sum: float;
	begin
		i := 0;
		result := -1;
		sum := 0.0;
		repeat
			cw := nextWeight(param);
			if cw < 0.0 then break;

			sum += cw;
			if (cw > 0.0) and ((result < 0) or (GetFloat * sum < cw)) then
			begin
				result := i;
				if Assigned(beats) then beats(param);
			end else
				if Assigned(dont) then dont(param);
			inc(i);
		until no;

		MaybeNothingChosen(result, flags);
	end;

	function GetWeightFromFloatArray(id: sint; param: pointer): float;
	begin
		result := pFloat(param)[id];
	end;

	function RNG.Choose(const weights: array of float; flags: ChooseFlags = []): sint;
	begin
		result := Choose(length(weights), @GetWeightFromFloatArray, pFloat(weights), flags);
	end;

	function RNG.RandomElements(from, n: sint; pick: PickElement; param: pointer): uint;

		{$define elem := sint} {$define InsertInSorted} {$include sorted.inc}

		procedure PickOne(id: sint; pick: PickElement; param: pointer; var nPicked: uint);
		begin
			pick(id, param);
			inc(nPicked);
		end;

		procedure Tracked(from, n: sint; pick: PickElement; param: pointer; var nPicked: uint);
		var
			selected: array of sint;
			elemNo, randomElement, i: sint;
		begin
			SetLength(selected, n);
			for elemNo := 0 to n - 1 do
			begin
				randomElement := GetUint(from - elemNo);
				for i := 0 to elemNo - 1 do
					if selected[i] <= randomElement then
						inc(randomElement); // можно ли быстрее?
			{$ifdef DebugChooseRandomElements}
				for i := 0 to elemNo - 1 do
					Assert(randomElement <> selected[i]);
			{$endif}
				PickOne(randomElement, pick, param, nPicked);
				InsertInSorted(pSint(selected), elemNo, randomElement);
			end;
		end;

		procedure SelectDelete(from, n: sint; pick: PickElement; param: pointer; var nPicked: uint);
		var
			items: array of sint;
			i, id: sint;
		begin
			SetLength(items, from);
			for i := 0 to High(items) do
				items[i] := i;
			for i := 0 to n - 1 do
			begin
				id := GetUint(from - i);
				PickOne(items[id], pick, param, nPicked);
				items[id] := items[from - 1 - i];
			end;
		end;

	begin
		result := 0;
		if n > from then n := from;
		if (from < 64) or (n < from div 4) then
			Tracked(from, n, pick, param, result)
		else
			SelectDelete(from, n, pick, param, result);
	end;

	constructor RNG.ImplBase.Init; begin end;
	destructor RNG.ImplBase.Done; begin end;
	procedure RNG.ImplBase.Initialize(const seeds: string; flags: InitFlags); begin unused_args seeds _ flags end_list end;
	procedure RNG.ImplBase.Serialize(s: pStream); begin Assert(@s = @s); end;
	procedure RNG.ImplBase.Deserialize(s: pStream); begin Assert(@s = @s); end;

	function RNG.CreateImpl(algo: Algorithm): pImplBase;
	begin
		case algo of
			   MT19937_32: result := new(pMT19937_32, Init);
			   MT19937_64: result := new(pMT19937_64, Init);
			    TinyMT_32: result := new(pTinyMT_32, Init);
			    TinyMT_64: result := new(pTinyMT_64, Init);
			        Crawl: result := new(pCrawl, Init);
		{$ifdef TestRNG}
			         Test: result := new(pTest, Init);
		{$endif}
			Cryptographic: result := new(pCryptographic, Init);
			         else  raise ExhaustiveCase(ord(algo), 'RNG.algo');
		end;
	end;

	function RNG.Serializable(algo: Algorithm): boolean;
	begin
		result := Deterministic in AlgorithmInfo[algo].flags;
	end;

	function RNG.BellFallback: float;
	begin
		result := clamp(1.0 - (2/3) * (GetFloat + GetFloat + GetFloat), -1, 1);
	end;

	procedure RNG.SeedToBuf(const seed: string; buf: pointer; bufSize: size_t; flags: InitFlags);
	var
		ps: pointer;
		ss: size_t;
	begin
		if seed = '' then
		begin
			if DontUseGlobalRNG in flags then NonCrucialRandom(buf, bufSize) else GlobalRNG.Random(buf, bufSize);
			exit;
		end;

		ps := pointer(seed);
		ss := length(seed) * sizeof(char);
		if ss <= bufSize then
		begin
			repeat
				memcpy(ps, buf, ss);
				buf += ss;
				bufSize -= ss;
			until bufSize <= ss;
			memcpy(ps, buf, bufSize);
		end else
		begin
			memcpy(ps, buf, bufSize);
			ps += bufSize;
			ss -= bufSize;

			while ss > bufSize do
			begin
				memxor(buf, ps, buf, bufSize);
				ss -= bufSize;
				ps += bufSize;
			end;
			memxor(buf, ps, buf, ss);
		end;
	end;

{$define impl :=
	const
		OneSize = sizeof((@result)[0]);
	var
		len: uint;
		i: sint;
	begin
		len := (length(seed) + OneSize - 1) div OneSize;
		if len < minLen then len := minLen;
		if len > maxLen then len := maxLen;
		Assert(len > 0);
		SetLength(result, len);
		SeedToBuf(seed, pointer(result), length(result) * OneSize, flags);
		for i := 0 to High(result) do result[i] := BEtoN(result[i]);
	end;}

	function RNG.SeedToUints32(const seed: string; minLen, maxLen: uint; flags: InitFlags): Uint32Seed; impl
	function RNG.SeedToUints64(const seed: string; minLen, maxLen: uint; flags: InitFlags): Uint64Seed; impl
{$undef impl}

	procedure RNG.MaybeNothingChosen(r: sint; flags: ChooseFlags);
	begin
		if (r < 0) and not (MayChooseNothing in flags) then
			raise Error('Не удалось выбрать из взвешенных вариантов. Возможно, их не было вовсе или все они имели нулевой вес.');
	end;

	constructor RNG.Impl32.Init;
	begin
		inherited Init;
	end;

	function RNG.Impl32.GetUint64: uint64;
	begin
		result := uint64(GetUint32) or (uint64(GetUint32) shl bitsizeof(uint32));
	end;

	procedure RNG.Impl32.Serialize(s: pStream);
	begin
		Assert(@s = @s);
	end;

	procedure RNG.Impl32.Deserialize(s: pStream);
	begin
		Assert(@s = @s);
	end;

	constructor RNG.Impl64.Init;
	begin
		inherited Init;
		has32 := no;
	end;

unchecked
	function RNG.Impl64.GetUint32: uint32;
	var
		t: uint64;
	begin
		if has32 then
		begin
			result := x32;
			has32 := no;
		end else
		begin
			t := GetUint64;
			result := t;
			x32 := t shr bitsizeof(uint32);
			has32 := yes;
		end;
	end;
end_unchecked

	procedure RNG.Impl64.Serialize(s: pStream);
	begin
		Serialize_ui8(s, uint(has32));
		if has32 then Serialize_ui32(s, x32);
	end;

	procedure RNG.Impl64.Deserialize(s: pStream);
	begin
		has32 := Deserialize_ui8(s) <> 0;
		if has32 then x32 := Deserialize_ui32(s);
	end;

unchecked
	procedure RNG.MT19937_32Impl.Initialize(const seeds: string; flags: InitFlags);

		procedure Wrap(var i: uint);
		begin
			inc(i);
			if i >= N then
			begin
				mt[0] := mt[N - 1];
				i := 1;
			end;
		end;

	var
		i, k, iseed: uint;
		seed: Uint32Seed;
	begin
		seed := RNG.SeedToUints32(seeds, 8, N, flags);
		Setup(19650218);
		i := 1;
		iseed := 0;

		for k := 0 to N - 1 do
		begin
			mt[i] := (mt[i] xor ((mt[i-1] xor (mt[i-1] shr 30)) * 1664525)) + seed[iseed] + k;
			Wrap(i);
			inc(iseed);
			if iseed >= uint(length(seed)) then iseed := 0;
		end;

		for k := 0 to N - 2 do
		begin
			mt[i] := (mt[i] xor ((mt[i-1] xor (mt[i-1] shr 30)) * 1566083941)) - i;
			Wrap(i);
		end;

		mt[0] := 1 shl 31;
	end;

	procedure RNG.MT19937_32Impl.Setup(seed: uint32);
	var
		i: uint;
	begin
		mt[0] := seed;
		for i := 1 to N - 1 do
			mt[i] := 1812433253 * (mt[i-1] xor (mt[i-1] shr 30)) + i;
		mti := N;
	end;

	function RNG.MT19937_32Impl.GetUint32: uint32;
	const
		Mag01: array[0 .. 1] of uint32 = (0, $9908b0df);
		UpperMask = $80000000;
		LowerMask = $7fffffff;
	var
		y: uint32;
		kk: uint;
	begin
		if mti >= N then
		begin
			for kk := 0 to N - M - 1 do
			begin
				y := (mt[kk] and UpperMask) or (mt[kk+1] and LowerMask);
				mt[kk] := mt[kk+M] xor (y shr 1) xor Mag01[y and 1];
			end;

			for kk := N - M to N - 2 do
			begin
				y := (mt[kk] and UpperMask) or (mt[kk+1] and LowerMask);
				mt[kk] := mt[kk-N+M] xor (y shr 1) xor Mag01[y and 1];
			end;
			y := (mt[N-1] and UpperMask) or (mt[0] and LowerMask);
			mt[N-1] := mt[M-1] xor (y shr 1) xor Mag01[y and 1];

			mti := 0;
		end;

		y := mt[mti];
		inc(mti);

		y := y xor (y shr 11);
		y := y xor ((y shl 7) and $9d2c5680);
		y := y xor ((y shl 15) and $efc60000);
		y := y xor (y shr 18);
		result := y;
	end;
end_unchecked

	procedure RNG.MT19937_32Impl.Serialize(s: pStream);
	var
		i: sint;
	begin
		for i := 0 to High(mt) do
			Serialize_ui32(s, mt[i]);
		Serialize_ui16(s, mti - 1);
		inherited Serialize(s);
	end;

	procedure RNG.MT19937_32Impl.Deserialize(s: pStream);
	var
		i: sint;
		sane: boolean;
	begin
		sane := no;
		for i := 0 to High(mt) do
		begin
			mt[i] := Deserialize_ui32(s);
			sane := sane or (mt[i] <> 0);
		end;
		if not sane then raise Error('Нулевое состояние MT19937_32.');

		mti := 1 + Deserialize_ui16(s);
		if mti > N then raise Error('Неверный индекс в состоянии MT19937_32.');
		inherited Deserialize(s);
	end;

unchecked
	procedure RNG.MT19937_64Impl.Initialize(const seeds: string; flags: InitFlags);

		procedure Wrap(var i: uint);
		begin
			inc(i);
			if i >= NN then
			begin
				mt[0] := mt[NN-1];
				i := 1;
			end;
		end;

	var
		i, k, iseed: uint;
		seed: Uint64Seed;
	begin
		seed := RNG.SeedToUints64(seeds, 4, NN, flags);

		Setup(19650218);
		i := 1;
		iseed := 0;

		for k := 0 to NN - 1 do
		begin
			mt[i] := (mt[i] xor ((mt[i-1] xor (mt[i-1] shr 62)) * uint64(3935559000370003845))) + seed[iseed] + k;
			Wrap(i);
			inc(iseed);
			if iseed >= uint(length(seed)) then iseed := 0;
		end;

		for k := 0 to NN - 2 do
		begin
			mt[i] := (mt[i] xor ((mt[i-1] xor (mt[i-1] shr 62)) * uint64(2862933555777941757))) - i;
			Wrap(i);
		end;

		mt[0] := uint64(9223372036854775808);
	end;

	procedure RNG.MT19937_64Impl.Setup(const seed: uint64);
	var
		i: sint;
	begin
		mt[0] := seed;
		for i := 1 to NN - 1 do
			mt[mti] := (uint64(6364136223846793005) * (mt[i-1] xor (mt[i-1] shr 62)) + i);
		mti := NN;
	end;

	function RNG.MT19937_64Impl.GetUint64: uint64;
	const
		UM = uint64($FFFFFFFF80000000); // 33 msb
		LM = uint64($7FFFFFFF); // 31 lsb
		Mag01: array[0 .. 1] of uint64 = (0, uint64($B5026F5AA96619E9));
	var
		i: uint;
		x: uint64;
	begin
		if mti >= NN then
		begin
			for i := 0 to NN - MM - 1 do
			begin
				x := (mt[i] and UM) or (mt[i+1] and LM);
				mt[i] := mt[i+MM] xor (x shr 1) xor Mag01[x and 1];
			end;

			for i := NN - MM to NN - 2 do
			begin
				x := (mt[i] and UM) or (mt[i+1] and LM);
				mt[i] := mt[i-NN+MM] xor (x shr 1) xor Mag01[x and 1];
			end;
			x := (mt[NN-1] and UM) or (mt[0] and LM);
			mt[NN-1] := mt[MM-1] xor (x shr 1) xor Mag01[x and 1];

			mti := 0;
		end;

		x := mt[mti];
		inc(mti);

		x := x xor ((x shr 29) and uint64($5555555555555555));
		x := x xor ((x shl 17) and uint64($71D67FFFEDA60000));
		x := x xor ((x shl 37) and uint64($FFF7EEE000000000));
		x := x xor (x shr 43);
		result := x;
	end;
end_unchecked

	procedure RNG.MT19937_64Impl.Serialize(s: pStream);
	var
		i: sint;
	begin
		for i := 0 to High(mt) do
			Serialize_ui64(s, mt[i]);
		Serialize_ui16(s, mti - 1);
		inherited Serialize(s);
	end;

	procedure RNG.MT19937_64Impl.Deserialize(s: pStream);
	var
		i: sint;
		sane: boolean;
	begin
		sane := no;
		for i := 0 to High(mt) do
		begin
			mt[i] := Deserialize_ui64(s);
			sane := sane or (mt[i] <> 0);
		end;
		if not sane then raise Error('Нулевое состояние MT19937_64.');

		mti := 1 + Deserialize_ui16(s);
		if mti > NN then raise Error('Неверный индекс в состоянии MT19937_64.');
		inherited Deserialize(s);
	end;

	procedure RNG.TinyMT_32Impl.Initialize(const seeds: string; flags: InitFlags);
	begin
		Setup(RNG.SeedToUints32(seeds, 1, 8, flags));
	end;

	procedure RNG.TinyMT_32Impl.Serialize(s: pStream);
	var
		i: sint;
	begin
		Serialize_ui8(s, variation - pVariation(Variations));
		for i := 0 to High(st) do
			Serialize_ui32(s, st[i]);
		inherited Serialize(s);
	end;

	procedure RNG.TinyMT_32Impl.Deserialize(s: pStream);
	var
		varidx, i: sint;
	begin
		varidx := Deserialize_ui8(s);
		if varidx > High(Variations) then raise Error('Неверный индекс набора параметров TinyMT-32.');
		variation := @Variations[varidx];

		for i := 0 to High(st) do
			st[i] := Deserialize_ui32(s);
		if not Validate(no) then raise Error('Загружено неверное состояние TinyMT-32.');
		inherited Deserialize(s);
	end;

	function RNG.TinyMT_32Impl.Validate(fix: boolean): boolean;
	begin
		result := (st[0] and Mask1 <> 0) or (st[1] <> 0) or (st[2] <> 0) or (st[3] <> 0);
		if (not result) and fix then begin st := EmergencyState; Assert(Validate(no)); end;
	end;

unchecked
	procedure RNG.TinyMT_32Impl.Setup(const seed: array of uint32);
	const
		Lag = 1;
		Mid = 1;
		Size = 4;
		MinLoop = 8;
		PreLoop = 8;
	var
		i, j, count, mc: sint;
		r: uint32;
	begin
		variation := @Variations[seed[0] mod length(Variations)];
		st[0] := 0;
		st[1] := variation^.mat1;
		st[2] := variation^.mat2;
		st[3] := variation^.tmat;
		if length(seed) >= MinLoop then count := length(seed) else count := MinLoop - 1;
		if length(seed) < count then mc := length(seed) else mc := count;

		r := Ini1(st[0] xor st[mid mod size] xor st[(size - 1) mod size]);
		st[mid mod size] += r;
		r += uint(length(seed));
		st[(mid + lag) mod size] += r;
		st[0] := r;

		i := 1;
		for j := 0 to mc - 1 do
		begin
			r := Ini1(st[i mod size] xor st[(i + mid) mod size] xor st[(i + size - 1) mod size]);
			st[(i + mid) mod size] += r;
			r += seed[j] + uint(i);
			st[(i + mid + lag) mod size] += r;
			st[i mod size] := r;
			i := (i + 1) mod size;
		end;

		for j := mc to count - 1 do
		begin
			r := Ini1(st[i mod size] xor st[(i + mid) mod size] xor st[(i + size - 1) mod size]);
			st[(i + mid) mod size] += r;
			r += uint(i);
			st[(i + mid + lag) mod size] += r;
			st[i mod size] := r;
			i := (i + 1) mod size;
		end;

		for j := 0 to size - 1 do
		begin
			r := Ini2(st[i mod size] + st[(i + mid) mod size] + st[(i + size - 1) mod size]);
			st[(i + mid) mod size] := st[(i + mid) mod size] xor r;
			r -= i;
			st[(i + mid + lag) mod size] := st[(i + mid + lag) mod size] xor r;
			st[i mod size] := r;
			i := (i + 1) mod size;
		end;
		Validate(yes);
		for i := 1 to PreLoop do NextState;
	end;

	procedure RNG.TinyMT_32Impl.NextState;
	var
		x, y: uint32;
	begin
		y := st[3];
		x := (st[0] and Mask1) xor st[1] xor st[2];
		x := x xor (x shl Sh0);
		y := y xor (y shr Sh0) xor x;
		st[0] := st[1];
		st[1] := st[2];
		st[2] := x xor (y shl Sh1);
		st[3] := y;
		st[1] := st[1] xor (-sint32(y and 1) and variation^.mat1);
		st[2] := st[2] xor (-sint32(y and 1) and variation^.mat2);
	end;

	function RNG.TinyMT_32Impl.Ini1(x: uint32): uint32;
	begin
		result := (x xor (x shr 27)) * 1664525;
	end;

	function RNG.TinyMT_32Impl.Ini2(x: uint32): uint32;
	begin
		result := (x xor (x shr 27)) * 1566083941;
	end;

	function RNG.TinyMT_32Impl.GetUint32: uint32;
	var
		t0, t1: uint32;
	begin
		NextState;
		t0 := st[3];
		t1 := st[0] + (st[2] shr Sh8);
		t0 := t0 xor t1;
		t0 := t0 xor (-sint(t1 and 1) and variation^.tmat);
		result := t0;
	end;
end_unchecked

	procedure RNG.TinyMT_64Impl.Initialize(const seeds: string; flags: InitFlags);
	begin
		Setup(RNG.SeedToUints64(seeds, 1, 4, flags));
	end;

	procedure RNG.TinyMT_64Impl.Serialize(s: pStream);
	var
		i: sint;
	begin
		Serialize_ui8(s, variation - pVariation(Variations));
		for i := 0 to High(st) do
			Serialize_ui64(s, st[i]);
		inherited Serialize(s);
	end;

	procedure RNG.TinyMT_64Impl.Deserialize(s: pStream);
	var
		varidx: uint;
		i: sint;
	begin
		varidx := Deserialize_ui8(s);
		if varidx > High(Variations) then raise Error('Неверный индекс набора параметров TinyMT-64.');
		variation := @Variations[varidx];

		for i := 0 to High(st) do
			st[i] := Deserialize_ui64(s);
		if not Validate(no) then raise Error('Загружено неверное состояние TinyMT-64.');
		inherited Deserialize(s);
	end;

	function RNG.TinyMT_64Impl.Validate(fix: boolean): boolean;
	begin
		result := (st[0] and Mask1 <> 0) or (st[1] <> 0);
		if (not result) and fix then begin st := EmergencyState; Assert(Validate(no)); end;
	end;

unchecked
	procedure RNG.TinyMT_64Impl.Setup(const seed: array of uint64);
	const
		Lag = 1;
		Mid = 1;
		Size = 4;
		MinLoop = 8;
	var
		i, j, count, mc: sint;
		r: uint64;
		s: array[0 .. 3] of uint64;
	begin
		variation := @Variations[seed[0] mod length(Variations)];
		s[0] := 0;
		s[1] := variation^.mat1;
		s[2] := variation^.mat2;
		s[3] := variation^.tmat;
		if length(seed) >= MinLoop then count := length(seed) else count := MinLoop - 1;
		if length(seed) < count then mc := length(seed) else mc := count;

		r := Ini1(s[0] xor s[mid mod size] xor s[(size - 1) mod size]);
		s[mid mod size] += r;
		r += uint(length(seed));
		s[(mid + lag) mod size] += r;
		s[0] := r;

		i := 1;
		for j := 0 to mc - 1 do
		begin
			r := Ini1(s[i] xor s[(i + mid) mod size] xor s[(i + size - 1) mod size]);
			s[(i + mid) mod size] += r;
			r += seed[j] + i;
			s[(i + mid + lag) mod size] += r;
			s[i] := r;
			i := (i + 1) mod size;
		end;

		for j := mc to count - 1 do
		begin
			r := Ini1(s[i] xor s[(i + mid) mod size] xor s[(i + size - 1) mod size]);
			s[(i + mid) mod size] += r;
			r += i;
			s[(i + mid + lag) mod size] += r;
			s[i] := r;
			i := (i + 1) mod size;
		end;

		for j := 0 to size - 1 do
		begin
			r := Ini2(s[i] + s[(i + mid) mod size] + s[(i + size - 1) mod size]);
			s[(i + mid) mod size] := s[(i + mid) mod size] xor r;
			r -= uint(i);
			s[(i + mid + lag) mod size] := s[(i + mid + lag) mod size] xor r;
			s[i] := r;
			i := (i + 1) mod size;
		end;
		st[0] := s[0] xor s[1];
		st[1] := s[2] xor s[3];
	end;

	procedure RNG.TinyMT_64Impl.NextState;
	var
		x: uint64;
	begin
		st[0] := st[0] and Mask1;
		x := st[0] xor st[1];
		x := x xor (x shl Sh0);
		x := x xor (x shr 32);
		x := x xor (x shl 32);
		x := x xor (x shl Sh1);
		st[0] := st[1];
		st[1] := x;
		st[0] := st[0] xor (-sint64(x and 1) and variation^.mat1);
		st[1] := st[1] xor (-sint64(x and 1) and (uint64(variation^.mat2) shl 32));
	end;

	function RNG.TinyMT_64Impl.Ini1(const x: uint64): uint64;
	begin
		result := (x xor (x shr 59)) * uint64(2173292883993);
	end;

	function RNG.TinyMT_64Impl.Ini2(const x: uint64): uint64;
	begin
		result := (x xor (x shr 59)) * uint64(58885565329898161);
	end;

	function RNG.TinyMT_64Impl.GetUint64: uint64;
	var
		x: uint64;
	begin
		NextState;
		x := st[0] + st[1];
		x := x xor (st[0] shr Sh8);
		x := x xor (-sint64(x and 1) and variation^.tmat);
		result := x;
	end;
end_unchecked

	procedure RNG.CrawlImpl.Initialize(const seeds: string; flags: InitFlags);
	var
		seed: Uint32Seed;
	begin
		seed := RNG.SeedToUints32(seeds, 5, 5, flags);
		lcg := seed[0];
		mwcm := seed[1];
		mwcc := seed[2];
		xorshift := seed[3];
		lfsr := seed[4];

		if lfsr = 0 then lfsr := 1;
	end;

unchecked
	function RNG.CrawlImpl.GetUint32: uint32;
	var
		t: uint64;
	begin
		lcg := 314527869 * lcg + 1234567;
		lfsr := (lfsr shr 1) xor (-sint32(lfsr and 1) and $D0000001);

		if lfsr and 1 <> 0 then
		begin
				xorshift := xorshift xor (xorshift shl 5);
				xorshift := xorshift xor (xorshift shr 7);
				xorshift := xorshift xor (xorshift shl 22);
		end else
		begin
			t := uint64(4294584393) * mwcm + mwcc;
			mwcc := t shr 32;
			mwcm := t;
		end;

		result := lcg + mwcm + xorshift;
	end;
end_unchecked

	procedure RNG.CrawlImpl.Serialize(s: pStream);
	begin
		Serialize_ui32(s, lcg);
		Serialize_ui32(s, mwcm);
		Serialize_ui32(s, mwcc);
		Serialize_ui32(s, xorshift);
		Serialize_ui32(s, lfsr);
		inherited Serialize(s);
	end;

	procedure RNG.CrawlImpl.Deserialize(s: pStream);
	begin
		lcg := Deserialize_ui32(s);
		mwcm := Deserialize_ui32(s);
		mwcc := Deserialize_ui32(s);
		xorshift := Deserialize_ui32(s);
		lfsr := Deserialize_ui32(s);
		if lfsr = 0 then raise Error('Нулевой lfsr в кравло-ГПСЧ (ASG+KISS).');
		inherited Deserialize(s);
	end;

	function RNG.CryptographicImpl.GetUint32: uint32;
	begin
		result := Crypt.Random32;
	end;

	function RNG.CryptographicImpl.GetUint64: uint64;
	begin
		result := Crypt.Random64;
	end;

{$ifdef TestRNG}
	procedure RNG.TestImpl.Initialize(const seeds: string; flags: InitFlags);
	begin
		ret := RNG.SeedToUints64(seeds, 1, 1, flags)[0];
	end;

	function RNG.TestImpl.GetUint32: uint32;
	begin
		result := ret and High(result);
	end;

	function RNG.TestImpl.GetUint64: uint64;
	begin
		result := ret;
	end;
{$endif}

{$ifdef use_serialization}
	procedure SerializeRNG(se: pSerializer; obj: pointer);
	var
		rng: pRNG absolute obj;
	begin
		rng^.Serialize(se^.stream);
	end;

	procedure DeserializeRNG(de: pDeserializer; obj: pointer);
	var
		rng: pRNG absolute obj;
	begin
		System.Initialize(rng^);
		Random.RNG.Deserialize(rng^, de^.stream);
	end;
{$endif}

	procedure Init;
	begin
		GlobalRNG.Init(Good, [NeedLock, DontUseGlobalRNG]); GlobalRNG.MakeStatic;
	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Random number generator', TypeOf(RNG), nil, sizeof(RNG), yes, @SerializeRNG, @DeserializeRNG, nil, nil)
		^.AddEnv(@GlobalRNG);
	{$endif}
	end;

	procedure Done;
	begin
		GlobalRNG.Done;
	end;

initialization
	&Unit('Random').Initialize(@Init, @Done);
end.
