unit PerlinNoise;

{$include opts.inc}

interface

uses
	USystem, UMath, Random;

type
	pPerlinNoiseGenerator = ^PerlinNoiseGenerator;
	PerlinNoiseGenerator = object
	private const
		PN_B = $100;
		PN_BM = $FF;
		PN_N = $1000;
	private
		p: array[0 .. 2 * PN_B + 1] of sint;
		g3: array[0 .. 2 * PN_B + 1] of Vec3;
		g2: array[0 .. 2 * PN_B + 1] of Vec2;
		g1: array[0 .. 2 * PN_B + 1] of float;
		procedure _setup(const i: float; out b0, b1: sint; out r0, r1: float);
	public
		procedure Init(const seed: string = '');
		procedure Done;
		function Noise1(const x: float): float;
		function Noise2(const x, y: float): float;
		function Noise3(const x, y, z: float): float;
		function Turbulence2(const x, y, freq: float): float;
		function Turbulence3(const x, y, z, freq: float): float;
		function TileNoise1(const x, w: float): float;
		function TileNoise2(const x, y, w, h: float): float;
		function TileNoise3(const x, y, z, w, h, d: float): float;
		function ZTileNoise3(const x, y, z, d: float): float;
		function TileTurbulence2(const x, y, w, h, freq: float): float;
		function TileTurbulence3(const x, y, z, w, h, d, freq: float): float;
		function ZTileTurbulence3(const x, y, z, d, freq: float): float;
	private type
		Turbulence = object
			sum, w, k, wsum: float;
			procedure Start;
			function Stop: float;
			function Feed(const value: float; const target: float): boolean;
		end;
	end;

implementation

const
	FIX1 = 1;
	FIX2 = 2;
	FIX3 = 3;

	procedure PerlinNoiseGenerator._setup(const i: float; out b0, b1: sint; out r0, r1: float);
	var
		t: float;
	begin
		t := i + PN_N;
		b0 := trunc(t) and PN_BM;
		b1 := (b0 + 1) and PN_BM;
		r0 := frac(t);
		r1 := r0 - 1.0;
	end;

	function PerlinNoiseGenerator.Noise1(const x: float): float;
	var
		bx0, bx1: sint;
		rx0, rx1, sx, u, v: float;
	begin
		_setup(x, bx0, bx1, rx0, rx1);
		sx := smoothstep(rx0);
		u := rx0 * g1[p[bx0]];
		v := rx1 * g1[p[bx1]];
		result := FIX1 * lerp(u, v, sx);
	end;

	function PerlinNoiseGenerator.Noise2(const x, y: float): float;
	var
		bx0, bx1, by0, by1, b00, b10, b01, b11: sint;
		rx0, rx1, ry0, ry1, sx, sy, a, b, u, v: float;
	begin
		_setup(x, bx0, bx1, rx0, rx1);
		_setup(y, by0, by1, ry0, ry1);

		b00 := p[p[bx0] + by0];
		b10 := p[p[bx1] + by0];
		b01 := p[p[bx0] + by1];
		b11 := p[p[bx1] + by1];

		sx := smoothstep(rx0);
		sy := smoothstep(ry0);

		u := rx0 * g2[b00].data[0] + ry0 * g2[b00].data[1];
		v := rx1 * g2[b10].data[0] + ry0 * g2[b10].data[1];
		a := lerp(u, v, sx);

		u := rx0 * g2[b01].data[0] + ry1 * g2[b01].data[1];
		v := rx1 * g2[b11].data[0] + ry1 * g2[b11].data[1];
		b := lerp(u, v, sx);

		result := FIX2 * lerp(a, b, sy);
	end;

	function PerlinNoiseGenerator.Noise3(const x, y, z: float): float;
	var
		bx0, bx1, by0, by1, bz0, bz1, b00, b10, b01, b11: sint;
		rx0, rx1, ry0, ry1, rz0, rz1, sy, sz, a, b, c, d, u, v, t: float;
		i, j: sint;
	begin
		_setup(x, bx0, bx1, rx0, rx1);
		_setup(y, by0, by1, ry0, ry1);
		_setup(z, bz0, bz1, rz0, rz1);

		i := p[bx0];
		j := p[bx1];

		b00 := p[i + by0];
		b10 := p[j + by0];
		b01 := p[i + by1];
		b11 := p[j + by1];

		t  := smoothstep(rx0);
		sy := smoothstep(ry0);
		sz := smoothstep(rz0);

		u := g3[b00 + bz0] ** Vec3.Make(rx0, ry0, rz0);
		v := g3[b10 + bz0] ** Vec3.Make(rx1, ry0, rz0);
		a := lerp(u, v, t);

		u := g3[b01 + bz0] ** Vec3.Make(rx0, ry1, rz0);
		v := g3[b11 + bz0] ** Vec3.Make(rx1, ry1, rz0);
		b := lerp(u, v, t);

		c := lerp(a, b, sy);

		u := g3[b00 + bz1] ** Vec3.Make(rx0, ry0, rz1);
		v := g3[b10 + bz1] ** Vec3.Make(rx1, ry0, rz1);
		a := lerp(u, v, t);

		u := g3[b01 + bz1] ** Vec3.Make(rx0, ry1, rz1);
		v := g3[b11 + bz1] ** Vec3.Make(rx1, ry1, rz1);
		b := lerp(u, v, t);

		d := lerp(a, b, sy);

		result := FIX3 * lerp(c, d, sz);
	end;

	function PerlinNoiseGenerator.Turbulence2(const x, y, freq: float): float;
	var
		t: Turbulence;
	begin
		t.Start;
		while not t.Feed(noise2(t.w * x, t.w * y), freq) do;
		result := t.Stop;
	end;

	function PerlinNoiseGenerator.Turbulence3(const x, y, z, freq: float): float;
	var
		t: Turbulence;
	begin
		t.Start;
		while not t.Feed(noise3(t.w * x, t.w * y, t.w * z), freq) do;
		result := t.Stop;
	end;

	function PerlinNoiseGenerator.TileNoise1(const x, w: float): float;
	begin
		result := (Noise1(x) * (w-x) + Noise1(x-w) * x) / w;
	end;

	function PerlinNoiseGenerator.TileNoise2(const x, y, w, h: float): float;
	begin
		result := (
				noise2(x,   y  ) * (w-x) * (h-y) +
				noise2(x-w, y  ) *    x  * (h-y) +
				noise2(x,   y-h) * (w-x) *    y  +
				noise2(x-w, y-h) *    x  *    y) / (w * h);
	end;

	function PerlinNoiseGenerator.TileNoise3(const x, y, z, w, h, d: float): float;
	begin
		result := (
				noise3(x,   y,   z  ) * (w-x) * (h-y) * (d-z) +
				noise3(x-w, y,   z  ) *    x  * (h-y) * (d-z) +
				noise3(x,   y-h, z  ) * (w-x) *    y  * (d-z) +
				noise3(x-w, y-h, z  ) *    x  *    y  * (d-z) +
				noise3(x,   y,   z-d) * (w-x) * (h-y) *    z  +
				noise3(x-w, y,   z-d) *    x  * (h-y) *    z  +
				noise3(x,   y-h, z-d) * (w-x) *    y  *    z  +
				noise3(x-w, y-h, z-d) *    x  *    y  *    z) / (w * h * d);
	end;

	function PerlinNoiseGenerator.ZTileNoise3(const x, y, z, d: float): float;
	begin
		result := (noise3(x, y, z) * (d-z) + noise3(x, y, z-d) * z) / d;
	end;

	function PerlinNoiseGenerator.TileTurbulence2(const x, y, w, h, freq: float): float;
	var
		t: Turbulence;
	begin
		t.Start;
		while not t.Feed(TileNoise2(t.w * x, t.w * y, w * t.w, h * t.w), freq) do;
		result := t.Stop;
	end;

	function PerlinNoiseGenerator.TileTurbulence3(const x, y, z, w, h, d, freq: float): float;
	var
		t: Turbulence;
	begin
		t.Start;
		while not t.Feed(TileNoise3(t.w * x, t.w * y, t.w * z, w * t.w, h * t.w, d * t.w), freq) do;
		result := t.Stop;
	end;

	function PerlinNoiseGenerator.ZTileTurbulence3(const x, y, z, d, freq: float): float;
	var
		t: Turbulence;
	begin
		t.Start;
		while not t.Feed(ZTileNoise3(t.w * x, t.w * y, t.w * z, d * t.w), freq) do;
		result := t.Stop;
	end;

	procedure PerlinNoiseGenerator.Init(const seed: string = '');
	var
		i: sint;
		rng: Random.RNG;
	begin
		if seed = '' then rng.Init(Good) else rng.Init(Crawl, seed);
		for i := 0 to PN_B - 1 do
		begin
			p[i] := i;
			g1[i] := rng.GetFloat(-1, 1);
			g2[i] := rng.Direction2;
			g3[i] := rng.Direction3;
		end;

		for i := PN_B - 1 downto 0 do
			Swap(p[i], p[rng.GetUint(PN_B)]);

		for i := 0 to PN_B + 1 do
		begin
			p[PN_B + i]  := p[i];
			g1[PN_B + i] := g1[i];
			g2[PN_B + i] := g2[i];
			g3[PN_B + i] := g3[i];
		end;
		rng.Done;
	end;

	procedure PerlinNoiseGenerator.Done;
	begin
	end;

	procedure PerlinNoiseGenerator.Turbulence.Start;
	begin
		sum  := 0.0;
		w    := 1.0;
		k    := 1.0;
		wsum := 0.0;
	end;

	function PerlinNoiseGenerator.Turbulence.Stop: float;
	begin
		result := sum / wsum;
	end;

	function PerlinNoiseGenerator.Turbulence.Feed(const value: float; const target: float): boolean;
	var
		coef: float;
	begin
		result := w >= target;
		// Пусть target = 9. Веса, применяемые по-обычному: 1, 2, 4, 8.
		// Вес 16 — последний — применяется частично, его вклад домножается на (9-8) / (16-8) = 9/8 - 1.0, где 8 — предыдущий вес.

		coef := k;
		if w > target then begin coef *= target * (2.0 * k) - 1.0; Assert(coef < k); end;
		sum += value * coef;
		wsum += coef;
		w *= 2;
		k *= 0.5;
	end;

end.
