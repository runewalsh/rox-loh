unit UMath;

{$include opts.inc}
{$ifdef cpui386} {$define use_asm} {$define asm386} {$endif}

{$include all_numbers.inc}
{$include all_vectors.inc}

interface

uses
	USystem {$ifdef Debug}, Debug {$endif};

const
	QuatEps  = sqrt(CloseToZeroEps);
	Pi       = System.pi();
	TwoPi    = 2 * Pi;
	HalfPi   = Pi / 2;
	Deg2Rad = Pi / 180;
	Rad2Deg = 180 / Pi;

{$define numberf :=
	function Min(const a, b: typ): typ; overload; cinline
	function Min(const a, b, c: typ): typ; overload;
	function Max(const a, b: typ): typ; overload; cinline
	function Max(const a, b, c: typ): typ; overload;
	procedure Swap(var a, b: typ); overload; cinline
	function Clamp(const x, a, b: typ): typ; overload; cinline
	function InRange(const x, a, b: typ): boolean; cinline
{$ifndef unsigned}
	function IntSign(const x: typ): sint; cinline
	function FloatSign(const x: typ): {$ifdef float} typ {$else} float {$endif}; cinline
	function FloatSign(const x, m: typ): {$ifdef float} typ {$else} float {$endif}; cinline
{$endif}} all_numbers

	function SmoothStep(const start, end_, x: float): float; overload; cinline
	function SmoothStep(const x: float): float; overload; cinline

	function tan(const x: float): float; {$ifdef use_asm} assembler; {$else} cinline {$endif}
	function ArcSin(const x: float): float; cinline
	function ArcCos(const x: float): float; cinline
	function ArcTan2(const y, x: float): float; {$ifdef use_asm} assembler; {$else} cinline {$endif}
	function AngleDiff(const a, b: float): float; cinline
	function NormalizeAngle(const a: float): float; cinline
	function AngleNormalized(const a: float): boolean;
	function pow(const base, exponent: float): float; cinline
	function pow(base, exponent: uint): uint;
	function signedPow(const base, exponent: float): float; cinline
{$define floatf :=
	function modf(const x, d: typ): typ;
	function closer(const x, t, d: typ): typ;} all_floats
	function logn(const base, x: float): float; cinline
	function floor(const x: float): float; cinline
	function ifloor(const x: float): sint; cinline
	function ceil(const x: float): float; cinline
	function iceil(const x: float): sint; cinline
	function remap(const x: float; const a1, b1, a2, b2: float): float;
	function remapc(const x: float; const a1, b1, a2, b2: float): float;

	function IsZero(const x: float): boolean; cinline
	function IsZero(const x: float; const eps: float): boolean; cinline
	function NotZero(const x: float): boolean; cinline
	function Equals(const a, b: float): boolean; cinline
	function Equals(const a, b: float; const eps: float): boolean; cinline
	function GreaterThan(const a, b: float): boolean; cinline
	function GreaterThan(const a, b: float; const eps: float): boolean; cinline
	function LessThan(const a, b: float): boolean; cinline
	function LessThan(const a, b: float; const eps: float): boolean; cinline
	function GreaterThanEqual(const a, b: float): boolean; cinline
	function GreaterThanEqual(const a, b: float; const eps: float): boolean; cinline
	function LessThanEqual(const a, b: float): boolean; cinline
	function LessThanEqual(const a, b: float; const eps: float): boolean; cinline

	function RangeIntersects(const l1, r1, l2, r2: float): boolean;
	function RangeIntersectsPrecise(const l1, r1, l2, r2: float): boolean;
	function RangeStrictlyIntersects(const l1, r1, l2, r2: float): boolean;
	function RangeStrictlyIntersectsPrecise(const l1, r1, l2, r2: float): boolean;
	function RangeIntersection(const l1, r1, l2, r2: float): float;
{$define intf := function RangeIntersects(const l1, r1, l2, r2: typ): boolean;} all_ints

{$define intf :=
	function IsPowerOf2(x: typ): boolean; cinline
	function UpToPow2(x: typ): typ; cinline
	function Log2(x: typ): uint; cinline
	function CeilLog2(x: typ): uint; cinline} all_uints
	function GCD(a, b: uint): uint;
	function LCM(a, b: uint): uint;

{$define vecf :=
type
	pvec = ^vec;
	vec = object
	type
		LinearData = array[0 .. veclen - 1] of base_type;
	var
		data: LinearData;
		function Make(const x: base_type): vec; static;
		function Make(const {$define one := item} comma_separated: base_type): vec; static;
	{$if veclen = 3} function Make(const xy: pair2; const z: base_type): vec; static; {$endif}
	{$if veclen = 4}
		function Make(const v: pair3; const w: base_type): vec; static;
		function Make(const a, b: pair2): vec; static;
	{$endif}
	{$define iterate := property item: base_type read data[itemid] write data[itemid];} foreach_component

{$ifdef floating}
	private
		function GetLength: base_type;
		procedure SetLength(newLength: base_type);
	public
		property Length: base_type read GetLength write SetLength;

		function IsZero: boolean;
		function NonZero: boolean;
		function IsIdentity: boolean;
		function SqrLength: base_type;
		function Normalized: vec;
		function MaybeNormalized: vec;
		function Normalized(out len: base_type): vec;
		function Clamped(const lim: base_type): vec;

	{$if veclen = 3} function LineEquation(const a, b: pair2): vec; {$endif}
	{$if veclen = 4} function Homo3: pair3; {$endif}
{$endif}

		function Sum: base_type;
		function Product: base_type;
	{$if veclen = 2}
		function Aspect: {$ifdef floating} base_type {$else} float {$endif};
	{$ifdef unsigned}
		function FitsClosed(const R: vec): boolean;
		function FitsOpen(const R: vec): boolean;
	{$endif}
	{$endif}

	{$ifdef integer}
		function Square: base_type_unsigned;
		function Positive: boolean;
	{$endif}

	{$if veclen = 2} function yx: pair2; {$endif}
	{$if veclen = 3} function xy: pair2; {$endif}
	{$if veclen = 4} function xyz: pair3; {$endif}

	const
		Zero: vec = (data: {$define one := 0} (comma_separated));
		Ones: vec = (data: {$define one := 1} (comma_separated));
	{$ifdef signed} MinusOnes: vec = (data: ({$define one := -1} comma_separated)); {$endif}

	{$push} {$warnings off} // nested comments
		PositiveX: vec = (data: {$define one := {$if itemid = 0} 1 {$else} 0 {$endif}} (comma_separated));
		PositiveY: vec = (data: {$define one := {$if itemid = 1} 1 {$else} 0 {$endif}} (comma_separated));
	{$if veclen >= 3} PositiveZ: vec = (data: {$define one := {$if itemid = 2} 1 {$else} 0 {$endif}} (comma_separated)); {$endif}

	{$ifdef signed}
		NegativeX: vec = (data: {$define one := {$if itemid = 0} -1 {$else} 0 {$endif}} (comma_separated));
		NegativeY: vec = (data: {$define one := {$if itemid = 1} -1 {$else} 0 {$endif}} (comma_separated));
	{$if veclen >= 3} NegativeZ: vec = (data: {$define one := {$if itemid = 2} -1 {$else} 0 {$endif}} (comma_separated)); {$endif}
	{$endif}
	{$pop}
	end;

	operator +(const a, b: vec): vec; cinline
	operator -(const a, b: vec): vec; cinline
{$ifdef signed} operator -(const v: vec): vec; cinline {$endif}
	operator *(const a, b: vec): vec; cinline
	operator *(const v: vec; const k: base_type): vec; cinline
	operator *(const k: base_type; const v: vec): vec; cinline
	operator =(const a, b: vec): boolean; cinline
	function Min(const a, b: vec): vec; cinline
	function Max(const a, b: vec): vec; cinline} all_vectors

{$define vecf :=
	operator /(const v: vec; const x: base_type): vec; cinline
	operator /(const a, b: vec): vec; cinline
	operator /(const x: base_type; const v: vec): vec;
	operator **(const a, b: vec): base_type; cinline

	function Equals(const a, b: vec; const eps: base_type = CloseToZeroEps): boolean; cinline
	function Clamp(const v, a, b: vec): vec; cinline
	function SqrDistance(const a, b: vec): base_type; cinline
	function Distance(const a, b: vec): base_type; cinline
	function NormalizeAngle(const a: vec): vec;
	function AngleDiff(const a, b: vec): vec;

{$if veclen = 2}
	function Angle(const a, b: vec): base_type;
{$endif}

{$if veclen = 3}
	operator ><(const a, b: vec): vec;
	function AngleUN(const a, b: vec): base_type;
	function AngleN(const a, b: vec): base_type;
	function Reflect(const I, N: vec): vec; cinline
{$endif veclen = 3}

	function UintTrunc(const v: vec): uint_vec;
	function UintRound(const v: vec): uint_vec;
	function IntTrunc(const v: vec): int_vec;
	function IntRound(const v: vec): int_vec;} all_floating_vectors

{$define vecf :=
	operator div(const a, b: vec): vec;
	operator div(const v: vec; const d: base_type): vec;
{$ifdef unsigned}
	operator mod(const a, b: vec): vec;
	operator mod(const v: vec; const d: base_type): vec;
	operator shl(const v: vec; bits: uint): vec;
	operator shr(const v: vec; bits: uint): vec;
{$endif}

{$if not (defined(unsigned) and not defined(signed_vec))}
	operator :=(const v: vec): {$ifdef signed} unsigned_vec {$else} signed_vec {$endif};
	operator :=(const v: vec): float_vec;
{$if defined(signed) and defined(integer)}
	operator +(const v: vec; const d: uint_vec): vec;
	operator -(const v: vec; const d: unsigned_vec): vec;
{$endif}
{$ifdef unsigned} operator +(const v: vec; const d: int_vec): vec; {$endif}
{$endif}} all_integer_vectors

	function Rotate(const v: Vec2; const angle: float): Vec2;
	function ShrinkToAspect(const size, aspect: UintVec2): UintVec2;

type
	pMatrix4 = ^Matrix4;
	Matrix4 = object
	type
		MatrixData = array[0 .. 3, 0 .. 3] of float;
		LinearData = array[0 .. 15] of float;
		DataUnion = record
		case byte of
			0: (l: LinearData);
			1: (m: MatrixData);
		end;
	var
		data: DataUnion;
		function Transposed: Matrix4;
		function Determinant: float;
		function Minor(x, y: sint): float;
		function Inversed: Matrix4;
		function Inversed(const det: float): Matrix4;

		function Rows(const x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, xa, xb, xc, xd, xe, xf: float): Matrix4; static; cinline
		function Translation(const v: Vec3): Matrix4; static;
		function Rotation(const ane: float; const v: Vec3): Matrix4; static;
		function Scaling(const kx, ky, kz: float): Matrix4; static;
		function FrustumProjection(const left, right, bottom, top, near, far: float): Matrix4; static;
		function PerspectiveProjection(const fovy, aspect, near, far: float): Matrix4; static;
		function OrthographicProjection(const l, r, b, t, n, f: float): Matrix4; static;

		property m: MatrixData read data.m write data.m;
		property l: LinearData read data.l write data.l;
	const
		Zero: Matrix4 = (data: (l: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)));
		Identity: Matrix4 = (data: (l: (1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)));
	end;

	pMatrix3 = ^Matrix3;
	Matrix3 = object
	type
		LinearData = array[0 .. 8] of float;
		MatrixData = array[0 .. 2, 0 .. 2] of float;
		DataUnion = record
		case byte of
			0: (l: LinearData);
			1: (m: MatrixData);
		end;
	var
		data: DataUnion;
	public
		function Determinant: float;
		function Inversed: Matrix3;
		function Inversed(const det: float): Matrix3;
		function Transposed: Matrix3;

		function Rows(const x0, x1, x2, x3, x4, x5, x6, x7, x8: float): Matrix3; static; cinline
		function Columns(const a, b, c: Vec3): Matrix3; static; cinline
		function RotationN(const ane: float; const v: Vec3): Matrix3;

		property m: MatrixData read data.m write data.m;
		property l: LinearData read data.l write data.l;
	const
		Zero: Matrix3 = (data: (l: (0, 0, 0, 0, 0, 0, 0, 0, 0)));
		Identity: Matrix3 = (data: (l: (1, 0, 0, 0, 1, 0, 0, 0, 1)));
	end;

	function Mat3(const m4: Matrix4): Matrix3; cinline
	function Mat4(const m3: Matrix3): Matrix4; cinline
	operator *(const mat: Matrix4; const vec: Vec4): Vec4; cinline
	operator *(const a, b: Matrix4): Matrix4; cinline
	operator *(const a, b: Matrix3): Matrix3; cinline
	operator *(const mat: Matrix3; const vec: Vec3): Vec3; cinline
	operator *(const vec: Vec3; const mat: Matrix3): Vec3; cinline

type
	pQuaternion = ^Quaternion;
	Quaternion = object
	type
		DataUnion = record
		case byte of
			0: (v: array[0 .. 3] of float);
			1: (v4: Vec4);
			2: (v3: Vec3; w: float);
		end;
	var
		data: DataUnion;
		function Make(const x, y, z, w: float): Quaternion; static;
		function Make(const v: Vec3; const w: float): Quaternion; static;
		function FromMatrix(const matrix: Matrix3): Quaternion; static;
		function Rotation(const angle: float; const axis: Vec3): Quaternion; static;
		function Rotation(const from, to_: Vec3): Quaternion; static;
		function RotationThroughX0Z(const from, to_: Vec3): Quaternion; static;
		function FromEuler(const yaw, pitch, roll: float): Quaternion; static;
		function FromEuler(const eul: Vec3): Quaternion; static;

		function IsUnit: boolean;
		function Length: float;
		function Norm: float;
		function Normalized: Quaternion;
		function MaybeNormalized: Quaternion;
		function Inversed: Quaternion;
		function ToMatrix3: Matrix3;
		function ToMatrix: Matrix4;
		procedure GetRotation(out angle: float; out axis: Vec3);
		function GetAngle: float;
		function GetAxis: Vec3;
		function ToEuler: Vec3;
		function GetAngleXZ: float;
		function Transformed100: Vec3;

		property v4: Vec4 read data.v4 write data.v4;
		property v3: Vec3 read data.v3 write data.v3;
		property x: float read data.v[0] write data.v[0];
		property y: float read data.v[1] write data.v[1];
		property z: float read data.v[2] write data.v[2];
		property w: float read data.v[3] write data.v[3];
	const
		Identity: Quaternion = (data: (v: (0, 0, 0, 1)));
	end;
	operator =(const a, b: Quaternion): boolean; cinline
	operator *(const q1, q2: Quaternion): Quaternion;
	operator *(const q: Quaternion; const x: float): Quaternion;
	operator *(const x: float; const q: Quaternion): Quaternion;
	operator *(const q: Quaternion; const v: Vec3): Vec3;
	operator +(const q1, q2: Quaternion): Quaternion;
	operator -(const q1, q2: Quaternion): Quaternion;
	operator -(const q: Quaternion): Quaternion;
	operator /(const q: Quaternion; const x: float): Quaternion;
	operator **(const a, b: Quaternion): float;
	function AngleUN(const a, b: Quaternion): float;
	function AngleN(const a, b: Quaternion): float;
	function SameRotation(const p, q: Quaternion; const eps: float = QuatEps): boolean; cinline
	function LimitByCone(const v: Vec3; const coneAxis: Vec3; const coneHalfAngle: float): Vec3;
	function AlignToGravity(const axis, up, gravity: Vec3): Quaternion;

type
	pTransform = ^Transform;
	Transform = object
		tr: Vec3;
		rot: Quaternion;
		scale: float;
		function FromMatrix(const mat: Matrix4): Transform; static;
		function ToMatrix: Matrix4;
		function ToMatrixWoScale: Matrix4;
		function Inversed: Transform;
	const
		Identity: Transform =
		(
			tr: (data: (0, 0, 0));
			rot: (data: (v: (0, 0, 0, 1)));
			scale: 1;
		);
	end;
	function Translate(const tr: Vec3): Transform;
	function Translate(const x, y, z: float): Transform;
	function Rotate(const rot: Quaternion): Transform;
	function Rotate(const angle: float; const axis: Vec3): Transform;
	function TranslateRotate(const tr: Vec3; const rot: Quaternion): Transform;
	function RotateTranslate(const rot: Quaternion; const tr: Vec3): Transform;
	function Scale(const factor: float): Transform;

	function Equals(const a, b: Transform; const eps: float): boolean;
	operator =(const a, b: Transform): boolean;
	operator *(const a, b: Transform): Transform;
	operator *(const t: Transform; const v: Vec3): Vec3;

type
	Transform2 = object
		trans: Vec2;
		rot, scale: float;
	const
		Identity: Transform2 = (trans: (data: (0, 0)); rot: 0; scale: 1);
	end;
	operator =(const a, b: Transform2): boolean;
	operator *(const a, b: Transform2): Transform2;
	operator *(const t: Transform2; const v: Vec2): Vec2;

	function Translate2(const trans: Vec2): Transform2;
	function Translate2(const x, y: float): Transform2;
	function Rotate2(const rot: float): Transform2;
	function Scale2(const scale: float): Transform2;
	function TranslateRotate2(const trans: Vec2; const rot: float): Transform2;
	function RotateTranslate2(const rot: float; const trans: Vec2): Transform2;

type
	pPlane = ^Plane;
	Plane = object
	type
		DataUnion = record
		case byte of
			0: (coefs: Vec4.LinearData);
			1: (v3: Vec3);
			2: (v4: Vec4);
		end;
	var
		data: DataUnion;
		function Make(const p1, p2, p3: Vec3): Plane; static;
		function Make(const point, normal: Vec3): Plane; static;
		function Make(const a, b, c, d: float): Plane; static;
		function Make(const coefs: Vec4): Plane; static;

		function Normalized: Plane;
		function ReflectPoint(const point: Vec3): Vec3;
		function ReflectVector(const vec: Vec3): Vec3;
		function Reflect(const q: Quaternion): Quaternion;
		function Reflect(const p: Plane): Plane;
		function SignedDistance(const pt: Vec3): float;
		function SignedDistance(const x, y, z: float): float;
		function Distance(const pt: Vec3): float;
		function Project(const p: Vec3): Vec3;
		function AnyPoint: Vec3;
		function Reversed: Plane;
		function ToBasis(const o: Vec3; const basis: Matrix3): Plane;
		function IntersectLine(const p, n: Vec3): Vec3;

		property a: float read data.coefs[0] write data.coefs[0];
		property b: float read data.coefs[1] write data.coefs[1];
		property c: float read data.coefs[2] write data.coefs[2];
		property d: float read data.coefs[3] write data.coefs[3];
		property v3: Vec3 read data.v3 write data.v3;
		property normal: Vec3 read data.v3;
		property v4: Vec4 read data.v4 write data.v4;
	const
		None: Plane = (data: (coefs: (0, 0, 0, 1)));
	end;
	operator *(const t: Transform; const p: Plane): Plane;

type
	BoundingRelation = (bb_FullyIn, bb_FullyOut, bb_Intersects);

	pSphere = ^Sphere;
	Sphere = object
		center: Vec3;
		radius: float;
		function Make(const newCenter: Vec3; const newRadius: float): Sphere; static;

		function Intersects(const a, b: Sphere): boolean; static;
		function Bound(const spheres: array of Sphere): Sphere; static;
		function Contains(const v: Vec3): boolean;
		function Enlarge(const pt: Vec3): boolean;
		function Enlarge(const c2: Vec3; const r2: float): boolean;
	end;

type
	pAABB = ^AABB;
	AABB = object
		A, B: Vec3;
		function Make(const p1, p2: Vec3): AABB; static;
		function Make(const pt: Vec3): AABB; static;
		function Make(const sph: Sphere): AABB; static;

		function Undefined: AABB; static;
		function Intersects(const b1, b2: AABB): boolean; static;
		function Intersection(const b1, b2: AABB): AABB; static;
		function Bound(const p: array of Vec3): AABB; static;
		function Bound(const spheres: array of Sphere): AABB; static;

		function IsUndefined: boolean;
		function Enlarge(const pt: Vec3): boolean;
		procedure Enlarge(const p: array of Vec3);
		function Enlarge(const c: Vec3; const radius: float): boolean;
		function Enlarge(const bb: AABB): boolean;
		procedure Grow(const factor, bias: float);
		function Contains(const pt: Vec3): boolean;
		procedure ClampToSphere(const sph: Sphere);
	type
		EightPoints = array[0 .. 7] of Vec3;
	var
		function GetEightPoints: EightPoints;
		function SizeX: float;
		function SizeY: float;
		function SizeZ: float;
		function Size(dim: sint): float;
		function Sizes: Vec3;
		function Volume: float;
		function Center: Vec3;
		function SupportVertex(const d: Vec3): Vec3;
	const
		Zero: AABB = (a: (data: (0, 0, 0)); b: (data: (0, 0, 0)));
	end;
	operator *(const t: Transform; const bb: AABB): AABB;

type
	pFrustum = ^Frustum;
	Frustum = object
	type
		PlaneEnum = (Right, Left, Top, Bottom, Near, Far);
		PlanesArray = array[PlaneEnum] of Plane;
		EightPoints = AABB.EightPoints;
	var
		planes: PlanesArray;
		function FromMatrix(const mvp: Matrix4): Frustum; static;
		function TestPoint(const point: Vec3): boolean;
		function TestSphere(const center: Vec3; const radius: float): boolean;
		function TestSphere(const sph: Sphere): boolean;
		function ClassifySphere(const sph: Sphere): BoundingRelation;
		function TestAABB(const bb: AABB): boolean;
		function GetEightPoints: EightPoints;

		property RightPlane : Plane read planes[Right ] write planes[Right ];
		property LeftPlane  : Plane read planes[Left  ] write planes[Left  ];
		property TopPlane   : Plane read planes[Top   ] write planes[Top   ];
		property BottomPlane: Plane read planes[Bottom] write planes[Bottom];
		property NearPlane  : Plane read planes[Near  ] write planes[Near  ];
		property FarPlane   : Plane read planes[Far   ] write planes[Far   ];
	end;

	pRect = ^Rect;
	Rect = object
		A, B: Vec2;
		function Make(const x1, y1, x2, y2: float): Rect; static;
		function Make(const a, b: Vec2): Rect; static;
		function Make(const v: Vec4): Rect; static;

		function SizeX: float;
		function SizeY: float;
		function Size(dim: uint): float;
		function Size: Vec2;
		function Aspect: float;
		function Square: float;
		procedure Enlarge(const r: Rect);
		function Contains(const pt: Vec2): boolean;
		function Corporeal: boolean;
		function Intersection(const second: Rect): Rect;
		function AsVec4: Vec4;

	type
		// В FPC 3.0 нельзя встраивать внешний тип во внутренний, но такой обходной манёвр работает.
		// Однако внимание, будет тот же глюк, что со старыми внутренними: чтобы sizeof вернула корректный размер, объявление должно быть в конце,
		// после всех полей внешнего.
		Subdivision = object
			n: sint;
			function GetRect(id: uint): Rect;
			property Rects[id: uint]: Rect read GetRect; default;
		private
			mem: array[0 .. 8 * sizeof(Rect) {$ifdef Debug} + 1 {$endif} - 1] of byte;
			procedure TryAdd(const ax, ay, bx, by: float);
		end;
		function Subdivide(const divisor: Rect): Subdivision;
	const
		OrthoIdentity: Rect = (A: (data: (-1, -1)); B: (data: (1, 1)));
		Zero: Rect = (A: (data: (0, 0)); B: (data: (0, 0)));
	end;

	operator =(const a, b: Rect): boolean;
	operator +(const rect: Rect; const v: Vec2): Rect;
	function Equals(const a, b: Rect; const eps: float = CloseToZeroEps): boolean;
	function RectsIntersection(const a, b: Rect): Rect;
	function PlanesIntersection(const a, b, c: Plane): Vec3;

type
	pUintRect = ^UintRect;
	UintRect = object
		pos, size: UintVec2;
		function Make(const pos, size: UintVec2): UintRect; static;
		function Contains(const point: UintVec2): boolean;
		function Square: uint;
	end;

	function lerp(const a, b: float; const t: float): float; cinline
	function lerp(const p: Quaternion; q: Quaternion; const x: float): Quaternion;
	function raw_lerp(const p, q: Quaternion; const x: float): Quaternion;
	function slerp(const p: Quaternion; q: Quaternion; const x: float): Quaternion;
	function CatmullRomSpline(const p0, p1, p2, p3: float; const t: float): float;
	function CatmullRomSpline(const p0, p1, p2, p3: Quaternion; const t: float): Quaternion;
	function BSpline(const p0, p1, p2, p3: float; const t: float): float;
{$define vecf :=
	function lerp(const a, b: vec; const t: float): vec;
	function CatmullRomSpline(const p0, p1, p2, p3: vec; const t: float): vec;
	function BSpline(const p0, p1, p2, p3: vec; const t: float): vec;} all_floating_vectors

	function SolveLinear(const a, b: hp_float; out x: hp_float): boolean;
	function SolveQuadratic(const a, b, c: hp_float; out x1, x2: hp_float): boolean;

implementation

uses
	Utils;

const
	IdentityVectorEps = sqrt(sqrt(CloseToZeroEps)) * sqrt(sqrt(sqrt(CloseToZeroEps)));

{$define numberf :=
	function Min(const a, b: typ): typ;
	begin
		if a < b then result := a else result := b;
	end;

	function Min(const a, b, c: typ): typ;
	begin
		if a < b then
			if a < c then result := a else result := c
		else // b <= a
			if b < c then result := b else result := c;
	end;

	function Max(const a, b: typ): typ;
	begin
		if a > b then result := a else result := b;
	end;

	function Max(const a, b, c: typ): typ;
	begin
		if a > b then
			if a > c then result := a else result := c
		else // b >= a
			if b > c then result := b else result := c;
	end;

	procedure Swap(var a, b: typ);
	var
		t: typ;
	begin
		t := a;
		a := b;
		b := t;
	end;

	function Clamp(const x, a, b: typ): typ;
	begin
		if x < a then result := a else
			if x > b then result := b else
				result := x;
	end;

	function InRange(const x, a, b: typ): boolean;
	begin
		result := (x >= a) and (x <= b);
	end;

{$ifndef unsigned}
	{$define signimpl := begin if x > 0 then result := 1 else if x < 0 then result := -1 else result := 0; end;}
		function IntSign(const x: typ): sint; signimpl
		function FloatSign(const x: typ): {$ifdef float} typ {$else} float {$endif}; signimpl
		function FloatSign(const x, m: typ): {$ifdef float} typ {$else} float {$endif};
		begin
			if x > 0 then result := m else
				if x < 0 then result := -m else
					result := 0;
		end;
	{$undef signimpl}
{$endif}} all_numbers

	function SmoothStep(const start, end_, x: float): float;
	var
		t: float;
	begin
		t := Clamp((x - start) / (end_ - start), 0, 1);
		result := t * t * (3 - 2 * t);
	end;

	function SmoothStep(const x: float): float;
	var
		t: float;
	begin
		t := Clamp(x, 0, 1);
		result := t * t * (3 - 2 * t);
	end;

	function tan(const x: float): float;
{$ifdef asm386} assembler;
	asm
		fld x
		fptan
		fdivp st, st
		fstp result
	end;
{$else}
{$note tan: assembler disabled}
	begin
		result := sin(x) / cos(x);
	end;
{$endif}

unchecked
	function ArcSin(const x: float): float;
	begin
		if abs(x) <= 1.0 then
			result := arctan(x / sqrt(1 - sqr(x)))
		else
			result := FloatSign(x) * HalfPi;
	end;
end_unchecked

	function ArcCos(const x: float): float;
	begin
		result := HalfPi - ArcSin(x);
	end;

	function ArcTan2(const y, x: float): float;
{$ifdef asm386} assembler;
	asm
		fld y
		fld x
		fpatan
		fstp result
	end;
{$else}
{$note arctan2: assembler disabled}
	begin
		if y <> 0 then result := 2 * arctan((sqrt(sqr(x) + sqr(y)) - x) / y) else
			if x >= 0 then result := 0 else result := Pi;
	end;
{$endif}

	function AngleDiff(const a, b: float): float;
	begin
		result := modf(a - b, TwoPi);
		if result > Pi then result -= TwoPi;
	end;

	function NormalizeAngle(const a: float): float;
	begin
		result := modf(a, TwoPi);
		if result > Pi then result -= TwoPi;
	end;

	function AngleNormalized(const a: float): boolean;
	begin
		result := (a >= -Pi - CloseToZeroEps) and (a <= Pi + CloseToZeroEps);
	end;

	function pow(const base, exponent: float): float;
	begin
		if base = 0 then result := 0 else result := exp(ln(base) * exponent);
	end;

unchecked
	function pow(base, exponent: uint): uint;
	begin
		result := 1;
		while exponent > 0 do
		begin
			if exponent and 1 <> 0 then result *= base;
			base := sqr(base);
			exponent := exponent shr 1;
		end;
	end;
end_unchecked

	function signedPow(const base, exponent: float): float;
	begin
		if base >= 0 then
			result := pow(base, exponent)
		else
			result := -pow(-base, exponent);
	end;

{$define floatf :=
	function modf(const x, d: typ): typ;
	begin
		if x >= CloseToZeroEps then
			result := x - d * int(x / d)
		else
			if x <= -CloseToZeroEps then
				result := x + d * ceil(-x / d)
			else
				result := 0;
	end;

	function closer(const x, t, d: typ): typ;
	begin
		if x < t then
		begin
			result := x + d;
			if GreaterThanEqual(result, t) then result := t;
		end else
		begin
			result := x - d;
			if LessThanEqual(result, t) then result := t;
		end;
	end;} all_floats

	function logn(const base, x: float): float;
	begin
		result := ln(x) / ln(base);
	end;

{$define impl :=
	function floorf(const x: float): rtype;
	begin
		result := truncf(x);
		if (x < 0) and (frac(x) < 0) then
			result -= 1;
	end;

	function ceilf(const x: float): rtype;
	begin
		result := truncf(x);
		if (x > 0) and (frac(x) > 0) then
			result += 1;
	end; {$undef ceilf} {$undef floorf} {$undef truncf}}
	{$define rtype := float} {$define ceilf := ceil} {$define floorf := floor} {$define truncf := int} impl
	{$define rtype := sint} {$define ceilf := iceil} {$define floorf := ifloor} {$define truncf := trunc} impl
{$undef impl}

	function remap(const x: float; const a1, b1, a2, b2: float): float;
	begin
		result := a2 + (x - a1) / (b1 - a1) * (b2 - a2);
		if b1 = a1 then result := a2;
	end;

	function remapc(const x: float; const a1, b1, a2, b2: float): float;
	begin
		result := a2 + (x - a1) / (b1 - a1) * (b2 - a2);
		if result > b2 then result := b2;
		if result >= a2 then else result := a2; // сработает на NaN
	end;

	function IsZero(const x: float): boolean;                      begin result := abs(x) < CloseToZeroEps    ; end;
	function IsZero(const x: float; const eps: float): boolean;    begin result := abs(x) < eps               ; end;
	function NotZero(const x: float): boolean;                     begin result := abs(x) >= CloseToZeroEps   ; end;
	function Equals(const a, b: float): boolean;                   begin result := abs(a - b) < CloseToZeroEps; end;
	function Equals(const a, b: float; const eps: float): boolean; begin result := abs(a - b) < eps           ; end;
	function GreaterThan(const a, b: float): boolean;              begin result := a - b >= CloseToZeroEps    ; end;
	function GreaterThan(const a, b: float; const eps: float): boolean; begin result := a - b >= eps          ; end;
	function LessThan(const a, b: float): boolean;                 begin result := b - a >= CloseToZeroEps    ; end;
	function LessThan(const a, b: float; const eps: float): boolean; begin result := b - a >= eps             ; end;
	function GreaterThanEqual(const a, b: float): boolean;         begin result := b - a < CloseToZeroEps     ; end;
	function GreaterThanEqual(const a, b: float; const eps: float): boolean; begin result := b - a < eps      ; end;
	function LessThanEqual(const a, b: float): boolean;            begin result := a - b < CloseToZeroEps     ; end;
	function LessThanEqual(const a, b: float; const eps: float): boolean; begin result := a - b < eps         ; end;

	function RangeIntersects(const l1, r1, l2, r2: float): boolean;
	begin
		result := GreaterThanEqual(r1, l2) and GreaterThanEqual(r2, l1);
	end;

	function RangeIntersectsPrecise(const l1, r1, l2, r2: float): boolean;
	begin
		result := (r1 >= l2) and (l1 <= r2);
	end;

	function RangeStrictlyIntersects(const l1, r1, l2, r2: float): boolean;
	begin
		result := GreaterThan(r1, l2) and GreaterThan(r2, l1);
	end;

	function RangeStrictlyIntersectsPrecise(const l1, r1, l2, r2: float): boolean;
	begin
		result := (r1 > l2) and (l1 < r2);
	end;

	function RangeIntersection(const l1, r1, l2, r2: float): float;
	begin
		if l1 < l2 then // L1 L2
			if r1 < l2 then // L1 R1 L2
				result := 0
			else // L1 L2 R1
				if r1 < r2 then // L1 L2 R1 R2
					result := r1 - l2
				else // L1 L2 R2 R1
					result := r2 - l2
		else // L2 L1
			if r2 < l1 then // L2 R2 L1
				result := 0
			else // L2 L1 R2
				if r1 < r2 then // L2 L1 R1 R2
					result := r1 - l1
				else // L2 L1 R2 R1
					result := r2 - l1;
	end;

{$define intf :=
	function RangeIntersects(const l1, r1, l2, r2: typ): boolean;
	begin
		result := ((l1 <= l2) and (r1 >= l2)) or ((l1 <= r2) and (r1 >= r2));
	end;} all_ints

unchecked
{$define intf :=
	function IsPowerOf2(x: typ): boolean;
	begin
		result := x and (x - 1) = 0;
	end;

	function OnesRight(x: typ): typ;
	begin
		x := x or (x shr 1);
		x := x or (x shr 2);
		x := x or (x shr 4);
		x := x or (x shr 8);
		x := x or (x shr 16);
	{$if sizeof(x) > 4} x := x or (x shr 32); {$endif}
	{$if sizeof(x) > 8} {$error OnesRight: too few iterations} {$endif}
		result := x;
	end;

	function UpToPow2(x: typ): typ;
	begin
		result := OnesRight(typ(x - 1)) + 1;
	end;

	function Log2(x: typ): uint;
	begin
		Assert(x > 0);
		result :=
			{$if sizeof(x) = sizeof(uint32)} BsrDWord
			{$elseif sizeof(x) = sizeof(uint64)} BsrQWord
			{$else} {$error Log2: неподдерживаемый uint} {$endif}
			(x);
	end;

	function CeilLog2(x: typ): uint;
	begin
		result := Log2(x);
		if x xor (typ(1) shl result) <> 0 then inc(result);
	end;} all_uints
end_unchecked

	function TrustedGCD(a, b: uint): uint;
	var
		t: uint;
	begin
		while b <> 0 do
		begin
			t := b;
			b := a mod b;
			a := t;
		end;
		result := a;
	end;

	function GCD(a, b: uint): uint;
	begin
		if (a = 0) or (b = 0) then
			result := a + b
		else
			if a > b then
				result := TrustedGCD(a, b)
			else
				result := TrustedGCD(b, a);
	end;

	function LCM(a, b: uint): uint;
	begin
		result := (a div GCD(a, b)) * b;
	end;

{$define vec_compo_op := begin {$define iterate := result.item := op;} foreach_component end; {$undef op}}
{$define vecf :=
	{$define sqrlen := {$define one := sqr(item)} reduce_vec}

	function vec.Make(const x: base_type): vec; {$define op := x} vec_compo_op
	function vec.Make(const {$define one := item} comma_separated: base_type): vec; {$define op := item} vec_compo_op
{$if veclen = 3} function vec.Make(const xy: pair2; const z: base_type): vec; begin result.x := xy.x; result.y := xy.y; result.z := z; end; {$endif}
{$if veclen = 4}
	function vec.Make(const v: pair3; const w: base_type): vec; begin result.x := v.x; result.y := v.y; result.z := v.z; result.w := w; end;
	function vec.Make(const a, b: pair2): vec;                  begin result.x := a.x; result.y := a.y; result.z := b.x; result.w := b.y; end;
{$endif}

{$ifdef floating}
	function vec.GetLength: base_type;         begin result := sqrt(sqrlen); end;
	procedure vec.SetLength(newLength: base_type); begin self := Normalized * newLength; end;
	function vec.IsZero: boolean;             begin result := sqrlen < IdentityVectorEps; end;
	function vec.NonZero: boolean;            begin result := sqrlen >= IdentityVectorEps; end;
	function vec.IsIdentity: boolean;         begin result := abs(sqrlen - 1) < IdentityVectorEps; end;
	function vec.SqrLength: base_type;        begin result := sqrlen; end;

	function vec.Normalized: vec;
	var
		t: base_type;
	begin
		t := sqrlen;
		if UMath.IsZero(t) then
			result := Zero
		else
			result := self / sqrt(t);
	end;

	function vec.MaybeNormalized: vec;
	var
		t: base_type;
	begin
	{$ifdef Debug} stat.Increment(maybenormalized_vectors); {$endif}
		t := sqrlen;
		if Equals(t, 1, IdentityVectorEps) then result := self else
		begin
		{$ifdef Debug} stat.Increment(maybenormalized_denorm_vectors); {$endif}
			if UMath.IsZero(t) then result := Zero else
				result := self / sqrt(t);
		end;
	end;

	function vec.Normalized(out len: base_type): vec;
	var
		t: base_type;
	begin
		t := sqrlen;
		if NotZero(t) then
		begin
			t := sqrt(t);
			len := t;
			t := 1/t;
		end else
		begin
			t := 0;
			len := 0;
		end;
		result := self * t;
	end;

	function vec.Clamped(const lim: base_type): vec;
	var
		t: base_type;
	begin
		t := sqrlen;
		if t <= sqr(lim) then
			result := self
		else
		begin
			if NotZero(t) then
				t := lim / sqrt(t)
			else
				t := 0;
			result := self * t;
		end;
	end;

{$if veclen = 3}
	function vec.LineEquation(const a, b: pair2): vec;
	var
		d: pair2;
		s: float;
	begin
		d := b - a;
		s := d.SqrLength;
		if UMath.IsZero(s) then result := vec.PositiveZ else
		begin
			s := 1 / sqrt(s);
			result.x := d.y * s;
			result.y := -d.x * s;
			result.z := -(result.x * a.x + result.y * a.y);
		end;
	end;
{$endif}

{$if veclen = 4}
	function vec.Homo3: pair3; begin result := xyz / w; end;
{$endif veclen = 4}
{$endif floating}

	function vec.Sum: base_type; begin result := {$define one := item} reduce_vec; end;
	function vec.Product: base_type; begin result := {$define one := item} {$define op := *} reduce_vec; end;
{$if veclen = 2}
	function vec.Aspect: {$ifdef floating} base_type {$else} float {$endif}; begin result := x/y; end;
{$ifdef unsigned}
	function vec.FitsClosed(const R: vec): boolean; begin result := {$define one := (item <= R.item)} {$define op := and} reduce_vec; end;
	function vec.FitsOpen(const R: vec): boolean; begin result := {$define one := (item < R.item)} {$define op := and} reduce_vec; end;
{$endif}
{$endif}

{$ifdef integer}
	function vec.Square: base_type_unsigned; begin result := {$define one := sqr(item)} reduce_vec; end;
	function vec.Positive: boolean; begin result := {$define one := (item <> 0)} {$define op := and} reduce_vec; end;
{$endif}

{$if veclen = 2} function vec.yx: pair2; begin result.x := y; result.y := x; end; {$endif}
{$if veclen = 3} function vec.xy: pair2; begin result.x := x; result.y := y; end; {$endif}
{$if veclen = 4} function vec.xyz: pair3; begin result.x := x; result.y := y; result.z := z; end; {$endif}

	operator =(const a, b: vec): boolean; begin result := {$define one := (a.item = b.item)} {$define op := and} reduce_vec; end;
	operator +(const a, b: vec): vec; {$define op := a.item + b.item} vec_compo_op
	operator -(const a, b: vec): vec; {$define op := a.item - b.item} vec_compo_op
{$ifdef signed} operator -(const v: vec): vec; {$define op := -v.item} vec_compo_op {$endif}
	operator *(const a, b: vec): vec; {$define op := a.item * b.item} vec_compo_op
	operator *(const v: vec; const k: base_type): vec; {$define op := v.item * k} vec_compo_op
	operator *(const k: base_type; const v: vec): vec; {$define op := k * v.item} vec_compo_op
	function Min(const a, b: vec): vec; {$define op := Min(a.item, b.item)} vec_compo_op
	function Max(const a, b: vec): vec; {$define op := Max(a.item, b.item)} vec_compo_op
{$undef sqrlen}} all_vectors

{$define vecf :=
	operator /(const v: vec; const x: base_type): vec; begin result := v * (1/x); end;
	operator /(const a, b: vec): vec; {$define op := a.item / b.item} vec_compo_op
	operator /(const x: base_type; const v: vec): vec; {$define op := x / v.item} vec_compo_op
	operator **(const a, b: vec): base_type;  begin result := {$define one := a.item * b.item} reduce_vec; end;

	function Equals(const a, b: vec; const eps: base_type = CloseToZeroEps): boolean; begin result := SqrDistance(a, b) < sqr(eps); end;
	function Clamp(const v, a, b: vec): vec; {$define op := Clamp(v.item, a.item, b.item)} vec_compo_op
	function SqrDistance(const a, b: vec): base_type; begin result := {$define one := sqr(a.item - b.item)} reduce_vec; end;
	function Distance(const a, b: vec): base_type; begin result := sqrt(SqrDistance(a, b)); end;
	function NormalizeAngle(const a: vec): vec; {$define op := NormalizeAngle(a.item)} vec_compo_op
	function AngleDiff(const a, b: vec): vec; {$define op := AngleDiff(a.item, b.item)} vec_compo_op

{$if veclen = 2}
	function Angle(const a, b: vec): base_type; begin result := NormalizeAngle(ArcTan2(a.y, a.x) - ArcTan2(b.y, b.x)); end;
{$endif}

{$if veclen = 3}
	operator ><(const a, b: vec): vec;
	begin
		result.x := a.y * b.z - a.z * b.y;
		result.y := a.z * b.x - a.x * b.z;
		result.z := a.x * b.y - a.y * b.x;
	end;

	function AngleUN(const a, b: vec): base_type;
	var
		len: base_type;
	begin
		len := sqrt(a.SqrLength * b.SqrLength);
		if NotZero(len) then result := ArcCos((a ** b) / len) else result := 0;
	end;

	function AngleN(const a, b: vec): base_type;
	begin
		Assert(a.IsIdentity, 'Неединичный вектор A! (' + ToString(a) + ')');
		Assert(b.IsIdentity, 'Неединичный вектор B! (' + ToString(b) + ')');
		result := ArcCos(a ** b);
	end;

	function Reflect(const I, N: vec): vec;
	begin
		result := I - 2 * (N ** I) * N;
	end;
{$endif veclen = 3}

	function UintTrunc(const v: vec): uint_vec; {$define op := trunc(v.item)} vec_compo_op
	function UintRound(const v: vec): uint_vec; {$define op := round(v.item)} vec_compo_op
	function IntTrunc(const v: vec): int_vec; {$define op := trunc(v.item)} vec_compo_op
	function IntRound(const v: vec): int_vec; {$define op := round(v.item)} vec_compo_op} all_floating_vectors

{$define vecf :=
	operator div(const a, b: vec): vec; {$define op := a.item div b.item} vec_compo_op
	operator div(const v: vec; const d: base_type): vec; {$define op := v.item div d} vec_compo_op
{$ifdef unsigned}
	operator mod(const a, b: vec): vec; {$define op := a.item mod b.item} vec_compo_op
	operator mod(const v: vec; const d: base_type): vec; {$define op := v.item mod d} vec_compo_op
	operator shl(const v: vec; bits: uint): vec; {$define op := v.item shl bits} vec_compo_op
	operator shr(const v: vec; bits: uint): vec; {$define op := v.item shr bits} vec_compo_op
{$endif}

{$if not (defined(unsigned) and not defined(signed_vec))}
	operator :=(const v: vec): {$ifdef signed} unsigned_vec {$else} signed_vec {$endif}; {$define op := base_type(v.item)} vec_compo_op
	operator :=(const v: vec): float_vec; {$define op := v.item} vec_compo_op
{$if defined(signed) and defined(integer)}
	operator +(const v: vec; const d: uint_vec): vec; {$define op := v.item + base_type_signed(d.item)} vec_compo_op
	operator -(const v: vec; const d: unsigned_vec): vec; {$define op := v.item - base_type_signed(d.item)} vec_compo_op
{$endif}
{$ifdef unsigned} operator +(const v: vec; const d: int_vec): vec; {$define op := base_type(base_type_signed(v.item) + d.item)} vec_compo_op {$endif}
{$endif}} all_integer_vectors

	function Rotate(const v: Vec2; const angle: float): Vec2;
	var
		cosa, sina: float;
	begin
		cosa := cos(angle);
		sina := sin(angle);
		result.x := v.x*cosa - v.y*sina;
		result.y := v.x*sina + v.y*cosa;
	end;

	function ShrinkToAspect(const size, aspect: UintVec2): UintVec2;
	var
		rx, ry: uint;
	begin
		if not aspect.Positive then exit(UintVec2.Zero);
		rx := size.x * aspect.y;
		ry := size.y * aspect.x;
		if rx > ry then result := UintVec2.Make((size.y * aspect.x) div aspect.y, size.y) else
			if rx < ry then result := UintVec2.Make(size.x, (size.x * aspect.y) div aspect.x) else
				result := size;
	end;

	function Matrix4.Transposed: Matrix4;
	var
		i, j: uint;
	begin
		for j := 0 to 3 do
			for i := 0 to 3 do
				result.data.m[i, j] := data.m[j, i];
	end;

	function Matrix4.Determinant: float;
	begin
		result :=
			(m[0,0]*m[1,1] - m[0,1]*m[1,0]) * (m[2,2]*m[3,3] - m[2,3]*m[3,2]) -
			(m[0,0]*m[1,2] - m[0,2]*m[1,0]) * (m[2,1]*m[3,3] - m[2,3]*m[3,1]) +
			(m[0,0]*m[1,3] - m[0,3]*m[1,0]) * (m[2,1]*m[3,2] - m[2,2]*m[3,1]) +
			(m[0,1]*m[1,2] - m[0,2]*m[1,1]) * (m[2,0]*m[3,3] - m[2,3]*m[3,0]) -
			(m[0,1]*m[1,3] - m[0,3]*m[1,1]) * (m[2,0]*m[3,2] - m[2,2]*m[3,0]) +
			(m[0,2]*m[1,3] - m[0,3]*m[1,2]) * (m[2,0]*m[3,1] - m[2,1]*m[3,0]);
	end;

	function Matrix4.Minor(x, y: sint): float;
	var
		i, j, q, w: sint;
		m3: Matrix3;
	begin
		w := 0;
		for j := 0 to 2 do
		begin
			if w = y then inc(w);
			q := 0;
			for i := 0 to 2 do
			begin
				if q = x then inc(q);
				m3.data.m[i, j] := m[q, w];
				inc(q);
			end;
			inc(w);
		end;
		result := m3.Determinant;
	end;

	function Matrix4.Inversed: Matrix4; begin result := Inversed(Determinant); end;

	function Matrix4.Inversed(const det: float): Matrix4;
	var
		rm: MatrixData absolute result.data.m;
		id: float;
	begin
		if IsZero(det) then exit(Identity);
		id := 1/det;
		rm[0,0]:=id*(m[1,1]*(m[2,2]*m[3,3]-m[2,3]*m[3,2])+m[1,2]*(m[2,3]*m[3,1]-m[2,1]*m[3,3])+m[1,3]*(m[2,1]*m[3,2]-m[2,2]*m[3,1]));
		rm[0,1]:=id*(m[2,1]*(m[0,2]*m[3,3]-m[0,3]*m[3,2])+m[2,2]*(m[0,3]*m[3,1]-m[0,1]*m[3,3])+m[2,3]*(m[0,1]*m[3,2]-m[0,2]*m[3,1]));
		rm[0,2]:=id*(m[3,1]*(m[0,2]*m[1,3]-m[0,3]*m[1,2])+m[3,2]*(m[0,3]*m[1,1]-m[0,1]*m[1,3])+m[3,3]*(m[0,1]*m[1,2]-m[0,2]*m[1,1]));
		rm[0,3]:=id*(m[0,1]*(m[1,3]*m[2,2]-m[1,2]*m[2,3])+m[0,2]*(m[1,1]*m[2,3]-m[1,3]*m[2,1])+m[0,3]*(m[1,2]*m[2,1]-m[1,1]*m[2,2]));
		rm[1,0]:=id*(m[1,2]*(m[2,0]*m[3,3]-m[2,3]*m[3,0])+m[1,3]*(m[2,2]*m[3,0]-m[2,0]*m[3,2])+m[1,0]*(m[2,3]*m[3,2]-m[2,2]*m[3,3]));
		rm[1,1]:=id*(m[2,2]*(m[0,0]*m[3,3]-m[0,3]*m[3,0])+m[2,3]*(m[0,2]*m[3,0]-m[0,0]*m[3,2])+m[2,0]*(m[0,3]*m[3,2]-m[0,2]*m[3,3]));
		rm[1,2]:=id*(m[3,2]*(m[0,0]*m[1,3]-m[0,3]*m[1,0])+m[3,3]*(m[0,2]*m[1,0]-m[0,0]*m[1,2])+m[3,0]*(m[0,3]*m[1,2]-m[0,2]*m[1,3]));
		rm[1,3]:=id*(m[0,2]*(m[1,3]*m[2,0]-m[1,0]*m[2,3])+m[0,3]*(m[1,0]*m[2,2]-m[1,2]*m[2,0])+m[0,0]*(m[1,2]*m[2,3]-m[1,3]*m[2,2]));
		rm[2,0]:=id*(m[1,3]*(m[2,0]*m[3,1]-m[2,1]*m[3,0])+m[1,0]*(m[2,1]*m[3,3]-m[2,3]*m[3,1])+m[1,1]*(m[2,3]*m[3,0]-m[2,0]*m[3,3]));
		rm[2,1]:=id*(m[2,3]*(m[0,0]*m[3,1]-m[0,1]*m[3,0])+m[2,0]*(m[0,1]*m[3,3]-m[0,3]*m[3,1])+m[2,1]*(m[0,3]*m[3,0]-m[0,0]*m[3,3]));
		rm[2,2]:=id*(m[3,3]*(m[0,0]*m[1,1]-m[0,1]*m[1,0])+m[3,0]*(m[0,1]*m[1,3]-m[0,3]*m[1,1])+m[3,1]*(m[0,3]*m[1,0]-m[0,0]*m[1,3]));
		rm[2,3]:=id*(m[0,3]*(m[1,1]*m[2,0]-m[1,0]*m[2,1])+m[0,0]*(m[1,3]*m[2,1]-m[1,1]*m[2,3])+m[0,1]*(m[1,0]*m[2,3]-m[1,3]*m[2,0]));
		rm[3,0]:=id*(m[1,0]*(m[2,2]*m[3,1]-m[2,1]*m[3,2])+m[1,1]*(m[2,0]*m[3,2]-m[2,2]*m[3,0])+m[1,2]*(m[2,1]*m[3,0]-m[2,0]*m[3,1]));
		rm[3,1]:=id*(m[2,0]*(m[0,2]*m[3,1]-m[0,1]*m[3,2])+m[2,1]*(m[0,0]*m[3,2]-m[0,2]*m[3,0])+m[2,2]*(m[0,1]*m[3,0]-m[0,0]*m[3,1]));
		rm[3,2]:=id*(m[3,0]*(m[0,2]*m[1,1]-m[0,1]*m[1,2])+m[3,1]*(m[0,0]*m[1,2]-m[0,2]*m[1,0])+m[3,2]*(m[0,1]*m[1,0]-m[0,0]*m[1,1]));
		rm[3,3]:=id*(m[0,0]*(m[1,1]*m[2,2]-m[1,2]*m[2,1])+m[0,1]*(m[1,2]*m[2,0]-m[1,0]*m[2,2])+m[0,2]*(m[1,0]*m[2,1]-m[1,1]*m[2,0]));
	end;

	function Matrix4.Rows(const x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, xa, xb, xc, xd, xe, xf: float): Matrix4;
	var
		z: Matrix4.MatrixData absolute result.data.m;
	begin
		z[0, 0] := x0; z[1, 0] := x1; z[2, 0] := x2; z[3, 0] := x3;
		z[0, 1] := x4; z[1, 1] := x5; z[2, 1] := x6; z[3, 1] := x7;
		z[0, 2] := x8; z[1, 2] := x9; z[2, 2] := xa; z[3, 2] := xb;
		z[0, 3] := xc; z[1, 3] := xd; z[2, 3] := xe; z[3, 3] := xf;
	end;

	function Matrix4.Translation(const v: Vec3): Matrix4;
	begin
		result := Rows(
			1, 0, 0, v.x,
			0, 1, 0, v.y,
			0, 0, 1, v.z,
			0, 0, 0,   1);
	end;

	function Matrix4.Rotation(const ane: float; const v: Vec3): Matrix4;
	begin
		result := Quaternion.Rotation(ane, v.Normalized).ToMatrix;
	end;

	function Matrix4.Scaling(const kx, ky, kz: float): Matrix4;
	begin
		result := Rows(
			kx, 0,  0, 0,
			0, ky,  0, 0,
			0,  0, kz, 0,
			0,  0,  0, 1);
	end;

	function Matrix4.FrustumProjection(const left, right, bottom, top, near, far: float): Matrix4;
	var
		nn, dx, dy, dz: float;
	begin
		nn := 2 * near;
		dx := right - left;
		dy := top - bottom;
		dz := far - near;
		result := Rows(
			nn/dx,     0, (right + left) / dx,                0,
					0, nn/dy, (top + bottom) / dy,                0,
					0,     0,  (-far - near) / dz, (-nn * far) / dz,
					0,     0,                  -1,                0);
	end;

	function Matrix4.PerspectiveProjection(const fovy, aspect, near, far: float): Matrix4;
	var
		xmax, ymax: float;
	begin
		ymax := near * tan(0.5 * fovy * Deg2Rad);
		xmax := ymax * aspect;
		result := FrustumProjection(-xmax, xmax, -ymax, ymax, near, far);
	end;

	function Matrix4.OrthographicProjection(const l, r, b, t, n, f: float): Matrix4;
	begin
		result := Rows(
			2/(r - l),            0,         0,         (r + l)/(l - r),
			        0,    2/(t - b),         0,         (t + b)/(b - t),
			        0,            0,         2/(n - f), (f + n)/(f - n),
			        0,            0,         0,                      1);
	end;

	function Matrix3.Determinant: float;
	begin
		result :=
			m[0,0] * (m[1,1]*m[2,2] - m[1,2]*m[2,1]) -
			m[0,1] * (m[1,0]*m[2,2] - m[1,2]*m[2,0]) +
			m[0,2] * (m[1,0]*m[2,1] - m[1,1]*m[2,0]);
	end;

	function Matrix3.Inversed: Matrix3; begin result := Inversed(Determinant); end;

	function Matrix3.Inversed(const det: float): Matrix3;
	var
		rm: MatrixData absolute result.data.m;
		id: float;
	begin
		if IsZero(det) then exit(Identity);
		id := 1/det;
		rm[0,0]:=(m[1,1]*m[2,2]-m[2,1]*m[1,2])*id;
		rm[1,0]:=-(m[1,0]*m[2,2]-m[2,0]*m[1,2])*id;
		rm[2,0]:=(m[1,0]*m[2,1]-m[2,0]*m[1,1])*id;
		rm[0,1]:=-(m[0,1]*m[2,2]-m[2,1]*m[0,2])*id;
		rm[1,1]:=(m[0,0]*m[2,2]-m[2,0]*m[0,2])*id;
		rm[2,1]:=-(m[0,0]*m[2,1]-m[2,0]*m[0,1])*id;
		rm[0,2]:=(m[0,1]*m[1,2]-m[1,1]*m[0,2])*id;
		rm[1,2]:=-(m[0,0]*m[1,2]-m[1,0]*m[0,2])*id;
		rm[2,2]:=(m[0,0]*m[1,1]-m[1,0]*m[0,1])*id;
	end;

	function Matrix3.Transposed: Matrix3;
	var
		i, j: uint;
	begin
		for j := 0 to 2 do
			for i := 0 to 2 do
				result.data.m[i, j] := data.m[j, i];
	end;

	function Matrix3.Rows(const x0, x1, x2, x3, x4, x5, x6, x7, x8: float): Matrix3;
	var
		z: Matrix3.MatrixData absolute result.data.m;
	begin
		z[0, 0] := x0; z[1, 0] := x1; z[2, 0] := x2;
		z[0, 1] := x3; z[1, 1] := x4; z[2, 1] := x5;
		z[0, 2] := x6; z[1, 2] := x7; z[2, 2] := x8;
	end;

	function Matrix3.Columns(const a, b, c: Vec3): Matrix3;
	begin
		Vec3.LinearData(result.data.m[0]) := a.data;
		Vec3.LinearData(result.data.m[1]) := b.data;
		Vec3.LinearData(result.data.m[2]) := c.data;
	end;

	function Matrix3.RotationN(const ane: float; const v: Vec3): Matrix3;
	begin
		Assert(v.IsIdentity, ToString(v));
		result := Quaternion.Rotation(ane, v).ToMatrix3;
	end;

	function Mat3(const m4: Matrix4): Matrix3;
	var
		i, j: uint;
	begin
		for j := 0 to 2 do
			for i := 0 to 2 do
				result.data.m[i, j] := m4.m[i, j];
	end;

	function Mat4(const m3: Matrix3): Matrix4;
	var
		m: Matrix3.MatrixData absolute m3.data.m;
	begin
		result := Matrix4.Rows(
			m[0, 0], m[1, 0], m[2, 0], 0,
			m[0, 1], m[1, 1], m[2, 1], 0,
			m[0, 2], m[1, 2], m[2, 2], 0,
					0,       0,       0, 1);
	end;

	operator *(const mat: Matrix4; const vec: Vec4): Vec4;
	var
		i, j: uint;
	begin
		result := Vec4.Zero;
		for j := 0 to 3 do
			for i := 0 to 3 do
				result.data[j] += vec.data[i] * mat.m[i, j];
	end;

	operator *(const a, b: Matrix4): Matrix4;
	var
		i, j, k: uint;
		t: float;
	begin
		for i := 0 to 3 do
			for j := 0 to 3 do
			begin
				t := 0;
				for k := 0 to 3 do
					t += a.m[k, j] * b.m[i, k];
				result.data.m[i, j] := t;
			end;
	end;

	operator *(const a, b: Matrix3): Matrix3;
	var
		i, j, k: uint;
	begin
		result := Matrix3.Zero;
		for i := 0 to 2 do
			for j := 0 to 2 do
				for k := 0 to 2 do
					result.data.m[i, j] += a.m[k, j] * b.m[i, k];
	end;

	operator *(const mat: Matrix3; const vec: Vec3): Vec3;
	var
		i, j: uint;
	begin
		result := Vec3.Zero;
		for j := 0 to 2 do
			for i := 0 to 2 do
				result.data[j] += vec.data[i] * mat.m[i, j];
	end;

	operator *(const vec: Vec3; const mat: Matrix3): Vec3; begin result := mat * vec; end;

	function AngleUN(const a, b: Quaternion): float;
	var
		len: float;
	begin
		len := a.Norm * b.Norm;
		if NotZero(len) then
			result := 2 * ArcCos((a ** b) / sqrt(len))
		else
			result := 0;
	end;

	function AngleN(const a, b: Quaternion): float;
	begin
		Assert(a.IsUnit, 'Non-unit quaternion A! (' + ToString(a) + ')');
		Assert(b.IsUnit, 'Non-unit quaternion B! (' + ToString(a) + ')');
		result := 2 * ArcCos(a ** b);
	end;

	function Quaternion.Make(const v: Vec3; const w: float): Quaternion;
	begin
		result.v4 := Vec4.Make(v, w);
	end;

	function Quaternion.Make(const x, y, z, w: float): Quaternion;
	begin
		result.v4 := Vec4.Make(x, y, z, w);
	end;

	function Quaternion.FromMatrix(const matrix: Matrix3): Quaternion;
	var
		m: Matrix3.MatrixData absolute matrix.data.m;
		tr, t: float;
	begin
		tr := m[0, 0] + m[1, 1] + m[2, 2];
		if tr > 0 then
		begin
			t := tr + 1;
			result := Make(m[1, 2] - m[2, 1], m[2, 0] - m[0, 2], m[0, 1] - m[1, 0], t);
		end else
			if ((m[0, 0] > m[1, 1]) and (m[0, 0] > m[2, 2])) then
			begin
				t := 1 + m[0, 0] - m[1, 1] - m[2, 2];
				result := Make(t, m[1, 0] + m[0, 1], m[2, 0] + m[0, 2], m[1, 2] - m[2, 1]);
			end else
				if (m[1, 1] > m[2, 2]) then
				begin
					t := 1 + m[1, 1] - m[0, 0] - m[2, 2];
					result := Make(m[1, 0] + m[0, 1], t, m[2, 1] + m[1, 2], m[2, 0] - m[0, 2]);
				end else
				begin
					t := 1 + m[2, 2] - m[0, 0] - m[1, 1];
					result := Make(m[2, 0] + m[0, 2], m[2, 1] + m[1, 2], t, m[0, 1] - m[1, 0]);
				end;

		result *= 0.5 / sqrt(t);
	end;

	function Quaternion.Rotation(const angle: float; const axis: Vec3): Quaternion;
	begin
		Assert(axis.IsIdentity, 'Non-identity vector "axis"! (' + ToString(axis) + ')');
		result.v3 := axis * sin(0.5 * angle);
		result.w := cos(0.5 * angle);
	end;

	function Quaternion.Rotation(const from, to_: Vec3): Quaternion;
	var
		c: Vec3;
	begin
		Assert(from.IsIdentity, 'Non-identity vector "from"! (' + ToString(from) + ')');
		Assert(to_.IsIdentity, 'Non-identity vector "to"! (' + ToString(to_) + ')');
		c := from >< to_;
		result := Make(c, from ** to_);
		result.data.v[3] += 1; // reducing angle to halfangle
		if result.w < CloseToZeroEps then // angle close to PI
		begin
			if sqr(from.z) > sqr(from.x) then
				result := Make(0, from.z, -from.y, result.w) // f2*Vec3(1,0,0)
			else
				result := Make(from.y, -from.x, 0, result.w); // f2*Vec3(0,0,1)
		end;
		result := result.Normalized;
	end;

	function Quaternion.RotationThroughX0Z(const from, to_: Vec3): Quaternion;
	var
		from_y0, to_y0: Vec3;
		anY: float;
	begin
		from_y0 := from; from_y0.y := 0;
		to_y0 := to_; to_y0.y := 0;
		if IsZero(from_y0.SqrLength) or IsZero(to_y0.SqrLength) then exit(Rotation(from, to_));
		anY := AngleUN(to_y0, to_);
		result := Rotation(FloatSign(-from.y) * AngleUN(from, from_y0), Vec3.PositiveX) *
			Rotation(from_y0.Normalized, to_y0.Normalized) *
			Rotation(FloatSign(to_.y) * anY, Vec3.PositiveX);
	end;

	function Quaternion.FromEuler(const yaw, pitch, roll: float): Quaternion;
	var
		c1, s1, c2, s2, c3, s3, c1c2, s1s2: float;
	begin
		// Heading = Yaw
		// Attitude = Pitch
		// Bank = Roll

		c1 := cos(0.5 * yaw);
		s1 := sin(0.5 * yaw);
		c3 := cos(0.5 * pitch);
		s3 := sin(0.5 * pitch);
		c2 := cos(0.5 * roll);
		s2 := sin(0.5 * roll);
		c1c2 := c1 * c2;
		s1s2 := s1 * s2;

		result.x := c1c2*s3  + s1s2*c3;
		result.y := s1*c2*c3 + c1*s2*s3;
		result.z := c1*s2*c3 - s1*c2*s3;
		result.w := c1c2*c3  - s1s2*s3;
	end;

	function Quaternion.FromEuler(const eul: Vec3): Quaternion;
	begin
		result := FromEuler(eul.x, eul.y, eul.z);
	end;

	function Quaternion.Length: float; begin result := sqrt(sqr(x) + sqr(y) + sqr(z) + sqr(w)); end;
	function Quaternion.IsUnit: boolean; begin result := abs(Norm - 1) < IdentityVectorEps; end;
	function Quaternion.Norm: float; begin result := sqr(x) + sqr(y) + sqr(z) + sqr(w); end;

	function Quaternion.Normalized: Quaternion;
	var
		n: float;
	begin
		n := Norm;
		if NotZero(n) then
			result.v4 := v4 / sqrt(n)
		else
			result := Quaternion.Identity;
	end;

	function Quaternion.MaybeNormalized: Quaternion;
	var
		n: float;
	begin
	{$ifdef Debug} stat.Increment(maybenormalized_quaternions); {$endif}
		n := Norm;
		if Equals(n, 1, IdentityVectorEps) then result := self else
		begin
		{$ifdef Debug} stat.Increment(maybenormalized_denorm_quaternions); {$endif}
			if NotZero(n) then result.v4 := v4 / sqrt(n) else
				result := Quaternion.Identity;
		end;
	end;

	function Quaternion.Inversed: Quaternion;
	begin
		{n := Norm;
		if (n >= CloseToZeroEps) and not Equals(n, 1) then
		begin
			inorm := 1 / n;
			result := Make(-x * inorm, -y * inorm, -z * inorm, w * inorm);
		end else
			result := IdentityQuaternion;}

		result := Make(v3, -w);
	end;

	function Quaternion.ToMatrix: Matrix4; begin result := Mat4(ToMatrix3); end;

	function Quaternion.ToMatrix3: Matrix3;
	var
		n, wx, wy, wz, xx, yy, yz, xy, xz, zz, x2, y2, z2, s: float;
	begin
		n := Norm;
		if n = 0 then exit(Matrix3.Identity);
		s := 2 / Norm;
		x2 := x * s;    y2 := y * s;    z2 := z * s;
		xx := x * x2;   xy := x * y2;   xz := x * z2;
		yy := y * y2;   yz := y * z2;   zz := z * z2;
		wx := w * x2;   wy := w * y2;   wz := w * z2;

		result := Matrix3.Rows(
			1 - (yy + zz),       xy - wz,      xz + wy,
					xy + wz, 1 - (xx + zz),      yz - wx,
					xz - wy,       yz + wx, 1 - (xx + yy));
	end;

	procedure Quaternion.GetRotation(out angle: float; out axis: Vec3);
	var
		halfAngle: float;
	begin
		// Q = V*sin(alpha/2); cos(alpha/2)
		halfAngle := ArcCos(w);
		if halfAngle <> 0 then
			axis := v3 * (1 / sin(halfAngle))
		else
			axis := v3.Normalized;
		angle := 2 * halfAngle;
		if angle > Pi then angle -= TwoPi;
	end;

	function Quaternion.GetAngle: float;
	var
		axis: Vec3;
	begin
		GetRotation(result, axis);
	end;

	function Quaternion.GetAngleXZ: float;
	var
		xzt: Vec3;
	begin
		xzt := self * Vec3.PositiveZ;
		result := Angle(Vec2.PositiveY, Vec2.Make(xzt.x, xzt.z));
	end;

	function Quaternion.GetAxis: Vec3;
	var
		angle: float;
	begin
		GetRotation(angle, result);
	end;

	function Quaternion.Transformed100: Vec3; begin result := 2 * Vec3.Make(-y*y - z*z + 0.5, z*w + x*y, x*z - y*w); end;

	function Quaternion.ToEuler: Vec3;
	const
		Threshold = 0.5 - CloseToZeroEps;
	var
		xy, zw, test, xx, xz, xw, yy, yw, yz, zz: float;
	begin
		xy := X * Y;
		zw := Z * W;
		test := xy + zw;

		if test >= Threshold then result := Vec3.Make(2 * ArcTan2(x, w), HalfPi, 0) else
			if test <= -Threshold then result := Vec3.Make(-2 * ArcTan2(x, w), -HalfPi, 0) else
			begin
				xx := sqr(x);
				xz := x * z;
				xw := x * w;

				yy := sqr(y);
				yw := y * w;
				yz := y * z;

				zz := sqr(z);

				result.x := ArcTan2(2*(yw - xz), 1 - 2*(yy + zz));
				result.y := ArcTan2(2*(xw - yz), 1 - 2*(xx + zz));
				result.z := ArcSin(2 * test);
			end;
	end;

	function SameRotation(const p, q: Quaternion; const eps: float = QuatEps): boolean;
	begin
		result := Equals(p.v4, q.v4, eps) or Equals(p.v4, -q.v4, eps);
	end;

	function LimitByCone(const v: Vec3; const coneAxis: Vec3; const coneHalfAngle: float): Vec3;
	var
		an: float;
	begin
		an := AngleN(v, coneAxis);
		if LessThanEqual(an, coneHalfAngle) then
			result := v
		else
			result := Quaternion.Rotation(an - coneHalfAngle, (v >< coneAxis).Normalized) * v;
	end;

	function AlignToGravity(const axis, up, gravity: Vec3): Quaternion;
	var
		plane: Vec3 absolute axis;
		a, b: Vec3;
	begin
		a := plane >< up >< plane;
		b := plane >< gravity >< plane;
		result := Quaternion.Rotation(AngleUN(a, b) * FloatSign(Matrix3.Columns(up, gravity, axis).Determinant), axis);
	end;

	operator **(const a, b: Quaternion): float; begin result := a.v4 ** b.v4; end;
	operator =(const a, b: Quaternion): boolean; begin result := (a.x = b.x) and (a.y = b.y) and (a.z = b.z) and (a.w = b.w); end;

	operator *(const q1, q2: Quaternion): Quaternion;
	var
		xx, yy, zz, ww, qq: float;
	begin
		// http://ariya.blogspot.com/2010/07/faster-quaternion-multiplication.html — меньше умножений, больше сложений
		ww := (q1.z + q1.x) * (q2.x + q2.y);
		yy := (q1.w - q1.y) * (q2.w + q2.z);
		zz := (q1.w + q1.y) * (q2.w - q2.z);
		xx := ww + yy + zz;
		qq := 0.5 * (xx + (q1.z - q1.x) * (q2.x - q2.y));

		result := Quaternion.Make(
			qq - xx + (q1.x + q1.w) * (q2.x + q2.w),
			qq - yy + (q1.w - q1.x) * (q2.y + q2.z),
			qq - zz + (q1.z + q1.y) * (q2.w - q2.x),
			qq - ww + (q1.z - q1.y) * (q2.y - q2.z));
	end;

	operator *(const q: Quaternion; const x: float): Quaternion;
	begin
		result.v4 := q.v4 * x;
	end;

	operator *(const x: float; const q: Quaternion): Quaternion; begin result := q * x; end;

	operator *(const q: Quaternion; const v: Vec3): Vec3;
	begin
		// result := Vec3.Make((q * Make(v, 0) * q.Inversed).v4);
		result := v + 2 * (q.v3 >< ((q.v3 >< v) + q.w * v));
	end;

	operator +(const q1, q2: Quaternion): Quaternion;
	begin
		result.v4 := q1.v4 + q2.v4;
	end;

	operator -(const q1, q2: Quaternion): Quaternion;
	begin
		result.v4 := q1.v4 - q2.v4;
	end;

	operator -(const q: Quaternion): Quaternion;
	begin
		result.v4 := -q.v4;
	end;

	operator /(const q: Quaternion; const x: float): Quaternion;
	begin
		result.v4 := q.v4 / x;
	end;

	function Transform.FromMatrix(const mat: Matrix4): Transform;
	begin
		result.rot := Quaternion.FromMatrix(Mat3(mat));
		result.tr := Vec3.Make(mat.m[3, 0], mat.m[3, 1], mat.m[3, 2]);
		result.scale := mat.Determinant; // для неоднородного масштабирования коэффициентами будут длины базисных векторов, т. е. столбцов
	end;

	function Transform.ToMatrix: Matrix4;
	begin
		result := Matrix4.Translation(tr) * rot.ToMatrix;
		if scale <> 1 then
			result := result * Matrix4.Scaling(scale, scale, scale);
	end;

	function Transform.ToMatrixWoScale: Matrix4;
	begin
		result := Matrix4.Translation(tr) * rot.ToMatrix;
	end;

	function Transform.Inversed: Transform;
	begin
		result.rot := rot.Inversed;
		result.tr := result.rot*-tr;
		if scale <> 0 then
		begin
			result.scale := 1 / scale;
			result.tr *= result.scale;
		end else result.scale := 1;
	end;

	function Translate(const tr: Vec3): Transform;
	begin
		result.tr := tr;
		result.rot := Quaternion.Identity;
		result.scale := 1;
	end;

	function Translate(const x, y, z: float): Transform; begin result := Translate(Vec3.Make(x, y, z)); end;

	function Rotate(const rot: Quaternion): Transform;
	begin
		result.tr := Vec3.Zero;
		result.rot := rot;
		result.scale := 1;
	end;

	function Rotate(const angle: float; const axis: Vec3): Transform;
	begin
		result := Rotate(Quaternion.Rotation(angle, axis));
	end;

	function TranslateRotate(const tr: Vec3; const rot: Quaternion): Transform;
	begin
		result.tr := tr;
		result.rot := rot;
		result.scale := 1;
	end;

	function RotateTranslate(const rot: Quaternion; const tr: Vec3): Transform;
	begin
		result.tr := rot * tr;
		result.rot := rot;
		result.scale := 1;
	end;

	function Scale(const factor: float): Transform;
	begin
		result.tr := Vec3.Zero;
		result.rot := Quaternion.Identity;
		result.scale := factor;
	end;

	function Equals(const a, b: Transform; const eps: float): boolean;
	begin
		result := Equals(a.tr, b.tr, eps) and SameRotation(a.rot, b.rot, eps) and Equals(a.scale, b.scale, eps);
	end;

	operator =(const a, b: Transform): boolean;
	begin
		result := (a.tr = b.tr) and (a.rot = b.rot) and (a.scale = b.scale);
	end;

	operator *(const a, b: Transform): Transform;
	begin // result := ByMatrix(a.ToMatrix * b.ToMatrix);
		result.tr := a * b.tr;
		result.rot := a.rot * b.rot;
		result.scale := a.scale * b.scale;
	end;

	operator *(const t: Transform; const v: Vec3): Vec3;
	begin //result := (t.ToMatrix * Vec4.Make(v, 1)).v3;
		if t.scale = 1 then
			result := t.tr + t.rot * v
		else
			result := t.tr + t.rot * v * t.scale;
	end;

	operator =(const a, b: Transform2): boolean;
	begin
		result := (a.trans = b.trans) and (a.rot = b.rot) and (a.scale = b.scale);
	end;

	operator *(const a, b: Transform2): Transform2;
	begin
		result.trans := a * b.trans;
		result.rot := a.rot + b.rot;
		result.scale := a.scale * b.scale;
	end;

	operator *(const t: Transform2; const v: Vec2): Vec2;
	begin
		if t.scale = 1 then
			result := t.trans + Rotate(v, t.rot)
		else
			result := t.trans + Rotate(v, t.rot) * t.scale;
	end;

	function Translate2(const trans: Vec2): Transform2;
	begin
		result.trans := trans;
		result.rot := 0;
		result.scale := 1;
	end;

	function Translate2(const x, y: float): Transform2; begin result := Translate2(Vec2.Make(x, y)); end;

	function Rotate2(const rot: float): Transform2;
	begin
		result.trans := Vec2.Zero;
		result.rot := rot;
		result.scale := 1;
	end;

	function Scale2(const scale: float): Transform2;
	begin
		result.trans := Vec2.Zero;
		result.rot := 0;
		result.scale := scale;
	end;

	function TranslateRotate2(const trans: Vec2; const rot: float): Transform2;
	begin
		result.trans := trans;
		result.rot := rot;
		result.scale := 1;
	end;

	function RotateTranslate2(const rot: float; const trans: Vec2): Transform2;
	begin
		result.trans := UMath.Rotate(trans, rot);
		result.rot := rot;
		result.scale := 1;
	end;

	function Plane.Make(const p1, p2, p3: Vec3): Plane;
	var
		n: Vec3;
	begin
		n := (p2 - p1) >< (p3 - p1);
		if NotZero(n.Length) then
			result := Plane.Make(p1, n.Normalized)
		else
			result := Plane.None;
	end;

	function Plane.Make(const point: Vec3; const normal: Vec3): Plane;
	begin
		// normal (A, B, C)
		// point (x0, y0, z0)
		// then A(x - x0) + B(y - y0) + C(z - z0) = 0
		result.a := normal.x;
		result.b := normal.y;
		result.c := normal.z;
		result.d := -(result.a * point.x + result.b * point.y + result.c * point.z);
	end;

	function Plane.Make(const a, b, c, d: float): Plane;
	begin
		result.v4 := Vec4.Make(a, b, c, d);
	end;

	function Plane.Make(const coefs: Vec4): Plane;
	begin
		result.v4 := coefs;
	end;

	function Plane.Normalized: Plane;
	var
		len: float;
	begin
		len := v3.SqrLength;
		if Equals(len, 1) or IsZero(len) then
			result := self
		else
			result.v4 := v4 / sqrt(len);
	end;

	function Plane.ReflectPoint(const point: Vec3): Vec3;
	begin
		result := point - 2 * normal * (point - Vec3.Make(d));
	end;
	function Plane.ReflectVector(const vec: Vec3): Vec3;
	begin
		result := UMath.Reflect(vec, normal);
	end;

	function Plane.Reflect(const q: Quaternion): Quaternion;
	begin
		result.v3 := UMath.Reflect(-q.v3, normal);
		result.w := q.w;
	end;

	function Plane.Reflect(const p: Plane): Plane; begin result := Make(ReflectPoint(p.AnyPoint), ReflectVector(p.normal)); end;
	function Plane.SignedDistance(const pt: Vec3): float; begin result := v3 ** pt + d; end;
	function Plane.SignedDistance(const x, y, z: float): float; begin result := a * x + b * y + c * z + d;; end;
	function Plane.Distance(const pt: Vec3): float; begin result := abs(SignedDistance(pt)); end;
	function Plane.Project(const p: Vec3): Vec3; begin result := p - v3 * SignedDistance(p); end;
	function Plane.AnyPoint: Vec3; begin result := normal * -d; end;

	function Plane.Reversed: Plane;
	begin
		result.v4 := -self.v4;
	end;

	function Plane.ToBasis(const o: Vec3; const basis: Matrix3): Plane;
	begin
		result := Plane.Make(basis.Inversed * (AnyPoint - o), basis.Transposed * normal);
	end;

	function Plane.IntersectLine(const p, n: Vec3): Vec3;
	var
		num, den: float;
	begin
		den := n ** normal;
		if IsZero(den) then exit(p); // на самом деле это значит параллельность линии и плоскости, или совпадение (num = 0) как частный случай

		num := (AnyPoint - p) ** normal;
		result := p + n * (num / den);
	end;

	operator *(const t: Transform; const p: Plane): Plane; begin result := Plane.Make(t * p.AnyPoint, t.rot * p.normal); end;

	function Frustum.FromMatrix(const mvp: Matrix4): Frustum;
	var
		m: Matrix4.LinearData absolute mvp.data.l;
	begin
		result. RightPlane := Plane.Make(m[3] - m[0], m[7] - m[4], m[11] - m[8], m[15] - m[12]).Normalized;
		result.  LeftPlane := Plane.Make(m[3] + m[0], m[7] + m[4], m[11] + m[8], m[15] + m[12]).Normalized;
		result.   TopPlane := Plane.Make(m[3] - m[1], m[7] - m[5], m[11] - m[9], m[15] - m[13]).Normalized;
		result.BottomPlane := Plane.Make(m[3] + m[1], m[7] + m[5], m[11] + m[9], m[15] + m[13]).Normalized;
		result.   FarPlane := Plane.Make(m[3] - m[2], m[7] - m[6], m[11] - m[10], m[15] - m[14]).Normalized;
		result.  NearPlane := Plane.Make(m[3] + m[2], m[7] + m[6], m[11] + m[10], m[15] + m[14]).Normalized;
	end;

	function Frustum.TestPoint(const point: Vec3): boolean;
	begin
		result :=
			( RightPlane.SignedDistance(point) > 0) and
			(  LeftPlane.SignedDistance(point) > 0) and
			(   TopPlane.SignedDistance(point) > 0) and
			(BottomPlane.SignedDistance(point) > 0) and
			(  NearPlane.SignedDistance(point) > 0) and
			(   FarPlane.SignedDistance(point) > 0);
	end;

	function Frustum.TestSphere(const center: Vec3; const radius: float): boolean;
	begin
		result :=
			( RightPlane.SignedDistance(center) > -radius) and
			(  LeftPlane.SignedDistance(center) > -radius) and
			(   TopPlane.SignedDistance(center) > -radius) and
			(BottomPlane.SignedDistance(center) > -radius) and
			(  NearPlane.SignedDistance(center) > -radius) and
			(   FarPlane.SignedDistance(center) > -radius);
	end;

	function Frustum.TestSphere(const sph: Sphere): boolean;
	begin
		result := TestSphere(sph.center, sph.radius);
	end;

	function Frustum.ClassifySphere(const sph: Sphere): BoundingRelation;
	var
		plane: PlaneEnum;
		d: float;
	begin
		result := bb_FullyIn;
		for plane in PlaneEnum do
		begin
			d := planes[plane].SignedDistance(sph.center);
			if d <= -sph.radius then exit(bb_FullyOut) else
				if d < sph.radius then result := bb_Intersects;
		end;
	end;

	function Frustum.TestAABB(const bb: AABB): boolean;
	begin
		result :=
			(RightPlane .SignedDistance(bb.SupportVertex(RightPlane .Normal)) > 0) and
			(LeftPlane  .SignedDistance(bb.SupportVertex(LeftPlane  .Normal)) > 0) and
			(TopPlane   .SignedDistance(bb.SupportVertex(TopPlane   .Normal)) > 0) and
			(BottomPlane.SignedDistance(bb.SupportVertex(BottomPlane.Normal)) > 0) and
			(NearPlane  .SignedDistance(bb.SupportVertex(NearPlane  .Normal)) > 0) and
			(FarPlane   .SignedDistance(bb.SupportVertex(FarPlane   .Normal)) > 0);
	end;

	function Frustum.GetEightPoints: EightPoints;
	begin
		result[0] := PlanesIntersection(RightPlane, TopPlane, NearPlane);
		result[1] := PlanesIntersection(RightPlane, TopPlane, FarPlane);
		result[2] := PlanesIntersection(RightPlane, BottomPlane, NearPlane);
		result[3] := PlanesIntersection(RightPlane, BottomPlane, FarPlane);
		result[4] := PlanesIntersection(LeftPlane, TopPlane, NearPlane);
		result[5] := PlanesIntersection(LeftPlane, TopPlane, FarPlane);
		result[6] := PlanesIntersection(LeftPlane, BottomPlane, NearPlane);
		result[7] := PlanesIntersection(LeftPlane, BottomPlane, FarPlane);
	end;

	function AABB.Make(const p1, p2: Vec3): AABB;
	begin
		result.A := p1;
		result.B := p2;
	end;

	function AABB.Make(const pt: Vec3): AABB; begin result := AABB.Make(pt, pt); end;

	function AABB.Make(const sph: Sphere): AABB;
	var
		rs: Vec3;
	begin
		rs := Vec3.Make(sph.radius);
		result := AABB.Make(sph.center - rs, sph.center + rs);
	end;

	function AABB.Undefined: AABB;
	const
		InfA: Vec3 = (data: (Infinity, Infinity, Infinity));
		InfB: Vec3 = (data: (NegInfinity, NegInfinity, NegInfinity));
	begin
		result.A := InfA;
		result.B := InfB;
	end;

	function AABB.Intersects(const b1, b2: AABB): boolean;
	begin
		result := (RangeIntersects(b1.A.x, b1.B.x, b2.A.x, b2.B.x)) and
		          (RangeIntersects(b1.A.y, b1.B.y, b2.A.y, b2.B.y)) and
		          (RangeIntersects(b1.A.z, b1.B.z, b2.A.z, b2.B.z));
	end;

	function AABB.Intersection(const b1, b2: AABB): AABB;
	begin
		result.A := max(b1.A, b2.A);
		result.B := min(b1.B, b2.B);
	end;

	function AABB.Bound(const p: array of Vec3): AABB;
	var
		i: sint;
	begin
		Assert(length(p) > 0);
		result := AABB.Make(p[0]);
		for i := 1 to High(p) do
			result.Enlarge(p[i]);
	end;

	function AABB.Bound(const spheres: array of Sphere): AABB;
	var
		i: sint;
	begin
		Assert(length(spheres) > 0);
		result := AABB.Make(spheres[0]);
		for i := 1 to High(spheres) do
			result.Enlarge(spheres[i].center, spheres[i].radius);
	end;

	function AABB.IsUndefined: boolean; begin result := A.x = Infinity; end;

	procedure AABB.Enlarge(const p: array of Vec3);
	var
		i: sint;
	begin
		for i := 0 to High(p) do
			Enlarge(p[i]);
	end;

	function AABB.Enlarge(const bb: AABB): boolean;
	begin
		result := no;
		if bb.A.x < A.x then begin A.x := bb.A.x; result := yes; end;
		if bb.A.y < A.y then begin A.y := bb.A.y; result := yes; end;
		if bb.A.z < A.z then begin A.z := bb.A.z; result := yes; end;
		if bb.B.x > B.x then begin B.x := bb.B.x; result := yes; end;
		if bb.B.y > B.y then begin B.y := bb.B.y; result := yes; end;
		if bb.B.z > B.z then begin B.z := bb.B.z; result := yes; end;
	end;

	function AABB.Enlarge(const pt: Vec3): boolean;
	begin
		result := no;
		if pt.x < A.x then begin A.x := pt.x; result := yes; end;
		if pt.y < A.y then begin A.y := pt.y; result := yes; end;
		if pt.z < A.z then begin A.z := pt.z; result := yes; end;
		if pt.x > B.x then begin B.x := pt.x; result := yes; end;
		if pt.y > B.y then begin B.y := pt.y; result := yes; end;
		if pt.z > B.z then begin B.z := pt.z; result := yes; end;
	end;

	function AABB.Enlarge(const c: Vec3; const radius: float): boolean;
	begin
		result := no;
		if c.x - radius < A.x then begin A.x := c.x - radius; result := yes; end;
		if c.y - radius < A.y then begin A.y := c.y - radius; result := yes; end;
		if c.z - radius < A.z then begin A.z := c.z - radius; result := yes; end;
		if c.x + radius > B.x then begin B.x := c.x + radius; result := yes; end;
		if c.y + radius > B.y then begin B.y := c.y + radius; result := yes; end;
		if c.z + radius > B.z then begin B.z := c.z + radius; result := yes; end;
	end;

	procedure AABB.Grow(const factor, bias: float);
	var
		mid, sz, d: float;
		i: sint;
	begin
		if factor <> 1 then
			for i := 0 to 2 do
			begin
				sz := B.data[i] - A.data[i];
				if NotZero(sz) then
				begin
					mid := 0.5 * (A.data[i] + B.data[i]);
					d := 0.5 * factor * sz;
					A.data[i] := mid - d;
					B.data[i] := mid + d;
				end;
			end;

		if bias <> 0 then
			for i := 0 to 2 do
			begin
				A.data[i] := A.data[i] - bias;
				B.data[i] := B.data[i] + bias;
			end;
	end;

	function AABB.Contains(const pt: Vec3): boolean;
	begin
		result := GreaterThanEqual(pt.x, A.x) and LessThanEqual(pt.x, B.x) and
			GreaterThanEqual(pt.y, A.y) and LessThanEqual(pt.y, B.y) and
			GreaterThanEqual(pt.z, A.z) and LessThanEqual(pt.z, B.z);
	end;

	procedure AABB.ClampToSphere(const sph: Sphere);
	var
		rs: Vec3;
	begin
		rs := Vec3.Make(sph.radius);
		A := max(A, sph.center - rs);
		B := min(B, sph.center + rs);
	end;

	function AABB.GetEightPoints: EightPoints;
	begin
		result[0] := Vec3.Make(A.x, A.y, A.z);
		result[1] := Vec3.Make(A.x, A.y, B.z);
		result[2] := Vec3.Make(A.x, B.y, A.z);
		result[3] := Vec3.Make(A.x, B.y, B.z);
		result[4] := Vec3.Make(B.x, A.y, A.z);
		result[5] := Vec3.Make(B.x, A.y, B.z);
		result[6] := Vec3.Make(B.x, B.y, A.z);
		result[7] := Vec3.Make(B.x, B.y, B.z);
	end;

	function AABB.SizeX: float; begin result := B.x - A.x; end;
	function AABB.SizeY: float; begin result := B.y - A.y; end;
	function AABB.SizeZ: float; begin result := B.z - A.z; end;
	function AABB.Size(dim: sint): float; begin result := B.data[dim] - A.data[dim]; end;
	function AABB.Sizes: Vec3; begin result := B - A; end;
	function AABB.Volume: float; begin result := Sizes.Product; end;
	function AABB.Center: Vec3; begin result := 0.5 * (A + B); end;

	function AABB.SupportVertex(const d: Vec3): Vec3;
	begin
		if d.x > 0 then result.x := B.x else result.x := A.x;
		if d.y > 0 then result.y := B.y else result.y := A.y;
		if d.z > 0 then result.z := B.z else result.z := A.z;
	end;

	operator *(const t: Transform; const bb: AABB): AABB;
	begin
		result := AABB.Make(t * bb.A);
		result.Enlarge(t * Vec3.Make(bb.A.x, bb.A.y, bb.B.z));
		result.Enlarge(t * Vec3.Make(bb.A.x, bb.B.y, bb.A.z));
		result.Enlarge(t * Vec3.Make(bb.A.x, bb.B.y, bb.B.z));
		result.Enlarge(t * bb.B);
		result.Enlarge(t * Vec3.Make(bb.B.x, bb.B.y, bb.A.z));
		result.Enlarge(t * Vec3.Make(bb.B.x, bb.A.y, bb.B.z));
		result.Enlarge(t * Vec3.Make(bb.B.x, bb.A.y, bb.A.z));
	end;

	function Sphere.Make(const newCenter: Vec3; const newRadius: float): Sphere;
	begin
		result.center := newCenter;
		result.radius := newRadius;
	end;

	function Sphere.Intersects(const a, b: Sphere): boolean;
	begin
		result := SqrDistance(a.center, b.center) <= sqr(a.radius + b.radius);
	end;

	function Sphere.Bound(const spheres: array of Sphere): Sphere;
	var
		i, j, bestI, bestJ: sint;
		curD, curDFull, bestD, bestDFull: float;
		curConsumed, bestConsumed: boolean;
		v: Vec3;
	begin
		Assert(length(spheres) > 0);
		if length(spheres) = 1 then exit(spheres[0]);
		bestI := -1;
		bestDFull := NaN; // calm down compiler

		// TODO: квадрат? а побыстрее?
		for i := 0 to High(spheres) - 1 do
			for j := i + 1 to High(spheres) do
			begin
				curD := Distance(spheres[i].center, spheres[j].center);
				curConsumed := LessThanEqual(curD, abs(spheres[i].radius - spheres[j].radius));
				if curConsumed then
					curDFull := 2 * max(spheres[i].radius, spheres[j].radius)
				else
					curDFull := curD + spheres[i].radius + spheres[j].radius;
				if (bestI < 0) or (curDFull > bestDFull) then
				begin
					bestD := curD;
					bestDFull := curDFull;
					bestConsumed := curConsumed;
					bestI := i;
					bestJ := j;
				end;
			end;
		Assert(bestI >= 0);

		if not bestConsumed then
		begin
			Assert(NotZero(bestD));
			v := (spheres[bestJ].center - spheres[bestI].center) / bestD;
			result := Sphere.Make(
				0.5 * (spheres[bestJ].center + spheres[bestI].center + (spheres[bestJ].radius - spheres[bestI].radius) * v),
				0.5 * bestDFull);
		end else
			if spheres[bestI].radius > spheres[bestJ].radius then
				result := spheres[bestI]
			else
				result := spheres[bestJ];
	end;

	function Sphere.Contains(const v: Vec3): boolean; begin result := SqrDistance(v, center) <= sqr(radius); end;

	function Sphere.Enlarge(const pt: Vec3): boolean;
	var
		d: float;
		v: Vec3;
	begin
		d := SqrDistance(center, pt);
		result := d > sqr(radius);
		if not result then exit;

		d := sqrt(d);
		if NotZero(d) then
			v := (pt - center) / d
		else
			v := Vec3.PositiveX;
		center := 0.5 * (center + pt - v * radius);
		radius := 0.5 * (radius + d);
	end;

	function Sphere.Enlarge(const c2: Vec3; const r2: float): boolean;
	var
		dc: float;
		v: Vec3;
	begin
		dc := Distance(center, c2);
		result := dc + r2 > radius;
		if not result then exit;

		if NotZero(dc) then
			v := (c2 - center) / dc
		else
			v := Vec3.PositiveX;
		center := 0.5 * (center + c2 + v * (r2 - radius));
		radius := 0.5 * (radius + r2 + dc);
	end;

	function Rect.Make(const x1, y1, x2, y2: float): Rect;
	begin
		result := Make(Vec2.Make(x1, y1), Vec2.Make(x2, y2));
	end;

	function Rect.Make(const a, b: Vec2): Rect;
	begin
		result.A := a;
		result.B := b;
	end;

	function Rect.Make(const v: Vec4): Rect;
	begin
		result := Make(v.x, v.y, v.z, v.w);
	end;

	function Rect.SizeX: float; begin result := B.x - A.x; end;
	function Rect.SizeY: float; begin result := B.y - A.y; end;
	function Rect.Size(dim: uint): float; begin result := B.data[dim] - A.data[dim]; end;
	function Rect.Size: Vec2; begin result := B - A; end;

	function Rect.Aspect: float;
	begin
		if SizeY <> 0 then
			result := SizeX / SizeY
		else
			result := 1;
	end;

	function Rect.Square: float; begin result := SizeX * SizeY; end;

	procedure Rect.Enlarge(const r: Rect);
	begin
		A.x := min(A.x, r.A.x);
		A.y := min(A.y, r.A.y);
		B.x := max(B.x, r.B.x);
		B.y := max(B.y, r.B.y);
	end;

	function Rect.Contains(const pt: Vec2): boolean; begin result := (pt.x >= A.x) and (pt.x <= B.x) and (pt.y >= A.y) and (pt.y <= B.y); end;
	function Rect.Corporeal: boolean; begin result := (SizeX >= CloseToZeroEps) and (SizeY >= CloseToZeroEps); end;

	function Rect.Intersection(const second: Rect): Rect;
	begin
		result.A := max(A, second.A);
		result.B := min(B, second.B);
	end;

	function Rect.AsVec4: Vec4;
	begin
		result := Vec4.Make(A, B);
	end;

	function Rect.Subdivision.GetRect(id: uint): Rect;
	begin
		Assert(id < uint(n));
		result := (pRect(@mem) + id)^;
	end;

	procedure Rect.Subdivision.TryAdd(const ax, ay, bx, by: float);
	var
		r: pRect;
	begin
		if (ax < bx) and (ay < by) then
		begin
			r := pRect(@mem);
			r[n] := Rect.Make(ax, ay, bx, by);
			inc(n);
		end;
	end;

	function Rect.Subdivide(const divisor: Rect): Subdivision;
	var
		d: Rect absolute divisor;
	{$ifdef Debug} sent: Sentinel; {$endif}
	begin
		result.n := 0;
	{$ifdef Debug} sent := Sentinel.Setup(@result.mem, sizeof(result.mem)); {$endif}
		// ┌
		// │ │ D
		// └
		if (A.x < d.A.x) and (B.x > d.A.x) then
			result.TryAdd(A.x, max(A.y, d.A.y), d.A.x, min(B.y, d.B.y));
		// ┌─────┐
		//   ───
		//    D
		if (A.y < d.A.y) and (B.y > d.A.y) then
			result.TryAdd(max(A.x, d.A.x), A.y, min(B.x, d.B.x), d.A.y);
		//     ┐
		// D │ │
		//     ┘
		if (A.x < d.B.x) and (B.x > d.B.x) then
			result.TryAdd(d.B.x, max(A.y, d.A.y), B.x, min(B.y, d.B.y));
		//    D
		//   ───
		// └─────┘
		if (A.y < d.B.y) and (B.y > d.B.y) then
			result.TryAdd(max(A.x, d.A.x), d.B.y, min(B.x, d.B.x), B.y);
		// ┌────
		// │ ┌───
		// │ │ D
		if (A.x < d.A.x) and (B.x > d.A.x) and (A.y < d.A.y) and (B.y > d.A.y) then
			result.TryAdd(A.x, A.y, d.A.x, d.A.y);
		//  ────┐
		// ───┐ │
		//  D │ │
		if (A.x < d.B.x) and (B.x > d.B.x) and (A.y < d.A.y) and (B.y > d.A.y) then
			result.TryAdd(d.B.x, A.y, B.x, d.A.y);
		// │ │ D
		// │ └───
		// └────
		if (A.x < d.A.x) and (B.x > d.A.x) and (A.y < d.B.y) and (B.y > d.B.y) then
			result.TryAdd(A.x, d.B.y, d.A.x, B.y);
		//  D │ │
		// ───┘ │
		//  ────┘
		if (A.x < d.B.x) and (B.x > d.B.x) and (A.y < d.B.y) and (B.y > d.B.y) then
			result.TryAdd(d.B.x, d.B.y, B.x, B.y);
		// делитель вне прямоугольника
		if (result.n = 0) and ((B.x <= d.A.x) or (A.x >= d.B.x) or (B.y <= d.A.y) or (A.y >= d.B.y)) then
			result.TryAdd(A.x, A.y, B.x, B.y);
	{$ifdef Debug} sent.Verify(pointer(@result.mem[0]) + result.n * sizeof(Rect), sizeof(result.mem) - result.n * sizeof(Rect), 'Rect.Subdivision'); {$endif}
	end;

	operator =(const a, b: Rect): boolean;
	begin
		result := (a.A = b.A) and (a.B = b.B);
	end;

	operator +(const rect: Rect; const v: Vec2): Rect;
	begin
		result.A := rect.A + v;
		result.B := rect.B + v;
	end;

	function Equals(const a, b: Rect; const eps: float = CloseToZeroEps): boolean;
	begin
		result := Equals(a.A, b.A, eps) and Equals(a.B, b.B, eps);
	end;

	function RectsIntersection(const a, b: Rect): Rect;
	begin
		result.A.x := max(a.A.x, b.A.x);
		result.A.y := max(a.A.y, b.A.y);
		result.B.x := min(a.B.x, b.B.x);
		result.B.y := min(a.B.y, b.B.y);
	end;

	function PlanesIntersection(const a, b, c: Plane): Vec3;
	var
		m: Matrix3;
	begin
		m.data.m[0, 0] := a.a; m.data.m[1, 0] := a.b; m.data.m[2, 0] := a.c;
		m.data.m[0, 1] := b.a; m.data.m[1, 1] := b.b; m.data.m[2, 1] := b.c;
		m.data.m[0, 2] := c.a; m.data.m[1, 2] := c.b; m.data.m[2, 2] := c.c;
		result := m.Inversed * Vec3.Make(-a.d, -b.d, -c.d);
	end;

	function UintRect.Make(const pos, size: UintVec2): UintRect;
	begin
		result.pos := pos;
		result.size := size;
	end;

	function UintRect.Contains(const point: UintVec2): boolean; begin result := pos.FitsClosed(point) and point.FitsOpen(pos + size); end;
	function UintRect.Square: uint; begin result := size.Product; end;

	function lerp(const a, b: float; const t: float): float;
	begin
		result := a + t*(b - a);
	end;

	function lerp(const p: Quaternion; q: Quaternion; const x: float): Quaternion;
	begin
		if p ** q < 0 then q := -q;
		result := raw_lerp(p, q, x);
	end;

	function raw_lerp(const p, q: Quaternion; const x: float): Quaternion;
	begin
		result := q*x + p*(1 - x);
	end;

	function slerp(const p: Quaternion; q: Quaternion; const x: float): Quaternion;
	var
		omega, sinom, cosom: float;
	begin
		cosom := p ** q;
		if cosom < 0 then
		begin
			cosom := -cosom;
			q := -q;
		end;

		omega := ArcCos(cosom);
		if IsZero(omega) then result := lerp(p, q, x) else
		begin
			sinom := sqrt(1 - sqr(cosom));
			result := (p * sin((1 - x) * omega) + q * sin(x * omega)) / sinom
		end;
	end;

	function CatmullRomSpline(const p0, p1, p2, p3: float; const t: float): float;
	begin
		result := 0.5 * ((2.0 * p1) + ((p2 - p0) + ((2.0 * p0 - 5.0 * p1 + 4.0 * p2 - p3) + (3.0 * p1 - 3.0 * p2 + p3 - p0) * t) * t) * t);
	end;

	function CatmullRomSpline(const p0, p1, p2, p3: Quaternion; const t: float): Quaternion;
	begin
		result := Quaternion(CatmullRomSpline(p0.v4, p1.v4, p2.v4, p3.v4, t)).MaybeNormalized;
	end;

	function BSpline(const p0, p1, p2, p3: float; const t: float): float;
	begin
		result := (p0 + 4.0 * p1 + p2) * (1/6) + (0.5 * (p2 - p0) + (0.5 * (p0 - 2*p1 + p2) + (0.5 * (p1 - p2) + (p3 - p0) * (1/6)) * t) * t) * t;
	end;

{$define vecf :=
	function lerp(const a, b: vec; const t: float): vec; {$define op := lerp(a.item, b.item, t)} vec_compo_op
	function CatmullRomSpline(const p0, p1, p2, p3: vec; const t: float): vec; {$define op := CatmullRomSpline(p0.item, p1.item, p2.item, p3.item, t)} vec_compo_op
	function BSpline(const p0, p1, p2, p3: vec; const t: float): vec; {$define op := BSpline(p0.item, p1.item, p2.item, p3.item, t)} vec_compo_op}
	all_floating_vectors

	function SolveLinear(const a, b: hp_float; out x: hp_float): boolean;
	begin
		if NotZero(a) then
		begin
			x := -b / a;
			result := yes;
		end else
			if NotZero(b) then
				result := no
			else
			begin
				x := 0.0;
				result := yes;
			end;
	end;

	function SolveQuadratic(const a, b, c: hp_float; out x1, x2: hp_float): boolean;
	var
		D, itwoa: hp_float;
	begin
		if NotZero(a) then
		begin
			D := sqr(b) - 4*a*c;
			if GreaterThan(D, 0.0) then
			begin
				D := sqrt(D);
				itwoa := 1.0 / (2*a);
				x1 := (-b + D) * itwoa;
				x2 := (-b - D) * itwoa;
				result := yes;
			end else
				if IsZero(D) then
				begin
					x1 := -b / (2*a);
					x2 := x1;
					result := yes;
				end else
					result := no;
		end else
		begin
			result := SolveLinear(b, c, x1);
			if result then x2 := x1;
		end;
	end;

end.
