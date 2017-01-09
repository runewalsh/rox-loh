unit SpatialIndex;

{$include opts.inc}

interface

uses
	USystem, UMath, Utils;

const
	MaxKdDepth = 48;

type
	pBounding = ^Bounding;
	Bounding = object
	type
		BaseEnum = (UnknownBase, BasedOnSphere, BasedOnAABB);
	private
		_base: BaseEnum;
		_sphere: Sphere;
		_aabb: AABB;
		function Make(newBase: BaseEnum; const newSphere: Sphere; const newAabb: AABB): Bounding; static;
	public
		function ByPoint(const pt: Vec3): Bounding; static;
		function BySphere(const center: Vec3; const radius: float): Bounding; static;
		function BySphere(const newSph: Sphere): Bounding; static;
		function ByAABB(const a, b: Vec3): Bounding; static;
		function ByAABB(const newAabb: AABB): Bounding; static;
		function BySphereAndAABB(const newSph: Sphere; const newAabb: AABB): Bounding; static;
		function Enlarge(const bnd: Bounding): boolean;
		function Enlarge(const pt: Vec3): boolean;
		function Enlarge(const c2: Vec3; const r2: float): boolean;
		function Contains(const pt: Vec3): boolean;
		function Sizes: Vec3;
		function Intersects(const a, b: Bounding): boolean; static;
		function IsUndefined: boolean;

		property Center: Vec3 read _sphere.center;
		property Radius: float read _sphere.radius;
		property Base: BaseEnum read _base;
		property Sphere: Sphere read _sphere;
		property AABB: AABB read _aabb;
	const
		Undefined: Bounding =
		(
			_base: UnknownBase;
			_sphere:
			(
				center: (data: (NaN, NaN, NaN));
				radius: NaN
			);
			_aabb:
			(
				A: (data: (NaN, NaN, NaN));
				B: (data: (NaN, NaN, NaN));
			)
		);
	end;

	operator =(const a, b: Bounding): boolean;
	operator *(const t: Transform; const bnd: Bounding): Bounding;
	function ToString(const bnd: Bounding): string; overload;

type
	pMaterialID = ^tMaterialID;
	tMaterialID = type byte;

const
	MaterialID_Transparent = 101;

implementation

	function Bounding.Make(newBase: BaseEnum; const newSphere: Sphere; const newAabb: AABB): Bounding;
	begin
		result._base   := newBase;
		result._sphere := newSphere;
		result._aabb   := newAabb;
	end;

	function Bounding.ByPoint(const pt: Vec3): Bounding;
	begin
		result := Make(BasedOnAABB, UMath.Sphere.Make(pt, 0), UMath.AABB.Make(pt, pt));
	end;

	function Bounding.BySphere(const center: Vec3; const radius: float): Bounding;
	begin
		result := BySphere(UMath.Sphere.Make(center, radius));
	end;

	function Bounding.BySphere(const newSph: Sphere): Bounding;
	begin
		result := Make(BasedOnSphere, newSph, UMath.AABB.Make(newSph));
	end;

	function Bounding.ByAABB(const a, b: Vec3): Bounding;
	begin
		result := ByAABB(UMath.AABB.Make(a, b));
	end;

	function Bounding.ByAABB(const newAabb: AABB): Bounding;
	var
		o: Vec3;
	begin
		o := newAabb.Center;
		result := Make(BasedOnAABB, UMath.Sphere.Make(o, Distance(o, newAabb.A)), newAabb);
	end;

	function Bounding.BySphereAndAABB(const newSph: Sphere; const newAabb: AABB): Bounding;
	begin
		result := Make(UnknownBase, newSph, newAabb);
	end;

	function Bounding.Enlarge(const bnd: Bounding): boolean;
	begin
		result := _aabb.Enlarge(bnd._aabb);
		if result then self := ByAABB(_aabb);
	end;

	function Bounding.Enlarge(const pt: Vec3): boolean;
	var
		ok1, ok2: boolean;
	begin
		ok1 := _aabb.Enlarge(pt);
		ok2 := _sphere.Enlarge(pt);
		result := ok1 or ok2;
	end;

	function Bounding.Enlarge(const c2: Vec3; const r2: float): boolean;
	var
		ok1, ok2: boolean;
	begin
		case _base of
			BasedOnAABB:
				begin
					result := _aabb.Enlarge(c2, r2);
					if result then _sphere.Enlarge(c2, r2);
				end;
			BasedOnSphere:
				begin
					result := _sphere.Enlarge(c2, r2);
					if result then _aabb.Enlarge(c2, r2);
				end;
			else
				begin
					Assert(_base = UnknownBase);
					ok1 := _sphere.Enlarge(c2, r2);
					ok2 := _aabb.Enlarge(c2, r2);
					result := ok1 or ok2;
				end;
		end;
	end;

	function Bounding.Contains(const pt: Vec3): boolean;
	begin
		case _base of
			BasedOnSphere: result := _sphere.Contains(pt);
			else
			begin
				Assert((_base = BasedOnAABB) or (_base = UnknownBase));
				result := _aabb.Contains(pt);
			end
		end;
	end;

	function Bounding.Sizes: Vec3;
	begin
		case _base of
			BasedOnSphere: result := Vec3.Make(2.0 * _sphere.radius);
			else
			begin
				Assert((_base = BasedOnAABB) or (_base = UnknownBase));
				result := _aabb.Sizes;
			end;
		end;
	end;

	function Bounding.Intersects(const a, b: Bounding): boolean;
	begin
		case a._base of
			BasedOnSphere:
				case b._base of
					BasedOnAABB: result := UMath.AABB.Intersects(a._aabb, b._aabb);
					else
					begin
						Assert((b._base = BasedOnSphere) or (b._base = UnknownBase));
						result := UMath.Sphere.Intersects(a._sphere, b._sphere);
					end;
				end;
			BasedOnAABB:
				result := UMath.AABB.Intersects(a._aabb, b._aabb);
			else
			begin
				Assert(a._base = UnknownBase);
				case b._base of
					BasedOnSphere: result := UMath.Sphere.Intersects(a._sphere, b._sphere);
					else
					begin
						Assert((b._base = BasedOnAABB) or (b._base = UnknownBase));
						result := UMath.AABB.Intersects(a._aabb, b._aabb);
					end;
				end;
			end;
		end;
	end;

	function Bounding.IsUndefined: boolean;
	begin
		result := FloatIs.NaN(_sphere.center.x);
	end;

	operator =(const a, b: Bounding): boolean;
	begin
		result := (a.sphere.center = b.sphere.center) and (a.sphere.radius = b.sphere.radius) and (a.AABB.A = b.AABB.A) and (a.AABB.B = b.AABB.B);
	end;

	operator *(const t: Transform; const bnd: Bounding): Bounding;
	var
		sph: Sphere;
	begin
		sph := Sphere.Make(t * bnd._sphere.center, t.scale * bnd._sphere.radius);
		case bnd._base of
			BasedOnSphere: result := Bounding.BySphere(sph);
			else
			begin
				Assert((bnd._base = BasedOnAABB) or (bnd._base = UnknownBase));
				result := Bounding.Make(bnd._base, sph, t * bnd.aabb);
			end;
		end;
	end;

	function ToString(const bnd: Bounding): string;
		function DescSphere: string;
		begin
			result := 'сфера ' + Utils.ToString(bnd.Sphere);
		end;

		function DescAABB: string;
		begin
			result := 'AABB ' + ToString(bnd.AABB);
		end;
	begin
		case bnd._base of
			BasedOnSphere: result := DescSphere;
			BasedOnAABB: result := DescAABB;
			else
			begin
				Assert(bnd._base = UnknownBase);
				result := DescSphere + ', ' + DescAABB;
			end;
		end;
	end;

initialization
{$ifdef Debug} stat.Note(kd_depth_limit, MaxKDDepth); {$endif}
end.
