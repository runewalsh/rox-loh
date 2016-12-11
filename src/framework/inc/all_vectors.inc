{$define _all_vectors :=
	{$if defined(base_type) or defined(signed) or defined(unsigned) or
		defined(vec) or defined(pvec) or defined(veclen) or defined(vectypename) or
		defined(uint_vec) or defined(int_vec) or defined(float_vec) or defined(signed_vec) or defined(unsigned_vec) or defined(pair2) or defined(pair3) or defined(pair4) or
		defined(item) or defined(itemid) or defined(itemname)} {$error all_vectors defines leak} {$endif}

	{$define foreach_component :=
		{$define item := x} {$define itemid := 0} {$define itemname := 'x'} {$define first} iterate {$undef first}
		{$define item := y} {$define itemid := 1} {$define itemname := 'y'} iterate
		{$if veclen >= 3} {$define item := z} {$define itemname := 'z'} {$define itemid := 2} iterate {$endif}
		{$if veclen >= 4} {$define item := w} {$define itemname := 'w'} {$define itemid := 3} iterate {$endif}
	{$undef item} {$undef itemid} {$undef itemname} {$undef iterate}}

	{$define reduce_vec := // {$define op := *} reduce_vec   =>   x * y * z * w
		{$define iterate := {$ifndef first} {$ifdef op} op {$else} + {$endif} {$endif} {$ifdef one} one {$else} item {$endif}} foreach_component {$undef op} {$undef one}}

	{$define comma_separated := {$define op := ,} reduce_vec}

{$if defined(only_float) or defined(only_floating) or defined(all)} {$define floating} {$define signed}
	{$define base_type := float} {$define pair2 := Vec2} {$define pair3 := Vec3} {$define pair4 := Vec4}
	{$define vec := Vec2} {$define pvec := pVec2} {$define veclen := 2} {$define vectypename := 'Vec2'} {$define uint_vec := UintVec2} {$define int_vec := IntVec2} vecf
	{$define vec := Vec3} {$define pvec := pVec3} {$define veclen := 3} {$define vectypename := 'Vec3'} {$define uint_vec := UintVec3} {$define int_vec := IntVec3} vecf
	{$define vec := Vec4} {$define pvec := pVec4} {$define veclen := 4} {$define vectypename := 'Vec4'} {$define uint_vec := UintVec4} {$define int_vec := IntVec4} vecf
	{$undef only_float}

	{$undef floating} {$undef signed} {$undef only_floating} {$undef uint_vec} {$undef int_vec}
{$endif floating}

{$if defined(only_integer) or defined(all)} {$define integer}
	{$define base_type := sint} {$define signed} {$define base_type_unsigned := uint} {$define base_type_signed := sint} {$define pair2 := IntVec2} {$define pair3 := IntVec3} {$define pair4 := IntVec4}
	{$define vec := IntVec2} {$define pvec := pIntVec2} {$define veclen := 2} {$define vectypename := 'IntVec2'} {$define unsigned_vec := UintVec2} {$define uint_vec := UintVec2} {$define float_vec := Vec2} vecf
	{$define vec := IntVec3} {$define pvec := pIntVec3} {$define veclen := 3} {$define vectypename := 'IntVec3'} {$define unsigned_vec := UintVec3} {$define uint_vec := UintVec3} {$define float_vec := Vec3} vecf
	{$define vec := IntVec4} {$define pvec := pIntVec4} {$define veclen := 4} {$define vectypename := 'IntVec4'} {$define unsigned_vec := UintVec4} {$define uint_vec := UintVec4} {$define float_vec := Vec4} vecf
	{$undef signed} {$undef unsigned_vec} {$undef uint_vec}

	{$define base_type := uint} {$define unsigned} {$define base_type_unsigned := uint} {$define base_type_signed := sint} {$define pair2 := UintVec2} {$define pair3 := UintVec3} {$define pair4 := UintVec4}
	{$define vec := UintVec2} {$define pvec := pUintVec2} {$define veclen := 2} {$define vectypename := 'UintVec2'} {$define signed_vec := IntVec2} {$define int_vec := IntVec2} {$define float_vec := Vec2} vecf
	{$define vec := UintVec3} {$define pvec := pUintVec3} {$define veclen := 3} {$define vectypename := 'UintVec3'} {$define signed_vec := IntVec3} {$define int_vec := IntVec3} {$define float_vec := Vec3} vecf
	{$define vec := UintVec4} {$define pvec := pUintVec4} {$define veclen := 4} {$define vectypename := 'UintVec4'} {$define signed_vec := IntVec4} {$define int_vec := IntVec4} {$define float_vec := Vec4} vecf
	{$undef unsigned} {$undef signed_vec} {$undef int_vec}

	{$undef integer} {$undef only_integer} {$undef float_vec} {$undef base_type_unsigned} {$undef base_type_signed}
{$endif integer}

	{$undef base_type} {$undef pair2} {$undef pair3} {$undef pair4}
	{$undef vec} {$undef pvec} {$undef veclen} {$undef vectypename}
	{$undef foreach_component} {$undef reduce_vec} {$undef comma_separated}
	{$undef all} {$undef vecf}

	{$if defined(all) or defined(only_floating) or defined(only_integer) or defined(only_float)} {$error all_vectors defines leak 2} {$endif}}

{$define all_vectors := {$define all} _all_vectors}
{$define all_floating_vectors := {$define only_floating} _all_vectors}
{$define all_integer_vectors := {$define only_integer} _all_vectors}
{$define all_float_vectors := {$define only_float} _all_vectors}
