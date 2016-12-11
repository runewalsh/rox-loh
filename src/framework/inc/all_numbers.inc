{$define all_ints :=
	{$define typ := sint} {$define signed} {$define unsigned_pair := uint} intf {$undef signed} {$undef unsigned_pair}
	{$define typ := uint} {$define unsigned} intf {$undef unsigned}
	{$define typ := ilong} {$define signed} {$define unsigned_pair := ulong} intf {$undef signed} {$undef unsigned_pair}
	{$define typ := ulong} {$define unsigned} intf {$undef unsigned}
	{$undef typ} {$undef intf}}

{$define all_floats :=
	{$define typ := float} floatf
{$if sizeof(hp_float) <> sizeof(float)} {$define typ := hp_float} floatf {$endif}
	{$undef typ} {$undef floatf}}

{$define all_numbers :=
	{$define intf := numberf} {$define int} all_ints {$undef int}
	{$define floatf := numberf} {$define float} all_floats {$undef float}
	{$undef numberf}}

{$define all_sints :=
	{$define typ := sint} {$define uint_pair := uint} intf
	{$define typ := ilong} {$define uint_pair := ulong} intf
	{$undef typ} {$undef uint_pair} {$undef intf}}

{$define all_uints :=
	{$define typ := uint} {$define sint_pair := sint} intf
	{$define typ := ulong} {$define sint_pair := ilong} intf
	{$undef typ} {$undef sint_pair} {$undef intf}}

{$define all_uint_sizes :=
	{$define typ := uint8} intf
	{$define typ := uint16} intf
	{$define typ := uint32} intf
	{$define typ := uint64} intf
	{$undef typ} {$undef intf}}
