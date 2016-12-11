{$ifdef less_wrap} {$error} {$endif}
{$ifdef get_key_wrap} {$error} {$endif}
{$ifndef key} {$define key := sorted_elem} {$endif}
{$define less_wrap := {$ifdef less} (less) {$else} (_1 < _2) {$endif} {$undef _1} {$undef _2}}
{$define get_key_wrap := {$ifdef get_key} (get_key) {$else} (elem) {$endif} {$undef elem}}

{$include unique_def.inc}
{$define sorted_elem := unique_def}
{$define sorted_elemptr := unique_def2}

{$if defined(FirstGreater) or defined(InsertInSorted) or defined(EmplaceInSorted)}
type
	sorted_elem = elem;
	sorted_elemptr = ^sorted_elem;

	function FirstGreater(list: sorted_elemptr; n: uint; const e: key): uint;
	var
		L, R, mid: uint;
	begin
		if n = 0 then exit(0);

		L := 0;
		R := n - 1;
		repeat
			mid := L + (R - L) div 2;
			if {$define _1 := e} {$define _2 := {$define elem := list[mid]} get_key_wrap} less_wrap then
				R := mid
			else
				L := mid + 1;
		until (L >= R) and not ((L = R) and {$define _1 := {$define elem := list[L]} get_key_wrap} {$define _2 := e} less_wrap);
		result := L;
	end;
{$undef FirstGreater}
{$endif}

{$push} {$warnings off} // Comment level 2
{$ifdef IsSorted}
	function IsSorted(list: sorted_elemptr; n: uint): boolean;
	var
		i: uint;
	begin
		if n < 2 then exit(yes);
		for i := 0 to n - 2 do
			if {$define _1 := {$define elem := list[i + 1]} get_key_wrap} {$define _2 := {$define elem := list[i]} get_key_wrap} less_wrap then
				exit(no);
		result := yes;
	end;
{$undef IsSorted}
{$endif}
{$pop}

{$define insert_or_emplace_impl :=
	function {$ifdef emplace} EmplaceInSorted {$else} InsertInSorted {$endif}
		(list: sorted_elemptr; n: uint; const e: {$ifdef emplace} key {$else} sorted_elem {$endif}): {$ifdef emplace} sorted_elemptr {$else} uint {$endif};
	var
		at, i: uint;
	begin
		at := FirstGreater(list, n, {$ifdef emplace} e {$else} {$define elem := e} get_key_wrap {$endif});
		for i := n downto at + 1 do
			list[i] := list[i - 1];
	{$ifdef emplace} result := @list[at]; {$else} list[at] := e; result := at; {$endif}
	end; {$undef emplace}}

{$ifdef InsertInSorted} {$ifdef emplace} {$error emplace defined} {$endif} insert_or_emplace_impl {$undef InsertInSorted} {$endif}
{$ifdef EmplaceInSorted} {$define emplace} insert_or_emplace_impl {$undef EmplaceInSorted} {$endif}
{$undef insert_or_emplace_impl}

end_unique_def
{$undef sorted_elem} {$undef sorted_elemptr}
{$undef less_wrap} {$undef get_key_wrap}
{$undef elem} {$undef less} {$undef key} {$undef get_key}
