{$include unique_def.inc}
{$define perm_elem := unique_def}
{$define perm_pelem := unique_def2}

type
	perm_elem = elemType;
	perm_pelem = ^perm_elem;

{$ifdef Shuffle}
	procedure Shuffle(list: perm_pelem; n: sint {$ifndef use_rng}; var rng: RNG {$endif});
	var
		i, x: sint;
		t: elemType;
	begin
		for i := 0 to n - 2 do
		begin
			x := {$ifdef use_rng} use_rng {$else} rng {$endif}.GetUint(i, n - 1);
			t := list[i];
			list[i] := list[x];
			list[x] := t;
		end;
	end;

{$ifdef openarray}
	procedure Shuffle(var list: array of perm_elem {$ifndef use_rng}; var rng: RNG {$endif});
	begin
		Shuffle(perm_pelem(list), length(list)  {$ifndef use_rng}, rng {$endif});
	end;
{$endif}
{$undef Shuffle} {$undef use_rng}
{$endif}

end_unique_def {$undef perm_elem} {$undef perm_pelem}
{$undef elemType} {$undef openarray}