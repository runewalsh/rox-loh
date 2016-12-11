var
	ap: pPtrUint absolute a;
	bp: pPtrUint absolute b;
	{$ifndef inplace_op} tp: pPtrUint absolute target; {$endif}
	ab: pByte absolute a;
	bb: pByte absolute b;
	{$ifndef inplace_op} tb: pByte absolute target; {$endif}
begin
	while size >= sizeof(ap^) do
	begin
		{$define _1 := ap^} {$define _2 := bp^}
		{$ifdef inplace_op} inplace_op {$else} {$define R := tp^} op {$undef R} {$endif}
		{$undef _1} {$undef _2};
		{$ifndef inplace_op} tp += 1; {$endif} ap += 1; bp += 1; size -= sizeof(ap^);
	end;

	while size > 0 do
	begin
		{$define _1 := ab^} {$define _2 := bb^} {$define byte}
		{$ifdef inplace_op} inplace_op {$else} {$define R := tb^} op {$undef R} {$endif}
		{$undef _1} {$undef _2} {$undef byte};
		{$ifndef inplace_op} tb += 1; {$endif} ab += 1; bb += 1; size -= sizeof(ab^);
	end;
end;

{$undef inplace_op} {$undef op}
