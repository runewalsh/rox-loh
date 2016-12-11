	operator <(const a, b: typ): boolean;
	begin
		{$ifdef ref} result := {$define _1 := a} ref < {$define _1 := b} ref;
		{$else} result := {$define _1 := a} {$define _2 := b} (less) {$undef _1} {$undef _2};
		{$endif}
	end;

	operator <=(const a, b: typ): boolean;
	begin
		{$ifdef ref} result := {$define _1 := a} ref <= {$define _1 := b} ref;
		{$else} result := not {$define _1 := b} {$define _2 := a} (less) {$undef _1} {$undef _2};
		{$endif}
	end;

	operator >(const a, b: typ): boolean;
	begin
		{$ifdef ref} result := {$define _1 := a} ref > {$define _1 := b} ref;
		{$else} result := {$define _1 := b} {$define _2 := a} (less) {$undef _1} {$undef _2};
		{$endif}
	end;

	operator >=(const a, b: typ): boolean;
	begin
		{$ifdef ref} result := {$define _1 := a} ref >= {$define _1 := b} ref;
		{$else} result := not {$define _1 := a} {$define _2 := b} (less) {$undef _1} {$undef _2};
		{$endif}
	end;

	operator =(const a, b: typ): boolean;
	begin
		{$if defined(ref)} result := {$define _1 := a} ref = {$define _1 := b} ref;
		{$elseif defined(equal)} result := {$define _1 := a} {$define _2 := b} equal;
		{$else} result := not {$define _1 := a} {$define _2 := b} (less) {$undef _1} {$undef _2}
		              and not {$define _1 := b} {$define _2 := a} (less) {$undef _1} {$undef _2};
			{$note emulating "equal" through "less"}
		{$endif}
	end;
{$undef typ} {$undef less} {$undef equal} {$undef ref}
