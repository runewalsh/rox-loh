{$push} {$warnings off} // comment level 2
{$ifndef free_only}
{$define create_impl :=
	function classname.Create {$ifdef constructor_args} (constructor_args) {$endif}: pSelf;
	begin
		new(result);
		try
			result^.Init {$ifdef pass_constructor_args} (pass_constructor_args) {$endif};
		except
			dispose(result);
			raise;
		end;
	end;}

	create_impl
{$ifdef constructor_args2} {$define constructor_args := constructor_args2} {$define pass_constructor_args := pass_constructor_args2} create_impl {$endif}
{$ifdef constructor_args3} {$define constructor_args := constructor_args3} {$define pass_constructor_args := pass_constructor_args3} create_impl {$endif}
{$ifdef constructor_args4} {$define constructor_args := constructor_args4} {$define pass_constructor_args := pass_constructor_args4} create_impl {$endif}
{$endif}
{$pop}

	procedure classname.Free;
	begin
	{$ifdef Debug} stat.Increment(dyn_obj_frees); {$endif}
		if Assigned(@self) then
		begin
		{$ifndef no_done} Done; {$endif}
			dispose(@self);
		end {$ifdef Debug} else stat.Increment(dyn_obj_nil_frees) {$endif};
	end;

	procedure classname.Free(var link: pSelf);
	var
		t: pSelf;
	begin
		t := link; link := nil;
		t^.Free;
	end;

{$ifndef keep_classname_define} {$undef classname} {$endif}
{$undef pass_constructor_args} {$undef constructor_args} {$undef pass_constructor_args2} {$undef constructor_args2}
{$undef pass_constructor_args3} {$undef constructor_args3} {$undef pass_constructor_args4} {$undef constructor_args4}
{$undef free_only} {$undef no_done} {$undef pSelf}
