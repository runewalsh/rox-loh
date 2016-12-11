{$ifndef free_only}
	function Create {$ifdef constructor_args} (constructor_args) {$endif}: pSelf; static;
{$ifdef constructor_args2} function Create (constructor_args2): pSelf; static; {$endif}
{$ifdef constructor_args3} function Create (constructor_args3): pSelf; static; {$endif}
{$ifdef constructor_args4} function Create (constructor_args4): pSelf; static; {$endif}
{$endif}
	procedure Free;
	procedure Free(var link: pSelf); static;
{$undef constructor_args} {$undef constructor_args2} {$undef constructor_args3} {$undef constructor_args4} {$undef free_only} {$undef pSelf}
