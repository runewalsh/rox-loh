{$if defined(instance) or defined(locked_get) or defined(destroy_on_exit)} {$error} {$endif}
{$include unique_def.inc} {$define instance := unique_def} {$define locked_get := unique_def2} {$define destroy_on_exit := unique_def3}
{$ifdef instance_name} {$define instance := instance_name} {$endif}
var
	instance: instance_type = nil;

{$if defined(destroy_instance)} procedure destroy_on_exit; begin destroy_instance; end; {$endif}

	function locked_get: instance_type;
	begin
		SingletonLock.Enter;
		if Assigned(instance) then result := instance else
		begin
			result := create_instance;
		{$if defined(destroy_instance)}
			{$if defined(unitname)}
				units.AddSingleton(unitname, @destroy_on_exit);
			{$else}
				AddExitProc(@destroy_on_exit);
			{$endif}
		{$endif}
			instance := result;
		end;
		SingletonLock.Leave;
	end;

	function accessor: instance_type;
	begin
		result := instance;
		if not Assigned(result) then result := locked_get;
	end;

end_unique_def {$undef instance} {$undef locked_get} {$undef destroy_on_exit}
{$undef create_instance} {$undef destroy_instance} {$undef instance_name} {$undef unitname}
{$undef instance_type} {$undef accessor}
