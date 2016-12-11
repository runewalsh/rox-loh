procedure fname(var ss: ScriptState {$ifndef readonly}; read: boolean {$endif});
type
	obj_ptr = ^otype;
{$if defined(prop_object) and not defined(readonly)} prop_optr = ^prop_object; {$endif}
var
	this: obj_ptr;
begin
	this := obj_ptr(ss.ToSelf);
{$if defined(prop_float)} {$define _push := ss.PushFloat(_1)} {$define _get := ss.ToFloat(3)} {$undef prop_float}
{$elseif defined(prop_vec2)} {$define _push := ss.PushVec2(_1)} {$define _get := ss.ToVec2(3)} {$undef prop_vec2}
{$elseif defined(prop_vec3)} {$define _push := ss.PushVec3(_1)} {$define _get := ss.ToVec3(3)} {$undef prop_vec3}
{$elseif defined(prop_vec4)} {$define _push := ss.PushVec4(_1)} {$define _get := ss.ToVec4(3)} {$undef prop_vec4}
{$elseif defined(prop_quat)} {$define _push := ss.PushQuaternion(_1)} {$define _get := ss.ToQuaternion(3)} {$undef prop_quat}
{$elseif defined(prop_bool)} {$define _push := ss.PushBool(_1)} {$define _get := ss.ToBool(3)} {$undef prop_bool}
{$elseif defined(prop_sint)} {$define _push := ss.PushBool(_1)} {$define _get := ss.ToSint(3)} {$undef prop_sint}
{$elseif defined(prop_string) or defined(prop_stream)}
	{$define _push := ss.PushString(_1)}
	{$ifdef prop_string} {$define _get := ss.ToString(3)} {$else} {$define _get := ss.ToStream(3)} {$endif}
{$elseif defined(prop_object)}
	{$define _push := ss.PushObject(_1)} {$define _get := prop_optr(ss.ToObject(3, TypeOf(prop_object)))}
{$elseif defined(enum_ids)}
	{$define _push := ss.PushString(enum_ids[_1])} {$define _get := enum_type(FindStr(ss.ToString(3), enum_ids, ord(_1)))}
{$else}
	{$error prop_TYPE undefined}
{$endif}

	{$define _1 := this^.field}
	{$ifndef readonly} if read then {$endif} _push {$ifndef readonly} else _1 := _get {$endif};
	{$undef _1}
	{$undef _push} {$undef _get}

	{$undef prop_string} {$undef prop_stream}
	{$undef prop_object}
{$ifdef enum_ids} {$undef enum_ids} {$undef enum_type} {$endif}
end;

{$undef fname}
{$undef otype}
{$undef field}
{$undef readonly}