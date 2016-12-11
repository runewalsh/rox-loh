{$if defined(unique_def)} {$error} {$endif}
{$if defined(unique_def2)} {$error} {$endif}
{$if defined(end_unique_def)} {$error} {$endif}

{$if not defined(UNIQUE_DEF_I_USED)} {$define UNIQUE_DEF_I_USED}
	{$define unique_def:=UNIQUE_DEF_I} {$define unique_def2:=UNIQUE_DEF2_I} {$define unique_def3:=UNIQUE_DEF3_I}
	{$define unique_def4:=UNIQUE_DEF4_I} {$define unique_def5:=UNIQUE_DEF5_I}
{$elseif not defined(UNIQUE_DEF_II_USED)} {$define UNIQUE_DEF_II_USED}
	{$define unique_def:=UNIQUE_DEF_II} {$define unique_def2:=UNIQUE_DEF2_II} {$define unique_def3:=UNIQUE_DEF3_II}
	{$define unique_def4:=UNIQUE_DEF4_II} {$define unique_def5:=UNIQUE_DEF5_II}
{$elseif not defined(UNIQUE_DEF_III_USED)} {$define UNIQUE_DEF_III_USED}
	{$define unique_def:=UNIQUE_DEF_III} {$define unique_def2:=UNIQUE_DEF2_III} {$define unique_def3:=UNIQUE_DEF3_III}
	{$define unique_def4:=UNIQUE_DEF4_III} {$define unique_def5:=UNIQUE_DEF5_III}
{$elseif not defined(UNIQUE_DEF_IV_USED)} {$define UNIQUE_DEF_IV_USED}
	{$define unique_def:=UNIQUE_DEF_IV} {$define unique_def2:=UNIQUE_DEF2_IV} {$define unique_def3:=UNIQUE_DEF3_IV}
	{$define unique_def4:=UNIQUE_DEF4_IV} {$define unique_def5:=UNIQUE_DEF5_IV}
{$elseif not defined(UNIQUE_DEF_V_USED)} {$define UNIQUE_DEF_V_USED}
	{$define unique_def:=UNIQUE_DEF_V} {$define unique_def2:=UNIQUE_DEF2_V} {$define unique_def3:=UNIQUE_DEF3_V}
	{$define unique_def4:=UNIQUE_DEF4_V} {$define unique_def5:=UNIQUE_DEF5_V}
{$elseif not defined(UNIQUE_DEF_VI_USED)} {$define UNIQUE_DEF_VI_USED}
	{$define unique_def:=UNIQUE_DEF_VI} {$define unique_def2:=UNIQUE_DEF2_VI} {$define unique_def3:=UNIQUE_DEF3_VI}
	{$define unique_def4:=UNIQUE_DEF4_VI} {$define unique_def5:=UNIQUE_DEF5_VI}
{$else} {$error добавь ещё уникальных имён в unique_def.inc! :)}
{$endif}
{$define end_unique_def:={$undef unique_def} {$undef unique_def2} {$undef unique_def3} {$undef unique_def4} {$undef unique_def5} {$undef end_unique_def}}