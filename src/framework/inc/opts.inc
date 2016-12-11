// Debug Profile use_discrete_gpu use_serialization use_console alter_cd selftest is_test dont_mirror_log_in_console
{$ifndef fpc} {$fatal} {$endif}
{$if defined(arrayofconst)} {$undef arrayofconst} {$mode objfpc} {$else} {$mode fpc} {$endif}
{$ASMMODE Intel}
{$MODESWITCH RESULT+} {$MODESWITCH DEFAULTPARAMETERS+} {$MODESWITCH EXCEPTIONS+} {$MODESWITCH OUT+} {$MODESWITCH CLASS+}
{$MODESWITCH PCHARTOSTRING-} {$modeswitch duplicatelocals+}

{$COPERATORS ON} {$GOTO ON} {$MACRO ON} {$LONGSTRINGS ON} {$TYPEINFO OFF} {$WRITEABLECONST OFF} {$IOCHECKS OFF} {$typedaddress on}
{$define unsafe_ptr := {$define safe_ptr := {$undef safe_ptr}}} // может, попытаться подправить код под $CHECKPOINTER?
                                                                // (на то, что есть, он бросается интерналэррорами).

{$packset default} {$packenum default}
{$fputype sse42}

{$define cinline:= {$ifndef Debug} inline; {$endif}}
{$define ConstructorFailed:=begin Done; System.Finalize(self); Fail; end}
{$ifdef Profile}
	{$define trace_call := try Profile.TraceCall}
	{$define leave_call := finally Profile.LeaveCall; end;}
{$else}
	{$define trace_call := //}
	{$define leave_call := }
{$endif}
{$ifopt C+} {$define assert} {$else} {$ifdef assert} {$error assert defined} {$endif} {$endif}
{$define unchecked := {$push} {$rangechecks off} {$overflowchecks off} {$define end_unchecked := {$pop} {$undef end_unchecked}}}
{$define fieldoffset := (pointer(@ {$define _ := (nil^). {$define _ := ) - nil) {$undef _}}}}

{$define instant_reraise_from_constructor := begin self.ReplaceVMT(TypeOf(BaseObject)); raise; end}
// Из-за WPO ReplaceVMT может не сработать сразу.
{$define instantly_reraised_from_constructor := self.VMTWasReplacedWith(TypeOf(BaseObject))}
{$define try2 := try try} {$define finally2 := finally} {$define except2 := end except}
{$define unused_args := Assert((@ {$define _ := > nil) and (@} {$define end_list := > nil)); {$undef _} {$undef end_list}}}
{$if FPC_FULLVERSION >= 30000} {$define FPC_3_BUG := } {$else} {$define FPC_3_BUG := //} {$endif}
{$define scoped_enum_ := {$push} {$scopedenums on} {$define _end := {$pop} {$undef _end}}}
{$define writeable_const_ := const {$push} {$writeableconst on} {$define _end := {$pop} {$undef _end}}}
{$define disable_hints := {$push} {$hints off} {$define reenable_hints := {$pop} {$undef reenable_hints}}}
{$define unsafe_exceptions := {$implicitexceptions off} {$define _end := {$implicitexceptions on} {$undef _end}}}
