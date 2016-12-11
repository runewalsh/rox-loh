{$define maybe_param := {$ifdef param_type}; {$ifdef param_qualifier} param_qualifier {$endif} param: param_type {$endif}}

{$define for_each_predicate :=
	{$define predicate:=ConstPredicate} {$define item_qualifier:=const} {$define param_type:=pointer} iterate_predicate {$undef item_qualifier} {$undef param_type}
	{$define predicate:=UnparaConstPredicate} {$define item_qualifier:=const} iterate_predicate {$undef item_qualifier}
	{$define predicate:=ValuePredicate} {$define param_type:=pointer} iterate_predicate {$undef param_type}
	{$define predicate:=UnparaValuePredicate} iterate_predicate
{$ifndef vector_common_no_elem_id}
	{$define predicate:=ConstIPredicate} {$define item_qualifier:=const} {$define elem_id} {$define param_type:=pointer} iterate_predicate {$undef item_qualifier} {$undef elem_id} {$undef param_type}
	{$define predicate:=UnparaConstIPredicate} {$define item_qualifier:=const} {$define elem_id} iterate_predicate {$undef elem_id} {$undef item_qualifier}
	{$define predicate:=ValueIPredicate} {$define param_type:=pointer} {$define elem_id} iterate_predicate {$undef param_type} {$undef elem_id}
	{$define predicate:=UnparaValueIPredicate} {$define elem_id} iterate_predicate {$undef elem_id}
{$endif}
{$ifdef extra_predicate_param}
	{$define predicate:=ConstExtraConstPredicate} {$define item_qualifier:=const} {$define param_qualifier:=const} {$define param_type:=ExtraPredicateParam} iterate_predicate {$undef item_qualifier} {$undef param_qualifier} {$undef param_type}
{$endif}
	{$undef iterate_predicate} {$undef predicate}}

{$define for_each_item_proc :=
	{$define item_proc:=ConstItemProc} {$define param_type:=pointer} {$define item_qualifier:=const} iterate_item_proc {$undef param_type} {$undef item_qualifier}
	{$define item_proc:=UnparaConstItemProc} {$define item_qualifier:=const} iterate_item_proc {$undef item_qualifier}
	{$define item_proc:=VarItemProc} {$define param_type:=pointer} {$define item_qualifier:=var} iterate_item_proc {$undef param_type} {$undef item_qualifier}
	{$define item_proc:=UnparaVarItemProc} {$define item_qualifier:=var} iterate_item_proc {$undef item_qualifier}
	{$define item_proc:=ValueItemProc} {$define param_type:=pointer} iterate_item_proc {$undef param_type}
	{$define item_proc:=UnparaValueItemProc}  iterate_item_proc
{$ifndef vector_common_no_elem_id}
	{$define item_proc:=ConstItemIProc} {$define param_type:=pointer} {$define elem_id} {$define item_qualifier:=const} iterate_item_proc {$undef param_type} {$undef elem_id} {$undef item_qualifier}
	{$define item_proc:=UnparaConstItemIProc} {$define item_qualifier:=const} {$define elem_id} iterate_item_proc {$undef item_qualifier} {$undef elem_id}
	{$define item_proc:=VarItemIProc} {$define item_qualifier:=var} {$define elem_id} {$define param_type:=pointer} iterate_item_proc {$undef item_qualifier} {$undef elem_id} {$undef param_type}
	{$define item_proc:=UnparaVarItemIProc} {$define elem_id} {$define item_qualifier:=var} iterate_item_proc {$undef elem_id} {$undef item_qualifier}
	{$define item_proc:=ValueItemIProc} {$define param_type:=pointer} {$define elem_id} iterate_item_proc {$undef param_type} {$undef elem_id}
	{$define item_proc:=UnparaValueItemIProc} {$define elem_id} iterate_item_proc {$undef elem_id}
{$endif}
{$ifdef extra_itemproc_param}
	{$define item_proc:=ExtraConstItemProc} {$define item_qualifier:=const} {$define param_type:=ExtraItemProcParam} iterate_item_proc {$undef item_qualifier} {$undef param_type}
	{$define item_proc:=ExtraValueItemProc} {$define param_type:=ExtraItemProcParam} iterate_item_proc {$undef param_type}
{$endif}
	{$undef iterate_item_proc} {$undef item_proc}}

{$define end_vector_common := {$undef maybe_param} {$undef for_each_predicate} {$undef for_each_item_proc} {$undef end_vector_common}}
