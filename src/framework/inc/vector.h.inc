{$ifdef detect_corruption} {$error} {$endif}
{$ifdef Debug} {$define detect_corruption} {$endif}
{$include vector.common.inc}

	classname = object
	type
		pSelf = ^classname;
		Item = item_type;
		pItem = ^Item;
		Iterator = uint; // для меньшей несовместимости с vector-link
	{$ifdef user_param} UserParam = user_param; {$endif}
	private
		_n, alloc: uint;
		_items: pItem;
	{$ifdef user_param} param: user_param; {$endif}
	public
		procedure Init({$ifdef user_param} const param: UserParam; {$endif} preallocate: uint = 0);
		procedure Done;
	{$define constructor_args := {$ifdef user_param} const param: UserParam; {$endif} preallocate: uint = 0} {$include dyn_obj.h.inc}
		property Items: pItem read _items;
		property N: uint read _n;
		property Count: uint read _n;

		procedure Push(const newItem: Item); cinline
		procedure Push(newItems: pItem; count: uint);
		procedure Pop;
		procedure RemoveReplace(id: uint); cinline
		procedure RemoveShift(id: uint);
		procedure Clear;
		function Last: Item; cinline
		function LastPtr: pItem; cinline
		function LastIndex: uint; cinline

		function Grow(by: uint): pItem;
		procedure Pack;
		procedure SetExactLength(len: uint);

	private
		function Reallocate(newAlloc: uint): pItem;
		function ReallocateWithMargin: pItem;
	{$ifdef store_ptr} procedure PostRelocate(n: uint); {$endif}

{$ifdef detect_corruption}
	private type
		SentinelItemFootprint = packed array[0 .. sizeof(Item) - 1] of byte;
	var
		firstSentinel, lastSentinel: SentinelItemFootprint;
		procedure CheckForCorruption;
		procedure SetupLastSentinel;
{$endif}

	public type
	{$ifdef extra_predicate_param} ExtraPredicateParam = extra_predicate_param; {$endif}
	{$ifdef extra_itemproc_param} ExtraItemProcParam = extra_itemproc_param; {$endif}

	{$define iterate_predicate :=
	type
		predicate = function({$ifdef item_qualifier} item_qualifier {$endif} item: Item {$ifdef elem_id}; id: uint {$endif} maybe_param): boolean;
	var
		function Each(f: predicate maybe_param): boolean;
		function Any(f: predicate maybe_param): boolean;
		function Find(f: predicate maybe_param): sint;
		function Find(f: predicate maybe_param; out id: uint): boolean;
		function Matching(f: predicate maybe_param): uint;} for_each_predicate

	type
		scoped_enum_ ForEachFlag = (ReversedOrder); _end
		ForEachFlags = set of ForEachFlag;
	const
		ReversedOrder = ForEachFlag.ReversedOrder;

	{$define iterate_item_proc :=
	type
		item_proc = procedure({$ifdef item_qualifier} item_qualifier {$endif} item: Item {$ifdef elem_id}; id: uint {$endif} maybe_param);
	var
		procedure ForEach(f: item_proc maybe_param; flags: ForEachFlags = []);} for_each_item_proc

	public type
		DetectVectorCorruption = {$ifdef detect_corruption} sint {$else} record end {$endif};
		HasExtraPredicateParam = {$ifdef extra_predicate_param} sint {$else} record end {$endif};
		HasExtraItemProcParam  = {$ifdef extra_itemproc_param} sint {$else} record end {$endif};
		PassUserParam          = {$ifdef user_param} boolean {$else} record end {$endif};
		procedure _CompileTimeCheck;
	end;

end_vector_common
{$undef detect_corruption}
{$undef extra_predicate_param} {$undef extra_itemproc_param} {$undef user_param}
{$undef store_ptr}
{$undef classname} {$undef item_type}
