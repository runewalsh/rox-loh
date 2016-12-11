{$define vector_common_no_elem_id}
{$include vector.common.inc}

	classname = object
	type
		pSelf = ^classname;
		Item = item_type;
		pItem = ^Item;
	{$ifdef user_param} UserParam = user_param; {$endif}

		pNode = ^Node;
		Node = object
			used, allocated: uint;
			next: pNode;
			items: array[0 .. 0] of Item;
			function Allocate(n: uint): pNode; static;
		end;

		Iterator = object
			function MakeToStore(node: pNode; index: uint): Iterator; static;
		private
			node: pNode;
			index: uint;
		{$ifdef Debug} validated: boolean; {$endif}
		end;
	public
		procedure Init({$ifdef user_param} const param: UserParam; {$endif} preallocate: uint = 0);
		procedure Done;
	{$define constructor_args := {$ifdef user_param} const param: UserParam; {$endif} preallocate: uint = 0} {$include dyn_obj.h.inc}

		procedure Push(const item: Item);
		procedure Push(items: pItem; count: uint);
		procedure Pop;
		procedure Pop(n: uint);
		// Ставит на место удаляемого элемента один из последующих — вызывающий не делает предположений, какой именно, известно только, что
		// можно итерировать дальше, проверив Valid.
		procedure RemoveReplace(var it: Iterator);

		// Из-за передачи по ссылки версия выше не работает с итераторами, хранящимися в узлах (на самом деле я не проверял :))).
		procedure RemoveReplaceSelf(it: Iterator);
		procedure Clear;
		function LastItem: Item;
		function LastPtr: pItem;
		
		function GetIterator: Iterator;
		procedure Next(var it: Iterator);
		function Valid({$ifdef Debug} var {$else} const {$endif} it: Iterator): boolean;
		function GetItem(const it: Iterator): Item;
		function GetItemPtr(const it: Iterator): pItem;

		// медленно, но если для каких-нибудь редких операций...
		function IteratorFromIndex(index: uint): Iterator;
		function ItemPtrFromIndex(index: uint): pItem;
		function ItemFromIndex(index: uint): Item;

		function Grow(by: uint): pItem;
		procedure Pack;
	{$ifdef dump} function Dump: string; {$endif}
	private
		nItems: uint;
		first, last: pNode;
	{$ifdef user_param} param: user_param; {$endif}
		function AllocateAndAppendNode(guarantee: uint = 0): pNode;
		procedure PopFromLast(n: uint);
		procedure RemoveAndDestroyLast;
		function InternalValid(const it: Iterator): boolean;
		function FindPrev(n: pNode): pNode;
	{$ifdef Debug}
	const
		CHECK_VALID = 'На итераторе не вызвана Valid.';
	{$endif}
	public
		property Count: uint read nItems;

	type
	{$ifdef extra_predicate_param} ExtraPredicateParam = extra_predicate_param; {$endif}
	{$ifdef extra_itemproc_param} ExtraItemProcParam = extra_itemproc_param; {$endif}

	{$define iterate_predicate :=
	type
		predicate = function({$ifdef item_qualifier} item_qualifier {$endif} item: Item {$ifdef elem_id}; id: uint {$endif} maybe_param): boolean;
	var
		function Each(f: predicate maybe_param): boolean;
		function Any(f: predicate maybe_param): boolean;
		function Find(f: predicate maybe_param): Iterator;
		function Find(f: predicate maybe_param; out id: Iterator): boolean;
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
		SupportDump            = {$ifdef dump} boolean {$else} record end {$endif};
		HasExtraPredicateParam = {$ifdef extra_predicate_param} boolean {$else} record end {$endif};
		HasExtraItemProcParam  = {$ifdef extra_itemproc_param} boolean {$else} record end {$endif};
		PassUserParam          = {$ifdef user_param} boolean {$else} record end {$endif};
		procedure _CompileTimeCheck;
	end;

end_vector_common
{$undef vector_common_no_elem_id}
{$undef extra_predicate_param} {$undef extra_itemproc_param} {$undef user_param}
{$undef classname} {$undef item_type} {$undef dump}
