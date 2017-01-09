// store_ptr  := макрос, работающий с item и ptr. Сохраняет указатель на элемент.
// store_iter := то же самое, но вместо указателя ptr сохраняет итератор iter.
// Инвариант: ни в одном узле не может быть 0 элементов. Если от этого отказаться, можно будет не занулять итератор в Remove...
{$ifdef extra_predicate_param} {$error} {$endif}
{$ifdef extra_itemproc_param} {$error} {$endif}
{$ifdef dump} {$error} {$endif}
{$ifdef user_param} {$error} {$endif}

// записывает указатель и/или итератор в свежедобавленный или перемещённый элемент.
{$define store_extra_wrap :=
	begin
	{$ifdef store_ptr}
		{$define item := pItem(node^.items)[index]} {$define ptr := (pItem(node^.items) + (index))} begin store_ptr end; {$undef item} {$undef ptr}
	{$endif}

	{$if defined(store_iter) and not defined(iters_unchanged)}
		{$define item := pItem(node^.items)[index]} {$define iter := Iterator.MakeToStore(node, index)} begin store_iter end; {$undef item} {$undef iter}
	{$endif}
	end {$undef node} {$undef index} {$undef iters_unchanged}}

	procedure classname._CompileTimeCheck;
	type
		HEP = classname.HasExtraPredicateParam;
		HIP = classname.HasExtraItemProcParam;
		D   = classname.SupportDump;
		UP  = classname.PassUserParam;
	begin
	{$if sizeof(HEP) > 0} {$define extra_predicate_param} {$endif}
	{$if sizeof(HIP) > 0} {$define extra_itemproc_param} {$endif}
	{$if sizeof(D) > 0} {$define dump} {$endif}
	{$if sizeof(UP) > 0} {$define user_param} {$endif}
	{$define vector_common_no_elem_id}
	{$include vector.common.inc}
	end;

	function classname.Node.Allocate(n: uint): pNode;
	begin
		Assert(n > 0);
		result            := GetMem(sizeof(Node) - sizeof(Node.items) + sizeof(Node.items[0]) * n);
		result^.used      := 0;
		result^.allocated := n;
		result^.next      := nil;
	end;

	function classname.Iterator.MakeToStore(node: pNode; index: uint): Iterator;
	begin
		result.node  := node;
		result.index := index;
	{$ifdef Debug} result.validated := yes; {$endif}
	end;

	procedure classname.Init({$ifdef user_param} const param: UserParam; {$endif} preallocate: uint = 0);
	begin
		nItems := 0;
		first := nil;
		last  := nil;
	{$ifdef user_param} self.param := param; {$endif}
		if preallocate > 0 then AllocateAndAppendNode(preallocate);
	end;

	procedure classname.Done;
	begin
		Clear;
	end;

{$define constructor_args := {$ifdef user_param} const param: UserParam; {$endif} preallocate: uint = 0}
{$define pass_constructor_args := {$ifdef user_param} param, {$endif} preallocate}
{$define keep_classname_define} {$include dyn_obj.pp.inc} {$undef keep_classname_define}

	procedure classname.Push(const item: Item);
	var
		n: pNode;
	begin
		n := last;
		if not Assigned(n) or (n^.used = n^.allocated) then
			n := AllocateAndAppendNode;

		System.Initialize(pItem(n^.items)[n^.used]);
		pItem(n^.items)[n^.used] := item;
	{$define node := n} {$define index := n^.used} store_extra_wrap;

		inc(n^.used);
		inc(nItems);
	end;

	procedure classname.Push(items: pItem; count: uint);
	var
		n: pNode;
		nFree, toExisting, i: uint;
	begin
		// добавить в существующий узел, полностью (если умещается) или частично
		n := last;
		if Assigned(n) then
		begin
			nFree := n^.allocated - n^.used;
			if count <= nFree then toExisting := count else toExisting := nFree;

			System.Initialize(pItem(n^.items)[n^.used], toExisting);
			for i := 1 to toExisting do
			begin
				pItem(n^.items)[n^.used + (i - 1)] := items^;
			{$define node := n} {$define index := n^.used + (i - 1)} store_extra_wrap;
				inc(items); dec(count);
			end;
			n^.used += toExisting;
			nItems += toExisting;
		end;

		// добавить остаток в новый узел
		if count > 0 then
		begin
			n := AllocateAndAppendNode(count);
			System.Initialize(pItem(n^.items)[0], count);
			for i := 1 to count do
			begin
				pItem(n^.items)[i - 1] := items[i - 1];
			{$define node := n} {$define index := i - 1} store_extra_wrap;
			end;
			n^.used := count;
			nItems += count;
		end;
	end;

	procedure classname.Pop;
	begin
		PopFromLast(1);
	end;

	procedure classname.Pop(n: uint);
	begin
		Assert(n <= nItems);

		while (n > 0) and (n >= last^.used) do
		begin
			System.Finalize(pItem(last^.items)[0], last^.used);
			nItems -= last^.used;
			n      -= last^.used;
			RemoveAndDestroyLast;
		end;

		if n > 0 then PopFromLast(n);
	end;

	procedure classname.RemoveReplace(var it: Iterator);
	var
		n: pNode;
	begin
	{$ifdef Debug} Assert(it.validated, CHECK_VALID); {$endif}
		Assert((pointer(@it) < pointer(pItem(it.node^.items))) or (pointer(@it) >= pointer(pItem(it.node^.items) + it.node^.used)),
			'Итератор хранится в самом удаляемом элементе, следует использовать RemoveReplaceSelf.');

		// Если узел последний или в нём больше половины элементов — заменить удаляемый элемент последним из этого же узла.
		// Если в результате получилось node.used = 0, нужно удалить node и обновить итератор.
		if (it.node = last) or (it.node^.used > it.node^.allocated div 2) then
		begin
			dec(it.node^.used);
			pItem(it.node^.items)[it.index] := pItem(it.node^.items)[it.node^.used];
			with it do store_extra_wrap; // семантика дефайнов node и index совпадает с полями в Iterator
			System.Finalize(pItem(it.node^.items)[it.node^.used]);
			dec(nItems);

			if it.node^.used = 0 then
			begin
				if it.node = last then
				begin
					RemoveAndDestroyLast;
					it.node := nil;
				end else
				begin
					n       := it.node;
					it.node := it.node^.next;

					if n = first then
					begin
						first := first^.next;
						Assert(Assigned(first));
						// first^.prev := nil;
					end else
						FindPrev(n)^.next := n^.next;
					FreeMem(n);
				end;
			end;
		end else
		// Иначе — заменить узел последним по всём векторе.
		begin
			pItem(it.node^.items)[it.index] := pItem(last^.items)[last^.used - 1];
			with it do store_extra_wrap;
			PopFromLast(1);
		end;

	{$ifdef Debug} it.validated := no; {$endif}
	end;

	procedure classname.RemoveReplaceSelf(it: Iterator);
	begin
		RemoveReplace(it);
	end;

	procedure classname.Clear;
	var
		c, t: pNode;
	begin
		c := first;
		while Assigned(c) do
		begin
			System.Finalize(pItem(c^.items)[0], c^.used);
			t := c;
			c := c^.next;
			FreeMem(t);
		end;

		first  := nil;
		last   := nil;
		nItems := 0;
	end;

	function classname.LastItem: Item;
	begin
		Assert(nItems > 0);
		result := pItem(last^.items)[last^.used - 1];
	end;

	function classname.LastPtr: pItem;
	begin
		Assert(nItems > 0);
		result := pItem(last^.items) + (last^.used - 1);
	end;

	function classname.GetIterator: Iterator;
	begin
		result.node  := first;
		result.index := 0;
	{$ifdef Debug} result.validated := no; {$endif}
	end;

	procedure classname.Next(var it: Iterator);
	begin
	{$ifdef Debug} Assert(it.validated, CHECK_VALID); {$endif}
		inc(it.index);
		if it.index >= it.node^.used then
		begin
			it.node  := it.node^.next;
			it.index := 0;
			Assert(not Assigned(it.node) or (it.index < it.node^.used));
		end;
	{$ifdef Debug} it.validated := no; {$endif}
	end;

	function classname.Valid({$ifdef Debug} var {$else} const {$endif} it: Iterator): boolean;
	begin
	{$ifdef Debug} Assert(not it.validated, 'Итератор проверен дважды.'); {$endif}
		result := InternalValid(it);
	{$ifdef Debug} it.validated := yes; {$endif}
	end;

	function classname.GetItem(const it: Iterator): Item;
	begin
	{$ifdef Debug} Assert(it.validated, CHECK_VALID); {$endif}
		Assert(InternalValid(it));
		result := pItem(it.node^.items)[it.index];
	end;

	function classname.GetItemPtr(const it: Iterator): pItem;
	begin
	{$ifdef Debug} Assert(it.validated, CHECK_VALID); {$endif}
		Assert(InternalValid(it));
		result := pItem(it.node^.items) + it.index;
	end;

	function classname.IteratorFromIndex(index: uint): Iterator;
	var
		c: pNode;
	begin
		c := first;
		repeat
			Assert(Assigned(c));
			if index < c^.used then
			begin
				result.node := c;
				result.index := index;
			{$ifdef Debug} result.validated := yes; {$endif}
				exit;
			end;

			index -= c^.used;
			c := c^.next;
		until no;
	end;

	function classname.ItemPtrFromIndex(index: uint): pItem;
	begin
		result := GetItemPtr(IteratorFromIndex(index));
	end;

	function classname.ItemFromIndex(index: uint): Item;
	begin
		result := GetItem(IteratorFromIndex(index));
	end;

	function classname.Grow(by: uint): pItem;
	var
		n: pNode;
	begin
		n := last;
		if not Assigned(n) or (n^.used + by > n^.allocated) then
			n := AllocateAndAppendNode(by);

		result := pItem(n^.items) + n^.used;
		System.Initialize(result[0], by);
		n^.used += by;
		nItems += by;
	end;

	procedure classname.Pack;
{$ifdef store_ptr} var oldPtr: pNode; i: uint; {$endif}
	begin
		if Assigned(last) and (last^.used < last^.allocated) then
		begin
		{$ifdef store_ptr} oldPtr := last; {$endif}

			ReallocMem(last, sizeof(last^) - sizeof(last^.items) + sizeof(last^.items[0]) * last^.used);
			last^.allocated := last^.used;

		{$ifdef store_ptr}
			if oldPtr <> last then
				for i := 1 to last^.used do
				{$define node := last} {$define index := i - 1} {$define iters_unchanged} store_extra_wrap;
		{$endif}
		end;
	end;

{$ifdef dump}
	function classname.Dump: string;
	var
		c: pNode;
		i, nodeIndex: uint;
	begin
		result := 'N = ' + ToString(nItems) + ', узлы:';

		c := first;
		nodeIndex := 0;
		while Assigned(c) do
		begin
			result += EOL + ToString(nodeIndex) + ': (' + ToString(c^.used);
			if c^.allocated > c^.used then result += '/' + ToString(c^.allocated);
			if c = first then result += ', первый';
			if c = last then result += ', последний';
			result += ')';

			for i := 1 to c^.used do
			begin
				if i = 1 then result += ' ' else result += ', ';
				result += {$define _1 := pItem(c^.items)[i - 1]} item_to_string {$undef _1};
			end;

			c := c^.next;
			inc(nodeIndex);
		end;

		if not Assigned(first) then result += ' (нет)';
	end;
{$endif}

	function classname.AllocateAndAppendNode(guarantee: uint = 0): pNode;
	const
		MinAlloc = 8;
	var
		alloc: uint;
	begin
		alloc := MinAlloc + nItems div 2;
		if alloc < guarantee then alloc := guarantee;
		if alloc < MinAlloc then alloc := MinAlloc;
		alloc := align(alloc, sizeof(pointer));
		result := Node.Allocate(alloc {, prev = last});

		if Assigned(last) then last^.next := result else first := result;
		last := result;
	end;

	procedure classname.PopFromLast(n: uint);
	begin
		Assert(n <= last^.used);
		last^.used -= n; System.Finalize(pItem(last^.items)[last^.used], n);
		if last^.used = 0 then RemoveAndDestroyLast;
		nItems -= n;
	end;

	procedure classname.RemoveAndDestroyLast;
	var
		t: pNode;
	begin
		Assert(last^.used = 0);
		t    := last;
		last := FindPrev(last);
		if Assigned(last) then last^.next := nil else first := nil;
		FreeMem(t);
	end;

	function classname.InternalValid(const it: Iterator): boolean;
	begin
		result := Assigned(it.node);
	end;

	function classname.FindPrev(n: pNode): pNode;
	var
		c: pNode;
	begin
		result := nil;
		c := first;
		while Assigned(c) do
			if n = c then exit else
			begin
				result := c;
				c := c^.next;
			end;
	end;

// копипаст из vector.pp.inc
{$define each_any_matching_find :=
{$define result_type := boolean}
{$if defined(each)} {$define procname := Each}
{$elseif defined(Any)} {$define procname := Any}
{$elseif defined(Matching)} {$define procname := Matching} {$define result_type := uint}
{$elseif defined(Find_o)} {$define procname := Find}
{$elseif defined(Find_r)} {$define procname := Find} {$define result_type := Iterator}
{$else} {$error nothing from Each, Any, Matching, Find_o, Find_r defined}
{$endif}

	function classname.procname(f: predicate maybe_param {$ifdef Find_o}; out id: Iterator {$endif}): result_type;
	var
		c: pNode;
		i: uint;
	begin
	{$ifdef matching} result := 0; {$endif}
		c := first;
		while Assigned(c) do
		begin
			i := 0;
			while i < c^.used do
			begin
				if {$ifdef each} not {$endif} f(pItem(c^.items)[i] {$ifdef param_type}, param {$endif}) then
					{$if defined(each)} exit(no)
					{$elseif defined(any)} exit(yes)
					{$elseif defined(matching)} inc(result)
					{$elseif defined(Find_o)} begin id.node := c; id.index := i; {$ifdef Debug} id.validated := yes; {$endif} exit(yes); end
					{$elseif defined(Find_r)} begin result.node := c; result.index := i; {$ifdef Debug} result.validated := no; {$endif} exit; end
					{$else} {$error nothing defined} {$endif};
				inc(i);
			end;
			c := c^.next;
		end;

	{$if defined(any) or defined(Find_o)} result := no; {$endif}
	{$ifdef all} result := yes; {$endif}
	{$ifdef Find_r} result.node := nil; {$ifdef Debug} result.validated := no; {$endif} {$endif}
	end; {$undef procname} {$undef result_type}
	{$undef each} {$undef any} {$undef matching} {$undef Find_o} {$undef Find_r}}

{$define iterate_predicate :=
	{$define each} each_any_matching_find
	{$define any} each_any_matching_find
	{$define matching} each_any_matching_find
	{$define Find_o} each_any_matching_find
	{$define Find_r} each_any_matching_find} for_each_predicate
{$undef each_any_matching_find}

{$define iterate_item_proc :=
	procedure classname.ForEach(f: item_proc maybe_param; flags: ForEachFlags = []);
	var
		c: pNode;
		i: uint;
	begin
		if ReversedOrder in flags then
		begin
			c := last;
			while Assigned(c) do
			begin
				i := c^.used;
				while i > 0 do
				begin
					dec(i);
					f(pItem(c^.items)[i] {$ifdef param_type}, param {$endif});
				end;
				c := FindPrev(c);
			end;
		end else
		begin
			c := first;
			while Assigned(c) do
			begin
				i := 0;
				while i < c^.used do
				begin
					f(pItem(c^.items)[i] {$ifdef param_type}, param {$endif});
					inc(i);
				end;
				c := c^.next;
			end;
		end;
	end;} for_each_item_proc

end_vector_common
{$undef vector_common_no_elem_id}
{$undef extra_predicate_param} {$undef extra_itemproc_param} {$undef user_param}
{$undef classname} {$undef dump} {$undef item_to_string} {$undef store_ptr} {$undef store_iter} {$undef store_extra_wrap}
