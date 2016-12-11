{$ifdef detect_corruption} {$error} {$endif}
{$ifdef extra_predicate_param} {$error} {$endif}
{$ifdef extra_itemproc_param} {$error} {$endif}
{$ifdef user_param} {$error} {$endif}

{$define store_extra_wrap :=
	begin
	{$ifdef store_ptr}
		{$define item := _items[index]} {$define ptr := (_items + (index))} begin store_ptr end; {$undef item} {$undef ptr}
	{$endif}
	end {$undef index}}

	procedure classname._CompileTimeCheck;
	type
		DVC = classname.DetectVectorCorruption;
		HEP = classname.HasExtraPredicateParam;
		HIP = classname.HasExtraItemProcParam;
		UP  = classname.PassUserParam;
	begin
	{$if sizeof(DVC) > 0} {$define detect_corruption} {$endif}
	{$if sizeof(HEP) > 0} {$define extra_predicate_param} {$endif}
	{$if sizeof(HIP) > 0} {$define extra_itemproc_param} {$endif}
	{$if sizeof(UP) > 0} {$define user_param} {$endif}
	{$include vector.common.inc}
	end;

	procedure classname.Init({$ifdef user_param} const param: UserParam; {$endif} preallocate: uint = 0);
	{$ifdef detect_corruption}
		procedure InitSentinel(out sentinel: SentinelItemFootprint);
		begin
			NonCrucialRandom(@sentinel, sizeof(sentinel));
		end;
	{$endif}
	begin
		_n := 0;
		alloc := preallocate;
		_items := GetMem((alloc {$ifdef detect_corruption} + 2 {$endif}) * sizeof(Item));
	{$ifdef user_param} self.param := param; {$endif}

	{$ifdef detect_corruption}
		Assert(sizeof(SentinelItemFootprint) = sizeof(Item));
		InitSentinel(firstSentinel);
		InitSentinel(lastSentinel);
		_items += 1;
		Move(firstSentinel, (_items - 1)^, sizeof(Item));
		SetupLastSentinel;
	{$endif}
	end;

	procedure classname.Done;
	begin
	{$ifdef detect_corruption} CheckForCorruption; {$endif}
		System.Finalize(_items^, _n);
	{$ifdef detect_corruption} _items -= 1; {$endif}
		FreeMem(_items);
	end;

{$define constructor_args := {$ifdef user_param} const param: UserParam; {$endif} preallocate: uint = 0}
{$define pass_constructor_args := {$ifdef user_param} param, {$endif} preallocate}
{$define keep_classname_define} {$include dyn_obj.pp.inc} {$undef keep_classname_define}

	procedure classname.Push(const newItem: Item);
	begin
		Grow(1)^ := newItem;
	{$define index := _n - 1} store_extra_wrap;
	end;

	procedure classname.Push(newItems: pItem; count: uint);
	var
	{$ifdef store_ptr} start: uint; {$endif}
		i: uint;
		it: pItem;
	begin
	{$ifdef store_ptr} start := _n; {$endif}
		it := Grow(count);
		for i := 1 to count do
		begin
			it[i - 1] := newItems[i - 1];
		{$define index := start + (i - 1)} store_extra_wrap;
		end;
	end;

	procedure classname.Pop;
	{$ifdef store_ptr} var oldPtr: pItem; {$endif}
	begin
	{$ifdef detect_corruption} CheckForCorruption; {$endif}
		Assert(_n > 0);
		dec(_n);
		System.Finalize(_items[_n]);
	{$ifdef detect_corruption} SetupLastSentinel; {$endif}

		if 4 * _n < alloc then
		begin
		{$ifdef store_ptr} oldPtr := _items; {$endif}
			ReallocateWithMargin;
		{$ifdef store_ptr} if oldPtr <> _items then PostRelocate(_n); {$endif}
		end;
	end;

	procedure classname.RemoveReplace(id: uint);
	begin
		Assert(id < _n);
		if id + 1 < _n then
		begin
			_items[id] := _items[_n - 1];
		{$define index := id} store_extra_wrap;
		end;
		Pop;
	end;

	procedure classname.RemoveShift(id: uint);
	begin
		Assert(id < _n);
		while id + 1 < n do
		begin
			_items[id] := _items[id + 1];
		{$define index := id} store_extra_wrap;
			inc(id);
		end;
		Pop;
	end;

	procedure classname.Clear;
	begin
	{$ifdef detect_corruption} CheckForCorruption; {$endif}
		System.Finalize(_items^, _n);
		_n := 0;
	{$ifdef detect_corruption} SetupLastSentinel; {$endif}
		ReallocateWithMargin;
	end;

	function classname.Last: Item;
	begin
		Assert(_n > 0);
		result := _items[_n - 1];
	end;

	function classname.LastPtr: pItem;
	begin
		Assert(_n > 0);
		result := @_items[_n - 1];
	end;

	function classname.LastIndex: uint;
	begin
		Assert(_n > 0);
		result := _n - 1;
	end;

	function classname.Grow(by: uint): pItem;
	{$ifdef store_ptr} var oldPtr: pItem; {$endif}
	begin
	{$ifdef detect_corruption} CheckForCorruption; {$endif}
		result := _items + _n;
		_n += by;
		if _n > alloc then
		begin
		{$ifdef store_ptr} oldPtr := _items; {$endif}
			result := ReallocateWithMargin + (_n - by);
		{$ifdef store_ptr} if _items <> oldPtr then PostRelocate(_n - by); {$endif}
		end;
	{$ifdef detect_corruption} SetupLastSentinel; {$endif}
		System.Initialize(result^, by);
	end;

	procedure classname.Pack;
	{$ifdef store_ptr} var oldPtr: pItem; {$endif}
	begin
		if alloc > N then
		begin
		{$ifdef store_ptr} oldPtr := _items; {$endif}
			Reallocate(N);
		{$ifdef store_ptr} if _items <> oldPtr then PostRelocate(_n); {$endif}
		end;
	end;

	procedure classname.SetExactLength(len: uint);
	{$ifdef store_ptr} var oldPtr: pItem; {$endif}
	begin
	{$ifdef detect_corruption} CheckForCorruption; {$endif}
	{$ifdef store_ptr} oldPtr := _items; {$endif}
		if len < _n then System.Finalize(_items[len], _n - len);
		Reallocate(len);
		if len > _n then System.Initialize(_items[_n], len - _n);
		_n := len;
	{$ifdef store_ptr} if _items <> oldPtr then PostRelocate(len); {$endif}
	{$ifdef detect_corruption} SetupLastSentinel; {$endif}
	end;

	function classname.Reallocate(newAlloc: uint): pItem;
	begin
		alloc := newAlloc;
	{$ifdef detect_corruption} _items -= 1; {$endif}
		result := ReallocMem(_items, (alloc {$ifdef detect_corruption} + 2 {$endif}) * sizeof(Item));
	{$ifdef detect_corruption} _items += 1; result += 1; {$endif}
		Assert(_items = result);
	end;

	function classname.ReallocateWithMargin: pItem;
	begin
		result := Reallocate(2 * _n);
	end;

{$ifdef store_ptr}
	procedure classname.PostRelocate(n: uint);
	var
		i: uint;
	begin
		for i := 1 to n do
		{$define index := i - 1} store_extra_wrap;
	end;
{$endif}

{$ifdef detect_corruption}
	procedure classname.CheckForCorruption;
	begin
		if System.CompareByte((_items - 1)^, firstSentinel, sizeof(Item)) <> 0 then Fatal('Вектор повреждён (firstSentinel).');
		if System.CompareByte(_items[_n], lastSentinel, sizeof(Item)) <> 0 then Fatal('Вектор повреждён (lastSentinel).');
	end;

	procedure classname.SetupLastSentinel;
	begin
		System.Move(lastSentinel, _items[_n], sizeof(Item));
	end;
{$endif}

// также копипаст с изменениями в vector_link.pp.inc
{$define each_any_matching_find :=
{$define result_type := boolean}
{$if defined(each)} {$define procname := Each}
{$elseif defined(Any)} {$define procname := Any}
{$elseif defined(Matching)} {$define procname := Matching} {$define result_type := uint}
{$elseif defined(Find_o)} {$define procname := Find}
{$elseif defined(Find_r)} {$define procname := Find} {$define result_type := sint}
{$else} {$error nothing from Each, Any, Matching, Find_o, Find_r defined}
{$endif}

	function classname.procname(f: predicate maybe_param {$ifdef Find_o}; out id: uint {$endif}): result_type;
	var
		i: uint;
	begin
		i := 0;
	{$ifdef matching} result := 0; {$endif}
		while i < _n do
		begin
			if {$ifdef each} not {$endif} f(_items[i] {$ifdef elem_id}, i {$endif} {$ifdef param_type}, param {$endif})
			then
				{$if defined(each)} exit(no)
				{$elseif defined(any)} exit(yes)
				{$elseif defined(matching)} inc(result)
				{$elseif defined(Find_o)} begin id := i; exit(yes); end
				{$elseif defined(Find_r)} exit(i)
				{$else} {$error nothing defined} {$endif};
			inc(i);
		end;
	{$if defined(any) or defined(Find_o)} result := no; {$endif}
	{$ifdef all} result := yes; {$endif}
	{$ifdef Find_r} result := -1; {$endif}
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
		i: uint;
	begin
		if ReversedOrder in flags then
		begin
			i := n;
			while i > 0 do
			begin
				dec(i);
				f(_items[i] {$ifdef elem_id}, i {$endif} {$ifdef param_type}, param {$endif});
			end;
		end else
		begin
			i := 0;
			while i < _n do
			begin
				f(_items[i] {$ifdef elem_id}, i {$endif} {$ifdef param_type}, param {$endif});
				inc(i);
			end;
		end;
	end;} for_each_item_proc

end_vector_common
{$undef detect_corruption}
{$undef extra_predicate_param} {$undef extra_itemproc_param} {$undef user_param}
{$undef store_ptr} {$undef store_extra_wrap}
{$undef classname}
