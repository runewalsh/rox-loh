{$ifdef watch_handles} {$error} {$endif}
{$ifdef heap} {$error} {$endif}
{$ifdef fib} {$error} {$endif}
{$ifdef rand} {$error} {$endif}
{$ifdef allow_prioritize} {$error} {$endif}
{$ifdef allow_delete} {$error} {$endif}
{$ifdef allow_union} {$error} {$endif}
{$ifdef FinalizeItem} {$error} {$endif}
{$ifdef FinalizationRequired} {$error} {$endif}
{$ifdef DebugFH} {$error} {$endif}
{$define FinalizeItem := {$ifdef finalize_item} begin finalize_item end {$endif} {$undef _1}}
{$if defined(finalize_item)} {$define FinalizationRequired} {$endif}
{-$define DebugFH}

	procedure CompileTimeCheck(var t: classname);
	type
		AP = classname.AllowPrioritize;
		AD = classname.AllowDelete;
		AU = classname.AllowUnion;
		MB = classname.MoreIsBetter;
		FI = classname.Fib;
		RA = classname.RandImp;
		NA = classname.Naive;
	begin
		Assert(@t = @t);
		{$if sizeof(AP) > 0} {$define allow_prioritize} {$endif}
		{$if sizeof(AD) > 0} {$define allow_delete} {$endif}
		{$if sizeof(AU) > 0} {$define allow_union} {$endif}
		{$if sizeof(MB) > 0} {$define better_than := >} {$else} {$define better_than := <} {$endif}
		{$if sizeof(FI) > 0} {$define fib} {$if (sizeof(RA) > 0) or (sizeof(NA) > 0)} {$error} {$endif}
		{$elseif sizeof(RA) > 0} {$define rand} {$if (sizeof(FI) > 0) or (sizeof(NA) > 0)} {$error} {$endif}
		{$elseif sizeof(NA) > 0} {$define naive} {$if (sizeof(FI) > 0) or (sizeof(RA) > 0)} {$error} {$endif}
		{$else} {$define heap} {$endif}
		{$if defined(allow_prioritize) or defined(allow_delete)} {$define watch_handles} {$endif}

	{$ifdef rand}
		Assert(classname.RandomChildBitsRequired <= bitsizeof(classname.rand) div 4);
	{$endif}
	end;

	constructor classname.Init;
	begin
		nodes := 0;

	{$if defined(heap) or defined(naive)}
		allocated := 0;
		n := nil;
	{$elseif defined(fib)}
		degreeLim := not uint(0);
		consBuf := nil;
		min := nil;
		root := nil;
	{$elseif defined(rand)}
		root := nil;
		rng.Init({GenericQuick}Crawl, '123');
		randBits := 0;
	{$else} {$error} {$endif}
	end;

	destructor classname.Done;
	begin
		// fib: Внимание, должно корректно сработать на полу-уничтоженном аргументе Consume!
		Clear;
	{$if defined(heap) or defined(naive)} // ничего
	{$elseif defined(fib)} FreeMem(consBuf);
	{$elseif defined(rand)} rng.Done;
	{$else} {$error} {$endif}
	end;

	procedure classname.Clear;
	{$ifdef fib}
		procedure WipeRow(start: pNode);
		var
			x, c: pNode;
		begin
			if not Assigned(start) then exit;
			c := start;
			repeat
				WipeRow(c^.child);
				x := c;
				c := c^.right;
				{$define _1 := x^.item} FinalizeItem;
				dispose(x);
			until c = start;
		end;
	{$endif}
	{$ifdef rand}
		procedure Wipe(n: pNode);
		var
			c: pNode;
		begin
			if not Assigned(n) then exit;
			for c in n^.childs do Wipe(c);
			{$define _1 := n^.item} FinalizeItem;
			dispose(n);
		end;
	{$endif}
	{$if (defined(heap) or defined(naive)) and defined(FinalizationRequired)} var i: sint; {$endif}
	begin
	{$if defined(heap) or defined(naive)}
	{$ifdef FinalizationRequired}
		for i := 1 to nodes do
		begin
			{$define _1 := n[i-1].item} FinalizeItem;
		end;
	{$endif}
		System.Finalize(n^, nodes);
		FreeMem(n);
		allocated := 0;

	{$elseif defined(fib)}
		WipeRow(root);
		min := nil;
		root := nil;

	{$elseif defined(rand)}
		Wipe(root);
		root := nil;
	{$else} {$error} {$endif}
		nodes := 0;
	end;

	procedure classname.Put(const item: ItemType; const priority: PriorityType {$ifdef watch_handles} ; handle: pHandle_t {$endif});
{$if defined(heap) or defined(naive)}
	var
		id: uint;
	begin
		id := nodes;
		Resize(id + 1);
		System.Initialize(n[id]);
		n[id].item := item;
		n[id].priority := priority;
	{$ifdef watch_handles}
		n[id].handle := handle;
		handle^ := 1 + id;
	{$endif}
	{$ifdef heap} HeapifyUp(id); {$endif}
	end;
{$elseif defined(fib) or defined(rand)}
	var
		e: pNode;
	begin
		new(e);
		e^.item := item;
		e^.priority := priority;
	{$ifdef fib}
		e^.degree := 0;
		e^.marked := no;
		e^.parent := nil;
		e^.child := nil;
		e^.left := e;
		e^.right := e;
	{$endif}
	{$ifdef rand} inc(nodes); {$endif}
		InsertNode(e);
	{$ifdef watch_handles} handle^ := e; {$endif}
	end;
{$else} {$error} {$endif}

	function classname.Peek: pItem;
	begin
	{$if defined(heap)} if nodes > 0 then result := @n[0].item else result := nil;
	{$elseif defined(fib)} if Assigned(min) then result := @min^.item else result := nil;
	{$elseif defined(rand)} if Assigned(root) then result := @root^.item else result := nil;
	{$elseif defined(naive)} if nodes > 0 then result := @n[FindMin].item else result := nil;
	{$else} {$error} {$endif}
	end;

	function classname.Get(out item: ItemType): boolean;
	{$if defined(fib) or defined(rand)} var z: pNode; {$endif}
	{$ifdef naive} var im: uint; {$endif}
	begin
	{$if defined(heap)}
		result := nodes > 0;
		if result then
		begin
			item := n[0].item;
			DeleteNode(0);
		end;
	{$elseif defined(fib)}
		result := Assigned(min);
		if result then
		begin
			z := ExtractMin;
			item := z^.item;
			dispose(z);
		end;
	{$elseif defined(rand)}
		z := root;
		Assert((nodes > 0) = Assigned(z));
		result := Assigned(z);
		if result then
		begin
			item := z^.item;
			DeleteNode(z);
			dec(nodes);
			dispose(z);
		end;
	{$elseif defined(naive)}
		result := nodes > 0;
		if result then
		begin
			im := FindMin;
			item := n[im].item;
			DeleteNode(im);
		end;
	{$else} {$error} {$endif}
	end;

{$ifdef allow_prioritize}
	{$define hn_impl :=
		var
			id: uint;
		{$if defined(heap) and defined(prioritize)} better: boolean; {$endif}
		begin
			Assert(handle <> NullHandle);
			id := handle - 1;
		{$if defined(bump)} Assert(not (n[id].priority better_than newPriority), 'PriorityQueue.Bump не предназначена для снижения приоритета.');
		{$elseif defined(relax)} Assert(not (newPriority better_than n[id].priority), 'PriorityQueue.Relax не предназначена для повышения приоритета.');
		{$elseif defined(prioritize)} {$ifdef heap} better := newPriority better_than n[id].priority; {$endif}
		{$else} {$error} {$endif}

			n[id].priority := newPriority;

		{$ifdef heap}
			{$if defined(bump)} HeapifyUp(id);
			{$elseif defined(relax)} HeapifyDown(id);
			{$elseif defined(prioritize)} if better then HeapifyUp(id) else HeapifyDown(id);
			{$else} {$error} {$endif}
		{$endif}
		end; {$undef bump} {$undef relax} {$undef prioritize}}
	{$define x_change_priority_and_reinsert := begin x^.priority := newPriority; DeleteNode(x); InsertNode(x); end}

	procedure classname.Bump({$ifdef Debug} var {$endif} handle: handle_t; const newPriority: PriorityType);
	{$if defined(heap) or defined(naive)} {$define bump} hn_impl
	{$elseif defined(fib) or defined(rand)}
	var
		x: pNode absolute handle;
	begin
		Assert(not (x^.priority better_than newPriority));
		{$ifdef fib} InternalBump(x, @newPriority); {$endif}
		{$ifdef rand} x_change_priority_and_reinsert; {$endif}
	end;
	{$else} {$error} {$endif}

	procedure classname.Relax({$ifdef Debug} var {$endif} handle: handle_t; const newPriority: PriorityType);
	{$if defined(heap) or defined(naive)} {$define relax} hn_impl
	{$elseif defined(fib) or defined(rand)}
	var
		x: pNode absolute handle;
	begin
		Assert(not (newPriority better_than x^.priority));
		x_change_priority_and_reinsert;
	end;
	{$else} {$error} {$endif}

	procedure classname.Prioritize({$ifdef Debug} var {$endif} handle: handle_t; const newPriority: PriorityType);
	{$if defined(heap) or defined(naive)} {$define prioritize} hn_impl
	{$elseif defined(fib) or defined(rand)}
	var
		x: pNode absolute handle;
	begin
		{$ifdef fib} if x^.priority better_than newPriority then x_change_priority_and_reinsert else InternalBump(x, @newPriority); {$endif}
		{$ifdef rand} x_change_priority_and_reinsert; {$endif}
	end;
	{$else} {$error} {$endif}
	{$undef hn_impl}
	{$undef x_change_priority_and_reinsert}
{$endif}

{$ifdef allow_delete}
	procedure classname.Delete({$ifdef Debug} var {$endif} handle: handle_t);
	{$if defined(fib) or defined(rand)} var x: pNode absolute handle; {$endif}
	begin
		Assert(handle <> NullHandle);
	{$if defined(heap) or defined(naive)}
		{$define _1 := n[handle - 1].item} FinalizeItem;
		DeleteNode(handle - 1);
	{$elseif defined(fib) or defined(rand)}
		DeleteNode(x);
		{$define _1 := x^.item} FinalizeItem;
		dispose(x);
	{$else} {$error} {$endif}
	end;
{$endif}

{$ifdef allow_union}
	procedure classname.Consume(var victim: classname);
	var
		x: pNode;
	begin
	{$if defined(fib)}
		if not Assigned(victim.root) then exit;
		if Assigned(root) then
		begin
			root^.left^.right := victim.root;
			victim.root^.left^.right := root;
			x := root^.left;
			root^.left := victim.root^.left;
			victim.root^.left := x;
		end else
			root := victim.root;
		nodes += victim.nodes;
		if (not Assigned(min)) or (victim.min^.priority better_than min^.priority) then min := victim.min;
		victim.root := nil;
		victim.Done;
	{$elseif defined(rand)}
		root := Meld(victim.root, root, no); Assert(not Assigned(root^.parent));
		nodes += victim.nodes;
		victim.root := nil;
		victim.Done;
	{$else} {$error} {$endif}
	end;

	end;
{$endif}

{$ifdef heap}
	procedure classname.HeapifyDown(id: uint);
	var
		toSwap, child, i: uint;
		t: tNode;
	begin
	{$ifdef watch_handles} Assert(n[id].handle^ = 1 + id, ToString(n[id].handle^) + ' ' + ToString(id)); {$endif}
		toSwap := id;
		repeat
			child := HeapArity * id;
			for i := 0 to HeapArity - 1 do
			begin
				inc(child);
				if (child < nodes) and (n[child].priority better_than n[toSwap].priority) then toSwap := child;
			end;
			if toSwap <> id then
			begin
				t := n[toSwap];
				n[toSwap] := n[id];
				n[id] := t;
			{$ifdef watch_handles} n[id].handle^ := 1 + id; {$endif}
				id := toSwap;
			end else
			begin
			{$ifdef watch_handles} n[id].handle^ := 1 + id; {$endif}
				break;
			end;
		until no;
	end;

	procedure classname.HeapifyUp(id: uint);
	var
		parent: uint;
		t: tNode;
	begin
	{$ifdef watch_handles} Assert(n[id].handle^ = 1 + id, ToString(n[id].handle^) + ' ' + ToString(id)); {$endif}
		while id > 0 do
		begin
			parent := (id - 1) div HeapArity;
			if n[id].priority better_than n[parent].priority then
			begin
				t := n[id];
				n[id] := n[parent];
				n[parent] := t;
			{$ifdef watch_handles} n[id].handle^ := 1 + id; {$endif}
				id := parent;
			end else
				break;
		end;
	{$ifdef watch_handles} n[id].handle^ := 1 + id; {$endif}
	end;
{$endif heap}

{$if defined(heap) or defined(naive)}
	procedure classname.Resize(newN: uint);
	var
		newAllocated: uint;
	begin
		nodes := newN;
		if (newN > allocated) or (newN < allocated div 16) then
		begin
			newAllocated := UpToPow2(2 * newN + 1) - 1;
			Assert((newAllocated <> allocated) and (newAllocated >= newN));
			allocated := newAllocated;
			ReallocMem(n, newAllocated * sizeof(tNode));
		end;
	end;

	procedure classname.DeleteNode(id: uint);
	var
		last: uint;
	begin
	{$ifdef watch_handles} n[id].handle^ := NullHandle; {$endif}
		last := nodes - 1;
		n[id] := n[last];
		System.Finalize(n[last]);
		Resize(last);
		if id < last then
		begin
		{$ifdef watch_handles} n[id].handle^ := 1 + id; {$endif}
		{$ifdef heap} HeapifyDown(id); {$endif}
		end;
	end;
{$endif}

{$ifdef fib}
	procedure classname.DeleteNode(x: pNode);
	begin
		InternalBump(x, nil);
		if ExtractMin <> x then Assert(no);
	end;

	procedure classname.InternalBump(x: pNode; newPriority: pPriority);
	var
		infinite: boolean;
		y: pNode;
	begin
		infinite := not Assigned(newPriority);
		if not infinite then x^.priority := newPriority^;
		y := x^.parent;

		if Assigned(y) and (infinite or (newPriority^ better_than y^.priority)) then
		begin
			Cut(x, y);
			CascadingCut(y);
		end;

		if infinite or (newPriority^ better_than min^.priority) then min := x;
	end;

	function classname.ExtractMin: pNode;
	var
		x, orig, CL, RL: pNode;
	begin
		result := min;

		// всех детей в корневой список
		{x := result^.child;
		orig := x;
		if Assigned(x) then
			repeat
				y := x^.right;
				x^.p := nil;
				InsertRoot(x);
				x := y;
			until x = orig;}
		orig := result^.child;
		if Assigned(orig) then
		begin
			x := orig;
			repeat
				x^.parent := nil;
				x := x^.right;
			until x = orig;

			//     ->    ->        ->                ->    ----\  /-->       ->
			// ...    CL    Child     ...        ...    CL      \/     Child    ...
			//     <-    <-        <-                <-    <-\   \/---       <-
			//                               =>                \   \
			//     ->    ->      ->                  ->    --/ \   \->      ->
			// ...    RL    Root    ...          ...    RL     /\      Root    ...
			//     <-    <-      <-                  <-    <--/  \----      <-

			CL := x^.left;
			RL := root^.left;
			CL^.right := root;
			root^.left := CL;
			RL^.right := x;
			x^.left := RL;
		end;

		RemoveRoot(result);
		dec(nodes);

		if nodes = 0 then min := nil else
		begin
			min := result^.right;
			Consolidate;
		end;
	end;

	procedure classname.InsertRoot(x: pNode);
	begin
		if Assigned(root) then InsertAfter(root, x) else
		begin
			root := x;
			x^.left := x;
			x^.right := x;
		end;
	end;

	procedure classname.RemoveRoot(x: pNode);
	begin
		if x^.left = x then root := nil else root := RemoveNode(x);
	end;

	procedure classname.Consolidate;
	var
		a: ppNode;
		w, y, x: pNode;
		i, d: uint;
	begin
		a := ConsolidationBuffer;
		for i := 0 to degreeLim do a[i] := nil;

		while Assigned(root) do
		begin
			w := root;
			x := w;
			RemoveRoot(w);
			d := x^.degree; Assert(d <= degreeLim);

			while Assigned(a[d]) do
			begin
				y := a[d];
				if y^.priority better_than x^.priority then Swap(pointer(x), pointer(y));
				HeapLink(y, x);
				a[d] := nil;
				inc(d); Assert(d <= degreeLim);
			end;
			a[d] := x;
		end;
		min := nil;
		for i := 0 to degreeLim do
			if Assigned(a[i]) then
			begin
				InsertRoot(a[i]);
				if (not Assigned(min)) or (a[i]^.priority better_than min^.priority) then
					min := a[i];
			end;
	end;

	procedure classname.HeapLink(y, x: pNode);
	begin
		if Assigned(x^.child) then
			InsertBefore(x^.child, y)
		else
			x^.child := y;
		y^.parent := x;
		inc(x^.degree);
		y^.marked := no;
	end;

	procedure classname.Cut(x, y: pNode);
	begin
		RemoveNode(x);
		dec(y^.degree);
		InsertRoot(x);
		x^.parent := nil;
		x^.marked := no;
	end;

	procedure classname.CascadingCut(y: pNode);
	var
		z: pNode;
	begin
		while Assigned(y^.parent) do
		begin
			if not y^.marked then
			begin
				y^.marked := yes;
				break;
			end;

			z := y^.parent;
			Cut(y, z);
			y := z;
		end;
	end;

	procedure classname.InsertAfter(a, b: pNode);
	begin
		if a = a^.right then
		begin
			a^.right := b;
			a^.left := b;
			b^.right := a;
			b^.left := a;
		end else
		begin
			b^.right := a^.right;
			a^.right^.left := b;
			a^.right := b;
			b^.left := a;
		end;
	end;

	procedure classname.InsertBefore(a, b: pNode);
	begin
		InsertAfter(a^.left, b);
	end;

	function classname.RemoveNode(x: pNode): pNode;
	begin
		if x = x^.left then result := nil else result := x^.left;
		if Assigned(x^.parent) and (x^.parent^.child = x) then x^.parent^.child := result;
		x^.right^.left := x^.left;
		x^.left^.right := x^.right;

		x^.parent := nil;
		x^.left := x;
		x^.right := x;
	end;

	function classname.ConsolidationBuffer: ppNode;
	var
		old: uint;
	begin
		result := consBuf;
		Assert(nodes > 0);
		if (0 = not degreeLim) or (nodes > uint(1 shl degreeLim)) then
		begin
			old := degreeLim;
			degreeLim := 1 + CeilLog2(nodes);
			if old <> degreeLim then
			begin
			{$ifdef DebugFH} Log('FibHeap.Realloc(consBuf): ' + ToString(old) + ' -> ' + ToString(degreeLim)); {$endif}
				result := ReallocMem(consBuf, sizeof(consBuf^) * (degreeLim + 1));
			end;
		end;
	end;

	procedure classname.InsertNode(x: pNode);
	begin
		InsertRoot(x);
		if (not Assigned(min)) or (x^.priority better_than min^.priority) then min := x;
		inc(nodes);
	end;
{$endif fib}

{$ifdef rand}
	function classname.Meld(a, b: pNode; checkParent: boolean; parentIfB: pNode = nil): pNode;
	var
		c: ppNode;
	begin
		if not Assigned(b) then exit(a);
		if not Assigned(a) then
		begin
			if checkParent then b^.parent := parentIfB;
			exit(b);
		end;
		if b^.priority better_than a^.priority then
		begin
			if checkParent then b^.parent := parentIfB;
			result := b;
			b := a;
		end else
			result := a;

		c := @result^.childs[RandomChild];
		c^ := Meld(c^, b, yes, result);
		if checkParent and Assigned(result) then assert(result^.parent = parentIfB);
	end;

	procedure classname.InsertNode(x: pNode);
	var
		i: sint;
	begin
		for i := 0 to High(x^.childs) do
			x^.childs[i] := nil;
		root := Meld(root, x, yes, nil);
	end;

	procedure classname.DeleteNode(x: pNode);
	var
		c, p, cur: pNode;
	begin
		p := x^.parent;
		c := nil;
		for cur in x^.childs do
			if Assigned(cur) then
				if Assigned(c) then c := Meld(c, cur, no) else c := cur;

		Assert((x = root) = not Assigned(p));
		if Assigned(p) then
			p^.childs[LocateChild(p, x)] := c
		else
			root := c;
		if Assigned(c) then c^.parent := p;
	end;

	function classname.LocateChild(parent, x: pNode): sint;
	begin
		result := 0;
		while (result < High(parent^.childs)) and (parent^.childs[result] <> x) do
			inc(result);
		Assert(parent^.childs[result] = x);
	end;

	function classname.RandomChild: uint;
	var
		restBits: uint;
	begin
		if randBits >= RandomChildBitsRequired then
		begin
			result := rand and (1 shl RandomChildBitsRequired - 1);
			rand := rand shr RandomChildBitsRequired;
			randBits -= RandomChildBitsRequired;
		end else
		begin
			restBits := RandomChildBitsRequired - randBits;
			result := rand shl restBits;
			rand := rng.GetUint;
			result := result or (rand and (1 shl restBits - 1));
			rand := rand shr restBits;
			randBits := bitsizeof(rand) - restBits;
		end;
	{$if sizeof(ChildIndexDifferenceT) > 0}
		{$note модифицированный алгоритм (реализация)}
		result := result mod length(tNode.childs);
	{$endif}
	end;
{$endif}

{$ifdef naive}
	function classname.FindMin: uint;
	var
		i: uint;
	begin
		Assert(nodes > 0);
		result := 0;
		for i := 1 to nodes - 1 do
			if n[i].priority better_than n[result].priority then result := i;
	end;
{$endif}

{$undef allow_prioritize}
{$undef allow_delete}
{$undef allow_union}
{$undef watch_handles} {$undef fib} {$undef heap} {$undef naive} {$undef rand} {$undef better_than} {$undef DebugFH}
{$undef finalize_item} {$undef FinalizeItem} {$undef FinalizationRequired}
{$undef classname}
