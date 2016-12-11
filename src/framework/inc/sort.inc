{$ifndef procname} {$define procname := sort} {$endif}
{$ifdef sort_less} {$error} {$endif}
{$define sort_less := ({$ifdef less} less {$else} _1 < _2 {$endif}) {$undef _1} {$undef _2}}
{-$define generic_implementation}

{$include unique_def.inc}
{$define sort_elem := unique_def}
{$define sort_pelem := unique_def2}
type
	sort_elem = elem;
	sort_pelem = ^sort_elem;

	procedure procname(p: sort_pelem; count: uint);
	const
		InsertionThreshold = 12;
		HeapArity = {$ifdef force_heap_arity} {$note задана пользовательская арность кучи} force_heap_arity {$else} 3 {$endif};

		procedure InsertionSort(p: sort_pelem; count: sint);
		var
			i, j, imin: sint;
			t: elem;
		begin
			for i := 0 to count - 2 do
			begin
				imin := i;
				for j := i + 1 to count - 1 do
					{$define _1 := p[j]} {$define _2 := p[imin]}
					if sort_less then imin := j;

				if i <> imin then
				begin
					t := p[i];
					p[i] := p[imin];
					p[imin] := t;
				end;
			end;
		end;

		procedure HeapDown(p: sort_pelem; count, id: sint);
		var
			swap, child, i: sint;
			t: elem;
		begin
			swap := id;
			repeat
				child := HeapArity * id;
				for i := 0 to HeapArity - 1 do
				begin
					inc(child);
					if (child < count) and {$define _1 := p[swap]} {$define _2 := p[child]} sort_less then swap := child;
				end;
				if swap <> id then
				begin
					t := p[id];
					p[id] := p[swap];
					p[swap] := t;
					id := swap;
				end else
					break;
			until no;
		end;

		procedure HeapSort(p: sort_pelem; count: sint);
		var
			i: sint;
			t: elem;
		begin
			for i := (count + HeapArity - 1) div HeapArity - 1 downto 0 do
				HeapDown(p, count, i);
			for i := count - 1 downto 1 do
			begin
				t := p[i];
				p[i] := p[0];
				p[0] := t;
				HeapDown(p, i, 0);
			end;
		end;

		function Median(p: sort_pelem; count: sint): sint;
		var
			avg: sint absolute result;
		begin
			avg := count div 2;
			if {$define _1 := p[0]} {$define _2 := p[avg]} sort_less then // first < avg
				if {$define _1 := p[avg]} {$define _2 := p[count-1]} sort_less then // first < avg < last
				else // avg >= first, avg >= last
					if {$define _1 := p[0]} {$define _2 := p[count-1]} sort_less then // first < last <= avg
						avg := count - 1
					else // last <= first <= avg
						avg := 0
			else // avg <= first
				if {$define _1 := p[count-1]} {$define _2 := p[avg]} sort_less then // last < avg <= first
				begin
				end else // avg <= first, avg <= last
					if {$define _1 := p[0]} {$define _2 := p[count-1]} sort_less then // avg <= first < last
						avg := 0
					else // avg <= last <= first
						avg := count - 1;
		end;

		procedure QSort(p: sort_pelem; count: sint; maxDepth: uint);
		var
			L, R, avg {$ifndef generic_implementation}, i {$endif}: sint;
			t: elem;
		begin
			if count = 0 then exit;
			repeat
				if count <= InsertionThreshold then
				begin
					InsertionSort(p, count);
					exit;
				end;
				if maxDepth = 0 then
				begin
				{$ifdef Debug} USystem.stat.Increment(qsort_fallbacks); {$endif}
					HeapSort(p, count);
					exit;
				end;

				avg := Median(p, count);
				R := 0;
				L := count - 1;

			{$ifdef generic_implementation}
				{$note using generic implementation}
				repeat
					{$define _1 := p[R]} {$define _2 := p[avg]} while sort_less do inc(R);
					{$define _1 := p[avg]} {$define _2 := p[L]} while sort_less do dec(L);
					if R <= L then
					begin
						t := p[R];
						p[R] := p[L];
						p[L] := t;
						if R = avg then avg := L else
							if L = avg then avg := R;
						inc(R);
						dec(L);
					end;
				until R > L;
			{$else}
				// Оптимизировано под сценарий, когда все элементы разные. В отличие от варианта выше, быстрее, но может зафейлить разделение массива.
				repeat
					{$define _1 := p[avg]} {$define _2 := p[R]} while (R <= L) and not sort_less do inc(R);
					{$define _1 := p[L]} {$define _2 := p[avg]} while (R <= L) and not sort_less do dec(L);
					Assert(R <> L, 'R = L');
					if R < L then
					begin
						t := p[R];
						p[R] := p[L];
						p[L] := t;
						if R = avg then avg := L else
							if L = avg then avg := R;
						inc(R);
						dec(L);
					end;
				until R > L;

				// Если зафейлилось (одна из частей выродилась в 0) — вероятно, в массиве много одинаковых элементов, в этом случае эффективнее метод ниже.
				if (L < 0) or (R >= count) then
				begin
					L := -1;
					R := count;
					i := 0;
					while i < R do
					begin
						{$define _1 := p[i]} {$define _2 := p[avg]} if sort_less then
						begin
							inc(L);
							t := p[i];
							p[i] := p[L];
							p[L] := t;
							if i = avg then avg := L else
								if L = avg then avg := i;
						end;
						inc(i);
					end;

					i := count - 1;
					while i > L do
					begin
						{$define _1 := p[avg]} {$define _2 := p[i]} if sort_less then
						begin
							dec(R);
							t := p[i];
							p[i] := p[R];
							p[R] := t;
							if i = avg then avg := R else
								if R = avg then avg := i;
						end;
						dec(i);
					end;
				end;
			{$endif}

				// [0 .. L], [R .. count - 1]
				if count - R <= L then
				begin
					if R + 1 < count then QSort(p + R, count - R, maxDepth - 1);
					count := L + 1;
				end else
				begin
					if L > 0 then QSort(p, L + 1, maxDepth - 1);
					p += R;
					count -= R;
				end;
			until count <= 0;
		end;

		procedure Sort(p: sort_pelem; count: sint);
		begin
		{$ifdef Debug} USystem.stat.Increment(qsorts); {$endif}
			QSort(p, count, {$ifdef no_math} bitsizeof(count) {$else} Log2(1 + uint(count)) {$endif});
		end;

	{$if defined(Debug) and not defined(less_is_consciously_intransitive)}
		procedure CheckSorted(p: sort_pelem; count: uint);
		var i: uint;
		begin
			if count < 2 then exit;
			for i := 0 to count - 2 do
				if {$define _1 := p[i + 1]} {$define _2 := p[i]} sort_less then
					raise Error('Сортировка не выполнена (вероятно, некоммутативный или нетранзитивный less)');
		end;
	{$endif}

	begin
	{$if defined(force_insert)} {$note форсирована сортировка вставками} InsertionSort
	{$elseif defined(force_heap)} {$note форсирована пирамидальная сортировка} HeapSort
	{$else} Sort
	{$endif} (p, count);
	{$if defined(Debug) and not defined(less_is_consciously_intransitive))} CheckSorted(p, count); {$endif}
	end;

{$ifdef openarray}
	procedure procname(var p: array of sort_elem);
	begin
		procname(sort_pelem(p), length(p));
	end;
{$endif}

end_unique_def
{$undef sort_elem} {$undef sort_pelem} {$undef sort_less}
{$undef force_insert} {$undef force_heap} {$undef force_heap_arity} {$undef generic_implementation}
{$undef elem} {$undef procname} {$undef less} {$undef openarray} {$undef no_math} {$undef less_is_consciously_intransitive)}
