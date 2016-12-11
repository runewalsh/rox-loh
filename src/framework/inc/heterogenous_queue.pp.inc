{$ifdef threading} {$error} {$endif}
{$ifdef msg_wait} {$error} {$endif}
	procedure classname.CompileTimeCheck;
	type
		TH = Threading;
		MW = MsgWait;
	begin
		{$if sizeof(TH) > 0} {$define threading} {$endif}
		{$if sizeof(MW) > 0} {$define msg_wait} {$endif}
	end;

	procedure classname.Init;
	begin
	{$ifdef threading}
		opLock.Init;
		getLock.Init;
		{$ifdef msg_wait} thereAreItems.Init([AllowMultiWait]); {$else} heyThereIsItem.Init; {$endif}
	{$endif}

		first := nil;
		last  := nil;
		emptyBlock := nil;
		sumSize := 0;
		sumAlloc := 0;
	{$ifdef Debug}
		nItems  := 0;
		nBlocks := 0;
		stats.allocations := 0;
		stats.maxSumSize  := 0;
		stats.maxSumAlloc := 0;
	{$endif}
	end;

	procedure classname.Done;
	var
		cur, next: pBlock;
	begin
		cur := first;
		if Assigned(cur) then
			repeat
				next := cur^.next;
				Destroy(cur);
				cur := next;
			until cur = first
		else
			if Assigned(emptyBlock) then Destroy(emptyBlock);

	{$ifdef threading}
		{$ifdef msg_wait} thereAreItems.Done; {$else} heyThereIsItem.Done; {$endif}
		getLock.Done;
		opLock.Done;
	{$endif}
	end;

	function classname.{$ifdef threading} LockPut {$else} Put {$endif}(size: size_t): pointer;
	var
		b: pBlock;
		allocate: size_t;
	begin
		size := align(size, Alignment);
	{$ifdef threading}
		opLock.Enter;
		wasEmpty := sumSize = 0;
	{$endif}
		sumSize += size;

	{$ifdef Debug}
		inc(nItems);
		if sumSize > stats.maxSumSize then stats.maxSumSize := sumSize;
		if nItems > stats.maxItems then stats.maxItems := nItems;
	{$endif}

		// умещается в last?
		if Assigned(last) and (last^.dataEnd + size <= last^.allocated) then
		begin
			result        := pointer(@last^.data) + last^.dataEnd;
			last^.dataEnd += size;
		end else

		// умещается в emptyBlock?
		if Assigned(emptyBlock) and (size <= emptyBlock^.allocated) then
		begin
			last       := emptyBlock;
			emptyBlock := nil;
			result     := pointer(@last^.data);
			last^.dataStart := 0;
			last^.dataEnd   := size;

			// он был единственным?
			if not Assigned(first) then begin first := last; last^.next := first; end;
		end else

		// создать новый блок
		begin
			allocate := align(sumAlloc div 4 + sumSize + 16 * size, Alignment);
			b := GetMem(sizeof(Block) - sizeof(Block.data) + allocate);
			sumAlloc += allocate;
		{$ifdef Debug}
			inc(stats.allocations);
			inc(nBlocks);
			if nBlocks > stats.maxBlocks then stats.maxBlocks := nBlocks;
			if sumAlloc > stats.maxSumAlloc then stats.maxSumAlloc := sumAlloc;
		{$endif}
			b^.allocated := allocate;
			b^.dataStart := 0;
			b^.dataEnd   := size;
			result := pointer(@b^.data);

			// вставить в цепочку
			if Assigned(last) then
			begin
				b^.next := last^.next;
				last^.next := b;
			end else

			// цепочка была пустой
			begin
				first := b;

				// возможно, emptyBlock существовал, но не прошёл по размеру — соблюдаем инвариант, что если он существует,
				// то всегда вставлен между last и first
				if Assigned(emptyBlock) then
				begin
					b^.next := emptyBlock;
					emptyBlock^.next := b;
				end else
					b^.next := b;
			end;
			last := b;
		end;
	end;

{$ifdef threading}
	procedure classname.UnlockPut;
	var
		we: boolean;
	begin
		we := wasEmpty;
		opLock.Leave;
		if we then {$ifdef msg_wait} thereAreItems.SetEvent {$else} heyThereIsItem.WakeOne {$endif};
	end;
{$endif}

	function classname.LockGet({$ifdef threading} wait: boolean {$endif}): pointer;
	begin
	{$ifdef threading}
		getLock.Enter;
		opLock.Enter;
		if sumSize = 0 then
		begin
			if not wait then
			begin
				opLock.Leave;
				getLock.Leave;
				exit(nil);
			end;

			{$ifndef msg_wait} heyThereIsItem.StartWait; {$endif}
			repeat
			{$ifdef msg_wait}
				opLock.Leave;
				if ThreadEvent.WaitAny(@thereAreItems, [OrMessage]) = 1 then begin getLock.Leave; exit(nil); end;
				opLock.Enter;
			{$else}
				heyThereIsItem.Wait(opLock);
			{$endif}
			until sumSize <> 0;
			{$ifndef msg_wait} heyThereIsItem.EndWait; {$endif}
		end;
	{$else}
		if sumSize = 0 then exit(nil);
	{$endif}

		result := pointer(@first^.data) + first^.dataStart;
	{$ifdef threading} opLock.Leave; {$endif}
	end;

	procedure classname.UnlockGet(size: size_t);
	var
		next: pBlock;
	begin
		size := align(size, Alignment);
	{$ifdef threading} opLock.Enter; {$endif}
		Assert(sumSize >= size);
		sumSize -= size;
	{$ifdef Debug} dec(nItems); {$endif}

		first^.dataStart += size;
		Assert(first^.dataStart <= first^.dataEnd, 'Похоже, размер в Get не соответствовал размеру в Put.');
		// блок стал пустым?
		if first^.dataStart >= first^.dataEnd then
		begin
			// если emptyBlock уже существовал — оставить больший
			if Assigned(emptyBlock) then
			begin
				// оставить first?
				if first^.allocated > emptyBlock^.allocated then
				begin
					Destroy(emptyBlock);
					emptyBlock := first;
					if first = last then
					begin
						first := nil;
						last  := nil;
					end else
					begin
						first := first^.next; // = emptyBlock^.next
						last^.next := emptyBlock;
					end;
				end else

				// оставить emptyBlock
				begin
					next := first^.next;
					Destroy(first);
					if first = last then
					begin
						first := nil;
						last  := nil;
					end else
					begin
						first := next;
						emptyBlock^.next := next;
					end;
				end;
			end else

			// переместить first в emptyBlock
			begin
				Assert(last^.next = first);
				emptyBlock := first;
				if first = last then
				begin
					first := nil;
					last := nil;
				end else
					first := first^.next;
			end;
		end;
	{$ifdef threading}
		{$ifdef msg_wait} if sumSize = 0 then thereAreItems.ResetEvent; {$endif}
		opLock.Leave;
		getLock.Leave;
	{$endif}
	end;

	function classname.Dump({$ifdef threading} opLocked: boolean = no {$endif}): string;
	var
		cur: pBlock;
	begin
	{$ifdef threading} if not opLocked then opLock.Enter; {$endif}
		result := ToString(sumSize);
		if Assigned(first) then
		begin
			result := PadLeft(result, 3) + ', ';
			cur := first;
			repeat
				if cur <> first then result += ' -> ';
				result += '(' + ToString(cur^.allocated) + ': ';
				if cur = emptyBlock then result += 'empty' else result += ToString(cur^.dataStart) + '–' + ToString(cur^.dataEnd);
				result += ')';
				cur := cur^.next;
			until cur = first;
		end else
		if Assigned(emptyBlock) then result += ', emptyBlock = ' + ToString(emptyBlock^.allocated);
	{$ifdef threading} if not opLocked then opLock.Leave; {$endif}
	end;

	procedure classname.Destroy(block: pBlock);
	begin
	{$ifdef Debug}
		dec(nBlocks);
		sumAlloc -= block^.allocated;
	{$endif}
		FreeMem(block);
	end;

{$ifdef Debug}
	function classname.DumpStats: string;
	begin
		result := Format('allocations = {0}, maxItems = {1}, maxBlocks = {2}, maxSumSize = {3}, maxSumAlloc = {4}',
		                 [stats.allocations, stats.maxItems, stats.maxBlocks, stats.maxSumSize, stats.maxSumAlloc]);
	end;
{$endif}

{$undef classname} {$undef threading} {$undef msg_wait}
