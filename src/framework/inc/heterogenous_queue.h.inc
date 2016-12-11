	classname = object
		procedure Init;
		procedure Done;

	{$ifdef threading}
		function LockPut(size: size_t): pointer;
		procedure UnlockPut;
	{$else}
		function Put(size: size_t): pointer;
	{$endif}

		function LockGet({$ifdef threading} wait: boolean {$endif}): pointer;
		procedure UnlockGet(size: size_t);

		function Dump({$ifdef threading} opLocked: boolean = no {$endif}): string;

	private type
		pBlock = ^Block;
		Block = object
			dataStart, dataEnd, allocated: size_t;
			next: pBlock;
			data: array[0 .. 0] of GenericAlignedType;
		end;

	var
		// opLock — блокировка операции. Put является операцией от начала до конца. На время Get'а удерживается только getLock.
		// Т. о. если поток-исполнитель один, он может выполнять работу, не вылезая из Get'а.
	{$ifdef threading} opLock, getLock: ThreadLock; {$endif}
		first, last, emptyBlock: pBlock; // emptyBlock — после last и перед first, либо nil
		sumSize, sumAlloc: size_t;       // (также возможно first = last = nil и emptyBlock <> nil)
	{$ifdef threading}
	{$ifdef msg_wait} thereAreItems: ThreadEvent; {$else} heyThereIsItem: ThreadCV; {$endif}
		wasEmpty: boolean;
	{$endif}
		procedure Destroy(block: pBlock); {$ifndef Debug} static; {$endif}
	const
		Alignment = sizeof(GenericAlignedType);

	{$ifdef Debug}
	public
		nItems, nBlocks: uint;
		stats: record
			allocations, maxItems, maxBlocks: uint;
			maxSumSize, maxSumAlloc: size_t;
		end;
		function DumpStats: string;
	{$endif}

	public type
		Threading = {$ifdef threading} boolean {$else} record end {$endif};
		MsgWait   = {$ifdef msg_wait} boolean {$else} record end {$endif};

		procedure CompileTimeCheck;
	end;

{$undef classname} {$undef threading} {$undef msg_wait}
