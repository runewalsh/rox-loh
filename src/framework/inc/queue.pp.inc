	procedure classname.Init(preallocateItems: sint = 0);
	begin
		unused_args preallocateItems end_list
		nItems := 0;
		hq.Init;
	end;

	procedure classname.Done;
	begin
		hq.Done;
	end;

	procedure classname.Put(const item: ItemType);
	var
		p: pItem;
	begin
		p := hq.Put(sizeof(ItemType));
		System.Initialize(p^);
		p^ := item;
		nItems += 1;
	end;

	procedure classname.Put(const items: array of ItemType);
	var
		p: pItem;
		i: sint;
	begin
		p := hq.Put(sizeof(ItemType) * length(items));
		System.Initialize(p^, length(items));
		for i := 0 to High(items) do p[i] := items[i];
		nItems += length(items);
	end;

	function classname.Get(out item: ItemType): boolean;
	var
		p: pItem;
	begin
		result := nItems > 0;
		if not result then exit;

		p := hq.LockGet;
		item := p^;
		System.Finalize(p^);
		hq.UnlockGet(sizeof(ItemType));
		nItems -= 1;
	end;

	function classname.Get(const items: array of pItem): boolean;
	var
		p: pItem;
		i: sint;
	begin
		result := nItems >= length(items);
		if not result then exit;

		p := hq.LockGet;
		for i := 0 to High(items) do items[i]^ := p[i];
		System.Finalize(p^, length(items));
		hq.UnlockGet(sizeof(ItemType) * length(items));
		nItems -= length(items);
	end;

{$undef classname}
