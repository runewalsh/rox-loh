	classname = object
	type
		ItemType = item_type;
		pItem = ^ItemType;
	private
		nItems: sint;
	public
		procedure Init(preallocateItems: sint = 0);
		procedure Done;
		procedure Put(const item: ItemType);
		procedure Put(const items: array of ItemType);
		function Get(out item: ItemType): boolean;
		function Get(const items: array of pItem): boolean;

		property Count: sint read nItems;
	private
		hq: HeterogenousQueue;
	end;

{$undef item_type}
{$undef classname}
