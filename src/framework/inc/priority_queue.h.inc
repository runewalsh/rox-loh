{$ifdef watch_handles} {$error} {$endif}
{$ifdef heap} {$error} {$endif}
{$ifdef fib} {$error} {$endif}
{$ifdef rand} {$error} {$endif}
{$ifdef naive} {$error} {$endif}

{$if defined(force_heap)} {$define heap} {$undef force_heap} {$note используется «обычная» N-арная куча} {$endif}
{$if defined(force_fib)} {$define fib} {$undef force_fib} {$note используется фибоначчиева куча}
{$elseif defined(force_rand)} {$define rand} {$undef force_rand} {$note используется рандомизированная куча}
{$elseif defined(force_naive)} {$define naive} {$undef force_naive} {$note используется наивная линейная куча}
{$elseif defined(allow_union)} {$define fib}
{$else} {$define heap} {$endif}
{$if defined(allow_prioritize) or defined(allow_delete)} {$define watch_handles} {$endif}

	classname = object
	public type
		pItem = ^ItemType;
		ItemType = item_type;
		pPriority = ^PriorityType;
		PriorityType = priority_type;

	{$ifdef watch_handles}
		handle_t = {$if defined(heap) or defined(naive)} uint {$elseif defined(fib) or defined(rand)} pointer {$else} {$error} {$endif};
		pHandle_t = ^handle_t;
	{$endif}
	const
		{$ifdef watch_handles} NullHandle = handle_t(0); {$endif}
	private var
		nodes: uint;
	public var
		constructor Init;
		destructor Done;
		procedure Clear;
		procedure Put(const item: ItemType; const priority: PriorityType {$ifdef watch_handles}; handle: pHandle_t {$endif});
		function Peek: pItem;
		function Get(out item: ItemType): boolean;
	{$ifdef allow_prioritize}
		procedure Bump({$ifdef Debug} var {$endif} handle: handle_t; const newPriority: PriorityType);
		procedure Relax({$ifdef Debug} var {$endif} handle: handle_t; const newPriority: PriorityType);
		procedure Prioritize({$ifdef Debug} var {$endif} handle: handle_t; const newPriority: PriorityType);
	{$endif}
	{$ifdef allow_delete} procedure Delete({$ifdef Debug} var {$endif} handle: handle_t); {$endif}
	{$ifdef allow_union} procedure Consume(var victim: classname); {$endif}

		property Count: uint read nodes;
	public type
		AllowPrioritize = {$ifdef allow_prioritize} boolean {$else} record end {$endif};
		AllowDelete = {$ifdef allow_delete} boolean {$else} record end {$endif};
		AllowUnion = {$ifdef allow_union} boolean {$else} record end {$endif};
		MoreIsBetter = {$ifdef more_is_better} boolean {$else} record end {$endif};
		Fib = {$ifdef fib} boolean {$else} record end {$endif};
		RandImp = {$ifdef rand} boolean {$else} record end {$endif};
		Naive = {$ifdef naive} boolean {$else} record end {$endif};

	private type
		pNode = ^tNode;
		tNode = record
			item: ItemType;
			priority: PriorityType;
	{$if defined(heap)}
		{$ifdef watch_handles} handle: pHandle_t; {$endif}
		end;
	const
		HeapArity = {$ifdef force_heap_arity} {$note задана пользовательская арность кучи} force_heap_arity {$undef force_heap_arity} {$else} 3 {$endif};
	var
		allocated: uint;
		n: pNode;
		procedure HeapifyDown(id: uint);
		procedure HeapifyUp(id: uint);
		procedure Resize(newN: uint);
		procedure DeleteNode(id: uint);

	{$elseif defined(fib)}
			parent, child, left, right: pNode;
			degree: sint;
			marked: boolean;
		end;
		ppNode = ^pNode;

	var
		degreeLim: uint;
		consBuf: ppNode;
		min, root: pNode;
		procedure InternalBump(x: pNode; newPriority: pPriority);
		procedure InsertRoot(x: pNode);
		procedure RemoveRoot(x: pNode);
		procedure Consolidate;
		procedure HeapLink(y, x: pNode);
		procedure Cut(x, y: pNode);
		procedure CascadingCut(y: pNode);
		function ExtractMin: pNode;
		function ConsolidationBuffer: ppNode;
		procedure InsertNode(x: pNode);
		procedure DeleteNode(x: pNode);

		procedure InsertAfter(a, b: pNode);
		procedure InsertBefore(a, b: pNode);
		function RemoveNode(x: pNode): pNode;
	{$elseif defined(rand)}
			parent: pNode;
			childs: array[0 ..
				{$ifdef force_heap_arity}
					{$note задана пользовательская арность кучи} force_heap_arity {$undef force_heap_arity}
				{$else}
					4
				{$endif} - 1] of pNode;
		end;
		ppNode = ^pNode;

	const
		ChildIndexBits = {$define max := High(tNode.childs)} {$include bits_to_store.inc};
		ChildIndexDifference = 1 shl ChildIndexBits - length(tNode.childs);

	type
		ChildIndexDifferencePlusOne = array[0 .. ChildIndexDifference] of byte;
		ChildIndexDifferenceT = {$if sizeof(ChildIndexDifferencePlusOne) = 1} record end {$else} array[1 .. ChildIndexDifference] of byte {$endif};
	const
		RandomChildBitsRequired = ChildIndexBits {$if sizeof(ChildIndexDifferenceT) > 0} {$note модифицированный алгоритм (интерфейс)} + 2 {$endif};

	var
		rng: RNG;
		root: pNode;
		rand, randBits: uint;
		function Meld(a, b: pNode; checkParent: boolean; parentIfB: pNode = nil): pNode;
		procedure InsertNode(x: pNode);
		procedure DeleteNode(x: pNode);
		function LocateChild(parent, x: pNode): sint;
		function RandomChild: uint;
	{$elseif defined(naive)}
		{$ifdef watch_handles} handle: pHandle_t; {$endif}
		end;
	var
		allocated: uint;
		n: pNode;
		function FindMin: uint;
		procedure DeleteNode(id: uint);
		procedure Resize(newN: uint);
	{$else} {$error} {$endif}
	end;

{$undef allow_prioritize}
{$undef allow_delete}
{$undef allow_union}
{$undef more_is_better}
{$undef priority_type}
{$undef item_type}
{$undef classname}

{$undef watch_handles} {$undef fib} {$undef heap} {$undef rand} {$undef naive}