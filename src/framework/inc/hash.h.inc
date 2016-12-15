// classname  :=          имя типа
// key_type   :=          тип ключа
// value_type :=          тип значения (без key_type получится хэш-множество).
// null_value :=          если null_value не определено, Find возвращает указатель на значение или nil
//                        Если определено — само найденное значение или null_value.
// inline_key :=          тип ключа для элементов множества, если ключом целесообразно считать только их часть (или вовсе производный объект).
// force_open_addressing  принудительно использует открытую адресацию
// force_chaining         принудительно использует цепочки коллизий
// huge_keys              принудительно запоминает хэши (например, резонно выставить для ключей-строк)
// tiny_keys              принудительно не запоминает хэши (например, если они и так запоминаются снаружи и хэш-функция их просто возвращает)
// dont_replace           Add не будет заменять существующий элемент (по умолчанию заменяет)
// user_param :=          тип пользовательского параметра, который передаётся в конструктор и доступен хэш-функции и сравнению.
// on_new_ret             Add возвращает out-параметром факт (не)добавления новой точки
// delayed_key :=         в Add передаётся только SearchKey, она выделяет место для настоящего ключа в таблице; предполагается, что
//                        он будет добавлен сразу после вызова.
// store_iter :=          подсказка для delayed_key, в других случаях этот дефайн нужен только для .pp.
{$ifdef store_hash} {$error} {$endif}
{$ifdef chaining} {$error} {$endif}
{$ifdef ishash} {$error} {$endif}
{$ifdef open_addressing} {$error} {$endif}

{$if defined(value_type)} {$define ishash} {$endif}
{$define debug_hash}

	classname = object
	type
		KeyType = key_type;
		pKey = ^KeyType;

	{$ifdef ishash}
		ValueType = value_type;
		pValue = ^ValueType;
	{$endif}

		SearchKey = {$ifdef inline_key} inline_key {$else} KeyType {$endif};
		CellData = {$ifdef ishash} ValueType {$else} KeyType {$endif};
		pCellData = ^CellData;
	private type
		SmallKeySize                  = array[0 .. 1] of pointer;
		SuitableForOpenAddressingSize = array[0 .. 3] of pointer;
		ApproxCellDataSize            = array[1 .. sizeof(KeyType) {$ifdef ishash} + sizeof(ValueType) {$endif}] of byte;

{$if defined(force_open_addressing)} {$define open_addressing} {$undef force_open_addressing} {$note forced open addressing}
{$elseif defined(force_chaining)} {$define chaining} {$undef force_chaining} {$note forced chaining}
{$elseif sizeof(ApproxCellDataSize) <= sizeof(SuitableForOpenAddressingSize)} {$define open_addressing}
{$else} {$define chaining}
{$endif}
{$if sizeof(SearchKey) > sizeof(SmallKeySize)} {$define store_hash} {$endif}
{$ifdef huge_keys} {$define store_hash} {$undef huge_keys} {$endif}
{$ifdef tiny_keys} {$undef store_hash} {$undef tiny_keys} {$endif}

{$ifdef debug_hash}
	public type
		DebugStatsDesc = record
			nGrows, nShrinks: uint;
		end;
{$endif}

	strict private
		nItems: uint;
	{$ifdef debug_hash} dbg: DebugStatsDesc; {$endif}
	type
		pCell = ^Cell;
		Cell = record
			key: KeyType;
		{$ifdef ishash} value: ValueType; {$endif}
		{$ifdef store_hash} hash: Hash.Value; {$endif}
		end;

	public type
		pIterator = ^Iterator;
		Iterator = object
		private
			hid: uint;
		{$ifdef chaining} chainid: uint; {$endif}
		end;
	{$ifdef user_param} UserParam = user_param; {$endif}

	var
	{$ifdef user_param} param: UserParam; {$endif}
		procedure Init({$ifdef user_param} const newParam: UserParam {$endif});
		procedure Done;
	{$ifndef delayed_key}
		function Add(const key: KeyType
			{$ifdef ishash}; const value: ValueType {$endif}
			{$ifdef on_new_ret}; out new: boolean {$endif}): pCellData; {$ifdef inline_key} cinline {$endif}
	{$endif}

	{$ifdef inline_key}
		function Add({$ifndef delayed_key} const key: KeyType; {$endif} const skey: SearchKey
			{$ifdef ishash}; const value: ValueType {$endif}
			{$ifdef on_new_ret}; out new: boolean {$endif}
			{$if defined(delayed_key) and defined(store_iter)}; out newIt: Iterator {$endif}): pCellData;
	{$endif}
		function Find(const key: SearchKey): {$if defined(null_value)} CellData {$else} pCellData {$endif};
		function Find(const key: SearchKey; out value: CellData): boolean;
		function Find(const key: SearchKey; out value: pCellData): boolean;
		function Contains(const key: SearchKey): boolean;
		function Remove(const key: SearchKey): boolean; cinline
		function Remove(it: Iterator): boolean;
		procedure Clear;
		function GetIterator: Iterator; cinline
		function Next(var iter: Iterator): boolean;
		function GetKey(var iter: Iterator): pKey; cinline
	{$ifdef ishash} function GetValue(var iter: Iterator): pValue; cinline {$endif}
		function AnyElement(out iter: Iterator): boolean;
		procedure CloseIterator(var iter: Iterator); cinline

		property Count: uint read nItems;
	{$ifdef debug_hash} property DebugStats: DebugStatsDesc read dbg; {$endif}

	{$if defined(null_value)} const NullValue = null_value; {$endif}
	strict private type
	{$ifdef chaining}
		pChain = ^Chain;
		Chain = record
			nChain: uint;
			chain: pCell;
		end;
	{$endif}

	var
		nh, nhmask: uint;
		_h: {$ifdef chaining} pChain {$else} pCell {$endif};
	{$ifdef open_addressing}
		hx: pUint;
		nRemoved, upperPopulationLimit, lowerPopulationLimit: uint;
	{$endif}
	{$ifdef Debug} nOpenedIterators: sint; {$endif}
		procedure Rebuild(nc: uint);
		procedure Grow(nc: uint);
		procedure Shrink(nc: uint);
		function FindCell(const key: SearchKey; hv: Hash.pValue): Iterator;
		function ExtractHash(const c: Cell): Hash.Value; cinline
		function Equals(const c: Cell; const key: SearchKey {$ifdef store_hash}; const kh: Hash.Value{$endif}): boolean; cinline

{$ifdef open_addressing}
	type
		scoped_enum_ CellStatusEnum = (Empty, Active, Removed); _end
	const
		EmptyCell   = CellStatusEnum.Empty;
		ActiveCell  = CellStatusEnum.Active;
		RemovedCell = CellStatusEnum.Removed;
		{$define max := ord(RemovedCell)} {$define nbits := CellStatusBits} {$define mask := CellStatusMask} {$include bits_to_store.inc}
		StatusesPerHx = bitsizeof(uint) div CellStatusBits;
	var
		function CellStatus(id: uint): CellStatusEnum;
		procedure SetCellStatus(id: uint; status: CellStatusEnum);
{$endif}

	public type
		OpenAddressing = {$ifdef open_addressing} boolean {$else} record end {$endif};
		IsHash = {$ifdef ishash} boolean {$else} record end {$endif};
		StoreHash = {$ifdef store_hash} boolean {$else} record end {$endif};
		NullDefined = {$ifdef null_value} boolean {$else} record end {$endif};
		InlineKey = {$ifdef inline_key} boolean {$else} record end {$endif};
		PassUserParam = {$ifdef user_param} boolean {$else} record end {$endif};
		DontReplace = {$ifdef dont_replace} boolean {$else} record end {$endif};
		OnNewRet = {$ifdef on_new_ret} boolean {$else} record end {$endif};
		DelayedKey = {$ifdef delayed_key} boolean {$else} record end {$endif};
		DebugHash = {$ifdef debug_hash} boolean {$else} record end {$endif};

		procedure CompileTimeCheck;
	end;

{$undef key_type} {$undef value_type} {$undef null_value}
{$undef open_addressing} {$undef chaining} {$undef store_hash}
{$undef ishash} {$undef inline_key} {$undef dont_replace} {$undef on_new_ret} {$undef delayed_key} {$undef store_iter}
{$undef classname} {$undef user_param}
{$undef debug_hash}
