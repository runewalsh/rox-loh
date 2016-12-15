// classname      := имя объекта
//
// inline_hash    := хэш параметра _1, пример: {$define inline_hash := Hash.OfUint(_1.A) xor Hash.OfPointer(_1.X)}
// hash_func      := альтернатива inline_hash: хэш-функция ключей
//
// inline_eq      := равенство ключей _1 и _2, либо, при inline_key, элемента _1 и чьего-то inline_key _2.
// eq_func        := альтернатива inline_eq, аналогично inline_hash + hash_func.
//                   function SameString(const A, B: string): boolean; {$define eq_func:=SameString}
//
// finalize_key   := финализатор ключа, в т. ч. при присваивании нового
// finalize_value := финализатор значения, в т. ч. при присваивании нового (только хэши)
// get_key        := получение ключа из аргумента _1 в связке с inline_key. Пример: {$define get_key := _1.GetKey}
// store_iter     := lvalue из ключа _1, куда записываются итераторы к элементам

{$ifdef open_addressing} {$error} {$endif}
{$ifdef chaining} {$error} {$endif}
{$ifdef ishash} {$error} {$endif}
{$ifdef inline_key} {$error} {$endif}
{$ifdef dont_replace} {$error} {$endif}
{$ifdef on_new_ret} {$error} {$endif}
{$ifdef delayed_key} {$error} {$endif}
{$ifdef store_hash} {$error} {$endif}
{$ifdef null_defined} {$error} {$endif}
{$ifdef _Key_} {$error} {$endif}
{$ifdef _Value_} {$error} {$endif}
{$ifdef finalize_key_wrap} {$error} {$endif}
{$ifdef finalize_value_wrap} {$error} {$endif}
{$ifdef _1} {$error} {$endif}
{$ifdef _2} {$error} {$endif}
{$ifdef iter} {$error} {$endif}
{$ifdef ht} {$error} {$endif}
{$ifdef invalidate_iterator} {$error} {$endif}
{$ifdef iter_ok} {$error} {$endif}
{$ifdef iter_cell} {$error} {$endif}
{$ifdef FinalizationRequired} {$error} {$endif}
{$ifdef debug_hash} {$error} {$endif}
{$ifdef user_param} {$error} {$endif}

	procedure classname.CompileTimeCheck;
	type
		OA = OpenAddressing;
		HV = IsHash;
		SH = StoreHash;
		ND = NullDefined;
		IK = InlineKey;
		UP = PassUserParam;
		NR = DontReplace;
		ONR = OnNewRet;
		DK = DelayedKey;
		DB = DebugHash;
	begin
	{$if sizeof(OA) > 0} {$define open_addressing} {$else} {$define chaining} {$endif}
	{$if sizeof(HV) > 0} {$define ishash} {$endif}
	{$if sizeof(SH) > 0} {$define store_hash} {$endif}
	{$if sizeof(ND) > 0} {$define null_defined} {$endif}
	{$if sizeof(IK) > 0} {$define inline_key} {$ifndef get_key} {$error define get_key} {$endif} {$endif}
	{$if sizeof(UP) > 0} {$define user_param} {$endif}
	{$if sizeof(NR) > 0} {$define dont_replace} {$endif}
	{$if sizeof(ONR) > 0} {$define on_new_ret} {$endif}
	{$if sizeof(DK) > 0} {$define delayed_key} {$endif}
	{$if sizeof(DB) > 0} {$define debug_hash} {$endif}
	end;
{$if defined(finalize_value) and not defined(ishash)} {$error finalize_value & not ishash} {$endif}
{$if defined(inline_key) <> defined(get_key)} {$error inline_key without get_key (or vice versa)} {$endif}
{$define finalize_key_wrap := {$ifdef finalize_key} begin finalize_key end {$endif} {$undef _1}}
{$define finalize_value_wrap := {$ifdef finalize_value} begin finalize_value end {$endif} {$undef _1}}
{$if defined(finalize_key) or defined(finalize_value)} {$define FinalizationRequired} {$endif}
{$define get_key_wrap := {$ifdef get_key} get_key {$else} _1 {$endif} {$undef _1}}
{$define invalidate_iterator := _1.hid := not uint(0) {$undef _1}}
{$define iter_ok := (0 <> not iter.hid) {$undef iter}}
{$define iter_cell :=
	{$ifdef chaining} {$ifdef ht} ht. {$endif} _h[iter.hid].chain[iter.chainid] {$endif}
	{$ifdef open_addressing} {$ifdef ht} ht. {$endif} _h[iter.hid] {$endif} {$undef iter} {$undef ht}}
// {$ifdef open_addressing} {$note using OA} {$else} {$note using chain} {$endif}
// {$ifdef store_hash} {$note storing hash} {$else} {$note not storing hash} {$endif}

	procedure classname.Rebuild(nc: uint);
	var
		t: pCell;
		i, it, newlen, newlenmask: uint;
	{$ifdef chaining}
		j: uint;
		n: pUint;
	{$endif}
	{$ifdef open_addressing}
		LPL2, probe: uint;
	{$endif}
	begin
		newlen := UpToPow2({$ifdef open_addressing} 3 * nc {$else} nc {$endif});
	{$ifdef debug_hash}
		(*writeln('[hash.rebuild: ' + Utils.ToString(nh) + ' -> ' + Utils.ToString(newlen)
			{$ifdef open_addressing} + ' (nRem = ' + Utils.ToString(nRemoved) + ')' {$endif}
			+ ' @ ' + Utils.ToString(sizeof(Cell)) + ']; ');*)
	{$endif}
	{$ifdef Debug}
		Assert((newlen <> nh) {$ifdef open_addressing} or (nRemoved > 0) {$endif},
			'redundant Rebuild: nh = newlen = ' + Utils.ToString(nh)
		{$ifdef open_addressing} + ', nRemoved = ' + Utils.ToString(nRemoved) {$endif});
	{$endif}

		if newlen > 0 then
		begin
			t := GetMem(nItems * sizeof(Cell));
			System.Initialize(t^, nItems);
			newlenmask := newlen - 1;
		{$ifdef chaining}
			n := GetMem(newlen * sizeof(uint));
			for i := 0 to newlen - 1 do
				n[i] := 0;
		{$endif}
		end;

		i := 0;
		it := 0;
		while i < nh do
		begin
		{$ifdef chaining}
			j := 0;
			while j < _h[i].nChain do
			begin
				t[it] := _h[i].chain[j];
				inc(n[ExtractHash(t[it]) and newlenmask]);
				inc(it);
				inc(j);
			end;
			System.Finalize(_h[i].chain^, _h[i].nChain);
			FreeMem(_h[i].chain);
		{$endif}

		{$ifdef open_addressing}
			if CellStatus(i) = ActiveCell then
			begin
				t[it] := _h[i];
				inc(it);
				System.Finalize(_h[i]);
			end;
		{$endif}
			inc(i);
		end;
	{$ifdef Debug} Assert(it = nItems, Utils.ToString(it) + ' ' + Utils.ToString(nItems)); {$endif}
		FreeMem(_h);

		nh := newlen;
	{$ifdef open_addressing}
		nRemoved := 0;
		lowerPopulationLimit := nc div 2;
		LPL2 := nh div 8;
		if LPL2 < lowerPopulationLimit then lowerPopulationLimit := LPL2;
		upperPopulationLimit := (7 * nh) div 10;
		Assert(lowerPopulationLimit <= upperPopulationLimit);
		Assert((nc = 0) or (upperPopulationLimit > nc));
		Assert((nc = 0) or (upperPopulationLimit < nh));
	{$endif}
		if newlen = 0 then exit;
		nhmask := newlenmask;
		_h := GetMem(newlen * sizeof({$ifdef chaining} Chain {$endif} {$ifdef open_addressing} Cell {$endif})
			{$ifdef open_addressing} + ((newlen + uint(StatusesPerHx - 1)) div StatusesPerHx) * sizeof(uint) {$endif});

	{$ifdef chaining}
		for i := 0 to newlen - 1 do
		begin
			_h[i].nChain := n[i];
			_h[i].chain := GetMem(n[i] * sizeof(Cell));
			System.Initialize(_h[i].chain^, n[i]);
		{$ifdef Debug} stat.Note(max_chain_hash_chain_length, n[i]); {$endif}
			n[i] := 0;
		end;
	{$endif}

	{$ifdef open_addressing}
		hx := pointer(_h + newlen);
		Assert(aligned(hx, sizeof(hx[0])));
		for i := 0 to (newlen + uint(StatusesPerHx - 1)) div StatusesPerHx - 1 do
			hx[i] := 0;
	{$endif}

		i := 0;
		while i < nItems do
		begin
			it := ExtractHash(t[i]) and newlenmask;
		{$ifdef chaining}
			_h[it].chain[n[it]] := t[i];
		{$ifdef store_iter} {$define _1 := t[i].key} store_iter.hid := it; store_iter.chainid := n[it]; {$undef _1} {$endif}
			inc(n[it]);
		{$endif}

		{$ifdef open_addressing}
			probe := 0;
			while CellStatus(it) <> EmptyCell do
			begin
				inc(probe);
				it := (it + probe * probe) and newlenmask;
			end;
		{$ifdef Debug} stat.Note(max_open_hash_chain_length, 1 + probe); {$endif}
			System.Initialize(_h[it]);
			_h[it] := t[i];
		{$ifdef store_iter} {$define _1 := t[i].key} store_iter.hid := it; {$undef _1} {$endif}
			SetCellStatus(it, ActiveCell);
		{$endif}
			inc(i);
		end;
	{$ifdef chaining} FreeMem(n); {$endif}
		System.Finalize(t^, nItems);
		FreeMem(t);
	end;

	procedure classname.Grow(nc: uint);
	begin
		if {$ifdef chaining} nc > nh {$endif}
			{$ifdef open_addressing} (nc + nRemoved > upperPopulationLimit) {$endif}
		then
		begin
			Rebuild(nc);
		{$ifdef debug_hash} inc(dbg.nGrows); {$endif}
		end;
		nItems := nc;
	{$ifdef Debug} stat.Note(max_hash_nelems, nc); {$endif}
	end;

	procedure classname.Shrink(nc: uint);
	begin
		nItems := nc;
		if {$ifdef chaining} (nc < nh div 8) {$endif}
			{$ifdef open_addressing} (nc < lowerPopulationLimit) {$endif}
			then
		begin
			Rebuild(nc);
		{$ifdef debug_hash} inc(dbg.nShrinks); {$endif}
		end;
	end;

{$ifdef open_addressing}
	function classname.CellStatus(id: uint): CellStatusEnum;
	begin
		result := CellStatusEnum((hx[id div StatusesPerHx] shr (CellStatusBits * (id mod StatusesPerHx))) and CellStatusMask);
	end;

	procedure classname.SetCellStatus(id: uint; status: CellStatusEnum);
	var
		shift: uint;
	begin
		shift := CellStatusBits * (id mod StatusesPerHx);
		hx[id div StatusesPerHx] := (hx[id div StatusesPerHx] and not (CellStatusMask shl shift)) or (uint(status) shl shift);
	end;
{$endif}

	procedure classname.Init({$ifdef user_param} const newParam: UserParam {$endif});
	begin
	{$ifdef user_param} param := newParam; {$endif}
		nItems := 0;
		nh := 0;
		_h := nil;
	{$ifdef open_addressing}
		nRemoved := 0;
		upperPopulationLimit := 0;
		lowerPopulationLimit := 0;
	{$endif}
	{$ifdef Debug} nOpenedIterators := 0; {$endif}
	{$ifdef debug_hash} dbg.nGrows := 0; dbg.nShrinks := 0; {$endif}
	end;

	procedure classname.Done;
	begin
	{$ifdef Debug}
		if nOpenedIterators <> 0 then
			Log('Незакрытые итераторы (' + Utils.ToString(nOpenedIterators) + ' шт.)!', logError);
	{$endif}
		Clear;
		FreeMem(_h);
	end;

{$if defined(inline_key) and not defined(delayed_key)}
	function classname.Add(const key: KeyType
		{$ifdef ishash}; const value: ValueType {$endif}
		{$ifdef on_new_ret}; out new: boolean {$endif}): pCellData;
	begin
		result := Add(key, {$define _1:=key} get_key_wrap
			{$ifdef ishash}, value {$endif}
			{$ifdef on_new_ret}, new {$endif});
	end;
{$endif}

	function classname.Add(
		{$ifndef delayed_key} const key: KeyType {$endif}
		{$ifdef inline_key}{$ifndef delayed_key};{$endif} const skey: SearchKey {$endif}
		{$ifdef ishash}; const value: ValueType {$endif}
		{$ifdef on_new_ret}; out new: boolean {$endif}
		{$if defined(delayed_key) and defined(store_iter)}; out newIt: Iterator {$endif}): pCellData;
	var
		found: Iterator;
		nn: pCell;
		h: Hash.Value;
		id: uint;
	{$ifdef open_addressing}
		status: CellStatusEnum;
		probe: uint;
	{$endif}
	begin
	{$ifdef Debug}
		if nOpenedIterators <> 0 then
			Fatal('Попытка добавить элемент в хэш-таблицу с незакрытыми итераторами (' + Utils.ToString(nOpenedIterators) + ' шт.)!');
	{$endif}
		found := FindCell({$ifdef inline_key} skey {$else} key {$endif}, @h);
	{$ifdef on_new_ret} new := not {$define iter := found} iter_ok; {$endif}

		if {$define iter := found} iter_ok then
		begin
			nn := {$define iter := found} @iter_cell;
		{$ifdef dont_replace}
			{$define _1 := key} finalize_key_wrap
			{$define _1 := value} finalize_value_wrap
		{$else}
			{$define _1 := nn^.key} finalize_key_wrap;
			{$define _1 := nn^.value} finalize_value_wrap;
		{$endif}
		end else
		begin
			Grow(nItems + 1);
			id := h and nhmask;
		{$ifdef chaining}
			inc(_h[id].nChain); {$ifdef Debug} stat.Note(max_chain_hash_chain_length, _h[id].nChain); {$endif}
			ReallocMem(_h[id].chain, _h[id].nChain * sizeof(Cell));
			nn := @_h[id].chain[_h[id].nChain - 1];
		{$endif}

		{$ifdef open_addressing}
			probe := 0;
			repeat
				status := CellStatus(id);
				if status <> ActiveCell then
				begin
					if status = RemovedCell then dec(nRemoved);
					SetCellStatus(id, ActiveCell);
					nn := @_h[id];
					break;
				end;
				inc(probe);
				id := (id + probe * probe) and nhmask;
			until no;
		{$ifdef Debug} stat.Note(max_open_hash_chain_length, 1 + probe); {$endif}
		{$endif}

      {$if defined(delayed_key) and defined(store_iter)}
			{$ifdef chaining} newIt.hid := id; newIt.chainid := _h[id].nChain - 1; {$endif}
			{$ifdef open_addressing} newIt.hid := id; {$endif}
		{$endif}

			System.Initialize(nn^);
		{$ifdef store_hash} nn^.hash := h; {$endif}
		end;
	{$ifdef dont_replace} if not {$define iter := found} iter_ok then {$endif}
		begin
		{$ifndef delayed_key} nn^.key := key; {$endif}
		{$ifdef ishash} nn^.value := value; {$endif}
		end;
		result := @nn^.{$ifdef ishash} value {$else} key {$endif};
	end;

	function classname.FindCell(const key: SearchKey; hv: Hash.pValue): Iterator;
	var
		id: uint;
		h: Hash.Value;
	{$ifdef chaining} i: uint; {$endif}
	{$ifdef open_addressing}
		status: CellStatusEnum;
		probe, firstRemoved: uint;
	{$endif}
	begin
	{$ifdef Debug}
		{$ifdef chaining} stat.Increment(chain_hash_searches); {$endif}
		{$ifdef open_addressing} stat.Increment(open_hash_searches); {$endif}
	{$endif}
		h := {$if defined(hash_func)} hash_func(key) {$else} {$define _1 := key} inline_hash {$undef _1} {$endif};
		if Assigned(hv) then hv^ := h;
		if nh = 0 then begin {$define _1 := result} invalidate_iterator; exit; end;
		id := h and nhmask;

	{$ifdef chaining}
		i := 0;
		while i < _h[id].nChain do
		begin
		{$ifdef Debug} stat.Increment(passed_chain_hash_elems); {$endif}
			if Equals(_h[id].chain[i], key {$ifdef store_hash}, h {$endif}) then
			begin
				result.hid := id;
				result.chainid := i;
				exit;
			end;
			inc(i);
		end;
	{$endif}

	{$ifdef open_addressing}
		probe := 0;
		firstRemoved := not uint(0);
		repeat
			status := CellStatus(id);
		{$ifdef Debug} if status <> EmptyCell then stat.Increment(passed_open_hash_elems); {$endif}
			case status of
				EmptyCell: break;
				RemovedCell: if (not firstRemoved) = 0 then firstRemoved := id;
				ActiveCell:
					if Equals(_h[id], key {$ifdef store_hash}, h {$endif}) then
					begin
						if 0 <> not firstRemoved then
						begin
							System.Initialize(_h[firstRemoved]);
							_h[firstRemoved] := _h[id];
							System.Finalize(_h[id]);
							SetCellStatus(firstRemoved, ActiveCell);
							SetCellStatus(id, RemovedCell);
							id := firstRemoved;
						{$ifdef store_iter} {$define _1 := _h[id].key} store_iter.hid := id; {$undef _1} {$endif}
						{$ifdef Debug} stat.Increment(relocated_open_hash_elems); {$endif}
						end;
						result.hid := id;
						exit;
					end;
			{$ifdef Debug} else raise Error('Некорректный CellStatus: {0}.', [ord(status)]); {$endif}
			end;
			inc(probe);
			id := (id + probe * probe) and nhmask;
		until no;
	{$endif}
	{$define _1 := result} invalidate_iterator;
	end;

	function classname.ExtractHash(const c: Cell): Hash.Value;
		function K: SearchKey; cinline
		begin
			result := {$define _1 := c.key} get_key_wrap;
		end;
	begin
		result := {$ifdef store_hash} c.hash
		          {$else} {$if defined(hash_func)} hash_func(K) {$else} {$define _1 := K} inline_hash {$undef _1} {$endif}
		          {$endif}
	end;

	function classname.Equals(const c: Cell; const key: SearchKey {$ifdef store_hash}; const kh: Hash.Value{$endif}): boolean;
	begin
		result := {$if defined(store_hash)} (kh = c.hash) and {$endif}
			({$if defined(inline_eq)}
				{$define _1 := c.key} {$define _2 := key} inline_eq {$undef _1} {$undef _2}
			{$elseif defined(eq_func)}
				eq_func(c.key, key)
			{$else}
				{$define _1 := c.key} get_key_wrap = key
			{$endif});
	end;

	function classname.Find(const key: SearchKey): {$if defined(null_defined)} CellData {$else} pCellData {$endif};
	var
		c: Iterator;
	begin
		c := FindCell(key, nil);
		if {$define iter := c} iter_ok then
			result := {$ifndef null_defined} @ {$endif} {$define iter := c} iter_cell.{$ifdef ishash} value {$else} key {$endif}
		else
			result := {$ifdef null_defined} NullValue {$else} nil {$endif};
	end;

	function classname.Find(const key: SearchKey; out value: CellData): boolean;
	var
		c: Iterator;
	begin
		c := FindCell(key, nil);
		result := {$define iter := c} iter_ok;
		if result then value := {$define iter := c} iter_cell.{$ifdef ishash} value {$else} key {$endif};
	end;

	function classname.Find(const key: SearchKey; out value: pCellData): boolean;
	var
		c: Iterator;
	begin
		c := FindCell(key, nil);
		result := {$define iter := c} iter_ok;
		if result then value := @{$define iter := c} iter_cell.{$ifdef ishash} value {$else} key {$endif};
	end;

	function classname.Contains(const key: SearchKey): boolean;
	var
		c: Iterator;
	begin
		c := FindCell(key, nil);
		result := {$define iter := c} iter_ok;
	end;

	function classname.Remove(const key: SearchKey): boolean;
	begin
		result := Remove(FindCell(key, nil));
	end;

	function classname.Remove(it: Iterator): boolean;
	var
		hid: uint absolute it.hid;
	{$ifdef chaining} chainid: uint absolute it.chainid; {$endif}
	begin
	{$ifdef Debug}
		if nOpenedIterators <> 0 then
			Fatal('Попытка удалить элемент из хэш-таблицы с незакрытыми итераторами (' + Utils.ToString(nOpenedIterators) + ' шт.)!');
	{$endif}

		result := {$define iter := it} iter_ok;
		if result then
		begin
			{$define _1 := {$define iter := it} iter_cell.key} finalize_key_wrap;
			{$define _1 := {$define iter := it} iter_cell.value} finalize_value_wrap;

		{$ifdef chaining}
			if chainid + 1 <> _h[hid].nChain then
			begin
				_h[hid].chain[chainid] := _h[hid].chain[_h[hid].nChain-1];
			{$ifdef store_iter} {$define _1 := _h[hid].chain[chainid].key} store_iter.chainid := chainid; {$undef _1} {$endif}
			end;
			dec(_h[hid].nChain);
			System.Finalize(_h[hid].chain[_h[hid].nChain]);
			ReallocMem(_h[hid].chain, _h[hid].nChain * sizeof(Cell));
		{$endif}

		{$ifdef open_addressing}
			System.Finalize(_h[hid]);
			SetCellStatus(hid, RemovedCell);
			inc(nRemoved);
		{$endif}
			Shrink(nItems - 1);
		end;
	end;

	procedure classname.Clear;
	var
		i {$if defined(chaining) and defined(FinalizationRequired)}, j{$endif}: uint;
	begin
	{$ifdef Debug}
		if nOpenedIterators <> 0 then
			Fatal('Попытка очистить хэш-таблицу с незакрытыми итераторами (' + Utils.ToString(nOpenedIterators) + ' шт.)!');
	{$endif}
		if nItems > 0 then
		begin
		{$ifdef chaining}
			for i := 0 to nh - 1 do
			begin
			{$ifdef FinalizationRequired}
				j := 0;
				while j < _h[i].nChain do
				begin
				{$define _1 := _h[i].chain[j].key} finalize_key_wrap;
				{$define _1 := _h[i].chain[j].value} finalize_value_wrap;
					inc(j);
				end;
			{$endif}
				System.Finalize(_h[i].chain^, _h[i].nChain);
				FreeMem(_h[i].chain);
				_h[i].nChain := 0;
			end;
		{$endif}

		{$ifdef open_addressing}
			for i := 0 to nh - 1 do
				if CellStatus(i) = ActiveCell then
				begin
					{$define _1 := _h[i].key} finalize_key_wrap;
					{$define _1 := _h[i].value} finalize_value_wrap;
					System.Finalize(_h[i]);
				end;
			for i := 0 to (nh + uint(StatusesPerHx - 1)) div StatusesPerHx - 1 do
				hx[i] := 0;
	{$endif}
		end;
		Shrink(0);
	end;

	function classname.GetIterator: Iterator;
	begin
	{$define _1 := result} invalidate_iterator;
	{$ifdef Debug} inc(nOpenedIterators); {$endif}
	end;

	function classname.Next(var iter: Iterator): boolean;
	begin
		if iter_ok then
		begin
		{$ifdef chaining} inc(iter.chainid); {$endif}
		{$ifdef open_addressing} inc(iter.hid); {$endif}
		end else
		begin
			iter.hid := 0;
		{$ifdef chaining} iter.chainid := 0; {$endif}
		end;

	{$ifdef chaining}
		if (iter.hid < nh) and (iter.chainid >= _h[iter.hid].nChain) then
		begin
			iter.chainid := 0;
			repeat
				inc(iter.hid);
			until (iter.hid >= nh) or (_h[iter.hid].nChain > 0);
		end;
		result := iter.hid < nh;
	{$endif}

	{$ifdef open_addressing}
		while (iter.hid < nh) and (CellStatus(iter.hid) <> ActiveCell) do
			inc(iter.hid);
		result := iter.hid < nh;
	{$endif}
		if not result then CloseIterator(iter);
	end;

	function classname.GetKey(var iter: Iterator): pKey; begin result := @iter_cell.key; end;
{$ifdef ishash} function classname.GetValue(var iter: Iterator): pValue; begin result := @iter_cell.value; end; {$endif}

	function classname.AnyElement(out iter: Iterator): boolean;
	begin
		iter := GetIterator;
		result := Next(iter);
		if result then CloseIterator(iter);
	end;

	procedure classname.CloseIterator(var iter: Iterator);
	begin
	{$ifdef Debug} Assert(nOpenedIterators > 0, 'Нет открытых итераторов.'); {$endif}
		Assert(@iter = @iter);
	{$ifdef Debug} dec(nOpenedIterators); {$endif}
	end;

{$undef hash_func} {$undef inline_hash}
{$undef inline_eq} {$undef eq_func}
{$undef finalize_key} {$undef finalize_key_wrap}
{$undef finalize_value} {$undef finalize_value_wrap} {$undef FinalizationRequired}
{$undef get_key} {$undef get_key_wrap}
{$undef null_defined}
{$undef open_addressing} {$undef chaining} {$undef store_hash} {$undef invalidate_iterator} {$undef iter_ok} {$undef iter_cell}
{$undef ishash} {$undef inline_key} {$undef dont_replace} {$undef on_new_ret} {$undef delayed_key} {$undef store_iter}
{$undef classname} {$undef user_param}
{$undef debug_hash}
