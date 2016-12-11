{$ifndef _key2ctl_}
	{$error см. ниже}
	Определи макрос _key2ctl_, преобразующий параметр key_type типа Key в LeafControl_t.
	Например, если Key — указатель на структуру с полем bnd: pBounding, реализацией будет key_type^.bnd^.
	Если определён символ Container, макросу будет доступен параметр container, переданный в конструктор.
{$endif}
{$define key2ctl := {$ifndef key_type} {$define key_type:=key} {$endif} _key2ctl_ {$undef key_type}}
{$ifdef allow_change} {$error} {$endif}
{$ifdef use_point} {$error} {$endif}
{$ifdef use_bounding} {$error} {$endif}
{$ifdef Container} {$error} {$endif}
{$define key2string := {$ifndef key_type} {$define key_type := key} {$endif} _key2string_ {$undef key_type}}

	procedure CompileTimeCheck(var t: classname);
	type
		AC = classname.AllowChange;
		UP = classname.UsePoint;
		UB = classname.UseBounding;
		C = classname.UseContainer;
		VC = classname.VerifyChanged;
	begin
		Assert(@t = @t);
	{$if sizeof(AC) > 0} {$define allow_change} {$endif}
	{$if sizeof(UP) > 0} {$define use_point} {$endif}
	{$if sizeof(UB) > 0} {$define use_bounding} {$endif}
	{$if sizeof(C) > 0} {$define Container} {$endif}
	{$if sizeof(VC) > 0} {$define verify_changed} {$endif}
	end;

	procedure classname.Init({$ifdef Container} newContainer: pContainer; {$endif} newNodeCapacity: uint = DefaultKdLimit);
	begin
		root := nil;
		nodeCapacity := newNodeCapacity;
		nObjs := 0;
	{$ifdef Container} container := newContainer; {$endif}
	end;

	procedure classname.Done;
	begin
		Clear;
	end;

	procedure classname.Clear;
	begin
		if Assigned(root) then root^.Destroy;
		root := nil;
		nObjs := 0;
	end;

	function classname.tNode.Create(newParent: pNode): pNode;
	begin
		new(result);
		result^.lt := nil;
		result^.rt := nil;
		result^.parent := newParent;
		result^.first := nil;
		result^.last := nil;
		result^.nLeafs := 0;
		result^.bnd := Bounding.Undefined;
	end;

	procedure classname.tNode.Destroy;
	begin
		Clear;
		dispose(@self);
	end;

	procedure classname.tNode.Clear;
	var
		cur, t: pLeaf;
	begin
		cur := first;
		while Assigned(cur) do
		begin
			t := cur;
			cur := cur^.next;
			dispose(t);
		end;
		first := nil;
		last := nil;
		nLeafs := 0;
		if Assigned(lt) then
		begin
			lt^.Destroy; lt := nil;
			rt^.Destroy; rt := nil;
		end;
	end;

	function classname.tNode.Find(const key: tKey; var node: pNode; var leaf: pLeaf {$ifdef Container}; container: pContainer {$endif}): boolean;
	var
		cur: pLeaf;
	begin
		cur := first;
		while Assigned(cur) do
			if cur^.key <> key then cur := cur^.next else
			begin
				node := @self;
				leaf := cur;
				exit(yes);
			end;

		result := Assigned(lt) and
			(
				(Contains(lt^.bnd, key2ctl) and lt^.Find(key, node, leaf {$ifdef Container}, container {$endif})) or
				(Contains(rt^.bnd, key2ctl) and rt^.Find(key, node, leaf {$ifdef Container}, container {$endif}))
			);
	end;

	procedure classname.tNode.AddNode(n: pLeaf; doPropagate: boolean {$ifdef Container}; container: pContainer {$endif});
	begin
		if doPropagate then
		begin
			if bnd.IsUndefined then
				bnd := {$ifdef use_point} Bounding.ByPoint {$endif} ({$define key_type:=n^.key} key2ctl)
			else
				bnd.Enlarge({$define key_type:=n^.key} key2ctl);
		{$ifdef verify_changed} n^.knownCtl := {$define key_type := n^.key} key2ctl; {$endif}

			if Assigned(lt) then
			begin
				if SqrDistance(lt^.bnd.center, {$define key_type:=n^.key} key2ctl {$ifdef use_bounding}.Center{$endif}) < SqrDistance(rt^.bnd.center, {$define key_type:=n^.key} key2ctl {$ifdef use_bounding}.Center{$endif}) then
					lt^.AddNode(n, doPropagate {$ifdef Container}, container {$endif})
				else
					rt^.AddNode(n, doPropagate {$ifdef Container}, container {$endif});
				exit;
			end;
		end;

		n^.next := first;
		first := n;
		if nLeafs = 0 then last := n;
		inc(nLeafs);
	end;

	procedure classname.Add(const key: tKey; propagate: boolean);
	var
		n: pLeaf;
	begin
		if not Assigned(root) then root := tNode.Create(nil);
		new(n);
		n^.key := key;
		root^.AddNode(n, propagate {$ifdef Container}, container {$endif});
		inc(nObjs);
	end;

	function classname.tNode.Remove(const key: tKey; const ctl: LeafControl_t {$ifdef Container}; container: pContainer {$endif}): boolean;
	var
		cur, prev, t: pLeaf;
	begin
		result := yes;
		prev := nil;
		cur := first;
		while Assigned(cur) do
		begin
			if cur^.key <> key then
			begin
				prev := cur;
				cur := cur^.next;
			end else
			begin
				t := cur;
				cur := cur^.next;
				if Assigned(prev) then
				begin
					prev^.next := cur;
					if not Assigned(cur) then last := prev;
				end else
				begin
					first := cur;
					if not Assigned(cur) then last := nil;
				end;
			{$ifdef verify_changed} t^.VerifyChanged('Remove', ctl); {$endif}
				dispose(t);
				dec(nLeafs);
				Propagate(yes {$ifdef Container}, container {$endif});
				exit;
			end;
		end;

		if Assigned(lt) then
		begin
			if Contains(lt^.bnd, ctl) then
				if lt^.Remove(key, ctl {$ifdef Container}, container {$endif}) then exit;
			if Contains(rt^.bnd, ctl) then
				if rt^.Remove(key, ctl {$ifdef Container}, container {$endif}) then exit;
		end;
		result := no;
	end;

	function classname.tNode.UltimateRemove(const key: tKey {$ifdef Container}; container: pContainer {$endif}): boolean;
	var
		cur, prev, t: pLeaf;
	begin
		prev := nil;
		cur := first;
		while Assigned(cur) do
		begin
			if cur^.key <> key then
			begin
				prev := cur;
				cur := cur^.next
			end else
			begin
				t := cur;
				cur := cur^.next;
				if Assigned(prev) then
				begin
					prev^.next := cur;
					if not Assigned(cur) then last := prev;
				end else
				begin
					first := cur;
					if not Assigned(cur) then last := nil;
				end;
			{$ifdef Debug} LogR('ВНЕЗАПНО в узле ' + ToString(bnd.AABB) + '! ', logError); {$endif}
			{$ifdef verify_changed} t^.VerifyChanged('UltimateRemove'); {$endif}
				dispose(t);
				dec(nLeafs);
				Propagate(yes {$ifdef Container}, container {$endif});
				exit(yes);
			end;
		end;
		result := Assigned(lt) and (lt^.UltimateRemove(key {$ifdef Container}, container {$endif}) or rt^.UltimateRemove(key {$ifdef Container}, container {$endif}));
	end;

	procedure classname.Remove(const key: tKey);
	{$ifdef Debug}
	var
		rem: boolean;
	{$endif}
	begin
	{$ifdef Debug}
		rem := Assigned(root) and root^.Remove(key, key2ctl {$ifdef Container}, container {$endif});
		if not rem then
		begin
			LogR('Узел kD ' + ToString(key2ctl {$ifdef use_bounding} .AABB {$endif})
				{$ifdef _key2string_} + ' (' +  key2string + ')' {$endif}
				+ ' не найден, обхожу всё дерево... ', logError);
			rem := Assigned(root) and root^.UltimateRemove(key {$ifdef Container}, container {$endif});
			if rem then
				Log('Что-то не так.', logError)
			else
				Log('Не найден ВООБЩЕ', logError);
		end;
		if rem then dec(nObjs);
	{$else}
		if Assigned(root) then
			if root^.Remove(key, key2ctl {$ifdef Container}, container {$endif}) or root^.UltimateRemove(key {$ifdef Container}, container {$endif}) then
				dec(nObjs);
	{$endif}
	end;

	procedure classname.tNode.Divide(nodeCapacity: uint; depth: sint {$ifdef Container}; container: pContainer {$endif});
	const
		MedianeWeight = 0.95;
		CenterWeight = 1.0 - MedianeWeight;
	var
		divp: float;
		sizes: Vec3;
		curLbox: AABB;
		dimension: sint;
		avg: Vec3;
		cur, t: pLeaf;
	begin
		if Assigned(lt) then CollapseTo(@self);
		RecalcBounding({$ifdef Container} container {$endif});
	{$ifdef Debug} stat.Note(max_kd_depth, depth); {$endif}
		if (nLeafs <= nodeCapacity) or (depth >= MaxKdDepth) then exit;

		avg := Vec3.Zero;
		cur := first;
		while Assigned(cur) do
		begin
			avg := avg + {$define key_type:=cur^.key} key2ctl {$ifdef use_bounding}.Center{$endif};
			cur := cur^.next;
		end;
		avg /= nLeafs;

		sizes := bnd.Sizes;
		dimension := ord(sizes.data[1] > sizes.data[0]);
		if sizes.data[2] > sizes.data[dimension] then dimension := 2;
		curLbox := bnd.AABB;
		divp := MedianeWeight * avg.data[dimension] + CenterWeight * bnd.center.data[dimension];
		curLbox.B.data[dimension] := divp;

		lt := tNode.Create(@self);
		rt := tNode.Create(@self);

		cur := first;
		while Assigned(cur) do
		begin
			t := cur^.next;
			if curLbox.Contains({$define key_type:=cur^.key} key2ctl {$ifdef use_bounding}.Center{$endif}) then
				lt^.AddNode(cur, no {$ifdef Container}, container {$endif})
			else
				rt^.AddNode(cur, no {$ifdef Container}, container {$endif});
			cur := t;
		end;

		if (lt^.nLeafs = 0) or (rt^.nLeafs = 0) then
		begin
			if lt^.nLeafs = 0 then
			begin
				first := rt^.first;
				last := rt^.last;
				rt^.first := nil;
				rt^.last := nil;
				rt^.nLeafs := 0;
			end else
			begin
				first := lt^.first;
				last := lt^.last;
				lt^.first := nil;
				lt^.last := nil;
				lt^.nLeafs := 0;
			end;
			lt^.Destroy; lt := nil;
			rt^.Destroy; rt := nil;
		end else
		begin
			first := nil;
			last := nil;
			nLeafs := 0;
			lt^.Divide(nodeCapacity, depth + 1 {$ifdef Container}, container {$endif});
			rt^.Divide(nodeCapacity, depth + 1 {$ifdef Container}, container {$endif});
		end;
	end;

	procedure classname.Divide;
	begin
		if Assigned(root) then root^.Divide(nodeCapacity, 1 {$ifdef Container}, container {$endif});
	end;

	function classname.tNode.RecalcBounding({$ifdef Container} container: pContainer {$endif}): boolean;
	var
		cur: pLeaf;
		newBnd: Bounding;
	begin
		if nLeafs > 0 then
		begin
			newBnd := {$ifdef use_point} Bounding.ByPoint {$endif} ({$define key_type:=first^.key} key2ctl);
			cur := first^.next;
			while Assigned(cur) do
			begin
				newBnd.Enlarge({$define key_type:=cur^.key} key2ctl);
				cur := cur^.next;
			end;
		end;
		if Assigned(lt) then
		begin
			if nLeafs = 0 then
				newBnd := lt^.bnd
			else
				newBnd.Enlarge(lt^.bnd);
			newBnd.Enlarge(rt^.bnd);
		end;
		result := (Assigned(lt) or (nLeafs > 0)) and (bnd.IsUndefined or (bnd <> newBnd));
		if result then bnd := newBnd;
	end;

	procedure classname.tNode.Propagate(doClear: boolean {$ifdef Container}; container: pContainer {$endif});
	var
		nn, node: pNode;
	begin
		Assert(doClear or (nLeafs > 0));
		node := @self;
		repeat
			if doClear and Assigned(node^.lt) and ((node^.lt^.Empty) or (node^.rt^.Empty)) then
			begin
				node^.lt^.CollapseTo(node);
				node^.rt^.CollapseTo(node);
				node^.lt^.Destroy; node^.lt := nil;
				node^.rt^.Destroy; node^.rt := nil;
			end;
			nn := node;
			node := node^.parent;
		until (not nn^.RecalcBounding({$ifdef Container} container {$endif})) or (not Assigned(node));
	end;

	function classname.tNode.Empty: boolean;
	begin
		result := (not Assigned(lt)) and (nLeafs = 0);
	end;

{$ifdef allow_change}
	function classname.tNode.Changed(const key: tKey; const oldCtl: LeafControl_t {$ifdef Container}; container: pContainer {$endif}): boolean;
	var
		cur: pLeaf;
	begin
		cur := first;
		while Assigned(cur) do
		begin
			if cur^.key <> key then cur := cur^.next else
			begin
			{$ifdef verify_changed} cur^.knownCtl := key2ctl; {$endif}
				if bnd.Enlarge(key2ctl) then Propagate(no {$ifdef Container}, container{$endif});
				exit(yes);
			end;
		end;

		result := Assigned(lt) and
			(
				(Contains(lt^.bnd, oldCtl) and lt^.Changed(key, oldCtl {$ifdef Container}, container {$endif})) or
				(Contains(rt^.bnd, oldCtl) and rt^.Changed(key, oldCtl {$ifdef Container}, container {$endif}))
			);
	end;

	procedure classname.Changed(const key: tKey; const oldCtl: LeafControl_t);
	begin
		if Assigned(root) then
			if not root^.Changed(key, oldCtl {$ifdef Container}, container {$endif}) then
				;
	end;
{$endif}

	procedure classname.tNode.CollapseTo(n: pNode);
	begin
		if Assigned(lt) then
		begin
			lt^.CollapseTo(n);
			rt^.CollapseTo(n);
			lt^.Destroy; lt := nil;
			rt^.Destroy; rt := nil;
		end;
		if (nLeafs > 0) and (n <> @self) then
		begin
			if n^.nLeafs > 0 then
			begin
				last^.next := n^.first;
				n^.first := first;
			end else
			begin
				n^.first := first;
				n^.last := last;
			end;
			n^.nLeafs += nLeafs;
			first := nil;
			last := nil;
			nLeafs := 0;
		end;
	end;

	function classname.tNode.ForEach(proc: tLeafProc; param: pointer): boolean;
	var
		cur: pLeaf;
	begin
		cur := first;
		while Assigned(cur) do
		begin
			if not proc(cur^.key, param) then exit(no);
			cur := cur^.next;
		end;

		if Assigned(lt) then
		begin
			if not lt^.ForEach(proc, param) then exit(no);
			if not rt^.ForEach(proc, param) then exit(no);
		end;
		result := yes;
	end;

	function classname.tNode.ForEachIntersected(var ibnd: Bounding; proc: tLeafProc; param: pointer {$ifdef Container}; container: pContainer {$endif}): boolean;
	var
		cur: pLeaf;
	begin
		if not Bounding.Intersects(ibnd, bnd) then exit(yes);
		if nLeafs <> 1 then
		begin
			cur := first;
			while Assigned(cur) do
			begin
				if Contains(ibnd, {$define key_type := cur^.key} key2ctl) then
					if not proc(cur^.key, param) then exit(no);
				cur := cur^.next;
			end;
		end else
			if not proc(first^.key, param) then exit(no);

		if Assigned(lt) then
		begin
			if not lt^.ForEachIntersected(ibnd, proc, param {$ifdef Container}, container {$endif}) then exit(no);
			if not rt^.ForEachIntersected(ibnd, proc, param {$ifdef Container}, container {$endif}) then exit(no);
		end;
		result := yes;
	end;

	procedure classname.ForEachIntersected(var ibnd: Bounding; proc: tLeafProc; param: pointer);
	begin
		if Assigned(root) then root^.ForEachIntersected(ibnd, proc, param {$ifdef Container}, container {$endif});
	end;

{$ifdef use_point}
	function classname.FindNearest(const point: Vec3; const maxDistance: float; out key: tKey): boolean;
	var
		bestSqrDistance, eta: float;
	begin
		if not Assigned(root) then exit(no);
		if maxDistance > 0.0 then
			eta := 1.01 * sqr(maxDistance)
		else
			eta := Infinity;
		bestSqrDistance := eta;
		Assert(@key = @key);
		root^.FindNearest(point, key, bestSqrDistance {$ifdef Container}, container {$endif});
		result := bestSqrDistance < eta;
	end;

	procedure classname.tNode.FindNearest(const point: Vec3; var key: tKey; var bestSqrDistance: float {$ifdef Container}; container: pContainer {$endif});
	var
		cur: pLeaf;
		sqDist: float;
		cl, cr: boolean;
	begin
		sqDist := SqrDistance(point, bnd.Center);
		if (sqDist >= sqr(bnd.Radius)) and (sqr(sqrt(sqDist) - bnd.Radius) >= bestSqrDistance) then exit;

		cur := first;
		while Assigned(cur) do
		begin
			sqDist := SqrDistance({$define key_type:=cur^.key} key2ctl, point);
			if sqDist < bestSqrDistance then
			begin
				bestSqrDistance := sqDist;
				key := cur^.key;
			end;
			cur := cur^.next;
		end;
		if Assigned(lt) then
		begin
			cl := lt^.bnd.Contains(point);
			cr := rt^.bnd.Contains(point);
			if cl and ((not cr) or (GlobalRNG.GetUint mod 2 = 0)) then
			begin
				lt^.FindNearest(point, key, bestSqrDistance {$ifdef Container}, container {$endif});
				rt^.FindNearest(point, key, bestSqrDistance {$ifdef Container}, container {$endif});
			end else
			begin
				rt^.FindNearest(point, key, bestSqrDistance {$ifdef Container}, container {$endif});
				lt^.FindNearest(point, key, bestSqrDistance {$ifdef Container}, container {$endif});
			end;
		end;
	end;
{$endif}

{$ifdef use_bounding}
	function classname.tNode.ForEachIntersected(const frustum: Frustum; proc: tLeafProc; param: pointer {$ifdef Container}; container: pContainer {$endif}): boolean;
	var
		cur: pLeaf;
	begin
		case frustum.ClassifySphere(bnd.Sphere) of
			bb_FullyOut: result := yes;
			bb_FullyIn: result := ForEach(proc, param);
			else
				if (bnd.Base = BasedOnSphere) or frustum.TestAABB(bnd.AABB) then
				begin
					if nLeafs <> 1 then
					begin
						cur := first;
						while Assigned(cur) do
						begin
							if frustum.TestAABB({$define key_type:=cur^.key} key2ctl.AABB) then
								if not proc(cur^.key, param) then exit(no);
							cur := cur^.next;
						end;
					end else
						if not proc(first^.key, param) then exit(no);

					if Assigned(lt) then
					begin
						if not lt^.ForEachIntersected(frustum, proc, param {$ifdef Container}, container {$endif}) then exit(no);
						if not rt^.ForEachIntersected(frustum, proc, param {$ifdef Container}, container {$endif}) then exit(no);
					end;
					result := yes;
				end else
					result := yes;
		end;
	end;

	procedure classname.ForEachIntersected(const frustum: Frustum; proc: tLeafProc; param: pointer);
	begin
		if Assigned(root) then root^.ForEachIntersected(frustum, proc, param {$ifdef Container}, container {$endif});
	end;

	procedure classname.tNode.GetLeafAABBs(var list: tAABBs; var ofs: uint {$ifdef Container}; container: pContainer {$endif});
	var
		cur: pLeaf;
	begin
		cur := first;
		while Assigned(cur) do
		begin
		{$ifdef verify_changed} cur^.VerifyChanged('GetLeafAABBs'); {$endif}
			list[ofs] := {$define key_type:=cur^.key} key2ctl.AABB;
			inc(ofs);
			cur := cur^.next;
		end;
		if Assigned(lt) then
		begin
			lt^.GetLeafAABBs(list, ofs {$ifdef Container}, container {$endif});
			rt^.GetLeafAABBs(list, ofs {$ifdef Container}, container {$endif});
		end;
	end;

	function classname.GetLeafAABBs: tAABBs;
	var
		ofs: uint;
	begin
		if not Assigned(root) then exit(nil);
		SetLength(result, nObjs);
		ofs := 0;
		root^.GetLeafAABBs(result, ofs);
	end;
{$endif}

	function classname.tNode.NodesCount: sint;
	begin
		result := 1;
		if Assigned(lt) then inc(result, lt^.NodesCount + rt^.NodesCount);
	end;

	function classname.GetAABB: AABB;
	begin
		if Assigned(root) then
			result := root^.bnd.AABB
		else
			result := AABB.Make(Vec3.Zero);
	end;

	function classname.GetNodeAABBs: tAABBs;
	var
		ofs: uint;
	begin
		if not Assigned(root) then exit(nil);
		SetLength(result, root^.NodesCount);
		ofs := 0;
		root^.GetNodeAABBs(result, ofs);
	end;

	procedure classname.tNode.GetNodeAABBs(var list: tAABBs; var ofs: uint);
	begin
		list[ofs] := bnd.AABB;
		inc(ofs);
		if Assigned(lt) then
		begin
			lt^.GetNodeAABBs(list, ofs);
			rt^.GetNodeAABBs(list, ofs);
		end;
	end;

	function classname.tNode.Contains(const bn: Bounding; const ctl: classname.LeafControl_t): boolean;
	begin
		result := {$ifdef use_bounding} Bounding.Intersects(bn, ctl) {$else} bn.Contains(ctl) {$endif}
	end;

{$ifdef verify_changed}
	function classname.tLeaf.VerifyChanged(const src: string; const ctl: LeafControl_t): boolean;
	begin
		result := knownCtl = ctl;
		if not result then Log('KdTree.Changed выполнена неверно ({0}): известен лист {1}, но реальный {2}' {$ifdef _key2string_} + ' ({3})' {$endif} + '.', src,
				ToString(knownCtl {$ifdef use_bounding} .AABB {$endif}),
				ToString(ctl {$ifdef use_bounding} .AABB {$endif}),
				{$ifdef _key2string_} key2string, {$endif}
				logError);
	end;

	function classname.tLeaf.VerifyChanged(const src: string): boolean;
	begin
		result := VerifyChanged(src, key2ctl);
	end;

	function classname.CheckIntegrity(const key: tKey): boolean;
	var
		node: pNode;
		leaf: pLeaf;
	begin
		result := Assigned(root) and root^.Find(key, node, leaf);
		if result then
			result := leaf^.VerifyChanged('CheckIntegrity')
		else
			Log('KdTree.Changed: узел не найден' {$ifdef _key2string_} + ' (' + key2string + ')' {$endif}, logError);
	end;
{$endif}

{$undef _key2ctl_} {$undef key2ctl}
{$undef _key2string_} {$undef key2string}
{$undef Container}
{$undef use_point} {$undef use_bounding} {$undef allow_change} {$undef verify_changed}
{$undef classname}
