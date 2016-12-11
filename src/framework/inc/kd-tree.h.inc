{$if defined(Debug) and defined(allow_change)} {-$define verify_changed} {$endif}

	classname = object
	private const
		DefaultKdLimit = 4;
	type
		LeafControl_t =
			{$if defined(use_bounding)} Bounding
			{$elseif defined(use_point)} Vec3
			{$else} {$error define leaf control type} {$endif};
		tKey = type key_type;
	{$ifdef _Container_} pContainer = _Container_; {$endif}

		pLeaf = ^tLeaf;
		tLeaf = object
			key: tKey;
			next: pLeaf;
		{$ifdef verify_changed} knownCtl: LeafControl_t; {$endif}
		{$ifdef verify_changed}
			function VerifyChanged(const src: string): boolean;
			function VerifyChanged(const src: string; const ctl: LeafControl_t): boolean;
		{$endif}
		end;

		tLeafProc = function(const key: key_type; param: pointer): boolean;
		tAABBs = array of AABB;

		pNode = ^tNode;
		tNode = object
			bnd: Bounding;
			nLeafs: uint;
			first, last: pLeaf;
			parent, lt, rt: pNode;
			function Create(newParent: pNode): pNode; static;
			procedure Destroy;
			procedure Clear;
			function Find(const key: tKey; var node: pNode; var leaf: pLeaf {$ifdef _Container_}; container: pContainer {$endif}): boolean;
			procedure Divide(nodeCapacity: uint; depth: sint {$ifdef _Container_}; container: pContainer {$endif});
		{$ifdef allow_change} function Changed(const key: tKey; const oldCtl: LeafControl_t {$ifdef _Container_}; container: pContainer {$endif}): boolean; {$endif}
			procedure CollapseTo(n: pNode);
			function RecalcBounding({$ifdef _Container_} container: pContainer {$endif}): boolean;
			procedure Propagate(doClear: boolean {$ifdef _Container_}; container: pContainer {$endif});
			function UltimateRemove(const key: tKey {$ifdef _Container_}; container: pContainer {$endif}): boolean;
			function Remove(const key: tKey; const ctl: LeafControl_t {$ifdef _Container_}; container: pContainer {$endif}): boolean;
			procedure AddNode(n: pLeaf; doPropagate: boolean {$ifdef _Container_}; container: pContainer {$endif});
			function Empty: boolean;
			function ForEach(proc: tLeafProc; param: pointer): boolean;
			function ForEachIntersected(var ibnd: Bounding; proc: tLeafProc; param: pointer {$ifdef _Container_}; container: pContainer {$endif}): boolean;
			procedure GetNodeAABBs(var list: tAABBs; var ofs: uint);
		{$ifdef use_point} procedure FindNearest(const point: Vec3; var key: tKey; var bestSqrDistance: float {$ifdef _Container_}; container: pContainer {$endif}); {$endif}
			function NodesCount: sint;
		{$ifdef use_bounding}
			function ForEachIntersected(const frustum: Frustum; proc: tLeafProc; param: pointer): boolean;
			procedure GetLeafAABBs(var list: tAABBs; var ofs: uint {$ifdef _Container_}; container: pContainer {$endif});
		{$endif}
			function Contains(const bn: Bounding; const ctl: classname.LeafControl_t): boolean; static; cinline
		end;

	var
		nodeCapacity, nObjs: uint;
		root: pNode;
	{$ifdef _Container_} container: pContainer; {$endif}
	public
		procedure Init({$ifdef _Container_} newContainer: pContainer; {$endif} newNodeCapacity: uint = DefaultKdLimit);
		procedure Done;
		procedure Clear;
		procedure Add(const key: tKey; propagate: boolean);
		procedure Remove(const key: tKey);
	{$ifdef allow_change} procedure Changed(const key: tKey; const oldCtl: LeafControl_t); {$endif}
		procedure Divide;
		procedure ForEachIntersected(var ibnd: Bounding; proc: tLeafProc; param: pointer);
	{$ifdef use_point} function FindNearest(const point: Vec3; const maxDistance: float; out key: tKey): boolean; {$endif}
		function GetAABB: AABB;
		function GetNodeAABBs: tAABBs;
	{$ifdef use_bounding}
		function GetLeafAABBs: tAABBs;
		procedure ForEachIntersected(const frustum: Frustum; proc: tLeafProc; param: pointer);
	{$endif}
	{$ifdef verify_changed} function CheckIntegrity(const key: tKey): boolean; {$endif}

		property ObjectsCount: uint read nObjs;
	public type
		AllowChange = {$ifdef allow_change} boolean {$else} record end {$endif};
		UsePoint = {$ifdef use_point} boolean {$else} record end {$endif};
		UseBounding = {$ifdef use_bounding} boolean {$else} record end {$endif};
		UseContainer = {$ifdef _Container_} boolean {$else} record end {$endif};
		VerifyChanged = {$ifdef verify_changed} boolean {$else} record end {$endif};
	end;

{$undef _Container_}
{$undef key_type}
{$undef use_point} {$undef use_bounding} {$undef allow_change} {$undef verify_changed}
{$undef classname}