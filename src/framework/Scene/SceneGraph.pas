unit SceneGraph;

{$include opts.inc}

interface

uses
	USystem, UMath, UClasses, SpatialIndex
{$ifdef use_serialization}, Streams {$endif}
{$ifdef Debug}, ULog, Utils {$endif};

type
	SceneKnowledge =
	(
		scene_Update,
		scene_DelayedUpdate,
		scene_OmniVisibleWithoutCulling,
		scene_Light,
		scene_Water
	);
	SceneKnowledgeSet = set of SceneKnowledge;

	ScenePrivate = object
		queryable: boolean;
		knowledgeId: array[SceneKnowledge] of sint;
		procedure Initialize;
		procedure Finalize;
	end;

	pSceneNode = ^SceneNode;
	SceneNode = object(&Object)
	public type
		OnUpdateProc = procedure(node: pSceneNode; const info: SingleDelegateInfo);
	{$define classname := List} {$define item_type := pSceneNode} {$include vector.h.inc}
	private var
		_startBnd, _bnd: pBounding;
	{$ifdef Debug} _lastFrameUpdated, {$endif} _lastFrameUpdatedVis: sint;
		_transform, _globalTransform: Transform;
		actions: EntityActions;
		parentId: sint;
		_serializable: boolean;
		procedure _SetTransform(const newTf: Transform);
		procedure _SetGlobalTransform(const newTf: Transform);
		procedure _SetLocalPos(const newPos: Vec3);
		procedure _SetLocalRot(const newRot: Quaternion);
		procedure _SetLocalScale(const newScale: float);
		procedure _SetBnd(newbnd: pBounding);
		procedure _SetStartBnd(newStartBnd: pBounding);
		procedure _MakeQueryable;
		procedure _UnmakeQueryable;
		function _KnowledgeRequired(know: SceneKnowledge): boolean;
		procedure _UpdateTransform(recursive: boolean = yes);
		procedure _UpdateBounding;
	protected
		scene: pSceneNode;
		procedure _OnApplyTransform; virtual;
		procedure _OnUpdate(const dt: float); virtual;
		procedure _OnUpdateVis; virtual;
		procedure _AfterAttach; virtual;
		procedure _BeforeDetach; virtual;
		function _SuitsTo(know: SceneKnowledge): boolean; virtual;
		procedure KnowledgeMayChange(know: SceneKnowledge; newState: boolean);
		function TransformBaseFor(node: pSceneNode): Transform; virtual;
		procedure _NotifyDetach(node: pSceneNode); virtual;
	public
		scp: ScenePrivate;
		childs: List;
		parent: pSceneNode;
		onUpdate: MultiDelegate;
		constructor Init;
		constructor DeseInit;
		destructor Done; virtual;
		procedure Update(const dt: float);
		procedure UpdateVis; cinline
		procedure AddAction(ac: pEntityAction);
		function CalculateLOD(const viewer: Vec3): float;
		procedure Attach(obj: pSceneNode);
		procedure Detach;
		procedure ChangeBounding(const newBnd: Bounding);
		procedure ChangeStartBounding(const newBnd: Bounding);
		function Cullable: boolean;
		function SerializableChildsCount: sint;

		property Serializable: boolean read _serializable write _serializable;
		property Root: pSceneNode read scene;
		property LocalTransform: Transform read _transform write _SetTransform;
		property globalTransform: Transform read _globalTransform write _SetGlobalTransform;
		property Bnd: pBounding read _bnd write _SetBnd;
		property StartBnd: pBounding read _startBnd write _SetStartBnd;
		property LocalPos: Vec3 read _transform.tr write _SetLocalPos;
		property LocalRot: Quaternion read _transform.rot write _SetLocalRot;
		property LocalScale: float read _transform.scale write _SetLocalScale;
		property WorldPos: Vec3 read _globalTransform.tr;
		property WorldRot: Quaternion read _globalTransform.rot;
		property WorldScale: float read _globalTransform.scale;
	end;

	pSceneKdTree = ^SceneKdTree;
	{$define classname:=SceneKdTree} {$define key_type:=pSceneNode} {$define use_bounding} {$define allow_change}
	{$include kd-tree.h.inc}

	pSceneNodeMove = ^SceneNodeMove;
	SceneNodeMove = object(Slide)
	public
		constructor Init(path: pDimensionalPath);
		destructor Done; virtual;
	protected
		procedure _Process(entity: pObject; const dt: float); virtual;
	private const
		SlideID = 'M';
	end;

	pSceneNodeRotate = ^SceneNodeRotate;
	SceneNodeRotate = object(Slide)
	public
		constructor Init(node: pSceneNode; const newTarget: Quaternion; path: pDimensionalPath);
		destructor Done; virtual;
	protected
		procedure _Process(entity: pObject; const dt: float); virtual;
	private
		_sRot, _tRot: Quaternion;
	const
		SlideID = 'R';
	end;

	pSceneNodeTimer = ^SceneNodeTimer;
	SceneNodeTimer = object(EntityAction)
	public type
		OnTimerProc = procedure(action: pSceneNodeTimer; const dt: float; const info: SingleDelegateInfo);
		pOnTimerArgs = ^OnTimerArgs;
		OnTimerArgs = record
			action: pSceneNodeTimer;
			dt: pFloat;
		end;
	private var
		_period: float;
		_timeAcc: float;
		_single, _now: boolean;
	protected
		procedure _Process(entity: pObject; const dt: float); virtual;
	public
		onTimer: MultiDelegate;
		constructor Init(const newPeriod: float = 1.0);
		destructor Done; virtual;

		property Period: float read _period write _period;
		property Now: boolean read _now write _now;
		property Single: boolean read _single write _single;
	end;

implementation

uses
	USkeleton, Physics, MMSystem, Scene, RenderLists
{$ifdef use_serialization}, Serialization {$endif}
{$ifdef Profile}, Profile {$endif};

	{$define classname := SceneNode.List}
	{$include vector.pp.inc}

	{$define classname:=SceneKdTree} {$define _key2ctl_:=key_type^.bnd^}
	{$include kd-tree.pp.inc}

	procedure ScenePrivate.Initialize;
	var
		know: SceneKnowledge;
	begin
		queryable := no;
		for know in SceneKnowledge do
			knowledgeId[know] := -1;
	end;

	procedure ScenePrivate.Finalize;
	begin
	end;

	procedure _OnUpdateChanged(var md: MultiDelegate; param: pointer);
	var
		node: pSceneNode absolute param;
	begin
		node^.KnowledgeMayChange(scene_Update, not md.Empty);
	end;

	constructor SceneNode.Init;
	begin
		inherited Init;
		_lastFrameUpdatedVis := -123;
	{$ifdef Debug} _lastFrameUpdated := -123; {$endif}
		_transform := Transform.Identity;
		_globalTransform := Transform.Identity;
		_startBnd := nil;
		_bnd := nil;
		childs.Init;
		parent := nil;
		parentId := -1;
		scene := nil;

		_bnd := nil;
		actions.Init;
		onUpdate.Init;
		onUpdate.SetCallbacks(@_OnUpdateChanged, @self);
		_serializable := yes;
		scp.Initialize;
	end;

	constructor SceneNode.DeseInit;
	begin
		Init;
	end;

	destructor SceneNode.Done;
	begin
		scp.Finalize;
		onUpdate.Done;
		while childs.n > 0 do childs.Last^.Detach;
		childs.Done;
		actions.Done;
		if Assigned(_startBnd) then dispose(_bnd);
		inherited Done;
	end;

	procedure SceneNode.UpdateVis;
	begin
	trace_call('SceneNode.UpdateVis');
		if _lastFrameUpdatedVis <> mm.FrameNo then
		begin
			_lastFrameUpdatedVis := mm.FrameNo;
			_OnUpdateVis;
		end;
	leave_call
	end;

	procedure SceneNode.Update(const dt: float);
	begin
	trace_call('SceneNode.Update');
	{$ifdef Debug}
		Assert(_lastFrameUpdated <> mm.FrameNo, 'updated twice');
		_lastFrameUpdated := mm.FrameNo;
	{$endif}
		_OnUpdate(dt);
	leave_call
	end;

	procedure SceneNode.Attach(obj: pSceneNode);
	begin
		if not Assigned(obj) then exit;
		Assert(not Assigned(obj^.parent));
		childs.Push(MakeRef(obj));
		obj^.parent := @self;
		obj^.parentId := childs.n - 1;
		if Assigned(scene) then obj^._AfterAttach;
	end;

{$ifdef Debug}
	function SameNode(n: pSceneNode; param: pointer): boolean;
	begin
		result := n = pSceneNode(param);
	end;
{$endif}

	procedure SceneNode.Detach;
	var
		id: sint;
	begin
		if not Assigned(parent) then exit;
		id := parentId;
		Assert(id >= 0);
	{$ifdef Debug}
		Assert(parent^.childs.items[id] = @self, Format('{0} @ {1} <> {2}, right {3}.', ToString(parent^.childs.items[id]),
		                                                ToString(id), ToString(@self), ToString(parent^.childs.Find(@SameNode, @self))));
	{$endif}
		parent^.childs.Last^.parentId := id;
		parent^.childs.RemoveReplace(id);
		parent^._NotifyDetach(@self);
		parent := nil;
		parentId := -1;

		if Assigned(scene) then _BeforeDetach;
		ReleaseWeak(@self);
	end;

	procedure SceneNode._SetTransform(const newTf: Transform);
	begin
		if newTf <> _transform then
		begin
			_transform := newTf;
			_UpdateTransform;
		end;
	end;

	procedure SceneNode._SetGlobalTransform(const newTf: Transform);
	begin
		if _globalTransform <> newTf then
		begin
			_globalTransform := newTf;
			if Assigned(parent) then
				_transform := parent^.TransformBaseFor(@self).Inversed * _globalTransform
			else
				_transform := newTf;
			_UpdateTransform;
		end;
	end;

	procedure SceneNode._SetLocalPos(const newPos: Vec3);
	begin
		if newPos <> _transform.tr then
		begin
			_transform.tr := newPos;
			_UpdateTransform;
		end;
	end;

	procedure SceneNode._SetLocalRot(const newRot: Quaternion);
	begin
		if newRot <> _transform.rot then
		begin
			_transform.rot := newRot;
			_UpdateTransform;
		end;
	end;

	procedure SceneNode._SetLocalScale(const newScale: float);
	begin
		if not Equals(_transform.scale, newScale) then
		begin
			_transform.scale := newScale;
			_UpdateTransform;
		end;
	end;

	procedure SceneNode._SetBnd(newbnd: pBounding);
	begin
		if _bnd = newbnd then exit;
		_UnmakeQueryable;
		if Assigned(_startBnd) then dispose(_bnd);
		_bnd := newbnd;
		_startBnd := nil;
		_MakeQueryable;
		KnowledgeMayChange(scene_OmniVisibleWithoutCulling, not Assigned(_bnd));
	end;

	procedure SceneNode._SetStartBnd(newStartBnd: pBounding);
	var
		b: Bounding;
		newBnd: pBounding;
	begin
		if _startBnd = newStartBnd then exit;
		if Assigned(newStartBnd) then
		begin
			b := _globalTransform * newStartBnd^;
			if Assigned(Bnd) then ChangeBounding(b) else
			begin
				new(newBnd);
				newBnd^ := b;
				Bnd := newBnd;
			end;
		end else
			Bnd := nil;
		_startBnd := newStartBnd;
	end;

	procedure SceneNode.ChangeBounding(const newBnd: Bounding);
	var
		oldBnd: Bounding;
		q: boolean;
	begin
		Assert(Assigned(bnd));
		if bnd^ = newBnd then exit;
		q := scp.queryable;
		if q then oldBnd := bnd^;
		bnd^ := newBnd;
		if q then pScene(scene)^.QueryableChanged(@self, oldBnd);
	end;

	procedure SceneNode.ChangeStartBounding(const newBnd: Bounding);
	begin
		Assert(Assigned(startBnd));
		if startBnd^ <> newBnd then
		begin
			startBnd^ := newBnd;
			_UpdateBounding;
		end;
	end;

	function SceneNode.Cullable: boolean;
	begin
		result := Assigned(_bnd);
	end;

	function IsSerializableNode(n: pSceneNode): boolean;
	begin
		result := n^.Serializable;
	end;

	function SceneNode.SerializableChildsCount: sint;
	begin
		result := childs.Matching(@IsSerializableNode);
	end;

	function SceneNode.TransformBaseFor(node: pSceneNode): Transform;
	begin
		Assert(@node = @node);
		result := _globalTransform;
	end;

	procedure SceneNode._NotifyDetach(node: pSceneNode);
	begin
		Assert(@node = @node);
	end;

	procedure CallAfterAttach(n: pSceneNode);
	begin
		n^._AfterAttach;
	end;

	procedure SceneNode._AfterAttach;
	var
		know: SceneKnowledge;
	begin
		// Это может быть и сама сцена, ага.
		if not Assigned(scene) then
		begin
			Assert(Assigned(parent));
			Assert(Assigned(parent^.scene));
			scene := parent^.scene;
			pScene(scene)^.NotifyAtDt(@self, yes);
		end else
		begin
			if TypeOf(self) <> TypeOf(SceneRoot) then
			begin
			{$ifdef Debug} Log('AfterAttach: shit just happened, though harmless' {$ifdef use_serialization} + ' (' + SerializationDB.Shared^.TypeName(typeof(self)) + ')' {$endif}, logWarning); {$endif}
				pScene(scene)^.NotifyAtDt(@self, yes); // ?
			end else
				Assert(scene = @self, 'не ТА сцена!');
		end;
		_UpdateTransform(no);

		childs.ForEach(@CallAfterAttach);
		for know in SceneKnowledge do
			KnowledgeMayChange(know, yes);
		_MakeQueryable;
	end;

	procedure SceneNode._BeforeDetach;
	var
		i: uint;
	begin
		Assert(Assigned(scene));
		i := childs.n;
		while i > 0 do
		begin
			dec(i);
			if (i < childs.n) and Assigned(childs.items[i]^.scene) then
				childs.items[i]^._BeforeDetach;
		end;

		pScene(scene)^.Forget(@self);
		_UnmakeQueryable;
		if scene <> @self then
		begin
			pScene(scene)^.NotifyAtDt(@self, no);
			scene := nil;
		end;
	end;

	procedure SceneNode._OnApplyTransform; begin end;
	procedure SceneNode._OnUpdateVis; begin end;

	procedure _CallOnUpdate(const info: SingleDelegateInfo; param: pointer);
	var
		node: pSceneNode absolute param;
	begin
		SceneNode.OnUpdateProc(info.proc)(node, info);
	end;

	procedure SceneNode._OnUpdate(const dt: float);
	begin
		if not actions.Process(@self, dt) then
			KnowledgeMayChange(scene_Update, no);

		if not onUpdate.Empty then
			onUpdate.Call(@_CallOnUpdate, @self);
	end;

	function SceneNode._SuitsTo(know: SceneKnowledge): boolean;
	begin
		Assert(@know = @know);
		result := no;
	end;

	function SceneNode._KnowledgeRequired(know: SceneKnowledge): boolean;
	begin
		case know of
			scene_Update: result := (not actions.Empty) or (not onUpdate.Empty) or _SuitsTo(know);
			scene_OmniVisibleWithoutCulling: result := (not Assigned(_bnd)) and _SuitsTo(know);
			else result := _SuitsTo(know);
		end;
	end;

	procedure SceneNode.KnowledgeMayChange(know: SceneKnowledge; newState: boolean);
	var
		lastState, realState: boolean;
	begin
		if not Assigned(scene) then exit;
		lastState := pScene(scene)^.Remembered(@self, know);
		if newState <> lastState then
		begin
			realState := _KnowledgeRequired(know);
			if realState <> lastState then
				if realState then
					pScene(scene)^.Remember(@self, know)
				else
					pScene(scene)^.Forget(@self, know);
		end;
	end;

	procedure SceneNode._UpdateBounding;
	begin
		Assert(Assigned(StartBnd));
		if Assigned(scene) then
			ChangeBounding(_globalTransform * StartBnd^);
	end;

	procedure CallUpdateTransform(n: pSceneNode);
	begin
		n^._UpdateTransform;
	end;

	procedure SceneNode._UpdateTransform(recursive: boolean = yes);
	begin
		if Assigned(parent) then
			_globalTransform := parent^.TransformBaseFor(@self) * _transform
		else
			_globalTransform := _transform;
		if recursive then childs.ForEach(@CallUpdateTransform);
		if Assigned(StartBnd) then _UpdateBounding;
		if Assigned(scene) then _OnApplyTransform;
	end;

	procedure SceneNode._MakeQueryable;
	begin
		if Assigned(scene) and Assigned(_bnd) then
			pScene(scene)^.MakeQueryable(@self);
	end;

	procedure SceneNode._UnmakeQueryable;
	begin
		if Assigned(scene) and Assigned(_bnd) then
			pScene(scene)^.UnmakeQueryable(@self);
	end;

	procedure SceneNode.AddAction(ac: pEntityAction);
	begin
		if actions.Add(@self, ac) then KnowledgeMayChange(scene_Update, yes);
	end;

	function SceneNode.CalculateLOD(const viewer: Vec3): float;
	const
		BoundingRadiusLimit = 4.0; // TODO: костыль
	var
		d: float;
	begin
	trace_call('SceneNode.CalculateLOD');
		if Assigned(_bnd) then
		begin
			d := Distance(viewer, _bnd^.center);
			if d <= 2.0 * _bnd^.radius then
				result := 1.0
			else
				result := min(8.0 * min(_bnd^.radius, BoundingRadiusLimit) / d, 1.0)
		end else
			result := 1.0;
	leave_call
	end;

	constructor SceneNodeMove.Init(path: pDimensionalPath);
	begin
		inherited Init(path, SlideID);
	end;

	destructor SceneNodeMove.Done;
	begin
		inherited Done;
	end;

	procedure SceneNodeMove._Process(entity: pObject; const dt: float);
	var
		node: pSceneNode absolute entity;
	begin
		inherited _Process(entity, dt);
		node^.LocalPos := dm.CurrentV3;
	end;

	constructor SceneNodeRotate.Init(node: pSceneNode; const newTarget: Quaternion; path: pDimensionalPath);
	begin
		inherited Init(path, SlideID);
		_sRot := node^.LocalRot;
		_tRot := newTarget;
	end;

	destructor SceneNodeRotate.Done;
	begin
		inherited Done;
	end;

	procedure SceneNodeRotate._Process(entity: pObject; const dt: float);
	var
		node: pSceneNode absolute entity;
	begin
		inherited _Process(entity, dt);
		if dm.Finished then
			node^.LocalRot := _tRot
		else
			node^.LocalRot := slerp(_sRot, _tRot, dm.CurrentF);
	end;

	constructor SceneNodeTimer.Init(const newPeriod: float = 1.0);
	begin
		inherited Init;
		_period := newPeriod;
		_timeAcc := 0.0;
		_single := no;
		_now := no;
		onTimer.Init;
	end;

	destructor SceneNodeTimer.Done;
	begin
		onTimer.Done;
		inherited Done;
	end;

	procedure _CallOnTimer(const info: SingleDelegateInfo; param: pointer);
	var
		args: SceneNodeTimer.pOnTimerArgs absolute param;
	begin
		with args^ do SceneNodeTimer.OnTimerProc(info.proc)(args^.action, args^.dt^, info);
	end;

	procedure SceneNodeTimer._Process(entity: pObject; const dt: float);
	var
		args: OnTimerArgs;
	begin
		Assert(@entity = @entity);
		_timeAcc += dt;
		if (_timeAcc >= _period) or (_now) then
		begin
			if not onTimer.Empty then
			begin
				args.action := @self;
				args.dt := @_timeAcc;
				onTimer.Call(@_CallOnTimer, @args);
			end;
			if _timeAcc < 2.0 * _period then
				_timeAcc := modf(_timeAcc, _period)
			else
				_timeAcc := 0.0;
			if _now then _now := no;
			if _single then Stop(reason_Done);
		end;
	end;

{$ifdef use_serialization}
const
	NODE_HAS_ONUPDATE_BIT  = 1 shl 0;
	NODE_HAS_TRANSFORM_BIT = 1 shl 1;
	NODE_HAS_ACTIONS_BIT   = 1 shl 2;
	NODE_HAS_CHILDS_BIT    = 1 shl 3;

	// Описанные объёмы не сериализуются!
	procedure SerializeSceneNode(se: pSerializer; obj: pointer);
	var
		n: pSceneNode absolute obj;
		flags, i, nSeCh: uint;
	begin
		with se^ do
		begin
			Assert(n^.Serializable);
			flags := 0;
			if not n^.onUpdate.Empty  then flags := flags or NODE_HAS_ONUPDATE_BIT;
			if n^.LocalTransform <> Transform.Identity then flags := flags or NODE_HAS_TRANSFORM_BIT;
			if not n^.actions.Empty then flags := flags or NODE_HAS_ACTIONS_BIT;
			nSeCh := n^.SerializableChildsCount;
			if nSeCh > 0 then flags := flags or NODE_HAS_CHILDS_BIT;

			Serialize_ui8(stream, flags);
			if (flags and NODE_HAS_TRANSFORM_BIT) <> 0 then Serialize_tf32r16(stream, n^._transform);
			if (flags and NODE_HAS_ACTIONS_BIT) <> 0 then
				SeObject(@n^.actions, ObjType_EntityActions);
			if nSeCh > 0 then
			begin
				VarInt.Write(stream, nSeCh);
				i := 0;
				while i < n^.childs.n do
				begin
					if n^.childs.items[i]^.Serializable then
						SeObject(n^.childs.items[i]);
					inc(i);
				end;
			end;
			if not n^.onUpdate.Empty then SeObject(@n^.onUpdate, ObjType_MultiDelegate);
		end;
	end;

	procedure DeserializeSceneNode(de: pDeserializer; obj: pointer);
	var
		n: pSceneNode absolute obj;
		flags, childsCount: uint;
		i: sint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			if (flags and NODE_HAS_TRANSFORM_BIT) <> 0 then n^._transform := Deserialize_tf32r16(stream);
			if (flags and NODE_HAS_ACTIONS_BIT) <> 0 then
			begin
				n^.actions.Done;
				DeWeakAtR(n^.actions);
			end;
			if (flags and NODE_HAS_CHILDS_BIT) <> 0 then
			begin
				childsCount := VarInt.Read(stream);
				n^.childs.Grow(childsCount);
				for i := 0 to childsCount - 1 do
					DeObjectA(n^.childs.items[i]);
			end;
			if (flags and NODE_HAS_ONUPDATE_BIT) <> 0 then
			begin
				n^.onUpdate.Done;
				DeWeakAtR(n^.onUpdate);
			end;
		end;
	end;

	procedure SetNodeParentAndID(node: pSceneNode; id: uint; parent: pointer);
	begin
		node^.parent := parent;
		node^.parentId := id;
	end;

	procedure SceneNodeDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		n: pSceneNode absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: n^.DeseInit;
			de_After:
				begin
					if Assigned(n^._startBnd) then
					begin
						new(n^._bnd);
						n^._bnd^ := Bounding.ByPoint(Vec3.Zero);
					end;
					n^.onUpdate.SetCallbacks(@_OnUpdateChanged, n);
					n^.childs.ForEach(@SetNodeParentAndID, n);
				end;
			de_After2:
				begin
					if (not Assigned(n^.parent)) and (not Assigned(n^.scene)) then
						n^._UpdateTransform;
				end;
		end;
	end;

	procedure SceneNodeMoveDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		ac: pSceneNodeMove absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: ac^.DeseInit;
		end;
	end;

	procedure SerializeSceneNodeRotate(se: pSerializer; obj: pointer);
	var
		ac: pSceneNodeRotate absolute obj;
	begin
		with se^ do
		begin
			Serialize_IQuat16(stream, ac^._sRot);
			Serialize_IQuat16(stream, ac^._tRot);
		end;
	end;

	procedure DeserializeSceneNodeRotate(de: pDeserializer; obj: pointer);
	var
		ac: pSceneNodeRotate absolute obj;
	begin
		with de^ do
		begin
			ac^._sRot := Deserialize_IQuat16(stream);
			ac^._tRot := Deserialize_IQuat16(stream);
		end;
	end;

	procedure SceneNodeRotateDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		ac: pSceneNodeRotate absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: ac^.DeseInit;
		end;
	end;

const
	TIMER_SINGLE_BIT      = 1 shl 0;
	TIMER_NOW_BIT         = 1 shl 1;
	TIMER_HAS_TIMEACC_BIT = 1 shl 2;
	TIMER_HAS_ONTIMER_BIT = 1 shl 3; // ну мало ли

	procedure SerializeSceneNodeTimer(se: pSerializer; obj: pointer);
	var
		ac: pSceneNodeTimer absolute obj;
		flags: uint;
	begin
		with se^ do
		begin
			flags := 0;
			if ac^._single then flags := flags or TIMER_SINGLE_BIT;
			if ac^._now then flags := flags or TIMER_NOW_BIT;
			if NotZero(ac^._timeAcc) then flags := flags or TIMER_HAS_TIMEACC_BIT;
			if not ac^.onTimer.Empty then flags := flags or TIMER_HAS_ONTIMER_BIT;
			Serialize_ui8(stream, flags);
			Serialize_f32(stream, ac^._period);
			if NotZero(ac^._timeAcc) then Serialize_fN16(stream, ac^._timeAcc, 0.0, ac^._period);
			if not ac^.onTimer.Empty then SeObject(@ac^.onTimer, ObjType_MultiDelegate);
		end;
	end;

	procedure DeserializeSceneNodeTimer(de: pDeserializer; obj: pointer);
	var
		ac: pSceneNodeTimer absolute obj;
		flags: uint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			ac^._single := (flags and TIMER_SINGLE_BIT) <> 0;
			ac^._now := (flags and TIMER_NOW_BIT) <> 0;
			ac^._period := Deserialize_f32(stream);
			if (flags and TIMER_HAS_TIMEACC_BIT) <> 0 then ac^._timeAcc := Deserialize_fN16(stream, 0.0, ac^._period) else ac^._timeAcc := 0.0;
			if (flags and TIMER_HAS_ONTIMER_BIT) <> 0 then DeWeakAtR(ac^.onTimer) else ac^.onTimer.Init;
		end;
	end;

	procedure SceneNodeTimerDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		ac: pSceneNodeTimer absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: ac^.DeseInit;
		end;
	end;
{$endif}

	procedure Init;
	begin
	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Scene node', TypeOf(SceneNode), nil, sizeof(SceneNode), yes,
		               @SerializeSceneNode, @DeserializeSceneNode, nil, @SceneNodeDeSpecial)
		^.RegisterType('Scene node move', TypeOf(SceneNodeMove), TypeOf(Slide), sizeof(SceneNodeMove), yes,
		               nil, nil, nil, @SceneNodeMoveDeSpecial)
		^.RegisterType('Scene node rotate', TypeOf(SceneNodeRotate), TypeOf(Slide), sizeof(SceneNodeRotate), yes,
		               @SerializeSceneNodeRotate, @DeserializeSceneNodeRotate, nil, @SceneNodeRotateDeSpecial)
		^.RegisterType('Scene node timer', TypeOf(SceneNodeTimer), TypeOf(EntityAction), sizeof(SceneNodeTimer), yes,
		               @SerializeSceneNodeTimer, @DeserializeSceneNodeTimer, nil, @SceneNodeTimerDeSpecial);
	{$endif}
	end;

initialization
	&Unit('SceneGraph').Initialize(@Init);
end.
