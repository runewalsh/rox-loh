unit PathFinding;

{$include opts.inc}

interface

uses
	USystem, Errors, Streams, UMath, Random, UClasses, Script, Utils, Algo, SpatialIndex
{$ifdef Debug}, ULog{$endif};

type
	SearchMethod = (search_Default, search_Wave, search_AStar);

	pDistance_t = ^distance_t;
	distance_t = uint;

const
	HugeDist = High(distance_t) div 8; // огромное расстояние, но достаточно далёкое от High(distance_t)

type
	pWay = ^Way;

	pWaypoints = ^Waypoints;
	Waypoints = object(&Object)
	private const
		DistanceUintK = 10;
		InvDistanceUintK = 1.0 / DistanceUintK;
		AStar_PrecalculatedPriorityK = 1;
		DefaultMaxDistToCorners = 3.5;
		KdLimit = 16;

	private type
		nEdges_t = type uint;

		SerializationInfo = object
			lenFmt, vidFmt: UiBinaryFormat;
			procedure Serialize(stream: pStream);
			function Deserialize(stream: pStream): SerializationInfo; static;
		end;

		pEdgeRec = ^EdgeRec;
		EdgeRec = object
			v: uint;
			len: distance_t;
			disableCounter: sint;
			procedure Serialize(s: pStream; const info: SerializationInfo);
			function Deserialize(s: pStream; const info: SerializationInfo): EdgeRec; static;
		end;

		PointRec = object
			position: Vec3;
			edgesTo: pEdgeRec;
			nEdgesTo: nEdges_t;
			procedure Initialize(const pos: Vec3);
			procedure Initialize(const cp: PointRec; vshift: sint; const tf: Transform);
			procedure Finalize;
			procedure Serialize(s: pStream; var wp: Waypoints; thisId: uint; const aabb: AABB; const info: SerializationInfo);
			function Deserialize(s: pStream; var wp: Waypoints; thisId: uint; const aabb: AABB; const info: SerializationInfo): PointRec; static;
			function FindEdgeTo(v2: uint): pEdgeRec;
			function IsBidirEdge(edgeId: sint; var wp: Waypoints; thisId: uint): boolean;
		end;

		ExclusionRec = record
			name: PoolString;
			sphere: Sphere;
		end;

	public type
		{$define classname:=tQueue_PointID} {$define item_type:=uint} {$include queue.h.inc}

		pPointsKdTree = ^PointsKdTree;
		{$define classname:=PointsKdTree} {$define key_type:=uint} {$define use_point} {$define _Container_:=pWaypoints}
		{$include kd-tree.h.inc}

		NeighbourDesc = record
			id: sint;
			sqrDistance: float;
		end;
		NeighboursArray = array of NeighbourDesc;

		ReplicaOp = (op_Append, op_Point, op_Edge, op_BiEdge);
		pOneReplica = ^OneReplica;
		OneReplica = object
			constructor Init;
			destructor Done; virtual;
			function Op: ReplicaOp; virtual; abstract;
			procedure Apply(var wp: Waypoints); virtual; abstract;
		end;

		pAppendReplica = ^AppendReplica;
		AppendReplica = object(OneReplica)
			name: PoolString;
			awp: pWaypoints;
			tf: Transform;
			destructor Done; virtual;
			function Op: ReplicaOp; virtual;
			procedure Apply(var wp: Waypoints); virtual;
		end;

		pPointReplica = ^PointReplica;
		PointReplica = object(OneReplica)
			pos: Vec3;
			function Op: ReplicaOp; virtual;
			procedure Apply(var wp: Waypoints); virtual;
		end;

		pEdgeReplica = ^EdgeReplica;
		EdgeReplica = object(OneReplica)
			a, b: uint;
			distance: distance_t;
			function Op: ReplicaOp; virtual;
			procedure Apply(var wp: Waypoints); virtual;
		end;

		pBiEdgeReplica = ^BiEdgeReplica;
		BiEdgeReplica = object(EdgeReplica)
			procedure Apply(var wp: Waypoints); virtual;
			function Op: ReplicaOp; virtual;
		end;

		pReplica = ^Replica;
		Replica = object
		const
			Sizes: array[ReplicaOp] of size_t = (sizeof(AppendReplica), sizeof(PointReplica), sizeof(EdgeReplica), sizeof(BiEdgeReplica));
		var
			blob: Blob;
			locked: boolean;
			constructor Init;
			destructor Done;
			function Add(newOp: ReplicaOp): pointer;
			procedure AddAppend(const newName: PoolString; newWp: pWaypoints; const newTf: Transform);
			procedure AddPoint(const newPos: Vec3);
			procedure AddEdge(newA, newB: uint; const newDistance: float);
			procedure AddBiEdge(newA, newB: uint; const newDistance: float);
			procedure Replicate(var wp: Waypoints);
			function GetSerializationInfo: SerializationInfo;
		end;

	private var
		_kd: PointsKdTree;
		_distances: pDistance_t;
		_maxDistToCorners: float;
		_excls: array of ExclusionRec;
		_replica: pReplica;

		function _EncodeDist(const dist: float): distance_t; static; cinline
		function _DecodeDist(const dist: distance_t): float; static; cinline
		procedure _Deserialize(s: pStream);
		procedure _InitializeEdgeList(var list: pEdgeRec; var n: nEdges_t); static;
		procedure _FinalizeEdgeList(var list: pEdgeRec); static;
		procedure _AddEdgeToList(var list: pEdgeRec; var n: nEdges_t; const edge: EdgeRec); static;
		function _RemoveEdgeFromList(var list: pEdgeRec; var n: nEdges_t; b: uint): boolean; static;
		procedure _ChangeVertexID(old, new: sint);
		procedure _RemoveIsolated;
		function _ValidatePoint(v: sint): boolean; cinline
		function _FindWay_Wave(a, b: uint; const maxLen: distance_t; np: uint; p: pUint; out len: distance_t): sint;
		function _EstimateDistance(a, b: uint): distance_t; cinline
		function _FindWay_AStar(a, b: uint; const maxLen: distance_t; np: uint; p: pUint; out len: distance_t): sint;
		function _ProcessExclusion(const sph: Sphere; delta: sint): sint;
		function _ShouldReplicate: boolean;
		function _NowReplicating: boolean;

	public type
		BuildEdgeCallback = function(const a, b: Vec3; param: pointer): boolean;
		PredefinedBuildEdgeCallback = (edge_BodyCast);
	public const
		MaxPointsInWay = 1000;
	public var
		pts: array of PointRec;
		constructor Init(replicable: boolean);
		constructor Init(s: pStream);
		destructor Done; virtual;
		function Serialize(s: pStream): boolean;
		procedure Append(const name: PoolString; wp: pWaypoints; const tf: Transform; const maxDist: float; glue: BuildEdgeCallback; param: pointer = nil);
		procedure Remove(const name: PoolString);
		procedure AddExclusion(const exclName: PoolString; const sphere: Sphere);
		procedure RemoveExclusion(const exclName: PoolString);
		function GetKd: pPointsKdTree;

		function AddPoint(const pos: Vec3): sint;
		function FindNearestPoint(const p: Vec3; const maxDistance: float): sint;
		function FindNearestPoints(const p: Vec3; const maxDistance: float; maxN: uint): NeighboursArray;

		procedure AddEdge(a, b: sint; const theLen: float; bidir: boolean = yes);
		procedure AddEdge(a, b: sint; bidir: boolean = yes);
		function RemoveEdge(a, b: sint; bidir: boolean = yes): sint;
		function HasEdge(a, b: sint): boolean;
		procedure BuildEdges(const maxDist: float; maxN: uint; fn: BuildEdgeCallback; fnParam: pointer = nil);
		procedure BuildEdges(const maxDist: float; maxN: uint; fn: PredefinedBuildEdgeCallback; fnParam: pointer = nil);
		function GetEdgesCount: uint;

		procedure CalcDistances(a: uint; theDist: pFloat);
		procedure PrecalculateDistances;

		function FindWay(const a, b: Vec3; const theMaxLen: float; method: SearchMethod): pWay;
		function FindWay(a, b: sint; const theMaxLen: float; np: uint; p: pUint; out theLen: float; method: SearchMethod): sint;
		property MaxDistToCorners: float read _maxDistToCorners write _maxDistToCorners;

	const
		Signature = 'WAY';
	end;

	Way = object(&Object)
	private type
		ModeEnum = (way_Points);
	private const
		SmoothPointsK = 2.0;
	private var
		_mode: ModeEnum;
		_len: float;
		_points: array of Vec3;
		_nSmoothPoints: sint;
		procedure _Update(recalculateLen: boolean);
		function _GetSmoothPoint(id: sint): Vec3;
		function _GetTarget: Vec3; cinline
	public
		constructor Init(newMode: ModeEnum);
		constructor InitFromWaypoints(wp: pWaypoints; nWpPoints: sint; wpPoints: pUint; const theLen: float);
		constructor InitFromWaypoints(wp: pWaypoints; const start: Vec3;
			nWpPoints: sint; wpPoints: pUint; const wpLen: float; const finish: Vec3);
		destructor Done; virtual;
		function GetNPoints: sint;
		function GetPoint(id: sint): Vec3;
		function GetNSegments: sint;
		procedure GetSegment(id: sint; out a, b: Vec3);
		function GetSegmentB(id: sint): Vec3;
		function GetNControlPoints: sint;
		procedure Shift(const dist: float);

		property Target: Vec3 read _GetTarget;
		property Len: float read _len;
	end;

	procedure OpenScript(var script: ScriptState);

implementation

uses
	Physics
{$ifdef use_serialization}, Serialization, Scene {$endif};

	{$define classname:=Waypoints.tQueue_PointID} {$include queue.pp.inc}

	{$define classname:=Waypoints.PointsKdTree} {$define _key2ctl_:=container^.pts[key_type].position}
	{$include kd-tree.pp.inc}

type
	{$define classname:=tPrioritizeByDist} {$define item_type:=uint} {$define priority_type:=distance_t} {$define allow_prioritize}
	{$include priority_queue.h.inc}
	{$define classname:=tPrioritizeByDist} {$include priority_queue.pp.inc}

const
	SearchMethodIds: array[SearchMethod] of string = ('default', 'wave', 'A*');

var
	defaultMethod: SearchMethod = search_AStar;

	procedure Waypoints._InitializeEdgeList(var list: pEdgeRec; var n: nEdges_t);
	begin
		list := nil;
		n := 0;
	end;

	procedure Waypoints._FinalizeEdgeList(var list: pEdgeRec);
	begin
		FreeMem(list);
	end;

	procedure Waypoints.SerializationInfo.Serialize(stream: pStream);
	begin
		Serialize_ui8(stream, bitpack([ord(vidFmt), 3, ord(lenFmt), 3]));
	end;

	function Waypoints.SerializationInfo.Deserialize(stream: pStream): SerializationInfo;
	var
		t: uint;
	begin
		t := Deserialize_ui8(stream);
		result.vidFmt := UiBinaryFormat(bits(t, 0, 3));
		result.lenFmt := UiBinaryFormat(bits(t, 3, 3));
	end;

	procedure Waypoints.PointRec.Initialize(const pos: Vec3);
	begin
		position := pos;
		Waypoints._InitializeEdgeList(edgesTo, nEdgesTo);
	end;

	procedure Waypoints.PointRec.Initialize(const cp: PointRec; vshift: sint; const tf: Transform);
	var
		i: sint;
	begin
		position := tf * cp.position;
		nEdgesTo := cp.nEdgesTo;
		edgesTo := GetMem(nEdgesTo * sizeof(edgesTo[0]));
		for i := 0 to sint(nEdgesTo) - 1 do
		begin
			edgesTo[i] := cp.edgesTo[i];
			edgesTo[i].v += uint(vshift);
		end;
	end;

	procedure Waypoints.PointRec.Finalize;
	begin
		Waypoints._FinalizeEdgeList(edgesTo);
	end;

	procedure Waypoints.PointRec.Serialize(s: pStream; var wp: Waypoints; thisId: uint; const aabb: AABB; const info: SerializationInfo);
	var
		i, nGeneric, nBidir: sint;
	begin
		Serialize_vec3N16(s, position, aabb.A, aabb.B);

		nGeneric := 0;
		nBidir := 0;
		for i := 0 to sint(nEdgesTo) - 1 do
			if IsBidirEdge(i, wp, thisId) then
			begin
				if (thisId > edgesTo[i].v) then inc(nBidir);
			end else
				inc(nGeneric);
		Serialize_ui8(s, nGeneric);
		Serialize_ui8(s, nBidir);

		for i := 0 to sint(nEdgesTo) - 1 do
			if not IsBidirEdge(i, wp, thisId) then
				edgesTo[i].Serialize(s, info);

		for i := 0 to sint(nEdgesTo) - 1 do
			if (thisId > edgesTo[i].v) and IsBidirEdge(i, wp, thisId) then
				edgesTo[i].Serialize(s, info);
	end;

	function Waypoints.PointRec.Deserialize(s: pStream; var wp: Waypoints; thisId: uint; const aabb: AABB; const info: SerializationInfo): PointRec;
	var
		i, nGeneric, nBidir: sint;
		edge: EdgeRec;
		srcv: uint;
	begin
		result.position := Deserialize_vec3N16(s, aabb.A, aabb.B);

		nGeneric := Deserialize_ui8(s);
		nBidir := Deserialize_ui8(s);

		result.nEdgesTo := nGeneric + nBidir;
		result.edgesTo := GetMem(result.nEdgesTo * sizeof(result.edgesTo[0]));
		for i := 0 to nGeneric - 1 do
			result.edgesTo[i] := EdgeRec.Deserialize(s, info);
		for i := nGeneric to nGeneric + nBidir - 1 do
		begin
			result.edgesTo[i] := EdgeRec.Deserialize(s, info);
			edge := result.edgesTo[i];
			srcv := edge.v;
			edge.v := thisId;
			Waypoints._AddEdgeToList(wp.pts[srcv].edgesTo, wp.pts[srcv].nEdgesTo, edge);
		end;
	end;

	function Waypoints.PointRec.FindEdgeTo(v2: uint): pEdgeRec;
	var
		i: sint;
	begin
		for i := 0 to sint(nEdgesTo) - 1 do
			if edgesTo[i].v = v2 then
				exit(@edgesTo[i]);
		result := nil;
	end;

	function Waypoints.PointRec.IsBidirEdge(edgeId: sint; var wp: Waypoints; thisId: uint): boolean;
	var
		e2: pEdgeRec;
	begin
		e2 := wp.pts[edgesTo[edgeId].v].FindEdgeTo(thisId);
		result := Assigned(e2) and (e2^.len = edgesTo[edgeId].len);
	end;

	procedure Waypoints.EdgeRec.Serialize(s: pStream; const info: SerializationInfo);
	begin
		Serialize_ui(s, v, info.vidFmt);
		Serialize_ui(s, len, info.lenFmt);
	end;

	function Waypoints.EdgeRec.Deserialize(s: pStream; const info: SerializationInfo): EdgeRec;
	begin
		result.v := Deserialize_ui(s, info.vidFmt);
		result.len := Deserialize_ui(s, info.lenFmt);
		result.disableCounter := 0;
	end;

	constructor Waypoints.Init(replicable: boolean);
	begin
		inherited Init;
		pts := nil;
		_distances := nil;
		_maxDistToCorners := DefaultMaxDistToCorners;
		_kd.Init(@self, KdLimit);
		if replicable then _replica := new(pReplica, Init) else _replica := nil;
	end;

	constructor Waypoints.Init(s: pStream);
	begin
		Init(no);
		_Deserialize(s);
	end;

	destructor Waypoints.Done;
	var
		i: sint;
	begin
		if Assigned(_replica) then dispose(_replica, Done);
		FreeMem(_distances);
		for i := 0 to High(pts) do
			pts[i].Finalize;
		_kd.Done;
		inherited Done;
	end;

	function Waypoints._EncodeDist(const dist: float): distance_t;
	begin
		if dist <> Infinity then
			result := round(dist * DistanceUintK)
		else
			result := HugeDist;
	end;

	function Waypoints._DecodeDist(const dist: distance_t): float;
	begin
		if dist < HugeDist then
			result := dist * InvDistanceUintK
		else
			result := Infinity;
	end;

	function Waypoints.Serialize(s: pStream): boolean;
		function _ChooseLenFmt: UiBinaryFormat;
		var
			i, j: sint;
			ch: UiBinaryFormatChooser;
		begin
			ch.Init('длин рёбер');
			for i := 0 to High(pts) do
				for j := 0 to sint(pts[i].nEdgesTo) - 1 do
					ch.Note(pts[i].edgesTo[j].len);
			result := ch.DestructiveChoose;
		end;
	var
		i: sint;
		aabb: UMath.AABB;
		info: SerializationInfo;
	begin
		result := no;
		if not Assigned(MakeRef(s)) then exit;

		info.lenFmt := _ChooseLenFmt;
		info.vidFmt := UiBinaryFormatChooser.Choose(uint(max(High(pts), 0)), 'индексов вершин');

		s^.Write(Signature);
		info.Serialize(s);
		aabb := _kd.GetAABB;
		Serialize_vec3f32(s, aabb.A);
		Serialize_vec3f32(s, aabb.B);
		VarInt.Write(s, length(pts));
		for i := 0 to High(pts) do
			pts[i].Serialize(s, self, i, aabb, info);

		Release(s);
		result := yes;
	end;

	procedure Waypoints._Deserialize(s: pStream);
	var
		i: sint;
		aabb: UMath.AABB;
		info: SerializationInfo;
	begin
		MakeRef(s);
		try
		{$ifdef Debug} LogR('Загрузка путевых точек из ' + StreamPath.Log(s^.path) + '... '); {$endif}
			Deserialize_signature(s, Signature, no);
			info := SerializationInfo.Deserialize(s);
			aabb.A := Deserialize_vec3f32(s);
			aabb.B := Deserialize_vec3f32(s);
			SetLength(pts, VarInt.Read(s));
			for i := 0 to High(pts) do
				pts[i].Initialize(Vec3.Zero);
			for i := 0 to High(pts) do
			begin
				pts[i] := PointRec.Deserialize(s, self, i, aabb, info);
				_kd.Add(i, no);
			end;
			_kd.Divide;

			// PrecalculateDistances;

		{$ifdef Debug} Log('OK (Nv = ' + ToString(length(pts)) + ', Ne = ' + ToString(GetEdgesCount) + ')', logOK); {$endif}
		finally
			Release(s);
		end;
	end;

	procedure Waypoints.Append(const name: PoolString; wp: pWaypoints; const tf: Transform;
		const maxDist: float; glue: BuildEdgeCallback; param: pointer = nil);
	const
		Probes: array[0 .. 2] of float = (0.3, 0.6, 1.0);
		MaxEdgesCreated = 3;
		NEdgesEstimation = 2 * MaxEdgesCreated;
		GoodProbeNPts = 4;
	var
		i, j, probe, cur, start, nconn: sint;
		curMaxDist: float;
		neis: array of NeighboursArray;
		nei: NeighboursArray;
	begin
		Assert(Assigned(_replica));
		if not Assigned(wp) then exit;
		FreeMem(_distances);
		if _ShouldReplicate then _replica^.AddAppend(name, wp, tf);

		if not _NowReplicating then
		begin
			for i := 0 to High(_excls) do
				_ProcessExclusion(_excls[i].sphere, -1);
			SetLength(neis, length(wp^.pts));
			for probe := Low(Probes) to High(Probes) do
			begin
				nconn := 0;
				curMaxDist := Probes[probe] * maxDist;

				for i := 0 to High(neis) do
				begin
					neis[i] := FindNearestPoints(tf * wp^.pts[i].position, curMaxDist, NEdgesEstimation);
					if length(neis[i]) > 0 then inc(nconn);
				end;
				if nconn >= GoodProbeNPts then break;
			end;
		end;

		start := length(pts);
		SetLength(pts, length(pts) + length(wp^.pts));
		for i := 0 to High(wp^.pts) do
		begin
			pts[start + i].Initialize(wp^.pts[i], start, tf);
			_kd.Add(start + i, no);
		end;

		if not _NowReplicating then
		begin
			_kd.Divide;

			for i := 0 to High(wp^.pts) do
			begin
				nei := neis[i];
				cur := start + i;
				nconn := 0;
				for j := 0 to High(nei) do
				begin
					Assert(start > nei[j].id);
					if ((not Assigned(glue)) or (glue(pts[cur].position, pts[nei[j].id].position, param))) then
					begin
						AddEdge(cur, nei[j].id);
						inc(nconn);
						if nconn >= MaxEdgesCreated then break;
					end;
				end;
			end;

			for i := 0 to High(_excls) do
				_ProcessExclusion(_excls[i].sphere, +1);
		end;

	{$ifdef Debug}
		Log('Присоединены путевые точки "' + name + '" (' + ToString(length(wp^.pts)) + ' v., ' + ToString(wp^.GetEdgesCount) + ' e.)');
	{$endif}
	end;

	procedure Waypoints.Remove(const name: PoolString);
	begin
		Assert(@name = @name);
		Fatal('unimplemented');
	end;

	procedure Waypoints.AddExclusion(const exclName: PoolString; const sphere: Sphere);
	{$ifdef Debug}
	var
		nPts: sint;
		t: Ticks;
	{$endif}
	begin
	{$ifdef Debug}
		LogR('Новое исключение пути: "' + exclName + '" (сфера, R = ' + ToString(sphere.radius) + ')... ');
		t := Ticks.Get;
	{$endif}
		SetLength(_excls, length(_excls) + 1);
		_excls[High(_excls)].name := exclName;
		_excls[High(_excls)].sphere := sphere;

	{$ifdef Debug} nPts := {$endif} _ProcessExclusion(sphere, +1);
	{$ifdef Debug}
		Log('Исключение пути "' + exclName + '" добавлено (вершин: ' + ToString(nPts) + ', ' + ToString((Ticks.Get - t).ToMilliseconds) + ' мс)', logOK);
	{$endif}
	end;

	function Waypoints._ProcessExclusion(const sph: Sphere; delta: sint): sint;
	var
		i, j: sint;
		in_pts: array of sint;
		edge: pEdgeRec;
	begin
		in_pts := nil;
		for i := 0 to High(pts) do
			if sph.Contains(pts[i].position) then
			begin
				SetLength(in_pts, length(in_pts) + 1);
				in_pts[High(in_pts)] := i;
			end;

		for i := 0 to High(in_pts) do
			for j := 0 to sint(pts[in_pts[i]].nEdgesTo) - 1 do
			begin
				pts[in_pts[i]].edgesTo[j].disableCounter += delta;
				edge := pts[pts[in_pts[i]].edgesTo[j].v].FindEdgeTo(in_pts[i]);
				if Assigned(edge) then edge^.disableCounter += delta;
			end;
		result := length(in_pts);
	end;

	function Waypoints._ShouldReplicate: boolean;
	begin
		result := Assigned(_replica) and not _replica^.locked;
	end;

	function Waypoints._NowReplicating: boolean;
	begin
		result := Assigned(_replica) and _replica^.locked;
	end;

	procedure Waypoints.RemoveExclusion(const exclName: PoolString);
	var
		i: sint;
	{$ifdef Debug}
		nPts: sint;
		t: Ticks;
	{$endif}
	begin
		for i := 0 to High(_excls) do
			if _excls[i].name = exclName then
			begin
			{$ifdef Debug}
				LogR('Удаляю исключение пути: "' + exclName + '"... ');
				t := Ticks.Get;
				nPts :=
			{$endif}
				_ProcessExclusion(_excls[i].sphere, -1);
			{$ifdef Debug}
				Log('Исключение пути "' + exclName + '" удалено (вершин: ' + ToString(nPts) + ', ' + ToString((Ticks.Get - t).ToMilliseconds) + ' мс)', logOK);
			{$endif}
				_excls[i] := _excls[High(_excls)];
				SetLength(_excls, length(_excls) - 1);
				exit;
			end;
	{$ifdef Debug} Log('Исключение пути "' + exclName + '" не найдено', logWarning); {$endif}
	end;

	function Waypoints.GetKd: pPointsKdTree;
	begin
		result := @_kd;
	end;

	function Waypoints.AddPoint(const pos: Vec3): sint;
	begin
		result := length(pts);
		SetLength(pts, result + 1);
		pts[result].Initialize(pos);
		_kd.Add(result, no);
		if _ShouldReplicate then _replica^.AddPoint(pos);
	end;

	function Waypoints.FindNearestPoint(const p: Vec3; const maxDistance: float): sint;
	var
		bestId: uint;
	begin
		if _kd.FindNearest(p, 0, bestId) and (SqrDistance(p, pts[bestId].position) <= sqr(maxDistance)) then
			result := bestId
		else
			result := -1;
	{$ifdef Debug}
		if result < 0 then Log('Не удалось найти ближайшую к (' + ToString(p) + ') точку (maxDist = ' + ToString(maxDistance) + ')', logWarning);
	{$endif}
	end;

type
	pFindNearestPointsParams = ^tFindNearestPointsParams;
	tFindNearestPointsParams = record
		wp: pWaypoints;
		point: Vec3;
		sqMaxDist: float;
		nei: Waypoints.NeighboursArray;
	end;

	function __AddNeighbour(const id: uint; param: pointer): boolean;
	var
		p: pFindNearestPointsParams absolute param;
		nei: Waypoints.NeighbourDesc;
	begin
		result := yes;
		nei.sqrDistance := SqrDistance(p^.point, p^.wp^.pts[id].position);
		if nei.sqrDistance <= p^.sqMaxDist then
		begin
			nei.id := id;
			SetLength(p^.nei, length(p^.nei) + 1);
			p^.nei[High(p^.nei)] := nei;
		end;
	end;

{$define procname:=sort_by_distance} {$define elem:=Waypoints.NeighbourDesc} {$define less := _1.sqrDistance < _2.sqrDistance} {$define openarray} {$include sort.inc}

	function Waypoints.FindNearestPoints(const p: Vec3; const maxDistance: float; maxN: uint): NeighboursArray;
	var
		bnd: Bounding;
		param: tFindNearestPointsParams;
	begin
		bnd := Bounding.BySphere(p, maxDistance);
		param.wp := @self;
		param.point := p;
		param.sqMaxDist := sqr(maxDistance);
		param.nei := nil;
		_kd.ForEachIntersected(bnd, @__AddNeighbour, @param);

		result := param.nei;
		sort_by_distance(result);
		if uint(length(result)) > maxN then SetLength(result, maxN);
	end;

	procedure Waypoints._AddEdgeToList(var list: pEdgeRec; var n: nEdges_t; const edge: EdgeRec);
	begin
		inc(n);
		ReallocMem(list, n * sizeof(list[0]));
		list[n - 1] := edge;
	{$ifdef Debug} stat.Note(max_edges_incident_to_waypoint_vertex, n); {$endif}
	end;

	function Waypoints._RemoveEdgeFromList(var list: pEdgeRec; var n: nEdges_t; b: uint): boolean;
	var
		i: sint;
	begin
		for i := 0 to sint(n) - 1 do
			if list[i].v = b then
			begin
				dec(n);
				if uint(i) <> n then list[i] := list[n];
				ReallocMem(list, n * sizeof(list[0]));
				exit(yes);
			end;
		result := no;
	end;

	procedure Waypoints._ChangeVertexID(old, new: sint);
	var
		i: sint;
		edge: pEdgeRec;
	begin
		for i := 0 to High(pts) do
		begin
			edge := pts[i].FindEdgeTo(old);
			if Assigned(edge) then edge^.v := new;
		end;
	end;

	procedure Waypoints._RemoveIsolated;
	var
		i: sint;
	{$ifdef Debug}
		nRem: sint;
		t: Ticks;
	{$endif}
	begin
	{$ifdef Debug}
		nRem := 0;
		LogR('Удаление изолированных точек... ');
		t := Ticks.Get;
	{$endif}
		for i := High(pts) downto 0 do
		begin
			if pts[i].nEdgesTo = 0 then
			begin
				_ChangeVertexID(High(pts), i);
				pts[i] := pts[High(pts)];
				SetLength(pts, length(pts) - 1);
			{$ifdef Debug} inc(nRem); {$endif}
			end;
		end;
	{$ifdef Debug}
		if nRem > 0 then
			Log('Удалено точек: ' + ToString(nRem) + ' (' + ToString((Ticks.Get - t).ToMicroseconds) + ' мкс)', logOK)
		else
			Log('Изолированные точки не найдены');
	{$endif}
	end;

	function Waypoints._ValidatePoint(v: sint): boolean;
	begin
		result := (v >= 0) and (v < length(pts));
	end;

	procedure Waypoints.AddEdge(a, b: sint; const theLen: float; bidir: boolean = yes);
	var
		edge: EdgeRec;
	begin
	{$ifdef Debug}
		if not (_ValidatePoint(a) and _ValidatePoint(b)) then
		begin
			Log('Waypoints.AddEdge: неверный индекс', logError);
			exit;
		end;
	{$endif}
		if Assigned(pts[a].FindEdgeTo(b)) or Assigned(pts[b].FindEdgeTo(a)) then
		begin
		{$ifdef Debug} Log('Хотя бы одно из рёбер ' + ToString(a) + ' -> ' + ToString(b) + ' уже существует', logWarning); {$endif}
		end;

		edge.len := _EncodeDist(theLen);
		edge.v := b;
		edge.disableCounter := 0;
		_AddEdgeToList(pts[a].edgesTo, pts[a].nEdgesTo, edge);
		if bidir then
		begin
			edge.v := a;
			_AddEdgeToList(pts[b].edgesTo, pts[b].nEdgesTo, edge);
		end;
		if _ShouldReplicate then
			if bidir then
				_replica^.AddBiEdge(a, b, theLen)
			else
				_replica^.AddEdge(a, b, theLen);
	end;

	procedure Waypoints.AddEdge(a, b: sint; bidir: boolean = yes);
	begin
		AddEdge(a, b, Distance(pts[a].position, pts[b].position), bidir);
	end;

	function Waypoints.RemoveEdge(a, b: sint; bidir: boolean = yes): sint;
	begin
		Assert(not _ShouldReplicate, 'RemoveEdge replication unsupported');
		result := 0;
	{$ifdef Debug}
		if not (_ValidatePoint(a) and _ValidatePoint(b)) then
		begin
			Log('Waypoints.RemoveEdge: неверный индекс', logError);
			exit;
		end;
	{$endif}
		if _RemoveEdgeFromList(pts[a].edgesTo, pts[a].nEdgesTo, b) then
			inc(result);
		if bidir and _RemoveEdgeFromList(pts[b].edgesTo, pts[b].nEdgesTo, a) then
			inc(result);
	end;

	function Waypoints.HasEdge(a, b: sint): boolean;
	begin
	{$ifdef Debug}
		if not (_ValidatePoint(a) and _ValidatePoint(b)) then
		begin
			Log('Waypoints.HasEdge: неверный индекс', logError);
			exit(no);
		end;
	{$endif}
		result := Assigned(pts[a].FindEdgeTo(b));
	end;

	procedure Waypoints.BuildEdges(const maxDist: float; maxN: uint; fn: BuildEdgeCallback; fnParam: pointer = nil);
	var
		i, j: sint;
		nei: NeighboursArray;
	{$ifdef Debug} t: Ticks; {$endif}
	begin
		Assert(not _ShouldReplicate, 'BuildEdges replication unsupported');
	{$ifdef Debug}
		LogR('Генерация рёбер (Nv = ' + ToString(length(pts)) + ')... ');
		t := Ticks.Get;
	{$endif}
		_kd.Divide;

		for i := 0 to High(pts) do
		begin
			nei := FindNearestPoints(pts[i].position, maxDist, maxN);
			for j := 0 to High(nei) do
				if (nei[j].id < i) and ((not Assigned(fn)) or (fn(pts[nei[j].id].position, pts[i].position, fnParam))) then
					AddEdge(nei[j].id, i);
		end;
	{$ifdef Debug} Log('Рёбра сгенерированы, Ne = ' + ToString(GetEdgesCount) + ', ' + ToString((Ticks.Get - t).ToMilliseconds) + ' мс', logOK); {$endif}
		_RemoveIsolated;
	end;

	function EdgeFitsBody(const a, b: Vec3; param: pointer): boolean;
	var
		rigid: pRigidBody absolute param;
	begin
		result := (not rigid^.Primitive^.RayCast(a, b)) and (not rigid^.Primitive^.RayCast(b, a));
	end;

	procedure Waypoints.BuildEdges(const maxDist: float; maxN: uint; fn: PredefinedBuildEdgeCallback; fnParam: pointer = nil);
	var
		objParam: pObject absolute fnParam;
	begin
		case fn of
			edge_BodyCast:
				begin
					Assert((not Assigned(objParam)) or (TypeOf(objParam^) = TypeOf(RigidBody)), 'edge_BodyCast requires body as parameter');
					if Assigned(objParam) then
					begin
					{$ifdef Debug} LogR('Режим: body-cast; ', logDebug); {$endif}
						BuildEdges(maxDist, maxN, @EdgeFitsBody, objParam)
					end
				{$ifdef Debug} else Log('BuildEdges: body-cast без body', logError) {$endif};
				end;
		end;
	end;

	function Waypoints.GetEdgesCount: uint;
	var
		i: sint;
	begin
		result := 0;
		for i := 0 to High(pts) do
			inc(result, pts[i].nEdgesTo);
	end;

	procedure Waypoints.CalcDistances(a: uint; theDist: pFloat);
	var
		que: tQueue_PointID;
		dist: array of distance_t;
		i: sint;
		v: uint;
		newlen: distance_t;
	begin
		que.Init(32 + length(pts) div 8);

		SetLength(dist, length(pts));
		for i := 0 to High(dist) do
			dist[i] := HugeDist;
		que.Put(a);
		dist[a] := 0;

		repeat
			if not que.Get(v) then break;

			for i := 0 to sint(pts[v].nEdgesTo) - 1 do
			begin
				newlen := dist[v] + pts[v].edgesTo[i].len;
				if newlen < dist[pts[v].edgesTo[i].v] then
				begin
					dist[pts[v].edgesTo[i].v] := newlen;
					que.Put(pts[v].edgesTo[i].v);
				end;
			end;
		until no;

		for i := 0 to High(pts) do
			theDist[i] := _DecodeDist(dist[i]);
		que.Done;
	end;

	procedure Waypoints.PrecalculateDistances;
	var
		d: pDistance_t;
		cd: pFloat;
		a, b: sint;
	begin
		FreeMem(_distances);
		d := GetMem(length(pts) * length(pts) * sizeof(distance_t));
		cd := GetMem(length(pts) * sizeof(float));
		for a := 0 to High(pts) do
		begin
			CalcDistances(a, cd);
			for b := 0 to High(pts) do
			begin
				d[a * length(pts) + b] := _EncodeDist(cd[b]);
				d[b * length(pts) + a] := _EncodeDist(cd[b]);
			end;
		end;
		FreeMem(cd);
		_distances := d;
	end;

	function Waypoints._FindWay_Wave(a, b: uint; const maxLen: distance_t; np: uint; p: pUint; out len: distance_t): sint;
	var
		que: tQueue_PointID;
		etc: array of record
			prev, nsteps: uint;
			len: distance_t;
		end;
		i, n: sint;
		v: uint;
		newlen: distance_t;
		newnsteps: uint;
	begin
		result := 0;
		i := 32 + uint(length(pts)) div 16;
		if Assigned(p) then i := min(i, np);
		que.Init(i);

		SetLength(etc, length(pts));
		for i := 0 to High(etc) do
			etc[i].nsteps := 0;
		que.Put(a);
		etc[a].len := 0;
		etc[a].nsteps := 1;

		repeat
			if not que.Get(v) then break;
			if v = b then
			begin
				n := etc[v].nsteps;
				result := n;
				len := etc[v].len;
				if Assigned(p) then
					while n > 0 do
					begin
						dec(n);
						p[n] := v;
						Assert((n > 0) or (v = a));
						v := etc[v].prev;
					end;
				break;
			end;

			newnsteps := etc[v].nsteps + 1;
			if Assigned(p) and (newnsteps > np) then continue;
			for i := 0 to sint(pts[v].nEdgesTo) - 1 do
			begin
				if pts[v].edgesTo[i].disableCounter > 0 then continue;
				newlen := etc[v].len + pts[v].edgesTo[i].len;
				if (maxLen > 0) and (newlen > maxLen) then continue;
				if (etc[pts[v].edgesTo[i].v].nsteps = 0) or (newlen < etc[pts[v].edgesTo[i].v].len) then
				begin
					etc[pts[v].edgesTo[i].v].prev := v;
					etc[pts[v].edgesTo[i].v].len := newlen;
					etc[pts[v].edgesTo[i].v].nsteps := newnsteps;
					que.Put(pts[v].edgesTo[i].v);
				end;
			end;
		until no;

		que.Done;
	end;

	function Waypoints._EstimateDistance(a, b: uint): distance_t;
	var
		pa, pb: Vec3;
	begin
		if Assigned(_distances) then result := AStar_PrecalculatedPriorityK * _distances[a * uint(length(pts)) + b] else
		begin
			pa := pts[a].position;
			pb := pts[b].position;
			result := _EncodeDist(Distance(pa, pb));
		end;
	end;

	function Waypoints._FindWay_AStar(a, b: uint; const maxLen: distance_t; np: uint; p: pUint; out len: distance_t): sint;
	var
		// g - стоимость пути от старта
		// h - эвристическая оценка пути до финиша
		// приоритет в очереди — g + h
		etc: array of record
			g, h, len: distance_t;
			handle: tPrioritizeByDist.handle_t;
			passed: boolean;
			prev, nsteps: uint;
		end;
		openset: tPrioritizeByDist;
		cur, v2: uint;
		newscore, newlen, h: distance_t;
		isBetter, shouldOpen: boolean;
		i: sint;
		newnsteps, n: uint;
	begin
		result := 0;
		openset.Init;
		SetLength(etc, length(pts));
		for i := 0 to High(etc) do
		begin
			etc[i].passed := no;
			etc[i].handle := openset.NullHandle;
		end;

		etc[a].nsteps := 1;
		etc[a].g := 0;
		h := _EstimateDistance(a, b);
		etc[a].h := h;
		etc[a].len := 0;
		openset.Put(a, 0 + h, @etc[a].handle);

		while openset.Get(cur) do
		begin
			if cur = b then
			begin
				n := etc[b].nsteps;
				result := n;
				len := etc[b].len;
				if Assigned(p) then
					while n > 0 do
					begin
						dec(n);
						p[n] := cur;
						Assert((n > 0) or (cur = a));
						cur := etc[cur].prev;
					end;
				break;
			end;
			etc[cur].passed := yes;

			newnsteps := etc[cur].nsteps + 1;
			if Assigned(p) and (newnsteps > np) then continue;

			for i := 0 to sint(pts[cur].nEdgesTo) - 1 do
			begin
				if pts[cur].edgesTo[i].disableCounter > 0 then continue;
				v2 := pts[cur].edgesTo[i].v;
				if etc[v2].passed then continue;
				newlen := etc[cur].len + pts[cur].edgesTo[i].len;
				if (maxLen > 0) and (newlen > maxLen) then continue;

				newscore := etc[cur].g + pts[cur].edgesTo[i].len;

				shouldOpen := etc[v2].handle = openset.NullHandle;
				isBetter := shouldOpen or (newscore < etc[v2].g);

				if isBetter then
				begin
					etc[v2].prev := cur;
					etc[v2].nsteps := newnsteps;
					etc[v2].g := newscore;
					h := _EstimateDistance(v2, b);
					etc[v2].h := h;
					etc[v2].len := newlen;
					if not shouldOpen then
						openset.Bump(etc[v2].handle, newscore + h);
				end;

				if shouldOpen then
					openset.Put(v2, etc[v2].g + etc[v2].h, @etc[v2].handle);
			end;
		end;

		openset.Done;
	end;

	function Waypoints.FindWay(a, b: sint; const theMaxLen: float; np: uint; p: pUint; out theLen: float; method: SearchMethod): sint;
	var
		len, maxLen: distance_t;
	begin
		result := 0;
		if not (_ValidatePoint(a) and _ValidatePoint(b)) then exit {$ifdef Debug} (-2) {$endif};
		if method = search_Default then method := defaultMethod;
		maxLen := _EncodeDist(theMaxLen);

		case method of
			search_Wave: result := _FindWay_Wave(a, b, maxLen, np, p, len);
			search_AStar: result := _FindWay_AStar(a, b, maxLen, np, p, len);
			else raise ExhaustiveCase(ord(method), 'FindWay.method');
		end;

		if result <> 0 then
			theLen := _DecodeDist(len);
	end;

	function Waypoints.FindWay(const a, b: Vec3; const theMaxLen: float; method: SearchMethod): pWay;
	var
		ipts: pUint;
		nPts, ia, ib: sint;
		wayLen: float;
	{$ifdef Debug} t: Ticks; {$endif}
	begin
	{$ifdef Debug}
		LogR('Поиск пути из A{0} в B{1}... ', ToString(a), ToString(b));
		t := Ticks.Get;
	{$endif}
		nPts := MaxPointsInWay;
		ipts := GetMem(nPts * sizeof(uint));
		ia := FindNearestPoint(a, _maxDistToCorners);
		if _ValidatePoint(ia) then ib := FindNearestPoint(b, _maxDistToCorners) else ib := -1;
		nPts := FindWay(ia, ib, theMaxLen, nPts, ipts, wayLen, method);

	{$ifdef Debug}
		if nPts <> -2 then
		begin
			stat.Increment(n_way_searches);
			stat.Increment(way_searches_time, (Ticks.Get - t).ToSeconds);
		end;
	{$endif}

		if nPts > 0 then
		begin
		{$ifdef Debug} Log('Путь найден: len = ' + ToString(wayLen) + ', N = ' + ToString(nPts) + ' (' + ToString((Ticks.Get - t).ToMilliseconds) + ' мс)', logOK); {$endif}
			result := new(pWay, InitFromWaypoints(@self, a, nPts, ipts, wayLen, b));
		end else
		begin
		{$ifdef Debug}
			if nPts <> -2 then
				Log('Путь не найден (' + ToString((Ticks.Get - t).ToMilliseconds) + ' мс)', logWarning)
			else
				Log('Путь не найден', logWarning);
		{$endif}
			result := nil;
		end;
		FreeMem(ipts);
	end;

	constructor Waypoints.OneReplica.Init;
	begin
	end;

	destructor Waypoints.OneReplica.Done;
	begin
	end;

	destructor Waypoints.AppendReplica.Done;
	begin
		Release(awp);
		Finalize(name);
		inherited Done;
	end;

	function Waypoints.AppendReplica.Op: ReplicaOp;
	begin
		result := op_Append;
	end;

	procedure Waypoints.AppendReplica.Apply(var wp: Waypoints);
	begin
		wp.Append(name, awp, tf, 0.0, nil, nil);
	end;

	function Waypoints.PointReplica.Op: ReplicaOp;
	begin
		result := op_Point;
	end;

	procedure Waypoints.PointReplica.Apply(var wp: Waypoints);
	begin
		wp.AddPoint(pos);
	end;

	function Waypoints.EdgeReplica.Op: ReplicaOp;
	begin
		result := op_Edge;
	end;

	procedure Waypoints.EdgeReplica.Apply(var wp: Waypoints);
	begin
		wp.AddEdge(a, b, Waypoints._DecodeDist(distance), no);
	end;

	function Waypoints.BiEdgeReplica.Op: ReplicaOp;
	begin
		result := op_BiEdge;
	end;

	procedure Waypoints.BiEdgeReplica.Apply(var wp: Waypoints);
	begin
		wp.AddEdge(a, b, Waypoints._DecodeDist(distance), yes);
	end;

	constructor Waypoints.Replica.Init;
	begin
		blob.Init;
		locked := no;
	end;

	procedure _KillReplica(var replica);
	begin
		Waypoints.OneReplica(replica).Done;
	end;

	function _ReplicaSize(var replica): size_t;
	begin
		result := sizeof(Waypoints.OneReplica(replica));
	end;

	destructor Waypoints.Replica.Done;
	begin
		blob.Done(@_KillReplica, @_ReplicaSize);
	end;

	function Waypoints.Replica.Add(newOp: ReplicaOp): pointer;
	begin
		result := blob.Add(Sizes[newOp]);
		case newOp of
			op_Append: AppendReplica(result^).Init;
			op_Point:   PointReplica(result^).Init;
			op_Edge:     EdgeReplica(result^).Init;
			op_BiEdge: BiEdgeReplica(result^).Init;
			else Assert(no);
		end;
	end;

	procedure Waypoints.Replica.AddAppend(const newName: PoolString; newWp: pWaypoints; const newTf: Transform);
	begin
		with AppendReplica(Add(op_Append)^) do
		begin
			name := newName;
			awp  := MakeRef(newWp);
			tf   := newTf;
		end;
	end;

	procedure Waypoints.Replica.AddPoint(const newPos: Vec3);
	begin
		with PointReplica(Add(op_Point)^) do
			pos := newPos;
	end;

	procedure Waypoints.Replica.AddEdge(newA, newB: uint; const newDistance: float);
	begin
		with EdgeReplica(Add(op_Edge)^) do
		begin
			a        := newA;
			b        := newB;
			distance := Waypoints._EncodeDist(newDistance);
		end;
	end;

	procedure Waypoints.Replica.AddBiEdge(newA, newB: uint; const newDistance: float);
	begin
		with BiEdgeReplica(Add(op_BiEdge)^) do
		begin
			Init;
			a        := newA;
			b        := newB;
			distance := Waypoints._EncodeDist(newDistance);
		end;
	end;

	procedure Waypoints.Replica.Replicate(var wp: Waypoints);
	var
		it: pointer;
	begin
		Assert(not locked);
		locked := yes;
		it := nil;
		while blob.Next(it, @_ReplicaSize) do
			OneReplica(it^).Apply(wp);
		wp._kd.Divide;
		locked := no;
	end;

	function Waypoints.Replica.GetSerializationInfo: SerializationInfo;
	var
		ch: UiBinaryFormatChooser;
		it: pointer;
	begin
		ch.Init('длин рёбер реплики');
		it := nil;
		while blob.Next(it, @_ReplicaSize) do
			case pOneReplica(it)^.Op of
				op_Edge, op_BiEdge: ch.Note(pEdgeReplica(it)^.distance);
			end;
		result.lenFmt := ch.DestructiveChoose;

		ch.Init('индексов вершин реплики');
		it := nil;
		while blob.Next(it, @_ReplicaSize) do
			case OneReplica(it^).Op of
				op_Edge, op_BiEdge:
					begin
						ch.Note(pEdgeReplica(it)^.a);
						ch.Note(pEdgeReplica(it)^.b);
					end;
			end;
		result.vidFmt := ch.DestructiveChoose;
	end;

	constructor Way.Init(newMode: ModeEnum);
	begin
		inherited Init;
		_mode := newMode;
		_points := nil;
		_len := 0.0;
	end;

	constructor Way.InitFromWaypoints(wp: pWaypoints; nWpPoints: sint; wpPoints: pUint; const theLen: float);
	var
		i: sint;
	begin
		Init(way_Points);
		SetLength(_points, nWpPoints);
		for i := 0 to High(_points) do
			_points[i] := wp^.pts[wpPoints[i]].position;
		_len := theLen;
		_Update(no);
		if nWpPoints < 2 then
		begin
		{$ifdef Debug} Log('Для пути нужны минимум 2 точки!', logError); {$endif}
			ConstructorFailed;
		end;
	end;

	constructor Way.InitFromWaypoints(wp: pWaypoints; const start: Vec3;
		nWpPoints: sint; wpPoints: pUint; const wpLen: float; const finish: Vec3);
	const
		MaxAngle = pi / 2;
	var
		a, b: Vec3;
		i: sint;
	begin
		// при слишком большом угле с направлениями на старт/финиш убрать первую...
		if nWpPoints >= 1 then
		begin
			a := wp^.pts[wpPoints[0]].position;
			if nWpPoints > 1 then
				b := wp^.pts[wpPoints[1]].position
			else
				b := finish;
			if AngleUN(a - start, b - a) > MaxAngle then
			begin
				wpPoints += 1;
				dec(nWpPoints);
			end;
		end;

		// ...и последнюю
		if nWpPoints >= 1 then
		begin
			if nWpPoints > 1 then
				a := wp^.pts[wpPoints[nWpPoints - 2]].position
			else
				a := start;
			b := wp^.pts[wpPoints[nWpPoints - 1]].position;
			if AngleUN(b - a, finish - b) > MaxAngle then
				dec(nWpPoints);
		end;

		Init(way_Points);
		SetLength(_points, 2 + nWpPoints);
		_points[0] := start;
		for i := 0 to nWpPoints - 1 do
			_points[1 + i] := wp^.pts[wpPoints[i]].position;
		_points[High(_points)] := finish;
		_len := wpLen; // в этой длине не учтены start и finish, ну и пофиг, точное значение всё равно не важно
		_Update(no);
	end;

	destructor Way.Done;
	begin
		inherited Done;
	end;

	procedure Way._Update(recalculateLen: boolean);
	var
		i: sint;
	begin
		_nSmoothPoints := round(SmoothPointsK * length(_points));
		if recalculateLen then
		begin
			_len := 0.0;
			for i := 0 to High(_points) - 1 do
				_len += Distance(_points[i], _points[i + 1]);
		end;
	end;

	function Way.GetNSegments: sint;
	begin
		case _mode of
			way_Points: result := _nSmoothPoints - 1;
			else Assert(no);
		end;
	end;

	procedure Way.GetSegment(id: sint; out a, b: Vec3);
	begin
		case _mode of
			way_Points:
				begin
					Assert((id >= 0) and (id < _nSmoothPoints - 1), 'segment ID out of range: ' + ToString(id));
					a := _GetSmoothPoint(id);
					b := _GetSmoothPoint(id + 1);
				end;
			else Assert(no);
		end;
	end;

	function Way.GetSegmentB(id: sint): Vec3;
	begin
		case _mode of
			way_Points:
				begin
					Assert((id >= 0) and (id < _nSmoothPoints - 1), 'segment ID out of range: ' + ToString(id));
					result := _GetSmoothPoint(id + 1);
				end;
			else Assert(no);
		end;
	end;

	function Way.GetNPoints: sint;
	begin
		case _mode of
			way_Points: result := _nSmoothPoints;
			else Assert(no);
		end;
	end;

	function Way.GetPoint(id: sint): Vec3;
	begin
		case _mode of
			way_Points: result := _GetSmoothPoint(id);
			else Assert(no);
		end;
	end;

	function Way.GetNControlPoints: sint;
	begin
		case _mode of
			way_Points: result := length(_points);
			else Assert(no);
		end;
	end;

	procedure Way.Shift(const dist: float);
	var
		i: sint;
		v, old, new: Vec3;
		sqrdist: float;
	begin
		if IsZero(dist) then exit;
		old := _points[0];
		sqrdist := sqr(dist);
		for i := 0 to High(_points) - 2 do
		begin
			if SqrDistance(old, _points[High(_points) - 1]) <= sqrdist then break;
			v := _points[i + 1] - old;
			v.Length := v.Length + dist;
			new := old + v;
			old := _points[i + 1];
			_points[i + 1] := new;
		end;
	end;

	function Way._GetSmoothPoint(id: sint): Vec3;
	var
		k: hp_float;
		id0, id1, id2, id3: sint;
	begin
		k := length(_points) * (id / _nSmoothPoints);
		id1 := trunc(k);
		id0 := max(id1 - 1, 0);
		id2 := min(id1 + 1, High(_points));
		id3 := min(id2 + 1, High(_points));
		result := BSpline(_points[id0], _points[id1], _points[id2], _points[id3], k - id1);
	end;

	function Way._GetTarget: Vec3;
	begin
		result := _points[High(_points)];
	end;

	procedure Script_CreateWaypoints(var ss: ScriptState);
	var
		w: pWaypoints;
	begin
		if ss.Typ(1) = script_String then
			w := ResourcePool.Shared^.LoadRef(TypeOf(Waypoints), ss.ToStream(1), yes)
		else
			w := MakeRef(new(pWaypoints, Init(no)));
		ss.PushObject(w);
		Release(w);
	end;

	function Script_PathFindingMethod(var ss: ScriptState): sint;
	begin
		if ss.Top = 0 then
		begin
			ss.PushString(SearchMethodIds[defaultMethod]);
			result := 1;
		end else
		begin
			result := 0;
			defaultMethod := SearchMethod(FindStr(ss.ToString(1), SearchMethodIds, ord(defaultMethod)));
		end;
	end;

	procedure Script_Waypoint_AddPoint(var ss: ScriptState);
	begin
		ss.PushSint(pWaypoints(ss.ToSelf)^.AddPoint(ss.ToVec3(2)));
	end;

	procedure Script_Waypoint_AddEdge(var ss: ScriptState);
	begin
		pWaypoints(ss.ToSelf)^.AddEdge(ss.ToSint(2), ss.ToSint(3));
	end;

type
	pBuildEdgesCallbackParams = ^tBuildEdgesCallbackParams;
	tBuildEdgesCallbackParams = record
		ss: pScriptState;
		fnid: sint;
	end;

	function __BuildEdgeCallback(const a, b: Vec3; param: pointer): boolean;
	var
		p: pBuildEdgesCallbackParams absolute param;
		ss: pScriptState;
	begin
		ss := p^.ss;
		ss^.PushCopy(p^.fnid);
		ss^.PushVec3(a);
		ss^.PushVec3(b);
		ss^.Call(2, 1);
		result := ss^.ToBool(-1);
		ss^.Pop;
	end;

	procedure Script_Waypoint_BuildEdges(var ss: ScriptState);
	var
		w: pWaypoints;
		maxDist: float;
		maxN: uint;
		cbParam: tBuildEdgesCallbackParams;
		what: string;
		body: pRigidBody;
	begin
		w := ss.ToSelf;
		maxDist := ss.GetFloatField(2, 'maxDist', 0.1);
		maxN := ss.GetSintField(2, 'maxN');
		if ss.GetTableS(2, 'callback') then
		begin
			case ss.Typ(-1) of
				script_Function:
					begin
						cbParam.ss := @ss;
						cbParam.fnid := ss.AbsIdx(-1);
						w^.BuildEdges(maxDist, maxN, @__BuildEdgeCallback, @cbParam);
					end;
				script_String:
					begin
						what := ss.ToString(-1);
						if what = 'body-cast' then
						begin
							ss.GetTableS(2, 'body');
							body := ss.ToObject(-1, TypeOf(RigidBody));
							ss.Pop;
							w^.BuildEdges(maxDist, maxN, edge_BodyCast, body);
						end else
							ss.UnknownIdentifier(what);
					end;
			end;
			ss.Pop;
		end else
			w^.BuildEdges(maxDist, maxN, nil);
	end;

	procedure Script_Waypoint_Append(var ss: ScriptState);
	var
		name: PoolString;
		wp, ap: pWaypoints;
		tf: Transform;
		maxDist: float;
		glueFn: Waypoints.BuildEdgeCallback;
		glueFnParams: tBuildEdgesCallbackParams;
	begin
		wp := ss.ToSelf;
		name := ss.GetStringField(2, 'name', '???');
		if ss.GetTableS(2, 'wp') then
		begin
			ap := ss.ToObject(-1, TypeOf(Waypoints));
			ss.Pop;
		end else
			ap := nil;
		tf := ss.GetTransformField(2, 'tf');
		maxDist := ss.GetFloatField(2, 'maxDist');

		if ss.GetTableS(2, 'fn') then
		begin
			glueFn := @__BuildEdgeCallback;
			glueFnParams.ss := @ss;
			glueFnParams.fnid := ss.AbsIdx(-1);
			wp^.Append(name, ap, tf, maxDist, glueFn, @glueFnParams);
			ss.Pop;
		end else
			wp^.Append(name, ap, tf, maxDist, nil);
	end;

	procedure Script_Waypoint_Remove(var ss: ScriptState);
	begin
		pWaypoints(ss.ToSelf)^.Remove(ss.ToString(2));
	end;

	procedure Script_Waypoint_AddExclusion(var ss: ScriptState);
	begin
		pWaypoints(ss.ToSelf)^.AddExclusion(ss.ToString(2), Sphere.Make(ss.ToVec3(3), ss.ToFloat(4)));
	end;

	procedure Script_Waypoint_RemoveExclusion(var ss: ScriptState);
	begin
		pWaypoints(ss.ToSelf)^.RemoveExclusion(ss.ToString(2));
	end;

	procedure OpenScript(var script: ScriptState);
	const
		Stuff: array[0 .. 9] of ScriptStuffDesc =
		(
			(s: TypeDesc; p: TypeOf(Waypoints)),
			(s: 'AddPoint:0'; p: @Script_Waypoint_AddPoint),
			(s: 'AddEdge:0'; p: @Script_Waypoint_AddEdge),
			(s: 'BuildEdges:0'; p: @Script_Waypoint_BuildEdges),
			(s: 'Append:0'; p: @Script_Waypoint_Append),
			(s: 'Remove:0'; p: @Script_Waypoint_Remove),
			(s: 'AddExclusion:0'; p: @Script_Waypoint_AddExclusion),
			(s: 'RemoveExclusion:0'; p: @Script_Waypoint_RemoveExclusion),

			(s: FunctionsDesc + 'CreateWaypoints:1' + RequireEnv; p: @Script_CreateWaypoints),
			(s: 'PathFindingMethod'; p: @Script_PathFindingMethod)
		);
	begin
		script.AddStuff(Stuff);
	end;

{$ifdef use_serialization}
const
	REPLICA_OP_BITS = {$define max := ord(High(Waypoints.ReplicaOp))} {$include bits_to_store.inc};
	WP_HAS_REPLICA_BIT    = 1 shl 0;
	WP_HAS_EXCLUSIONS_BIT = 1 shl 1;

	procedure SerializeWaypoints(se: pSerializer; obj: pointer);
	var
		wp: pWaypoints absolute obj;
		flags: uint;
		info: Waypoints.SerializationInfo;
		it: pointer;
		i: sint;
		bits: BitStream;
	begin
		Assert(Assigned(wp^._replica));
		with se^ do
		begin
			flags := 0;
			if Assigned(wp^._replica) then flags := flags or WP_HAS_REPLICA_BIT;
			if length(wp^._excls) > 0 then flags := flags or WP_HAS_EXCLUSIONS_BIT;
			Serialize_ui8(stream, flags);

			if Assigned(wp^._replica) then
				with wp^._replica^ do
				begin
					info := GetSerializationInfo;
					info.Serialize(stream);
					VarInt.Write(stream, blob.Count);
					bits := BitStream.Open(stream, file_Write);
					it := nil;
					while blob.Next(it, @_ReplicaSize) do
						bits.Write(ord(Waypoints.OneReplica(it^).Op), REPLICA_OP_BITS);
					bits.Close;

					it := nil;
					while blob.Next(it, @_ReplicaSize) do
						case Waypoints.OneReplica(it^).Op of
							op_Append:
								with Waypoints.AppendReplica(it^) do
								begin
									Serialize_string(stream, name);
									SeObject(awp);
									Serialize_tf32r8(stream, tf);
								end;
							op_Point: Serialize_vec3f32(stream, Waypoints.PointReplica(it^).pos);
							op_Edge, op_BiEdge:
								with Waypoints.EdgeReplica(it^) do
								begin
									Serialize_ui(stream, a, info.vidFmt);
									Serialize_ui(stream, b, info.vidFmt);
									Serialize_ui(stream, distance, info.lenFmt);
								end;
							else Assert(no);
						end;
				end;

			if length(wp^._excls) > 0 then
			begin
				VarInt.Write(stream, length(wp^._excls));
				for i := 0 to High(wp^._excls) do
					with wp^._excls[i] do
					begin
						Serialize_string(stream, name);
						Serialize_vec3f32(stream, sphere.center);
						Serialize_f16(stream, sphere.radius);
					end;
			end;
		end;
	end;

	procedure DeserializeWaypoints(de: pDeserializer; obj: pointer);
	var
		wp: pWaypoints absolute obj;
		flags: uint;
		info: Waypoints.SerializationInfo;
		it: pointer;
		i, n: sint;
		bits: BitStream;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);

			if (flags and WP_HAS_REPLICA_BIT) <> 0 then
			begin
				wp^._replica := new(Waypoints.pReplica, Init);
				with wp^._replica^ do
				begin
					info := Waypoints.SerializationInfo.Deserialize(stream);
					n := VarInt.Read(stream);
					bits := BitStream.Open(stream, file_Read);
					for i := 0 to n - 1 do
						Add(Waypoints.ReplicaOp(bits.Read(REPLICA_OP_BITS)));
					bits.Close;

					it := nil;
					while blob.Next(it, @_ReplicaSize) do
						case Waypoints.OneReplica(it^).Op of
							op_Append:
								with Waypoints.AppendReplica(it^) do
								begin
									name := Deserialize_string(stream);
									DeObjectA(awp);
									tf := Deserialize_tf32r8(stream);
								end;
							op_Point: Waypoints.PointReplica(it^).pos := Deserialize_vec3f32(stream);
							op_Edge, op_BiEdge:
								with Waypoints.EdgeReplica(it^) do
								begin
									a := Deserialize_ui(stream, info.vidFmt);
									b := Deserialize_ui(stream, info.vidFmt);
									distance := Deserialize_ui(stream, info.lenFmt);
								end;
							else Assert(no);
						end;
				end;
			end;

			if (flags and WP_HAS_EXCLUSIONS_BIT) <> 0 then
			begin
				SetLength(wp^._excls, VarInt.Read(stream));
				for i := 0 to High(wp^._excls) do
					with wp^._excls[i] do
					begin
						name          := Deserialize_string(stream);
						sphere.center := Deserialize_vec3f32(stream);
						sphere.radius := Deserialize_f16(stream);
					end;
			end;
		end;
	end;

	procedure WaypointsDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		wp: pWaypoints absolute obj;
		i: sint;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: wp^.Init(no);
			de_After:
				begin
					if Assigned(wp^._replica) then wp^._replica^.Replicate(wp^);
					for i := 0 to High(wp^._excls) do
						wp^._ProcessExclusion(wp^._excls[i].sphere, +1);
				end;
		end;
	end;

	procedure SerializeWay(se: pSerializer; obj: pointer);
	var
		way: pWay absolute obj;
		aabb: UMath.AABB;
		i: sint;
	begin
		with se^ do
		begin
			Serialize_ui8(stream, ord(way^._mode));
			VarInt.Write(stream, length(way^._points));
			if length(way^._points) > 0 then
			begin
				aabb := AABB.Bound(way^._points);
				Serialize_vec3f32(stream, aabb.A);
				Serialize_vec3f32(stream, aabb.B);
				for i := 0 to High(way^._points) do
					Serialize_vec3N16(stream, way^._points[i], aabb.A, aabb.B);
			end;
		end;
	end;

	procedure DeserializeWay(de: pDeserializer; obj: pointer);
	var
		way: pWay absolute obj;
		aabb: UMath.AABB;
		i: sint;
	begin
		with de^ do
		begin
			way^._mode := PathFinding.Way.ModeEnum(Deserialize_ui8(stream));
			SetLength(way^._points, VarInt.Read(stream));
			if length(way^._points) > 0 then
			begin
				aabb.A := Deserialize_vec3f32(stream);
				aabb.B := Deserialize_vec3f32(stream);
				for i := 0 to High(way^._points) do
					way^._points[i] := Deserialize_vec3N16(stream, aabb.A, aabb.B);
			end;
		end;
	end;

	procedure WayDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		way: pWay absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: way^.DeseInit;
			de_After: way^._Update(yes);
		end;
	end;
{$endif}

	function LoadWaypoints(s: pStream): pObject; begin result := new(pWaypoints, Init(s)); end;

	procedure Init;
	begin
		ResourcePool.Shared^.Register(TypeOf(Waypoints), @LoadWaypoints);

	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Waypoints ～レプリカ～', TypeOf(Waypoints), nil, sizeof(Waypoints), yes,
		               @SerializeWaypoints, @DeserializeWaypoints, nil, @WaypointsDeSpecial)
		^.RegisterType('Way', TypeOf(Way), nil, sizeof(Way), yes,
		               @SerializeWay, @DeserializeWay, nil, @WayDeSpecial);
	{$endif}
	end;

initialization
	&Unit('PathFinding').Initialize(@Init);
end.
