unit GLUtils;

{$include opts.inc}

interface

uses
	USystem, Errors, Streams, UMath, Utils, UClasses, Algo,
	U_GL, GLBase, GLClasses, USkeleton,
	SpatialIndex, PathFinding, SceneGraph, Human
{$ifdef Debug}, ULog {$endif};

	procedure ValidateImageSize(const size: UintVec2);

type
	Filter = object
	type
		FlagEnum = (Filter3D, Filter3x8, Filter8Bit);
		FlagSet = set of FlagEnum;

		pInfoRec = ^InfoRec;
		InfoRec = record
			name: string;
			flags: FlagSet;
		end;
	var
		constructor Init;
		destructor Done; virtual;
		procedure Apply(var im: TextureImage);
		procedure DestructiveApply(var im: TextureImage);
		procedure Apply(const size: UintSize3; format: GLImageFormat; src, dst: pointer);
		procedure DestructiveApply(const size: UintSize3; format: GLImageFormat; src, dst: pointer);
	protected
		procedure Impl(const size: UintVec2; format: GLImageFormat; src, dst: pointer); virtual;
		procedure Impl3D(const size: UintVec3; format: GLImageFormat; src, dst: pointer); virtual;
		function GetInfo: pInfoRec; virtual; abstract;
	end;

	Filters = object
	type
		// TODO: свёрточные фильтры через ДПФ? (должно быть намного быстрее асимптотически)
		SeparableConvolution = object(Filter)
		type
			GetRowProc = procedure(x, y: uint; param: pointer; out n: uint; out row: pFloat);
		var
			getRow: GetRowProc;
			param: pointer;
			function Create(newGetRow: GetRowProc; newParam: pointer): SeparableConvolution; static;
		protected
			procedure Impl(const size: UintVec2; format: GLImageFormat; src, dst: pointer); virtual;
			function GetInfo: pInfoRec; virtual;
		private
			procedure Apply(vert: boolean; const size: UintVec2; format: GLImageFormat; src, dst: pointer);
		const
			Info: InfoRec = (name: 'SeparableConvolution'; flags: [Filter8Bit]);
		end;

	type
		Soapify = object(Filter)
		type
			GetRadiusProc = function(x, y: uint; param: pointer): float;
			pParamsRec = ^ParamsRec;
			ParamsRec = record
				getRadius: GetRadiusProc;
				param: pointer;
				rows: array of array of float; // ядра свёрток по радиусам, начиная с 2
			end;
		var
			params: pParamsRec;
			function Create(newGetRadius: GetRadiusProc; newParam: pointer): Soapify; static;
			destructor Done; virtual;
		protected
			procedure Impl(const size: UintVec2; format: GLImageFormat; src, dst: pointer); virtual;
			function GetInfo: pInfoRec; virtual;
		const
			Info: InfoRec = (name: 'Soapify'; flags: [Filter8Bit]);
		private
			sc: SeparableConvolution;
		end;
	end;

	procedure VisualizeKD(objs: boolean; n: pSceneKdTree; b: pGLBatch);
	procedure VisualizeWaypointsKD(n: Waypoints.pPointsKdTree; b: pGLBatch);
	procedure VisualizeSkeleton(sk: pSkeletonNode; b: pGLBatch);
	procedure VisualizeWaypoints(w: pWaypoints; b: pGLBatch; edges: boolean);
	procedure VisualizeWay(way: pWay; b: pGLBatch);
{$ifdef Debug} procedure VisualizeHeapDump(const stream: string); {$endif}

type
	Aspect2Method = (asp2_x1, asp2_y1, asp2_min1, asp2_max1);

	pAspectPair = ^AspectPair;
	AspectPair = object
		aspect, invAspect: float;
		function Empty: AspectPair; static;
		function Make(const size: Vec2): AspectPair; static;
		function Make(size: Vec2; rotated: boolean; const fix: float = 1.0): AspectPair; static;
		function Aspect2(method: Aspect2Method; const mul: float): Vec2;
		function Aspect2Item(method: Aspect2Method; dim: uint; const mul: float): float;
		procedure Fix(const by: float);
		function ReverseCombined(const ap: AspectPair): AspectPair;
	const
		Identity: AspectPair = (aspect: 1.0; invAspect: 1.0);
	end;

	pAtlas = ^Atlas;
	Atlas = object(&Object)
	public type
		pAtlasItem = ^AtlasItem;
		AtlasItem = record
			name: PoolString;
			rect: Rect;
			rotated: boolean;
			aia: AspectPair;
		end;

	private type
		{$define classname := ItemsSet} {$define key_type := AtlasItem} {$define inline_key := PoolString} {$define delayed_key} {$include hash.h.inc}
	private var
		imgAspect: float;
		items: ItemsSet;
		linked: pTexture;
		procedure Deserialize(s: pStream);
	public var
		constructor Init(const imgAspect: float);
		constructor Init(s: pStream);
		destructor Done; virtual;
		procedure Add(const name: PoolString; const rect: Rect; rotated: boolean);
		function Find(const name: PoolString): pAtlasItem;
		function GetTex(const name: PoolString; out texA, texB, texC, texD: Vec2): boolean;
		function Aspect2(const name: PoolString; method: Aspect2Method; const mul: float): Vec2;
		function Aspect2Item(const name: PoolString; method: Aspect2Method; dim: uint; const mul: float): float;
		procedure Clear;
		property LinkedImage: pTexture read linked;
	public const
		Signature = 'ATLAS';
		ROTATED_BIT = 1 shl 0;
	end;

	pAtlasPacker = ^AtlasPacker;
	AtlasPacker = object
	public type
		InsertDesc = object
			pos, size: UintVec2;
			rotated: boolean;
			user: pointer;
			function Make(const newSize: UintVec2; newUser: pointer = nil): InsertDesc; static;
			function ToRect(var apk: AtlasPacker): Rect;
		end;
		BatchInsertDesc = array of InsertDesc;

		GrowFunc = function(const size: UintVec2; param: pointer): boolean;
		GetAllFunc = function(param: pointer): BatchInsertDesc;
		ApplyFunc = procedure(const desc: InsertDesc; param: pointer);
		GrowStrategy = (grow_Fast, grow_Precise);

	public const
		SizeLimit = 4096;

	private type
		SkylineNode = record
			x, y, sizeX: uint;
		end;

	private var
		sz: UintVec2;
		skyline: array of SkylineNode;
		_usedArea, _border, _sizeAlignment: uint;
		_growStrategy: GrowStrategy;
		procedure Reset(const newSize: UintVec2);
		function FindPositionForNewNodeBottomLeft(const size: UintVec2; out rect: UintRect; out bestSizeY, bestSizeX, bestIndex: uint): boolean;
		function RectangleFits(skylineId: uint; const size: UintVec2; out y: uint): boolean;
		procedure AddSkylineLevel(index: uint; const rect: UintRect);
		procedure MergeSkylines;
		procedure InsertOne(var item: InsertDesc; skyline: uint; const node: UintRect);
		procedure Revert(var item: InsertDesc);
		function GrowAndReinsert(var items: array of InsertDesc; grow: GrowFunc; param: pointer): boolean;
	public
		procedure Init(const newSize: UintVec2; newGrowStrategy: GrowStrategy);
		procedure Done;
		function Insert(var items: array of InsertDesc): boolean;
		function Insert(const aitems: array of InsertDesc; grow: GrowFunc; getall: GetAllFunc; apply: ApplyFunc; param: pointer): boolean;

		property FullSize: UintVec2 read sz;
		property UsedArea: uint read _usedArea;
		property Border: uint read _border write _border;
		property SizeAlignment: uint read _sizeAlignment write _sizeAlignment;
	end;

type
	AtlasAnimationFlag = (anim_Looped, anim_InPool, anim_NotifyLocaleChanges);
	AtlasAnimationFlags = set of AtlasAnimationFlag;

	pAtlasAnimation = ^AtlasAnimation;
	AtlasAnimation = object(&Object)
	private type
		FrameMode = (frame_Rect, frame_Atlas);
		FrameRec = record
			duration, startTime: float;
		case mode: FrameMode of
			frame_Rect: (rect: Rect);
			frame_Atlas: (aitem: Atlas.pAtlasItem; srcName: pPoolString);
		end;

		{$define classname:=tPool} {$define key_type:=pAtlasAnimation} {$define null_value:=nil} {$include hash.h.inc}
	private var
		_pool: tPool; static;
		_nInstances: sint; static;
		procedure _InitInstance; static;
		procedure _DoneInstance; static;
		function _Hash(aa: pAtlasAnimation): Hash.Value; static;
		function _Equals(a, b: pAtlasAnimation): boolean; static;
	private
		_atlas: pAtlas;
		_frames: array of FrameRec;
		_length: float;
		_flags: AtlasAnimationFlags;
		procedure _FinalizeFrame(var frame: FrameRec);
		procedure _Recalculate(start: sint = 0);
		procedure _Append(const newFrames: array of FrameRec);
		procedure _EnableLocaleNotifications;
		procedure _DisableLocaleNotifications;
		procedure _UpdateLocale(var loc: Locale; fix: boolean);
		procedure _UpdateAspects;
	public
		asp: AspectPair;
		constructor Init(newAtlas: pAtlas; const newFlags: AtlasAnimationFlags);
		destructor Done; virtual;
		procedure Append(const newRect: Rect; const newDuration: float);
		procedure Append(const baseRect: Rect; const theDx, theDy: float; nx, n: sint; const newDuration: float);
		procedure Append(const newItem: string; const newDuration: float);
		function Merge(anim: pAtlasAnimation): pAtlasAnimation; static;
		function GetRect(const time: float; out rect: Rect; out rotated: boolean): boolean;

		property Atlas: pAtlas read _atlas;
		property Flags: AtlasAnimationFlags read _flags;
	end;

type
	PosABConverter = object
		a: Vec3;
		idist: Vec3;
		function Prepare(const bnd: Bounding): PosABConverter; static;
		function AB(const v: Vec3): Vec3;
	end;

const
	Aspect2Ids: array[Aspect2Method] of string = ('x1', 'y1', 'min1', 'max1');

implementation

uses
	MMSystem, PNG, BMP
{$ifdef use_serialization}, Serialization {$endif};

{$define classname:=AtlasAnimation.tPool} {$define hash_func:=AtlasAnimation._Hash} {$define inline_eq := AtlasAnimation._Equals(_1, _2)}
{$include hash.pp.inc}

{$define classname := Atlas.ItemsSet} {$define get_key := _1.name} {$define inline_hash := _1.Hash} {$define inline_eq := _1.name = _2}
{$define finalize_key := }
{$include hash.pp.inc}

	procedure ValidateImageSize(const size: UintVec2);
	const
		MaxPixels = (512 * 1024 * 1024) {512 Mb} div sizeof(Vec3u8) {RGB};
	begin
		if (size.x < 1) or (size.x > MaxPixels) or (size.y < 1) or (size.y > MaxPixels div size.x) then
			raise Error('Изображение повреждено, либо размер слишком велик.');
	end;

	constructor Filter.Init;
	begin
	end;

	destructor Filter.Done;
	begin
	end;

	procedure Filter.Apply(var im: TextureImage);
	begin
		Apply(im.Size, im.format, im.FirstLevel, im.FirstLevel);
	end;

	procedure Filter.DestructiveApply(var im: TextureImage);
	begin
		DestructiveApply(im.Size, im.format, im.FirstLevel, im.FirstLevel);
	end;

	procedure Filter.Apply(const size: UintSize3; format: GLImageFormat; src, dst: pointer);
	var
		info: pInfoRec;
		i: uint;
		srcp, dstp: pointer;
		planesz: size_t;
	begin
		info := GetInfo;
		if ((Filter8bit in info^.flags) and (GLImageFormatsInfo[format].pixelSize = GLImageFormatsInfo[format].nChannels * sizeof(uint8)))
			or ((Filter3x8 in info^.flags) and (GLImageFormatsInfo[format].nChannels = 3) and (GLImageFormatsInfo[format].pixelSize = sizeof(Vec3u8)))
		then
			// формат поддерживается
		else
			raise Error('Фильтр {0} не поддерживает формат {1}.', info^.name, GLImageFormatIds[format]);

		if Filter3D in info^.flags then
			Impl3D(size, format, src, dst)
		else
		begin
			srcp := src;
			dstp := dst;
			planesz := size.xy.Product * GLImageFormatsInfo[format].pixelSize;
			for i := 1 to size.z do
			begin
				Impl(size.xy, format, srcp, dstp);
				srcp += planesz; dstp += planesz;
			end;
		end;
	end;

	procedure Filter.DestructiveApply(const size: UintSize3; format: GLImageFormat; src, dst: pointer);
	begin
		try
			Apply(size, format, src, dst);
		finally
			Done;
		end;
	end;

	procedure Filter.Impl(const size: UintVec2; format: GLImageFormat; src, dst: pointer);
	begin
		unused_args size _ format _ src _ dst end_list
		raise Unimplemented(GetInfo^.name + '.Impl');
	end;

	procedure Filter.Impl3D(const size: UintVec3; format: GLImageFormat; src, dst: pointer);
	begin
		unused_args size _ format _ src _ dst end_list
		raise Unimplemented(GetInfo^.name + '.Impl3D');
	end;

	function Filters.SeparableConvolution.Create(newGetRow: GetRowProc; newParam: pointer): SeparableConvolution;
	begin
		result.Init;
		result.getRow     := newGetRow;
		result.param      := newParam;
	end;

	procedure Filters.SeparableConvolution.Impl(const size: UintVec2; format: GLImageFormat; src, dst: pointer);
	var
		tmp: pointer;
	begin
		tmp := GetMem(size.Product * GLImageFormatsInfo[format].pixelSize);
		Apply(no, size, format, src, tmp);
		Apply(yes, size, format, tmp, dst);
		FreeMem(tmp);
	end;

	function Filters.SeparableConvolution.GetInfo: pInfoRec; begin result := @Info; end;

	procedure Filters.SeparableConvolution.Apply(vert: boolean; const size: UintVec2; format: GLImageFormat; src, dst: pointer);
	label next;
	var
		x, y, irow, nrow, ic, components: uint;
		invWeightSum: float;
		sum: array[0 .. MaxColorChannels - 1] of float;
		{центр} srccp, dstcp, {вправо} srcrp, {влево} srclp: pUint8;
		step: size_t;
		row: pFloat;
	begin
		components := GLImageFormatsInfo[format].nChannels;
		srccp := src;
		dstcp := dst;
		if vert then step := components * size.x else step := components;

		y := 0;
		while y < size.y do
		begin
			x := 0;
			while x < size.x do
			begin
				getRow(x, y, param, nrow, row);
				if nrow <= 1 then begin memcpy(srccp, dstcp, components * sizeof(srccp^)); goto next; end;
				for ic := 0 to components - 1 do sum[ic] := row[0] * srccp[ic];
				srcrp := srccp;
				srclp := srccp;
				invWeightSum := row[0];

				for irow := 1 to nrow - 1 do
				begin
					if (not vert and ({x - irow >= 0} x >= irow)) or (vert and ({y - irow >= 0} y >= irow)) then
					begin
						srclp -= step;
						invWeightSum += row[irow];
						for ic := 0 to components - 1 do sum[ic] += uint(srclp[ic]) * row[irow];
					end;
					if (not vert and (x + irow < size.x)) or (vert and (y + irow < size.y)) then
					begin
						srcrp += step;
						invWeightSum += row[irow];
						for ic := 0 to components - 1 do sum[ic] += uint(srcrp[ic]) * row[irow];
					end;
				end;

				Assert(invWeightSum > 0);
				invWeightSum := 1.0 / invWeightSum;
				for ic := 0 to components - 1 do dstcp[ic] := round(min(sum[ic] * invWeightSum, High(srccp^)));
			next:
				inc(x);
				srccp += components;
				dstcp += components;
			end;
			inc(y);
		end;
	end;

	procedure GetSoapifyRow(x, y: uint; param: pointer; out n: uint; out row: pFloat);
	var
		p: Filters.Soapify.pParamsRec absolute param;
		radius: float;
		i, irow: uint;
	begin
		radius := p^.getRadius(x, y, p^.param); Assert(radius >= 0);
		if radius < 0.5/High(uint8) then
		begin
			n := 0;
			row := nil;
		end else
		begin
			n := iceil(1 + radius); Assert(n >= 2);
			irow := n - 2;
			if irow >= uint(length(p^.rows)) then SetLength(p^.rows, irow + 1);
			if length(p^.rows[irow]) = 0 then
			begin
				SetLength(p^.rows[irow], n);
				i := 0;
				while i < n do
				begin
					p^.rows[irow][i] := exp(-4.0 * sqr(i / n));
					inc(i);
				end;
			end;

			Assert(n = uint(length(p^.rows[irow])));
			row := pFloat(p^.rows[irow]);
		end;
	end;

	function Filters.Soapify.Create(newGetRadius: GetRadiusProc; newParam: pointer): Soapify; static;
	begin
		result.Init;
		new(result.params);
		result.params^.getRadius := newGetRadius;
		result.params^.param     := newParam;
		result.sc                := SeparableConvolution.Create(@GetSoapifyRow, result.params);
	end;

	destructor Filters.Soapify.Done;
	begin
		sc.Done;
		dispose(params);
		inherited Done;
	end;

	procedure Filters.Soapify.Impl(const size: UintVec2; format: GLImageFormat; src, dst: pointer);
	begin
		sc.Impl(size, format, src, dst);
	end;

	function Filters.Soapify.GetInfo: pInfoRec; begin result := @Info; end;

	procedure VisualizeLineBox(const aabb: AABB; vp: pVec3f; var inds: MeshIndices; var vertexOfs, indexOfs: sint; shift: float);
	const
		LineBoxInds: array[0 .. 23] of sint = (0, 1, 1, 5, 5, 3, 3, 0, 0, 2, 3, 6, 5, 7, 1, 4, 2, 6, 6, 7, 7, 4, 4, 2);
	var
		i: sint;
	begin
		vp[vertexOfs + 0] := aabb.A;
		vp[vertexOfs + 1] := Vec3.Make(aabb.B.x, aabb.A.y, aabb.A.z);
		vp[vertexOfs + 2] := Vec3.Make(aabb.A.x, aabb.B.y, aabb.A.z);
		vp[vertexOfs + 3] := Vec3.Make(aabb.A.x, aabb.A.y, aabb.B.z);
		vp[vertexOfs + 4] := Vec3.Make(aabb.B.x, aabb.B.y, aabb.A.z);
		vp[vertexOfs + 5] := Vec3.Make(aabb.B.x, aabb.A.y, aabb.B.z);
		vp[vertexOfs + 6] := Vec3.Make(aabb.A.x, aabb.B.y, aabb.B.z);
		vp[vertexOfs + 7] := aabb.B;
		if shift <> 0.0 then
			for i := vertexOfs + 0 to vertexOfs + 7 do
				vp[i] := FromGL(vp[i]) + (FromGL(vp[i]) - aabb.Center).Normalized * shift;
		for i := 0 to High(LineBoxInds) do
			inds[indexOfs + i] := vertexOfs + LineBoxInds[i];
		inc(vertexOfs, 8);
		inc(indexOfs, length(LineBoxInds));
	end;

	procedure VisualizeKD(objs: boolean; n: pSceneKdTree; b: pGLBatch);
	var
		vp: pNativeGLValue;
		i, si, sv: sint;
		aabb: array of UMath.AABB;
		shift: float;
		inds: pMeshIndices;
	begin
		if (not Assigned(n)) or (not Assigned(b)) then exit;
		if objs then
		begin
			aabb := n^.GetLeafAABBs;
			shift := 0.0;
		end else
		begin
			aabb := n^.GetNodeAABBs;
			shift := 0.01;
		end;
		sv := 0;
		si := 0;

		inds := @b^.batch^.inds;
		b^.batch^.VerticesCount := sv + 8 * length(aabb);
		inds^.Count := si + 24 * length(aabb);
		vp := b^.batch^.FindVA('pos');

		for i := 0 to High(aabb) do
			VisualizeLineBox(aabb[i], vp^.RawVec3, inds^, sv, si, shift);

		b^.batch^.VerticesCount := sv;
		inds^.Count := si;
		b^.mesh^.Changed;
	end;

	procedure VisualizeWaypointsKD(n: Waypoints.pPointsKdTree; b: pGLBatch);
	var
		vp: pNativeGLValue;
		i, si, sv: sint;
		aabb: array of UMath.AABB;
		inds: pMeshIndices;
	begin
		if (not Assigned(n)) or (not Assigned(b)) then exit;
		aabb := n^.GetNodeAABBs;
		sv := 0;
		si := 0;

		inds := @b^.batch^.inds;
		b^.batch^.VerticesCount := sv + 8 * length(aabb);
		inds^.Count := si + 24 * length(aabb);
		vp := b^.batch^.FindVA('pos');

		for i := 0 to High(aabb) do
			VisualizeLineBox(aabb[i], vp^.RawVec3, inds^, sv, si, 0.0);

		b^.batch^.VerticesCount := sv;
		inds^.Count := si;
		b^.mesh^.Changed;
	end;

	procedure VisualizeSkeleton(sk: pSkeletonNode; b: pGLBatch);
	var
		vp: pNativeGLValue;
		i, sv, si: sint;
		batch: pBatch;
		inds: pMeshIndices;
		tf: Transform;
	begin
		if (not Assigned(sk)) or (not Assigned(b)) then exit;
		batch := b^.batch;
		sv := 0;
		si := 0;

		inds := @batch^.inds;
		batch^.VerticesCount := sv + 2 * length(sk^.bones);
		inds^.Count := si + 2 * length(sk^.bones);
		vp := batch^.FindVA('pos');
		for i := 0 to High(sk^.bones) do
		begin
			tf := sk^.bones[i].WorldTransform(sk^);
			vp^.RawVec3[sv + 2*i] := tf.tr;
			vp^.RawVec3[sv + 2*i + 1] := tf.tr + tf.rot * BaseBoneVector * sk^.EstimateBoneLength(i);
			inds^[si + 2*i] := sv + 2*i;
			inds^[si + 2*i + 1] := sv + 2*i + 1;
		end;
		b^.mesh^.Changed;
	end;

	procedure VisualizeWaypoints(w: pWaypoints; b: pGLBatch; edges: boolean);
	const
		OctaR = 0.05;
		OctaPts: array[0 .. 5] of Vec3 =
		(
			(data: (0.0, OctaR, 0.0)),
			(data: (0.0, -OctaR, 0.0)),
			(data: (OctaR, 0.0, 0.0)),
			(data: (-OctaR, 0.0, 0.0)),
			(data: (0.0, 0.0, OctaR)),
			(data: (0.0, 0.0, -OctaR))
		);
		OctaInds: array[0 .. 23] of sint = (0, 2, 0, 3, 0, 4, 0, 5, 1, 2, 1, 3, 1, 4, 1, 5, 2, 4, 4, 3, 3, 5, 5, 2);
	var
		vp: pNativeGLValue;
		batch: pBatch;
		i, j, sv, si, curv, curi, n: sint;
		inds: pMeshIndices;
	begin
		if (not Assigned(w)) or (not Assigned(b)) then exit;
		batch := b^.batch;
		inds := @batch^.inds;
		sv := 0;
		si := 0;
		vp := batch^.FindVA('pos');

		if edges then
		begin
			n := w^.GetEdgesCount;
			batch^.VerticesCount := sv + 2 * n;
			inds^.Count := si + 2 * n;
			curv := 0;
			curi := 0;
			for i := 0 to High(w^.pts) do
				for j := 0 to w^.pts[i].nEdgesTo - 1 do
				begin
					if (uint(i) > w^.pts[i].edgesTo[j].v) and (w^.HasEdge(w^.pts[i].edgesTo[j].v, i)) then continue;
					if w^.pts[i].edgesTo[j].disableCounter > 0 then continue;
					vp^.RawVec3[sv + curv] := w^.pts[i].position;
					vp^.RawVec3[sv + curv + 1] := w^.pts[w^.pts[i].edgesTo[j].v].position;
					inds^[si + curi] := sv + curv;
					inds^[si + curi + 1] := sv + curv + 1;
					inc(curv, 2);
					inc(curi, 2);
				end;
			batch^.VerticesCount := sv + curv;
			inds^.Count := si + curi;
		end else
		begin
			n := length(w^.pts);
			batch^.VerticesCount := sv + length(OctaPts) * n;
			inds^.Count := si + length(OctaInds) * n;
			curv := 0;
			curi := 0;
			for i := 0 to High(w^.pts) do
			begin
				if (w^.pts[i].nEdgesTo = 0) or (w^.pts[i].edgesTo[0].disableCounter = 0) then
				begin
					for j := 0 to High(OctaPts) do
						vp^.RawVec3[sv + curv + j] := w^.pts[i].position + OctaPts[j];
					for j := 0 to High(OctaInds) do
						inds^[si + curi + j] := sv + curv + OctaInds[j];
					inc(curv, length(OctaPts));
					inc(curi, length(OctaInds));
				end else
				begin
				end;
			end;
			batch^.VerticesCount := sv + curv;
			inds^.Count := si + curi;
		end;
		b^.mesh^.Changed;
	end;

	procedure VisualizeWay(way: pWay; b: pGLBatch);
	var
		vp: pNativeGLValue;
		i, sv, si, n: sint;
		batch: pBatch;
		inds: pMeshIndices;
	begin
		if (not Assigned(way)) or (not Assigned(b)) then exit;
		batch := b^.batch;
		sv := 0;
		si := 0;
		vp := batch^.FindVA('pos');
		n := way^.GetNPoints;
		batch^.VerticesCount := sv + n;
		inds := @batch^.inds;
		inds^.Count := si + 2 * max(n - 1, 0);
		for i := 0 to n - 1 do
		begin
			vp^.RawVec3[sv + i] := way^.GetPoint(i);
			if i < n - 1 then
			begin
				inds^[si + 2*i] := sv + i;
				inds^[si + 2*i + 1] := sv + i + 1;
			end;
		end;
		b^.mesh^.Changed;
	end;

{$ifdef Debug}
	procedure VisualizeHeapDump(const stream: string);
	label _finally_;
	const
		FirstColor: Vec3u8 = (255, 0, 0);
		NextColor: Vec3u8 = (0, 128, 255);
	var
		dump: HeapDump;
		image: TextureImage;
		b, i, startpx, npx: sint;
		maxAddress: PtrUint;
		pixel: pVec3u8;
	begin
		dump := HeapDump.Get;
		if dump.nBlocks = 0 then goto _finally_;
		Log('Блоков в куче: ' + ToString(dump.nBlocks));

		maxAddress := dump.blocks[0].address;
		for b := 1 to dump.nBlocks - 1 do
			maxAddress := max(maxAddress, dump.blocks[b].address);

		image.Init(UintVec2.Make(512, 384), GLformat_RGB);
		image.Fill(Vec4.Zero, 0);
		for b := 0 to dump.nBlocks - 1 do
		begin
			startpx := min(round((dump.blocks[b].address / maxAddress) * image.size.xy.product), image.size.xy.product - 1);
			pixel := pVec3u8(image.FirstLevel) + startpx;
			npx := min(1 + round(image.size.xy.product* (dump.blocks[b].size / maxAddress)), image.size.xy.product - startpx);
			pixel^ := FirstColor;
			for i := 1 to npx - 1 do
				pixel[i] := NextColor;
		end;
		image.Save(stream);

	_finally_: dump.Close;
	end;
{$endif}

	function AspectPair.Empty: AspectPair;
	begin
		result.aspect := 1.0;
		result.invAspect := 1.0;
	end;

	function AspectPair.Make(const size: Vec2): AspectPair;
	begin
		if size.y <> 0.0 then result.aspect := size.aspect else result.aspect := 1.0;
		if size.x <> 0.0 then result.invAspect := size.yx.aspect else result.invAspect := 1.0;
	end;

	function AspectPair.Make(size: Vec2; rotated: boolean; const fix: float = 1.0): AspectPair;
	begin
		size.data[0] *= fix;
		if rotated then size := size.yx;
		result := Make(size);
	end;

	function AspectPair.Aspect2(method: Aspect2Method; const mul: float): Vec2;
	begin
		if (method = asp2_x1) or ((method = asp2_min1) and (aspect < 1.0)) or ((method = asp2_max1) and (aspect > 1.0)) then
			result := Vec2.Make(mul, mul * invAspect)
		else
			result := Vec2.Make(mul * aspect, mul);
	end;

	function AspectPair.Aspect2Item(method: Aspect2Method; dim: uint; const mul: float): float;
	begin
		Assert(dim < 2);
		if (method = asp2_x1) or ((method = asp2_min1) and (aspect < 1.0)) or ((method = asp2_max1) and (aspect > 1.0)) then
			if dim = 0 then result := mul else result := mul * invAspect
		else
			if dim = 0 then result := mul * aspect else result := mul;
	end;

	procedure AspectPair.Fix(const by: float);
	begin
		aspect *= by;
		invAspect /= by;
	end;

	function AspectPair.ReverseCombined(const ap: AspectPair): AspectPair;
	begin
		result.aspect := aspect * ap.invAspect;
		result.invAspect := invAspect * ap.aspect;
	end;

	constructor Atlas.Init(const imgAspect: float);
	begin
		inherited Init;
		self.imgAspect := imgAspect;
		items.Init;
		linked := nil;
	end;

	constructor Atlas.Init(s: pStream);
	begin
		Init(1.0);
	{$ifdef Debug} LogR('Загрузка атласа из ' + StreamPath.Log(s^.path) + '... '); {$endif}
		try
			Deserialize(MakeRef(s));
		{$ifdef Debug} Log('Атлас ' + StreamPath.Log(s^.path) + ' загружен, записей: ' + ToString(items.Count), logOK); {$endif}
		finally
			Release(s);
		end;
	end;

	destructor Atlas.Done;
	begin
		Release(linked);
		items.Done;
		inherited Done;
	end;

	procedure Atlas.Deserialize(s: pStream);
	var
		i, n: sint;
		rect: UMath.Rect;
		flags: uint;
		name: string;
	begin
		Deserialize_signature(s, Signature, no);

		name := Deserialize_string(s);
		if name <> '' then linked := ResourcePool.Shared^.LoadRef(TypeOf(tTexture), StreamPath.Resolve(name, s^.path));
		imgAspect := Deserialize_f32(s);
		n := VarInt.Read(s);
		for i := 1 to n do
		begin
			name  := Deserialize_string(s);
			flags := Deserialize_ui8(s);
			rect  := UMath.Rect.Make(Deserialize_vec4N16(s, Vec4.Zero, Vec4.Ones));
			Add(name, rect, flags and ROTATED_BIT <> 0);
		end;
	end;

	procedure Atlas.Add(const name: PoolString; const rect: Rect; rotated: boolean);
	var
		item: pAtlasItem;
	begin
		item := items.Add(name);
		item^.name := name;
		item^.rect := rect;
		item^.rotated := rotated;
		item^.aia := AspectPair.Make(rect.Size);
	end;

	function Atlas.Find(const name: PoolString): pAtlasItem;
	begin
		result := items.Find(name);
	{$ifdef Debug} if not Assigned(result) then Log('"' + name + '" не найден в атласе', logError); {$endif}
	end;

	function Atlas.GetTex(const name: PoolString; out texA, texB, texC, texD: Vec2): boolean;
	var
		item: pAtlasItem;
		rect: pRect;
	begin
		item := Find(name);
		result := Assigned(item);
		if not result then exit;

		rect := @item^.rect;
		if item^.rotated then
		begin
			texA := Vec2.Make(rect^.B.x, rect^.A.y);
			texB := Vec2.Make(rect^.A.x, rect^.A.y);
			texC := Vec2.Make(rect^.A.x, rect^.B.y);
			texD := Vec2.Make(rect^.B.x, rect^.B.y);
		end else
		begin
			texA := Vec2.Make(rect^.A.x, rect^.A.y);
			texB := Vec2.Make(rect^.A.x, rect^.B.y);
			texC := Vec2.Make(rect^.B.x, rect^.B.y);
			texD := Vec2.Make(rect^.B.x, rect^.A.y);
		end;
	end;

	function Atlas.Aspect2(const name: PoolString; method: Aspect2Method; const mul: float): Vec2;
	var
		item: pAtlasItem;
	begin
		item := Find(mm.locale.Localize(name));
		if Assigned(item) then result := item^.aia.Aspect2(method, mul) else result := Vec2.Make(mul, mul);
	end;

	function Atlas.Aspect2Item(const name: PoolString; method: Aspect2Method; dim: uint; const mul: float): float;
	var
		item: pAtlasItem;
	begin
		item := Find(mm.locale.Localize(name));
		if Assigned(item) then result := item^.aia.Aspect2Item(method, dim, mul) else result := mul;
	end;

	procedure Atlas.Clear;
	begin
		items.Clear;
	end;

	function AtlasPacker.InsertDesc.Make(const newSize: UintVec2; newUser: pointer = nil): InsertDesc;
	begin
		result.size    := newSize;
		result.rotated := no;
		result.user    := newUser;
	end;

	function AtlasPacker.InsertDesc.ToRect(var apk: AtlasPacker): Rect;
	begin
		result := Rect.Make(pos / apk.sz, (pos + size) / apk.sz);
	end;

	procedure AtlasPacker.Reset(const newSize: UintVec2);
	begin
		sz := max(newSize, UintVec2.Ones);
		SetLength(skyline, 1);
		skyline[0].x := 0;
		skyline[0].y := 0;
		skyline[0].sizeX := sz.x;
		_usedArea := 0;
	end;

	procedure AtlasPacker.Init(const newSize: UintVec2; newGrowStrategy: GrowStrategy);
	begin
		_border := 0;
		_sizeAlignment := 4;
		_growStrategy := newGrowStrategy;
		Reset(newSize);
	end;

	procedure AtlasPacker.Done;
	begin
	end;

	procedure AtlasPacker.InsertOne(var item: InsertDesc; skyline: uint; const node: UintRect);
	begin
		AddSkylineLevel(skyline, node);
		item.rotated := item.size.x + 2 * _border <> node.size.x;
		if item.rotated then item.size := item.size.YX;
		_usedArea += node.Square;
		item.pos := node.pos + UintVec2.Make(_border);
	end;

	procedure AtlasPacker.Revert(var item: InsertDesc);
	begin
		if item.rotated then item.size := item.size.YX;
	end;

	function AtlasPacker.Insert(var items: array of InsertDesc): boolean;
	label _finally_;
	var
		used: array of boolean;
		rest, i: sint;
		bestNode, curNode: UintRect;
		bestScoreA, bestScoreB, curScoreA, curScoreB: uint;
		bestSkylineId, curSkylineId, bestId: uint;
	begin
		result := no;
		rest := length(items);
		SetLength(used, length(items));
		for i := 0 to High(used) do used[i] := no;

		while rest > 0 do
		begin
			bestScoreA := High(bestScoreA);
			bestScoreB := High(bestScoreB);
			bestSkylineId := High(bestSkylineId);
			bestId := High(bestId);

			for i := 0 to High(items) do
			begin
				if used[i] then continue;
				if FindPositionForNewNodeBottomLeft(items[i].size + UintVec2.Make(2 * _border), curNode, curScoreA, curScoreB, curSkylineId) then
					if (curScoreA < bestScoreA) or ((curScoreA = bestScoreA) and (curScoreB < bestScoreB)) then
					begin
						bestNode := curNode;
						bestScoreA := curScoreA;
						bestScoreB := curScoreB;
						bestSkylineId := curSkylineId;
						bestId := i;
					end;
			end;
			if bestId = High(bestId) then
			begin
			{$ifdef Debug} Log('Для {0} не нашлось места в атласе', lang_amount(rest, '{N} картин{ки/ок/ок}'), logWarning); {$endif}
				goto _finally_;
			end;

			InsertOne(items[bestId], bestSkylineId, bestNode);
			used[bestId] := yes;
			dec(rest);
		end;

		result := yes;
	_finally_:
		if not result then
			for i := 0 to High(used) do
				if used[i] then Revert(items[i]);
	end;

	function AtlasPacker.Insert(const aitems: array of InsertDesc; grow: GrowFunc; getall: GetAllFunc; apply: ApplyFunc; param: pointer): boolean;
	var
		all: array of InsertDesc;
		i, start: sint;
	begin
		SetLength(all, length(aitems));
		for i := 0 to High(all) do
			all[i] := aitems[i];

		result := Insert(all);
		if not result then
		begin
			if Assigned(getall) then all := getall(param) else all := nil;
			start := length(all);
			SetLength(all, start + length(aitems));
			for i := start to High(all) do
				all[i] := aitems[i - start];
			result := GrowAndReinsert(all, grow, param);
		end;

		if result and Assigned(apply) then
			for i := 0 to High(all) do
				apply(all[i], param);
	end;

	function AtlasPacker.GrowAndReinsert(var items: array of InsertDesc; grow: GrowFunc; param: pointer): boolean;
		function align(const v: UintVec2; alignment: size_t): UintVec2;
		begin
			result := UintVec2.Make(USystem.align(v.x, alignment), USystem.align(v.y, alignment));
		end;
	var
		growing, ok, xnow: boolean;
		i: sint;
		ns, smin, smax: UintVec2;
	begin
		result := no;
		case _growStrategy of
			grow_Precise:
				begin
					smin := align(sz + UintVec2.Ones, _sizeAlignment);
					smax := smin;
					ns := smin;
					growing := yes;
					xnow := no;
				end;
		end;

		repeat
			case _growStrategy of
				grow_Fast:
					if sz.x <= sz.y then
						ns := UintVec2.Make((3 * sz.x) div 2, sz.y)
					else
						ns := UintVec2.Make(sz.x, (3 * sz.y) div 2);
				grow_Precise:
					if growing then
					begin
						xnow := ns.y >= ns.x;
						if xnow then ns.data[0] *= 2 else ns.data[1] *= 2;
					end else
						ns := align((smin + smax) div 2 + UintVec2.Ones - UintVec2.Make(_sizeAlignment), _sizeAlignment);
				else Assert(no);
			end;
			result := ns.FitsClosed(UintVec2.Make(SizeLimit, SizeLimit));
			if not result then exit;

			Reset(ns);
			ok := Insert(items);
			// writeln(nsx,' ',nsy,' -> ',ok,'; sxmin=',sxmin,', sxmax=',sxmax,', symin=',symin,', symax=', symax,'; growing: ',growing);
			case _growStrategy of
				grow_Fast: if ok then break;
				grow_Precise:
					if ok then
						if (not growing) and (smin = smax) then
						begin
							if length(items) > 0 then ns := items[0].pos + items[0].size else ns := UintVec2.Ones;
							for i := 1 to High(items) do
								ns := max(ns, items[i].pos + items[i].size);
							ns := align(ns, _sizeAlignment);
							break;
						end else
						begin
							for i := 0 to High(items) do Revert(items[i]);
							smax := ns;
							growing := no;
						end
					else
					begin
						smin := ns;
						if ((smin.x > smin.y) or (smin.x = smax.x)) and (smin.y <> smax.y) then smin.data[1] += _sizeAlignment else smin.data[0] += _sizeAlignment;
					end;
				else Assert(no);
			end;
		until no;

		result := result and ((not Assigned(grow)) or grow(ns, param));
		if result then sz := ns;
	end;

	function AtlasPacker.FindPositionForNewNodeBottomLeft(const size: UintVec2; out rect: UintRect; out bestSizeY, bestSizeX, bestIndex: uint): boolean;
	var
		i: sint;
		y: uint;
	begin
		bestSizeX := High(bestSizeX);
		bestSizeY := High(bestSizeY);
		bestIndex := High(bestIndex);

		for i := 0 to High(skyline) do
		begin
			if RectangleFits(i, size, y) then
				if (y + size.y < bestSizeY) or (y + size.y = bestSizeY) and (skyline[i].sizeX < bestSizeX) then
				begin
					bestSizeY := y + size.y;
					bestIndex := i;
					bestSizeX := skyline[i].sizeX;
					rect := UintRect.Make(UintVec2.Make(skyline[i].x, y), size);
				end;
			if RectangleFits(i, size.YX, y) then
				if (y + size.x < bestSizeY) or (y + size.x = bestSizeY) and (skyline[i].sizeX < bestSizeX) then
				begin
					bestSizeY := y + size.x;
					bestIndex := i;
					bestSizeX := skyline[i].sizeX;
					rect := UintRect.Make(UintVec2.Make(skyline[i].x, y), size.yx);
				end;
		end;
		result := bestIndex <> High(bestIndex);
	end;

	function AtlasPacker.RectangleFits(skylineId: uint; const size: UintVec2; out y: uint): boolean;
	var
		wLeft, i: uint;
	begin
		if skyline[skylineId].x + size.x > sz.x then
			exit(no);

		wLeft := size.x;
		i := skylineId;
		y := skyline[skylineId].y;

		repeat
			y := max(y, skyline[i].y);
			if y + size.y > sz.y then
				exit(no);

			if wLeft <= skyline[i].sizeX then
				exit(yes);

			wLeft -= skyline[i].sizeX;
			inc(i); Assert(i < uint(length(skyline)));
		until no;
	end;

	procedure AtlasPacker.AddSkylineLevel(index: uint; const rect: UintRect);
	var
		nn: SkylineNode;
		i, j: sint;
		shrink: uint;
	begin
		nn.x := rect.pos.x;
		nn.y := rect.pos.y + rect.size.y;
		nn.sizeX := rect.size.x;

		SetLength(skyline, length(skyline) + 1);
		for i := High(skyline) downto index + 1 do
			skyline[i] := skyline[i - 1];
		skyline[index] := nn;

		Assert(nn.x + nn.sizeX <= sz.x);
		Assert(nn.y <= sz.y);

		i := index + 1;
		while i <= High(skyline) do
		begin
			Assert(skyline[i-1].x <= skyline[i].x);

			if skyline[i].x < skyline[i-1].x + skyline[i-1].sizeX then
			begin
				shrink := skyline[i-1].x + skyline[i-1].sizeX;
				shrink -= skyline[i].x;

				if skyline[i].sizeX <= shrink then
				begin
					for j := i to High(skyline) - 1 do
						skyline[j] := skyline[j + 1];
					SetLength(skyline, length(skyline) - 1);
					dec(i);
				end else
				begin
					skyline[i].x += shrink;
					skyline[i].sizeX -= shrink;
					break;
				end;
			end else
				break;
			inc(i);
		end;
		MergeSkylines;
	end;

	procedure AtlasPacker.MergeSkylines;
	var
		i, j: sint;
	begin
		for i := High(skyline) - 1 downto 0 do
			if skyline[i].y = skyline[i + 1].y then
			begin
				skyline[i].sizeX += skyline[i + 1].sizeX;

				for j := i + 1 to High(skyline) - 1 do
					skyline[j] := skyline[j + 1];
				SetLength(skyline, length(skyline) - 1);
			end;
	end;

	function AtlasAnimation._Hash(aa: pAtlasAnimation): Hash.Value;
		function HashFrame(const frame: FrameRec): Hash.Value;
		begin
			case frame.mode of
				frame_Rect: result := Hash.OfFloat(frame.rect.A.x) xor Hash.OfFloat(frame.rect.B.y);
				frame_Atlas: result := Hash.OfPointer(frame.aitem);
				else Assert(no);
			end;
			result := result xor Hash.OfFloat(frame.duration);
		end;
	begin
		with aa^ do
		begin
			result := Hash.OfPointer(atlas) xor Hash.OfUint(uint(length(_frames))) xor Hash.OfFloat(_length);
			if length(_frames) > 0 then
			begin
				result := result xor HashFrame(_frames[0]);
				if length(_frames) > 1 then
					result := result xor HashFrame(_frames[High(_frames)]);
			end;
		end;
	end;

	function AtlasAnimation._Equals(a, b: pAtlasAnimation): boolean;
	const
		TestFlags = [anim_Looped];
	var
		i: sint;
		mode: FrameMode;
	begin
		if (anim_InPool in a^._flags * b^._flags) or (a = b) then exit(a = b);
		result := (a^._atlas = b^._atlas) and (a^._length = b^._length) and (length(a^._frames) = length(b^._frames)) and
			(TestFlags * a^._flags = TestFlags * b^._flags);
		if result then
			for i := 0 to High(a^._frames) do
			begin
				mode := a^._frames[i].mode;
				result := (mode = b^._frames[i].mode) and
					(
						((mode = frame_Rect) and (a^._frames[i].rect = b^._frames[i].rect)) or
						((mode = frame_Atlas) and (a^._frames[i].aitem = b^._frames[i].aitem))
					) and
					(a^._frames[i].duration = b^._frames[i].duration);
				if not result then exit;
			end;
	end;

	procedure AtlasAnimation._InitInstance;
	begin
		if _nInstances = 0 then _pool.Init;
		inc(_nInstances);
	end;

	procedure AtlasAnimation._DoneInstance;
	begin
		dec(_nInstances);
		if _nInstances = 0 then _pool.Done;
	end;

	constructor AtlasAnimation.Init(newAtlas: pAtlas; const newFlags: AtlasAnimationFlags);
	begin
		_InitInstance;
		inherited Init;

		_atlas := MakeRef(newAtlas);
		_frames := nil;
		_flags := newFlags;
		_length := 1.0;
		asp := AspectPair.Identity;
	end;

	destructor AtlasAnimation.Done;
	var
		i: sint;
	begin
		_DisableLocaleNotifications;
		if anim_InPool in _flags then _pool.Remove(@self);
		for i := 0 to High(_frames) do
			_FinalizeFrame(_frames[i]);
		Release(_atlas);
		inherited Done;
		_DoneInstance;
	end;

	procedure AtlasAnimation._FinalizeFrame(var frame: FrameRec);
	begin
		if (frame.mode = frame_Atlas) and Assigned(frame.srcName) then dispose(frame.srcName);
	end;

	procedure AtlasAnimation._Recalculate(start: sint = 0);
	var
		i: sint;
	begin
		for i := start to High(_frames) do
			if i <> 0 then
				_frames[i].startTime := _frames[i - 1].startTime + _frames[i - 1].duration
			else
				_frames[i].startTime := 0.0;

		if length(_frames) <> 0 then
		begin
			_length := _frames[High(_frames)].startTime + _frames[High(_frames)].duration;
			if IsZero(_length) then _length := 1.0;
		end;
	end;

	procedure AtlasAnimation._Append(const newFrames: array of FrameRec);
	var
		start, i: sint;
	begin
		start := length(_frames);
		SetLength(_frames, start + length(newFrames));
		for i := start to High(_frames) do
			_frames[i] := newFrames[i - start];
		_Recalculate(start);
		if start = 0 then _UpdateAspects;
	end;

	procedure _ChangeAtlasAnimationLocale(var loc: Locale; const info: SingleDelegateInfo);
	begin
		Assert(Locale.OnChangeProc(@_ChangeAtlasAnimationLocale) = @_ChangeAtlasAnimationLocale);
		pAtlasAnimation(info.user)^._UpdateLocale(loc, no);
	end;

	procedure AtlasAnimation._EnableLocaleNotifications;
	begin
		if anim_NotifyLocaleChanges in _flags then exit;
		Include(_flags, anim_NotifyLocaleChanges);
		mm.locale.onChange.Add(@_ChangeAtlasAnimationLocale, @self);
	end;

	procedure AtlasAnimation._DisableLocaleNotifications;
	begin
		if not (anim_NotifyLocaleChanges in _flags) then exit;
		Exclude(_flags, anim_NotifyLocaleChanges);
		mm.locale.onChange.Remove(@_ChangeAtlasAnimationLocale, @self);
	end;

	procedure AtlasAnimation._UpdateLocale(var loc: Locale; fix: boolean);
	var
		lname: string;
		i: sint;
	begin
		for i := 0 to High(_frames) do
			if (_frames[i].mode = frame_Atlas) and Assigned(_frames[i].srcName) then
			begin
				lname := loc.Localize(_frames[i].srcName^);
				if fix then
					if _frames[i].srcName^ = lname then
					begin
						dispose(_frames[i].srcName);
						_frames[i].srcName := nil;
					end else
						_EnableLocaleNotifications;
				if Assigned(_atlas) then _frames[i].aitem := _atlas^.Find(lname) else _frames[i].aitem := nil;
			end;
		_UpdateAspects;
	end;

	procedure AtlasAnimation._UpdateAspects;
	var
		r: pRect;
		rotated: boolean;
		fix: float;
	begin
		rotated := no;
		r := nil;
		if length(_frames) > 0 then
			case _frames[0].mode of
				frame_Rect: r := @_frames[0].rect;
				frame_Atlas:
					if Assigned(_frames[0].aitem) then
					begin
						r := @_frames[0].aitem^.rect;
						rotated := _frames[0].aitem^.rotated;
					end else
						r := nil;
				else Assert(no);
			end;

		if Assigned(r) then
		begin
			if Assigned(_atlas) then fix := _atlas^.imgAspect else fix := 1.0;
			asp := AspectPair.Make(r^.Size, rotated, fix);
		end;
	end;

	procedure AtlasAnimation.Append(const newRect: Rect; const newDuration: float);
	var
		frame: FrameRec;
	begin
		frame.mode := frame_Rect;
		frame.rect := newRect;
		frame.duration := newDuration;
		_Append(frame);
	end;

	procedure AtlasAnimation.Append(const baseRect: Rect; const theDx, theDy: float; nx, n: sint; const newDuration: float);
	var
		delta: Vec2;
		i, x, y: sint;
		frames: array of FrameRec;
	begin
		if theDx <> 0.0 then delta.x := theDx else delta.x := baseRect.SizeX;
		if theDy <> 0.0 then delta.y := theDy else delta.y := baseRect.SizeY;
		if n = 0 then n := nx;
		SetLength(frames, n);

		x := 0;
		y := 0;
		for i := 0 to n - 1 do
		begin
			frames[i].mode := frame_Rect;
			frames[i].rect := baseRect + Vec2.Make(x, y) * delta;
			frames[i].duration := newDuration;
			inc(x);
			if x >= nx then
			begin
				x := 0;
				inc(y);
			end;
		end;
		_Append(frames);
	end;

	procedure AtlasAnimation.Append(const newItem: string; const newDuration: float);
	var
		frame: FrameRec;
	begin
		if not Assigned(_atlas) then
		begin
		{$ifdef Debug} Log('"' + newItem + '": атлас не задан.', logError); {$endif}
			exit;
		end;
		frame.mode := frame_Atlas;
		if Locale.LocalizationRequired(newItem) then
		begin
			new(frame.srcName);
			frame.srcName^ := newItem;
			_EnableLocaleNotifications;
		end else
			frame.srcName := nil;
		frame.aitem := _atlas^.Find(mm.locale.Localize(newItem));
		frame.duration := newDuration;
		if Assigned(frame.aitem) then _Append(frame) else _FinalizeFrame(frame);
	end;

	function AtlasAnimation.Merge(anim: pAtlasAnimation): pAtlasAnimation;
	begin
		if (not Assigned(anim)) or (anim_InPool in anim^._flags) then exit(anim);
		result := _pool.Find(anim);
		if Assigned(result) then
		begin
			Assert(anim <> result);
			PumpRef(anim);
		end else
		begin
			_pool.Add(anim);
			Include(anim^._flags, anim_InPool);
			result := anim;
		end;
	end;

	function AtlasAnimation.GetRect(const time: float; out rect: Rect; out rotated: boolean): boolean;
		function return(const frame: FrameRec): boolean;
		begin
			result := yes;
			case frame.mode of
				frame_Rect:
					begin
						rect := frame.rect;
						rotated := no;
					end;
				frame_Atlas:
					begin
						result := Assigned(frame.aitem);
						if result then
						begin
							rect := frame.aitem^.rect;
							rotated := frame.aitem^.rotated;
						end;
					end;
				else Assert(no);
			end;
		end;
	var
		i: sint;
		t2: float;
	begin
		result := no;
		case length(_frames) of
			0: exit;
			1: exit(return(_frames[0]));
		end;

		if anim_Looped in _flags then
			t2 := modf(time, _length)
		else
			t2 := clamp(time, 0.0, _length);
		for i := High(_frames) downto 0 do
			if _frames[i].startTime <= t2 then
				exit(return(_frames[i]));
	end;

	function PosABConverter.Prepare(const bnd: Bounding): PosABConverter;
	const
		CloseToZeroEps3: Vec3 = (data: (CloseToZeroEps, CloseToZeroEps, CloseToZeroEps));
	begin
		result.a := bnd.AABB.A;
		result.idist := 1.0 / Max(bnd.AABB.B - bnd.AABB.A, CloseToZeroEps3);
	end;

	function PosABConverter.AB(const v: Vec3): Vec3;
	begin
		result := (v - a) * idist;
	end;

{$ifdef use_serialization}
const
	AA_LOOPED_BIT         = 1 shl 0;
	AA_ONLY_ONE_FRAME_BIT = 1 shl 1;
	AA_HAS_ATLAS          = 1 shl 2;
	{$define max := ord(High(AtlasAnimation.FrameMode))} {$define nbits := FRAME_MODE_NBITS} {$define mask := FRAME_MODE_MASK} {$include bits_to_store.inc}
	FRAME_LOCALIZED_BIT = 1 shl FRAME_MODE_NBITS;

	procedure SerializeAtlasAnimation(se: pSerializer; obj: pointer);
	var
		anim: pAtlasAnimation absolute obj;
		flags, fflags: uint;
		i: sint;
	begin
		with se^ do
		begin
			flags := 0;
			if anim_Looped in anim^._flags then flags := flags or AA_LOOPED_BIT;
			if length(anim^._frames) = 1 then flags := flags or AA_ONLY_ONE_FRAME_BIT;
			if Assigned(anim^._atlas) then flags := flags or AA_HAS_ATLAS;
			Serialize_ui8(stream, flags);

			if (flags and AA_HAS_ATLAS) <> 0 then SeObject(anim^.atlas);
			if length(anim^._frames) <> 1 then VarInt.Write(stream, length(anim^._frames));
			for i := 0 to High(anim^._frames) do
			begin
				fflags := ord(anim^._frames[i].mode);
				if (anim^._frames[i].mode = frame_Atlas) and Assigned(anim^._frames[i].srcName) then
					fflags := fflags or FRAME_LOCALIZED_BIT;
				Serialize_ui8(stream, fflags);

				case anim^._frames[i].mode of
					frame_Rect: Serialize_vec4N16(stream, anim^._frames[i].rect.AsVec4, Vec4.Zero, Vec4.Ones);
					frame_Atlas:
						if (fflags and FRAME_LOCALIZED_BIT) <> 0 then
							Serialize_string(stream, anim^._frames[i].srcName^)
						else
							Serialize_string(stream, anim^._frames[i].aitem^.name);
				end;
				if length(anim^._frames) > 1 then Serialize_f16(stream, anim^._frames[i].duration);
			end;
		end;
	end;

	procedure DeserializeAtlasAnimation(de: pDeserializer; obj: pointer);
	var
		anim: pAtlasAnimation absolute obj;
		flags, fflags: uint;
		i, n: sint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			if (flags and AA_LOOPED_BIT) <> 0 then Include(anim^._flags, anim_Looped);

			if (flags and AA_HAS_ATLAS) <> 0 then DeObjectR(anim^._atlas) else anim^._atlas := nil;
			if (flags and AA_ONLY_ONE_FRAME_BIT) <> 0 then n := 1 else
				n := VarInt.Read(stream);
			SetLength(anim^._frames, n);
			for i := 0 to High(anim^._frames) do
			begin
				fflags := Deserialize_ui8(stream);
				anim^._frames[i].mode := AtlasAnimation.FrameMode(fflags and FRAME_MODE_MASK);
				case anim^._frames[i].mode of
					frame_Rect: anim^._frames[i].rect := Rect.Make(Deserialize_vec4N16(stream, Vec4.Zero, Vec4.Ones));
					frame_Atlas:
						begin
							pPtrUint(@anim^._frames[i].aitem)^ := fflags;
							new(anim^._frames[i].srcName);
							anim^._frames[i].srcName^ := Deserialize_string(stream);
						end;
					else raise ExhaustiveCase(ord(anim^._frames[i].mode), 'AtlasAnimation.FrameMode');
				end;
				if length(anim^._frames) > 1 then anim^._frames[i].duration := Deserialize_f16(stream) else anim^._frames[i].duration := 1.0;
			end;
		end;
	end;

	procedure AtlasAnimationDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		anim: pAtlasAnimation absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: anim^.Init(nil, []);
			de_After:
				begin
					anim^._Recalculate;
					anim^._UpdateLocale(mm.locale, yes);
					anim := AtlasAnimation.Merge(anim);
				end;
		end;
	end;
{$endif}

	function LoadAtlas(s: pStream): pObject; begin result := new(pAtlas, Init(s)); end;

	procedure Init;
	begin
		AtlasAnimation._nInstances := 0;
		ResourcePool.Shared^.Register(TypeOf(Atlas), @LoadAtlas)^.Tag(GLResourceTag);

	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Atlas', TypeOf(Atlas), nil, sizeof(Atlas), yes, nil, nil, nil, nil)
		^.RegisterType('Atlas animation', TypeOf(AtlasAnimation), nil, sizeof(AtlasAnimation), yes,
			@SerializeAtlasAnimation, @DeserializeAtlasAnimation, nil, @AtlasAnimationDeSpecial);
	{$endif}
	end;

initialization
	&Unit('GLUtils').Initialize(@Init);
end.
