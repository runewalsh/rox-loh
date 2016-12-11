unit Labyrinth;

{$include opts.inc}
{$ifdef Debug}
	{-$define ExtDebug}
{$endif}

interface

uses
	USystem, Errors, UMath, Random, UClasses, Streams, Utils, Tokenizer, Algo, Script, GLBase {$ifdef Debug}, ULog{$endif};

type
	CellType =
	(
		cell_Empty,
		cell_Wall,
		cell_Floor, cell_Passage, cell_Door,
		cell_Water
	);
	CellTypes = set of CellType;

	pCell = ^Cell;
	Cell = record
		typ: CellType;
	end;

	WallType = (wall_None, wall_Wall);

	pWall = ^Wall;
	Wall = record
		typ: WallType;
	end;

	LayoutRotation = (rotate_None, rotate_CCW90, rotate_180, rotate_CW90);

	pLayout = ^Layout;
	Layout = object
	private
		_sizeX, _sizeY: sint;
		_cells: array of Cell;
		_xwalls, _ywalls: array of Wall;
		procedure _WipeAndResize(newSizeX, newSizeY: sint);
		function _ValidatePoint(x, y: sint): boolean;
		function _GetCell(x, y: sint): pCell;
		function _GetWall(x1, y1, x2, y2: sint): pWall;
		procedure _FillWithWalls(typ: WallType);
		procedure _ReplaceWalls(from, to_: WallType);
	public
		constructor Init(newSizeX, newSizeY: sint);
		constructor Init(var cp: Layout);
		destructor Done;
		procedure Clear;
		procedure Fix;
		function Visualize(const stream: string): boolean;
		function GetCopy(rot: LayoutRotation): pLayout;

		property SizeX: sint read _sizeX;
		property SizeY: sint read _sizeY;
		property Cells[x, y: sint]: pCell read _GetCell;
		property Walls[x1, y1, x2, y2: sint]: pWall read _GetWall;
	end;

	{$define classname:=Name2Cell} {$define key_type:=string} {$define inline_key:=StringView} {$define value_type:=Cell}
	{$include hash.h.inc}

	pVaultReaderOpts = ^VaultReaderOpts;
	VaultReaderOpts = object
		subst: Name2Cell;
		constructor Init;
		constructor Init(var cp: VaultReaderOpts);
		destructor Done;
		procedure Clear;
		procedure AddSubst(const s: string; const cell: Cell);
		function FindSubst(const s: StringView): pCell;
		function HandleToken(var ts: tTokenizer; const id: string): boolean;
	end;

	VaultFlag = (vault_Entry);
	VaultFlags = set of VaultFlag;

	pVaultsGroup = ^VaultsGroup;
	VaultsGroup = object(&Object)
	private
		_attraction, _repulsion: float;
		function _ReadFromTokens(var ts: tTokenizer): boolean;
	public
		constructor Init(var ts: tTokenizer);
		destructor Done; virtual;

		property Attraction: float read _attraction;
		property Repulsion: float read _repulsion;
	end;

	{$define classname:=tSet_VaultsGroup} {$define key_type:=pVaultsGroup} {$include hash.h.inc}

	pVaults = ^Vaults;

	pVault = ^Vault;
	Vault = object(&Object)
	private
		_flags: VaultFlags;
		_maxInclusions: sint;
		_chance: float;
		function _ReadData(const s: string; var opts: VaultReaderOpts): boolean;
		function _ReadFromTokens(var ts: tTokenizer; vaults: pVaults): boolean;
	public
		lab: pLayout;
		name, internal: PoolString;
		groups: tSet_VaultsGroup;

		constructor Init;
		constructor Init(var ts: tTokenizer; vaults: pVaults);
		destructor Done; virtual;

		property Flags: VaultFlags read _flags;
		property MaxInclusions: sint read _maxInclusions;
		property Chance: float read _chance;
	end;

	pLocalizedVault = ^LocalizedVault;
	LocalizedVault = object
	public
		vault: pVault;
		local: pLayout;
		x, y: sint;
		rot: LayoutRotation;
		constructor Init(newVault: pVault; newX, newY: sint; newRot: LayoutRotation; newLocal: pLayout);
		destructor Done;
		function SqrDistanceTo(v2: pLocalizedVault): uint;
	end;

	VaultPeekCriteria = function(v: pLocalizedVault; param: pointer): float;

	{$define classname := tHash_Name2VaultsGroup} {$define key_type := PoolString} {$define value_type := pVaultsGroup} {$define null_value := nil}
	{$include hash.h.inc}

	{$define classname:=tHash_Key2Vault} {$define key_type:=uint} {$define value_type:=pLocalizedVault} {$define null_value:=nil}
	{$include hash.h.inc}

	Vaults = object(&Object)
	private
		_cache: tHash_Key2Vault;
		function _GetLocalized(id: sint; rot: LayoutRotation; out key: tHash_Key2Vault.KeyType): pLocalizedVault;
		procedure _StealLocalizedForever(const key: tHash_Key2Vault.KeyType);
	public
		list: array of pVault;
		groups: tHash_Name2VaultsGroup;
		opts: VaultReaderOpts;
		constructor Init;
		destructor Done; virtual;
		procedure Load(s: pStream);
		procedure Load(ts: pTokenizer);
		procedure Clear;
		function Peek(var rng: RNG; criteria: VaultPeekCriteria; param: pointer = nil): pLocalizedVault;
		function PeekAny(var rng: RNG): pLocalizedVault;
		function PeekEntry(var rng: RNG): pLocalizedVault;
	end;

	{$define classname:=tHash_Vault2N} {$define key_type:=pVault} {$define value_type:=uint} {$define null_value:=0}
	{$include hash.h.inc}

	pDungeonGenerator = ^DungeonGenerator;
	DungeonGenerator = object(&Object)
	public const
		SafeNOpened = 8;
	private var
		_openedPassages: array of record
			x, y: sint;
		end;
		_totalPassages, _estimatedPassages: sint;
		_nUsed: tHash_Vault2N;
		_rng: RNG;
		function _GetCell(x, y: sint): pCell;
		function _GetFinalCell(x, y: sint): Cell;
		function _CanPlace(vault: pLocalizedVault): boolean;
		procedure _Place(vault: pLocalizedVault);
		function _GetNUsed(vault: pVault): sint;
		procedure _ChangeNUsed(vault: pVault; delta: sint);
		function _PeekVaultForOpenedPassage(id: sint): pLocalizedVault;
		procedure _AdjustVaultToPassage(vault: pLocalizedVault; passageX, passageY: sint);
		procedure _PlacePassage(x, y: sint);
		procedure _ClosePassage(id: sint);
		procedure _EstimatePassages(vault: pLocalizedVault; out nOpened, nClosed: sint);
		function _FindNearestVault(ref: pLocalizedVault; group: pVaultsGroup): pLocalizedVault;
	public
		vaults: pVaults;
		usedVaults: array of pLocalizedVault;
		entry: pLocalizedVault;
		constructor Init(newVaults: pVaults);
		destructor Done; virtual;
		procedure Clear;
		procedure Generate(newEstimatedPassages: sint);
		function ToLayout: pLayout;
	end;

	procedure OpenScript(var script: ScriptState);

implementation

uses
	U_GL;

	{$define classname:=tSet_VaultsGroup} {$define hash_func:=Hash.OfPointer} {$define finalize_key := Release(_1)}
	{$include hash.pp.inc}

	{$define classname:=Name2Cell} {$define hash_func:=Hash.OfString} {$define get_key:=@_1}
	{$include hash.pp.inc}

	{$define classname:=tHash_Key2Vault} {$define hash_func:=Hash.OfUint}
	{$include hash.pp.inc}

	{$define classname := tHash_Name2VaultsGroup} {$define inline_hash := _1.Hash} {$define finalize_value := Release(_1)}
	{$include hash.pp.inc}

	{$define classname:=tHash_Vault2N} {$define hash_func:=Hash.OfPointer} {$include hash.pp.inc}

type
	CellFlag = (cellf_Passable, cellf_Passage);
	CellFlags = set of CellFlag;

const
	CellTypeIds: array[CellType] of string = ('empty', 'wall', 'floor', 'passage', 'door', 'water');
	CellTypeInfo: array[CellType] of record
		flags: CellFlags;
		allowedOverlaps, strongerThan: CellTypes;
		finalOverlap: CellType;
		color, color2: Vec3;
	end =
	(
		( // empty
			flags: [cellf_Passable];
			allowedOverlaps: [Low(CellType) .. High(CellType)];
			strongerThan: [];
			finalOverlap: cell_Empty;
			color: (data: (0.0, 0.0, 0.0));
			color2: (data: (0.0, 0.0, 0.0))
		),
		( // wall
			flags: [];
			allowedOverlaps: [cell_Wall];
			strongerThan: [];
			finalOverlap: cell_Wall;
			color: (data: (0.3, 0.0, 0.0));
			color2: (data: (0.3, 0.0, 0.0))
		),
		( // floor
			flags: [cellf_Passable];
			allowedOverlaps: [];
			strongerThan: [];
			finalOverlap: cell_Floor;
			color: (data: (0.3, 0.15, 0.07));
			color2: (data: (0.3, 0.15, 0.07))
		),
		( // passage
			flags: [cellf_Passable, cellf_Passage];
			allowedOverlaps: [cell_Passage, cell_Door];
			strongerThan: [];
			finalOverlap: cell_Floor;
			color: (data: (0.4, 0.4, 0.0));
			color2: (data: (0.3, 0.15, 0.07))
		),
		( // door
			flags: [cellf_Passable, cellf_Passage];
			allowedOverlaps: [cell_Passage];
			strongerThan: [cell_Passage];
			finalOverlap: cell_Door;
			color: (data: (0.5, 0.1, 0.0));
			color2: (data: (0.5, 0.1, 0.0))
		),
		( // water
			flags: [];
			allowedOverlaps: [cell_Water];
			strongerThan: [];
			finalOverlap: cell_Water;
			color: (data: (0.0, 0.2, 0.5));
			color2: (data: (0.0, 0.2, 0.5))
		)
	);

	WallTypeInfo: array[WallType] of record
		color: Vec3;
	end =
	(
		(color: (data: (1.0, 0.0, 0.0))),
		(color: (data: (0.0, 0.0, 0.0)))
	);

	EmptyCell: Cell = (typ: cell_Empty);
	EmptyWall: Wall = (typ: wall_None);
	RotationAngles: array[LayoutRotation] of float = (0.0, HalfPi, Pi, -HalfPi);

	procedure Layout._WipeAndResize(newSizeX, newSizeY: sint);
	begin
		_sizeX := newSizeX;
		_sizeY := newSizeY;
		SetLength(_cells, _sizeX * _sizeY);
		SetLength(_xwalls, (_sizeX - 1) * _sizeY);
		SetLength(_ywalls, _sizeX * (_sizeY - 1));
		Clear;
	end;

	function Layout._ValidatePoint(x, y: sint): boolean;
	begin
		result := (x >= 0) and (x < _sizeX) and (y >= 0) and (y < _sizeY);
	end;

	function Layout._GetCell(x, y: sint): pCell;
	begin
		Assert(_ValidatePoint(x, y), 'cell out of range');
		result := @_cells[y * _sizeX + x];
	end;

	function Layout._GetWall(x1, y1, x2, y2: sint): pWall;
	begin
		Assert(_ValidatePoint(x1, y1) and _ValidatePoint(x2, y2), 'wall out of range');
		if x1 > x2 then Swap(x1, x2);
		if y1 > y2 then Swap(y1, y2);

		if (x1 = x2) then // --
		begin
			Assert(y1 + 1 = y2, 'impossible Y-wall');
			result := @_ywalls[y1 * _sizeX + x1];
		end else
			if (y1 = y2) then // |
			begin
				Assert(x1 + 1 = x2, 'impossible X-wall');
				result := @_xwalls[y1 * (_sizeX - 1) + x1];
			end else
			begin
				Assert(no, 'impossible wall');
			end;
	end;

	constructor Layout.Init(newSizeX, newSizeY: sint);
	begin
		_WipeAndResize(newSizeX, newSizeY);
	end;

	constructor Layout.Init(var cp: Layout);
	var
		i: sint;
	begin
		_sizeX := cp._sizeX;
		_sizeY := cp._sizeY;
		SetLength(_cells, length(cp._cells));
		for i := 0 to High(_cells) do
			_cells[i] := cp._cells[i];
		SetLength(_xwalls, length(cp._xwalls));
		for i := 0 to High(_xwalls) do
			_xwalls[i] := cp._xwalls[i];
		SetLength(_ywalls, length(cp._ywalls));
		for i := 0 to High(_ywalls) do
			_ywalls[i] := cp._ywalls[i];
	end;

	destructor Layout.Done;
	begin
	end;

	procedure Layout.Clear;
	var
		i: sint;
	begin
		for i := 0 to High(_cells) do _cells[i] := EmptyCell;
		for i := 0 to High(_xwalls) do _xwalls[i] := EmptyWall;
		for i := 0 to High(_ywalls) do _ywalls[i] := EmptyWall;
	end;

	procedure Layout._FillWithWalls(typ: WallType);
	var
		x, y: sint;
	begin
		for y := 0 to _sizeY - 1 do
			for x := 0 to _sizeX - 1 do
			begin
				if x + 1 < _sizeX then Walls[x, y, x + 1, y]^.typ := typ;
				if y + 1 < _sizeY then Walls[x, y, x, y + 1]^.typ := typ;
			end;
	end;

	procedure Layout._ReplaceWalls(from, to_: WallType);
	var
		x, y: sint;
		wall: pWall;
	begin
		for y := 0 to _sizeY - 1 do
			for x := 0 to _sizeX - 1 do
			begin
				if x + 1 < _sizeX then
				begin
					wall := Walls[x, y, x + 1, y];
					if wall^.typ = from then wall^.typ := to_;
				end;
				if y + 1 < _sizeY then
				begin
					wall := Walls[x, y, x, y + 1];
					if wall^.typ = from then wall^.typ := to_;
				end;
			end;
	end;

	procedure Layout.Fix;
	var
		x, y: sint;
		a, b: boolean;
	begin
		for y := 0 to _sizeY - 2 do
			for x := 0 to _sizeX - 2 do
			begin
				a := cellf_Passable in CellTypeInfo[Cells[x, y]^.typ].flags;
				b := cellf_Passable in CellTypeInfo[Cells[x + 1, y]^.typ].flags;
				if (Walls[x, y, x + 1, y]^.typ in [wall_None]) and (a <> b) then
					Walls[x, y, x + 1, y]^.typ := wall_Wall;
				a := cellf_Passable in CellTypeInfo[Cells[x, y]^.typ].flags;
				b := cellf_Passable in CellTypeInfo[Cells[x, y + 1]^.typ].flags;
				if (Walls[x, y, x, y + 1]^.typ in [wall_None]) and (a <> b) then
					Walls[x, y, x, y + 1]^.typ := wall_Wall;
			end;
	end;

	function Layout.Visualize(const stream: string): boolean;
		function check_wall(x1, y1, x2, y2: sint; out wall: WallType): boolean;
		var
			t: WallType;
		begin
			t := Walls[x1, y1, x2, y2]^.typ;
			result := t <> wall_None;
			if result then wall := t;
		end;
	const
		WallK = 0.1;
		Scale = 16;
	var
		im, wim: pByte;
		imSize: UintVec2;
		x, y, ix, iy: sint;
		sampleX, sampleY: hp_float;
		xf, yf: float;
		color: Vec3;
		pwx1, pwy1, pwx2, pwy2: boolean;
		wall: WallType;
	begin
		result := no;
	{$ifdef Debug} LogR('Визуализация лабиринта в ' + StreamPath.Log(stream) + '...'); {$endif}
		imSize := UintVec2.Make(_sizeX, _sizeY) * Scale;
		im := GetMem(imSize.Product * sizeof(Vec3u8));
		wim := im;

		for iy := 0 to imSize.y - 1 do
			for ix := 0 to imSize.x - 1 do
			begin
				sampleX := ix / imSize.x * _sizeX;
				sampleY := iy / imSize.y * _sizeY;
				x := trunc(sampleX);
				y := trunc(sampleY);
				xf := frac(sampleX);
				yf := frac(sampleY);
				pwx1 := (xf < WallK) and (x > 0);
				pwx2 := (xf > 1.0 - WallK) and (x + 1 < _sizeX);
				pwy1 := (yf < WallK) and (y > 0);
				pwy2 := (yf > 1.0 - WallK) and (y + 1 < _sizeY);
				if (pwx1 and check_wall(x, y, x-1, y, wall)) or
					(pwx2 and check_wall(x, y, x+1, y, wall)) or
					(pwy1 and check_wall(x, y, x, y-1, wall)) or
					(pwy2 and check_wall(x, y, x, y+1, wall)) or
					(pwx1 and pwy1 and (check_wall(x, y-1, x-1, y-1, wall) or check_wall(x-1, y, x-1, y-1, wall))) or
					(pwx2 and pwy1 and (check_wall(x, y-1, x+1, y-1, wall) or check_wall(x+1, y, x+1, y-1, wall))) or
					(pwx1 and pwy2 and (check_wall(x, y+1, x-1, y+1, wall) or check_wall(x-1, y, x-1, y+1, wall))) or
					(pwx2 and pwy2 and (check_wall(x, y+1, x+1, y+1, wall) or check_wall(x+1, y, x+1, y+1, wall))) then
				begin
					color := WallTypeInfo[wall].color;
				end else
					if xf + yf < 1.0 then
						color := CellTypeInfo[Cells[x, y]^.typ].color
					else
						color := CellTypeInfo[Cells[x, y]^.typ].color2;
				wim^ := round(color.X * High(wim^));
				(wim + 1)^ := round(color.Y * High(wim^));
				(wim + 2)^ := round(color.Z * High(wim^));
				wim := wim + 3;
			end;

		TextureImage.Save(stream, imSize, GLformat_RGB, im, yes);
	end;

	function Layout.GetCopy(rot: LayoutRotation): pLayout;
	var
		x, y: sint;
	begin
		case rot of
			rotate_None: result := new(pLayout, Init(self));
			rotate_CCW90:
				begin
					result := new(pLayout, Init(sizeY, sizeX));
					for y := 0 to sizeY - 1 do
						for x := 0 to sizeX - 1 do
							result^.Cells[y, sizeX - x - 1]^ := Cells[x, y]^;
				end;
			rotate_180:
				begin
					result := new(pLayout, Init(sizeX, sizeY));
					for y := 0 to sizeY - 1 do
						for x := 0 to sizeX - 1 do
							result^.Cells[sizeX - x - 1, sizeY - y - 1]^ := Cells[x, y]^;
				end;
			rotate_CW90:
				begin
					result := new(pLayout, Init(sizeY, sizeX));
					for y := 0 to sizeY - 1 do
						for x := 0 to sizeX - 1 do
							result^.Cells[sizeY - y - 1, x]^ := Cells[x, y]^;
				end;
			else
				Assert(no);
		end;
	end;

	function VaultsGroup._ReadFromTokens(var ts: tTokenizer): boolean;
	var
		id: string;
	begin
		result := no;
		if not ts.ReadBlockStart then exit;

		repeat
			case ts.NextTokenType of
				token_Identifier: id := ts.ReadIdentifier;
				token_BlockEnd: break;
				else exit;
			end;
			if id = 'attraction' then
			begin
				_attraction := ts.ReadFloat;
			end else
			if id = 'repulsion' then
			begin
				_repulsion := ts.ReadFloat;
			end else
				raise UnknownIdentifier(id, 'VaultGroup');
		until no;

		ts.ReadBlockEnd;
		result := yes;
	end;

	constructor VaultsGroup.Init(var ts: tTokenizer);
	begin
		inherited Init;
		_attraction := 0.0;
		_repulsion := 0.0;
		if not _ReadFromTokens(ts) then ConstructorFailed;
	end;

	destructor VaultsGroup.Done;
	begin
		inherited Done;
	end;

	function Vault._ReadData(const s: string; var opts: VaultReaderOpts): boolean;
	const
		MaxSubstLen = 2;
	var
		pos: sint;
		buf: array[0 .. MaxSubstLen - 1] of char;
		cur: StringView;
		cell: pCell;
		cells: array of array of Labyrinth.Cell; // [y][x]
		x, y, sizeX, sizeY: sint;
		neol: size_t;
	begin
		result := no;
		SetLength(cells, 1);
		cells[0] := nil;
		pos := 1;
		cur := StringView.Make(buf, 0);
		x := 0;
		y := 0;
		sizeX := 0;
		sizeY := 0;

		while pos <= length(s) do
			if UTF8.IsEOL(pChar(s) + pos - 1, length(s) - pos + 1, neol) then
			begin
				if x > 0 then
				begin
					sizeX := max(sizeX, x);
					x := 0;
					inc(y);
					SetLength(cells, y + 1);
				end;
				pos += sint(neol);
			end else
			if s[pos] in [TabSym] then inc(pos) else
			begin
				cur.p[cur.n] := s[pos];
				inc(cur.n);
				inc(pos);
				cell := opts.FindSubst(cur);
				if not Assigned(cell) then
					if cur.n < MaxSubstLen then
						continue
					else
					begin
					{$ifdef Debug} LogR('Неопознанный кусок "{0}" в ({1}, {2})', cur.ToString, ToString(x), ToString(y), logError); {$endif}
						cell := @EmptyCell;
					end;
				cur.n := 0;
				SetLength(cells[y], x + 1);
				cells[y, x] := cell^;
				inc(x);
			end;
		if x > 0 then
		begin
			sizeX := max(sizeX, x);
			sizeY := y + 1;
		end else
			sizeY := y;
		if (sizeX < 1) or (sizeY < 1) then exit;

		if Assigned(lab) then dispose(lab, Done);
		lab := new(pLayout, Init(sizeX, sizeY));
		for y := 0 to sizeY - 1 do
			for x := 0 to High(cells[y]) do
				lab^.Cells[x, y]^ := cells[y, x];
	end;

	function Vault._ReadFromTokens(var ts: tTokenizer; vaults: pVaults): boolean;
	label _finally_;
	var
		id: string;
		group: pVaultsGroup;
		opts: VaultReaderOpts;
	begin
		result := no;

		name := ts.ReadString;
	{$ifdef ExtDebug} LogR('Чтение ваулта "' + name^.Ansi + '"... '); {$endif}
		opts.Init(vaults^.opts);
		if not ts.ReadBlockStart then goto _finally_;

		repeat
			case ts.NextTokenType of
				token_Identifier: id := ts.ReadIdentifier;
				token_BlockEnd: break;
				else goto _finally_;
			end;
			if id = 'internal' then
			begin
				internal := ts.ReadString;
			end else
			if id = 'group' then
			begin
				group := MakeRef(vaults^.groups.Find(ts.ReadString));
				if Assigned(group) then
					groups.Add(group)
				{$ifdef Debug} else Log('Группа "' + id + '" не найдена', logError) {$endif};
			end else
			if id = 'data' then
			begin
				_ReadData(ts.ReadString, opts);
			end else
			if id = 'entry' then
			begin
				Include(_flags, vault_Entry);
			end else
			if id = 'max_inclusions' then
			begin
				_maxInclusions := ts.ReadInteger;
			end else
			if id = 'unique' then
			begin
				_maxInclusions := 1;
			end else
			if id = 'chance' then
			begin
				_chance := ts.ReadFloat;
			end else
			if opts.HandleToken(ts, id) then
			begin
			end else
				raise UnknownIdentifier(id, 'Vault');
		until no;
		ts.ReadBlockEnd;

		result := Assigned(lab);
	_finally_:
		opts.Done;
	{$ifdef Debug}
		if result then
		begin
		{$ifdef ExtDebug} Log('Прочитан ваулт "' + name^.Ansi + '": sizeX = ' + ToString(lab^.sizeX) + ', sizeY = ' + ToString(lab^.sizeY), logOK) {$endif}
		end else
			Log('Ошибка при чтении ваулта "' + name + '"', logError);
	{$endif}
	end;

	constructor Vault.Init;
	begin
		inherited Init;
		lab := nil;
		name := '';
		internal := '';
		groups.Init;
		_flags := [];
		_maxInclusions := 0;
		_chance := 1.0;
	end;

	constructor Vault.Init(var ts: tTokenizer; vaults: pVaults);
	begin
		Init;
		if not _ReadFromTokens(ts, vaults) then ConstructorFailed;
	end;

	destructor Vault.Done;
	begin
		groups.Done;
		if Assigned(lab) then dispose(lab, Done);
		inherited Done;
	end;

	constructor VaultReaderOpts.Init;
	begin
		subst.Init;
	end;

	constructor VaultReaderOpts.Init(var cp: VaultReaderOpts);
	var
		it: Name2Cell.Iterator;
	begin
		Init;
		it := cp.subst.GetIterator;
		while cp.subst.Next(it) do
			AddSubst(cp.subst.GetKey(it)^, cp.subst.GetValue(it)^);
	end;

	destructor VaultReaderOpts.Done;
	begin
		subst.Done;
	end;

	procedure VaultReaderOpts.Clear;
	begin
		subst.Clear;
	end;

	procedure VaultReaderOpts.AddSubst(const s: string; const cell: Cell);
	begin
		subst.Add(s, cell);
	end;

	function VaultReaderOpts.FindSubst(const s: StringView): pCell;
	begin
		result := subst.Find(s);
	end;

	function VaultReaderOpts.HandleToken(var ts: tTokenizer; const id: string): boolean;
	var
		s: string;
		cell: Labyrinth.Cell;
	begin
		result := yes;
		if id = 'subst' then
		begin
			s := ts.ReadString;
			cell := EmptyCell;
			cell.typ := CellType(FindStr(ts.ReadString, CellTypeIds, ord(cell_Empty)));
			AddSubst(s, cell);
		end else
			result := no;
	end;

	constructor Vaults.Init;
	begin
		inherited Init;
		list := nil;
		_cache.Init;
		groups.Init;
		opts.Init;
	end;

	destructor Vaults.Done;
	begin
		Clear;
		opts.Done;
		groups.Done;
		_cache.Done;
		inherited Done;
	end;

	procedure Vaults.Load(s: pStream);
	begin
		Load(new(pTokenizer, Init(s)));
	end;

	procedure Vaults.Load(ts: pTokenizer);
	label _finally_;
	var
		id: string;
		vault: pVault;
		group: pVaultsGroup;
	{$ifdef Debug} nLoaded: sint; {$endif}
	begin
		if not Assigned(MakeRef(ts)) then exit;
	{$ifdef Debug}
		LogR('Загрузка ваултов из ' + StreamPath.Log(ts^.StreamPath) + '... ');
		nLoaded := 0;
	{$endif}

		repeat
			case ts^.NextTokenType of
				token_Identifier: id := ts^.ReadIdentifier;
				token_Finale: break;
				else goto _finally_;
			end;
			if id = 'vault' then
			begin
				vault := MakeRef(new(pVault, Init(ts^, @self)));
				if not Assigned(vault) then goto _finally_;
				SetLength(list, length(list) + 1);
				list[High(list)] := vault;
			{$ifdef Debug} inc(nLoaded); {$endif}
			end else
			if id = 'group' then
			begin
				id := ts^.ReadString;
				group := MakeRef(new(pVaultsGroup, Init(ts^)));
				if not Assigned(group) then goto _finally_;
				groups.Add(id, group);
			end else
			if opts.HandleToken(ts^, id) then
			begin
			end else
				raise UnknownIdentifier(id, 'Vaults');
		until no;

	_finally_:
	{$ifdef Debug} Log(StreamPath.Log(ts^.StreamPath) + ' — ваулты загружены (' + ToString(nLoaded) + ' шт.)', logOK); {$endif}
		Release(ts);
	end;

	procedure Vaults.Clear;
	var
		i: sint;
		it: tHash_Key2Vault.Iterator;
	begin
		opts.Clear;
		it := _cache.GetIterator;
		while _cache.Next(it) do
			dispose(_cache.GetValue(it)^, Done);
		_cache.Clear;

		for i := 0 to High(list) do
			Release(list[i]);
		list := nil;
	end;

	function Vaults._GetLocalized(id: sint; rot: LayoutRotation; out key: tHash_Key2Vault.KeyType): pLocalizedVault;
	begin
		key := (tHash_Key2Vault.KeyType(ord(rot)) shl (bitsizeof(tHash_Key2Vault.KeyType) div 2)) + tHash_Key2Vault.KeyType(id);
		result := _cache.Find(key);
		if not Assigned(result) then
		begin
			result := new(pLocalizedVault, Init(list[id], 0, 0, rot, list[id]^.lab^.GetCopy(rot)));
			_cache.Add(key, result);
		end;
	end;

	procedure Vaults._StealLocalizedForever(const key: tHash_Key2Vault.KeyType);
	begin
		_cache.Remove(key);
	end;

	function Vaults.Peek(var rng: RNG; criteria: VaultPeekCriteria; param: pointer = nil): pLocalizedVault;
	var
		i, n: sint;
		lv: array of pLocalizedVault;
		ids: array of tHash_Key2Vault.KeyType;
		crits: array of float;
		rot: LayoutRotation;
	begin
		SetLength(crits, length(list) * (ord(High(rot)) + 1));
		SetLength(ids, length(crits));
		SetLength(lv, length(crits));
		n := 0;
		for i := 0 to High(list) do
			for rot in LayoutRotation do
			begin
				lv[n] := _GetLocalized(i, rot, ids[n]);
				crits[n] := criteria(lv[n], param);
				if crits[n] > 0.0 then inc(n);
			end;
		SetLength(ids, n);
		SetLength(crits, n);
		SetLength(lv, n);
		i := rng.Choose(crits, [MayChooseNothing]);
		if i >= 0 then
		begin
			result := lv[i];
			_StealLocalizedForever(ids[i]);
		end else
			result := nil;
	end;

	function _PeekAnyVault(vault: pLocalizedVault; param: pointer): float;
	begin
		Assert(@param = @param);
		result := vault^.vault^.chance;
	end;

	function Vaults.PeekAny(var rng: RNG): pLocalizedVault;
	begin
		result := Peek(rng, @_PeekAnyVault);
	end;

	function _PeekEntryVault(vault: pLocalizedVault; param: pointer): float;
	begin
		Assert(@param = @param);
		if (vault_Entry in vault^.vault^.flags) and (vault^.rot = rotate_None) then
			result := vault^.vault^.chance
		else
			result := 0.0;
	end;

	function Vaults.PeekEntry(var rng: RNG): pLocalizedVault;
	begin
		result := Peek(rng, @_PeekEntryVault);
	end;

	constructor LocalizedVault.Init(newVault: pVault; newX, newY: sint; newRot: LayoutRotation; newLocal: pLayout);
	begin
		vault := MakeRef(newVault);
		x := newX;
		y := newY;
		rot := newRot;
		local := newLocal;
	end;

	destructor LocalizedVault.Done;
	begin
		if Assigned(local) then dispose(local, Done);
		Release(vault);
	end;

	function LocalizedVault.SqrDistanceTo(v2: pLocalizedVault): uint;
	begin
		result :=
			sqr((v2^.x + v2^.local^.sizeX div 2) - (x + local^.sizeX div 2)) +
			sqr((v2^.y + v2^.local^.sizeY div 2) - (y + local^.sizeY div 2));
	end;

	constructor DungeonGenerator.Init(newVaults: pVaults);
	begin
		if not Assigned(newVaults) then Fail;
		inherited Init;
		_rng.Init(Good);
		vaults := MakeRef(newVaults);
		_nUsed.Init;
		usedVaults := nil;
		_openedPassages := nil;
		Clear;
	end;

	destructor DungeonGenerator.Done;
	begin
		Clear;
		_nUsed.Done;
		Release(vaults);
		_rng.Done;
		inherited Done;
	end;

	procedure DungeonGenerator.Clear;
	var
		i: sint;
	begin
		for i := 0 to High(usedVaults) do
			dispose(usedVaults[i], Done);
		usedVaults := nil;
		_nUsed.Clear;
		_openedPassages := nil;
		_totalPassages := 0;
	end;

	procedure DungeonGenerator.Generate(newEstimatedPassages: sint);
	label _retry_;
	const
		MaxTries = 100;
	var
		vault: pLocalizedVault;
		id: sint;
		retryNo: sint;
	begin
		retryNo := 0;
		_estimatedPassages := newEstimatedPassages;
	{$ifdef Debug}
		LogR('Генерация уровня (ориентировочное число переходов: ' + ToString(_estimatedPassages) + ')... ');
	{$endif}

	_retry_:
		Clear;
		if retryNo >= MaxTries then
		begin
		{$ifdef Debug} Log('Превышено максимальное количество попыток (' + ToString(MaxTries) + ')', logError); {$endif}
			exit;
		end;
		inc(retryNo);
	{$ifdef Debug} LogR('Попытка #' + ToString(retryNo) + '... '); {$endif}

		vault := vaults^.PeekEntry(_rng);
		if not Assigned(vault) then
		begin
		{$ifdef Debug} Log('Не удалось подобрать стартовый ваулт', logWarning); {$endif}
			exit;
		end;
		_Place(vault);

		while length(_openedPassages) > 0 do
		begin
			id := 0;
			vault := _PeekVaultForOpenedPassage(id);
			if Assigned(vault) then
			begin
				_AdjustVaultToPassage(vault, _openedPassages[id].x, _openedPassages[id].y);
				_Place(vault);
			end else
			begin
			{$ifdef Debug}
				Log('Не удалось подобрать ваулт для закрытия перехода (' +
					ToString(_openedPassages[id].x) + ', ' + ToString(_openedPassages[id].y) + ')', logError);
			{$endif}
				_ClosePassage(id);
			end;
		end;

		if _totalPassages < newEstimatedPassages div 3 then
		begin
		{$ifdef Debug} Log('Слишком мало переходов (' + ToString(_totalPassages) + ')', logWarning); {$endif}
			goto _retry_;
		end;

	{$ifdef Debug}
		Log('OK (ваултов: ' + ToString(length(usedVaults)) +
			'; переходов: ' + ToString(_totalPassages) + ')', logOK);
	{$endif}
	end;

	function DungeonGenerator.ToLayout: pLayout;
	var
		i, x, y, sizeX, sizeY, x1, y1, x2, y2: sint;
		uv: pLocalizedVault;
		local: pLayout;
	begin
		x1 := High(x1);
		y1 := High(y1);
		x2 := Low(x2);
		y2 := Low(y2);
	{$ifdef Debug} LogR('"Растеризация" данжа (ваултов: ' + ToString(length(usedVaults)) + ')... '); {$endif}
		for i := 0 to High(usedVaults) do
		begin
			uv := usedVaults[i];
			local := uv^.local;
			x1 := min(x1, uv^.x);
			y1 := min(y1, uv^.y);
			x2 := max(x2, uv^.x + local^.SizeX);
			y2 := max(y2, uv^.y + local^.SizeY);
		end;
		if (x1 > x2) or (y1 > y2) then
		begin
		{$ifdef Debug} Log('Fail', logError); {$endif}
			exit(nil);
		end;

		sizeX := x2 - x1 + 1;
		sizeY := y2 - y1 + 1;
		result := new(pLayout, Init(sizeX, sizeY));
		for y := 0 to sizeY - 1 do
			for x := 0 to sizeX - 1 do
				result^.Cells[x, y]^ := _GetFinalCell(x1 + x, y1 + y);
	{$ifdef Debug} Log('OK (размеры: ' + ToString(sizeX) + ' x ' + ToString(sizeY) + ')', logOK); {$endif}
	end;

	function DungeonGenerator._GetCell(x, y: sint): pCell;
	var
		uv: pLocalizedVault;
		local: pLayout;
		i: sint;
		cell: pCell;
	begin
		result := nil;
		for i := 0 to High(usedVaults) do
		begin
			uv := usedVaults[i];
			local := uv^.local;
			if (x < uv^.x) or (y < uv^.y) or (x >= uv^.x + local^.SizeX) or (y >= uv^.y + local^.SizeY) then continue;
			cell := local^.Cells[x - uv^.x, y - uv^.y];
			if cell^.typ = cell_Empty then continue;
			if Assigned(result) then
			begin
				Assert(result^.typ in CellTypeInfo[cell^.typ].allowedOverlaps,
					'shouldn''t overlap: ' + ToString(ord(result^.typ)) + ' ' + ToString(ord(cell^.typ)));
				Assert(
					(result^.typ in CellTypeInfo[cell^.typ].allowedOverlaps) and
					(cell^.typ in CellTypeInfo[result^.typ].allowedOverlaps),
					'non-symmetric allowedOverlaps: ' + ToString(ord(result^.typ)) + ' ' + ToString(ord(cell^.typ)));
				if result^.typ in CellTypeInfo[cell^.typ].strongerThan then
				begin
					Assert(not (cell^.typ in CellTypeInfo[result^.typ].strongerThan), 'non-symmetric strongerThan: ' + ToString(ord(cell^.typ)) + ' ' + ToString(ord(result^.typ)));
					result := cell;
				end;
			end else
				result := cell;
		end;
	end;

	function DungeonGenerator._GetFinalCell(x, y: sint): Cell;
	var
		cell: pCell;
	begin
		cell := _GetCell(x, y);
		if Assigned(cell) then
			result := cell^
		else
			result := EmptyCell;
		result.typ := CellTypeInfo[result.typ].finalOverlap;
	end;

	function DungeonGenerator._CanPlace(vault: pLocalizedVault): boolean;
	var
		x, y: sint;
		cell, localCell: pCell;
	begin
		if vault^.vault^.MaxInclusions <> 0 then
			if _GetNUsed(vault^.vault) >= vault^.vault^.MaxInclusions then
				exit(no);
		for y := 0 to vault^.local^.SizeY - 1 do
			for x := 0 to vault^.local^.SizeX - 1 do
			begin
				cell := _GetCell(vault^.x + x, vault^.y + y);
				if not Assigned(cell) then continue;
				localCell := vault^.local^.Cells[x, y];
				if cell^.typ in CellTypeInfo[localCell^.typ].allowedOverlaps then continue;
				exit(no);
			end;
		result := yes;
	end;

	procedure DungeonGenerator._Place(vault: pLocalizedVault);
	var
		x, y: sint;
		cell: pCell;
	begin
		for y := 0 to vault^.local^.sizeY - 1 do
			for x := 0 to vault^.local^.sizeX - 1 do
			begin
				cell := vault^.local^.Cells[x, y];
				if cellf_Passage in CellTypeInfo[cell^.typ].flags then
					_PlacePassage(vault^.x + x, vault^.y + y);
			end;
	{$ifdef ExtDebug}
		Log('Ваулт "' + vault^.vault^.name^.Ansi + '" размещён в (' + ToString(vault^.x) + ', ' + ToString(vault^.y) + ')', logOK);
	{$endif}
		SetLength(usedVaults, length(usedVaults) + 1);
		usedVaults[High(usedVaults)] := vault;
		_ChangeNUsed(vault^.vault, +1);
	end;

	function DungeonGenerator._GetNUsed(vault: pVault): sint;
	begin
		result := _nUsed.Find(vault);
	end;

	procedure DungeonGenerator._ChangeNUsed(vault: pVault; delta: sint);
	var
		n: sint;
	begin
		n := _GetNUsed(vault) + delta;
		if n > 0 then
			_nUsed.Add(vault, n)
		else
			_nUsed.Remove(vault);
	end;

type
	PeekVaultForPassageParams = record
		dg: pDungeonGenerator;
		passageX, passageY: sint;
	end;

	function _ModifyChanceByPassages(vault: pLocalizedVault; dg: pDungeonGenerator; const chance: float): float;
	var
		nOpened, nClosed, delta: sint;
	begin
		result := chance;
		dg^._EstimatePassages(vault, nOpened, nClosed);
		delta := nOpened - nClosed;
		if delta >= 0 then
		begin
			if dg^._totalPassages + nOpened > dg^._estimatedPassages then exit(result * 1e-6);
			if nClosed >= 2 then
				result *= 15.0 * sqrt(nClosed);
		end else
		begin
			if (dg^._totalPassages < dg^._estimatedPassages - DungeonGenerator.SafeNOpened) and
				(length(dg^._openedPassages) + delta < DungeonGenerator.SafeNOpened)
			then
				exit(result * 1e-6);
		end;
	end;

	function _ModifyChanceByRepulsion(local: pLocalizedVault; dg: pDungeonGenerator; const chance: float): float;
	var
		it: tSet_VaultsGroup.Iterator;
		vault: pVault;
		group: pVaultsGroup;
		ref: pLocalizedVault;
		dist: float;
	begin
		result := chance;
		vault := local^.vault;

		it := vault^.groups.GetIterator;
		while vault^.groups.Next(it) do
		begin
			group := vault^.groups.GetKey(it)^;
			ref := dg^._FindNearestVault(local, group);
			if not Assigned(ref) then continue;

			dist := sqrt(local^.SqrDistanceTo(ref));
			if dist < group^.Attraction then
			begin
				result *= 1.0 + 9.0 * (1.0 - dist / group^.Attraction);
			end;
			if dist < group^.Repulsion then
			begin
				result *= pow(dist / group^.Repulsion, 2.0);
			end;
		end;
	end;

	function _PeekVaultForPassage(vault: pLocalizedVault; param: pointer): float;
	var
		params: ^PeekVaultForPassageParams absolute param;
		x, y: sint;
		cell: pCell;
	begin
		for y := 0 to vault^.local^.sizeY - 1 do
			for x := 0 to vault^.local^.sizeX - 1 do
			begin
				cell := vault^.local^.Cells[x, y];
				if cellf_Passage in CellTypeInfo[cell^.typ].flags then
				begin
					vault^.x := params^.passageX - x;
					vault^.y := params^.passageY - y;
					if params^.dg^._CanPlace(vault) then
					begin
						result := vault^.vault^.Chance;
						result := _ModifyChanceByPassages(vault, params^.dg, result);
						result := _ModifyChanceByRepulsion(vault, params^.dg, result);
						exit;
					end;
				end;
			end;
		result := 0.0;
	end;

	function DungeonGenerator._PeekVaultForOpenedPassage(id: sint): pLocalizedVault;
	var
		params: PeekVaultForPassageParams;
	begin
	{$ifdef ExtDebug}
		LogR('Поиск ваулта для перехода (' + ToString(_openedpassages[id].x) + ', ' + ToString(_openedpassages[id].y) + ')... ', logDebug);
	{$endif}
		params.dg := @self;
		params.passageX := _openedPassages[id].x;
		params.passageY := _openedPassages[id].y;
		result := vaults^.Peek(_rng, @_PeekVaultForPassage, @params);
	end;

	procedure DungeonGenerator._AdjustVaultToPassage(vault: pLocalizedVault; passageX, passageY: sint);
	var
		variants: array of record
			x, y: sint;
		end;
		id, x, y: sint;
	begin
		variants := nil;
		for y := 0 to vault^.local^.sizeY - 1 do
			for x := 0 to vault^.local^.sizeX - 1 do
				if cellf_Passage in CellTypeInfo[vault^.local^.Cells[x, y]^.typ].flags then
				begin
					vault^.x := passageX - x;
					vault^.y := passageY - y;
					if _CanPlace(vault) then
					begin
						SetLength(variants, length(variants) + 1);
						variants[High(variants)].x := vault^.x;
						variants[High(variants)].y := vault^.y;
					end;
				end;
		Assert(length(variants) > 0);
		id := _rng.GetUint(length(variants));
		vault^.x := variants[id].x;
		vault^.y := variants[id].y;
	end;

	procedure DungeonGenerator._PlacePassage(x, y: sint);
	var
		i: sint;
	begin
		for i := 0 to High(_openedPassages) do
			if (_openedPassages[i].x = x) and (_openedPassages[i].y = y) then
			begin
				_ClosePassage(i);
				exit;
			end;
		SetLength(_openedPassages, length(_openedPassages) + 1);
		_openedPassages[High(_openedPassages)].x := x;
		_openedPassages[High(_openedPassages)].y := y;
		inc(_totalPassages);
	{$ifdef ExtDebug} LogR('открыт переход ('+ToString(x)+', '+ToString(y)+'); ', logDebug); {$endif}
	end;

	procedure DungeonGenerator._ClosePassage(id: sint);
	var
		i: sint;
	begin
	{$ifdef ExtDebug} LogR('закрыт переход ('+ToString(_openedPassages[id].x)+', '+ToString(_openedPassages[id].y)+'); ', logDebug); {$endif}
		for i := id to High(_openedPassages) - 1 do
			_openedPassages[i] := _openedPassages[i + 1];
		SetLength(_openedPassages, length(_openedPassages) - 1);
	end;

	procedure DungeonGenerator._EstimatePassages(vault: pLocalizedVault; out nOpened, nClosed: sint);
	var
		x, y: sint;
		cell: pCell;
	begin
		nOpened := 0;
		nClosed := 0;
		for y := 0 to vault^.local^.sizeY - 1 do
			for x := 0 to vault^.local^.sizeX - 1 do
				if cellf_Passage in CellTypeInfo[vault^.local^.Cells[x, y]^.typ].flags then
				begin
					cell := _GetCell(vault^.x + x, vault^.y + y);
					if Assigned(cell) and (cellf_Passage in CellTypeInfo[cell^.typ].flags) then
						inc(nClosed)
					else
						inc(nOpened);
				end;
	end;

	function DungeonGenerator._FindNearestVault(ref: pLocalizedVault; group: pVaultsGroup): pLocalizedVault;
	var
		i: sint;
		vault: pVault;
		candidates: array of pLocalizedVault;
		nCandidates: sint;
		curDist, bestDist: sint;
	begin
		SetLength(candidates, length(usedVaults));
		nCandidates := 0;
		for i := 0 to High(usedVaults) do
		begin
			vault := usedVaults[i]^.vault;
			if Assigned(vault^.groups.Find(group)) then
			begin
				candidates[nCandidates] := usedVaults[i];
				inc(nCandidates);
			end;
		end;

		if nCandidates > 0 then
		begin
			result := candidates[0];
			bestDist := ref^.SqrDistanceTo(result);
			for i := 1 to nCandidates - 1 do
			begin
				curDist := ref^.SqrDistanceTo(candidates[i]);
				if curDist < bestDist then
				begin
					bestDist := curDist;
					result := candidates[i];
				end;
			end;
		end else
			result := nil;
	end;

	procedure Script_VisualizeDungeon(var ss: ScriptState);
	var
		dg: pDungeonGenerator;
		lab: pLayout;
	begin
		dg := ss.ToObject(1, TypeOf(DungeonGenerator));
		lab := dg^.ToLayout;
		if Assigned(lab) then
		begin
			lab^.Visualize(ss.ToStream(2));
			dispose(lab, Done);
		end;
	end;

	procedure Script_DungeonGenerator_Generate(var ss: ScriptState);
	begin
		pDungeonGenerator(ss.ToSelf)^.Generate(ss.GetSintField(2, 'nPassages', 128));
	end;

	procedure Script_DungeonGenerator_Decompose(var ss: ScriptState);
	var
		i: sint;
		dg: pDungeonGenerator;
		uv: pLocalizedVault;
		modelX, modelY: sint;
	begin
		dg := pDungeonGenerator(ss.ToSelf);
		ss.PushTable;
		for i := 0 to High(dg^.usedVaults) do
		begin
			uv := dg^.usedVaults[i];
			ss.PushTable;
			ss.PushString(uv^.vault^.internal); ss.SetTableS(-2, 'internal');
			ss.PushFloat(RotationAngles[uv^.rot]); ss.SetTableS(-2, 'ry');
			modelX := uv^.x;
			modelY := uv^.y;
			case uv^.rot of
				rotate_None: ;
				rotate_CW90:
					begin
						modelX += uv^.local^.sizeX;
					end;
				rotate_180:
					begin
						modelX += uv^.local^.sizeX;
						modelY += uv^.local^.sizeY;
					end;
				rotate_CCW90:
					begin
						modelY += uv^.local^.sizeY;
					end;
			end;
			ss.PushSint(modelX); ss.SetTableS(-2, 'modelX');
			ss.PushSint(modelY); ss.SetTableS(-2, 'modelY');
			ss.SetTableI(-2, i + 1);
		end;
		ss.PushSint(length(dg^.usedVaults)); ss.SetTableS(-2, 'n');
	end;

	procedure Script_CreateDungeonGenerator(var ss: ScriptState);
	var
		v: pVaults;
		i: sint;
	begin
		v := new(pVaults, Init);
		for i := 1 to ss.RawLen(1) do
			v^.Load(GetStream(ss.ToStream(ss.GetStringField(1, i))));
		ss.PushObject(new(pDungeonGenerator, Init(v)));
	end;

	procedure OpenScript(var script: ScriptState);
	const
		Stuff: array[0 .. 4] of ScriptStuffDesc =
		(
			(s: TypeDesc; p: TypeOf(DungeonGenerator)),
			(s: 'Generate:0'; p: @Script_DungeonGenerator_Generate),
			(s: 'Decompose:1'; p: @Script_DungeonGenerator_Decompose),

			(s: FunctionsDesc + 'CreateDungeonGenerator:1' + RequireEnv; p: @Script_CreateDungeonGenerator),
			(s: 'VisualizeDungeon:0' + RequireEnv; p: @Script_VisualizeDungeon)
		);
	begin
		script.AddStuff(Stuff);
	end;

end.
