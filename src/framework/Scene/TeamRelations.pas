unit TeamRelations;

{$include opts.inc}

interface

uses
	USystem, UMath, Algo, UClasses, Script, Utils {$ifdef use_serialization}, Streams {$endif} {$ifdef Debug}, ULog {$endif};

type
	// TODO: сделать как в PhysMaterialDB
	pSquads = ^Squads;
	Squads = object(&Object)
	public const
		AllyThreshold  = 3.0;
		EnemyThreshold = -3.0;
		DefaultRelation = 0.0;
	private type
		pPairDesc = ^PairDesc;
		PairDesc = object
			relation: float;
			function Create: PairDesc; static;
			procedure Initialize;
			procedure Finalize;
			function IsDefault: boolean;
		end;

		{$define classname := Name2Pair} {$define key_type := PoolString} {$define value_type := PairDesc}
		{$include hash.h.inc}

		pSquadDesc = ^SquadDesc;
		SquadDesc = object
			pairs: Name2Pair;
			function Create: SquadDesc; static;
			procedure Done;
		end;

		{$define classname := Name2Info} {$define key_type := PoolString} {$define value_type := SquadDesc}
		{$include hash.h.inc}
	private var
		_squads: Name2Info;
		function Get(const name: PoolString): pSquadDesc;
		function Ordered(const a, b: PoolString): boolean;
		function _GetPair(const a, b: PoolString; force: boolean): pPairDesc;
		procedure RemovePair(const a, b: PoolString);
	public
		constructor Init;
		destructor Done; virtual;

		procedure Add(const name: PoolString);
		procedure Remove(const name: PoolString);
		function GetRelation(const a, b: PoolString): float;
		procedure SetRelation(const a, b: PoolString; const newRelation: float);

		function AreEnemies(const a, b: PoolString): boolean;
		function AreAllies(const a, b: PoolString): boolean;
		function AreNeutral(const a, b: PoolString): boolean;
	end;

	procedure OpenScript(var script: ScriptState);

implementation

{$ifdef use_serialization}
uses
	Serialization;
{$endif}

	procedure DonePair(var pair: Squads.PairDesc);
	begin
		pair.Finalize;
	end;

	{$define classname := Squads.Name2Pair} {$define inline_hash := _1.Hash}
	{$include hash.pp.inc}

	{$define classname:=Squads.Name2Info} {$define inline_hash := _1.Hash} {$define finalize_value := _1.Done}
	{$include hash.pp.inc}

	function Squads.PairDesc.Create: PairDesc;
	begin
		result.relation := 0.0; // avoid warning
		result.Initialize;
	end;

	procedure Squads.PairDesc.Initialize;
	begin
		relation := DefaultRelation;
	end;

	procedure Squads.PairDesc.Finalize;
	begin
	end;

	function Squads.PairDesc.IsDefault: boolean;
	begin
		result := Equals(relation, DefaultRelation);
	end;

	function Squads.SquadDesc.Create: SquadDesc;
	begin
		(@result)^.pairs.Init;
	end;

	procedure Squads.SquadDesc.Done;
	begin
		pairs.Done;
	end;

	function Squads.Get(const name: PoolString): pSquadDesc;
	begin
		result := _squads.Find(name);
		if not Assigned(result) then raise Error('Сквада "{0}" не существует.', name);
	end;

	function Squads.Ordered(const a, b: PoolString): boolean;
	begin
		result := a.ToIndex <= b.ToIndex;
	end;

	function Squads._GetPair(const a, b: PoolString; force: boolean): pPairDesc;
	var
		si: pSquadDesc;
	begin
		if not Ordered(a, b) then exit(_GetPair(b, a, force));
		si := Get(a); {$ifdef Debug} Get(b); {$endif}
		result := si^.pairs.Find(b);
		if not Assigned(result) and force then result := si^.pairs.Add(b, PairDesc.Create);
	end;

	procedure Squads.RemovePair(const a, b: PoolString);
	begin
		if not Ordered(a, b) then begin RemovePair(b, a); exit; end;
		if not _squads.Find(a)^.pairs.Remove(b) then Assert(no);
	end;

	constructor Squads.Init;
	begin
		inherited Init;
		_squads.Init;
	end;

	destructor Squads.Done;
	begin
		_squads.Done;
		inherited Done;
	end;

	procedure Squads.Add(const name: PoolString);
	begin
		if Assigned(_squads.Find(name)) then raise Error('Сквад "{0}" уже существует.', name);
		_squads.Add(name, SquadDesc.Create);
	end;

	procedure Squads.Remove(const name: PoolString);
	var
		it: Name2Info.Iterator;
	begin
	{$ifdef Debug} Get(name); {$endif}
		it := _squads.GetIterator;
		while _squads.Next(it) do
			_squads.GetValue(it)^.pairs.Remove(name);
		_squads.Remove(name);
	end;

	function Squads.GetRelation(const a, b: PoolString): float;
	var
		pair: pPairDesc;
	begin
		pair := _GetPair(a, b, no);
		if Assigned(pair) then result := pair^.relation else result := DefaultRelation;
	end;

	procedure Squads.SetRelation(const a, b: PoolString; const newRelation: float);
	var
		pair: pPairDesc;
	begin
		pair := _GetPair(a, b, not Equals(newRelation, DefaultRelation));
		if Assigned(pair) then
		begin
			pair^.relation := newRelation;
			if pair^.IsDefault then RemovePair(a, b);
		end;
	end;

	function Squads.AreEnemies(const a, b: PoolString): boolean;
	begin
		result := GetRelation(a, b) <= EnemyThreshold;
	end;

	function Squads.AreAllies(const a, b: PoolString): boolean;
	begin
		result := GetRelation(a, b) >= AllyThreshold;
	end;

	function Squads.AreNeutral(const a, b: PoolString): boolean;
	var
		relation: float;
	begin
		relation := GetRelation(a, b);
		result := (relation > EnemyThreshold) and (relation < AllyThreshold);
	end;

	procedure Script_CreateSquads(var ss: ScriptState);
	var
		squads: pSquads;
		i: sint;
	begin
		squads := new(pSquads, Init);
		for i := 1 to ss.Top do
			squads^.Add(ss.ToString(i));
		ss.PushObject(squads);

	end;

	procedure Script_Squads_Add(var ss: ScriptState);
	var
		i: sint;
	begin
		for i := 2 to ss.Top do
			pSquads(ss.ToSelf)^.Add(ss.ToString(i));
	end;

	procedure Script_Squads_Remove(var ss: ScriptState);
	var
		i: sint;
	begin
		for i := 2 to ss.Top do
			pSquads(ss.ToSelf)^.Remove(ss.ToString(i));
	end;

	procedure Script_Squads_GetRelation(var ss: ScriptState);
	begin
		ss.PushFloat(pSquads(ss.ToSelf)^.GetRelation(ss.ToString(2), ss.ToString(3)));
	end;

	procedure Script_Squads_SetRelation(var ss: ScriptState);
	begin
		pSquads(ss.ToSelf)^.SetRelation(ss.ToString(2), ss.ToString(3), ss.ToFloat(4));
	end;

	procedure Script_Squads_AreEnemies(var ss: ScriptState);
	begin
		ss.PushBool(pSquads(ss.ToSelf)^.AreEnemies(ss.ToString(2), ss.ToString(3)));
	end;

	procedure Script_Squads_AreAllies(var ss: ScriptState);
	begin
		ss.PushBool(pSquads(ss.ToSelf)^.AreAllies(ss.ToString(2), ss.ToString(3)));
	end;

	procedure Script_Squads_AreNeutral(var ss: ScriptState);
	begin
		ss.PushBool(pSquads(ss.ToSelf)^.AreNeutral(ss.ToString(2), ss.ToString(3)));
	end;

	procedure OpenScript(var script: ScriptState);
	const
		Stuff: array[0 .. 8] of ScriptStuffDesc =
		(
			(s: TypeDesc; p: TypeOf(Squads)),
			(s: 'Add:0'; p: @Script_Squads_Add),
			(s: 'Remove:0'; p: @Script_Squads_Remove),
			(s: 'GetRelation:1'; p: @Script_Squads_GetRelation),
			(s: 'SetRelation:0'; p: @Script_Squads_SetRelation),
			(s: 'AreEnemies:1'; p: @Script_Squads_AreEnemies),
			(s: 'AreAllies:1'; p: @Script_Squads_AreAllies),
			(s: 'AreNeutral:1'; p: @Script_Squads_AreNeutral),

			(s: FunctionsDesc + 'CreateSquads:1'; p: @Script_CreateSquads)
		);
	begin
		script.AddStuff(Stuff);
	end;

{$ifdef use_serialization}
type
	pDeSquadsOne = ^DeSquadsOne;
	DeSquadsOne = record
		name: PoolString;
		pairs: array of record
			name: PoolString;
			relation: float;
		end;
	end;

	pDeSquads = ^DeSquads;
	DeSquads = record
		squads: array of DeSquadsOne;
	end;

	procedure SerializeSquads(se: pSerializer; obj: pointer);
	var
		hsq: ^Squads.Name2Info;
		info: Squads.pSquadDesc;
		it: Squads.Name2Info.Iterator;
		hpair: ^Squads.Name2Pair;
		sit: Squads.Name2Pair.Iterator;
		pair: Squads.pPairDesc;
		squads: pSquads absolute obj;
	begin
		with se^ do
		begin
			hsq := @squads^._squads;
			Serialize_ui8(stream, hsq^.Count);
			it := hsq^.GetIterator;
			while hsq^.Next(it) do
			begin
				Serialize_string(stream, hsq^.GetKey(it)^);
				info := hsq^.GetValue(it);
				hpair := @info^.pairs;
				Serialize_ui8(stream, hpair^.Count);
				sit := hpair^.GetIterator;
				while hpair^.Next(sit) do
				begin
					Serialize_string(stream, hpair^.GetKey(sit)^);
					pair := hpair^.GetValue(sit);
					Serialize_f16(stream, pair^.relation);
				end;
			end;
		end;
	end;

	procedure DeserializeSquads(de: pDeserializer; obj: pointer);
	var
		squads: pSquads absolute obj;
		desq: pDeSquads;
		isq, is2: sint;
		csq: pDeSquadsOne;
	begin
		with de^ do
		begin
			desq := new(pDeSquads);
			pPointer(@squads^._squads)^ := desq;
			SetLength(desq^.squads, Deserialize_ui8(stream));
			for isq := 0 to High(desq^.squads) do
			begin
				csq := @desq^.squads[isq];
				csq^.name := Deserialize_string(stream);
				SetLength(csq^.pairs, Deserialize_ui8(stream));
				for is2 := 0 to High(csq^.pairs) do
				begin
					csq^.pairs[is2].name := Deserialize_string(stream);
					csq^.pairs[is2].relation := Deserialize_f16(stream);
				end;
			end;
		end;
	end;

	procedure SquadsDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		squads: pSquads absolute obj;
		desq: pDeSquads;
		isq, is2: sint;
		csq: pDeSquadsOne;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize:
				begin
					squads^.Init;
					squads^._squads.Done;
				end;
			de_After:
				begin
					desq := pPointer(@squads^._squads)^;
					squads^._squads.Init;
					for isq := 0 to High(desq^.squads) do
						squads^.Add(desq^.squads[isq].name);
					for isq := 0 to High(desq^.squads) do
					begin
						csq := @desq^.squads[isq];
						for is2 := 0 to High(csq^.pairs) do
							squads^.SetRelation(csq^.name, csq^.pairs[is2].name, csq^.pairs[is2].relation);
					end;
					dispose(desq);
				end;
		end;
	end;
{$endif}

	procedure Init;
	begin
	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Squads', TypeOf(Squads), nil, sizeof(Squads), yes,
		               @SerializeSquads, @DeserializeSquads, nil, @SquadsDeSpecial);
	{$endif}
	end;

initialization
	&Unit('Squads').Initialize(@Init);
end.
