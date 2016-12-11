unit Inventories;

{$include opts.inc}
{$ifdef Debug}
	{$define ExtDebug}
{$endif}

interface

uses
	USystem, {$ifdef use_serialization} Errors,{$endif} {$ifdef Debug} ULog, {$endif} Algo, UMath, Random, UClasses, Utils, Script, Streams, GLUtils, Human,
	SceneGraph, Physics;

type
	ItemTrait =
	(
		item_Weapon, item_Consumable, item_Fragile, item_Projectile, item_Readable
	);
	ItemTraits = set of ItemTrait;

	pItem = ^Item;

	pItemMixin = ^ItemMixin;
	ItemMixin = object
		constructor Init;
		destructor Done; virtual;
		procedure Serialize(s: pStream); virtual;
		procedure Deserialize(s: pStream); virtual;
		function Describe(item: pItem): string; virtual;
	end;

	pItemAsWeapon = ^ItemAsWeapon;
	ItemAsWeapon = object(ItemMixin)
		damage: float;
		constructor Init;
		destructor Done; virtual;
		procedure Serialize(s: pStream); virtual;
		procedure Deserialize(s: pStream); virtual;
		function Describe(item: pItem): string; virtual;
	end;

	pItemAsConsumable = ^ItemAsConsumable;
	ItemAsConsumable = object(ItemMixin)
	public type
		KindEnum = (kind_Food, kind_NoRenew, kind_Rechargeable);
	const
		KindIds: array[KindEnum] of string = ('food', 'norenew', 'rechargeable');
		KindPrefixCodes: array[KindEnum] of string = ('f', 'N', 'r');
	private var
		_kind: KindEnum;
		_charges, _maxCharges: uint;
		procedure _SetCharges(newCh: uint);
		procedure _SetMaxCharges(newMax: uint);
		procedure SerializeCurMaxPair(s: pStream; cur, max: uint); static;
		procedure DeserializeCurMaxPair(s: pStream; out cur, max: uint); static;
	public
		constructor Init;
		destructor Done; virtual;
		procedure Serialize(s: pStream); virtual;
		procedure Deserialize(s: pStream); virtual;
		function Describe(item: pItem): string; virtual;

		function CanDrain(n: uint): boolean;
		function Drain(n: uint): boolean;
		function Recharge(by: uint): uint;
		function Infinite: boolean;

		property Kind: KindEnum read _kind write _kind;
		property Charges: uint read _charges write _SetCharges;
		property MaxCharges: uint read _maxCharges write _SetMaxCharges;
	private const
		CURMAX_INFINITE_BIT = 1 shl 0;
		CURMAX_NONIDEAL_BIT = 1 shl 1;
	end;

	pItemAsFragile = ^ItemAsFragile;
	ItemAsFragile = object(ItemMixin)
	private var
		_durability, _maxDurability: uint;
		procedure _SetDurability(newDur: uint);
		procedure _SetMaxDurability(newMax: uint);
	public
		constructor Init;
		destructor Done; virtual;
		procedure Serialize(s: pStream); virtual;
		procedure Deserialize(s: pStream); virtual;
		function Describe(item: pItem): string; virtual;

		function Indestructible: boolean;
		function Damage(amount: uint): uint;
		function Repair(amount: uint): uint;

		property Durability: uint read _durability write _SetDurability;
		property MaxDurability: uint read _maxDurability write _SetMaxDurability;
	end;

	pItemAsProjectile = ^ItemAsProjectile;
	ItemAsProjectile = object(ItemMixin)
	public
		damage: float;
		constructor Init;
		destructor Done; virtual;
		procedure Serialize(s: pStream); virtual;
		procedure Deserialize(s: pStream); virtual;
		function Describe(item: pItem): string; virtual;
	end;

	pItemAsReadable = ^ItemAsReadable;
	ItemAsReadable = object(ItemMixin)
	public
		inscription, text: string;
		constructor Init;
		destructor Done; virtual;
		procedure Serialize(s: pStream); virtual;
		procedure Deserialize(s: pStream); virtual;
		function Describe(item: pItem): string; virtual;
	end;

	pInventory = ^Inventory;

	OnCreateDestroyOutdoorItemProc = procedure(item: pItem; node: pSceneNode; const info: SingleDelegateInfo);
	pOnCreateDestroyOutdoorItemArgs = ^OnCreateDestroyOutdoorItemArgs;
	OnCreateDestroyOutdoorItemArgs = record
		item: pItem;
		node: pSceneNode;
	end;

	Item = object(&Object)
	private type
		FlagEnum = (item_Floating, item_CantDrop);
		FlagSet = set of FlagEnum;

		OutdoorMode = (outdoor_None, outdoor_Levitate, outdoor_Phys);
		OutdoorLevitate = object
			yAmp, yFreq: float;
			rxVel: float;
			procedure Initialize(const newYAmp, newYFreq, newRxVel: float);
			procedure Finalize;
		end;

		OutdoorPhys = object
			prim: ^PoolString;
			mass: float;
			procedure Initialize(const newPrim: PoolString; const newMass: float);
			procedure Finalize;
		end;

		OutdoorDesc = record
		case mode: OutdoorMode of
			outdoor_Levitate: (levitate: OutdoorLevitate);
			outdoor_Phys:     (phys: OutdoorPhys);
		end;
	var
		_id: PoolString;
		_traits: ItemTraits;
		_mask: Bitfield2D;
		_sprite: pAtlasAnimation; // TODO: Это очень временное решение.
		                          // Нужна функция, которая будет вызываться САМИМ ПРЕДМЕТОМ (чтобы не дёргать каждый кадр) при
		                          // (возможной) смене спрайта.
		                          // И вообще как-нибудь красиво сделать опциональную зависимость спрайта от параметров.
		                          // UPD: Лол. Нет ничего более постоянного, чем временное решение. Понадобится — исправлю.
		traitsData: UClasses.Blob;
		_flags: FlagSet;
		_inventoryID: sint;
		_radius: float;
		_outdoor: OutdoorDesc;
		_name, _desc: string;
		function GetFlag(flag: FlagEnum): boolean;
		procedure SetFlag(flag: FlagEnum; value: boolean);
		procedure FillTraitsBlob(const traits: ItemTraits);
		procedure SetSprite(newSprite: pAtlasAnimation);
		function GetName: string;
	private
		function GetAs(trait: ItemTrait): pointer;
		function GetAsWeapon:     pItemAsWeapon;
		function GetAsConsumable: pItemAsConsumable;
		function GetAsFragile:    pItemAsFragile;
		function GetAsProjectile: pItemAsProjectile;
		function GetAsReadable:   pItemAsReadable;
	public
		constructor Init(const newId: PoolString; newTraits: ItemTraits; const newMask: string);
		constructor Init(const newMask: string);
		destructor Done; virtual;
		function Mask: pBitfield2D;
		procedure SetOutdoorLevitate(const newYAmp, newYFreq, newRxVel: float);
		procedure SetOutdoorPhys(const newPrim: PoolString; const newMass: float);
		function Describe: string;

		property ID: PoolString read _id;
		property Name: string read GetName write _name;
		property Desc: string read _desc write _desc;
		property Radius: float read _radius write _radius;
		property Sprite: pAtlasAnimation read _sprite write SetSprite;
		property CantDrop: boolean index item_CantDrop read GetFlag write SetFlag;
		property Floating: boolean index item_Floating read GetFlag;
	public
		property Traits: ItemTraits read _traits;
		property AsWeapon:     pItemAsWeapon     read GetAsWeapon;
		property AsConsumable: pItemAsConsumable read GetAsConsumable;
		property AsFragile:    pItemAsFragile    read GetAsFragile;
		property AsProjectile: pItemAsProjectile read GetAsProjectile;
		property AsReadable:   pItemAsReadable   read GetAsReadable;
	const
		OutdoorModeIds: array[OutdoorMode] of string = ('none', 'levitate', 'phys');
	end;

	pOutdoorItem = ^OutdoorItem;

	Inventory = object(&Object)
	public type
		pItemRec = ^tItemRec;
		tItemRec = record
			pos: UintVec2;
			item: pItem;
		end;
		tFloatingMode = (floating_FromThis, floating_FromOutdoor);
		tOnStopFloat = procedure(inv: pInventory; item: pItem; param: pointer);

		pFloatingRec = ^tFloatingRec;
		tFloatingRec = record
			item: pItem;
			onStop: tOnStopFloat;
			onStopParam: pointer;
		case mode: tFloatingMode of
			floating_FromThis: (invPos: UintVec2);
			floating_FromOutdoor: (outdoor_wtf: Transform);
		end;
		tAddCallback = procedure(inv: pInventory; item: pItem; param: pointer);
		tRemoveCallback = procedure(inv: pInventory; item: pItem; param: pointer);
		tScrewCallback = procedure(inv: pInventory; param: pointer);

	private type
		ViewCallbacks = object
			add: tAddCallback;
			remove: tRemoveCallback;
			screw: tScrewCallback;
			userParam: pointer;
			procedure Clear;
		end;
	private var
		_floating: array of tFloatingRec;
		_view: ViewCallbacks;
		_mask: Bitfield2D;
		_owner: pSceneNode;
		function _GetSize: UintVec2;
		function _GetScene: pSceneNode;
	public
		items: array of tItemRec;
		constructor Init(const newSize: UintVec2);
		destructor Done; virtual;
		function Locate(item: pItem): pItemRec;
		function Fits(item: pItem; const pos: UintVec2): boolean;
		function Fits(item: pItem): boolean;
		function FindPlace(item: pItem; out pos: UintVec2): boolean;
		function Add(newItem: pItem; const pos: UintVec2): boolean;
		function Add(newItem: pItem): boolean;
		procedure PickFloating(oitem: pOutdoorItem; onStop: tOnStopFloat = nil; onStopParam: pointer = nil);
		function ItemAt(const pos: UintVec2): pItem;
		procedure Remove(item: pItem);
		procedure StartFloat(item: pItem; onStop: tOnStopFloat = nil; onStopParam: pointer = nil);
		procedure StopFloat(item: pItem; away: boolean);
		procedure SetViewCallbacks(newAdd: tAddCallback; newRemove: tRemoveCallback; newScrew: tScrewCallback; newParam: pointer);
		procedure ResetViewCallbacks;
		function Drop(item: pItem; worldTf: pTransform; worldVel: pVec3; force: boolean; disCollide: pRigidBody = nil): boolean;
		procedure Screw(const localBase, localCast: Vec3; disCollide: pRigidBody = nil);

		property Owner: pSceneNode read _owner write _owner;
		property Size: UintVec2 read _GetSize;
	end;

	OutdoorItem = object(RigidBody)
	private const
		PhaseCycle = 4000.0;
	private var
		_item: pItem;
		_phase: float;
		_baseTransform: Transform;
		_baseTransformLock: boolean;
	protected
		procedure _OnApplyTransform; virtual;
		function _SuitsTo(know: SceneKnowledge): boolean; virtual;
		procedure _OnUpdate(const dt: float); virtual;
	public
		constructor Init(newItem: pItem);
		destructor Done; virtual;
		property Item: pItem read _item;
	end;

const
	ItemTraitIds: array[ItemTrait] of string = ('weapon', 'consumable', 'fragile', 'projectile', 'readable');

	procedure OpenScript(var script: ScriptState);

implementation

uses
	GUI, Scene
{$ifdef use_serialization}, Serialization {$endif};

const
	ItemTraitInfo: array[ItemTrait] of record
		sizeof: size_t;
		typeof: pointer;
	end =
	(
		(sizeof: sizeof(ItemAsWeapon);     typeof: typeof(ItemAsWeapon)),
		(sizeof: sizeof(ItemAsConsumable); typeof: typeof(ItemAsConsumable)),
		(sizeof: sizeof(ItemAsFragile);    typeof: typeof(ItemAsFragile)),
		(sizeof: sizeof(ItemAsProjectile); typeof: typeof(ItemAsProjectile)),
		(sizeof: sizeof(ItemAsReadable);   typeof: typeof(ItemAsReadable))
	);

	constructor ItemMixin.Init;
	begin
	end;

	destructor ItemMixin.Done;
	begin
	end;

	procedure ItemMixin.Serialize(s: pStream);
	begin
		Assert(@s = @s);
	end;

	procedure ItemMixin.Deserialize(s: pStream);
	begin
		Assert(@s = @s);
	end;

	function ItemMixin.Describe(item: pItem): string;
	begin
		Assert(@item = @item);
		result := '';
	end;

	constructor ItemAsWeapon.Init;
	begin
		inherited Init;
		damage := 0.0;
	end;

	destructor ItemAsWeapon.Done;
	begin
		inherited Done;
	end;

	procedure ItemAsWeapon.Serialize(s: pStream);
	begin
		Serialize_f32(s, damage);
	end;

	procedure ItemAsWeapon.Deserialize(s: pStream);
	begin
		damage := Deserialize_f32(s);
	end;

	function ItemAsWeapon.Describe(item: pItem): string;
	begin
		Assert(@item = @item);
		result := Locale.Localized('items.mix.wpn');
	end;

	constructor ItemAsConsumable.Init;
	begin
		inherited Init;
		_charges := 0;
		_maxCharges := High(_maxCharges);
		_kind := kind_Rechargeable;
	end;

	destructor ItemAsConsumable.Done;
	begin
		inherited Done;
	end;

	procedure ItemAsConsumable.SerializeCurMaxPair(s: pStream; cur, max: uint);
	var
		c2: uint;
	begin
		c2 := cur shl 2;
		if max = High(max) then c2 := c2 or CURMAX_INFINITE_BIT else
			if cur < max then c2 := c2 or CURMAX_NONIDEAL_BIT;
		VarInt.Write(s, c2);
		if (c2 and CURMAX_NONIDEAL_BIT) <> 0 then
			VarInt.Write(s, max);
	end;

	procedure ItemAsConsumable.DeserializeCurMaxPair(s: pStream; out cur, max: uint);
	var
		c2: uint;
	begin
		c2 := VarInt.Read(s);
		cur := c2 shr 2;
		if (c2 and CURMAX_INFINITE_BIT) <> 0 then max := High(max) else
			if (c2 and CURMAX_NONIDEAL_BIT) = 0 then max := cur else
				max := VarInt.Read(s);
	end;

	procedure ItemAsConsumable.Serialize(s: pStream);
	begin
		Serialize_enum(s, ord(_kind), KindPrefixCodes);
		SerializeCurMaxPair(s, _charges, _maxCharges);
	end;

	procedure ItemAsConsumable.Deserialize(s: pStream);
	begin
		_kind := KindEnum(Deserialize_enum(s, KindPrefixCodes));
		DeserializeCurMaxPair(s, _charges, _maxCharges);
	end;

	function ItemAsConsumable.Describe(item: pItem): string;
	begin
		Assert(@item = @item);
		case _kind of
			kind_Food:
				if Infinite then result := Locale.Localized('items.mix.cons.food.inf') else
					if _charges > 0 then
					begin
						result := Locale.Localized('items.mix.cons.food');
						if _charges > 1 then result += ' (' + ToString(_charges) + ')';
						result += '.';
					end else
						result := '';
			kind_NoRenew:
				if Infinite then result := Locale.Localized('items.mix.cons.inf') else
					if _charges > 0 then
					begin
						result := Locale.Localized('items.mix.cons');
						if _charges > 1 then result += ' (' + ToString(_charges) + ')';
						result += '.';
					end else
						result := '';
			kind_Rechargeable:
					if Infinite then result := Locale.Localized('items.mix.cons.charge.inf') else
						if _charges = 0 then result := Locale.Localized('items.mix.cons.charge.0') else
							if _charges = 1 then result := Locale.Localized('items.mix.cons.charge.1') else
								if _charges = _maxCharges then
									result := Locale.Localized('items.mix.cons.charge.full') + ' (' + ToString(_charges) + ')'
								else
									result := Locale.Localized('items.mix.cons.charge') + ': ' + ToString(_charges) + ' / ' + ToString(_maxCharges);
			else Assert(no);
		end;
	end;

	procedure ItemAsConsumable._SetCharges(newCh: uint);
	begin
		if not Infinite then _charges := min(newCh, _maxCharges);
	end;

	procedure ItemAsConsumable._SetMaxCharges(newMax: uint);
	begin
		if _charges > newMax then _charges := newMax;
		_maxCharges := newMax;
		if Infinite then _charges := 0;
	end;

	function ItemAsConsumable.CanDrain(n: uint): boolean;
	begin
		result := Infinite or (n >= _charges);
	end;

	function ItemAsConsumable.Drain(n: uint): boolean;
	begin
		result := CanDrain(n);
		if result and not Infinite then
			_charges -= n;
	end;

	function ItemAsConsumable.Recharge(by: uint): uint;
	begin
		if Infinite then
			result := by
		else
			if _charges + by <= _maxCharges then
			begin
				_charges += by;
				result := 0;
			end else
			begin
				result := by - (_maxCharges - _charges);
				_charges := _maxCharges;
			end;
	end;

	function ItemAsConsumable.Infinite: boolean;
	begin
		result := _maxCharges = High(_maxCharges);
	end;

	constructor ItemAsFragile.Init;
	begin
		inherited Init;
		_durability := 0;
		_maxDurability := High(_maxDurability);
	end;

	destructor ItemAsFragile.Done;
	begin
		inherited Done;
	end;

	procedure ItemAsFragile.Serialize(s: pStream);
	begin
		ItemAsConsumable.SerializeCurMaxPair(s, _durability, _maxDurability);
	end;

	procedure ItemAsFragile.Deserialize(s: pStream);
	begin
		ItemAsConsumable.DeserializeCurMaxPair(s, _durability, _maxDurability);
	end;

	function ItemAsFragile.Describe(item: pItem): string;
	begin
		Assert(@item = @item);
		if Indestructible then result := Locale.Localized('items.mix.fragile.indestructible') else
			result := Locale.Localized('items.mix.fragile') + ': ' + ToString(100.0 * _durability / max(_maxDurability, 1)) + '%.';
	end;

	function ItemAsFragile.Indestructible: boolean;
	begin
		result := _maxDurability = High(_maxDurability);
	end;

	function ItemAsFragile.Damage(amount: uint): uint;
	begin
		if Indestructible then result := amount else
			if amount <= _durability then
			begin
				_durability -= amount;
				result := 0;
			end else
			begin
				result := _durability;
				_durability := 0;
			end;
	end;

	function ItemAsFragile.Repair(amount: uint): uint;
	begin
		if Indestructible then result := amount else
			if _durability + amount <= _maxDurability then
			begin
				_durability += amount;
				result := 0;
			end else
			begin
				result := amount - (_maxDurability - _durability);
				_durability := _maxDurability;
			end;
	end;

	procedure ItemAsFragile._SetDurability(newDur: uint);
	begin
		if not Indestructible then _durability := min(newDur, _maxDurability);
	end;

	procedure ItemAsFragile._SetMaxDurability(newMax: uint);
	begin
		if (_durability > newMax) or (_maxDurability = High(_maxDurability)) then
			_durability := newMax;
		_maxDurability := newMax;
	end;

	constructor ItemAsProjectile.Init;
	begin
		inherited Init;
		damage := 0.0;
	end;

	destructor ItemAsProjectile.Done;
	begin
		inherited Done;
	end;

	procedure ItemAsProjectile.Serialize(s: pStream);
	begin
		Serialize_f32(s, damage);
	end;

	procedure ItemAsProjectile.Deserialize(s: pStream);
	begin
		damage := Deserialize_f32(s);
	end;

	function ItemAsProjectile.Describe(item: pItem): string;
	begin
		Assert(@item = @item);
		result := Locale.Localized('items.mix.projectile');
	end;

	constructor ItemAsReadable.Init;
	begin
		inherited Init;
		inscription := '';
		text := '';
	end;

	destructor ItemAsReadable.Done;
	begin
		inherited Done;
	end;

	procedure ItemAsReadable.Serialize(s: pStream);
	begin
		Serialize_string(s, inscription);
		Serialize_string(s, text);
	end;

	procedure ItemAsReadable.Deserialize(s: pStream);
	begin
		inscription := Deserialize_string(s);
		text := Deserialize_string(s);
	end;

	function ItemAsReadable.Describe(item: pItem): string;
	begin
		Assert(@item = @item);
		if inscription <> '' then result := Locale.Localized('items.mix.readable.inscription') + ': "' + inscription + '". ' else result := '';
		result += Locale.Localized('items.mix.readable');
	end;

	procedure Item.OutdoorLevitate.Initialize(const newYAmp, newYFreq, newRxVel: float);
	begin
		yAmp := newYAmp;
		yFreq := newYFreq;
		rxVel := newRxVel;
	end;

	procedure Item.OutdoorLevitate.Finalize;
	begin
	end;

	procedure Item.OutdoorPhys.Initialize(const newPrim: PoolString; const newMass: float);
	begin
		new(prim);
		prim^ := newPrim;
		mass := newMass;
	end;

	procedure Item.OutdoorPhys.Finalize;
	begin
		dispose(prim);
	end;

	procedure DestroyTrait(var trait);
	begin
		ItemMixin(trait).Done;
	end;

	function TraitSize(var trait): size_t;
	begin
		result := sizeof(ItemMixin(trait));
	end;

	constructor Item.Init(const newId: PoolString; newTraits: ItemTraits; const newMask: string);
	begin
		inherited Init;
		_id := newId;
		_traits := newTraits;
		_mask.Init(newMask);
		_sprite := nil;
		_outdoor.mode := outdoor_None;
		_radius := 1.0;

		traitsData.Init;
		FillTraitsBlob(_traits);
		_flags := [];
		_inventoryID := -1;
	end;

	constructor Item.Init(const newMask: string);
	begin
		Init('', [], newMask);
	end;

	destructor Item.Done;
	begin
		case _outdoor.mode of
			outdoor_None: ;
			outdoor_Levitate: _outdoor.levitate.Finalize;
			outdoor_Phys: _outdoor.phys.Finalize;
		{$ifdef Debug} else Assert(no); {$endif}
		end;
		traitsData.Done(@DestroyTrait, @TraitSize);
		Release(_sprite);
		_mask.Done;
		inherited Done;
	end;

	function Item.Mask: pBitfield2D;
	begin
		result := @_mask;
	end;

	procedure Item.SetOutdoorLevitate(const newYAmp, newYFreq, newRxVel: float);
	begin
		Assert(_outdoor.mode = outdoor_None);
		_outdoor.mode := outdoor_Levitate;
		_outdoor.levitate.Initialize(newYAmp, newYFreq, newRxVel);
	{$ifdef ExtDebug}
		Log('Воплощение предмета "' + _id + '" во внешнем мире — парит в воздухе. YAmp = ' + ToString(newYAmp) + ', YFreq = ' +
			ToString(newYFreq) + ', RxVel = ' + ToString(newRxVel), logDebug);
	{$endif}
	end;

	procedure Item.SetOutdoorPhys(const newPrim: PoolString; const newMass: float);
	begin
		Assert(_outdoor.mode = outdoor_None);
		_outdoor.mode := outdoor_Phys;
		_outdoor.phys.Initialize(newPrim, newMass);
	{$ifdef ExtDebug}
		Log('Воплощение предмета "' + _id + '" во внешнем мире — физическое тело. Примитив — ' + StreamPath.Log(newPrim) +
			', масса = ' + ToString(newMass), logDebug);
	{$endif}
	end;

	function Item.Describe: string;
	var
		t: string;
		trait: pointer;
	begin
		result := _desc;
		trait := nil;
		while traitsData.Next(trait, @TraitSize) do
		begin
			t := pItemMixin(trait)^.Describe(@self);
			if t <> '' then
			begin
				if result <> '' then result += EOL + EOL;
				result += t;
			end;
		end;
	end;

	function Item.GetFlag(flag: FlagEnum): boolean;
	begin
		result := flag in _flags;
	end;

	procedure Item.SetFlag(flag: FlagEnum; value: boolean);
	begin
		if value <> (flag in _flags) then
			if value then
				Include(_flags, flag)
			else
				Exclude(_flags, flag);
	end;

	procedure Item.FillTraitsBlob(const traits: ItemTraits);
	var
		trait: ItemTrait;
		cur: pointer;
	begin
		for trait in ItemTrait do
			if trait in traits then
			begin
				cur := traitsData.Add(ItemTraitInfo[trait].sizeof);
				case trait of
					item_Weapon:         pItemAsWeapon(cur)^.Init;
					item_Consumable: pItemAsConsumable(cur)^.Init;
					item_Fragile:       pItemAsFragile(cur)^.Init;
					item_Projectile: pItemAsProjectile(cur)^.Init;
					item_Readable:     pItemAsReadable(cur)^.Init;
					else
						Assert(no);
				end;
			end;
	end;

	procedure Item.SetSprite(newSprite: pAtlasAnimation);
	begin
		SetRef(_sprite, newSprite);
	end;

	function Item.GetName: string;
	begin
		if _name <> '' then result := _name else result := _id;
	end;

	function SameType(var trait; ty: pointer): boolean;
	begin
		result := TypeOf(ItemMixin(trait)) = ty;
	end;

	function Item.GetAs(trait: ItemTrait): pointer;
	begin
		result := traitsData.Find(@TraitSize, @SameType, ItemTraitInfo[trait].typeof);
	end;

	function Item.GetAsWeapon: pItemAsWeapon;         begin result := GetAs(item_Weapon); end;
	function Item.GetAsConsumable: pItemAsConsumable; begin result := GetAs(item_Consumable); end;
	function Item.GetAsFragile: pItemAsFragile;       begin result := GetAs(item_Fragile); end;
	function Item.GetAsProjectile: pItemAsProjectile; begin result := GetAs(item_Projectile); end;
	function Item.GetAsReadable: pItemAsReadable;     begin result := GetAs(item_Readable); end;

	procedure Inventory.ViewCallbacks.Clear;
	begin
		add := nil;
		remove := nil;
		screw := nil;
		userParam := nil;
	end;

	constructor Inventory.Init(const newSize: UintVec2);
	begin
		inherited Init;
		_mask.Init(newSize);
		items := nil;
		_view.Clear;
		_owner := nil;
	end;

	destructor Inventory.Done;
	var
		i: sint;
	begin
		_mask.Done;
		for i := 0 to High(_floating) do
			Release(_floating[i].item);
		for i := 0 to High(items) do
			Release(items[i].item);
		items := nil;
		inherited Done;
	end;

	function Inventory.Locate(item: pItem): pItemRec;
	begin
		result := @items[item^._inventoryId];
		Assert(result^.item = item);
	end;

	function Inventory.Fits(item: pItem; const pos: UintVec2): boolean;
	var
		f: pFloatingRec;
	begin
		if not Assigned(item) then exit(no);
		result := _mask.FitsWithOnes(item^.mask^, pos);

		// "Плавающий" предмет мог стать препятствием для самого себя. Внимание,
		// если он стал препятствием для другого "плавающего" — это нормально (иначе они рискуют не вернуться)
		// но это редкий случай, поэтому не стоит проверять сразу же.
		if (not result) and (item_Floating in item^._flags) and (item^._inventoryID < length(_floating)) then
		begin
			f := @_floating[item^._inventoryID];
			if (f^.item = item) and (f^.mode = floating_FromThis) then
			begin
				_mask.InplaceAndNot(item^._mask, f^.invPos);
				result := _mask.FitsWithOnes(item^._mask, pos);
				_mask.InplaceOr(item^._mask, f^.invPos);
			end;
		end;
	end;

	function Inventory.Fits(item: pItem): boolean;
	var
		pos: UintVec2;
	begin
		result := Assigned(item) and FindPlace(item, pos);
	end;

	// обход:
	// 1  2  4  7
	// 3  5  8  10
	// 6  9  11 12
	// возможно, сделаю метод обхода параметром и добавлю оценку вариантов
	function Inventory.FindPlace(item: pItem; out pos: UintVec2): boolean;
	var
		start, cur, edge: UintVec2;
	begin
		start := UintVec2.Zero;
		repeat
			cur := start;
			// влево-вниз, пока возможно
			while cur.y + item^._mask.size.y <= _mask.size.y do
			begin
				if Fits(item, cur) then
				begin
					pos := cur;
					exit(yes);
				end;

				if cur.x = 0 then break;
				cur += IntVec2.Make(-1, 1);
			end;

			edge := start + item^._mask.size;
			if edge.x < _mask.size.x then start.data[0] += 1 else start.data[1] += 1;
		until edge.y > _mask.size.y;
		result := no;
	end;

	function Inventory.Add(newItem: pItem; const pos: UintVec2): boolean;
	label _finally_;
	var
		id: sint;
	begin
		if not Assigned(newItem) then exit(no);
		Assert(newItem^._inventoryID = -1);
		result := Fits(MakeRef(newItem), pos);
		if not result then goto _finally_;

		id := length(items);
		SetLength(items, id + 1);
		items[id].item := MakeRef(newItem);
		items[id].pos := pos;
		newItem^._inventoryId := id;
		_mask.InplaceOr(newItem^.mask^, pos);
		if Assigned(_view.add) then
			_view.add(@self, newItem, _view.userParam);
	_finally_:
		Release(newItem);
	end;

	function Inventory.Add(newItem: pItem): boolean;
	var
		pos: UintVec2;
	begin
		result := FindPlace(newItem, pos) and Add(newItem, pos);
	end;

	procedure Inventory.PickFloating(oitem: pOutdoorItem; onStop: tOnStopFloat = nil; onStopParam: pointer = nil);
	var
		nid: sint;
	begin
		if not Assigned(oitem) then exit;
		Assert(Assigned(_GetScene));
		Assert(_GetScene = oitem^.Root);

		nid := length(_floating);
		SetLength(_floating, nid + 1);
		_floating[nid].onStop := onStop;
		_floating[nid].onStopParam := onStopParam;
		with _floating[nid] do
		begin
			item := MakeRef(oitem^.item);
			item^._inventoryId := nid;
			Include(item^._flags, item_Floating);
			mode := floating_FromOutdoor;
			outdoor_wtf := oitem^.GlobalTransform;
			pScene(oitem^.Root)^.DestroyOutdoorItem(oitem);
		end;
	end;

	function Inventory.ItemAt(const pos: UintVec2): pItem;
	var
		i: sint;
		local: UintVec2;
	begin
		for i := 0 to High(items) do
			if (pos.x >= items[i].pos.x) and (pos.y >= items[i].pos.y) then
			begin
				local := pos - items[i].pos;
				if items[i].item^.mask^.ValidatePoint(local) and items[i].item^.mask^.Bit(local) then
					exit(items[i].item);
			end;
		result := nil;
	end;

	procedure Inventory.Remove(item: pItem);
	var
		id: sint;
	begin
		if not Assigned(item) then exit;
		if item_Floating in item^._flags then
			StopFloat(item, yes)
		else
		begin
			id := item^._inventoryId;
			item^._inventoryId := -1;
			Assert(items[id].item = item);
			_mask.InplaceAndNot(item^.mask^, items[id].pos);
			if Assigned(_view.remove) then
				_view.remove(@self, item, _view.userParam);
			Release(items[id].item);
			if id <> High(items) then
			begin
				items[id] := items[High(items)];
				items[id].item^._inventoryId := id;
			end;
			SetLength(items, length(items) - 1);
		end;
	end;

	procedure Inventory.StartFloat(item: pItem; onStop: tOnStopFloat = nil; onStopParam: pointer = nil);
	var
		id, nid: sint;
	begin
		Assert((not (item_Floating in item^._flags)) and (items[item^._inventoryId].item = item));
		id := item^._inventoryId;
		Include(item^._flags, item_Floating);

		nid := length(_floating);
		item^._inventoryId := nid;
		SetLength(_floating, nid + 1);
		_floating[nid].mode := floating_FromThis;
		_floating[nid].onStop := onStop;
		_floating[nid].onStopParam := onStopParam;
		_floating[nid].item := items[id].item;
		_floating[nid].invPos := items[id].pos;

		if id <> High(items) then
		begin
			items[id] := items[High(items)];
			items[id].item^._inventoryId := id;
		end;
		SetLength(items, length(items) - 1);

		if Assigned(_view.remove) then
			_view.remove(@self, item, _view.userParam);
	end;

	procedure Inventory.StopFloat(item: pItem; away: boolean);
	var
		id, nid: sint;
	begin
		Assert(item_Floating in item^._flags);
		id := item^._inventoryId;
		Assert(_floating[id].item = item);
		if Assigned(_floating[id].onStop) then
			_floating[id].onStop(@self, item, _floating[id].onStopParam);

		if away then
		begin
			if _floating[id].mode = floating_FromThis then
				_mask.InplaceAndNot(item^.mask^, _floating[id].invPos);
			Exclude(item^._flags, item_Floating);
			item^._inventoryId := -1;
			Release(_floating[id].item);
			if id <> High(_floating) then
			begin
				_floating[id] := _floating[High(_floating)];
				_floating[id].item^._inventoryId := id;
			end;
			SetLength(_floating, length(_floating) - 1);
		end else
			case _floating[id].mode of
				floating_FromThis:
					begin
						nid := length(items);
						item^._inventoryId := nid;
						SetLength(items, nid + 1);
						items[nid].item := _floating[id].item;
						items[nid].pos := _floating[id].invPos;
						if id <> High(_floating) then
						begin
							_floating[id] := _floating[High(_floating)];
							_floating[id].item^._inventoryId := id;
						end;
						SetLength(_floating, length(_floating) - 1);
						Exclude(item^._flags, item_Floating);

						if (not away) and Assigned(_view.add) then
							_view.add(@self, item, _view.userParam);
					end;
				floating_FromOutdoor: Drop(item, @_floating[id].outdoor_wtf, nil, yes);
				else
					Assert(no);
			end;
	end;

	procedure Inventory.SetViewCallbacks(newAdd: tAddCallback; newRemove: tRemoveCallback; newScrew: tScrewCallback; newParam: pointer);
	begin
		with _view do
		begin
			add := newAdd;
			remove := newRemove;
			screw := newScrew;
			userParam := newParam;
		end;
	end;

	procedure Inventory.ResetViewCallbacks;
	begin
		_view.Clear;
	end;

	function Inventory.Drop(item: pItem; worldTf: pTransform; worldVel: pVec3; force: boolean; disCollide: pRigidBody = nil): boolean;
	label _finally_;
	var
		outdoor: pOutdoorItem;
		scene: pScene;
		tmpTf: Transform;
	begin
		result := no;
		if item^.CantDrop and not force then goto _finally_;
		scene := pScene(_GetScene);
		if not Assigned(scene) then
		begin
		{$ifdef Debug} Log('Выбросить предмет невозможно: владелец не задан либо отсутствует в сцене', logError); {$endif}
			goto _finally_;
		end;
		if not Assigned(worldTf) then
		begin
			tmpTf := _owner^.GlobalTransform;
			worldTf := @tmpTf;
		end;
		outdoor := scene^.PlaceOutdoorItem(item, worldTf^, worldVel);
		result  := Assigned(outdoor);
		if result and Assigned(disCollide) then
			outdoor^.Interaction(disCollide)^.Collidable := no;
	_finally_:
		if result or force then
		begin
			result := yes;
			Remove(item);
		end;
	end;

	procedure Inventory.Screw(const localBase, localCast: Vec3; disCollide: pRigidBody = nil);
	var
		scene: pScene;
		base, cast, ncast, pos: Vec3;
		tf: Transform;
		item: pItem;
		i: sint;
		radiuses: array of float;
		centers: array of Vec2;
	begin
		scene := pScene(_GetScene); Assert(Assigned(scene));
		for i := High(_floating) downto 0 do
			StopFloat(_floating[i].item, no);
		if length(items) = 0 then exit;

		base := _owner^.GlobalTransform * localBase;
	{$ifdef Debug} Log('Основа для выбрасывания предметов: ' + ToString(base) + '.'); {$endif}
		cast := _owner^.WorldRot * localCast;
		ncast := cast.Normalized;

		SetLength(radiuses, length(items));
		SetLength(centers, length(items));
		for i := 0 to High(items) do
			radiuses[i] := items[i].item^.radius;
		PackCircles(length(items), pointer(radiuses), pointer(centers));

		for i := High(items) downto 0 do
		begin
			item := MakeRef(items[i].item);
			pos := base + Vec3.Make(centers[i].x, 0.0, centers[i].y);
			pos := scene^.RayCast(cast_ForItem, pos, pos + cast) - ncast * item^.radius;
			tf := Translate(pos);
			Drop(item, @tf, nil, yes, disCollide);
		{$ifdef Debug} Log('Предмет "' + item^.id + '" выброшен в (' + ToString(pos) + ')'); {$endif}
			Release(item);
		end;
		if Assigned(_view.screw) then _view.screw(@self, _view.userParam);
	end;

	function Inventory._GetSize: UintVec2;
	begin
		result := _mask.size;
	end;

	function Inventory._GetScene: pSceneNode;
	begin
		if Assigned(_owner) and Assigned(_owner^.Root) then
			result := _owner^.Root
		else
			result := nil;
	end;

	procedure OutdoorItem._OnApplyTransform;
	begin
		inherited _OnApplyTransform;
		if not _baseTransformLock then
			_baseTransform := LocalTransform;
	end;

	function OutdoorItem._SuitsTo(know: SceneKnowledge): boolean;
	begin
		case know of
			scene_Update: result := (_item^._outdoor.mode = outdoor_Levitate) or inherited _SuitsTo(know);
			else
				result := inherited _SuitsTo(know);
		end;
	end;

	procedure OutdoorItem._OnUpdate(const dt: float);
	begin
		case _item^._outdoor.mode of
			outdoor_Levitate:
				begin
					_phase := modf(_phase + dt, PhaseCycle);
					_baseTransformLock := yes;
					with _item^._outdoor.levitate do
						LocalTransform := _baseTransform * Translate(0.0, yAmp * (0.5 + 0.5 * sin(_phase * yFreq)), 0.0) * Rotate(_phase * rxVel, Vec3.PositiveY);
					_baseTransformLock := no;
				end;
		end;
	end;

	constructor OutdoorItem.Init(newItem: pItem);
	label _Fail_;
	var
		newPrim: pRigidPrimitive;
	begin
		_item := MakeRef(newItem);
		if not Assigned(_item) then goto _Fail_;

		with newItem^._outdoor do
		begin
			case mode of
				outdoor_None: goto _Fail_;
				outdoor_Phys:
					begin
						newPrim := ResourcePool.Shared^.LoadRef(TypeOf(RigidPrimitive), phys.prim^);
						if Assigned(newPrim) then
						begin
							inherited Init(newPrim, phys.mass);
							Release(newPrim);
						end else
							goto _Fail_;
					end;
				outdoor_Levitate:
					begin
						inherited InitPhantom(new(pRigidPrimitive, InitSphere(_item^.radius, Transform.Identity, no)));
						_phase := GlobalRNG.GetFloat(TwoPi);
					end;
			end;
		end;
		exit;
	_Fail_:
		Release(_item);
		Fail;
	end;

	destructor OutdoorItem.Done;
	begin
		Release(_item);
		inherited Done;
	end;

	procedure ReadAsWeapon(var trait: ItemMixin; var ss: ScriptState);
	var
		wpn: ItemAsWeapon absolute trait;
	begin
		wpn.damage := ss.GetFloatField(-1, 'damage', wpn.damage);
	end;

	procedure ReadAsConsumable(var trait: ItemMixin; var ss: ScriptState);
	var
		cons: ItemAsConsumable absolute trait;
	begin
		cons.Kind := ItemAsConsumable.KindEnum(FindStr(ss.GetStringField(-1, 'kind', ItemAsConsumable.KindIds[cons.Kind]), ItemAsConsumable.KindIds, ord(cons.Kind)));
		if ss.GetTableS(-1, 'charges') then
		begin
			cons.MaxCharges := ss.ToSint(-1);
			cons.Charges    := cons.MaxCharges;
			ss.Pop;
		end;
		if ss.GetTableS(-1, 'max_charges') then
		begin
			cons.MaxCharges := ss.ToSint(-1);
			ss.Pop;
		end;
	end;

	procedure ReadAsFragile(var trait: ItemMixin; var ss: ScriptState);
	var
		frag: ItemAsFragile absolute trait;
	begin
		if ss.GetTableS(-1, 'dur') then
		begin
			frag.MaxDurability := ss.ToSint(-1);
			frag.Durability := frag.MaxDurability;
			ss.Pop;
		end;
		if ss.GetTableS(-1, 'max_dur') then
		begin
			frag.MaxDurability := ss.ToSint(-1);
			ss.Pop;
		end;
	end;

	procedure ReadAsProjectile(var trait: ItemMixin; var ss: ScriptState);
	var
		proj: ItemAsProjectile absolute trait;
	begin
		proj.damage := ss.GetFloatField(-1, 'damage', proj.damage);
	end;

	procedure ReadAsReadable(var trait: ItemMixin; var ss: ScriptState);
	var
		rd: ItemAsReadable absolute trait;
	begin
		rd.inscription := ss.GetStringField(-1, 'inscription', rd.inscription);
		rd.text        := ss.GetStringField(-1, 'text', rd.text);
	end;

const
	ReadAs: array[ItemTrait] of procedure(var trait: ItemMixin; var ss: ScriptState) =
	(
		@ReadAsWeapon, @ReadAsConsumable, @ReadAsFragile, @ReadAsProjectile, @ReadAsReadable
	);

	procedure Script_CreateItem(var ss: ScriptState);
	var
		item: pItem;
		trait: ItemTrait;
		traits: ItemTraits;
		primitive: string;
		mode: Inventories.Item.OutdoorMode;
	begin
		traits := [];
		for trait in ItemTraits do
			if ss.GetBoolField(1, ItemTraitIds[trait]) then
				Include(traits, trait);
		item := new(pItem, Init(ss.GetStringField(1, 'id'), traits, ss.GetStringField(1, 'mask')));

		if ss.GetTableS(1, 'sprite') then
		begin
			item^.sprite := Script_create_animation(ss, -1, 1);
			ss.Pop;
		end;

		if ss.GetTableS(1, 'outdoor') then
		begin
			item^.radius := ss.GetFloatField(-1, 'radius', item^.radius);
			for mode in Inventories.Item.OutdoorMode do
				if ss.GetTableS(-1, Inventories.Item.OutdoorModeIds[mode]) then
				begin
					case mode of
						outdoor_Levitate: item^.SetOutdoorLevitate(ss.GetFloatField(-1, 'y_amp'),
							ss.GetFloatField(-1, 'y_freq'), ss.GetFloatField(-1, 'rx_vel'));
						outdoor_Phys:
							begin
								if ss.GetTableS(-1, 'primitive') then
								begin
									primitive := ss.ToStream(-1);
									ss.Pop;
								end else
									ss.Throw('Phys: требуется поле primitive');
								item^.SetOutdoorPhys(primitive, ss.GetFloatField(-1, 'mass', INFINITE_MASS));
							end;
						{$ifdef Debug} else Assert(no); {$endif}
					end;
					ss.Pop;
					break;
				end;
			ss.Pop;
		end;
		item^.Name := ss.GetStringField(1, 'name', item^.Name);
		item^.Desc := ss.GetStringField(1, 'desc', item^.Desc);

		ss.PushObject(item);
		for trait in ItemTraits do
			if (trait in item^.Traits) and ss.GetTableS(1, ItemTraitIds[trait]) then
			begin
				ReadAs[trait](pItemMixin(item^.GetAs(trait))^, ss);
				ss.Pop;
			end;
	end;

	procedure Script_Item_id(var ss: ScriptState); begin ss.PushString(pItem(ss.ToSelf)^.id); end;
	procedure Script_Item_radius(var ss: ScriptState); begin ss.PushFloat(pItem(ss.ToSelf)^.radius); end;
	procedure Script_Item_name(var ss: ScriptState); begin ss.PushString(pItem(ss.ToSelf)^.name); end;
	procedure Script_Item_desc(var ss: ScriptState); begin ss.PushString(pItem(ss.ToSelf)^.Describe); end;
	procedure Script_Item_outdoor(var ss: ScriptState); begin ss.PushString(Item.OutdoorModeIds[pItem(ss.ToSelf)^._outdoor.mode]); end;

	procedure Script_CreateInventory(var ss: ScriptState);
	var
		inv: pInventory;
	begin
		inv := new(pInventory, Init(UintTrunc(ss.GetVec2Field(1, 'size'))));
		if ss.GetTableS(1, 'owner') then
		begin
			inv^.Owner := pSceneNode(ss.ToObject(-1, TypeOf(SceneNode)));
			ss.Pop;
		end;
		ss.PushObject(inv);
	end;

	procedure Script_Inventory_Add(var ss: ScriptState);
	var
		inv: pInventory;
		item: pItem;
		ret: boolean;
	begin
		inv := ss.ToSelf;
		ss.GetTableS(2, 'item');
		item := MakeRef(ss.ToObject(-1, TypeOf(Inventories.Item)));
		ss.Pop;
		if ss.HasField(2, 'pos') then
			ret := inv^.Add(item, UintTrunc(ss.GetVec2Field(2, 'pos')))
		else
			ret := inv^.Add(item);
		Release(item);
		ss.PushBool(ret);
	end;

	procedure Script_OutdoorItem_item(var ss: ScriptState);
	begin
		ss.PushObject(pOutdoorItem(ss.ToSelf)^.item);
	end;

	procedure OpenScript(var script: ScriptState);
	const
		Stuff: array[0 .. 11] of ScriptStuffDesc =
		(
			(s: TypeDesc; p: TypeOf(Item)),
			(s: 'id'; p: @Script_Item_id),
			(s: 'radius'; p: @Script_Item_radius),
			(s: 'name'; p: @Script_Item_name),
			(s: 'desc'; p: @Script_Item_desc),
			(s: 'outdoor'; p: @Script_Item_outdoor),

			(s: TypeDesc; p: TypeOf(Inventory)),
			(s: 'Add:1'; p: @Script_Inventory_Add),

			(s: TypeDesc; p: TypeOf(OutdoorItem)),
			(s: 'item'; p: @Script_OutdoorItem_item),

			(s: FunctionsDesc + 'CreateItem:1' + RequireEnv; p: @Script_CreateItem),
			(s: 'CreateInventory:1'; p: @Script_CreateInventory)
		);
	begin
		script.AddStuff(Stuff);
	end;

{$ifdef use_serialization}
const
	ITEM_CANT_DROP_BIT        = 1 shl 0;
	ITEM_OUTDOOR_LEVITATE_BIT = 1 shl 1;
	ITEM_OUTDOOR_PHYS_BIT     = 1 shl 2;
	ITEM_RECT_MASK            = 1 shl 3;

	procedure SerializeItem(se: pSerializer; obj: pointer);
	var
		item: pItem absolute obj;
		flags: uint;
		n: sint;
		trait: ItemTrait;
	begin
		with se^ do
		begin
			Assert(not item^.Floating);
			flags := 0;
			if item^.CantDrop then flags := flags or ITEM_CANT_DROP_BIT;
			if item^._outdoor.mode = outdoor_Levitate then flags := flags or ITEM_OUTDOOR_LEVITATE_BIT;
			if item^._outdoor.mode = outdoor_Phys then flags := flags or ITEM_OUTDOOR_PHYS_BIT;
			if item^._mask.raw.Ones then flags := flags or ITEM_RECT_MASK;
			Serialize_ui8(stream, flags);

			n := 0;
			for trait in ItemTrait do
				if trait in item^.Traits then
					inc(n);
			Serialize_ui8(stream, n);
			for trait in ItemTrait do
				if trait in item^.Traits then
					Serialize_ui8(stream, ord(trait));

			Serialize_string(stream, item^.id);
			Serialize_string(stream, item^._name);
			Serialize_string(stream, item^._desc);
			item^._mask.Serialize(stream, yes, (ITEM_RECT_MASK and flags) <> 0);
			SeObject(item^._sprite);
			Serialize_f16(stream, item^.Radius);
			case item^._outdoor.mode of
				outdoor_Levitate:
					with item^._outdoor.levitate do
					begin
						Serialize_f16(stream, yAmp);
						Serialize_f16(stream, yFreq);
						Serialize_f16(stream, rxVel);
					end;
				outdoor_Phys:
					with item^._outdoor.phys do
					begin
						Serialize_string(stream, prim^);
						Serialize_f16(stream, mass);
					end;
			end;

			for trait in ItemTrait do
				if trait in item^.Traits then
					pItemMixin(item^.GetAs(trait))^.Serialize(stream);
		end;
	end;

	procedure DeserializeItem(de: pDeserializer; obj: pointer);
	var
		item: pItem absolute obj;
		flags: uint;
		i, n: sint;
		trait: ItemTrait;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			item^._flags := [];
			if (flags and ITEM_CANT_DROP_BIT) <> 0 then Include(item^._flags, item_CantDrop);
			if (flags and ITEM_OUTDOOR_LEVITATE_BIT) <> 0 then item^._outdoor.mode := outdoor_Levitate else
			if (flags and ITEM_OUTDOOR_PHYS_BIT) <> 0 then item^._outdoor.mode := outdoor_Phys else
				item^._outdoor.mode := outdoor_None;

			n := Deserialize_ui8(stream);
			item^._traits := [];
			for i := 0 to n - 1 do
				Include(item^._traits, ItemTrait(Deserialize_ui8(stream)));

			item^._id := Deserialize_string(stream);
			item^._name := Deserialize_string(stream);
			item^._desc := Deserialize_string(stream);
			item^._mask.Deserialize(stream, yes, (ITEM_RECT_MASK and flags) <> 0);
			if (ITEM_RECT_MASK and flags) <> 0 then item^._mask.raw.FillWithOnes;
			DeObjectR(item^._sprite);
			item^._radius := Deserialize_f16(stream);

			case item^._outdoor.mode of
				outdoor_Levitate:
					with item^._outdoor.levitate do
					begin
						yAmp := Deserialize_f16(stream);
						yFreq := Deserialize_f16(stream);
						rxVel := Deserialize_f16(stream);
					end;
				outdoor_Phys:
					with item^._outdoor.phys do
					begin
						new(prim); prim^ := Deserialize_string(stream);
						mass := Deserialize_f16(stream);
					end;
			end;

			item^.FillTraitsBlob(item^.Traits);
			for trait in ItemTrait do
				if trait in item^.Traits then
					pItemMixin(item^.GetAs(trait))^.Deserialize(stream);
		end;
	end;

	procedure ItemDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		item: pItem absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: item^.Init('', [], '#');
		end;
	end;

const
	INV_HAS_CALLBACKS_BIT = 1 shl 0;
	INV_HAS_OWNER_BIT     = 1 shl 1;

	procedure SerializeInventory(se: pSerializer; obj: pointer);
	var
		inv: pInventory absolute obj;
		flags: uint;
		i: sint;
	begin
		with se^ do
		begin
			Assert(length(inv^._floating) = 0);
			flags := 0;
			if Assigned(inv^._view.add) or Assigned(inv^._view.remove) or Assigned(inv^._view.screw) then
				flags := flags or INV_HAS_CALLBACKS_BIT;
			if Assigned(inv^._owner) then flags := flags or INV_HAS_OWNER_BIT;
			Serialize_ui8(stream, flags);

			Serialize_ui16(stream, inv^.Size.x or inv^.Size.y shl bitsizeof(uint8));
			VarInt.Write(stream, length(inv^.items));
			for i := 0 to High(inv^.items) do
			begin
				VarInt.Write(stream, inv^.items[i].pos.x);
				VarInt.Write(stream, inv^.items[i].pos.y);
				SeObject(inv^.items[i].item);
			end;
			if (flags and INV_HAS_CALLBACKS_BIT) <> 0 then
			begin
				SeFunction(inv^._view.add);
				SeFunction(inv^._view.remove);
				SeFunction(inv^._view.screw);
				SeObject(inv^._view.userParam);
			end;
			if Assigned(inv^._owner) then SeObject(inv^._owner);
		end;
	end;

	procedure DeserializeInventory(de: pDeserializer; obj: pointer);
	var
		inv: pInventory absolute obj;
		flags: uint;
		i, sxy: sint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			sxy := Deserialize_ui16(stream);
			inv^._mask.Resize(UintVec2.Make(RangeCheckMin(sxy and High(uint8), 1, 'SizeX'), RangeCheckMin(sxy shr bitsizeof(uint8), 1, 'SizeY')));
			SetLength(inv^.items, VarInt.Read(stream));
			for i := 0 to High(inv^.items) do
			begin
				inv^.items[i].pos.x := VarInt.Read(stream);
				inv^.items[i].pos.y := VarInt.Read(stream);
				DeObjectA(inv^.items[i].item);
			end;
			if (flags and INV_HAS_CALLBACKS_BIT) <> 0 then
			begin
				pointer(inv^._view.add   ) := DeFunction();
				pointer(inv^._view.remove) := DeFunction();
				pointer(inv^._view.screw ) := DeFunction();
				DeWeakR(inv^._view.userParam);
			end;
			if (flags and INV_HAS_OWNER_BIT) <> 0 then DeWeakR(inv^._owner);
		end;
	end;

	procedure InventorySeSpecial(se: pSerializer; what: SeSpecial; obj: pointer);
	var
		inv: pInventory absolute obj;
	begin
		Assert(@se = @se);
		case what of
			se_Before:
				while length(inv^._floating) > 0 do
					inv^.StopFloat(inv^._floating[0].item, no);
		end;
	end;

	procedure InventoryDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		inv: pInventory absolute obj;
		i: sint;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: inv^.Init(UintVec2.Zero);
			de_After:
				for i := 0 to High(inv^.items) do
				begin
					inv^.items[i].item^._inventoryId := i;
					inv^._mask.InplaceOr(inv^.items[i].item^.mask^, inv^.items[i].pos);
				end;
		end;
	end;

const
	OITEM_HAS_PHASE          = 1 shl 0;
	OITEM_HAS_BASE_TRANSFORM = 1 shl 1;

	procedure SerializeOutdoorItem(se: pSerializer; obj: pointer);
	var
		o: pOutdoorItem absolute obj;
		flags: uint;
	begin
		with se^ do
		begin
			flags := 0;
			case o^._item^._outdoor.mode of
				outdoor_Levitate: flags := flags or OITEM_HAS_PHASE or OITEM_HAS_BASE_TRANSFORM;
			end;
			Serialize_ui8(stream, flags);
			SeObject(o^._item);
			if (flags and OITEM_HAS_PHASE) <> 0 then Serialize_fN16(stream, o^._phase, 0.0, OutdoorItem.PhaseCycle);
			if (flags and OITEM_HAS_BASE_TRANSFORM) <> 0 then Serialize_tf32r8(stream, o^._baseTransform);
		end;
	end;

	procedure DeserializeOutdoorItem(de: pDeserializer; obj: pointer);
	var
		o: pOutdoorItem absolute obj;
		flags: uint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			DeObjectR(o^._item);
			if (flags and OITEM_HAS_PHASE) <> 0 then o^._phase := Deserialize_fN16(stream, 0.0, OutdoorItem.PhaseCycle);
			if (flags and OITEM_HAS_BASE_TRANSFORM) <> 0 then
			begin
				o^._baseTransform := Deserialize_tf32r8(stream);
				o^._baseTransformLock := no;
			end;
		end;
	end;

	procedure OutdoorItemDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		o: pOutdoorItem absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: o^.DeseInit;
		end;
	end;
{$endif}

	procedure Init;
	begin
	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Item', TypeOf(Item), nil, sizeof(Item), yes,
		               @SerializeItem, @DeserializeItem, nil, @ItemDeSpecial)
		^.RegisterType('Inventory', TypeOf(Inventory), nil, sizeof(Inventory), yes,
		               @SerializeInventory, @DeserializeInventory, @InventorySeSpecial, @InventoryDeSpecial)
		^.RegisterType('Outdoor item', TypeOf(OutdoorItem), TypeOf(RigidBody), sizeof(OutdoorItem), yes,
		               @SerializeOutdoorItem, @DeserializeOutdoorItem, nil, @OutdoorItemDeSpecial);
	{$endif}
	end;

initialization
	&Unit('Inventory').Initialize(@Init);
end.
