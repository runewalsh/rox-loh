{$include opts.inc}
unit rox_ep_ship;

interface

uses
	USystem, UMath,
	rox_state_adventure, rox_decoration, rox_paths, rox_world;

type
	pEp_Ship = ^Ep_Ship;
	Ep_Ship = object(Adventure)
		constructor Init(world: pWorld);
	const
		StateID = 'ep_ship';
	end;

implementation

	constructor Ep_Ship.Init(world: pWorld);
	begin
		inherited Init(StateID, world);
		(new(pDecoration, Init(Environment('ship-floor.png'), Transform2.Identity, Vec2.Make(2, Deduce))))^.SetLayer(-2)^.AddTo(location);
		location^.Add(player);
	end;

end.

