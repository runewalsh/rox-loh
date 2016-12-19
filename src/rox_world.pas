unit rox_world;

interface

uses
	USystem, UMath,
	rox_actor, rox_paths;

type
	pWorld = ^World;
	World = object(&Object)
		player: pActor;
		spaceshipArrived: boolean;
		constructor Init;
		destructor Done; virtual;
	end;

implementation

	constructor World.Init;
	begin
		inherited Init;
		player := new(pActor, Init(Vec2.Make(0.14, 0.28), Character('rox', 'model.png'), Vec2.Make(1/4, 1/8)))^.NewRef;
		player^.AddState('idle', Vec2.Make(0, 0), 4, 8, 0.0, 'idle', []);
		player^.AddState('walk', Vec2.Make(0, 0), 4, 8, 0.6, 'walk', [MovingState]);
	end;

	destructor World.Done;
	begin
		Release(player);
		inherited Done;
	end;

end.

