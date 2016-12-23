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
		player := new(pActor, Init(Vec2.Make(0.14, 0.28), Character('rox', 'model.png'), Vec2.Make(1/9, 1/8)))^.NewRef;
		player^.AddState('idle', Vec2.Make(0, 0), 4, 8, 0.0, 'idle', []);
		player^.AddState('walk', Vec2.Make(0, 0), 4, 8, 0.6, 'walk', [MovingState]);
		player^.AddState('idle-wpn', Vec2.Make(4/9, 0), 4, 8, 0.0, 'idle-wpn', []);
		player^.AddState('walk-wpn', Vec2.Make(4/9, 0), 4, 8, 0.6, 'walk-wpn', [MovingState]);
		player^.AddState('firing', Vec2.Make(8/9, 0), 1, 8, 0.2, 'idle-wpn', [MovingState]);

		player^.SetupAimOrigins([
			Vec2.Make(49/50, 1-44/100), Vec2.Make(1-2/50, 1-27/100), Vec2.Make(23/50,1-11/100), Vec2.Make(2/50, 1-27/100),
			Vec2.Make(1-49/50, 1-44/100), Vec2.Make(1-45/50, 1-58/100), Vec2.Make(25/50, 1-61/100), Vec2.Make(45/50, 1-58/100)]);
	end;

	destructor World.Done;
	begin
		Release(player);
		inherited Done;
	end;

end.

