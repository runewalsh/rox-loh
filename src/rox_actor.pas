{$include opts.inc}
unit rox_actor;

interface

uses
	USystem, UMath, rox_gfx;

type
	pActor = ^Actor;
	Actor = object
		pos, size: Vec2;
		tex: pTexture;
		states: array of record
			name: string;
			len: float;
			frames: uint;
			base: Vec2;
		end;

		{constructor Init;
		destructor Done; virtual;}
	end;


implementation

end.

