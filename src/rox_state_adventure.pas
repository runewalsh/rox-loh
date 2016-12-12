{$include opts.inc}
unit rox_state_adventure;

interface

uses
	USystem, UMath, rox_state, rox_gl, rox_ui, rox_actor;

type
	pAdventure = ^Adventure;
	Adventure = object(State)
		switchOut: boolean;
		constructor Init;
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;
		procedure HandleMouse(action: MouseAction; const pos: Vec2); virtual;
	const
		StateID = 'adventure';
	end;

implementation

uses
	rox_state_mainmenu;

	constructor Adventure.Init;
	begin
		inherited Init(StateID);
	end;

	destructor Adventure.Done;
	begin
		inherited Done;
	end;

	procedure Adventure.HandleUpdate(const dt: float);
	begin
		inherited HandleUpdate(dt);
		if switchOut then mgr^.Switch(new(pMainMenu, Init));
	end;

	procedure Adventure.HandleDraw;
	begin
	end;

	procedure Adventure.HandleMouse(action: MouseAction; const pos: Vec2);
	begin
		if action = MouseRClick then switchOut := yes;
		inherited HandleMouse(action, pos);
	end;

end.

