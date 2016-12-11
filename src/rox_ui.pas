{$include opts.inc}
unit rox_ui;

interface

uses
	USystem, UMath, GLUtils, rox_gfx, rox_mm;

const
	DefaultState = '';
	PressedState = 'press';

type
	ClickHandler = procedure(param: pointer);
	UpdateHandler = procedure(param: pointer);

	pControlState = ^ControlState;
	ControlState = object
		name: string;
		rect: Rect;
		ap: AspectPair;
		function Make(const name: string; const rect: Rect): ControlState; static;
	end;

	pControl = ^Control;
	Control = object(&Object)
		tex: pTexture;
		pos: Vec2;
		size: float;
		states: array of ControlState;
		state: sint;
		onUpdate: UpdateHandler;
		param: pointer;
		sizeMethod: Aspect2Method;
		constructor Init(tex: pTexture; const states: array of ControlState);
		destructor Done; virtual;
		procedure Switch(const name: string);
		procedure Update;
		procedure Draw;
		function Place(const pos: Vec2; const size: float; sizeMethod: Aspect2Method): pControl;
	private
		function FindState(const name: string): sint;
	end;

	UserInterface = object
		controls: array of pControl;
		procedure Init;
		procedure Done;
		procedure Update;
		procedure Draw;
		procedure Add(ctl: pControl);
		procedure Remove(ctl: pControl);
	end;

var
	ui: UserInterface;

implementation

	function ControlState.Make(const name: string; const rect: Rect): ControlState;
	begin
		result.name := name;
		result.rect := rect;
		result.ap := AspectPair.Make(rect.size);
	end;

	constructor Control.Init(tex: pTexture; const states: array of ControlState);
	var
		i: sint;
	begin
		inherited Init;
		self.tex := tex;
		pos := Vec2.Zero;
		size := 1.0;
		SetLength(self.states, length(states));
		for i := 0 to High(self.states) do
			self.states[i] := states[i];
		state := 0;
		onUpdate := nil;
		param := nil;
		if length(states) = 0 then raise Error('У контрола нет состояний.');
		sizeMethod := asp2_x1;
	end;

	destructor Control.Done;
	begin
		Release(tex);
		inherited Done;
	end;

	procedure Control.Switch(const name: string);
	var
		id: sint;
	begin
		id := FindState(name);
		if id < 0 then begin Warning('Неизвестное состояние ' + name + '.'); exit; end;
		state := id;
	end;

	procedure Control.Update;
	begin
		if Assigned(onUpdate) then onUpdate(param);
	end;

	procedure Control.Draw;
	var
		finalSize: Vec2;
	begin
		finalSize := states[state].ap.ReverseCombined(viewportAp).Aspect2(sizeMethod, size);
		DrawQuad(tex, pos, finalSize, states[state].rect.A, states[state].rect.Size, 0);
		if Rect.Make(pos, pos + finalSize).Contains(mouse) then Switch(PressedState) else Switch(DefaultState);
	end;

	function Control.Place(const pos: Vec2; const size: float; sizeMethod: Aspect2Method): pControl;
	begin
		self.pos := pos;
		self.size := size;
		self.sizeMethod := sizeMethod;
		result := @self;
	end;

	function Control.FindState(const name: string): sint;
	begin
		result := Index(name, pointer(pControlState(states)) + fieldoffset ControlState _ name _, length(states), sizeof(ControlState));
	end;

	procedure UserInterface.Init;
	begin
		controls := nil;
	end;

	procedure UserInterface.Done;
	var
		i: sint;
	begin
		for i := 0 to High(controls) do
			Release(controls[i]);
	end;

	procedure UserInterface.Update;
	var
		i: sint;
	begin
		for i := 0 to High(controls) do
			controls[i]^.Update;
	end;

	procedure UserInterface.Draw;
	var
		i: sint;
	begin
		for i := 0 to High(controls) do
			controls[i]^.Draw;
	end;

	procedure UserInterface.Add(ctl: pControl);
	var
		id: sint;
	begin
		try
			id := Index(ctl, pPointer(controls), length(controls));
			if id >= 0 then raise Error('Контрол уже в UI.');
			SetLength(controls, length(controls) + 1);
			controls[High(controls)] := ctl;
		except
			Release(ctl);
			raise;
		end;
	end;

	procedure UserInterface.Remove(ctl: pControl);
	var
		id: sint;
	begin
		id := Index(ctl, pPointer(controls), length(controls));
		if id < 0 then raise Error('Контрол не в UI.');
		Release(ctl);
		for id := id to High(controls) - 1 do
			controls[id] := controls[id + 1];
		SetLength(controls, length(controls) - 1);
	end;

end.

