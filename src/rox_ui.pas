{$include opts.inc}
unit rox_ui;

interface

uses
	USystem, UMath, Utils, GLUtils, rox_gl, rox_gfx;

type
	MouseAction = (MouseMove, MouseLClick, MouseLRelease, MouseRClick, MouseRRelease);
	KeyboardAction = (KeyClick, KeyRelease);
	HandlerExtra = object
	strict private
		justTry: boolean;
		handled: boolean;
	public
		procedure SetupTry;
		procedure SetupReal;
		function Handle: boolean;
		function HandleSilent: boolean;
		property WasHandled: boolean read handled;
	end;

const
	DefaultState = '';
	PressedState = 'press';
	HoverState = 'hover';

type
	ClickHandler = procedure(param: pointer);
	UpdateHandler = procedure(param: pointer);

	pControlState = ^ControlState;
	ControlState = object
		name: string;
		rect: Rect;
		ap: AspectPair;
		function Make(const name: string; const rect: Rect): ControlState; static;

	type
		ArrayOf = array of ControlState;
		function Split(const start, size: Vec2; horizontal: boolean; rowLength: uint; const names: array of string): ArrayOf; static;
	end;

	pUserInterface = ^UserInterface;
	pControl = ^Control;
	Control = object(&Object)
		ui: pUserInterface;
		tex: pTexture;
		local: Transform2;
		size: float;
		states: array of ControlState;
		state: sint;
		sizeMethod: Aspect2Method;
		color: Vec4;
		rawSize: Vec2;
		constructor Init(tex: pTexture; const states: array of ControlState);
		destructor Done; virtual;
		procedure Switch(const name: string);
		procedure Update(const dt: float); virtual;
		procedure Draw; virtual;
		function Place(const pos: Vec2; const size: float; sizeMethod: Aspect2Method): pControl;
		procedure ChangeTexture(tex: pTexture);
		function CalculateRawSize: Vec2;
		procedure Detach;

		procedure HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra); virtual;
	private
		function Under(const pos: Vec2): boolean;
		function FindState(const name: string): sint;
	end;

	pButton = ^Button;
	Button = object(&Control)
		mouseEntered, mouseLClickedOver: boolean;
		onClick: ClickHandler;
		onClickParam: pointer;
		procedure HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra); virtual;
	end;

	UserInterface = object
		mouseHandled: boolean;
		_mgr: pointer {pStateManager};
		controls: array of record
			ctl: pControl;
			group: string;
		end;
		procedure Init(mgr: pointer);
		procedure Done;
		procedure Update(const dt: float);
		procedure Draw;
		procedure Add(ctl: pControl; const group: string);
		procedure Remove(ctl: pControl);
		procedure RemoveGroup(const group: string);

		procedure HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra);
	private
		procedure InternalRemove(id: uint);
	end;

implementation

uses
	rox_win, rox_state;

	procedure HandlerExtra.SetupTry;
	begin
		justTry := yes;
		handled := no;
	end;

	procedure HandlerExtra.SetupReal;
	begin
		justTry := no;
		handled := no;
	end;

	function HandlerExtra.Handle: boolean;
	begin
		result := not justTry and not handled;
		handled := yes;
	end;

	function HandlerExtra.HandleSilent: boolean;
	begin
		result := not justTry and not handled;
	end;

	function ControlState.Make(const name: string; const rect: Rect): ControlState;
	begin
		result.name := name;
		result.rect := rect;
		result.ap := AspectPair.Make(rect.size);
	end;

	function ControlState.Split(const start, size: Vec2; horizontal: boolean; rowLength: uint; const names: array of string): ArrayOf;
	var
		rowStart, current: Vec2;
		rowPos: uint;
		i: sint;
	begin
		SetLength(result, length(names));
		rowStart := start;
		current := rowStart;
		rowPos := 0;
		for i := 0 to High(names) do
		begin
			result[i] := Make(names[i], UMath.Rect.Make(current, current + size));
			current.data[ord(not horizontal)] += size.data[ord(not horizontal)];
			inc(rowPos);
			if rowPos = rowLength then
			begin
				rowPos := 0;
				rowStart.data[ord(horizontal)] += size.data[ord(horizontal)];
				current := rowStart;
			end;
		end;
	end;

	constructor Control.Init(tex: pTexture; const states: array of ControlState);
	var
		i: sint;
	begin
		inherited Init;
		ui := nil;
		self.tex := tex;
		local := Transform2.Identity;
		size := 1.0;
		if length(states) = 0 then
		begin
			SetLength(self.states, 1);
			self.states[0] := ControlState.Make('default', Rect.ZeroOnes);
		end else
		begin
			SetLength(self.states, length(states));
			for i := 0 to High(self.states) do
				self.states[i] := states[i];
		end;
		ChangeTexture(tex);

		state := 0;
		sizeMethod := asp2_x1;
		color := Vec4.Ones;
		rawSize := Vec2.Ones;
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

	procedure Control.Update(const dt: float);
	begin
		Assert(@dt = @dt);
		rawSize := CalculateRawSize;
	end;

	procedure Control.Draw;
	var
		q: Quad;
	begin
		if size = 0 then exit;
		q.fields := [q.Field.Transform]; q.transform := local;
		if color <> Vec4.Ones then begin q.fields += [q.Field.Color]; q.color := color; end;
		q.Draw(tex, Vec2.Zero, rawSize, states[state].rect.A, states[state].rect.Size);
	end;

	function Control.Place(const pos: Vec2; const size: float; sizeMethod: Aspect2Method): pControl;
	begin
		self.local := Translate(pos);
		self.size := size;
		self.sizeMethod := sizeMethod;
		result := @self;
	end;

	procedure Control.ChangeTexture(tex: pTexture);
	var
		i: sint;
	begin
		if Assigned(self.tex) and (self.tex <> tex) then raise Error('Текстура не может быть заменена.');
		self.tex := tex;
		if Assigned(tex) then
			for i := 0 to High(states) do
				self.states[i].ap := self.states[i].ap.Combined(tex^.ap);
	end;

	function Control.CalculateRawSize: Vec2;
	begin
		result := states[state].ap.Aspect2(sizeMethod, size);
	end;

	procedure Control.Detach;
	begin
		if not Assigned(ui) then raise Error('Контрол не в UI.');
		ui^.Remove(@self);
	end;

	procedure Control.HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra);
	begin
		Assert((@action = @action) and (@pos = @pos) and (@extra = @extra));
	end;

	function Control.Under(const pos: Vec2): boolean;
	begin
		result := Rect.Make(local.trans, local.trans + rawSize).Contains(pos);
	end;

	function Control.FindState(const name: string): sint;
	begin
		result := Index(name, first_field states _ name _, length(states), sizeof(states[0]));
	end;

	procedure Button.HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra);
	begin
		case action of
			MouseMove:
				if (mouseEntered or Under(pos)) and extra.Handle then
				begin
					mouseEntered := Under(pos);
					if mouseEntered and Assigned(onClick) then
						if mouseLClickedOver then Switch(PressedState) else Switch(HoverState)
					else
						Switch(DefaultState);
				end;
			MouseLClick:
				if Under(pos) and extra.Handle then
					if Assigned(onClick) then
					begin
						mouseLClickedOver := yes;
						Switch(PressedState);
					end;
			MouseLRelease:
				if mouseLClickedOver and extra.Handle then
				begin
					mouseLClickedOver := no;
					if Under(pos) then
					begin
						if Assigned(onClick) then onClick(onClickParam);
						if Assigned(onClick) then Switch(HoverState) else Switch(DefaultState);
					end else
						Switch(DefaultState);
				end;
		end;
	end;

	procedure UserInterface.Init(mgr: pointer);
	begin
		pStateManager(mgr)^.Verify;
		_mgr := mgr;
		controls := nil;
	end;

	procedure UserInterface.Done;
	var
		i: sint;
	begin
		for i := High(controls) downto 0 do
			InternalRemove(i);
		controls := nil;
	end;

	procedure UserInterface.Update(const dt: float);
	var
		i: sint;
	begin
		for i := 0 to High(controls) do
			controls[i].ctl^.Update(dt);
	end;

	procedure UserInterface.Draw;
	var
		i: sint;
	begin
		for i := 0 to High(controls) do
			controls[i].ctl^.Draw;
	end;

	procedure UserInterface.Add(ctl: pControl; const group: string);
	begin
		try
			if Assigned(ctl^.ui) then raise Error('Контрол уже в UI.');
			Assert(Index(ctl, first_field controls _ ctl _, length(controls), sizeof(controls[0])) < 0);
			SetLength(controls, length(controls) + 1);
			controls[High(controls)].ctl := ctl;
			controls[High(controls)].group := group;
			ctl^.ui := @self;
		except
			Release(ctl);
			raise;
		end;
	end;

	procedure UserInterface.Remove(ctl: pControl);
	var
		id: sint;
	begin
		id := Index(ctl, first_field controls _ ctl _, length(controls), sizeof(controls[0]));
		if id < 0 then raise Error('Контрол не в UI.');
		InternalRemove(id);
	end;

	procedure UserInterface.RemoveGroup(const group: string);
	var
		i: sint;
	begin
		for i := High(controls) downto 0 do
			if controls[i].group = group then
				InternalRemove(i);
	end;

	procedure UserInterface.HandleMouse(action: MouseAction; const pos: Vec2; var extra: HandlerExtra);
	var
		i: sint;
	begin
		for i := 0 to High(controls) do
			controls[i].ctl^.HandleMouse(action, pos, extra);
	end;

	procedure UserInterface.InternalRemove(id: uint);
	var
		i: sint;
	begin
		controls[id].ctl^.ui := nil;
		Release(controls[id].ctl);
		for i := id to High(controls) - 1 do
			controls[i] := controls[i + 1];
		SetLength(controls, length(controls) - 1);
	end;

end.

