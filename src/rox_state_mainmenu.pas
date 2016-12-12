{$include opts.inc}
unit rox_state_mainmenu;

interface

uses
	USystem, Errors, UMath, Utils, GLBase, GLUtils, rox_state, rox_gl, rox_gfx, rox_ui, rox_state_adventure;

type
	pMainMenu = ^MainMenu;
	MainMenu = object(State)
	type
		ButtonEnum = (NewgameButton, GalleryButton, QuitButton);
	var
		state: (Prepare, FadeIn, Idle, StartingNewGame);
		fade: float;
		bg: pTexture;
		bgTime, stateTime, uiTime: float;
		buttons: array[ButtonEnum] of record
			tex: pTexture;
			ctl: pButton;
			progress: float;
		end;
		uiState: (UiRequestCreate, UiRequestFadeIn, UiFadingIn, UiShown, UiFadingButtonOut, UiDisabled);
		skipFx: boolean;
		selectedButton: ButtonEnum;
		earlyButton: sint; // ButtonEnum, нажатая до полного появления, -1, если нет

		constructor Init;
		destructor Done; virtual;

		procedure HandleActivation; virtual;

		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;

		procedure HandleMouse(action: MouseAction; const pos: Vec2); virtual;
	const
		ButtonInfo: array[ButtonEnum] of record
			id: string;
			pos: Vec2;
			size: float;
			inVec: Vec2;
		end =
		(
			(id: 'newgame.png'; pos: (data: (-0.25, 0.5)); size: 0.5; inVec: (data: (0, 0.05))),
			(id: 'gallery.png'; pos: (data: (-1.0, -1.0)); size: 0.5; inVec: (data: (-0.05, -0.05))),
			(id: 'quit.png'; pos: (data: (0.7, -1.0)); size: 0.3; inVec: (data: (0.05, -0.05)))
		);

		StateID = 'title';
	private
		procedure HandleClick(selected: ButtonEnum);
	end;

implementation

	constructor MainMenu.Init;
	var
		b: ButtonEnum;
	begin
		inherited Init(StateID);
		for b in ButtonEnum do
			buttons[b].tex := Texture.Load('ui/' + ButtonInfo[b].id);
		state := Prepare;
	end;

	destructor MainMenu.Done;
	var
		b: ButtonEnum;
	begin
		for b in ButtonEnum do
		begin
			Release(buttons[b].ctl);
			Release(buttons[b].tex);
		end;
		Release(bg);
		inherited Done;
	end;

	procedure MainMenu.HandleActivation;
	var
		name: string;
		time: float;
	begin
		inherited HandleActivation;
		if mgr^.bgm.CurrentTrack(@name, @time, nil) and (name = 'ps2phantasy2') and (time < 0.8) then
			mgr^.bgm.Rewind(0.8 - time);
	end;

	procedure NewgameClick(param: pointer); begin pMainMenu(param)^.HandleClick(NewgameButton); end;
	procedure GalleryClick(param: pointer); begin pMainMenu(param)^.HandleClick(GalleryButton); end;
	procedure QuitClick(param: pointer); begin pMainMenu(param)^.HandleClick(QuitButton); end;

	procedure MainMenu.HandleUpdate(const dt: float);
	label again, uiAgain;
	var
		completed: boolean;
		b: ButtonEnum;
		ctl: pButton;
	begin again:
		case state of
			Prepare:
				begin
					bgTime := 0;
					fade := 0;
					bg := Texture.Load('bg[c].dds');
					state := FadeIn; goto again;
				end;

			FadeIn:
				begin
               fade := min(fade + (0.5 + 1.5*ord(skipFx))*dt, 1.0);
					if fade = 1.0 then
					begin
						if uiState = UiShown then skipFx := no;
						state := Idle; goto again;
					end;
				end;

			Idle: ;

			StartingNewGame: if stateTime >= 2.5 then mgr^.Switch(new(pAdventure, Init));
		end;
		if state > Prepare then bgTime := modf(bgTime + dt, PrettyTimeCycle);
		stateTime += dt;
		uiTime += dt;

		uiAgain: case uiState of
			UiRequestCreate:
				if (bgTime > 3.0) or skipFx then
				begin
					for b in ButtonEnum do
					begin
						ctl := new(pButton, Init(buttons[b].tex^.NewRef,
							ControlState.Split(Vec2.Make(0, 0), Vec2.Make(1, 1/3), no, 99,
							[DefaultState, IfThen(b = NewgameButton, PressedState, HoverState), IfThen(b = NewgameButton, HoverState, PressedState)])))^.NewRef;
						buttons[b].ctl := ctl;
						buttons[b].ctl^.onClickParam := @self;
						mgr^.ui.Add(ctl^.Place(ButtonInfo[b].pos * mgr^.nvp - ButtonInfo[b].inVec, ButtonInfo[b].size * mgr^.nvp.x, asp2_x1)^.NewRef, self.id);
					end;

					uiState := UiRequestFadeIn; goto uiAgain;
				end;

			UiRequestFadeIn:
				begin
					for b in ButtonEnum do
						buttons[b].progress := 0;

					buttons[QuitButton].ctl^.onClick := @QuitClick;
					buttons[NewgameButton].ctl^.onClick := @NewgameClick;
					buttons[GalleryButton].ctl^.onClick := @GalleryClick;
					earlyButton := -1;

					uiState := UiFadingIn; goto uiAgain;
				end;

			UiFadingIn:
				begin
					completed := yes;
					for b in ButtonEnum do
					begin
						if skipFx then buttons[b].progress := 1 else
							if uiTime >= 0.3 * ord(b) then buttons[b].progress := min(buttons[b].progress + 0.6*dt, 1);
						completed := completed and (buttons[b].progress = 1);
						buttons[b].ctl^.local :=
							Translate2(ButtonInfo[b].pos * mgr^.nvp - ButtonInfo[b].inVec * (1 - buttons[b].progress)) *
							Translate2(0.5 * buttons[b].ctl^.rawSize) *
							Scale2(lerp(1.3, 1, buttons[b].progress)) *
							Translate2(-0.5 * buttons[b].ctl^.rawSize);
						buttons[b].ctl^.color.w := buttons[b].progress;
					end;

					if completed then
					begin
						if not (state in [FadeIn]) then skipFx := no;
						uiState := UiShown;
						if earlyButton >= 0 then HandleClick(ButtonEnum(RangeCheck(earlyButton, ord(High(ButtonEnum)), 'EarlyButton')));
					end;
				end;

			UiFadingButtonOut:
				begin
					completed := yes;
					for b in ButtonEnum do
					begin
						if skipFx then buttons[b].progress := 1;
						if b = selectedButton then
						begin
							buttons[b].progress := min(buttons[b].progress + 1.6*dt, 1);
							buttons[b].ctl^.local :=
								Translate2(ButtonInfo[b].pos * mgr^.nvp) *
								Translate2(0.5 * buttons[b].ctl^.rawSize) *
								Scale2(1.0 + 0.3 * buttons[b].progress) *
								Translate2(-0.5 * buttons[b].ctl^.rawSize);
						end else
							if uiTime > 0.2 then
							begin
								buttons[b].progress := min(buttons[b].progress + 2.0*dt, 1);
								buttons[b].ctl^.local :=
									Translate2(ButtonInfo[b].pos * mgr^.nvp + (3.0 * ButtonInfo[b].inVec) * buttons[b].progress);
							end;
						buttons[b].ctl^.color.w := 1.0 - buttons[b].progress;

						completed := completed and (buttons[b].progress = 1);
					end;

					if completed then
						case selectedButton of
							NewgameButton:
								begin
									stateTime := 0;
									state := StartingNewGame;
									uiState := UiDisabled;
									goto again;
								end;
							GalleryButton:
								begin
									uiState := UiRequestFadeIn;
									skipFx := no;
									state := Idle;
								end;
							QuitButton: mgr^.Pop;
						end;
				end;
		end;
	end;

	procedure MainMenu.HandleDraw;
	var
		texZ, lodBias, t, time: float;
		q: Quad;
		name: string;
		freqs, fft: array[0 .. 1] of float;
		pos, size: Vec2;
		i: sint;
	begin
		case state of
			FadeIn, Idle, StartingNewGame:
				begin
					gl.Disable(gl.DEPTH_TEST);
					gl.BindTexture(bg^.targetEnum, bg^.handle);
					if bgTime < 4.0 then
					begin
						texZ := sqrt(bgTime / 4);
						lodBias := max(0, 2 * (1 - bgTime/2));
					end else
					begin
						t := abs(1.0 - modf(0.2 * (bgTime - 4.0), 2.0));
						texZ := (1-4/bg^.sizeZ) + 3.5/bg^.sizeZ * lerp(t, smoothstep(t), 0.6);
						lodBias := 1 * (2.0 * abs(0.5 - frac(texZ*bg^.sizeZ)));
					end;

					if mgr^.bgm.CurrentTrack(@name, @time, nil) and (name = 'ps2phantasy2') and (time > 26) then
					begin
						lodBias += clamp(1.5 * (5 - abs(31 - time)), 0, 5);
						if time > 34 then
						begin
							freqs[0] := 560;
							freqs[1] := 1050;
							mgr^.bgm.Spectre(length(freqs), freqs, nil, fft);
							lodBias += min(1, 0.5 * (time - 33)) * min(20 * fft[0] + 20 * fft[1], 5);
						end;
					end;

					gl.TexParameterf(bg^.targetEnum, gl.TEXTURE_LOD_BIAS, lodBias);

					size := bg^.ap.Aspect2(asp2_y1, 2);
					pos := -0.5 * size;

					q.fields := [q.Field.TexZ];
					q.texZ := texZ;
					case state of
						FadeIn:
							begin
								q.fields += [q.Field.Color];
								q.color := Vec4.Make(fade, fade, fade, 1);
							end;
					end;

					case state of
						StartingNewGame:
							for i := 1 to 4 do
							begin
								q.fields += [q.Field.Transform];
								q.transform :=
									Scale2(1 + sqr(1 + i/4)*pow(stateTime, 4)) *
									Rotate2(0.5 * sqr(i/4)*sqr(stateTime)) *
									Translate2(0, remapc(sqr(stateTime), sqr(2), 0, -0.18, 0));
								if not (q.Field.Color in q.fields) then begin q.fields += [q.Field.Color]; q.color := Vec4.Ones; end;
								if stateTime >= 2.0 then q.color := Vec4.Make(Vec3.Make(remapc(stateTime, 2.5, 2, 0, 1)), q.color.w);
								q.color.w := sqrt((5-i)/4);
								q.Draw(bg, pos, size, Vec2.Zero, Vec2.Ones);
							end;
						else
							q.Draw(bg, pos, size, Vec2.Zero, Vec2.Ones);
					end;

					gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_LOD_BIAS, 0);
					gl.Enable(gl.DEPTH_TEST);
				end;
		end;
	end;

	procedure MainMenu.HandleMouse(action: MouseAction; const pos: Vec2);
	begin
		case action of
			MouseLClick: if (state in [FadeIn]) or (uiState in [UiRequestCreate, UiFadingIn, UiFadingButtonOut]) then skipFx := yes;
		end;
		inherited HandleMouse(action, pos);
	end;

	procedure MainMenu.HandleClick(selected: ButtonEnum);
	var
		b: ButtonEnum;
	begin
		if uiState = UiFadingIn then begin earlyButton := ord(selected); skipFx := yes; end;
		if not (state in [FadeIn, Idle]) or not (uiState in [UiShown]) then exit;
		for b in ButtonEnum do
		begin
			buttons[b].ctl^.onClick := nil;
			buttons[b].progress := 0;
		end;
		selectedButton := selected;
		uiTime := 0;
		uiState := UiFadingButtonOut;
	end;

end.

