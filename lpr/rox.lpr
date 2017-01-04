{$include opts.inc}
{$R *.res}
program rox;

uses
	USystem, Utils, UMath, UClasses, GLUtils, Windowing,
	rox_win, rox_gfx, rox_gl, rox_paths, rox_world,
	rox_ui, rox_state, rox_state_mainmenu, rox_state_adventure,
	rox_ep_entry, rox_ep_bar, rox_mv_flight, rox_ep_mars, rox_ep_ship;

	procedure LoadBGM(var window: Window);
	begin
		window.state.bgm.AddTheme(MainMenu.StateID).AddItem(Music('phantasy2.mid'));
		window.state.bgm.AddTheme(Ep_Entry.EntryStateID).AddItem(Music('restoration1.mid'));
		window.state.bgm.AddTheme(Ep_Bar.StateID).AddItem(Music('pressure5.mid'));
		window.state.bgm.AddTheme(Ep_Entry.DepartStateID).AddItem(Music('machinecenter4.mid'));
		window.state.bgm.AddTheme('over').AddItem(Music('over.mid'));
		window.state.bgm.AddTheme(Ep_Mars.StateID).AddItem(Music('silentzone1.mid'));
		window.state.bgm.AddTheme(Ep_Ship.StateID).AddItem(Music('stepup1.mid'));
	end;

var
	prevTime, curTime, time, minFrameTime, cumTime, cumTimeWithoutSleep: Ticks;
	lastDt: float;
	cumTimeFrames: uint;
	err: gl.enum;
	ignoreOpenGLErrors: boolean = no;
	fpsNote: WindowCaption.Cookie;
	window: rox_win.Window;
	note: string;

	procedure ResetCumTime;
	begin
		prevTime := Ticks.Get;
		cumTime := Ticks.Zero;
		cumTimeWithoutSleep := Ticks.Zero;
		cumTimeFrames := 0;
	end;

	procedure ParseCommandLine;
	{$ifndef use_console} var total: string; {$endif}

		procedure Note(const msg: string);
		begin
		{$ifdef use_console} Con.WriteLine(msg);
		{$else} ContinueString(total, msg, EOL);
		{$endif}
		end;

	var
		cmd: CommandLine;
		i: uint;
	{$ifndef use_console} show: boolean; {$endif}

	begin
	{$ifndef use_console} total := ''; show := no; {$endif}
		cmd := CommandLine.Get;
		try
			i := 0;
			while i < cmd.Count do
			begin
				case cmd[i] of
					'no-fps-limit': begin Note('Ограничение FPS снято.'); minFrameTime := Ticks.Zero; inc(i); end;
					'show': begin {$ifndef use_console} show := yes; {$endif} inc(i); end;
					else
						raise Error(cmd.Raw + EOL + 'Неизвестная опция командной строки в позиции {0}: {1}.', [1 + i, cmd[i]]);
				end;
			end;
		{$ifndef use_console} if show and (total <> '') then Info.Title('Командная строка').Show(cmd.Raw + EOL + total); {$endif}
		finally
			cmd.Done;
		end;
	end;

begin
	AppInfo.Feedback := 'Обратная связь: https://telegram.me/rika_ichinose';
	units.InitializeAll;
	window.Invalidate;
	minFrameTime := Ticks.FromSeconds(1/80);
	ParseCommandLine;

	try
		try
			window.Open;
			window.cursor := Cursor0;
			fpsNote := WindowCaption.Cookie.Empty;
			LoadBGM(window);

			gl.PixelStorei(gl.PACK_ALIGNMENT, 1);
			gl.PixelStorei(gl.UNPACK_ALIGNMENT, 1);
			gl.Enable(gl.TEXTURE_2D);
			gl.Enable(gl.BLEND); gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
			gl.L.EnableClientState(gl.L.VERTEX_ARRAY);
			gl.L.EnableClientState(gl.L.TEXTURE_COORD_ARRAY);
			gl.ClearColor(0.01, 0.06, 0.015, 1);

			window.state.Switch(new(pEp_Mars{pEp_Entry}{pMainMenu}{pMv_Flight}, Init(new(pWorld, Init))));
			ResetCumTime;
			lastDt := 0.0;
			repeat
				if not window.Process(lastDt) then break;
				if window.WasDeactivatedDuringLastProcess or window.state.switchedDuringLastUpdate then
					ResetCumTime
				else
					cumTimeFrames += window.RedrawsDuringLastProcess;

				err := gl.GetError();
				if (err <> gl.NO_ERROR) and not ignoreOpenGLErrors then
					case Error.Text('OpenGL сообщила ' + gl.InlineErrorDescription(err) + '.').Title('Ошибка OpenGL').
						ContinueOrStopVariants.Variant('Игнорировать ошибки OpenGL').Show of
						TaskV1: ;
						TaskV2: exit;
						TaskV3: ignoreOpenGLErrors := yes;
					end;

				curTime := Ticks.Get;
				time := curTime - prevTime;
				if time.ToSeconds < 1 then cumTimeWithoutSleep += time;
				if time < MinFrameTime then
				begin
					Thread.Sleep(MinFrameTime - time);
					curTime := Ticks.Get;
					time := curTime - prevTime;
				end;

				if time.ToSeconds < 1 then
				begin
					cumTime += time;
					lastDt := time.ToSeconds;
				end;

				if (cumTime.ToSeconds >= 1) and (cumTimeFrames > 3) then
				begin
					note := 'FPS: ' + ToString(cumTimeFrames / cumTime.ToSeconds);
					if (minFrameTime > Ticks.Zero) and (cumTimeWithoutSleep.ToSeconds > 0) then
						note += ' (' + ToString(cumTimeFrames / cumTimeWithoutSleep.ToSeconds) + ')';
					window.caption.SetNote(fpsNote, note);
					ResetCumTime;
				end;
				prevTime := curTime;
			until no;
		finally
			window.caption.RemoveNote(fpsNote);
			window.Close;
		end;
	except
		Exception.Show;
	end;
end.
