{$include opts.inc}
{$R *.res}
program rox;

uses
	USystem, Utils, UMath, UClasses, GLUtils, rox_mm, rox_gfx, rox_gl, rox_ui;

var
	prevTime, curTime, time, minFrameTime, cumTime: Ticks;
	cumTimeFrames: uint;
	err: gl.enum;
	ignoreOpenGLErrors: boolean = no;
	viewport: UintVec2;

	procedure Draw;
	begin
		viewport := ShrinkToAspect(window.size, ScreenAspect);
		viewportAp := AspectPair.Make(viewport);
		mouse := 2 * (IntVec2(window.mouse) - (window.size - viewport) div 2) / max(viewport, UintVec2.Ones) - Vec2.Ones;
		mouse.y := -mouse.y;
		gl.Viewport((window.size.x - viewport.x) div 2, (window.size.y - viewport.y) div 2, viewport.x, viewport.y);

		gl.Clear(gl.COLOR_BUFFER_BIT or gl.DEPTH_BUFFER_BIT);
		ui.Draw;
		CleanupGLGraveyard;
		window.SwapBuffers;
	end;

	procedure OnRepaint(param: pointer);
	begin
		Assert(@param = @param);
		Draw;
	end;

	procedure ResetCumTime;
	begin
		prevTime := Ticks.Get;
		cumTime := Ticks.Zero;
		cumTimeFrames := 0;
	end;

begin
	AppInfo.Feedback := 'Обратная связь: https://telegram.me/rika_ichinose';
	units.InitializeAll;

	window.Open;
	ui.Add(pControl(new(pControl, Init(Texture.Load('ui/newgame.png'),
		[ControlState.Make('', Rect.Make(0, 0, 1, 0.5)),
		ControlState.Make('press', Rect.Make(0, 0.5, 1, 1))]))^.NewRef)^.Place(Vec2.Make(-0.25, 0.5), 0.5, asp2_x1));

	minFrameTime := Ticks.FromSeconds(1/100);
	try
		bgm.AddTheme('menu').AddItem(Paths.Data + 'BGM/ps2phantasy2.mid');
		bgm.Priority('menu')^.SetModifier('menu', op_Add, +1, 0);
		gl.ClearColor(0.01, 0.06, 0.015, 1);
		gl.Enable(gl.DEPTH_TEST);
		gl.Enable(gl.TEXTURE_2D);
		gl.Enable(gl.BLEND);
		gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
		window.onRepaint := @OnRepaint;

		ResetCumTime;
		repeat
			if not window.Process then break;
			if window.WasDeactivatedDuringLastProcess then ResetCumTime;
			if window.mouse.x > window.size.x div 2 then window.cursor := Cursor1 else window.cursor := Cursor0;

			Draw;
			inc(cumTimeFrames);

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
			if time < MinFrameTime then Thread.Sleep(MinFrameTime - time);
			if time.ToSeconds < 1 then cumTime += time;
			if (cumTime.ToSeconds >= 1) and (cumTimeFrames > 3) then
			begin
			{$ifdef use_console} Con.WriteLine('FPS: ' + ToString(cumTimeFrames / cumTime.ToSeconds)); {$endif}
				ResetCumTime;
			end;
			prevTime := curTime;
		until no;
	finally
		window.Close;
	end;
end.
