{$include opts.inc}
unit rox_win;

interface

uses
	ctypes, Windows, USystem, Errors, DynamicLoader, UMath, Utils, Human, Windowing, Input, GLUtils, Audio, rox_state, rox_paths;

	function ShrinkToAspect(const size, aspect: UintVec2): UintVec2;
	procedure Warning(const msg: string);

const
	ScreenAspect: UintVec2 = (data: (16, 9));

type
	CursorEnum = (DefaultCursor, Cursor0, Cursor1, Cursor2, CursorHidden);
	RepaintProc = procedure(param: pointer);

	pWindow = ^Window;
	Window = object
		rect: WindowRect;
		viewport: UintVec2;
		winMouse: UintVec2;
		vpMouse: Vec2;
		caption: WindowCaption;
		state: StateManager;
		procedure Invalidate;
		function FromPointer(ptr: pointer): pWindow; static;
		procedure Open;
		procedure Close;
		function Process(const dt: float): boolean;
		procedure SwapBuffers;
		procedure RequestClose;

		property pos: IntVec2 read rect.pos write rect.pos;
		property size: UintVec2 read rect.size write rect.size;
	private
		handle: HWND;
		classh: ATOM;
		classn: widestring;

		dc: HDC;
		rc: HGLRC;
		rcThread: Thread.ID;
		oglLoaded, openSucceed, active, wasDeactivated: boolean;
		cursors: array[CursorEnum] of HCURSOR;
		activeCursor, realCursor: CursorEnum;
		magic: array[0 .. 3] of char;
		redraws: uint;

		wglChoosePixelFormatARB: function(hdc: HDC; piAttribIList: pcint; pfAttribFList: pcfloat; nMaxFormats: Windows.UINT;
			piFormats: pcint; nNumFormats: Windows.PUINT): Windows.BOOL; stdcall;

		function QueryRect(client: boolean): WindowRect;
		function ClientToScreen(const rect: WindowRect; style: dword): WindowRect;
		procedure CleanupRenderingContext;
		function LoadCursor(const fn: string): HCURSOR;
		procedure ChangeCursor(cur: HCURSOR);
		procedure SetActiveCursor(cur: CursorEnum);
		procedure UpdateRealCursor(cur: CursorEnum);

		function RunWindowProc(wnd: hWnd; msg: Windows.UINT; wparam: wParam; lparam: lParam): lResult;
		procedure Redraw;
		procedure UpdateViewport(const rect: WindowRect);
		procedure UpdateNormalizedMouse(const winMouse: UintVec2);
	const
		CorrectMagic = 'WND~';
		IncorrectMagic = '!wnd';
	public
		property Cursor: CursorEnum read activeCursor write SetActiveCursor;
		property GLContextOwner: Thread.ID read rcThread;
		property GLContext: HGLRC read rc;
		property WasDeactivatedDuringLastProcess: boolean read wasDeactivated;
		property RedrawsDuringLastProcess: uint read redraws;
	const
		MSAA = 4;
	end;

implementation

uses
	rox_gl, rox_ui, rox_gfx;

type
	Win = WindowsSpecific;

	function ShrinkToAspect(const size, aspect: UintVec2): UintVec2;
	var
		rx, ry: uint;
	begin
		if not aspect.Positive then exit(UintVec2.Zero);
		rx := size.x * aspect.y;
		ry := size.y * aspect.x;
		if rx > ry then result := UintVec2.Make((ry + aspect.y div 2) div aspect.y, size.y) else
			if rx < ry then result := UintVec2.Make(size.x, (rx + aspect.x div 2) div aspect.x) else
				result := size;
	end;

	procedure Warning(const msg: string);
	begin
	{$ifdef use_console} Con.WriteLine(msg); {$endif}
	{$ifdef Debug} USystem.Warning.Show(msg); {$endif}
		Assert(@msg = @msg);
	end;

	procedure WindowsWarning(const what: string; code: dword = 0);
	begin
		Warning(Win.OperationFailedMessage(what, code));
	end;

	function WindowProc(wnd: hWnd; msg: Windows.UINT; wparam: wParam; lparam: lParam): lResult; stdcall;
	var
		null: pointer;
	begin
		null := @pointer(nil^);
		result := Window((null + GetWindowLongPtrW(wnd, GWL_USERDATA))^).RunWindowProc(wnd, msg, wparam, lparam);
	end;
	function AnimateWindow(hwnd: HWND; dwTime: DWORD; dwFlags: DWORD): BOOL; stdcall; external user32;

	procedure Window.Invalidate;
	begin
		handle := 0;
		classh := 0;
		dc := 0;
		rc := 0;
		rcThread := 0;
		oglLoaded := no;
		winMouse := UintVec2.Zero;
		vpMouse := Vec2.Zero;
		caption.Invalidate;
		state.Invalidate;

		activeCursor := DefaultCursor;
		realCursor := DefaultCursor;
		Zero(@cursors, sizeof(cursors));
		openSucceed := no;
		active := yes;
		magic := IncorrectMagic;
	end;

	function Window.FromPointer(ptr: pointer): pWindow;
	begin
		result := ptr;
		if not Assigned(result) or (result^.magic <> CorrectMagic) then raise Error('Окно не валидно.');
	end;

	procedure OnUpdateCaption(const cap: string; param: pointer);
	begin
		if Window(param^).handle <> 0 then SetWindowTextW(Window(param^).handle, pWideChar(UTF8Decode(cap)));
	end;

	procedure Window.Open;
	const
		WGL_DRAW_TO_WINDOW_ARB = $2001;
		WGL_SUPPORT_OPENGL_ARB = $2010;
		WGL_DOUBLE_BUFFER_ARB = $2011;
		WGL_PIXEL_TYPE_ARB = $2013;
		WGL_TYPE_RGBA_ARB = $202B;
		WGL_COLOR_BITS_ARB = $2014;
		WGL_DEPTH_BITS_ARB = $2022;
		WGL_SAMPLE_BUFFERS_ARB = $2041;
		WGL_SAMPLES_ARB = $2042;

	var
		wc: WNDCLASSW;
		style, err: dword;
		onScreen: WindowRect;
		pfd: PIXELFORMATDESCRIPTOR;
		pixfmt, rcTry: cint;
		nFormats: Windows.UINT;
		attribs: array[0 .. 16] of cint;
	begin
		Invalidate;
		try
			magic := CorrectMagic;
			size := ShrinkToAspect((ScreenSize * 2) div 3, ScreenAspect);
			caption.Init(@OnUpdateCaption, nil, @self);
			caption.base := '(´・ω・`)';

			classn := UTF8Decode('Главное окно');
			Zero(@wc, sizeof(wc));
			wc.lpszClassName := pWideChar(classn);
			wc.lpfnWndProc := @WindowProc;
			wc.hInstance := HInstance;
			wc.hIcon := LoadIcon(0, IDI_APPLICATION);
			wc.hCursor := Windows.LoadCursor(0, IDC_ARROW);

			classh := RegisterClassW(wc);
			if classh = 0 then raise Win.OperationFailed('создать класс окна (RegisterClassW)');

			style := WS_SYSMENU or WS_CAPTION {$ifdef Debug} or WS_SIZEBOX {$endif} or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
			rect.pos := (IntVec2(ScreenSize) - self.size) div 2;
			onScreen := ClientToScreen(rect, style);

			for rcTry := 1 to 2 do
			begin
				handle := CreateWindowW(pWideChar(classn), pWideChar(UTF8Decode(caption.Join)), style,
					onScreen.pos.x, onScreen.pos.y, onScreen.size.x, onScreen.size.y, 0, 0, 0, nil);
				if handle = 0 then raise Win.OperationFailed('открыть окно (CreateWindow)');

				SetLastError(0);
				if SetWindowLongPtrW(handle, GWL_USERDATA, pointer(@self) - NULL) = 0 then
				begin
					err := GetLastError;
					if err <> 0 then raise Win.OperationFailed('выставить пользовательский указатель в окне (SetWindowLongPtrW)');
				end;

				dc := GetDC(handle);
				if dc = 0 then raise Win.OperationFailed('получить контекст устройства (GetDC)');

				if rcTry = 1 then
				begin
					Zero(@pfd, sizeof(pfd));
					pfd.nSize := sizeof(pfd);
					pfd.nVersion := 1;
					pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
					pfd.iPixelType := PFD_TYPE_RGBA;
					pfd.cColorBits := 24;
					pfd.cDepthBits := 24;
					pfd.iLayerType := PFD_MAIN_PLANE;

					pixfmt := ChoosePixelFormat(dc, @pfd);
					if pixfmt = 0 then raise Win.OperationFailed('выбрать подходящий формат вывода картинки (ChoosePixelFormat)');
				end else
				begin
					attribs[0] := WGL_DRAW_TO_WINDOW_ARB; attribs[1] := 1;
					attribs[2] := WGL_SUPPORT_OPENGL_ARB; attribs[3] := 1;
					attribs[4] := WGL_DOUBLE_BUFFER_ARB; attribs[5] := 1;
					attribs[6] := WGL_PIXEL_TYPE_ARB; attribs[7] := WGL_TYPE_RGBA_ARB;
					attribs[8] := WGL_COLOR_BITS_ARB; attribs[9] := 24;
					attribs[10] := WGL_DEPTH_BITS_ARB; attribs[11] := 24;
					attribs[12] := WGL_SAMPLE_BUFFERS_ARB; attribs[13] := 1;
					attribs[14] := WGL_SAMPLES_ARB; attribs[15] := MSAA;
					attribs[16] := 0;

					if not wglChoosePixelFormatARB(dc, @attribs[0], nil, 1, @pixfmt, @nFormats) then
						raise Win.OperationFailed('выбрать подходящий формат вывода картинки (wglChoosePixelFormatARB)');
					if nFormats < 1 then raise Error('Не найдено подходящих форматов экранного изображения (24-битный цвет + 24-битная глубина + {0}x AA).', [MSAA]);
				end;
				if not SetPixelFormat(dc, pixfmt, @pfd) then raise Win.OperationFailed('выставить формат вывода картинки (SetPixelFormat)');

				rc := wglCreateContext(dc);
				if rc = 0 then raise Win.OperationFailed('создать GL-контекст (wglCreateContext)');

				if not wglMakeCurrent(dc, rc) then raise Win.OperationFailed('привязать GL-контекст к потоку');
				rcThread := Thread.Current;

				if rcTry = 1 then
				begin
					pointer(wglChoosePixelFormatARB) := wglGetProcAddress('wglChoosePixelFormatARB');
					if not Assigned(wglChoosePixelFormatARB) then
					begin
						Warning('Расширенный контекст OpenGL не будет создан (не найдена wglChoosePixelFormatARB).');
						break;
					end;
					CleanupRenderingContext;

					if not DestroyWindow(handle) then WindowsWarning('уничтожить окно (DestroyWindow)');
					handle := 0;
				end;
			end;

			gl.Load;
			rox_gfx.InitGL(@self);
			Audio.Config.window := handle;
			oglLoaded := yes;

			cursors[DefaultCursor] := Windows.LoadCursor(0, IDC_ARROW);
			if cursors[DefaultCursor] = 0 then raise Win.OperationFailed('загрузить обычный указатель (LoadCursor)');

			cursors[Cursor0] := LoadCursor(rox_paths.Cursor('Pulse_Glass.ani'));
			cursors[Cursor1] := LoadCursor(rox_paths.Cursor('Pulse_Glass_Working.ani'));

			state.Init(@self);
			UpdateViewport(rect);
			if not AnimateWindow(handle, 200, AW_ACTIVATE or AW_BLEND) then raise Win.OperationFailed('отобразить окно (AnimateWindow)');
			openSucceed := yes;
		except
			Close;
			raise;
		end;
	end;

	procedure Window.Close;
	begin
		state.Done;
		openSucceed := no;
		CleanupRenderingContext;
		caption.Done;
		if handle <> 0 then
		begin
			if not DestroyWindow(handle) then WindowsWarning('уничтожить окно (DestroyWindow)');
			handle := 0;
		end;

		if classh <> 0 then
		begin
			if not UnregisterClassW(pWideChar(classn), 0) then WindowsWarning('уничтожить класс окна (UnregisterClassW)');
			classh := 0;
		end;
		Invalidate;
	end;

	function Window.Process(const dt: float): boolean;
	var
		message: MSG;
		mouseAction: rox_ui.MouseAction;
		key: KeyboardKey;
	begin
		wasDeactivated := no;
		redraws := 0;
		if handle = 0 then exit(no);
		result := yes;
		repeat
			if not PeekMessage((@message)^, 0, 0, 0, PM_Remove) then
				if active or (handle = 0) then break else
				begin
					wasDeactivated := yes;
					case cint(GetMessage((@message)^, handle, 0, 0)) of
						-1: raise Win.OperationFailed('извлечь UI-сообщение из очереди');
						0: result := no;
					end;
				end;

			case message.message of
				WM_QUIT: result := no;
				WM_SYSKEYDOWN, WM_KEYDOWN, WM_SYSKEYUP, WM_KEYUP:
					if Win.DecryptKey(message.wparam, @message, key) then
						if (message.message = WM_SYSKEYDOWN) or (message.message = WM_KEYDOWN) then
							state.HandleKeyboard(KeyClick, key)
						else
							state.HandleKeyboard(KeyRelease, key);
				WM_MOUSEMOVE:
					begin
						winMouse := UintVec2.Make(GET_X_LPARAM(message.lParam), GET_Y_LPARAM(message.lParam));
						UpdateNormalizedMouse(winMouse);
					end;
				WM_LBUTTONDOWN, WM_LBUTTONUP, WM_RBUTTONDOWN, WM_RBUTTONUP:
					begin
						case message.message of
							WM_LBUTTONDOWN: mouseAction := MouseLClick;
							WM_LBUTTONUP: mouseAction := MouseLRelease;
							WM_RBUTTONDOWN: mouseAction := MouseRClick;
							WM_RBUTTONUP: mouseAction := MouseRRelease;
							else raise ExhaustiveCase(message.message, 'MouseMessage');
						end;
						state.HandleMouse(mouseAction, vpMouse);
					end;
				WM_MOUSEWHEEL:
					begin
						// TWMMouseWheel(addr(message.message)^).WheelDelta / WHEEL_DELTA
					end;
			end;
			TranslateMessage(message);
			DispatchMessage(message);
		until no;

		if result then
		begin
			state.Update(dt);
			Redraw;
		end;
	end;

	procedure Window.SwapBuffers;
	begin
		Windows.SwapBuffers(dc);
	end;

	procedure Window.RequestClose;
	begin
		PostMessage(handle, WM_CLOSE, 0, 0);
	end;

	function Window.QueryRect(client: boolean): WindowRect;
	var
		r: Windows.RECT;
	begin
		Assert(handle <> 0);
		if client then
			if not GetClientRect(handle, @r) then raise Win.OperationFailed('запросить клиентский прямоугольник окна') else
		else
			if not Windows.GetWindowRect(handle, @r) then raise Win.OperationFailed('запросить прямоугольник окна');
		result := WindowRect.Make(IntVec2.Make(r.left, r.top), UintVec2.Make(max(r.right - r.left, 0), max(r.bottom - r.top, 0)));
	end;

	function Window.ClientToScreen(const rect: WindowRect; style: dword): WindowRect;
	var
		wr: Windows.RECT;
		b: IntVec2;
	begin
		wr.left := rect.pos.x;
		wr.top  := rect.pos.y;
		b := pos + size;
		wr.right := b.x;
		wr.bottom := b.y;
		if AdjustWindowRect(wr, style, no) then
			result := WindowRect.Make(IntVec2.Make(wr.left, wr.top), UintVec2.Make(max(wr.right - wr.left, 0), max(wr.bottom - wr.top, 0)))
		else
		begin
			WindowsWarning('вычислить позицию окна (AdjustWindowRect)');
			result := rect;
		end;
	end;

	procedure Window.CleanupRenderingContext;
	begin
		if oglLoaded then
		begin
			Audio.Config.window := 0;
			rox_gfx.DoneGL;
		end;

		if rc <> 0 then
		begin
			if rcThread <> 0 then
				if rcThread = Thread.Current then
					if not wglMakeCurrent(dc, rc) then WindowsWarning('отвязать контекст устройства от потока') else
				else
					WindowsWarning('контекст устройства привязан к другому потоку!');
			if not wglDeleteContext(rc) then WindowsWarning('уничтожить GL-контекст');
			rc := 0;
			rcThread := 0;
		end;

		if oglLoaded then
		begin
			gl.Unload;
			oglLoaded := no;
		end;

		if dc <> 0 then
		begin
			if ReleaseDC(handle, dc) = 0 then WindowsWarning('освободить контекст устройства (ReleaseDC)');
			dc := 0;
		end;
	end;

	function Window.LoadCursor(const fn: string): HCURSOR;
	var
		code: dword;
	begin
		result := LoadCursorFromFileW(pWideChar(Win.ToWideFileName(fn)));
		if result = 0 then
		begin
			code := GetLastError;
			raise Win.OperationFailed('загрузить курсор ' + fn, code);
		end;
	end;

	procedure Window.ChangeCursor(cur: HCURSOR);
	begin
		if cur <> 0 then
		begin
			SetLastError(0);
			if (SetClassLongPtrW(handle, GCL_HCURSOR, cur) = 0) and (GetLastError <> 0) then
				Warning(Win.OperationFailedMessage('установить курсор'));
		end;
	end;

	procedure Window.SetActiveCursor(cur: CursorEnum);
	begin
		if cur <> activeCursor then
		begin
			activeCursor := cur;
			if realCursor <> DefaultCursor then UpdateRealCursor(cur);
		end;
	end;

	procedure Window.UpdateRealCursor(cur: CursorEnum);
	begin
		if realCursor <> cur then
		begin
			if cur = CursorHidden then ShowCursor(no) else
			begin
				ChangeCursor(cursors[cur]);
				if realCursor = CursorHidden then ShowCursor(yes);
			end;
			realCursor := cur;
		end;
	end;

	function Window.RunWindowProc(wnd: hWnd; msg: Windows.UINT; wparam: wParam; lparam: lParam): lResult;
	var
		activated: boolean;
	begin
		case msg of
			WM_ACTIVATE:
				begin
					activated := LOWORD(wparam) <> WA_INACTIVE;
					if activated <> active then
					begin
						if not activated then
							Sound.GlobalPause
						else
							Sound.GlobalResume;
						active := activated;
					end;
				end;
			WM_SETCURSOR:
				if LOWORD(lparam) = HTCLIENT then
					UpdateRealCursor(activeCursor)
				else
					UpdateRealCursor(DefaultCursor);
			WM_MOVE, WM_SIZE:
				begin
					rect := QueryRect(yes);
					UpdateViewport(rect);
				end;
			WM_PAINT: Redraw;
			WM_DESTROY:
				begin
					state.Done;
					CleanupRenderingContext;
					handle := 0;
					if openSucceed then PostQuitMessage(0);
				end;
			WM_CLOSE: AnimateWindow(handle, 200, AW_HIDE or AW_BLEND);
		end;
		Result := DefWindowProcW(wnd, msg, wparam, lparam);
	end;

	procedure Window.Redraw;
	begin
		gl.Clear(gl.COLOR_BUFFER_BIT);
		state.Draw;
		inc(redraws);
		CleanupGLGraveyard;
		SwapBuffers;
	end;

	procedure Window.UpdateViewport(const rect: WindowRect);
	begin
		viewport := ShrinkToAspect(rect.size, ScreenAspect);
		gl.Viewport((size.x - viewport.x) div 2, (size.y - viewport.y) div 2, viewport.x, viewport.y);
		state.HandleViewportChange(viewport);
	end;

	procedure Window.UpdateNormalizedMouse(const winMouse: UintVec2);
	begin
		vpMouse := 2 * (IntVec2(winMouse) - (size - viewport) div 2) / max(viewport, UintVec2.Ones) - Vec2.Ones;
		vpMouse.y := -vpMouse.y;
		state.HandleMouse(MouseMove, vpMouse);
	end;

end.

