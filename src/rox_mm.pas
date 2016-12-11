{$include opts.inc}
unit rox_mm;

interface

uses
	ctypes, Windows, USystem, DynamicLoader, UMath, Utils, Human, Windowing, Input, GLUtils, Audio;

	procedure Warning(const msg: string);

const
	ScreenAspect: UintVec2 = (data: (16, 9));

type
	CursorEnum = (DefaultCursor, Cursor0, Cursor1, Cursor2);
	RepaintProc = procedure(param: pointer);

	PlainWindow = object
		rect: WindowRect;
		mouse: UintVec2;
		procedure Invalidate;
		procedure Open;
		procedure Close;
		function Process: boolean;
		procedure SwapBuffers;

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
		function QueryRect(client: boolean): WindowRect;
		function ClientToScreen(const rect: WindowRect; style: dword): WindowRect;
		procedure CleanupRenderingContext;
		function LoadCursor(const fn: string): HCURSOR;
		procedure ChangeCursor(cur: HCURSOR);

		function RunWindowProc(wnd: hWnd; msg: Windows.UINT; wparam: wParam; lparam: lParam): lResult;
	public
		onRepaint: RepaintProc;
		onRepaintParam: pointer;
		property Cursor: CursorEnum read activeCursor write activeCursor;
		property GLContextOwner: Thread.ID read rcThread;
		property GLContext: HGLRC read rc;
		property WasDeactivatedDuringLastProcess: boolean read wasDeactivated;
	end;

var
	window: PlainWindow;
	bgm: MusicPlayer;
	viewportAp: AspectPair;
	mouse: Vec2;

implementation

uses
	rox_gl, rox_gfx;

type
	Win = WindowsSpecific;

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
		result := PlainWindow((null + GetWindowLongPtrW(wnd, GWL_USERDATA))^).RunWindowProc(wnd, msg, wparam, lparam);
	end;
	function AnimateWindow(hwnd: HWND; dwTime: DWORD; dwFlags: DWORD): BOOL; stdcall; external user32;

	procedure PlainWindow.Invalidate;
	begin
		handle := 0;
		classh := 0;
		dc := 0;
		rc := 0;
		rcThread := 0;
		oglLoaded := no;
		mouse := UintVec2.Zero;

		activeCursor := DefaultCursor;
		realCursor := DefaultCursor;
		Zero(@cursors, sizeof(cursors));
		openSucceed := no;
		active := yes;
		onRepaint := nil; onRepaintParam := nil;
	end;

	procedure PlainWindow.Open;
	var
		wc: WNDCLASSW;
		style: dword;
		onScreen: WindowRect;
		err: dword;
		pfd: PIXELFORMATDESCRIPTOR;
		pixfmt: cint;
	begin
		Invalidate;
		try
			size := ShrinkToAspect((ScreenSize * 2) div 3, ScreenAspect);
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

			handle := CreateWindowW(pWideChar(classn), pWideChar(UTF8Decode('(´・ω・`)')), style,
				onScreen.pos.x, onScreen.pos.y, onScreen.size.x, onScreen.size.y, 0, 0, 0, nil);
			if handle = 0 then raise Win.OperationFailed('открыть окно (CreateWindow)');

			SetLastError(0);
			if SetWindowLongPtrW(handle, GWL_USERDATA, pointer(@self) - NULL) = 0 then
			begin
				err := GetLastError;
				if err <> 0 then raise Win.OperationFailed('выставить пользовательский указатель в окне (SetWindowLongPtrW)');
			end;

			AnimateWindow(handle, 200, AW_ACTIVATE or AW_BLEND);

			dc := GetDC(handle);
			if dc = 0 then raise Win.OperationFailed('получить контекст устройства (GetDC)');

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

			if not SetPixelFormat(dc, pixfmt, @pfd) then raise Win.OperationFailed('выставить формат вывода картинки (SetPixelFormat)');

			rc := wglCreateContext(dc);
			if rc = 0 then raise Win.OperationFailed('создать GL-контекст (wglCreateContext)');

			if not wglMakeCurrent(dc, rc) then raise Win.OperationFailed('привязать GL-контекст к потоку');
			rcThread := Thread.Current;

			gl.Load;
			rox_gfx.InitGL;
			oglLoaded := yes;

			cursors[DefaultCursor] := Windows.LoadCursor(0, IDC_ARROW);
			if cursors[DefaultCursor] = 0 then raise Win.OperationFailed('загрузить обычный указатель (LoadCursor)');

			cursors[Cursor0] := LoadCursor(Paths.Data + 'cursors/Pulse_Glass.ani');
			cursors[Cursor1] := LoadCursor(Paths.Data + 'cursors/Pulse_Glass_Working.ani');
			openSucceed := yes;
		except
			Close;
			raise;
		end;
	end;

	procedure PlainWindow.Close;
	begin
		CleanupRenderingContext;
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
	end;

	function PlainWindow.Process: boolean;
	var
		message: MSG;
		key: KeyboardKey;
		down: boolean;
	begin
		wasDeactivated := no;
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
					begin
						down := (message.message = WM_SYSKEYDOWN) or (message.message = WM_KEYDOWN);
						if Win.DecryptKey(message.wparam, key) then
						{$ifdef use_console} Con.WriteLine(KeyboardKeyIds[key]) {$endif};
					end;
				WM_MOUSEMOVE: mouse := UintVec2.Make(GET_X_LPARAM(message.lParam), GET_Y_LPARAM(message.lParam));
				WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_LBUTTONUP, WM_RBUTTONUP: ;
				WM_MOUSEWHEEL:
					begin
					{$ifdef use_console} Con.WriteLine(ToString(TWMMouseWheel(addr(message.message)^).WheelDelta / WHEEL_DELTA)); {$endif}
					end;
			end;
			TranslateMessage(message);
			DispatchMessage(message);
		until no;
	end;

	procedure PlainWindow.SwapBuffers;
	begin
		Windows.SwapBuffers(dc);
	end;

	function PlainWindow.QueryRect(client: boolean): WindowRect;
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

	function PlainWindow.ClientToScreen(const rect: WindowRect; style: dword): WindowRect;
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

	procedure PlainWindow.CleanupRenderingContext;
	begin
		if oglLoaded then
		begin
			rox_gfx.DoneGL;
			gl.Unload;
			oglLoaded := no;
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

		if dc <> 0 then
		begin
			if ReleaseDC(handle, dc) = 0 then WindowsWarning('освободить контекст устройства (ReleaseDC)');
			dc := 0;
		end;
	end;

	function PlainWindow.LoadCursor(const fn: string): HCURSOR;
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

	procedure PlainWindow.ChangeCursor(cur: HCURSOR);
	begin
		if cur <> 0 then
		begin
			SetLastError(0);
			if (SetClassLongPtrW(handle, GCL_HCURSOR, cur) = 0) and (GetLastError <> 0) then
				Warning(Win.OperationFailedMessage('установить курсор'));
		end;
	end;

	function PlainWindow.RunWindowProc(wnd: hWnd; msg: Windows.UINT; wparam: wParam; lparam: lParam): lResult;
	var
		ht: uint;
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
				begin
					ht := LOWORD(lparam);
					if ht = HTCLIENT then
						if realCursor <> activeCursor then
						begin
							ChangeCursor(cursors[activeCursor]);
							realCursor := activeCursor;
						end else
					else
						if realCursor <> DefaultCursor then
						begin
							ChangeCursor(cursors[DefaultCursor]);
							realCursor := DefaultCursor;
						end;
				end;
			WM_MOVE, WM_SIZE: rect := QueryRect(yes);
			WM_PAINT:
				if Assigned(onRepaint) then onRepaint(onRepaintParam);
			WM_DESTROY:
				begin
					CleanupRenderingContext;
					handle := 0;
					if openSucceed then PostQuitMessage(0);
				end;
			WM_CLOSE: AnimateWindow(handle, 200, AW_HIDE or AW_BLEND);
			WM_CHAR: {$ifdef use_console} UTF8.CodepointToString(wparam) {$endif};
		end;
		Result := DefWindowProcW(wnd, msg, wparam, lparam);
	end;

	procedure Init;
	begin
		bgm.Init;
	end;

	procedure Done;
	begin
		bgm.Done;
	end;

initialization
	window.Invalidate;
	&Unit('rox_mm').Initialize(@Init, @Done);
end.

