unit Windowing;

{$include opts.inc}

interface

uses
	ctypes,
	{$if defined(Windows)} Windows
	{$else} {$error unknown platform}
	{$endif},
	USystem, Utils, UMath, Input, UClasses, Errors {$ifdef Debug}, ULog {$endif};

type
	WindowRect = object
		pos: IntVec2;
		size: UintVec2;
		function Make(const pos: IntVec2; const size: UintVec2): WindowRect; static;
	end;

	WindowFlag =
	(
		window_Centered,
		window_Fullscreen,
		window_ForwardGL,
	{$ifdef Debug} window_DebugGL, {$endif}
		window_VSync,
		window_DestroyingMeansQuit,
		window_QuitRequested,
		window_GotWMQuit,
		window_Active,
		window_InactivatedInFullscreen
	);
	WindowFlags = set of WindowFlag;

	WindowCaption = object
	type
		Cookie = object
			function Empty: Cookie; static;
		private
			uid: uint;
		end;
		LockProc = procedure(lock: boolean; param: pointer);
		UpdateProc = procedure(const cap: string; param: pointer);

		procedure Invalidate;
		procedure Init(onUpdate: UpdateProc; onLock: LockProc; param: pointer);
		procedure Done;
		procedure SetNote(var c: Cookie; const text: string);
		function SetNote(const text: string): Cookie;
		procedure RemoveNote(var c: Cookie);
		function Join: string;

	private type
		NoteDesc = record
			uid: uint;
			text: string;
		end;
	var
		_base, _suffix: string;
		notes: array of NoteDesc;
		onUpdate: UpdateProc;
		onLock: LockProc;
		param: pointer;
		function FindNote(uid: uint; throw: boolean): sint;
		procedure SetBase(const value: string);
		procedure SetSuffix(const value: string);
		procedure SetAndUpdate(var part: string; const value: string; lock: boolean);
	public
		property Base: string read _base write SetBase;
		property Suffix: string read _suffix write SetSuffix;
	end;

	Comd = object
	type
		scoped_enum_ Op = (Open, TiCh, SetFullscreen, Terminate, _CommandCompleted);

		pBase = ^Base;
		Base = object
			op: Op;
		end;

		Open = object(Base) end; pOpen = ^Open;

		pTiCh = ^TiCh;
		TiCh = object(Base)
			title: string;
		end;

		pSetFullscreen = ^SetFullscreen;
		SetFullscreen = object(Base)
			enabled: boolean;
		end;

		Terminate = object(Base) end; pTerminate = ^Terminate;
	end;

	Callback = object
	type
		scoped_enum_ Op = (MMove, MScroll, MClick, MRelease, KClick, KRelease, GButton, GStick); _end

		pBase = ^Base;
		Base = object
			op: Op;
		end;

		pMMove = ^MMove;
		MMove = object(Base)
			pos: UintVec2;
			silent: boolean;
			procedure Setup(const pos: UintVec2; silent: boolean);
		end;

		pMScroll = ^MScroll;
		MScroll = object(Base)
			delta: float;
		end;

		pMButton = ^MButton;
		MButton = object(Base)
			btn: Input.MouseButton;
		end;

		pKKey = ^KKey;
		KKey = object(Base)
			key: USystem.KeyboardKey;
		end;

		pGButton = ^GButton;
		GButton = object(Base)
			bid: sint;
			state: boolean;
			procedure Setup(bid: sint; state: boolean);
		end;

		pGStick = ^GStick;
		GStick = object(Base)
			stick: GamepadInput.Stick;
			state: GamepadInput.StickState;
			procedure Setup(stick: GamepadInput.Stick; state: GamepadInput.StickState);
		end;
	end;

{$define classname:=CommandQueue} {$define item_type:=Command} {$define threading} {$define msg_wait}
{$include heterogenous_queue.h.inc}

	pWindow = ^Window;
	Window = object
	private
		_rect: WindowRect;
		_flags: WindowFlags;
		_thread: Thread;
		_tlock: ThreadLock;
		cmds: CommandQueue;
		immediateLock: ThreadLock;
		cmdFinished: ThreadCV;
		_callbacks: ThreadedHeterogenousQueue;
		_inputEnabled: sint;
		_input: array[0 .. 127] of char;
		_nInput: sint;
		_inputExceptions: KeyboardKeys;
		_filterNextChar: boolean;
		function RecommendMT: boolean;
		procedure _Lock;
		procedure _Unlock;
		procedure _RawSetFlag(flag: WindowFlag; value: boolean);
		function _GetFlag(flag: WindowFlag): boolean;
		procedure _SetFlag(flag: WindowFlag; value: boolean);
		procedure _ForceVSync(value: boolean);
		procedure _SetFullscreen(value, internal: boolean);
		procedure HandleActivation(activated: boolean);
		function _Centerify(lock: boolean): boolean;
		function _BindGLContext: boolean;
		function _UnbindGLContext: boolean;
	{$ifdef Debug} procedure _DprHID; {$endif}

		function _Open: boolean;
		procedure _Close;
		function _ProcessSignals: boolean;
		function LockPutCallback(op: Callback.Op; size: size_t): Callback.pBase;
		procedure UnlockPutCallback;
		function LockPut(op: Comd.Op; size: size_t): Comd.pBase;
		procedure UnlockPut;
		procedure UnlockPutImmediate(cmd: Comd.pBase);
		function ExecuteNext(calledFromThread: boolean): boolean;
	public
		handle: {$ifdef Windows} Windows.HANDLE {$else} {$error Window.handle: ???} {$endif};
		msaaSamples: sint;
		glMajor, glMinor: uint;
		caption: WindowCaption;

		procedure Initialize;
		procedure Finalize;
		function Open: boolean;
		function BindGLContext: boolean;
		function UnbindGLContext: boolean;
		function Process(keyboard: pKeyboardInput; mouse: pMouseInput; gamepad: pGamepadInput): boolean;
		function IsActive: boolean;
		procedure CenterMouse;
		procedure SendQuitSignal;
		procedure SwapBuffers;
		function Centerify: boolean;
		procedure SetPos(const fp: Vec2);
		procedure SetSizes(const fs: Vec2);
		procedure EnableInput(exceptions: KeyboardKeys);
		procedure DisableInput;
		function Input(clear: boolean): string;

		property SizeX: uint read _rect.size.data[0];
		property SizeY: uint read _rect.size.data[1];

		property ForwardGL: boolean index window_ForwardGL read _GetFlag write _SetFlag;
	{$ifdef Debug} property DebugGL: boolean index window_DebugGL read _GetFlag write _SetFlag; {$endif}
		property VSync: boolean index window_VSync read _GetFlag write _SetFlag;
		property Centered: boolean index window_Centered read _GetFlag write _SetFlag;
		property Fullscreen: boolean index window_Fullscreen read _GetFlag write _SetFlag;

	private var
		classNameW: widestring;
		classHandle: Windows.ATOM;
		smallIcon, largeIcon: Windows.HICON;
		function _DecryptKeyboardKey(const message: tMsg; code: sint; out key: KeyboardKey): boolean;

	private var
		_origScreenRect: WindowRect;
		_style, _styleBeforeFullscreen: PtrUint;
		_cursorHidden: boolean;
		_dc: HDC;
		_hrc: HGLRC;
		_hrcThread: {$ifdef Debug} Thread.ID {$else} boolean {$endif};
		_wglGetSwapInterval: function: sint32; stdcall;
		_wglSwapInterval: procedure(interval: sint32); stdcall;
		_wglChoosePixelFormatARB: function(hdc: HDC; piAttribIList: pSint32; pfAttribFList: pFloat32; nMaxFormats: UINT;
			piFormats: pSint32; nNumFormats: pUint): BOOL; stdcall;
		_wglCreateContextAttribsARB: function(dc: HDC; hshareContext: HGLRC; attribs: pSint32): HGLRC; stdcall;
		_gamepads: array of pointer;
		function _PeekVideoMode(const winSize: IntVec2; out mode: DEVMODEW): boolean;
		function _CreateWindow: boolean;
		function _DestroyWindow(nested: boolean): boolean;
		function _SetDCPixelFormat(hdc: HDC; format: pSint32): boolean;
		function _CreateGLContext: boolean;
		function _CreateGLContext1: boolean;
		function _CreateGLContext2: boolean;
		function _CreateHGLRC: HGLRC;
		function ClientRectToScreenRect(const r: WindowRect): WindowRect;
		function _QueryRect(client: boolean): WindowRect;
	end;

	function ScreenSize: UintVec2;
	function GetGLProcAddress(const name: string): pointer;

implementation

uses
	MMSystem;

{$define classname:=CommandQueue} {$include heterogenous_queue.pp.inc}

	function WindowRect.Make(const pos: IntVec2; const size: UintVec2): WindowRect;
	begin
		result.pos := pos;
		result.size := size;
	end;

	function WindowCaption.Cookie.Empty: Cookie;
	begin
		result.uid := 0;
	end;

	procedure WindowCaption.Invalidate;
	begin
		notes := nil;
	end;

	procedure WindowCaption.Init(onUpdate: UpdateProc; onLock: LockProc; param: pointer);
	begin
		self.onUpdate := onUpdate;
		self.onLock := onLock;
		self.param := param;
	end;

	procedure WindowCaption.Done;
	begin
		Assert(length(notes) = 0);
	end;

	procedure WindowCaption.SetNote(var c: Cookie; const text: string);
	var
		i: sint;
	begin
		if Assigned(onLock) then onLock(yes, param);
		try
			if c.uid = 0 then
			begin
				repeat inc(c.uid); until FindNote(c.uid, no) < 0;
				i := length(notes);
				SetLength(notes, i + 1);
				notes[i].uid := c.uid;
			end else
				i := FindNote(c.uid, yes);
			SetAndUpdate(notes[i].text, text, no);
		finally
			if Assigned(onLock) then onLock(no, param);
		end;
	end;

	function WindowCaption.SetNote(const text: string): Cookie;
	begin
		result := Cookie.Empty;
		SetNote(result, text);
	end;

	procedure WindowCaption.RemoveNote(var c: Cookie);
	var
		i: sint;
	begin
		if c.uid = 0 then exit;
		if Assigned(onLock) then onLock(yes, param);
		i := FindNote(c.uid, yes);
		for i := i to High(notes) - 1 do
			notes[i] := notes[i + 1];
		SetLength(notes, length(notes) - 1);
		if Assigned(onUpdate) then onUpdate(Join, param);
		if Assigned(onLock) then onLock(no, param);
		c.uid := 0;
	end;

	function WindowCaption.Join: string;
	var
		i: sint;
		surrounded: string;
	begin
		result := Continued(_base, '   ', _suffix);
		for i := 0 to High(notes) do
		begin
			surrounded := notes[i].text;
			if (length(notes[i].text) = 0) or not (notes[i].text[length(notes[i].text)] in [')', ']']) then
				surrounded := '[' + surrounded + ']';
			result := Continued(result, '   ', surrounded);
		end;
	end;

	function WindowCaption.FindNote(uid: uint; throw: boolean): sint;
	begin
		result := Index(uid, first_field notes _ uid _, length(notes), sizeof(notes[0]));
		if (result < 0) and throw then raise Error('Часть заголовка окна не найдена.');
	end;

	procedure WindowCaption.SetBase(const value: string); begin SetAndUpdate(_base, value, yes); end;
	procedure WindowCaption.SetSuffix(const value: string); begin SetAndUpdate(_suffix, value, yes); end;

	procedure WindowCaption.SetAndUpdate(var part: string; const value: string; lock: boolean);
	begin
		if lock and Assigned(onLock) then onLock(yes, param);
		if value <> part then
		begin
			part := value;
			if Assigned(onUpdate) then onUpdate(Join, param);
		end;
		if lock and Assigned(onLock) then onLock(no, param);
	end;

	procedure Callback.MMove.Setup(const pos: UintVec2; silent: boolean);
	begin
		self.pos    := pos;
		self.silent := silent;
	end;

	procedure Callback.GButton.Setup(bid: sint; state: boolean);
	begin
		self.bid   := bid;
		self.state := state;
	end;

	procedure Callback.GStick.Setup(stick: GamepadInput.Stick; state: GamepadInput.StickState);
	begin
		self.stick := stick;
		self.state := state;
	end;

const
	user32 = 'user32.dll';
	shell32 = 'shell32.dll';
	hidapi = 'hid.dll';

var
	oglLib: DynamicLibrary;
	wglGetProcAddress: function(procName: pChar): pointer; stdcall;

	function AnimateWindow(hwnd: HWND; dwTime: DWORD; dwFlags: DWORD): BOOL; stdcall; external user32;
	function ExtractIconExW(lpszFile: LPCWSTR; nIconIndex: longint; phiconLarge, phiconSmall: Windows.PHANDLE; nIcons: Windows.UINT): Windows.UINT;
		stdcall; external shell32;

const
{$ifdef Debug}
	RIDI_DEVICENAME = $20000007;
	RIDI_DEVICEINFO = $2000000b;
{$endif}
	RIDI_PREPARSEDDATA = $20000005;

{$ifdef Debug}
	RIM_TYPEMOUSE    = 0;
	RIM_TYPEKEYBOARD = 1;
	RIM_TYPEHID      = 2;
{$endif}

{$ifdef Debug}
type
	pRawInputDeviceList = ^RawInputDeviceList;
	RawInputDeviceList = record
		hDevice: Windows.HANDLE;
		dwType:  Windows.DWORD;
	end;

	RIDeviceInfo_Mouse = record
		dwId: DWORD;
		dwNumberOfButtons: DWORD;
		dwSampleRate: DWORD;
		fHasHorizontalWheel: Windows.BOOL;
	end;

	RIDeviceInfo_Keyboard = record
		dwType: DWORD;
		dwSubType: DWORD;
		dwKeyboardMode: DWORD;
		dwNumberOfFunctionKeys: DWORD;
		dwNumberOfIndicators: DWORD;
		dwNumberOfKeysTotal: DWORD;
	end;

	RIDeviceInfo_Hid = record
		dwVendorId: DWORD;
		dwProductId: DWORD;
		dwVersionNumber: DWORD;
		usUsagePage: Windows.USHORT;
		usUsage: Windows.USHORT;
	end;

	RidDeviceInfo = record
		cbSize: DWORD;
	case dwType: DWORD of
		RIM_TYPEMOUSE:    (mouse:    RIDeviceInfo_Mouse);
		RIM_TYPEKEYBOARD: (keyboard: RIDeviceInfo_Keyboard);
		RIM_TYPEHID:      (hid:      RIDeviceInfo_Hid);
	end;
{$endif}

const
	RIDEV_DEVNOTIFY = $00002000;

type
	pRawInputDevice = ^RawInputDevice;
	RawInputDevice = record
		usUsagePage: Windows.USHORT;
		usUsage: Windows.USHORT;
		dwFlags: DWORD;
		hwndTarget: HWND;
	end;

	RawInputHeader = record
		dwType: DWORD;
		dwSize: DWORD;
		hDevice: HANDLE;
		wParam: WPARAM;
	end;

	RawMouse = record
		usFlags:        Windows.USHORT;
		usButtonFlags:  Windows.USHORT;
		usButtonData:   Windows.USHORT;
		ulRawButtons:   Windows.ULONG;
		lLastX, lLastY: Windows.LONG;
		ulExtraInformation: Windows.ULONG;
	end;

	RawKeyboard = record
		MakeCode: Windows.USHORT;
		Flags:    Windows.USHORT;
		Reserved: Windows.USHORT;
		VKey:     Windows.USHORT;
		Message:  Windows.UINT;
		ExtraInformation: Windows.ULONG;
	end;

	RawHid = record
		dwSizeHid: DWORD;
		dwCount: DWORD;
		bRawData: array[0 .. 0] of byte;
	end;

	pRawInput = ^RawInput;
	RawInput = record
		header: RawInputHeader;
	case byte of
		0: (mouse: RawMouse);
		1: (keyboard: RawKeyboard);
		2: (hid: RawHid);
	end;

{$push} {$packrecords c}
	HidpCaps = record
		Usage: Windows.USHORT;
		UsagePage: Windows.USHORT;
		InputReportByteLength: Windows.USHORT;
		OutputReportByteLength: Windows.USHORT;
		FeatureReportByteLength: Windows.USHORT;
		Reserved: array[0 .. 16] of Windows.USHORT;
		NumberLinkCollectionNodes: Windows.USHORT;
		NumberInputButtonCaps: Windows.USHORT;
		NumberInputValueCaps: Windows.USHORT;
		NumberInputDataIndices: Windows.USHORT;
		NumberOutputButtonCaps: Windows.USHORT;
		NumberOutputValueCaps: Windows.USHORT;
		NumberOutputDataIndices: Windows.USHORT;
		NumberFeatureButtonCaps: Windows.USHORT;
		NumberFeatureValueCaps: Windows.USHORT;
		NumberFeatureDataIndices: Windows.USHORT;
	end;

	pHidpButtonCaps = ^HidpButtonCaps;
	HidpButtonCaps = record
		UsagePage: Windows.USHORT;
		ReportID:  Windows.UCHAR;
		IsAlias:   ByteBool;

		BitField: Windows.USHORT;
		LinkCollection: Windows.USHORT;   // A unique internal index pointer

		LinkUsage: Windows.USHORT;
		LinkUsagePage: Windows.USHORT;

		IsRange: ByteBool;
		IsStringRange: ByteBool;
		IsDesignatorRange: ByteBool;
		IsAbsolute: ByteBool;

		Reserved: array[0 .. 9] of Windows.ULONG;
	case byte of
		0:
			(
				UsageMin, UsageMax: Windows.USHORT;
				StringMin, StringMax: Windows.USHORT;
				DesignatorMin, DesignatorMax: Windows.USHORT;
				DataIndexMin, DataIndexMax: Windows.USHORT;
			);
		1:
			(
				Usage, Reserved1: Windows.USHORT;
				StringIndex, Reserved2: Windows.USHORT;
				DesignatorIndex, Reserved3: Windows.USHORT;
				DataIndex, Reserved4: Windows.USHORT;
			);
	end;

	pHidpValueCaps = ^HidpValueCaps;
	HidpValueCaps = record
		UsagePage: Windows.USHORT;
		ReportID: Windows.UCHAR;
		IsAlias: ByteBool;

		BitField: Windows.USHORT;
		LinkCollection: Windows.USHORT;

		LinkUsage: Windows.USHORT;
		LinkUsagePage: Windows.USHORT;

		IsRange: ByteBool;
		IsStringRange: ByteBool;
		IsDesignatorRange: ByteBool;
		IsAbsolute: ByteBool;

		HasNull: ByteBool;
		Reserved: Windows.UCHAR;
		BitSize: Windows.USHORT;

		ReportCount: Windows.USHORT;
		Reserved1: array[0 .. 4] of Windows.USHORT;

		UnitsExp: Windows.ULONG;
		Units: Windows.ULONG;

		LogicalMin, LogicalMax: Windows.LONG;
		PhysicalMin, PhysicalMax: Windows.LONG;

	case byte of
		0:
			(
				UsageMin, UsageMax: Windows.USHORT;
				StringMin, StringMax: Windows.USHORT;
				DesignatorMin, DesignatorMax: Windows.USHORT;
				DataIndexMin, DataIndexMax: Windows.USHORT;
			);
		1:
			(
				Usage, Reserved2: Windows.USHORT;
				StringIndex, Reserved3: Windows.USHORT;
				DesignatorIndex, Reserved4: Windows.USHORT;
				DataIndex, Reserved5: Windows.USHORT;
			)
	end;
{$pop}

type
	NTSTATUS = Windows.LONG;

const
	GIDC_ARRIVAL = 1;
	GIDC_REMOVAL = 2;

	RID_INPUT  = $10000003;
	HIDP_STATUS_SUCCESS = NTSTATUS(1114112);

{$ifdef Debug}
	HID_USAGE_PAGE_UNDEFINED = $0;
{$endif}
	HID_USAGE_PAGE_GENERIC = $1;
{$ifdef Debug}
	HID_USAGE_GENERIC_UNDEFINED = $0;
	HID_USAGE_GENERIC_POINTER  = $1;
	HID_USAGE_GENERIC_MOUSE    = $2;
{$endif}
	HID_USAGE_GENERIC_JOYSTICK = $4;
{$ifdef Debug}
	HID_USAGE_GENERIC_GAMEPAD  = $5;
	HID_USAGE_GENERIC_KEYBOARD = $6;
	HID_USAGE_GENERIC_KEYPAD   = $7;
{$endif}
	HID_USAGE_GENERIC_X = $30;
	HID_USAGE_GENERIC_Y = $31;
	HID_USAGE_GENERIC_Z = $32;
{$ifdef Debug}
	HID_USAGE_GENERIC_RX = $33;
	HID_USAGE_GENERIC_RY = $34;
{$endif}
	HID_USAGE_GENERIC_RZ = $35;
{$ifdef Debug}
	HID_USAGE_GENERIC_SLIDER = $36;
	HID_USAGE_GENERIC_DIAL = $37;
	HID_USAGE_GENERIC_WHEEL = $38;
	HID_USAGE_GENERIC_HATSWITCH = $39;
{$endif}
	HID_USAGE_PAGE_BUTTON = $9;

	HidP_Input   = 0;
	HidP_Output  = 1;
{$ifdef Debug} HidP_Feature = 2; {$endif}

{$ifdef Debug}
	function GetRawInputDeviceList(pRawInputDeviceList: pRawInputDeviceList; var uiNumDevices: Windows.UINT; cbSize: Windows.UINT): Windows.UINT; stdcall; external user32;
{$endif}
	function GetRawInputDeviceInfoW(hDevice: Windows.HANDLE; uiCommand: Windows.UINT; pData: pointer; var pcbSize: Windows.UINT): Windows.UINT; stdcall; external user32;
	function RegisterRawInputDevices(pRawInputDevices: pRawInputDevice; uiNumDevices: Windows.UINT; cbSize: Windows.UINT): Windows.BOOL; stdcall; external user32;
	function GetRawInputData(hRawInput: Windows.HANDLE; uiCommand: Windows.UINT; pData: pointer; var pcbSize: Windows.UINT; cbSizeHeader: Windows.UINT): Windows.UINT; stdcall; external user32;

	function HidP_GetCaps(PreparsedData: pointer; out Capabilities: HidpCaps): NTSTATUS; stdcall; external hidapi;
	function HidP_GetButtonCaps(ReportType: cint; ButtonCaps: pHidpButtonCaps; var ButtonCapsLength: Windows.USHORT; PreparsedData: pointer): NTSTATUS; stdcall; external hidapi;
	function HidP_GetValueCaps(ReportType: cint; ValueCaps: pHidpValueCaps; var ValueCapsLength: Windows.USHORT; PreparsedData: pointer): NTSTATUS; stdcall; external hidapi;
	function HidP_GetUsages(ReportType: cint; UsagePage: Windows.USHORT; LinkCollection: Windows.USHORT;
		UsageList: Windows.PUSHORT; var UsageLength: Windows.ULONG; PreparsedData: pointer; Report: pointer;
		ReportLength: Windows.ULONG): NTSTATUS; stdcall; external hidapi;
	function HidP_GetUsageValue(ReportType: cint; UsagePage: Windows.USHORT; LinkCollection: Windows.USHORT; Usage: Windows.USHORT;
		out UsageValue: Windows.ULONG; PreparsedData: pointer; Report: pointer; ReportLength: Windows.ULONG): NTSTATUS; stdcall; external hidapi;
	function HidP_SetUsageValue(ReportType: cint; UsagePage: Windows.USHORT; LinkCollection: Windows.USHORT; Usage: Windows.USHORT;
		UsageValue: Windows.ULONG; PreparsedData: pointer; Report: pointer; ReportLength: Windows.ULONG): NTSTATUS; stdcall; external hidapi;
	function HidD_SetOutputReport(HidDeviceObject: Windows.HANDLE; ReportBuffer: pointer; ReportBufferLength: Windows.ULONG): ByteBool; stdcall; external hidapi;

{$ifdef Debug}
	function HidUsagePageDesc(page: Windows.USHORT): string;
	begin
		case page of
			HID_USAGE_PAGE_UNDEFINED: result := 'UNDEFINED';
			HID_USAGE_PAGE_GENERIC:   result := 'GENERIC';
			HID_USAGE_PAGE_BUTTON:    result := 'BUTTON';
			else result := '?';
		end;
		result += Format(' ({0})', [page]);
	end;

	function HidUsageDesc(page: Windows.USHORT; usage: Windows.USHORT): string;
	begin
		result := '?';
		case page of
			HID_USAGE_PAGE_GENERIC:
				case usage of
					HID_USAGE_GENERIC_UNDEFINED: result := 'UNDEFINED';
					HID_USAGE_GENERIC_POINTER:   result := 'POINTER';
					HID_USAGE_GENERIC_MOUSE:     result := 'MOUSE';
					HID_USAGE_GENERIC_JOYSTICK:  result := 'JOYSTICK';
					HID_USAGE_GENERIC_GAMEPAD:   result := 'GAMEPAD';
					HID_USAGE_GENERIC_KEYBOARD:  result := 'KEYBOARD';
					HID_USAGE_GENERIC_KEYPAD:    result := 'KEYPAD';
					HID_USAGE_GENERIC_X:         result := 'X';
					HID_USAGE_GENERIC_Y:         result := 'Y';
					HID_USAGE_GENERIC_Z:         result := 'Z';
					HID_USAGE_GENERIC_RX:        result := 'RX';
					HID_USAGE_GENERIC_RY:        result := 'RY';
					HID_USAGE_GENERIC_RZ:        result := 'RZ';
					HID_USAGE_GENERIC_SLIDER:    result := 'SLIDER';
					HID_USAGE_GENERIC_DIAL:      result := 'DIAL';
					HID_USAGE_GENERIC_WHEEL:     result := 'WHEEL';
					HID_USAGE_GENERIC_HATSWITCH: result := 'HATSWITCH';
				end;
		end;
		result += Format(' (P{0}/U{1})', [page, usage]);
	end;
{$endif}

type
	pGamepadInternal = ^GamepadInternal;
	GamepadInternal = object
	type
		pButtonCap = ^ButtonCap;
		ButtonCap = record
			caps: HidpButtonCaps;
			usages: array of Windows.USHORT;
			nPressed: sint;
			pressed: array of sint;
		end;
		pValueCap = ^ValueCap;
		ValueCap = record
			caps: HidpValueCaps;
			hasValue: boolean;
			value: Windows.ULONG;
		end;
	var
		device: Windows.HANDLE;
		caps: HidpCaps;
		bcaps: array of ButtonCap;
		vcaps, ovcaps: array of ValueCap;
		sticks: array[GamepadInput.Stick] of record
			state: GamepadInput.StickState;
			changed: boolean;
		end;
		preparsed: pointer;
		function Initialize(newHandle: Windows.HANDLE): boolean;
		procedure Finalize;
		procedure SetStick(stick: GamepadInput.Stick; dim: sint; const value: float);
		procedure UpdateSticks(win: pWindow);
	end;

	function GamepadInternal.Initialize(newHandle: Windows.HANDLE): boolean;
	{$ifdef Debug}
		function VcapDump(what, id: sint; const caps: HidpValueCaps): string;
		const
			WhatDesc: array[HidP_Input .. HidP_Feature] of string = ('', 'Output ', 'Feature ');
		begin
			result := WhatDesc[what] + 'Value Cap #' + ToString(id) + ': page = ' + HidUsagePageDesc(caps.UsagePage) +
				', usage = ' + HidUsageDesc(caps.UsagePage, caps.Usage) +
				', LogicalMin = ' + ToString(caps.LogicalMin) + ', LogicalMax = ' + ToString(caps.LogicalMax) +
				', PhysicalMin = ' + ToString(caps.PhysicalMin) + ', PhysicalMax = ' + ToString(caps.PhysicalMax);
		end;
	{$endif}

	label _finally_;
	var
	{$ifdef Debug} name: array[0 .. 255] of widechar; nameLen: Windows.UINT; {$endif}
		preparsedSize: Windows.UINT;
		bcaps_caps: array of HidpButtonCaps;
		vcaps_caps: array of HidpValueCaps;
		ncaps: Windows.USHORT;
		i: sint;
		stick: GamepadInput.Stick;
	begin
		result := no;
		device := newHandle;
		preparsedSize := 0;
		preparsed := nil;
		for stick in GamepadInput.Stick do
		begin
			sticks[stick].state := Vec2.Zero;
			sticks[stick].changed := no;
		end;
	{$ifdef Debug} Log('Обнаружен геймпад, handle = {0}.', [device]); {$endif}

	{$ifdef Debug}
		nameLen := length(name);
		if GetRawInputDeviceInfoW(device, RIDI_DEVICENAME, @name[0], nameLen) = High(Windows.UINT) then
		begin
			Log('Ошибка GetRawInputDeviceInfo(RIDI_DEVICENAME), код {0}.', [GetLastError], logError);
			goto _finally_;
		end;
	{$endif}

		if GetRawInputDeviceInfoW(device, RIDI_PREPARSEDDATA, nil, preparsedSize) = High(Windows.UINT) then
		begin
		{$ifdef Debug} Log('Ошибка GetRawInputDeviceInfo(RIDI_PREPARSEDDATA), код {0}.', [GetLastError], logError); {$endif}
			goto _finally_;
		end;

		preparsed := GetMem(preparsedSize);
		if GetRawInputDeviceInfoW(device, RIDI_PREPARSEDDATA, preparsed, preparsedSize) = High(Windows.UINT) then goto _finally_;

		if HidP_GetCaps(preparsed, caps) <> HIDP_STATUS_SUCCESS then
		begin
		{$ifdef Debug} Log('Ошибка HidP_GetCaps.', logError); {$endif}
			goto _finally_;
		end;

	{$ifdef Debug}
		Log('ButtonCaps: input = {0}, output = {1}, feature = {2}.' + EOL +
			'ValueCaps: input = {3}, output = {4}, feature = {5}.' + EOL +
			'TLC = "{6}".',
			[caps.NumberInputButtonCaps, caps.NumberOutputButtonCaps, caps.NumberFeatureButtonCaps,
			caps.NumberInputValueCaps, caps.NumberOutputValueCaps, caps.NumberFeatureValueCaps,
			UTF8Encode(Copy(name, 0, nameLen))]);
	{$endif}

		ncaps := caps.NumberInputButtonCaps;
		SetLength(bcaps_caps, ncaps);
		if HidP_GetButtonCaps(HidP_Input, pHidpButtonCaps(bcaps_caps), ncaps, preparsed) <> HIDP_STATUS_SUCCESS then
		begin
		{$ifdef Debug} Log('Ошибка HidP_GetButtonCaps.', logError); {$endif}
			goto _finally_;
		end;
		Assert(ncaps = length(bcaps_caps));
		SetLength(bcaps, ncaps);

		for i := 0 to High(bcaps) do
		begin
			bcaps[i].caps := bcaps_caps[i];
			SetLength(bcaps[i].usages, bcaps[i].caps.UsageMax - bcaps[i].caps.UsageMin + 1);
			SetLength(bcaps[i].pressed, length(bcaps[i].usages));
			bcaps[i].nPressed := 0;
		{$ifdef Debug}
			Log('Button Cap #{0}: page = {1}, usage = {2}, UsageMin = {3}, UsageMax = {4}.',
				[i, HidUsagePageDesc(bcaps[i].caps.UsagePage), HidUsageDesc(bcaps[i].caps.UsagePage, bcaps[i].caps.Usage),
				bcaps[i].caps.UsageMin, bcaps[i].caps.UsageMax], logDebug);
		{$endif}
		end;

		ncaps := caps.NumberInputValueCaps;
		SetLength(vcaps_caps, ncaps);
		if HidP_GetValueCaps(HidP_Input, pHidpValueCaps(vcaps_caps), ncaps, preparsed) <> HIDP_STATUS_SUCCESS then
		begin
		{$ifdef Debug} Log('Ошибка HidP_GetValueCaps', logError); {$endif}
			goto _finally_;
		end;
		Assert(ncaps = length(vcaps_caps));
		SetLength(vcaps, ncaps);

		for i := 0 to High(vcaps) do
		begin
			vcaps[i].caps := vcaps_caps[i];
			vcaps[i].hasValue := no;
		{$ifdef Debug} Log(VcapDump(HidP_Input, i, vcaps[i].caps), logDebug); {$endif}
		end;

		ncaps := caps.NumberOutputValueCaps;
		if ncaps > 0 then
		begin
			SetLength(vcaps_caps, ncaps);
			if HidP_GetValueCaps(HidP_Output, pHidpValueCaps(vcaps_caps), ncaps, preparsed) <> HIDP_STATUS_SUCCESS then
			begin
			{$ifdef Debug} Log('Ошибка HidP_GetValueCaps(HidP_Output)', logError); {$endif}
			end else
			begin
				SetLength(ovcaps, ncaps);

				for i := 0 to High(ovcaps) do
				begin
					ovcaps[i].caps := vcaps_caps[i];
				{$ifdef Debug} Log(VcapDump(HidP_Output, i, ovcaps[i].caps), logDebug); {$endif}
					end;
			end;
		end;

		result := yes;
	_finally_:
		if not result then
		begin
			device := 0;
			Finalize;
		end;
	end;

	procedure GamepadInternal.Finalize;
	begin
		FreeMem(preparsed);
		bcaps := nil;
		vcaps := nil;
		ovcaps := nil;
	{$ifdef Debug} if device <> 0 then Log('Внутренняя информация о геймпаде (handle = ' + ToString(device) + ') уничтожена', logDebug); {$endif}
	end;

	procedure GamepadInternal.SetStick(stick: GamepadInput.Stick; dim: sint; const value: float);
	begin
		if not Equals(sticks[stick].state.data[dim], value) then
		begin
			sticks[stick].changed := yes;
			sticks[stick].state.data[dim] := value;
		end;
	end;

	procedure GamepadInternal.UpdateSticks(win: pWindow);
	var
		stick: GamepadInput.Stick;
	begin
		for stick in GamepadInput.Stick do
			if sticks[stick].changed then
			begin
				sticks[stick].changed := no;
				Callback.pGStick(win^.LockPutCallback(Callback.Op.GStick, sizeof(Callback.GStick)))^.Setup(stick, sticks[stick].state);
				win^.UnlockPutCallback;
			end;
	end;

	procedure _HandleWMInput(win: pWindow; wnd: hWnd; rim: sint; rih: Windows.HANDLE);
	label _finally_;
	var
		rawSize: Windows.UINT;
		raw: pRawInput;
		gp: pGamepadInternal;
		i, j, k, cid: sint;
		nusages: Windows.ULONG;
		pressed: boolean;
		bcap: GamepadInternal.pButtonCap;
		vcap: GamepadInternal.pValueCap;
		uvalue: Windows.ULONG;
		delta: sint;
		value: float;
	begin
		Assert(rim = 0, 'RIM_INPUTSINK assumed to be disabled!'); // 0 = RIM_INPUT, 1 = RIM_INPUTSINK
		Assert((@win = @win) and (@wnd = @wnd));

		raw := nil;
		rawSize := 0;
		if GetRawInputData(rih, RID_INPUT, nil, rawSize, sizeof(RawInputHeader)) = High(Windows.UINT) then
		begin
		{$ifdef Debug} Log('Ошибка GetRawInputData(RID_INPUT), код ' + ToString(GetLastError), logError); {$endif}
			goto _finally_;
		end;
		raw := GetMem(rawSize);
		if GetRawInputData(rih, RID_INPUT, raw, rawSize, sizeof(RawInputHeader)) = High(Windows.UINT) then goto _finally_;

		gp := nil;
		for i := 0 to High(win^._gamepads) do
			if pGamepadInternal(win^._gamepads[i])^.device = raw^.header.hDevice then
			begin
				gp := win^._gamepads[i];
				break;
			end;

		// XP не поддерживает WM_INPUT_DEVICE_CHANGE.
		if not Assigned(gp) then
		begin
		{$ifdef Debug} Log('WM_INPUT на незарегистрированном геймпаде: handle = ' + ToString(raw^.header.hDevice) + ', регистрирую...', logWarning); {$endif}
			new(gp);
			if gp^.Initialize(raw^.header.hDevice) then
			begin
				SetLength(win^._gamepads, length(win^._gamepads) + 1);
				win^._gamepads[High(win^._gamepads)] := gp;
			end else
			begin
				dispose(gp);
				goto _finally_;
			end;
		end;

		for i := 0 to High(gp^.bcaps) do
		begin
			bcap := @gp^.bcaps[i];
			nusages := length(bcap^.usages);
			if HidP_GetUsages(HidP_Input, bcap^.caps.UsagePage, 0, Windows.PUSHORT(bcap^.usages), nusages, gp^.preparsed, @raw^.hid.bRawData, raw^.hid.dwSizeHid) <> HIDP_STATUS_SUCCESS then
			begin
			{$ifdef Debug} Log('Ошибка HidP_GetUsages', logWarning); {$endif}
				continue;
			end;

			case bcap^.caps.UsagePage of
				HID_USAGE_PAGE_BUTTON:
					begin
						// Отпускание кнопок, запомненных как нажатые, но отсутствующих в репорте.
						for j := bcap^.nPressed - 1 downto 0 do
						begin
							pressed := no;
							for k := 0 to sint(nusages) - 1 do
								if bcap^.pressed[j] = bcap^.usages[k] - bcap^.caps.UsageMin then
								begin
									pressed := yes;
									break;
								end;

							if not pressed then
							begin
								Callback.pGButton(win^.LockPutCallback(Callback.Op.GButton, sizeof(Callback.GButton)))^.Setup(
									bcap^.pressed[j], no);
								win^.UnlockPutCallback;

								bcap^.pressed[j] := bcap^.pressed[bcap^.nPressed - 1];
								dec(bcap^.nPressed);
							end;
						end;

						// Нажатие новых кнопок.
						for j := 0 to sint(nusages) - 1 do
						begin
							cid := gp^.bcaps[i].usages[j] - gp^.bcaps[i].caps.UsageMin;
							if Index(cid, pSint(bcap^.pressed), bcap^.nPressed) < 0 then
							begin
								Callback.pGButton(win^.LockPutCallback(Callback.Op.GButton, sizeof(Callback.GButton)))^.Setup(cid, yes);
								win^.UnlockPutCallback;

								inc(bcap^.nPressed);
								bcap^.pressed[bcap^.nPressed - 1] := cid;
							end;
						end;
					end;
				else
					// контролы, не принадлежащие HID_USAGE_PAGE_BUTTON — тихо проигнорировать
					;
			end;
		end;

		for i := 0 to High(gp^.vcaps) do
		begin
			vcap := @gp^.vcaps[i];
			if HidP_GetUsageValue(HidP_Input, vcap^.caps.UsagePage, 0, vcap^.caps.Usage, uvalue, gp^.preparsed, @raw^.hid.bRawData, raw^.hid.dwSizeHid) <> HIDP_STATUS_SUCCESS then
			begin
			{$ifdef Debug} Log('Ошибка HidP_GetUsageValue', logWarning); {$endif}
				continue;
			end;

			if (not vcap^.hasValue) or (vcap^.value <> uvalue) then
			begin
				vcap^.hasValue := yes;
				vcap^.value := uvalue;

				case vcap^.caps.UsagePage of
					HID_USAGE_PAGE_GENERIC:
						begin
							case vcap^.caps.Usage of
								HID_USAGE_GENERIC_X, HID_USAGE_GENERIC_Y, HID_USAGE_GENERIC_Z, HID_USAGE_GENERIC_RZ:
									begin
										delta := vcap^.caps.LogicalMax - vcap^.caps.LogicalMin;
										// ремапнуть середину в 0, если она вдруг не точно посередине (вроде 128/255).
										if (delta >= 6) and (abs(sint(uvalue) - delta div 2) <= 1) then
											value := 0.0
										else
											value := clamp(-1 + 2 * ((uvalue - vcap^.caps.LogicalMin) / delta), -1, 1);

										case vcap^.caps.Usage of
											HID_USAGE_GENERIC_X: gp^.SetStick(LeftStick, 0, value);
											HID_USAGE_GENERIC_Y: gp^.SetStick(LeftStick, 1, -value);
											HID_USAGE_GENERIC_Z: gp^.SetStick(RightStick, 0, value);
											HID_USAGE_GENERIC_RZ: gp^.SetStick(RightStick, 1, -value);
											else Assert(no);
										end;
									end;
							end;
						end;
					// не-HID_USAGE_PAGE_GENERIC — игнор
				end;
			end;
		end;
		gp^.UpdateSticks(win);

	_finally_:
		FreeMem(raw);
	end;

	function WindowProc(wnd: hWnd; msg: Windows.UINT; wparam: wParam; lparam: lParam): lResult; stdcall;
	var
		ht: sint;
		lp: LONG_PTR;
		win: pWindow;
		gp: pGamepadInternal;
		i: sint;
	begin
		lp := GetWindowLongPtrW(wnd, GWL_USERDATA);
		win := pPointer(@lp)^;
		case msg of
			WM_Activate: win^.HandleActivation(LOWORD(wparam) <> WA_INACTIVE);
			WM_SetCursor:
				begin
					win^._Lock;
					ht := LOWORD(lparam);
					if (ht = HTCLIENT) and not win^._cursorHidden then
					begin
						win^._cursorHidden := yes;
						ShowCursor(no);
					end else
					if (ht <> HTCLIENT) and win^._cursorHidden then
					begin
						win^._cursorHidden := no;
						ShowCursor(yes);
					end;
					win^._Unlock;
				end;
			WM_Move, WM_Size:
				begin
					win^._Lock;
					win^._rect := win^._QueryRect(yes);
				{$ifdef Debug}
					if msg = WM_Move then
					begin
						Log('Координаты окна изменены: ' + ToString(win^._QueryRect(no).pos) + ', клиентская область ' + ToString(win^._QueryRect(yes).pos));
					end else
						Log('Размеры окна изменены: ' + SizeToString(win^._QueryRect(no).size) + ', клиентская область ' + ToString(win^._QueryRect(yes).size));
				{$endif}
					win^._Unlock;
				end;
			WM_Destroy:
				begin
					win^._Lock;
				{$IFDEF Debug} LogR('Получено сообщение WM_Destroy; ', logDebug); {$ENDIF}
					win^._DestroyWindow(yes);
					if window_DestroyingMeansQuit in win^._flags then
					begin
					{$ifdef Debug} LogR('Посылаю приложению WM_Quit; ', logDebug); {$endif}
						PostQuitMessage(0);
					end;
					win^._Unlock;
				end;
			WM_Close: if not win^.Fullscreen then AnimateWindow(wnd, 200, AW_HIDE or AW_BLEND);
			WM_Input:
				if win^.IsActive then
				begin
					win^._Lock;
					_HandleWMInput(win, wnd, wparam, lparam);
					win^._Unlock;
				end;
			WM_Input_Device_Change:
				begin
				{$ifdef Debug}
					LogR('WM_InputDeviceChange: ', logDebug);
					case wParam of
						GIDC_ARRIVAL: LogR('+', logDebug);
						GIDC_REMOVAL: LogR('-', logDebug);
						else Log('?', logWarning);
					end;
					Log(ToString(lParam), logDebug);
				{$endif}

					win^._Lock;
					case wParam of
						GIDC_ARRIVAL:
							begin
								new(gp);
								if gp^.Initialize(lParam) then
								begin
									SetLength(win^._gamepads, length(win^._gamepads) + 1);
									win^._gamepads[High(win^._gamepads)] := gp;
								end else
									dispose(gp);
							end;
						GIDC_REMOVAL:
							begin
								for i := 0 to High(win^._gamepads) do
									if pGamepadInternal(win^._gamepads[i])^.device = Windows.HANDLE(lParam) then
									begin
										gp := win^._gamepads[i];
										gp^.Finalize;
										dispose(gp);
										win^._gamepads[i] := win^._gamepads[High(win^._gamepads)];
										SetLength(win^._gamepads, length(win^._gamepads) - 1);
									end;
							end;
					end;
					win^._Unlock;
				end;
			WM_CHAR:
				begin
					win^._Lock;
					if win^._inputEnabled > 0 then
						if win^._filterNextChar then
							win^._filterNextChar := no
						else
						begin
							if (wparam >= 32) or (wparam = ord(TabSym)) or (wparam = ord(Backspace)) or (wparam = 13) then
							begin
								if wparam = 13 then wparam := ord(EOL);
								if win^._nInput + UTF8.MaxCharLen <= length(win^._input) then
									win^._nInput += UTF8.CodepointToString(wparam, @win^._input[win^._nInput]);
							end;
						end;
					win^._Unlock;
				end;
		end;
		Result := DefWindowProcW(wnd, msg, wparam, lparam);
	end;

	procedure OnUpdateCaption(const cap: string; param: pointer);
	var
		TiCh: Comd.pTiCh;
	begin
		if pWindow(param)^.handle <> 0 then
		begin
			TiCh := Comd.pTiCh(pWindow(param)^.LockPut(Comd.Op.TiCh, sizeof(Comd.TiCh)));
			System.Initialize(TiCh^);
			TiCh^.title := cap;
			pWindow(param)^.UnlockPut;
		end;
	end;

	procedure OnLockUnlockCaption(lock: boolean; param: pointer);
	begin
		if lock then pWindow(param)^._Lock else pWindow(param)^._Unlock;
	end;

	procedure Window.Initialize;
	begin
		_flags := [window_Centered, window_ForwardGL {$ifdef Debug}, window_DebugGL {$endif}];
		handle := 0;
		_hrc := 0;
		_hrcThread := {$ifdef Debug} 0 {$else} no {$endif};
		_cursorHidden := no;
		_rect := WindowRect.Make(IntVec2.Zero, UintVec2.Make(640, 480));
		_tlock.Init;
		_thread := Thread.Invalid;
		cmds.Init;
		immediateLock.Init;
		cmdFinished.Init;
		_callbacks.Init;
		caption.Init(@OnUpdateCaption, @OnLockUnlockCaption, @self);
		_inputEnabled := 0;
		caption.base := '%% :3 %%';

		msaaSamples := 0;
		glMajor := gl.REC_MAJOR_VERSION;
		glMinor := gl.REC_MINOR_VERSION;

		smallIcon := 0;
		largeIcon := 0;
		classHandle := 0;
		classNameW := UTF8Decode('／人◕ ‿‿ ◕人＼');
		_style := 0;
		_gamepads := nil;

	{$ifdef Debug} _DprHID; {$endif}
	end;

	procedure Window.Finalize;
	var
		i: sint;
	begin
		if _thread.OK then
		begin
			LockPut(Comd.Op.Terminate, sizeof(Comd.Terminate)); UnlockPut;
			_thread.Close;
		end else
			while (handle <> 0) and not (window_GotWMQuit in _flags) and _ProcessSignals do;
		_Close;

		for i := 0 to High(_gamepads) do
		begin
			pGamepadInternal(_gamepads[i])^.Finalize;
			dispose(pGamepadInternal(_gamepads[i]));
		end;
		_gamepads := nil;
		caption.Done;
		_callbacks.Done;
		immediateLock.Done;
		cmdFinished.Done;
		cmds.Done;
		_tlock.Done;
	end;

	function Window.RecommendMT: boolean;
	begin
		result :=
			{$ifdef Windows} WindowsSpecific.CheckVersion(Vista)
			{$else} {$error Window.RecommendMT unimplemented}
			{$endif};
	end;

	procedure Window._Lock;
	begin
		if _thread.OK then _tlock.Enter;
	end;

	procedure Window._Unlock;
	begin
		if _thread.OK then _tlock.Leave;
	end;

{$ifdef Debug}
	procedure Window._DprHID;
	const
		MaxNameLen = 256;
	var
		hid: array of RawInputDeviceList;
		name: array[0 .. MaxNameLen - 1] of widechar;
		info: RidDeviceInfo;
		r, hidCount: Windows.UINT;
		i: sint;
		caps: HidpCaps;
		preparsed: pointer;
		preparsedSize: Windows.UINT;
		postmsg: string;
	begin
		hid := nil;
		hidCount := 0;
		r := GetRawInputDeviceList(nil, hidCount, sizeof(hid[0]));

		if r = High(Windows.UINT) then
		begin
			Log('Ошибка GetRawInputDeviceList, код ' + ToString(GetLastError), logError);
			exit;
		end;

		if hidCount = 0 then
		begin
			Log('Raw Input Devices не замечено...', logWarning);
			exit;
		end;

		SetLength(hid, hidCount);
		r := GetRawInputDeviceList(pRawInputDeviceList(hid), hidCount, sizeof(hid[0]));
		if r = High(Windows.UINT) then
		begin
			Log('Ошибка GetRawInputDeviceList #2, код ' + ToString(GetLastError), logError);
			exit;
		end;

		for i := 0 to hidCount - 1 do
		begin
			r := MaxNameLen;

			if GetRawInputDeviceInfoW(hid[i].hDevice, RIDI_DEVICENAME, @name[0], r) = High(Windows.UINT) then
			begin
				Log('Ошибка GetRawInputDeviceInfo(RIDI_DEVICENAME), код ' + ToString(GetLastError), logError);
				continue;
			end;
			LogR('Human Interface Device #' + ToString(i) + ': "' + UTF8Encode(Copy(name, 0, r)) + '", handle = ' + ToString(hid[i].hDevice));

			r := sizeof(info);
			info.cbSize := r;
			if GetRawInputDeviceInfoW(hid[i].hDevice, RIDI_DEVICEINFO, @info, r) = High(Windows.UINT) then
			begin
				Log('Ошибка GetRawInputDeviceInfo(RIDI_DEVICEINFO), код ' + ToString(GetLastError), logError);
				continue;
			end;

			postmsg := '';
			preparsedSize := 0;
			if (GetRawInputDeviceInfoW(hid[i].hDevice, RIDI_PREPARSEDDATA, nil, preparsedSize) <> High(Windows.UINT)) and (preparsedSize <> 0) then
			begin
				preparsed := GetMem(preparsedSize);
				if GetRawInputDeviceInfoW(hid[i].hDevice, RIDI_PREPARSEDDATA, preparsed, preparsedSize) <> High(Windows.UINT) then
					if HidP_GetCaps(preparsed, caps) = HIDP_STATUS_SUCCESS then
						postmsg := Format('; размеры репортов (Input / Output / Feature): {0} / {1} / {2}',
							[caps.InputReportByteLength, caps.OutputReportByteLength, caps.FeatureReportByteLength]);
				FreeMem(preparsed);
			end;

			case info.dwType of
				RIM_TYPEMOUSE:
					Log(', мышка, ID: {0}, кнопок: {1}, частота: {2}' + postmsg,
						[info.mouse.dwId, info.mouse.dwNumberOfButtons, info.mouse.dwSampleRate]);
				RIM_TYPEKEYBOARD:
					Log(', клавиатура, тип: {0}, подтип: {1}, режим: {2}, функциональных клавиш: {3}, индикаторов: {4}, клавиш всего: {5}' + postmsg,
						[info.keyboard.dwType, info.keyboard.dwSubType, info.keyboard.dwKeyboardMode, info.keyboard.dwNumberOfFunctionKeys,
						info.keyboard.dwNumberOfIndicators, info.keyboard.dwNumberOfKeysTotal]);
				RIM_TYPEHID:
					Log(', Generic HID, вендор: {0}, продукт: {1}, версия: {2}, Usage Page: {3}, Usage: {4}' + postmsg,
						[info.hid.dwVendorId, info.hid.dwProductId, info.hid.dwVersionNumber,
						HidUsagePageDesc(info.hid.usUsagePage), HidUsageDesc(info.hid.usUsagePage, info.hid.usUsage)]);
				else
					Log(', явообщехзчтоэто, даже не HID' + postmsg, logWarning);
			end;
		end;
	end;
{$endif}

	procedure WindowThread(param: pointer);
	var
		win: pWindow absolute param;
		finish: boolean;
	begin
		finish := no;
		try
			repeat
				if not win^._ProcessSignals then finish := yes;
				while win^.ExecuteNext(yes) do;
			until finish;
		except
			on TerminateThread do {nothing};
		end;
	{$ifdef Debug} Log('Поток окна завершён'); {$endif}
	end;

	function Window.Open: boolean;
	var
		enableMt: boolean;
	begin
		enableMt := MMSystem.Config.allowMT.Decide(RecommendMT);
	{$ifdef Debug}
		Log('Поток окна в{0}ключен ({1})', IfThen(enableMT, '', 'ы'), IfThen(MMSystem.Config.allowMT.Undefined, 'автоматически', 'вручную'),
			LogMessageStyle(IfThen(RecommendMT and enableMt, ord(logPlain), ord(logWarning))));
	{$endif}
		if enableMt then
		begin
			Assert(not _thread.OK);
			Thread.Start('окна', _thread, @WindowThread, @self);
		{$ifdef Debug} Log('Окно: туредо стаато! ' + _thread.Human(BracketedInfo), logOK); {$endif}
		end;
		UnlockPutImmediate(LockPut(Comd.Op.Open, sizeof(Comd.Open)));
		_Lock; result := handle <> 0; _Unlock;
	end;

	function Window._Open: boolean;
	label _finally_;
	var
		windowClass: WndClassW;
		gamepad: RawInputDevice;
	begin
		result := no;

		if ExtractIconExW(pWideChar(UTF8Decode(ExecFileName)), 0, @largeIcon, @smallIcon, 1) = 2 then
		begin
		{$ifdef Debug} Log('Загружены иконки :3', logOK); {$endif}
		end else
		begin
		{$ifdef Debug} Log('Не удалось загрузить иконки', logWarning); {$endif}
		end;

		Zero(@windowClass, sizeof(windowClass));
		windowClass.lpszClassName := pWideChar(classNameW);
		windowClass.lpfnWndProc := @WindowProc;
		windowClass.hInstance := HInstance;
		windowClass.hIcon := LoadIcon(0, IDI_APPLICATION);
		windowClass.hCursor := LoadCursor(0, IDC_ARROW);
		classHandle := RegisterClassW(windowClass);
		if classHandle <> 0 then
		begin
		{$IFDEF Debug} Log('Класс окна зарегистрирован, имя: "' + UTF8Encode(classNameW) + '"', logOK); {$ENDIF}
		end else
		begin
		{$ifdef Debug} Log('Не удалось зарегистрировать класс окна', logError); {$endif}
			goto _finally_;
		end;

		if not _CreateWindow then goto _finally_;
		if largeIcon <> 0 then SetClassLongW(handle, GCL_HICON, largeIcon);
		if smallIcon <> 0 then SetClassLongW(handle, GCL_HICONSM, smallIcon);

		if _CreateGLContext then
		begin
		{$ifdef Debug} Log('Это окончательный вариант контекста OpenGL', logOK); {$endif}
			_ForceVSync(window_VSync in _flags);
			_UnbindGLContext;
		end else
		begin
			Error.Show('Не удалось создать контекст OpenGL.');
			goto _finally_;
		end;
		Include(_flags, window_DestroyingMeansQuit);

		_rect := _QueryRect(yes);
		if window_Fullscreen in _flags then
		begin
			// ShowWindow(handle, SW_SHOW);
			_RawSetFlag(window_Fullscreen, no);
			_SetFullscreen(yes, yes);
		end else
			AnimateWindow(handle, 200, AW_ACTIVATE or AW_BLEND);

	{$ifdef Debug} LogR('Регистрация (потенциального) геймпада... '); {$endif}
		// google -> HID usage tables
		gamepad.usUsagePage := HID_USAGE_PAGE_GENERIC;
		gamepad.usUsage     := HID_USAGE_GENERIC_JOYSTICK;
		gamepad.dwFlags     := 0;
		if WindowsSpecific.CheckVersion(Vista) then gamepad.dwFlags := gamepad.dwFlags or RIDEV_DEVNOTIFY {$ifdef Debug} else Log('RIDEV_DEVNOTIFY не поддерживается; ', logDebug) {$endif};
		gamepad.hwndTarget  := handle;
		if RegisterRawInputDevices(@gamepad, 1, sizeof(gamepad)) then
		begin
		{$ifdef Debug} Log('Геймпад зарегистрирован', logOK); {$endif}
		end else
		begin
		{$ifdef Debug} Log('Ошибка регистрации геймпада, код ' + ToString(GetLastError), logError); {$endif}
		end;

		result := yes;
	_finally_:
		if not result then _Close;
	end;

	procedure Window._Close;
		procedure DestroyIcon(var ref: Windows.HICON);
		begin
			if ref <> 0 then
			begin
				Windows.DestroyIcon(ref);
				ref := 0;
			end;
		end;
	begin
		_DestroyWindow(no);

		if classHandle <> 0 then
		begin
			if UnregisterClassW(pWideChar(classNameW), 0) then
			begin
			{$IFDEF Debug} Log('Класс окна уничтожен', logOK); {$ENDIF}
			end else
			begin
			{$ifdef Debug} Log('Класс окна не уничтожился', logWarning); {$endif}
			end;
			classHandle := 0;
		end;
		DestroyIcon(largeIcon);
		DestroyIcon(smallIcon);
	end;

	function Window._ProcessSignals: boolean;
	const
		MClickRelease: array[boolean] of Callback.Op = (Callback.Op.MRelease, Callback.Op.MClick);
		MLeftRight:    array[boolean] of MouseButton = (mouse_Right, mouse_Left);
		KClickRelease: array[boolean] of Callback.Op = (Callback.Op.KRelease, Callback.Op.KClick);

		function _FilterInput(key: KeyboardKey; down: boolean): boolean;
		begin
			result := key in CharKeys;
			if not result then exit;

			_Lock;
			result := _inputEnabled > 0;
			if result and (key in _inputExceptions) then
			begin
				result := no;
				if down then _filterNextChar := yes;
			end;
			_Unlock;
		end;

		procedure PutKeyboard(op: Callback.Op; key: KeyboardKey);
		begin
			Callback.pKKey(LockPutCallback(op, sizeof(Callback.KKey)))^.key := key;
			UnlockPutCallback;
		end;

		procedure PutMouse(op: Callback.Op; btn: MouseButton);
		begin
			Callback.pMButton(LockPutCallback(op, sizeof(Callback.MButton)))^.btn := btn;
			UnlockPutCallback;
		end;

	var
		message: Windows.MSG;
		key: KeyboardKey;
		down: boolean;
	begin
		result := yes;
		while PeekMessage((@message)^, 0, 0, 0, PM_Remove) do
		begin
			case message.message of
				WM_QUIT :
				begin
					_flags += [window_GotWMQuit];
				{$IFDEF Debug} Log('Приложение получило WM_QUIT'); {$ENDIF}
					result := no;
				end;
				WM_SysKeyDown, WM_KeyDown, WM_SysKeyUp, WM_KeyUp:
					begin
						down := (message.message = WM_SysKeyDown) or (message.message = WM_KeyDown);
						if _DecryptKeyboardKey(message, message.wparam, key) and not _FilterInput(key, down) then
							PutKeyboard(KClickRelease[down], key);
					end;
				WM_MouseMove:
					begin
						Callback.pMMove(LockPutCallback(Callback.Op.MMove, sizeof(Callback.MMove)))^.Setup(
							IntVec2.Make(GET_X_LPARAM(message.lParam), GET_Y_LPARAM(message.lParam)), no);
						UnlockPutCallback;
					end;
				WM_LButtonDown, WM_RButtonDown, WM_LButtonUp, WM_RButtonUp:
					PutMouse(MClickRelease[(message.message = WM_LButtonDown) or (message.message = WM_RButtonDown)],
					         MLeftRight[(message.message = WM_LButtonDown) or (message.message = WM_LButtonUp)]);
				WM_MouseWheel:
					begin
						Callback.pMScroll(LockPutCallback(Callback.Op.MScroll, sizeof(Callback.MScroll)))^.delta :=
							tWMMouseWheel(pointer(@message.message)^).WheelDelta / WHEEL_DELTA;
						UnlockPutCallback;
					end;
			end;
			TranslateMessage(message);
			DispatchMessage(message);
		end;
	end;

	function Window.LockPutCallback(op: Callback.Op; size: size_t): Callback.pBase;
	begin
		result     := _callbacks.LockPut(size);
		result^.op := op;
	end;

	procedure Window.UnlockPutCallback;
	begin
		_callbacks.UnlockPut;
	end;

	function Window.LockPut(op: Comd.Op; size: size_t): Comd.pBase;
	begin
		result     := cmds.LockPut(size);
		result^.op := op;
	end;

	procedure Window.UnlockPut;
	begin
		cmds.UnlockPut;
		if not _thread.OK then ExecuteNext(no);
	end;

	function CommandFinished(param: pointer): boolean;
	begin
		result := Comd.pBase(param)^.op = Comd.Op._CommandCompleted;
	end;

	procedure Window.UnlockPutImmediate(cmd: Comd.pBase);
	begin
		cmds.UnlockPut;
		if _thread.OK then
		begin
			immediateLock.Enter;
			cmdFinished.Wait(immediateLock, @CommandFinished, cmd);
			immediateLock.Leave;
		end else
			ExecuteNext(no);
	end;

	function Window.ExecuteNext(calledFromThread: boolean): boolean;
		procedure NoteCompleted(cmd: Comd.pBase);
		begin
			immediateLock.Enter;
			cmd^.op := Comd.Op._CommandCompleted;
			immediateLock.Leave;
			cmdFinished.WakeOne;
		end;
	var
		cmd: Comd.pBase;
		fs: Comd.pSetFullscreen absolute cmd;
		size: size_t;
		immUnlock: boolean;

		procedure CommandSize(sz: size_t);
		begin
			if immUnlock then cmds.UnlockGet(sz) else size := sz;
		end;

	begin
		immUnlock := not _thread.OK;
		cmd := cmds.LockGet({wait?} calledFromThread);
		result := Assigned(cmd);
		if not result then exit; // при calledFromThread означает пришедшее сообщение!

		case cmd^.op of
			Comd.Op.Open:
				begin
					CommandSize(sizeof(Comd.Open));
					_Open;
					NoteCompleted(cmd);
				end;
			Comd.Op.TiCh:
				begin
					CommandSize(sizeof(Comd.TiCh));
					_Lock;
					SetWindowTextW(handle, pWideChar(UTF8Decode(Comd.pTiCh(cmd)^.title)));
					System.Finalize(Comd.pTiCh(cmd)^);
					_Unlock;
				end;
			Comd.Op.SetFullscreen:
				begin
					CommandSize(sizeof(Comd.SetFullscreen));
					_SetFullscreen(fs^.enabled, no);
					NoteCompleted(cmd);
				end;
			Comd.Op.Terminate:
				begin
					CommandSize(sizeof(Comd.Terminate));
					if calledFromThread then begin if not immUnlock then cmds.UnlockGet(size); Thread.TerminateSelf; end;
				end;
			else raise ExhaustiveCase(ord(cmd^.op), 'Window.Op');
		end;
		if not immUnlock then cmds.UnlockGet(size);
	end;

	function Window._BindGLContext: boolean;
	begin
		if _hrc = 0 then exit(no);
	{$ifdef Debug} Assert(_hrcThread = 0); {$endif}
	{$ifdef Debug} LogR('Попытка передать OpenGL-контекст потоку ' + ToString(Thread.Current) + '... '); {$endif}
		result := wglMakeCurrent(_dc, _hrc);
		if result then
		begin
			{$ifdef Debug} Log('Получилось!', logOk); {$endif}
			_hrcThread := {$ifdef Debug} Thread.Current {$else} yes {$endif};
		end {$ifdef Debug} else Log('Fail', logError) {$endif};
	end;

	function Window._UnbindGLContext: boolean;
	begin
		if {$ifdef Debug} _hrcThread = 0 {$else} not _hrcThread {$endif} then exit(no);
	{$ifdef Debug} Assert(_hrcThread = Thread.Current); {$endif}
		result := wglMakeCurrent(0, 0);
		_hrcThread := {$ifdef Debug} 0 {$else} no {$endif};
	{$ifdef Debug}
		if result then
			LogR('Контекст OpenGL (' + ToString(_hrc) + ') отвязан от потока ' + ToString(Thread.Current) + '; ', logOK)
		else
			LogR('Не удалось отвязать контекст OpenGL (' + ToString(_hrc) + ') от потока ' + ToString(Thread.Current) + '; ', logWarning);
	{$endif}
	end;

	function Window.BindGLContext: boolean;
	begin
		_Lock;
		result := _BindGLContext;
		_Unlock;
	end;

	function Window.UnbindGLContext: boolean;
	begin
		_Lock;
		result := _UnbindGLContext;
		_Unlock;
	end;

	function Window._PeekVideoMode(const winSize: IntVec2; out mode: DEVMODEW): boolean;
	const
		Bpp = 32;
		MaxAspectError = 0.2;
	var
		i: sint;
		cur: DEVMODEW;
		scrSize, targetSize, curSize, bestSize: IntVec2;
		curBpp, bestBpp: sint;
		aspect: float;
		first, featured: boolean;
	{$ifdef Debug}
		devname, msg: string;
		driverversion: sint;
	{$endif}
	begin
		result := no;
		i := 0;
		fillchar((@cur)^, sizeof(cur), 0);
		cur.dmSize := sizeof(cur);
		scrSize := ScreenSize;
		aspect := scrSize.Aspect;
		targetSize := (winSize + scrSize) div 2;

	{$ifdef Debug} devname := ''; driverversion := 0; {$endif}
		bestSize := IntVec2.Zero; bestBpp := 0;
		first := yes;

		while EnumDisplaySettingsW(nil, i, cur) do
		begin
			curSize := IntVec2.Make(cur.dmPelsWidth, cur.dmPelsHeight);
			curBpp := cur.dmBitsPerPel;
			featured := first or
				((abs(curSize.x - targetSize.x) <= abs(bestSize.x - targetSize.x)) and (abs(curSize.y - targetSize.y) <= abs(bestSize.y - targetSize.y)) and
				(abs(curBpp - Bpp) <= abs(bestBpp - Bpp)));
			featured := featured and (abs(curSize.aspect - aspect) <= MaxAspectError);
			if featured then
			begin
				bestSize := curSize;
				bestBpp := curBpp;
				result := yes;
				mode := cur;
				mode.dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
				first := no;
			end;
		{$ifdef Debug}
			if (i = 0) or (UTF8Encode(widestring(cur.dmDeviceName)) <> devname) or (cur.dmDriverVersion <> driverversion) then
			begin
				devname := UTF8Encode(widestring(cur.dmDeviceName));
				msg := 'устройство "' + devname + '", версия драйвера ' + ToString(cur.dmDriverVersion);
				driverversion := cur.dmDriverVersion;
			end else
				msg := '-''''-';
			msg += ', ' + ToString(cur.dmPelsWidth) + ' x ' + ToString(cur.dmPelsHeight) + ' @ ' + ToString(cur.dmBitsPerPel) + ' bpp, ' +
				ToString(cur.dmDisplayFrequency) + ' Гц';
			LogR('Видеорежим #' + ToString(i) + ': ' + msg, logDebug);
			if featured then Log(' <- ничо так', logOK) else Log('');
		{$endif}
			inc(i);
		end;
	end;

	function Window._CreateWindow: boolean;
	var
		screenRect: WindowRect;
		ptr: pointer;
	begin
		result := no;
		_style := WS_SYSMENU or WS_CAPTION or WS_SIZEBOX or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
		screenRect := ClientRectToScreenRect(_rect);
		handle := CreateWindowW(pWideChar(classNameW), pWideChar(UTF8Decode(caption.Join)), _style,
		                        screenRect.pos.x, screenRect.pos.y, screenRect.size.x, screenRect.size.y, 0, 0, 0, nil);
		if handle = 0 then
		begin
		{$ifdef Debug} Log('Ошибка при создании окна', logError); {$endif}
			exit;
		end;

		ptr := @self;
		SetWindowLongPtrW(handle, GWL_USERDATA, pPtrUint(@ptr)^);

		if window_Centered in _flags then _Centerify(no);
	{$IFDEF Debug} Log('Cоздано окно ' + ToString(_QueryRect(yes).size) + ' @ ' + ToString(_QueryRect(no).pos), logOK); {$ENDIF}
		result := yes;
	end;

	function Window._DestroyWindow(nested: boolean): boolean;
	begin
		if not nested then
		begin
			if _hrc <> 0 then
			begin
				_UnbindGLContext;
				if wglDeleteContext(_hrc) then
				begin
				{$ifdef Debug} LogR('Контекст OpenGL (' + ToString(_hrc) + ') уничтожен; ', logOK); {$endif}
				end else
				begin
				{$ifdef Debug} LogR('Контекст OpenGL не уничтожился ({0}, поток: {1}); ', [_hrc, Thread.Current], logWarning); {$endif}
				end;
				_hrc := 0;
			end;
			if handle <> 0 then
				if DestroyWindow(handle) then
				begin
					{$ifdef Debug} Log('Окно уничтожено', logOK); {$endif}
					Assert(handle = 0);
				end {$ifdef Debug} else Log('Окно не уничтожилось', logWarning) {$endif};
		end;
		if _dc <> 0 then
		begin
			if ReleaseDC(handle, _DC) = 0 then begin {$ifdef Debug} LogR('Контекст устройства не освободился; ', logWarning); {$endif} end;
			_dc := 0;
		end;
		handle := 0;
		result := yes;
	end;

	function Window.IsActive: boolean;
	begin
		_Lock;
		result := (window_Active in _flags) and _cursorHidden;
		_Unlock;
	end;

	procedure Window.CenterMouse;
	var
		pos: IntVec2;
		pt: TPOINT;
	begin
		_Lock;
		pos := _rect.size div 2;
		pt.x := pos.x;
		pt.y := pos.y;
		ClientToScreen(handle, pt);
		_Unlock;

		Callback.pMMove(LockPutCallback(Callback.Op.MMove, sizeof(Callback.MMove)))^.Setup(pos, yes);
		UnlockPutCallback;
		Windows.SetCursorPos(pt.x, pt.y);
	end;

	function Window.Process(keyboard: pKeyboardInput; mouse: pMouseInput; gamepad: pGamepadInput): boolean;
	var
		cb: Callback.pBase;
		mmove: Callback.pMMove absolute cb;
		mscroll: Callback.pMScroll absolute cb;
		mb: Callback.pMButton absolute cb;
		kk: Callback.pKKey absolute cb;
		gb: Callback.pGButton absolute cb;
		gs: Callback.pGStick absolute cb;
		size: size_t;
	begin
		_Lock; result := handle <> 0; _Unlock;
		if not result then exit;

		if Assigned(mouse) then
		begin
			_Lock;
			mouse^.maxPos := _rect.size;
			_Unlock;
		end;

		if not _thread.OK then
			result := _ProcessSignals;

		repeat
			cb := _callbacks.LockGet(no);
			if not Assigned(cb) then break;

			case cb^.op of
				Callback.Op.MMove:
					begin
						if Assigned(mouse) then mouse^.Move(mmove^.pos, mmove^.silent);
						size := sizeof(mmove^);
					end;
				Callback.Op.MScroll:
					begin
						if Assigned(mouse) then mouse^.Scroll(mscroll^.delta);
						size := sizeof(mscroll^);
					end;
				Callback.Op.MClick, Callback.Op.MRelease:
					begin
						if Assigned(mouse) then
							if cb^.op = Callback.Op.MClick then mouse^.ClickButton(mb^.btn) else mouse^.ReleaseButton(mb^.btn);
						size := sizeof(mb^);
					end;
				Callback.Op.KClick, Callback.Op.KRelease:
					begin
						if Assigned(keyboard) then
							if cb^.op = Callback.Op.KClick then keyboard^.PressKey(kk^.key) else keyboard^.ReleaseKey(kk^.key);
						size := sizeof(kk^);
					end;
				Callback.Op.GButton:
					begin
						if Assigned(gamepad) then gamepad^.SetButtonState(gb^.bid, gb^.state);
						size := sizeof(gb^);
					end;
				Callback.Op.GStick:
					begin
						if Assigned(gamepad) then gamepad^.SetStickState(gs^.stick, gs^.state);
						size := sizeof(gs^);
					end;
				else raise ExhaustiveCase(ord(cb^.op), 'Window.Callback.Op');
			end;
			_callbacks.UnlockGet(size);
		until no;

		result := result and not (window_QuitRequested in _flags);
	end;

	function Window._DecryptKeyboardKey(const message: tMsg; code: sint; out key: KeyboardKey): boolean;
	begin
		result := WindowsSpecific.DecryptKey(code, @message, key);
		if result then
			case key of
				key_Enter:
					if HIWORD(message.lParam) and KF_ALTDOWN = KF_ALTDOWN then
						key := key_AltEnter;
			end;
	end;

	procedure Window.SendQuitSignal;
	begin
		_Lock;
		_flags += [window_QuitRequested];
		PostMessage(handle, WM_CLOSE, 0, 0);
		_Unlock;
	end;

	function Window._SetDCPixelFormat(hdc: HDC; format: pSint32): boolean;
	const
		pfd: tPixelFormatDescriptor =
		(
			nSize: sizeof(tPixelFormatDescriptor);
			nVersion: 1;
			dwFlags: PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
			iPixelType: PFD_TYPE_RGBA;
			cColorBits: 24;
			cRedBits: 0; cRedShift: 0; cGreenBits: 0; cGreenShift: 0; cBlueBits: 0; cBlueShift: 0;
			cAlphaBits: 0; cAlphaShift: 0;
			cAccumBits: 0; cAccumRedBits: 0; cAccumGreenBits: 0; cAccumBlueBits: 0; cAccumAlphaBits: 0;
			cDepthBits: 24;
			cStencilBits: 0;
			cAuxBuffers: 0;
			iLayerType: PFD_MAIN_PLANE;
			bReserved: 0;
			dwLayerMask: 0; dwVisibleMask: 0; dwDamageMask: 0;
		);
	var
		pixelFormat: sint32;
	begin
		if Assigned(format) then
			pixelFormat := format^
		else
			pixelFormat := ChoosePixelFormat(hdc, @pfd);
		if pixelFormat = 0 then
		begin
		{$ifdef Debug} Log('ChoosePixelFormat — FAIL', logError); {$endif}
			exit(no);
		end;
	{$ifdef Debug} LogR('SetPixelFormat... ', logDebug); {$endif}
		result := SetPixelFormat(hdc, pixelFormat, @pfd);
	{$ifdef Debug} if result then LogR('OK; ', logOK) else LogR('fail; ', logError); {$endif}
	end;

	function Window._CreateGLContext: boolean;
	var
		ctx: HGLRC;
	begin
		result := no;
		if not (_CreateGLContext1 and _BindGLContext) then exit;
		if not Assigned(wglGetProcAddress) then
		begin
			if not oglLib.OK then
				try
					DynamicLibrary.Open(oglLib, 'opengl32');
				except
				{$ifdef Debug} Log('Ошибка при загрузке OpenGL32.dll', logError); {$endif}
					exit;
				end;

			pPointer(@wglGetProcAddress)^ := oglLib.FindProc('wglGetProcAddress');
			if not Assigned(wglGetProcAddress) then
			begin
			{$ifdef Debug} Log('wglGetProcAddress не найдена', logError); {$endif}
				exit;
			end;
		end;

		pPointer(@_wglGetSwapInterval)^ := wglGetProcAddress('wglGetSwapIntervalEXT');
		pPointer(@_wglSwapInterval)^ := wglGetProcAddress('wglSwapIntervalEXT');
		pPointer(@_wglChoosePixelFormatARB)^ := wglGetProcAddress('wglChoosePixelFormatARB');
		pPointer(@_wglCreateContextAttribsARB)^ := wglGetProcAddress('wglCreateContextAttribsARB');
	{$ifdef Debug}
		if Assigned(_wglChoosePixelFormatARB) then LogR('wglChoosePixelFormatARB — OK; ', logOK)
		                                      else LogR('wglChoosePixelFormatARB нету; ' , logWarning);
		if Assigned(_wglCreateContextAttribsARB) then LogR('wglCreateContextAttribsARB — OK; ', logOK)
		                                         else LogR('wglCreateContextAttribsARB нету; ', logWarning);
	{$endif}

		if msaaSamples > 1 then
			result := _CreateGLContext2
		else
		begin
			ctx := _CreateHGLRC;
			if ctx <> 0 then
			begin
				_UnbindGLContext;
				wglDeleteContext(_hrc);
				_hrc := ctx;
				result := _BindGLContext;
			end;
		end;
	end;

	function Window._CreateGLContext1: boolean;
	begin
		Assert((_dc = 0) and (_hrc = 0));
		_DC := GetDC(handle);
		if not _SetDCPixelFormat(_dc, nil) then
		begin
			ReleaseDC(handle, _DC);
			exit(no);
		end;
		_hrc := _CreateHGLRC;
		result := _hrc <> 0;
	end;

	function Window._CreateGLContext2: boolean;
	const
		// ARB_pixel_format
		WGL_DRAW_TO_WINDOW_ARB = $2001;
		WGL_SUPPORT_OPENGL_ARB = $2010;
		WGL_DOUBLE_BUFFER_ARB = $2011;
		WGL_PIXEL_TYPE_ARB = $2013;
		WGL_TYPE_RGBA_ARB = $202B;
		WGL_COLOR_BITS_ARB = $2014;
		WGL_DEPTH_BITS_ARB = $2022;
		// WGL_STENCIL_BITS_ARB = $2023;

		// ARB_multisample
		WGL_SAMPLE_BUFFERS_ARB = $2041;
		WGL_SAMPLES_ARB = $2042;

	var
		attribs: packed array[0 .. 16] of sint32;
		format: sint32;
		nFormats: Windows.UINT;
	begin
		result := yes;
		if Assigned(_wglChoosePixelFormatARB) and Assigned(_wglCreateContextAttribsARB) then
		begin
			attribs[0] := WGL_DRAW_TO_WINDOW_ARB; attribs[1] := 1;
			attribs[2] := WGL_SUPPORT_OPENGL_ARB; attribs[3] := 1;
			attribs[4] := WGL_DOUBLE_BUFFER_ARB; attribs[5] := 1;
			attribs[6] := WGL_PIXEL_TYPE_ARB; attribs[7] := WGL_TYPE_RGBA_ARB;
			attribs[8] := WGL_COLOR_BITS_ARB; attribs[9] := 24;
			attribs[10] := WGL_DEPTH_BITS_ARB; attribs[11] := 24;
			attribs[12] := WGL_SAMPLE_BUFFERS_ARB; attribs[13] := 1;
			attribs[14] := WGL_SAMPLES_ARB; attribs[15] := msaaSamples;
			attribs[16] := 0;
		{$ifdef Debug} Log('Запрошен контекст с MSAA (' + ToString(msaaSamples) + 'x). Окно будет пересоздано.'); {$endif}
			if _wglChoosePixelFormatARB(_dc, @attribs[0], nil, 1, @format, @nFormats) then
			begin
				result := no;
				_DestroyWindow(no);
				if _CreateWindow then
				begin
					Assert((_dc = 0) and (_hrc = 0));
					_dc := GetDC(handle);
					result := _SetDCPixelFormat(_dc, @format);
					if result then
					begin
						_hrc := _CreateHGLRC;
						result := _hrc <> 0;
					end else
						_hrc := 0;
					if result then
					begin
					{$ifdef Debug} Log('Охренеть, получилось!', logOK); {$endif}
					end else
					begin
					{$ifdef Debug} Log('Fail. Попытаюсь пересоздать старый контекст. Если всё рухнет, выключи MSAA.', logWarning); {$endif}
						_DestroyWindow(no);
						result := _CreateWindow and _CreateGLContext1;
					end;
					if result then _BindGLContext;
				end;
			end
			{$ifdef Debug} else Log('Fail. Даже формат не выбрался. Если всё рухнет, выключи MSAA.', logError) {$endif};
		end;
	end;

	function Window._CreateHGLRC: HGLRC;
	const
		// forward-compatible context
		WGL_CONTEXT_MAJOR_VERSION_ARB = $2091;
		WGL_CONTEXT_MINOR_VERSION_ARB = $2092;

		WGL_CONTEXT_FLAGS_ARB         = $2094;
		{$ifdef Debug} WGL_CONTEXT_DEBUG_BIT_ARB = $1; {$endif}
			WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB = $2;

		WGL_CONTEXT_PROFILE_MASK_ARB  = $9126;
			WGL_CONTEXT_CORE_PROFILE_BIT_ARB = $1;
			WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB = $2;
	var
		attribs: packed array[0 .. 8] of sint32;
		n: sint;
		cflags: sint32;
	{$ifdef Debug} msg: string; {$endif}

		procedure put(a, b: sint);
		begin
			attribs[n] := a;
			attribs[n + 1] := b;
			attribs[n + 2] := 0;
			inc(n, 2);
		end;

	begin
		result := 0;
		if Assigned(_wglCreateContextAttribsARB) then
		begin
		{$ifdef Debug}
			msg := 'Создаю параметризованный контекст OpenGL';
			if window_ForwardGL in _flags then
				msg += ' ' + ToString(glMajor) + '.' + ToString(glMinor) + ', forward-compatible';
			if window_DebugGL in _flags then
				msg += ', debug';
			LogR(msg + '... ');
		{$endif}

			n := 0; attribs[n] := 0;
			cflags := 0;
			put(WGL_CONTEXT_MAJOR_VERSION_ARB, glMajor);
			put(WGL_CONTEXT_MINOR_VERSION_ARB, glMinor);
		{$ifdef Debug} if window_DebugGL in _flags then cflags := cflags or WGL_CONTEXT_DEBUG_BIT_ARB; {$endif}
			if (window_ForwardGL in _flags) and (glMajor >= 3) then
			begin
				cflags := cflags or WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB;
				put(WGL_CONTEXT_PROFILE_MASK_ARB, WGL_CONTEXT_CORE_PROFILE_BIT_ARB);
			end else
				put(WGL_CONTEXT_PROFILE_MASK_ARB, WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB);
			if cflags <> 0 then put(WGL_CONTEXT_FLAGS_ARB, cflags);
			result := _wglCreateContextAttribsARB(_dc, 0, @attribs[0]);
		end;
		if result = 0 then
		begin
		{$ifdef Debug} LogR('Создаю базовый контекст OpenGL... '); {$endif}
			result := wglCreateContext(_DC);
		end;
	{$ifdef Debug} if result <> 0 then LogR('OK (HRC: ' + ToString(result) + '); ', logOK) else LogR('Fail; ', logError); {$endif}
	end;

	function Window.ClientRectToScreenRect(const r: WindowRect): WindowRect;
	var
		wr: Windows.RECT;
	begin
		wr.left := r.pos.x;
		wr.top  := r.pos.y;
		wr.right := r.pos.x + sint(r.size.x);
		wr.bottom := r.pos.y + sint(r.size.y);
		if AdjustWindowRect(wr, _style, no) then
			result := WindowRect.Make(IntVec2.Make(wr.left, wr.top), UintVec2.Make(wr.right - wr.left, wr.bottom - wr.top))
		else
		begin
		{$ifdef Debug} Log('AdjustWindowRect: fail', logWarning); {$endif}
			result := r;
		end;
	end;

	function Window._QueryRect(client: boolean): WindowRect;
	var
		r: Windows.RECT;
	begin
		Assert(handle <> 0);
		if client then
			GetClientRect(handle, @r)
		else
			Windows.GetWindowRect(handle, @r);
		result := WindowRect.Make(IntVec2.Make(r.left, r.top), UintVec2.Make(r.right - r.left, r.bottom - r.top));
	end;

	procedure Window.SwapBuffers;
	begin
		Windows.SwapBuffers(_DC);
	end;

	procedure Window._SetFullscreen(value, internal: boolean);
	label _finally_;
	var
		mode: DEVMODEW;
		ok: boolean;
		ns: PtrUint;
		orig: WindowRect;
	begin
		_Lock;
		if (value = (window_Fullscreen in _flags)) or ((not internal) and (window_InactivatedInFullscreen in _flags)) then goto _finally_;
		if value then
		begin
			_origScreenRect := _QueryRect(no);
			ok := _PeekVideoMode(_rect.size, mode);
			if ok then
			begin
				_Unlock;
				ok := ChangeDisplaySettingsW(mode, CDS_FULLSCREEN) = DISP_CHANGE_SUCCESSFUL;
				_Lock;
				if ok then
				begin
					ns := WS_VISIBLE or WS_POPUP or WS_MAXIMIZE or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
					_styleBeforeFullscreen := SetWindowLongPtrW(handle, GWL_STYLE, LONG_PTR(pointer(@ns)^)) or WS_VISIBLE;
					if _styleBeforeFullscreen <> 0 then
					begin
						_style := ns;
						_Unlock;
						SetWindowPos(handle, HWND_TOP, 0, 0, mode.dmPelsWidth, mode.dmPelsHeight, SWP_FRAMECHANGED or SWP_NOZORDER);
						_Lock;
						ok := yes;
					end else
					begin
						_Unlock;
						ChangeDisplaySettingsW(PDEVMODEW(nil)^, 0);
						_Lock;
					end;
				end;
			end;
			if ok then
			begin
				Include(_flags, window_Fullscreen);
			{$ifdef Debug} Log('Окно переведено в полноэкранный режим', logOK); {$endif}
			end {$ifdef Debug} else Log('Не удаётся перевести окно в полноэкранный режим', logWarning) {$endif};
		end else
		begin
			_Unlock;
			ok := ChangeDisplaySettingsW(PDEVMODEW(nil)^, 0) = DISP_CHANGE_SUCCESSFUL;
			_Lock;
			if ok then
			begin
				_style := _styleBeforeFullscreen;
				SetWindowLongPtrW(handle, GWL_STYLE, LONG_PTR(pointer(@_style)^));
				orig := _origScreenRect;
				_Unlock;
				SetWindowPos(handle, HWND_TOP, orig.pos.x, orig.pos.y, orig.size.x, orig.size.y, SWP_FRAMECHANGED or SWP_NOZORDER);
				_Lock;
			end;
			if ok then
			begin
				Exclude(_flags, window_Fullscreen);
			{$ifdef Debug} Log('Окно восстановлено из полноэкранного режима', logOK); {$endif}
			end {$ifdef Debug} else Log('Не удаётся восстановить окно из полноэкранного режима', logWarning) {$endif};
		end;
	_finally_:
		_Unlock;
	end;

	procedure Window.HandleActivation(activated: boolean);
	begin
		_Lock;
		_RawSetFlag(window_Active, activated);
		if (window_DestroyingMeansQuit in _flags)
			and ((not activated and (window_Fullscreen in _flags)) or (activated and (window_InactivatedInFullscreen in _flags))) then
		begin
			if not activated then
			begin
				Include(_flags, window_InactivatedInFullscreen);
			{$ifdef Debug} Log('Автоматический вывод деактивируемого окна из полноэкранного режима.'); {$endif}
			end {$ifdef Debug} else Log('Автоматическое переключение активируемого окна в полноэкранный режим.'); {$endif};
			_Unlock;
			_SetFullscreen(activated, yes);
			_Lock;
			if activated then Exclude(_flags, window_InactivatedInFullscreen);
		end;
		_Unlock;
	end;

	function Window._Centerify(lock: boolean): boolean;
	var
		full: WindowRect;
	begin
		if lock then _Lock;
		full := _QueryRect(yes);
		full.pos := (IntVec2(ScreenSize) - IntVec2(full.size)) div 2;
		full := ClientRectToScreenRect(full);
		result := SetWindowPos(handle, HWND_TOP, full.pos.x, full.pos.y, 0, 0, SWP_NOZORDER or SWP_NOSIZE or SWP_ASYNCWINDOWPOS);
		if lock then _Unlock;
	end;

	function Window.Centerify: boolean;
	begin
		result := _Centerify(yes);
	end;

	procedure Window.SetPos(const fp: Vec2);
	begin
		_Lock;
		Assert(handle = 0);
		_rect.pos := IntTrunc(fp);
		_Unlock;
	end;

	procedure Window.SetSizes(const fs: Vec2);
	begin
		_Lock;
		Assert(handle = 0);
		if fs.x > 1.0 then
			_rect.size.x := round(fs.x)
		else
			_rect.size.x := round(ScreenSize.x * fs.x);
		if fs.y > 1.0 then
			_rect.size.y := round(fs.y)
		else
			_rect.size.y := round(ScreenSize.y * fs.y);
		_Unlock;
	end;

	procedure Window.EnableInput(exceptions: KeyboardKeys);
	begin
		_Lock;
		if _inputEnabled = 0 then
		begin
			_nInput := 0;
			_inputExceptions := [];
			_filterNextChar := no;
		end;
		inc(_inputEnabled);
		_inputExceptions += exceptions;
		_Unlock;
	end;

	procedure Window.DisableInput;
	begin
		_Lock;
		Assert(_inputEnabled > 0);
		dec(_inputEnabled);
		_Unlock;
	end;

	function Window.Input(clear: boolean): string;
	begin
		_Lock;
		Assert(_inputEnabled > 0);
		result := Copy(_input, 0, _nInput);
		if clear then _nInput := 0;
		_Unlock;
	end;

	procedure Window._RawSetFlag(flag: WindowFlag; value: boolean);
	begin
		if value then Include(_flags, flag) else Exclude(_flags, flag);
	end;

	function Window._GetFlag(flag: WindowFlag): boolean;
	begin
		_Lock;
		result := flag in _flags;
		_Unlock;
	end;

	procedure Window._SetFlag(flag: WindowFlag; value: boolean);
	label _finally_;
	var
		cmd: Comd.pBase;
		fs: Comd.pSetFullscreen absolute cmd;
	begin
		_Lock;
		if value <> (flag in _flags) then
		begin
			case flag of
				window_VSync: Assert(handle = 0);
				window_Fullscreen:
					if handle <> 0 then
					begin
						_Unlock;
						cmd := LockPut(Comd.Op.SetFullscreen, sizeof(Comd.SetFullscreen));
						fs^.enabled := value;
						UnlockPutImmediate(cmd);
						_Lock;
						goto _finally_;
					end;
			end;
			_RawSetFlag(flag, value);
		end;
	_finally_:
		_Unlock;
	end;

	procedure Window._ForceVSync(value: boolean);
	begin
		if handle = 0 then exit;
		if (not Assigned(_wglSwapInterval)) or (not Assigned(_wglGetSwapInterval)) then exit;
		_wglSwapInterval(uint(value));
		_RawSetFlag(window_VSync, _wglGetSwapInterval() > 0);
	{$ifdef Debug}
		if window_VSync in _flags then
			if value then Log('VSync включен', logWarning) else Log('Не удаётся выключить VSync', logWarning)
		else
			if value then Log('Не удаётся выключить VSync', logWarning) else Log('VSync выключен', logOK);
	{$endif}
	end;

	function GetGLProcAddress(const name: string): pointer;
	begin
		result := wglGetProcAddress(pChar(name));
		if not Assigned(result) then result := oglLib.FindProc(name);
	end;

	function ScreenSize: UintVec2;
		function GetOne(enum: cint; const human: string): cint;
		var
			err: dword;
		begin
			result := GetSystemMetrics(enum);
			if result <= 0 then
			begin
				err := GetLastError;
				raise WindowsSpecific.OperationFailed(Format('получить размер экрана ({0})', human), err);
			end;
		end;
	begin
		result := UintVec2.Make(GetOne(SM_CXSCREEN, 'SM_CXSCREEN'), GetOne(SM_CYSCREEN, 'SM_CYSCREEN'));
	end;

	procedure Init;
	begin
		oglLib := DynamicLibrary.Invalid;
	end;

	procedure Done;
	begin
		oglLib.Close;
	end;

initialization
	&Unit('Windowing').Initialize(@Init, @Done);
end.
