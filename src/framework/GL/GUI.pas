unit GUI;

{$include opts.inc}
{$ifdef Debug}
	{-$define DebugFonts}
	{-$define DebugAutoPos}
{$endif}

interface

uses
	USystem, Errors, UMath, Random, UClasses, Utils, U_GL, GLBase, GLClasses, GLUtils, Script, Input, Algo, Streams, Tokenizer, Human,
	SceneGraph, EnvironmentMaps {$ifdef Debug}, ULog {$endif}, Inventories, FreeType;

type
	GUIPosBase = (pos_A, pos_B, pos_Parent01);
	GUIPosBases = array[0 .. 1] of GUIPosBase;
	GUIPosBaseStr = array[0 .. 1] of ansichar;

	GUISizeBase = (size_Literal, size_Parent, size_X1, size_Y1, size_Min1, size_Max1, size_Fit);
	GUISizeBases = array[0 .. 1] of GUISizeBase;

type
	ControlFlag = (gui_ExternalImageSoItsOkayToDrawWithoutExplicitlyChosen, gui_NonSerializable, gui_AutoPos);
	ControlFlags = set of ControlFlag;

	// MouseLeave — мышка была нажата на контроле и, не отпушенная, покинула его.
	// Это требуется явно разрешить (AllowLeave), тогда MouseDown сможет закончиться и MouseUp, и MouseLeave, иначе — только MouseUp.
	// Пример: для кнопки это полезно, для перетаскиваемого окна — наоборот.
	MouseEvent = (gui_MouseEnter, gui_MouseOver, gui_MouseLeave, gui_MouseDown, gui_MouseDragging, gui_MouseAbandonDragging, gui_MouseUp);

	// Simulacra — GUI не будет следить за контролом, пока пользователь его не отжал.
	// Необходимо, если контрол был уничтожен в своём же обработчике — да, такое бывает.
	MouseEventFlag = (gui_MouseProceed, gui_AllowAbandonDragging, gui_BringToFront, gui_Simulacra);
	MouseEventFlags = set of MouseEventFlag;

	tMemoryPart = (gui_RememberPos, gui_RememberSize);
	tMemoryParts = set of tMemoryPart;

type
	pControl = ^Control;
	pHint = ^Hint;

	OnMouseEnterLeaveProc = procedure(ctl: pControl; const gpos: Vec2; const info: SingleDelegateInfo);
	OnMouseOverProc = procedure(ctl: pControl; const gpos, delta: Vec2; const info: SingleDelegateInfo);
	OnMouseDownProc = procedure(ctl: pControl; const gpos: Vec2; const info: SingleDelegateInfo);
	pOnMouseArgs = ^OnMouseArgs;
	OnMouseArgs = record
		ctl: pControl;
		gpos, delta: pVec2;
	end;

	Control = object(&Object)
	private type
		ListenerProc = procedure(ctl: pControl; param: pointer);
	private
		_flags: ControlFlags;
		_pos, _screenPos, _sizes, _screenSizes: Vec2;
		_posBase: GUIPosBases;
		_sizeBase: GUISizeBases;
		_tfDirty: boolean;
		_clipRect: Rect;
		_gui: pControl;
		p_GUIpos, p_GUIsizes, p_GUItex, p_GUIClipRect: pNativeGLValue;
		_material: pGLMaterial;
		_parentId: sint;
		_actions: EntityActions;
		_memoryId: PoolString;
		resizeListeners: &Object.CallbacksList;
		procedure _EnableRuntime;
		procedure _InvalidateTransform;
		procedure _UpdateTransform;
		function _GetFlag(flag: ControlFlag): boolean;
		procedure _SetFlag(flag: ControlFlag; newValue: boolean);

		procedure _SetPos(const newPos: Vec2);
		procedure _SetPosBase(index: sint; newBase: GUIPosBase);
		procedure _SetPosBases(const newBases: GUIPosBases);
		procedure _SetPosBaseX(newBase: GUIPosBase);
		procedure _SetPosBaseY(newBase: GUIPosBase);
		function _GetPosBaseStr: GUIPosBaseStr;
		procedure _SetPosBaseStr(const newBase: GUIPosBaseStr);

		procedure _SetSizes(const newSizes: Vec2);
		procedure _SetSizeBase(index: sint; newBase: GUISizeBase);
		procedure _SetSizeBases(const newBases: GUISizeBases);
		procedure _SetSizeBaseX(newBase: GUISizeBase);
		procedure _SetSizeBaseY(newBase: GUISizeBase);
		function _GetSizeBaseStr: string;
		procedure _SetSizeBaseStr(const newBase: string);

		function _GetScreenPos: Vec2;
		function _GetScreenSizes: Vec2;
		function _GetClipRect: Rect;
		procedure _SetMaterial(newMat: pGLMaterial);
		procedure _SetMemoryID(const newId: PoolString);
		function _MaybeRecall: boolean;
		procedure _MaybeRemember;
	protected
		procedure _RelocateNativeGLValue(old, new: pNativeGLValue); virtual;
		procedure _Update(const dt: float); virtual;
		procedure _AfterAttach; virtual;
		procedure _BeforeDetach; virtual;
		procedure _OnSetSizes; virtual;
		function _HandleMouse(const gpos, delta: Vec2; event: MouseEvent; justTry: boolean): MouseEventFlags; virtual;
		function _HandleMouse(const gpos, delta: Vec2; event: MouseEvent): MouseEventFlags;
		function _WhatToRemember: tMemoryParts; virtual;
		function _AttachHint(hparent, source: pControl; const at: Vec2): pHint; virtual;
		function _ShaderRect: boolean; virtual;
		function _ClipChildrens: boolean; virtual;
		function _GetScreenSize(dim: sint; base: GUISizeBase): float; virtual;
	public
		parent: pControl;
		childs: array of pControl;
		gl: GLEntityParams;
		onMouseEnter, onMouseOver, onMouseLeave, onMouseDown, onHint, onPrettyDetach: MultiDelegate;
		constructor Init;
		destructor Done; virtual;
		procedure Attach(ctrl: pControl); virtual;
		procedure Detach;
		procedure PrettyDetach;
		procedure Draw(RT: pGLRenderTarget); virtual;
		function SerializableChildsCount: sint;
		procedure AddAction(ac: pEntityAction);
		procedure AddResizeListener(proc: ListenerProc; param: pointer);
		procedure RemoveResizeListener(proc: ListenerProc; param: pointer);

		property MemoryID: PoolString read _memoryId write _SetMemoryID;
		property Material: pGLMaterial read _material write _SetMaterial;
		property Position: Vec2 read _pos write _SetPos;
		property PositionBase: GUIPosBases read _posBase write _SetPosBases;
		property PositionBaseStr: GUIPosBaseStr read _GetPosBaseStr write _SetPosBaseStr;
		property PositionBaseX: GUIPosBase read _posBase[0] write _SetPosBaseX;
		property PositionBaseY: GUIPosBase read _posBase[1] write _SetPosBaseY;
		property Sizes: Vec2 read _sizes write _SetSizes;
		property SizeBase: GUISizeBases read _sizeBase write _SetSizeBases;
		property SizeBaseStr: string read _GetSizeBaseStr write _SetSizeBaseStr;
		property SizeBaseX: GUISizeBase read _sizeBase[0] write _SetSizeBaseX;
		property SizeBaseY: GUISizeBase read _sizeBase[1] write _SetSizeBaseY;
		property ScreenPos: Vec2 read _GetScreenPos;
		property ScreenSizes: Vec2 read _GetScreenSizes;
		property ClipRect: Rect read _GetClipRect;
		property Flags: ControlFlags read _flags;
		property Flag[what: ControlFlag]: boolean read _GetFlag write _SetFlag;
	type
		OnHintProc = function(ctl, parent: pControl; const at: Vec2; const info: SingleDelegateInfo): pHint;
		OnHintArgs = record
			ctl, hparent: pControl;
			at: pVec2;
			hint: pHint;
		end;
		OnPrettyDetachProc = procedure(ctl: pControl; const info: SingleDelegateInfo);
	const
		DefaultPosBases: GUIPosBases = (pos_A, pos_A);
		DefaultSizeBases: GUISizeBases = (size_Literal, size_Literal);
	end;

	pControlMove = ^ControlMove;
	ControlMove = object(Slide)
	public
		constructor Init(path: pDimensionalPath);
		destructor Done; virtual;
	protected
		procedure _Process(entity: pObject; const dt: float); virtual;
	private const
		SlideID = 'GM';
	end;

	pImage = ^Image;
	Image = object(Control)
	private
		_im: pTexture;
		_activeAnim: pAtlasAnimation;
		_animStartTime: float;
		_animRotated, _notifyLocaleChanges: boolean;
		p_image: pNativeGLValue;
		_asp: AspectPair;
		procedure _SetIm(newIm: pTexture);
		procedure _UpdateIm;
		procedure _UpdateActiveAnim;
		procedure _UpdateAnimation;
		procedure _SetLocaleWatch(watch: boolean);
	protected
		procedure _RelocateNativeGLValue(old, new: pNativeGLValue); virtual;
		procedure _Update(const dt: float); virtual;
		function _GetScreenSize(dim: sint; base: GUISizeBase): float; virtual;
	public
		constructor Init;
		destructor Done; virtual;
		procedure SwitchToAnim(anim: pAtlasAnimation);
		procedure Draw(RT: pGLRenderTarget); virtual;
		function Corporeal: boolean;
		function AspectPair: pAspectPair;

		property Im: pTexture read _im write _SetIm;
	end;

	pText = ^Text;

	pFont = ^Font;
	Font = object(&Object)
	public type
		pSymRec = ^SymRec;
		SymRec = object
			sym: UTFchar;
			resolutionQuant: sint;
			texA, texB, texC, texD: Vec2;
			ra: RasterizedSymbol;
			procedure Finalize;
			function Hash: Hash.Value;
			function Equals(const another: SymRec): boolean;
		{$ifdef Debug} function InternalName: string; {$endif}
			function RealResolution: float;
		const
			QuantBase = 1.5;
		end;

	private type
		SymPair = object
			a, b: UTFchar;
			function Make(newA, newB: UTFchar): SymPair; static;
			function Hash(const pair: SymPair): Hash.Value; static;
			function Equals(const L, R: SymPair): boolean; static;
		end;

		pSymPairInfo = ^SymPairInfo;
		SymPairInfo = object
			kerning: float;
			function Dummy: SymPairInfo; static;
		end;

		{$define classname:=SymbolSet} {$define key_type:=SymRec} {$include hash.h.inc}
		{$define classname:=tHash_SymPair2Info} {$define key_type:=SymPair} {$define value_type:=SymPairInfo} {$include hash.h.inc}
	private const
		BorderPx = 1 {$ifdef DebugFonts} +1 {$endif};
	private var
		ft, ft2: TrueType;
		im: pTexture;
		apk: AtlasPacker;
		syms: SymbolSet;
		sympairs: tHash_SymPair2Info;
		listeners: array of pText;
		denyKerningPairs: boolean;
		opts: RasterizationOptions;
		function _ReadFromTokens(var ts: tTokenizer): boolean;
		procedure _ClearImage;
		function _AddToAtlas(var rec: SymRec): boolean;
		procedure _PlaceImageInAtlas(var rec: SymRec; const desc: AtlasPacker.InsertDesc);
		function _GetSymRec(const sym: UTFchar; const resolution: float): pSymRec;
		function _Englify(const sym: UTFchar): UTFchar;
		function _GetSymPairInfo(const a, b: UTFchar): pSymPairInfo;
		procedure _AddListener(text: pText);
		procedure _RemoveListener(text: pText);
		procedure _NotifyListeners;
	public
		constructor Init(s: pStream);
		destructor Done; virtual;
		function GetSym(const sym: UTFchar; const resolution: float): pSymRec;
		function GetKerning(const a, b: UTFchar; out kerning: float): boolean;
		procedure Prefetch(const text: string; const resolution: float);
		property Image: pTexture read im;
	end;

	Text = object(Control)
	private
		_scale: float;
		_glMesh: tGLMesh;
		_batch: pBatch;
		_font: pFont;
		_text, _ldText: string;
		_changed, _notifyLocaleChanges: boolean;
		p_image: pNativeGLValue;
		va_pos, va_tex: pNativeGLValue;
		_estimatedResolution, _maxWidth: float;
		_paraIdent: float;
		function _EstimateResolution: float;
		procedure _SetScale(const newScale: float);
		procedure _UpdateScale;
		procedure _SetFont(newFont: pFont);
		procedure _UpdateFont;
		procedure _SetText(const newText: string);
		procedure _UpdateText;
		procedure _SetMaxWidth(const newMax: float);
		procedure _SetParaIdent(const newIdent: float);
		procedure _Redraw;
	protected
		procedure _AfterAttach; virtual;
		procedure _BeforeDetach; virtual;
		procedure _RelocateNativeGLValue(old, new: pNativeGLValue); virtual;
		function _ShaderRect: boolean; virtual;
	public
		constructor Init;
		destructor Done; virtual;
		procedure Draw(RT: pGLRenderTarget); virtual;

		property Scale: float read _scale write _SetScale;
		property Font: pFont read _font write _SetFont;
		property Text: string read _text write _SetText;
		property MaxWidth: float read _maxWidth write _SetMaxWidth;
		property ParaIdent: float read _paraIdent write _SetParaIdent;
	end;

	pButton = ^Button;
	Button = object(Image)
	public type
		OnClickProc = procedure(button: pButton; const info: SingleDelegateInfo);
	private vaR
		_pressed: boolean;
		_pressedAnims: array[boolean] of pAtlasAnimation;
		procedure _Press;
		procedure _Release(silent: boolean);
		procedure _UpdateButtonState;
	protected
		function _HandleMouse(const gpos, delta: Vec2; event: MouseEvent; justTry: boolean): MouseEventFlags; virtual;
	public
		onClick: MultiDelegate;
		constructor Init(baseAnim, pressedAnim: pAtlasAnimation);
		destructor Done; virtual;
	end;

	pIndicatorGroup = ^IndicatorGroup;
	IndicatorGroup = object(Control)
	public type
		pBinding = ^Binding; ppBinding = ^pBinding;
		OnUnbindProc = procedure(binding: pBinding; param: pObject);

		Binding = object(&Object)
		private
			group: pIndicatorGroup;
			indicatorId: sint;
			onUnbind: OnUnbindProc;
			onUnbindParam: pObject;
		public
			constructor Init(group: pIndicatorGroup; indicatorId: sint; onUnbind: OnUnbindProc; onUnbindParam: pObject);
			destructor Done; virtual;
			procedure ChangeValue(const newValue: float);
		end;

		OnCreateControlProc = procedure(group: pIndicatorGroup; const id: PoolString; ctl: pControl; const info: SingleDelegateInfo);
		pOnCreateControlArgs = ^OnCreateControlArgs;
		OnCreateControlArgs = record
			group: pIndicatorGroup;
			id: PoolString;
			ctl: pControl;
		end;

	private type
		pIndicatorData = ^IndicatorData;
		IndicatorData = object
			namae: PoolString;
			anims: array of pAtlasAnimation;
			reversed: boolean;
			constructor Init(const newName: PoolString; const newAnims: array of pAtlasAnimation; newReversed: boolean);
			destructor Done;
		end;

		pActiveIndicator = ^ActiveIndicator;
		ActiveIndicator = object
			id: sint;
			ctl: pImage;
			dm: pDimensionalMove;
			start, finish: Vec2;
			fade, fadeStartTimeout: float;
			discrete: sint;
			stillActive, ignoreNextMove: boolean;

			constructor Init(group: pIndicatorGroup; newId: sint; const newValue: float);
			destructor Done;
			procedure ChangeValue(group: pIndicatorGroup; const newValue: float);
			function Update(const dt: float): boolean;
			procedure Move(const target: Vec2);
			procedure SetFade(const newFade: float);
		end;

	private var
		_ids: array of IndicatorData;
		_active: array of ActiveIndicator;
		_bindings: array of pBinding;

		function _FindActive(id: sint): pActiveIndicator;
		procedure _ChangeValue(id: sint; const newValue: float);
		procedure _RecalculateActives;
		function _GetID(const name: PoolString): sint;
		procedure _Unbind(id: sint);
	protected
		procedure _Update(const dt: float); virtual;
	public
		onCreateControl: MultiDelegate;
		constructor Init;
		destructor Done; virtual;
		procedure Add(const name: PoolString; const anims: array of pAtlasAnimation; reversed: boolean);
		function Bind(const name: PoolString; onUnbind: OnUnbindProc; onUnbindParam: pObject): pBinding;
		procedure Unbind(const name: PoolString);
	end;

	pGUIWindow = ^GUIWindow;
	GUIWindow = object(Image)
	public type
		ActionEnum = (action_None, action_Dragging, action_Sizing);
	private
		_draggable, _sizeable, _skipOnClick: boolean;
		_minSizes, _maxSizes: Vec2;
		_action: ActionEnum;
		function _FixupPosition(const p: Vec2): Vec2;
	protected
		procedure _AfterAttach; virtual;
		function _HandleMouse(const gpos, delta: Vec2; event: MouseEvent; justTry: boolean): MouseEventFlags; virtual;
		function _WhatToRemember: tMemoryParts; virtual;
		function _ClipChildrens: boolean; virtual;
	public
		constructor Init;
		destructor Done; virtual;

		property Draggable: boolean read _draggable write _draggable;
		property Sizeable: boolean read _sizeable write _sizeable;
		property SkipOnClick: boolean read _skipOnClick write _skipOnClick;
		property MinSizes: Vec2 read _minSizes write _minSizes;
		property MaxSizes: Vec2 read _maxSizes write _maxSizes;
	end;

	pMemoryItem = ^MemoryItem;
	MemoryItem = object
		what: tMemoryParts;
		posBase: GUIPosBases;
		sizeBase: GUISizeBases;
		pos, sizes: Vec2;
		function Dummy: MemoryItem; static;
	end;

	pMemory = ^Memory;
	Memory = object
	private type
		{$define classname:=tHash_ID2Item} {$define key_type := PoolString} {$define value_type := MemoryItem}
		{$include hash.h.inc}
	private var
		_mem: tHash_ID2Item;
	public
		procedure Initialize;
		procedure Finalize;
		function Parts(const id: PoolString): tMemoryParts;
		procedure Forget(const id: PoolString);
		function Restore(const id: PoolString; ctl: pControl): boolean;
		procedure Remember(const id: PoolString; ctl: pControl; what: tMemoryParts);
	end;

	Hint = object(GUIWindow)
	public type
		tOnUpdateProc = procedure(hint: pHint; const dt: float; const info: SingleDelegateInfo);
		tOnUpdateArgs = record
			hint: pHint;
			dt: pFloat;
		end;

	private const
		UpdatePeriod = 0.1;

	private
		_updateTimeout: float;
	protected
		procedure _Update(const dt: float); virtual;
	public
		onUpdate: MultiDelegate;
		constructor Init;
		destructor Done; virtual;
	end;

	pGUI = ^GUIRoot;
	GUIRoot = object(GUIWindow)
	private type
		MouseTrackingRec = record
			ctl: pControl;
			flags: MouseEventFlags;
		end;
		pCursorData = ^CursorData;
		CursorData = object
			image: pImage;
			spot: Vec2;
			constructor Init(newImage: pImage; const newSpot: Vec2);
			destructor Done; virtual;
			procedure Open(var gui: GUIRoot);
			procedure Close(var gui: GUIRoot);
			function ImgSize: Vec2;
		end;
		FloatStopMode = (float_JustAsPlanned, float_Abandon, float_Emergency);
		FloatMoveCallback = procedure(const gpos: Vec2; ctl: pControl; param: pointer);
		FloatDoneCallback = function(const gpos: Vec2; ctl: pControl; mode: FloatStopMode; param: pointer): boolean;
		FloatRec = record
			cursor: CursorData;
			moveProc: FloatMoveCallback;
			doneProc: FloatDoneCallback;
			userParam: pointer;
		end;
		SkippingStage = (DontSkip, SkipRequested, ConfirmedSkip);
	public type
		OnAttachDetachProc = procedure(ctl: pControl; attach: boolean; const info: SingleDelegateInfo);
		OnAttachDetachArgs = record
			ctl: pControl;
			attach: boolean;
		end;
	private var
		_mouseEnabled, _neutralized: boolean;
		_mousePos: Vec2;
		_mouseLPressed: boolean;
		_mouseTracking: array of MouseTrackingRec;
		_baseCursor: CursorData;
		_float: array of FloatRec;
		_hint: pHint;
		_skip: SkippingStage;
		_lastUpdate: Ticks;
		procedure _RecalcSizes(RT: pGLRenderTarget);
		procedure _HandleMouseEnabled(newEnabled: boolean);
		procedure _SetMouseEnabled(newEnabled: boolean);
		procedure _MoveMouseBy(const delta: Vec2);
		procedure _UpdateMouse;
		function _GetLastRespondedToMouseEvent(const gpos, delta: Vec2; ev: MouseEvent): pControl;
		procedure _HandleMove(const gpos, delta: Vec2);
		procedure _HandlePress(const gpos: Vec2);
		procedure _HandleDragging(const gpos, delta: Vec2);
		procedure _HandleRelease(const gpos: Vec2; update: boolean);
		procedure _InstantCloseTrackingIfAny(ctl: pControl);
		procedure _BringToFront(ctl: pControl);
		function _FindFloat(img: pImage): sint;
		function _CurrentCursor: pCursorData;
		function _AutoPos(const sz: Vec2; ignore: pControl): Vec2;
		function _AllowMouse(ctl: pControl): boolean;
		procedure _ChangeHint(newHint: pHint);
		procedure _UpdateHint(const mpos: Vec2);
	protected
		procedure _AfterAttach; virtual;
		procedure _BeforeDetach; virtual;
		procedure _OnSetSizes; virtual;
	public
		memory: Memory;
		onAttachDetach: MultiDelegate;
		constructor Init(RT: pGLRenderTarget);
		destructor Done; virtual;
		procedure Neutralize;
		procedure Attach(ctrl: pControl); virtual;
		procedure Update(RT: pGLRenderTarget; const dt: float);
		procedure Draw(RT: pGLRenderTarget); virtual;
		procedure ChangeCursor(newImage: pImage; const newSpot: Vec2);
		procedure StartFloat(newImage: pImage; const newSpot: Vec2; newMoveProc: FloatMoveCallback; newDoneProc: FloatDoneCallback;
			newParam: pointer);
		procedure StopFloat(img: pImage; at: pControl; mode: FloatStopMode);
		procedure AbandonFloat(img: pImage);
		procedure AbandonAnyFloat;
		function Floating: pImage;
		procedure NotifyAtDt(ctl: pControl; attached: boolean);
		procedure SkipEffects;
		procedure SkipEffectsBreakpoint;

		property MousePos: Vec2 read _mousePos write _mousePos;
		property MouseEnabled: boolean read _mouseEnabled write _SetMouseEnabled;
	end;

	pMinimap = ^Minimap;
	Minimap = object(GUIWindow)
	private
		_scene, _target: pSceneNode;
		_op: RTTOperator;
		_shift: Vec3;
		_baseColor: Vec3;
		_baseAlpha: float;
		procedure _Initialize;
		procedure _SetScene(newScene: pSceneNode);
		procedure _SetTarget(newTarget: pSceneNode);
		procedure _UnbindScene;
		procedure _UnbindTarget;
		procedure _UpdateOpTransform;
		procedure _UpdateOpSizes;
	protected
		procedure _AfterAttach; virtual;
		procedure _BeforeDetach; virtual;
		procedure _Update(const dt: float); virtual;
		procedure _OnSetSizes; virtual;
	public
		constructor Init(const newBaseColor: Vec3; const newBaseAlpha: float);
		constructor DeseInit;
		destructor Done; virtual;
		property Scene: pSceneNode read _scene write _SetScene;
		property Target: pSceneNode read _target write _SetTarget;
		property Shift: Vec3 read _shift write _shift;
	end;

	pInventoryWindow = ^InventoryWindow;

	pInventoryItem = ^InventoryItem;
	InventoryItem = object(Image)
	private
		_item: pItem;
		_inv: pInventoryWindow;
		_invId: sint;
	protected
		function _HandleMouse(const gpos, delta: Vec2; event: MouseEvent; justTry: boolean): MouseEventFlags; virtual;
		function _AttachHint(hparent, source: pControl; const at: Vec2): pHint; virtual;
	public
		constructor Init(newItem: pItem; newInv: pInventoryWindow; newInvId: sint);
		destructor Done; virtual;

		property Item: pItem read _item;
		property Inv: pInventoryWindow read _inv;
	end;

	CellularHighlight = object
	private
		tex: tTexture;
		size: UintVec2;
	public
		function Invalid: CellularHighlight; static;
		procedure Initialize(const size: UintVec2);
		procedure Finalize;
		procedure Rect(ax, ay, w, h: sint; const color: Vec4);
		function Texture: pTexture;
	end;

	InventoryWindow = object(GUIWindow)
	private type
		RecalculateItem = (recalc_Sizes, recalc_Bg);
		RecalculateItems = set of RecalculateItem;
	public type
		OnDropProc = function(inv: pInventoryWindow; item: pItem; var tf: Transform; var vel: Vec3; const info: SingleDelegateInfo): boolean;
		OnDropArgs = record
			inv: pInventoryWindow;
			item: pItem;
			tf: Transform;
			vel: Vec3;
			ret: boolean;
		end;
		OnDropRet = (onDrop_AllowDrop, onDrop_HasTransform, onDrop_HasVel);
		OnDropRets = set of OnDropRet;

		ValidateProc = function(inv: pInventoryWindow; const info: SingleDelegateInfo): boolean;
		DestroyProc = procedure(inv: pInventoryWindow; const info: SingleDelegateInfo);
	private var
		_inv: pInventory;
		_items: array of pInventoryItem;
		_floating: array of pointer;
		_tile: boolean;
		_cellSize: float;
		_recalc: RecalculateItems;
		_relBorder, _absBorder: Vec4;
		highlight: CellularHighlight;
		procedure _Initialize;
		procedure _EnableRuntime;
		procedure _DisableRuntime;
		procedure _Recalculate(what: RecalculateItems);
		procedure _Recalculate(item: pInventoryItem);
		procedure MaybeRecalculate;
		procedure _SetCellSize(const newSize: float);
		procedure _SetTile(newTile: boolean);
		procedure _SetBorder(const newBorder: Vec4);
		function _Find(item: pItem): pInventoryItem;
		procedure _HandleAdd(item: pItem);
		procedure _HandleRemove(item: pItem);
		function CallOnDrop(item: pItem; out tf: Transform; out vel: Vec3): OnDropRets;
		function Validate: boolean;
		procedure _Invalidate;
		procedure _AbandonFloatings;
	protected
		procedure _Update(const dt: float); virtual;
		procedure _AfterAttach; virtual;
		procedure _BeforeDetach; virtual;
		function _AttachHint(hparent, source: pControl; const at: Vec2): pHint; virtual;
	public
		onDrop, onValidate, onDestroy: MultiDelegate;
		constructor Init(newInv: pInventory);
		constructor DeseInit;
		destructor Done; virtual;
		procedure Project(const local: Vec2; out ix, iy: sint; center: boolean);
		function ItemAt(const local: Vec2): pItem;
		procedure StartFloat(item: pItem; oitem: pOutdoorItem; const pos: Vec2; isMousePos: boolean);
		function Pick(oitem: pOutdoorItem): boolean;

		property CellSize: float read _cellSize write _SetCellSize;
		property Tile: boolean read _tile write _SetTile;
		property Border: Vec4 read _relBorder write _SetBorder;
	end;

	pConsole = ^Console;
	Console = object(GUIWindow)
	private const
		MAX_HISTORY = 25;
		Prompt: array[boolean] of string = ('> ', '+> ');
	var
		_ss: pScriptState;
		_inputException: KeyboardKey;
		_text: Text;
		_lines, _maxlines: sint;
		_nhist, _hipos: sint;
		_history: array[0 .. MAX_HISTORY - 1] of string;
		_inpcur: sint; // символ _input, после которого находится курсор
		_frozen, _input, _chunk: string;
		_continue, _active: boolean;
		procedure _SetActive(value: boolean);
		function _ApplyInputLine(const line: string): boolean;
		procedure _ApplyInput(const input: string);
		procedure _SubmitInput;
		procedure _AddToHistory(const line: string; shiftOnlyNew: boolean);
		procedure _HandleKey(key: KeyboardKey; event: ButtonEvent);
		procedure _UpdateText;
	protected
		procedure _Update(const dt: float); virtual;
		procedure _AfterAttach; virtual;
		procedure _BeforeDetach; virtual;
		procedure _OnSetSizes; virtual;
	public
		constructor Init(newSS: pScriptState);
		destructor Done; virtual;
		procedure Write(const line: string);
		procedure Clear;

		property Active: boolean read _active write _SetActive;
		property InputException: KeyboardKey read _inputException write _inputException;
	end;

const
	GUIPosBaseIds: array[GUIPosBase] of string = ('a', 'b', 'r');
	GUISizeBaseIds: array[GUISizeBase] of string = ('=', 'p', 'x1', 'y1', 'min1', 'max1', 'fit');

	procedure OpenScript(var script: ScriptState);
	procedure RegisterGUI(gui: pGUI; var script: ScriptState);
	function Script_create_animation(var ss: ScriptState; idx: sint; atlastable: sint): pAtlasAnimation;

implementation

uses
	MMSystem, Script_EngineAPI
{$ifdef use_serialization}, Serialization {$endif}
{$ifdef Profile}, Profile {$endif};

	{$define classname:=Font.SymbolSet} {$define inline_hash:=_1.Hash} {$define inline_eq := _1.Equals(_2)} {$define finalize_key := _1.Finalize}
	{$include hash.pp.inc}

	{$define classname:=Font.tHash_SymPair2Info} {$define hash_func:=Font.SymPair.Hash}
	{$define inline_eq := Font.SymPair.Equals(_1, _2)}
	{$include hash.pp.inc}

	{$define classname:=Memory.tHash_ID2Item} {$define inline_hash := _1.hash} {$define finalize_key := } {$include hash.pp.inc}

	procedure Control._RelocateNativeGLValue(old, new: pNativeGLValue);
	begin
		if old = p_GUIpos      then p_GUIpos := new   else
		if old = p_GUIsizes    then p_GUIsizes := new else
		if old = p_GUItex      then p_GUItex := new   else
		if old = p_GUIClipRect then p_GUIClipRect := new;
	end;

	procedure RelocateControlGLValue(old, new: pNativeGLValue; param: pObject);
	begin
		pControl(param)^._RelocateNativeGLValue(old, new);
	end;

	procedure Control._EnableRuntime;
	begin
		gl.values.SetCallbacks(nil, @RelocateControlGLValue, @self);
	end;

	constructor Control.Init;
	begin
		p_GUIpos   := nil;
		p_GUIsizes := nil;
		p_GUItex   := nil;
		p_GUIClipRect := nil;

		inherited Init;
		_pos := Vec2.Zero;
		_tfDirty := yes;
		_sizes := Vec2.MinusOnes;
		_posBase := DefaultPosBases;
		_sizeBase := DefaultSizeBases;
		_flags := [];
		parent := nil;
		_parentId := -1;
		_gui := nil;
		childs := nil;

		gl.Init;
		_EnableRuntime;
		p_GUIpos   := gl.values.Value('GUIpos', GLType.Vec2, 1, [NativeGLValueFlag.NonSerializable]);
		p_GUIsizes := gl.values.Value('GUIsizes', GLType.Vec2, 1, [NativeGLValueFlag.NonSerializable]);
		p_GUItex   := gl.values.Value('GUItex', GLType.Vec4, 1, [NativeGLValueFlag.NonSerializable]);
		p_GUIClipRect := gl.values.Value('GUIClipRect', GLType.Vec4, 1, [NativeGLValueFlag.NonSerializable]);

		p_GUItex^.SetVec4(Vec4.Make(0.0, 0.0, 1.0, 1.0));
		onMouseEnter.Init;
		onMouseOver.Init;
		onMouseLeave.Init;
		onMouseDown.Init;
		onHint.Init;
		onPrettyDetach.Init;
		_actions.Init;
		_memoryId := '';
		resizeListeners.Init;
	end;

	destructor Control.Done;
	var
		i: sint;
	begin
		Release(_material);
		for i := High(childs) downto 0 do
			childs[i]^.Detach;

		_actions.Done;
		onPrettyDetach.Done;
		onHint.Done;
		onMouseEnter.Done;
		onMouseOver.Done;
		onMouseLeave.Done;
		onMouseDown.Done;
		gl.Done;
		Assert(resizeListeners.Empty);
		resizeListeners.Done;
		inherited Done;
	end;

	procedure Control.Attach(ctrl: pControl);
	begin
		if not Assigned(ctrl) then exit;
		Assert(not Assigned(ctrl^.parent));
		SetLength(childs, length(childs) + 1);
		childs[High(childs)] := ctrl^.NewRef;
		ctrl^.parent := @self;
		ctrl^._parentId := High(childs);
		if Assigned(_gui) then ctrl^._AfterAttach;
	end;

	procedure Control.Detach;
	var
		ref: pControl;
	begin
		if not Assigned(parent) then exit;
		Assert(parent^.childs[_parentId] = @self);
		if Assigned(_gui) then _BeforeDetach;
		ref := parent^.childs[_parentId];
		parent^.childs[_parentId] := parent^.childs[High(parent^.childs)];
		parent^.childs[_parentId]^._parentId := _parentId;
		SetLength(parent^.childs, length(parent^.childs) - 1);
		parent := nil;
		_parentId := -1;
		Release(ref);
	end;

	procedure _CallOnPrettyDetach(const info: SingleDelegateInfo; param: pointer);
	var
		ctl: pControl absolute param;
	begin
		Control.OnPrettyDetachProc(info.proc)(ctl, info);
	end;

	procedure Control.PrettyDetach;
	begin
		if not onPrettyDetach.Empty then
			onPrettyDetach.Call(@_CallOnPrettyDetach, @self)
		else
			Detach;
	end;

	procedure Control.Draw(RT: pGLRenderTarget);
	var
		i: sint;
	begin
		for i := 0 to High(childs) do
		begin
			childs[i]^._UpdateTransform;
			childs[i]^.Draw(rt);
		end;
	end;

	function Control.SerializableChildsCount: sint;
	var
		i: sint;
	begin
		result := 0;
		for i := 0 to High(childs) do
			if not (gui_NonSerializable in childs[i]^.flags) then
				inc(result);
	end;

	procedure Control.AddAction(ac: pEntityAction);
	begin
		_actions.Add(@self, ac);
	end;

	procedure Control.AddResizeListener(proc: ListenerProc; param: pointer);
	begin
		resizeListeners.Add(&Object.Callback(proc), param);
	end;

	procedure Control.RemoveResizeListener(proc: ListenerProc; param: pointer);
	begin
		resizeListeners.Remove(&Object.Callback(proc), param);
	end;

	function ActionWorthyToSkip(id: uint; param: pointer): boolean;
	begin
		result := not InheritsFrom(TypeOf(pEntityActions(param)^.list[id]^), TypeOf(SceneNodeTimer));
	end;

	procedure Control._Update(const dt: float);
	var
		i: sint;
		gui: pGUI;
		adjustedDt: float;
		skip: GUIRoot.SkippingStage;
	begin
		for i := 0 to High(childs) do
		begin
			if i > High(childs) then break;
			childs[i]^._Update(dt);
		end;

		gui := pGUI(_gui);
		if not Assigned(gui) then exit;
		adjustedDt := dt;
		skip := gui^._skip;
		if skip in [SkipRequested, ConfirmedSkip] then adjustedDt *= 9.0;
		if (skip = SkipRequested) and Range.Open(_actions.Count).Any(@ActionWorthyToSkip, @_actions) then gui^._skip := ConfirmedSkip;
		_actions.Process(@self, adjustedDt);
	end;

	procedure Control._AfterAttach;
	var
		i: sint;
		recalled: boolean;
	begin
		if not Assigned(_gui) then
		begin
			Assert(Assigned(parent));
			_gui := parent^._gui;
			Assert(Assigned(_gui));
			if not Assigned(_material) then SetRef(_material, pGUI(_gui)^.Material);
			pGUI(_gui)^.NotifyAtDt(@self, yes);
		end else
			Assert(_gui = @self);
		_tfDirty := yes;

		recalled := _MaybeRecall;
		if gui_AutoPos in _flags then
		begin
			if not recalled then _pos := pGUI(_gui)^._AutoPos(sizes, @self);
			Exclude(_flags, gui_AutoPos);
		end;
		for i := 0 to High(childs) do
		begin
			if (_gui = @self) and Assigned(childs[i]^._gui) then continue;
			childs[i]^._AfterAttach;
		end;
	end;

	procedure Control._BeforeDetach;
	var
		i: sint;
	begin
		Assert(Assigned(_gui));
		_MaybeRemember;
		pGUI(_gui)^._InstantCloseTrackingIfAny(@self);
		if _gui <> @self then
		begin
			pGUI(_gui)^.NotifyAtDt(@self, no);
			if _material = pGUI(_gui)^.Material then Release(_material);
			_gui := nil;
		end;
		for i := 0 to High(childs) do
			childs[i]^._BeforeDetach;
	end;

	procedure Control._OnSetSizes;
	begin
		_InvalidateTransform;
		resizeListeners.Execute(@self, no);
	end;

	procedure _CallOnMouseEnterLeave(const info: SingleDelegateInfo; param: pointer);
	var
		args: pOnMouseArgs absolute param;
	begin
		with args^ do OnMouseEnterLeaveProc(info.proc)(ctl, gpos^, info);
	end;

	procedure _CallOnMouseOver(const info: SingleDelegateInfo; param: pointer);
	var
		args: pOnMouseArgs absolute param;
	begin
		with args^ do OnMouseOverProc(info.proc)(ctl, gpos^, delta^, info);
	end;

	procedure _CallOnMouseDown(const info: SingleDelegateInfo; param: pointer);
	var
		args: pOnMouseArgs absolute param;
	begin
		with args^ do OnMouseDownProc(info.proc)(ctl, gpos^, info);
	end;

	function Control._HandleMouse(const gpos, delta: Vec2; event: MouseEvent; justTry: boolean): MouseEventFlags;
	var
		args: OnMouseArgs;
	begin
		Assert((@gpos = @gpos) and (@event = @event) and (@delta = @delta) and (@justTry = @justTry));
		result := [];
		case event of
			gui_MouseEnter, gui_MouseOver, gui_MouseLeave:
				begin
					if not (onMouseEnter.Empty and onMouseOver.Empty and onMouseLeave.Empty) then
					begin
						result += [gui_MouseProceed];
						if not justTry then
						begin
							args.ctl := @self;
							args.gpos := @gpos;
							args.delta := @delta;
						end;
					end else
						if not onHint.Empty then result += [gui_MouseProceed];
				end;
			gui_MouseDown:
				if not onMouseDown.Empty then
				begin
					result += [gui_MouseProceed];
					if not justTry then
					begin
						args.ctl := @self;
						args.gpos := @gpos;
						onMouseDown.Call(@_CallOnMouseDown, @args);
					end;
				end;
		end;
		if not justTry then
			case event of
				gui_MouseEnter: if not onMouseEnter.Empty then onMouseEnter.Call(@_CallOnMouseEnterLeave, @args);
				gui_MouseLeave: if not onMouseLeave.Empty then onMouseLeave.Call(@_CallOnMouseEnterLeave, @args);
				gui_MouseOver: if not onMouseOver.Empty then onMouseOver.Call(@_CallOnMouseOver, @args);
			end;
	end;

	function Control._HandleMouse(const gpos, delta: Vec2; event: MouseEvent): MouseEventFlags;
	begin
		result := _HandleMouse(gpos, delta, event, no);
	end;

	function Control._GetFlag(flag: ControlFlag): boolean;
	begin
		result := flag in _flags;
	end;

	procedure Control._SetFlag(flag: ControlFlag; newValue: boolean);
	begin
		if newValue <> (flag in _flags) then
		begin
			if newValue then
				Include(_flags, flag)
			else
				Exclude(_flags, flag);
		end;
	end;

	procedure Control._SetPos(const newPos: Vec2);
	begin
		if _pos <> newPos then
		begin
			_pos := newPos;
			_InvalidateTransform;
		end;
	end;

	procedure Control._SetPosBase(index: sint; newBase: GUIPosBase);
	begin
		if _posBase[index] <> newBase then
		begin
			_posBase[index] := newBase;
			_InvalidateTransform;
		end;
	end;

	procedure Control._SetPosBases(const newBases: GUIPosBases);
	var
		i: sint;
	begin
		for i := 0 to High(_posBase) do
			if _posBase[i] <> newBases[i] then
			begin
				_posBase := newBases;
				_InvalidateTransform;
				exit;
			end;
	end;

	procedure Control._SetPosBaseX(newBase: GUIPosBase);
	begin
		_SetPosBase(0, newBase);
	end;

	procedure Control._SetPosBaseY(newBase: GUIPosBase);
	begin
		_SetPosBase(1, newBase);
	end;

	function Control._GetPosBaseStr: GUIPosBaseStr;
	var
		i: sint;
	begin
		for i := 0 to High(_posBase) do
			result[i] := GUIPosBaseIds[_posBase[i]][1];
	end;

	procedure Control._SetPosBaseStr(const newBase: GUIPosBaseStr);
	var
		i: sint;
	begin
		for i := 0 to High(_posBase) do
			_SetPosBase(i, GUIPosBase(FindStr(newBase[i], GUIPosBaseIds, ord(_posBase[i]))));
	end;

	procedure Control._SetSizes(const newSizes: Vec2);
	begin
		if _sizes <> newSizes then
		begin
			_sizes := newSizes;
			_OnSetSizes;
		end;
	end;

	procedure Control._SetSizeBase(index: sint; newBase: GUISizeBase);
	begin
		if _sizeBase[index] <> newBase then
		begin
			_sizeBase[index] := newBase;
			_InvalidateTransform;
		end;
	end;

	procedure Control._SetSizeBases(const newBases: GUISizeBases);
	var
		i: sint;
	begin
		for i := 0 to High(_sizeBase) do
			if _sizeBase[i] <> newBases[i] then
			begin
				_sizeBase := newBases;
				_InvalidateTransform;
				exit;
			end;
	end;

	procedure Control._SetSizeBaseX(newBase: GUISizeBase);
	begin
		_SetSizeBase(0, newBase);
	end;

	procedure Control._SetSizeBaseY(newBase: GUISizeBase);
	begin
		_SetSizeBase(1, newBase);
	end;

	function Control._GetSizeBaseStr: string;
	var
		i: sint;
	begin
		result := '';
		for i := 0 to High(_sizeBase) do
			result += GUISizeBaseIds[_sizeBase[i]];
	end;

	procedure Control._SetSizeBaseStr(const newBase: string);
	var
		i, ofs: sint;
		sb: GUISizeBase;
	begin
		ofs := 1;
		for i := 0 to High(_sizeBase) do
		begin
			if Prefixed(string(','), pChar(newBase) + (ofs - 1), length(newBase) - ofs + 1) then inc(ofs);
			for sb in GUISizeBase do
				if Prefixed(string(GUISizeBaseIds[sb]), pChar(newBase) + (ofs - 1), length(newBase) - ofs + 1) then
				begin
					_SetSizeBase(i, sb);
					ofs += length(GUISizeBaseIds[sb]);
					if ofs > length(newBase) then ofs := 1;
					break;
				end;
		end;
	end;

	function Control._GetScreenPos: Vec2;
	begin
		_UpdateTransform;
		result := _screenPos;
	end;

	function Control._GetScreenSizes: Vec2;
	begin
		_UpdateTransform;
		result := _screenSizes;
	end;

	function Control._GetClipRect: Rect;
	begin
		_UpdateTransform;
		result := _clipRect;
	end;

	procedure Control._SetMaterial(newMat: pGLMaterial);
	begin
		if _material <> newMat then
		begin
			if Assigned(newMat) then
				SetRef(_material, newMat)
			else
				if Assigned(_gui) then
					SetRef(_material, pGUI(_gui)^.Material)
				else
					Release(_material);
		end;
	end;

	procedure Control._SetMemoryID(const newId: PoolString);
	begin
		Assert(_memoryId <> newId, 'redundant memoryID change');
		_memoryId := newId;
		_MaybeRecall;
	end;

	function Control._MaybeRecall: boolean;
	begin
		result := Assigned(_gui) and not _memoryId.Empty and pGUI(_gui)^.memory.Restore(_memoryId, @self);
		if result then pGUI(_gui)^.memory.Forget(_memoryId);
	end;

	procedure Control._MaybeRemember;
	begin
		if Assigned(_gui) and not _memoryId.Empty then
			pGUI(_gui)^.memory.Remember(_memoryId, @self, _WhatToRemember);
	end;

	procedure Control._InvalidateTransform;
	var
		i: sint;
	begin
		if not _tfDirty then
		begin
			_tfDirty := yes;
			for i := 0 to High(childs) do
				childs[i]^._InvalidateTransform;
		end;
	end;

	procedure Control._UpdateTransform;
	var
		i: sint;
	begin
		if not _tfDirty then exit;

		for i := 0 to High(_sizeBase) do
			_screenSizes.data[i] := _GetScreenSize(i, _sizeBase[i]);

		if Assigned(parent) then
		begin
			for i := 0 to High(_posBase) do
				case _posBase[i] of
					pos_A: _screenPos.data[i] := parent^.ScreenPos.data[i] + _pos.data[i];
					pos_B: _screenPos.data[i] := parent^.ScreenPos.data[i] + parent^.ScreenSizes.data[i] - _screenSizes.data[i] - _pos.data[i];
					pos_Parent01: _screenPos.data[i] := lerp(parent^.ScreenPos.data[i], parent^.ScreenPos.data[i] + parent^.ScreenSizes.data[i] - _screenSizes.data[i], _pos.data[i]);
					else Assert(no);
				end;
			if Assigned(_gui) then
				p_GUIpos^.SetVec2(Vec2.Make(_screenPos.x, pGUI(_gui)^.ScreenSizes.y - _screenPos.y - _screenSizes.y));
			if _ShaderRect then p_GUIsizes^.SetVec2(_screenSizes);
		end;
		_clipRect := Rect.Make(_screenPos, _screenPos + _screenSizes);
		if Assigned(parent) and parent^._ClipChildrens then
			_clipRect := RectsIntersection(_clipRect, parent^._clipRect);
		if Assigned(_gui) then
			p_GUIClipRect^.SetVec4(Vec4.Make(_clipRect.A.x, pGUI(_gui)^.Sizes.y - _clipRect.B.y, _clipRect.B.x, pGUI(_gui)^.Sizes.y - _clipRect.A.y));
		_tfDirty := no;
	end;

	function Control._WhatToRemember: tMemoryParts;
	begin
		result := [gui_RememberPos];
	end;

	procedure _CallOnHint(const info: SingleDelegateInfo; param: pointer);
	var
		args: ^Control.OnHintArgs absolute param;
	begin
		with args^ do hint := Control.OnHintProc(info.proc)(ctl, hparent, at^, info);
	end;

	function Control._AttachHint(hparent, source: pControl; const at: Vec2): pHint;
	var
		args: OnHintArgs;
	begin
		Assert(@source = @source);
		if not onHint.Empty then
		begin
			args.ctl := @self;
			args.hparent := hparent;
			args.at  := @at;
			onHint.Call(@_CallOnHint, @args);
			result := args.hint;
			if Assigned(result) then
				if Assigned(result^.parent) then Assert(result^.parent = hparent) else hparent^.Attach(result);
		end else
			result := nil;
	end;

	function Control._ShaderRect: boolean;
	begin
		result := yes;
	end;

	function Control._ClipChildrens: boolean;
	begin
		result := no;
	end;

	function Control._GetScreenSize(dim: sint; base: GUISizeBase): float;
	begin
		case base of
			size_Literal: result := _sizes.data[dim];
			size_Parent: if Assigned(parent) then result := parent^.screenSizes.data[dim] * _sizes.data[dim] else result := _sizes.data[dim];
			else Assert(no, GUISizeBaseIds[base]);
		end;
	end;

	procedure ControlMove._Process(entity: pObject; const dt: float);
	begin
		inherited _Process(entity, dt);
		pControl(entity)^.Position := dm.CurrentV2;
	end;

	constructor ControlMove.Init(path: pDimensionalPath);
	begin
		inherited Init(path, SlideID);
	end;

	destructor ControlMove.Done;
	begin
		inherited Done;
	end;

	procedure Image._RelocateNativeGLValue(old, new: pNativeGLValue);
	begin
		if old = p_image then p_image := new else
			inherited _RelocateNativeGLValue(old, new);
	end;

	procedure Image._Update(const dt: float);
	begin
		_UpdateAnimation;
		inherited _Update(dt);
	end;

	function Image._GetScreenSize(dim: sint; base: GUISizeBase): float;
	const
		NA = asp2_x1;
		Sb2Asp2: array[GUISizeBase] of Aspect2Method = (NA, NA, asp2_x1, asp2_y1, asp2_min1, asp2_max1, NA);
	var
		psz: Vec2;
		ap: pAspectPair;
	begin
		ap := AspectPair;
		case base of
			size_X1, size_Y1, size_Min1, size_Max1: result := ap^.Aspect2Item(Sb2Asp2[base], dim, _sizes.data[dim]);
			size_Fit:
				begin
					psz := parent^.screenSizes;
					if (psz.y <> 0.0) and (psz.x > psz.y * ap^.aspect) then
						result := ap^.Aspect2Item(asp2_y1, dim, psz.y)
					else
						result := ap^.Aspect2Item(asp2_x1, dim, psz.x);
					result *= _sizes.data[dim];
				end;
			else
				result := inherited _GetScreenSize(dim, base);
		end;
	end;

	procedure Image._SetIm(newIm: pTexture);
	begin
		if _im <> newIm then
		begin
			SetRef(_im, newIm);
			_UpdateIm;
		end;
	end;

	procedure Image._UpdateIm;
	begin
		p_image^.SetTex(_im);
		if Assigned(_im) then
			_asp := GLUtils.AspectPair.Make(_im^.Size.XY);
	end;

	procedure _Image_ChangeLocale(var loc: Locale; const info: SingleDelegateInfo);
	var
		img: pImage absolute info.user;
	begin
		Assert(Locale.OnChangeProc(@_Image_ChangeLocale) = @_Image_ChangeLocale);
		Assert(@loc = @loc);
		img^._InvalidateTransform;
	end;

	procedure Image._UpdateActiveAnim;
	begin
		if (not Assigned(Im)) and Assigned(_activeAnim) and Assigned(_activeAnim^.Atlas) and Assigned(_activeAnim^.Atlas^.LinkedImage) then
			Im := _activeAnim^.Atlas^.LinkedImage;
		_SetLocaleWatch(Assigned(_activeAnim) and (anim_NotifyLocaleChanges in _activeAnim^.Flags));
	end;

	procedure Image._UpdateAnimation;
	var
		tf: Rect;
	begin
		if Assigned(_activeAnim) and _activeAnim^.GetRect(mm.GUITimeSince(_animStartTime), tf, _animRotated) then
			p_GUITex^.SetVec4(Vec4.Make(tf.A.x, 1.0 - tf.A.y - tf.SizeY, tf.SizeX, tf.SizeY));
	end;

	procedure Image._SetLocaleWatch(watch: boolean);
	begin
		if watch <> _notifyLocaleChanges then
		begin
			_notifyLocaleChanges := watch;
			if watch then mm.locale.onChange.Add(@_Image_ChangeLocale, @self) else mm.locale.onChange.Remove(@_Image_ChangeLocale, @self);
		end;
	end;

	constructor Image.Init;
	begin
		p_image := nil;
		inherited Init;
		p_image := gl.values.Value('image', GLType.Sampler, 1, [NativeGLValueFlag.NonSerializable]);
		_im := nil;
		_activeAnim := nil;
		_animStartTime := 0.0;
		_notifyLocaleChanges := no;
		_asp := GLUtils.AspectPair.Identity;
	end;

	destructor Image.Done;
	begin
		_SetLocaleWatch(no);
		Release(_activeAnim);
		Release(_im);
		inherited Done;
	end;

	procedure Image.SwitchToAnim(anim: pAtlasAnimation);
	begin
		if anim <> _activeAnim then
		begin
			SetRef(_activeAnim, anim);
			_animStartTime := mm.GUITime;
			_UpdateActiveAnim;
		end;
	end;

	procedure Image.Draw(RT: pGLRenderTarget);
	var
		rp: tParams4Renderable;
		mesh: pGLMesh;
	begin
		if Corporeal then
		begin
			rp.Reset;
			rp.rt := RT;
			rp.roParams := @gl;
			rp.useMatBlend := yes;
			if _animRotated then mesh := @Quad01CW else mesh := @Quad01;
			mesh^.Draw(rp, _material^.AnyLevel);
			rp.Finalize;
		end;
		inherited Draw(rt);
	end;

	function Image.Corporeal: boolean;
	begin
		result := Assigned(_im) or (gui_ExternalImageSoItsOkayToDrawWithoutExplicitlyChosen in _flags);
	end;

	function Image.AspectPair: pAspectPair;
	begin
		if Assigned(_activeAnim) then
			result := @_activeAnim^.asp
		else
			result := @_asp;
	end;

	function _FontStream(const token: string; param: pointer): string;
	begin
		Assert(@param = @param);
		if token = '' then result := Paths.MiscLibs else result := '';
	end;

	function Font._ReadFromTokens(var ts: tTokenizer): boolean;
	var
		id: string;
	begin
	{$ifdef Debug} LogR('Загрузка шрифта из ' + StreamPath.Log(ts.StreamPath) + '... '); {$endif}
		result := no;
		repeat
			id := '';
			case ts.NextTokenType of
				token_Identifier: id := ts.ReadIdentifier;
				token_Finale: break;
				else
				begin
				{$ifdef Debug} ts.UnexpectedToken; {$endif}
					exit;
				end;
			end;
			if id = 'ttf' then
			begin
				ft.Unload;
				ft := TrueType.Load(ts.ReadStream(@_FontStream));
			end else
			if id = 'secondary_ttf' then
			begin
				ft2.Unload;
				ft2 := TrueType.Load(ts.ReadStream(@_FontStream));
			end else
			if id = 'base_color4' then opts.BaseColor := ts.ReadVec4 else
			if id = 'outline_color4' then opts.OutlineColor := ts.ReadVec4 else
			if id = 'outline_size' then opts.OutlineSize := ts.ReadFloat else
			if id = 'deny_kerning_pairs' then denyKerningPairs := yes else
				raise UnknownIdentifier(id, 'Font');
		until no;
		result := yes;
	{$ifdef Debug} Log('Шрифт ' + StreamPath.Log(ts.StreamPath) + ' загружен', logOK); {$endif}
	end;

	procedure Font.SymRec.Finalize;
	begin
		ra.Done;
	end;

	function Font.SymRec.Hash: Hash.Value;
	begin
		result := Algo.Hash.OfUint(sym) xor Algo.Hash.OfUint(uint(resolutionQuant));
	end;

	function Font.SymRec.Equals(const another: SymRec): boolean;
	begin
		result := (sym = another.sym) and (resolutionQuant = another.resolutionQuant);
	end;

{$ifdef Debug}
	function Font.SymRec.InternalName: string;
	begin
		result := '"' + UTF8.CodepointToString(sym) + '" (qS = ' + ToString(resolutionQuant) + ')';
	end;
{$endif}

	function Font.SymRec.RealResolution: float;
	begin
		result := pow(QuantBase, resolutionQuant);
	end;

	function Font.SymPair.Make(newA, newB: UTFchar): SymPair;
	begin
		result.a := newA;
		result.b := newB;
	end;

	function Font.SymPair.Hash(const pair: SymPair): Hash.Value;
	begin
		result := Algo.Hash.OfUint(pair.a) xor Ror(Algo.Hash.OfUint(pair.b), bitsizeof(Algo.Hash.Value) div 2);
	end;

	function Font.SymPair.Equals(const L, R: SymPair): boolean;
	begin
		result := (L.a = R.a) and (L.b = R.b);
	end;

	function Font.SymPairInfo.Dummy: SymPairInfo;
	begin
		result.kerning := 0.0;
	end;

	procedure Font._ClearImage;
	var
		data: pVec4u8;
		size: size_t;
	begin
		size := GetTextureDataSize(im^.size.XY, GLformat_RGBA);
		data := GetMem(size);
		Zero(data, size);
		im^.SubImage(UintVec2.Zero, im^.Size.XY, GLformat_RGBA, size, data, 0, yes);
	end;

	function apk_resize(const newSize: UintVec2; param: pointer): boolean;
	var
		font: pFont absolute param;
	begin
		font^.im^.Resize(newSize);
		font^._ClearImage;
		font^._NotifyListeners;
		result := yes;
	end;

	function apk_getall(param: pointer): AtlasPacker.BatchInsertDesc;
	var
		font: pFont absolute param;
		v: GUI.Font.pSymRec;
		it: GUI.Font.SymbolSet.Iterator;
		n: sint;
	begin
		SetLength(result, font^.syms.Count);
		n := 0;

		it := font^.syms.GetIterator;
		while font^.syms.Next(it) do
		begin
			v := font^.syms.GetKey(it);
			if Assigned(v^.ra.image) then
			begin
				result[n] := AtlasPacker.InsertDesc.Make(v^.ra.image^.Size.XY, v);
				inc(n);
			end;
		end;
		SetLength(result, n);
	end;

	procedure apk_apply(const desc: AtlasPacker.InsertDesc; param: pointer);
	var
		font: pFont absolute param;
	begin
		font^._PlaceImageInAtlas(GUI.Font.pSymRec(desc.user)^, desc);
	end;

	function Font._AddToAtlas(var rec: SymRec): boolean;
	begin
		Assert(Assigned(rec.ra.image));
		result := apk.Insert(AtlasPacker.InsertDesc.Make(rec.ra.image^.Size.XY, @rec), @apk_resize, @apk_getall, @apk_apply, @self);
	end;

{$ifdef DebugFonts}
var
	blits, rotations: uint;
	blitsTime, rotationsTime: Ticks;
{$endif}

	procedure Font._PlaceImageInAtlas(var rec: SymRec; const desc: AtlasPacker.InsertDesc);
{$ifdef DebugFonts}
	type
		SizeofMinusOneIsBorderPx = array[0 .. BorderPx] of byte;
	var
		t: Ticks;
{$if sizeof(SizeofMinusOneIsBorderPx) - 1 > 0}
	const
		BorderColorA: Vec3u8 = (0, 128, 255);
		BorderColorB: Vec3u8 = (0, 51, 204);
		BorderColorC: Vec3u8 = (0, 0, 178);
		BorderColorD: Vec3u8 = (0, 0, 102);
	var
		sx, sy, x, y: sint;
		sub: pVec3u8;
{$endif BorderPx > 0}
{$endif DebugFonts}
	var
		a, b: Vec2;
		si: TextureImage;
	begin
		Assert(Assigned(rec.ra.image));

		if desc.rotated then
		begin
			si.Init(rec.ra.image^.target, rec.ra.image^.Size, rec.ra.image^.format);
		{$ifdef DebugFonts} t := Ticks.Get; {$endif}
			si.Blit(rec.ra.image^, 0, 0, 0, 0, img_Replace);

		{$ifdef DebugFonts}
			blitsTime += t.Elapsed; inc(blits);
			t := Ticks.Get;
		{$endif}

			si.RotateCwOZ;
		{$ifdef DebugFonts} rotationsTime += t.Elapsed; inc(rotations); {$endif}
			im^.SubImage(si, desc.pos);
			si.Done;
		end else
			im^.SubImage(rec.ra.image^, desc.pos);

	{$if defined(DebugFonts) and (sizeof(SizeofMinusOneIsBorderPx) - 1 > 0)}
		sx := desc.size.x + 2 * BorderPx;
		sub := nil;
		sub := GetMem(sx * sizeof(sub^));
		for x := 0 to sx - 1 do
			sub[x] := BorderColorA;
		im^.SubImage(desc.pos - UintVec2.Make(BorderPx), UintVec2.Make(sx, 1), GLformat_RGB, sx * sizeof(sub^), sub);
		for x := 0 to sx - 1 do
			sub[x] := BorderColorC;
		im^.SubImage(desc.pos + IntVec2.Make(-BorderPx, desc.size.y + (BorderPx - 1)), UintVec2.Make(sx, 1), GLformat_RGB, sx * sizeof(sub^), sub);
		FreeMem(sub);

		sy := desc.size.y + 2 * BorderPx;
		sub := GetMem(sy * sizeof(sub^));
		for y := 0 to sy - 1 do
			sub[y] := BorderColorB;
		im^.SubImage(desc.pos - UintVec2.Make(BorderPx), UintVec2.Make(1, sy), GLformat_RGB, sy * sizeof(sub^), sub);
		for y := 0 to sy - 1 do
			sub[y] := BorderColorD;
		im^.SubImage(desc.pos + IntVec2.Make(desc.size.x + (BorderPx - 1), -BorderPx), UintVec2.Make(1, sy), GLformat_RGB, sy * sizeof(sub^), sub);
		FreeMem(sub);
	{$endif}

		a := desc.pos / apk.FullSize;
		b := (desc.pos + desc.size) / apk.FullSize;
		if desc.rotated then
		begin
			rec.texA := Vec2.Make(b.x, a.y);
			rec.texB := a;
			rec.texC := Vec2.Make(a.x, b.y);
			rec.texD := b;
		end else
		begin
			rec.texA := a;
			rec.texB := Vec2.Make(a.x, b.y);
			rec.texC := b;
			rec.texD := Vec2.Make(b.x, a.y);
		end;
	end;

	function Font._GetSymRec(const sym: UTFchar; const resolution: float): pSymRec;
	var
		ns: SymRec;
		ok: boolean;
	begin
		ns.sym := sym;
		ns.resolutionQuant := round(0.4 + logn(SymRec.QuantBase, max(resolution, 1.0)));
		result := syms.Find(ns);

		if not Assigned(result) then
		begin
			ok := ft.Rasterize(sym, ns.RealResolution, opts, ns.ra);
			if not ok and (sym = ord(TabSym)) then
			begin
				ok := ft.Rasterize(ord(' '), ns.RealResolution, opts, ns.ra);
				if ok then ns.ra.cursorShift *= 4.0;
			end;
			if not ok and ft2.OK then ok := ft2.Rasterize(sym, ns.RealResolution, opts, ns.ra);
			if not ok and (sym <> ord('?')) then
			begin
				ok := ft.Rasterize(ord('?'), ns.RealResolution, opts, ns.ra);
				if not ok and ft2.OK then ok := ft2.Rasterize(ord('?'), ns.RealResolution, opts, ns.ra);
			end;
			if not ok then
			begin
			{$ifdef Debug} Log('Не удалось ни получить символ {0}, ни заменить его заглушкой.', ns.InternalName, logError); {$endif}
				exit;
			end;
			if Assigned(ns.ra.image) then
			begin
				ok := _AddToAtlas(ns);
			{$ifdef Debug}
				if ok then
					Log('Символ {0} добавлен в атлас', ns.InternalName, logDebug)
				else
					Log('Не удалось добавить {0} в атлас', ns.InternalName, logError);
			{$endif}
			end {$ifdef Debug} else Log('Изображение символа {0} пусто', ns.InternalName, logDebug) {$endif};
			if ok then
				result := syms.Add(ns)
			else
			begin
				ns.Finalize;
				result := nil;
			end;
		end;
	end;

	function Font._Englify(const sym: UTFchar): UTFchar;
	begin
		case sym of
			{А} $410: result := {A} $41;
			{В} $412: result := {B} $42;
			{Е} $415: result := {E} $45;
			{Ё} $401: result := {E} $45;
			{К} $41A: result := {K} $4B;
			{М} $41C: result := {M} $4D;
			{Н} $41D: result := {H} $48;
			{О} $41E: result := {O} $4F;
			{Р} $420: result := {P} $50;
			{С} $421: result := {C} $43;
			{Т} $422: result := {T} $54;
			{Х} $425: result := {X} $58;
			{а} $430: result := {a} $61;
			{е} $435: result := {e} $65;
			{ё} $451: result := {e} $65;
			{о} $43E: result := {o} $6F;
			{р} $440: result := {p} $70;
			{с} $441: result := {c} $63;
			{у} $443: result := {y} $79;
			{х} $445: result := {x} $78;
			else
				result := sym;
		end;
	end;

	function Font._GetSymPairInfo(const a, b: UTFchar): pSymPairInfo;
	var
		key: SymPair;
	begin
		key := SymPair.Make(_Englify(a), _Englify(b));
		result := sympairs.Find(key);

		if not Assigned(result) then
		begin
			result := sympairs.Add(key, SymPairInfo.Dummy);
			result^.kerning := ft.Kerning(a, b);
		end;
	end;

	procedure Font._AddListener(text: pText);
{$ifdef Debug} var i: sint; {$endif}
	begin
	{$ifdef Debug} for i := 0 to High(listeners) do Assert(listeners[i] <> text, text^.text); {$endif}
		SetLength(listeners, length(listeners) + 1);
		listeners[High(listeners)] := text;
	end;

	procedure Font._RemoveListener(text: pText);
	var
		i: sint;
	begin
		for i := 0 to High(listeners) do
			if listeners[i] = text then
			begin
				listeners[i] := listeners[High(listeners)];
				SetLength(listeners, length(listeners) - 1);
				exit;
			end;
		Assert(no, text^.text);
	end;

	procedure Font._NotifyListeners;
	var
		i: sint;
	begin
		for i := 0 to High(listeners) do
			listeners[i]^._changed := yes;
	end;

	constructor Font.Init(s: pStream);
	var
		ts: pTokenizer;
		ok: boolean;
	begin
		inherited Init;
		im := MakeRef(new(pTexture, Init(GLtexture_2D, UintVec2.Make(256, 256), GLformat_RGBA, [], texture_Dynamic)));
		_ClearImage;
		apk.Init(im^.Size.XY, grow_Fast);
		apk.Border := BorderPx;
		syms.Init;
		sympairs.Init;
		ft := TrueType.Invalid;
		ft2 := TrueType.Invalid;
		denyKerningPairs := no;
		opts := RasterizationOptions.Create;

		ts := MakeRef(new(pTokenizer, Init(s)));
		ok := Assigned(ts) and _ReadFromTokens(ts^) and ft.OK;
		Release(ts);
		if not ok then ConstructorFailed;
	end;

	destructor Font.Done;
	{$ifdef DebugFonts} var msg: string; {$endif}
	begin
	{$ifdef DebugFonts}
		msg := '';
		if blits > 0 then ContinueString(msg, Format('blit: {0} ({1}/{2})', [timetostring(blitsTime/blits), tostring(blitstime), blits]), EOL);
		if rotations > 0 then ContinueString(msg, Format('rotation: {0} ({1}/{2})', [timetostring(rotationsTime/rotations), tostring(rotationstime), rotations]), EOL);
		if msg <> '' then Info.Show(msg);
		im^.Save(StreamPath.FilenameNoExt(ft.SourceFilename) + '.png', 0);
	{$endif}
		ft2.Unload;
		ft.Unload;
		opts.Done;
		apk.Done;
		sympairs.Done;
		syms.Done;
		Release(im);
		inherited Done;
	end;

	function Font.GetSym(const sym: UTFchar; const resolution: float): pSymRec;
	begin
	trace_call('Font.GetSym');
		result := _GetSymRec(sym, resolution);
	leave_call
	end;

	function Font.GetKerning(const a, b: UTFchar; out kerning: float): boolean;
	var
		pi: pSymPairInfo;
	begin
		if denyKerningPairs then exit(no);
		pi := _GetSymPairInfo(a, b);
		result := Assigned(pi) and (pi^.kerning <> 0.0);
		if result then kerning := pi^.kerning;
	end;

	procedure Font.Prefetch(const text: string; const resolution: float);
	var
		sym: UTFchar;
		i: sint;
	begin
		i := 1;
		while UTF8.Next(text, i, sym) <> UTFInvalid do
			GetSym(sym, resolution);
	end;

	procedure ExternalTextUpdateRequest(ctl: pControl; param: pointer);
	begin
		unused_args ctl end_list
		pText(param)^._changed := yes;
	end;

	procedure Text._AfterAttach;
	begin
		inherited _AfterAttach;
		parent^.AddResizeListener(@ExternalTextUpdateRequest, @self);
	end;

	procedure Text._BeforeDetach;
	begin
		parent^.RemoveResizeListener(@ExternalTextUpdateRequest, @self);
		inherited _BeforeDetach;
	end;

	procedure Text._RelocateNativeGLValue(old, new: pNativeGLValue);
	begin
		inherited _RelocateNativeGLValue(old, new);
		if old = p_image then p_image := new;
	end;

	function Text._ShaderRect: boolean;
	begin
		result := no;
	end;

	constructor Text.Init;
	var
		m: pMesh;
	begin
		p_image := nil;
		inherited Init;
		_font := nil;
		p_image := gl.values.Value('image', GLType.Sampler, 1, [NativeGLValueFlag.NonSerializable]);
		m := new(pMesh, Init{$ifdef Debug}('text'){$endif});
		_batch := m^.AddBatch('');
		_batch^.AddVA('pos2d', GLType.Vec2);
		_batch^.AddVA('tex2d', GLType.Vec2);

		va_pos := _batch^.FindVA('pos2d');
		va_tex := _batch^.FindVA('tex2d');
		_glMesh.Init(m, yes); _glMesh.MakeStatic;
		if MMSystem.gl.PrimitiveRestartSupported then _glMesh.topology := GLtopology_TriStrip;
		_text := '';
		_scale := 0.25; _UpdateScale;
		_changed := no;
		_notifyLocaleChanges := no;
		_estimatedResolution := 0.0;
		_maxWidth := 0.0;
		_paraIdent := 0.0;
	end;

	procedure _Text_OnLocaleChange(var loc: Locale; const info: SingleDelegateInfo);
	var
		text: pText absolute info.user;
	begin
		Assert(Locale.OnChangeProc(@_Text_OnLocaleChange) = @_Text_OnLocaleChange);
		Assert(@loc = @loc);
		text^._changed := yes;
	end;

	destructor Text.Done;
	begin
		if Assigned(_font) then _font^._RemoveListener(@self);
		if _notifyLocaleChanges then mm.locale.onChange.Remove(@_Text_OnLocaleChange, @self);
		Release(_font);
		_glMesh.Done;
		inherited Done;
	end;

	procedure Text._Redraw;
		procedure checksx(var sz: Vec2; const curX: float);
		begin
			sz.x := max(sz.x, curX);
		end;
		procedure checksy(var sz: Vec2; const curY: float; r: Font.pSymRec);
		begin
			sz.y := max(sz.y, -curY + r^.ra.sizes.y - r^.ra.imShift.y);
		end;
	var
		txt: string;
		i, sp, nextsp, len, vc, ic, sv, strsid, strstart: sint;
		curX, curY, x2, szLimit: float;
		curSym, nextSym: UTFchar;
		r: Font.pSymRec;
		sz: Vec2;
		inds: pMeshIndices;
		resolution, kerning: float;
		fb: array of record
			sp, vc: sint;
			sym: UTFchar;
			curX, curY: float;
			sz: Vec2;
		end;
		hyph: (NoHyphen, HyphenSym, HyphenEol);
	begin
	trace_call('Text._Redraw');
		if not Assigned(font) then exit;

		resolution := _EstimateResolution;
		_changed := no;
		if _notifyLocaleChanges then txt := mm.locale.Localize(_text) else txt := _text;

		len := UTF8.Codepoints(txt);
		_batch^.VerticesCount := 4 * len;
		sp := 1;
		curX := _paraIdent;
		curY := 0.0;
		vc := 0;
		sz := Vec2.Zero;
		strsid := 0;
		strstart := 1;
		case _posBase[0] of
			pos_A, pos_B: szLimit := parent^.ScreenSizes.x - Position.x;
			pos_Parent01: szLimit := parent^.ScreenSizes.x;
			else Assert(no);
		end;
		if _maxWidth <> 0.0 then szLimit := min(szLimit, maxWidth);
		szLimit /= max(_scale, CloseToZeroEps);
		SetLength(fb, len);
		hyph := NoHyphen;
		nextsp := 0;

		while (hyph in [HyphenSym, HyphenEol]) or (UTF8.Next(txt, sp, curSym) <> UTFInvalid) do
		begin
			if (curSym = ord(EOL)) or (hyph = HyphenEol) then
			begin
				checksx(sz, curX);
				if hyph <> NoHyphen then
				begin
					curX := 0.0;
					sp := nextsp;
				end else
					curX := _paraIdent;
				curY := curY - font^.ft.LineDistance;
				strsid := 0;
				strstart := sp;
				hyph := NoHyphen;
			end else
			begin
				r := font^.GetSym(curSym, resolution);
				if not Assigned(r) then
				begin
				{$ifdef Debug}
					Log('Символ не найден ("' + UTF8.CodepointToString(curSym) + '", UTF-32: #' + ToString(curSym) + ') (исходный текст: ' + txt + ')', logWarning);
				{$endif}
					if hyph = HyphenSym then hyph := HyphenEol;
					continue;
				end;
				if _changed then
				begin
					_Redraw;
					exit;
				end;

				if (curSym <> ord(TabSym)) and (curSym <> ord(' ')) then
				begin
					if strsid = 0 then
					begin
						if r^.ra.imShift.x < 0.0 then curX -= r^.ra.imShift.x;
						// if r^.ra.imShift.y < 0.0 then curY -= r^.ra.imShift.y;
					end;

					sv := vc;
					inc(vc, 4);
					if vc > _batch^.VerticesCount then _batch^.VerticesCount := 2 * vc;
					va_pos^.RawVec2[sv] := r^.ra.imShift + Vec2.Make(curX, curY);
					va_tex^.RawVec2[sv] := r^.texA;
					va_pos^.RawVec2[sv + 1] := r^.ra.imShift + Vec2.Make(curX, curY - r^.ra.sizes.y);
					va_tex^.RawVec2[sv + 1] := r^.texB;
					x2 := curX + r^.ra.sizes.x;
					va_pos^.RawVec2[sv + 2] := r^.ra.imShift + Vec2.Make(x2, curY);
					va_tex^.RawVec2[sv + 2] := r^.texD;
					va_pos^.RawVec2[sv + 3] := r^.ra.imShift + Vec2.Make(x2, curY - r^.ra.sizes.y);
					va_tex^.RawVec2[sv + 3] := r^.texC;
					nextSym := UTF8.Peek(txt, sp);
					if (nextSym <> UTFInvalid) and _font^.GetKerning(curSym, nextSym, kerning) then
						curX += kerning;
					checksy(sz, curY, r);
				end;
				curX += r^.ra.cursorShift;
				if GreaterThanEqual(curX, szLimit) then
				begin
					hyph := NoHyphen;
					for i := strsid - 1 downto 0 do
						if Hyphen(txt, strstart, fb[i].sp, nextsp, curSym) then
						begin
							if curSym <> UTFInvalid then hyph := HyphenSym else hyph := HyphenEol;
							sp := fb[i].sp;
							vc := fb[i].vc;
							curX := fb[i].curX;
							curY := fb[i].curY;
							sz := fb[i].sz;
							strsid := i;
							break;
						end;
					if hyph = NoHyphen then hyph := HyphenEol else continue;
					nextsp := sp;
				end;
				if hyph <> NoHyphen then hyph := HyphenEol else
				begin
					fb[strsid].sp := sp;
					fb[strsid].vc := vc;
					fb[strsid].sym := curSym;
					fb[strsid].curX := curX;
					fb[strsid].curY := curY;
					fb[strsid].sz := sz;
					inc(strsid);
				end;
			end;
		end;

		checksx(sz, curX);
		for i := 0 to vc - 1 do
			va_pos^.RawVec2[i][1] += sz.y;
		sz.y := sz.y + font^.ft.LineDistance;

		Sizes := sz * _scale;
		_batch^.VerticesCount := vc;
		inds := @_batch^.inds;

		vc := 0;
		ic := 0;
		if MMSystem.gl.PrimitiveRestartSupported then
		begin
			inds^.count := 5 * (_batch^.VerticesCount div 4);
			for i := 0 to _batch^.VerticesCount div 4 - 1 do
			begin
				inds^[ic] := vc;
				inds^[ic + 1] := vc + 1;
				inds^[ic + 2] := vc + 2;
				inds^[ic + 3] := vc + 3;
				inds^[ic + 4] := MeshIndices.RestartIndex;
				inc(vc, 4);
				inc(ic, 5);
			end;
			if inds^.Count > 0 then inds^.Count := inds^.Count - 1;
		end else
		begin
			inds^.count := 6 * (_batch^.VerticesCount div 4);
			for i := 0 to _batch^.VerticesCount div 4 - 1 do
			begin
				inds^[ic] := vc;
				inds^[ic + 1] := vc + 1;
				inds^[ic + 2] := vc + 2;
				inds^[ic + 3] := vc + 1;
				inds^[ic + 4] := vc + 3;
				inds^[ic + 5] := vc + 2;
				inc(vc, 4);
				inc(ic, 6);
			end;
		end;
		_glMesh.Changed;
		_ldText := _text;
		_estimatedResolution := resolution;
	leave_call
	end;

	function Text._EstimateResolution: float;
	begin
		result := _scale * min(mm.window.sizeX, mm.window.sizeY);
	end;

	procedure Text._SetScale(const newScale: float);
	begin
		if not Equals(_scale, newScale) then
		begin
			_scale := newScale;
			_UpdateScale;
		end;
	end;

	procedure Text._UpdateScale;
	begin
		p_GUIsizes^.SetVec2(Vec2.Make(_scale));
		_changed := yes;
	end;

	procedure Text._SetFont(newFont: pFont);
	begin
		if _font <> newFont then
		begin
			if Assigned(_font) then _font^._RemoveListener(@self);
			SetRef(_font, newFont);
			_UpdateFont;
		end;
	end;

	procedure Text._UpdateFont;
	begin
		if Assigned(_font) then
		begin
			p_image^.SetTex(_font^.im);
			_font^._AddListener(@self);
		end;
		_changed := yes;
	end;

	procedure Text._SetText(const newText: string);
	begin
		if _text <> newText then
		begin
			_text := newText;
			_UpdateText;
		end;
	end;

	procedure Text._UpdateText;
	var
		notify: boolean;
	begin
		notify := Locale.LocalizationRequired(_text);
		if notify <> _notifyLocaleChanges then
		begin
			if notify then mm.locale.onChange.Add(@_Text_OnLocaleChange, @self) else mm.locale.onChange.Remove(@_Text_OnLocaleChange, @self);
			_notifyLocaleChanges := notify;
		end;

		if _text <> _ldText then
			_changed := yes
		else
			_text := _ldText;
	end;

	procedure Text._SetMaxWidth(const newMax: float);
	begin
		if not Equals(_maxWidth, newMax) then
		begin
			_maxWidth := newMax;
			_changed := yes;
		end;
	end;

	procedure Text._SetParaIdent(const newIdent: float);
	begin
		if not Equals(_paraIdent, newIdent) then
		begin
			_paraIdent := newIdent;
			_changed := yes;
		end;
	end;

	procedure Text.Draw(RT: pGLRenderTarget);
	var
		rp: tParams4Renderable;
	begin
		if _changed or (_estimatedResolution <> _EstimateResolution) then _Redraw;
		if _text <> '' then
		begin
			_UpdateTransform;
			rp.Reset;
			rp.rt := RT;
			rp.roParams := @gl;
			rp.useMatBlend := yes;
			_glMesh.Draw(rp, _material^.AnyLevel);
			rp.Finalize;
		end;
		inherited Draw(rt);
	end;

	constructor GUIWindow.Init;
	begin
		inherited Init;
		_draggable := no;
		_sizeable := no;
		_skipOnClick := no;
		_minSizes := Vec2.Make(0.05, 0.05);
		_maxSizes := Vec2.Make(10.0, 10.0);
		_action := action_None;
	end;

	destructor GUIWindow.Done;
	begin
		inherited Done;
	end;

	function GUIWindow._FixupPosition(const p: Vec2): Vec2;
	begin
		if Assigned(parent) then
			result := Clamp(p, Vec2.Zero, parent^._sizes - _sizes)
		else
			result := p;
	end;

	procedure GUIWindow._AfterAttach;
	var
		ap: boolean;
	begin
		ap := gui_AutoPos in _flags;
		inherited _AfterAttach;
		if ap and not (gui_AutoPos in _flags) then Position := _FixupPosition(Position);
	end;

	function GUIWindow._HandleMouse(const gpos, delta: Vec2; event: MouseEvent; justTry: boolean): MouseEventFlags;
	var
		np: Vec2;
		i: sint;
	begin
		result := [];
		case event of
			gui_MouseDown:
				begin
					if _sizeable and Rect.Make(ScreenPos + 0.9 * _sizes, ScreenPos + _sizes).Contains(gpos) then
					begin
						result += [gui_MouseProceed, gui_BringToFront];
						if not justTry then _action := action_Sizing;
					end else
						if _draggable then
						begin
							result += [gui_MouseProceed, gui_BringToFront];
							if not justTry then _action := action_Dragging;
						end else
							if pGUIWindow(_gui) <> @self then
							begin
								result += [gui_MouseProceed];
								if not _skipOnClick then result += [gui_BringToFront];
								if not justTry then
									if _skipOnClick then pGUI(_gui)^.SkipEffects;
							end;
				end;
			gui_MouseDragging:
				begin
					result += [gui_MouseProceed];
					if not justTry then
						case _action of
							action_Dragging:
								begin
									for i := 0 to High(_posBase) do
										case _posBase[i] of
											pos_A: np.data[i] := Position.data[i] + delta.data[i];
											pos_B: np.data[i] := Position.data[i] - delta.data[i];
											else Assert(no, 'dragging elements with non-trivial positions unimplemented');
										end;
									Position := _FixupPosition(np);
								end;
							action_Sizing: Sizes := Clamp(_sizes + delta, _minSizes, Min(parent^.Sizes - Position, _maxSizes));
						end;
				end;
			gui_MouseUp:
				begin
					result += [gui_MouseProceed];
					if not justTry then _action := action_None;
				end;
		end;
		result += inherited _HandleMouse(gpos, delta, event, justTry);
		if (event in [gui_MouseEnter]) and not (gui_MouseProceed in result) then result += [gui_MouseProceed, gui_Simulacra];
	end;

	function GUIWindow._WhatToRemember: tMemoryParts;
	begin
		result := inherited _WhatToRemember;
		if _sizeable then result += [gui_RememberSize];
	end;

	function GUIWindow._ClipChildrens: boolean;
	begin
		result := yes;
	end;

	procedure Button._Press;
	begin
		if not _pressed then
		begin
			_pressed := yes;
			_UpdateButtonState;
		end;
	end;

	procedure _CallOnClick(const info: SingleDelegateInfo; param: pointer);
	var
		button: pButton absolute param;
	begin
		button^.OnClickProc(info.proc)(button, info);
	end;

	procedure Button._Release(silent: boolean);
	begin
		if _pressed then
		begin
			_pressed := no;
			_UpdateButtonState;
			if (not silent) and (not onClick.Empty) then
			begin
				onClick.Call(@_CallOnClick, @self);
			end;
		end;
	end;

	procedure Button._UpdateButtonState;
	begin
		SwitchToAnim(_pressedAnims[_pressed and (not onClick.Empty)]);
	end;

	function Button._HandleMouse(const gpos, delta: Vec2; event: MouseEvent; justTry: boolean): MouseEventFlags;
	begin
		result := [];
		case event of
			gui_MouseDown:
				begin
					result += [gui_MouseProceed, gui_BringToFront, gui_AllowAbandonDragging];
					if not justTry then _Press;
				end;
			gui_MouseUp:
				begin
					result += [gui_MouseProceed];
					if not justTry then _Release(no);
				end;
			gui_MouseAbandonDragging:
				begin
					result += [gui_MouseProceed];
					if not justTry then _Release(yes);
				end;
		end;
		result += inherited _HandleMouse(gpos, delta, event, justTry);
	end;

	constructor Button.Init(baseAnim, pressedAnim: pAtlasAnimation);
	begin
		inherited Init;
		_pressedAnims[no] := MakeRef(baseAnim);
		_pressedAnims[yes] := MakeRef(pressedAnim);
		_pressed := no;
		_UpdateButtonState;
		onClick.Init;
	end;

	destructor Button.Done;
	var
		b: boolean;
	begin
		onClick.Done;
		for b in boolean do
			Release(_pressedAnims[b]);
		inherited Done;
	end;

	constructor IndicatorGroup.Binding.Init(group: pIndicatorGroup; indicatorId: sint; onUnbind: OnUnbindProc; onUnbindParam: pObject);
	begin
		inherited Init;
		self.group         := group;
		self.indicatorId   := indicatorId;
		self.onUnbind      := onUnbind;
		self.onUnbindParam := onUnbindParam;
	end;

	destructor IndicatorGroup.Binding.Done;
	var
		i: sint;
	begin
		if Assigned(group) then
		begin
			i := Index(pointer(@self), pPointer(group^._bindings), length(group^._bindings));
			if i >= 0 then
			begin
				group^._bindings[i] := group^._bindings[High(group^._bindings)];
				SetLength(group^._bindings, length(group^._bindings) - 1);
			end;
		end;
		inherited Done;
	end;

	procedure IndicatorGroup.Binding.ChangeValue(const newValue: float);
	begin
		Assert(Assigned(group));
		group^._ChangeValue(indicatorId, newValue);
	end;

	constructor IndicatorGroup.IndicatorData.Init(const newName: PoolString; const newAnims: array of pAtlasAnimation; newReversed: boolean);
	var
		i: sint;
	begin
		Assert(length(newAnims) > 0);
		namae := newName;

		SetLength(anims, length(newAnims));
		for i := 0 to High(newAnims) do
			anims[i] := MakeRef(newAnims[i]);
		reversed := newReversed;
	end;

	destructor IndicatorGroup.IndicatorData.Done;
	begin
		ReleaseArray(USystem.ObjectsList(anims));
	end;

	procedure CallOnCreateIndicatorGroupControl(const info: SingleDelegateInfo; param: pointer);
	var
		args: IndicatorGroup.pOnCreateControlArgs absolute param;
	begin
		IndicatorGroup.OnCreateControlProc(info.proc)(args^.group, args^.id, args^.ctl, info);
	end;

	constructor IndicatorGroup.ActiveIndicator.Init(group: pIndicatorGroup; newId: sint; const newValue: float);
	var
		args: OnCreateControlArgs;
	begin
		id := newId;
		ctl := MakeRef(new(pImage, Init));
		ctl^._flags += [gui_NonSerializable];
		ctl^.PositionBase := group^.PositionBase;

		if not group^.onCreateControl.Empty then
		begin
			args.group := group;
			args.id := group^._ids[id].namae;
			args.ctl := ctl;
			group^.onCreateControl.Call(@CallOnCreateIndicatorGroupControl, @args);
		end;

		group^.Attach(ctl);
		dm := nil;
		fade := -1.0;
		SetFade(0.0);
		fadeStartTimeout := 0.25;
		ignoreNextMove := yes;
		ChangeValue(group, newValue);
	end;

	destructor IndicatorGroup.ActiveIndicator.Done;
	begin
		ctl^.Detach;
		if Assigned(dm) then
		begin
			dm^.Done;
			dispose(dm);
		end;
		Release(ctl);
	end;

	procedure IndicatorGroup.ActiveIndicator.ChangeValue(group: pIndicatorGroup; const newValue: float);
	var
		ind: pIndicatorData;
		nd: sint;
	begin
		ind := @group^._ids[id];
		nd := trunc(newValue * length(ind^.anims));
		if nd > High(ind^.anims) then
		begin
			nd := High(ind^.anims);
			Assert(nd >= 0);
		end;

		if nd <> discrete then
		begin
			discrete := nd;
			ctl^.SwitchToAnim(ind^.anims[discrete]);
		end;
	end;

	function IndicatorGroup.ActiveIndicator.Update(const dt: float): boolean;
	begin
		if Assigned(dm) then
		begin
			dm^.Process(dt);
			ctl^.Position := start * (1.0 - dm^.CurrentF) + finish * dm^.CurrentF;
			if dm^.Finished then begin dm^.Done; dispose(dm); dm := nil; end;
		end;

		if stillActive then
		begin
			if fadeStartTimeout = 0.0 then
			begin
				if fade < 1.0 then
					SetFade(min(fade + 4.0 * dt, 1.0));
			end else
				fadeStartTimeout := max(fadeStartTimeout - dt, 0.0);
			result := yes;
		end else
		begin
			SetFade(max(fade - 1.0 * dt, 0.0));
			result := fade > 0.0;
		end;
	end;

	procedure IndicatorGroup.ActiveIndicator.Move(const target: Vec2);
	begin
		if ignoreNextMove then
		begin
			ctl^.Position := target;
			ignoreNextMove := no;
			exit;
		end;
		if Assigned(dm) and (dm^.CurrentF < 0.5) then
		begin
			finish := target;
			exit;
		end else
			if target = ctl^.Position then exit;

		start := ctl^.Position;
		finish := target;

		if Assigned(dm) then dm^.Done else new(dm);
		dm^.Init(1);
		dm^.path^.AddAUA([0.0], [1.0], [0.0], [0.0], 0.0, 0.25, 0.0, 0.0);
	end;

	procedure IndicatorGroup.ActiveIndicator.SetFade(const newFade: float);
	begin
		if fade <> newFade then
		begin
			fade := newFade;
			ctl^.gl.values.Value('indicator_fade', GLType.Float, 1, [NativeGLValueFlag.NonSerializable])^.SetFloat(newFade);
		end;
	end;

	function IndicatorGroup._FindActive(id: sint): pActiveIndicator;
	var
		i: sint;
	begin
		for i := 0 to High(_active) do
			if _active[i].id = id then
				exit(@_active[i]);

		result := nil;
	end;

	procedure IndicatorGroup._ChangeValue(id: sint; const newValue: float);
	var
		ac: pActiveIndicator;
		na: boolean;
		i, at: sint;
	begin
		ac := _FindActive(id);
		if _ids[id].reversed then
			na := newValue > 0.0
		else
			na := newValue < 1.0;

		if (Assigned(ac) and ac^.stillActive) <> na then
		begin
			if not Assigned(ac) then
			begin
				at := length(_active);
				for i := 0 to High(_active) do
					if _active[i].id > id then
					begin
						at := i;
						break;
					end;

				SetLength(_active, length(_active) + 1);
				for i := High(_active) downto at + 1 do
					_active[i] := _active[i - 1];
				ac := @_active[at];
				ac^.Init(@self, id, newValue);
				_RecalculateActives;
			end;
			ac^.stillActive := na;
		end;

		if Assigned(ac) then
			ac^.ChangeValue(@self, newValue);
	end;

	procedure IndicatorGroup._RecalculateActives;
	var
		i: sint;
		cp, ns: Vec2;
		rps: array of Vec2;
	begin
		cp := Vec2.Zero;
		ns := Vec2.Zero;

		SetLength(rps, length(_active));
		for i := 0 to High(_active) do
		begin
			rps[i] := cp;
			cp.y := cp.y + _active[i].ctl^.sizes.y;
			ns.x := max(ns.x, _active[i].ctl^.sizes.x);
			ns.y := max(max(ns.y, cp.y), _active[i].ctl^.Position.y + _active[i].ctl^.Sizes.y); // TODO: запилить автоматический расчёт размеров
		end;
		Sizes := ns;

		if _posBase[1] = pos_B then
			for i := 0 to High(rps) do
				rps[i].y := cp.y - rps[i].y - _active[i].ctl^.sizes.y;

		for i := 0 to High(_active) do
			_active[i].Move(rps[i]);
	end;

	procedure IndicatorGroup._Update(const dt: float);
	var
		i, j: sint;
	begin
		for i := High(_active) downto 0 do
			if not _active[i].Update(dt) then
			begin
				_active[i].Done;
				for j := i to High(_active) - 1 do
					_active[j] := _active[j + 1];
				SetLength(_active, length(_active) - 1);
				_RecalculateActives;
			end;
		inherited _Update(dt);
	end;

	function IndicatorGroup._GetID(const name: PoolString): sint;
	begin
		result := Index(name.ToIndex, first_field _ids _ namae _, length(_ids), sizeof(_ids[0]));
	{$ifdef Debug} if result < 0 then Log('Индикатора "' + name + '" не существует', logError); {$endif}
	end;

	constructor IndicatorGroup.Init;
	begin
		inherited Init;
		_ids := nil;
		_active := nil;
		_bindings := nil;
		onCreateControl.Init;
	end;

	destructor IndicatorGroup.Done;
	var
		i: sint;
	begin
		onCreateControl.Done;
		for i := High(_bindings) downto 0 do
			_Unbind(_bindings[i]^.indicatorId);

		for i := 0 to High(_active) do
			_active[i].Done;

		for i := 0 to High(_ids) do
			_ids[i].Done;
		inherited Done;
	end;

	procedure IndicatorGroup.Add(const name: PoolString; const anims: array of pAtlasAnimation; reversed: boolean);
	begin
		Assert((length(_active) = 0) and (length(_bindings) = 0));
		SetLength(_ids, length(_ids) + 1);
		_ids[High(_ids)].Init(name, anims, reversed);
	end;

	function IndicatorGroup.Bind(const name: PoolString; onUnbind: OnUnbindProc; onUnbindParam: pObject): pBinding;
	var
		id: sint;
	begin
		id := _GetID(name);
		if id < 0 then exit(nil);
		_Unbind(id);

		result := new(pBinding, Init(@self, id, onUnbind, onUnbindParam));
		SetLength(_bindings, length(_bindings) + 1);
		_bindings[High(_bindings)] := result;
	end;

	procedure IndicatorGroup.Unbind(const name: PoolString);
	begin
		_Unbind(_GetID(name));
	end;

	procedure IndicatorGroup._Unbind(id: sint);
	var
		i: sint;
	begin
		for i := 0 to High(_bindings) do
			if _bindings[i]^.indicatorId = id then
			begin
				_bindings[i]^.group := nil;
				if Assigned(_bindings[i]^.onUnbind) then
					_bindings[i]^.onUnbind(_bindings[i], _bindings[i]^.onUnbindParam);
				_bindings[i] := _bindings[High(_bindings)];
				SetLength(_bindings, length(_bindings) - 1);
				exit;
			end;
	end;

	function MemoryItem.Dummy: MemoryItem;
	begin
		result.what := [];
	end;

	procedure Memory.Initialize;
	begin
		_mem.Init;
	end;

	procedure Memory.Finalize;
	begin
		_mem.Done;
	end;

	function Memory.Parts(const id: PoolString): tMemoryParts;
	var
		item: pMemoryItem;
	begin
		item := _mem.Find(id);
		if Assigned(item) then result := item^.what else result := [];
	end;

	procedure Memory.Forget(const id: PoolString);
	begin
		_mem.Remove(id);
	end;

	function Memory.Restore(const id: PoolString; ctl: pControl): boolean;
	var
		item: pMemoryItem;
	begin
		item := _mem.Find(id);
		result := Assigned(item);
		if not result then exit;

		if gui_RememberPos in item^.what then
		begin
			ctl^.PositionBase := item^.posBase;
			ctl^.Position := item^.pos;
		{$ifdef Debug} Log('Вспомнил по ключу "' + id + '" позицию: ' + ToString(item^.pos), logDebug); {$endif}
		end;
		if gui_RememberSize in item^.what then
		begin
		{$ifdef Debug} Log('Вспомнил по ключу "' + id + '" размеры: ' + ToString(item^.sizes), logDebug); {$endif}
			ctl^.SizeBase := item^.sizeBase;
			ctl^.Sizes := item^.sizes;
		end;
	end;

	procedure Memory.Remember(const id: PoolString; ctl: pControl; what: tMemoryParts);
	var
		item: pMemoryItem;
	begin
		item := _mem.Find(id);
		if not Assigned(item) then
			item := _mem.Add(id, MemoryItem.Dummy);
		item^.what := what;

		if gui_RememberPos in what then
		begin
			item^.posBase := ctl^.PositionBase;
			item^.pos := ctl^.Position;
		{$ifdef Debug} Log('Запомнил по ключу "' + id + '" позицию: ' + ToString(item^.pos), logDebug); {$endif}
		end;
		if gui_RememberSize in what then
		begin
			item^.sizeBase := ctl^.SizeBase;
			item^.sizes := ctl^.Sizes;
		{$ifdef Debug} Log('Запомнил по ключу "' + id + '" размеры: ' + ToString(item^.sizes), logDebug); {$endif}
		end;
	end;

	procedure _CallOnUpdateHint(const info: SingleDelegateInfo; param: pointer);
	var
		args: ^Hint.tOnUpdateArgs absolute param;
	begin
		Hint.tOnUpdateProc(info.proc)(args^.hint, args^.dt^, info);
	end;

	procedure Hint._Update(const dt: float);
	var
		args: tOnUpdateArgs;
	begin
		inherited _Update(dt);
		if not onUpdate.Empty then
		begin
			_updateTimeout += dt;
			if _updateTimeout >= UpdatePeriod then
			begin
				args.hint := @self;
				args.dt   := @_updateTimeout;
				onUpdate.Call(@_CallOnUpdateHint, @args);
				_updateTimeout := 0.0;
			end;
		end;
	end;

	constructor Hint.Init;
	begin
		inherited Init;
		_flags += [gui_NonSerializable];
		_updateTimeout := 0.0;
		onUpdate.Init;
	end;

	destructor Hint.Done;
	begin
		onUpdate.Done;
		inherited Done;
	end;

	constructor GUIRoot.CursorData.Init(newImage: pImage; const newSpot: Vec2);
	begin
		image := MakeRef(newImage);
		spot  := newSpot;
	end;

	destructor GUIRoot.CursorData.Done;
	begin
		Release(image);
	end;

	function GUIRoot.CursorData.ImgSize: Vec2;
	begin
		if Assigned(image) then result := image^.Sizes else result := Vec2.Zero;
	end;

	procedure GUIRoot.CursorData.Open(var gui: GUIRoot);
	begin
		if Assigned(image) and gui._mouseEnabled then
		begin
			gui.Attach(image);
			gui._UpdateMouse;
		end;
	end;

	procedure GUIRoot.CursorData.Close(var gui: GUIRoot);
	begin
		Assert(@gui = @gui);
		if Assigned(image) then image^.Detach;
	end;

	procedure GUIRoot._MoveMouseBy(const delta: Vec2);
	var
		cur: pCursorData;
	begin
		if delta = Vec2.Zero then exit;
		cur := _CurrentCursor;
		_mousePos := Clamp(_mousePos + delta, Vec2.Zero - cur^.imgSize, sizes);
		_UpdateMouse;
		if _mouseLPressed then
			_HandleDragging(_mousePos, delta)
		else
			_HandleMove(_mousePos, delta);
	end;

	procedure GUIRoot._UpdateMouse;
	var
		cur: pCursorData;
	begin
		cur := _CurrentCursor;
		if Assigned(cur^.image) then
			cur^.image^.Position := _mousePos - cur^.spot;
	end;

	procedure GUIRoot._HandleDragging(const gpos, delta: Vec2);
	var
		i: sint;
	begin
		for i := High(_mouseTracking) downto 0 do
			if _mouseTracking[i].ctl^.ClipRect.Contains(gpos) or not (gui_AllowAbandonDragging in _mouseTracking[i].flags) then
				_mouseTracking[i].ctl^._HandleMouse(gpos, delta, gui_MouseDragging)
			else
			begin
				_mouseTracking[i].ctl^._HandleMouse(gpos, delta, gui_MouseAbandonDragging);
				_mouseTracking[i] := _mouseTracking[High(_mouseTracking)];
				SetLength(_mouseTracking, length(_mouseTracking) - 1);
			end;
	end;

	procedure GUIRoot._HandleRelease(const gpos: Vec2; update: boolean);
	var
		i: sint;
	begin
		if not _mouseLPressed then exit;
		_mouseLPressed := no;
		for i := 0 to High(_mouseTracking) do
			_mouseTracking[i].ctl^._HandleMouse(gpos, Vec2.Zero, gui_MouseUp);
		_mouseTracking := nil;
		if update then _HandleMove(gpos, Vec2.Zero);
	end;

	procedure GUIRoot._InstantCloseTrackingIfAny(ctl: pControl);
	var
		i: sint;
	begin
		for i := 0 to High(_mouseTracking) do
			if _mouseTracking[i].ctl = ctl then
			begin
				if _mouseLPressed then
					if gui_AllowAbandonDragging in _mouseTracking[i].flags then
						ctl^._HandleMouse(_mousePos, Vec2.Zero, gui_MouseAbandonDragging)
					else
						ctl^._HandleMouse(_mousePos, Vec2.Zero, gui_MouseUp)
				else
					ctl^._HandleMouse(_mousePos, Vec2.Zero, gui_MouseLeave);

				_mouseTracking[i] := _mouseTracking[High(_mouseTracking)];
				SetLength(_mouseTracking, length(_mouseTracking) - 1);
				break;
			end;
	end;

	procedure GUIRoot._OnSetSizes;
	begin
		inherited _OnSetSizes;
		if NotZero(sizes.x) and NotZero(sizes.y) then
			UInvGUISizes^.SetVec2(Vec2.Make(1.0 / sizes.x, 1.0 / sizes.y));
	end;

	procedure GUIRoot._RecalcSizes(RT: pGLRenderTarget);
	begin
		if RT^.size.Positive then
			if RT^.size.x > RT^.size.y then
				Sizes := Vec2.Make(RT^.size.x / RT^.size.y, 1.0)
			else
				Sizes := Vec2.Make(1.0, RT^.size.y / RT^.size.x);
	end;

	procedure GUIRoot.ChangeCursor(newImage: pImage; const newSpot: Vec2);
	begin
		if (_baseCursor.image <> newImage) or (_baseCursor.spot <> newSpot) then
		begin
			_CurrentCursor^.Close(self);
			_baseCursor.Done;
			_baseCursor.Init(newImage, newSpot);
			_CurrentCursor^.Open(self);
		end;
	end;

	function GUIRoot._FindFloat(img: pImage): sint;
	var
		i: sint;
	begin
		for i := 0 to High(_float) do
			if _float[i].cursor.image = img then
				exit(i);
		result := -1;
	end;

	function GUIRoot._CurrentCursor: pCursorData;
	begin
		if length(_float) > 0 then
			result := @_float[High(_float)].cursor
		else
			result := @_baseCursor;
	end;

	function GUIRoot._AutoPos(const sz: Vec2; ignore: pControl): Vec2;
	type
		RectsList = array of Rect;

		procedure traverse(ctl: pControl; var rects: RectsList);
		var
			i: sint;
		begin
			if ctl = ignore then exit;
			if InheritsFrom(TypeOf(ctl^), TypeOf(GUIWindow)) and pGUIWindow(ctl)^.Corporeal then
			begin
				SetLength(rects, length(rects) + 1);
				rects[High(rects)] := ctl^.clipRect;
				i := High(rects);
			end;
			for i := 0 to High(ctl^.childs) do
				traverse(ctl^.childs[i], rects);
		end;

		function aspdiff(const a, b: float): float;
		begin
			if a > b then result := a / max(b, CloseToZeroEps) else result := b / max(a, CloseToZeroEps);
		end;

	var
		rects: RectsList;
		e: EmptyRects.List;
		best: sint;
		curEstimation, bestEstimation, sz_square, sz_aspect: float;
		i: sint;
	begin
		rects := nil;
		traverse(@self, rects);
		sz_square := sz.x * sz.y;
		sz_aspect := sz.x / max(sz.y, CloseToZeroEps);

	{$ifdef DebugAutoPos}
		Log('Прямоугольнички:');
		for i := 0 to high(rects) do
			log(ToString(rects[i].a)+'; '+ToString(rects[i].b));
	{$endif}
		e := EmptyRects.Get(_clipRect, rects);
	{$ifdef DebugAutoPos}
		Log('Прямоугольнички пустого места:');
		for i := 0 to high(e) do
			log(ToString(i) + '. ' + ToString(e[i].a)+'; '+ToString(e[i].b));
	{$endif}
		best := -1;
		bestEstimation := 0.0;
		for i := 0 to High(e) do
		begin
			if GreaterThanEqual(e[i].SizeX, sz.X) and GreaterThanEqual(e[i].SizeY, sz.Y) then
				curEstimation := 0.5 * sz_square / max(e[i].Square, CloseToZeroEps)
			else
				curEstimation := max(sz_square / max(e[i].Square, CloseToZeroEps), 1) * aspdiff(e[i].Aspect, sz_aspect);
			if (best < 0) or (curEstimation < bestEstimation) then
			begin
				best := i;
				bestEstimation := curEstimation;
			end;
		end;
		if best >= 0 then
		begin
			result := e[best].A + 0.5 * (e[best].Size - sz);
		{$ifdef DebugAutoPos} Log('Выбран ' + ToString(best) + '-й', logDebug); {$endif}
		end else
		begin
			result := clipRect.A + 0.5 * sz + Vec2.Make(GlobalRNG.GetFloat, GlobalRNG.GetFloat) * (ScreenSizes - 0.5 * sz);
		{$ifdef DebugAutoPos} Log('Выбран случайно', logDebug); {$endif}
		end;
	end;

	function GUIRoot._AllowMouse(ctl: pControl): boolean;
	begin
		result := (ctl <> pControl(_CurrentCursor^.image));
	end;

	procedure GUIRoot._ChangeHint(newHint: pHint);
	begin
		if _hint = newHint then exit;
		if Assigned(_hint) then _hint^.PrettyDetach;
		_hint := newHint;
		if Assigned(_hint) then
		begin
			Assert(_hint^.parent = pControl(@self));
			_UpdateHint(_mousePos);
		end;
	end;

	procedure GUIRoot._UpdateHint(const mpos: Vec2);
	const
		Shift: Vec2 = (data: (0.05, 0.02));
	begin
		if not Assigned(_hint) then exit;
		_hint^.position := mpos + Shift;
	end;

	procedure GUIRoot.StartFloat(newImage: pImage; const newSpot: Vec2; newMoveProc: FloatMoveCallback; newDoneProc: FloatDoneCallback;
		newParam: pointer);
	begin
		_CurrentCursor^.Close(self);
		SetLength(_float, length(_float) + 1);
		with _float[High(_float)] do
		begin
			cursor.Init(newImage, newSpot);
			moveProc := newMoveProc;
			doneProc := newDoneProc;
			userParam := newParam;
		end;
		_CurrentCursor^.Open(self);

		if Assigned(newMoveProc) then
			newMoveProc(_mousePos, _GetLastRespondedToMouseEvent(_mousePos, Vec2.Zero, gui_MouseEnter), newParam);
	end;

	procedure GUIRoot.StopFloat(img: pImage; at: pControl; mode: FloatStopMode);
	var
		i, id: sint;
		keep: boolean;
	begin
		id := _FindFloat(img);
		Assert(id >= 0);

		with _float[id] do
		begin
			if Assigned(doneProc) then
			begin
				keep := (not doneProc(_mousePos, at, mode, userParam)) and (mode = float_JustAsPlanned);
				if keep then exit;
			end;
		end;
		_CurrentCursor^.Close(self);
		_float[id].cursor.Done;
		for i := id to High(_float) - 1 do
			_float[i] := _float[i + 1];
		SetLength(_float, length(_float) - 1);
		_CurrentCursor^.Open(self);
	end;

	procedure GUIRoot.AbandonFloat(img: pImage);
	begin
		StopFloat(img, nil, float_Abandon);
	end;

	procedure GUIRoot.AbandonAnyFloat;
	var
		i: sint;
	begin
		for i := High(_float) downto 0 do
			AbandonFloat(_float[i].cursor.image);
	end;

	function GUIRoot.Floating: pImage;
	begin
		if length(_float) > 0 then
			result := _float[High(_float)].cursor.image
		else
			result := nil;
	end;

	procedure _CallOnAttachDetach(const info: SingleDelegateInfo; param: pointer);
	var
		args: ^GUIRoot.OnAttachDetachArgs absolute param;
	begin
		with args^ do GUIRoot.OnAttachDetachProc(info.proc)(ctl, attach, info);
	end;

	procedure GUIRoot.NotifyAtDt(ctl: pControl; attached: boolean);
	var
		args: OnAttachDetachArgs;
	begin
		if not onAttachDetach.Empty then
		begin
			args.ctl := ctl;
			args.attach := attached;
			onAttachDetach.Call(@_CallOnAttachDetach, @args);
		end;
	end;

	procedure GUIRoot.SkipEffects;
	begin
		if _skip = DontSkip then _skip := SkipRequested;
	end;

	procedure GUIRoot.SkipEffectsBreakpoint;
	begin
		_skip := DontSkip;
	end;

	procedure GUIRoot._HandleMouseEnabled(newEnabled: boolean);
	var
		i: sint;
	begin
		if newEnabled then
		begin
			_CurrentCursor^.Open(self);
			if _mouseLPressed then
				_HandlePress(_mousePos)
			else
				_HandleMove(_mousePos, Vec2.Zero);
		end else
		begin
			AbandonAnyFloat;
			_ChangeHint(nil);
			_CurrentCursor^.Close(self);
			if _mouseLPressed then
				_HandleRelease(_mousePos, no)
			else
			begin
				for i := 0 to High(_mouseTracking) do
					_mouseTracking[i].ctl^._HandleMouse(_mousePos, Vec2.Zero, gui_MouseLeave);
				_mouseTracking := nil;
			end;
		end;
	end;

	procedure GUIRoot._SetMouseEnabled(newEnabled: boolean);
	begin
		if _mouseEnabled <> newEnabled then
		begin
			_mouseEnabled := newEnabled;
			_HandleMouseEnabled(_mouseEnabled);
		end;
	end;

	function GUIRoot._GetLastRespondedToMouseEvent(const gpos, delta: Vec2; ev: MouseEvent): pControl;
		procedure traverse(ctl: pControl);
		var
			i: sint;
		begin
			if ctl^.ClipRect.Contains(gpos) and _AllowMouse(ctl) then
			begin
				if gui_MouseProceed in ctl^._HandleMouse(gpos, delta, ev, yes) then
					result := ctl;
				for i := 0 to High(ctl^.childs) do
					traverse(ctl^.childs[i]);
			end;
		end;
	begin
		result := nil;
		traverse(@self);
	end;

	procedure GUIRoot._HandleMove(const gpos, delta: Vec2);
	var
		ctl: pControl;
		i, id: sint;
		nh: pHint;
	begin
		_UpdateHint(gpos);
		ctl := _GetLastRespondedToMouseEvent(gpos, delta, gui_MouseEnter);
		id := -1;
		nh := nil;
		if Assigned(ctl) and (length(_float) = 0) then
		begin
			for i := 0 to High(_mouseTracking) do
				if _mouseTracking[i].ctl = ctl then
				begin
					id := i;
					break;
				end;

			if id < 0 then
			begin
				id := length(_mouseTracking);
				SetLength(_mouseTracking, id + 1);
				_mouseTracking[id].ctl := ctl;
				_mouseTracking[id].flags := ctl^._HandleMouse(gpos, Vec2.Zero, gui_MouseEnter);
			end else
				nh := _hint;
			if not Assigned(nh) then nh := ctl^._AttachHint(@self, ctl, gpos);
		end;
		_ChangeHint(nh);
		for i := 0 to High(_mouseTracking) do
			_mouseTracking[i].ctl^._HandleMouse(gpos, delta, gui_MouseOver);

		for i := High(_mouseTracking) downto 0 do
			if i <> id then
			begin
				_mouseTracking[i].ctl^._HandleMouse(gpos, Vec2.Zero, gui_MouseLeave);
				_mouseTracking[i] := _mouseTracking[High(_mouseTracking)];
				SetLength(_mouseTracking, length(_mouseTracking) - 1);
			end;

		for i := 0 to High(_float) do
			if Assigned(_float[i].moveProc) then
				_float[i].moveProc(gpos, ctl, _float[i].userParam);
	end;

	procedure GUIRoot._HandlePress(const gpos: Vec2);
	var
		i: sint;
		newFlags: MouseEventFlags;
		nctl: pControl;
	begin
		if _mouseLPressed then exit;
		_mouseLPressed := yes;
		for i := 0 to High(_mouseTracking) do
			_mouseTracking[i].ctl^._HandleMouse(gpos, Vec2.Zero, gui_MouseLeave);
		_mouseTracking := nil;
		_ChangeHint(nil);

		nctl := _GetLastRespondedToMouseEvent(gpos, Vec2.Zero, gui_MouseDown);
		if (not Assigned(nctl)) or (nctl = pControl(@self)) then SkipEffects;

		if length(_float) > 0 then
		begin
			if nctl = pControl(@self) then nctl := nil;
			for i := High(_float) downto 0 do
				StopFloat(_float[i].cursor.image, nctl, float_JustAsPlanned);
		end else
			if Assigned(nctl) then
			begin
				newFlags := nctl^._HandleMouse(gpos, Vec2.Zero, gui_MouseDown);
				if gui_BringToFront in newFlags then _BringToFront(nctl);
				if gui_Simulacra in newFlags then exit;

				SetLength(_mouseTracking, length(_mouseTracking) + 1);
				with _mouseTracking[High(_mouseTracking)] do
				begin
					ctl := nctl;
					flags := newFlags;
				end;
			end;
		_HandleDragging(gpos, Vec2.Zero);
	end;

	procedure GUIRoot._BringToFront(ctl: pControl);
		procedure traverse(control: pControl);
		var
			i: sint;
		begin
			if not Assigned(control^.parent) then exit;
			with control^.parent^ do
			begin
				for i := control^._parentId to High(childs) - 1 do
				begin
					childs[i] := childs[i + 1];
					childs[i]^._parentId := i;
				end;
				childs[High(childs)] := control;
				control^._parentId := High(childs);
			end;
			traverse(control^.parent);
		end;
	var
		cur: pCursorData;
	begin
		traverse(ctl);
		if _mouseEnabled then
		begin
			cur := _CurrentCursor;
			if Assigned(cur^.image) and (ctl <> pControl(cur^.image)) then
				_BringToFront(cur^.image);
		end;
	end;

	procedure _HandleMouseButton(button: MouseButton; ev: ButtonEvent; const info: SingleDelegateInfo);
	var
		gui: pGUI absolute info.user;
	begin
		Assert(MouseInput.OnButtonProc(@_HandleMouseButton) = @_HandleMouseButton);
		if not gui^.MouseEnabled then exit;
		case button of
			mouse_Left:
				begin
					case ev of
						button_Click: gui^._HandlePress(gui^._mousePos);
						button_Release: gui^._HandleRelease(gui^._mousePos, yes);
					end;
				end;
		end;
	end;

	procedure _HandleMouseOver(const delta: Vec2; const info: SingleDelegateInfo);
	var
		gui: pGUI absolute info.user;
	begin
		Assert(MouseInput.OnMoveProc(@_HandleMouseOver) = @_HandleMouseOver);
		if not gui^.MouseEnabled then exit;
		gui^._MoveMouseBy(delta);
	end;

	procedure _HandleGamepadButton(button: GamepadInput.Button; ev: ButtonEvent; const info: SingleDelegateInfo);
	var
		gui: pGUI absolute info.user;
	begin
		Assert(GamepadInput.OnButtonProc(@_HandleGamepadButton) = @_HandleGamepadButton);
		if not gui^.MouseEnabled then exit;
		case button of
			btn_Triangle, btn_LStick, btn_RStick:
				case ev of
					button_Click: gui^._HandlePress(gui^._mousePos);
					button_Release: gui^._HandleRelease(gui^._mousePos, yes);
				end;
		end;
	end;

	procedure _HandleGamepadStick(stick: GamepadInput.Stick; const state: Vec2; const info: SingleDelegateInfo);
	var
		gui: pGUI absolute info.user;
	begin
		Assert(GamepadInput.OnStickProcessProc(@_HandleGamepadStick) = @_HandleGamepadStick);
		Assert(@stick = @stick);
		if not gui^.MouseEnabled then exit;
		gui^._MoveMouseBy(mm.FrameDt * 0.5 * Vec2.Make(state.x, -state.y));
	end;

	constructor GUIRoot.Init(RT: pGLRenderTarget);
	begin
		inherited Init;
		_gui := @self;
		memory.Initialize;
		_baseCursor.Init(nil, Vec2.Zero);
		_mouseEnabled := no;
		_mouseTracking := nil;
		_float := nil;
		_neutralized := no;
		onAttachDetach.Init;
		_AfterAttach;
		_RecalcSizes(RT);
		_mousePos := 0.5 * _sizes;
		_skip := DontSkip;
		_lastUpdate := Ticks.Zero;
	end;

	destructor GUIRoot.Done;
	var
		i: sint;
	begin
		onAttachDetach.Clear;
		for i := High(_float) downto 0 do
			StopFloat(_float[i].cursor.image, nil, float_Emergency);
		_baseCursor.Done;
		if not _neutralized then _BeforeDetach;
		onAttachDetach.Done;
		inherited Done;
		memory.Finalize;
	end;

	procedure GUIRoot.Neutralize;
	begin
		Assert(not _neutralized);
		_BeforeDetach;
		_neutralized := yes;
	end;

	procedure GUIRoot._AfterAttach;
	var
		gb: GamepadInput.Button;
	begin
		inherited _AfterAttach;
		mm.mouse.buttonEvents[mouse_Left, button_Click].Add(@_HandleMouseButton, @self);
		mm.mouse.buttonEvents[mouse_Left, button_Release].Add(@_HandleMouseButton, @self);
		mm.mouse.onMove.Add(@_HandleMouseOver, @self);
		mm.gamepad.sticks[LeftStick].onProcess.Add(@_HandleGamepadStick, @self);
		mm.gamepad.sticks[RightStick].onProcess.Add(@_HandleGamepadStick, @self);
		for gb in [btn_Triangle, btn_LStick, btn_RStick] do
		begin
			mm.gamepad.buttons[gb].events[button_Click].Add(@_HandleGamepadButton, @self);
			mm.gamepad.buttons[gb].events[button_Release].Add(@_HandleGamepadButton, @self);
		end;
	end;

	procedure GUIRoot._BeforeDetach;
	var
		gb: GamepadInput.Button;
	begin
		mm.mouse.buttonEvents[mouse_Left, button_Click].Remove(@_HandleMouseButton, @self);
		mm.mouse.buttonEvents[mouse_Left, button_Release].Remove(@_HandleMouseButton, @self);
		mm.mouse.onMove.Remove(@_HandleMouseOver, @self);
		mm.gamepad.sticks[LeftStick].onProcess.Remove(@_HandleGamepadStick, @self);
		mm.gamepad.sticks[RightStick].onProcess.Remove(@_HandleGamepadStick, @self);
		for gb in [btn_Triangle, btn_LStick, btn_RStick] do
		begin
			mm.gamepad.buttons[gb].events[button_Click].Remove(@_HandleGamepadButton, @self);
			mm.gamepad.buttons[gb].events[button_Release].Remove(@_HandleGamepadButton, @self);
		end;
		inherited _BeforeDetach;
	end;

	procedure GUIRoot.Attach(ctrl: pControl);
	begin
		inherited Attach(ctrl);
		if Assigned(_CurrentCursor^.image) then _BringToFront(_CurrentCursor^.image);
	end;

	procedure GUIRoot.Update(RT: pGLRenderTarget; const dt: float);
	var
		ct: Ticks;
	begin
		ct := Ticks.Get;
		if (_skip = SkipRequested) and ((ct - _lastUpdate).ToSeconds > 1) then _skip := DontSkip;
		_lastUpdate := ct;
		_RecalcSizes(RT);
		_Update(dt);
		case _skip of
			SkipRequested: _skip := DontSkip;
			ConfirmedSkip: _skip := SkipRequested;
		end;
	end;

	procedure GUIRoot.Draw(RT: pGLRenderTarget);
	var
		rast: GLRasterizerState;
	begin
		MMSystem.gl.PushRasterizerState;
		rast.depthTest := no;
		rast.wire := [];
		MMSystem.gl.SetRasterizerState(rast, [GLrast_DepthTest, GLrast_Wire]);
		if RT^.Size.Positive then UInvDestRTSizes^.SetVec2(1.0 / RT^.Size);
		inherited Draw(RT);
		MMSystem.gl.PopRasterizerState;
	end;

	procedure __UnbindMinimapTarget(obj: pObject; param: pointer);
	var
		minimap: pMinimap absolute param;
	begin
		Assert(@obj = @obj);
		minimap^._UnbindTarget;
	end;

	procedure __UnbindMinimapScene(obj: pObject; param: pointer);
	var
		minimap: pMinimap absolute param;
	begin
		Assert(@obj = @obj);
		minimap^._UnbindScene;
	end;

	procedure Minimap._SetScene(newScene: pSceneNode);
	begin
		if _scene <> newScene then
		begin
			if Assigned(_scene) then
			begin
				_scene^.RemoveOnDestroyProc(@__UnbindMinimapScene, @self);
				_UnbindScene;
			end;
			_scene := newScene;
			if Assigned(_scene) then
			begin
				_scene^.AddOnDestroyProc(@__UnbindMinimapScene, @self);
				if Assigned(_gui) then _scene^.Attach(@_op);
			end;
		end;
	end;

	procedure Minimap._SetTarget(newTarget: pSceneNode);
	begin
		if _target <> newTarget then
		begin
			if Assigned(_target) then
			begin
				_target^.RemoveOnDestroyProc(@__UnbindMinimapTarget, @self);
				_UnbindTarget;
			end;
			_target := newTarget;
			if Assigned(_target) then
			begin
				_target^.AddOnDestroyProc(@__UnbindMinimapTarget, @self);
				_UpdateOpTransform;
			end;
		end;
	end;

	procedure Minimap._UnbindScene;
	begin
		_op.Detach;
		_scene := nil;
	end;

	procedure Minimap._UnbindTarget;
	begin
		_target := nil;
	end;

	procedure Minimap._UpdateOpTransform;
	begin
		_op.LocalTransform := Translate(_target^.WorldPos) * Rotate(_target^.WorldRot.GetAngleXZ, Vec3.PositiveY) * Translate(_shift);
		_op.camera.tTarget := _target^.WorldPos;
	end;

	procedure Minimap._UpdateOpSizes;
	var
		i: sint;
	begin
		for i := 0 to 1 do
			if NotZero(pGUI(_gui)^.sizes.data[i]) then
				_op.SetSize(i, sizes.data[i] / pGUI(_gui)^.sizes.data[i]);
		if NotZero(sizes.y) then _op.camera.forceAspect := sizes.x / sizes.y;
	end;

	procedure MinimapOnGuiResize(ctl: pControl; param: pointer);
	begin
		unused_args ctl end_list
		pMinimap(param)^._UpdateOpSizes;
	end;

	procedure Minimap._AfterAttach;
	begin
		inherited _AfterAttach;
		if Assigned(_scene) then _scene^.Attach(@_op);
		_UpdateOpSizes;
		pGUI(_gui)^.AddResizeListener(@MinimapOnGuiResize, @self);
	end;

	procedure Minimap._BeforeDetach;
	begin
		pGUI(_gui)^.RemoveResizeListener(@MinimapOnGuiResize, @self);
		_op.Detach;
		inherited _BeforeDetach;
	end;

	procedure Minimap._Update(const dt: float);
	begin
		inherited _Update(dt);
		if Assigned(_target) then
			_UpdateOpTransform;
	end;

	procedure Minimap._OnSetSizes;
	begin
		inherited;
		if Assigned(_gui) then _UpdateOpSizes;
	end;

	procedure Minimap._Initialize;
	var
		ns, nt: pSceneNode;
	begin
		_op.Init('minimap', @MinimapScenario, GLtexture_2D, [GLformat_RGBA], [0.1], [rtop_WindowSize],
			[rtop_Endless, rtop_ExternalCameraControl, rtop_Precise]);
		_op.MakeStatic;
		_op.Serializable := no;
		_op.Quality := 0.2;
		_op.Bind(@self, @self.gl, 'minimap');
		_op.BaseRefreshPeriod := 1.0 / 18.0;
		_op.camera.Cinematic := yes;
		_op.camera.AccK := 6.0;
		_op.GetRT^.ClearColor := Vec4.Make(_baseColor, _baseAlpha);

		ns     := _scene;  nt      := _target;
		_scene := nil;     _target := nil;
		Scene  := ns;      Target  := nt;
	end;

	constructor Minimap.Init(const newBaseColor: Vec3; const newBaseAlpha: float);
	begin
		inherited Init;
		Include(_flags, gui_ExternalImageSoItsOkayToDrawWithoutExplicitlyChosen);
		Draggable := yes;
		_scene := nil;
		_target := nil;
		_shift := Vec3.Make(10.0, 10.0, 10.0);
		_baseColor := newBaseColor;
		_baseAlpha := newBaseAlpha;
		_Initialize;
	end;

	constructor Minimap.DeseInit;
	begin
		inherited Init;
	end;

	destructor Minimap.Done;
	begin
		_SetTarget(nil);
		_SetScene(nil);
		_op.Done;
		inherited Done;
	end;

	function InventoryItem._HandleMouse(const gpos, delta: Vec2; event: MouseEvent; justTry: boolean): MouseEventFlags;
	var
		local: Vec2;
		cell: UintVec2;
	begin
		result := [];
		local := gpos - ScreenPos;
		if (local.x < 0) or (local.y < 0) then exit;
		cell := UintTrunc(local / _sizes * _item^.mask^.size);
		if _item^.mask^.ValidatePoint(cell) and not _item^.mask^.Bit(cell) then exit;

		case event of
			gui_MouseEnter: result += [gui_MouseProceed];
			gui_MouseDown:
				begin
					if Assigned(_inv) and _inv^.Validate then
					begin
						result += [gui_MouseProceed, gui_BringToFront, gui_Simulacra];
						if not justTry then
						begin
							_inv^.StartFloat(_item, nil, screenPos, no);
							exit; // после _inv^.StartFloat этот предмет будет уничтожен!
						end;
					end;
				end;
		end;
		result += inherited _HandleMouse(gpos, delta, event, justTry);
	end;

	function InventoryItem._AttachHint(hparent, source: pControl; const at: Vec2): pHint;
	begin
		result := _inv^._AttachHint(hparent, source, at);
	end;

	constructor InventoryItem.Init(newItem: pItem; newInv: pInventoryWindow; newInvId: sint);
	begin
		if not Assigned(newItem) then Fail;
		inherited Init;
		_item := MakeRef(newItem);
		_inv := newInv;
		_invId := newInvId;
		SwitchToAnim(_item^.Sprite);
	end;

	destructor InventoryItem.Done;
	begin
		Release(_item);
		inherited Done;
	end;

	function CellularHighlight.Invalid: CellularHighlight;
	begin
		result.size.x := High(result.size.x);
	end;

	procedure CellularHighlight.Initialize(const size: UintVec2);
	begin
		self.size := size; Assert(self.size.x <> High(self.size.x));
		tex.Init(GLtexture_2D, self.size, GLformat_RGBA, [texture_DontFilter], texture_Dynamic); tex.MakeStatic;
		Rect(0, 0, size.x, size.y, Vec4.Zero);
	end;

	procedure CellularHighlight.Finalize;
	begin
		if size.x = High(size.x) then exit;
		tex.Done;
	end;

	procedure CellularHighlight.Rect(ax, ay, w, h: sint; const color: Vec4);
	var
		i: sint;
		tdata: pVec4f;
		tdatasize: size_t;
	begin
		ay := sint(size.y) - ay - h;
		if ax < 0 then
		begin
			w += ax;
			ax := 0;
		end;
		if ay < 0 then
		begin
			h += ay;
			ay := 0;
		end;
		if ax + w > sint(size.x) then w := sint(size.x) - ax;
		if ay + h > sint(size.y) then h := sint(size.y) - ay;
		if (w <= 0) or (h <= 0) then exit;

		tdata := nil;
		tdatasize := w * h * sizeof(tdata^);
		tdata := GetMem(tdatasize);
		for i := 0 to w * h - 1 do
			tdata[i] := color;
		if (w > 0) and (h > 0) then tex.SubImage(UintVec2.Make(ax, ay), UintVec2.Make(w, h), GLformat_RGBA32f, tdatasize, tdata);
		FreeMem(tdata);
	end;

	function CellularHighlight.Texture: pTexture;
	begin
		result := @tex;
	end;

	procedure InventoryWindow._Recalculate(what: RecalculateItems);
	var
		i: sint;
		sz, cellsSize: Vec2;
	begin
		if (recalc_Bg in what) then
		begin
			if _tile then
				p_GUItex^.SetVec4(Vec4.Make(Vec2.Zero, _inv^.Size))
			else
				p_GUItex^.SetVec4(Vec4.Make(Vec2.Zero, Vec2.Ones));
		end;
		if recalc_Sizes in what then
		begin
			sz := _inv^.Size;
			_absBorder := _relBorder * _cellSize * Vec4.Make(sz, sz);
			cellsSize := _cellSize * sz;
			Sizes := Vec2.Make(_absBorder.x + _absBorder.z, _absBorder.y + _absBorder.w) + cellsSize;
			for i := 0 to High(_items) do
				_Recalculate(_items[i]);

			gl.values.Value('borderXYicellsZW', GLType.Vec4, 1, [NativeGLValueFlag.NonSerializable])^.SetVec4(
				Vec4.Make(_relBorder.x, _relBorder.y, 1.0 / max(1.0 - _relBorder.x - _relBorder.z, 0.01), 1.0 / max(1.0 - _relBorder.y - _relBorder.w, 0.01)));
			gl.values.Value('nCells', GLType.Vec2, 1, [NativeGLValueFlag.NonSerializable])^.SetVec2(_inv^.Size);
		end;
	end;

	procedure InventoryWindow._Recalculate(item: pInventoryItem);
	var
		invRec: Inventory.pItemRec;
	begin
		Assert(item^._inv = @self);
		if item^._invId >= 0 then
		begin
			Assert(_items[item^._invId] = item);
			invRec := _inv^.Locate(item^.item);
			item^.Position := Vec2.Make(_absBorder.x, _absBorder.y) + _cellSize * invRec^.pos;
		end;
		item^.Sizes := _cellSize * item^._item^.mask^.size;
	end;

	procedure InventoryWindow.MaybeRecalculate;
	begin
		if _recalc <> [] then
		begin
			_Recalculate(_recalc);
			_recalc := [];
		end;
	end;

	procedure InventoryWindow._SetCellSize(const newSize: float);
	begin
		if not Equals(_cellSize, newSize) then
		begin
			_cellSize := newSize;
			Include(_recalc, recalc_Sizes);
		end;
	end;

	procedure InventoryWindow._SetTile(newTile: boolean);
	begin
		if _tile <> newTile then
		begin
			_tile := newTile;
			Include(_recalc, recalc_Bg);
		end;
	end;

	procedure InventoryWindow._SetBorder(const newBorder: Vec4);
	begin
		if _relBorder <> newBorder then
		begin
			_relBorder := newBorder;
			Include(_recalc, recalc_Sizes);
		end;
	end;

	procedure InventoryWindow._HandleAdd(item: pItem);
	var
		nid: sint;
		gitem: pInventoryItem;
	begin
		nid := length(_items);
		gitem := new(pInventoryItem, Init(item, @self, nid));
		gitem^._flags += [gui_NonSerializable];
		SetLength(_items, nid + 1);
		_items[nid] := MakeRef(gitem);
		Attach(gitem);
		_Recalculate(gitem);
	end;

	procedure InventoryWindow._HandleRemove(item: pItem);
	var
		id: sint;
		gitem: pInventoryItem;
	begin
		gitem := _Find(item);
		Assert(Assigned(gitem));

		id := gitem^._invId;
		Assert(_items[id] = gitem);
		Release(_items[id]);
		if id <> High(_items) then
		begin
			_items[id] := _items[High(_items)];
			_items[id]^._invId := id;
		end;
		SetLength(_items, length(_items) - 1);
		gitem^.Detach;
	end;

	function InventoryWindow._Find(item: pItem): pInventoryItem;
	var
		i: sint;
	begin
		for i := 0 to High(_items) do
			if _items[i]^._item = item then
				exit(_items[i]);
		result := nil;
	end;

	procedure _OnAddItem(inv: pInventory; item: pItem; param: pointer);
	var
		gui: pInventoryWindow absolute param;
	begin
		Assert(inv = gui^._inv);
		gui^._HandleAdd(item);
	end;

	procedure _OnRemoveItem(inv: pInventory; item: pItem; param: pointer);
	var
		gui: pInventoryWindow absolute param;
	begin
		Assert(inv = gui^._inv);
		gui^._HandleRemove(item);
	end;

	procedure _OnScrew(inv: pInventory; param: pointer);
	var
		gui: pInventoryWindow absolute param;
	begin
		Assert(inv = gui^._inv);
		pInventoryWindow(param)^._Invalidate;
	end;

	constructor InventoryWindow.Init(newInv: pInventory);
	begin
		inherited Init;
		_inv := MakeRef(newInv);
		Draggable := yes;
		_tile := no;
		_cellSize := 0.09;
		_relBorder := Vec4.Zero;
		_absBorder := Vec4.Zero;
		_items := nil;
		_floating := nil;
		onDrop.Init;
		onValidate.Init;
		onDestroy.Init;
		highlight := CellularHighlight.Invalid;
		if Assigned(_inv) then
			_Initialize
		else
			ConstructorFailed;
	end;

	constructor InventoryWindow.DeseInit;
	begin
		inherited Init;
	end;

	destructor InventoryWindow.Done;
	begin
		gl.values.ForceCleanup;
		highlight.Finalize;
		onDestroy.Done;
		onValidate.Done;
		onDrop.Done;
		ReleaseArray(USystem.ObjectsList(_items));
		Release(_inv);
		inherited Done;
	end;

	procedure InventoryWindow._Initialize;
	var
		i: sint;
	begin
		highlight.Initialize(_inv^.size);
		gl.values.Value('highlight', GLType.Sampler, 1, [NativeGLValueFlag.NonSerializable])^.SetTex(highlight.Texture);
		for i := 0 to High(_inv^.items) do
			_HandleAdd(_inv^.items[i].item);
		_recalc := [Low(RecalculateItem) .. High(RecalculateItem)];
	end;

	procedure InventoryWindow._EnableRuntime;
	begin
		_inv^.SetViewCallbacks(@_OnAddItem, @_OnRemoveItem, @_OnScrew, @self);
	end;

	procedure InventoryWindow._DisableRuntime;
	begin
		_inv^.ResetViewCallbacks;
	end;

	procedure InventoryWindow.Project(const local: Vec2; out ix, iy: sint; center: boolean);
	var
		f: Vec2;
	begin
		f := (local - Vec2.Make(_absBorder.x, _absBorder.y)) / _cellSize;
		if center then ix := round(f.x) else ix := trunc(f.x);
		if center then iy := round(f.y) else iy := trunc(f.y);
	end;

	function InventoryWindow.ItemAt(const local: Vec2): pItem;
	var
		x, y: sint;
	begin
		Project(local, x, y, no);
		if (x >= 0) and (y >= 0) then result := _inv^.ItemAt(UintVec2.Make(x, y)) else result := nil;
	end;

type
	pItemFloatParams = ^ItemFloatParams;
	ItemFloatParams = object
		gitem: pInventoryItem;
		spot: Vec2;
		stoppedByGui: boolean;
		rects: Bitfield2D.Rects;
		backlit: record
			active: boolean;
			inv: pInventoryWindow;
			x, y: sint;
			color: Vec4;
		end;
		constructor Init(newGItem: pInventoryItem; const newSpot: Vec2);
		destructor Done; virtual;
		function SrcItem: pItem;
		function SrcGInv: pInventoryWindow;
		function SrcInv: pInventory;
		procedure Highlight(inv: pInventoryWindow; atX, atY: sint; const color: Vec4);
		procedure CloseHighlight;
	end;

	constructor ItemFloatParams.Init(newGItem: pInventoryItem; const newSpot: Vec2);
	begin
		gitem := MakeRef(newGItem);
		spot := newSpot;
		stoppedByGui := no;
		backlit.active := no;
		rects := SrcItem^.mask^.ToRects;
	end;

	destructor ItemFloatParams.Done;
	begin
		CloseHighlight;
		Release(gitem);
	end;

	function ItemFloatParams.SrcItem: pItem; begin result := gitem^.item; end;
	function ItemFloatParams.SrcGInv: pInventoryWindow; begin result := gitem^.inv; end;
	function ItemFloatParams.SrcInv: pInventory; begin result := gitem^.inv^._inv; end;

	procedure ItemFloatParams.Highlight(inv: pInventoryWindow; atX, atY: sint; const color: Vec4);
	var
		i: sint;
	begin
		if backlit.active and (backlit.inv = inv) and (backlit.x = atX) and (backlit.y = atY) and (backlit.color = color) then exit;
		CloseHighlight;
		backlit.active := yes;
		backlit.inv := MakeRef(inv);
		for i := 0 to High(rects) do
			inv^.highlight.Rect(atX + sint(rects[i].pos.x), atY + sint(rects[i].pos.y), rects[i].size.x, rects[i].size.y, color);
		backlit.x := atX;
		backlit.y := atY;
	end;

	procedure ItemFloatParams.CloseHighlight;
	var
		i: sint;
	begin
		if not backlit.active then exit;
		for i := 0 to High(rects) do
			backlit.inv^.highlight.Rect(backlit.x + sint(rects[i].pos.x), backlit.y + sint(rects[i].pos.y), rects[i].size.x, rects[i].size.y, Vec4.Zero);
		Release(backlit.inv);
		backlit.active := no;
	end;

	procedure _ItemFloatMove(const gpos: Vec2; ctl: pControl; param: pointer);
	const
		Colors: array[boolean] of Vec4 =
		(
			(data: (1.0, 0.0, 0.0, 1.0)),
			(data: (0.0, 1.0, 0.0, 1.0))
		);
	var
		p: pItemFloatParams absolute param;
		x, y: sint;
		ginv: pInventoryWindow absolute ctl;
	begin
		Assert((@gpos = @gpos) and (@ctl = @ctl) and (@p = @p));
		if Assigned(ctl) and (TypeOf(ctl^) = TypeOf(InventoryWindow)) then
		begin
			ginv^.Project(gpos - p^.spot - ginv^.ScreenPos, x, y, yes);
			p^.Highlight(ginv, x, y, Colors[(x >= 0) and (y >= 0) and ginv^._inv^.Fits(p^.SrcItem, UintVec2.Make(x, y))]);
		end else
			p^.CloseHighlight;
	end;

	function _ItemFloatDone(const gpos: Vec2; ctl: pControl; mode: GUIRoot.FloatStopMode; param: pointer): boolean;
	var
		p: pItemFloatParams absolute param;
		inv: pInventoryWindow absolute ctl;
		x, y, i: sint;
		ret: InventoryWindow.OnDropRets;
		tf: Transform;
		vel: Vec3;
		tfp: pTransform;
		velp: pVec3;
	begin
		p^.stoppedByGui := yes;
		case mode of
			float_Abandon, float_Emergency:
				begin
					if mode = float_Abandon then p^.SrcInv^.StopFloat(p^.SrcItem, no);
					result := yes;
				end;
			float_JustAsPlanned:
				if Assigned(ctl) then
				begin
					if TypeOf(ctl^) = TypeOf(InventoryWindow) then
					begin
						inv^.Project(gpos - p^.spot - inv^.ScreenPos, x, y, yes);
						result := (x >= 0) and (y >= 0) and inv^._inv^.Fits(p^.SrcItem, UintVec2.Make(x, y));
						if result then
						begin
							p^.SrcInv^.StopFloat(p^.SrcItem, yes);
							if not inv^._inv^.Add(p^.SrcItem, UintVec2.Make(x, y)) then Assert(no, 'предмет помещался мгновение назад! :(');
						end;
					end else
						result := no;
				end else
				begin
					ret := p^.SrcGInv^.CallOnDrop(p^.SrcItem, tf, vel);
					result := onDrop_AllowDrop in ret;
					if result then
					begin
						if onDrop_HasTransform in ret then tfp := @tf else tfp := nil;
						if onDrop_HasVel in ret then velp := @vel else velp := nil;
						result := p^.SrcInv^.Drop(p^.SrcItem, tfp, velp, no);
				end;
				end;
			else Assert(no);
		end;

		if result then
		begin
			p^.CloseHighlight;
			inv := p^.SrcGInv;
			for i := 0 to High(inv^._floating) do
				if inv^._floating[i] = p then
				begin
					inv^._floating[i] := inv^._floating[High(inv^._floating)];
					SetLength(inv^._floating, length(inv^._floating) - 1);
				end;
			dispose(p, Done);
		end else
			p^.stoppedByGui := no;
	end;

	procedure _CancelFloat(inv: pInventory; item: pItem; param: pointer);
	var
		p: pItemFloatParams absolute param;
	begin
		Assert((@inv = @inv) and (@item = @item));
		if not p^.stoppedByGui then
			pGUI(p^.SrcGInv^._gui)^.StopFloat(p^.gitem, nil, float_Emergency);
	end;

	procedure InventoryWindow.StartFloat(item: pItem; oitem: pOutdoorItem; const pos: Vec2; isMousePos: boolean);
	var
		fp: pItemFloatParams;
		mpos, spot: Vec2;
		gitem: pInventoryItem;
	begin
		if Assigned(oitem) then
		begin
			Assert(not Assigned(item));
			item := oitem^.item;
		end;

		spot := 0.5 * _cellSize * item^.mask^.size;
		if isMousePos then mpos := pos else mpos := pos + spot;

		if Assigned(oitem) then
		begin
			gitem := new(pInventoryItem, Init(item, @self, -1));
			_Recalculate(gitem);
		end else
			gitem := _Find(item);
		fp := new(pItemFloatParams, Init(gitem, spot));
		if Assigned(oitem) then
			_inv^.PickFloating(oitem, @_CancelFloat, fp)
		else
			_inv^.StartFloat(item, @_CancelFloat, fp);
		SetLength(_floating, length(_floating) + 1);
		_floating[High(_floating)] := fp;

		pGUI(_gui)^.MousePos := mpos;
		pGUI(_gui)^.StartFloat(gitem, spot, @_ItemFloatMove, @_ItemFloatDone, fp);
	end;

	function InventoryWindow.Pick(oitem: pOutdoorItem): boolean;
	begin
		result := Assigned(oitem) and Assigned(_gui);
		if result then
			StartFloat(nil, oitem, pGUI(_gui)^._mousePos, yes);
	end;

	procedure _CallOnDrop(const info: SingleDelegateInfo; param: pointer);
	var
		args: ^InventoryWindow.OnDropArgs absolute param;
	begin
		with args^ do ret := InventoryWindow.OnDropProc(info.proc)(inv, item, tf, vel, info);
	end;

	function InventoryWindow.CallOnDrop(item: pItem; out tf: Transform; out vel: Vec3): OnDropRets;
	var
		args: OnDropArgs;
	begin
		if onDrop.Empty then exit([onDrop_AllowDrop]);
		result := [];
		args.inv := @self;
		args.item := item;
		args.tf.tr.x := NaN;
		args.vel.x := NaN;
		onDrop.Call(@_CallOnDrop, @args);
		if args.ret then Include(result, onDrop_AllowDrop);
		if not FloatIs.NaN(args.tf.tr.x) then
		begin
			Include(result, onDrop_HasTransform);
			tf := args.tf;
		end;
		if not FloatIs.NaN(args.vel.x) then
		begin
			Include(result, onDrop_HasVel);
			vel := args.vel;
		end;
	end;

	procedure _CallOnValidate(const info: SingleDelegateInfo; param: pointer);
	var
		ppinv: ^pInventoryWindow absolute param;
	begin
		if not InventoryWindow.ValidateProc(info.proc)(ppinv^, info) then ppinv^ := nil;
	end;

	function InventoryWindow.Validate: boolean;
	var
		inv: pInventoryWindow;
	begin
		if not Assigned(_inv) then result := no else
		begin
			if onValidate.Empty then result := yes else
			begin
				inv := @self;
				onValidate.Call(@_CallOnValidate, @inv);
				result := Assigned(inv);
				if not result then
					_Invalidate;
			end;
		end;
	end;

	procedure _CallOnDestroy(const info: SingleDelegateInfo; param: pointer);
	var
		inv: pInventoryWindow absolute param;
	begin
		InventoryWindow.DestroyProc(info.proc)(inv, info);
	end;

	procedure InventoryWindow._Invalidate;
	begin
		Assert(Assigned(_inv));
		if not onDestroy.Empty then onDestroy.Call(@_CallOnDestroy, @self);
		_AbandonFloatings;
		PrettyDetach;
		Release(_inv);
	end;

	procedure InventoryWindow._AbandonFloatings;
	var
		i: sint;
	begin
		for i := High(_floating) downto 0 do
			pGUI(_gui)^.AbandonFloat(pItemFloatParams(_floating[i])^.gitem);
	end;

	procedure InventoryWindow._Update(const dt: float);
	begin
		if not Validate then exit;
		MaybeRecalculate;
		inherited _Update(dt);
	end;

	procedure InventoryWindow._AfterAttach;
	begin
		MaybeRecalculate;
		inherited _AfterAttach;
		_EnableRuntime;
	end;

	procedure InventoryWindow._BeforeDetach;
	begin
		_AbandonFloatings;
		_DisableRuntime;
		inherited _BeforeDetach;
	end;

	function InventoryWindow._AttachHint(hparent, source: pControl; const at: Vec2): pHint;
	begin
		if source = pControl(@self) then result := nil else result := inherited _AttachHint(hparent, source, at);
	end;

	procedure _ConHandleKey(key: KeyboardKey; ev: ButtonEvent; const info: SingleDelegateInfo);
	begin
		Assert(KeyboardInput.tOnKeyProc(@_ConHandleKey) = @_ConHandleKey);
		pConsole(info.user)^._HandleKey(key, ev);
		if ev <> button_Release then info.Stop;
	end;

	procedure Console._SetActive(value: boolean);
	var
		key: KeyboardKey;
		ev: ButtonEvent;
	begin
		Assert(Assigned(_gui));
		if _active = value then exit;
		_active := value;

		if value then
		begin
			mm.window.EnableInput([_inputException]);
			for key in [key_Delete, key_Up, key_Down, key_Left, key_Right, key_Home, key_End] do
				for ev in ButtonEvent do
					mm.keyboard.keyEvents[key, ev].Add(@_ConHandleKey, @self, +1);
		end else
		begin
			for key in [key_Delete, key_Up, key_Down, key_Left, key_Right, key_Home, key_End] do
				for ev in ButtonEvent do
					mm.keyboard.keyEvents[key, ev].Remove(@_ConHandleKey, @self);
			mm.window.DisableInput;
		end;
	end;

	function Console._ApplyInputLine(const line: string): boolean;
	var
		t: string;
		i, n, p, pp: sint;
	begin
		result := no;
		n := 0;
		t := line;
		for i := 1 to length(line) do
			case line[i] of
				EOL: Assert(no);
				Backspace:
					if n > 0 then UTF8.Prev(t, n) else
						if _inpcur > 0 then
						begin
							pp := _inpcur + 1;
							p := pp;
							if UTF8.Prev(_input, p) <> UTFInvalid then
							begin
								delete(_input, p, pp - p);
								_inpcur := p - 1;
							end;
							result := yes;
						end;
				TabSym, #32 .. #255:
					begin
						inc(n);
						t[n] := line[i];
					end;
			end;
		if n > 0 then
		begin
			result := yes;
			insert(Copy(t, 1, n), _input, _inpcur + 1);
			_inpcur += n;
		end;
	end;

	procedure Console._ApplyInput(const input: string);
	var
		pp, p: sint;
	begin
		if input = '' then exit;
		p  := 1;

		repeat
			pp := p;
			p := Pos(EOL, input, p);
			if p = 0 then
			begin
				if _ApplyInputLine(Copy(input, pp, length(input) - pp + 1)) then
					_UpdateText;
				break;
			end else
			begin
				_ApplyInputLine(Copy(input, pp, p - pp));
				_SubmitInput;
				inc(p, length(EOL));
			end;
		until no;
	end;

	procedure Console._SubmitInput;
	var
		errmsg: string;
		empty: boolean;
	begin
		empty := _input = '';
		_AddToHistory(_input, no);
		inc(_lines);
		_frozen += Prompt[_continue] + _input + EOL;
		_chunk += _input + EOL;
		_input := '';
		_inpcur := 0;

		if empty and _continue then
		begin
			_continue := no;
			_chunk := '';
			_UpdateText;
			exit;
		end;

		if not _ss^.GetAssociated(@self) then Assert(no);
		if _ss^.GetAssociated(@self, 1) then
		begin
			_ss^.Call(0, 1);
			if not _ss^.IsTable(-1) then _ss^.Throw('От Console.env() ожидается таблица');
			_ss^.PushNil;
			while _ss^.Next(-2) do
			begin
				_ss^.PushCopy(-2);
				_ss^.Insert(-2);
				_ss^.SetTable(-5);
			end;
			_ss^.Pop;
		end;

		_continue := no;
		case _ss^.Execute(_chunk, 'консоль', -1, @errmsg) of
			exec_Ok: ;
			exec_Incomplete: _continue := yes;
			else
				Write(errmsg);
		end;
		if not _continue then
			_chunk := '';
		_ss^.Pop;

		_UpdateText;
		_hipos := -1;
	end;

	procedure Console._AddToHistory(const line: string; shiftOnlyNew: boolean);
	var
		i, j: sint;
		shift: boolean;
	begin
		if line = '' then exit;
		if (_nhist > 0) and (Prefixed(_history[0], line)) then
		begin
			_history[0] := line;
			exit;
		end;

		for i := 0 to _nhist do
			if (i = _nhist) or (_history[i] = line) then
			begin
				shift := (i = _nhist) or not shiftOnlyNew;
				if (i = _nhist) and (_nhist < length(_history)) then
					inc(_nhist);

				if shift then
				begin
					if _hipos < i then _hipos := (_hipos + 1) mod _nhist;
					for j := min(i, High(_history)) downto 1 do
						_history[j] := _history[j - 1];
					_history[0] := line;
				end;
				exit;
			end;
		Assert(no);
	end;

	procedure Console._HandleKey(key: KeyboardKey; event: ButtonEvent);
		procedure GetInputFromHistory;
		begin
			if _hipos < 0 then exit;
			_AddToHistory(_input, yes);
			_input := _history[_hipos];
			_inpcur := length(_input);
			_UpdateText;
		end;
	var
		pp, p: sint;
	begin
		case event of
			button_Click:
				case key of
					key_Up:
						begin
							if _nhist > 0 then
								if _hipos < 0 then _hipos := 0 else _hipos := (_hipos + 1) mod _nhist
							else
								_hipos := -1;
							GetInputFromHistory;
						end;
					key_Down:
						begin
							if (_nhist > 0) and (_hipos < 0) then _hipos := 0 else
								if _hipos > 0 then dec(_hipos) else _hipos := _nhist - 1;
							GetInputFromHistory;
						end;
					key_Left:
						if _inpcur > 0 then
						begin
							inc(_inpcur);
							if UTF8.Prev(_input, _inpcur) = UTFInvalid then _inpcur := 0 else dec(_inpcur);
							_UpdateText;
						end;
					key_Right:
						if _inpcur < length(_input) then
						begin
							inc(_inpcur);
							if UTF8.Next(_input, _inpcur) = UTFInvalid then _inpcur := length(_input) else dec(_inpcur);
							_UpdateText;
						end;
					key_Delete:
						begin
							pp := _inpcur + 1;
							p := pp;
							if UTF8.Next(_input, p) <> UTFInvalid then
							begin
								delete(_input, pp, p - pp);
								_UpdateText;
							end;
						end;
					key_Home:
						if _inpcur > 0 then
						begin
							_inpcur := 0;
							_UpdateText;
						end;
					key_End:
						if _inpcur < length(_input) then
						begin
							_inpcur := length(_input);
							_UpdateText;
						end;
				end;
		end;
	end;

	procedure Console._UpdateText;
	var
		i, p: sint;
	begin
		p := 0;
		for i := 1 to _lines - _maxlines do
		begin
			p := Pos(EOL, _frozen, p + ord(p = 0));
			Assert(p > 0);
			inc(p, length(EOL));
		end;

		if p > 0 then
		begin
			delete(_frozen, 1, p - 1);
			_lines := _maxlines;
		end;

		_text.text := _frozen + Prompt[_continue] + Copy(_input, 1, _inpcur) + '|' + Copy(_input, _inpcur + 1, length(_input) - _inpcur);
	end;

	procedure Console._Update(const dt: float);
	begin
		if Sizes.x <> pGUI(_gui)^.Sizes.x then Sizes := Vec2.Make(pGUI(_gui)^.Sizes.x, Sizes.y);
		if _active then _ApplyInput(mm.window.Input(yes));
		inherited _Update(dt);
	end;

	procedure Console._AfterAttach;
	begin
		inherited _AfterAttach;
		Active := yes;
	end;

	procedure Console._BeforeDetach;
	begin
		Active := no;
		inherited _BeforeDetach;
	end;

	procedure Console._OnSetSizes;
	begin
		inherited _OnSetSizes;
		_maxLines := max(1, trunc(min(Sizes.y / (_text.scale * _text.font^.ft.LineDistance) - 0.5, 1000.0)));
		_UpdateText;
	end;

	constructor Console.Init(newSS: pScriptState);
	begin
		inherited Init;
		_ss := newSS;
		_flags += [gui_NonSerializable];
		_text.Init; _text.MakeStatic; _text._flags += [gui_NonSerializable];
		_text.Position := Vec2.Make(0.03);
		_text.PositionBaseY := pos_B;
		Attach(@_text);

		_maxlines := 1;
		_inputException := key_Esc;
		_active := no;
		Clear;
	end;

	destructor Console.Done;
	begin
		Assert(not _active);
		inherited Done;
		_text.Done;
	end;

	procedure Console.Write(const line: string);
	var
		p: sint;
	begin
		if line = '' then exit;
		p := 1;
		repeat
			p := Pos(EOL, line, p);
			if p > 0 then
			begin
				inc(_lines);
				inc(p);
			end else
				break;
		until no;

		_frozen += line;
		if line[length(line)] <> EOL then
		begin
			inc(_lines);
			_frozen += EOL;
		end;
		_UpdateText;
	end;

	procedure Console.Clear;
	begin
		_lines := 1;
		_nhist := 0;
		_hipos := -1;
		_inpcur := 0;
		_frozen := '';
		_input := '';
		_chunk := '';
		_continue := no;
	end;

	procedure Script_GUI_SetCursor(var ss: ScriptState);
	var
		im: pImage;
	begin
		if ss.GetTableS(2, 'image') then
		begin
			im := MakeRef(ss.ToObject(-1, TypeOf(Image)));
			ss.Pop;
		end else
			im := nil;
		pGUI(ss.ToSelf)^.ChangeCursor(im, ss.GetVec2Field(2, 'spot'));
		Release(im);
	end;

	procedure Script_GUI_AbandonAnyFloat(var ss: ScriptState);
	begin
		pGUI(ss.ToSelf)^.AbandonAnyFloat;
	end;

	procedure Script_GUI_Forget(var ss: ScriptState);
	begin
		pGUI(ss.ToSelf)^.memory.Forget(ss.ToString(2));
	end;

	procedure Script_GUI_SkipFx(var ss: ScriptState);
	begin
		pGUI(ss.ToSelf)^.SkipEffects;
	end;

	procedure Script_GUI_SkipFxBreakpoint(var ss: ScriptState);
	begin
		pGUI(ss.ToSelf)^.SkipEffectsBreakpoint;
	end;

{$define fname := Script_GUI_mouseFocus} {$define otype := GUIRoot} {$define field := MouseEnabled} {$define prop_bool} {$include script_prop.inc}

	procedure Script_GUI_floating(var ss: ScriptState);
	begin
		ss.PushObject(pGUI(ss.ToSelf)^.Floating);
	end;

	procedure Script_GUI_mousePosN(var ss: ScriptState; read: boolean);
	begin
		if read then
			ss.PushVec2(pGUI(ss.ToSelf)^.MousePos / pGUI(ss.ToSelf)^.Sizes)
		else
			pGUI(ss.ToSelf)^.MousePos := ss.ToVec2(3);
	end;

	procedure Script_GUI_mousePos(var ss: ScriptState);
	begin
		ss.PushVec2(pGUI(ss.ToSelf)^.MousePos);
	end;

	procedure Script_Control_attached(var ss: ScriptState);
	begin
		ss.PushBool(Assigned(pControl(ss.ToSelf)^._gui));
	end;

	procedure Script_Control_pos(var ss: ScriptState; read: boolean);
	var
		ctl: pControl;
	begin
		ctl := pControl(ss.ToSelf);
		if read then
			ss.PushVec2(ctl^.Position)
		else
		begin
			if ss.IsTable(3) then
			begin
				ctl^.Position := ss.GetVec2Field(3, 1);
				if ss.HasField(3, 2) then ctl^.PositionBaseStr := ss.GetStringField(3, 2);
			end else
				ctl^.Position := ss.ToVec2(3);
		end;
	end;

	procedure Script_Control_screenPos(var ss: ScriptState);
	begin
		ss.PushVec2(pControl(ss.ToSelf)^.ScreenPos);
	end;

{$define fname := Script_Control_sizes} {$define otype := Control} {$define field := Sizes} {$define prop_vec2}
{$include script_prop.inc}

	function Script_Control_GL(var ss: ScriptState): sint;
	begin
		result := Script_modify_gl_and_also_query(ss, 2, pControl(ss.ToSelf)^.gl);
	end;

	procedure Script_Control_material(var ss: ScriptState; read: boolean);
	var
		ctl: pControl;
	begin
		ctl := ss.ToSelf;
		if read then
			ss.PushObject(ctl^.Material)
		else
			ctl^.Material := Script_create_material(ss, 3);
	end;

{$define fname := Script_Control_memory} {$define otype := Control} {$define field := MemoryID} {$define prop_string}
{$include script_prop.inc}

	procedure _Control_OnMouseEnterLeave(ctl: pControl; const gpos: Vec2; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(OnMouseEnterLeaveProc(@_Control_OnMouseEnterLeave) = @_Control_OnMouseEnterLeave);
		if not sd^.GetFunction {$ifdef Debug}('Control.OnMouseEnter/Leave'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(ctl);
			PushVec2(gpos);
			Call(2, 0);
		end;
	end;

	procedure _Control_OnMouseOver(ctl: pControl; const gpos, delta: Vec2; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(OnMouseOverProc(@_Control_OnMouseOver) = @_Control_OnMouseOver);
		if not sd^.GetFunction {$ifdef Debug}('Control.OnMouseOver'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(ctl);
			PushVec2(gpos);
			PushVec2(delta);
			Call(3, 0);
		end;
	end;

	procedure _Control_OnMouseDown(ctl: pControl; const gpos: Vec2; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(OnMouseDownProc(@_Control_OnMouseDown) = @_Control_OnMouseDown);
		if not sd^.GetFunction {$ifdef Debug}('Control.OnMouseDown'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(ctl);
			PushVec2(gpos);
			Call(2, 0);
		end;
	end;

	procedure Script_Control_onMouseEnter(var ss: ScriptState);
	var
		ctl: pControl;
	begin
		ctl := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(ctl, @ctl^.onMouseEnter, @_Control_onMouseEnterLeave)));
	end;

	procedure Script_Control_onMouseOver(var ss: ScriptState);
	var
		ctl: pControl;
	begin
		ctl := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(ctl, @ctl^.onMouseOver, @_Control_onMouseOver)));
	end;

	procedure Script_Control_onMouseLeave(var ss: ScriptState);
	var
		ctl: pControl;
	begin
		ctl := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(ctl, @ctl^.onMouseLeave, @_Control_onMouseEnterLeave)));
	end;

	procedure Script_Control_onMouseDown(var ss: ScriptState);
	var
		ctl: pControl;
	begin
		ctl := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(ctl, @ctl^.onMouseDown, @_Control_onMouseDown)));
	end;

	procedure Script_Control_Attach(var ss: ScriptState);
	var
		i: sint;
	begin
		for i := 2 to ss.Top do
			pControl(ss.ToSelf)^.Attach(pControl(ss.ToObject(i, TypeOf(Control))));
	end;

	procedure Script_Control_Detach(var ss: ScriptState);
	var
		ctl: pControl;
	begin
		ctl := ss.ToSelf;
		if ss.IsTable(2) and (ss.GetBoolField(2, 'pretty')) then
			ctl^.PrettyDetach
		else
			ctl^.Detach;
	end;

	procedure _HandleStrongLinkToControl(ctl: pControl; link: boolean; var ss: ScriptState); forward;

	function Script_Control_Move(var ss: ScriptState): sint;
	var
		ctl: pControl;
		path: pDimensionalPath;
	begin
		ctl := ss.ToSelf;
		if ss.GetTableS(2, 'path') then
		begin
			path := Script_dimensional_path_ref(ss, -1, 2);
			ss.Pop;
		end else
			ss.Throw('не задан path');
		result := Script_add_action(ss, ctl, new(pControlMove, Init(path)));
		_HandleStrongLinkToControl(ctl, yes, ss);
		Release(path);
	end;

	function Script_Control_SlideGL(var ss: ScriptState): sint;
	var
		ctl: pControl;
	begin
		ctl := ss.ToSelf;
		result := Script_SlideGL(ss, ctl, ctl^.gl);
	end;

{$define fname := Script_Text_text} {$define otype := Text} {$define field := Text} {$define prop_string}
{$include script_prop.inc}

	function _Control_OnHint(ctl, parent: pControl; const at: Vec2; const info: SingleDelegateInfo): pHint;
	var
		sd: pScriptDelegate absolute info.user;
	begin
		if not sd^.GetFunction {$ifdef Debug} ('Control.OnHint') {$endif} then exit(nil);
		with sd^.ss^ do
		begin
			PushObject(ctl);
			PushObject(parent);
			PushVec2(at);
			Call(3, 1);
			if Typ(-1) = script_Object then result := ToObject(-1, TypeOf(Hint)) else result := nil;
			Pop;
		end;
	end;

	procedure _Control_OnPrettyDetach(ctl: pControl; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(Control.OnHintProc(@_Control_OnHint) = @_Control_OnHint);
		Assert(Control.OnPrettyDetachProc(@_Control_OnPrettyDetach) = @_Control_OnPrettyDetach);
		if not sd^.GetFunction {$ifdef Debug} ('Control.OnPrettyDetach') {$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(ctl);
			Call(1, 0);
		end;
	end;

	procedure __common_create_control(var ss: ScriptState; var ctl: Control; idx: sint);
	var
		i: sint;
	begin
		if not ss.IsTable(idx) then exit;
		ctl.Position := ss.GetVec2Field(idx, 'pos', ctl.Position);
		ctl.PositionBaseStr := ss.GetStringField(idx, 'pb', ctl.PositionBaseStr);
		ctl.sizes := ss.GetVec2Field(idx, 'sizes', ctl.sizes);
		ctl.SizeBaseStr := ss.GetStringField(idx, 'sb', ctl.SizeBaseStr);
		if ss.GetTableS(idx, 'gl') then
		begin
			Script_modify_gl(ss, -1, ctl.gl);
			ss.Pop;
		end;
		if ss.GetTableS(idx, 'material') then
		begin
			ctl.Material := Script_create_material(ss, -1);
			ss.Pop;
		end;
		if ss.GetTableS(idx, 'childs') then
		begin
			for i := 1 to ss.RawLen(-1) do
				ctl.Attach(ss.GetObjectField(-1, i, TypeOf(Control)));
			ss.Pop;
		end;
		if ss.GetTableS(idx, 'memory') then
		begin
			ctl.MemoryID := ss.ToString(-1);
			ss.Pop;
		end;
		if ss.GetTableS(idx, 'onMouseEnter') then
			ss.SetDelegate(@ctl, @ctl.onMouseEnter, @_Control_OnMouseEnterLeave, '');
		if ss.GetTableS(idx, 'onMouseLeave') then
			ss.SetDelegate(@ctl, @ctl.onMouseLeave, @_Control_OnMouseEnterLeave, '');
		if ss.GetTableS(idx, 'onMouseOver') then
			ss.SetDelegate(@ctl, @ctl.onMouseOver, @_Control_OnMouseOver, '');
		if ss.GetTableS(1, 'onHint') then
			ss.SetDelegate(@ctl, @ctl.onHint, Control.OnHintProc(@_Control_OnHint), '');
		if ss.GetBoolField(idx, 'external_image') then ctl.Flag[gui_ExternalImageSoItsOkayToDrawWithoutExplicitlyChosen] := yes;
		if ss.GetBoolField(idx, 'auto_pos') then ctl.Flag[gui_AutoPos] := yes;
		if ss.GetTableS(idx, 'onPrettyDetach') then
			ss.SetDelegate(@ctl, @ctl.onPrettyDetach, Control.OnPrettyDetachProc(@_Control_OnPrettyDetach), '');

		if ss.GetTableS(idx, 'parent') then
		begin
			pControl(ss.ToObject(-1, TypeOf(Control)))^.Attach(@ctl);
			ss.Pop;
		end;
	end;

	procedure Script_CreateAtlas(var ss: ScriptState);
	var
		atlas: pAtlas;
	begin
		atlas := ResourcePool.Shared^.LoadRef(TypeOf(GLUtils.Atlas), ss.ToStream(1));
		ss.PushObject(atlas);
		Release(atlas);
	end;

	procedure __common_create_text(var ss: ScriptState; var text: Text; idx: sint);
	var
		font: pFont;
	begin
		font := ResourcePool.Shared^.LoadRef(TypeOf(GUI.Font), ss.GetStreamField(idx, 'font'));
		text.font := font;
		Release(font);
		text.scale := ss.GetFloatField(idx, 'scale', text.scale);
		text.text := ss.GetStringField(idx, 'text', text.text);
		text.maxWidth := ss.GetFloatField(idx, 'max_width', text.maxWidth);
		text.paraIdent := ss.GetFloatField(idx, 'para_ident', text.paraIdent);
		__common_create_control(ss, text, idx);
	end;

	procedure Script_CreateText(var ss: ScriptState);
	var
		t: pText;
	begin
		t := new(pText, Init);
		ss.PushObject(t);
		__common_create_text(ss, t^, 1);
	end;

	function Script_create_animation(var ss: ScriptState; idx: sint; atlastable: sint): pAtlasAnimation;
	var
		flags: AtlasAnimationFlags;
		i: sint;
		defDuration: float;
		atlas: pAtlas;
	begin
		idx := ss.AbsIdx(idx);
		flags := [anim_Looped];
		if ss.IsTable(idx) then
		begin
			if ss.GetTableS(idx, 'looped') then
			begin
				if ss.ToBool(-1) then Include(flags, anim_Looped);
				ss.Pop;
			end;
		end;

		if (atlastable <> 0) and ss.HasField(atlastable, 'atlas') then
			atlas := ResourcePool.Shared^.LoadRef(TypeOf(GLUtils.Atlas), ss.GetStreamField(atlastable, 'atlas'))
		else
			atlas := nil;

		result := new(pAtlasAnimation, Init(atlas, flags));
		Release(atlas);

		case ss.Typ(idx) of
			script_Pointer: result^.Append(Rect.Make(ss.ToVec4(idx)), 0.0);
			script_String:  result^.Append(ss.ToString(idx), 0.0);
			script_Table:
				begin
					defDuration := ss.GetFloatField(idx, 'frame_duration', 1.0);
					for i := 1 to ss.RawLen(idx) do
					begin
						ss.GetTableI(idx, i);
						case ss.Typ(-1) of
							script_Pointer: result^.Append(Rect.Make(ss.ToVec4(-1)), defDuration);
							script_String:  result^.Append(ss.ToString(-1), defDuration);
							script_Table:
								begin
									ss.GetTableI(-1, 1);
									if ss.Typ(-1) = script_String then
										result^.Append(ss.ToString(-1), ss.GetFloatField(-1, 'duration', defDuration))
									else
										if ss.GetSintField(-1, 'nx', High(sint)) = High(sint) then
											result^.Append(Rect.Make(ss.GetVec4Field(-1, 'rect')), ss.GetFloatField(-1, 'duration', defDuration))
										else
											result^.Append(Rect.Make(ss.GetVec4Field(-1, 'base_rect')), ss.GetFloatField(-1, 'dx'), ss.GetFloatField(-1, 'dy'),
												ss.GetSintField(-1, 'nx'), ss.GetSintField(-1, 'n'), ss.GetFloatField(-1, 'duration', defDuration));
									ss.Pop;
								end;
						end;
						ss.Pop;
					end;
				end;
		end;
		result := AtlasAnimation.Merge(result);
	end;

	procedure __common_create_image(var ss: ScriptState; var im: Image);
	var
		tex: pTexture;
	begin
		if not ss.IsTable(1) then exit;
		__common_create_control(ss, im, 1);
		if ss.GetTableS(1, 'image') then
		begin
			if ss.IsStream(-1) then
			begin
				tex := ResourcePool.Shared^.LoadRef(TypeOf(tTexture), ss.ToStream(-1));
				im.im := tex;
				Release(tex);
			end else
				im.im := ss.ToObject(-1, TypeOf(tTexture));
			ss.Pop;
		end;

		if ss.GetTableS(1, 'anim') then
		begin
			im.SwitchToAnim(Script_create_animation(ss, -1, 1));
			ss.Pop;
		end;
	end;

	procedure Script_CreateImage(var ss: ScriptState);
	var
		im: pImage;
	begin
		im := new(pImage, Init);
		ss.PushObject(im);
		__common_create_image(ss, im^);
	end;

	procedure _Button_OnClick(sender: pButton; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(Button.OnClickProc(@_Button_OnClick) = @_Button_OnClick);
		if not sd^.GetFunction {$ifdef Debug}('[Button] OnClick'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(sender);
			Call(1, 0);
		end;
	end;

	procedure Script_Button_onClick(var ss: ScriptState);
	var
		button: pButton;
	begin
		button := ss.ToSelf;
		ss.PushObject(new(pScriptDelegateWrapper, Init(button, @button^.onClick, @_Button_OnClick)));
	end;

	procedure Script_CreateButton(var ss: ScriptState);
	var
		baseAnim, pressedAnim: pAtlasAnimation;
		b: pButton;
	begin
		baseAnim := nil;
		pressedAnim := nil;
		if ss.GetTableS(1, 'button_states') then
		begin
			if ss.GetTableS(-1, 'base') then
			begin
				baseAnim := Script_create_animation(ss, -1, 1);
				ss.Pop;
			end;
			if ss.GetTableS(-1, 'pressed') then
			begin
				pressedAnim := Script_create_animation(ss, -1, 1);
				ss.Pop;
			end;
		end;
		b := new(pButton, Init(baseAnim, pressedAnim));
		ss.PushObject(b);
		if ss.GetTableS(1, 'onClick') then
			ss.SetDelegate(b, @b^.onClick, @_Button_OnClick, '');
		__common_create_image(ss, b^);
	end;

	procedure __common_create_window(var ss: ScriptState; var w: GUIWindow);
	begin
		if not ss.IsTable(1) then exit;
		if ss.GetBoolField(1, 'draggable') then w.draggable := yes;
		if ss.GetBoolField(1, 'sizeable') then w.sizeable := yes;
		if ss.GetBoolField(1, 'skip_on_click') then w.SkipOnClick := yes;
		__common_create_image(ss, w);
	end;

	procedure Script_CreateWindow(var ss: ScriptState);
	var
		w: pGUIWindow;
	begin
		w := new(pGUIWindow, Init);
		ss.PushObject(w);
		__common_create_window(ss, w^);
	end;

	procedure _Hint_OnUpdate(hint: pHint; const dt: float; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(hint^.tOnUpdateProc(@_Hint_OnUpdate) = @_Hint_OnUpdate);
		if not sd^.GetFunction {$ifdef Debug} ('Hint.OnUpdate') {$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(hint);
			PushFloat(dt);
			Call(2, 0);
		end;
	end;

	procedure Script_CreateHint(var ss: ScriptState);
	var
		h: pHint;
	begin
		h := new(pHint, Init);
		ss.PushObject(h);
		if ss.GetTableS(1, 'onUpdate') then
			ss.SetDelegate(h, @h^.onUpdate, Hint.tOnUpdateProc(@_Hint_OnUpdate), '');
		__common_create_window(ss, h^);
	end;

	procedure Script_CreateMinimap(var ss: ScriptState);
	var
		minimap: pMinimap;
	begin
		minimap := new(pMinimap, Init(
			ss.GetVec3Field(1, 'baseColor'),
			ss.GetFloatField(1, 'baseAlpha', 1.0)));

		if ss.GetTableS(1, 'scene') then
		begin
			minimap^.scene := ss.ToObject(-1, TypeOf(SceneNode));
			ss.Pop;
		end;
		if ss.GetTableS(1, 'target') then
		begin
			minimap^.target := ss.ToObject(-1, TypeOf(SceneNode));
			ss.Pop;
		end;
		minimap^.Shift := ss.GetVec3Field(1, 'shift');

		ss.PushObject(minimap);
		__common_create_window(ss, minimap^);
	end;

{$define fname:=Script_Minimap_scene} {$define otype:=Minimap} {$define field:=scene} {$define prop_object:=SceneNode} {$i script_prop.inc}
{$define fname:=Script_Minimap_target} {$define otype:=Minimap} {$define field:=target} {$define prop_object:=SceneNode} {$i script_prop.inc}
{$define fname:=Script_Minimap_shift} {$define otype:=Minimap} {$define field:=shift} {$define prop_vec3} {$i script_prop.inc}

	procedure OnCreateIndicatorGroupControl(group: pIndicatorGroup; const id: PoolString; ctl: pControl; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(IndicatorGroup.OnCreateControlProc(@OnCreateIndicatorGroupControl) = @OnCreateIndicatorGroupControl);
		if not sd^.GetFunction {$ifdef Debug}('OnCreateIndicatorGroupControl'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(group);
			PushObject(ctl);
			PushString(id);
			Call(3, 0);
		end;
	end;

	procedure Script_CreateIndicatorGroup(var ss: ScriptState);
	var
		ind: pIndicatorGroup;
		anims: array of pAtlasAnimation;
		it, i, n: sint;
		reversed: boolean;
	begin
		ind := new(pIndicatorGroup, Init);
		ss.PushObject(ind);

		if ss.GetTableS(1, 'indicators') then
		begin
			n := ss.RawLen(-1);
			for it := 0 to n div 2 - 1 do
			begin
				ss.GetTableI(-1, 2*it + 2);
				anims := nil;
				for i := 1 to ss.RawLen(-1) do
				begin
					ss.GetTableI(-1, i);
					SetLength(anims, length(anims) + 1);
					anims[High(anims)] := Script_create_animation(ss, -1, 1);
					ss.Pop;
				end;
				reversed := ss.GetBoolField(-1, 'reversed');
				ss.Pop;

				ind^.Add(ss.GetStringField(-1, 2*it + 1), anims, reversed);
			end;
			ss.Pop;
		end;
		if ss.GetTableS(1, 'onCreateControl') then
			ss.SetDelegate(ind, @ind^.onCreateControl, IndicatorGroup.OnCreateControlProc(@OnCreateIndicatorGroupControl), '');
		__common_create_control(ss, ind^, 1);
	end;

	function _InventoryWindow_OnValidate(inv: pInventoryWindow; const info: SingleDelegateInfo): boolean;
	var
		sd: pScriptDelegate absolute info.user;
	begin
		if not sd^.GetFunction {$ifdef Debug}('InventoryWindow.OnValidate'){$endif} then exit(no);
		with sd^.ss^ do
		begin
			PushObject(inv);
			Call(1, 1);
			result := (Typ(-1) = script_Nil) or ToBool(-1);
			Pop(1);
		end;
	end;

	function _InventoryWindow_OnDrop(inv: pInventoryWindow; item: pItem; var tf: Transform; var vel: Vec3; const info: SingleDelegateInfo): boolean;
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(InventoryWindow.ValidateProc(@_InventoryWindow_OnValidate) = @_InventoryWindow_OnValidate);
		if not sd^.GetFunction {$ifdef Debug}('InventoryWindow.OnDrop'){$endif} then exit(no);
		with sd^.ss^ do
		begin
			PushObject(inv);
			PushObject(item);
			Call(2, 3);
			result := (Typ(-3) = script_Nil) or ToBool(-3);
			if Typ(-2) <> script_Nil then tf := ToTransform(-2);
			if Typ(-1) <> script_Nil then vel := ToVec3(-1);
			Pop(3);
		end;
	end;

	procedure _InventoryWindow_OnDestroy(inv: pInventoryWindow; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(InventoryWindow.OnDropProc(@_InventoryWindow_OnDrop) = @_InventoryWindow_OnDrop);
		Assert(InventoryWindow.DestroyProc(@_InventoryWindow_OnDestroy) = @_InventoryWindow_OnDestroy);
		if not sd^.GetFunction {$ifdef Debug}('InventoryWindow.OnDestroy'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(inv);
			Call(1, 0);
		end;
	end;

	procedure Script_CreateInventoryWindow(var ss: ScriptState);
	var
		inv: pInventory;
		guinv: pInventoryWindow;
	begin
		if ss.GetTableS(1, 'inventory') then
		begin
			inv := ss.ToObject(-1, TypeOf(Inventory));
			ss.Pop;
		end else
			inv := nil;

		guinv := new(pInventoryWindow, Init(inv));
		if not Assigned(guinv) then
		begin
			ss.PushNil;
			exit;
		end;
		guinv^.Border := ss.GetVec4Field(1, 'border');

		ss.PushObject(guinv);
		if ss.GetTableS(1, 'onDrop') then
			ss.SetDelegate(guinv, @guinv^.onDrop, @_InventoryWindow_OnDrop, '');
		if ss.GetTableS(1, 'onValidate') then
			ss.SetDelegate(guinv, @guinv^.onValidate, @_InventoryWindow_OnValidate, '');
		if ss.GetTableS(1, 'onDestroy') then
			ss.SetDelegate(guinv, @guinv^.onDestroy, @_InventoryWindow_OnDestroy, '');
		__common_create_window(ss, guinv^);
	end;

	procedure Script_InventoryWindow_Pick(var ss: ScriptState);
	var
		inv: pInventoryWindow;
		oitem: pOutdoorItem;
	begin
		inv := ss.ToSelf;
		oitem := ss.ToObject(2, TypeOf(OutdoorItem));
		inv^.Pick(oitem);
	end;

	procedure Script_InventoryWindow_ItemAt(var ss: ScriptState);
	begin
		ss.PushObject(pInventoryWindow(ss.ToSelf)^.ItemAt(ss.ToVec2(2)));
	end;

	procedure Script_CreateConsole(var ss: ScriptState);
	var
		con: pConsole;
	begin
		con := new(pConsole, Init(@ss));
		if ss.GetTableS(1, 'text') then
		begin
			__common_create_text(ss, con^._text, -1);
			ss.Pop;
		end;
		if ss.GetTableS(1, 'ekey') then
		begin
			con^.InputException := KeyboardKey(FindStr(ss.ToString(-1), KeyboardKeyIds, ord(con^.InputException)));
			ss.Pop;
		end;
		ss.PushObject(con);

		ss.PushTable;
		ss.Associate(con);

		__common_create_window(ss, con^);
		if ss.GetTableS(1, 'env') then
			if ss.IsFunction(-1) then
				ss.Associate(con, 1)
			else
				ss.Throw('Console.env — функция');
	end;

	procedure Script_Console_Write(var ss: ScriptState);
	var
		w: string;
		i: sint;
	begin
		w := '';
		for i := 2 to ss.Top do
			w += ss.ToString(i);
		pConsole(ss.ToSelf)^.Write(w);
	end;

	procedure Script_Console_Clear(var ss: ScriptState);
	begin
		pConsole(ss.ToSelf)^.Clear;
	end;

{$define fname:=Script_Console_active} {$define otype:=Console} {$define field:=active} {$define prop_bool} {$include script_prop.inc}

	procedure Script_Console_env(var ss: ScriptState; read: boolean);
	begin
		if read then
		begin
			if not ss.GetAssociated(ss.ToSelf, 0) then ss.PushNil;
		end else
		begin
			if not ss.IsTable(-1) then ss.Throw('ожидается таблица');
			ss.Associate(ss.ToSelf, 0);
		end;
	end;

	procedure OpenScript(var script: ScriptState);
	const
		Stuff: array[0 .. 59] of ScriptStuffDesc =
		(
			(s: TypeDesc; p: TypeOf(Control)),
			(s: 'assoc' + Writeable; p: @Script_handle_assoc),
			(s: 'attached'; p: @Script_Control_attached),
			(s: 'pos' + Writeable; p: @Script_Control_pos),
			(s: 'screenPos'; p: @Script_Control_screenPos),
			(s: 'sizes' + Writeable; p: @Script_Control_sizes),
			(s: 'material' + Writeable; p: @Script_Control_material),
			(s: 'memory' + Writeable; p: @Script_Control_memory),
			(s: 'onMouseEnter'; p: @Script_Control_onMouseEnter),
			(s: 'onMouseOver'; p: @Script_Control_onMouseOver),
			(s: 'onMouseLeave'; p: @Script_Control_onMouseLeave),
			(s: 'onMouseDown'; p: @Script_Control_onMouseDown),

			(s: 'GL'; p: @Script_Control_GL),
			(s: 'Attach:0'; p: @Script_Control_Attach),
			(s: 'Detach:0'; p: @Script_Control_Detach),
			(s: 'Move'; p: @Script_Control_Move),
			(s: 'SlideGL'; p: @Script_Control_SlideGL),
			(s: 'AddTimer'; p: @Script_AddTimer),
			(s: 'Slide'; p: @Script_Slide),

			(s: TypeDesc; p: TypeOf(ControlMove)),

			(s: TypeDesc; p: TypeOf(Text)),
			(s: 'text' + Writeable; p: @Script_Text_text),

			(s: TypeDesc; p: TypeOf(Image)),

			(s: TypeDesc; p: TypeOf(Button)),
			(s: 'onClick'; p: @Script_Button_onClick),

			(s: TypeDesc; p: TypeOf(IndicatorGroup)),
			(s: TypeDesc; p: TypeOf(GUIWindow)),

			(s: TypeDesc; p: TypeOf(GUIRoot)),
			(s: 'mouseFocus' + Writeable; p: @Script_GUI_mouseFocus),
			(s: 'floating'; p: @Script_GUI_floating),
			(s: 'mousePosN' + Writeable; p: @Script_GUI_mousePosN),
			(s: 'mousePos'; p: @Script_GUI_mousePos),

			(s: 'SetCursor:0'; p: @Script_GUI_SetCursor),
			(s: 'AbandonAnyFloat:0'; p: @Script_GUI_AbandonAnyFloat),
			(s: 'Forget:0'; p: @Script_GUI_Forget),
			(s: 'SkipFx:0'; p: @Script_GUI_SkipFx),
			(s: 'SkipFxBreakpoint:0'; p: @Script_GUI_SkipFxBreakpoint),

			(s: TypeDesc; p: TypeOf(Hint)),

			(s: TypeDesc; p: TypeOf(Minimap)),
			(s: 'target' + Writeable; p: @Script_Minimap_target),
			(s: 'scene' + Writeable; p: @Script_Minimap_scene),
			(s: 'shift' + Writeable; p: @Script_Minimap_shift),

			(s: TypeDesc; p: TypeOf(InventoryWindow)),
			(s: 'Pick:0'; p: @Script_InventoryWindow_Pick),
			(s: 'ItemAt:1'; p: @Script_InventoryWindow_ItemAt),

			(s: TypeDesc; p: TypeOf(Console)),
			(s: 'active' + Writeable; p: @Script_Console_active),
			(s: 'env' + Writeable; p: @Script_Console_env),

			(s: 'Write:0'; p: @Script_Console_Write),
			(s: 'Clear:0'; p: @Script_Console_Clear),

			(s: FunctionsDesc + 'CreateAtlas:1' + RequireEnv; p: @Script_CreateAtlas),
			(s: 'CreateGUIText:1' + RequireEnv; p: @Script_CreateText),
			(s: 'CreateGUIImage:1' + RequireEnv; p: @Script_CreateImage),
			(s: 'CreateGUIButton:1' + RequireEnv; p: @Script_CreateButton),
			(s: 'CreateGUIWindow:1' + RequireEnv; p: @Script_CreateWindow),
			(s: 'CreateGUIHint:1' + RequireEnv; p: @Script_CreateHint),
			(s: 'CreateGUIMinimap:1'; p: @Script_CreateMinimap),
			(s: 'CreateGUIIndicatorGroup:1' + RequireEnv; p: @Script_CreateIndicatorGroup),
			(s: 'CreateGUIInventory:1' + RequireEnv; p: @Script_CreateInventoryWindow),
			(s: 'CreateGUIConsole:1' + RequireEnv; p: @Script_CreateConsole)
		);
	begin
		script.AddStuff(Stuff);
	end;

type
	pGUIAttachGuard = ^GUIScriptGuard;
	GUIScriptGuard = object(&Object)
		gui: pGUI;
		ss: pScriptState;
		constructor Init(newGUI: pGUI; newSS: pScriptState);
	end;

	constructor GUIScriptGuard.Init(newGUI: pGUI; newSS: pScriptState);
	begin
		inherited Init;
		gui := newGUI;
		ss  := newSS;
		// Log('GUI guard activated!', logWarning);
	end;

	procedure _HandleStrongLinkToControl(ctl: pControl; link: boolean; var ss: ScriptState);
		function clear(ctl: pControl): boolean;
		begin
			result := ((not ctl^.Flag[gui_NonSerializable]) or InheritsFrom(TypeOf(ctl^), TypeOf(Hint)))
				and (not ctl^.Static) and ((not Assigned(ctl^.parent)) or clear(ctl^.parent));
		end;
	var
		ok: boolean;
	begin
		if not clear(ctl) then exit;
		if not ss.GetAssociated(ctl^._gui) then
		begin
		{$ifdef Debug} Log('Потерялась ссылка на GUI либо не вызвана RegisterGUI(script).', logError); {$endif}
			exit;
		end;
		if link then
		begin
			ok := ss.HasUV(ctl);
			if ok then
			begin
				ss.PushObject(ctl);
				if gui_NonSerializable in ctl^.flags then ss.DontSerialize(ctl);
			end;
		end else
			ok := ss.PushExisting(ctl);
		if ok then
		begin
			if link then ss.PushBool(yes) else ss.PushNil;
			ss.SetTable(-3);
		end;
		ss.Pop;
	end;

	procedure _GUIScriptAtDt(ctl: pControl; attach: boolean; const info: SingleDelegateInfo);
	var
		guard: pGUIAttachGuard absolute info.user;
	begin
		Assert(GUIRoot.OnAttachDetachProc(@_GUIScriptAtDt) = @_GUIScriptAtDt);
		Assert(pGUI(ctl^._gui) = guard^.gui, 'GUI не совпадают');
		_HandleStrongLinkToControl(ctl, attach, guard^.ss^);
	end;

	procedure _RemoveGUIAtDtGuardByScript(obj: pObject; param: pointer);
	var
		ss: pScriptState absolute obj;
		guard: pGUIAttachGuard absolute param;
	begin
	{$ifdef Debug} Log('GUI отвязан от скрипта ({0})', 'ScriptState.Done', logDebug); {$endif}
		Assert(pObject(ss) = obj);
		guard^.ss := nil;
		guard^.gui^.onAttachDetach.Remove(GUIRoot.OnAttachDetachProc(@_GUIScriptAtDt), guard);
	end;

	procedure _GUIAtDtRemoved(const info: SingleDelegateInfo);
	var
		guard: pGUIAttachGuard absolute info.user;
	begin
		if Assigned(guard^.ss) then
		begin
		{$ifdef Debug} Log('GUI отвязан от скрипта ({0})', 'MultiDelegate.Done', logDebug); {$endif}
			guard^.ss^.RemoveOnDestroyProc(@_RemoveGUIAtDtGuardByScript, guard);
		end;
		FreeWeak(guard);
	end;

	procedure RegisterGUI(gui: pGUI; var script: ScriptState);
	var
		guard: pGUIAttachGuard;
	begin
		new(guard, Init(gui, @script));
		gui^.onAttachDetach.Add(GUIRoot.OnAttachDetachProc(@_GUIScriptAtDt), guard, @_GUIAtDtRemoved);
		script.AddOnDestroyProc(@_RemoveGUIAtDtGuardByScript, guard);
		script.PushTable; script.Associate(gui);
	{$ifdef Debug} Log('GUI привязан к скрипту.', logDebug); {$endif}
	end;

{$ifdef use_serialization}
const
	PbsbBits = bitsizeof(uint8) div 4;
	PbsbMask = 1 shl PbsbBits - 1;

	function _pbsb_encode8(const pb: GUIPosBases; const sb: GUISizeBases): uint;
	begin
		result := ord(pb[0]) shl (PbsbBits * 3) or
		          ord(pb[1]) shl (PbsbBits * 2) or
		          ord(sb[0]) shl (PbsbBits * 1) or
		          ord(sb[1]);
	end;

	procedure _pbsb_decode8(t: uint; out pb: GUIPosBases; out sb: GUISizeBases);
	begin
		pb[0] := GUIPosBase(t shr (PbsbBits * 3));
		pb[1] := GUIPosBase(t shr (PbsbBits * 2) and PbsbMask);
		sb[0] := GUISizeBase(t shr (PbsbBits * 1) and PbsbMask);
		sb[1] := GUISizeBase(t and PbsbMask);
	end;

const
	CTL_EXTERNAL_IMAGE_BIT   = 1 shl 0;
	CTL_USER_BASES_BIT       = 1 shl 1;
	CTL_HAS_CHILDS_BIT       = 1 shl 2;
	CTL_HAS_GL_BIT           = 1 shl 3;
	CTL_HAS_ONMOUSEENTER_BIT = 1 shl 4;
	CTL_HAS_ONMOUSEOVER_BIT  = 1 shl 5;
	CTL_HAS_ONMOUSELEAVE_BIT = 1 shl 6;
	CTL_HAS_ONMOUSEDOWN_BIT  = 1 shl 7;
	CTL_HAS_ACTIONS_BIT      = 1 shl 8;
	CTL_HAS_MEMORY_BIT       = 1 shl 9;
	CTL_AUTOPOS_BIT          = 1 shl 10;
	CTL_HAS_ONHINT_BIT       = 1 shl 11;
	CTL_HAS_MATERIAL_BIT     = 1 shl 12;
	CTL_HAS_ONPRETTYDETACH_BIT = 1 shl 13;

const
	CtlFlag2Enum: array[ControlFlag] of sint = (CTL_EXTERNAL_IMAGE_BIT, -1, CTL_AUTOPOS_BIT);

	procedure SerializeControl(se: pSerializer; obj: pointer);
	var
		ctl: pControl absolute obj;
		flags: uint;
		flag: ControlFlag;
		i, nSeCh: sint;
	begin
		with se^ do
		begin
			Assert(not (gui_NonSerializable in ctl^.flags));
			flags := 0;
			for flag in ControlFlag do
				if (CtlFlag2Enum[flag] >= 0) and (flag in ctl^.flags) then flags := flags or uint(CtlFlag2Enum[flag]);
			if (ctl^.PositionBaseX <> pos_A) or (ctl^.PositionBaseY <> pos_A) or
				(ctl^.SizeBaseX <> size_Literal) or (ctl^.SizeBaseY <> size_Literal)
			then
				flags := flags or CTL_USER_BASES_BIT;
			nSeCh := ctl^.SerializableChildsCount;
			if nSeCh > 0 then flags := flags or CTL_HAS_CHILDS_BIT;
			if not ctl^.gl.Empty then flags := flags or CTL_HAS_GL_BIT;
			if not ctl^.onMouseEnter.Empty then flags := flags or CTL_HAS_ONMOUSEENTER_BIT;
			if not ctl^.onMouseOver.Empty then flags := flags or CTL_HAS_ONMOUSEOVER_BIT;
			if not ctl^.onMouseLeave.Empty then flags := flags or CTL_HAS_ONMOUSELEAVE_BIT;
			if not ctl^.onMouseDown.Empty then flags := flags or CTL_HAS_ONMOUSEDOWN_BIT;
			if not ctl^._actions.Empty then flags := flags or CTL_HAS_ACTIONS_BIT;
			if not ctl^._memoryId.Empty then flags := flags or CTL_HAS_MEMORY_BIT;
			if not ctl^.onHint.Empty then flags := flags or CTL_HAS_ONHINT_BIT;
			if Assigned(ctl^.material) and (not Assigned(ctl^._gui) or (ctl^._gui^.material <> ctl^.material) or (ctl^._gui = ctl)) then flags := flags or CTL_HAS_MATERIAL_BIT;
			if not ctl^.onPrettyDetach.Empty then flags := flags or CTL_HAS_ONPRETTYDETACH_BIT;
			Serialize_ui16(stream, flags);

			if (CTL_USER_BASES_BIT and flags) <> 0 then Serialize_ui8(stream, _pbsb_encode8(ctl^._posBase, ctl^._sizeBase));
			Serialize_vec2f16(stream, ctl^._pos);
			Serialize_vec2f16(stream, ctl^._sizes);
			if nSeCh > 0 then
			begin
				VarInt.Write(stream, nSeCh);
				for i := 0 to High(ctl^.childs) do
					if not (gui_NonSerializable in ctl^.childs[i]^.flags) then
						SeObject(ctl^.childs[i]);
			end;
			if not ctl^.gl.Empty then SeObject(@ctl^.gl, TypeOf(GLEntityParams));
			if (CTL_HAS_MATERIAL_BIT and flags) <> 0 then SeObject(ctl^.material);
			if not ctl^.onMouseEnter.Empty then SeObject(@ctl^.onMouseEnter, ObjType_MultiDelegate);
			if not ctl^.onMouseOver.Empty then SeObject(@ctl^.onMouseOver, ObjType_MultiDelegate);
			if not ctl^.onMouseLeave.Empty then SeObject(@ctl^.onMouseLeave, ObjType_MultiDelegate);
			if not ctl^.onMouseDown.Empty then SeObject(@ctl^.onMouseDown, ObjType_MultiDelegate);
			if (flags and CTL_HAS_ACTIONS_BIT) <> 0 then SeObject(@ctl^._actions, ObjType_EntityActions);
			if (flags and CTL_HAS_MEMORY_BIT) <> 0 then Serialize_string(stream, ctl^._memoryId);
			if (flags and CTL_HAS_ONHINT_BIT) <> 0 then SeObject(@ctl^.onHint, ObjType_MultiDelegate);
			if (flags and CTL_HAS_ONPRETTYDETACH_BIT) <> 0 then SeObject(@ctl^.onPrettyDetach, ObjType_MultiDelegate);
		end;
	end;

	procedure DeserializeControl(de: pDeserializer; obj: pointer);
	var
		ctl: pControl absolute obj;
		flags: uint;
		flag: ControlFlag;
		i: sint;
		gl: pGLEntityParams;
	begin
		with de^ do
		begin
			flags := Deserialize_ui16(stream);
			for flag in ControlFlag do
				if CtlFlag2Enum[flag] >= 0 then
					if (flags and CtlFlag2Enum[flag]) <> 0 then
						Include(ctl^._flags, flag)
					else
						Exclude(ctl^._flags, flag);

			if (CTL_USER_BASES_BIT and flags) <> 0 then
				_pbsb_decode8(Deserialize_ui8(stream), ctl^._posBase, ctl^._sizeBase);
			ctl^._pos := Deserialize_vec2f16(stream);
			ctl^._sizes := Deserialize_vec2f16(stream);
			if (CTL_HAS_CHILDS_BIT and flags) <> 0 then
			begin
				SetLength(ctl^.childs, VarInt.Read(stream));
				for i := 0 to High(ctl^.childs) do
					DeObjectA(ctl^.childs[i]);
			end;
			gl := new(pGLEntityParams, Init);
			Assert(sizeof(ctl^._clipRect) >= sizeof(gl));
			pGLEntityParams(pointer(@ctl^._clipRect)^) := gl;
			if (CTL_HAS_GL_BIT and flags) <> 0 then
			begin
				gl^.Done;
				DeWeakAtA(gl^);
			end;
			if (CTL_HAS_MATERIAL_BIT and flags) <> 0 then DeObjectR(ctl^._material);
			if (CTL_HAS_ONMOUSEENTER_BIT and flags) <> 0 then
			begin
				ctl^.onMouseEnter.Done;
				DeWeakAtR(ctl^.onMouseEnter);
			end;
			if (CTL_HAS_ONMOUSEOVER_BIT and flags) <> 0 then
			begin
				ctl^.onMouseOver.Done;
				DeWeakAtR(ctl^.onMouseOver);
			end;
			if (CTL_HAS_ONMOUSELEAVE_BIT and flags) <> 0 then
			begin
				ctl^.onMouseLeave.Done;
				DeWeakAtR(ctl^.onMouseLeave);
			end;
			if (CTL_HAS_ONMOUSEDOWN_BIT and flags) <> 0 then
			begin
				ctl^.onMouseDown.Done;
				DeWeakAtR(ctl^.onMouseDown);
			end;
			if (flags and CTL_HAS_ACTIONS_BIT) <> 0 then
			begin
				ctl^._actions.Done;
				DeWeakAtR(ctl^._actions);
			end;
			if (flags and CTL_HAS_MEMORY_BIT) <> 0 then ctl^._memoryId := Deserialize_string(stream);
			if (flags and CTL_HAS_ONHINT_BIT) <> 0 then
			begin
				ctl^.onHint.Done;
				DeWeakAtR(ctl^.onHint);
			end;
			if (flags and CTL_HAS_ONPRETTYDETACH_BIT) <> 0 then
			begin
				ctl^.onPrettyDetach.Done;
				DeWeakAtR(ctl^.onPrettyDetach);
			end;
		end;
	end;

	procedure ControlDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		ctl: pControl absolute obj;
		i: sint;
		gl: pGLEntityParams;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: ctl^.Init;
			de_After:
				begin
					for i := 0 to High(ctl^.childs) do
					begin
						ctl^.childs[i]^.parent := ctl;
						ctl^.childs[i]^._parentId := i;
					end;
					gl := pGLEntityParams(pointer(@ctl^._clipRect)^);
					if not gl^.Empty then
					begin
						ctl^.gl.Merge(gl^);
						de^.ChangeLink(gl, TypeOf(GLEntityParams), @ctl^.gl);
					end;
					dispose(gl, Done);
					ctl^._OnSetSizes;
				end;
		end;
	end;

	procedure ControlMoveDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointeR);
	var
		ac: pControlMove absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: ac^.DeseInit;
		end;
	end;

	procedure SerializeImage(se: pSerializer; obj: pointer);
	var
		img: pImage absolute obj;
	begin
		with se^ do
		begin
			SeObject(img^._im);
			SeObject(img^._activeAnim);
			Serialize_f16(stream, mm.GUITimeSince(img^._animStartTime));
		end;
	end;

	procedure DeserializeImage(de: pDeserializer; obj: pointer);
	var
		img: pImage absolute obj;
	begin
		with de^ do
		begin
			DeObjectR(img^._im);
			DeObjectR(img^._activeAnim);
			img^._animStartTime := mm.GUITime - Deserialize_f16(stream);
		end;
	end;

	procedure ImageDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		img: pImage absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: img^.Init;
			de_After2: // After занят анимациями
				begin
					img^._UpdateIm;
					img^._UpdateActiveAnim;
				end;
		end;
	end;

	procedure SerializeText(se: pSerializer; obj: pointer);
	var
		text: pText absolute obj;
	begin
		with se^ do
		begin
			Serialize_f16(stream, text^.scale);
			SeObject(text^.font);
			Serialize_string(stream, text^.text);
			Serialize_f16(stream, text^.MaxWidth);
			Serialize_f16(stream, text^.ParaIdent);
		end;
	end;

	procedure DeserializeText(de: pDeserializer; obj: pointer);
	var
		text: pText absolute obj;
	begin
		with de^ do
		begin
			text^._scale := Deserialize_f16(stream);
			DeObjectR(text^._font);
			text^._text := Deserialize_string(stream);
			text^._maxWidth := Deserialize_f16(stream);
			text^._paraIdent := Deserialize_f16(stream);
			text^._changed := yes;
		end;
	end;

	procedure TextDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		text: pText absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: text^.Init;
			de_After:
				begin
					text^._UpdateScale;
					text^._UpdateFont;
					text^._UpdateText;
				end;
		end;
	end;

	procedure SerializeButton(se: pSerializer; obj: pointer);
	var
		button: pButton absolute obj;
	begin
		with se^ do
		begin
			Assert(not button^._pressed);
			SeObject(button^._pressedAnims[no]);
			SeObject(button^._pressedAnims[yes]);
			SeObject(@button^.onClick, ObjType_MultiDelegate);
		end;
	end;

	procedure DeserializeButton(de: pDeserializer; obj: pointer);
	var
		button: pButton absolute obj;
	begin
		with de^ do
		begin
			DeObjectR(button^._pressedAnims[no]);
			DeObjectR(button^._pressedAnims[yes]);
			DeWeakAtR(button^.onClick);
		end;
	end;

	procedure ButtonDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		button: pButton absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize:
				begin
					button^.Init(nil, nil);
					button^.onClick.Done;
				end;
		end;
	end;

	procedure SerializeIndicatorGroup(se: pSerializer; obj: pointer);
	var
		group: pIndicatorGroup absolute obj;
		i, j: sint;
	begin
		with se^ do
		begin
			Serialize_ui8(stream, length(group^._ids));
			for i := 0 to High(group^._ids) do
				with group^._ids[i] do
				begin
					Serialize_string(stream, namae);
					Serialize_ui8(stream, length(anims));
					for j := 0 to High(anims) do
						SeObject(anims[j]);
					Serialize_ui8(stream, uint(reversed));
				end;
			Serialize_ui8(stream, length(group^._bindings));
			for i := 0 to High(group^._bindings) do
				SeObject(group^._bindings[i]);
			SeObject(@group^.onCreateControl, ObjType_MultiDelegate);
		end;
	end;

	procedure DeserializeIndicatorGroup(de: pDeserializer; obj: pointer);
	var
		group: pIndicatorGroup absolute obj;
		i, j: sint;
	begin
		with de^ do
		begin
			SetLength(group^._ids, Deserialize_ui8(stream));
			for i := 0 to High(group^._ids) do
				with group^._ids[i] do
				begin
					namae := Deserialize_string(stream);
					SetLength(anims, Deserialize_ui8(stream));
					for j := 0 to High(anims) do
						DeObjectA(anims[j]);
					reversed := Deserialize_ui8(stream) > 0;
				end;
			SetLength(group^._bindings, Deserialize_ui8(stream));
			for i := 0 to High(group^._bindings) do
				DeWeakA(group^._bindings[i]);
			DeWeakAtR(group^.onCreateControl);
		end;
	end;

	procedure IndicatorGroupDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		group: pIndicatorGroup absolute obj;
		i: sint;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: group^.Init;
			de_After:
				for i := 0 to High(group^._bindings) do
					group^._bindings[i]^.group := group;
		end;
	end;

	procedure SerializeIndicatorBinding(se: pSerializer; obj: pointer);
	var
		bind: IndicatorGroup.pBinding absolute obj;
	begin
		with se^ do
		begin
			Serialize_ui8(stream, bind^.indicatorId);
			SeFunction(bind^.onUnbind);
			SeObject(bind^.onUnbindParam);
		end;
	end;

	procedure DeserializeIndicatorBinding(de: pDeserializer; obj: pointer);
	var
		bind: IndicatorGroup.pBinding absolute obj;
	begin
		with de^ do
		begin
			bind^.indicatorId := Deserialize_ui8(stream);
			pointer(bind^.onUnbind) := DeFunction;
			DeWeakR(bind^.onUnbindParam);
		end;
	end;

	procedure IndicatorBindingDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		bind: IndicatorGroup.pBinding absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: bind^.Init(nil, 0, nil, nil);
		end;
	end;

const
	WIN_DRAGGABLE_BIT = 1 shl 0;
	WIN_SIZEABLE_BIT  = 1 shl 1;
	WIN_SKIP_ON_CLICK_BIT  = 1 shl 2;

	procedure SerializeWindow(se: pSerializer; obj: pointer);
	var
		win: pGUIWindow absolute obj;
		flags: uint;
	begin
		with se^ do
		begin
			flags := 0;
			if win^.Draggable then flags := flags or WIN_DRAGGABLE_BIT;
			if win^.Sizeable then flags := flags or WIN_SIZEABLE_BIT;
			if win^.SkipOnClick then flags := flags or WIN_SKIP_ON_CLICK_BIT;
			Serialize_ui8(stream, flags);
			if win^.Sizeable then
			begin
				Serialize_vec2f16(stream, win^._minSizes);
				Serialize_vec2f16(stream, win^._maxSizes);
			end;
		end;
	end;

	procedure DeserializeWindow(de: pDeserializer; obj: pointer);
	var
		win: pGUIWindow absolute obj;
		flags: uint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			win^._draggable := (flags and WIN_DRAGGABLE_BIT) <> 0;
			win^._sizeable := (flags and WIN_SIZEABLE_BIT) <> 0;
			if win^._sizeable then
			begin
				win^._minSizes := Deserialize_vec2f16(stream);
				win^._maxSizes := Deserialize_vec2f16(stream);
			end;
			win^._skipOnClick := (flags and WIN_SKIP_ON_CLICK_BIT <> 0);
		end;
	end;

	procedure WindowDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		win: pGUIWindow absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: win^.Init;
		end;
	end;

const
	GUI_MOUSE_ENABLED_BIT = 1 shl 0;
	GUI_HAS_MEMORY_BIT    = 1 shl 1;
	GUI_HAS_ONATDT_BIT    = 1 shl 2;
	GUI_MEM_POS_BIT = 1 shl 0;
	GUI_MEM_SZ_BIT  = 1 shl 1;

	procedure SerializeGUI(se: pSerializer; obj: pointer);
	var
		gui: pGUI absolute obj;
		flags, flags2: uint;
		mem: pMemory;
		mi: pMemoryItem;
		it: Memory.tHash_ID2Item.Iterator;
	begin
		with se^ do
		begin
			mem := @gui^.memory;
			flags := 0;
			if gui^._mouseEnabled then flags := flags or GUI_MOUSE_ENABLED_BIT;
			if mem^._mem.Count > 0 then flags := flags or GUI_HAS_MEMORY_BIT;
			if not gui^.onAttachDetach.Empty then flags := flags or GUI_HAS_ONATDT_BIT;
			Serialize_ui8(stream, flags);

			Assert(length(gui^._mouseTracking) = 0);
			Assert(length(gui^._float) = 0);
			SeObject(gui^._baseCursor.image);
			Serialize_vec2f16(stream, gui^._baseCursor.spot);

			if (flags and GUI_HAS_MEMORY_BIT) <> 0 then
			begin
				Serialize_ui8(stream, mem^._mem.Count);
				it := mem^._mem.GetIterator;
				while mem^._mem.Next(it) do
				begin
					Serialize_string(stream, mem^._mem.GetKey(it)^);
					mi := mem^._mem.GetValue(it);
					flags2 := 0;
					if gui_RememberPos in mi^.what then flags2 := flags2 or GUI_MEM_POS_BIT;
					if gui_RememberSize in mi^.what then flags2 := flags2 or GUI_MEM_SZ_BIT;
					Serialize_ui8(stream, flags2);
					if (flags2 and GUI_MEM_POS_BIT) <> 0 then
					begin
						Serialize_ui8(stream, _pbsb_encode8(mi^.posBase, Control.DefaultSizeBases));
						Serialize_vec2f16(stream, mi^.pos);
					end;
					if (flags2 and GUI_MEM_SZ_BIT) <> 0 then
					begin
						Serialize_ui8(stream, _pbsb_encode8(Control.DefaultPosBases, mi^.sizeBase));
						Serialize_vec2f16(stream, mi^.sizes);
					end;
				end;
			end;
			if (flags and GUI_HAS_ONATDT_BIT) <> 0 then SeObject(@gui^.onAttachDetach, ObjType_MultiDelegate);
		end;
	end;

	procedure DeserializeGUI(de: pDeserializer; obj: pointer);
	var
		gui: pGUI absolute obj;
		ci: string;
		mi: MemoryItem;
		flags, flags2: uint;
		i: sint;
		tpb: GUIPosBases;
		tsb: GUISizeBases;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			gui^._mouseEnabled := (flags and GUI_MOUSE_ENABLED_BIT) <> 0;
			DeObjectR(gui^._baseCursor.image);
			gui^._baseCursor.spot := Deserialize_vec2f16(stream);

			if (flags and GUI_HAS_MEMORY_BIT) <> 0 then
			begin
				for i := 0 to sint(Deserialize_ui8(stream)) - 1 do
				begin
					ci := Deserialize_string(stream);
					flags2 := Deserialize_ui8(stream);
					mi.what := [];
					if (flags2 and GUI_MEM_POS_BIT) <> 0 then
					begin
						mi.what += [gui_RememberPos];
						_pbsb_decode8(Deserialize_ui8(stream), mi.posBase, tsb);
						mi.pos := Deserialize_vec2f16(stream);
					end;
					if (flags2 and GUI_MEM_SZ_BIT) <> 0 then
					begin
						mi.what += [gui_RememberSize];
						_pbsb_decode8(Deserialize_ui8(stream), tpb, mi.sizeBase);
						mi.sizes := Deserialize_vec2f16(stream);
					end;
					gui^.memory._mem.Add(ci, mi);
				end;
			end;
			if (flags and GUI_HAS_ONATDT_BIT) <> 0 then
			begin
				gui^.onAttachDetach.Done;
				DeWeakAtR(gui^.onAttachDetach);
			end;
		end;
	end;

	procedure GUISeSpecial(se: pSerializer; what: SeSpecial; obj: pointer);
	var
		gui: pGUI absolute obj;
	begin
		Assert(@se = @se);
		case what of
			se_Before: if gui^.MouseEnabled then gui^._HandleMouseEnabled(no);
			se_After: if gui^.MouseEnabled then gui^._HandleMouseEnabled(yes);
		end;
	end;

	procedure GUIDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		gui: pGUI absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize:
				begin
					gui^.Init(MainRT.inGL);
					gui^._BeforeDetach;
				end;
			de_After2:
				begin
					gui^._AfterAttach;
					if gui^._mouseEnabled then gui^._HandleMouseEnabled(yes);
				end;
		end;
	end;

	procedure SerializeGUIAttachGuard(se: pSerializer; obj: pointer);
	var
		guard: pGUIAttachGuard absolute obj;
	begin
		with se^ do
		begin
			SeObject(guard^.gui);
			SeObject(guard^.ss);
		end;
	end;

	procedure DeserializeGUIAttachGuard(de: pDeserializer; obj: pointer);
	var
		guard: pGUIAttachGuard absolute obj;
	begin
		with de^ do
		begin
			DeWeakR(guard^.gui);
			DeWeakR(guard^.ss);
		end;
	end;

	procedure GUIAttachGuardDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		guard: pGUIAttachGuard absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: guard^.DeseInit;
			de_After: guard^.ss^.AddOnDestroyProc(@_RemoveGUIAtDtGuardByScript, guard);
		end;
	end;

	procedure SerializeMinimap(se: pSerializer; obj: pointer);
	var
		minimap: pMinimap absolute obj;
	begin
		with se^ do
		begin
			SeObject(minimap^.scene);
			SeObject(minimap^.target);
			Serialize_vec3f16(stream, minimap^.shift);
			Serialize_vec4N8(stream, minimap^._op.GetRT^.ClearColor, Vec4.Zero, Vec4.Ones);
		end;
	end;

	procedure DeserializeMinimap(de: pDeserializer; obj: pointer);
	var
		minimap: pMinimap absolute obj;
		c4: Vec4;
	begin
		with de^ do
		begin
			DeWeakR(minimap^._scene);
			DeWeakR(minimap^._target);
			minimap^._shift := Deserialize_vec3f16(stream);
			c4 := Deserialize_vec4N8(stream, Vec4.Zero, Vec4.Ones);
			minimap^._baseColor := c4.xyz;
			minimap^._baseAlpha := c4.w;
		end;
	end;

	procedure MinimapDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		minimap: pMinimap absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: minimap^.DeseInit;
			de_After: minimap^._Initialize;
		end;
	end;

const
	INV_TILE_BIT       = 1 shl 0;
	INV_HAS_ONDROP_BIT = 1 shl 1;
	INV_HAS_ONVALIDATE_BIT = 1 shl 2;
	INV_HAS_ONDESTROY_BIT  = 1 shl 3;

	procedure SerializeInventoryWindow(se: pSerializer; obj: pointer);
	var
		inv: pInventoryWindow absolute obj;
		flags: uint;
	begin
		with se^ do
		begin
			flags := 0;
			if inv^._tile then flags := flags or INV_TILE_BIT;
			if not inv^.onDrop.Empty then flags := flags or INV_HAS_ONDROP_BIT;
			if not inv^.onValidate.Empty then flags := flags or INV_HAS_ONVALIDATE_BIT;
			if not inv^.onDestroy.Empty then flags := flags or INV_HAS_ONDESTROY_BIT;
			Serialize_ui8(stream, flags);

			SeObject(inv^._inv);
			Serialize_f16(stream, inv^._cellSize);
			Serialize_vec4f16(stream, inv^._relBorder);
			if not inv^.onDrop.Empty then SeObject(@inv^.onDrop, ObjType_MultiDelegate);
			if not inv^.onValidate.Empty then SeObject(@inv^.onValidate, ObjType_MultiDelegate);
			if (flags and INV_HAS_ONDESTROY_BIT) <> 0 then SeObject(@inv^.onDestroy, ObjType_MultiDelegate);
		end;
	end;

	procedure DeserializeInventoryWindow(de: pDeserializer; obj: pointer);
	var
		inv: pInventoryWindow absolute obj;
		flags: uint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			inv^._tile := (flags and INV_TILE_BIT) <> 0;
			DeObjectR(inv^._inv);
			inv^._cellSize := Deserialize_f16(stream);
			inv^._relBorder := Deserialize_vec4f16(stream);
			if (flags and INV_HAS_ONDROP_BIT) <> 0 then DeWeakAtR(inv^.onDrop) else inv^.onDrop.Init;
			if (flags and INV_HAS_ONDROP_BIT) <> 0 then DeWeakAtR(inv^.onValidate) else inv^.onValidate.Init;
			if (flags and INV_HAS_ONDESTROY_BIT) <> 0 then DeWeakAtR(inv^.onDestroy) else inv^.onDestroy.Init;
		end;
	end;

	procedure InventoryWindowSeSpecial(se: pSerializer; what: SeSpecial; obj: pointer);
	var
		inv: pInventoryWindow absolute obj;
	begin
		Assert(@se = @se);
		case what of
			se_Before: if Assigned(inv^._gui) then inv^._DisableRuntime;
			se_After: if Assigned(inv^._gui) then inv^._EnableRuntime;
		end;
	end;

	procedure InventoryWindowDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		inv: pInventoryWindow absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: inv^.DeseInit;
			de_After2: inv^._Initialize; // After занят инвентарём
		end;
	end;
{$endif}

	function LoadFont(s: pStream): pObject; begin result := new(pFont, Init(s)); end;

	procedure Init;
	begin
		ResourcePool.Shared^.Register(TypeOf(Font), @LoadFont)^.Tag(GLResourceTag);
	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('GUI control', TypeOf(Control), nil, sizeof(Control), yes,
			@SerializeControl, @DeserializeControl, nil, @ControlDeSpecial)
		^.RegisterFuncs([@_Control_OnMouseEnterLeave, @_Control_OnMouseOver, @_Control_OnMouseDown, @_Control_OnHint])
		^.RegisterType('GUI control move', TypeOf(ControlMove), TypeOf(Slide), sizeof(ControlMove), yes,
			nil, nil, nil, @ControlMoveDeSpecial)
		^.RegisterType('GUI image', TypeOf(Image), TypeOf(Control), sizeof(Image), yes,
			@SerializeImage, @DeserializeImage, nil, @ImageDeSpecial)
		^.RegisterFunc(@RelocateControlGLValue)
		^.RegisterType('Font', TypeOf(Font), nil, sizeof(Font), yes, nil, nil, nil, nil)
		^.RegisterType('GUI text', TypeOf(Text), TypeOf(Control), sizeof(Text), yes,
			@SerializeText, @DeserializeText, nil, @TextDeSpecial)
		^.RegisterType('GUI button', TypeOf(Button), TypeOf(Image), sizeof(Button), yes,
			@SerializeButton, @DeserializeButton, nil, @ButtonDeSpecial)
		^.RegisterFunc(@_Button_OnClick)
		^.RegisterType('GUI indicator group', TypeOf(IndicatorGroup), TypeOf(Control), sizeof(IndicatorGroup), yes,
			@SerializeIndicatorGroup, @DeserializeIndicatorGroup, nil, @IndicatorGroupDeSpecial)
		^.RegisterType('GUI indicator binding', TypeOf(IndicatorGroup.Binding), nil, sizeof(IndicatorGroup.Binding), yes,
			@SerializeIndicatorBinding, @DeserializeIndicatorBinding, nil, @IndicatorBindingDeSpecial)
		^.RegisterFunc(@OnCreateIndicatorGroupControl)
		^.RegisterType('GUI window', TypeOf(GUIWindow), TypeOf(Image), sizeof(GUIWindow), yes,
			@SerializeWindow, @DeserializeWindow, nil, @WindowDeSpecial)
		^.RegisterType('GUI hint', TypeOf(Hint), TypeOf(GUIWindow), sizeof(Hint), yes, nil, nil, nil, nil)
		^.RegisterType('GUI root', TypeOf(GUIRoot), TypeOf(GUIWindow), sizeof(GUIRoot), yes, @SerializeGUI, @DeserializeGUI, @GUISeSpecial, @GUIDeSpecial)
		^.RegisterFuncs([@_GUIScriptAtDt, @_GUIAtDtRemoved])
		^.RegisterType('GUI script guard', TypeOf(GUIScriptGuard), nil, sizeof(GUIScriptGuard), yes,
			@SerializeGUIAttachGuard, @DeserializeGUIAttachGuard, nil, @GUIAttachGuardDeSpecial)
		^.RegisterType('Minimap', TypeOf(Minimap), TypeOf(GUIWindow), sizeof(Minimap), yes,
			@SerializeMinimap, @DeserializeMinimap, nil, @MinimapDeSpecial)
		^.RegisterType('GUI inventory', TypeOf(InventoryWindow), TypeOf(GUIWindow), sizeof(InventoryWindow), yes,
			@SerializeInventoryWindow, @DeserializeInventoryWindow, @InventoryWindowSeSpecial, @InventoryWindowDeSpecial)
		^.RegisterFuncs([@_InventoryWindow_OnDrop, @_InventoryWindow_OnValidate, @_InventoryWindow_OnDestroy]);
	{$endif}
	end;

initialization
	&Unit('GUI').Initialize(@Init);
end.
