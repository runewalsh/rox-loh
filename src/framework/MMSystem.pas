unit MMSystem;

{$include opts.inc}
{-$define DebugSoapifyTime}

interface

uses
	USystem, UClasses, UMath, Windowing, Streams, Script, OpenGL_Impl, GUI, Audio, Utils, Input, Scene, Human, GLClasses, PNG
{$ifdef Debug}, ULog, Errors {$endif};

type
	pMultimediaSystem = ^MultimediaSystem;
	MultimediaSystem = object(&Object)
	const
		SceneTimeCycle = 8000.0;
		GUITimeCycle = 720.0;
		FPSCountingPeriod = 0.01;
		MinFPSCountingFrames = 3;
		MaxFrameDt = 0.07;
		DefaultFPSLimit = 60.0;

		MMStrongLink = '_MM_';
		GUIStrongLink = '_GUI_';
		PreviewSignature = EOL + 'preview :3' + EOL;
		NoPreviewSignature = '/no preview/';
	private var
		_frameNo: sint;
		_sceneTime, _guiTime: float;
		_curFramesCount: uint;
		_frameStart, _periodStart, _minFrameTime: Ticks;
		_fps, _frameDt: float;
		_updateScene: boolean;
	{$ifdef Debug} _fpsLogTimeout: float; {$endif}
		procedure _SetFPSLimit(newFPSLimit: float);
	public
		frameDynamics: ValueDynamics;
		window: Window;
		GUI: pGUI;
		mouse: MouseInput;
		keyboard: KeyboardInput;
		gamepad: GamepadInput;
		script: pScriptState;
		scriptEntry: string;
		bgm: MusicPlayer;
		scene: pScene;
		locale: Human.Locale;
		constructor Init(const configFileName: string = '');
		destructor Done; virtual;
		procedure Run;
		function BeginFrame: boolean;
		procedure EndFrame;
		function StunUnlessActive: boolean;
		function SceneTimeSince(const start: float): float;
		function GUITimeSince(const start: float): float;

		procedure ChangeScene(newScene: pScene);
		procedure Pause;
		procedure Unpause;
		function Paused: boolean;
	{$ifdef use_serialization}
		procedure Save(const fn: string; extra: sint);
		function Load(const fn: string): boolean;
	{$endif}
	public
		property FrameNo: sint read _frameNo;
		property SceneTime: float read _sceneTime;
		property GUITime: float read _guiTime;
		property FrameDt: float read _frameDt;
		property FPSLimit: float write _SetFPSLimit;

	private
		procedure _InitializeScript(const ep: string);
		procedure Bookkeep;
	{$ifdef use_serialization}
		function ValidatePreviewSize(const size: UintVec2): boolean;
		procedure SavePreviewSync(f: pStream);
		function LoadPreview(f: pStream): pTexture;
	{$endif}
	end;

	procedure OpenScript(var script: ScriptState);

var
	Config: record
		previewSize: UintVec2;
		allowMT: Tribool;
	end =
	(
		previewSize: (data: (200, 150));
		allowMT: (value: 0)
	);

	mm: MultimediaSystem;
	gl: tOpenGL;

implementation

uses
	U_GL, GLBase, Cameras, Script_EngineAPI
{$ifdef use_serialization}, GLUtils, Serialization {$endif}
{$ifdef Profile}, Profile {$endif};

	function StartLoadResource(const stream: string; param: pointer): pointer;
	type
		Cookie = WindowCaption.Cookie; {$if sizeof(Cookie) > sizeof(pointer)} {$error don't fits} {$endif}
	var
		mm: pMultimediaSystem absolute param;
		uid: WindowCaption.Cookie absolute result;
	begin
		uid := mm^.window.caption.SetNote('загрузка: ' + StreamPath.Human(stream));
	end;

	procedure EndLoadResource(result, param: pointer);
	var
		mm: pMultimediaSystem absolute param;
		uid: WindowCaption.Cookie absolute result;
	begin
		mm^.window.caption.RemoveNote(uid);
	end;

type
	pConfigState = ^ConfigState;
	ConfigState = object
		ss: pScriptState;
		nOpts: uint;
		opts: array[0 .. 3] of StringView;
		procedure Init(ss: pScriptState);
		procedure Push(const opt: StringView);
	end;

	procedure ConfigState.Init(ss: pScriptState);
	begin
		self.ss    := ss;
		nOpts      := 0;
	end;

	function GetNestedItem(id: uint; cfg: pConfigState): string;
	begin
		if id >= cfg^.nOpts then result := cfg^.ss^.ToString(2) else result := cfg^.opts[id].ToString;
	end;

	procedure ConfigState.Push(const opt: StringView);
	begin
		if nOpts >= High(opts) then ss^.Throw(Format('{0}: превышена вложенность параметров.',
		                                             SeparatedList.Join(nOpts + 1, GetIndexedString(@GetNestedItem), @self, '.')));
		inc(nOpts); opts[nOpts - 1] := opt;
	end;

	// IndexConfig срабатывает на строках конфига вида "phys.A = B" (в примере у окружения будет запрошен ключ 'phys').
	// Возвращает само окружение (хак).
	// Если возможно, спускается на уровень вниз, т. е. в
	// script.                      <- здесь вложенность станет script
	// 	entry = "scripts/main"    <- script.[newindex]
	// 	gc.                       <- здесь — script, gc, т. к. script содержит вложенный элемент
	// 		pause = 100            <- script.gc.[newindex]
	// gamepad.xxx = yyy            <- здесь — gamepad, т. к. такого вложенного в script.gc или script нет
	//
	// Используются StringView — предполагается, что строки не уничтожатся GC, т. к. уже содержатся в пуле констант исходника.
	// Возможно, это неверно.
	procedure IndexConfig(var ss: ScriptState);
	var
		c: pConfigState;
		newItem: StringView;
	begin
		c := ss.ToData(ScriptState.EnvIndex.ParamUpvalue);
		newItem := ss.ToStringView(2);
		c^.Push(newItem);
		while (c^.nOpts > 0) and (Config2.Apply(ss, Slice(c^.opts, c^.nOpts), Config2.TEST_IDX) <> Config2.Intermediate) do
		begin
			if c^.nOpts <= 1 then ss.Throw('Неизвестная опция {0}.', ss.ToString(2));
			dec(c^.nOpts);
			c^.opts[c^.nOpts - 1] := newItem;
		end;
		ss.PushCopy(1);
	end;

	procedure NewIndexConfig(var ss: ScriptState);
	var
		c: pConfigState;
	begin
		c := ss.ToData(ScriptState.EnvIndex.ParamUpvalue);
		c^.Push(ss.ToStringView(2));
		if Config2.Apply(ss, Slice(c^.opts, c^.nOpts), 3) <> Config2.Leaf then ss.Throw('Неизвестная опция {0}.', ss.ToString(2));
		dec(c^.nOpts);
	end;

	constructor MultimediaSystem.Init(const configFileName: string = '');
	var
		i: sint;
		cfgIndex: ScriptState.EnvIndex;
		cfgState: ConfigState;
	begin
		inherited Init;
		MakeStatic;
		_curFramesCount := 0;
		_frameStart := Ticks.Get;
		_fps := 0.0;
		_frameDt := 0.01;
		_frameNo := -1;
		_sceneTime := 0.0;
		_guiTime := 0.0;
		_minFrameTime := Ticks.Zero;
		FPSLimit := DefaultFPSLimit;
		gui := nil;
		mouse.Init;
		keyboard.Init;
		gamepad.Init;
		frameDynamics.Init;
		window.Initialize;

	{$ifdef Debug} Log('Размеры экрана: ' + ToString(ScreenSize)); {$endif}

		bgm.Init;
		gl.Init;
		scriptEntry := '';
		script := MakeRef(new(pScriptState, Init));
		locale.Init(GetSystemLanguage(DefaultLanguage), script);
		scene := nil;
		_updateScene := yes;

		if Assigned(script) then
		begin
			Script_EngineAPI.OpenScript(script^);
			if configFileName <> '' then
			begin
				// Хак: глобальная таблица GL конфликтует с опцией конфига GL. >_<
				script^.GetGlobal('GL'); script^.PushNil; script^.SetGlobal('GL');

				script^.PushTable; script^.PushSint(0); script^.SetTableS(-2, 'auto');
				cfgState.Init(script);
				cfgIndex := ScriptState.EnvIndex.Make(@IndexConfig, @NewIndexConfig, @cfgState);
				script^.DoFile(configFileName, no, 0, 0, -1, @cfgIndex);
				script^.SetGlobal('GL');
			end;
		end;
	{$ifdef use_serialization} SerializationDB.Shared^.AddEnv([@self, script]); {$endif}

		if GLBase.Config.forceGLver <> '' then
		begin
			i := 1;
			window.glMajor := StrToInt(ScanToken(GLBase.Config.forceGLver, i, ['0'..'9']), 1);
			window.glMinor := StrToInt(ScanToken(GLBase.Config.forceGLver, i, ['0'..'9']));
		end;

		if not window.Open then raise Error('');
		window.caption.suffix := '[загрузка...]';
		ResourcePool.Shared^.SetCallbacks(@StartLoadResource, @EndLoadResource, @self);

		if not gl.InitContext(UintVec2.Make(window.sizeX, window.sizeY)) then raise Error('');

		gui := MakeRef(new(pGUI, Init(MainRT.inGL)));
	{$ifdef Debug} _fpsLogTimeout := 5.0; {$endif}

		if Assigned(script) and (scriptEntry <> '') then _InitializeScript(scriptEntry);
	end;

	destructor MultimediaSystem.Done;
	begin
		ResourcePool.Shared^.ResetCallbacks;
		ResourcePool.Shared^.Deactivate(GLResourceTag);
		GlobalGL.values.ForceCleanup;
	{$ifdef use_serialization} if Assigned(script) then SerializationDB.Shared^.RemEnv([script, @self]); {$endif}
		ChangeScene(nil);
		Release(script);
		Release(gui);

		locale.Done;
		gl.Done;
		bgm.Done;
		mouse.Done;
		keyboard.Done;
		gamepad.Done;
		frameDynamics.Done;
		window.Finalize;
		inherited Done;
	end;

type
	ScriptMainOptions = record
		useFunction: boolean;
		functionName: string;
	end;

	procedure HandleScriptMainOption(id: uint; const value: StringView; param: pointer);
	begin
		case id of
			0: begin ScriptMainOptions(param^).useFunction := yes; ScriptMainOptions(param^).functionName := value.ToString; end;
		{$ifdef Debug} else raise ExhaustiveCase(id, 'Script.Main.option'); {$endif}
		end;
	end;

	procedure MultimediaSystem._InitializeScript(const ep: string);

		procedure Prepare;
		begin
			script^.PushObject(@self); script^.SetGlobal(MMStrongLink);
			script^.PushObject(gui); script^.SetGlobal(GUIStrongLink);
			script^.PushObject(@self);
			RegisterGUI(gui, script^);
		end;

		function ExtractReadOnlyFolder(const fn: string): string;
		var
			i: sint;
		begin
			i := Pos(FileSeparator, fn);
			if i > 0 then result := Copy(fn, 1, i) else result := fn;
		end;

	var
		filename: string;
		opts: ScriptMainOptions;
	begin
		opts.useFunction := no;
		filename := StringOptionals.Split(ep, [':'], @HandleScriptMainOption, @opts).ToString;
		MarkAsReadOnly(ExtractReadOnlyFolder(filename));

		if not opts.useFunction then Prepare;
		if script^.DoFile(filename, opts.useFunction, uint(not opts.useFunction), uint(not opts.useFunction)) >= 0 then
		begin
			if opts.useFunction then
			begin
				script^.ForceGetTableS(-1, opts.functionName);
				script^.Remove(-2);
				if script^.IsFunction(-1) then
				begin
					Prepare;
					script^.Call(1, 1);
				end;
			end;

			if script^.Typ(-1) = script_Object then ChangeScene(script^.ToObject(-1, TypeOf(SceneRoot)));
			script^.Pop;
		{$ifdef Debug} if not Assigned(scene) then Log('Скриптовая точка входа должна задать функцию F(mm), возвращающую корень сцены', logWarning); {$endif}
		end;
	end;

	procedure MultimediaSystem.Run;
	var
		proceed: boolean;
		osc: pScene;
		ogui: pGUI;
	begin
		if (not Assigned(scene)) or (not Assigned(script)) then exit;
		repeat
			if not BeginFrame then break;
			bgm.Process(scene^.Camera{, FrameDt});

			repeat
				osc := scene;
				ogui := gui;
				script^.PushObject(scene);
				script^.PushObject(gui);
				if _updateScene then scene^.Update(FrameDt);
				GUI^.Update(MainRT.inGL, FrameDt);
				if scene <> osc then
				begin
					dec(_frameNo);
					scene^.Update(FrameDt);
					inc(_frameNo);
				end;
				if gui <> ogui then ogui^.Neutralize;

				proceed := scene = osc;
				if proceed then
				begin
					scene^.Render(scene^.Camera, @MainScenario, 1.0, nil, nil, [GLbuffer_Color, GLbuffer_Depth]);
					GUI^.Draw(MainRT.inGL);
				end;
				script^.Pop(2);
			until proceed;
			EndFrame;
			if not window.IsActive then
				if not StunUnlessActive then break;
		until no;
	end;

	function MultimediaSystem.BeginFrame: boolean;
	var
		cpuFull, cpuKernel, cpuUser: float;
		t: Ticks;
	begin
		t := Ticks.Get;
		if _curFramesCount = 0 then _periodStart := t;
		Inc(_curFramesCount);
		_frameStart := t;
	{$push} {$overflowchecks off} inc(_frameNo); {$pop}

		if FrameNo > 0 then
		begin
			GetCPULoad(cpuFull, cpuKernel, cpuUser);
			window.caption.suffix := '[FPS: ' + ToString(frameDynamics.AveragePerSec, FloatFormat.MaxAfterPoint(1)) + ']' +
				'   [CPU: ' + ToString(cpuFull * 100, FloatFormat.MaxAfterPoint(0)) + '% = '
				+ 'K:' + ToString(cpuKernel * 100, FloatFormat.MaxAfterPoint(0)) + '% + '
				+ 'U:' + ToString(cpuUser * 100, FloatFormat.MaxAfterPoint(0)) + '%]';
		end else
			window.caption.suffix := '[компиляция шейдеров и всё такое...]';

		result := window.Process(@keyboard, @mouse, @gamepad);
		if not result then exit;
		MainRT.Resize(UintVec2.Make(window.sizeX, window.sizeY));

		mouse.Update;
		window.CenterMouse;
		keyboard.Update;
		gamepad.Update;

		gl.BeginFrame;
		result := yes;
	end;

	procedure MultimediaSystem.EndFrame;
	var
		frameTime, curTime, ndt: Ticks;
		dTime, newFrameDt: float;
	begin
		gl.EndFrame(frameNo);
		Bookkeep;

		frameTime := Ticks.Get - _frameStart;
		if (_minFrameTime <> Ticks.Zero) and (frameTime < _minFrameTime) then
			Thread.Sleep(_minFrameTime - frameTime);

		frameDynamics.NewValue(1, frameNo);
		curTime := Ticks.Get;
		frameTime := curTime - _frameStart;
		dTime := min(frameTime.ToSeconds, MaxFrameDt);
		if _updateScene then
			_sceneTime := modf(_sceneTime + dTime, SceneTimeCycle);
		_guiTime := modf(_guiTime + dTime, GUITimeCycle);

		ndt := curTime - _periodStart;
		if (ndt >= Ticks.FromSeconds(FPSCountingPeriod)) and (_curFramesCount >= MinFPSCountingFrames) then
		begin
			_fps := _curFramesCount / ndt.ToSeconds;
			_curFramesCount := 0;
		end;
		if NotZero(_fps) then
			newFrameDt := UMath.min(1.0 / _fps, MaxFrameDt)
		else
			newFrameDt := MaxFrameDt;
		if (IsZero(_frameDt)) or (abs(newFrameDt/_frameDt - 1.0) < 0.05) then
			_frameDt := newFrameDt
		else
			_frameDt := _frameDt * 0.7 + newFrameDt * 0.3;

	{$ifdef Debug}
		_fpsLogTimeout -= _frameDt;
		if _fpsLogTimeout <= 0.0 then
		begin
			LogR('[FPS: ' + ToString(frameDynamics.AveragePerSec) + ']; ', logDebug);
			_fpsLogTimeout := 5.0;
		end;
	{$endif}
	end;

	procedure MultimediaSystem.Bookkeep;
	begin
		script^.Bookkeep;
	end;

	procedure MultimediaSystem._SetFPSLimit(newFPSLimit: float);
	begin
		if newFPSLimit > 0.0 then
		begin
			_minFrameTime := Ticks.FromSeconds(1.0 / newFPSLimit);
		{$ifdef Debug} Log('Ограничение FPS: {0} ({1})', ToString(newFPSLimit), lang_amount(_minFrameTime.Internal, '{N} тик{/а/ов}'), logDebug); {$endif}
		end else
		begin
			_minFrameTime := Ticks.Zero;
		{$ifdef Debug} Log('Ограничение FPS: нет', logDebug); {$endif}
		end;
	end;

	function MultimediaSystem.StunUnlessActive: boolean;
	begin
		result := yes;
		Sound.GlobalPause;
		window.caption.suffix := '[окно не активно]';
		repeat
			if not window.Process(@keyboard, @mouse, @gamepad) then
			begin
				result := no;
				break;
			end;
			Thread.Sleep(333);
			Bookkeep;
		until window.IsActive;
		window.CenterMouse;
		Sound.GlobalResume;
	end;

{$define impl :=
	begin
		if time >= start then
			result := time - start
		else
			result := (cycle - start) + time;
	end; {$undef time} {$undef cycle}}
	function MultimediaSystem.SceneTimeSince(const start: float): float; {$define time := SceneTime} {$define cycle := SceneTimeCycle} impl
	function MultimediaSystem.GUITimeSince(const start: float): float; {$define time := GUITime} {$define cycle := GUITimeCycle} impl
{$undef impl}

	procedure MultimediaSystem.ChangeScene(newScene: pScene);
	begin
		if scene <> newScene then
		begin
			if Assigned(scene) then scene^.Neutralize;
			SetRef(scene, newScene);
		end;
	end;

	procedure MultimediaSystem.Pause;
	begin
		_updateScene := no;
	end;

	procedure MultimediaSystem.Unpause;
	begin
		_updateScene := yes;
	end;

	function MultimediaSystem.Paused: boolean;
	begin
		result := not _updateScene;
	end;

{$ifdef use_serialization}
	function MultimediaSystem.ValidatePreviewSize(const size: UintVec2): boolean;
	const
		MaxAspect = 3;
	begin
		result := (size.x >= 16) and (size.x <= 768) and (size.y >= 16) and (size.y <= 768) and (size.x < size.y * MaxAspect) and (size.y < size.x * MaxAspect);
	end;

	procedure SaveByteSizeBeforeData(f: pStream; size: size_t; param: pointer);
	begin
		Assert(@param = @param);
		VarInt.Write(f, size);
	end;

type
	GetSoapifyRadiusParam = record
		twor, isx, isy: float;
	end;

	function GetSoapifyRadius(x, y: uint; param: pointer): float;
	var
		p: ^GetSoapifyRadiusParam absolute param;
	begin
		result := p^.twor * (sqr(x*p^.isx - 0.5) + sqr(y*p^.isy - 0.5));
	end;

	procedure MultimediaSystem.SavePreviewSync(f: pStream);
	var
		size: UintVec2;
		p: GetSoapifyRadiusParam;
		rt: pRenderTarget;
		chunk: pointer;
	{$ifdef DebugSoapifyTime} t: Ticks; {$endif}
	begin
		size := Config.previewSize;
		if not ValidatePreviewSize(size) then
		begin
			Serialize_conststring(f, NoPreviewSignature);
			exit;
		end;
		rt := new(pRenderTarget, Init('preview', GLtexture_2D, [GLformat_RGB], size, rt_ExclusiveDepth));
		rt^.Prepare;

		scene^.camera.ChangeRT(rt);
		scene^.Render(scene^.Camera, @MainScenario);
		GUI^.Draw(rt^.inGL);
		scene^.camera.ChangeRT(@MainRT);

	{$ifdef Debug} LogR('Получаю превьюшку ' + ToString(size.x) + ' x ' + ToString(size.y) + '... '); {$endif}
		chunk := gl.GetRTImage(rt^.inGL, GLformat_RGB);
		if Assigned(chunk) then
		begin
		{$ifdef DebugSoapifyTime} t := Ticks.Get; {$endif}
			p.twor := 2.0 * min(size.x, size.y) / 8; p.isx := 1/size.x; p.isy := 1/size.y;
			// Filters.MirrorY.Create.DestructiveApply(sx, sy, Glformat_RGB, chunk, chunk); SavePNG('t1.png', sx, sy, GLformat_RGB, chunk);
			Filters.Soapify.Create(@GetSoapifyRadius, @p).DestructiveApply(size, GLformat_RGB, chunk, chunk);
		{$ifdef DebugSoapifyTime} t := t.Elapsed; Info.Show('Soapify: ' + TimeToString(t)); {$endif}
			// SavePNG('t2.png', sx, sy, GLformat_RGB, chunk); Filters.MirrorY.Create.DestructiveApply(sx, sy, Glformat_RGB, chunk, chunk);
		{$ifdef Debug} Log('OK', logOk); {$endif}
		end else
		begin
			size := size.Zero;
		{$ifdef Debug} Log('Fail', logError); {$endif}
		end;
		dispose(rt, Done);

		Serialize_conststring(f, MultimediaSystem.PreviewSignature);
		if size <> size.Zero then
			PNG.Save(f, size, GLformat_RGB, chunk, Fast, @SaveByteSizeBeforeData, nil)
		else
			VarInt.Write(f, 0);
		FreeMem(chunk);
	end;

	function MultimediaSystem.LoadPreview(f: pStream): pTexture;
	var
		size: size_t;
	begin
		result := nil;
		if not Deserialize_signature(f, PreviewSignature, yes) then
		begin
			Deserialize_signature(f, NoPreviewSignature, no);
			exit;
		end;
		size := VarInt.Read(f);
		if size <> 0 then
		begin
		{$ifdef Debug} LogR('Загружаю превьюшку... '); {$endif}
			result := new(pTexture, Init(f, [texture_Flipped], 'png', size));
		end {$ifdef Debug} else LogR('Превьюшки нет; ', logWarning) {$endif};
	end;

type
	SavePreviewTask = object
		instance: Task;
		mm: pMultimediaSystem;
      f: pStream;
      procedure Run;
	end;

	procedure SavePreviewTask.Run;
	begin
		mm^.SavePreviewSync(f);
	end;

	procedure RunSavePreviewTask(param: pointer);
	begin
		SavePreviewTask(param^).Run;
	end;

	procedure MultimediaSystem.Save(const fn: string; extra: sint);
	var
		se: pSerializer;
		hasuv: boolean;
		tmp, f: pStream;
		savePreview: SavePreviewTask;
	begin
		if not Assigned(script) then raise Error('Для сохранения нужно состояние скрипта.');
		tmp := FileStream.CreateTemp(fn);
		se := nil;
		f := nil;

		try
			try
				se := new(pSerializer, Init(fn, GetExecVersion));
				f := se^.stream^.NewRef; se^.ChangeStream(tmp);
				savePreview.mm := @self;
				savePreview.f := f;
				Work.Queue(savePreview.instance, @RunSavePreviewTask, @savePreview, [Task.HardWork]);

				try
					if extra <> 0 then extra := script^.AbsIdx(extra);
					if not script^.GetAssociated(@self) then script^.PushNil; // assoc
					hasuv := script^.KillUV(@self);                           // assoc UV
					script^.PushTable;                                        // assoc UV sav
					script^.PushObject(scene); script^.SetTableI(-2, 0);
					script^.PushObject(gui); script^.SetTableI(-2, 1);
					script^.PushCopy(-3); script^.SetTableI(-2, 2);
					if (extra <> 0) and (script^.ToBool(extra)) then script^.PushCopy(extra) else script^.PushBool(yes);
					script^.SetTableI(-2, 3);
					script^.Serialize(-1, se);
					script^.Pop; // assoc UV

					bgm.Save(se^.stream);
					if hasuv then Script^.RestoreUV(@self); // assoc
					script^.Pop;
				finally
					savePreview.instance.Close;
				end;
				f^.AppendFrom(tmp, FilePos.Zero, tmp^.Size);
			finally
				if Assigned(se) then dispose(se, Done);
            Release(f);
				Release(tmp);
			end;
		except
			&File.Erase(fn);
			raise;
		end;
	end;

	function MultimediaSystem.Load(const fn: string): boolean;
	var
		de: pDeserializer;
		tex: pTexture;
	begin
		result := no;
		if not Assigned(script) then exit;
		de := new(pDeserializer, Init(fn, GetExecVersion)); if not Assigned(de) then exit;
		tex := MakeRef(LoadPreview(de^.stream));

		if script^.Deserialize(de) then
		begin
			if script^.IsTable(-1) then
			begin
				script^.GetTableI(-1, 0); ChangeScene(script^.ToObject(-1, TypeOf(SceneRoot))); script^.Pop;
				script^.GetTableI(-1, 1); SetRef(gui, script^.ToObject(-1, TypeOf(GUIRoot))); Script^.PushObject(gui); Script^.SetGlobal(GUIStrongLink); script^.Pop;
				script^.GetTableI(-1, 2); script^.Associate(@self);
				script^.GetTableI(-1, 3);
				script^.Remove(-2);
				if Assigned(tex) and (script^.IsTable(-1)) then
				begin
					script^.PushObject(tex); script^.SetTableS(-2, 'preview');
				end
			{$ifdef Debug} else Log('Я бы записал текстуру с превьюшкой в поле "preview" пользовательской скриптовой таблицы, но её в сохранении нет. =(') {$endif};
				result := yes;
			end else
			begin
			{$ifdef Debug} Log('Загруженный из сейва скриптовый объект не является таблицей', logError); {$endif}
				script^.Pop;
			end;
		end;
		Release(tex);
		bgm.Restore(de^.stream);
		dispose(de, Done);
	end;
{$endif}

	procedure _MouseOnMove(const delta: Vec2; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(MouseInput.OnMoveProc(@_MouseOnMove) = @_MouseOnMove);
		if not sd^.GetFunction {$ifdef Debug}('[tMouseInput] OnMove'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushVec2(delta);
			Call(1, 0);
		end;
	end;

	procedure _MouseOnScroll(const delta: float; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(MouseInput.OnScrollProc(@_MouseOnScroll) = @_MouseOnScroll);
		if not sd^.GetFunction {$ifdef Debug}('[tMouseInput] OnScroll'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushFloat(delta);
			Call(1, 0);
		end;
	end;

	procedure _MouseOnButton(button: MouseButton; ev: ButtonEvent; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(MouseInput.OnButtonProc(@_MouseOnButton) = @_MouseOnButton);
		Assert((@button = @button) and (@ev = @ev));
		if not sd^.GetFunction {$ifdef Debug}('[tMouseInput] OnButton'){$endif} then exit;
		with sd^.ss^ do
		begin
			Call(0, 0);
		end;
	end;

	procedure _KeyboardOnKey(key: KeyboardKey; ev: ButtonEvent; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(KeyboardInput.tOnKeyProc(@_KeyboardOnKey) = @_KeyboardOnKey);
		Assert((@key = @key) and (@ev = @ev));
		if not sd^.GetFunction {$ifdef Debug}('[tKeyboardInput] OnKey'){$endif} then exit;
		with sd^.ss^ do
		begin
			Call(0, 0);
		end;
	end;

	procedure _GamepadOnButton(button: GamepadInput.Button; ev: ButtonEvent; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(GamepadInput.OnButtonProc(@_GamepadOnButton) = @_GamepadOnButton);
		Assert((@button = @button) and (@ev = @ev));
		if not sd^.GetFunction {$ifdef Debug}('GamepadInput.OnButton'){$endif} then exit;
		with sd^.ss^ do
		begin
			Call(0, 0);
		end;
	end;

	procedure _GamepadOnStickProcess(stick: GamepadInput.Stick; const state: GamepadInput.StickState; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(GamepadInput.OnStickProcessProc(@_GamepadOnStickProcess) = @_GamepadOnStickProcess);
		Assert(@stick = @stick);
		if not sd^.GetFunction {$ifdef Debug}('GamepadInput.OnStickProcess'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushVec2(state);
			PushFloat(mm.FrameDt);
			Call(2, 0);
		end;
	end;

	procedure Script_mm_SetHandlers(var ss: ScriptState);
	var
		mm: pMultimediaSystem;
		b: MouseButton;
		k: KeyboardKey;
		bev: ButtonEvent;
		gb: GamepadInput.Button;
		stick: GamepadInput.Stick;
		deid, s: string;
		id: sint;
	begin
		mm := ss.ToSelf;
		deid := ss.GetStringField(2, 'id', '');

		if ss.GetTableS(2, 'mouse') then
		begin
			if ss.GetTableS(-1, 'move') then
				ss.SetDelegate(mm, @mm^.mouse.onMove, @_MouseOnMove, deid);
			if ss.GetTableS(-1, 'scroll') then
				ss.SetDelegate(mm, @mm^.mouse.onScroll, @_MouseOnScroll, deid);
			for b in MouseButton do
				if ss.GetTableS(-1, MouseInput.ButtonIds[b]) then
					if ss.IsTable(-1) then
					begin
						for bev in ButtonEvent do
							if ss.GetTableS(-1, ButtonEventIds[bev]) then
								ss.SetDelegate(mm, @mm^.mouse.buttonEvents[b, bev], @_MouseOnButton, deid);
						ss.Pop;
					end else
						ss.SetDelegate(mm, @mm^.mouse.buttonEvents[b, button_Click], @_MouseOnButton, deid);
			ss.Pop;
		end;
		if ss.GetTableS(2, 'keyboard') then
		begin
			ss.PushNil;
			while ss.Next(-2) do
			begin
				s := ss.ToString(-2);
				id := FindStr(s, KeyboardKeyIds);
				if id >= 0 then
				begin
					k := KeyboardKey(id);
					if ss.IsTable(-1) then
					begin
						for bev in ButtonEvent do
							if ss.GetTableS(-1, ButtonEventIds[bev]) then
								ss.SetDelegate(mm, @mm^.keyboard.keyEvents[k, bev], @_KeyboardOnKey, deid);
					end else
					begin
						ss.PushCopy(-1);
						ss.SetDelegate(mm, @mm^.keyboard.keyEvents[k, button_Click], @_KeyboardOnKey, deid);
					end;
				end else
					ss.UnknownIdentifier(s);
				ss.Pop;
			end;
			ss.Pop;
		end;
		if ss.GetTableS(2, 'gamepad') then
		begin
			ss.PushNil;
			while ss.Next(-2) do
			begin
				s := ss.ToString(-2);
				if s = 'buttons' then
				begin
					ss.PushNil;
					while ss.Next(-2) do
					begin
						gb := GamepadInput.Button(FindStr(ss.ToString(-2), GamepadInput.ButtonIds, ord(btn_Select)));
						if ss.IsTable(-1) then
						begin
							for bev in ButtonEvent do
								if ss.GetTableS(-1, ButtonEventIds[bev]) then
									ss.SetDelegate(mm, @mm^.gamepad.buttons[gb].events[bev], @_GamepadOnButton, deid);
						end else
						begin
							ss.PushCopy(-1);
							ss.SetDelegate(mm, @mm^.gamepad.buttons[gb].events[button_Click], @_GamepadOnButton, deid);
						end;
						ss.Pop;
					end;
				end else
				if s = 'sticks' then
				begin
					ss.PushNil;
					while ss.Next(-2) do
					begin
						stick := GamepadInput.Stick(FindStr(ss.ToString(-2), GamepadInput.StickIds, ord(LeftStick)));
						if ss.GetTableS(-1, 'onProcess') then
							ss.SetDelegate(mm, @mm^.gamepad.sticks[stick].onProcess, @_GamepadOnStickProcess, deid);
						ss.Pop;
					end;
				end else
					ss.UnknownIdentifier(s);
				ss.Pop;
			end;
		end;
	end;

	procedure Script_mm_WipeHandlers(var ss: ScriptState);
	var
		name: string;
		{$ifdef Debug} n: sint; {$endif}

		procedure Wipe(var h: MultiDelegate);
		begin
			if Assigned(h.FindNamed(name)) then
				{$ifdef Debug} if {$endif} h.RemoveNamed(name) {$ifdef Debug} then inc(n) {$endif};
		end;

	var
		mm: pMultimediaSystem;
		b: MouseButton;
		k: KeyboardKey;
		bev: ButtonEvent;
		gb: GamepadInput.Button;
		stick: GamepadInput.Stick;
	begin
		mm := ss.ToSelf;
		name := ss.ToString(2);
	{$ifdef Debug}
		LogR('Убираю обработчики событий по ID: "' + name + '"... ');
		n := 0;
	{$endif}
		Wipe(mm^.mouse.onMove);
		Wipe(mm^.mouse.onScroll);

		for b in MouseButton do
			for bev in ButtonEvent do
				Wipe(mm^.mouse.buttonEvents[b, bev]);

		for k in KeyboardKey do
			for bev in ButtonEvent do
				Wipe(mm^.keyboard.keyEvents[k, bev]);

		for gb in GamepadInput.Button do
			for bev in ButtonEvent do
				Wipe(mm^.gamepad.buttons[gb].events[bev]);

		for stick in GamepadInput.Stick do
			Wipe(mm^.gamepad.sticks[stick].onProcess);

	{$ifdef Debug} Log('Удалено обработчиков: ' + ToString(n)); {$endif}
	end;

{$define fname := Script_mm_fullscreen} {$define otype:=MultimediaSystem} {$define field := window.Fullscreen} {$define prop_bool}
{$include script_prop.inc}

	procedure Script_mm_CenterifyWindow(var ss: ScriptState);
	begin
		pMultimediaSystem(ss.ToSelf)^.window.Centerify;
	end;

	procedure Script_mm_Quit(var ss: ScriptState);
	begin
		pMultimediaSystem(ss.ToSelf)^.window.SendQuitSignal;
	end;

	procedure Script_mm_ChangeScene(var ss: ScriptState);
	begin
		pMultimediaSystem(ss.ToSelf)^.ChangeScene(ss.ToObject(2, TypeOf(SceneRoot)));
	end;

	procedure Script_mm_Pause(var ss: ScriptState);
	begin
		pMultimediaSystem(ss.ToSelf)^.Pause;
	end;

	procedure Script_mm_Unpause(var ss: ScriptState);
	begin
		pMultimediaSystem(ss.ToSelf)^.Unpause;
	end;

	procedure Script_mm_windowSizes(var ss: ScriptState);
	var
		mm: pMultimediaSystem;
	begin
		mm := ss.ToSelf;
		ss.PushVec2(Vec2.Make(mm^.window.sizeX, mm^.window.sizeY));
	end;

	procedure Script_mm_gui(var ss: ScriptState; read: boolean);
	begin
		if read then
			ss.PushObject(pMultimediaSystem(ss.ToSelf)^.gui)
		else
			SetRef(pMultimediaSystem(ss.ToSelf)^.gui, ss.ToObject(3, TypeOf(GUIRoot)));
	end;

	procedure OnOpenCloseBGM(var cam: Camera; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(MusicPlayer.OnOpenCloseProc(@OnOpenCloseBGM) = @OnOpenCloseBGM);
		if not sd^.GetFunction {$ifdef Debug}('OnOpenCloseBGM'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(@cam);
			Call(1, 0);
		end;
	end;

	procedure OnProcessBGM(var au: Sound; var cam: Camera; const info: SingleDelegateInfo);
	var
		sd: pScriptDelegate absolute info.user;
	begin
		Assert(MusicPlayer.OnProcessProc(@OnProcessBGM) = @OnProcessBGM);
		if not sd^.GetFunction {$ifdef Debug}('OnProcessBGM'){$endif} then exit;
		with sd^.ss^ do
		begin
			PushObject(@au);
			PushObject(@cam);
			Call(2, 0);
		end;
	end;

	procedure Script_LoadBGM(var ss: ScriptState);
	var
		mm: pMultimediaSystem;
		theme: MusicPlayer.ThemeProxy;
		song: MusicPlayer.ItemProxy;
		name: string;
	begin
		try
			mm := ss.ToObject(1, TypeOf(MultimediaSystem));
			ss.PushNil;
			while ss.Next(2) do
			begin
				theme := mm^.bgm.AddTheme(ss.ToString(-2));

				ss.PushNil;
				while ss.Next(-2) do
				begin
					name := ss.ToString(-2);
					if name = 'fadeout' then theme.FadeoutTime(ss.ToFloat(-1)) else
						case ss.Typ(-1) of
							script_Table:
								begin
									song := theme.AddItem(name, ss.ToStream(ss.GetStringField(-1, 1)));
									if ss.GetTableS(-1, 'volume') then begin song.Volume(ss.ToFloat(-1)); ss.Pop; end;
									if ss.GetTableS(-1, 'effect') then
									begin
										if ss.GetTableS(-1, 'open') then ss.SetDelegate(mm, song.OnOpen, @OnOpenCloseBGM, '');
										if ss.GetTableS(-1, 'process') then ss.SetDelegate(mm, song.OnProcess, @OnProcessBGM, '');
										if ss.GetTableS(-1, 'close') then ss.SetDelegate(mm, song.OnClose, @OnOpenCloseBGM, '');
										ss.Pop;
									end;
								end;
							script_String: theme.AddItem(name, ss.ToStream(-1));
							else ss.Throw(ss.ToString(-1) + ' — ожидается "файл" или {файл, параметры}.');
						end;
					ss.Pop;
				end;
				ss.Pop;
			end;
		except
			ss.Throw(Exception.Message);
		end;
	end;

	procedure Script_mm_BGM_switch(var ss: ScriptState);
	begin
		pMultimediaSystem(ss.ToSelf)^.bgm.Switch;
	end;

	function Script_mm_BGM_theme(var ss: ScriptState): sint;
	var
		mm: pMultimediaSystem;
		priority: pModifiableValue;
		active: string;
	begin
		mm := ss.ToSelf;
		if ss.Top > 1 then
		begin
			priority := mm^.bgm.Priority(ss.ToString(2));
			if Assigned(priority) then
				Script_modifiable(ss, 3, priority^);
			result := 0;
		end else
		begin
			active := mm^.bgm.ActiveTheme;
			if active <> '' then
				ss.PushString(active)
			else
				ss.PushNil;
			result := 1;
		end;
	end;

	procedure Script_mm_BGM_rewind(var ss: ScriptState);
	begin
		pMultimediaSystem(ss.ToSelf)^.bgm.Rewind(ss.ToFloat(2));
	end;

	function Script_mm_BGM_track(var ss: ScriptState): sint;
	var
		name: string;
		time, total: float;
	begin
		if pMultimediaSystem(ss.ToSelf)^.bgm.CurrentTrack(@name, @time, @total) then
		begin
			ss.PushString(name);
			ss.PushFloat(time);
			ss.PushFloat(total);
			result := 3;
		end else
			result := 0;
	end;

{$define fname:=Script_mm_language} {$define otype:=MultimediaSystem} {$define field:=locale.language} {$define prop_string}
{$include script_prop.inc}

{$define fname:=Script_mm_localizationPath} {$define otype:=MultimediaSystem} {$define field:=locale.FsBase} {$define prop_stream}
{$include script_prop.inc}

{$ifdef use_serialization}
	procedure Script_mm_Save(var ss: ScriptState);
	var
		extra: sint;
	begin
		if ss.Top > 2 then extra := 3 else extra := 0;
		pMultimediaSystem(ss.ToSelf)^.Save(ss.ToStream(2), extra);
	end;

	procedure Script_mm_Load(var ss: ScriptState);
	begin
		if not pMultimediasystem(ss.ToSelf)^.Load(ss.ToStream(2)) then ss.PushNil;
	end;

	function Script_ValidateSave(var ss: ScriptState): sint;
		function Error(const msg: string): sint;
		begin
			ss.PushBool(no);
			ss.PushString(msg);
			result := 2;
		end;
	var
		stream: string;
		de: pDeserializer;
		tex: pTexture;
	begin
		stream := ss.ToStream(1);
		if not &File.Exists(stream) then exit(Error('N/A'));
		try
			de := new(pDeserializer, Init(stream, GetExecVersion, yes));
			try
				ss.PushBool(yes);
				ss.PushTable;
				ss.PushString(Format(ToString(de^.date, '{D} {0} {Y}, {h}:{m}:{s}'), Locale.Localized('misc.months.' + Months[de^.date.month].abbrev + '.gen')));
				ss.SetTableS(-2, 'timestamp');
				tex := mm.LoadPreview(de^.stream);
				if Assigned(tex) then
				begin
					ss.PushObject(tex);
					ss.SetTableS(-2, 'preview');
				end;
				result := 2;
			finally
				dispose(de, Done);
			end;
		except
			exit(Error(IfThen(Exception.Current is Deserializer.VersionMismatch, 'ver', 'corrupted')));
		end;
	end;

	procedure Script_mm_previewAspect(var ss: ScriptState);
	var
		aspect: float;
	begin
		if pMultimediaSystem(ss.ToSelf)^.ValidatePreviewSize(Config.previewSize) then
			aspect := Config.previewSize.Aspect
		else
			aspect := 4/3;
		ss.PushFloat(aspect);
	end;
{$endif}

	procedure OpenScript(var script: ScriptState);
	const
		Stuff: array[0 .. 18 {$ifdef use_serialization} +4 {$endif}] of ScriptStuffDesc =
		(
			(s: TypeDesc; p: TypeOf(MultimediaSystem)),
			(s: 'windowSizes'; p: @Script_mm_windowSizes),
			(s: 'fullscreen' + Writeable; p: @Script_mm_fullscreen),
			(s: 'gui' + Writeable; p: @Script_mm_gui),
			(s: 'assoc' + Writeable; p: @Script_handle_assoc),
			(s: 'language' + Writeable; p: @Script_mm_language),
			(s: 'localizationPath' + Writeable; p: @Script_mm_localizationPath),

			(s: 'SetHandlers:0'; p: @Script_mm_SetHandlers),
			(s: 'WipeHandlers:0'; p: @Script_mm_WipeHandlers),
			(s: 'CenterifyWindow:0'; p: @Script_mm_CenterifyWindow),
			(s: 'Quit:0'; p: @Script_mm_Quit),
			(s: 'ChangeScene:0'; p: @Script_mm_ChangeScene),
			(s: 'Pause:0'; p: @Script_mm_Pause),
			(s: 'Unpause:0'; p: @Script_mm_Unpause),
		{$ifdef use_serialization}
			(s: 'Save:0'; p: @Script_mm_Save),
			(s: 'Load:1'; p: @Script_mm_Load),
			(s: 'previewAspect'; p: @Script_mm_previewAspect),
		{$endif}
			(s: 'BGM_switch:0'; p: @Script_mm_BGM_switch),
			(s: 'BGM_theme'; p: @Script_mm_BGM_theme),
			(s: 'BGM_rewind:0'; p: @Script_mm_BGM_rewind),
			(s: 'BGM_track'; p: @Script_mm_BGM_track),

			(s: FunctionsDesc + 'LoadBGM:0' + RequireEnv; p: @Script_LoadBGM)
		{$ifdef use_serialization}, (s: 'ValidateSave' + RequireEnv; p: @Script_ValidateSave) {$endif}
		);
	begin
		script.AddStuff(Stuff);
	end;

	procedure Init;
	begin
	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Multimedia system', TypeOf(MultimediaSystem), nil, sizeof(MultimediaSystem), yes, nil, nil, nil, nil)
		^.RegisterFuncs([@_MouseOnMove, @_MouseOnScroll, @_MouseOnButton, @_KeyboardOnKey]);
	{$endif}
	end;

initialization
	&Unit('MMSystem').Initialize(@Init);
end.
