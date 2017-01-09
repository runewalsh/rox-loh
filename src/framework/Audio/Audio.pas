unit Audio;

{$include opts.inc}
{$ifdef windows}
	{$define basscall:=stdcall}
{$else}
	{$define basscall:=cdecl}
{$endif}

interface

uses
	ctypes, Errors, BassLib, DynamicLoader, USystem, Streams, UMath, Random, Utils, UClasses,
	SceneGraph, Script, Physics
{$ifdef Debug}, ULog, Human{$endif};

type
	SoundFont = object
		handle: Midi.HSOUNDFONT;
		image: pStreamImage;
		function Load(out sf: SoundFont; const fn: string): boolean; static;
		procedure StartUnloading;
		procedure EndUnloading;
	{$ifdef Debug} procedure LogInfo; {$endif}
	end;

	pSound = ^Sound;
	SoundWatch = object
		interfaceLock, timerLock: ThreadLock;
		timer: ThreadTimer;
		timerBoosted, stopTimer: boolean;
		playing: array of pSound;
		procedure Init;
		procedure Done;
		procedure Lock(full: boolean);
		procedure Unlock(full: boolean);
		procedure UnlockedAdd(snd: pSound);
		procedure Remove(snd: pSound);
		procedure UnlockedRemove(snd: pSound; id: uint);
		function ShouldBoostFor(snd: pSound): boolean;
	const
		Timeouts: array[boolean] of uint = (20000, 2000); // или выставлять точный таймаут в зависимости от времени фейда или проигрывания...
	end;

	Sound = object(&Object)
	type
		scoped_enum_ Flag = (Looped, Use3D, StartPaused, DontThrow, _BassLoaded, _FadingForever, _Dummy); _end
		FlagSet = set of Flag;
		scoped_enum_ State = (Playing, Paused, Stalled, Stopped); _end

		constructor Init(const stream: string; flags: FlagSet);
		destructor Done; virtual;
		procedure Play;
		procedure Pause;
		procedure Stop;
		function QueryState: State;
		procedure Fade(const time, targetVolume: float; lock: boolean = yes);
		procedure FadeOut(const time: float);
		function Amplitude: float;
		procedure Spectre(n: uint; freqs: pFloat; ranges: pFloat; amps: pFloat);

	private type
		pSample = ^SampleType;
		SampleType = object(&Object)
			h2d, h3d: Bass.HSAMPLE;
			stream: string;
			constructor Init(const stream: string);
			destructor Done; virtual;
			function GetChannel(use3d: boolean): Bass.HCHANNEL;

		private
			procedure LoadOne(out h: Bass.HSAMPLE; use3d: boolean);
			procedure DestroySample(h: Bass.HSAMPLE {$ifdef Debug}; const sampleKind: string = '' {$endif});
		const
			FAILED_HANDLE = High(Bass.HSAMPLE);
		end;

		SampleGetChannelAborted = class(Exception) end;

		p3DParams = ^_3DParams;
		_3DParams = object
		type
			FieldEnum = (
				LocationSet,   OrientationSet,   VelocitySet,   DistancesSet,   ConeSet,   OutVolSet,
				LocationDirty, OrientationDirty, VelocityDirty, DistancesDirty, ConeDirty, OutVolDirty);
			FieldSet = set of FieldEnum;
		var
			self: pSound;
			mask: FieldSet;
			location, orientation, velocity: Vec3;
			minDist, maxDist: float;
			inAngle, outAngle: float;
			outVol: float;
		end;

	var
		h: BASS.Handle;
		flags: FlagSet;
		sample: pSample;
		stream: pStream;
		_3d: p3DParams;
		fadeTargetVolume: float; // -1, если не актуально
		watchId: uint; // not uint(0), если не отслеживается
	{$ifdef Debug} name: string; {$endif}
		procedure InternalInit(const stream: string; flags: FlagSet);
		procedure InternalDone(fromWatch: boolean);
		procedure InternalStop(fromWatch: boolean);
		function SourceStream: string;
		function PreferSample(const stream: string): boolean; static;
		function BytesToSeconds(const bytes: qword): cfloat;
		function SecondsToBytes(const sec: cfloat): qword;
		function GetDuration: float;
		function GetVolume: float;   procedure SetVolume(const volume: float);
		function GetPosition: float; procedure SetPosition(const position: float);
	public
		property Volume: float read GetVolume write SetVolume;
		property Position: float read GetPosition write SetPosition;
		property Duration: float read GetDuration;
	const
		Looped      = Flag.Looped;
		Use3D       = Flag.Use3D;
		StartPaused = Flag.StartPaused;
		DontThrow   = Flag.DontThrow;

		Playing     = State.Playing;
		Paused      = State.Paused;
		Stalled     = State.Stalled;
		Stopped     = State.Stopped;

	type
		SpaceProxy = object
			_3d: p3DParams;
			function Location(const loc: Vec3): SpaceProxy;
			function Orientation(const orie: Vec3): SpaceProxy;
			function Velocity(const vel: Vec3): SpaceProxy;
			function Distances(const min, max: float): SpaceProxy;
			function Cone(const inAngle, outAngle: float): SpaceProxy;
			function OutVol(const vol: float): SpaceProxy;
			procedure Apply;
		end;
		function SpaceParams: SpaceProxy;

	private writeable_const_
		HasPrevViewer: boolean = no;
		PrevPos: Vec3   = (data: (0.0, 0.0, 0.0));
		PrevTime: Ticks = (value: 0);
		LastVel: Vec3   = (data: (0.0, 0.0, 0.0)); _end
	var
		PrevFront, PrevTop: Vec3; static;

	public
		procedure SetViewer(const pos, front, top: Vec3; force: boolean = no); static;
		procedure ResetViewer(force: boolean = no); static;
		procedure GlobalPause; static;
		procedure GlobalResume; static;

	protected
		watch: SoundWatch; static;
		midiFonts: array of SoundFont; static;
		procedure LoadMidiFonts; static;
	const
		MaxSimultaneousSamplePlaybacks = 12;
		StateIds: array[State] of string = ('playing', 'paused', 'stalled', 'stopped');
	end;

	pSoundNode = ^SoundNode;
	SoundNode = object(SceneNode)
		snd: pSound;
		constructor Init(const stream: string; newFlags: Sound.FlagSet);
		destructor Done; virtual;
	protected
		procedure _AfterAttach; virtual;
		procedure _BeforeDetach; virtual;
		procedure _OnUpdate(const dt: float); virtual;
		procedure _OnApplyTransform; virtual;
		function _SuitsTo(know: SceneKnowledge): boolean; virtual;
	private
		_parentIsRigidBody: boolean;
	end;

	pMusicPlayer = ^MusicPlayer;
	MusicPlayer = object
	type
		OnOpenCloseProc = procedure(cam: pObject; const info: SingleDelegateInfo);
		OnProcessProc = procedure(var au: Sound; cam: pObject; const info: SingleDelegateInfo);

		ItemProxy = object
			procedure Volume(const vol: float);
			function OnOpen: pMultiDelegate;
			function OnProcess: pMultiDelegate;
			function OnClose: pMultiDelegate;
		private
			it: pointer; // pItemDesc
		end;

		ThemeProxy = object
			function AddItem(const stream: string): ItemProxy;
			function AddItem(const name, stream: string): ItemProxy;
			function FadeoutTime(const ft: float): ThemeProxy;
		private
			th: pointer; // pThemeDesc
		end;

		procedure Init;
		procedure Done;
		procedure Process(cam: pObject);
		function AddTheme(const name: PoolString): ThemeProxy;
		function CurrentTrack(name: pString; time, total: pFloat): boolean;
		procedure Switch;
		procedure Rewind(const delta: float);
		function Priority(const theme: PoolString; throw: boolean = yes): pModifiableValue;
		function ActiveTheme: PoolString;
		procedure Save(stream: pStream);
		procedure Restore(stream: pStream);
		function Amplitude: float;
		procedure Spectre(n: uint; freqs: pFloat; ranges: pFloat; amps: pFloat);
		procedure ResetTheme(const name: PoolString);
		procedure ResetAllThemes;

	const
		MasterVolume = 0.5;
		DefaultFadeInTime = 3.0;
		DefaultFadeOutTime = 1.5;
		MinTimeLeftToRemember = 10.0;

	private type
		OnProcessArgs = record
			au: pSound;
			cam: pObject;
		end;

		pItemDesc = ^ItemDesc;
		ItemDesc = object
			name, stream: string;
			volume: float;
			open, process, close: MultiDelegate;
			savedPosition: float; // [0; 1); -1, если нет
			procedure Init(const name, stream: string);
			procedure Done; {$define pSelf := pItemDesc} {$define constructor_args := const name, stream: string} {$include dyn_obj.h.inc}
		end;

		pThemeDesc = ^ThemeDesc;
		ThemeDesc = object
			name: PoolString;
			items: array of pItemDesc;
			priority: ModifiableValue;
			shuffle: array of uint;
			shufflePos: sint;
			fadeoutTime: float;
			procedure Init(const name: PoolString);
			procedure Done;
			function AddItem(const name, stream: string): pItemDesc;
			function FindItem(const name: string): sint;
			function FetchItem(remember: boolean): sint;
			procedure UpdateShuffle;
			procedure RemoveItem(id: uint);
			procedure MoveItem(from, &to: uint);
			procedure ChangeItemShufflePos(item: uint; newSp: sint);
		{$ifdef Debug} function DumpItems: string; {$endif}
		end;

	var
		interfaceLock, timerLock: ThreadLock;
		timer: ThreadTimer;
		stopTimer: boolean;
		themes: array of ThemeDesc;
		playing: pSound;
		playingTheme, playingItem: sint; // индексы в themes и themes[playingTheme].items, -1, если нет
		// playingTheme — скорее bestTheme, может быть выставлена, когда playingItem не валиден (но не наоборот).
		// playingItem и playing валидны-невалидны синхронно.
		activeCam: pObject; // для этой «камеры» был вызван open
		activeCamItem: pItemDesc; // nil, если нет
	{$ifdef Debug} function Dump: string; {$endif}
		procedure Lock(full: boolean);
		procedure Unlock(full: boolean);
		procedure CallOnOpenClose(var md: MultiDelegate; cam: pObject);
		procedure CallOnProcess(var md: MultiDelegate; var au: Sound; cam: pObject);
		procedure UpdateTheme(lock: boolean);
		procedure InternalSwitch(theme: sint; timerInstance: ThreadTimer.pCallbackInstance; force: boolean);
		function FindTheme(const name: PoolString; throw: boolean): sint;

		procedure SetCamera(cam: pObject);
		procedure CloseCamera(byCam: boolean);

		// если с удаляемым предметом связана камера и он содержит close, его нельзя удалять прямо сейчас, пока камера не закроется.
		function TryRemoveItem(theme, item: uint): boolean;
		function SoundRestToTimerDue(snd: pSound): uint;
		function GetSavedPosition(var snd: Sound; const theme: ThemeDesc): float;
	const
		ITEM_NAME_EXTRA_BITS         = 1;
		ITEM_HAS_SAVED_POSITION_BITN = 0;
		THEME_NAME_EXTRA_BITS        = 1;
		THEME_HAS_PRIORITY_BITN      = 0;
	end;

	procedure OpenScript(var script: ScriptState);

var
	Config: record
		device: sint;
		sampleRate: sint;
		window: PtrUint;
	end =
	(
		device: 1;
		sampleRate: 44100;
		window: 0
	);

implementation

uses
	Script_EngineAPI {$ifdef use_serialization}, Serialization {$endif};

	function SoundFont.Load(out sf: SoundFont; const fn: string): boolean;
	const
		MaxMemSize = {$ifdef Debug} 1 {$else} 5 {$endif} * 1024 * 1024;
	var
		stream: pStream;
	begin
		result := no;
		sf.handle := 0;
		sf.image := nil;
		if not Assigned(Midi.FontInit) then exit;

	{$ifdef Debug} LogR('Загрузка MIDI-шрифта из ' + StreamPath.Log(fn) + '... '); {$endif}
		stream := MakeRef(GetStream(fn));
		if Assigned(stream) and ((stream^.Size - stream^.Position).value <= MaxMemSize) then
		begin
			sf.image := stream^.GetImage(stream^.Position, (stream^.Size - stream^.Position).AsSizeT);
			Release(stream);
			if Assigned(sf.image) then
			begin
			{$ifdef Debug} LogR('(замаплен в память); ', logDebug); {$endif}
				sf.handle := Midi.FontInit(sf.image^.data, Midi.FONT_MEM);
				if sf.handle = 0 then Release(sf.image);
			end;
		end;
		Release(stream);

		if not Assigned(sf.image) then
			sf.handle := Midi.FontInitU8(StreamPath.System(fn), Midi.FONT_MMAP);

		result := sf.handle <> 0;
		if result then {$ifdef Debug} sf.LogInfo {$endif} else
		begin
		{$ifdef Debug} Log(Bass.LastOperationFailedMessage('загрузить SoundFont', 'FontInit'), logError); {$endif}
			sf.StartUnloading;
			sf.EndUnloading;
		end;
	end;

	procedure SoundFont.StartUnloading;
	begin
		if handle <> 0 then Midi.FontFree(handle);
	end;

	procedure SoundFont.EndUnloading;
	begin
		Release(image);
	end;

{$ifdef Debug}
type
	BankInfo = record
		id: uint;
		name: string;
	end;

	PresetInfo = record
		banks: array of BankInfo;
	end;

	function DescribeBank(id: uint; param: pointer): string;
	var
		banks: ^BankInfo absolute param;
	begin
		result := '(' + ToString(banks[id].id) + ') "' + banks[id].name + '"';
	end;

	function DescribePreset(id: uint; param: pointer): string;
	var
		presets: ^PresetInfo absolute param;
	begin
		if length(presets[id].banks) = 0 then exit('');
		result := ToString(id) + ': ' + SeparatedList.Join(length(presets[id].banks), @DescribeBank, pointer(presets[id].banks), '; ');
	end;

	function GetFontItem(id: uint; param: pointer): string;
	var
		font: ^Midi.FONTINFO absolute param;
	begin
		result := '';
		case id of
			0: if font^.name <> '' then result := 'имя: "' + font^.name + '"';
			1: if font^.samsize <> Bass.DW_ERROR then result := 'размер: ' + ToStringSuff_b(font^.samsize);
			2: if font^.presets > 0 then result := 'инструментов: ' + ToString(font^.presets);
			3: if font^.copyright <> '' then result := 'копирайт: "' + font^.copyright + '"';
			4: if font^.comment <> '' then result := 'комментарий: "' + font^.comment + '"';
			5: if font^.samtype <> 0 then result := 'формат: ' + BASS.DescribeChannelType(font^.samtype);
		end;
	end;

	procedure SoundFont.LogInfo;
	const
		ReasonablePresetsCountLimit = 128;
	var
		i: sint;
		s: string;
		fontInfo: Midi.FONTINFO;
		presetIDs: array of dword;
		presetID, bankID, ibank: uint;
		presets: array of PresetInfo;
		presetName: pChar;
	begin
		if Midi.FontGetInfo(handle, fontInfo) then
		begin
			s := SeparatedList.Join(6, @GetFontItem, @fontInfo, ', ');
			if s <> '' then Log(FixupSentence(s), logOK);

			SetLength(presetIDs, fontInfo.presets);
			if Midi.FontGetPresets(handle, pDword(presetIDs)) then
			begin
				presets := nil;
				for i := 0 to High(presetIDs) do
				begin
					presetID := Lo(presetIDs[i]);
					bankID   := Hi(presetIDs[i]);
					presetName := Midi.FontGetPreset(handle, presetID, bankID);
					if Assigned(presetName) then
					begin
						if presetID >= uint(length(presets)) then
							if presetID < ReasonablePresetsCountLimit then
								SetLength(presets, presetID + 1)
							else
							begin
								Log('Слишком много пресетов.', logError);
								break;
							end;

						ibank := length(presets[presetID].banks);
						SetLength(presets[presetID].banks, ibank + 1);
						presets[presetID].banks[ibank].id   := bankID;
						presets[presetID].banks[ibank].name := presetName;
					end;
				end;

				if length(presets) > 0 then
					Log('Инструменты по модулям: ' + EOL + SeparatedList.Join(length(presets), @DescribePreset, pointer(presets), EOL));
			end else
				{$ifdef Debug} Log(Bass.LastOperationFailedMessage('запросить пресеты шрифта', 'FontGetPresets'), logError) {$endif};
		end else
			{$ifdef Debug} Log(Bass.LastOperationFailedMessage('запросить информацию о шрифте', 'FontGetInfo'), logError) {$endif};
	end;
{$endif}

	procedure WatchTimer(param: pointer; var instance: ThreadTimer.CallbackInstance);
	var
		w: ^SoundWatch absolute param;
		snd: pSound;
		i: sint;
		stillRequired, boost, remove: boolean;
		trash: ObjectsList;
	begin
		stillRequired := no;
		trash.Init;
		w^.Lock(no);
		try
			if w^.stopTimer then exit;
			boost := no;
			for i := High(w^.playing) downto 0 do
			begin
				snd := w^.playing[i];
				remove := no;
				if snd^.QueryState = Sound.Stopped then remove := yes else
					if (snd^.fadeTargetVolume >= 0) and not Bass.ChannelIsSliding(snd^.h, Bass.ATTRIB_VOL) then
					begin
						snd^.fadeTargetVolume := -1;
						if Sound.Flag._FadingForever in snd^.flags then
							remove := yes;
					end;

				if remove then
				begin
					w^.UnlockedRemove(snd, i);
					snd^.InternalStop(yes);
					trash.Push(snd);
				end else
				begin
					stillRequired := yes;
					boost := boost or w^.ShouldBoostFor(snd);
				end;
			end;

			if stillRequired then
			begin
				Assert(not (boost and not w^.timerBoosted));
				if not boost and w^.timerBoosted then
				begin
				{$ifdef Debug} Log('Переключаю слежение за звуками в медленный режим.', logDebug); {$endif}
					w^.timerBoosted := no;
					instance.Reset(w^.Timeouts[w^.timerBoosted], w^.Timeouts[w^.timerBoosted]);
				end;
			end else
			begin
			{$ifdef Debug} Log('Слежение за проигрываемыми звуками закрыто.'); {$endif}
				w^.stopTimer := yes;
				instance.Close;
			end;
		finally
			w^.Unlock(no);
		end;
		trash.ForEach(@Release);
		trash.Done;
	end;

	procedure SoundWatch.Init;
	begin
		interfaceLock.Init;
		timerLock.Init;
		timer.Invalidate;
		playing := nil;
	end;

	procedure SoundWatch.Done;
	var
		i: sint;
	begin
		timerLock.Enter;
		for i := 0 to High(playing) do
		begin
			playing[i]^.InternalDone(yes);
			Release(playing[i]);
		end;
		timer.Close;
		timerLock.Leave;
		timerLock.Done;
		interfaceLock.Done;
	end;

	procedure SoundWatch.Lock(full: boolean);
	begin
		if full then interfaceLock.Enter;
		timerLock.Enter;
	end;

	procedure SoundWatch.Unlock(full: boolean);
	begin
		timerLock.Leave;
		if full then interfaceLock.Leave;
	end;

	procedure SoundWatch.UnlockedAdd(snd: pSound);
	begin
		Assert(interfaceLock.AcquiredAssert and timerLock.AcquiredAssert);

		if 0 = not snd^.watchId then
		begin
			SetLength(playing, length(playing) + 1);
			playing[High(playing)] := snd^.NewRef;
			snd^.watchId := High(playing);

			if not timer.Valid then
			begin
				timerBoosted := ShouldBoostFor(snd);
				stopTimer := no;
				ThreadTimer.Open(timer, @WatchTimer, @self, Timeouts[timerBoosted], Timeouts[timerBoosted]);
			{$ifdef Debug} Log('Открыто слежение за проигрываемыми звуками ({0} режим).', IfThen(timerBoosted, 'быстрый', 'медленный')); {$endif}
			end;
		end;

		if timer.Valid and not timerBoosted and ShouldBoostFor(snd) then
		begin
		{$ifdef Debug} Log('Переключаю слежение за звуками в быстрый режим.', logDebug); {$endif}
			timerBoosted := yes;
			timerLock.Leave;
			timer.Reset(Timeouts[timerBoosted], Timeouts[timerBoosted]);
			timerLock.Enter;
		end;
	end;

	procedure SoundWatch.Remove(snd: pSound);
	begin
		Lock(yes);
		if 0 <> not snd^.watchId then UnlockedRemove(snd, snd^.watchId) else snd := nil;
		Unlock(yes);
		Release(snd);
	end;

	procedure SoundWatch.UnlockedRemove(snd: pSound; id: uint);
	begin
		Assert(timerLock.AcquiredAssert);
		// Релизнуть snd нужно вручную.
		Assert(id < uint(length(playing)));
		Assert(snd = playing[id]);
		playing[id] := playing[High(playing)];
		playing[id]^.watchId := id;
		SetLength(playing, length(playing) - 1);
		snd^.watchId := not uint(0);
	end;

	function SoundWatch.ShouldBoostFor(snd: pSound): boolean;
	begin
		result := Sound.Flag._FadingForever in snd^.flags;
	end;

	constructor Sound.Init(const stream: string; flags: FlagSet);
	begin
		inherited Init;
		self.h     := 0;
		self.flags := [];
		self.sample := nil;
		self.stream := nil;
		self._3d    := nil;
		self.fadeTargetVolume := -1;
		self.watchId := not uint(0);
		if not (Flag._Dummy in flags) then InternalInit(stream, flags);
	end;

	destructor Sound.Done;
	begin
		InternalDone(yes);
		inherited Done;
	end;

	procedure Sound.Play;
	begin
		if h <> 0 then
		begin
			watch.Lock(yes);
			if Looped in flags then
				if Bass.ChannelFlags(h, Bass.SAMPLE_LOOP, Bass.SAMPLE_LOOP) = Bass.DW_ERROR then
				{$ifdef Debug} Log(Bass.LastOperationFailedMessage('выставить SAMPLE_LOOP ' + StreamPath.Log(name), 'ChannelFlags'), logError) {$endif};
			if not Bass.ChannelPlay(h, no) then
			{$ifdef Debug} Log(Bass.LastOperationFailedMessage('воспроизвести ' + StreamPath.Log(name), 'ChannelPlay'), logError) {$endif};
			watch.UnlockedAdd(@self);
			watch.Unlock(yes);
		end;
	end;

	procedure Sound.Pause;
	{$ifdef Debug} var code: dword; {$endif}
	begin
		if h <> 0 then
		begin
			if not Bass.ChannelPause(h) then
			begin
			{$ifdef Debug}
				code := Bass.ErrorGetCode();
				if code <> Bass.ERROR_NOPLAY then
					Log(Bass.LastOperationFailedMessage('приостановить ' + StreamPath.Log(name), 'ChannelPause'), logError);
			{$endif}
			end;
		end;
	end;

	procedure Sound.Stop;
	begin
		InternalStop(no);
	end;

	function Sound.QueryState: State;
	begin
		if h = 0 then result := Stopped else
			case Bass.ChannelIsActive(h) of
				Bass.ACTIVE_PLAYING: result := Playing;
				Bass.ACTIVE_PAUSED:  result := Paused;
				Bass.ACTIVE_STALLED: result := Stalled;
				else                 result := Stopped;
			end;
	end;

	procedure Sound.Fade(const time, targetVolume: float; lock: boolean = yes);
	begin
		if lock then watch.Lock(yes);
		fadeTargetVolume := max(targetVolume, 0);
		if not Bass.ChannelSlideAttribute(h, Bass.ATTRIB_VOL, targetVolume, ToMilliseconds(time)) then
		begin
		{$ifdef Debug} if h <> 0 then Log(Bass.LastOperationFailedMessage('задать плавное изменение громкости ' + StreamPath.Log(name), 'ChannelSlideAttribute'), logError) {$endif};
			Volume := targetVolume;
		end;
		watch.UnlockedAdd(@self);
		if lock then watch.Unlock(yes);
	end;

	procedure Sound.FadeOut(const time: float);
	begin
		watch.Lock(yes);
		flags += [Flag._FadingForever];
		Fade(time, 0, no);
		watch.Unlock(yes);
	end;

	function Sound.Amplitude: float;
	{$if sizeof(cfloat) <> sizeof(float)} var amp: cfloat; {$endif}
	begin
		if Bass.ChannelGetLevelEx(h, @{$if sizeof(cfloat) = sizeof(float)} result {$else} amp {$endif}, 0.02, Bass.LEVEL_MONO) then
		{$if sizeof(cfloat) <> sizeof(float)} result := amp {$endif}
		else
		{$ifdef Debug} if h <> 0 then Log(Bass.LastOperationFailedMessage('получить мгновенную громкость ' + StreamPath.Log(name), 'ChannelGetLevelEx'), logError) {$endif};
	end;

	procedure Sound.Spectre(n: uint; freqs: pFloat; ranges: pFloat; amps: pFloat);
	label fallback;
	var
		fft: packed array[0 .. 511] of float32;
		i, ifreq: uint;
		freq, freqToFftPosK, fftPos: float;

		function SafeIndexFFT(pos: sint): float32;
		begin
			if (pos >= 0) and (pos < length(fft)) then result := fft[pos] else result := 0;
		end;

	begin
		unused_args ranges end_list
		if not Bass.ChannelGetAttribute(h, Bass.ATTRIB_FREQ, freq) then
		begin
		{$ifdef Debug} if h <> 0 then Log(Bass.LastOperationFailedMessage('запросить частоту ' + StreamPath.Log(name), 'ChannelGetAttribute'), logError); {$endif}
			goto fallback;
		end;
		freqToFftPosK := length(fft) / freq;

		if Bass.ChannelGetData(h, @fft, Bass.DATA_FFT1024) = Bass.DW_ERROR then
		begin
		{$ifdef Debug} Log(Bass.LastOperationFailedMessage('получить спектр ' + StreamPath.Log(name), 'ChannelGetInfo'), logError); {$endif}
			goto fallback;
			exit;
		end;

		ifreq := 0;
		while ifreq < n do
		begin
			fftPos := freqs[ifreq] * freqToFftPosK;
			if frac(fftPos) = 0 then
				amps[ifreq] := SafeIndexFFT(round(fftPos))
			else
				amps[ifreq] := lerp(SafeIndexFFT(ifloor(fftPos)), SafeIndexFFT(iceil(fftPos)), frac(fftPos));
			inc(ifreq);
		end;
		exit;

	fallback:
		i := 0;
		while i < n do
		begin
			amps[i] := 0;
			inc(i);
		end;
	end;

	constructor Sound.SampleType.Init(const stream: string);
	begin
		try
			Bass.loader.Load;
		except
			instant_reraise_from_constructor;
		end;
		inherited Init;
		h2d := 0;
		h3d := 0;
		self.stream := stream;
	end;

	destructor Sound.SampleType.Done;
	begin
		if instantly_reraised_from_constructor then exit;
		DestroySample(h2d);
		DestroySample(h3d {$ifdef Debug}, '3d' {$endif});
		inherited Done;
		Bass.loader.Unload;
	end;

	function Sound.SampleType.GetChannel(use3d: boolean): Bass.HCHANNEL;
		function GetChannelFailure: Exception;
		var
			msg: string;
		begin
			msg := Bass.LastOperationFailedMessage('воспроизвести ' + StreamPath.Log(stream), 'SampleGetChannel');
			if Bass.ErrorGetCode() = Bass.ERROR_TIMEOUT then result := SampleGetChannelAborted.Create(msg) else result := Error(msg);
		end;
	var
		h: ^Bass.HSAMPLE;
	begin
		if use3d then h := @h3d else h := @h2d;
		case h^ of
			0: LoadOne(h^, use3d);
			FAILED_HANDLE: raise Error(StreamPath.Human(stream) + ' не удалось загрузить ранее.');
		end;

		result := Bass.SampleGetChannel(h^, yes);
		if result = 0 then raise GetChannelFailure;
		if not use3d and (h^ = h3d) then
			if not Bass.ChannelSet3DAttributes(h^, Bass._3DMODE_OFF, -1, -1, -1, -1, -1) then
				try
					raise Bass.LastOperationFailed('убрать 3D-функциональность у ' + StreamPath.Human(stream), 'ChannelSet3DAttributes');
				finally
					if not Bass.ChannelStop(h^) then {$ifdef Debug} Log(Bass.LastOperationFailedMessage('экстренно уничтожить ' + StreamPath.Log(stream), 'ChannelStop'), logError) {$endif};
				end;
	end;

	procedure Sound.SampleType.LoadOne(out h: Bass.HSAMPLE; use3d: boolean);
	var
		s: pStream;
		im: pStreamImage;
		info: Bass.SAMPLE;
	begin
		try
			s := GetStreamRef(stream);
			try
				im := s^.GetImage(s^.Position, (s^.Size - s^.Position).AsSizeT);
			finally
				Release(s);
			end;

			try
				h := Bass.SampleLoad(yes, im^.data, 0, im^.size, MaxSimultaneousSamplePlaybacks, IfThen(use3d, Bass.SAMPLE_MONO or Bass.SAMPLE_3D or Bass.SAMPLE_OVER_POS, 0));
				if h = 0 then raise Bass.LastOperationFailed('загрузить ' + StreamPath.Human(stream), 'SampleLoad');

				if not Bass.SampleGetInfo(h, info) then
					raise Bass.LastOperationFailed('получить информацию ' + StreamPath.Human(stream), 'SampleGetInfo');
				begin
				{$ifdef Debug}
					Log('Сэмпл {0} загружен: {1}, {2} Гц, {3}.',
						[StreamPath.Log(stream), ToString(FileSize.Explicit(info.length)), info.freq, lang_amount(info.chans, '{N} канал{/а/ов}')], logDebug);
				{$endif}
					// TODO: переиспользовать сэмпл для 2D и 3D, если он моно.
				end;

			finally
				Release(im);
			end;
		except
			h := FAILED_HANDLE;
			raise;
		end;
	end;

	procedure Sound.SampleType.DestroySample(h: Bass.HSAMPLE {$ifdef Debug}; const sampleKind: string = '' {$endif});
	{$ifdef Debug} var comment: string; {$endif}
	begin
		if (h = 0) or (h = FAILED_HANDLE) then exit;
		if h = h2d then h2d := 0;
		if h = h3d then h3d := 0;

	{$ifdef Debug} comment := WrapNonempty(sampleKind, ' (/)'); {$endif}
		if Bass.SampleFree(h) then
			{$ifdef Debug} Log('Сэмпл {0}{1} уничтожен', StreamPath.Log(stream), comment, logDebug) {$endif}
		else
			{$ifdef Debug} Log(Bass.LastOperationFailedMessage('освободить ' + StreamPath.Log(stream) + comment, 'SampleFree'), logWarning) {$endif};
	end;

	procedure BassFileClose(user: Pointer); basscall;
	var
		au: pSound absolute user;
	begin
	{$ifdef Debug} Log('BASS закрыла {0}.', StreamPath.Log(au^.stream^.path)); {$endif}
		Release(au^.stream);
	end;

	function BassFileLen(user: Pointer): qword; basscall;
	var
		au: pSound absolute user;
	begin
		result := au^.stream^.Size.value;
	end;

	function BassFileRead(buffer: Pointer; length: dword; user: Pointer): dword; basscall;
	var
		au: pSound absolute user;
	begin
		result := au^.stream^.TryRead(buffer, length);
		if result = 0 then result := Bass.DW_ERROR;
	end;

	function BassFileSeek(offset: qword; user: Pointer): LongBool; basscall;
	var
		au: pSound absolute user;
	begin
		au^.stream^.Position := FilePos.Explicit(offset);
		result := au^.stream^.Position.value = offset;
	end;

const
	BassFileProcs2: Bass.FILEPROCS =
	(
		close: @BASSFileClose;
		length: @BASSFileLen;
		read: @BASSFileRead;
		seek: @BASSFileSeek
	);

	procedure Sound.InternalInit(const stream: string; flags: FlagSet);
	var
		bassFlags: dword;
	begin
	{$ifdef Debug}
		self.name := StreamPath.Filename(stream);
		if stream = '' then self.name := '(заглушка)';
	{$endif}
		NewRef;

		try
			Bass.loader.Load;
			self.flags += [Flag._BassLoaded];
			bassFlags := 0;
			if Use3D in flags then
			begin
				self.flags += [Use3D];
				bassFlags := bassFlags or Bass.SAMPLE_3D;
				new(self._3d); self._3d^.self := @self; self._3d^.mask := [];
			end;

			if (stream <> '') or not (DontThrow in flags) then
				if PreferSample(stream) then
				begin
					sample := ResourcePool.Shared^.LoadRef(TypeOf(Sound.SampleType), stream);
					h := sample^.GetChannel(Use3D in flags);
				end else
				begin
					self.stream := GetStreamRef(stream);
					h := Bass.StreamCreateFileUser(Bass.STREAMFILE_NOBUFFER, bassflags, BassFileProcs2, @self);
					if h = 0 then raise Bass.LastOperationFailed('создать BASS-поток из ' + StreamPath.Human(stream), 'StreamCreateFileUser');
				{$ifdef Debug} Log('Создано аудио из {0}.', StreamPath.Log(stream)); {$endif}
				end;

			if Looped in flags then
				self.flags += [Looped];

			if not (StartPaused in flags) and not (Use3D in flags) then Play;
		except
			if DontThrow in flags then
			begin
			{$ifdef Debug}
				if not (Exception.Current is Exception) or not Exception(Exception.Current).Logged then
					Log(Exception.Message, LogMessageStyle(IfThen(Exception.Current is SampleGetChannelAborted, ord(logWarning), ord(logError))));
			{$endif}
				InternalDone(no);
			end else
			begin
				SilentUnref;
				raise;
			end;
		end;
	end;

	procedure Sound.InternalDone(fromWatch: boolean);
	begin
		if Flag._BassLoaded in flags then
		begin
			InternalStop(fromWatch);

			// Внимание, возможно (или вообще почти всегда будет?) h <> 0 и NOT Assigned(stream), т. к. его закрыла BASSFileClose.
			// Лучше, наверное, предусмотреть «режим» (sample/stream/etc.) во флагах, особенно если будет больше вариантов
			// (для BASS_Music* того же отдельный API).
			if (h <> 0) and not Assigned(sample) then
			begin
				Bass.StreamFree(h);
				h := 0; // здесь, а не в InternalStop, только чтобы остановленные потоки возвращали длительность — для красоты (CurrentTrack).
			end;
			Bass.loader.Unload;
			flags -= [Flag._BassLoaded];
		end;
		if Assigned(_3d) then begin dispose(_3d); _3d := nil; end;
		Release(sample);
		Release(stream);
	end;

	procedure Sound.InternalStop(fromWatch: boolean);
	begin
		if h <> 0 then
		begin
			if not fromWatch then watch.Remove(@self);
			if not Bass.ChannelStop(h) then
			{$ifdef Debug} Log(Bass.LastOperationFailedMessage('остановить ' + StreamPath.Log(name), 'ChannelStop'), logError) {$endif};
			if Assigned(sample) then
			begin
				h := 0;
				Release(sample);
			end;
		end;
	end;

	function Sound.SourceStream: string;
	begin
		if Assigned(sample) then result := sample^.stream else
			if Assigned(stream) then result := stream^.path else
				result := '';
	end;

	function Sound.PreferSample(const stream: string): boolean;
	const
		Extensions: array[0 .. 0] of string = ('wav');
	begin
		result := FindStr(StreamPath.Extension(stream), Extensions) >= 0;
	end;

	function Sound.BytesToSeconds(const bytes: qword): cfloat;
	begin
		result := Bass.ChannelBytes2Seconds(h, bytes);
		if result < 0 then
		begin
		{$ifdef Debug} if h <> 0 then Log(Bass.LastOperationFailedMessage('получить время по позиции в ' + StreamPath.Log(name), 'ChannelBytes2Seconds'), logError); {$endif}
			result := 0;
		end;
	end;

	function Sound.SecondsToBytes(const sec: cfloat): qword;
	begin
		result := Bass.ChannelSeconds2Bytes(h, sec);
		if result = Bass.QW_ERROR then
		begin
		{$ifdef Debug} if h <> 0 then Log(Bass.LastOperationFailedMessage('получить позицию в файле ' + StreamPath.Log(name) + ' по времени', 'ChannelBytes2Seconds'), logError); {$endif}
			result := 0;
		end;
	end;

	function Sound.GetDuration: float;
	var
		lenb: qword;
	begin
		lenb := Bass.ChannelGetLength(h, Bass.POS_BYTE);
		if lenb <> Bass.QW_ERROR then
			result := BytesToSeconds(lenb)
		else
		begin
		{$ifdef Debug} if h <> 0 then Log(Bass.LastOperationFailedMessage('получить длину ' + StreamPath.Log(name), 'ChannelGetLength'), logError); {$endif}
			result := 0;
		end;
	end;

	function Sound.GetVolume: float;
	{$if sizeof(cfloat) <> sizeof(float)} var vol: cfloat; {$endif}
	begin
		if Bass.ChannelGetAttribute(h, Bass.ATTRIB_VOL, {$if sizeof(cfloat) <> sizeof(float)} vol {$else} result {$endif}) then
		{$if sizeof(cfloat) <> sizeof(float)} result := vol {$endif}
		else {$ifdef Debug} if h <> 0 then Log(Bass.LastOperationFailedMessage('получить громкость ' + StreamPath.Log(name), 'ChannelGetAttribute'), logError); {$endif};
	end;

	procedure Sound.SetVolume(const volume: float);
	begin
		if not Bass.ChannelSetAttribute(h, Bass.ATTRIB_VOL, volume) then
		{$ifdef Debug} if h <> 0 then Log(Bass.LastOperationFailedMessage('установить громкость ' + StreamPath.Log(name), 'ChannelGetAttribute'), logError) {$endif};
	end;

	function Sound.GetPosition: float;
	var
		posb: qword;
	begin
		posb := Bass.ChannelGetPosition(h, Bass.POS_BYTE);
		if posb <> Bass.QW_ERROR then
			result := BytesToSeconds(posb)
		else
		begin
		{$ifdef Debug} if h <> 0 then Log(Bass.LastOperationFailedMessage('получить позицию воспроизведения ' + StreamPath.Log(name), 'ChannelGetPosition'), logError); {$endif}
			result := 0;
		end;
	end;

	procedure Sound.SetPosition(const position: float);
	var
	{$ifdef Debug} code, {$endif} posb, lenb: qword;
	begin
		posb := SecondsToBytes(position);
		lenb := Bass.ChannelGetLength(h, Bass.POS_BYTE);
		if posb = lenb then // BASS ругается :C
			if Looped in flags then posb := 0 else
			begin
				Stop;
				exit;
			end;

		if not Bass.ChannelSetPosition(h, posb, Bass.POS_BYTE) then
		begin
		{$ifdef Debug}
			if h <> 0 then
			begin
				code := Bass.ErrorGetCode();
				Log(Bass.OperationFailedMessage(Format('установить позицию воспроизведения {0}: pos = {1}, dur = {2}, len = {3}, s2b = {4}',
					[StreamPath.Log(name), position, duration, Bass.ChannelGetLength(h, Bass.POS_BYTE), posb]), 'ChannelSetPosition', code), logError);
			end;
		{$endif}
		end;
	end;

	function Sound.SpaceProxy.Location(const loc: Vec3): SpaceProxy;
	begin result := self;
		if Assigned(_3d) then
			if not (LocationSet in _3d^.mask) or (_3d^.location <> loc) then
			begin
				_3d^.location := loc;
				_3d^.mask += [LocationSet, LocationDirty];
			end;
	end;

	function Sound.SpaceProxy.Orientation(const orie: Vec3): SpaceProxy;
	begin result := self;
		if Assigned(_3d) then
			if not (OrientationSet in _3d^.mask) or (_3d^.orientation <> orie) then
			begin
				_3d^.orientation := orie;
				_3d^.mask += [OrientationSet, OrientationDirty];
			end;
	end;

	function Sound.SpaceProxy.Velocity(const vel: Vec3): SpaceProxy;
	begin result := self;
		if Assigned(_3d) then
			if not (VelocitySet in _3d^.mask) or (_3d^.velocity <> vel) then
			begin
				_3d^.velocity := vel;
				_3d^.mask += [VelocitySet, VelocityDirty];
			end;
	end;

	function Sound.SpaceProxy.Distances(const min, max: float): SpaceProxy;
	begin result := self;
		if Assigned(_3d) then
			if not (DistancesSet in _3d^.mask) or (_3d^.minDist <> min) or (_3d^.maxDist <> max) then
			begin
				_3d^.minDist := min;
				_3d^.maxDist := max;
				_3d^.mask += [DistancesSet, DistancesDirty];
			end;
	end;

	function Sound.SpaceProxy.Cone(const inAngle, outAngle: float): SpaceProxy;
	begin result := self;
		if Assigned(_3d) then
			if not (ConeSet in _3d^.mask) or (_3d^.inAngle <> inAngle) or (_3d^.outAngle <> outAngle) then
			begin
				_3d^.inAngle := inAngle;
				_3d^.outAngle := outAngle;
				_3d^.mask += [ConeSet, ConeDirty];
			end;
	end;

	function Sound.SpaceProxy.OutVol(const vol: float): SpaceProxy;
	begin result := self;
		if Assigned(_3d) then
			if not (OutVolSet in _3d^.mask) or (_3d^.outVol <> vol) then
			begin
				_3d^.outVol := vol;
				_3d^.mask += [OutVolSet, OutVolDirty];
			end;
	end;

	procedure Sound.SpaceProxy.Apply;
	var
		loc, orie, vel: Bass._3DVECTOR;
		locp, oriep, velp: Bass.p3DVECTOR;
		min, max, ovol: cfloat;
		inOutAngle: IntVec2;
	begin
		if Assigned(_3d) and ([LocationDirty, OrientationDirty, VelocityDirty, DistancesDirty, ConeDirty, OutVolDirty] * _3d^.mask <> []) then
		begin
			if [LocationDirty, OrientationDirty, VelocityDirty] * _3d^.mask <> [] then
			begin
				if LocationDirty in _3d^.mask then begin loc := _3d^.location; locp := @loc; end else locp := nil;
				if OrientationDirty in _3d^.mask then begin orie := _3d^.orientation; oriep := @orie; end else oriep := nil;
				if VelocityDirty in _3d^.mask then begin vel := _3d^.velocity; velp := @vel; end else velp := nil;
				Bass.ChannelSet3DPosition(_3d^.self^.h, locp, oriep, velp);
			end;

			if [DistancesDirty, ConeDirty, OutVolDirty] * _3d^.mask <> [] then
			begin
				if DistancesDirty in _3d^.mask then begin min := _3d^.minDist; max := _3d^.maxDist; end else begin min := -1; max := -1; end;
				if ConeDirty in _3d^.mask then inOutAngle := IntRound(Vec2.Make(_3d^.inAngle, _3d^.outAngle) * Rad2Deg) else inOutAngle := IntVec2.MinusOnes;
				if OutVolDirty in _3d^.mask then ovol := _3d^.outVol else ovol := -1;
				Bass.ChannelSet3DAttributes(_3d^.self^.h, Bass._3DMODE_NORMAL, min, max, inOutAngle.data[0], inOutAngle.data[1], ovol);
				_3d^.mask -= [DistancesDirty, ConeDirty, OutVolDirty];
			end;
			Bass.Apply3D;
		end;
	end;

	function Sound.SpaceParams: SpaceProxy;
	begin
		result._3d := _3d;
	{$ifdef Debug} if not (Use3D in flags) then Log('Для ' + StreamPath.Log(name) + ' вызвана SpaceParams, но он создан без Use3D', logError) {$endif};
	end;

	procedure Sound.SetViewer(const pos, front, top: Vec3; force: boolean = no);
	var
		vel: Vec3;
		curTime, dt: Ticks;
	begin
		vel := LastVel;
		curTime := Ticks.Get;
		if HasPrevViewer then
			dt := curTime - PrevTime;
		if (not HasPrevViewer) or (dt > Ticks.FromSeconds(0.03)) then
		begin
			if HasPrevViewer then
			begin
				vel := (pos - PrevPos) / dt.ToSeconds;
				LastVel := vel;
			end else
				HasPrevViewer := yes;
			PrevPos := pos;
			PrevTime := curTime;
		end;

		if force or Bass.loader.IsLoaded then
		begin
			Bass.Set3DPosition(pos, vel, front, top);
			Bass.Apply3D;
		end else
		begin
			PrevFront := front;
			PrevTop := top;
		end;
	end;

	procedure Sound.ResetViewer(force: boolean = no);
	begin
		if HasPrevViewer or force then
		begin
			HasPrevViewer := no;
			Bass.Set3DPosition(Vec3.Make(999999999.0), Vec3.Zero, Vec3.NegativeZ, Vec3.PositiveY);
			Bass.Apply3D;
		end;
	end;

	procedure Sound.GlobalPause;
	begin
		if Bass.loader.IsLoaded then Bass.Pause;
	end;

	procedure Sound.GlobalResume;
	begin
		if Bass.loader.IsLoaded then Bass.Start;
	end;

	procedure Sound.LoadMIDIFonts;
	const
		Extensions: array[0 .. 3] of string = ('sf2', 'SF2', 'sf2pk', 'SF2PK');
	var
		fontsh: array of Midi.FONT;
		f: FoundFile;
		i {$ifdef Debug}, found {$endif}: sint;
	begin
	{$ifdef Debug} found := 0; {$endif}
		for f in Folder.Scan(Paths.MiscLibs, '*.sf2*', OnlyFiles) do
			if not Prefixed('-', f.name) and (FindStr(StreamPath.Extension(f.name), Extensions) >= 0) then
			begin
			{$ifdef Debug} inc(found); {$endif}
				SetLength(midiFonts, length(midiFonts) + 1);
				if not SoundFont.Load(midiFonts[High(midiFonts)], f.SearchedName) then SetLength(midiFonts, length(midiFonts) - 1);
			end;

	{$ifdef Debug}
		if found > 0 then
			if length(midiFonts) = found then
				Log('Загружено MIDI-шрифтов: ' + ToString(found), logOK)
			else
				if length(midiFonts) > 0 then
					Log('Загружено MIDI-шрифтов: {0} / {1}', [length(midiFonts), found], logWarning)
				else
					Log('Ни один MIDI-шрифт не загружен', logError);
	{$endif}

		if length(midiFonts) > 0 then
		begin
			SetLength(fontsh, length(midiFonts));
			for i := 0 to High(midiFonts) do
			begin
				fontsh[i].font := midiFonts[i].handle;
				fontsh[i].preset := -1;
				fontsh[i].bank := 0;
			end;
			if not Midi.StreamSetFonts(0, pointer(fontsh), length(fontsh)) then
			{$ifdef Debug} Log(Bass.LastOperationFailedMessage('установить MIDI-шрифты по умолчанию', 'StreamSetFonts'), logError) {$endif};
		end;
	end;

	constructor SoundNode.Init(const stream: string; newFlags: Sound.FlagSet);
	begin
		inherited Init;
		snd := new(pSound, Init(stream, newFlags + [Sound.Use3D, Sound.StartPaused, Sound.DontThrow]));
	end;

	destructor SoundNode.Done;
	begin
		inherited Done;
		snd^.Stop;
		Release(snd);
	end;

	procedure SoundNode._AfterAttach;
	begin
		inherited _AfterAttach;
		_parentIsRigidBody := InheritsFrom(TypeOf(parent^), TypeOf(RigidBody));
		snd^.Play;
	end;

	procedure SoundNode._BeforeDetach;
	begin
		snd^.{Pause} Stop;
		inherited _BeforeDetach;
	end;

	procedure SoundNode._OnUpdate(const dt: float);
	begin
		inherited _OnUpdate(dt);
		if _parentIsRigidBody then snd^.SpaceParams.Velocity(pRigidBody(parent)^.velocity).Apply;
		if snd^.QueryState = snd^.Stopped then Detach;
	end;

	procedure SoundNode._OnApplyTransform;
	begin
		inherited _OnApplyTransform;
		snd^.SpaceParams.Location(WorldPos).Apply;
	end;

	function SoundNode._SuitsTo(know: SceneKnowledge): boolean;
	begin
		case know of
			scene_Update: result := yes;
			else
				result := inherited _SuitsTo(know);
		end;
	end;

	procedure MusicPlayer.ItemProxy.Volume(const vol: float); begin pItemDesc(it)^.volume := RangeCheck(vol, 0, 10, 'volume'); end;
	function MusicPlayer.ItemProxy.OnOpen: pMultiDelegate;    begin result := @pItemDesc(it)^.open; end;
	function MusicPlayer.ItemProxy.OnProcess: pMultiDelegate; begin result := @pItemDesc(it)^.process; end;
	function MusicPlayer.ItemProxy.OnClose: pMultiDelegate;   begin result := @pItemDesc(it)^.close; end;

	function MusicPlayer.ThemeProxy.AddItem(const stream: string): ItemProxy;
	begin
		result := AddItem(StreamPath.FilenameNoExt(stream), stream);
	end;

	function MusicPlayer.ThemeProxy.AddItem(const name, stream: string): ItemProxy;
	begin
		result.it := pThemeDesc(th)^.AddItem(name, stream);
	end;

	function MusicPlayer.ThemeProxy.FadeoutTime(const ft: float): ThemeProxy;
	begin
		pThemeDesc(th)^.fadeoutTime := RangeCheck(ft, 0, 10, 'fadeout');
		result := self;
	end;

	procedure MusicPlayer.Init;
	begin
		interfaceLock.Init;
		timerLock.Init;
		timer.Invalidate;
		themes := nil;
		playing := nil;
		playingItem := -1;
		playingTheme := -1;
		activeCam := nil;
		activeCamItem := nil;
	end;

	procedure MusicPlayer.Done;
	var
		i: sint;
	begin
		if Assigned(activeCam) then CloseCamera(no);
		for i := 0 to High(themes) do
			themes[i].Done;
		timer.Close;
		if Assigned(playing) then playing^.Stop;
		Release(playing);
		timerLock.Done;
		interfaceLock.Done;
	end;

	procedure MusicPlayer.Process(cam: pObject);
	begin
		Lock(yes);
		if (cam <> activeCam) or Assigned(activeCamItem) and (not Assigned(playing) or (activeCamItem <> themes[playingTheme].items[playingItem])) then
		begin
			if Assigned(activeCam) then CloseCamera(no);
			SetCamera(cam);
		end;

		if Assigned(activeCamItem) and not activeCamItem^.process.Empty then
		begin
			Assert(activeCamItem = themes[playingTheme].items[playingItem]);
			CallOnProcess(activeCamItem^.process, playing^, cam);
		end;
		Unlock(yes);
	end;

	procedure OnThemePriorityChanged(var v: ModifiableValue; param: pointer);
	begin
		unused_args v end_list
		pMusicPlayer(param)^.UpdateTheme(yes);
	end;

	function MusicPlayer.AddTheme(const name: PoolString): ThemeProxy;
	var
		id: sint;
	begin
		id := FindTheme(name, no);
		if id < 0 then
		begin
			id := length(themes);
			SetLength(themes, id + 1);
			themes[id].Init(name);
			themes[id].priority.SetChangeCallback(@OnThemePriorityChanged, @self);
		end;
		result.th := @themes[id];
	end;

	function MusicPlayer.CurrentTrack(name: pString; time, total: pFloat): boolean;
	begin
		Lock(yes);
		result := playingItem >= 0;
		if result then
		begin
			if Assigned(name) then name^ := themes[playingTheme].items[playingItem]^.name;
			if Assigned(time) then time^ := playing^.Position;
			if Assigned(total) then total^ := playing^.Duration;
		end;
		Unlock(yes);
	end;

	procedure MusicPlayer.Switch;
	begin
		Lock(yes); InternalSwitch(playingTheme, nil, no); Unlock(yes);
	end;

	procedure MusicPlayer.Rewind(const delta: float);
	var
		due: uint;
	begin
		Lock(yes);
		if Assigned(playing) then
		begin
			if playing^.QueryState <> Sound.Stopped then
			begin
				playing^.Position := Clamp(playing^.Position + delta, 0, playing^.Duration);
				if playing^.QueryState <> Sound.Stopped then due := SoundRestToTimerDue(playing) else due := 0;
				timer.SelfReset(due, 0);
			{$ifdef Debug} Log('Трек "{0}" перемотан на {1}{2} с, таймер перезадан на {3} с.', [playing^.name, IfThen(delta > 0, '+'), delta, due/1000]); {$endif}
			end else
			begin
			{$ifdef Debug} Log('Трек "{0}" остановлен, переключаю.', playing^.name); {$endif}
				InternalSwitch(playingTheme, nil, no);
			end;
		end;
		Unlock(yes);
	end;

	function MusicPlayer.Priority(const theme: PoolString; throw: boolean = yes): pModifiableValue;
	var
		id: sint;
	begin
		id := FindTheme(theme, throw);
		if id >= 0 then result := @themes[FindTheme(theme, yes)].priority else result := nil;
	end;

	function MusicPlayer.ActiveTheme: PoolString;
	begin
		Lock(yes);
		if playingTheme >= 0 then result := themes[playingTheme].name else result := '';
		Unlock(yes);
	end;

	procedure MusicPlayer.Save(stream: pStream);
		procedure SaveItem(const item: ItemDesc);
		begin
			Serialize_string_xbits(stream, item.name, ITEM_NAME_EXTRA_BITS, uint(item.savedPosition >= 0) shl ITEM_HAS_SAVED_POSITION_BITN);
			if item.savedPosition > 0 then
				Serialize_fN8(stream, clamp(item.savedPosition, 0, 255/256), 0, 255/256);
		end;
	var
		theme, i: sint;
	begin
		unused_args stream end_list
		// Для каждой темы сохраняются треки с сохранёнными позициями (по именам) в порядке собственных шаффлов, начиная с текущего.
		// При загрузке неизвестные игнорируются, не упомянутые ресетаются в 0.
		Lock(yes);
		if Assigned(playing) then
			themes[playingTheme].items[playingItem]^.savedPosition := GetSavedPosition(playing^, themes[playingTheme]);
	{$ifdef Debug} Log('Сохраняемое состояние плеера:' + EOL + Dump, logDebug); {$endif}
		try
			Serialize_ui8(stream, RangeCheck(length(themes), High(uint8), 'ThemesCount'));
			for theme := 0 to High(themes) do
			begin
				Serialize_string_xbits(stream, themes[theme].name, THEME_NAME_EXTRA_BITS, uint(not themes[theme].priority.Empty) shl THEME_HAS_PRIORITY_BITN);
				if not themes[theme].priority.Empty then themes[theme].priority.Serialize(stream);
				Serialize_ui8(stream, RangeCheck(length(themes[theme].items), High(uint8), 'ItemsCount'));

				// «выпрямить» шаффл
				for i := max(themes[theme].shufflePos, 0) to High(themes[theme].items) do
					SaveItem(themes[theme].items[themes[theme].shuffle[i]]^);
				for i := 0 to themes[theme].shufflePos - 1 do
					SaveItem(themes[theme].items[themes[theme].shuffle[i]]^);
			end;
		finally
			Unlock(yes);
		end;
	end;

	procedure MusicPlayer.Restore(stream: pStream);
	var
		rt: array of record
			priority: ModifiableValue;
			name: string;
			items: array of record
				name: string;
				savedPosition: float;
			end;
		end;
	{$ifdef Debug} rdump, idump: string; {$endif}
		ritem, rtheme, item, theme, targetPos: sint;
		bits: uint;
	begin
		Lock(yes);
	{$ifdef Debug} Log('Состояние плеера перед загрузкой:' + EOL + Dump, logDebug); {$endif}
		InternalSwitch(-1, nil, no);
		try
			// Прочитать во временную структуру.
		{$ifdef Debug} rdump := ''; {$endif}
			SetLength(rt, Deserialize_ui8(stream));
			for rtheme := 0 to High(rt) do
			begin
				rt[rtheme].name := Deserialize_string_xbits(stream, THEME_NAME_EXTRA_BITS, bits);
				if bits and (1 shl THEME_HAS_PRIORITY_BITN) <> 0 then ModifiableValue.Deserialize(rt[rtheme].priority, stream) else rt[rtheme].priority.Init(0);
				SetLength(rt[rtheme].items, Deserialize_ui8(stream));

			{$ifdef Debug} idump := ''; {$endif}
				for ritem := 0 to High(rt[rtheme].items) do
				begin
					rt[rtheme].items[ritem].name := Deserialize_string_xbits(stream, ITEM_NAME_EXTRA_BITS, bits);
				{$ifdef Debug} ContinueString(idump, Format('({0}) "{1}"', [ritem, rt[rtheme].items[ritem].name]), ', '); {$endif}
					if bits and (1 shl ITEM_HAS_SAVED_POSITION_BITN) <> 0 then rt[rtheme].items[ritem].savedPosition := Deserialize_fN8(stream, 0, 255/256) else rt[rtheme].items[ritem].savedPosition := -1;
				end;
			{$ifdef Debug} ContinueString(rdump, '"' + rt[rtheme].name + '": ' + IfThen(idump <> '', idump, '(пусто)'), EOL); {$endif}
			end;
		{$ifdef Debug} Log('Загруженное состояние плеера:' + EOL + IfThen(rdump <> '', rdump, '(пусто)'), logDebug); {$endif}

			// Все шаффлы и неизвестные позиции выставляются в -1.
			for theme := 0 to High(themes) do
			begin
				themes[theme].shufflePos := -1;
				for item := 0 to High(themes[theme].items) do
					themes[theme].items[item]^.savedPosition := -1;
			end;

			// Применить к плееру. Неизвестные темы и треки скипаются. Известные перемещаются в начало шаффлов.
			for rtheme := 0 to High(rt) do
			begin
				theme := FindTheme(rt[rtheme].name, no);
				if theme < 0 then
				begin
				{$ifdef Debug} Log('Тема BGM "{0}" неизвестна.', rt[rtheme].name, logError); {$endif}
					rt[rtheme].priority.Done;
					continue;
				end;

				themes[theme].priority.Done;
				themes[theme].priority := rt[rtheme].priority;
				themes[theme].priority.SetChangeCallback(@OnThemePriorityChanged, @self);

				targetPos := 0;
				for ritem := 0 to High(rt[rtheme].items) do
				begin
					item := themes[theme].FindItem(rt[rtheme].items[ritem].name);
					if item < 0 then
					begin
					{$ifdef Debug} Log('Трек "{0}.{1}" неизвестен.', rt[rtheme].name, rt[rtheme].items[ritem].name, logError); {$endif}
						continue;
					end;

					// Подвинуть этот предмет в targetPos, выставить savedPosition.
					themes[theme].ChangeItemShufflePos(item, targetPos);
					themes[theme].items[item]^.savedPosition := rt[rtheme].items[ritem].savedPosition;
					inc(targetPos);
				end;
			end;
		finally
			playingTheme := -1; // хак, чтобы UpdateTheme дёрнула SwitchTheme
			UpdateTheme(no);
		{$ifdef Debug} Log('Состояние плеера после слияния с загруженным:' + EOL + Dump, logDebug); {$endif}
			Unlock(yes);
		end;
	end;

	function MusicPlayer.Amplitude: float;
	begin
		Lock(yes);
		if Assigned(playing) then result := playing^.Amplitude else result := 0;
		Unlock(yes);
	end;

	procedure MusicPlayer.Spectre(n: uint; freqs: pFloat; ranges: pFloat; amps: pFloat);
	begin
		Lock(yes);
		if Assigned(playing) then playing^.Spectre(n, freqs, ranges, amps) else Zero(amps, n * sizeof(float));
		Unlock(yes);
	end;

	procedure MusicPlayer.ResetTheme(const name: PoolString);
	var
		i, id: sint;
	begin
		id := FindTheme(name, no);
		if id >= 0 then
		begin
			Lock(yes);
			for i := 0 to High(themes[id].items) do
				themes[id].items[i]^.savedPosition := -1;
			Unlock(yes);
		end;
	end;

	procedure MusicPlayer.ResetAllThemes;
	var
		t, i: sint;
	begin
		Lock(yes);
		for t := 0 to High(themes) do
			for i := 0 to High(themes[t].items) do
				themes[t].items[i]^.savedPosition := -1;
		Unlock(yes);
	end;

	procedure MusicPlayer.ItemDesc.Init(const name, stream: string);
	begin
		self.name   := name;
		self.stream := stream;
		volume      := 1;
		open.Init;
		process.Init;
		close.Init;
		savedPosition := -1;
	end;

	procedure MusicPlayer.ItemDesc.Done;
	begin
		open.Done;
		process.Done;
		close.Done;
	end;
{$define classname := MusicPlayer.ItemDesc} {$define pSelf := pItemDesc} {$define constructor_args := const name, stream: string} {$define pass_constructor_args := name, stream}
{$include dyn_obj.pp.inc}

	procedure MusicPlayer.ThemeDesc.Init(const name: PoolString);
	begin
		self.name := name;
		items := nil;
		priority.Init(0);
		shuffle := nil;
		shufflePos := -1;
		fadeoutTime := DefaultFadeoutTime;
	end;

	procedure MusicPlayer.ThemeDesc.Done;
	var
		i: sint;
	begin
		for i := 0 to High(items) do
			items[i]^.Free;
		priority.Done;
	end;

	function MusicPlayer.ThemeDesc.AddItem(const name, stream: string): pItemDesc;
	var
		index: uint;
	begin
		result := ItemDesc.Create(name, stream);
		SetLength(items, length(items) + 1);
		items[High(items)] := result;

		SetLength(shuffle, length(shuffle) + 1);
		index := GlobalRNG.GetUint(length(items));
		shuffle[High(shuffle)] := shuffle[index];
		shuffle[index] := High(shuffle);
	end;

	function MusicPlayer.ThemeDesc.FindItem(const name: string): sint;
	begin
		result := IndexIndirect(name, pPointer(items), length(items), @ItemDesc(nil^).name);
	end;

	function MusicPlayer.ThemeDesc.FetchItem(remember: boolean): sint;
	begin
		if remember and (shufflePos >= 0) and (items[shuffle[shufflePos]]^.savedPosition >= 0) then
			exit(shuffle[shufflePos]);

		if shufflePos >= High(shuffle) then
		begin
			UpdateShuffle;
			shufflePos := -1;
		end;
		inc(shufflePos);
		if shufflePos >= length(shuffle) then
		begin
			shufflePos := -1;
			exit(-1);
		end;
		result := shuffle[shufflePos];
	end;

	procedure MusicPlayer.ThemeDesc.UpdateShuffle;
	{$define elemType := uint} {$define Shuffle := ShuffleUint} {$define use_rng := GlobalRNG} {$define openarray} {$include perm.inc}

	{$ifdef Debug}
		function HumanShuffle: string;
		begin
			result := '(' + SeparatedList.Join(length(shuffle), @GetStringFromSintArray, pSint(shuffle), ', ') + ')';
		end;
	{$endif}
	var
		half: uint;
	begin
		// Выбирается элемент, близкий к среднему, затем левая и правая части перемешиваются отдельно.
		// Так одна и та же композиция не повторится слишком быстро.
		half := uint(length(shuffle)) div 2 + GlobalRNG.GetUint((2 * uint(length(shuffle))) div 3);
		half -= uint(length(shuffle)) div 3;
	{$ifdef Debug} Log('Тема "{0}" до перемешивания: {1}, половинка на позиции {2}', [string(name), HumanShuffle, half], logDebug); {$endif}
		ShuffleUint(pUint(shuffle), half);
		ShuffleUint(pUint(shuffle) + half, length(shuffle) - half);
	{$ifdef Debug} Log('Тема "{0}" перемешана: {1}', string(name), HumanShuffle, logDebug); {$endif}
	end;

	procedure MusicPlayer.ThemeDesc.RemoveItem(id: uint);
	var
		sp, i: sint;
	begin
		MoveItem(id, High(items));
		items[High(items)]^.Free;
		SetLength(items, length(items) - 1);

		// Удалить соответствующий элемент перестановки.
		sp := Index(length(items), pUint(shuffle), length(shuffle)); Assert(sp >= 0);
		if shufflePos >= sp then dec(shufflePos);
		for i := sp to High(shuffle) - 1 do
			shuffle[i] := shuffle[i + 1];
		SetLength(shuffle, length(shuffle) - 1);
	end;

	procedure MusicPlayer.ThemeDesc.MoveItem(from, &to: uint);
	var
		t: pItemDesc;
		i: sint;
	begin
		// from <= to:                        from > to:
		// A1 B2 C3 D4 E5 F6 G7               A1 B2 C3 D4 E5 F6 G7
		// 3->6                               6->3
		// A1 B2 D3(4) E4(5) F5(6) C6(3) G7   A1 B2 F3(6) C4(3) D5(4) E6(5) G7
		t := items[from];
		if from <= &to then
			for i := from to &to - 1 do
				items[i] := items[i + 1]
		else
			for i := from downto &to + 1 do
				&items[i] := items[i - 1];
		items[&to] := t;

		for i := 0 to High(shuffle) do
			if shuffle[i] = from then shuffle[i] := &to else
				if from <= &to then
					if (shuffle[i] > from) and (shuffle[i] <= &to) then dec(shuffle[i]) else
				else
					if (shuffle[i] >= &to) and (shuffle[i] < &from) then inc(shuffle[i]);
	end;

	procedure MusicPlayer.ThemeDesc.ChangeItemShufflePos(item: uint; newSp: sint);
	var
		sp, i: sint;
	begin
		sp := Index(item, pUint(shuffle), length(shuffle)); Assert(sp >= 0);
		if sp <= newSp then
			for i := sp to newSp - 1 do
				shuffle[i] := shuffle[i + 1]
		else
			for i := sp downto newSp + 1 do
				shuffle[i] := shuffle[i - 1];
		shuffle[newSp] := item;
	end;

{$ifdef Debug}
	function MusicPlayer.ThemeDesc.DumpItems: string;
	var
		shuffleId, item: sint;
	begin
		result := '';
		for shuffleId := 0 to High(shuffle) do
		begin
			item := shuffle[shuffleId];
			ContinueString(result, Format('{0}Трек {1}: "{2}", savedPosition = {3}',
				[IfThen(shufflePos = shuffleId, '> '), item, items[item]^.name, items[item]^.savedPosition]), EOL);
		end;
		if result = '' then result := '(пусто)';
	end;
{$endif}

{$ifdef Debug}
	function MusicPlayer.Dump: string;
	var
		theme: sint;
	begin
		result := '';
		for theme := 0 to High(themes) do
		begin
			ContinueString(result, Format('Тема {0}: "{1}", приоритет {2}{3}',
				[theme, themes[theme].name.internal, IfThen(themes[theme].priority.Value > 0, '+'), themes[theme].priority.Value]), EOL);
			ContinueString(result, themes[theme].DumpItems, EOL);
		end;
		if result = '' then result := '(пусто)';
	end;
{$endif}

	procedure MusicPlayer.Lock(full: boolean);
	begin
		if full then interfaceLock.Enter;
		timerLock.Enter;
	end;

	procedure MusicPlayer.Unlock(full: boolean);
	begin
		timerLock.Leave;
		if full then interfaceLock.Leave;
	end;

	procedure OnOpenCloseCaller(const info: SingleDelegateInfo; param: pointer);
	begin
		MusicPlayer.OnOpenCloseProc(info.proc)(pObject(param), info);
	end;

	procedure MusicPlayer.CallOnOpenClose(var md: MultiDelegate; cam: pObject);
	begin
		md.Call(@OnOpenCloseCaller, cam);
	end;

	procedure OnProcessCaller(const info: SingleDelegateInfo; param: pointer);
	var
		args: ^MusicPlayer.OnProcessArgs absolute param;
	begin
		MusicPlayer.OnProcessProc(info.proc)(args^.au^, args^.cam, info);
	end;

	procedure MusicPlayer.CallOnProcess(var md: MultiDelegate; var au: Sound; cam: pObject);
	var
		args: OnProcessArgs;
	begin
		args.au := @au;
		args.cam := cam;
		md.Call(@OnProcessCaller, @args);
	end;

	procedure MusicPlayer.UpdateTheme(lock: boolean);
	var
		best, i: sint;
	begin
		best := -1;
		if lock then self.Lock(yes);
		for i := 0 to High(themes) do
			if ((best < 0) or (themes[i].priority.Value > themes[best].priority.Value)) and (themes[i].priority.Value > 0) then
				best := i;

		if best <> playingTheme then
		begin
		{$ifdef Debug}
			if best >= 0 then
				Log('Активация музыкальной темы "{0}" (приоритет {1}).', [string(themes[best].name), themes[best].priority.Value])
			else
				Log('Музыка выключена.');
		{$endif}
			InternalSwitch(best, nil, no);
		end;
		if lock then self.Unlock(yes);
	end;

	procedure SwitchTrackTimer(param: pointer; var instance: ThreadTimer.CallbackInstance);
	var
		bgm: pMusicPlayer absolute param;
		due: uint;
	begin
		bgm^.Lock(no);
		Assert(Assigned(bgm^.playing));
		if not bgm^.stopTimer then
			if bgm^.playing^.QueryState <> Sound.Stopped then
			begin
				due := bgm^.SoundRestToTimerDue(bgm^.playing);
			{$ifdef Debug} Log('Таймер на "{0}" сработал слишком рано. Перезадаю на {1} с.', [bgm^.playing^.name, due/1000], logWarning); {$endif}
				instance.Reset(due, 0);
			end else
			begin
			{$ifdef Debug} Log('Таймер на "{0}" сработал, переключаю трек.', bgm^.playing^.name, logOK); {$endif}
				bgm^.InternalSwitch(bgm^.playingTheme, @instance, yes);
			end;
		bgm^.Unlock(no);
	end;

	procedure MusicPlayer.InternalSwitch(theme: sint; timerInstance: ThreadTimer.pCallbackInstance; force: boolean);
	label again;
	var
		item, maxRetries: sint;
		targetVol: float;
		np: pSound;
	begin
		Assert((Assigned(timerInstance) or interfaceLock.AcquiredAssert) and timerLock.AcquiredAssert);
		if theme >= 0 then maxRetries := length(themes[theme].items) - 1 else maxRetries := 0;

	again:
		if theme >= 0 then item := themes[theme].FetchItem(theme <> playingTheme) else item := -1;
	{$ifdef Debug}
		if item >= 0 then begin Log('Выбран трек "{0}".', themes[theme].items[item]^.name); end else
			if theme >= 0 then Log('Нет доступных треков.', logWarning);
	{$endif}

		// тот же трек?
		if (theme = playingTheme) and (item = playingItem) and not force then exit;

		// остановить проигрываемый
		if Assigned(playing) then
		begin
			themes[playingTheme].items[playingItem]^.savedPosition := GetSavedPosition(playing^, themes[playingTheme]);

			playing^.FadeOut(themes[playingTheme].fadeoutTime);
			Release(playing);
			// playingTheme := -1;
			playingItem := -1;
			if Assigned(timerInstance) then
				timerInstance^.Close
			else
			begin
				stopTimer := yes;
				Unlock(no);
				timer.Close;
				Lock(no);
			end;
		end;
		if item < 0 then exit;

		// Переход к новому треку.
		targetVol := themes[theme].items[item]^.volume * MasterVolume;
		np := nil;
		try
			np := new(pSound, Init(themes[theme].items[item]^.stream, [Sound.StartPaused]));
		except
		{$ifdef Debug} Exception.Log; {$endif}
		end;

		if Assigned(np) then
		begin
			if themes[theme].items[item]^.savedPosition > 0 then
			begin
				np^.Position := themes[theme].items[item]^.savedPosition * np^.Duration;
				np^.Volume := 0;
				np^.Fade(DefaultFadeInTime, targetVol);
			end else
				np^.Volume := targetVol;
			np^.Play;

			if np^.QueryState = Sound.Playing then
			begin
				playing := np;
				playingTheme := theme;
				playingItem := item;
				stopTimer := no;
				ThreadTimer.Open(timer, @SwitchTrackTimer, @self, SoundRestToTimerDue(np), 0);
			end else
			begin
				np^.Stop;
				Release(np);
			end;
		end;

		if not Assigned(np) and TryRemoveItem(theme, item) then
		begin
			dec(maxRetries);
			if maxRetries >= 0 then goto again;
		end;
	end;

	function MusicPlayer.FindTheme(const name: PoolString; throw: boolean): sint;
	begin
		result := USystem.Index(name.ToIndex, first_field themes _ name _, length(themes), sizeof(themes[0]));
		if (result < 0) and throw then raise Error('Тема "{0}" не найдена.', name);
	end;

	procedure KillCamera(obj: pObject; param: pointer);
	var
		bgm: pMusicPlayer absolute param;
	begin
		Assert(pObject(bgm^.activeCam) = obj);
		bgm^.CloseCamera(yes);
	end;

	procedure MusicPlayer.SetCamera(cam: pObject);
	begin
		Assert(interfaceLock.AcquiredAssert);
		Assert(not Assigned(activeCam));
		activeCam := cam;
		cam^.AddOnDestroyProc(@KillCamera, @self);
		if playingItem >= 0 then activeCamItem := themes[playingTheme].items[playingItem] else activeCamItem := nil;
		if Assigned(activeCamItem) and not activeCamItem^.open.Empty then
			CallOnOpenClose(activeCamItem^.open, cam);
	end;

	procedure MusicPlayer.CloseCamera(byCam: boolean);
	begin
		Assert(Assigned(activeCam));
		if Assigned(activeCamItem) and not activeCamItem^.close.Empty then
			CallOnOpenClose(activeCamItem^.close, activeCam);
		if not byCam then activeCam^.RemoveOnDestroyProc(@KillCamera, @self);
		activeCam := nil;
		activeCamItem := nil;
	end;

	function MusicPlayer.TryRemoveItem(theme, item: uint): boolean;
	begin
		result := activeCamItem <> themes[theme].items[item];
		if result then themes[theme].RemoveItem(item);
	end;

	function MusicPlayer.SoundRestToTimerDue(snd: pSound): uint;
	const
		Min = 0.5;
		Bias = 0.05;
	begin
		result := ToMilliseconds(max(Min, snd^.Duration - snd^.Position + Bias));
	end;

	function MusicPlayer.GetSavedPosition(var snd: Sound; const theme: ThemeDesc): float;
	var
		rawPos, duration: float;
	begin
		rawPos := snd.Position;
		duration := snd.Duration;
		result := rawPos + 0.5 * theme.fadeoutTime;
		if (result >= 0) and (result <= duration - MinTimeLeftToRemember) then
		begin
		{$ifdef Debug} Log('Запомнена позиция трека "{0}": {1} с.', [snd.name, result]); {$endif}
			result := min(1, result / max(1.0, duration));
		end else
			result := -1;
	end;

	function GetSoundFlags(var ss: ScriptState): Sound.FlagSet;
	begin
		result := [];
		if ss.GetBoolField(1, 'loop') then result += [Sound.Looped];
		if ss.GetBoolField(1, 'paused') then result += [Sound.StartPaused];
	end;

	procedure Script_CreateSound(var ss: ScriptState);
	begin
		ss.PushObject(new(pSound, Init(ss.ToStream(ss.GetStringField(1, 1)), GetSoundFlags(ss))), yes);
	end;

{$define fname := Script_Sound_volume} {$define otype := Sound} {$define field := Volume} {$define prop_float} {$include script_prop.inc}
{$define fname := Script_Sound_position} {$define otype := Sound} {$define field := Position} {$define prop_float} {$include script_prop.inc}
{$define fname := Script_Sound_length} {$define otype := Sound} {$define field := Duration} {$define prop_float} {$define readonly} {$include script_prop.inc}

	procedure Script_Sound_Play(var ss: ScriptState); begin pSound(ss.ToSelf)^.Play; end;
	procedure Script_Sound_Pause(var ss: ScriptState); begin pSound(ss.ToSelf)^.Pause; end;
	procedure Script_Sound_Stop(var ss: ScriptState); begin pSound(ss.ToSelf)^.Stop; end;
	procedure Script_Sound_Fade(var ss: ScriptState); begin pSound(ss.ToSelf)^.Fade(ss.ToFloat(2), ss.ToFloat(3)); end;
	procedure Script_Sound_Amplitude(var ss: ScriptState); begin ss.PushFloat(pSound(ss.ToSelf)^.Amplitude); end;

	procedure Script_Sound_Spectre(var ss: ScriptState);
	const
		MaxFreqs = 16;
	var
		freqs, ranges: array[0 .. MaxFreqs - 1] of float;
		amps: array[0 .. MaxFreqs - 1] of float;
		n, i: sint;
	begin
		n := ss.RawLen(2) div 2;
		if n > MaxFreqs then
		begin
			ss.PushNil;
			exit;
		end;

		for i := 0 to n - 1 do
		begin
			freqs[i] := ss.GetFloatField(2, 1 + 2*i);
			ranges[i] := ss.GetFloatField(2, 1 + 2*i + 1);
		end;

		pSound(ss.ToSelf)^.Spectre(n, @freqs[0], @ranges[0], @amps[0]);

		ss.PushTable(n, 1);
		for i := 0 to n - 1 do
		begin
			ss.PushFloat(amps[i]);
			ss.SetTableI(-2, 1 + i);
		end;
	end;

	procedure Script_CreateSoundNode(var ss: ScriptState);
	var
		au: pSoundNode;
	begin
		au := new(pSoundNode, Init(ss.ToStream(ss.GetStringField(1, 1)), GetSoundFlags(ss)));
		ss.PushObject(au);

		if ss.HasField(1, 'volume') then au^.snd^.Volume := ss.GetFloatField(1, 'volume', au^.snd^.Volume);
		Script_common_create_scene_node(ss, au);
	end;

	procedure Script_SoundNode_Play(var ss: ScriptState); begin pSoundNode(ss.ToSelf)^.snd^.Play; end;
	procedure Script_SoundNode_Pause(var ss: ScriptState); begin pSoundNode(ss.ToSelf)^.snd^.Pause; end;
	procedure Script_SoundNode_Stop(var ss: ScriptState); begin pSoundNode(ss.ToSelf)^.snd^.Stop; end;

	procedure Script_SoundNode_Fade(var ss: ScriptState);
	var
		n: pSoundNode;
	begin
		n := ss.ToSelf;
		if ss.Top = 3 then
			n^.snd^.Fade(ss.ToFloat(2), ss.ToFloat(3))
		else
			n^.snd^.FadeOut(ss.ToFloat(2));
	end;

{$define fname := Script_SoundNode_volume} {$define otype := SoundNode} {$define field := snd^.Volume} {$define prop_float} {$include script_prop.inc}
{$define fname := Script_SoundNode_position} {$define otype := SoundNode} {$define field := snd^.Position} {$define prop_float} {$include script_prop.inc}
{$define fname := Script_SoundNode_length} {$define otype := SoundNode} {$define field := snd^.Duration} {$define prop_float} {$define readonly} {$include script_prop.inc}

	procedure OpenScript(var script: ScriptState);
	const
		Stuff: array[0 .. 19] of ScriptStuffDesc =
		(
			(s: TypeDesc; p: TypeOf(Sound)),
			(s: 'volume' + Writeable; p: @Script_Sound_volume),
			(s: 'position' + Writeable; p: @Script_Sound_position),
			(s: 'length'; p: @Script_Sound_length),

			(s: 'Play:0'; p: @Script_Sound_Play),
			(s: 'Pause:0'; p: @Script_Sound_Pause),
			(s: 'Stop:0'; p: @Script_Sound_Stop),
			(s: 'Fade:0'; p: @Script_Sound_Fade),
			(s: 'Amplitude:1'; p: @Script_Sound_Amplitude),
			(s: 'Spectre:1'; p: @Script_Sound_Spectre),

			(s: TypeDesc; p: TypeOf(SoundNode)),
			(s: 'volume' + Writeable; p: @Script_SoundNode_volume),
			(s: 'position' + Writeable; p: @Script_SoundNode_position),
			(s: 'length'; p: @Script_SoundNode_length),

			(s: 'Play:0'; p: @Script_SoundNode_Play),
			(s: 'Pause:0'; p: @Script_SoundNode_Pause),
			(s: 'Stop:0'; p: @Script_SoundNode_Stop),
			(s: 'Fade:0'; p: @Script_SoundNode_Fade),

			(s: FunctionsDesc + 'CreateSound:1' + RequireEnv; p: @Script_CreateSound),
			(s: 'CreateSoundNode:1' + RequireEnv; p: @Script_CreateSoundNode)
		);
	begin
		script.AddStuff(Stuff);
	end;

{$ifdef use_serialization}
const
	NODE_LOOPED_BIT           = 1 shl 0;
	NODE_FADING_BIT           = 1 shl 1;
	NODE_FADING_FOREVER_BIT   = 1 shl 2;
	NODE_DISTANCES_SET_BIT    = 1 shl 3;
	NODE_CONE_SET_BIT         = 1 shl 4;
	NODE_OUT_VOL_SET_BIT      = 1 shl 5;
	NODE_USER_VOL_BIT         = 1 shl 6;

	procedure SerializeSoundNode(se: pSerializer; obj: pointer);
	var
		n: pSoundNode absolute obj;
		flags: uint;
	begin
		Sound.watch.timerLock.EnterShared;
		with se^ do
		begin
			flags := 0;
			if Sound.Looped in n^.snd^.flags then flags := flags or NODE_LOOPED_BIT;
			if n^.snd^.fadeTargetVolume >= 0 then flags := flags or NODE_FADING_BIT;
			if Sound.Flag._FadingForever in n^.snd^.flags then flags := flags or NODE_FADING_FOREVER_BIT;
			if n^.snd^.Volume <> 1 then flags := flags or NODE_USER_VOL_BIT;

			if Assigned(n^.snd^._3d) then
			begin
				if DistancesSet in n^.snd^._3d^.mask then flags := flags or NODE_DISTANCES_SET_BIT;
				if ConeSet in n^.snd^._3d^.mask then flags := flags or NODE_CONE_SET_BIT;
				if OutVolSet in n^.snd^._3d^.mask then flags := flags or NODE_OUT_VOL_SET_BIT;
			end;

			Serialize_ui8(stream, flags);
			if n^.snd^.QueryState = Sound.Stopped then Serialize_string(stream, '') else Serialize_string(stream, CutPrefix(Paths.Data, n^.snd^.SourceStream));
			Serialize_fN8(stream, n^.snd^.Position,  0, max(n^.snd^.Duration, 0.1));
			if flags and NODE_FADING_BIT <> 0 then Serialize_f16(stream, n^.snd^.fadeTargetVolume);
			if flags and NODE_DISTANCES_SET_BIT <> 0 then Serialize_vec2f16(stream, Vec2.Make(n^.snd^._3d^.minDist, n^.snd^._3d^.maxDist));
			if flags and NODE_CONE_SET_BIT <> 0 then Serialize_vec2f16(stream, Vec2.Make(n^.snd^._3d^.inAngle, n^.snd^._3d^.outAngle));
			if flags and NODE_OUT_VOL_SET_BIT <> 0 then Serialize_f16(stream, n^.snd^._3d^.outVol);
			if flags and NODE_USER_VOL_BIT <> 0 then Serialize_f16(stream, n^.snd^.Volume);
		end;
		Sound.watch.timerLock.LeaveShared;
	end;

	procedure DeserializeSoundNode(de: pDeserializer; obj: pointer);
		function ClampVolume(const vol: float): float;
		begin
			result := Clamp(vol, 0, 10);
		end;
	var
		n: pSoundNode absolute obj;
		sstream: string;
		flags: uint;
		sflags: Sound.FlagSet;
		space: Sound.SpaceProxy;
		v2: Vec2;
		pos: float;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			sstream := Deserialize_string(stream);
			pos := Deserialize_fN8(stream, 0, 1);
			if sstream <> '' then sstream := Paths.Data + sstream;

			sflags := [];
			if flags and NODE_LOOPED_BIT <> 0 then sflags += [Sound.Looped];
			if flags and NODE_FADING_BIT <> 0 then
			begin
				n^.snd^.fadeTargetVolume := ClampVolume(Deserialize_f16(stream));
				if flags and NODE_FADING_FOREVER_BIT <> 0 then sflags += [Sound.Flag._FadingForever];
			end;

			n^.snd^.InternalInit(sstream, sflags + [Sound.Use3D, Sound.DontThrow]);
			space := n^.snd^.SpaceParams;
			if flags and NODE_DISTANCES_SET_BIT <> 0 then
			begin
				v2 := Clamp(Deserialize_vec2f16(stream), Vec2.Zero, Vec2.Make(30000.0));
				space.Distances(max(v2.data[0], 0), max(v2.data[1], 0));
			end;
			if flags and NODE_CONE_SET_BIT <> 0 then
			begin
				v2 := Clamp(Deserialize_vec2f16(stream), Vec2.Zero, Vec2.Make(TwoPi));
				space.Cone(v2.data[0], v2.data[1]);
			end;
			if flags and NODE_OUT_VOL_SET_BIT <> 0 then space.OutVol(ClampVolume(Deserialize_f16(stream)));
			space.Apply;
			if flags and NODE_USER_VOL_BIT <> 0 then n^.snd^.Volume := ClampVolume(Deserialize_f16(stream));
			n^.snd^.Position := pos * n^.snd^.Duration;
		end;
	end;

	procedure SoundNodeSeSpecial(se: pSerializer; what: SeSpecial; obj: pointer);
	var
		au: pSoundNode absolute obj;
	begin
		Assert(@se = @se);
		case what of
			se_Before: if Assigned(au^.Root) then au^.snd^.Pause;
			se_After:  if Assigned(au^.Root) and (au^.snd^.QueryState = Sound.Paused) then au^.snd^.Play;
		end;
	end;

	procedure SoundNodeDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		au: pSoundNode absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: au^.Init('', [Sound.Flag._Dummy]);
		end;
	end;
{$endif}

	procedure AfterLoad;
{$ifdef Debug}
	var
		device: sint;
		devInfo: Bass.DEVICEINFO;
		ver: dword;
{$endif}
	begin
		Bass.SetConfig(Bass.CONFIG_UNICODE, 1);
	{$ifdef Debug}
		ver := Bass.GetVersion();
		LogR('Версия BASS: ' + ToString(Hi(Hi(ver))) + '.' + ToString(Lo(Hi(ver))) + '.' +
			ToString(Hi(Lo(ver))) + '.' + ToString(Lo(Lo(ver))) + '. ', logDebug);
		LogR('Инициализация... ');
		if Config.window = 0 then Log('Не задано окно (Config.window)', logWarning);
	{$endif}

		if not Bass.Init(Config.device, Config.sampleRate, Bass.DEVICE_3D, Config.window, nil) then
			raise Error('Не удалось инициализировать BASS. ' + Bass.DescribeCurrentError);

	{$ifdef Debug}
		Log('BASS инициализирована', logOK);
		device := 0;
		while Bass.GetDeviceInfo(device, devInfo) do
		begin
			Log('Устройство ' + ToString(device) + ': "' + string(devInfo.name) + '", драйвер "' + string(devInfo.driver) + '"', logDebug);
			inc(device);
		end;
		Log('Макс. одновременно воспроизводящихся каналов из сэмпла: ' + ToString(Sound.MaxSimultaneousSamplePlaybacks));
	{$endif}

		Sound.LoadMIDIFonts;
		Bass.Set3DFactors(0.5, 1.0, 1.0);
		if Sound.HasPrevViewer then Sound.SetViewer(Sound.PrevPos, Sound.PrevFront, Sound.PrevTop, yes) else Sound.ResetViewer(yes);
		// ↑ вызовут Apply3D
	end;

	procedure BeforeUnload;
	var
		i: sint;
	begin
	{$ifdef Debug} LogR('Финализация BASS... '); {$endif}
		if Bass.Free_() then
		begin
		{$ifdef Debug} Log('BASS финализирована', logOK); {$endif}
		end else
		begin
		{$ifdef Debug} Log('Ошибка финализации BASS: ' + BASS.DescribeCurrentError, logWarning); {$endif}
		end;

		for i := 0 to High(Sound.midiFonts) do
			Sound.midiFonts[i].StartUnloading;
	end;

	procedure AfterUnload;
	var
		i: sint;
	begin
		for i := 0 to High(Sound.midiFonts) do
			Sound.midiFonts[i].EndUnloading;
		Sound.midiFonts := nil;
	end;

	function LoadSample(const stream: string): pObject; begin result := new(Sound.pSample, Init(stream)); end;
	procedure Init;
	begin
		ResourcePool.Shared^.Register(TypeOf(Sound.SampleType), @LoadSample);
		Bass.loader.Hook(+0).AfterLoad(@AfterLoad).BeforeUnload(@BeforeUnload).AfterUnload(@AfterUnload);
		Sound.watch.Init;

	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Sound node', TypeOf(SoundNode), TypeOf(SceneNode), sizeof(SoundNode), yes,
		               @SerializeSoundNode, @DeserializeSoundNode, @SoundNodeSeSpecial, @SoundNodeDeSpecial);
	{$endif}
	end;

	procedure Done;
	begin
		Sound.watch.Done;
	end;

initialization
	&Unit('Audio').Initialize(@Init, @Done);
end.
