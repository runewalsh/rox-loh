{$include opts.inc}
unit BinaryScripts;

interface

uses
	USystem, Errors, Streams, Algo, LuaLib, Utils, Script, SectionedFiles {$ifdef Debug}, UMath, ULog, Human {$endif};

type
	BinaryScript = object
		procedure Save(ls: lua.State; const name, source: string; s: pStream); static;
		function Load(s: pStream; debug: boolean): Strings; static;

	private const
		ReleaseSection       = 'R';
		DiffedDebugSection   = 'd';
		UndiffedDebugSection = 'D';

		procedure WriteDiff(s: pStream; const diff: Algo.Diff.Based; const b: string); static;
		procedure WriteSection(s: pStream; const name: string; const release, debug: string; const diff: Algo.Diff.Based); static;

		function LoadSectioned(s: pStream; debug: boolean): Strings; static;
		function LoadRawSection(s: pStream; const sec: SectionedFile.SectionDesc): string; static;
		function LoadDiffed(s: pStream; const sec: SectionedFile.SectionDesc; const a: string): Strings; static;
	end;

implementation

type
	BasedDiff = Algo.Diff.Based;
	WriteSectionParam = record
		release, debug: ^string;
		diff: ^BasedDiff;
	end;

	procedure WriteSectionCallback(const name: string; s: pStream; param: pointer);
	var
		p: ^WriteSectionParam absolute param;
	begin
		BinaryScript.WriteSection(s, name, p^.release^, p^.debug^, p^.diff^);
	end;

	procedure BinaryScript.Save(ls: lua.State; const name, source: string; s: pStream);
		function Compile(debug: boolean): string;
		var
			namex, errmsg: string;
		begin
			namex := name + IfThen(debug, ' (debug)', ' (release)');
			if not Lua.LoadString(ls, Preprocess(source, debug), name {без x, он для ошибок, а не для Lua-имени чанка}, @errmsg) then
				raise Error('Ошибка компиляции ' + namex + '.' + EOL + errmsg);
			result := lua.Dump(ls, -1, not debug);
			lua.pop(ls);
		end;

	var
		release, debug, debugSection: string;
		useDiff: boolean;
		diff: Algo.Diff.Based;
		p: WriteSectionParam;
	begin
		MakeRef(s);
		try
			release := Compile(no);
			debug   := Compile(yes);
			if debug = release then
			begin
				s^.Write(release);
				exit;
			end;

			diff := BasedDiff.Build(pChar(release), length(release), pChar(debug), length(debug), 128, Approx);
			useDiff := diff.totalTakenFromA > size_t(length(release)) div 3;
			debugSection := IfThen(useDiff, DiffedDebugSection, UndiffedDebugSection);
			p.release := @release; p.debug := @debug; p.diff := @diff;
			SectionedFile.Writer.Write(s, [ReleaseSection, debugSection], @WriteSectionCallback, @p);
		finally
			debug := ''; release := '';
			USystem.Release(s);
		end;
	end;

	function BinaryScript.Load(s: pStream; debug: boolean): Strings;
	var
		op: FilePos;
		os: pStream;
		sigChar: char;
	begin
		// TODO: Seek в потоках сжатия >___<" Или хотя бы Stream.Connect какой-нибудь.
		if s^.CanSeek then
			s^.NewRef
		else
		begin
			os := MakeRef(s);
			try
				s := s^.Window(s^.Position, s^.Size - s^.Position, s^.path)^.NewRef;
			finally
				Release(os);
			end;
		end;

		try
			op := s^.Position;
			s^.Read(@sigChar, sizeof(sigChar));
			s^.Position := op;
			case sigChar of
				#27 {Первый байт в сигнатуре бинарного чанка}, #$EF {Первый байт UTF-8 BOM} :
					begin
						SetLength(result, 1);
						result[0] := ReadWholeAsString(s, ReadWholeAsStringOpts.Binary(sigChar = #27));
					end;
				SectionedFile.Signature: result := LoadSectioned(s, debug);
				else raise Error('{0}: неверный формат бинарного скрипта.', StreamPath.Human(s^.path));
			end;
		finally
			Release(s);
		end;
	end;

	procedure SaveBFragment(s: pStream; const op: Diff.Based.OpDesc; const state: Diff.Based.SerializationState; param: pointer);
	var
		b: pString absolute param;
	begin
		if state.pass = 1 then s^.Write(pChar(b^) + op.start, op.count * sizeof(char));
	end;

	procedure BinaryScript.WriteDiff(s: pStream; const diff: BasedDiff; const b: string);
	begin
		diff.Serialize(s, nil, @SaveBFragment, @b);
	end;

	procedure BinaryScript.WriteSection(s: pStream; const name: string; const release, debug: string; const diff: Algo.Diff.Based);
	begin
		if name = ReleaseSection then s^.Write(release)
		else if name = DiffedDebugSection then WriteDiff(s, diff, debug)
		else if name = UndiffedDebugSection then s^.Write(debug)
		else raise Error('Неизвестная секция {0}.', name);
	end;

	function BinaryScript.LoadSectioned(s: pStream; debug: boolean): Strings;
		function FromString(const s: string): Strings;
		begin
			SetLength(result, 1);
			result[0] := s;
		end;
	var
		f: SectionedFile;
		release: string;
		sec: SectionedFile.pSectionDesc;
	begin
		f := SectionedFile.Open(s);
		try
			if debug and Assigned(f.TryGetSection(UndiffedDebugSection, sec, f.GetSectionMode.Precise)) then
				result := FromString(LoadRawSection(s, sec^))
			else
			begin
				release := LoadRawSection(s, f.GetSection(ReleaseSection, f.GetSectionMode.Precise)^);
				if debug then
					result := LoadDiffed(s, f.GetSection(DiffedDebugSection, f.GetSectionMode.Precise)^, release)
				else
					result := FromString(release);
			end;
		finally
			f.Close;
		end;
	end;

	function BinaryScript.LoadRawSection(s: pStream; const sec: SectionedFile.SectionDesc): string;
	begin
		s^.Position := sec.offset;
		SetLength(result, sec.length.value);
		s^.Read(pChar(result), length(result) * sizeof(char));
	end;

type
	LoadingState = record
		a: pString;
		r: ^Strings;
		sec: SectionedFile.pSectionDesc;
	{$ifdef Debug} takenFromA: size_t; {$endif}
	end;

	procedure LoadCount(nOps: uint; param: pointer);
	begin
		SetLength(LoadingState(param^).r^, nOps);
	end;

	procedure LoadAFragment(start, count: size_t; const lstate: Diff.Based.DeserializationState; param: pointer);
	var
		state: ^LoadingState absolute param;
	begin
		RangeCheck(start, size_t(length(state^.a^)) - 1, 'Diff.A.start');
		RangeCheck(count, size_t(length(state^.a^)) - start, 'Diff.A.count');
		state^.r^[lstate.opIndex] := Copy(state^.a^, 1 + start, count);
	{$ifdef Debug} state^.takenFromA += count; {$endif}
	end;

	procedure LoadBFragment(s: pStream; count: size_t; const lstate: Diff.Based.DeserializationState; param: pointer);
	var
		state: ^LoadingState absolute param;
	begin
		SetLength(state^.r^[lstate.opIndex], RangeCheck(count, (state^.sec^.length - (s^.Position - state^.sec^.offset)).value, 'Diff.B.count'));
		s^.Read(pChar(state^.r^[lstate.opIndex]), count * sizeof(char));
	end;

	function BinaryScript.LoadDiffed(s: pStream; const sec: SectionedFile.SectionDesc; const a: string): Strings;
	var
		state: LoadingState;
	begin
		s^.Position := sec.offset;
		state.a   := @a;
		state.r   := @result;
		state.sec := @sec;
	{$ifdef Debug} state.takenFromA := 0; {$endif}
		Diff.Based.Deserialize(s, @LoadCount, @LoadAFragment, @LoadBFragment, @state);

	{$ifdef Debug} Log('{0} {1}% данных.',
	                   lang_amount(length(result), 'Примен{ё/е/е}н{/ы/о} {N} дифф{/а/ов}, переиспользующи{й/е/х}'),
	                   ToString(state.takenFromA / max(length(a), 1) * 100), logDebug); {$endif}
	end;

end.
