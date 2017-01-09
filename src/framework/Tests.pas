{$include opts.inc}
{$if not defined(selftest) and not defined(is_test)} {$error Используй дефайн selftest.} {$endif}
unit Tests;

interface

uses
	USystem, Errors, UMath, Utils, Human {$ifdef use_console}, ConUtils {$endif};

type
	TestSuite = object
	type
		TestProcKind       = (TestString2String, TestStrings2String);
		String2StringProc  = function(const input: string): string;
		Strings2StringProc = function(const input: array of string): string;

		TestProc = record
		case kind: TestProcKind of
			TestString2String: (string2string: String2StringProc);
			TestStrings2String: (strings2string: Strings2StringProc);
		end;

		TestFailed = class(Exception) end;
	var
		function Start: TestSuite; static;
		function Feature(const name: string; const proc: TestProc): TestSuite;
		function &Case(const input: string; const expected: string): TestSuite;
		function &Case(const input: array of string; const expected: string): TestSuite;
		procedure Done;

	type
		FeatureRec = record
			name: string;
			proc: TestProc;
			passed, failed, total: uint;
		end;
	private var
		feats: array of FeatureRec;
		function &Case(const inputString: string; const inputStrings: array of string; const expected: string): TestSuite;
	const
		Empty: TestSuite = (feats: nil);

	public
		CompletedRegistry: array of FeatureRec; static;
		DontAskAnymore: boolean; static;
		procedure ClearRegistry; static;
	end;
	operator :=(proc: TestSuite.String2StringProc): TestSuite.TestProc;
	operator :=(proc: TestSuite.Strings2StringProc): TestSuite.TestProc;

type
	Benchmark = object
	type
		SectionEnum = (BenchmarkSection, ItemSection, ItemPreparationSection, ItemBenchmarkSection, ResultsSection, EmergencyRetrySection);
		scoped_enum_ ProcKind = (NotSet, Parametrized, Unparametrized); _end

	const
		NotSet = ProcKind.NotSet;
		Parametrized = ProcKind.Parametrized;
		Unparametrized = ProcKind.Unparametrized;

	type
		ParaInitDone   = procedure(var b: Benchmark);
		UnparaInitDone = procedure;
		InitDoneProc = record
		case kind: ProcKind of
			Parametrized:   (para: ParaInitDone);
			Unparametrized: (unpara: UnparaInitDone);
		end;

		ParaTest   = procedure(iterations: uint; var b: Benchmark);
		UnparaTest = procedure(iterations: uint);
		TestProc = record
		case kind: ProcKind of
			Parametrized:   (para: ParaTest);
			Unparametrized: (unpara: UnparaTest);
		end;

		ParaSection   = procedure(const item: string; section: SectionEnum; finished: boolean; var b: Benchmark);
		UnparaSection = procedure(const item: string; section: SectionEnum; finished: boolean);
		SectionProc = record
		case kind: ProcKind of
			Parametrized:   (para: ParaSection);
			Unparametrized: (unpara: UnparaSection);
		end;

		ParaProgress   = procedure(const item: string; const progress: float; var b: Benchmark);
		UnparaProgress = procedure(const itemName: string; const progress: float);
		ProgressProc = record
		case kind: ProcKind of
			Parametrized:   (para: ParaProgress);
			Unparametrized: (unpara: UnparaProgress);
		end;

		ParaResult   = procedure(const item: string; const oneTime: float; isBest: boolean; const bestTimes: float; var b: Benchmark);
		UnparaResult = procedure(const itemName: string; const oneTime: float; isBest: boolean; const bestTimes: float);
		ResultProc = record
		case kind: ProcKind of
			Parametrized:   (para: ParaResult);
			Unparametrized: (unpara: UnparaResult);
		end;

		function Create(const name: string; param: pointer): Benchmark; static;
		function Init(const proc: InitDoneProc): Benchmark;
		function Done(const proc: InitDoneProc): Benchmark;
		function Item(const name: string; proc: TestProc): Benchmark;
		function OnSection(const proc: SectionProc): Benchmark;
		function OnProgress(const proc: ProgressProc): Benchmark;
		function OnResult(const proc: ResultProc): Benchmark;
	{$ifdef use_console} function ReportToConsole: Benchmark; {$endif}
		procedure Run;

	var
		name: string;
		param: pointer;

	private type
		ItemRec = record
			name: string;
			proc: TestProc;
		end;

	const
		MinJudgeableInternal = 1000;
		MinJudgeableTime = 0.001;
		UninformedMultiplier = 32;

		PassTimeAim = 1.0;
		MinPassTime = PassTimeAim/2;
		MaxReasonablePassTime = 5.0;
		PassesCount = 7;
		MaxEmergencyRetries = 2;

	var
		items: array of ItemRec;
		_init, _done: InitDoneProc;
		_onSection: SectionProc;
		_onProgress: ProgressProc;
		_onResult: ResultProc;

		procedure Run(const one: ItemRec; out oneTime: float);
		procedure NoteSection(const itemName: string; section: SectionEnum; finished: boolean);
		procedure NoteProgress(const itemName: string; const progress: float);
		procedure NoteResult(const itemName: string; const oneTime: float; isBest: boolean; const bestTimes: float);
	end;
	operator :=(proc: Benchmark.ParaInitDone):   Benchmark.InitDoneProc;
	operator :=(proc: Benchmark.UnparaInitDone): Benchmark.InitDoneProc;
	operator :=(proc: Benchmark.ParaTest):       Benchmark.TestProc;
	operator :=(proc: Benchmark.UnparaTest):     Benchmark.TestProc;
	operator :=(proc: Benchmark.ParaSection):    Benchmark.SectionProc;
	operator :=(proc: Benchmark.UnparaSection):  Benchmark.SectionProc;
	operator :=(proc: Benchmark.ParaProgress):   Benchmark.ProgressProc;
	operator :=(proc: Benchmark.UnparaProgress): Benchmark.ProgressProc;
	operator :=(proc: Benchmark.ParaResult):     Benchmark.ResultProc;
	operator :=(proc: Benchmark.UnparaResult):   Benchmark.ResultProc;

implementation

	function TestSuite.Start: TestSuite;
	begin
		result := Empty;
	end;

	function TestSuite.Feature(const name: string; const proc: TestProc): TestSuite;
	var
		f: ^FeatureRec;
	begin
		SetLength(feats, length(feats) + 1);
		f := @feats[High(feats)];
		f^.name := name;
		f^.proc := proc;
		f^.passed := 0;
		f^.failed := 0;
		f^.total  := 0;
		result := self;
	end;

	function TestSuite.&Case(const input: string; const expected: string): TestSuite;
	begin
		result := &Case(input, [input], expected);
	end;

	function TestSuite.&Case(const input: array of string; const expected: string): TestSuite;
	begin
		result := &Case(SafeIndex(input, 0), input, expected);
	end;

	procedure TestSuite.Done;
	var
		strt, i: sint;
	begin
		SingletonLock.Enter;
		try
			strt := length(CompletedRegistry);
			SetLength(CompletedRegistry, strt + length(feats));
			for i := 0 to High(feats) do
				CompletedRegistry[strt + i] := feats[i];
			feats := nil;
		finally
			SingletonLock.Leave;
		end;
	end;

	function TestSuite.&Case(const inputString: string; const inputStrings: array of string; const expected: string): TestSuite;
	var
		f: ^FeatureRec;
		got: string;

		function Failed(kind: TestProcKind): Exception;
		var
			inputAsString: string;
		begin
			case kind of
				TestString2String: inputAsString := inputString;
				TestStrings2String: inputAsString := SeparatedList.Join(inputStrings, ', ' + SeparatedList.Empty + '(ничего)');
				else raise ExhaustiveCase(ord(kind), 'TestProcKind');
			end;

			result := TestFailed.Create(Format(
				'Тест {0} #{1} не пройден.' + EOL + EOL +
				'Дано:' + EOL + '{2}' + EOL + EOL +
				'Ожидается:' + EOL + '{3}' + EOL + EOL +
				'Получено:' + EOL + '{4}',
				f^.name, ToString(f^.total-1), PrintableString(inputAsString, yes), PrintableString(expected, yes), PrintableString(got, yes)));
		end;

	begin result := self;
		f := @feats[High(feats)];
		try
			case f^.proc.kind of
				TestString2String: got := f^.proc.string2string(inputString);
				TestStrings2String: got := f^.proc.strings2string(inputStrings);
				else raise ExhaustiveCase(ord(f^.proc.kind), 'TestProcKind');
			end;
			inc(f^.total);
			if got = expected then
				inc(f^.passed)
			else
				raise Failed(f^.proc.kind);
		except
			inc(f^.failed);

			if not DontAskAnymore then
				case Warning.Text(Exception.Message).Title('Тест не пройден')
					.ContinueOrStopVariants.Variant('Игнорировать проваленные тесты').Show
				of
					TaskV1: ;
					TaskV3: DontAskAnymore := yes;
					else {$ifdef Debug} raise {$else} Fatal {$endif};
				end;
		end;
	end;

	procedure TestSuite.ClearRegistry;
	begin
		CompletedRegistry := nil;
	end;

	operator :=(proc: TestSuite.String2StringProc): TestSuite.TestProc; begin result.kind := TestString2String; result.string2string := proc; end;
	operator :=(proc: TestSuite.Strings2StringProc): TestSuite.TestProc; begin result.kind := TestStrings2String; result.strings2string := proc; end;

	function Benchmark.Create(const name: string; param: pointer): Benchmark;
	begin
	FPC_3_BUG System.Initialize(result);
		result.name             := name;
		result.param            := param;
		result.items            := nil;
		result._init.kind       := NotSet;
		result._done.kind       := NotSet;
		result._onSection.kind  := NotSet;
		result._onProgress.kind := NotSet;
		result._onResult.kind   := NotSet;
	end;

	function Benchmark.Init(const proc: InitDoneProc): Benchmark; begin _init := proc; result := self; end;
	function Benchmark.Done(const proc: InitDoneProc): Benchmark; begin _done := proc; result := self; end;

	function Benchmark.Item(const name: string; proc: TestProc): Benchmark;
	begin
		SetLength(items, length(items) + 1);
		items[High(items)].name := name;
		items[High(items)].proc := proc;
		result := self;
	end;

	function Benchmark.OnSection(const proc: SectionProc): Benchmark;   begin _onSection := proc; result := self; end;
	function Benchmark.OnProgress(const proc: ProgressProc): Benchmark; begin _onProgress := proc; result := self; end;
	function Benchmark.OnResult(const proc: ResultProc): Benchmark;     begin _onResult := proc; result := self; end;

{$ifdef use_console}
const
	SectionNames: array[Benchmark.SectionEnum] of string = ('', '', 'оценка', 'основной проход', 'Результаты:', 'Экстренное повторение.');

	function LongestNameLen(var b: Benchmark): sint;
	var
		i: sint;
	begin
		result := UTF8.Codepoints(b.items[0].name);
		for i := 1 to High(b.items) do
			result := max(result, UTF8.Codepoints(b.items[i].name));
	end;

	function PadToLongestName(const itemName: string; var b: Benchmark): string;
	begin
		result := StrDup(' ', LongestNameLen(b) - sint(UTF8.Codepoints(itemName)));
	end;

	procedure PrintBenchmarkSectionToConsole(const item: string; section: Benchmark.SectionEnum; finished: boolean; var b: Benchmark);
	begin
		case section of
			BenchmarkSection: if not finished then Con.WriteLine(item);
			ItemSection: ;
			ResultsSection, EmergencyRetrySection:
				if not finished then Con.WriteLine(EOL + SectionNames[section]) else
					if section = ResultsSection then Con.WriteLine;
			ItemBenchmarkSection: if finished then Con.WriteLine;
			else
				if finished then
				else
					Con.Write(item + ': ' + PadToLongestName(item, b) + SectionNames[section] + '... ');
		end;
	end;

	procedure PrintBenchmarkProgressToConsole(const itemName: string; const progress: float; var b: Benchmark);
	begin
		if progress = 0 then
			// *5/3 — костыль для консоли в яплокали
			Con.Write(Carriage + StrDup(' ', (5 * UTF8.Codepoints(itemName + ': ' + PadToLongestName(itemName, b) + SectionNames[ItemPreparationSection] + '...') div 3)));
		Con.Write(Carriage + itemName + ': ' + PadToLongestName(itemName, b) + '[' + Bar(progress, b.PassesCount) + ']');
	end;

	procedure PrintBenchmarkResultToConsole(const itemName: string; const oneTime: float; isBest: boolean; const bestTimes: float);
		function Describe: string;
		begin
			if isBest then
				result := 'лучший'
			else
				result := Format('{0}x', ToString(bestTimes));
		end;
	begin
		Con.WriteLine(Format('{0}: {1} ({2})', itemName, TimeToString(oneTime), Describe));
	end;

	function Benchmark.ReportToConsole: Benchmark;
	begin
		result := OnSection (@PrintBenchmarkSectionToConsole)
		         .OnProgress(@PrintBenchmarkProgressToConsole)
		         .OnResult  (@PrintBenchmarkResultToConsole);
	end;
{$endif}

	procedure Benchmark.Run;
	var
		time: array of record
			one: float;
			bestTimes: float;
		end;
	var
		i: sint;
		best: float;
	begin
		case _init.kind of
			Parametrized: _init.para(self);
			Unparametrized: _init.unpara();
		end;
		NoteSection(name, BenchmarkSection, no);

		SetLength(time, length(items));
		for i := 0 to High(items) do
		begin
			Run(items[i], time[i].one);
			if i = 0 then best := time[i].one else best := min(best, time[i].one);
		end;

		NoteSection('', ResultsSection, no);
		for i := 0 to High(items) do
			time[i].bestTimes := time[i].one / best;
		for i := 0 to High(items) do
			NoteResult(items[i].name, time[i].one, time[i].one = best, time[i].bestTimes);
		NoteSection('', ResultsSection, yes);

		NoteSection(name, BenchmarkSection, yes);
		case _done.kind of
			Parametrized: _done.para(self);
			Unparametrized: _done.unpara();
		end;
	FPC_3_BUG System.Finalize(self);
	end;

	procedure Benchmark.Run(const one: ItemRec; out oneTime: float);
	var
		emergencyRetries: uint;

		function EmergencyRetry: boolean;
		begin
			inc(emergencyRetries);
			result := emergencyRetries <= MaxEmergencyRetries;
			if result then NoteSection(one.name, EmergencyRetrySection, no);
		end;

		procedure OnePass(iterations: uint; out t: Ticks; out sec: float);
		begin
			t := Ticks.Get;
			case one.proc.kind of
				Parametrized: one.proc.para(iterations, self);
				Unparametrized: one.proc.unpara(iterations);
				else raise Error('Не задана тестируемая функция.');
			end;
			t := Ticks.Get - t;
			sec := t.ToSeconds;

			if sec > MaxReasonablePassTime then
			begin
				if not EmergencyRetry then
					raise Error('{0}: допустимое время превышено{1}.', one.name,
					            IfThen(emergencyRetries > 1, Format(' в{0} {1}-й раз', IfThen(emergencyRetries = 2, 'о', ''), ToString(emergencyRetries)), ''));
				OnePass(iterations, t, sec);
			end;
		end;

		function AdjustIterations(it: uint; const mul: float): uint;
		var
			t: hp_float;
		begin
			t := max(hp_float(it) * mul + 0.5, 1.0);
			if t >= High(it) then raise Error('{0}: все доступные объёмы итераций выполняются слишком быстро.', one.name);
			result := trunc(t);
		end;

	var
		iterations, passes: uint;
		timeEstimationStage: (IfNeedToEstimate, EstimationInProcess, EstimationCompleted);
		t: Ticks;
		curTime, curOneTime, bestOneTime: float;
	begin
		passes := 0;
		emergencyRetries := 0;
		iterations := 1;
		timeEstimationStage := IfNeedToEstimate;
		NoteSection(one.name, ItemSection, no);

		repeat
			OnePass(iterations, t, curTime);

			if curTime >= MinPassTime then
			begin
				case timeEstimationStage of
					IfNeedToEstimate, EstimationInProcess:
						begin
							if timeEstimationStage = EstimationInProcess then NoteSection(one.name, ItemPreparationSection, yes);
							NoteSection(one.name, ItemBenchmarkSection, no);
							timeEstimationStage := EstimationCompleted;
							NoteProgress(one.name, 0);
						end;
				end;

				curOneTime := curTime / iterations;
				if passes = 0 then bestOneTime := curOneTime else bestOneTime := min(bestOneTime, curOneTime);
				inc(passes);
				if passes = PassesCount then break;
				NoteProgress(one.name, passes / PassesCount);
				iterations := AdjustIterations(iterations, PassTimeAim / curTime);
			end else
			begin
				case timeEstimationStage of
					IfNeedToEstimate:
						begin
							timeEstimationStage := EstimationInProcess;
							NoteSection(one.name, ItemPreparationSection, no);
						end;
					EstimationCompleted:
						if not EmergencyRetry then
							raise Error('{0}: время итераций расходится с прежней оценкой ({1}, мин. {2}).', one.name, TimeToString(curTime), TimeToString(MinPassTime));
				end;

				if (t.Internal >= MinJudgeableInternal) and (curTime >= MinJudgeableTime) then
					iterations := AdjustIterations(iterations, PassTimeAim / curTime)
				else
					iterations := AdjustIterations(iterations, UninformedMultiplier);
			end;
		until no;

		Assert(timeEstimationStage = EstimationCompleted);
		NoteProgress(one.name, 1);
		NoteSection(one.name, ItemBenchmarkSection, yes);
		NoteSection(one.name, ItemSection, yes);
		oneTime := bestOneTime;
	end;

	procedure Benchmark.NoteSection(const itemName: string; section: SectionEnum; finished: boolean);
	begin
		case _onSection.kind of
			Parametrized: _onSection.para(itemName, section, finished, self);
			Unparametrized: _onSection.unpara(itemName, section, finished);
		end;
	end;

	procedure Benchmark.NoteProgress(const itemName: string; const progress: float);
	begin
		case _onProgress.kind of
			Parametrized: _onProgress.para(itemName, progress, self);
			Unparametrized: _onProgress.unpara(itemName, progress);
		end;
	end;

	procedure Benchmark.NoteResult(const itemName: string; const oneTime: float; isBest: boolean; const bestTimes: float);
	begin
		case _onResult.kind of
			Parametrized: _onResult.para(itemName, oneTime, isBest, bestTimes, self);
			Unparametrized: _onResult.unpara(itemName, oneTime, isBest, bestTimes);
		end;
	end;

	operator :=(proc: Benchmark.ParaInitDone):   Benchmark.InitDoneProc; begin result.kind := Benchmark.Parametrized; result.para := proc; end;
	operator :=(proc: Benchmark.UnparaInitDone): Benchmark.InitDoneProc; begin result.kind := Benchmark.Unparametrized; result.unpara := proc; end;
	operator :=(proc: Benchmark.ParaTest):       Benchmark.TestProc;     begin result.kind := Benchmark.Parametrized; result.para := proc; end;
	operator :=(proc: Benchmark.UnparaTest):     Benchmark.TestProc;     begin result.kind := Benchmark.Unparametrized; result.unpara := proc; end;
	operator :=(proc: Benchmark.ParaSection):    Benchmark.SectionProc;  begin result.kind := Benchmark.Parametrized; result.para := proc; end;
	operator :=(proc: Benchmark.UnparaSection):  Benchmark.SectionProc;  begin result.kind := Benchmark.Unparametrized; result.unpara := proc; end;
	operator :=(proc: Benchmark.ParaProgress):   Benchmark.ProgressProc; begin result.kind := Benchmark.Parametrized; result.para := proc; end;
	operator :=(proc: Benchmark.UnparaProgress): Benchmark.ProgressProc; begin result.kind := Benchmark.Unparametrized; result.unpara := proc; end;
	operator :=(proc: Benchmark.ParaResult):     Benchmark.ResultProc;   begin result.kind := Benchmark.Parametrized; result.para := proc; end;
	operator :=(proc: Benchmark.UnparaResult):   Benchmark.ResultProc;   begin result.kind := Benchmark.Unparametrized; result.unpara := proc; end;

end.
