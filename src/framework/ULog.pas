unit ULog;

{$define arrayofconst} {$include opts.inc}
{$ifNdef Debug} {$error Dedicated for debug} {$endif}
{$if defined(use_console) and not defined(dont_mirror_log_in_console)} {$define mirror_in_console} {$endif}

interface

uses
	USystem, UMath, Random, Streams;

type
	LogMessageStyle =
	(
		logTime,
		logPlain, logOk, logError, logWarning, logDebug,
		logName, logNumber, logSymbol, logDisabled,
		logBlood
	);

	pFileLogger = ^FileLogger;
	FileLogger = object
	type
		pSelf = pFileLogger;
	private
		f: &File;
		piece: string;
		startTime: Ticks;
		lock: RecursiveThreadLock;
		lastStyle: LogMessageStyle;
		styleOpened, lineOpened: boolean;
		quote: char;
		rng: RNG;
		function HTMLFix(const s: string): string;
		procedure StartStyle(style: LogMessageStyle);
		procedure EndStyle;
		procedure ChangeStyle(style: LogMessageStyle);
		procedure StartLine(const time: Ticks);
		procedure EndLine;
		procedure ResetLine(const time: Ticks);
		procedure WriteStartInfo;
		procedure WriteEndInfo;
		procedure FlushPiece;
	public
		procedure Init(const fileName: string);
		procedure Done;
	{$define constructor_args := const fileName: string} {$include dyn_obj.h.inc}
		procedure Add(const s: string; style: LogMessageStyle = logPlain);
		procedure Add(const s: string; const time: Ticks; style: LogMessageStyle = logPlain);
		function TimeStr(const time: Ticks): string;

{$ifdef mirror_in_console}
	type
		pWriteToConsoleParam = ^WriteToConsoleParam;
		WriteToConsoleParam = record
			s: string;
			thensetcolor: boolean;
			color: Color;
			next: pWriteToConsoleParam;
		end;
	var
		allWritten: ThreadCV;
		writesSync: PendingSync;
		firstWrite, lastWrite: pWriteToConsoleParam;
		writing: boolean;
		procedure QueueWrite(const s: string; color: pColor);
		procedure QueueWrite(const s: string; const color: Color);
		procedure QueueWrite(const s: string);
{$endif}
	end;

	procedure Log(const s: string; style: LogMessageStyle = logPlain);
	procedure LogR(const s: string; style: LogMessageStyle = logPlain);
{$define func:=
	procedure Log(const s: string; const _ARGS_: string; style: LogMessageStyle = logPlain);
	procedure LogR(const s: string; const _ARGS_: string; style: LogMessageStyle = logPlain);} {$include variadic.inc}
	procedure Log(const s: string; const args: array of const; style: LogMessageStyle = logPlain);
	procedure LogR(const s: string; const args: array of const; style: LogMessageStyle = logPlain);

implementation

uses
	Utils {$ifdef selftest}, Tests {$endif};

const
	Styles: array[LogMessageStyle] of record
		htmlClass: string;
		color: record
			html: string[6];
			console: array[0 .. 2] of float;
		end;
	end =
	(
	{$define s := string}
		(htmlClass: s('c'); color: (html: '646464'; console: ($64/$FF, $64/$FF, $64/$FF))), // time - dark gray
		(htmlClass: s('t'); color: (html: 'FFFFFF'; console: ($80/$FF, $80/$FF, $80/$FF))), // plain - white
		(htmlClass: s('o'); color: (html: '90FF90'; console: ($90/$FF, $FF/$FF, $90/$FF))), // ok - green
		(htmlClass: s('e'); color: (html: 'FF4040'; console: ($FF/$FF, $40/$FF, $40/$FF))), // error   - red
		(htmlClass: s('w'); color: (html: 'FFFF40'; console: ($FF/$FF, $FF/$FF, $40/$FF))), // warning - yellow
		(htmlClass: s('d'); color: (html: '8080FF'; console: ($80/$FF, $80/$FF, $FF/$FF))), // debug - light blue
		(htmlClass: s('n'); color: (html: '00FFFF'; console: ($00/$FF, $FF/$FF, $FF/$FF))), // name
		(htmlClass: s('N'); color: (html: '00AA80'; console: ($00/$FF, $AA/$FF, $80/$FF))), // number
		(htmlClass: s('s'); color: (html: '00AA00'; console: ($00/$FF, $AA/$FF, $00/$FF))), // symbol
		(htmlClass: s('h'); color: (html: '444444'; console: ($44/$FF, $44/$FF, $44/$FF))), // disabled
		(htmlClass: s('b'); color: (html: 'FF2020'; console: ($FF/$FF, $20/$FF, $20/$FF)))  // blood
	{$undef s}
	);
	DefaultHtmlStyle = logPlain;

	function FileLogger.TimeStr(const time: Ticks): string;
	const
		TimeDigits = 8;
	var
		t: Ticks;
	begin
		t := time - startTime;
		result := ToString(t.ToIMilliseconds, IntFormat.Padding(TimeDigits));
		if length(result) > TimeDigits then
			delete(result, 1, length(result) - TimeDigits);
		result := '[' + result + ']';
	end;

	function DescribeUnit(id: uint; param: pointer): string;
	var
		units: ^UnitRegistry absolute param;
	begin
		result := units^.list.items[id].name;
		if units^.list.items[id].priority <> 0 then
			result += Format(' ({0}{1})', [IfThen(units^.list.items[id].priority > 0, '+', ''), units^.list.items[id].priority]);
	end;

{$ifdef selftest}
	function DescribeTest(id: uint; param: pointer): string;
	var
		f: ^TestSuite.FeatureRec absolute param;
	begin
		if f[id].passed = f[id].total then
			result := Format('{0} x{1}', [f[id].name, f[id].total])
		else
			result := Format('{0} ({1}/{2})', [f[id].name, f[id].passed, f[id].total]);
	end;
{$endif}

	procedure FileLogger.WriteStartInfo;
	const
		Envs: array[0 .. 8] of string =
		(
			'COMPUTERNAME', 'LOGONSERVER',
			'PROCESSOR_IDENTIFIER', 'PROCESSOR_ARCHITECTURE', 'PROCESSOR_LEVEL', 'PROCESSOR_REVISION',
			'OS', 'HOMEPATH', 'PATH'
		);
		SystemFeats: array[SystemFeature] of string = ('slim R/W locks', 'условные переменные');
	var
		cenv: string;
		i: sint;
		feat: SystemFeature;
		feats: array of string;
		enab: boolean;
	begin
		Add('Лог начат, ' + ToString(DateTime.GetLocal) + EOL);
		Add('☣ ', logOK);
		Add('Компилятор: FPC, версия ' + {$INCLUDE %FPCVersion%} + ', целевой CPU - ' + {$INCLUDE %FPCTargetCPU%} + ', целевая ОС - ' + {$INCLUDE %FPCTargetOS%} + EOL);
		Add('Билд v' + GetExecVersion + ', собран ' + {$include %date%} + ' ' + {$include %time%} + EOL);
		Add(Format('SizeOf(pointer / size_t / sint / float / hp_float) = {0}/{1}/{2}/{3}/{4}' + EOL,
			[sizeof(pointer), sizeof(size_t), sizeof(sint), sizeof(float), sizeof(hp_float)]), logDebug);
		Add(Format('SizeOf(vec2 / vec3 / vec4) = {0}/{1}/{2}' + EOL, [sizeof(Vec2), sizeof(Vec3), sizeof(Vec4)]), logDebug);
	{$ifdef Profile} Add('Включена профилировка.' + EOL, logWarning); {$endif}
		Add(Format('Частота системного таймера: {0}' + EOL, ToStringSuff(Ticks.InternalFrequency)), logDebug);
		Add('Глобальный ГПСЧ: ' + GlobalRNG.Describe + EOL, logDebug);

		Add(Format('Количество процессоров (ядер): {0}', [SystemInfo.nCPUs]) + EOL);
		SetLength(feats, ord(High(feat)) + 1);
		for enab := no to yes do
		begin
			i := 0;
			for feat in SystemFeature do
				if enab = (feat in SystemInfo.features) then
				begin
					feats[i] := SystemFeats[feat];
					inc(i);
				end;
			if i > 0 then
				Add(Format('{0} {1}.' + EOL, IfThen(enab, 'Доступны', 'Недоступны'), SeparatedList.Join(Slice(feats, i), ', ' + SeparatedList.LastSep + ' и ')),
						LogMessageStyle(IfThen(enab, ord(logOK), ord(logDebug))));
		end;

		Add('Гранулярность резервирования памяти: ' + ToStringSuff_b(SystemInfo.allocationGranularity) + ', '
				+ 'размер страницы: ' + ToStringSuff_b(SystemInfo.pageSize) + EOL, logDebug);

		Add(Format('Исполняемый файл: {0}, PID = {1}' + EOL, [StreamPath.Log(ExecFileName), GetProcessID]));

		cenv := CommandLine.Raw;
		if cenv <> '' then
		begin
			Add('Командная строка: ');
			Add(cenv + EOL, logDebug);
		end;

		for i := Low(Envs) to High(Envs) do
		begin
			cenv := GetEnv(Envs[i]);
			if length(cenv) > 0 then Add(Envs[i] + ' = "' + cenv + '"' + EOL, logDebug);
		end;
		Add('Порядок инициализации модулей: ' + SeparatedList.Join(units.list.N, @DescribeUnit, @units, ' → ' + SeparatedList.Empty + '(пусто)') + EOL, logDebug);
	{$ifdef selftest}
		Add('Тесты пройдены: ' + SeparatedList.Join(length(TestSuite.CompletedRegistry), @DescribeTest, pointer(TestSuite.CompletedRegistry), ', ' + SeparatedList.Empty + '(нет)') + EOL, logDebug);
		TestSuite.ClearRegistry;
	{$endif}
	end;

	procedure FileLogger.WriteEndInfo;
	const
		MaxMsg: array[Statistics.MaxEnum] of string =
		(
			'Максимальное значение счётчика ссылок: {0} (предел: {1})',
			'Максимальная длина цепочки onDestroy: {0} (сработавшей: {1})', '',
			'Максимальное количество задач, одновременно назначенных пулу потоков',
			'Максимальное количество одновременных асинхронных I/O запросов',
			'Максимальное число одновременных отображений одного файла',
			'Максимальное число записей в хэш-таблице',
			'Максимальная длина цепочки коллизий',
			'Максимальная длина последовательности коллизий открытой адресации',
			'Максимальный размер пула строк',
			'Максимальное число ссылок на строку',
			'Максимальная глубина KD-дерева: {0} (предел: {1})',
			'Максимальное число одновременных контактов (Newton): {0} (предел: {1})',
			'Максимальное количество юниформов в шейдере',
			'Максимальное количество рёбер, инцидентных одной вершине',
			'Максимальное количество юниформов на инстанс',
			'Максимальное количество VAO в меше',
			'Максимальное количество функций в множественном делегате',
			'Максимальный ID скриптового делегата',
			'Максимальное количество одновременно выполняемых сущностью действий (включая таймеры)',
			'Максимальное количество GL-материалов в пуле',
			'Максимальное количество физических примитивов в пуле',
			'Максимальное количество частиц в системе',
			'Максимум повторных попыток генерации нормально распределённого числа',
			'Максимум повторных попыток генерации равномерно распределённого числа'
		);
		TotalMsg: array[Statistics.TotalEnum] of string =
		(
			'Создано объектов', 'Ложных срабатываний CV: {0} ({1}%)', '',
			'Всего задач назначено пулу потоков',
			'Выполнено истинно асинхронных I/O запросов',
			'Синхронно выполнено асинхронных I/O запросов',
			'Переиспользованных отображений',
			'Срабатываний таймеров',
			'Отменено не закончивших выполнение таймеров',
			'', 'Средняя длина цепочки коллизий: {0}',
			'', 'Средняя длина последовательности коллизий открытой адресации: {0}',
			'Переброшено элементов с открытой адресацией ближе к исходной позиции',
			'Средний размер блока памяти Lua: {0}. Выделено для строк: {1}%, таблиц: {2}%, функций: {3}%, юзердат: {4}%, потоков: {5}%, остального: {6}%',
			'Вызовов поиска пути: {0} (в среднем по {1})',
			'', 'Экстренных прерываний QSort: {0} / {1}',
			'', 'Денормализованных Vec.MaybeNormalized: {0} ({1}%)',
			'', 'Денормализованных Quaternion.MaybeNormalized: {0} ({1}%)',
			'', 'Освобождено нулевых ссылок в DynObj.Free: {0} ({1}%)',
			'Повторных попыток оценки оверхеда таймера'
		);
		CurrentAndMaxMsg: array[Statistics.CurrentAndMax] of string =
		(
			'Максимальное количество одновременно существующих объектов',
			'Максимальное количество задач, одновременно выполняемых пулом потоков',
			'Максимальное число одновременно открытых файлов'
		);
	var
		msg: string;
		max: Statistics.MaxEnum;
		total: Statistics.TotalEnum;
		cam: Statistics.CurrentAndMax;
	begin
		Add('Оверхед таймера: ' + TimeToString(Ticks.Overhead(Ticks.InternalFrequency).ToSeconds / Ticks.InternalFrequency) + EOL, logDebug);
		for max in Statistics.MaxEnum do
			if stat.max[max] <> 0 then
			begin
				msg := MaxMsg[max];
				case max of
					max_object_refcount: Add(Format(msg, [stat.max[max], ulong(&Object.MAX_REFCOUNT)]) + EOL, logDebug);
					max_ondestroy_chain_len: Add(Format(msg, [stat.max[max], stat.max[max_real_ondestroy_chain_len]]) + EOL, logDebug);
					max_kd_depth: Add(Format(msg, [stat.max[max], stat.jv[kd_depth_limit]]) + EOL, logDebug);
					max_rigid_contacts: Add(Format(msg, [stat.max[max], stat.jv[rigid_contacts_limit]]) + EOL, logDebug);
					max_real_ondestroy_chain_len: ;
					else Add(msg + ': ' + ToString(stat.max[max]) + EOL, logDebug);
				end;
			end;

		for total in Statistics.TotalEnum do
			if stat.total[total] <> 0 then
			begin
				msg := TotalMsg[total];
				case total of
					total_wakeups, passed_chain_hash_elems, passed_open_hash_elems, qsorts, maybenormalized_vectors, maybenormalized_quaternions,
					dyn_obj_frees:
						;
					spurious_wakeups:
						Add(Format(msg, [stat.total[total], stat.total[total] / stat.total[total_wakeups] * 100]) + EOL, logDebug);
					chain_hash_searches: Add(Format(msg, [stat.total[passed_chain_hash_elems] / stat.total[total]]) + EOL, logDebug);
					open_hash_searches: Add(Format(msg, [stat.total[passed_open_hash_elems] / stat.total[total]]) + EOL, logDebug);
					n_lua_blocks:
						Add(Format(msg, [round(stat.totalSize[lua_total_blocks_mem] / stat.total[total]),
							stat.totalSize[lua_strings_mem] / stat.totalSize[lua_total_blocks_mem] * 100,
							stat.totalSize[lua_tables_mem] / stat.totalSize[lua_total_blocks_mem] * 100,
							stat.totalSize[lua_functions_mem] / stat.totalSize[lua_total_blocks_mem] * 100,
							stat.totalSize[lua_udatas_mem] / stat.totalSize[lua_total_blocks_mem] * 100,
							stat.totalSize[lua_threads_mem] / stat.totalSize[lua_total_blocks_mem] * 100,
							stat.totalSize[lua_other_mem] / stat.totalSize[lua_total_blocks_mem] * 100]) + EOL, logDebug);
					n_way_searches: Add(Format(msg, [stat.total[total], TimeToString(stat.totalHpf[way_searches_time] / stat.total[total])]) + EOL, logDebug);
					qsort_fallbacks: Add(Format(msg, [stat.total[total], stat.total[qsorts]]) + EOL, logDebug);
					maybenormalized_denorm_vectors: Add(Format(msg, [stat.total[total], stat.total[total] / stat.total[maybenormalized_vectors] * 100]) + EOL, logDebug);
					maybenormalized_denorm_quaternions: Add(Format(msg, [stat.total[total], stat.total[total] / stat.total[maybenormalized_quaternions] * 100]) + EOL, logDebug);
					dyn_obj_nil_frees: Add(Format(msg, [stat.total[total], stat.total[total] / stat.total[dyn_obj_frees] * 100]) + EOL, logDebug);
					else
						Add(msg + ': ' + ToString(stat.total[total]) + EOL,
							LogMessageStyle(IfThen(total in [ticks_overhead_estimation_retries], ord(logWarning), ord(logDebug))));
				end;
			end;

		for cam in Statistics.CurrentAndMax do
			if stat.cam[cam].max <> 0 then
			begin
				msg := CurrentAndMaxMsg[cam];
				Add(msg + ': ' + ToString(stat.cam[cam].max) + EOL, logDebug);
			end;

		Add(ToString(DateTime.GetLocal) + ' — лог завершён ');
		Add('☪', logBlood);
	end;

	function FileLogger.HTMLFix(const s: string): string;
	var
		i: sint;
	begin
		result := s;
		for i := length(result) downto 1 do
			if result[i] in ([#0 .. #31, '<', '>', '&'] - [EOL]) then
				case result[i] of
					TabSym: result := StrStuff(result, i, 1, '&emsp;');
					'&': result := StrStuff(result, i, 1, '&amp;');
					'<': result := StrStuff(result, i, 1, '&#60;');
					'>': result := StrStuff(result, i, 1, '&#62;');
					else result := StrStuff(result, i, 1, '#' + ToString(ord(s[i])));
				end;
	end;

	procedure FileLogger.StartStyle(style: LogMessageStyle);
	begin
		if style = DefaultHtmlStyle then exit;
		piece += '<span class="' + Styles[style].htmlClass + '">';
		styleOpened := yes;
	end;

	procedure FileLogger.EndStyle;
	var
		pos: size_t;
	begin
		pos := length(piece) + 1;
		while (pos > 1) and (piece[pos - 1] = ' ') do dec(pos);
		insert('</span>', piece, pos);
		styleOpened := no;
	end;

	procedure FileLogger.ChangeStyle(style: LogMessageStyle);
	begin
		if styleOpened then
			if style = lastStyle then exit else EndStyle;
		lastStyle := style;
		StartStyle(style);
	end;

	procedure FileLogger.StartLine(const time: Ticks);
	begin
		if styleOpened then EndStyle;
		piece += '<p>';
		lineOpened := yes;
		ChangeStyle(logTime);
		piece += TimeStr(time) + ' ';
	end;

	procedure FileLogger.EndLine;
	begin
		if styleOpened then EndStyle;
		piece += '</p>' + EOL;
		lineOpened := no;
	end;

	procedure FileLogger.ResetLine(const time: Ticks);
	begin
		if lineOpened then EndLine;
		StartLine(time);
	end;

	procedure FileLogger.Init(const fileName: string);
	var
		style: LogMessageStyle;
	begin
		lock.Init;
	{$ifdef mirror_in_console} allWritten.Init; {$endif}
		quote := #0;
		lastStyle := logBlood;
		startTime := Ticks.Get;
		piece := '';
		rng.Init(Tiny, [DontUseGlobalRNG]);
	{$ifdef mirror_in_console}
		writesSync.Init;
		firstWrite := nil;
		lastWrite := nil;
		writing := no;
	{$endif}

		try
			f := &File.Open(fileName + '.html', [file_Write]);
		except
			Exception.Show;
			f := &File.Invalid;
		end;
		if f.OK then
		begin
			piece +=
				'<!DOCTYPE html>' + EOL +
				'<html>' + EOL +
				'<head>' + EOL +
				'<meta charset="utf-8"/>' + EOL +
				'<title>Log</title>' + EOL +
				'<style type="text/css">' + EOL +
				'body { color: #' + Styles[DefaultHtmlStyle].color.html + '; background-color: #000000 }' + EOL;
			for style in LogMessageStyle do
				if style <> DefaultHtmlStyle then
					piece += '.' + Styles[style].htmlClass + ' { color: #' + Styles[style].color.html + ' }' + EOL;
			piece +=
				'</style>' + EOL +
				'</head>' + EOL +
				'<body>' + EOL;
			lineOpened := no;
			styleOpened := no;
			StartLine(Ticks.Get);
			WriteStartInfo;
		end;
	end;

{$ifdef mirror_in_console} function AllWritesFinished(param: pointer): boolean; begin result := not pFileLogger(param)^.writing; end; {$endif}

	procedure FileLogger.Done;
	begin
		WriteEndInfo;
	{$ifdef mirror_in_console} lock.Enter; allWritten.Wait(lock, @AllWritesFinished, @self); lock.Leave; {$endif}
		if lineOpened then EndLine;
		piece +=
			'</body>' + EOL +
			'</html>';
		FlushPiece;
		f.Close;
	{$ifdef mirror_in_console} writesSync.Done; allWritten.Done; {$endif}
		rng.Done;
		lock.Done;
	end;

{$define classname := FileLogger} {$define constructor_args := const fileName: string} {$define pass_constructor_args := fileName}
{$include dyn_obj.pp.inc}

	procedure FileLogger.FlushPiece;
	var
		n: size_t;
	begin
		n := length(piece);
		while (n > 0) and (piece[n] = ' ') do dec(n);
		if f.OK then f.Write(pointer(piece), n);
		if n = size_t(length(piece)) then piece := '' else delete(piece, 1, n);
	end;

	procedure FileLogger.Add(const s: string; style: LogMessageStyle = logPlain);
	begin
		Add(s, Ticks.Get, style);
	end;

	procedure FileLogger.Add(const s: string; const time: Ticks; style: LogMessageStyle = logPlain);
	var
		p: sint;
		neol: size_t;
		sym: UTFchar;
		syms: UTF8.CharBytes;
		symc, prevc: char;
		ns: LogMessageStyle;
	{$ifdef mirror_in_console}
		lcs: LogMessageStyle;
		toCon: string;
	{$endif}
	begin
		lock.Enter;

		if style = logError then ToStderr(s);
	{$ifdef mirror_in_console}
		toCon := '';
		lcs := logTime;
	{$endif}

		prevc := #0;
		p := 1;
		while UTF8.Next(s, p, sym) <> UTFInvalid do
		begin
			syms := UTF8.CodepointToString(sym);
			if length(syms) = 1 then symc := syms[1] else symc := #255;

			if quote = #0 then
			begin
				if symc in ['"',''''] then
				begin
					quote := symc;
					// continue;
				end;
			end else
				if symc = quote then
				begin
					quote := #0;
					// continue;
				end;

			ns := style;
			if (quote <> #0) or (symc in ['"','''']) then
				ns := logName
			else
				if (symc in ['0'..'9','.','-']) then
				begin
					if not ((prevc in ['a'..'z','A'..'Z']) or ((prevc in ['0'..'9','.','-','+']) and (not (lastStyle in [logNumber, logBlood])))) then
						ns := logNumber;
				end else
					if not (symc in [' ', 'A'..'Z', 'a'..'z', #128..#255]) then
						if style = logPlain then
							ns := logSymbol;
			if rng.GetUint(2048) = 0 then ns := logBlood;

		{$ifdef mirror_in_console}
			if IsConsole and (ns <> lcs) then
			begin
				lcs := ns;
				QueueWrite(toCon, Color.RGB(Styles[ns].color.console[0], Styles[ns].color.console[1], Styles[ns].color.console[2]));
				toCon := '';
			end;
		{$endif}
			if UTF8.IsEOL(@s[p - length(syms)], length(s) - (p - length(syms)) + 1, neol) then
			begin
				quote := #0;
				ResetLine(time);
			end else
			begin
				ChangeStyle(ns);
				piece += HTMLFix(syms);
			end;
		{$ifdef mirror_in_console} toCon += syms; {$endif}

			prevc := symc;
		end;

	{$ifdef mirror_in_console} QueueWrite(toCon); {$endif}
		FlushPiece;
		lock.Leave;
	end;

{$ifdef mirror_in_console}
	procedure WriteItems(param: pointer);
	var
		log: pFileLogger absolute param;
		item: FileLogger.pWriteToConsoleParam;
	begin
		log^.lock.Enter;
		repeat
			item := log^.firstWrite;
			if not Assigned(item) then
			begin
				log^.writing := no;
				break;
			end;

			if Assigned(item^.next) then
				log^.firstWrite := item^.next
			else
			begin
				log^.firstWrite := nil;
				log^.lastWrite := nil;
			end;
			log^.lock.Leave;
			Con.Write(item^.s);
			if item^.thensetcolor then Con.SetColor(item^.color);
			dispose(item);
			log^.lock.Enter;
			log^.writesSync.KillOne;
		until no;
		log^.allWritten.WakeAll;
		log^.lock.Leave;
	end;

	procedure FileLogger.QueueWrite(const s: string; color: pColor);
	var
		p: pWriteToConsoleParam;
	begin
		new(p);
		p^.next := nil;
		p^.s := s;
		p^.thensetcolor := Assigned(color);
		if Assigned(color) then p^.color := color^;

		writesSync.AddOne;
		if Assigned(lastWrite) then
			lastWrite^.next := p
		else
			firstWrite := p;
		lastWrite := p;
		if not writing then
		begin
			writing := yes;
			lock.Leave;
			Work.Queue(@WriteItems, @self);
			lock.Enter;
		end;
	end;

	procedure FileLogger.QueueWrite(const s: string; const color: Color); begin QueueWrite(s, @color); end;
	procedure FileLogger.QueueWrite(const s: string); begin QueueWrite(s, nil); end;
{$endif}

{$define accessor := sharedLog} {$define instance_type := pFileLogger}
{$define create_instance := FileLogger.Create(Paths.Logs + StreamPath.FilenameNoExt(ExecFileName) + '-' + DateTime.Start.ToCode)}
{$define instance_name := shared_log_instance} {$include lazy_singleton.inc}

	procedure Log(const s: string; style: LogMessageStyle = logPlain);
	begin
		LogR(s + EOL, style);
	end;

	procedure LogR(const s: string; style: LogMessageStyle = logPlain);
	begin
		sharedLog^.Add(s, style);
	end;

{$define func:=
	procedure Log(const s: string; const _ARGS_: string; style: LogMessageStyle = logPlain);
	begin
		Log(Format(s, _ARGS_), style);
	end;

	procedure LogR(const s: string; const _ARGS_: string; style: LogMessageStyle = logPlain);
	begin
		LogR(Format(s, _ARGS_), style);
	end;} {$include variadic.inc}

	procedure Log(const s: string; const args: array of const; style: LogMessageStyle = logPlain);
	begin
		Log(Format(s, args), style);
	end;

	procedure LogR(const s: string; const args: array of const; style: LogMessageStyle = logPlain);
	begin
		LogR(Format(s, args), style);
	end;

	procedure RemoveOldLogs;
	const
		MaxKeep = 8;
	type
		OneFile = record
			name: string;
		end;

		SessionLogs = record
			dateid: string;
			files: array of OneFile;
		end;

		// TODO: регексы -_-
		function ExtractID(const srcfn: string): string;
		const
			MinDigitsInPart = 4;
		var
			t: string;
			start, &end, reonfail, digstart: sint;
			ok: boolean;
		begin
			t := StreamPath.CutExtension(srcfn);
			start := 1;
			repeat
				ok := no;
				while (start <= length(t)) and not (t[start] in ['0' .. '9']) do inc(start);
				&end := start;

				digstart := &end;
				while (&end <= length(t)) and (t[&end] in ['0' .. '9']) do inc(&end);
				reonfail := &end;

				if (&end <= length(t)) and (t[&end] = '-') and (&end - &digstart >= MinDigitsInPart) then
				begin
					inc(&end);
					digstart := &end;
					while (&end <= length(t)) and (t[&end] in ['0' .. '9']) do inc(&end);
					ok := (&end - &digstart >= MinDigitsInPart) and (&end > length(t));
				end;
				if not ok then start := reonfail;
			until ok or (start > length(t));
			result := Copy(t, start, &end - start);
		end;

		{$define elem := SessionLogs} {$define less := _1.dateid < _2.dateid} {$define openarray} {$include sort.inc}

	var
		f: FoundFile;
		logs: array of SessionLogs;
		id: string;
		i, j: sint;
	begin
		logs := nil;
		for f in Folder.Scan(Paths.Logs, OnlyFiles) do
		begin
			id := ExtractID(f.name);
			if id = '' then continue;

			i := Index(id, pointer(logs) + fieldoffset SessionLogs _ dateid _, length(logs), sizeof(SessionLogs));
			if i < 0 then
			begin
				i := length(logs);
				SetLength(logs, i + 1);
				logs[i].dateid := id;
			end;

			SetLength(logs[i].files, length(logs[i].files) + 1);
			logs[i].files[High(logs[i].files)].name := f.SearchedName;
		end;

		if length(logs) > MaxKeep then
		begin
			sort(logs);
			for i := 0 to length(logs) - MaxKeep - 1 do
				for j := 0 to High(logs[i].files) do
					&File.Erase(logs[i].files[j].name);
		end;
	end;

	procedure LogException(const message: string);
	begin
		Log(message, logError);
	end;

	procedure Init;
	begin
		Exception.SetLogger(@LogException);
	end;

	procedure Done;
	begin
		Exception.SetLogger(nil);
		shared_log_instance^.Free(shared_log_instance);
		RemoveOldLogs;
	end;

initialization
	&Unit('Log').Initialize(@Init, @Done).Priority(+3);
end.
