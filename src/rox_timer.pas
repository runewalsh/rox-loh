{$include opts.inc}
unit rox_timer;

interface

uses
	USystem;

type
	pTimer = ^Timer;
	Timer = object(&Object)
	type
		DoneReason = (Timeout, Stopped, Emergency);
		ProcessCallback = procedure(const dt: float; param: pointer);
		DoneCallback = procedure(reason: DoneReason; param: pointer);
	var
		left: float;
		onProcess: ProcessCallback;
		onDone: DoneCallback;
		param: pointer;

		constructor Init(const timeout: float; onProcess: ProcessCallback; onDone: DoneCallback; param: pointer);
		destructor Done; virtual;
		procedure Update(const dt: float);
		function Dead: boolean;
		procedure Stop;
	private
		procedure ShotDone(reason: DoneReason);
	end;

implementation

	constructor Timer.Init(const timeout: float; onProcess: ProcessCallback; onDone: DoneCallback; param: pointer);
	begin
		inherited Init;
		self.left := timeout;
		self.onProcess := onProcess;
		self.onDone := onDone;
		self.param := param;
	end;

	destructor Timer.Done;
	begin
		ShotDone(Emergency);
		inherited Done;
	end;

	procedure Timer.Update(const dt: float);
	var
		nt: float;
	begin
		nt := left - dt;
		if Assigned(onProcess) then onProcess(dt, param);
		if (left >= 0) and (nt < 0) then ShotDone(Timeout);
		left := nt;
	end;

	function Timer.Dead: boolean;
	begin
		result := left < 0;
	end;

	procedure Timer.Stop;
	begin
		ShotDone(Stopped);
		left := -1;
	end;

	procedure Timer.ShotDone(reason: DoneReason);
	var
		dn: DoneCallback;
	begin
		if Assigned(onDone) then
		begin
			dn := onDone;
			onDone := nil;
			dn(reason, param);
		end;
	end;

end.

