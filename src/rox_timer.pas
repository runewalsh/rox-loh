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
		ProcessCallback = procedure(timer: pTimer; const dt: float; param: pointer);
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
		procedure Emergency;
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
		ShotDone(DoneReason.Emergency);
		inherited Done;
	end;

	procedure Timer.Update(const dt: float);
	var
		nt: float;
		justCompleted: boolean;
	begin
		nt := left - dt;
		justCompleted := (left >= 0) and (nt < 0);
		left := nt;
		if Assigned(onProcess) then
		begin
			onProcess(@self, dt, param);
			justCompleted := justCompleted and (left < 0);
		end;
		if justCompleted then ShotDone(Timeout);
	end;

	function Timer.Dead: boolean;
	begin
		result := left < 0;
	end;

	procedure Timer.Stop;
	begin
		ShotDone(Stopped);
	end;

	procedure Timer.Emergency;
	begin
		ShotDone(DoneReason.Emergency);
	end;

	procedure Timer.ShotDone(reason: DoneReason);
	var
		dn: DoneCallback;
	begin
		left := -1;
		if Assigned(onDone) then
		begin
			dn := onDone;
			onDone := nil;
			dn(reason, param);
		end;
	end;

end.

