unit OpenGL_Hacks;

{$include opts.inc}

interface

uses
	USystem, OpenGL_Headers;

	procedure NoVAO_Replace2ImplicitImmediate;

implementation

uses
	U_GL;

var
	boundVb: gl.uint = 0;
	activeVA: sint = -1;
	vaCount: sint = 0;
	va: array of record
		used: boolean;
		ib: gl.enum;
		enabledAttribs: set of 0 .. MaxVertexAttributes - 1;
		attr: array[0 .. MaxVertexAttributes - 1] of record
			vb: gl.enum;
			size: gl.int;
			typ: gl.enum;
			norm: gl.boolean;
			stride: gl.sizei;
			vbOfs: gl.intptr;
		end;
	end;
	freeVAcount: sint = 0;
	freeVAids: array of gl.uint = nil;
	oldglBindBuffer: procedure(target: gl.enum; buffer: gl.uint); stdcall;
	oldglEnableVertexAttribArray: procedure(index: gl.uint); stdcall;
	oldglDisableVertexAttribArray: procedure(index: gl.uint); stdcall;
	oldglVertexAttribPointer: procedure(index: gl.uint; size: gl.int; type_: gl.enum; normalized: gl.boolean; stride: gl.sizei; pointer: pointer); stdcall;

	procedure Replace_glBindVertexArray(vao: gl.uint); stdcall;
	var
		i, ai: sint;
		bvb, nvb: gl.uint;
	begin
		ai := sint(vao) - 1;
		Assert((ai < vaCount) and ((ai < 0) or va[ai].used));
		if activeVA >= 0 then
			for i := 0 to MaxVertexAttributes - 1 do
			begin
				if (i in va[activeVA].enabledAttribs) and not ((ai >= 0) and (i in va[ai].enabledAttribs)) then
					oldglDisableVertexAttribArray(i);
			end;
		activeVA := ai;
		if ai < 0 then exit;

		oldglBindBuffer(gl.ELEMENT_ARRAY_BUFFER, va[ai].ib);
		bvb := boundVb;
		for i := 0 to MaxVertexAttributes - 1 do
			if i in va[ai].enabledAttribs then
			begin
				oldglEnableVertexAttribArray(i);
				nvb := va[ai].attr[i].vb;
				if bvb <> nvb then oldglBindBuffer(gl.ARRAY_BUFFER, nvb);
				bvb := nvb;
				oldglVertexAttribPointer(i,
					va[ai].attr[i].size,
					va[ai].attr[i].typ, va[ai].attr[i].norm, va[ai].attr[i].stride,
					NULL + va[ai].attr[i].vbOfs);
			end else
				oldglDisableVertexAttribArray(i);
		boundVb := bvb;
	end;

	procedure Replace_glDeleteVertexArrays(n: gl.sizei; arrays: gl.pUint); stdcall;
	var
		i: sint;
	begin
		for i := 0 to n - 1 do
			if (arrays[i] > 0) and (sint64(arrays[i]) <= vaCount) then
				if va[arrays[i] - 1].used then
				begin
					va[arrays[i] - 1].used := no;
					inc(freeVAcount);
					if freeVAcount >= length(freeVAids) then SetLength(freeVAids, 2 * freeVAcount);
					freeVAids[freeVAcount - 1] := arrays[i] - 1;
				end;
	end;

	procedure Replace_glGenVertexArrays(n: gl.sizei; arrays: gl.pUint); stdcall;
	var
		i: sint;
	begin
		for i := 0 to n - 1 do
		begin
			arrays[i] := 0;
			if freeVAcount > 0 then
			begin
				arrays[i] := freeVAids[freeVAcount - 1] + 1;
				dec(freeVAcount);
			end else
			begin
				inc(vaCount);
				if vaCount >= length(va) then SetLength(va, 2 * vaCount);
				arrays[i] := vaCount;
			end;
			with va[arrays[i] - 1] do
			begin
				ib := 0;
				used := yes;
				enabledAttribs := [];
			end;
		end;
	end;

	procedure Extend_glBindBuffer(target: gl.enum; buffer: gl.uint); stdcall;
	begin
		if target = gl.ARRAY_BUFFER then boundVb := buffer;
		if activeVA >= 0 then
			case target of
				gl.ELEMENT_ARRAY_BUFFER: va[activeVA].ib := buffer;
			end;
		oldglBindBuffer(target, buffer);
	end;

	procedure Extend_glEnableVertexAttribArray(index: gl.uint); stdcall;
	begin
		Assert(activeVA >= 0);
		oldglEnableVertexAttribArray(index);
		va[activeVA].enabledAttribs += [index];
	end;

	procedure Extend_glDisableVertexAttribArray(index: gl.uint); stdcall;
	begin
		Assert(activeVA >= 0);
		oldglDisableVertexAttribArray(index);
		va[activeVA].enabledAttribs -= [index];
	end;

	procedure Extend_glVertexAttribPointer(index: gl.uint; size: gl.int; type_: gl.enum; normalized: gl.boolean; stride: gl.sizei; pointer: pointer); stdcall;
	begin
		Assert(activeVA >= 0);
		oldglVertexAttribPointer(index, size, type_, normalized, stride, pointer);
		va[activeVA].attr[index].vb := boundVb;
		va[activeVA].attr[index].size := size;
		va[activeVA].attr[index].typ := type_;
		va[activeVA].attr[index].norm := normalized;
		va[activeVA].attr[index].stride := stride;
		va[activeVA].attr[index].vbOfs := pointer - NULL;
	end;

	procedure NoVAO_Replace2ImplicitImmediate;
	var
		emptyVAO: gl.uint;
	begin
		if Assigned(gl.GenVertexArrays) then
		begin
			gl.GenVertexArrays(1, @emptyVAO);
			gl.BindVertexArray(emptyVAO);
		end;

		pointer(oldglBindBuffer)               := gl.BindBuffer;
		pointer(oldglEnableVertexAttribArray)  := gl.EnableVertexAttribArray;
		pointer(oldglDisableVertexAttribArray) := gl.DisableVertexAttribArray;
		pointer(oldglVertexAttribPointer)      := gl.VertexAttribPointer;
		pointer(gl.BindBuffer)                  := @Extend_glBindBuffer;
		pointer(gl.EnableVertexAttribArray)     := @Extend_glEnableVertexAttribArray;
		pointer(gl.DisableVertexAttribArray)    := @Extend_glDisableVertexAttribArray;
		pointer(gl.VertexAttribPointer)         := @Extend_glVertexAttribPointer;
		pointer(gl.BindVertexArray)             := @Replace_glBindVertexArray;
		pointer(gl.GenVertexArrays)             := @Replace_glGenVertexArrays;
		pointer(gl.DeleteVertexArrays)          := @Replace_glDeleteVertexArrays;
	end;

end.
