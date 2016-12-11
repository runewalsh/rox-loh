{$ifndef procname} {$define procname := sort} {$endif}
{$ifdef sort_less} {$error} {$endif}
{$define sort_less := ({$ifdef less} less {$else} _1 < _2 {$endif}) {$undef _1} {$undef _2}}

	procedure procname(var p: elem_ptr);
	var
		ha, hb, ca, cb: elem_ptr;
		c: elem_ptr;
		nexta: boolean;
	begin
		if not Assigned(p) or not Assigned(p^.next) then exit;
		ha := p; ca := p;
		hb := p^.next; cb := p^.next;
		nexta := yes;
		c := p^.next^.next;
		while Assigned(c) do
		begin
			if nexta then
			begin
				ca^.next := c;
				ca := c;
			end else
			begin
				cb^.next := c;
				cb := c;
			end;
			nexta := not nexta;
			c := c^.next;
		end;
		ca^.next := nil;
		cb^.next := nil;

		procname(ha);
		procname(hb);
		if {$define _1 := ha^} {$define _2 := hb^} sort_less then
		begin
			p := ha;
			ha := ha^.next;
		end else
		begin
			p := hb;
			hb := hb^.next;
		end;
		c := p;
		while Assigned(ha) and Assigned(hb) do
		begin
			if {$define _1 := ha^} {$define _2 := hb^} sort_less then
			begin
				c^.next := ha;
				c := ha;
				ha := ha^.next;
			end else
			begin
				c^.next := hb;
				c := hb;
				hb := hb^.next;
			end;
		end;
		if Assigned(ha) then c^.next := ha else c^.next := hb;
	end;
{$undef less} {$undef sort_less}
{$undef elem_ptr}
{$undef procname}