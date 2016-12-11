{$if defined(count)} {$define max := count - 1}
{$elseif not defined(max)} {$error определи "max" или "count"} {$endif}
{$define nbits_expr := uint(trunc(ln(max) / ln(2) + 1))}

{$if defined(nbits)} nbits = nbits_expr; {$endif}
{$if defined(mask)} mask = 1 shl nbits_expr - 1; {$endif}
{$if not defined(nbits) and not defined(mask)} nbits_expr {$endif}

{$undef nbits_expr}
{$undef max} {$undef count} {$undef nbits} {$undef mask}
