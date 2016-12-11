{$if times >= 1} {$define repid := 0} rep {$endif} {$if times >= 2} {$define repid := 1} rep {$endif}
{$if times >= 3} {$define repid := 2} rep {$endif} {$if times >= 4} {$define repid := 3} rep {$endif}
{$if times >= 5} {$define repid := 4} rep {$endif} {$if times >= 6} {$define repid := 5} rep {$endif}
{$if times >= 7} {$define repid := 6} rep {$endif} {$if times >= 8} {$define repid := 7} rep {$endif}
{$if times >= 9} {$define repid := 8} rep {$endif} {$if times >= 10} {$define repid := 9} rep {$endif}
{$if times >= 11} {$define repid := 10} rep {$endif} {$if times >= 12} {$define repid := 11} rep {$endif}
{$if times >= 13} {$define repid := 12} rep {$endif} {$if times >= 14} {$define repid := 13} rep {$endif}
{$if times >= 15} {$define repid := 14} rep {$endif} {$if times >= 16} {$define repid := 15} rep {$endif}
{$if times >= 17} {$error too many repeats} {$endif}
{$undef repid} {$undef times} {$undef rep}