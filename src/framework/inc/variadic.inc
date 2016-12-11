{$ifndef limit} {$define limit := 16} {$endif}

{$define _FOREACH_:=
	{$if _NARGS_>0 } {$define _ARG_:=arg0}  {$define _IARG_:=0}  {$if _NARGS_=1 } {$define _LAST_} {$endif} iterate {$endif}
	{$if _NARGS_>1 } {$define _ARG_:=arg1}  {$define _IARG_:=1}  {$if _NARGS_=2 } {$define _LAST_} {$endif} iterate {$endif}
	{$if _NARGS_>2 } {$define _ARG_:=arg2}  {$define _IARG_:=2}  {$if _NARGS_=3 } {$define _LAST_} {$endif} iterate {$endif}
	{$if _NARGS_>3 } {$define _ARG_:=arg3}  {$define _IARG_:=3}  {$if _NARGS_=4 } {$define _LAST_} {$endif} iterate {$endif}
	{$if _NARGS_>4 } {$define _ARG_:=arg4}  {$define _IARG_:=4}  {$if _NARGS_=5 } {$define _LAST_} {$endif} iterate {$endif}
	{$if _NARGS_>5 } {$define _ARG_:=arg5}  {$define _IARG_:=5}  {$if _NARGS_=6 } {$define _LAST_} {$endif} iterate {$endif}
	{$if _NARGS_>6 } {$define _ARG_:=arg6}  {$define _IARG_:=6}  {$if _NARGS_=7 } {$define _LAST_} {$endif} iterate {$endif}
	{$if _NARGS_>7 } {$define _ARG_:=arg7}  {$define _IARG_:=7}  {$if _NARGS_=8 } {$define _LAST_} {$endif} iterate {$endif}
	{$if _NARGS_>8 } {$define _ARG_:=arg8}  {$define _IARG_:=8}  {$if _NARGS_=9 } {$define _LAST_} {$endif} iterate {$endif}
	{$if _NARGS_>9 } {$define _ARG_:=arg9}  {$define _IARG_:=9}  {$if _NARGS_=10} {$define _LAST_} {$endif} iterate {$endif}
	{$if _NARGS_>10} {$define _ARG_:=arg10} {$define _IARG_:=10} {$if _NARGS_=11} {$define _LAST_} {$endif} iterate {$endif}
	{$if _NARGS_>11} {$define _ARG_:=arg11} {$define _IARG_:=11} {$if _NARGS_=12} {$define _LAST_} {$endif} iterate {$endif}
	{$if _NARGS_>12} {$define _ARG_:=arg12} {$define _IARG_:=12} {$if _NARGS_=13} {$define _LAST_} {$endif} iterate {$endif}
	{$if _NARGS_>13} {$define _ARG_:=arg13} {$define _IARG_:=13} {$if _NARGS_=14} {$define _LAST_} {$endif} iterate {$endif}
	{$if _NARGS_>14} {$define _ARG_:=arg14} {$define _IARG_:=14} {$if _NARGS_=15} {$define _LAST_} {$endif} iterate {$endif}
	{$if _NARGS_>15} {$define _ARG_:=arg15} {$define _IARG_:=15} {$if _NARGS_=16} {$define _LAST_} {$endif} iterate {$endif}
	{$undef iterate} {$undef _ARG_} {$undef _IARG_} {$ifndef _LAST_} {$error _LAST_ wasn't defined} {$endif} {$undef _LAST_}}

{$define _ARGS_ :=
	{$define iterate := {$ifdef conv} conv {$else} _ARG_ {$endif} {$ifndef _LAST_} {$ifdef sep} sep {$else}, {$endif} {$endif}} _FOREACH_
	{$undef sep} {$undef conv}}

{$if limit >= 1} {$define _NARGS_:=1} func {$endif} {$if limit >= 2} {$define _NARGS_:=2} func {$endif}
{$if limit >= 3} {$define _NARGS_:=3} func {$endif} {$if limit >= 4} {$define _NARGS_:=4} func {$endif}
{$if limit >= 5} {$define _NARGS_:=5} func {$endif} {$if limit >= 6} {$define _NARGS_:=6} func {$endif}
{$if limit >= 7} {$define _NARGS_:=7} func {$endif} {$if limit >= 8} {$define _NARGS_:=8} func {$endif}
{$if limit >= 9} {$define _NARGS_:=9} func {$endif} {$if limit >= 10} {$define _NARGS_:=10} func {$endif}
{$if limit >= 11} {$define _NARGS_:=11} func {$endif} {$if limit >= 12} {$define _NARGS_:=12} func {$endif}
{$if limit >= 13} {$define _NARGS_:=13} func {$endif} {$if limit >= 14} {$define _NARGS_:=14} func {$endif}
{$if limit >= 15} {$define _NARGS_:=15} func {$endif} {$if limit >= 16} {$define _NARGS_:=16} func {$endif}

{$undef _FOREACH_}
{$undef _NARGS_}
{$undef _ARGS_}
{$undef func} {$undef limit}