{$include opts.inc}
unit rox_paths;

interface

uses
	USystem;

	function Cursor(const filename: string): string;
	function UI(const filename: string): string;
	function Environment(const filename: string): string;
	function Fx(const filename: string): string;
	function Dialogue(const char, filename: string): string;
	function Face(const char, filename: string): string;
	function Character(const char, filename: string): string;
	function Music(const filename: string): string;

implementation

	function Cursor(const filename: string): string;          begin result := UI('cursor/' + filename); end;
	function UI(const filename: string): string;              begin result := Paths.Data + 'ui/' + filename; end;
	function Environment(const filename: string): string;     begin result := Paths.Data + 'environment/' + filename; end;
	function Fx(const filename: string): string;              begin result := Paths.Data + 'fx/' + filename; end;
	function Dialogue(const char, filename: string): string;  begin result := Character(char, 'dialogue/' + filename); end;
	function Face(const char, filename: string): string;      begin result := Character(char, 'face/' + filename); end;
	function Character(const char, filename: string): string; begin result := Paths.Data + 'character/' + char + '/' + filename; end;
	function Music(const filename: string): string;           begin result := Paths.Data + 'BGM/' + filename; end;

end.

