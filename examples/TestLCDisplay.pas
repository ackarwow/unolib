program TestLCDisplay;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

uses
  timer, liquidcrystal;

begin
  LC.Init(12, 11, 5, 4, 3, 2);
  LC._begin(16, 2);
  LC.Write('hello, world!');

  while True do
  begin
    LC.NoDisplay;
    Delay(500);

    LC.Display;
    Delay(500);
  end;
end.
