program TestLCAutoscroll;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

uses
  timer, liquidcrystal;

var
  i: UInt8;
  s: string;
begin
  LC.Init(12, 11, 5, 4, 3, 2);
  LC._begin(16, 2);

  while True do
  begin
    LC.SetCursor(0,0);
    for i:=0 to 9 do
    begin
      Str(i, s);
      LC.Write(s);
      Delay(500);
    end;

    LC.SetCursor(16,1);
    LC.Autoscroll;
    for i:=0 to 9 do
    begin
      Str(i, s);
      LC.Write(s);
      Delay(500);
    end;

    LC.NoAutoscroll;
    LC.Clear;
  end;
end.
