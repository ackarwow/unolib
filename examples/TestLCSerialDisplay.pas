program TestLCSerialDisplay;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

uses
  timer, liquidcrystal, hardwareserial;

begin
  LC.Init(12, 11, 5, 4, 3, 2);
  LC._begin(16, 2);
  Serial.Start(9600);

  while True do
  begin
    if Serial.Available>0 then
    begin
      Delay(100);

      LC.Clear;
      while Serial.Available>0 do
        LC.WriteChar(Serial.ReadChar);
    end;
  end;
end.
