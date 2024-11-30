program TestDigital;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

uses
  defs, digital;

const
  BtnPin = 2;
  LedPin = 13; //internal LED

var
  iVal: integer;
begin
  PinMode(BtnPin, INPUT_PULLUP);

  PinMode(LedPin, OUTPUT);
  DigitalWrite(ledPin, LOW);

  while True do
  begin
    iVal:=DigitalRead(btnPin);
    DigitalWrite(ledPin, iVal)
  end;
end.

