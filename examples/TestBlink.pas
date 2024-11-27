program TestBlink;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

uses
  defs, timer, digital;

const
  LedPin = 13; //internal LED

begin
  PinMode(LedPin, OUTPUT);
  DigitalWrite(ledPin, LOW);

  while True do
  begin
    DigitalWrite(ledPin, HIGH);
    Delay(1000);
    DigitalWrite(ledPin, LOW);
    Delay(1000);
  end;
end.
