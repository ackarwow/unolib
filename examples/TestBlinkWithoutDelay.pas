program TestBlinkWithoutDelay;

{
  created 2005 by David A. Mellis
  modified 8 Feb 2010 by Paul Stoffregen
  modified 11 Nov 2013 by Scott Fitzgerald
  modified 9 Jan 2017 by Arturo Guadalupi
  AVRPascal version Oct 2024 by Henk Heemstra
  modified 13 Oct 2024 by Andrzej Karwowski
}

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

uses
  defs, timer, digital;

const
  LED_BUILTIN = 13; //internal LED
  INTERVAL = 1000;

var
  LedState: UInt8 = LOW;
  PreviousMillis: UInt32 = 0;
  CurrentMillis: UInt32;

begin
  PinMode(LED_BUILTIN, OUTPUT);

  while True do
  begin
    CurrentMillis := Millis;
    if (CurrentMillis - PreviousMillis >= INTERVAL) then
    begin
      PreviousMillis := CurrentMillis;
      LedState := not LedState;
      DigitalWrite(LED_BUILTIN, LedState);
    end;
  end;
end.

