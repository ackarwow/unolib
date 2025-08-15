program TestSerial;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

{
Connection parameters:
Bauds: 9600
Data bits: 8
Stop bits: 1
Parity: None
Line Ending: New Line

How to test via serial port monitor:
type ON to turn on internal LED
type OFF to turn off internal LED
}

uses
  timer, defs, digital, hardwareserial;

const
  LedPin = 13; //internal LED

var
  cReceived: char;
  sInData: shortstring;
begin
  PinMode(LedPin, OUTPUT);
  Serial.Start(9600);
  Serial.WriteLn('Serial connection started, waiting for instructions...');
  sInData:='';

  while True do
  begin
    while (Serial.Available>0) do
    begin
      cReceived:=Serial.ReadChar;
      // Process message when new line character is recieved
      if (cReceived = #10) then
      begin
        Serial.Write('My Arduino received: ');
        Serial.WriteLn(sInData);

        if (sInData = 'ON') then
        begin
          Serial.WriteLn('LED ON');
          DigitalWrite(ledPin, HIGH);
        end;

        if (sInData = 'OFF') then
        begin
          Serial.WriteLn('LED OFF');
          DigitalWrite(ledPin, LOW);
        end;

        sInData:= ''; // Clear recieved buffer
      end
      else
        sInData:=sInData+cReceived;
    end;
  end;
end.

