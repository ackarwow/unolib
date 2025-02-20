program i2cscan;

// *******************************
// ***** I2C Scan by Dzandaa *****
// *******************************
// SDA = A4
// SCL = A5

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

uses
  defs, timer, digital, hardwareserial, i2c, StringUtils;


const
	LedPin = 13; //internal LED

var
  I2C1: TI2C;

// ***********************************************
// ***** Blink Led Count Times for debugging *****
// ***********************************************
procedure I2CError(Count: integer);
var
  i: integer;
begin
	while(True) do
	begin
		for i := 1 to Count do
		begin
			DigitalWrite(LedPin, HIGH);
			Delay(200);
			DigitalWrite(LedPin, LOW);
			Delay(200);
		end;
		Delay(1000);
	end;
end;

// ********************************
// ***** Scan I2C for Devices *****
// ********************************
procedure i2c_scan;
var
  i, j, addr: UInt8;
begin


	I2C1.init(I2C_400kHz);

	for i := 0 to 7 do
	begin
		for j := 0 to 15 do
		begin
			addr := (i shl 4) or j;
			if I2C1.start(addr shl 1, false) then
			begin
				Serial.WriteLn('Found = ' + UInt8ToHexString(addr));

			end;
			I2C1.stop;
		end;
		Serial.Write('');
	end;
end;

// ************************
// ***** Main Program *****
// ************************
begin
		PinMode(LedPin, OUTPUT);
		Serial.Start(9600);

	repeat
		Serial.WriteLn('Start Scan');
		i2c_scan;
		delay(3000);
		Serial.WriteLn('');
	until false;
end.

