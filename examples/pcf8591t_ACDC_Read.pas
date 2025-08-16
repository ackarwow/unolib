program PCF8591TTestRead;

// *************************************
// ***** ACDC Read Test by Dzandaa *****
// *************************************
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

const
	PCF8591: UInt8 = $48 shl 1;

// AC to DC
	ADC0: UInt8 = $00;
	ADC1: UInt8 = $01;
	ADC2: UInt8 = $02;
	ADC3: UInt8 = $03;
// DC to AC
	DAC0: UInt8 = $40;

Procedure ReadADC();
var
	UInt8Data: array[0..1] of UInt8;
	ADC: UInt8;
begin
Serial.WriteLn('Start ACDC ');
	for ADC := 0 to 3 do
	begin
		Serial.Write('Port: ');
		Serial.Write(UInt8ToString(3-ADC));

		Serial.Write(' Values: ');

		if not I2C1.ReadBytesFromReg(PCF8591, ADC, UInt8Data, 2) then
		begin
			Serial.WriteLn('Error Reading Data');
			exit;
		end;

			Serial.WriteLn(UInt8ToString(UInt8Data[0])); // Only 1st value is the data

	end;

end;

begin
	PinMode(LedPin, OUTPUT);
	Serial.Start(9600);
	Serial.WriteLn('Start');

	I2C1.init(I2C_100kHz);

	while(True) do
	begin
		ReadADC();
		delay(3000);
	end;

end.
