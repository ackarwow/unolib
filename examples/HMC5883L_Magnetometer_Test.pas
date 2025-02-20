program HMC5883LTest;

// ****************************************
// ***** Magnetometre Test by Dzandaa *****
// ****************************************
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

const
	HMC5883L: UInt8 = $1E shl 1;

// ***********************
//***** Main Program *****
// ***********************
var
	i: UInt8;
	UInt8Data: array[0..5] of UInt8;
	Int16Data: array[0..2] of Int16;
	Axe: Array[0..2] of String = ('X =', 'Y =', 'Z =');
begin
	PinMode(LedPin, OUTPUT);
	Serial.Start(9600);
	Serial.WriteLn('Start');

	for i := 0 to 5 do UInt8Data[i] := 0;

	I2C1.init(I2C_100kHz);

	if not I2C1.WriteByteToReg(HMC5883L, $00, $70) then  // 8-average, 15 Hz default, normal measurement
	begin
		Serial.WriteLn('Error Measurement Frenquency');
		exit;
	end;

	if not I2C1.WriteByteToReg(HMC5883L, $05, $A0) then // Gain=5, or any other desired gain
	begin
		Serial.WriteLn('Error Gain');
		exit;
	end;

	delay(6);

	if not I2C1.WriteByteToReg(HMC5883L, $02, $00) then // Continuous-measurement mode
	begin
		Serial.WriteLn('Error Measurement Mode');
		exit;
	end;

	delay(6);

	while(True) do
	begin

// Read the 3 axes
		if not I2C1.ReadBytesFromReg(HMC5883L, $03, UInt8Data, 6) then
		begin
			Serial.WriteLn('Error Reading Data');
			exit;
		end;

Int16Data[0] := (Int16(UInt8Data[0]) shl 8) or int16(UInt8Data[1]);
Int16Data[1] := (Int16(UInt8Data[2]) shl 8) or int16(UInt8Data[3]);
Int16Data[2] := (Int16(UInt8Data[4]) shl 8) or int16(UInt8Data[5]);

// Integer
		for i := 0 to 2 do
		begin
			Serial.WriteLn(Axe[i] + ' ' + Int16ToString(Int16Data[i]));
		end;

		Serial.Writeln('');
		delay(1000);
	end;

end.
