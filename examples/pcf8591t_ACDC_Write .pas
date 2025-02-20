program PCF8591TTestWrite;

// **************************************
// ***** ACDC Write Test by Dzandaa *****
// **************************************
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

const
 SinWave: Array[0..255] of Uint8 =
(
$14, $15, $15, $16, $16, $17, $17, $18,
$19, $19, $1A, $1A, $1B, $1C, $1C, $1D,
$1E, $1E, $1F, $20, $20, $21, $22, $22,
$23, $24, $24, $25, $26, $26, $27, $28,
$29, $29, $2A, $2B, $2B, $2C, $2D, $2E,
$2F, $2F, $30, $31, $32, $32, $33, $34,
$35, $36, $36, $37, $38, $39, $3A, $3B,
$3B, $3C, $3D, $3E, $3F, $40, $41, $41,
$42, $43, $44, $45, $46, $47, $48, $49,
$49, $4A, $4B, $4C, $4D, $4E, $4F, $50,
$51, $52, $53, $54, $55, $55, $56, $57,
$58, $59, $5A, $5B, $5C, $5D, $5E, $5F,
$60, $61, $62, $63, $64, $65, $66, $67,
$68, $69, $6A, $6B, $6C, $6D, $6E, $6F,
$70, $71, $72, $73, $74, $75, $76, $77,
$78, $79, $7A, $7B, $7C, $7D, $7E, $7F,
$7F, $80, $81, $82, $83, $84, $85, $86,
$87, $88, $89, $8A, $8B, $8C, $8D, $8E,
$8F, $90, $91, $92, $93, $94, $95, $96,
$97, $98, $99, $9A, $9B, $9C, $9D, $9E,
$9F, $A0, $A1, $A2, $A3, $A4, $A5, $A6,
$A7, $A8, $A9, $A9, $AA, $AB, $AC, $AD,
$AE, $AF, $B0, $B1, $B2, $B3, $B4, $B5,
$B5, $B6, $B7, $B8, $B9, $BA, $BB, $BC,
$BD, $BD, $BE, $BF, $C0, $C1, $C2, $C3,
$C3, $C4, $C5, $C6, $C7, $C8, $C8, $C9,
$CA, $CB, $CC, $CC, $CD, $CE, $CF, $CF,
$D0, $D1, $D2, $D3, $D3, $D4, $D5, $D5,
$D6, $D7, $D8, $D8, $D9, $DA, $DA, $DB,
$DC, $DC, $DD, $DE, $DE, $DF, $E0, $E0,
$E1, $E2, $E2, $E3, $E4, $E4, $E5, $E5,
$E6, $E7, $E7, $E8, $E8, $E9, $E9, $EA
);


FREQ = 100;

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

// ********************************
// ***** Write a sinus to DAC *****
// ********************************
Function WriteDCA(): boolean;
var
	Cnt: UInt8;
begin
Serial.WriteLn('Start DCA ');
	while(true) do
begin
		for Cnt := 0 to 255 do
		begin
			if not I2C1.WriteByteToReg(PCF8591, DAC0, SinWave[Cnt]) then exit(False);
				DelayMicroseconds(FREQ);
		end;
		for Cnt := 255 Downto 0 do
		begin
			if not I2C1.WriteByteToReg(PCF8591, DAC0, SinWave[Cnt]) then exit(False);
				DelayMicroseconds(FREQ);
		end;
	end;
	exit(True);
end;

// *****************
// ***** Start *****
// *****************
begin
	PinMode(LedPin, OUTPUT);
	Serial.Start(9600);
	Serial.WriteLn('Start');

	I2C1.init(I2C_100kHz);

	if not WriteDCA() then
	Serial.WriteLn('Error in WriteDCA');

	delay(3000);

end.
