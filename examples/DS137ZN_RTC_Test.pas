program DS137ZN_RTC;

// *******************************************
// ***** Real Time Clock Test by Dzandaa *****
// *******************************************
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
	SECONDS: UInt8 = $00;
	MINUTES: UInt8 = $01;
	HOURS: UInt8 = $02;
	DAY: UInt8 = $03;
	DATE: UInt8 = $04;
	MONTH: UInt8 = $05;
	YEAR: UInt8 = $06;

const
	DS137ZNT: UInt8 = $68 shl 1;

var
	UInt8Date: array[0..6] of UInt8;

// *******************************
// ***** Encode UInt8 to BCD *****
// *******************************
function EncodeBCD(val: UInt8): UInt8;
Var
	Ret: UInt8;
begin
	Ret := (((Val div 10) shl 4) and $F0) + (Val Mod 10);
	exit(Ret);
end;

// ******************************
// ***** Encode Date to BCD *****
// ******************************
function EncodeDateToBCD(Yr, Mo, Dy, Ho, Mi, Se, Da: Uint8): Boolean;
begin
	UInt8Date[YEAR] := EncodeBCD(Yr);
	UInt8Date[MONTH] := EncodeBCD(Mo);
	UInt8Date[DAY] := Dy and $07;
	UInt8Date[HOURS] := EncodeBCD(Ho);
	UInt8Date[MINUTES] := EncodeBCD(Mi);
	UInt8Date[SECONDS] := EncodeBCD(Se);
	UInt8Date[DATE] := EncodeBCD(Da);
	exit(True);
end;


// #y25m02D03H11M34S00d12!

// ********************************
// ***** Get Date From Serial *****
// ********************************
function GetDateFromSerial(): boolean;
var
//	Str, Str1: String;
	i: UInt16;
	Yr, Mo, Dy, Ho, Mi, Se, Da: Uint8;
	MStart: Word;
	Data: array [0..23] of Uint8;
	Val: UInt8;
begin
	i := 0;
//	Str := '';
	MStart := Millis() + 1000;
	while(i < 23) do
	begin

	if(Millis() > MStart) then
	begin
		Serial.WriteLn('Input Timeout');
		exit(False);
	end;

		if (Serial.Available > 0) then
		begin
			Val := Serial.ReadByte;
			Data[i] := Val;
			Inc(i);
		end;
		delay(10);
		end;

	i := 0;
	if(Data[0] = UInt8('y')) then
	begin
		Yr := ((Data[1] - $30) * 10) + (Data[2] - $30);
	end
	else
	begin
		Serial.WriteLn('Year Error');
		exit(false);
	end;

	if(Data[3] = UInt8('m')) then
	begin
		Mo := ((Data[4] - $30) * 10) + (Data[5] - $30);
	end
	else
	begin
		Serial.WriteLn('Month Error');
		exit(false);
	end;

	if(Data[6] = UInt8('D')) then
	begin
		Dy := ((Data[7] - $30) * 10) + (Data[8] - $30);
	end
	else
	begin
		Serial.WriteLn('Day of Week Error');
		exit(false);
	end;

	if(Data[9] = UInt8('H')) then
	begin
		Ho := ((Data[10] - $30) * 10) + (Data[11] - $30);
	end
	else
	begin
		Serial.WriteLn('Hour Error');
		exit(false);
	end;

	if(Data[12] = UInt8('M')) then
	begin
		Mi := ((Data[13] - $30) * 10) + (Data[14] - $30);
	end
	else
	begin
		Serial.WriteLn('Minute Error');
		exit(false);
	end;

	if(Data[15] = UInt8('S')) then
	begin
		Se := ((Data[16] - $30) * 10) + (Data[17] - $30);
	end
	else
	begin
		Serial.WriteLn('Second Error');
		exit(false);
	end;

	if(Data[18] = UInt8('d')) then
	begin
		Da := ((Data[19] - $30) * 10) + (Data[20] - $30);
		Inc(i, 3);
	end
	else
	begin
		Serial.WriteLn('Day Error');
		exit(false);
	end;

	if not(Data[21] = UInt8('!')) then
	begin
		Serial.WriteLn('Error End');
		exit(false);
	end;

// ******************************
// ***** Encode Date to BCD *****
// ******************************
	EncodeDateToBCD(Yr, Mo, Dy, Ho, Mi, Se, Da);

	if not I2C1.WriteBytesToReg(DS137ZNT, SECONDS, UInt8Date, 7) then
		begin
			Serial.WriteLn('Error Writing Date Data');
			exit(False);
		end;

	Serial.WriteLn('OK Encoding');
	exit(True);
end;

// *******************************
// ***** Send Date to Serial *****
// *******************************

procedure SendDateToSerial();
begin
	Serial.Write('D: ' + UInt8ToString(UInt8Date[DATE] shr 4) + UInt8ToString(UInt8Date[DATE] and $0f) + ' ');
	Serial.Write('M: ' + UInt8ToString(UInt8Date[MONTH] shr 4) + UInt8ToString(UInt8Date[MONTH] and $0f) + ' ');
	Serial.Write('Y: 20' + UInt8ToString(UInt8Date[YEAR] shr 4) + UInt8ToString(UInt8Date[YEAR] and $0f) + ' ');

	Serial.Write('H: ' + UInt8ToString(UInt8Date[HOURS] shr 4) + UInt8ToString(UInt8Date[HOURS] and $0f) + ' ');
	Serial.Write('M: ' + UInt8ToString(UInt8Date[MINUTES] shr 4) + UInt8ToString(UInt8Date[MINUTES] and $0f) + ' ');
	Serial.Write('S: ' + UInt8ToString(UInt8Date[SECONDS] shr 4) + UInt8ToString(UInt8Date[SECONDS] and $0f) + ' ');
	Serial.Write('Dy: ' + UInt8ToString((UInt8Date[DAY] and $07)) + ' ');
	Serial.WriteLn('');
end;

// ************************
// ***** Main Program *****
// ************************
var
	Sb: Char;
begin
	PinMode(LedPin, OUTPUT);
	Serial.Start(9600);
	Serial.WriteLn('Start');

	I2C1.init(I2C_100kHz);

	EncodeDateToBCD(25, 02, 02, 17, 12, 0, 11);

	if not I2C1.WriteBytesToReg(DS137ZNT, SECONDS, UInt8Date, 7) then
		begin
			Serial.WriteLn('Error Writing Date Data');
			exit;
		end;

	while(True) do
	begin
		if (Serial.Available > 0) then
		begin
			Sb := Serial.ReadChar;
			if (Sb = '#') then
			begin
				if(not GetDateFromSerial()) then
				begin
					Serial.WriteLn('Error converting Date');
				end;
			end
			else
			begin
				while(not (Serial.Available = 0)) do
					Sb := Serial.ReadChar;
			end;
		end;

		if not I2C1.ReadBytesFromReg(DS137ZNT, SECONDS, UInt8Date, 7) then
		begin
			Serial.WriteLn('Error Reading Date Data');
			exit;
		end;

		SendDateToSerial();

		delay(1000);
	end;
end.
