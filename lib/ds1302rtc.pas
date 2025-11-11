unit ds1302rtc;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

//based on DS1302 library by Henning Karlsen available on http://www.rinkydinkelectronics.com

interface

const
  FORMAT_SHORT = 1;
  FORMAT_LONG = 2;

  FORMAT_LITTLEENDIAN = 1;
  FORMAT_BIGENDIAN = 2;
  FORMAT_MIDDLEENDIAN = 3;

  MONDAY = 1;
  TUESDAY = 2;
  WEDNESDAY = 3;
  THURSDAY = 4;
  FRIDAY = 5;
  SATURDAY = 6;
  SUNDAY = 7;

  TCR_D1R2K = 165;
  TCR_D1R4K = 166;
  TCR_D1R8K = 167;
  TCR_D2R2K = 169;
  TCR_D2R4K = 170;
  TCR_D2R8K = 171;
  TCR_OFF = 92;

  REG_SEC = 0;
  REG_MIN = 1;
  REG_HOUR = 2;
  REG_DATE = 3;
  REG_MON = 4;
  REG_DOW = 5;
  REG_YEAR = 6;
  REG_WP = 7;
  REG_TCR = 8;

type
  TDS1302RAM = array[0..30] of UInt8;

  TDS1302rtc = object
  private
    _CE_pin: UInt8;
    _DATA_pin: UInt8;
    _SCLK_pin: UInt8;
    _burstArray: array [0..7] of UInt8;
    procedure _burstRead;
    function _readByte: UInt8;
    function _readRegister(reg: UInt8): UInt8;
    procedure _writeByte(value: UInt8);
    procedure _writeRegister(reg, value: UInt8);
  public
    seconds: UInt8;
    minutes: UInt8;
    hours: UInt8;
    dayofweek: UInt8;
    dayofmonth: UInt8;
    month: UInt8;
    year: integer;
    procedure Init(SCLK_pin, DATA_pin, CE_pin: UInt8);
    procedure GetTime;
    procedure SetTime(aseconds, aminutes, ahours, adayofweek, adayofmonth, amonth: UInt8; ayear: UInt16);
    procedure Halt(enable: UInt8{boolean});
    procedure WriteProtect(enable: UInt8{bool});
    procedure setTCR(value: UInt8);
    procedure WriteBuffer(r: TDS1302RAM);
    function ReadBuffer: TDS1302RAM;
    procedure Poke(addr, value: Uint8);
    function Peek(addr: Uint8): UInt8;
  end;

var
  _RTC: TDS1302rtc;

implementation

uses
  defs, digital, timer;

function _decode(value: UInt8): UInt8;
var
  decoded: UInt8;
begin
  decoded:= value and 127;
  decoded:= (decoded and 15) + 10 * ((decoded and (15 shl 4)) shr 4);
  Result:=decoded;
end;

function _decodeH(value: UInt8): UInt8;
var
  decoded: UInt8;
begin
  decoded:=value;
  if (decoded and 128)>0 then
    decoded:= (decoded and 15) + (12 * ((decoded and 32) shr 5))
  else
    decoded:= (decoded and 15) + (10 * ((decoded and 48) shr 4));
  Result:=decoded;
end;

function _decodeY(value: UInt8): UInt8;
var
  decoded: UInt8;
begin
  decoded:= (value and 15) + 10 * ((value and (15 shl 4)) shr 4);
  Result:=decoded;
end;

function _encode(value: UInt8): UInt8;
var
  encoded: UInt8;
begin
  encoded:= ((value div 10) shl 4) + (value mod 10);
  Result:=encoded;
end;

procedure TDS1302rtc.Init(SCLK_pin, DATA_pin, CE_pin: UInt8);
begin
  _SCLK_pin:= SCLK_pin;
  _DATA_pin:= DATA_pin;
  _CE_pin:= CE_pin;

  PinMode(_CE_pin, OUTPUT);
  PinMode(_SCLK_pin, OUTPUT);
end;

procedure TDS1302rtc.GetTime;
begin
  _BurstRead;
  seconds:=_decode(_burstArray[0]);
  minutes:=_decode(_burstArray[1]);
  hours:=_decodeH(_burstArray[2]);
  dayofmonth:=_decode(_burstArray[3]);
  month:=_decode(_burstArray[4]);
  dayofweek:=_burstArray[5];
  year:=_decodeY(_burstArray[6])+2000;
end;

procedure TDS1302rtc.SetTime(aseconds, aminutes, ahours, adayofweek, adayofmonth, amonth: UInt8; ayear: UInt16);
begin
  seconds:= aseconds;
  minutes:= aminutes;
  hours:= ahours;
  dayofweek:= adayofweek;
  dayofmonth:= adayofmonth;
  month:= amonth;
  year:= ayear{-2000};

  _writeRegister(REG_HOUR, _encode(hours));
  _writeRegister(REG_MIN, _encode(minutes));
  _writeRegister(REG_SEC, _encode(seconds));
  _writeRegister(REG_YEAR, _encode(year-2000));
  _writeRegister(REG_MON, _encode(month));
  _writeRegister(REG_DATE, _encode(dayofmonth));
  _writeRegister(REG_DOW, dayofweek);
end;

function TDS1302rtc._readByte: UInt8;
var
  i, value, currentBit: UInt8;
begin
  PinMode(_DATA_pin, INPUT);

  value:= 0;
  currentBit:= 0;

  for i:= 0 to 7 do
  begin
    currentBit:= digitalRead(_DATA_pin);
    value:=Value or (currentBit shl i);
    DigitalWrite(_SCLK_pin, HIGH);
    DelayMicroseconds(1);
    digitalWrite(_SCLK_pin, LOW);
  end;

  Result:=value;
end;

procedure TDS1302rtc._writeByte(value: UInt8);
begin
  PinMode(_DATA_pin, OUTPUT);
  ShiftOut(_DATA_pin, _sclk_pin, LSBFIRST, value);
end;

procedure TDS1302rtc._writeRegister(reg, value: UInt8);
var
  cmdByte: UInt8;
begin
  cmdByte:= (128 or (reg shl 1));

  DigitalWrite(_SCLK_pin, LOW);
  DigitalWrite(_CE_pin, HIGH);

  _writeByte(cmdByte);
  _writeByte(value);

  digitalWrite(_CE_pin, LOW);
end;

function TDS1302rtc._readRegister(reg: UInt8): UInt8;
var
  cmdByte, readValue: UInt8;
begin
  cmdByte:= 129;
  cmdByte:=cmdByte or (reg shl 1);

  digitalWrite(_SCLK_pin, LOW);
  digitalWrite(_CE_pin, HIGH);

  _writeByte(cmdByte);
  readValue:= _readByte();

  digitalWrite(_CE_pin, LOW);

  Result:=ReadValue;
end;

procedure TDS1302rtc._burstRead;
var
  i: UInt8;
begin
  DigitalWrite(_SCLK_pin, LOW);
  DigitalWrite(_CE_pin, HIGH);

  _WriteByte(191);
  for i:=0 to 7 do
    _burstArray[i]:=_readByte;

  DigitalWrite(_CE_pin, LOW);
end;

procedure TDS1302rtc.Halt(enable: UInt8{boolean});
var
  _reg: UInt8;
begin
  _reg:= _readRegister(REG_SEC);
  _reg:=_reg and not (1 shl 7);
  _reg:=_reg or (enable shl 7);
  _writeRegister(REG_SEC, _reg);
end;

procedure TDS1302rtc.WriteProtect(enable: UInt8{bool});
var
  _reg: UInt8;
begin
  _reg:= (enable shl 7);
  _writeRegister(REG_WP, _reg);
end;

procedure TDS1302rtc.setTCR(value: UInt8);
begin
  _writeRegister(REG_TCR, value);
end;

procedure TDS1302rtc.WriteBuffer(r: TDS1302RAM);
var
  i: UInt8;
begin
  digitalWrite(_SCLK_pin, LOW);
  digitalWrite(_CE_pin, HIGH);

  _writeByte(254);
  for i:=0 to 30 do
    _writeByte(r[i]);

  DigitalWrite(_CE_pin, LOW);
end;

function TDS1302rtc.ReadBuffer: TDS1302RAM;
var
  i: UInt8;
begin
  digitalWrite(_SCLK_pin, LOW);
  digitalWrite(_CE_pin, HIGH);

  _writeByte(255);
  for i:=0 to 30 do
    Result[i]:=_readByte;

  digitalWrite(_CE_pin, LOW);
end;

procedure TDS1302rtc.Poke(addr, value: Uint8);
begin
  if ((addr>=0) and (addr<=30)) then
  begin
    addr:= (addr * 2) + 192;

    digitalWrite(_SCLK_pin, LOW);
    digitalWrite(_CE_pin, HIGH);

    _writeByte(addr);
    _writeByte(value);

    digitalWrite(_CE_pin, LOW);
  end;
end;

function TDS1302rtc.Peek(addr: Uint8): UInt8;
var
  readValue: UInt8;
begin
  if ((addr >=0) and (addr<=30)) then
  begin
    addr:= (addr * 2) + 193;

    digitalWrite(_SCLK_pin, LOW);
    digitalWrite(_CE_pin, HIGH);

    _writeByte(addr);
    readValue:= _readByte();

    digitalWrite(_CE_pin, LOW);

    Result:=readValue;
  end
  else
    Result:= 0;
end;

end.

