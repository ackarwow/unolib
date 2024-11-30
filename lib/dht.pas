unit dht;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

{
  After DHTlib-0.1.34 by Rob Tillaart (https://github.com/RobTillaart/DHTlib).
  Ported to Pascal by Andrzej Karwowski 2021.

  Changes:

  v02
  - new methods: BitsPtr and HexBits in TDHT class
}

interface

uses
  defs, fix16;

const
  // Define types of sensors.
  DHT11 = 11;  // DHT TYPE 11
  DHT12 = 12;

  DHT21 = 21;
  DHT22 = 21;
  DHT33 = 21;
  DHT44 = 21;
  DHT2301 = 21;
  DHT2302 = 21;
  DHT2303 = 21;
  DHT2320 = 21;
  DHT2322 = 21;

  DHT_OK: Int8 = 0;
  DHT_ERROR_CHECKSUM: Int8 = -1;
  DHT_ERROR_TIMEOUT: Int8 = -2;
  DHT_ERROR_CONNECT: Int8 = -3;
  DHT_ERROR_ACK_L: Int8 = -4;
  DHT_ERROR_ACK_H: Int8 = -5;

  DHT_DHT11_WAKEUP = 18;
  DHT_DHT_WAKEUP = 1;

  DHT_DHT11_LEADING_ZEROS = 1;
  DHT_DHT_LEADING_ZEROS = 6;

  DHT_INVALID_VALUE: Int16 = -999;

  // max timeout is 100 usec.
  // For a 16 Mhz proc 100 usec is 1600 clock cycles
  // loops using DHTLIB_TIMEOUT use at least 4 clock cycli
  // so 100 us takes max 400 loops
  // so by dividing F_CPU by 40000 we "fail" as fast as possible
  DHT_TIMEOUT: UInt16 = (F_CPU div 40000);

type
  TDHT=object
  private
    _bits: array[0..4] of UInt8;
    _pin: UInt8;
    _model: UInt8;
    function ReadSensor(wakeupDelay, leadingZeroBits: UInt8): Int8;
    function Read11: Int8;
    function Read12: Int8;
    function ReadOther: Int8;
  public
    DisableIRQ: boolean;
    Humidity: TFix16;
    Temperature: TFix16;
    function Read: Int8;
    function BitsPtr: UInt8P0;
    function HexBits: shortstring;
    procedure Init(pin, model: UInt8);
  end;

var
  _DHT: TDHT;

implementation

uses
  timer, digital;

procedure TDHT.Init(pin, model: UInt8);
begin
  _pin:= pin;
  _model:= model;
  DisableIRQ:=false;
end;

function min(const val1, val2: UInt16): UInt16;
begin
  if (val1<val2) then
    min:=val1
  else
    min:=val2;
end;

function TDHT.ReadSensor(wakeupDelay, leadingZeroBits: UInt8): Int8;
var
  i, mask, idx, bit, port, data, state, pstate: UInt8;
  PIRPtr: PUInt8;
  loopCnt, zeroLoop, delta: UInt16;
begin
  // INIT BUFFERVAR TO RECEIVE DATA
  leadingZeroBits:= 40 - leadingZeroBits; // reverse counting...

  // EMPTY BUFFER
  for i:= 0 to 4 do
    _bits[i]:= 0;

  // replace digitalRead() with Direct Port Reads.
  // reduces footprint ~100 bytes => portability issue?
  // direct port read is about 3x faster
  bit:= digitalPinToBitMask(_pin);
  port:= digitalPinToPort(_pin);
  PIRPtr:= portInputRegister(port);

  // REQUEST SAMPLE
  pinMode(_pin, OUTPUT);
  digitalWrite(_pin, LOW); // T-be
  if (wakeupDelay > 8) then delay(wakeupDelay)
    else delayMicroseconds(wakeupDelay * 1000);
  digitalWrite(_pin, HIGH);   // T-go
  pinMode(_pin, INPUT);

  loopCnt:= DHT_TIMEOUT * 2;  // 200uSec max
  while ((PIRPtr^ and bit) <> LOW) do
  begin
    Dec(loopCnt);
    if (loopCnt = 0) then
    begin
      Result:=DHT_ERROR_CONNECT;
      Exit;
    end;
  end;

  // GET ACKNOWLEDGE or TIMEOUT
  loopCnt:= DHT_TIMEOUT;
  while ((PIRPtr^ and bit) = LOW) do  // T-rel
  begin
    Dec(loopCnt);
    if (loopCnt = 0) then
    begin
      Result:=DHT_ERROR_ACK_L;
      Exit;
    end;
  end;

  loopCnt:= DHT_TIMEOUT;
  while ((PIRPtr^ and bit) <> LOW) do  // T-reh
  begin
    Dec(loopCnt);
    if (loopCnt = 0) then
    begin
      Result:=DHT_ERROR_ACK_H;
      Exit;
    end;
  end;

  loopCnt:= DHT_TIMEOUT;
  mask:= 128;
  idx:= 0;
  data:= 0;
  state:= LOW;
  pstate:= LOW;
  zeroLoop:= DHT_TIMEOUT;
  delta:= 0;

  // READ THE OUTPUT - 40 BITS => 5 BYTES
  i:=40;
  while (i>0) do
  begin
    // WAIT FOR FALLING EDGE
    state:= (PIRPtr^ and bit);
    if (state =LOW) and (pstate <> LOW) then
    begin
      if (i > leadingZeroBits) then // DHT22 first 6 bits are all zero !!   DHT11 only 1
      begin
        zeroLoop:= min(zeroLoop, loopCnt);
        delta:= (DHT_TIMEOUT - zeroLoop) div 4;
      end
      else if (loopCnt <= (zeroLoop - delta)) then // long -> one
        data:=data or mask;

      mask:=mask shr 1;

      if (mask = 0) then  // next byte
      begin
        mask:= 128;
        _bits[idx]:= data;
        Inc(idx);
        data:= 0;
      end;
      // next bit
      Dec(i);
      // reset timeout flag
      loopCnt:= DHT_TIMEOUT;
    end;
    pstate:=state;
    // Check timeout
    Dec(loopCnt);
    if (loopCnt = 0) then
    begin
      Result:=DHT_ERROR_TIMEOUT;
      Exit;
    end;
  end;

  //pinMode(_pin, OUTPUT);
  //digitalWrite(_pin, HIGH);

  Result:=DHT_OK;
end;

function TDHT.Read11: Int8;
var
  rv: Int8;
  sum: UInt8;
begin
    //  VALUES
    if (DisableIRQ) then Cli;
    rv:=ReadSensor(DHT_DHT11_WAKEUP, DHT_DHT11_LEADING_ZEROS);
    if (DisableIRQ) then Sei;

    // these bits are always zero, masking them reduces errors.
    _bits[0]:=_bits[0] and $7F;
    _bits[2]:=_bits[2] and $7F;

    // CONVERT AND STORE
    Humidity:=IntToFix16(_bits[0]);  // bits[1] == 0;
    Temperature:=IntToFix16(_bits[2]);  // bits[3] == 0;

    // TEST CHECKSUM
    sum:= _bits[0] + _bits[1] + _bits[2] + _bits[3];
    if (_bits[4] <> sum) then
      Result:=DHT_ERROR_CHECKSUM
    else
      Result:=rv;
end;

function TDHT.Read12: Int8;
var
  rv: Int8;
  sum: UInt8;
begin
    // READ VALUES
    if (DisableIRQ) then Cli;
    rv:=ReadSensor(DHT_DHT11_WAKEUP, DHT_DHT11_LEADING_ZEROS);
    if (DisableIRQ) then Sei;

    // CONVERT AND STORE
    Humidity:=Fix16Mul(IntToFix16(_bits[0] +_bits[1]), StrToFix16('0.1'));
    Temperature:= Fix16Mul(IntToFix16(_bits[2] + (_bits[3] and $7F)), StrToFix16('0.1'));
    if (_bits[3] and $80)>0 then  // negative temperature
      Temperature:= -Temperature;

    // TEST CHECKSUM
    sum:= _bits[0] + _bits[1] + _bits[2] + _bits[3];
    if (_bits[4] <> sum) then
      Result:= DHT_ERROR_CHECKSUM
    else
      Result:=rv;
end;

function TDHT.ReadOther: Int8;
var
  rv: Int8;
  sum: UInt8;
  t: Int16;
begin
    // READ VALUES
    if (DisableIRQ) then Cli;
    rv:= ReadSensor(DHT_DHT_WAKEUP, DHT_DHT_LEADING_ZEROS);
    if (DisableIRQ) then Sei;

    // these bits are always zero, masking them reduces errors.
    _bits[0]:=_bits[0] and $03;
    _bits[2]:=_bits[2] and $83;

    // CONVERT AND STORE
    humidity:= Fix16Div(IntToFix16(_bits[0] * 256 + _bits[1]), IntToFix16(10));
    t:= ((_bits[2] and $7F) * 256 + _bits[3]);
    if (t = 0) then
      temperature:=StrToFix16('0.0')     // prevent -0.0;
    else
    begin
      temperature:= Fix16Div(IntToFix16(t), IntToFix16(10));
      if ((_bits[2] and $80) = $80) then
        temperature:= -temperature;
    end;

   { // HEXDUMP DEBUG
    Serial.println();
    // CHECKSUM
    if (_bits[4] < 0x10) Serial.print(0);
    Serial.print(_bits[4], HEX);
    Serial.print("    ");
    // TEMPERATURE
    if (_bits[2] < 0x10) Serial.print(0);
    Serial.print(_bits[2], HEX);
    if (_bits[3] < 0x10) Serial.print(0);
    Serial.print(_bits[3], HEX);
    Serial.print("    ");
    Serial.print(temperature, 1);
    Serial.print("    ");
    // HUMIDITY
    if (_bits[0] < 0x10) Serial.print(0);
    Serial.print(_bits[0], HEX);
    if (_bits[1] < 0x10) Serial.print(0);
    Serial.print(_bits[1], HEX);
    Serial.print("    ");
    Serial.print(humidity, 1);
    }

    // TEST CHECKSUM
    sum:= _bits[0] + _bits[1] + _bits[2] + _bits[3];
    if (_bits[4] <> sum) then
      Result:= DHT_ERROR_CHECKSUM
    else
      Result:=rv;
end;


function TDHT.Read: Int8;
begin
  case _model of
  DHT11: Result:=Read11;
  DHT12: Result:=Read12;
  DHT21: Result:=ReadOther;
  else Result:=DHT_INVALID_VALUE;
  end;
end;

function TDHT.BitsPtr: UInt8P0;
begin
  Result:=@_bits;
end;

function TDHT.HexBits: shortstring;
var
  i: UInt8;
begin
  Result:='';

  for i:=0 to 4 do
    Result:=Result+ByteToHex(_bits[i]);
end;

end.

