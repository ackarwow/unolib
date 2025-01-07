unit defs;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

{
  Based on Arduino library source
  ported to Pascal by Andrzej Karwowski 2021

  - modified 3 Dec 2024 by Andrzej Karwowski (inlined routines: BitIsSet, BitIsClear,
    BitClear, BitRead, BitSet, BitWrite, _BV, Cbi, Sbi, PortModeRegister, PortInputRegister,
    PortOutputRegister)

  - modified 4 Dec 2024 by Andrzej Karwowski (added default values of routines:
    PortModeRegister, PortInputRegister, PortOutputRegister; thanks for @Dzandaa for
    pointing it out!)

  - modified 07 Jan 2025 by Andrzej Karwowski (cli and sei replaced by avr_cli and avr_sei from intrinsics)
}

interface

type
  UInt8RA0 = array [0..0] of UInt8;
  UInt8P0 = ^UInt8RA0;

const
  INT32_MAX:Int32 = $7fffffff;
  UINT32_MAX: UInt32 = $ffffffff;


  HIGH = $1;
  LOW = $0;

  INPUT = $0;
  OUTPUT = $1;
  INPUT_PULLUP = $2;

  LSBFIRST = 0;
  MSBFIRST = 1;

  //Arduino.h

  PA = 1; 
  PB = 2;
  PC = 3;
  PD = 4;
  PE = 5;
  PF = 6;
  PG = 7;
  PH = 8;
  PJ = 10;
  PK = 11;
  PL = 12;

  NOT_ON_TIMER = 0;
  TIMER0A = 1;
  TIMER0B = 2;
  TIMER1A = 3;
  TIMER1B = 4;
  TIMER1C = 5;
  TIMER2  = 6;
  TIMER2A = 7;
  TIMER2B = 8;

  TIMER3A = 9;
  TIMER3B = 10;
  TIMER3C = 11;
  TIMER4A = 12;
  TIMER4B = 13;
  TIMER4C = 14;
  TIMER4D = 15;
  TIMER5A = 16;
  TIMER5B = 17;
  TIMER5C = 18;

  //iom328p.h
  COM0B1 = 5;
  COM1B1 = 5;
  COM2B1 = 5;
  COM2A0 = 6;
  COM1A1 = 7;
  COM0A1 = 7;
  COM2A1 = 7;

  CS00 = 0;
  CS01 = 1;
  CS02 = 2;

  CS10 = 0;
  CS11 = 1;
  CS12 = 2;

  CS20 = 0;
  CS21 = 1;
  CS22 = 2;

  WGM00 = 0;
  WGM01 = 1;

  WGM10 = 0;
  WGM11 = 1;

  WGM20 = 0;
  WGM21 = 1;

  ADPS0 = 0;
  ADPS1 = 1;
  ADPS2 = 2;

  NOT_A_PIN = 0;
  NOT_A_PORT = 0;

  TWPS0 = 0;
  TWPS1 = 1;
  TWS3 = 3;
  TWS4 = 4;
  TWS5 = 5;
  TWS6 = 6;
  TWS7 = 7;

  SPM_PAGESIZE = 128;
  RAMSTART = $100;
  RAMEND = $8FF;     // Last On-Chip SRAM Location
  XRAMSIZE = 0;
  XRAMEND = RAMEND;
  E2END = $3FF;
  E2PAGESIZE = 4;
  FLASHEND = $7FFF;

  //#  define SREG_C  (0)
  SREG_C = 0;
  //#  define SREG_Z  (1)
  SREG_Z = 1;
  //#  define SREG_N  (2)
  SREG_N = 2;
  //#  define SREG_V  (3)
  SREG_V = 3;
  //#  define SREG_S  (4)
  SREG_S = 4;
  //#  define SREG_H  (5)
  SREG_H = 5;
  //#  define SREG_T  (6)
  SREG_T = 6;
  //#  define SREG_I  (7)
  SREG_I = 7;

  MPCM0 = 0;

  //pins_arduino.h
  SDA = 2;
  SCL = 3;

function ByteToHex(val: UInt8): shortstring;
//procedure ByteToStr(val: UInt8; var s: string; Digits: UInt8=3);

function BitIsSet(const aSfrp: PUInt8; const aBit: UInt8): boolean; inline;
function BitIsClear(const aSfrp: PUInt8; const aBit: UInt8): boolean; inline;
procedure BitClear(var value: UInt8; const bit: UInt8); inline;
function BitRead(const value, bit: UInt8): UInt8; inline;
procedure BitSet(var value: UInt8; const bit: UInt8); inline;
procedure BitWrite(var value: UInt8; const bit, bitvalue: UInt8); inline;
function _BV(const aBit: UInt8): UInt8; inline;

procedure Cbi(const aSfrp: PUInt8; const aBit: UInt8); inline;
procedure Sbi(const aSfrp: PUInt8; const aBit: UInt8); inline;

function PortModeRegister(const aPort: UInt8): PUInt8; inline;
function PortInputRegister(const aPort: UInt8): PUint8; inline;
function PortOutputRegister(const aPort: UInt8): PUint8; inline;

implementation

const
  HexTbl : array[0..15] of char='0123456789ABCDEF';

{function hexstr(val : longint;cnt : byte) : shortstring;
var
  i : ObjpasInt;
begin
  hexstr[0]:=char(cnt);
  for i:=cnt downto 1 do
   begin
     hexstr[i]:=hextbl[val and $f];
     val:=val shr 4;
   end;
end;}

function ByteToHex(val: UInt8): shortstring;
const
  cnt = 2;
var
  i : UInt8;
begin
  ByteToHex[0]:=char(cnt);
  for i:=cnt downto 1 do
  begin
    ByteToHex[i]:=hextbl[val and $f];
    val:=val shr 4;
  end;
end;

{procedure ByteToStr(val: UInt8; var s: string; Digits: UInt8=3);
var
  tmp, i, p: UInt8;
begin
  tmp:=val;

  p:=1;

  s:='';
  i:=tmp div 100;
  tmp:=tmp mod 100;
  if Digits>=3 then
  begin
    //s[p]:=Chr(i-$30);
    s:=s+Chr(i-$30);
    Inc(p);
  end;

  i:=tmp div 10;
  tmp:=tmp mod 10;
  if Digits>=2 then
  begin
    //s[p]:=Chr(i-$30);
    s:=s+Chr(i-$30);
    Inc(p);
  end;

  if Digits>=1 then
    //s[p]:=Chr(i-$30);
    s:=s+Chr(i-$30);
end;}


function BitIsSet(const aSfrp: PUInt8; const aBit: UInt8): boolean; inline;
begin
  Result:=(aSfrp^ and _BV(aBit))>0;
end;

//Test whether bit \c bit in IO register \c sfr is clear.
//This will return non-zero if the bit is clear, and a 0
//if the bit is set.
function BitIsClear(const aSfrp: PUInt8; const aBit: UInt8): boolean; inline;
begin
  Result:=(aSfrp^ and _BV(aBit))=0;
end;

procedure BitClear(var value: UInt8; const bit: UInt8); inline;
begin
  value:=value and not (1 shl bit);
end;

function BitRead(const value, bit: UInt8): UInt8; inline;
begin
  Result:=(((value) shr (bit)) and $01);
end;

procedure BitSet(var value: UInt8; const bit: UInt8); inline;
begin
  value:=value or UInt8(1 shl bit);
end;

procedure BitWrite(var value: UInt8; const bit, bitvalue: UInt8); inline;
begin
  if bitvalue>0 then
    BitSet(value, bit)
  else
    bitClear(value, bit);
end;

//sfr_defs.h
function _BV(const aBit: UInt8): UInt8; inline;
begin
  Result:=1 shl aBit;
end;

//wiring_private.h
procedure Cbi(const aSfrp: PUInt8; const aBit: UInt8); inline;
begin
  aSfrp^:=aSfrp^ and not _BV(aBit);
end;

procedure Sbi(const aSfrp: PUInt8; const aBit: UInt8); inline;
begin
  aSfrp^:=aSfrp^ or _BV(aBit);
end;

//pins_arduino.h
function PortModeRegister(const aPort: UInt8): PUInt8; inline;
begin
  case aPort of
  0: Result:=Pointer(NOT_A_PORT);
  1: Result:=Pointer(NOT_A_PORT);
  2: Result:=@DDRB;
  3: Result:=@DDRC;
  4: Result:=@DDRD;
  else Result:=nil; //added 4 dec 2024 by A. K.
  end;
end;

function PortInputRegister(const aPort: UInt8): PUint8; inline;
begin
  case aPort of
  0: Result:=Pointer(NOT_A_PORT);
  1: Result:=Pointer(NOT_A_PORT);
  2: Result:=@PINB;
  3: Result:=@PINC;
  4: Result:=@PIND;
  else Result:=nil; //added 4 dec 2024 by A. K.
  end;
end;

//pins_arduino.h
function PortOutputRegister(const aPort: UInt8): PUint8; inline;
begin
  case aPort of
  0: Result:=Pointer(NOT_A_PORT);
  1: Result:=Pointer(NOT_A_PORT);
  2: Result:=@PORTB;
  3: Result:=@PORTC;
  4: Result:=@PORTD;
  else Result:=nil; //added 4 dec 2024 by A. K.
  end;
end;



end.

