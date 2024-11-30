unit fix16;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

interface

{
  Based on libfixmath by Petteri Aimonen
  https://github.com/PetteriAimonen/libfixmath

  ported to Pascal by Andrzej Karwowski 2021
}


{ SizeOf(type):
 longint=4 bytes (signed)
 integer=smallint: 2 bytes (signed)
 word: 2 bytes
 string, shortstring: 256 bytes
}

{Fix16.h}

type
  TFix16 = longint; //int32_t;

const
  FOUR_DIV_PI: TFix16 = $145F3;                //Fix16 value of 4/PI
  _FOUR_DIV_PI2: TFix16 = $FFFF9840;           //Fix16 value of -4/PI²
  X4_CORRECTION_COMPONENT: TFix16 = $399A;     //Fix16 value of 0.225
  PI_DIV_4: TFix16 = $0000C90F;                //Fix16 value of PI/4
  THREE_PI_DIV_4: TFix16 = $00025B2F;          //Fix16 value of 3PI/4

  fix16_maximum: TFix16 = $FFFFFFF;            //the maximum value of fix16_t
  fix16_minimum: TFix16 = $80000000;           //the minimum value of fix16_t
  fix16_overflow: TFix16 = $80000000;          //the value used to indicate overflows when FIXMATH_NO_OVERFLOW is not specified

  fix16_pi: TFix16 = 205887;                   //fix16_t value of pi
  fix16_e: TFix16 = 178145;                    //fix16_t value of e
  fix16_one: TFix16 = $00010000;               //fix16_t value of 1
  fix16_eps: TFix16 = 1;                       //fix16_t epsilon

  {versions for FIXMATH_OPTIMIZE_8BIT = optimized for AVR}

  function IntToFix16(const a: longint): TFix16;
  function Fix16Add(const inArg0, inArg1: TFix16): TFix16;
  function Fix16Sub(const inArg0, inArg1: TFix16): TFix16;
  function Fix16Mul(const inArg0, inArg1: TFix16): TFix16;
  function Fix16Div(const inArg0, inArg1: TFix16): TFix16;
  function Fix16Mod(x, y: TFix16): TFix16;
  procedure Fix16ToStr(const Value: TFix16; const decimals: byte; var OutStr: shortstring);
  function StrToFix16(const buf: shortstring): TFix16;

implementation

function IntToFix16(const a: longint): TFix16;
begin
  Result:=a * fix16_one;
end;

function Fix16ToInt(const a: TFix16):longint;
begin
  {$IFDEF FIXMATH_NO_ROUNDING}
    Result:=(a shr 16);
  {$ELSE}
    if (a >= 0) then
      Result:=(a + (fix16_one shr 1)) div fix16_one
    else
      Result:=(a - (fix16_one shr 1)) div fix16_one;
  {$ENDIF}
end;

function Fix16Abs(const x: TFix16): TFix16;
begin
  if x<0 then Result:=TFix16(-x)
  else Result:=x;
end;

function Fix16Floor(const x: TFix16): TFix16;
begin
  Result:=x and $FFFF0000;
end;

function Fix16Ceil(const x: TFix16): TFix16;
begin
  Result:=x and $FFFF0000;
  if (x and $0000FFFF)>0 then
    Result:=Result+fix16_one;
end;

function Fix16Min(const x, y: TFix16): TFix16;
begin
  if x<y then
    Result:=x
  else
    Result:=y;
end;

function Fix16Max(const x, y: TFix16): TFix16;
begin
  if x>y then
    Result:=x
  else
    Result:=y;
end;

function Fix16Add(const inArg0, inArg1: TFix16): TFix16;
{$IFNDEF FIXMATH_NO_OVERFLOW}
var
  _a, _b, sum: UInt32;
{$ENDIF}
begin
{$IFDEF FIXMATH_NO_OVERFLOW}
  Result:=(inArg0 + inArg1);
{$ELSE}
  // Use unsigned integers because overflow with signed integers is
  // an undefined operation (http://www.airs.com/blog/archives/120).
  _a:= inArg0;
  _b:= inArg1;
  sum:= _a + _b;

  // Overflow can only happen if sign of a == sign of b, and then
  // it causes sign of sum != sign of a.
  if (not(((_a xor _b) and $80000000)>0)) and (((_a xor sum) and $80000000)>0) then
    Result:=fix16_overflow
  else
    Result:=sum;
{$ENDIF}
end;

function Fix16Sub(const inArg0, inArg1: TFix16): TFix16;
{$IFNDEF FIXMATH_NO_OVERFLOW}
var
  _a, _b, diff: UInt32;
{$ENDIF}
begin
{$IFDEF FIXMATH_NO_OVERFLOW}
  Result:=(inArg0 - inArg1);
{$ELSE}
  _a:= inArg0;
  _b:= inArg1;
  diff:= _a - _b;

  // Overflow can only happen if sign of a != sign of b, and then
  // it causes sign of diff != sign of a.
  if (not(((_a xor _b) and $80000000)>0)) and (((_a xor diff) and $80000000)>0) then
    Result:=fix16_overflow
  else
    Result:=diff;
{$ENDIF}
end;

//Multiplies the two given fix16_t's and returns the result.
function Fix16Mul(const inArg0, inArg1: TFix16): TFix16;
var
  _a, _b, low, mid: UInt32;
  va, vb: array[0..3] of UInt8;
begin
  _a:= Fix16Abs(inArg0);
  _b:= Fix16Abs(inArg1);

  va[0]:=_a; va[1]:=_a shr 8; va[2]:=_a shr 16; va[3]:=_a shr 24;
  vb[0]:=_b; vb[1]:=_b shr 8; vb[2]:=_b shr 16; vb[3]:=_b shr 24;

  //va=0,0,13,0
  //vb=0,0,20,0

  low:= 0;
  mid:= 0;

  // Result column i depends on va[0..i] and vb[i..0]
  {$IFNDEF FIXMATH_NO_OVERFLOW}
  // i = 6
  if (va[3]>0) and (vb[3]>0) then
  begin
    Result:=fix16_overflow;
    Exit;
  end;
  {$ENDIF}

  // i = 5
  if (va[2]>0) and (vb[3]>0) then
    {%H-}Inc(mid, UInt16(va[2])*UInt16(vb[3]));
  if (va[3]>0) and (vb[2]>0) then
    {%H-}Inc(mid, UInt16(va[3])*UInt16(vb[2]));
  mid:=mid shl 8;

  // i = 4
  if (va[1]>0) and (vb[3]>0) then
    {%H-}Inc(mid, UInt16(va[1])*UInt16(vb[3]));
  if (va[2]>0) and (vb[2]>0) then
    {%H-}Inc(mid, UInt16(va[2])*UInt16(vb[2]));
  if (va[3]>0) and (vb[1]>0) then
    {%H-}Inc(mid, UInt16(va[3])*UInt16(vb[1]));

  {$IFNDEF FIXMATH_NO_OVERFLOW}
  if (mid and $FF000000)>0 then
  begin
    Result:=fix16_overflow;
    Exit;
  end;
  {$ENDIF}
  mid:=mid shl 8;

  // i = 3
  if (va[0]>0) and (vb[3]>0) then
    {%H-}Inc(mid, UInt16(va[0])*UInt16(vb[3]));
  if (va[1]>0) and (vb[2]>0) then
    {%H-}Inc(mid, UInt16(va[1])*UInt16(vb[2]));
  if (va[2]>0) and (vb[1]>0) then
    {%H-}Inc(mid, UInt16(va[2])*UInt16(vb[1]));
  if (va[3]>0) and (vb[0]>0) then
    {%H-}Inc(mid, UInt16(va[3])*UInt16(vb[0]));

  {$IFNDEF FIXMATH_NO_OVERFLOW}
  if (mid and $FF000000)>0 then
  begin
    Result:=fix16_overflow;
    Exit;
  end;
  {$ENDIF}
  mid:=mid shl 8;

  // i = 2
  if (va[0]>0) and (vb[2]>0) then
    {%H-}Inc(mid, UInt16(va[0])*UInt16(vb[2]));
  if (va[1]>0) and (vb[1]>0) then
    {%H-}Inc(mid, UInt16(va[1])*UInt16(vb[1]));
  if (va[2]>0) and (vb[0]>0) then
    {%H-}Inc(mid, UInt16(va[2])*UInt16(vb[0]));

  // i = 1
  if (va[0]>0) and (vb[1]>0) then
    {%H-}Inc(low, UInt16(va[0]) * UInt16(vb[1]));
  if (va[1]>0) and (vb[0]>0) then
    {%H-}Inc(low, UInt16(va[1]) * UInt16(vb[0]));
  low:=low shl 8;

  // i = 0
  if (va[0]>0) and (vb[0]>0) then
    {%H-}Inc(low, UInt16(va[0]) * UInt16(vb[0]));

  {$IFNDEF FIXMATH_NO_ROUNDING}
  Inc(low, $8000);
  {$ENDIF}
  Inc(mid, (low shr 16));

  {$IFNDEF FIXMATH_NO_OVERFLOW}
  if (mid and $80000000)>0 then
  begin
    Result:=fix16_overflow;
    Exit;
  end;
  {$ENDIF}

  Result:= mid;

  // Figure out the sign of result
  if ((inArg0 >= 0) <> (inArg1 >= 0)) then
    Result:=-Result;
end;

//Divides the first given fix16_t by the second and returns the result.
function Fix16Div(const inArg0, inArg1: TFix16): TFix16;
var
  remainder, divider, quotient, bit: UInt32;
begin
  // This uses the basic binary restoring division algorithm.
  // It appears to be faster to do the whole division manually than
  // trying to compose a 64-bit divide out of 32-bit divisions on
  // platforms without hardware divide.

  if (inArg1 = 0) then
  begin
    Result:=fix16_minimum;
    Exit;
  end;

  remainder:= Fix16Abs(inArg0);
  divider:= Fix16Abs(inArg1);

  quotient:= 0;
  bit:= $10000;

  // The algorithm requires D >= R
  while (divider < remainder) do
  begin
    divider:=(divider shl 1);
    bit:=(bit shl 1);
  end;

  {$IFNDEF FIXMATH_NO_OVERFLOW}
  if not (bit>0) then
  begin
    Result:=fix16_overflow;
    Exit;
  end;
  {$ENDIF}

  if (divider and $80000000)>0 then
  begin
    // Perform one step manually to avoid overflows later.
    // We know that divider's bottom bit is 0 here.
    if (remainder >= divider) then
    begin
      quotient:=quotient or bit;
      Dec(remainder, divider);
    end;
    divider:=divider shr 1;
    bit:=bit shr 1;
  end;

  //Main division loop
  while (bit>0) and (remainder>0) do
  begin
    if (remainder >= divider) then
    begin
      quotient:=quotient or bit;
      Dec(remainder, divider);
    end;

    remainder:=remainder shl 1;
    bit:=bit shr 1;
  end;

  {$IFNDEF FIXMATH_NO_ROUNDING}
  if (remainder >= divider) then
    Inc(quotient);
  {$ENDIF}

  Result:= quotient;

  //Figure out the sign of result
  if ((inArg0 xor inArg1) and $80000000)>0 then
  begin
    {$IFNDEF FIXMATH_NO_OVERFLOW}
    if (result = fix16_minimum) then
    begin
      Result:=fix16_overflow;
      Exit;
    end;
    {$ENDIF}

    result:= -result;
  end;
end;

function Fix16Mod(x, y: TFix16): TFix16;
begin
  // The reason we do this, rather than use a modulo operator
  // is that if you don't have a hardware divider, this will result
  // in faster operations when the angles are close to the bounds.

  while (x >=  y) do
    Dec(x, y);
  while(x <= -y) do
    Inc(x, y);

  Result:=x;
end;

const
  scales:array[0..7] of UInt32 =
      // 5 decimals is enough for full fix16_t precision
      (1, 10, 100, 1000, 10000, 100000, 100000, 100000);

procedure itoa_loop(var buf: shortstring; scale, value: UInt32; skip: boolean);
var
  digit: byte;
begin
  while (scale>0) do
  begin
    digit:=(value div scale);

    if (not skip) or (digit>0) or (scale = 1) then
    begin
      skip:= false;
      buf:=buf+Chr(Ord('0')+digit);
      value:=value mod scale;
    end;

    scale:=scale div 10;
  end;
end;

procedure Fix16ToStr(const Value: TFix16; const decimals: byte; var OutStr: shortstring);
var
  uvalue, intpart, fracpart, scale: UInt32;
begin
  OutStr:='';

  if Value>=0 then
    uvalue:=value
  else
    uvalue:=-value;

  if Value<0 then
    OutStr:=OutStr+'-';

  //Separate the integer and decimal parts of the value
  intpart:= uvalue shr 16;
  fracpart:= uvalue and $FFFF;
  scale:= scales[decimals and 7];
  fracpart:= Fix16Mul(fracpart, scale);

  if (fracpart >= scale) then
  begin
    //Handle carry from decimal part
    Inc(intpart);
    Dec(fracpart, scale);
  end;

  //Format integer part
  itoa_loop(OutStr, 10000, intpart, true);

  //Format decimal part (if any)
  if (scale <> 1) then
  begin
    OutStr:=OutStr+'.';
    itoa_loop(OutStr, scale div 10, fracpart, false);
  end;
end;

function StrToFix16(const buf: shortstring): TFix16;
var
  i, len: UInt8;
  negative: boolean;
  intpart, fracpart, scale: UInt32;
  cnt: integer;
  value: TFix16;
begin
  i:=1;
  len:=Length(buf);

  while ((buf[i]=' ') and (i<len)) do  //removing spaces
    Inc(i);

  negative:=buf[i]='-';
  if (buf[i]='+') or (buf[i]='-') then
    Inc(i);

  //Decode the integer part
  intpart:=0;
  cnt:= 0;
  while (buf[i] in ['0'..'9']) do
  begin
    intpart:=intpart*10;
    Inc(intpart, Ord(buf[i])-Ord('0'));
    Inc(i);
    Inc(cnt);
  end;

  if ((cnt=0) or (cnt>5) or (intpart>32768) or ((negative=false) and (intpart>32767))) then
  begin
    Result:= fix16_overflow;
    exit;
  end;

  value:=intpart shl 16;

  //Decode the decimal part
  if (buf[i] = '.') or (buf[i] = ',') then
  begin
    Inc(i);

    fracpart:= 0;
    scale:= 1;
    while (buf[i] in ['0'..'9']) and (scale < 100000) do
    begin
      scale:=scale*10;
      fracpart:=fracpart*10;
      Inc(fracpart, Ord(buf[i])-Ord('0'));
      Inc(i); //#1 for not in condition - additional incrementation
    end;

    value:=value+Fix16Div(fracpart, scale);
  end;

  // Verify that there is no garbage left over
  if (i-1)<>len then //(i-1) because of #1
  begin
    Result:=fix16_overflow;
    Exit;
  end;

  if negative then
    Result:=-value
  else
    Result:=value;
end;

end.
