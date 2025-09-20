unit float32;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

{
  translation of Float32 library from C# to Pascal by Andrzej Karwowski (2024)
  C# source: https://github.com/Kimbatt/soft-float-starter-pack

  conversion from string to float inspired by:
  - https://github.com/keith-packard/snek/blob/main/snek-atof.c
  - https://numeral-systems.com/ieee-754-converter/

  routines by @Dzandaa:
  - Float32Sqrt, Float32Abs, Float32Inv, Float32InvSqrt, Float32Deg2Rad,
    Float32Rad2Deg, Float32Sinus, Float32Cosinus, Float32Tan, Float32Cotan,
    Float32Int, Float32Log2, Float32IntPow
  - and all routines for TFloat32 type

}

interface

type
  TRawFloat32 = UInt32;

  TRawFloat32Rec=bitpacked record
    mantissa: 0..8388607;
    exp: 0..255;
    sign: 0..1;
  end;

const
  MANTISSA_BIT_SIZE = 23;
  UINT32_BIT_SIZE = 32;

function Float32Add(const f1, f2: TRawFloat32): TRawFloat32;
function Float32Neg(const f1: TRawFloat32): TRawFloat32;
function Float32Sub(const f1, f2: TRawFloat32): TRawFloat32;
function Float32Mul(const f1, f2: TRawFloat32): TRawFloat32;
function Float32Comp(const f1, f2: TRawFloat32): Int16;
function Float32Div(const f1, f2: TRawFloat32): TRawFloat32;
function Float32Mod(const f1, f2: TRawFloat32): TRawFloat32;
function Float32ToInt(const f: TRawFloat32): Int32;
function IntToFloat32(const value: Int32):TRawFloat32;

{routines by @Dzandaa}
function Float32Sqrt(const f1: TRawFloat32): TRawFloat32;
function Float32Abs(const f1: TRawFloat32): TRawFloat32;
function Float32Inv(const f1: TRawFloat32): TRawFloat32;
function Float32InvSqrt(const f1: TRawFloat32): TRawFloat32;
function Float32Deg2Rad(const f1: TRawFloat32): TRawFloat32;
function Float32Rad2Deg(const f1: TRawFloat32): TRawFloat32;
function Float32Int(const f1: TRawFloat32): TRawFloat32;
function Float32Sin(const f1: TRawFloat32): TRawFloat32;
function Float32Cos(const f1: TRawFloat32): TRawFloat32;
function Float32Tan(const f1: TRawFloat32): TRawFloat32;
function Float32Cotan(const f1: TRawFloat32): TRawFloat32;
function Float32Log2(const f1: TRawFloat32): TRawFloat32;
function Float32Ln(const f1: TRawFloat32): TRawFloat32;
function Float32Log10(const f1: TRawFloat32): TRawFloat32;
function Float32IntPow(x: TRawFloat32; n: Int32): TRawFloat32;
{end of routines by @Dzandaa}

function StrToFloat32(const s: PChar; const len: UInt8; out rerror: boolean): TRawFloat32;
{by @Dzandaa and @ackarwow}
function Float32ToStr(const s: PChar; const maxlen, decplaces: UInt8; f: TRawFloat32): UInt8;
{by @Dzandaa and @ackarwow}
function Float32ToStrE(const s: PChar; const maxlen, decplaces: UInt8; f: TRawFloat32): UInt8;

//additional test funtion, to be modified as needed
function Float32Test(const f1, f2: TRawFloat32): TRawFloat32;

function Float32Pow(const f1, f2: TRawFloat32): TRawFloat32;
function Float32Exp(const f1: TRawFloat32): TRawFloat32;

function IsNegative(const aValue: TRawFloat32): boolean;

{ TFloat32 }

type
  PkFloat32=packed record
	case
		boolean of
			true: (Raw: TRawFloat32);
			false: (Value: TRawFloat32Rec);
  end;

  TFloat32String=string[21]; //buffers for string representation of TFloat32 numbers

  TFloat32=packed record
		RawData: PkFloat32;

// Mathematic
		class operator + (f1, f2: TFloat32): TFloat32;
		class operator - (f1, f2: TFloat32): TFloat32;
		class operator * (f1, f2: TFloat32): TFloat32;
		class operator / (f1, f2: TFloat32): TFloat32;
		class operator mod (f1, f2: TFloat32): TFloat32;

// Comparison
		class operator = (f1, f2: TFloat32): boolean;
		class operator > (f1, f2: TFloat32): boolean;
		class operator < (f1, f2: TFloat32): boolean;
		class operator >= (f1, f2: TFloat32): boolean;
		class operator <= (f1, f2: TFloat32): boolean;
		class operator <> (f1, f2: TFloat32): boolean;

// Creation and Strings
		constructor Create(f1: TRawFloat32);
		constructor Create(Str: TFloat32String);
		function ToString(DecPlaces: UInt8):TFloat32String;
		function ToStringE(DecPlaces: UInt8):TFloat32String;
		function ToInt32():Int32;
		function Sign(): Integer;
		function Frac(): Integer;
		function Exp(): Integer;
		function Raw(): UInt32;
	end;

// Scientific
function StringToFloat32(Str: TFloat32String): TFloat32;
function SqrtFloat32(const f1: TFloat32): TFloat32;
function AbsFloat32(const f1: TFloat32): TFloat32;
function InvFloat32(const f1: TFloat32): TFloat32;
function InvSqrtFloat32(const f1: TFloat32): TFloat32;
function Deg2RadFloat32(const f1: TFloat32): TFloat32;
function Rad2DegFloat32(const f1: TFloat32): TFloat32;
function IntFloat32(const f1: TFloat32): TFloat32;
function SinFloat32(const f1: TFloat32): TFloat32;
function CosFloat32(const f1: TFloat32): TFloat32;
function TanFloat32(const f1: TFloat32): TFloat32;
function CotanFloat32(const f1: TFloat32): TFloat32;
function Log2Float32(const f1: TFloat32): TFloat32;
function LnFloat32(const f1: TFloat32): TFloat32;
function Log10Float32(const f1: TFloat32): TFloat32;
function IntPowFloat32(f1: TFloat32; n: Int32): TFloat32;
function Int32ToFloat32(const value: Int32):TFloat32;
function NegFloat32(const f1: TFloat32):TFloat32;

implementation

uses
  stringutils
{$ifdef DebugStrings}
  Dialogs, SysUtils, StrUtils
{$endif}
  ;

{ TRawFloat32 Routines }

const
  SignMask: TRawFloat32 = $80000000;
  MantissaBits: Int32 = 23;
  ExponentBias: Int32 = 127;

  RawZero: TRawFloat32 = 0;
  RawNaN: TRawFloat32 = $FFC00000;//same as float.NaN
  RawPositiveInfinity: TRawFloat32 = $7F800000;
  RawNegativeInfinity: TRawFloat32 = {(RawPositiveInfinity xor SignMask)} $FF800000;
  RawOne: TRawFloat32 = $3F800000;
  RawTwo: TRawFloat32 = $40000000;
  RawTen: TRawFloat32 = $41200000;
  //RawMinusOne: TRawFloat32 =  {RawOne xor SignMask} $BF800000;
  //RawMaxValue: TRawFloat32 =  $7F7FFFFF;
  //RawMinValue: TRawFloat32 =  { $7F7FFFFF xor SignMask} $FF7FFFFF;
  //RawEpsilon: TRawFloat32 = $00000001;
  //ULONG_MAX:UInt32 = $FFFFFFFF;
  ULONG_MAX_DIV_10:UInt32 = $FFFFFFFF div 10;

const
  //MostSignificantBit table
  msb: array[0..255] of Int8 =
  ( -1, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
     5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
     6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
     6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
     7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
     7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
     7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
     7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7);

  debruijn32: array[0..63] of Int32=
    (32, 8,  17, -1, -1, 14, -1, -1, -1, 20, -1, -1, -1, 28, -1, 18,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0,  26, 25, 24,
    4,  11, 23, 31, 3,  7,  10, 16, 22, 30, -1, -1, 2,  6,  13, 9,
    -1, 15, -1, 21, -1, 29, 19, -1, -1, -1, -1, -1, 1,  27, 5,  12);

  normalizeAmounts: array[0..31] of UInt8 =
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8, 8, 8, 8, 8, 16, 16, 16, 16, 16, 16, 16, 16, 24, 24, 24, 24, 24, 24, 24);


// Returns the leading zero count of the given 32-bit integer
function clz(x: Int32): Int32;
const
  c: UInt32 = $8c0b2891;
var
  xu: UInt32 absolute x;
begin
  x:=x or x shr 1;
  x:=x or x shr 2;
  x:=x or x shr 4;
  x:=x or x shr 8;
  x:=x or x shr 16;

  Result:=debruijn32[UInt32(xu * c) shr 26];
end;

function BitScanReverse8(const b: {UInt8}Int32): Int8; inline;
begin
  Result:=msb[b];
end;

//please do not change and use carefully
function RawMantissa(const aVal: TRawFloat32): UInt32; inline;
begin
  Result:=UInt32(aVal) and $7FFFFF;
  //Result:= TRawFloat32Rec(aVal).mantissa;
end;

//please do not change and use carefully
function RawExponent(const aVal: TRawFloat32): UInt8; inline;
begin
  Result:=UInt8(UInt32(aVal) shr MantissaBits);
  //Result:= TRawFloat32Rec(aVal).exp;
end;

//please do not change and use carefully
function RawSign(const aVal: TRawFloat32): UInt32; inline;
var
  sVal: Int32 absolute aVal;
begin
  Result:= UInt32(Int64(sVal) shr 31); ////HERE cast to Int64
  //Result:= TRawFloat32Rec(aVal).sign;
end;

function Mantissa(const aVal: TRawFloat32): Int32;
var
  sign: UInt32;
begin
  sign:= RawSign(aVal);
  if (RawExponent(aVal) <> 0) then
    Result:=Int32(((RawMantissa(aVal) or $800000) xor sign) - sign)
  else
    Result:=Int32((RawMantissa(aVal) xor sign) - sign);
end;

function Exponent(const aValue: TRawFloat32): Int8;
begin
  Result:=Int8(RawExponent(aValue) - ExponentBias);
end;

//negative value of number/operator -
function Float32Neg(const f1: TRawFloat32): TRawFloat32;
begin
  Result:=f1 xor $80000000;
end;

function IsFinite(const aValue: TRawFloat32): boolean;
begin
  Result:=RawExponent(aValue) <> 255;
end;

function IsInfinity(const aValue: TRawFloat32): boolean;
begin
  Result:=(aValue and $7FFFFFFF) = $7F800000;
end;

function IsNegativeInfinity(const aValue: TRawFloat32): boolean;
begin
  Result:=aValue=RawNegativeInfinity;
end;

function IsPosiiveInfinity(const aValue: TRawFloat32): boolean;
begin
  Result:=aValue=RawPositiveInfinity;
end;

function IsZero(const aValue: TRawFloat32): boolean;
begin
  Result:=(aValue and $7FFFFFFF) = 0;
end;

function IsNAN(const aValue: TRawFloat32): boolean;
begin
  Result:=(RawExponent(aValue) = 255) and not IsInfinity(aValue);
end;

function IsSubnormal(const aValue: TRawFloat32): boolean;
begin
  Result:=(RawExponent(aValue) = 0);
end;

function IsPositive(const aValue: TRawFloat32): boolean;
begin
  Result:=(aValue and $80000000) = 0;
end;

function IsNegative(const aValue: TRawFloat32): boolean;
begin
  Result:=(aValue and $80000000) <> 0;
end;

function FromParts(const sign: boolean; const exponent, mantissa: UInt32): TRawFloat32;
begin
  if sign then
    Result:=SignMask or ((exponent and $ff) shl MantissaBits) or (mantissa and ((UInt32(1) shl MantissaBits) - 1))
  else
    Result:=0 or ((exponent and $ff) shl MantissaBits) or (mantissa and ((UInt32(1) shl MantissaBits) - 1));
end;

function InternalAdd(const f1, f2: TRawFloat32): TRawFloat32;
var
  rawExp1, rawExp2: UInt8;
  deltaExp: Int32;
  man1, man2, man, rawExp, msbIndex, absMan, amount, shift: Int32;
  sign1, sign2, raw: UInt32;

  f1s: Int32 absolute f1;
  f2s: Int32 absolute f2;
begin
  rawExp1:=RawExponent(f1);
  rawExp2:=RawExponent(f2);
  deltaExp:= rawExp1 - rawExp2;

  if (deltaExp >= 0) then
  begin
    {*}if (rawExp1 <> 255) then
    begin //Finite
      {*}if (deltaExp > 25) then
        Exit(f1);
      if (rawExp2 <> 0) then
      begin
        //http://graphics.stanford.edu/~seander/bithacks.html#ConditionalNegate
        {$R-}
        {*}sign1:= UInt32(Int64(f1s) shr 31); ////HERE cast to Int64
        man1:= Int32(((RawMantissa(f1) or $800000) xor sign1) - sign1);

        sign2:= UInt32(Int64(f2s) shr 31); ////HERE cast to Int64
        man2:= Int32(((RawMantissa(f2) or $800000) xor sign2) - sign2);
        {$R+}
      end
      else
      begin//Subnorm
        sign2:= UInt32(Int64(f2s) shr 31); ////HERE cast to Int64
        man2:= Int32((RawMantissa(f2) xor sign2) - sign2);
        man1:= Mantissa(f1);

        rawExp2:= 1;
        if (rawExp1 = 0) then
          rawExp1:= 1;
        deltaExp:= rawExp1 - rawExp2;
      end;
      {$R-}
      //int man = (man1 << 6) + ((man2 << 6) >> deltaExp);
      man:= Int32(Int64(man1) shl 6) + Int32((Int64(man2) shl 6) shr deltaExp); //HERE cast to Int64
      {$R+}
      absMan:= Abs(man);
      if (absMan = 0) then
        Exit(RawZero);  //                                          xor $80000000;
      rawExp:= rawExp1 - 6;
      shift:=clz(UInt32(absMan));

      amount:= normalizeAmounts[shift];
      rawExp:=rawExp -amount;
      absMan:=absMan shl amount;

      msbIndex:= BitScanReverse8(absMan shr MantissaBits);
      rawExp:=rawExp+msbIndex;
      absMan:=absMan shr msbIndex;
      if (UInt32(rawExp - 1) < 254) then
      begin
        {*}raw:= (UInt32(man) and $80000000) or (UInt32(rawExp) shl 23) or (absMan and $7FFFFF);
        Exit(TRawFloat32(raw));
      end
      else
      begin
        if (rawExp >= 255) then
        begin //Overflow
          if (man >= 0) then
            Exit(RawPositiveInfinity)
          else
            Exit(RawNegativeInfinity);
        end;
        if (rawExp >= -24) then//Fixme
        begin
          raw:= UInt32(man) and $80000000 or absMan shr (-rawExp + 1);
          Exit(TRawFloat32(raw));
        end;
        Exit(RawZero);
      end
    end
    else
    begin//special
      if (rawExp2 <> 255) then //f1 is NaN, +Inf, -Inf and f2 is finite
        Exit(f1);
      // Both not finite
      if (f1 = f2) then
        Exit(f1)
      else
        Exit(RawNaN);
    end;
  end
  else
  begin
    //ToDo manually write this code
    Exit(Float32Add(f2, f1));//flip operands
  end;
end;

function Float32Add(const f1, f2: TRawFloat32): TRawFloat32;
begin
  //return f1.RawExponent - f2.RawExponent >= 0 ? InternalAdd(f1, f2) : InternalAdd(f2, f1);
  if (RawExponent(f1)-RawExponent(f2))>=0 then
    Result:=InternalAdd(f1, f2)
  else
    Result:=InternalAdd(f2, f1);
end;

function Float32Sub(const f1, f2: TRawFloat32): TRawFloat32;
begin
  Result:=Float32Add(f1, Float32Neg(f2));
end;

function Float32Mul(const f1, f2: TRawFloat32): TRawFloat32;
var
  man1, man2, rawExp1, rawExp2, rawExp, man, shift: Int32;
  sign1, sign2, sign, rawMan1, rawMan2, absMan: UInt32;
  longMan: Int64;
begin
  rawExp1:=RawExponent(f1);

  if (rawExp1 = 0) then
  begin//SubNorm
    sign1:=RawSign(f1); //UInt32(Int32(f1) shr 31);
    rawMan1:= RawMantissa(f1);
    if (rawMan1 = 0) and IsFinite(f2) then
      Exit((f1 xor f2) and SignMask);
    rawExp1:= 1;
    while ((rawMan1 and $800000) = 0) do
    begin
      rawMan1:=rawMan1 shl 1;
      Dec(rawExp1);
    end;
    Assert(rawMan1 shr 23 = 1); //??
    man1:= Int32((rawMan1 xor sign1) - sign1);
  end
  else if (rawExp1 <> 255) then
  begin//Norm
    sign1:= RawSign(f1);//UInt32(Int32(f1) shr 31);
    man1:= Int32(((RawMantissa(f1) or $800000) xor sign1) - sign1);
  end
  else
  begin//Non finite
    if (f1 = RawPositiveInfinity) then
    begin
      if IsZero(f2) then
        Exit(RawNaN);
      if IsNaN(f2) then
        Exit(f2);
      if (Int32(f2) >= 0) then
        Exit(RawPositiveInfinity)
      else
        Exit(RawNegativeInfinity);
    end
    else if (f1 = RawNegativeInfinity) then
    begin
      if IsZero(f2) then
        Exit(RawNaN);
      if IsNaN(f2) then
        Exit(f2);
      if (Int32(f2) < 0) then
        Exit(RawPositiveInfinity)
      else
        Exit(RawNegativeInfinity)
    end
    else Exit(f1);
  end;

  rawExp2:= RawExponent(f2);
  if (rawExp2 = 0) then
  begin //SubNorm
    sign2:= RawSign(f2);// UInt32(Int32(f2) >> 31);
    rawMan2:= RawMantissa(f2);
    if (rawMan2 = 0) then
    begin
      if IsFinite(f1) then // f1 * 0
        Exit((f1 xor f2) and SignMask)
      else // Infinity * 0, NaN * 0
        Exit(RawNaN);
    end;
    shift:= clz(rawMan2 and $F00ffffff) - 8;
    rawMan2:=rawMan2 shl shift;
    rawExp2:= 1 - shift;

    //Assert(rawMan2 shr MantissaBits = 1);
    man2:= Int32((rawMan2 xor sign2) - sign2);
  end
  else if (rawExp2 <> 255) then
  begin //Norm
    sign2:=  RawSign(f2);//UInt32(Int32(f2) shr 31);
    man2:= Int32(((RawMantissa(f2) or $800000) xor sign2) - sign2);
  end
  else
  begin //Non finite
    if (f2 = RawPositiveInfinity) then
    begin
      if IsZero(f1) then
        Exit(RawNaN);
      if (Int32(f1) >= 0) then
        Exit(RawPositiveInfinity)
      else
        Exit(RawNegativeInfinity);
    end
    else if (f2 = RawNegativeInfinity) then
    begin
      if IsZero(f1) then
        Exit(RawNaN);
      if (int32(f1) < 0) then
        Exit(RawPositiveInfinity)
      else
        Exit(RawNegativeInfinity);
    end
    else
      Exit(f2);
  end;

  longMan:= Int64(man1) * Int64(man2);
  man:= Int32(longMan shr MantissaBits);
  //Assert(man <> 0); //Debug
  absMan:= UInt32(Abs(man));
  rawExp:= rawExp1 + rawExp2 - ExponentBias;
  sign:= UInt32(man) and $80000000;
  if ((absMan and $1000000) <> 0) then
  begin
    absMan:=absMan shr 1;
    Inc(rawExp);
  end;
  //Assert(absMan shr 23 = 1); //Debug
  if (rawExp >= 255) then
    Exit(sign xor RawPositiveInfinity);//Overflow

  if (rawExp <= 0) then
  begin //Subnorms/Underflow
    if (rawExp <= -24) then //Fixme - check correct value
      Exit(sign);
    absMan:=absMan shr -rawExp + 1;
    rawExp:= 0;
  end;

  Result:=sign or UInt32(rawExp) shl MantissaBits or absMan and $7FFFFF;
end;

function Equals(const f1, f2: TRawFloat32): boolean;
begin
  if (RawExponent(f1) <> 255) then
    Exit((f1 = f2) or ((f1 and $7FFFFFFF) = 0) and ((f2 and $7FFFFFFF) = 0)) //0==-0
  else
  begin
    if (RawMantissa(f1) = 0) then
      Exit(f1 = f2) //infinities
    else
      Exit(RawMantissa(f2) <> 0);//NaNs are equal for `Equals` (as opposed to the == operator)
  end;
end;

function CompareValue(const A, B  : Int32): Int16;
begin
  result:=1;
  if a=b then
    result:=0
  else
   if a<b then
     result:=-1;
end;

function Float32Comp(const f1, f2: TRawFloat32): Int16;
var
  sign1, sign2: UInt32;
  val1, val2: Int32;
begin
  if (IsNaN(f1) and IsNaN(f2)) then
    Exit(0);

  sign1:= RawSign(f1);// UInt32(Int32(f1) shr 31);
  val1:= Int32((f1 xor (sign1 and $7FFFFFFF)) - sign1);

  sign2:= RawSign(f2);// UInt32(Int32(f2) shr 31);
  val2:= Int32((f2 xor (sign2 and $7FFFFFFF)) - sign2);

  Result:=CompareValue(val1, val2);
end;

function Float32Div(const f1, f2: TRawFloat32): TRawFloat32;
var
  man1, rawExp1, rawMan1: Int32;
  man2, rawExp2, rawMan2: Int32;
  sign1, sign2, absMan, sign: UInt32;
  shift, man, rawExp: Int32;
  longMan: Int64;
begin
  if (IsNAN(f1) or IsNAN(f2)) then
    Exit(RawNAN);

  rawExp1:=RawExponent(f1);
  if (rawExp1 = 0) then
  begin
    // SubNorm
    sign1:= RawSign(f1);
    rawMan1:= int32(RawMantissa(f1));
    if (rawMan1 = 0) then
    begin
        if IsZero(f2) then // 0 / 0
          Exit(RawNaN)
        else  // 0 / f
          Exit((f1 xor f2) and SignMask);
    end;

    shift:= clz(rawMan1 and $00ffffff) - 8;
    rawMan1:=rawMan1 shl shift;
    rawExp1:= 1 - shift;

    //Debug.Assert(rawMan1 >> MantissaBits == 1);
    man1:= Int32((rawMan1 xor sign1) - sign1);
  end
  else if (rawExp1 <> 255) then
  begin
    // Norm
    sign1:= RawSign(f1);
    man1:= Int32(((RawMantissa(f1) or $800000) xor sign1) - sign1);
  end
  else
  begin
    // Non finite
    if (f1 = RawPositiveInfinity) then
    begin
      if (IsZero(f2)) then // Infinity / 0
        Exit(RawPositiveInfinity);

      // +-Infinity / Infinity
      Exit(RawNaN);
    end
    else if (f1 = RawNegativeInfinity) then
    begin
      if IsZero(f2) then // -Infinity / 0
        Exit(RawNegativeInfinity);

      // -Infinity / +-Infinity
      Exit(RawNaN);
    end
    else // NaN
      Exit(f1);
  end;

  rawExp2:= RawExponent(f2);
  if (rawExp2 = 0) then
  begin
    // SubNorm
    sign2:= RawSign(f2);
    rawMan2:= Int32(RawMantissa(f2));
    if (rawMan2 = 0) then // f / 0
      Exit(((f1 xor f2) and SignMask) or RawPositiveInfinity);

    shift:= clz(rawMan2 and $00ffffff) - 8;
    rawMan2:=rawMan2 shl shift;
    rawExp2:= 1 - shift;

    //Debug.Assert(rawMan2 >> MantissaBits == 1);
    man2:= Int32((rawMan2 xor sign2) - sign2);
  end
  else if (rawExp2 <> 255) then
  begin
    // Norm
    sign2:= RawSign(f2);
    man2:= Int32(((RawMantissa(f2) or $800000) xor sign2) - sign2);
  end
  else
  begin
    // Non finite
    if (f2 = RawPositiveInfinity) then
    begin
      if IsZero(f1) then // 0 / Infinity
        Exit(RawZero);

      if (Int32(f1) >= 0) then // f / Infinity
        Exit(RawPositiveInfinity)
      else // -f / Infinity
        Exit(RawNegativeInfinity);
    end
    else if (f2 = RawNegativeInfinity) then
    begin
      if IsZero(f1) then // 0 / -Infinity
        Exit(SignMask);

      if (Int32(f1) < 0) then // -f / -Infinity
        Exit(RawPositiveInfinity)
      else // f / -Infinity
        Exit(RawNegativeInfinity);
    end
    else // NaN
      Exit(f2);
  end;

  longMan:= (Int64(man1) shl MantissaBits) div Int64(man2);
  man:= Int32(longMan);
  //Debug.Assert(man != 0);
  absMan:= UInt32(Abs(man));
  rawExp:= rawExp1 - rawExp2 + ExponentBias;
  sign:= UInt32(man) and $80000000;

  if ((absMan and $800000) = 0) then
  begin
    absMan:=absMan shl 1;
    Dec(rawExp);
  end;

  //Debug.Assert(absMan >> MantissaBits == 1);
  if (rawExp >= 255) then // Overflow
    Exit(sign xor RawPositiveInfinity);

  if (rawExp <= 0) then
  begin
    // Subnorms/Underflow
    if (rawExp <= -24) then
      Exit(sign);

    //absMan >>= -rawExp + 1;
    absMan:=absMan shr UInt32(-rawExp + 1);
    rawExp:= 0;
  end;

  Result:= sign or UInt32(rawExp) shl MantissaBits or absMan and $7FFFFF;
end;

function Float32Mod(const f1, f2: TRawFloat32): TRawFloat32;
var
  uxi, uyi: UInt32;
  ex, ey: Int32;
  sx, i: UInt32;
  uex: UInt32 absolute ex;
begin
  uxi:=f1;
  uyi:=f2;
  ex:= Int32(uxi shr 23 and $ff);
  ey:= Int32(uyi shr 23 and $ff);
  sx:= uxi and $80000000;

  if ((uyi shl 1) = 0) or IsNaN(f2) or (ex = $ff) then
    Exit(Float32Div(Float32Mul(f1, f2), Float32Mul(f1, f2))); //return (x * y) / (x * y);

  if (uxi shl 1) <= (uyi shl 1) then
  begin
    if (uxi shl 1) = (uyi shl 1) then //return 0.0 * x;
      Exit(RawZero);

    Exit(f1);
  end;

  // normalize x and y
  if (ex = 0) then
  begin
    i:= uxi shl 9;
    while (i shr 31 = 0) do
    begin
      Inc(ex);
      i:=i shl 1;
    end;

    uxi:=uxi shl (-ex + 1);
  end
  else
  begin
    uxi:=uxi and ($FFFFFFFF shr 9);
    uxi:=uxi or (1 shl 23);
  end;

  if (ey = 0) then
  begin
    i:= uyi shl 9;
    while (i shr 31 = 0) do
    begin
      Inc(ey);
      i:=i shl 1;
    end;

    uyi:=uyi shl (-ey + 1);
  end
  else
  begin
    uyi:=uyi and ($FFFFFFFF shr 9);
    uyi:=uyi or (1 shl 23);
  end;

  // x mod y
  while (ex > ey) do
  begin
    i:= UInt32(uxi - uyi);
    if (i shr 31 = 0) then
    begin
      if (i = 0) then //return 0.0 * x;
        Exit (RawZero);

      uxi:= i;
    end;

    uxi:=uxi shl 1;

    Dec(ex, 1);
  end;

  i:= UInt32(uxi - uyi);
  if (i shr 31 = 0) then
  begin
    if (i = 0) then //return 0.0 * x;
      Exit (RawZero);

    uxi:= i;
  end;

  while (uxi shr 23 = 0) do
  begin
    uxi:=uxi shl 1;
    Dec(ex, 1);
  end;

  // scale result up
  if (ex > 0) then
  begin
    Dec(uxi, 1 shl 23);
    uxi:=uxi or uex shl 23;
  end
  else
    uxi:=uxi shr (-ex + 1);

  uxi:=uxi or sx;
  Result:=uxi;
end;

function Float32ToInt(const f: TRawFloat32): Int32;
var
  shift: Int32;
  mantissa, value: Int32;
begin
  if (Exponent(f) < 0) then
    Exit(0);

  shift:= MantissaBits - Exponent(f);
  mantissa:= Int32(RawMantissa(f) or (UInt32(1) shl MantissaBits));
  if shift < 0 then
    value:=mantissa shl -shift
  else
    value:=mantissa shr shift;

  if IsPositive(f) then
    Result:=value
  else
    Result:=-value;
end;

function IntToFloat32(const value: Int32):TRawFloat32;
var
  si: UInt32 absolute value;
  negative: boolean;
  u, shifts, lzcnt, count: Int32;
  exponent: UInt32;
begin
  if (value = 0) then
    Exit(0);

  if (si = 0) then // special case
    Exit($cf000000);

  negative:= value < 0;
  u:= Abs(value);

  lzcnt:= clz(u);
  if (lzcnt < 8) then
  begin
    count:= 8 - lzcnt;
    u:=u shr count;
    shifts:= -count;
  end
  else
  begin
    count:= lzcnt - 8;
    u:=u shl count;
    shifts:= count;
  end;

  exponent:= UInt32(ExponentBias + MantissaBits - shifts);
  Result:=FromParts(negative, exponent, UInt32(u));
end;

function Float32Sqrt(const f1: TRawFloat32): TRawFloat32;
var
  f1s: Int32 absolute f1;
  sign: Int32 = Int32($80000000);
  ix, s, q, m, t, i: Int32;
  r: UInt32;
  uix: UInt32 absolute ix;
begin
  ix:=f1s;

  // take care of Inf and NaN
  if ((UInt32(ix) and $7f800000) = $7f800000) then
  begin
    //return x * x + x; /* sqrt(NaN)=NaN, sqrt(+inf)=+inf, sqrt(-inf)=sNaN */
    if (IsNaN(f1) or IsNegativeInfinity(f1)) then
      Exit(RawNAN)
    else // if (x.IsPositiveInfinity())
      Exit(RawPositiveInfinity);
  end;

  // take care of zero
  if (ix <= 0) then
  begin
    if ((ix and not sign) = 0) then
      Exit(f1); // sqrt(+-0) = +-0

    if (ix < 0) then  //return (x - x) / (x - x); sqrt(-ve) = sNaN
      Exit(RawNaN);
  end;

  // normalize x
  m:= ix shr 23;
  if (m = 0) then
  begin
    // subnormal x
    i:= 0;
    while ((ix and $00800000) = 0) do
    begin
      ix:=ix shl 1;
      Inc(i);
    end;

    Dec(m, i - 1);
  end;

  Dec(m, 127); // unbias exponent
  ix:= (ix and $007fffff) or $00800000;
  if ((m and 1) = 1) then
  begin
    // odd m, double x to make it even
    ix:=ix+ix;
  end;

  m:=m shr 1; // m = [m/2]

  // generate sqrt(x) bit by bit
  ix:=ix+ix;
  q:= 0;
  s:= 0;
  r:= $01000000; // r = moving bit from right to left

  while (r <> 0) do
  begin
    t:= s + Int32(r);
    if (t <= ix) then
    begin
      s:= t + Int32(r);
      Dec(ix, t);
      Inc(q, Int32(r));
    end;

    ix:=ix+ix;
    r:=r shr 1;
  end;

  // use floating add to find out rounding direction
  if (ix <> 0) then
    q:=q+(q and 1);

  ix:= (q shr 1) + $3f000000;
  ix:=ix+(m shl 23);
  Result:=uix;
end;

{routines by @Dzandaa 27-28.12.2024
 adapted from "The Software Optimization cook book"}

// ***********************
// ***** Square Root *****
// ***********************
{function Float32Sqrt(const f1: TRawFloat32): TRawFloat32;
var
  f1s: Int32 absolute f1;
begin
  Result := Int32(Int64((f1s + 127 shl 23) shr 1));
end;}

// ********************
// ***** Absolute *****
// ********************
function Float32Abs(const f1: TRawFloat32): TRawFloat32;
var
  f1s: Int32 absolute f1;
begin
  if (RawExponent(f1) <> 255) or IsInfinity(f1) then // added by AK
    Result := Int32(Int64(f1s) and $7FFFFFFF)
  else
    Result:=f1; // Leave NaN untouched
end;

// *******************
// ***** Inverse *****
// *******************
function Float32Inv(const f1: TRawFloat32): TRawFloat32;
var
  //f1s: Int32 absolute f1;
  s: UInt8;
  f: TRawFloat32;
begin
  //Result := Int32($7F000000 - Int64(f1s));
  f:=f1;
  s:=TRawFloat32Rec(f).sign;
  if s=0 then s:=1 else s:=0;
  TRawFloat32Rec(f).sign:=s;
  Result:=f;
end;

// *******************************
// ***** Inverse Square Root *****
// *******************************
function Float32InvSqrt(const f1: TRawFloat32): TRawFloat32;
var
  f1s: Int32 absolute f1;
begin
  Result := $5F375A86 - (f1s shr 1);
end;

function Float32Deg2Rad(const f1: TRawFloat32): TRawFloat32;
begin
  Result := Float32Mul(f1, $3C8EFA39);
end;

function Float32Rad2Deg(const f1: TRawFloat32): TRawFloat32;
begin
  Result := Float32Mul(f1,$42652EE1);
end;

function Float32Int(const f1: TRawFloat32): TRawFloat32;
begin
 exit((IntToFloat32(Float32ToInt(f1))));
end;

type
  SCRes = Packed record
	  Si: TRawFloat32;
	  Co: TRawFloat32;
  end;

// ************************************************************
// ***** Return Sinus and Cosinus of any number in Radian *****
// ************************************************************
function Float32CORDIC(const f1: TRawFloat32): SCRes;
Const
	D90  = $3FC90FDB; // 1.570796
	D360 = $40C90FDB; // 6.283185
	K= $3F1B74EE; // Computed in conv2Hexa Here for 32 Values
 CordicLookup: Array[0..31] of Uint32 =
(
$3F490FDB, $3EED6338, $3E7ADBB0, $3DFEADD5, $3D7FAADE, $3CFFEAAE, $3C7FFAAB, $3BFFFEAB,
$3B7FFFAB, $3AFFFFEB, $3A7FFFFB, $39FFFFFF, $39800000, $39000000, $38800000, $38000000,
$37800000, $37000000, $36800000, $36000000, $35800000, $35000000, $34800000, $34000000,
$33800000, $33000000, $32800000, $32000000, $31800000, $31000000, $30800000, $30000000
);
var
	Res: SCRes;
	Co, Si, z, v: TRawFloat32;
	d, tCo, tSi, tz: TRawFloat32;
	rCo, rSi: TRawFloat32;
	i: uint16;
var
	AB, Theta: TRawFloat32;
 Quad: uint16;
begin
	AB := Float32Int(Float32Div(f1, D360));
	Theta := Float32Sub(f1, Float32Mul(AB, D360));  // 0 to 2PI  (0 to 360)

	if((Theta and $80000000) = $80000000) then Theta := Float32Add(Theta, D360);
 Quad := Float32ToInt(Float32Div(Theta, D90)) Mod 4;
 Theta := Float32Sub(Theta, Float32Mul(IntToFloat32(Quad), D90));

	Co := K;
	Si := $0; // O.0
	z := Theta;
	v := $3F800000; // 1.0
	Res.Si := $0;
	Res.Co := $0;
	for i := 0 to 31 do // Number of values in LookupTable
	begin

		if((Float32Comp(z, $0) >= 0)) then
			d := $3F800000 // 1.0
		else
			d := $BF800000; // -1

		tCo := Float32Sub(Co, Float32Mul(d, Float32Mul(Si, v)));
		tSi := Float32Add(Si, Float32Mul(d, Float32Mul(Co, v)));
		tz := Float32Sub(z, Float32Mul(d, CordicLookup[i]));
		Co := tCo;
		Si := tSi;
		z := tz;
		v := Float32Mul(v, $3F000000); // 0.5
		// v := Float32Div(v, $40000000); // 2.0
	end;


	case Quad of
	 0:
		begin
			rSi := Si; // Sinus
			rCo := Co; // Cosinus
		end;
		1:
		begin
			rSi := Co; // Cosinus
			rCo := Float32Neg(Si); // -Sinus
		end;
		2:
		begin
			rSi := Float32Neg(Si); // -Sinus
			rCo := Float32Neg(Co); // -Cosinus
		end;
		3:
		begin
			rSi := Float32Neg(Co); // - Cosinus
			rCo := Si; // Sinus
		end;
	end;

 Res.Co := rCo;
	Res.Si := rSi;
	result := Res;

end;

function Float32Sin(const f1: TRawFloat32): TRawFloat32;
var
	Res: SCRes;
begin
	Res := Float32CORDIC(f1);
	Result := Res.Si;
end;

function Float32Cos(const f1: TRawFloat32): TRawFloat32;
var
	Res: SCRes;
begin
	Res := Float32CORDIC(f1);
	Result := Res.Co;
end;

function Float32Tan(const f1: TRawFloat32): TRawFloat32;
var
	Res: SCRes;
begin
	Res := Float32CORDIC(f1);
	if(Equals(Res.Co, RawZero)) then
		Result := RawNaN
	else
		Result := Float32Div(Res.Si, Res.Co);
end;

function Float32Cotan(const f1: TRawFloat32): TRawFloat32;
var
	Res: SCRes;
begin
	Res := Float32CORDIC(f1);
	if(Equals(Res.Si, RawZero)) then
		Result := RawNaN
	else
		Result := Float32Div(Res.Co, Res.Si);
end;

function Float32Log2(const f1: TRawFloat32): TRawFloat32;
const
  IVLN2HI_U32: UInt32 = $3fb8b000; // 1.4428710938e+00
  IVLN2LO_U32: UInt32 = $b9389ad4; // -1.7605285393e-04
  LG1_U32: UInt32 = $3f2aaaaa; // 0.66666662693 /*  0xaaaaaa.0p-24*/
  LG2_U32: UInt32 = $3eccce13; // 0.40000972152 /*  0xccce13.0p-25 */
  LG3_U32: UInt32 = $3e91e9ee; // 0.28498786688 /*  0x91e9ee.0p-25 */
  LG4_U32: UInt32 = $3e789e26; // 0.24279078841 /*  0xf89e26.0p-26 */
var
  x1p25f: TRawFloat32;
  hfsq, f, s, z, r, w, t1, t2, hi, lo: TRawFloat32;
  ui, ix: UInt32;
  k: Int32;
  x: TRawFloat32;
  lom1, lom2, him, ta, tb: TRawFloat32;
begin
 x := f1;
 x1p25f := $4c000000;
 ui := x;

 ix := ui;
 k := 0;

 if ((ix < $00800000) or ((ix shr 31)>0)) then
 begin
   // x < 2**-126
   if ((ix shl 1)= 0) then //return -1. / (x * x); /* log(+-0)=-inf
     exit(RawNegativeInfinity);

   if ((ix shr 31) > 0) then // Negative
     exit(RawNaN);

   //subnormal number, scale up x
   k := k - 25;
   x := Float32Mul(x, x1p25f);
   ui := x;
   ix := ui;
 end
 else if (ix >= $7f800000) then
   exit(x)
 else if (ix = $3f800000) then
   exit(RawZero);

 // reduce x into [sqrt(2)/2, sqrt(2)]
 ix := ix + $3f800000 - $3f3504f3;
 k := k + Int32(ix shr 23) - $7f;
 ix := (ix and $007fffff) + $3f3504f3;
 ui := ix;
 x := ui;

 f := Float32Sub(x, RawOne); // 1.0
 s := Float32Div(f , Float32Add($40000000, f));  // 2.0 (AK: maybe RawTwo needed)
 z := Float32Mul(s, s);
 w := Float32Mul(z, z);

 //t1 = w * (sfloat.FromRaw(LG2_U32) + w * sfloat.FromRaw(LG4_U32));
 ta := Float32Mul(w, LG4_U32);
 tb := Float32Add(LG2_U32, ta);
 t1 := Float32Mul(w, tb);

 //t2 = z * (sfloat.FromRaw(LG1_U32) + w * sfloat.FromRaw(LG3_U32));
 ta := Float32Mul(w, LG3_U32);
 tb := Float32Add(LG1_U32, ta);
 t2 := Float32Mul(z, tb);

 r := Float32Add(t2, t1);
 hfsq := Float32Mul($3F000000, Float32Mul(f, f));  // 0.5 (AK: maybe RawHalf needed)
 hi := Float32Sub(f, hfsq);
 ui := hi;
 ui := ui and $fffff000;
 hi := ui;

 lo := Float32Sub(f, hi);
 lo := Float32Sub(lo, hfsq);
 lo := Float32Add(lo, Float32mul(s, Float32Add(hfsq, r)));  //  lo = f - hi - hfsq + s * (hfsq + r);

 //return (lo + hi) * sfloat.FromRaw(IVLN2LO_U32) + lo * sfloat.FromRaw(IVLN2HI_U32) + hi * sfloat.FromRaw(IVLN2HI_U32) + (sfloat)k;
 // lom1, lom2, him, Ret: TRawFloat32;
 lom1 := Float32Mul(Float32Add(lo, hi), IVLN2LO_U32);  //  (lo + hi) * sfloat.FromRaw(IVLN2LO_U32)
 lom2 := Float32Mul(lo, IVLN2HI_U32); //  lo * sfloat.FromRaw(IVLN2HI_U32)
 him := Float32Mul(hi, IVLN2HI_U32); // hi * sfloat.FromRaw(IVLN2HI_U32)
 Result := Float32Add(Float32Add(Float32Add(lom1, lom2), him), IntToFloat32(k)); // lom1 + lom2 + him + k
end;

function Float32Ln(const f1: TRawFloat32): TRawFloat32;
Const
  LN2: TRawFloat32 = $3F317218;
begin
  Result := Float32Mul(Float32Log2(f1), LN2);
end;

function Float32Log10(const f1: TRawFloat32): TRawFloat32;
Const
  LOG210: TRawFloat32 = $40549A78;
begin
  Result := Float32Div(Float32Log2(f1), LOG210);
end;

function Float32IntPow(x: TRawFloat32; n: Int32): TRawFloat32;
var
 y: TRawFloat32;
begin
  if (n<0) then
  begin
    x := Float32Div(RawOne, x);
    n:=-n;
  end;

  if n = 0 then
    exit(RawOne);

  y := RawOne;

  while n>1 do
  begin
    if (((n div 2) * 2) <> n) then // odd
    begin
      y := Float32Mul(x, y);
      Dec(n);
    end;

    x := Float32Mul(x, x);
    n := n div 2;
  end;

  Result := Float32Mul(x, y);
end;

// Recursive using Int32

function Float32PowInt(f1: TRawFloat32; i1: Int32): TRawFloat32;
var
  x: TRawFloat32;
  n: Int32;
begin
  x := f1;
  n := i1;

  if n < 0 then
    exit(Float32PowInt(Float32Div(RawOne, x), -n))
  else
    if n = 0 then
      exit(RawOne);

  if (((n div 2) * 2) = n) then // Even
    exit(Float32PowInt(Float32Mul(x, x), n div 2))
  else
    exit(Float32Mul(x, Float32PowInt(Float32Mul(x, x), (n-1) div 2)));
end;

//end of functions by @Dzandaa

function CountBits(n: UInt32): UInt8;
var
  cnt: UInt8;
begin
  cnt:= 0;
  while (n>0) do
  begin
    Inc(cnt);
    n:=n shr 1;
  end;
  Exit(cnt);
end;

function RoundUpPow10(aVal: UInt32; aDecCnt: UInt8; out rVal: UInt32): boolean;
var
  d: UInt8;
begin
  Result:=true;

  d:=1;
  rVal:=1;
  while ((aVal>rVal) or (d<=aDecCnt)) do
  begin
    if (rVal<=ULONG_MAX_DIV_10) then
    begin
      rVal:=rVal*10;
      Inc(d);
    end
    else
    begin
      Result:=false;
      Break;
    end;
  end;
end;

//aPos - 0 based index of fraction bit to be rounded
function RoundBinaryFrac(aFraction: UInt32; aPos: UInt8; var aExp: Int8): UInt32;
type
  TUint32Array=bitpacked array [0..31] of 0..1;
var
  FractionArray: TUint32Array absolute aFraction;
  RSVal: UInt8;
  i: UInt8;
  bRound: boolean;
begin
  //bits counted from the end

  RSVal:=0;
  if FractionArray[31-aPos-1]=1 then //Sticky, behind
    RSVal:=RSVal or 1;
  if FractionArray[31-aPos]=1 then //Round
    RSVal:=RSVal or 2;

  case RSVal of
  %00: bRound:=false;
  %01: bRound:=true;
  %10: bRound:=false;
  %11: bRound:=true;
  end;

  if bRound then
  begin
    for i:=31-aPos-1 downto 0 do //simply clear
     FractionArray[i]:=0;

    for i:=31-aPos to 31 do
    begin
      case FractionArray[i] of
      0: begin
           FractionArray[i]:=1;
           Break;
         end;
      1: begin
           FractionArray[i]:=0;
           if i=31 then Inc(aExp); //binary point must therefore be shifted one place to the left
         end;
      end;
    end;
  end
  else
  begin
    for i:=31-aPos-1 downto 0 do //simply clear
      FractionArray[i]:=0;
  end;

  Result:=aFraction;
end;

procedure ShiftBinaryFrac(var aFraction, aTail: UInt32; var aShift: Int8);
type
  TUint32Array=bitpacked array [0..UINT32_BIT_SIZE-1] of 0..1;
var
  FractionArray: TUint32Array absolute aFraction;
  TailArray: TUint32Array absolute aTail;
begin
  //bits counted from the end

  while FractionArray[31]=0 do
  begin
    aFraction:=aFraction shl 1;
    FractionArray[0]:=TailArray[UINT32_BIT_SIZE-1];
    aTail:=aTail shl 1;
    Dec(aShift);
  end;
end;

//warning: conversion does not work for scientific notation (eg. 1.23E-1)
function TryStrToFloat32(const s: PChar; const len: UInt8; out rValue: TRawFloat32): boolean;
const
  FL_ANY       = $01; // any digit was readed
  FL_DOT       = $02; // decimal '.' was
  FL_OVERFLOW  = $04; // too many digits after the decimal '.'
var
  mantissa, num, frac, fracmax, fracbin, fracbintail, tmpbin: UInt32;
  numcnt, fraccnt: UInt8;
  c: char;
  shift, exp, deccnt: Int8;
  flag, j: UInt8;
  p: PChar;
  sign: UInt8;
begin
  //inspired by
  //https://github.com/keith-packard/snek/blob/main/snek-atof.c

  p:=s;
  c:= p^;
  Inc(p);

  flag:= 0;
  num:=0;
  frac:=0;
  exp:= 0;
  sign:= 0; //positive
  deccnt:=0;
  rValue:=0;

  if c='+' then
  begin
    c:= p^;
    Inc(p);
  end
  else if c='-' then
  begin
    sign:=1; //negative
    c:= p^;
    Inc(p);
  end;

  //find integer and fraction
  while true do
  begin
    if (c in ['0'..'9']) then
    begin
      flag:=flag or FL_ANY;

      if (flag and FL_OVERFLOW)=0 then
      begin
        if (flag and FL_DOT)>0 then
        begin
          if frac<=ULONG_MAX_DIV_10 then
          begin
            frac:=frac * 10 + (Ord(c)-Ord('0'));
            Inc(deccnt);
          end
          else
            flag:=flag or FL_OVERFLOW;
        end
        else
        begin
          if num<=ULONG_MAX_DIV_10 then
            num:= num * 10 + (Ord(c)-Ord('0'))
          else
            flag:=flag or FL_OVERFLOW;
        end
      end;
    end
    else if ((c = '.') and ((flag and FL_DOT)=0)) then
      flag:=flag or FL_DOT
    else
      Break;

    c:= p^;
    Inc(p);
  end;

  if ((flag and FL_OVERFLOW)>0) then Exit(false); //OVERFLOW

  if (num=0) and (frac=0) then
  begin
    rValue:=RawZero;
    Exit(true);
  end;

  //inspired by
  //https://numeral-systems.com/ieee-754-converter/

  numcnt:=CountBits(num);
  Inc(exp, numcnt);
  num:=num shl (UINT32_BIT_SIZE-numcnt);

  fraccnt:=0;
  fracbin:=0;
  fracbintail:=0;

  Result:=RoundUpPow10(frac, deccnt, fracmax);
  if not Result then
  begin
    flag:=flag or FL_OVERFLOW;
    Exit(false);
  end;

  if frac>0 then
  begin
    for j:=0 to (UINT32_BIT_SIZE*2)-1 do //32-bit fraction+32-bit tail
    begin
      frac:=frac*2;
      if frac>=fracmax then
      begin
        if j<UINT32_BIT_SIZE then
          fracbin:=fracbin or (UInt32(1) shl (UINT32_BIT_SIZE-1-j))
        else
          fracbintail:=fracbintail or (UInt32(1) shl (UINT32_BIT_SIZE*2-UInt32(1)-j));

        if frac=fracmax then
          break;

        frac:=frac-fracmax;
      end;
    end;
    fraccnt:=j;
  end;

  tmpbin:=fracbin shl (UINT32_BIT_SIZE-numcnt);
  fracbintail:=fracbintail shr numcnt;
  fracbintail:=fracbintail or tmpbin;

  mantissa:=num or (fracbin shr numcnt);

  //shift binary to starting with 1
  shift:=0;
  ShiftBinaryFrac(mantissa, fracbintail, shift);
  Inc(exp, shift);
  Dec(exp); //why?

  if Int16(numcnt)+Int16(fraccnt)+Int16(shift)>MANTISSA_BIT_SIZE then
    mantissa:=RoundBinaryFrac(mantissa, MANTISSA_BIT_SIZE, exp); //0-based index

  mantissa:=mantissa shr (UINT32_BIT_SIZE-MANTISSA_BIT_SIZE-1);

  rValue:=(sign shl 31) or (UInt32(ExponentBias+exp) shl MantissaBits) or (mantissa and $7FFFFF);
end;

function StrToFloat32(const s: PChar; const len: UInt8; out rerror: boolean): TRawFloat32;
begin
  rError:=not TryStrToFloat32(s, len, Result);
end;

//IncDigits parameter indicates an increasing of digits of the number by rounding (e.g. from 9.99 to 10.00)
function InternalFloat32ToStr(const s: PChar; const maxlen, decplaces: UInt8; f: TRawFloat32; out IncDigits: boolean): UInt8; inline;
var
  Val, Int, TenFrac: TRawFloat32;
  IntVal, FracVal, Tmp: Int32;
  Sgn, IntDigits, FracDigits, RndFracDigits, OutDigits, OutLen, i: UInt8;
  LeadingZeroCnt: Int8;
  sPtr: PChar;
begin
  IncDigits:=false;

  if maxlen<4 then //unable to convert, buffer to small
    Exit(0);

  if decplaces>8 then //unable to convert, unsuppotred digits number
  begin
    //='DTL'#0 to large decplaces number
    s[0]:='D';
    s[1]:='T';
    s[2]:='L';
    s[3]:=#0;
    Exit(3);
  end;

  if IsNan(f) then
  begin
    //='NaN'#0
    s[0]:='N';
    s[1]:='a';
    s[2]:='N';
    s[3]:=#0;
    Exit(3);
  end;

  if IsInfinity(f) then
  begin
    //='Inf'#0
    s[0]:='I';
    s[1]:='n';
    s[2]:='f';
    s[3]:=#0;
    Exit(3);
  end;

  if (f<>RawZero) and IsSubnormal(f) then
  begin
    //='Sub#0
    s[0]:='S';
    s[1]:='u';
    s[2]:='b';
    s[3]:=#0;
    Exit(3);
  end;

  OutLen:=0;

  if f=RawZero then
  begin
    Sgn:=0;
    IntVal := 0;
    FracVal := 0;
  end
  else
  begin
    // Convert Digits to 10^Digits
    Sgn:= TRawFloat32Rec(f).sign;

    // Here extract Sign
    Val:=f;
    Val:=Val and $7FFFFFFF;
    Int:= Float32Int(Val);

    TenFrac:= Float32Mul(Float32Sub(Val, Int), Float32IntPow(RawTen, decplaces+1)); // Fractional part, adding 1 to digits for rounding

    IntVal := Float32ToInt(Int);
    FracVal := Float32ToInt(TenFrac);
  end;

  //Rounding
  FracDigits:=UInt32Digits(FracVal);
  IntDigits:=UInt32Digits(IntVal);
  i:=FracVal mod 10;
  if i>=5 then
    Inc(FracVal, 10-i);
  RndFracDigits:=UInt32Digits(FracVal);
  if ((RndFracDigits-FracDigits)>0) and ((RndFracDigits-decplaces-1)>0) then
  begin
    Inc(IntVal);
    FracVal:=0;
    if UInt32Digits(IntVal)>IntDigits then
      IncDigits:=true;
  end;
  FracVal:=FracVal div 10;
  FracDigits:=UInt32Digits(FracVal);
  IntDigits:=UInt32Digits(IntVal);

  if (IntVal=0) and (FracVal=0) then
    Sgn:=0;

  sPtr:=s;

  // Check Sign
  if (Sgn > 0) then
  begin
    sPtr^:='-';
    Inc(sPtr);
    Inc(OutLen);
  end;

  // Convert Integer Part to String
  Tmp := IntVal;
  if (Tmp = 0) then
  begin
    sPtr^:='0';
    Inc(sPtr);
    Inc(OutLen);
  end
  else
  begin
    while IntDigits>0 do
    begin
      sPtr^:=Chr((Tmp div UInt32DigitsArray[IntDigits-1]) + $30);
      Inc(sPtr);
      Inc(OutLen);

      if OutLen=(MaxLen-1) then
      begin
        sPtr^:=#0;
        Exit(MaxLen);
      end;

      Tmp:=Tmp mod UInt32DigitsArray[IntDigits-1];
      Dec(IntDigits);
    end;
  end;

  if decplaces>0 then
  begin
    sPtr^:='.';
    Inc(sPtr);
    Inc(OutLen);
  end;

  if OutLen=(MaxLen-1) then
  begin
    sPtr^:=#0;
    Exit(MaxLen);
  end;

  // Convert Fractional to String

  LeadingZeroCnt:=decplaces-FracDigits;
  if LeadingZeroCnt>0 then
    for i:=0 to LeadingZeroCnt-1 do
    begin
      sPtr^:='0';
      Inc(sPtr);
      Inc(OutLen);

      if OutLen=(MaxLen-1) then
      begin
        sPtr^:=#0;
        Exit(MaxLen);
      end;
    end;

  Tmp := FracVal;
  OutDigits:=decplaces;

  while ((FracDigits>0) and (OutDigits>0)) do
  begin
    sPtr^:=Chr((Tmp div UInt32DigitsArray[FracDigits-1]) + $30);
    Inc(sPtr);
    Inc(OutLen);

    if OutLen=(MaxLen-1) then
    begin
      sPtr^:=#0;
      Exit(MaxLen);
    end;

    Tmp:=Tmp mod UInt32DigitsArray[FracDigits-1];
    Dec(FracDigits);
    Dec(OutDigits);
  end;

  sPtr^:=#0;
  Result:=OutLen;
end;

//return length
//s must indicate an array of char with length of at least 4
//maxlen - size of s
//decplaces - depends on number and limitations of TRawFloat32, should work efectively with 0-6 digits
function Float32ToStr(const s: PChar; const maxlen, decplaces: UInt8; f: TRawFloat32): UInt8;
var
  IncDigits: boolean;
begin
  Result:=InternalFloat32ToStr(s, maxlen, decplaces, f, IncDigits);
end;

function Float32ToStrE(const s: PChar; const maxlen, decplaces: UInt8; f: TRawFloat32): UInt8;
var
  Val: TRawFloat32;
  IExp: UInt16;
  Sgn, OutDigits, Len, Pos: UInt8;
  P: PChar;
  IncDigits: boolean;
begin
  if maxlen<4 then //unable to convert, buffer to small
    Exit(0);

  if decplaces>8 then //unable to convert, unsuppotred digits number
  begin
    //='DTL'#0 to large decplaces number
    s[0]:='D';
    s[1]:='T';
    s[2]:='L';
    s[3]:=#0;
    Exit(3);
  end;

  OutDigits:=decplaces;
  if OutDigits<2 then
    OutDigits:=2;

  //format: -0.[digits including E]-nnn+null terminator -> max 16
  if maxlen<(OutDigits+8) then
  begin
    //='BTS'#0 buffer to small
    s[0]:='B';
    s[1]:='T';
    s[2]:='S';
    s[3]:=#0;
    Exit(3);
  end;

  if IsNan(f) then
  begin
    //='NaN'#0
    s[0]:='N';
    s[1]:='a';
    s[2]:='N';
    s[3]:=#0;
    Exit(3);
  end;

  if IsInfinity(f) then
  begin
    //='Inf'#0
    s[0]:='I';
    s[1]:='n';
    s[2]:='f';
    s[3]:=#0;
    Exit(3);
  end;

  if (f<>RawZero) and IsSubnormal(f) then
  begin
    //='Sub#0
    s[0]:='S';
    s[1]:='u';
    s[2]:='b';
    s[3]:=#0;
    Exit(3);
  end;

  Pos:=0;
  Val:=f;
  Sgn := TRawFloat32Rec(Val).sign;
  P:=s;

  if (Sgn > 0) then
  begin
    P^:='-';
    Inc(P);
    Inc(Pos);
  end;

  IExp := 0;

  //Test if Value > 0  without sign
  if (Float32Abs(Float32Int(Val)) > RawZero) then
  begin
    Val:= Float32Abs(Val);
    while (Float32Comp(Val, RawTen)>0) do
    begin
      Inc(IExp);
      Val:= Float32Div(Val, RawTen);
    end;
    Len:=InternalFloat32ToStr(P, maxlen-Pos, OutDigits-1, Val, IncDigits);
    if IncDigits then
    begin
      Inc(IExp);
      Val:= Float32Div(Val, RawTen);
      len:= Float32ToStr(P, maxlen-Pos, OutDigits-1, Val);
    end;
    Inc(Pos, Len);
    Inc(P, Len);
    P^:='E';
    Inc(P);
    Inc(Pos);
    P^:='+';
    Inc(P);
    Inc(Pos);
    Len:=UInt16ToStr(P, maxlen-Pos, 3, IExp);
    Inc(Pos, Len);
    Inc(P, Len);
  end
  else if (Val <> RawZero) then
  begin
    Val:= Float32Abs(Val);
    while (Float32Comp(Val, RawOne)<0) do
    begin
      Inc(IExp);
      Val:= Float32Mul(Val, RawTen);
    end;
    len:=InternalFloat32ToStr(P, maxlen-Pos, OutDigits-1, Val, IncDigits);
    if IncDigits then
    begin
      Dec(IExp);
      Val:= Float32Div(Val, RawTen);
      len:= Float32ToStr(P, maxlen-Pos, OutDigits-1, Val);
    end;
    Inc(Pos, Len);
    Inc(P, Len);
    P^:='E';
    Inc(P);
    Inc(Pos);
    if IExp>0 then P^:='-' else P^:='+';
    Inc(P);
    Inc(Pos);
    Len:=UInt16ToStr(P, maxlen-Pos, 3, IExp);
    Inc(Pos, Len);
    Inc(P, Len);
  end
  else
  begin
    Len:=Float32ToStr(P, maxlen-Pos, decplaces, Val);
    Inc(Pos, Len);
    Inc(P, Len);
  end;

  P^:= #0; //null terminator

  Result:=Pos;
end;

function Float32Test(const f1, f2: TRawFloat32): TRawFloat32;
begin
  //write cde to test on Arduino
end;

function scalbnf(x: TRawFloat32; n: Int32): TRawFloat32;
const
  x1p127: TRawFloat32 = $7f000000; // 0x1p127f === 2 ^ 127
  x1p_126: TRawFloat32 = $800000; // 0x1p-126f === 2 ^ -126
  x1p24: TRawFloat32 = $4b800000; // 0x1p24f === 2 ^ 24
begin
  if (n > 127) then
  begin
    x:=Float32Mul(x, x1p127);
    Dec(n, 127);
    if (n > 127) then
    begin
      x:=Float32Mul(x, x1p127);
      Dec(n, 127);
      if (n > 127) then
        n:= 127;
    end
  end
  else if (n < -126) then
  begin
    x:=Float32Mul(x, Float32Mul(x1p_126, x1p24));
    Inc(n, (126 - 24));
    if (n < -126) then
    begin
      x:=Float32Mul(x, Float32Mul(x1p_126, x1p24));
      Inc(n, (126 - 24));
      if (n < -126) then
        n:= -126;
    end;
  end;

  Result:=Float32Mul(x, TRawFloat32((UInt32($7f + n)) shl 23));
end;

// Returns f1 raised to the power f2
function Float32Pow(const f1, f2: TRawFloat32): TRawFloat32;
const
  BP_0_U32: UInt32 = $3f800000; // 1.0
  BP_1_U32: UInt32 = $3fc00000; // 1.5
  DP_H_0_U32: UInt32 = $00000000; // 0.0
  DP_H_1_U32: UInt32 = $3f15c000; // 5.84960938e-01
  DP_L_0_U32: UInt32 = $00000000; // 0.0
  DP_L_1_U32: UInt32 = $35d1cfdc; // 1.56322085e-06
  TWO24_U32: UInt32 = $4b800000; // 16777216.0
  HUGE_U32: UInt32 = $7149f2ca; // 1.0e30
  TINY_U32: UInt32 = $0da24260; // 1.0e-30
  L1_U32: UInt32 = $3f19999a; // 6.0000002384e-01
  L2_U32: UInt32 = $3edb6db7; // 4.2857143283e-01
  L3_U32: UInt32 = $3eaaaaab; // 3.3333334327e-01
  L4_U32: UInt32 = $3e8ba305; // 2.7272811532e-01
  L5_U32: UInt32 = $3e6c3255; // 2.3066075146e-01
  L6_U32: UInt32 = $3e53f142; // 2.0697501302e-01
  P1_U32: UInt32 = $3e2aaaab; // 1.6666667163e-01
  P2_U32: UInt32 = $bb360b61; // -2.7777778450e-03
  P3_U32: UInt32 = $388ab355; // 6.6137559770e-05
  P4_U32: UInt32 = $b5ddea0e; // -1.6533901999e-06
  P5_U32: UInt32 = $3331bb4c; // 4.1381369442e-08
  LG2_U32: UInt32 = $3f317218; // 6.9314718246e-01
  LG2_H_U32: UInt32 = $3f317200; // 6.93145752e-01
  LG2_L_U32: UInt32 = $35bfbe8c; // 1.42860654e-06
  OVT_U32: UInt32 = $3338aa3c; // 4.2995665694e-08 =-(128-log2(ovfl+.5ulp))
  CP_U32: UInt32 = $3f76384f; // 9.6179670095e-01 =2/(3ln2)
  CP_H_U32: UInt32 = $3f764000; // 9.6191406250e-01 =12b cp
  CP_L_U32: UInt32 = $b8f623c6; // -1.1736857402e-04 =tail of cp_h
  IVLN2_U32: UInt32 = $3fb8aa3b; // 1.4426950216e+00
  IVLN2_H_U32: UInt32 = $3fb8aa00; // 1.4426879883e+00
  IVLN2_L_U32: UInt32 = $36eca570; // 7.0526075433e-06
var
  x, y: TRawFloat32;
  z, ax, z_h, z_l, p_h, p_l, y1, t1, t2, r, s, sn, t, u, v, w: TRawFloat32;
  i, j, k, yisint, n, hx, hy, ix, iy, _iS: Int32;
  s2, s_h, s_l, t_h, t_l: TRawFloat32;
begin
  x:=f1;
  y:=f2;

  hx:= Int32(x);
  hy:= Int32(y);

  ix:= hx and $7fffffff;
  iy:= hy and $7fffffff;

  // x**0 = 1, even if x is NaN
  if (iy = 0) then
    Exit(RawOne);

  // 1**y = 1, even if y is NaN
  if (hx = $3f800000) then
    Exit(RawOne);

  // NaN if either arg is NaN
  if (ix > $7f800000) or (iy > $7f800000) then
    Exit(RawNaN);

  // determine if y is an odd int when x < 0
  // yisint = 0       ... y is not an integer
  // yisint = 1       ... y is an odd int
  // yisint = 2       ... y is an even int

  yisint:= 0;
  if (hx < 0) then
  begin
    if (iy >= $4b800000) then
      yisint:= 2 // even integer y
    else if (iy >= $3f800000) then
    begin
      k:= (iy shr 23) - $7f; // exponent
      j:= iy shr (23 - k);
      if ((j shl (23 - k)) = iy) then
        yisint:= 2 - (j and 1);
    end;
  end;

  // special value of y
  if (iy = $7f800000) then
  begin
    // y is +-inf
    if (ix = $3f800000) then // (-1)**+-inf is 1
      Exit(RawOne)
    else if (ix > $3f800000) then // (|x|>1)**+-inf = inf,0
    begin
      if hy >= 0 then
        Exit(y)
      else
        Exit(RawZero);
    end
    else // (|x|<1)**+-inf = 0,inf *
    begin
      if hy >= 0 then
        Exit(RawZero)
      else
        Exit(Float32Neg(y));
    end;
  end;

  if (iy = $3f800000) then
  begin // y is +-1
    if hy >= 0 then
      Exit (x)
    else
      Exit(Float32Div(RawOne, x));
  end;

  if (hy = $40000000) then
  begin // y is 2
    Exit (Float32Mul(x, x));
  end;

  if (hy = $3f000000) { y is  0.5 } and (hx >= 0) then
  begin
    // x >= +0
    Exit (Float32Sqrt(x));
  end;

  ax:= Float32Abs(x);
  // special value of x
  if (ix = $7f800000) or (ix = 0) or (ix = $3f800000) then
  begin
    // x is +-0,+-inf,+-1
    z:= ax;
    if (hy < 0) then //z = (1/|x|)
        z:= Float32Div(RawOne,z);

    if (hx < 0) then
    begin
      if (((ix - $3f800000) or yisint) = 0) then //z = (z - z) / (z - z); /* (-1)**non-int is NaN */
        z:=RawNaN
      else if (yisint = 1) then // (x<0)**odd = -(|x|**odd)
        z:=Float32Neg(z);
    end;

    Exit(z);
  end;

  sn:= RawOne; // sign of result
  if (hx < 0) then
  begin
    if (yisint = 0) then // (x<0)**(non-int) is NaN
      Exit (RawNan);  //return (x - x) / (x - x);

    if (yisint = 1) then // (x<0)**(odd int)
      sn:= Float32Neg(RawOne);
  end;

  // |y| is HUGE
  if (iy > $4d000000) then
  begin
    // if |y| > 2**27
    // over/underflow if x is not close to one
    if (ix < $3f7ffff8) then
    begin
      if (hy < 0) then
        Exit(Float32Mul(sn, Float32Mul(HUGE_U32, HUGE_U32)))
      else
        Exit(Float32Mul(sn, Float32Mul(TINY_U32, TINY_U32)))
    end;

    if (ix > $3f800007) then
    begin
      if (hy > 0) then
        Exit(Float32Mul(sn, Float32Mul(HUGE_U32, HUGE_U32)))
      else
        Exit(Float32Mul(sn, Float32Mul(TINY_U32, TINY_U32)))
    end;

    // now |1-x| is TINY <= 2**-20, suffice to compute
    // log(x) by x-x^2/2+x^3/3-x^4/4
    t:= Float32Sub(ax, RawOne); // t has 20 trailing zeros
    //w = (t * t) * (sfloat.FromRaw(0x3f000000) - t * (sfloat.FromRaw(0x3eaaaaab) - t * sfloat.FromRaw(0x3e800000)));
    w:= Float32Mul(Float32Mul(t, t), Float32Sub($3f000000, Float32Mul(t, (Float32Sub($3eaaaaab, Float32Mul(t, $3e800000))))));
    u:= Float32Mul(IVLN2_H_U32, t); // IVLN2_H has 16 sig. bits
    v:= Float32Sub(Float32Mul(t,IVLN2_L_U32),Float32Mul(w,IVLN2_U32));
    t1:=Float32Add(u,v);
    _iS:= Int32(t1);
    t1:= UInt32(_iS) and $fffff000;
    t2:= Float32Sub(v, Float32Sub(t1, u));
  end
  else
  begin
    n:= 0;
    // take care subnormal number
    if (ix < $00800000) then
    begin
      ax:=Float32Mul(ax, TWO24_U32);
      Dec(n, 24);
      ix:=Int32(ax);
    end;

    n:=n+ ((ix) shr 23) - $7f;
    j:= ix and $007fffff;
    // determine interval
    ix:= j or $3f800000; // normalize ix
    if (j <= $1cc471) then // |x|<sqrt(3/2)
      k:= 0
    else if (j < $5db3d7) then // |x|<sqrt(3)
      k:= 1
    else
    begin
      k:= 0;
      Inc(n);
      Dec(ix, $00800000);
    end;

    ax:= UInt32(ix);

    // compute s = s_h+s_l = (x-1)/(x+1) or (x-1.5)/(x+1.5)
    //u:= ax - sfloat.FromRaw(k == 0 ? BP_0_U32 : BP_1_U32); /* bp[0]=1.0, bp[1]=1.5 */
    //v = sfloat.One / (ax + sfloat.FromRaw(k == 0 ? BP_0_U32 : BP_1_U32));
    if k = 0 then
    begin
      u:= Float32Sub(ax, BP_0_U32);
      v:= Float32Div(RawOne, Float32Add(ax, BP_0_U32));
    end
    else
    begin
      u:= Float32Sub(ax, BP_1_U32);
      v:= Float32Div(RawOne, Float32Add(ax, BP_1_U32));
    end;

    s:= Float32Mul(u, v);
    s_h:= s;
    _iS:= Int32(s_h);
    s_h:= UInt32(_iS) and $fffff000;

    // t_h=ax+bp[k] High
    _iS:= Int32(((UInt32(ix) shr 1) and $fffff000) or $20000000);
    //t_h = sfloat.FromRaw((uint)iS + 0x00400000 + (((uint)k) << 21));
    t_h:= UInt32(_iS) + $00400000 + (UInt32(k) shl 21);
    //t_l = ax - (t_h - sfloat.FromRaw(k == 0 ? BP_0_U32 : BP_1_U32));
    if k = 0 then
      t_l:= Float32Sub(ax, Float32Sub(t_h, BP_0_U32))
    else
      t_l:= Float32Sub(ax, Float32Sub(t_h, BP_1_U32));

    //s_l = v * ((u - s_h * t_h) - s_h * t_l);
    s_l:= Float32Mul(v, Float32Sub(Float32Sub(u, Float32Mul(s_h, t_h)), Float32Mul(s_h, t_l)));

    // compute log(ax)
    s2:= Float32Mul(s, s);
    //r = s2 * s2 * (sfloat.FromRaw(L1_U32) + s2 * (sfloat.FromRaw(L2_U32) + s2 * (sfloat.FromRaw(L3_U32) + s2 * (sfloat.FromRaw(L4_U32) + s2 * (sfloat.FromRaw(L5_U32) + s2 * sfloat.FromRaw(L6_U32))))));
    r:= Float32Mul(Float32Mul(s2, s2),Float32Add(L1_U32, Float32Mul(s2, Float32Add(L2_U32, Float32Mul(s2, Float32Add(L3_U32, Float32Mul(s2, Float32Add(L4_U32, Float32Mul(s2, Float32Add(L5_U32, Float32Mul(s2,L6_U32)))))))))));
    r:= Float32Add(r, Float32Mul(s_l, Float32Add(s_h, s)));
    s2:= Float32Mul(s_h, s_h);
    t_h:= Float32Add($40400000, Float32Add(s2, r));
    _iS:= Int32(t_h);
    t_h:= UInt32(_iS) and $fffff000;
    t_l:= Float32Sub(r, Float32Sub(Float32Sub(t_h, $40400000), s2));

    // u+v = s*(1+...)
    u:= Float32Mul(s_h, t_h);
    v:= Float32Add(Float32Mul(s_l, t_h), Float32Mul(t_l, s));

    //* 2/(3log2)*(s+...)
    p_h:= Float32Add(u,v);
    _iS:= Int32(p_h);
    p_h:= UInt32(_iS) and $fffff000;
    p_l:= Float32Sub(v, Float32Sub(p_h, u));
    z_h:= Float32Mul(CP_H_U32, p_h); // cp_h+cp_l = 2/(3*log2)
    //z_l = sfloat.FromRaw(CP_L_U32) * p_h + p_l * sfloat.FromRaw(CP_U32) + sfloat.FromRaw(k == 0 ? DP_L_0_U32 : DP_L_1_U32);
    if k=0 then
      z_l:= Float32Add(Float32Add(Float32Mul(CP_L_U32, p_h), Float32Mul(p_l, CP_U32)), DP_L_0_U32)
    else
      z_l:= Float32Add(Float32Add(Float32Mul(CP_L_U32, p_h), Float32Mul(p_l, CP_U32)), DP_L_1_U32);

    // log2(ax) = (s+..)*2/(3*log2) = n + dp_h + z_h + z_l
    t:= IntToFloat32(n);
    //t1 = ((z_h + z_l) + sfloat.FromRaw(k == 0 ? DP_H_0_U32 : DP_H_1_U32)) + t;
    if k=0 then
      t1:= Float32Add(Float32Add(Float32Add(z_h, z_l), DP_H_0_U32), t)
    else
      t1:= Float32Add(Float32Add(Float32Add(z_h, z_l), DP_H_1_U32), t);

    _iS:= Int32(t1);
    t1:= UInt32(_iS) and $fffff000;
    //t2 = z_l - (((t1 - t) - sfloat.FromRaw(k == 0 ? DP_H_0_U32 : DP_H_1_U32)) - z_h);
    if k=0 then
      t2:= Float32Sub(z_l, Float32Sub(Float32Sub(Float32Sub(t1, t), DP_H_0_U32), z_h))
    else
      t2:= Float32Sub(z_l, Float32Sub(Float32Sub(Float32Sub(t1, t), DP_H_1_U32), z_h));
  end;

  // split up y into y1+y2 and compute (y1+y2)*(t1+t2)
  _iS:= Int32(y);
  y1:= uint32(_iS) and $fffff000;
  p_l:= Float32Add(Float32Mul(Float32Sub(y, y1), t1), Float32Mul(y, t2));
  p_h:= Float32Mul(y1, t1);
  z:= Float32Add(p_l, p_h);
  j:= Int32(z);
  if (j > $43000000) then // if z > 128
    Exit(Float32Mul(sn, Float32Mul(HUGE_U32, HUGE_U32))) // overflow
  else if (j = $43000000) then // if z == 128
  begin
    if Float32Comp(Float32Add(p_l, OVT_U32), Float32Sub(z, p_h))=1 then
      Exit(Float32Mul(sn, Float32Mul(HUGE_U32, HUGE_U32))) // overflow
  end
  else if ((j and $7fffffff) > $43160000) then // z < -150
      // FIXME: check should be  (uint32_t)j > 0xc3160000
      Exit(Float32Mul(sn, Float32Mul(TINY_U32, TINY_U32))) // underflow
  else if ((UInt32(j) = $c3160000 )// z == -150
       and (Float32Comp(p_l, Float32Sub(z,p_h))<=0)) then
     Exit(Float32Mul(sn, Float32Mul(TINY_U32, TINY_U32))); // underflow

  //
  // compute 2**(p_h+p_l)
  //
  i:= j and $7fffffff;
  k:= (i shr 23) - $7f;
  n:= 0;
  if (i > $3f000000) then
  begin
    // if |z| > 0.5, set n = [z+0.5]
    n:= j + ($00800000 shr (k + 1));
    k:= ((n and $7fffffff) shr 23) - $7f; // new k for n
    t:= UInt32(n) and not (UInt32($007fffff) shr k);
    n:= ((n and $007fffff) or $00800000) shr (23 - k);
    if (j < 0) then
      n:= -n;
    p_h:=Float32Sub(p_h, t);
  end;

  t:= Float32Add(p_l, p_h);
  _iS:= Int32(t);
  t:= UInt32(_iS) and $ffff8000;
  u:= Float32Mul(t, LG2_H_U32);
  //v = (p_l - (t - p_h)) * sfloat.FromRaw(LG2_U32) + t * sfloat.FromRaw(LG2_L_U32);
  v:= Float32Add(Float32Mul(Float32Sub(p_l, Float32Sub(t, p_h)), LG2_U32), Float32Mul(t, LG2_L_U32));
  z:= Float32Add(u, v);
  w:= Float32Sub(v, Float32Sub(z, u));
  t:= Float32Mul(z, z);
  //t1 = z - t * (sfloat.FromRaw(P1_U32) + t * (sfloat.FromRaw(P2_U32) + t * (sfloat.FromRaw(P3_U32) + t * (sfloat.FromRaw(P4_U32) + t * sfloat.FromRaw(P5_U32)))));
  //-> t1 = z - t * (P1_U32 + t * (P2_U32 + t * (P3_U32 + t * (P4_U32 + t * P5_U32))));
  t1:= Float32Sub(z, Float32Mul(t, Float32Add(P1_U32, Float32Mul(t, Float32Add(P2_U32, Float32Mul(t, Float32Add(P3_U32, Float32Mul(t, Float32Add(P4_U32, Float32Mul(t, P5_U32))))))))));
  //r = (z * t1) / (t1 - sfloat.FromRaw(0x40000000)) - (w + z * w);
  r:= Float32Sub(Float32Div(Float32Mul(z, t1), Float32Sub(t1, $40000000)), Float32Add(w, Float32Mul(z, w)));
  z:= Float32Sub(RawOne, Float32Sub(r, z));
  j:= Int32(z);
  j:=j + (n shl 23);
  if ((j shr 23) <= 0) then //subnormal output
    z:= scalbnf(z, n)
  else
    z:= UInt32(j);

  Result:=Float32Mul(sn, z);
end;

// returns the exponent of f1, i.e. the number e to the power f1.
function Float32Exp(const f1: TRawFloat32): TRawFloat32;
const
  LN2_HI_U32: UInt32 = $3f317200; // 6.9314575195e-01
  LN2_LO_U32: UInt32 = $35bfbe8e; // 1.4286067653e-06
  INV_LN2_U32: UInt32 = $3fb8aa3b; // 1.4426950216e+00

  P1_U32: UInt32 = $3e2aaa8f; // 1.6666625440e-1 /*  0xaaaa8f.0p-26 */
  P2_U32: UInt32 = $bb355215; // -2.7667332906e-3 /* -0xb55215.0p-32 */

  x1p127: TRawFloat32 = $7f000000; // 0x1p127f === 2 ^ 127
  //x1p_126: TRawFloat32 = $800000; // 0x1p-126f === 2 ^ -126  /*original 0x1p-149f    ??????????? */
var
  x, xx, c, y: TRawFloat32;
  hx: UInt32;
  sign: Int32;
  signb: boolean;
  k: Int32;
  hi, lo: TRawFloat32;
  kf: TRawFloat32;
begin
  x:=f1;
  hx:= x;
  sign:= Int32(hx shr 31); // sign bit of x
  signb:= sign <> 0;
  hx:=hx and $7fffffff; // high word of |x|

  // special cases
  if (hx >= $42aeac50) then
  begin
    // if |x| >= -87.33655f or NaN
    if (hx > $7f800000) then
      Exit(x); // NaN

    if (hx >= $42b17218) and not signb then
    begin
      // x >= 88.722839f
      // overflow
      x:=Float32Mul(x, x1p127);
      Exit(x);
    end;

    if (signb) then
    begin
      // underflow
      //force_eval!(-x1p_126 / x);

      if (hx >= $42cff1b5) then // x <= -103.972084f
        Exit(RawZero);
    end;
  end;

  // argument reduction
  if (hx > $3eb17218) then
  begin
    // if |x| > 0.5 ln2
    if (hx > $3f851592) then
    begin
      // if |x| > 1.5 ln2                                   3F000000                  BF000000
      //k = (int)(sfloat.FromRaw(INV_LN2_U32) * x + (signb ? (sfloat)0.5f : (sfloat)(-0.5f)));
      if signb then
        k:=Float32ToInt(Float32Add(Float32Mul(INV_LN2_U32, x), $3F000000))
      else
        k:=Float32ToInt(Float32Add(Float32Mul(INV_LN2_U32, x), $BF000000));
    end
    else
      k:= 1 - sign - sign;
    kf:=IntToFloat32(k);
    hi:= Float32Sub(x, Float32Mul(kf, LN2_HI_U32)); // k*ln2hi is exact here
    lo:= Float32Mul(kf, LN2_LO_U32);
    x:= Float32Sub(hi, lo);
  end
  else if (hx > $39000000) then
  begin
    // |x| > 2**-14
    k:= 0;
    hi:= x;
    lo:= RawZero;
  end
  else
  begin
    // raise inexact
    //force_eval!(x1p127 + x);
    Exit(Float32Add(RawOne, x));
  end;

  // x is now in primary range
  xx:= Float32Mul(x, x); 
  c:= Float32Sub(x, Float32Mul(xx, Float32Add(P1_U32, Float32Mul(xx, P2_U32))));
  //y = One + (x * c / (Two - c) - lo + hi);
  y:= Float32Add(RawOne, (Float32Add(Float32Sub(Float32Div(Float32Mul(x, c), Float32Sub(RawTwo, c)), lo), hi)));
  if k=0 then
    Result:=y
  else
    Result:=scalbnf(y, k);
end;

{ TFloat32 Routines }

constructor TFloat32.Create(f1: TRawFloat32);
begin
  RawData.Raw := f1;
end;

constructor TFloat32.Create(Str: TFloat32String);
begin
	RawData.Raw := (StringToFloat32(Str)).RawData.Raw;
end;

class operator TFloat32.+ (f1, f2: TFloat32): TFloat32;
begin
	 Result:=TFloat32(Float32Add(f1.RawData.Raw, f2.RawData.Raw));
end;

class operator TFloat32.- (f1, f2: TFloat32): TFloat32;
begin
	Result:=TFloat32(Float32Sub(f1.RawData.Raw, f2.RawData.Raw));
end;

class operator TFloat32.* (f1, f2: TFloat32): TFloat32;
begin
	Result:=TFloat32(Float32Mul(f1.RawData.Raw, f2.RawData.Raw));
end;

class operator TFloat32./ (f1, f2: TFloat32): TFloat32;
begin
	Result:=TFloat32(Float32Div(f1.RawData.Raw, f2.RawData.Raw));
end;

class operator TFloat32. = (f1, f2: TFloat32): boolean;
begin
	if(f1.RawData.Raw = f2.RawData.Raw) then exit(True);
		exit(False);
end;

class operator TFloat32.>(f1, f2: TFloat32): boolean;
begin
 if(Float32Comp(f1.RawData.Raw, f2.RawData.Raw) > 0) then Result := True
	else Result := False;
end;

class operator TFloat32.<(f1, f2: TFloat32): boolean;
begin
 if(Float32Comp(f1.RawData.Raw, f2.RawData.Raw) < 0) then Result := True
	else Result := False;
end;

class operator TFloat32.>=(f1, f2: TFloat32): boolean;
begin
 if(Float32Comp(f1.RawData.Raw, f2.RawData.Raw) >= 0) then Result := True
	else Result := False;
end;

class operator TFloat32.<=(f1, f2: TFloat32): boolean;
begin
	if(Float32Comp(f1.RawData.Raw, f2.RawData.Raw) <= 0) then Result := True
	else Result := False;
end;

class operator TFloat32.<>(f1, f2: TFloat32): boolean;
begin
	if(f1.RawData.Raw = f2.RawData.Raw) then exit(False);
		exit(True);
end;

class operator TFloat32.mod (f1, f2: TFloat32): TFloat32;
begin
 Result := f1 - f2 * IntFloat32(f1 / f2);
end;


function Int32ToFloat32(const value: Int32):TFloat32;
begin
	Result.RawData.Raw := intToFloat32(value);
end;

function NegFloat32(const f1: TFloat32): TFloat32;
begin
	Result.RawData.Raw  := f1.RawData.Raw  xor $80000000;
end;

function TFloat32.Raw(): UInt32;
begin
	Result := RawData.Raw;
end;

function TFloat32.Sign(): Integer;
begin
	result := RawData.Value.sign;
end;

function TFloat32.Frac(): Integer; //?????
begin
	result := RawData.Value.Mantissa;
end;

function TFloat32.Exp(): Integer;
begin
	result := RawData.Value.Exp - 127;
end;

function SqrtFloat32(const f1: TFloat32): TFloat32;
var
	Ret32: TFloat32;
begin
	Ret32.RawData.Raw := Float32Sqrt(f1.RawData.Raw);
	exit(Ret32);
end;

function AbsFloat32(const f1: TFloat32): TFloat32;
var
	Ret32: TFloat32;
begin
	Ret32.RawData.Raw := Float32Abs(f1.RawData.Raw);
	exit(Ret32);
end;

function InvFloat32(const f1: TFloat32): TFloat32;
var
	Ret32: TFloat32;
begin
 Ret32.RawData.Raw := Float32Inv(f1.RawData.Raw);
	exit(Ret32);
end;

function InvSqrtFloat32(const f1: TFloat32): TFloat32;
var
	Ret32: TFloat32;
begin
	Ret32.RawData.Raw := Float32InvSqrt(f1.RawData.Raw);
	exit(Ret32);
end;

function Deg2RadFloat32(const f1: TFloat32): TFloat32;
var
	Ret32: TFloat32;
begin
	Ret32.RawData.Raw := Float32Deg2Rad(f1.RawData.Raw);
	exit(Ret32);
end;

function Rad2DegFloat32(const f1: TFloat32): TFloat32;
var
	Ret32: TFloat32;
begin
	Ret32.RawData.Raw := Float32Rad2Deg(f1.RawData.Raw);
	exit(Ret32);
end;

function IntFloat32(const f1: TFloat32): TFloat32;
var
	Ret32: TFloat32;
begin
	Ret32.RawData.Raw := Float32Int(f1.RawData.Raw);
	exit(Ret32);
end;

function SinFloat32(const f1: TFloat32): TFloat32;
var
	Ret32: TFloat32;
begin
	Ret32.RawData.Raw := Float32Sin(f1.RawData.Raw);
	exit(Ret32);
end;

function CosFloat32(const f1: TFloat32): TFloat32;
var
	Ret32: TFloat32;
begin
	Ret32.RawData.Raw := Float32Cos(f1.RawData.Raw);
	exit(Ret32);
end;

function TanFloat32(const f1: TFloat32): TFloat32;
var
	Ret32: TFloat32;
begin
	Ret32.RawData.Raw := Float32Tan(f1.RawData.Raw);
	exit(Ret32);
end;

function CotanFloat32(const f1: TFloat32): TFloat32;
var
	Ret32: TFloat32;
begin
	Ret32.RawData.Raw := Float32Cotan(f1.RawData.Raw);
	exit(Ret32);
end;

function Log2Float32(const f1: TFloat32): TFloat32;
var
	Ret32: TFloat32;
begin
	Ret32.RawData.Raw := Float32Log2(f1.RawData.Raw);
	exit(Ret32);
end;

function LnFloat32(const f1: TFloat32): TFloat32;
var
	Ret32: TFloat32;
begin
	Ret32.RawData.Raw := Float32Ln(f1.RawData.Raw);
	exit(Ret32);
end;

function Log10Float32(const f1: TFloat32): TFloat32;
var
	Ret32: TFloat32;
begin
	Ret32.RawData.Raw := Float32Log10(f1.RawData.Raw);
	exit(Ret32);
end;

function IntPowFloat32(f1: TFloat32; n: Int32): TFloat32;
var
	Ret32: TFloat32;
begin
	Ret32.RawData.Raw := Float32IntPow(f1.RawData.Raw, n);
	exit(Ret32);
end;

function StringToFloat32(Str: TFloat32String): TFloat32;
var
	Ret: TFloat32;
	rerror: boolean;
	Len: integer;
	x1: Pchar;
begin
  Str := Str + #0;
	Len := Length(Str);
  if(Len > 0) then x1 := @Str[1] else x1 := Nil;
	Ret.RawData.Raw := StrToFloat32(x1, Len, rerror);
	if(rerror) then
		Ret.RawData.Raw := Ret.RawData.Raw or $FFFFFFFF;
	exit(Ret);
end;

function TFloat32.ToInt32(): Int32;
begin
	Result := Float32ToInt(RawData.Raw);
end;

function TFloat32.ToString(DecPlaces: UInt8): TFloat32String;
var
  StrRes: array[0..20] of char; //max 1 for minus sign+max 10 of integer part+1 decimal separator+max 8 digits+null terminator
begin
  FillChar(StrRes, Sizeof(StrRes), 0); //I do not know why here is hint "Local variable "StrRes" does not seem to be initialized"
  Float32ToStr(StrRes, Sizeof(StrRes), DecPlaces, RawData.Raw);
  Result:=StrRes;
end;

function TFloat32.ToStringE(DecPlaces: UInt8): TFloat32String;
var
  StrRes: array[0..15] of char; //-0.[digits including E]-3 signs of exponent+null terminator -> max 16
begin
  FillChar(StrRes, Sizeof(StrRes), 0); //I do not know why here is hint "Local variable "StrRes" does not seem to be initialized"
  Float32ToStrE(StrRes, Sizeof(StrRes), DecPlaces, RawData.Raw);
  Result:=StrRes;
end;

end.

