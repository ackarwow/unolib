unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    cmdTestExp: TButton;
    cmdTest2Str: TButton;
    cmdTestLog2: TButton;
    cmdTestPow: TButton;
    cmdTestIntPow: TButton;
    cmdTestSinus: TButton;
    cmdTests: TButton;
    memOutput: TMemo;
    procedure cmdTest2StrClick(Sender: TObject);
    procedure cmdTestExpClick(Sender: TObject);
    procedure cmdTestIntPowClick(Sender: TObject);
    procedure cmdTestLog2Click(Sender: TObject);
    procedure cmdTestPowClick(Sender: TObject);
    procedure cmdTestSinusClick(Sender: TObject);
    procedure cmdTestsClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

uses
  float32, math;

function Float2Str(f: single): string;
begin
  Str(f, Result);
end;

function Str2Float(s: string): single;
var
  {%H-}c: integer;
begin
  Val(s, Result, c);
end;

procedure TForm1.cmdTestsClick(Sender: TObject);
const
  aFloats : array of PChar=('0.12345', '-12.3456', '1000000', '39887.5625', '0.1015625',
                            '2.01', '0.005', '0.001', '0.0000001', '9.99', '3.33',
                            '0.123456789', '0.1234567890', '0.9999999', '0.99999999',
                            '0.0', 'Nan', 'Inf');
var
  f1: single;
  f2, f2a, f2b, f2c: TFloat32;
  i: UInt8;
  l1: UInt32 absolute f1;
  l2: UInt32 absolute f2;
  s2a: single absolute f2a;
  s2b: single absolute f2b;
  s2c: single absolute f2c;
  bConvErr: boolean;
  Buff: array[0..100] of char;
begin
  memOutput.Clear;

  memOutput.Lines.Add('');
  memOutput.Lines.Add('Test of conversion TFloat32 to str');
  for i:=Low(aFloats) to High(aFloats) do
  begin
    f1:=Str2Float(aFloats[i]);
    Float32ToStr(Buff, Sizeof(Buff), l1);

    memOutput.Lines.Add(Format('"%s", single as longint: 0x%s, TFloat32 as string "%s", by Float2Str: %s',
      [aFloats[i], IntTohex(l1), Buff, Float2Str(f1)]));
  end;

  memOutput.Lines.Add('');
  memOutput.Lines.Add('Test of conversion str to TFloat32');
  for i:=Low(aFloats) to High(aFloats) do
  begin
    f1:=Str2Float(aFloats[i]);
    f2:=StrToFloat32(aFloats[i], Sizeof(Buff),  bConvErr);
    //f2:=StrToFloat32(aFloats[i], memOutput.Lines);
    memOutput.Lines.Add(Format('"%s", single as longint: 0x%s, TFloat32 as longint: 0x%s, OK: %s, ConvErr: %s',
      [aFloats[i], IntTohex(l1), IntToHex(l2), BoolToStr(l1=l2, true), BoolToStr(bConvErr, true)]));
  end;

  memOutput.Lines.Add('');
  memOutput.Lines.Add('Test of addition of TFloat32');
  f2a:=StrToFloat32('2.22', 4,  bConvErr);
  memOutput.Lines.Add(Format('%f + %f', [s2a, s2a]));
  f2b:=Float32Add(f2a, f2a);
  memOutput.Lines.Add(Format('Result:=%f', [s2b]));

  memOutput.Lines.Add('');
  memOutput.Lines.Add('Test of addition of TFloat32');
  f2a:=StrToFloat32('3.33', 4,  bConvErr);
  f2b:=StrToFloat32('3.33', 4,  bConvErr);
  memOutput.Lines.Add(Format('%f + %f', [s2a, s2b]));
  f2c:=Float32Add(f2a, f2b);
  memOutput.Lines.Add(Format('Result:=%f', [s2c]));

  memOutput.Lines.Add('');
  memOutput.Lines.Add('Test of negation of TFloat32');
  f2a:=StrToFloat32('3.33', 4,  bConvErr);
  memOutput.Lines.Add(Format('%f', [s2a]));
  f2b:=Float32Neg(f2a);
  memOutput.Lines.Add(Format('Result:=%f', [s2b]));

  memOutput.Lines.Add('');
  memOutput.Lines.Add('Test of subtraction of TFloat32');
  f2a:=StrToFloat32('9.99', 4,  bConvErr);
  f2b:=StrToFloat32('3.33', 4,  bConvErr);
  memOutput.Lines.Add(Format('%f - %f', [s2a, s2b]));
  f2c:=Float32Sub(f2a, f2b);
  memOutput.Lines.Add(Format('Result:=%f', [s2c]));

  memOutput.Lines.Add('');
  memOutput.Lines.Add('Test of multiplication of TFloat32');
  f2a:=0; f2b:=0;
  f2a:=StrToFloat32('2.01', 4,  bConvErr);
  memOutput.Lines.Add(Format('%f * %f', [s2a, s2a]));
  f2b:=Float32Mul(f2a, f2a);
  memOutput.Lines.Add(Format('Result:=%f', [s2b]));

  memOutput.Lines.Add('');
  memOutput.Lines.Add('Test of multiplication of TFloat32');
  f2a:=0; f2b:=0;
  f2a:=StrToFloat32('0.643353462', 11,  bConvErr);
  f2b:=StrToFloat32('-1', 2,  bConvErr);
  memOutput.Lines.Add(Format('%f * %f', [s2a, s2b]));
  f2c:=Float32Mul(f2a, f2b);
  memOutput.Lines.Add(Format('Result:=%f', [s2c]));

  memOutput.Lines.Add('');
  memOutput.Lines.Add('Test of division of TFloat32');
  f2a:=StrToFloat32('127.03125', 9,  bConvErr);
  f2b:=StrToFloat32('16.9375', 7,  bConvErr);
  memOutput.Lines.Add(Format('%f / %f', [s2a, s2b]));
  f2c:=Float32Div(f2a, f2b);
  memOutput.Lines.Add(Format('Result:=%f', [s2c]));

  memOutput.Lines.Add('');
  memOutput.Lines.Add('Test of division of TFloat32');
  f2a:=StrToFloat32('33.33', 5,  bConvErr);
  f2b:=StrToFloat32('11.11', 5,  bConvErr);
  memOutput.Lines.Add(Format('%f / %f', [s2a, s2b]));
  f2c:=Float32Div(f2a, f2b);
  memOutput.Lines.Add(Format('Result:=%f', [s2c]));

  memOutput.Lines.Add('');
  memOutput.Lines.Add('Test of division of TFloat32');
  f2a:=StrToFloat32('1', 1,  bConvErr);
  f2b:=StrToFloat32('10.00', 5,  bConvErr);
  memOutput.Lines.Add(Format('%f / %f', [s2a, s2b]));
  f2c:=Float32Div(f2a, f2b);
  memOutput.Lines.Add(Format('Result:=%f', [s2c]));
  memOutput.Lines.Add('');

  memOutput.Lines.Add('');
  memOutput.Lines.Add('Test of modulo of TFloat32');
  f2a:=StrToFloat32('123.45', 6,  bConvErr);
  f2b:=StrToFloat32('11.11', 5,  bConvErr);
  memOutput.Lines.Add(Format('%f mod %f', [s2a, s2b]));
  f2c:=Float32Mod(f2a, f2b);
  memOutput.Lines.Add(Format('Result:=%f', [s2c]));
  memOutput.Lines.Add('');

  memOutput.Lines.Add('Test square root of TFloat32');
  f2a:=StrToFloat32('529.0', 5,  bConvErr);
  memOutput.Lines.Add(Format('sqrt(%f)', [s2a]));
  f2c:=Float32Sqrt(f2a);
  memOutput.Lines.Add(Format('Result:=%f', [s2c]));

  memOutput.Lines.Add('Test square root of TFloat32');
  f2a:=StrToFloat32('1.2321', 6,  bConvErr);
  memOutput.Lines.Add(Format('sqrt(%f)', [s2a]));
  f2c:=Float32Sqrt(f2a);
  memOutput.Lines.Add(Format('Result:=%f', [s2c]));
end;

procedure TForm1.cmdTestSinusClick(Sender: TObject);
var
  s1, s3: single;
  f1: TFloat32 absolute s1;
  f2: TFloat32;
  s2: single absolute f2;
  i: Int16;
begin
 memOutput.Clear;
 memOutput.Lines.Add('');
 memOutput.Lines.Add('Test of sinus routine (Float32Sin/CORDIC) by @Dzandaa');
 for i:=-180 to 180 do
 begin
   s1:=i;
   f2:=Float32Sin(f1);
   s3:=sin(s1);

   memOutput.Lines.Add(Format('angle [rad]: %8.8f Float32Sin: %8.8f sin (RTL): %8.8f, diff: %8.8f', [s1, s2, s3, Abs(s3-s2)]));

 end;
end;

function SingleFloat32Sinus(x: Single): Single; // x in Radian
const
  B = 1.273239; // 4 / PI;
  C = -0.405284; // -4 / (PI* PI);
  P = 0.225;
  Q = 0.775;
var
  n: Single;
  AB, Rs: Single;
begin
  AB := int(x / PI);

  Rs := x - (AB * PI);
  x := Rs;
  n := 1.0;

  if((AB / 2) <> (int(AB/2))) then n := -1;

  Result := B * x + C * x * abs(x);
  // Result := P * (Result * abs(Result) -Result) + Result;
  Result := Q * Result + P * Result * abs(Result);

  Result := Result * n;
end;

procedure TForm1.cmdTestLog2Click(Sender: TObject);
const
  sr: single = 55.3859;
var
  fr: TFloat32 absolute sr;

  f2: TFloat32;
  s2: single absolute f2;
  s3: single;
begin
  memOutput.Clear;

  f2:=Float32Log2(fr);
  s3:=Log2(sr);
  memOutput.Lines.Add(Format('Log2: %8.8f, Float32Log2: %8.8f', [s3, s2]));
end;

procedure TForm1.cmdTestExpClick(Sender: TObject);
const
  sp1: single = 1.23456;
var
  fp1: TFloat32 absolute sp1;

  fr: TFloat32;
  sr: single absolute fr;

  r: single;
begin
  memOutput.Clear;

  fr:=Float32Exp(fp1);
  r:=Exp(sp1);
  memOutput.Lines.Add(Format('Exp: %8.8f, Float32Exp: %8.8f', [r, sr]));
end;

procedure TForm1.cmdTest2StrClick(Sender: TObject);
const
  RawTwo: TFloat32 = $40000000;
  Raw_29_1: TFloat32 = $41E8CCCD; //29.1
var
  Buff: array[0..30] of char;
  BuffPtr: PChar;
  duration, cm: TFloat32;
  b: UInt8;
begin
 duration := IntToFloat32(2328);
 //cm:=duration;
 cm:=Float32Div(Float32Div(duration, RAWTwo), Raw_29_1);

 FillChar(Buff, SizeOf(Buff), #0);
 b:=Float32ToStr(Buff,30,cm);
 if b>0 then
   memOutput.Lines.Add(Format(' Cm: %s', [Buff]));

end;

procedure TForm1.cmdTestIntPowClick(Sender: TObject);
const
  sp1: single = 2.0;
  ip2: integer = 3;
var
  fp1: TFloat32 absolute sp1;

  fr: TFloat32;
  sr: single absolute fr;

  r: single;
begin
  memOutput.Clear;

  fr:=Float32IntPow(fp1, ip2);
  r:=IntPower(sp1, ip2);
  memOutput.Lines.Add(Format('IntPower: %8.8f, Float32IntPow: %8.8f', [r, sr]));
end;

procedure TForm1.cmdTestPowClick(Sender: TObject);
const
  //sp1: single = 12.3345;
  //sp2: single = 2.1;
  sp1: single = -2.0;
  sp2: single = 3.2;
var
  fp1: TFloat32 absolute sp1;
  fp2: TFloat32 absolute sp2;

  fr: TFloat32;
  sr: single absolute fr;

  r: single;
begin
  memOutput.Clear;

  fr:=Float32Pow(fp1, fp2);
  r:=Power(sp1, sp2);
  memOutput.Lines.Add(Format('Power: %8.8f, Float32Pow: %8.8f', [r, sr]));
end;

end.


