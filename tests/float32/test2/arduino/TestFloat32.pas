program TestFloat32;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano))}
{$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

uses
  defs,
  timer,
  digital,
  hardwareserial,
  float32;

const
// Led pin
  LedPin = 13; //internal LED
// Handshake, Values for commands, must match with Lazarus Code.
  H_Send = 'S'; // Send
  H_Receive = 'R'; // Receive
  H_Error = 'E'; // Error
  H_Ready = 'K'; // Ready
//  H_Info = 'I'; // Info

  F_Add = 'A'; //function Add
  F_Sub = 'R'; //function Subtract
  F_Mul = 'M'; //function Multiply
  F_Div = 'D'; //function Divide
  F_Sqr = 'Q'; //function Sqrt
  F_Sin = 'S'; //function Sinus
  F_Cos = 'C'; //function Cosinus
  F_Deg = '1'; //function DegRad
  F_Rad = '2'; //function RadDeg
  F_Abs = '3'; //function Absolute
  F_Neg = '4'; //function Negate
  F_Tan = '5'; //Function Tangent
  F_Ctn = '6'; //Function Cotangent
  F_Mod = 'O'; //function Mod
  F_Lg2 = 'L'; //function Log2
  F_LgN = 'N'; //function LogN
  F_L10 = '0'; //function Log10
  F_InP = 'I'; //function IntPow
  F_Pow = 'P'; //function Pow
  F_Exp = 'E'; //function Exp

  F_Tst = 'T'; //Function special test

type
  TComData = packed record
    Command: char;     // Test Command
    Func:    char;    // function to test
    Param1:  TFloat32; // Parameter1
    Param2:  TFloat32; // Parameter2
    RetVal:  TFloat32; // return value of function
  end;
// you can add Values here for data, must match with Lazarus Code.

const
  StructSize = SizeOf(TComData);


type
  TBufferData = packed record
    case boolean of
      True: (Values: TComData);
      False: (DataBuffer: array[0..StructSize - 1] of byte);
  end;

var
  Buff: TBufferData;

procedure Blink(Count: integer);
var
  i: integer;
begin
  for i := 1 to Count do
  begin
    DigitalWrite(LedPin, HIGH);
    Delay(50);
    DigitalWrite(LedPin, LOW);
    Delay(50);
  end;
end;

procedure SetData(Command, Func: char; Param1, Param2, RetVal: TFloat32);
begin
  Buff.Values.Command := Command;
  Buff.Values.Func := Func;
  Buff.Values.Param1 := Param1;
  Buff.Values.Param2 := Param2;
  Buff.Values.RetVal :=RetVal;
end;

procedure ClearReceiveBuffer();
begin
  while (Serial.Available <> 0) do Serial.ReadChar;
end;

// ****************************
// ***** Process the Data *****
// ****************************

procedure ProcessData();
var
  f1: TFloat32 absolute Buff.Values.Param1;
  f2: TFloat32 absolute Buff.Values.Param2;
  f3: TFloat32 absolute Buff.Values.RetVal;
  i1: Int32;
begin
  Buff.Values.Command:=H_Receive;
  case Buff.Values.Func of
  F_Add: //Add
    f3 := Float32Add(f1, f2);
  F_Sub: //Subtract
    f3 := Float32Sub(f1, f2);
  F_Mul: //Multiply
    f3 := Float32Mul(f1, f2);
  F_Div: //Divide
    f3 := Float32Div(f1, f2);
  F_Sqr: //Sqrt
    f3 := Float32Sqrt(f1);
  F_Sin: //Sinus
    f3 := Float32Sin(f1);
  F_Cos: //Sinus
    f3 := Float32Cos(f1);
  F_Tan: //Tangent
    f3 := Float32Tan(f1);
  F_Ctn: //Cotangent
    f3 := Float32Cotan(f1);
  F_Deg: //Deg2Rad
    f3 := Float32Deg2Rad(f1);
  F_Rad: //Rad2Deg
    f3 := Float32Rad2Deg(f1);
  F_Abs: //Absolute
    f3 := Float32Abs(f1);
  F_Neg: //Negate
    f3 := Float32Neg(f1);
  F_Mod: //Mod
    f3 := Float32Mod(f1, f2);
  F_Lg2: //Log2
    f3 := Float32Log2(f1);
  F_LgN: //LogN
    f3 := Float32Ln(f1);
  F_L10: //Log10
    f3 := Float32Log10(f1);
  F_InP: //IntPow
    begin
      i1 := Float32ToInt(f2);
      f3 := Float32IntPow(f1, i1);
    end;
  F_Pow: //Pow
    f3 := Float32Pow(f1, f2);
  F_Exp: //Exp
    f3 := Float32Exp(f1);

  F_Tst: // special test
    f3 := Float32Test(f1, f2);
  end;
end;


// ****************************
// ***** Setup *****
// ****************************
procedure Setup();
begin
//    RetError := 0; // Error Code

  Serial.Start(57600); // Must be the same as Lazarus code
  SetData(H_Ready, #0, 0, 0, 0); // Arduino Ready
  Serial.WriteBuff(Buff.DataBuffer, StructSize);
end;

// **************************************
// ***** Main Program With ReadBuff *****
// **************************************
var
  b: char;
begin
  Setup();
  delay(100);
  while True do
  begin

    if (Serial.Available = StructSize) then
    begin
      Serial.ReadBuff(Buff.DataBuffer, StructSize);
      b := Buff.Values.Command;
      if (b = H_Send) then
      begin
        ProcessData();
        Serial.WriteBuff(Buff.DataBuffer, StructSize);
        Blink(2);
      end
      else
      begin
        SetData(H_Error, #0, 0, 0, 0);
        Serial.WriteBuff(Buff.DataBuffer, StructSize);
        ClearReceiveBuffer();
        Blink(10);
      end;
    end;
    Delay(45 * StructSize);

  end;

end.
