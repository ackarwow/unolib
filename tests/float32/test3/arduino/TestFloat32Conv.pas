program TestFloat32Conv;

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

  F_Str = 'S'; //function StrToFloat
  F_Flt = 'F'; //function FloatToStr
  F_Tst = 'T'; // Function special test

type
  TStrVal = array[0..15] of char;

  TComData = packed record
    Command: char;     // Test Command
    Func:    char;     // function to test
    FltVal:  TRawFloat32; // float32 value (input in FloatToStr, output in StrToFloat)
    StrLen:  Int8;     // length of str param (input in StrToFloat, output in FloatToStr) or error in conversion (-1, output in StrToFloat)
    StrVal:  TStrVal;  // Parameter - string value (input in StrToFloat, output in FloatToStr)
  end;
// you can add Values here for data, must match with Lazarus Code.

const
  StructSize = SizeOf(TComData);

{$if StructSize>64}
  {$fatal StructSize cannot exceed serial buffer size}
{$endif}

type
  TBufferData = packed record
    case boolean of
      True: (Values: TComData);
      False: (DataBuffer: array[0..StructSize - 1] of byte);
  end;

const
  EmptyStr: TStrVal = (
                        #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,
                        #0,#0,#0,#0,#0,#0
                        );

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

procedure SetData(Command, Func: char; FltVal: TRawFloat32; StrLen: Int8; StrVal:  TStrVal);
begin
  Buff.Values.Command := Command;
  Buff.Values.Func := Func;
  Buff.Values.FltVal := FltVal;
  Buff.Values.StrLen := StrLen;
  Buff.Values.StrVal := StrVal;
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
  bError: boolean;
begin
  Buff.Values.Command:=H_Receive;
  case Buff.Values.Func of
  F_Str: //string to float
    begin
      //Buff.Values.FltVal:=$3F800000//1;
      Buff.Values.FltVal:=StrToFloat32(Buff.Values.StrVal, Buff.Values.StrLen, bError);
      if bError then Buff.Values.StrLen:=-1
      else Buff.Values.StrLen:=0;
    end;
  F_Flt: //float to string
    Buff.Values.StrLen:=Float32ToStr(Buff.Values.StrVal, Buff.Values.StrLen, 6, Buff.Values.FltVal);
  end;
end;


// ****************************
// ***** Setup *****
// ****************************
procedure Setup();
begin
//    RetError := 0; // Error Code
  Serial.Start(57600); // Must be the same as Lazarus code
  SetData(H_Ready, #0, 0, 0, EmptyStr); // Arduino Ready
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
        SetData(H_Error, #0, 0, 0, EmptyStr);
        Serial.WriteBuff(Buff.DataBuffer, StructSize);
        ClearReceiveBuffer();
        Blink(10);
      end;
    end;
    Delay(45 * StructSize);

  end;

end.
