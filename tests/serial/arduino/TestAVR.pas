program TestAVR;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano))}
{$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

uses
  defs,
  timer,
  digital,
  hardwareserial;

const
// Led pin
  LedPin = 13; //internal LED
// Handshake, Values for commands, must match with Lazarus Code.
  H_Send = 'S'; // Send
  H_Receive = 'R'; // Receive
  H_Error = 'E'; // Error
  H_Ready = 'K'; // Ready
//  H_Info = 'I'; // Info

type
  ComandValues = (Test1, Test2);  // Values for commands, must match with Lazarus Code.
  Errors = (ErrNone, ErrStartSwitch, ErrEndSwitch);

type
  ComData = packed record
    Command: char; // Header Command
    Value: int16; // Command Value
    Data: int16; // Data
    DataEx: int32; // Data Extended
    Code: int16; // Error Code
    Count: int16; // Count
  end;
// you can add Values here for data, must match with Lazarus Code.

const
  StructSize = SizeOf(ComData);


type
  BufferData = packed record
    case boolean of
      True: (Values: ComData);
      False: (DataBuffer: array[0..StructSize - 1] of byte);
  end;

var
//  RetError: int16 = 0; // Error Code
  Buff: BufferData;

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

  procedure SetData(Command: char; Value: int16; Data: int16; DataEx: int32;
    Code: int16; Count: int16);
  begin
    Buff.Values.Command := Command;
    Buff.Values.Value := Value;
    Buff.Values.Data := Data;
    Buff.Values.DataEx := DataEx;
    Buff.Values.Code := Code;
    Buff.Values.Count := Count;
  end;

  procedure ClearReceiveBuffer();
  begin
    while (Serial.Available <> 0) do Serial.ReadChar;
  end;

  // ****************************
  // ***** Process the Data *****
  // ****************************

  procedure ProcessData();
  begin

    case Buff.Values.Value of
      int16(Test1): // Test 1 Command
        SetData(H_Receive, Buff.Values.Value, Buff.Values.Data + 10,
          Buff.Values.DataEx+10, int16(ErrNone), Buff.Values.Count + 10);

      int16(Test2): // Test 2 Command
        SetData(H_Receive, Buff.Values.Value, Buff.Values.Data + 20,
          Buff.Values.DataEx+20, int16(ErrNone), Buff.Values.Count + 20);
    end;
  end;


  // ****************************
  // ***** Setup *****
  // ****************************
  procedure Setup();
  begin
//    RetError := 0; // Error Code

    Serial.Start(57600); // Must be the same as Lazarus code
    SetData(H_Ready, 0, 0, 0, 0, 0); // Arduino Ready
    Serial.WriteBuff(Buff.DataBuffer, StructSize);
  end;

// { // Uncomment or Comment
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
        SetData(H_Error, 0, 0, 0, 0, 0);
        Serial.WriteBuff(Buff.DataBuffer, StructSize);
        ClearReceiveBuffer();
        Blink(10);
      end;
    end;
    Delay(45 * StructSize);

  end;

end.
// } // Uncomment or Comment

{ // Uncomment or Comment
  // *****************************************************
  // ***** Main Program Alternative Without ReadBuff *****
  // *****************************************************

var
  b: char;
  i: integer;
begin
  Setup();

  while True do
  begin
    if (Serial.Available > 0) then
    begin
      b := Serial.ReadChar;
      if (b = H_Send) then
      begin
        i := 1;
        while (i < StructSize) do
        begin
          if (Serial.Available > 0) then
          begin
            Buff.DataBuffer[i] := byte(Serial.ReadChar);
            Inc(i);
          end;
          delay(10);
        end;

        ProcessData();
        Serial.WriteBuff(Buff.DataBuffer, StructSize);
        Blink(2);
      end
      else
      begin
        SetData(H_Error, 0, 0, 0, 0, 0);
        Serial.WriteBuff(Buff.DataBuffer, StructSize);
        ClearReceiveBuffer();
        Blink(10);
      end;
    end;
    Delay(8 * StructSize);
  end;

end.
} // Uncomment or Comment
