program TestHCSR04;

{
  Test Ultrasonic Sensor HC-SR04

  Ultrasonic sensor Pins:
    VCC: +5VDC
    Trig : Trigger (INPUT) - Pin11
    Echo: Echo (OUTPUT) - Pin 12
    GND: GND

  ported 2025 to Pascal by @ackarwow
  based on sketch by Rui Santos (https://randomnerdtutorials.com)
}

{$IFDEF AVRPascal}
  {$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano))}
    {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
  {$ENDIF}
{$ELSE}
  {$IF NOT (DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
    {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
  {$ENDIF}
{$ENDIF}

{$mode objfpc}

uses
  timer, defs, digital, hardwareserial, strings, pulse, float32;

Const
  TrigPin=11; // Trigger
  EchoPin=12; // Echo

  RawTwo: TRawFloat32 = $40000000;
  Raw_29_1: TRawFloat32 = $41E8CCCD; //29.1

var
   Res: UInt32;
   B: Uint8;
   Buff: array[0..30] of char;
   BuffPtr: PChar;
   duration, cm: TRawFloat32;
begin
  //Define inputs and outputs
  pinMode(trigPin, OUTPUT);
  pinMode(echoPin, INPUT);
  //Serial Port begin
  Serial.Start(9600);

  //Main loop
  while True do
  begin
    // The sensor is triggered by a HIGH pulse of 10 or more microseconds.
    // Give a short LOW pulse beforehand to ensure a clean HIGH pulse:
    digitalWrite(trigPin, LOW);
    delayMicroseconds(5);
    digitalWrite(trigPin, HIGH);
    delayMicroseconds(10);
    digitalWrite(trigPin, LOW);

    // Read the signal from the sensor: a HIGH pulse whose
    // duration is the time (in microseconds) from the sending
    // of the ping to the reception of its echo off of an object.
    Res:=pulseIn(echoPin, HIGH);

    FillChar(Buff, SizeOf(Buff), #0);
    BuffPtr:=UInt32ToStr(Res, Buff,10);
    Serial.Write(' Res: ');
    Serial.Writeln(BuffPtr);

    duration := IntToFloat32(Res);

    // Convert the time [microseconds] into a distance [cm]
    // cm = (duration/2) / 29.1
    cm:=Float32Div(Float32Div(duration, RAWTwo), Raw_29_1);

    FillChar(Buff, SizeOf(Buff), #0);
    b:=Float32ToStr(Buff,30,2,cm);
    if b>0 then
    begin
      Serial.Write(' Cm: ');
      Serial.Writeln(Buff);
    end;

    delay(250);
  end;
end.
