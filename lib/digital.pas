unit digital;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

{
  Based on Arduino library source
  ported to Pascal by Andrzej Karwowski 2021
}

interface

function DigitalPinToBitmask(const aPin: UInt8): UInt8;
function DigitalPinToPort(const aPin: UInt8): UInt8;
function DigitalPinToTimer(const aPin: UInt8): UInt8;
function DigitalPinToPCICR(const aPin: UInt8): PUInt8;
function DigitalPinToPCICRbit(const aPin: UInt8): UInt8;
function DigitalPinToPCMSK(const aPin: UInt8): PUInt8;
function DigitalPinToPCMSKbit(const aPin: UInt8): UInt8;

procedure PinMode(const aPin, aMode: UInt8);

procedure DigitalWrite(const aPin, aVal: UInt8);
function DigitalRead(const aPin: UInt8): integer;

function ShiftIn(dataPin, clockPin, bitOrder: UInt8): UInt8;
procedure ShiftOut(dataPin, clockPin, bitOrder, val: UInt8);

procedure DigitalDebugStart;
procedure DigitalDebugBreak;

implementation

uses
  defs;

//standard/pins_arduino.h
function DigitalPinToBitmask(const aPin: UInt8): UInt8;
const
  aPGM: packed array[0..19] of UINt8=({0, port D}
                              0,1,2,3,4,5,6,7,
                              {8, port B}
                              0,1,2,3,4,5,
                              {14, port C}
                              0,1,2,3,4,5);
begin
  if aPin in [0..19] then
    Result:=1 shl (aPGM[aPin])
  else
    Result:=NOT_A_PIN;
end;

//Arduino.h
function DigitalPinToPort(const aPin: UInt8): UInt8;
const
  aPTP: packed array[0..19] of UInt8=({0}
                              PD,PD,PD,PD,PD,PD,PD,PD,
                              {8}
                              PB,PB,PB,PB,PB,PB,
                              {14}
                              PC,PC,PC,PC,PC,PC);
begin
  if aPin in [0..19] then
    Result:=aPTP[aPin]
  else
    Result:=NOT_A_PIN;
end;

//pins_arduino.h
function DigitalPinToTimer(const aPin: UInt8): UInt8;
const
  aPTT: packed array[0..19] of UInt8=({0 - port D}
                              NOT_ON_TIMER, NOT_ON_TIMER, NOT_ON_TIMER,	TIMER2B, NOT_ON_TIMER, TIMER0B,	TIMER0A, NOT_ON_TIMER,
                              {8 - port B}
                              NOT_ON_TIMER, TIMER1A, TIMER1B, TIMER2A, NOT_ON_TIMER, NOT_ON_TIMER, NOT_ON_TIMER,
                              {14 - port C}
                              NOT_ON_TIMER, NOT_ON_TIMER, NOT_ON_TIMER, NOT_ON_TIMER, NOT_ON_TIMER);
begin
  if aPin in [0..19] then
    Result:=aPTT[aPin]
  else
    Result:=NOT_A_PIN;
end;

function DigitalPinToPCICR(const aPin: UInt8): PUInt8;
begin
  if (aPin >=0) and (aPin<=21) then
    Result:=@PCICR
  else
    Result:=nil;
end;

function DigitalPinToPCICRbit(const aPin: UInt8): UInt8;
begin
  if (aPin<=7) then
    Result:=2
  else if (aPin<=13) then
    Result:=0
  else
    Result:=1;
end;

function DigitalPinToPCMSK(const aPin: UInt8): PUInt8;
begin
  if (aPin<=7) then
    Result:=@PCMSK2
  else if (aPin<= 13) then
    Result:=@PCMSK0
  else if (aPin<= 21) then
    Result:=@PCMSK1
  else
    Result:=nil;
end;

function DigitalPinToPCMSKbit(const aPin: UInt8): UInt8;
begin
  if (aPin<= 7) then
    Result:=aPin
  else if (aPin<= 13) then
    Result:=aPin- 8
  else Result:=aPin-14;
end;

//wiring_digital.c
procedure PinMode(const aPin, aMode: UInt8);
var
  bit, port, oldSREG: UInt8;
  regp, outp: PUInt8;
begin
  bit:=DigitalPinToBitmask(aPin);
  port:=DigitalPinToPort(aPin);

  if (port = NOT_A_PIN) then Exit;

  regp:= PortModeRegister(port);
  outp:= PortOutputRegister(port);

  if (aMode = INPUT) then
  begin
    oldSREG:= SREG;
    Cli;
    regp^:=regp^ and not bit;
    outp^:=outp^ and not bit;
    SREG:= oldSREG;
  end
  else if (aMode = INPUT_PULLUP) then
  begin
    oldSREG:= SREG;
    Cli;
    regp^:=regp^ and not bit;
    outp^:=outp^ or bit;
    SREG:= oldSREG;
  end
  else
  begin
    oldSREG:= SREG;
    Cli;
    regp^:=regp^ or bit;
    SREG:= oldSREG;
  end
end;

//wiring_digital.c
procedure TurnOffPWM(const aTimer: UInt8);
begin
  case aTimer of
    TIMER1A: Cbi(@TCCR1A, COM1A1);
    TIMER1B: Cbi(@TCCR1A, COM1B1);
    TIMER0A: Cbi(@TCCR0A, COM0A1);
    TIMER0B: Cbi(@TCCR0A, COM0B1);
    TIMER2A: Cbi(@TCCR2A, COM2A1);
    TIMER2B: Cbi(@TCCR2A, COM2B1);
  end;
end;

//wiring_digital.c
procedure DigitalWrite(const aPin, aVal: UInt8);
var
  timer, bit, port, oldSREG: UInt8;
  outp: PUInt8;
begin
  timer:= DigitalPinToTimer(aPin);
  bit:= DigitalPinToBitMask(aPin);
  port:= DigitalPinToPort(aPin);

  if (port = NOT_A_PIN) then
    Exit;

  // If the pin that support PWM output, we need to turn it off
  // before doing a digital write.
  if (timer<>NOT_ON_TIMER) then
    TurnOffPWM(timer);

  outp:= PortOutputRegister(port);
  oldSREG:= SREG;
  Cli;

  if (aVal = LOW) then
    outp^:=outp^ and not bit
  else
    outp^:=outp^ or bit;

  SREG:= oldSREG;
end;

//wiring_digital.c
function DigitalRead(const aPin: UInt8): integer;
var
  timer, bit, port: UInt8;
begin
  Result:=LOW;

  timer:= DigitalPinToTimer(aPin);
  bit:= DigitalPinToBitMask(aPin);
  port:= DigitalPinToPort(aPin);

  if (port = NOT_A_PIN) then Exit;

  // If the pin that support PWM output, we need to turn it off
  // before getting a digital reading.
  if (timer <> NOT_ON_TIMER) then
    TurnOffPWM(timer);

  if ((PortInputRegister(port)^ and bit)>0) then
    Result:= HIGH;
end;

function ShiftIn(dataPin, clockPin, bitOrder: UInt8): UInt8;
var
  value, i: UInt8;
begin
  value:= 0;

  for i:= 0 to 7 do
  begin
    DigitalWrite(clockPin, HIGH);
    if (bitOrder = LSBFIRST) then
      value:=value or DigitalRead(dataPin) shl i
    else
      value:=value or DigitalRead(dataPin) shl (7 - i);
    digitalWrite(clockPin, LOW);
  end;
  Result:=value;
end;

procedure ShiftOut(dataPin, clockPin, bitOrder, val: UInt8);
var
  i: Uint8;
begin
  for i:= 0 to 7 do
  begin
    if (bitOrder = LSBFIRST) then
    begin
      DigitalWrite(dataPin, val and 1);
      val:=val shr 1;
    end
    else
    begin
      if (val and 128) > 0 then
        DigitalWrite(dataPin, 1)
      else
        DigitalWrite(dataPin, 0);
      val:=val shl 1;
    end;

    digitalWrite(clockPin, HIGH);
    digitalWrite(clockPin, LOW);
  end;
end;

procedure DigitalDebugStart;
begin
  pinMode(13, OUTPUT);
  DigitalWrite(13, LOW);
end;

procedure DigitalDebugBreak;
begin
  DigitalWrite(13, HIGH);
  while (true) do; //unfinited loop
end;

end.

