unit tone;

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

{
  Based on Arduino library source
  ported to Pascal 2025 by @ackarwow

  ToneNoInt routine written 2024 by @Myel
}

interface

procedure ToneNoInt(Pin: UInt8; Freq: UInt16; WaveTime: UInt32);

procedure _tone(_pin: UInt8; frequency: UInt16; duration: UInt32 = 0);
procedure noTone(_pin: UInt8);


implementation

uses
  defs, timer, digital;

//@cakarwow: only timer2 is used!
const
  tone_pin_to_timer_PGM: UInt8 = 2; {$ifdef CPUAVR}section '.progmem';{$endif}

var
  tone_pins: UInt8 = 255;

  timer2_toggle_count: Int32;
  timer2_pin_port: PUint8;
  timer2_pin_mask: UInt8;

function GetProgmemByte(const Address: Pointer): UInt8; assembler; nostackframe;
asm
  movw ZL, r24
  lpm r24, Z
end;

procedure ToneNoInt(Pin: UInt8; Freq: UInt16; WaveTime: UInt32);
// Generates a square wave of specified frequency on a pin during a certain time
// It is not "interrupt" based.

// Inputs:
// Pin: pin
// Freq: wave frequency
// WaveTime: wave time
var
 EndWave: UInt32;
 WaveHalfPeriod: UInt16; // Half period of wave in microseconds
begin
 EndWave := Micros() + WaveTime;
 WaveHalfPeriod := 500000 Div Freq;
 while (Micros() < EndWave) do
 begin
  DigitalWrite(Pin, HIGH);
  DelayMicroseconds(WaveHalfPeriod);
  DigitalWrite(Pin, LOW);
  DelayMicroseconds(WaveHalfPeriod);
 end;
end;

function toneBegin(_pin:UInt8): Int8;
var
  _timer: Int8 = -1;
begin
  // if we're already using the pin, the timer should be configured.
  if tone_pins = _pin then
    Exit(GetProgMemByte(@tone_pin_to_timer_PGM));

  // search for an unused timer.
  if (tone_pins = 255) then
  begin
    tone_pins:= _pin;
    _timer:= GetProgMemByte(@tone_pin_to_timer_PGM);
  end;

  if (_timer=2) then
  begin
    // Set timer specific stuff
    // All timers in CTC mode
    // 8 bit timers will require changing prescalar values,
    // whereas 16 bit timers are set to either ck/1 or ck/64 prescalar
    // 8 bit timer
    TCCR2A:= 0;
    TCCR2B:= 0;
    bitWrite(TCCR2A, WGM21, 1);
    bitWrite(TCCR2B, CS20, 1);
    timer2_pin_port:= portOutputRegister(digitalPinToPort(_pin));
    timer2_pin_mask:= digitalPinToBitMask(_pin);
  end;

  Result:=_timer;
end;

// frequency (in hertz) and duration (in milliseconds).
procedure _tone(_pin: UInt8; frequency: UInt16; duration: UInt32 = 0);
var
  prescalarbits: UInt8 = %001;
  toggle_count: Int32 = 0;
  ocr: UInt32 = 0;
  _timer: Int8;
begin
  _timer:= toneBegin(_pin);

  if (_timer = 2) then
  begin
    if frequency = 0 then frequency:= 1; //by @ackarwow, otherwise division by zero and crash

    // Set the pinMode as OUTPUT
    pinMode(_pin, OUTPUT);

    // if we are using an 8 bit timer, scan through prescalars to find the best fit
    ocr:= F_CPU div frequency div 2 - 1;
    prescalarbits:= %001;  // ck/1: same for both timers
    if (ocr > 255) then
    begin
      ocr:= F_CPU div frequency div 2 div 8 - 1;
      prescalarbits:= %010;  // ck/8: same for both timers

      if (ocr > 255) then
      begin
        ocr:= F_CPU div frequency div 2 div 32 - 1;
        prescalarbits:= %011;
      end;

      if (ocr > 255) then
      begin
        ocr:= F_CPU div frequency div 2 div 64 - 1;
        prescalarbits:=%100;

        if (ocr > 255) then
        begin
          ocr:= F_CPU div frequency div 2 div 128 - 1;
          prescalarbits:= %101;
        end;

        if (ocr > 255) then
        begin
          ocr:= F_CPU div frequency div 2 div 256 - 1;
          prescalarbits:=%110;
          if (ocr > 255) then
          begin
            // can't do any better than /1024
            ocr:= F_CPU div frequency div 2 div 1024 - 1;
            prescalarbits:=%111;
          end;
        end;
      end;
    end;

    TCCR2B:= (TCCR2B and %11111000) or prescalarbits;

    // Calculate the toggle count
    if (duration > 0) then
      toggle_count:= 2 * frequency * duration div 1000
    else
      toggle_count:= -1;

    // Set the OCR for the given timer,
    // set the toggle count,
    // then turn on the interrupts
    OCR2A:= ocr;
    timer2_toggle_count:= toggle_count;
    bitWrite(TIMSK2, OCIE2A, 1);
  end;
end;

// this function only works properly for timer 2 (the only one we use
// currently).  for the others, it should end the tone, but won't restore
// proper PWM functionality for the timer.
procedure disableTimer(_timer: UInt8);
begin
  if _timer=2 then
  begin
  //#if defined(TIMSK2) && defined(OCIE2A)
    bitWrite(TIMSK2, OCIE2A, 0); // disable interrupt
  //#if defined(TCCR2A) && defined(WGM20)
    TCCR2A:= (1 shl WGM20);
  //#if defined(TCCR2B) && defined(CS22)
    TCCR2B:= (TCCR2B and %11111000) or (1 shl CS22);
  //#if defined(OCR2A)
    OCR2A:= 0;
  end;
end;

procedure noTone(_pin: UInt8);
var
  _timer: Int8 = -1;
begin
  if (tone_pins = _pin) then
  begin
    _timer:= GetProgMemByte(@tone_pin_to_timer_PGM);
    tone_pins:= 255;
  end;

  disableTimer(_timer);
  digitalWrite(_pin, 0);
end;

procedure TIMER2_COMPA_Vect; alias: 'TIMER2_COMPA_ISR'; interrupt; public;
begin
  if (timer2_toggle_count <> 0) then
  begin
    // toggle the pin
    timer2_pin_port^:= timer2_pin_port^ xor timer2_pin_mask;

    if (timer2_toggle_count > 0) then
      Dec(timer2_toggle_count);
  end
  else
  begin
    // need to call noTone() so that the tone_pins[] entry is reset, so the
    // timer gets initialized next time we call tone().
    // XXX: this assumes timer 2 is always the first one used.
    noTone(tone_pins);
//    disableTimer(2);
//    *timer2_pin_port &= ~(timer2_pin_mask);  // keep pin low after stop
  end;
end;

end.
