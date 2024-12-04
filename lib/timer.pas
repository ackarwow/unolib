unit timer;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

{$goto on}

{
  Based on Arduino library source
  ported to Pascal by Andrzej Karwowski 2021
  
  - modified 13 Oct 2024 by Andrzej Karwowski

  - modified 3 Dec 2024 by Andrzej Karwowski (inlined routines: ClockCyclesToMicroseconds, MicrosecondsToClockCycles)
}

interface

uses
  defs;

const
  clockCyclesPerMicrosecond: UInt32 = F_CPU div 1000000; {=16}

var
  MICROSECONDS_PER_TIMER0_OVERFLOW: UInt16;
  MILLIS_INC: UInt8;
  FRACT_INC:UInt8;
  FRACT_MAX:UInt8;

  timer0_millis: UInt32 = 0; public;//volatile()
  timer0_overflow_count: UInt32 = 0; public; //volatile()
  timer0_fract: UInt8 = 0; public;//volatile()

function ClockCyclesToMicroseconds(aCnt: UInt32): UInt32; inline;
function MicrosecondsToClockCycles(aCnt: UInt32): UInt32; inline;

function Micros: UInt32;
function Millis: UInt32;

procedure Delay(ms: UInt32);
procedure Delay2(const ms: Uint16);
//procedure DelayShortMicroseconds(const us: UInt8);
procedure DelayMicroseconds(const us: UInt16);

implementation

type
  TUInt16Rec = packed record
    LoByte, HiByte: Byte;
  end;

  TUInt32Rec = packed record
    LoWord, HiWord: TUInt16Rec;
  end;

procedure Delay2(const ms: Uint16);
const
  FCPU_div_1000:UInt32 = F_CPU div 1000;
var
  c1: Uint32;
label
  loop;
begin
  asm
    NOP
    NOP
  end;
  //3 cycles setup, 6 cycles loop
  c1:= ((ms*FCPU_div_1000) div 6)-3;  //unknown cycles count, kilkaset

  asm
    LDD r18, TUInt32Rec(c1).LoWord.LoByte //ll   //1 cycle
    LDD r24, TUInt32Rec(c1).LoWord.HiByte //lh   //1 cycle
    LDD r25, TUInt32Rec(c1).HiWord.LoByte //hl   //1 cycle
  loop:           //6 cycles
    NOP           //1 cycle
    SUBI r18, 1   //1 cycle
    SBCI r24, 0   //1 cycle
    SBCI r25, 0   //1 cycle
    BRNE loop     //2 cycles where jumping, 1 otherwise
  end['r18','r24','r25'];
end;

function ClockCyclesToMicroseconds(aCnt: UInt32): UInt32; inline;
begin
  Result:=aCnt div clockCyclesPerMicrosecond{16};
end;

function MicrosecondsToClockCycles(aCnt: UInt32): UInt32; inline;
begin
  Result:=(aCnt * clockCyclesPerMicrosecond);
end;

function Millis: UInt32;
var
  m: UInt32;
  oldSREG: UInt8;
begin
  oldSREG:= SREG;

  // disable interrupts while we read timer0_millis or we might get an
  // inconsistent value (e.g. in the middle of a write to timer0_millis)
  Cli;
  m:= timer0_millis;
  SREG:= oldSREG;

  Result:=m;
end;

function Micros: UInt32;
var
  m: UInt32;
  t, oldSREG: UInt8;
begin
  oldSREG:= SREG;

  Cli;
  m:= timer0_overflow_count;
  t:= TCNT0;

  if ((TIFR0 and _BV(TOV0)>0) and (t < 255)) then
    Inc(m);

  SREG:= oldSREG;

  Result:=((m shl 8) + t) * (64 div clockCyclesPerMicrosecond);
end;

procedure Delay(ms: UInt32);
var
  start: UInt32;
begin
  start:= micros;

  while (ms > 0) do
  begin
//    Yield;
    while ((ms > 0) and ((micros - start) >= 1000)) do
    begin
      Dec(ms);
      Inc(start, 1000);
    end;
  end;
end;

(*
procedure DelayMicroseconds(us: UInt16);
label
  loop;
begin
  // call = 4 cycles + 2 to 4 cycles to init us(2 for constant delay, 4 for variable)

  // calling avrlib's delay_us() function with low values (e.g. 1 or
  // 2 microseconds) gives delays longer than desired.
  //delay_us(us);

  // for the 16 MHz clock on most Arduino boards

  // for a one-microsecond delay, simply return.  the overhead
  // of the function call takes 14 (16) cycles, which is 1us
  if (us <= 1) then Exit; //  = 3 cycles, (4 when true)

  // the following loop takes 1/4 of a microsecond (4 cycles)
  // per iteration, so execute it four times for each microsecond of
  // delay requested.
  us:=us shl 2; // x4 us, = 4 cycles

  // account for the time taken in the preceeding commands.
  // we just burned 19 (21) cycles above, remove 5, (5*4=20)
  // us is at least 8 so we can substract 5
  Dec(us, 5); // = 2 cycles,

  // busy wait
  {__asm__ __volatile__ (
	  "1: sbiw %0,1" "\n\t" // 2 cycles
	  "brne 1b" : "=w" (us) : "0" (us) // 2 cycles
  );}
  asm //TO VERIFY!!!
    ldd r27, TUInt16Rec(us).HiByte
    ldd r26, TUInt16Rec(us).LoByte

    loop:
    sbiw r26, 1 // 2 cycyles
    brne loop
  end['r27','r26'];
  // return = 4 cycles
end;
*)

{procedure DelayMicroseconds(const us: Uint16);
const
  tickFreq:UInt32 = (F_CPU div 1000000);
var
  c1: Uint32;
label
  loop;
begin
  asm
    NOP
    NOP
  end;
  //3 cycles setup, 6 cycles loop
  c1:= ((us*tickFreq) div 6)-3;  //unknown cycles count, kilkaset

  asm
    LDD r18, TUInt32Rec(c1).LoWord.LoByte //ll   //1 cycle
    LDD r24, TUInt32Rec(c1).LoWord.HiByte //lh   //1 cycle
    LDD r25, TUInt32Rec(c1).HiWord.LoByte //hl   //1 cycle
  loop:           //6 cycles
    NOP           //1 cycle
    SUBI r18, 1   //1 cycle
    SBCI r24, 0   //1 cycle
    SBCI r25, 0   //1 cycle
    BRNE loop     //2 cycles where jumping, 1 otherwise
  end['r18','r24','r25'];
end;}

//16 clock cycles per microsecond
procedure DelayShortMicroseconds(const us: UInt8);
var
  ml: UInt8;
label
  loop;
begin
  ml:=us * 4;
  asm
    ldd r16, ml
    loop: //4 clock cycles
      nop
      dec r16
      brne loop
  end ['r16'];
end;

{routine afer: https://github.com/ccrause/fpc-avr}
{us should not be greater than 65535 - max value of WORD}
procedure DelayMicroseconds(const us: UInt16); assembler;
const
  tickFreq = (F_CPU div 1000000); // only valid from 1 - 64 MHz clock
  shortlimit = 26;                // If delay ticks less than this, exit
  overheadloop = 36;              // fixed overhead besides loop
label
  loop, finish;
asm
  // Calculate required # ticks
  // tickfreq * us; uint8 * uitn16
  // us passed in R25:R24
  // put ticks in R26:R25:R24
  ldi R22, tickFreq
  mul R24, R22
  mov R24, R0
  mov R23, R1    // temp R

  clr R26
  mul R25, R22
  mov R25, R0
  add R25, R23   // add temp from previous mul
  adc R26, R1

  // restore R1 to zero
  clr R1

  // test if ticks required < overhead, jump to finish if true
  cpi R24, shortlimit
  cpc R25, R1
  cpc R26, R1
  brlo finish    // 2 cycles to branch, 1 to continue

  // Calculate loop counter
  // First substract overhead
  sbiw R24, overheadloop          // 2 cycles
  sbc R26, R1

  // Loop count = tickcount / 4
  // http://www.avrfreaks.net/comment/150853#comment-150853
  lsr R26
  ror R25
  ror R24
  lsr R26
  ror R25
  ror R24

  // loop counter correction for outer loop iterations
  // subtract 3*(loopcounter shr 16) = sub 3*R26 from loopcounter
  ldi R22, 3
  mul R26, R22
  sub R24, R0
  sbc R25, R1
  sbci R26, 0
  clr R1

loop:
  sbiw R24, 1      // 2 cycles
  brcc loop        // 2 cycles to branch, 1 to continue
  subi R26, 1
  brcc loop        // 2 cycles to branch, 1 to continue
finish:
end;

procedure TIMER0_OVF_Vect; Alias: 'TIMER0_OVF_ISR'; Interrupt; Public;
var
  m: UInt32;
  f: UInt8;
begin
  // copy these to local variables so they can be stored in registers
  // (volatile variables must be read from memory on every access)
  m:= timer0_millis;
  f:= timer0_fract;

  Inc(m, MILLIS_INC);
  Inc(f, FRACT_INC);
  if (f >= FRACT_MAX) then
  begin
    Dec(f, FRACT_MAX);
    Inc(m, 1);
  end;

  timer0_fract:= f;
  timer0_millis:= m;
  Inc(timer0_overflow_count);
end;

procedure InitATMega328P;
begin
  // this needs to be called before setup() or some functions won't
  // work there
  Sei;

  // on the ATmega168, timer 0 is also used for fast hardware pwm
  // (using phase-correct PWM would mean that timer 0 overflowed half as often
  // resulting in different millis() behavior on the ATmega8 and ATmega168)
  Sbi(@TCCR0A, WGM01);
  Sbi(@TCCR0A, WGM00);

  // this combination is for the standard 168/328/1280/2560
  Sbi(@TCCR0B, CS01);
  Sbi(@TCCR0B, CS00);

  // enable timer 0 overflow interrupt
  Sbi(@TIMSK0, TOIE0);

  // timers 1 and 2 are used for phase-correct hardware pwm
  // this is better for motors as it ensures an even waveform
  // note, however, that fast pwm mode can achieve a frequency of up
  // 8 MHz (with a 16 MHz clock) at 50% duty cycle

  TCCR1B:= 0;
  // set timer 1 prescale factor to 64
  Sbi(@TCCR1B, CS11);
  Sbi(@TCCR1B, CS10);

  // put timer 1 in 8-bit phase correct pwm mode
  Sbi(@TCCR1A, WGM10);

  // set timer 2 prescale factor to 64
  Sbi(@TCCR2B, CS22);

  // configure timer 2 for phase correct pwm (8-bit)
  Sbi(@TCCR2A, WGM20);

  // set a2d prescaler so we are inside the desired 50-200 KHz range.
  Sbi(@ADCSRA, ADPS2);
  Sbi(@ADCSRA, ADPS1);
  Sbi(@ADCSRA, ADPS0);
  // enable a2d conversions
  Sbi(@ADCSRA, ADEN);

  // the bootloader connects pins 0 and 1 to the USART; disconnect them
  // here so they can be used as normal digital i/o; they will be
  // reconnected in Serial.begin()
  UCSR0B:= 0;
end;

initialization

MICROSECONDS_PER_TIMER0_OVERFLOW:= clockCyclesToMicroseconds(64 * 256);
MILLIS_INC:=UInt8(MICROSECONDS_PER_TIMER0_OVERFLOW div 1000); //16
FRACT_INC:=((MICROSECONDS_PER_TIMER0_OVERFLOW mod 1000) shr 3); //48
FRACT_MAX:=1000 shr 3; //125

InitATMega328P;

end.

