unit analog;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

{
  Based on Arduino library source
  ported to Pascal by Andrzej Karwowski 2021
}

interface

procedure AnalogReference(const aMode: UInt8);
function AnalogRead(aPin: UInt8): integer;
procedure AnalogWrite(const aPin: UInt8; aVal: integer);

implementation

uses
  defs, digital;

const
  DEFAULT = 1;
  EXTERNAL = 0;

var
  analog_reference: UInt8 = DEFAULT; public;

procedure AnalogReference(const aMode: UInt8);
begin
  // can't actually set the register here because the default setting
  // will connect AVCC and the AREF pin, which would cause a short if
  // there's something connected to AREF.
  analog_reference:= aMode;
end;

function AnalogRead(aPin: UInt8): integer;
var
  low, high: UInt8;
begin
  if (aPin >= 14) then Dec(aPin, 14); // allow for channel or pin numbers

  // set the analog reference (high two bits of ADMUX) and select the
  // channel (low 4 bits).  this also sets ADLAR (left-adjust result)
  // to 0 (the default).

  ADMUX:= (analog_reference shl 6) or (aPin and $07);

  // without a delay, we seem to read from the wrong channel
  //delay(1);

  // start the conversion
  Sbi(@ADCSRA, ADSC);

  // ADSC is cleared when the conversion finishes
  while BitIsSet(@ADCSRA, ADSC) do;

  // we have to read ADCL first; doing so locks both ADCL
  // and ADCH until ADCH is read.  reading ADCL second would
  // cause the results of each conversion to be discarded,
  // as ADCL and ADCH would be locked when it completed.
  low:= ADCL;
  high:= ADCH;

  // combine the two bytes
  Result:=(high shl 8) or low;
end;

// Right now, PWM output only works on the pins with
// hardware support.  These are defined in the appropriate
// pins_*.c file.  For the rest of the pins, we default
// to digital output.
procedure AnalogWrite(const aPin: UInt8; aVal: integer);
begin
  // We need to make sure the PWM output is enabled for those pins
  // that support it, as we turn it off when digitally reading or
  // writing with them.  Also, make sure the pin is in output mode
  // for consistenty with Wiring, which doesn't require a pinMode
  // call for the analog output pins.
  PinMode(aPin, OUTPUT);
  case aVal of
  0:   DigitalWrite(aPin, LOW);
  255: DigitalWrite(aPin, HIGH);
  else
    begin
      case DigitalPinToTimer(aPin) of
      TIMER0A:
        begin
      	  // connect pwm to pin on timer 0, channel A
      	  Sbi(@TCCR0A, COM0A1);
      	  OCR0A:= aVal; // set pwm duty
      	end;
      TIMER0B:
        begin
          // connect pwm to pin on timer 0, channel B
          Sbi(@TCCR0A, COM0B1);
          OCR0B:= aVal; // set pwm duty
        end;
      TIMER1A:
        begin
          // connect pwm to pin on timer 1, channel A
          Sbi(@TCCR1A, COM1A1);
          OCR1A:= aVal; // set pwm duty
        end;
      TIMER1B:
        begin
          // connect pwm to pin on timer 1, channel B
          Sbi(@TCCR1A, COM1B1);
          OCR1B:= aVal; // set pwm duty
        end;
      TIMER2A:
      	begin
          // connect pwm to pin on timer 2, channel A
          Sbi(@TCCR2A, COM2A1);
          OCR2A:= aVal; // set pwm duty
        end;
      TIMER2B:
        begin
          // connect pwm to pin on timer 2, channel B
          Sbi(@TCCR2A, COM2B1);
          OCR2B:= aVal; // set pwm duty
        end;
      NOT_ON_TIMER:;
      else
        begin
  	  if (aVal < 128) then
            DigitalWrite(aPin, LOW)
          else
            DigitalWrite(aPin, HIGH);
        end;
      end;
    end;
  end;
end;

end.

