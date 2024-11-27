program ccrause_blink_leonardo;

{
  source: https://github.com/ccrause/fpc-avr/blob/master/src/examples/blink1/blink.pp
  USB CDC serial implementation for Arduino Leonardo by A. Karwowski (25.11.2024)
}
uses
  ccrause_delay
  {$if defined(FPC_MCU_ARDUINOLEONARDO)}
  ,cdc, usb
  {$endif}
  ;

const
  {$if defined(FPC_MCU_ATMEGA328P) or defined(FPC_MCU_ATTINY104) or defined(FPC_MCU_ATMEGA8)}
  // Assume Uno or attiny104 Xplained Nano layout
  LEDpin = 1 shl 5;
  {$elseif defined(FPC_MCU_ATMEGA32U4) or defined (FPC_MCU_ARDUINOLEONARDO)}
  // Assume Leonardo layout
  LEDpin = 1 shl 7;
  {$elseif defined(FPC_MCU_ATMEGA2560)}
  // Assume Mega layout
  LEDpin = 1 shl 7;
  {$else}
  LEDpin = 1 shl 2;
  {$endif}
var
{$if defined(FPC_MCU_ATTINY104)}
  LEDport: byte absolute PORTA;
  LEDdir: byte absolute DDRA;
{$elseif defined(FPC_MCU_ATMEGA32U4) or defined(FPC_MCU_ARDUINOLEONARDO)}
  LEDport: byte absolute PORTC;
  LEDdir: byte absolute DDRC;
{$else}
  LEDport: byte absolute PORTB;
  LEDdir: byte absolute DDRB;
{$endif}

procedure blinkOn; inline;
begin
  LEDport := LEDport or LEDpin;
end;

procedure blinkOff; inline;
begin
  LEDport := LEDport and not(LEDpin);
end;

//disable interrputs
procedure Cli; assembler; inline;
asm
  CLI
end;

//enable interrupts
procedure Sei; assembler; inline;
asm
  SEI
end;

begin
  {$if defined(FPC_MCU_ARDUINOLEONARDO)}
  USBDevice.Attach;

  Sei;
  {$endif}

  LEDdir := LEDpin;
  blinkOff;

  while True do
  begin
     blinkOn;
     delay_ms(500);
     blinkOff;
     delay_ms(500);
  end;
end.
