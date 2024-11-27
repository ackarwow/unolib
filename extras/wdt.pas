unit wdt;

{$mode objfpc}

{
  Based on Arduino library source
  ported to Pascal and modified by Andrzej Karwowski 2021
}

interface

const
  WDTO_15MS = 0;
  WDTO_30MS = 1;
  WDTO_60MS = 2;
  WDTO_120MS = 3;
  WDTO_250MS = 4;
  WDTO_500MS = 5;
  WDTO_1S = 6;
  WDTO_2S = 7;
  WDTO_4S = 8;
  WDTO_8S = 9;

procedure wdt_reset;
procedure wdt_disable;
procedure wdt_enable(value: UInt8);

implementation

procedure wdt_reset; assembler;
asm
  wdr
end;

{procedure wdt_disable;
var
  l_sreg: UInt8;
begin
  l_sreg:=SREG;                                    // store SREG state specially for global interrupt enable
  Cli;                                             // turn off interrupts
  wdt_reset;                                       // reset watchdog
  MCUSR:=MCUSR and not WDRF;                       // make sure WDRF is cleared in MCU status reg or else watchdog cannot be turned off
  WDTCSR:=WDTCSR or ((1 shl WDCE) or (1 shl WDE)); // send special value to command register enabling it to be editted
  WDTCSR:= $00;					   // turn off watchdog by clearing WDE & WDIE plus all the rest
  SREG:= l_Sreg;				   // restore SREG. if interrupts were enabled they will be again
end;}

procedure wdt_disable;
const
  cseq = ($0 and (0 shl WDRF));
  oseq = (1 shl WDCE) or (1 shl WDE);
  offseq = (0 shl WDE);
begin
  asm
    cli      // turn off interrupts
    wdr      // reset watchdog
    lds r16, MCUSR  //clear WDRF in MCUSR
    andi r16, cseq
    sts MCUSR, r16
    //write logical one to WDCE and WDE
    //keep old prescaler to prevent unintentional time-out
    lds r16, WDTCSR
    ori r16, oseq
    sts WDTCSR, r16
    ldi r16, offseq //turn off WDT
    sts WDTCSR, r16
    sei // turn on interrupts
  end['r16'];
end;

{procedure wdt_enable(value: UInt8);
begin
  Cli;                                             // turn off interrupts
  wdt_reset;                                       // reset watchdog
  WDTCSR:=WDTCSR or ((1 shl WDCE) or (1 shl WDE)); // send special value to command register enabling it to be editted
  WDTCSR:=UInt8((1 shl WDE) or (value));	          	   // set new prescaler time
  Sei;                                             // turn on interrupts
end;}

procedure wdt_enable(value: UInt8);
// example: 2 sec
//WDIF WDIE WDP3 WDCE WDE WDP2 WDP1 WDP0
//  0    0   0    0    1   1    1    1
//WDTCSR:=(1 shl WDE) or 7; = %00001111;
const
  tseq = ((1 shl WDCE) or (1 shl WDE));
var
  mpresc: UInt8; //mode and prescaller
begin
  mpresc:=UInt8(1 shl WDE) or (value);

  asm
    cli                     // turn off interrupts
    wdr                     // reset watchdog
    lds r16, WDTCSR         // send special value to command register enabling it to be editted
    ori r16, tseq           // WDTCSR:=WDTCSR or tseq
    sts WDTCSR, r16
    ldd r16, mpresc         // set new prescaler time
    sts WDTCSR, r16
    sei                     // turn on interrupts
  end['r16'];

end;

end.

