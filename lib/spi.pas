unit spi;

{$IFDEF AVRPascal}
  {$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano))}
    {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
  {$ENDIF}
{$ELSE}
  {$IF NOT (DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
    {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
  {$ENDIF}
{$ENDIF}

{$MODE objfpc}

{
  Based on Arduino library source.
  Ported to Pascal by Andrzej Karwowski 2026.
}

interface

uses
  defs;

const
  // SPI_HAS_TRANSACTION means SPI has beginTransaction(), endTransaction(),
  // usingInterrupt(), and SPISetting(clock, bitOrder, dataMode)
  SPI_HAS_TRANSACTION = 1;
  // SPI_HAS_NOTUSINGINTERRUPT means that SPI has notUsingInterrupt() method
  SPI_HAS_NOTUSINGINTERRUPT = 1;
  // SPI_ATOMIC_VERSION means that SPI has atomicity fixes and what version.
  // This way when there is a bug fix you can check this define to alert users
  // of your code if it uses better version of this library.
  // This also implies everything that SPI_HAS_TRANSACTION as documented above is
  // available too.
  SPI_ATOMIC_VERSION = 1;
  // Uncomment this line to add detection of mismatched begin/end transactions.
  // A mismatch occurs if other libraries fail to use SPI.endTransaction() for
  // each SPI.beginTransaction().  Connect an LED to this pin.  The LED will turn
  // on if any mismatch is ever detected.
  //{$DEFINE SPI_TRANSACTION_MISMATCH_LED}
  {$IFDEF SPI_TRANSACTION_MISMATCH_LED}
  SPI_TRANSACTION_MISMATCH_LED = 5;
  {$ENDIF}

  LSBFIRST = 0;
  MSBFIRST = 1;

  SPI_CLOCK_DIV4   = $00;
  SPI_CLOCK_DIV16  = $01;
  SPI_CLOCK_DIV64  = $02;
  SPI_CLOCK_DIV128 = $03;
  SPI_CLOCK_DIV2   = $04;
  SPI_CLOCK_DIV8   = $05;
  SPI_CLOCK_DIV32  = $06;

  SPI_MODE0 = $00;
  SPI_MODE1 = $04;
  SPI_MODE2 = $08;
  SPI_MODE3 = $0C;

  SPI_MODE_MASK     = $0C;  // CPOL = bit 3, CPHA = bit 2 on SPCR
  SPI_CLOCK_MASK    = $03;  // SPR1 = bit 1, SPR0 = bit 0 on SPCR
  SPI_2XCLOCK_MASK  = $01;  // SPI2X = bit 0 on SPSR

  SPI_INT0_MASK = (1 shl INT0);
  SPI_INT1_MASK = (1 shl INT1);

  //SPI_AVR_EIMSK = EIMSK;

type
  TSPISettings = object
  public
    constructor Init(clock: UInt32; bitOrder, dataMode: UInt8); overload;
    constructor Init; overload;
  private
    spcr: UInt8;
    spsr: UInt8;
    procedure InitAlwaysInline(clock: UInt32; bitOrder, dataMode: UInt8);
  end;

  TSPI = object
  public
    // Initialize the SPI library
    constructor Init;
    procedure _begin;
    procedure _end;

    // If SPI is used from within an interrupt, this function registers
    // that interrupt with the SPI library, so beginTransaction() can
    // prevent conflicts.  The input interruptNumber is the number used
    // with attachInterrupt.  If SPI is used from a different interrupt
    // (eg, a timer), interruptNumber should be 255.
    procedure UsingInterrupt(interruptNumber: UInt8);
    // And this does the opposite.
    procedure NotUsingInterrupt(interruptNumber: UInt8);
    // Note: the usingInterrupt and notUsingInterrupt functions should
    // not to be called from ISR context or inside a transaction.
    procedure BeginTransaction(settings: TSPISettings);
    // Before using SPI.transfer() or asserting chip select pins,
    // this function is used to gain exclusive access to the SPI bus
    // and configure the correct settings.
    function Transfer(data: UInt8): UInt8;
    function Transfer16(data: UInt16): UInt16;
    procedure Transfer(buf: Pointer; count: SizeUInt);
    // After performing a group of transfers and releasing the chip select
    // signal, this function allows others to access the SPI bus
    procedure EndTransaction; inline;

    // This function is deprecated.  New applications should use
    // beginTransaction() to configure SPI settings.
    procedure setBitOrder(bitOrder: UInt8); inline;
    // This function is deprecated.  New applications should use
    // beginTransaction() to configure SPI settings.
    procedure setDataMode(dataMode: UInt8); inline;
    // This function is deprecated.  New applications should use
    // beginTransaction() to configure SPI settings.
    procedure setClockDivider(clockDiv: UInt8); inline;
    // These undocumented functions should not be used.  SPI.transfer()
    // polls the hardware flag which is automatically cleared as the
    // AVR responds to SPI's interrupt
    procedure attachInterrupt; inline;
    procedure detachInterrupt; inline;
  private
    initialized: UInt8;
    interruptMode: UInt8; // 0=none, 1=mask, 2=global
    interruptMask: UInt8; // which interrupts to mask
    interruptSave: UInt8; // temp storage, to restore state
    {$IFDEF SPI_TRANSACTION_MISMATCH_LED}
    inTransactionFlag: UInt8;
    {$ENDIF}
  end;

var
  _SPI: TSPI;

implementation

uses
  intrinsics, digital;

constructor TSPISettings.Init(clock: UInt32; bitOrder, dataMode: UInt8);
begin
  InitAlwaysInline(clock, bitOrder, dataMode);
end;

constructor TSPISettings.Init;
begin
  InitAlwaysInline(4000000, MSBFIRST, SPI_MODE0)
end;

procedure TSPISettings.InitAlwaysInline(clock: UInt32; bitOrder, dataMode: UInt8); inline;
var
  ClockDiv: UInt8;
begin
  // Clock settings are defined as follows. Note that this shows SPI2X
  // inverted, so the bits form increasing numbers. Also note that
  // fosc/64 appears twice
  // SPR1 SPR0 ~SPI2X Freq
  //   0    0     0   fosc/2
  //   0    0     1   fosc/4
  //   0    1     0   fosc/8
  //   0    1     1   fosc/16
  //   1    0     0   fosc/32
  //   1    0     1   fosc/64
  //   1    1     0   fosc/64
  //   1    1     1   fosc/128

  // We find the fastest clock that is less than or equal to the
  // given clock rate. The clock divider that results in clock_setting
  // is 2 ^^ (clock_div + 1). If nothing is slow enough, we'll use the
  // slowest (128 == 2 ^^ 7, so clock_div = 6).

  if Clock >= (F_CPU div 2) then
    ClockDiv := 0
  else if Clock >= (F_CPU div 4) then
    ClockDiv := 1
  else if Clock >= (F_CPU div 8) then
    ClockDiv := 2
  else if Clock >= (F_CPU div 16) then
    ClockDiv := 3
  else if Clock >= (F_CPU div 32) then
    ClockDiv := 4
  else if Clock >= (F_CPU div 64) then
    ClockDiv := 5
  else
    ClockDiv := 6;

  // Compensate for the duplicate fosc/64
  if ClockDiv = 6 then
    ClockDiv := 7;

  // Invert the SPI2X bit
  ClockDiv := ClockDiv xor $01;

  SPCR :=
    _BV(SPE) or
    _BV(MSTR) or
    (Ord(BitOrder = LSBFIRST) * _BV(DORD)) or
    (DataMode and SPI_MODE_MASK) or
    ((ClockDiv shr 1) and SPI_CLOCK_MASK);

  SPSR := ClockDiv and SPI_2XCLOCK_MASK;
end;

constructor TSPI.Init;
begin
  initialized:= 0;
  interruptMode:= 0;
  interruptMask:= 0;
  interruptSave:= 0;
  {$IFDEF SPI_TRANSACTION_MISMATCH_LED}
  inTransactionFlag:= 0;
  {$ENDIF}
end;

procedure TSPI._begin;
var
  _sreg, port, bit: UInt8;
  regptr: PUInt8;
begin
  _sreg:= SREG;
  avr_cli; // noInterrupts - Protect from a scheduler and prevent transactionBegin
  if (initialized=0) then
  begin
    // Set SS to high so a connected chip will be "deselected" by default
    port:= DigitalPinToPort(SS);
    bit:= digitalPinToBitMask(SS);
    regptr:= PortModeRegister(port);

    // if the SS pin is not already configured as an output
    // then set it high (to enable the internal pull-up resistor)
    if ((regptr^ and bit)=0) then
      DigitalWrite(SS, HIGH);

    // When the SS pin is set as OUTPUT, it can be used as
    // a general purpose output port (it doesn't influence
    // SPI operations).
    pinMode(SS, OUTPUT);

    // Warning: if the SS pin ever becomes a LOW INPUT then SPI
    // automatically switches to Slave, so the data direction of
    // the SS pin MUST be kept as OUTPUT.
    SPCR := SPCR or _BV(MSTR);
    SPCR := SPCR or _BV(SPE);

    // Set direction register for SCK and MOSI pin.
    // MISO pin automatically overrides to INPUT.
    // By doing this AFTER enabling SPI, we avoid accidentally
    // clocking in a single bit since the lines go directly
    // from "input" to SPI control.
    // http://code.google.com/p/arduino/issues/detail?id=888
    pinMode(SCK, OUTPUT);
    pinMode(MOSI, OUTPUT);
  end;

  Inc(initialized); // reference count
  SREG:= _sreg;
end;

procedure TSPI._end;
var
  _sreg: UInt8;
begin
  _sreg:= SREG;
  avr_cli; // noInterrupts() - Protect from a scheduler and prevent transactionBegin
  // Decrease the reference counter
  if (initialized>0) then
    Dec(initialized);
  // If there are no more references disable SPI
  if (initialized = 0) then
  begin
    SPCR:=SPCR and not _BV(SPE); //SPCR &= ~_BV(SPE);
    interruptMode:= 0;
    {$IFDEF SPI_TRANSACTION_MISMATCH_LED}
    inTransactionFlag = 0;
    {$ENDIF}
  end;
  SREG:= _sreg;
end;

procedure TSPI.UsingInterrupt(interruptNumber: UInt8);
var
  mask: UInt8 = 0;
  _sreg: UInt8;
begin
  _sreg:= SREG;
  avr_cli; //noInterrupts(); Protect from a scheduler and prevent transactionBegin
  case interruptNumber of
    0: mask:= SPI_INT0_MASK;
    1: mask:= SPI_INT1_MASK;
    else
      interruptMode:= 2;
  end;

  interruptMask:=interruptMask or mask;
  if (interruptMode = 0) then
    interruptMode:= 1;
  SREG:=_sreg;
end;

procedure TSPI.NotUsingInterrupt(interruptNumber: UInt8);
var
  mask: UInt8 = 0;
  _sreg: UInt8;
begin
  if (interruptMode = 2) then Exit;

  _sreg:= SREG;
  avr_cli; //noInterrupts();

  case interruptNumber of
    0: mask:= (1 shl INT0);   // 0x01
    1: mask:= (1 shl INT1);   // 0x02
  end;

  interruptMask:=interruptMask and not mask;

  if (interruptMask=0) then
    interruptMode:= 0;

  SREG:= _sreg;
end;

procedure TSPI.BeginTransaction(Settings: TSPISettings);
var
  _sreg: UInt8;
begin
  if interruptMode > 0 then
  begin
    _sreg := SREG;
    avr_cli; //noInterrupts();

    if interruptMode = 1 then
    begin
      interruptSave := EIMSK; //<-SPI_AVR_EIMSK
      EIMSK := EIMSK and not interruptMask;
      SREG := _sreg;
    end
    else
      interruptSave := _sreg;
  end;

  {$IFDEF SPI_TRANSACTION_MISMATCH_LED}
  if inTransactionFlag <> 0 then
  begin
    PinMode(SPI_TRANSACTION_MISMATCH_LED, OUTPUT);
    DigitalWrite(SPI_TRANSACTION_MISMATCH_LED, HIGH);
  end;
  inTransactionFlag := 1;
  {$ENDIF}

  SPCR := Settings.spcr;
  SPSR := Settings.spsr;
end;

// Write to the SPI bus (MOSI pin) and also receive (MISO pin)
function TSPI.Transfer(data: UInt8): UInt8; //inline; <-asm not supported in inline
begin
  SPDR:= data;
  {
     The following NOP introduces a small delay that can prevent the wait
     loop form iterating when running at the maximum speed. This gives
     about 10% more speed, even if it seems counter-intuitive. At lower
     speeds it is unnoticed.
  }
   asm
     nop
   end;
   while ((SPSR and _BV(SPIF))=0) do; // wait
   Result:=SPDR;
end;

function TSPI.Transfer16(data: UInt16): UInt16; //inline; <-asm not supported in inline
var
  InLSB, InMSB : UInt8;
  OutLSB, OutMSB : UInt8;
begin
  InLSB := Lo(Data);
  InMSB := Hi(Data);

  if (SPCR and _BV(DORD)) = 0 then
  begin
    // MSB first
    SPDR := InMSB;
    asm  //see Transfer(data: UInt8): UInt8
      nop
    end;
    while ((SPSR and _BV(SPIF))=0) do;
    OutMSB := SPDR;

    SPDR := InLSB;
    asm nop end;
    while ((SPSR and _BV(SPIF))=0) do;
    OutLSB := SPDR;
  end
  else
  begin
    // LSB first
    SPDR := InLSB;
    asm nop end;
    while ((SPSR and _BV(SPIF))=0) do;
    OutLSB := SPDR;

    SPDR := InMSB;
    asm nop end;
    while ((SPSR and _BV(SPIF))=0) do;
    OutMSB := SPDR;
  end;

  Result := (UInt16(OutMSB) shl 8) or OutLSB;
end;

procedure TSPI.Transfer(buf: Pointer; count: SizeUInt);
var
  p: PUInt8;
  OutByte: UInt8;
  InByte: UInt8;
begin
  if Count = 0 then Exit;

  P := buf;
  SPDR := p^;
  Dec(count);

  while count > 0 do
  begin
    OutByte := p[1];

    while ((SPSR and _BV(SPIF))=0) do;

    InByte := SPDR;
    SPDR := OutByte;
    p^ := InByte;
    Inc(p);
    Dec(count);
  end;

  while (SPSR and _BV(SPIF)) = 0 do;

  p^ := SPDR;
end;

procedure TSPI.EndTransaction;
var
  _sreg: UInt8;
begin
  {$IFDEF SPI_TRANSACTION_MISMATCH_LED}
  if inTransactionFlag = 0 then
  begin
    PinMode(SPI_TRANSACTION_MISMATCH_LED, OUTPUT);
    DigitalWrite(SPI_TRANSACTION_MISMATCH_LED, HIGH);
  end;
  inTransactionFlag := 0;
  {$ENDIF}

  if interruptMode > 0 then
  begin
    _sreg := SREG;
    avr_cli; // noInterrupts()
    if interruptMode = 1 then
    begin
      EIMSK := interruptSave; //<- SPI_AVR_EIMSK
      SREG := _sreg;
    end
    else
    begin
      SREG := interruptSave;
    end;
  end;
end;

procedure TSPI.setBitOrder(bitOrder: UInt8);
begin
  if (bitOrder = LSBFIRST) then
    SPCR:=SPCR or _BV(DORD)
  else
    SPCR:=SPCR and not (_BV(DORD));
end;

procedure TSPI.setDataMode(dataMode: UInt8);
begin
  SPCR:= (SPCR and not SPI_MODE_MASK) or dataMode;
end;

procedure TSPI.setClockDivider(clockDiv: UInt8);
begin
  SPCR:= (SPCR and not SPI_CLOCK_MASK) or (clockDiv and SPI_CLOCK_MASK);
  SPSR:= (SPSR and not SPI_2XCLOCK_MASK) or ((clockDiv shr 2) and SPI_2XCLOCK_MASK);
end;

procedure TSPI.attachInterrupt;
begin
  SPCR:= SPCR or _BV(SPIE);
end;

procedure TSPI.detachInterrupt;
begin
  SPCR := SPCR and ($FF xor _BV(SPIE)); // =  SPCR:=SPCR and UInt8(not _BV(SPIE));
end;

initialization

 _SPI.Init;

end.

