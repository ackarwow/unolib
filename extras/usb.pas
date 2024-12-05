unit usb;

{
  Based on Arduino library source
  ported to Pascal by Andrzej Karwowski 2024

  v08 (05.12.2024)
  - added TODO: support for PluggableUSB
}

{$mode objfpc}

interface

const
  MAGIC_KEY  = $7777;
  MAGIC_KEY_POS =  $0800;
  NEW_LUFA_SIGNATURE = $DCFB;

  USB_EP_SIZE = 64;

  REQUEST_HOSTTODEVICE = $00;
  REQUEST_DEVICETOHOST = $80;
  REQUEST_CLASS = $20;
  REQUEST_INTERFACE = $01;
  REQUEST_STANDARD = $00;
  REQUEST_TYPE = $60;
  REQUEST_DEVICE = $00;
  REQUEST_ENDPOINT = $02;
  REQUEST_OTHER	= $03;
  REQUEST_RECIPIENT = $03;
  //standard requests
  GET_STATUS = 0;
  CLEAR_FEATURE	= 1;
  SET_FEATURE = 3;
  SET_ADDRESS = 5;
  GET_DESCRIPTOR = 6;
  SET_DESCRIPTOR = 7;
  GET_CONFIGURATION = 8;
  SET_CONFIGURATION = 9;
  GET_INTERFACE	= 10;
  SET_INTERFACE	= 11;

  REQUEST_DEVICETOHOST_CLASS_INTERFACE = REQUEST_DEVICETOHOST or REQUEST_CLASS or REQUEST_INTERFACE;
  REQUEST_HOSTTODEVICE_CLASS_INTERFACE = REQUEST_HOSTTODEVICE or REQUEST_CLASS or REQUEST_INTERFACE;
  REQUEST_DEVICETOHOST_STANDARD_INTERFACE = REQUEST_DEVICETOHOST or REQUEST_STANDARD or REQUEST_INTERFACE;

  TX_RX_LED_PULSE_MS = 100;

  CDC_FIRST_ENDPOINT = 1;
  CDC_ENDPOINT_ACM = CDC_FIRST_ENDPOINT; // CDC First
  CDC_ENDPOINT_OUT = CDC_FIRST_ENDPOINT+1;
  CDC_ENDPOINT_IN = CDC_FIRST_ENDPOINT+2;
  CDC_RX = CDC_ENDPOINT_OUT;
  CDC_TX = CDC_ENDPOINT_IN;

  // usb_20.pdf Table 9.6 Standard Feature Selectors
  DEVICE_REMOTE_WAKEUP = 1;
  ENDPOINT_HALT = 2;
  TEST_MODE = 3;

  // usb_20.pdf Figure 9-4. Information Returned by a GetStatus() Request to a Device
  FEATURE_SELFPOWERED_ENABLED = (1 shl 0);
  FEATURE_REMOTE_WAKEUP_ENABLED = (1 shl 1);

  USB_DEVICE_DESCRIPTOR_TYPE =            1;
  USB_CONFIGURATION_DESCRIPTOR_TYPE =     2;
  USB_STRING_DESCRIPTOR_TYPE =            3;
  USB_INTERFACE_DESCRIPTOR_TYPE =         4;
  USB_ENDPOINT_DESCRIPTOR_TYPE =          5;

  USB_CONFIG_POWERED_MASK =               $40;
  USB_CONFIG_BUS_POWERED =                $80;
  USB_CONFIG_SELF_POWERED =               $C0;
  USB_CONFIG_REMOTE_WAKEUP =              $20;

  USB_CONFIG_POWER = 500;

  IMANUFACTURER = 1;
  IPRODUCT      = 2;
  ISERIAL       = 3;

  //USB_PRODUCT:PChar = 'USB IO Board';
  //USB_MANUFACTURER:PChar = 'Arduino LLC';

  ISERIAL_MAX_LEN = 20;

type
  //USB
  PUSBSetup = ^TUSBSetup;
  TUSBSetup = packed record
    bmRequestType: UInt8;
    bRequest: UInt8;
    wValueL: UInt8;
    wValueH: UInt8;
    wIndex: UInt16;
    wLength: UInt16;
  end;

  //Config
  TConfigDescriptor=packed record
    len: UInt8;	// 9
    dtype: UInt8; // 2
    clen: UInt16; // total length
    numInterfaces: UInt8;
    config: UInt8;
    iconfig: UInt8;
    attributes: UInt8;
    maxPower: UInt8;
  end;

  TUSBDevice = object
  public
    //constructor USBDevice_();
    function Configured: boolean;
    procedure Attach;
    procedure Detach; // Serial port goes down too...
    procedure Poll;
    function WakeupHost: boolean; // returns false, when wakeup cannot be processed
    function IsSuspended: boolean;
  end;

function USB_SendControl(flags: UInt8; const d: PUInt8; len: Int16): Int16;
function USB_RecvControl(d: PUint8; len: Int16): Int16;
function USB_SendSpace(ep: UInt8): UInt8;
function USB_Send(ep: UInt8; const d: PUint8; len: Int16): Int16;
function USB_Available(ep: UInt8): UInt8;
function USB_Recv(ep: UInt8): Int16;
procedure USB_Flush(ep: UInt8);

var
  USBDevice: TUSBDevice;

implementation

uses
  ccrause_delay, cdc;

const
  EP_SINGLE_64 = $32; // EP0
  EP_DOUBLE_64 = $36; // Other endpoints
  EP_SINGLE_16 = $12;

  {$ifdef FPC_MCU_ARDUINOLEONARDO}
  EPDIR	= 0;
  EPTYPE0 = 6;
  EPTYPE1 = 7;

  USB_ENDPOINTS = 7; // AtMegaxxU4
  {$ENDIF}

  EP_TYPE_CONTROL = $00;
  EP_TYPE_BULK_IN = ((1 shl EPTYPE1) or (1 shl EPDIR));
  EP_TYPE_BULK_OUT = (1 shl EPTYPE1);
  EP_TYPE_INTERRUPT_IN = ((1 shl EPTYPE1) or (1 shl EPTYPE0) or (1 shl EPDIR));
  EP_TYPE_INTERRUPT_OUT = ((1 shl EPTYPE1) or (1 shl EPTYPE0));
  EP_TYPE_ISOCHRONOUS_IN = ((1 shl EPTYPE0) or (1 shl EPDIR));
  EP_TYPE_ISOCHRONOUS_OUT = (1 shl EPTYPE0);

  _initEndpoints: array[0..USB_ENDPOINTS-1] of UInt8 =
    (
	0,                      // Control Endpoint
	EP_TYPE_INTERRUPT_IN,   // CDC_ENDPOINT_ACM
	EP_TYPE_BULK_OUT,       // CDC_ENDPOINT_OUT
	EP_TYPE_BULK_IN,        // CDC_ENDPOINT_IN
	// Following endpoints are automatically initialized to 0
        0,
        0,
        0
     );

type
  TLockEP = object
    _sreg: UInt8;
  public
    procedure LockEP(ep: UInt8);
    procedure UnlockEP;
  end;

var
  _cmark: Int16;
  _cend: Int16;

  RxLEDPulse: UInt8; // Milliseconds remaining for data Rx LED pulse
  TxLEDPulse: UInt8; // Milliseconds remaining for data Tx LED pulse

  _usbConfiguration: UInt8 = 0;
  _usbCurrentStatus: UInt8 = 0; // meaning of bits see usb_20.pdf, Figure 9-4.
  _usbSuspendState: UInt8 = 0; // copy of UDINT to check SUSPI and WAKEUPI bits

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

procedure InitEP(index, _type, size: UInt8); inline;
begin
  UENUM:= index;
  UECONX:= (1 shl EPEN);
  UECFG0X:= _type;
  UECFG1X:= size;
end;

procedure SetEP(ep: UInt8); inline;
begin
  UENUM:= ep;
end;

procedure TLockEP.LockEP(ep: UInt8);
begin
  _sreg:=SREG;
  cli;
  SetEP(ep and 7);
end;

procedure TLockEP.UnlockEP;
begin
  SREG:=_sreg;
end;

procedure Stall; inline;
begin
  UECONX:= (1 shl STALLRQ) or (1 shl EPEN);
end;

function ReadWriteAllowed: UInt8; inline;
begin
  Result:=UEINTX and (1 shl RWAL);
end;

function FifoByteCount: UInt8; inline;
begin
  Result:=UEBCLX;
end;

function ReceivedSetupInt: UInt8; inline;
begin
  Result:= UEINTX and (1 shl RXSTPI);
end;

procedure ClearSetupInt; inline;
begin
  UEINTX:= not ((1 shl RXSTPI) or (1 shl RXOUTI) or (1 shl TXINI));
end;

function WaitForINOrOUT: boolean; inline;
begin
  while ((UEINTX and ((1 shl TXINI) or (1 shl RXOUTI)))=0) do;
  Result:=(UEINTX and (1 shl RXOUTI)) = 0;
end;

procedure WaitIN; inline;
begin
  while ((UEINTX and (1shl TXINI))=0) do
    ;
end;

procedure ClearIN; inline;
begin
  UEINTX:= not (1 shl TXINI);
end;

procedure ClearOUT; inline;
begin
  UEINTX:= not (1 shl RXOUTI);
end;

procedure WaitOUT; inline;
begin
  while ((UEINTX and (1 shl RXOUTI))=0) do;
end;

procedure Send8(d: UInt8); inline;
begin
  UEDATX:= d;
end;

{$ifdef FPC_MCU_ARDUINOLEONARDO}

{#define TX_RX_LED_INIT	DDRD |= (1<<5), DDRB |= (1<<0)
#define TXLED0			PORTD |= (1<<5)
#define TXLED1			PORTD &= ~(1<<5)
#define RXLED0			PORTB |= (1<<0)
#define RXLED1			PORTB &= ~(1<<0)}

procedure TX_RX_LED_INIT; inline;
begin
  DDRD:=DDRD or (1 shl 5);
  DDRB:=DDRB or (1 shl 0);
end;

procedure RXLED0; inline;
begin
  PORTB:=PORTB or (1 shl 0);
end;

procedure RXLED1; inline;
begin
  PORTB:=PORTB and not (1 shl 0);
end;

procedure TXLED0; inline;
begin
  PORTD:=PORTD or (1 shl 5);
end;

procedure TXLED1; inline;
begin
  PORTD:=PORTD and not (1 shl 5);
end;
{$endif}

procedure Recv(data: PUInt8; count: UInt8); inline;
begin
  while (count>0) do
  begin
    data^:= UEDATX;
    Inc(data);
    Dec(count);
  end;

  RXLED1;					// light the RX LED
  RxLEDPulse:= TX_RX_LED_PULSE_MS;
end;

function Recv8: UInt8; inline;
begin
  RXLED1;					// light the RX LED
  RxLEDPulse:= TX_RX_LED_PULSE_MS;
  Result:=UEDATX;
end;

procedure ReleaseRX; inline;
begin
  UEINTX:= $6B;	// FIFOCON=0 NAKINI=1 RWAL=1 NAKOUTI=0 RXSTPI=1 RXOUTI=0 STALLEDI=1 TXINI=1
end;

procedure ReleaseTX; inline;
begin
  UEINTX:= $3A;	// FIFOCON=0 NAKINI=0 RWAL=1 NAKOUTI=1 RXSTPI=1 RXOUTI=0 STALLEDI=1 TXINI=0
end;

procedure InitControl(_end: Int16);
begin
  SetEP(0);
  _cmark:= 0;
  _cend:= _end;
end;

{#######}
function SendControl(d: Uint8): boolean;
begin
  Result:=false;
  if (_cmark < _cend) then
  begin
    if ( not WaitForINOrOUT) then Exit;
    Send8(d);
    if (((_cmark + 1) and $3F)=0) then
      ClearIN;	// Fifo is full, release this packet
  end;
  Inc(_cmark);
  Result:=true;
end;

procedure InitEndpoints;
var
  i: UInt8;
begin
  for i:= 1 to sizeof(_initEndpoints)-1 do
  begin
    if _initEndpoints[i] <> 0 then
    begin
      UENUM:= i;
      UECONX:= (1 shl EPEN);
      UECFG0X:= _initEndpoints[i];
  //#if USB_EP_SIZE == 16
  //		UECFG1X = EP_SINGLE_16;
  //#elif USB_EP_SIZE == 64
      UECFG1X:= EP_DOUBLE_64;
  //#else
  //#error Unsupported value for USB_EP_SIZE
  //#endif
    end;
  end;

  UERST:= $7E;	// And reset them
  UERST:= 0;
end;

//Handle CLASS_INTERFACE requests
function ClassInterfaceRequest(setup: TUSBSetup): boolean;
var
  i: UInt8;
begin
  i:= setup.wIndex;

  if (i = CDC_ACM_INTERFACE) then
    Exit(CDC_Setup(setup));

  {TODO: support for PluggableUSB}
//#ifdef PLUGGABLE_USB_ENABLED
//	return PluggableUSB().setup(setup);
//#endif
  Result:=false;
end;

{#######}
//Space in send EP
function USB_SendSpace(ep: UInt8): UInt8;
var
  LockEP: TLockEP;
begin
  LockEP.LockEP(ep); //LockEP lock(ep);      ??????????????
  if (not ReadWriteAllowed>0) then
    Result:=0
  else
    Result:=USB_EP_SIZE - FifoByteCount;
  LockEP.UnlockEP;
end;

//	Blocking Send of data to an endpoint
function USB_Send(ep: UInt8; const d: PUint8; len: Int16): Int16;
var
  r: Int16;
  data: PUInt8;
  timeout, n: UInt8;
  sendZlp: boolean;
  LockEP: TLockEP;
begin
  if (_usbConfiguration=0) then
    Exit(-1);

  if (_usbSuspendState>0) and ((1 shl SUSPI)>0) then
    UDCON:=UDCON or (1 shl RMWKUP); //send a remote wakeup

  r:=len;
  data:=d;
  timeout:= 250;		// 250ms timeout on send? TODO
  sendZlp:= false;

  while (len>0) or (sendZlp=true) do
  begin
    n:=USB_SendSpace(ep);
    if (n = 0) then
    begin
      Dec(timeout);
      if (timeout=0) then Exit(-1);
      delay_ms(1); //delay(1)
      Continue;
    end;

    if (n > len) then
      n:= len;

    // Frame may have been released by the SOF interrupt handler
    if (ReadWriteAllowed=0) then
      Continue;

    len:=len-n;

    LockEP.LockEP(ep); //LockEP lock(ep);      ??????????????
    while (n>0) do
    begin
      Send8(data^);
      Inc(data);
      Dec(n);
    end;
    LockEP.UnlockEP;

    if (sendZlp) then
    begin
      ReleaseTX;
      sendZlp:= false;
    end
    else if (ReadWriteAllowed=0) then // ...release if buffer is full...
    begin
      ReleaseTX;
      if (len = 0) then
        sendZlp:= true;
    end;
  end;
  TXLED1; // light the TX LED
  TxLEDPulse:= TX_RX_LED_PULSE_MS;
  Result:=r;
end;

{#######}
// Send a USB descriptor string. The string is stored in PROGMEM as a
// plain ASCII string but is sent out as UTF-16 with the correct 2-byte
// prefix
function USB_SendStringDescriptor(const string_P: PChar; string_len, flags: UInt8): boolean;
var
  i: UInt8;
  r: boolean;
begin
  SendControl(2 + string_len * 2);
  SendControl(3);

  //bool pgm = flags & TRANSFER_PGM;
  for i:= 0 to string_len-1 do
  begin
    //bool r = SendControl(pgm ? pgm_read_byte(&string_P[i]) : string_P[i]);
    r:= SendControl(Ord(string_P[i]));
    r:=r and SendControl(0); // high byte
    if not r then Exit(false);
  end;
  Exit(true);
end;

{#######}
function USB_SendControl(flags: UInt8; const d: PUInt8; len: Int16): Int16;
var
  sent: Int16;
  data: PUInt8;
  c: UInt8;
begin
  sent:=len;

  data:=d;
  //bool pgm = flags & TRANSFER_PGM;
  while (len>0) do
  begin
    //u8 c = pgm ? pgm_read_byte(data++) : *data++;
    c:=data^;
    if (not SendControl(c)) then Exit(-1);

    Inc(data);
    Dec(len);
  end;

  Exit(sent);
end;

//	Does not timeout or cross fifo boundaries
function USB_RecvControl(d: PUint8; len: Int16): Int16;
var
  length, recvLength: Int16;
begin
  length:= len;
  while(length>0) do
  begin
    // Dont receive more than the USB Control EP has to offer
    // Use fixed 64 because control EP always have 64 bytes even on 16u2.
    recvLength:= length;
    if (recvLength > 64) then
      recvLength:= 64;

    // Write data to fit to the end (not the beginning) of the array
    WaitOUT();
    Recv(PUint8(UInt16(d) + len - length), recvLength);
    ClearOUT();
    Dec(length, recvLength);
  end;
  Exit(len);
end;

//	Number of bytes, assumes a rx endpoint
function USB_Available(ep: UInt8): UInt8;
var
  LockEP: TLockEP;
begin
  LockEP.LockEP(ep); //LockEP lock(ep);      ??????????????
  Result:=FifoByteCount();
  LockEP.UnlockEP;
end;

//	Non Blocking receive
//	Return number of bytes read
function USB_RecvBuff(ep: UInt8; d: PUInt8; len: Int16): Int16;
var
  n: UInt8;
  dst: PUint8;
  LockEP: TLockEP;
begin
  if (_usbConfiguration=0) or (len < 0) then
    Exit(-1);

  LockEP.LockEP(ep); //LockEP lock(ep);      ??????????????
  n:= FifoByteCount;
  if n<len then
    len:=n;
  n:= len;
  dst:=d;
  while (n>0) do
  begin
    dst^:=Recv8();
    Inc(dst);
    Dec(n);
  end;
  if (len>0) and (FifoByteCount=0) then	// release empty buffer
    ReleaseRX();
  LockEP.UnlockEP;

  Result:=len;
end;

//	Recv 1 byte if ready
function USB_Recv(ep: UInt8): Int16;
var
  c: UInt8;
begin
  if (USB_RecvBuff(ep,@c,1) <> 1) then
    Result:=-1
  else
    Result:=c;
end;

procedure USB_Flush(ep: UInt8);
begin
  SetEP(ep);
  if (FifoByteCount>0) then
    ReleaseTX;
end;

procedure USB_ClockEnable; inline;
begin
//#if defined(UHWCON)
	UHWCON :=UHWCON or (1 shl UVREGE); // power internal reg
//#endif
  USBCON:= (1 shl USBE) or (1 shl FRZCLK);	// clock frozen, usb enabled

// ATmega32U4
//#if defined(PINDIV)
{$if F_CPU = 16000000}
  PLLCSR:=PLLCSR or (1 shl PINDIV); // Need 16 MHz xtal
{$elseif F_CPU = 8000000}
  PLLCSR:=PLLCSR and not (1 shl PINDIV); // Need  8 MHz xtal
{$else}
 {$Fatal Clock rate of F_CPU not supported}
{$endif}

//#elif defined(__AVR_AT90USB82__) || defined(__AVR_AT90USB162__) || defined(__AVR_ATmega32U2__) || defined(__AVR_ATmega16U2__) || defined(__AVR_ATmega8U2__)
//	// for the u2 Series the datasheet is confusing. On page 40 its called PINDIV and on page 290 its called PLLP0
//#if F_CPU == 16000000UL
//	// Need 16 MHz xtal
//	PLLCSR |= (1 << PLLP0);
//#elif F_CPU == 8000000UL
//	// Need 8 MHz xtal
//	PLLCSR &= ~(1 << PLLP0);
//#endif
//
//// AT90USB646, AT90USB647, AT90USB1286, AT90USB1287
//#elif defined(PLLP2)
//#if F_CPU == 16000000UL
//#if defined(__AVR_AT90USB1286__) || defined(__AVR_AT90USB1287__)
//	// For Atmel AT90USB128x only. Do not use with Atmel AT90USB64x.
//	PLLCSR = (PLLCSR & ~(1<<PLLP1)) | ((1<<PLLP2) | (1<<PLLP0)); // Need 16 MHz xtal
//#elif defined(__AVR_AT90USB646__) || defined(__AVR_AT90USB647__)
//	// For AT90USB64x only. Do not use with AT90USB128x.
//	PLLCSR = (PLLCSR & ~(1<<PLLP0)) | ((1<<PLLP2) | (1<<PLLP1)); // Need 16 MHz xtal
//#else
//#error "USB Chip not supported, please defined method of USB PLL initialization"
//#endif
//#elif F_CPU == 8000000UL
//	// for Atmel AT90USB128x and AT90USB64x
//	PLLCSR = (PLLCSR & ~(1<<PLLP2)) | ((1<<PLLP1) | (1<<PLLP0)); // Need 8 MHz xtal
//#else
//#error "Clock rate of F_CPU not supported"
//#endif
//#else
//#error "USB Chip not supported, please defined method of USB PLL initialization"
//#endif

	PLLCSR :=PLLCSR or (1 shl PLLE);
	while ((PLLCSR and (1 shl PLOCK))=0) do		// wait for lock pll
	begin
	end;

	// Some tests on specific versions of macosx (10.7.3), reported some
	// strange behaviors when the board is reset using the serial
	// port touch at 1200 bps. This delay fixes this behavior.
	delay_ms(1);
//#if defined(OTGPADE)
	USBCON:= (USBCON and not (1 shl FRZCLK)) or (1 shl OTGPADE);	// start USB clock, enable VBUS Pad
//#else
//	USBCON:= USBCON and not (1 shl FRZCLK);	// start USB clock
//#endif

//#if defined(RSTCPU)
//#if defined(LSM)
	UDCON:=UDCON and not ((1 shl RSTCPU) or (1 shl LSM) or (1 shl RMWKUP) or (1 shl DETACH));	// enable attach resistor, set full speed mode
//#else // u2 Series
//	UDCON &= ~((1 << RSTCPU) | (1 << RMWKUP) | (1 << DETACH));	// enable attach resistor, set full speed mode
//#endif
//#else
//	// AT90USB64x and AT90USB128x don't have RSTCPU
//	UDCON &= ~((1<<LSM) | (1<<RMWKUP) | (1<<DETACH));	// enable attach resistor, set full speed mode
//#endif
end;

//	General interrupt
procedure USB_GEN_vect; public name 'USB_GEN_ISR'; interrupt;
var
  _udint: UInt8;
begin
  _udint:= UDINT;
  UDINT:= UDINT and not ((1 shl EORSTI) or (1 shl SOFI)); // clear the IRQ flags for the IRQs which are handled here, except WAKEUPI and SUSPI (see below)

  //End of Reset
  if (_udint and (1 shl EORSTI))>0 then
  begin
    InitEP(0, EP_TYPE_CONTROL, EP_SINGLE_64);	// init ep0
    _usbConfiguration:= 0;			// not configured yet
    UEIENX:= 1 shl RXSTPE;			// Enable interrupts for ep0
  end;

  //Start of Frame - happens every millisecond so we use it for TX and RX LED one-shot timing, too
  if (_udint and (1 shl SOFI))>0 then
  begin
    USB_Flush(CDC_TX); // Send a tx frame if found

    // check whether the one-shot period has elapsed.  if so, turn off the LED
    if (TxLEDPulse>0) then
    begin
      Dec(TxLEDPulse);
      if TxLEDPulse=0 then
        TXLED0;
    end;
    if (RxLEDPulse>0) then
    begin
      Dec(RxLEDPulse);
      if RxLEDPulse=0 then
        RXLED0;
    end;
  end;

  // the WAKEUPI interrupt is triggered as soon as there are non-idle patterns on the data
  // lines. Thus, the WAKEUPI interrupt can occur even if the controller is not in the "suspend" mode.
  // Therefore the we enable it only when USB is suspended
  if (_udint>0) and ((1 shl WAKEUPI)>0) then
  begin
    UDIEN:= (UDIEN and not (1 shl WAKEUPE)) or (1 shl SUSPE); // Disable interrupts for WAKEUP and enable interrupts for SUSPEND

    //TODO
    // WAKEUPI shall be cleared by software (USB clock inputs must be enabled before).
    //USB_ClockEnable();
    UDINT:= UDINT and not (1 shl WAKEUPI);
    _usbSuspendState:= (_usbSuspendState and not (1 shl SUSPI)) or (1 shl WAKEUPI);
  end
  else if (udint>0) and ((1 shl SUSPI)>0) then // only one of the WAKEUPI / SUSPI bits can be active at time
  begin
    UDIEN:= (UDIEN and not (1 shl SUSPE)) or (1 shl WAKEUPE); // Disable interrupts for SUSPEND and enable interrupts for WAKEUP

    //TODO
    //USB_ClockDisable();

    UDINT:=UDINT and not ((1 shl WAKEUPI) or (1 shl SUSPI)); // clear any already pending WAKEUP IRQs and the SUSPI request
    _usbSuspendState:= (_usbSuspendState and not (1 shl WAKEUPI)) or (1 shl SUSPI);
  end;
end;

function SendInterfaces: UInt8;
var
  interfaces: UInt8 = 0;
begin

//#ifdef CDC_ENABLED
  CDC_GetInterface(interfaces);
//#endif

  {TODO: support for PluggableUSB}
//#ifdef PLUGGABLE_USB_ENABLED
//	PluggableUSB().getInterface(&interfaces);
//#endif

  Result:=interfaces;
end;

function SendConfiguration(maxlen: Int16): boolean;
var
  interfaces: UInt8;
  config: TConfigDescriptor;
begin
  //	Count and measure interfaces
  InitControl(0);
  interfaces:= SendInterfaces();
//  config = D_CONFIG(_cmark + sizeof(ConfigDescriptor),interfaces);
//#define D_CONFIG(_totalLength,_interfaces) \
//	{ 9, 2, _totalLength,_interfaces, 1, 0, USB_CONFIG_BUS_POWERED | USB_CONFIG_REMOTE_WAKEUP, USB_CONFIG_POWER_MA(USB_CONFIG_POWER) }
  config.len:=9;
  config.dtype:=2;
  config.clen:=_cmark + sizeof(TConfigDescriptor);
  config.numInterfaces:=interfaces;
  config.config:=1;
  config.iconfig:=0;
  config.attributes:=USB_CONFIG_BUS_POWERED or USB_CONFIG_REMOTE_WAKEUP;
  config.maxPower:=USB_CONFIG_POWER div 2;

  //	Now send them
  InitControl(maxlen);
  USB_SendControl(0,@config,sizeof(TConfigDescriptor));
  SendInterfaces();
  Result:=true;
end;

function SendDescriptor(setup: TUSBSetup): boolean;
var
  t, desc_length: UInt8;
  desc_addr: PUint8; //16?
  name: array[0..ISERIAL_MAX_LEN-1] of char;
begin
  t:= setup.wValueH;
  if (t = USB_CONFIGURATION_DESCRIPTOR_TYPE) then
    Exit(SendConfiguration(setup.wLength));

  InitControl(setup.wLength);

  {TODO: support for PluggableUSB}
//#ifdef PLUGGABLE_USB_ENABLED
//	int ret = PluggableUSB().getDescriptor(setup);
//	if (ret != 0) {
//		return (ret > 0 ? true : false);
//	}
//#endif

  desc_addr:=nil;
  desc_length:=0;

  if (t = USB_DEVICE_DESCRIPTOR_TYPE) then
  begin
    desc_addr:= @USB_DeviceDescriptorIAD;
    desc_length:=Sizeof(USB_DeviceDescriptorIAD);
  end
  else if (t = USB_STRING_DESCRIPTOR_TYPE) then
  begin
    if (setup.wValueL = 0) then
    begin
      desc_addr:= @STRING_LANGUAGE;
      desc_length:=Sizeof(STRING_LANGUAGE);
    end
    else if (setup.wValueL = IPRODUCT) then
      Exit(USB_SendStringDescriptor(STRING_PRODUCT, strlen({USB_PRODUCT}STRING_PRODUCT), 0))
    else if (setup.wValueL = IMANUFACTURER) then
      Exit(USB_SendStringDescriptor(STRING_MANUFACTURER, strlen({USB_MANUFACTURER}STRING_MANUFACTURER), 0))
    else if (setup.wValueL = ISERIAL) then
    begin
  {TODO: support for PluggableUSB}
//#ifdef PLUGGABLE_USB_ENABLED
//			char name[ISERIAL_MAX_LEN];
//			PluggableUSB().getShortName(name);
//			return USB_SendStringDescriptor((uint8_t*)name, strlen(name), 0);
//#endif
      name:='TEST'; //'A'+NumInterfaces
      Exit(USB_SendStringDescriptor(name, strlen(name), 0));
    end
    else
      Exit(false);
  end;

  if (desc_addr = nil) then
    Exit(false);
  //u8 desc_length = pgm_read_byte(desc_addr);

  USB_SendControl({TRANSFER_PGM}0,desc_addr,desc_length);
  Result:=true;
end;

//Endpoint 0 interrupt
procedure USB_COM_vect; public name 'USB_COM_ISR'; interrupt;
var
  Setup: TUSBSetup;
  requestType, r: UInt8;
  wValue: UInt16;
  ok: boolean;
begin
  SetEP(0);
  if (ReceivedSetupInt=0) then
    Exit;

  Recv(@setup,8);
  ClearSetupInt();

  requestType:= setup.bmRequestType;
  if (requestType and REQUEST_DEVICETOHOST)>0 then
    WaitIN
  else
    ClearIN;

  ok:= true;
  if (requestType and REQUEST_TYPE)=REQUEST_STANDARD then
  begin
    //	Standard Requests
    r:= setup.bRequest;
    wValue:= setup.wValueL or (setup.wValueH shl 8);
    if (r = GET_STATUS) then
    begin
      if (requestType = (REQUEST_DEVICETOHOST or REQUEST_STANDARD or REQUEST_DEVICE)) then
      begin
        Send8(_usbCurrentStatus);
        Send8(0);
      end
      else
      begin
	// TODO: handle the HALT state of an endpoint here
	// see "Figure 9-6. Information Returned by a GetStatus() Request to an Endpoint" in usb_20.pdf for more information
	Send8(0);
	Send8(0);
      end
    end
    else if (r = CLEAR_FEATURE) then
    begin
      if ((requestType = (REQUEST_HOSTTODEVICE or REQUEST_STANDARD or REQUEST_DEVICE)) and (wValue = DEVICE_REMOTE_WAKEUP)) then
        _usbCurrentStatus:=_usbCurrentStatus and not FEATURE_REMOTE_WAKEUP_ENABLED;
    end
    else if (r = SET_FEATURE) then
    begin
      if ((requestType = (REQUEST_HOSTTODEVICE or REQUEST_STANDARD or REQUEST_DEVICE)) and (wValue = DEVICE_REMOTE_WAKEUP)) then
        _usbCurrentStatus:=_usbCurrentStatus or FEATURE_REMOTE_WAKEUP_ENABLED;
    end
    else if (r = SET_ADDRESS) then
    begin
      WaitIN();
      UDADDR:= setup.wValueL or (1 shl ADDEN);
    end
    else if (r = GET_DESCRIPTOR) then
      ok:= SendDescriptor(setup)
    else if (r = SET_DESCRIPTOR) then
      ok:= false
    else if (r = GET_CONFIGURATION) then
      Send8(1)
    else if (r = SET_CONFIGURATION) then
    begin
      if ((requestType and REQUEST_RECIPIENT) = REQUEST_DEVICE) then
      begin
	InitEndpoints;
	_usbConfiguration:= setup.wValueL;
      end
      else
	ok:= false;
    end
    else if (r = GET_INTERFACE) then
    begin
    end
    else if (r = SET_INTERFACE) then
    begin
    end
  end
  else
  begin
    InitControl(setup.wLength); // Max length of transfer
    ok:= ClassInterfaceRequest(setup);
  end;

  if ok then
    ClearIN
  else
    Stall;
end;

procedure TUSBDevice.Attach;
begin
  _usbConfiguration:= 0;
  _usbCurrentStatus:= 0;
  _usbSuspendState:= 0;
  USB_ClockEnable();

  UDINT:=UDINT and not ((1 shl WAKEUPI) or (1 shl SUSPI)); // clear already pending WAKEUP / SUSPEND requests
  UDIEN:= (1 shl EORSTE) or (1 shl SOFE) or (1 shl SUSPE); // Enable interrupts for EOR (End of Reset), SOF (start of frame) and SUSPEND

  TX_RX_LED_INIT;
end;

procedure TUSBDevice.Detach;
begin
  //empty
end;

//	Check for interrupts
//	TODO: VBUS detection
function TUSBDevice.Configured: boolean;
begin
  Result:=_usbConfiguration>0;
end;

procedure TUSBDevice.Poll;
begin
  //empty
end;

function TUSBDevice.WakeupHost: boolean;
begin
  Result:=false;
  // clear any previous wakeup request which might have been set but could be processed at that time
  // e.g. because the host was not suspended at that time
  UDCON:=UDCON and not (1 shl RMWKUP);

  if (((UDCON and (1 shl RMWKUP))=0) and ((_usbSuspendState and (1 shl SUSPI))>0)
    and ((_usbCurrentStatus and FEATURE_REMOTE_WAKEUP_ENABLED)>0)) then
  begin
    // This short version will only work, when the device has not been suspended. Currently the
    // Arduino core doesn't handle SUSPEND at all, so this is ok.
    USB_ClockEnable();
    UDCON:=UDCON or (1 shl RMWKUP); // send the wakeup request
    Result:=true;
  end;
end;

function TUSBDevice.IsSuspended: boolean;
begin
  Result:=(_usbSuspendState and (1 shl SUSPI))>0;
end;

end.
