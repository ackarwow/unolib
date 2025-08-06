unit cdc;

{
  Based on Arduino library source
  ported to Pascal by Andrzej Karwowski 2024
}

interface

{$mode objfpc}


uses
  usb, wdt;

const
{$IF FPC_SRAMSIZE < 1023}
  SERIAL_BUFFER_SIZE = 16;
{$ELSE}
  SERIAL_BUFFER_SIZE = 64;
{$ENDIF}

  FLASHEND = FPC_FLASHBASE + FPC_FLASHSIZE;

  ONE_STOP_BIT = 0;
  ONE_AND_HALF_STOP_BIT = 1;
  TWO_STOP_BITS = 2;

  NO_PARITY = 0;
  ODD_PARITY = 1;
  EVEN_PARITY = 2;
  MARK_PARITY = 3;
  SPACE_PARITY = 4;

{ .elf dump:

USBCore.cpp
PluggableUSB.cpp
CDC.cpp
wiring.c
Print.h
Stream.h
USBAPI.h
wdt.h
main.cpp
stdint.h
USBAPI.h
PluggableUSB.h
stddef.h
USBCore.h
hooks.c

<STRING_LANGUAGE>:
<USB_DeviceDescriptorIAD>:
<STRING_MANUFACTURER>:
<STRING_PRODUCT>:
<_Z8USB_RecvhPvi.constprop.5>
<_Z13USB_SendSpaceh.constprop.3>
<_Z12PluggableUSBv>
<_ZN7Serial_5writeEh>
<_ZN7Serial_5flushEv>
<_ZN7Serial_17availableForWriteEv> -> USB_SendSpace
<_ZN7Serial_4readEv>
<_ZN7Serial_4peekEv>
<_ZN7Serial_9availableEv>
<_ZL11SendControlh>
<_ZL24USB_SendStringDescriptorPKhhh>
<_Z15USB_SendControlhPKvi>
<_ZL14SendInterfacesv>
<_ZL4RecvPVhh>
<micros>
<delay.constprop.10>
<_ZN7Serial_5writeEPKhj>
<__vector_10>
<__vector_11>
<__vector_23>
<_GLOBAL__sub_I__cdcInterface>
<main>
}

type
  TDeviceDescriptor = packed record
    len: UInt8;	// 18
    dtype: UInt8; // 1 USB_DEVICE_DESCRIPTOR_TYPE
    usbVersion: UInt16;	// 0x200 or 0x210
    deviceClass: UInt8;
    deviceSubClass: UInt8;
    deviceProtocol: UInt8;
    packetSize0: UInt8;	// Packet 0
    idVendor: UInt16;
    idProduct: UInt16;
    deviceVersion: UInt16; // 0x100
    iManufacturer: UInt8;
    iProduct: UInt8;
    iSerialNumber: UInt8;
    bNumConfigurations: UInt8;
  end;

  TLineInfo = packed record
    dwDTERate: UInt32;
    bCharFormat: UInt8;
    bParityType: UInt8;
    bDataBits: UInt8;
    lineState: UInt8;
  end;

  TIADDescriptor = packed record
    len: UInt8; // 8
    dtype: UInt8; // 11
    firstInterface: UInt8;
    interfaceCount: UInt8;
    functionClass: UInt8;
    functionSubClass: UInt8;
    functionProtocol: UInt8;
    iInterface: UInt8;
  end;

  //Interface
  TInterfaceDescriptor = packed record
    len: UInt8;	// 9
    dtype: UInt8; // 4
    number: UInt8;
    alternate: UInt8;
    numEndpoints: UInt8;
    interfaceClass: UInt8;
    interfaceSubClass: UInt8;
    protocol: UInt8;
    iInterface: UInt8;
  end;

  //	CDC CS interface descriptor
  TCDCCSInterfaceDescriptor = packed record
    len: UInt8;	// 5
    dtype: UInt8; // 0x24
    subtype: UInt8;
    d0: UInt8;
    d1: UInt8;
  end;

  TCMFunctionalDescriptor = packed record
    len: UInt8;
    dtype: UInt8; // 0x24
    subtype: UInt8; // 1
    bmCapabilities: UInt8;
    bDataInterface: UInt8;
  end;

  TACMFunctionalDescriptor = packed record
    len: UInt8;
    dtype: UInt8; // 0x24
    subtype: UInt8; // 1
    bmCapabilities: UInt8;
  end;

  //Endpoint
  TEndpointDescriptor = packed record
    len: UInt8;	// 7
    dtype: UInt8; // 5
    addr: UInt8;
    attr: UInt8;
    packetSize: UInt16;
    interval: UInt8;
  end;

  TCDCDescriptor = packed record
    // IAD
    iad: TIADDescriptor; // Only needed on compound device

    // Control
    cif: TInterfaceDescriptor;
    header: TCDCCSInterfaceDescriptor;
    callManagement: TCMFunctionalDescriptor; // Call Management
    controlManagement: TACMFunctionalDescriptor; // ACM
    functionalDescriptor: TCDCCSInterfaceDescriptor; // CDC_UNION
    cifin: TEndpointDescriptor;

    //	Data
    dif: TInterfaceDescriptor;
    _in: TEndpointDescriptor;
    _out: TEndpointDescriptor;
  end;

  TCDCSerial = object
  private
    peek_buffer: Int16;
    write_error: Int16;
    procedure SetWriteError;
  public
    _rx_buffer_head: UInt8;
    _rx_buffer_tail: UInt8;
    _rx_buffer: {packed} array [0..SERIAL_BUFFER_SIZE-1] of char;

    procedure Start(baud_count: UInt32); //begin
    procedure Stop; //end;
    function Available: Int16;
    function Peek: Int16;
    function Read: Int16;
    function AvailableForWrite: Int16;
    procedure Flush;
    function Write(c: UInt8): UInt16; //size_t
    function WriteBuff(const buffer: PUint8; size: UInt16): UInt16; //size_t
    function IsOK: boolean;//operator bool();

    // This method allows processing "SEND_BREAK" requests sent by
    // the USB host. Those requests indicate that the host wants to
    // send a BREAK signal and are accompanied by a single uint16_t
    // value, specifying the duration of the break. The value 0
    // means to end any current break, while the value 0xffff means
    // to start an indefinite break.
    // readBreak() will return the value of the most recent break
    // request, but will return it at most once, returning -1 when
    // readBreak() is called again (until another break request is
    // received, which is again returned once).
    // This also mean that if two break requests are received
    // without readBreak() being called in between, the value of the
    // first request is lost.
    // Note that the value returned is a long, so it can return
    // 0-0xffff as well as -1.
    function ReadBreak: Int32;

    // These return the settings specified by the USB host for the
    // serial port. These aren't really used, but are offered here
    // in case a sketch wants to act on these settings.
    function Baud: UInt32;
    function StopBits: UInt8;
    function ParityType: UInt8;
    function NumBits: UInt8;
    function Dtr: boolean;
    function Rts: boolean;
  end;

const
  RAMEND = (FPC_SRAMBASE + FPC_SRAMSIZE - 1);  // Last On-Chip SRAM Location

  USB_VERSION = $200;
  USB_VID = $2341;
  USB_PID = $8036;
  USB_ENDPOINT_TYPE_INTERRUPT = $03;
  USB_ENDPOINT_TYPE_BULK = $02;

  I_MANUFACTURER = 1;
  I_PRODUCT = 2;
  I_SERIAL = 3;

  USB_DeviceDescriptorIAD: TDeviceDescriptor =
    (len: 18; dtype: 1; usbVersion: USB_VERSION; deviceClass: $EF;
     deviceSubClass: $02; deviceProtocol: $01; packetSize0: 64;
     idVendor: USB_VID; idProduct: USB_PID; deviceVersion: $100;
     iManufacturer: I_MANUFACTURER; iProduct: I_PRODUCT;
     iSerialNumber: I_SERIAL; bNumConfigurations: 1);

{00000100 <USB_DeviceDescriptorIAD>:
 100:	12 01 00 02 ef 02 01 40 41 23 36 80 00 01 01 02     .......@A#6.....
 110:	03 01}

  STRING_MANUFACTURER: PChar = 'Arduino LLC';
  STRING_PRODUCT: PChar = 'Arduino Leonardo';
  STRING_LANGUAGE: packed array[0..1] of UInt16=((3 shl 8) or (2+2), $0409 {English});

  CDC_SET_LINE_CODING = $20;
  CDC_GET_LINE_CODING = $21;
  CDC_SET_CONTROL_LINE_STATE = $22;
  CDC_SEND_BREAK = $23;

  CDC_ACM_INTERFACE = 0; // CDC ACM
  CDC_DATA_INTERFACE = 1; // CDC Data
  CDC_FIRST_ENDPOINT = 1;
  CDC_ENDPOINT_ACM = CDC_FIRST_ENDPOINT; // CDC First
  CDC_ENDPOINT_OUT = CDC_FIRST_ENDPOINT+1;
  CDC_ENDPOINT_IN = CDC_FIRST_ENDPOINT+2;

  CDC_COMMUNICATION_INTERFACE_CLASS = $02;
  CDC_ABSTRACT_CONTROL_MODEL = $02;
  CDC_HEADER = $00;
  CDC_CALL_MANAGEMENT = $01;
  CDC_ABSTRACT_CONTROL_MANAGEMENT = $02;
  CDC_UNION = $06;
  CDC_DATA_INTERFACE_CLASS = $0A;

  //INTERFACE_COUNT = MSC_INTERFACE + MSC_INTERFACE_COUNT)

  CDC_RX = CDC_ENDPOINT_OUT;
  CDC_TX = CDC_ENDPOINT_IN;

  _cdcInterface: TCDCDescriptor =
  (
    iad:
     (len: 8; dtype: 11; firstInterface: 0; interfacecount: 2;
      functionclass: CDC_COMMUNICATION_INTERFACE_CLASS;
      functionSubClass: CDC_ABSTRACT_CONTROL_MODEL;
      functionProtocol: 0; iInterface: 0);

    cif: // CDC communication interface
     (len: 9; dtype: 4; number: CDC_ACM_INTERFACE; alternate: 0; numEndpoints: 1;
      interfaceclass: CDC_COMMUNICATION_INTERFACE_CLASS;
      interfacesubClass: CDC_ABSTRACT_CONTROL_MODEL;
      protocol: 0; iInterface: 0);

    header: // Header (1.10 bcd)
      (len: 5; dtype: $24; subtype: CDC_HEADER; d0: $10; d1: $01);

    callManagement: // Device handles call management (not)
      (len: 5; dtype: $24; subtype: CDC_CALL_MANAGEMENT; bmCapabilities: 1; bDataInterface: 1);

    ControlManagement: // SET_LINE_CODING, GET_LINE_CODING, SET_CONTROL_LINE_STATE supported
      (len: 4; dtype: $24; subtype: CDC_ABSTRACT_CONTROL_MANAGEMENT; bmCapabilities: 6);

    functionalDescriptor: // Communication interface is master, data interface is slave 0
      (len: 5; dtype: $24; subtype: CDC_UNION; d0: CDC_ACM_INTERFACE; d1: CDC_DATA_INTERFACE);

    cifin:
      (len: 7; dtype: 5; addr: ({Lo}(CDC_ENDPOINT_ACM or $80)); attr: USB_ENDPOINT_TYPE_INTERRUPT;
       packetSize: $10; interval: $40);

    //CDC data interface

    dif:
      (len: 9; dtype: 4; number: CDC_DATA_INTERFACE; alternate: 0; numEndpoints: 2;
       interfaceClass: CDC_DATA_INTERFACE_CLASS; interfacesubClass: 0; protocol: 0; iInterface: 0);

    _in:
      (len: 7; dtype: 5; addr: ({Lo}(CDC_ENDPOINT_OUT or $00)); attr: USB_ENDPOINT_TYPE_BULK;
       packetSize: USB_EP_SIZE; interval: 0);

    _out:
      (len: 7; dtype: 5; addr: ({Lo}(CDC_ENDPOINT_IN or $80)); attr: USB_ENDPOINT_TYPE_BULK;
       packetSize: USB_EP_SIZE; interval: 0);
  );


//struct ring_buffer;

function CDC_GetInterface(var interfaceNum: UInt8): Int16;
function CDC_Setup(setup: TUSBSetup): boolean;

var
  _usbLineInfo: TLineInfo=(dwDTERate: 57600; bCharFormat: $00; bParityType: $00; bDataBits: $00; lineState:  $00);
  breakValue: Int32 = -1;
  wdtcsr_save: UInt8;
  Serial: TCDCSerial;

implementation

uses
  ccrause_delay, intrinsics;

//after https://github.com/ccrause/fpc-avr/blob/33856a5abc210138f61e9c13f567a8f4ac7e1486/src/library/progmem.pas#L30
function pgm_read_word(const address: pointer): UInt16; assembler; nostackframe;
asm
  movw ZL, r24
  lpm r24, Z+
  lpm r25, Z
end['r24', 'r25', 'r30', 'r31'];

function IsLUFABootloader: boolean;
begin
  //return pgm_read_word(FLASHEND - 1) == NEW_LUFA_SIGNATURE;
  Result:=pgm_read_word(Pointer(FLASHEND-1))= NEW_LUFA_SIGNATURE;
end;

function CDC_GetInterface(var interfaceNum: UInt8): Int16;
begin
  Inc(interfaceNum, 2);	// uses 2
  Result:=USB_SendControl({TRANSFER_PGM}0,@_cdcInterface,sizeof(_cdcInterface));
end;

function CDC_Setup(setup: TUSBSetup): boolean;
var
  r, requestType: UInt8;
  magickeypos: UInt16;
  c: UInt16;
begin
  r:= setup.bRequest;
  requestType:= setup.bmRequestType;

  if (requestType = REQUEST_DEVICETOHOST_CLASS_INTERFACE) then
  begin
    if (r = CDC_GET_LINE_CODING) then
    begin
      USB_SendControl(0,@_usbLineInfo,7);
      Exit(true);
    end;
  end;

  Result:=false;
  if (requestType = REQUEST_HOSTTODEVICE_CLASS_INTERFACE) then
  begin
    if (r = CDC_SEND_BREAK) then
    begin
      breakValue:= ((UInt16(setup.wValueH) shl 8) or setup.wValueL);
    end;

    if (r = CDC_SET_LINE_CODING) then
    begin
      USB_RecvControl(@_usbLineInfo,7);
    end;

    if (r = CDC_SET_CONTROL_LINE_STATE) then
    begin
      _usbLineInfo.lineState:=setup.wValueL;

      // auto-reset into the bootloader is triggered when the port, already
      // open at 1200 bps, is closed.  this is the signal to start the watchdog
      // with a relatively long period so it can finish housekeeping tasks
      // like servicing endpoints before the sketch ends

      magickeypos:= MAGIC_KEY_POS;

      // If we don't use the new RAMEND directly, check manually if we have a newer bootloader.
      // This is used to keep compatible with the old leonardo bootloaders.
      // You are still able to set the magic key position manually to RAMEND-1 to save a few bytes for this check.
      if (MAGIC_KEY_POS > RAMEND-1) then
      begin
        // For future boards save the key in the inproblematic RAMEND
        // Which is reserved for the main() return value (which will never return)
        if (isLUFAbootloader()) then // hooray, we got a new bootloader!
	        magickeypos:=RAMEND-1;
      end;

      // We check DTR state to determine if host port is open (bit 0 of lineState).
      if (_usbLineInfo.dwDTERate=1200) and ((_usbLineInfo.lineState and $01) = 0) then
      begin
        if MAGIC_KEY_POS <> (RAMEND-1) then
        begin
          // Backup ram value if its not a newer bootloader and it hasn't already been saved.
          // This should avoid memory corruption at least a bit, not fully
          if ((magickeypos <> (RAMEND-1)) and (PUint16(magic_key_pos)^ <> MAGIC_KEY)) then
             PUint16(RAMEND-1)^:=PUint16(magic_key_pos)^;
        end;
	      // Store boot key
	      PUint16(magic_key_pos)^:= MAGIC_KEY;
	      // Save the watchdog state in case the reset is aborted.
	      wdtcsr_save:= WDTCSR;
	      wdt_enable(WDTO_120MS);
        {$IFDEF AVRPASCAL}
        while (true) do;
        {$ENDIF}
      end
      else if (PUInt16(magic_key_pos)^ = MAGIC_KEY) then
      begin
	      // Most OSs do some intermediate steps when configuring ports and DTR can
	      // twiggle more than once before stabilizing.
	      // To avoid spurious resets we set the watchdog to 120ms and eventually
	      // cancel if DTR goes back high.
	      // Cancellation is only done if an auto-reset was started, which is
	      // indicated by the magic key having been set.
	      wdt_reset();
	      // Restore the watchdog state in case the sketch was using it.
	      WDTCSR:=WDTCSR or (1 shl WDCE) or (1 shl WDE);
	      WDTCSR:= wdtcsr_save;
        if MAGIC_KEY_POS <> (RAMEND-1) then
        begin
          // Restore backed up (old bootloader) magic key data
          if (magickeypos <> (RAMEND-1)) then
            PUint16(magic_key_pos)^:= PUint16(RAMEND-1)^;
        end;
	      // Clean up RAMEND key
	      PUint16(magic_key_pos)^:=$0000;
      end;
    end;
    Result:=true;
  end;
end;

procedure TCDCSerial.Start(baud_count: UInt32); //begin
begin
  peek_buffer:=-1;
  write_error:=0;
end;

procedure TCDCSerial.Stop; //end;
begin
  //empty
end;

function TCDCSerial.Available: Int16;
begin
  if (peek_buffer >= 0) then
    Result:=1 + USB_Available(CDC_RX)
  else
    Result:=USB_Available(CDC_RX);
end;

function TCDCSerial.Peek: Int16;
begin
  if (peek_buffer < 0) then
    peek_buffer:= USB_Recv(CDC_RX);

  Result:=peek_buffer;
end;

function TCDCSerial.Read: Int16;
var
  c: Int16;
begin
  if (peek_buffer >= 0) then
  begin
    c:=peek_buffer;
    peek_buffer:= -1;
    Result:=c;
  end
  else
    Result:=USB_Recv(CDC_RX);
end;

function TCDCSerial.AvailableForWrite: Int16;
begin
  Result:=USB_SendSpace(CDC_TX);
end;

procedure TCDCSerial.Flush;
begin
  USB_Flush(CDC_TX);
end;

procedure TCDCSerial.SetWriteError;
begin
  write_error:=1;
end;

function TCDCSerial.Write(c: UInt8): UInt16; //size_t
begin
  Result:=WriteBuff(@c, 1);
end;

function TCDCSerial.WriteBuff(const buffer: PUint8; size: UInt16): UInt16;
var
  r: Int16;
begin
  //only try to send bytes if the high-level CDC connection itself
  //is open (not just the pipe) - the OS should set lineState when the port
  //is opened and clear lineState when the port is closed.
  //bytes sent before the user opens the connection or after
  //the connection is closed are lost - just like with a UART.

  // TODO - ZE - check behavior on different OSes and test what happens if an
  // open connection isn't broken cleanly (cable is yanked out, host dies
  // or locks up, or host virtual serial port hangs)
  if (_usbLineInfo.lineState > 0) then
  begin
    r:= USB_Send(CDC_TX,buffer,size);
    if (r > 0) then
      Result:= r
    else
    begin
      SetWriteError;
      Result:=0;
    end;
  end
  else
  begin
    setWriteError();
    Result:=0;
  end;
end;

// This operator is a convenient way for a sketch to check whether the
// port has actually been configured and opened by the host (as opposed
// to just being connected to the host).  It can be used, for example, in
// setup() before printing to ensure that an application on the host is
// actually ready to receive and display the data.
// We add a short delay before returning to fix a bug observed by Federico
// where the port is configured (lineState != 0) but not quite opened.
function TCDCSerial.IsOK: boolean;//operator bool();
begin
  Result:= false;
  if (_usbLineInfo.lineState > 0) then
    Result:= true;
  delay_ms(10);
end;

function TCDCSerial.ReadBreak: Int32;
begin
  // Disable IRQs while reading and clearing breakValue to make
  // sure we don't overwrite a value just set by the ISR.
  //ATOMIC_BLOCK(ATOMIC_RESTORESTATE) {
  //	ret = breakValue;
  //	breakValue = -1;
  //}
  avr_cli;
  Result:= breakValue;
  breakValue:= -1;
  avr_sei;
end;

function TCDCSerial.Baud: UInt32;
begin
  //// Disable interrupts while reading a multi-byte value
  //uint32_t baudrate;
  //ATOMIC_BLOCK(ATOMIC_RESTORESTATE) {
  //	baudrate =  _usbLineInfo.dwDTERate;
  //}
  //return baudrate;
  avr_cli;
  Result:=_usbLineInfo.dwDTERate;
  avr_sei;
end;

function TCDCSerial.StopBits: UInt8;
begin
  Result:=_usbLineInfo.bCharFormat;
end;

function TCDCSerial.ParityType: UInt8;
begin
  Result:=_usbLineInfo.bParityType;
end;

function TCDCSerial.NumBits: UInt8;
begin
  Result:=_usbLineInfo.bDataBits;
end;

function TCDCSerial.Dtr: boolean;
begin
  Result:=(_usbLineInfo.lineState and $1)>0;
end;

function TCDCSerial.Rts: boolean;
begin
  Result:=(_usbLineInfo.lineState and $2)>0;
end;

end.


