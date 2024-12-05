unit hardwareserial;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$MODE objfpc}

{
  Based on Arduino library source.
  Ported to Pascal by Andrzej Karwowski 2021.

  Changes:

  v02
  - buffer size decreased to 16 bytes
  - minor technical changes
  v07 (10-16.11.2024: changes suggested by @Dzandaa and @VisualLab)
  - added THardwareSerial.WriteBuff for writing buffer of bytes
  - added THardwareSerial.ReadBuff for reading buffer of bytes
  - THardwareSerial._begin renamed to THardwareSerial.Start, THardwareSerial._end renamed to THardwareSerial.Stop
  - added THardwareSerial.Read for Arduino compatibility
  (Many thanks to @Dzandaa, @ccrause, and @VisualLab for discussion)
  v08 (05.12.2024) 
  - added THardwareSerial.ReadByte by @Dzandaa
}

interface

uses
  defs;

const
  SERIAL_RX_BUFFER_SIZE = 16; // read buffer size
  SERIAL_TX_BUFFER_SIZE = 16; // write buffer size

type
  THardwareSerial = object
  protected
    RXBuffer: array[0..SERIAL_RX_BUFFER_SIZE - 1] of byte;
    TXBuffer: array[0..SERIAL_TX_BUFFER_SIZE - 1] of byte;
    RXBufferTail: byte;
    RXBufferHead: byte;
    TXBufferTail: byte;
    TXBufferHead: byte;
    Written: boolean;
    procedure RXCompleteIRQ;
    procedure TXUDREmptyIRQ;
  public
    procedure Start(const aBaud: UInt32);
    procedure Stop;
    function Available: byte;
    function AvailableForWrite: byte;
    function PeekChar: char;
    function ReadChar: char;
    function ReadByte: byte;
    function Read: Int16;
    procedure Flush;
    procedure WriteChar(const aChar: char);
    procedure Write(const aStr: shortstring);
    procedure WriteLn(const aStr: shortstring);
    procedure WriteBuff(const aBuff: PUInt8; const aLen: UInt8);
    function ReadBuff(const aBuff: PUInt8; const aLen: UInt8): boolean;
  end;

var
  Serial: THardwareSerial;

implementation

procedure THardwareSerial.Start(const aBaud: UInt32);
var
  divider: UInt16;
begin
  divider := F_CPU div (16 * aBaud) - 1;

  UBRR0 := divider; //Baud

  //control registers:

  UCSR0A := (0 shl U2X0); //0 - Normal speed, for synchronic transmission

  UCSR0B := (1 shl TXEN0) or (1 shl RXEN0) or (1 shl RXCIE0) and not (1 shl UDRIE0);
  //TXEN0 - if set then transfer
  //RXEN0 - if set then receive mode
  //RXCIE0 - if set then indicate that an interrupt is triggered when a character is received
  //UDRIE0 - if set then indicate that an interrupt is triggered when a transmit buffer is ready to write
  //-> receive and send, with interrupt on character is received

  //UCSR0C := %011 shl UCSZ0; //8-bit word, 1 stop bit
  //UCSR0C := (0 shl UMSEL0) or (0 shl USBS0) or (3 shl UCSZ0) or (0 shl UPM0);
  UCSR0C := 0
    or (0 shl UMSEL0) // Asynchronous USART
    or (0 shl UPM0) // Parity Disabled
    or (0 shl USBS0) // 1 stop bit
    or (3 shl UCSZ0) // 8-bit character size
    or (0 shl UCPOL0) // Clock polarity - Rising TX, falling RX (for synchronous mode)
    ;

  RXBufferTail := 0;
  RXBufferHead := 0;
  TXBufferTail := 0;
  TXBufferHead := 0;
  Written := false;
end;

procedure THardwareSerial.Stop;
begin
  Flush;

  UCSR0B := 0; // Release blocked pins 0 and 1

  RXBufferTail := 0;
  RXBufferHead := 0;
  TXBufferTail := 0;
  TXBufferHead := 0;
end;

function THardwareSerial.PeekChar: char;
begin
  if (RXBufferHead = RXBufferTail) then
    Result := #0 //-1
  else
    Result := Chr(RXBuffer[RXBufferTail]);
end;


function THardwareSerial.ReadChar: char;
var
  b: byte;
begin
  // if the head isn't ahead of the tail, we don't have any characters
  if (RXBufferHead = RXBufferTail) then
    Result := #0 //-1
  else
  begin
    b := RXBuffer[RXBufferTail];
    RXBufferTail := (RXBufferTail + 1) mod SERIAL_RX_BUFFER_SIZE;
    Result := Chr(b);
  end;
end;

function THardwareSerial.ReadByte: byte;
begin
  // if the head isn't ahead of the tail, we don't have any characters
  if (RXBufferHead = RXBufferTail) then
    Result := 0 //-1
  else
  begin
    Result := RXBuffer[RXBufferTail];
    RXBufferTail := (RXBufferTail + 1) mod SERIAL_RX_BUFFER_SIZE;
  end;
end;

function THardwareSerial.Read: Int16;
var
  b: byte;
begin
  // if the head isn't ahead of the tail, we don't have any characters
  if (RXBufferHead = RXBufferTail) then
    Result := -1
  else
  begin
    b := RXBuffer[RXBufferTail];
    RXBufferTail := (RXBufferTail + 1) mod SERIAL_RX_BUFFER_SIZE;
    Result := b;
  end;
end;

procedure THardwareSerial.WriteChar(const aChar: char);
var
  i: byte;
begin
  Written := true;
  // If the buffer and the data register is empty, just write the byte
  // to the data register and be done. This shortcut helps
  // significantly improve the effective datarate at high (>
  // 500kbit/s) bitrates, where interrupt overhead becomes a slowdown.
  if ((TXBufferHead = TXBufferTail) and BitIsSet(@UCSR0A, UDRE0)) then
  begin
    // If TXC is cleared before writing UDR and the previous byte
    // completes before writing to UDR, TXC will be set but a byte
    // is still being transmitted causing flush() to return too soon.
    // So writing UDR must happen first.
    // Writing UDR and clearing TC must be done atomically, otherwise
    // interrupts might delay the TXC clear so the byte written to UDR
    // is transmitted (setting TXC) before clearing TXC. Then TXC will
    // be cleared when no bytes are left, causing flush() to hang
    //ATOMIC_BLOCK(ATOMIC_RESTORESTATE)
    //  *_udr = c;
    //  *_ucsra = ((*_ucsra) & ((1 << U2X0) | (1 << MPCM0))) | (1 << TXC0);
    UDR0 := byte(aChar);
    UCSR0A := (UCSR0A and ((1 shl U2X0) or (1 shl MPCM0))) or (1 shl TXC0);
    Exit;
  end;

  i := (TXBufferHead + 1) mod SERIAL_TX_BUFFER_SIZE;

  // If the output buffer is full, there's nothing for it other than to
  // wait for the interrupt handler to empty it a bit
  while (TXBufferTail = i) do
  begin
    if (BitIsClear(@SREG, SREG_I)) then
    begin
      // Interrupts are disabled, so we'll have to poll the data
      // register empty flag ourselves. If it is set, pretend an
      // interrupt has happened and call the handler to free up
      // space for us.
      if (BitIsSet(@UCSR0A, UDRE0)) then
        TXUDREmptyIRQ;
    end
    else
    begin
      // nop, the interrupt handler will free up space for us
    end;
  end;

  TXBuffer[TXBufferHead] := byte(aChar);

  // make atomic to prevent execution of ISR between setting the
  // head pointer and setting the interrupt flag resulting in buffer
  // retransmission
  //ATOMIC_BLOCK(ATOMIC_RESTORESTATE) {
  //  _tx_buffer_head = i;
  //  sbi(*_ucsrb, UDRIE0);
  //}
  TXBufferHead := i;
  UCSR0B := UCSR0B or (1 shl UDRIE0); //enable send interrupt
end;

procedure THardwareSerial.Flush;
begin
  // If we have never written a byte, no need to flush. This special
  // case is needed since there is no way to force the TXC (transmit
  // complete) bit to 1 during initialization
  if (not Written) then Exit;

  while (BitIsSet(@UCSR0B, UDRIE0) or BitIsClear(@UCSR0A, TXC0)) do
  begin
    if (BitIsClear(@SREG, SREG_I) and BitIsSet(@UCSR0A, UDRIE0)) then
 // Interrupts are globally disabled, but the DR empty
 // interrupt should be enabled, so poll the DR empty flag to
 // prevent deadlock
      if (BitIsSet(@UCSR0A, UDRE0)) then
        TXUDREmptyIRQ;
  end;
  // If we get here, nothing is queued anymore (DRIE is disabled) and
  // the hardware finished tranmission (TXC is set).
end;

function THardwareSerial.Available: byte;
begin
  Result := WORD(SERIAL_RX_BUFFER_SIZE + RXBufferHead - RXBufferTail) mod SERIAL_RX_BUFFER_SIZE;
end;

function THardwareSerial.AvailableForWrite: byte;
var
  head, tail: byte;
begin
(*  TX_BUFFER_ATOMIC {
    head = _tx_buffer_head;
    tail = _tx_buffer_tail;
  } *)

  head := TXBufferHead;
  tail := TXBufferTail;

  if (head >= tail) then
    Result := SERIAL_TX_BUFFER_SIZE - 1 - head + tail
  else
    Result := tail - head - 1;
end;

procedure THardwareSerial.Write(const aStr: shortstring);
var
  i: byte;
begin
  for i := 1 to Length(aStr) do
    WriteChar(aStr[i]);
end;

procedure THardwareSerial.WriteLn(const aStr: shortstring);
begin
  Write(aStr + #10);
end;

procedure THardwareSerial.WriteBuff(const aBuff: PUInt8; const aLen: UInt8);
var
  i: UInt8;
  P: PUInt8;
begin
  if aBuff = nil then Exit;

  P := aBuff;
  i := 0;
  while i < aLen do
  begin
    WriteChar(Chr(P^));
    Inc(P);
    Inc(i);
  end;
end;

function THardwareSerial.ReadBuff(const aBuff: PUInt8; const aLen: UInt8): boolean;
var
  b, i: UInt8;
  P: PUInt8;
begin
  Result:=true;

  if (aLen=0) then
  begin
    Result:=false;
    Exit;
  end;

  i:=aLen;
  P:=aBuff;
  while (Available>0) and (i>0) do
  begin
    // if the head isn't ahead of the tail, we don't have any characters
    if (RXBufferHead = RXBufferTail) then
    begin
      Result:=false;
      Break;
    end
    else
    begin
      b := RXBuffer[RXBufferTail];
      RXBufferTail := (RXBufferTail + 1) mod SERIAL_RX_BUFFER_SIZE;
      P^:=b;
      Dec(i);
      Inc(P);
    end;
  end;

  if Result then
    Result:=i=0;
end;

//_rx_complete_irq

procedure THardwareSerial.RXCompleteIRQ;
var
  b, i: byte;
begin
  if (UCSR0A and (1 shl UPE0)) = 0 then
  begin
    // No Parity error, read byte and store it in the buffer if there is
    // room
    b := UDR0;
    i := (RXBufferHead + 1) mod SERIAL_RX_BUFFER_SIZE;

    // if we should be storing the received character into the location
    // just before the tail (meaning that the head would advance to the
    // current location of the tail), we're about to overflow the buffer
    // and so we don't write the character or advance the head.
    if (i <> RXBufferTail) then
    begin
      RXBuffer[RXBufferHead] := b;
      RXBufferHead := i;
    end;
  end
  else
    // Parity error, read byte but discard it
    b := UDR0;
end;

procedure THardwareSerial.TXUDREmptyIRQ;
var
  b: byte;
begin
  // If interrupts are enabled, there must be more data in the output
  // buffer. Send the next byte
  b := TXBuffer[TXBufferTail];
  TXBufferTail := (TXBufferTail + 1) mod SERIAL_TX_BUFFER_SIZE;
  UDR0 := b;

  // clear the TXC bit -- "can be cleared by writing a one to its bit
  // location". This makes sure flush() won't return until the bytes
  // actually got written. Other r/w bits are preserved, and zeroes
  // written to the rest.
  UCSR0A := (UCSR0A and ((1 shl U2X0) or (1 shl MPCM0))) or (1 shl TXC0);

  if TXBufferTail = TXBufferHead then
    // Buffer empty, so disable interrupts
    UCSR0B := UCSR0B and not (1 shl UDRIE0);
end;

//interrupt handler that is called when a data is received using serial communication

procedure UART_RX_Receiving; public name 'USART__RX_ISR'; interrupt;
begin
  Serial.RXCompleteIRQ;
end;

//interrupt handler

procedure UART_UDRE_Send; public name 'USART__UDRE_ISR'; interrupt;
begin
  Serial.TXUDREmptyIRQ;
end;

end.

