program ArduinoISP;

{
  ArduinoISP version 04m3
  ported to Pascal by ackarwow (2026)

  This program turns the Arduino into a AVRISP
  using the following arduino pins:

  pin name:
  slave reset: 10
  MOSI:        11
  MISO:        12
  SCK:         13

  Put an LED (with resistor) on the following pins:
  9: Heartbeat   - shows the programmer is running
  8: Error       - Lights up if something goes wrong (use red if that makes sense)
  7: Programming - In communication with the slave
}

{$IFDEF AVRPascal}
 {$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano))}
    {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
  {$ENDIF}
{$ELSE}
  {$IF NOT (DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
    {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
  {$ENDIF}
{$ENDIF}

{$mode objfpc} //for Result, overloading etc.

uses
  defs,
  timer,
  digital,
  analog,
  hardwareserial;

const
  RESET        = SS;

  LED_HB       = 9;
  LED_ERR      = 8;
  LED_PMODE    = 7;
  PROG_FLICKER = true;

  HWVER        = 2;
  SWMAJ        = 1;
  SWMIN        = 18;

  // STK Definitions
  STK_OK       = $10;
  STK_FAILED   = $11;
  STK_UNKNOWN  = $12;
  STK_INSYNC   = $14;
  STK_NOSYNC   = $15;
  CRC_EOP      = $20; //ok it is a space...

type
  TParameter = record
    DeviceCode : UInt8;
    Revision   : UInt8;
    ProgType   : UInt8;
    ParMode    : UInt8;
    Polling    : UInt8;
    SelfTimed  : UInt8;
    LockBytes  : UInt8;
    FuseBytes  : UInt8;

    FlashPoll  : UInt16;
    EEPROMPoll : UInt16;
    PageSize   : UInt16;
    EEPROMSize : UInt16;

    FlashSize  : UInt32;
  end;

var
  Error : Int16 = 0;
  PMode : Boolean = False;

  // address for reading and writing, set by 'U' command
  Here  : Uint16;
  Buff  : array[0..255] of UInt8;
  Param : TParameter;

function GetCh: UInt8;
begin
  while (Serial.Available=0) do;
  Result:=Serial.Read;
end;

procedure Fill(n: UInt16);
var
  x: UInt16;
begin
  // security
  if n > SizeOf(Buff) then
  begin
    Inc(Error);
    Exit;
  end;

  for x:= 0 to n-1 do
    Buff[x]:= GetCh;
end;

const
  PTIME = 30;

procedure Pulse(APin: UInt8; ATimes: Int16);
begin
  repeat
    DigitalWrite(APin, HIGH);
    Delay(PTIME);

    DigitalWrite(APin, LOW);
    Delay(PTIME);

    Dec(ATimes);
  until ATimes < 0;
end;

function BeGet16(P: PByte): UInt16;
begin
  //Result := UInt16(P^) shl 8 + PUInt8(PtrUInt(P)+1)^;
  Result := (UInt16(P[0]) shl 8) or P[1];
end;

// this provides a heartbeat on pin 9, so you can tell the software is running.
var
  hbval: UInt8 = 128;
  hbdelta: Int8 = 8;

procedure Heartbeat;
begin
  if (hbval > 192) then hbdelta:= -hbdelta;
  if (hbval < 32) then hbdelta:= -hbdelta;
  Inc(hbval, hbdelta);
  analogWrite(LED_HB, hbval);
  delay(20);
end;

procedure ProgLamp(state: UInt8);
begin
  if PROG_FLICKER then
    digitalWrite(LED_PMODE, state);
end;

procedure spi_init;
var
  x: UInt8;
begin
  SPCR:= $53;
  x:=SPSR;
  x:=SPDR;
end;

procedure spi_wait;
begin
  while (SPSR and (1 shl SPIF)) = 0 do;
end;

function spi_send(B: UInt8): UInt8;
begin
  SPDR:= B;
  SPI_Wait;
  Result:= SPDR;
end;

function spi_transaction(A, B, C, D : UInt8): UInt8;
var
  N: UInt8;
begin
  spi_send(A);
  N:= spi_send(B);
  // if N <> A then Error := -1;
  N:= spi_send(C);
  Result:= spi_send(D);
end;

procedure EmptyReply;
begin
  if CRC_EOP = getch then
  begin
    Serial.WriteChar(Chr(UInt8(STK_INSYNC)));
    Serial.WriteChar(Chr(UInt8(STK_OK)));
  end
  else
  begin
    Inc(Error);
    Serial.WriteChar(Chr(UInt8(STK_NOSYNC)));
  end;
end;

procedure BReply(B: UInt8);
begin
  if CRC_EOP = GetCh then
  begin
    Serial.WriteChar(Chr(UInt8(STK_INSYNC)));
    Serial.WriteChar(Chr(B));
    Serial.WriteChar(Chr(Uint8(STK_OK)));
  end
  else
  begin
    Inc(Error);
    Serial.WriteChar(Chr(UInt8(STK_NOSYNC)));
  end;
end;

procedure GetVersion(C : UInt8);
begin
  case C of
    $80: BReply(HWVER);
    $81: BReply(SWMAJ);
    $82: BReply(SWMIN);
    $93: BReply(Ord('S')); // serial programmer
  else
    BReply(0);
  end;
end;

procedure SetParameters;
begin
  Param.DeviceCode := Buff[0];
  Param.Revision   := Buff[1];
  Param.ProgType   := Buff[2];
  Param.ParMode    := Buff[3];
  Param.Polling    := Buff[4];
  Param.SelfTimed  := Buff[5];
  Param.LockBytes  := Buff[6];
  Param.FuseBytes  := Buff[7];

  Param.FlashPoll  := Buff[8];

  // Buff[9] ignored

  Param.EEPROMPoll := BeGet16(@Buff[10]);
  Param.PageSize   := BeGet16(@Buff[12]);
  Param.EEPROMSize := BeGet16(@Buff[14]);

  Param.FlashSize :=
      (LongWord(Buff[16]) shl 24)
    or (LongWord(Buff[17]) shl 16)
    or (LongWord(Buff[18]) shl  8)
    or  LongWord(Buff[19]);
end;

procedure StartPmode;
begin
  spi_init;

  // following delays may not work on all targets...

  PinMode(RESET, OUTPUT);
  DigitalWrite(RESET, HIGH);

  PinMode(SCK, OUTPUT);
  DigitalWrite(SCK, LOW);

  Delay(50);

  DigitalWrite(RESET, LOW);

  Delay(50);

  PinMode(MISO, INPUT);
  PinMode(MOSI, OUTPUT);

  spi_transaction($AC, $53, $00, $00);

  PMode := True;
end;

procedure EndPmode;
begin
  PinMode(MISO, INPUT);
  PinMode(MOSI, INPUT);
  PinMode(SCK, INPUT);
  PinMode(RESET, INPUT);

  PMode := False;
end;

procedure Universal;
var
  Ch : UInt8;
begin
  Fill(4);
  Ch := spi_transaction(Buff[0], Buff[1], Buff[2], Buff[3]);
  BReply(Ch);
end;

procedure Flash(Hilo : Byte; Addr : Word; Data : Byte);
begin
  SPI_Transaction(
    $40 + 8 * Hilo,
    (Addr shr 8) and $FF,
    Addr and $FF,
    Data);
end;

procedure Commit(Addr : Word);
begin
  if PROG_FLICKER then
    ProgLamp(LOW);

  spi_transaction($4C, (Addr shr 8) and $FF, Addr and $FF, 0);

  if PROG_FLICKER then
  begin
    Delay(PTIME);
    ProgLamp(HIGH);
  end;
end;

function CurrentPage(AAddr: UInt16): UInt16;
begin
  case Param.PageSize of
    32  : Result := AAddr and $FFF0;
    64  : Result := AAddr and $FFE0;
    128 : Result := AAddr and $FFC0;
    256 : Result := AAddr and $FF80;
  else
    Result := AAddr;
  end;
end;

function WriteFlashPages(ALength: UInt16): UInt8;
var
  X    : UInt16;
  Page : UInt16;
begin
  X := 0;
  Page := CurrentPage(Here);

  while X < ALength do
  begin
    if Page <> CurrentPage(Here) then
    begin
      Commit(Page);
      Page := CurrentPage(Here);
    end;

    Flash(LOW,  Here, Buff[X]);
    Inc(X);

    Flash(HIGH, Here, Buff[X]);
    Inc(X);

    Inc(Here);
  end;

  Commit(Page);

  Result := STK_OK;
end;

procedure WriteFlash(ALength: UInt16);
begin
  Fill(ALength);

  if CRC_EOP = GetCh then
  begin
    Serial.WriteChar(Chr(UInt8(STK_INSYNC)));
    Serial.WriteChar(Chr(WriteFlashPages(ALength)));
  end
  else
  begin
    Inc(Error);
    Serial.WriteChar(Chr(UInt8(STK_NOSYNC)));
  end;
end;

function WriteEEPROMChunk(StartAddr, ALength: UInt16): UInt8;
var
  X    : UInt16;
  Addr : UInt16;
begin
  Fill(ALength);

  ProgLamp(LOW);

  for X := 0 to ALength - 1 do
  begin
    Addr := StartAddr + X;

    SPI_Transaction(
      $C0,
      (Addr shr 8) and $FF,
      Addr and $FF,
      Buff[X]);

    Delay(45);
  end;

  ProgLamp(HIGH);

  Result := STK_OK;
end;

const
  EECHUNK = 32;

function WriteEEPROM(ALength: UInt16): UInt8;
var
  StartAddr : UInt16;
  Remaining : UInt16;
begin
  StartAddr := Here * 2;
  Remaining := ALength;

  if ALength > Param.EEPROMSize then
  begin
    Inc(Error);
    Exit(STK_FAILED);
  end;

  while Remaining > EECHUNK do
  begin
    WriteEEPROMChunk(StartAddr, EECHUNK);
    Inc(StartAddr, EECHUNK);
    Dec(Remaining, EECHUNK);
  end;

  WriteEEPROMChunk(StartAddr, Remaining);

  Result := STK_OK;
end;

procedure ProgramPage;
var
  MemType : UInt8;
  Length  : UInt16;
  ResultB : UInt8;
begin
  ResultB := STK_FAILED;

  Length := UInt16(GetCh) shl 8;
  Length := Length + GetCh;
  MemType := GetCh;

  if MemType = Ord('F') then
  begin
    WriteFlash(Length);
    Exit;
  end;

  if MemType = Ord('E') then
  begin
    ResultB := WriteEEPROM(Length);

    if CRC_EOP = GetCh then
    begin
      Serial.WriteChar(Chr(UInt8(STK_INSYNC)));
      Serial.WriteChar(Chr(ResultB));
    end
    else
    begin
      Inc(Error);
      Serial.WriteChar(Chr(UInt8(STK_NOSYNC)));
    end;

    Exit;
  end;

  Serial.WriteChar(Chr(UInt8(STK_FAILED)));
end;

function FlashRead(Hilo : UInt8; Addr : UInt16) : UInt8;
begin
  Result := SPI_Transaction($20 + Hilo * 8, (Addr shr 8) and $FF, Addr and $FF, 0);
end;

function FlashReadPage(ALength : UInt16) : UInt8;
var
  X    : UInt16;
  LowB : UInt8;
  HighB: UInt8;
begin
  X := 0;

  while X < ALength do
  begin
    LowB := FlashRead(LOW, Here);
    Serial.WriteChar(Chr(LowB));

    HighB := FlashRead(HIGH, Here);
    Serial.WriteChar(Chr(HighB));

    Inc(Here);
    Inc(X, 2);
  end;

  Result := STK_OK;
end;

function EEPROMReadPage(ALength : UInt16) : UInt8;
var
  StartAddr : UInt16;
  Addr      : UInt16;
  X         : UInt16;
  EE        : UInt8;
begin
  StartAddr := Here * 2;

  for X := 0 to ALength - 1 do
  begin
    Addr := StartAddr + X;

    EE := SPI_Transaction(
            $A0,
            (Addr shr 8) and $FF,
            Addr and $FF,
            $FF);

    Serial.WriteChar(Chr(EE));
  end;

  Result := STK_OK;
end;

procedure ReadPage;
var
  MemType : UInt8;
  Length  : UInt16;
  ResultB : UInt8;
begin
  ResultB := STK_FAILED;

  Length := UInt16(GetCh) shl 8;
  Length := Length + GetCh;

  MemType := GetCh;

  if CRC_EOP <> GetCh then
  begin
    Inc(Error);
    Serial.WriteChar(Chr(UInt8(STK_NOSYNC)));
    Exit;
  end;

  Serial.Write(Chr(UInt8(STK_INSYNC)));

  if MemType = Ord('F') then
    ResultB := FlashReadPage(Length);

  if MemType = Ord('E') then
    ResultB := EEPROMReadPage(Length);

  Serial.WriteChar(Chr(ResultB));
end;

procedure ReadSignature;
var
  HighB   : UInt8;
  MiddleB : UInt8;
  LowB    : UInt8;
begin
  if CRC_EOP <> GetCh then
  begin
    Inc(Error);
    Serial.WriteChar(Chr(UInt8(STK_NOSYNC)));
    Exit;
  end;

  Serial.WriteChar(Chr(UInt8(STK_INSYNC)));

  HighB := SPI_Transaction($30, $00, $00, $00);
  Serial.WriteChar(Chr(HighB));

  MiddleB := SPI_Transaction($30, $00, $01, $00);
  Serial.WriteChar(Chr(MiddleB));

  LowB := SPI_Transaction($30, $00, $02, $00);
  Serial.WriteChar(Chr(LowB));

  Serial.WriteChar(Chr(STK_OK));
end;

//STK500v1
procedure AVRISP;
const
  aBuff: array[0..7] of char='AVR ISP';
var
  Ch    : UInt8;
  Data  : UInt8;
  LowB  : UInt8;
  HighB : UInt8;
begin
  Ch := GetCh;

  case Ch of

    Ord('0'): begin
      Error := 0;
      EmptyReply;
    end;

    Ord('1'): begin
      if GetCh = CRC_EOP then
      begin
        Serial.WriteChar(Chr(UInt8(STK_INSYNC)));
        //Serial.Write('AVR ISP');
        Serial.WriteBuff(PUInt8(@aBuff[0]), 7);
        Serial.WriteChar(Chr(UInt8(STK_OK)));
      end;
    end;

    Ord('A'):
      GetVersion(GetCh);

    Ord('B'): begin
      Fill(20);
      SetParameters;
      EmptyReply;
    end;

    Ord('E'): begin
      Fill(5);
      EmptyReply;
    end;

    Ord('P'): begin
      StartPMode;
      EmptyReply;
    end;

    Ord('U'): begin
      Here := GetCh;
      Here := Here + (Word(GetCh) shl 8);
      EmptyReply;
    end;

    $60: begin
      LowB  := GetCh;
      HighB := GetCh;
      EmptyReply;
    end;

    $61: begin
      Data := GetCh;
      EmptyReply;
    end;

    $64:
      ProgramPage;

    $74:
      ReadPage;

    Ord('V'):
      Universal;

    Ord('Q'): begin
      Error := 0;
      EndPMode;
      EmptyReply;
    end;

    $75:
      ReadSignature;

    CRC_EOP: begin
      Inc(Error);
      Serial.WriteChar(Chr(UInt8(STK_NOSYNC)));
    end;

  else
    begin
      Inc(Error);

      if GetCh = CRC_EOP then
        Serial.WriteChar(Chr(UInt8(STK_UNKNOWN)))
      else
        Serial.WriteChar(Chr(UInt8(STK_NOSYNC)));
    end;
  end;
end;

begin
  //setup
  Serial.Start(19200);
  pinMode(LED_PMODE, OUTPUT);
  pulse(LED_PMODE, 2);
  pinMode(LED_ERR, OUTPUT);
  pulse(LED_ERR, 2);
  pinMode(LED_HB, OUTPUT);
  pulse(LED_HB, 2);

  //loop
  while true do
  begin
    // is pmode active?
    if pmode then
      digitalWrite(LED_PMODE, HIGH)
    else
      digitalWrite(LED_PMODE, LOW);
    // is there an error?
    if (error>0) then
      digitalWrite(LED_ERR, HIGH)
    else
      digitalWrite(LED_ERR, LOW);

    // light the heartbeat LED
    heartbeat;
    if (Serial.Available>0) then
      AVRISP;
  end;
end.

