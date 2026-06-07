program TestILI9163;

{
  ILI9163 TFT SPI 128x128 Display

  TTF   Arduino Uno
  --------------------

  LED      -> 3.3V
  SCK      -> D13
  SDA      -> D11
  A0       -> D8
  RESET    -> D9
  CS       -> D10
  GND      -> GND
  VCC      -> 3.3V

  written 2026 by ackarwow
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

//{$mode objfpc} Result, overloading etc. not needed

uses
  defs,
  timer, digital,
  spi;

{$i UnoLibPalette.inc} //BGR565 format
{$i UnoLibBitmap.inc}

const
  TFT_CS    = 10;
  TFT_DC    = 8;
  TFT_RESET = 9;

//after https://github.com/ccrause/fpc-avr/blob/33856a5abc210138f61e9c13f567a8f4ac7e1486/src/library/progmem.pas

function pgm_read_byte(const address: pointer): byte;  assembler;  nostackframe;
asm
  movw ZL, r24
  lpm r24, Z
end['r24', 'r25', 'r30', 'r31'];

function pgm_read_word(const address: pointer): UInt16; assembler; nostackframe;
asm
  movw ZL, r24
  lpm r24, Z+
  lpm r25, Z
end['r24', 'r25', 'r30', 'r31'];

procedure TFT_WriteCommand(Cmd: Byte);
begin
  DigitalWrite(TFT_DC, LOW);
  DigitalWrite(TFT_CS, LOW);
  _SPI.Transfer(Cmd);
  DigitalWrite(TFT_CS, HIGH);
end;

procedure TFT_WriteData(Data: Byte);
begin
  DigitalWrite(TFT_DC, HIGH);
  DigitalWrite(TFT_CS, LOW);
  _SPI.Transfer(Data);
  DigitalWrite(TFT_CS, HIGH);
end;

procedure TFT_FillScreen(Color: UInt16);
var
  i: UInt16;
begin
  DigitalWrite(TFT_CS, LOW);

  DigitalWrite(TFT_DC, LOW);

  // RAM write
  // After sending command $2C the controller expects
  // a continuous stream of pixel data.
  // CS must remain LOW during the whole transfer.
  _SPI.Transfer($2C); // RAMWR

  DigitalWrite(TFT_DC, HIGH);

  for i:=0 to 16383 do
  begin
    _SPI.Transfer(Color shr 8);
    _SPI.Transfer(Color and $FF);
  end;

  DigitalWrite(TFT_CS, HIGH);
end;

procedure TFT_DrawBitmap128;
var
  i: UInt16;
  idx: Byte;
  Color: UInt16;
begin
// Draw 128x128 indexed bitmap stored in program memory.
// Bitmap - 16384 bytes (1 byte per pixel)
// Palette - 256 entries (BGR565)
// Both tables are stored in .progmem and accessed
// through pgm_read_byte / pgm_read_word.

  DigitalWrite(TFT_CS, LOW);

  DigitalWrite(TFT_DC, LOW);

  // RAM write
  // After sending command $2C the controller expects
  // a continuous stream of pixel data.
  // CS must remain LOW during the whole transfer.
  _SPI.Transfer($2C); // RAMWR

  DigitalWrite(TFT_DC, HIGH);

  for i:=0 to 16383 do
  begin
    idx:=pgm_read_byte(@UnoLibBitmap[i]);
    Color:=pgm_read_word(@UnoLibPalette[idx]);

    _SPI.Transfer(Color shr 8);
    _SPI.Transfer(Color and $FF);
  end;

  DigitalWrite(TFT_CS, HIGH);
end;

begin
  _SPI._begin;
  _SPI.setBitOrder(MSBFIRST);
  _SPI.setDataMode(SPI_MODE0);
  _SPI.setClockDivider(SPI_CLOCK_DIV2);

  PinMode(TFT_CS, OUTPUT);
  PinMode(TFT_DC, OUTPUT);
  PinMode(TFT_RESET, OUTPUT);

  DigitalWrite(TFT_CS, HIGH);
  DigitalWrite(TFT_DC, HIGH);
  DigitalWrite(TFT_RESET, HIGH);

  // Hardware reset
  DigitalWrite(TFT_RESET, LOW);
  Delay(20);

  DigitalWrite(TFT_RESET, HIGH);
  Delay(120);

  // Software reset
  TFT_WriteCommand($01);
  Delay(150);

  // Exit sleep mode
  TFT_WriteCommand($11);
  Delay(150);

  // Pixel format = BGR 565 (16-bit)
  TFT_WriteCommand($3A);
  TFT_WriteData($05);
  Delay(10);

  // Display ON
  TFT_WriteCommand($29);
  Delay(150);

  // Column address range: 0..127
  TFT_WriteCommand($2A); // CASET
  TFT_WriteData(0);
  TFT_WriteData(0);
  TFT_WriteData(0);
  TFT_WriteData(127);

  // Row address range: 0..127
  TFT_WriteCommand($2B); // PASET
  TFT_WriteData(0);
  TFT_WriteData(0);
  TFT_WriteData(0);
  TFT_WriteData(127);

  //BGR565
  TFT_FillScreen($F800); //blue
  Delay(500);

  TFT_FillScreen($07E0); //green
  Delay(500);

  TFT_FillScreen($001F);  //red
  Delay(500);

  TFT_DrawBitmap128;

  while true do ;
end.

