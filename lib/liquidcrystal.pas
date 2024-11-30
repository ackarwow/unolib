unit liquidcrystal;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

{
  Based on Arduino library source
  ported to Pascal by Andrzej Karwowski 2021
}

interface

uses
  defs;

// When the display powers up, it is configured as follows:
//
// 1. Display clear
// 2. Function set:
//    DL = 1; 8-bit interface data
//    N = 0; 1-line display
//    F = 0; 5x8 dot character font
// 3. Display on/off control:
//    D = 0; Display off
//    C = 0; Cursor off
//    B = 0; Blinking off
// 4. Entry mode set:
//    I/D = 1; Increment by 1
//    S = 0; No shift
//
// Note, however, that resetting the Arduino doesn't reset the LCD, so we
// can't assume that its in that state when a sketch starts (and the
// LiquidCrystal constructor is called).

const
  // commands
  LCD_CLEARDISPLAY = $01;
  LCD_RETURNHOME = $02;
  LCD_ENTRYMODESET = $04;
  LCD_DISPLAYCONTROL = $08;
  LCD_CURSORSHIFT = $10;
  LCD_FUNCTIONSET = $20;
  LCD_SETCGRAMADDR = $40;
  LCD_SETDDRAMADDR = $80;

  // flags for display entry mode
  LCD_ENTRYRIGHT = $00;
  LCD_ENTRYLEFT = $02;
  LCD_ENTRYSHIFTINCREMENT = $01;
  LCD_ENTRYSHIFTDECREMENT = $00;

  // flags for display on/off control
  LCD_DISPLAYON = $04;
  LCD_DISPLAYOFF = $00;
  LCD_CURSORON = $02;
  LCD_CURSOROFF = $00;
  LCD_BLINKON = $01;
  LCD_BLINKOFF = $00;

  // flags for display/cursor shift
  LCD_DISPLAYMOVE = $08;
  LCD_CURSORMOVE = $00;
  LCD_MOVERIGHT = $04;
  LCD_MOVELEFT = $00;

  // flags for function set
  LCD_8BITMODE = $10;
  LCD_4BITMODE = $00;
  LCD_2LINE = $08;
  LCD_1LINE = $00;
  LCD_5x10DOTS = $04;
  LCD_5x8DOTS = $00;

type
  TLiquidCrystal=object
  private
    _rs_pin: UInt8; // LOW: command.  HIGH: character.
    _rw_pin: UInt8; // LOW: write to LCD.  HIGH: read from LCD.
    _enable_pin: UInt8; // activated by a HIGH pulse.
    _data_pins: array [0..7] of UInt8;

    _displayfunction: UInt8;
    _displaycontrol: UInt8;
    _displaymode: UInt8;

    //_initialized: UInt8;

    _numlines: UInt8;
    _row_offsets: array [0..3] of UInt8;
    procedure Write4bits(value: UInt8);
    procedure Write8Bits(value: UInt8);
    procedure PulseEnable;
    procedure Send(value, mode: UInt8);
  public
    procedure Init(rs, rw, enable, d0, d1, d2, d3, d4, d5, d6, d7: UInt8); overload;
    procedure Init(rs, enable, d0, d1, d2, d3, d4, d5, d6, d7: UInt8); overload;
    procedure Init(rs, rw, enable, d0, d1, d2, d3: UInt8); overload;
    procedure Init(rs, enable, d0, d1, d2, d3: UInt8); overload;

    procedure _init(fourbitmode, rs, rw, enable, d0, d1, d2, d3, d4, d5, d6, d7: UInt8);
    procedure _begin(cols, rows: UInt8; charsize: UInt8 = LCD_5x8DOTS);

    procedure Clear;
    procedure Home;

    procedure NoDisplay;
    procedure Display;
    procedure NoBlink;
    procedure Blink;
    procedure NoCursor;
    procedure Cursor;
    procedure ScrollDisplayLeft;
    procedure ScrollDisplayRight;
    procedure LeftToRight;
    procedure RightToLeft;
    procedure Autoscroll;
    procedure NoAutoscroll;

    procedure SetRowOffsets(row0, row1, row2, row3: UInt8);
    procedure CreateChar(location: UInt8; charmapptr: UInt8P0);
    procedure SetCursor(col, row: UInt8);
    procedure Command(value: UInt8);

    procedure WriteChar(aChar: char);
    procedure Write(aStr: shortstring);
  end;

var
  LC: TLiquidCrystal;

implementation

uses
  digital, timer;

procedure TLiquidCrystal._init(fourbitmode, rs, rw, enable, d0, d1, d2, d3, d4, d5, d6, d7: UInt8);
begin
  _rs_pin:= rs;
  _rw_pin:= rw;
  _enable_pin:= enable;

  _data_pins[0]:= d0;
  _data_pins[1]:= d1;
  _data_pins[2]:= d2;
  _data_pins[3]:= d3;
  _data_pins[4]:= d4;
  _data_pins[5]:= d5;
  _data_pins[6]:= d6;
  _data_pins[7]:= d7;

  if (fourbitmode>0) then
    _displayfunction:= LCD_4BITMODE or LCD_1LINE or LCD_5x8DOTS
  else
    _displayfunction:= LCD_8BITMODE or LCD_1LINE or LCD_5x8DOTS;

  _begin(16, 1);
end;

procedure TLiquidCrystal.Init(rs, rw, enable, d0, d1, d2, d3, d4, d5, d6, d7: UInt8);
begin
  _init(0, rs, rw, enable, d0, d1, d2, d3, d4, d5, d6, d7);
end;

procedure TLiquidCrystal.Init(rs, enable, d0, d1, d2, d3, d4, d5, d6, d7: UInt8);
begin
  _init(0, rs, 255, enable, d0, d1, d2, d3, d4, d5, d6, d7);
end;

procedure TLiquidCrystal.Init(rs, rw, enable, d0, d1, d2, d3: UInt8);
begin
  _init(1, rs, rw, enable, d0, d1, d2, d3, 0, 0, 0, 0);
end;

procedure TLiquidCrystal.Init(rs, enable, d0, d1, d2, d3: UInt8);
begin
  _init(1, rs, 255, enable, d0, d1, d2, d3, 0, 0, 0, 0);
end;

procedure TLiquidCrystal._begin(cols, rows: UInt8; charsize: UInt8 = LCD_5x8DOTS);
var
  i, imax: UInt8;
begin
  if (rows > 1) then
    _displayfunction:= _displayfunction or LCD_2LINE;

  _numlines:= rows;

  SetRowOffsets($00, $40, $00 + cols, $40 + cols);

  // for some 1 line displays you can select a 10 pixel high font
  if ((charsize <> LCD_5x8DOTS) and (rows = 1)) then
    _displayfunction:= _displayfunction or LCD_5x10DOTS;

  PinMode(_rs_pin, OUTPUT);
  // we can save 1 pin by not using RW. Indicate by passing 255 instead of pin#
  if (_rw_pin <> 255)  then
    pinMode(_rw_pin, OUTPUT);

  PinMode(_enable_pin, OUTPUT);

  // Do these once, instead of every time a character is drawn for speed reasons.

  if (_displayfunction and LCD_8BITMODE)>0 then
    iMax:=8
  else
    iMax:=4;

  for i:=0 to iMax-1 do
    PinMode(_data_pins[i], OUTPUT);

  // SEE PAGE 45/46 FOR INITIALIZATION SPECIFICATION!
  // according to datasheet, we need at least 40ms after power rises above 2.7V
  // before sending commands. Arduino can turn on way before 4.5V so we'll wait 50
  DelayMicroseconds(50000); //<-TO VERIFY!
  // Now we pull both RS and R/W low to begin commands
  DigitalWrite(_rs_pin, LOW);
  DigitalWrite(_enable_pin, LOW);
  if (_rw_pin <> 255) then
    DigitalWrite(_rw_pin, LOW);

  //put the LCD into 4 bit or 8 bit mode
  if not ((_displayfunction and LCD_8BITMODE)>0)  then
  begin
    // this is according to the hitachi HD44780 datasheet
    // figure 24, pg 46

    // we start in 8bit mode, try to set 4 bit mode
    Write4bits($03);
    DelayMicroseconds(4500); // wait min 4.1ms

    // second try
    write4bits($03);
    DelayMicroseconds(4500); // wait min 4.1ms

    // third go!
    Write4bits($03);
    DelayMicroseconds(150);

    // finally, set to 4-bit interface
    Write4bits($02);
  end
  else
  begin
    // this is according to the hitachi HD44780 datasheet
    // page 45 figure 23

    // Send function set command sequence
    Command(LCD_FUNCTIONSET or _displayfunction);
    DelayMicroseconds(4500);  // wait more than 4.1ms

    // second try
    Command(LCD_FUNCTIONSET or _displayfunction);
    DelayMicroseconds(150);

    // third go
    Command(LCD_FUNCTIONSET or _displayfunction);
  end;

  // finally, set # lines, font size, etc.
  Command(LCD_FUNCTIONSET or _displayfunction);

  // turn the display on with no cursor or blinking default
  _displaycontrol:= LCD_DISPLAYON or LCD_CURSOROFF or LCD_BLINKOFF;
  Display;

  // clear it off
  Clear;

  // Initialize to default text direction (for romance languages)
  _displaymode:= LCD_ENTRYLEFT or LCD_ENTRYSHIFTDECREMENT;
  // set the entry mode
  Command(LCD_ENTRYMODESET or _displaymode);
end;

procedure TLiquidCrystal.SetRowOffsets(row0, row1, row2, row3: UInt8);
begin
  _row_offsets[0]:= row0;
  _row_offsets[1]:= row1;
  _row_offsets[2]:= row2;
  _row_offsets[3]:= row3;
end;

procedure TLiquidCrystal.CreateChar(location: UInt8; charmapptr: UInt8P0);
var
  i: UInt8;
begin
  location:=location and  $7; // we only have 8 locations 0-7
  command(LCD_SETCGRAMADDR or (location shl 3));
  for i:=0 to 7 do
    WriteChar(Chr(charmapptr^[i]));
end;

procedure TLiquidCrystal.SetCursor(col, row: UInt8);
var
  max_lines: UInt8;//size_t
begin
  max_lines:=system.high(_row_offsets)-system.low(_row_offsets);
//  max_lines:= SizeOf(_row_offsets) div SizeOf({*_row_offsets}UInt8);
  if ( row >= max_lines ) then
    row:= max_lines - 1;    // we count rows starting w/0
  if ( row >= _numlines ) then
    row:= _numlines - 1;    // we count rows starting w/0

  Command(LCD_SETDDRAMADDR or (col + _row_offsets[row]));
end;

procedure TLiquidCrystal.Write4Bits(value: UInt8);
var
  i: UInt8;
begin
  for i:= 0 to 3 do
    DigitalWrite(_data_pins[i], (value shr i) and $01);

  PulseEnable;
end;

procedure TLiquidCrystal.Write8Bits(value: UInt8);
var
  i: UInt8;
begin
  for i:=0 to 7 do
    DigitalWrite(_data_pins[i], (value shr i) and $01);

  PulseEnable();
end;

procedure TLiquidCrystal.Command(value: UInt8);
begin
  Send(value, LOW);
end;

procedure TLiquidCrystal.Write(aStr: shortstring);
var
  i : byte;
begin
  for i := 1 to Length(aStr) do
    WriteChar(aStr[i]);
end;

procedure TLiquidCrystal.Display;
begin
  _displaycontrol:=_displaycontrol or LCD_DISPLAYON;
  Command(LCD_DISPLAYCONTROL or _displaycontrol);
end;

procedure TLiquidCrystal.NoBlink;
begin
  _displaycontrol:=_displaycontrol and not LCD_BLINKON;
  Command(LCD_DISPLAYCONTROL or _displaycontrol);
end;

procedure TLiquidCrystal.Blink;
begin
  _displaycontrol:=_displaycontrol or LCD_BLINKON;
  Command(LCD_DISPLAYCONTROL or _displaycontrol);
end;

procedure TLiquidCrystal.NoCursor;
begin
  _displaycontrol:=_displaycontrol and not LCD_CURSORON;
  Command(LCD_DISPLAYCONTROL or _displaycontrol);
end;

procedure TLiquidCrystal.Cursor;
begin
  _displaycontrol:=_displaycontrol or LCD_CURSORON;
  Command(LCD_DISPLAYCONTROL or _displaycontrol);
end;

procedure TLiquidCrystal.ScrollDisplayLeft;
begin
  Command(LCD_CURSORSHIFT or LCD_DISPLAYMOVE or LCD_MOVELEFT);
end;

procedure TLiquidCrystal.ScrollDisplayRight;
begin
  Command(LCD_CURSORSHIFT or LCD_DISPLAYMOVE or LCD_MOVERIGHT);
end;

// This is for text that flows Left to Right
procedure TLiquidCrystal.LeftToRight;
begin
  _displaymode:=_displaymode or LCD_ENTRYLEFT;
  Command(LCD_ENTRYMODESET or _displaymode);
end;

// This is for text that flows Right to Left
procedure TLiquidCrystal.RightToLeft;
begin
  _displaymode:=_displaymode and not LCD_ENTRYLEFT;
  Command(LCD_ENTRYMODESET or _displaymode);
end;

procedure TLiquidCrystal.Autoscroll;
begin
  _displaymode:=_displaymode or LCD_ENTRYSHIFTINCREMENT;
  Command(LCD_ENTRYMODESET or _displaymode);
end;

procedure TLiquidCrystal.NoAutoscroll;
begin
  _displaymode:=_displaymode and not LCD_ENTRYSHIFTINCREMENT;
  Command(LCD_ENTRYMODESET or _displaymode);
end;

procedure TLiquidCrystal.Clear;
begin
  Command(LCD_CLEARDISPLAY);  // clear display, set cursor position to zero
  DelayMicroseconds(2000);  // this command takes a long time!
end;

procedure TLiquidCrystal.Home;
begin
  Command(LCD_RETURNHOME);  // set cursor position to zero
  DelayMicroseconds(2000);  // this command takes a long time!
end;

procedure TLiquidCrystal.NoDisplay;
begin
  _displaycontrol:=_displaycontrol and not LCD_DISPLAYON;
  Command(LCD_DISPLAYCONTROL or _displaycontrol);
end;

// write either command or data, with automatic 4/8-bit selection
procedure TLiquidCrystal.Send(value, mode: UInt8);
begin
  DigitalWrite(_rs_pin, mode);

  // if there is a RW pin indicated, set it low to Write
  if (_rw_pin <> 255) then
    DigitalWrite(_rw_pin, LOW);

  if (_displayfunction and LCD_8BITMODE)>0 then
    Write8Bits(value)
  else
  begin
    write4bits(value shr 4);
    write4bits(value);
  end;
end;

procedure TLiquidCrystal.WriteChar(aChar: char);
begin
  Send(UInt8(aChar), HIGH);
end;

procedure TLiquidCrystal.PulseEnable;
begin
  DigitalWrite(_enable_pin, LOW);
  DelayMicroseconds(1);
  DigitalWrite(_enable_pin, HIGH);
  DelayMicroseconds(1);    // enable pulse must be >450ns
  DigitalWrite(_enable_pin, LOW);
  DelayMicroseconds(100);   // commands need > 37us to settle
end;

end.

