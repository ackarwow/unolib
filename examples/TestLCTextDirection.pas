program TestLCTextDirection;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

uses
  timer, liquidcrystal;

var
  thischar: char = 'a';
begin
  LC.Init(12, 11, 5, 4, 3, 2);
  LC._begin(16, 2);
  LC.Cursor;

  while True do
  begin
    // reverse directions at 'm':
    if (thisChar = 'm') then
      // go right for the next letter
      LC.RightToLeft();

    // reverse again at 's':
    if (thisChar = 's') then
      // go left for the next letter
      LC.leftToRight();

    // reset at 'z':
    if (thisChar > 'z') then
    begin
      // go to (0,0):
      LC.home;
      // start again at 0
      thisChar:= 'a';
    end;

    // print the character
    LC.writechar(thisChar);
    // wait a second:
    Delay(1000);
    // increment the letter:
    Inc(thischar);
  end;
end.
