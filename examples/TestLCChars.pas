program TestLCChars;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

uses
  timer, defs, liquidcrystal;

const
  bell: array[0..7] of UInt8 = ($4, $e, $e, $e, $1f, $0, $4, $0);
  note: array[0..7] of UInt8 = ($2, $3, $2, $e, $1e, $c, $0, $0);
  clock: array[0..7] of UInt8 = ($0, $e, $15, $17, $11, $e, $0, $0);
  heart: array[0..7] of UInt8 = ($0, $a, $1f, $1f, $e, $4, $0, $0);
  duck: array[0..7] of UInt8  = ($0, $c, $1d, $f, $f, $6, $0, $0);
  check: array[0..7] of UInt8 = ($0, $1 ,$3, $16, $1c, $8, $0, $0);
  cross: array[0..7] of UInt8 = ($0, $1b, $e, $4, $e, $1b, $0, $0);
  retarrow: array[0..7] of UInt8 = ($1, $1, $5, $9, $1f, $8, $4, $0);

// display all keycodes
procedure DisplayKeyCodes;
var
  i, j: UInt8;
  s: shortstring;
begin
  i:= 0;

  while (true) do
  begin
    LC.Clear();
    LC.Write('Codes $');
    s:=ByteToHex(i);
    LC.Write(s);
    LC.Write('-$');
    s:=ByteToHex(i+16);
    LC.Write(s);
    LC.SetCursor(0, 1);

    for j:= 0 to 15 do
      LC.Write(Chr(i+j));
    Inc(i, 16);

    delay(4000);
  end;
end;

begin
  LC.Init(12, 11, 5, 4, 3, 2);
  LC._begin(16, 2);

  LC.CreateChar(0, @bell);
  LC.CreateChar(1, @note);
  LC.createChar(2, @clock);
  LC.createChar(3, @heart);
  LC.createChar(4, @duck);
  LC.createChar(5, @check);
  LC.createChar(6, @cross);
  LC.createChar(7, @retarrow);
  LC.Home;

  LC.Write('Hello world...');
  LC.SetCursor(0, 1);
  LC.Write(' i ');
  LC.WriteChar(#3);
  LC.Write(' arduinos!');
  Delay(5000);
  DisplayKeyCodes();

  while True do;
end.
