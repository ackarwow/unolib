unit stringutils;

// ***********************************************
// ***** Some functions to convert to String *****
// ***** By Dzandaa ******************************
// ***********************************************

//{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
// {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
//{$ENDIF}

{$mode objfpc}
interface

function UInt8ToHexString(Val: UInt8): String;
function UInt16toHexString(Val: UInt16): String;

function UInt8ToString(I8: UInt8): String;
function Int8ToString(I8: Int8): String;
function UInt16ToString(I16: Int16): String;
function Int16ToString(I16: Int16): String;

//by @ackarwow
function UInt32Digits(aVal: UInt32): UInt8;
function UInt16Digits(aVal: UInt16): UInt8;

function UInt16ToStr(const s: PChar; const maxlen, digits: UInt8; const Val: UInt16): UInt8;
function Int16ToStr(const s: PChar; const maxlen, digits: UInt8; const Val: Int16): UInt8;

const
  UInt16DigitsArray: array[0..4] of UInt16=(1, 10, 100, 1000, 10000);
  UInt32DigitsArray: array[0..9] of UInt32=(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000);

implementation

// ********************************
// ***** UInt8 to Hexa String *****
// ********************************
function  UInt8ToHexString(Val: UInt8): String;
var
	HexTab: Array [0..15] of char = '0123456789ABCDEF';
	Str: String;
begin
	Str := HexTab[Val shr 4] + HexTab[Val and $0f];
	exit(Str);
end;

// *********************************
// ***** UInt16 to Hexa String *****
// *********************************
function UInt16toHexString(Val: UInt16): String;
var
	Str: String;
begin
	Str := UInt8ToHexString(UInt8(Val >> 8));
	Str := Str + UInt8ToHexString(UInt8(Val and $ff));
	exit(Str);
end;

// ***************************
// ***** UInt8 to String *****
// ***************************
function UInt8ToString(I8: UInt8): String;
var
	Valt, Valr: UInt16;
	Res: String;
begin

if(I8 = 0) then exit('0');
	Res := '';
	Valt := UINT16(I8);
	While Valt <> 0 do
	begin
		Valr := Valt Div 10;
		Res := char((Valt - (Valr * 10)) + $30) + Res;
		Valt := Valr;
	end;
	exit(Res);
end;

// **************************
// ***** Int8 to String *****
// **************************
function Int8ToString(I8: Int8): String;
var
	Valt, Valr: UInt16;
	Res, Sign : String;
begin
if(I8 < 0) then
begin
	Sign := '-';
	I8 := I8 and $7F;
	I8 := $7F - I8;
	end
	else Sign := '';
	if(I8 = 0) then exit('0');

	Res := '';
	Valt := UINT16(I8);
	While Valt <> 0 do
	begin
		Valr := Valt Div 10;
		Res := char((Valt - (Valr * 10)) + $30) + Res;
		Valt := Valr;
	end;
	Res := Sign + Res;
	exit(Res);
end;

// ****************************
// ***** UInt16 to String *****
// ****************************
function UInt16ToString(I16: Int16): String;
var
	Valt, Valr: UInt32;
	Res: String;
begin
	if(I16 = 0) then exit('0');
	Res := '';
	Valt := UINT32(I16);
	While Valt <> 0 do
	begin
		Valr := Valt Div 10;
		Res := char((Valt - (Valr * 10)) + $30) + Res;
		Valt := Valr;
	end;

	exit(Res);
end;

// ***************************
// ***** Int16 to String *****
// ***************************
function Int16ToString(I16: Int16): String;
var
	Valt, Valr: UInt32;
	Res, Sign: String;
begin

	if(I16 < 0) then
	begin
		Sign := '-';
		I16 := I16 and $7FFF; // Mask Sign
		I16 := $7FFF - I16;
	end
	else Sign := '';
	if(I16 = 0) then exit('0');

	Res := '';
	Valt := UINT32(I16);
	While Valt <> 0 do
	begin
		Valr := Valt Div 10;
		Res := char((Valt - (Valr * 10)) + $30) + Res;
		Valt := Valr;
	end;
	Res := Sign + Res;
	exit(Res);
end;

function UInt16Digits(aVal: UInt16): UInt8; inline;
begin
  if (aVal < UInt16DigitsArray[1]) then Result := 1 else
  if (aVal < UInt16DigitsArray[2]) then Result := 2 else
  if (aVal < UInt16DigitsArray[3]) then Result := 3 else
  if (aVal < UInt16DigitsArray[4]) then Result := 4 else
    Result := 5;
end;

function UInt32Digits(aVal: UInt32): UInt8; inline;
begin
  if (aVal < UInt32DigitsArray[1]) then Result := 1 else
  if (aVal < UInt32DigitsArray[2]) then Result := 2 else
  if (aVal < UInt32DigitsArray[3]) then Result := 3 else
  if (aVal < UInt32DigitsArray[4]) then Result := 4 else
  if (aVal < UInt32DigitsArray[5]) then Result := 5 else
  if (aVal < UInt32DigitsArray[6]) then Result := 6 else
  if (aVal < UInt32DigitsArray[7]) then Result := 7 else
  if (aVal < UInt32DigitsArray[8]) then Result := 8 else
  if (aVal < UInt32DigitsArray[9]) then Result := 9 else
    Result := 10;
end;

function UInt16ToStr(const s: PChar; const maxlen, digits: UInt8; const Val: UInt16): UInt8;
var
  i: UInt16;
  b: UInt8;
  P, PTmp: PChar;
  LeadingDigits: Int8;
  IntDigits, OutLen: UInt8;
begin
  OutLen:=0;

  IntDigits:=UInt16Digits(Val);

  if maxlen<(IntDigits+1) then //buffer to small, 1 for null terminator
    Exit(OutLen);

  if digits>=maxlen then //buffer to small
    Exit(OutLen);

  P:=s;

  LeadingDigits:=Digits-IntDigits;
  while LeadingDigits>0 do
  begin
    P^:='0';
    Inc(P);
    Inc(OutLen);
    Dec(LeadingDigits);
  end;

  i := Abs(Val);
  if i=0 then
  begin
    P^:= '0';
    Inc(P);
    Inc(OutLen);
  end
  else
  begin
    PTmp:=P;
    Inc(PTmp, IntDigits);
    while (i > 0) do
    begin
      Dec(PTmp);
      b := (i mod 10) + $30;
      PTmp^:= Chr(b);
      i := i div 10;
    end;
    Inc(OutLen, IntDigits);
    Inc(P, IntDigits);
  end;

  P^:= #0;

  Result:=OutLen;
end;

function Int16ToStr(const s: PChar; const maxlen, digits: UInt8; const Val: Int16): UInt8;
var
  i: Int16;
  b: UInt8;
  P, PTmp: PChar;
  LeadingDigits: Int8;
  IntDigits, SgnDigit, OutLen: UInt8;
begin
  OutLen:=0;

  IntDigits:=UInt16Digits(Abs(Val));
  SgnDigit:=0;
  if Val<0 then
    Inc(SgnDigit);

  if maxlen<(IntDigits+SgnDigit+1) then //buffer to small, 1 for null terminator
    Exit(OutLen);

  if digits>=maxlen then //buffer to small
    Exit(OutLen);

  P:=s;

  if SgnDigit>0 then
  begin
    P^:='-';
    Inc(P);
    Inc(OutLen);
  end;

  LeadingDigits:=Digits-IntDigits;
  while LeadingDigits>0 do
  begin
    P^:='0';
    Inc(P);
    Inc(OutLen);
    Dec(LeadingDigits);
  end;

  i := Abs(Val);
  if i=0 then
  begin
    P^:= '0';
    Inc(P);
    Inc(OutLen);
  end
  else
  begin
    PTmp:=P;
    Inc(PTmp, IntDigits);
    while (i > 0) do
    begin
      Dec(PTmp);
      b := (i mod 10) + $30;
      PTmp^:= Chr(b);
      i := i div 10;
    end;
    Inc(OutLen, IntDigits);
    Inc(P, IntDigits);
  end;

  P^:= #0;

  Result:=OutLen;
end;

end.
