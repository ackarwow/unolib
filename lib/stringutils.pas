unit stringutils;

// ***********************************************
// ***** Some functions to convert to String *****
// ***** By Dzandaa ******************************
// ****  Extended and optimized by ackarwow ******
// ***********************************************

//no blocking directive needed

{$mode objfpc}

interface

type
  TIntString = string[11];

  {TInt8String  = string[4];
  TInt16String = string[6];
  TInt32String = string[11];

  TUInt8String  = string[3];
  TUInt16String = string[5];
  TUInt32String = string[10];}

function UInt8ToHexString(const Val: UInt8): TIntString;
function UInt16ToHexString(Val: UInt16): TIntString;

function UInt8ToString(Val: UInt8): TIntString;
function Int8ToString(Val: Int8): TIntString;
function UInt16ToString(Val: UInt16): TIntString;
function Int16ToString(Val: Int16): TIntString;
function UInt32ToString(Val: UInt32): TIntString;
function Int32ToString(Val: Int32): TIntString;

//by @ackarwow
function UInt32Digits(aVal: UInt32): UInt8; inline;
function UInt16Digits(aVal: UInt16): UInt8; inline;
function UInt8Digits(aVal: UInt8): UInt8; inline;

function UInt32ToStr(const s: PChar; const maxlen, digits: UInt8; const Val: UInt32): UInt8;
function Int32ToStr(const s: PChar; const maxlen, digits: UInt8; const Val: Int32): UInt8;

function UInt16ToStr(const s: PChar; const maxlen, digits: UInt8; const Val: UInt16): UInt8;
function Int16ToStr(const s: PChar; const maxlen, digits: UInt8; const Val: Int16): UInt8;

function UInt8ToStr(const s: PChar; const maxlen, digits: UInt8; const Val: UInt8): UInt8;
function Int8ToStr(const s: PChar; const maxlen, digits: UInt8; const Val: Int8): UInt8;

function UInt8ToHexStr(const s: PChar; const maxlen, digits: UInt8; const Val: UInt8): UInt8;
function UInt16ToHexStr(const s: PChar; const maxlen, digits: UInt8; const Val: UInt16): UInt8;


const
  UInt8DigitsArray: array[0..2] of UInt16=(1, 10, 100);
  UInt16DigitsArray: array[0..4] of UInt16=(1, 10, 100, 1000, 10000);
  UInt32DigitsArray: array[0..9] of UInt32=(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000);

implementation

const
	HexTab: array [0..15] of char = '0123456789ABCDEF';

// ********************************
// ***** UInt8 to Hexa String *****
// ********************************
function  UInt8ToHexString(const Val: UInt8): TIntString;
var
  Buff: array[0..2] of char;
begin
  UInt8ToHexStr(Buff, Sizeof(Buff), 0, Val);
  Result:=Buff;
end;

// *********************************
// ***** UInt16 to Hexa String *****
// *********************************
function UInt16ToHexString(Val: UInt16): TIntString;
var
  Buff: array[0..4] of char;
begin
  UInt16ToHexStr(Buff, Sizeof(Buff), 0, Val);
  Result:=Buff;
end;

// ***************************
// ***** UInt8 to String *****
// ***************************
function UInt8ToString(Val: UInt8): TIntString;
var
  Buff: array[0..3] of char;
begin
  UInt8ToStr(Buff, Sizeof(Buff), 0, Val);
  Result:=Buff;
end;

// **************************
// ***** Int8 to String *****
// **************************
function Int8ToString(Val: Int8): TIntString;
var
  Buff: array[0..4] of char;
begin
  Int8ToStr(Buff, Sizeof(Buff), 0, Val);
  Result:=Buff;
end;

// ****************************
// ***** UInt16 to String *****
// ****************************
function UInt16ToString(Val: UInt16): TIntString;
var
  Buff: array[0..5] of char;
begin
  UInt16ToStr(Buff, Sizeof(Buff), 0, Val);
  Result:=Buff;
end;

// ***************************
// ***** Int16 to String *****
// ***************************
function Int16ToString(Val: Int16): TIntString;
var
  Buff: array[0..6] of char;
begin
  Int16ToStr(Buff, Sizeof(Buff), 0, Val);
  Result:=Buff;
end;

// ****************************
// ***** UInt32 to String *****
// ****************************
function UInt32ToString(Val: UInt32): TIntString;
var
  Buff: array[0..10] of char;
begin
  UInt32ToStr(Buff, Sizeof(Buff), 0, Val);
  Result:=Buff;
end;

// ***************************
// ***** Int32 to String *****
// ***************************
function Int32ToString(Val: Int32): TIntString;
var
  Buff: array[0..11] of char;
begin
  Int32ToStr(Buff, Sizeof(Buff), 0, Val);
  Result:=Buff;
end;

//by @ackarwow

function UInt8Digits(aVal: UInt8): UInt8; inline;
begin
  if (aVal < UInt8DigitsArray[1]) then Result := 1 else
  if (aVal < UInt8DigitsArray[2]) then Result := 2 else
    Result := 3;
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

function UInt32ToStr(const s: PChar; const maxlen, digits: UInt8; const Val: UInt32): UInt8;
var
  u: UInt32;
  b: UInt8;
  P, PTmp: PChar;
  LeadingDigits: Int8;
  IntDigits, OutLen: UInt8;
begin
  OutLen:=0;

  IntDigits:=UInt32Digits(Val);

  if maxlen<(IntDigits+1) then //buffer too small, 1 for null terminator
    Exit(OutLen);

  if digits>=maxlen then //buffer too small
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

  u := Val;
  if u=0 then
  begin
    P^:= '0';
    Inc(P);
    Inc(OutLen);
  end
  else
  begin
    PTmp:=P;
    Inc(PTmp, IntDigits);
    while (u > 0) do
    begin
      Dec(PTmp);
      b := (u mod 10) + $30;
      PTmp^:= Chr(b);
      u := u div 10;
    end;
    Inc(OutLen, IntDigits);
    Inc(P, IntDigits);
  end;

  P^:= #0;

  Result:=OutLen;
end;

function Int32ToStr(const s: PChar; const maxlen, digits: UInt8; const Val: Int32): UInt8;
var
  u: UInt32;
  b: UInt8;
  P, PTmp: PChar;
  LeadingDigits: Int8;
  IntDigits, SgnDigit, OutLen: UInt8;
begin
  OutLen:=0;

  if Val < 0 then
  begin
    if Val = Low(Int32) then
      u:= $80000000
    else
      u:= UInt32(-Val);
  end
  else
    u:= UInt32(Val);

  IntDigits:=UInt32Digits(u);
  SgnDigit:=0;
  if Val<0 then
    Inc(SgnDigit);

  if maxlen<(IntDigits+SgnDigit+1) then //buffer too small, 1 for null terminator
    Exit(OutLen);

  if digits>=maxlen then //buffer too small
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

  if u=0 then
  begin
    P^:= '0';
    Inc(P);
    Inc(OutLen);
  end
  else
  begin
    PTmp:=P;
    Inc(PTmp, IntDigits);
    while (u > 0) do
    begin
      Dec(PTmp);
      b := (u mod 10) + $30;
      PTmp^:= Chr(b);
      u := u div 10;
    end;
    Inc(OutLen, IntDigits);
    Inc(P, IntDigits);
  end;

  P^:= #0;

  Result:=OutLen;
end;

function UInt16ToStr(const s: PChar; const maxlen, digits: UInt8; const Val: UInt16): UInt8;
var
  u: UInt16;
  b: UInt8;
  P, PTmp: PChar;
  LeadingDigits: Int8;
  IntDigits, OutLen: UInt8;
begin
  OutLen:=0;

  IntDigits:=UInt16Digits(Val);

  if maxlen<(IntDigits+1) then //buffer too small, 1 for null terminator
    Exit(OutLen);

  if digits>=maxlen then //buffer too small
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

  u := Val;
  if u=0 then
  begin
    P^:= '0';
    Inc(P);
    Inc(OutLen);
  end
  else
  begin
    PTmp:=P;
    Inc(PTmp, IntDigits);
    while (u > 0) do
    begin
      Dec(PTmp);
      b := (u mod 10) + $30;
      PTmp^:= Chr(b);
      u := u div 10;
    end;
    Inc(OutLen, IntDigits);
    Inc(P, IntDigits);
  end;

  P^:= #0;

  Result:=OutLen;
end;

function Int16ToStr(const s: PChar; const maxlen, digits: UInt8; const Val: Int16): UInt8;
var
  u: UInt16;
  b: UInt8;
  P, PTmp: PChar;
  LeadingDigits: Int8;
  IntDigits, SgnDigit, OutLen: UInt8;
begin
  OutLen:=0;

  if Val < 0 then
  begin
    if Val = Low(Int16) then
      u:= 32768
    else
      u:= UInt16(-Val);
  end
  else
    u:= UInt16(Val);

  IntDigits:=UInt16Digits(u);
  SgnDigit:=0;
  if Val<0 then
    Inc(SgnDigit);

  if maxlen<(IntDigits+SgnDigit+1) then //buffer too small, 1 for null terminator
    Exit(OutLen);

  if digits>=maxlen then //buffer too small
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

  if u=0 then
  begin
    P^:= '0';
    Inc(P);
    Inc(OutLen);
  end
  else
  begin
    PTmp:=P;
    Inc(PTmp, IntDigits);
    while (u > 0) do
    begin
      Dec(PTmp);
      b := (u mod 10) + $30;
      PTmp^:= Chr(b);
      u := u div 10;
    end;
    Inc(OutLen, IntDigits);
    Inc(P, IntDigits);
  end;

  P^:= #0;

  Result:=OutLen;
end;

function UInt8ToStr(const s: PChar; const maxlen, digits: UInt8; const Val: UInt8): UInt8;
var
  u: UInt8;
  b: UInt8;
  P, PTmp: PChar;
  LeadingDigits: Int8;
  IntDigits, OutLen: UInt8;
begin
  OutLen:=0;

  IntDigits:=UInt8Digits(Val);

  if maxlen<(IntDigits+1) then //buffer too small, 1 for null terminator
    Exit(OutLen);

  if digits>=maxlen then //buffer too small
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

  u:= Val;
  if u=0 then
  begin
    P^:= '0';
    Inc(P);
    Inc(OutLen);
  end
  else
  begin
    PTmp:=P;
    Inc(PTmp, IntDigits);
    while (u > 0) do
    begin
      Dec(PTmp);
      b := (u mod 10) + $30;
      PTmp^:= Chr(b);
      u := u div 10;
    end;
    Inc(OutLen, IntDigits);
    Inc(P, IntDigits);
  end;

  P^:= #0;

  Result:=OutLen;
end;

function Int8ToStr(const s: PChar; const maxlen, digits: UInt8; const Val: Int8): UInt8;
var
  u: UInt8;
  b: UInt8;
  P, PTmp: PChar;
  LeadingDigits: Int8;
  IntDigits, SgnDigit, OutLen: UInt8;
begin
  OutLen:=0;

  if Val < 0 then
  begin
    if Val = Low(Int8) then
      u:= 128
    else
      u:= UInt8(-Val);
  end
  else
    u:= UInt8(Val);

  IntDigits:=UInt8Digits(u);
  SgnDigit:=0;
  if Val<0 then
    Inc(SgnDigit);

  if maxlen<(IntDigits+SgnDigit+1) then //buffer too small, 1 for null terminator
    Exit(OutLen);

  if digits>=maxlen then //buffer too small
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

  if u=0 then
  begin
    P^:= '0';
    Inc(P);
    Inc(OutLen);
  end
  else
  begin
    PTmp:=P;
    Inc(PTmp, IntDigits);
    while (u > 0) do
    begin
      Dec(PTmp);
      b := (u mod 10) + $30;
      PTmp^:= Chr(b);
      u := u div 10;
    end;
    Inc(OutLen, IntDigits);
    Inc(P, IntDigits);
  end;

  P^:= #0;

  Result:=OutLen;
end;

function UInt8ToHexStr(const s: PChar; const maxlen, digits: UInt8; const Val: UInt8): UInt8;
var
  P: PChar;
  LeadingDigits: Int8;
  IntDigits: UInt8 = 2;
  OutLen: UInt8;
begin
  OutLen:=0;

  if maxlen<(IntDigits+1) then //buffer too small, 1 for null terminator
    Exit(OutLen);

  if digits>=maxlen then //buffer too small
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

  P^:=HexTab[Val shr 4];
  Inc(P);
  Inc(OutLen);

  P^:=HexTab[Val and $0f];
  Inc(P);
  Inc(OutLen);

  P^:= #0;

  Result:=OutLen;
end;

function UInt16ToHexStr(const s: PChar; const maxlen, digits: UInt8; const Val: UInt16): UInt8;
var
  b: UInt8;
  P: PChar;
  LeadingDigits: Int8;
  IntDigits: UInt8 = 4;
  OutLen: UInt8;
begin
  OutLen:=0;

  if maxlen<(IntDigits+1) then //buffer too small, 1 for null terminator
    Exit(OutLen);

  if digits>=maxlen then //buffer too small
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

  b:=UInt8(Val shr 8);

  P^:=HexTab[b shr 4];
  Inc(P);
  Inc(OutLen);

  P^:=HexTab[b and $0f];
  Inc(P);
  Inc(OutLen);

  b:=UInt8(Val and $ff);

  P^:=HexTab[b shr 4];
  Inc(P);
  Inc(OutLen);

  P^:=HexTab[b and $0f];
  Inc(P);
  Inc(OutLen);

  P^:= #0;

  Result:=OutLen;
end;

end.
