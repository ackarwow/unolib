unit stringutils;

// ***********************************************
// ***** Some functions to convert to String *****
// ***** By Dzandaa ******************************
// ***********************************************

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}
interface

	function UInt8ToHexString(Val: UInt8): String;
	function UInt16toHexString(Val: UInt16): String;

	function UInt8ToString(I8: UInt8): String;
	function Int8ToString(I8: Int8): String;
	function UInt16ToString(I16: Int16): String;
	function Int16ToString(I16: Int16): String;

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

end.
