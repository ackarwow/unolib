unit conv2hexaU;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Spin, StdCtrls,
        ExtCtrls, Math, StrUtils;

type

	{ TConv2HexaForm }

 TConv2HexaForm = class(TForm)
          BCordic: TButton;
                BDec2Hex: TButton;
                BDec2Fix: TButton;
                BHex2Dec: TButton;
                BUInt322Str: TButton;
																BSinus: TButton;
                FSEValue: TFloatSpinEdit;
                FFEValue: TFloatSpinEdit;
                gbConversion: TGroupBox;
                gbCodeGeneration: TGroupBox;
		MLog: TMemo;
                pnlTools: TPanel;
                SPIterations: TSpinEdit;
                SPUInt32: TSpinEdit;
                TBHexa: TEdit;
		procedure BDec2HexClick(Sender: TObject);
		procedure BCordicClick(Sender: TObject);
                procedure BDec2FixClick(Sender: TObject);
		procedure BHex2DecClick(Sender: TObject);
		procedure BSinusClick(Sender: TObject);
		procedure BUInt322StrClick(Sender: TObject);
		procedure SPUInt32KeyPress(Sender: TObject; var Key: char);
		procedure TBHexaKeyPress(Sender: TObject; var Key: char);
	private

	public

	end;

var
	Conv2HexaForm: TConv2HexaForm;

implementation

{$R *.lfm}

{ TConv2HexaForm }

procedure TConv2HexaForm.BDec2HexClick(Sender: TObject);
var
	f: Single;
	uf: UInt32 absolute f;
	Str: String;
begin
	f := FSEValue.Value;

	Str := 'Value: ' + f.ToString + ' hex: ' + IntToHex(uf);
	MLog.Append(Str);
end;

procedure TConv2HexaForm.BCordicClick(Sender: TObject);
const
	C2P23: Single = 8388608.0;
var
	i, j: integer;
	f, f1: Single;
	uf: Uint32 absolute f;
	uf1: UInt32 absolute f1;
	Str: String;
	ZeroToOne: Single;
begin
 j := pred(SPIterations.Value);
 Str := 'Const' + LineEnding;
	Str := Str + ' CordicLookup: Array[0..'+ j.ToString+'] of Uint32 = ' + LineEnding;
	Str := Str + '('+LineEnding;

	// CORDIX Table
	for i := 0 to j do
	begin
			if(((i mod 8) = 0) and (i <> 0)) then
			Str := Str + LineEnding;
 	f := arctan(1 / power(2,i));
		Str := Str + '$' + IntToHex(uf);
		if(i< j) then Str := Str + ', ';
	end;
	Str := Str + LineEnding;
	Str := Str + ');';
	MLog.Append(Str);

	f := 1.0;
	for i := 0 to j do
	begin
		f := f *( 1 / sqrt(1 + power(2, -2 * i)));
	end;
	Str := 'K= ' + '$' + IntToHex(uf) + ';' + LineEnding;
	MLog.Append(Str);
 Mlog.Append('');

end;

//single to TFix16
//https://github.com/mitsuhiko/libfixmath/blob/master/libfixmath/fix16.h
procedure TConv2HexaForm.BDec2FixClick(Sender: TObject);
const
  fix16_one: Int32 = $00010000;
var
  os: single absolute fix16_one;
  f: single;
  tmp: single;
  r: Int32 absolute tmp;
  s: string;
begin
 f := FFEValue.Value;
 tmp:=f * {fix16_one}os;
 //#ifndef FIXMATH_NO_ROUNDING
 //if tmp >= 0.0 then
 //  tmp:=tmp+0.50
 //else
 //  tmp:=tmp-0.50;
 //#endif

 s := 'Value: ' + f.ToString + ' hex: ' + IntToHex(r);
 MLog.Append(s);
end;

procedure TConv2HexaForm.BHex2DecClick(Sender: TObject);
begin

	MLog.Append('Hex: $' + TBHexa.Text + ' Dec: ' + Hex2Dec(TBHexa.Text).ToString);

end;

procedure TConv2HexaForm.BSinusClick(Sender: TObject);
var
	fsin, fstep, fval: Real;
	iStep, i: UInt8;
	Str: String;
begin
	Str := 'Const' + LineEnding;
	Str := Str + ' SinWave: Array[0..255] of Uint8 = ' + LineEnding;
	Str := Str + '('+LineEnding;

	fsin := -1.0;
	fstep := 2.0 / 255.0;
	i := 0;
	// Sinus Table
	while (True) do
	begin
		if(((i mod 8) = 0) and (i <> 0)) then
			Str := Str + LineEnding;
		fval := (sin(fsin) * 127.0) + 127.0;
		iStep := UInt8(Round(fval));
		Str := Str + '$' + IntToHex(iStep);
		if(i< 255) then Str := Str + ', ';
		if(i = 255) then break;
		inc(i);
		fsin := -1.0 + (fstep * i);
//		fsin := fsin + fstep;
	end;
	Str := Str + LineEnding;
	Str := Str + ');';
	MLog.Append(Str);

end;

procedure TConv2HexaForm.BUInt322StrClick(Sender: TObject);
var
  Val, Valt, Valr: Uint32;
  Res: Array [0..9] of char = '0000000000';  // Max value = 2147483647
  Ptr: PChar;
  Pos: Integer;
  Str: String;
begin
 Val := SPUInt32.Value;
 if(Val <= 0) then
 begin
  Str := 'UInt32: ' + Val.ToString + ' Text: ' + '0';
  Mlog.append(Str);
  exit();
 end;

 Ptr := Res;
 Valt := Val;
 Pos := 9;

 While Valt <> 0 do
 begin
  Valr := Valt Div 10;
  Res[Pos] := char((Valt - (Valr * 10)) + $30);
  Dec(Pos);
  Valt := Valr;
 end;

 while(Ptr^ = '0') do Inc(Ptr);

 Res := Ptr;
 Mlog.append('UInt32: ' + Val.ToString + ' Text: ' + Res);
end;

procedure TConv2HexaForm.SPUInt32KeyPress(Sender: TObject; var Key: char);
begin
	if Key = '-' then // Only Positive numbers
	begin
		Key := #0;
	end
end;

procedure TConv2HexaForm.TBHexaKeyPress(Sender: TObject; var Key: char);
begin
	if not (Key in [#8, '0'..'9', 'a'..'f', 'A'..'F']) then
	begin
//		ShowMessage('Invalid key');
	// Discard the key
	Key := #0;
	end
end;

end.

