program TestLM35;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

uses
  fix16, timer, analog, liquidcrystal;

const
  AnalogPin = 1; //A1 connected to LM35

var
  iVal: integer;
  fTemp: TFix16;
  s: shortstring;
begin
  LC.Init(12, 11, 5, 4, 3, 2);
  LC._begin(16, 2);
  s:='';

  while True do
  begin
    LC.Home;

    iVal:=AnalogRead(AnalogPin);
    LC.Write('Temp (C): ');

    //analog LM35
    //V=(ADC * 5.0)/1024.0
    //T=V*100
    //formula: ((ADC*5.0)/1024.0)*100 = ADC*0.48828125

    fTemp:=Fix16Mul(IntToFix16(iVal), StrToFix16('0.48828'));
    Fix16ToStr(fTemp, 2, s);
    LC.Write(s);

    Delay(1000);
  end;
end.

