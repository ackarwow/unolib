program TestDHT11;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

uses
  fix16, timer, liquidcrystal, dht;

const
  DHT11Pin = 6;

var
  s: shortstring;
begin
  LC.Init(12, 11, 5, 4, 3, 2);
  LC._begin(16, 2);

  _DHT.Init(DHT11Pin, DHT11);
  s:='';

  while True do
  begin
    LC.Clear;
    if _DHT.Read=DHT_OK then
    begin
      LC.Write('T:');
      Fix16ToStr(_DHT.Temperature, 1, s);
      LC.Write(s);
      LC.Write(' H:');
      Fix16ToStr(_DHT.Humidity, 1, s);
      LC.Write(s);
    end
    else
      LC.Write('ERR');

    Delay(2000);
  end;
end.

