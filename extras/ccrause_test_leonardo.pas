program ccrause_test_leonardo;

uses
  cdc, usb, intrinsics;

const
  LEDpinMask = 1 shl 4;

var
  serial: TCDCSerial;
  c: byte;
  LEDport: byte absolute PORTb;
  LEDddr: byte absolute DDRB;
begin
  USBDevice.attach;
  avr_sei;
  serial.Start(9600);

  repeat
    LEDport := LEDport xor LEDpinMask;
    if serial.Available > 0 then
    begin
      LEDport := LEDport xor LEDpinMask;
      c:=serial.Read;
      serial.Write(c+1);
    end;
  until false;
end.
