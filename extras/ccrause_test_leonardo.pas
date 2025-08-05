program ccrause_test_leonardo;

uses
  cdc, usb, intrinsics, debug_leonardo;

var
  serial: TCDCSerial;
  c: byte;
begin
  USBDevice.attach;
  avr_sei;
  serial.Start(9600);

  repeat
    //LedOFF;
    if serial.Available > 0 then
    begin
      //LedON;
      c:=serial.Read;
      serial.Write(c+1);
    end;
  until false;
end.
