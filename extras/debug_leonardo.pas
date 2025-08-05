unit debug_leonardo;

//type compilation mode if necessary

interface

procedure LedON;
procedure LedOFF;

implementation

const
  LEDpinMask = 1 shl 7;

var
  LEDport: byte absolute PORTC;
  LEDddr: byte absolute DDRC;

procedure LedON;
begin
  LEDddr := LEDddr or LEDpinMask;
  LEDport := LEDport or LEDpinMask;
end;

procedure LedOFF;
begin
  LEDddr := LEDddr or LEDpinMask;
  LEDport := LEDport and not LEDpinMask;
end;


end.
