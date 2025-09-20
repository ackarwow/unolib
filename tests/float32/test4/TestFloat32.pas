program TestFloat32;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode ObjFPC}{H+}
{$MODESWITCH ADVANCEDRECORDS}

uses
	float32, hardwareserial, timer, stringutils;

var
	StrA, StrB, StrC: string[32];
	F32a, F32b, F32c: TFloat32;
	i: UInt32;
begin

//type your setup code here

	Serial.Start(9600);
	Delay(1000);

	//program main loop
	//type your code here
	while true do
	begin
		Serial.WriteLn('BEGIN TEST');
		Serial.Flush();

		StrA := 'Hello ' + 'How '+ 'Are ' + 'You';
		Serial.WriteLn(StrA);
		Serial.Flush();

		StrA := 'Very ' + 'Well '+ 'Thank ' + 'You';
		Serial.WriteLn(StrA);
		Serial.Flush();

		StrA := '';
		F32a.RawData.Raw := $44201062; // 640.256
		StrA := F32a.ToString(3);
		Serial.Write('TFloat32 : $44201062 = 640.256  result: ' + StrA);
		Serial.Writeln('');
		Serial.Flush();

		StrA := '';
		F32a.RawData.Raw := $40490FDB; // 3.14159
		StrA := F32a.ToString(5);
		Serial.Write('TFloat32 : $40490FDB = 3.14159  result: ' + StrA);
		Serial.Writeln('');
		Serial.Flush();

		StrA := '';
		F32a.RawData.Raw := $3F49FBE7; // 0.789
		StrA := F32a.ToString(3);
		Serial.Write('TFloat32 : $3F49FBE7 = 0.789  result: ' + StrA);
		Serial.Writeln('');
		Serial.Flush();
		Serial.Flush();

		StrA := '';
		F32a.RawData.Raw := $C2029DB2; // -32.654
		StrA := F32a.ToString(3);
		Serial.Write('TFloat32 : $C2029DB2 = -32.654  result: ' + StrA);
		Serial.Writeln('');
		Serial.Flush();

        StrA := '';
		F32a := TFloat32.Create('789.123');
		StrA := F32a.ToString(3);
		Serial.Write('TFloat32.Create(''798.123'') result: ' + StrA);
		Serial.Writeln('');
		Serial.Flush();

        StrA := '';
		F32b := TFloat32.Create('3.14159');
		F32c := Rad2DegFloat32(F32b);
        StrB := '';
		StrC := F32c.ToString(5);
		Serial.Write('Rad2DegFloat32(3.14159) result: ' + StrC);
		Serial.Writeln('');
		Serial.Flush();

		StrC := '';
		F32b := TFloat32.Create('3.14159');
		F32c := SinFloat32(F32b);
        StrC := F32c.ToString(5);
		Serial.Write('SinFloat32(3.14159) result: ' + StrC);
		Serial.Writeln('');
		Serial.Flush();

		StrC := '';
		F32b := TFloat32.Create('3.14159');
		F32c := CosFloat32(F32b);
        StrC := F32c.ToString(5);
		Serial.Write('CosFloat32(3.14159) result: ' + StrC);
		Serial.Writeln('');
		Serial.Flush();

		StrA := '';
		StrB := '';
		StrC := '';
		F32a := TFloat32.Create('10.0');
		F32b := TFloat32.Create('21.0');

		F32c := F32a + F32b;
		StrC := F32c.ToString(6);
		StrA := F32A.ToString(6);
		StrB := F32B.ToString(6);
		Serial.Write(StrA + ' + ' + StrB + ' = ' + StrC);
		Serial.Writeln('');
		Serial.Flush;

		StrA := '';
		StrB := '';
		StrC := '';
		F32a := TFloat32.Create('125.0');
		F32b := TFloat32.Create('4.0');
		F32C := F32a mod F32b;
		StrC := F32c.ToString(6);
		StrA := F32A.ToString(6);
		StrB := F32B.ToString(6);
		Serial.Write(StrA + ' mod ' + StrB + ' = ' + StrC);
		Serial.Writeln('');
		Serial.Flush;

		Serial.WriteLn('END TEST');
		Serial.Flush();
        Delay(1000);
	end;
end.
