unit TestAVRU;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF UNIX}
	{$IFDEF UseCThreads}
	 cthreads,
	{$ENDIF}
{$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, FileUtil, Registry, LazSerial;

const
  H_Send = 'S'; // Send
  H_Receive = 'R'; // Receive
  H_Error = 'E'; // Error
  H_Ready = 'K'; // Ready
  H_Info = 'I'; // Info

  F_Add = 'A'; //function Add
  F_Sub = 'R'; //function Subtract
  F_Mul = 'M'; //function Multiply
  F_Div = 'D'; //function Divide
  F_Sqr = 'Q'; //function Sqrt
  F_Sin = 'S'; //function Sinus
  F_Cos = 'C'; //function Cosinus
  F_Deg = '1'; //function DegRad
  F_Rad = '2'; //function RadDeg
  F_Abs = '3'; //function Absolute
  F_Neg = '4'; //function Negate
  F_Tan = '5'; //Function Tangent
  F_Ctn = '6'; //Function Cotangent
  F_Mod = 'O'; //function Mod
  F_Lg2 = 'L'; //function Log2
  F_LgN = 'N'; //function LogN
  F_L10 = '0'; //function Log10
  F_InP = 'I'; //function IntPow
  F_Pow = 'P'; //function Pow
  F_Exp = 'E'; //function Exp

  F_Tst = 'T'; //Function special test

type
  TRawFloat32 = UInt32;

  TComData = packed record
    Command: char;     // Test Command
    Func:    char;    // function to test
    Param1:  TRawFloat32; // Parameter1
    Param2:  TRawFloat32; // Parameter2
    RetVal:  TRawFloat32; // return value of function
  end;

const
  StructSize = SizeOf(TComData);

type
  TBufferData = packed record
    case boolean of
      True: (Values: TComData);
      False: (Buffer: array[0..StructSize - 1] of char);
  end;

type
  TTestParams = array[0..1] of single;
  TTestParamsArray = array of TTestParams;

  { TTestAVRForm }

  TTestAVRForm = class(TForm)
    BClearLog: TButton;
    BRS232Start: TButton;
    BTests: TButton;
    BSelectAll: TButton;
    CBBaudRate: TComboBox;
    CBDataBits: TComboBox;
    CBFlow: TComboBox;
    CBParity: TComboBox;
    CBRS232Port: TComboBox;
    CBStopBits: TComboBox;
    cgbFunctions: TCheckGroup;
    chkSpecialSineTest: TCheckBox;
    FSEVal1: TFloatSpinEdit;
    FSEVal2: TFloatSpinEdit;
    GRS232: TGroupBox;
    LValue1: TLabel;
    LValue2: TLabel;
    pnlFunctions: TPanel;
    pnlMain: TPanel;
    pnlTools: TPanel;
    rbPredefined: TRadioButton;
    rbManual: TRadioButton;
    RS232: TLazSerial;
    LBaudrate: TLabel;
    LDataBits: TLabel;
    LFlow: TLabel;
    LParity: TLabel;
    LRS232Port: TLabel;
    LStopBits: TLabel;
    MLog: TMemo;
    RBRS232: TRadioButton;
    procedure BClearLogClick(Sender: TObject);
    procedure BRS232StartClick(Sender: TObject);
    procedure BSelectAllClick(Sender: TObject);
    procedure BTestsClick(Sender: TObject);
    procedure CBBaudRateChange(Sender: TObject);
    procedure CBDataBitsChange(Sender: TObject);
    procedure CBFlowChange(Sender: TObject);
    procedure CBParityChange(Sender: TObject);
    procedure CBRS232PortChange(Sender: TObject);
    procedure CBStopBitsChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RS232RxData(Sender: TObject);

    function SendData(Timeout: QWord): boolean;

    function ProceedData(aFunc: char; aParam1, aParam2: TRawFloat32; aTimeout: integer; out rValue: TRawFloat32): boolean;

    function WaitForReady(Timeout: QWord): boolean;

    procedure SetButtons(OnOff: boolean);

    procedure ShowSendLog();
    procedure ShowReceiveLog();

  private
    // ***** RS232 *****
    Data: TComData;
    BuffData: TBufferData;
    ACK: boolean;
    procedure DoTest(aTestType: char; aDataArray: TTestParamsArray);
  public

  end;

var
  TestAVRForm: TTestAVRForm;

implementation

{$R *.lfm}

uses
  float32, math;

const
  aTestAddData: TTestParamsArray = (
                                    (1.11, 2.22),
                                    (0.001, -0.001)
                                   );
  aTestSubData: TTestParamsArray = (
                                    (1.11, 3.33),
                                    (0.001, -0.001)
                                   );
  aTestMulData: TTestParamsArray = (
                                    (1.00, 1.00),
                                    (0.001, -0.001)
                                   );
  aTestDivData: TTestParamsArray = (
                                    (1.00, 1.00),
                                    (0.001, -0.001)
                                   );
  aTestSqrData: TTestParamsArray = (
                                    (529.0, 0.0)
                                    );
  aTestSinData: TTestParamsArray = (
                                    (174.0, 0.0)
                                    );
  aTestCosData: TTestParamsArray = (
                                    (-13.29940, 0.0)
                                    );
  aTestTanData: TTestParamsArray = (
                                    (-13.29940, 0.0)
                                    );
  aTestCtnData: TTestParamsArray = (
                                    (-13.29940, 0.0)
                                    );
  aTestR2DData: TTestParamsArray = (
                                    (-13.29940, 0.0)
                                    );
  aTestD2RData: TTestParamsArray = (
                                    (-762.0, 0.0)
                                    );
  aTestAbsData: TTestParamsArray = (
                                    (-12.35, 0.0)
                                    );
  aTestNegData: TTestParamsArray = (
                                    (-5.68, 0.0)
                                    );
  aTestModData: TTestParamsArray = (
                                    (5.432, 1.234)
                                    );
  aTestLg2Data: TTestParamsArray = (
                                    (55.3859, 0.0)
                                    );
  aTestLgNData: TTestParamsArray = (
                                    (55.3859, 0.0)
                                    );
  aTestL10Data: TTestParamsArray = (
                                    (55.3859, 0.0)
                                    );
  aTestInPData: TTestParamsArray = (
                                    (-2.0, 3.0)
                                    );
  aTestPowData: TTestParamsArray = (
                                    (55.3859, 1.234)
                                    );
  aTestExpData: TTestParamsArray = (
                                    (1.2345, 0.0)
                                    );
  aTestTstData: TTestParamsArray = (
                                    (0.001, -0.001)
                                    );

// ********************************************
// ***** Real Sleep with Messages Process *****
// ********************************************
procedure RealSleep(DelayTickCount: QWORD);
var
  StartTickCount: QWORD;
begin
  StartTickCount := GetTickCount64;
  while (GetTickCount64 < StartTickCount + DelayTickCount) and
    (not Application.Terminated) do
  begin
    Application.ProcessMessages;
    Sleep(10);
  end;
end;

{ TTestAVRForm }
{$REGION 'Form'}

{$IFDEF WINDOWS}
function GetSerialPorts: TStringList;
var
  reg: TRegistry;
  COMList, COMName: TStringList;
		Cnt: Integer;
//		Str: String;
begin
  COMList := TStringList.Create;
		COMName := TStringList.Create;
  reg := TRegistry.Create;

  try
{$IFNDEF VER100}
{$IFNDEF VER120}
    reg.Access := KEY_READ;
{$ENDIF}
{$ENDIF}
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey('\HARDWARE\DEVICEMAP\SERIALCOMM\', false);
    reg.GetValueNames(COMList);
				for Cnt := 0 to Pred(COMList.Count) do
				begin
//     Str := COMList[Cnt];
//					if STR.Contains('USB') then
						COMName.Add(Pchar(reg.ReadString(COMList[Cnt])));
				end;
		finally
    reg.Free;
				ComList.Free;
  end;
		exit(COMName);
end;
{$ENDIF}

{$IFDEF UNIX}

// **********************************
// ***** Get Serial Ports Linux *****
// **********************************
function GetSerialPorts: TStringList;
var
 COMList: TStringList;
begin
{$IFDEF LINUX}
	COMList :=  FindAllFiles('/dev','ttyUSB*;ttyACM*', False);
{$ELSE}  // MacOS
	COMList :=  FindAllFiles('/dev','tty.usbmodem*;tty.usbserial*', False);
{$ENDIF}

	exit(COMList);
end;
{$ENDIF}

// ***********************
// ***** Create Form *****
// ***********************
procedure TTestAVRForm.FormCreate(Sender: TObject);
var
  COMNames: TStringList;
  Cnt: integer;
begin

  // RS232 init

  COMNames := GetSerialPorts;
  CBRS232Port.Clear;

  for Cnt := 0 to Pred(COMNames.Count) do
    CBRS232Port.Items.Add(COMNames[Cnt]);
  COMNames.Free;
  if (CBRS232Port.Items.Count <> -1) then CBRS232Port.ItemIndex := 0;


  if (CBRS232Port.Items.Count <> 0) then RS232.Device := CBRS232Port.Items[0];

  CBBaudRate.ItemIndex := 11;
  RS232.BaudRate := br_57600;

  CBDataBits.ItemIndex := 0;
  RS232.DataBits := db8bits;

  CBStopBits.ItemIndex := 0;
  RS232.StopBits := sbOne;

  CBParity.ItemIndex := 0;
  RS232.Parity := pNone;

  CBFlow.ItemIndex := 0;
  RS232.FlowControl := fcNone;

  MLog.Clear;

  SetButtons(False);
end;

// **********************
// ***** Close Form *****
// **********************
procedure TTestAVRForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

// ************************
// ***** Destroy Form *****
// ************************
procedure TTestAVRForm.FormDestroy(Sender: TObject);
begin
  RS232.Destroy;
end;

{$ENDREGION 'Form'}

{$REGION 'RS232'}
// *****************************
// ***** Change RS232 Port *****
// *****************************
procedure TTestAVRForm.CBRS232PortChange(Sender: TObject);
begin
  RS232.Device := CBRS232Port.Items[CBRS232Port.ItemIndex];
end;

// **********************************
// ***** Change RS232 Stop Bits *****
// **********************************
procedure TTestAVRForm.CBStopBitsChange(Sender: TObject);
begin
  RS232.StopBits := TStopBits(CBStopBits.ItemIndex);
end;

// **********************************
// ***** Change RS232 Baud Rate *****
// **********************************
procedure TTestAVRForm.CBBaudRateChange(Sender: TObject);
begin
  RS232.BaudRate := TBaudRate(CBBaudRate.ItemIndex);
end;

// **********************************
// ***** Change RS232 Data Bits *****
// **********************************
procedure TTestAVRForm.CBDataBitsChange(Sender: TObject);
begin
  RS232.DataBits := TDataBits(CBDataBits.ItemIndex);
end;

// *****************************
// ***** Change RS232 Flow *****
// *****************************
procedure TTestAVRForm.CBFlowChange(Sender: TObject);
begin
  RS232.FlowControl := TFlowControl(CBFlow.ItemIndex);
end;

// *******************************
// ***** Change RS232 Parity *****
// *******************************
procedure TTestAVRForm.CBParityChange(Sender: TObject);
begin
  RS232.Parity := TParity(CBParity.ItemIndex);
end;

// ******************************
// ***** RS232 Receive Data *****
// ******************************
procedure TTestAVRForm.RS232RxData(Sender: TObject);
var
  RXData: string;
  BuffSize, i: integer;
  TChar: TCharArray;
begin
  if (not RS232.DataAvailable) then
  begin
    Exit;
  end;
  RBRS232.Checked := not RBRS232.Checked;
  RBRS232.Refresh;
  RXData := '';

  RXData := RS232.ReadData;

  // Message Header Unknown
  if (not (RxData.Chars[0] in [H_Receive, H_Ready, H_Error, H_Info])) then
  begin
    MLog.Append('Error: Unknown Header: "' + RxData.Chars[0] + '"');
    while (RS232.DataAvailable) do
    begin
      RS232.ReadData;
      RealSleep(10);
    end;
    exit;
  end;

  BuffSize := RXData.Length;

  // Case Fragmentation
  while (BuffSize < StructSize) do
  begin
    BuffSize := RXData.Length;
    if (RS232.DataAvailable) then RXData := RXDAta + RS232.ReadData;
    RealSleep(10);
  end;

  if (BuffSize = StructSize) then
  begin
    TChar := RXData.ToCharArray;
    for i := 0 to Pred(StructSize) do
    begin
      BuffData.Buffer[i] := Tchar[i];
    end;
  end
  // BuffSize <> StructSize
  else
  begin
    MLog.Append('Error: Invalid buffer Size received');
    while (RS232.DataAvailable) do RS232.ReadData;
    exit;
  end;

  // Buffer loaded
  // Check Command
  case BuffData.Values.Command of
    H_Error:
    begin
      MLog.Append('Error: Arduino returned Error');
      ACK := True;
    end;

    H_Receive: // Acnowledge
    begin
      RealSleep(25);
      if (BuffData.Values.Func = Data.Func) then //AK ????
        ACK := True;
    end;

    H_Ready: // Rail Ready and set to Zero Position
    begin
      MLog.Append('RS232 Ready');
    end;

    H_Info: // Information
    begin

    end;

    else
    begin // Error Command Unknown
      MLog.Append('Error: Wrong Command Acknowledge received');
      while (RS232.DataAvailable) do
      begin
        RS232.ReadData; // Clear Buffer Data
        RealSleep(10);
      end;
    end;
  end;

end;

// ***********************
// ***** RS232 Start *****
// ***********************
procedure TTestAVRForm.BRS232StartClick(Sender: TObject);
begin
  if (BRS232Start.Caption = 'Start RS232') then
  begin
    BRS232Start.Caption := 'Stop RS232';

{$IFDEF UNIX}
RS232.SynSer.NonBlock := True;
{$ENDIF}
    try
      RS232.Open;
    except
      on E: Exception do
      begin
        ShowMessage('Problem Opening RS232: ' + E.Message);
        BRS232Start.Caption := 'Start RS232';
        RS232.Active := False;

        SetButtons(False);
        exit();
      end;
    end;
    // OK RS232 Opened

    MLog.Clear();
    RealSleep(1000);

    SetButtons(True);
  end
  else
  begin
    // Close RS232
    BRS232Start.Caption := 'Start RS232';
    RS232.Close;
    RS232.Active := False;

    SetButtons(False);
    RealSleep(1000);
  end;
end;

procedure TTestAVRForm.BClearLogClick(Sender: TObject);
begin
  MLog.Clear;
end;

procedure TTestAVRForm.BSelectAllClick(Sender: TObject);
var
  Cnt: integer;
begin
  for Cnt:=0 to cgbFunctions.Items.Count-1 do
    cgbFunctions.Checked[Cnt]:=true;
end;

// ***************************
// ***** RS232 Send Data *****
// ***************************
function TTestAVRForm.SendData(Timeout: QWord): boolean;
begin
  RS232.WriteBuffer(Data, StructSize);
  MLog.Refresh;
  RealSleep(50);
  exit(WaitForReady(Timeout));
end;

// **************************************
// ***** RS232 Wait for RS232 Ready *****
// **************************************
function TTestAVRForm.WaitForReady(Timeout: QWord): boolean;
var
  StartTime: QWord;
begin
  ACK := False;

  StartTime := GetTickCount64 + Timeout; // Timeout in Milliseconds

  // Wait for Acknowledge
  while (not ACK) do
  begin
    if (GetTickCount64 > StartTime) then
    begin
      // Error Timeout
      exit(False);
    end;
    RealSleep(10);
  end;
  // Acknowledge received
  exit(True);
end;

{$ENDREGION 'RS232'}

// ***********************************
// ***** Send Command To Arduino *****
// ***********************************
function TTestAVRForm.ProceedData(aFunc: char; aParam1, aParam2: TRawFloat32; aTimeout: integer; out rValue: TRawFloat32): boolean;
begin
  if (not RS232.Active) then
  begin
    ShowMessage('Error: RS232 Not Started!!');
    Exit(false);
  end;

  Data.Command := H_Send;
  Data.Func := aFunc;
  Data.Param1:=aParam1;
  Data.Param2:=aParam2;

  ShowSendLog();
  RealSleep(250);
  Result:=SendData(aTimeout);
  if Result then
  begin
    ShowReceiveLog();
    rValue:=BuffData.Values.RetVal;
  end
  else
    Mlog.Append('Error: Timeout on Acknowledge');
end;

// ***************************************
// ***** Enable Disable Test buttons *****
// ***************************************
procedure TTestAVRForm.SetButtons(OnOff: boolean);
const
  aLabelColors: array[boolean] of TColor=(clGray, clBlack);
begin
  BTests.Enabled := OnOff;

  rbPredefined.Enabled:=OnOff;
  rbManual.Enabled:=OnOff;
  FSEVal1.Enabled:=OnOff;
  FSEVal2.Enabled:=OnOff;
  LValue1.Font.Color:=aLabelColors[OnOff];
  LValue2.Font.Color:=aLabelColors[OnOff];
  cgbFunctions.Enabled:=OnOff;
  BSelectAll.Enabled:=OnOff;
  chkSpecialSineTest.Enabled:=OnOff;
end;

// *************************
// ***** Show Send Log *****
// *************************

function SingleToStr(Value: single): string;
begin
  Str(Value:4:8, result);
end;

function Float32ToStr(Value: TRawFloat32): string;
var
  s: single absolute Value;
begin
  Str(s:4:8, result);
end;

function SingleToFloat32(Value: single): TRawFloat32;
var
  r: TRawFloat32 absolute Value;
begin
  Result:=r;
end;

function FuncToStr(aFunc: char): string;
begin
  case aFunc of
  F_Add: Result:= 'Float32Add';
  F_Sub: Result:= 'Float32Sub';
  F_Mul: Result:= 'Float32Mul';
  F_Div: Result:= 'Float32Div';
  F_Sqr: Result:= 'Float32Sqrt';
  F_Sin: Result:= 'Float32Sin';
  F_Cos: Result:= 'Float32Cos';
  F_Deg: Result:= 'Float32Deg2Rad';
  F_Rad: Result:= 'Float32Rad2Deg';
  F_Abs: Result:= 'Float32Abs';
  F_Neg: Result:= 'Float32Neg';
  F_Tan: Result:= 'Float32Tan';
  F_Ctn: Result:= 'Float32Cotan';
  F_Mod: Result:= 'Float32Mod';
  F_Lg2: Result:= 'Float32Log2';
  F_LgN: Result:= 'Float32Ln';
  F_L10: Result:= 'Float32Log10';
  F_InP: Result:= 'Float32IntPow';
  F_Pow: Result:= 'Float32Pow';
  F_Exp: Result:= 'Float32Exp';

  F_Tst: Result:= 'Float32Test';

  else Result:='Unknown/Undefined';
  end;
end;

function FuncToIdx(aFunc: char): integer;
begin
  case aFunc of
  F_Add: Result:= 0;
  F_Sub: Result:= 1;
  F_Mul: Result:= 2;
  F_Div: Result:= 3;
  F_Sqr: Result:= 4;
  F_Sin: Result:= 5;
  F_Cos: Result:= 6;
  F_Deg: Result:= 7;
  F_Rad: Result:= 8;
  F_Abs: Result:= 9;
  F_Neg: Result:= 10;
  F_Tan: Result:= 11;
  F_Ctn: Result:= 12;
  F_Mod: Result:= 13;
  F_Lg2: Result:= 14;
  F_LgN: Result:= 15;
  F_L10: Result:= 16;
  F_InP: Result:= 17;
  F_Pow: Result:= 18;
  F_Exp: Result:= 19;

  F_Tst: Result:= 20;
  else Result:=-1;
  end;
end;

procedure TTestAVRForm.ShowSendLog();
begin
  MLog.Append('***** Sending:');
  MLog.Append(Format('  Func: %s (%s)', [Data.Func, FuncToStr(Data.Func)]));
  MLog.Append('  Param1: ' + Float32ToStr(Data.Param1));
  MLog.Append('  Param2: ' + Float32ToStr(Data.Param2));
  MLog.Refresh();
end;

// ****************************
// ***** Show Receive Log *****
// ****************************
procedure TTestAVRForm.ShowReceiveLog();
begin
  MLog.Append('***** Receiving:');
  MLog.Append(Format('  Func: %s (%s)', [BuffData.Values.Func, FuncToStr(BuffData.Values.Func)]));
  MLog.Append('  Param1: ' + Float32ToStr(BuffData.Values.Param1));
  MLog.Append('  Param2: ' + Float32ToStr(BuffData.Values.Param2));
  MLog.Append('  RetVal: ' + Float32ToStr(BuffData.Values.RetVal));
  MLog.Refresh();
end;

procedure TTestAVRForm.DoTest(aTestType: char; aDataArray: TTestParamsArray);
const
  TIMEOUT: integer = 1000;
var
  //return value from Arduino
  f1: TRawFloat32;
  s1: single absolute f1;
  //return value on PC side
  f2: TRawFloat32;
  s2: single absolute f2;
  //parameters
  s, sParam1, sParam2: single;
  i: UInt16;
  idx: integer;
begin
  idx:=FuncToIdx(aTestType);
  if (idx>-1) then
    if cgbFunctions.Checked[idx] then
    begin

      case aTestType of
      F_Add: MLog.Append('TEST Addition');
      F_Sub: MLog.Append('TEST Subtraction');
      F_Mul: MLog.Append('TEST Multiplication');
      F_Div: MLog.Append('TEST Division');
      F_Sqr: MLog.Append('TEST Square Root');
      F_Sin: MLog.Append('TEST Sine');
      F_Cos: MLog.Append('TEST Cosine');
      F_Tan: MLog.Append('TEST Tangent');
      F_Ctn: MLog.Append('TEST Cotangent');
      F_Rad: MLog.Append('TEST Rad2Deg');
      F_Deg: MLog.Append('TEST Deg2Rad');
      F_Abs: MLog.Append('TEST Absolute');
      F_Neg: MLog.Append('TEST Negate');
      F_Mod: MLog.Append('TEST Modulo');
      F_Lg2: MLog.Append('TEST Log2');
      F_LgN: MLog.Append('TEST Ln');
      F_L10: MLog.Append('TEST Log10');
      F_InP: MLog.Append('TEST IntPow');
      F_Pow: MLog.Append('TEST Pow');
      F_Exp: MLog.Append('TEST Exp');

      F_Tst: MLog.Append('TEST special test');
      else raise exception.CreateFmt('Unsupported test type "%s"', [aTestType]);
      end;

      for i:=Low(aDataArray) to High(aDataArray) do
      begin
        sParam1:=aDataArray[i,0];
        sParam2:=aDataArray[i,1];
        if ProceedData(aTestType, SingleToFloat32(sParam1), SingleToFloat32(sParam2), TIMEOUT, f1) then //Arduino side
        begin
          case aTestType of
          F_Add:
            begin
              f2:=Float32Add(SingleToFloat32(sParam1), SingleToFloat32(sParam2)); //PC side
              s:=sParam1+sParam2; //RTL
            end;
          F_Sub:
            begin
              f2:=Float32Sub(SingleToFloat32(sParam1), SingleToFloat32(sParam2)); //PC side
              s:=sParam1-sParam2; //RTL
            end;
          F_Mul:
            begin
              f2:=Float32Mul(SingleToFloat32(sParam1), SingleToFloat32(sParam2)); //PC side
              s:=sParam1*sParam2; //RTL
            end;
          F_Div:
            begin
              f2:=Float32Div(SingleToFloat32(sParam1), SingleToFloat32(sParam2)); //PC side
              s:=sParam1/sParam2; //RTL
            end;
          F_Sqr:
            begin
              f2:=Float32Sqrt(SingleToFloat32(sParam1)); //PC side
              s:=Sqrt(sParam1); //RTL
            end;
          F_Sin:
            begin
              f2:=Float32Sin(SingleToFloat32(sParam1)); //PC side
              s:=Sin(sParam1); //RTL
            end;
          F_Cos:
            begin
              f2:=Float32Cos(SingleToFloat32(sParam1)); //PC side
              s:=Cos(sParam1); //RTL
            end;
          F_Tan:
            begin
              f2:=Float32Tan(SingleToFloat32(sParam1)); //PC side
              s:=Tan(sParam1); //RTL
            end;
          F_Ctn:
            begin
              f2:=Float32Cotan(SingleToFloat32(sParam1)); //PC side
              s:=Cotan(sParam1); //RTL
            end;
          F_Rad:
            begin
              f2:=Float32Rad2Deg(SingleToFloat32(sParam1)); //PC side
              s:=RadToDeg(sParam1); //RTL
            end;
          F_Deg:
            begin
              f2:=Float32Deg2Rad(SingleToFloat32(sParam1)); //PC side
              s:=DegToRad(sParam1); //RTL
            end;
          F_Abs:
            begin
              f2:=Float32Abs(SingleToFloat32(sParam1)); //PC side
              s:=Abs(sParam1); //RTL
            end;
          F_Neg:
            begin
              f2:=Float32Neg(SingleToFloat32(sParam1)); //PC side
              s:=-sParam1;
            end;
          F_Mod:
            begin
              f2:=Float32Mod(SingleToFloat32(sParam1), SingleToFloat32(sParam2)); //PC side
              s:=FMod(sParam1,sParam2); //RTL
            end;
          F_Lg2:
            begin
              f2:=Float32Log2(SingleToFloat32(sParam1)); //PC side
              s:=Log2(sParam1); //RTL
            end;
          F_LgN:
            begin
              f2:=Float32Ln(SingleToFloat32(sParam1)); //PC side
              s:=Ln(sParam1); //RTL
            end;
          F_L10:
            begin
              f2:=Float32Log10(SingleToFloat32(sParam1)); //PC side
              s:=Log10(sParam1); //RTL
            end;
          F_InP:
            begin
              f2:=Float32IntPow(SingleToFloat32(sParam1), Trunc(sParam2)); //PC side
              s:=IntPower(sParam1, Trunc(sParam2)); //RTL
            end;
          F_Pow:
            begin
              f2:=Float32Pow(SingleToFloat32(sParam1), SingleToFloat32(sParam2)); //PC side
              s:=Power(sParam1, sParam2); //RTL
            end;
          F_Exp:
            begin
              f2:=Float32Exp(SingleToFloat32(sParam1)); //PC side
              s:=Exp(sParam1); //RTL
            end;
          F_Tst:
            begin
              f2:=Float32test(SingleToFloat32(sParam1), SingleToFloat32(sParam2)); //PC side
              s:=sParam1/sParam2;
              MLog.Append(Format('UInt32 on Arduino: %s', [IntToStr(f1)]));
              MLog.Append(Format('UInt32 on PC: %s', [IntToStr(f2)]));
            end
          else raise exception.CreateFmt('Unsupported test type "%s"', [aTestType]);
          end;
          MLog.Append(Format('Float32 on PC: %s', [SingleToStr(s2)]));
          MLog.Append(Format('RTL Value: %s', [SingleToStr(s)]));
          MLog.Append(Format('Difference: %s', [SingleToStr(Abs(s-s1))]));
        end;
      end;

      MLog.Append('');
    end;
end;

// *************************
// ***** Test 1 Button *****
// *************************
procedure TTestAVRForm.BTestsClick(Sender: TObject);
const
  RadMin: integer = -180;
  RadMax: integer = 180;
var
  ManualTestData: TTestParamsArray;
  SinTestData: TTestParamsArray;
  i: integer;
begin
  SetLength({%H-}ManualTestData, 1);
  ManualTestData[0, 0]:=FSEVal1.Value;
  ManualTestData[0, 1]:=FSEVal2.Value;

  //Addintion
  if rbPredefined.Checked then DoTest(F_Add, aTestAddData)
    else DoTest(F_Add, ManualTestData);

  //Subtraction
  if rbPredefined.Checked then DoTest(F_Sub, aTestSubData)
    else DoTest(F_Sub, ManualTestData);

  //Multiplication
  if rbPredefined.Checked then DoTest(F_Mul, aTestMulData)
    else DoTest(F_Mul, ManualTestData);

  //Division
  if rbPredefined.Checked then DoTest(F_Div, aTestDivData)
    else DoTest(F_Div, ManualTestData);

  //Square root
  if rbPredefined.Checked then DoTest(F_Sqr, aTestSqrData)
    else DoTest(F_Sqr, ManualTestData);

  //Sinus - special test
  if rbPredefined.Checked then
  begin
    if chkSpecialSineTest.Checked then
    begin
      SetLength({%H-}SinTestData, 361);
      for i:=0 to 360 do
      begin
        SinTestData[i,0]:=i-180;
        SinTestData[i,1]:=0.0;
      end;

      DoTest(F_Sin, SinTestData);
      SetLength(SinTestData, 0);
    end
    else
      DoTest(F_Sin, aTestSinData);
  end
  else DoTest(F_Sin, ManualTestData);

  //Cosinus
  if rbPredefined.Checked then DoTest(F_Cos, aTestCosData)
    else DoTest(F_Cos, ManualTestData);

  //Tangent
  if rbPredefined.Checked then DoTest(F_Tan, aTestTanData)
    else DoTest(F_Tan, ManualTestData);

  //Cotangent
  if rbPredefined.Checked then DoTest(F_Ctn, aTestCtnData)
    else DoTest(F_Ctn, ManualTestData);

  //Rad2Deg
  if rbPredefined.Checked then DoTest(F_Rad, aTestR2DData)
    else DoTest(F_Rad, ManualTestData);

  //Deg2Rad
  if rbPredefined.Checked then DoTest(F_Deg, aTestD2RData)
    else DoTest(F_Deg, ManualTestData);

  //Absolute
  if rbPredefined.Checked then DoTest(F_Abs, aTestAbsData)
    else DoTest(F_Abs, ManualTestData);

  //Negate
  if rbPredefined.Checked then DoTest(F_Neg, aTestNegData)
    else DoTest(F_Neg, ManualTestData);

  //Modulo
  if rbPredefined.Checked then DoTest(F_Mod, aTestModData)
    else DoTest(F_Mod, ManualTestData);

  //Log2
  if rbPredefined.Checked then DoTest(F_Lg2, aTestLg2Data)
    else DoTest(F_Lg2, ManualTestData);

  //LogN
  if rbPredefined.Checked then DoTest(F_LgN, aTestLgNData)
    else DoTest(F_LgN, ManualTestData);

  //Log10
  if rbPredefined.Checked then DoTest(F_L10, aTestL10Data)
    else DoTest(F_L10, ManualTestData);

  //IntPow
  if rbPredefined.Checked then DoTest(F_InP, aTestInPData)
    else DoTest(F_InP, ManualTestData);

  //Pow
  if rbPredefined.Checked then DoTest(F_Pow, aTestPowData)
    else DoTest(F_Pow, ManualTestData);

  //Exp
  if rbPredefined.Checked then DoTest(F_Exp, aTestExpData)
    else DoTest(F_Exp, ManualTestData);

  //special test
  if rbPredefined.Checked then DoTest(F_Tst, aTestTstData);
    //else DoTest(F_Tst, ManualTestData);
end;

end.
