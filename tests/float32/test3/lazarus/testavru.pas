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

  F_Str = 'S'; //function StrToFloat
  F_Flt = 'F'; //function FloatToStr
  F_Tst = 'T'; // Function special test

type
  TRawFloat32 = UInt32;

  TStrVal = array[0..15] of char;

  TComData = packed record
    Command: char;     // Test Command
    Func:    char;     // function to test
    FltVal:  TRawFloat32; // float32 value (input in FloatToStr, output in StrToFloat)
    StrLen:  Int8;     // length of str param (input in StrToFloat, output in FloatToStr) or error in conversion (-1, output in StrToFloat)
    StrVal:  TStrVal;  // Parameter - string value (input in StrToFloat, output in FloatToStr)
  end;

const
  StructSize = SizeOf(TComData); //max struct size = 64 bytes

{$if StructSize>64}
  {$fatal StructSize cannot exceed serial buffer size}
{$endif}

type
  TBufferData = packed record
    case boolean of
      True: (Values: TComData);
      False: (Buffer: array[0..StructSize - 1] of char);
  end;

type
  TParamRec = record
    StrVal: TStrVal;
    FltVal: single;
  end;

  TTestParamsArray = array of TParamRec;

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
    FSEVal: TFloatSpinEdit;
    GRS232: TGroupBox;
    LValue: TLabel;
    pnlFunctions: TPanel;
    pnlMain: TPanel;
    pnlTools: TPanel;
    chkPredefined: TRadioButton;
    chkManual: TRadioButton;
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

    function ProceedData(aFunc: char; var aFltVal: TRawFloat32; var aStrLen: Int8; var aStrVal: TStrVal; aTimeout: integer): boolean;

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

const
  aTestData: TTestParamsArray = (
                                 (StrVal: '-1'#0; FltVal: -1),
                                 (StrVal: '20.0'#0; FltVal: 20.0),
                                 (StrVal: '1.23456'#0; FltVal: 1.23456)
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
function TTestAVRForm.ProceedData(aFunc: char; var aFltVal: TRawFloat32; var aStrLen: Int8; var aStrVal: TStrVal; aTimeout: integer): boolean;
begin
  if (not RS232.Active) then
  begin
    ShowMessage('Error: RS232 Not Started!!');
    Exit(false);
  end;

  Data.Command := H_Send;
  Data.Func := aFunc;
  Data.FltVal:=aFltVal;
  Data.StrLen:=aStrLen;
  Data.StrVal:=aStrVal;

  ShowSendLog();
  RealSleep(250);
  Result:=SendData(aTimeout);
  if Result then
  begin
    ShowReceiveLog();

    aFltVal:=Data.FltVal;
    aStrLen:=Data.StrLen;
    aStrVal:=Data.StrVal;
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

  chkPredefined.Enabled:=OnOff;
  chkManual.Enabled:=OnOff;
  FSEVal.Enabled:=OnOff;
  LValue.Font.Color:=aLabelColors[OnOff];
  cgbFunctions.Enabled:=OnOff;
  BSelectAll.Enabled:=OnOff;
end;

// *************************
// ***** Show Send Log *****
// *************************

function Str2Float(Value: string): single;
var
  {%H-}c: integer;
begin
  Val(Value, Result, c);
end;

function Float2Str(Value: single): string;
begin
  Str(Value:4:8, Result);
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
  F_Str: Result:= 'StrToFloat32';
  F_Flt: Result:= 'Float32ToStr';
//  F_Tst: Result:= 'Float32Test';
  else Result:='Unknown/Undefined';
  end;
end;

function FuncToIdx(aFunc: char): integer;
begin
  case aFunc of
  F_Str: Result:= 0;
  F_Flt: Result:= 1;
  end;
end;

procedure TTestAVRForm.ShowSendLog();
begin
  MLog.Append('***** Sending:');
  MLog.Append(Format('  Func: %s (%s)', [Data.Func, FuncToStr(Data.Func)]));
  MLog.Append(Format('  Value: %s', [Float32ToStr(Data.FltVal)]));
  MLog.Append(Format('  StrMaxLen: %d', [Data.StrLen]));
  MLog.Append(Format('  String: "%s"', [Data.StrVal]));
  MLog.Refresh();
end;

// ****************************
// ***** Show Receive Log *****
// ****************************
procedure TTestAVRForm.ShowReceiveLog();
begin
  MLog.Append('***** Receiving:');
  MLog.Append(Format('  Func: %s (%s)', [BuffData.Values.Func, FuncToStr(BuffData.Values.Func)]));
  MLog.Append(Format('  Value: %s', [Float32ToStr(BuffData.Values.FltVal)]));
  MLog.Append(Format('  StrLen: %d', [BuffData.Values.StrLen]));
  MLog.Append(Format('  String: "%s"', [BuffData.Values.StrVal]));
  MLog.Refresh();
end;

procedure TTestAVRForm.DoTest(aTestType: char; aDataArray: TTestParamsArray);
const
  TIMEOUT: integer = 1000;
var
  s: single;
  f: TRawFloat32 absolute s;
  i: UInt8;
  idx: integer;

  FltVal: TRawFloat32;
  StrLen: Int8;
  StrVal: TStrVal;
begin
  idx:=FuncToIdx(aTestType);
  if (idx>-1) then
    if cgbFunctions.Checked[idx] then
    begin

      case aTestType of
      F_Str: MLog.Append('TEST STRTOFLOAT');
      F_Flt: MLog.Append('TEST FLOATTOSTR');
      F_Tst: MLog.Append('TEST special test');
      else raise exception.CreateFmt('Unsupported test type "%s"', [aTestType]);
      end;

      for i:=Low(aDataArray) to High(aDataArray) do
      begin
        FillChar({%H-}StrVal, SizeOf(TStrVal), 0);

        case aTestType of
        F_Str: begin //string to float
                 StrCopy(StrVal, aDataArray[i].StrVal);
                 StrLen:=System.StrLen(StrVal)+1; //max len
                 s:=0.0;//Str2Float(aDataArray[i].StrVal);
                 FltVal:=f;
               end;
        F_Flt: begin //float to string
                 StrLen:=SizeOf(TStrVal);
                 FltVal:=SingleToFloat32(aDataArray[i].FltVal);
               end;
        end;

        ProceedData(aTestType, FltVal, Strlen, StrVal, TIMEOUT);
      end;

      MLog.Append('');
    end;
end;

// *************************
// ***** Test 1 Button *****
// *************************
procedure TTestAVRForm.BTestsClick(Sender: TObject);
var
  ManualTestData: TTestParamsArray;
begin
  SetLength({%H-}ManualTestData, 1);
  StrPCopy(ManualTestData[0].StrVal, Float2Str(FSEVal.Value));
  ManualTestData[0].FltVal:=FSEVal.Value;

  //Addintion
  if chkPredefined.Checked then DoTest(F_Str, aTestData)
    else DoTest(F_Str, ManualTestData);

  //Subtraction
  if chkPredefined.Checked then DoTest(F_Flt, aTestData)
    else DoTest(F_Flt, ManualTestData);
end;

end.
