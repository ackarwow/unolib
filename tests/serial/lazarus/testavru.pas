unit TestAVRU;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF UNIX}
	{$IFDEF UseCThreads}
	 cthreads,
	{$ENDIF}
{$ENDIF}
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, PairSplitter, FileUtil,
	{$IFDEF WINDOWS}
	registry,
	{$ENDIF}
LazSerial;

const
	H_Send = 'S'; // Send
	H_Receive = 'R'; // Receive
	H_Error = 'E'; // Error
	H_Ready = 'K'; // Ready
 H_Info = 'I'; // Info

	ComName: Array of String = ('Test1', 'Test2');
type
	ComValues = (Test1, Test2);

type
	ComData = packed record
		Command: Char; // Header Command
		Value: Int16; // Command Value
		Data: Int16; // Data
		DataEx: Int32; // Data Extended
		Code: Int16; // Error Code
		Count: Int16;
	end;

const StructSize = SizeOf(ComData);

type
	BufferData = packed record
		case boolean of
			true: (Values: ComData);
			false: (Buffer: array[0..StructSize-1] of char);
end;

type

	{ TTestAVRForm }

 TTestAVRForm = class(TForm)
		BRS232Start: TButton;
		BTest1: TButton;
		BTest2: TButton;
		BExit: TButton;
		CBBaudRate: TComboBox;
		CBDataBits: TComboBox;
		CBFlow: TComboBox;
		CBParity: TComboBox;
		CBRS232Port: TComboBox;
		CBStopBits: TComboBox;
		GRS232: TGroupBox;
//		RS232: TLazSerial;
		LBaudrate: TLabel;
		LDataBits: TLabel;
		LFlow: TLabel;
		LParity: TLabel;
		LRS232Port: TLabel;
		LStopBits: TLabel;
		MainSplitter: TPairSplitter;
		MLog: TMemo;
		RS232: TLazSerial;
		SplitterRS232: TPairSplitterSide;
		SplitterLog: TPairSplitterSide;
		RBRS232: TRadioButton;
		procedure BExitClick(Sender: TObject);
  procedure BRS232StartClick(Sender: TObject);
		procedure BTest1Click(Sender: TObject);
		procedure BTest2Click(Sender: TObject);
  procedure CBBaudRateChange(Sender: TObject);
		procedure CBDataBitsChange(Sender: TObject);
		procedure CBFlowChange(Sender: TObject);
		procedure CBParityChange(Sender: TObject);
  procedure CBRS232PortChange(Sender: TObject);
		procedure CBStopBitsChange(Sender: TObject);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure MainSplitterResize(Sender: TObject);
		procedure RS232RxData(Sender: TObject);

		function SendData(Timeout: QWord):Boolean;

		procedure ProceedData(DValue: Int16; // Command Value
																							DData: Int16; // Data
																							DDataEx: Int32; // Data Extended
																							DCode: Int16; // Error Code
																							DCount: Int16; // Time
																							Timeout: Integer);  // Timeout

		function WaitForReady(Timeout: QWord): boolean;

		procedure SetButtons(OnOff: Boolean);

		procedure ShowSendLog();
		procedure ShowReceiveLog();

	private
	// ***** RS232 *****
		Data: ComData;
		BuffData: BufferData;
		ACK: Boolean;
//		RS232: TLazSerial;

	public

	end;

var
	TestAVRForm: TTestAVRForm;

implementation

{$R *.lfm}

// ********************************************
// ***** Real Sleep with Messages Process *****
// ********************************************
procedure RealSleep(DelayTickCount: QWORD);
var
  StartTickCount : QWORD;
begin
	StartTickCount := GetTickCount64;
	while (GetTickCount64 < StartTickCount + DelayTickCount) and (not Application.Terminated) do
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
 Cnt: Integer;
begin

	 // RS232 init
//	RS232 := TLazSerial.Create(Self);
//	RS232.OnRxData := @RS232RxData;
	COMNames := GetSerialPorts;
	CBRS232Port.Clear;

	for Cnt := 0 to Pred(COMNames.Count) do
		CBRS232Port.Items.Add(COMNames[Cnt]);
		COMNames.Free;
	if(CBRS232Port.Items.Count <> -1) then CBRS232Port.ItemIndex := 0;


 	if(CBRS232Port.Items.Count <> 0) then RS232.Device := CBRS232Port.Items[0];

 	CBBaudRate.ItemIndex := 11;
 	RS232.BaudRate :=  br_57600;

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
procedure TTestAVRForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
	);
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

procedure TTestAVRForm.MainSplitterResize(Sender: TObject);
begin
 MainSplitter.Position := 250;
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
	RXData: String;
	BuffSize, i: Integer;
	TChar: TCharArray;
begin
	if(not RS232.DataAvailable) then
	begin
		Exit;
	end;
	RBRS232.Checked := not RBRS232.Checked;
	RBRS232.Refresh;
	RXData := '';

	RXData := RS232.ReadData;

	// Message Header Unknown
	if(not (RxData.Chars[0] in [H_Receive, H_Ready, H_Error, H_Info])) then
	begin
		MLog.Append('Error: Unknown Header: "' + RxData.Chars[0] + '"');
		while(RS232.DataAvailable) do
  begin
   RS232.ReadData;
   RealSleep(10);
  end;
		exit;
	end;

	BuffSize := RXData.Length;

// Case Fragmentation
	while(BuffSize < StructSize) do
	begin
		BuffSize := RXData.Length;
		if( RS232.DataAvailable) then RXData := RXDAta + RS232.ReadData;
  RealSleep(10);
	end;

	if(BuffSize = StructSize) then
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
		while(RS232.DataAvailable) do RS232.ReadData;
		exit;
	end;

	// Buffer loaded
	// Check Command
	case  BuffData.Values.Command of
		H_Error:
 		begin
				MLog.Append('Error: Arduino returned Error Code: '+ BuffData.Values.Code.ToString);
    ACK := True;
			end;

  H_Receive: // Acnowledge
			begin
				RealSleep(25);
				if(BuffData.Values.Value = Data.Value) then
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
				while(RS232.DataAvailable) do
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

// ****************
// ***** Exit *****
// ****************
procedure TTestAVRForm.BExitClick(Sender: TObject);
begin
	Close;
end;

// ***************************
// ***** RS232 Send Data *****
// ***************************
function TTestAVRForm.SendData(Timeout: QWord): Boolean;
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
	While(not ACK) do
	begin
		if(GetTickCount64 > StartTime) then
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
procedure TTestAVRForm.ProceedData(DValue: Int16; DData: Int16; DDataEx: Int32;
	DCode: Int16; DCount: Int16; Timeout: Integer);
begin
	if(Not RS232.Active) then
	begin
		ShowMessage('Error: RS232 Not Started!!');
		exit;
	end;

	Data.Command := H_Send;
	Data.Value := DValue;
	Data.Data := DData;
	Data.DataEx := DDataEx;
	Data.Code := DCode;
	Data.Count := DCount;

	ShowSendLog();
 RealSleep(250);
	if(SendData(Timeout)) then
		ShowReceiveLog()
	else
		Mlog.Append('Error: Timeout on Acknowledge');
end;

// ***************************************
// ***** Enable Disable Test buttons *****
// ***************************************
procedure TTestAVRForm.SetButtons(OnOff: Boolean);
begin
	if(OnOff) then
	begin
		BTest1.Visible := True;
		BTest1.Enabled := True;
		BTest2.Visible := True;
		BTest2.Enabled := True;
	end
	else
	begin
		BTest1.Visible := False;
		BTest1.Enabled := False;
		BTest2.Visible := False;
		BTest2.Enabled := False;
	end;
end;

// *************************
// ***** Show Send Log *****
// *************************
procedure TTestAVRForm.ShowSendLog();
begin
	MLog.Append('***** Sending:');
	MLog.Append('  Value: ' + ComName[Data.Value]);
	MLog.Append('  Data: ' + Data.Data.ToString);
	MLog.Append('  DataEx: ' + Data.DataEx.ToString);
	MLog.Append('  Code: ' + Data.Code.ToString);
	MLog.Append('  Count: ' + Data.Count.ToString);
	MLog.Refresh();
end;

// ****************************
// ***** Show Receive Log *****
// ****************************
procedure TTestAVRForm.ShowReceiveLog();
begin
	MLog.Append('***** Receiving:');
	MLog.Append('  Value: ' + ComName[BuffData.Values.Value]);
	MLog.Append('  Data: ' + BuffData.Values.Data.ToString);
	MLog.Append('  DataEx: ' + BuffData.Values.DataEx.ToString);
	MLog.Append('  Code: ' + BuffData.Values.Code.ToString);
	MLog.Append('  Count: ' + BuffData.Values.Count.ToString);
	MLog.Refresh();
end;

// *************************
// ***** Test 1 Button *****
// *************************
procedure TTestAVRForm.BTest1Click(Sender: TObject);
begin
	ProceedData(int16(Test1), 0, 0, 0, 0, 1000);
end;

// *************************
// ***** Test 2 Button *****
// *************************
procedure TTestAVRForm.BTest2Click(Sender: TObject);
begin
	ProceedData(int16(Test2), 0, 0, 0, 0, 1000);
end;

end.

