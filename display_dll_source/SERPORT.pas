unit SERPORT;

{$MODE Delphi}

interface

uses
  Windows;

const
  { communications mode flag bitmask constants }
  BinaryModeFlag = $00000001;
  ParityEnableFlag = $00000002;
  CTSOutputControlFlag = $00000004;
  DSROutputControlFlag = $00000008;
  DTRFlowControlTypeMask = $00000030;
  SetDTRFlag = $00000010;
  DTRHandshakingFlag = $00000020;
  DSRSensitivityFlag = $00000040;
  TXContinueOnXOffFlag = $00000080;
  XonXoffOutputFlowControlFlag = $00000100;
  XonXoffInputFlowControlFlag = $00000200;
  ErrorReplacementEnableFlag = $00000400;
  NullStrippingEnableFlag = $00000800;
  RTSFlowControlMask = $00003000;
  SetRTSFlag = $00001000;
  RTSHandshakingFlag = $00002000;
  AbortOnErrorEnableFlag = $00004000;
  ReservedFlagBitsMask = $FFFF8000;

type
  TSerialPort = class
  private
    fCommHandle : THandle;
    function InitComPort(FComPortName : string; FComPortBaud : longint; Bits,bParity,Stop : byte; COMFlags : word) : boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CloseSerialPort;
    procedure OpenSerialPort(S : string);
    function  WriteByte(B : byte) : boolean;
    function  ReadByte(var B : byte) : boolean;
    function  Write(Buf : pbyte; Len : DWORD) : boolean;
    function  Read(Buf : pbyte; Len : DWORD) : dword;
  end;

function SubString(var S : string) : string;

implementation

uses
  SysUtils;

constructor TSerialPort.Create;
begin
  fCommHandle := 0;
end;

destructor TSerialPort.Destroy;
begin
  CloseSerialPort;
end;

procedure TSerialPort.CloseSerialPort;
begin
  if (fCommHandle > 0) then begin
    FlushFileBuffers(fCommHandle);
    FileClose(fCommHandle); { *Converted from CloseHandle* }
  end;
end;

function TSerialPort.InitComPort(FComPortName : string; FComPortBaud : longint; Bits,bParity,Stop : byte; COMFlags : word) : boolean;
      // 0-4=no,odd,even,mark,space
      // 0,1,2 = 1, 1.5, 2
var
  Security : TSECURITYATTRIBUTES;
  Timeouts : TCOMMTIMEOUTS;
  CommDCB : TDCB;
  PortName,TempError : string;
begin
  Result := false;
  if (FComPortName = '') then begin
    raise EInOutError.Create('Illegal com port name!');
  end else begin
    PortName := FComPortName + #0;
    fillchar(Security,sizeof(TSECURITYATTRIBUTES),0);
    with Security do begin
      nLength := sizeof(TSECURITYATTRIBUTES);
      bInheritHandle := true;
    end;
//  BuildCommDCB('9600,n,8,1',CommDCB);     { 1.11 this will crash in Win98 }
    fillchar(CommDCB,sizeof(CommDCB),$00); // 1.11
    with CommDCB do begin
      DCBLength := sizeof(CommDCB);
      BaudRate := FComPortBaud;
      Flags := Flags or BinaryModeFlag or COMFlags;
      ByteSize := Bits;
      Parity := bParity;
      StopBits := Stop;
    end;
    fillchar(Timeouts,sizeof(TCOMMTIMEOUTS),0);
    Timeouts.ReadIntervalTimeout := $FFFFFFFF; { forces immediate read returns }
    fCommHandle :=
      CreateFile(@PortName[1], { address of name of the file }
                 GENERIC_READ or GENERIC_WRITE, { access (read-write) mode }
                 0, { share mode }
                 @Security, { address of security descriptor } {necessary?}
                 OPEN_EXISTING, { how to create }
                 FILE_ATTRIBUTE_NORMAL,{ file attributes } {necessary?}
                 0); { handle of file with attributes to copy }

    if (fCommHandle <= 0)
      or not SetCommState(fCommHandle,CommDCB)
        or not SetCommTimeouts(fCommHandle,Timeouts)
    then begin
      if (fCommHandle > 0) then FileClose(fCommHandle); { *Converted from CloseHandle* }
      fCommHandle := 0;
      TempError := SysErrorMessage(GetLastError);
      raise EInOutError.Create(TempError);
    end else begin
      InitComPort := true;
    end;
  end;
end;

function SubString(var S : string) : string;
var
  P : longint;
begin
  P := pos(',',S);
  if (P > 0) then begin
    SubString := uppercase(trim(copy(S,1,P-1)));
    delete(S,1,P);
  end else begin
    SubString := uppercase(trim(S));
    S := '';
  end;
end;

procedure TSerialPort.OpenSerialPort(S : string);
// COM1,9600,8,N,1,0  // flags in DECIMAL or HEX format
var
  S2,PortName : string;
  ComPortBaud : longint;
  PortNumber,Bits,Parity,Stop : byte;
  COMFlags : word;
begin
  S2 := SubString(S);
  if (pos('COM',S2) = 1) then delete(S2,1,3);
  try
    PortNumber := StrToInt(S2);
  except
    raise EInOutError.Create('Invalid COM port name');
  end;
  S2 := SubString(S);
  try
    COMPortBaud := StrToInt(S2);
  except
    raise EInOutError.Create('Invalid Baud Rate');
  end;
  S2 := SubString(S);
  try
    Bits := StrToInt(S2);
  except
    raise EInOutError.Create('Invalid Data Bits');
  end;
  S2 := SubString(S);
  case S2[1] of
     // 0-4=no,odd,even,mark,space
    'N' : Parity := 0;
    'O' : Parity := 1;
    'E' : Parity := 2;
    'M' : Parity := 3;
    'S' : Parity := 4;
    else raise EInOutError.Create('Invalid Parity');
  end;
  S2 := SubString(S);
  if (S2 = '1') then Stop := 0
  else if (S2 = '1.5') then Stop := 1
  else if (S2 = '2') then Stop := 2
  else raise EInOutError.Create('Invalid Stop Bits');
  S2 := SubString(S);
  COMFlags := 0;
  if (S2 <> '') then begin
    try
      COMFlags := StrToInt(S2);
    except
      raise EInOutError.Create('Invalid Flags Field');
    end;
  end;
  PortName := '\\.\COM'+inttostr(PortNumber);
  InitComPort(PortName,COMPortBaud,Bits,Parity,Stop,COMFlags);
end;

function TSerialPort.ReadByte(var B : byte) : boolean;
var
  Bytes : DWORD;
begin
  ReadByte := ReadFile(fCommHandle,B,1,Bytes,nil) and (Bytes = 1);
end;

function TSerialPort.WriteByte(B : byte) : boolean;
var
  Bytes : DWORD;
begin
  WriteByte := WriteFile(fCommHandle,B,1,Bytes,nil) and (Bytes = 1);
end;

function TSerialPort.Write(Buf : pbyte; Len : DWORD) : boolean;
var
  Bytes : DWORD;
begin
  Write := WriteFile(fCommHandle,Buf^,Len,Bytes,nil) and (Bytes = Len);
end;

function TSerialPort.Read(Buf : pbyte; Len : DWORD) : dword;
var
  Bytes : DWORD;
begin
  Read := 0;
  if ReadFile(fCommHandle,Buf^,Len,Bytes,nil) then
    Read := Bytes;
end;

end.
