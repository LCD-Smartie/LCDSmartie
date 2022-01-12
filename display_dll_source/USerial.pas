unit USerial;

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
  TSerialFlag = (RTS_ENABLE, DTR_ENABLE, TWO_STOPBITS, EVEN_PARITY, ODD_PARITY);
  TSerialFlags = set of TSerialFlag;

  TSerial = class(TObject)
    public
      function Read(var onebyte: Byte): Boolean; overload;
      function Read(buffer: pointer; uiSize: Cardinal): Cardinal; overload;
      procedure Write(var buffer: array of byte; uiSize: Cardinal); overload;
      procedure Write(buffer: PByte; uiSize: Cardinal); overload;
      procedure Write(onebyte: byte); overload;
      procedure Write(const str: String); overload;
      procedure OpenSerialPort(S : string);
      constructor Create(uPort: Byte; uiBaud: Cardinal; flags: TSerialFlags);
      constructor AlternateCreate;
      destructor Destroy; override;
    private
      hSerial: Cardinal;
      procedure CreateCommPort(uPort: Byte; uiBaud: Cardinal; Bits,bParity,Stop : byte; flags: Word);
  end;

implementation

uses
  SysUtils{, UUtils};

function errMsg(uError: Cardinal): String;
var
  psError: pointer;
  sError: String;
begin
  if (uError <> 0) then
  begin
    if (FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
      nil, uError, 0, @psError, 0, nil ) = 0) then psError := nil;

    if (psError <> nil) then
    begin
      sError := '#' + IntToStr(uError) + ': ' + PChar(psError);
      LocalFree(Cardinal(psError));
    end
    else
      sError := '#' + IntToStr(uError);
    Result := sError;
  end
  else
    Result := '#0'; // don't put "operation completed successfully!" It's too confusing!
end;


destructor TSerial.Destroy;
begin
  if (hSerial <> INVALID_HANDLE_VALUE) then
    CloseHandle(hSerial);
  hSerial := INVALID_HANDLE_VALUE;
end;

procedure TSerial.CreateCommPort(uPort: Byte; uiBaud: Cardinal; Bits,bParity,Stop : byte; flags: Word);
      // 0-4=no,odd,even,mark,space
      // 0,1,2 = 1, 1.5, 2
var
  ourDCB: DCB;
  ourTimeouts: COMMTIMEOUTS;
begin
  inherited Create;

  hSerial := 1;

  hSerial := CreateFile(PAnsiChar('\\.\COM' + IntToStr(uPort)),
    GENERIC_READ or GENERIC_WRITE,
    0  {exclusive access},		nil {no security attribs},
		OPEN_EXISTING,		0 {not overlapped I/O},  0 {no template});

	if(hSerial = INVALID_HANDLE_VALUE) then
    raise Exception.create('Failed to open COM'+IntToStr(uPort)+': '
      + errMsg(GetLastError));

  if (not GetCommState(hSerial, ourDCB)) then
     raise Exception.create('Failed to get CommState for COM'+IntToStr(uPort)+': '
      + errMsg(GetLastError));

  // Set required flags - delphi's copy of DCB doesn't contain bit flags...
  ourDCB.Flags := ourDCB.Flags or
      ((1 shl 0) // fBinary
      //or (1 shl 1) // fParity
      //or (1 shl 2) // fOutxCtxFlow
      //or (1 shl 3) // fOutxDsrFlow
      // or (1 shl 4) // fDtrControl1
      // or (1 shl 5) // fDtrControl2
      //or (1 shl 6) // fDtrSensitivity
      //or (1 shl 7) // fTXContinueOnXOff
      //or (1 shl 8) // fOutX
      //or (1 shl 9) // fInX
      //or (1 shl 10) // fErrorChar
      //or (1 shl 11) // fNull
      //or (1 shl 12) // fRtsControl1
      //or (1 shl 13) // fRtsControl2
      //or (1 shl 14) // fAbortOnError
      //or (1 shl 15)); // fDummy2
      );
  // reset unneeded flags
  ourDCB.Flags := ourDCB.Flags and not
      (//(1 shl 0) // fBinary
         (1 shl 1) // fParity
      or (1 shl 2) // fOutxCtxFlow
      or (1 shl 3) // fOutxDsrFlow
      or (1 shl 4) // fDtrControl1
      or (1 shl 5) // fDtrControl2
      or (1 shl 6) // fDtrSensitivity
      //or (1 shl 7) // fTXContinueOnXOff
      or (1 shl 8) // fOutX
      or (1 shl 9) // fInX
      or (1 shl 10) // fErrorChar
      or (1 shl 11) // fNull
      or (1 shl 12) // fRtsControl1
      or (1 shl 13) // fRtsControl2
      or (1 shl 14) // fAbortOnError
      //or (1 shl 15) // fDummy2
      );

  ourDCB.Flags := ourDCB.Flags or Flags;

  ourDCB.BaudRate := uiBaud;
  ourDCB.ByteSize := Bits;
  ourDCB.Parity := bParity;
  ourDCB.StopBits := Stop;

  if (not SetCommState(hSerial, ourDCB)) then
     raise Exception.create('Failed to set CommState for COM'+IntToStr(uPort)+': '
      + errMsg(GetLastError));

  if(not GetCommTimeouts(hSerial, ourTimeouts)) then
    raise Exception.create('Failed to get CommTimeouts for COM'+IntToStr(uPort)+': '
      + errMsg(GetLastError));

	ourTimeouts.ReadIntervalTimeout:=2;
	ourTimeouts.ReadTotalTimeoutMultiplier:=0;
	ourTimeouts.ReadTotalTimeoutConstant:=50;

 	ourTimeouts.WriteTotalTimeoutMultiplier:=0;
	ourTimeouts.WriteTotalTimeoutConstant:=200;

	if (not SetCommTimeouts(hSerial, ourTimeouts)) then
    raise Exception.create('Failed to set CommTimeouts for COM'+IntToStr(uPort)+': '
      + errMsg(GetLastError));

  // Give serial device enough time to boot.
  // [Some devices are powered by the RTS/DTR lines and would have just been
  // powered up and are now booting.]
  Sleep(500);
end;

constructor TSerial.Create(uPort: Byte; uiBaud: Cardinal; flags: TSerialFlags);
var
  DCBFlags : word;
  DCBParity,DCBStopBits : byte;
begin
  inherited Create;
  DCBFlags := 0;
  if (DTR_ENABLE in flags) then
    DCBFlags := DCBFlags or SetDTRFlag;
  if (RTS_ENABLE in flags) then
    DCBFlags := DCBFlags or SetRTSFlag;

  if (ODD_PARITY in flags) then
    DCBParity := ODDPARITY
  else if (EVEN_PARITY in flags) then
    DCBParity := EVENPARITY
  else
    DCBParity := NOPARITY;

  if (TWO_STOPBITS in flags) then
    DCBStopBits := TWOSTOPBITS
  else
    DCBStopBits := ONESTOPBIT;


  CreateCommPort(uPort,uiBaud,8,DCBParity,DCBStopBits,DCBFlags);
end;

constructor TSerial.AlternateCreate;
begin
  inherited Create;
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

procedure TSerial.OpenSerialPort(S : string);
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
{
  PortName := '\\.\COM'+inttostr(PortNumber);
  InitComPort(PortName,COMPortBaud,Bits,Parity,Stop,COMFlags);
}
  CreateCommPort(PortNumber,COMPortBaud,Bits,Parity,Stop,COMFlags);
end;

function TSerial.Read(buffer: pointer; uiSize: Cardinal): Cardinal;
var
  bytesRead: Cardinal;
  p: PByte;
begin
  p := buffer;
  bytesRead := 0;
  if (not ReadFile(hSerial, p^, uiSize, bytesRead, nil)) then
  begin
    raise Exception.Create('read failed: '+errMsg(GetLastError()));
  end;

  Result := bytesRead;
end;

function TSerial.Read(var onebyte: Byte): Boolean;
begin
  if (Read(@onebyte, 1) = 1) then
    Result := true
  else
    Result := false;
end;

procedure TSerial.Write(buffer: PByte; uiSize: Cardinal);
var
  bytesWritten: Cardinal;
begin
  if (not WriteFile(hSerial, buffer^, uiSize, bytesWritten, nil)) then
  begin
    raise Exception.Create('write failed: '+errMsg(GetLastError()));
  end;
end;

procedure TSerial.Write(var buffer: array of byte; uiSize: Cardinal);
begin
  Write(@buffer[0], uiSize);
end;

procedure TSerial.Write(onebyte: byte);
begin
  Write(@onebyte, 1);
end;

procedure TSerial.Write(const str: String);
begin
  if (Length(str)>0) then
    Write(@str[1], Length(str));
end;

end.
