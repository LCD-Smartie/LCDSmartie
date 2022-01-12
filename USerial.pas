unit USerial;

interface

uses Windows;

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
      constructor Create(uPort: Byte; uiBaud: Cardinal; flags: TSerialFlags);
      destructor Destroy; override;
    private
      hSerial: Cardinal;
  end;

implementation

uses  SysUtils, UUtils;

destructor TSerial.Destroy;
begin
  if (hSerial <> INVALID_HANDLE_VALUE) then
    CloseHandle(hSerial);
  hSerial := INVALID_HANDLE_VALUE;
end;

constructor TSerial.Create(uPort: Byte; uiBaud: Cardinal; flags: TSerialFlags);
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

  if (DTR_ENABLE in flags) then
    ourDCB.Flags := ourDCB.Flags or (1 shl 4);

  if (RTS_ENABLE in flags) then
    ourDCB.Flags := ourDCB.Flags or (1 shl 12);


  ourDCB.BaudRate := uiBaud;
	ourDCB.ByteSize := 8;

  if (ODD_PARITY in flags) then
  	ourDCB.Parity := ODDPARITY
  else if (EVEN_PARITY in flags) then
  	ourDCB.Parity := EVENPARITY
  else
   	ourDCB.Parity := NOPARITY;

  if (TWO_STOPBITS in flags) then
  	ourDCB.StopBits := TWOSTOPBITS
  else
  	ourDCB.StopBits := ONESTOPBIT;

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
