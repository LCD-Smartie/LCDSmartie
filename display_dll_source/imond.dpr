library imond;

{$R *.res}

uses
  Windows,SysUtils,Math;

const
  DLLProjectName = 'Soundgraph iMON DirectWrite Display DLL';
  Version = 'v1.0';
type
  pboolean = ^boolean;
  tUSBPacket = array[0..7] of byte;

var
  FrameIndex : byte;
  hdevice : longint;
  UpdatePackets : byte;
  Packets : array[0..5] of tUSBPacket;

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

function DISPLAYDLL_Init(SizeX,SizeY : byte; StartupParameters : pchar; OK : pboolean) : pchar; stdcall;
// return startup error
// open port
var
  lpDeviceName : string;
  Loop : byte;
  lpSecurityAttributes : TSECURITYATTRIBUTES;
  P : string;
begin
  OK^ := true;
  Result := PChar(DLLProjectName + ' ' + Version+#0);
  FrameIndex := 0;
  hDevice := 0;
  UpdatePackets := 6;
  P := StartupParameters;
  if (P = '5') then UpdatePackets := 5;
  fillchar(Packets,sizeof(Packets),$00);
  for Loop := 0 to 31 do begin
    Packets[Loop div 7][Loop mod 7] := 32;
  end;
  for Loop := 0 to 5 do
    Packets[Loop][7] := Loop*2;
  for Loop := 4 to 6 do
    Packets[4][Loop] := $FF;
  Packets[5][0] := $01;
  Packets[5][5] := $FF;
  Packets[5][6] := $FF;
  try
    lpDeviceName := '\\.\SGIMON'+#0;
    fillchar(lpSecurityAttributes,sizeof(TSECURITYATTRIBUTES),0);
    with lpSecurityAttributes do begin
      nLength := sizeof(TSECURITYATTRIBUTES);
      bInheritHandle := true;
    end;
    hDevice :=
      CreateFile(pchar(lpDeviceName),
                 GENERIC_READ or GENERIC_WRITE,
                 FILE_SHARE_WRITE or FILE_SHARE_READ,
                 @lpSecurityAttributes,
                 OPEN_EXISTING,0,0); // Try and open the device. This will fail if device not present
    if hDevice <= 0 then begin // check for error
      result := PChar('IMOND.DLL: Display not found.'+#0);
      OK^ := false;
      hDevice := 0;
    end;
  except
    on E: Exception do begin
      result := PChar('IMOND.DLL Exception: ' + E.Message + #0);
      OK^ := false;
    end;
  end;
end;

procedure DISPLAYDLL_Done(); stdcall;
// close port
begin
  try
    if hdevice > 0 then
      CloseHandle(hDevice);
    hdevice := 0;
  except
  end;
end;

function DISPLAYDLL_CustomCharIndex(Index : byte) : byte; stdcall;
begin
  DISPLAYDLL_CustomCharIndex := Index + 7; // 8-15
end;

procedure SendPacketToUSB(TxPacket: tUSBPacket);
var
  RetBytes : dword;
begin
  try
    if hdevice > 0 then
      DeviceIOControl(hDevice,$222018,@TxPacket,8,nil,0,RetBytes,nil);
  except
  end;
end;

procedure DISPLAYDLL_Write(Str : pchar); stdcall;
// write string
var
  S : string;
  Loop : byte;
begin
  S := string(Str);
  for Loop := 1 to length(S) do begin
    Packets[FrameIndex div 7][FrameIndex mod 7] := ord(S[Loop]);
    inc(FrameIndex);
    if (FrameIndex > 31) then FrameIndex := 0;
  end;
  for Loop := 0 to UpdatePackets-1 do
    SendPacketToUSB(Packets[Loop]);
end;

procedure DISPLAYDLL_SetPosition(X, Y: byte); stdcall;
// set cursor position
begin
  dec(X);
  dec(Y);
  FrameIndex := min(X,15)+16*min(Y,1);
end;

function DISPLAYDLL_DefaultParameters : pchar; stdcall;
begin
  Result := pchar('6'#0);
end;

function DISPLAYDLL_Usage : pchar; stdcall;
begin
  Result := pchar('Usage: <n>'+#13#10+'n = packets per update,'+#13#10+'6 is standard, 5 is optional.'#0);
end;

function DISPLAYDLL_DriverName : pchar; stdcall;
begin
  Result := PChar(DLLProjectName + ' ' + Version+#0);
end;

// don't forget to export the funtions, else nothing works :)
exports
  DISPLAYDLL_Write,
  DISPLAYDLL_SetPosition,
  DISPLAYDLL_DefaultParameters,
  DISPLAYDLL_CustomCharIndex,
  DISPLAYDLL_Usage,
  DISPLAYDLL_DriverName,
  DISPLAYDLL_Done,
  DISPLAYDLL_Init;
begin
end.

