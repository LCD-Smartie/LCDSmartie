library pertelian;

{$R *.res}

uses
  Windows,SysUtils,StrUtils,SyncObjs,Math,SERPORT;

(*

 revhist

1.0 initial driver

*)

const
  DLLProjectName = 'Pertelian Display DLL';
  Version = 'v1.0';
type
  pboolean = ^boolean;
  TCustomArray = array[0..7] of byte;
var
  COMPort : TSerialPort = nil;
  LineLength : byte = 20;
  CurrentX : byte = 1;
  CurrentY : byte = 1;


/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

function DISPLAYDLL_ReadKey : word; stdcall;
// return 00xx upon success, FF00 on fail
begin
  Result := $FF00;
  try
  except
    // seetron displays have varying inputs which I am way too lazy to code
  end;
end;

function DISPLAYDLL_CustomCharIndex(Index : byte) : byte; stdcall;
begin
  DISPLAYDLL_CustomCharIndex := 7+Index;
end;

procedure DISPLAYDLL_Write(Str : pchar); stdcall;
// write string
var
  S : string;
begin
  S := string(Str);
  try
    COMPort.Write(@S[1], length(S));
  except
  end;
end;

procedure DISPLAYDLL_SetPosition(X, Y: byte); stdcall;
// set cursor position
var
  Posit : byte;
begin
  CurrentX := X;
  CurrentY := Y;
  try
    if assigned(COMPort) then begin
      Posit :=  128 + (min(X,40) - 1) +  ($40*((Y-1) mod 2)) + (((Y-1) div 2) * LineLength);
      COMPort.WriteByte(254);
      COMPort.WriteByte(Posit);
    end;
  except
  end;
end;

procedure DISPLAYDLL_CustomChar(Chr : byte; Data : TCustomArray); stdcall;
// define custom character
var
  Address : byte;
begin
  try
    if assigned(COMPort) then begin
      Address := 64 + (Chr - 1)*8;
      COMPort.WriteByte(254);
      COMPort.WriteByte(Address);
      for Address := 0 to 7 do
        COMPort.WriteByte(Data[Address]);
      DISPLAYDLL_SetPosition(CurrentX,CurrentY);
    end;
  except
  end;
end;

procedure DISPLAYDLL_SetBrightness(Brightness : byte); stdcall;
// VFD only
begin
// no VFDs with this protocol
end;

procedure DISPLAYDLL_SetContrast(Contrast : byte); stdcall;
begin
// these displays do not appear to have a software selectable contrast
end;

procedure DISPLAYDLL_SetBacklight(LightOn : boolean); stdcall;
// turn backlighting on/off
begin
  try
    if assigned(COMPort) then begin
      COMPort.WriteByte($FE);
      if LightOn then COMPort.WriteByte($03)
      else COMPort.WriteByte($02);
    end;
  except
  end;
end;

procedure DISPLAYDLL_SetGPO(GPO : byte; GPOOn : boolean); stdcall;
// turn on GPO
begin
// no GPO on this display
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

procedure InitDisplay;
begin
  COMPort.WriteByte($FE);
  COMPort.WriteByte($38);  // 8 bit, 2 lines, 5x8 font
  COMPort.WriteByte($FE);
  COMPort.WriteByte($06);  // cursor increments, no display shift
  COMPort.WriteByte($FE);
  COMPort.WriteByte($10);  // cursor move
  COMPort.WriteByte($FE);
  COMPort.WriteByte($0C);  // display on, no blinking cursor
  COMPort.WriteByte($FE);
  COMPort.WriteByte($01);  // clear display
end;

function DISPLAYDLL_Init(SizeX,SizeY : byte; StartupParameters : pchar; OK : pboolean) : pchar; stdcall;
// return startup error
// open port
var
  S,S2 : string;
begin
  LineLength := SizeX;
  OK^ := true;
  Result := PChar(DLLProjectName + ' ' + Version + #0);
  try
    S2 := uppercase(string(StartupParameters));
    S := substring(S2) + ',' + substring(S2);  // get COM1,9600
    S := S + ',8,N,1';
    COMPort := TSerialPort.Create;
    COMPort.OpenSerialPort(S);
    InitDisplay;
  except
    on E: Exception do begin
      result := PChar('PERTELIAN.DLL Exception: ' + E.Message + #0);
      OK^ := false;
    end;
  end;
end;

procedure DISPLAYDLL_Done(); stdcall;
// close port
begin
  try
    DISPLAYDLL_SetBacklight(false);
    if assigned(COMPort) then begin
      COMPort.Free;
      COMPort := nil;
    end;
  except
  end;
end;

function DISPLAYDLL_DefaultParameters : pchar; stdcall;
begin
  DISPLAYDLL_DefaultParameters := pchar('COM1,9600' + #0);
end;

function DISPLAYDLL_Usage : pchar; stdcall;
begin
  Result := pchar('Usage: COM1,9600' + #0);
end;

function DISPLAYDLL_DriverName : pchar; stdcall;
begin
  Result := PChar(DLLProjectName + ' ' + Version + #0);
end;

// don't forget to export the funtions, else nothing works :)
exports
  DISPLAYDLL_SetGPO,
  DISPLAYDLL_SetBrightness,
  DISPLAYDLL_SetContrast,
  DISPLAYDLL_SetBacklight,
  DISPLAYDLL_ReadKey,
  DISPLAYDLL_CustomChar,
  DISPLAYDLL_CustomCharIndex,
  DISPLAYDLL_Write,
  DISPLAYDLL_SetPosition,
  DISPLAYDLL_DefaultParameters,
  DISPLAYDLL_Usage,
  DISPLAYDLL_DriverName,
  DISPLAYDLL_Done,
  DISPLAYDLL_Init;
begin
end.

