library seetron;

{$R *.res}

uses
  Windows,SysUtils,StrUtils,SyncObjs,Math,SERPORT;

(*

 revhist

1.0 initial driver
1.1 added backlight off on shutdown

*)

const
  DLLProjectName = 'Seetron Display DLL';
  Version = 'v1.1';
type
  pboolean = ^boolean;
  TCustomArray = array[0..7] of byte;
  TProtocol = (prLCD,prTerminal);
var
  COMPort : TSerialPort = nil;
  Protocol : TProtocol = prLCD;
  LineLength : byte = 16;
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
  DISPLAYDLL_CustomCharIndex := 127+Index;
  case Protocol of
    prLCD : DISPLAYDLL_CustomCharIndex := 7+Index;
  end;
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
      case Protocol of
        prLCD : begin
          Posit :=  128 + (min(X,40) - 1) + (((Y-1) div 2) * LineLength) + (64*((Y-1) mod 2));
          COMPort.WriteByte(254);
          COMPort.WriteByte(Posit);
        end;
        prTerminal : begin
          Posit :=  64 + (min(X,LineLength) - 1) + (LineLength*(min(Y,4)-1));
          COMPort.WriteByte(16);  // P
          COMPort.WriteByte(Posit);
        end;                                       
      end;
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
      case Protocol of
        prLCD : begin
          Address := 64 + (Chr - 1)*8;
          COMPort.WriteByte(254);
          COMPort.WriteByte(Address);
          for Address := 0 to 7 do
            COMPort.WriteByte(Data[Address]);
          DISPLAYDLL_SetPosition(CurrentX,CurrentY);
        end;
        prTerminal : begin
          Address := 48 + (Chr - 1);
          COMPort.WriteByte(27);  // esc
          COMPort.WriteByte(ord('D'));  // "D"
          COMPort.WriteByte(Address);
          for Address := 0 to 7 do
            COMPort.WriteByte(Data[Address]);
        end;
      end;
    end;
  except
  end;
end;

procedure DISPLAYDLL_SetBrightness(Brightness : byte); stdcall;
// VFD only
begin
  Brightness := Brightness div 64;  // 0-3 is the brightness
  try
    if assigned(COMPort) then begin
      case Protocol of
        prLCD : begin
          // no VFDs with this protocol
        end;
        prTerminal : begin
          COMPort.WriteByte(27);  // esc
          COMPort.WriteByte(48 + Brightness);  // ascii "0" - "3"
        end;
      end;
    end;
  except
  end;
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
      case Protocol of
        prLCD : begin
          // backlighting switch controlled, not software controlled
        end;
        prTerminal : begin
          if LightOn then COMPort.WriteByte(14)
          else COMPort.WriteByte(15);
        end;
      end;
    end;
  except
  end;
end;

procedure DISPLAYDLL_SetGPO(GPO : byte; GPOOn : boolean); stdcall;
// turn on GPO
begin
  try
    if assigned(COMPort) then begin
      case Protocol of
        prLCD : begin
          // no GPO on this display
        end;
        prTerminal : begin
          COMPort.WriteByte(7);  // bell
        end;
      end;
    end;
  except
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

function DISPLAYDLL_Init(SizeX,SizeY : byte; StartupParameters : pchar; OK : pboolean) : pchar; stdcall;
// return startup error
// open port
var
  S,S2,P : string;
begin
  LineLength := SizeX;
  OK^ := true;
  Result := PChar(DLLProjectName + ' ' + Version + #0);
  try
    S2 := uppercase(string(StartupParameters));
    S := substring(S2) + ',' + substring(S2);  // get COM1,9600
    P := SubString(S2);
    Protocol := prLCD;
    if (P = '2') then Protocol := prTerminal;
    S := S + ',8,N,1';
    COMPort := TSerialPort.Create;
    COMPort.OpenSerialPort(S);
  except
    on E: Exception do begin
      result := PChar('SEETRON.DLL Exception: ' + E.Message + #0);
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
  DISPLAYDLL_DefaultParameters := pchar('COM1,9600,1' + #0);
end;

function DISPLAYDLL_Usage : pchar; stdcall;
begin
  Result := pchar('Usage: COM1,9600,p'+#13#10+
                  'p = Protocol'+#13#10+
                  '1=LCD 2=Terminal' + #0);
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

