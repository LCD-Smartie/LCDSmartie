library testdriver;

{$R *.res}

uses
  IniFiles,Windows,SysUtils,SyncObjs,Math,SERPORT;

(*

 revhist

1.0 initial driver

*)

const
  DLLProjectName = 'Test Driver Display DLL';
  Version = 'v1.0';
type
  pboolean = ^boolean;
var
  COMPort : TSerialPort = nil;
  sGotoLine : array[1..4] of string = ('','','','');
  sInit : string = '';
  sFini : string = '';
  Charmap : array [0..255] of char;

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

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

// take a string of the form:
// D1,D2,D3...D5,D6 where Dx is 0 to 255
// and convert into a string
// C1C2C3C4C5C6  where Cx is the character with value Dx
function Parse(S : string): string;
var
  S2,Return : string;
  Value : byte;
begin
  Return := '';
  while (S <> '') do begin
    S2 := SubString(S);
    try
      Value := StrToInt(S2);
    except
      Value := 32; // space if we can't figure it out
    end;
    Return := Return + chr(Value);
  end;
  Parse := Return;
end;

procedure ReadINIFile;
const
  Section = 'Test Driver';
  GotoKey = 'GotoLine';
  InitKey = 'Init';
  FiniKey = 'Fini';
  CharMapKey = 'CharMap';
var
  INIFile : TIniFile;
  Loop : longint;
  sCharMap,S : string;
begin
  // set to defaults
  for Loop := 0 to 255 do
    Charmap[Loop] := chr(Loop);

  sInit := extractfilepath(ParamStr(0))+'testdriver.ini';
  INIFile := TIniFile.Create(sInit);
  try
    S := INIFile.ReadString(Section,InitKey,'');
    sInit := Parse(S);
    S := INIFile.ReadString(Section,FiniKey,'');
    sFini := Parse(S);
    for Loop := 1 to 4 do begin
      S := INIFile.ReadString(Section,GotoKey+IntToStr(Loop),'');
      sGotoLine[Loop] := Parse(S);
    end;
    S := INIFile.ReadString(Section,CharMapKey,'');
    sCharMap := Parse(S);
  finally
    INIFile.Free;
  end;

  // replace custom chars with space
  Charmap[Ord('°')] := ' ';
  Charmap[Ord('ž')] := ' ';
  Charmap[131] := ' ';
  Charmap[132] := ' ';
  Charmap[133] := ' ';
  Charmap[134] := ' ';
  Charmap[135] := ' ';
  Charmap[136] := ' ';

  // Process their mapping
  for Loop := 0 to (length(sCharMap) div 2)-1 do
    Charmap[ord(sCharMap[Loop*2+1])] := sCharMap[Loop*2+2];
end;

procedure InitDisplay;
begin
  try
    COMPort.Write(@sInit[1],length(sInit));
  except
  end;
end;

procedure CloseDisplay;
begin
  try
    COMPort.Write(@sFini[1],length(sFini));
  except
  end;
end;

procedure DISPLAYDLL_Write(Str : pchar); stdcall;
// write string
var
  S : string;
  I : integer;
begin
  S := string(Str);
  try
    // Replace all custom chars as spaces.
    for I := 1 to length(S) do
      S[I] := Charmap[ord(S[I])];

    COMPort.Write(@S[1], length(S));
  except
  end;
end;

procedure DISPLAYDLL_SetPosition(X, Y: byte); stdcall;
// set cursor position
var
  S : string;
begin
  Y := min(Y,4);
  try
    if assigned(COMPort) then begin
      S := sGotoLine[Y];
      COMPort.Write(@S[1],length(S));
    end;
  except
  end;
end;

function DISPLAYDLL_Init(SizeX,SizeY : byte; StartupParameters : pchar; OK : pboolean) : pchar; stdcall;
// return startup error
// open port
var
  S,S2 : string;
begin
  OK^ := true;
  Result := PChar(DLLProjectName + ' ' + Version + #0);
  try
    ReadINIFile;
    S2 := uppercase(string(StartupParameters));
    S := substring(S2) + ',' + substring(S2) + ',' + substring(S2) + ',' +
         substring(S2) + ',' + substring(S2);  // get COM1,9600,8,N,1
    S := S + ',$'+ IntToHex(SetRTSFlag+SetDTRFlag,8);
    COMPort := TSerialPort.Create;
    COMPort.OpenSerialPort(S);
    InitDisplay;
  except
    on E: Exception do begin
      result := PChar('TESTDRIVER.DLL Exception: ' + E.Message + #0);
      OK^ := false;
    end;
  end;
end;

procedure DISPLAYDLL_Done(); stdcall;
// close port
begin
  try
    if assigned(COMPort) then begin
      CloseDisplay;
      COMPort.Free;
      COMPort := nil;
    end;
  except
  end;
end;

function DISPLAYDLL_DefaultParameters : pchar; stdcall;
begin
  DISPLAYDLL_DefaultParameters := pchar('COM1,9600,8,N,1' + #0);
end;

function DISPLAYDLL_Usage : pchar; stdcall;
begin
  Result := pchar('Usage: COM1,9600,8,N,1' + #0);
end;

function DISPLAYDLL_DriverName : pchar; stdcall;
begin
  Result := PChar(DLLProjectName + ' ' + Version + #0);
end;

// don't forget to export the funtions, else nothing works :)
exports
  DISPLAYDLL_Write,
  DISPLAYDLL_SetPosition,
  DISPLAYDLL_DefaultParameters,
  DISPLAYDLL_Usage,
  DISPLAYDLL_DriverName,
  DISPLAYDLL_Done,
  DISPLAYDLL_Init;
begin
end.

