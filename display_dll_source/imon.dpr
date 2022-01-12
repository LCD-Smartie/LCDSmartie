library imon;

{$R *.res}

uses
  Windows,SysUtils,Math;

//uses SysUtils,StrUtils,SyncObjs,SERPORT;

(*
#define VFDHW_IMON_VFD		4

///////////////////////////////////////////////
//	Open VFD driver and initialize parameters.
//	Call this method, when application starts.
//	Return value informs driver is open or not
IMONVFD_API bool iMONVFD_Init(int vfdType, int resevered=0);

///////////////////////////////////////////////
//	Close VFD driver.
//	Call this method, when application destroyed.
IMONVFD_API void iMONVFD_Uninit(void);

///////////////////////////////////////////////
//	Check if VFD driver is opened.
IMONVFD_API bool iMONVFD_IsInited(void);

///////////////////////////////////////////////
//	Send text data to VFD. VFD supports only English character set.
IMONVFD_API bool iMONVFD_SetText(char* szFirstLine, char* szSecondLine);

///////////////////////////////////////////////
//	Send EQ data to VFD.
//	Total 16band, each band ranges from 0 to 100
//  make EQ data with integer array.
IMONVFD_API bool iMONVFD_SetEQ(int* arEQValue);
*)

const
  DLLProjectName = 'Soundgraph iMON Display DLL';
  Version = 'v1.0';
type
  pboolean = ^boolean;

var
  FrameBuffer : array[1..2] of array[1..40] of byte;
  IMONDLL : HMODULE;
  MyX,MyY : byte;

type
  TiMONInitFunc = function : boolean; stdcall;
  TiMONUninitProc = procedure; stdcall;
  TiMONIsInitedFunc = function : boolean; stdcall;
  TiMONSetTextFunc = function(szFirstLine,szSecondLine : pchar) : boolean; stdcall;
  TiMONSetEQFunc = function(arEQValue : pinteger) : boolean; stdcall;

var
  iMONInitFunc : TiMONInitFunc = nil;
  iMONUninitProc : TiMONUninitProc = nil;
  iMONIsInitedFunc : TiMONIsInitedFunc = nil;
  iMONSetTextFunc : TiMONSetTextFunc = nil;
  iMONSetEQFunc : TiMONSetEQFunc = nil;

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

function DISPLAYDLL_Init(SizeX,SizeY : byte; StartupParameters : pchar; OK : pboolean) : pchar; stdcall;
// return startup error
// open port
var
  Path : string;
  DisplayInitted : boolean;
begin
  OK^ := true;
  Result := PChar(DLLProjectName + ' ' + Version + #0);
  fillchar(FrameBuffer,sizeof(FrameBuffer),$00);
  MyX := 1;
  MyY := 1;
  DisplayInitted := false;
  try
    Path := trim(string(StartupParameters));
    if (length(Path) > 0) then begin
      Path := includetrailingpathdelimiter(Path);
    end;
    IMONDLL := LoadLibrary(pchar(Path+'sg_vfd.dll' + #0));
    if (IMONDLL = 0) then begin
      result := PChar('IMON.DLL Exception: <'+Path+'sg_vfd.dll> not found!' + #0);
      OK^ := false;
    end else begin
      iMONInitFunc := getprocaddress(IMONDLL,pchar('iMONVFD_Init' + #0));
      iMONUninitProc := getprocaddress(IMONDLL,pchar('iMONVFD_Uninit' + #0));
      iMONIsInitedFunc := getprocaddress(IMONDLL,pchar('iMONVFD_IsInited' + #0));
      iMONSetTextFunc := getprocaddress(IMONDLL,pchar('iMONVFD_SetText' + #0));
      iMONSetEQFunc := getprocaddress(IMONDLL,pchar('iMONVFD_SetEQ' + #0));
      if assigned(iMONInitFunc) then
        DisplayInitted := iMONInitFunc;
      if not DisplayInitted then begin
        result := PChar('IMON.DLL: Display not found' + #0);
        OK^ := false;
      end;
    end;
  except
    on E: Exception do begin
      result := PChar('IMON.DLL Exception: ' + E.Message + #0);
      OK^ := false;
    end;
  end;
end;

procedure DISPLAYDLL_Done(); stdcall;
// close port
begin
  try
    if not (IMONDLL = 0) then begin
      if assigned(iMONUninitProc) then
        iMONUninitProc;
      FreeLibrary(IMONDLL);
    end;
  except
  end;
end;

function DISPLAYDLL_CustomCharIndex(Index : byte) : byte; stdcall;
begin
  DISPLAYDLL_CustomCharIndex := Index + 7; // 8-15
end;

procedure DISPLAYDLL_Write(Str : pchar); stdcall;
// write string
var
  S : string;
  Loop : integer;
  B : byte;
begin
  S := string(Str);
  for Loop := 1 to length(S) do begin
    B := ord(S[Loop]);
    if ((B < 32) or (B > 254)) and ((B < 8) or (B > 15)) then
      S[Loop] := ' ';
  end;
  strcopy(pchar(@FrameBuffer[MyY][MyX]),pchar(S));
  try
    if assigned(iMONSetTextFunc) then
      iMONSetTextFunc(pchar(@FrameBuffer[1]),pchar(@FrameBuffer[2]));
  except
  end;
end;

procedure DISPLAYDLL_SetPosition(X, Y: byte); stdcall;
// set cursor position
begin
  MyX := max(min(X,40),1);
  MyY := max(min(Y,2),1);
end;

function DISPLAYDLL_DefaultParameters : pchar; stdcall;
begin
  DISPLAYDLL_DefaultParameters := pchar(#0);
end;

function DISPLAYDLL_Usage : pchar; stdcall;
begin
  Result := pchar('Usage: <dllpath>'+#13#10+'where dllpath is the location of sg_vfd.dll' + #0);
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
  DISPLAYDLL_CustomCharIndex,
  DISPLAYDLL_Usage,
  DISPLAYDLL_DriverName,
  DISPLAYDLL_Done,
  DISPLAYDLL_Init;
begin
end.

