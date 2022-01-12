library HD44780P;

{$R *.res}

uses
  SysUtils,
  Windows,
  HD44780OBJ;

(*

 revhist

1.0 initial driver
1.1 combined with HD44780 serial display

*)

const
  DLLProjectName = 'HD44780 Parallel Display DLL';
  Version = 'v1.1';
type
  pboolean = ^boolean;
  TCustomArray = array[0..7] of byte;

const
  // control pins
  RS = 4; // pin 16
  RW = 2; // pin 14
  E1 = 1; // pin 1,  Enable device 1
  E2 = 8; // pin 17, Enable device 2, or Backlight on 1 controller devices
  ControlMask = 11; // The parallel hardware inverts some of the outputs.
  uiDelayBus      = 17;

type
  TDlPortWritePortUchar = procedure (Port: Integer; Data: byte); stdcall;//  external 'IOPlugin.dll';
  TDlPortReadPortUchar = function (Port: Integer): byte; stdcall;//  external 'IOPlugin.dll';

  TLCD_HD_LPT = class(TLCD_HD44780)
  private
    Backlight : byte;
    IOPlugin: HMODULE;
    FBaseAddr: Word;
    FCtrlAddr: Word;
    DlPortWritePortUchar: TDlPortWritePortUchar;
    DlPortReadPortUchar: TDlPortReadPortUchar;
    bHasIOPlugin: Boolean;
    bHasIO: Boolean;
    procedure ControlOut(const AValue: byte);
    procedure DataOut(const AValue: byte);
  public
    procedure SetBacklight(State : boolean); override;
    procedure WriteControl(const Controllers : TControllers; const AValue: byte); override;
    procedure WriteData(const Controllers : TControllers; const AValue: byte); override;
    procedure LoadIO; override;
    procedure UnloadIO; override;
    constructor Create(const PortAddress: Word; const Width, Height: byte;
                       const TimingMult : integer; const A1x16,AKS0073 : boolean);
    destructor Destroy; override;
  end;

var
  LCD_HD_LPT : TLCD_HD_LPT = nil;

//************************************
//
//           LPT Code
//
//************************************

procedure TLCD_HD_LPT.LoadIO;
var
  VI: TOSVERSIONINFO; // For checking Windows version
begin
  { Try inpout32.dll first, since it supports x64 }
  IOPlugin := LoadLibrary(PChar('inpout32.dll' + #0));
  if (IOPlugin <> 0) then begin
    DlPortWritePortUchar := TDlPortWritePortUchar(GetProcAddress(IOPlugin,'Out32'));
    DlPortReadPortUchar := TDlPortReadPortUchar(GetProcAddress(IOPlugin,'Inp32'));
    if (not Assigned(DlPortWritePortUchar)) or (not Assigned(DlPortReadPortUchar)) then
      raise Exception.Create('Loaded inpout32, but unable to obtain API.')
    else begin
      bHasIOPlugin := true;
      exit; { Got our plugin, we're done }
    end;
  end;

  { Try good old dlportio.dll next, for backwards compatiability }
  IOPlugin := LoadLibrary(PChar('dlportio.dll' + #0));
  if (IOPlugin <> 0) then begin
    DlPortWritePortUchar := TDlPortWritePortUchar(GetProcAddress(IOPlugin,
      'DlPortWritePortUchar'));
    DlPortReadPortUchar := TDlPortReadPortUchar(GetProcAddress(IOPlugin,
      'DlPortReadPortUchar'));
    if (not Assigned(DlPortWritePortUchar)) or (not Assigned(DlPortReadPortUchar)) then
      raise Exception.Create('Loaded dlportio, but unable to obtain API.')
    else begin
      bHasIOPlugin := true;
      exit; { Got our plugin, we're done }
    end;
  end;

  { Last resort: If Win9x, fallback to low-level I/O.  This option isn't available
    for WinNT/2000/XP due to access priviledges. }
  bHasIOPlugin := false;
  VI.dwOSVersionInfoSize := SizeOf(VI);
  if not GetVersionEx(VI) then begin
    bHasIO := false;
    raise Exception.Create('Unable to determine Windows version');
  end;
  if (VI.dwPlatformId = VER_PLATFORM_WIN32_NT) then begin
    bHasIO := false;
    raise Exception.Create('Either dlportio.dll or inpout32.dll are required for Windows NT/2000/XP');
  end else
  { Safe to use low-level I/O under Win9x }
    bHasIO := true;
end;

procedure TLCD_HD_LPT.UnloadIO;
begin
  if (bHasIOPlugin = true) then FreeLibrary(IOPlugin);
  IOPlugin := 0;
  bHasIOPlugin := false;
  bHasIO := false;
  DlPortWritePortUchar := nil;
  DlPortReadPortUchar := nil;
end;

procedure PortOut(IOport:word; Value:byte); assembler;
asm
  xchg ax,dx
  out dx,al
end;

procedure TLCD_HD_LPT.ControlOut(const AValue: byte);
begin
  if (not bHasIO) then exit;
  if (bHasIOPlugin) then begin
    if (@DlPortWritePortUchar <> nil) then
      DlPortWritePortUchar(FCtrlAddr, AValue xor ControlMask);
  end else
    PortOut(FCtrlAddr, AValue xor ControlMask);
end;

procedure TLCD_HD_LPT.DataOut(const AValue: byte);
begin
  if (not bHasIO) then exit;
  if (bHasIOPlugin) then begin
    if (@DlPortWritePortUchar <> nil) then
      DlPortWritePortUchar(FBaseAddr, AValue);
  end else
    PortOut(FBaseAddr, AValue);
end;

//************************************
//
//           HD44780 code
//
//************************************

constructor TLCD_HD_LPT.Create(const PortAddress: Word; const Width, Height: byte;
                               const TimingMult : integer; const A1x16,AKS0073 : boolean);
begin
  Backlight := 8;
  bHasIO := false;
  FBaseAddr := PortAddress;
  FCtrlAddr := FBaseAddr + 2;
  inherited Create(Width,Height,TimingMult,A1x16,AKS0073);
// bHasIO := true;
  // the line usually used for the Backlight is used to enable the second controller.
  if TwoControllers then
    Backlight := 0;
end;

destructor TLCD_HD_LPT.Destroy;
begin
  inherited;
end;

procedure TLCD_HD_LPT.SetBacklight(State : boolean);
begin
  if State and not TwoControllers then
    Backlight := 8
  else
    Backlight := 0;

  if (not TwoControllers) then
    ControlOut(Backlight);; //update 'lightline'
end;

procedure TLCD_HD_LPT.WriteControl(const Controllers : TControllers; const AValue : byte);
var
  EnableLines, PortControl: byte;
begin
  if TwoControllers then begin
    case Controllers of
      All: EnableLines := E1 or E2;
      C1: EnableLines := E1;
      C2: EnableLines := E2;
      else EnableLines := E1;
    end;
    PortControl := 0;
  end else begin
    assert(Controllers <> C2);
    EnableLines := E1;
    PortControl := Backlight;
  end;

  ControlOut(PortControl);
  DataOut(AValue);
  UsecDelay(uiDelayBus);
  ControlOut(EnableLines or PortControl);
  UsecDelay(uiDelayBus);
  ControlOut(PortControl);
  UsecDelay(uiDelayShort);

  // Some displays may to need this
  //ControlOut(RW or PortControl);
  // UsecDelay(200);
  {
  ControlOut(3 or Backlight);  // 3/11 RS=0, R/W=0, E=0,         0000 0011
  DataOut(x);
  ControlOut(2 or Backlight);  // 2/10 RS=0, R/W=0, E1=1, E2=0,  0000 0010
  ControlOut(3 or Backlight);  // 3/11 RS=0, R/W=0, E=0          0000 0011
  ControlOut(1 or Backlight);  // 1/9 RS=0, R/W=1, E=0           0000 0001
  Sleep(3); //max execution time = 1,64 ms, so this should be safe
  }
end;

procedure TLCD_HD_LPT.WriteData(const Controllers : TControllers; const AValue : byte);
var
  EnableLines, PortControl: byte;
begin

  if TwoControllers then begin
    case Controllers of
      All: EnableLines := E1 or E2;
      C1: EnableLines := E1;
      C2: EnableLines := E2;
      else EnableLines := E1;
    end;
    PortControl := RS;
  end else begin
    assert(Controllers <> C2);
    EnableLines := E1;
    PortControl := RS or Backlight;
  end;

  ControlOut(PortControl);
  DataOut(AValue);
  UsecDelay(uiDelayBus);
  ControlOut(EnableLines or PortControl);
  UsecDelay(uiDelayBus);
  ControlOut(PortControl);
  UsecDelay(uiDelayShort);

  // Some displays may to need this
  //if (bTwoControllers) then
  //  ControlOut(RW)
  //else
  //  ControlOut(RW or Backlight);
  //    UsecDelay(200);
{
  ControlOut(7 or Backlight);     //7 + 8   B111 === B100  RS | Backlight
  DataOut(x);
  ControlOut(6 or Backlight);     //6  RS=1, R/W=0, E=1  B110 == 101      RS | E1
  ControlOut(7 or Backlight);     //7  B111 = B100 = RS
  ControlOut(5 or Backlight);     //5  101 = 110 RS|RW  ?????

  //Sleep(1);  //instead of the line below because of faster processors

  for i := 0 to 65535 do begin  +/- 40 us  end;
  for i := 0 to 65535 do begin  +/- 40 us  end;
  }
end;

//************************************
//
//           DLL interface code
//
//************************************


/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

function DISPLAYDLL_Init(SizeX,SizeY : byte; StartupParameters : pchar; OK : pboolean) : pchar; stdcall;
// return startup error
// open port
var
  Addr,S,Params : string;
  PortAddr : word;
  Mult : integer;
  b1x16,bKS0073 : boolean;
begin
  OK^ := true;
  Result := PChar(DLLProjectName + ' ' + Version + #0);
  try
    Params := string(StartupParameters);
    Addr := uppercase(SubString(Params));
    Mult := 1;
    b1x16 := false;
    bKS0073 := false;
    try
      if (Addr = 'LPT1') then PortAddr := $378
      else if (Addr = 'LPT2') then PortAddr := $3BC
      else if (Addr = 'LPT3') then PortAddr := $278
      else PortAddr := StrToInt(Addr);
      S := SubString(Params);
      if (S <> '') then
        Mult := StrToInt(S);
      S := SubString(Params);
      if (S <> '') then
        b1x16 := StrToBool(S);
      S := SubString(Params);
      if (S <> '') then
        bKS0073 := StrToBool(S);
    except
      PortAddr := $378;
      Mult := 1;
      b1x16 := false;
      bKS0073 := false;
    end;
    LCD_HD_LPT := TLCD_HD_LPT.Create(PortAddr,SizeX,SizeY,Mult,b1x16,bKS0073);
  except
    on E: Exception do begin
      result := PChar('HD44780P.DLL Exception: ' + E.Message + #0);
      OK^ := false;
    end;
  end;
end;

procedure DISPLAYDLL_Done(); stdcall;
// close port
begin
  try
    if assigned(LCD_HD_LPT) then begin
      LCD_HD_LPT.Free;
      LCD_HD_LPT := nil;
    end;
  except
  end;
end;

procedure DISPLAYDLL_Write(Str : pchar); stdcall;
// Write string
var
  S : string;
begin
  try
    if assigned(LCD_HD_LPT) then begin
      S := string(Str);
      LCD_HD_LPT.Write(S);
    end;
  except
  end;
end;

function DISPLAYDLL_CustomCharIndex(Index : byte) : byte; stdcall;
begin
  DISPLAYDLL_CustomCharIndex := Index-1; {0-7}
end;

procedure DISPLAYDLL_CustomChar(Chr : byte; Data : TCustomArray); stdcall;
// define custom character
begin
  try
    if assigned(LCD_HD_LPT) then
       LCD_HD_LPT.CustomChar(Chr,Data);
  except
  end;
end;

procedure DISPLAYDLL_SetBacklight(LightOn : boolean); stdcall;
// turn on Backlighting
begin
  try
    if assigned(LCD_HD_LPT) then
       LCD_HD_LPT.SetBacklight(LightOn);
  except
  end;
end;

procedure DISPLAYDLL_SetPosition(X, Y: byte); stdcall;
// set cursor position
begin
  try
    if assigned(LCD_HD_LPT) then
       LCD_HD_LPT.SetPosition(X,Y);
  except
  end;
end;

function DISPLAYDLL_DefaultParameters : pchar; stdcall;
begin
  DISPLAYDLL_DefaultParameters := pchar('LPT1' + #0);
end;

function DISPLAYDLL_Usage : pchar; stdcall;
begin
  Result := pchar('Usage: PORT[,m,a1,a2]'+#13#10+
                  'PORT = LPT1 or $378    m = timing multiplier'+#13#10+
                  'a1 = 1x16 addressing   a2 = KS0073 addressing' + #0);
end;

function DISPLAYDLL_DriverName : pchar; stdcall;
begin
  Result := PChar(DLLProjectName + ' ' + Version + #0);
end;

// don't forget to export the funtions, else nothing works :)
exports
  DISPLAYDLL_SetBacklight,
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


