library FutabaVFD_LCD;
// Futaba VFD - LCD emulator driver.
// Based on HD44780 Parallel Display DLL

{$R *.res}

uses
  SysUtils,
  Windows;

const
  DLLProjectName = 'Futaba VFD - LCD emulators DLL';
  Version = 'v1.0';
type
  pboolean = ^boolean;
  TCustomArray = array[0..7] of byte;

const
  // control pins
  RS = 4; // pin 16
  RW = 2; // pin 14
  E1 = 1; // pin 1,  Enable device 1
  E2 = 8; // pin 17, Enable device 2, or backlight on 1 controller devices
  CtrlMask = 11; // The parallel hardware inverts some of the outputs.
  // delays required by device
  uiDelayShort = 40;
  uiDelayMed = 100;
  uiDelayLong = 1600;
  uiDelayInit = 4100;
  uiDelayBus = 17;
  // control functions
  ClearScreen = 1;
  SetPos = 128;
  SetCGRamAddr = 64;
  OnOffCtrl = 8;
  OODisplayOn = 4;
  OODisplayOff = 0;
  OOCursorOn = 2;
  OOCursorOff = 0;
  OOCursorBlink = 1;
  OOCursorNoBlink = 0;

type
  TDlPortWritePortUchar = procedure (Port: Integer; Data: Byte); stdcall;//  external 'IOPlugin.dll';
  TDlPortReadPortUchar = function (Port: Integer): Byte; stdcall;//  external 'IOPlugin.dll';

  TControllers = (All, C1, C2);

  TLCD_HD = class
      procedure customChar(character: Integer; data: Array of Byte);
      procedure setPosition(x, y: Integer);
      procedure write(str: String);
      procedure setbacklight(state: Boolean);
      procedure setBrightness(level: Integer);
      constructor CreateParallel(const poortadres: Word; const width, heigth: Byte;
                                 const TimingMult : integer; const A1x16,AKS0073 : boolean);
      destructor Destroy; override;
    private
      bHDKS0073Addressing : boolean;
      bHDAltAddressing : boolean;
      iHDTimingMultiplier : integer;

      IOPlugin: HMODULE;
      FBaseAddr: Word;
      FCtrlAddr: Word;
      cursorx, cursory: Word;
      backlight, width, height: Byte;
      bTwoControllers: Boolean;
      bHighResTimers: Boolean;
      iHighResTimerFreq: Int64;
      DlPortWritePortUchar: TDlPortWritePortUchar;
      DlPortReadPortUchar: TDlPortReadPortUchar;
      bHasIOPlugin: Boolean;
      bHasIO: Boolean;
      procedure writectrl(const controllers: TControllers; const x: Byte);
      procedure writedata(const controllers: TControllers; const x: Byte);
      procedure writestring(const controllers: TControllers; s: String);
      procedure clear;
      procedure UsecDelay(uiUsecs: Cardinal);
      procedure init;
      procedure CtrlOut(const AValue: Byte);
      procedure DataOut(const AValue: Byte);
      procedure initClass;
      procedure LoadIO;
      procedure UnloadIO;
  end;

var
  LCD_HD : TLCD_HD = nil;

constructor TLCD_HD.CreateParallel(const poortadres: Word; const width, heigth: Byte;
                                   const TimingMult : integer; const A1x16,AKS0073 : boolean);
{ var
  x, y: integer; }
begin
  bHDKS0073Addressing := AKS0073;
  bHDAltAddressing := A1x16;
  iHDTimingMultiplier := TimingMult;

  bHasIO := False;
  bHighResTimers := QueryPerformanceFrequency(iHighResTimerFreq);

  FBaseAddr := poortadres;
  FCtrlAddr := FBaseAddr + 2;
  self.width := width;
  self.height := heigth;

  LoadIO();
  bHasIO := True;

  initClass;

   {      // DEBUG CODE for checking addressing.
    for y:=1 to height do
      for x:=1 to width do
         SetPosition(x, y);
          }
end;

destructor TLCD_HD.Destroy;
begin
  clear;

  setbacklight(false);

  writectrl(All, OnOffCtrl or OODisplayOff);
  UsecDelay(uiDelayShort);

  UnloadIO();


  inherited;
end;

procedure TLCD_HD.LoadIO;
var
  VI: TOSVERSIONINFO; // For checking Windows version
begin
  { Try inpout32.dll first, since it supports x64 }
  IOPlugin := LoadLibrary(PChar('inpout32.dll' + #0));
  if (IOPlugin <> 0) then
  begin
    DlPortWritePortUchar := TDlPortWritePortUchar(GetProcAddress(IOPlugin,'Out32'));
    DlPortReadPortUchar := TDlPortReadPortUchar(GetProcAddress(IOPlugin,'Inp32'));
    if (not Assigned(DlPortWritePortUchar)) or (not Assigned(DlPortReadPortUchar)) then
      raise Exception.Create('Loaded inpout32, but unable to obtain API.')
    else
    begin
      bHasIOPlugin := True;
      Exit; { Got our plugin, we're done }
    end;
  end;

  { Try good old dlportio.dll next, for backwards compatiability }
  IOPlugin := LoadLibrary(PChar('dlportio.dll' + #0));
  if (IOPlugin <> 0) then
  begin
    DlPortWritePortUchar := TDlPortWritePortUchar(GetProcAddress(IOPlugin,
      'DlPortWritePortUchar'));
    DlPortReadPortUchar := TDlPortReadPortUchar(GetProcAddress(IOPlugin,
      'DlPortReadPortUchar'));
    if (not Assigned(DlPortWritePortUchar)) or (not Assigned(DlPortReadPortUchar)) then
      raise Exception.Create('Loaded dlportio, but unable to obtain API.')
    else
    begin
      bHasIOPlugin := True;
      Exit;
    end;
  end;

  { Last resort: If Win9x, fallback to low-level I/O.  This option isn't available
    for WinNT/2000/XP due to access priviledges. }
  bHasIOPlugin := False;
  VI.dwOSVersionInfoSize := SizeOf(VI);
  if not GetVersionEx(VI)
  then
  begin
    bHasIO := False;
    raise Exception.Create('Unable to determine Windows version');
  end;
  if VI.dwPlatformId = VER_PLATFORM_WIN32_NT
  then
  begin
    bHasIO := False;
    raise Exception.Create('Either dlportio.dll or inpout32.dll are required for Windows NT/2000/XP');
  end
  else
    { Safe to use low-level I/O under Win9x }
    bHasIO := True;

end;

procedure TLCD_HD.UnloadIO;
begin
  if (bHasIOPlugin = True) then FreeLibrary(IOPlugin);
  IOPlugin := 0;
  bHasIOPlugin := False;
  bHasIO := False;
  DlPortWritePortUchar := nil;
  DlPortReadPortUchar := nil;
end;

procedure TLCD_HD.initClass;
begin
  backlight := 8;
  cursorx := 1;
  cursory := 1;

  //defining wether lcd is 1 or 2 controller based
  if (width * height > 80) then begin
    bTwoControllers := True;

    // the line usually used for the backlight is used to enable the second
    // controller.
    backlight := 0;
  end
  else bTwoControllers := False;

  init;
end;

procedure TLCD_HD.init;
const
  FuncSet = 32;
  FSInterface8Bit = 16;
  FSInterface4Bit = 0;
  FSTwoLine = 8;
  FSOneLine = 0;
  FSSmallFont = 0;
  FSBigFont = 4;
  EntryMode = 4;
  EMIncrement = 2;
  EMDecrement = 0;
  EMShift = 1;
  EMNoShift = 0;
  HomeCursor = 2;
  { Extra functions of KS0073 }
  ExtFuncSet = 8;
  FSExtReg = 4;
  EFSFourLine = 1;
  EFSOneTwoLine = 0;
  EFSFontWidth6 = 4;
  EFSFontWidth5 = 0;

begin
  // perform initalising by instruction, just in case std power reset failed.

  writectrl(All, FuncSet or FSInterface8Bit or FSTwoLine or FSSmallFont);
  UsecDelay(uiDelayInit);

  writectrl(All, FuncSet or FSInterface8Bit or FSTwoLine or FSSmallFont);
  UsecDelay(uiDelayMed);

  writectrl(All, FuncSet or FSInterface8Bit or FSTwoLine or FSSmallFont);
  UsecDelay(uiDelayShort);

  writectrl(All, FuncSet or FSInterface8Bit or FSTwoLine or FSSmallFont);
  UsecDelay(uiDelayShort);

  if (bHDKS0073Addressing = True) then
  begin
    { Need to set extended functions }
    writectrl(All, FuncSet or FSInterface8Bit or FSTwoLine or FSSmallFont
      or FSExtReg);
    UsecDelay(uiDelayShort);
    if height = 4
    then
      writectrl(All, ExtFuncSet or EFSFourLine)
    else
      writectrl(All, ExtFuncSet);
    UsecDelay(uiDelayShort);
    writectrl(All, FuncSet or FSInterface8Bit or FSTwoLine or FSSmallFont);
  end;

  writectrl(All, OnOffCtrl or OODisplayOff);
  UsecDelay(uiDelayShort);

  writectrl(All, ClearScreen);
  UsecDelay(uiDelayLong);

  writectrl(All, EntryMode or EMIncrement or EMNoShift);
  UsecDelay(uiDelayMed);


  // initialization finished.

  writectrl(All, OnOffCtrl or  OODisplayOn or OOCursorOff or OOCursorNoBlink);
  UsecDelay(uiDelayLong);

  writectrl(All, HomeCursor);
  UsecDelay(uiDelayLong);

  {
   // was:
  writectrl(C1,56 xor CtrlMask);// 111000 = FuncSet or FSInterface8Bit or FSTwoLine or FSSmallFont
  writectrl(C1,56 xor CtrlMask);// 111000 = FuncSet or FSInterface8Bit or FSTwoLine or FSSmallFont
  writectrl(C1,56 xor CtrlMask);// 111000 = FuncSet or FSInterface8Bit or FSTwoLine or FSSmallFont
  writectrl(C1,6 xor CtrlMask); // 000110 = EntryMode or EMIncrement or EMNoShift
  writectrl(C1,12 xor CtrlMask);// 001100 = OnOffCtrl or OODisplayOn or OOCursorOff or OOCursorNoBlick
 }
end;

procedure TLCD_HD.setbacklight(state: Boolean);
begin
  if state = True and not bTwoControllers then
    backlight := 8
  else
    backlight := 0;

  if state = True then
    writectrl(All, OnOffCtrl or OODisplayOn)
  else
    writectrl(All, OnOffCtrl or OODisplayOff);

  UsecDelay(uiDelayLong);
end;

procedure TLCD_HD.setBrightness(level: Integer);
const
  FuncSet = 32;
  FSInterface8Bit = 16;
  FSTwoLine = 8;
  FSSmallFont = 0;
  Bright100 = 0;
  Bright075 = 1;
  Bright050 = 2;
  Bright025 = 3;
var
  l : Byte;
begin
  if (level >= 0) and (level <= 63) then l := Bright025
  else if (level >= 64) and (level <= 127) then l := Bright050
  else if (level >= 128) and (level <= 191) then l := Bright075
  else l := Bright100;

  writectrl(All, FuncSet or FSInterface8Bit or FSTwoLine or FSSmallFont or l);
  UsecDelay(uiDelayLong);
end;

procedure TLCD_HD.write(str: String);
var
 i: Cardinal;
begin
  for i:= 1 to Length(str) do
  begin
    case Ord(str[i]) of
      Ord('°'): str[i]:=Chr(0);
      Ord('ž'): str[i]:=Chr(1);
      131: str[i]:=Chr(2);
      132: str[i]:=Chr(3);
      133: str[i]:=Chr(4);
      134: str[i]:=Chr(5);
      135: str[i]:=Chr(6);
      136: str[i]:=Chr(7);
    end;
  end;

  if (not bTwoControllers) or (cursory < ((height div 2)+1)) then
    writestring(C1, str)
  else
    writestring(C2, str);
end;


procedure TLCD_HD.customChar(character: Integer; data: Array of Byte);
var
  i: Byte;
begin
    writectrl(All, SetCGRamAddr + ((character-1) * 8));
    for i := 0 to 7 do
      writedata(All, data[i]);

    {
    // for 2nd controller
    writectrl2(SetCGRamAddr + ((character-1) * 8));
    for i := 0 to 7 do writedata2(data[i]);
    }

    { why twice?
    writectrl2(64 + ((adr-1) * 8));
    for i := 1 to 8 do
      writedata2(data[i]);
    }

end;

procedure TLCD_HD.clear;
begin
  writectrl(All, ClearScreen);
  UsecDelay(uiDelayLong);
end;

procedure TLCD_HD.UsecDelay(uiUsecs: Cardinal);
var
  uiElapsed: int64;
  uiUsecsScaled: int64;
  iBegin, iCurr: int64;
begin
  {$R-}
  uiUsecsScaled := int64(uiUsecs) * int64(iHDTimingMultiplier);

  if (uiUsecs <= 0) then Exit;

  if (bHighResTimers) then
  begin
    QueryPerformanceCounter(iBegin);

	  repeat
      QueryPerformanceCounter(iCurr);

      if (iCurr < iBegin) then iBegin := 0;
		  uiElapsed := ((iCurr - iBegin) * 1000000) div iHighResTimerFreq;

	  until (uiElapsed > uiUsecsScaled);
  end
  else
  begin
    raise exception.create('PerformanceCounter not supported on this system');
  end;

  {$R+}
end;

procedure TLCD_HD.writectrl(const controllers: TControllers; const x: Byte);
var
  enableLines, portControl: Byte;
begin

  if bTwoControllers then
  begin
    case controllers of
      All: enableLines := E1 or E2;
      C1: enableLines := E1;
      C2: enableLines := E2;
      else enableLines := E1;
    end;
    portControl := 0;
  end
  else
  begin
    assert(controllers <> C2);
    enableLines := E1;
    portControl := backlight;
  end;

  CtrlOut(portControl);
  DataOut(x);
  UsecDelay(uiDelayBus);
  CtrlOut(enableLines or portControl);
  UsecDelay(uiDelayBus);
  CtrlOut(portControl);
  UsecDelay(uiDelayShort);

  // Some displays may to need this
  //CtrlOut(RW or portControl);
  // UsecDelay(200);
  {
  CtrlOut(3 or backlight);  // 3/11 RS=0, R/W=0, E=0,         0000 0011
  DataOut(x);
  CtrlOut(2 or backlight);  // 2/10 RS=0, R/W=0, E1=1, E2=0,  0000 0010
  CtrlOut(3 or backlight);  // 3/11 RS=0, R/W=0, E=0          0000 0011
  CtrlOut(1 or backlight);  // 1/9 RS=0, R/W=1, E=0           0000 0001
  Sleep(3); //max execution time = 1,64 ms, so this should be safe
  }
end;

procedure TLCD_HD.writedata(const controllers: TControllers; const x: Byte);
var
  enableLines, portControl: Byte;
begin

  if bTwoControllers then
  begin
    case controllers of
      All: enableLines := E1 or E2;
      C1: enableLines := E1;
      C2: enableLines := E2;
      else enableLines := E1;
    end;
    portControl := RS;
  end
  else
  begin
    assert(controllers <> C2);
    enableLines := E1;
    portControl := RS or backlight;
  end;

  CtrlOut(portControl);
  DataOut(x);
  UsecDelay(uiDelayBus);
  CtrlOut(enableLines or portControl);
  UsecDelay(uiDelayBus);
  CtrlOut(portControl);
  UsecDelay(uiDelayShort);

  // Some displays may to need this
  //if (bTwoControllers) then
  //  CtrlOut(RW)
  //else
  //  CtrlOut(RW or backlight);
  //    UsecDelay(200);
{
  CtrlOut(7 or backlight);     //7 + 8   B111 === B100  RS | backlight
  DataOut(x);
  CtrlOut(6 or backlight);     //6  RS=1, R/W=0, E=1  B110 == 101      RS | E1
  CtrlOut(7 or backlight);     //7  B111 = B100 = RS
  CtrlOut(5 or backlight);     //5  101 = 110 RS|RW  ?????

  //Sleep(1);  //instead of the line below because of faster processors

  for i := 0 to 65535 do begin  +/- 40 us  end;
  for i := 0 to 65535 do begin  +/- 40 us  end;
  }
end;

procedure TLCD_HD.writestring(const controllers: TControllers; s: String);
var
  i: Byte;
begin
  i := 1;
  while (i <= length(s)) and (cursorx <= width) do begin

    // special case for 1 chip 1x16 displays
    if (bHDAltAddressing) and (width = 16) and (height = 1)
      and (cursorx = 9) then
      setPosition(cursorx, cursory);

    writedata(controllers, ord(s[i]));
    inc(i);
    inc(cursorx);
  end;
  //for i := 1 to length(s) do writedata(ord(s[i]));
end;


procedure TLCD_HD.setPosition(x, y: Integer);
var
  tempX, tempY: Byte;
  DDaddr: Byte;
  controller: TControllers;
begin
  // store theses values as they are used when a write occurs.
  cursory := y;
  cursorx := x;

  tempX := x - 1;
  tempY := y - 1;

  if (bTwoControllers) and (tempY >= (height div 2)) then
  begin
    tempY := tempY mod (height div 2);
    controller := C2;
  end
  else
    controller := C1;
  if (bHDKS0073Addressing = False) then
  begin
    // Find DDRAM address for HDD44780

    // special case for 1 chip 1x16 displays, acts like 2x8 display
    if (bHDAltAddressing) and (width = 16) and (height = 1)
      and (tempX >= 8) then
    begin
      tempX := tempX - 8;
      tempY := tempY + 1;
    end;

    DDaddr := tempX + (tempY mod 2) * 64;

    // line 3 logically follows line 1, (same for 4 and 2)
    if ((tempY mod 4) >= 2) then
      DDaddr := DDaddr + width;
  end
  else
  begin
    // Find DDRAM address for KS0073
    { Addressing:
      Line 1 - 0x00
      Line 2 - 0x20
      Line 3 - 0x40
      Line 4 - 0x60 }
    DDaddr := tempX + 32 * tempY;
  end;

  writectrl(controller, SetPos or DDaddr);
end;

procedure PortOut(IOport:word; Value:byte); assembler;
asm
  xchg ax,dx
  out dx,al
end;

procedure TLCD_HD.CtrlOut(const AValue: Byte);
begin
  if (not bHasIO) then exit;
  if (bHasIOPlugin) then
  begin
      if (@DlPortWritePortUchar <> nil) then
        DlPortWritePortUchar(FCtrlAddr, AValue xor CtrlMask);
  end
  else
    PortOut(FCtrlAddr, AValue xor CtrlMask);
end;

procedure TLCD_HD.DataOut(const AValue: Byte);
begin
  if (not bHasIO) then exit;
  if (bHasIOPlugin) then
  begin
    if (@DlPortWritePortUchar <> nil) then
      DlPortWritePortUchar(FBaseAddr, AValue);
  end
  else
    PortOut(FBaseAddr, AValue);
end;

{
function TLCD_HD.lcdpresent: Boolean;
begin
  Result := DlPortReadPortUchar(FCtrlAddr) < 255;
  init;
end;  }

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

function StrToBool(T : string) : boolean;
begin
  T := Uppercase(Trim(T));
  StrToBool := (T = '1') or
               (T = 'YES') or
               (T = 'Y') or
               (T = 'T') or
               (T = 'ON') or
               (T = 'TRUE') or
               (T = 'JA') or
               (T = 'OK') or
               (T = 'OUI') or
               (T = 'JAWOL') or
               (T = 'SI') or
               (T = 'ENABLE') or
               (T = 'DA') or
               (T = 'OBVIOUSLY') or
               (T = 'OFCOURSE') or
               (T = 'PLEASE');
end;

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
    PortAddr := $378;
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
    LCD_HD := TLCD_HD.CreateParallel(PortAddr,SizeX,SizeY,Mult,b1x16,bKS0073);
  except
    on E: Exception do begin
      result := PChar('HD44780.DLL Exception: ' + E.Message + #0);
      OK^ := false;
    end;
  end;
end;

procedure DISPLAYDLL_Done(); stdcall;
// close port
begin
  try
    if assigned(LCD_HD) then begin
      LCD_HD.Free;
      LCD_HD := nil;
    end;
  except
  end;
end;


procedure DISPLAYDLL_Write(Str : pchar); stdcall;
// write string
var
  S : string;
begin
  try
    if assigned(LCD_HD) then begin
      S := string(Str);
      LCD_HD.Write(S);
    end;
  except
  end;
end;

procedure DISPLAYDLL_CustomChar(Chr : byte; Data : TCustomArray); stdcall;
// define custom character
begin
  try
    if assigned(LCD_HD) then
       LCD_HD.CustomChar(Chr,Data);
  except
  end;
end;

procedure DISPLAYDLL_SetBacklight(LightOn : boolean); stdcall;
// turn on backlighting
begin
  try
    if assigned(LCD_HD) then
       LCD_HD.SetBacklight(LightOn);
  except
  end;
end;

procedure DISPLAYDLL_SetBrightness(Level : Byte); stdcall;
// set brightness level
begin
  try
    if assigned(LCD_HD) then
       LCD_HD.SetBrightness(Level);
  except
  end;
end;

procedure DISPLAYDLL_SetPosition(X, Y: byte); stdcall;
// set cursor position
begin
  try
    if assigned(LCD_HD) then
       LCD_HD.SetPosition(X,Y);
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
  DISPLAYDLL_SetBrightness,
  DISPLAYDLL_CustomChar,
  DISPLAYDLL_Write,
  DISPLAYDLL_SetPosition,
  DISPLAYDLL_DefaultParameters,
  DISPLAYDLL_Usage,
  DISPLAYDLL_DriverName,
  DISPLAYDLL_Done,
  DISPLAYDLL_Init;
begin
end.


