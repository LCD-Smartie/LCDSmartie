(***************************************************************

Instruction             Code                               time**  Description
                        RS R/W D7 D6 D5 D4 D3  D2  D1  D0
Clear display            0  0  0  0  0  0  0   0   0   1   1.64mS  Clears display and returns cursor to the home position (address 0).
Cursor home              0  0  0  0  0  0  0   0   1   *   1.64mS  Returns cursor to home position (address 0). Also returns display being shifted to the original position. DDRAM contents remains unchanged.
Entry mode set           0  0  0  0  0  0  0   1   I/D S   40uS    Sets cursor move direction (I/D), specifies to shift the display (S). These operations are performed during data read/write.
Display On/Off control   0  0  0  0  0  0  1   D   C   B   40uS    Sets On/Off of all display (D), cursor On/Off (C) and blink of cursor position character (B).
Cursor/display shift     0  0  0  0  0  1  S/C R/L *   *   40uS    Sets cursor-move or display-shift (S/C), shift direction (R/L). DDRAM contents remains unchanged.
Function set             0  0  0  0  1  DL N   F   *   *   40uS    Sets interface data length (DL), number of display line (N) and character font(F).
Set CGRAM address        0  0  0  1  [  CGRAM address  ]   40uS    Sets the CGRAM address. CGRAM data is sent and received after this setting.
Set DDRAM address        0  0  1  [     DDRAM address  ]   40uS    Sets the DDRAM address. DDRAM data is sent and received after this setting.
Read busy-flag and
address counter          0  1  BF [CGRAM/DDRAM address ]   0uS     Reads Busy-flag (BF) indicating internal operation is being performed and reads CGRAM or DDRAM address counter contents (depending on previous instruction).
Write to CGRAM or DDRAM  1  0  [ write data            ]   40uS    Writes data to CGRAM or DDRAM.
Read from CGRAM or DDRAM 1  1  [ read data             ]   40uS    Reads data from CGRAM or DDRAM.

Remarks:
- DDRAM = Display Data RAM.
- CGRAM = Character Generator RAM.
- DDRAM address corresponds to cursor position.

- *  = Don't care.
- ** = Based on Fosc = 250kHz.

Bit     Setting / Status
I/D     0 = Decrement cursor position   1 = Increment cursor position
S       0 = No display shift            1 = Display shift
D       0 = Display off                 1 = Display on
C       0 = Cursor off                  1 = Cursor on
B       0 = Cursor blink off            1 = Cursor blink on
S/C     0 = Move cursor                 1 = Shift display
R/L     0 = Shift left                  1 = Shift right
DL      0 = 4-bit interface             1 = 8-bit interface
N       0 = 1/8 or 1/11 Duty (1 line)   1 = 1/16 Duty (2 lines)
F       0 = 5x7 dots                    1 = 5x10 dots
BF      0 = Can accept instruction      1 = Internal operation in progress

****************************************************************)
unit HD44780OBJ;

interface

uses
  Windows,Classes;

const
// control functions
  ClearScreen     = $01;

  HomeCursor      = $02;

  EntryMode       = $04;
  EMIncrement     = $02;
  EMDecrement     = $00;
  EMShift         = $01;
  EMNoShift       = $00;

  OnOffCtrl       = $08;
  OODisplayOn     = $04;
  OODisplayOff    = $00;
  OOCursorOn      = $02;
  OOCursorOff     = $00;
  OOCursorBlink   = $01;
  OOCursorNoBlink = $00;
  { Extra functions of KS0073 }
  ExtFuncSet      = $08;
  EFSFontWidth6   = $04;
  EFSFontWidth5   = $00;
  FSExtReg        = $04;
  EFSFourLine     = $01;
  EFSOneTwoLine   = $00;

  CursorDispShift = $10;
  CDShiftDisplay  = $08;
  CDMoveCursor    = $00;
  CDShiftRight    = $04;
  CDShiftLeft     = $00;

  FuncSet         = $20;
  FSInterface8Bit = $10;
  FSInterface4Bit = $00;
  FSTwoLine       = $08;
  FSOneLine       = $00;
  FSBigFont       = $04;
  FSSmallFont     = $00;

  SetCGRamAddr    = $40;

  SetDDRamAddr    = $80;

  // delays required by device
  uiDelayShort    = 40;
  uiDelayMed      = 100;
  uiDelayLong     = 1640;
  uiDelayInit     = 4100;

type
  TControllers = (All, C1, C2);

  TLCD_HD44780 = class
  private
    bHDKS0073Addressing : boolean;
    bHDAltAddressing : boolean;
    iHDTimingMultiplier : integer;
    CurrentX, CurrentY: Word;
    fWidth, fHeight: byte;
    bTwoControllers : Boolean;
    bHighResTimers: Boolean;
    iHighResTimerFreq: Int64;
    procedure Clear;
    procedure InitDisplay;
    procedure WriteString(const Controllers : TControllers; S : String);
  public
    property  TwoControllers : boolean read bTwoControllers write bTwoControllers default false;
    // delays
    procedure UsecDelay(uiUsecs: Cardinal);
    // com stuff
    procedure WriteControl(const Controllers : TControllers; const AValue: byte); virtual;
    procedure WriteData(const Controllers : TControllers; const AValue: byte); virtual;
    procedure LoadIO; virtual;
    procedure UnloadIO; virtual;
    // display stuff
    procedure SetBacklight(State : boolean); virtual;
    procedure CustomChar(Character : integer; Data : array of byte);
    procedure SetPosition(X,Y : integer);
    procedure Write(Str : String);
    // create,destroy
    constructor Create(const Width, heigth: byte;
                       const TimingMult : integer; const A1x16,AKS0073 : boolean);
    destructor Destroy; override;
  end;

function SubString(var S : string) : string;
function StrToBool(T : string) : boolean;

implementation

uses
  SysUtils;

function StrToBool(T : string) : boolean;
begin
  T := Uppercase(Trim(T));
  StrToBool := (T = '1') or
               (T = 'YES') or
               (T = 'Y') or
               (T = 'T') or
               (T = 'ON') or
               (T = 'true') or
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

constructor TLCD_HD44780.Create(const Width, heigth: byte;
                                const TimingMult : integer; const A1x16,AKS0073 : boolean);
begin
  bHDKS0073Addressing := AKS0073;
  bHDAltAddressing := A1x16;
  iHDTimingMultiplier := TimingMult;
  bHighResTimers := QueryPerformanceFrequency(iHighResTimerFreq);
  fWidth := Width;
  fHeight := heigth;
  LoadIO;
  InitDisplay;
end;

destructor TLCD_HD44780.Destroy;
begin
  Clear;
  SetBacklight(false);
  WriteControl(All, OnOffCtrl or OODisplayOff);
  UsecDelay(uiDelayShort);
  UnloadIO;
  inherited;
end;

procedure TLCD_HD44780.SetBacklight(State : boolean);
begin
end;

procedure TLCD_HD44780.WriteControl(const Controllers : TControllers; const AValue: byte);
begin
end;

procedure TLCD_HD44780.WriteData(const Controllers : TControllers; const AValue: byte);
begin
end;

procedure TLCD_HD44780.LoadIO;
begin
end;

procedure TLCD_HD44780.UnloadIO;
begin
end;

procedure TLCD_HD44780.WriteString(const Controllers : TControllers; S : String);
var
  I : byte;
begin
  I := 1;
  while (I <= length(S)) and (CurrentX <= fWidth) do begin

    // special case for 1 chip 1x16 displays
    if (bHDAltAddressing) and (fWidth = 16) and (fHeight = 1) and (CurrentX = 9) then
      SetPosition(CurrentX, CurrentY);

    WriteData(Controllers, ord(S[I]));
    inc(I);
    inc(CurrentX);
  end;
end;

procedure TLCD_HD44780.Clear;
begin
  WriteControl(All, ClearScreen);
  UsecDelay(uiDelayLong);
end;

procedure TLCD_HD44780.UsecDelay(uiUsecs: Cardinal);
var
  uiElapsed: int64;
  uiUsecsScaled: int64;
  iBegin, iCurr: int64;
begin
  {$R-}
  uiUsecsScaled := int64(uiUsecs) * int64(iHDTimingMultiplier);

  if (uiUsecs <= 0) then exit;

  if (bHighResTimers) then begin
    QueryPerformanceCounter(iBegin);
    repeat
      QueryPerformanceCounter(iCurr);

      if (iCurr < iBegin) then iBegin := 0;
      uiElapsed := ((iCurr - iBegin) * 1000000) div iHighResTimerFreq;

    until (uiElapsed > uiUsecsScaled);
  end else begin
    raise Exception.Create('PerformanceCounter not supported on this system');
  end;

  {$R+}
end;

procedure TLCD_HD44780.InitDisplay;
begin
  CurrentX := 1;
  CurrentY := 1;

  //defining wether lcd is 1 or 2 controller based
  bTwoControllers := (fWidth * fHeight > 80);

  // perform initalising by instruction, just in case std power reset failed.
  // 8 bit, 2 lines, 5x8 font
  WriteControl(All, FuncSet or FSInterface8Bit or FSTwoLine or FSSmallFont);
  UsecDelay(uiDelayInit);

  WriteControl(All, FuncSet or FSInterface8Bit or FSTwoLine or FSSmallFont);
  UsecDelay(uiDelayMed);

  WriteControl(All, FuncSet or FSInterface8Bit or FSTwoLine or FSSmallFont);
  UsecDelay(uiDelayShort);

  WriteControl(All, FuncSet or FSInterface8Bit or FSTwoLine or FSSmallFont);
  UsecDelay(uiDelayShort);


  if (bHDKS0073Addressing = true) then begin
    { Need to set extended functions }
    WriteControl(All, FuncSet or FSInterface8Bit or FSTwoLine or FSSmallFont or FSExtReg);
    UsecDelay(uiDelayShort);
    if (fHeight = 4) then
      WriteControl(All, ExtFuncSet or EFSFourLine)
    else
      WriteControl(All, ExtFuncSet or EFSOneTwoLine);
    UsecDelay(uiDelayShort);
    WriteControl(All, FuncSet or FSInterface8Bit or FSTwoLine or FSSmallFont);
  end;

  // display off
  WriteControl(All, OnOffCtrl or OODisplayOff);
  UsecDelay(uiDelayShort);

  // clear display
  WriteControl(All, ClearScreen);
  UsecDelay(uiDelayLong);

  // cursor increments, no display shift
  WriteControl(All, EntryMode or EMIncrement or EMNoShift);
  UsecDelay(uiDelayMed);

  // cursor move
  WriteControl(All, CursorDispShift or CDMoveCursor or CDShiftLeft);
  UsecDelay(uiDelayShort);

  // initialization finished.

  // display on, no blinking cursor
  WriteControl(All, OnOffCtrl or  OODisplayOn or OOCursorOff or OOCursorNoBlink);
  UsecDelay(uiDelayLong);

  // home cursor
  WriteControl(All, HomeCursor);
  UsecDelay(uiDelayLong);
end;

procedure TLCD_HD44780.CustomChar(Character : integer; Data : array of byte);
var
  Address : byte;
begin
  Address := SetCGRamAddr + (Character - 1) * 8;
  WriteControl(All, Address);
  for Address := 0 to 7 do
    WriteData(All, Data[Address]);
  SetPosition(CurrentX,CurrentY);
end;

procedure TLCD_HD44780.SetPosition(X,Y : integer);
{  DDAddr :=  (min(X,40) - 1) +  ($40*((Y-1) mod 2)) + (((Y-1) div 2) * LineLength);}
var
  TempX, TempY: byte;
  DDAddr : byte;
  Controller : TControllers;
begin
  // store theses values as they are used when a Write occurs.
  CurrentX := X;
  CurrentY := Y;
  // cursor positions are 0 based in display
  TempX := X - 1;
  TempY := Y - 1;
  // figure out which controller we're talking to
  if (bTwoControllers) and (TempY >= (fHeight div 2)) then begin
    TempY := TempY mod (fHeight div 2);
    Controller := C2;
  end else
    Controller := C1;
  // figure out address
  if not bHDKS0073Addressing then begin  // Find DDRAM Address for HDD44780
    // special case for 1 chip 1x16 displays, acts like 2x8 display
    if (bHDAltAddressing) and (fWidth = 16) and
       (fHeight = 1) and (TempX >= 8) then begin
      TempX := TempX - 8;
      TempY := TempY + 1;
    end;
    DDAddr := TempX + (TempY mod 2) * $40;
    // line 3 logically follows line 1, (same for 4 and 2)
    DDAddr := DDAddr + (TempY div 2) * fWidth;
  end else begin // Find DDRAM Address for KS0073
    { Addressing:  Line 1 - 0x00    Line 2 - 0x20   Line 3 - 0x40   Line 4 - 0x60 }
    DDAddr := TempX + $20 * TempY;
  end;

  WriteControl(Controller, SetDDRamAddr or DDAddr);
end;

procedure TLCD_HD44780.Write(Str : String);
begin
  if (not bTwoControllers) or (CurrentY < ((fHeight div 2)+1)) then
    WriteString(C1, Str)
  else
    WriteString(C2, Str);
end;

end.


