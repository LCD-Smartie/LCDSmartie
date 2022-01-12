library HD44780S;

{$R *.res}

uses
  Windows,SysUtils,Math,
  HD44780OBJ,SERPORT;

(*

 revhist

1.0 initial driver
1.1 combined with HD44780 parallel display

*)

const
  DLLProjectName = 'HD44780 Serial Display DLL';
  Version = 'v1.1';
type
  pboolean = ^boolean;
  TCustomArray = array[0..7] of byte;

  TLCD_HD_COM = class(TLCD_HD44780)
  private
    fPortParams : string;
    COMPort : TSerialPort;
  public
    procedure SetBacklight(State : boolean); override;
    procedure WriteControl(const Controllers : TControllers; const AValue: byte); override;
    procedure WriteData(const Controllers : TControllers; const AValue: byte); override;
    procedure LoadIO; override;
    procedure UnloadIO; override;
    constructor Create(PortParams : string; const Width, Height: byte;
                       const TimingMult : integer; const A1x16,AKS0073 : boolean);
    destructor Destroy; override;
  end;

var
  LCD_HD_COM : TLCD_HD_COM = nil;

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

procedure TLCD_HD_COM.SetBacklight(State : boolean);
begin
  if State then WriteControl(All,$03)
  else WriteControl(All,$02);
end;

procedure TLCD_HD_COM.WriteControl(const Controllers : TControllers; const AValue : byte);
begin
  if assigned(COMPort) then begin
    COMPort.WriteByte($FE);
    COMPort.WriteByte(AValue);
  end;
end;

procedure TLCD_HD_COM.WriteData(const Controllers : TControllers; const AValue : byte);
begin
  if assigned(COMPort) then
    COMPort.WriteByte(AValue);
end;

procedure TLCD_HD_COM.LoadIO;
begin
  COMPort := TSerialPort.Create;
  COMPort.OpenSerialPort(fPortParams);
end;

procedure TLCD_HD_COM.UnloadIO;
begin
  if assigned(COMPort) then begin
    COMPort.Free;
    COMPort := nil;
  end;
end;

constructor TLCD_HD_COM.Create(PortParams : string; const Width, Height: byte;
                               const TimingMult : integer; const A1x16,AKS0073 : boolean);
begin
  COMPort := nil;
  fPortParams := PortParams;
  inherited Create(Width,Height,TimingMult,A1x16,AKS0073);
  TwoControllers := false;  // never in a serial controller
end;

destructor TLCD_HD_COM.Destroy;
begin
  inherited;
end;

procedure DISPLAYDLL_CustomChar(Chr : byte; Data : TCustomArray); stdcall;
// define custom character
begin
  try
    if assigned(LCD_HD_COM) then
       LCD_HD_COM.CustomChar(Chr,Data);
  except
  end;
end;

procedure DISPLAYDLL_Write(Str : pchar); stdcall;
// Write string
var
  S : string;
begin
  try
    if assigned(LCD_HD_COM) then begin
      S := string(Str);
      LCD_HD_COM.Write(S);
    end;
  except
  end;
end;

procedure DISPLAYDLL_SetBacklight(LightOn : boolean); stdcall;
// turn on Backlighting
begin
  try
    if assigned(LCD_HD_COM) then
       LCD_HD_COM.SetBacklight(LightOn);
  except
  end;
end;

procedure DISPLAYDLL_SetPosition(X, Y: byte); stdcall;
// set cursor position
begin
  try
    if assigned(LCD_HD_COM) then
       LCD_HD_COM.SetPosition(X,Y);
  except
  end;
end;

function DISPLAYDLL_CustomCharIndex(Index : byte) : byte; stdcall;
begin
  DISPLAYDLL_CustomCharIndex := 7+Index;
end;

function DISPLAYDLL_Init(SizeX,SizeY : byte; StartupParameters : pchar; OK : pboolean) : pchar; stdcall;
// return startup error
// open port
var
  PortParams,S,Params : string;
  Mult : integer;
  b1x16,bKS0073 : boolean;
begin
  OK^ := true;
  Result := PChar(DLLProjectName + ' ' + Version + #0);
  try
    Params := uppercase(string(StartupParameters));
    PortParams := substring(Params) + ',' + substring(Params);  // get COM1,9600
    PortParams := PortParams + ',8,N,1';
    Mult := 1;
    b1x16 := false;
    bKS0073 := false;
    S := SubString(Params);
    if (S <> '') then
      Mult := StrToInt(S);
    S := SubString(Params);
    if (S <> '') then
      b1x16 := StrToBool(S);
    S := SubString(Params);
    if (S <> '') then
      bKS0073 := StrToBool(S);
    LCD_HD_COM := TLCD_HD_COM.Create(PortParams,SizeX,SizeY,Mult,b1x16,bKS0073);
  except
    on E: Exception do begin
      result := PChar('HD44780S.DLL Exception: ' + E.Message + #0);
      OK^ := false;
    end;
  end;
end;

procedure DISPLAYDLL_Done(); stdcall;
// close port
begin
  try
    if assigned(LCD_HD_COM) then begin
      LCD_HD_COM.Free;
      LCD_HD_COM := nil;
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
  Result := pchar('Usage: COM1,9600[,m,a1,a2]'+#13#10+
                  'm = timing multiplier'+#13#10+
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

