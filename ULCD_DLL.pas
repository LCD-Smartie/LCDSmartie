unit ULCD_DLL;

interface

uses
  ULCD;

type
  pboolean = ^boolean;
  TCustomArray = array[0..7] of byte;

  TInitFunc = function(SizeX,SizeY : byte; StartupParameters : pchar; OK : pboolean) : pchar; stdcall;
  TDoneProc = procedure; stdcall;
  TSetPositionProc = procedure(X, Y: byte); stdcall;
  TWriteProc = procedure(Str : pchar); stdcall;
  TCustomChar = procedure(Chr : byte; Data : TCustomArray); stdcall;
  TReadKeyFunc = function : word; stdcall;
  TSetBacklightProc = procedure(LightOn : boolean); stdcall;
  TSetContrastProc = procedure(Contrast : byte); stdcall;
  TSetBrightnessProc = procedure(Brightness : byte); stdcall;
  TPowerResumeProc = procedure; stdcall;
  TSetGPOProc = procedure(GPO : byte; GPOOn : boolean); stdcall;
  TSetFanProc = procedure(T1,T2 : byte); stdcall;
  TCustomCharIndex = function(Index : byte) : byte; stdcall;

  TLCD_DLL = class(TLCD)
  public
    procedure SetPosition(X, Y: integer); override;
    procedure Write(Str: string); override;
    procedure CustomChar(Chr: integer; Data: array of byte); override;
    procedure Setbacklight(On: boolean); override;
    function  ReadKey(var Key: char) : Boolean; override;
    procedure SetFan(T1, T2: integer); override;
    procedure SetGPO(gpo: Byte; On: boolean); override;
    procedure SetContrast(Level: integer); override;
    procedure SetBrightness(Level: integer); override;
    procedure PowerResume; override;
    constructor CreateDLL(SizeX,SizeY : byte; DLLName,StartupParameters : string);
    destructor Destroy; override;
  private
    { Private declarations }
    MyDLL: HMODULE;
    InitFunc : TInitFunc;
    DoneProc : TDoneProc;
    SetPositionProc : TSetPositionProc;
    WriteProc : TWriteProc;
    CustomCharProc : TCustomChar;
    ReadKeyFunc : TReadKeyFunc;
    SetBacklightProc : TSetBacklightProc;
    SetContrastProc : TSetContrastProc;
    SetBrightnessProc : TSetBrightnessProc;
    PowerResumeProc : TPowerResumeProc;
    SetGPOProc : TSetGPOProc;
    SetFanProc : TSetFanProc;
    CustomIndex : TCustomCharIndex;
    CustomCharIndex : array[1..8] of byte;
    OK : boolean;
  end;

implementation

uses
  Windows,SysUtils;

constructor TLCD_DLL.CreateDLL(SizeX,SizeY : byte; DLLName,StartupParameters : string);
var
  S : string;
  Loop : byte;
begin
  MyDLL := 0;
  InitFunc := nil;
  DoneProc := nil;
  SetPositionProc := nil;
  WriteProc := nil;
  CustomCharProc := nil;
  ReadKeyFunc := nil;
  SetBacklightProc := nil;
  SetContrastProc := nil;
  SetBrightnessProc := nil;
  PowerResumeProc := nil;
  SetGPOProc := nil;
  SetFanProc := nil;
  CustomIndex := nil;
  CustomCharIndex[1] := 176;
  CustomCharIndex[2] := 158;
  for Loop := 3 to 8 do
    CustomCharIndex[Loop] := 128+Loop;
  OK := false;
  if (DLLName = '') then exit;
  OK := true;
  S := extractfilepath(paramstr(0))+'displays\'+DLLName;
  if not FileExists(S) then begin
    raise EInOutError.Create('DLL not found: '+S);
    OK := false;
  end;
  if OK then begin
    try
      MyDLL := LoadLibrary(pchar(S));
      if not (MyDll = 0) then begin
        InitFunc := GetProcAddress(MyDLL,pchar('DISPLAYDLL_Init'));
        DoneProc := GetProcAddress(MyDLL,pchar('DISPLAYDLL_Done'));
        SetPositionProc := GetProcAddress(MyDLL,pchar('DISPLAYDLL_SetPosition'));
        WriteProc := GetProcAddress(MyDLL,pchar('DISPLAYDLL_Write'));
        CustomCharProc := GetProcAddress(MyDLL,pchar('DISPLAYDLL_CustomChar'));
        ReadKeyFunc := GetProcAddress(MyDLL,pchar('DISPLAYDLL_ReadKey'));
        SetBacklightProc := GetProcAddress(MyDLL,pchar('DISPLAYDLL_SetBacklight'));
        SetContrastProc := GetProcAddress(MyDLL,pchar('DISPLAYDLL_SetContrast'));
        SetBrightnessProc := GetProcAddress(MyDLL,pchar('DISPLAYDLL_SetBrightness'));
        PowerResumeProc := GetProcAddress(MyDLL,pchar('DISPLAYDLL_PowerResume'));
        SetGPOProc := GetProcAddress(MyDLL,pchar('DISPLAYDLL_SetGPO'));
        SetFanProc := GetProcAddress(MyDLL,pchar('DISPLAYDLL_SetFan'));
        CustomIndex := GetProcAddress(MyDLL,pchar('DISPLAYDLL_CustomCharIndex'));
      end;
    except
      on E:Exception do begin
        OK := false;
        raise EInOutError.Create('Exception while loading DLL.');
      end;
    end;
  end;
  if OK then begin
    if assigned(InitFunc) then begin
      OK := false;
      S := string(InitFunc(SizeX,SizeY,pchar(StartupParameters),@OK));
      if not OK then
        raise EInOutError.Create('DLL Initialize error: '+S)
      else begin
        if assigned(CustomIndex) then begin
          for Loop := 1 to 8 do
            CustomCharIndex[Loop] := CustomIndex(Loop);
        end;
      end;
    end;
  end;
  inherited;
end;

destructor TLCD_DLL.Destroy;
begin
  if not (MyDLL = 0) then begin
    if assigned(DoneProc) then begin
      try
        DoneProc;
      except
      end;
    end;
    FreeLibrary(MyDLL);
  end;
  inherited;
end;

procedure TLCD_DLL.PowerResume;
begin
  if OK and assigned(PowerResumeProc) then begin
    try
      PowerResumeProc;
    except
    end;
  end;
end;

procedure TLCD_DLL.setContrast(Level: integer);
begin
  if OK and assigned(SetContrastProc) then begin
    try
      SetContrastProc(Level);
    except
    end;
  end;
end;

procedure TLCD_DLL.SetBrightness(Level: integer);
begin
  if OK and assigned(SetBrightnessProc) then begin
    try
      SetBrightnessProc(Level);
    except
    end;
  end;
end;

procedure TLCD_DLL.setGPO(GPO: byte; On: boolean);
begin
  if OK and assigned(SetGPOProc) then begin
    try
      SetGPOProc(GPO,On);
    except
    end;
  end;
end;

procedure TLCD_DLL.setFan(T1, T2: integer);
begin
  if OK and assigned(SetFanProc) then begin
    try
      SetFanProc(T1,T2);
    except
    end;
  end;
end;

procedure TLCD_DLL.SetBacklight(On: boolean);
begin
  if OK and assigned(SetBacklightProc) then begin
    try
      SetBacklightProc(On);
    except
    end;
  end;
end;

procedure TLCD_DLL.CustomChar(Chr: Integer; Data: array of byte);
var
  Data1 : TCustomArray;
begin
  if OK and assigned(CustomCharProc) then begin
    try
      move(Data,Data1,8);
      CustomCharProc(Chr,Data1);
    except
    end;
  end;
end;

procedure TLCD_DLL.SetPosition(x, y: integer);
begin
  if OK and assigned(SetPositionProc) then begin
    try
      SetPositionProc(X,Y);
    except
    end;
  end;
end;

function TLCD_DLL.ReadKey(var Key: char) : Boolean;
var
  W : word;
begin
  Result := false;
  Key := #0;
  if OK and assigned(ReadKeyFunc) then begin
    try
      W := ReadKeyFunc;
      if (W < 256) then begin
        Key := char(W and $FF);
        Result := true;
      end;
    except
    end;
  end;
end;

procedure TLCD_DLL.Write(Str: String);
var
  i: byte;
begin
  if OK then begin
    if assigned(CustomIndex) then begin
      for i:= 1 to Length(str) do
      begin
        case byte(str[i]) of
          176 : str[i] := Chr(CustomCharIndex[1]);
          158 : str[i] := Chr(CustomCharIndex[2]);
          131 : str[i] := Chr(CustomCharIndex[3]);
          132 : str[i] := Chr(CustomCharIndex[4]);
          133 : str[i] := Chr(CustomCharIndex[5]);
          134 : str[i] := Chr(CustomCharIndex[6]);
          135 : str[i] := Chr(CustomCharIndex[7]);
          136 : str[i] := Chr(CustomCharIndex[8]);
        end;
      end;
    end;
    if assigned(WriteProc) then begin
      try
        WriteProc(pchar(Str));
      except
      end;
    end;
  end;
end;

end.
