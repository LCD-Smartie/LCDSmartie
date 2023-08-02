library Tee;

{$MODE Delphi}

uses
  Classes, SysUtils, windows, IniFiles, TeeConfig, Forms,
  Interfaces
  { you can add units after this };

const
  DLLProjectName = 'Tee Driver DLL';
  Version = 'v0.1';
  IniFile = 'Tee.ini';
  MaxDrivers = 10;

type
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

type
  TClientDll = record
    DriverPath: string;
    DriverParams: string;
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

  TTeeDrv = class
  public
    ClientDrivers: Array [1..10] of TClientDll;
  private
    { Private declarations }
  end;

var
 TeeConfig : TTeeConfig = nil;
 TeeDrv: TTeeDrv;

function DISPLAYDLL_Init(SizeX,SizeY : byte; StartupParameters : pchar; OK : pboolean) : pchar; stdcall;
var
  S : string;
  Loop, i, j: integer;
  Settings : TIniFile;
  path: string;
begin
  OK^ := true;
  S := StartupParameters;

  try
    if ( pos('config', lowercase(S)) > 0) then
    begin
      TeeConfig := TTeeConfig.Create(nil);
      TeeConfig.ShowModal;
    end;

    Settings := TIniFile.Create(IniFile);
    TeeDrv := TTeeDrv.Create;

    for i := 1 to MaxDrivers do
    begin
      TeeDrv.ClientDrivers[i].DriverPath := 'displays\' + Settings.ReadString('Driver'+inttostr(i), 'DllName', '');
      TeeDrv.ClientDrivers[i].DriverParams := Settings.ReadString('Driver'+inttostr(i), 'DllParameters', '');
    end;

    Result := PChar(DLLProjectName + ' ' + Version + #0);

    // try to load drivers
    for i := 1 to MaxDrivers do
    begin
      for j := 1 to MaxDrivers do
      begin
        if (TeeDrv.ClientDrivers[i].DriverPath = TeeDrv.ClientDrivers[j].DriverPath) and (i <> j) then
        begin
          // OK, we've seen this driver before but we can only load one at a time
          // create temp dir
          CreateDir(GetTempDir+'displays');
          // copy the driver to temp dir
          CopyFile(pchar(TeeDrv.ClientDrivers[i].DriverPath), pchar(GetTempDir+TeeDrv.ClientDrivers[i].DriverPath+inttostr(i)), false);
          // Change the path to point to the new dll
          TeeDrv.ClientDrivers[i].DriverPath := GetTempDir+TeeDrv.ClientDrivers[i].DriverPath+inttostr(i);
        end;
      end;

      TeeDrv.ClientDrivers[i].MyDLL := LoadLibrary(pchar(TeeDrv.ClientDrivers[i].DriverPath));
      if not (TeeDrv.ClientDrivers[i].MyDll = 0) then
      begin
        TeeDrv.ClientDrivers[i].InitFunc := GetProcAddress(TeeDrv.ClientDrivers[i].MyDLL,pchar('DISPLAYDLL_Init'));
        TeeDrv.ClientDrivers[i].DoneProc := GetProcAddress(TeeDrv.ClientDrivers[i].MyDLL,pchar('DISPLAYDLL_Done'));
        TeeDrv.ClientDrivers[i].SetPositionProc := GetProcAddress(TeeDrv.ClientDrivers[i].MyDLL,pchar('DISPLAYDLL_SetPosition'));
        TeeDrv.ClientDrivers[i].WriteProc := GetProcAddress(TeeDrv.ClientDrivers[i].MyDLL,pchar('DISPLAYDLL_Write'));
        TeeDrv.ClientDrivers[i].CustomCharProc := GetProcAddress(TeeDrv.ClientDrivers[i].MyDLL,pchar('DISPLAYDLL_CustomChar'));
        TeeDrv.ClientDrivers[i].ReadKeyFunc := GetProcAddress(TeeDrv.ClientDrivers[i].MyDLL,pchar('DISPLAYDLL_ReadKey'));
        TeeDrv.ClientDrivers[i].SetBacklightProc := GetProcAddress(TeeDrv.ClientDrivers[i].MyDLL,pchar('DISPLAYDLL_SetBacklight'));
        TeeDrv.ClientDrivers[i].SetContrastProc := GetProcAddress(TeeDrv.ClientDrivers[i].MyDLL,pchar('DISPLAYDLL_SetContrast'));
        TeeDrv.ClientDrivers[i].SetBrightnessProc := GetProcAddress(TeeDrv.ClientDrivers[i].MyDLL,pchar('DISPLAYDLL_SetBrightness'));
        TeeDrv.ClientDrivers[i].PowerResumeProc := GetProcAddress(TeeDrv.ClientDrivers[i].MyDLL,pchar('DISPLAYDLL_PowerResume'));
        TeeDrv.ClientDrivers[i].SetGPOProc := GetProcAddress(TeeDrv.ClientDrivers[i].MyDLL,pchar('DISPLAYDLL_SetGPO'));
        TeeDrv.ClientDrivers[i].SetFanProc := GetProcAddress(TeeDrv.ClientDrivers[i].MyDLL,pchar('DISPLAYDLL_SetFan'));
        TeeDrv.ClientDrivers[i].CustomIndex := GetProcAddress(TeeDrv.ClientDrivers[i].MyDLL,pchar('DISPLAYDLL_CustomCharIndex'));
      end;
    end;

    // run everyones initfunc
    for i := 1 to MaxDrivers do
    begin
      if assigned(TeeDrv.ClientDrivers[i].InitFunc) then
      begin
        TeeDrv.ClientDrivers[i].OK := false;
        S := string(TeeDrv.ClientDrivers[i].InitFunc(SizeX,SizeY,pchar(TeeDrv.ClientDrivers[i].DriverParams),@TeeDrv.ClientDrivers[i].OK));
        if not TeeDrv.ClientDrivers[i].OK then
          raise EInOutError.Create('DLL Initialize error: '+S)
        else
        begin
          if assigned(TeeDrv.ClientDrivers[i].CustomIndex) then
          begin
            for Loop := 1 to 8 do
              TeeDrv.ClientDrivers[i].CustomCharIndex[Loop] := TeeDrv.ClientDrivers[i].CustomIndex(Loop);
          end
          else
          begin
            TeeDrv.ClientDrivers[i].CustomCharIndex[1] := 176;
            TeeDrv.ClientDrivers[i].CustomCharIndex[2] := 158;
            for Loop := 3 to 8 do
              TeeDrv.ClientDrivers[i].CustomCharIndex[Loop] := 128+Loop;
          end;
        end;
      end;
    end;

  except
    on E: Exception do begin
      result := PChar('Tee.DLL Exception: ' + E.Message + #0);
      OK^ := false;
    end;
  end;
end;

procedure DISPLAYDLL_Done(); stdcall;
var
  i: integer;
begin
  for i := 1 to MaxDrivers do
  begin
    if not (TeeDrv.ClientDrivers[i].MyDLL = 0) then
    begin
      if assigned(TeeDrv.ClientDrivers[i].DoneProc) then
      begin
        try
          TeeDrv.ClientDrivers[i].DoneProc;
        except
        end;
      end;
      FreeLibrary(TeeDrv.ClientDrivers[i].MyDLL);
    end;
  end;
  if assigned(TeeConfig) then
    freeandnil(TeeConfig);
  freeandnil(TeeDrv);
end;

procedure DISPLAYDLL_SetPosition(X, Y: byte); stdcall;
var
  i: integer;
begin
  for i := 1 to MaxDrivers do
  begin
    if not (TeeDrv.ClientDrivers[i].MyDLL = 0) then
    begin
      if assigned(TeeDrv.ClientDrivers[i].SetPositionProc) then
      begin
        try
          TeeDrv.ClientDrivers[i].SetPositionProc(X,Y);;
        except
        end;
      end;
    end;
  end;
end;

procedure DISPLAYDLL_Write(Str : pchar); stdcall;
var
  S : string;
  i, j: integer;
begin
  for i := 1 to MaxDrivers do
  begin
    S := Str;
    if assigned(TeeDrv.ClientDrivers[i].CustomIndex) then
    begin
        for j:= 1 to Length(S) do
        begin
          case byte(S[j]) of
            176 : S[j] := Chr(TeeDrv.ClientDrivers[i].CustomCharIndex[1]);
            158 : S[j] := Chr(TeeDrv.ClientDrivers[i].CustomCharIndex[2]);
            131 : S[j] := Chr(TeeDrv.ClientDrivers[i].CustomCharIndex[3]);
            132 : S[j] := Chr(TeeDrv.ClientDrivers[i].CustomCharIndex[4]);
            133 : S[j] := Chr(TeeDrv.ClientDrivers[i].CustomCharIndex[5]);
            134 : S[j] := Chr(TeeDrv.ClientDrivers[i].CustomCharIndex[6]);
            135 : S[j] := Chr(TeeDrv.ClientDrivers[i].CustomCharIndex[7]);
            136 : S[j] := Chr(TeeDrv.ClientDrivers[i].CustomCharIndex[8]);
          end;
        end;
    end;
    if assigned(TeeDrv.ClientDrivers[i].WriteProc) then
    begin
      try
        TeeDrv.ClientDrivers[i].WriteProc(pchar(S));
      except
      end;
    end;
  end;
end;

procedure DISPLAYDLL_CustomChar(Char : byte; Data : TCustomArray); stdcall;
var
  i: integer;
begin
  for i := 1 to MaxDrivers do
  begin
    if assigned(TeeDrv.ClientDrivers[i].CustomCharProc) then
      TeeDrv.ClientDrivers[i].CustomCharProc(Char,Data);
  end;
end;

function DISPLAYDLL_ReadKey : word; stdcall;
var
  W : word;
  i: integer;
begin
  for i := 1 to MaxDrivers do
  begin
    if assigned(TeeDrv.ClientDrivers[i].ReadKeyFunc) then
    begin
      W := TeeDrv.ClientDrivers[i].ReadKeyFunc;
      if (W < 256) then begin
        Result := byte(W and $FF);
        Exit;
      end
    end;
  end;
  Result := $ff00;
end;

procedure DISPLAYDLL_SetBacklight(LightOn : boolean); stdcall;
var
  i: integer;
begin
  for i := 1 to MaxDrivers do
  begin
    if assigned(TeeDrv.ClientDrivers[i].SetBacklightProc) then
    begin
      TeeDrv.ClientDrivers[i].SetBacklightProc(LightOn);
    end;
  end;
end;

procedure DISPLAYDLL_SetContrast(Contrast : byte); stdcall;
var
  i: integer;
begin
  for i := 1 to MaxDrivers do
  begin
    if assigned(TeeDrv.ClientDrivers[i].SetContrastProc) then
    begin
      TeeDrv.ClientDrivers[i].SetContrastProc(Contrast);
    end;
  end;
end;

procedure DISPLAYDLL_SetBrightness(Brightness : byte); stdcall;
var
  i: integer;
begin
  for i := 1 to MaxDrivers do
  begin
    if assigned(TeeDrv.ClientDrivers[i].SetBrightnessProc) then
    begin
      TeeDrv.ClientDrivers[i].SetBrightnessProc(Brightness);
    end;
  end;
end;

procedure DISPLAYDLL_SetGPO(GPO : byte; GPOOn : boolean); stdcall;
var
  i: integer;
begin
  for i := 1 to MaxDrivers do
  begin
    if assigned(TeeDrv.ClientDrivers[i].SetGPOProc) then
    begin
      TeeDrv.ClientDrivers[i].SetGPOProc(GPO, GPOOn);
    end;
  end;
end;

procedure DISPLAYDLL_SetFan(T1,T2 : byte); stdcall;
var
  i: integer;
begin
  for i := 1 to MaxDrivers do
  begin
    if assigned(TeeDrv.ClientDrivers[i].SetFanProc) then
    begin
      TeeDrv.ClientDrivers[i].SetFanProc(T1, T2);
    end;
  end;
end;

function DISPLAYDLL_DefaultParameters : pchar; stdcall;
begin
  DISPLAYDLL_DefaultParameters := pchar('config' + #0);
end;

function DISPLAYDLL_Usage : pchar; stdcall;
begin
  Result := pchar('Usage: ''config'' to open config blank otherwise' + #0);
end;

function DISPLAYDLL_DriverName : pchar; stdcall;
begin
  Result := PChar(DLLProjectName + ' ' + Version + #0);
end;

// don't forget to export the funtions, else nothing works :)
exports
DISPLAYDLL_SetFan,
DISPLAYDLL_SetGPO,
DISPLAYDLL_SetBrightness,
DISPLAYDLL_SetContrast,
DISPLAYDLL_SetBacklight,
DISPLAYDLL_ReadKey,
DISPLAYDLL_CustomChar,
DISPLAYDLL_Write,
DISPLAYDLL_SetPosition,
DISPLAYDLL_DefaultParameters,
DISPLAYDLL_Usage,
DISPLAYDLL_DriverName,
DISPLAYDLL_Done,
DISPLAYDLL_Init;

begin
  Application.Initialize;
  //Application.CreateForm(TTeeConfig, TeeConfig);
end.

