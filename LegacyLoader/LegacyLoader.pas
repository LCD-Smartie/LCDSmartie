program LegacyLoader;

uses
  Classes, SysUtils, CustApp,  Windows, fpTimer;

const
// if this is increased, increase the 3 legacyloader function ID's also
  iMaxPluginFuncs = 20;
  finiFuncId = 21;
  infoFuncId = 22;
  demoFuncId = 23;

type
    TSharedMem=record
    LibraryID: Integer;
    LibraryPath: Array[1..255] of char;
    LibraryFunc: Integer;
    LibraryParam1: Array[1..2048] of char;
    LibraryParam2: Array[1..2048] of char;
    LibraryResult: Array[1..2048] of char;
  end;

  TFunctionProc = function(param1: pchar; param2: pchar): Pchar; stdcall;
  TinfoProc = function(): Pchar; stdcall;
  TdemoProc = function(): Pchar; stdcall;
  TFiniProc = procedure(); stdcall;

  TDll = Record
    sName: String;
    hDll: HMODULE;
    functions: Array [1..iMaxPluginFuncs] of TFunctionProc;
    infoFunc: TinfoProc;
    demoFunc: TdemoProc;
    finiFunc: TFiniProc;
    uiMinRefreshInterval: Cardinal;
  end;

  { LegacyPluginLoader }

  LegacyPluginLoader = class(TCustomApplication)
  private
    dlls: Array of TDll;
    uiTotalDlls: Cardinal;
    FormatSettings : TFormatSettings;
    procedure LoadPlugin(PluginPath: string);
    function ExecuteFunction(ID: integer; iFunc: integer; sParam1: string; sparam2: string): string;
    procedure Timercheck(Sender: TObject);
    procedure ConsoleWrite(S: String);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

   PTSharedMem=^TSharedMem;

var
  Application: LegacyPluginLoader;
  hMap: THandle;
  LegacySharedMem: PTSharedMem;
  hLegacyLoadLibraryEvent: THandle;
  hLegacyCallFunctionEvent: THandle;
  hLegacyRecvEvent: THandle;
  waitresult:dword;
  HandleArray: array[0..1] of THandle;
  timer1: TFPTimer;
  ShMemID: string;
  console: boolean;

procedure LegacyPluginLoader.DoRun;
begin
  // hope all this never gets called from multiple threads
  if  not ( hMap > 0) then
    hMap:=OpenFileMapping(FILE_MAP_ALL_ACCESS, False, Pchar('Local\LCDSmartieLegacyPluginLoaderSM' + ShMemID));
  if hMap = 0 then
     Terminate;

  if not (LegacySharedMem <> nil) then
    LegacySharedMem := MapViewOfFile(hMap, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  if LegacySharedMem = nil then
    Terminate;

  if  not ( hLegacyLoadLibraryEvent > 0) then
    hLegacyLoadLibraryEvent := OpenEvent(EVENT_ALL_ACCESS, False, Pchar('Local\LCDSmartieLegacyLoadLibraryEvent' + ShMemID));
  if hLegacyLoadLibraryEvent = 0 then
    terminate;

  if  not ( hLegacyCallFunctionEvent > 0) then
    hLegacyCallFunctionEvent := OpenEvent(EVENT_ALL_ACCESS, False, Pchar('Local\LCDSmartieLegacyCallFunctionEvent' + ShMemID));
  if hLegacyCallFunctionEvent = 0 then
    terminate;

  if  not ( hLegacyRecvEvent > 0) then
    hLegacyRecvEvent := OpenEvent(EVENT_ALL_ACCESS, False, Pchar('Local\LCDSmartieLegacyRecvEvent' + ShMemID));
  if hLegacyRecvEvent = 0 then
    terminate;

  HandleArray[0] := hLegacyLoadLibraryEvent;
  HandleArray[1] := hLegacyCallFunctionEvent;
  waitresult := WaitForMultipleObjects(2, PWOHandleArray(@HandleArray), FALSE, INFINITE);
  case (waitresult) of
      0 : begin
            try
              LoadPlugin(strpas(@LegacySharedMem^.LibraryPath));
            except
              on E: Exception do
              begin
                LegacySharedMem^.LibraryResult := e.Message;
                LegacySharedMem^.LibraryID := -1;
                setevent(hLegacyRecvEvent);
                ConsoleWrite(E.Message);
              end;
            end;
          end;
      1 : begin
            try
              begin
                LegacySharedMem^.LibraryResult := ExecuteFunction( LegacySharedMem^.LibraryID,
                  LegacySharedMem^.LibraryFunc, LegacySharedMem^.LibraryParam1,
                    LegacySharedMem^.LibraryParam2);
                setevent(hLegacyRecvEvent);
              end;
            except
              on E: Exception do
              begin
                LegacySharedMem^.LibraryResult := e.Message;
                setevent(hLegacyRecvEvent);
                ConsoleWrite(E.Message);
              end;
            end;
          end;
      3 : terminate;
  end;
end;

procedure LegacyPluginLoader.LoadPlugin(PluginPath: string);
type
  TMinRefreshFunc = function: Integer; stdcall;
var
  uiDll: Cardinal;
  i: Integer;
  minRefreshFunc: TMinRefreshFunc;
  minRefresh: Integer;
  initFunc:  procedure; stdcall;
  err: longword;
begin
  inc(uiTotalDlls);
  uiDll := uiTotalDlls - 1;
  SetLength(dlls, uiTotalDlls);
  dlls[uiDll].uiMinRefreshInterval := 300;
  dlls[uiDll].sName := PluginPath;

  ConsoleWrite('Loading ' + PluginPath + ' with ID: ' + inttostr(uiDll));
  dlls[uiDll].hDll := SafeLoadLibrary(pchar(PluginPath));
  err := GetLastError();

  if (dlls[uiDll].hDll <> 0) then
  begin
    initFunc := getprocaddress(dlls[uiDll].hDll, PChar('SmartieInit'));
    if (not Assigned(initFunc)) then
      initFunc := getprocaddress(dlls[uiDll].hDll, PChar('_SmartieInit@0'));

    dlls[uiDll].finiFunc := getprocaddress(dlls[uiDll].hDll, PChar('SmartieFini'));
    if (not Assigned(dlls[uiDll].finiFunc)) then
      dlls[uiDll].finiFunc := getprocaddress(dlls[uiDll].hDll, PChar('_SmartieFini@0'));

    minRefreshFunc := getprocaddress(dlls[uiDll].hDll, PChar('GetMinRefreshInterval'));
    if (not Assigned(minRefreshFunc)) then
      minRefreshFunc := getprocaddress(dlls[uiDll].hDll, PChar('_GetMinRefreshInterval@0'));

    dlls[uiDll].infoFunc := getprocaddress(dlls[uiDll].hDll, PChar('SmartieInfo'));
    if (not Assigned(dlls[uiDll].infoFunc)) then
      dlls[uiDll].infoFunc := getprocaddress(dlls[uiDll].hDll, PChar('_SmartieInfo@0'));

    dlls[uiDll].demoFunc := getprocaddress(dlls[uiDll].hDll, PChar('SmartieDemo'));
    if (not Assigned(dlls[uiDll].demoFunc)) then
      dlls[uiDll].demoFunc := getprocaddress(dlls[uiDll].hDll, PChar('_SmartieDemo@0'));

    if (Assigned(initFunc)) then
      ConsoleWrite('ID: '+ inttostr(uiDll) + ' Found SmartieInit');

    if (Assigned(dlls[uiDll].finiFunc)) then
      ConsoleWrite('ID: '+ inttostr(uiDll) + ' Found SmartieFini');

    if (Assigned(dlls[uiDll].infoFunc)) then
      ConsoleWrite('ID: '+ inttostr(uiDll) + ' Found SmartieInfo');

    if (Assigned(dlls[uiDll].demoFunc)) then
      ConsoleWrite('ID: '+ inttostr(uiDll) + ' Found SmartieDemo');

    if (Assigned(minRefreshFunc)) then
      ConsoleWrite('ID: '+ inttostr(uiDll) + ' Found GetMinRefreshInterval');

    i := 0;
    repeat
      inc(i);
      @dlls[uiDll].functions[i] := getprocaddress(dlls[uiDll].hDll,
      PChar('function' + IntToStr(i)));
      if (@dlls[uiDll].functions[i] = nil) then
        @dlls[uiDll].functions[i] := getprocaddress(dlls[uiDll].hDll,
            PChar('_function' + IntToStr(i)+'@8'));
    until (@dlls[uiDll].functions[i] = nil);

    ConsoleWrite('ID: '+ inttostr(uiDll) + ' Found ' + inttostr(i - 1) + ' function(s)');

    if (Assigned(initFunc)) then
    begin
      try
        ConsoleWrite('ID: '+ inttostr(uiDll) + ' Calling SmartieInit');
        initFunc();
      except
        on E: Exception do
          raise Exception.Create('Legacy Plugin '+PluginPath+' had an exception during Init: '
            + E.Message);
      end;
    end;

    if (@minRefreshFunc <> nil) then
    begin
      try
        ConsoleWrite('ID: '+ inttostr(uiDll) + ' Calling GetMinRefreshInterval');
        minRefresh := minRefreshFunc();
      except
        on E: Exception do
          raise Exception.Create('Legacy Plugin '+PluginPath
            +' had an exception during GetMinRefreshInterval:' + E.Message);
      end;
      if (minRefresh > 0) then
        dlls[uiDll].uiMinRefreshInterval := minRefresh;
    end;

    LegacySharedMem^.LibraryResult := inttostr(dlls[uiDll].uiMinRefreshInterval);
    LegacySharedMem^.LibraryID := uiDll;
    setevent(hLegacyRecvEvent);
    ConsoleWrite('ID: '+ inttostr(uiDll) + ' Load Success');
  end
  else
  begin
    dec(uiTotalDlls);
    raise Exception.Create('Legacy Plugin '+ExtractFileName(PluginPath)+' had an exception during load: '
            + inttostr(err) + ' ' + SysErrorMessage(err));
  end;
end;

function LegacyPluginLoader.ExecuteFunction(ID: integer; iFunc: integer; sParam1: string; sparam2: string): string;
begin
  result := '';
  if iFunc <= iMaxPluginFuncs then
    try
      result := strpas(dlls[ID].functions[iFunc]( pchar(sParam1), pchar(sParam2)))
    except
      on E: Exception do
        raise Exception.Create('ID: '+inttostr(ID)+' Legacy Plugin '+dlls[ID].sName+' had an exception calling function: '
              + E.Message);
    end
  else if iFunc = finiFuncId then
  begin
    if assigned(dlls[ID].finiFunc) then
    try
      ConsoleWrite('ID: '+ inttostr(ID) + ' Calling SmartieFini');
      dlls[ID].finiFunc();
    except
      on E: Exception do
        raise Exception.Create('ID: '+inttostr(ID)+' Legacy Plugin '+dlls[ID].sName+' had an exception during closedown: '
              + E.Message);
    end
  end
  else if iFunc = infoFuncId then
  begin
    if assigned(dlls[ID].infoFunc) then
      try
        ConsoleWrite('ID: '+ inttostr(ID) + ' Calling SmartieInfo');
        Result := strpas(dlls[ID].infoFunc)
      except
      on E: Exception do
        raise Exception.Create('ID: '+inttostr(ID)+' Legacy Plugin '+dlls[ID].sName+' had an exception during infofunc: '
              + E.Message);
      end
    else
      result := '';
  end
  else if iFunc = demoFuncId then
  begin
    if assigned(dlls[ID].demoFunc) then
      try
        ConsoleWrite('ID: '+ inttostr(ID) + ' Calling SmartieDemo');
        Result := strpas(dlls[ID].demoFunc);
      except
        on E: Exception do
          raise Exception.Create('ID: '+inttostr(ID)+' Legacy Plugin '+dlls[ID].sName+' had an exception during demofunc: '
                + E.Message);
      end
    else
      Result := '';
  end;
end;

procedure LegacyPluginLoader.Timercheck(Sender: TObject);
var
  h: cardinal;
begin
  h := OpenEvent(EVENT_ALL_ACCESS, TRUE, Pchar('Local\LCDSmartieLegacyLoadLibraryRunning' + ShMemID));
  if h = 0 then
  begin
    ConsoleWrite('LCD Smartie has stopped');
    ConsoleWrite('Terminating...');
    timer1.Enabled := false;
    Halt; // terminate doesn't work when launched from smartie. Maybe the open handles prevents this
  end;
  closehandle(h);
end;

procedure LegacyPluginLoader.ConsoleWrite(S: String);
begin
  if IsConsole then
    writeln(DateTimeToStr(now, FormatSettings) + ' ' + S);
end;

constructor LegacyPluginLoader.Create(TheOwner: TComponent);
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);

  if (paramCount < 2) then
  begin
    ConsoleWrite('This Loader should only be run by LCD Smartie');
    Sleep(3000);
  end;

  inherited Create(TheOwner);
  StopOnException:=False;
  uiTotalDlls := 0;
  timer1 := tfpTimer.Create(timer1);
  timer1.OnTimer := Timercheck();
  timer1.UseTimerThread := true;
  timer1.Interval := 10;
  timer1.Enabled := true;
  ConsoleWrite('WARNING: DO NOT CLOSE THIS WINDOW - It will cause the main program to hang');
  ConsoleWrite('LCD Smartie LegacyLoader Started');
  ConsoleWrite('GUID: '+ ShMemID);
end;

destructor LegacyPluginLoader.Destroy;
begin
  inherited Destroy;
end;

{$R *.res}

begin
  console := false;
  if paramCount < 1 then
    console := true;

  if paramCount > 1 then begin
    ShMemID := ParamStr(1); // this is a guid passed from smartie so we're unique to that instance
    if strtoint(ParamStr(2)) = 1 then
    console := true;
  end;

  if console then
  begin
    AllocConsole;
    IsConsole := True;
    SysInitStdIO;
  end;

  Application:=LegacyPluginLoader.Create(nil);
  Application.Run;
  Application.Free;
end.

