program LegacyLoader;

uses
  Classes, SysUtils, CustApp,  Windows, fpTimer;

const
  iMaxPluginFuncs = 20;

type
    TSharedMem=record
    LibraryID: Integer;
    LibraryPath: Array[1..255] of char;
    LibraryFunc: Integer;
    LibraryParam1: Array[1..2048] of char;
    LibraryParam2: Array[1..2048] of char;
    LibraryResult: Array[1..2048] of char;
  end;

  TMyProc = function(param1: pchar; param2: pchar): Pchar; stdcall;
  TFiniProc = procedure(); stdcall;

  TDll = Record
    sName: String;
    hDll: HMODULE;
//    bBridge: Boolean;  // leave 32bit .net for now
//    iBridgeId: Integer;  // as above
    functions: Array [1..iMaxPluginFuncs] of TMyProc;
//    bridgeFunc: TBridgeProc;
    finiFunc: TFiniProc;
    uiMinRefreshInterval: Cardinal;
  end;

  { LegacyPluginLoader }

  LegacyPluginLoader = class(TCustomApplication)
  private
    dlls: Array of TDll;
    uiTotalDlls: Cardinal;
    procedure LoadPlugin(PluginPath: string);
    procedure ExecuteFunction(ID: integer; iFunc: integer; sParam1: string; sparam2: string);
    procedure Timercheck(Sender: TObject);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

   PTSharedMem=^TSharedMem;

var
  hMap: THandle;
  LegacySharedMem: PTSharedMem;
  hLegacyLoadLibraryEvent: THandle;
  hLegacyCallFunctionEvent: THandle;
  hLegacyRecvEvent: THandle;
  waitresult:dword;
  HandleArray: array[0..1] of THandle;
  timer1: TFPTimer;
  ShMemID: string;

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
    hLegacyLoadLibraryEvent := OpenEvent(EVENT_ALL_ACCESS, TRUE, Pchar('Local\LCDSmartieLegacyLoadLibraryEvent' + ShMemID));
  if hLegacyLoadLibraryEvent = 0 then
    terminate;

  if  not ( hLegacyCallFunctionEvent > 0) then
    hLegacyCallFunctionEvent := OpenEvent(EVENT_ALL_ACCESS, TRUE, Pchar('Local\LCDSmartieLegacyCallFunctionEvent' + ShMemID));
  if hLegacyCallFunctionEvent = 0 then
    terminate;

  if  not ( hLegacyRecvEvent > 0) then
    hLegacyRecvEvent := OpenEvent(EVENT_ALL_ACCESS, TRUE, Pchar('Local\LCDSmartieLegacyRecvEvent' + ShMemID));
  if hLegacyRecvEvent = 0 then
    terminate;

  HandleArray[0] := hLegacyLoadLibraryEvent;
  HandleArray[1] := hLegacyCallFunctionEvent;
  waitresult := WaitForMultipleObjects( 2, PWOHandleArray(@HandleArray), FALSE, INFINITE);
  case (waitresult) of
      0 : begin
            try
              LoadPlugin(LegacySharedMem^.LibraryPath);
            except
              on E: Exception do
              begin
                LegacySharedMem^.LibraryResult := e.Message;
                setevent(hLegacyRecvEvent);
              end;
            end;
          end;
      1 : begin
            try
              ExecuteFunction( LegacySharedMem^.LibraryID, LegacySharedMem^.LibraryFunc, LegacySharedMem^.LibraryParam1, LegacySharedMem^.LibraryParam2);
            except
              on E: Exception do
              begin
                LegacySharedMem^.LibraryResult := e.Message;
                setevent(hLegacyRecvEvent);
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
  // Yeah, I just ripped most of this from smartie itself
  inc(uiTotalDlls);
  uiDll := uiTotalDlls - 1;
  SetLength(dlls, uiTotalDlls);
  dlls[uiDll].uiMinRefreshInterval := 300;
  dlls[uiDll].sName := PluginPath;
  dlls[uiDll].hDll := LoadLibrary(pchar(PluginPath));
  err := GetLastError();
  if (dlls[uiDll].hDll <> 0) then
  begin
    initFunc := getprocaddress(dlls[uiDll].hDll, PChar('SmartieInit'));
    if (not Assigned(initFunc)) then
      initFunc := getprocaddress(dlls[uiDll].hDll, PChar('_SmartieInit@0'));

    dlls[uiDll].finiFunc := getprocaddress(dlls[uiDll].hDll, PChar('SmartieFini'));
    if (not Assigned(dlls[uiDll].finiFunc)) then
      dlls[uiDll].finiFunc := getprocaddress(dlls[uiDll].hDll, PChar('_SmartieFini@0'));

    if (Assigned(initFunc)) then
    begin
      try
        initFunc();
      except
        on E: Exception do
          raise Exception.Create('Plugin '+PluginPath+' had an exception during Init: '
            + E.Message);
      end;
    end;

    for i:= 1 to iMaxPluginFuncs do
      begin
        @dlls[uiDll].functions[i] := getprocaddress(dlls[uiDll].hDll,
          PChar('function' + IntToStr(i)));
        if (@dlls[uiDll].functions[i] = nil) then
          @dlls[uiDll].functions[i] := getprocaddress(dlls[uiDll].hDll,
            PChar('_function' + IntToStr(i)+'@8'));
        if (@dlls[uiDll].functions[i] <> nil) then
      end;

    minRefreshFunc := getprocaddress(dlls[uiDll].hDll, PChar('GetMinRefreshInterval'));
      if (not Assigned(minRefreshFunc)) then
        minRefreshFunc := getprocaddress(dlls[uiDll].hDll, PChar('_GetMinRefreshInterval@0'));

      if (@minRefreshFunc <> nil) then
      begin
        try
          minRefresh := minRefreshFunc();
        except
          on E: Exception do
            raise Exception.Create('Plugin '+PluginPath
              +' had an exception during GetMinRefreshInterval:' + E.Message);
        end;
        if (minRefresh > 0) then
          dlls[uiDll].uiMinRefreshInterval := minRefresh;
      end;

    LegacySharedMem^.LibraryResult := inttostr(dlls[uiDll].uiMinRefreshInterval);
    LegacySharedMem^.LibraryID := uiDll;
    setevent(hLegacyRecvEvent);

  end
  else
  begin
    raise Exception.Create('Plugin '+PluginPath+' had an exception during load: '
            + inttostr(err));
    dec(uiTotalDlls);
  end;
end;

procedure LegacyPluginLoader.ExecuteFunction(ID: integer; iFunc: integer; sParam1: string; sparam2: string);
begin
  if iFunc <= iMaxPluginFuncs then
    LegacySharedMem^.LibraryResult := strpas(dlls[ID].functions[iFunc]( pchar(sParam1), pchar(sParam2)))
  else
    try
      dlls[ID].finiFunc();
    except
      on E: Exception do
        raise Exception.Create('Plugin '+dlls[ID].sName+' had an exception during closedown: '
              + E.Message);
    end;

  setevent(hLegacyRecvEvent);
end;

procedure LegacyPluginLoader.Timercheck(Sender: TObject);
var
  h: cardinal;
begin
  h := OpenEvent(EVENT_ALL_ACCESS, TRUE, Pchar('Local\LCDSmartieLegacyLoadLibraryRunning' + ShMemID));
  if h = 0 then
  begin
    timer1.Enabled := false;
    Halt; // terminate doesn't work when launched from smartie. Maybe the open handles prevents this
  end;
  closehandle(h);
end;

constructor LegacyPluginLoader.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=False;
  uiTotalDlls := 1;
  timer1 := tfpTimer.Create(timer1);
  timer1.OnTimer := Timercheck();
  timer1.UseTimerThread := true;
  timer1.Interval := 10;
  timer1.Enabled := true;
  ShMemID := ParamStr(1); // this is a guid passed from smartie so we're unique to that instance
end;

destructor LegacyPluginLoader.Destroy;
begin
  inherited Destroy;
end;

var
  Application: LegacyPluginLoader;
begin
  Application:=LegacyPluginLoader.Create(nil);
  Application.Run;
  Application.Free;
end.

