{******************************************************************************
 *
 *  LCD Smartie - LCD control software.
 *  Copyright (C) 2000-2003  BassieP
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either version 2
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 *****************************************************************************}
unit UDataSystem;

//{$MODE Delphi}

interface

uses
  DataThread, Windows, SysUtils, Classes, Registry, shellapi, JwaWindows, jwatlhelp32, jwaWinBase, usmbios;

const
  // lets put a prefix key to make it clear what they are
  SysKey = '$Sys';
  UserNameKey = SysKey + 'Username';
  ComputerNameKey = SysKey + 'Computername';
  UpTimeKey = SysKey + 'Uptime';
  UpTimeShortKey = SysKey + 'Uptims';
  ScreensaverActiveKey = SysKey + 'SSActive';
  FullScreenGameActiveKey = SysKey + 'FSGameActive';
  FullScreenAppActiveKey = SysKey + 'FSAppActive';
  ApplicationActiveKey = SysKey + 'AppActive';
  CPUUseKey = SysKey + 'CPUUsage';
  CPUTypeKey = SysKey + 'CPUType';
  CPUSpeedMhzKey = SysKey + 'CPUSpeedMhz';
  CPUSpeedGhzKey = SysKey + 'CPUSpeedGhz';
  CPUCoreUsageKey = SysKey + 'CPUCoreUsage';
  CPUCoreSpeedKey = SysKey + 'CPUCoreSpeed';

  MemKey = '$Mem';
  MemFreeKey = MemKey + 'Free';
  MemUsedKey = MemKey + 'Used';
  MemTotalKey = MemKey + 'Total';
  MemFreePercentKey = MemKey + 'F%';
  MemUsedPercentKey = MemKey + 'U%';
  PageKey = '$Page';
  PageFreeKey = PageKey + 'Free';
  PageUsedKey = PageKey + 'Used';
  PageTotalKey = PageKey + 'Total';
  PageFreePercentKey = PageKey + 'F%';
  PageUsedPercentKey = PageKey + 'U%';

  alpha = 0.5;
  PDH_FMT_LONG   = $100;
  PDH_FMT_DOUBLE = $200;
  PDH_FMT_LARGE  = $400;
  PDH_MORE_DATA  = $FFFFFFFF800007D2;

type
  TPROCESSOR_POWER_INFORMATION = record
    Number: ULONG;
    MaxMhz: ULONG;
    CurrentMhz: ULONG;
    MhzLimit: ULONG;
    MaxIdleState: ULONG;
    CurrentIdleState: ULONG;
  end;

type
  // Should be variant but the case command used can only handle up to a 32 bit space in 32bit code
  // thats fine we can typecast to long or double
  PTPDH_FMT_COUNTERVALUE = ^TPDH_FMT_COUNTERVALUE;
  TPDH_FMT_COUNTERVALUE = record
    CStatus: DWORD ;
    LongValue: int64;
    //case integer of
    //  1: ( longValue: Int32; );
    //  2: ( doubleValue: double; );
    //  3: ( largeValue: Int64; );
    //   4: ( AnsiStringValue: PAnsiChar; );
    //  5: ( WideStringValue: PWideChar; );
  end;

type
  PTPDH_FMT_COUNTERVALUE_ITEM_A = ^TPDH_FMT_COUNTERVALUE_ITEM_A;
  TPDH_FMT_COUNTERVALUE_ITEM_A = record
    szName: PChar;
    PDH_FMT_COUNTERVALUE: TPDH_FMT_COUNTERVALUE;
  end;

type
  TCPUUsage = record
    name: string;
    value: long;
  end;
type
  TCPUPerformance = record
    name: string;
    value: long;
  end;

{$M+}
type
  TSystemDataThread = class(TDataThread)
  private
    STUsername, STComputername : String;
    STPageFree, STPageTotal: Int64;
    STMemFree, STMemTotal: Int64;
    iUptime: Int64;
    iLastUptime: Cardinal;
    uptimereg, uptimeregs: String;
    FullScreenGameActive: string;
    FullScreenAppActive: string;
    SnapTime: TDateTime;
    CPUUse:double;
    SMBios: tsmbios;
    CPUType: string;
    CPUSpeed: string;
    systeminfo: SYSTEM_INFO;
    PPIbuff: array of TPROCESSOR_POWER_INFORMATION;
    pdhQueryHandle: THANDLE;
    pdhCPUUsageCounterHandle: THANDLE;
    pdhCPUPerformanceCounterHandle: THANDLE;
    pdhCPUFrequencyCounterHandle: THANDLE;
    pdhCounterType: DWORD;
    pdhCounterItemsBuffer: array of TPDH_FMT_COUNTERVALUE_ITEM_A; // needs to be global
    pdhFmtCounterValue: TPDH_FMT_COUNTERVALUE; // needs to be global no time or patience to figure out why
    CPUUsageArray: array of TCPUUsage;
    CPUPerformanceArray: array of TCPUperformance;
    function getIsWin2kXP: Boolean;
    function getIsWin10: Boolean;
    function getTotalPhysMemory: Int64;
    function getAvailPhysMemory: Int64;
    function getTotalPageFile: Int64;
    function getAvailPageFile: Int64;
    function getUsername: String;
    function getComputername: String;
  protected
    function AllowRefresh : boolean; override;
    procedure  DoUpdate; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  ResolveVariables(var Line : string); override;
  published
    property bIsWin2kXP: Boolean read getIsWin2kXP;
    property bIsWin10: Boolean read getIsWin10;
    property totalPhysmemory: Int64 read gettotalphysmemory;
    property AvailPhysmemory: Int64 read getavailphysmemory;
    property totalPageFile: Int64 read gettotalPageFile;
    property AvailPageFile: Int64 read getAvailPageFile;
    property Username: String read getUsername;
    property Computername: String read getComputername;
    function isscreensaveractive: integer;
    function isfullscreengameactive: integer;
    function isfullscreenappactive: integer;
    function isapplicationactive(application: string): integer;
  end;


type
  PMemoryStatusEx = ^TMemoryStatusEx;
  LPMEMORYSTATUSEX = PMemoryStatusEx;
  {$EXTERNALSYM LPMEMORYSTATUSEX}
  _MEMORYSTATUSEX = packed Record
    dwLength : DWORD;
    dwMemoryLoad : DWORD;
    ullTotalPhys : Int64;
    ullAvailPhys : Int64;
    ullTotalPageFile: Int64;
    ullAvailPageFile: Int64;
    ullTotalVirtual : Int64;
    ullAvailVirtual : Int64;
    ullAvailExtenededVirtual : Int64;
  end;
  {$EXTERNALSYM _MEMORYSTATUSEX}
  TMemoryStatusEx = _MEMORYSTATUSEX;
  MEMORYSTATUSEX = _MEMORYSTATUSEX;
  {$EXTERNALSYM MEMORYSTATUSEX}

var
  ProcList: TStringList;
  FLastIdleTime: Int64;
  FLastKernelTime: Int64;
  FLastUserTime: Int64;
implementation

uses
  UUtils, StrUtils;

function PdhOpenQueryA( szDataSource : PAnsiChar; dwUserData : PDWORD; phQuery: pointer ) : HRESULT; stdcall; external 'pdh' name 'PdhOpenQueryA';
function PdhCloseQuery( hQuery: THANDLE ) : HRESULT; stdcall; external 'pdh' name 'PdhCloseQuery';
function PdhAddEnglishCounterA( hQuery : THANDLE; szFullCounterPath : PAnsiChar; dwUserData: PDWORD; phCounter : pointer ) : HRESULT; stdcall; external 'pdh' name 'PdhAddCounterA';
function PdhCollectQueryData( hQuery : THANDLE ) : HRESULT; stdcall; external 'pdh' name 'PdhCollectQueryData';
function PdhGetFormattedCounterValue( hCounter : THANDLE; dwFormat : DWORD; lpdwType: pointer; pValue: pointer ) : HRESULT; stdcall; external 'pdh' name 'PdhGetFormattedCounterValue';
function PdhGetFormattedCounterArrayA( hCounter : THANDLE; dwFormat : DWORD; lpdwBufferSize: PDWORD; lpdwItemCount: PDWORD; ItemBuffer : PTPDH_FMT_COUNTERVALUE_ITEM_A) : HRESULT; stdcall; external 'pdh' name 'PdhGetFormattedCounterArrayA';

function SHQueryUserNotificationState( p : Pointer ) : HRESULT; stdcall; external shell32 name 'SHQueryUserNotificationState';

constructor TSystemDataThread.Create;
var
  lprocessorinfo:TProcessorInformation;
  pdhRet: HRESULT;
begin
  STComputername := Computername;
  STUsername := Username;
  ProcList := TStringList.Create;

  Smbios := tsmbios.create;
    for LProcessorInfo in SMBios.ProcessorInfo do
      CPUType := LProcessorInfo.ProcessorVersionStr;
  CPUSpeed := '0';
  setLength(PPIbuff, ProcessorCount);

  if ( PdhOpenQueryA(nil, nil, @pdhQueryHandle) >= 0 ) then
  begin
    if getIsWin10 then
    begin
      pdhRet := PdhAddEnglishCounterA(pdhQueryHandle, pchar('\Processor Information(*)\% Processor Utility'), nil, @pdhCPUUsageCounterHandle);
      pdhRet := PdhAddEnglishCounterA(pdhQueryHandle, pchar('\Processor Information(*)\% Processor Performance'), nil, @pdhCPUPerformanceCounterHandle);
      pdhRet := PdhAddEnglishCounterA(pdhQueryHandle, pchar('\Processor Information(*)\Processor Frequency'), nil, @pdhCPUFrequencyCounterHandle);
    end
    else
    begin
      pdhRet := PdhAddEnglishCounterA(pdhQueryHandle, pchar('\Processor Information(*)\% Processor Time'), nil, @pdhCPUUsageCounterHandle);
      pdhRet := PdhAddEnglishCounterA(pdhQueryHandle, pchar('\Processor Performance(*)\frequency'), nil, @pdhCPUPerformanceCounterHandle);
    end;
  end;

  inherited Create(100);
end;

destructor TSystemDataThread.Destroy;
begin
  ProcList.Free;
  Proclist := nil;
  PdhCloseQuery(pdhQueryHandle);
  inherited;
end;

function TSystemDataThread.AllowRefresh : boolean;
begin
  Result := true;
end;

function GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx): BOOL; stdcall;
type
  TFNGlobalMemoryStatusEx = function(var msx: TMemoryStatusEx): BOOL;
  stdcall;
var
  FNGlobalMemoryStatusEx: TFNGlobalMemoryStatusEx;
begin
  lpBuffer.dwLength := SizeOf(TMemoryStatusEx);

  FNGlobalMemoryStatusEx := TFNGlobalMemoryStatusEx(
    GetProcAddress(GetModuleHandle(kernel32), 'GlobalMemoryStatusEx'));
  if not Assigned(FNGlobalMemoryStatusEx) then
  begin
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
    Result := False;
  end
  else
  begin
    Result := FNGlobalMemoryStatusEx(lpBuffer);
  end;
end;

// TODO: This may not be needed anymore as alot has changed in the years from
// when it was needed. Many new features of LCDSmartie require at least Vista
// maybe even 7
function TSystemDataThread.getIsWin2kXP: Boolean;
var
  oviVersionInfo: windows.TOSVERSIONINFO;
begin
  oviVersionInfo.dwOSVersionInfoSize := SizeOf(oviVersionInfo);
  if not windows.GetVersionEx(oviVersionInfo) then raise
    Exception.Create('Can''t get the Windows version');
  if (oviVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT) and
    (oviVersionInfo.dwMajorVersion >= 5) then getIsWin2kXP := true
  else getIsWin2kXP := false;
end;

function TSystemDataThread.getIsWin10: Boolean;
var
  oviVersionInfo: windows.TOSVERSIONINFO;
begin
  oviVersionInfo.dwOSVersionInfoSize := SizeOf(oviVersionInfo);
  if not windows.GetVersionEx(oviVersionInfo) then raise
    Exception.Create('Can''t get the Windows version');
  if (oviVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT) and
    (oviVersionInfo.dwMajorVersion >= 10) then getIsWin10 := true
  else getIsWin10 := false;
end;

function TSystemDataThread.gettotalphysmemory: Int64;
var
  memory: TMemoryStatus;
  memoryEx: TMemoryStatusEx;
begin
  if getIsWin2kXP then
  begin
    memoryEx.dwLength := sizeof(TMemoryStatusEx);
    if GlobalMemoryStatusEx(memoryEx) then gettotalphysmemory :=
      memoryEx.ulltotalphys
    else gettotalphysmemory := 0;
  end
  else
  begin
    memory.dwLength := sizeof(TMemoryStatus);
    GlobalMemoryStatus(memory);
    gettotalphysmemory := memory.dwtotalphys;
  end;
end;

function TSystemDataThread.getavailphysmemory: Int64;
var
  memory: TMemoryStatus;
  memoryEx: TMemoryStatusEx;
begin
  if getIsWin2kXP then
  begin
    memoryEx.dwLength := sizeof(TMemoryStatusEx);
    if GlobalMemoryStatusEx(memoryEx) then getavailphysmemory :=
      memoryEx.ullavailphys
    else getavailphysmemory := 0;
  end
  else
  begin
    memory.dwLength := sizeof(TMemoryStatus);
    GlobalMemoryStatus(memory);
    getavailphysmemory := memory.dwavailphys;
  end;
end;

function TSystemDataThread.gettotalpagefile: Int64;
var
  memory: TMemoryStatus;
  memoryEx: TMemoryStatusEx;
begin
  if getIsWin2kXP then
  begin
    memoryEx.dwLength := sizeof(TMemoryStatusEx);
    if GlobalMemoryStatusEx(memoryEx) then gettotalpagefile :=
      memoryEx.ulltotalpagefile
    else gettotalpagefile := 0;
  end
  else
  begin
    memory.dwLength := sizeof(TMemoryStatus);
    GlobalMemoryStatus(memory);
    gettotalpagefile := memory.dwtotalpagefile;
  end;
end;

function TSystemDataThread.getavailpagefile: Int64;
var
  memory: TMemoryStatus;
  memoryEx: TMemoryStatusEx;
begin
  if getIsWin2kXP then
  begin
    memoryEx.dwLength := sizeof(TMemoryStatusEx);
    if GlobalMemoryStatusEx(memoryEx) then getavailpagefile :=
      memoryEx.ullavailpagefile
    else getavailpagefile := 0;
  end
  else
  begin
    memory.dwLength := sizeof(TMemoryStatus);
    GlobalMemoryStatus(memory);
    getavailpagefile := memory.dwavailpagefile;
  end;
end;

function TSystemDataThread.getusername: String;
var
  p: Pchar;
  size: Dword;
begin
  size := 1024;
  p := stralloc(size);
  windows.getusername(p, size);
  getusername := p;
  strdispose(p);
end;

function TSystemDataThread.getcomputername: String;
var
  p: Pchar;
  size: Dword;
begin
  size := MAX_COMPUTERNAME_LENGTH + 1;
  p := stralloc(size);
  windows.getcomputername(p, size);
  getcomputername := p;
  strdispose(p);
end;

function processExists(exeFileName: string): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to ProcList.Count -1 do
    if ProcList[i] = UpperCase(ExeFileName) then
      Result := True;
end;

function TSystemDataThread.isscreensaveractive: integer;
var
  Reg: Tregistry;
  SSStr: String;
begin
  reg := TRegistry.Create ;
  Reg.RootKey := HKEY_CURRENT_USER;
  Reg.OpenKey('Control Panel\Desktop',False);
  SSStr := ExtractFileName(Reg.ReadString('SCRNSAVE.EXE'));
  If processExists(SSStr) then
    result:= 1
  else
    result := 0;
  Reg.Free;
end;


function TSystemDataThread.isfullscreengameactive: integer;
var
 i : LongInt;
begin
 result := -1;
 if (SHQueryUserNotificationState(@i) = S_OK) then
   if (i = 3)  then // d3d full screen
     result := 1
   else
     result := 0;
end;

function TSystemDataThread.isfullscreenappactive: integer;
var
 i : LongInt;
begin
  result := -1;
  if (SHQueryUserNotificationState(@i) = S_OK) then
    if (i = 2)  then // non d3d full screen
      result := 1
    else
      result := 0;
end;

function DateTimeToMilliseconds(aDateTime: TDateTime): Int64;
var
 TimeStamp: TTimeStamp;
begin
  TimeStamp:= DateTimeToTimeStamp (aDateTime);
  Result:= TimeStamp.Time;
end;

function TSystemDataThread.isapplicationactive(application: string): integer;
begin
  if processExists(application) then
    result :=1
  else
    result :=0;
end;

procedure TSystemDataThread.DoUpdate;
var
  t: longword;
  y, mo, d, h, m, s : Cardinal;
  uiRemaining: Cardinal;
  sTempUptime: String;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  i: integer;
  NextCPUUse: Int64;
  pdhItemCount: dword;
  pdhBufferSize: dword;
  ret: HRESULT;
begin
  if (not Terminated) then begin
    fDataLock.Enter;
    try
      STMemfree := availPhysmemory div (1024 * 1024);
      STMemTotal := totalPhysmemory div (1024 * 1024);
    finally
      fDataLock.Leave;
    end;
  end;
  
  if (not Terminated) then begin
    fDataLock.Enter;
    try
      STPageTotal := totalPageFile div (1024 * 1024);
      STPageFree := AvailPageFile div (1024 * 1024);
    finally
      fDataLock.Leave;
    end;
  end;

  //uptime!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  t := GetTickCount64;
  if (t < iLastUptime) then iUptime := iUptime + t + (MAXDWORD-iLastUptime)
  else iUptime := iUptime + (t - iLastUptime);
  iLastUptime := t;

  y :=  iUptime div ticksperyear;
  mo := (iUptime div tickspermonth) mod 12;
  d := (iUptime div ticksperdag) mod 30;
  h := (iUptime div ticksperhour) mod 24;
  m := (iUptime div ticksperminute) mod 60;
  s := (iUptime div ticksperseconde) mod 60;

  sTempUptime := '';
  if (y > 0) or (sTempUptime<>'') then
    sTempUptime := sTempUptime + IntToStr(y) +  'yrs ';
  if (mo > 0) or (sTempUptime<>'') then
    sTempUptime := sTempUptime + IntToStr(mo) +  'mts ';
  if (d > 0) or (sTempUptime<>'') then
    sTempUptime := sTempUptime + IntToStr(d) +  'dys ';
  if (h > 0) or (sTempUptime<>'') then
    sTempUptime := sTempUptime + IntToStr(h) +  'hrs ';
  if (m > 0) or (sTempUptime<>'') then
    sTempUptime := sTempUptime + IntToStr(m) +  'min ';
  sTempUptime := sTempUptime + Format('%.2d',[s], localeFormat) + 'secs';
  fDataLock.Enter();
  uptimereg := sTempUptime;
  fDataLock.Leave();

  // Create the short uptime string
  // Display the three largest units, i.e. '15d 7h 12m' or '7h 12m 2s'
  sTempUptime := '';
  uiRemaining := 0;
  if (y>0) and (sTempUptime='') then uiRemaining := 3;
  if (uiRemaining > 0) then
  begin
    Dec(uiRemaining);
    sTempUptime := sTempUptime + IntToStr(y) +'y ';
  end;

  if (mo>0) and (sTempUptime='') then uiRemaining := 3;
  if (uiRemaining > 0) then
  begin
    Dec(uiRemaining);
    sTempUptime := sTempUptime + IntToStr(mo) +'m ';
  end;

  if (d>0) and (sTempUptime='') then uiRemaining := 3;
  if (uiRemaining > 0) then
  begin
    Dec(uiRemaining);
    sTempUptime := sTempUptime + IntToStr(d) +'d ';
  end;

  if (h>0) and (sTempUptime='') then uiRemaining := 3;
  if (uiRemaining > 0) then
  begin
    Dec(uiRemaining);
    sTempUptime := sTempUptime + IntToStr(h) +'h ';
  end;

  if (m>0) and (sTempUptime='') then uiRemaining := 3;
  if (uiRemaining > 0) then
  begin
    Dec(uiRemaining);
    sTempUptime := sTempUptime + IntToStr(m) +'m ';
  end;

  if (sTempUptime='') or (uiRemaining > 0) then
  begin
    sTempUptime := sTempUptime + Format('%.2d', [s], localeFormat) +'s ';
  end;

  // remove the trailing space and assign to class member
  fDataLock.Enter;
  uptimeregs := MidStr(sTempUptime, 1, Length(sTempUptime)-1);
  fDataLock.Leave;

// crap for detecting fulscreen applications
// just leave this here should it be needed
//typedef enum  {
//  QUNS_NOT_PRESENT              = 1,
//  QUNS_BUSY                     = 2,
//  QUNS_RUNNING_D3D_FULL_SCREEN  = 3,
//  QUNS_PRESENTATION_MODE        = 4,
//  QUNS_ACCEPTS_NOTIFICATIONS    = 5,
//  QUNS_QUIET_TIME               = 6,
//  QUNS_APP                      = 7
//} QUERY_USER_NOTIFICATION_STATE;
  if (SHQueryUserNotificationState(@i) = S_OK) then
    case (i) of
      2: begin // non d3d full screen
           FullScreenAppActive := '1';
           FullScreenGameActive := '0';
         end;
      3: begin // d3d full screen
           FullScreenAppActive := '0';
           FullScreenGameActive := '1';
         end;
      else
        FullScreenAppActive := '0';
        FullScreenGameActive := '0';
    end;

  // this is a hog. better to run in the refresh proc and limit it to every 300ms or more
  // executing CreateToolhelp32Snapshot(), Process32First() and Process32Next() everytime
  // actions call through here cause massive cpu usage and nearly 30ms delay. Possibly more
  // depending on the type and quantity of actions called
  if DateTimeToMilliseconds(Now() - SnapTime) >= 300 then
  begin
    fDataLock.Enter;
    SnapTime := Now();
    FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
    FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if  Process32First(FSnapshotHandle, FProcessEntry32)then
    begin
      ProcList.Clear;
      repeat
        ProcList.Add(UpperCase(ExtractFileName(FProcessEntry32.szExeFile)));
      until not Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
    closehandle(FSnapshotHandle);
    fDataLock.Leave;
  end;

  fDataLock.Enter;
  PdhCollectQueryData(pdhQueryHandle);

  pdhBufferSize := 0;
  ret := PdhGetFormattedCounterArrayA(pdhCPUUsageCounterHandle, PDH_FMT_LONG, @pdhBufferSize, @pdhItemCount, nil);
  if ret = PDH_MORE_DATA then
  begin
    SetLength(pdhCounterItemsBuffer, pdhItemCount);
    ret := PdhGetFormattedCounterArrayA(pdhCPUUsageCounterHandle, PDH_FMT_LONG, @pdhBufferSize, @pdhItemCount, @pdhCounterItemsBuffer[0]);
  end;

  if pdhItemCount > 0 then
  begin
    SetLength(CPUUsageArray, pdhItemCount);
    for i := 0 to pdhItemCount -1 do
    begin
      CPUUsageArray[i].name := strpas(pdhCounterItemsBuffer[i].szName);
      CPUUsageArray[i].value := int32(pdhCounterItemsBuffer[i].PDH_FMT_COUNTERVALUE.longValue);
    end;
    NextCPUUse := int32(pdhCounterItemsBuffer[pdhItemCount - 1].PDH_FMT_COUNTERVALUE.longValue);
  end;
  pdhItemCount := 0;
  pdhBufferSize := 0;
  ret := PdhGetFormattedCounterArrayA(pdhCPUPerformanceCounterHandle, PDH_FMT_LONG, @pdhBufferSize, @pdhItemCount, nil);
  if ret = PDH_MORE_DATA then
  begin
    SetLength(pdhCounterItemsBuffer, pdhItemCount);
    ret := PdhGetFormattedCounterArrayA(pdhCPUPerformanceCounterHandle, PDH_FMT_LONG, @pdhBufferSize, @pdhItemCount, @pdhCounterItemsBuffer[0]);
  end;

  if getIsWin10 then
  begin
    if pdhItemCount > 0 then
    begin
      SetLength(CPUPerformanceArray, pdhItemCount);
      for i := 0 to pdhItemCount -1 do
      begin
        ret := PdhGetFormattedCounterValue(pdhCPUFrequencyCounterHandle, PDH_FMT_LONG, @pdhCounterType, @pdhFmtCounterValue);
        CPUPerformanceArray[i].name := strpas(pdhCounterItemsBuffer[i].szName);
        CPUPerformanceArray[i].value := round((int32(pdhFmtCounterValue.longValue) / 100) * int32(pdhCounterItemsBuffer[i].PDH_FMT_COUNTERVALUE.longValue));
      end;
      CPUSpeed := inttostr(CPUPerformanceArray[pdhItemCount - 1].value);
    end;
  end
  else
  begin
    if pdhItemCount > 0 then
    begin
      SetLength(CPUPerformanceArray, pdhItemCount);
      for i := 0 to pdhItemCount -1 do
      begin
        CPUPerformanceArray[i].name := strpas(pdhCounterItemsBuffer[i].szName);
        CPUPerformanceArray[i].value := int32(pdhCounterItemsBuffer[i].PDH_FMT_COUNTERVALUE.longValue);
      end;
      CPUSpeed := inttostr(CPUPerformanceArray[pdhItemCount - 1].value);
    end;
  end;

  CPUUse := NextCPUUse;
  fDataLock.Leave;
end;

procedure TSystemDataThread.ResolveVariables(var Line : string);
var
  mem: Int64;
  args: Array [1..maxArgs] of String;
  prefix, postfix: String;
  numArgs: Cardinal;
  arg: string;
  i: integer;
  cpuname: string;
  cpuval: string;
begin

  fDataLock.Enter();
  if (pos(SysKey,Line) > 0) then
  begin
    while decodeArgs(line, ApplicationActiveKey, maxArgs, args, prefix, postfix, numargs) do
    begin
      Line := prefix;
      Line := Line + inttostr(isapplicationactive(args[1])) + postfix;
    end;
  end;
  fDataLock.Leave();
  fDataLock.Enter();
  if (pos(SysKey,Line) > 0) then
  begin
    while decodeArgs(line, CPUCoreUsageKey, maxArgs, args, prefix, postfix, numargs) do
    begin
      cpuval := '0';
      try
        RequiredParameters(numargs, 1, 2);
        if numargs = 2 then
          cpuname := stripspaces(args[1])+','+stripspaces(args[2])
        else if numargs = 1 then
          cpuname := stripspaces(args[1])
        else
          raise Exception.Create('Bad parameters');

        for i := 0 to length(CPUUsageArray) - 1  do
          if CPUUsageArray[i].name = cpuname then
            cpuval := inttostr(CPUUsageArray[i].value);

        Line := prefix + cpuval + postfix
      except
        on E: Exception do line := prefix + '[SysCPUCoreusage: '
        + CleanString(E.Message) + ']' + postfix;
      end;
    end;
  end;

  fDataLock.Leave();
  fDataLock.Enter();
  if (pos(SysKey,Line) > 0) then
  begin
    while decodeArgs(line, CPUCoreSpeedKey, maxArgs, args, prefix, postfix, numargs) do
    begin
      cpuval := '0';
      try
        RequiredParameters(numargs, 1, 2);
        if numargs = 2 then
          cpuname := stripspaces(args[1])+','+stripspaces(args[2])
        else if numargs = 1 then
          cpuname := stripspaces(args[1])
        else
          raise Exception.Create('Bad parameters');

        for i := 0 to length(CPUPerformanceArray) - 1  do
          if CPUPerformanceArray[i].name = cpuname then
            cpuval := inttostr(CPUPerformanceArray[i].value);

        Line := prefix + cpuval + postfix
      except
        on E: Exception do line := prefix + '[SysCPUCoreSpeed: '
        + CleanString(E.Message) + ']' + postfix;
      end;
    end;
   end;
  fDataLock.Leave();
  fDataLock.Enter();
  if (pos(SysKey,Line) > 0) then
  begin
    Line := StringReplace(line, CPUTypeKey, CPUType, [rfReplaceAll]);
    Line := StringReplace(line, CPUUseKey, Format('%.0f', [CPUUse]), [rfReplaceAll]);
    Line := StringReplace(line, CPUSpeedMhzKey, CPUSpeed, [rfReplaceAll]);
    Line := StringReplace(line, CPUSpeedGhzKey, Format('%.2f', [strtoint(CPUSpeed) / 1000]) , [rfReplaceAll]);
  end;
  fDataLock.Leave();
  fDataLock.Enter();
  if (pos(SysKey,Line) > 0) then
  begin
    Line := StringReplace(line, UserNameKey, STUsername, [rfReplaceAll]);
    Line := StringReplace(line, ComputerNameKey, STcomputername, [rfReplaceAll]);
    line := StringReplace(line, UpTimeKey, uptimereg, [rfReplaceAll]);
    line := StringReplace(line, UpTimeShortKey, uptimeregs, [rfReplaceAll]);
  end;
  fDataLock.Leave();
  fDataLock.Enter();
  if (pos(SysKey,Line) > 0) then
  begin
    Line := StringReplace(line, ScreensaverActiveKey, inttostr(isscreensaveractive), [rfReplaceAll]);
    Line := StringReplace(line, FullScreenGameActiveKey, FullScreenGameActive, [rfReplaceAll]);
    Line := StringReplace(line, FullScreenAppActiveKey, FullScreenAppActive, [rfReplaceAll]);
  end;
  fDataLock.Leave();

  if (pos(MemKey,Line) > 0) then begin
    fDataLock.Enter;
    try
      line := StringReplace(line, MemFreeKey, IntToStr(STMemFree), [rfReplaceAll]);
      line := StringReplace(line, MemUsedKey, IntToStr(STMemTotal-STMemFree), [rfReplaceAll]);
      line := StringReplace(line, MemTotalKey, IntToStr(STMemTotal), [rfReplaceAll]);
      if pos(MemFreePercentKey, line) <> 0 then
      begin
        if (STMemTotal > 0) then mem := round(100/STMemTotal*STMemfree)
        else mem := 0;
        line := StringReplace(line, MemFreePercentKey, IntToStr(mem), [rfReplaceAll]);
      end;
      if pos(MemUsedPercentKey, line) <> 0 then
      begin
        if (STMemTotal > 0) then mem := round(100/STMemTotal*(STMemTotal-STMemfree))
        else mem := 0;
        line := StringReplace(line, MemUsedPercentKey, IntToStr(mem), [rfReplaceAll]);
      end;
    finally
      fDataLock.Leave;
    end;
  end;

  if (pos(PageKey,Line) > 0) then begin
    fDataLock.Enter;
    try
      line := StringReplace(line, PageFreeKey, IntToStr(STPageFree), [rfReplaceAll]);
      line := StringReplace(line, PageUsedKey, IntToStr(STPageTotal-STPageFree), [rfReplaceAll]);
      line := StringReplace(line, PageTotalKey, IntToStr(STPageTotal), [rfReplaceAll]);
      if pos(PageFreePercentKey, line) <> 0 then
      begin
        if (STPageTotal > 0) then mem := round(100/STPageTotal*STPagefree)
        else mem := 0;
        line := StringReplace(line, PageFreePercentKey, IntToStr(mem), [rfReplaceAll]);
      end;

      if pos(PageUsedPercentKey, line) <> 0 then
      begin
        if (STPageTotal > 0) then mem := round(100/STPageTotal*(STPageTotal-STPagefree))
        else mem := 0;
        line := StringReplace(line, PageUsedPercentKey, IntToStr(mem), [rfReplaceAll]);
      end;
    finally
      fDataLock.Leave;
    end;
  end;
end;

end.
