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
  DataThread, Windows, SysUtils, Classes, Registry, shellapi, JwaWindows, jwatlhelp32, jwaWinBase, usmbios, UMain;

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
    LargeValue: int64;
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
  PTPDH_COUNTER_PATH_ELEMENTS_W = ^TPDH_COUNTER_PATH_ELEMENTS_W;
  TPDH_COUNTER_PATH_ELEMENTS_W =record
    szMachineName: LPSTR;
    szObjectName: LPSTR;
    szInstanceName: LPSTR;
    szParentInstance: LPSTR;
    dwInstanceIndex: DWORD;
    szCounterName: LPSTR;
  end;

type
  PTPDH_COUNTER_INFO_W = ^TPDH_COUNTER_INFO_W;
  TPDH_COUNTER_INFO_W = record
    dwLength: DWORD;
    dwType: DWORD;
    CVersion: DWORD;
    CStatus: DWORD;
    lScale: LONG;
    lDefaultScale: LONG;
    dwUserData: PDWORD;
    dwQueryUserData: PDWORD;
    szFullPath: PWideChar;
    PDH_COUNTER_PATH_ELEMENTS_W: TPDH_COUNTER_PATH_ELEMENTS_W;
    szExplainText:  PWideChar;
    //DataBuffer[1]:  DWORD;
  end;


type
  TCPUUsage = record
    name: string;
    value: double;
  end;
type
  TCPUPerformance = record
    name: string;
    value: double;
  end;

{$M+}
type
  TSystemDataThread = class(TDataThread)
  private
    STUsername, STComputername : String;
    STPageFree, STPageTotal: Int64;
    STMemFree, STMemTotal: Int64;
    iUptime: Int64;
    iLastUptime: QWORD;
    uptimereg, uptimeregs: String;
    FullScreenGameActive: string;
    FullScreenAppActive: string;
    SnapTime: TDateTime;
    CPUUse:double;
    SMBios: tsmbios;
    CPUType: string;
    CPUSpeed: double;
    systeminfo: SYSTEM_INFO;
    PPIbuff: array of TPROCESSOR_POWER_INFORMATION;
    pdhQueryHandle: THANDLE;
    pdhCPUUsageCounterHandle: THANDLE;
    pdhCPUPerformanceCounterHandle: THANDLE;
    pdhCPUFrequencyCounterHandle: THANDLE;
    CPUUsageArray: array of TCPUUsage;
    CPUPerformanceArray: array of TCPUperformance;
    function getIsWin2kXP: Boolean;
    function getIsWin10: Boolean;
    function getIsWin11: Boolean;
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
    property bIsWin11: Boolean read getIsWin11;
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

function PdhOpenQueryW( szDataSource : PAnsiChar; dwUserData : PDWORD; phQuery: pointer ) : HRESULT; stdcall; external 'pdh' name 'PdhOpenQueryW';
function PdhCloseQuery( hQuery: THANDLE ) : HRESULT; stdcall; external 'pdh' name 'PdhCloseQuery';
function PdhAddEnglishCounterW( hQuery : THANDLE; szFullCounterPath : PWideChar; dwUserData: PDWORD; phCounter : pointer ) : HRESULT; stdcall; external 'pdh' name 'PdhAddEnglishCounterW';
function PdhGetCounterInfoW( hCounter : THANDLE; bRetrieveExplainText : boolean; lpdwBufferSize: PDWORD; lpBuffer : pointer ) : HRESULT; stdcall; external 'pdh' name 'PdhGetCounterInfoW';
function PdhExpandWildCardPathW( szDataSource : PWideChar; szWildCardPath : PWideChar; mszExpandedPathList : PPWideChar; pcchPathListLength : PDWORD; dwFlags: DWORD) : HRESULT; stdcall; external 'pdh' name 'PdhExpandWildCardPathW';

function PdhAddCounterW( hQuery : THANDLE; szFullCounterPath : PWideChar; dwUserData: PDWORD; phCounter : pointer ) : HRESULT; stdcall; external 'pdh' name 'PdhAddCounterW';
function PdhCollectQueryData( hQuery : THANDLE ) : HRESULT; stdcall; external 'pdh' name 'PdhCollectQueryData';
function PdhGetFormattedCounterValue( hCounter : THANDLE; dwFormat : DWORD; lpdwType: pointer; pValue: pointer ) : HRESULT; stdcall; external 'pdh' name 'PdhGetFormattedCounterValue';
function PdhGetFormattedCounterArrayA( hCounter : THANDLE; dwFormat : DWORD; lpdwBufferSize: PDWORD; lpdwItemCount: PDWORD; ItemBuffer : PTPDH_FMT_COUNTERVALUE_ITEM_A) : HRESULT; stdcall; external 'pdh' name 'PdhGetFormattedCounterArrayA';

function SHQueryUserNotificationState( p : Pointer ) : HRESULT; stdcall; external shell32 name 'SHQueryUserNotificationState';

constructor TSystemDataThread.Create;
var
  lprocessorinfo:TProcessorInformation;
begin
  STComputername := Computername;
  STUsername := Username;
  ProcList := TStringList.Create;

  Smbios := tsmbios.create;
    for LProcessorInfo in SMBios.ProcessorInfo do
      CPUType := LProcessorInfo.ProcessorVersionStr;
  CPUSpeed := 0;
  setLength(PPIbuff, ProcessorCount);

  if ( PdhOpenQueryW(nil, nil, @pdhQueryHandle) >= 0 ) then
  begin
    if getIsWin10 then // >= win10
    begin
      PdhAddEnglishCounterW(pdhQueryHandle, pWidechar('\Processor Information(*)\% Processor Utility'), nil, @pdhCPUUsageCounterHandle);
      PdhAddEnglishCounterW(pdhQueryHandle, pWidechar('\Processor Information(*)\% Processor Performance'), nil, @pdhCPUPerformanceCounterHandle);
      PdhAddEnglishCounterW(pdhQueryHandle, pWidechar('\Processor Information(*)\Processor Frequency'), nil, @pdhCPUFrequencyCounterHandle);
    end
    else
    begin // < win10 Needs testing
      PdhAddEnglishCounterW(pdhQueryHandle, pWidechar('\Processor Information(*)\% Processor Time'), nil, @pdhCPUUsageCounterHandle);
      PdhAddEnglishCounterW(pdhQueryHandle, pWidechar('\ProcessorPerformance(*)\frequency'), nil, @pdhCPUPerformanceCounterHandle);
    end;
  end;
  inherited Create(500);
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

// Get windows versions
// TODO: Need to look into this more. we need to determine which version of windows we have
// Maybe rewrite some stuff to make this check simpler
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

function TSystemDataThread.getIsWin11: Boolean;
var
  oviVersionInfo: windows.TOSVERSIONINFO;
begin
  oviVersionInfo.dwOSVersionInfoSize := SizeOf(oviVersionInfo);
  if not windows.GetVersionEx(oviVersionInfo) then raise
    Exception.Create('Can''t get the Windows version');
  if (oviVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT) and
    (oviVersionInfo.dwMajorVersion >= 10) and (oviVersionInfo.dwBuildNumber >=22000) then getIsWin11 := true
  else getIsWin11 := false;
end;
// end get windows versions

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
  t: QWORD;
  y, mo, d, h, m, s : Cardinal;
  uiRemaining: Cardinal;
  sTempUptime: String;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  i: integer;
  pdhCounterType: DWORD;
  pdhCounterItems: array of TPDH_FMT_COUNTERVALUE_ITEM_A;
  pdhFmtCounterValue: TPDH_FMT_COUNTERVALUE;
  pdhItemCount: dword;
  pdhBuffer: pointer;
  pp: pointer; // so we can move around in the above pointer
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

  fDataLock.Enter;
  PdhCollectQueryData(pdhQueryHandle);

  // CPU Usage
  pdhBufferSize := 0;
  ret := PdhGetFormattedCounterArrayA(pdhCPUUsageCounterHandle, PDH_FMT_DOUBLE, @pdhBufferSize, @pdhItemCount, nil);
  if ret = PDH_MORE_DATA then
  begin
    pdhBuffer := getmem(pdhBufferSize); // we can't just pass CounterItems to PdhGetFormattedCounterArrayA as the pointer mem is tacked to the end of the structure
    pp := pdhBuffer;
    SetLength(pdhCounterItems, pdhItemCount);
    ret := PdhGetFormattedCounterArrayA(pdhCPUUsageCounterHandle, PDH_FMT_DOUBLE, @pdhBufferSize, @pdhItemCount, pdhBuffer);

    for i := 0 to pdhItemCount -1 do
    begin
      pdhCounterItems[i] := TPDH_FMT_COUNTERVALUE_ITEM_A(pp^);
      pp := pp + 24; // advance the pointer to the start of the next record.
    end;
  end;

  if pdhItemCount > 0 then
  begin
    SetLength(CPUUsageArray, pdhItemCount);
    for i := 0 to pdhItemCount -1 do
    begin
      CPUUsageArray[i].name := strpas(pdhCounterItems[i].szName);
      CPUUsageArray[i].value := (double(pdhCounterItems[i].PDH_FMT_COUNTERVALUE.LargeValue) + CPUUsageArray[i].value) / 2;
    end;
    CPUUse := CPUUsageArray[pdhItemCount - 1].Value;
  end;
  if pdhItemCount > 0 then
  freemem(pdhBuffer);

  // CPU Clock
  pdhItemCount := 0;
  pdhBufferSize := 0;
  ret := PdhGetFormattedCounterArrayA(pdhCPUPerformanceCounterHandle, PDH_FMT_DOUBLE, @pdhBufferSize, @pdhItemCount, nil);
  if ret = PDH_MORE_DATA then
  begin
    pdhBuffer := getmem(pdhBufferSize);
    pp := pdhBuffer;
    SetLength(pdhCounterItems, pdhItemCount);
    ret := PdhGetFormattedCounterArrayA(pdhCPUPerformanceCounterHandle, PDH_FMT_DOUBLE, @pdhBufferSize, @pdhItemCount, pdhBuffer);

    for i := 0 to pdhItemCount -1 do
    begin
      pdhCounterItems[i] := TPDH_FMT_COUNTERVALUE_ITEM_A(pp^);
      pp := pp + 24; // advance the pointer to the start of the next record.
    end;
  end;

  if getIsWin10 then
  begin
    if pdhItemCount > 0 then
    begin
      SetLength(CPUPerformanceArray, pdhItemCount);
      for i := 0 to pdhItemCount -1 do
      begin
        ret := PdhGetFormattedCounterValue(pdhCPUFrequencyCounterHandle, PDH_FMT_DOUBLE, @pdhCounterType, @pdhFmtCounterValue);
        CPUPerformanceArray[i].name := strpas(pdhCounterItems[i].szName);
        CPUPerformanceArray[i].value := (((double(pdhFmtCounterValue.LargeValue)  / 100) * double(pdhCounterItems[i].PDH_FMT_COUNTERVALUE.LargeValue)) + CPUPerformanceArray[i].value) /2;
      end;
      CPUSpeed := CPUPerformanceArray[pdhItemCount - 1].value;
    end;
  end
  else
  begin
    if pdhItemCount > 0 then
    begin
      SetLength(CPUPerformanceArray, pdhItemCount);
      for i := 0 to pdhItemCount -1 do
      begin
        CPUPerformanceArray[i].name := strpas(pdhCounterItems[i].szName);
        CPUPerformanceArray[i].value := (double(pdhCounterItems[i].PDH_FMT_COUNTERVALUE.LargeValue) + CPUPerformanceArray[i].value) / 2;
      end;
      CPUSpeed := CPUPerformanceArray[pdhItemCount - 1].value;
    end;
  end;
  if pdhItemCount > 0 then
  freemem(pdhBuffer);
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
      Line := Line + inttostr(isapplicationactive(LCDSmartieDisplayForm.Data.change(args[1]))) + postfix;
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
          cpuname := stripspaces(LCDSmartieDisplayForm.Data.change(args[1]))+','+stripspaces(LCDSmartieDisplayForm.Data.change(args[2]))
        else if numargs = 1 then
          cpuname := stripspaces(LCDSmartieDisplayForm.Data.change(args[1]))
        else
          raise Exception.Create('Bad parameters');

        for i := 0 to length(CPUUsageArray) - 1  do
          if CPUUsageArray[i].name = cpuname then
            cpuval := Format('%.0f', [CPUUsageArray[i].value]);

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
        begin
          cpuname := stripspaces(LCDSmartieDisplayForm.Data.change(args[1]))+','+stripspaces(LCDSmartieDisplayForm.Data.change(args[2]));
          for i := 0 to length(CPUPerformanceArray) - 1  do
          if CPUPerformanceArray[i].name = cpuname then
            cpuval := Format('%.0f', [CPUPerformanceArray[i].value]);
        end
        else if numargs = 1 then
        begin
          cpuname := stripspaces(LCDSmartieDisplayForm.Data.change(args[1]));
          cpuval := Format('%.0f', [CPUPerformanceArray[strtoint(cpuname)].value]);
        end
        else
          raise Exception.Create('Bad parameters');



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
    Line := StringReplace(line, CPUSpeedMhzKey, Format('%.0f', [CPUSpeed]), [rfReplaceAll]);
    Line := StringReplace(line, CPUSpeedGhzKey, Format('%.2f', [CPUSpeed / 1000]) , [rfReplaceAll]);
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
