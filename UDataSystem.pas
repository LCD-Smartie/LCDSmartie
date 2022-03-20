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

{$MODE Delphi}

interface

uses
  DataThread, Windows, SysUtils, Classes, Registry, shellapi, jwatlhelp32;

const
  // lets put a prefix key to make it clear what they are
  SysKey = '$Sys';
  UserNameKey = SysKey + 'Username';
  ComputerNameKey = SysKey + 'Computername';
  UpTimeKey = SysKey + 'Uptime';
  UpTimeShortKey = SysKey + 'Uptims';
  ScreensaverActiveKey = SysKey + 'SSActive';
  FullScreenGameActive = SysKey + 'FSGameActive';
  FullScreenAppActive = SysKey + 'FSAppActive';
  ApplicationActive = SysKey + 'AppActive';


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
    function getIsWin2kXP: Boolean;
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

implementation

uses
  UUtils, StrUtils;

function SHQueryUserNotificationState( p : Pointer ) : HRESULT; stdcall; external shell32 name 'SHQueryUserNotificationState';

constructor TSystemDataThread.Create;
begin
  STComputername := Computername;
  STUsername := Username;
  inherited Create(100);
end;

destructor TSystemDataThread.Destroy;
begin
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

function TSystemDataThread.getIsWin2kXP: Boolean;
var
  oviVersionInfo: TOSVERSIONINFO;
begin
  oviVersionInfo.dwOSVersionInfoSize := SizeOf(oviVersionInfo);
  if not GetVersionEx(oviVersionInfo) then raise
    Exception.Create('Can''t get the Windows version');
  if (oviVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT) and
    (oviVersionInfo.dwMajorVersion >= 5) then getIsWin2kXP := true
  else getIsWin2kXP := false;
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

//// crap to detect if program is active
function processExists(exeFileName: string): Boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  Result := False;
  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(ExeFileName))) then
    begin
      Result := True;
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

//// detect if screensaver is active
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
  fDataLock.Enter();
  uptimeregs := MidStr(sTempUptime, 1, Length(sTempUptime)-1);
  fDataLock.Leave();

end;

procedure TSystemDataThread.ResolveVariables(var Line : string);
var
  mem: Int64;
  args: Array [1..maxArgs] of String;
  prefix, postfix: String;
  numArgs: Cardinal;
begin

  if (pos(SysKey,Line) > 0) then begin
  if (pos(ApplicationActive,Line) > 0) then begin
    while decodeArgs(line, ApplicationActive, maxArgs, args, prefix, postfix, numargs) do
    begin
      Line := prefix;
      Line := Line + inttostr(isapplicationactive(args[1])) + postfix;
    end;
  end;

  if (pos(UserNameKey,Line) > 0) then
  Line := StringReplace(line, UserNameKey, STUsername, [rfReplaceAll]);

  if (pos(ComputerNameKey,Line) > 0) then
  Line := StringReplace(line, ComputerNameKey, STcomputername, [rfReplaceAll]);

  if (pos(UpTimeKey,Line) > 0) then
  line := StringReplace(line, UpTimeKey, uptimereg, [rfReplaceAll]);

  if (pos(UpTimeShortKey,Line) > 0) then
  line := StringReplace(line, UpTimeShortKey, uptimeregs, [rfReplaceAll]);

  if (pos(ScreensaverActiveKey,Line) > 0) then
  Line := StringReplace(line, ScreensaverActiveKey, inttostr(isscreensaveractive), [rfReplaceAll]);

  if (pos(FullScreenGameActive,Line) > 0) then
  Line := StringReplace(line, FullScreenGameActive, inttostr(isfullscreengameactive), [rfReplaceAll]);

  if (pos(FullScreenAppActive,Line) > 0) then
  Line := StringReplace(line, FullScreenAppActive, inttostr(isfullscreenappactive), [rfReplaceAll]);
  end;

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
