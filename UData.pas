unit UData;

{$MODE Delphi}

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
 *  $Source: /root/lcdsmartie-cvsbackup/lcdsmartie/UData.pas,v $
 *  $Revision: 1.73 $ $Date: 2007/01/04 11:37:03 $
 *****************************************************************************}

interface

uses
  Classes, SysUtils, UDataEmail,math;

const
  // if this is increased, increase the 3 legacyloader function ID's also
  iMaxPluginFuncs = 20;
  finiFuncId = 21;
  infoFuncId = 22;
  demoFuncId = 23;

type
  TMyProc = function(param1: pchar; param2: pchar): Pchar; stdcall;
  TFiniProc = procedure(); stdcall;
  TinfoProc = function(): Pchar; stdcall;
  TdemoProc = function(): Pchar; stdcall;
  TBridgeProc = function(iBridgeId: Integer; iFunc: Integer; param1: pchar; param2: pchar): Pchar; stdcall;
  TBridgeInfoProc = function(iBridgeId: Integer): Pchar; stdcall;
  TBridgeDemoProc = function(iBridgeId: Integer): Pchar; stdcall;
  {$IF Defined(CPUX64)}
  TSharedMem=record
    LibraryID: Integer;
    LibraryPath: Array[1..255] of char;
    LibraryFunc: Integer;
    LibraryParam1: Array[1..2048] of char;
    LibraryParam2: Array[1..2048] of char;
    LibraryResult: Array[1..2048] of char;
  end;
  PTSharedMem=^TSharedMem;
  {$IFEND}

  TDll = Record
    sName: String;
    hDll: HMODULE;
    bBridge: Boolean;
    legacyDll: boolean;
    iBridgeId: Integer;
    functions: Array [1..iMaxPluginFuncs] of TMyProc;
    bridgeFunc: TBridgeProc;
    bridgeInfoFunc: TBridgeInfoProc;
    bridgeDemoFunc: TBridgeDemoProc;
    finiFunc: TFiniProc;
    infoFunc:  TinfoProc;
    demoFunc:  TdemoProc;
    uiLastRefreshed: Cardinal;  // time when Dll results were refreshed.
    uiMinRefreshInterval: Cardinal; // min Refresh interval between refreshes.
  end;

  TData = Class(TObject)
  private
    cacheresult_lastFindPlugin: Cardinal;
    cache_lastFindPlugin: String;
    uiScreenStartTime: Cardinal; // time that new start refresh started (used by plugin cache code)
    bNewScreenEvent: Boolean;
    bForceRefresh: Boolean;

    // DLL plugins
    dlls: Array of TDll;
    uiTotalDlls: Cardinal;
    sDllResults: array of string;
    iDllResults: Integer;

    // email thread
    EmailThread : TEmailDataThread;  // keep a copy for mainline "GotMail"

    DataThreads : TList;  // of TDataThread

    // other variables
    procedure ResolveOtherVariables(var line: String);
    procedure ResolveTimeVariable(var line: String);
    procedure ResolveStringFunctionVariables(var line: String);
    procedure ResolveLCDFunctionVariables(var line: String);
    // file data
    procedure ResolveFileVariables(var line: String);
    // dll plugins
    procedure LoadPlugin(sDllName: String; bDotNet: Boolean = false);
    procedure ResolvePluginVariables(var line: String; qstattemp: Integer;
      bCacheResults: Boolean);

    // e-mail stuff
    function  GetGotEmail : boolean;
  public
    cLastKeyPressed: Char;
    storage: array [0..100] of string;
    procedure ScreenStart;
    procedure ScreenEnd;
    procedure NewScreen(bYes: Boolean);
    function change(line: String; qstattemp: Integer = 1;
      bCacheResults: Boolean = false): String;
    function CallPlugin(uiDll: Integer; iFunc: Integer;
                    const sParam1: String; const sParam2:String) : String;
    function FindPlugin(const sDllName: String): Cardinal;
    function GetPluginInfo(dllName: string) : String;
    function GetPluginDemos(dllName: string) : String;
    constructor Create;
    destructor Destroy; override;
    function CanExit: Boolean;
    procedure RefreshDataThreads;
    //
    property GotEmail : boolean read GetGotEmail;
  end;

implementation

uses
  Windows, Forms, Dialogs, StrUtils, Winsock,
  UMain, UUtils, UConfig,
  DataThread, UDataNetwork, UDataDisk, UDataGame, UDataSystem,
  UDataBoinc,  UDataFolding, UDataRSS, UDataDNet,
  UDataWinamp, UDataSender, UDataPerf;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////                                                                       ////
////      M A I N   D A T A    F U N C T I O N S                           ////
////                                                                       ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

var
  dllmessage: string;
  {$IF Defined(CPUX64)}
  LegacySharedMem: PTSharedMem;
  hMap: THandle;
  hLegacyLoadLibraryRunning: THandle;
  hLegacyLoadLibraryEvent: THandle;
  hLegacyCallFunctionEvent: THandle;
  hLegacyRecvEvent: THandle;
  LegacyLoaderStarted: boolean;
  {$IFEND}
constructor TData.Create;
var
//  status: Integer;
//  WSAData: TWSADATA;
  DataThread : TDataThread;
begin
  inherited;

//  status := WSAStartup(MAKEWORD(2,0), WSAData);
//  if status <> 0 then
//     raise Exception.Create('WSAStartup failed');

  {$IF Defined(CPUX64)}
  LegacyLoaderStarted := false;
  {$IFEND}

  uiTotalDlls := 0;

  DataThreads := TList.Create;

  EmailThread := TEmailDataThread.Create;  // keep a copy for mainline GotEmail call
  EmailThread.Start;
  DataThreads.Add(EmailThread);

  DataThread := TGameDataThread.Create;
  DataThread.Start;
  DataThreads.Add(DataThread);

  DataThread := TNetworkDataThread.Create;
  DataThread.Start;
  DataThreads.Add(DataThread);

  DataThread := TDiskDataThread.Create;
  DataThread.Start;
  DataThreads.Add(DataThread);

  DataThread := TSystemDataThread.Create;
  DataThread.Start;
  DataThreads.Add(DataThread);

  DataThread := TBOINCDataThread.Create;
  DataThread.Start;
  DataThreads.Add(DataThread);

  DataThread := TFoldingDataThread.Create;
  DataThread.Start;
  DataThreads.Add(DataThread);

  DataThread := TDNetDataThread.Create;
  DataThread.Start;
  DataThreads.Add(DataThread);
  
  DataThread := TSenderDataThread.Create;
  DataThread.Start;
  DataThreads.Add(DataThread);

  DataThread := TPerfDataThread.Create;
  DataThread.Start;
  DataThreads.Add(DataThread);

  DataThread := TRSSDataThread.Create;
  DataThread.Start;
  DataThreads.Add(DataThread);
end;

function TData.CanExit: Boolean;
var
  uiDll: Cardinal;
  Loop : longint;
  {$IF Defined(CPUX64)}
  waitresult:dword;
  {$IFEND}
begin
  for Loop := 0 to DataThreads.Count-1 do begin
    TDataThread(DataThreads[Loop]).Terminate;
  end;

  // close all plugins
  for uiDll:=1 to uiTotalDlls do
  begin
    try
      {$IF Defined(CPUX64)}
      if dlls[uiDll-1].legacyDll then
      begin
        LegacySharedMem^.LibraryID := dlls[uiDll-1].iBridgeId;
        LegacySharedMem^.LibraryFunc := finiFuncId;
        setevent(hLegacyCallFunctionEvent);
        waitresult := WaitForSingleObject(hLegacyRecvEvent,10000);
        if (waitresult = WAIT_TIMEOUT) then
          raise Exception.Create('Plugin '+dlls[uiDll-1].sName+'Legacy Plugin close library time out');
        continue;
      end;
      {$IFEND}

      if (dlls[uiDll-1].hDll <> 0) then
      begin
        // call SmartieFini if it exists
        if (Assigned(dlls[uiDll-1].finiFunc)) then
        begin
          try
            dlls[uiDll-1].finiFunc();
          except
            on E: Exception do
              raise Exception.Create('Plugin '+dlls[uiDll-1].sName+' had an exception during closedown: '
              + E.Message);
          end;
        end;
        FreeLibrary(dlls[uiDll-1].hDll);
      end;
    except
    end;
    dlls[uiDll-1].hDll := 0;
  end;
  uiTotalDlls := 0;

  Result := True;
end;

destructor TData.Destroy;
var
  Loop : longint;
begin

  for Loop := 0 to DataThreads.Count-1 do begin
    TDataThread(DataThreads[Loop]).WaitFor;
    TDataThread(DataThreads[Loop]).Free;
  end;

  DataThreads.Free;
  WSACleanup();

  inherited;
end;

procedure TData.RefreshDataThreads;
var
  Loop : longint;
begin
  for Loop := 0 to DataThreads.Count-1 do begin
    TDataThread(DataThreads[Loop]).ForceRefresh;
  end;
end;

procedure TData.NewScreen(bYes: Boolean);
var
  Loop : longint;
begin
  bNewScreenEvent := bYes;
  if (bYes) then
  begin
    bForceRefresh := true;
    for Loop := 0 to DataThreads.Count-1 do begin
      TDataThread(DataThreads[Loop]).Active := false;
      TDataThread(DataThreads[Loop]).Refresh;
    end;
  end;
end;

procedure TData.ScreenStart;
begin
  iDllResults := 0;
  uiScreenStartTime := GetTickCount();
end;

procedure TData.ScreenEnd;
begin
  bForceRefresh := false;
end;

function TData.change(line: String; qstattemp: Integer = 1;
   bCacheResults: Boolean = false): String;
label
  endChange;
var
  Loop : longint;
begin
  try
      for Loop := 0 to DataThreads.Count-1 do begin
        TDataThread(DataThreads[Loop]).ResolveVariables(Line);
        if (Pos('$', line) = 0) then goto endChange;
      end;

      ResolvePluginVariables(line, qstattemp, bCacheResults);
      if (Pos('$', line) = 0) then goto endChange;
      ResolveOtherVariables(Line);
      ResolveFileVariables(Line);
      if (Pos('$', line) = 0) then goto endChange;
      ResolveLCDFunctionVariables(Line);
      ResolveWinampVariables(line);
      if (Pos('$', line) = 0) then goto endChange;
      ResolveTimeVariable(Line);

endChange:
  ResolveStringFunctionVariables(Line); // only do these after all others resolved
  except
    on E: Exception do line := '[Unhandled Exception: '
      + CleanString(E.Message) + ']';
  end;

  line := StringReplace(line, Chr($A), '', [rfReplaceAll]);
  line := StringReplace(line, Chr($D), '', [rfReplaceAll]);
  line := StringReplace(line, #226+#150+#136, #255, [rfReplaceAll]); // full Block
  line := StringReplace(line, #194+#176, #176, [rfReplaceAll]); // Degree - hd44780 only has a square degree in 223 so this is a custom
  line := StringReplace(line, #226+#128+#153, #39, [rfReplaceAll]); // Utf8 apostrophe
  line := StringReplace(line, #239+#191+#189, '?', [rfReplaceAll]); // un-representable character
  result := line;
end;


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////                                                                       ////
////      E - M A I L    C H E C K I N G    P R O  C E D U R E S           ////
////                                                                       ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

function TData.GetGotEmail : boolean;
begin
  Result := false;
  if assigned(EmailThread) then
    Result := EmailThread.GotEmail;
end;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////                                                                       ////
////      R E S O L V E    O T H E R    V A R I A B L E S                  ////
////                                                                       ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

procedure TData.ResolveOtherVariables(var line: String);
var
  args: Array [1..maxArgs] of String;
  prefix, postfix: String;
  numArgs: Cardinal;
  spacecount : Integer;
  ccount: double;
  tempst : String;
  iPos1, iPos2 : Integer;
  screenResolution: String;
  t: extended;
  fmt: TFormatSettings;
begin
  fmt := DefaultFormatSettings;
  fmt.DecimalSeparator := '.';

  while decodeArgs(line, '$ActionEnabled', maxArgs, args, prefix, postfix, numargs) do begin
    if lowercase(config.actionsArray[strtoint(args[1]), 5]) = 'true' then
      tempst := '1'
    else
      tempst := '0';
    line := prefix + tempst + postfix;
  end;

  while decodeArgs(line, '$ScreenReso', maxArgs, args, prefix, postfix, numargs) do begin
    screenResolution := IntToStr(Screen.Monitors[strtoint(args[1])].Width) + 'x' +
      IntToStr(Screen.Monitors[strtoint(args[1])].Height);

    line := prefix + screenResolution  + postfix;
  end;

  if (pos('$ScreenReso', line) <> 0) then
  begin
    screenResolution := IntToStr(Screen.DesktopWidth) + 'x' +
      IntToStr(Screen.DesktopHeight);

    line := StringReplace(line, '$ScreenReso', screenResolution,
      [rfReplaceAll]);
  end;

  if decodeArgs(line, '$MObutton', maxArgs, args, prefix, postfix, numargs)
    then
  begin
    spacecount := 0;
    if (numargs = 1) and (cLastKeyPressed = change(args[1])) then spacecount := 1;

    line := prefix + intToStr(spacecount) + postfix;
  end;

  if pos('$ScreenChanged', line) <> 0 then
  begin
    spacecount := 0;
    if (bNewScreenEvent) then
      spacecount := 1;

    line := StringReplace(line, '$ScreenChanged', IntToStr(spacecount), [rfReplaceAll]);
  end;

  while decodeArgs(line, '$Count', maxArgs, args, prefix, postfix, numargs)
    do
  begin
    ccount := 0;
    try
      RequiredParameters(numargs, 1, 1);
      tempst := change(args[1]);
      iPos1 := 1;
      iPos2 := pos('#', tempst);

      repeat
        if (iPos2 = 0) then
          ccount := ccount + StrToFloatN(tempst, iPos1, length(tempst)-iPos1+1)
        else
          ccount := ccount + StrToFloatN(tempst, iPos1, iPos2-iPos1);
        iPos1 := iPos2 + 1;
        iPos2 := PosEx('#', tempst, iPos1);
      until (iPos1 = 1);

      line := prefix + FloatToStr(ccount, fmt) + postfix;
    except
      on E: Exception do line := prefix + '[Count: '
        + CleanString(E.Message) + ']' + postfix;
    end;
  end;

  while decodeArgs(line, '$Store', maxArgs, args, prefix, postfix, numargs)
  do
  begin
    try
      RequiredParameters(numargs, 2, 2);
      storage[strtoint(change(args[2]))] := change(args[1]);
      line := prefix + postfix;
    except
      on E: Exception do line := prefix + '[Count: '
          + CleanString(E.Message) + ']' + postfix;
    end;
  end;

  while decodeArgs(line, '$Fetch', maxArgs, args, prefix, postfix, numargs)
  do
  begin
    ccount := 0;
    try
      RequiredParameters(numargs, 1, 1);
      line := prefix + storage[strtoint(change(args[1]))] + postfix;
    except
      on E: Exception do line := prefix + '[Count: '
        + CleanString(E.Message) + ']' + postfix;
    end;
  end;

  while decodeArgs(line, '$Round', maxArgs, args, prefix, postfix, numargs)
  do
  begin
    try
      RequiredParameters(numargs, 2, 2);
      t := power(10, strtoint(change(args[2])));
      line := prefix + floattostr(round(strtofloat(change(args[1]),fmt)*t)/t,fmt) + postfix;
    except
      on E: Exception do line := prefix + '[Round: '
        + CleanString(E.Message) + ']' + postfix;
    end;
  end;

  while decodeArgs(line, '$Add', maxArgs, args, prefix, postfix, numargs)
  do
  begin
    try
      RequiredParameters(numargs, 2, 2);
      line := prefix + floattostr(strtofloat(change(args[1]),fmt) + strtofloat(change(args[2]),fmt),fmt) + postfix;
    except
      on E: Exception do line := prefix + '[Add: '
        + CleanString(E.Message) + ']' + postfix;
    end;
  end;

  while decodeArgs(line, '$Sub', maxArgs, args, prefix, postfix, numargs)
  do
  begin
    try
      RequiredParameters(numargs, 2, 2);
      line := prefix + floattostr(strtofloat(change(args[1]),fmt) - strtofloat(change(args[2]),fmt),fmt) + postfix;
    except
      on E: Exception do line := prefix + '[Sub: '
        + CleanString(E.Message) + ']' + postfix;
    end;
  end;

  while decodeArgs(line, '$Mul', maxArgs, args, prefix, postfix, numargs)
  do
  begin
    try
      RequiredParameters(numargs, 2, 2);
      line := prefix + floattostr(strtofloat(change(args[1]),fmt) * strtofloat(change(args[2]),fmt),fmt) + postfix;
    except
      on E: Exception do line := prefix + '[Mul: '
        + CleanString(E.Message) + ']' + postfix;
    end;
  end;

  while decodeArgs(line, '$Div', maxArgs, args, prefix, postfix, numargs)
  do
  begin
    try
      RequiredParameters(numargs, 2, 2);
      line := prefix + floattostr(strtofloat(change(args[1]),fmt) / strtofloat(change(args[2]),fmt),fmt) + postfix;
    except
      on E: Exception do line := prefix + '[Div: '
        + CleanString(E.Message) + ']' + postfix;
    end;
  end;
end;

procedure TData.ResolveTimeVariable(var line: String);
var
  tempst, line2 : String;
begin
  while pos('$Time(', line) <> 0 do
  begin
    try
      line2 := copy(line, pos('$Time(', line) + 6, length(line));
      if (pos(')', line2) = 0) then
        raise Exception.Create('No ending bracket');
      line2 := copy(line2, 1, pos(')', line2)-1);
      tempst := formatdatetime(line2, now, localeFormat);
      line := StringReplace(line, '$Time(' + line2 + ')', tempst, []);
    except
      on E: Exception do line := StringReplace(line, '$Time(', '[Time: '
        + CleanString(E.Message) + ']', []);
    end;
  end;
end;

procedure TData.ResolveLCDFunctionVariables(var line: String);
var
  spaceline, line2 : String;
  h, iPos1, iPos2 : Integer;
begin
  iPos1 :=  pos('$CustomChar(', line);
  while (iPos1 <> 0) do
  begin
    try
      iPos2 := PosEx(')', line, iPos1+12);
      if (iPos2 = 0) then
        raise Exception.Create('No ending bracket');
      LCDSmartieDisplayForm.customchar(AnsiMidStr(line, iPos1+12, iPos2-(iPos1+12)));
      Delete(line, iPos1, iPos2-iPos1+1);
    except
      on E: Exception do line := StringReplace(line, '$CustomChar(',
        '[CustomChar: ' + CleanString(E.Message) + ']', []);
    end;
    iPos1 :=  PosEx('$CustomChar(', line, iPos1);
  end;

  while (pos('$Flash(', line) <> 0) do
  begin
    try
      line2 := copy(line, pos('$Flash(', line) + 7, (pos('$)$',
        line))-(pos('$Flash(', line) + 7));
      if (LCDSmartieDisplayForm.doesflash) then
      begin
        spaceline := '';
        for h := 1 to length(line2) do
        begin
          spaceline := spaceline + ' ';
        end;
      end
      else
      begin
        spaceline := line2;
      end;
      if pos('$)$', line) <> 0 then line := StringReplace(line, '$Flash('
        + line2 + '$)$', spaceline, [])
      else line := StringReplace(line, '$Flash(', 'ERROR', []);
    except
      on E: Exception do line := StringReplace(line, '$Flash(', '[Flash: '
        + CleanString(E.Message) + ']', []);
    end;
  end;
end;

procedure TData.ResolveStringFunctionVariables(var line: String);
var
  args: Array [1..maxArgs] of String;
  prefix, postfix: String;
  numArgs: Cardinal;
  h, x, iPos1, iPos2, iPos3: Integer;
  spacecount : Integer;
  spaceline : string;
  STHDBar: String;
begin

  iPos1 := pos('$Right(', line);
  while iPos1 <> 0 do
  begin
    try
      iPos2 := PosEx(',$', line, iPos1+1);
      if (iPos2 = 0) then
        raise Exception.Create('Missing ",$"');

      iPos3 := PosEx('%)', line, iPos2+2);
      if (iPos3 = 0) then
        raise Exception.Create('Missing "%)"');

      spacecount := StrToIntN(line, iPos2 + 2, iPos3-(iPos2+2));
      Delete(line, iPos2, (iPos3+2)-iPos2);
      Delete(line, iPos1, 7);
      if (spacecount >  iPos2-(iPos1+7)) then
        Insert(DupeString(' ', spacecount-(iPos2-(iPos1+7))), line, iPos1);
    except
      on E: Exception do line := StringReplace(line, '$Right(', '[Right: '
        + CleanString(E.Message) + ']', []);
    end;

    iPos1 := PosEx('$Right(',line,iPos1);
  end;

  while decodeArgs(line, '$Center', maxArgs, args, prefix, postfix, numargs)
    do
  begin
    try
      RequiredParameters(numargs, 1, 2);
      if (numargs = 1) then spacecount := config.width
      else spacecount := StrToInt(change(args[2]));

      line := prefix + CenterText(change(args[1]), spacecount) + postfix;
    except
      on E: Exception do line := prefix + '[Center: '
        + CleanString(E.Message) + ']' + postfix;
    end;
  end;

  while decodeArgs(line, '$Chr', maxArgs, args, prefix, postfix, numargs) do
  begin
    try
      RequiredParameters(numargs, 1, 1);
      line := prefix + Chr(StrToInt(change(args[1]))) + postfix;
    except
      on E: Exception do line := prefix + '[Chr: '
        + CleanString(E.Message) + ']' + postfix;
    end;
  end;

  while decodeArgs(line, '$Fill', maxArgs, args, prefix, postfix, numargs) do
  begin
    try
      RequiredParameters(numargs, 1, 1);
      spacecount := StrToInt(change(args[1]));
      spaceline := '';

      if spacecount > length(prefix) then
        spaceline := DupeString(' ', spacecount - length(prefix));

      line := prefix + spaceline + postfix;
    except
      on E: Exception do line := prefix + '[Fill: ' + E.Message + ']' +
        postfix;
    end;
  end;

  while decodeArgs(line, '$Bar', maxArgs, args, prefix, postfix, numargs)
    do
  begin
    try
      RequiredParameters(numargs, 3, 3);
      spacecount := strtoint(change(args[3]))*3;

      if (StrToFloat(change(args[2]), localeFormat) <> 0) then
        x := round(StrToFloat(change(args[1]), localeFormat)
                  * spacecount / StrToFloat(change(args[2]), localeFormat))
      else x := 0;

      if x > spacecount then x := spacecount;
      STHDBar := '';
      for h := 1 to (x div 3) do STHDBar := STHDBar + #255 ;
      if (x mod 3 = 1) then STHDBar := STHDBar + chr(131);
      if (x mod 3 = 2) then STHDBar := STHDBar + chr(132);
      for h := 1 to round(spacecount/3)-Length(STHDBar) do STHDBar :=
        STHDBar + '_';

      line := prefix + STHDBar + postfix;
    except
      on E: Exception do line := prefix + '[Bar: '
        + CleanString(E.Message) + ']' + postfix;
    end;
  end;

end;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////                                                                       ////
////      L O A D   F R O M    F I L E    P R O C E D U R E S              ////
////                                                                       ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


procedure TData.ResolveFileVariables(var line: String);
var
  hdcounter: Integer;
  args: Array [1..maxArgs] of String;
  prefix, postfix: String;
  numArgs: Cardinal;
  spaceline, sFileloc : string;
  i,iFileline: Integer;
  FileStream: TFileStream;
  Lines: TStringList;
  fFile3: textfile;
  line3 : string;
  iBytesToRead: Integer;
  counter3: Integer;
begin
  hdcounter := 0;
  while decodeArgs(line, '$LogFile', maxArgs, args, prefix, postfix,
    numargs) do
  begin
    try
      hdcounter := hdcounter + 1;
      if hdcounter > 4 then line := StringReplace(line, '$LogFile(',
        'error', []);

      sFileloc := change(args[1]);
      if (sFileloc[1] = '"') and (sFileloc[Length(sFileLoc)] = '"') then
        sFileloc := copy(sFileloc, 2, Length(sFileloc)-2);

      if (not FileExists(sFileloc)) then
        raise Exception.Create('No such file');

      RequiredParameters(numargs, 2, 2);
      iFileline := StrToInt(change(args[2]));

      if iFileline > 3 then iFileline := 3;
      if iFileline < 0 then iFileline := 0;

      FileStream := TFileStream.Create(sFileloc, fmOpenRead or fmShareDenyNone);
      iBytesToRead := 1024;
      if (FileStream.Size < iBytesToRead) then
        iBytesToRead := FileStream.Size;
      SetLength(spaceline, iBytesToRead);

      FileStream.Seek(-1 * iBytesToRead, soFromEnd);
      FileStream.ReadBuffer(spaceline[1], iBytesToRead);
      FileStream.Free;

      Lines := TStringList.Create;
      Lines.Text := spaceline;
      spaceline := stripspaces(lines[lines.count - iFileline]);
      if (pos('] ', spaceline) <> 0) then
        spaceline := copy(spaceline, pos('] ', spaceline) + 2, length(spaceline));

      for i := 0 to 7 do spaceline := StringReplace(spaceline, chr(i), '',
        [rfReplaceAll]);
      Lines.Free;
      line := prefix + spaceline + postfix;
    except
      on E: Exception do line := prefix + '[LogFile: '
        + CleanString(E.message) + ']' + postfix;
    end;
  end;

  while decodeArgs(line, '$File', maxArgs, args, prefix, postfix, numargs) do
  begin
    sFileloc := change(args[1]);
    if (sFileloc[1] = '"') and (sFileloc[Length(sFileLoc)] = '"') then
      sFileloc := copy(sFileloc, 2, Length(sFileloc)-2);

    try
      RequiredParameters(numargs, 2, 2);
      iFileline := StrToInt(change(args[2]));
      if (not FileExists(sFileloc)) then
        raise Exception.Create('No such file');
      assignfile(fFile3, sFileloc);
      reset(fFile3);
      for counter3 := 1 to iFileline do readln(fFile3, line3);
      closefile(fFile3);
      line := prefix + line3 + postfix;
    except
      on E: Exception do line := prefix + '[File: '
        + CleanString(E.Message) + ']' + postfix;
    end;
  end;
end;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////                                                                       ////
////      D L L    P L U G I N        P R O C E D U R E S                  ////
////                                                                       ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


procedure TData.ResolvePluginVariables(var line: String; qstattemp: Integer;
  bCacheResults: Boolean);
var
  args: Array [1..maxArgs] of String;
  prefix, postfix: String;
  numArgs: Cardinal;
  sParam1, sParam2: String;
  sAnswer: String;
  uiPlugin: Cardinal;
  uiMinRefresh: Cardinal;
  bCallPlugin: Boolean;
begin
  while decodeArgs(line, '$dll', maxArgs, args, prefix, postfix, numargs) do
  begin
    try
      RequiredParameters(numargs, 4, 4);

      uiPlugin := FindPlugin(change(args[1]));
      if (bCacheResults) and (not bForceRefresh) then
      begin
        if (dlls[uiPlugin].uiMinRefreshInterval < Cardinal(config.dllPeriod)) then
          uiMinRefresh := config.dllPeriod
        else
          uiMinRefresh := dlls[uiPlugin].uiMinRefreshInterval;

        if (uiScreenStartTime <= dlls[uiPlugin].uiLastRefreshed)
          or (uiScreenStartTime - dlls[uiPlugin].uiLastRefreshed > uiMinRefresh) then
        begin
          dlls[uiPlugin].uiLastRefreshed := uiScreenStartTime;

          bCallPlugin := True;
        end
        else
          bCallPlugin := False;
      end
      else
        bCallPlugin := True; // always call, if new screen or not to be cached.

      if (bCallPlugin) then
      begin
        //sParam1 := change(args[3], qstattemp);
        //sParam2 := change(args[4], qstattemp);
        try
          sAnswer := CallPlugin(uiPlugin, StrToInt(change(args[2])), change(args[3], qstattemp), change(args[4], qstattemp));
        except
          on E: Exception do
            sAnswer := '[Dll: ' + CleanString(E.Message) + ']';
        end;
      end;

      if (bCacheResults) then
      begin
        Inc(iDllResults);
        if (iDllResults >= Length(sDllResults)) then
           SetLength(sDllResults, iDllResults + 5);

        if (bCallPlugin) then
          sDllResults[iDllResults] := sAnswer // save result
        else
          sAnswer := sDllResults[iDllResults]; // get cached result
      end;

      sAnswer := change(sAnswer, qstattemp);

      line := prefix +  sAnswer + postfix;
    except
      on E: Exception do
        line := prefix + '[Dll: ' + CleanString(E.Message) + ']' + postfix;
    end;
  end;
end;

function TData.FindPlugin(const sDllName: String): Cardinal;
var
  uiDll: Cardinal;
  sLoadDllName: String;
begin
  // for speed reason - check if this is the same plugin as the last one:
  if (sDllName = cache_lastFindPlugin) then
    Result := cacheresult_lastFindPlugin
  else
  begin
    // check if we have seen this dll before
    sLoadDllName := sDllName;
    if (Pos('.DLL', UpperCase(sLoadDllName)) = 0) then
      sLoadDllName := sLoadDllName + '.dll';
    uiDll:=1;
    while (uiDll<=uiTotalDlls) and (dlls[uiDll-1].sName <> sLoadDllName) do
      Inc(uiDll);
    Dec(uiDll);

    if (uiDll >= uiTotalDlls) then
    begin // we havent seen this one before - load it
      try
        LoadPlugin(sLoadDllName);
      except
        on E: Exception do
          //showmessage('Load of plugin failed: ' + e.Message) // bloody annoying popup
          dllmessage := e.Message; // save it here instead so we can print it out later
      end;
    end;

    cacheresult_lastFindPlugin := uiDll;
    cache_lastFindPlugin := sDllName;

    Result := uiDll;
  end;
end;

function TData.CallPlugin(uiDll: Integer; iFunc: Integer;
                    const sParam1: String; const sParam2:String) : String;
{$IF Defined(CPUX64)}
var
  waitresult: integer;
{$IFEND}
begin
  {$IF Defined(CPUX64)}
  if dlls[uiDll].legacyDll then
  begin
    LegacySharedMem^.LibraryID := dlls[uiDll].iBridgeId;
    LegacySharedMem^.LibraryFunc := iFunc;
    LegacySharedMem^.LibraryParam1 := sParam1;
    LegacySharedMem^.LibraryParam2 := sParam2;
    setevent(hLegacyCallFunctionEvent);
    waitresult := WaitForSingleObject(hLegacyRecvEvent,5000);
    if (waitresult = WAIT_TIMEOUT) then
      result := 'Legacy plugin function timed out'
    else
    begin
      result := strpas(@LegacySharedMem.LibraryResult[1]);
      LegacySharedMem^.LibraryResult[1] := #0;
    end;
  end
  else
  {$IFEND}

  if (dlls[uiDll].hDll <> 0) then
  begin
    if (iFunc >= 0) and (iFunc <= iMaxPluginFuncs) then
    begin
      if (iFunc = 0) then iFunc := 10;
      try
        if (dlls[uiDll].bBridge) then
        begin
          if (@dlls[uiDll].bridgeFunc = nil) then
            raise Exception.Create('No Bridge Func');
          Result := dlls[uiDll].bridgeFunc( dlls[uiDll].iBridgeId, iFunc,
             pchar(sParam1), pchar(sParam2) );
        end
        else if @dlls[uiDll].functions[iFunc] <> nil then
          Result := dlls[uiDll].functions[iFunc]( pchar(sParam1), pchar(sParam2) )
        else
          Result := '[Dll: Function not found]';
      except
        on E: Exception do
          Result := '[Dll: ' + CleanString(E.Message) + ']';
      end;
    end
    else
      Result := '[Dll: function number out of range]';
  end
  else
    Result := '[Dll: Can not load plugin]' + dllmessage;
end;

procedure TData.LoadPlugin(sDllName: String; bDotNet: Boolean = false);
type
  TBridgeInit = function(dll: PChar; var id: Integer; var refresh: Integer): PChar; stdcall;
  TMinRefreshFunc = function: Integer; stdcall;
var
  uiDll: Cardinal;
  i: Integer;
  id: Integer;
  minRefresh: Integer;
  initFunc:  procedure; stdcall;
  minRefreshFunc: TMinRefreshFunc;
  bridgeInitFunc: TBridgeInit;
  bFound: Boolean;
  sLibraryPath: String;
  sResult: String;
  {$IF Defined(CPUX64)}
  waitresult:dword;
  GUID: TGUID;
  ShMemID: string;
  {$IFEND}
   le: integer;
begin
  bFound := false;

  uiDll := uiTotalDlls;

  Inc(uiTotalDlls);
  SetLength(dlls, uiTotalDlls);
  dlls[uiDll].sName := sDllName;
  dlls[uiDll].uiLastRefreshed := 0;
  dlls[uiDll].uiMinRefreshInterval := 300;

  dlls[uiDll].bBridge := bDotNet;
  if (bDotNet) then
    sLibraryPath := 'DNBridge.dll'
  else
    sLibraryPath := 'plugins\' + sDllName;

  dlls[uiDll].hDll := safeLoadLibrary(pchar(extractfilepath(application.exename) +
    sLibraryPath));

  {$IF Defined(CPUX64)}
  if GetLastError = 193 then
  begin
    // this means were trying to load a 32bit dll in a 64bit program
    dlls[uiDll].legacyDll := true;

    if not LegacyLoaderStarted then
    begin
      CreateGUID(GUID);
      ShMemID := GUIDToString(GUID);

      // create 8k shared memory
      if  not ( hMap > 0) then
        hMap := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, 8192, Pchar('Local\LCDSmartieLegacyPluginLoaderSM' + ShMemID));
      // map memory window
      if not (LegacySharedMem <> nil) then
        LegacySharedMem := MapViewOfFile(hMap, FILE_MAP_ALL_ACCESS, 0, 0, 0);

      // create event objects
      if  not ( hLegacyLoadLibraryRunning > 0) then
        hLegacyLoadLibraryRunning := CreateEvent(nil, FALSE, FALSE, Pchar('Local\LCDSmartieLegacyLoadLibraryRunning' + ShMemID));
      if  not ( hLegacyLoadLibraryEvent > 0) then
        hLegacyLoadLibraryEvent := CreateEvent(nil, FALSE, FALSE, Pchar('Local\LCDSmartieLegacyLoadLibraryEvent' + ShMemID));
      if  not ( hLegacyCallFunctionEvent > 0) then
        hLegacyCallFunctionEvent := CreateEvent(nil, FALSE, FALSE, Pchar('Local\LCDSmartieLegacyCallFunctionEvent' + ShMemID));
      if  not ( hLegacyRecvEvent > 0) then
        hLegacyRecvEvent := CreateEvent(nil, FALSE, FALSE, Pchar('Local\LCDSmartieLegacyRecvEvent' + ShMemID));

      // launch wrapper
      ShellExecute(0,nil, PChar('LegacyLoader.exe'),PChar(ShMemID),nil,0);
      LegacyLoaderStarted := true;
      end;

      // tell it to load this dll
      LegacySharedMem^.LibraryPath := copy(extractfilepath(application.exename) + sLibraryPath,0,255);

      // trigger load of the library. The loader will run SmartieInit and GetMinRefreshInterval functions if they exist
      setevent(hLegacyLoadLibraryEvent);

      waitresult := WaitForSingleObject(hLegacyRecvEvent,10000); // I would think and hope 10 secs is long enough to wait
      if (waitresult = WAIT_TIMEOUT) then begin
        Dec(uiTotalDlls);
        raise Exception.Create('Legacy Plugin Load library time out' );
      end;

      // we can make use of the .net iBridgeId here
      dlls[uiDll].iBridgeId := LegacySharedMem.LibraryID;
      dlls[uiDll].uiMinRefreshInterval := strtoint(PChar(@LegacySharedMem.LibraryResult[1]));
      Exit; // don't process any further. For speed plus its not neccessary
    end;
  {$IFEND}

  if (dlls[uiDll].hDll <> 0) then
  begin
    initFunc := getprocaddress(dlls[uiDll].hDll, PChar('SmartieInit'));
    if (not Assigned(initFunc)) then
      initFunc := getprocaddress(dlls[uiDll].hDll, PChar('_SmartieInit@0'));

    dlls[uiDll].finiFunc := getprocaddress(dlls[uiDll].hDll, PChar('SmartieFini'));
    if (not Assigned(dlls[uiDll].finiFunc)) then
      dlls[uiDll].finiFunc := getprocaddress(dlls[uiDll].hDll, PChar('_SmartieFini@0'));

    dlls[uiDll].infoFunc := getprocaddress(dlls[uiDll].hDll, PChar('SmartieInfo'));
    if (not Assigned(dlls[uiDll].infoFunc)) then
      dlls[uiDll].infoFunc := getprocaddress(dlls[uiDll].hDll, PChar('_SmartieInfo@0'));

    dlls[uiDll].demoFunc := getprocaddress(dlls[uiDll].hDll, PChar('SmartieDemo'));
    if (not Assigned(dlls[uiDll].demoFunc)) then
      dlls[uiDll].demoFunc := getprocaddress(dlls[uiDll].hDll, PChar('_SmartieDemo@0'));

    // Call SmartieInit if it exists.
    if (Assigned(initFunc)) then
    begin
      try
        initFunc();
      except
        on E: Exception do
          raise Exception.Create('Plugin '+sDllName+' had an exception during Init: '
            + E.Message);
      end;
    end;

    if (bDotNet) then
    begin
      {$IF Defined(CPUX86)}
      @bridgeInitFunc := getprocaddress(dlls[uiDll].hDll, PChar('_BridgeInit@12'));
      {$ELSEIF Defined(CPUX64)}
      @bridgeInitFunc := getprocaddress(dlls[uiDll].hDll, PChar('BridgeInit'));
      {$IFEND}

      if (@bridgeInitFunc = nil) then
        raise Exception.Create('Could not init bridge');

      try
        sResult := bridgeInitFunc(PChar(dlls[uiDll].sName), id, minRefresh);
      except
        on E: Exception do
          raise Exception.Create('Bridge Init for '+dlls[uiDll].sName+' had an exception: '
            + E.Message);
      end;
      if (id = -1) or (sResult <> '') then
         raise Exception.Create('Bridge Init for '+dlls[uiDll].sName+' failed with: '
            + sResult);
      dlls[uiDll].iBridgeId := id;
      if (minRefresh > 0) then
        dlls[uiDll].uiMinRefreshInterval := minRefresh;

      {$IF Defined(CPUX86)}
      @dlls[uiDll].BridgeFunc := getprocaddress(dlls[uiDll].hDll,
        PChar('_BridgeFunc@16'));
      @dlls[uiDll].BridgeInfoFunc := getprocaddress(dlls[uiDll].hDll,
        PChar('_BridgeInfoFunc@4'));
      le := GetLastError;
      @dlls[uiDll].BridgeDemoFunc := getprocaddress(dlls[uiDll].hDll,
        PChar('_BridgeDemoFunc@4'));
      {$ELSEIF Defined(CPUX64)}
      @dlls[uiDll].BridgeFunc := getprocaddress(dlls[uiDll].hDll,
        PChar('BridgeFunc'));

      @dlls[uiDll].BridgeInfoFunc := getprocaddress(dlls[uiDll].hDll,
        PChar('BridgeInfoFunc'));

      @dlls[uiDll].BridgeDemoFunc := getprocaddress(dlls[uiDll].hDll,
        PChar('BridgeDemoFunc'));
      {$IFEND}

      if (@dlls[uiDll].BridgeFunc = nil) then
        raise Exception.Create('No Bridge function found.');
    end
    else
    begin
      for i:= 1 to iMaxPluginFuncs do
      begin
        @dlls[uiDll].functions[i] := getprocaddress(dlls[uiDll].hDll,
          PChar('function' + IntToStr(i)));
        if (@dlls[uiDll].functions[i] = nil) then
          @dlls[uiDll].functions[i] := getprocaddress(dlls[uiDll].hDll,
            PChar('_function' + IntToStr(i)+'@8'));
        if (@dlls[uiDll].functions[i] <> nil) then
          bFound := True;
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
            raise Exception.Create('Plugin '+sDllName
              +' had an exception during GetMinRefreshInterval:' + E.Message);
        end;
        if (minRefresh > 0) then
          dlls[uiDll].uiMinRefreshInterval := minRefresh;
      end;

      if (not bFound) then
      begin
        if (dlls[uiDll].hDll <> 0) then FreeLibrary(dlls[uiDll].hDll);
        dlls[uiDll].hDll := 0;
        Dec(uiTotalDlls);
        LoadPlugin(dlls[uiDll].sName, true);
      end;
    end;
  end
  else
    raise Exception.Create('LoadLibrary failed with ' + ErrMsg(GetLastError));
end;

function TData.GetPluginInfo(dllName: string) : String;
var
  {$IF Defined(CPUX64)}
  waitresult: integer;
  {$IFEND}
  uiPlugin: integer;
  R: string;
begin
  result := '';
  uiPlugin := FindPlugin(dllName);
  {$IF Defined(CPUX64)}
  if dlls[uiPlugin].legacyDll then
  begin
    LegacySharedMem^.LibraryID := dlls[uiPlugin].iBridgeId;
    LegacySharedMem^.LibraryFunc := infoFuncId;
    //LegacySharedMem^.LibraryParam1 := InfoType;
    setevent(hLegacyCallFunctionEvent);
    waitresult := WaitForSingleObject(hLegacyRecvEvent,4000);
    if (waitresult = WAIT_TIMEOUT) then
      raise Exception.Create('Plugin '+dllName+' exception Legacy Loader timed out')
    else
    begin
      try
      result := strpas(@LegacySharedMem.LibraryResult[1]);
      except
        on E: Exception do
          raise Exception.Create('Plugin '+dllName+' Legacy Loader had an exception fetching info: '
            + E.Message);
      end;
    end;
  end
  else
  {$IFEND}
  if (dlls[uiPlugin].bBridge) then
  begin
    if (@dlls[uiPlugin].bridgeFunc = nil) then
      Exit;
    try
      R := dlls[uiPlugin].bridgeInfoFunc( dlls[uiPlugin].iBridgeId);
      if length(R) > 0 then
        result := R;
    except
      on E: Exception do
      raise Exception.Create('Plugin '+dllName+' DNBridge had an exception fetching info: '
            + E.Message);
    end;
  end
  else
  if (Assigned(dlls[uiPlugin].infoFunc)) then
  begin
    try
      result := dlls[uiPlugin].infoFunc;
    except
      on E: Exception do
          raise Exception.Create('Plugin '+dllName+' had an exception fetching info: '
            + E.Message);
    end;
  end;
end;

function TData.GetPluginDemos(dllName: string) : String;
var
  {$IF Defined(CPUX64)}
  waitresult: integer;
  {$IFEND}
  uiPlugin: integer;
  R: string;
begin
  result := '';
  uiPlugin := FindPlugin(dllName);
  {$IF Defined(CPUX64)}
  if dlls[uiPlugin].legacyDll then
  begin
    LegacySharedMem^.LibraryID := dlls[uiPlugin].iBridgeId;
    LegacySharedMem^.LibraryFunc := demoFuncId;
    //LegacySharedMem^.LibraryParam1 := inttostr(DemoNum);
    setevent(hLegacyCallFunctionEvent);
    waitresult := WaitForSingleObject(hLegacyRecvEvent,4000);
    if (waitresult = WAIT_TIMEOUT) then
      raise Exception.Create('Plugin '+dllName+' exception Legacy Loader timed out')
    else
    begin
      try
      result := strpas(@LegacySharedMem.LibraryResult[1]);
      except
        on E: Exception do
          raise Exception.Create('Plugin '+dllName+' Legacy Loader had an exception fetching demos: '
            + E.Message);
      end;
    end;
  end
  else
  {$IFEND}
  if (dlls[uiPlugin].bBridge) then
  begin
    if (@dlls[uiPlugin].bridgeFunc = nil) then
      Exit;

    try
      R := dlls[uiPlugin].bridgeDemoFunc(dlls[uiPlugin].iBridgeId);
      if length(R) > 0 then
      result := R;
    except
      on E: Exception do
          raise Exception.Create('Plugin '+dllName+' DNBridge had an exception fetching demos: '
            + E.Message);
    end;


  end
  else
  if (Assigned(dlls[uiPlugin].demoFunc)) then
  begin
    try
      result := dlls[uiPlugin].demoFunc;
    except
      on E: Exception do
          raise Exception.Create('Plugin '+dllName+' had an exception fetching demos: '
            + E.Message);
    end;
  end;
end;

end.

