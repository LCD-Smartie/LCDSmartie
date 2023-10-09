unit UUtils;

interface

uses
  SysUtils, SyncObjs, Classes;

var
  localeFormat : TFormatSettings;

const
  maxArgs = 10;
  ticksperseconde = 1000;
  ticksperminute = ticksperseconde * 60;
  ticksperhour = ticksperminute * 60;
  ticksperdag = ticksperhour * 24;
  ticksperweek = ticksperdag * 7;
  tickspermonth: Int64 = Int64(ticksperdag) * 30;
  ticksperyear: Int64 = Int64(ticksperdag) * 30 * 12;


type
  EExiting = Class(Exception);

type
  TThreadMethod = procedure of object;
  {$M+}
  TMyThread = class(TTHREAD)
  private
    method: TThreadMethod;

  published

    procedure Execute; override;
  public
    exited: TEvent;
    property Terminated;
    constructor Create(myMethod: TThreadMethod);
    destructor Destroy;  override;
  end;
   {$M-}
Function CenterText(const sLine: String; iWidth: Integer): String;
//procedure AddPluginsToPath;
procedure CreateShortcut(const sName, FileName,Args: string; uninstall: Boolean = False);
procedure SetupSchedulerAutoStart(const sName, FileName,Args: string; asAdmin: Boolean; uninstall: Boolean = False);
function IsAdministrator: Boolean;
function errMsg(uError: Cardinal): String;
function decodeArgs(const str: String; const funcName: String; maxargs: Cardinal; var
    args: Array of String; var prefix: String; var postfix: String; var
    numArgs: Cardinal): Boolean;
function StrToIntN(const sStr: String; iStart: Integer; iSize: Integer): Integer;
function StrToFloatN(const sStr: String; iStart: Integer; iSize: Integer): double;

function CleanString(str: String): String;
function FileToString(sFilename: String): String;
function stripspaces(FString: String): String;
procedure RequiredParameters(uiArgs: Cardinal; uiMinArgs: Cardinal; uiMaxArgs: Cardinal = 0);
function stripHtml(str: String): String;


implementation

uses
  Windows, Registry, ShlObj, ActiveX, ComObj, Forms, StrUtils;

constructor TMyThread.Create(myMethod: TThreadMethod);
begin
  method := myMethod;
  exited := TEvent.Create(nil, true, false, '');
  inherited Create(true);   // Create suspended.
end;

destructor TMyThread.Destroy;
begin
  exited.Free();
  inherited;
end;

procedure TMyThread.Execute;
begin
  method();
  exited.SetEvent();
end;

function StrToIntN(const sStr: String; iStart: Integer; iSize: Integer): Integer;
var
  num: Integer;
  i: Integer;
begin
  num := 0;
  // skip space space
  while (iSize > 0) and (sStr[iStart] = ' ') do
  begin
    iStart := iStart + 1;
    iSize := iSize - 1;
  end;

  if (iSize <= 0) then
    raise Exception.Create('invalid number');

  for i := iStart to iStart+iSize-1 do
  begin
    if (sStr[i] < '0') or (sStr[i] > '9') then
      raise Exception.Create('invalid number: ' + MidStr(sStr,iStart,iSize));
    num := num * 10 + Ord(sStr[i]) - Ord('0');
  end;
  Result := num;
end;

function StrToFloatN(const sStr: String; iStart: Integer; iSize: Integer): double;
var
  iDecPoint: Integer;
begin
  iDecPoint := PosEx('.', sStr, iStart);

  if (iDecPoint = 0) or (iDecPoint > iStart+iSize) then
    Result := StrToIntN(sStr, iStart, iSize)
  else
    Result := StrToIntN(sStr, iStart, iDecPoint-iStart)
      + StrToIntN(sStr, iDecPoint+1, (iStart+iSize)-(iDecPoint+1)) /
      (10.0 * ((iStart+iSize)-(iDecPoint+1)));

end;

function errMsg(uError: Cardinal): String;
var
  psError: pointer;
  sError: String;
begin
  if (uError <> 0) then
  begin
    if (FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
      nil, uError, 0, @psError, 0, nil ) = 0) then psError := nil;

    if (psError <> nil) then
    begin
      sError := '#' + IntToStr(uError) + ': ' + PChar(psError);
      LocalFree(Cardinal(psError));
    end
    else
      sError := '#' + IntToStr(uError);
    Result := sError;
  end
  else
    Result := '#0'; // don't put "operation completed successfully!" It's too confusing!
end;



// Takes a string like: 'C:$Bar(20,30,10) jterlktjer(fsdfs)sfsdf(sdf)'
// with funcName '$Bar'
// and returns true(found) and numArgs=3 and an array with: '20', '30', '10'
// postfix=' jterlktjer(fsdfs)sfsdf(sdf)'
// prefix='C:'
// Return false if any args contain $variables allowing for another run at resolveVariables
function decodeArgs(const str: String; const funcName: String; maxargs: Cardinal;
  var args: Array of String; var prefix: String; var postfix: String; var
  numArgs: Cardinal): Boolean;
var
  posParen, posFuncStart, posArgsStart, posArgsEnd, posComma, posComma2: Integer;
  posTemp: Integer;
  uiLevel: Cardinal;
  iStrLen, i: Integer;
begin
  Result := true;
  numArgs := 0;
  iStrLen := Length(str);

  // For speed reasons this is instead of: posFuncStart = Pos(funcName + '(', str):
  posFuncStart := 0;
  repeat
    posFuncStart := PosEx(funcName, str, posFuncStart+1);
    posParen := posFuncStart + length(funcName);
    if (posFuncStart > 0) and
       (posParen <= iStrLen) and
       (str[posParen] = '(') then break;
  until (posFuncStart = 0);

  if (posFuncStart <> 0) then
  begin
    posArgsStart := posFuncStart + length(funcName);

    // find end of function and cope with nested brackets
    posTemp := posArgsStart + 1;
    uiLevel := 1;
    repeat
      case (str[posTemp]) of
      '(': Inc(uiLevel);
      ')': Dec(uiLevel);
      end;
      Inc(posTemp);
    until (uiLevel = 0) or (posTemp > iStrLen);

    if (uiLevel = 0) then
      posArgsEnd := posTemp-1
    else
      posArgsEnd := iStrLen;

    prefix := AnsiMidStr(str, 1, posFuncStart-1);
    postfix := AnsiMidStr(str, posArgsEnd + 1, iStrLen-posArgsEnd + 1);

    if (posArgsStart <> 0) and (posArgsEnd <> 0) then
    begin
      Dec(posArgsEnd);

      // between posArgsStart+1 and posArgsEnd  is now something like: '20,30,10'
      posComma2 := posArgsStart;
      repeat
        // Find next comma ignoring those in brackets.
        uiLevel := 0;
        posComma := posComma2;
        repeat
          Inc(posComma);
          case (str[posComma]) of
          '(': Inc(uiLevel);
          ')': Dec(uiLevel);
          end;
        until (posComma >= posArgsEnd)
          or ((uiLevel = 0) and (str[posComma] = ','));

        if (posComma >= posArgsEnd) then
        begin
          if (str[posComma] = ',') then
          //cope with last parameter being empty
          begin
            args[numArgs] := AnsiMidStr(str, posComma2+1, posComma-(PosComma2+1));
            Inc(numArgs);

            if (numArgs < maxArgs) then
            begin
              args[numArgs] := '';
              Inc(numArgs);
            end;
          end
          else
          begin
            args[numArgs] := AnsiMidStr(str, posComma2+1, posComma-PosComma2);
            Inc(numArgs);
          end;
        end
        else
        begin
          args[numArgs] := AnsiMidStr(str, posComma2+1, PosComma-(PosComma2+1));
          Inc(numArgs);
        end;
        posComma2 := posComma;
      until (poscomma >= posArgsEnd) or (numArgs >= maxArgs);
    end;
  end
  else Result := false;

end;


// ** This code was posted on http://www.experts-exchange.com by 'inthe'
// ** which was based on code by 'madshi'.
procedure CreateShortcut(const sName, FileName,Args: string; uninstall: Boolean = False);
var
  LPUnknown : IUnknown;
  pShlLnk : IShellLink;
  pszFileName : IPersistFile;
  Dir : string;
  FullPath : WideString;
  R : TRegistry;
begin
  LPUnknown := CreateComObject(CLSID_ShellLink);
  pShlLnk := LPUnknown as IShellLink;
  pszFileName := LPUnknown as IPersistFile;
  pShlLnk.SetPath(PChar(FileName));
  pShlLnk.SetArguments(PChar(Args));
  pShlLnk.SetDescription(PChar('Automatically created by LCD Smartie'));
  pShlLnk.SetWorkingDirectory(PChar(ExtractFilePath(FileName)));

  R := TRegistry.Create(KEY_READ);
  R.RootKey := HKEY_CURRENT_USER;
  R.OpenKeyReadOnly('Software\MicroSoft\Windows\CurrentVersion\Explorer\Shell Folders\');
  try
    Dir := R.ReadString('Startup');
    if Dir <> '' then
    begin
      FullPath := Dir + '\'+sName+'.lnk';
      if uninstall then
        SysUtils.DeleteFile(FullPath)
      else
        pszFileName.Save(PWChar(FullPath), False);
    end;
  finally
    R.Free;
  end;
end;

procedure SetupSchedulerAutoStart(const sName, FileName, Args: string; asAdmin: Boolean; uninstall: Boolean = False);
var
  service,rootfolder,taskdefinition,reginfo,principal,settings,triggers,
  trigger,action_:Olevariant;
begin
  if not uninstall then
  begin
    service:=CreateOleObject('Schedule.Service');
    service.connect;
    rootfolder:=service.GetFolder(Olevariant('\'));
    taskdefinition:=service.NewTask(0);
    reginfo:=taskdefinition.RegistrationInfo;
    reginfo.Description:='LCDSmartieAutoStart';
    reginfo.Author:='LCDSmartie';
    principal:=taskdefinition.Principal;
    principal.LogonType:=3; // TASK_LOGON_INTERACTIVE_TOKEN

    if (asAdmin) then
      principal.RunLevel := 1; // TASK_RUNLEVEL_HIGHEST

    settings:=taskdefinition.Settings;
    settings.Enabled:=true;
    settings.StartWhenAvailable:=true;
    settings.Hidden:=False;
    settings.RunOnlyIfIdle:=False;
    settings.DisallowStartIfOnBatteries := False;
    settings.StopIfGoingOnBatteries := False;
    settings.ExecutionTimeLimit:=olevariant('PT0S'); // infinite execution time
    triggers:=taskdefinition.Triggers;
    trigger:=triggers.Create(9); // TASK_TRIGGER_LOGON
    trigger.Id:=olevariant(sName);
    trigger.Enabled:=true;
    action_:=taskdefinition.Actions.Create(0);
    action_.Path:=olevariant(FileName);
    action_.Arguments := olevariant(Args);
    action_.WorkingDirectory := olevariant(PChar(ExtractFilePath(FileName)));
    rootfolder.RegisterTaskDefinition(olevariant(sName), taskdefinition, 6, NULL, NULL, 3);
  end
  else
  begin
    service:=CreateOleObject('Schedule.Service');
    service.connect;
    rootfolder:=service.GetFolder(Olevariant('\'));
    try
      rootfolder.DeleteTask(Olevariant(sName), 0)
    except
      on E: Exception do; // catch this
    end;
  end;
end;

// Function to check for adminstrator privileges
function CheckTokenMembership(TokenHandle: THANDLE; SidToCheck: Pointer; var IsMember: BOOL): BOOL; stdcall; external advapi32 name 'CheckTokenMembership';

function IsAdministrator: Boolean;
var
  psidAdmin: Pointer;
  B: BOOL;
const
  SECURITY_NT_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID  = $00000020;
  DOMAIN_ALIAS_RID_ADMINS      = $00000220;
  {%H-}SE_GROUP_USE_FOR_DENY_ONLY  = $00000010;
begin
  psidAdmin := nil;
  try
    Win32Check(AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
      SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0,
      psidAdmin));

    if CheckTokenMembership(0, psidAdmin, B{%H-}) then
      Result := B
    else
      Result := False;
  finally
    if psidAdmin <> nil then
      FreeSid(psidAdmin);
  end;
end;

{ // not sure this is neccesary. plus it excepts when not admin 
procedure AddPluginsToPath;
var
  Reg: TRegistry;
  sName: String;
begin
  try
    Reg := TRegistry.Create;

    sName := 'Software\Microsoft\Windows\CurrentVersion\App Paths\' +
      extractfilename(application.exename);

    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKey(sName, true) then
      begin
        Reg.WriteString('Path',
          extractfilepath(application.exename)+'plugins');
        Reg.CloseKey;
      end;
    finally
      Reg.Free;
    end;
  except
  end;
end;
}

Function CenterText(const sLine: String; iWidth: Integer): String;
var
  sSpace: String;
begin
  if (length(sLine) < iWidth) then
  begin
    sSpace := DupeString(' ', iWidth - length(sLine) );
    Insert(sLine, sSpace, (Length(sSpace) div 2)+1);

    result := sSpace;
  end
  else
    result := sLine;
end;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////                                                                       ////
////          U T I L I T Y        F U N C T I O N  S                      ////
////                                                                       ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


// Remove $'s from the string - this is used when an exception
// message is inserted into the parsed string. This avoids
// any chance of infinite recursion.
function CleanString(str: String): String;
begin
  Result := StringReplace(str, '$', '', [rfReplaceAll]);
end;

function FileToString(sFilename: String): String;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    try
      sl.LoadFromFile(sFilename);
      Result := sl.Text;
    except
      on E: Exception do Result := '[Exception: ' + E.Message + ']';
    end;
  finally
    sl.Free;
  end;
end;

function stripspaces(FString: String): String;
begin
  FString := StringReplace(FString, chr(10), '', [rfReplaceAll]);
  FString := StringReplace(FString, chr(13), '', [rfReplaceAll]);
  FString := StringReplace(FString, chr(9), ' ', [rfReplaceAll]);

  while copy(fString, 1, 1) = ' ' do
  begin
    fString := copy(fString, 2, length(fString));
  end;
  while copy(fString, length(fString), 1) = ' ' do
  begin
    fString := copy(fString, 1, length(fString)-1);
  end;

  result := fString;
end;


function stripHtml(str: String): String;
var
  posTag, posTagEnd: Cardinal;
begin
  //LMB: this is not the best place to add this, but I have to make it work quickly:
  str := StringReplace(str,'&deg;',#176{'°'},[rfIgnoreCase,rfReplaceAll]);
  //LMB: <br> may be used as a separator, so instead of discarding, replace with space
  str := StringReplace(str,'<br>',#32{space},[rfReplaceAll]);

  repeat
    posTag := pos('<', str);
    if (posTag <> 0) then
    begin
      posTagEnd := posEx('>', str, posTag + 1);
      if (posTagEnd <> 0) then Delete(str, posTag, posTagEnd-posTag + 1);
    end;

  until (posTag = 0);

  result := str;
end;

procedure RequiredParameters(uiArgs: Cardinal; uiMinArgs: Cardinal; uiMaxArgs: Cardinal = 0);
begin
  if (uiArgs < uiMinArgs) then
    raise Exception.Create('Too few parameters');
  if (uiArgs > uiMaxArgs) then
    raise Exception.Create('Too many parameters');
end;

begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, localeFormat);
end.

