library wanip;

{$MODE Delphi}

 //by vcorp :D
 // Converted to threaded model by Stokie-Ant

uses
  Windows, SysUtils, Classes, Wininet, dateutils;

var
  IPADDR: string;
  ThreadRunning: boolean;
  LastUpdate: TDateTime;
  RefreshInterval: integer;
// Smartie will call this when the plugin is 1st loaded
// This function is optional
Procedure SmartieInit; stdcall;
begin
  RefreshInterval := 900000;
end;

// Smartie will call this just before the plugin is unloaded
// This function is optional
Procedure SmartieFini; stdcall;
begin
end;

// Define the minimum interval that a screen should get fresh data from our
// plugin.
// The actual value used by Smartie will be the higher of this value and
// of the 'dll check interval' setting
// on the Misc tab.  [This function is optional, Smartie will assume
// 300ms if it is not provided.]
Function GetMinRefreshInterval: Integer; stdcall;
begin
  result := 300; // ms
end;

// mi carico la pagina htm nel memo
function LoadFromURL(URL: String): String;
var hSession, hURL: hInternet;
    Buffer: array[0..1023] of Byte;
    BufferLength: DWord;
    Stream: TStringStream;
begin
  stream := TStringStream.Create('');
  Result := '';
  if (Pos('http://', LowerCase(url)) = 0) then
    URL := 'http://' + URL;
  try
    hSession := InternetOpen('My_App;)', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

    if hSession = nil then
      Exit;

    hURL := InternetOpenURL(hSession, PChar(URL), nil, 0, INTERNET_FLAG_RELOAD, 0);

    if hURL = nil then
      Exit;

    repeat
      InternetReadFile(hURL, @Buffer, 1024, BufferLength);
      Stream.WriteBuffer(Buffer, BufferLength);
    until (BufferLength = 0);
      Result := Stream.DataString;

  finally
    InternetCloseHandle(hURL);
    InternetCloseHandle(hSession);
    Stream.free;
  end;
end;
// ###########################################################
procedure run;
var
  rimpiazza, htm :string;
  conta     :integer;
  infondo   :boolean;
begin
  ThreadRunning := true;
  LastUpdate := Now();
  try
    infondo:=false;
    htm := LoadFromURL('http://checkip.dyndns.org');
    conta:=1;
    if copy(htm, 76,1) = ' ' then
    begin
      repeat
        rimpiazza := copy(htm, 77,conta);
        inc(conta);
        if copy(htm, 76+conta,1) = '<' then
          infondo:=true;
      until
        infondo;
    end;
    IPADDR := rimpiazza;
  except
  end;
  ThreadRunning := false;
end;

//Leggo il contenuto del memo e faccio il parsing per ottenere solo L'ip
function function1(param1:pchar;param2:pchar):pchar; stdcall;
var
  thread: TThread;
begin
  try
    RefreshInterval := strtoint(param1)*1000*60;
  except
  end;
  RefreshInterval := max(RefreshInterval, 30000);
  if not ThreadRunning and (MilliSecondsBetween(Now(), LastUpdate) > RefreshInterval) then
    thread := TThread.ExecuteInThread(@run);
  result := pchar(IPADDR);
end;

function SmartieDemo():pchar; stdcall;
begin
  result := PChar('Get Wan IP Address' +#10#13+
                  '$dll(wanip,1,[interval],0)' +#10#13+
                  'Default interval (15 minutes)' +#10#13+
                  '$dll(wanip,1,0,0)' +#10#13+
                  'configure interval (10 minutes)' +#10#13+
                  '$dll(wanip,1,10,0)');
end;

// don't forget to export the funtions, else nothing works :)
exports
  function1,
  SmartieInit,
  SmartieFini,
  SmartieDemo,
  GetMinRefreshInterval;

begin
end.
