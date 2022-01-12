library wanip;

{$MODE Delphi}

 //by vcorp :D

uses
  Windows, SysUtils, Classes, Wininet;

{$R *.res}

// Smartie will call this when the plugin is 1st loaded
// This function is optional
Procedure SmartieInit; stdcall;
begin
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
	result := 900000; // ms
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
if (Pos('http://', LowerCase(url)) = 0) then URL := 'http://' + URL;
hSession := InternetOpen('My_App;)', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
try
hURL := InternetOpenURL(hSession, PChar(URL), nil, 0, INTERNET_FLAG_RELOAD, 0);
try
repeat
InternetReadFile(hURL, @Buffer, 1024, BufferLength);
Stream.WriteBuffer(Buffer, BufferLength);
// Application.Processmessages;
until (BufferLength = 0);
Result := Stream.DataString;
finally
InternetCloseHandle(hURL)
end;
finally
InternetCloseHandle(hSession);
Stream.free;
end;
end;
// ###########################################################



//Leggo il contenuto del memo e faccio il parsing per ottenere solo L'ip
function function1(param1:pchar;param2:pchar):pchar; stdcall;
var rimpiazza, htm :string;
    conta     :integer;
    infondo   :boolean;
begin
  try
infondo:=false;
htm := LoadFromURL('http://checkip.dyndns.org');
conta:=1;
if copy(htm, 76,1) = ' '
then begin
repeat
rimpiazza := copy(htm, 77,conta);
inc(conta);
if copy(htm, 76+conta,1) = '<' then infondo:=true;
until infondo;
end;
result := PChar(rimpiazza);
exit;
// #############################################################

  except
    on E: Exception do
      result := PChar('plugin had exception: ' + E.Message);
  end;
end;

// don't forget to export the funtions, else nothing works :)
exports
  function1,
  SmartieInit,
  SmartieFini,
  GetMinRefreshInterval;

begin
end.
