library testdll;

{$MODE Delphi}

uses
  SysUtils, Classes;

var
  demolist: TStringList;

// Smartie will call this when the plugin is 1st loaded
// This function is optional
Procedure SmartieInit; stdcall;
begin
  demolist := TStringList.Create;
  demolist.Add('----function 1----');
  demolist.Add('> Adds two numbers <');
  demolist.Add('2+3 $dll(testdll,1,2,3)');
  demolist.Add('5+7 $dll(testdll,1,5,7)');
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
//Function GetMinRefreshInterval: Integer; stdcall;
//begin
//	result := 900000; // ms
//end;

function function1(param1:pchar;param2:pchar):pchar; stdcall;
begin

result := pchar(inttostr(strtoint(param1) + strtoint(param2)));

end;

function SmartieInfo():pchar; stdcall;
begin
  result := PChar('Developer:Stokie-Ant'+#13#10+'Version:1.0');
end;

function SmartieDemo():pchar; stdcall;
begin
  // many ways this can be done. Get creative
  // Using a TStringList
  if assigned(demolist) then
    result := pchar(demolist.Text);
end;

// don't forget to export the funtions, else nothing works :)
exports
  function1,
  SmartieInit,
  SmartieFini,
  SmartieInfo,
  SmartieDemo;
  //GetMinRefreshInterval;

begin
end.
