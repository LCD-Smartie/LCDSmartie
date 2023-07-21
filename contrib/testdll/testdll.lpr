library testdll;

{$MODE Delphi}

uses
  Windows, SysUtils, Classes, Wininet;

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
  demolist.Add(''); // empty string to mark the end
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

function SmartieInfo(param1:pchar):pchar; stdcall;
begin
  case (Param1[0]) of
    'd': result := PChar('Stokie-Ant');
    'v': result := PChar('1.0');
  end;


end;

function SmartieDemo(param1:integer):pchar; stdcall;
var
  S: String;
begin
  // many ways this can be done. Get creative
  // Using case statement
  {case (param1) of
       0: S := 'Title';
       1: S := 'Funtion title';
       2: S := 'func 1 does this $dll(dll,1,2,3)';
       3: S := 'and so on...';
       4: S := ''; // empty string marks the end
  end;
  result := PChar(S);}

  // Using a TStringList
  if assigned(demolist) then
    result := pchar(demolist.Strings[param1]);
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
