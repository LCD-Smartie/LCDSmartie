library speedfan;

{$MODE Delphi}

uses
  SysUtils, Classes, Windows;

const
  SHARED_MEMORY_NAME = 'SFSharedMemory_ALM';

type
    TSharedMem=packed record
      version:word;
      flags :word;
      MemSize:integer;
      handle :THandle;
      NumTemps:word;
      NumFans :word;
      NumVolts:word;
      temps:array[0..31] of integer;
      fans :array[0..31] of integer;
      volts:array[0..31] of integer;
    end;

    PTSharedMem=^TSharedMem;

var
  myHandle: Cardinal;
  memory: PTSharedMem;
  localeFormat : TFormatSettings;


{$R *.res}

Function MapMemory: Boolean;
begin
  result := false;

  if (myHandle > 0) and (memory <> nil) then result := true
  else
  begin

    myHandle := OpenFileMapping(FILE_MAP_READ, False, SHARED_MEMORY_NAME);
    if myHandle > 0 then
    begin
      memory := MapViewOfFile(myHandle, FILE_MAP_READ, 0, 0, 0);
      if (memory <> nil) then
          result := true
      else
      begin
          FileClose(myHandle); { *Converted from CloseHandle* }
          myHandle := 0;
      end;
    end;

  end;
end;


// Smartie will call this when the plugin is 1st loaded
// This function is optional
Procedure SmartieInit; stdcall;
begin
  myHandle := 0;
  memory := nil;
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, localeFormat);
end;

// Smartie will call this just before the plugin is unloaded
// This function is optional
Procedure SmartieFini; stdcall;
begin
  if (myHandle > 0) then
  begin
    if (memory <> nil) then
    begin
       UnMapViewOfFile(memory);
       memory := nil;
    end;
    FileClose(myHandle); { *Converted from CloseHandle* }
    myHandle := 0;
  end;
end;

// Define the minimum interval that a screen should get fresh data from our
// plugin.
// The actual value used by Smartie will be the higher of this value and
// of the 'dll check interval' setting
// on the Misc tab.  [This function is optional, Smartie will assume
// 300ms if it is not provided.]
Function GetMinRefreshInterval: Integer; stdcall;
begin
	result := 300; // 300 ms
end;

// Fetch fan speeds
Function function1(param1:pchar;param2:pchar):pchar; stdcall;
var
  fan: Integer;
begin
  try
    if (MapMemory()) then
    begin

      fan := StrToInt(param1);
      if (fan > 0) and (fan <= memory^.NumFans) then
        result := PChar(IntToStr(memory^.fans[fan-1]))
      else
        result := 'no such fan';
    end
    else
      result := 'speedfan not running';

  except
    on E: Exception do
      result := PChar('plugin had exception: ' + E.Message);
  end;
end;

// Fetch temps
Function function2(param1:pchar;param2:pchar):pchar; stdcall;
var
  temp: Integer;
  precison: Integer;
begin
  try
    if (MapMemory()) then
    begin

      temp := StrToInt(param1);
      precison:= StrToInt(param2);

      if (temp > 0) and (temp <= memory^.NumTemps) then
        result := PChar(FloatToStrF(memory^.temps[temp-1] / 100,
                                  ffFixed, 18, precison, localeFormat))
      else
        result := 'no such temp';
    end
    else
      result := 'speedfan not running';

  except
    on E: Exception do
      result := PChar('plugin had exception: ' + E.Message);
  end;
end;

// Fetch volts
Function function3(param1:pchar;param2:pchar):pchar; stdcall;
var
  volt: Integer;
  precison: Integer;
begin
  try
    if (MapMemory()) then
    begin

      volt := StrToInt(param1);
      precison:= StrToInt(param2);

      if (volt > 0) and (volt <= memory^.NumVolts) then
        result := PChar(FloatToStrF(memory^.volts[volt-1] / 100,
                                  ffFixed, 18, precison, localeFormat))
      else
        result := 'no such volt';
    end
    else
      result := 'speedfan not running';

  except
    on E: Exception do
      result := PChar('plugin had exception: ' + E.Message);
  end;
end;


// don't forget to export the funtions, else nothing works :)
exports
  function1,
  function2,
  function3,
  SmartieInit,
  SmartieFini,
  GetMinRefreshInterval;
begin
end.

