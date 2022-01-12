library wmpblog;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{ wmp bloggin plugin interface

  DLL functions:

  Function 1:  first param = which value  second param = ignored

    value
       0 = Author
       1 = Album
       2 = Duration
       3 = Name
       4 = Title

  Function 2: both params ignored

     wmp active (1) or not active (0)  used for actions screen to skip
     wmp screen in LCD Smartie setup

}

uses
  SysUtils,
  Classes,
  WMPREG;

{$R *.res}

// Smartie will call this when the plugin is 1st loaded
// This function is optional
Procedure SmartieInit; stdcall;
begin
  StartRegThread;
end;

// Smartie will call this just before the plugin is unloaded
// This function is optional
Procedure SmartieFini; stdcall;
begin
  EndRegThread;
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

function function1(param1:pchar;param2:pchar):pchar; stdcall;
var
  Func : byte;
begin
  Result := pchar(#0);
  try
    if assigned(DataLock) then begin
      Func := StrToInt(param1);
      DataLock.Enter;
      try
        try
          case Func of
            0 : Result := pchar(string(Author));
            1 : Result := pchar(string(Album));
            2 : Result := pchar(string(Duration));
            3 : Result := pchar(string(Name));
            4 : Result := pchar(string(Title));
          end;
        except
          on E: Exception do
            result := PChar('wmpblog.dll exception: ' + E.Message);
        end;
      finally
        DataLock.Leave;
      end;
    end;
  except
    on E: Exception do
      result := PChar('wmpblog.dll exception: ' + E.Message);
  end;
end;

function function2(param1:pchar;param2:pchar):pchar; stdcall;
begin
  Result := pchar('0'+#0);
  try
    if assigned(DataLock) then begin
      DataLock.Enter;
      try
        if (Author <> '') or (Album <> '') or (Duration <> '') or
           (Name <> '') or (Title <> '') then
          Result := pchar('1'+#0);
      finally
        DataLock.Leave;
      end;
    end;
  except
    on E: Exception do
      result := PChar('wmpblog.dll exception: ' + E.Message);
  end;
end;

exports
  function1,
  function2,
  SmartieInit,
  SmartieFini,
  GetMinRefreshInterval;
begin
end.


