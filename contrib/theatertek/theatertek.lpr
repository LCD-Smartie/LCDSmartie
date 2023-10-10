library theatertek;

{$MODE Delphi}

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{ theatertek interface

  DLL functions:

  Function 1:  first param = which value  second param = ignored

    value

       0 = File name
       1 = Title number
       2 = Chapter number
       3 = Play state (text)
       4 = Play speed
       5 = Aspect ratio
       6 = Audio channels
       7 = Audio format (text)
       8 = Audio bitrate (Kbps)
       9 = Video bitrate (Mbps)
      10 = Disk ID

  Function 2:  time on disc

    first param = which time  second param = format

       0 = Time
       1 = Time remaining
       2 = Total time

    format = hh:nn:ss

  Function 3: both params ignored

     play state using custom characters

  Function 4: both params ignored

     theatertek active (1) or not active (0)  used for actions screen to skip
     theatertek screen in LCD Smartie setup

}

uses
  Interfaces,
  SysUtils,
  Classes,
  TTINFO;

{$R *.res}

const
// bad legacy to have weird characters like this defined
  CC1 = #176;
  CC2 = #158;
  CC3 = #131;
  CC4 = #132;
  CC5 = #133;
  CC6 = #134;
  CC7 = #135;
  CC8 = #136;

  CustomChar1 = '$CustomChar(1,16,24,28,30,28,24,16,0)'; // play
  CustomChar2 = '$CustomChar(2,27,27,27,27,27,27,27,0)'; // pause
  CustomChar3 = '$CustomChar(3,0,31,31,31,31,31,0,0)';   // stop
  CustomChar4 = '$CustomChar(4,1,3,7,15,7,3,1,0)';       // rew
  CustomChar5 = '$CustomChar(5,17,25,29,31,29,25,17,0)'; // next
  CustomChar6 = '$CustomChar(6,17,19,23,31,23,19,17,0)'; // prev

  AllChars = CustomChar1 + CustomChar2 + CustomChar3 + CustomChar4;

// Smartie will call this when the plugin is 1st loaded
// This function is optional
Procedure SmartieInit; stdcall;
begin
  TTINFOForm := TTTINFOForm.Create(nil);
end;

// Smartie will call this just before the plugin is unloaded
// This function is optional
Procedure SmartieFini; stdcall;
begin
  TTINFOForm.Free;
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
  S : string;
begin
  Result := pchar(#0);
  try
    if assigned(TTINFOForm) and (TTINFOForm.TTWnd <> 0) then begin
      Func := StrToInt(param1);
      with sData do begin
        case Func of
          0 : Result := pchar(string(szFileName));
          1 : Result := pchar(IntToStr(nTitle));
          2 : Result := pchar(IntToStr(nChapter));
          3 : begin
            S := GRAPH_STATE_STRING[nState];
            if (nState = gsScanning) then begin
              if (nSpeed < 0) then S := 'REW'
              else S := 'FF';
            end;
            Result := pchar(S);
          end;
          4 : Result := pchar(IntToStr(abs(nSpeed)));
          5 : Result := pchar(string(szAR));
          6 : Result := pchar(IntToStr(nChannels));
          7 : begin
            S := AUDIO_FORMAT_STRING[nAudioFormat];
            Result := pchar(S);
          end;
          8 : Result := pchar(IntToStr(nAudBitrate div 1024));
          9 : Result := pchar(FormatFloat('##0.0',nVidBitrate/1024000));
         10 : Result := pchar(IntToHex(nDiskID,16));
        end;
      end;
    end;
  except
    on E: Exception do
      result := PChar('theatertek.dll exception: ' + E.Message);
  end;
end;

function function2(param1:pchar;param2:pchar):pchar; stdcall;
var
  Func : byte;
  S : string;
  TT,TR,CT : TDateTime;
begin
  Result := pchar(#0);
  try
    if assigned(TTINFOForm) and (TTINFOForm.TTWnd <> 0) then begin
      Func := StrToInt(param1);
      S := param2;
      with sData do begin
        with TotalTime do
          TT := BHours / 24 + BMinutes / (24*60) + bSeconds / (24*60*60);
        with CurTime.TimeCode do
          CT := BHours / 24 + BMinutes / (24*60) + bSeconds / (24*60*60);
        TR := TT - CT;
        case Func of
          0 : Result := pchar(FormatDateTime(S,CT));
          1 : Result := pchar(FormatDateTime(S,TR));
          2 : Result := pchar(FormatDateTime(S,TT));
        end;
      end;
    end;
  except
    on E: Exception do
      result := PChar('theatertek.dll exception: ' + E.Message);
  end;
end;

function function3(param1:pchar;param2:pchar):pchar; stdcall;
var
  S : string;
begin
  Result := pchar(#0);
  try
    if assigned(TTINFOForm) and (TTINFOForm.TTWnd <> 0) then begin
      with sData do case nState of
        gsUninitialized,
        gsGraph_Stopped1,
        gsGraph_Stopped2,
        gsNav_Stopped : S := CC3;
        gsPlaying : S := CC1;
        gsGraph_Paused,
        gsNav_Paused : S := CC2;
        gsScanning : begin
          if (nSpeed < 0) then S := CC4+CC4
          else S := CC1+CC1;
        end;
      end;
      Result := pchar(AllChars+S);
    end;
  except
    on E: Exception do
      result := PChar('theatertek.dll exception: ' + E.Message);
  end;
end;

function function4(param1:pchar;param2:pchar):pchar; stdcall;
begin
  Result := pchar('0'+#0);
  try
    if assigned(TTINFOForm) and (TTINFOForm.TTWnd <> 0) then begin
      Result := pchar('1'+#0);
    end;
  except
    on E: Exception do
      result := PChar('theatertek.dll exception: ' + E.Message);
  end;
end;

exports
  function1,
  function2,
  function3,
  function4,
  SmartieInit,
  SmartieFini,
  GetMinRefreshInterval;
begin
end.


