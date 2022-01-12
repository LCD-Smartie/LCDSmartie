unit WinampCtrl;

{ Created by Stu Pidass = Lee_Nover }
{ I've used all of the functions I found on winamp , DO use the Control,
  maybe change it ? taka a look at the code for explanations and stuff...
  comments, suggestions, anything .. to Lee.Nover@Email.si }


interface

uses
  Windows, Messages, SysUtils, Classes, Dialogs,  ExtCtrls;

type
  TRunThread = class(TThread)
  public
    FileName: TFileName;
    Params: String;
    NewInstance: Boolean;
    procedure Execute;override;
  end;

  TWinampCtrl = class(TComponent)
  private
    // Cached results:
    lastresult_TrackLength: int64;
    lasttime_TrackLength: Cardinal;
    lastresult_TrackPosition: int64;
    lasttime_TrackPosition: Cardinal;
    lastresult_GetCurrSongTitle: String;
    lasttime_GetCurrSongTitle: Cardinal;
    lastresult_GetSongInfo: Array [0..2] of Integer;
    lasttime_GetSongInfo: Array [0..2] of Cardinal;
    lastresult_GetState: Integer;
    lasttime_GetState: Cardinal;
    lastresult_GetListPos: Integer;
    lasttime_GetListPos: Cardinal;
    lastresult_GetListCount: Integer;
    lasttime_GetListCount: Cardinal;

    hwndWinamp : HWND;
    lastFoundHwnd: Cardinal;
    FWinampLocation: TFileName;
    FParams: String;
    RunThread: TRunThread;
    FTitleList: TStringList;
    FFileNameList: TStringList;
    FLengthList: TStringList;
    FFreeLists: Boolean;
    FAlwaysLoadList: Boolean;
    FUseBalanceCorrection: Boolean;

    PlaylistPos: integer;
    FOnSongChanged: TNotifyEvent;
    procedure SetWinampLocation(Value: TFileName);
    function GetWAhwnd: Integer;
    Function MySendMessage(Msg: Cardinal; wParam: Integer; IParam: Integer): Integer;
  protected
    { Protected declarations }
    procedure LoadFileNameList;
    procedure LoadTitleList;
    procedure LoadLengthList;
  public
    { Public declarations }

    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;

    procedure CheckIfSongChanged;//(var Msg: TWMSetText);message WM_SETTEXT;
    function GetState : integer;
    function GetOutputTime(TimeMode : Integer) : Int64;
    function GetSongInfo(InfoMode : Integer) : Integer;
    function GetEQData(Position : Integer) : Integer;
    function GetListCount : Integer;
    function GetListPos : Integer;
    function GetCurrSongTitle : String;
    // from within a Plugin only -----------------------------------------------
    function GetSongFileName(Position: Integer) : String;
    function GetSongTitle(Position: Integer) : String;
    // getting info from WinAmps internal playlist -----------------------------
    function PlayListGetSongTitle(Position: Integer):String;
    function PlayListGetSongFileName(Position: Integer):String;
    function PlayListGetSongLength(Position: Integer):Integer;
    // aditional Functions -----------------------------------------------------
    function TrackPosition: Int64;
    function TrackLength: Int64;
    function SampleRate: integer;
    function Frequency: integer;
    function NofChannels: integer;
    procedure EQEnable(Enable: Boolean);
    procedure EQAutoLoad(Enable: Boolean);

    procedure SaveTitleListToFile(FileName: TFileName);
    procedure SaveFileNameListToFile(FileName: TFileName);
    procedure StartPlay;
    Procedure Previous;
    Procedure Play;
    Procedure Pause;
    Procedure Stop;
    Procedure Next;
    procedure Previous_Shift;
    procedure Play_Shift;
    procedure Stop_Shift;
    procedure Next_Shift;
    procedure Previous_Ctrl;
    procedure Play_Ctrl;
    procedure Next_Ctrl;
    procedure StopAfterCurrTrack;
    Procedure Hide;
    Procedure Show;
    procedure SetVolume(Volume : Byte);
    procedure SetPanning(Panning : Byte);
    procedure SetEQData(Position,Value : Integer);
    procedure SetListPos(Position : Integer);
    procedure SetTimeDisplay(Remaining: Boolean);
    procedure ToggleEQ;
    procedure TogglePlaylist;
    procedure ToggleAutoscrolling;
    procedure ToggleWindowShade;
    procedure TogglePlayListWindowShade;
    procedure ToggleDoubleSize;
    procedure ToggleMainVisible;
    procedure ToggleMiniBrowser;
    procedure ToggleEasyMove;
    procedure ToggleRepeat;
    procedure ToggleShufflE;
    procedure ToggleAlwaysOnTop;
    procedure VolumeUp;
    procedure VolumeDown;
    procedure FastForward_5s;
    procedure Rewind_5s;
    procedure PopUpPrefs;
    procedure OpenFileInfoBox;
    procedure JumpToTime;
    procedure JumpToFile;
    procedure LoadFiles;
    procedure RunWinAmp(NewInstance: Boolean);
    procedure CloseWinAmp;

    procedure Seek(Offset: Integer);// in milliseconds

  published
    { Published declarations }
    property AlwaysLoadList: Boolean read FAlwaysLoadList write FAlwaysLoadList default False;
    property FreeLists: Boolean read FFreeLists write FFreeLists default false;
    property UseBalanceCorrection: Boolean read FUseBalanceCorrection write FUseBalanceCorrection default True;
    property WinampLocation: TFileName read fWinampLocation write SetWinampLocation;
    property Params: String read FParams write FParams;

    property OnSongChanged: TNotifyEvent read FOnSongChanged write FOnSongChanged;
  end;

implementation



const
  WinampClassName = 'winamp v1.x';

constructor TWinampCtrl.Create(AOwner: TComponent);
begin
     inherited;
     FFileNameList:=TStringList.Create;
     FTitleList:=TStringList.Create;
     FLengthList:=TStringList.Create;
     FFreeLists:=true;
     FUseBalanceCorrection:=true;
end;

destructor TWinampCtrl.Destroy;
begin
     if Assigned(FFileNameList)then FFileNameList.Free;
     if Assigned(FTitleList)then FTitleList.Free;
     if Assigned(FLengthList)then FLengthList.Free;
     inherited;
end;

// WM_COMMAND messages ---------------------------------------------------------

Function TWinampCtrl.GetWAhwnd(): Integer;
var
  now: Cardinal;
begin
  now := GetTickCount();
  if (now < lastFoundHwnd) or ((now - lastFoundHwnd) > 250) then
  begin
    lastFoundHwnd := now;
    hwndWinamp := FindWindow(WinampClassName,nil);
  end;
  result := hwndWinamp;
end;

Function TWinampCtrl.MySendMessage(Msg: Cardinal; wParam: Integer; IParam: Integer): Integer;
var
  winamp: HWND;
begin
  winamp := GetWAhwnd();
  if (winamp <> 0) then
    Result := SendMessage(winamp, Msg, wParam, IParam)
  else
    Result := 0;
end;

Procedure TWinampCtrl.Previous;
begin
  MySendMessage(WM_COMMAND,40044,0);
end;

Procedure TWinampCtrl.Play;
begin
  MySendMessage(WM_COMMAND,40045,0);
end;

Procedure TWinampCtrl.Pause;
begin
  MySendMessage(WM_COMMAND,40046,0);
end;

Procedure TWinampCtrl.Stop;
begin
  MySendMessage(WM_COMMAND,40047,0);
end;

Procedure TWinampCtrl.Next;
begin
  MySendMessage(WM_COMMAND,40048,0);
end;

procedure TWinampCtrl.FastForward_5s;
begin
     MySendMessage(WM_COMMAND,40060,0);
end;

procedure TWinampCtrl.Rewind_5s;
begin
     MySendMessage(WM_COMMAND,40061,0);
end;

// start of playlist
procedure TWinampCtrl.Previous_Ctrl;
begin
     MySendMessage(WM_COMMAND,40154,0);
end;

// open location
procedure TWinampCtrl.Play_Ctrl;
begin
     MySendMessage(WM_COMMAND,40155,0);
end;

// end of playlist
procedure TWinampCtrl.Next_Ctrl;
begin
     MySendMessage(WM_COMMAND,40158,0);
end;

// rewind 5 seconds
procedure TWinampCtrl.Previous_Shift;
begin
     MySendMessage(WM_COMMAND,40144,0);
end;

// open file(s)
procedure TWinampCtrl.Play_Shift;
begin
     MySendMessage(WM_COMMAND,40145,0);
end;

// fade out and stop
procedure TWinampCtrl.Stop_Shift;
begin
     MySendMessage(WM_COMMAND,40147,0);
end;

// fast forward 5 sec0nds
procedure TWinampCtrl.Next_Shift;
begin
     MySendMessage(WM_COMMAND,40148,0);
end;

procedure TWinampCtrl.StopAfterCurrTrack;
begin
     MySendMessage(WM_COMMAND,40157,0);
end;

procedure TWinampCtrl.ToggleEQ;
begin
     MySendMessage(WM_COMMAND,40036,0);
end;

procedure TWinampCtrl.TogglePlaylist;
begin
     MySendMessage(WM_COMMAND,40040,0);
end;

procedure TWinampCtrl.VolumeUp;
begin
     MySendMessage(WM_COMMAND,40058,0);
end;

procedure TWinampCtrl.VolumeDown;
begin
     MySendMessage(WM_COMMAND,40059,0);
end;

procedure TWinampCtrl.ToggleAlwaysOnTop;
begin
     MySendMessage(WM_COMMAND,40019,0);
end;

procedure TWinampCtrl.PopUpPrefs;
begin
     MySendMessage(WM_COMMAND,40012,0);
end;

procedure TWinampCtrl.OpenFileInfoBox;
begin
     MySendMessage(WM_COMMAND,40188,0);
end;

procedure TWinampCtrl.LoadFiles;
begin
     MySendMessage(WM_COMMAND,40029,0);
end;

procedure TWinampCtrl.SetTimeDisplay(Remaining: Boolean);
begin
     MySendMessage(WM_COMMAND,40037+Integer(Remaining),0)
end;

procedure TWinampCtrl.ToggleAutoscrolling;
begin
     MySendMessage(WM_COMMAND,40189,0);
end;

procedure TWinampCtrl.ToggleWindowShade;
begin
     MySendMessage(WM_COMMAND,40064,0);
end;

procedure TWinampCtrl.TogglePlayListWindowShade;
begin
     MySendMessage(WM_COMMAND,40266,0);
end;

procedure TWinampCtrl.ToggleDoubleSize;
begin
     MySendMessage(WM_COMMAND,40165,0);
end;

procedure TWinampCtrl.ToggleMainVisible;
begin
     MySendMessage(WM_COMMAND,40258,0);
end;

procedure TWinampCtrl.ToggleMiniBrowser;
begin
     MySendMessage(WM_COMMAND,40298,0);
end;

procedure TWinampCtrl.ToggleEasyMove;
begin
     MySendMessage(WM_COMMAND,40186,0);
end;

procedure TWinampCtrl.ToggleRepeat;
begin
     MySendMessage(WM_COMMAND,40022,0);
end;

procedure TWinampCtrl.ToggleShufflE;
begin
     MySendMessage(WM_COMMAND,40023,0);
end;

procedure TWinampCtrl.JumpToFile;
begin
     MySendMessage(WM_COMMAND,40194,0);
end;

procedure TWinampCtrl.JumpToTime;
begin
     MySendMessage(WM_COMMAND,40193,0);
end;

procedure TWinampCtrl.CloseWinAmp;
begin
     MySendMessage(WM_COMMAND,40001,0);
end;

// WM_USER messages ------------------------------------------------------------

procedure TWinampCtrl.StartPlay;
begin
     MySendMessage(WM_USER,0,102);
end;

procedure TWinampCtrl.Seek(Offset: Integer);
begin
     MySendMessage(WM_USER,Offset,106);
end;

function TWinampCtrl.GetState: Integer;
var
  now: Cardinal;
begin
  now := GetTickCount();

  if (now < lasttime_GetState) or ((now - lasttime_GetState) > 250) then
  begin
    lasttime_GetState := now;

    lastresult_GetState := MySendMessage(WM_USER,0,104);
  end;

  Result := lastresult_GetState;
     {  States :
         0: Stopped
         1: Playing
         3: Paused }
end;

function TWinampCtrl.GetListCount: Integer;
var
  now: Cardinal;
begin
  now := GetTickCount();
  if (now < lasttime_GetListCount) or ((now - lasttime_GetListCount) > 250) then
  begin
    lasttime_GetListCount := now;

    lastresult_GetListCount := MySendMessage(WM_USER,0,124);
  end;

  Result := lastresult_GetListCount;
end;

function TWinampCtrl.GetListPos: integer;
var
  now: Cardinal;
begin
  now := GetTickCount();

  if (now < lasttime_GetListPos) or ((now - lasttime_GetListPos) > 250) then
  begin
    lasttime_GetListPos := now;
    lastresult_GetListPos := MySendMessage(WM_USER,0,125);
  end;

  Result := lastresult_GetListPos;
end;

function TWinampCtrl.GetOutputTime(TimeMode : Integer) : Int64;
begin
     Result:=MySendMessage(WM_USER,TimeMode,105);
     { TimeMode :
        0 : Position in milliseconds
        1 : Length in seconds }
end;

// available only from an internal dll -----------------------------------------
// an alternative method is used just a bit down to get the Playlist info ------
function TWinampCtrl.GetSongFileName(Position: Integer) : String;
var
  res: Integer;
begin
     res := MySendMessage(WM_USER,Position,211);
     if (res <> 0) then
        Result := PChar(res)
     else
        Result := '';
end;

function TWinampCtrl.GetSongTitle(Position: Integer) : String;
var
  res: Integer;
begin
     res := MySendMessage(WM_USER,Position,212);
     if (res <> 0) then
        Result := PChar(res)
     else
        Result := '';
end;
// all other things work O.K. --------------------------------------------------

procedure TWinampCtrl.SetVolume(Volume : Byte);
begin
     MySendMessage(WM_USER,Volume,122);
end;

procedure TWinampCtrl.SetPanning(Panning : Byte);
begin
     if FUseBalanceCorrection then begin// some strange logic for the Balance ?
        if Panning in[0..127] then Panning:=Panning+128 else// so correct it
        if Panning in[128..255] then Panning:=Panning-128;
     end;
     MySendMessage(WM_USER,Panning,123);
end;

procedure TWinampCtrl.SetListPos(Position : Integer);
begin
     MySendMessage(WM_USER,Position,121);
end;

function TWinampCtrl.GetSongInfo(InfoMode : Integer): Integer;
var
  now: Cardinal;
begin
     { InfoMode
     0 : SampleRate
     1 : BitRate
     2 : Channels }

  now := GetTickCount();

  if (now < lasttime_GetSongInfo[InfoMode])
    or ((now - lasttime_GetSongInfo[InfoMode]) > 250) then
  begin
    lasttime_GetSongInfo[InfoMode] := now;

     lastresult_GetSongInfo[InfoMode] := MySendMessage(WM_USER,InfoMode,126);
  end;

  Result := lastresult_GetSongInfo[InfoMode];
end;

function TWinampCtrl.GetEQData(Position : Integer) : Integer;
begin
     Result:=MySendMessage(WM_USER,Position,127);
     { Position :
      0-9 : the ten EQ bands from left to right : Result = 0-63 ( +20 - -20 dB )
      10 : the preamplifier value : Result = 0-63 ( +20 - -20 dB )
      11 : EQ Enabled : Result = 0 if EQ Disabled else Result = NonZero
      12 : Autoload Enabled : Result = 0 if EQ Disabled else Result = NonZero
     }
end;

procedure TWinampCtrl.SetEQData(Position,Value : Integer);
begin
     MySendMessage(WM_USER,Position,127);
     MySendMessage(WM_USER,Value,128);
end;

// some Additional functions (separated from existing functions)

function TWinampCtrl.TrackLength: int64;
var
  now: Cardinal;
begin
     now := GetTickCount();
     if (now < lasttime_TrackLength)
      or ((now - lasttime_TrackLength) > 250) then
     begin
       lasttime_TrackLength := now;
       lastresult_TrackLength := GetOutputTime(1);
     end;
     Result:=lastresult_TrackLength;
end;


function TWinampCtrl.TrackPosition: int64;
var
  now: Cardinal;
begin
     now := GetTickCount();
     if (now < lasttime_TrackPosition)
        or ((now - lasttime_TrackPosition) > 250) then
     begin
       lasttime_TrackPosition := now;
       lastresult_TrackPosition := GetOutputTime(0);
     end;
     Result:=lastresult_TrackPosition;
end;

function TWinampCtrl.Frequency: integer;
begin
     Result:=GetSongInfo(1);
end;

function TWinampCtrl.SampleRate: integer;
begin
     Result:=GetSongInfo(0);
end;

function TWinampCtrl.NofChannels: integer;
begin
     Result:=GetSongInfo(2);
end;

procedure TWinampCtrl.EQAutoLoad(Enable: boolean);
begin
     SetEQData(12,Integer(Enable));
end;

procedure TWinampCtrl.EQEnable(Enable: boolean);
begin
     SetEQData(11,Integer(Enable));
end;

// loading lists ---------------------------------------------------------------

// FileNames list --------------------------------------------------------------
procedure TWinampCtrl.LoadFileNameList;
var P: Integer;
begin
     if not FileExists(FWinAmpLocation) then Exit;
     MySendMessage(WM_USER,0,120);
     FFileNameList.LoadFromFile(ExtractFilePath(FWinampLocation)+'winamp.m3u');
     FFileNameList.Delete(0);
     for P:=FFileNameList.Count-1 downto 0 do
         if (P mod 2)=0 then
            FFileNameList.Delete(P);
end;

// Titles list -----------------------------------------------------------------
procedure TWinampCtrl.LoadTitleList;
var P,L: Integer;
    S: String;
begin
     if not FileExists(FWinAmpLocation) then Exit;
     MySendMessage(WM_USER,0,120);
     FTitleList.LoadFromFile(ExtractFilePath(FWinampLocation)+'winamp.m3u');
     FTitleList.Delete(0);
     for P:=FTitleList.Count-1 downto 0 do
         if (P mod 2)<>0 then
            FTitleList.Delete(P)
         else begin
              S:=FTitleList.Strings[P];
              L:=Pos(',',S);
              FTitleList.Strings[P]:=Copy(S,L+1,Length(S)-L);
         end;
end;

// Length list -----------------------------------------------------------------
procedure TWinampCtrl.LoadLengthList;
var P,L,X: Integer;
    S: String;
begin
     if not FileExists(FWinAmpLocation) then Exit;
     MySendMessage(WM_USER,0,120);
     FLengthList.LoadFromFile(ExtractFilePath(FWinampLocation)+'winamp.m3u');
     FLengthList.Delete(0);
     for P:=FLengthList.Count-1 downto 0 do
         if (P mod 2)<>0 then
            FLengthList.Delete(P)
         else begin
              S:=FLengthList.Strings[P];
              L:=Pos(',',S);
              X:=Pos(':',S);
              FLengthList.Strings[P]:=Copy(S,X+1,L-X-1);
         end;
end;

// Retrieving PlayList info ----------------------------------------------------

// Songs FileName --------------------------------------------------------------
function TWinampCtrl.PlayListGetSongFileName(Position: Integer):String;
begin

     if FFreeLists and (FFileNameList=NIL) then FFileNameList:=TStringList.Create;

     if ((FFileNameList.Count * 2)+1 <> MySendMessage(WM_USER,0,124))
     or FAlwaysLoadList then LoadFileNameList;

     if Position >= FFileNameList.Count then Position:=FFileNameList.Count-1;
     if Position < 0 then Position:=0;
     Result:=FFileNameList.Strings[Position];
     if FFreeLists and (Assigned(FFileNameList)) then FFileNameList.Free;
end;

// Songs Title -----------------------------------------------------------------
function TWinampCtrl.PlayListGetSongTitle(Position: Integer):String;
begin

     if FFreeLists and (FTitleList=NIL) then FTitleList:=TStringList.Create;

     if ((FTitleList.Count * 2)+1 <> MySendMessage(WM_USER,0,124))
     or FAlwaysLoadList then LoadTitleList;

     if Position >= FTitleList.Count then Position:=FTitleList.Count-1;
     if Position < 0 then Position:=0;
     Result:=FTitleList.Strings[Position];
     if FFreeLists and (Assigned(FTitleList)) then FTitleList.Free;
end;

// Songs Length in seconds -----------------------------------------------------
function TWinampCtrl.PlayListGetSongLength(Position: Integer):Integer;
begin

     if FFreeLists and (FLengthList=NIL) then FLengthList:=TStringList.Create;

     if ((FLengthList.Count * 2)+1 <> MySendMessage(WM_USER,0,124))
     or FAlwaysLoadList then LoadLengthList;

     if Position >= FLengthList.Count then Position:=FLengthList.Count-1;
     if Position < 0 then Position:=0;
     Result:=StrToInt(FLengthList.Strings[Position]);
     if FFreeLists and (Assigned(FLengthList)) then FLengthList.Free;
end;

procedure TWinampCtrl.SaveFileNameListToFile(FileName: TFileName);
begin
     if FFreeLists and (FFileNameList=NIL) then FFileNameList:=TStringList.Create;
     LoadFileNameList;
     FFileNameList.SaveToFile(FileName);
     if FFreeLists and (Assigned(FFileNameList)) then FFileNameList.Free;
end;

procedure TWinampCtrl.SaveTitleListToFile(FileName: TFileName);
begin
     if FFreeLists and (FTitleList=NIL) then FTitleList:=TStringList.Create;
     LoadTitleList;
     FTitleList.SaveToFile(FileName);
     if FFreeLists and (Assigned(FTitleList)) then FTitleList.Free;
end;

// miscelaneous stuff ----------------------------------------------------------

// hide manually ---------------------------------------------------------------
procedure TWinampCtrl.Hide;
var
  WP : TWindowPlaceMent;
  h: HWND;
begin
  h := GetWAHwnd();
  wp.length := SizeOf(TWindowPlaceMent);
  wp.showCmd := SW_HIDE;
  SetWindowPlacement(h,@wp);
end;

// show manually ---------------------------------------------------------------
procedure TWinampCtrl.Show;
var
  WP : TWindowPlaceMent;
  h: HWND;
begin
  h := GetWAHwnd();
  wp.length := SizeOf(TWindowPlaceMent);
  wp.showCmd := SW_SHOW;
  SetWindowPlacement(h,@wp);
end;

// here we read the WinAmp Title :-) -------------------------------------------
function TWinampCtrl.GetCurrSongTitle: String;
var WATitle: PChar;
    Stringica: String;
    I,Sizica: Integer;
    s: string;
    h: HWND;
    now: Cardinal;
begin
     now := GetTickCount();

     if (now < lasttime_GetCurrSongTitle)
        or ((now - lasttime_GetCurrSongTitle) > 250) then
     begin
       lasttime_GetCurrSongTitle := now;

       h := GetWAHwnd();
       if h<>0 then
       begin
         Sizica:=GetWindowTextLength(h);
         WATitle:=StrAlloc(Sizica+1);// allocte the memory for the buffer
         GetWindowText(h,WATitle,Sizica+1);
         Stringica:=StrPas(WATitle);
         StrDispose(WATitle);// free the memory
         for I:=1 to Length(Stringica) do begin
             s:=Copy(Stringica,I,8);
             if LowerCase(s)='- winamp' then break;
         end;
         lastresult_GetCurrSongTitle := Copy(Stringica,1,I-1);
       end
       else
         lastresult_GetCurrSongTitle := '';
     end;

     Result := lastresult_GetCurrSongTitle;
end;

procedure TWinampCtrl.CheckIfSongChanged;//(var Msg: TWMSetText);
var
  h: HWND;
begin
     h := GetWAHwnd();
     if h=0 then exit;
     if PlaylistPos<>GetListPos then begin
        PlaylistPos:=GetListPos;
        if Assigned(FOnSongChanged) then FOnSongChanged(Self);
     end;
end;

//running WinAmp ---------------------------------------------------------------

procedure TWinampCtrl.RunWinAmp(NewInstance: Boolean);
begin
     if fWinAmpLocation='' then begin
        MessageBox(0,'No file to run','Error',mb_IconError);
        Exit;
     end;
     if FileExists(fWinAmpLocation) then begin
        if LowerCase(ExtractFileExt(fWinAmpLocation))='.exe' then begin
           RunThread:=TRunThread.Create(true);
           RunThread.FileName:=fWinAmpLocation;
           RunThread.Params:=FParams;
           RunThread.NewInstance:=NewInstance;
           RunThread.FreeOnTerminate:=true;
           RunThread.Priority:=tpNormal;
           RunThread.Resume;
        end
        else MessageBox(0,'Not an executable file!','Error',mb_IconError);
     end
     else MessageBox(0,'File doesn''t exist!','Error',mb_IconError);
end;

procedure TWinampCtrl.SetWinampLocation(Value: TFileName);
begin
     if Value <> fWinAmpLocation then fWinAmpLocation:=Value;
end;

// executing winamp ------------------------------------------------------------

procedure TRunThread.Execute;
begin
     if NewInstance then WinExec(PChar(FileName+' /NEW'+' '+Params),SW_SHOW)
     else WinExec(PChar(FileName+' '+Params),SW_SHOW);
end;

end.
