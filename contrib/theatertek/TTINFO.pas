unit TTINFO;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

const
  TT_CONTROL_EVENT = WM_USER+102;
  TT_GETINFO = 300;

  MAX_PATH = 260;

type
  TCopyDataStruct = packed record
    dwData: DWORD; //up to 32 bits of data to be passed to the receiving application
    cbData: DWORD; //the size, in bytes, of the data pointed to by the lpData member
    lpData: Pointer; //Points to data to be passed to the receiving application. This member can be nil.
  end;

  DVD_HMSF_TIMECODE = record
    bHours : byte;
    bMinutes : byte;
    bSeconds : byte;
    bFrames : byte;
  end;

  DVD_PLAYBACK_LOCATION2 = record
    TitleNum : dword;
    ChapterNum : dword;
    TimeCode : DVD_HMSF_TIMECODE;
    TimeCodeFlags : dword;
  end;

  DVD_AUDIO_FORMAT =
    (da_AC3,da_MPEG1,da_MPEG1_DRC,da_MPEG2,da_MPEG2_DRC,
     da_LPCM,da_DTS,da_SDDS,da_Other);

  GRAPH_STATE = (gsUninitialized,gsGraph_Stopped1,gsGraph_Stopped2,
    gsNav_Stopped,gsPlaying,gsGraph_Paused,gsNav_Paused,gsScanning);

const
  GRAPH_STATE_STRING : array[GRAPH_STATE] of string[10] =
    ('NONE','STOP','STOP','STOP','PLAY','PAUSE','PAUSE','SCAN');

  AUDIO_FORMAT_STRING : array[DVD_AUDIO_FORMAT] of string[10] =
    ('AC3','MPEG1','MPEG1_DRC','MPEG2','MPEG2_DRC','LPCM','DTS','SDDS','Other');

type
  ttinfo_struct = record
    Garbage : array[1..16] of byte;
    szFileName : array[1..MAX_PATH] of char; // Current filename
    nTitle : longint;                     // Current Title numbe
    nChapter : longint;                   // Current Chapter number
    nState : GRAPH_STATE;                 // Play, Pause etc. (GRAPH_STATE)
    nSpeed : longint;                     // 8x etc.
    curTime : DVD_PLAYBACK_LOCATION2;     // Current time
    totalTime : DVD_HMSF_TIMECODE;        // Media total time
    szAR : array[1..80] of char;          // Aspect Ratio
    nChannels : longint;                  // Number Audio channels
    nAudioFormat : DVD_AUDIO_FORMAT;      // Audio format (DVD_AUDIO_FORMAT)
    nAudBitrate : longint;                // Kbps (Divide by 1024)
    nVidBitrate : longint;                // Mbps (Divide by 1024000.0)
    nDiskID : Int64;                      // 64 bit unique disk identifier
    szPAD : array[1..120] of char;        // Padding for future
  end;

type
  TTTINFOForm = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    LastRec : dword;
    procedure WMCopyData(var Msg : TWMCopyData); message WM_COPYDATA;
  public
    { Public declarations }
    TTWnd : hWnd;
    sData : ttinfo_struct;
  end;

var
  TTINFOForm : TTTINFOForm = nil;

implementation

{$R *.lfm}

uses
  Math;

procedure TTTINFOForm.FormCreate(Sender: TObject);
begin
  fillchar(sData,sizeof(sData),$00);
  TTWnd := FindWindowEx(0,0,'TTWndClass','TheaterTek DVD');
  LastRec := gettickcount;
end;

procedure TTTINFOForm.Timer1Timer(Sender: TObject);
begin
  if (TTWnd <> 0) then begin
    if ((gettickcount - LastRec) < 10000) then
      PostMessage(TTWnd, TT_CONTROL_EVENT, TT_GETINFO, TTINFOForm.Handle)
    else
      TTWnd := 0;  // force us to go look for the window again
  end else begin
    TTWnd := FindWindowEx(0,0,'TTWndClass','TheaterTek DVD');
    LastRec := gettickcount;
  end;
end;

procedure TTTINFOForm.WMCopyData(var Msg : TWMCopyData);
begin
  LastRec := gettickcount;
  move(Msg.CopyDataStruct.lpData,sData,min(sizeof(sData),Msg.CopyDataStruct.cbData));
end;

end.
