unit UDataGame;

{$MODE Delphi}

interface

uses
  UConfig,DataThread;

const
  UnrealKey = '$Unreal';
  Quake2Key = '$QuakeII';
  Quake3Key = '$QuakeIII';
  HalfLifeKey = '$Half-life';

type
  TGameType = (gtUnreal,gtQuake2,gtQuake3,gtHalfLife);

const
  GameKeys : array[TGameType] of string = (UnrealKey,Quake2Key,Quake3Key,HalfLifeKey);
  GameCommandLineParams : array[TGameType] of string = ('-uns','-q2s','-q3s','-hls');

  MinGame = gtUnreal;
  MaxGame = gtHalfLife;

type
  TGameDataThread = class(TDataThread)
  private
    qstatreg1: Array[1..MaxScreens, 1..MaxLines] of String;  //Guarded by dataCs,  data+main thread
    qstatreg2: Array[1..MaxScreens, 1..MaxLines] of String;  //Guarded by dataCs,  data+main thread
    qstatreg3: Array[1..MaxScreens, 1..MaxLines] of String;  //Guarded by dataCs,  data+main thread
    qstatreg4: Array[1..MaxScreens, 1..MaxLines] of String;  //Guarded by dataCs,  data+main thread
    procedure  ResolveGameVariables(var Line : string; qstattemp: Integer = 1);
  protected
    procedure  DoUpdate; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  ResolveVariables(var Line : string); override;
  end;

implementation

uses
  Windows,UMain,UUtils,SysUtils;

constructor TGameDataThread.Create;
begin
  inherited Create(config.gameRefresh*60000);
end;

destructor TGameDataThread.Destroy;
begin
  inherited;
end;

procedure  TGameDataThread.DoUpdate;
var
  templine2, templine4, line: String;
  templine1: Array [1..80] of String;
  counter, counter2: Integer;
  fFile2: textfile;
  ScreenCount, LineCount: Integer;
  GameCount : TGameType;
  screenline: String;
  srvr: String;
  sTemp: String;
begin
  for ScreenCount := 1 to MaxScreens do
  begin
    for LineCount := 1 to config.height do
    begin
      try
        screenline := config.screen[ScreenCount].line[LineCount].text;
        for GameCount := MinGame to MaxGame do begin
          if (pos(GameKeys[GameCount], screenline) <> 0) then begin
            if (Terminated) then raise EExiting.Create('');
            srvr := GameCommandLineParams[GameCount];
            winexec(PChar(extractfilepath(paramstr(0)) +
              'qstat.exe -P -of txt'
              + intToStr(ScreenCount) + '-' + intToStr(LineCount) +
              '.txt -sort F ' + srvr + ' ' + config.gameServer[ScreenCount, LineCount]), sw_hide);

            sleep(1000);

            assignfile (fFile2, extractfilepath(paramstr(0)) + 'txt' +
              IntToStr(ScreenCount) + '-' + IntToStr(LineCount) + '.txt');
            try
              reset (fFile2);
              counter := 1;
              while (not eof(fFile2)) and (counter < 80) do
              begin
                readln (fFile2, templine1[counter]);
                counter := counter + 1;
              end;
            finally
              closefile(fFile2);
            end;

            if (pos(GameKeys[GameCount]+'1', screenline) <> 0) then
            begin
              sTemp := copy(templine1[2], pos(' / ', templine1[2]) +
                3, length(templine1[2]));
              sTemp := stripspaces(copy(sTemp, pos(' ', sTemp) + 1, length(sTemp)));

              fDataLock.Enter();
              qstatreg1[ScreenCount, LineCount] := sTemp;
              fDataLock.Leave();
            end;

            if (pos(GameKeys[GameCount]+'2', screenline) <> 0) then
            begin
              sTemp := copy(templine1[2], pos(':', templine1[2]), length(templine1[2]));
              sTemp := copy(sTemp, pos('/', sTemp) + 4, length(sTemp));
              sTemp := copy(sTemp, 1, pos('/', sTemp)-5);
              sTemp := stripspaces(copy(sTemp, pos(' ', sTemp) + 1, length(sTemp)));

              fDataLock.Enter();
              qstatreg2[ScreenCount, LineCount] := sTemp;
              fDataLock.Leave();
            end;

            if (pos(GameKeys[GameCount]+'3', screenline) <> 0) then
            begin
              sTemp := stripspaces(copy(templine1[2], pos(' ', templine1[2]),
                   length(templine1[2])));
              sTemp := stripspaces(copy(sTemp, 1, pos('/', sTemp) + 3));

              fDataLock.Enter();
              qstatreg3[ScreenCount, LineCount] := sTemp;
              fDataLock.Leave();
            end;

            if (pos(GameKeys[GameCount]+'4', screenline) <> 0) then
            begin
              sTemp := '';
              for counter2 := 1 to counter-3 do
              begin
                line := stripspaces(templine1[counter2 + 2]);
                templine2 := stripspaces(copy(copy(line, pos('s ', line) + 1,
                  length(line)), pos('s ', line) + 2, length(line)));
                templine4 := stripspaces(copy(line, 2, pos(' frags ',
                  line)-1));
                line := templine2 + ':' + templine4 + ',';
                sTemp := sTemp + line;
              end;
              fDataLock.Enter();
              qstatreg4[ScreenCount, LineCount] := sTemp;
              fDataLock.Leave();
            end;
          end;
        end;  // gamecount
      except
        on EExiting do raise;
        on E: Exception do
        begin
          fDataLock.Enter();
          try
            qstatreg1[ScreenCount, LineCount] := '[Exception: ' + E.Message + ']';
            qstatreg2[ScreenCount, LineCount] := '[Exception: ' + E.Message + ']';
            qstatreg3[ScreenCount, LineCount] := '[Exception: ' + E.Message + ']';
            qstatreg4[ScreenCount, LineCount] := '[Exception: ' + E.Message + ']';
          finally
            fDataLock.Leave();
          end;
        end;
      end;  // try/except
    end;  // linecount
  end;  // screencount
end;

procedure  TGameDataThread.ResolveGameVariables(var Line : string; qstattemp: Integer = 1);
var
  GameCount : TGameType;
begin
  for GameCount := MinGame to MaxGame do begin
    if (pos(GameKeys[GameCount], line) <> 0) then begin
      fDataLock.Enter();
      try
        line := StringReplace(line, GameKeys[GameCount]+'1', qstatreg1[activeScreen,
          qstattemp], [rfReplaceAll]);
        line := StringReplace(line, GameKeys[GameCount]+'2', qstatreg2[activeScreen,
          qstattemp], [rfReplaceAll]);
        line := StringReplace(line, GameKeys[GameCount]+'3', qstatreg3[activeScreen,
          qstattemp], [rfReplaceAll]);
        line := StringReplace(line, GameKeys[GameCount]+'4', qstatreg4[activeScreen,
          qstattemp], [rfReplaceAll]);
      finally
        fDataLock.Leave();
      end;
    end;
  end;
end;

procedure  TGameDataThread.ResolveVariables(var Line : string);
begin
  ResolveGameVariables(Line);
end;

end.
