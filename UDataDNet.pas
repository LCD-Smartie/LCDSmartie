unit UDataDNet;

{$MODE Delphi}

interface

uses
  DataThread;

type
  TDNetDataThread = class(TDataThread)
  private
    replline2, replline1: String;
    procedure UpdateDNetStats;
  protected
    procedure  DoUpdate; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  ResolveVariables(var Line : string); override;
  end;

implementation

uses
  SysUtils,UConfig;

constructor TDNetDataThread.Create;
begin
  replline2 := '';
  replline1 := '';
  inherited Create(100000);  // this is probably way too often, but just duplicating past functionality
end;

destructor TDNetDataThread.Destroy;
begin
  inherited;
end;

procedure TDNetDataThread.UpdateDNetStats;
var
  fFile: textfile;
  x,counter: Integer;
  line: String;
  replline: String;
  sTemp: String;
begin

  x := 0;
  replline := 'File not found';
  if FileExists(config.distLog) = true then
  begin
    assignfile(fFile, config.distLog);
    reset (fFile);
    while not eof(fFile) do
    begin
      readln (fFile);
      x := x + 1;
    end;
    reset(fFile);
    for counter := 1 to x-50 do
    begin
      readln(fFile);
    end;
    while not eof(fFile) do
    begin
      readln(fFile, line);
      replline := replline + ' ' + line;
    end;
    closefile(fFile);
  end;
  replline := copy(replline, pos('Completed', replline)-5,
    length(replline));
  for x := 1 to 9 do
  begin
    if pos('Completed', replline) <> 0 then
    begin
      replline := copy(replline, pos('Completed', replline)-5,
        length(replline));
      replline := StringReplace(replline, 'Completed', '-', []);
    end;
  end;

  if copy(replline, 1, 3) = 'RC5' then
  begin
    sTemp := copy(replline, pos('- [', replline) + 3, pos(' keys',
      replline)-pos('- [', replline));
    if length(sTemp) > 7 then
    begin
      sTemp := copy(sTemp, 1, pos(',', copy(sTemp, 3,
        length(sTemp))) + 1);
    end;
    fDataLock.Enter();
    replline1 := sTemp;
    fDataLock.Leave();

    replline := copy(replline, pos('completion', replline) + 30, 200);
    sTemp := copy(replline, pos('(', replline) + 1, pos('.',
      replline)-pos('(', replline)-1);

    fDataLock.Enter();
    replline2 := sTemp;
    fDataLock.Leave();
  end;

  if copy(replline, 1, 3) = 'OGR' then
  begin
    sTemp := copy(replline, pos('- [', replline) + 3, pos(' nodes',
      replline)-pos('- [', replline));
    if length(sTemp) > 7 then
    begin
      sTemp := copy(sTemp, 1, pos(',', copy(sTemp, 3,
        length(sTemp))) + 1);
    end;

    fDataLock.Enter();
    replline1 := sTemp;
    fDataLock.Leave();

    replline := copy(replline, pos('remain', replline) + 8, 100);
    sTemp := copy(replline, pos('(', replline) + 1, pos('stats',
      replline)-pos('(', replline)-3);

    fDataLock.Enter();
    replline2 := sTemp;
    fDataLock.Leave();
  end;
end;

procedure TDNetDataThread.DoUpdate;
var
  bReplz: Boolean;
  ScreenCount, LineCount: Integer;
  screenline: String;
begin
  bReplz := false;

  for ScreenCount := 1 to MaxScreens do
  begin
    for LineCount := 1 to config.height do
    begin
      screenline := config.screen[ScreenCount].line[LineCount].text;
      if (not bReplz) and (pos('$Dnet', screenline) <> 0) then bReplz := true;
    end;
  end;

  if bReplz then
    UpdateDNetStats;
end;

procedure TDNetDataThread.ResolveVariables(var Line : string);
begin
  if (pos('$Dnet', line) <> 0) then
  begin
    fDataLock.Enter();
    try
      line := StringReplace(line, '$DnetDone', replline2, [rfReplaceAll]);
      line := StringReplace(line, '$DnetSpeed', replline1, [rfReplaceAll]);
    finally
      fDataLock.Leave();
    end;
  end;
end;

end.
