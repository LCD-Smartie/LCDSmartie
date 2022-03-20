unit UDataFolding;

{$MODE Delphi}

interface

uses
  SysUtils,URLThread;

type
  TFoldingDataThread = class(TURLThread)
  private
     foldLastWU, foldActClientsWeek, foldActClientsFiftyMin,
     foldTeamName, foldTeamScore, foldTeamWU, foldTeamLastWU,
     foldScore, foldRank, foldWU, foldUser: String;
    procedure DoFoldingHTTPUpdate;
  protected
    procedure  DoUpdate; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  ResolveVariables(var Line : string); override;
  end;

implementation

uses
  UConfig, UUtils, UJSON;

constructor TFoldingDataThread.Create;
begin
  inherited Create(60000);
end;

destructor TFoldingDataThread.Destroy;
begin
  inherited;
end;

procedure TFoldingDataThread.ResolveVariables(var Line : string);
begin
  if (pos('$FOLD', line) <> 0) then
  begin
    fDataLock.Enter();
    try
      line := StringReplace(line, '$FOLDlastwu', foldLastWU, [rfReplaceAll]);
      line := StringReplace(line, '$FOLDact50min', foldActClientsFiftyMin, [rfReplaceAll]);
      line := StringReplace(line, '$FOLDactweek', foldActClientsWeek, [rfReplaceAll]);
      line := StringReplace(line, '$FOLDteamname', foldTeamName, [rfReplaceAll]);
      line := StringReplace(line, '$FOLDteamscore', foldTeamScore, [rfReplaceAll]);
      line := StringReplace(line, '$FOLDteamwu', foldTeamWU, [rfReplaceAll]);
      line := StringReplace(line, '$FOLDteamlastwu', foldTeamLastWU, [rfReplaceAll]);
      line := StringReplace(line, '$FOLDscore', foldScore, [rfReplaceAll]);
      line := StringReplace(line, '$FOLDrank', foldRank, [rfReplaceAll]);
      line := StringReplace(line, '$FOLDwu', foldWU, [rfReplaceAll]);
      line := StringReplace(line, '$FOLDUser', foldUser, [rfReplaceAll]);
    finally
      fDataLock.Leave();
    end;
  end;
end;

procedure TFoldingDataThread.DoFoldingHTTPUpdate;
var
  sFilename: String;
  JsonStats: myJSONItem;
  JsonString: string;
  JsonTeam: myJSONItem;
  iRank: integer;
  i: integer;
begin
  try
    JsonStats := myJSONItem.Create;
    if TryStrToInt(config.foldUserid, i) then
    begin
      if ( strtoint(config.foldUserid) > 0 )then
        sFilename := getUrl('https://api2.foldingathome.org/uid/'+config.foldUserid)
    end
    else
      Raise Exception.Create('User ID must be a number');

    JsonStats.LoadFromFile(sFilename);

    fDataLock.Enter();
    foldUser := JsonStats.Value[0].getstr; // name
    foldScore := JsonStats.Value[2].getstr; // Score
    foldWU := JsonStats.Value[3].getstr; // work units

    iRank :=4;
    if JsonStats.Has['rank'] then     // if rank exists
    begin
      foldRank := JsonStats.Value[iRank].getstr;
      iRank := iRank + 1; // for wu that could be 4 or 5 depending on rank being available
    end
    else
      foldRank := 'No rank';

    foldActClientsFiftyMin := JsonStats.Value[iRank].getstr; // Clients 50 min
    foldActClientsWeek := JsonStats.Value[iRank+1].getstr; // Clients week
    foldLastWU := JsonStats.Value[iRank+2].getstr; // last wu

    /// Team Stuff
    JsonTeam := myJSONItem.Create;
    JsonString := JsonStats.Value[iRank+4].getJSON;
    delete(JsonString, 1, 1);
    JsonString := Copy(JsonString,1,length(JsonString)-1);
    JsonTeam.Code := JsonString;
    foldTeamName := JsonTeam.Value[1].getstr;
    foldTeamScore := JsonTeam.Value[2].getstr;
    foldTeamWU := JsonTeam.Value[3].getstr;
    foldTeamLastWU := JsonTeam.Value[4].getstr;
    fDataLock.Leave();

  except
    on EExiting do raise;
    on E: Exception do
    begin
      fDataLock.Enter();
      try
        foldLastWU := '[fold: ' + E.Message + ']';
        foldActClientsWeek := '[fold: ' + E.Message + ']';
        foldActClientsFiftyMin := '[fold: ' + E.Message + ']';
        foldTeamName := '[fold: ' + E.Message + ']';
        foldTeamScore := '[fold: ' + E.Message + ']';
        foldTeamWU := '[fold: ' + E.Message + ']';
        foldTeamLastWU := '[fold: ' + E.Message + ']';
        foldScore := '[fold: ' + E.Message + ']';
        foldRank := '[fold: ' + E.Message + ']';
        foldWU := '[fold: ' + E.Message + ']';
      finally
        fDataLock.Leave();
      end;
    end;
  end;
end;

procedure TFoldingDataThread.DoUpdate;
var
  MyDoUpdate : boolean;
  ScreenCount,LineCount : integer;
  screenline : string;
begin
  MyDoUpdate := false;
  for ScreenCount := 1 to MaxScreens do
  begin
    for LineCount := 1 to config.height do
    begin
      screenline := config.screen[ScreenCount].line[LineCount].text;
      if (pos('$FOLD', screenline) <> 0) then MyDoUpdate := true;
    end;
  end;
  if config.foldEnabled then
    if MyDoUpdate then
      DoFoldingHTTPUpdate;
end;

end.
