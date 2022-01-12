unit UDataSeti;

{$MODE Delphi}

interface

uses
  SysUtils,URLThread;

type
  TSETIDataThread = class(TURLThread)
  private
    usFormat : TFormatSettings; //this is initialized with US/English
    setiNumResults, setiCpuTime, setiAvgCpu, setiLastResult, setiUserTime,
    setiTotalUsers, setiRank, setiShareRank, setiMoreWU: String;
    procedure DoSETIHTTPUpdate;
  protected
    procedure DoUpdate; override;
    function UsesCOMObjects : boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  ResolveVariables(var Line : string); override;
  end;

implementation

uses
  UConfig,UUtils;

constructor TSETIDataThread.Create;
begin
  GetLocaleFormatSettings($0409,usFormat);  //English/USA
  inherited Create(config.newsRefresh*60000);
end;

destructor TSETIDataThread.Destroy;
begin
  inherited;
end;

function TSETIDataThread.UsesCOMObjects : boolean;
begin
  result := true;
end;

procedure TSETIDataThread.ResolveVariables(var Line : string);
begin
  if (pos('$SETI', line) <> 0) then
  begin
    fDataLock.Enter();
    try
      line := StringReplace(line, '$SETIResults', setiNumResults, [rfReplaceAll]);
      line := StringReplace(line, '$SETICPUTime', setiCpuTime, [rfReplaceAll]);
      line := StringReplace(line, '$SETIAverage', setiAvgCpu, [rfReplaceAll]);
      line := StringReplace(line, '$SETILastresult', setiLastResult, [rfReplaceAll]);
      line := StringReplace(line, '$SETIusertime', setiUserTime, [rfReplaceAll]);
      line := StringReplace(line, '$SETItotalusers', setiTotalUsers, [rfReplaceAll]);
      line := StringReplace(line, '$SETIrank', setiRank, [rfReplaceAll]);
      line := StringReplace(line, '$SETIsharingrank', setiShareRank, [rfReplaceAll]);
      line := StringReplace(line, '$SETImoreWU', setiMoreWU, [rfReplaceAll]);
    finally
      fDataLock.Leave();
    end;
  end;
end;

procedure TSETIDataThread.DoSETIHTTPUpdate;
{var
  StartItemNode : IXMLNode;
  ANode : IXMLNode;
  XMLDoc : IXMLDocument;}
//  Filename: String;
begin
  // Fetch the Rss data  (but not more often than 24 hours)
{  try
    FileName := getUrl(
      'http://setiathome2.ssl.berkeley.edu/fcgi-bin/fcgi?cmd=user_xml&email='
      + config.setiEmail, 12*60);
    if (Terminated) then raise EExiting.Create('');

    // Parse the Xml data
    if FileExists(Filename) then
    begin
      XMLDoc := LoadXMLDocument(Filename);

      StartItemNode := XMLDoc.DocumentElement.ChildNodes.FindNode('userinfo');
      ANode := StartItemNode;

      fDataLock.Enter();
      try
        setiNumResults := ANode.ChildNodes['numresults'].Text;
        setiCpuTime := ANode.ChildNodes['cputime'].Text;
        setiAvgCpu := ANode.ChildNodes['avecpu'].Text;
        setiLastResult := ANode.ChildNodes['lastresulttime'].Text;
        setiUserTime := ANode.ChildNodes['usertime'].Text;
      finally
        fDataLock.Leave();
      end;
      // not used: 'regdate'

      // not used: group info.

      StartItemNode := XMLDoc.DocumentElement.ChildNodes.FindNode('rankinfo');
      ANode := StartItemNode;

      fDataLock.Enter();
      try
        setiTotalUsers := ANode.ChildNodes['ranktotalusers'].Text;
        setiRank := ANode.ChildNodes['rank'].Text;
        setiShareRank := ANode.ChildNodes['num_samerank'].Text;
        // SETI provides floats not dependent on user's locale, but always in US format

        setiMoreWU := FloatToStr(
          100-StrToFloat(ANode.ChildNodes['top_rankpct'].Text, usFormat),
          localeFormat);
      finally
        fDataLock.Leave();
      end;
    end;
  except
    on EExiting do raise;
    on E: Exception do
    begin
      fDataLock.Enter();
      try
        setiNumResults := '[Seti: ' + E.Message + ']';
        setiCpuTime := '[Seti: ' + E.Message + ']';
        setiAvgCpu := '[Seti: ' + E.Message + ']';
        setiLastResult := '[Seti: ' + E.Message + ']';
        setiUserTime := '[Seti: ' + E.Message + ']';
        setiTotalUsers := '[Seti: ' + E.Message + ']';
        setiRank := '[Seti: ' + E.Message + ']';
        setiShareRank := '[Seti: ' + E.Message + ']';
        setiMoreWU := '[Seti: ' + E.Message + ']';
      finally
        fDataLock.Leave();
      end;
    end;
  end; }
end;

procedure TSETIDataThread.DoUpdate;
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
      if (pos('$SETI', screenline) <> 0) then MyDoUpdate := true;
    end;
  end;
  if config.setiEnabled then
   if MyDoUpdate then
     DoSETIHTTPUpdate;
end;

end.
