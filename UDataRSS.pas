unit UDataRSS;

{$MODE Delphi}

interface

uses
  SysUtils,URLThread, UMain;

const
  MaxRSSItems = 40;

type
  TRSS = record
    url: String;
    title: array [0..MaxRSSItems] of string;  // 0 is all titles
    desc: array [0..MaxRSSItems] of string;   // 0 is all descs
    items: cardinal;
    whole: string;                            // all titles and descs
    maxfreq: cardinal;                        // hours - 0 means no restriction
  end;

  TRSSDataThread = class(TURLThread)
  private
    RSS: array of TRSS;    // Guarded by RSSCs, data + main thread
    RSSEntries: cardinal;  // Guarded by RSSCs, data + main thread
    procedure DoRSSHTTPUpdate;
    function GetRSS(Url: String; var titles, descs: Array of String;
                    maxitems: Cardinal; maxfreq: Cardinal = 0): Cardinal;
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
  laz2_XMLRead, laz2_DOM, UConfig, UUtils;

constructor TRSSDataThread.Create;
begin
  inherited Create(60000); // check every minute
end;

destructor TRSSDataThread.Destroy;
begin
  inherited;
end;

function TRSSDataThread.UsesCOMObjects : boolean;
begin
  result := true;
end;

procedure TRSSDataThread.ResolveVariables(var Line : string);
var
  args: Array [1..maxArgs] of String;
  prefix, postfix: String;
  numArgs: Cardinal;
  RSSFeedIndex: Cardinal;
  found: Boolean;
  ItemIndex : Cardinal;
begin
  // $Rss(URL, TYPE [, NUM [, FREQ]])
  //   TYPE is t=title, d=desc, b=both
  //   NUM is 1 for item 1, etc. 0 means all (default). [when TYPR is b, then 0 is used]
  //   FREQ is the number of hours that must past before checking stream again.
  while decodeArgs(line, '$Rss', maxArgs, args, prefix, postfix, numargs)
    do
  begin
    RequiredParameters(numargs, 2, 4);
    if (numargs < 3) then
    begin
      args[3] := '0';
    end;

    // locate entry
    RSSFeedIndex := 0;
    found := false;
    fDataLock.Enter();
    try
      while (RSSFeedIndex < RSSEntries) and (not found) do
      begin
        if (RSS[RSSFeedIndex].url = LCDSmartieDisplayForm.Data.change(args[1])) then found := true
        else Inc(RSSFeedIndex);
      end;

      try
        if (found) and (RSS[RSSFeedIndex].items > 0)
          and (Cardinal(StrToInt(LCDSmartieDisplayForm.Data.change(args[3]))) <= RSS[RSSFeedIndex].items) then
        begin

          ItemIndex := StrToInt(LCDSmartieDisplayForm.Data.change(args[3]));
          case (LCDSmartieDisplayForm.Data.change(args[2])[1]) of
            't' : line := prefix + RSS[RSSFeedIndex].title[ItemIndex] + postfix;  // title
            'd' : line := prefix + RSS[RSSFeedIndex].desc[ItemIndex] + postfix; // description
            'b' : begin
              if (ItemIndex > 0) then  // both
                line := prefix + RSS[RSSFeedIndex].title[ItemIndex] + ':' +
                        RSS[RSSFeedIndex].desc[ItemIndex] + postfix
              else
                line := prefix + RSS[RSSFeedIndex].whole + postfix;
            end;
            else line := prefix + '[Error: Rss: bad arg #2]' + postfix;
          end; // case

        end
        else
        begin
          if (found) then
          begin

            // We know about the Rss entry but have no data...
            if (copy(RSS[RSSFeedIndex].whole, 1, 6) = '[Rss: ') then
            begin
              // Assume an error message is in whole
              line := prefix + RSS[RSSFeedIndex].whole + postfix;
            end
            else
            begin
              line := prefix + '[Rss: No Data]' + postfix;
            end;

          end
          else
          begin

            // Nothing known yet - waiting for data thread...
            line := prefix + '[Rss: Waiting for data]' + postfix;

          end;
        end;
      except
        on E: Exception do line := prefix + '[Rss: '
          + CleanString(E.Message) + ']' + postfix;
      end;
    finally
      fDataLock.Leave();
    end;
  end;
end;


function TRSSDataThread.GetRSS(Url: String; var titles, descs: Array of String;
  maxitems: Cardinal; maxfreq: Cardinal = 0): Cardinal;
var
  XMLDoc: TXMLDocument;
  ANode, channel, propertynode: TDOMNode;
  items: Cardinal;
  RSSFilename: String;

begin
  items := 0;

  //
  // Fetch the RSS data
  //
  if (maxfreq = 0) then maxfreq := 1;
  RSSFileName := getUrl(Url, maxfreq);
  if (Terminated) then raise EExiting.Create('');

  // This needed a complete rewrite from the delphi version
  if FileExists(RSSFilename) then
  begin
    ReadXMLFile(XMLDoc, RSSFilename);
    if (XMLDoc.documentElement.hasChildNodes) then
    begin
      channel := nil;
      propertynode := XMLDoc.DocumentElement.FindNode('item'); //Slashdot Style. items are top level
      if (propertynode = nil) then
        channel := XMLDoc.DocumentElement.FindNode('channel'); // BBC news style. Items are below channel
      if channel <> nil then
        propertynode := channel.FindNode('item');
      if propertynode <> nil then
      repeat
        Inc(items);
        ANode := propertynode.FindNode('title');
        if ANode <> nil then
          titles[items] := stripHtml(ANode.TextContent) ;
        ANode := propertynode.FindNode('description');
        if ANode <> nil then
          descs[items] := stripHtml(ANode.TextContent) ;
        propertynode := propertynode.NextSibling
      until (propertynode = nil) or (items >= maxItems);
    end;   // phew, now that's done I need to call free on all this shit
    XMLDoc.Free;
//    channel.Free;
//    propertynode.Free;
//    anode.Free;
  end;
  Result := items;
end;

procedure TRSSDataThread.DoRSSHTTPUpdate;
var
  counter, counter2: Integer;
  sAllTitles, sAllDescs, sWhole: String;
  items: Cardinal;
  titles: Array[0..MaxRSSItems] of String;
  descs: Array[0..MaxRSSItems] of String;
begin
  for counter := 0 to RSSEntries-1 do
  begin
    if (RSS[counter].url <> '') then
    begin
      if (Terminated) then raise EExiting.Create('');

      try
        items := GetRSS(RSS[counter].url, titles,
          descs, MaxRSSItems, RSS[counter].maxfreq);

        fDataLock.Enter();
        try
          RSS[counter].items := items;

          sAllTitles := '';
          sAllDescs := '';
          sWhole := '';
          for counter2 := 1 to items do
          begin
            RSS[counter].title[counter2] := titles[counter2];
            RSS[counter].desc[counter2] := descs[counter2];

            sAllTitles := sAllTitles + titles[counter2] + ' | ';
            sAllDescs := sAllDescs + descs[counter2] + ' | ';
            sWhole := sWhole + titles[counter2] + ':' +
              descs[counter2] + ' | ';

            if (Terminated) then raise EExiting.Create('');
          end;
          RSS[counter].whole := sWhole;
          RSS[counter].title[0] := sAllTitles;
          RSS[counter].desc[0] := sAllDescs;
        finally
          fDataLock.Leave();
        end;
      except
        on EExiting do raise;
        on E: Exception do
        begin
          fDataLock.Enter();
          try
            RSS[counter].items := 0;
            RSS[counter].title[0] := '[Rss: ' + E.Message + ']';
            RSS[counter].desc[0] := '[Rss: ' + E.Message + ']';
            RSS[counter].whole := '[Rss: ' + E.Message + ']';
          finally
            fDataLock.Leave();
          end;
        end;
      end;
    end;
  end;
end;

procedure TRSSDataThread.DoUpdate;
var
  ScreenCount,LineCount : integer;
  screenline : string;
  MyRSSCount: Integer;
  args: Array [1..maxArgs] of String;
  prefix: String;
  postfix: String;
  numargs: Cardinal;
  iFound: Integer;
  RSSLoop : integer;
  iMaxFreq: Integer;
begin
  // TODO: this should only be done when the config changes...
  MyRSSCount := 0;

  for ScreenCount := 1 to MaxScreens do
  begin
    for LineCount := 1 to config.height do
    begin
      screenline := config.screen[ScreenCount].line[LineCount].text;
      while decodeArgs(screenline, '$Rss', maxArgs, args, prefix, postfix,
        numargs) do
      begin
        // check if we have already seen this url:
        iFound := -1;
        for RSSLoop := 0 to MyRSSCount-1 do
          if (RSS[RSSLoop].url = LCDSmartieDisplayForm.Data.change(args[1])) then iFound := RSSLoop;

        iMaxFreq := 0;
        if (numargs >= 4) then
        begin
          try
            iMaxFreq := StrToInt(LCDSmartieDisplayForm.Data.change(args[4])) * 60;
          except
          end;
        end;

        if (iFound = -1) then
        begin
          fDataLock.Enter();

          try
            // not found - add details:
            if (MyRSSCount + 1 >= Length(RSS)) then
              SetLength(RSS, MyRSSCount + 10);

            if (RSS[MyRSSCount].url <> LCDSmartieDisplayForm.Data.change(args[1])) then
            begin
              RSS[MyRSSCount].url := LCDSmartieDisplayForm.Data.change(args[1]);
              RSS[MyRSSCount].whole := '';
              RSS[MyRSSCount].items := 0;
            end;

            RSS[MyRSSCount].maxfreq := 0;
            if (numargs >= 4) then
            begin
                RSS[MyRSSCount].maxfreq := iMaxFreq
            end;
          finally
            fDataLock.Leave();
          end;

          Inc(MyRSSCount);
        end
        else
        begin
          // seen this one before - raise the maxfreq if this one is higher.
          if (numargs >= 4)
            and (RSS[iFound].maxfreq < Cardinal(iMaxFreq)) then
          begin
            try
              fDataLock.Enter();
              try
                RSS[iFound].maxfreq := iMaxFreq;
              finally
                fDataLock.Leave();
              end;
            except
            end;
          end;
        end;

        // remove this RSS, and continue to parse the rest
        screenline := prefix + postfix;
      end;
    end;
  end;

  fDataLock.Enter();
  try
    RSSEntries := MyRSSCount;
  finally
    fDataLock.Leave();
  end;
  if (MyRSSCount > 0) then
    DoRSSHTTPUpdate;
end;

end.
