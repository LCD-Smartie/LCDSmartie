unit UDataBoinc;

{$MODE Delphi}

interface

uses
  SysUtils, UConfig, URLThread;

const
  MaxHosts = 20;

type
  THostStats = record
    id: string;
    create_time: string;
    rpc_seqno: string;
    rpc_time: string;
    host_cpid: string;
    total_credit: string;
    expavg_credit: string;
    expavg_time: string;
    domain_name: string;
    p_ncpus: string;
    p_vendor: string;
    p_model: string;
    p_fpops: string;
    p_iops: string;
    os_name: string;
    os_version: string;
    m_nbytes: string;
    d_free: string;
    d_total: string;
    venue: string;
  end;

type
  TBoincStats = record
    BaseURL: string;
    authenticator: string;
    id: string;
    cpid: string;
    create_time: string;
    name: string;
    country: string;
    total_credit: string;
    expavg_credit: string;
    expavg_time: string;
    teamid: string;
    url: string;
    hoststats: array [1..MaxHosts] of THostStats;
  end;

Const
  EmptyBoincStats: TBoincStats =
   (id: 'BOINC: this server entry not configured';
    cpid: 'BOINC: this server entry not configured';
    create_time: 'BOINC: this server entry not configured';
    name: 'BOINC: this server entry not configured';
    country: 'BOINC: this server entry not configured';
    total_credit: 'BOINC: this server entry not configured';
    expavg_credit: 'BOINC: this server entry not configured';
    expavg_time: 'BOINC: this server entry not configured';
    teamid: 'BOINC: this server entry not configured';
    url: 'BOINC: this server entry not configured');

  DisabledBoincStats: TBoincStats =
   (id: 'BOINC: boinc is disabled';
    cpid: 'BOINC: boinc is disabled';
    create_time: 'BOINC: boinc is disabled';
    name: 'BOINC: boinc is disabled';
    country: 'BOINC: boinc is disabled';
    total_credit: 'BOINC: boinc is disabled';
    expavg_credit: 'BOINC: boinc is disabled';
    expavg_time: 'BOINC: boinc is disabled';
    teamid: 'BOINC: boinc is disabled';
    url: 'BOINC: boinc is disabled');

type
  TBOINCDataThread = class(TURLThread)
  private
    BoincStats: array [1..maxBoincAccounts] of TBoincStats;
    procedure DoBOINCHTTPUpdate(Server: String; ServerIndex: integer);
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
  laz2_XMLRead, laz2_DOM, UUtils, md5;

constructor TBOINCDataThread.Create;
begin
  inherited Create(60*60000);
end;

destructor TBOINCDataThread.Destroy;
begin
  inherited;
end;

function TBOINCDataThread.UsesCOMObjects : boolean;
begin
  result := true;
end;

procedure TBOINCDataThread.ResolveVariables(var Line : string);
var
  i, j: integer;
begin
  if (pos('$BOINC', line) <> 0) then
  begin
    fDataLock.Enter();
    try
      for i := 1 to maxBoincAccounts do
      begin
        line := StringReplace(line, '$BOINCid('+inttostr(i)+')', BoincStats[i].id, [rfReplaceAll]);
        line := StringReplace(line, '$BOINCcpid('+inttostr(i)+')', BoincStats[i].cpid, [rfReplaceAll]);
        line := StringReplace(line, '$BOINCcreate_time('+inttostr(i)+')', BoincStats[i].create_time, [rfReplaceAll]);
        line := StringReplace(line, '$BOINCname('+inttostr(i)+')', BoincStats[i].name, [rfReplaceAll]);
        line := StringReplace(line, '$BOINCcountry('+inttostr(i)+')', BoincStats[i].country, [rfReplaceAll]);
        line := StringReplace(line, '$BOINCtotal_credit('+inttostr(i)+')', BoincStats[i].total_credit, [rfReplaceAll]);
        line := StringReplace(line, '$BOINCexpavg_credit('+inttostr(i)+')', BoincStats[i].expavg_credit, [rfReplaceAll]);
        line := StringReplace(line, '$BOINCexpavg_time('+inttostr(i)+')', BoincStats[i].expavg_time, [rfReplaceAll]);
        line := StringReplace(line, '$BOINCteamid('+inttostr(i)+')', BoincStats[i].teamid, [rfReplaceAll]);
        line := StringReplace(line, '$BOINCurl('+inttostr(i)+')', BoincStats[i].url, [rfReplaceAll]);

        if (pos('$BOINCHost', line) <> 0) then
        begin
          for j := 1 to MaxHosts do
          begin
            line := StringReplace(line, '$BOINCHostid('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].id, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostcreate_time('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].create_time, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostrpc_seqno('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].rpc_seqno, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostrpc_time('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].rpc_time, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHosthost_cpid('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].host_cpid, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHosttotal_credit('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].total_credit, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostexpavg_credit('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].expavg_credit, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostexpavg_time('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].expavg_time, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostdomain_name('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].domain_name, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostp_ncpus('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].p_ncpus, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostp_vendor('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].p_vendor, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostp_model('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].p_model, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostp_fpops('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].p_fpops, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostp_iops('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].p_iops, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostos_name('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].os_name, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostos_version('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].os_version, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostm_nbytes('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].m_nbytes, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostd_free('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].d_free, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostd_total('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].d_total, [rfReplaceAll]);
            line := StringReplace(line, '$BOINCHostvenue('+inttostr(i)+','+inttostr(j)+')', BoincStats[i].hoststats[j].venue, [rfReplaceAll]);
          end;
        end;
      end;
    finally
      fDataLock.Leave();
    end;
  end;
end;

procedure TBOINCDataThread.DoBOINCHTTPUpdate(Server: String; serverindex: integer);
var
  XMLDoc: TXMLDocument;
  ANode, HostNode: TDOMNode;
  BoincFilename, reqURL, PasswordHash: String;
  i: integer;
begin
  // first get the server config to find the base rpc url
  reqURL := Server+'get_project_config.php';
  fDataLock.Enter();
  try
    BoincFilename := getUrl(reqURL, 60000*60);
    if (Terminated) then raise EExiting.Create('');
    if FileExists(BoincFilename) then
    begin
      ReadXMLFile(XMLDoc, BoincFilename);
      if (XMLDoc.documentElement.hasChildNodes) then
      begin
        ANode := XMLDoc.DocumentElement.FindNode('web_rpc_url_base');
        BoincStats[serverindex].BaseURL := stripHtml(ANode.TextContent);
      end;
    end;
  finally
    fDataLock.Leave();
  end;

  // perform the account lookup to get authenticator string
  PasswordHash := MD5Print(MD5String(config.boincAccount[ServerIndex].password+config.boincAccount[ServerIndex].user));
  reqURL := BoincStats[serverindex].BaseURL+'lookup_account.php?email_addr='+config.boincAccount[ServerIndex].user+'&passwd_hash='+PasswordHash+'&get_opaque_auth=1';
  fDataLock.Enter();
  try
    BoincFilename := getUrl(reqURL, 60000*60);
    if (Terminated) then raise EExiting.Create('');
    if FileExists(BoincFilename) then
    begin
      ReadXMLFile(XMLDoc, BoincFilename);
      if (XMLDoc.documentElement.hasChildNodes) then
      begin
        ANode := XMLDoc.DocumentElement.FindNode('authenticator');
        BoincStats[serverindex].authenticator := stripHtml(ANode.TextContent);
      end;
    end;
  finally
    fDataLock.Leave();
  end;

  // lookup the information on user
  reqURL := BoincStats[serverindex].BaseURL+'show_user.php?auth='+BoincStats[serverindex].authenticator+'&format=xml';
  try
    BoincFilename := getUrl(reqURL, 60000*60);
    if (Terminated) then raise EExiting.Create('');
    if FileExists(BoincFilename) then
    begin
      ReadXMLFile(XMLDoc, BoincFilename);
      if (XMLDoc.documentElement.hasChildNodes) then
      begin
        fDataLock.Enter();
        try
          // this lot should be under the user node. IDK why its working like this
          ANode := XMLDoc.DocumentElement.FindNode('id');
          BoincStats[serverindex].id := stripHtml(ANode.TextContent);
          ANode := XMLDoc.DocumentElement.FindNode('cpid');
          BoincStats[serverindex].cpid := stripHtml(ANode.TextContent);
          ANode := XMLDoc.DocumentElement.FindNode('create_time');
          BoincStats[serverindex].create_time := stripHtml(ANode.TextContent);
          ANode := XMLDoc.DocumentElement.FindNode('name');
          BoincStats[serverindex].name := stripHtml(ANode.TextContent);
          ANode := XMLDoc.DocumentElement.FindNode('country');
          BoincStats[serverindex].country := stripHtml(ANode.TextContent);
          ANode := XMLDoc.DocumentElement.FindNode('total_credit');
          BoincStats[serverindex].total_credit := stripHtml(ANode.TextContent);
          ANode := XMLDoc.DocumentElement.FindNode('expavg_credit');
          BoincStats[serverindex].expavg_credit := stripHtml(ANode.TextContent);
          ANode := XMLDoc.DocumentElement.FindNode('expavg_time');
          BoincStats[serverindex].expavg_time := stripHtml(ANode.TextContent);
          ANode := XMLDoc.DocumentElement.FindNode('teamid');
          BoincStats[serverindex].teamid := stripHtml(ANode.TextContent);
          ANode := XMLDoc.DocumentElement.FindNode('url');
          BoincStats[serverindex].url := stripHtml(ANode.TextContent);

          // Now get the host stuff
          HostNode := XMLDoc.DocumentElement.FindNode('host');
          i := 0;
          if HostNode <> nil then
          repeat
            Inc(i);
            ANode := HostNode.FindNode('id');
            // skip this round if theres no data
            if ANode = nil then continue;
            BoincStats[serverindex].hoststats[i].id := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('create_time');
            BoincStats[serverindex].hoststats[i].create_time := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('rpc_seqno');
            BoincStats[serverindex].hoststats[i].rpc_seqno := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('rpc_time');
            BoincStats[serverindex].hoststats[i].rpc_time := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('host_cpid');
            BoincStats[serverindex].hoststats[i].host_cpid := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('total_credit');
            BoincStats[serverindex].hoststats[i].total_credit := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('expavg_credit');
            BoincStats[serverindex].hoststats[i].expavg_credit := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('expavg_time');
            BoincStats[serverindex].hoststats[i].expavg_time := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('domain_name');
            BoincStats[serverindex].hoststats[i].domain_name := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('p_ncpus');
            BoincStats[serverindex].hoststats[i].p_ncpus := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('p_vendor');
            BoincStats[serverindex].hoststats[i].p_vendor := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('p_model');
            BoincStats[serverindex].hoststats[i].p_model := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('p_fpops');
            BoincStats[serverindex].hoststats[i].p_fpops := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('p_iops');
            BoincStats[serverindex].hoststats[i].p_iops := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('os_name');
            BoincStats[serverindex].hoststats[i].os_name := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('os_version');
            BoincStats[serverindex].hoststats[i].os_version := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('m_nbytes');
            BoincStats[serverindex].hoststats[i].m_nbytes := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('d_free');
            BoincStats[serverindex].hoststats[i].d_free := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('d_total');
            BoincStats[serverindex].hoststats[i].d_total := stripHtml(ANode.TextContent);
            ANode := HostNode.FindNode('venue');
            BoincStats[serverindex].hoststats[i].venue := stripHtml(ANode.TextContent);

            HostNode := HostNode.NextSibling;

          until (HostNode = nil) or (i >= MaxHosts);
        finally
          fDataLock.Leave();
        end;
      end;
    end;
  except
    on EExiting do raise;
    on E: Exception do
    begin
      fDataLock.Enter();
      try
        BoincStats[serverindex].id := '[BOINC: ' + E.Message + ']';
        BoincStats[serverindex].cpid := '[BOINC: ' + E.Message + ']';
        BoincStats[serverindex].create_time := '[BOINC: ' + E.Message + ']';
        BoincStats[serverindex].name := '[BOINC: ' + E.Message + ']';
        BoincStats[serverindex].country := '[BOINC: ' + E.Message + ']';
        BoincStats[serverindex].total_credit := '[BOINC: ' + E.Message + ']';
        BoincStats[serverindex].expavg_credit := '[BOINC: ' + E.Message + ']';
        BoincStats[serverindex].expavg_time := '[BOINC: ' + E.Message + ']';
        BoincStats[serverindex].teamid := '[BOINC: ' + E.Message + ']';
        BoincStats[serverindex].url := '[BOINC: ' + E.Message + ']';
      finally
        fDataLock.Leave();
      end;
    end;
  end;
  XMLDoc.Free;
end;

procedure TBOINCDataThread.DoUpdate;
var
  ServerIndex: integer;
begin
  if config.boincEnabled then
      for ServerIndex := 1 to maxBoincAccounts do
        if (config.boincAccount[ServerIndex].server <> '') and (config.boincAccount[ServerIndex].user <> '') and (config.boincAccount[ServerIndex].password <> '') then
          DoBOINCHTTPUpdate(config.boincAccount[ServerIndex].server, ServerIndex)
        else
          BoincStats[serverindex] := EmptyBoincStats
  else
    for ServerIndex := 1 to maxBoincAccounts do
      BoincStats[serverindex] := DisabledBoincStats;
end;

end.
