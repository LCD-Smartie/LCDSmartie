unit URLThread;

interface

uses
  DataThread,IdHTTP,IdSSLOpenSSL,Classes;

type
  PHttp = ^TIdHttp;

  TURLThread = class(TDataThread)
  private
    httpCopy: PHttp;   // so we can cancel the request.
  protected
  public
    constructor Create(AInterval : longint);
    destructor Destroy; override;
    function GetUrl(Url: String; maxfreq: Cardinal = 0; PostParameters: TStringList = nil): String; virtual;
  end;


implementation

uses
  DateUtils,SysUtils,UConfig,UUtils;

constructor TURLThread.Create(AInterval : longint);
begin
  httpCopy := nil;
  inherited;
end;

destructor TURLThread.Destroy;
begin
  fDataLock.Enter();
  if assigned(httpCopy) then
    httpCopy^.Disconnect;
  fDataLock.Leave();
  inherited;
end;

// Download URL and return file location.
// Just return cached file if newer than maxfreq minutes.
function TURLThread.GetUrl(Url: String; maxfreq: Cardinal = 0; PostParameters: TStringList = nil): String;
var
  HTTP: TIdHTTP;
  Id_HandlerSocket : TIdSSLIOHandlerSocketOpenSSL;
  sl: TStringList;
  Filename: String;
  lasttime: TDateTime;
  toonew: Boolean;
  sRest: String;
  iRest: Integer;
  i: Integer;

begin
  // Generate a filename for the cached Rss stream.
  Filename := copy(LowerCase(Url),1,30);
  sRest := copy(LowerCase(Url),30,length(Url)-30);

  Filename := StringReplace(Filename, 'http://', '_', [rfReplaceAll]);
  Filename := StringReplace(Filename, 'https://', '_', [rfReplaceAll]);
  Filename := StringReplace(Filename, '\', '_', [rfReplaceAll]);
  Filename := StringReplace(Filename, ':', '_', [rfReplaceAll]);
  Filename := StringReplace(Filename, '/', '_', [rfReplaceAll]);
  Filename := StringReplace(Filename, '"', '_', [rfReplaceAll]);
  Filename := StringReplace(Filename, '|', '_', [rfReplaceAll]);
  Filename := StringReplace(Filename, '<', '_', [rfReplaceAll]);
  Filename := StringReplace(Filename, '>', '_', [rfReplaceAll]);
  Filename := StringReplace(Filename, '&', '_', [rfReplaceAll]);
  Filename := StringReplace(Filename, '?', '_', [rfReplaceAll]);
  Filename := StringReplace(Filename, '=', '_', [rfReplaceAll]);
  Filename := StringReplace(Filename, '.', '_', [rfReplaceAll]);
  Filename := StringReplace(Filename, '%', '_', [rfReplaceAll]);
  iRest := 0;
  for i := 1 to length(sRest) do
  begin
     iRest := iRest + (Ord(sRest[i]) xor i);
  end;
  Filename := Filename + IntToHex(iRest, 0);
  Filename :=  extractfilepath(paramstr(0)) + 'cache\\' + Filename + '.cache';

  try
    toonew := false;
    sl := TStringList.create;
    HTTP := TIdHTTP.Create(nil);

    HTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:12.0) Gecko/20100101 Firefox/12.0';
    Id_HandlerSocket := TIdSSLIOHandlerSocketOpenSSL.Create( HTTP );
    Id_HandlerSocket.SSLOptions.Mode := sslmClient;
    Id_HandlerSocket.SSLOptions.Method := sslvSSLv23;
    HTTP.IOHandler := Id_HandlerSocket;

    try
      // Only fetch new data if it's newer than the cache files' date.
      // and if it's older than maxfreq hours.
      if FileExists(Filename) then
      begin
        lasttime := FileDateToDateTime(FileAge(Filename));
        if (MinutesBetween(Now, lasttime) < maxfreq) then toonew := true;
        HTTP.Request.LastModified := lasttime;
      end;

      if (not toonew) then
      begin
        HTTP.HandleRedirects := True;
        if (config.httpProxy <> '') and (config.httpProxyPort <> 0) then
        begin
          HTTP.ProxyParams.ProxyServer := config.httpProxy;
          HTTP.ProxyParams.ProxyPort := config.httpProxyPort;
        end;
        HTTP.ReadTimeout := 30000;  // 30 seconds

        if (Terminated) then raise EExiting.Create('');

        fDataLock.Enter();
        httpCopy := @HTTP;
        fDataLock.Leave();

        {
        if (PostParameters.Count > 0) then
        sl.Text := HTTP.Post(Url, Parameters)
        else
        }
        sl.Text := HTTP.Get(Url);
        // the get call can block for a long time so check if smartie is exiting
        if (Terminated) then raise EExiting.Create('');

        sl.savetofile(Filename);
      end;
    finally
      fDataLock.Enter();
      httpCopy := nil;
      fDataLock.Leave();

      sl.Free;
      HTTP.Free;
    end;
  except
    on E: EIdHTTPProtocolException do
    begin
      if (Terminated) then raise EExiting.Create('');
      if (E.ErrorCode <> 304) then   // 304=Not Modified.
        raise;
    end;

    else
    begin
      if (Terminated) then raise EExiting.Create('');
      raise;
    end;
  end;

  // Even if we fail to download - give the filename so they can use the old data.
  Result := filename;
end;

end.
