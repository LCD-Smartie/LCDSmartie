unit UDataSender;

{$MODE Delphi}

interface

uses
  SysUtils, DataThread, IdGlobal, classes,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdSSLOpenSSL, UMain, IdExceptionCore;

const
  SenderKeyPrefix = '$Sender';

type
  TSenderInfo = record
  ClientActive: boolean;
  ClientSSL: boolean;
  ClientIP: string;
  ClientPort: string;
  LastClientIP: string;
  LastClientPort: string;
  Password: string;
end;

TSenderDataThread = class(TDataThread)
  private
    SenderInfo : TSenderInfo;
    IdTCPClient1: TIdTCPClient;
    cSSL: TIdSSLIOHandlerSocketOpenSSL;
    RLine: Array[1..4] of string;
    SkipRefresh: integer;
  protected
    function AllowRefresh : boolean; override;
    procedure  DoUpdate; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  ResolveVariables(var Line : string); override;
  end;


implementation

uses
  UUtils;

constructor TSenderDataThread.Create;
begin
    SenderInfo.ClientIP := '0';
  inherited Create(10);
end;

destructor TSenderDataThread.Destroy;
begin
  if SenderInfo.ClientActive then
  begin
    SenderInfo.ClientActive := false;
    IdTCPClient1.Disconnect(true);
    IdTCPClient1.Destroy;
  end;
  inherited;
end;

function TSenderDataThread.AllowRefresh : boolean;
begin
  Result := true;
end;

procedure TSenderDataThread.DoUpdate;
var
  Loop: integer;
  s: string;
   // technically we dont support lines greater than 100 but
   // I should make this fit MaxLineLen if there is one
  BufferArray :TIdBytes;
  linelen, linenum: integer;
  t: string;
begin
  // make a delay that depends on the refresh interval rather than sleeping in the update thread
  if (SkipRefresh > 0) then
  begin
    dec(SkipRefresh);
    exit;
  end;
  if not (SenderInfo.ClientIP = '0') then
  begin
    if not SenderInfo.ClientActive then
    begin
      IdTCPClient1 := TIdTCPClient.Create(nil);
      if SenderInfo.ClientSSL then
      begin
        cSSL := TIdSSLIOHandlerSocketOpenSSL.Create( IdTCPClient1 );
        cSSL.SSLOptions.SSLVersions := [sslvTLSv1_2];
        cSSL.PassThrough := false;
        IdTCPClient1.IOHandler := cSSL;
      end;
      if not (SenderInfo.ClientIP = '') and
        not (SenderInfo.ClientPort = '') then
      begin
        IdTCPClient1.Host := SenderInfo.ClientIP;
        IdTCPClient1.Port := strtoint(SenderInfo.ClientPort);

        SenderInfo.ClientActive := true;
        try
          IdTCPClient1.Connect;
          IdTCPClient1.ReadTimeout := 1000;
          IdTCPClient1.IOHandler.DefStringEncoding := IndyTextEncoding_ASCII;
          IdTCPClient1.IOHandler.DefAnsiEncoding := IndyTextEncoding_ASCII;
          IdTCPClient1.IOHandler.WriteLn(SenderInfo.Password+#13#10);
        except
          on E : Exception do
          begin
            RLine[1] := E.Message;
            RLine[2] := E.Message;
            RLine[3] := E.Message;
            RLine[4] := E.Message;
            IdTCPClient1.Destroy;
            SenderInfo.ClientActive := false;
            SkipRefresh := 100;  // don't hammer the server. SkipRefresh x refresh interval
            exit;
          end;
        end;
      end;
      SenderInfo.LastClientIP := SenderInfo.ClientIP;
      SenderInfo.LastClientPort := SenderInfo.ClientPort;
    end;

    if not (SenderInfo.LastClientIP = SenderInfo.ClientIP) or
      not (SenderInfo.LastClientPort = SenderInfo.ClientPort) then
    begin
      IdTCPClient1.Disconnect(true);
      IdTCPClient1.Destroy;
      SenderInfo.ClientActive := false; // address change force re-connect
    end;

    if SenderInfo.ClientActive then
     begin
      if SenderInfo.ClientActive then
      begin
        for Loop := 1 to 4 do begin
          try
            s := '';
            BufferArray := TIdBytes.Create;
            // We have to read byte at at time because readln re-encodes characters
            linenum := IdTCPClient1.IOHandler.ReadByte;
            linelen := IdTCPClient1.IOHandler.ReadByte;
            IdTCPClient1.IOHandler.ReadBytes(BufferArray, linelen, False);
            s := PAnsiChar(@BufferArray[0]);
            setlength(s, linelen);
            RLine[linenum] := s;
          except
            on E: EIdReadTimeOut do Exit;
            on E : Exception do
            begin
              RLine[1] := E.Message;
              RLine[2] := E.Message;
              RLine[3] := E.Message;
              RLine[4] := E.Message;
              SenderInfo.ClientActive := false;
              exit;
            end;
          end;
        end;
      end;
   end;

  end;
end;

procedure TSenderDataThread.ResolveVariables(var Line : string);
var
  args: Array [1..maxArgs] of String;
   prefix, postfix: String;
  numArgs: Cardinal;

begin
  if (pos(SenderKeyPrefix, Line) = 0) then exit;

  while decodeArgs(line, '$Sender', maxArgs, args, prefix, postfix, numargs) do
  begin
    RequiredParameters(numargs, 5, 5); //ipaddr,port,pass,ssl,line
    SenderInfo.ClientIP := LCDSmartieDisplayForm.Data.change(args[1]);
    SenderInfo.ClientPort := LCDSmartieDisplayForm.Data.change(args[2]);
    SenderInfo.Password := LCDSmartieDisplayForm.Data.change(args[3]);
    SenderInfo.ClientSSL := boolean(strtoint(LCDSmartieDisplayForm.Data.change(args[4])));

  Line := prefix;
     case (strtoint(LCDSmartieDisplayForm.Data.change(args[5][1]))) of
            1 : line := line+RLine[1];
            2 : line := line+RLine[2];
            3 : line := line+RLine[3];
            4 : line := line+RLine[4];
     end;
     Line := Line + postfix;
  end;
end;

end.
