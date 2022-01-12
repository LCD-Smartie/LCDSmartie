library irtrans;

uses
  Windows,Sockets,Classes,SysUtils;

{$R *.res}

const
  DLLProjectName = 'IRTrans Display DLL';
  Version = 'v1.0';
type
  pboolean = ^boolean;
  TCustomArray = array[0..7] of byte;
var
  ClientSocket: TTcpClient;

type  // can't use Timers (VCL component) in DLLs, so a separate thread is needed
  TSocketHealthThread = class(TThread)
  private
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TSocketHealthThread.Create;
begin
  FreeOnTerminate := false;
  inherited Create(true);  // create suspended
end;

destructor TSocketHealthThread.Destroy;
begin
  inherited Destroy;
end;

procedure TSocketHealthThread.Execute;
var
  LastSocketCheck : dword;
begin
  LastSocketCheck := gettickcount;
  while not Terminated do begin
    if ((gettickcount - LastSocketCheck) > 5000) then begin
      if not ClientSocket.Active then ClientSocket.Active := true;
      LastSocketCheck := gettickcount;
    end;
    sleep(100);
  end;
end;

const
  STATUS_RECEIVE = 4;

  LCD_BACKLIGHT = 1;
  LCD_TEXT = 2;

  COMMAND_LCD = 15;
  COMMAND_BRIGHTNESS = 44;
  COMMAND_DEFINECHAR = 45;

type
  TSTATUSBUFFER = record
    clientid : dword;
    statuslen : word;
    statustype : word;
    adress : word;
    align : array[1..2] of byte;
    data : array[1..32768] of char
  end;

  TNETWORKCOMMAND = record
    netcommand : byte;
    mode : byte;
    timeout : word;
    adress : dword;
    protocol_version : dword;
    remote : array[0..79] of byte;
    command : array[0..19] of byte;
    trasmit_freq : byte;
  end;

  TLCDCOMMAND = record
    netcommand : byte;
    mode : byte;
    lcdcommand : byte;
    timeout : byte;
    adress : dword;
    protocol_version : dword;
    wid : byte;
    hgt : byte;
    framebuffer : array[0..199] of char;
  end;

var
  ID : longint;
  MyX,MyY : integer;
  LCDComRec : TLCDCOMMAND;
  NetworkCommand : TNETWORKCOMMAND;
  SocketHealthThread: TSocketHealthThread;
//  procedure ClientSocketError(Sender: TObject; SocketError: Integer);
//  procedure ClientSocketConnect(Sender: TObject);


procedure DISPLAYDLL_SetPosition(X, Y: byte); stdcall;
// set cursor position
begin
  MyX := X;
  MyY := Y;
end;

procedure DISPLAYDLL_Write(Str : pchar); stdcall;
var
  Index : integer;
  buf : TSTATUSBUFFER;
  res : integer;
  statustimeout : dword;
begin
  try
    LCDComRec.netcommand := COMMAND_LCD;
    LCDComRec.adress := ord('L');
    Index := (MyY-1)*40 + (MyX-1);
    strcopy(pchar(@LCDComRec.framebuffer[Index]),Str);
    if ClientSocket.Connected then begin
      // send the displays' frame buffer
      ClientSocket.SendBuf(LCDComRec,sizeof(LCDComRec));
      // wait for the servers response
      statustimeout := gettickcount;
      repeat
        res := ClientSocket.ReceiveBuf(buf,32768);
        if (res = 8) then break;
      until (buf.statustype = STATUS_RECEIVE) or ((gettickcount - statustimeout) > 2000);
    end;
  except
  end;
end;

procedure DISPLAYDLL_CustomChar(Chr : byte; Data : TCustomArray); stdcall;
// define custom character
begin
  chr := ((chr - 1) mod 8) + 1;  // characters 1-8
  Move(data[0], NetworkCommand.Remote[Chr*9-7], 8);
  try
    if ClientSocket.Connected then
      ClientSocket.SendBuf(NetworkCommand,sizeof(NetworkCommand));
  except
  end;
end;

function DISPLAYDLL_CustomCharIndex(Index : byte) : byte; stdcall;
begin
  DISPLAYDLL_CustomCharIndex := Index;
end;

procedure DISPLAYDLL_SetBrightness(Brightness : byte); stdcall;
// 0 - 255
begin
  LCDComRec.netcommand := COMMAND_BRIGHTNESS;
  LCDComRec.adress := Brightness div 64;  // 0-3 is the brightness
  try
    if ClientSocket.Connected then
      ClientSocket.SendBuf(LCDComRec,sizeof(LCDComRec));
  except
  end;
end;

type
  TLCD_IR = class
  public
    constructor Create;
    destructor Destroy; override;
  private
    procedure ClientSocketError(Sender: TObject; SocketError: Integer);
    procedure ClientSocketConnect(Sender: TObject);
  end;

constructor TLCD_IR.Create;
begin
end;

destructor TLCD_IR.Destroy;
begin
end;

procedure TLCD_IR.ClientSocketConnect(Sender: TObject);
begin
  try
    // we've just connected, send our ID and the custom character set
    ClientSocket.SendBuf(ID,4);
    ClientSocket.SendBuf(NetworkCommand,sizeof(NetworkCommand));
  except
  end;
end;

var
  Sockethandler : TLCD_IR = nil;

procedure TLCD_IR.ClientSocketError(Sender: TObject; SocketError: Integer);
begin
  // got an error, close the socket
  ClientSocket.Close;
end;

function DISPLAYDLL_Init(SizeX,SizeY : byte; StartupParameters : pchar; OK : pboolean) : pchar; stdcall;
// return startup error
// open port
var
  Loop : integer;
begin
  OK^ := true;
  Result := PChar(DLLProjectName + ' ' + Version + #0);
  try
    MyX := 1;
    MyY := 1;

    // our ID to the IRTrans server.  Anything but 0 seems to fail
    ID := 0;

    // custom characters, sent when the socket is established (in case the connection drops while LCDSmartie is running)
    fillchar(NetworkCommand,sizeof(NetworkCommand),$00);
    NetworkCommand.netcommand := COMMAND_DEFINECHAR;
    NetworkCommand.protocol_version := 200;
    NetworkCommand.Remote[0] := 8;
    for Loop := 1 to 8 do
      NetworkCommand.Remote[Loop*9-8] := Loop;

    // custom characters, sent when the socket is established (in case the connection drops while LCDSmartie is running)
    fillchar(LCDComRec,sizeof(LCDComRec),$00);
    // these are 4x40 regardless of the size of the display.  The IRTrans server sorts out the actual display size!
    LCDComRec.hgt := 4;
    LCDComRec.wid := 40;
    LCDComRec.lcdcommand := LCD_BACKLIGHT + LCD_TEXT;
    LCDComRec.protocol_version := 200;

    // create the socket, try to bring it up
    try
      Sockethandler :=  TLCD_IR.Create;
      ClientSocket := TTcpClient.Create(nil);
      with ClientSocket do begin
        BlockMode := bmBlocking;
        RemoteHost := string(StartupParameters);
        RemotePort := '21000';
        OnError := SocketHandler.ClientSocketError;
        OnConnect := SocketHandler.ClientSocketConnect;
        Active := true;
      end;
    except
    end;

    // monitor the socket, if it's closed, bring it back up
    SocketHealthThread := TSocketHealthThread.Create;
    SocketHealthThread.Resume;
  except
    on E: Exception do begin
      result := PChar('IRTRANS.DLL Exception: ' + E.Message + #0);
      OK^ := false;
    end;
  end;
end;

procedure DISPLAYDLL_Done(); stdcall;
// close socket
begin
  SocketHealthThread.Terminate;
  SocketHealthThread.Free;
  ClientSocket.Free;
  SocketHandler.Free;
end;

function DISPLAYDLL_DefaultParameters : pchar; stdcall;
begin
  DISPLAYDLL_DefaultParameters := pchar('localhost' + #0);
end;

function DISPLAYDLL_Usage : pchar; stdcall;
begin
  DISPLAYDLL_Usage := pchar('Usage: serverhost'+#13#10+'usually 127.0.0.1 or localhost' + #0);
end;

function DISPLAYDLL_DriverName : pchar; stdcall;
begin
  Result := PChar(DLLProjectName + ' ' + Version + #0);
end;

// don't forget to export the funtions, else nothing works :)
exports
  DISPLAYDLL_SetBrightness,
  DISPLAYDLL_CustomChar,
  DISPLAYDLL_CustomCharIndex,
  DISPLAYDLL_Write,
  DISPLAYDLL_SetPosition,
  DISPLAYDLL_DefaultParameters,
  DISPLAYDLL_Usage,
  DISPLAYDLL_DriverName,
  DISPLAYDLL_Done,
  DISPLAYDLL_Init;
begin
end.

