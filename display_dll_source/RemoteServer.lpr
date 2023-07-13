library RemoteServer;

{$MODE Delphi}

{.$R *.res}

uses
  IniFiles,Windows,SysUtils,SyncObjs,Math, IdContext, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdTCPServer, IdGlobal;

(*

 revhist

1.0 initial driver

*)

const
  DLLProjectName = 'Remote Display DLL';
  Version = 'v1.0';
  MaxConnections = 10;
  MaxWidth = 100;
  MaxHeight = 8;

type
   TRemoteServer = class(Tobject)
  Public
    procedure InitDisplay(SizeX: byte; SizeY: byte);
    procedure IdTCPServer1Connect(AContext: TIdContext);
    procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure IdTCPServer1Disconnect(AContext: TIdContext);
    procedure IdTCPServer1Exception(AContext: TIdContext; Ex: Exception);
  end;

type
  TCustomArray = array[0..7] of byte;
  TCustomChars = array[1..8] of TCustomArray;

var
  RemoteServer : TRemoteServer;
  IdTCPServer1: TIdTCPServer;
  contexts: Array[1..MaxConnections] of TIdContext;
  CustomChars: TCustomChars;
  SzX,SzY: byte;
  CurrentX: byte;
  CurrentY: byte;
  FrameBuffer : array[1..MaxWidth*MaxHeight] of byte;
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

procedure TRemoteServer.IdTCPServer1Connect(AContext: TIdContext);
var
  loop, i, j: integer;
begin
  try
    AContext.Connection.IOHandler.DefStringEncoding := IndyTextEncoding_ASCII;
    AContext.Connection.IOHandler.DefAnsiEncoding := IndyTextEncoding_ASCII;
    for loop := 1 to MaxConnections do
    begin
      if not assigned(contexts[loop]) then
      begin
        contexts[loop] := AContext;
        // send the size
        contexts[loop].Connection.IOHandler.Write(Chr($FE)+'A'+Chr(szY)+Chr(SzX));


        // send the custom char list
        for i :=1 to 8 do
        begin
          contexts[loop].Connection.IOHandler.Write(Chr($FE)+'N'+Chr(i));
          contexts[loop].Connection.IOHandler.Write(CustomChars[i,0]);
          contexts[loop].Connection.IOHandler.Write(CustomChars[i,1]);
          contexts[loop].Connection.IOHandler.Write(CustomChars[i,2]);
          contexts[loop].Connection.IOHandler.Write(CustomChars[i,3]);
          contexts[loop].Connection.IOHandler.Write(CustomChars[i,4]);
          contexts[loop].Connection.IOHandler.Write(CustomChars[i,5]);
          contexts[loop].Connection.IOHandler.Write(CustomChars[i,6]);
          contexts[loop].Connection.IOHandler.Write(CustomChars[i,7]);
        end;

        // send the current frame buffer
        j := 1;
        for i := 1 to szY do
        begin
          l := i div SzX;
          contexts[loop].Connection.IOHandler.Write(Chr($FE)+'G'+Chr(1)+Chr(i));
          for j := j to j + SzX do
            contexts[loop].Connection.IOHandler.Write(FrameBuffer[j]);
        end;
        Break;
      end;
    end;
  except
  end;
end;

procedure TRemoteServer.IdTCPServer1Execute(AContext: TIdContext);
begin
  sleep(1); // we need this procedure for indy sockets to work even though it does nothing
end;

procedure TRemoteServer.IdTCPServer1Disconnect(AContext: TIdContext);
var
  loop: integer;
begin
  try
    for loop := 1 to MaxConnections do
      if assigned(contexts[loop]) then
        if contexts[loop].Equals(AContext) then
          contexts[loop] := nil;
  except
  end;
end;

procedure TRemoteServer.IdTCPServer1Exception(AContext: TIdContext; Ex: Exception);
begin

end;

procedure TRemoteServer.InitDisplay(SizeX: byte; SizeY: byte);
begin
  // save the size for display re-inits
  SzX := SizeX;
  SzY := SizeY;
  try
    IdTCPServer1 := TIdTCPServer.Create();
    IdTCPServer1.DefaultPort := 0;
    IdTCPServer1.OnConnect := IdTCPServer1Connect;
    IdTCPServer1.OnExecute := IdTCPServer1Execute;
    IdTCPServer1.OnDisconnect := IdTCPServer1Disconnect;
    IdTCPServer1.OnException := IdTCPServer1Exception;
    IdTCPServer1.Bindings.Add.IP   := '0.0.0.0';
    IdTCPServer1.Bindings.Add.Port := 6000;
    IdTCPServer1.Active := True;
  except
  end;
end;

procedure DISPLAYDLL_Write(Str : pchar); stdcall;
// write string
var
  loop : integer;
  S: string;
begin
  S := Str;
  Move(S[1], FrameBuffer[1+(CurrentY-1)*szX], szX);
  try
    for loop := 1 to MaxConnections do
      if assigned(contexts[loop]) then
        if contexts[loop].Connection.Connected then
          contexts[loop].Connection.IOHandler.Write(Str);
  except
  end;
end;

procedure DISPLAYDLL_SetPosition(X, Y: byte); stdcall;
var
  loop: integer;
begin
  CurrentX := X;
  CurrentY := Y;
  try
    for loop := 1 to MaxConnections do
      if assigned(contexts[loop]) then
        if contexts[loop].Connection.Connected then
          begin
           contexts[loop].Connection.IOHandler.Write(Chr($FE)+'G'+Chr(x)+Chr(y));
          end;
  except
  end;
end;

procedure DISPLAYDLL_CustomChar(index : byte; Data : TCustomArray); stdcall;
var
  loop: integer;
// define custom character
begin
  // save the custom chars for display re-inits
  CustomChars[index] := Data;
  try
    for loop := 1 to MaxConnections do
      if assigned(contexts[loop]) then
        if contexts[loop].Connection.Connected then
          begin
            contexts[loop].Connection.IOHandler.Write(Chr($FE)+'N'+Chr(index));
            contexts[loop].Connection.IOHandler.Write(Data[0]);
            contexts[loop].Connection.IOHandler.Write(Data[1]);
            contexts[loop].Connection.IOHandler.Write(Data[2]);
            contexts[loop].Connection.IOHandler.Write(Data[3]);
            contexts[loop].Connection.IOHandler.Write(Data[4]);
            contexts[loop].Connection.IOHandler.Write(Data[5]);
            contexts[loop].Connection.IOHandler.Write(Data[6]);
            contexts[loop].Connection.IOHandler.Write(Data[7]);
        end;
  except
  end;
end;

function DISPLAYDLL_ReadKey : word; stdcall;
// return 00xx upon success, FF00 on fail
var
  B : byte;
  loop: integer;
begin
  Result := $FF00;
  try
    for loop := 1 to MaxConnections do
    if assigned(contexts[loop]) then
    begin
      if contexts[loop].Connection.IOHandler.InputBuffer.Size > 0 then
      begin
       Result := contexts[loop].Connection.IOHandler.ReadByte;

      end;
    end;
  except
  end;
end;

function DISPLAYDLL_Init(SizeX,SizeY : byte; StartupParameters : pchar; OK : pboolean) : pchar; stdcall;
begin
  OK^ := true;
  Result := PChar(DLLProjectName + ' ' + Version + #0);
  //try
    RemoteServer.InitDisplay(SizeX, SizeY);
  //except
  //end;
end;

procedure DISPLAYDLL_Done(); stdcall;
var
  loop: Byte;
begin
  try
    for loop := 1 to MaxConnections do
    if assigned(contexts[loop]) then
      if contexts[loop].Connection.Connected then
        contexts[loop].Connection.Disconnect();

    IdTCPServer1.Active:=false; // or we get exception
  except
  end;
end;

function DISPLAYDLL_DefaultParameters : pchar; stdcall;
begin
  DISPLAYDLL_DefaultParameters := pchar('COM1,9600,8,N,1' + #0);
end;

function DISPLAYDLL_Usage : pchar; stdcall;
begin
  Result := pchar('Usage: COM1,9600,8,N,1' + #0);
end;

function DISPLAYDLL_DriverName : pchar; stdcall;
begin
  Result := PChar(DLLProjectName + ' ' + Version + #0);
end;

// don't forget to export the funtions, else nothing works :)
exports
  DISPLAYDLL_Write,
  DISPLAYDLL_SetPosition,
  DISPLAYDLL_CustomChar,
  DISPLAYDLL_ReadKey,
  DISPLAYDLL_DefaultParameters,
  DISPLAYDLL_Usage,
  DISPLAYDLL_DriverName,
  DISPLAYDLL_Done,
  DISPLAYDLL_Init;
begin
end.

