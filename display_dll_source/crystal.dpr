library crystal;

{$R *.res}

(*

 revhist

1.0 initial driver
1.1 removed clearscreen on shutdown to allow custom shutdown message

*)

uses
  Windows,SysUtils,StrUtils,SyncObjs,SERPORT;

const
  DLLProjectName = 'Crystal Fontz Display DLL';
  Version = 'v1.1';

type
  pboolean = ^boolean;
  TCustomArray = array[0..7] of byte;

const
  MaxPacketLen = 23;
  MaxKeys = 20;

type
  TVersion = (Unknown, New633, Old633, V631);

  TLCD_CF = class
    procedure customChar(character: Integer; data: Array of Byte);
    procedure setPosition(x, y: Integer);
    procedure write(str: String);
    procedure setbacklight(on: Boolean);
    procedure setContrast(level: Integer);
    procedure setBrightness(level: Integer);
    function readKey(var key: Char): Boolean;
    procedure ClearScreen;
    constructor CreateSerial(StartupParameters : string);
    destructor Destroy; override;
  private
    iCF_cgrom : byte;
    COMPort : TSerialPort;
    xPos: Cardinal;
    yPos: Cardinal;
    bPackets: Boolean;
    packet: array [1..MaxPacketLen] of Byte;
    keyBuf: String;
    uiBytesAvail: Cardinal;
    uiMaxContrast: Cardinal;
    version: TVersion;
    csKeys: TCriticalSection;
    localeFormat : TFormatSettings;
    function CalcCrc(buffer: PByte; uiSize: Cardinal):Word;
    function PacketCmd(cmdCode: Byte): Boolean;  overload;
    function PacketCmd(cmdCode: Byte; data: Byte): Boolean;  overload;
    function PacketCmd(cmdCode: Byte; sData: String): Boolean;  overload;
    function PacketCmd(cmdCode: Byte; dataLen: Byte; buffer: PByte): Boolean; overload;
    function PacketReply(cmdCode: Byte): Boolean;
    procedure ProcessVersion(var packet: array of Byte);
    procedure AddKey(key: Char);
  end;

var
  LCD_CF : TLCD_CF = nil;

const
  {=packet command codes=}
  CmdPing = 0;
  CmdGetVersion = 1;
  CmdClearScreen = 6;
  CmdSetLine1 = 7;
  CmdSetLine2 = 8;
  CmdSetCustomChar = 9;
  CmdSetCursor = 12;
  CmdSetContrast = 13;
  CmdSetBrightness = 14;
  CmdKeyReporting = 23;
  CmdSendText = 31;

procedure TLCD_CF.AddKey(key: char);
begin
  csKeys.Enter;
  try
    keyBuf := keyBuf + key;
  finally
    csKeys.Leave;
  end;
end;

function TLCD_CF.readKey(var key: Char): Boolean;
begin
  // send/receive a ping - a side effect of this is processing of key events.
  if (bPackets) then
    PacketCmd(CmdPing);

  if (Length(keyBuf) > 0) then
  begin
    csKeys.Enter;
    try
      key := keyBuf[1];
      keyBuf := copy(keyBuf,2,length(keyBuf)-1);
    finally
      csKeys.Leave;
    end;
    Result := true;
  end
  else
    Result := false;
end;

function TLCD_CF.PacketCmd(cmdCode: Byte): Boolean;
begin
  result := PacketCmd(cmdCode, 0, nil);
end;

function TLCD_CF.PacketCmd(cmdCode: Byte; data: Byte): Boolean;
begin
  result := PacketCmd(cmdCode, 1, @data);
end;

function TLCD_CF.PacketCmd(cmdCode: Byte; sData: String): Boolean;
begin
  result := PacketCmd(cmdCode, Length(sData), @sData[1]);
end;

function TLCD_CF.PacketCmd(cmdCode: Byte; dataLen: Byte; buffer: PByte): Boolean;
var
  command: array of Byte;
  crc: Word;
  tries: Integer;
  pCmd: ^Byte;
  bOk: Boolean;
begin
  SetLength(command, dataLen+4);
  pCmd := @command[0];
  Inc(pCmd, 2);

  command[0] := cmdCode;
  command[1] := dataLen;
  if (dataLen > 0) then
    Move(buffer^, pCmd^, dataLen);
  crc := CalcCrc(@command[0], dataLen+2);
  command[dataLen+2] := Lo(crc);
  command[dataLen+3] := Hi(crc);

  tries := 0;
  repeat
    COMPort.Write(@command[0], dataLen+4);
    Inc(tries);
    bOk := PacketReply(cmdCode)
  until (bOk) or (tries >= 3);

  Result := bOk;
end;

function TLCD_CF.PacketReply(cmdCode: Byte): Boolean;
const
  CMDCODEPOS = 1;
  DATALENPOS = 2;
  KEY_UL = 13;  KEY_UR = 14;
  KEY_LL = 15;  KEY_LR = 16;
  KEY_UP = 1; KEY_DOWN = 2;
  KEY_LEFT = 3; KEY_RIGHT = 4;
  KEY_ENTER = 5; KEY_CANCEL = 6;

var
  uiStartTime: Cardinal;
  uiNow: Cardinal;
  bDone: Boolean;
  uiBytesRead: Cardinal;
  wCrc: Word;
  uiBytesToRemove: Cardinal;
begin
  uiStartTime := GetTickCount();
  bDone := false;
  uiBytesAvail := 0;
  uiBytesToRemove := 0;
  repeat
    // give the display a chance to do some work.
    //Sleep(1);

    assert(uiBytesAvail < MaxPacketLen);
    uiBytesRead := COMPort.Read(@packet[uiBytesAvail+1], MaxPacketLen - uiBytesAvail);
    uiBytesAvail := uiBytesAvail + uiBytesRead;

    //Check if we have enough data for a packet
    if (uiBytesAvail >= 4) then
    begin

      if (packet[DATALENPOS] < MaxPacketLen-4) then
      begin
        if (uiBytesAvail >= (packet[DATALENPOS]+4)) then
        begin
          // we have enough data for the packet.
          // check then crc
          wCrc := CalcCrc(@packet[1], packet[DATALENPOS]+2);

          if (packet[packet[DATALENPOS]+3] = Lo(wCrc))
            and (packet[packet[DATALENPOS]+4] = Hi(wCrc)) then
          begin
            // packet is valid.
            // Is it the wanted packet?
            if (packet[CMDCODEPOS] = ($40 or cmdCode)) then
            begin
              bDone := true;               // success

              // Check if it's a version packet - if so, process it now
              // (yes I know it's ugly).
              if (cmdCode = 1) and (packet[DATALENPOS]>=16) then // version packet
              begin
                ProcessVersion(packet);
              end;
            end
            else  // unhandled packet - ignore it
            begin
              if (packet[CMDCODEPOS] = $80) and (packet[DATALENPOS]=1) then
              begin
                // keyboard event
                case packet[3] of
                    // CFA631
                   KEY_UL: AddKey('A');
                   KEY_UR: AddKey('B');
                   KEY_LL: AddKey('C');
                   KEY_LR: AddKey('D');
                    // CFA633
                   KEY_UP: AddKey('E');
                   KEY_DOWN: AddKey('F');
                   KEY_LEFT: AddKey('G');
                   KEY_RIGHT: AddKey('H');
                   KEY_ENTER: AddKey('I');
                   KEY_CANCEL: AddKey('J');
                   // else ignore
                end;
              end;
              {else
                raise exception.create('unexpected packet: '
                  + IntToStr(packet[CMDCODEPOS]) + ':cmd=' + IntToStr(cmdCode));}
            end;

            uiBytesToRemove := packet[DATALENPOS] + 4;
          end
          else // crc is invaild, stream may be out of sync - remove discard one byte.
            uiBytesToRemove := 1;
        end;
      end
      else // data len is invaild, stream may be out of sync - remove discard one byte.
        uiBytesToRemove := 1;
    end;


    // Remove any unwanted data.
    if (uiBytesToRemove <> 0) then
    begin
      Dec(uiBytesAvail, uiBytesToRemove);
      if (uiBytesAvail > 0) then
        Move(packet[uiBytesToRemove+1], packet[1], uiBytesAvail);
      uiBytesToRemove := 0;
    end;

    uiNow := GetTickCount();
  until (bDone)
    or (uiNow < uiStartTime)
    or (uiNow - uiStartTime > 250);


  {if (not bDone) then
    raise exception.create('read timedout'); }

  Result := bDone;
end;

procedure TLCD_CF.ProcessVersion(var packet: array of Byte);
var
  sVersion: String;
  sFirmware: String;
begin
  //  Data will be 'CFA633:hX.X,fY.Y'
  //  my 633 gave: 'CFA633:h1.4,k1.9'
  //  my 631 gave: 'CFA631:h1.0,b1.0'
  SetLength(sVersion, packet[1]);
  Move(packet[2], sVersion[1], packet[1]);
  if (LeftStr(sVersion, 6) = 'CFA633') then
  begin
    version := New633;
    uiMaxContrast := 50;

    // Check if it has a firmware older than 1.9
    if (Pos(',', sVersion) <> 0) then
    begin
      sFirmware := RightStr(sVersion, (Length(sVersion)-Pos(',', sVersion)-1));
      try
        if (StrToFloat(sFirmware, localeFormat) < 1.9) then
          version := Old633;
      except
      end;
    end;

  end
  else if (LeftStr(sVersion, 6) = 'CFA631') then
  begin
    version := V631;
    uiMaxContrast := 255;
  end;
end;

constructor TLCD_CF.CreateSerial(StartupParameters : string);
var
  S2 : string;
begin
  csKeys := TCriticalSection.Create;
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, localeFormat);

  S2 := SubString(StartupParameters)+','+SubString(StartupParameters);
  iCF_cgrom := 1;
  try
    iCF_cgrom := StrToInt(SubString(StartupParameters));
  except
    iCF_cgrom := 1;
  end;
  COMPort := TSerialPort.Create;
  StartupParameters := S2 + ',8,N,1,$';
  StartupParameters := StartupParameters + IntToHex(SetRTSFlag+SetDTRFlag,8);
  COMPort.OpenSerialPort(StartupParameters);

  bPackets := false;
  // Test what kind of display we have:
  // we send: 0x0, 0x0, 0x47, 0x0f, which:
  //  - on a packet based display will cause an ack packet to be sent back.
  //  - on an 'ascii' based display it will mean:
  //    0x0: null
  //    0x0: null
  //    0x47: G
  //    0x0f: set contrast - wants one more character
  //   and no ack will be sent.
  if (PacketCmd(CmdPing)) then
  begin
    // understands packet protocol
    bPackets := true;
  end;

  if (bPackets) then
  begin
    PacketCmd(CmdGetVersion); // this is ugly; the reply is handled in the reading code
    PacketCmd(CmdSetCursor, 0); // hide cursor
    // ask for all keys to be reported
    if (version = New633) or (version = Old633) then
      PacketCmd(CmdKeyReporting, ''+#$3f+#0)
    else if (version = V631) then
      PacketCmd(CmdKeyReporting, ''+#$f+#0);

  end
  else
  begin
    COMPort.WriteByte(0);   // sync up the ascii stream (it's waiting for contrast level)
    COMPort.WriteByte(4);   // Hide cursor
    COMPort.WriteByte(20);  // Scroll off.
    COMPort.WriteByte(24);  // Wrap off.
    COMPort.WriteByte(22);  // Disable scrolling marquee
    COMPort.WriteByte(255);
    COMPort.WriteByte(1);
    COMPort.WriteByte(50);
    COMPort.WriteByte(3);   // Restore display
  end;

  ClearScreen;
end;


destructor TLCD_CF.Destroy;
begin
  if (Assigned(COMPort)) then
  begin
    setbacklight(false);
//  ClearScreen;
    COMPort.Destroy;

    if Assigned(csKeys) then csKeys.Free;
  end;
 
  inherited;
end;

procedure TLCD_CF.ClearScreen;
begin
  if (bPackets) then
    PacketCmd(CmdClearScreen)
  else
    COMPort.WriteByte(12);
end;

// level is in the range of 0-100
procedure TLCD_CF.setContrast(level: Integer);
begin
  if (bPackets) then
  begin
    // range 0-255 on CFA631, 0-50 on CFA633!
    if (uiMaxContrast > 0) then
      PacketCmd(CmdSetContrast, (Cardinal(level)*uiMaxContrast) div 100);
  end
  else
  begin
    // range 0-100
    COMPort.WriteByte(15);
    COMPort.WriteByte(level);
  end;
end;

procedure TLCD_CF.setBrightness(level: Integer);
begin
  if (bPackets) then
  begin
    PacketCmd(CmdSetBrightness, level);
  end
  else
  begin
    COMPort.WriteByte(14);
    COMPort.WriteByte(level);
  end;
end;


procedure TLCD_CF.setbacklight(on: Boolean);
begin

  if (on) then
    setBrightness(100)
  else
    setBrightness(0);
end;

procedure TLCD_CF.setPosition(x, y: Integer);
begin
  if (bPackets) then
  begin
    xPos := x;
    yPos := y;
  end
  else
  begin
    // This is what basiep used to do...
    if (x=1) and (y=1) then
    begin
      COMPort.WriteByte(3);   // Restore display
      COMPort.WriteByte(4);   // Hide cursor
      COMPort.WriteByte(20);  // Scroll off.
    end;

    COMPort.WriteByte(17);
    COMPort.WriteByte(x-1);
    COMPort.WriteByte(y-1);
  end;
end;

procedure TLCD_CF.write(str: String);
var
  buffer: array of Byte;
  i: Cardinal;
begin

  {================ map characters onto displays character set =============}

  // The full block character can be mapped to the normal character set
  // but is currently custom defined so users can remap it.
  // Ideally we will need to know the difference betwen a normal full block
  // and a full block used in the graph.

  // Now handle all simple byte to byte mappings
  for i:= 1 to Length(str) do
  begin
    // Custom characters - these mapping exists only to be compatible with older
    // smartie releases.
    case Ord(str[i]) of
      Ord('ž') {158}: str[i]:=Chr(1);
      131: str[i]:=Chr(2);
      132: str[i]:=Chr(3);
      133: str[i]:=Chr(4);
      134: str[i]:=Chr(5);
      135: str[i]:=Chr(6);
      136: str[i]:=Chr(7);
    end;

    // packet based display, with cgrom v2
    if (bPackets) and (iCF_cgrom = 2) then
    begin
      case Ord(str[i]) of
        Ord('^') {94}: str[i]:= Chr(29);
        //Ord('ž') {158}: str[i]:= Chr(31); // needs to be redefinable
      end;
    end;

    // v1 cgrom
    if (iCF_cgrom = 1) then
    begin
       { // map to cgrom - v1.0 displays
           '\' and '~' are not ascii mapped, but also don't appear in the cgrom...
       }
       if (str[i] = '°') {176} then str[i] := Chr(0); // custom char
       //if (str[i] = 'ž') then str[i] := Chr(255); // needs to be redefinable
    end
    else
    begin
      // v2 cgrom
      case Ord(str[i]) of
        Ord('°') {176}: str[i]:=Chr(0); // custom char

        Ord('$') {36}: str[i]:= Chr(162); // was 202
        Ord('@') {64}: str[i]:= Chr(160);
        Ord('[') {91}: str[i]:= Chr(250);
        Ord('\') {92}: str[i]:= Chr(251);
        Ord(']') {93}: str[i]:= Chr(252);
        Ord('_') {95}: str[i]:= Chr(196);
        Ord('`') {96}: str[i]:= Chr(39); // ` not in cgrom, mapped to ' instead
        Ord('’') {146}: str[i]:= Chr(39);
        Ord('{') {123}: str[i]:= Chr(253);
        Ord('|') {124}: str[i]:= Chr(254);
        Ord('}') {125}: str[i]:= Chr(255);
        Ord('~') {126}: str[i]:= Chr(206);
        Ord('£') {163}: str[i]:= Chr(161);
      end;
    end;
    // on ascii based displays, map on to special char locations
    if (not bPackets) and (Ord(str[i])<=7) then str[i]:=Chr(Ord(str[i])+128);
  end;

  // ascii based display with cgrom v2
  if (not bPackets) and (iCF_cgrom = 2) then
  begin
    str := StringReplace(str, '^', #30+#1+#29, [rfReplaceAll]);
    //str := StringReplace(str, Chr(158) {ž}, #30+#1+#31, [rfReplaceAll]);
    assert(Pos('^', str)=0);
    assert(Pos(Chr(158), str)=0);
  end;

  {==== Now write it! ====}

  if (bPackets) then
  begin
    if (version = Old633) then
    begin
      if (xPos = 1) then
        PacketCmd(CmdSetLine1, str)
      else
        PacketCmd(CmdSetLine2, str);
    end
    else
    begin
      SetLength(buffer, Length(str)+2);
      buffer[0] := xPos-1;
      buffer[1] := yPos-1;
      Move(str[1], buffer[2], Length(str));
      PacketCmd(CmdSendText, Length(str)+2, @buffer[0]);
    end;

    xPos := xPos + Cardinal(Length(str));
  end
  else
  begin
    COMPort.Write(@str[1], Length(str));
  end;
end;


procedure TLCD_CF.customChar(character: Integer; data: Array of Byte);
var
  buffer: Array [0..8] of Byte;
begin

  if (bPackets) then
  begin
    buffer[0] := character-1;
    Move(data[0], buffer[1], 8);
    PacketCmd(CmdSetCustomChar, 9, @buffer[0]);
  end
  else
  begin
    COMPort.WriteByte(25);           //this starts the custom characters
    COMPort.WriteByte(character-1);  //00 to 07 for 8 custom characters.
    COMPort.Write(@data[0], 8);
  end;
end;

function TLCD_CF.CalcCrc(buffer: PByte; uiSize: Cardinal):Word;
const
  crcs: array [0..$ff] of word =
    ($0000, $1189, $2312, $329b, $4624, $57ad, $6536, $74bf,
    $8c48, $9dc1, $af5a, $bed3, $ca6c, $dbe5, $e97e, $f8f7,
    $1081, $0108, $3393, $221a, $56a5, $472c, $75b7, $643e,
    $9cc9, $8d40, $bfdb, $ae52, $daed, $cb64, $f9ff, $e876,
    $2102, $308b, $0210, $1399, $6726, $76af, $4434, $55bd,
    $ad4a, $bcc3, $8e58, $9fd1, $eb6e, $fae7, $c87c, $d9f5,
    $3183, $200a, $1291, $0318, $77a7, $662e, $54b5, $453c,
    $bdcb, $ac42, $9ed9, $8f50, $fbef, $ea66, $d8fd, $c974,
    $4204, $538d, $6116, $709f, $0420, $15a9, $2732, $36bb,
    $ce4c, $dfc5, $ed5e, $fcd7, $8868, $99e1, $ab7a, $baf3,
    $5285, $430c, $7197, $601e, $14a1, $0528, $37b3, $263a,
    $decd, $cf44, $fddf, $ec56, $98e9, $8960, $bbfb, $aa72,
    $6306, $728f, $4014, $519d, $2522, $34ab, $0630, $17b9,
    $ef4e, $fec7, $cc5c, $ddd5, $a96a, $b8e3, $8a78, $9bf1,
    $7387, $620e, $5095, $411c, $35a3, $242a, $16b1, $0738,
    $ffcf, $ee46, $dcdd, $cd54, $b9eb, $a862, $9af9, $8b70,
    $8408, $9581, $a71a, $b693, $c22c, $d3a5, $e13e, $f0b7,
    $0840, $19c9, $2b52, $3adb, $4e64, $5fed, $6d76, $7cff,
    $9489, $8500, $b79b, $a612, $d2ad, $c324, $f1bf, $e036,
    $18c1, $0948, $3bd3, $2a5a, $5ee5, $4f6c, $7df7, $6c7e,
    $a50a, $b483, $8618, $9791, $e32e, $f2a7, $c03c, $d1b5,
    $2942, $38cb, $0a50, $1bd9, $6f66, $7eef, $4c74, $5dfd,
    $b58b, $a402, $9699, $8710, $f3af, $e226, $d0bd, $c134,
    $39c3, $284a, $1ad1, $0b58, $7fe7, $6e6e, $5cf5, $4d7c,
    $c60c, $d785, $e51e, $f497, $8028, $91a1, $a33a, $b2b3,
    $4a44, $5bcd, $6956, $78df, $0c60, $1de9, $2f72, $3efb,
    $d68d, $c704, $f59f, $e416, $90a9, $8120, $b3bb, $a232,
    $5ac5, $4b4c, $79d7, $685e, $1ce1, $0d68, $3ff3, $2e7a,
    $e70e, $f687, $c41c, $d595, $a12a, $b0a3, $8238, $93b1,
    $6b46, $7acf, $4854, $59dd, $2d62, $3ceb, $0e70, $1ff9,
    $f78f, $e606, $d49d, $c514, $b1ab, $a022, $92b9, $8330,
    $7bc7, $6a4e, $58d5, $495c, $3de3, $2c6a, $1ef1, $0f78);

var
  x: Cardinal;
  crc: Word;
begin
  crc := $ffff;
  for x := 0 to uiSize-1 do
  begin
    crc := Hi(crc) xor crcs[ buffer^ xor Lo(crc) ];
    Inc(buffer);
  end;

  Result := not crc;
end;


/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

function DISPLAYDLL_Init(SizeX,SizeY : byte; StartupParameters : pchar; OK : pboolean) : pchar; stdcall;
// return startup error
// open port
var
  S : string;
begin
  OK^ := true;
  Result := PChar(DLLProjectName + ' ' + Version + #0);
  try
    S := uppercase(string(StartupParameters));
    LCD_CF := TLCD_CF.CreateSerial(S);
  except
    on E: Exception do begin
      result := PChar('CRYSTAL.DLL Exception: ' + E.Message + #0);
      OK^ := false;
    end;
  end;
end;

procedure DISPLAYDLL_Done(); stdcall;
// close port
begin
  try
    if assigned(LCD_CF) then begin
      LCD_CF.Free;
      LCD_CF := nil;
    end;
  except
  end;
end;


function DISPLAYDLL_ReadKey : word; stdcall;
// return 00xx upon success, FF00 on fail
var
  B : byte;
begin
  Result := $FF00;
  try
    if assigned(LCD_CF) then begin
      if LCD_CF.ReadKey(char(B)) then
        Result := B;
    end;
  except
  end;
end;

procedure DISPLAYDLL_Write(Str : pchar); stdcall;
// write string
var
  S : string;
begin
  try
    if assigned(LCD_CF) then begin
      S := string(Str);
      LCD_CF.Write(S);
    end;
  except
  end;
end;

procedure DISPLAYDLL_CustomChar(Chr : byte; Data : TCustomArray); stdcall;
// define custom character
begin
  try
    if assigned(LCD_CF) then
       LCD_CF.CustomChar(Chr,Data);
  except
  end;
end;

procedure DISPLAYDLL_SetBacklight(LightOn : boolean); stdcall;
// turn on backlighting
begin
  try
    if assigned(LCD_CF) then
       LCD_CF.SetBacklight(LightOn);
  except
  end;
end;

procedure DISPLAYDLL_SetContrast(Contrast : byte); stdcall;
begin
// level is in the range of 0-100
  Contrast := round(cardinal(Contrast)*100 div 255);
  try
    if assigned(LCD_CF) then
       LCD_CF.SetContrast(Contrast);
  except
  end;
end;

procedure DISPLAYDLL_SetBrightness(Brightness : byte); stdcall;
begin
// level is in the range of 0-100
  Brightness := round(cardinal(Brightness)*100 div 255);
  try
    if assigned(LCD_CF) then
       LCD_CF.SetBrightness(Brightness);
  except
  end;
end;

procedure DISPLAYDLL_SetPosition(X, Y: byte); stdcall;
// set cursor position
begin
  try
    if assigned(LCD_CF) then
       LCD_CF.SetPosition(X,Y);
  except
  end;
end;

function DISPLAYDLL_DefaultParameters : pchar; stdcall;
begin
  DISPLAYDLL_DefaultParameters := pchar('COM1,9600,2' + #0);
end;

function DISPLAYDLL_Usage : pchar; stdcall;
begin
  Result := pchar('Usage: COM1,9600,v'+#13#10+'v = CGROM version (1 or 2)' + #0);
end;

function DISPLAYDLL_DriverName : pchar; stdcall;
begin
  Result := PChar(DLLProjectName + ' ' + Version + #0);
end;

// don't forget to export the funtions, else nothing works :)
exports
  DISPLAYDLL_SetBrightness,
  DISPLAYDLL_SetContrast,
  DISPLAYDLL_SetBacklight,
  DISPLAYDLL_ReadKey,
  DISPLAYDLL_CustomChar,
  DISPLAYDLL_Write,
  DISPLAYDLL_SetPosition,
  DISPLAYDLL_DefaultParameters,
  DISPLAYDLL_Usage,
  DISPLAYDLL_DriverName,
  DISPLAYDLL_Done,
  DISPLAYDLL_Init;
begin
end.

