library HD44780_FTDI_BB;
{
  Driver for HD44780 bitbanged via FTDI USB serial
}
{$mode objfpc}{$H+}

uses
  windows, sysutils
  { you can add units after this };

type
  TFTDIInfo = array[0..7] of byte; // {Device index, D4, D5, D6, D7, E, RW, RS}

type
  TCustomArray = array[0..7] of byte;

function FT_Open(Index:Integer; ftHandle:Pointer):integer; stdcall; External 'FTD2XX.DLL' name 'FT_Open';
function FT_SetBitMode(ftHandle:Dword; Mask,Enable:Byte):integer; stdcall; External 'FTD2XX.DLL' name 'FT_SetBitMode';
function FT_SetBaudRate(ftHandle:Dword; BaudRate:DWord):integer; stdcall; External 'FTD2XX.DLL' name 'FT_SetBaudRate';
function FT_Write(ftHandle:Dword; FTOutBuf:Pointer; BufferSize:LongInt; ResultPtr:Pointer):integer; stdcall; External 'FTD2XX.DLL' name 'FT_Write';
function FT_Close(ftHandle:Dword):integer; stdcall; External 'FTD2XX.DLL' name 'FT_Close';

const
  FUNCTIONSET = $20;
  TWOLINE = $08;
  DISPLAYCONTROL = $08;
  DISPLAYON = $04;
  CLEARDISPLAY = $01;
  DLLProjectName = 'HD44780 FTDI BitBang Display DLL';
  Version = 'v1.1';
var
  hFtdi: handle;
  Buffer: Array[0..4] of Byte;
  Write_Result: Integer;
  ftdiInfo: TFTDIInfo;
  width: byte;

function write(val: byte; cmd: boolean):boolean;
var
  b: byte;
begin
  if cmd then
    b := 0
  else
    b := (1 shl ftdiInfo[7]); // RS

  Buffer[0] := ((val shr 4) and $0F) or (1 shl ftdiInfo[5]) or b;
  FT_Write(hFtdi, @Buffer, 1, @Write_Result);
  Buffer[0] := ((val shr 4) and $0F) or b and not (1 shl ftdiInfo[5]);
  FT_Write(hFtdi, @Buffer, 1, @Write_Result);
  Buffer[0] := (val and $0F) or (1 shl ftdiInfo[5]) or b;
  FT_Write(hFtdi, @Buffer, 1, @Write_Result);
  Buffer[0] := (val and $0F) or b and not (1 shl ftdiInfo[5]);
  FT_Write(hFtdi, @Buffer, 1, @Write_Result);
end;

function DISPLAYDLL_Init(SizeX,SizeY : byte; StartupParameters : pchar; OK : pboolean) : pchar; stdcall;
var
  DevIndex : DWord;
  res: integer;
  i, b, c: cardinal;
  ftInitval: byte;
begin
  OK^ := true;
  width := SizeX;
  Result := PChar(DLLProjectName + ' ' + Version + #0);
  hFtdi := LoadLibrary('FTD2XX.DLL');
  if (hFtdi <> 0) then
  begin
    // parse arguments
    i := 0;
    b := 1;
    c := 0;
    try
      while true do
      begin
       i := pos(',', StartupParameters, i+1);
       if i = 0 then break;
       ftdiInfo[c] := strtoint(copy(StartupParameters, b, i - b));
       b := i + 1;
       inc(c);
      end;
      ftdiInfo[c] := strtoint(copy(StartupParameters, b));

      ftInitval := 0;
      for i := 1 to 7 do
        ftInitval :=  ftInitval or (1 shl ftdiInfo[i]);

      DevIndex := ftdiInfo[0];
    except
      DevIndex := 0;
      for i := 0 to 6 do
        ftdiInfo[i+1] := i;
    end;

    If (FT_Open(DevIndex,@hFtdi) <> 0) then
      OK^ := false
    else
    If (FT_SetBitMode(hFtdi, ftInitval, 1) <> 0) then
      OK^ := false
    else
    // apparently higher baudrate = less garbage
    If (FT_SetBaudRate(hFtdi, 921600) <> 0) then
      OK^ := false;

    if OK^ = false then
      Exit;

    Buffer[0] := 0;
    FT_Write(hFtdi, @Buffer, 1, @Write_Result); // make all pins low

    write($33, true);
    sleep(5);

    write($32, true);
    sleep(5);

    write(FUNCTIONSET or TWOLINE, true);
    write(DISPLAYCONTROL or DISPLAYON, true);

    write(CLEARDISPLAY, true);
    sleep(2);

    //write(ord('T'), false);
    //write(ord('E'), false);
    //write(ord('S'), false);
    //write(ord('T'), false);
  end
  else
    OK^ := false
end;

procedure DISPLAYDLL_Write(Str : pchar); stdcall;
// write string
var
  S : string;
  i: integer;
begin
  try
    S := string(Str);
    for i := 1 to length(S) do
     begin
      case byte(S[i]) of
        176: S[i]:=Chr(0);
        158: S[i]:=Chr(1);
        131: S[i]:=Chr(2);
        132: S[i]:=Chr(3);
        133: S[i]:=Chr(4);
        134: S[i]:=Chr(5);
        135: S[i]:=Chr(6);
        136: S[i]:=Chr(7);
      end;
      write(ord(S[i]), false);
    end;
  except
  end
end;

procedure DISPLAYDLL_SetPosition(X, Y: byte); stdcall;
// set cursor position
var
tempX, tempY, DDaddr: Byte;
begin
  try
    tempX := x - 1;
    tempY := y - 1;
    DDaddr := tempX + (tempY mod 2) * 64;

    // line 3 logically follows line 1, (same for 4 and 2)
    if ((tempY mod 4) >= 2) then
      DDaddr := DDaddr + width;

    write($80 or DDaddr , true);
  except
  end;
end;

procedure DISPLAYDLL_CustomChar(Chr : byte; Data : TCustomArray); stdcall;
// define custom character
var
  i: integer;
begin
  try
    write(64 + ((Chr -1)*8), true) ;
    for i := 0 to 7 do
      write(data[i], false);
  except
  end;
end;

function DISPLAYDLL_DefaultParameters : pchar; stdcall;
begin
  DISPLAYDLL_DefaultParameters := pchar('0,0,1,2,3,4,5,6' + #0);
end;

function DISPLAYDLL_Usage : pchar; stdcall;
begin
  Result := pchar('Dev index, D4, D5, D6, D7, E, RW, RS' + #0);
end;

function DISPLAYDLL_DriverName : pchar; stdcall;
begin
  Result := PChar(DLLProjectName + ' ' + Version + #0);
end;

procedure DISPLAYDLL_Done(); stdcall;
// close port
begin
  try
    if hFtdi > 0 then
      FT_Close(hFtdi);
      //unloadLibrary(hFtdi);
  except
  end;
end;

exports
  DISPLAYDLL_Init,
  DISPLAYDLL_Write,
  DISPLAYDLL_SetPosition,
  DISPLAYDLL_CustomChar,
  DISPLAYDLL_DefaultParameters,
  DISPLAYDLL_Usage,
  DISPLAYDLL_DriverName,
  DISPLAYDLL_Done;

//{$R *.res}

{$R *.res}

begin
end.
