library ezio;

{$MODE Delphi}

uses
  Windows,SysUtils,Classes,{USerial,}SERPORT;


{.$R *.res}

(*

 revhist

1.0 initial EZIO driver

*)

const
  DLLProjectName = 'ezio Display DLL';
  Version = 'v1.0';

type
  pboolean = ^boolean;
  TCustomArray = array[0..7] of byte;

type
  TLCD_EZIO = class
  public
    procedure customChar(character: Integer; data: Array of Byte);
    procedure setPosition(x, y: Integer);
    procedure write(str: String);
    function readKey(var key: Char): Boolean;
    constructor CreateSerial(StartupParameters : string);
    constructor Create;
    destructor Destroy; override;
  private
    COMPort : TSerialPort;
    bConnected: Boolean;
    localeFormat: TFormatSettings;
    procedure doReadThread;               // for Usb
    procedure writeDevice(buffer: string); overload;
    procedure writeDevice(byte: Byte); overload;
    function readDevice(var chr: Char): Boolean;
    procedure initLCD;
  end;

var
  LCD_EZIO : TLCD_EZIO = nil;

constructor TLCD_EZIO.CreateSerial(StartupParameters : string);
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, localeFormat);
  COMPort := TSerialPort.Create;
//  COMPort := TSerial.AlternateCreate;
  StartupParameters := StartupParameters + ',8,N,1,$';
  StartupParameters := StartupParameters + IntToHex(SetRTSFlag+SetDTRFlag,8);
  COMPort.OpenSerialPort(StartupParameters);
  Create();
end;

procedure TLCD_EZIO.initLCD;
begin
  if (not bConnected) then Exit;
  writeDevice($0FE); // command
  writeDevice($028); // init
  writeDevice($0FE); // command
  writeDevice($028); // init
  writeDevice($0FE); // command
  writeDevice($001); // home
end;

constructor TLCD_EZIO.Create;
begin
  bConnected := True;

  try
    initLcd;
  except
    bConnected := False;
    raise;
  end;

  inherited Create;
end;

destructor TLCD_EZIO.Destroy;
begin
    bConnected := false;
    if (Assigned(COMPort)) then
      COMPort.Free();
  inherited;
end;

function TLCD_EZIO.readKey(var key: Char): Boolean;
begin
  // test device has no keys and so this section is unwritten
  Result := false;
end;

procedure TLCD_EZIO.setPosition(x, y: Integer);
var pos: integer;
begin
	case y of
		1: pos := x  - 1;
		2: pos := $040 + x - 1; // do these displays come with more than 2 lines?
	end;
        writeDevice(Chr($0FE)+Chr($080+pos));
  inherited;
end;


procedure TLCD_EZIO.write(str: String);
var
  i: Cardinal;
begin
  for i:= 1 to Length(str) do
  begin
    case Ord(str[i]) of
       176: str[i]:=Chr(0);
       158: str[i]:=Chr(1);
       131: str[i]:=Chr(2);
       132: str[i]:=Chr(3);
       133: str[i]:=Chr(4);
       134: str[i]:=Chr(5);
       135: str[i]:=Chr(6);
       136: str[i]:=Chr(7);
     end;
  end;

  writeDevice(str);
end;

procedure TLCD_EZIO.customChar(character: Integer; data: Array of Byte);
begin
  writeDevice($0FE);
  writeDevice($064+character-1);
  writeDevice(data[0]);
  writeDevice(data[1]);
  writeDevice(data[2]);
  writeDevice(data[3]);
  writeDevice(data[4]);
  writeDevice(data[5]);
  writeDevice(data[6]);
  writeDevice(data[7]);
end;

procedure TLCD_EZIO.writeDevice(buffer: string);
var
  len: Cardinal;
begin
  len := length(buffer);
  if (len = 0) then Exit;
  COMPort.Write(@buffer[1], len);
end;

procedure TLCD_EZIO.writeDevice(byte: Byte);
begin
  writeDevice(Char(byte));
end;


function TLCD_EZIO.readDevice(var chr: Char): Boolean;
begin
  // unwritten as no device with keys to test
  Result := False;
end;

procedure TLCD_EZIO.doReadThread;
begin
  // unwritten as no device with keys to test
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
    LCD_EZIO := TLCD_EZIO.CreateSerial(S);
  except
    on E: Exception do begin
      result := PChar('EZIO.DLL Exception: ' + E.Message + #0);
      OK^ := false;
    end;
  end;
end;

procedure DISPLAYDLL_Done(); stdcall;
// close port
begin
  try
    if assigned(LCD_EZIO) then begin
      LCD_EZIO.Free;
      LCD_EZIO := nil;
    end;
  except
  end;
end;


function DISPLAYDLL_ReadKey : word; stdcall;
// return 00xx upon success, FF00 on fail
begin
  // unwritten as no device with keys to test
  Result := $FF00;
end;

procedure DISPLAYDLL_Write(Str : pchar); stdcall;
// write string
var
  S : string;
begin
  try
    if assigned(LCD_EZIO) then begin
      S := string(Str);
      LCD_EZIO.Write(S);
    end;
  except
  end;
end;

procedure DISPLAYDLL_CustomChar(Chr : byte; Data : TCustomArray); stdcall;
// define custom character
begin
  try
    if assigned(LCD_EZIO) then
       LCD_EZIO.CustomChar(Chr,Data);
  except
  end;
end;

procedure DISPLAYDLL_SetPosition(X, Y: byte); stdcall;
// set cursor position
begin
  try
    if assigned(LCD_EZIO) then
       LCD_EZIO.SetPosition(X,Y);
  except
  end;
end;

function DISPLAYDLL_DefaultParameters : pchar; stdcall;
begin
  DISPLAYDLL_DefaultParameters := pchar('COM1,2400' + #0);
end;

function DISPLAYDLL_Usage : pchar; stdcall;
begin
  Result := pchar('Usage: COM1,2400' + #0);
end;

function DISPLAYDLL_DriverName : pchar; stdcall;
begin
  Result := PChar(DLLProjectName + ' ' + Version + #0);
end;

// don't forget to export the funtions, else nothing works :)
exports
  //DISPLAYDLL_ReadKey,
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

