unit UDataDisk;

{$MODE Delphi}

interface

uses
  DataThread, windows, UMain;

const
  HDKey = '$HD';
  HDFreeKey = HDKey + 'Free';
  HDUsedKey = HDKey + 'Used';
  HDTotalKey = HDKey + 'Total';
  HDFreePercentKey = HDKey + 'F%';
  HDUsedPercentKey = HDKey + 'U%';
  HDFreeGigaBytesKey = HDKey + 'Freg';
  HDUsedGigaBytesKey = HDKey + 'Useg';
  HDTotalGigaBytesKey = HDKey + 'Totag';

  FirstDrive = ord('A');
  LastDrive = ord('Z');

type
  THardDriveStats = (dsHDFree,dsHDUsed,dsHDTotal,dsHDFreePercent,dsHDUsedPercent,
                     dsHDFreeGigaBytes,dsHDUsedGigaBytes,dsHDTotalGigaBytes);

const
  HDKeys : array[THardDriveStats] of string = (
    HDFreeKey,HDUsedKey,HDTotalKey,HDFreePercentKey,HDUsedPercentKey,
    HDFreeGigaBytesKey,HDUsedGigaBytesKey,HDTotalGigaBytesKey);

  FirstHDStat = dsHDFree;
  LastHDStat = dsHDTotalGigaBytes;

type
  TDiskDataThread = class(TDataThread)
  private
    bDoDisk: Array[FirstDrive..LastDrive] of Boolean; // cpu + main threads
    STHDFree, STHDTotal: Array[FirstDrive..LastDrive] of Int64; // cpu + main thread.
    //System1: Tsystem;
    procedure ResolveDiskSpaceVariable(HDStat : THardDriveStats; var Line : string);
  protected
    function AllowRefresh : boolean; override;
    procedure  DoUpdate; override;
  public
    function diskindrive(lw: char;statusanzeige: Boolean): Boolean;
    function disktyp(lw: char): String;
    function diskserialnumber(lw: char): Integer;
    function diskfilesystem(lw: char): String;
    function disknamelength(lw: char): Integer;
    function diskfreespace(lw: char): int64;
    function disktotalspace(lw: char): int64;
    constructor Create;
    destructor Destroy; override;
    procedure  ResolveVariables(var Line : string); override;
  end;

implementation

uses
  SysUtils,UUtils;

constructor TDiskDataThread.Create;
begin
//  System1 := TSystem.Create(nil);
  inherited Create(1000);
end;

destructor TDiskDataThread.Destroy;
begin
  inherited;
  //System1.Free;
end;

function TDiskDataThread.AllowRefresh : boolean;
begin
  Result := true;
end;
function TDiskDataThread.diskindrive(lw: char; statusanzeige: Boolean): Boolean;
var
  sRec: TsearchRec;
  i: Integer;
begin
  result := false;
     {$I-}
  i := findfirst(lw + ':\*.*', faAnyfile, Srec);
  findclose(Srec);
     {$I+}
  case i of
    0: result := true;
    2, 18:
      begin
        if statusanzeige then
//               showmessage('Diskette im Laufwerk ' + lw + ' ist leer !');
          result := true;
      end;
    21, 3:
      if statusanzeige then
      begin
      end // showmessage('Keine Diskette im Laufwerk ' + lw + ' !');
      else
        if statusanzeige then
        begin
        end// showmessage('Diskette nicht formatiert !' + inttostr(i));
  end;
end;

function TDiskDataThread.disktyp(lw: char): String;
var
  typ: Integer;
  s: String;
begin
  if diskindrive(lw, false) then
  begin
    s := lw + ':\';
    typ := getdrivetype(Pchar(s));
    if typ <> 0 then
      case typ of
        DRIVE_REMOVABLE: result := 'Diskette';
        DRIVE_FIXED: result := 'HardDisk';
        DRIVE_CDROM: result := 'CDROM';
        DRIVE_RAMDISK: result := 'RAMDisk';
        DRIVE_REMOTE: result := 'Network';
        else result := 'Unknown';
      end;
  end;
end;

function TDiskDataThread.diskserialnumber(lw: char): Integer;
var
  root: String;
  volumenamebuffer, filesystemnamebuffer: pchar;
  filesystemflags, maximumcomponentlength: Dword;
  volumeserialnumber: dword;
begin
  volumeserialnumber := 0;
  if diskindrive(lw, false)then
  begin
    root := lw + ':\';
    volumenamebuffer := stralloc(256);
    filesystemnamebuffer := stralloc(256);
    Windows.GetVolumeInformation(pchar(root),volumenamebuffer, 255,
      @volumeserialnumber, maximumcomponentlength, filesystemflags,
      filesystemnamebuffer, 255);
    strdispose(volumenamebuffer);
    strdispose(filesystemnamebuffer);
  end;
  result := volumeserialnumber;
end;

function TDiskDataThread.diskfilesystem(lw: char): String;
var
  root: String;
  volumenamebuffer, filesystemnamebuffer: pchar;
  filesystemflags, maximumcomponentlength: Dword;
  volumeserialnumber: dword;
begin
  if diskindrive(lw, false)then
  begin
    root := lw + ':\';
    volumenamebuffer := stralloc(256);
    filesystemnamebuffer := stralloc(256);
    GetVolumeInformation(pchar(root),volumenamebuffer, 255,
      @volumeserialnumber, maximumcomponentlength, filesystemflags,
      filesystemnamebuffer, 255);
    result := filesystemnamebuffer;
    strdispose(volumenamebuffer);
    strdispose(filesystemnamebuffer);
  end;
end;

function TDiskDataThread.disknamelength(lw: char): Integer;
var
  root: String;
  volumenamebuffer, filesystemnamebuffer: pchar;
  filesystemflags, maximumcomponentlength: Dword;
  volumeserialnumber: dword;
begin
  if diskindrive(lw, false)then
  begin
    root := lw + ':\';
    volumenamebuffer := stralloc(256);
    filesystemnamebuffer := stralloc(256);
    GetVolumeInformation(pchar(root),volumenamebuffer, 255,
      @volumeserialnumber, maximumcomponentlength, filesystemflags,
      filesystemnamebuffer, 255);
    result := maximumcomponentlength;
    strdispose(volumenamebuffer);
    strdispose(filesystemnamebuffer);
  end
  else
  begin
    result := 0;
  end;
end;

function TDiskDataThread.diskfreespace(lw: char): int64;
var
  la: byte;
  lw2: char;
  uiOldMode: Cardinal;
begin
  // We don't want any system dislogs poping up if a file or drive isn't found.
  uiOldMode := SetErrorMode(SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS);
  lw2 := upcase(lw);
  la := ord(lw2)-64;
  result := diskfree(la);
  if (result = -1) then result := 0;
  SetErrorMode(uiOldMode);
end;

function TDiskDataThread.disktotalspace(lw: char): int64;
var
  la: byte;
  lw2: char;
  uiOldMode: Cardinal;
begin
  // We don't want any system dislogs poping up if a file or drive isn't found.
  uiOldMode := SetErrorMode(SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS);
  lw2 := upcase(lw);
  la := ord(lw2)-64;
  result := disksize(la);
  if (result = -1) then result := 0;
  SetErrorMode(uiOldMode);
end;

procedure TDiskDataThread.ResolveDiskSpaceVariable(HDStat : THardDriveStats; var Line : string);
var
  prefix, postfix: String;
  args: Array [1..maxArgs] of String;
  numArgs: Cardinal;
  letter : Cardinal;
  MyKey : string;
begin
  MyKey := HDKeys[HDStat];
  while decodeArgs(Line, MyKey, maxArgs, args, prefix, postfix, numargs) do begin
    try
      RequiredParameters(numargs, 1, 1);
      letter := ord(upcase(LCDSmartieDisplayForm.Data.change(args[1])[1]));
      Line := prefix;
      fDataLock.Enter();
      bDoDisk[letter] := true;
      try
        case HDStat of
          dsHDFree : Line := Line + IntToStr(STHDFree[letter]);
          dsHDUsed : Line := Line + IntToStr(STHDTotal[letter]-STHDFree[letter]);
          dsHDTotal : Line := Line + IntToStr(STHDTotal[letter]);
          dsHDFreePercent : begin
            if (STHDTotal[letter]*STHDFree[letter] <> 0) then
              Line := Line + intToStr(round(100/STHDTotal[letter]*STHDFree[letter]))
            else
              Line := Line + '0';
          end;
          dsHDUsedPercent : begin
            if (STHDTotal[letter]*(STHDTotal[letter]-STHDFree[letter]) <> 0) then
              Line := Line + intToStr(round(100/STHDTotal[letter]*(STHDTotal[letter]-STHDFree[letter])))
            else
              Line := Line + '0';
          end;
          dsHDFreeGigaBytes : Line := Line + IntToStr(round((STHDFree[letter])/1024));
          dsHDUsedGigaBytes : Line := Line + IntToStr(round((STHDTotal[letter]-STHDFree[letter])/1024));
          dsHDTotalGigaBytes : Line := Line + IntToStr(round(STHDTotal[letter]/1024));
        end; // case
      finally
        fDataLock.Leave();
      end;
      Line := Line + postfix;
    except
      on E: Exception do Line := prefix + '['+ CleanString(MyKey + ': ' + E.Message) + ']' + postfix;
    end;
  end;
end;

procedure TDiskDataThread.ResolveVariables(var Line : string);
var
  HDStats : THardDriveStats;
begin
  if (pos(HDKey, Line) > 0) then begin
    Active := true;
    for HDStats := FirstHDStat to LastHDStat do begin
      ResolveDiskSpaceVariable(HDStats,Line);
    end;
  end;
end;

procedure TDiskDataThread.DoUpdate;
var
  letter: Cardinal;
begin
  if Active then
  begin
    for letter := FirstDrive to LastDrive do
    begin
      try
        if (bDoDisk[letter]) then
        begin
          bDoDisk[letter] := false;
          STHDFree[letter] := diskfreespace(chr(letter)) div (1024*1024);
          STHDTotal[letter] := disktotalspace(chr(letter)) div (1024*1024);
        end;
      except
      end;
    end;
  end;
end;

end.
