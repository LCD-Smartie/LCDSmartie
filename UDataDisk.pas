unit UDataDisk;

{$MODE Delphi}

interface

uses
  DataThread,System2;

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
    System1: Tsystem;
    procedure ResolveDiskSpaceVariable(HDStat : THardDriveStats; var Line : string);
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
  SysUtils,UUtils;

constructor TDiskDataThread.Create;
begin
  System1 := TSystem.Create(nil);
  inherited Create(1000);
end;

destructor TDiskDataThread.Destroy;
begin
  inherited;
  System1.Free;
end;

function TDiskDataThread.AllowRefresh : boolean;
begin
  Result := true;
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
      letter := ord(upcase(args[1][1]));
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
          STHDFree[letter] := system1.diskfreespace(chr(letter)) div (1024*1024);
          STHDTotal[letter] := system1.disktotalspace(chr(letter)) div (1024*1024);
        end;
      except
      end;
    end;
  end;
end;

end.
