unit UDataMemory;

{$MODE Delphi}

interface

uses
  System2,DataThread;

const
  // lets put a prefix key to make it clear what they are
  SysKey = '$Sys';
  UserNameKey = SysKey + 'Username';
  ComputerNameKey = SysKey + 'Computername';
  ScreensaverActiveKey = SysKey + 'SSActive';
  FullScreenGameActive = SysKey + 'FSGameActive';
  FullScreenAppActive = SysKey + 'FSAppActive';
  ApplicationActive = SysKey + 'AppActive';


  MemKey = '$Mem';
  MemFreeKey = MemKey + 'Free';
  MemUsedKey = MemKey + 'Used';
  MemTotalKey = MemKey + 'Total';
  MemFreePercentKey = MemKey + 'F%';
  MemUsedPercentKey = MemKey + 'U%';
  PageKey = '$Page';
  PageFreeKey = PageKey + 'Free';
  PageUsedKey = PageKey + 'Used';
  PageTotalKey = PageKey + 'Total';
  PageFreePercentKey = PageKey + 'F%';
  PageUsedPercentKey = PageKey + 'U%';
  
type
  TMemoryDataThread = class(TDataThread)
  private
    System1: Tsystem;
    STUsername, STComputername : String;
    STPageFree, STPageTotal: Int64;
    STMemFree, STMemTotal: Int64;
	
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
  SysUtils, UUtils;

constructor TMemoryDataThread.Create;
begin
  System1 := TSystem.Create(nil);
  STComputername := System1.Computername;
  STUsername := System1.Username;
  
  inherited Create(1000);
end;

destructor TMemoryDataThread.Destroy;
begin
  inherited;
  System1.Free;
end;

function TMemoryDataThread.AllowRefresh : boolean;
begin
  Result := true;
end;

procedure TMemoryDataThread.DoUpdate;
begin
  if (not Terminated) then begin
    fDataLock.Enter;
    try
      STMemfree := system1.availPhysmemory div (1024 * 1024);
      STMemTotal := system1.totalPhysmemory div (1024 * 1024);
    finally
      fDataLock.Leave;
    end;
  end;
  
  if (not Terminated) then begin
    fDataLock.Enter;
    try
      STPageTotal := system1.totalPageFile div (1024 * 1024);
      STPageFree := system1.AvailPageFile div (1024 * 1024);
    finally
      fDataLock.Leave;
    end;
  end;
end;

procedure TMemoryDataThread.ResolveVariables(var Line : string);
var
  mem: Int64;
  args: Array [1..maxArgs] of String;
  prefix, postfix: String;
  numArgs: Cardinal;
begin

  if (pos(ApplicationActive,Line) > 0) then begin
    while decodeArgs(line, ApplicationActive, maxArgs, args, prefix, postfix, numargs) do
    begin
      Line := prefix;
      Line := Line + inttostr(system1.isapplicationactive(args[1])) + postfix;
    end;
  end;

  if (pos(UserNameKey,Line) > 0) then
  Line := StringReplace(line, UserNameKey, STUsername, [rfReplaceAll]);

  if (pos(ComputerNameKey,Line) > 0) then
  Line := StringReplace(line, ComputerNameKey, STcomputername, [rfReplaceAll]);

  if (pos(ScreensaverActiveKey,Line) > 0) then
  Line := StringReplace(line, ScreensaverActiveKey, inttostr(system1.isscreensaveractive), [rfReplaceAll]);

  if (pos(FullScreenGameActive,Line) > 0) then
  Line := StringReplace(line, FullScreenGameActive, inttostr(system1.isfullscreengameactive), [rfReplaceAll]);

  if (pos(FullScreenAppActive,Line) > 0) then
  Line := StringReplace(line, FullScreenAppActive, inttostr(system1.isfullscreenappactive), [rfReplaceAll]);

  if (pos(MemKey,Line) > 0) then begin
    fDataLock.Enter;
    try
      line := StringReplace(line, MemFreeKey, IntToStr(STMemFree), [rfReplaceAll]);
      line := StringReplace(line, MemUsedKey, IntToStr(STMemTotal-STMemFree), [rfReplaceAll]);
      line := StringReplace(line, MemTotalKey, IntToStr(STMemTotal), [rfReplaceAll]);
      if pos(MemFreePercentKey, line) <> 0 then
      begin
        if (STMemTotal > 0) then mem := round(100/STMemTotal*STMemfree)
        else mem := 0;
        line := StringReplace(line, MemFreePercentKey, IntToStr(mem), [rfReplaceAll]);
      end;
      if pos(MemUsedPercentKey, line) <> 0 then
      begin
        if (STMemTotal > 0) then mem := round(100/STMemTotal*(STMemTotal-STMemfree))
        else mem := 0;
        line := StringReplace(line, MemUsedPercentKey, IntToStr(mem), [rfReplaceAll]);
      end;
    finally
      fDataLock.Leave;
    end;
  end;

  if (pos(PageKey,Line) > 0) then begin
    fDataLock.Enter;
    try
      line := StringReplace(line, PageFreeKey, IntToStr(STPageFree), [rfReplaceAll]);
      line := StringReplace(line, PageUsedKey, IntToStr(STPageTotal-STPageFree), [rfReplaceAll]);
      line := StringReplace(line, PageTotalKey, IntToStr(STPageTotal), [rfReplaceAll]);
      if pos(PageFreePercentKey, line) <> 0 then
      begin
        if (STPageTotal > 0) then mem := round(100/STPageTotal*STPagefree)
        else mem := 0;
        line := StringReplace(line, PageFreePercentKey, IntToStr(mem), [rfReplaceAll]);
      end;

      if pos(PageUsedPercentKey, line) <> 0 then
      begin
        if (STPageTotal > 0) then mem := round(100/STPageTotal*(STPageTotal-STPagefree))
        else mem := 0;
        line := StringReplace(line, PageUsedPercentKey, IntToStr(mem), [rfReplaceAll]);
      end;
    finally
      fDataLock.Leave;
    end;
  end;
end;

end.
