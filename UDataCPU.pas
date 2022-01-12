unit UDataCPU;

{$MODE Delphi}

interface

uses
  SysUtils, DataThread;

type
  TCPUDataThread = class(TDataThread)
  private
    //cpu usage
    STCPUType : string;
    bDoCpuSpeed: Boolean; // cpu + main threads.
    STCPUSpeed: String;                   // Guarded by dataCs, cpu + main thread
    bDoCpuUsage: Boolean; // cpu + main threads.
    STCPUUsage: Cardinal;                 //cpu + main thread
    CPUUsage: Array [1..5] of Cardinal;   //cpu thread only
    CPUUsageCount: Cardinal;              //cpu thread only
    CPUUsagePos: Cardinal;                //cpu thread only
    lastSpdUpdate: LongWord;              //cpu thread only
    iUptime: Int64;                       //cpu thread only
    iLastUptime: Cardinal;                //cpu thread only
    uptimereg, uptimeregs: String;        // Guarded by dataCs, cpu + main thread
  protected
    function AllowRefresh : boolean; override;
    procedure  DoUpdate; override;
    procedure SetActive(Value : boolean); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  ResolveVariables(var Line : string); override;
  end;

implementation

uses
  UUtils, StrUtils, Windows, cxCpu40, adCpuUsage;

constructor TCPUDataThread.Create;
begin
  // Get CPU speed first time:
  try
    STCPUSpeed := IntToStr(cxCpu[0].Speed.RawSpeed.AsNumber);
  except
    on e:exception do STCPUSpeed := '0';
    // BUGBUG: This has been reported as failing when with Range check error,
    // they reported that it only occured when they ran a slow 16 bit app
  end;

  Active := false;  // set the defaults
  inherited Create(250);
end;

destructor TCPUDataThread.Destroy;
begin
  inherited;
end;

function TCPUDataThread.AllowRefresh : boolean;
begin
  Result := true;
end;

procedure TCPUDataThread.SetActive(Value : boolean);
begin
  bDoCpuUsage := false;
  bDoCpuSpeed := false;
  CPUUsageCount := 0;
  CPUUsagePos := 1;
  inherited;
end;

procedure TCPUDataThread.ResolveVariables(var Line : string);
begin
  if (pos('$CPU', line) <> 0) then
  begin
    line := StringReplace(line, '$CPUType', STCPUType, [rfReplaceAll]);

    if (pos('$CPUSpeed', line) <> 0) then
    begin
      bDoCpuSpeed := true;
      fDataLock.Enter();
      try
        line := StringReplace(line, '$CPUSpeed', STCPUSpeed, [rfReplaceAll]);
      finally
        fDataLock.Leave();
      end;
    end;

    if (pos('$CPUUsage%', line) <> 0) then
    begin
      bDoCpuUsage := true;
      line := StringReplace(line, '$CPUUsage%', IntToStr(STCPUUsage),
        [rfReplaceAll]);
    end;
  end;

  if (pos('$UpTim', line) <> 0) then
  begin
    fDataLock.Enter();
    try
      line := StringReplace(line, '$UpTime', uptimereg, [rfReplaceAll]);
      line := StringReplace(line, '$UpTims', uptimeregs, [rfReplaceAll]);
    finally
      fDataLock.Leave();
    end;
  end;
end;

procedure TCPUDataThread.DoUpdate;
var
  t: longword;
  y, mo, d, h, m, s : Cardinal;
  total, x: Cardinal;
  rawcpu: Double;
  uiRemaining: Cardinal;
  sTempUptime: String;
begin
//cputype!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  try
    STCPUType := cxCpu[0].Name.AsString;
  except
    on E: Exception do STCPUType := '[CPUType: ' + E.Message + ']';
  end;

//try
  //cpuusage!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  //Application.ProcessMessages;

  if (bDoCpuUsage) then
  begin
    try
      CollectCPUData;
      rawcpu := adCpuUsage.GetCPUUsage(0);
      rawcpu := abs(rawcpu) * 100;
    except
       on e:exception do rawcpu := 0;
    end;

    if (rawcpu > 100) then rawcpu := 100;
    if (rawcpu < 0) then rawcpu := 0;

    CPUUsage[CPUUsagePos] := Trunc(rawcpu);
    Inc(CPUUsagePos);
    if (CPUUsagePos > 5) then CPUUsagePos := 1;
    if (CPUUsageCount < 5) then Inc(CPUUsageCount);

    total := 0;
    for x := 1 to CPUUsageCount do total := total + CPUUsage[x];
    if (CPUUsageCount > 0) then STCPUUsage := total div CPUUsageCount;
  end;

  t := GetTickCount;

  // The below code creates a very high priority thread which could cause
  // cpu spikes.  Don't do it unless the user is using $CPUSpeed.
  if (bDoCPUSpeed) then
  begin
    //Update CPU Speed (might change on clock-throttling systems
    if (t < lastSpdUpdate) or (t - lastSpdUpdate > (ticksperseconde * 2)) then
    begin                                                 // Update every 2 s
      lastSpdUpdate := t;

      try
        fDataLock.Enter();
        try
          STCPUSpeed := IntToStr(cxCpu[0].Speed.RawSpeed.AsNumber);
        finally
          fDataLock.Leave();
        end;
      except
          on e:exception do;
        // BUGBUG: This has been reported as failing when with Range check error,
        // they reported that it only occured when they ran a slow 16 bit app.
      end;
    end;
  end;


  //time/uptime!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if (t < iLastUptime) then iUptime := iUptime + t + (MAXDWORD-iLastUptime)
  else iUptime := iUptime + (t - iLastUptime);
  iLastUptime := t;

  y :=  iUptime div ticksperyear;
  mo := (iUptime div tickspermonth) mod 12;
  d := (iUptime div ticksperdag) mod 30;
  h := (iUptime div ticksperhour) mod 24;
  m := (iUptime div ticksperminute) mod 60;
  s := (iUptime div ticksperseconde) mod 60;

  sTempUptime := '';
  if (y > 0) or (sTempUptime<>'') then
    sTempUptime := sTempUptime + IntToStr(y) +  'yrs ';
  if (mo > 0) or (sTempUptime<>'') then
    sTempUptime := sTempUptime + IntToStr(mo) +  'mts ';
  if (d > 0) or (sTempUptime<>'') then
    sTempUptime := sTempUptime + IntToStr(d) +  'dys ';
  if (h > 0) or (sTempUptime<>'') then
    sTempUptime := sTempUptime + IntToStr(h) +  'hrs ';
  if (m > 0) or (sTempUptime<>'') then
    sTempUptime := sTempUptime + IntToStr(m) +  'min ';
  sTempUptime := sTempUptime + Format('%.2d',[s], localeFormat) + 'secs';
  fDataLock.Enter();
  uptimereg := sTempUptime;
  fDataLock.Leave();

  // Create the short uptime string
  // Display the three largest units, i.e. '15d 7h 12m' or '7h 12m 2s'
  sTempUptime := '';
  uiRemaining := 0;
  if (y>0) and (sTempUptime='') then uiRemaining := 3;
  if (uiRemaining > 0) then
  begin
    Dec(uiRemaining);
    sTempUptime := sTempUptime + IntToStr(y) +'y ';
  end;

  if (mo>0) and (sTempUptime='') then uiRemaining := 3;
  if (uiRemaining > 0) then
  begin
    Dec(uiRemaining);
    sTempUptime := sTempUptime + IntToStr(mo) +'m ';
  end;

  if (d>0) and (sTempUptime='') then uiRemaining := 3;
  if (uiRemaining > 0) then
  begin
    Dec(uiRemaining);
    sTempUptime := sTempUptime + IntToStr(d) +'d ';
  end;

  if (h>0) and (sTempUptime='') then uiRemaining := 3;
  if (uiRemaining > 0) then
  begin
    Dec(uiRemaining);
    sTempUptime := sTempUptime + IntToStr(h) +'h ';
  end;

  if (m>0) and (sTempUptime='') then uiRemaining := 3;
  if (uiRemaining > 0) then
  begin
    Dec(uiRemaining);
    sTempUptime := sTempUptime + IntToStr(m) +'m ';
  end;

  if (sTempUptime='') or (uiRemaining > 0) then
  begin
    sTempUptime := sTempUptime + Format('%.2d', [s], localeFormat) +'s ';
  end;

  // remove the trailing space and assign to class member
  fDataLock.Enter();
  uptimeregs := MidStr(sTempUptime, 1, Length(sTempUptime)-1);
  fDataLock.Leave();

end;

end.

