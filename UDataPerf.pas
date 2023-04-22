unit UDataPerf;

{$MODE Delphi}

interface

uses
  DataThread, UConfig, SysUtils;

const
  PDH_FMT_LONG   = $100;
  PDH_FMT_DOUBLE = $200;
  PDH_FMT_LARGE  =  $400;

  PDH_FMT_NOSCALE  = $1000;
  PDH_FMT_1000     = $2000;
  PDH_FMT_NODATA   = $4000;
  PDH_FMT_NOCAP100 = $8000;

type
  // Should be variant but the case command used can only handle up to a 32 bit space in 32bit code
  // thats fine we can typecast to long or double
  PTPDH_FMT_COUNTERVALUE = ^TPDH_FMT_COUNTERVALUE;
  TPDH_FMT_COUNTERVALUE = record
    CStatus: DWORD ;
    largeValue: Int64;
    //case integer of
    //  1: ( longValue: Int32; );
    //  2: ( doubleValue: double; );
    //  3: ( largeValue: Int64; );
   //   4: ( AnsiStringValue: PAnsiChar; );
    //  5: ( WideStringValue: PWideChar; );
  end;

type
  TCounterData = record
    Handle: THANDLE;
    format: integer;
    Scale: DWORD;
    Data: int64;
  end;

type
  TPerfDataThread = class(TDataThread)
  private
    pdhQueryHandle: THANDLE;
    pdhDataArray: Array [1..length(config.PerfSettings)] of TCounterData;
    pdhLastSettingsArray: Array [1..length(config.PerfSettings)] of TPerfSettings;
  protected
    function AllowRefresh : boolean; override;
    procedure DoUpdate; override;
    procedure SetupQuery;
    procedure CloseQuery;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ResolveVariables(var Line : string); override;

  end;

implementation

uses
  UUtils, StrUtils, windows;

function PdhOpenQueryW( szDataSource : PAnsiChar; dwUserData : PDWORD; phQuery: pointer ) : HRESULT; stdcall; external 'pdh' name 'PdhOpenQueryW';
function PdhCloseQuery( hQuery: THANDLE ) : HRESULT; stdcall; external 'pdh' name 'PdhCloseQuery';
function PdhAddCounterW( hQuery : THANDLE; szFullCounterPath : PWideChar; dwUserData: PDWORD; phCounter : pointer ) : HRESULT; stdcall; external 'pdh' name 'PdhAddCounterW';
function PdhCollectQueryData( hQuery : THANDLE ) : HRESULT; stdcall; external 'pdh' name 'PdhCollectQueryData';
function PdhGetFormattedCounterValue( hCounter : THANDLE; dwFormat : DWORD; lpdwType: pointer; pValue: pointer ) : HRESULT; stdcall; external 'pdh' name 'PdhGetFormattedCounterValue';

constructor TPerfDataThread.Create;
begin
  SetupQuery;
  inherited Create(500);
end;

destructor TPerfDataThread.Destroy;
begin
  CloseQuery;
  inherited;
end;

function TPerfDataThread.AllowRefresh : boolean;
begin
  Result := true;
end;

procedure TPerfDataThread.SetupQuery;
var
  pdhRet: HRESULT;
  pdhCounterPath: string;
  i: integer;
  wideChars   : array[0..255] of WideChar;
begin
  if pdhQueryHandle > 0 then CloseQuery;

  pdhRet := PdhOpenQueryW(nil, nil, @pdhQueryHandle);
  if pdhRet <> 0 then
  begin
    for i := 1 to length(config.PerfSettings) do
      pdhDataArray[i].Data := 0;
    exit;
  end;

  for i := 1 to length(config.PerfSettings) do
  begin
    pdhLastSettingsArray[i] := config.PerfSettings[i];
    if (config.PerfSettings[i].PerfObject = '') or
       (config.PerfSettings[i].Counter = '') then
         continue;
    pdhDataArray[i].Data := 0;
    pdhCounterPath := '\' + config.PerfSettings[i].PerfObject;
    if (config.PerfSettings[i].Instance <> '') then
      pdhCounterPath := pdhCounterPath + '(' + config.PerfSettings[i].Instance + ')';

    pdhCounterPath := pdhCounterPath + '\' + config.PerfSettings[i].Counter;
    StringToWideChar(rawbytestring(pdhCounterPath), wideChars, 255);
    pdhRet := PdhAddCounterW(pdhQueryHandle, pwidechar(wideChars), nil, @pdhDataArray[i].Handle);

    case config.PerfSettings[i].Format of
      0: pdhDataArray[i].format := PDH_FMT_LONG;
      1: pdhDataArray[i].format := PDH_FMT_DOUBLE;
      2: pdhDataArray[i].format := PDH_FMT_LARGE;
    end;

    case config.PerfSettings[i].Scaling of
      0: pdhDataArray[i].Scale := 0;
      1: pdhDataArray[i].Scale := PDH_FMT_NOSCALE;
      2: pdhDataArray[i].Scale := PDH_FMT_1000;
      3: pdhDataArray[i].Scale := PDH_FMT_NODATA;
      4: pdhDataArray[i].Scale := PDH_FMT_NOCAP100;
    end;
  end;
end;

procedure TPerfDataThread.CloseQuery;
begin
   PdhCloseQuery(pdhQueryHandle);
end;

procedure TPerfDataThread.DoUpdate;
var
  pdhRet: HRESULT;
  pdhCounterType: DWORD;
  pdhFmtCounterValue: TPDH_FMT_COUNTERVALUE;
  i: integer;
begin
  if pdhQueryHandle > 0 then
    PdhCollectQueryData(pdhQueryHandle);

  for i := 1 to length(config.PerfSettings) do
  begin
    if (pdhLastSettingsArray[i].PerfObject <> config.PerfSettings[i].PerfObject) or
      (pdhLastSettingsArray[i].Counter <> config.PerfSettings[i].Counter) or
      (pdhLastSettingsArray[i].Instance <> config.PerfSettings[i].Instance) or
      (pdhLastSettingsArray[i].Format <> config.PerfSettings[i].Format) or
      (pdhLastSettingsArray[i].Scaling <> config.PerfSettings[i].Scaling) then
    begin
      SetupQuery; // tear it all down and start again
      exit;
    end;

    pdhRet := PdhGetFormattedCounterValue(pdhDataArray[i].Handle,  pdhDataArray[i].format or pdhDataArray[i].Scale , @pdhCounterType, @pdhFmtCounterValue);
    if pdhRet = 0 then
    begin
      case config.PerfSettings[i].Format of
        0: pdhDataArray[i].Data := round((int32(pdhFmtCounterValue.largeValue) + pdhDataArray[i].Data) / 2);
        1: pdhDataArray[i].Data := int64((double(pdhFmtCounterValue.largeValue) + double(pdhDataArray[i].Data)) / 2);
        2: pdhDataArray[i].Data := round((pdhFmtCounterValue.largeValue + pdhDataArray[i].Data) / 2);
      end;
    end
  end;
end;

procedure TPerfDataThread.ResolveVariables(var Line : string);
var
  i: integer;
  fmt: TFormatSettings;
begin
  fmt := DefaultFormatSettings;
  fmt.DecimalSeparator := '.';

  if (pos('$Perf',Line) > 0) then
    for i := 1 to length(config.PerfSettings) do
    begin
      case config.PerfSettings[i].Format of
        0: line := StringReplace(line, '$Perf(' + IntToStr(i) + ')', inttostr(pdhDataArray[i].Data), [rfReplaceAll]);
        1: line := StringReplace(line, '$Perf(' + IntToStr(i) + ')', floattostr(double(pdhDataArray[i].Data), fmt), [rfReplaceAll]);
        2: line := StringReplace(line, '$Perf(' + IntToStr(i) + ')', inttostr(pdhDataArray[i].Data), [rfReplaceAll]);
      end;
    end;
end;

end.

