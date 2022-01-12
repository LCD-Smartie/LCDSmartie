unit UDataNetwork;

{$MODE Delphi}

interface

uses
  SysUtils, DataThread;

type
  TNetworkStatistics =
    (nsNetIPAddress, nsNetAdapter, nsNetDownK, nsNetUpK, nsNetDownM, nsNetUpM,
     nsNetDownG, nsNetUpG, nsNetErrDown, nsNetErrUp, nsNetErrTot, nsNetUniDown,
     nsNetUniUp, nsNetUniTot, nsNetNuniDown, nsNetNuniUp, nsNetNuniTot,
     nsNetPackTot, nsNetDiscDown, nsNetDiscUp, nsNetDiscTot, nsNetSpDownK,
     nsNetSpUpK, nsNetSpDownM, nsNetSpUpM);

const
  NetKeyPrefix = '$Net';
  NetIPAddressKey = NetKeyPrefix + 'IPaddress';
  NetAdapterKey = NetKeyPrefix + 'Adapter';
  NetDownKKey = NetKeyPrefix + 'DownK';
  NetUpKKey = NetKeyPrefix + 'UpK';
  NetDownMKey = NetKeyPrefix + 'DownM';
  NetUpMKey = NetKeyPrefix + 'UpM';
  NetDownGKey = NetKeyPrefix + 'DownG';
  NetUpGKey = NetKeyPrefix + 'UpG';
  NetErrDownKey = NetKeyPrefix + 'ErrDown';
  NetErrUpKey = NetKeyPrefix + 'ErrUp';
  NetErrTotKey = NetKeyPrefix + 'ErrTot';
  NetUniDownKey = NetKeyPrefix + 'UniDown';
  NetUniUpKey = NetKeyPrefix + 'UniUp';
  NetUniTotKey = NetKeyPrefix + 'UniTot';
  NetNuniDownKey = NetKeyPrefix + 'NuniDown';
  NetNuniUpKey = NetKeyPrefix + 'NuniUp';
  NetNuniTotKey = NetKeyPrefix + 'NuniTot';
  NetPackTotKey = NetKeyPrefix + 'PackTot';
  NetDiscDownKey = NetKeyPrefix + 'DiscDown';
  NetDiscUpKey = NetKeyPrefix + 'DiscUp';
  NetDiscTotKey = NetKeyPrefix + 'DiscTot';
  NetSpDownKKey = NetKeyPrefix + 'SpDownK';
  NetSpUpKKey = NetKeyPrefix + 'SpUpK';
  NetSpDownMKey = NetKeyPrefix + 'SpDownM';
  NetSpUpMKey = NetKeyPrefix + 'SpUpM';

  NetworkUserHints : array[TNetworkStatistics] of string = (
    'IP address',
    'Adapter name (adapterNr)',
    'Total Down (adapterNr)  (KB)',
    'Total up (adapterNr)  (KB)',
    'Total Down (adapterNr)  (MB)',
    'Total up (adapterNr)  (MB)',
    'Total Down (adapterNr)  (GB)',
    'Total up (adapterNr)  (GB)',
    'Errors down (adapterNr)',
    'Errors up (adapterNr)',
    'Total Errors (adapterNr)',
    'Unicast Packets down (adapterNr)',
    'Unicast Packets up (adapterNr)',
    'Total Uni. Packets (adapterNr)',
    'Non-Uni. Packets down (adapterNr)',
    'Non-Uni. Packets up (adapterNr)',
    'Total Non-Uni. Packets (adapterNr)',
    'Total nr of Packets (adapterNr)',
    'Discards down (adapterNr)',
    'Discards up (adapterNr)',
    'Total Discards (adapterNr)',
    'Speed Down (adapterNr)  (KB)',
    'Speed Up (adapterNr)  (KB)',
    'Speed Down (adapterNr)  (MB)',
    'Speed Up (adapterNr)  (MB)');

  NetworkStatisticsKeys : array[TNetworkStatistics] of string =
    (NetIPAddressKey, NetAdapterKey, NetDownKKey, NetUpKKey, NetDownMKey, NetUpMKey,
     NetDownGKey, NetUpGKey, NetErrDownKey, NetErrUpKey, NetErrTotKey, NetUniDownKey,
     NetUniUpKey, NetUniTotKey, NetNuniDownKey, NetNuniUpKey, NetNuniTotKey,
     NetPackTotKey, NetDiscDownKey, NetDiscUpKey, NetDiscTotKey, NetSpDownKKey,
     NetSpUpKKey, NetSpDownMKey, NetSpUpMKey);

  FirstNetworkStat = nsNetIPAddress;
  LastNetworkStat = nsNetSpUpM;

const
  MAXNETSTATS = 99;

type
  TNetworkAdapterStats = record
    netadapterip: String;
    netadaptername: String;
    iNetTotalDown, iNetTotalUp,
    iNetTotalDownOld, iPrevSysNetTotalDown,
    iNetTotalUpOld, iPrevSysNetTotalUp : Int64;
    uiNetUnicastDown, uiNetUnicastUp, uiNetNonUnicastDown,
    uiNetNonUnicastUp,  uiNetDiscardsDown, uiNetDiscardsUp,
    uiNetErrorsDown, uiNetErrorsUp: Cardinal;
    dNetSpeedDownK, dNetSpeedUpK,
    dNetSpeedDownM, dNetSpeedUpM: double;
  end;

  TNetworkDataThread = class(TDataThread)
  private
    // network stats
    NetworkAdapterStats : Array[0..MAXNETSTATS-1] of TNetworkAdapterStats;
    procedure ResolveNetVariable(Variable : TNetworkStatistics; var Line : string);
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
  Windows, IpRtrMib, IpTypes, IpHlpApi, UUtils;

constructor TNetworkDataThread.Create;
begin
  inherited Create(1000);
end;

destructor TNetworkDataThread.Destroy;
begin
  inherited;
end;

function TNetworkDataThread.AllowRefresh : boolean;
begin
  Result := true;
end;

procedure  TNetworkDataThread.DoUpdate;
var
  Size: ULONG;
  IntfTable: PMibIfTable;
  AdapterNumber: Integer;
  MibRow: TMibIfRow;
  maxEntries: Cardinal;
  Ret: DWORD;
  BufLen: ULONG;
  Adapter, Adapters: PIP_ADAPTER_INFO;
  IPAddr: PIP_ADDR_STRING;
begin

  if (Active) then
  begin
 
 //   GetHostName(Buffer, Sizeof(Buffer));
 //   phoste := GetHostByName(buffer);
 //   fDataLock.Enter();
 //   try
 //     if phoste = nil then ipaddress := '127.0.0.1'
 //     else ipaddress := StrPas(inet_ntoa(PInAddr(phoste^.h_addr_list^)^));
 //   finally
 //     fDataLock.Leave();
 //   end;

    Size := 0;
    if GetIfTable(nil, Size, True) <> ERROR_INSUFFICIENT_BUFFER then  Exit;
      //raise Exception.Create('getiftable failed with ' + errMsg(GetLastError));

    if (Size < sizeof( TMibIftable)) then Exit;
//          raise Exception.Create('size too small');
    IntfTable := AllocMem(Size);
 //   if (IntfTable = nil) then
   //     raise Exception.Create('no memory?');

    try
      if (IntfTable <> nil) and (GetIfTable(IntfTable, Size, True) = NO_ERROR) then
      begin
        maxEntries := min(IntfTable^.dwNumEntries,MAXNETSTATS);
        for AdapterNumber := 0 to maxEntries - 1 do with NetworkAdapterStats[AdapterNumber] do
        begin
        {$R-}MibRow := IntfTable.Table[AdapterNumber];{$R+}
        // Ignore everything except ethernet cards
        //if MibRow.dwType <> MIB_IF_TYPE_ETHERNET then Continue;

          fDataLock.Enter();
          try
            netadaptername := stripspaces(PChar(@MibRow.bDescr[0]));
          finally
            fDataLock.Leave();
          end;

          // System values have a limit of 4Gb, so keep our own values,
          // and track overflows.
          if (MibRow.dwInOctets < iPrevSysNetTotalDown) then
          begin
            // System values have wrapped (at 4Gb)
            iNetTotalDown := iNetTotalDown + MibRow.dwInOctets
              + (MAXDWORD - iPrevSysNetTotalDown)
          end
          else
          begin
            iNetTotalDown := iNetTotalDown + (MibRow.dwInOctets - iPrevSysNetTotalDown);
          end;
          iPrevSysNetTotalDown := MibRow.dwInOctets;

          // System values have a limit of 4Gb, so keep our own values,
          // and track overflows.
          if (MibRow.dwOutOctets < iPrevSysNetTotalUp) then
          begin
            // System values have wrapped (at 4Gb)
            iNetTotalUp := iNetTotalUp + MibRow.dwOutOctets + (MAXDWORD - iPrevSysNetTotalUp)
          end
          else
          begin
            iNetTotalUp := iNetTotalUp + (MibRow.dwOutOctets - iPrevSysNetTotalUp);
          end;
          iPrevSysNetTotalUp := MibRow.dwOutOctets;
          uiNetUnicastDown := MibRow.dwInUcastPkts;
          uiNetUnicastUp := MibRow.dwOutUcastPkts;
          uiNetNonUnicastDown := MibRow.dwInNUcastPkts;
          uiNetNonUnicastUp := MibRow.dwOutNUcastPkts;
          uiNetDiscardsDown := MibRow.dwInDiscards;
          uiNetDiscardsUp := MibRow.dwOutDiscards;
          uiNetErrorsDown := MibRow.dwInErrors;
          uiNetErrorsUp := MibRow.dwOutErrors;
          dNetSpeedDownK := round((iNetTotalDown-iNetTotalDownOld)/1024*10)/10;
          dNetSpeedUpK := round((iNetTotalUp-iNetTotalupOld)/1024*10)/10;
          dNetSpeedDownM := round(((iNetTotalDown-iNetTotalDownOld) div 1024)/1024*10)/10;
          dNetSpeedUpM := round(((iNetTotalUp-iNetTotalUpOld) div 1024)/1024*10)/10;
          iNetTotalDownOld := iNetTotalDown;
          iNetTotalUpOld := iNetTotalUp;
        end;
      end;
    finally
      if (IntfTable <> nil) then FreeMem(IntfTable);
    end;

    BufLen := 1024*15;
    GetMem(Adapters, BufLen);
	
    try
      repeat
        Ret := GetAdaptersInfo(Adapters, BufLen);
        case Ret of
          ERROR_SUCCESS:
          begin
            if BufLen = 0 then begin
              Exit;
            end;
            Break;
          end;
          ERROR_NOT_SUPPORTED,
          ERROR_NO_DATA:
            Exit;
          ERROR_BUFFER_OVERFLOW:
            ReallocMem(Adapters, BufLen);
        end;
      until False;

      if Ret = ERROR_SUCCESS then
      begin
        Adapter := Adapters;
        repeat
          IPAddr := @(Adapter^.IpAddressList);
		     	NetworkAdapterStats[Adapter^.index - 1].netadapterip := IPAddr.IpAddress.S;
          Adapter := Adapter^.Next;
        until Adapter = nil;
      end;
    finally
      FreeMem(Adapters);
    end;

  end;
end;

procedure TNetworkDataThread.ResolveNetVariable(Variable : TNetworkStatistics; var Line : string);
var
  VarKey : string;
  args: Array [1..maxArgs] of String;
  prefix, postfix: String;
  numArgs: Cardinal;
  adapterNum: Cardinal;
begin
  VarKey := NetworkStatisticsKeys[Variable];
  while decodeArgs(Line, VarKey, maxArgs, args, prefix, postfix, numargs) do begin
    try
      RequiredParameters(numargs, 1, 1);
      adapterNum := StrToInt(args[1]);
      Line := prefix;
      fDataLock.Enter();
      with NetworkAdapterStats[adapterNum] do begin
        try
          case Variable of
            nsNetIPAddress : Line := Line + netadapterip;
			nsNetAdapter : Line := Line + netadaptername;
            nsNetDownK : Line := Line + FloatToStrF(Round(iNetTotalDown/1024*10)/10,ffFixed, 18, 1, localeFormat);
            nsNetUpK : Line := Line + FloatToStrF(Round(iNetTotalUp/1024*10)/10,ffFixed, 18, 1, localeFormat);
            nsNetDownM : Line := Line + FloatToStrF(Round((iNetTotalDown div 1024)/1024*10)/10,ffFixed, 18, 1, localeFormat);
            nsNetUpM : Line := Line + FloatToStrF(Round((iNetTotalUp div 1024)/1024*10)/10,ffFixed, 18, 1, localeFormat);
            nsNetDownG : Line := Line + FloatToStrF(Round((iNetTotalDown div (1024*1024))/1024*10)/10,ffFixed, 18, 1, localeFormat);
            nsNetUpG : Line := Line + FloatToStrF(Round((iNetTotalUp div (1024*1024))/1024*10)/10,ffFixed, 18, 1, localeFormat);
            nsNetErrDown : Line := Line + IntToStr(uiNetErrorsDown);
            nsNetErrUp : Line := Line + IntToStr(uiNetErrorsUp);
            nsNetErrTot : Line := Line + IntToStr(uiNetErrorsDown + uiNetErrorsUp);
            nsNetUniDown : Line := Line + IntToStr(uiNetUnicastDown);
            nsNetUniUp : Line := Line + IntToStr(uiNetUnicastUp);
            nsNetUniTot : Line := Line + IntToStr(uiNetUnicastUp + uiNetUnicastDown);
            nsNetNuniDown : Line := Line + IntToStr(uiNetNonUnicastDown);
            nsNetNuniUp : Line := Line + IntToStr(uiNetNonUnicastUp);
            nsNetNuniTot : Line := Line + IntToStr(uiNetNonUnicastUp + uiNetNonUnicastDown);
            nsNetPackTot : Line := Line + IntToStr(Int64(uiNetNonUnicastUp) + uiNetNonUnicastDown + uiNetUnicastDown + uiNetUnicastUp);
            nsNetDiscDown : Line := Line + IntToStr(uiNetDiscardsDown);
            nsNetDiscUp : Line := Line + IntToStr(uiNetDiscardsUp);
            nsNetDiscTot : Line := Line + IntToStr(uiNetDiscardsUp + uiNetDiscardsDown);
            nsNetSpDownK : Line := Line + FloatToStrF(dNetSpeedDownK, ffFixed, 18, 1, localeFormat);
            nsNetSpUpK : Line := Line + FloatToStrF(dNetSpeedUpK, ffFixed, 18, 1, localeFormat);
            nsNetSpDownM : Line := Line + FloatToStrF(dNetSpeedDownM, ffFixed, 18, 1, localeFormat);
            nsNetSpUpM : Line := Line + FloatToStrF(dNetSpeedUpM, ffFixed, 18, 1, localeFormat);
          end;
        finally
          fDataLock.Leave();
        end;
      end;
      Line := Line + postfix;
    except
      on E: Exception do Line := prefix + '[' + CleanString(VarKey + ': ' + E.Message) + ']' + postfix;
    end;
  end;
end;


procedure  TNetworkDataThread.ResolveVariables(var Line : string);
var
  NetStatLoop : TNetworkStatistics;
begin
  if (pos(NetKeyPrefix, Line) = 0) then exit;
  Active := true;
  for NetStatLoop := FirstNetworkStat to LastNetworkStat do begin
    ResolveNetVariable(NetStatLoop,Line);
  end;
end;

end.
