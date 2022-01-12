unit WMPREG;

interface

uses
  SyncObjs;

var
  DataLock : TCriticalSection = nil;
  Author : string = '';
  Album : string = '';
  Duration : string = '';
  Name : string = '';
  Title : string = '';

procedure StartRegThread;
procedure EndRegThread;

implementation

uses
  Registry,Classes,Windows,SysUtils;

const
  CycleTime = 100;  // mS

  MetaDataKey = 'Software\Microsoft\MediaPlayer\CurrentMetadata';
  AuthorValue = 'Author';
  AlbumValue = 'Album';
  DurationValue = 'DurationString';
  NameValue = 'Name';
  TitleValue = 'Title';

type
  TRegReadThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TRegReadThread.Create;
begin
  FreeOnTerminate := true;
  inherited Create(false); // starts running immediately
end;

destructor TRegReadThread.Destroy;
begin
  inherited Destroy;
end;

procedure TRegReadThread.Execute;
var
  Reg : TRegistry;
  Error : string;

  procedure ReadTheValues;
  begin
    with Reg do begin
      DataLock.Enter;
      try
        try
          Author := ReadString(AuthorValue);
          Album := ReadString(AlbumValue);
          Duration := ReadString(DurationValue);
          Name := ReadString(NameValue);
          Title := ReadString(TitleValue);
        except
          on E:Exception do begin
            Error := 'Error reading registry: '+E.Message;
          end;
        end;
      finally
        DataLock.Leave;
      end;
    end;
  end;

var
  RegOpen : boolean;
  hEvent : THandle;
  RegNotifyEvent : longint;
begin
  hEvent := 0;
  RegNotifyEvent := -1;
  Reg := TRegistry.Create;
  with Reg do begin
    try
      RegOpen := OpenKey(MetaDataKey,false);
    except
      RegOpen := false;
      Error := 'Error opening registry';
    end;
    if RegOpen then begin
      ReadTheValues;
      try
        hEvent := CreateEvent(nil,false,false,nil);
      except
        RegOpen := false;
        Error := 'Error creating notify event';
      end;
    end;
  end;
  while not Terminated do begin
    if (Error <> '') then begin
      DataLock.Enter;
      try
        Title := Error;
        Error := '';
      finally
        DataLock.Leave;
      end;
    end;
    if RegOpen then begin
      if (RegNotifyEvent <> ERROR_SUCCESS) then
        RegNotifyEvent := RegNotifyChangeKeyValue(Reg.CurrentKey,true,REG_NOTIFY_CHANGE_LAST_SET,hEvent,true);
      if (RegNotifyEvent = ERROR_SUCCESS) then begin
        if (WaitForSingleObject(hEvent,CycleTime) = WAIT_OBJECT_0) then begin
          ReadTheValues;
          RegNotifyEvent := -1;
        end;
      end else
        sleep(CycleTime);
    end else begin
      sleep(CycleTime);
    end;
  end;
  try
    if RegOpen then Reg.CloseKey;
    if (hEvent > 0) then CloseHandle(hEvent);
  except
  end;
  Reg.Free;
end;

var
  RegReadThread : TRegReadThread = nil;

procedure StartRegThread;
begin
  DataLock := TCriticalSection.Create;
  RegReadThread := TRegReadThread.Create;
end;

procedure EndRegThread;
begin
  RegReadThread.Terminate;
  DataLock.Free;
  DataLock := nil;
end;

end.