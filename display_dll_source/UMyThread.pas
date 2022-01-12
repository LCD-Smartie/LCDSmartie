unit UMyThread;

{$MODE Delphi}{$M+}

interface

uses SyncObjs, Classes;

type
  TThreadMethod = procedure of object;

  TMyThread = class(TTHREAD)
  private
    method: TThreadMethod;
  published
    procedure Execute; override;
  public
    exited: TEvent;
    property Terminated;
    constructor Create(myMethod: TThreadMethod);
    destructor Destroy;  override;
  end;

  function errMsg(uError: Cardinal): String;

implementation

uses
  SysUtils, Windows;

constructor TMyThread.Create(myMethod: TThreadMethod);
begin
  method := myMethod;
  exited := TEvent.Create(nil, true, false, '');
  inherited Create(true);   // Create suspended.
end;

destructor TMyThread.Destroy;
begin
  exited.Free();
  inherited;
end;

procedure TMyThread.Execute;
begin
  method();
  exited.SetEvent();
end;

function errMsg(uError: Cardinal): String;
var
  psError: pointer;
  sError: String;
begin
  if (uError <> 0) then
  begin
    if (FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
      nil, uError, 0, @psError, 0, nil ) = 0) then psError := nil;

    if (psError <> nil) then
    begin
      sError := '#' + IntToStr(uError) + ': ' + PChar(psError);
      LocalFree(Cardinal(psError));
    end
    else
      sError := '#' + IntToStr(uError);
    Result := sError;
  end
  else
    Result := '#0'; // don't put "operation completed successfully!" It's too confusing!
end;

end.

