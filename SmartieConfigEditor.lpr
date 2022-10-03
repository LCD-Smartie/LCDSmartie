program SmartieConfigEditor;

{$MODE Delphi}

uses
  Interfaces, // this includes the LCL widgetset
  Forms, USetup, ExceptionLogger, runtimetypeinfocontrols
  { you can add units after this };

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSetupForm, SetupForm);
  Application.Run;
end.

