program LCDSmartie;

{$MODE Delphi}

uses
  {madExcept,
  madLinkDisAsm,}
  Forms, Interfaces,
  UMain  {LCDSmartieDisplayForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'LCD Smartie';
  Application.HelpFile := 'readme.txt';
  Application.ShowMainForm := false;
  Application.CreateForm(TLCDSmartieDisplayForm, LCDSmartieDisplayForm);
  Application.Run;
end.
