unit TeeConfig;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, IniFiles,
    windows;

const
  IniFile = 'Tee.ini';
  MaxDrivers = 10;

type
  TDriverInfo = record
    DllName: string;
    DllParameters: string;
  end;

type

  { TTeeConfig }

  TTeeConfig = class(TForm)
    AddButton: TButton;
    CloseButton: TButton;
    DisplayPluginComboBox: TComboBox;
    ParametersEdit: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    IDLabel: TLabel;
    Label7: TLabel;
    UsageLabel: TLabel;
    SpinEdit1: TSpinEdit;
    procedure AddButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure DisplayPluginComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadHint(DisplayDLLName: string);
    procedure SpinEdit1Change(Sender: TObject);
  private
    Driverinfo: Array [1 .. MaxDrivers] of TdriverInfo;
  public

  end;

{var
  TeeConfig: TTeeConfig;}

implementation

{$R *.lfm}

{ TTeeConfig }

procedure TTeeConfig.LoadHint(DisplayDLLName: string);
type
  TUsageFunc = function: PChar; stdcall;
var
  MyDLL: HMODULE;
  UsageFunc: TUsageFunc;
begin
  UsageLabel.Caption := 'no parameters';
  IDLabel.Caption := 'Warning: DLL may not be compatible!';
  ParametersEdit.Text := '';
  if FileExists(DisplayDLLName) then
  begin
    try
      MyDLL := LoadLibrary(PChar(DisplayDLLName));
      if GetLastError = 193 then
      {$IF Defined(CPUX64)}
        IDLabel.Caption := 'Warning: 32 bit DLL is not compatible!';
      {$ELSEIF Defined(CPUX86)}
        IDLabel.Caption := 'Warning: 64 bit DLL is not compatible!';
      {$IFEND}
      if not (MyDll = 0) then
      begin
        UsageFunc := GetProcAddress(MyDLL, PChar('DISPLAYDLL_Usage'));
        if assigned(UsageFunc) then
          UsageLabel.Caption := string(UsageFunc);
        UsageFunc := GetProcAddress(MyDLL, PChar('DISPLAYDLL_DriverName'));

        if assigned(UsageFunc) then
          IDLabel.Caption := string(UsageFunc);
        UsageFunc := GetProcAddress(MyDLL, PChar('DISPLAYDLL_DefaultParameters'));
        if assigned(UsageFunc) then
          ParametersEdit.Text := string(UsageFunc);

        FreeLibrary(MyDLL);
      end;
    except
      on e: Exception do ;
    end;
  end;
end;

procedure TTeeConfig.SpinEdit1Change(Sender: TObject);
var
  i: integer;
begin
  DisplayPluginComboBox.ItemIndex := 0;
  for i := 0 to DisplayPluginComboBox.Items.Count - 1 do
  begin
    if lowercase(Driverinfo[SpinEdit1.Value].DllName) = lowercase(DisplayPluginComboBox.Items[i]) then
    begin
      DisplayPluginComboBox.ItemIndex := i;
    end
  end;
end;

procedure TTeeConfig.DisplayPluginComboBoxChange(Sender: TObject);
begin
  if (DisplayPluginComboBox.ItemIndex > 0) then
    LoadHint('displays\' + DisplayPluginComboBox.Text)
  else
  begin
    UsageLabel.Caption := 'no parameters';
    IDLabel.Caption := '';
    ParametersEdit.Text := '';
  end;
  Driverinfo[SpinEdit1.Value].DllName := DisplayPluginComboBox.Text;
  Driverinfo[SpinEdit1.Value].DllParameters := ParametersEdit.Text;
end;

procedure TTeeConfig.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TTeeConfig.AddButtonClick(Sender: TObject);
var
  Settings : TIniFile;
  i: integer;
begin
  Settings := TIniFile.Create(IniFile);

  Driverinfo[SpinEdit1.Value].DllName := DisplayPluginComboBox.Text;
  Driverinfo[SpinEdit1.Value].DllParameters := ParametersEdit.Text;

  for i := 1 to MaxDrivers do
  begin
    Settings.WriteString('Driver'+inttostr(i), 'DllName', Driverinfo[i].DllName);
    Settings.WriteString('Driver'+inttostr(i), 'DllParameters', Driverinfo[i].DllParameters);
  end;
  Settings.free;
end;

procedure TTeeConfig.FormCreate(Sender: TObject);
var
  Settings : TIniFile;
  FindResult, i: integer;
  SR: TSearchRec;
  DLLPath: string;
begin
  Settings := TIniFile.Create(IniFile);

  for i := 1 to MaxDrivers do
  begin
    Driverinfo[i].DllName := Settings.ReadString('Driver'+inttostr(i), 'DllName', '');
    Driverinfo[i].DllParameters := Settings.ReadString('Driver'+inttostr(i), 'DllParameters', '');
  end;
  Settings.free;

  DisplayPluginComboBox.Items.Clear;
  DisplayPluginComboBox.Items.Add('None');
  DLLPath := extractfilepath(ParamStr(0)) + 'displays\';
  FindResult := findfirst(DLLPath + '*.dll', 0, SR);
  while (FindResult = 0) do
  begin
    DisplayPluginComboBox.Items.Add(extractfilename(SR.Name));
    FindResult := FindNext(SR);
  end;
  SysUtils.findclose(SR);
  DisplayPluginComboBox.ItemIndex := 0;

  for i := 0 to DisplayPluginComboBox.Items.Count - 1 do
  begin
    if lowercase(Driverinfo[1].DllName) = lowercase(DisplayPluginComboBox.Items[i]) then
    begin
      DisplayPluginComboBox.ItemIndex := i;
    end;
  end;
  if (DisplayPluginComboBox.ItemIndex > 0) then
    LoadHint('displays\' + DisplayPluginComboBox.Text);
   ParametersEdit.Text := Driverinfo[1].DllParameters;
  //DisplayPluginComboBoxChange(nil);
end;



end.

