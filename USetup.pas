unit USetup;
{******************************************************************************
 *
 *  LCD Smartie - LCD control software.
 *  Copyright (C) 2000-2003  BassieP
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either version 2
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 *  $Source: /root/lcdsmartie-cvsbackup/lcdsmartie/USetup.pas,v $
 *  $Revision: 1.64 $ $Date: 2011/06/04 16:48:30 $
 *****************************************************************************}
{.DEFINE VCORP}
interface

uses
  Commctrl,
  Dialogs, Grids, StdCtrls, Controls, Spin, Buttons, ComCtrls, Classes,
  Forms, ExtCtrls, FileCtrl,
  ExtDlgs, EditBtn, SpinEx, UExceptionLogger;

const
  NoVariable = 'Variable: ';

  { TSetupForm }
type

  TCheckBoxArray = array of TCheckBox; // for custom character editor

  TSetupForm = class(TForm)
    ActionsTimerSpinEdit: TSpinEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    ComPortsButton: TButton;
    CopyToScreenComboBox: TComboBox;
    DLLCheckIntervalSpinEdit: TSpinEdit;
    ExceptionLogger1: TExceptionLogger;
    Label24: TLabel;
    Label58: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Line1MemoEdit: TMemo;
    LeftPageControl: TPageControl;
    Line2MemoEdit: TMemo;
    Line3MemoEdit: TMemo;
    Line1EditButton: TSpeedButton;
    Line2EditButton: TSpeedButton;
    Line3EditButton: TSpeedButton;
    Line4EditButton: TSpeedButton;
    Line4MemoEdit: TMemo;
    MoveToScreenComboBox: TComboBox;
    RandomizeScreensCheckBox: TCheckBox;
    StayOnTopCheckBox: TCheckBox;
    SwapWithScreenComboBox: TComboBox;
    WebProxyPortEdit: TEdit;
    WebProxyServerEdit: TEdit;
    WinampTabSheet: TTabSheet;
    SysInfoTabSheet: TTabSheet;
    GameStatsTabSheet: TTabSheet;
    InternetTabSheet: TTabSheet;
    MiscTabSheet: TTabSheet;
    MiscListBox: TListBox;
    InternetListBox: TListBox;
    SysInfoListBox: TListBox;
    WinampListBox: TListBox;
    VariableEdit: TEdit;
    GameServerEdit: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    QStatLabel: TLabel;
    OpenDialog2: TOpenDialog;
    GameTypeComboBox: TComboBox;
    InsertButton: TButton;
    InternetRefreshTimeSpinEdit: TSpinEdit;
    Label36: TLabel;
    GamestatsRefreshTimeSpinEdit: TSpinEdit;
    Label37: TLabel;
    Label35: TLabel;
    Label40: TLabel;
    SetiAtHomeTabSheet: TTabSheet;
    SetiAtHomeListBox: TListBox;
    SetiAtHomeEmailEdit: TEdit;
    Label41: TLabel;
    EmailTabSheet: TTabSheet;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    EmailPasswordEdit: TEdit;
    EmailLoginEdit: TEdit;
    EmailServerEdit: TEdit;
    EmailCheckTimeSpinEdit: TSpinEdit;
    Label48: TLabel;
    EmailAccountComboBox: TComboBox;
    Label50: TLabel;
    OpenDialog1: TOpenDialog;
    WinampLocationEdit: TEdit;
    WinampLocationBrowseButton: TSpeedButton;
    WinampLocationLabel: TLabel;
    GamestatsListBox: TListBox;
    NetworkStatsTabSheet: TTabSheet;
    NetworkStatsListBox: TListBox;
    FoldingAtHomeTabSheet: TTabSheet;
    FoldingAtHomeEmailEdit: TEdit;
    FoldingAtHomeListBox: TListBox;
    Label23: TLabel;
    MainPageControl: TPageControl;
    ScreensTabSheet: TTabSheet;
    ActionsTabSheet: TTabSheet;
    ScreenSettingsGroupBox: TGroupBox;
    Label5: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label47: TLabel;
    Label46: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label4: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    DontScrollLine1CheckBox: TCheckBox;
    DontScrollLine2CheckBox: TCheckBox;
    DontScrollLine3CheckBox: TCheckBox;
    DontScrollLine4CheckBox: TCheckBox;
    GroupBox4: TGroupBox;
    ContinueLine1CheckBox: TCheckBox;
    ContinueLine2CheckBox: TCheckBox;
    ContinueLine3CheckBox: TCheckBox;
    GroupBox5: TGroupBox;
    CenterLine1CheckBox: TCheckBox;
    CenterLine2CheckBox: TCheckBox;
    CenterLine3CheckBox: TCheckBox;
    CenterLine4CheckBox: TCheckBox;
    GroupBox6: TGroupBox;
    ThemeNumberSpinEdit: TSpinEdit;
    ProgramSettingsGroupBox: TGroupBox;
    Label1: TLabel;
    ProgramRefreshIntervalSpinEdit: TSpinEdit;
    ScreenEnabledCheckBox: TCheckBox;
    TimeToShowSpinEdit: TSpinEdit;
    ActionsStringGrid: TStringGrid;
    ActionAddButton: TButton;
    ActionDeleteButton: TButton;
    LCDSmartieUpdateCheckBox: TCheckBox;
    LCDFeaturesTabSheet: TTabSheet;
    ButtonsListBox: TListBox;
    Label27: TLabel;
    Label29: TLabel;
    LastKeyPressedEdit: TEdit;
    Label56: TLabel;
    Label30: TLabel;
    Label57: TLabel;
    Label6: TLabel;
    ProgramScrollIntervalSpinEdit: TSpinEdit;
    Label59: TLabel;
    Label28: TLabel;
    StickyCheckbox: TCheckBox;
    StartupTabSheet: TTabSheet;
    GroupBox7: TGroupBox;
    NoAutoStart: TRadioButton;
    AutoStart: TRadioButton;
    AutoStartHide: TRadioButton;
    GroupBox8: TGroupBox;
    HideOnStartup: TCheckBox;
    ColorSchemeComboBox: TComboBox;
    ScreenLabel: TLabel;
    ScreenSpinEdit: TSpinEdit;
    DisplayGroup2: TGroupBox;
    Panel1: TPanel;
    DisplayPageControl: TPageControl;
    PluginTabsheet: TTabSheet;
    DisplayPluginsLabel: TLabel;
    IDLabel: TLabel;
    Label14: TLabel;
    UsageLabel: TLabel;
    DisplayPluginList: TComboBox;
    ParametersEdit: TEdit;
    ScreenTabsheet: TTabSheet;
    Label3: TLabel;
    LCDSizeComboBox: TComboBox;
    GroupBox3: TGroupBox;
    ContrastTrackBar: TTrackBar;
    GroupBox1: TGroupBox;
    BrightnessTrackBar: TTrackBar;
    EmailSSLEdit: TEdit;
    Label2: TLabel;
    EmulateLCDCheckbox: TCheckBox;
    ShutdownMessageGroup: TGroupBox;
    ShutdownEdit1: TEdit;
    ShutdownEdit2: TEdit;
    ShutdownEdit3: TEdit;
    ShutdownEdit4: TEdit;
    MyTabSheet: TTabSheet;
    TrayIcon: TEdit;
    Label9: TLabel;
    TrayIconPreview32: TImage;
    OpenIco: TOpenPictureDialog;
    TrayIconBrowseButton: TSpeedButton;
    TrayIconPreview16: TImage;
    Label10: TLabel;
    SkinPath: TEdit;
    SkinPathBrowseButton: TSpeedButton;
    DistributedNetLogfileEdit: TEdit;
    DistributedNetBrowseButton: TSpeedButton;
    Label34: TLabel;
    PluginsTabSheet: TTabSheet;
    PluginListBox: TFileListBox;
    Btn_PluginRefresh: TButton;
    CCharTabSheet: TTabSheet;
    CreateCCharLocSpinEdit: TSpinEdit;
    CreateCCharRadioButton: TRadioButton;
    Label20: TLabel;
    Panel2: TPanel;
    CCharCheckBox1: TCheckBox;
    CCharCheckBox2: TCheckBox;
    CCharCheckBox3: TCheckBox;
    CCharCheckBox4: TCheckBox;
    CCharCheckBox5: TCheckBox;
    CCharCheckBox6: TCheckBox;
    CCharCheckBox7: TCheckBox;
    CCharCheckBox8: TCheckBox;
    CCharCheckBox9: TCheckBox;
    CCharCheckBox10: TCheckBox;
    CCharCheckBox11: TCheckBox;
    CCharCheckBox12: TCheckBox;
    CCharCheckBox13: TCheckBox;
    CCharCheckBox14: TCheckBox;
    CCharCheckBox15: TCheckBox;
    CCharCheckBox16: TCheckBox;
    CCharCheckBox17: TCheckBox;
    CCharCheckBox18: TCheckBox;
    CCharCheckBox19: TCheckBox;
    CCharCheckBox20: TCheckBox;
    CCharCheckBox21: TCheckBox;
    CCharCheckBox22: TCheckBox;
    CCharCheckBox23: TCheckBox;
    CCharCheckBox24: TCheckBox;
    CCharCheckBox25: TCheckBox;
    CCharCheckBox26: TCheckBox;
    CCharCheckBox27: TCheckBox;
    CCharCheckBox28: TCheckBox;
    CCharCheckBox29: TCheckBox;
    CCharCheckBox30: TCheckBox;
    CCharCheckBox31: TCheckBox;
    CCharCheckBox32: TCheckBox;
    CCharCheckBox33: TCheckBox;
    CCharCheckBox34: TCheckBox;
    CCharCheckBox35: TCheckBox;
    CCharCheckBox36: TCheckBox;
    CCharCheckBox37: TCheckBox;
    CCharCheckBox38: TCheckBox;
    CCharCheckBox39: TCheckBox;
    CCharCheckBox40: TCheckBox;
    UseCCharRadioButton2: TRadioButton;
    Label21: TLabel;
    UseCCharLocSpinEdit: TSpinEdit;
    NetworkStatsAdapterListButton: TButton;
    EmailLastSubjectRadioButton: TRadioButton;
    EmailLastFromRadioButton: TRadioButton;
    EmailMessageCountRadioButton: TRadioButton;
    CopyToScreenButton: TButton;
    MoveToScreenButton: TButton;
    SwapWithScreenButton: TButton;
    TabSheet1: TTabSheet;
    EnableRemoteSendCheckBox: TCheckBox;
    Label16: TLabel;
    RemoteSendBindIPEdit: TEdit;
    RemoteSendPortEdit: TEdit;
    RemoteSendUseSSLCheckBox: TCheckBox;
    RemoteSendGenerateCertKeyButton: TButton;
    Label19: TLabel;
    Label22: TLabel;
    Label38: TLabel;
    RemoteSendPasswordEdit: TEdit;
    Label39: TLabel;
    Label42: TLabel;
    Label49: TLabel;
    Label55: TLabel;
    FoldEnableCheckBox: TCheckBox;
    SetiEnableCheckBox: TCheckBox;
    TransitionStyleComboBox: TComboBox;
    TransitionTimeSpinEdit: TSpinEdit;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;

    procedure ActionsStringGridSelectEditor(Sender: TObject; aCol,
      aRow: Integer; var Editor: TWinControl);
    procedure ActionsStringGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure ComPortsButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LCDSizeComboBoxChange(Sender: TObject);
    procedure PluginTabsheetShow(Sender: TObject);
    procedure ScreenSpinEditChange(Sender: TObject);
    procedure WinampListBoxClick(Sender: TObject);
    procedure InsertButtonClick(Sender: TObject);
    procedure SysInfoListBoxClick(Sender: TObject);
    procedure InternetListBoxClick(Sender: TObject);
    procedure QStatLabelClick(Sender: TObject);
    procedure MiscListBoxClick(Sender: TObject);
    procedure LeftPageControlChange(Sender: TObject);
    procedure GameServerEditExit(Sender: TObject);
    procedure SetiAtHomeListBoxClick(Sender: TObject);
    procedure DistributedNetBrowseButtonClick(Sender: TObject);
    procedure EmailAccountComboBoxChange(Sender: TObject);
    procedure ContinueLine1CheckBoxClick(Sender: TObject);
    procedure ContinueLine2CheckBoxClick(Sender: TObject);
    procedure ContinueLine3CheckBoxClick(Sender: TObject);
    procedure WinampLocationBrowseButtonClick(Sender: TObject);
    procedure GamestatsListBoxClick(Sender: TObject);
    procedure Line1MemoEditEnter(Sender: TObject);
    procedure Line2MemoEditEnter(Sender: TObject);
    procedure Line3MemoEditEnter(Sender: TObject);
    procedure Line4MemoEditEnter(Sender: TObject);
    procedure NetworkStatsListBoxClick(Sender: TObject);
    procedure FoldingAtHomeListBoxClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MainPageControlChange(Sender: TObject);
    procedure ActionAddButtonClick(Sender: TObject);
    procedure ActionDeleteButtonClick(Sender: TObject);
    procedure ButtonsListBoxClick(Sender: TObject);
    procedure Line4MemoEditKeyDown(Sender: TObject; var Key: Word; Shift:
      TShiftState);
    procedure Line1MemoEditKeyDown(Sender: TObject; var Key: Word; Shift:
      TShiftState);
    procedure Line2MemoEditKeyDown(Sender: TObject; var Key: Word; Shift:
      TShiftState);
    procedure Line3MemoEditKeyDown(Sender: TObject; var Key: Word; Shift:
      TShiftState);
    procedure StickyCheckboxClick(Sender: TObject);
    procedure ColorSchemeComboBoxChange(Sender: TObject);
    procedure DisplayPluginListChange(Sender: TObject);
    procedure ContrastTrackBarChange(Sender: TObject);
    procedure BrightnessTrackBarChange(Sender: TObject);
    procedure Btn_PluginRefreshClick(Sender: TObject);
    procedure PluginListBoxDblClick(Sender: TObject);
    procedure PluginListBoxClick(Sender: TObject);
    procedure ShutdownEditEnter(Sender: TObject);

    procedure ShutdownEdit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ShutdownEdit2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ShutdownEdit3KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ShutdownEdit4KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure TrayIconBrowseButtonClick(Sender: TObject);

    procedure DrawPreviewIcons(const sIconFileName: string);

    procedure SkinPathBrowseButtonClick(Sender: TObject);
    procedure LineEditClick(Sender: TObject);
    procedure OpeIcoFolderChange(Sender: TObject);
    procedure CCharEditGridChange(Sender: TObject);
    procedure NetworkStatsAdapterListButtonClick(Sender: TObject);
    procedure CopyToScreenButtonClick(Sender: TObject);
    procedure MoveToScreenButtonClick(Sender: TObject);
    procedure SwapWithScreenButtonClick(Sender: TObject);
    procedure RemoteSendGenerateCertKeyButtonClick(Sender: TObject);
    procedure RemoteSendUseSSLCheckBoxClick(Sender: TObject);
    procedure FoldEnableCheckBoxClick(Sender: TObject);
    procedure SetiEnableCheckBoxClick(Sender: TObject);

  private
    DLLPath : string;
    setupbutton: Integer;
    CurrentlyShownEmailAccount: Integer;
    CurrentScreen: Integer;
    Procedure FocusToInputField;
    procedure SaveScreen(scr: Integer);
    procedure LoadScreen(scr: Integer);
    procedure LoadHint(DisplayDLLName : string);
  end;

  procedure UpdateSetupForm(cKey : char);

implementation
uses
  Windows, ShellApi, graphics, sysutils,
  UMain, UConfig, UDataEmail, UDataNetwork, UDataWinamp,
  UIconUtils, UEditLine, IpRtrMib, IpHlpApi, lazutf8, registry;

{$R *.lfm}

procedure TSetupForm.LoadHint(DisplayDLLName : string);
type
  TUsageFunc = function : pchar; stdcall;
var
  MyDLL : HMODULE;
  UsageFunc : TUsageFunc;
begin
  UsageLabel.Caption := 'no parameters';
  IDLabel.Caption := 'Warning: DLL may not be compatible!';
  ParametersEdit.Text := '';
  if FileExists(DisplayDLLName) then begin
    try
      MyDLL := LoadLibrary(pchar(DisplayDLLName));
      if not (MyDll = 0) then begin
        UsageFunc := GetProcAddress(MyDLL,pchar('DISPLAYDLL_Usage'));
        if assigned(UsageFunc) then
          UsageLabel.Caption := string(UsageFunc);
        UsageFunc := GetProcAddress(MyDLL,pchar('DISPLAYDLL_DriverName'));
        
        if assigned(UsageFunc) then
          IDLabel.Caption := string(UsageFunc);
        UsageFunc := GetProcAddress(MyDLL,pchar('DISPLAYDLL_DefaultParameters'));
        if assigned(UsageFunc) then
          ParametersEdit.Text := string(UsageFunc);

        FreeLibrary(MyDLL);
      end;
    except
      on e:exception do;
    end;
  end;
end;

procedure TSetupForm.FormShow(Sender: TObject);
var
  SR : TSearchRec;
  Loop,FindResult : integer;
  NetStat : TNetworkStatistics;
  WinampStat : TWinampStat;
  i : Integer;
  ActOpr : string;
begin
  SetupForm.Top := config.SettingsFormPosTop;
  SetupForm.Left := config.SettingsFormPosLeft;

  MainPageControl.ActivePage := ScreensTabSheet;
  LeftPageControl.ActivePageIndex := config.LastTabIndex;

  GameServerEdit.text := config.gameServer[1, 1];

  ActionsStringGrid.colcount := 6;
  ActionsStringGrid.rowcount := 1; // lazarus grids dont work with 0 rows

  // setup grid column widths
  ActionsStringGrid.ColWidths[0] := 40;
  ActionsStringGrid.ColWidths[1] := 205;
  ActionsStringGrid.ColWidths[2] := 40;
  ActionsStringGrid.ColWidths[3] := 46;
  ActionsStringGrid.ColWidths[4] := 36;
  ActionsStringGrid.ColWidths[5] := 196;
  // Populate the grid
  for i := 1 to config.totalactions do
  begin
    ActionsStringGrid.Cells[0, ActionsStringGrid.RowCount-1] := 'if';
    ActionsStringGrid.Cells[1, ActionsStringGrid.RowCount-1] := config.actionsArray[i, 1];
    case (StrToInt(config.actionsArray[i, 2]))  of
      0 : ActOpr := '>';
      1 : ActOpr := '<';
      2 : ActOpr := '=';
      3 : ActOpr := '<=';
      4 : ActOpr := '>=';
      else ActOpr := '<>';
    end;
    ActionsStringGrid.Cells[2, ActionsStringGrid.RowCount-1] := ActOpr;
    ActionsStringGrid.Cells[3, ActionsStringGrid.RowCount-1] := config.actionsArray[i, 3];
    ActionsStringGrid.Cells[4, ActionsStringGrid.RowCount-1] := 'then';
    ActionsStringGrid.Cells[5, ActionsStringGrid.RowCount-1] := config.actionsArray[i, 4];
    ActionsStringGrid.RowCount := ActionsStringGrid.RowCount + 1;
  end;
  ActionsStringGrid.DeleteRow(config.totalactions); // now delete that first row we created

  ScreenSpinEdit.MaxValue := MaxScreens;

  // load curent screen into setup form
  ScreenSpinEdit.Value := activeScreen;
  LoadScreen(activeScreen);

  ProgramRefreshIntervalSpinEdit.Value := config.refreshRate;
  WinampLocationEdit.text := config.winampLocation;
  ColorSchemeComboBox.itemindex := config.colorOption;

  TrayIcon.Text := config.sTrayIcon;
  SkinPath.Text := config.sSkinPath;
  DrawPreviewIcons(TrayIcon.Text);

  SetiAtHomeEmailEdit.text := config.setiEmail;

  DistributedNetLogfileEdit.text := config.distLog;

  EmailCheckTimeSpinEdit.Value := config.emailPeriod;
  DLLCheckIntervalSpinEdit.Value := config.dllPeriod;
  ProgramScrollIntervalSpinEdit.Value := config.scrollPeriod;

  StayOnTopCheckBox.checked := config.alwaysOnTop;
  HideOnStartup.Checked := config.bHideOnStartup;
  NoAutoStart.Checked := True;
  AutoStart.Checked := config.bAutoStart;
  AutoStartHide.Checked := config.bAutoStartHide;
  EmulateLCDCheckbox.Checked := config.EmulateLCD;
  HideOnStartup.Checked := config.bHideOnStartup;
  ShutdownEdit1.Text := config.ShutdownMessage[1];
  ShutdownEdit2.Text := config.ShutdownMessage[2];
  ShutdownEdit3.Text := config.ShutdownMessage[3];
  ShutdownEdit4.Text := config.ShutdownMessage[4];

  WebProxyServerEdit.text := config.httpProxy;
  WebProxyPortEdit.text := IntToStr(config.httpProxyPort);

  EmailAccountComboBox.Clear;
  for i := 1 to MaxEmailAccounts do begin
    EmailAccountComboBox.Items.Add(IntToStr(i));
  end;
  EmailAccountComboBox.itemindex := 0;
  EmailServerEdit.text := config.pop[1].server;
  EmailLoginEdit.text := config.pop[1].user;
  EmailPasswordEdit.text := config.pop[1].pword;
  EmailSSLEdit.Text := config.pop[1].port_ssl;

  NetworkStatsListBox.Clear;
  for NetStat := FirstNetworkStat to LastNetworkStat do begin
    NetworkStatsListBox.Items.Add(NetworkUserHints[NetStat]);
  end;

  WinampListBox.Clear;
  for WinampStat := FirstWinampStat to LastWinampStat do begin
    WinampListBox.Items.Add(WinampHints[WinampStat]);
  end;

  LCDSizeComboBox.Items.Clear;
  for i := 1 to MaxScreenSizes do
    LCDSizeComboBox.Items.Add(ScreenSizes[i].SizeName);
  LCDSizeComboBox.itemindex := config.ScreenSize-1;
  LCDSizeComboBoxChange(Sender);

  // put display plugin settings on screen
  ContrastTrackBar.position := config.DLL_contrast;
  BrightnessTrackBar.position := config.DLL_brightness;

  DisplayPluginList.Items.Clear;
  DisplayPluginList.Items.Add('None');
  DLLPath := extractfilepath(paramstr(0))+'displays\';
  FindResult := findfirst(DLLPath+'*.dll',0,SR);
  while (FindResult = 0) do begin
    DisplayPluginList.Items.Add(extractfilename(SR.Name));
    FindResult := FindNext(SR);
  end;
  findclose(SR);
  DisplayPluginList.ItemIndex := 0;
  for Loop := 0 to DisplayPluginList.Items.Count-1 do begin
    if lowercase(config.DisplayDLLName) = lowercase(DisplayPluginList.Items[Loop]) then begin
      DisplayPluginList.ItemIndex := Loop;
    end;
  end;

  DisplayPluginListChange(Sender);
  ParametersEdit.Text := config.DisplayDLLParameters; // set our original parameters back

  InternetRefreshTimeSpinEdit.Value := config.newsRefresh;
  RandomizeScreensCheckBox.checked := config.randomScreens;
  GamestatsRefreshTimeSpinEdit.Value := config.gameRefresh;
  FoldingAtHomeEmailEdit.text := config.foldUserid;
  LCDSmartieUpdateCheckBox.checked := config.checkUpdates;

  EnableRemoteSendCheckBox.Checked := config.EnableRemoteSend;
  RemoteSendBindIPEdit.Text := config.RemoteSendBindIP;
  RemoteSendPortEdit.Text := config.RemoteSendPort;
  RemoteSendPasswordEdit.Text := config.RemoteSendPassword;
  
  if fileExists(ExtractFilePath(ParamStr(0))+'openssl\cert.pem') and
     fileExists(ExtractFilePath(ParamStr(0))+'openssl\key.pem') then
     RemoteSendUseSSLCheckBox.Checked := config.RemoteSendUseSSL
  else
     RemoteSendUseSSLCheckBox.Checked := false;

  FoldEnableCheckBox.Checked := config.foldEnabled;
  SetiEnableCheckBox.Checked := config.setiEnabled;

  for i := 1 to 24 do ButtonsListBox.Items.Delete(1);
  LCDFeaturesTabSheet.Enabled := true;
  ButtonsListBox.Items.Add('FanSpeed(1,1) (nr,divider)');

  // Screen re-arrange populate combo boxes
  CopyToScreenComboBox.Clear;
  MoveToScreenComboBox.Clear;
  SwapWithScreenComboBox.Clear;
  for i := 1 to MaxScreens do begin
    CopyToScreenComboBox.Items.Add(IntToStr(i));
    MoveToScreenComboBox.Items.Add(IntToStr(i));
    SwapWithScreenComboBox.Items.Add(IntToStr(i));
  end;
  CopyToScreenComboBox.ItemIndex := 0;
  MoveToScreenComboBox.ItemIndex := 0;
  SwapWithScreenComboBox.ItemIndex := 0;

  ActionsTimerSpinEdit.Value := config.ActionsTimer;

  { The below is commented out as it is reading/writing directly to the device.
    We need to know more details so we can move it in to the lcd code.

     // var section move down to here:
      var
        line, line2: String;
        ch: char;
        laatstepacket: Boolean;

      label nextpacket;

  LCDSmartieDisplayForm.VaComm1.WriteChar(chr($FE));   //probe 4 one-wire devices
  LCDSmartieDisplayForm.VaComm1.WriteChar(chr($C8));
  LCDSmartieDisplayForm.VaComm1.WriteChar(chr($02));


  laatstepacket := false;
nextpacket:
  line := '';
  line2 := '';
  while LCDSmartieDisplayForm.VaComm1.ReadBufUsed >= 1 do
  begin
    LCDSmartieDisplayForm.VaComm1.ReadChar(Ch);
    line := line + ch;
  end;
  if length(line)>13 then
  begin
    if line[1] + line[2]='#*' then
    begin
      if line[3] = chr(10) then laatstepacket := true;
      if (line[5] = chr(0)) and (line[14] = chr(0)) then
      begin
        for i := 0 to 7 do
        begin
          line2 := line2 + IntToHex(ord(line[i + 6]), 2) + ' ';
        end;
        ButtonsListBox.Items.Add(line2);
      end;
    end;
    if laatstepacket <> true then goto nextpacket;
  end;}

end;

procedure TSetupForm.ActionsStringGridSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
var
  Items : array [0..27] of string = ('Next Theme',
          'Last Theme',
          'Next screen',
          'Last screen',
          'Goto Theme(2)',
          'Goto Screen(2)',
          'Freeze/unfreeze screen',
          'Refresh all data',
          'Backlight(0/1) (0=off 1=on)',
          'BacklightToggle',
          'Backlight Flash(5) (nr. of times)',
          'PlayWave[c:\wave.wav]',
          'Execute[c:\autoexec.bat]',
          'Winamp next track',
          'Winamp last track',
          'Winamp play',
          'Winamp stop',
          'Winamp pause',
          'Winamp Shuffle (toggle)',
          'Winamp volume down',
          'Winamp volume up',
          'EnableScreen(1-20)',
          'DisableScreen(1-20)',
          '$dll(name.dll,2,param1,param2)',
          'GPO(1-8,0/1)  (0=off 1=on)',
          'GPOToggle(1-8)',
          'GPOFlash(1-8,2)  (nr. of times)',
          'Fan(1-3,0-255) (0-255=speed)');
begin
  if aCol=2 then begin
    Editor := ActionsStringGrid.EditorByStyle(cbsPickList);
    TPickListCellEditor(Editor).Items.CommaText := '=,<,>,<=,>=,<>';
  end;
  if aCol=5 then begin
    Editor := ActionsStringGrid.EditorByStyle(cbsPickList);
   TPickListCellEditor(Editor).Items.SetStrings(items);
  end;
end;

procedure TSetupForm.ActionsStringGridSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
  if (aCol=0) or (aCol=4) then
    ActionsStringGrid.Options:=ActionsStringGrid.Options-[goEditing];

  if (aCol=1) or (aCol=2) or (aCol=3) or (aCol=5) then
    ActionsStringGrid.Options:=ActionsStringGrid.Options+[goEditing];
end;

//////////// LIST COM PORTS BUTTON ///////////////////

procedure TSetupForm.ComPortsButtonClick(Sender: TObject);
var
i:integer;
reg:tregistry;
portnames: tstringlist;
ports: string;
begin
  // really should try..except this
  portnames := TStringList.Create;
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if Reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
    reg.GetValueNames(portnames);
  for i := 0 to portnames.Count - 1 do
    ports := ports + portnames[i] + ' ' + (reg.ReadString(portnames[i]))+#13#10;
  showmessage(ports);
  Reg.Free;
end;

procedure TSetupForm.DisplayPluginListChange(Sender: TObject);
begin
  if (DisplayPluginList.ItemIndex > 0) then
    LoadHint(DLLPath+DisplayPluginList.Text)
  else begin
    UsageLabel.Caption := 'no parameters';
    IDLabel.Caption := '';
    ParametersEdit.Text := '';
  end;
end;

procedure TSetupForm.LCDSizeComboBoxChange(Sender: TObject);
begin
  if LCDSizeComboBox.itemindex < 0 then LCDSizeComboBox.itemindex := 0;

  if LCDSizeComboBox.itemindex < 5 then
  begin
    ContinueLine1CheckBox.checked := false;
    Line2MemoEdit.Visible := false;
    Line3MemoEdit.Visible := false;
    Line4MemoEdit.Visible := false;
    ShutdownEdit2.Visible := false;
    ShutdownEdit3.Visible := false;
    ShutdownEdit4.Visible := false;
    DontScrollLine2CheckBox.Visible := false;
    DontScrollLine3CheckBox.Visible := false;
    DontScrollLine4CheckBox.Visible := false;
    ContinueLine1CheckBox.Visible := false;
    ContinueLine2CheckBox.Visible := false;
    ContinueLine3CheckBox.Visible := false;
    CenterLine2CheckBox.visible := false;
    CenterLine3CheckBox.visible := false;
    CenterLine4CheckBox.visible := false;
  end;
  if (LCDSizeComboBox.itemindex < 9) and (LCDSizeComboBox.itemindex > 4) then
  begin
    if ContinueLine1CheckBox.checked = false then Line2MemoEdit.Visible := true;
    ContinueLine2CheckBox.checked := false;
    Line3MemoEdit.Visible := false;
    Line4MemoEdit.Visible := false;
    ShutdownEdit2.Visible := true;
    ShutdownEdit3.Visible := false;
    ShutdownEdit4.Visible := false;
    DontScrollLine2CheckBox.Visible := true;
    DontScrollLine3CheckBox.Visible := false;
    DontScrollLine4CheckBox.Visible := false;
    ContinueLine1CheckBox.Visible := true;
    ContinueLine2CheckBox.Visible := false;
    ContinueLine3CheckBox.Visible := false;
    CenterLine2CheckBox.visible := true;
    CenterLine3CheckBox.visible := false;
    CenterLine4CheckBox.visible := false;
  end;
  if LCDSizeComboBox.itemindex > 8 then
  begin
    if ContinueLine1CheckBox.checked = false then Line2MemoEdit.Visible := true;
    if ContinueLine2CheckBox.checked = false then Line3MemoEdit.Visible := true;
    if ContinueLine3CheckBox.checked = false then Line4MemoEdit.Visible := true;
    ShutdownEdit2.Visible := true;
    ShutdownEdit3.Visible := true;
    ShutdownEdit4.Visible := true;
    DontScrollLine2CheckBox.Visible := true;
    DontScrollLine3CheckBox.Visible := true;
    DontScrollLine4CheckBox.Visible := true;
    ContinueLine1CheckBox.Visible := true;
    ContinueLine2CheckBox.Visible := true;
    ContinueLine3CheckBox.Visible := true;
    CenterLine2CheckBox.visible := true;
    CenterLine3CheckBox.visible := true;
    CenterLine4CheckBox.visible := true;
  end;
end;

procedure TSetupForm.PluginTabsheetShow(Sender: TObject);
begin

end;

procedure TSetupForm.SaveScreen(scr: Integer);
var
  y: Integer;
begin
  if scr = 0 then Exit;
  config.screen[scr].line[1].text := Line1MemoEdit.text;
  config.screen[scr].line[2].text := Line2MemoEdit.text;
  config.screen[scr].line[3].text := Line3MemoEdit.text;
  config.screen[scr].line[4].text := Line4MemoEdit.text;

  config.screen[scr].settings.enabled := ScreenEnabledCheckBox.checked;
  try
    config.screen[scr].settings.theme := ThemeNumberSpinEdit.value-1;
  except
    config.screen[scr].settings.theme := 0;
  end;
  try
    config.screen[scr].settings.showTime := TimeToShowSpinEdit.value;
  except
    config.screen[scr].settings.showTime := 10;
  end;
    config.screen[scr].settings.bSticky := StickyCheckbox.Checked;

  config.screen[scr].line[1].center := CenterLine1CheckBox.checked;
  config.screen[scr].line[2].center := CenterLine2CheckBox.checked;
  config.screen[scr].line[3].center := CenterLine3CheckBox.checked;
  config.screen[scr].line[4].center := CenterLine4CheckBox.checked;

  config.screen[scr].line[1].noscroll := DontScrollLine1CheckBox.checked;
  config.screen[scr].line[2].noscroll := DontScrollLine2CheckBox.checked;
  config.screen[scr].line[3].noscroll := DontScrollLine3CheckBox.checked;
  config.screen[scr].line[4].noscroll := DontScrollLine4CheckBox.checked;
  LCDSmartieDisplayForm.ResetScrollPositions();

  config.screen[scr].line[1].contNextLine := ContinueLine1CheckBox.checked;
  config.screen[scr].line[2].contNextLine := ContinueLine2CheckBox.checked;
  config.screen[scr].line[3].contNextLine := ContinueLine3CheckBox.checked;
  config.screen[scr].line[4].contNextLine := False;

  config.screen[scr].settings.TransitionStyle := TTransitionStyle(TransitionStyleComboBox.ItemIndex);
  config.screen[scr].settings.TransitionTime := TransitionTimeSpinEdit.Value;
end;

procedure TSetupForm.LoadScreen(scr: Integer);
var
  ascreen: TScreen;
begin
  ascreen := config.screen[scr];
  ScreenEnabledCheckBox.checked := ascreen.settings.enabled;
  ThemeNumberSpinEdit.value := ascreen.settings.theme + 1;
  TimeToShowSpinEdit.value := ascreen.settings.showTime;
  StickyCheckbox.checked := ascreen.settings.bSticky;
  TimeToShowSpinEdit.enabled := not ascreen.settings.bSticky;

  DontScrollLine1CheckBox.checked := false;
  DontScrollLine2CheckBox.checked := false;
  DontScrollLine3CheckBox.checked := false;
  DontScrollLine4CheckBox.checked := false;
  ContinueLine1CheckBox.checked := false;
  ContinueLine2CheckBox.checked := false;
  ContinueLine3CheckBox.checked := false;
  DontScrollLine1CheckBox.enabled := true;
  DontScrollLine1CheckBox.checked := false;
  Line2MemoEdit.enabled := true;
  DontScrollLine2CheckBox.enabled := true;
  DontScrollLine2CheckBox.checked := false;
  Line3MemoEdit.enabled := true;
  DontScrollLine3CheckBox.enabled := true;
  DontScrollLine3CheckBox.checked := false;
  Line4MemoEdit.enabled := true;

  Line1MemoEdit.color := $00A1D7A4;
  Line2MemoEdit.color := clWhite;
  Line3MemoEdit.color := clWhite;
  Line4MemoEdit.color := clWhite;
  setupbutton := 1;
  GameServerEdit.text := config.gameServer[scr, 1];

  ascreen := config.screen[scr];
  DontScrollLine1CheckBox.checked := ascreen.line[1].noscroll;
  if ascreen.line[1].contNextLine then
  begin
    ContinueLine1CheckBox.checked := true;
    DontScrollLine1CheckBox.Checked := true;
    DontScrollLine1CheckBox.enabled := false;
    Line2MemoEdit.enabled := false;
    Line2MemoEdit.color := $00BBBBFF;
  end;
  Line1MemoEdit.text := ascreen.line[1].text;
  CenterLine1CheckBox.Checked := ascreen.line[1].center;

  DontScrollLine2CheckBox.checked := ascreen.line[2].noscroll;
  if ascreen.line[2].contNextLine then
  begin
    ContinueLine2CheckBox.checked := true;
    DontScrollLine2CheckBox.Checked := true;
    DontScrollLine2CheckBox.enabled := false;
    Line3MemoEdit.enabled := false;
    Line3MemoEdit.color := $00BBBBFF;
  end;
  Line2MemoEdit.text := ascreen.line[2].text;
  CenterLine2CheckBox.Checked := ascreen.line[2].center;

  DontScrollLine3CheckBox.checked := ascreen.line[3].noscroll;
  if ascreen.line[3].contNextLine then
  begin
    ContinueLine3CheckBox.checked := true;
    DontScrollLine3CheckBox.Checked := true;
    DontScrollLine3CheckBox.enabled := false;
    Line4MemoEdit.enabled := false;
    Line4MemoEdit.color := $00BBBBFF;
  end;
  Line3MemoEdit.text := ascreen.line[3].text;
  CenterLine3CheckBox.Checked := ascreen.line[3].center;

  DontScrollLine4CheckBox.checked := ascreen.line[4].noscroll;
  Line4MemoEdit.text := ascreen.line[4].text;
  CenterLine4CheckBox.Checked := ascreen.line[4].center;

  TransitionStyleComboBox.ItemIndex := ord(ascreen.settings.TransitionStyle);
  TransitionTimeSpinEdit.Value := ascreen.settings.TransitionTime;
end;

procedure TSetupForm.ScreenSpinEditChange(Sender: TObject);
begin
  SaveScreen(CurrentScreen);

  try
    CurrentScreen := max(1,min(MaxScreens,ScreenSpinEdit.Value));
  except
    CurrentScreen := 1;
  end;

  LoadScreen(CurrentScreen);
  LCDSmartieDisplayForm.ChangeScreen(CurrentScreen);
end;

procedure TSetupForm.WinampListBoxClick(Sender: TObject);
var
  WinampStat : TWinampStat;
begin
  WinampStat := TWinampStat(WinampListBox.itemindex);
  if (WinampStat >= FirstWinampStat) and (WinampStat <= LastWinampStat) then begin
    VariableEdit.Text := WinampKeys[WinampStat];
    if (WinampStat = wsWinampPosition) then  // special case, should be resolved elsewhere
      VariableEdit.Text := VariableEdit.Text + '(10)';
  end else
    VariableEdit.Text := NoVariable;

  if not (VariableEdit.Text = NoVariable) then
    FocusToInputField();
end;


// Select currently active text field that will receive variable if 'insert'
// is pressed.
Procedure TSetupForm.FocusToInputField;
var
  tempint1, tempint2: Integer;
begin
  if (ScreensTabSheet.visible) then // in Screens tab
  begin
    // not all the lines will be enabled/visible because of different size displays.
    if (setupbutton = 2) and (Line2MemoEdit.Enabled) and (Line2MemoEdit.visible) then
    begin
      tempint1 := Line2MemoEdit.SelStart;
      tempint2 := Line2MemoEdit.SelLength;
      Line2MemoEdit.setfocus;
      Line2MemoEdit.SelStart := tempint1;
      Line2MemoEdit.SelLength := tempint2;
    end
    else if (setupbutton = 3) and (Line3MemoEdit.enabled) and (Line3MemoEdit.visible) then
    begin
      tempint1 := Line3MemoEdit.SelStart;
      tempint2 := Line3MemoEdit.SelLength;
      Line3MemoEdit.setfocus;
      Line3MemoEdit.SelStart := tempint1;
      Line3MemoEdit.SelLength := tempint2;
    end
    else if (setupbutton = 4) and (Line4MemoEdit.enabled) and (Line4MemoEdit.visible) then
    begin
      tempint1 := Line4MemoEdit.SelStart;
      tempint2 := Line4MemoEdit.SelLength;
      Line4MemoEdit.setfocus;
      Line4MemoEdit.SelStart := tempint1;
      Line4MemoEdit.SelLength := tempint2;
    end
    else if (Line1MemoEdit.Enabled) and (Line1MemoEdit.visible) then // default to line 1 of screen section
    begin // setupbutton = 1
      tempint1 := Line1MemoEdit.SelStart;
      tempint2 := Line1MemoEdit.SelLength;
      Line1MemoEdit.setfocus;
      Line1MemoEdit.SelStart := tempint1;
      Line1MemoEdit.SelLength := tempint2;
    end;
  end;
end;

procedure TSetupForm.InsertButtonClick(Sender: TObject);
var
  tempint: Integer;
begin
  if VariableEdit.Text <> NoVariable then
  begin
    if (ScreensTabSheet.visible) then // in Screens tab
    begin
      if (setupbutton = 2) and (Line2MemoEdit.enabled) and (Line2MemoEdit.visible) then
      begin
        tempint := Line2MemoEdit.SelStart;
        Line2MemoEdit.text := utf8copy(Line2MemoEdit.text, 1, Line2MemoEdit.SelStart) + VariableEdit.Text +
          utf8copy(Line2MemoEdit.text, Line2MemoEdit.SelStart + 1 + Line2MemoEdit.SelLength,
          UTF8Length(Line2MemoEdit.Text));
        Line2MemoEdit.SetFocus;
        Line2MemoEdit.selstart := tempint + utf8length(VariableEdit.text);

      end
      else if (setupbutton = 3) and (Line3MemoEdit.enabled) and (Line3MemoEdit.visible) then
      begin
        tempint := Line3MemoEdit.SelStart;
        Line3MemoEdit.text := utf8copy(Line3MemoEdit.text, 1, Line3MemoEdit.SelStart) + VariableEdit.Text +
          utf8copy(Line3MemoEdit.text, Line3MemoEdit.SelStart + 1 + Line3MemoEdit.SelLength,
          UTF8Length(Line3MemoEdit.Text));
        Line3MemoEdit.SetFocus;
        Line3MemoEdit.selstart := tempint + UTF8Length(VariableEdit.text);
      end
      else if (setupbutton = 4) and (Line4MemoEdit.enabled) and (Line4MemoEdit.visible) then
      begin
        tempint := Line4MemoEdit.SelStart;
        Line4MemoEdit.text := utf8copy(Line4MemoEdit.text, 1, Line4MemoEdit.SelStart) + VariableEdit.Text +
          utf8copy(Line4MemoEdit.text, Line4MemoEdit.SelStart + 1 + Line4MemoEdit.SelLength,
          UTF8Length(Line4MemoEdit.Text));
        Line4MemoEdit.SetFocus;
        Line4MemoEdit.selstart := tempint + UTF8Length(VariableEdit.text);
      end
      else if (Line1MemoEdit.enabled) and (Line1MemoEdit.visible) then // default to line 1
      begin
        tempint := Line1MemoEdit.SelStart;
        Line1MemoEdit.text := utf8copy(Line1MemoEdit.text, 1, tempint) + VariableEdit.Text +
          utf8copy(Line1MemoEdit.text, tempint + 1 + Line1MemoEdit.SelLength,
          UTF8Length(Line1MemoEdit.Text));
        Line1MemoEdit.SetFocus;
        Line1MemoEdit.selstart := tempint + UTF8Length(VariableEdit.text);
      end;
    end
    else if (ActionsTabSheet.Visible) then // in Actions tab
    begin
      if (LastKeyPressedEdit.text='') and (VariableEdit.text='$MObutton') then
      begin
        showmessage ('please press the button you want to bind');
      end
      else
      begin
        if pos('$MObutton', VariableEdit.Text) <> 0 then
          VariableEdit.Text := '$MObutton(' + LastKeyPressedEdit.text + ')';
         ActionsStringGrid.Cells[1, ActionsStringGrid.row] := VariableEdit.text;
      end;
    end;
  end;
end;

procedure TSetupForm.SysInfoListBoxClick(Sender: TObject);
begin
  case SysInfoListBox.itemindex of
    0 : VariableEdit.Text := '$SysUsername';
    1 : VariableEdit.Text := '$SysComputername';
    2 : VariableEdit.Text := '$CPUType';
    3 : VariableEdit.Text := '$CPUSpeed';
    4 : VariableEdit.Text := '$CPUUsage%';
    5 : VariableEdit.Text := '$Bar($CPUUsage%,100,10)';
    6 : VariableEdit.Text := '$MemFree';
    7 : VariableEdit.Text := '$MemUsed';
    8 : VariableEdit.Text := '$MemTotal';
    9 : VariableEdit.Text := '$MemF%';
    10 : VariableEdit.Text := '$MemU%';
    11 : VariableEdit.Text := '$Bar($MemFree,$MemTotal,10)';
    12 : VariableEdit.Text := '$Bar($MemUsed,$MemTotal,10)';
    13 : VariableEdit.Text := '$PageFree';
    14 : VariableEdit.Text := '$PageUsed';
    15 : VariableEdit.Text := '$PageTotal';
    16 : VariableEdit.Text := '$PageF%';
    17 : VariableEdit.Text := '$PageU%';
    18 : VariableEdit.Text := '$Bar($PageFree,$PageTotal,10)';
    19 : VariableEdit.Text := '$Bar($PageUsed,$PageTotal,10)';
    20 : VariableEdit.Text := '$HDFree(C)';
    21 : VariableEdit.Text := '$HDUsed(C)';
    22 : VariableEdit.Text := '$HDTotal(C)';
    23 : VariableEdit.Text := '$HDFreg(C)';
    24 : VariableEdit.Text := '$HDUseg(C)';
    25 : VariableEdit.Text := '$HDTotag(C)';
    26 : VariableEdit.Text := '$HDF%(C)';
    27 : VariableEdit.Text := '$HDU%(C)';
    28 : VariableEdit.Text := '$Bar($HDFree(C),$HDTotal(C),10)';
    29 : VariableEdit.Text := '$Bar($HDUsed(C),$HDTotal(C),10)';
    30 : VariableEdit.Text := '$ScreenReso';
    31 : VariableEdit.Text := '$SysSSActive';
    32 : VariableEdit.Text := '$SysFSGameActive';
    33 : VariableEdit.Text := '$SysFSAppActive';
    34 : VariableEdit.Text := '$SysAppActive(LCDSmartie.exe)';
    else VariableEdit.Text := NoVariable;
  end; // case

  if not (VariableEdit.Text = NoVariable) then
    FocusToInputField();
end;

procedure TSetupForm.InternetListBoxClick(Sender: TObject);
begin
  case InternetListBox.itemindex of
    0: VariableEdit.Text := '$Rss(https://news.bbc.co.uk/rss/newsonline_uk_edition/world/rss091.xml,b)';
    1: VariableEdit.Text := '$Rss(https://news.bbc.co.uk/rss/newsonline_uk_edition/uk/rss091.xml,b)';
    2: VariableEdit.Text := '$Rss(https://feeds.feedburner.com/tweakers/mixed,b)';
    3: VariableEdit.Text := '$Rss(https://www.theregister.com/headlines.rss,b)';
    4: VariableEdit.Text := '$Rss(http://rss.slashdot.org/Slashdot/slashdot,b)'; // only http
    5: VariableEdit.Text := '$Rss(https://www.wired.com/feed/rss,b)';
    6: VariableEdit.Text := '$Rss(https://sourceforge.net/p/lcdsmartie/news/feed,b,1)';
    7: VariableEdit.Text := '$Rss(https://sourceforge.net/p/palmorb/news/feed,b,1)';
    8: VariableEdit.Text := '$Rss(https://news.bbc.co.uk/rss/newsonline_world_edition/business/rss091.xml,b)';
    9: VariableEdit.Text := '$Rss(https://www.washingtonpost.com/wp-srv/business/rssheadlines.xml,b)';
    10: VariableEdit.Text := '$Rss(https://news.yahoo.com/rss/entertainment,b)';
    11: VariableEdit.Text := '$Rss(https://www.nytimes.com/services/xml/rss/nyt/Health.xml,b)';
    12: VariableEdit.Text := '$Rss(https://rss.nytimes.com/services/xml/rss/nyt/Sports.xml,b)';
    13: VariableEdit.Text := '$Rss(https://www.volkskrant.nl/economie/rss.xml,b)';
    14: VariableEdit.Text := '$Rss(https://rs.vpro.nl/v3/api/feeds/3voor12/section/3voor12%20Landelijk,b)';
    15: VariableEdit.Text := '$Rss(https://www.ad.nl/home/rss.xml,b)';
    else VariableEdit.Text := NoVariable;
  end; // case

  if not (VariableEdit.Text = NoVariable) then
    FocusToInputField();
end;

procedure TSetupForm.QStatLabelClick(Sender: TObject);
begin
  ShellExecute(0, Nil, pchar('www.qstat.org'), Nil, Nil, SW_NORMAL);
end;

procedure TSetupForm.MiscListBoxClick(Sender: TObject);
begin
  case MiscListBox.itemindex of
    0 : VariableEdit.Text := '$DnetSpeed';
    1 : VariableEdit.Text := '$DnetDone';
    2 : VariableEdit.Text := '$Time(d mmmm yyyy hh: nn: ss)';
    3 : VariableEdit.Text := '$UpTime';
    4 : VariableEdit.Text := '$UpTims';
    5 : VariableEdit.Text := '°';
    6 : VariableEdit.Text := '█';
    7 : VariableEdit.Text := '$Chr(20)';
    8 : VariableEdit.Text := '$File(C:\file.txt,1)';
    9 : VariableEdit.Text := '$LogFile(C:\file.log,0)';
    10 : VariableEdit.Text := '$dll(demo.dll,5,param1,param2)';
    11 : VariableEdit.Text := '$Count(101#$CPUSpeed#4)';
    12 : VariableEdit.Text := '$Bar(30,100,20)';
    13 : VariableEdit.Text := '$Right(ins variable(s) here,$3%)';
    14 : VariableEdit.Text := '$Fill(10)';
    15 : VariableEdit.Text := '$Flash(insert text here$)$';
    16 : VariableEdit.Text := '$CustomChar(1, 31, 31, 31, 31, 31, 31, 31, 31)';
    17 : VariableEdit.Text := '$Rss(URL,t|d|b,ITEM#,MAXFREQHRS)';
    18 : VariableEdit.Text := '$Center(text here,15)';
    19 : VariableEdit.Text := '$ScreenChanged';
    20 : VariableEdit.Text := '$Sender(127.0.0.10,6088,password1234,1,1)';
    else VariableEdit.Text := NoVariable;
  end; // case

  if not (VariableEdit.Text = NoVariable) then
    FocusToInputField();
end;

procedure TSetupForm.SetiAtHomeListBoxClick(Sender: TObject);
begin
  case SetiAtHomeListBox.itemindex of
    0 : VariableEdit.Text := '$SETIResults';
    1 : VariableEdit.Text := '$SETICPUTime';
    2 : VariableEdit.Text := '$SETIAverage';
    3 : VariableEdit.Text := '$SETILastresult';
    4 : VariableEdit.Text := '$SETIusertime';
    5 : VariableEdit.Text := '$SETItotalusers';
    6 : VariableEdit.Text := '$SETIrank';
    7 : VariableEdit.Text := '$SETIsharingrank';
    8 : VariableEdit.Text := '$SETImoreWU%';
    else VariableEdit.Text := NoVariable;
  end; // case

  if not (VariableEdit.Text = NoVariable) then
    FocusToInputField();
end;

procedure TSetupForm.LeftPageControlChange(Sender: TObject);
begin
  if LeftPageControl.ActivePage = WinampTabSheet then
    WinampListBoxClick(Sender);
  if LeftPageControl.ActivePage = SysInfoTabSheet then
    SysInfoListBoxClick(Sender);
  if LeftPageControl.ActivePage = LCDFeaturesTabSheet then
    ButtonsListBoxClick(Sender);
  if LeftPageControl.ActivePage = GameStatsTabSheet then
    GamestatsListBoxClick(Sender);
  if LeftPageControl.ActivePage = InternetTabSheet then
    InternetListBoxClick(Sender);
  if LeftPageControl.ActivePage = MiscTabSheet then
    MiscListBoxClick(Sender);
  if LeftPageControl.ActivePage = SetiAtHomeTabSheet then
    SetiAtHomeListBoxClick(Sender);
  if LeftPageControl.ActivePage = FoldingAtHomeTabSheet then
    FoldingAtHomeListBoxClick(Sender);
  if LeftPageControl.ActivePage = EmailTabSheet then
    EmailAccountComboBoxChange(Sender);
  if LeftPageControl.ActivePage = NetworkStatsTabSheet then
    NetworkStatsListBoxClick(Sender);
end;


procedure TSetupForm.GameServerEditExit(Sender: TObject);
begin
  if (setupbutton >= 0) and (setupbutton <= 4) then
  begin
    config.gameServer[ScreenSpinEdit.Value, setupbutton] := GameServerEdit.text;
  end;
end;

procedure TSetupForm.DistributedNetBrowseButtonClick(Sender: TObject);
var
  line, line2: String;
begin
// remove duplicate backslash
  line := DistributedNetLogfileEdit.text;
  line2 := '';
  while pos('\', line) <> 0 do
  begin
    line2 := line2 + copy(line, 1, pos('\', line));
    line := copy(line, pos('\', line) + 1, length(line));
  end;
  opendialog2.InitialDir := line2;
  opendialog2.FileName := DistributedNetLogfileEdit.text;
  Opendialog2.Execute;
  if opendialog2.FileName <> '' then DistributedNetLogfileEdit.text := opendialog2.FileName;
end;

procedure TSetupForm.EmailAccountComboBoxChange(Sender: TObject);
begin
  config.pop[CurrentlyShownEmailAccount + 1].server := EmailServerEdit.text;
  config.pop[CurrentlyShownEmailAccount + 1].user := EmailLoginEdit.text;
  config.pop[CurrentlyShownEmailAccount + 1].pword := EmailPasswordEdit.text;
  config.pop[CurrentlyShownEmailAccount + 1].port_ssl := EmailSSLEdit.text;

  if EmailAccountComboBox.itemIndex < 0 then EmailAccountComboBox.itemindex := 0;

  CurrentlyShownEmailAccount := EmailAccountComboBox.itemindex;
  EmailServerEdit.text := config.pop[CurrentlyShownEmailAccount + 1].server;
  EmailLoginEdit.text := config.pop[CurrentlyShownEmailAccount + 1].user;
  EmailPasswordEdit.text := config.pop[CurrentlyShownEmailAccount + 1].pword;
  EmailSSLEdit.text := config.pop[CurrentlyShownEmailAccount + 1].port_ssl;

  if EmailMessageCountRadioButton.Checked then
  VariableEdit.Text := EmailCountKey+IntToStr(CurrentlyShownEmailAccount+1)+EmailKeyPostfix
  else if EmailLastSubjectRadioButton.Checked then
  VariableEdit.Text := EmailSubjectKey+IntToStr(CurrentlyShownEmailAccount+1)+EmailKeyPostfix
  else if EmailLastFromRadioButton.Checked then
  VariableEdit.Text := EmailFromKey+IntToStr(CurrentlyShownEmailAccount+1)+EmailKeyPostfix;

  if not (VariableEdit.Text = NoVariable) then
    FocusToInputField();
end;

procedure TSetupForm.ContinueLine1CheckBoxClick(Sender: TObject);
var
  tempint1: Integer;

begin
  if ContinueLine1CheckBox.checked = true then
  begin
    DontScrollLine1CheckBox.Checked := true;
    DontScrollLine1CheckBox.enabled := false;
    if setupbutton = 2 then
    begin
      tempint1 := Line1MemoEdit.SelStart;
      Line1MemoEdit.setfocus;
      Line1MemoEdit.SelStart := tempint1;
    end;
    Line2MemoEdit.enabled := false;
    Line2MemoEdit.color := $00BBBBFF;
  end
  else
  begin
    DontScrollLine1CheckBox.enabled := true;
    DontScrollLine1CheckBox.checked := false;
    Line2MemoEdit.enabled := true;
    Line2MemoEdit.color := clWhite;
  end;
end;

procedure TSetupForm.ContinueLine2CheckBoxClick(Sender: TObject);
var
  tempint1: Integer;

begin
  if ContinueLine2CheckBox.checked = true then
  begin
    DontScrollLine2CheckBox.Checked := true;
    DontScrollLine2CheckBox.enabled := false;
    if setupbutton = 3 then
    begin
      tempint1 := Line1MemoEdit.SelStart;
      Line1MemoEdit.setfocus;
      Line1MemoEdit.SelStart := tempint1;
    end;
    Line3MemoEdit.enabled := false;
    Line3MemoEdit.color := $00BBBBFF;
  end
  else
  begin
    DontScrollLine2CheckBox.enabled := true;
    DontScrollLine2CheckBox.checked := false;
    Line3MemoEdit.enabled := true;
    Line3MemoEdit.color := clWhite;
  end;
end;

procedure TSetupForm.ContinueLine3CheckBoxClick(Sender: TObject);
var
  tempint1: Integer;

begin
  if ContinueLine3CheckBox.checked = true then
  begin
    DontScrollLine3CheckBox.Checked := true;
    DontScrollLine3CheckBox.enabled := false;
    if setupbutton = 4 then
    begin
      tempint1 := Line1MemoEdit.SelStart;
      Line1MemoEdit.setfocus;
      Line1MemoEdit.SelStart := tempint1;
    end;
    Line4MemoEdit.enabled := false;
    Line4MemoEdit.color := $00BBBBFF;
  end
  else
  begin
    DontScrollLine3CheckBox.enabled := true;
    DontScrollLine3CheckBox.checked := false;
    Line4MemoEdit.enabled := true;
    Line4MemoEdit.color := clWhite;
  end;
end;

procedure TSetupForm.WinampLocationBrowseButtonClick(Sender: TObject);
begin
  opendialog1.Execute;
  if opendialog1.FileName <> '' then WinampLocationEdit.text := opendialog1.FileName;
end;

procedure TSetupForm.GamestatsListBoxClick(Sender: TObject);
var
  S : string;
begin
  case GameTypeComboBox.itemindex of
    0 : S := '$Half-life';
    1 : S := '$QuakeII';
    2 : S := '$QuakeIII';
    3 : S := '$Unreal';
    else S := NoVariable;
  end; // case

  if not (S = NoVariable) then begin
    VariableEdit.Text := S + IntToStr(GamestatsListBox.Itemindex+1);
    FocusToInputField();
  end else
    VariableEdit.Text := S;
end;

procedure TSetupForm.Line1MemoEditEnter(Sender: TObject);
begin
  GameServerEdit.text := config.gameServer[ScreenSpinEdit.Value, 1];
  setupbutton := 1;
  Line1MemoEdit.color := $00A1D7A4;
  if Line2MemoEdit.enabled= true then Line2MemoEdit.color := clWhite
  else Line2MemoEdit.color := $00BBBBFF;
  if Line3MemoEdit.enabled= true then Line3MemoEdit.color := clWhite
  else Line3MemoEdit.color := $00BBBBFF;
  if Line4MemoEdit.enabled= true then Line4MemoEdit.color := clWhite
  else Line4MemoEdit.color := $00BBBBFF;
end;

procedure TSetupForm.Line2MemoEditEnter(Sender: TObject);
begin
  GameServerEdit.text := config.gameServer[ScreenSpinEdit.Value, 2];
  setupbutton := 2;
  Line2MemoEdit.color := $00A1D7A4;
  if Line1MemoEdit.enabled= true then Line1MemoEdit.color := clWhite
  else Line1MemoEdit.color := $00BBBBFF;
  if Line3MemoEdit.enabled= true then Line3MemoEdit.color := clWhite
  else Line3MemoEdit.color := $00BBBBFF;
  if Line4MemoEdit.enabled= true then Line4MemoEdit.color := clWhite
  else Line4MemoEdit.color := $00BBBBFF;
end;

procedure TSetupForm.Line3MemoEditEnter(Sender: TObject);
begin
  GameServerEdit.text := config.gameServer[ScreenSpinEdit.Value, 3];
  setupbutton := 3;
  Line3MemoEdit.color := $00A1D7A4;
  if Line2MemoEdit.enabled= true then Line2MemoEdit.color := clWhite
  else Line2MemoEdit.color := $00BBBBFF;
  if Line1MemoEdit.enabled= true then Line1MemoEdit.color := clWhite
  else Line1MemoEdit.color := $00BBBBFF;
  if Line4MemoEdit.enabled= true then Line4MemoEdit.color := clWhite
  else Line4MemoEdit.color := $00BBBBFF;
end;

procedure TSetupForm.Line4MemoEditEnter(Sender: TObject);
begin
  GameServerEdit.text := config.gameServer[ScreenSpinEdit.Value, 4];
  setupbutton := 4;
  Line4MemoEdit.color := $00A1D7A4;
  if Line2MemoEdit.enabled= true then Line2MemoEdit.color := clWhite
  else Line2MemoEdit.color := $00BBBBFF;
  if Line3MemoEdit.enabled= true then Line3MemoEdit.color := clWhite
  else Line3MemoEdit.color := $00BBBBFF;
  if Line1MemoEdit.enabled= true then Line1MemoEdit.color := clWhite
  else Line1MemoEdit.color := $00BBBBFF;
end;

procedure TSetupForm.NetworkStatsListBoxClick(Sender: TObject);
var
  NetStat : TNetworkStatistics;
begin
  NetStat := TNetworkStatistics(NetworkStatsListBox.itemindex);
  if (NetStat >= FirstNetworkStat) and (NetStat <= LastNetworkStat) then begin
    VariableEdit.Text := NetworkStatisticsKeys[NetStat]+ '(1)';
  end else
    VariableEdit.Text := NoVariable;

  if not (VariableEdit.Text = NoVariable) then
    FocusToInputField();
end;

procedure TSetupForm.FoldingAtHomeListBoxClick(Sender: TObject);
begin
  case FoldingAtHomeListBox.itemindex of
    0 : VariableEdit.Text := '$FOLDUser';
    1 : VariableEdit.Text := '$FOLDwu';
    2 : VariableEdit.Text := '$FOLDlastwu';
    3 : VariableEdit.Text := '$FOLDact50min';
    4 : VariableEdit.Text := '$FOLDactweek';
    5 : VariableEdit.Text := '$FOLDscore';
    6 : VariableEdit.Text := '$FOLDrank';
    7 : VariableEdit.Text := '$FOLDteamname';
    8 : VariableEdit.Text := '$FOLDteamscore';
    9 : VariableEdit.Text := '$FOLDteamwu';
    10 : VariableEdit.Text := '$FOLDteamlastwu';
    else VariableEdit.Text := NoVariable;
  end; // case

  if not (VariableEdit.Text = NoVariable) then
    FocusToInputField();
end;


// Apply pressed.
procedure TSetupForm.ApplyButtonClick(Sender: TObject);
var
  ReinitLCD, ReloadSkin: Boolean;
  x: Integer;
  iMaxUsedRow: Integer;
begin
  ReinitLCD := false;
  ReloadSkin := false;

  iMaxUsedRow := -1;
  for x := 0 to ActionsStringGrid.RowCount-1 do
  begin
    if (ActionsStringGrid.cells[1, x] <> '') and (ActionsStringGrid.cells[5,
      x] <> '') then
    begin
        iMaxUsedRow := x;
        config.actionsArray[x + 1, 1] := ActionsStringGrid.Cells[1, x];

        if ActionsStringGrid.Cells[2, x]='>' then
           config.actionsArray[x + 1, 2] := '0';
        if ActionsStringGrid.Cells[2, x]='<' then
           config.actionsArray[x + 1, 2] := '1';
        if ActionsStringGrid.Cells[2, x]='=' then
           config.actionsArray[x + 1, 2] := '2';
        if ActionsStringGrid.Cells[2, x]='<=' then
           config.actionsArray[x + 1, 2] := '3';
        if ActionsStringGrid.Cells[2, x]='>=' then
           config.actionsArray[x + 1, 2] := '4';
        if ActionsStringGrid.Cells[2, x]='<>' then
           config.actionsArray[x + 1, 2] := '5';

        config.actionsArray[x + 1, 3] := ActionsStringGrid.Cells[3, x];
        config.actionsArray[x + 1, 4] := ActionsStringGrid.Cells[5, x];
    end;
  end;
  config.totalactions := iMaxUsedRow + 1;

  // Check if Com settings have changed.

   if (config.DisplayDLLParameters <> ParametersEdit.Text) then ReinitLCD := true;
   if (config.DisplayDLLName <> DisplayPluginList.Text) then ReinitLCD := true;

  LCDSmartieDisplayForm.WinampCtrl1.WinampLocation := WinampLocationEdit.text;
  config.winampLocation := WinampLocationEdit.text;
  config.refreshRate := ProgramRefreshIntervalSpinEdit.Value;
  config.setiEmail := SetiAtHomeEmailEdit.text;

  config.ScreenSize := LCDSizeComboBox.itemindex + 1;
  config.randomScreens := RandomizeScreensCheckBox.checked;
  config.newsRefresh := InternetRefreshTimeSpinEdit.Value;
  config.foldUserid := FoldingAtHomeEmailEdit.text;
  config.gameRefresh := GamestatsRefreshTimeSpinEdit.Value;
  config.checkUpdates := LCDSmartieUpdateCheckBox.checked;
  config.colorOption := ColorSchemeComboBox.itemindex;
  config.distLog := DistributedNetLogfileEdit.text;
  config.dllPeriod := DLLCheckIntervalSpinEdit.value;
  config.emailPeriod := EmailCheckTimeSpinEdit.Value;
  config.scrollPeriod := ProgramScrollIntervalSpinEdit.value;
  config.alwaysOnTop := StayOnTopCheckBox.checked;
  config.bHideOnStartup := HideOnStartup.Checked;
  config.bAutoStart := AutoStart.checked;
  config.bAutoStartHide := AutoStartHide.checked;
  config.EmulateLCD := EmulateLCDCheckbox.Checked;
  config.ShutdownMessage[1] := ShutdownEdit1.Text;
  config.ShutdownMessage[2] := ShutdownEdit2.Text;
  config.ShutdownMessage[3] := ShutdownEdit3.Text;
  config.ShutdownMessage[4] := ShutdownEdit4.Text;

  config.DLL_contrast := ContrastTrackBar.position;
  config.DLL_brightness := BrightnessTrackBar.position;
  config.DisplayDLLParameters := ParametersEdit.Text;
  config.DisplayDLLName := DisplayPluginList.Text;

  if (config.DisplayDLLName = 'None') then
    config.DisplayDLLName := '';

  LCDSmartieDisplayForm.SetupAutoStart();

  // Check if Com settings have changed.
  config.pop[(EmailAccountComboBox.itemindex + 1) mod MaxEmailAccounts].server := EmailServerEdit.text;
  config.pop[(EmailAccountComboBox.itemindex + 1) mod MaxEmailAccounts].user := EmailLoginEdit.text;
  config.pop[(EmailAccountComboBox.itemindex + 1) mod MaxEmailAccounts].pword := EmailPasswordEdit.text;
  config.pop[(EmailAccountComboBox.itemindex + 1) mod MaxEmailAccounts].port_ssl := EmailSSLEdit.text;



  if (WebProxyPortEdit.text = '') then WebProxyPortEdit.text := '0';
  config.httpProxy := WebProxyServerEdit.text;
  config.httpProxyPort := StrToInt(WebProxyPortEdit.text);

  SaveScreen(ScreenSpinEdit.Value);
  LCDSmartieDisplayForm.ScrollFlashTimer.interval := config.scrollPeriod;
  LCDSmartieDisplayForm.Data.RefreshDataThreads;

  config.LastTabIndex := LeftPageControl.ActivePageIndex;
  if config.sSkinPath <> SkinPath.Text then ReloadSkin := true;
  if config.sTrayIcon <> TrayIcon.Text then ReloadSkin := true;
  config.sSkinPath :=  SkinPath.Text;
  config.sTrayIcon := TrayIcon.Text;
  if ReloadSkin then
  begin
    LCDSmartieDisplayForm.LoadSkin();
    LCDSmartieDisplayForm.LoadColors();
  end;

  config.EnableRemoteSend := EnableRemoteSendCheckBox.Checked;
  config.RemoteSendBindIP := RemoteSendBindIPEdit.Text;
  config.RemoteSendPort := RemoteSendPortEdit.Text;
  config.RemoteSendPassword := RemoteSendPasswordEdit.Text;
  config.RemoteSendUseSSL := RemoteSendUseSSLCheckBox.Checked;

  config.SettingsFormPosTop := SetupForm.Top;
  config.SettingsFormPosLeft := SetupForm.Left;
  config.ActionsTimer := ActionsTimerSpinEdit.Value;
  config.save();

  if ReinitLCD = true then
 begin
    LCDSmartieDisplayForm.ReInitLCD();
  end;

  LCDSmartieDisplayForm.ShowTrueLCD := Config.EmulateLCD;
end;

// ok has been pressed.
procedure TSetupForm.OKButtonClick(Sender: TObject);
begin
  ApplyButtonClick(Sender);
end;

procedure TSetupForm.FormCreate(Sender: TObject);
var
  pathssl :string;
begin
  pathssl := ExtractFilePath(ParamStr(0))+'openssl\';

 // check if ssl dll exists , if not block the ssl edit !!!
  if not fileExists(pathssl+'libeay32.dll') or
     not fileExists(pathssl+'ssleay32.dll') then EmailSSLEdit.Enabled :=False ;

  //point PluginListBox to the plugin dirs
  PluginListBox.Directory := ExtractFilePath(ParamStr(0))+'plugins\';
end;

procedure TSetupForm.MainPageControlChange(Sender: TObject);
begin

  if MainPageControl.ActivePage = ScreensTabSheet then
  begin
    if LeftPageControl.activepage = LCDFeaturesTabSheet then
    begin
      if pos('$MObutton', VariableEdit.text) <> 0 then VariableEdit.text := NoVariable;
      LeftPageControl.ActivePage := WinampTabSheet;
    end;
    GameServerEdit.text := config.gameServer[ScreenSpinEdit.Value, 1];
    setupbutton := 1;
    Line1MemoEdit.color := $00A1D7A4;
    if Line2MemoEdit.enabled= true then Line2MemoEdit.color := clWhite
    else Line2MemoEdit.color := $00BBBBFF;
    if Line3MemoEdit.enabled= true then Line3MemoEdit.color := clWhite
    else Line3MemoEdit.color := $00BBBBFF;
    if Line4MemoEdit.enabled= true then Line4MemoEdit.color := clWhite
    else Line4MemoEdit.color := $00BBBBFF;
  end;
end;

procedure TSetupForm.ActionAddButtonClick(Sender: TObject);
begin
  ActionsStringGrid.RowCount := ActionsStringGrid.RowCount+1;
  ActionsStringGrid.Cells[0, ActionsStringGrid.RowCount-1] := 'if';
  ActionsStringGrid.Cells[4, ActionsStringGrid.RowCount-1] := 'then';
end;

procedure TSetupForm.ActionDeleteButtonClick(Sender: TObject);
var
  counter2: Integer;
begin
    counter2 := ActionsStringGrid.Selection.Top;
    ActionsStringGrid.DeleteRow(counter2);
end;

procedure TSetupForm.ButtonsListBoxClick(Sender: TObject);
begin
  case ButtonsListBox.itemindex of
    0 : VariableEdit.Text := '$MObutton';
    1 : VariableEdit.Text := '$FanSpeed(1,1)';
    2 : VariableEdit.Text := '$Sensor1';
    3 : VariableEdit.Text := '$Sensor2';
    4 : VariableEdit.Text := '$Sensor3';
    5 : VariableEdit.Text := '$Sensor4';
    6 : VariableEdit.Text := '$Sensor5';
    7 : VariableEdit.Text := '$Sensor6';
    8 : VariableEdit.Text := '$Sensor7';
    9 : VariableEdit.Text := '$Sensor8';
    else VariableEdit.Text := NoVariable;
  end; // case

  if not (VariableEdit.Text = NoVariable) then
    FocusToInputField();
end;

procedure TSetupForm.Line1MemoEditKeyDown(Sender: TObject; var Key: Word; Shift:
  TShiftState);
begin
  if ord(key) = VK_UP then
  begin
    // select bottom row
    if (Line4MemoEdit.Enabled) and (Line4MemoEdit.Visible) then Line4MemoEdit.SetFocus
    else if (Line3MemoEdit.Enabled) and (Line3MemoEdit.Visible) then Line3MemoEdit.SetFocus
    else if (Line2MemoEdit.Enabled) and (Line2MemoEdit.Visible) then Line2MemoEdit.SetFocus;
  end
  else if ord(key) = VK_DOWN then
  begin
    // Select next row if used.
    if (Line2MemoEdit.Enabled) and (Line2MemoEdit.Visible) then Line2MemoEdit.SetFocus;
  end;
end;

procedure TSetupForm.Line2MemoEditKeyDown(Sender: TObject; var Key: Word; Shift:
  TShiftState);
begin
  if ord(key) = VK_UP then Line1MemoEdit.SetFocus
  else if ord(key) = VK_DOWN then
  begin
    // Select next row if used otherwise go to top.
    if (Line3MemoEdit.Enabled) and (Line3MemoEdit.Visible) then Line3MemoEdit.SetFocus
    else Line1MemoEdit.SetFocus;
  end;
end;

procedure TSetupForm.Line3MemoEditKeyDown(Sender: TObject; var Key: Word; Shift:
  TShiftState);
begin
  if ord(key) = VK_UP then Line2MemoEdit.SetFocus
  else if ord(key) = VK_DOWN then
  begin
    // Select next row if used otherwise go to top.
    if (Line4MemoEdit.Enabled) and (Line4MemoEdit.Visible) then Line4MemoEdit.SetFocus
    else Line1MemoEdit.SetFocus;
  end;
end;

procedure TSetupForm.Line4MemoEditKeyDown(Sender: TObject; var Key: Word; Shift:
  TShiftState);
begin
  if ord(key) = VK_UP then Line3MemoEdit.SetFocus;
  if ord(key) = VK_DOWN then Line1MemoEdit.SetFocus;
end;

procedure TSetupForm.StickyCheckboxClick(Sender: TObject);
begin
  TimeToShowSpinEdit.enabled := not StickyCheckbox.checked;
end;

procedure TSetupForm.ColorSchemeComboBoxChange(Sender: TObject);
begin
  if ColorSchemeComboBox.ItemIndex < 0 then ColorSchemeComboBox.ItemIndex := 0;
end;

procedure UpdateSetupForm(cKey : char);
begin
  if assigned(SetupForm) then
    SetupForm.LastKeyPressedEdit.text := cKey;
end;

procedure TSetupForm.ContrastTrackBarChange(Sender: TObject);
begin
  LCDSmartieDisplayForm.lcd.setContrast(ContrastTrackBar.position);
end;

procedure TSetupForm.BrightnessTrackBarChange(Sender: TObject);
begin
  LCDSmartieDisplayForm.lcd.setBrightness(BrightnessTrackBar.position);
end;

 /////// SHUTDOWN MESSAGE EDIT ///////////////

procedure TSetupForm.ShutdownEditEnter(Sender: TObject);
var
  oEdit: TEdit;
begin

  oEdit := Sender As TEdit;
  oEdit.Color := $00A1D7A4;

  if (ShutdownEdit1 <> Sender) And ShutdownEdit1.Enabled = true then
    ShutdownEdit1.color := clWhite
  else
    ShutdownEdit1.color := $00BBBBFF;

  if (ShutdownEdit2 <> Sender) And ShutdownEdit2.enabled = true then
    ShutdownEdit2.color := clWhite
  else
    ShutdownEdit2.color := $00BBBBFF;

  if (ShutdownEdit3 <> Sender) And ShutdownEdit3.enabled = true then
    ShutdownEdit3.color := clWhite
  else
    ShutdownEdit3.color := $00BBBBFF;

  if (ShutdownEdit4 <> Sender) And ShutdownEdit4.enabled = true then
    ShutdownEdit4.color := clWhite
  else
    ShutdownEdit4.color := $00BBBBFF;
end;

/////////////////////////////////////////////////////////////////
//////////////////////////// KEYS ///////////////////////////////
/////////////////////////////////////////////////////////////////
procedure TSetupForm.ShutdownEdit1KeyDown(Sender: TObject; var Key: Word; Shift:
  TShiftState);
begin
  if ord(key) = VK_UP then
  begin
    // select bottom row
    if (ShutdownEdit4.Enabled) and (ShutdownEdit4.Visible) then ShutdownEdit4.SetFocus
    else if (ShutdownEdit3.Enabled) and (ShutdownEdit3.Visible) then ShutdownEdit3.SetFocus
    else if (ShutdownEdit2.Enabled) and (ShutdownEdit2.Visible) then ShutdownEdit2.SetFocus;
  end
  else if ord(key) = VK_DOWN then
  begin
    // Select next row if used.
    if (ShutdownEdit2.Enabled) and (ShutdownEdit2.Visible) then ShutdownEdit2.SetFocus;
  end;
end;

procedure TSetupForm.ShutdownEdit2KeyDown(Sender: TObject; var Key: Word; Shift:
  TShiftState);
begin
  if ord(key) = VK_UP then ShutdownEdit1.SetFocus
  else if ord(key) = VK_DOWN then
  begin
    // Select next row if used otherwise go to top.
    if (ShutdownEdit3.Enabled) and (ShutdownEdit3.Visible) then ShutdownEdit3.SetFocus
    else ShutdownEdit1.SetFocus;
  end;
end;

procedure TSetupForm.ShutdownEdit3KeyDown(Sender: TObject; var Key: Word; Shift:
  TShiftState);
begin
  if ord(key) = VK_UP then ShutdownEdit2.SetFocus
  else if ord(key) = VK_DOWN then
  begin
    // Select next row if used otherwise go to top.
    if (ShutdownEdit4.Enabled) and (ShutdownEdit4.Visible) then ShutdownEdit4.SetFocus
    else ShutdownEdit1.SetFocus;
  end;
end;

procedure TSetupForm.ShutdownEdit4KeyDown(Sender: TObject; var Key: Word; Shift:
  TShiftState);
begin
  if ord(key) = VK_UP then ShutdownEdit3.SetFocus;
  if ord(key) = VK_DOWN then ShutdownEdit1.SetFocus;
end;

/////////////////////////////////////////////////////////////////
//////////////////// PLUGIN LIST BOX ////////////////////////////
/////////////////////////////////////////////////////////////////
procedure TSetupForm.Btn_PluginRefreshClick(Sender: TObject);
var
  sCurrentDir : string;
begin
  // awkward shi as refresh doesnt just refresh the list
  sCurrentDir := PluginListBox.Directory;
  PluginListBox.Directory := '.';
  PluginListBox.Directory := sCurrentDir;
  PluginListBox.Refresh;
end;

procedure TSetupForm.PluginListBoxDblClick(Sender: TObject);
var
  plugin_name :string;
begin
  plugin_name := Lowercase(ExtractFileName(PluginListBox.FileName));
  plugin_name := copy(plugin_name,0,Length(plugin_name)-4)+'.txt';
  if FileExists(ExtractFilePath(ParamStr(0))+'plugins\'+plugin_name) then
    ShellExecute(0, Nil, pchar(plugin_name), Nil, Nil, SW_NORMAL)
  else
    ShowMessage('File : '+plugin_name+' does not exist');
end;


procedure TSetupForm.PluginListBoxClick(Sender: TObject);
begin
  VariableEdit.text := '$dll('+Lowercase(ExtractFileName(PluginListBox.FileName))+',1,0,0)';
end;

/////////////////////////////////////////////////////////////////
////////////////////////// ICON PATH ////////////////////////////
/////////////////////////////////////////////////////////////////
procedure TSetupForm.TrayIconBrowseButtonClick(Sender: TObject);
var
  bEnd: bool;
  s : string;
begin
  bEnd:=False;
  repeat
  begin
    OpenIco.InitialDir := ExtractFilePath(application.exename) + SkinPath.Text;
    OpenIco.FileName := TrayIcon.Text;

    if OpenIco.Execute() then
      begin
      s := ExtractFilePath(OpenIco.FileName);
      if s = OpenIco.InitialDir then
        bEnd := True
      else
        ShowMessage('Error'+ sLineBreak +
                    'Tray Icon can be only in the current selected Skin path')
      end
    else
      bEnd:=True;
  end
  until bEnd;

  TrayIcon.Text := extractfilename(OpenIco.FileName);
  DrawPreviewIcons(TrayIcon.Text);
end;

procedure TSetupForm.OpeIcoFolderChange(Sender: TObject);
begin
  OpenIco.InitialDir := ExtractFilePath(application.exename) + SkinPath.Text;
end;


procedure TSetupForm.DrawPreviewIcons(const sIconFileName: String);
var
hIcon: tIcon;
IconPathName: string;
begin
  hIcon := TIcon.Create;
  IconPathName := extractfilepath(application.exename) + SkinPath.Text + sIconFileName;
  try
    GetIconFromFile(IconPathName, hIcon, SHIL_SMALL);
    TrayIconPreview16.Picture.Icon.Assign(hIcon);

    GetIconFromFile(IconPathName, hIcon, SHIL_LARGE);
    TrayIconPreview32.Picture.Icon.Assign(hIcon);
  except
    on E: Exception do
    begin
      showmessage('Error' + sLineBreak + 'Unable to load Tray Icon from Skin path, ' +
        SkinPath.Text + sIconFileName + ': ' + E.Message);
    end;
  end;
  hIcon.Destroy;
end;

Function MyFileExists(const FileName: string): Boolean;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then
        Result:=true
      else
        Result:=false;
    end
  else Result:=false;
end;

/////////////////////////////////////////////////////////////////
////////////////////////// SKIN PATH ////////////////////////////
/////////////////////////////////////////////////////////////////
procedure TSetupForm.SkinPathBrowseButtonClick(Sender: TObject);
var
  b,f,s,x: string;
begin
  b := ExtractFilePath(application.exename);
  f:= '\images';
  if SelectDirectory('Select a skin directory', b, f)
  then
    begin
      x := f + '\colors.cfg';
      if MyFileExists(x) then
        begin
          s := ExtractRelativePath(b, f);
          SkinPath.Text := s + '\';
        end
      else
        ShowMessage('Selected directory does not contain a valid skin');
    end;
end;

/////////////////////////////////////////////////////////////////
////////////// LINE EDIT FORM BUTTONS ///////////////////////////
/////////////////////////////////////////////////////////////////

procedure TSetupForm.LineEditClick(Sender: TObject);
begin
  FormEdit := tFormEdit.Create(self);

  if Sender = Line1EditButton then FormEdit.Memo1.Text := Line1MemoEdit.Text;
  if Sender = Line2EditButton then FormEdit.Memo1.Text := Line2MemoEdit.Text;
  if Sender = Line3EditButton then FormEdit.Memo1.Text := Line3MemoEdit.Text;
  if Sender = Line4EditButton then FormEdit.Memo1.Text := Line4MemoEdit.Text;

  FormEdit.Top := config.EditFormPosTop;
  FormEdit.Left := config.EditFormPosLeft;

  with FormEdit do begin
    showmodal;

   if ModalResult = mrOK then
   begin
     config.EditFormPosTop := FormEdit.Top;
     config.EditFormPosLeft := FormEdit.Left;

     if Sender = Line1EditButton then Line1MemoEdit.Text := FormEdit.Memo1.Text;
     if Sender = Line2EditButton then Line2MemoEdit.Text := FormEdit.Memo1.Text;
     if Sender = Line3EditButton then Line3MemoEdit.Text := FormEdit.Memo1.Text;
     if Sender = Line4EditButton then Line4MemoEdit.Text := FormEdit.Memo1.Text;
   end;
  end;

end;

/////////////////////////////////////////////////////////////////
////////////// CUSTOM CHAR EDIT TAB /////////////////////////////
/////////////////////////////////////////////////////////////////

function GetAllCheckboxes(_frm: TForm): TCheckBoxArray;
var
  i: Integer;
  cmp: TComponent;
begin
  SetLength(Result, _frm.ComponentCount);
  i := 1;
  repeat
    cmp := _frm.FindComponent('CCharCheckBox' + IntToStr(i));
    if cmp <> nil then begin
      Result[i - 1] := cmp as TCheckBox;
      Inc(i);
    end;
  until cmp = nil;
  SetLength(Result, i - 1);
end;

procedure TSetupForm.CCharEditGridChange(Sender: TObject);
type
  TLineArray = array [1..8] of integer;
var
  CheckBoxes: TCheckBoxArray;
  i: integer;
  box: integer;
  CCharLine: TLineArray;
const
  CCharLocation: TLineArray = (176, 158, 131, 132, 133, 134, 135, 136);
begin

  box := 0;
  CheckBoxes := GetAllCheckboxes(SetupForm);

  if CreateCCharRadioButton.Checked then
  begin
    CreateCCharLocSpinEdit.enabled := true;
    UseCCharLocSpinEdit.enabled := false;

    for i:=0 to 39 do
    begin
      CheckBoxes[i].Enabled := true;
    end;

    i := 1;
    while box<40 do
    begin
      CCharLine[i] := 0 ;
      if CheckBoxes[box].Checked then
      begin
        CCharLine[i] := CCharLine[i]+16;
      end;
      Inc(box);
      if CheckBoxes[box].Checked then
      begin
        CCharLine[i] := CCharLine[i]+8;
      end;
      Inc(box);
      if CheckBoxes[box].Checked then
      begin
        CCharLine[i] := CCharLine[i]+4;
      end;
      Inc(box);
      if CheckBoxes[box].Checked then
      begin
        CCharLine[i] := CCharLine[i]+2;
      end;
      Inc(box);
      if CheckBoxes[box].Checked then
      begin
        CCharLine[i] := CCharLine[i]+1;
      end;
      Inc(box);
      Inc(i);
    end;

    VariableEdit.Text := '$CustomChar('+IntToStr(CreateCCharLocSpinEdit.Value)
    +','+IntToStr(CCharLine[1])+','+IntToStr(CCharLine[2])+','+IntToStr(CCharLine[3])
    +','+IntToStr(CCharLine[4])+','+IntToStr(CCharLine[5])+','+IntToStr(CCharLine[6])
    +','+IntToStr(CCharLine[7])+','+IntToStr(CCharLine[8])+')';
    end
    else
    begin
      CreateCCharLocSpinEdit.enabled := false;
      UseCCharLocSpinEdit.enabled := true;

      for i:=0 to 39 do
      begin
        CheckBoxes[i].Enabled := false;
      end;
      VariableEdit.Text := '$Chr('+inttostr(CCharLocation[UseCCharLocSpinEdit.value])+')'
    end;
    checkboxes := nil;
end;

// some code taken from UDataNetwork.pas to list interface numbers
// very handy as my machine has over 40 interfaces
procedure TSetupForm.NetworkStatsAdapterListButtonClick(Sender: TObject);
var
  Size: ULONG;
  IntfTable: PMibIfTable;
  MaxEntries: Cardinal;
  Names: String;
  i: Integer;
begin
  Size := 0;
  if GetIfTable(nil, Size, True) <> ERROR_INSUFFICIENT_BUFFER then  Exit;
  if (Size < sizeof( TMibIftable)) then Exit;
  IntfTable := AllocMem(Size);
  if (IntfTable <> nil) and (GetIfTable(IntfTable, Size, True) = NO_ERROR) then
  begin
    MaxEntries := min(IntfTable^.dwNumEntries,MAXNETSTATS);
    for i:=0 to MaxEntries-1 do
    begin
    {$R-}Names := Names+IntToStr(i)+' '+PChar(@IntfTable.Table[i].bDescr[0])+#13#10;{$R+}
    end;
    ShowMessage(Names);
  end;
end;

////////////////////////////////////////////////////////
//               RE-ARRANGING SCREENS                 //
////////////////////////////////////////////////////////
procedure TSetupForm.CopyToScreenButtonClick(Sender: TObject);
begin
  if ScreenSpinEdit.value = CopyToScreenComboBox.ItemIndex+1 then
  begin
    showmessage('Destination screen is the same as this screen'+#13#10+'Choose another destination');
    exit;
  end;
  config.screen[CopyToScreenComboBox.itemindex+1] := config.screen[screenspinedit.value];
end;

procedure TSetupForm.MoveToScreenButtonClick(Sender: TObject);
begin
  if ScreenSpinEdit.value = MoveToScreenComboBox.ItemIndex+1 then
  begin
    showmessage('Destination screen is the same as this screen'+#13#10+'Choose another destination');
    exit;
  end;
  config.screen[MoveToScreenComboBox.ItemIndex+1] := config.screen[ScreenSpinEdit.value];
  config.screen[ScreenSpinEdit.value] := Default(TScreen);
  LoadScreen(ScreenSpinEdit.value);
end;

procedure TSetupForm.SwapWithScreenButtonClick(Sender: TObject);
var
  TempScreen: Tscreen;
begin
  if ScreenSpinEdit.value = SwapWithScreenComboBox.ItemIndex+1 then
  begin
    showmessage('Destination screen is the same as this screen'+#13#10+'Choose another destination');
    exit;
  end;
  TempScreen := config.screen[ScreenSpinEdit.value];
  config.screen[ScreenSpinEdit.value] := config.screen[SwapWithScreenComboBox.ItemIndex+1];
  config.screen[SwapWithScreenComboBox.ItemIndex+1] :=  TempScreen;
  LoadScreen(ScreenSpinEdit.value);
end;

////////////////////////////////////////////////////////
//               REMOTE SMARTIE SSL                   //
////////////////////////////////////////////////////////
procedure TSetupForm.RemoteSendUseSSLCheckBoxClick(Sender: TObject);
begin
  if RemoteSendUseSSLCheckBox.Checked then
    if fileExists(ExtractFilePath(application.exename)+'openssl\cert.pem') and
       fileExists(ExtractFilePath(application.exename)+'openssl\key.pem') then
       config.RemoteSendUseSSL := true
    else
    begin
       config.RemoteSendUseSSL := false;
       RemoteSendUseSSLCheckBox.Checked := false;
       showmessage('Generate SSL certificate first');
    end
  else
  begin
    RemoteSendUseSSLCheckBox.Checked := false;
    config.RemoteSendUseSSL := false;
  end;
end;

procedure TSetupForm.RemoteSendGenerateCertKeyButtonClick(Sender: TObject);
var
  OSSLDirname: string;
begin
  OSSLDirname := ExtractFilePath(application.exename)+'openssl\';

  if fileExists(OSSLDirname+'openssl.exe') then
    ShellExecute(Handle, 'open', pchar(OSSLDirname+'openssl.exe'), 'req -x509 -nodes -days 365 -newkey rsa:1024 -keyout key.pem -out cert.pem -subj "/C=LC/ST=DSM/L=ART/O=Mon organisation/CN=IE" -config openssl.cfg',
     pchar(OSSLDirname), SW_SHOWNORMAL)
  else
    showmessage('openssl\openssl.exe is missing');
end;


////////////// FOLDING/SETI ENABLE/DISABLE /////////////////
procedure TSetupForm.FoldEnableCheckBoxClick(Sender: TObject);
begin
  config.foldEnabled := FoldEnableCheckBox.Checked;
end;

procedure TSetupForm.SetiEnableCheckBoxClick(Sender: TObject);
begin
  config.setiEnabled := SetiEnableCheckBox.Checked;
end;

end.
