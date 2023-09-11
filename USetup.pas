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
interface

uses
  Commctrl,
  Dialogs, Grids, StdCtrls, Controls, Spin, Buttons, ComCtrls, Classes,
  Forms, ExtCtrls, FileCtrl,
  ExtDlgs, CheckLst, Menus, SpinEx, RTTICtrls, Process, FileUtil,
  Windows, Types, UConfig, UEditLine, LazFileUtils, Clipbrd, UxTheme;

const
  NoVariable = 'Variable:';
  PERF_DETAIL_WIZARD = $400;
  PDH_MORE_DATA  = $FFFFFFFF800007D2;

{ TSetupForm }
type
  TProcessEntry = record
    WindowTitle: string;
    Pid: integer;
  end;

type
  TProcessList = array of TProcessEntry;

type
  TCheckBoxArray = array of TCheckBox; // for custom character editor

type
  TSetupForm = class(TForm)
    ActionAddButton: TButton;
    ActionDeleteButton: TButton;
    ActionsStringGrid: TStringGrid;
    ActionsTabSheet: TTabSheet;
    ActionsTimerSpinEdit: TSpinEdit;
    AppendConfigNameCheckBox: TCheckBox;
    AutoStart: TRadioButton;
    AutoStartHide: TRadioButton;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BacklightBitBtn: TBitBtn;
    BoincServerIndexComboBox: TComboBox;
    AddRSSButton: TButton;
    ActionLogButton: TButton;
    BrightnessTrackBar: TTrackBar;
    Button1: TButton;
    Label80: TLabel;
    OneBySixteenLCDFixupCheckBox: TCheckBox;
    GroupBox1: TGroupBox;
    Line1ClearButton: TButton;
    CenterLine1CheckBox: TCheckBox;
    CenterLine2CheckBox: TCheckBox;
    CenterLine3CheckBox: TCheckBox;
    CenterLine4CheckBox: TCheckBox;
    CenterLine5CheckBox: TCheckBox;
    CenterLine6CheckBox: TCheckBox;
    CenterLine7CheckBox: TCheckBox;
    CenterLine8CheckBox: TCheckBox;
    ComPortsButton: TButton;
    ContrastTrackBar: TTrackBar;
    Label78: TLabel;
    Label79: TLabel;
    Line2ClearButton: TButton;
    Line3ClearButton: TButton;
    Line4ClearButton: TButton;
    Line5ClearButton: TButton;
    Line6ClearButton: TButton;
    Line7ClearButton: TButton;
    Line8ClearButton: TButton;
    ShowNetStatsCheckBox: TCheckBox;
    ShowGameStatsCheckBox: TCheckBox;
    ShowBOINCCheckBox: TCheckBox;
    ShowFoldingAtHomeCheckBox: TCheckBox;
    ShowEmailCheckBox: TCheckBox;
    ShowMiscCheckBox: TCheckBox;
    ShowLCDFeaturesCheckBox: TCheckBox;
    ShowPluginsCheckBox: TCheckBox;
    ShowCharEditorCheckBox: TCheckBox;
    ShowPerfCheckBox: TCheckBox;
    ShowSysinfoCheckBox: TCheckBox;
    ShowWinampCheckBox: TCheckBox;
    ShowRssCheckBox: TCheckBox;
    ColorSchemeComboBox: TComboBox;
    LeftTabsPositionComboBox: TComboBox;
    ContinueLine1CheckBox: TCheckBox;
    ContinueLine2CheckBox: TCheckBox;
    ContinueLine3CheckBox: TCheckBox;
    ContinueLine4CheckBox: TCheckBox;
    ContinueLine5CheckBox: TCheckBox;
    ContinueLine6CheckBox: TCheckBox;
    ContinueLine7CheckBox: TCheckBox;
    ContinueLine8CheckBox: TCheckBox;
    ContinueNextLineGroupBox: TGroupBox;
    CopyToScreenButton: TButton;
    CopyToScreenSpinEdit: TSpinEdit;
    CustomCharsSizeEdit: TSpinEdit;
    CustomLinesSizeEdit: TSpinEdit;
    CustomTitleTIEdit1: TTIEdit;
    DisplayPluginList: TComboBox;
    DisplayPluginsLabel: TLabel;
    DistributedNetBrowseButton: TSpeedButton;
    DistributedNetLogfileEdit: TEdit;
    DLLCheckIntervalSpinEdit: TSpinEdit;
    DontScrollGroupBox: TGroupBox;
    DontScrollLine1CheckBox: TCheckBox;
    DontScrollLine2CheckBox: TCheckBox;
    DontScrollLine3CheckBox: TCheckBox;
    DontScrollLine4CheckBox: TCheckBox;
    DontScrollLine5CheckBox: TCheckBox;
    DontScrollLine6CheckBox: TCheckBox;
    DontScrollLine7CheckBox: TCheckBox;
    DontScrollLine8CheckBox: TCheckBox;
    EmulateLCDCheckbox: TCheckBox;
    ExportLinesButton: TButton;
    GroupBox11: TGroupBox;
    GroupBox12: TGroupBox;
    GroupBox13: TGroupBox;
    GroupBox14: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox9: TGroupBox;
    IconSelectComboBox: TComboBox;
    IDLabel: TLabel;
    ImportLinesButton: TButton;
    Label1: TLabel;
    Label10: TLabel;
    Label14: TLabel;
    Label18: TLabel;
    Label3: TLabel;
    Label34: TLabel;
    Label4: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label5: TLabel;
    Label51: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label6: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label7: TLabel;
    Label71: TLabel;
    Label77: TLabel;
    Label8: TLabel;
    ExportMenuItem: TMenuItem;
    ExportClipboard: TMenuItem;
    ExportFile: TMenuItem;
    ImportMenuItem: TMenuItem;
    ImportClipboard: TMenuItem;
    ImportFile: TMenuItem;
    ActionsExportImportPoup: TPopupMenu;
    Label9: TLabel;
    LCDSizeComboBox: TComboBox;
    Line1EditButton: TSpeedButton;
    Line1MemoEdit: TEdit;
    Line2EditButton: TSpeedButton;
    Line2MemoEdit: TEdit;
    Line3EditButton: TSpeedButton;
    Line3MemoEdit: TEdit;
    Line4EditButton: TSpeedButton;
    Line4MemoEdit: TEdit;
    Line5EditButton: TSpeedButton;
    Line5MemoEdit: TEdit;
    Line6EditButton: TSpeedButton;
    Line6MemoEdit: TEdit;
    Line7EditButton: TSpeedButton;
    Line7MemoEdit: TEdit;
    Line8EditButton: TSpeedButton;
    Line8MemoEdit: TEdit;
    MoveToScreenButton: TButton;
    MoveToScreenSpinEdit: TSpinEdit;
    ParametersEdit: TEdit;
    ProgramRefreshIntervalSpinEdit: TSpinEdit;
    ProgramScrollIntervalSpinEdit: TSpinEdit;
    ProgramSettingsGroupBox: TGroupBox;
    RandomizeScreensCheckBox: TCheckBox;
    ScreenEnabledCheckBox: TCheckBox;
    ScreenLabel: TLabel;
    ScreenSpinEdit: TSpinEdit;
    SkinSelectComboBox: TComboBox;
    GroupBox2: TGroupBox;
    Label76: TLabel;
    OpenPluginFolderButton: TButton;
    CustomCharUseButton: TButton;
    CustomCharCreateButton: TButton;
    CustomCharInvertButton: TButton;
    CustomCharClearButton: TButton;
    DuplicateActionButton: TButton;
    HidePluginMenuItem: TMenuItem;
    Label20: TLabel;
    Label21: TLabel;
    Label75: TLabel;
    ExportPopupMenu: TPopupMenu;
    ImportPopupMenu: TPopupMenu;
    ExportClipboardMenuItem: TMenuItem;
    ExportFileMenuItem: TMenuItem;
    ImportClipboardMenuItem: TMenuItem;
    ImportFileMenuItem: TMenuItem;
    ImportFileOpenDialog: TOpenDialog;
    ExportFileSaveDialog: TSaveDialog;
    CustomCharGridMirrorSpeedButton: TSpeedButton;
    CChar1SpeedButton: TSpeedButton;
    CChar2SpeedButton: TSpeedButton;
    CChar3SpeedButton: TSpeedButton;
    CChar4SpeedButton: TSpeedButton;
    CChar5SpeedButton: TSpeedButton;
    CChar6SpeedButton: TSpeedButton;
    CChar7SpeedButton: TSpeedButton;
    CChar8SpeedButton: TSpeedButton;
    StayOnTopCheckBox: TCheckBox;
    StickyCheckbox: TCheckBox;
    SwapWithScreenButton: TButton;
    SwapWithScreenSpinEdit: TSpinEdit;
    TabSheet2: TTabSheet;
    ThemeNumberSpinEdit: TSpinEdit;
    TimeToShowSpinEdit: TSpinEdit;
    TransitionStyleComboBox: TComboBox;
    TransitionTimeSpinEdit: TSpinEdit;
    TrayIconPreview16: TImage;
    TrayIconPreview32: TImage;
    UnHidePluginMenuItem: TMenuItem;
    ShowHiddenPluginsMenuItem: TMenuItem;
    PluginsListBoxPopupMenu: TPopupMenu;
    UpdateRSSButton: TButton;
    DeleteRSSButton: TButton;
    RSSNameEdit: TEdit;
    Label41: TLabel;
    Label72: TLabel;
    Label74: TLabel;
    PluginVersionLabel: TLabel;
    Label73: TLabel;
    PluginDeveloperLabel: TLabel;
    PluginDemoListBox: TListBox;
    ScrollBox2: TScrollBox;
    ShutdownEdit1: TMemo;
    ShutdownEdit2: TMemo;
    ShutdownEdit3: TMemo;
    ShutdownEdit4: TMemo;
    ShutdownEdit5: TMemo;
    ShutdownEdit6: TMemo;
    ShutdownEdit7: TMemo;
    ShutdownEdit8: TMemo;
    Splitter1: TSplitter;
    UsageLabel: TLabel;
    WebProxyPortEdit: TEdit;
    WebProxyServerEdit: TEdit;
    WindowsVersionLabel: TLabel;
    pdhRefreshButton: TButton;
    Button2: TButton;
    GroupBox6: TGroupBox;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label67: TLabel;
    Label68: TLabel;
    Label69: TLabel;
    Label70: TLabel;
    RunTimeLabel: TLabel;
    ScalingComboBox: TComboBox;
    CountersComboBox: TComboBox;
    InstancesComboBox: TComboBox;
    FormatComboBox: TComboBox;
    PerfSettingsIndexComboBox: TComboBox;
    PerfCountersListBox: TListBox;
    PerfTabSheet: TTabSheet;
    InfoTimer: TTimer;
    InfoTabSheet: TTabSheet;
    StorageStringGrid: TStringGrid;
    UseTaskSchedulerCheckBox: TCheckBox;
    CopyingFileLabel: TLabel;
    BoincServerEdit: TEdit;
    BoincUserNameEdit: TEdit;
    BoincPasswordEdit: TEdit;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label63: TLabel;
    MiCopyConfigBitBtn: TBitBtn;
    Label47: TLabel;
    MiCurrentConfigLabel: TLabel;
    MiRiRefreshBitBtn: TBitBtn;
    MiStartupItemsLaunchBitBtn: TBitBtn;
    MiStartupItemsCheckListBox: TCheckListBox;
    MiStartupItemAddBitBtn: TBitBtn;
    MiStartupItemRemoveBitBtn: TBitBtn;
    MiConfigsLoadBitBtn: TBitBtn;
    MiConfigsRefreshBitBtn: TBitBtn;
    GroupBox10: TGroupBox;
    Label46: TLabel;
    MiConfigsListBox: TFileListBox;
    MiCreateNewProgDirButton1: TButton;
    MiStartupItemsRefreshBitBtn: TBitBtn;
    MiRiStopInstanceBitBtn: TBitBtn;
    MiDeleteBitBtn: TBitBtn;
    MiNewConfigBitBtn: TBitBtn;
    Btn_PluginRefresh: TButton;
    ButtonsListBox: TListBox;
    CCharTabSheet: TTabSheet;
    CreateCCharLocSpinEdit: TSpinEdit;
    MiConfigNameEdit: TEdit;
    EmailAccountComboBox: TComboBox;
    EmailCheckTimeSpinEdit: TSpinEdit;
    EmailLastFromRadioButton: TRadioButton;
    EmailLastSubjectRadioButton: TRadioButton;
    EmailLoginEdit: TEdit;
    EmailMessageCountRadioButton: TRadioButton;
    EmailPasswordEdit: TEdit;
    EmailServerEdit: TEdit;
    EmailSSLEdit: TEdit;
    EmailTabSheet: TTabSheet;
    EnableRemoteSendCheckBox: TCheckBox;
    FoldEnableCheckBox: TCheckBox;
    FoldingAtHomeEmailEdit: TEdit;
    FoldingAtHomeListBox: TListBox;
    FoldingAtHomeTabSheet: TTabSheet;
    GameServerEdit: TEdit;
    GamestatsListBox: TListBox;
    GamestatsRefreshTimeSpinEdit: TSpinEdit;
    GameStatsTabSheet: TTabSheet;
    GameTypeComboBox: TComboBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    HideOnStartup: TCheckBox;
    InternetListBox: TListBox;
    InternetTabSheet: TTabSheet;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label42: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    LastKeyPressedEdit: TEdit;
    LCDFeaturesTabSheet: TTabSheet;
    LeftPageControl: TPageControl;
    MainPageControl: TPageControl;
    MultiInstancePageControl: TPageControl;
    MiCopyToNewRadioButton: TRadioButton;
    RadioButton2: TRadioButton;
    RSSAddressTMemoEdit: TMemo;
    MiscListBox: TListBox;
    MiscTabSheet: TTabSheet;
    MyTabSheet: TTabSheet;
    NetworkStatsAdapterListButton: TButton;
    NetworkStatsListBox: TListBox;
    NetworkStatsTabSheet: TTabSheet;
    NoAutoStart: TRadioButton;
    OpenDialog3: TOpenDialog;
    Panel2: TPanel;
    PluginListBox: TFileListBox;
    PluginsTabSheet: TTabSheet;
    QStatLabel: TLabel;
    RemoteSendBindIPEdit: TEdit;
    RemoteSendGenerateCertKeyButton: TButton;
    RemoteSendPasswordEdit: TEdit;
    RemoteSendPortEdit: TEdit;
    RemoteSendUseSSLCheckBox: TCheckBox;
    RssTypeComboBox: TComboBox;
    ScreensTabSheet: TTabSheet;
    BOINCListBox: TListBox;
    BOINCTabSheet: TTabSheet;
    BOINCEnableCheckBox: TCheckBox;
    ShutdownMessageGroup: TGroupBox;
    RssMaxFreqSpinedit: TSpinEdit;
    RssItemNumSpinEdit: TSpinEdit;
    StartAsAdminCheckBox: TCheckBox;
    StartupTabSheet: TTabSheet;
    MiRunningInstancesListGrid: TStringGrid;
    SysInfoListBox: TListBox;
    SysInfoTabSheet: TTabSheet;
    TabSheet1: TTabSheet;
    MultiInstanceTabSheet: TTabSheet;
    MiConfigsTabSheet: TTabSheet;
    MiRunningProcessesTabSheet: TTabSheet;
    MiStartupItemsTabSheet: TTabSheet;
    VariableEdit: TEdit;
    OpenDialog2: TOpenDialog;
    InsertButton: TButton;
    OpenDialog1: TOpenDialog;
    OpenIco: TOpenPictureDialog;
    WinampListBox: TListBox;
    WinampLocationBrowseButton: TSpeedButton;
    WinampLocationEdit: TEdit;
    WinampLocationLabel: TLabel;
    WinampTabSheet: TTabSheet;
    procedure ActionLogButtonClick(Sender: TObject);
    procedure AddRSSButtonClick(Sender: TObject);
    procedure BacklightBitBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CustomCharClearButtonClick(Sender: TObject);
    procedure CustomCharCreateButtonClick(Sender: TObject);
    procedure CustomCharGridMirrorSpeedButtonClick(Sender: TObject);
    procedure CustomCharInvertButtonClick(Sender: TObject);
    procedure CustomCharUseButtonClick(Sender: TObject);
    procedure ExportClipboardClick(Sender: TObject);
    procedure ExportFileClick(Sender: TObject);
    procedure ExportFileMenuItemClick(Sender: TObject);
    procedure ExportLinesButtonClick(Sender: TObject);
    procedure HidePluginMenuItemClick(Sender: TObject);
    procedure IconSelectComboBoxChange(Sender: TObject);
    procedure IconSelectComboBoxGetItems(Sender: TObject);
    procedure ImportClipboardClick(Sender: TObject);
    procedure ImportFileClick(Sender: TObject);
    procedure ImportFileMenuItemClick(Sender: TObject);
    procedure ImportLinesButtonClick(Sender: TObject);
    procedure ImportLinesClipboardMenuItemClick(Sender: TObject);
    procedure ExportLinesClipboardMenuitemClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure DeleteRSSButtonClick(Sender: TObject);
    procedure DuplicateActionButtonClick(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure InfoTimerTimer(Sender: TObject);
    procedure LeftTabsPositionComboBoxChange(Sender: TObject);
    procedure LineClearButtonClick(Sender: TObject);
    procedure MainPageControlResize(Sender: TObject);
    procedure OpenPluginFolderButtonClick(Sender: TObject);
    procedure pdhRefreshButtonClick(Sender: TObject);
    procedure PerfCountersListBoxClick(Sender: TObject);
    procedure PerfSettingsIndexComboBoxChange(Sender: TObject);
    procedure PluginDemoListBoxClick(Sender: TObject);
    procedure ListBoxKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PluginListBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShowHiddenPluginsMenuItemClick(Sender: TObject);
    procedure ShowTabsCheckBoxClick(Sender: TObject);
    procedure SkinSelectComboBoxChange(Sender: TObject);
    procedure SkinSelectComboBoxGetItems(Sender: TObject);
    procedure CCharSpeedButtonClick(Sender: TObject);
    procedure Splitter1ChangeBounds(Sender: TObject);
    procedure UnHidePluginMenuItemClick(Sender: TObject);
    procedure UpdateRSSButtonClick(Sender: TObject);
    procedure VariableEditChange(Sender: TObject);
    procedure ActionsStringGridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ActionsStringGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ActionsStringGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ActionsStringGridSelectEditor(Sender: TObject;
      aCol, aRow: integer; var Editor: TWinControl);
    procedure ActionsStringGridSelection(Sender: TObject; aCol, aRow: integer);
    procedure BitBtn4Click(Sender: TObject);
    procedure BoincPasswordEditChange(Sender: TObject);
    procedure BoincServerEditChange(Sender: TObject);
    procedure BoincServerIndexComboBoxChange(Sender: TObject);
    procedure BoincUserNameEditChange(Sender: TObject);
    procedure ComPortsButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure LCDSizeComboBoxChange(Sender: TObject);
    procedure MiConfigsListBoxClick(Sender: TObject);
    procedure MiConfigsLoadBitBtnClick(Sender: TObject);
    procedure MiConfigsRefreshBitBtnClick(Sender: TObject);
    procedure MiConfigsTabSheetShow(Sender: TObject);
    procedure MiCopyConfigBitBtnClick(Sender: TObject);
    procedure MiCreateNewProgDirButton1Click(Sender: TObject);
    procedure InvertButtonClick(Sender: TObject);
    procedure SelectAllButtonClick(Sender: TObject);
    procedure MiDeleteBitBtnClick(Sender: TObject);
    procedure MiNewConfigBitBtnClick(Sender: TObject);
    procedure MiRiRefreshBitBtnClick(Sender: TObject);
    procedure MiRiStopInstanceBitBtnClick(Sender: TObject);
    procedure MiRunningProcessesTabSheetShow(Sender: TObject);
    procedure MiStartupItemAddBitBtnClick(Sender: TObject);
    procedure MiStartupItemRemoveBitBtnClick(Sender: TObject);
    procedure MiStartupItemsLaunchBitBtnClick(Sender: TObject);
    procedure MiStartupItemsRefreshBitBtnClick(Sender: TObject);
    procedure MiStartupItemsTabSheetShow(Sender: TObject);
    procedure RssPageChange(Sender: TObject);
    procedure ScreenSpinEditChange(Sender: TObject);
    procedure UseTaskSchedulerCheckBoxChange(Sender: TObject);
    procedure WinampListBoxClick(Sender: TObject);
    procedure InsertButtonClick(Sender: TObject);
    procedure SysInfoListBoxClick(Sender: TObject);
    procedure InternetListBoxClick(Sender: TObject);
    procedure QStatLabelClick(Sender: TObject);
    procedure MiscListBoxClick(Sender: TObject);
    procedure LeftPageControlChange(Sender: TObject);
    procedure GameServerEditExit(Sender: TObject);
    procedure BOINCListBoxClick(Sender: TObject);
    procedure DistributedNetBrowseButtonClick(Sender: TObject);
    procedure EmailAccountComboBoxChange(Sender: TObject);
    procedure ContinueLineCheckBoxClick(Sender: TObject);
    procedure WinampLocationBrowseButtonClick(Sender: TObject);
    procedure GamestatsListBoxClick(Sender: TObject);
    procedure LineEditEnter(Sender: TObject);
    procedure NetworkStatsListBoxClick(Sender: TObject);
    procedure FoldingAtHomeListBoxClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure SaveAsButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MainPageControlChange(Sender: TObject);
    procedure ActionAddButtonClick(Sender: TObject);
    procedure ActionDeleteButtonClick(Sender: TObject);
    procedure ButtonsListBoxClick(Sender: TObject);
    procedure StickyCheckboxClick(Sender: TObject);
    procedure ColorSchemeComboBoxChange(Sender: TObject);
    procedure DisplayPluginListChange(Sender: TObject);
    procedure ContrastTrackBarChange(Sender: TObject);
    procedure BrightnessTrackBarChange(Sender: TObject);
    procedure Btn_PluginRefreshClick(Sender: TObject);
    procedure PluginListBoxClick(Sender: TObject);
    procedure ShutdownEditEnter(Sender: TObject);
    procedure DrawPreviewIcons(const sIconFileName: string);
    procedure LineEditClick(Sender: TObject);
    procedure CCharEditGridChange(Sender: TObject);
    procedure NetworkStatsAdapterListButtonClick(Sender: TObject);
    procedure CopyToScreenButtonClick(Sender: TObject);
    procedure MoveToScreenButtonClick(Sender: TObject);
    procedure SwapWithScreenButtonClick(Sender: TObject);
    procedure RemoteSendGenerateCertKeyButtonClick(Sender: TObject);
    procedure RemoteSendUseSSLCheckBoxClick(Sender: TObject);
    procedure FoldEnableCheckBoxClick(Sender: TObject);
    procedure BOINCEnableCheckBoxClick(Sender: TObject);
    procedure FormEditApply(Sender: TObject);
    procedure FormEditOk(Sender: TObject);
    procedure FormEditCancel(Sender: TObject);
    procedure FormEditMemoEnter(Sender: TObject);
    procedure FormEditMemoOnClick(Sender: TObject);
    procedure PluginListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    CustomCharCheckBoxes: Array[0..40] of TCheckBox;
    SavedCustomCharButtons: Array[0..32] of TSpeedButton;
    FormEditArray: Array [1..MaxLines] of TFormEdit;
    LineEditArray: Array[1..MaxLines] of TEdit;
    LineEditButtonArray: Array[1..MaxLines] of TSpeedButton;
    LineClearButtonArray: Array[1..MaxLines] of TButton;
    ContinueLineCheckBoxArray: Array[1..MaxLines] of TCheckBox;
    DontScrollLineCheckBoxArray: Array[1..MaxLines] of TCheckBox;
    CenterLineCheckBoxArray: Array[1..MaxLines] of TCheckBox;
    ShutdownEditArray: Array[1..MaxLines] of TMemo;
    CCharSpeedButtonArray: Array[1..8] of TSpeedButton;
    DLLPath: string;
    setupbutton: integer;
    shdownmessagebutton: integer;
    CurrentlyShownEmailAccount: integer;
    CurrentScreen: integer;
    DetectedOS: string;
    ShowHiddenPlugins: boolean;
    MirrorCustomArray: TCustomArray;
    procedure LoadCCharSpeedButtonGlyphs;
    procedure FocusToInputField;
    procedure SaveScreen(scr: integer);
    procedure LoadScreen(scr: integer);
    procedure LoadHint(DisplayDLLName: string);
    procedure LoadConfig(Sender: TObject);
    procedure LoadSettings(Sender: TObject);
    procedure CustomCharToButtonGlyph(Button: TSpeedButton; CustomArray: TCustomArray);
    procedure SavedCustomCharButtonClick(Sender: TObject);
    procedure CCharDefFormApply(Sender: TObject);
    procedure CCharDefFormOK(Sender: TObject);
    procedure CCharDefFormCancel(Sender: TObject);
    procedure CCharDefFormEditEnter(Sender: TObject);
  end;

procedure UpdateSetupForm(cKey: char);

var
  SourceCol, SourceRow: integer;
{$IFDEF STANDALONESETUP}
  SetupForm: TSetupForm;
  ConfigFileName: String = 'config.ini';
  ProcessesList: TProcessList;
  FileSelectList: TCheckListBox;
{$ENDIF}

implementation

uses
  ShellApi, Graphics, SysUtils, dateutils, UCCharDef,
{$IFNDEF STANDALONESETUP}
  UMain,
{$ELSE}
  JwaTlHelp32,
  strutils,
{$ENDIF}
   UDataNetwork, UDataWinamp, UData,
  UIconUtils, UUtils, IpRtrMib, IpHlpApi, lazutf8, registry, IniFiles, StrUtils;

function PdhEnumObjectsA( szDataSource: PAnsiChar; szMachineName: PAnsiChar; mszObjectList: PPAnsiChar; pcchBufferSize: PDWORD; dwDetailLevel: DWORD; bRefresh: boolean ) : HRESULT; stdcall; external 'pdh' name 'PdhEnumObjectsA';
function PdhEnumObjectItemsW( szDataSource: PAnsiString;
  szMachineName: PAnsiString;
  szObjectName: pointer;
  mszCounterList: PPWideChar;
  pcchCounterListLength: PDWORD;
  mszInstanceList: PPWideChar;
  pcchInstanceListLength: PDWORD;
  dwDetailLevel: DWORD;
  dwFlags: DWORD ) : HRESULT; stdcall; external 'pdh' name 'PdhEnumObjectItemsW';


{$R *.lfm}

procedure TSetupForm.VariableEditChange(Sender: TObject);
begin
  VariableEdit.Hint:=VariableEdit.Text;
end;

procedure TSetupForm.pdhRefreshButtonClick(Sender: TObject);
var
  buffsize: DWORD;
  buff: array of byte;
  ret: HRESULT;
  list: array of string;
  i: integer;
begin


  buffsize := 0;
  if (PdhEnumObjectsA(nil, nil, nil, @buffsize, PERF_DETAIL_WIZARD, true) = PDH_MORE_DATA) then
  begin
    // according to https://learn.microsoft.com/en-us/windows/win32/api/pdh/nf-pdh-pdhenumobjectsa
    // Were only supposed to add 1 on xp but we get a range check error sometimes without it here
    SetLength(buff, buffsize+1);
    ret := PdhEnumObjectsA(nil, nil, @buff[0], @buffsize, PERF_DETAIL_WIZARD, false);
    if ret <> 0 then
    begin
      PerfCountersListBox.Clear;
      PerfCountersListBox.Enabled := false;
      PerfCountersListBox.Items.Add('Failed: PdhEnumObjects ' + inttostr(ret));
      PerfCountersListBox.Items.Add('Try refresh');
      exit;
    end;
    FormatComboBox.Enabled := true;
    ScalingComboBox.Enabled := true;
    Button2.Enabled := true;
    PerfSettingsIndexComboBox.Enabled := true;
  end
  else
  begin
    PerfCountersListBox.Clear;
    PerfCountersListBox.Enabled := false;
    PerfCountersListBox.Items.Add('Failed: PdhEnumObjects failed');
    PerfCountersListBox.Items.Add('Try refresh');
    exit;
  end;
  PerfCountersListBox.Enabled := true;

  while length(buff) > 0 do
  begin
    SetLength(list, length(list)+1);
    list[length(list) -1] := PAnsiChar(buff);
    delete(buff, 0, length(PAnsiChar(buff)) +1);
    if (list[length(list)-1] = '') then
      delete(list, length(list)-1, 1);
  end;
  PerfCountersListBox.Clear;
  for i := 0 to length(list) -1 do
  PerfCountersListBox.Items.AddText(TEncoding.ANSI.GetAnsiString(tbytes(list[i])));

  PerfSettingsIndexComboBox.Clear;
  for i := 1 to length(config.PerfSettings) do
    PerfSettingsIndexComboBox.Items.Add(inttostr(i));

    PerfSettingsIndexComboBox.ItemIndex := 0;
  PerfSettingsIndexComboBoxChange(Sender);
end;

procedure TSetupForm.PerfCountersListBoxClick(Sender: TObject);
var
  item: string;
  buff1size: DWORD;
  buff2size: DWORD;
  buff1: array of widechar;
  buff2: array of widechar;
  ret: HRESULT;
  list: array of string;
  wideChars   : array[0..255] of WideChar;
begin
  item := PerfCountersListBox.GetSelectedText;
  StringToWideChar(rawbytestring(item), wideChars, 255);
  buff1size := 0;
  buff2size := 0;
  PerfCountersListBox.Hint := PerfCountersListBox.GetSelectedText;

  if ( PdhEnumObjectItemsW(nil, nil, pwidechar(wideChars), nil, @buff1size, nil, @buff2size, PERF_DETAIL_WIZARD, 0) = PDH_MORE_DATA ) then
  begin
    // according to https://learn.microsoft.com/en-us/windows/win32/api/pdh/nf-pdh-pdhenumobjectsa
    // We're only supposed to add 1 on xp and only to PdhEnumObjectsA buffer but we get a range check error
    // on certain counters without it here
    SetLength(buff1, buff1size+1);
    SetLength(buff2, buff2size+1);
    ret := PdhEnumObjectItemsW(nil, nil, pwidechar(wideChars), @buff1[0], @buff1size, @buff2[0], @buff2size, PERF_DETAIL_WIZARD, 0);
    if ret <> 0 then
    begin
      PerfCountersListBox.Clear;
      PerfCountersListBox.Enabled := false;
      PerfCountersListBox.Items.Add('Failed: PdhEnumObjectItems ' + inttostr(ret));
      PerfCountersListBox.Items.Add('Try refresh');
      exit;
    end;
  end
  else
  begin
    PerfCountersListBox.Clear;
    PerfCountersListBox.Enabled := false;
    PerfCountersListBox.Items.Add('Failed: PdhEnumObjectItems failed');
    PerfCountersListBox.Items.Add('Try refresh');
    exit;
  end;

  while length(buff1) > 0 do
  begin
    SetLength(list, length(list)+1);
    list[length(list) -1] := PWideChar(buff1);

    delete(buff1, 0, length(PWideChar(buff1))+1);
    if (list[length(list)-1] = '') then
      delete(list, length(list)-1, 1);
  end;

  CountersComboBox.Clear;
  if (length(list) > 0) then
  begin
    CountersComboBox.Enabled := true;
    CountersComboBox.Items.AddStrings(list);
    CountersComboBox.ItemIndex := 0;
  end
  else
    CountersComboBox.Enabled := false;

  SetLength(list, 0);

  while length(buff2) > 0 do
  begin
    SetLength(list, length(list)+1);
    list[length(list) -1] := PWideChar(buff2);
    delete(buff2, 0, length(PWideChar(buff2)) +1);
    if (list[length(list)-1] = '') then
      delete(list, length(list)-1, 1);
  end;

  InstancesComboBox.Clear;
  if (length(list) > 0) then
  begin
    InstancesComboBox.Enabled := true;
    InstancesComboBox.Items.AddStrings(list);
    InstancesComboBox.ItemIndex := 0;
  end
  else
    InstancesComboBox.Enabled := false;
end;

procedure TSetupForm.Button2Click(Sender: TObject);
begin
  config.PerfSettings[PerfSettingsIndexComboBox.ItemIndex+1].PerfObject := PerfCountersListBox.GetSelectedText;
  config.PerfSettings[PerfSettingsIndexComboBox.ItemIndex+1].Counter := CountersComboBox.Text;
  config.PerfSettings[PerfSettingsIndexComboBox.ItemIndex+1].Instance := InstancesComboBox.Text;
  config.PerfSettings[PerfSettingsIndexComboBox.ItemIndex+1].Format := FormatComboBox.ItemIndex;
  config.PerfSettings[PerfSettingsIndexComboBox.ItemIndex+1].Scaling := ScalingComboBox.ItemIndex;
end;

// fix some silliness in the way controls are resized(or rather, not resized)
procedure TSetupForm.FormChangeBounds(Sender: TObject);
var
  i: integer;
begin
  While MainPageControl.Width > width - MainPageControl.Left do
    begin
      i := width - (MainPageControl.Left + MainPageControl.Width);
      Splitter1.MoveSplitter(i);
    end;
end;

procedure TSetupForm.BacklightBitBtnClick(Sender: TObject);
begin
  if LCDSmartieDisplayForm.Backlight then
  begin
    BacklightBitBtn.Caption := 'Backlight' + #13#10 + 'On';
    LCDSmartieDisplayForm.backlit();
  end
  else
  begin
    BacklightBitBtn.Caption := 'Backlight' + #13#10 + 'Off';
    LCDSmartieDisplayForm.backlit();
  end;
end;

procedure TSetupForm.Button1Click(Sender: TObject);
var
  loop: integer;
begin
  if CCharDefForm = nil then
  begin
    CCharDefForm := TCCharDefForm.Create(Self);
    CCharDefForm.CCharDefFormApplyButton.OnClick := CCharDefFormApply;
    CCharDefForm.CCharDefFormOKButton.OnClick := CCharDefFormOK;
    CCharDefForm.CCharDefFormCancelButton.OnClick := CCharDefFormCancel;
    CCharDefForm.Top := config.CCharFormTop;
    CCharDefForm.Left := config.CCharFormLeft;
    for  loop := 1 to 8 do
    begin
      CCharDefForm.EditArray[loop].OnEnter := CCharDefFormEditEnter;
      CCharDefForm.EditArray[loop].Text := config.screen[ScreenSpinEdit.Value].CustomCharacters[loop];
    end;

    CCharDefForm.Show;
    CCharDefForm.EditArray[1].SetFocus;
  end;

end;

////////////////////////////
// Import/Export lines /////
////////////////////////////
procedure TSetupForm.ImportLinesButtonClick(Sender: TObject);
begin
  ImportPopupMenu.PopUp;
end;

procedure TSetupForm.ExportLinesButtonClick(Sender: TObject);
begin
  ExportPopupMenu.PopUp;
end;

procedure TSetupForm.ImportFileMenuItemClick(Sender: TObject);
var
  Lines: TStringlist;
  str: string;
  i, j: integer;
begin

  if ImportFileOpenDialog.Execute then
  begin
    Lines := TStringlist.Create;
    Lines.LoadFromFile(ImportFileOpenDialog.FileName);
    For i := 0 to lines.Count -1 do
    begin
      str := lines[i];
      for j := 1 to MaxLines do
      begin
        if (pos('Text0'+inttostr(j)+'="', str) > 0) then
        begin
          // is the line longer than Text0N=" and is the last char "
          if (length(str) > 8) and (copy(str, length(str), 1) = '"') then
          begin
            // looks good copy the text
            LineEditArray[j].Text := copy(str, 9 , length(str) - 9);
          end;
        end;
      end;
    end;

    for i := 1 to 8 do
    for j := 0 to lines.Count -1 do
      if pos('CustomCharacters'+inttostr(i)+'=', lines[j]) > 0 then
      begin
        str := copy(lines[j], 19);
        delete(str, pos(#$0D, str),1); // remove this or something will get mad
        config.screen[ScreenSpinEdit.Value].CustomCharacters[i] := str;
      end;
    LoadCCharSpeedButtonGlyphs;
    Lines.Free;
  end;
end;

procedure TSetupForm.ExportFileMenuItemClick(Sender: TObject);
var
  Lines: TStringlist;
  i: integer;
begin
  ExportFileSaveDialog.FileName := ('screen'+inttostr(ScreenSpinEdit.Value)+'.txt');
  if ExportFileSaveDialog.Execute then
  begin
    Lines := TStringlist.Create;
    for i := 1 to MaxLines do
      Lines.Add('Text0'+inttostr(i)+'="'+LineEditArray[i].Text+'"');
    for i := 1 to 8 do
      Lines.Add('CustomCharacters'+inttostr(i)+'='+config.screen[ScreenSpinEdit.Value].CustomCharacters[i]);

    Lines.SaveToFile(ExportFileSaveDialog.Filename);
  end;
  Lines.Free;
end;

procedure TSetupForm.ImportLinesClipboardMenuItemClick(Sender: TObject);
var
  Clipboard: TClipboard;
  Lines: TStringlist;
  str: string;
  i, j: integer;
begin
  Clipboard := TClipboard.Create;
  Lines := TStringlist.Create;
  Lines.AddDelimitedtext(Clipboard.AsText, #10, true);
  For i := 0 to lines.Count -1 do
  begin
    str := lines[i];
    // Remove the CR (#13) char
    delete(str, pos(#13, str),1);
    for j := 1 to MaxLines do
    begin
      if (pos('Text0'+inttostr(j)+'="', str) > 0) then
      begin
        // is the line longer than Text0N=" and is the last char "
        if (length(str) > 8) and (copy(str, length(str), 1) = '"') then
        begin
          // looks good copy the text
          LineEditArray[j].Text := copy(str, 9 , length(str) - 9);
        end;
      end;
    end;
  end;
  for i := 1 to 8 do
    for j := 0 to lines.Count -1 do
      if pos('CustomCharacters'+inttostr(i)+'=', lines[j]) > 0 then
      begin
        str := copy(lines[j], 19);
        delete(str, pos(#$0D, str),1); // remove this or something will get mad
        config.screen[ScreenSpinEdit.Value].CustomCharacters[i] := str;
      end;

  LoadCCharSpeedButtonGlyphs;
  Lines.Free;
  Clipboard.free;
end;

procedure TSetupForm.ExportLinesClipboardMenuitemClick(Sender: TObject);
var
  Clipboard: TClipboard;
  Lines: TStringlist;
  i: integer;
begin
  Clipboard := TClipboard.Create;
  Lines := TStringlist.Create;

  for i := 1 to MaxLines do
    Lines.Add('Text0'+inttostr(i)+'="'+LineEditArray[i].Text+'"');

  for i := 1 to 8 do
      Lines.Add('CustomCharacters'+inttostr(i)+'='+config.screen[ScreenSpinEdit.Value].CustomCharacters[i]);

  Clipboard.AsText := Lines.Text;
  Lines.Free;
  Clipboard.Free;
end;

procedure TSetupForm.AddRSSButtonClick(Sender: TObject);
var
  i: integer;
begin
  if (length(RSSNameEdit.Text) > 0) and (length(RSSAddressTMemoEdit.Text) > 0) then
  begin
    for i := 0 to config.RSSList.Names.Count -1 do
      if RSSNameEdit.Text = config.RSSList.Names[i] then
      begin
        showmessage('Choose a name that doesn''t already exist or use Update instead');
        Exit;
      end;

    config.RSSList.Names.Add(RSSNameEdit.Text);
    config.RSSList.Addresses.Add(RSSAddressTMemoEdit.Text);
    InternetListBox.Items := config.RSSList.Names;

  end;
end;

procedure TSetupForm.ActionLogButtonClick(Sender: TObject);
begin
  if LCDSmartieDisplayForm.ActionLogForm.Visible then
    LCDSmartieDisplayForm.ActionLogForm.Hide
  else
    LCDSmartieDisplayForm.ActionLogForm.Show;
end;

procedure TSetupForm.UpdateRSSButtonClick(Sender: TObject);
begin
  if (length(RSSNameEdit.Text) > 0) and (length(RSSAddressTMemoEdit.Text) > 0) and (InternetListBox.ItemIndex >= 0) then
  begin
    config.RSSList.Names[InternetListBox.ItemIndex] := RSSNameEdit.Text;
    config.RSSList.Addresses[InternetListBox.ItemIndex] := RSSAddressTMemoEdit.Text;
    InternetListBox.Items := config.RSSList.Names;
  end;
end;

procedure TSetupForm.DeleteRSSButtonClick(Sender: TObject);
begin
  if (InternetListBox.ItemIndex >= 0) then
  begin
    config.RSSList.Names.Delete(InternetListBox.ItemIndex);
    config.RSSList.Addresses.Delete(InternetListBox.ItemIndex);
    InternetListBox.Items := config.RSSList.Names;
  end;
end;

function CorrectPlural(const s: string; Count: Integer): string;
begin
  Result := IntToStr(Count) + ' ' + s;
  if Count<>1 then begin
    Result := Result + 's';
  end;
end;

function HumanReadableTime(Time: Double): string;
//Time is in seconds
const
  SecondsPerMinute = 60;
  SecondsPerHour = 60*SecondsPerMinute;
  SecondsPerDay = 24*SecondsPerHour;
  SecondsPerWeek = 7*SecondsPerDay;
  SecondsPerYear = 365*SecondsPerDay;

var
  Years, Weeks, Days, Hours, Minutes, Seconds: Int64;

begin
  Try
    Years := Trunc(Time/SecondsPerYear);
    Time := Time - Years*SecondsPerYear;
    Weeks := Trunc(Time/SecondsPerWeek);
    Time := Time - Weeks*SecondsPerWeek;
    Days := Trunc(Time/SecondsPerDay);
    Time := Time - Days*SecondsPerDay;
    Hours := Trunc(Time/SecondsPerHour);
    Time := Time - Hours*SecondsPerHour;
    Minutes := Trunc(Time/SecondsPerMinute);
    Time := Time - Minutes*SecondsPerMinute;
    Seconds := Trunc(Time);

    if Years>5000 then begin
      Result := IntToStr(Round(Years/1000))+' millennia';
    end else if Years>500 then begin
      Result := IntToStr(Round(Years/100))+' centuries';
    end else if Years>0 then begin
      Result := CorrectPlural('year', Years) + ' ' + CorrectPlural('week', Weeks);
    end else if Weeks>0 then begin
      Result := CorrectPlural('week', Weeks) + ' ' + CorrectPlural('day', Days);
    end else if Days>0 then begin
      Result := CorrectPlural('day', Days) + ' ' + CorrectPlural('hour', Hours);
    end else if Hours>0 then begin
      Result := CorrectPlural('hour', Hours) + ' ' + CorrectPlural('minute', Minutes);
    end else if Minutes>0 then begin
      Result := CorrectPlural('minute', Minutes)+ ' ' + CorrectPlural('second', Seconds);
    end else begin
      Result := CorrectPlural('second', Seconds);
    end;
  Except
    Result := 'an eternity';
  End;
end;

procedure TSetupForm.InfoTimerTimer(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to length(LCDSmartieDisplayForm.Data.storage) - 1 do
    if LCDSmartieDisplayForm.Data.storage[i] <> '' then
      StorageStringGrid.Cells[1,i+1] :=  LCDSmartieDisplayForm.Data.storage[i];

  RunTimeLabel.Caption :=  HumanReadableTime(secondsbetween(Now, LCDSmartieDisplayForm.StartTime));
  WindowsVersionLabel.Caption := DetectedOS;
end;

procedure TSetupForm.LeftTabsPositionComboBoxChange(Sender: TObject);
begin
  config.TabsPosition := TTabPosition(LeftTabsPositionComboBox.ItemIndex);
  LeftPageControl.TabPosition := config.TabsPosition;
end;

procedure TSetupForm.LineClearButtonClick(Sender: TObject);
var
  loop: integer;
begin
  for loop := 1 to MaxLines do
  if Sender = LineClearButtonArray[loop] then
    begin
      LineEditArray[loop].Clear;
      // dont know if we want to do this yet
      //if assigned(FormEditArray[Loop]) then
      //  FormEditArray[Loop].Memo1.Clear;
    end;
end;

procedure TSetupForm.MainPageControlResize(Sender: TObject);
begin
  MainPageControl.Invalidate;
end;

procedure TSetupForm.OpenPluginFolderButtonClick(Sender: TObject);
begin
  ShellExecute(0, Nil, pchar('plugins'), Nil, Nil, SW_NORMAL);
end;

procedure TSetupForm.PerfSettingsIndexComboBoxChange(Sender: TObject);
var
  itemindex: integer;
begin

  itemindex := PerfCountersListBox.Items.IndexOf(config.PerfSettings[PerfSettingsIndexComboBox.ItemIndex+1].PerfObject);
  PerfCountersListBox.ItemIndex := itemindex;

  if itemindex >= 0 then
    PerfCountersListBoxClick(Sender);

  itemindex := CountersComboBox.Items.IndexOf(config.PerfSettings[PerfSettingsIndexComboBox.ItemIndex+1].Counter);
  CountersComboBox.ItemIndex := itemindex;

  itemindex := InstancesComboBox.Items.IndexOf(config.PerfSettings[PerfSettingsIndexComboBox.ItemIndex+1].Instance);
  InstancesComboBox.ItemIndex := itemindex;

  FormatComboBox.ItemIndex := config.PerfSettings[PerfSettingsIndexComboBox.ItemIndex+1].Format;
  ScalingComboBox.ItemIndex := config.PerfSettings[PerfSettingsIndexComboBox.ItemIndex+1].Scaling;
  VariableEdit.Caption := '$Perf('+inttostr(PerfSettingsIndexComboBox.ItemIndex+1)+')';
end;

procedure TSetupForm.LoadHint(DisplayDLLName: string);
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

procedure TSetupForm.FormShow(Sender: TObject);
begin
  {$IFDEF STANDALONESETUP}
  MultiInstanceTabSheet.TabVisible := true;
  SetupForm.BorderIcons := [biSystemMenu,biMinimize];
  BitBtn1.Caption := '&Save';
  BitBtn1.Hint := 'Save configuration';
  BitBtn2.Caption := '&Exit';
  BitBtn2.Kind := bkClose;
  BitBtn3.Caption := 'S&ave as';
  BitBtn3.Hint := 'Save under a different filename';
  BitBtn3.OnClick := SaveAsButtonClick;
  BitBtn4.Visible := true;
  LoadConfig(Sender);
  if not FileExists(ConfigFileName) then
  exitprocess(1);
  {$ENDIF}
  LoadSettings(Sender);

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

procedure TSetupForm.LoadConfig(Sender: TObject);
begin
  {$IFDEF STANDALONESETUP}
  if (ConfigFileName = '') or (not FileExists(ConfigFileName)) then
  begin
    OpenDialog3.Execute;

    if (OpenDialog3.FileName = '')  then
      // Brute force as calling Application.terminate would execute the rest of this procedure first
      exit;

    ConfigFileName := OpenDialog3.Filename;
  end;

    config := TConfig.Create(ConfigFileName);
    Caption := ConfigFileName;
    MiCurrentConfigLabel.Caption := ConfigFileName;
    if (config.load() = false) then
    begin
      if FileExists(ConfigFileName) then
      begin
        showmessage('Fatal Error:  Failed to load configuration ('+ConfigFileName+')');
        exitprocess(1);
      end;
      hConfig := FileCreate(ConfigFileName); // create empty config
      If hConfig=-1 then
      begin
        FileClose(hConfig);
        showmessage('Configuration file ('+ConfigFileName+') could not be created');
        exitprocess(1);
      end;
      FileClose(hConfig);
      config.load(); // Load default values for empty config
      config.save(); // save default values
    end;
  {$ENDIF}
end;

procedure TSetupForm.LoadSettings(Sender: TObject);
var
  SR: TSearchRec;
  Loop, FindResult: integer;
  NetStat: TNetworkStatistics;
  WinampStat: TWinampStat;
  i: integer;
  ActOpr: string;
begin
  AppendConfigNameCheckBox.Checked := config.AppendConfigName;
  CustomTitleTIEdit1.Text := config.MainFormCaption;

  {$IFNDEF STANDALONESETUP}
  SetupForm.Top := config.SettingsFormPosTop;
  SetupForm.Left := config.SettingsFormPosLeft;
  SetupForm.Height := config.SettingsFormPosHeight;
  SetupForm.Width := config.SettingsFormPosWidth;

  MainPageControl.ActivePage := ScreensTabSheet;
  {$ENDIF}

  GameServerEdit.Text := config.gameServer[1, 1];

  ActionsStringGrid.DragMode := dmManual;
  ActionsStringGrid.colcount := 8;
  ActionsStringGrid.rowcount := 1; // leave one for the header row

  // setup grid column widths
  ActionsStringGrid.ColWidths[0] := config.ActionIndexCol;
  ActionsStringGrid.ColWidths[1] := config.ActionIfCol;
  ActionsStringGrid.ColWidths[2] := config.ActionVariableCol;
  ActionsStringGrid.ColWidths[3] := config.ActionIsCol;
  ActionsStringGrid.ColWidths[4] := config.ActionValueCol;
  ActionsStringGrid.ColWidths[5] := config.ActionThenCol;
  ActionsStringGrid.ColWidths[6] := config.ActionActionCol;
  ActionsStringGrid.ColWidths[7] := config.ActionEnabledCol;
  // Populate the grid
  for i := 1 to config.totalactions do
  begin
    ActionsStringGrid.RowCount := ActionsStringGrid.RowCount + 1;
    ActionsStringGrid.Cells[1, ActionsStringGrid.RowCount - 1] := 'if';
    ActionsStringGrid.Cells[2, ActionsStringGrid.RowCount - 1] := config.actionsArray[i, 1];
    case (StrToInt(config.actionsArray[i, 2])) of
      0: ActOpr := '>';
      1: ActOpr := '<';
      2: ActOpr := '=';
      3: ActOpr := '<=';
      4: ActOpr := '>=';
      else
        ActOpr := '<>';
    end;
    ActionsStringGrid.Cells[3, ActionsStringGrid.RowCount - 1] := ActOpr;
    ActionsStringGrid.Cells[4, ActionsStringGrid.RowCount - 1] := config.actionsArray[i, 3];
    ActionsStringGrid.Cells[5, ActionsStringGrid.RowCount - 1] := 'then';
    ActionsStringGrid.Cells[6, ActionsStringGrid.RowCount - 1] := config.actionsArray[i, 4];
    ActionsStringGrid.Cells[7, ActionsStringGrid.RowCount - 1] := config.actionsArray[i, 5];
  end;
  //ActionsStringGrid.DeleteRow(config.totalactions);

  ScreenSpinEdit.MaxValue := MaxScreens;

  // load curent screen into setup form
  {$IFNDEF STANDALONESETUP}
  ScreenSpinEdit.Value := LCDSmartieDisplayForm.activeScreen;
  LoadScreen(LCDSmartieDisplayForm.activeScreen);
  {$ELSE}
  LoadScreen(1);
  {$ENDIF}
  ProgramRefreshIntervalSpinEdit.Value := config.refreshRate;
  WinampLocationEdit.Text := config.winampLocation;
  ColorSchemeComboBox.ItemIndex := config.colorOption;
  IconSelectComboBox.Text := config.sTrayIcon;
  SkinSelectComboBox.Text := config.sSkinPath;
  DrawPreviewIcons(IconSelectComboBox.Text);

  DistributedNetLogfileEdit.Text := config.distLog;

  EmailCheckTimeSpinEdit.Value := config.emailPeriod;
  DLLCheckIntervalSpinEdit.Value := config.dllPeriod;
  ProgramScrollIntervalSpinEdit.Value := config.scrollPeriod;

  StayOnTopCheckBox.Checked := config.alwaysOnTop;
  HideOnStartup.Checked := config.bHideOnStartup;
  NoAutoStart.Checked := True;
  AutoStart.Checked := config.bAutoStart;
  AutoStartHide.Checked := config.bAutoStartHide;
  StartAsAdminCheckBox.Checked := config.bStartAsAdmin;
  UseTaskSchedulerCheckBox.Checked := config.bUseTaskScheduler;
  EmulateLCDCheckbox.Checked := config.EmulateLCD;
  HideOnStartup.Checked := config.bHideOnStartup;
  ShutdownEdit1.Text := config.ShutdownMessage[1];
  ShutdownEdit2.Text := config.ShutdownMessage[2];
  ShutdownEdit3.Text := config.ShutdownMessage[3];
  ShutdownEdit4.Text := config.ShutdownMessage[4];

  WebProxyServerEdit.Text := config.httpProxy;
  WebProxyPortEdit.Text := IntToStr(config.httpProxyPort);

  EmailAccountComboBox.Clear;
  for i := 1 to MaxEmailAccounts do
  begin
    EmailAccountComboBox.Items.Add(IntToStr(i));
  end;
  EmailAccountComboBox.ItemIndex := 0;
  EmailServerEdit.Text := config.pop[1].server;
  EmailLoginEdit.Text := config.pop[1].user;
  EmailPasswordEdit.Text := config.pop[1].pword;
  EmailSSLEdit.Text := config.pop[1].port_ssl;

  NetworkStatsListBox.Clear;
  for NetStat := FirstNetworkStat to LastNetworkStat do
  begin
    NetworkStatsListBox.Items.Add(NetworkUserHints[NetStat]);
  end;

  WinampListBox.Clear;
  for WinampStat := FirstWinampStat to LastWinampStat do
  begin
    WinampListBox.Items.Add(WinampHints[WinampStat]);
  end;

  LCDSizeComboBox.Items.Clear;
  for i := 1 to MaxScreenSizes do
    LCDSizeComboBox.Items.Add(ScreenSizes[i].SizeName);
  LCDSizeComboBox.ItemIndex := config.ScreenSize - 1;
  CustomLinesSizeEdit.Value := config.Custom_Height;
  CustomCharsSizeEdit.Value := config.Custom_width;
  LCDSizeComboBoxChange(Sender);

  // put display plugin settings on screen
  ContrastTrackBar.position := config.DLL_contrast;
  BrightnessTrackBar.position := config.DLL_brightness;

  DisplayPluginList.Items.Clear;
  DisplayPluginList.Items.Add('None');
  DLLPath := extractfilepath(ParamStr(0)) + 'displays\';
  FindResult := findfirst(DLLPath + '*.dll', 0, SR);
  while (FindResult = 0) do
  begin
    DisplayPluginList.Items.Add(extractfilename(SR.Name));
    FindResult := FindNext(SR);
  end;
  findclose(SR);
  DisplayPluginList.ItemIndex := 0;
  for Loop := 0 to DisplayPluginList.Items.Count - 1 do
  begin
    if lowercase(config.DisplayDLLName) = lowercase(DisplayPluginList.Items[Loop]) then
    begin
      DisplayPluginList.ItemIndex := Loop;
    end;
  end;

  DisplayPluginListChange(Sender);
  ParametersEdit.Text := config.DisplayDLLParameters; // set our original parameters back

  RandomizeScreensCheckBox.Checked := config.randomScreens;
  GamestatsRefreshTimeSpinEdit.Value := config.gameRefresh;
  FoldingAtHomeEmailEdit.Text := config.foldUserid;

  EnableRemoteSendCheckBox.Checked := config.EnableRemoteSend;
  RemoteSendBindIPEdit.Text := config.RemoteSendBindIP;
  RemoteSendPortEdit.Text := config.RemoteSendPort;
  RemoteSendPasswordEdit.Text := config.RemoteSendPassword;

  if fileExists(ExtractFilePath(ParamStr(0)) + 'openssl\cert.pem') and
    fileExists(ExtractFilePath(ParamStr(0)) + 'openssl\key.pem') then
    RemoteSendUseSSLCheckBox.Checked := config.RemoteSendUseSSL
  else
    RemoteSendUseSSLCheckBox.Checked := False;

  for i :=1 to 20 do BoincServerIndexComboBox.Items.Add(inttostr(i));
  BoincServerIndexComboBox.ItemIndex := 0;
  BoincServerIndexComboBoxChange(Sender);

  FoldEnableCheckBox.Checked := config.foldEnabled;
  BOINCEnableCheckBox.Checked := config.boincEnabled;

  for i := 1 to 24 do ButtonsListBox.Items.Delete(1);
  LCDFeaturesTabSheet.Enabled := True;
  ButtonsListBox.Items.Add('FanSpeed(1,1) (nr,divider)');

  // Screen re-arrange populate SpinEdits
  CopyToScreenSpinEdit.Value := 1;
  MoveToScreenSpinEdit.Value := 1;
  SwapWithScreenSpinEdit.Value := 1;

  ActionsTimerSpinEdit.Value := config.ActionsTimer;

  for i := 0 to length(LCDSmartieDisplayForm.Data.storage) - 1 do
    StorageStringGrid.Cells[0,i+1] := inttostr(i);

  if LCDSmartieDisplayForm.Backlight then
    BacklightBitBtn.Caption := 'Backlight' + #13#10 + 'Off'
  else
    BacklightBitBtn.Caption := 'Backlight' + #13#10 + 'On';

  // Load RSS list
  InternetListBox.Items := config.RSSList.Names;

  ShowNetStatsCheckBox.Checked := config.ShowNetStats;
  ShowMiscCheckBox.Checked := config.ShowMisc;
  ShowLCDFeaturesCheckBox.Checked := config.ShowLCDFeatures;
  ShowPluginsCheckBox.Checked := config.ShowPlugins;
  ShowCharEditorCheckBox.Checked := config.ShowCharEditor;
  ShowPerfCheckBox.Checked := config.ShowPerf;
  ShowSysinfoCheckBox.Checked := config.ShowSysinfo;
  ShowWinampCheckBox.Checked := config.ShowWinamp;
  ShowRssCheckBox.Checked := config.ShowRSS;
  ShowGameStatsCheckBox.Checked := config.ShowGameStats;
  ShowBOINCCheckBox.Checked := config.ShowBOINC;
  ShowFoldingAtHomeCheckBox.Checked := config.ShowFoldingAtHome;
  ShowEmailCheckBox.Checked := config.ShowEmail;
  ShowTabsCheckBoxClick(nil);

  LeftTabsPositionComboBox.ItemIndex := Ord(config.TabsPosition);
  LeftPageControl.TabPosition := config.TabsPosition;
  LeftPageControl.ActivePageIndex := config.LastTabIndex;
  MainPageControl.ActivePageIndex := config.LastMainTabIndex;

  OneBySixteenLCDFixupCheckBox.Checked := config.OneBySixteenFixup;

  VariableEdit.Text := NoVariable;
end;

procedure TSetupForm.ActionsStringGridSelectEditor(Sender: TObject;
  aCol, aRow: integer; var Editor: TWinControl);
var
  Items: array [0..37] of string = ('NextTheme', 'LastTheme',
    'NextScreen', 'LastScreen', 'GotoTheme(2)',
    'GotoScreen(2)', 'FreezeScreen', 'UnfreezeScreen', 'ToggleFreeze',
    'RefreshAll', 'Backlight(0/1)',
    'BacklightToggle', 'BacklightFlash(5)',
    'PlayWave[c:\wave.wav]', 'Exec[c:\autoexec.bat]',
    'WinampNextTrack', 'WinampLastTrack',
    'WinampPlay', 'WinampStop', 'WinampPause',
    'WinampShuffle (toggle)', 'WinampVolumeDown',
    'WinampVolumeUp', 'EnableScreen(1-99)',
    'DisableScreen(1-99)', '$dll(name.dll,2,param1,param2)',
    'GPO(1-8,0/1)', 'GPOToggle(1-8)', 'SystemVolumeDown',
    'SystemVolumeMute', 'SystemVolumeUp',
    'GPOFlash(1-8,2)', 'Fan(1-3,0-255)', 'HTTPReq(URL)',
    'HTTPPost(URL,Key1=Data1,Key2=Data2,...)', 'ActionAfterSecs(Action, seconds)', 'ActionAndDisable(Action)', 'EnableAction(ActionIndex, enabled 1/0)');
begin
  if aCol = 3 then
  begin
    Editor := ActionsStringGrid.EditorByStyle(cbsPickList);
    TPickListCellEditor(Editor).Items.CommaText := '=,<,>,<=,>=,<>';
  end;
  if aCol = 6 then
  begin
    Editor := ActionsStringGrid.EditorByStyle(cbsPickList);
    TPickListCellEditor(Editor).Items.SetStrings(items);
  end;
  if aCol = 7 then
  begin
    Editor := ActionsStringGrid.EditorByStyle(cbsPickList);
    TPickListCellEditor(Editor).Items.CommaText := 'True, False';
  end;
end;

procedure TSetupForm.ActionsStringGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pnt: TPoint;
begin
  ActionsStringGrid.MouseToCell(X, Y, SourceCol, SourceRow);
  ActionsStringGrid.Row := SourceRow;
  if button = mbRight then
  begin
    if GetCursorPos(pnt) then
    ActionsExportImportPoup.Popup(pnt.X, pnt.Y);
  end
  else if SourceRow > 0 then
    ActionsStringGrid.BeginDrag(False, 4);
end;


procedure TSetupForm.ActionsStringGridDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
  var
  CurrentCol, CurrentRow: integer;
begin
  ActionsStringGrid.MouseToCell(X, Y, CurrentCol, CurrentRow);
  Accept := (Sender = Source);
end;

procedure TSetupForm.ActionsStringGridDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  DestCol, DestRow: Integer;
begin
  ActionsStringGrid.MouseToCell(X, Y, DestCol, DestRow);
  if DestRow > 0 then
    ActionsStringGrid.MoveColRow(false, SourceRow, DestRow);
end;

procedure TSetupForm.ActionsStringGridSelection(Sender: TObject; aCol, aRow: integer);
begin
  if (aCol = 0) or (aCol = 1) or (aCol = 5) then
    ActionsStringGrid.Options := ActionsStringGrid.Options - [goEditing];

  if (aCol = 2) or (aCol = 3) or (aCol = 4) or (aCol = 6) or (aCol = 7)then
    ActionsStringGrid.Options := ActionsStringGrid.Options + [goEditing];
end;

procedure TSetupForm.BitBtn4Click(Sender: TObject);
begin
  {$IFDEF STANDALONESETUP}
  ConfigFileName := '';
  FormShow(Sender);
  {$ENDIF}
end;

procedure TSetupForm.BoincPasswordEditChange(Sender: TObject);
begin
  config.boincAccount[BoincServerIndexComboBox.ItemIndex+1].password := BoincPasswordEdit.Text;
end;

procedure TSetupForm.BoincServerEditChange(Sender: TObject);
begin
  config.boincAccount[BoincServerIndexComboBox.ItemIndex+1].server := BoincServerEdit.Text;
end;

procedure TSetupForm.BoincServerIndexComboBoxChange(Sender: TObject);
begin
  BoincServerEdit.Text := config.boincAccount[BoincServerIndexComboBox.ItemIndex+1].server;
  BoincUserNameEdit.Text := config.boincAccount[BoincServerIndexComboBox.ItemIndex+1].user;
  BoincPasswordEdit.Text := config.boincAccount[BoincServerIndexComboBox.ItemIndex+1].password;
end;

procedure TSetupForm.BoincUserNameEditChange(Sender: TObject);
begin
  config.boincAccount[BoincServerIndexComboBox.ItemIndex+1].user := BoincUserNameEdit.Text;
end;

//////////// LIST COM PORTS BUTTON ///////////////////

procedure TSetupForm.ComPortsButtonClick(Sender: TObject);
var
  i: integer;
  reg: tregistry;
  portnames: TStringList;
  ports: string;
  aProcess: TProcess;
  lists: TStringList;
begin
  // List Parallel ports
  aProcess := TProcess.Create(nil);
  aProcess.Executable := 'WMIC';
  aProcess.Parameters.Add('/namespace:\\root\cimv2');
  aProcess.Parameters.Add('path');
  aProcess.Parameters.Add('Win32_ParallelPort');
  aProcess.Parameters.Add('get');
  aProcess.Parameters.Add('caption');
  aProcess.Options := aProcess.Options + [poWaitOnExit, poUsePipes, poNoConsole];
  aProcess.Execute;
  lists := TStringList.Create;
  lists.LoadFromStream(aProcess.Output);
  aProcess.Free;
  ports := 'Parallel ports:' + #13#10;
  for i := 1 to lists.Count - 1 do
    ports := ports + lists[i] + #13#10;

  ports := ports + 'Serial ports:' + #13#10;

  // really should try..except this
  portnames := TStringList.Create;
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if Reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
    reg.GetValueNames(portnames);
  for i := 0 to portnames.Count - 1 do
    ports := ports + portnames[i] + ' ' + (reg.ReadString(portnames[i])) + #13#10;
  ShowMessage(ports);
  Reg.Free;
end;

procedure TSetupForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if CCharDefForm <> nil then
  begin
    CCharDefForm.Close;
    CCharDefForm.Free;
    CCharDefForm := nil;
  end;
  {$IFDEF STANDALONESETUP}
  config.Free;
  config := nil;
  {$ENDIF}
end;

procedure TSetupForm.DisplayPluginListChange(Sender: TObject);
begin
  if (DisplayPluginList.ItemIndex > 0) then
    LoadHint(DLLPath + DisplayPluginList.Text)
  else
  begin
    UsageLabel.Caption := 'no parameters';
    IDLabel.Caption := '';
    ParametersEdit.Text := '';
  end;
end;

procedure TSetupForm.LCDSizeComboBoxChange(Sender: TObject);
var
  LineCount: integer;
  loop: integer;
begin
  if LCDSizeComboBox.ItemIndex < 0 then LCDSizeComboBox.ItemIndex := 0;

  if ScreenSizes[LCDSizeComboBox.ItemIndex+1].XSize = 0 then
  begin
    LineCount := CustomLinesSizeEdit.Value;
    CustomLinesSizeEdit.Enabled := true;
    CustomCharsSizeEdit.Enabled := true;
  end else
  begin
    LineCount := ScreenSizes[LCDSizeComboBox.ItemIndex+1].YSize;
    CustomLinesSizeEdit.Enabled := false;
    CustomCharsSizeEdit.Enabled := false;
  end;

  if LineCount < 1 then LineCount := 1;
  DontScrollGroupBox.Height := 10 + (23 * LineCount);
  ContinueNextLineGroupBox.Height := DontScrollGroupBox.Height;

  for loop :=1 to MaxLines do
  begin
    LineEditArray[loop].Visible := False;
    LineEditArray[loop].Enabled := True;
    LineEditButtonArray[loop].Visible := False;
    LineClearButtonArray[loop].Visible := False;
    ContinueLineCheckBoxArray[loop].Visible := False;
    DontScrollLineCheckBoxArray[loop].Visible := False;
    CenterLineCheckBoxArray[loop].Visible := False;
    ShutdownEditArray[loop].Visible := False;
  end;

  for loop := 1 to LineCount do
  begin
    LineEditArray[loop].Visible := True;
    LineEditButtonArray[loop].Visible := True;
    LineClearButtonArray[loop].Visible := True;
    if (ContinueLineCheckBoxArray[loop].Checked = true) and (loop < LineCount) then LineEditArray[loop+1].Enabled := False;
    if LineEditArray[loop].Visible = True then
    begin
      LineEditButtonArray[loop].Visible := True;
      LineClearButtonArray[loop].Visible := True;
    end;
    ContinueLineCheckBoxArray[loop].Visible := True;
    DontScrollLineCheckBoxArray[loop].Visible := True;
    CenterLineCheckBoxArray[loop].Visible := True;
    ShutdownEditArray[loop].Visible := True;
  end;
  ContinueLineCheckBoxArray[loop].Visible := False;
end;

// Multi instance manager
procedure TSetupForm.MiConfigsListBoxClick(Sender: TObject);
begin
  MiConfigNameEdit.Text := ExtractFileName(MiConfigsListBox.FileName);
end;

procedure TSetupForm.MiConfigsLoadBitBtnClick(Sender: TObject);
begin
  {$IFDEF STANDALONESETUP}
  if MiConfigNameEdit.Text <> '' then
  begin
    ConfigFileName := MiConfigNameEdit.Text;
    FormShow(Sender);
  end;
  {$ENDIF}
end;

procedure TSetupForm.MiConfigsRefreshBitBtnClick(Sender: TObject);
var
  sCurrentDir: string;
begin
  // awkward shi as refresh doesnt just refresh the list
  sCurrentDir := MiConfigsListBox.Directory;
  MiConfigsListBox.Directory := '..';
  MiConfigsListBox.Directory := sCurrentDir;
  MiConfigsListBox.Refresh;
end;

procedure TSetupForm.MiConfigsTabSheetShow(Sender: TObject);
begin
  MiConfigsRefreshBitBtnClick(Sender);
end;

procedure TSetupForm.MiCopyConfigBitBtnClick(Sender: TObject);
var
  NewConfigFileName: string;
  len: integer;
  ext: string;
begin
  if MiConfigsListBox.FileName = '' then
    Exit;

  if MiConfigNameEdit.Text = '' then
  begin
    ShowMessage('Configuration file name is empty. Type a new name');
    Exit;
  end;

  len := length(MiConfigNameEdit.Text);
  ext := copy(MiConfigNameEdit.Text, len - 3, 4);
  if (ext = '.ini') then
    NewConfigFileName := ExtractFilePath(application.exename) + MiConfigNameEdit.Text
  else
    NewConfigFileName := ExtractFilePath(application.exename) +
      MiConfigNameEdit.Text + '.ini';

  if FileExists(NewConfigFileName) then
  begin
    ShowMessage('Configuration file (' + NewConfigFileName +
      ') already exists. Choose another name');
    Exit;
  end;

  CopyFile(PChar(MiConfigsListBox.FileName), PChar(NewConfigFileName), True);
  MiConfigsRefreshBitBtnClick(Sender);
end;

// create a new smartie program directory
// I tried to keep this as self contained as possible
procedure TSetupForm.InvertButtonClick(Sender: TObject);
var
  i: integer;
begin
   {$IFDEF STANDALONESETUP}
  for i := 0 to FileSelectList.Count -1 do
    FileSelectList.Checked[i] := not FileSelectList.Checked[i];
   {$ENDIF}
end;

procedure TSetupForm.SelectAllButtonClick(Sender: TObject);
var
  i: integer;
begin
  {$IFDEF STANDALONESETUP}
  for i := 0 to FileSelectList.Count -1 do
    FileSelectList.Checked[i] := true;
  {$ENDIF}
end;

procedure TSetupForm.MiCreateNewProgDirButton1Click(Sender: TObject);
{$IFDEF STANDALONESETUP}
var
  NewDir: string;
  CopyList, CopyPlugins, CopyRootFiles, TmpList: TStringList;
  selectionform: TForm;
  OkButton, CancelButton, SelectAllButton, InvertButton: TButton;
  i, j: integer;
  hConfig: longint;
  NewConfig: TConfig;
  selectdir: TSelectDirectoryDialog;
begin

  selectdir := TSelectDirectoryDialog.Create(nil);
  selectdir.Title := 'Select or create new folder';
  selectdir.InitialDir:='.';
  selectdir.Options:=[ofOldStyleDialog, ofCreatePrompt, ofEnableSizing];
  if not selectdir.Execute then
      Exit;
  NewDir := selectdir.FileName;
  selectdir.Free;

  selectionform := TForm.Create(nil);
  selectionform.SetBounds(Left, Top, 395, 300);
  selectionform.BorderStyle := bsDialog;

  // buttons
  SelectAllButton := TButton.create(selectionform);
  SelectAllButton.Caption := 'Select all';
  SelectAllButton.SetBounds(310, 140, 75, 30);
  SelectAllButton.Parent := selectionform;
  SelectAllButton.OnClick := SelectAllButtonClick;

  InvertButton := TButton.create(selectionform);
  InvertButton.Caption := 'Invert';
  InvertButton.SetBounds(310, 180, 75, 30);
  InvertButton.Parent := selectionform;
  InvertButton.OnClick := InvertButtonClick;

  OkButton := TButton.create(selectionform);
  OkButton.Caption := 'Ok';
  OkButton.SetBounds(310, 220, 75, 30);
  OkButton.Parent := selectionform;
  OkButton.ModalResult := mrOK;

  CancelButton := TButton.create(selectionform);
  CancelButton.Caption := 'Cancel';
  CancelButton.SetBounds(310, 260, 75, 30);
  CancelButton.Parent := selectionform;
  CancelButton.ModalResult := mrCancel;

  FileSelectList := TCheckListBox.Create(selectionform);
  FileSelectList.SetBounds(0, 0, 300, 300);
  FileSelectList.Parent := selectionform;

  TmpList := TStringlist.Create;
  FindAllFiles(TmpList, 'plugins', '*', false);

  FileSelectList.Clear;
  for i := 0 to TmpList.Count - 1 do
  begin
    FileSelectList.AddItem(ExtractFileName(TmpList[i]), nil);
  end;

  selectionform.Caption := 'Select plugins to copy';
  with selectionform do
    ShowModal;

  if (selectionform.ModalResult = mrCancel) then
  begin
    TmpList.Free;
    FileSelectList.Free;
    selectionform.Free;
    Exit;
  end;

  CopyPlugins := TStringlist.Create;
  for i := 0 to FileSelectList.Count -1 do
  begin
    if FileSelectList.Checked[i] then
       CopyPlugins.Add(FileSelectList.Items[i]);
  end;

  // next window
  TmpList.Clear;
  FindAllFiles(TmpList, '.', '*', false);

  FileSelectList.Clear;
  for i := 0 to TmpList.Count - 1 do
  begin
    FileSelectList.AddItem(ExtractFileName(TmpList[i]), nil);
  end;

  selectionform.Caption := 'Select other files to copy';
  with selectionform do
    ShowModal;

  if (selectionform.ModalResult = mrCancel) then
  begin // user has canceled the operation
    TmpList.Free;
    FileSelectList.Free;
    selectionform.Free;
    Exit;
  end;

  CopyRootFiles := TStringlist.Create;
  for i := 0 to FileSelectList.Count -1 do
  begin
    if FileSelectList.Checked[i] then
       CopyRootFiles.Add(FileSelectList.Items[i]);
  end;

  FileSelectList.Free;
  TmpList.Clear;

  // copy root files and create root dirs
  CopyList := TStringlist.Create;
  CopyList.AddCommaText('DNBridge.dll, LCDSmartie.exe, LCDSmartie.exe.config, LegacyLoader.exe');

  for i := 0 to CopyRootFiles.Count -1 do
    CopyList.Add(CopyRootFiles[i]);

  createdir(NewDir);
  createdir(NewDir+'\cache');
  createdir(NewDir+'\displays');
  createdir(NewDir+'\images');
  createdir(NewDir+'\openssl');
  createdir(NewDir+'\plugins');

  for i := 0 to CopyList.Count -1 do
  begin
    CopyingFileLabel.Caption := 'Copying: '+ExtractFileName(CopyList[i]);
    CopyingFileLabel.Repaint;
    copyfile(pchar(CopyList[i]), pchar(NewDir +'\'+CopyList[i]), false);
  end;

  // copy displays dir
  CopyList.Clear;
  FindAllFiles(CopyList, 'displays', '*', false);
  for i := 0 to CopyList.Count -1 do
  begin
    CopyingFileLabel.Caption := 'Copying: '+ExtractFileName(CopyList[i]);
    CopyingFileLabel.Repaint;
    copyfile(pchar(CopyList[i]), pchar(NewDir +'\'+CopyList[i]), false);
  end;

  // copy openssl dir
  CopyList.Clear;
  FindAllFiles(CopyList, 'openssl', '*', false);
  for i := 0 to CopyList.Count -1 do
  begin
    CopyingFileLabel.Caption := 'Copying: '+ExtractFileName(CopyList[i]);
    CopyingFileLabel.Repaint;
    copyfile(pchar(CopyList[i]), pchar(NewDir +'\'+CopyList[i]), false);
  end;

  // images dir is different, theres also subdirs
  CopyList.Clear;
  FindAllFiles(CopyList, 'images', '*', false);
  for i := 0 to CopyList.Count -1 do
  begin
    CopyingFileLabel.Caption := 'Copying: '+ExtractFileName(CopyList[i]);
    CopyingFileLabel.Repaint;
    copyfile(pchar(CopyList[i]), pchar(NewDir+'\'+CopyList[i]), false);
  end;

  FindAllDirectories(TmpList, 'images', false);
  for i := 0 to TmpList.Count -1 do
  begin
    CopyList.Clear;
    createdir(NewDir+'\'+TmpList[i]);
    FindAllFiles(CopyList, TmpList[i], '*', false);
    for j := 0 to CopyList.Count -1 do
    begin
      CopyingFileLabel.Caption := 'Copying: '+ExtractFileName(CopyList[i]);
      CopyingFileLabel.Repaint;
      copyfile(pchar(CopyList[j]), pchar(NewDir +'\'+CopyList[j]), false);
    end;
  end;

  // copy plugins
  CopyList.Clear;
  for i := 0 to CopyPlugins.Count -1 do
    CopyList.Add(CopyPlugins[i]);
  for i := 0 to CopyList.Count -1 do
  begin
    CopyingFileLabel.Caption := 'Copying: '+ExtractFileName(CopyList[i]);
    CopyingFileLabel.Repaint;
    copyfile(pchar('plugins\'+CopyList[i]), pchar(NewDir+'\plugins\'+CopyList[i]), false);
  end;
  CopyingFileLabel.Caption := 'Copying config';

  // copy config
  if MiCopyToNewRadioButton.Checked then
  begin
    ApplyButtonClick(Sender);
    CopyFile(pchar(config.filename), pchar(NewDir+'\config.ini'), false);
  end
  else
  begin
    hConfig := FileCreate(NewDir+'\config.ini');
    FileClose(hConfig);
    NewConfig := TConfig.Create(NewDir+'\config.ini');
    NewConfig.load();
    NewConfig.save();
    NewConfig.Free;
    NewConfig := nil;
  end;

  CopyingFileLabel.Caption := 'Done';
{$ELSE}
begin
{$ENDIF}
end;

procedure TSetupForm.MiDeleteBitBtnClick(Sender: TObject);
var
  filename: string;
begin
  if MiConfigsListBox.FileName = '' then
    Exit;

  filename := MiConfigsListBox.FileName;

  case QuestionDlg('Delete Config?', 'Sure to delete' + sLineBreak +
      filename, mtInformation, [mrYes, 'Yes', mrNo,
      'No', 'IsDefault'], '') of
    mrYes: DeleteFile(filename);
  end;
  MiConfigsRefreshBitBtnClick(Sender);
end;

procedure TSetupForm.MiNewConfigBitBtnClick(Sender: TObject);
var
  hConfig: longint;
  NewConfig: TConfig;
  NewConfigFileName: string;
  len: integer;
  ext: string;
begin
  if MiConfigNameEdit.Text = '' then
  begin
    ShowMessage('Configuration file name is empty. Type a new name');
    Exit;
  end;

  len := length(MiConfigNameEdit.Text);
  ext := copy(MiConfigNameEdit.Text, len - 3, 4);
  if (ext = '.ini') then
    NewConfigFileName := ExtractFilePath(application.exename) + MiConfigNameEdit.Text
  else
    NewConfigFileName := ExtractFilePath(application.exename) +
      MiConfigNameEdit.Text + '.ini';

  if FileExists(NewConfigFileName) then
  begin
    ShowMessage('Configuration file (' + NewConfigFileName +
      ') already exists. Choose another name');
    Exit;
  end;

  NewConfig := TConfig.Create(NewConfigFileName);
  hConfig := FileCreate(NewConfigFileName);
  if hConfig = -1 then
  begin
    FileClose(hConfig);
    ShowMessage('Configuration file (' + NewConfigFileName + ') could not be created');
    Exit;
  end;

  FileClose(hConfig);
  NewConfig.load();
  NewConfig.save();
  NewConfig.Free;
  NewConfig := nil;
  MiConfigsRefreshBitBtnClick(Sender);
end;

// EnumWindows() callback function. Enumerates windows until function returns false
function EnumWindowsProc(WHandle: HWND; LParM: LParam): longbool; stdcall;
{$IFDEF STANDALONESETUP}
var
  Title: array[0..128] of char;
  sTitle: string;
  pid: DWORD;
  ProcessesListLen: integer;
  Style: Long;
begin
  Result:=True;
  GetWindowText(wHandle, Title,128);
  GetWindowThreadProcessId( wHandle, &pid);
  sTitle:=Title;
  if (pid = LParM) then
  begin
    Style:= GetWindowLong(wHandle, GWL_STYLE);
    if (IsWindowVisible(wHandle) and IsWindow(wHandle)) or (Style = $06C80000) then
    begin
      ProcessesListLen := Length(ProcessesList);
      setLength(ProcessesList, ProcessesListLen + 1);
      ProcessesList[ProcessesListLen].WindowTitle := sTitle;
      ProcessesList[ProcessesListLen].Pid := pid;
      Result := false;
    end;
  end;
{$ELSE}
begin
{$ENDIF}
end;

procedure TSetupForm.MiRiRefreshBitBtnClick(Sender: TObject);
{$IFDEF STANDALONESETUP}
var
  ProcessLoop: BOOL;
  ProcessSnapshotHandle: THandle;
  ProcessEntry32: TProcessEntry32;
  i: integer;
begin
  ProcessSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  ProcessEntry32.dwSize := SizeOf(ProcessEntry32);
  ProcessLoop := Process32First(ProcessSnapshotHandle, ProcessEntry32);

  setLength(ProcessesList, 0);

  while Integer(ProcessLoop) <> 0 do
  begin
    if ProcessEntry32.szExeFile = 'LCDSmartie.exe' then
    begin

      EnumWindows(@EnumWindowsProc, ProcessEntry32.th32ProcessID);
    end;
    ProcessLoop := Process32Next(ProcessSnapshotHandle, ProcessEntry32);
  end;

  MiRunningInstancesListGrid.Clear;
  MiRunningInstancesListGrid.colcount := 2;
  MiRunningInstancesListGrid.rowcount := 1;
  MiRunningInstancesListGrid.FixedRows := 1;
  MiRunningInstancesListGrid.ColWidths[0] := 185;
  MiRunningInstancesListGrid.ColWidths[1] := 50;

  MiRunningInstancesListGrid.Cells[0, 0] := 'Window Title';
  MiRunningInstancesListGrid.Cells[1, 0] := 'PID';

  for i := 0 to Length(ProcessesList) -1 do
  begin
    MiRunningInstancesListGrid.RowCount := MiRunningInstancesListGrid.RowCount + 1;
    MiRunningInstancesListGrid.Cells[0, MiRunningInstancesListGrid.RowCount-1] := ProcessesList[i].WindowTitle;
    MiRunningInstancesListGrid.Cells[1, MiRunningInstancesListGrid.RowCount-1] := inttostr(ProcessesList[i].Pid);
  end;
{$ELSE}
begin
{$ENDIF}
end;

procedure TSetupForm.MiRiStopInstanceBitBtnClick(Sender: TObject);
{$IFDEF STANDALONESETUP}
var
  pid: integer;
  hnd: THandle;
begin
  pid := StrToInt(MiRunningInstancesListGrid.Cells[1, MiRunningInstancesListGrid.Row]);

  hnd := OpenProcess(PROCESS_TERMINATE, False, pid);
  if hnd > 0 then
    try
      Win32Check(TerminateProcess(hnd, 0));
    finally
      CloseHandle(hnd);
    end;
  MiRiRefreshBitBtnClick(Sender);
{$ELSE}
begin
{$ENDIF}
end;

procedure TSetupForm.MiRunningProcessesTabSheetShow(Sender: TObject);
begin
  MiRiRefreshBitBtnClick(Sender);
end;

procedure TSetupForm.MiStartupItemAddBitBtnClick(Sender: TObject);
{$IFDEF STANDALONESETUP}
var
  sParameters: string;
  pos: integer;
begin
  if (config.bAutoStartHide) then
    sParameters := '-hide ';

  sParameters := sParameters + '-config '+ '"' + ExtractFileName(config.filename) + '"';
  pos := 1;
  CreateShortcut('LCD Smartie '+ExtractSubstr(ExtractFileName(ConfigFileName), pos, ['.']), ExtractFilePath(Application.ExeName)+'LCDSmartie.exe' , sParameters, False);
  MiStartupItemsRefreshBitBtnClick(Sender);
{$ELSE}
begin
{$ENDIF}
end;

procedure TSetupForm.MiStartupItemRemoveBitBtnClick(Sender: TObject);
var
  i: integer;
  empty: string;
begin
  for i := 0 to MiStartupItemsCheckListBox.Count - 1 do
  begin
    if MiStartupItemsCheckListBox.Checked[i] then
    begin
      CreateShortcut(MiStartupItemsCheckListBox.Items[i], empty, empty, True);
    end;
  end;
  MiStartupItemsRefreshBitBtnClick(Sender);
end;

procedure TSetupForm.MiStartupItemsLaunchBitBtnClick(Sender: TObject);
var
  R: TRegistry;
  Dir: string;
  i: integer;
begin
  R := TRegistry.Create(KEY_READ);
  R.RootKey := HKEY_CURRENT_USER;
  R.OpenKeyReadOnly('Software\MicroSoft\Windows\CurrentVersion\Explorer\Shell Folders\');
  Dir := R.ReadString('Startup');

  for i := 0 to MiStartupItemsCheckListBox.Count - 1 do
  begin
    if MiStartupItemsCheckListBox.Checked[i] then
    begin
      ShellExecute(Handle, 'open',
        PChar(Dir + '\' + MiStartupItemsCheckListBox.Items[i] + '.lnk'), '',
        PChar(ExtractFilePath(Application.ExeName)), SW_SHOWNORMAL);
    end;
  end;

  R.Free;
end;

procedure TSetupForm.MiStartupItemsRefreshBitBtnClick(Sender: TObject);
var
  R: TRegistry;
  Dir: string;
  Items: TStringList;
  i: integer;
begin
  R := TRegistry.Create(KEY_READ);
  R.RootKey := HKEY_CURRENT_USER;
  R.OpenKeyReadOnly('Software\MicroSoft\Windows\CurrentVersion\Explorer\Shell Folders\');
  Dir := R.ReadString('Startup');
  Items := TStringList.Create;
  FindAllFiles(Items, Dir, 'LCD Smartie*.lnk', False);
  MiStartupItemsCheckListBox.Clear;
  for i := 0 to Items.Count - 1 do
  begin
    MiStartupItemsCheckListBox.AddItem(ExtractFileNameOnly(
      ExtractFileName(Items[i])), nil);
  end;
  Items.Free;
  R.Free;
end;

procedure TSetupForm.MiStartupItemsTabSheetShow(Sender: TObject);
begin
  MiStartupItemsRefreshBitBtnClick(Sender);
end;

procedure TSetupForm.RssPageChange(Sender: TObject);
var
  feeditem: string;
begin
  if RSSAddressTMemoEdit.Text <> '' then
  begin
    case RssTypeComboBox.ItemIndex of
      0: feeditem := 't';
      1: feeditem := 'd';
      2: feeditem := 'b';
    end;
    VariableEdit.Text := '$Rss(' + RSSAddressTMemoEdit.Text + ',' + feeditem + ',' +
      IntToStr(RssItemNumSpinEdit.Value) + ',' + IntToStr(RssMaxFreqSpinedit.Value) + ')';
  end;
end;
////// end multi instance manager

procedure TSetupForm.SaveScreen(scr: integer);
var
  loop: integer;
begin
  if scr = 0 then Exit;
  for loop := 1 to MaxLines do
  begin
    config.screen[scr].line[loop].Text := LineEditArray[loop].Text;
    config.screen[scr].line[loop].center := CenterLineCheckBoxArray[loop].Checked;
    config.screen[scr].line[loop].noscroll := DontScrollLineCheckBoxArray[loop].Checked;
    config.screen[scr].line[loop].contNextLine := ContinueLineCheckBoxArray[loop].Checked;
  end;

  config.screen[scr].settings.Enabled := ScreenEnabledCheckBox.Checked;
  try
    config.screen[scr].settings.theme := ThemeNumberSpinEdit.Value - 1;
  except
    config.screen[scr].settings.theme := 0;
  end;
  try
    config.screen[scr].settings.showTime := TimeToShowSpinEdit.Value;
  except
    config.screen[scr].settings.showTime := 10;
  end;
  config.screen[scr].settings.bSticky := StickyCheckbox.Checked;

{$IFNDEF STANDALONESETUP}
  LCDSmartieDisplayForm.ResetScrollPositions();
{$ENDIF}

  config.screen[scr].settings.TransitionStyle :=
    TTransitionStyle(TransitionStyleComboBox.ItemIndex);
  config.screen[scr].settings.TransitionTime := TransitionTimeSpinEdit.Value;
end;

function CustomStringToCustomArray(CString: String): TCustomArray;
var
  CustomArray: TCustomArray;
  i: Integer;
  iPosStart, iPosEnd: Integer;
begin
  CustomArray := default(TCustomArray);
  if pos('$CustomChar(', CString) > 0 then
  begin
  iPosEnd := pos(',', CString);
  iPosEnd := pos(',', CString, iPosEnd);
  for i := 0 to 6 do
  begin
    iPosStart := iPosEnd + 1;
    iPosEnd := PosEx(',', CString, iPosStart);
    try
      CustomArray[i] := StrToIntN(CString, iPosStart, iPosEnd-iPosStart);
    except
      exit;
    end;
  end;
  iPosStart := iPosEnd + 1;
  iPosEnd := pos(')', CString, iPosStart);
  CustomArray[7] := StrToIntN(CString, iPosStart, iPosEnd-iPosStart);
  end;
  result := CustomArray;
end;

procedure TSetupForm.LoadScreen(scr: integer);
var
  ascreen: TScreen;
  loop: integer;
begin
  ascreen := config.screen[scr];
  ScreenEnabledCheckBox.Checked := ascreen.settings.Enabled;
  ThemeNumberSpinEdit.Value := ascreen.settings.theme + 1;
  TimeToShowSpinEdit.Value := ascreen.settings.showTime;
  StickyCheckbox.Checked := ascreen.settings.bSticky;
  TimeToShowSpinEdit.Enabled := not ascreen.settings.bSticky;

  for loop := 1 to MaxLines do
  begin
    DontScrollLineCheckBoxArray[loop].Enabled := True;
    DontScrollLineCheckBoxArray[loop].Checked := False;
    ContinueLineCheckBoxArray[loop].Checked := False;
    LineEditArray[loop].Enabled := True;
    LineEditArray[loop].color := clWhite;
  end;

  Line1MemoEdit.color := $00A1D7A4;
  setupbutton := 1;
  GameServerEdit.Text := config.gameServer[scr, 1];

  ascreen := config.screen[scr];

  if assigned(CCharDefForm) then
  begin
    for loop := 1 to 8 do
      CCharDefForm.EditArray[loop].Text := ascreen.CustomCharacters[loop];
  end;

  for loop := 1 to MaxLines do
  begin
    if assigned(FormEditArray[Loop]) then
    begin
      FormEditArray[Loop].Memo1.Clear;
      FormEditArray[Loop].Memo1.Lines.Add(ascreen.line[loop].Text);
    end;

    if assigned(CCharDefForm) then
    begin
      CCharDefForm.EditArray[Loop].Text:=ascreen.CustomCharacters[loop];
    end;

    LineEditArray[loop].Text := ascreen.line[loop].Text;
    CenterLineCheckBoxArray[loop].Checked := ascreen.line[loop].center;
    DontScrollLineCheckBoxArray[loop].Checked := ascreen.line[loop].noscroll;
    if ascreen.line[loop].contNextLine and (loop < MaxLines) then
    begin
      ContinueLineCheckBoxArray[loop].Checked := True;
      DontScrollLineCheckBoxArray[loop].Checked := True;
      DontScrollLineCheckBoxArray[loop].Enabled := False;
      LineEditArray[loop+1].Enabled := False;
      LineEditArray[loop+1].color := $00BBBBFF;
    end;
  end;
  LoadCCharSpeedButtonGlyphs;
  TransitionStyleComboBox.ItemIndex := Ord(ascreen.settings.TransitionStyle);
  TransitionTimeSpinEdit.Value := ascreen.settings.TransitionTime;
end;

procedure TSetupForm.ScreenSpinEditChange(Sender: TObject);
begin
  SaveScreen(CurrentScreen);

  try
    CurrentScreen := max(1, min(MaxScreens, ScreenSpinEdit.Value));
  except
    CurrentScreen := 1;
  end;
  LoadScreen(CurrentScreen);
{$IFNDEF STANDALONESETUP}
  LCDSmartieDisplayForm.ChangeScreen(CurrentScreen);
{$ENDIF}
end;

procedure TSetupForm.UseTaskSchedulerCheckBoxChange(Sender: TObject);
begin
  if not IsAdministrator then
    ShowMessage('Use task scheduler will not work Unless LCD Smartie is running as administrator');

end;

procedure TSetupForm.WinampListBoxClick(Sender: TObject);
var
  WinampStat: TWinampStat;
begin
  WinampStat := TWinampStat(WinampListBox.ItemIndex);
  if (WinampStat >= FirstWinampStat) and (WinampStat <= LastWinampStat) then
  begin
    VariableEdit.Text := WinampKeys[WinampStat];
    if (WinampStat = wsWinampPosition) then
      // special case, should be resolved elsewhere
      VariableEdit.Text := VariableEdit.Text + '(10)';
  end
  else
    VariableEdit.Text := NoVariable;

  //if not (VariableEdit.Text = NoVariable) then
  //  FocusToInputField();
end;


// Select currently active text field that will receive variable if 'insert'
// is pressed.
// I dont think this is neccessary. Also it prevents the arrow keys from being used to move through list boxes
procedure TSetupForm.FocusToInputField;
var
  tempint1, tempint2: integer;
  loop: integer;
begin
  if (ScreensTabSheet.Visible) then // in Screens tab
  begin
    for loop := 1 to MaxLines do
      if (loop = setupbutton) and (LineEditArray[loop].Enabled) and (LineEditArray[loop].Visible) then
      begin
      tempint1 := LineEditArray[loop].SelStart;
      tempint2 := LineEditArray[loop].SelLength;
      LineEditArray[loop].SetFocus;
      LineEditArray[loop].SelStart := tempint1;
      LineEditArray[loop].SelLength := tempint2;
      end;
  end;
end;

procedure TSetupForm.InsertButtonClick(Sender: TObject);
var
  tempint, loop, sp, ep: integer;
begin
  if VariableEdit.Text <> NoVariable then
  begin
    if setupbutton > (MaxLines * 2) then // in custom char define form
    begin
      if pos('$CustomChar', VariableEdit.Text) > 0 then
      begin
        ep := pos(',', VariableEdit.Text);
        loop := StrToIntN(VariableEdit.Text, 13, ep - 13);
        CCharDefForm.EditArray[loop].Text := VariableEdit.Text;
      end;
    end
    else if setupbutton > MaxLines then  // in a line edit form
    begin
    for loop := 1 to MaxLines do
    begin
      if assigned(FormEditArray[Loop]) then
      begin
        if Loop = setupbutton - MaxLines then
        begin
          tempint := FormEditArray[Loop].Memo1.SelStart;
          FormEditArray[Loop].Memo1.Text := utf8copy(FormEditArray[Loop].Memo1.Text, 1, FormEditArray[Loop].Memo1.SelStart) +
            VariableEdit.Text + utf8copy(FormEditArray[Loop].Memo1.Text, FormEditArray[Loop].Memo1.SelStart +
            1 + FormEditArray[Loop].Memo1.SelLength, UTF8Length(FormEditArray[Loop].Memo1.Text));
          FormEditArray[Loop].Memo1.SetFocus;
          FormEditArray[Loop].Memo1.selstart := tempint + utf8length(VariableEdit.Text);
        end;
      end;
    end;
    end
    else if (ScreensTabSheet.Visible) then // in Screens tab
    begin
      for loop := 1 to MaxLines do
      begin
        if (loop = setupbutton) and (LineEditArray[loop].Enabled) and (LineEditArray[loop].Visible) then
        begin
          tempint := LineEditArray[loop].SelStart;
          LineEditArray[loop].Text := utf8copy(LineEditArray[loop].Text, 1, LineEditArray[loop].SelStart) +
          VariableEdit.Text + utf8copy(LineEditArray[loop].Text, LineEditArray[loop].SelStart +
            1 + LineEditArray[loop].SelLength, UTF8Length(LineEditArray[loop].Text));
            LineEditArray[loop].SetFocus;
          LineEditArray[loop].selstart := tempint + utf8length(VariableEdit.Text);
        end;
      end;
    end
    else if (ActionsTabSheet.Visible) then // in Actions tab
    begin
      if (LastKeyPressedEdit.Text = '') and (VariableEdit.Text = '$MObutton') then
      begin
        ShowMessage('please press the button you want to bind');
      end
      else
      begin
        if pos('$MObutton', VariableEdit.Text) <> 0 then
          VariableEdit.Text := '$MObutton(' + LastKeyPressedEdit.Text + ')';
        ActionsStringGrid.Cells[2, ActionsStringGrid.row] := VariableEdit.Text;
      end;
    end
    else if (StartupTabSheet.Visible) then // in startup/shutdown tab
    begin
      for loop := 1 to MaxLines do
      begin
        if (loop = shdownmessagebutton) and (ShutdownEditArray[loop].Enabled) and (ShutdownEditArray[loop].Visible) then
        begin
          tempint := ShutdownEditArray[loop].SelStart;
          ShutdownEditArray[loop].Text := utf8copy(ShutdownEditArray[loop].Text, 1, ShutdownEditArray[loop].SelStart) +
          VariableEdit.Text + utf8copy(ShutdownEditArray[loop].Text, ShutdownEditArray[loop].SelStart +
            1 + ShutdownEditArray[loop].SelLength, UTF8Length(ShutdownEditArray[loop].Text));
            ShutdownEditArray[loop].SetFocus;
          ShutdownEditArray[loop].selstart := tempint + utf8length(VariableEdit.Text);
        end;
      end;
    end;
  end;
end;

procedure TSetupForm.SysInfoListBoxClick(Sender: TObject);
begin
  case SysInfoListBox.ItemIndex of
    0: VariableEdit.Text := '$SysUsername';
    1: VariableEdit.Text := '$SysComputername';
    2: VariableEdit.Text := '$SysCPUType';
    3: VariableEdit.Text := '$SysCPUSpeedMhz';
    4: VariableEdit.Text := '$SysCPUSpeedGhz';
    5: VariableEdit.Text := '$SysCPUUsage';
    6: VariableEdit.Text := '$Bar($SysCPUUsage,100,10)';
    7: VariableEdit.Text := '$SysCPUCoreUsage(0, _Total)';
    8: VariableEdit.Text := '$SysCPUCoreSpeed(0, _Total)';
    9: VariableEdit.Text := '$SysUptime';
    10: VariableEdit.Text := '$SysUptims';
    11: VariableEdit.Text := '$MemFree';
    12: VariableEdit.Text := '$MemUsed';
    13: VariableEdit.Text := '$MemTotal';
    14: VariableEdit.Text := '$MemF%';
    15: VariableEdit.Text := '$MemU%';
    16: VariableEdit.Text := '$Bar($MemFree,$MemTotal,10)';
    17: VariableEdit.Text := '$Bar($MemUsed,$MemTotal,10)';
    18: VariableEdit.Text := '$PageFree';
    19: VariableEdit.Text := '$PageUsed';
    20: VariableEdit.Text := '$PageTotal';
    21: VariableEdit.Text := '$PageF%';
    22: VariableEdit.Text := '$PageU%';
    23: VariableEdit.Text := '$Bar($PageFree,$PageTotal,10)';
    24: VariableEdit.Text := '$Bar($PageUsed,$PageTotal,10)';
    25: VariableEdit.Text := '$HDFree(C)';
    26: VariableEdit.Text := '$HDUsed(C)';
    27: VariableEdit.Text := '$HDTotal(C)';
    28: VariableEdit.Text := '$HDFreg(C)';
    29: VariableEdit.Text := '$HDUseg(C)';
    30: VariableEdit.Text := '$HDTotag(C)';
    31: VariableEdit.Text := '$HDF%(C)';
    32: VariableEdit.Text := '$HDU%(C)';
    33: VariableEdit.Text := '$Bar($HDFree(C),$HDTotal(C),10)';
    34: VariableEdit.Text := '$Bar($HDUsed(C),$HDTotal(C),10)';
    35: VariableEdit.Text := '$ScreenReso';
    36: VariableEdit.Text := '$SysSSActive';
    37: VariableEdit.Text := '$SysFSGameActive';
    38: VariableEdit.Text := '$SysFSAppActive';
    39: VariableEdit.Text := '$SysAppActive(LCDSmartie.exe)';
    else
      VariableEdit.Text := NoVariable;
  end;
end;

procedure TSetupForm.InternetListBoxClick(Sender: TObject);
begin
  VariableEdit.Text := NoVariable;
  if InternetListBox.ItemIndex >= 0 then
  begin
    RSSNameEdit.Text := config.RSSList.Names[InternetListBox.ItemIndex];
    RSSAddressTMemoEdit.Text := config.RSSList.Addresses[InternetListBox.ItemIndex];
  end;
end;

procedure TSetupForm.QStatLabelClick(Sender: TObject);
begin
  ShellExecute(0, nil, PChar('www.qstat.org'), nil, nil, SW_NORMAL);
end;

procedure TSetupForm.MiscListBoxClick(Sender: TObject);
begin
  case MiscListBox.ItemIndex of
    0: VariableEdit.Text := '$DnetSpeed';
    1: VariableEdit.Text := '$DnetDone';
    2: VariableEdit.Text := '$Time(d mmmm yyyy hh: nn: ss)';
    3: VariableEdit.Text := '°';
    4: VariableEdit.Text := '█';
    5: VariableEdit.Text := '$Chr(20)';
    6: VariableEdit.Text := '$File(C:\file.txt,1)';
    7: VariableEdit.Text := '$LogFile(C:\file.log,0)';
    8: VariableEdit.Text := '$dll(demo.dll,5,param1,param2)';
    9: VariableEdit.Text := '$Count(101#$CPUSpeed#4)';
    10: VariableEdit.Text := '$Bar(30,100,20)';
    11: VariableEdit.Text := '$Right(ins variable(s) here,$3%)';
    12: VariableEdit.Text := '$Fill(10)';
    13: VariableEdit.Text := '$Flash(insert text here$)$';
    14: VariableEdit.Text := '$CustomChar(1, 31, 31, 31, 31, 31, 31, 31, 31)';
    15: VariableEdit.Text := '$Rss(URL,t|d|b,ITEM#,MAXFREQHRS)';
    16: VariableEdit.Text := '$Center(text here,15)';
    17: VariableEdit.Text := '$ScreenChanged';
    18: VariableEdit.Text := '$Sender(127.0.0.10,6088,password1234,1,1)';
    19: VariableEdit.Text := '$Store(Some Text,0)';
    20: VariableEdit.Text := '$Fetch(0)';
    21: VariableEdit.Text := '$Round(3.14159265359,3)';
    22: VariableEdit.Text := '$Add(3,2)';
    23: VariableEdit.Text := '$Sub(5,3)';
    24: VariableEdit.Text := '$Mul(2,5)';
    25: VariableEdit.Text := '$Div(10,2)';
    26: VariableEdit.Text := '$ActionEnabled(1)';
    else
      VariableEdit.Text := NoVariable;
  end; // case

  //if not (VariableEdit.Text = NoVariable) then
  //  FocusToInputField();
end;

procedure TSetupForm.BOINCListBoxClick(Sender: TObject);
begin
  case BOINCListBox.ItemIndex of
    0: VariableEdit.Text := '$BOINCid('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+')';
    1: VariableEdit.Text := '$BOINCcpid('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+')';
    2: VariableEdit.Text := '$BOINCcreate_time('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+')';
    3: VariableEdit.Text := '$BOINCname('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+')';
    4: VariableEdit.Text := '$BOINCcountry('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+')';
    5: VariableEdit.Text := '$BOINCtotal_credit('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+')';
    6: VariableEdit.Text := '$BOINCexpavg_credit('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+')';
    7: VariableEdit.Text := '$BOINCexpavg_time('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+')';
    8: VariableEdit.Text := '$BOINCteamid('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+')';
    9: VariableEdit.Text := '$BOINCurl('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+')';
    10: VariableEdit.Text := '$BOINCHostid('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    11: VariableEdit.Text := '$BOINCHostcreate_time('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    12: VariableEdit.Text := '$BOINCHostrpc_seqno('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    13: VariableEdit.Text := '$BOINCHostrpc_time('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    14: VariableEdit.Text := '$BOINCHosthost_cpid('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    15: VariableEdit.Text := '$BOINCHosttotal_credit('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    16: VariableEdit.Text := '$BOINCHostexpavg_credit('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    17: VariableEdit.Text := '$BOINCHostexpavg_time('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    18: VariableEdit.Text := '$BOINCHostdomain_name('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    19: VariableEdit.Text := '$BOINCHostp_ncpus('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    20: VariableEdit.Text := '$BOINCHostp_vendor('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    21: VariableEdit.Text := '$BOINCHostp_model('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    22: VariableEdit.Text := '$BOINCHostp_fpops('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    23: VariableEdit.Text := '$BOINCHostp_iops('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    24: VariableEdit.Text := '$BOINCHostos_name('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    25: VariableEdit.Text := '$BOINCHostos_version('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    26: VariableEdit.Text := '$BOINCHostm_nbytes('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    27: VariableEdit.Text := '$BOINCHostd_free('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    28: VariableEdit.Text := '$BOINCHostd_total('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    29: VariableEdit.Text := '$BOINCHostvenue('+inttostr(BoincServerIndexComboBox.ItemIndex+1)+',host index)';
    else
      VariableEdit.Text := NoVariable;
  end; // case

  //if not (VariableEdit.Text = NoVariable) then
  //  FocusToInputField();
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
  if LeftPageControl.ActivePage = BOINCTabSheet then
    BOINCListBoxClick(Sender);
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
    config.gameServer[ScreenSpinEdit.Value, setupbutton] := GameServerEdit.Text;
  end;
end;

procedure TSetupForm.DistributedNetBrowseButtonClick(Sender: TObject);
var
  line, line2: string;
begin
  // remove duplicate backslash
  line := DistributedNetLogfileEdit.Text;
  line2 := '';
  while pos('\', line) <> 0 do
  begin
    line2 := line2 + copy(line, 1, pos('\', line));
    line := copy(line, pos('\', line) + 1, length(line));
  end;
  opendialog2.InitialDir := line2;
  opendialog2.FileName := DistributedNetLogfileEdit.Text;
  Opendialog2.Execute;
  if opendialog2.FileName <> '' then DistributedNetLogfileEdit.Text :=
      opendialog2.FileName;
end;

procedure TSetupForm.EmailAccountComboBoxChange(Sender: TObject);
begin
  config.pop[CurrentlyShownEmailAccount + 1].server := EmailServerEdit.Text;
  config.pop[CurrentlyShownEmailAccount + 1].user := EmailLoginEdit.Text;
  config.pop[CurrentlyShownEmailAccount + 1].pword := EmailPasswordEdit.Text;
  config.pop[CurrentlyShownEmailAccount + 1].port_ssl := EmailSSLEdit.Text;

  if EmailAccountComboBox.ItemIndex < 0 then EmailAccountComboBox.ItemIndex := 0;

  CurrentlyShownEmailAccount := EmailAccountComboBox.ItemIndex;
  EmailServerEdit.Text := config.pop[CurrentlyShownEmailAccount + 1].server;
  EmailLoginEdit.Text := config.pop[CurrentlyShownEmailAccount + 1].user;
  EmailPasswordEdit.Text := config.pop[CurrentlyShownEmailAccount + 1].pword;
  EmailSSLEdit.Text := config.pop[CurrentlyShownEmailAccount + 1].port_ssl;

  if EmailMessageCountRadioButton.Checked then
    VariableEdit.Text := '$Email(' + IntToStr(CurrentlyShownEmailAccount + 1) + ')'
  else if EmailLastSubjectRadioButton.Checked then
    VariableEdit.Text := '$EmailSub(' + IntToStr(CurrentlyShownEmailAccount + 1) + ')'
  else if EmailLastFromRadioButton.Checked then
    VariableEdit.Text := '$EmailFrom(' + IntToStr(CurrentlyShownEmailAccount + 1) + ')';

  //if not (VariableEdit.Text = NoVariable) then
  //  FocusToInputField();
end;

procedure TSetupForm.ContinueLineCheckBoxClick(Sender: TObject);
var
  loop: integer;
begin
  for loop := 1 to MaxLines do
  begin
    if ContinueLineCheckBoxArray[loop].Checked = true then
    begin
      DontScrollLineCheckBoxArray[loop].Checked := True;
      DontScrollLineCheckBoxArray[loop].Enabled := False;
      if loop < MaxLines then
      begin
      LineEditArray[loop+1].Enabled := False;
      LineEditArray[loop+1].color := $00BBBBFF;
      end;
    end
    else
    begin
      DontScrollLineCheckBoxArray[loop].Enabled := True;
      DontScrollLineCheckBoxArray[loop].Checked := False;
      if loop < MaxLines then
      begin
      LineEditArray[loop+1].Enabled := True;
      LineEditArray[loop+1].color := clWhite;
      end;
    end;
  end;
end;

procedure TSetupForm.WinampLocationBrowseButtonClick(Sender: TObject);
begin
  opendialog1.Execute;
  if opendialog1.FileName <> '' then WinampLocationEdit.Text := opendialog1.FileName;
end;

procedure TSetupForm.GamestatsListBoxClick(Sender: TObject);
begin
  case GameTypeComboBox.ItemIndex of
    0: VariableEdit.Text := '$Half-life';
    1: VariableEdit.Text := '$QuakeII';
    2: VariableEdit.Text := '$QuakeIII';
    3: VariableEdit.Text := '$Unreal';
    else
      VariableEdit.Text := NoVariable;
  end; // case

  {if not (S = NoVariable) then
  begin
    VariableEdit.Text := S + IntToStr(GamestatsListBox.ItemIndex + 1);
    FocusToInputField();
  end
  else
    VariableEdit.Text := S; }
end;

procedure TSetupForm.LineEditEnter(Sender: TObject);
var
  loop: integer;
begin
  for loop := 1 to MaxLines do
  begin
    if (LineEditArray[loop] <> Sender) and LineEditArray[loop].Enabled = True then
      LineEditArray[loop].color := clWhite
    else
    if LineEditArray[loop].Enabled = True then
    begin
      LineEditArray[loop].color := $00A1D7A4;
      setupbutton := loop;
    end;
  end;
end;

procedure TSetupForm.NetworkStatsListBoxClick(Sender: TObject);
var
  NetStat: TNetworkStatistics;
begin
  NetStat := TNetworkStatistics(NetworkStatsListBox.ItemIndex);
  if (NetStat >= FirstNetworkStat) and (NetStat <= LastNetworkStat) then
  begin
    VariableEdit.Text := NetworkStatisticsKeys[NetStat] + '(1)';
  end
  else
    VariableEdit.Text := NoVariable;

  //if not (VariableEdit.Text = NoVariable) then
  //  FocusToInputField();
end;

procedure TSetupForm.FoldingAtHomeListBoxClick(Sender: TObject);
begin
  case FoldingAtHomeListBox.ItemIndex of
    0: VariableEdit.Text := '$FOLDUser';
    1: VariableEdit.Text := '$FOLDwu';
    2: VariableEdit.Text := '$FOLDlastwu';
    3: VariableEdit.Text := '$FOLDact50min';
    4: VariableEdit.Text := '$FOLDactweek';
    5: VariableEdit.Text := '$FOLDscore';
    6: VariableEdit.Text := '$FOLDrank';
    7: VariableEdit.Text := '$FOLDteamname';
    8: VariableEdit.Text := '$FOLDteamscore';
    9: VariableEdit.Text := '$FOLDteamwu';
    10: VariableEdit.Text := '$FOLDteamlastwu';
    else
      VariableEdit.Text := NoVariable;
  end; // case

  //if not (VariableEdit.Text = NoVariable) then
  //  FocusToInputField();
end;

// for detecting Enter being pressed in a list box
procedure TSetupForm.ListBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    InsertButtonClick(nil);
end;

procedure TSetupForm.Splitter1ChangeBounds(Sender: TObject);
begin
  if Splitter1.Left < LeftPageControl.Constraints.MinWidth then
     Splitter1.Left := LeftPageControl.Width;
  self.Update;
end;

// Apply pressed.
procedure TSetupForm.ApplyButtonClick(Sender: TObject);
var
  ReInitLCD, ReloadSkin: boolean;
  x, y: integer;
  iMaxUsedRow: integer;
  loop: integer;
begin
  ReInitLCD := False;
  if (DisplayPluginList.Caption <> config.DisplayDLLName) or (ParametersEdit.Caption <> config.DisplayDLLParameters) or
    (LCDSizeComboBox.ItemIndex +1  <> config.ScreenSize) or (CustomLinesSizeEdit.Value <> config.Custom_Height) or (CustomCharsSizeEdit.Value <> config.Custom_width) then
    ReInitLCD := True;
  config.Custom_height := CustomLinesSizeEdit.Value;
  config.Custom_width := CustomCharsSizeEdit.Value;
  ReloadSkin := False;

  config.ActionIndexCol :=  ActionsStringGrid.ColWidths[0];
  config.ActionIfCol := ActionsStringGrid.ColWidths[1];
  config.ActionVariableCol := ActionsStringGrid.ColWidths[2];
  config.ActionIsCol := ActionsStringGrid.ColWidths[3];
  config.ActionValueCol := ActionsStringGrid.ColWidths[4];
  config.ActionThenCol := ActionsStringGrid.ColWidths[5];
  config.ActionActionCol := ActionsStringGrid.ColWidths[6];
  config.ActionEnabledCol := ActionsStringGrid.ColWidths[7];

  iMaxUsedRow := -1;
  y := 0;
  for x := 1 to ActionsStringGrid.RowCount - 1 do
  begin
    if (ActionsStringGrid.cells[2, x] <> '') and
       (ActionsStringGrid.cells[3, x] <> '') and
       (ActionsStringGrid.cells[4, x] <> '') and
       (ActionsStringGrid.cells[6, x] <> '') and
       (ActionsStringGrid.cells[7, x] <> '') then
    begin
      iMaxUsedRow := y;
      config.actionsArray[y + 1, 1] := ActionsStringGrid.Cells[2, x];

      if ActionsStringGrid.Cells[3, x] = '>' then
        config.actionsArray[y + 1, 2] := '0';
      if ActionsStringGrid.Cells[3, x] = '<' then
        config.actionsArray[y + 1, 2] := '1';
      if ActionsStringGrid.Cells[3, x] = '=' then
        config.actionsArray[y + 1, 2] := '2';
      if ActionsStringGrid.Cells[3, x] = '<=' then
        config.actionsArray[y + 1, 2] := '3';
      if ActionsStringGrid.Cells[3, x] = '>=' then
        config.actionsArray[y + 1, 2] := '4';
      if ActionsStringGrid.Cells[3, x] = '<>' then
        config.actionsArray[y + 1, 2] := '5';

      config.actionsArray[y + 1, 3] := ActionsStringGrid.Cells[4, x];
      config.actionsArray[y + 1, 4] := ActionsStringGrid.Cells[6, x];
      config.actionsArray[y + 1, 5] := ActionsStringGrid.Cells[7, x];
      inc(y);
    end;
  end;
  config.totalactions := iMaxUsedRow + 1;

{$IFNDEF STANDALONESETUP}
  LCDSmartieDisplayForm.WinampCtrl1.WinampLocation := WinampLocationEdit.Text;
{$ENDIF}
  config.winampLocation := WinampLocationEdit.Text;
  config.refreshRate := ProgramRefreshIntervalSpinEdit.Value;
  config.ScreenSize := LCDSizeComboBox.ItemIndex + 1;
  config.Custom_Height := CustomLinesSizeEdit.Value;
  config.Custom_width := CustomCharsSizeEdit.Value;
  config.randomScreens := RandomizeScreensCheckBox.Checked;
  config.foldUserid := FoldingAtHomeEmailEdit.Text;
  config.gameRefresh := GamestatsRefreshTimeSpinEdit.Value;
  config.colorOption := ColorSchemeComboBox.ItemIndex;
  config.distLog := DistributedNetLogfileEdit.Text;
  config.dllPeriod := DLLCheckIntervalSpinEdit.Value;
  config.emailPeriod := EmailCheckTimeSpinEdit.Value;
  config.scrollPeriod := ProgramScrollIntervalSpinEdit.Value;
  config.alwaysOnTop := StayOnTopCheckBox.Checked;
  config.bHideOnStartup := HideOnStartup.Checked;
  config.bAutoStart := AutoStart.Checked;
  config.bAutoStartHide := AutoStartHide.Checked;
  config.bStartAsAdmin := StartAsAdminCheckBox.Checked;
  config.bUseTaskScheduler := UseTaskSchedulerCheckBox.Checked;
  config.EmulateLCD := EmulateLCDCheckbox.Checked;

  for loop := 1 to MaxLines do
    config.ShutdownMessage[loop] := ShutdownEditArray[loop].text;

  config.DLL_contrast := ContrastTrackBar.position;
  config.DLL_brightness := BrightnessTrackBar.position;
  config.DisplayDLLParameters := ParametersEdit.Text;
  config.DisplayDLLName := DisplayPluginList.Text;

  if (config.DisplayDLLName = 'None') then
    config.DisplayDLLName := '';
{$IFNDEF STANDALONESETUP}
  LCDSmartieDisplayForm.SetupAutoStart();
{$ENDIF}

  config.pop[(EmailAccountComboBox.ItemIndex + 1) mod MaxEmailAccounts].server :=
    EmailServerEdit.Text;
  config.pop[(EmailAccountComboBox.ItemIndex + 1) mod MaxEmailAccounts].user :=
    EmailLoginEdit.Text;
  config.pop[(EmailAccountComboBox.ItemIndex + 1) mod MaxEmailAccounts].pword :=
    EmailPasswordEdit.Text;
  config.pop[(EmailAccountComboBox.ItemIndex + 1) mod MaxEmailAccounts].port_ssl :=
    EmailSSLEdit.Text;

  if not (WebProxyPortEdit.Text = '') then
    config.httpProxyPort := StrToInt(WebProxyPortEdit.Text);
  config.httpProxy := WebProxyServerEdit.Text;

  SaveScreen(ScreenSpinEdit.Value);
  {$IFNDEF STANDALONESETUP}
  LCDSmartieDisplayForm.ScrollFlashTimer.interval := config.scrollPeriod;
  LCDSmartieDisplayForm.Data.RefreshDataThreads;
  {$ENDIF}
  config.LastTabIndex := LeftPageControl.ActivePageIndex;
  config.LastMainTabIndex := MainPageControl.ActivePageIndex;
  if config.sSkinPath <> SkinSelectComboBox.Text then ReloadSkin := True;
  if config.sTrayIcon <> IconSelectComboBox.Text then ReloadSkin := True;
  config.sSkinPath := SkinSelectComboBox.Text;
  config.sTrayIcon := IconSelectComboBox.Text;
  {$IFNDEF STANDALONESETUP}
  if ReloadSkin then
  begin
    LCDSmartieDisplayForm.LoadSkin();
    LCDSmartieDisplayForm.LoadColors();
  end;
  {$ENDIF}
  config.EnableRemoteSend := EnableRemoteSendCheckBox.Checked;
  config.RemoteSendBindIP := RemoteSendBindIPEdit.Text;
  config.RemoteSendPort := RemoteSendPortEdit.Text;
  config.RemoteSendPassword := RemoteSendPasswordEdit.Text;
  config.RemoteSendUseSSL := RemoteSendUseSSLCheckBox.Checked;

  config.AppendConfigName := AppendConfigNameCheckBox.Checked;
  Config.MainFormCaption := CustomTitleTIEdit1.Text;
  config.SettingsFormPosTop := SetupForm.Top;
  config.SettingsFormPosLeft := SetupForm.Left;
  config.SettingsFormPosHeight := SetupForm.Height;
  config.SettingsFormPosWidth := SetupForm.Width;
  config.ActionsTimer := ActionsTimerSpinEdit.Value;

  config.ShowNetStats := ShowNetStatsCheckBox.Checked;
  config.ShowMisc := ShowMiscCheckBox.Checked;
  config.ShowLCDFeatures := ShowLCDFeaturesCheckBox.Checked;
  config.ShowPlugins := ShowPluginsCheckBox.Checked;
  config.ShowCharEditor := ShowCharEditorCheckBox.Checked;
  config.ShowPerf := ShowPerfCheckBox.Checked;
  config.ShowSysinfo := ShowSysinfoCheckBox.Checked;
  config.ShowWinamp := ShowWinampCheckBox.Checked;
  config.ShowRSS := ShowRssCheckBox.Checked;
  config.ShowGameStats := ShowGameStatsCheckBox.Checked;
  config.ShowBOINC := ShowBOINCCheckBox.Checked;
  config.ShowFoldingAtHome := ShowFoldingAtHomeCheckBox.Checked;
  config.ShowEmail := ShowEmailCheckBox.Checked;
  config.OneBySixteenFixup := OneBySixteenLCDFixupCheckBox.Checked;
  config.save();
  {$IFNDEF STANDALONESETUP}
  if ReinitLcd then
    LCDSmartieDisplayForm.ReInitLCD();

  ReInitLCD := False;
  LCDSmartieDisplayForm.ShowTrueLCD := Config.EmulateLCD;
  {$ENDIF}
end;

// ok has been pressed.
procedure TSetupForm.OKButtonClick(Sender: TObject);
begin
  ApplyButtonClick(Sender);
end;

procedure TSetupForm.SaveAsButtonClick(Sender: TObject);
begin
  OpenDialog3.Filename := ExtractFileName(OpenDialog3.Filename);
  OpenDialog3.Execute;
  config.filename := OpenDialog3.Filename;
  ApplyButtonClick(Sender);
  SetupForm.Caption := config.filename;
end;

procedure TSetupForm.FormCreate(Sender: TObject);
var
  pathssl: string;
  oviVersionInfo: windows.TOSVERSIONINFO;
  i, j, k: integer;
begin
  {$IFDEF STANDALONESETUP}
  SetupForm.BorderStyle := bsSingle;
  {$ENDIF}
  // putting all these into an array makes things easier
  // it'd be nicer to generate them programatically though
  LineEditArray[1] := Line1MemoEdit; LineEditArray[2] := Line2MemoEdit;
  LineEditArray[3] := Line3MemoEdit; LineEditArray[4] := Line4MemoEdit;
  LineEditArray[5] := Line5MemoEdit; LineEditArray[6] := Line6MemoEdit;
  LineEditArray[7] := Line7MemoEdit; LineEditArray[8] := Line8MemoEdit;

  LineEditButtonArray[1] := Line1EditButton; LineEditButtonArray[2] := Line2EditButton;
  LineEditButtonArray[3] := Line3EditButton; LineEditButtonArray[4] := Line4EditButton;
  LineEditButtonArray[5] := Line5EditButton; LineEditButtonArray[6] := Line6EditButton;
  LineEditButtonArray[7] := Line7EditButton; LineEditButtonArray[8] := Line8EditButton;

  LineClearButtonArray[1] := Line1ClearButton; LineClearButtonArray[2] := Line2ClearButton;
  LineClearButtonArray[3] := Line3ClearButton; LineClearButtonArray[4] := Line4ClearButton;
  LineClearButtonArray[5] := Line5ClearButton; LineClearButtonArray[6] := Line6ClearButton;
  LineClearButtonArray[7] := Line7ClearButton; LineClearButtonArray[8] := Line8ClearButton;

  ContinueLineCheckBoxArray[1] := ContinueLine1CheckBox; ContinueLineCheckBoxArray[2] := ContinueLine2CheckBox;
  ContinueLineCheckBoxArray[3] := ContinueLine3CheckBox; ContinueLineCheckBoxArray[4] := ContinueLine4CheckBox;
  ContinueLineCheckBoxArray[5] := ContinueLine5CheckBox; ContinueLineCheckBoxArray[6] := ContinueLine6CheckBox;
  ContinueLineCheckBoxArray[7] := ContinueLine7CheckBox; ContinueLineCheckBoxArray[8] := ContinueLine8CheckBox;

  DontScrollLineCheckBoxArray[1] := DontScrollLine1CheckBox; DontScrollLineCheckBoxArray[2] := DontScrollLine2CheckBox;
  DontScrollLineCheckBoxArray[3] := DontScrollLine3CheckBox; DontScrollLineCheckBoxArray[4] := DontScrollLine4CheckBox;
  DontScrollLineCheckBoxArray[5] := DontScrollLine5CheckBox; DontScrollLineCheckBoxArray[6] := DontScrollLine6CheckBox;
  DontScrollLineCheckBoxArray[7] := DontScrollLine7CheckBox; DontScrollLineCheckBoxArray[8] := DontScrollLine8CheckBox;

  CenterLineCheckBoxArray[1] := CenterLine1CheckBox; CenterLineCheckBoxArray[2] := CenterLine2CheckBox;
  CenterLineCheckBoxArray[3] := CenterLine3CheckBox; CenterLineCheckBoxArray[4] := CenterLine4CheckBox;
  CenterLineCheckBoxArray[5] := CenterLine5CheckBox; CenterLineCheckBoxArray[6] := CenterLine6CheckBox;
  CenterLineCheckBoxArray[7] := CenterLine7CheckBox; CenterLineCheckBoxArray[8] := CenterLine8CheckBox;

  ShutdownEditArray[1] := ShutdownEdit1; ShutdownEditArray[2] := ShutdownEdit2;
  ShutdownEditArray[3] := ShutdownEdit3; ShutdownEditArray[4] := ShutdownEdit4;
  ShutdownEditArray[5] := ShutdownEdit5; ShutdownEditArray[6] := ShutdownEdit6;
  ShutdownEditArray[7] := ShutdownEdit7; ShutdownEditArray[8] := ShutdownEdit8;

  CCharSpeedButtonArray[1] := CChar1SpeedButton; CCharSpeedButtonArray[2] := CChar2SpeedButton;
  CCharSpeedButtonArray[3] := CChar3SpeedButton; CCharSpeedButtonArray[4] := CChar4SpeedButton;
  CCharSpeedButtonArray[5] := CChar5SpeedButton; CCharSpeedButtonArray[6] := CChar6SpeedButton;
  CCharSpeedButtonArray[7] := CChar7SpeedButton; CCharSpeedButtonArray[8] := CChar8SpeedButton;

  pathssl := ExtractFilePath(ParamStr(0)) + 'openssl\';
  // check if ssl dll exists , if not block the ssl edit !!!
  if not fileExists(pathssl + 'libeay32.dll') or not
    fileExists(pathssl + 'ssleay32.dll') then EmailSSLEdit.Enabled := False;

  //point PluginListBox to the plugin dirs
  Btn_PluginRefreshClick(nil);
  ShowHiddenPlugins := false;

  DetectedOs := 'Unknown';
  oviVersionInfo.dwOSVersionInfoSize := SizeOf(oviVersionInfo);
  if windows.GetVersionEx(oviVersionInfo) then
    DetectedOs := inttostr(oviVersionInfo.dwMajorVersion)
    +'.'+ inttostr(oviVersionInfo.dwMinorVersion)
    +'.'+ inttostr(oviVersionInfo.dwBuildNumber);

  Caption := 'LCD Smartie ' + GetFmtFileVersion() + ' Setup';

  k := 0;
  for i := 0 to 7 do
  begin
    for j := 0 to 4 do
    begin
      CustomCharCheckBoxes[k] := TCheckBox.create(nil);
      CustomCharCheckBoxes[k].Parent := Panel2;
      CustomCharCheckBoxes[k].Visible:=true;
      CustomCharCheckBoxes[k].Top := 8 + (16 * i);
      CustomCharCheckBoxes[k].Left := 16 + (16 * j);
      CustomCharCheckBoxes[k].OnClick := CCharEditGridChange;
      inc(k);
    end;
  end;

  k := 0;
  for i := 0 to 3 do
  begin
    for j := 0 to 7 do
    begin
      SavedCustomCharButtons[k] := TSpeedButton.create(nil);
      SavedCustomCharButtons[k].Parent := CCharTabSheet;
      SavedCustomCharButtons[k].Visible:=true;
      SavedCustomCharButtons[k].Top := 168 + (24 * i);
      SavedCustomCharButtons[k].Left := 8 + (24 * j);
      SavedCustomCharButtons[k].ImageIndex:= 0;
      SavedCustomCharButtons[k].GroupIndex:=1;
      SavedCustomCharButtons[k].Hint := 'Each button is a save slot for easy access to your custom characters';
      SavedCustomCharButtons[k].ShowHint := true;
      // load customs
      CustomCharToButtonGlyph(SavedCustomCharButtons[k], config.SavedCustomChars[k]);
      SavedCustomCharButtons[k].OnClick := SavedCustomCharButtonClick;
      inc(k);
    end;
  end;
  SavedCustomCharButtons[0].Down := true;
  SavedCustomCharButtonClick(SavedCustomCharButtons[0]);

  SkinSelectComboBoxGetItems(nil);
  IconSelectComboBoxGetItems(nil);
end;

procedure TSetupForm.ShowTabsCheckBoxClick(Sender: TObject);
begin
  NetworkStatsTabSheet.TabVisible := ShowNetStatsCheckBox.Checked;
  MiscTabSheet.TabVisible := ShowMiscCheckBox.Checked;
  LCDFeaturesTabSheet.TabVisible := ShowLCDFeaturesCheckBox.Checked;
  PluginsTabSheet.TabVisible := ShowPluginsCheckBox.Checked;
  CCharTabSheet.TabVisible := ShowCharEditorCheckBox.Checked;
  PerfTabSheet.TabVisible := ShowPerfCheckBox.Checked;
  SysInfoTabSheet.TabVisible := ShowSysinfoCheckBox.Checked;
  WinampTabSheet.TabVisible := ShowWinampCheckBox.Checked;
  InternetTabSheet.TabVisible := ShowRssCheckBox.Checked;
  GameStatsTabSheet.TabVisible := ShowGameStatsCheckBox.Checked;
  BOINCTabSheet.TabVisible := ShowBOINCCheckBox.Checked;
  FoldingAtHomeTabSheet.TabVisible := ShowFoldingAtHomeCheckBox.Checked;
  EmailTabSheet.TabVisible := ShowEmailCheckBox.Checked;
end;

procedure TSetupForm.MainPageControlChange(Sender: TObject);
begin
  if MainPageControl.ActivePage = ScreensTabSheet then
  begin
    if LeftPageControl.ActivePage = LCDFeaturesTabSheet then
    begin
      if pos('$MObutton', VariableEdit.Text) <> 0 then VariableEdit.Text := NoVariable;
      LeftPageControl.ActivePage := WinampTabSheet;
    end;
    GameServerEdit.Text := config.gameServer[ScreenSpinEdit.Value, 1];
    setupbutton := 1;
    LineEditEnter(Line1MemoEdit);
  end;
end;

procedure TSetupForm.ActionAddButtonClick(Sender: TObject);
var
  Selection: integer;
begin
  Selection := ActionsStringGrid.Selection.Top;
  ActionsStringGrid.InsertColRow(false, Selection +1);
  ActionsStringGrid.Cells[1, Selection +1] := 'if';
  ActionsStringGrid.Cells[5, Selection +1] := 'then';
  ActionsStringGrid.Cells[7, Selection +1] := 'True';
end;

procedure TSetupForm.DuplicateActionButtonClick(Sender: TObject);
var
  Selection: integer;
begin
  Selection := ActionsStringGrid.Selection.Top;
  ActionsStringGrid.InsertColRow(false, Selection +1);
  ActionsStringGrid.Cells[1, Selection +1] := 'if';
  ActionsStringGrid.Cells[2, Selection +1] := ActionsStringGrid.Cells[2, Selection];
  ActionsStringGrid.Cells[3, Selection +1] := ActionsStringGrid.Cells[3, Selection];
  ActionsStringGrid.Cells[4, Selection +1] := ActionsStringGrid.Cells[4, Selection];
  ActionsStringGrid.Cells[5, Selection +1] := 'then';
  ActionsStringGrid.Cells[6, Selection +1] := ActionsStringGrid.Cells[6, Selection];
  ActionsStringGrid.Cells[7, Selection +1] := ActionsStringGrid.Cells[7, Selection];
end;

procedure TSetupForm.ActionDeleteButtonClick(Sender: TObject);
begin
  ActionsStringGrid.DeleteRow(ActionsStringGrid.Selection.Top);
end;

procedure TSetupForm.ButtonsListBoxClick(Sender: TObject);
begin
  case ButtonsListBox.ItemIndex of
    0: VariableEdit.Text := '$MObutton';
    1: VariableEdit.Text := '$FanSpeed(1,1)';
    2: VariableEdit.Text := '$Sensor1';
    3: VariableEdit.Text := '$Sensor2';
    4: VariableEdit.Text := '$Sensor3';
    5: VariableEdit.Text := '$Sensor4';
    6: VariableEdit.Text := '$Sensor5';
    7: VariableEdit.Text := '$Sensor6';
    8: VariableEdit.Text := '$Sensor7';
    9: VariableEdit.Text := '$Sensor8';
    else
      VariableEdit.Text := NoVariable;
  end; // case

  //if not (VariableEdit.Text = NoVariable) then
  //  FocusToInputField();
end;

procedure TSetupForm.StickyCheckboxClick(Sender: TObject);
begin
  TimeToShowSpinEdit.Enabled := not StickyCheckbox.Checked;
end;

procedure TSetupForm.ColorSchemeComboBoxChange(Sender: TObject);
begin
  if ColorSchemeComboBox.ItemIndex < 0 then ColorSchemeComboBox.ItemIndex := 0;
end;

procedure UpdateSetupForm(cKey: char);
begin
  if assigned(SetupForm) then
    SetupForm.LastKeyPressedEdit.Text := cKey;
end;

procedure TSetupForm.ContrastTrackBarChange(Sender: TObject);
begin
  {$IFNDEF STANDALONESETUP}
  LCDSmartieDisplayForm.lcd.setContrast(ContrastTrackBar.position);
  {$ENDIF}
end;

procedure TSetupForm.BrightnessTrackBarChange(Sender: TObject);
begin
  {$IFNDEF STANDALONESETUP}
  LCDSmartieDisplayForm.lcd.setBrightness(BrightnessTrackBar.position);
  {$ENDIF}
end;

/////// SHUTDOWN MESSAGE EDIT ///////////////

procedure TSetupForm.ShutdownEditEnter(Sender: TObject);
var
  oEdit: TMemo;
  loop: integer;
begin
  oEdit := Sender as TMemo;
  oEdit.Color := $00A1D7A4;

  for loop := 1 to MaxLines do
  if (ShutdownEditArray[loop] <> Sender) and ShutdownEditArray[loop].Enabled = True then
    ShutdownEditArray[loop].color := clWhite
  else
    shdownmessagebutton := loop;
end;

/////////////////////////////////////////////////////////////////
//////////////////// PLUGIN LIST BOX ////////////////////////////
/////////////////////////////////////////////////////////////////
// Deal with a right click in the list box
procedure TSetupForm.PluginListBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var item:integer;
begin
  item:=PluginListBox.ItemAtPos(Point(X,Y),true);
  if item>-1 then PluginListBox.Selected[item]:=true;
  PluginListBoxClick(nil);
end;

// hide an item
procedure TSetupForm.HidePluginMenuItemClick(Sender: TObject);
var
  hiddenDllsIni: TIniFile;
begin
  hiddenDllsIni := TIniFile.Create('plugins\Hidden.ini');
  hiddenDllsIni.WriteString('HiddenDlls', ExtractFileName(PluginListBox.FileName), 'Hide');
  hiddenDllsIni.Free;
  Btn_PluginRefreshClick(nil);
end;

// unhide an item
procedure TSetupForm.UnHidePluginMenuItemClick(Sender: TObject);
var
  hiddenDllsIni: TIniFile;
begin
  hiddenDllsIni := TIniFile.Create('plugins\Hidden.ini');
  hiddenDllsIni.DeleteKey('HiddenDlls', ExtractFileName(PluginListBox.FileName));
  hiddenDllsIni.Free;
  Btn_PluginRefreshClick(nil);
end;

// Show all hidden items
procedure TSetupForm.ShowHiddenPluginsMenuItemClick(Sender: TObject);
begin
  if ShowHiddenPluginsMenuItem.Caption = 'Show Hidden Plugins' then
  begin
    ShowHiddenPlugins := true;
    ShowHiddenPluginsMenuItem.Caption := 'Hide Hidden Plugins'
  end
  else
  begin
    ShowHiddenPlugins := false;
    ShowHiddenPluginsMenuItem.Caption := 'Show Hidden Plugins'
  end;
  Btn_PluginRefreshClick(nil);
end;

procedure TSetupForm.PluginListBoxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  ListBox: TFileListBox;
  Canvas: TCanvas;
  hiddenDllsIni: TIniFile;
  slhiddenDlls: TStringList;
  j: integer;
begin
  ListBox := Control as TFileListBox;
  Canvas := PluginListBox.Canvas;
  Canvas.FillRect(Rect);
  hiddenDllsIni := TIniFile.Create('plugins\Hidden.ini');
  slhiddenDlls := TStringList.Create;
  hiddenDllsIni.ReadSection('HiddenDlls', slhiddenDlls);
  hiddenDllsIni.Free;
  Canvas.Font.Color := clBlack;
  for j := 0 to slhiddenDlls.Count -1 do
    if ListBox.Items[Index] = slhiddenDlls[j] then
    begin
      Canvas.Font.Color := clGray;
      Break;
    end;

  Canvas.TextOut(Rect.Left, Rect.Top, ListBox.Items[Index]);

  if odFocused in State then
    Canvas.DrawFocusRect(Rect);
end;

procedure TSetupForm.Btn_PluginRefreshClick(Sender: TObject);
var
  hiddenDllsIni: TIniFile;
  slhiddenDlls: TStringList;
  i, j: integer;
begin
  hiddenDllsIni := TIniFile.Create('plugins\Hidden.ini');
  slhiddenDlls := TStringList.Create;
  hiddenDllsIni.ReadSection('HiddenDlls', slhiddenDlls);
  hiddenDllsIni.Free;

  PluginListBox.Directory := '.';
  PluginListBox.Directory := ExtractFilePath(ParamStr(0)) + 'plugins\';
  PluginListBox.Refresh;

  if  not ShowHiddenPlugins then
  For i := PluginListBox.Count - 1 downto 0  do
    for j := 0 to slhiddenDlls.Count -1 do
      if PluginListBox.Items[i] = slhiddenDlls[j] then
           PluginListBox.Items.Delete(i);

end;

procedure TSetupForm.PluginListBoxClick(Sender: TObject);
var
  PluginName, Reply, TextFileName, tfcLine: string;
  GotInfo, GotDemo: boolean;
  TextFileContent: TStrings;
  i, indexStart, indexEnd, p: integer;
  InfoList: TStringList;
begin
  GotInfo := false;
  GotDemo := false;
  PluginName := ExtractFileName(PluginListBox.FileName);
  PluginDemoListBox.Clear;

  try
    LCDSmartieDisplayForm.Data.FindPlugin(PluginName);
  except
  end;

  PluginDeveloperLabel.Caption := 'N/A';
  PluginVersionLabel.Caption := 'N/A';

  VariableEdit.Text := '$dll('+ExtractFileNameOnly(PluginName)+',1,,)';

  try
    reply := LCDSmartieDisplayForm.Data.GetPluginInfo(PluginName);

    if not (reply = '') then begin
      InfoList := TStringList.Create;
      InfoList.AddDelimitedText(reply, #10, true);

      for i := 0 to InfoList.Count - 1 do begin
        tfcLine := lowercase(InfoList[i]);
        p := pos('dev', tfcLine);
        if p > 0 then begin
          p := pos(':', tfcLine);
          reply := copy(InfoList[i], p+1, length(InfoList[i]) - p + 1);
          if reply[1] = ' ' then
            delete(reply, 1, 1);
          PluginDeveloperLabel.Caption := reply;
        end;
        p := pos('ver', tfcLine);
        if p > 0 then begin
          p := pos(':', tfcLine);
          reply := copy(InfoList[i], p+1, length(InfoList[i]) - p + 1);
          if reply[1] = ' ' then
            delete(reply, 1, 1);
          PluginVersionLabel.Caption := reply;
        end;
      end;
      GotInfo := true;
    end;
  except
      on E: Exception do
          showmessage('Plugin '+PluginName+ E.Message);
  end;

  try
    reply := LCDSmartieDisplayForm.Data.GetPluginDemos(PluginName);
    if not (reply = '') then begin
      PluginDemoListBox.Items.AddDelimitedText(reply,#10,true);
      GotDemo := true;
    end;
  except
    on E: Exception do
      showmessage('Plugin '+PluginName+ E.Message);
  end;

  if (GotInfo = false) and (GotDemo = false) then
  begin
    // we can try reading the info and demos from a .txt file
    TextFileName := 'plugins\'+ExtractFileNameOnly(PluginName)+'.txt';
    if FileExists(TextFileName, false) then
    begin
      TextFileContent := TStringlist.Create;
      TextFileContent.LoadFromFile(TextFileName);
      indexStart := TextFileContent.IndexOf('[[INFO]]');
      indexEnd := TextFileContent.IndexOf('[[END]]', indexStart);

      if not ((indexEnd < 0) or (indexStart < 0) or ( indexEnd < indexStart)) then
      begin
        for i := indexStart+1 to  indexEnd - 1 do
        begin
          tfcLine := lowercase(TextFileContent[i]);
          p := pos('dev', tfcLine);
          if p > 0 then begin
            p := pos(':', tfcLine);
            reply := copy(TextFileContent[i], p+1, length(TextFileContent[i]) - p + 1);
            if reply[1] = ' ' then
               delete(reply, 1, 1);
            PluginDeveloperLabel.Caption := reply;
          end;
          p := pos('ver', tfcLine);
          if p > 0 then begin
            p := pos(':', tfcLine);
            reply := copy(TextFileContent[i], p+1, length(TextFileContent[i]) - p + 1);
            if reply[1] = ' ' then
              delete(reply, 1, 1);
            PluginVersionLabel.Caption := reply;
          end;
        end;
        GotInfo := true;
      end;

      indexStart := TextFileContent.IndexOf('[[DEMO]]', 0);
      indexEnd := TextFileContent.IndexOf('[[END]]', indexStart);
      if not ((indexEnd < 0) or (indexStart < 0) or ( indexEnd < indexStart)) then
      begin
        for i := indexStart+1 to indexEnd - 1 do
        begin
          PluginDemoListBox.Items.Add(TextFileContent[i]);
        end;
        GotDemo := true;
      end;
      TextFileContent.Free;
    end;
  end;
end;

procedure TSetupForm.PluginDemoListBoxClick(Sender: TObject);
var
  S: String;
  P: integer;
begin
  VariableEdit.Text := NoVariable;
  if PluginDemoListBox.ItemIndex > 0 then
  begin
    S := PluginDemoListBox.Items[PluginDemoListBox.ItemIndex];

    if ((Pos('http://', lowercase(S)) = 1) or (Pos('https://', lowercase(S)) = 1)) and (GetKeyState(VK_SHIFT) < 0) then
    Begin
      ShellExecute(0, Nil, pchar(S), Nil, Nil, SW_NORMAL);
      Exit;
    end;


    P := Pos('$', S);
    if P > 0 then
      VariableEdit.Text := copy(S, P, length(S) - P+1);
  end;
end;

/////////////////////////////////////////////////////////////////
////////////////////////// ICON PATH ////////////////////////////
/////////////////////////////////////////////////////////////////
procedure TSetupForm.DrawPreviewIcons(const sIconFileName: string);
var
  hIcon: tIcon;
  IconPathName: string;
begin
  hIcon := TIcon.Create;
  if sIconFileName = 'default for this skin' then
    IconPathName := extractfilepath(application.exename) + LCDSmartieDisplayForm.sSkinDir + config.sSkinPath + '\smartie.ico'
  else
    IconPathName := extractfilepath(application.exename) + LCDSmartieDisplayForm.sSkinDir + sIconFileName;

  try
    if FileExists(IconPathName) then
    begin
      GetIconFromFile(IconPathName, hIcon, SHIL_SMALL);

      TrayIconPreview16.Picture.Icon.Assign(hIcon);

      GetIconFromFile(IconPathName, hIcon, SHIL_LARGE);

      TrayIconPreview32.Picture.Icon.Assign(hIcon);
    end
    else
      TrayIconPreview32.Picture.Icon.Assign(Application.Icon);

  except
    on E: Exception do
    begin
      ShowMessage('Error' + sLineBreak + 'Unable to load Tray Icon from Skin path, ' +
        LCDSmartieDisplayForm.sSkinDir + sIconFileName + ': ' + E.Message);
    end;
  end;
  hIcon.Free;
end;

procedure TSetupForm.IconSelectComboBoxChange(Sender: TObject);
begin
  DrawPreviewIcons(IconSelectComboBox.Text);
end;

procedure TSetupForm.IconSelectComboBoxGetItems(Sender: TObject);
 var
   IconFiles: TStringList;
   s: string;
   i: integer;
begin
  IconFiles := TStringList.Create;
  FindAllFiles(IconFiles, LCDSmartieDisplayForm.sSkinDir, '*.ico', true);
  IconSelectComboBox.Items := IconFiles;

  s := IconSelectComboBox.Text;
  IconSelectComboBox.Clear;
  IconSelectComboBox.Text := s;
  for i := 0 to IconFiles.Count -1 do
  begin
    s := StringReplace(IconFiles[i], LCDSmartieDisplayForm.sSkinDir, '', [rfIgnoreCase]);
    IconSelectComboBox.Items.Add(s);
  end;
  IconSelectComboBox.Items.Insert(0, 'default for this skin');
end;

function MyFileExists(const FileName: string): boolean;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      Result := True
    else
      Result := False;
  end
  else
    Result := False;
end;

/////////////////////////////////////////////////////////////////
////////////////////////// SKIN PATH ////////////////////////////
/////////////////////////////////////////////////////////////////
procedure TSetupForm.SkinSelectComboBoxChange(Sender: TObject);
begin
  config.sSkinPath := SkinSelectComboBox.Text;
  LCDSmartieDisplayForm.LoadSkin();
  LCDSmartieDisplayForm.LoadColors();
end;

procedure TSetupForm.SkinSelectComboBoxGetItems(Sender: TObject);
var
  ColorsFiles: TStringList;
  s: string;
  i: integer;
begin
  ColorsFiles := TStringList.Create;
  FindAllFiles(ColorsFiles, LCDSmartieDisplayForm.sSkinDir, 'colors.cfg', true);
  s := SkinSelectComboBox.Text;
  SkinSelectComboBox.Clear;
  SkinSelectComboBox.Text := s;
  for i := 0 to ColorsFiles.Count -1 do
  begin
    s := StringReplace(ColorsFiles[i], LCDSmartieDisplayForm.sSkinDir, '', [rfIgnoreCase]);
    s := StringReplace(s, '\colors.cfg', '', [rfIgnoreCase]);
    SkinSelectComboBox.Items.Add(s);
  end;
end;

/////////////////////////////////////////////////////////////////
////////////// LINE EDIT FORM BUTTONS ///////////////////////////
/////////////////////////////////////////////////////////////////
procedure TSetupForm.LineEditClick(Sender: TObject);
var
  loop: integer;
begin
  for loop := 1 to MaxLines do
    if Sender = LineEditButtonArray[loop] then
    begin
      if not assigned(FormEditArray[Loop]) then
      begin
        FormEditArray[Loop] := TFormEdit.Create(self);
        FormEditArray[Loop].Apply.OnClick := FormEditApply;
        FormEditArray[Loop].OK.OnClick := FormEditOk;
        FormEditArray[Loop].Cancel.OnClick := FormEditCancel;
        FormEditArray[Loop].Memo1.OnEnter := FormEditMemoEnter;
        FormEditArray[Loop].Memo1.OnClick := FormEditMemoOnClick;
        FormEditArray[Loop].Memo1.Text := LineEditArray[loop].Text;
        FormEditArray[Loop].LineNumber := loop;
        FormEditArray[Loop].Caption := FormEditArray[Loop].Caption + ' ' + inttostr(loop);
        FormEditArray[Loop].Top := config.EditFormPosTop;
        FormEditArray[Loop].Left := config.EditFormPosLeft;
        FormEditArray[Loop].Height := config.EditFormPosHeight;
        FormEditArray[Loop].Width := config.EditFormPosWidth;
        FormEditArray[Loop].Show;
      end;
    end;
end;

procedure TSetupForm.FormEditApply(Sender: TObject);
var
  loop: integer;
begin
  for loop := 1 to MaxLines do
    if assigned(FormEditArray[Loop]) then
      if Sender = FormEditArray[loop].Apply then
      begin
        LineEditArray[FormEditArray[loop].LineNumber].Text := FormEditArray[loop].Memo1.Text;
      end;
end;

procedure TSetupForm.FormEditOk(Sender: TObject);
var
  loop: integer;
begin
  for loop := 1 to MaxLines do
    if assigned(FormEditArray[Loop]) then
      if Sender = FormEditArray[loop].OK then
      begin
        LineEditArray[FormEditArray[loop].LineNumber].Text := FormEditArray[loop].Memo1.Text;
        config.EditFormPosTop := FormEditArray[loop].Top;
        config.EditFormPosLeft := FormEditArray[loop].Left;
        config.EditFormPosHeight := FormEditArray[loop].Height;
        config.EditFormPosWidth := FormEditArray[loop].Width;
        config.save();
        FormEditArray[loop].Close;
        freeandnil(FormEditArray[loop]);
      end;
end;

procedure TSetupForm.FormEditCancel(Sender: TObject);
var
  loop: integer;
begin
  for loop := 1 to MaxLines do
    if assigned(FormEditArray[Loop]) then
      if Sender = FormEditArray[loop].Cancel then
      begin
        FormEditArray[loop].Close;
        freeandnil(FormEditArray[loop]);
      end;
end;

procedure TSetupForm.FormEditMemoEnter(Sender: TObject);
var
  loop: integer;
begin
  for loop := 1 to MaxLines do
    if assigned(FormEditArray[Loop]) then
      if Sender = FormEditArray[loop].Memo1 then
      begin
        setupbutton := loop + MaxLines;
      end;
end;

procedure TSetupForm.FormEditMemoOnClick(Sender: TObject);
var
  loop: integer;
begin
  for loop := 1 to MaxLines do
    if assigned(FormEditArray[Loop]) then
      if Sender = FormEditArray[loop].Memo1 then
      begin
        setupbutton := loop + MaxLines;
      end;
end;

/////////////////////////////////////////////////////////////////
////////////// CUSTOM CHAR SPEED BUTTONS ////////////////////////
/////////////////////////////////////////////////////////////////
procedure TSetupForm.CCharSpeedButtonClick(Sender: TObject);
var
  loop: integer;
begin
  for loop := 0 to 7 do
  begin
    if Sender = CCharSpeedButtonArray[loop+1] then
    begin
      VariableEdit.Text := '$Chr('+inttostr(loop)+')';
      InsertButtonClick(nil);
    end;
  end;
end;

procedure TSetupForm.LoadCCharSpeedButtonGlyphs;
var
  loop: integer;
  CustomArray: TCustomArray;
begin
  for loop := 1 to 8 do
  begin
    CustomArray := CustomStringToCustomArray(config.screen[ScreenSpinEdit.Value].CustomCharacters[loop]);
    CustomCharToButtonGlyph(CCharSpeedButtonArray[loop], CustomArray);
  end;
end;

/////////////////////////////////////////////////////////////////
////////////// CUSTOM CHAR DEFINE FORM //////////////////////////
/////////////////////////////////////////////////////////////////
procedure TSetupForm.CCharDefFormApply(Sender: TObject);
var
  loop: integer;
begin
  for loop := 1 to 8 do
  begin
    config.screen[ScreenSpinEdit.Value].CustomCharacters[loop] := CCharDefForm.EditArray[loop].Text;
    CustomCharToButtonGlyph(CCharSpeedButtonArray[loop],
    CustomStringToCustomArray(config.screen[ScreenSpinEdit.Value].CustomCharacters[loop]));
  end;
  config.CCharFormTop := CCharDefForm.Top;
  config.CCharFormLeft := CCharDefForm.Left;
  config.save;
end;

procedure TSetupForm.CCharDefFormOK(Sender: TObject);
begin
  CCharDefFormApply(Self);
  CCharDefForm.Close;
  freeandnil(CCharDefForm);
end;

procedure TSetupForm.CCharDefFormCancel(Sender: TObject);
begin
  CCharDefForm.Close;
  freeandnil(CCharDefForm);
end;

procedure TSetupForm.CCharDefFormEditEnter(Sender: TObject);
var
  loop: integer;
begin
  for loop := 1 to 8 do
    if Sender = CCharDefForm.EditArray[loop] then
      setupbutton := loop + MaxLines + 8;
end;

/////////////////////////////////////////////////////////////////
////////////// CUSTOM CHAR EDIT TAB /////////////////////////////
/////////////////////////////////////////////////////////////////
procedure TSetupForm.CustomCharToButtonGlyph(Button: TSpeedButton; CustomArray: TCustomArray);
var
  bitmap: TBitmap;
  x, y: integer;
begin
  bitmap := TBitmap.Create;
  bitmap.Width := 5;
  bitmap.Height := 8;
  with bitmap.Canvas do
  for X := 0 to 5-1 do begin
    for Y := 0 to 8-1 do begin
      if ((CustomArray[Y] and (1 shl X)) > 0) then
        Pixels[5-1-X,Y] := clBlack
      else
        Pixels[5-1-X,Y] := Button.Color;
    end;
  end ;
  Button.Glyph.Width := 10;
  Button.Glyph.Height := 16;
  Button.Glyph.Canvas.StretchDraw(Rect(0, 0, 10, 16), bitmap);
  bitmap.free;
end;

procedure TSetupForm.CustomCharClearButtonClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to 39 do
      CustomCharCheckBoxes[i].Checked := False;
end;

procedure TSetupForm.CustomCharCreateButtonClick(Sender: TObject);
var
  loc, i: integer;
begin
  loc := CreateCCharLocSpinEdit.Value;
  for i := 0 to 31 do
  begin
    if SavedCustomCharButtons[i].Down then
    begin
      VariableEdit.Text := '$CustomChar(' + IntToStr(loc) +
        ',' + IntToStr(config.SavedCustomChars[i][0]) +
        ',' + IntToStr(config.SavedCustomChars[i][1]) +
        ',' + IntToStr(config.SavedCustomChars[i][2]) +
        ',' + IntToStr(config.SavedCustomChars[i][3]) +
        ',' + IntToStr(config.SavedCustomChars[i][4]) +
        ',' + IntToStr(config.SavedCustomChars[i][5]) +
        ',' + IntToStr(config.SavedCustomChars[i][6]) +
        ',' + IntToStr(config.SavedCustomChars[i][7]) + ')';
      InsertButtonClick(nil);
      break;
    end;
  end;
end;

procedure TSetupForm.CustomCharGridMirrorSpeedButtonClick(Sender: TObject);
var
  i, X, Y: integer;
begin
  // disable grid updates
  for i := 0 to 39 do
    CustomCharCheckBoxes[i].OnClick := nil;
  i := 0;
  for Y := 0 to 8-1 do begin
    for X := 4 downto 0 do begin
      if ((MirrorCustomArray[Y] and (1 shl X)) > 0) then
        CustomCharCheckBoxes[i].Checked := true
      else
        CustomCharCheckBoxes[i].Checked := false;
      inc(i);
    end;
  end;
  // re-enable grid updates
  for i := 0 to 39 do
    CustomCharCheckBoxes[i].OnClick := CCharEditGridChange;
  CCharEditGridChange(nil);
end;

procedure TSetupForm.CustomCharInvertButtonClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to 39 do
      CustomCharCheckBoxes[i].Checked := not CustomCharCheckBoxes[i].Checked;
end;

procedure TSetupForm.CustomCharUseButtonClick(Sender: TObject);
begin
  VariableEdit.Text := '$Chr('+IntToStr(CreateCCharLocSpinEdit.Value -1)+')';
  InsertButtonClick(nil);
end;

procedure TSetupForm.ExportClipboardClick(Sender: TObject);
var
  Clipboard: TClipboard;
  ActionStrings: TStringList;
begin
  Clipboard := TClipboard.Create;
  ActionStrings := TStringList.Create;
  // add the Action01XXX just to give us something to validate when importing
  ActionStrings.Add('Action01Variable=' + config.actionsArray[ActionsStringGrid.Row, 1]); // Variable
  ActionStrings.Add('Action01Condition=' + config.actionsArray[ActionsStringGrid.Row, 2]); // Condition
  ActionStrings.Add('Action01ConditionValue=' + config.actionsArray[ActionsStringGrid.Row, 3]); // ConditionValue
  ActionStrings.Add('Action01Action=' + config.actionsArray[ActionsStringGrid.Row, 4]); // ActionStrings
  ActionStrings.Add('Action01Enabled=' + config.actionsArray[ActionsStringGrid.Row, 5]); // Enabled
  Clipboard.AsText := ActionStrings.Text;
  ActionStrings.Free;
  Clipboard.Free;
end;

procedure TSetupForm.ExportFileClick(Sender: TObject);
var
  ActionStrings: TStringlist;
begin
  ExportFileSaveDialog.FileName := ('Action.txt');
  if ExportFileSaveDialog.Execute then
  begin
    ActionStrings := TStringlist.Create;
    ActionStrings.Add('Action01Variable=' + config.actionsArray[ActionsStringGrid.Row, 1]); // Variable
    ActionStrings.Add('Action01Condition=' + config.actionsArray[ActionsStringGrid.Row, 2]); // Condition
    ActionStrings.Add('Action01ConditionValue=' + config.actionsArray[ActionsStringGrid.Row, 3]); // ConditionValue
    ActionStrings.Add('Action01Action=' + config.actionsArray[ActionsStringGrid.Row, 4]); // ActionStrings
    ActionStrings.Add('Action01Enabled=' + config.actionsArray[ActionsStringGrid.Row, 5]); // Enabled
    ActionStrings.SaveToFile(ExportFileSaveDialog.Filename);
  end;
end;

procedure TSetupForm.ImportClipboardClick(Sender: TObject);
var
  Clipboard: TClipboard;
  ActionStrings: TStringList;
  i, selection: integer;
  ActOpr: string;
begin
  Clipboard := TClipboard.Create;
  ActionStrings := TStringList.Create;
  ActionStrings.AddText(Clipboard.AsText);

  if (pos('Action', ActionStrings[0]) > 0) then
  begin
    i := pos('=', ActionStrings[0]);
    if i > 0  then
    begin
      Selection := ActionsStringGrid.Selection.Top;
      ActionsStringGrid.InsertColRow(false, Selection +1);
      ActionsStringGrid.Cells[1, Selection +1] := 'if';
      ActionsStringGrid.Cells[2, Selection +1] := copy(ActionStrings[0], i + 1, 255);
      i := pos('=', ActionStrings[1]);
      if i > 0 then
      begin
        case (StrToInt(copy(ActionStrings[1], i+1,1))) of
            0: ActOpr := '>';
            1: ActOpr := '<';
            2: ActOpr := '=';
            3: ActOpr := '<=';
            4: ActOpr := '>=';
          else
            ActOpr := '<>';
          end;
          ActionsStringGrid.Cells[3, Selection +1] := ActOpr;
        i := pos('=', ActionStrings[2]);
        if i > 0 then
        begin
          ActionsStringGrid.Cells[4, Selection +1] := copy(ActionStrings[2], i + 1, 255);
          ActionsStringGrid.Cells[5, Selection +1] := 'then';
          i := pos('=', ActionStrings[3]);
          if i > 0 then begin
            ActionsStringGrid.Cells[6, Selection +1] := copy(ActionStrings[3], i + 1, 255);
            i := pos('=', ActionStrings[4]);
            if i > 0 then begin
              ActionsStringGrid.Cells[7, Selection +1] := copy(ActionStrings[4], i + 1, 255);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TSetupForm.ImportFileClick(Sender: TObject);
var
  ActionStrings: TStringlist;
  i, Selection: integer;
  ActOpr: string;
begin
  if ImportFileOpenDialog.Execute then
  begin
    ActionStrings := TStringList.Create;
    ActionStrings.LoadFromFile(ImportFileOpenDialog.FileName);

    if (pos('Action', ActionStrings[0]) > 0) then
    begin
      i := pos('=', ActionStrings[0]);
      if i > 0  then
      begin
        Selection := ActionsStringGrid.Selection.Top;
        ActionsStringGrid.InsertColRow(false, Selection +1);
        ActionsStringGrid.Cells[1, Selection +1] := 'if';
        ActionsStringGrid.Cells[2, Selection +1] := copy(ActionStrings[0], i + 1, 255);
        i := pos('=', ActionStrings[1]);
        if i > 0 then
        begin
          case (StrToInt(copy(ActionStrings[1], i+1,1))) of
            0: ActOpr := '>';
            1: ActOpr := '<';
            2: ActOpr := '=';
            3: ActOpr := '<=';
            4: ActOpr := '>=';
          else
            ActOpr := '<>';
          end;
          ActionsStringGrid.Cells[3, Selection +1] := ActOpr;
          i := pos('=', ActionStrings[2]);
          if i > 0 then
          begin
            ActionsStringGrid.Cells[4, Selection +1] := copy(ActionStrings[2], i + 1, 255);
            ActionsStringGrid.Cells[5, Selection +1] := 'then';
            i := pos('=', ActionStrings[3]);
            if i > 0 then begin
              ActionsStringGrid.Cells[6, Selection +1] := copy(ActionStrings[3], i + 1, 255);
              i := pos('=', ActionStrings[4]);
              if i > 0 then begin
                ActionsStringGrid.Cells[7, Selection +1] := copy(ActionStrings[4], i + 1, 255);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TSetupForm.SavedCustomCharButtonClick(Sender: TObject);
var
  i, j, x, y: integer;
  button: TSpeedButton;
  CustomArray: TCustomArray;
begin
  // disable grid updates
  for i := 0 to 39 do
    CustomCharCheckBoxes[i].OnClick := nil;

  button := Sender as TSpeedButton;

  for i := 0 to 31 do
  begin
    if button = SavedCustomCharButtons[i] then
    begin
      j := 0;
      CustomArray := config.SavedCustomChars[i];
      for Y := 0 to 8-1 do begin
        for X := 4 downto 0 do begin
          if ((CustomArray[Y] and (1 shl X)) > 0) then
            CustomCharCheckBoxes[j].Checked := true
          else
            CustomCharCheckBoxes[j].Checked := false;
          inc(j);
        end;
      end;
      break;
    end;
  end;
  // re-enable grid updates
  for i := 0 to 39 do
    CustomCharCheckBoxes[i].OnClick := CCharEditGridChange;
end;

procedure TSetupForm.CCharEditGridChange(Sender: TObject);
type
  TLineArray = array [1..8] of integer;
var
  i, j: integer;
  box: integer;
  CCharLine: TLineArray;
  bitmap: TBitmap;
  x, y: integer;
begin
  box := 0;
  i := 1;
  while box < 40 do
  begin
    CCharLine[i] := 0;
    if CustomCharCheckBoxes[box].Checked then
    begin
      CCharLine[i] := CCharLine[i] + 16;
    end;
    Inc(box);
    if CustomCharCheckBoxes[box].Checked then
    begin
      CCharLine[i] := CCharLine[i] + 8;
    end;
    Inc(box);
    if CustomCharCheckBoxes[box].Checked then
    begin
      CCharLine[i] := CCharLine[i] + 4;
    end;
    Inc(box);
    if CustomCharCheckBoxes[box].Checked then
    begin
      CCharLine[i] := CCharLine[i] + 2;
    end;
    Inc(box);
    if CustomCharCheckBoxes[box].Checked then
    begin
      CCharLine[i] := CCharLine[i] + 1;
    end;
    Inc(box);
    Inc(i);
  end;

  // copy to the mirror
  for i := 0 to 7 do
    MirrorCustomArray[i] := CCharLine[i+1];
  bitmap := TBitmap.Create;
  bitmap.Width := 5;
  bitmap.Height := 8;
  with bitmap.Canvas do
    for X := 0 to 5-1 do begin
      for Y := 0 to 8-1 do begin
        if ((CCharLine[Y+1] and (1 shl X)) > 0) then
          Pixels[5-1-X,Y] := clBlack
        else
          Pixels[5-1-X,Y] := CustomCharGridMirrorSpeedButton.Color;
      end;
    end ;
  CustomCharGridMirrorSpeedButton.Glyph.Width := 10;
  CustomCharGridMirrorSpeedButton.Glyph.Height := 16;
  CustomCharGridMirrorSpeedButton.Glyph.Canvas.StretchDraw(Rect(0, 0, 10, 16), bitmap);
  bitmap.free;

  // save to the char selected
  for i := 0 to 31 do
  begin
    if SavedCustomCharButtons[i].Down then
    begin
      for j := 0 to 7 do
      config.SavedCustomChars[i][j] := CCharLine[j+1];
      CustomCharToButtonGlyph(SavedCustomCharButtons[i], config.SavedCustomChars[i]);
      break;
    end;
  end;
end;

// some code taken from UDataNetwork.pas to list interface numbers
// very handy as my machine has over 60 interfaces
procedure TSetupForm.NetworkStatsAdapterListButtonClick(Sender: TObject);
var
  Size: ULONG;
  IntfTable: PMibIfTable;
  MaxEntries: cardinal;
  Names: string;
  i: integer;
begin
  Size := 0;
  if GetIfTable(nil, Size, True) <> ERROR_INSUFFICIENT_BUFFER then  Exit;
  if (Size < sizeof(TMibIftable)) then Exit;
  IntfTable := AllocMem(Size);
  if (IntfTable <> nil) and (GetIfTable(IntfTable, Size, True) = NO_ERROR) then
  begin
    MaxEntries := min(IntfTable^.dwNumEntries, MAXNETSTATS);
    for i := 0 to MaxEntries - 1 do
    begin
    {$R-}
      Names := Names + IntToStr(i) + ' ' + PChar(@IntfTable.Table[i].bDescr[0]) + #13#10;
{$R+}
    end;
    ShowMessage(Names);
  end;
end;

////////////////////////////////////////////////////////
//               RE-ARRANGING SCREENS                 //
////////////////////////////////////////////////////////
procedure TSetupForm.CopyToScreenButtonClick(Sender: TObject);
begin
  if (CopyToScreenSpinEdit.Value > MaxScreens) or (CopyToScreenSpinEdit.Value < 0) then
  begin
    CopyToScreenSpinEdit.Value := 1;
    ShowMessage('Destination screen value should be between 0 and ' + inttostr(MaxScreens) +
      #13#10 + 'Choose another Value');
    exit;
  end;
  if ScreenSpinEdit.Value = CopyToScreenSpinEdit.Value then
  begin
    ShowMessage('Destination screen is the same as this screen' +
      #13#10 + 'Choose another destination');
    exit;
  end;
  if ScreenSpinEdit.Value = CopyToScreenSpinEdit.Value then
  begin
    ShowMessage('Destination screen is the same as this screen' +
      #13#10 + 'Choose another destination');
    exit;
  end;
  config.screen[CopyToScreenSpinEdit.Value] := config.screen[screenspinedit.Value];
end;

procedure TSetupForm.MoveToScreenButtonClick(Sender: TObject);
begin
  if (MoveToScreenSpinEdit.Value > MaxScreens) or (MoveToScreenSpinEdit.Value < 0) then
  begin
    MoveToScreenSpinEdit.Value := 1;
    ShowMessage('Destination screen value should be between 0 and ' + inttostr(MaxScreens) +
      #13#10 + 'Choose another Value');
    exit;
  end;
  if ScreenSpinEdit.Value = MoveToScreenSpinEdit.Value then
  begin
    ShowMessage('Destination screen is the same as this screen' +
      #13#10 + 'Choose another destination');
    exit;
  end;
  config.screen[MoveToScreenSpinEdit.Value] := config.screen[ScreenSpinEdit.Value];
  config.screen[ScreenSpinEdit.Value] := Default(TScreen);
  LoadScreen(ScreenSpinEdit.Value);
end;

procedure TSetupForm.SwapWithScreenButtonClick(Sender: TObject);
var
  TempScreen: Tscreen;
begin
  if (SwapWithScreenSpinEdit.Value > MaxScreens) or (SwapWithScreenSpinEdit.Value < 0) then
  begin
    SwapWithScreenSpinEdit.Value := 1;
    ShowMessage('Destination screen value should be between 0 and ' + inttostr(MaxScreens) +
      #13#10 + 'Choose another Value');
    exit;
  end;
  if ScreenSpinEdit.Value = SwapWithScreenSpinEdit.Value then
  begin
    ShowMessage('Destination screen is the same as this screen' +
      #13#10 + 'Choose another destination');
    exit;
  end;
  TempScreen := config.screen[ScreenSpinEdit.Value];
  config.screen[ScreenSpinEdit.Value] :=
    config.screen[SwapWithScreenSpinEdit.Value];
  config.screen[SwapWithScreenSpinEdit.Value] := TempScreen;
  LoadScreen(ScreenSpinEdit.Value);
end;

////////////////////////////////////////////////////////
//               REMOTE SMARTIE SSL                   //
////////////////////////////////////////////////////////
procedure TSetupForm.RemoteSendUseSSLCheckBoxClick(Sender: TObject);
begin
  if RemoteSendUseSSLCheckBox.Checked then
    if fileExists(ExtractFilePath(application.exename) + 'openssl\cert.pem') and
      fileExists(ExtractFilePath(application.exename) + 'openssl\key.pem') then
      config.RemoteSendUseSSL := True
    else
    begin
      config.RemoteSendUseSSL := False;
      RemoteSendUseSSLCheckBox.Checked := False;
      ShowMessage('Generate SSL certificate first');
    end
  else
  begin
    RemoteSendUseSSLCheckBox.Checked := False;
    config.RemoteSendUseSSL := False;
  end;
end;

procedure TSetupForm.RemoteSendGenerateCertKeyButtonClick(Sender: TObject);
var
  OSSLDirname: string;
begin
  OSSLDirname := ExtractFilePath(application.exename) + 'openssl\';

  if fileExists(OSSLDirname + 'openssl.exe') then
    ShellExecute(Handle, 'open', PChar(OSSLDirname + 'openssl.exe'),
      'req -x509 -nodes -days 365 -newkey rsa:1024 -keyout key.pem -out cert.pem -subj "/C=LC/ST=DSM/L=ART/O=Mon organisation/CN=IE" -config openssl.cfg',
      PChar(OSSLDirname), SW_SHOWNORMAL)
  else
    ShowMessage('openssl\openssl.exe is missing');
end;


////////////// FOLDING/BOINC ENABLE/DISABLE /////////////////
procedure TSetupForm.FoldEnableCheckBoxClick(Sender: TObject);
begin
  config.foldEnabled := FoldEnableCheckBox.Checked;
end;

procedure TSetupForm.BOINCEnableCheckBoxClick(Sender: TObject);
begin
  config.boincEnabled := BOINCEnableCheckBox.Checked;
end;

end.
