unit UMain;

{$MODE Delphi}

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
 *  $Source: /root/lcdsmartie-cvsbackup/lcdsmartie/UMain.pas,v $
 *  $Revision: 1.93 $ $Date: 2011/06/04 16:48:30 $
 *****************************************************************************}

interface

uses
  Menus, Graphics, WinampCtrl, ExtCtrls, Controls, Buttons, Classes, Forms,
  USetup, UConfig, ULCD, UData, lcdline, UExceptionLogger, IdComponent,
  IdCustomTCPServer, IdTCPServer, IdContext, IdSSL, IdSSLOpenSSL, SysUtils,
  IdGlobal, IdIOHandler, IdIOHandlerStack, IdSSLOpenSSLHeaders, Windows, math,
  URLThread, stdctrls, LazFileUtils, dateutils;

  { TLCDSmartieDisplayForm }
type
  TActionURLThread = class(TThread)
  protected
    URLThread: TURLThread;
    procedure Execute; override;
  public
    Url: String;
    MaxFreq: Cardinal;
    PostParms: TStringList;
  end;

type
  PObject = ^TObject;

  TOnScreenLineWrapper = class
  private
    fOwner : PObject;
    fCaption : string;
    fTrueLCD : boolean;
    procedure SetVisible(Value : boolean);
    procedure SetLineWidth(Value : integer);
    procedure SetCaption(Value : string);
    procedure SetLineColor(Value : TColor);
    procedure SetFontColor(Value : TColor);
  protected
    property Visible : boolean write SetVisible;
    property LineWidth : integer write SetLineWidth;
    property Caption : string read fCaption write SetCaption;
    property LineColor : TColor write SetLineColor;
    property FontColor : TColor write SetFontColor;
  public
    constructor Create(AOwner : PObject; TrueLCD : boolean);
    destructor Destroy; override;
  end;

  TLCDSmartieDisplayForm = class(TForm)
    ExceptionLogger1: TExceptionLogger;
    ControlMenuItem: TMenuItem;
    CreditsMenuItem: TMenuItem;
    ToggleActionLogMenuItem: TMenuItem;
    SavePositionMenuItem: TMenuItem;
    N1: TMenuItem;
    NextScreenTimer: TTimer;
    ScrollFlashTimer: TTimer;
    TrayIcon1: TTrayIcon;
    WinampCtrl1: TWinampCtrl;
    // These are only used by us:
    PopupMenu1: TPopupMenu;
    ShowWindow1: TMenuItem;
    Close1: TMenuItem;
    LogoImage: TImage;
    Configure1: TMenuItem;
    BacklightOn1: TMenuItem;
    Commands1: TMenuItem;
    Freeze1: TMenuItem;
    NextTheme1: TMenuItem;
    LastTheme1: TMenuItem;
    N2: TMenuItem;
    NextScreenImage: TImage;
    PreviousImage: TImage;
    BarLeftImage: TImage;
    BarRightImage: TImage;
    BarMiddleImage: TImage;
    SetupImage: TImage;
    HideImage: TImage;
    ScreenNumberPanel: TPanel;
    TransitionTimer: TTimer;
    ActionsTimer: TTimer;
    LeftManualScrollTimer: TTimer;
    RightManualScrollTimer: TTimer;
    TimerRefresh: TTimer;
    IdServerIOHandlerSSLOpenSSL1: TIdServerIOHandlerSSLOpenSSL;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ReloadSkinMenuItemClick(Sender: TObject);
    procedure SavePositionClick(Sender: TObject);
    procedure ShowActionLogMenuItemClick(Sender: TObject);
    procedure ShowWindow1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure LogoImageClick(Sender: TObject);
    procedure HideButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BacklightOn1Click(Sender: TObject);
    procedure TimerRefreshTimer(Sender: TObject);
    procedure ActionsTimerTimer(Sender: TObject);
    procedure LeftManualScrollTimerTimer(Sender: TObject);
    procedure RightManualScrollTimerTimer(Sender: TObject);
    procedure NextScreenTimerTimer(Sender: TObject);
    procedure Credits1Click(Sender: TObject);
    procedure NextTheme1Click(Sender: TObject);
    procedure LastTheme1Click(Sender: TObject);
    procedure LogoImageDblClick(Sender: TObject);
    procedure Freeze1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure PreviousImageMouseUp(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure PreviousImageMouseDown(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure SetupImageMouseDown(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure SetupImageMouseUp(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure HideImageMouseDown(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure HideImageMouseUp(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure LineRightScrollImageMouseDown(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure LineRightScrollImageMouseUp(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure LineLeftScrollImageMouseDown(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure LineLeftScrollImageMouseUp(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure NextScreenImageMouseDown(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure NextScreenImageMouseUp(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure PreviousImageClick(Sender: TObject);
    procedure NextScreenImageClick(Sender: TObject);
    procedure TransitionTimerTimer(Sender: TObject);
    procedure ScrollFlashTimerTimer(Sender: TObject);
    procedure SetupButtonClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure LoadSkin;
    procedure LoadColors;
    procedure ServerConnect(AContext: TIdContext);
    procedure ServerExecute(AContext: TIdContext);
    procedure ServerDisconnect(AContext: TIdContext);

  private
    LineRightScrollImages: Array[1..MaxLines] of TImage;
    LineLCDPanels: Array[1..MaxLines] of TLCDLineFrame;
    xLinePanels: Array[1..Maxlines] of TPanel;
    LineLeftScrollImages: Array[1..MaxLines] of TImage;
    ScreenLCD: Array[1..MaxLines] of TOnScreenLineWrapper;
    parsedLine: Array[1..MaxLines] of String;
    scrollPos: Array[1..MaxLines] of Integer;
    Oldline: Array[1..MaxLines] of String;
    Newline: Array[1..MaxLines] of String;
    LastLineLCD: array [1..MaxLines] of string;
    tmpline: Array [1..MaxLines] of String;
    GuessArray: Array[1..MaxLines, 1..MaxCols] of Boolean;
    canflash: Boolean;
    bSavedEmulateLCD: boolean;
    iSavedHeight, iSavedWidth: Integer;
    iSavedColorMode: Integer;
    didAction: Array [1..MaxActions] of Boolean;
    file1: String;
    line2scroll: Integer;
    forgroundcoloroff, forgroundcoloron, backgroundcoloroff,
      backgroundcoloron: Integer;
    Gotnewlines: Boolean;
    TransStart: Cardinal;
    TransitionTemp, TransitionTemp2 : TTransitionStyle;
    TransCycle, TempTransitionTimerInterval : Integer;
    activetheme: Integer; canscroll: Boolean;
    GPO: Array [1..8] of Boolean;
    customChars: Array [1..8, 0..7] of Byte;
    customCharsChanged: Array [1..8] of Boolean;
    doesGPOflash: Boolean;
    GPOflash, whatGPO: Integer;
    flash: Integer;
    ResetContrast: Boolean;
    flashdelay: Cardinal;
    bNewScreen: Boolean;
    frozen: Boolean;
    NumberOfScreensToShift: Integer;
    iLastRandomTranCycle: Integer;
    ConfigFileName: String;
    RestartAsAdmin: boolean;
    IdTCPServer1: TIdTCPServer;
    scrollT: TDateTime;
    function DoGuess(line: Integer): Integer;
    procedure freeze();
    procedure DoGPO(const ftemp1, ftemp2: Integer);
    function scroll(const scrollvar: String;const line, speed: Integer): String;
    procedure scrollLine(line: Byte; direction: Integer);
    procedure DoTransitions;
    function EscapeAmp(const sStr: string):String;
    function UnescapeAmp(const sStr: string): String;
    procedure SendCustomChars;
    procedure ProcessAction(bDoAction: Boolean; sAction: String; ActionIndex: integer);
    procedure InitLCD();
    procedure FiniLCD(WriteShutdownMessage : boolean);
    procedure ResizeHeight;
    procedure ResizeWidth;
    procedure ProcessCommandLineParams;
    procedure AssignOnscreenDisplay(TrueLCD : boolean);
    procedure OnEndSession(Sender:Tobject);
    procedure LineLCDPanelsOnClick(Sender: Tobject);
  public
    Backlight: boolean;
    doesflash: Boolean;
    lcd: TLCD;
    Data: TData;
    PrevWndProc: WNDPROC;
    activeScreen : Integer;
    OurVersMaj : integer;
    OurVersMin : integer;
    OurVersRel : integer;
    OurVersBuild : integer;
    ShowWindowFlag: Boolean;
    DisplayError: boolean;
    StartTime: TDATETIME;
    sSkinDir: string;

    ActionLogForm: TForm;
    ActionLogMemo: TMemo;
    CurrentScreen: integer;
    CContext: TIdContext;
    procedure hidelog(Sender: TObject; var CanClose: Boolean);
    procedure SetOnscreenBacklight();
    procedure backlit(iOn: Integer = -1);
    procedure DoFullDisplayDraw;
    procedure UpdateTimersState(InSetupState : boolean);
    procedure ChangeScreen(scr: Integer);
    procedure ResetScrollPositions;
    procedure SetupAutoStart;
    procedure ReInitLCD();
    procedure customchar(fline: String);
    //function ProcessTimer(AWnd: HWND; AMsg: UINT; AIDEvent: UINT_PTR; ATicks: DWORD): uint;
    property ShowTrueLCD : boolean write AssignOnscreenDisplay;
  end;

  function GetFmtFileVersion(const FileName: String = '';  const Fmt: String = '%d.%d.%d.%d'): String;
var
  LCDSmartieDisplayForm: TLCDSmartieDisplayForm;
  SetupForm : tsetupform;
  bTerminating: Boolean = false;
implementation
{$IFNDEF STANDALONESETUP}
{$R *.lfm}
{$ENDIF}
uses
  Dialogs, ShellAPI, mmsystem, StrUtils,
  UCredits, ULCD_DLL, UUtils, lazutf8,
  FONTMGR, InterfaceBase, Win32Int, ComObj;

// power broadcast message handler
// lazarus only supports passing certain messages so we have to implement our own handler
function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam):LRESULT; stdcall;
const
  WM_POWERBROADCAST = $218;
  PBT_APMSUSPEND = 4;
  PBT_APMSTANDBY = 5;
  PBT_APMRESUMECRITICAL = 6;
  PBT_APMRESUMESUSPEND = 7;
  PBT_APMRESUMESTANDBY = 8;
  PBT_APMRESUMEAUTOMATIC = $012;
begin
  if ( uMsg =  WM_POWERBROADCAST) then
  begin
    if (wParam = PBT_APMRESUMEAUTOMATIC) or
       (wParam = PBT_APMRESUMECRITICAL) or
       (wParam = PBT_APMRESUMESTANDBY) or
       (wParam = PBT_APMRESUMESUSPEND)
    then
      LCDSmartieDisplayForm.ReInitLCD()

    else if (wParam = PBT_APMSUSPEND) or
            (wParam = PBT_APMSTANDBY)
    then
    begin
      LCDSmartieDisplayForm.FiniLCD(true);
      LCDSmartieDisplayForm.Lcd := TLCD.Create(); // replace with a dummy driver.
    end;
    result := 0;
  end
  else
    result:= CallWindowProc(LCDSmartieDisplayForm.PrevWndProc,Ahwnd,uMsg,WParam,LParam); // pass on all other messages
end;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////                                                                       ////
////      O N   S C R E E N   D I S P L A Y                                ////
////                                                                       ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

constructor TOnScreenLineWrapper.Create(AOwner : PObject; TrueLCD : boolean);
begin
  fCaption := '';
  fOwner := AOwner;
  fTrueLCD := TrueLCD;
end;

destructor TOnScreenLineWrapper.Destroy;
begin
  inherited;
end;

procedure TOnScreenLineWrapper.SetVisible(Value : boolean);
begin
  if fTrueLCD then (fOwner^ as TLCDLineFrame).Visible := Value
  else (fOwner^ as TPanel).Visible := Value;
end;

procedure TOnScreenLineWrapper.SetLineWidth(Value : integer);
var
  iDelta : integer;
begin
  if fTrueLCD then (fOwner^ as TLCDLineFrame).LineWidth := value
  else begin
    iDelta := 321 - ((321 * Value) div 40);
    (fOwner^ as TPanel).Width := 321 - iDelta;
  end;
end;

procedure TOnScreenLineWrapper.SetCaption(Value : string);
begin
  fCaption := Value;
  if fTrueLCD then begin
    (fOwner^ as TLCDLineFrame).Caption := Value
  end else
    (fOwner^ as TPanel).Caption := Value;
end;

procedure TOnScreenLineWrapper.SetLineColor(Value : TColor);
begin
  if fTrueLCD then
    (fOwner^ as TLCDLineFrame).LineColor := Value
  else
    (fOwner^ as TPanel).Color := Value;
end;

procedure TOnScreenLineWrapper.SetFontColor(Value : TColor);
begin
  if fTrueLCD then
    (fOwner^ as TLCDLineFrame).FontColor := Value
  else
    (fOwner^ as TPanel).Font.Color := Value;
end;

procedure TLCDSmartieDisplayForm.OnEndSession(Sender: Tobject);
begin
  close;
end;

procedure TLCDSmartieDisplayForm.LineLCDPanelsOnClick(Sender: Tobject);
begin
  backlit();
end;

procedure TLCDSmartieDisplayForm.AssignOnscreenDisplay(TrueLCD : boolean);
var
  Loop : byte;
begin
  if assigned(ScreenLCD[1]) and (ScreenLCD[1].fTrueLCD = TrueLCD) then exit;

  for Loop := 1 to MaxLines do begin
    if assigned(ScreenLCD[Loop]) then begin
      ScreenLCD[Loop].Visible := false;
      ScreenLCD[Loop].Free;
    end;

    if TrueLCD then
    begin
      LineLCDPanels[loop].OnLineClicked := LineLCDPanelsOnClick; // so we can click on the virtual display for lights out
      ScreenLCD[loop] := TOnScreenLineWrapper.Create(@LineLCDPanels[loop],TrueLCD);
    end
    else
    begin
      xLinePanels[loop].OnClick := LineLCDPanelsOnClick;
      ScreenLCD[loop] := TOnScreenLineWrapper.Create(@xLinePanels[loop],TrueLCD);
    end;
    ScreenLCD[loop].visible := config.height > loop - 1;
  end;

  SetOnscreenBacklight;
end;

function RunAsAdmin(const Handle: Hwnd; const Path, Params: string): Boolean;
var
  sei: TShellExecuteInfoA;
begin
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.Wnd := Handle;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT;
  sei.lpVerb := 'runas';
  sei.lpFile := PAnsiChar(Path);
  sei.lpParameters := PAnsiChar(Params);
  sei.nShow := SW_SHOWNORMAL;
  Result := ShellExecuteExA(@sei);
end;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////                                                                       ////
////      M A I N      F O R M     E V E N T S                             ////
////                                                                       ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

procedure TLCDSmartieDisplayForm.hidelog(Sender: TObject; var CanClose: Boolean);
begin
  ActionLogForm.Hide;
  CanClose := False;
end;

procedure TLCDSmartieDisplayForm.FormCreate(Sender: TObject);
var
  hConfig: longint;
  i: integer;
  allParameters: string;
  loop: byte;
begin
  StartTime := now;
  // this to fix/work around a problem with lazarus and exceptions not being dealt with properly in DLLs
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

  DisplayError := false;
  application.OnEndSession := OnEndSession; // lazarus has support for this message
  PrevWndProc:= Windows.WNDPROC(SetWindowLongPtr(Self.Handle,GWL_WNDPROC,PtrInt(@WndCallback))); // message handler

  // keep all ssl related stuff in a sub dir
  IdOpenSSLSetLibPath('.\openssl\');

  // construct virtual display
  // create the form dynamically to allow for more lines in future
    for Loop := 1 to MaxLines do begin

      LineRightScrollImages[loop] := TImage.Create(nil);
      LineRightScrollImages[loop].Parent := LCDSmartieDisplayForm;
      LineRightScrollImages[loop].Left := 16;
      LineRightScrollImages[loop].Height := 16;
      LineRightScrollImages[loop].Hint := 'Scroll line right.';
      LineRightScrollImages[loop].Top := 16 * (loop - 1);
      LineRightScrollImages[loop].Width := 16;
      LineRightScrollImages[loop].OnMouseDown := LineRightScrollImageMouseDown;
      LineRightScrollImages[loop].OnMouseUp := LineRightScrollImageMouseUp;
      LineRightScrollImages[loop].ParentShowHint := False;
      LineRightScrollImages[loop].ShowHint := True;
      LineRightScrollImages[loop].Name := 'ScrollR' + inttostr(loop); // this is parsed by LineScrollImageMouseDown
      LineRightScrollImages[loop].Visible := false;

      LineLCDPanels[loop] := TLCDLineFrame.Create(nil);
      LineLCDPanels[loop].Parent := LCDSmartieDisplayForm;
      LineLCDPanels[loop].Left := 32;
      LineLCDPanels[loop].Top := 16 * (loop - 1);
      LineLCDPanels[loop].Height := 16;
      LineLCDPanels[loop].Hint := 'Virtual display.';
      LineLCDPanels[loop].DoubleBuffered := True;
      LineLCDPanels[loop].ShowHint := True;
      LineLCDPanels[loop].Visible := false;

      xLinePanels[loop] := TPanel.Create(nil);
      xLinePanels[loop].Parent := LCDSmartieDisplayForm;
      xLinePanels[loop].Left := 32;
      xLinePanels[loop].Top := 16 * (loop - 1);
      xLinePanels[loop].Height := 16;
      xLinePanels[loop].Visible := false;
      xLinePanels[loop].BorderStyle := bsNone;
      xLinePanels[loop].Hint := 'Virtual display.';
      xLinePanels[loop].Alignment := taLeftJustify;
      xLinePanels[loop].BevelOuter := bvNone;
      xLinePanels[loop].Color := clNone;
      xLinePanels[loop].DoubleBuffered := True;
      xLinePanels[loop].Font.CharSet := 4;
      xLinePanels[loop].Font.Color := clWindowText;
      xLinePanels[loop].Font.Height := -14;
      xLinePanels[loop].Font.Name := 'Modern';
      xLinePanels[loop].Font.Pitch := fpFixed;
      xLinePanels[loop].ParentColor := False;
      xLinePanels[loop].ParentDoubleBuffered := False;
      xLinePanels[loop].ParentFont := False;
      xLinePanels[loop].ParentShowHint := False;
      xLinePanels[loop].ShowHint := True;
      xLinePanels[loop].Visible := false;

      LineLeftScrollImages[loop] := TImage.Create(nil);
      LineLeftScrollImages[loop].Parent := LCDSmartieDisplayForm;
      LineLeftScrollImages[loop].Left := 16;
      LineLeftScrollImages[loop].Height := 16;
      LineLeftScrollImages[loop].Hint := 'Scroll line left.';
      LineLeftScrollImages[loop].Top := 16 * (loop - 1);
      LineLeftScrollImages[loop].Width := 16;
      LineLeftScrollImages[loop].OnMouseDown := LineLeftScrollImageMouseDown;
      LineLeftScrollImages[loop].OnMouseUp := LineLeftScrollImageMouseUp;
      LineLeftScrollImages[loop].ParentShowHint := False;
      LineLeftScrollImages[loop].ShowHint := True;
      LineLeftScrollImages[loop].Name := 'ScrollL' + inttostr(loop); // this is parsed by LineScrollImageMouseDown
      LineLeftScrollImages[loop].Visible := false;;
    end;

  fillchar(ScreenLCD,sizeof(ScreenLCD),$00);
  Randomize;

  SetCurrentDir(extractfilepath(application.exename));
  CreateDirectory('cache', nil);
  CreateDirectory('plugins', nil);
  CreateDirectory('displays', nil);

  if DirectoryExists('skins') then
    sSkinDir := 'skins\'
  else if DirectoryExists('images') then
    sSkinDir := 'images\'
  else
    sSkinDir := 'skins\';

  //AddPluginsToPath();  // I don't think we need to do this for this program

  ConfigFileName := 'config.ini';
  ProcessCommandLineParams;  // can change config file name

  config := TConfig.Create(ConfigFileName);

  if (config.load() = false) then
  begin
    if FileExists(ConfigFileName) then
    begin
      showmessage('Fatal Error:  Failed to load configuration ('+ConfigFileName+')');
      application.Terminate;
    end
    else
      hConfig := FileCreate (ConfigFileName); // create empty config
      If hConfig=-1 then begin
        FileClose(hConfig);
        showmessage('Default configuration ('+ConfigFileName+') could not be created');
        application.Terminate;
      end;
      FileClose(hConfig);
      config.load(); // load default values for empty config
      config.save(); // save default values
      showmessage('Default configuration ('+ConfigFileName+') created')
  end;

  if not IsAdministrator and config.bStartAsAdmin then
        RestartAsAdmin := true;

  if RestartAsAdmin then
  begin
    for i := 1 to ParamCount do
      allParameters := allParameters+' '+ParamStr(i);

    application.MainForm.Hide;
    application.Terminate;
    RunAsAdmin(0, application.ExeName, allParameters);
    exit;
   end;


  ActionLogForm := TForm.Create(nil);
  ActionLogForm.Name := 'ActionsLog';
  ActionLogForm.Caption := 'Actions Log';
  ActionLogForm.SetBounds(config.ActionLogLeft, config.ActionLogTop, config.ActionLogWidth, config.ActionLogHeight);
  ActionLogMemo := TMemo.create(nil);
  ActionLogMemo.Align := alClient;
  ActionLogMemo.Parent := ActionLogForm;
  ActionLogMemo.ReadOnly := true;
  ActionLogForm.OnCloseQuery := hidelog;

  while (pos('\', config.sSkinPath) > 0) do
    config.sSkinPath := copy(config.sSkinPath, pos('\', config.sSkinPath) + 1 , 255);

  if (config.alwaysOnTop) then
    FormStyle := fsSystemStayOnTop
  else
    FormStyle := fsNormal;

  if (Config.MainFormCaption = '') then
    LCDSmartieDisplayForm.Caption := 'LCD Smartie ' + GetFmtFileVersion()
  else
    LCDSmartieDisplayForm.Caption := Config.MainFormCaption;

  if config.AppendConfigName then
    LCDSmartieDisplayForm.Caption := LCDSmartieDisplayForm.Caption + ' ' + ExtractFileNameWithoutExt(extractfilename(config.filename));

  trayicon1.Hint:=LCDSmartieDisplayForm.Caption;
  ShowTrueLCD := Config.EmulateLCD;

  LoadSkin;
  LCDSmartieDisplayForm.color := $00BFBFBF;
  NumberOfScreensToShift := 1;
  LoadColors;

  // delete/create startup shortcut as required.
  SetupAutoStart();

  Data := TData.Create();

  LCDSmartieDisplayForm.WinampCtrl1.WinampLocation := config.winampLocation;
  file1 := config.distLog;

  Backlight := true;
  SetOnscreenBacklight();

  InitLCD();

  ChangeScreen(1);

  LCDSmartieDisplayForm.Top  := config.MainFormPosTop;
  LCDSmartieDisplayForm.Left := config.MainFormPosLeft;

  LCDSmartieDisplayForm.Visible:=true;
  if (config.bHideOnStartup) or not (ShowWindowFlag)  then
    ShowWindow1Click(Sender);

// start sender server
  if config.EnableRemoteSend then
  begin
    try
      IdTCPServer1 := TIdTCPServer.Create;
      if fileExists(ExtractFilePath(ParamStr(0))+'openssl\cert.pem') and
        fileExists(ExtractFilePath(ParamStr(0))+'openssl\key.pem')  and
        config.RemoteSendUseSSL then
      begin
        IdServerIOHandlerSSLOpenSSL1.SSLOptions.CertFile := ExtractFilePath(ParamStr(0))+'openssl\cert.pem';
        IdServerIOHandlerSSLOpenSSL1.SSLOptions.KeyFile := ExtractFilePath(ParamStr(0))+'openssl\key.pem';
        IdServerIOHandlerSSLOpenSSL1.SSLOptions.VerifyMode := [];
        IdServerIOHandlerSSLOpenSSL1.SSLOptions.VerifyDepth  := 0;
        IdServerIOHandlerSSLOpenSSL1.SSLOptions.SSLVersions := [sslvTLSv1_2];
        IdTCPServer1.IOHandler := IdServerIOHandlerSSLOpenSSL1;
      end;

      IdTCPServer1.DefaultPort := strtoint(config.RemoteSendPort);
      if not (config.RemoteSendBindIP = '0.0.0.0') and
       not (config.RemoteSendBindIP = '') then
      IdTCPServer1.Bindings.Add.IP := config.RemoteSendBindIP;

      IdTCPServer1.Bindings.Add.Port := strtoint(config.RemoteSendPort);
      IdTCPServer1.OnConnect := ServerConnect;
      IdTCPServer1.OnExecute := ServerExecute;
      IdTCPServer1.OnDisconnect := ServerDisconnect;
      IdTCPServer1.Active := True;
    except
      on E : Exception do
        ShowMessage('Remote Display:'+#13#10+E.Message);
    end;
  end;

end;

procedure TLCDSmartieDisplayForm.LoadSkin;
var
  sSkinPath: String;
  IconPath: string;
  hIcon: TIcon;
  loop: byte;
begin
  try
    sSkinPath := extractfilepath(application.exename) + sSkinDir + config.sSkinPath + '\';
    LogoImage.picture.LoadFromFile(sSkinPath + 'logo.bmp');

    for loop := 1 to MaxLines do
    begin
      LineRightScrollImages[loop].picture.LoadFromFile(sSkinPath + 'small_arrow_left_up'+inttostr(loop)+'.bmp');
      LineLeftScrollImages[loop].picture.LoadFromFile(sSkinPath + 'small_arrow_right_up'+inttostr(loop)+'.bmp');
    end;

    NextScreenImage.picture.LoadFromFile(sSkinPath + 'big_arrow_right_up.bmp');
    PreviousImage.picture.LoadFromFile(sSkinPath + 'big_arrow_left_up.bmp');
    BarLeftImage.picture.LoadFromFile(sSkinPath + 'bar_left.bmp');
    BarRightImage.picture.LoadFromFile(sSkinPath + 'bar_right.bmp');
    BarMiddleImage.picture.LoadFromFile(sSkinPath + 'bar_middle.bmp');
    SetupImage.picture.LoadFromFile(sSkinPath + 'setup_up.bmp');
    HideImage.picture.LoadFromFile(sSkinPath + 'hide_up.bmp');

    if config.sTrayIcon = 'default for this skin' then
      IconPath := sSkinpath + '\smartie.ico'
    else
      IconPath := extractfilepath(application.exename) + sSkinDir + config.sTrayIcon;

    TrayIcon1.Icon.LoadFromFile(IconPath);
    application.Icon.LoadFromFile(IconPath);
    config.SkinError := false;
  except
    on E: Exception do
    begin
      if not config.SkinError then
      showmessage('Unable to load skin from ' + sSkinPath +
      #13#10 + 'Error was: ' +E.Message +
      #13#10 + 'Will use embedded skin instead');

      config.SkinError := true;

      LogoImage.picture.LoadFromResourceName(HInstance, 'logo');
      for loop := 1 to MaxLines do
      begin
        LineRightScrollImages[loop].picture.LoadFromResourceName(HInstance, 'small_arrow_left_up'+inttostr(loop));
        LineLeftScrollImages[loop].picture.LoadFromResourceName(HInstance, 'small_arrow_right_up'+inttostr(loop));
      end;

      NextScreenImage.picture.LoadFromResourceName(HInstance, 'big_arrow_right_up');
      PreviousImage.picture.LoadFromResourceName(HInstance, 'big_arrow_left_up');
      BarLeftImage.picture.LoadFromResourceName(HInstance, 'bar_left');
      BarRightImage.picture.LoadFromResourceName(HInstance, 'bar_right');
      BarMiddleImage.picture.LoadFromResourceName(HInstance, 'bar_middle');
      SetupImage.picture.LoadFromResourceName(HInstance, 'setup_up');
      HideImage.picture.LoadFromResourceName(HInstance, 'hide_up');

      TrayIcon1.Icon := application.Icon;
      ///application.Icon.LoadFromResourceName(HInstance, 'smartie');
    end;
  end;
end;

procedure TLCDSmartieDisplayForm.LoadColors;
var
  Line : string;
  initfile: textfile;
begin
//register
  try
    assignfile(initfile, extractfilepath(application.exename) + sSkinDir + config.sSkinPath + '\colors.cfg');
    reset(initfile);
    readln(initfile, line);
    ScreenNumberPanel.Color := StrToInt('$00' + copy(line, 1, 6));
    readln(initfile, line);
    ScreenNumberPanel.font.Color := StrToInt('$00' + copy(line, 1, 6));
    readln(initfile, line);
    forgroundcoloron := StrToInt('$00' + copy(line, 1, 6));
    readln(initfile, line);
    backgroundcoloron := StrToInt('$00' + copy(line, 1, 6));
    readln(initfile, line);
    forgroundcoloroff := StrToInt('$00' + copy(line, 1, 6));
    readln(initfile, line);
    backgroundcoloroff := StrToInt('$00' + copy(line, 1, 6));
    closefile(initfile);
    // toggle backlight to force color reload
    SetOnscreenBacklight;
    SetOnscreenBacklight;
  except
    on E: Exception do
    begin
      if not config.SkinError then
      showmessage('Unable to load skin colors from ' + extractfilepath(application.exename) +
        config.sSkinPath + '\colors.cfg' +
        #13#10 + 'Error was: ' + E.Message +
        #13#10 + 'Will use embedded skin colors instead');

      ScreenNumberPanel.Color := $7F0000;
      ScreenNumberPanel.font.Color := $FFFFFF;
      forgroundcoloron := $000000;
      backgroundcoloron := $FFFFFF;
      forgroundcoloroff := $000000;
      backgroundcoloroff := $CECECE;
      SetOnscreenBacklight;
      SetOnscreenBacklight;
    end;
  end;
end;

procedure TLCDSmartieDisplayForm.ProcessCommandLineParams;
var
  I: integer;
  parameter: String;
begin
  ShowWindowFlag := True;
  i := 1;
  while (i <= ParamCount) do
  begin
    parameter := LowerCase(ParamStr(i));

    if (parameter = '-hide') then
      ShowWindowFlag := False;

    if (parameter = '-totalhide') then
      ShowWindowFlag := False;

    if (parameter = '-config') then
    begin
      Inc(i);
      ConfigFileName := ParamStr(i);  // will give '' if out of range
    end;

    if not IsAdministrator then
      if (parameter = '-admin') then
        RestartAsAdmin := true;

    Inc(i);
  end;
end;

procedure TLCDSmartieDisplayForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  FormatSettings : TFormatSettings;
  TimeDateString: string;
  BackupNameList: TStringList;
  SR: TSearchRec;
  aTime: LongInt;
  configName: string;
begin

  if assigned(IdTCPServer1) then
  begin
    if assigned(CContext) then
    begin
      CContext.Connection.Disconnect(true);
    end;
    IdTCPServer1.Active := False;
    IdTCPServer1.Free;
  end;

  config.ActionLogLeft := ActionLogForm.Left;
  config.ActionLogTop := ActionLogForm.Top;
  config.ActionLogWidth := ActionLogForm.Width;
  config.ActionLogHeight := ActionLogForm.Height;

  config.MainFormPosTop := LCDSmartieDisplayForm.Top;
  config.MainFormPosLeft := LCDSmartieDisplayForm.Left;
  config.save;
  configName := config.filename;
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);
  SetCurrentDir(extractfilepath(application.exename));
  TimeDateString := DateTimeToStr(now, FormatSettings);
  if not DirectoryExists('backup') then
    if not CreateDir('backup') then
    ShowMessage('Could not create backup directory');

  TimeDateString := StringReplace(TimeDateString, '/', '-', [rfReplaceAll]);
  TimeDateString := StringReplace(TimeDateString, ':', '-', [rfReplaceAll]);
  TimeDateString := StringReplace(TimeDateString, ' ', '_', [rfReplaceAll]);

  if not CopyFile(pchar(configName), pchar('backup\'+ExtractFileNameWithoutExt(extractfilename(configName))+'_'+TimeDateString+'.ini'), false) then
    ShowMessage('Could not create config backup');

  BackupNameList := TStringList.Create;

  aTime := DateTimeToFileDate(Now);
  try
    if FindFirst('backup\'+ExtractFileNameWithoutExt(extractfilename(configName))+'_*', faAnyFile and not faDirectory, SR) = 0 then
    repeat
      if sr.Time < aTime then
      begin
        aTime := sr.Time;
        BackupNameList.Insert(0, sr.Name);
      end
      else
        BackupNameList.Add(sr.Name);
    until FindNext(SR) <> 0;
    SysUtils.FindClose(SR);

    While BackupNameList.Count > 10 do // make this a config option?
    begin
      DeleteFile(pchar('backup\'+BackupNameList[0]));
      BackupNameList.Delete(0);
    end;
  finally
    BackupNameList.Free;
  end;

  bTerminating := true;
  while timerRefresh.enabled = true do timerRefresh.enabled := false;
  while ActionsTimer.enabled = true do ActionsTimer.enabled := false;
  while LeftManualScrollTimer.enabled = true do LeftManualScrollTimer.enabled := false;
  while RightManualScrollTimer.enabled = true do RightManualScrollTimer.enabled := false;
  while NextScreenTimer.enabled = true do NextScreenTimer.enabled := false;
  while ScrollFlashTimer.enabled = true do ScrollFlashTimer.enabled := false;
  while TransitionTimer.enabled = true do TransitionTimer.enabled := false;
  FiniLCD(true);

  while Assigned(Data) do
  begin
    if Data.CanExit() then
    begin
      Data.free;
      Data := nil;
    end;
  end;

  while Assigned(config) do
  begin
    config.free;
    config := nil;
  end;
end;

procedure TLCDSmartieDisplayForm.FormShow(Sender: TObject);
begin
  if assigned(config) then
  begin
    if config.Snapping then
    begin
      SnapOptions.SnapFormTarget := true;
      SnapOptions.SnapToForms := true;
      SnapOptions.SnapToMonitor := true;
    end
    else
    begin
      SnapOptions.SnapFormTarget := false;
      SnapOptions.SnapToForms := false;
      SnapOptions.SnapToMonitor := false;
    end;

    // restore window position from config
    LCDSmartieDisplayForm.Top  := config.MainFormPosTop;
    LCDSmartieDisplayForm.Left := config.MainFormPosLeft;
    timerRefresh.Interval := 1; // make it short in case minimized has been selected.
    //TrayIcon1.ShowIcon:=true;
  end;
end;

procedure TLCDSmartieDisplayForm.ReloadSkinMenuItemClick(Sender: TObject);
begin
  LoadSkin;
end;

procedure TLCDSmartieDisplayForm.SavePositionClick(Sender: TObject);
begin
  config.ActionLogLeft := ActionLogForm.Left;
  config.ActionLogTop := ActionLogForm.Top;
  config.ActionLogWidth := ActionLogForm.Width;
  config.ActionLogHeight := ActionLogForm.Height;

  config.MainFormPosTop := LCDSmartieDisplayForm.Top;
  config.MainFormPosLeft := LCDSmartieDisplayForm.Left;
  config.save;
end;

procedure TLCDSmartieDisplayForm.ShowActionLogMenuItemClick(Sender: TObject);
begin
  if ActionLogForm.Visible then
    ActionLogForm.Hide
  else
    ActionLogForm.Show;
end;


procedure TLCDSmartieDisplayForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if upcase(key)='Z' then winampctrl1.Previous;
  if upcase(key)='X' then winampctrl1.Play;
  if upcase(key)='C' then winampctrl1.Pause;
  if upcase(key)='V' then winampctrl1.Stop;
  if upcase(key)='B' then winampctrl1.Next;
  if upcase(key)='N' then backlit();
  if upcase(key)='M' then freeze();
  if upcase(key)='K' then
  begin
    activetheme := activetheme-1;
    if activetheme=-1 then activetheme := 9;
    frozen := true;
    freeze();
  end;
  if upcase(key)='L' then
  begin
    activetheme := activetheme + 1;
    if activetheme = 10 then activetheme := 0;
    frozen := true;
    freeze();
  end;
  if upcase(key)=',' then
    begin
      NumberOfScreensToShift := -1;
      frozen := true;
      freeze();
    end;
  if upcase(key)='.' then
    begin
      NumberOfScreensToShift := 1;
      frozen := true;
      freeze();
    end;

  if (upcase(key)='?') or (upcase(key)='/') then
  begin
    Data.RefreshDataThreads;
  end;
end;

procedure TLCDSmartieDisplayForm.ResizeHeight;
var
  iDelta: Integer;
  loop: byte;
begin
  for loop := 1 to MaxLines do
  begin
    ScreenLCD[loop].visible := config.height > loop - 1;
    LineRightScrollImages[loop].visible := config.height > loop - 1;
    LineLeftScrollImages[loop].visible := config.height > loop - 1;
  end;

  iDelta := 16 * (4-config.height);

  SetupImage.Top := 69 - iDelta;
  BarLeftImage.Top := 64 - iDelta;
  BarMiddleImage.Top := 64 - iDelta;
  ScreenNumberPanel.Top := 72 - iDelta;
  BarRightImage.Top := 64 - iDelta;
  HideImage.Top := 69 - iDelta;
  LogoImage.Top := 64 - iDelta;
  ClientHeight := 90 - iDelta;

  NextScreenImage.Height := 64 - iDelta;
  PreviousImage.Height := 64 - iDelta;
  NextScreenImage.Stretch := (config.height <> 4);
  PreviousImage.Stretch := (config.height <> 4);
end;

procedure TLCDSmartieDisplayForm.ResizeWidth;
var
  h: Integer;
  iDelta: Integer;
  iTempWidth: Integer;
begin
  iTempWidth := config.width;
  if (iTempWidth < 20) then iTempWidth := 20;

  iDelta := 318 - ((318 * iTempWidth) div 40);

  Width := 382 - iDelta;
  LogoImage.left := 356 - iDelta;
  NextScreenImage.left := 368 - iDelta;
  BarRightImage.left := 266 - iDelta;
  HideImage.left := 323 - iDelta;
  for h := 1 to MaxLines do
  begin
    LineLeftScrollImages[h].Left := 352 - iDelta;
    ScreenLCD[h].LineWidth := iTempWidth;
  end;
  BarMiddleImage.width := 220 - iDelta;
  BarMiddleImage.Stretch := true;
  if (config.width >= 40) then
  begin
    ScreenNumberPanel.left := (Width - 130)  div 2;//115;
    ScreenNumberPanel.width := 130;
    ScreenNumberPanel.Caption := 'Theme: ' + IntToStr(activetheme + 1) + ' Screen: ' +
      IntToStr(activeScreen)
  end
  else
  begin
    ScreenNumberPanel.left := (87 * iTempWidth) div 20;
    ScreenNumberPanel.width := 33;
    ScreenNumberPanel.Caption := IntToStr(activetheme + 1) + ' | ' + IntToStr(activeScreen);
  end;
  CurrentScreen := activeScreen;
end;

procedure TLCDSmartieDisplayForm.PopupMenu1Popup(Sender: TObject);
begin
  if Visible then
    ShowWindow1.caption := 'Hide'
  else
    ShowWindow1.caption := 'Show';
end;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////                                                                       ////
////      T I M E R      E V E N T S                                       ////
////                                                                       ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

procedure TLCDSmartieDisplayForm.TransitionTimerTimer(Sender: TObject);
begin
  TransitionTimer.Enabled := false;
end;

procedure TLCDSmartieDisplayForm.ScrollFlashTimerTimer(Sender: TObject);
begin
  ScrollFlashTimer.Interval := 0;
  ScrollFlashTimer.Interval := config.scrollPeriod;
  canscroll := true;
  Inc(flashdelay);
  if ((not doesflash) and (flashdelay >= 2)) or
    (doesflash and (flashdelay >= 1)) then
  begin
    flashdelay := 0;
    canflash := true;
  end;
end;

procedure TLCDSmartieDisplayForm.ActionsTimerTimer(Sender: TObject);
//ACTIONS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
var
  counter: Integer;
  cKey: Char;
  iLeftValue, iRightValue: Integer;
  sLeftValue, sRightValue, sAction: String;
  bNum: Boolean;
  doAction: Array[1..MaxActions] of Boolean;
begin
  ActionsTimer.Interval := config.ActionsTimer;

  if (Lcd <> nil) and (Lcd.readKey(cKey)) then
  begin
    UpdateSetupForm(cKey);
    data.cLastKeyPressed := cKey;
  end;

  if assigned(SetupForm) then exit;

  //
  // Work out the state of each action condition.
  //
  for counter := 1 to config.totalactions do
  begin
    doAction[counter] := false;

    if lowercase(config.actionsArray[counter, 5]) = 'false' then
      continue; // skip as this action is disabled

    sLeftValue := Data.change(config.actionsArray[counter, 1]);
    sRightValue := config.actionsArray[counter, 3];
    bNum := false;

    if Trystrtoint(sLeftValue,iLeftValue) and Trystrtoint(sRightValue,iRightValue) then begin
      bNum := true;
      case StrToInt(config.actionsArray[counter, 2]) of
        0: if (iLeftValue > iRightValue) then doAction[counter] := true;
        1: if (iLeftValue < iRightValue) then doAction[counter] := true;
        2: if (iLeftValue = iRightValue) then doAction[counter] := true;
        3: if (iLeftValue <= iRightValue) then doAction[counter] := true;
        4: if (iLeftValue >= iRightValue) then doAction[counter] := true;
        5: if (iLeftValue <> iRightValue) then doAction[counter] := true;
      end;
    end;

    if (not bNum) then
    begin
      // not a numeric value - lets do a string comparsion
      case StrToInt(config.actionsArray[counter, 2]) of
        0: if (sLeftValue > sRightValue) then doAction[counter] := true;
        1: if (sLeftValue < sRightValue) then doAction[counter] := true;
        2: if (sLeftValue = sRightValue) then doAction[counter] := true;
        3: if (sLeftValue <= sRightValue) then doAction[counter] := true;
        4: if (sLeftValue >= sRightValue) then doAction[counter] := true;
        5: if (sLeftValue <> sRightValue) then doAction[counter] := true;
      end;
    end;
  end;

  // All actions have been processed using this key.
  // Delete it so a repeated press is processed.
  data.cLastKeyPressed := Chr(0);

  // Reset new screen - the following actions may set this again.
  data.NewScreen(False);

  //
  // Run any required actions.
  //
  for counter := 1 to config.totalactions do
  begin
    if (doAction[counter] <> didAction[counter]) then
    begin
      sAction :=  config.actionsArray[counter, 4];
      ProcessAction(doAction[counter], sAction, counter);
      didAction[counter] := doAction[counter];
    end;

    // Ugly special case - [the action code needs a rewrite]
    // If action was caused by a key press then don't record that we have
    // done it - this will reduce the delay required to reset actions.
    // This delay impacts the user experience when using keys.
    if (Pos('MObutton', config.actionsArray[counter, 1]) <> 0) then
      didAction[counter] := false;
  end;
end;

// For scrolling right when a line scroll button is pressed.
procedure TLCDSmartieDisplayForm.LeftManualScrollTimerTimer(Sender: TObject);
begin
  scrollLine(line2scroll, 1);
end;

// For scrolling left when a line scroll button is pressed.
procedure TLCDSmartieDisplayForm.RightManualScrollTimerTimer(Sender: TObject);
begin
  scrollLine(line2scroll, -1);
end;

procedure TLCDSmartieDisplayForm.NextScreenTimerTimer(Sender: TObject);
//NEXT SCREEN!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
var
  ScreenCount, TotalScreenCount: Integer;
  ascreen: TScreen;
  tmpscreen: Integer;
  FindAnotherScreen : boolean;

begin
  tmpScreen := activeScreen;
  TotalScreenCount := 0;
  ScreenCount := 0;
  FindAnotherScreen := true;
  while FindAnotherScreen do begin
    inc(TotalScreenCount);
    inc(ScreenCount);
    if (config.randomScreens) and (TotalScreenCount < 500) then
    begin
      tmpScreen := round(random(MaxScreens) + 1);
      if tmpScreen > MaxScreens then tmpScreen := MaxScreens;
      if tmpScreen < 1 then tmpScreen := 1;
    end
    else
    begin
      tmpScreen := tmpScreen + NumberOfScreensToShift;
      if tmpScreen > MaxScreens then tmpScreen := 1;
      if tmpScreen < 1 then tmpScreen := MaxScreens;
    end;

    if ScreenCount > (MaxScreens+2) then
    begin
      activetheme := activetheme + 1;
      ScreenCount := 0;
    end;
    if (((TotalScreenCount > (MaxScreens+2)*(MaxThemes+1)) and (not config.randomScreens)) or ((TotalScreenCount> 1000) and
      (config.randomScreens))) then
    begin

      // It seems that we are in a endless loop because no screen is able to be
      // displayed.  Force screen 1 to be displayed.
      TotalScreenCount := 0;

      config.screen[1].settings.enabled := True;

      tmpScreen := 1;
      activetheme := 0;
    end;

    ascreen := config.screen[tmpScreen];
    FindAnotherScreen := false;
    if (ascreen.settings.theme <> activetheme) then FindAnotherScreen := true;
    if (not ascreen.settings.enabled) then FindAnotherScreen := true;
  end;

  NumberOfScreensToShift := 1;

  if (activeScreen <> tmpScreen) then
  begin
    ChangeScreen(tmpScreen); // changes activeScreen
    TransitionTimer.Enabled := True;
  end;
end;

procedure TLCDSmartieDisplayForm.TimerRefreshTimer(Sender: TObject);
var
  counter, h, loop: Integer;
  line, CCharLine: String;
  scrolldone: boolean;
begin
  timerRefresh.Interval := 0;
  timerRefresh.Interval := config.refreshRate;

  if ((gotnewlines = false) OR (TransitionTimer.enabled = false))then
  begin
    if (bNewScreen) and (gotnewlines) then
    begin
       bNewScreen := False;
       customchar('1, 12, 18, 18, 12, 0, 0, 0, 0');
       customchar('2, 31, 31, 31, 31, 31, 31, 31, 31');
       customchar('3, 16, 16, 16, 16, 16, 16, 31, 16');
       customchar('4, 28, 28, 28, 28, 28, 28, 31, 28');
    end;

    for loop := 1 to 8 do
      if pos('$CustomChar', config.screen[activeScreen].CustomCharacters[loop]) > 0 then
      begin
        CCharLine := copy(config.screen[activeScreen].CustomCharacters[loop],
        13,
        length(config.screen[activeScreen].CustomCharacters[loop]) - 13);
        Delete(CCharLine, -1,1);
        customchar(CCharLine);
      end;

    if iSavedColorMode <> config.colorOption then
    begin
      iSavedColorMode := config.colorOption;
      SetOnscreenBacklight();
    end;

    if (config.width <> iSavedWidth) or (config.EmulateLCD <> bSavedEmulateLCD) then
    begin
      iSavedWidth := config.width;
      bSavedEmulateLCD := config.EmulateLCD;
      ResizeWidth();
    end;

    if config.height <> iSavedHeight then
    begin
      iSavedHeight := config.height;
      ResizeHeight();
    end;

    if (canflash) then
    begin
      canflash := false;
      doesflash := not doesflash;
      if (flash > 0) then
      begin
        flash := flash -1;
        backlit()
      end;
    end;

    Data.ScreenStart();
    for counter := 1 to config.height do
    begin
      //Application.ProcessMessages;
      line := config.screen[activeScreen].line[counter].text;

      line := Data.change(line, counter, true);

      // Center the line if requested.
      if config.screen[activeScreen].line[counter].center then
        line := CenterText(line, config.width);

      parsedLine[counter] := line;
      newline[counter] := line;  // Used by screen change transition.
    end;

    if (not TransitionTimer.enabled) then SendCustomChars();
    Data.ScreenEnd();

    for h := 1 to MaxLines do
    begin
      // handle continuing on the next line (if req)
      if (h < MaxLines) and (config.screen[activeScreen].line[h].contNextLine) then
      begin
        newline[h + 1] := copy(newline[h], 1 + config.width, length(newline[h]));
        parsedLine[h + 1] := copy(parsedLine[h], 1 + config.width, length(parsedLine[h]));
      end;
    end;
    gotnewlines := true;
  end;

  if TransitionTimer.Enabled = false then
  begin
    if (ResetContrast) then
    begin
      // A contrast fade "transition" has just happened so reset the contrast
      // just in case we failed to get the expected number of cycles (due to
      // high cpu loads etc).
      ResetContrast := False;
      Lcd.setContrast(config.DLL_contrast);
    end;

    if (canscroll) then
    begin
      canscroll := false;
      doesGPOflash := not doesGPOflash;
      if (GPOflash > 0) then
      begin
        GPOflash := GPOflash -1;
        DoGPO(whatGPO, 2)
      end;
    end;

    // calculate scroll positions
    for counter := 1 to config.height do
    begin
      if (not config.screen[activeScreen].line[counter].noscroll) then
      begin
        if MilliSecondsBetween(now, scrollT) >= config.scrollPeriod then
        begin
          ScreenLCD[counter].Caption := EscapeAmp(scroll(parsedLine[counter], counter, 1));
          scrolldone := true;
        end;
      end
      else
        if (scrollPos[counter]>1) then // maintain manual scroll postion
        begin
            ScreenLCD[counter].Caption := EscapeAmp(scroll(parsedLine[counter], counter, 0))
        end
        else
        begin
            ScreenLCD[counter].Caption := EscapeAmp(copy(parsedLine[counter], 1, config.width));
        end;
    end;

    if scrolldone then
    begin
      scrollT := now;
      scrolldone := false;
    end;
  end
  else
  begin          // TransitionTimer.Enabled = true
    DoTransitions();
    for h := 1 to config.height do LastLineLCD[h] := ''; // force display update while doing transition
  end;

  for h := 1 to config.height do
  begin
    tmpline[h] := ScreenLCD[h].Caption;
    tmpline[h] := copy(UnescapeAmp(tmpline[h]) + '                                                                                                    ', 1, config.width);

    if tmpline[h] <> LastLineLCD[h] then
    begin
      LastLineLCD[h] := tmpline[h];
      Lcd.setPosition(1, h);

      if config.OneBySixteenFixup then
      begin
        Lcd.write(copy(tmpline[h], 0, trunc(config.width / 2)));
        Lcd.setPosition(1, h+1);
        Lcd.write(copy(tmpline[h], trunc(config.width / 2)+1, trunc(config.width / 2)));
      end
      else
        Lcd.write(tmpline[h]);

      if assigned(CContext) then
      begin
        try
          CContext.Connection.IOHandler.write(chr(h)+chr(length(tmpline[h]))+tmpline[h]);
        except
        end;
      end;
    end;
  end;
end;

procedure TLCDSmartieDisplayForm.UpdateTimersState(InSetupState : boolean);
begin
  if not InSetupState then
  begin    // We're not in setup
    // don't change timer states if we're waiting for a HD44780 to start.
    if not frozen then
    begin
      NextScreenTimer.enabled := true; // next screen
    end;
  end
  else
  begin    // We're in Setup
    LeftManualScrollTimer.enabled := false;     // left manual scroll
    RightManualScrollTimer.enabled := false;     // right manual scroll
    NextScreenTimer.enabled := false;     // next screen
    TransitionTimer.enabled := false; // "transitions"
  end;


  ActionsTimer.enabled := true;  // actions
  ScrollFlashTimer.enabled := true; // scroll/flash
  timerRefresh.enabled := true;  // update lcd and data
end;


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////                                                                       ////
////       B U T T O N  /  M E N U       H A N D L E R S                   ////
////                                                                       ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

procedure TLCDSmartieDisplayForm.Close1Click(Sender: TObject);
begin
  Close;
end;

// ShowMenu/Minimize has been selected from the tray/popup menu
procedure TLCDSmartieDisplayForm.ShowWindow1Click(Sender: TObject);
begin
  if LCDSmartieDisplayForm.Visible then
  begin // hide
    config.MainFormPosTop := LCDSmartieDisplayForm.Top;
    config.MainFormPosLeft := LCDSmartieDisplayForm.Left;
    config.save;
    LCDSmartieDisplayForm.Visible:=false;
    WindowState := wsNormal;
  end
  else
  begin // show
    LCDSmartieDisplayForm.Top := config.MainFormPosTop;
    LCDSmartieDisplayForm.Left := config.MainFormPosLeft;
    LCDSmartieDisplayForm.Visible:= true;
    ResizeHeight;
  end;
end;

// The LCD Smartie logo has been clicked - raise popup menu
procedure TLCDSmartieDisplayForm.LogoImageClick(Sender: TObject);
begin
  popupmenu1.Popup(LCDSmartieDisplayForm.left + LogoImage.left + round(LogoImage.width/2), LCDSmartieDisplayForm.top
    + LogoImage.top + round(LogoImage.height));
end;

procedure TLCDSmartieDisplayForm.SetupButtonClick(Sender: TObject);
begin
  UpdateTimersState(true); // turns off required timers as setup form is visible.
  if not Visible then
    LCDSmartieDisplayForm.Visible:=true;
  LCDSmartieDisplayForm.NextScreenTimer.interval := 0; //freeze screen change timer
  SetupForm := TSetupForm.Create(self);
  with SetupForm do
    ShowModal;

  setupform.Free;
  setupform := nil;

  if config.Snapping then
  begin
    SnapOptions.SnapFormTarget := true;
    SnapOptions.SnapToForms := true;
    SnapOptions.SnapToMonitor := true;
  end
  else
  begin
    SnapOptions.SnapFormTarget := false;
    SnapOptions.SnapToForms := false;
    SnapOptions.SnapToMonitor := false;
  end;

  if (config.alwaysOnTop) then
    FormStyle := fsSystemStayOnTop
  else
    FormStyle := fsNormal;

  UpdateTimersState(false);
    if (not config.screen[activeScreen].settings.bSticky) then
      LCDSmartieDisplayForm.NextScreenTimer.interval := config.screen[activeScreen].settings.showTime*1000;
  if not DisplayError then
  begin
    if (Config.MainFormCaption = '') then
      LCDSmartieDisplayForm.Caption := 'LCD Smartie ' + GetFmtFileVersion()
    else
      LCDSmartieDisplayForm.Caption := Config.MainFormCaption;

    if config.AppendConfigName then
      LCDSmartieDisplayForm.Caption := LCDSmartieDisplayForm.Caption + ' ' + ExtractFileNameWithoutExt(extractfilename(config.filename));
  end;
  trayicon1.Hint:=LCDSmartieDisplayForm.Caption;
end;

procedure TLCDSmartieDisplayForm.HideButtonClick(Sender: TObject);
begin
  ShowWindow1Click(Sender);
end;

procedure TLCDSmartieDisplayForm.PreviousImageClick(Sender: TObject);
begin
  NumberOfScreensToShift := -1;
  frozen := true;
  freeze();
end;

procedure TLCDSmartieDisplayForm.NextScreenImageClick(Sender: TObject);
begin
  NumberOfScreensToShift := 1;
  frozen := true;
  freeze();
end;

procedure TLCDSmartieDisplayForm.BacklightOn1Click(Sender: TObject);
begin
  backlit();
end;

procedure TLCDSmartieDisplayForm.Credits1Click(Sender: TObject);
begin
  DoCreditsForm;
end;

procedure TLCDSmartieDisplayForm.LogoImageDblClick(Sender: TObject);
begin
  DoCreditsForm;
end;

procedure TLCDSmartieDisplayForm.NextTheme1Click(Sender: TObject);
begin
  activetheme := activetheme + 1;
  if activetheme = MaxThemes then activetheme := 0;
  frozen := true;
  freeze();
end;

procedure TLCDSmartieDisplayForm.LastTheme1Click(Sender: TObject);
begin
  activetheme := activetheme-1;
  if activetheme=-1 then activetheme := MaxThemes-1;
  frozen := true;
  freeze();
end;

procedure TLCDSmartieDisplayForm.Freeze1Click(Sender: TObject);
begin
  freeze();
end;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////                                                                       ////
////    M O U S E    D O W N / U P    E V E N T    H A N D L E R S         ////
////                                                                       ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

procedure TLCDSmartieDisplayForm.LineRightScrollImageMouseDown(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
var
  Image: TImage;
  thisLine: integer;
begin
  Image := Sender As TImage;
  thisLine := strtoInt(copy(Image.Name, 8));
  try
    LineRightScrollImages[thisLine].picture.LoadFromFile(extractfilepath(application.exename) + sSkinDir +
      config.sSkinPath + '\small_arrow_left_down' + inttostr(thisLine) + '.bmp');
  except
    LineRightScrollImages[thisLine].picture.LoadFromResourceName(HInstance, 'small_arrow_left_down' + inttostr(thisLine));
  end;
  line2scroll := thisLine;
  RightManualScrollTimer.enabled := true;
  timerRefresh.enabled := false;
end;

procedure TLCDSmartieDisplayForm.LineRightScrollImageMouseUp(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
var
  Image: TImage;
  thisLine: integer;
begin
  Image := Sender As TImage;
  thisLine := strtoInt(copy(Image.Name, 8));
  try
    LineRightScrollImages[thisLine].picture.LoadFromFile(extractfilepath(application.exename) + sSkinDir +
      config.sSkinPath + '\small_arrow_left_up' + inttostr(thisLine) + '.bmp');
  except
    LineRightScrollImages[thisLine].picture.LoadFromResourceName(HInstance, 'small_arrow_left_up' + inttostr(thisLine));
  end;
  RightManualScrollTimer.enabled := false;
  timerRefresh.enabled := true;
end;

procedure TLCDSmartieDisplayForm.LineLeftScrollImageMouseDown(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
var
  Image: TImage;
  thisLine: integer;
begin
  Image := Sender As TImage;
  thisLine := strtoInt(copy(Image.Name, 8));
  try
    LineLeftScrollImages[thisLine].picture.LoadFromFile(extractfilepath(application.exename) + sSkinDir +
      config.sSkinPath + '\small_arrow_right_down' + inttostr(thisLine) + '.bmp');
  except
    LineLeftScrollImages[thisLine].picture.LoadFromResourceName(HInstance, 'small_arrow_right_down' + inttostr(thisLine));
  end;
  line2scroll := thisLine;
  LeftManualScrollTimer.enabled := true;
  timerRefresh.enabled := false;
end;

procedure TLCDSmartieDisplayForm.LineLeftScrollImageMouseUp(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
var
  Image: TImage;
  thisLine: integer;
begin
  Image := Sender As TImage;
  thisLine := strtoInt(copy(Image.Name, 8));
  try
    LineLeftScrollImages[thisLine].picture.LoadFromFile(extractfilepath(application.exename) + sSkinDir +
      config.sSkinPath + '\small_arrow_right_up' + inttostr(thisLine) + '.bmp');
  except
    LineLeftScrollImages[thisLine].picture.LoadFromResourceName(Hinstance, 'small_arrow_right_up' + inttostr(thisLine));
  end;
  LeftManualScrollTimer.enabled := false;
  timerRefresh.enabled := true;
end;

procedure TLCDSmartieDisplayForm.PreviousImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  try
    PreviousImage.picture.LoadFromFile(extractfilepath(application.exename) + sSkinDir +
      config.sSkinPath + '\big_arrow_left_down.bmp');
  except
    PreviousImage.picture.LoadFromResourceName(Hinstance, 'big_arrow_left_down');
  end;
end;

procedure TLCDSmartieDisplayForm.PreviousImageMouseUp(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
  try
    PreviousImage.picture.LoadFromFile(extractfilepath(application.exename) + sSkinDir +
      config.sSkinPath + '\big_arrow_left_up.bmp');
  except
    PreviousImage.picture.LoadFromResourceName(HInstance, 'big_arrow_left_up');
  end;
end;

procedure TLCDSmartieDisplayForm.SetupImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // this is slightly different as an exception here prevents us
  // getting into settings to fix a skin issue
  try
    if fileexists(extractfilepath(application.exename) + sSkinDir +
      config.sSkinPath + '\setup_down.bmp') then
    SetupImage.picture.LoadFromFile(extractfilepath(application.exename) + sSkinDir +
      config.sSkinPath + '\setup_down.bmp')
      else
        SetupImage.picture.LoadFromResourceName(HInstance, 'setup_down');
  except
    SetupImage.picture.LoadFromResourceName(HInstance, 'setup_down');
  end;
end;

procedure TLCDSmartieDisplayForm.SetupImageMouseUp(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
  try
    SetupImage.picture.LoadFromFile(extractfilepath(application.exename) + sSkinDir +
      config.sSkinPath + '\setup_up.bmp');
  except
    SetupImage.picture.LoadFromResourceName(HInstance, 'setup_up');
  end;
end;

procedure TLCDSmartieDisplayForm.HideImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  try
    HideImage.picture.LoadFromFile(extractfilepath(application.exename) + sSkinDir +
      config.sSkinPath + '\hide_down.bmp');
  except
    HideImage.picture.LoadFromResourceName(HInstance, 'hide_down');
  end;
end;

procedure TLCDSmartieDisplayForm.HideImageMouseUp(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
  try
    HideImage.picture.LoadFromFile(extractfilepath(application.exename) + sSkinDir +
      config.sSkinPath + '\hide_up.bmp');
  except
    HideImage.picture.LoadFromResourceName(HInstance, 'hide_up');
  end;
end;

procedure TLCDSmartieDisplayForm.NextScreenImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  try
    NextScreenImage.picture.LoadFromFile(extractfilepath(application.exename) + sSkinDir +
      config.sSkinPath + '\big_arrow_right_down.bmp');
  except
    NextScreenImage.picture.LoadFromResourceName(HInstance, 'big_arrow_right_down');
  end;
end;

procedure TLCDSmartieDisplayForm.NextScreenImageMouseUp(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
  try
    NextScreenImage.picture.LoadFromFile(extractfilepath(application.exename) + sSkinDir +
      config.sSkinPath + '\big_arrow_right_up.bmp');
  except
    NextScreenImage.picture.LoadFromResourceName(HInstance, 'big_arrow_right_up');
  end;
end;


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////                                                                       ////
////      L C D    I N I T I A L I Z A T I O N     P R O C E D U R E S     ////
////                                                                       ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

procedure TLCDSmartieDisplayForm.InitLCD();
var
  i: Integer;
  w, h: integer;
begin
  // Sync the display to our current view of the custom chars.
  for i:= 1 to 8 do
    customCharsChanged[i] := true;

  // start connectivity
  try
    if not (config.DisplayDLLName = '') then
    begin
      if config.OneBySixteenFixup then
        Lcd := TLCD_DLL.CreateDLL(trunc(config.width / 2), config.height * 2,config.DisplayDLLName,config.DisplayDLLParameters)
      else
        Lcd := TLCD_DLL.CreateDLL(config.width,config.height,config.DisplayDLLName,config.DisplayDLLParameters);

    end
    else
      Lcd := TLCD.Create();
    DisplayError := false;
  except
    on E: Exception do
    begin
      // can we move this elsewhere to save an annoying popup?
      //showmessage('Failed to open device: ' + E.Message);
      LCDSmartieDisplayForm.Caption := E.Message; // yes, put it on the title bar
      DisplayError := true;
      Lcd := TLCD.Create();
    end;
  end;

  customchar('1, 12, 18, 18, 12, 0,   0,  0,  0');
  customchar('2, 31, 31, 31, 31, 31, 31, 31, 31');
  customchar('3, 16, 16, 16, 16, 16, 16, 31, 16');
  customchar('4, 28, 28, 28, 28, 28, 28, 31, 28');

  Lcd.setContrast(config.DLL_contrast);
  Lcd.setBrightness(config.DLL_brightness);

  DoFullDisplayDraw();

  UpdateTimersState(assigned(SetupForm));
end;

procedure TLCDSmartieDisplayForm.FiniLCD(WriteShutdownMessage : boolean);
var
  h,x : integer;
  row : string;
begin

  timerRefresh.enabled := false;  // stop updates to lcd

  try
    if assigned(Lcd) then begin
      if WriteShutdownMessage then begin

        for h := 1 to config.Height do
          begin
            row := Data.change(Config.ShutdownMessage[h]); // now we can use variables in shutdown message

            //for x := length(row)+1 to config.Width do
            row := copy(row + '                                                                                                   ', 0, config.Width);

            if config.OneBySixteenFixup then
            begin
              Lcd.write(copy(row, 0, trunc(config.width / 2)));
              Lcd.setPosition(1, h+1);
              Lcd.write(copy(row, trunc(config.width / 2)+1, trunc(config.width / 2)));
            end
            else
            begin
              Lcd.setPosition(1, h);
              Lcd.write(row);
            end;
            Sleep(20);
          end;
        Lcd.setbacklight(false);
      end;
      Lcd.Destroy();
    end;
  except
  on E: Exception do
    begin
      showmessage('Exception: ' + E.Message);
    end;
  end;
  Lcd := nil;
end;

procedure TLCDSmartieDisplayForm.ReInitLCD();
begin
  FiniLCD(false);
  InitLCD();
end;


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////                                                                       ////
////      L C D    D I S P L A Y     G L O B A L   P R O C E D U R E S     ////
////                                                                       ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

procedure TLCDSmartieDisplayForm.customchar(fline: String);
var
  character: Integer;
  waarde: Array[0..7] of Byte;
  i: Integer;
  iPosStart, iPosEnd: Integer;

begin
  iPosEnd := pos(',', fline);
  try
    character := StrToIntN(fline, 1, iPosEnd-1);

  except
    Exit;
  end;
  for i := 0 to 6 do
  begin
    iPosStart := iPosEnd + 1;
    iPosEnd := PosEx(',', fline, iPosStart);
    try
      waarde[i] := StrToIntN(fline, iPosStart, iPosEnd-iPosStart);
    except
      exit;
    end;
  end;
  waarde[7] := StrToIntN(fline, iPosEnd+1, length(fline)-iPosEnd);

  // Only send if not already defined.
  i := 0;
  while (i <= 7) and (waarde[i] = customChars[character, i]) do Inc(i);

  if (i <= 7) then
  begin
    customCharsChanged[character] := true;
    for i := 0 to 7 do customChars[character, i] := waarde[i];
  end;
end;

procedure TLCDSmartieDisplayForm.SendCustomChars;
var
  i: Integer;
begin
  for i:= 1 to 8 do
  begin
    if (customCharsChanged[i]) then
    begin
      Lcd.customChar(i, customChars[i]);
      customCharsChanged[i] := false;
      FontManager.CustomChar(i, customChars[i]);
    end;
  end;
end;


procedure TLCDSmartieDisplayForm.DoGPO(const ftemp1, ftemp2: Integer);
begin
  if ftemp1 < 9 then
  begin
    if ftemp2 = 0 then
    begin
      Lcd.setGPO(ftemp1, false);
      GPO[ftemp1] := false;
    end;
    if ftemp2 = 1 then
    begin
      Lcd.setGPO(ftemp1, true);
      GPO[ftemp1] := true;
    end;
    if ftemp2 = 2 then
    begin
      if (GPO[ftemp1]) then
      begin
        Lcd.setGPO(ftemp1, false);
        GPO[ftemp1] := false;
      end
      else
      begin
        Lcd.setGPO(ftemp1, true);
        GPO[ftemp1] := true;
      end;
    end;
  end;
end;

procedure ProcessTimer(AWnd: HWND; AMsg: UINT; AIDEvent: UINT_PTR; ATicks: DWORD); stdcall;
var
  st: string;
  p: pointer;
begin
  KillTimer(AWnd, AIDEvent);
  p := pointer(AIDEvent);
  st := string(p^);
  LCDSmartieDisplayForm.ProcessAction(true, st, 0);
  freemem(p);
end;

procedure TActionURLThread.Execute;
begin
  try
    URLThread := TURLThread.Create(1);
    try
      URLThread.getUrl(Url, MaxFreq, PostParms);
    except
    end;
  finally
    URLThread.Free;
  end;
end;

procedure TLCDSmartieDisplayForm.ProcessAction(bDoAction: Boolean; sAction: String; ActionIndex: integer);
const
  APPCOMMAND_VOLUME_UP = $A0000;
  APPCOMMAND_VOLUME_DOWN = $90000;
  APPCOMMAND_VOLUME_MUTE = $80000;
var
  temp1, temp2: String;
  postStrings: TStringList;
  iTemp: Integer;
  args: Array [0..maxArgs-1] of String;
  prefix, postfix: String;
  numArgs: Cardinal;
  sSecondAction: String;
  uiPlugin: Cardinal;
  p: pointer;
  u: uint_ptr;
  ActionURLThread: TActionURLThread;
begin
  while ActionLogMemo.Lines.Count > 500 do
    ActionLogMemo.Lines.Delete(0);

  if bDoAction then
    temp1 := 'True'
  else
    temp1 := 'False';

  ActionLogMemo.Lines.Add(DateTimeToStr(Now) + ' ' + 'Action: ' + sAction + ' DoAction: ' + temp1);

  // do these first and clear the string to prevent the embedded action from being run now
  if (bDoAction) then
  begin
    while decodeArgs(sAction, 'ActionAfterSecs', 2, args, prefix, postfix, numargs) do
    begin
      sAction := '';
      p := getmem(length(args[0]) + 1);
      u := qword(p);
      string(p^) := args[0];
      SetTimer(self.Handle, u, strtoint(args[1]) * 1000, @ProcessTimer);
    end;

    while decodeArgs(sAction, 'ActionAndDisable', 2, args, prefix, postfix, numargs) do
    begin
      sAction := '';
      ProcessAction(bDoAction, args[0], 0);
      config.actionsArray[ActionIndex, 5] := 'False';
    end;

    while decodeArgs(sAction, 'EnableAction', 2, args, prefix, postfix, numargs) do
    begin
      sAction := '';
      if strtoint(args[1]) = 0 then
        config.actionsArray[strtoint(args[0]), 5] := 'False'
      else
        config.actionsArray[strtoint(args[0]), 5] := 'True'
    end;
  end;

  // Handle actions have do something when they are activated and de-activated.

  if (pos('Backlight(', sAction) <> 0) then
  begin
    temp1 := copy(sAction, pos('(', sAction) + 1, 1);

    if (temp1 = '0') or (temp1 = '1') then
    begin
      iTemp := 1;
      if temp1 = '0' then iTemp := 0;

      if (not bDoAction) then
        iTemp := 1 - iTemp;

      backlit(iTemp);
    end;
  end;

  if (pos('GPO(', sAction) <> 0) then
  begin
    temp1 := copy(sAction, pos('(', sAction) + 1,
      pos(',', sAction)-pos('(', sAction)-1);
    temp2 := copy(sAction, pos(',', sAction) + 1,
      pos(')', sAction)-pos(',', sAction)-1);

    if (temp2 = '1') or (temp2 = '0') then
    begin
      if (not bDoAction) then
      begin
        // invert setting
        if temp2='1' then temp2 := '0'
        else temp2 := '1';
      end;

      try
        DoGPO(StrToInt(temp1), StrToInt(temp2));
      except
        on EConvertError do begin {ignore} end;
        else raise;
      end;
    end;
  end;


  if (pos('EnableScreen(', sAction) <> 0) then
  begin
    try
      iTemp := StrToInt(copy(sAction, pos('EnableScreen(', sAction) + 13,
        pos(')', sAction)-pos('EnableScreen(', sAction)-13));
      if (iTemp >= 1) and (iTemp <= MaxScreens) then
      begin
        if (bDoAction) then
        config.Screen[iTemp].settings.Enabled := bDoAction;
      end;
    except
      on EConvertError do begin {ignore} end;
      else raise;
    end;
  end;

  if (pos('DisableScreen(', sAction) <> 0) then
  begin
    try
      iTemp := StrToInt(copy(sAction, pos('DisableScreen(', sAction) + 14,
        pos(')', sAction)-pos('DisableScreen(', sAction)-14));
      if (iTemp >= 1) and (iTemp <= MaxScreens) then
      begin
        config.Screen[iTemp].settings.Enabled := not bDoAction;
      end;
    except
      on EConvertError do begin {ignore} end;
      else raise;
    end;
  end;


  // Handle actions that only do something when activated.
  if (bDoAction) then
  begin
    while decodeArgs(sAction, '$dll', maxArgs, args, prefix, postfix, numargs) do
    begin
      if (numargs = 4) then
      begin
        try
          uiPlugin := data.FindPlugin(args[0]);
          sSecondAction := data.CallPlugin(uiPlugin, StrToInt(args[1]), args[2], args[3]);
          ProcessAction(True, sSecondAction, 0);
        except
          on EConvertError do begin {ignore} end;
          else raise;
        end;
      end;
      sAction := prefix + postfix;
    end;

    temp1 := sAction;
    while decodeArgs(temp1, 'ChangeColor', 3, args, prefix, postfix, numargs) do
    begin
      temp1 := '';
      data.change('$Color('+args[0]+','+args[1]+','+args[2]+')');
    end;

    if (Pos('NextTheme', sAction) <> 0) then
    begin
      activetheme := activetheme + 1;
      if activetheme = MaxThemes then activetheme := 0;
      frozen := true;
      freeze();
    end;

    if (pos('LastTheme', sAction) <> 0) then
    begin
      activetheme := activetheme-1;
      if activetheme=-1 then activetheme := MaxThemes-1;
      frozen := true;
      freeze();
    end;

    if (pos('NextScreen', sAction) <> 0) then
    begin
      NumberOfScreensToShift := 1;
      frozen := true;
      freeze();
    end;

    if (pos('LastScreen', sAction) <> 0) then
    begin
      NumberOfScreensToShift := -1;
      frozen := true;
      freeze();
    end;

    if (pos('GotoTheme(', sAction) <> 0) then
    begin
      try
        iTemp := StrToInt(copy(sAction, pos('GotoTheme(', sAction) + 10,
          pos(')', sAction)-pos('GotoTheme(', sAction)-10))-1;
        if (iTemp >= 0) and (iTemp < MaxThemes) then
          activetheme := iTemp;
      except
        on EConvertError do begin {ignore} end;
        else raise;
      end;
    end;

    if (pos('GotoScreen(', sAction) <> 0) then
    begin
      try
        iTemp := StrToInt(copy(sAction, pos('GotoScreen(', sAction) + 11,
          pos(')', sAction)-pos('GotoScreen(', sAction)-11));
        if (iTemp >= 1) and (iTemp <= MaxScreens) then
          ChangeScreen(iTemp);
      except
        on EConvertError do begin {ignore} end;
        else raise;
      end;
    end;

    if pos('FreezeScreen', sAction) <> 0 then
    begin
      if not frozen then freeze();
    end;

    if pos('UnfreezeScreen', sAction) <> 0 then
    begin
      if frozen then freeze();
    end;

    if pos('ToggleFreeze', sAction) <> 0 then
    begin
      freeze();
    end;

    if pos('RefreshAll', sAction) <> 0 then
    begin
      Data.RefreshDataThreads;
    end;

    if pos('BacklightToggle', sAction) <> 0 then
    begin
      backlit();
    end;

    if pos('BacklightFlash(', sAction) <> 0 then
    begin
      temp1 := copy(sAction, pos('(', sAction) + 1, pos(')', sAction)
            - pos('(', sAction)-1);
      try
        flash := StrToInt(temp1)*2;
      except
        on EConvertError do begin {ignore} end;
        else raise;
      end;
    end;

    if pos('Wave[', sAction) <> 0 then
    begin
      temp1 := copy(sAction, pos('Wave[', sAction) + 5, pos(']', sAction)
         - pos('Wave[', sAction)-5);
      playsound(Pchar(temp1), 0, SND_FILENAME);
    end;

    if pos('Exec[', sAction) <> 0 then
    begin
      temp1 := copy(sAction, pos('Exec[', sAction) + 5, pos(']', sAction)
         - pos('Exec[', sAction)-5);
      shellexecute(0, 'open', PChar(temp1), '', '', SW_SHOW);
    end;

    temp1 := sAction;
    while decodeArgs(temp1, 'HTTPReq', 1, args, prefix, postfix, numargs) do
    begin
      temp1 := '';
      ActionURLThread := TActionURLThread.Create(true);
      ActionURLThread.FreeOnTerminate := True;
      ActionURLThread.Url := args[0];
      ActionURLThread.MaxFreq := 0;
      ActionURLThread.Start;
    end;

    temp1 := sAction;
    while decodeArgs(temp1, 'HTTPPost', 99, args, prefix, postfix, numargs) do
    begin
      temp1 := '';
      postStrings := TStringList.Create;

      for iTemp := 1 to numargs do
        postStrings.Add(args[iTemp]);

      ActionURLThread := TActionURLThread.Create(true);
      ActionURLThread.FreeOnTerminate := True;
      ActionURLThread.Url := args[0];
      ActionURLThread.MaxFreq := 0;
      ActionURLThread.PostParms := postStrings;
      ActionURLThread.Start;
    end;

    if (pos('Winamp', sAction) <> 0) then
    begin
      if pos('WinampNext', sAction) <> 0 then
        Winampctrl1.Next;

      if pos('WinampLast', sAction) <> 0 then
        Winampctrl1.Previous;

      if pos('WinampPlay', sAction) <> 0 then
        Winampctrl1.Play;

      if pos('WinampStop', sAction) <> 0 then
        Winampctrl1.Stop;

      if pos('WinampPause', sAction) <> 0 then
        Winampctrl1.Pause;

      if pos('WinampShuffle', sAction) <> 0 then
        Winampctrl1.ToggleShufflE;

      if pos('WinampVolumeDown', sAction) <> 0 then
      begin
        WinampCtrl1.VolumeDown;
        WinampCtrl1.VolumeDown;
        WinampCtrl1.VolumeDown;
        WinampCtrl1.VolumeDown;
        WinampCtrl1.VolumeDown;
      end;

      if pos('WinampVolumeUp', sAction) <> 0 then
      begin
        WinampCtrl1.VolumeUp;
        WinampCtrl1.VolumeUp;
        WinampCtrl1.VolumeUp;
        WinampCtrl1.VolumeUp;
        WinampCtrl1.VolumeUp;
      end;
    end;

    if (pos('GPOFlash(', sAction) <> 0) then
    begin
      try
        whatGPO := StrToInt(copy(sAction, pos('(', sAction) + 1,
          pos(',', sAction)-pos('(', sAction)-1));
        temp2 := copy(sAction, pos(',', sAction) + 1,
          pos(')', sAction)-pos(',', sAction)-1);
        GPOflash := StrToInt(temp2)*2;
      except
        on EConvertError do begin {ignore} end;
        else raise;
      end;
    end;

    if pos('GPOToggle(', sAction) <> 0 then
    begin
      try
        temp1 := copy(sAction, pos('(', sAction) + 1, pos(')', sAction)
           - pos('(', sAction)-1);
        DoGPO(StrToInt(temp1), 2)
      except
        on EConvertError do begin {ignore} end;
        else raise;
      end;
    end;

    if pos('SystemVolumeDown', sAction) <> 0 then
    begin
      SendMessage(Self.Handle, WM_APPCOMMAND, Self.Handle, APPCOMMAND_VOLUME_DOWN);
    end;

    if pos('SystemVolumeMute', sAction) <> 0 then
    begin
      SendMessage(Self.Handle, WM_APPCOMMAND, Self.Handle, APPCOMMAND_VOLUME_MUTE);
    end;

    if pos('SystemVolumeUp', sAction) <> 0 then
    begin
      SendMessage(Self.Handle, WM_APPCOMMAND, Self.Handle, APPCOMMAND_VOLUME_UP);
    end;

    if pos('Fan(', sAction) <> 0 then
    begin
      try
        temp1 := copy(sAction, pos('(', sAction) + 1, pos(',', sAction)
            - pos('(', sAction)-1);
        temp2 := copy(sAction, pos(',', sAction) + 1, pos(')', sAction)
            - pos(',', sAction)-1);

        Lcd.setFan(StrToInt(temp1), StrToInt(temp2));
      except
        on EConvertError do begin {ignore} end;
        else raise;
      end;
    end;
  end;
end;

// sets Backlight - toggles if no parameter given
procedure TLCDSmartieDisplayForm.backlit(iOn: Integer = -1);
begin
  if (iOn = -1) then
    Backlight := not Backlight
  else
    Backlight := boolean(iOn);

  Lcd.setbacklight(Backlight);

  if not Backlight then
    BacklightOn1.Caption := '&Backlight On'
  else
    BacklightOn1.Caption := '&Backlight Off';
  LCDSmartieDisplayForm.SetOnscreenBacklight();
end;


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////                                                                       ////
////  S C R E E N    D I S P L A Y     G L O B A L   P R O C E D U R E S   ////
////                                                                       ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
procedure TLCDSmartieDisplayForm.DoFullDisplayDraw;
var
  x: Integer;
begin

  // Wipe the our view of the display - this will cause a full redraw.
  for x := 1 to config.height do
  begin
    LastLineLCD[x] := '';
  end;
end;

procedure TLCDSmartieDisplayForm.ResetScrollPositions;
var
  y: Integer;
begin
  for y := 1 to MaxLines do
  begin
    scrollPos[y] := 1; // Reset scroll postion.
  end;
end;

function TLCDSmartieDisplayForm.scroll(const scrollvar: String;const line, speed: Integer):
  String;
var
  scrolltext: String;
  len: Integer;
begin
  if length(scrollvar) > config.width then
  begin
    scrollPos[line] := scrollPos[line] + speed;
    if (scrollPos[line]<1) then scrollPos[line] := length(scrollvar);
    if (scrollPos[line]>length(scrollvar)) then scrollPos[line] := 1;

    len := length(scrollvar)-scrollPos[line] + 1;
    if (len > config.width) then len := config.width;
    scrolltext := copy(scrollvar, scrollPos[line], len);

    if length(scrolltext) < config.width then
    begin
      scrolltext := scrolltext + copy(scrollvar, 1,
        config.width-length(scrolltext));
    end;
    result := scrolltext;
  end
  else result := scrollvar;
end;

procedure TLCDSmartieDisplayForm.freeze();
begin
  if frozen = false then
  begin
    frozen := true;
    NextScreenTimer.enabled := false;
    Freeze1.Caption := 'Unfreeze';
    LCDSmartieDisplayForm.caption := LCDSmartieDisplayForm.caption + ' - frozen'
  end
  else
  begin
    frozen := false;
    NextScreenTimer.enabled := true;
    NextScreenTimer.interval := 0;
    NextScreenTimer.interval := 5;
    Freeze1.Caption := 'Freeze';
    if pos('frozen', LCDSmartieDisplayForm.caption) <> 0 then LCDSmartieDisplayForm.caption :=
      copy(LCDSmartieDisplayForm.caption, 1, length(LCDSmartieDisplayForm.caption)-length(' - frozen'));
  end;
end;

// Only used when line scroll button is pressed.
procedure TLCDSmartieDisplayForm.scrollLine(line: Byte; direction: Integer);
var
  tmpline: Array [1..MaxLines] of String;
begin

  tmpline[line] := copy (scroll(parsedLine[line], line, direction)
    + '                                                                                                    ', 1, config.width);
  ScreenLCD[line].caption := EscapeAmp(tmpline[line]);
  Lcd.setPosition(1, line);
  Lcd.write(tmpline[line]);

  if assigned(CContext) then
  begin
    try
      CContext.Connection.IOHandler.write(chr(line)+chr(length(tmpline[line]))+tmpline[line]);
    except
    end;
  end;
end;

procedure TLCDSmartieDisplayForm.SetOnscreenBacklight;
var
  ScreenColor,FontColor : TColor;
  Loop : byte;
begin
  ScreenColor := clWhite;
  FontColor := clBlack;
  if Backlight then begin // Backlight is on
    case (config.colorOption) of
      0  : begin
        ScreenColor := $0001FFA8;
        FontColor := clBlack;
      end;
      1  : begin
        ScreenColor := $00FDF103;
        FontColor := clBlack;
      end;
      2  : begin
        ScreenColor := clYellow;
        FontColor := clBlack;
      end;
      3  : begin
        ScreenColor := clWhite;
        FontColor := clBlack;
      end;
      4  : begin
        ScreenColor := $00FFCE00;
        FontColor := clWhite;
      end;
      5  : begin
        ScreenColor := BackgroundColorOn;
        FontColor := ForgroundColorOn;
      end;
    end; // case
  end else begin // Backlight is off
    case (config.colorOption) of
      0  : begin
        ScreenColor := clGreen;
        FontColor := clBlack;
      end;
      1  : begin
        ScreenColor := $00C00000;
        FontColor := clWhite;
      end;
      2  : begin
        ScreenColor := clOlive;
        FontColor := clBlack;
      end;
      3  : begin
        ScreenColor := clSilver;
        FontColor := clBlack;
      end;
      4  : begin
        ScreenColor := BackgroundColorOff;
        FontColor := ForgroundColorOff;
      end;
      5  : begin
        ScreenColor := BackgroundColorOff;
        FontColor := ForgroundColorOff;
      end;
    end; // case
  end; // not background on

  LCDSmartieDisplayForm.Color := ScreenColor;
  for Loop := 1 to MaxLines do begin
    ScreenLCD[Loop].LineColor := ScreenColor;
    ScreenLCD[Loop].FontColor := FontColor;
  end;
end;



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////                                                                       ////
////        S C R E E N     C H A N G E      P R O C E D U R E S           ////
////                                                                       ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

procedure TLCDSmartieDisplayForm.ChangeScreen(scr: Integer);
var
  y: Integer;
  ascreen: TScreenSettings;
begin
  if TempTransitionTimerInterval <> 0 then
  begin
    TransitionTimer.Interval := 0;
    TransitionTimer.Interval := TempTransitionTimerInterval;
  end;
  NextScreenTimer.Interval := 0; // reset timer

  if (not config.screen[scr].settings.bSticky) then
    NextScreenTimer.Interval := config.screen[scr].settings.showTime*1000 + TempTransitionTimerInterval;

  if (activeScreen = scr) then
    Exit;

  activeScreen := scr;
  ascreen := config.screen[activeScreen].settings;

  for y := 1 to MaxLines do
  begin
    oldline[y] := UnescapeAmp(ScreenLCD[y].Caption);
  end;

  ResetScrollPositions();

  gotnewlines := false;
  TransStart := GetTickCount();
  TransCycle := 0;
  iLastRandomTranCycle := 0;

  for y := 1 to MaxCols do
  begin
    GuessArray[1, y] := false;
    GuessArray[2, y] := false;
    GuessArray[3, y] := false;
    GuessArray[4, y] := false;
    GuessArray[5, y] := false;
    GuessArray[6, y] := false;
    GuessArray[7, y] := false;
    GuessArray[8, y] := false;
  end;

  TempTransitionTimerInterval := ascreen.TransitionTime*100;

  TransitionTemp := TransitionTemp2;
  TransitionTemp2 := ascreen.TransitionStyle;

  if not ascreen.enabled then TransitionTemp2 := tsNone;
  if TransitionTemp2 = tsNone then TempTransitionTimerInterval := 1;

  if (config.width >= 40) then
    ScreenNumberPanel.Caption := 'Theme: ' + IntToStr(activetheme + 1) + ' Screen: ' +
      IntToStr(activeScreen)
  else
    ScreenNumberPanel.Caption := IntToStr(activetheme + 1) + ' | ' + IntToStr(activeScreen);

  bNewScreen := True;
  data.NewScreen(True);

end;

function TLCDSmartieDisplayForm.DoGuess(line: Integer): Integer;
var
  GoodGuess: Boolean;
  x: Integer;
  loopcount: Integer;

begin
  GoodGuess := false;
  x := 0;
  loopcount := 0;

  while not GoodGuess do begin
    Inc(loopcount);
    x := round(random(config.width) + 1);
    if GuessArray[line, x] = false then begin
      GoodGuess := true;
    end else if (loopcount > config.width*2) then begin
      // it's taking too long - use first unset element
      x := 0;
      repeat
        Inc(x);
        if (GuessArray[line, x] = false) then GoodGuess := true;
      until (x >= config.width) or (GoodGuess);
      if (not GoodGuess) then
      begin
      // all the elements are set - use 1 (arb.)
        x := 1;
        GoodGuess := true;
      end;
    end;
  end;
  GuessArray[line, x] := true;
  result := x;
end;

procedure TLCDSmartieDisplayForm.DoTransitions;
var
  GuessRegister: Array[1..MaxLines] of String;
  tempstr: String;
  line: Integer;
  maxTransCycles: Integer;
  Guess: Integer;
  x: Integer;
  iContrast: Integer;
  now: Cardinal;

begin
  // Changing screen - do any transitions required.
  //TransCycle := TransCycle + 1;

  now := GetTickCount();
  if (now < TransStart) then
    TransCycle := (now + (MAXDWORD-TransStart)) div timerRefresh.Interval
  else
    TransCycle := (now-TransStart) div timerRefresh.Interval;

  maxTransCycles := TransitionTimer.Interval div timerRefresh.Interval;

  if (TransCycle > maxTransCycles) or (maxTransCycles = 0) then
  begin
    CurrentScreen := activeScreen;
    Exit;

  end;

  if (TransCycle >= maxTransCycles / 2) then
  begin
    CurrentScreen := activeScreen;
    SendCustomChars();
  end;

  for x := 1 to config.height do begin
    oldline[x] := copy(oldline[x] +
      '                                                                                                               ', 1, config.width);
    newline[x] := copy(newline[x] +
      '                                                                                                               ', 1, config.width);
  end;

  case TransitionTemp of

    tsLeftRight  : begin  //left-->right

      for x := 1 to config.height do
      begin
        tempstr := copy(newline[x] + '|' + oldline[x], round((config.width-2)-TransCycle*((config.width+2)/maxTransCycles)), config.width);
        ScreenLCD[x].Caption := EscapeAmp(tempstr);
      end;
    end;

    tsRightLeft : begin  //right-->left

      for x := 1 to config.height do
      begin
        tempstr := copy(oldline[x] + '|' + newline[x],
          round(TransCycle*((config.width + 2)/maxTransCycles)),
          config.width);
        ScreenLCD[x].Caption := EscapeAmp(tempstr);
      end;
    end;

    tsTopBottom : begin //top-->bottom

      line := round(TransCycle*(config.height/maxTransCycles)) + 1;
      for x := 1 to line-1 do
      begin
        ScreenLCD[x].Caption := EscapeAmp(newline[config.height-(line-1)+ x]);
      end;

      if (line <= config.height) then
        ScreenLCD[line].Caption :=
          copy('---------------------------------------------------------------------------------------------------', 1, config.width);

      for x := line + 1 to config.height do
      begin
        ScreenLCD[x].Caption := EscapeAmp(oldline[x-(line + 1) + 1]);
      end;
    end;

    tsBottomTop : begin  //bottom-->top

      line := round(TransCycle*(config.height/maxTransCycles)) + 1;
      for x := 1 to config.height-line do
      begin
        ScreenLCD[x].Caption := EscapeAmp(oldline[x + line-1]);
      end;

      if (config.height-line + 1 > 0) then
        ScreenLCD[config.height-line + 1].Caption :=
          copy('---------------------------------------------------------------------------------------------------', 1, config.width);

      for x := config.height-line + 2 to config.height do
      begin
        ScreenLCD[x].Caption :=
          EscapeAmp(newline[x-(config.height-line + 2) + 1]);
      end;
    end;

    tsRandomChars : begin //random blocks

      for x := 1 to MaxLines do
      begin
        GuessRegister[x] := copy(UnescapeAmp(ScreenLCD[x].caption) +
          '                                                                                                   ', 1, config.width);
      end;

      for x := iLastRandomTranCycle to
        round((config.width/maxTransCycles)*TransCycle)-1 do
      begin
        for line := 1 to MaxLines do
        begin
          Guess := DoGuess(line);
          GuessRegister[line] := copy(GuessRegister[line], 1, Guess-1) +
            copy(newline[line], Guess, 1) + copy(GuessRegister[line], Guess +
            1, config.width-Guess);
        end;
      end;
      iLastRandomTranCycle := round((config.width/maxTransCycles)*TransCycle);
      for x := 1 to MaxLines do
      begin
        ScreenLCD[x].caption := EscapeAmp(GuessRegister[x]);
      end;
    end;

    tsFade : begin
      if (maxTransCycles >= 2) then begin  //contrast fade
  // The fade is a two step process, so we need at least two cycles.

  // We only fade down to iMinFadeContrast; because many LCDs displays will be
  // blank long before we reach 0. (One user reported that their display was
  // blank at a contrast of 40).

  // For the first half of the cycles - lower the contrast

        if (TransCycle <= maxTransCycles/2) then
        begin

          x := config.DLL_contrast;

          iContrast := round(x-(TransCycle*(x-config.xiMinFadeContrast)
            / (MaxTransCycles/2)));


          if iContrast < config.xiMinFadeContrast then
            iContrast := config.xiMinFadeContrast
          else
            if iContrast > x then iContrast := x;
          Lcd.setContrast(iContrast);
        end
        else
        begin
    // raise the contrast over the second half
          CurrentScreen := activeScreen;
          for x := 1 to MaxLines do
          begin
            ScreenLCD[x].Caption := EscapeAmp(newline[x]);
          end;

          x := config.DLL_contrast;

          iContrast := round((TransCycle-(MaxTransCycles/2))
            * (x-config.xiMinFadeContrast)/(MaxTransCycles/2))
            + config.xiMinFadeContrast;

          if iContrast > x then iContrast := x
          else
            if iContrast < config.xiMinFadeContrast then
              iContrast := config.xiMinFadeContrast;
          Lcd.setContrast(iContrast);
        end;
        ResetContrast := True;// Just to be sure the contrast is back to correct levels.
      end;
    end;
  end; // case
end;


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////                                                                       ////
////        M I S C E L L A N E O U S        P R O C E D U R E S           ////
////                                                                       ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

function TLCDSmartieDisplayForm.EscapeAmp(const sStr: string): String;
begin
  Result := StringReplace(sStr, '&', '&&', [rfReplaceAll])
end;

function TLCDSmartieDisplayForm.UnescapeAmp(const sStr: string): String;
begin
  Result := StringReplace(sStr, '&&', '&', [rfReplaceAll])
end;

procedure TLCDSmartieDisplayForm.SetupAutoStart;
var
  sParameters: String;
  sShortCutName: String;
  bDelete: Boolean;
begin
  sParameters := '';
  sShortCutName := 'LCD Smartie';

  if (config.bAutoStartHide) then
    sParameters := sParameters + '-hide ';

  if (config.bStartAsAdmin) then
    sParameters := sParameters + '-admin ';

  sParameters := sParameters + '-config ' + '"' + config.filename + '"';
  sShortCutName := sShortCutName + ' ' + ExtractFileNameWithoutExt(extractfilename(config.filename));

  bDelete := not (config.bAutoStart or config.bAutoStartHide);

  if not config.bUseTaskScheduler then
  begin
    try
      CreateShortcut(sShortCutName, application.exename, sParameters, bDelete);
    except
      on E: Exception do; // catch this to keep lazarus debugger happy
    end;
  end
  else
    if IsAdministrator then
      SetupSchedulerAutoStart(sShortCutName, application.exename, sParameters, config.bStartAsAdmin, bDelete);
end;

/// <summary>
///   This function reads the file resource of "FileName" and returns
///   the version number as formatted text.</summary>
/// <example>
///   Sto_GetFmtFileVersion() = '4.13.128.0'
///   Sto_GetFmtFileVersion('', '%.2d-%.2d-%.2d') = '04-13-128'
/// </example>
/// <remarks>If "Fmt" is invalid, the function may raise an
///   EConvertError exception.</remarks>
/// <param name="FileName">Full path to exe or dll. If an empty
///   string is passed, the function uses the filename of the
///   running exe or dll.</param>
/// <param name="Fmt">Format string, you can use at most four integer
///   values.</param>
/// <returns>Formatted version number of file, '' if no version
///   resource found.</returns>
function GetFmtFileVersion(const FileName: String = '';
  const Fmt: String = '%d.%d.%d.%d'): String;
var
  sFileName: String;
  iBufferSize: DWORD;
  iDummy: DWORD;
  pBuffer: Pointer;
  pFileInfo: Pointer;
  iVer: array[1..4] of Word;
begin
  // set default value
  Result := '';
  // get filename of exe/dll if no filename is specified
  sFileName := FileName;
  if (sFileName = '') then
  begin
    // prepare buffer for path and terminating #0
    SetLength(sFileName, MAX_PATH + 1);
    SetLength(sFileName,
      GetModuleFileName(hInstance, PChar(sFileName), MAX_PATH + 1));
  end;
  // get size of version info (0 if no version info exists)
  iBufferSize := GetFileVersionInfoSize(PChar(sFileName), iDummy);
  if (iBufferSize > 0) then
  begin
    GetMem(pBuffer, iBufferSize);
    try
    // get fixed file info (language independent)
    GetFileVersionInfo(PChar(sFileName), 0, iBufferSize, pBuffer);
    VerQueryValue(pBuffer, '\', pFileInfo, iDummy);
    // read version blocks
    iVer[1] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
    iVer[2] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
    iVer[3] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
    iVer[4] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);

    LCDSmartieDisplayForm.OurVersMaj := iVer[1];
    LCDSmartieDisplayForm.OurVersMin := iVer[2];
    LCDSmartieDisplayForm.OurVersRel := iVer[3];
    LCDSmartieDisplayForm.OurVersBuild := iVer[4];

    finally
      FreeMem(pBuffer);
    end;
    // format result string
    Result := Format(Fmt, [iVer[1], iVer[2], iVer[3], iVer[4]]);
  end;
end;


procedure TLCDSmartieDisplayForm.ServerConnect(AContext: TIdContext);
begin
  if (AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase) then
      TIdSSLIOHandlerSocketBase(AContext.Connection.IOHandler).PassThrough := false;
  if ( AContext.Connection.IOHandler.ReadLn = config.RemoteSendPassword) then
  begin
    CContext := AContext;
    CContext.Connection.IOHandler.DefStringEncoding := IndyTextEncoding_ASCII;
    CContext.Connection.IOHandler.DefAnsiEncoding := IndyTextEncoding_ASCII;
  end;
end;

procedure TLCDSmartieDisplayForm.ServerExecute(AContext: TIdContext);
begin
  sleep(1); // slow down execution
end;

procedure TLCDSmartieDisplayForm.ServerDisconnect(AContext: TIdContext);
begin
  CContext := nil;
end;

end.

