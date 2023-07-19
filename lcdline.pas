unit lcdline;

{$MODE Delphi}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, FONTMGR;

const
  MaxWidth = 100;
  DefaultWidth = 20;

type

  { TLCDLineFrame }

  TLCDLineFrame = class(TFrame)
    LCDPanel: TPanel;
    PaintBox: TPaintBox;
    procedure PaintBoxClick(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
  private
    { Private declarations }
    fForegroundColor : TColor;
    fBackgroundColor : TColor;
    fCaption : string;
    fLineWidth : integer;
    BackgroundBitmap : TBitmap;
    FOnLineClicked   : TNotifyEvent;

    procedure CreateBackground;
    procedure SendChar(C : byte; CurrentX : byte);
    procedure ScreenWrite(S : string);
    procedure SetLineWidth(NewWidth : integer);
    procedure SetBackgroundColor(Color : TColor);
    procedure SetForegroundColor(Color : TColor);
  public
    { Public declarations }
    Property OnLineClicked : TNotifyEvent Read FOnLineClicked write FOnLineClicked ;
    property Caption : string read fCaption write ScreenWrite;
    property LineWidth : integer read fLineWidth write SetLineWidth;
    property LineColor : TColor read fBackgroundColor write SetBackgroundColor;
    property FontColor : TColor read fForegroundColor write SetForegroundColor;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

uses
  DEFFONT, lazutf8;

procedure TLCDLineFrame.SetBackgroundColor(Color : TColor);
begin
  if (fBackgroundColor = Color) then exit;
  fBackgroundColor := Color;
  LCDPanel.Color := Color;
  with BackgroundBitmap.Canvas do begin
    Pen.Color := fBackgroundColor;
    Brush.Color := fBackgroundColor;
    try
    Rectangle(0,0,Width,Height); // why this causes exception?
    except
      on e:exception do; // keep it happy
    end;
  end;
  FontManager.BackgroundColor := Color;
  ScreenWrite(fCaption);
end;

procedure TLCDLineFrame.SetForegroundColor(Color : TColor);
begin
  if (fForegroundColor = Color) then exit;
  fForegroundColor := Color;
  FontManager.ForegroundColor := Color;
  ScreenWrite(fCaption);
end;

procedure TLCDLineFrame.SetLineWidth(NewWidth : integer);
begin
  fLineWidth := NewWidth;
  Width := NewWidth * 8;
  CreateBackground;
  ScreenWrite(fCaption);
end;

procedure TLCDLineFrame.CreateBackground;
begin
  if assigned(BackgroundBitmap) then BackgroundBitmap.Free;
  BackgroundBitmap := TBitmap.Create;
  with BackgroundBitmap do begin
//  Width := fLineWidth*(CharWidth+1);
    Width := fLineWidth*(CharWidth+3);
    Height := (CharHeight+1);
    Canvas.Pen.Color := fBackgroundColor;
    Canvas.Brush.Color := fBackgroundColor;
    Canvas.Rectangle(0,0,Width,Height);
  end;
end;

constructor TLCDLineFrame.Create(AOwner: TComponent);
begin
  inherited;
  fForegroundColor := clBlack;
  fBackgroundColor := clWhite;
  BackgroundBitmap := nil;
  fLineWidth := DefaultWidth;
  fCaption := '';
  CreateBackground;
end;

procedure TLCDLineFrame.PaintBoxPaint(Sender: TObject);
var
  DestRect : TRect;
begin
  with DestRect do begin
    Top := 2;
    Bottom := PaintBox.Height+1;
    Left := 2;
    Right := PaintBox.Width+1;
  end;
  PaintBox.Canvas.StretchDraw(DestRect,BackgroundBitmap);
end;

procedure TLCDLineFrame.PaintBoxClick(Sender: TObject);
begin
  If Assigned(FOnLineClicked) Then
    Self.OnLineClicked(Self);
end;

procedure TLCDLineFrame.SendChar(C : byte; CurrentX : byte);
var
  CurChar : TBitmap;
  DestRect : TRect;
  SrcRect : TRect;
begin
// this really needs a better more universal method....
  if (C > 128) then begin
    case C of
      176 : C := 0;
      158 : C := 1;
      131..136 : dec(C);
      255 : C := 255;
      else C := C;
    end;
  end;
  if (C < 32) then C := 128 + (C mod 8);
// this really needs a better more universal method....
  CurChar := FontManager.ColorFont[C];

  with SrcRect do begin
    Left := 0;
    Right := CharWidth;
    Top := 0;
    Bottom := CharHeight;
  end;
  with DestRect do begin
    Top := 0;
    Bottom := CharHeight;
    Left := (CurrentX-1)*(CharWidth+3);
    Right := Left + CharWidth;
  end;
  BackgroundBitmap.Canvas.CopyRect(DestRect,CurChar.Canvas,SrcRect);
end;

procedure TLCDLineFrame.ScreenWrite(S : string);
var
  C : byte;
  Loop : integer;
  Index : integer;
  bAmpersand : boolean;
begin
  fCaption := S;
  S := Copy(S+'                                                                                                   ', 1, fLineWidth);
  bAmpersand := false;
  Index:=1;
  for Loop := 1 to length(S) do begin
    C := ord(S[Loop]);
// filter out double ampersands
    if (not bAmpersand) then
      begin
        SendChar(C, Index);
        Inc(Index);
        bAmpersand := (C = $26);
      end
    else
      bAmpersand := false;
  end;
  PaintBoxPaint(nil);
end;

end.
