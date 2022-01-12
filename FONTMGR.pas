unit FONTMGR;

{$MODE Delphi}

interface

uses
  Graphics;

type
  TFontArray = array[32..255] of TBitmap;

  TFontManager = class
  private
    fForegroundColor : TColor;
    fBackgroundColor : TColor;
    fBWFont : TFontArray;
    fColorFont : TFontArray;
    procedure GetBlankChar(Bitmap : TBitmap);
    procedure CopyColorFontBitmap(SourceBitmap,DestBitmap : TBitmap);
    procedure GetDefaultBitmap(Index : byte; Bitmap : TBitmap);
    procedure LoadFont;
    procedure LoadColorFont;
    procedure SetBackgroundColor(Color : TColor);
    procedure SetForegroundColor(Color : TColor);
  public
    constructor Create;
    destructor Destroy; override;
    property  ColorFont : TFontArray read fColorFont;
    procedure CustomChar(Index : byte; Bytes : array of byte);
    property BackgroundColor : TColor write SetBackgroundColor;
    property ForegroundColor : TColor write SetForegroundColor;
  end;

var
  FontManager : TFontManager = nil;

implementation

uses
  DEFFONT;

constructor TFontManager.Create;
begin
  fillchar(fBWFont,sizeof(fBWFont),$00);
  fillchar(fColorFont,sizeof(fColorFont),$00);
  fForegroundColor := clBlack;
  fBackgroundColor := clWhite;
  LoadFont;
  LoadColorFont;
end;

destructor TFontManager.Destroy;
begin
  inherited;
end;

procedure TFontManager.SetBackgroundColor(Color : TColor);
begin
  if (Color <> fBackgroundColor) then begin
    fBackgroundColor := Color;
    LoadColorFont;
  end;
end;

procedure TFontManager.SetForegroundColor(Color : TColor);
begin
  if (Color <> fForegroundColor) then begin
    fForegroundColor := Color;
    LoadColorFont;
  end;
end;

procedure TFontManager.LoadFont;
var
  Loop : longint;
begin
  for Loop := 32 to 255 do begin
    fBWFont[Loop] := TBitmap.Create;
    fBWFont[Loop].Width := CharWidth;
    fBWFont[Loop].Height := CharHeight;
    case loop of
      176, 158, 131, 132,
      133, 134, 135, 136 : GetBlankChar(fBWFont[Loop]);
      else GetDefaultBitmap(Loop,fBWFont[Loop]);
    end;
  end;
end;

procedure TFontManager.GetBlankChar(Bitmap : TBitmap);
var
  X,Y : byte;
begin
  for Y := 0 to CharHeight-1 do begin
    for X := 0 to CharWidth-1 do begin
      Bitmap.Canvas.Pixels[X,Y] := clWhite;
    end;
  end;
end;

procedure TFontManager.CopyColorFontBitmap(SourceBitmap,DestBitmap : TBitmap);
var
  X,Y : byte;
begin
  for Y := 0 to CharHeight-1 do begin
    for X := 0 to CharWidth-1 do begin
      if (SourceBitmap.Canvas.Pixels[X,Y] = clBlack) then
        DestBitmap.Canvas.Pixels[X,Y] := fForegroundColor
      else
        DestBitmap.Canvas.Pixels[X,Y] := fBackgroundColor;
    end;
  end;
end;

procedure TFontManager.GetDefaultBitmap(Index : byte; Bitmap : TBitmap);
var
  X,Y : byte;
begin
  for Y := 0 to CharHeight-1 do begin
    for X := 0 to CharWidth-1 do begin
      if (DefaultFont[Index,Y] and (1 shl (4-X)) > 0) then
        Bitmap.Canvas.Pixels[X,Y] := clBlack
      else
        Bitmap.Canvas.Pixels[X,Y] := clWhite;
    end;
  end;
end;

procedure TFontManager.LoadColorFont;
var
  Loop : longint;
begin
  for Loop := 32 to 255 do begin
    if assigned(fColorFont[Loop]) then fColorFont[Loop].Free;
    fColorFont[Loop] := TBitmap.Create;
    fColorFont[Loop].Width := CharWidth;
    fColorFont[Loop].Height := CharHeight;
    CopyColorFontBitmap(fBWFont[Loop],fColorFont[Loop]);
  end;
end;

procedure TFontManager.CustomChar(Index : byte; Bytes : array of byte);
var
  X,Y : byte;
begin
  with fBWFont[127+Index].Canvas do begin
    for X := 0 to CharWidth-1 do begin
      for Y := 0 to CharHeight-1 do begin
        if ((Bytes[Y] and (1 shl X)) > 0) then
          Pixels[CharWidth-1-X,Y] := clBlack
        else
          Pixels[CharWidth-1-X,Y] := clWhite;
      end;
    end;
  end;
  CopyColorFontBitmap(fBWFont[127+Index],fColorFont[127+Index]);
end;

begin
  FontManager := TFontManager.Create;
end.
