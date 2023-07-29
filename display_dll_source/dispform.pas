unit DISPFORM;

{$MODE Delphi}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ImgList, Menus, win32int, lcltype, ComCtrls,
  IniFiles;

const
  MaxWidth = 100;
  MaxHeight = 8;
  CharWidth = 5;
  CharHeight = 8;

  DefaultFont : array[32..255,0..CharHeight-1] of byte = (
    ($00,$00,$00,$00,$00,$00,$00,$00), // char 32
    ($04,$04,$04,$04,$00,$00,$04,$00), // char 33
    ($0A,$0A,$0A,$00,$00,$00,$00,$00), // char 34
    ($0A,$0A,$1F,$0A,$1F,$0A,$0A,$00), // char 35
    ($04,$0F,$14,$0E,$05,$1E,$04,$00), // char 36
    ($18,$19,$02,$04,$08,$13,$03,$00), // char 37
    ($0C,$12,$14,$08,$15,$12,$0D,$00), // char 38
    ($0C,$04,$08,$00,$00,$00,$00,$00), // char 39
    ($02,$04,$08,$08,$08,$04,$02,$00), // char 40
    ($08,$04,$02,$02,$02,$04,$08,$00), // char 41
    ($00,$04,$15,$0E,$15,$04,$00,$00), // char 42
    ($00,$04,$04,$1F,$04,$04,$00,$00), // char 43
    ($00,$00,$00,$00,$0C,$04,$08,$00), // char 44
    ($00,$00,$00,$1F,$00,$00,$00,$00), // char 45
    ($00,$00,$00,$00,$00,$0C,$0C,$00), // char 46
    ($00,$01,$02,$04,$08,$10,$00,$00), // char 47
    ($0E,$11,$13,$15,$19,$11,$0E,$00), // char 48
    ($04,$0C,$04,$04,$04,$04,$0E,$00), // char 49
    ($0E,$11,$01,$02,$04,$08,$1F,$00), // char 50
    ($1F,$02,$04,$02,$01,$11,$0E,$00), // char 51
    ($02,$06,$0A,$12,$1F,$02,$02,$00), // char 52
    ($1F,$10,$1E,$01,$01,$11,$0E,$00), // char 53
    ($06,$08,$10,$1E,$11,$11,$0E,$00), // char 54
    ($1F,$01,$02,$04,$08,$08,$08,$00), // char 55
    ($0E,$11,$11,$0E,$11,$11,$0E,$00), // char 56
    ($0E,$11,$11,$0F,$01,$02,$0C,$00), // char 57
    ($00,$0C,$0C,$00,$0C,$0C,$00,$00), // char 58
    ($00,$0C,$0C,$00,$0C,$04,$08,$00), // char 59
    ($02,$04,$08,$10,$08,$04,$02,$00), // char 60
    ($00,$00,$1F,$00,$1F,$00,$00,$00), // char 61
    ($08,$04,$02,$01,$02,$04,$08,$00), // char 62
    ($0E,$11,$01,$02,$04,$00,$04,$00), // char 63
    ($0E,$11,$01,$0D,$15,$15,$0E,$00), // char 64
    ($0E,$11,$11,$11,$1F,$11,$11,$00), // char 65
    ($1E,$11,$11,$1E,$11,$11,$1E,$00), // char 66
    ($0E,$11,$10,$10,$10,$11,$0E,$00), // char 67
    ($1C,$12,$11,$11,$11,$12,$1C,$00), // char 68
    ($1F,$10,$10,$1E,$10,$10,$1F,$00), // char 69
    ($1F,$10,$10,$1E,$10,$10,$10,$00), // char 70
    ($0E,$11,$10,$17,$11,$11,$0F,$00), // char 71
    ($11,$11,$11,$1F,$11,$11,$11,$00), // char 72
    ($0E,$04,$04,$04,$04,$04,$0E,$00), // char 73
    ($07,$02,$02,$02,$02,$12,$0C,$00), // char 74
    ($11,$12,$14,$18,$14,$12,$11,$00), // char 75
    ($10,$10,$10,$10,$10,$10,$1F,$00), // char 76
    ($11,$1B,$15,$15,$11,$11,$11,$00), // char 77
    ($11,$11,$19,$15,$13,$11,$11,$00), // char 78
    ($0E,$11,$11,$11,$11,$11,$0E,$00), // char 79
    ($1E,$11,$11,$1E,$10,$10,$10,$00), // char 80
    ($0E,$11,$11,$11,$15,$12,$0D,$00), // char 81
    ($1E,$11,$11,$1E,$14,$12,$11,$00), // char 82
    ($0F,$10,$10,$0E,$01,$01,$1E,$00), // char 83
    ($1F,$04,$04,$04,$04,$04,$04,$00), // char 84
    ($11,$11,$11,$11,$11,$11,$0E,$00), // char 85
    ($11,$11,$11,$11,$11,$0A,$04,$00), // char 86
    ($11,$11,$11,$11,$15,$15,$0A,$00), // char 87
    ($11,$11,$0A,$04,$0A,$11,$11,$00), // char 88
    ($11,$11,$11,$0A,$04,$04,$04,$00), // char 89
    ($1F,$01,$02,$04,$08,$10,$1F,$00), // char 90
    ($0E,$08,$08,$08,$08,$08,$0E,$00), // char 91
    ($00,$10,$08,$04,$02,$01,$00,$00), // char 92
    ($0E,$02,$02,$02,$02,$02,$0E,$00), // char 93
    ($04,$0A,$11,$00,$00,$00,$00,$00), // char 94
    ($00,$00,$00,$00,$00,$00,$1F,$00), // char 95
    ($08,$04,$02,$00,$00,$00,$00,$00), // char 96
    ($00,$00,$0E,$01,$0F,$11,$0F,$00), // char 97
    ($10,$10,$16,$19,$11,$11,$1E,$00), // char 98
    ($00,$00,$0E,$10,$10,$11,$0E,$00), // char 99
    ($01,$01,$0D,$13,$11,$11,$0F,$00), // char 100
    ($00,$00,$0E,$11,$1F,$10,$0E,$00), // char 101
    ($06,$09,$08,$1C,$08,$08,$08,$00), // char 102
    ($00,$0F,$11,$11,$0F,$01,$0E,$00), // char 103
    ($10,$10,$16,$19,$11,$11,$11,$00), // char 104
    ($04,$00,$0C,$04,$04,$04,$0E,$00), // char 105
    ($02,$00,$06,$02,$02,$12,$0C,$00), // char 106
    ($10,$10,$12,$14,$18,$14,$12,$00), // char 107
    ($0C,$04,$04,$04,$04,$04,$0E,$00), // char 108
    ($00,$00,$1A,$15,$15,$11,$11,$00), // char 109
    ($00,$00,$16,$19,$11,$11,$11,$00), // char 110
    ($00,$00,$0E,$11,$11,$11,$0E,$00), // char 111
    ($00,$00,$1E,$11,$1E,$10,$10,$00), // char 112
    ($00,$00,$0D,$13,$0F,$01,$01,$00), // char 113
    ($00,$00,$16,$19,$10,$10,$10,$00), // char 114
    ($00,$00,$0E,$10,$0E,$01,$1E,$00), // char 115
    ($08,$08,$1C,$08,$08,$09,$06,$00), // char 116
    ($00,$00,$11,$11,$11,$13,$0D,$00), // char 117
    ($00,$00,$11,$11,$11,$0A,$04,$00), // char 118
    ($00,$00,$11,$11,$15,$15,$0A,$00), // char 119
    ($00,$00,$11,$0A,$04,$0A,$11,$00), // char 120
    ($00,$00,$11,$11,$0F,$01,$0E,$00), // char 121
    ($00,$00,$1F,$02,$04,$08,$1F,$00), // char 122
    ($02,$04,$04,$08,$04,$04,$02,$00), // char 123
    ($04,$04,$04,$04,$04,$04,$04,$00), // char 124
    ($08,$04,$04,$02,$04,$04,$08,$00), // char 125
    ($00,$09,$16,$00,$00,$00,$00,$00), // char 126
    ($00,$04,$08,$1F,$08,$04,$00,$00), // char 127
    ($00,$00,$00,$00,$00,$00,$00,$00), // char 128 // extended stuff
    ($04,$04,$04,$04,$00,$00,$04,$00), // char 129
    ($0A,$0A,$0A,$00,$00,$00,$00,$00), // char 130
    ($0A,$0A,$1F,$0A,$1F,$0A,$0A,$00), // char 131 // custom
    ($04,$0F,$14,$0E,$05,$1E,$04,$00), // char 132 // custom
    ($18,$19,$02,$04,$08,$13,$03,$00), // char 133 // custom
    ($0C,$12,$14,$08,$15,$12,$0D,$00), // char 134 // custom
    ($0C,$04,$08,$00,$00,$00,$00,$00), // char 135 // custom
    ($02,$04,$08,$08,$08,$04,$02,$00), // char 136 // custom
    ($08,$04,$02,$02,$02,$04,$08,$00), // char 137
    ($00,$04,$15,$0E,$15,$04,$00,$00), // char 138
    ($00,$04,$04,$1F,$04,$04,$00,$00), // char 139
    ($00,$00,$00,$00,$0C,$04,$08,$00), // char 140
    ($00,$00,$00,$1F,$00,$00,$00,$00), // char 141
    ($00,$00,$00,$00,$00,$0C,$0C,$00), // char 142
    ($00,$01,$02,$04,$08,$10,$00,$00), // char 143
    ($0E,$11,$13,$15,$19,$11,$0E,$00), // char 144
    ($04,$0C,$04,$04,$04,$04,$0E,$00), // char 145
    ($0E,$11,$01,$02,$04,$08,$1F,$00), // char 146
    ($1F,$02,$04,$02,$01,$11,$0E,$00), // char 147
    ($02,$06,$0A,$12,$1F,$02,$02,$00), // char 148
    ($1F,$10,$1E,$01,$01,$11,$0E,$00), // char 149
    ($06,$08,$10,$1E,$11,$11,$0E,$00), // char 150
    ($1F,$01,$02,$04,$08,$08,$08,$00), // char 151
    ($0E,$11,$11,$0E,$11,$11,$0E,$00), // char 152
    ($0E,$11,$11,$0F,$01,$02,$0C,$00), // char 153
    ($00,$0C,$0C,$00,$0C,$0C,$00,$00), // char 154
    ($00,$0C,$0C,$00,$0C,$04,$08,$00), // char 155
    ($02,$04,$08,$10,$08,$04,$02,$00), // char 156
    ($00,$00,$1F,$00,$1F,$00,$00,$00), // char 157
    ($08,$04,$02,$01,$02,$04,$08,$00), // char 158 // custom
    ($0E,$11,$01,$02,$04,$00,$04,$00), // char 159
    ($0E,$11,$01,$0D,$15,$15,$0E,$00), // char 160
    ($0E,$11,$11,$11,$1F,$11,$11,$00), // char 161
    ($1E,$11,$11,$1E,$11,$11,$1E,$00), // char 162
    ($0E,$11,$10,$10,$10,$11,$0E,$00), // char 163
    ($1C,$12,$11,$11,$11,$12,$1C,$00), // char 164
    ($1F,$10,$10,$1E,$10,$10,$1F,$00), // char 165
    ($1F,$10,$10,$1E,$10,$10,$10,$00), // char 166
    ($0E,$11,$10,$17,$11,$11,$0F,$00), // char 167
    ($11,$11,$11,$1F,$11,$11,$11,$00), // char 168
    ($0E,$04,$04,$04,$04,$04,$0E,$00), // char 169
    ($07,$02,$02,$02,$02,$12,$0C,$00), // char 170
    ($11,$12,$14,$18,$14,$12,$11,$00), // char 171
    ($10,$10,$10,$10,$10,$10,$1F,$00), // char 172
    ($11,$1B,$15,$15,$11,$11,$11,$00), // char 173
    ($11,$11,$19,$15,$13,$11,$11,$00), // char 174
    ($0E,$11,$11,$11,$11,$11,$0E,$00), // char 175
    ($1E,$11,$11,$1E,$10,$10,$10,$00), // char 176 // custom
    ($0E,$11,$11,$11,$15,$12,$0D,$00), // char 177
    ($1E,$11,$11,$1E,$14,$12,$11,$00), // char 178
    ($0F,$10,$10,$0E,$01,$01,$1E,$00), // char 179
    ($1F,$04,$04,$04,$04,$04,$04,$00), // char 180
    ($11,$11,$11,$11,$11,$11,$0E,$00), // char 181
    ($11,$11,$11,$11,$11,$0A,$04,$00), // char 182
    ($11,$11,$11,$11,$15,$15,$0A,$00), // char 183
    ($11,$11,$0A,$04,$0A,$11,$11,$00), // char 184
    ($11,$11,$11,$0A,$04,$04,$04,$00), // char 185
    ($1F,$01,$02,$04,$08,$10,$1F,$00), // char 186
    ($0E,$08,$08,$08,$08,$08,$0E,$00), // char 187
    ($00,$10,$08,$04,$02,$01,$00,$00), // char 188
    ($0E,$02,$02,$02,$02,$02,$0E,$00), // char 189
    ($04,$0A,$11,$00,$00,$00,$00,$00), // char 190
    ($00,$00,$00,$00,$00,$00,$1F,$00), // char 191
    ($0E,$0A,$0E,$00,$00,$00,$00,$00), // char 192
    ($00,$00,$0E,$01,$0F,$11,$0F,$00), // char 193
    ($10,$10,$16,$19,$11,$11,$1E,$00), // char 194
    ($00,$00,$0E,$10,$10,$11,$0E,$00), // char 195
    ($01,$01,$0D,$13,$11,$11,$0F,$00), // char 196
    ($00,$00,$0E,$11,$1F,$10,$0E,$00), // char 197
    ($06,$09,$08,$1C,$08,$08,$08,$00), // char 198
    ($00,$0F,$11,$11,$0F,$01,$0E,$00), // char 199
    ($10,$10,$16,$19,$11,$11,$11,$00), // char 200
    ($04,$00,$0C,$04,$04,$04,$0E,$00), // char 201
    ($02,$00,$06,$02,$02,$12,$0C,$00), // char 202
    ($10,$10,$12,$14,$18,$14,$12,$00), // char 203
    ($0C,$04,$04,$04,$04,$04,$0E,$00), // char 204
    ($00,$00,$1A,$15,$15,$11,$11,$00), // char 205
    ($00,$00,$16,$19,$11,$11,$11,$00), // char 206
    ($00,$00,$0E,$11,$11,$11,$0E,$00), // char 207
    ($00,$00,$1E,$11,$1E,$10,$10,$00), // char 208
    ($00,$00,$0D,$13,$0F,$01,$01,$00), // char 209
    ($00,$00,$16,$19,$10,$10,$10,$00), // char 210
    ($00,$00,$0E,$10,$0E,$01,$1E,$00), // char 211
    ($08,$08,$1C,$08,$08,$09,$06,$00), // char 212
    ($00,$00,$11,$11,$11,$13,$0D,$00), // char 213
    ($00,$00,$11,$11,$11,$0A,$04,$00), // char 214
    ($00,$00,$11,$11,$15,$15,$0A,$00), // char 215
    ($00,$00,$11,$0A,$04,$0A,$11,$00), // char 216
    ($00,$00,$11,$11,$0F,$01,$0E,$00), // char 217
    ($00,$00,$1F,$02,$04,$08,$1F,$00), // char 218
    ($02,$04,$04,$08,$04,$04,$02,$00), // char 219
    ($04,$04,$04,$04,$04,$04,$04,$00), // char 220
    ($08,$04,$04,$02,$04,$04,$08,$00), // char 221
    ($00,$09,$16,$00,$00,$00,$00,$00), // char 222
    ($1C,$14,$1C,$00,$00,$00,$00,$00), // char 223 // square degree
    ($00,$00,$00,$00,$00,$00,$00,$00), // char 224
    ($04,$04,$04,$04,$00,$00,$04,$00), // char 225
    ($0A,$0A,$0A,$00,$00,$00,$00,$00), // char 226
    ($0A,$0A,$1F,$0A,$1F,$0A,$0A,$00), // char 227
    ($04,$0F,$14,$0E,$05,$1E,$04,$00), // char 228
    ($18,$19,$02,$04,$08,$13,$03,$00), // char 229
    ($0C,$12,$14,$08,$15,$12,$0D,$00), // char 230
    ($0C,$04,$08,$00,$00,$00,$00,$00), // char 231
    ($02,$04,$08,$08,$08,$04,$02,$00), // char 232
    ($08,$04,$02,$02,$02,$04,$08,$00), // char 233
    ($00,$04,$15,$0E,$15,$04,$00,$00), // char 234
    ($00,$04,$04,$1F,$04,$04,$00,$00), // char 235
    ($00,$00,$00,$00,$0C,$04,$08,$00), // char 236
    ($00,$00,$00,$1F,$00,$00,$00,$00), // char 237
    ($00,$00,$00,$00,$00,$0C,$0C,$00), // char 238
    ($00,$01,$02,$04,$08,$10,$00,$00), // char 239
    ($0E,$11,$13,$15,$19,$11,$0E,$00), // char 240
    ($04,$0C,$04,$04,$04,$04,$0E,$00), // char 241
    ($0E,$11,$01,$02,$04,$08,$1F,$00), // char 242
    ($1F,$02,$04,$02,$01,$11,$0E,$00), // char 243
    ($02,$06,$0A,$12,$1F,$02,$02,$00), // char 244
    ($1F,$10,$1E,$01,$01,$11,$0E,$00), // char 245
    ($06,$08,$10,$1E,$11,$11,$0E,$00), // char 246
    ($1F,$01,$02,$04,$08,$08,$08,$00), // char 247
    ($0E,$11,$11,$0E,$11,$11,$0E,$00), // char 248
    ($0E,$11,$11,$0F,$01,$02,$0C,$00), // char 249
    ($00,$0C,$0C,$00,$0C,$0C,$00,$00), // char 250
    ($00,$0C,$0C,$00,$0C,$04,$08,$00), // char 251
    ($02,$04,$08,$10,$08,$04,$02,$00), // char 252
    ($00,$00,$1F,$00,$1F,$00,$00,$00), // char 253
    ($08,$04,$02,$01,$02,$04,$08,$00), // char 254
    ($1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F)); // char 255 // full block

type
  TConfig = record
    StayOnTop: boolean;
    AlphaBlend: boolean;
    AlphaBlendValue: integer;
    PosX: integer;
    PosY: integer;
  end;

type
  TFontArray = array[32..255] of TBitmap;

  { TLCDDisplayForm }

  TLCDDisplayForm = class(TForm)
    AlphaOffMenuItem: TMenuItem;
    Alpha90MenuItem: TMenuItem;
    Alpha10MenuItem: TMenuItem;
    Alpha20MenuItem: TMenuItem;
    Alpha30MenuItem: TMenuItem;
    Alpha40MenuItem: TMenuItem;
    Alpha50MenuItem: TMenuItem;
    Alpha60MenuItem: TMenuItem;
    Alpha70MenuItem: TMenuItem;
    Alpha80MenuItem: TMenuItem;
    StayOnTopMenuItem: TMenuItem;
    TransparentMenuItem: TMenuItem;
    PaintBox: TPaintBox;
    PopupMenu1: TPopupMenu;
    Timer1: TTimer;
    procedure AlphaMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Free;
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PopupMenu1Close(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    Config: TConfig;
    MyWidth,MyHeight : integer;
    WidthOffset,HeightOffset : integer;
    CurrentX,CurrentY : integer;
    Font : TFontArray;
    BackgroundBitmap : TBitmap;
    BackgroundColor : TColor;
    FrameBuffer : array[1..MaxWidth*MaxHeight] of byte;
    CaptionHeight: long;
    BorderWidth: long;
    procedure readconfig;
    procedure writeconfig;
    procedure WMNCHitTest(var Msg: TWMNCHitTest) ; message WM_NCHitTest;
    procedure LoadFont;
    procedure GetBlankChar(Bitmap : TBitmap);
    procedure GetDefaultBitmap(Index : byte; Bitmap : TBitmap);
    procedure SendChar(C : byte);
    procedure ClearDisplay;
  public
    { Public declarations }
    procedure SetSize(X,Y : integer);
    procedure SetPosition(X,Y : integer);
    procedure ScreenWrite(S : string);
    procedure SetBacklight(LightOn : boolean);
    procedure CustomChar(Index : byte; Bytes : array of byte);
  end;

implementation

{$R *.lfm}

uses
  Math;

procedure TLCDDisplayForm.readconfig;
var
  ini: TINIFile;
begin
  ini := TINIFile.Create('displays\desktopcfg.ini');
  config.StayOnTop := ini.ReadBool('config', 'StayOnTop', false);
  config.AlphaBlend := ini.ReadBool('config', 'AlphaBlend', false);
  config.AlphaBlendValue := ini.ReadInteger('config', 'AlphaBlendValue', 255);
  config.PosX := ini.ReadInteger('config', 'PosX', 0);
  config.PosY := ini.ReadInteger('config', 'PosY', 0);
  ini.Free;
end;

procedure TLCDDisplayForm.writeconfig;
var
  ini: TINIFile;
begin
  ini := TINIFile.Create('displays\desktopcfg.ini');
  ini.WriteBool('config', 'StayOnTop', config.StayOnTop);
  ini.WriteBool('config', 'AlphaBlend', config.AlphaBlend);
  ini.WriteInteger('config', 'AlphaBlendValue', config.AlphaBlendValue);
  ini.WriteInteger('config', 'PosX', config.PosX);
  ini.WriteInteger('config', 'PosY', config.PosY);
  ini.Free;
end;

procedure TLCDDisplayForm.Timer1Timer(Sender: TObject);
const
 SWP_NOMOVE = $0002;
 SWP_NOSIZE = $0001;
 SWP_NOACTIVATE = $0010;
 dwSWPflags = SWP_NOMOVE or  SWP_NOSIZE or SWP_NOACTIVATE;
begin
   if StayOnTopMenuItem.Checked then
     SetWindowPos(Handle, HWND_TOPMOST,	0,0,0,0, dwSWPflags)
   else
     SetWindowPos(Handle, HWND_NOTOPMOST, 0,0,0,0, dwSWPflags);

   if (config.StayOnTop <> StayOnTopMenuItem.Checked) or (config.AlphaBlend <> AlphaBlend) or
        (config.AlphaBlendValue <> AlphaBlendValue) or (config.PosY <> Top) or (config.PosX <> Left) then
   begin
     config.StayOnTop := StayOnTopMenuItem.Checked;
     config.AlphaBlend := AlphaBlend;
     config.AlphaBlendValue := AlphaBlendValue;
     config.PosY := Top;
     config.PosX := Left;
     writeconfig;
   end;
end;

procedure TLCDDisplayForm.WMNCHitTest(var Msg: TWMNCHitTest) ;
 var
  Res: SmallInt;
begin
  inherited;
  Res := GetKeyState(VK_RBUTTON);
  if Res > 0 then
    if Msg.Result = htClient then
      Msg.Result := htCaption;
end;

procedure TLCDDisplayForm.LoadFont;
var
  Loop : longint;
begin
  for Loop := 32 to 255 do begin
    Font[Loop] := TBitmap.Create;
    Font[Loop].Width := CharWidth;
    Font[Loop].Height := CharHeight;
    case loop of
      176, 158, 131, 132,
      133, 134, 135, 136 : GetBlankChar(Font[Loop]);
      else GetDefaultBitmap(Loop,Font[Loop]);
    end;
  end;
end;

procedure TLCDDisplayForm.GetBlankChar(Bitmap : TBitmap);
var
  X,Y : byte;
begin
  for Y := 0 to CharHeight-1 do begin
    for X := 0 to CharWidth-1 do begin
      Bitmap.Canvas.Pixels[X,Y] := clWhite;
    end;
  end;
end;

procedure TLCDDisplayForm.GetDefaultBitmap(Index : byte; Bitmap : TBitmap);
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

procedure TLCDDisplayForm.AlphaMenuItemClick(Sender: TObject);
begin
  if TMenuItem(Sender).Name = 'AlphaOffMenuItem' then
    Alphablend := false
  else
    begin
    Alphablend := true;
    if TMenuItem(Sender).Name = 'Alpha10MenuItem' then
      AlphaBlendValue := 229
    else if TMenuItem(Sender).Name = 'Alpha20MenuItem' then
      AlphaBlendValue := 180
    else if TMenuItem(Sender).Name = 'Alpha30MenuItem' then
      AlphaBlendValue := 178
    else if TMenuItem(Sender).Name = 'Alpha40MenuItem' then
      AlphaBlendValue := 153
    else if TMenuItem(Sender).Name = 'Alpha50MenuItem' then
      AlphaBlendValue := 127
    else if TMenuItem(Sender).Name = 'Alpha60MenuItem' then
      AlphaBlendValue := 102
    else if TMenuItem(Sender).Name = 'Alpha70MenuItem' then
      AlphaBlendValue := 76
    else if TMenuItem(Sender).Name = 'Alpha80MenuItem' then
      AlphaBlendValue := 51
    else if TMenuItem(Sender).Name = 'Alpha90MenuItem' then
      AlphaBlendValue := 25;
    end;
end;

procedure TLCDDisplayForm.FormCreate(Sender: TObject);
begin
  readconfig;
  BackgroundColor := clLime;
  BackgroundBitmap := TBitmap.Create;
  CurrentX := 1;
  CurrentY := 1;
  MyWidth := 40;
  MyHeight := 4;
  WidthOffset := Width - PaintBox.Width;
  HeightOffset := Height - PaintBox.Height;
  fillchar(Font,sizeof(Font),$00);
  fillchar(FrameBuffer,sizeof(FrameBuffer),32);
  LoadFont;
  show;
end;

procedure TLCDDisplayForm.FormShow(Sender: TObject);
var
  Style: long;
begin
  Left := config.PosX;
  Top := config.PosY;
  StayOnTopMenuItem.Checked := config.StayOnTop;
  Width := (MyWidth*(CharWidth+1)*2);
  Height := (MyHeight*(CharHeight+1)*2);

  if assigned(BackgroundBitmap) then BackgroundBitmap.Free;
  BackgroundBitmap := TBitmap.Create;
  with BackgroundBitmap do begin
    Width := 1+ MyWidth*(CharWidth+1);
    Height := 1 + MyHeight*(CharHeight+1);
    Canvas.Pen.Color := BackgroundColor;
    Canvas.Brush.Color := BackgroundColor;
    Canvas.Rectangle(1,1,Width,Height);
    ClearDisplay;
  end;

  Style:=GetWindowLong(Handle, GWL_STYLE);
  CaptionHeight := GetSystemMetrics(SM_CYCAPTION);
  BorderWidth := GetSystemMetrics(SM_CXFIXEDFRAME); // making the assumption horizontal and vertical frame are the same
  SetWindowLong(Handle,GWL_STYLE, Style and (not WS_CAPTION));
  AlphaBlendValue := config.AlphaBlendValue;
  Alphablend := config.AlphaBlend;

  if config.AlphaBlend = false then
    AlphaOffMenuItem.Checked := false
  else
    begin
    if config.AlphaBlendValue >= 229 then
      Alpha10MenuItem.Checked := true
    else if config.AlphaBlendValue >= 180 then
      Alpha20MenuItem.Checked := true
    else if config.AlphaBlendValue >= 178 then
      Alpha30MenuItem.Checked := true
    else if config.AlphaBlendValue >= 153 then
      Alpha40MenuItem.Checked := true
    else if config.AlphaBlendValue >= 127 then
      Alpha50MenuItem.Checked := true
    else if config.AlphaBlendValue >= 102 then
      Alpha60MenuItem.Checked := true
    else if config.AlphaBlendValue >= 76 then
      Alpha70MenuItem.Checked := true
    else if config.AlphaBlendValue >= 51 then
      Alpha80MenuItem.Checked := true
    else if config.AlphaBlendValue >= 25 then
      Alpha90MenuItem.Checked := true
    end;
end;

procedure TLCDDisplayForm.Free;
var
  Loop : longint;
begin
  for Loop := 32 to 255 do begin
    Font[Loop].Free;
  end;
  inherited;
end;

procedure TLCDDisplayForm.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    ReleaseCapture;
    SendMessage(Handle, WM_SYSCOMMAND, $F012, 0) ; //  61458
  end
  else
  begin
   popupmenu1.PopUp;
  end;
end;

procedure TLCDDisplayForm.PaintBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Timer1Timer(nil);
end;

procedure TLCDDisplayForm.SetPosition(X,Y : integer);
begin
  CurrentX := X;
  CurrentY := Y;
end;

procedure TLCDDisplayForm.ClearDisplay;
begin
  with BackgroundBitmap.Canvas do begin
    CopyMode := cmSrcCopy;
    Pen.Color := BackgroundColor;
    Pen.Mode := pmCopy;
    Pen.Style := psSolid;
    Pen.Width := 1;
    Brush.Color := BackgroundColor;
    Brush.Style := bsSolid;
    Rectangle(0,0,Width,Height);
  end;
end;

procedure TLCDDisplayForm.SetSize(X,Y : integer);
begin
  X := min(MaxWidth,X);
  Y := min(MaxHeight,Y);
  MyWidth := X;
  MyHeight := Y;
  Height := (MyHeight*(CharHeight+1)*2) - captionheight - borderwidth;
  Width := (MyWidth*(CharWidth+1)*2) - borderwidth - 1; // not sure where the extra dimensions come from
  PaintBox.Height := Height + captionheight + borderwidth + borderwidth;
  PaintBox.Width := Width + borderwidth + 3;

  if assigned(BackgroundBitmap) then BackgroundBitmap.Free;
  BackgroundBitmap := TBitmap.Create;
  with BackgroundBitmap do begin
    Width := 1+ MyWidth*(CharWidth+1);
    Height := 1 + MyHeight*(CharHeight+1);
    Canvas.Pen.Color := BackgroundColor;
    Canvas.Brush.Color := BackgroundColor;
    Canvas.Rectangle(1,1,Width,Height);
    ClearDisplay;
  end;
end;

procedure TLCDDisplayForm.PaintBoxPaint(Sender: TObject);
var
  DestRect : TRect;
begin
  with DestRect do begin
    Top := 0;
    Bottom := PaintBox.Height;
    Left := 0;
    Right := PaintBox.Width;
  end;
  PaintBox.Canvas.StretchDraw(DestRect,BackgroundBitmap);
end;

procedure TLCDDisplayForm.PopupMenu1Close(Sender: TObject);
begin
  timer1.Enabled:= true;
end;

procedure TLCDDisplayForm.PopupMenu1Popup(Sender: TObject);
begin
  timer1.Enabled:=false;
end;

procedure TLCDDisplayForm.SendChar(C : byte);
var
  CurChar : TBitmap;
  DestRect : TRect;
  SrcRect : TRect;
begin
  FrameBuffer[CurrentX+(CurrentY-1)*MyWidth] := C;
  CurChar := Font[C];
  with SrcRect do begin
    Left := 0;
    Right := CharWidth;
    Top := 0;
    Bottom := CharHeight;
  end;
  with DestRect do begin
    Top := ((CurrentY-1)*(CharHeight+1))+1;
    Bottom := Top + CharHeight;
    Left := ((CurrentX-1)*(CharWidth+1))+1;
    Right := Left + CharWidth;
  end;
  BackgroundBitmap.Canvas.CopyRect(DestRect,CurChar.Canvas,SrcRect);
  inc(CurrentX);
  if (CurrentX > MyWidth) then begin
    CurrentX := 1;
    inc(CurrentY);
    if (CurrentY > MyHeight) then
      CurrentY := 1;
  end;
end;

procedure TLCDDisplayForm.ScreenWrite(S : string);
var
  C : byte;
  Loop : integer;
begin
  for Loop := 1 to length(S) do begin
    C := ord(S[Loop]);
    SendChar(C);
  end;
  PaintBoxPaint(nil);
end;

procedure TLCDDisplayForm.SetBacklight(LightOn : boolean);
var
  Loop,OldPosX,OldPosY : longint;
begin
  if LightOn then BackgroundColor := clLime
  else BackgroundColor := clGreen;
  ClearDisplay;
  OldPosX := CurrentX;
  OldPosY := CurrentY;
  CurrentX := 1;
  CurrentY := 1;
  for Loop := 1 to MyHeight*MyWidth do begin
    SendChar(FrameBuffer[Loop]);
  end;
  CurrentX := OldPosX;
  CurrentY := OldPosY;
  PaintBoxPaint(nil);
end;

procedure TLCDDisplayForm.CustomChar(Index : byte; Bytes : array of byte);
var
  X,Y,C : byte;
begin
  case index of
    1: C:=176;
    2: C:=158;
    3: C:=131;
    4: C:=132;
    5: C:=133;
    6: C:=134;
    7: C:=135;
    8: C:=136;
  end;

  Index := min(8,max(1,Index));
  with Font[C].Canvas do begin
    for X := 0 to CharWidth-1 do begin
      for Y := 0 to CharHeight-1 do begin
        if ((Bytes[Y] and (1 shl X)) > 0) then
          Pixels[CharWidth-1-X,Y] := clBlack
        else
          Pixels[CharWidth-1-X,Y] := clWhite;
      end;
    end;
  end;
  SetBacklight(BackgroundColor = clLime);
end;

end.
