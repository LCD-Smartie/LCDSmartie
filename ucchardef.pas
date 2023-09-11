unit UCCharDef;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, StrUtils, UUtils, UConfig;

//type
//  TCustomArray = array[0..7] of byte;

type

  { TCCharDefForm }

  TCCharDefForm = class(TForm)
    CCharDefFormOKButton: TButton;
    CCharDefFormCancelButton: TButton;
    CCharDefFormApplyButton: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    procedure EditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadCCharSpeedButtonGlyphs;
    procedure CustomCharToButtonGlyph(Button: TSpeedButton; CustomArray: TCustomArray);
  private

  public
    EditArray: array [1..8] of TEdit;
    buttonArray: array [1..8] of TSpeedButton;
  end;

var
  CCharDefForm: TCCharDefForm;

implementation

{$R *.lfm}

{ TCCharDefForm }

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

procedure TCCharDefForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  EditArray[1] := Edit1; EditArray[2] := Edit2;
  EditArray[3] := Edit3; EditArray[4] := Edit4;
  EditArray[5] := Edit5; EditArray[6] := Edit6;
  EditArray[7] := Edit7; EditArray[8] := Edit8;
  buttonArray[1] := SpeedButton1; buttonArray[2] := SpeedButton2;
  buttonArray[3] := SpeedButton3; buttonArray[4] := SpeedButton4;
  buttonArray[5] := SpeedButton5; buttonArray[6] := SpeedButton6;
  buttonArray[7] := SpeedButton7; buttonArray[8] := SpeedButton8;

  for i := 1 to 8 do
    EditArray[i].OnChange := EditChange;

  LoadCCharSpeedButtonGlyphs;
end;

procedure TCCharDefForm.CustomCharToButtonGlyph(Button: TSpeedButton; CustomArray: TCustomArray);
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

procedure TCCharDefForm.LoadCCharSpeedButtonGlyphs;
var
  loop: integer;
  CustomArray: TCustomArray;
begin
  for loop := 1 to 8 do
  begin
    CustomArray := default(TCustomArray);
    CustomArray := CustomStringToCustomArray(EditArray[loop].Text);
    CustomCharToButtonGlyph(buttonArray[loop], CustomArray);
  end;
end;

procedure TCCharDefForm.EditChange(Sender: TObject);
begin
  LoadCCharSpeedButtonGlyphs;
end;

end.

