unit UCredits;

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
 *  $Source: /root/lcdsmartie-cvsbackup/lcdsmartie/UCredits.pas,v $
 *  $Revision: 1.12 $ $Date: 2011/06/04 16:48:30 $
 *****************************************************************************}

interface

uses Forms, ExtCtrls, Classes, StdCtrls, Graphics, Controls;

type

  { TCreditsForm }

  TCreditsForm = class(TForm)
    ThisVersion: TLabel;
    RootPanel: TPanel;
    ScrollPanel: TPanel;
    CreditPaintBox: TPaintBox;
    OrigLabel: TLabel;
    Image1: TImage;
    Timer1: TTimer;
    HTMLLabel: TLabel;
    ForumLabel: TLabel;
    Label3: TLabel;
    procedure CloseClick(Sender: TObject);
    procedure HTMLLabelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ThisVersionClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ForumLabelClick(Sender: TObject);
  private
    { Private declarations }
    CreditList : TStringList;
    YCoord : longint;
    YMax : longint;
    LineHeight : longint;
  public
    { Public declarations }
  end;

procedure DoCreditsForm;

implementation

uses Windows, ShellApi;

{$R *.lfm}

procedure DoCreditsForm;
var
  CreditsForm: TCreditsForm;
begin
  CreditsForm := TCreditsForm.Create(nil);
  with CreditsForm do begin
    ShowModal;
    Free;
  end;
end;

procedure TCreditsForm.CloseClick(Sender: TObject);
begin
  Close;
end;

procedure TCreditsForm.HTMLLabelClick(Sender: TObject);
begin
  ShellExecute(0, Nil, pchar('https://lcdsmartie.sourceforge.net'), Nil, Nil,
    SW_NORMAL);
end;

procedure TCreditsForm.ForumLabelClick(Sender: TObject);
begin
  ShellExecute(0, Nil, pchar('https://www.lcdsmartie.org'), Nil, Nil,
    SW_NORMAL);
end;

procedure TCreditsForm.ThisVersionClick(Sender: TObject);
begin
  ShellExecute(0, Nil, pchar('https://github.com/stokie-ant/lcdsmartie-laz'), Nil, Nil,
    SW_NORMAL);
end;

procedure TCreditsForm.FormCreate(Sender: TObject);
begin
  {$ifopt D+}
    Image1.Picture.LoadFromResourceName(HInstance, 'ABOUT LOGO BLUE');
  {$else}
    Image1.Picture.LoadFromResourceName(HInstance, 'ABOUT LOGO RED');
  {$endif}

  CreditList := TStringList.Create;
  with CreditList do begin    // add core developers here in alpha order
    Add('Core Development:');
    Add('Afonso Infante');
    Add('Carlo Adami');
    Add('Chris Lansley');
    Add('Cristiano Vaccarini');
    Add('Mike van Meeteren');
    Add('Anthony Blakemore');
    Add(' ');
    Add('Program Support:');
    Add('Nikos Georgousis (Limbo)');
    Add('Jason Jacobs (X7JAY7X)');
  end;
  with CreditPaintBox.Canvas do begin
    with Brush do begin
      Color := ScrollPanel.Color;
      Style := bsSolid;
    end;
    with Pen do begin
      Color := ScrollPanel.Color;
      Style := psSolid;
      Mode := pmCopy;
    end;
    LineHeight := TextHeight('X')*3 div 2;
    YMax := LineHeight*CreditList.Count+ScrollPanel.Height;
    YCoord := YMax;
  end;
end;

procedure TCreditsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  CreditList.Free;
end;

procedure TCreditsForm.Timer1Timer(Sender: TObject);
var
  Y,Loop : longint;
begin
  inc(YCoord);
  if (YCoord > YMax) then begin
    with CreditPaintBox.Canvas do begin
      Rectangle(0,0,ScrollPanel.Width,ScrollPanel.Height);
    end;
    YCoord := 0;
  end;
  with CreditPaintBox.Canvas do begin
    for Loop := 0 to CreditList.Count-1 do begin
      Y := (ScrollPanel.Height-YCoord)+(Loop*LineHeight);
      Rectangle(0,Y-1,ScrollPanel.Width,Y+LineHeight+2);
      TextOut(5,Y,CreditList[Loop]);
    end;
  end;
end;

end.
