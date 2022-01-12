unit UCFSetup;
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
 *  $Source: /root/lcdsmartie-cvsbackup/lcdsmartie/UCFSetup.pas,v $
 *  $Revision: 1.8 $ $Date: 2006/02/27 22:45:38 $
 *****************************************************************************}

interface

uses Forms, StdCtrls, ComCtrls, Classes, Controls;

type
  TCrystalFontzSetupForm = class(TForm)
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    OKButton: TButton;
    V2CGROMCheckbox: TCheckBox;
    ContrastTrackBar: TTrackBar;
    BacklightTrackBar: TTrackBar;
    CancelButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure ContrastTrackBarChange(Sender: TObject);
    procedure BacklightTrackBarChange(Sender: TObject);
    procedure V2CGROMCheckboxClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function DoCrystalFontzSetupForm : boolean;

implementation

uses
  UMain, USetup, UConfig;

{$R *.LFM}

function DoCrystalFontzSetupForm : boolean;
var
  CrystalFontzSetupForm : TCrystalFontzSetupForm;
begin
  CrystalFontzSetupForm := TCrystalFontzSetupForm.Create(nil);
  with CrystalFontzSetupForm do begin
    // put settings on screen
    ContrastTrackBar.position := config.CF_contrast;
    BacklightTrackBar.position := config.CF_brightness;
    V2CGROMCheckbox.checked := (config.iCF_cgrom = 2);
    ShowModal;
    Result := (ModalResult = mrOK);
    if Result then begin
      config.CF_contrast := ContrastTrackBar.position;
      config.CF_brightness := BacklightTrackBar.position;
    end;
    Free;
  end;
end;

procedure TCrystalFontzSetupForm.FormShow(Sender: TObject);
begin
end;

// CF options - contrast bar.
procedure TCrystalFontzSetupForm.ContrastTrackBarChange(Sender: TObject);
begin
  if (config.ScreenType = stCF) then
    LCDSmartieDisplayForm.lcd.setContrast(ContrastTrackBar.Position);
end;

// CF options - brightness bar.
procedure TCrystalFontzSetupForm.BacklightTrackBarChange(Sender: TObject);
begin
  if (config.ScreenType = stCF) then
    LCDSmartieDisplayForm.lcd.setBrightness(BacklightTrackBar.Position);
end;

procedure TCrystalFontzSetupForm.V2CGROMCheckboxClick(Sender: TObject);
begin
  if (V2CGROMCheckbox.checked) then
    config.iCF_cgrom := 2
  else
    config.iCF_cgrom := 1;

  LCDSmartieDisplayForm.DoFullDisplayDraw();
end;

end.
