unit WinampCtrlReg;

{ Created by Stu Pidass = Lee_Nover }
{ I've used all of the functions I found on winamp , DO use the Control,
  maybe change it ? taka a look at the code for explanations and stuff...
  comments, suggestions, anything .. to Lee.Nover@Email.si }


interface

uses
  Windows, Messages, SysUtils, Classes, Dialogs, {DesignConst, DesignEditors,
  DesignIntf,} propedits, ExtCtrls, WinampCtrl;

type
  TFileNameProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;
  
  procedure Register;

implementation

// property editor -------------------------------------------------------------

procedure TFileNameProperty.Edit;
begin
  with TOpenDialog.Create(nil) do
  try
    Filter:='WinAmp(Winamp.exe)|WinAmp.exe|Executables(*.exe)|*.exe|Any File|*.*';
    Options:=Options+[ofFileMustExist,ofPathMustExist];
    FileName := GetStrValue;
    if Execute then
      SetStrValue(FileName);
  finally
    Free;
  end;
end;

function TFileNameProperty.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TFileNameProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

function TFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure Register;
begin
  RegisterComponents('WinampCtrl Package', [TWinampCtrl]);
  RegisterPropertyEditor(TypeInfo(TFileName), TWinampCtrl, 'WinAmpLocation', TFileNameProperty);
end;

end.
