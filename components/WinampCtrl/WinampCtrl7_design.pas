{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit WinampCtrl7_design;

{$warn 5023 off : no warning about unused units}
interface

uses
  WinampCtrlReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('WinampCtrlReg', @WinampCtrlReg.Register);
end;

initialization
  RegisterPackage('WinampCtrl7_design', @Register);
end.
