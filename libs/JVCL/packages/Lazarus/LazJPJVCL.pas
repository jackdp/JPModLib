{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazJPJVCL;

{$warn 5023 off : no warning about unused units}
interface

uses
  JpJvRollOutPanel, LazJPJVCLRegister, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazJPJVCLRegister', @LazJPJVCLRegister.Register);
end;

initialization
  RegisterPackage('LazJPJVCL', @Register);
end.
