{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit JPLaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  JPLazRegister, JPLazSpinEdit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('JPLazRegister', @JPLazRegister.Register);
end;

initialization
  RegisterPackage('JPLaz', @Register);
end.
