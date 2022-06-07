unit JPLazRegister;

interface

uses
  Forms, LCLIntf, LCLType, LMessages, Classes, Graphics, Controls, SysUtils, StdCtrls,
  TypInfo, lresources,
  JPLazSpinEdit
  ;           
  
{$I ..\..\JPModLib.inc}

procedure Register;

implementation

procedure Register;

begin
  RegisterComponents(PageName, [TJPLazFloatSpinEdit, TJPLazSpinEdit]);

end;


initialization

end.
