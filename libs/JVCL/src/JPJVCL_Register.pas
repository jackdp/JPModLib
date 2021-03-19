unit JPJVCL_Register;

interface

uses 
  Windows, SysUtils, Classes, JpJvSpinEdit, JpJvCheckBox, JpJvRadioButton;

{$I ..\..\JPModLib.inc}

procedure Register;

implementation


procedure Register;
begin
  RegisterComponents(PageName, [TJPJvSpinEdit, TJPJvCheckBox, TJPJvRadioButton]);
end;


end.