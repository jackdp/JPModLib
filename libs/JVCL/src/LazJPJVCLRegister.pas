unit LazJPJVCLRegister;

interface

uses
  Forms, LCLIntf, LCLType, LMessages, Classes, Graphics, Controls, SysUtils, StdCtrls,
  TypInfo, lresources,
  //JpJvSpinEdit,
  JpJvCheckBox
  //, JpJvRadioButton
  //,JpJvRollOutPanel
  ;           
  
{$I ..\..\JPModLib.inc}

procedure Register;

implementation

procedure Register;

begin
  //RegisterComponents(PageName, [TJpJvRollOutPanel]);
  //RegisterComponents(PageName, [TJPJvSpinEdit, TJPJvCheckBox, TJPJvRadioButton]);
  //RegisterComponents(PageName, [TJPJvCheckBox, TJPJvRadioButton]);
  RegisterComponents(PageName, [TJPJvCheckBox]);
end;


initialization

end.
