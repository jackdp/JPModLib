unit LazJPJVCLRegister;

interface

uses
  Forms, LCLIntf, LCLType, LMessages, Classes, Graphics, Controls, SysUtils, StdCtrls,
  TypInfo, lresources,
  JpJvRollOutPanel
  ;           
  
{$I ..\..\JPModLib.inc}

procedure Register;

implementation

procedure Register;

begin
  RegisterComponents(PageName, [TJpJvRollOutPanel]);

end;


initialization

end.
