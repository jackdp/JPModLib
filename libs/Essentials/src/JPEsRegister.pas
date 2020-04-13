unit JPEsRegister;

interface

uses 
  Windows, SysUtils, Classes, JPEsGrad;

{$I ..\..\JPModLib.inc}

procedure Register;

implementation


procedure Register;
begin
  RegisterComponents(PageName, [TJPEsGradient]);
end;


end.