unit JPEsRegister;

interface

uses 
  Winapi.Windows, System.SysUtils, System.Classes, JPEsGrad;

{$I ..\..\JPModLib.inc}

procedure Register;

implementation


procedure Register;
begin
  RegisterComponents(PageName, [TJPEsGradient]);
end;


end.