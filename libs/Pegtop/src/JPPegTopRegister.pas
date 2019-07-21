unit JPPegTopRegister;

interface

uses
  Windows, SysUtils, Classes,
  JPPegtopTrackBars
  ;

{$INCLUDE PegtopDelphiVersions.inc}
{$I ..\..\JPModLib.inc}

procedure Register;


implementation


procedure Register;
begin
  RegisterComponents(PageName, [TJPPegtopTrackBar, TJPPegtopColorTrackBar, TJPPegtopRangeBar]);
end;



end.
