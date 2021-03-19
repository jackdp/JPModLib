@echo off

set Template=JPEssentials_TEMPLATE.dpk
set Desc="JP TurboPower Essentials"
set BaseOutDir=..\
set ShortDpkName=JPEssentials.dpk

for %%x in (2009,2010,XE,XE2,XE3,XE4,XE5,XE6,XE7,XE8) do (
  DpkGen -t %Template% -d %Desc% -o %BaseOutDir%%%x\%ShortDpkName% -s %%x
)

DpkGen -t %Template% -d %Desc% -o %BaseOutDir%10.0_Seattle\%ShortDpkName% -s Seattle
DpkGen -t %Template% -d %Desc% -o %BaseOutDir%10.1_Berlin\%ShortDpkName% -s Berlin
DpkGen -t %Template% -d %Desc% -o %BaseOutDir%10.2_Tokyo\%ShortDpkName% -s Tokyo
DpkGen -t %Template% -d %Desc% -o %BaseOutDir%10.3_Rio\%ShortDpkName% -s Rio
DpkGen -t %Template% -d %Desc% -o %BaseOutDir%10.4_Sydney\%ShortDpkName% -s Sydney
