@echo off

set ResCompiler=E:\Embarcadero\XE1\bin\cgrc.exe
set OutDir=..\src


%ResCompiler% -c65001 "JPPegtopTrackBars.rc" -foJPPegtopTrackBars.res

copy JPPegtopTrackBars.res %OutDir%



