@ECHO OFF
cd %~p0%

REM Prepare target directory: remove and recreate
rd /s /q ..\Published\src\network 2>NUL
mkdir ..\Published\src\network\cgi-bin

REM Copy all sources
for %%i in (Makefile make.bat publish_src.bat publish_bin.bat cgimain.lfm cgimain.pas checkruns.pas common.pas database.pas fattree.pas floatstr.pas torus.pas network.res network.lpi network.lpr) do copy %%i ..\Published\src\network\ >NUL

REM Copy files for the web server
xcopy cgi-bin ..\Published\src\network\cgi-bin /e >NUL
