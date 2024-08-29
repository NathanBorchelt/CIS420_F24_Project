@ECHO OFF
cd %~p0%
SET CURDIR=%~p0%

CALL ..\setpath.bat

IF NOT EXIST ..\clusterdesign GOTO not_exist

REM Copy source files from the sister project, "clusterdesign"
CD ..\clusterdesign
for %%i in (common.pas config.pas evaluate.pas floatstr.pas graph.pas) do copy %%i ..\dbcli >NUL
cd %CURDIR%

fpc -dAppTypeConsole -MObjFPC -Scaghi -CX -CirotR -O2 -XX -Xs dbcli.lpr
GOTO end

:not_exist
ECHO.
ECHO "dbcli" shares some source files with the "clusterdesign" application, but
ECHO the "clusterdesign" directory was not found in the parent directory. Exiting.
ECHO.
EXIT /B 1

:end
