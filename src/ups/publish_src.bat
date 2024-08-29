@ECHO OFF
cd %~p0%

REM Prepare target directory: remove and recreate
rd /s /q ..\Published\src\ups 2>NUL
mkdir ..\Published\src\ups\cgi-bin

REM Copy all sources
for %%i in (Makefile make.bat publish_src.bat publish_bin.bat) do copy %%i ..\Published\src\ups\ >NUL

REM Copy files for the web server
xcopy cgi-bin ..\Published\src\ups\cgi-bin /e >NUL
