@ECHO OFF
cd %~p0%

REM Prepare target directory: remove and recreate
rd /s /q ..\Published\src\webserverhome 2>NUL
mkdir ..\Published\src\webserverhome

REM Copy all sources
for %%i in (Makefile make.bat publish_src.bat publish_bin.bat cgiwebserver.py favicon.ico index-linux.html index-win.html start.html startwebserver.bat startwebserver.sh) do copy %%i ..\Published\src\webserverhome\ >NUL
