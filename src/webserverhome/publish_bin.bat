@ECHO OFF
cd %~p0%

REM Prepare target directory: remove and recreate
rd /s /q ..\Published\bin\webserverhome 2>NUL
mkdir ..\Published\bin\webserverhome

REM Copy all sources
for %%i in (cgiwebserver.py favicon.ico index-linux.html index-win.html start.html startwebserver.bat startwebserver.sh) do copy %%i ..\Published\bin\webserverhome\ >NUL
