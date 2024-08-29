@ECHO OFF
cd %~p0%

REM Prepare target directory: remove and recreate
rd /s /q ..\Published\bin\webserverhome\cgi-bin\network 2>NUL
mkdir ..\Published\bin\webserverhome\cgi-bin\network

REM Copy supporting files
xcopy cgi-bin ..\Published\bin\webserverhome\cgi-bin\network /e >NUL

REM Copy the executable
copy network.exe ..\Published\bin\webserverhome\cgi-bin\network >NUL
