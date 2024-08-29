@ECHO OFF
cd %~p0%
SET CURDIR=%~p0%

REM Prepare target directory: remove and recreate
rd /s /q ..\Published\bin\webserverhome\cgi-bin\ups 2>NUL
mkdir ..\Published\bin\webserverhome\cgi-bin\ups

REM Copy supporting files
xcopy cgi-bin ..\Published\bin\webserverhome\cgi-bin\ups /e >NUL
