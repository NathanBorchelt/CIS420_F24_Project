@echo off

REM     "StartWebServer.bat" - Web server start-up script for the MS Windows environment
REM 
REM     Written in 2012 by Konstantin S. Solnushkin (http://clusterdesign.org)
REM 
REM     To the extent possible under law, the author has dedicated all copyright and related and
REM     neighboring rights to this software to the public domain worldwide. This software is distributed
REM     without any warranty. 
REM 
REM     For the complete text of the "CC0 Public Domain Dedication",
REM     see <http://creativecommons.org/publicdomain/zero/1.0/>. 


REM Require the following Python version:

set PYTHON_REQ_VER=3.4.2
set WEB_SERVER_SCRIPT=cgiwebserver.py
set PYTHON=python

REM The working directory is the path where this batch file is located:

set WORK_DIR=%~p0%

echo Starting CGI Web server...

REM Check if Python is installed anywhere on PATH:

%PYTHON% --version >NUL 2>&1

if %ERRORLEVEL% equ 0 goto PythonAvailable

REM If we reached here, then Python was not available. Inform the user and exit.

echo.
echo Python was not found either on PATH or in %PYTHON%
echo Please install Python ver. %PYTHON_REQ_VER% or greater. Thanks!
pause
exit /b 2

REM Python is available, the executable is in %PYTHON%

:PythonAvailable

REM Change the directory first

cd /d %WORK_DIR%

REM Finally, we can launch the Python CGI web server

if exist %WEB_SERVER_SCRIPT% goto LaunchWebServer

REM If we reached here, then the Python script to launch the web server was not found

echo.
echo The script "%WEB_SERVER_SCRIPT%" was not found in the same directory as this batch file, "%WORK_DIR%".
echo Make sure you downloaded the whole set of scripts. Thanks!
echo.
exit /b 3

:LaunchWebServer

%PYTHON% %WEB_SERVER_SCRIPT%
