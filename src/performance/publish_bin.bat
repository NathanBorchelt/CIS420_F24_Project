@ECHO OFF
cd %~p0%

REM Only create target directory (do not remove it beforehand)
mkdir ..\Published\bin\webserverhome\cgi-bin\performance 2>NUL

REM Copy "locator.py"
copy locator.py ..\Published\bin\webserverhome\cgi-bin\performance >NUL
