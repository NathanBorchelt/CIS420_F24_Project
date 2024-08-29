@ECHO OFF
cd %~p0%

REM Prepare target directory: remove and recreate
rd /s /q ..\..\Published\bin\webserverhome\cgi-bin\performance\fluentperf 2>NUL
mkdir ..\..\Published\bin\webserverhome\cgi-bin\performance\fluentperf 2>NUL

REM Copy supporting files
xcopy cgi-bin ..\..\Published\bin\webserverhome\cgi-bin\performance\fluentperf /e >NUL

REM Copy the executable
copy fluentperf.exe ..\..\Published\bin\webserverhome\cgi-bin\performance\fluentperf >NUL
