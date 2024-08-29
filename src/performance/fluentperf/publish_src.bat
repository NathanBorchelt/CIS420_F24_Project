@ECHO OFF
cd %~p0%

REM Prepare target directory: remove and recreate
rd /s /q ..\..\Published\src\performance\fluentperf 2>NUL
mkdir ..\..\Published\src\performance\fluentperf\cgi-bin 2>NUL

REM Copy all sources
for %%i in (Makefile make.bat publish_src.bat publish_bin.bat main.lfm main.pas checkruns.pas perf_common.pas ansysfluent_v13.pas floatstr.pas fluentperf.lpi fluentperf.lpr fluentperf.res) do copy %%i ..\..\Published\src\performance\fluentperf\ >NUL

REM Copy files for the web server
xcopy cgi-bin ..\..\Published\src\performance\fluentperf\cgi-bin /e >NUL
