@ECHO OFF
cd %~p0%

REM Only create target directory (do not remove it beforehand)
mkdir ..\Published\src\performance 2>NUL

REM Copy supporting files
for %%i in (Makefile make.bat publish_src.bat publish_bin.bat locator.py) do copy %%i ..\Published\src\performance\ >NUL
