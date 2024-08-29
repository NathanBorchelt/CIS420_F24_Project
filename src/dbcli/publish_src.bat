@ECHO OFF
cd %~p0%

REM Prepare target directory: remove and recreate
rd /s /q ..\Published\src\dbcli 2>NUL
mkdir ..\Published\src\dbcli

for %%i in (Makefile make.bat publish_src.bat publish_bin.bat dbcli.ico dbcli.lpi dbcli.lpr dbcli.res dbcli-cgi.py main-form.html) do copy %%i ..\Published\src\dbcli\ >NUL
