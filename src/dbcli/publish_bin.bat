@ECHO OFF
cd %~p0%

REM Prepare target directory: remove and recreate
rd /s /q ..\Published\bin\webserverhome\cgi-bin\dbcli 2>NUL
mkdir ..\Published\bin\webserverhome\cgi-bin\dbcli

for %%i in (dbcli.exe dbcli-cgi.py main-form.html) do copy %%i ..\Published\bin\webserverhome\cgi-bin\dbcli >NUL
