@ECHO OFF
cd %~p0%
SET CURDIR=%~p0%

FOR %%i in (fluentperf) DO CALL %%i/make.bat & cd %CURDIR% & CALL %%i/publish_src.bat & cd %CURDIR% & CALL %%i/publish_bin.bat
