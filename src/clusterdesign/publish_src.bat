@ECHO OFF
cd %~p0%

REM Prepare target directory: remove and recreate
rd /s /q ..\Published\src\clusterdesign 2>NUL
mkdir ..\Published\src\clusterdesign
mkdir ..\Published\src\clusterdesign\synapse-lib

REM Copy all sources
for %%i in (Makefile make.bat publish_src.bat publish_bin.bat fpc_windows.cfg fpc_unix.cfg mainunit.lfm mainunit.pas common.pas config.pas clusterconfig.pas evaluate.pas floatstr.pas graph.pas undo.pas visualcontrols.pas clusterdesign.res clusterdesign.ico clusterdesign.lpi clusterdesign.lpr amd.xml hp.xml hp-bl465c_g7.xml hp-blade-memory.xml hp-network.xml) do copy %%i ..\Published\src\clusterdesign\ >NUL

REM Copy the Synapse library source files
xcopy synapse-lib ..\Published\src\clusterdesign\synapse-lib /e >NUL
