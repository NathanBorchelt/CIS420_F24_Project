@ECHO OFF
cd %~p0%

REM Prepare target directory: remove and recreate
rd /s /q ..\Published\bin\clusterdesign 2>NUL
mkdir ..\Published\bin\clusterdesign

for %%i in (clusterdesign.exe clusterdesign.ico amd.xml hp.xml hp-bl465c_g7.xml hp-blade-memory.xml hp-network.xml) do copy %%i ..\Published\bin\clusterdesign >NUL
