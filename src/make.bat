@ECHO OFF

REM Change to the directory where the batch file resides
cd %~p0%
SET MAINDIR=%~p0%

REM We will publish the source code of all software in this directory, which we hereby create
MKDIR published\src 2>NUL

REM Copy essential files
FOR %%i in (make.bat setpath.bat README.txt Changelog.txt set_perm.sh unix.sh version.txt) DO COPY %%i published\src >NUL

REM Call scripts in all subdirectories
FOR %%i in (clusterdesign webserverhome dbcli saddle floorplan network performance ups) DO cd %MAINDIR% & CALL %%i\make.bat & cd %MAINDIR% & CALL %%i\publish_src.bat & cd %MAINDIR% & CALL %%i\publish_bin.bat

REM Get back to where we started
cd %MAINDIR%
