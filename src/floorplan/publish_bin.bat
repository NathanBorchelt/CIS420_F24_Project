@ECHO OFF
cd %~p0%

REM Prepare target directory: remove and recreate
rd /s /q ..\Published\bin\webserverhome\cgi-bin\floorplan 2>NUL
mkdir ..\Published\bin\webserverhome\cgi-bin\floorplan

REM Copy supporting files
xcopy cgi-bin ..\Published\bin\webserverhome\cgi-bin\floorplan /e >NUL
