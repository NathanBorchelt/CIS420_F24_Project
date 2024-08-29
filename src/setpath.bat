@ECHO OFF
SET FPC_EXE_PATH="C:\lazarus\fpc\2.6.4\bin\i386-win32"
SET FPC_EXE="fpc.exe"
REM ECHO.
REM ECHO Using FPC.EXE path: %FPC_EXE_PATH%
REM ECHO.

REM Try to launch the compiler, send all output to NUL
%FPC_EXE% -h >NUL 2>&1
REM Everything fine, it's already on the PATH
IF %ERRORLEVEL% EQU 0 GOTO end

REM If we reached here, the executable was not readily found on the PATH.
REM Try to check manually if it exists.
IF NOT EXIST %FPC_EXE_PATH%\%FPC_EXE% GOTO not_exist
REM Exists, update the PATH and exit.
PATH=%PATH%;%FPC_EXE_PATH%
GOTO end

:not_exist
ECHO.
ECHO Message from SETPATH.BAT:
ECHO Free Pascal compiler executable not found:
ECHO File %FPC_EXE_PATH%\%FPC_EXE% does not exist.
ECHO.
EXIT /B 1

:end
