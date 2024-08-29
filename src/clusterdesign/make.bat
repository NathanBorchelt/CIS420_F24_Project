@ECHO OFF
cd %~p0%

CALL ..\setpath.bat

REM Create a directory to store object files
MKDIR lib\i386-win32 2>NUL

REM Use a compiler configuration file
COPY fpc_windows.cfg fpc.cfg >NUL

fpc -MObjFPC -Scaghi -CX -CirotR -O2 -XX -Xs -WG -dLCL -dLCLwin32 clusterdesign.lpr
