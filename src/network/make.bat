@ECHO OFF
cd %~p0%

CALL ..\setpath.bat
fpc -MObjFPC -Scaghi -CX -CirotR -O2 -XX network.lpr
