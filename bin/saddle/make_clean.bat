@ECHO OFF

REM Remove "__pycache__" folders
rmdir /s /q "__pycache__" 2>NUL
rmdir /s /q "ply/__pycache__" 2>NUL
rmdir /s /q "svgwrite/__pycache__" 2>NUL
rmdir /s /q "svgwrite/data/__pycache__" 2>NUL
