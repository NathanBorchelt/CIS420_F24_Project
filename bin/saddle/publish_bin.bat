@ECHO OFF
cd %~p0%

REM Prepare target directory: remove and recreate
rd /s /q ..\Published\bin\saddle 2>NUL
mkdir ..\Published\bin\saddle

for %%i in (Makefile make.bat make_clean.bat publish_src.bat publish_bin.bat bom.py cabling.py callmod.py colours.py common.py conflist.py database.py dbcliquery.py eqgroups.py evaluate.py example.py floorplan.py multipart.py parsetab.py racks.py saddle.bat saddle.py strconst.py svgoutput.py version.py README.txt pyparsing.py) do copy %%i ..\Published\bin\saddle >NUL

CALL make_clean.bat

REM Copy the database
xcopy db ..\Published\bin\saddle\db /i >NUL

REM Copy auxiliary files of libraries
xcopy ply ..\Published\bin\saddle\ply /i /e >NUL
xcopy svgwrite ..\Published\bin\saddle\svgwrite /i /e >NUL
