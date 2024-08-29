dbcli_sources = dbcli.lpr

dbcli	: $(dbcli_sources)
	(cd ../clusterdesign ; cp common.pas config.pas evaluate.pas floatstr.pas graph.pas ../dbcli) || (echo "Need source files from sister project, 'clusterdesign'. Exiting."; exit 1)
	fpc -dAppTypeConsole -MObjFPC -Scaghi -CX -CirotR -O2 -XX -Xs dbcli.lpr

publish	: dbcli
	-mkdir -p ../published/src/dbcli
	cp Makefile make.bat publish_src.bat publish_bin.bat dbcli.ico dbcli.lpi dbcli.lpr dbcli.res dbcli-cgi.py main-form.html ../published/src/dbcli
	-mkdir -p ../published/bin/webserverhome/cgi-bin/dbcli
	cp dbcli dbcli-cgi.py main-form.html ../published/bin/webserverhome/cgi-bin/dbcli

clean	:
	-rm dbcli *.a *.o *.ppu >/dev/null 2>&1
