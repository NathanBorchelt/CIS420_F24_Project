clusterdesign_sources = clusterdesign.lpr common.pas config.pas graph.pas evaluate.pas floatstr.pas \
  visualcontrols.pas clusterconfig.pas undo.pas

clusterdesign : $(clusterdesign_sources)
	mkdir -p lib/`fpc -iSP`-`fpc -iSO`
	cp fpc_unix.cfg fpc.cfg
	fpc -MObjFPC -Scaghi -CX -CirotR -O2 -XX -Xs -dLCL -dLCLgtk2 clusterdesign.lpr
  
publish	: clusterdesign
	-mkdir -p ../published/src/clusterdesign
	cp -r Makefile make.bat publish_src.bat publish_bin.bat fpc_windows.cfg fpc_unix.cfg mainunit.lfm mainunit.pas common.pas config.pas clusterconfig.pas evaluate.pas floatstr.pas graph.pas undo.pas visualcontrols.pas clusterdesign.res clusterdesign.ico clusterdesign.lpi clusterdesign.lpr amd.xml hp.xml hp-bl465c_g7.xml hp-blade-memory.xml hp-network.xml ../published/src/clusterdesign
	-mkdir -p ../published/bin/clusterdesign
	cp clusterdesign clusterdesign.ico amd.xml hp.xml hp-bl465c_g7.xml hp-blade-memory.xml hp-network.xml ../published/bin/clusterdesign

clean	:
	-rm clusterdesign *.a *.o *.ppu >/dev/null 2>&1
	-rm -r lib >/dev/null 2>&1
