network	: network.lpr cgimain.pas common.pas database.pas \
	checkruns.pas floatstr.pas fattree.pas torus.pas
	fpc -MObjFPC -Scaghi -CX -CirotR -O2 -XX network.lpr

# When you want to build your executable with support for
# anti-abuse measure, you need to define "CHECK_REMAINING_RUNS"
# when calling the compiler, and you also need to recompile
# all source files because they depend on this conditional
# define. The easiest way is to clean up everything, and
# then recompile the whole project:

withcheckruns: clean
	fpc -dCHECK_REMAINING_RUNS -MObjFPC -Scaghi -CX -CirotR -O2 -XX network.lpr

publish	: network
	-mkdir -p ../published/src/network
	cp -r Makefile make.bat publish_src.bat publish_bin.bat cgimain.lfm cgimain.pas checkruns.pas common.pas database.pas fattree.pas floatstr.pas torus.pas network.res network.lpi network.lpr cgi-bin ../published/src/network
	-mkdir -p ../published/bin/webserverhome/cgi-bin/network
	cp -r network cgi-bin/* ../published/bin/webserverhome/cgi-bin/network

clean	:
	-rm network *.a *.o *.ppu
