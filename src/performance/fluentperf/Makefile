sources = fluentperf.lpr ansysfluent_v13.pas checkruns.pas floatstr.pas main.pas perf_common.pas

fluentperf: $(sources)
	fpc -MObjFPC -Scaghi -CX -CirotR -O2 -XX fluentperf.lpr

withcheckruns: $(sources)
	fpc -dCHECK_REMAINING_RUNS -MObjFPC -Scaghi -CX -CirotR -O2 -XX fluentperf.lpr

publish: fluentperf
	-mkdir -p ../../published/src/performance/fluentperf
	cp -r Makefile make.bat publish_src.bat publish_bin.bat main.lfm main.pas checkruns.pas perf_common.pas ansysfluent_v13.pas floatstr.pas fluentperf.lpi fluentperf.lpr fluentperf.res cgi-bin ../../published/src/performance/fluentperf
	-mkdir -p ../../published/bin/webserverhome/cgi-bin/performance/fluentperf
	cp -r fluentperf cgi-bin/* ../../published/bin/webserverhome/cgi-bin/performance/fluentperf

clean	:
	-rm fluentperf *.a *.o *.ppu
