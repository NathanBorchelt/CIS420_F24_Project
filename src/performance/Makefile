publish	:
	make -C fluentperf publish
	-mkdir -p ../published/src/performance
	cp -r Makefile make.bat publish_src.bat publish_bin.bat locator.py ../published/src/performance
	-mkdir -p ../published/bin/webserverhome/cgi-bin/performance
	cp locator.py ../published/bin/webserverhome/cgi-bin/performance
