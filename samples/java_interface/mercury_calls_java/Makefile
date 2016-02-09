#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#
# This source file is hereby placed in the public domain.
#-----------------------------------------------------------------------------#

.PHONY: all
all: mercury_main

mercury_main: mercury_main.m my_package/JavaMain.class
	mmc --grade java --make mercury_main --java-classpath .

my_package/JavaMain.class: JavaMain.java
	javac JavaMain.java -d .

.PHONY: clean
clean:
	$(RM) mercury_main *.err
	$(RM) -r Mercury my_package
