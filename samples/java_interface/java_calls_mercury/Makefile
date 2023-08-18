#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#
# This source file is hereby placed in the public domain.
#-----------------------------------------------------------------------------#

# We need to tell javac about the Mercury libraries.
GRADE = java
MER_LIB_DIR = $(dir $(shell which mmc))../lib/mercury/lib/$(GRADE)
MER_JARS = $(MER_LIB_DIR)/mer_std.jar:$(MER_LIB_DIR)/mer_rt.jar

.PHONY: all
all: mercury_main

mercury_main: mercury_main.m my_package/JavaMain.class
	mmc --grade $(GRADE) --make mercury_main --java-classpath .

my_package/JavaMain.class: JavaMain.java libmercury_lib.jar
	javac JavaMain.java -cp $(MER_JARS):Mercury/classes -d .

libmercury_lib.jar: mercury_lib.m
	mmc --grade $(GRADE) --make libmercury_lib

.PHONY: clean
clean:
	$(RM) mercury_main mercury_lib.jar *.err
	$(RM) -r Mercury my_package
