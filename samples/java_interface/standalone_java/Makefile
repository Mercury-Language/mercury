#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#
# This source file is hereby placed in the public domain.
#-----------------------------------------------------------------------------#

MMC = mmc
JAVAC = javac
JAVA = java

# We need to tell javac and java where to find the Mercury runtime and
# standard libraries.
GRADE = java
MER_LIB_DIR = $(dir $(shell which mmc))../lib/mercury/lib/$(GRADE)
MER_JARS = $(MER_LIB_DIR)/mer_std.jar:$(MER_LIB_DIR)/mer_rt.jar

.PHONY: all
all: run

JavaMain.class: JavaMain.java mercury_lib.jar
	$(JAVAC) JavaMain.java -cp $(MER_JARS):Mercury/classes -d .

mercury_lib.jar: mercury_lib.m
	$(MMC) --grade $(GRADE) --make libmercury_lib

.PHONY: run
run: JavaMain.class
	$(JAVA) -cp $(MER_JARS):./mercury_lib.jar:. JavaMain

.PHONY: clean
clean:
	$(MMC) --make mercury_lib.realclean
	$(RM) -f mercury_lib.jar JavaMain.class
	$(RM) -rf Mercury

.PHONY: realclean
realclean: clean
