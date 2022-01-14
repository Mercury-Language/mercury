#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#
# This source file is hereby placed in the public domain.
#-----------------------------------------------------------------------------#

MMC = mmc
CSC = mcs 
CLI = mono

# We need to tell C# and CLI where to find about the Mercury standard
# library.
GRADE = csharp
MER_LIB_DIR = $(dir $(shell which mmc))../lib/mercury/lib/$(GRADE)

.PHONY: all
all: run

CSharpMain.exe: CSharpMain.cs mercury_lib.dll
	$(CSC) -lib:$(MER_LIB_DIR),. -r:mercury_lib.dll -r:mer_std.dll CSharpMain.cs

mercury_lib.dll: mercury_lib.m
	$(MMC) --grade $(GRADE) --make libmercury_lib

# The Microsoft .NET implementation does not have an equivalent of
# MONO_PATH; you will need to copy mer_std.dll into this directory
# in order to run the example.
.PHONY: run
run: CSharpMain.exe
	MONO_PATH=$(MER_LIB_DIR) $(CLI) CSharpMain.exe

.PHONY: clean
clean:
	$(MMC) --make mercury_lib.realclean
	$(RM) mercury_lib.dll CSharpMain.exe
	$(RM) -r Mercury

.PHONY: realclean
realclean: clean
