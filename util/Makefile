CC	= gcc
CFLAGS	=

mkinit:	mkinit.o
	gcc $(CFLAGS) -o mkinit mkinit.o

.PHONY: install
install: mkinit
	[ -d $(INSTALL_BINDIR) ] || mkdir -p $(INSTALL_BINDIR)
	cp mkinit $(INSTALL_BINDIR)

clean:
	-rm mkinit.o

realclean:
	-rm mkinit.o mkinit
