.PHONY:	all
all:	pic

pic:	*.hs
	ghc -O2 --make -o $@ Main

.PHONY:	clean
clean:
	rm -f *.o *.hi pic
