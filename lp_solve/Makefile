CC= cc

#CFLAGS= -g -DCHECK -DREAL='long double'

#should be OK in most situations:
#CFLAGS= -O -DCHECK

# HP/UX 9.0X 
CFLAGS= +O3 +Oaggressive +Olibcalls -Aa -D_POSIX_SOURCE -DCHECK +FP VZOUiD

# nice for gcc
#CFLAGS= -O3 -Wall -pedantic -ansi

# option -DCHECK checks for numerical problems during rounding of numbers
# it will slow things down a bit.
# you can add a -DREAL=<float type> to the CFLAGS, to change the default float
# type used in lp_solve (double) to float or 'long double'. However, type float
# might be fast on your computer, but it is not accurate enough to solve even
# moderately sized problems without running into numerical problems.
# the use of long doubles does increase the numerical stability of lp_solve,
# if your compiler actually implements them with more bits than a double. But
# it slows down things quite a bit.

LPKSRC.c= lpkit.c solve.c debug.c read.c readmps.c
LEXFILE.l= lex.l
YACCFILE.y= lp.y
ALLFILES= lpkit.c solve.c debug.c read.c readmps.c lex.l lp.y ex1.lp ex2.lp ex3.lp ex4.lp ex5.lp ex6.lp ex7.lp demo_lag.lp lpglob.h lpkit.h patchlevel.h debug.h read.h lp_solve.c demo.c README Makefile
TESTFILES= lp_examples/ex1.lp lp_examples/ex2.lp lp_examples/ex3.lp lp_examples/ex4.lp lp_examples/ex5.lp lp_examples/ex6.lp lp_examples/ex7.lp

TARGET=lp_solve
LEXFILE.c= $(LEXFILE.l:.l=.c)
YACCFILE.c= $(YACCFILE.y:.y=.c)
YACCFILE.o= $(YACCFILE.y:.y=.o)
CSOURCES=lpkit.c solve.c debug.c read.c readmps.c lp_solve.c demo.c $(LEXFILE.c) $(YACCFILE.c)
COBJ=$(CSOURCES:.c=.o)
LPKSRC= $(LPKSRC.c) $(YACCFILE.c)
LPKOBJ= $(LPKSRC:.c=.o)
HEADERS=lpkit.h lpglob.h patchlevel.h debug.h read.h

all:	demo $(TARGET) lp2mps mps2lp

$(COBJ): $(HEADERS)

demo:	demo.o lpklib.a
	$(CC) -o demo demo.o lpklib.a -ll -lm

lp2mps:	lp2mps.o lpklib.a
	$(CC) -o lp2mps lp2mps.o lpklib.a -ll -lm

mps2lp: mps2lp.o lpklib.a
	$(CC) -o mps2lp mps2lp.o lpklib.a -ll -lm

$(TARGET): lp_solve.o lpklib.a
	$(CC) -o $(TARGET) lp_solve.o $(LPKOBJ) -ll -lm $(CFLAGS)

lpklib.a: $(LPKOBJ)
	ar rv $@ $(LPKOBJ)
	ranlib lpklib.a

$(YACCFILE.o): $(LEXFILE.c)

$(LEXFILE.c): $(LEXFILE.l)
	lex $(LEXFILE.l)
	mv lex.yy.c $(LEXFILE.c)

$(YACCFILE.c): $(YACCFILE.y)
	yacc $(YACCFILE.y)
	mv y.tab.c $(YACCFILE.c)

test:
	-for i in $(TESTFILES); do\
		./$(TARGET) -p -s < $$i > xxx.tmp;\
		if diff xxx.tmp lp_examples/`basename $$i .lp`.out > /dev/null; then\
			echo "$$i gives the correct result";\
		else\
			echo "$$i gives different result, please check";\
		fi;\
	done;\
	rm xxx.tmp

$(TARGET).man: $(TARGET).1
	nroff -man $(TARGET).1 > $(TARGET).man

MANIFEST: clean
	ls -lR > MANIFEST; ls -lR > MANIFEST

clean:
	rm -f *.a *.o  $(TARGET).man TAGS $(LEXFILE.c) $(YACCFILE.c) demo $(TARGET) lp2mps mps2lp

distrib: $(TARGET).man MANIFEST
	cd ..; tar -czvf lp_solve_2.0.tar.gz lp_solve_2.0

