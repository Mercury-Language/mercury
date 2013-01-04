# vim: ts=8 sw=8 noet

MCFLAGS=--runtime-flags "--heap-size $$((1*1024*1024))"
MC=mmc

TARGETS = \
	qs_always_par \
	qs_always_par_gc \
	qs_always_seq \
	qs_top_par_6

#	qs_always_test_allqueues_8 \
#	qs_always_test_allqueues_7 \
#	qs_always_test_allqueues_6 \
#	qs_always_test_allqueues_5 \
#	qs_always_test_allqueues_4 \
#	qs_always_test_allqueues_3 \
#	qs_always_test_globalqueues_8 \
#	qs_always_test_globalqueues_7 \
#	qs_always_test_globalqueues_6 \
#	qs_always_test_globalqueues_5 \
#	qs_always_test_globalqueues_4 \
#	qs_always_test_globalqueues_3 \
#	qs_top_par_6 \
#	qs_top_par_5 \
#	qs_top_par_4 \
#	qs_top_par_3 \
#	qs_top_par_2 \
#	qs_top_par_1

all: 			\
			$(TARGETS:%=asm_fast.gc/%) \
			$(TARGETS:%=asm_fast/%) \
			$(TARGETS:%=asm_fast.ctgc/%) \
			$(TARGETS:%=asm_fast.gc.par/%) \
			$(TARGETS:%=asm_fast.par/%) \
			$(TARGETS:%=asm_fast.ctgc.par/%)
#			$(TARGETS:%=asm_fast.gc.par.stseg/%) \
#			$(TARGETS:%=asm_fast.par.stseg/%) \
#			$(TARGETS:%=asm_fast.gc.par.stseg.threadscope/%)

asm_fast.gc/%:		%.m qs_utils.m asm_fast.gc
			-@rm -rf Mercury
			$(MC) $(MCFLAGS) --grade asm_fast.gc --make $*
			mv $* asm_fast.gc/$*
			mv Mercury asm_fast.gc/Mercury.$*

asm_fast/%:		%.m qs_utils.m asm_fast
			-@rm -rf Mercury
			$(MC) $(MCFLAGS) --grade asm_fast --make $*
			mv $* asm_fast/$*
			mv Mercury asm_fast/Mercury.$*

asm_fast.ctgc/%:	%.m qs_utils.m asm_fast.ctgc
			-@rm -rf Mercury
			$(MC) $(MCFLAGS) --grade asm_fast --ctgc --make $*
			mv $* asm_fast.ctgc/$*
			mv Mercury asm_fast.ctgc/Mercury.$*

asm_fast.gc.par/%:	%.m qs_utils.m asm_fast.gc.par
			-@rm -rf Mercury
			$(MC) $(MCFLAGS) --grade asm_fast.gc.par --make $*
			mv $* asm_fast.gc.par/$*
			mv Mercury asm_fast.gc.par/Mercury.$*

asm_fast.par/%:		%.m qs_utils.m asm_fast.par
			-@rm -rf Mercury
			$(MC) $(MCFLAGS) --grade asm_fast.par --make $*
			mv $* asm_fast.par/$*
			mv Mercury asm_fast.par/Mercury.$*

asm_fast.ctgc.par/%:	%.m qs_utils.m asm_fast.ctgc.par
			-@rm -rf Mercury
			$(MC) $(MCFLAGS) --grade asm_fast.par --ctgc --make $*
			mv $* asm_fast.ctgc.par/$*
			mv Mercury asm_fast.ctgc.par/Mercury.$*

asm_fast.gc.par.stseg/%:	%.m qs_utils.m asm_fast.gc.par.stseg
			-@rm -rf Mercury
			$(MC) $(MCFLAGS) --grade asm_fast.gc.par.stseg --make $*
			mv $* asm_fast.gc.par.stseg/$*
			mv Mercury asm_fast.gc.par.stseg/Mercury.$*

asm_fast.par.stseg/%:	%.m qs_utils.m asm_fast.par.stseg
			-@rm -rf Mercury
			$(MC) $(MCFLAGS) --grade asm_fast.par.stseg --ctgc --make $*
			mv $* asm_fast.par.stseg/$*
			mv Mercury asm_fast.par.stseg/Mercury.$*

asm_fast.gc.par.stseg.threadscope/%:	%.m qs_utils.m asm_fast.gc.par.stseg.threadscope
			-@rm -rf Mercury
			$(MC) $(MCFLAGS) --grade asm_fast.gc.par.stseg.threadscope --make $*
			mv $* asm_fast.gc.par.stseg.threadscope/$*
			mv Mercury asm_fast.gc.par.stseg.threadscope/Mercury.$*

asm_fast.par.threadscope/%:	%.m qs_utils.m asm_fast.par.threadscope
			-@rm -rf Mercury
			$(MC) $(MCFLAGS) --grade asm_fast.par.threadscope --make $*
			mv $* asm_fast.par.threadscope/$*
			mv Mercury asm_fast.par.threadscope/Mercury.$*

asm_fast.gc:
	mkdir asm_fast.gc

asm_fast:
	mkdir asm_fast

asm_fast.ctgc:
	mkdir asm_fast.ctgc

asm_fast.gc.par:
	mkdir asm_fast.gc.par

asm_fast.par:
	mkdir asm_fast.par

asm_fast.ctgc.par:
	mkdir asm_fast.ctgc.par

asm_fast.gc.par.stseg:
	mkdir asm_fast.gc.par.stseg

asm_fast.par.stseg:
	mkdir asm_fast.par.stseg

asm_fast.gc.par.stseg.threadscope:
	mkdir asm_fast.gc.par.stseg.threadscope

asm_fast.par.threadscope:
	mkdir asm_fast.par.threadscope


%_8.m : $(subst _8,,$@)
	sed -e 's/THRESHOLD/8/' < $(subst _8,,$@) > $@
%_7.m : $(subst _7,,$@)
	sed -e 's/THRESHOLD/7/' < $(subst _7,,$@) > $@
%_6.m : $(subst _6,,$@)
	sed -e 's/THRESHOLD/6/' < $(subst _6,,$@) > $@
%_5.m : $(subst _5,,$@)
	sed -e 's/THRESHOLD/5/' < $(subst _5,,$@) > $@
%_4.m : $(subst _4,,$@)
	sed -e 's/THRESHOLD/4/' < $(subst _4,,$@) > $@
%_3.m : $(subst _3,,$@)
	sed -e 's/THRESHOLD/3/' < $(subst _3,,$@) > $@
%_2.m : $(subst _2,,$@)
	sed -e 's/THRESHOLD/2/' < $(subst _2,,$@) > $@
%_1.m : $(subst _1,,$@)
	sed -e 's/THRESHOLD/1/' < $(subst _1,,$@) > $@

clean :
	rm -rf $(TARGETS) Mercury \
	asm_fast \
	asm_fast.ctgc \
	asm_fast.gc \
	asm_fast.gc.par \
	asm_fast.ctgc.par \
	asm_fast.gc.par.stseg \
	asm_fast.gc.par.stseg.threadscope \
	asm_fast.gc.stseg \
	asm_fast.gc.threadscope \
	asm_fast.par \
	asm_fast.par.stseg

