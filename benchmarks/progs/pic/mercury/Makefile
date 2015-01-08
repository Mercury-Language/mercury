#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#

MC=mmc
MCFLAGS=-O2 --intermodule-optimisation

.PHONY:	all
all:	pic.seq pic.seq_stseg pic.profdeep pic.feedback \
	pic.par pic.par_stseg

pic.profdeep:   *.m
	-rm -rf pic pic.profdeep Mercury
	$(MC) $(MCFLAGS) --profile-for-implicit-parallelism --grade asm_fast.gc.profdeep --make pic
	mv pic pic.profdeep

# Normal build.
pic.seq:	*.m
	-rm -rf pic pic.seq Mercury
	$(MC) $(MCFLAGS) --grade asm_fast.gc --make -o $@ pic
	mv pic pic.seq

pic.seq_stseg:	*.m
	-rm -rf pic pic.seq_stseg Mercury
	$(MC) $(MCFLAGS) --grade asm_fast.gc.stseg --make -o $@ pic
	mv pic pic.seq_stseg

pic.par:	*.m
	-rm -rf pic pic.par Mercury
	$(MC) $(MCFLAGS) --grade asm_fast.gc.par --feedback-file pic.feedback \
		--implicit-parallelism --make pic
	mv pic pic.par

pic.par_stseg:	*.m
	-rm -rf pic pic.par_stseg Mercury
	$(MC) $(MCFLAGS) --grade asm_fast.gc.par.stseg --feedback-file pic.feedback \
		--implicit-parallelism --make pic
	mv pic pic.par_stseg

# Run analysis.

Deep.data: pic.profdeep
	-rm -f Deep.data Deep.procrep
	MERCURY_OPTIONS="--deep-procrep-file" ./pic.profdeep 10000

pic.feedback:	Deep.data
	-rm -f pic.feedback
	mdprof_feedback --candidate-parallel-conjunctions \
		--desired-parallelism 8 \
		--implicit-parallelism-clique-cost-threshold 2000 \
		--implicit-parallelism-call-site-cost-threshold 2000 \
		--implicit-parallelism-dependant-conjunctions \
		--implicit-parallelism-intermodule-var-use \
		Deep.data pic.feedback

.PHONY:	clean
clean:
	rm -fr Mercury *.mh *.err
	rm -f pic pic.profdeep pic.seq pic.seq_stseg \
     pic.feedback pic.par pic.par_stseg
