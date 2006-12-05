.PHONY: runtests
runtests: stream_util_test.runtest

.PHONY: stream_util_test.runtest
stream_util_test.runtest: stream_util_test.m Mercury.modules
	@mmc --make stream_util_test
	@./stream_util_test > stream_util_test.out
	@if diff stream_util_test.out stream_util_test.exp; then 	\
		echo PASSED TEST $@;					\
	else								\
		echo FAILED TEST $@;					\
	fi

Mercury.modules:
	mmc -f ../*.m *.m

realclean:
	-rm *.out
	mmc --make stream_util_test.realclean
	
