DEMOS = mdprof_cgid hello
MMC = mmc

# The following target builds the demos and links them against the posix
# library in the parent directory in situ.
#
all:
	$(MMC) --search-lib-files-dir .. \
		--init-file ../posix.init \
		--link-object ../libposix.a \
		--make $(DEMOS)

.PHONY: realclean
realclean:
	$(MMC) --make $(DEMOS:%=%.realclean)
	/bin/rm -rf Mercury
