#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#
#
# We don't assume that AllegroGL is installed so the default target doesn't
# build AllegroGL (or AllegroGL samples, if there were any).
# Run `make agl' for that.
#

MMC = mmc
MMC_MAKE = $(MMC) --make $(MMC_MAKE_FLAGS)
MMC_MAKE_FLAGS =

include Mercury.options
-include Make.params

.PHONY: default agl
default: $(MAIN_TARGET) examples samples

agl: default libmercury_allegrogl

.PHONY: libmercury_allegro libmercury_allegrogl
libmercury_allegro libmercury_allegrogl:
	$(MMC_MAKE) $@

.PHONY: examples
examples:
	@( cd $@ ; $(MMC_MAKE) )

SAMPLE_DIRS = \
	samples/demo \
	samples/mandel \
	samples/pendulum2 \
	samples/speed

samples: $(SAMPLE_DIRS)

.PHONY: $(SAMPLE_DIRS)
$(SAMPLE_DIRS):
	@( cd $@ ; $(MMC_MAKE) )

.PHONY: realclean
realclean:
	$(MMC_MAKE) mercury_allegro.realclean 2> /dev/null
	$(MMC_MAKE) mercury_allegrogl.realclean 2> /dev/null
	# XXX mmc --make --use-grade-subdirs doesn't yet remove .init files
	$(RM) mercury_allegro.init
	$(RM) mercury_allegrogl.init
	#
	(cd examples ; $(MMC_MAKE) exdata.realclean 2> /dev/null )
	(cd examples ; $(MMC_MAKE) exhello.realclean 2> /dev/null )
	(cd examples ; $(MMC_MAKE) exjoy.realclean 2> /dev/null )
	(cd examples ; $(MMC_MAKE) exmouse.realclean 2> /dev/null )
	(cd examples ; $(MMC_MAKE) exscroll.realclean 2> /dev/null )
	(cd examples ; $(MMC_MAKE) exspline.realclean 2> /dev/null )
	(cd examples ; $(MMC_MAKE) exsprite.realclean 2> /dev/null )
	(cd examples ; $(MMC_MAKE) extimer.realclean 2> /dev/null )
	(cd samples/demo ; $(MMC_MAKE) demo.realclean 2> /dev/null )
	(cd samples/mandel ; $(MMC_MAKE) mandel.realclean 2> /dev/null )
	(cd samples/pendulum2 ; $(MMC_MAKE) pendulum2.realclean 2> /dev/null )
	(cd samples/speed ; $(MMC_MAKE) speed.realclean 2> /dev/null )
