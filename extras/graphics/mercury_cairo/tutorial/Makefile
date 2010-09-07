include ../Cairo.options

MMC=mmc

PROGS=\
	fill			\
	mask			\
	paint			\
	path_close		\
	setsourcegradient	\
	setsourcergba		\
	showtext		\
	stroke			\
	textextents		\
	tips_ellipse		\
	tips_letter

.PHONY: all
all:
	$(MMC) --make $(PROGS)

.PHONY: realclean
realclean:
	for prog in $(PROGS); do \
		$(MMC) --make $$prog.realclean; \
	done
	/bin/rm -rf Mercury
