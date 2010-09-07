include ../Cairo.options

MMC=mmc

PROGS=\
	arc			\
	arc_negative		\
	clip			\
	clip_image		\
	curve_to		\
	dash			\
	fill_and_stroke2	\
	fill_style		\
	gradient		\
	hello			\
	image			\
	imagepattern		\
	multi_segment_caps	\
	set_line_cap		\
	set_line_join		\
	text			\
	text_align_center	\
	text_extents

.PHONY: all
all:
	$(MMC) --make $(PROGS)

.PHONY: realclean
realclean:
	for prog in $(PROGS); do \
		$(MMC) --make $$prog.realclean; \
	done
	/bin/rm -rf Mercury
