
all: dvi info html

install: install_html install_info install_dvi

install_html: html
	cp *.html $$HOME/www_public/mercury
install_info: info
	cp *.info* /usr/contrib/info
install_dvi: dvi
	cp *.dvi /usr/contrib/lib/mercury/doc

dvi: user_guide.dvi
info: mercury.info
html: user_guide_toc.html

user_guide.dvi: user_guide.texi
	texi2dvi user_guide.texi

mercury.info: user_guide.texi
	makeinfo user_guide.texi

user_guide_toc.html: user_guide.texi
	texi2html -split_node user_guide.texi

.PHONY: all install install_html install_info install_dvi dvi info html clean

clean:
	rm -f *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr

realclean: clean
	rm -f *.info* *.html *.dvi
