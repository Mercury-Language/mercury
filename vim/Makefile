# vim: ft=make noet
FILES := doc/mercury.txt
FILES += ftdetect/mercury.vim
FILES += ftplugin/mercury.vim
FILES += ftplugin/mercuryhdr.sh
FILES += syntax/mercury.vim

WC := wc
SED := sed
DATE := date
VIM := vim
RM := /bin/rm
COUNT_LINES := $(WC) -l

# NOTE: please do not modify the first line, since that is essential to
# the Vimball plugin
# Also note the tab character between $(FILE) and [[[1, that seems to be 
# necessary as well
mercury.vba: $(FILES)
	@echo \" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.>$@
	@echo UseVimball>>$@
	@echo finish>>$@
	@$(foreach FILE,$(FILES),  \
		echo "$(FILE)	[[[1">>$@; \
		$(COUNT_LINES) < $(FILE) | $(SED) "s/\s/\n/" >>$@; \
		cat $(FILE) >>$@; )

.PHONY: install
install: mercury.vba
	$(VIM) -c 'so %' -c 'q' $<

.PHONY: clean
clean:
	$(RM) mercury.vba
