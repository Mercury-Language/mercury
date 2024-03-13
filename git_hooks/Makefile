#---------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#---------------------------------------------------------------------------#
#
# A makefile for the update_copyright program.
#
# This Makefile is designed for manual operation by Mercury developers;
# it is *not* designed to be invoked by any action in any other Makefile
# or Mmakefile, in any other part of that workspace.
#

update_copyright:   update_copyright.m
	mmc --mercury-linkage static update_copyright.m

clean:
	git clean -fx 'update_copyright.*' 'update_copyright_init.*'
