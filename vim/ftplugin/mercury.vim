" Vim syntax file
" Language:     Mercury
" Maintainer:   Ralph Becket <rafe@cs.mu.oz.au>
" vim: ts=2 sw=2 et

if exists("b:did_mercury_ftplugin")
  finish
endif
let b:did_mercury_ftplugin = 1

  " Miscellaneous settings.

  " I find it handy to run `mtags' over the Mercury library .m files
  " and move the resulting tags file to `$HOME/mercury/tags.library'.
  "
setlocal tags+=$HOME/mercury/tags.library,$HOME/mercury/tags.compiler

  " Handy if you use `:make'.
  "
setlocal makeprg=mmake

  " Don't wrap over-long lines.
  "
setlocal wrapmargin=0
setlocal textwidth=0

  " These settings allow for neater coding styles, but
  " should not be imposed on existing files that use,
  " say, the default `tabstop=8, shiftwidth=8, noexpandtab'.
  "
  " It is a good idea to have a modeline comment at the top
  " of your Mercury source files containing
  " ` vim: ft=mercury ff=unix ts=4 sw=4 et '
  "
" setlocal tabstop=8
" setlocal shiftwidth=8
" setlocal expandtab   

  " Controls how autoindenting works.  See the Vim help pages for details.
  "
setlocal formatoptions=trcq

  " <C-X>l inserts a comment line.
  "
nnoremap <C-X>l o0<C-D>%------------------------------------------------------------------------------%<CR><ESC>x
inoremap <C-X>l --------------------------------------------------------------------------------<ESC>80<BAR>C%<CR>

  " <F6> attempts to wrap a call up with { } braces for DCG escapes.
  "
nnoremap <F6> I{ <ESC>%a }<ESC>j

  " <F7> and <F8> comment and uncomment lines out respectively.
  "
nnoremap <F7> 0i% <ESC>j
nnoremap <F8> :s/% //e<CR>j

  " <C-X>h runs `$HOME/.vim/ftplugin/mercuryhdr.sh' which inserts all the
  " usual boilerplate for a new Mercury module.
  "
nnoremap <C-X>h !!$HOME/.vim/ftplugin/mercuryhdr.sh %<CR>:set ft=mercury ff=unix ts=4 sw=4 et<CR>

  " Go to the bottom window and rerun the last mmake command.
  " Reload any .err buffers that have changed.
  "
nnoremap ,m <C-W>b:!mmake<UP><CR>
autocmd! FileChangedShell *.err vi!
