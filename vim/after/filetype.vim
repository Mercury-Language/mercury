" filetype.vim
" vim: ts=2 sw=2 et

augroup filetypedetect
  au! BufRead,BufNewFile  *.m,*.moo         setfiletype mercury
augroup END
