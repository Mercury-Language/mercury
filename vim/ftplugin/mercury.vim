" Vim syntax file
" Language:     Mercury
" Maintainer:   Sebastian Godelet <sebastian.godelet@outlook.com>
" Last Change:  2015-04-16
" vim: ts=2 sw=2 et

" for documentation, please use :help mercury-ftplugin

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
setlocal makeprg=mmc\ --make

  " Don't wrap over-long lines.
  "
setlocal wrapmargin=0

  " if you want to follow the Mercury coding standard of 78 wide columns,
  " enable following option in your ~/.vimrc:
  "
  "     let g:mercury_coding_standard = 1
  "
if exists("mercury_coding_standard") && mercury_coding_standard
  setlocal textwidth=78
  if v:version >= 703
    setlocal colorcolumn=+1
      " Controls how auto-indenting works.  See the Vim help pages for details.
    setlocal fo-=t " do not automatically wrap text
    setlocal fo+=c " auto wrap comments, using comment leader
    setlocal fo+=r " automatically insert the comment leader after <CR>
    setlocal fo+=q " enable tq feature
  endif
  setlocal tabstop=4
  setlocal shiftwidth=4
  setlocal expandtab
endif

  " key bindings

  " <C-X>l inserts a comment line.
  "
nnoremap <buffer> <silent> <C-X>l o0<C-D>%---------------------------------------------------------------------------%<CR><ESC>x

if exists("mercury_highlight_extra") && mercury_highlight_extra
  inoremap <buffer> <silent> <C-K>r <ESC> :call <SID>RenameCurrentVariable(1)<CR>
  nnoremap <buffer> <silent> <C-K>r :call <SID>RenameCurrentVariable(1)<CR>

  inoremap <buffer> <silent> <C-K>R <ESC> :call <SID>RenameCurrentVariable(0)<CR>
  nnoremap <buffer> <silent> <C-K>R :call <SID>RenameCurrentVariable(0)<CR>

  inoremap <buffer> <silent> <C-K>m <ESC> :call <SID>MarkCurrentPred()<CR>
  nnoremap <buffer> <silent> <C-K>m :call <SID>MarkCurrentPred()<CR>

    " Match all the occurrences of the variable under the cursor
    "
  augroup mercuryMatchVar
    autocmd! CursorMoved,CursorMovedI,WinEnter <buffer> call s:HighlightMatchingVariables()
  augroup END
endif

  " <C-X>h runs `$HOME/.vim/ftplugin/mercuryhdr.sh' which inserts all the
  " usual boilerplate for a new Mercury module.
  "
if has("win32")
  nnoremap <buffer> <silent> <C-X>h !!sh "\%userprofile\%\\.vim\\ftplugin\\mercuryhdr.sh" %<CR>:set ft=mercury ff=unix ts=4 sw=4 et<CR>
else
  nnoremap <buffer> <silent> <C-X>h !!$HOME/.vim/ftplugin/mercuryhdr.sh %<CR>:set ft=mercury ff=unix ts=4 sw=4 et<CR>
endif

  " Go to the bottom window and rerun the last mmake command.
  " Reload any .err buffers that have changed.
  "
nnoremap <buffer> ,m <C-W>b:!make<UP><CR>
autocmd! FileChangedShell *.err vi!

nnoremap <buffer> <silent> ,p <ESC>O:- pred<space>
nnoremap <buffer> <silent> ,f <ESC>O:- func<space>
nnoremap <buffer> <silent> ,t <ESC>O:- type<space>
nnoremap <buffer> <silent> ,i <ESC>O:- import_module<space>
nnoremap <buffer> <silent> ,P <ESC>O:- pragma foreign_proc("C",<CR><TAB>(_IO0::di, _IO::uo),<CR>[will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],<CR><BS>"<CR><CR><BS>").<ESC>kkkei
nnoremap <buffer> <silent> ,M <ESC>O:- mutable(VAR, TYPE, VALUE, ground, [untrailed, attach_to_io_state]).<ESC>2FVcw
nnoremap <buffer> <silent> ,T <ESC>Otrace [io(!IO)] (<CR>),<ESC>^O<TAB>

nnoremap <buffer> <silent> <C-X>i m':1/^:- interface<CR>jj
nnoremap <buffer> <silent> <C-X>I m':1/^:- implementation<CR>jj

inoremap <buffer> <silent> <F3> ::in,<space>
inoremap <buffer> <silent> <F4> ::out,<space>
inoremap <buffer> <silent> <F5> io::di, io::uo)<space>
inoremap <buffer> <silent> <F6> is det.

  " In a previous version of the ftplugin, there where these keybindings
  " defined which are presumably not used that often.

  " <F6> attempts to wrap a call up with { } braces for DCG escapes.
  "
" nnoremap <F6> I{ <ESC>%a }<ESC>j

  " <F7> and <F8> comment and uncomment lines out respectively.
  "
  " It might be better to use (and adjust) a common script for this task
  " http://www.vim.org/scripts/script.php?script_id=2844
  " TODO: Provide a copy and optionally bundle within vim/Makefile
  "
" nnoremap <F7> 0i% <ESC>j
" nnoremap <F8> :s/% //e<CR>j

fu! s:HighlightMatchingVariables()
    " Avoid that we remove the popup menu.
    " Return when there are no colors (looks like the cursor jumps).
  if pumvisible() || (&t_Co < 8 && !has("gui_running"))
    return
  endif

    " Get variable under cursor, or "" if empty
  let l:variable = s:GetCurrentCursorVariable()

  let l:lineL1PredStart = search("^[:a-z']", 'nWb')
  let l:posEnd = searchpos('\v[.]($|\s+)', 'nW')

    " We do not want to match a dot within strings or lists, etc.
  while s:CurrentSynIsRegion(l:posEnd) > 0
    let l:posEnd = searchpos('\v[.]($|\s+)%>' . l:posEnd[0] . 'l', 'nW')
  endwhile

  let l:lineL1PredEnd = l:posEnd[0]
  if l:lineL1PredStart <= 0|let l:lineL1PredStart = line('w0')|endif
  if l:lineL1PredEnd   <= 0|let l:lineL1PredEnd   = line('w$')|endif

    " If we are still on the same spot, then abort
  if exists('w:variable') && l:variable == w:variable &&
        \ l:lineL1PredStart == w:lineL1PredStart &&
        \ l:lineL1PredEnd   == w:lineL1PredEnd
    return
  elseif exists('w:variable_hl_on') && w:variable_hl_on
      " Remove any previous match.
    2match none
    let w:variable_hl_on = 0
  endif

  let w:variable         = l:variable
  let w:lineL1PredStart  = l:lineL1PredStart
  let w:lineL1PredEnd    = l:lineL1PredEnd
  let w:lineL1PredEndCol = l:posEnd[1]

    " Abort if there is no variable under the cursor
  if l:variable == ""|return|endif

    " Underline all Variable occurances in the current (outer)
    " predicate/function/pragma scope
  let l:variableMatch = s:CreateVariableMatch(l:variable,
        \ l:lineL1PredStart, l:lineL1PredEnd)
  exe '2match Underlined ' . l:variableMatch
  let w:variable_hl_on = 1
endfu

fu! s:CreateVariableMatch(variable, start, end)
  return '/\v<' . escape(a:variable, '\') . '>' .
        \ '%>' . (a:start - 1) . 'l' .
        \ '%<' . (a:end   + 1) . 'l' . '/'
endfu

fu! s:CurrentSynIsRegion(pos)
  if a:pos[0] == 0|return 0|endif
  let l:id = synID(a:pos[0], a:pos[1], 0)
    " TODO: Also check for mercuryString
  return l:id == synIDtrans(l:id) ? 1 : 0
endfu

fu! s:GetCurrentCursorVariable()
    " returns the variable under the cursor
  let l:word = expand("<cword>")
  if empty(matchstr(l:word, '\v^[A-Z]\c')) || l:word[0] ==# tolower(l:word[0])
    return ""
  else
    return l:word
  endif
endfu

fu! s:RenameCurrentVariable(keepName)
  if !empty(w:variable)
    call inputsave()
    if a:keepName
      let l:new = input('Enter a new variable name: ', w:variable)
    else
      let l:new = input('Enter a new variable name: ')
    endif
    call inputrestore()
      " Allow for a silent return if the input is empty (most likely it means
      " the user pressed ESC, or if input is unchanged
    if empty(l:new) || l:new ==# w:variable
      return
    endif
      " using the predicate range as a boundary for a global %s
    let l:variableMatch = s:CreateVariableMatch(w:variable,
          \ w:lineL1PredStart, w:lineL1PredEnd)
    exe '%s' . l:variableMatch . escape(l:new, '\') . '/g'
  else
    echoerr 'Nothing selected to be renamed!'
  endif
endfu

  " One can use the following function to visually select the current
  " code block (e.g. predicate definition, declaration, pragma, etc.), which
  " might be helpful for refactoring.
  "
fu! s:MarkCurrentPred()
  call setpos('.', [0, w:lineL1PredStart, 1])
  normal! v
  call setpos('.', [1, w:lineL1PredEnd, w:lineL1PredEndCol])
endfu
