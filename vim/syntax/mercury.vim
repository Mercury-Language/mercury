" Vim syntax file
" Language:     Mercury
" Maintainer:   Ralph Becket <rafe@cs.mu.oz.au>
" vim: ts=2 sw=2 et

if exists("b:current_syntax")
  finish
endif
let b:current_syntax = "mercury"

  " Mercury is case sensitive.
  "
syn case match

  " The default highlighting for Mercury comments is to only highlight the
  " initial `%' and subsequent `line' punctuation characters.  To highlight
  " everything including the comment text, add
  "
  "   let mercury_highlight_full_comment = 1
  "
  " somewhere in your `.vimrc' file.
  "
  " By default, parts of lines that extend over 80 characters will be
  " highlighted.  To avoid this behaviour, add
  "
  "   let mercury_no_highlight_overlong = 1
  "
  " somewhere in your `.vimrc' file.
  "
if exists("mercury_highlight_full_comment") && mercury_highlight_full_comment
  syn region  mercuryComment      start=+%+ end=+.*$+                                           contains=mercuryToDo
else
  syn region  mercuryComment      start=+%[-=%*_]*+ end=+.*$+he=s-1                             contains=mercuryToDo
endif
syn keyword mercuryKeyword      module use_module import_module
syn keyword mercuryKeyword      include_module end_module
syn keyword mercuryKeyword      interface implementation
syn keyword mercuryKeyword      pred mode func type inst 
syn keyword mercuryKeyword      is semidet det nondet multi erroneous
syn keyword mercuryKeyword      cc_nondet cc_multi
syn keyword mercuryKeyword      typeclass instance where
syn keyword mercuryKeyword      pragma promise
syn keyword mercuryPragma       inline no_inline
syn keyword mercuryPragma       type_spec source_file fact_table
syn keyword mercuryPragma       memo loop_check minimal_model
syn keyword mercuryPragma       terminates does_not_terminate check_termination
syn keyword mercuryCInterface   c_header_code c_code
syn keyword mercuryCInterface   foreign_proc foreign_header foreign_code
syn keyword mercuryCInterface   may_call_mercury will_not_call_mercury
syn keyword mercuryCInterface   thread_safe not_thread_safe
syn keyword mercuryCInterface   promise_pure promise_semipure
syn keyword mercuryImpure       impure semipure
syn keyword mercuryToDo         XXX TODO NOTE         
syn keyword mercuryLogical      some all not if then else true fail
syn match   mercuryImplication  +<=>\|<=\|=>\|/\\\|\\/+
syn match   mercuryNumCode      +0'.\|0[box][0-9a-fA-F]*+
syn region  mercuryAtom         start=+'+ skip=+\\.+ end=+'+
syn region  mercuryString       start=+"+ skip=+\\.+ end=+"+                              contains=mercuryStringFmt,mercuryCComment
syn match   mercuryStringFmt    +\\[abfnrtv]\|\\x[0-9a-fA-F]*\\\|%[-+# *.0-9]*[dioxXucsfeEgGp]+                                                                           contained
syn region  mercuryClauseHead   start=+^[a-zA-Z]+ end=+=\|:-\|\.\s*$\|-->+                    contains=mercuryComment,mercuryCComment,mercuryAtom,mercuryString
syn region  mercuryCComment     start=+/\*+ end=+\*/+                                         contains=mercuryToDo
if !exists("mercury_no_highlight_overlong") || !mercury_no_highlight_overlong
  syn match   mercuryFirst80 +^.\{80}+                                                          contains=ALL
  syn match   mercuryTooLong +^.\{81,}+                                                         contains=mercuryFirst80
endif

syn sync fromstart

hi link mercuryComment          Comment
hi link mercuryCComment         Comment
hi link mercuryNumCode          Special
hi link mercuryImpure           Special
hi link mercuryKeyword          Keyword
hi link mercuryPragma           PreProc
hi link mercuryCInterface       PreProc
hi link mercuryToDo             Todo
hi link mercuryLogical          Special
hi link mercuryImplication      Special
hi link mercuryClauseHead       Statement
hi link mercuryString           String
hi link mercuryStringFmt        Special
hi link mercuryAtom             Constant
hi link mercuryTooLong          ErrorMsg
