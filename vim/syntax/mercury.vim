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

syn keyword mercuryKeyword      any_func
syn keyword mercuryKeyword      any_pred
syn keyword mercuryKeyword      atomic
syn keyword mercuryKeyword      cc_multi
syn keyword mercuryKeyword      cc_nondet
syn keyword mercuryKeyword      det
syn keyword mercuryKeyword      end_module
syn keyword mercuryKeyword      erroneous
syn keyword mercuryKeyword      external
syn keyword mercuryKeyword      failure
syn keyword mercuryKeyword      finalize finalise
syn keyword mercuryKeyword      func
syn keyword mercuryKeyword      implementation
syn keyword mercuryKeyword      import_module
syn keyword mercuryKeyword      include_module
syn keyword mercuryKeyword      initialise initialize
syn keyword mercuryKeyword      inst
syn keyword mercuryKeyword      instance
syn keyword mercuryKeyword      interface
syn keyword mercuryKeyword      is
syn keyword mercuryKeyword      mode
syn keyword mercuryKeyword      module
syn keyword mercuryKeyword      multi
syn keyword mercuryKeyword      mutable
syn keyword mercuryKeyword      nondet
syn keyword mercuryKeyword      or_else
syn keyword mercuryKeyword      pragma
syn keyword mercuryKeyword      pred
syn keyword mercuryKeyword      promise
syn keyword mercuryKeyword      require_cc_multi
syn keyword mercuryKeyword      require_cc_nondet
syn keyword mercuryKeyword      require_complete_switch
syn keyword mercuryKeyword      require_det
syn keyword mercuryKeyword      require_erroneous
syn keyword mercuryKeyword      require_failure
syn keyword mercuryKeyword      require_multi 
syn keyword mercuryKeyword      require_nondet
syn keyword mercuryKeyword      require_semidet
syn keyword mercuryKeyword      require_switch_arms_cc_multi
syn keyword mercuryKeyword      require_switch_arms_cc_nondet
syn keyword mercuryKeyword      require_switch_arms_det
syn keyword mercuryKeyword      require_switch_arms_erroneous
syn keyword mercuryKeyword      require_switch_arms_failure
syn keyword mercuryKeyword      require_switch_arms_multi 
syn keyword mercuryKeyword      require_switch_arms_nondet
syn keyword mercuryKeyword      require_switch_arms_semidet
syn keyword mercuryKeyword      semidet
syn keyword mercuryKeyword      solver
syn keyword mercuryKeyword      trace
syn keyword mercuryKeyword      type
syn keyword mercuryKeyword      typeclass
syn keyword mercuryKeyword      use_module
syn keyword mercuryKeyword      where

syn keyword mercuryPragma       check_termination
syn keyword mercuryPragma       does_not_terminate
syn keyword mercuryPragma       fact_table
syn keyword mercuryPragma       inline
syn keyword mercuryPragma       loop_check
syn keyword mercuryPragma       memo
syn keyword mercuryPragma       minimal_model
syn keyword mercuryPragma       no_inline
syn keyword mercuryPragma       obsolete
syn keyword mercuryPragma       promise_equivalent_clauses
syn keyword mercuryPragma       source_file
syn keyword mercuryPragma       terminates
syn keyword mercuryPragma       type_spec

syn keyword mercuryCInterface   foreign_code
syn keyword mercuryCInterface   foreign_decl
syn keyword mercuryCInterface   foreign_enum
syn keyword mercuryCInterface   foreign_export
syn keyword mercuryCInterface   foreign_export_enum
syn keyword mercuryCInterface   foreign_import_module
syn keyword mercuryCInterface   foreign_proc
syn keyword mercuryCInterface   foreign_type

syn keyword mercuryCInterface   affects_liveness
syn keyword mercuryCInterface     does_not_affect_liveness
syn keyword mercuryCInterface     doesnt_affect_liveness
syn keyword mercuryCInterface   attach_to_io_state 
syn keyword mercuryCInterface   can_pass_as_mercury_type stable
syn keyword mercuryCInterface   may_call_mercury will_not_call_mercury
syn keyword mercuryCInterface   may_duplicate may_not_duplicate
syn keyword mercuryCInterface   may_modify_trail will_not_modify_trail
syn keyword mercuryCInterface   no_sharing unknown_sharing sharing
syn keyword mercuryCInterface   promise_pure promise_semipure
syn keyword mercuryCInterface   tabled_for_io local untrailed trailed 
syn keyword mercuryCInterface   thread_safe not_thread_safe maybe_thread_safe
syn keyword mercuryCInterface   will_not_throw_exception

syn keyword mercuryImpure       impure semipure

syn keyword mercuryToDo         XXX TODO NOTE         

syn keyword mercuryLogical      fail false true
syn keyword mercuryLogical      if then else
syn keyword mercuryLogical      impure_true 
syn keyword mercuryLogical      semidet_fail semidet_false
syn keyword mercuryLogical      semidet_succeed semidet_true
syn keyword mercuryLogical      some all not
syn keyword mercuryLogical      try catch catch_any

syn match   mercuryImplication  +<=>\|<=\|=>\|/\\\|\\/+
syn match   mercuryNumCode      +0'.\|0[box][0-9a-fA-F]*+
syn region  mercuryAtom         start=+'+ skip=+\\.+ end=+'+
syn region  mercuryString       start=+"+ skip=+\\.+ end=+"+                              contains=mercuryStringFmt
syn match   mercuryStringFmt    +\\[abfnrtv]\|\\x[0-9a-fA-F]*\\\|%[-+# *.0-9]*[dioxXucsfeEgGp]+                                                                           contained
syn	match   mercuryStringFmt    /\\[abfnrtv\\"]\|\\x[0-9a-fA-F]\+\\\|%[-+#*.0-9]*[dioxXucsfeEgGp]/ contained
syn region  mercuryClauseHead   start=+^[a-zA-Z]+ end=+=\|:-\|\.\s*$\|-->+                    contains=mercuryComment,mercuryCComment,mercuryAtom,mercuryString
syn region  mercuryCComment     start=+/\*+ end=+\*/+                                         contains=mercuryToDo
if !exists("mercury_no_highlight_overlong") || !mercury_no_highlight_overlong
  " The complicated regexp here matches an 80-column string,
  " with proper treatment of tabs (assuming the tab size is 8):
  " each row consists of 10 columns, and each column consists of either 8
  " non-tab characters, or 0-7 non-tab characters followed by a tab.
  syn match   mercuryFirst80 +^\([^	]\{8}\|[^	]\{0,7}	\)\{10}+                                contains=ALL
  syn match   mercuryTooLong +^\([^	]\{8}\|[^	]\{0,7}	\)\{10}..*+                             contains=mercuryFirst80
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
