" vim: ft=vim ts=2 sw=2 et
" Language:     Mercury
" Maintainer:   Sebastian Godelet <sebastian.godelet@outlook.com>
" Extensions:   *.m *.moo
" Last Change:  2018-09-01

" for documentation, please use :help mercury-syntax

if exists("b:current_syntax")
  finish
endif
let b:current_syntax = "mercury"

  " Mercury is case sensitive.
syn case match

set synmaxcol=250

if has("folding") && exists("mercury_folding") && mercury_folding
    " folding is only changed (if not forced) if the Vim default (manual) is active,
    " this avoids conflicts with existing user settings
  if (&fdm == "manual") || (exists("mercury_folding_force") && mercury_folding_force)
    set fdm=indent
  endif
    " the default foldminlines = 1 is not appropriate for Mercury,
    " so we set it higher
  if &foldminlines == 1
    set foldminlines=10
  endif
endif

syn match mercurySingleton      "\v<_([A-Z][a-z_A-Z0-9]*)?>"
syn keyword mercuryKeyword      any_func
syn keyword mercuryKeyword      any_pred
syn keyword mercuryLogical      atomic
syn keyword mercuryKeyword      cc_multi
syn keyword mercuryKeyword      cc_nondet
syn keyword mercuryKeyword      det
syn keyword mercuryKeyword      end_module
syn keyword mercuryKeyword      erroneous
syn keyword mercuryKeyword      external external_pred external_func
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
syn keyword mercuryLogical      or_else
syn keyword mercuryKeyword      pragma
syn keyword mercuryKeyword      pred
syn keyword mercuryKeyword      promise
syn keyword mercuryPragma       require_cc_multi
syn keyword mercuryPragma       require_cc_nondet
syn keyword mercuryPragma       require_complete_switch
syn keyword mercuryPragma       require_det
syn keyword mercuryPragma       require_erroneous
syn keyword mercuryPragma       require_failure
syn keyword mercuryPragma       require_multi
syn keyword mercuryPragma       require_nondet
syn keyword mercuryPragma       require_semidet
syn keyword mercuryPragma       require_switch_arms_cc_multi
syn keyword mercuryPragma       require_switch_arms_cc_nondet
syn keyword mercuryPragma       require_switch_arms_det
syn keyword mercuryPragma       require_switch_arms_erroneous
syn keyword mercuryPragma       require_switch_arms_failure
syn keyword mercuryPragma       require_switch_arms_multi
syn keyword mercuryPragma       require_switch_arms_nondet
syn keyword mercuryPragma       require_switch_arms_semidet
syn keyword mercuryKeyword      semidet
syn keyword mercuryKeyword      solver
syn keyword mercuryPurity       trace
syn keyword mercuryKeyword      type
syn keyword mercuryKeyword      typeclass
syn keyword mercuryKeyword      use_module
syn keyword mercuryKeyword      where

syn keyword mercuryPragma       check_termination
syn keyword mercuryPragma       consider_used
syn keyword mercuryPragma       does_not_terminate
syn keyword mercuryPragma       fact_table
syn keyword mercuryPragma       inline
syn keyword mercuryPragma       loop_check
syn keyword mercuryPragma       memo
syn keyword mercuryPragma       minimal_model
syn keyword mercuryPragma       no_inline
syn keyword mercuryPragma       obsolete
syn keyword mercuryPragma       obsolete_proc
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

syn keyword mercuryForeignMod   affects_liveness
syn keyword mercuryForeignMod     does_not_affect_liveness
syn keyword mercuryForeignMod     doesnt_affect_liveness
syn keyword mercuryForeignMod   attach_to_io_state
syn keyword mercuryForeignMod   can_pass_as_mercury_type word_aligned_pointer stable
syn keyword mercuryForeignMod   may_call_mercury will_not_call_mercury
syn keyword mercuryForeignMod   may_duplicate may_not_duplicate
syn keyword mercuryForeignMod   may_modify_trail will_not_modify_trail
syn keyword mercuryForeignMod   no_sharing unknown_sharing sharing
syn keyword mercuryForeignMod   promise_pure promise_semipure
syn keyword mercuryForeignMod   tabled_for_io local untrailed trailed
syn keyword mercuryForeignMod   thread_safe not_thread_safe maybe_thread_safe
syn keyword mercuryForeignMod   will_not_throw_exception
syn keyword mercuryForeignMod   terminates

syn keyword mercuryPurity       impure
syn keyword mercuryPurity       promise_impure
syn keyword mercuryPurity       promise_pure
syn keyword mercuryPurity       promise_semipure
syn keyword mercuryPurity       semipure

syn keyword mercuryLogical      fail false true
syn keyword mercuryLogical      if then else
syn keyword mercuryLogical      impure_true
syn keyword mercuryLogical      semidet_fail semidet_false
syn keyword mercuryLogical      semidet_succeed semidet_true
syn keyword mercuryLogical      some all not
syn keyword mercuryLogical      try catch catch_any
syn keyword mercuryLogical      promise_equivalent_solutions
syn keyword mercuryLogical      promise_equivalent_solution_sets arbitrary

syn keyword mercuryBool         yes no
syn keyword mercuryOperator     div rem mod

syn match   mercuryImplKeyword  "\v\$(file|grade|pred|module|line|\d+)>"
syn match   mercuryOperator     "/"           " divide
syn match   mercuryOperator     "//"          " (integer) divide
syn match   mercuryDelimiter    ","           " list seperator or conjunction
syn match   mercuryOperator     "-"           " substraction operator or unary minus
syn match   mercuryOperator     "="           " unification
syn match   mercuryDelimiter    "|"           " cons
syn match   mercuryImplication  "->"          " 'then' arrow
syn match   mercuryOperator     "-->"         " DCG clause
syn match   mercuryOperator     "--->"        " 'typedef'
syn match   mercuryOperator     "/\\"         " (binary) and
syn match   mercuryOperator     "\\"          " (bitwise) complement
syn match   mercuryOperator     "\\/"         " (binary) or
syn match   mercuryLogical      "\\+"         " logical not
syn match   mercuryOperator     "=\.\."       " Prolog univ
syn match   mercuryOperator     "=<"          " greater equal or contravariant
syn match   mercuryOperator     "=\\="        " not structual equal (for Prolog)
syn match   mercuryOperator     "@"
syn match   mercuryOperator     "@<"
syn match   mercuryOperator     "@=<"
syn match   mercuryOperator     "@>"
syn match   mercuryOperator     "@>="
syn match   mercuryOperator     ">="         " smaller equal or co-variant
syn match   mercuryOperator     ">"          " greater
syn match   mercuryOperator     ">>"         " right shift
syn match   mercuryOperator     "<"          " smaller
syn match   mercuryOperator     "<<"         " left shift
syn match   mercuryOperator     "\\="        " not unify
syn match   mercuryOperator     "\\=="       " not unify (for Prolog)
syn match   mercuryOperator     "\~"
syn match   mercuryOperator     "\~="
syn match   mercuryOperator     ":="         " field update
syn match   mercuryOperator     ":-"         " reverse implication
syn match   mercuryOperator     "=:="        " Structural equality (for Prolog)
syn match   mercuryPurity       "![:.]\?"    " State variable accessors
syn match   mercuryImplication  ";"          " Disjunction
syn match   mercuryOperator     "+"          " addition operator or unary plus
syn match   mercuryOperator     "++"         " concatenation
syn match   mercuryOperator     ":"
syn match   mercuryDelimiter    "::"         " Type/Mode specifier
syn match   mercuryOperator     "&"          " Parallel conjuction
syn match   mercuryOperator     "?-"         " Prolog compatability
syn match   mercuryOperator     "*"          " multiply
syn match   mercuryDelimiter    "\^"         " field access
syn match   mercuryOperator     /\v`[^`']+`/ " inlined operator
syn match   mercuryImplication  "<=>\|<=\|=>"
syn match   mercuryNumCode /\v<0b[01_]*[01](_*[iu](8|16|32|64)?)?>/
syn match   mercuryNumCode /\v<0o[0-7_]*[0-7](_*[iu](8|16|32|64)?)?>/
syn match   mercuryNumCode /\v<0x[0-9A-Fa-f_]*\x(_*[iu](8|16|32|64)?)?>/
syn match   mercuryNumCode /\v<0'.|<[0-9]([0-9_]*[0-9])?(_*[iu](8|16|32|64)?)?>/
syn match   mercuryFloat   /\v<[0-9]([0-9_]*[0-9])?\.[0-9]([0-9_]*[0-9])?([eE][-+]?[0-9]([0-9_]*[0-9])?)?>/
syn match   mercuryFloat   /\v<[0-9]([0-9_]*[0-9])?[eE][-+]?[0-9]([0-9_]*[0-9])?>/
syn region  mercuryAtom    start=+'+ skip=+\\'+   end=+'+ contains=
      \ mercuryStringEsc,@mercuryFormatting,mercuryEscErr,@Spell
syn region  mercuryString matchgroup=mercuryString
      \ start=+"+ skip=/\v(\\x?\x+|\\)@<!\\"|""/ end=+"+ keepend contains=
      \ mercuryStringFmt,mercuryStringEsc,@mercuryFormatting,
      \ mercuryEscErr,mercuryStringEsc,@Spell
syn match   mercuryStringFmt    /%[-+# *.0-9]*[dioxXucsfeEgGp]/       contained
  " mercury*Esc are common to "mercuryAtom" and "mercuryString"
syn match   mercuryEscErr       /\v\\([ \t]+$)@=/ contained " matching escaped newline
syn match   mercuryEscErr       /\v\\[uUx]/ contained " must come before \\u\x{4}
syn match   mercuryEscErr       /\v\\0/     contained " \0 literals are not allowed
syn match   mercuryStringEsc    /\\$/       contained " matching escaped newline
syn match   mercuryStringEsc    /\v\\[abfnrtv\\"]/     contained
syn match   mercuryStringEsc    /\v\\u\x{4}/           contained
syn match   mercuryStringEsc    /\v\\U00(10|0\x)\x{4}/ contained
syn match   mercuryStringEsc    /\v\\x\x+\\/           contained
syn match   mercuryStringEsc    /\v\\[0-7][0-7]+\\/    contained
syn match   mercuryStringEsc    /\v""/ contained
  " matching unbalanced brackets (before "mercuryTerm", "mercuryBlock", ...)
syn match mercuryErrInAny       "(\|\[{\|}\|\]\|)"
syn match mercuryTerminator     "\v\.(\s+|$)@=" " after mercuryErrInAny
syn match mercuryOperator       "\.\."          " after mercuryTerminator

  " see "https://github.com/Twinside/vim-haskellConceal"
  " see "http://rapidtables.com/math/symbols/Basic_Math_Symbols.htm"
if has("conceal") && exists("mercury_conceal") && mercury_conceal
  hi clear Conceal
  hi def link Conceal mercuryOperator
  setlocal conceallevel=2

    " A crude but simple "solution" to the compose operator problem
  syn match mercuryOperator  "`compose`" conceal cchar=o

  if exists("mercury_conceal_extra") && mercury_conceal_extra
      " these characters only display properly on some machines if
      " setglobal ambiw=double
    if has("multi_byte") && (!has("win32") || (exists("ambiw") && ambiw == "double"))
      syn match mercuryOperator  "/\\"          conceal cchar=∧
      syn match mercuryOperator  "\\/"          conceal cchar=∨
      syn match mercuryOperator  "`xor`"        conceal cchar=⊕
      syn match mercuryOperator  "`member`"     conceal cchar=∈
      syn match mercuryOperator  "`contains`"   conceal cchar=∋
      syn match mercuryOperator  "`union`"      conceal cchar=∪
      syn match mercuryOperator  "`intersect`"  conceal cchar=∩
      syn match mercuryOperator  "`difference`" conceal cchar=∆
      syn match mercuryOperator  "`insert`"     conceal cchar=⎀
      syn match mercuryOperator  "`delete`"     conceal cchar=\
      syn match mercuryOperator  "`subset`"     conceal cchar=⊆
      syn match mercuryOperator  "`superset`"   conceal cchar=⊇
    endif
      " This avoids confusion of =< and =>
    syn match mercuryOperator  ">="        conceal cchar=≥
    syn match mercuryOperator  "=<"        conceal cchar=≤
    syn match mercuryOperator  "\\=[=]\@!" conceal cchar=≠
    syn match mercuryOperator  "`x`"       conceal cchar=×
    syn match mercuryOperator  "//"        conceal cchar=÷
    syn match mercuryOperator "--->"       conceal cchar=⟶
    syn match mercuryOperator "=="         conceal cchar=≡
    syn match mercuryOperator ":-"         conceal cchar=⊢

       " unfortunately, Vim does not allow different conceal colours,
       " so these are not concealed by default
    if exists("mercury_conceal_logical") && mercury_conceal_logical
      syn match mercuryImplication "=>"   conceal cchar=⇒
      syn match mercuryImplication "<="   conceal cchar=⇐
      syn match mercuryImplication "<=>"  conceal cchar=⇔
      syn keyword mercuryNumCode  inf     conceal cchar=∞
      syn keyword mercuryLogical  some    conceal cchar=∃
      syn keyword mercuryLogical  all     conceal cchar=∀
      syn match mercuryLogical    "\\+"   conceal cchar=¬
    endif
  endif
endif

  " matching the `double star' after the multiplication operator
syn match mercuryOperator "\v[*]{2}"
  " All valid Mercury comments
syn cluster mercuryComments contains=mercuryComment,mercuryCComment
  " The clusters contain all valid Mercury code. The nesting is done to allow
  " for matching of parenthesis, DCG terms and lists
syn cluster mercuryTerms     contains=mercuryBlock,mercuryList,mercuryString,
      \ mercuryDelimiter,mercuryAtom,mercuryNumCode,mercuryFloat,
      \ @mercuryComments,mercuryKeyword,mercuryImplKeyword,
      \ @mercuryFormatting,mercuryErrInAny,mercuryBool,mercuryOperator,
      \ mercurySingleton,mercuryImplication,mercuryInlined,mercuryLogical,
      \ mercuryPurity,mercuryDCGOrTuple

syn region  mercuryList       matchgroup=mercuryBracket   start='\[' end=']'
      \ transparent fold  contains=@mercuryTerms
syn region  mercuryBlock      matchgroup=mercuryBracket   start='(' end=')'
      \ transparent fold  contains=@mercuryTerms
syn region  mercuryDCGOrTuple matchgroup=mercuryBracket   start='{' end='}'
      \ transparent fold  contains=@mercuryTerms
syn region  mercuryForeignModList matchgroup=mercuryBracket start='\[' end=']'
      \ transparent fold  contained contains=mercuryForeignMod,
      \ mercuryDelimiter,@mercuryComments,@mercuryFormatting,
      \ mercuryString,mercuryOperator,mercuryBlock

syn match mercuryClauseHead /\v^[a-z][a-zA-Z0-9_.]*[( =]@=/

if !exists("mercury_no_highlight_foreign") || !mercury_no_highlight_foreign
    " Basic syntax highlighting for foreign code
  syn cluster mercuryForeign contains=mercuryForeignModList,mercuryCInterface,
        \ mercuryKeyword,mercuryOperator,
        \ mercuryAtom,@mercuryComments,mercuryDelimiter,mercurySingleton,
        \ @mercuryFormatting,mercuryForeignId

  syn region  mercuryForeignCBlock       matchgroup=mercuryBracket start=/\v\(("C"|c)/rs=s+1 end=')'
        \ transparent fold contained contains=@mercuryForeign,
        \ mercuryCCode,mercuryBlock
  syn region  mercuryForeignCSharpBlock  matchgroup=mercuryBracket start=/\v\(("C#"|csharp)/rs=s+1 end=')'
        \ transparent fold contained contains=@mercuryForeign,
        \ mercuryCSharpCode,mercuryBlock
  syn region  mercuryForeignJavaBlock    matchgroup=mercuryBracket start=/\v\(("Java"|java)/rs=s+1 end=')'
        \ transparent fold contained contains=@mercuryForeign,
        \ mercuryJavaCode,mercuryBlock
  syn region  mercuryForeignErlangBlock  matchgroup=mercuryBracket start=/\v\(("Erlang"|erlang)/rs=s+1 end=')'
        \ transparent fold contained contains=@mercuryForeign,
        \ mercuryErlangCode,mercuryBlock
  syn cluster mercuryForeignBlock contains=mercuryForeignCBlock,
        \ mercuryForeignCSharpBlock,mercuryForeignJavaBlock,
        \ mercuryForeignErlangBlock
  syn match   mercuryPragmaForeign /\v^\s*:-\s+pragma\s+foreign_(code|proc|decl|type|export(_enum)?|enum|import_module)/
        \ transparent nextgroup=@mercuryForeignBlock

    " C-Style syntax as a basis for C, C# and Java
  syn keyword mercuryCLikeKeyword contained if else goto switch case for while
  syn keyword mercuryCLikeKeyword contained do break continue return
  syn keyword mercuryCLikeType contained const static volatile extern typedef inline
  syn keyword mercuryCLikeKeyword contained default
  syn keyword mercuryCLikeType contained void int char long short byte unsigned signed
  syn keyword mercuryCLikeType contained struct float double enum
  syn match mercuryCLikeDelimiter ";\|," contained
  syn match mercuryCLikeOperator "\v[-!+=*/><~?:%]" contained
  syn match mercuryCLikeOperator "[-!+=*/><]\?=" contained
  syn match mercuryCLikeOperator "--\|++" contained
  syn match mercuryCLikeOperator "|\{1,2}\|&\{1,2}" contained
  syn match mercuryCLikeBracket  "\[\|]" contained
  syn match mercuryCLikeBracket  "\v[{}()]" contained
  syn match mercuryCLikeCharEsc /\v\\\\([abfnrtv]|0[0-7]*|[xuU]\x+)?/ contained
  syn match mercuryCLikeCharEsc +\\\\""+ contained
  syn region mercuryCLikeChar start=+'+ end=+'+ contained contains=mercuryCLikeCharEsc
  syn match mercuryCLikeNumber /\v<([1-9][0-9]*|0[xX]\x+|0[0-7]*)/ contained
  syn match mercuryCLikeFloat /\v<([0-9]+\.[0-9]+([eE][-+]?[0-9]+)?)/ contained
  syn match mercuryCLikeFloat /\v<([0-9]+[eE][-+]?[0-9]+)/ contained
  syn cluster mercuryCLike contains=mercuryCLikeKeyword,mercuryCLikeType
  syn cluster mercuryCLike add=mercuryCLikeOperator,mercuryCComment,mercuryCppLikeComment,mercuryCLikeChar
  syn cluster mercuryCLike add=mercuryCLikeNumber,mercuryCLikeFloat,mercuryCLikeBracket
  syn cluster mercuryCLike add=mercuryCLikeDelimiter,mercuryForeignIface
  syn cluster mercuryCLike add=@mercuryFormatting

    " C-Language formatting with Mercury types MR_*
  syn keyword mercuryCType contained size_t pid_t offset_t union
  syn keyword mercuryCType contained MR_bool MR_Bool
  syn keyword mercuryCType contained MR_Word MR_Integer MR_Unsigned
  syn keyword mercuryCType contained MR_ArrayPtr MR_Float MR_file MercuryFile[Ptr]
  syn keyword mercuryCType contained MR_String MR_ConstString MR_Char
  syn match mercuryCType "\v<MR_((Pseudo)?TypeInfo|Construct_Info|TypeCtor(Desc|Info)|AllocSiteInfoPtr)|MercuryLock>" contained
  syn match mercuryCType "\v<(MR_)?[u]?int(_least|_fast)?(8|16|32|64)_t>" contained
  syn match mercuryForeignIface "\v<(MR_)?[U]?INT(_LEAST|_FAST)?(8|16|32|64)_(TYPE|LENGTH_MODIFIER)>" contained
  syn keyword mercuryCKeyword contained sizeof typeof offsetof
  syn keyword mercuryCConst contained NULL EOF
  syn keyword mercuryCConst contained CHAR_BIT CHAR_MAX CHAR_MIN
  syn keyword mercuryCConst contained SCHAR_BIT SCHAR_MAX SCHAR_MIN
  syn keyword mercuryCConst contained LONG_MAX ULONG_MAX LONG_MIN
  syn keyword mercuryCConst contained LLONG_MAX ULLONG_MAX LLONG_MIN
  syn keyword mercuryCConst contained INT_MAX UINT_MAX INT_MIN
  syn keyword mercuryCConst contained SHRT_MAX USHRT_MAX SHRT_MIN
  syn keyword mercuryCBool  contained MR_TRUE MR_FALSE
  syn keyword mercuryCBool  contained MR_YES MR_NO
  syn match mercuryForeignIface contained "\v<MR_[A-Z]+_LENGTH_MODIFIER>"
  syn match mercuryForeignIface contained "\v<MR_THREAD_SAFE>"
  syn match mercuryCFunc "\v<MR_(list_(empty|head|tail)|incr_hp((_atomic)?|((_type)?_msg))|assert|fatal_error|make_aligned_string)>" contained
  syn match mercuryCPreProc "#\(if\(n\?def\)\?\|else\|elif\|endif\|define\|undef\|include\|error\|warning\|line\)" contained
  syn match mercuryCPreProc    "\v(\\){1,2}$" contained
  syn match mercuryCStringFmt  /%[I]\?[-+# *.0-9]*[dioxXucsfeEgGp]/ contained
  syn region mercuryCString start=+""+ end=+""+ contained contains=mercuryCStringFmt,mercuryCLikeCharEsc,@Spell
  syn region mercuryCString start=+\v\\"+ end=+\v\\"+ contained contains=mercuryCStringFmt,mercuryCLikeCharEsc,@Spell
  syn cluster mercuryC contains=@mercuryCLike,mercuryCType,mercuryCKeyword
  syn cluster mercuryC add=mercuryCPreProc,mercuryCString,mercuryCBool,mercuryCConst,mercuryCFunc

    " C++-Style for Java and C# (bool, // comments, exception handling etc)
  syn keyword mercuryCppLikeKeyword contained class new delete try catch finally
        \ instanceof abstract throw[s] extends this super base synchronize[d]
        \ override foreach in using import ref implements
  syn keyword mercuryCppLikeBool contained true false
  syn keyword mercuryCppLikeConst contained null[ptr]
  syn match mercuryCppLikeOperator "@" contained
  syn match mercuryCppLikeType "\v<((io|runtime)\.(\_\s+)?)?(MR_)[A-Za-z_0-9]+>" contained
  syn keyword mercuryCppLikeMod contained public private protected internal virtual
  syn keyword mercuryCppLikeMod contained final readonly volatile transient
  syn cluster mercuryCppLike contains=@mercuryCLike,mercuryCPreProc,mercuryCString,mercuryCppLikeComment,mercuryCppLikeKeyword
  syn cluster mercuryCppLike add=mercuryCppLikeBool,mercuryCppLikeMod,mercuryCppLikeConst,mercuryCppLikeType,mercuryCppLikeOperator

    " Declaration for ISO C
  syn region mercuryCCode      matchgroup=mercuryString start=+"+ skip=+""+ end=+"+ transparent fold contained contains=@mercuryC

   " Declaration for C#
  syn match mercuryCSharpStringFmt "{[0-9]}" contained
  syn match mercuryCSharpStringFmtEsc "{{\|}}" contained
  syn keyword mercuryCSharpType contained object string decimal bool uint
  syn keyword mercuryCSharpType contained ulong sbyte ushort
  syn match mercuryCSharpType contained "\v<mr_bool>\."he=e-1 nextgroup=mercuryCSharpBool
  syn match mercuryCSharpBool contained "\v<(YES|NO)>"
  syn match mercuryCSharpType "\v<System\.((IO|Text|Diagnostics)\.)?[A-Z][A-Za-z_0-9]+>"
  syn region mercuryCSharpString start=+""+ end=+""+ contained contains=mercuryCLikeCharEsc,
        \ mercuryCSharpStringFmt,mercuryCSharpStringFmtEsc,@Spell
  syn region mercuryCSharpString start=+\v\\"+ end=+\v\\"+ contained contains=mercuryCLikeCharEsc,
        \ mercuryCSharpStringFmt,mercuryCSharpStringFmtEsc,@Spell
  syn cluster mercuryCSharp contains=@mercuryCppLike,mercuryCSharpString,mercuryCSharpType
  syn region mercuryCSharpCode matchgroup=mercuryString start=+"+ skip=+""+ end=+"+ transparent fold contained
        \ contains=@mercuryCSharp

    " Declaration for Java
  syn match mercuryJavaType "\v<([a-z_][A-Za-z0-9_]*\.(\_\s+)?)+[A-Z][A-Z_a-z0-9]+>" contained
  syn match mercuryJavaType "\v<(String(Builder)?|Override|Object|Integer|Byte)>" contained
  syn match mercuryJavaType "\v<(Short|Float|Double|Void|Boolean|Character|System|Runtime|boolean)>" contained
  syn match mercuryJavaType "\v<bool>\."he=e-1 contained nextgroup=mercuryJavaBool
  syn match mercuryJavaBool contained "\v<(YES|NO)>"
  syn region mercuryJavaCode   matchgroup=mercuryString start=+"+ skip=+""+ end=+"+
        \ transparent fold contained contains=@mercuryCppLike,mercuryCString,mercuryJavaType

    " Declaration for Erlang
  syn keyword mercuryErlangKeyword contained after and andalso band begin bnot bor bsl bsr bxor case
        \ catch cond end fun if let not of orelse query receive throw try when xor
  " syn keyword mercuryErlangBool true false
  syn match mercuryErlangExtNumLiteral "\v([2-9]|[12][0-9]|3[0-6])#[A-Za-z0-9]+" contained
  syn match mercuryErlangOperator "\v[?]" contained
  syn match mercuryErlangLogical "\v[,;.]" contained
  syn region mercuryErlangString start=+""+ end=+""+ contained contains=@Spell
  syn region mercuryErlangString start=+\v\\"+ end=+\v\\"+ contained contains=@Spell
  syn cluster mercuryErlangTerms contains=mercuryErlangBlock,mercuryErlangList,
        \ mercuryErlangString,mercuryCLikeChar,mercuryCLikeNumber,
        \ mercuryErlangExtNumLiteral,mercuryFloat,mercuryComment,mercuryKeyword,
        \ mercuryErlangKeyword, mercuryErlangOperator, mercuryCComment,
        \ mercuryErlangBool,mercuryOperator,mercurySingleton,mercuryImplication,
        \ mercuryErlangDCGAction,mercuryErlangLogical,@mercuryFormatting
  syn region  mercuryErlangList contained matchgroup=mercuryBracket
        \ start='\[' end=']' transparent fold  contains=@mercuryErlangTerms
  syn region  mercuryErlangBlock    contained matchgroup=mercuryBracket
        \ start='(' end=')'  transparent fold  contains=@mercuryErlangTerms
  syn region  mercuryErlangDCGAction contained matchgroup=mercuryBracket
        \ start='{' end='}'  transparent fold  contains=@mercuryErlangTerms

  syn cluster mercuryErlang contains=@mercuryErlangTerms,mercuryErlangDCGAction,
        \ mercuryForeignIface

  syn region mercuryErlangCode   matchgroup=mercuryString start=+"+ skip=+""+ end=+"+
        \ transparent fold contained contains=@mercuryErlang

    " Matching the foreign language name identifiers, this comes after all the
    " code blocks, to match the identifiers in quotes
  syn match mercuryForeignId /\v<(c|csharp|java|il|erlang)>/ contained
  syn region mercuryForeignId contained matchgroup=mercuryString
        \ start=+\v["](C#|Java|C|I[Ll]|Erlang)["]{-}+rs=s+1 end=+"+

    " Matching foreign interface builtins and success indicator
  syn keyword mercuryForeignIface contained SUCCESS_INDICATOR
  syn match mercuryForeignIface "\v<builtin.[A-Z][A-Z_0-9]+>" contained
  syn match mercuryForeignIface "\v<MR_(VERSION|FULLARCH|CYGWIN|WIN32|MINGW64|COMPARE_(LESS|EQUAL|GREATER)|ALLOC_ID)>" contained
endif

if !exists("mercury_no_highlight_trailing_whitespace") || !mercury_no_highlight_trailing_whitespace
  syn match mercuryWhitespace "\v[ ]+[\n]@="
  syn cluster mercuryFormatting add=mercuryWhitespace
endif

if !exists("mercury_no_highlight_tabs") || !mercury_no_highlight_tabs
  syn match mercuryWhitespace "\t"
  syn cluster mercuryFormatting add=mercuryWhitespace
endif

if exists("mercury_highlight_comment_special") && mercury_highlight_comment_special
  syn keyword mercuryToDo contained XXX TODO NOTE[_TO_IMPLEMENTORS] MISSING HACK
        \ nextgroup=mercuryCommentOp
  syn keyword mercuryToDo contained HINT WARNING IMPORTANT
        \ nextgroup=mercuryCommentOp
  syn match mercuryCommentOp contained ": "

  syn cluster mercuryCommentDirectives contains=@Spell,mercuryToDo,mercuryCommentUri

    " Highlights the output of the Mercury error command (in extras)
  syn match mercuryCommentErr "\v(\* )@<=###[ ]@=" contained

    " Matches file names, email, file and http addresses (on a best effort basis).
    " This avoids spell checking on those,
    " and could also be used for plug-in development to open a browser, etc.
  syn match mercuryCommentUri contained "\v<[-0-9a-zA-Z.+_]+[@][-0-9a-zA-Z.+_]+\.[a-zA-Z]{2,10}>"
  syn match mercuryCommentUri contained "\v<(http[s]?|file)://[^ ><]+>"
  syn match mercuryCommentUri contained "\v<([a-z][a-z0-9._]+[/])*[a-z][a-z0-9._]+[.]m>"

  syn match mercuryCommentSlash "/" contained nextgroup=mercuryCommentArity
  syn match mercuryCommentArity "\v\d+" contained
  syn match mercuryCommentSingleQuote /\v'[A-Za-z._0-9()]+'/ contained nextgroup=mercuryCommentSlash

    " Header means the line describing the Arguments of a predicate or function,
    " terminated with a colon. This also stops spell check on the argument names,
    " which Vim is not good at dealing with.
  syn region mercuryCommentHeader contained matchgroup=mercuryString
        \ start='\v[a-z][A-Za-z._0-9]*([(]([)]|[\[][a-z"])@!|\s*[=])@='
        \ matchgroup=NONE keepend
        \ end="\v([.])|([:][-]@!)|(<[a-z]@=)|[)%][ \t]*[\n]@="
        \ contains=mercuryOperator,mercuryCommentHeaderBlock,
        \ mercuryCommentHeaderList,mercuryCommentHeaderTuple,
        \ mercuryErrInAny,mercuryCommentHeaderCont,@mercuryFormatting
  syn match mercuryCommentHeaderCont contained "\v^[ \t]*[%]"
  syn region mercuryCommentHeaderList contained matchgroup=mercuryBracket
        \ start='\[' end=']' transparent fold
        \ contains=@mercuryTerms,mercuryCommentHeaderCont
  syn region mercuryCommentHeaderBlock contained matchgroup=mercuryBracket
        \ start='(' end=')' transparent fold
        \ contains=@mercuryTerms,mercuryCommentHeaderCont
  syn region  mercuryCommentHeaderTuple contained matchgroup=mercuryBracket
        \ start='{' end='}' transparent fold
        \ contains=@mercuryTerms,mercuryCommentHeaderCont
  syn region mercuryCommentTexSingleQuote contained oneline
        \ start="\v`[^`]@=" end="\v'" nextgroup=mercuryCommentSlash
  syn region mercuryCommentTexDblQuote start="``" end="''" oneline contained
        \ contains=@Spell
  syn region mercuryCommentVimLine contained start="\vvim[:]" end="\v[\n]@="

  syn cluster mercuryCommentDirectives add=mercuryCommentHeader
  syn cluster mercuryCommentDirectives add=mercuryCommentSingleQuote
  syn cluster mercuryCommentDirectives add=@mercuryCommentTex
  syn cluster mercuryCommentDirectives add=mercuryCommentVimLine
  syn cluster mercuryCommentTex contains=mercuryCommentTexDblQuote
  syn cluster mercuryCommentTex contains=mercuryCommentTexSingleQuote
endif

if exists("mercury_highlight_full_comment") && mercury_highlight_full_comment
  hi def link mercuryComment        Comment
  hi def link mercuryCommentUri     Underlined
  hi def link mercuryCComment       Comment
  hi def link mercuryCppLikeComment Comment

  syn region mercuryComment start=/%/ end=/\v[\n]@=/ oneline contains=
        \ @mercuryCommentDirectives,@mercuryFormatting
  syn region mercuryCComment start="/\*" end="\*/" fold contains=
        \ @mercuryCommentDirectives,@mercuryFormatting,mercuryCommentErr
  syn region mercuryCppLikeComment start="//" end=/\v[\n]@=/ oneline contained contains=
        \ @mercuryCommentDirectives,@mercuryFormatting
else
    " NOTE: the regions itself are not highlighted, just their start/end
    " tokens, this is needed in order to fake "transparent", which could be used
    " instead but does not support @Spell as a side-effect
  hi def link mercuryComment        Normal
  hi def link mercuryCComment       Normal
  hi def mercuryCommentUri     term=underline cterm=underline gui=underline
  hi def link mercuryCppLikeComment Normal
  hi def link mercuryLeadTrailStar  Comment

  syn match mercuryLeadTrailStar contained "^\v[ \t]*[*]+|[*]+$"
  syn region mercuryComment matchgroup=mercuryCommentToken start=/%[-=%*_]*/ end=/\v[\n]@=/ oneline
        \ contains=@mercuryCommentDirectives,@mercuryFormatting
  syn region mercuryCComment matchgroup=mercuryCommentToken start="\v/\*" end="\v[*]+/" keepend fold
        \ contains=@mercuryCommentDirectives,mercuryLeadTrailStar,@mercuryFormatting,
        \ mercuryCommentErr
  syn region mercuryCppLikeComment matchgroup=mercuryCommentToken start="//" end=/\v[\n]@=/ oneline
        \ contained contains=@mercuryCommentDirectives,@mercuryFormatting
endif

  " Matching the Unix shebang
syn region mercuryShebang matchgroup=mercuryCommentToken  start="^\%1l#!/" end=/\v.+$/
      \ oneline

  " Matching over-long lines
if !exists("mercury_no_highlight_overlong") || !mercury_no_highlight_overlong
  syn match mercuryTooLong /\%80v[^")}\]%]*/
  syn cluster mercuryFormatting add=mercuryTooLong
endif

  " Clear all syntax (this is maybe not needed for newer versions of Vim
syn sync clear
  " sync on a comment start, this assumes that no line comment is within a
  " C-style comment
syn sync match mercurySync grouphere NONE "\v^[%]------"

hi def link mercuryAccess           Identifier
hi def link mercuryAtom             Constant
hi def link mercuryBracket          Delimiter
hi def link mercuryClauseHead       Statement
hi def link mercuryCommentErr       ErrorMsg
hi def link mercuryCommentToken     Comment
hi def link mercuryCommentInfo      Identifier
if exists("mercury_highlight_comment_special") && mercury_highlight_comment_special
  hi def link mercuryCommentSlash   Operator
  hi def link mercuryCommentArity   Number
  hi def link mercuryCommentHeaderCont Comment
  hi def link mercuryCommentSingleQuote  String
  hi def link mercuryCommentTexDblQuote  String
  hi def link mercuryCommentTexSingleQuote  String
  hi def link mercuryCommentVimLine mercuryComment
endif
hi def link mercuryCommentOp        Operator
hi def link mercuryCInterface       mercuryPragma
if !exists("mercury_no_highlight_foreign") || !mercury_no_highlight_foreign
  hi def link mercuryForeignId        Identifier
  hi def link mercuryCLikeBracket     mercuryBracket
  hi def link mercuryCLikeOperator    mercuryOperator
  hi def link mercuryCLikeChar        mercuryAtom
  hi def link mercuryCLikeCharEsc     mercuryStringEsc
  hi def link mercuryCLikeDelimiter   mercuryDelimiter
  hi def link mercuryCLikeKeyword     Keyword
  hi def link mercuryCLikeString      String
  hi def link mercuryCLikeNumber      Number
  hi def link mercuryCLikeFloat       Float
  hi def link mercuryCppLikeType      Type
  hi def link mercuryCLikeType        Type
  hi def link mercuryCBool            mercuryBool
  hi def link mercuryCConst           Constant
  hi def link mercuryCFunc            Identifier
  hi def link mercuryCKeyword         Keyword
  hi def link mercuryCStringFmt       mercuryStringFmt
  hi def link mercuryCType            mercuryForeignType
  hi def link mercuryCPreProc         mercuryPragma
  hi def link mercuryCppLikeBool      mercuryBool
  hi def link mercuryCppLikeConst     Constant
  hi def link mercuryCppLikeKeyword   Keyword
  hi def link mercuryCppLikeMod       mercuryAccess
  hi def link mercuryCppLikeOperator  mercuryOperator
  hi def link mercuryCString          String
  hi def link mercuryCSharpBool       mercuryBool
  hi def link mercuryCSharpString     String
  hi def link mercuryCSharpStringFmt  mercuryStringFmt
  hi def link mercuryCSharpStringFmtEsc mercuryStringEsc
  hi def link mercuryCSharpType       mercuryForeignType
  hi def link mercuryJavaBool         mercuryBool
  hi def link mercuryJavaType         mercuryForeignType
  hi def link mercuryErlangKeyword    Keyword
  hi def link mercuryErlangOperator   Operator
  hi def link mercuryErlangBool       mercuryBool
  hi def link mercuryErlangExtNumLiteral Number
  hi def link mercuryErlangString     String
  hi def link mercuryErlangLogical    mercuryLogical
  if exists("mercury_highlight_extra") && mercury_highlight_extra
    hi def link mercuryForeignType  Type
  else
    hi def link mercuryForeignType  Normal
  endif
endif
hi def link mercuryDelimiter        Delimiter
hi def link mercuryPurity           Special
hi def link mercuryImplKeyword      Identifier
hi def link mercuryKeyword          Keyword
hi def link mercuryNumCode          Number
hi def link mercuryFloat            Float
hi def link mercuryPragma           PreProc
hi def link mercuryForeignMod       mercuryForeignIface
hi def link mercuryForeignOperator  Operator
hi def link mercuryForeignIface     PreProc
hi def link mercuryImplication      Special
hi def link mercuryLogical          Special
hi def link mercuryEscErr           ErrorMsg
hi def link mercuryErrInAny         ErrorMsg
hi def link mercuryInlined          Operator
hi def link mercuryString           String
hi def link mercuryStringEsc        Identifier
hi def link mercuryStringFmt        Special
hi def link mercuryToDo             Todo
hi def link mercuryTooLong          ErrorMsg
hi def link mercuryWhitespace       mercuryTodo
hi def link mercuryTerminator       Delimiter
hi def link mercuryType             Type
if exists("mercury_highlight_extra") && mercury_highlight_extra
  hi def link mercuryOperator   Operator
else
  hi def link mercuryOperator   Normal
endif
