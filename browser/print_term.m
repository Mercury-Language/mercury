%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: print_term.m.
%
% As its name implies, this module implements printing terms
% for the Mercury debugger.
%
%---------------------------------------------------------------------------%

:- module mdb.print_term.
:- interface.

:- import_module mdb.browser_info.
:- import_module mdb.browser_term.
:- import_module mdb.parse.

:- import_module bool.
:- import_module io.
:- import_module maybe.

:- pred portray_maybe_path(debugger::in, browse_caller_type::in,
    portray_format::in, browser_info::in, maybe(path)::in,
    io::di, io::uo) is cc_multi.

:- pred portray(debugger::in, browse_caller_type::in,
    portray_format::in, browser_info::in, io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

    % Defined for and exported to term_to_html.m.
    %
:- pred browser_term_to_html_flat_string(browser_term::in, string::out,
    bool::out, io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

    % For use in representing unbound head variables in the "print goal"
    % commands in the debugger.
    %
:- type unbound
    --->    '_'.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.frame.
:- import_module mdb.sized_pretty.
:- import_module mdb.term_paths.

:- import_module deconstruct.
:- import_module int.
:- import_module io.stream_db.
:- import_module list.
:- import_module pair.
:- import_module pretty_printer.
:- import_module stream.
:- import_module stream.string_writer.
:- import_module string.
:- import_module string.builder.
:- import_module term_io.
:- import_module univ.

%---------------------------------------------------------------------------%

portray_maybe_path(Debugger, Caller, Format, Info, MaybePath, !IO) :-
    (
        MaybePath = no,
        portray(Debugger, Caller, Format, Info, !IO)
    ;
        MaybePath = yes(Path),
        portray_path(Debugger, Caller, Format, Info, Path, !IO)
    ).

:- pred portray_path(debugger::in, browse_caller_type::in,
    portray_format::in, browser_info::in, path::in,
    io::di, io::uo) is cc_multi.

portray_path(Debugger, Caller, Format, Info0, Path, !IO) :-
    set_path(Path, Info0, Info),
    portray(Debugger, Caller, Format, Info, !IO).

%---------------------------------------------------------------------------%

portray(Debugger, Caller, Format, Info, !IO) :-
    browser_info.get_format_params(Info, Caller, Format, Params),
    deref_subterm(Info ^ bri_term, Info ^ bri_dirs, SubResult),
    (
        SubResult = deref_result(BrowserTerm),
        (
            Format = flat,
            portray_flat(Debugger, BrowserTerm, Params, !IO)
        ;
            Format = raw_pretty,
            portray_raw_pretty(Debugger, BrowserTerm, Params, !IO)
        ;
            Format = verbose,
            portray_verbose(Debugger, BrowserTerm, Params, !IO)
        ;
            Format = pretty,
            portray_pretty(Debugger, BrowserTerm, Params, !IO)
        )
    ;
        SubResult = deref_error(OKPath, ErrorDir),
        report_deref_error(Debugger, OKPath, ErrorDir, !IO)
    ),
    nl_debugger(Debugger, !IO).

%---------------------------------------------------------------------------%

:- pred portray_flat(debugger::in, browser_term::in, format_params::in,
    io::di, io::uo) is cc_multi.

portray_flat(Debugger, BrowserTerm, Params, !IO) :-
    % io.write handles the special cases such as lists, operators, etc better,
    % so we prefer to use it if we can. However, io.write doesn't have
    % a depth or size limit, so we need to check the size first; if the term
    % is small enough, we use string_writer.write (actually
    % string_writer.write_univ), otherwise we use term_to_string/4.
    %
    % XXX This ignores the maximum number of lines.

    browser_term_size_left_from_max(BrowserTerm, max_print_size,
        RemainingSize),
    ( if RemainingSize >= 0 then
        portray_flat_write_browser_term(string.builder.handle, BrowserTerm,
            string.builder.init, State),
        BrowserTermStr = to_string(State)
    else
        io.stream_db.get_stream_db(StreamDb, !IO),
        BrowserDb = browser_db(StreamDb),
        browser_term_to_string(BrowserDb, BrowserTerm, Params ^ size,
            Params ^ depth, BrowserTermStr)
    ),
    write_string_debugger(Debugger, BrowserTermStr, !IO).

    % The maximum estimated size for which we use `io.write'.
    %
:- func max_print_size = int.

max_print_size = 60.

:- pred portray_flat_write_browser_term(Stream::in, browser_term::in,
    State::di, State::uo) is cc_multi
    <= (stream.writer(Stream, string, State),
        stream.writer(Stream, character, State)).

portray_flat_write_browser_term(OutputStream, BrowserTerm, !IO) :-
    (
        BrowserTerm = plain_term(Univ),
        string_writer.write_univ(OutputStream, include_details_cc, Univ, !IO)
    ;
        BrowserTerm = synthetic_term(Functor, Args, MaybeReturn),
        put(OutputStream, Functor, !IO),
        (
            Args = []
        ;
            Args = [_ | _],
            put(OutputStream, "(", !IO),
            put_list(OutputStream, write_univ_or_unbound, put_comma_space,
                Args, !IO),
            put(OutputStream, ")", !IO)
        ),
        (
            MaybeReturn = yes(Return),
            put(OutputStream, " = ", !IO),
            string_writer.write_univ(OutputStream, include_details_cc,
                Return, !IO)
        ;
            MaybeReturn = no
        )
    ).

:- pred write_univ_or_unbound(Stream::in, univ::in, State::di, State::uo)
    is cc_multi
    <= (stream.writer(Stream, string, State),
        stream.writer(Stream, character, State)).

write_univ_or_unbound(Stream, Univ, !IO) :-
    ( if univ_to_type(Univ, _ `with_type` unbound) then
        put_char(Stream, '_', !IO)
    else
        string_writer.write_univ(Stream, include_details_cc, Univ, !IO)
    ).

:- pred put_comma_space(Stream::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

put_comma_space(Stream, !State) :-
    put(Stream, ", ", !State).

%---------------------%

:- pred browser_term_to_string(browser_db::in, browser_term::in,
    int::in, int::in, string::out) is cc_multi.

browser_term_to_string(BrowserDb, BrowserTerm, MaxSize, MaxDepth, Str) :-
    CurSize = 0,
    CurDepth = 0,
    browser_term_to_string_2(BrowserDb, BrowserTerm,
        MaxSize, CurSize, _NewSize, MaxDepth, CurDepth, Str).

    % Note: When the size limit is reached, we simply display further subterms
    % compressed. This is consistent with the User's Guide, which describes
    % the size limit as a "suggested maximum".
    %
:- pred browser_term_to_string_2(browser_db::in, browser_term::in,
    int::in, int::in, int::out, int::in, int::in, string::out) is cc_multi.

browser_term_to_string_2(BrowserDb, BrowserTerm, MaxSize, CurSize, NewSize,
        MaxDepth, CurDepth, Str) :-
    limited_deconstruct_browser_term_cc(BrowserDb, BrowserTerm, MaxSize,
        MaybeFunctorArityArgs, MaybeReturn),
    ( if
        CurSize < MaxSize,
        CurDepth < MaxDepth,
        MaybeFunctorArityArgs = yes({Functor, _Arity, Args})
    then
        browser_term_to_string_3(BrowserDb, Functor, Args, MaybeReturn,
            MaxSize, CurSize, NewSize, MaxDepth, CurDepth, Str)
    else
        browser_term_compress(BrowserDb, BrowserTerm, Str),
        NewSize = CurSize
    ).

:- pred browser_term_to_string_3(browser_db::in, string::in,
    list(univ)::in, maybe(univ)::in, int::in, int::in, int::out,
    int::in, int::in, string::out) is cc_multi.

browser_term_to_string_3(BrowserDb, Functor, Args, MaybeReturn,
        MaxSize, Size0, Size, MaxDepth, Depth0, Str) :-
    ( if
        Functor = "[|]",
        Args = [ListHead, ListTail],
        MaybeReturn = no
    then
        % For the purposes of size and depth, we treat lists as if they consist
        % of one functor plus an argument for each element of the list.
        Size1 = Size0 + 1,
        Depth1 = Depth0 + 1,
        browser_term_to_string_2(BrowserDb, plain_term(ListHead),
            MaxSize, Size1, Size2, MaxDepth, Depth1, HeadStr),
        list_tail_to_string_list(BrowserDb, ListTail,
            MaxSize, Size2, Size, MaxDepth, Depth1, TailStrs),
        list.append(TailStrs, ["]"], Strs),
        string.append_list(["[", HeadStr | Strs], Str)
    else if
        Functor = "[]",
        Args = [],
        MaybeReturn = no
    then
        Size = Size0 + 1,
        Str = "[]"
    else
        Size1 = Size0 + 1,
        Depth1 = Depth0 + 1,
        args_to_string_list(BrowserDb, Args, MaxSize, Size1, Size2,
            MaxDepth, Depth1, ArgStrs),
        BracketedArgsStr = bracket_string_list(ArgStrs),
        (
            MaybeReturn = yes(Return),
            browser_term_to_string_2(BrowserDb, plain_term(Return),
                MaxSize, Size2, Size, MaxDepth, Depth1, ReturnStr),
            string.append_list([Functor, BracketedArgsStr, " = ", ReturnStr],
                Str)
        ;
            MaybeReturn = no,
            Size = Size2,
            string.append_list([Functor, BracketedArgsStr], Str)
        )
    ).

%---------------------%

:- pred list_tail_to_string_list(browser_db::in, univ::in,
    int::in, int::in, int::out, int::in, int::in, list(string)::out)
    is cc_multi.

list_tail_to_string_list(BrowserDb, TailUniv, MaxSize, Size0, Size,
        MaxDepth, Depth0, TailStrs) :-
    % We want the limit to be at least two to ensure that the limited
    % deconstruct won't fail for any list term.
    Limit = max(MaxSize, 2),
    limited_deconstruct_browser_term_cc(BrowserDb, plain_term(TailUniv),
        Limit, MaybeFunctorArityArgs, MaybeReturn),
    (
        MaybeFunctorArityArgs = yes({Functor, _Arity, Args}),
        ( if
            Functor = "[]",
            Args = [],
            MaybeReturn = no
        then
            Size = Size0,
            TailStrs = []
        else if
            Functor = "[|]",
            Args = [ListHead, ListTail],
            MaybeReturn = no
        then
            ( if
                Size0 < MaxSize,
                Depth0 < MaxDepth
            then
                browser_term_to_string_2(BrowserDb, plain_term(ListHead),
                    MaxSize, Size0, Size1, MaxDepth, Depth0, HeadStr),
                list_tail_to_string_list(BrowserDb, ListTail, MaxSize,
                    Size1, Size, MaxDepth, Depth0, TailStrs0),
                TailStrs = [", ", HeadStr | TailStrs0]
            else
                Size = Size0,
                TailStrs = [", ..."]
            )
        else
            ( if
                Size0 < MaxSize,
                Depth0 < MaxDepth
            then
                browser_term_to_string_3(BrowserDb, Functor, Args, MaybeReturn,
                    MaxSize, Size0, Size, MaxDepth, Depth0, TailStr),
                TailStrs = [" | ", TailStr]
            else
                Size = Size0,
                browser_term_compress(BrowserDb, plain_term(TailUniv),
                    TailCompressedStr),
                TailStrs = [" | ", TailCompressedStr]
            )
        )
    ;
        MaybeFunctorArityArgs = no,
        Size = Size0,
        browser_term_compress(BrowserDb, plain_term(TailUniv),
            TailCompressedStr),
        TailStrs = [" | ", TailCompressedStr]
    ).

%---------------------%

:- pred args_to_string_list(browser_db::in, list(univ)::in,
    int::in, int::in, int::out, int::in, int::in, list(string)::out)
    is cc_multi.

args_to_string_list(_BrowserDb, [], _MaxSize, CurSize, NewSize,
        _MaxDepth, _CurDepth, Strs) :-
    Strs = [],
    NewSize = CurSize.
args_to_string_list(BrowserDb, [Univ | Univs], MaxSize, CurSize, NewSize,
        MaxDepth, CurDepth, Strs) :-
    browser_term_to_string_2(BrowserDb, plain_term(Univ),
        MaxSize, CurSize, NewSize1, MaxDepth, CurDepth, Str),
    args_to_string_list(BrowserDb, Univs, MaxSize, NewSize1, NewSize,
        MaxDepth, CurDepth, RestStrs),
    Strs = [Str | RestStrs].

:- func bracket_string_list(list(string)) = string.

bracket_string_list(Args) = Str :-
    (
        Args = [],
        Str = ""
    ;
        Args = [HeadArg | TailArgs],
        Str = "(" ++ HeadArg ++ comma_string_list(TailArgs) ++ ")"
    ).

:- func comma_string_list(list(string)) = string.

comma_string_list([]) = "".
comma_string_list([HeadArg | TailArgs]) =
    ", " ++ HeadArg ++ comma_string_list(TailArgs).

%---------------------%

:- pred browser_term_compress(browser_db::in, browser_term::in, string::out)
    is cc_multi.

browser_term_compress(BrowserDb, BrowserTerm, Str) :-
    functor_browser_term_cc(BrowserDb, BrowserTerm, Functor, Arity, IsFunc),
    ( if Arity = 0 then
        Str = Functor
    else
        (
            IsFunc = yes,
            string.format("%s/%d+1", [s(Functor), i(Arity)], Str)
        ;
            IsFunc = no,
            string.format("%s/%d", [s(Functor), i(Arity)], Str)
        )
    ).

%---------------------------------------------------------------------------%

:- pred portray_verbose(debugger::in, browser_term::in, format_params::in,
    io::di, io::uo) is cc_multi.

portray_verbose(Debugger, BrowserTerm, Params, !IO) :-
    io.stream_db.get_stream_db(StreamDb, !IO),
    BrowserDb = browser_db(StreamDb),
    browser_term_to_string_verbose(BrowserDb, BrowserTerm, Params ^ size,
        Params ^ depth, Params ^ width, Params ^ lines, Str),
    write_string_debugger(Debugger, Str, !IO).

    % Verbose printing. Tree layout with numbered branches.
    % Numbering makes it easier to change to subterms.
    %
:- pred browser_term_to_string_verbose(browser_db::in, browser_term::in,
    int::in, int::in, int::in, int::in, string::out) is cc_multi.

browser_term_to_string_verbose(BrowserDb, BrowserTerm, MaxSize, MaxDepth,
        X, Y, Str) :-
    CurSize = 0,
    CurDepth = 0,
    browser_term_to_string_verbose_2(BrowserDb, BrowserTerm,
        MaxSize, CurSize, _NewSize, MaxDepth, CurDepth, Frame),
    ClippedFrame = frame.clip(X-Y, Frame),
    unlines(ClippedFrame, Str).

:- pred browser_term_to_string_verbose_2(browser_db::in, browser_term::in,
    int::in, int::in, int::out, int::in, int::in, frame::out) is cc_multi.

browser_term_to_string_verbose_2(BrowserDb, BrowserTerm,
        MaxSize, CurSize, NewSize, MaxDepth, CurDepth, Frame) :-
    limited_deconstruct_browser_term_cc(BrowserDb, BrowserTerm, MaxSize,
        MaybeFunctorArityArgs, MaybeReturn),
    ( if
        CurSize < MaxSize,
        CurDepth < MaxDepth,
        MaybeFunctorArityArgs = yes({Functor, _Arity, Args0})
    then
        % XXX We should consider formatting function terms differently.
        (
            MaybeReturn = yes(Return),
            list.append(Args0, [Return], Args)
        ;
            MaybeReturn = no,
            Args = Args0
        ),
        CurSize1 = CurSize + 1,
        CurDepth1 = CurDepth + 1,
        ArgNum = 1,
        args_to_string_verbose_list(BrowserDb, Args, ArgNum,
            MaxSize, CurSize1, NewSize, MaxDepth, CurDepth1, ArgsFrame),
        Frame = frame.vglue([Functor], ArgsFrame)
    else
        browser_term_compress(BrowserDb, BrowserTerm, Line),
        Frame = [Line],
        NewSize = CurSize
    ).

:- pred args_to_string_verbose_list(browser_db::in, list(univ)::in,
    int::in, int::in, int::in, int::out, int::in, int::in, frame::out)
    is cc_multi.

args_to_string_verbose_list(_BrowserDb, [], _ArgNum,
        _MaxSize, CurSize, NewSize, _MaxDepth, _CurDepth, []) :-
    NewSize = CurSize.
args_to_string_verbose_list(BrowserDb, [Univ], ArgNum,
        MaxSize, CurSize, NewSize, MaxDepth, CurDepth, Frame) :-
    browser_term_to_string_verbose_2(BrowserDb, plain_term(Univ), MaxSize,
        CurSize, NewSize, MaxDepth, CurDepth, TreeFrame),
    % XXX: ArgNumS must have fixed length 2.
    string.int_to_string(ArgNum, ArgNumS),
    string.append_list([ArgNumS, "-"], LastBranchS),
    Frame = frame.hglue([LastBranchS], TreeFrame).
args_to_string_verbose_list(BrowserDb, [Univ1, Univ2 | Univs], ArgNum, MaxSize,
        CurSize, NewSize, MaxDepth, CurDepth, Frame) :-
    browser_term_to_string_verbose_2(BrowserDb, plain_term(Univ1),
        MaxSize, CurSize, NewSize1, MaxDepth, CurDepth, TreeFrame),
    ArgNum1 = ArgNum + 1,
    args_to_string_verbose_list(BrowserDb, [Univ2 | Univs], ArgNum1,
        MaxSize, NewSize1, NewSize2, MaxDepth, CurDepth, RestTreesFrame),
    NewSize = NewSize2,
    % XXX: ArgNumS must have fixed length 2.
    string.int_to_string(ArgNum, ArgNumS),
    string.append_list([ArgNumS, "-"], BranchFrameS),
    Height = frame.vsize(TreeFrame) - 1,
    list.duplicate(Height, "|", VBranchFrame),
    LeftFrame = frame.vglue([BranchFrameS], VBranchFrame),
    TopFrame = frame.hglue(LeftFrame, TreeFrame),
    Frame = frame.vglue(TopFrame, RestTreesFrame).

:- pred unlines(list(string)::in, string::out) is det.

unlines([], "").
unlines([Line | Lines], Str) :-
    string.append(Line, "\n", NLine),
    unlines(Lines, Strs),
    string.append(NLine, Strs, Str).

%---------------------------------------------------------------------------%

:- pred portray_pretty(debugger::in, browser_term::in, format_params::in,
    io::di, io::uo) is det.

portray_pretty(Debugger, BrowserTerm, Params, !IO) :-
    browser_term_to_string_pretty(Debugger, BrowserTerm, Params ^ width,
        Params ^ lines, Params ^ size, Params ^ depth, !IO).

    % Print using the pretty printer from the standard library.
    % XXX Because the pretty printer doesn't support a combination
    % of both size and depth, we use the depth, except when depth is 0,
    % in which case we use the size.
    %
:- pred browser_term_to_string_pretty(S::in, browser_term::in,
    int::in, int::in, int::in, int::in, io::di, io::uo) is det
    <= stream.writer(S, string, io).

browser_term_to_string_pretty(S, Term, Width, Lines, Size, Depth, !IO) :-
    (
        Term = plain_term(Univ),
        Doc = format_univ(Univ)
    ;
        Term = synthetic_term(Functor, Args, MaybeReturn),
        Doc = synthetic_term_to_doc(Functor, Args, MaybeReturn)
    ),
    get_default_formatter_map(Formatters, !IO),
    ( if Depth > 0 then
        Limit = triangular(Depth)
    else
        Limit = linear(Size)
    ),
    Params = pp_params(Width, Lines, Limit),
    promise_equivalent_solutions [!:IO] (
        put_doc(S, include_details_cc, Formatters, Params, Doc, !IO)
    ).

    % These two functions are just like pprint.to_doc, except their input
    % is not a natural term, but a synthetic term defined by a functor, a list
    % of arguments, and if the synthetic term is a function application, then
    % the result of that function application.
    %
    % The functor name has to be treated specially because '.'s therein
    % usually denote separators in a module qualified name; the
    % default pretty_printer formatter does not know this and will quote
    % such names.
    %
:- func synthetic_term_to_doc(string, list(univ), maybe(univ)) = doc.

synthetic_term_to_doc(Functor0, Args, MaybeReturn) = Doc :-
    ( if
        ( Functor0 = "!."
        ; Functor0 = "."
        ; Functor0 = ".."
        ; Functor0 = "=.."
        ; not string.contains_char(Functor0, ('.'))
        )
    then
        Doc0 = format_term(Functor0, Args)
    else
        FunctorDoc =
            qualified_functor_to_doc(string.split_at_char(('.'), Functor0)),
        (
            Args = [],
            Doc0 = FunctorDoc
        ;
            Args = [_ | _],
            Doc0 = indent([
                FunctorDoc, str("("),
                    format_list(Args, group([str(", "), nl])),
                str(")")
            ])
        )
    ),
    (
        MaybeReturn = no,
        Doc = Doc0
    ;
        MaybeReturn = yes(Return),
        Doc = docs([Doc0, str(" = "), format_arg(format_univ(Return))])
    ).

:- func qualified_functor_to_doc(list(string)) = doc.

qualified_functor_to_doc([]) = str("").
qualified_functor_to_doc([Part]) = str(term_io.quoted_atom(Part)).
qualified_functor_to_doc([PartA, PartB | Parts]) =
    docs([str(term_io.quoted_atom(PartA)), str("."),
        qualified_functor_to_doc([PartB | Parts])]).

%---------------------------------------------------------------------------%

:- pred portray_raw_pretty(debugger::in, browser_term::in, format_params::in,
    io::di, io::uo) is cc_multi.

portray_raw_pretty(Debugger, BrowserTerm, Params, !IO) :-
    io.stream_db.get_stream_db(StreamDb, !IO),
    BrowserDb = browser_db(StreamDb),
    sized_pretty.browser_term_to_string_line(BrowserDb, BrowserTerm,
        Params ^ width, Params ^ lines, Str),
    write_string_debugger(Debugger, Str, !IO).

%---------------------------------------------------------------------------%

browser_term_to_html_flat_string(BrowserTerm, Str, Elided, !IO) :-
    % Mimic portray_flat. We can afford larger sizes in a web browser
    % due to proportional fonts and horizontal scrolling.
    MaxTermSize = 120,
    browser_term_size_left_from_max(BrowserTerm, MaxTermSize, RemainingSize),
    ( if RemainingSize >= 0 then
        portray_flat_write_browser_term(string.builder.handle, BrowserTerm,
            string.builder.init, State),
        Str = to_string(State),
        Elided = no
    else
        io.stream_db.get_stream_db(StreamDb, !IO),
        BrowserDb = browser_db(StreamDb),
        MaxSize = 10,
        MaxDepth = 5,
        browser_term_to_string(BrowserDb, BrowserTerm, MaxSize, MaxDepth, Str),
        Elided = yes
    ).

%---------------------------------------------------------------------------%

    % Estimate the total term size, in characters, We count the number of
    % characters in the functor, plus two characters for each argument:
    % "(" and ")" for the first, and ", " for each of the rest, plus the
    % sizes of the arguments themselves. This is only approximate since it
    % doesn't take into account all the special cases such as operators.
    %
    % This predicate returns not the estimated total term size,
    % but the difference between the given maximum size the caller
    % is interested in and the estimated total term size.
    % This difference is positive if the term is smaller than the
    % maximum and negative if it is bigger. If the difference is
    % negative, term_size_left_from_max will return a negative difference
    % but the value will usually not be accurate, since in such cases
    % by definition the caller is not interested in the accurate value.
    %
:- pred browser_term_size_left_from_max(browser_term::in,
    int::in, int::out) is cc_multi.

browser_term_size_left_from_max(BrowserTerm, MaxSize, RemainingSize) :-
    (
        BrowserTerm = plain_term(Univ),
        term_size_left_from_max(Univ, MaxSize, RemainingSize)
    ;
        BrowserTerm = synthetic_term(Functor, Args, MaybeReturn),
        string.length(Functor, FunctorSize),
        list.length(Args, Arity),
        (
            MaybeReturn = yes(_),
            % "()", " = ", plus Arity-1 times ", "
            PrincipalSize = FunctorSize + Arity * 2 + 3
        ;
            MaybeReturn = no,
            % "()", plus Arity-1 times ", "
            PrincipalSize = FunctorSize + Arity * 2
        ),
        MaxArgsSize = MaxSize - PrincipalSize,
        list.foldl(term_size_left_from_max, Args, MaxArgsSize, RemainingSize)
    ).

:- pred term_size_left_from_max(univ::in, int::in, int::out) is cc_multi.

term_size_left_from_max(Univ, MaxSize, RemainingSize) :-
    ( if MaxSize < 0 then
        RemainingSize = MaxSize
    else
        deconstruct.limited_deconstruct_cc(univ_value(Univ), MaxSize,
            MaybeFunctorArityArgs),
        (
            MaybeFunctorArityArgs = yes({Functor, Arity, Args}),
            string.length(Functor, FunctorSize),
            % "()", plus Arity-1 times ", "
            PrincipalSize = FunctorSize + Arity * 2,
            MaxArgsSize = MaxSize - PrincipalSize,
            list.foldl(term_size_left_from_max, Args,
                MaxArgsSize, RemainingSize)
        ;
            MaybeFunctorArityArgs = no,
            RemainingSize = -1
        )
    ;
        RemainingSize = -1
    ).

%---------------------------------------------------------------------------%
:- end_module mdb.print_term.
%---------------------------------------------------------------------------%
