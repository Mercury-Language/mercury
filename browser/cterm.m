%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2007 The University of Melbourne.
% Copyright (C) 2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: cterm.m.
% Author: zs.
%
% This module provides a mechanism for matching terms from the running program
% against terms specified by debugger commands, which are implemented in C in
% runtime/mercury_trace_term.[ch].
%
%---------------------------------------------------------------------------%

:- module mdb.cterm.
:- interface.

:- import_module bool.

%---------------------------------------------------------------------------%

:- type cterm.
:- type cargs.

    % Succeed if and only if the given term matches the given cterm.
    %
:- pred match_with_cterm(T::in, cterm::in, bool::out) is cc_multi.

    % Implement deconstruct for cterms.
    %
:- pred cterm_deconstruct(cterm::in, string::out, cargs::out) is det.

    % Decompose a list of arguments into the first element and the rest.
    % Fail if the list is empty.
    %
:- pred cterm_head_tail(cargs::in, cterm::out, cargs::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module deconstruct.
:- import_module list.
:- import_module univ.

%---------------------------------------------------------------------------%

:- pragma foreign_decl(c, "
#include ""mercury_trace_term.h""
").

:- pragma foreign_type(c, cterm, "MR_CTerm", [can_pass_as_mercury_type]).
:- pragma foreign_type(c, cargs, "MR_CArgs", [can_pass_as_mercury_type]).

:- pragma foreign_export("C", match_with_cterm(in, in, out),
    "ML_BROWSE_match_with_cterm").

% Dummy types form non-C backends.
:- type cterm ---> cterm.
:- type cargs ---> cargs.

%---------------------------------------------------------------------------%

% Uncomment these and the unsafe_perform_ios below to debug match_with_cterm
% and its callers in the trace directory.
% :- import_module io.
% :- import_module unsafe.
% :- pragma promise_pure(match_with_cterm/3).

match_with_cterm(Term, CTerm, Match) :-
    deconstruct(Term, include_details_cc, TermFunctor, _, TermArgs),
    cterm_deconstruct(CTerm, CTermFunctor, CTermArgs),
    ( if CTermFunctor = TermFunctor then
        match_with_cterms(TermArgs, CTermArgs, Match)
    else if CTermFunctor = "_" then
        Match = yes
    else
        Match = no
    ).

:- pred match_with_cterms(list(univ)::in, cargs::in, bool::out) is cc_multi.

match_with_cterms(UnivArgs, CArgs, Match) :-
    ( if cterm_head_tail(CArgs, CHead, CTail) then
        (
            UnivArgs = [],
            Match = no
        ;
            UnivArgs = [UnivHead | UnivTail],
            Head = univ_value(UnivHead),
            match_with_cterm(Head, CHead, MatchHead),
            (
                MatchHead = no,
                Match = no
            ;
                MatchHead = yes,
                match_with_cterms(UnivTail, CTail, Match)
            )
        )
    else
        (
            UnivArgs = [],
            Match = yes
        ;
            UnivArgs = [_ | _],
            Match = no
        )
    ).

:- pragma foreign_proc(c,
    cterm_deconstruct(Term::in, Functor::out, Args::out),
    [will_not_call_mercury, promise_pure],
"
    if (Term == NULL) {
        MR_fatal_error(""cterm_deconstruct: NULL term"");
    }

    Functor = Term->MR_term_functor;
    Args = Term->MR_term_args;
").

:- pragma foreign_proc("Java",
    cterm_deconstruct(_Term::in, _Functor::out, _Args::out),
    [will_not_call_mercury, promise_pure],
"
    if (1 == 1) throw new Error(\"not supported in java grade\");
").

:- pragma foreign_proc(c,
    cterm_head_tail(Args::in, Head::out, Tail::out),
    [will_not_call_mercury, promise_pure],
"
    if (Args == NULL) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        Head = Args->MR_args_head;
        Tail = Args->MR_args_tail;
        SUCCESS_INDICATOR = MR_TRUE;
    }
").

:- pragma foreign_proc("Java",
    cterm_head_tail(_Args::in, _Head::out, _Tail::out),
    [will_not_call_mercury, promise_pure],
"
    if (1 == 1) throw new Error(\"not supported in java grade\");
").
