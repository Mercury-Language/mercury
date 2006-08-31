%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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

%---------------------------------------------------------------------------%

% Uncomment these and the unsafe_perform_ios below to debug match_with_cterm
% and its callers in the trace directory.
% :- import_module io, unsafe.
% :- pragma promise_pure(match_with_cterm/3).

match_with_cterm(Term, CTerm, Match) :-
    deconstruct(Term, include_details_cc, TermFunctor, _, TermArgs),
    cterm_deconstruct(CTerm, CTermFunctor, CTermArgs),
    % impure unsafe_perform_io(io.write_string("comparing <")),
    % impure unsafe_perform_io(io.write_string(TermFunctor)),
    % impure unsafe_perform_io(io.write_string("> to <")),
    % impure unsafe_perform_io(io.write_string(CTermFunctor)),
    % impure unsafe_perform_io(io.write_string(">\n")),
    ( TermFunctor = CTermFunctor ->
        match_with_cterms(TermArgs, CTermArgs, Match)
    ; CTermFunctor = "_" ->
        Match = yes
    ;
        Match = no
    ).

:- pred match_with_cterms(list(univ)::in, cargs::in, bool::out) is cc_multi.

match_with_cterms(UnivArgs, CArgs, Match) :-
    ( cterm_head_tail(CArgs, CHead, CTail) ->
        ( UnivArgs = [UnivHead | UnivTail] ->
            Head = univ_value(UnivHead),
            match_with_cterm(Head, CHead, MatchHead),
            (
                MatchHead = no,
                Match = no
            ;
                MatchHead = yes,
                match_with_cterms(UnivTail, CTail, Match)
            )
        ;
            Match = no
        )
    ;
        ( UnivArgs = [] ->
            Match = yes
        ;
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

    Functor = Term->term_functor;
    Args = Term->term_args;
").

:- pragma foreign_proc(c,
    cterm_head_tail(Args::in, Head::out, Tail::out),
    [will_not_call_mercury, promise_pure],
"
    if (Args == NULL) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        Head = Args->args_head;
        Tail = Args->args_tail;
        SUCCESS_INDICATOR = MR_TRUE;
    }
").
