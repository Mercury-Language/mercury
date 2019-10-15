%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_util.m.
% Main author: fjh.
%
% This module defines the types used by parse_module.m and its subcontractors
% to return the results of parsing, and some utility predicates needed
% by several of the parser modules.
%
% Most parsing predicates must check for errors. They return either the
% item(s) they were looking for, or an error indication.
%
% Most of the parsing predicates return a `maybe1(T)' or a `maybe2(T1, T2)',
% which will either be the `ok(ParseTree)' (or `ok(ParseTree1, ParseTree2)'),
% if the parse is successful, or `error(Message, Term)' if it is not.
% The `Term' there should be the term which is syntactically incorrect.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_util.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.

:- import_module cord.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- pred parse_implicitly_qualified_name_and_arity(module_name::in, term(T)::in,
    sym_name::out, arity::out) is semidet.

:- pred parse_unqualified_name_and_arity(term(T)::in,
    sym_name::out, arity::out) is semidet.

:- pred parse_pred_or_func_name_and_arity(term(T)::in,
    pred_or_func::out, sym_name::out, arity::out) is semidet.

%---------------------------------------------------------------------------%

    % Either ok2(SymName, Args - MaybeFuncRetArg) or error2(Specs).
:- type maybe_pred_or_func(T) == maybe2(sym_name, pair(list(T), maybe(T))).

:- pred parse_pred_or_func_and_args(term(T)::in,
    pred_or_func::out, sym_name::out, list(term(T))::out) is semidet.

:- pred parse_pred_or_func_and_args_general(maybe(module_name)::in,
    term(T)::in, varset(T)::in, cord(format_component)::in,
    maybe_pred_or_func(term(T))::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred list_term_to_term_list(term::in, list(term)::out) is semidet.

%---------------------------------------------------------------------------%

    % Convert a "disjunction" (bunch of terms separated by ';'s) to a list.
    %
:- pred disjunction_to_one_or_more(term(T)::in, one_or_more(term(T))::out)
    is det.
:- pred disjunction_to_list(term(T)::in, list(term(T))::out) is det.

    % Convert a "conjunction" (bunch of terms separated by ','s) to a list.
    %
:- pred conjunction_to_one_or_more(term(T)::in, one_or_more(term(T))::out)
    is det.
:- pred conjunction_to_list(term(T)::in, list(term(T))::out) is det.

    % one_or_more_to_conjunction(Context, List, Term):
    %
    % Convert a nonempty list to a "conjunction", i.e. a bunch of terms
    % separated by commas.
    %
:- pred one_or_more_to_conjunction(prog_context::in, one_or_more(term(T))::in,
    term(T)::out) is det.

    % Convert a "sum", i.e. a bunch of terms separated by '+' operators
    % to a nonempty list.
    %
:- pred sum_to_one_or_more(term(T)::in, one_or_more(term(T))::out) is det.
:- pred sum_to_list(term(T)::in, list(term(T))::out) is det.

%---------------------------------------------------------------------------%

:- type parser(T) == pred(term, maybe1(T)).
:- mode parser == (pred(in, out) is det).

    % Parse a comma-separated list (misleading described as a "conjunction")
    % of things.
    %
:- pred parse_one_or_more(parser(T)::parser, term::in,
    maybe1(one_or_more(T))::out) is det.
:- pred parse_list(parser(T)::parser, term::in,
    maybe1(list(T))::out) is det.

:- pred map_parser(parser(T)::parser, list(term)::in, maybe1(list(T))::out)
    is det.

%---------------------------------------------------------------------------%

    % A value of this type such as
    %
    %   conflict(single_prec_float, double_prec_float,
    %       "floats cannot be both single- and double-precision")
    %
    % gives two different options that may not be specified together
    % in a list of options, together with the error message to print
    % if a user nevertheless does specify them together.
    %
:- type conflict(T)
    --->    conflict(T, T, string).

    % report_any_conflicts(Context, ConflictingWhatInWhat, Conflicts,
    %   Specified, Spec):
    %
    % For each pair of elements in Specified that Conflicts says should
    % *not* be present together, generate an error message from the third
    % field of the relevent member of Conflicts.
    %
:- pred report_any_conflicts(prog_context::in, string::in,
    list(conflict(T))::in, list(T)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.parse_sym_name.

%---------------------------------------------------------------------------%

parse_implicitly_qualified_name_and_arity(ModuleName, PredAndArityTerm,
        SymName, Arity) :-
    PredAndArityTerm = term.functor(term.atom("/"),
        [PredNameTerm, ArityTerm], _),
    try_parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
        PredNameTerm, SymName),
    decimal_term_to_int(ArityTerm, Arity).

parse_unqualified_name_and_arity(PredAndArityTerm, SymName, Arity) :-
    parse_implicitly_qualified_name_and_arity(unqualified(""),
        PredAndArityTerm, SymName, Arity).

parse_pred_or_func_name_and_arity(PorFPredAndArityTerm,
        PredOrFunc, SymName, Arity) :-
    PorFPredAndArityTerm = term.functor(term.atom(PredOrFuncStr), Args, _),
    ( PredOrFuncStr = "pred", PredOrFunc = pf_predicate
    ; PredOrFuncStr = "func", PredOrFunc = pf_function
    ),
    Args = [Arg],
    ModuleName = unqualified(""),
    parse_implicitly_qualified_name_and_arity(ModuleName, Arg, SymName, Arity).

%---------------------------------------------------------------------------%

parse_pred_or_func_and_args(PredAndArgsTerm, PredOrFunc, SymName, ArgTerms) :-
    ( if
        PredAndArgsTerm = term.functor(term.atom("="),
            [FuncAndArgsTerm, FuncResultTerm], _)
    then
        try_parse_sym_name_and_args(FuncAndArgsTerm, SymName, ArgTerms0),
        PredOrFunc = pf_function,
        ArgTerms = ArgTerms0 ++ [FuncResultTerm]
    else
        try_parse_sym_name_and_args(PredAndArgsTerm, SymName, ArgTerms),
        PredOrFunc = pf_predicate
    ).

parse_pred_or_func_and_args_general(MaybeModuleName, PredAndArgsTerm,
        VarSet, ContextPieces, PredAndArgsResult) :-
    ( if
        PredAndArgsTerm = term.functor(term.atom("="),
            [FuncAndArgsTerm, FuncResultTerm], _)
    then
        FunctorTerm = FuncAndArgsTerm,
        MaybeFuncResult = yes(FuncResultTerm)
    else
        FunctorTerm = PredAndArgsTerm,
        MaybeFuncResult = no
    ),
    varset.coerce(VarSet, GenericVarSet),
    (
        MaybeModuleName = yes(ModuleName),
        parse_implicitly_qualified_sym_name_and_args(ModuleName, FunctorTerm,
            GenericVarSet, ContextPieces, Result)
    ;
        MaybeModuleName = no,
        parse_sym_name_and_args(GenericVarSet, ContextPieces,
            FunctorTerm, Result)
    ),
    (
        Result = ok2(SymName, Args),
        PredAndArgsResult = ok2(SymName, Args - MaybeFuncResult)
    ;
        Result = error2(Specs),
        PredAndArgsResult = error2(Specs)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

list_term_to_term_list(Term, Terms) :-
    (
        Term = term.functor(term.atom("[|]"), [HeadTerm, TailTerm], _),
        list_term_to_term_list(TailTerm, TailTerms),
        Terms = [HeadTerm | TailTerms]
    ;
        Term = term.functor(term.atom("[]"), [], _),
        Terms = []
    ).

%---------------------------------------------------------------------------%

disjunction_to_one_or_more(Term, OneOrMore) :-
    binop_term_to_one_or_more(";", Term, OneOrMore).

disjunction_to_list(Term, List) :-
    binop_term_to_one_or_more(";", Term, one_or_more(Head, Tail)),
    List = [Head | Tail].

conjunction_to_one_or_more(Term, OneOrMore) :-
    binop_term_to_one_or_more(",", Term, OneOrMore).

conjunction_to_list(Term, List) :-
    binop_term_to_one_or_more(",", Term, one_or_more(Head, Tail)),
    List = [Head | Tail].

one_or_more_to_conjunction(_, one_or_more(Term, []), Term).
one_or_more_to_conjunction(Context, one_or_more(First, [Second | Rest]),
        Term) :-
    one_or_more_to_conjunction(Context, one_or_more(Second, Rest), Tail),
    Term = term.functor(term.atom(","), [First, Tail], Context).

sum_to_one_or_more(Term, OneOrMore) :-
    binop_term_to_one_or_more("+", Term, OneOrMore).

sum_to_list(Term, List) :-
    binop_term_to_one_or_more("+", Term, one_or_more(Head, Tail)),
    List = [Head | Tail].

    % General predicate to convert terms separated by any specified operator
    % into a list.
    %
:- pred binop_term_to_one_or_more(string::in, term(T)::in,
    one_or_more(term(T))::out) is det.

binop_term_to_one_or_more(Op, Term, OneOrMore) :-
    binop_term_to_one_or_more_loop(Op, Term, [], OneOrMore).

:- pred binop_term_to_one_or_more_loop(string::in, term(T)::in,
    list(term(T))::in, one_or_more(term(T))::out) is det.

binop_term_to_one_or_more_loop(Op, Term, List0, OneOrMore) :-
    ( if Term = term.functor(term.atom(Op), [L, R], _Context) then
        binop_term_to_one_or_more_loop(Op, R, List0,
            one_or_more(RHead, RTail)),
        binop_term_to_one_or_more_loop(Op, L, [RHead | RTail], OneOrMore)
    else
        OneOrMore = one_or_more(Term, List0)
    ).

%---------------------------------------------------------------------------%

parse_one_or_more(Parser, Term, Result) :-
    conjunction_to_one_or_more(Term, one_or_more(Head, Tail)),
    map_parser_one_or_more(Parser, Head, Tail, Result).

parse_list(Parser, Term, Result) :-
    conjunction_to_list(Term, List),
    map_parser(Parser, List, Result).

:- pred map_parser_one_or_more(parser(T)::parser, term::in, list(term)::in,
    maybe1(one_or_more(T))::out) is det.

map_parser_one_or_more(Parser, Head, Tail, Result) :-
    call(Parser, Head, HeadResult),
    (
        Tail = [],
        (
            HeadResult = error1(Specs),
            Result = error1(Specs)
        ;
            HeadResult = ok1(Item),
            Result = ok1(one_or_more(Item, []))
        )
    ;
        Tail = [HeadTail | TailTail],
        map_parser_one_or_more(Parser, HeadTail, TailTail, TailResult),
        (
            HeadResult = error1(HeadSpecs),
            TailResult = error1(TailSpecs),
            Result = error1(HeadSpecs ++ TailSpecs)
        ;
            HeadResult = error1(Specs),
            TailResult = ok1(_),
            Result = error1(Specs)
        ;
            HeadResult = ok1(_),
            TailResult = error1(Specs),
            Result = error1(Specs)
        ;
            HeadResult = ok1(HeadItem),
            TailResult = ok1(TailItems),
            Result = ok1(one_or_more_cons(HeadItem, TailItems))
        )
    ).

map_parser(_, [], ok1([])).
map_parser(Parser, [Head | Tail], Result) :-
    call(Parser, Head, HeadResult),
    map_parser(Parser, Tail, TailResult),
    (
        HeadResult = error1(HeadSpecs),
        TailResult = error1(TailSpecs),
        Result = error1(HeadSpecs ++ TailSpecs)
    ;
        HeadResult = error1(Specs),
        TailResult = ok1(_),
        Result = error1(Specs)
    ;
        HeadResult = ok1(_),
        TailResult = error1(Specs),
        Result = error1(Specs)
    ;
        HeadResult = ok1(HeadItem),
        TailResult = ok1(TailItems),
        Result = ok1([HeadItem | TailItems])
    ).

%---------------------------------------------------------------------------%

report_any_conflicts(Context, ConflictingWhatInWhat, Conflicts, Specified,
        Specs) :-
    list.foldl(
        accumulate_conflict_specs(Context, ConflictingWhatInWhat, Specified),
        Conflicts, [], Specs).

:- pred accumulate_conflict_specs(prog_context::in, string::in,
    list(T)::in, conflict(T)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

accumulate_conflict_specs(Context, ConflictingWhatInWhat, Specified,
        Conflict, !Specs) :-
    Conflict = conflict(A, B, Diagnosis),
    ( if
        list.member(A, Specified),
        list.member(B, Specified)
    then
        Pieces = [words("Error:"), words(ConflictingWhatInWhat),
            suffix(":"), nl, words(Diagnosis), suffix("."), nl],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_util.
%---------------------------------------------------------------------------%
