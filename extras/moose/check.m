%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003, 2006, 2011 The University of Melbourne.
% Copyright (C) 2014, 2016, 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%---------------------------------------------------------------------------%
%
% file: check.m
% main author: conway,  November 1998
%
% This module implements various checking predicates for checking the
% input to moose. It checks for the following things:
%   - duplicate rule declarations.
%   - declared rules with no productions.
%   - productions with no rule declaraion.
%   - nonterminals with no rule declaraion.
%   - productions that are not connected to the start rule.
%   - productions that have no finite derivations.
%
% Unfortunately, we don't do anything about these yet. We should attempt
% to correct these errors so that we can look for later errors.
%
%---------------------------------------------------------------------------%

:- module check.
:- interface.

:- import_module grammar.

:- import_module io.
:- import_module list.
:- import_module term.

%---------------------------------------------------------------------------%

:- type error
    --->    error(list(string), context).

:- pred check_rule_decls(list(rule_decl)::in,
    rule_decls::out, list(check.error)::out) is det.

:- pred check_clauses(list(clause)::in, rule_decls::in,
    clauses::out, list(check.error)::out) is det.

:- pred check_useless(nonterminal::in, clauses::in, rule_decls::in,
    list(check.error)::out) is det.

:- pred check_inf_derivations(clauses::in, rule_decls::in,
    list(check.error)::out) is det.

    % Write an error message to stderr.
    %
:- pred write_error(check.error::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

check_rule_decls(DeclList, Decls, Errors) :-
    map.init(Decls0),
    check_rule_decls(DeclList, Decls0, Decls, Errors).

:- pred check_rule_decls(list(rule_decl)::in,
    rule_decls::in, rule_decls::out, list(check.error)::out) is det.

check_rule_decls([], !Decls, []).
check_rule_decls([Decl | DeclList], !Decls, Errors) :-
    Decl = rule(DeclId, _Args, _VarSet, DeclContext),
    % Look to see if we already have a declaration for this rule.
    ( if map.search(!.Decls, DeclId, PrevDecl) then
        PrevDecl = rule(_, _, _, PrevDeclContext),
        id(DeclId, Name, Arity),
        string.format("The previous declaration for %s/%d is here.",
            [s(Name), i(Arity)], Msg0),
        Err0 = error([Msg0], PrevDeclContext),
        string.format("Duplicate declaration for %s/%d.",
            [s(Name), i(Arity)], Msg1),
        Err1 = error([Msg1], DeclContext),
        Errors = [Err0, Err1 | Errors0],
        check_rule_decls(DeclList, !Decls, Errors0)
    else
        map.set(DeclId, Decl, !Decls),
        check_rule_decls(DeclList, !Decls, Errors)
    ).

%---------------------------------------------------------------------------%

check_clauses(ClauseList, Decls, Clauses, Errors) :-
    map.init(Clauses0),
    check_clauses0(ClauseList, Decls, Clauses0, Clauses, CheckClausesErrors),

    map.keys(Decls, DeclIds),
    set.sorted_list_to_set(DeclIds, DeclSet),
    map.keys(Clauses, ClauseIds),
    set.sorted_list_to_set(ClauseIds, ClauseSet),
    NoDeclSet = ClauseSet `set.difference` DeclSet,
    NoClauseSet = DeclSet `set.difference` ClauseSet,

    % Productions that have no rule declaration.
    set.to_sorted_list(NoDeclSet, NoDeclList),
    GenerateNoDeclError =
        ( pred(NoDeclId::in, NoDeclError::out) is det :-
            map.lookup(Clauses, NoDeclId, NoDeclClauseList),
            (
                NoDeclClauseList = [clause(_, _, _, NoDeclContext) | _],
                id(NoDeclId, NoDeclName, NoDeclArity),
                string.format("No rule declaration for %s/%d.",
                    [s(NoDeclName), i(NoDeclArity)], NoDeclMsg),
                NoDeclError = error([NoDeclMsg], NoDeclContext)
            ;
                NoDeclClauseList = [],
                error("check_clauses: no clause ids")
            )
        ),
    list.map(GenerateNoDeclError, NoDeclList, NoDeclErrors),

    % Rules that have no productions.
    set.to_sorted_list(NoClauseSet, NoClauseList),

    GenerateNoClauseError =
        ( pred(NoClauseId::in, NoClauseError::out) is det :-
            map.lookup(Decls, NoClauseId, Decl),
            Decl = rule(_, _, _, NoClauseContext),
            id(NoClauseId, NoClauseName, NoClauseArity),
            string.format("No productions for %s/%d.",
                [s(NoClauseName), i(NoClauseArity)], NoClauseMsg),
            NoClauseError = error([NoClauseMsg], NoClauseContext)
        ),
    list.map(GenerateNoClauseError, NoClauseList, NoClauseErrors),

    Errors = CheckClausesErrors ++ NoDeclErrors ++ NoClauseErrors.

:- pred check_clauses0(list(clause)::in, rule_decls::in,
    clauses::in, clauses::out, list(check.error)::out) is det.

check_clauses0([], _Decls, !Clauses, []).
check_clauses0([Clause | ClauseList], Decls, !Clauses, Errors) :-
    Clause = clause(Head, Prod, _, Context),
    Id = nonterminal(Head),
    ( if map.search(!.Clauses, Id, ClauseList0) then
        list.append(ClauseList0, [Clause], ClauseList1)
    else
        ClauseList1 = [Clause]
    ),
    map.set(Id, ClauseList1, !Clauses),

    % Look for used nonterminals that are not declared.
    UndeclaredNonTerminals =
        ( pred(NonTermId::out) is nondet :-
            % XXX performance
            nonterminals(Prod, NonTermIds),
            list.member(NonTermId, NonTermIds),
            not contains(Decls, NonTermId)
        ),
    solutions(UndeclaredNonTerminals, UnDeclaredIds),
    GenerateUndeclaredNonterminalError =
        ( pred(UnDeclaredId::in, UnDeclaredError::out) is det :-
            id(Id, CN, CA),
            id(UnDeclaredId, NN, NA),
            string.format("In production for %s/%d,",
                [s(CN), i(CA)], Msg0),
            string.format("  the nonterminal %s/%d is undeclared.",
                [s(NN), i(NA)], Msg1),
            UnDeclaredError = error([Msg0, Msg1], Context)
        ),
    list.map(GenerateUndeclaredNonterminalError, UnDeclaredIds,
        UndeclaredNonTerminalErrors),
    (
        UndeclaredNonTerminalErrors = [],
        check_clauses0(ClauseList, Decls, !Clauses, Errors)
    ;
        % Not tail recursive, so only do it if we have to.
        UndeclaredNonTerminalErrors = [_ | _],
        check_clauses0(ClauseList, Decls, !Clauses, CheckErrors),
        Errors = UndeclaredNonTerminalErrors ++ CheckErrors
    ).

%---------------------------------------------------------------------------%

check_useless(Start, Clauses, Decls, Errors) :-
    StartSet = set.make_singleton_set(Start),
    useful(StartSet, Clauses, StartSet, UsefulSet),
    map.keys(Clauses, AllIds),
    set.sorted_list_to_set(AllIds, AllSet),
    UselessSet = AllSet `set.difference` UsefulSet,
    set.to_sorted_list(UselessSet, UselessList),
    GenerateUselessErrors =
        ( pred(UselessId::in, Error::out) is semidet :-
            % Use search rather than lookup in case
            % it was an undeclared rule.
            map.search(Decls, UselessId, Decl),
            Decl = rule(_Id, _Args, _VarSet, Context),
            UselessId = Name / Arity,
            string.format("Grammar rule %s/%d is not used.",
                [s(Name), i(Arity)], Msg),
            Error = error([Msg], Context)
        ),
    list.filter_map(GenerateUselessErrors, UselessList, Errors).

    % Perform a fixpoint computation to find all the nonterminals
    % that are reachable from the start symbol.
    %
:- pred useful(set(nonterminal)::in, clauses::in,
    set(nonterminal)::in, set(nonterminal)::out) is det.

useful(New0, Clauses, !Useful) :-
    ( if set.is_empty(New0) then
        true
    else
        solutions_set(
            ( pred(UId::out) is nondet :-
                set.member(Id, New0),
                map.search(Clauses, Id, ClauseList),
                list.member(Clause, ClauseList),
                Clause = clause(_Head, Prod, _VarSet, _Context),
                nonterminal(UId, Prod)
            ), NewSet),
        New1 = NewSet `set.difference` !.Useful,
        !:Useful = New1 `set.union`!.Useful,
        useful(New1, Clauses, !Useful)
    ).

:- pred nonterminal(nonterminal::out, prod::in) is nondet.

nonterminal(nonterminal(Term), nonterminal(Term)).
nonterminal(NonTerminal, (A, B)) :-
    (
        nonterminal(NonTerminal, A)
    ;
        nonterminal(NonTerminal, B)
    ).
nonterminal(NonTerminal, (A ; B)) :-
    (
        nonterminal(NonTerminal, A)
    ;
        nonterminal(NonTerminal, B)
    ).

%---------------------------------------------------------------------------%

check_inf_derivations(Clauses, Decls, Errors) :-
    map.keys(Clauses, AllIds),
    set.sorted_list_to_set(AllIds, InfSet0),
    set.init(FinSet0),
    finite(FinSet0, Clauses, InfSet0, InfSet),
    set.to_sorted_list(InfSet, InfList),
    GenerateInfiniteErrors =
        ( pred(InfId::in, Error::out) is semidet :-
                % Use search rather than lookup in case
                % it was an undeclared rule.
            map.search(Decls, InfId, Decl),
            Decl = rule(_Id, _Args, _VarSet, Context),
            InfId = Name / Arity,
            string.format("Rule %s/%d does not have any finite derivations.",
                [s(Name), i(Arity)], Msg),
            Error = error([Msg], Context)
        ),
    list.filter_map(GenerateInfiniteErrors, InfList, Errors).

:- pred finite(set(nonterminal)::in, clauses::in,
    set(nonterminal)::in, set(nonterminal)::out) is det.

finite(Fin0, Clauses, !Inf) :-
    solutions_set(
        ( pred(NewFinId::out) is nondet :-
            set.member(NewFinId, !.Inf),
            % search rather than lookup in case the nonterminal
            % doesn't have any clauses. This may lead to
            % spurious infinite derivations.
            map.search(Clauses, NewFinId, ClauseList),
            list.member(Clause, ClauseList),
            Clause = clause(_Head, Prod, _VarSet, _Context),
            nonterminals(Prod, NonTerms),
            (
                NonTerms = []
            ;
                NonTerms = [_ | _],
                all [NId] (
                    list.member(NId, NonTerms)
                =>
                    set.member(NId, Fin0)
                )
            )
        ), NewFinSet),
    NewFin = NewFinSet `set.difference` Fin0,
    ( if set.is_empty(NewFin) then
        true
    else
        !:Inf = !.Inf `set.difference` NewFin,
        Fin = Fin0 `set.union` NewFin,
        finite(Fin, Clauses, !Inf)
    ).

:- pred nonterminals(prod::in, list(nonterminal)::out) is nondet.

nonterminals([], []).
nonterminals(terminal(_), []).
nonterminals(nonterminal(Term), [nonterminal(Term)]).
nonterminals((A, B), Syms) :-
    nonterminals(A, ASyms),
    nonterminals(B, BSyms),
    append(ASyms, BSyms, Syms).
nonterminals((A ; _B), Syms) :-
    nonterminals(A, Syms).
nonterminals((_A ; B), Syms) :-
    nonterminals(B, Syms).
nonterminals(action(_), []).

%---------------------------------------------------------------------------%

:- pred id(nonterminal::in, name::out, arity::out) is det.

id(Name/Arity, Name, Arity).
id(start, _, _) :-
    error("id: unexpected start").

%---------------------------------------------------------------------------%

write_error(error(MsgLines, Context), !IO) :-
    Context = term.context(File, Line),
    string.format("%s:%d: ", [s(File), i(Line)], ContextMsg),
    io.stderr_stream(StdErr, !IO),
    WriteContextAndMsg =
        ( pred(Msg::in, !.IO::di, !:IO::uo) is det :-
            io.write_string(StdErr, ContextMsg, !IO),
            io.write_string(StdErr, Msg, !IO),
            io.nl(StdErr, !IO)
        ),
    list.foldl(WriteContextAndMsg, MsgLines, !IO).

%---------------------------------------------------------------------------%

