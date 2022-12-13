%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2001, 2003, 2006, 2011 The University of Melbourne.
% Copyright (C) 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%---------------------------------------------------------------------------%
%
% file: grammar.m
% main author: conway,  November 1998
%
% This module defines the representation(s) of grammars that moose uses
% to construct parsers.
%
%---------------------------------------------------------------------------%

:- module grammar.
:- interface.

:- import_module array.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module term.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type grammar
    --->    grammar(
                rules,
                clauses,
                xforms,
                int,            % Next nonterminal
                index,          % Rule index
                first,
                follow
            ).

    % index maps from each nonterminal to the list (set) of normalized rules
    % for that nonterminal.
:- type index == map(nonterminal, list(int)).

:- type clauses == map(nonterminal, list(clause)).

:- type (clause)
    --->    clause(
                term,   % Head
                prod,   % body
                varset,
                term_context % Context of the `--->'
            ).

:- type prod
    --->    terminal(term)
    ;       nonterminal(term)
    ;       ( prod , prod )
    ;       { prod ;  prod }
    ;       action(term)
    ;       [].     % epsilon

:- type name == string.
:- type arity == int.

:- type terminal
        --->    epsilon % epsilon isn't really a terminal, but it avoids the
                        % need for wrappers in the FIRST(alpha) situations.
        ;       (name / arity)
        ;       ($)     % the special end-of-input symbol
        ;       (*).    % the dummy symbol used for lookahead computation.

:- type nonterminal
        --->    start   % S' - the distinguished start symbol. Will always
                        % correspond to prodnum == 0.
        ;       (name / arity).

:- type symbol
        --->    terminal(terminal)
        ;       nonterminal(nonterminal).

:- type symbols == array(symbol).

:- type bodyterm
        --->    terminal(term)
        ;       nonterminal(term).

:- type rule_decls == map(nonterminal, rule_decl).

:- type rule_decl
    --->    rule(
                nonterminal,    % Name/Arity
                list(term),     % types of the attributes
                varset,         % type variables of the attributes
                context         % context of the declaration.
            ).

:- type rules == map(int, (rule)).

:- type (rule)
    --->    rule(
                nonterminal,    % the nonterm that this rule belongs to
                term,           % Head
                symbols,        % Body
                list(bodyterm), % NTs with their arguments
                list(term),     % Actions
                varset,
                context         % context from the clause.
            ).

:- type xform
    --->    xform(
                nonterminal,
                string
            ).

:- type xforms == map(nonterminal, xform).

:- type first == map(nonterminal, set(terminal)).

:- type follow == map(nonterminal, set(terminal)).

:- type state == int.

:- type action
    --->    accept
    ;       shift(int)
    ;       reduce(int).

:- type action_table == map(state, map(terminal, action)).
% :- type action_table == (state -> terminal -> action).

:- type goto_table == map(state, map(nonterminal, state)).
% :- type goto_table == (state -> nonterminal -> state).

:- pred term_to_clause(term::in, varset::in, nonterminal::out, clause::out)
    is semidet.

:- pred add_clause(clauses::in, nonterminal::in, clause::in, clauses::out)
    is det.

:- pred construct_grammar(nonterminal::in, clauses::in, xforms::in,
    grammar::out) is det.

:- pred compute_first(rules::in, first::out) is det.

:- pred compute_follow(rules::in, nonterminal::in, terminal::in, first::in,
    follow::out) is det.

:- func terminal(term) = terminal.
:- func nonterminal(term) = nonterminal.
:- func first(first, symbols, int) = set(terminal).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module require.
:- import_module pair.
:- import_module string.
:- import_module solutions.

%---------------------------------------------------------------------------%

term_to_clause(functor(Atom, Args, Context), VarSet, Id, Rule) :-
    Atom = atom("--->"),
    Args = [Head, Body],
    Head = functor(atom(Name), HeadArgs, _),
    list.length(HeadArgs, Arity),
    Id = Name/Arity,
    Rule = clause(Head, Prod, VarSet, Context),
    term_to_prod(Body, Prod).

:- pred term_to_prod(term::in, prod::out) is semidet.

term_to_prod(functor(Atom, Args, Ctxt), Prod) :-
    ( if Atom = atom(","), Args = [Arg1, Arg2] then
        term_to_prod(Arg1, Left),
        term_to_prod(Arg2, Right),
        Prod = (Left, Right)
    else if Atom = atom(";"), Args = [Arg1, Arg2] then
        term_to_prod(Arg1, Left),
        term_to_prod(Arg2, Right),
        Prod = (Left; Right)
    else if Atom = atom("{}"), Args = [Goal] then
        Prod = action(Goal)
    else if Atom = atom("{}"), Args = [Goal | Goals] then
        list.foldl(
            (pred(G::in, Left::in, (Left, action(G))::out) is det),
            Goals, action(Goal), Prod)
    else if Atom = atom("[]"), Args = [] then
        Prod = []
    else if Atom = atom("[|]"), Args = [Head, Tail] then
        terminals(Tail, terminal(Head), Prod)
    else
        Prod = nonterminal(functor(Atom, Args, Ctxt))
    ).

:- pred terminals(term::in, prod::in, prod::out) is semidet.

terminals(functor(Atom, Args, _), Prod0, Prod) :-
    ( if Atom = atom("[]"), Args = [] then
        Prod = Prod0
    else if Atom = atom("[|]"), Args = [Head, Tail] then
        terminals(Tail, (Prod0, terminal(Head)), Prod)
    else
        fail
    ).

%---------------------------------------------------------------------------%

add_clause(Clauses0, Id, Clause, Clauses) :-
    ( if map.search(Clauses0, Id, These0) then
        These = [Clause | These0]
    else
        These = [Clause]
    ),
    map.set(Id, These, Clauses0, Clauses).

%---------------------------------------------------------------------------%

construct_grammar(Start, AllClauses, XForms, Grammar) :-
    map.to_assoc_list(AllClauses, ClauseList),
    Nont0 = 1,
    start_rule(Start, StartRule),
    map.from_assoc_list([0 - StartRule], Rules0),
    map.init(ClauseIndex0),
    map.init(First0),
    map.init(Follow0),
    Grammar0 = grammar(Rules0, AllClauses, XForms, Nont0, ClauseIndex0,
        First0, Follow0),
    list.foldl(transform_clause_list, ClauseList, Grammar0, Grammar1),
    compute_first0(Grammar1, Grammar2),
    compute_follow0(Grammar2, Grammar3),
    Grammar3 = grammar(Rules3, AllClauses3, XForms3, Nont3, ClauseIndex3,
        First3, Follow3),

    % Keep the nonterminals in reverse sorted order
    % for efficient processing in lalr.m
    map.map_values(
        ( pred(_K::in, V0::in, V::out) is det :-
            list.sort(V0, V1),
            list.reverse(V1, V)
        ), ClauseIndex3, ClauseIndex4),
    Grammar = grammar(Rules3, AllClauses3, XForms3, Nont3, ClauseIndex4,
        First3, Follow3).

:- pred start_rule(nonterminal::in, (rule)::out) is det.

start_rule(Id, Rule) :-
    (
        Id = Name / Arity
    ;
        Id = start,
        error("epsilon start rule")
    ),
    varset.init(VarSet0),
    varset.new_vars(Arity, Vars, VarSet0, VarSet1),
    list.foldl(
        (pred(V::in, VS0::in, VS::out) is det :-
            term.var_to_int(V, I),
            string.format("V%d", [i(I)], N),
            varset.name_var(V, N, VS0, VS)
        ), Vars, VarSet1, VarSet),
    term.var_list_to_term_list(Vars, Args),
    Context = context("foobie", 1),
    string.append(Name, "'", NewName),
    NewId = start,
    Head = functor(atom(NewName), Args, Context),
    Body = array([nonterminal(Id)]),
    Body1 = [nonterminal(functor(atom(Name), Args, Context))],
    Rule = rule(NewId, Head, Body, Body1, [], VarSet, Context).

:- pred transform_clause_list(pair(nonterminal, list(clause))::in,
    grammar::in, grammar::out) is det.

transform_clause_list(Id - Clauses, !Grammar) :-
    list.foldl(transform_clause(Id), Clauses, !Grammar).

:- pred transform_clause(nonterminal::in, clause::in,
    grammar::in, grammar::out) is det.

transform_clause(Id, Clause, !Grammar) :-
    Clause = clause(Head, Prod, Varset, Context),
    solutions(transform_prod(Prod), Bodies),
    list.foldl(add_rule(Id, Head, Varset, Context), Bodies, !Grammar).

:- pred add_rule(nonterminal::in, term::in, varset::in, context::in,
    pair(list(bodyterm), list(term))::in, grammar::in, grammar::out) is det.

add_rule(Id, Head, Varset, Context, BodyTerms - Actions, !Grammar) :-
    !.Grammar = grammar(Rules0, C, Xfs, Nont0, ClauseIndex0, F, L),
    list.map(
        ( pred(BodyTerm::in, BodyId::out) is det :-
            (
                BodyTerm = terminal(Term),
                ( if Term = functor(atom(Name), Args, _) then
                    length(Args, Arity),
                    BId0 = Name/Arity
                else
                    error("add_rule: bad body term")
                ),
                BodyId = terminal(BId0)
            ;
                BodyTerm = nonterminal(Term),
                ( if Term = functor(atom(Name), Args, _) then
                    length(Args, Arity),
                    BId0 = Name/Arity
                else
                    error("add_rule: bad body term")
                ),
                BodyId = nonterminal(BId0)
            )
        ), BodyTerms, BodyIds),
    Rule = rule(Id, Head, array(BodyIds), BodyTerms, Actions, Varset, Context),
    map.set(Nont0, Rule, Rules0, Rules),
    Nont = Nont0 + 1,
    ( if map.search(ClauseIndex0, Id, Prods0) then
        Prods = [Nont0 | Prods0]
    else
        Prods = [Nont0]
    ),
    map.set(Id, Prods, ClauseIndex0, ClauseIndex),
    !:Grammar = grammar(Rules, C, Xfs, Nont, ClauseIndex, F, L).

:- pred transform_prod(prod::in, pair(list(bodyterm), list(term))::out)
    is multi.

transform_prod(terminal(Term), [terminal(Term)] - []).
transform_prod(nonterminal(Term), [nonterminal(Term)] - []).
transform_prod(action(Term), [] - [Term]).
transform_prod((ProdA, ProdB), Body - Actions) :-
    transform_prod(ProdA, BodyA - ActionsA),
    transform_prod(ProdB, BodyB - ActionsB),
    list.append(BodyA, BodyB, Body),
    list.append(ActionsA, ActionsB, Actions).
transform_prod((ProdA ; ProdB), Result) :-
    (
        transform_prod(ProdA, Result)
    ;
        transform_prod(ProdB, Result)
    ).
transform_prod([], [] - []).

terminal(Term) = Terminal :-
    ( if
        Term = functor(atom(Name), Args, _),
        length(Args, Arity)
    then
        Terminal = Name / Arity
    else
        error("terminal: bad term")
    ).

nonterminal(Term) = Terminal :-
    ( if
        Term = functor(atom(Name), Args, _),
        length(Args, Arity)
    then
        Terminal = Name / Arity
    else
        error("nonterminal: bad term")
    ).

%---------------------------------------------------------------------------%

    % The computation of the first sets is directly from the dragon book.
    %
:- pred compute_first0(grammar::in, grammar::out) is det.

compute_first0(Grammar0, Grammar) :-
    Grammar0 = grammar(Rules, Clauses, Xfs, Nont, Index, _, Follow),
        compute_first(Rules, First),
        Grammar = grammar(Rules, Clauses, Xfs, Nont, Index, First, Follow).

:- type first_stuff
    --->    first_stuff(
                bool,               % Changed?
                list(nonterminal),  % Nonterminals
                rules,
                first
            ).

compute_first(Rules, First) :-
    collect_nonterminals(Rules, Nonterminals),
    map.init(First0),
    Stuff0 = first_stuff(no, Nonterminals, Rules, First0),
    until(
        ( pred(Stuff1::in, Stuff3::out) is det :-
            Stuff1 = first_stuff(_, N1, R1, F1),
            Stuff2 = first_stuff(no, N1, R1, F1),
            map.foldl(compute_first, Rules, Stuff2, Stuff3)
        ),
        ( pred(StuffN::in) is semidet :-
            StuffN = first_stuff(no, _, _, _)
        ), Stuff0, Stuff),
    Stuff = first_stuff(_, _, _, First).

:- pred compute_first(int::in, (rule)::in, first_stuff::in, first_stuff::out)
    is det.

compute_first(_RuleNum, Rule, Stuff0, Stuff) :-
    Rule = rule(Id, _Head, Elems, _Body, _Actions, _Varset, _Context),
    array.max(Elems, Max),
    ( if Max >= 0 then
        % If there are literals in the body of the rule, then compute
        % the first set that derives from what we currently know...
        Stuff0 = first_stuff(_, _, _, TmpFirst),
        set.init(Emp),
        compute_first(0, Max, Elems, TmpFirst, Emp, ComputedFirst)
    else
        % There were no literals in the body of the rule,
        % so it was an epsilon rule.
        ComputedFirst = set.make_singleton_set(epsilon)
    ),
    % Add the computed first set to what we currently know, noting
    % whether or not anything has changed.
    Stuff0 = first_stuff(Ch0, Ns, Rs, First0),
    ( if search(First0, Id, ThisFirst0) then
        difference(ComputedFirst, ThisFirst0, NewFirst),
        union(ThisFirst0, NewFirst, ThisFirst),
        ( if set.is_empty(NewFirst) then
            Ch1 = Ch0
        else
            Ch1 = yes
        )
    else
        ThisFirst = ComputedFirst,
        Ch1 = yes
    ),
    map.set(Id, ThisFirst, First0, First1),
    Stuff = first_stuff(Ch1, Ns, Rs, First1).

    % Compute the first set directly from what we currently
    % know (using rule 3 on p189 of the dragon book):
    % iterate over the body until we get to
    %       - the end
    %       - an element about which we know nothing,
    %       - a terminal
    %       - a first set for a nonterminal that does not
    %               contain epsilon
    %
:- pred compute_first(int::in, int::in, symbols::in, first::in,
    set(terminal)::in, set(terminal)::out) is det.

compute_first(I, IMax, Elems, First, Set0, Set) :-
    ( if I =< IMax then
        array.lookup(Elems, I, Elem),
        (
            Elem = terminal(Id),
            % If we get to a terminal, then we add it to the first set,
            % and remove epsilon (if it was there in the first place), since
            % this rule is certainly not nullable.
            set.insert(Id, Set0, Set1),
            set.difference(Set1, set.make_singleton_set(epsilon), Set)
        ;
            Elem = nonterminal(Id),
            ( if map.search(First, Id, Set1) then
                % If we know some information about the nonterminal,
                % then add it to what we already know. If it is not nullable,
                % then this rule is not nullable, and we are done.
                % If it is nullable, then we look at the next literal
                % in the body.
                set.union(Set0, Set1, Set2),
                ( if set.member(epsilon, Set1) then
                    compute_first(I + 1, IMax, Elems, First, Set2, Set)
                else
                    set.difference(Set2, set.make_singleton_set(epsilon), Set)
                )
            else
                % If we don't know anything about this nonterminal,
                % then stop here.
                Set = Set0
            )
        )
    else
        Set = Set0
    ).

:- pred collect_terminals(rules::in, set(terminal)::out) is det.

collect_terminals(Rules, Terminals) :-
    map.foldl(
        ( pred(_RN::in, Rule::in, Ts0::in, Ts::out) is det :-
            Rule = rule(_Id, _Head, Elems, _, _, _, _),
            array.foldl(
                ( pred(Elem::in, Ts1::in, Ts2::out) is det :-
                    (
                        Elem = terminal(Id),
                        Ts2 = [Id | Ts1]
                    ;
                        Elem = nonterminal(_Id_),
                        Ts2 = Ts1
                    )
                ), Elems, Ts0, Ts)
        ), Rules, [], TerminalsList),
    set.list_to_set(TerminalsList, Terminals).

:- pred collect_nonterminals(rules::in, list(nonterminal)::out) is det.

collect_nonterminals(Rules, Nonterminals) :-
    map.foldl(
        ( pred(_RN ::in, Rule::in, Ts0::in, Ts::out) is det :-
            Rule = rule(Id, _Head, _Elems, _, _, _Varset, _Context),
            Ts = [Id | Ts0]
        ), Rules, [], NonterminalsList),
    set.list_to_set(NonterminalsList, Nonterminals0),
    set.to_sorted_list(Nonterminals0, Nonterminals).

    % YYY This probably belongs in the library somewhere.
    %
:- pred while(pred(T)::in(pred(in) is semidet),
    pred(T, T)::in(pred(in, out) is det), T::in, T::out) is det.

while(Cond, Body, !Acc) :-
    ( if Cond(!.Acc) then
        Body(!Acc),
        while(Cond, Body, !Acc)
    else
        true
    ).

    % YYY This probably belongs in the library somewhere.
    %
:- pred until(pred(T, T)::in(pred(in, out) is det),
    pred(T)::in(pred(in) is semidet), T::in, T::out) is det.

until(Body, Cond, !Acc) :-
    Body(!Acc),
    ( if Cond(!.Acc) then
        true
    else
        until(Body, Cond, !Acc)
    ).

%---------------------------------------------------------------------------%

    % The computation of the follow sets is directly from the dragon book.
    %
:- pred compute_follow0(grammar::in, grammar::out) is det.

compute_follow0(Grammar0, Grammar) :-
    Grammar0 = grammar(Rules, Clauses, Xfs, Nont, Index, First, _),
    compute_follow(Rules, start, ($), First, Follow),
    Grammar = grammar(Rules, Clauses, Xfs, Nont, Index, First, Follow).

:- type follow_stuff
    --->    follow_stuff(
                bool,               % Changed?
                list(nonterminal),  % Nonterminals
                rules,
                first,
                follow
            ).

compute_follow(Rules, Start, EOF, First, Follow) :-
        map.init(Follow0),
        % Rule 1
        map.set(Start, set.make_singleton_set(EOF), Follow0, Follow1),
        collect_nonterminals(Rules, Ns),
        Stuff0 = follow_stuff(no, Ns, Rules, First, Follow1),
        until(
            ( pred(Stuff1::in, Stuff3::out) is det :-
                Stuff1 = follow_stuff(_, N1, R1, Fi1, Fo1),
                Stuff2 = follow_stuff(no, N1, R1, Fi1, Fo1),
                foldl(compute_follow, Rules, Stuff2, Stuff3)
            ),
            ( pred(StuffN::in) is semidet :-
                StuffN = follow_stuff(no, _, _, _, _)
            ),
            Stuff0, Stuff),
        Stuff = follow_stuff(_, _, _, _, Follow).

:- pred compute_follow(int::in, (rule)::in,
    follow_stuff::in, follow_stuff::out) is det.

compute_follow(_RuleNum, Rule, Stuff0, Stuff) :-
    Rule = rule(Id, _Head, Elems, _, _, _Varset, _Context),
    Stuff0 = follow_stuff(_, _, _, First, _),
    array.max(Elems, Max),
    % Apply Rule 2
    compute_follow2(0, Max, First, Elems, Stuff0, Stuff1),
    compute_follow3(Max, First, Id, Elems, Stuff1, Stuff).

:- pred compute_follow2(int::in, int::in, first::in, symbols::in,
    follow_stuff::in, follow_stuff::out) is det.

compute_follow2(I, IMax, First, Elems, Stuff0, Stuff) :-
    ( if I =< IMax then
        lookup(Elems, I, Elem),
        ( if Elem = nonterminal(Id) then
            IdFollow0 = first(First, Elems, I + 1),
            difference(IdFollow0, set.make_singleton_set(epsilon), IdFollow),
            add_follow(Id, IdFollow, Stuff0, Stuff1)
        else
            Stuff1 = Stuff0
        ),
        compute_follow2(I + 1, IMax, First, Elems, Stuff1, Stuff)
    else
        Stuff = Stuff0
    ).

:- pred compute_follow3(int::in, first::in, nonterminal::in, symbols::in,
    follow_stuff::in, follow_stuff::out) is det.

compute_follow3(I, First, MyId, Elems, Stuff0, Stuff) :-
    ( if I >= 0 then
        array.lookup(Elems, I, Elem),
        ( if Elem = nonterminal(Id) then
            get_follow(MyId, MyFollow, Stuff0, _),
            add_follow(Id, MyFollow, Stuff0, Stuff1),
            map.lookup(First, Id, IdFirst),
            ( if set.member(epsilon, IdFirst) then
                compute_follow3(I - 1, First, MyId, Elems, Stuff1, Stuff)
            else
                Stuff = Stuff1
            )
        else
            Stuff = Stuff0
        )
    else
        Stuff = Stuff0
    ).

:- pred get_follow(nonterminal::in, set(terminal)::out,
    follow_stuff::in, follow_stuff::out) is det.

get_follow(Id, IdFollow, Stuff, Stuff) :-
    Stuff = follow_stuff(_, _, _, _, Follow),
    ( if search(Follow, Id, IdFollow0) then
        IdFollow = IdFollow0
    else
        set.init(IdFollow)
    ).

:- pred add_follow(nonterminal::in, set(terminal)::in,
    follow_stuff::in, follow_stuff::out) is det.

add_follow(Id, IdFollow0, Stuff0, Stuff) :-
    Stuff0 = follow_stuff(Ch0, Ns, Rs, Fs, Follow0),
    ( if map.search(Follow0, Id, OldFollow) then
        difference(IdFollow0, OldFollow, NewFollow),
        ( if set.is_empty(NewFollow) then
            IdFollow = OldFollow,
            Ch = Ch0
        else
            union(OldFollow, NewFollow, IdFollow),
            Ch = yes
        )
    else
        IdFollow = IdFollow0,
        Ch = yes
    ),
    map.set(Id, IdFollow, Follow0, Follow),
    Stuff = follow_stuff(Ch, Ns, Rs, Fs, Follow).

%---------------------------------------------------------------------------%

first(First, Elems, I) = FirstI :-
    array.max(Elems, Max),
    ( if I =< Max then
        array.lookup(Elems, I, Elem),
        (
            Elem = terminal(Id),
            FirstI = set.make_singleton_set(Id)
        ;
            Elem = nonterminal(Id),
            map.lookup(First, Id, FirstI0),
            ( if set.member(epsilon, FirstI0) then
                RestFirst = first(First, Elems, I + 1),
                set.union(FirstI0, RestFirst, FirstI)
            else
                FirstI = FirstI0
            )
        )
    else
        FirstI = set.make_singleton_set(epsilon)
    ).

%---------------------------------------------------------------------------%
