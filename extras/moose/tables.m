%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003-2004, 2011 The University of Melbourne.
% Copyright (C) 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%---------------------------------------------------------------------------%

:- module tables.
:- interface.

:- import_module grammar.
:- import_module lalr.

:- import_module list.
:- import_module map.
:- import_module set.

%---------------------------------------------------------------------------%

:- type states == map(items, int).

:- type shifts == map(nonterminal, set(terminal)).

:- type action_errors == list(action_err).

:- type action_err
    --->    warning(action_warning)
    ;       error(action_error).

:- type action_warning
    --->    shiftreduce(state, prodnum).

:- type action_error
    --->    shiftshift(state, state)
    ;       reducereduce(prodnum, prodnum)
    ;       misc(action, action).

:- pred shifts(set(items)::in, rules::in, first::in, reaching::in, shifts::out)
    is det.

:- pred actions(set(items)::in, rules::in, lookaheads::in, gotos::in,
    shifts::in, states::out, action_table::out, action_errors::out) is det.

:- pred gotos(set(items)::in, states::in, gotos::in, goto_table::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module int.
:- import_module term.

%---------------------------------------------------------------------------%

shifts(_C, _Rules, First, Reaching, !:Shifts) :-
    map.init(!:Shifts),
    map.foldl(
        ( pred(N::in, Ts0::in, !.Shifts::in, !:Shifts::out) is det :-
            ( if map.search(Reaching, N, Ns0) then
                set.to_sorted_list(Ns0, Ns1)
            else
                Ns1 = []
            ),
            list.map(map.lookup(First), Ns1, Ts1),
            list.foldl(set.union, Ts1, Ts0, Ts2),
            Ts = Ts2 `set.difference` set.make_singleton_set(epsilon),
            map.set(N, Ts, !Shifts)
        ), First, !Shifts).

%---------------------------------------------------------------------------%

actions(C, Rules, Lookaheads, Gotos, Shifts, !:States, !:Actions, Errs) :-
    set.to_sorted_list(C, CList),
    map.init(!:States),
    number_states(CList, 0, !States),
    map.init(!:Actions),
    actions1(CList, Rules, Lookaheads, !.States, Gotos, Shifts, !Actions,
        [], Errs).

:- pred number_states(list(items)::in, int::in, states::in, states::out)
    is det.

number_states([], _N, !States).
number_states([I | Is], N, !States) :-
    map.det_insert(I, N, !States),
    number_states(Is, N + 1, !States).

:- pred actions1(list(items)::in, rules::in, lookaheads::in, states::in,
    gotos::in, shifts::in, action_table::in, action_table::out,
    action_errors::in, action_errors::out) is det.

actions1([], _Rules, _Lookaheads, _States, _Gotos, _Shifts, !Actions, !Errs).
actions1([I | Is], Rules, Lookaheads, States, Gotos, Shifts, !Actions, !Errs) :-
    map.lookup(States, I, Sn),
    set.to_sorted_list(I, IList),
    actions2(IList, I, Sn, Rules, Lookaheads, States, Gotos, Shifts,
        !Actions, !Errs),
    actions1(Is, Rules, Lookaheads, States, Gotos, Shifts, !Actions, !Errs).

:- pred actions2(list(item)::in, items::in, state::in, rules::in,
    lookaheads::in, states::in, gotos::in, shifts::in,
    action_table::in, action_table::out, action_errors::in, action_errors::out)
    is det.

actions2([], _I, _Sn, _Rules, _LA, _States, _Gotos, _Shifts, !Actions, !Errs).
actions2([A | As], I, Sn, Rules, LA, States, Gotos, Shifts, !Actions, !Errs) :-
    A = item(Ip, Id),
    map.lookup(Rules, Ip, rule(_, _, Syms, _, _, _, _)),
    array.max(Syms, Max),
    ( if Id =< Max then
        array.lookup(Syms, Id, X),
        map.lookup(Gotos, I, IGs),
        (
            X = terminal(T0),
            Ts = set.make_singleton_set(T0)
        ;
            X = nonterminal(N),
            ( if map.search(Shifts, N, Ts0) then
                Ts = Ts0
            else
                set.init(Ts)
            )
        ),
        set.to_sorted_list(Ts, TList),
        list.foldl2(
            ( pred(T::in, !.Actions::in, !:Actions::out,
                    !.Errs::in, !:Errs::out) is det :-
                map.lookup(IGs, terminal(T), J),
                map.lookup(States, J, Jn),
                addaction(Sn, T, shift(Jn), !Actions, !Errs)
            ), TList, !Actions, !Errs)
    else
        % A -> alpha .
        ( if
            map.search(LA, I, ILAs),
            map.search(ILAs, A, Alphas)
        then
            set.to_sorted_list(Alphas, AlphaList),
            list.foldl2(
                ( pred(T::in, !.Actions::in, !:Actions::out,
                        !.Errs::in, !:Errs::out) is det :-
                    ( if Ip = 0, T = ($) then
                        addaction(Sn, T, accept, !Actions, !Errs)
                    else
                        addaction(Sn, T, reduce(Ip), !Actions, !Errs)
                    )
                ), AlphaList, !Actions, !Errs)
        else
            true
        )
    ),
    actions2(As, I, Sn, Rules, LA, States, Gotos, Shifts, !Actions, !Errs).

:- pred addaction(state::in, terminal::in, action::in,
    action_table::in, action_table::out,
    action_errors::in, action_errors::out) is det.

addaction(Sn, T, A0, !Actions, !Errs) :-
    ( if map.search(!.Actions, Sn, State0) then
        State1 = State0
    else
        map.init(State1)
    ),
    ( if map.search(State1, T, A1) then
        ( if A0 = A1 then
            A = A1
        else if
            (
                A0 = shift(S),
                A1 = reduce(R),
                A2 = A0,
                Err = warning(shiftreduce(S, R))
            ;
                A0 = reduce(R),
                A1 = shift(S),
                A2 = A1,
                Err = warning(shiftreduce(S, R))
            )
        then
            A = A2,
            list.append([Err], !Errs)
        else
            A = A0,
            ( if
                A0 = shift(S0),
                A1 = shift(S1)
            then
                Err = error(shiftshift(S0, S1))
            else if
                A0 = reduce(R0),
                A1 = reduce(R1)
            then
                Err = error(reducereduce(R0, R1))
            else
                Err = error(misc(A0, A1))
            ),
            list.append([Err], !Errs)
        )
    else
        A = A0
    ),
    map.set(T, A, State1, State),
    map.set(Sn, State, !Actions).

%---------------------------------------------------------------------------%

gotos(_C, States, Gotos, !:GotoTable) :-
    map.init(!:GotoTable),
    map.foldl(
        ( pred(I0::in, IGs::in, !.GotoTable::in, !:GotoTable::out) is det :-
            map.lookup(States, I0, Sf),
            map.foldl(
                ( pred(Sym::in, J0::in, !.GotoTable::in, !:GotoTable::out)
                        is det :-
                ( if Sym = nonterminal(N) then
                    map.lookup(States, J0, St),
                    ( if map.search(!.GotoTable, Sf, X0) then
                        X1 = X0
                    else
                        map.init(X1)
                    ),
                    map.set(N, St, X1, X),
                    map.set(Sf, X, !GotoTable)
                else
                    true
                )
            ), IGs, !GotoTable)
        ), Gotos, !GotoTable).
