%----------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%----------------------------------------------------------------------------%

:- module tables.

:- interface.

:- import_module grammar, lalr, misc.
:- import_module int, list, set.

:- type states == (items -> int).

:- type shifts == (nonterminal -> set(terminal)).

:- type actionerrors == list(actionerr).

:- type actionerr
	--->	warning(actionwarning)
	;	error(actionerror)
	.

:- type actionwarning
	--->	shiftreduce(state, prodnum)
	.

:- type actionerror
	--->	shiftshift(state, state)
	;	reducereduce(prodnum, prodnum)
	;	misc(action, action)
	.

:- pred shifts(set(items), rules, first, reaching, shifts).
:- mode shifts(in, in, in, in, out) is det.

:- pred actions(set(items), rules, lookaheads, gotos, shifts,
		states, actiontable, actionerrors).
:- mode actions(in, in, in, in, in, out, out, out) is det.

:- pred gotos(set(items), states, gotos, gototable).
:- mode gotos(in, in, in, out) is det.

:- implementation.

:- import_module array, bool, map, require, std_util, term.

%------------------------------------------------------------------------------%

shifts(_C, _Rules, First, Reaching, !:Shifts) :-
	map__init(!:Shifts),
	map__foldl((pred(N::in, Ts0::in, !.Shifts::in, !:Shifts::out) is det :-
		( map__search(Reaching, N, Ns0) ->
			set__to_sorted_list(Ns0, Ns1)
		;
			Ns1 = []
		),
		list__map(map__lookup(First), Ns1, Ts1),
		list__foldl(set__union, Ts1, Ts0, Ts2),
		Ts = Ts2 - { epsilon },
		map__set(!.Shifts, N, Ts, !:Shifts)
	), First, !Shifts).

%------------------------------------------------------------------------------%

actions(C, Rules, Lookaheads, Gotos, Shifts, !:States, !:Actions, Errs) :-
	set__to_sorted_list(C, CList),
	map__init(!:States),
	number_states(CList, 0, !States),
	map__init(!:Actions),
	actions1(CList, Rules, Lookaheads, !.States, Gotos, Shifts, !Actions, 
		[], Errs).

:- pred number_states(list(items), int, states, states).
:- mode number_states(in, in, in, out) is det.

number_states([], _N, !States).
number_states([I | Is], N, !States) :-
	map__det_insert(!.States, I, N, !:States),
	number_states(Is, N + 1, !States).

:- pred actions1(list(items), rules, lookaheads, states, gotos, shifts,
		actiontable, actiontable, actionerrors, actionerrors).
:- mode actions1(in, in, in, in, in, in, in, out, in, out) is det.

actions1([], _Rules, _Lookaheads, _States, _Gotos, _Shifts, !Actions, !Errs).
actions1([I | Is], Rules, Lookaheads, States, Gotos, Shifts, !Actions, !Errs) :-
	map__lookup(States, I, Sn),
	set__to_sorted_list(I, IList),
	actions2(IList, I, Sn, Rules, Lookaheads, States, Gotos, Shifts, 
		!Actions, !Errs),
	actions1(Is, Rules, Lookaheads, States, Gotos, Shifts, !Actions, !Errs).

:- pred actions2(list(item), items, state, rules, lookaheads, states, gotos,
		shifts, actiontable, actiontable, actionerrors, actionerrors).
:- mode actions2(in, in, in, in, in, in, in, in, in, out, in, out) is det.

actions2([], _I, _Sn, _Rules, _LA, _States, _Gotos, _Shifts, !Actions, !Errs).
actions2([A | As], I, Sn, Rules, LA, States, Gotos, Shifts, !Actions, !Errs) :-
	A = item(Ip, Id),
	map__lookup(Rules, Ip, rule(_, _, Syms, _, _, _, _)),
	array__max(Syms, Max),
	( Id =< Max ->
		array__lookup(Syms, Id, X),
		map__lookup(Gotos, I, IGs),
		(
			X = terminal(T0),
			Ts = { T0 }
		;
			X = nonterminal(N),
			( map__search(Shifts, N, Ts0) ->
				Ts = Ts0
			;
				Ts = empty
			)
		),
		set__to_sorted_list(Ts, TList),
		list__foldl2((pred(T::in, !.Actions::in, !:Actions::out, 
				!.Errs::in, !:Errs::out) is det :-
			map__lookup(IGs, terminal(T), J),
			map__lookup(States, J, Jn),
			addaction(Sn, T, shift(Jn), !Actions, !Errs)
		), TList, !Actions, !Errs)
	;
		% A -> alpha .
		(
			map__search(LA, I, ILAs),
			map__search(ILAs, A, Alphas)
		->
			set__to_sorted_list(Alphas, AlphaList),
			list__foldl2((pred(T::in,
					!.Actions::in, !:Actions::out,
					!.Errs::in, !:Errs::out) is det :-
				( Ip = 0, T = ($) ->
					addaction(Sn, T, accept, !Actions, 
						!Errs)
				;
					addaction(Sn, T, reduce(Ip), !Actions, 
						!Errs)
				)
			), AlphaList, !Actions, !Errs)
		;
			true
		)
	),
	actions2(As, I, Sn, Rules, LA, States, Gotos, Shifts, !Actions, !Errs).

:- pred addaction(state, terminal, action, actiontable, actiontable,
		actionerrors, actionerrors).
:- mode addaction(in, in, in, in, out, in, out) is det.

addaction(Sn, T, A0, !Actions, !Errs) :-
	( map__search(!.Actions, Sn, State0) ->
		State1 = State0
	;
		map__init(State1)
	),
	( map__search(State1, T, A1) ->
		( A0 = A1 ->
			A = A1
		;
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
		->
			A = A2,
			list__append([Err], !Errs)
		;
			A = A0,
			(
				A0 = shift(S0),
				A1 = shift(S1)
			->
				Err = error(shiftshift(S0, S1))
			;
				A0 = reduce(R0),
				A1 = reduce(R1)
			->
				Err = error(reducereduce(R0, R1))
			;
				Err = error(misc(A0, A1))
			),
			list__append([Err], !Errs)
		)
	;
		A = A0
	),
	map__set(State1, T, A, State),
	map__set(!.Actions, Sn, State, !:Actions).

%------------------------------------------------------------------------------%

gotos(_C, States, Gotos, !:GotoTable) :-
	map__init(!:GotoTable),
	map__foldl((pred(I0::in, IGs::in, !.GotoTable::in, 
			!:GotoTable::out) is det :-
		map__lookup(States, I0, Sf),
		map__foldl((pred(Sym::in, J0::in, !.GotoTable::in, 
				!:GotoTable::out) is det :-
			( Sym = nonterminal(N) ->
				map__lookup(States, J0, St),
				( map__search(!.GotoTable, Sf, X0) ->
					X1 = X0
				;
					map__init(X1)
				),
				map__set(X1, N, St, X),
				map__set(!.GotoTable, Sf, X, !:GotoTable)
			;
				true
			)
		), IGs, !GotoTable)
	), Gotos, !GotoTable).
