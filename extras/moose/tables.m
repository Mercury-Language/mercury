%----------------------------------------------------------------------------%
% Copyright (C) 1998-2000 The University of Melbourne.
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

shifts(_C, _Rules, First, Reaching, Shifts) :-
	init(Shifts0),
	foldl((pred(N::in, Ts0::in, Ss0::in, Ss::out) is det :-
		( search(Reaching, N, Ns0) ->
			set__to_sorted_list(Ns0, Ns1)
		;
			Ns1 = []
		),
		map(lookup(First), Ns1, Ts1),
		foldl(union, Ts1, Ts0, Ts2),
		Ts = Ts2 - { epsilon },
		set(Ss0, N, Ts, Ss)
	), First, Shifts0, Shifts).

%------------------------------------------------------------------------------%

actions(C, Rules, Lookaheads, Gotos, Shifts, States, Actions, Errs) :-
	set__to_sorted_list(C, CList),
	init(States0),
	number_states(CList, 0, States0, States),
	init(Actions0),
	actions1(CList, Rules, Lookaheads, States, Gotos, Shifts,
		Actions0, Actions, [], Errs).

:- pred number_states(list(items), int, states, states).
:- mode number_states(in, in, in, out) is det.

number_states([], _N, States, States).
number_states([I|Is], N, States0, States) :-
	map__det_insert(States0, I, N, States1),
	number_states(Is, N + 1, States1, States).

:- pred actions1(list(items), rules, lookaheads, states, gotos, shifts,
		actiontable, actiontable, actionerrors, actionerrors).
:- mode actions1(in, in, in, in, in, in, in, out, in, out) is det.

actions1([], _Rules, _Lookaheads, _States, _Gotos, _Shifts,
		Actions, Actions, Errs, Errs).
actions1([I|Is], Rules, Lookaheads, States, Gotos, Shifts,
		Actions0, Actions, Errs0, Errs) :-
	lookup(States, I, Sn),
	set__to_sorted_list(I, IList),
	actions2(IList, I, Sn, Rules, Lookaheads, States, Gotos, Shifts,
		Actions0, Actions1, Errs0, Errs1),
	actions1(Is, Rules, Lookaheads, States, Gotos, Shifts,
		Actions1, Actions, Errs1, Errs).

:- pred actions2(list(item), items, state, rules, lookaheads, states, gotos,
		shifts, actiontable, actiontable, actionerrors, actionerrors).
:- mode actions2(in, in, in, in, in, in, in, in, in, out, in, out) is det.

actions2([], _I, _Sn, _Rules, _LA, _States, _Gotos, _Shifts,
		Actions, Actions, Errs, Errs).
actions2([A|As], I, Sn, Rules, LA, States, Gotos, Shifts,
		Actions0, Actions, Errs0, Errs) :-
	A = item(Ip, Id),
	lookup(Rules, Ip, rule(_, _, Syms, _, _, _, _)),
	array__max(Syms, Max),
	( Id =< Max ->
		lookup(Syms, Id, X),
		lookup(Gotos, I, IGs),
		(
			X = terminal(T0),
			Ts = { T0 }
		;
			X = nonterminal(N),
			( search(Shifts, N, Ts0) ->
				Ts = Ts0
			;
				Ts = empty
			)
		),
		set__to_sorted_list(Ts, TList),
		foldl2((pred(T::in, Ac0::in, Ac::out, in, out) is det -->
			{ lookup(IGs, terminal(T), J) },
			{ lookup(States, J, Jn) },
			addaction(Sn, T, shift(Jn), Ac0, Ac)
		), TList, Actions0, Actions1, Errs0, Errs1)
	;
		% A -> alpha .
		(
			search(LA, I, ILAs),
			search(ILAs, A, Alphas)
		->
			set__to_sorted_list(Alphas, AlphaList),
			foldl2((pred(T::in, A0::in, A1::out, in, out) is det -->
				( { Ip = 0, T = ($) } ->
					addaction(Sn, T, accept, A0, A1)
				;
					addaction(Sn, T, reduce(Ip), A0, A1)
				)
			), AlphaList, Actions0, Actions1, Errs0, Errs1)
		;
			Actions1 = Actions0,
			Errs1 = Errs0
		)
	),
	actions2(As, I, Sn, Rules, LA, States, Gotos, Shifts,
		Actions1, Actions, Errs1, Errs).

:- pred addaction(state, terminal, action, actiontable, actiontable,
		actionerrors, actionerrors).
:- mode addaction(in, in, in, in, out, in, out) is det.

addaction(Sn, T, A0, Actions0, Actions, Errs0, Errs) :-
	( search(Actions0, Sn, State0) ->
		State1 = State0
	;
		init(State1)
	),
	( search(State1, T, A1) ->
		( A0 = A1 ->
			A = A1,
			Errs = Errs0
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
			Errs = [Err|Errs0]
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
			Errs = [Err|Errs0]
		)
	;
		A = A0,
		Errs = Errs0
	),
	set(State1, T, A, State),
	set(Actions0, Sn, State, Actions).

%------------------------------------------------------------------------------%

gotos(_C, States, Gotos, GotoTable) :-
	init(GotoTable0),
	foldl((pred(I0::in, IGs::in, in, out) is det -->
		{ lookup(States, I0, Sf) },
		foldl((pred(Sym::in, J0::in, GT0::in, GT::out) is det :-
			( Sym = nonterminal(N) ->
				lookup(States, J0, St),
				( search(GT0, Sf, X0) ->
					X1 = X0
				;
					init(X1)
				),
				set(X1, N, St, X),
				set(GT0, Sf, X, GT)
			;
				GT = GT0
			)
		), IGs)
	), Gotos, GotoTable0, GotoTable).
