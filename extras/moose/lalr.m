%----------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%----------------------------------------------------------------------------%
%
% file: lalr.m
% main author: conway
%
% This module builds the lalr items and lookaheads for the grammar.
%
%------------------------------------------------------------------------------%
:- module lalr.

:- interface.

:- import_module grammar, misc.
:- import_module int, io, set.

:- type item
	--->	item(prodnum, dot).

:- type items	== set(item).

:- type gotos	== (items -> symbol -> items).

:- type lr1item
	--->	item(prodnum, dot, terminal).

:- type lr1items == set(lr1item).

:- type prodnum	== int.

:- type dot	== int.

:- type reaching == (nonterminal -> set(nonterminal)).

:- type propaheads == (items -> item -> items -> items).

:- type lookaheads == (items -> item -> set(terminal)).

:- type previews == (lookaheads - propaheads).

:- pred reaching(rules, first, reaching).
:- mode reaching(in, in, out) is det.

:- pred lr0items(rules, reaching, set(items), gotos).
:- mode lr0items(in, in, out, out) is det.

:- pred lookaheads(set(items), gotos, rules, first, index, lookaheads,
		io__state, io__state).
:- mode lookaheads(in, in, in, in, in, out, di, uo) is det.

:- implementation.

:- import_module array, bool, list, map, require, std_util, term.

%------------------------------------------------------------------------------%

reaching(Productions, First, Reaching) :-
	prodnums(Productions, ProdNums),
	init(Reaching0),
	reaching(ProdNums, Productions, First, no, Reaching0, Reaching).

:- pred reaching(list(prodnum), rules, first, bool, reaching, reaching).
:- mode reaching(in, in, in, in, in, out) is det.

reaching([], _Productions, _First, no, !Reaching).
reaching([], Productions, First, yes, !Reaching) :-
	prodnums(Productions, ProdNums),
	reaching(ProdNums, Productions, First, no, !Reaching).
reaching([ProdNum|ProdNums], Productions, First, !.Change, !Reaching) :-
	map__lookup(Productions, ProdNum, Prod),
	Prod = rule(NonTerminal, _Head, Symbols, _, _, _V, _C),
	array__max(Symbols, PMax),
	reaching_2(0, PMax, Symbols, First, NonTerminal, !Change, !Reaching),
	reaching(ProdNums, Productions, First, !.Change, !Reaching).

:- pred reaching_2(int, int, symbols, first, nonterminal, bool, bool,
		reaching, reaching).
:- mode reaching_2(in, in, in, in, in, in, out, in, out) is det.

reaching_2(SN, Max, Symbols, First, C, !Change, !Reaching) :-
	( SN > Max ->
		true
	;
		array__lookup(Symbols, SN, Symbol),
		(
			Symbol = terminal(_)
		;
			Symbol = nonterminal(A),
			reaches(C, A, !Change, !Reaching),
			( map__search(!.Reaching, A, AR) ->
				set__to_sorted_list(AR, ARList),
				list__foldl2(reaches(C), ARList, !Change, 
					!Reaching)
			;
				true
			),
			map__lookup(First, A, FirstA),
			( set__member(epsilon, FirstA) ->
				reaching_2(SN + 1, Max, Symbols, First, C, 
					!Change, !Reaching)
			;
				true
			)
		)
	).

:- pred reaches(nonterminal, nonterminal, bool, bool, reaching, reaching).
:- mode reaches(in, in, in, out, in, out) is det.

reaches(C, A, !Change, !Reaching) :-
	( map__search(!.Reaching, C, As0) ->
		( set__member(A, As0) ->
			true
		;
			!:Change = yes,
			As = As0 \/ { A },
			map__set(!.Reaching, C, As, !:Reaching)
		)
	;
		!:Change = yes,
		As = { A },
		map__set(!.Reaching, C, As, !:Reaching)
	).

%------------------------------------------------------------------------------%

lr0items(Productions, Reaching, C, Gotos) :-
	I0 = { item(0, 0) },
	C0 = { I0 },
	Pending = { I0 },
	map__init(Gotos0),
	lr0items1(Pending, Productions, Reaching, Gotos0, Gotos, C0, C).

:- pred lr0items1(set(items), rules, reaching, gotos, gotos,
		set(items), set(items)).
:- mode lr0items1(in, in, in, in, out, in, out) is det.

lr0items1(Pending0, Productions, Reaching, !Gotos, !C) :-
	( set__remove_least(Pending0, J, Pending1) ->
		set__to_sorted_list(J, JList),
		lr0items_1(JList, J, Productions, Reaching, !Gotos, empty, 
			NewSet),
		set__to_sorted_list(NewSet, NewItems),
		list__map((pred(Pair::in, J0::out) is det :-
			Pair = I0 - X,
			map__lookup(!.Gotos, I0, I0Gotos),
			map__lookup(I0Gotos, X, J0)
		), NewItems, PendingList),
		set__list_to_set(PendingList, NewPending0),
		NewPending = NewPending0 - !.C,
		!:C = !.C \/ NewPending,
		Pending = Pending1 \/ NewPending,
		lr0items1(Pending, Productions, Reaching, !Gotos, !C)
	;
		true	
	).

:- type new == set(pair(items, symbol)).

:- pred lr0items_1(list(item), items, rules, reaching, gotos, gotos, new, new).
:- mode lr0items_1(in, in, in, in, in, out, in, out) is det.

lr0items_1([], _I, _Productions, _Reaching, !Gotos, !New).
lr0items_1([BItem | RestItems], I, Productions, Reaching, !Gotos, !New) :-
	BItem = item(BProdNum, BDot),
	map__lookup(Productions, BProdNum, BProd),
	BProd = rule(_NonTerminal, _Head, BSyms, _, _, _V, _C),
	array__max(BSyms, BMax),
	(
		BDot =< BMax
	->
		array__lookup(BSyms, BDot, X),
		addgoto(I, X, item(BProdNum, BDot + 1), !Gotos, !New)
	;
		true
	),
	(
		BDot =< BMax,
		lookup(BSyms, BDot, nonterminal(C))
	->
		( map__search(Reaching, C, As) ->
			set__to_sorted_list(As, AXList)
		;
			AXList = []
		),
		addAs([C|AXList], I, Productions, !Gotos, !New)
	;
		true
	),
	lr0items_1(RestItems, I, Productions, Reaching, !Gotos, !New).

:- pred addgoto(items, symbol, item, gotos, gotos, new, new).
:- mode addgoto(in, in, in, in, out, in, out) is det.

addgoto(I, X, NewItem, !Gotos, !New) :-
	( map__search(!.Gotos, I, IGotos0) ->
		IGotos1 = IGotos0
	;
		init(IGotos1)
	),
	( map__search(IGotos1, X, GotoIX0) ->
		GotoIX1 = GotoIX0
	;
		GotoIX1 = empty
	),
	GotoIX = GotoIX1 \/ { NewItem },
	set(IGotos1, X, GotoIX, IGotos),
	set(!.Gotos, I, IGotos, !:Gotos),
	( GotoIX \= GotoIX1 ->
		!:New = !.New \/ { I - X }
	;
		true
	).

:- pred addAs(list(nonterminal), items, rules, gotos, gotos, new, new).
:- mode addAs(in, in, in, in, out, in, out) is det.

addAs([], _I, _Productions, !Gotos, !New).
addAs([A|As], I, Productions, !Gotos, !New) :-
	prodnums(Productions, ProdNums),
	addAs_2(ProdNums, A, I, Productions, !Gotos, !New),
	addAs(As, I, Productions, !Gotos, !New).

:- pred addAs_2(list(prodnum), nonterminal, items, rules, gotos, gotos,
		new, new).
:- mode addAs_2(in, in, in, in, in, out, in, out) is det.

addAs_2([], _A, _I, _Productions, !Gotos, !New).
addAs_2([Pn|Pns], A, I, Productions, !Gotos, !New) :-
	map__lookup(Productions, Pn, Prod),
	(
		Prod = rule(A, _Head, Symbols, _, _, _V, _C),
		array__max(Symbols, Max),
		Max >= 0
	->
		array__lookup(Symbols, 0, X),
		addgoto(I, X, item(Pn, 1), !Gotos, !New)
	;
		true
	),
	addAs_2(Pns, A, I, Productions, !Gotos, !New).

%------------------------------------------------------------------------------%

lookaheads(C, Gotos, Rules, First, Index, !:Lookaheads, !IO) :-
	map__from_assoc_list([item(0, 0) - { ($) }], I0),
	map__from_assoc_list([{item(0, 0)} - I0], !:Lookaheads),
	map__init(Propaheads0),
	set__to_sorted_list(C, CList),
	lookaheads(CList, Gotos, Rules, First, Index,
		!.Lookaheads - Propaheads0, !:Lookaheads - Propaheads),
	%foldl((pred(_I::in, IPs::in, di, uo) is det -->
	%	foldl((pred(Item::in, ItemsMap::in, di, uo) is det -->
	%		write(Item), write_string(" :\n"),
	%		foldl((pred(ToItems::in, ToItem::in, di, uo) is det -->
	%			write_string("\t"),
	%			write(ToItems), nl,
	%			write_string("\t\t"),
	%			write(ToItem), nl
	%		), ItemsMap), nl
	%	), IPs), nl
	%), Propaheads),
	io__stderr_stream(StdErr, !IO),
	io__write_string(StdErr, "\tpropagating...\n", !IO),
	propagate(C, Propaheads, !Lookaheads).

:- pred lookaheads(list(items), gotos, rules, first, index, previews, previews).
:- mode lookaheads(in, in, in, in, in, in, out) is det.

lookaheads([], _Gotos, _Rules, _First, _Index, !Lookaheads).
lookaheads([K | Ks], Gotos, Rules, First, Index, !Lookaheads) :-
	set__to_sorted_list(K, KList),
	lookaheads1(KList, K, Gotos, Rules, First, Index, !Lookaheads),
	lookaheads(Ks, Gotos, Rules, First, Index, !Lookaheads).

:- pred lookaheads1(list(item), items, gotos, rules, first, index,
		previews, previews).
:- mode lookaheads1(in, in, in, in, in, in, in, out) is det.

lookaheads1([], _I, _Gotos, _Rules, _First, _Index, !Lookaheads).
lookaheads1([BItem | BItems], I, Gotos, Rules, First, Index, !Lookaheads) :-
	BItem = item(Bp, Bd),
	BItem0 = item(Bp, Bd, (*)),
	J0 = closure({ BItem0 }, Rules, First, Index),
	set__to_sorted_list(J0, JList0),
	    % Reverse the list so that in add_spontaneous, the 
	    % set insertions are in reverse sorted order not
	    % sorted order thereby taking to cost from O(n) to O(1).
	list__reverse(JList0, JList),
	lookaheads2(JList, BItem, I, Gotos, Rules, !Lookaheads),
	lookaheads1(BItems, I, Gotos, Rules, First, Index, !Lookaheads).

:- func closure(lr1items, rules, first, index) = lr1items.

closure(I0, Rules, First, Index) = I :-
	closure(Rules, First, Index, I0, I0, I).

:- pred closure(rules, first, index, lr1items, lr1items, lr1items).
:- mode closure(in, in, in, in, in, out) is det.

closure(Rules, First, Index, !.New, I0, I) :-
	set__to_sorted_list(!.New, NewList),
	closure1(NewList, Rules, First, Index, [I0], Is),
	do_union(Is, I1),
	!:New = I1 - I0,
	( set__empty(!.New) ->
		I = I1
	;
		closure(Rules, First, Index, !.New, I1, I)
	).

:- pred closure1(list(lr1item), rules, first, index,
		list(lr1items), list(lr1items)).
:- mode closure1(in, in, in, in, in, out) is det.

closure1([], _Rules, _First, _Index, !I).
closure1([AItem | AItems], Rules, First, Index, !I) :-
	AItem = item(Ap, Ad, Asym),
	map__lookup(Rules, Ap, rule(_, _, Asyms, _, _, _, _)),
	array__max(Asyms, AMax),
	( Ad =< AMax ->
		array__lookup(Asyms, Ad, BSym),
		( BSym = nonterminal(Bn) ->
			Bf0 = first(First, Asyms, Ad + 1),
			( set__member(epsilon, Bf0) ->
				set__delete(Bf0, epsilon, Bf1),
				set__insert(Bf1, Asym, Bf)
				%Bf = Bf1 \/ { Asym }
			;
				Bf = Bf0
			),
			set__to_sorted_list(Bf, BfList0),
			    % Reverse the list so that we construct
			    % the new items in reverse sorted order
			    % so that the accumulated list is in
			    % sorted order. Thus we don't have to
			    % sort the list to turn it into a set.
			    % Reduces running time by > 10%
			list__reverse(BfList0, BfList),
			map__lookup(Index, Bn, Bps),
			make_items(Bps, BfList, [], NList),
			set__sorted_list_to_set(NList, N),
			list__append([N], !I)
		;
			true	
		)
	;
		true	
	),
	closure1(AItems, Rules, First, Index, !I).

	% create the union of a list of sets.
	% The simple `foldl' way has O(n^2) cost, so we do a
	% pairwise union until there is only one set left.
	% This has a cost of O(n log n).
:- pred do_union(list(lr1items), lr1items).
:- mode do_union(in, out) is det.

do_union([], I) :-
	init(I).
do_union(Is, I) :-
	Is = [_|_],
	do_union(Is, [], I).

:- pred do_union(list(lr1items), list(lr1items), lr1items).
:- mode do_union(in, in, out) is det.

do_union([], [], _) :-
	error("do_union: empty list").
do_union([], Is, I) :-
	Is = [_|_],
	do_union(Is, [], I).
do_union([I], [], I).
do_union([I0], Is, I) :-
	Is = [_|_],
	do_union([I0|Is], [], I).
do_union([I0, I1|Is0], Is1, I) :-
	I2 = I0 \/ I1,
	do_union(Is0, [I2|Is1], I).

:- pred lookaheads2(list(lr1item), item, items, gotos, rules,
		previews, previews).
:- mode lookaheads2(in, in, in, in, in, in, out) is det.

lookaheads2([], _B, _I, _Gotos, _Rules, !Lookaheads).
lookaheads2([A | As], B, I, Gotos, Rules, !Lookaheads) :-
	A = item(Ap, Ad, Alpha),
	map__lookup(Rules, Ap, rule(_, _, ASyms, _, _, _, _)),
	array__max(ASyms, AMax),
	( Ad =< AMax ->
		array__lookup(ASyms, Ad, X),
		( Gix = goto(Gotos, I, X) ->
			Ad1 = Ad + 1,
			( Alpha = (*) ->
				add_propagated(I, B, Gix, item(Ap, Ad1), 
					!Lookaheads)
			;
				add_spontaneous(Gix, item(Ap, Ad1), Alpha, 
					!Lookaheads)
			)
		;
			true
		)
	;
		true
	),
	lookaheads2(As, B, I, Gotos, Rules, !Lookaheads).

:- pred make_items(list(prodnum), list(terminal), list(lr1item), list(lr1item)).
:- mode make_items(in, in, in, out) is det.

make_items([], _, !Items).
make_items([Bp | Bps], BfList, !Items) :-
	make_items1(Bp, BfList, !Items),
	make_items(Bps, BfList, !Items).

:- pred make_items1(prodnum, list(terminal), list(lr1item), list(lr1item)).
:- mode make_items1(in, in, in, out) is det.

make_items1(_, [], !Items).
make_items1(Bp, [Bt | Bts], !Items) :-
	list__append([item(Bp, 0, Bt)], !Items),
	make_items1(Bp, Bts, !Items).

:- func goto(gotos, items, symbol) = items.
:- mode (goto(in, in, in) = out) is semidet.

goto(Gotos, I, X) = A :-
	map__search(Gotos, I, IXs),
	map__search(IXs, X, A).

:- pred add_propagated(items, item, items, item, previews, previews).
:- mode add_propagated(in, in, in, in, in, out) is det.

add_propagated(I, B, Ia, A, L - P0, L - P) :-
	( map__search(P0, I, X0) ->
		X1 = X0
	;
		map__init(X1)
	),
	( map__search(X1, B, Y0) ->
		Y1 = Y0
	;
		map__init(Y1)
	),
	( map__search(Y1, Ia, As0) ->
		As1 = As0
	;
		As1 = empty
	),
	set__insert(As1, A, As),
	map__set(Y1, Ia, As, Y),
	map__set(X1, B, Y, X),
	map__set(P0, I, X, P).

:- pred add_spontaneous(items, item, terminal, previews, previews).
:- mode add_spontaneous(in, in, in, in, out) is det.

add_spontaneous(I, B, Alpha, L0 - P, L - P) :-
	( map__search(L0, I, X0) ->
		X1 = X0
	;
		map__init(X1)
	),
	( map__search(X1, B, As0) ->
		As1 = As0
	;
		As1 = empty
	),
	set__insert(As1, Alpha, As),
	map__set(X1, B, As, X),
	map__set(L0, I, X, L).

:- pred propagate(set(items), propaheads, lookaheads, lookaheads).
:- mode propagate(in, in, in, out) is det.

propagate(C, Props, !Lookaheads) :-
	set__to_sorted_list(C, CList),
	propagate(CList, Props, no, Change, !Lookaheads),
	(
		Change = no
	;
		Change = yes,
		propagate(C, Props, !Lookaheads)
	).

:- pred propagate(list(items), propaheads, bool, bool, lookaheads, lookaheads).
:- mode propagate(in, in, in, out, in, out) is det.

propagate([], _Props, !Change, !Lookaheads).
propagate([I | Is], Props, !Change, !Lookaheads) :-
	set__to_sorted_list(I, IList),
	propagate1(IList, I, Props, !Change, !Lookaheads),
	propagate(Is, Props, !Change, !Lookaheads).

:- pred propagate1(list(item), items, propaheads, bool, bool,
		lookaheads, lookaheads).
:- mode propagate1(in, in, in, in, out, in, out) is det.

propagate1([], _I, _Props, !Change, !Lookaheads).
propagate1([Item | Items], I, Props, !Change, !Lookaheads) :-
	(
		map__search(!.Lookaheads, I, X),
		map__search(X, Item, Ts),
		map__search(Props, I, Y),
		map__search(Y, Item, Ps)
	->
		map__keys(Ps, Pkeys),
		propagate2(Pkeys, Ps, Ts, !Change, !Lookaheads)
	;
		true
	),
	propagate1(Items, I, Props, !Change, !Lookaheads).

:- pred propagate2(list(items), (items -> items), set(terminal), bool, bool,
		lookaheads, lookaheads).
:- mode propagate2(in, in, in, in, out, in, out) is det.

propagate2([], _Ps, _Ts, !Change, !Lookaheads).
propagate2([I|Pks], Ps, Ts, !Change, !Lookaheads) :-
	map__lookup(Ps, I, Ips),
	set__to_sorted_list(Ips, IPList),
	propagate3(IPList, I, Ts, !Change, !Lookaheads),
	propagate2(Pks, Ps, Ts, !Change, !Lookaheads).

:- pred propagate3(list(item), items, set(terminal), bool, bool,
		lookaheads, lookaheads).
:- mode propagate3(in, in, in, in, out, in, out) is det.

propagate3([], _I, _Ts, !Change, !Lookaheads).
propagate3([Item | Items], I, Ts0, !Change, !Lookaheads) :-
	( map__search(!.Lookaheads, I, X0) ->
		X1 = X0
	;
		map__init(X1)
	),
	( map__search(X1, Item, Ts1) ->
		Ts2 = Ts1
	;
		Ts2 = empty
	),
	NewTs = Ts0 - Ts2,
	( not set__empty(NewTs) ->
		Ts = Ts2 \/ NewTs,
		map__set(X1, Item, Ts, X),
		map__set(!.Lookaheads, I, X, !:Lookaheads),
		!:Change = yes
	;
		true
	),
	propagate3(Items, I, Ts0, !Change, !Lookaheads).

%------------------------------------------------------------------------------%

:- pred prodnums(rules, list(prodnum)).
:- mode prodnums(in, out) is det.

prodnums(Rules, ProdNums) :-
	map__keys(Rules, ProdNums).

%------------------------------------------------------------------------------%
