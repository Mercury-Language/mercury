:- module aditi_update.
:- interface.

:- import_module aditi.

:- pred aditi_update(aditi__state::aditi_di, aditi__state::aditi_uo) is det.

:- implementation.

:- import_module int.

:- pred p(aditi__state::aditi_mui, int::out, int::out) is nondet.
:- pragma base_relation(p/3).

:- func q(aditi__state::aditi_mui, int::out) = (int::out) is nondet.
:- pragma base_relation(q/2).

aditi_update -->
	aditi_insert(p(_, 1, 2)),
	aditi_insert(q(_, 1) = 2),

	aditi_delete(p(_, 1, 2)),
	aditi_delete(q(_, 1) = 2),

	aditi_bulk_insert(
		(p(_, X, Y) :-
			( X = 1, Y = 2
			; X = 2, Y = 3
			)
		)),

	aditi_bulk_delete(p(_, X, _) :- X < 2),
	aditi_bulk_delete(q(_, X) = _ :- X < 2),

	{ InsertP =
	    (aditi_bottom_up pred(_::aditi_mui, A1::out, A2::out) is nondet :-
		( A1 = 1, A2 = 2
		; A1 = 2, A2 = 3
		)
	    ) },
	aditi_bulk_insert(pred p/3, InsertP),
	aditi_bulk_delete(pred p/3, InsertP),

	aditi_bulk_insert(
		(p(_, A1, A2) :-
			( A1 = 1, A2 = 2
			; A1 = 2, A2 = 3
			)
		)
	),

	{ InsertQ =
	    (aditi_bottom_up func(_::aditi_mui, A1::out)
	    		= (A2::out) is nondet :-
		( A1 = 1, A2 = 2
		; A1 = 2, A2 = 3
		)
	    ) },
	aditi_bulk_insert(func q/2, InsertQ),
	aditi_bulk_delete(func q/2, InsertQ),

	aditi_bulk_insert(
		(q(_, A1) = A2 :-
			( A1 = 1, A2 = 2
			; A1 = 2, A2 = 3
			)
		)
	),

	aditi_bulk_modify(p(_, X0, Y0) ==> p(_, X0 + 1, Y0 + 1)),
	aditi_bulk_modify((p(_, X0, Y0) ==> p(_, X, Y) :-
			X = X0 + 1,
			Y = Y0 + 1
		)),
	aditi_bulk_modify((q(_, X0) = Y0) ==> (q(_, X0 + 1) = (Y0 + 1))),

	{ ModifyP1 =
	    (aditi_bottom_up pred(DB::aditi_mui, X0::out, Y0::out,
			_::unused, X::out, Y::out) is nondet :-
		p(DB, X0, Y0),
		X0 = 1,
		X = X0 + Y0,
		Y = X0 - Y0
	    ) },
	aditi_bulk_modify(pred p/3, ModifyP1),

	{ ModifyQ =
	    (aditi_bottom_up pred(DB::aditi_mui, X0::out, Y0::out,
			_::unused, X::out, Y::out) is nondet :-
		q(DB, X0) = Y0,
		X0 = 1,
		X = X0 + Y0,
		Y = X0 - Y0
	    ) },
	aditi_bulk_modify(func q/2, ModifyQ).

