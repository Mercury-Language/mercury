:- module aditi_update.
:- interface.

:- import_module aditi.

:- pred aditi_update(aditi__state::aditi_di, aditi__state::aditi_uo) is det.

:- implementation.

:- import_module int.

:- pred p(aditi__state::aditi_ui, int::out, int::out) is nondet.
:- pragma base_relation(p/3).

:- func q(aditi__state::aditi_ui, int::out) = (int::out) is nondet.
:- pragma base_relation(q/2).

aditi_update -->
	aditi_insert(p(_, 1, 2)),
	aditi_insert(q(_, 1) = 2),

	aditi_delete(p(_, X, _Y) :- X < 2),
	aditi_delete(q(_, X) = _Y :- X < 2),

	{ DeleteP =
		(aditi_top_down pred(_::unused, 1::in, 2::in) is semidet :-
			true
		) },
	aditi_delete(pred p/3, DeleteP),

	{ DeleteQ =
		(aditi_top_down func(_::unused, 1::in) = (2::in) is semidet)
	},
	aditi_delete(func q/2, DeleteQ),

	{ InsertP =
	    (aditi_bottom_up pred(_::aditi_ui, A1::out, A2::out) is nondet :-
		( A1 = 1, A2 = 2
		; A1 = 2, A2 = 3
		)
	    ) },
	aditi_bulk_insert(pred p/3, InsertP),
	aditi_bulk_delete(pred p/3, InsertP),

	{ InsertQ =
	    (aditi_bottom_up func(_::aditi_ui, A1::out)
	    		= (A2::out) is nondet :-
		( A1 = 1, A2 = 2
		; A1 = 2, A2 = 3
		)
	    ) },
	aditi_bulk_insert(func q/2, InsertQ),
	aditi_bulk_delete(func q/2, InsertQ),

	aditi_modify(p(_, X0, Y0) ==> p(_, X0 + 1, Y0 + 1)),
	aditi_modify((p(_, X0, Y0) ==> p(_, X, Y) :-
			X = X0 + 1,
			Y = Y0 + 1
		)),
	aditi_modify((q(_, X0) = Y0) ==> (q(_, X0 + 1) = (Y0 + 1))),

	{ ModifyP =
	    (aditi_top_down pred(_::unused, X0::in, Y0::in,
			_::unused, X::out, Y::out) is semidet :-
		X0 = 1,
		X = X0 + Y0,
		Y = X0 - Y0
	    ) },
	aditi_modify(pred p/3, ModifyP),

	{ ModifyQ =
	    (aditi_top_down pred(_::unused, X0::in, Y0::in,
			_::unused, X::out, Y::out) is semidet :-
		X0 = 1,
		X = X0 + Y0,
		Y = X0 - Y0
	    ) },
	aditi_modify(func q/2, ModifyQ).

