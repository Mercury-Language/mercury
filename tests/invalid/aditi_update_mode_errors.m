:- module aditi_update_mode_errors.

:- interface.

:- import_module aditi.

:- pred aditi_update_modes1(aditi__state::aditi_di,
	aditi__state::aditi_uo) is det.
:- pred aditi_update_modes2(aditi__state::aditi_di,
	aditi__state::aditi_uo) is det.
:- pred aditi_update_modes3(aditi__state::aditi_di,
	aditi__state::aditi_uo) is det.
:- pred aditi_update_modes4(aditi__state::aditi_di,
	aditi__state::aditi_uo) is det.
:- pred aditi_update_modes5(aditi__state::aditi_di,
	aditi__state::aditi_uo) is det.
:- pred aditi_update_modes6(aditi__state::aditi_di,
	aditi__state::aditi_uo) is det.
:- pred aditi_update_modes7(aditi__state::aditi_di,
	aditi__state::aditi_uo) is det.
:- pred aditi_update_modes8(aditi__state::aditi_di,
	aditi__state::aditi_uo) is det.
:- pred aditi_update_modes9(aditi__state::aditi_di,
	aditi__state::aditi_uo) is det.
:- pred aditi_update_modes10(aditi__state::aditi_di,
	aditi__state::aditi_uo) is det.
:- pred aditi_update_modes11(aditi__state::aditi_di,
	aditi__state::aditi_uo) is det.
:- pred aditi_update_modes12(aditi__state::aditi_di,
	aditi__state::aditi_uo) is det.
:- pred aditi_update_modes13(aditi__state::aditi_di,
	aditi__state::aditi_uo) is det.

:- implementation.

:- import_module int.

:- pred p(aditi__state::aditi_ui, int::out, int::out) is nondet.
:- pragma base_relation(p/3).

:- func q(aditi__state::aditi_ui, int::out) = (int::out) is nondet.
:- pragma base_relation(q/2).

:- pred anc(aditi__state::aditi_ui, int::out, int::out) is nondet.
:- pragma aditi(anc/3).

anc(_, X, X).
anc(DB, X, Y) :-
	p(DB, X, Z),
	anc(DB, Z, Y).

aditi_update_modes1 -->
	aditi_insert(p(_, _, 2)).

aditi_update_modes2 -->
	aditi_insert(q(_, _) = 2).

aditi_update_modes3 -->
	{ aditi_insert(p(_, 1, 2), _, _) }.

aditi_update_modes4 -->
	aditi_bulk_delete(p(_, _X, _Y) :- X < 2).

aditi_update_modes5 -->
	aditi_bulk_delete(q(_, _X) = _Y :- X < 2).

aditi_update_modes6 -->
	{ DeleteP =
		(aditi_bottom_up pred(_::unused, 1::out, 2::in) is nondet :-
			true
		) },
	aditi_bulk_delete(pred p/3, DeleteP).

aditi_update_modes7 -->
	{ DeleteQ =
		(aditi_bottom_up func(_::unused, 1::out) = (2::in) is nondet)
	},
	aditi_bulk_delete(func q/2, DeleteQ).

aditi_update_modes8 -->
	{ InsertP =
	    (aditi_bottom_up pred(_::aditi_ui, A1::in, A2::out) is nondet :-
		( A1 = 1, A2 = 2
		; A1 = 2, A2 = 3
		)
	    ) },
	aditi_bulk_insert(pred p/3, InsertP),
	aditi_bulk_delete(pred p/3, InsertP).

aditi_update_modes9 -->
	{ InsertQ =
	    (aditi_bottom_up func(_::aditi_ui, A1::in)
	    		= (A2::out) is nondet :-
		( A1 = 1, A2 = 2
		; A1 = 2, A2 = 3
		)
	    ) },
	aditi_bulk_insert(func q/2, InsertQ),
	aditi_bulk_delete(func q/2, InsertQ).

aditi_update_modes10 -->
	aditi_bulk_modify(p(_, _X0, Y0) ==> p(_, X0 + 1, Y0 + 1)).

aditi_update_modes11 -->
	aditi_bulk_modify((q(_, _X0) = Y0) ==> (q(_, X0 + 1) = (Y0 + 1))).

aditi_update_modes12 -->
	{ ModifyP =
	    (aditi_bottom_up pred(_::unused, X0::out, Y0::out,
			_::unused, X::in, Y::in) is semidet :-
		X0 = 1,
		X = X0 + Y0,
		Y = X0 - Y0
	    ) },
	aditi_bulk_modify(pred p/3, ModifyP).

aditi_update_modes13 -->
	{ ModifyQ =
	    (aditi_bottom_up pred(_::unused, X0::in, Y0::in,
			_::unused, X::in, Y::in) is nondet :-
		X0 = 1,
		X = X0 + Y0,
		Y = X0 - Y0
	    ) },
	aditi_bulk_modify(func q/2, ModifyQ).

