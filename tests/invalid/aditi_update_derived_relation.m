:- module aditi_update_derived_relation.  

:- interface.

:- import_module aditi.

:- pred aditi_update_derived_relation(aditi__state::aditi_di,
	aditi__state::aditi_uo) is det.

:- implementation.

:- import_module int.

:- pred q(aditi__state::aditi_mui, int::out, int::out) is multi.
:- pragma aditi(q/3).

aditi_update_derived_relation -->
	aditi_insert(q(_, 1, 2)).	

q(_, 1, 1).
q(_, 2, 2).
