:- module polymorphic_unification.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	[].

:- import_module list.

:- pred p(T, T).
:- mode p(in, list_skel >> dead).

p(X, X).

