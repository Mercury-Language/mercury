:- module ho_default_func_2__sub.
:- interface.

:- type t.

:- pred baz(id(t)::out) is det.
:- pred eq(t::in, t::out) is det.
:- pred do_io(t::in, io__state::di, io__state::uo) is det.

:- implementation.

:- type t == (func(int) = int).

baz(mkid(bar)).

eq(X,X).

do_io(F) --> io__write_int(F(42)), nl.

