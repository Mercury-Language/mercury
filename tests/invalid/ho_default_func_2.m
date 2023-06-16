%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ho_default_func_2.
:- interface.

:- include_module ho_default_func_2_helper_1.

:- import_module io.
:- import_module ho_default_func_2.ho_default_func_2_helper_1.

:- type t.

:- pred baz(id(t)::out) is det.
:- pred eq(t::in, t::out) is det.
:- pred do_io(t::in, io::di, io::uo) is det.

:- implementation.

:- type t == (func(int) = int).

baz(mkid(bar)).

eq(X, X).

do_io(F, !IO) :-
    io.write_int(F(42), !IO),
    io.nl(!IO).
