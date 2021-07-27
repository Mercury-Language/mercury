%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Compiling this module should generate an error message since
% it tries to cast a non-standard func inst to ground.

:- module ho_default_func_3.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module univ.

main(!IO) :-
    baz(foo, F),
    io__write_int(F(42), !IO),
    io.nl(!IO).

:- func foo(int) = int.

foo(X) = X + 1.

:- func bar(int) = int.
:- mode bar(di) = uo is det.

bar(X) = X.

:- pred baz(T::in, T::out) is det.

baz(X, Y) :-
    ( if univ_to_type(univ(bar), Y0) then
        Y = Y0
    else
        Y = X
    ).
