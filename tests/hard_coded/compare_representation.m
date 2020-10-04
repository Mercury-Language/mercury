%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module compare_representation.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module pair.
:- import_module univ.

main(!IO) :-
    test(d1, d1, !IO),
    test(d1, dm, !IO),
    test(dm, dm, !IO),
    test(dm, dt, !IO),
    test(df1, df2, !IO).

:- type val == pair(string, univ).

:- func d1 = val.
d1 = "1 : int" - univ(1).

:- func dm = val.
dm = "main : pred(io__state, io__state)" - univ(main).

:- func dt = val.
dt = "test(d1, dm) : pred(io__state, io__state)" - univ(test(d1, dm)).

:- func df1 = val.
df1 = "foo(1) : func(int) = int" - univ(foo(1)).

:- func df2 = val.
df2 = "foo(2) : func(int) = int" - univ(foo(2)).

:- func foo(int, int) = int.
foo(_, Z) = Z.

:- pred test(val::in, val::in, io::di, io::uo) is cc_multi.

test(SA - A, SB - B, !IO) :-
    io.write_string(SA, !IO),
    io.nl(!IO),
    io.write_string(SB, !IO),
    io.nl(!IO),
    compare_representation(Res, A, B),
    io.write_string("Result = ", !IO),
    io.write(Res, !IO),
    io.write_string(".\n\n", !IO).
