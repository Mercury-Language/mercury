%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module compare_rep_usereq.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module pair.
:- import_module univ.

main(!IO) :-
    test(da, da, !IO),
    test(da, db, !IO).

:- type val == pair(string, univ).

:- func da = val.
da = "aa : foo" - univ(aa).

:- func db = val.
db = "bb : foo" - univ(bb).

:- type foo
    --->    aa
    ;       bb
    where equality is foo_eq.

:- pred foo_eq(foo::in, foo::in) is semidet.

foo_eq(_, _) :-
    semidet_succeed.

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
