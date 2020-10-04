%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module compare_rep_array.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module array.
:- import_module list.
:- import_module pair.
:- import_module univ.

main(!IO) :-
    test(d1, d2, !IO),
    test(d2, d3, !IO),
    test(d3, d3, !IO).

:- type val == pair(string, univ).

:- func d1 = val.
d1 = "{1, 2, 3} : array(int)" - univ(array([1, 2, 3])).

:- func d2 = val.
d2 = "{1, 4, 9} : array(int)" - univ(array([1, 4, 9])).

:- func d3 = val.
d3 = "{1.0, 1.1, 1.2} : array(float)" - univ(array([1.0, 1.1, 1.2])).

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
