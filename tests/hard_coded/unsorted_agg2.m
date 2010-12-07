% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
% A simple test of solutions.unsorted_aggregate2/6.

:- module unsorted_agg2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.
:- import_module string.

main(!IO) :-
    unsorted_aggregate2(q, collect_q, [], DoubleQs, !IO),
    io.write(DoubleQs, !IO),
    io.nl(!IO).

:- pred collect_q(int::in, list(int)::in, list(int)::out,
    io::di, io::uo) is det.

collect_q(I, !DoubleQs, !IO) :-
    io.format("Collecting %d ... \n", [i(I)], !IO),
    !:DoubleQs = [ 2 * I | !.DoubleQs]. 

:- pred q(int::out) is multi.

q(3).
q(4).
q(5).
