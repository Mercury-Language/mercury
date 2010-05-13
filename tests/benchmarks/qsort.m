% vim: ts=4 sw=4 et ft=mercury
%
%   qsort
%
%   David H. D. Warren
%
%   quicksort a list of 50 integers

:- module qsort.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
    data(Data),
    qsort(Data, [], SortedData),
    print_list(SortedData, !IO).

:- pred data(list(int)::out) is det.

data([27,74,17,33,94,18,46,83,65,2,32,53,28,85,99,47,28,82,6,11,55,29,39,81,
90,37,10,0,66,51,7,21,85,27,31,63,75,4,95,99,11,28,61,74,18, 92,40,53,59,8]).

:- pred qsort(list(int)::in, list(int)::in, list(int)::out) is det.

qsort([], !SortedRest).
qsort([H | T], !SortedRest) :-
    partition(T, H, Los, His),
    qsort(His, !SortedRest),
    !:SortedRest = [H | !.SortedRest],
    qsort(Los, !SortedRest).

:- pred partition(list(int), int, list(int), list(int)).
:- mode partition(in, in, out, out) is det.

partition([], _P, [], []).
partition([H | T], Pivot, Lo, Hi) :-
    ( H =< Pivot ->
        partition(T, Pivot, LoTail, Hi),
        Lo = [H | LoTail]
    ;
        partition(T, Pivot, Lo, HiTail),
        Hi = [H | HiTail]
    ).

:- pred print_list(list(int)::in, io::di, io::uo) is det.

print_list(Xs, !IO) :-
    (
        Xs = [],
        io.write_string("[]\n", !IO)
    ;
        Xs = [H | T],
        io.write_string("[", !IO),
        print_list_elements(H, T, !IO),
        io.write_string("]\n", !IO)
    ).

:- pred print_list_elements(int::in, list(int)::in, io::di, io::uo) is det.

print_list_elements(X, Xs, !IO) :-
    io.write_int(X, !IO),
    (
        Xs = []
    ;
        Xs = [H | T],
        io.write_string(", ", !IO),
        print_list_elements(H, T, !IO)
    ).
