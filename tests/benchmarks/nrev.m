%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
%   nreverse
%
%   David H. D. Warren
%
%   "naive"-reverse a list of 30 integers

:- module nrev.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

main(!IO) :-
    data(Data),
    nreverse(Data, Out),
    print_list(Out, !IO).

:- pred data(list(int)::out) is det.

data([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30]).

:- pred nreverse(list(int)::in, list(int)::out) is det.

nreverse([X | L0], L) :-
    nreverse(L0, L1),
    concatenate(L1, [X], L).
nreverse([], []).

:- pred concatenate(list(int)::in, list(int)::in, list(int)::out) is det.

concatenate([X | L1], L2, [X | L3]) :-
    concatenate(L1, L2, L3).
concatenate([], L, L).

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
