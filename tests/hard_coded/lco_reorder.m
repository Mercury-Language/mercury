%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module lco_reorder.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.

%---------------------------------------------------------------------------%

main(!IO) :-
    Cs = dup_literal(10000000),
    io.write_string("length: ", !IO),
    io.write_int(len(Cs, 0), !IO),
    io.nl(!IO),

    GTs = dup_ground_term(10000000),
    io.write_string("length: ", !IO),
    io.write_int(len(GTs, 0), !IO),
    io.nl(!IO).

:- func dup_literal(int) = list(char).

dup_literal(N) = Xs :-
    ( if N > 0 then
        Xs0 = dup_literal(N - 1),
        % Previously the goal which constructs the literal would not be moved
        % before the recursive goal.
        Xs = ['A' | Xs0]
    else
        Xs = []
    ).

:- func dup_ground_term(int) = list(list(char)).

dup_ground_term(N) = Xs :-
    ( if N > 0 then
        Xs0 = dup_ground_term(N - 1),
        GT = ['A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'],
        Xs = [GT | Xs0]
    else
        Xs = []
    ).

:- func len(list(T), int) = int.

len([], N) = N.
len([_ | Xs], N) = len(Xs, N + 1).
