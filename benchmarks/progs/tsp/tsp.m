% vim: ft=mercury ts=4 sw=4 et
%
% A port of tsp.pl from the Reform Prolog project.
% http://user.it.uu.se/~thomasl/reform.html
%
% I'm not sure how accurate the port is.
%-----------------------------------------------------------------------------%

:- module tsp.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module int.
:- import_module list.

:- type path    == list(int).
:- type matrix  == list(list(int)).

%-----------------------------------------------------------------------------%

main(!IO) :-
    test_tsp(80, Path, Cost, Time, 1234, _),
    io.write_line({Path, Cost, Time}, !IO).

:- pred test_tsp(int::in, path::out, int::out, int::out, int::in, int::out)
    is cc_multi.

test_tsp(N, Path, Cost, Time, Seed, NewSeed) :-
    random_matrix(N, Matrix, Seed, NewSeed),
%   statistics(instr, [SeqA, ParA]),
%   statistics(walltime, [T1 | _]),
    tsp(N, N, Matrix, Path, Cost), 
%   statistics(walltime, [T2 | _]),
%   statistics(instr, [SeqB, ParB]),
%   Time is T2 - T1,
%   Instr is (SeqB + ParB) - (SeqA + ParA),
%   format('map: ~w instructions executed~n', [Instr]).
    Time = 0.

:- pred tsp(int::in, int::in, matrix::in, path::out, int::out) is cc_multi.

tsp(V, N, Matrix, Path, Cost) :-
    tsp(V, N, Matrix, [], 100000, Path, Cost).

:- pred tsp(int::in, int::in, matrix::in, path::in, int::in,
    path::out, int::out) is cc_multi.

tsp(V, N, Matrix, MinP, MinC, P, C) :-
    ( if V = 0 then
        P = MinP,
        C = MinC
    else
        (
            all_but_this_vertex(N, V, Vs1),
            ( if travel(Vs1, Matrix, [V], P1) then
                cost([V | P1], Matrix, C1),
                update_minimum1(C1, P1, V, MinC, MinP, MinC1, MinP1)
            else
                % Hack to make tsp not fail.
                MinC1 = 100000,
                MinP1 = []
            )
        &
            tsp(V - 1, N, Matrix, MinP1, MinC1, P, C)
        )
    ).

:- pred all_but_this_vertex(int::in, int::in, list(int)::out) is det.

all_but_this_vertex(N, M, X) :-
    ( if N = 0 then
        X = []
    else if N = M then
        all_but_this_vertex(N - 1, N, X)
    else
        all_but_this_vertex(N - 1, M, Vs),
        X = [N | Vs]
    ).

:- pred travel(path::in, matrix::in, path::in, path::out) is nondet.

travel([], _, P, P).
travel([V | Vs], Matrix, P0, P) :-
    minimal_cost([V | Vs], Matrix, P0, U, W), 
    delete_vertex([V | Vs], U, Vs1), 
    ( if append_once(Prefix, [W | Suffix], P0) then
        append_once(Prefix, [U, W | Suffix], P1),
        travel(Vs1, Matrix, P1, P)
    else
        fail
    ).

:- pred minimal_cost(path::in, matrix::in, path::in, int::out, int::out)
    is det.

minimal_cost(Vertices, Matrix, Path, U, W) :-
    Dummy = -1,
    minimal_cost(Vertices, Matrix, Path, U, W, 100000, Dummy, Dummy).

:- pred minimal_cost(list(int)::in, matrix::in, path::in, int::out, int::out,
    int::in, int::in, int::in) is det.

minimal_cost([], _, _, U0, W0, _, U1, W1) :-
    U0 = U1, W0 = W1.
minimal_cost([V | Vs], Matrix, Path, U, W, MinC, MinU, MinW) :-
    list.det_index1(Matrix, V) = Row,
    row_min(Path, Row, W1, C),
    update_minimum2(C, V, W1, MinC, MinU, MinW, MinC1, MinU1, MinW1),
    minimal_cost(Vs, Matrix, Path, U, W, MinC1, MinU1, MinW1).

:- pred row_min(path::in, list(int)::in, int::out, int::out) is det.

row_min(Verteces, Row, W, Cost) :-
    Dummy = -1,
    row_min(Verteces, Row, W, Cost, Dummy, 100000).

:- pred row_min(list(int)::in, list(int)::in, int::out, int::out,
    int::in, int::in) is det.

row_min([], _, W0, C0, W1, C1) :-
    W0 = W1, C0 = C1.
row_min([V | Vs], Row, W, C, MinW, MinC) :-
    list.det_index1(Row, V) = C1,
    update_minimum3(C1, V, MinC, MinW, MinC1, MinW1),
    row_min(Vs, Row, W, C, MinW1, MinC1). 

:- pred cost(list(int)::in, list(list(int))::in, int::out) is det.

cost(Path, Matrix, Cost) :-
    cost(Path, Matrix, Cost, 0).

:- pred cost(list(int)::in, list(list(int))::in, int::out, int::in) is det.

cost([], _, _, _) :-
    error($pred).
cost([_], _, Cost0, Cost1) :-
    Cost0 = Cost1.
cost([V1, V2 | Vs], Matrix, Cost, Cost0) :-
    list.det_index1(Matrix, V1) = Row,
    list.det_index1(Row, V2) = C,
    Cost1 = Cost0 + C,
    cost([V2 | Vs], Matrix, Cost, Cost1).

:- pred delete_vertex(list(int)::in, int::in, list(int)::out) is det.

delete_vertex([], _, []).
delete_vertex([V | Vs], U, Ws) :-
    ( if V = U then
        delete_vertex(Vs, U, Ws)
    else
        delete_vertex(Vs, U, Ws0),
        Ws = [V | Ws0]
    ).

:- pred update_minimum1(int::in, path::in, int::in, int::in, path::in,
        int::out, path::out) is det.

update_minimum1(C, P, V, MinC, MinP, MinC1, MinP1) :-
    ( if MinC =< C then
        MinC1 = MinC,
        MinP1 = MinP
    else
        MinC1 = C,
        MinP1 = [V | P]
    ).

:- pred update_minimum2(int::in, int::in, int::in, int::in, int::in, int::in,
        int::out, int::out, int::out) is det.

update_minimum2(C, U, W, MinC, MinU, MinW, MinC1, MinU1, MinW1) :-
    ( if MinC =< C then
        MinC1 = MinC,
        MinU1 = MinU,
        MinW1 = MinW
    else
        MinC1 = C,
        MinU1 = U,
        MinW1 = W
    ).

:- pred update_minimum3(int::in, int::in, int::in, int::in, int::out, int::out)
        is det.

update_minimum3(C, W, MinC, MinW, MinC1, MinW1) :-
    ( if MinC =< C then
        MinC1 = MinC,
        MinW1 = MinW    % bug in original!
    else
        MinC1 = C,
        MinW1 = W
    ).

:- pred random_matrix(int::in, matrix::out, int::in, int::out) is det.

random_matrix(N, Matrix, S0, S1) :-
    Limit = (N + N/2) // 1,
    random_rows(N, N, Limit, MatrixRows, S0, S1),
    Matrix = MatrixRows.

:- pred random_rows(int::in, int::in, int::in, list(list(int))::out,
        int::in, int::out) is det.

random_rows(N, Dim, Limit, X, !S) :-
    ( if N = 0 then
        X = []
    else
        random_row(Dim, Limit, Row, !S),
        random_rows(N - 1, Dim, Limit, Rows, !S),
        X = [Row | Rows]
    ).

:- pred random_row(int::in, int::in, list(int)::out, int::in, int::out) is det.

random_row(Dim, Limit, Row, S0, S1) :-
    random_elts(Dim, Limit, Elts, S0, S1),
    Row = Elts.

:- pred random_elts(int::in, int::in, list(int)::out, int::in, int::out)
    is det.

random_elts(N, Limit, X, !S) :-
    ( if N = 0 then
        X = []
    else
        random(Limit, X0, !S),
        random_elts(N - 1, Limit, Xs, !S),
        X = [X0 | Xs]
    ).

:- pred append_once(list(int), list(int), list(int)).
:- mode append_once(out, out, in) is multi.
:- mode append_once(in, in, out) is det.

append_once([], Y, Y).
append_once([A | X], Y, [A | Z]) :-
    append_once(X, Y, Z).

:- pred random(int::in, int::out, int::in, int::out) is det.

random(Limit, N, S, S1) :-
    N = (S `mod` Limit) + 1,
    S1 = (125 * S + 1) `mod` 4096.
