%----------------------------------------
% Near-optimal solution to the Traveling Salesman Problem.

test :-
	read(N),
	integer(N),
	test_tsp(N,Path,Cost,Time,17,_),
	format('tsp: path: ~w~ncost: ~w~ntime: ~w~n',[Path,Cost,Time]).

% test_tsp(+N, -Path, -Cost, -Time, +Seed, -NewSeed):

test_tsp(N, Path, Cost, Time, Seed, NewSeed) :-
	random_matrix(N, Matrix, Seed, NewSeed),
	statistics(instr,[SeqA, ParA]),
	statistics(walltime, [T1|_]),
	tsp(N, N, Matrix, Path, Cost), 
	statistics(walltime, [T2|_]),
	statistics(instr,[SeqB, ParB]),
	Time is T2-T1,
	Instr is (SeqB + ParB) - (SeqA + ParA),
	format('map: ~w instructions executed~n',[Instr]).

/**************************************************/

% tsp(+N, -Path, -Cost):
% Solves the TSP for a random graph of N nodes.

%tsp(N, Path, Cost) :- 
%	random_matrix(N, Matrix),
%	tsp(N, N, Matrix, Path, Cost).

tsp(V, N, Matrix, Path, Cost) :-
	tsp(V, N, Matrix, _, 100000, Path, Cost).

:- parallel([tsp/7]).

tsp(0, _, _, P0, C0, P1, C1) :- !, P0 = P1, C0 = C1.
tsp(V, N, Matrix, MinP, MinC, P, C) :-
	V > 0,
	V1 is V-1,
	all_but_this_vertex(N, V, Vs1),
	travel(Vs1, Matrix, [V], P1),
	cost([V|P1], Matrix, C1),
	update_minimum1(C1, P1, V, MinC, MinP, MinC1, MinP1),
	tsp(V1, N, Matrix, MinP1, MinC1, P, C).

all_but_this_vertex(0, _, X) :- !, X = [].
all_but_this_vertex(N, M, Vs) :-
	N =:= M, !,
	N1 is N-1,
	all_but_this_vertex(N1, N, Vs).
all_but_this_vertex(N, V, [N|Vs]) :-
	N1 is N-1,
	all_but_this_vertex(N1, V, Vs).

travel([], _, P0, P1) :- !, P0 = P1.
travel([V|Vs], Matrix, P0, P) :-
	minimal_cost([V|Vs], Matrix, P0, U, W), 
	delete_vertex([V|Vs], U, Vs1), 
	append_once(Prefix, [W|Suffix], P0),
	append_once(Prefix, [U,W|Suffix], P1),
	travel(Vs1, Matrix, P1, P).

% minimal_cost(Vertices+, Matrix+, Path+, U-, W-):
% U is a vertex among Vertices and W a vertex in Path, such that
% the distance from U to W is minimal.

minimal_cost(Vertices, Matrix, Path, U, W) :-
	minimal_cost(Vertices, Matrix, Path, U, W, 100000, _, _).

minimal_cost([], _, _, U0, W0, _, U1, W1) :- !, U0 = U1, W0 = W1.
minimal_cost([V|Vs], Matrix, Path, U, W, MinC, MinU, MinW) :-
	arg(V, Matrix, Row),
	row_min(Path, Row, W1, C),
	update_minimum2(C, V, W1, MinC, MinU, MinW, MinC1, MinU1, 
	                MinW1),
	minimal_cost(Vs, Matrix, Path, U, W, MinC1, MinU1, MinW1).

row_min(Verteces, Row, W, Cost) :-
	row_min(Verteces, Row, W, Cost, _, 100000).

row_min([], _, W0, C0, W1, C1) :- !, W0 = W1, C0 = C1.
row_min([V|Vs], Row, W, C, MinW, MinC) :-
	arg(V, Row, C1), 
	update_minimum3(C1, V, MinC, MinW, MinC1, MinW1),
	row_min(Vs, Row, W, C, MinW1, MinC1). 


cost(Path, Matrix, Cost) :-
	cost(Path, Matrix, Cost, 0).

cost([_], _, Cost0, Cost1) :- !, Cost0 = Cost1.
cost([V1,V2|Vs], Matrix, Cost, Cost0) :-
	arg(V1, Matrix, Row),
	arg(V2, Row, C),
	Cost1 is Cost0+C,
	cost([V2|Vs], Matrix, Cost, Cost1).

delete_vertex([], _, []).
delete_vertex([U|Vs], U, Ws) :- !,
	delete_vertex(Vs, U, Ws).
delete_vertex([V|Vs], U, [V|Ws]) :-
	delete_vertex(Vs, U, Ws).

update_minimum1(C, _, _, MinC, MinP, MinC1, MinP1) :-
	MinC =< C, !,
	MinC1 = MinC, MinP1 = MinP.
update_minimum1(C, P, V, _, _, C, [V|P]).

update_minimum2(C, _, _, MinC, MinU, MinW, MinC1, MinU1, MinW1) :-
	MinC =< C, !,
	MinC1 = MinC, MinU1 = MinU, MinW1 = MinW.
update_minimum2(C, V, W, _, _, _, C, V, W).

update_minimum3(C, _, MinC, MinW, MinC1, MinW1) :-
	MinC =< C, !,
	MinC1 = MinC, MinW = MinW.
update_minimum3(C, V, _, _, C, V).

/**************************************************/

% Generating random cost matrix.

random_matrix(N,Matrix,S0,S1) :-
	Limit is (N+N/2)//1,
	random_rows(N,N,Limit,MatrixRows,S0,S1),
	Matrix =.. [matrix|MatrixRows].

random_rows(0,_,_,X,S0,S1) :- !, X = [], S0 = S1.
random_rows(N,Dim,Limit,[Row|Rows],S0,S2) :- N > 0,
	random_row(Dim,Limit,Row,S0,S1),
	M is N-1,
	random_rows(M,Dim,Limit,Rows,S1,S2).

random_row(Dim,Limit,Row,S0,S1) :-
	random_elts(Dim,Limit,Elts,S0,S1),
	Row =.. [row|Elts].

random_elts(0,_,X,S0,S1) :- !, X = [], S0 = S1.
random_elts(N,Limit,[X|Xs],S0,S2) :- N > 0,
	random(Limit,X,S0,S1),
	M is N-1,
	random_elts(M,Limit,Xs,S1,S2).

/*
random_matrix(N, Matrix) :-
    functor(Matrix, matrix, N),
    empty_rows(N, N, Matrix),
    Limit is (N+N/2)//1,
    randoms_in_matrix(N, N, Limit, Matrix).

empty_rows(0, _, _) :- !.
empty_rows(N, Dim, Matrix) :-
    arg(N, Matrix, Row),
    functor(Row, row, Dim),
    N1 is N-1,
    empty_rows(N1, Dim, Matrix).

randoms_in_matrix(0, _, _, _) :- !.
randoms_in_matrix(I, N, Limit, Matrix) :-
    random_entry(N, I, Limit, Matrix),
    I1 is I-1,
    randoms_in_matrix(I1, N, Limit, Matrix).

random_entry(I, I, _, Matrix) :- !,
    arg(I, Matrix, Row),
    arg(I, Row, 0).
random_entry(J, I, Limit, Matrix) :-
    random(Limit, Random),
    arg(I, Matrix, Row),
    arg(J, Row, Random),
    arg(J, Matrix, Col),
    arg(I, Col, Random),
    J1 is J-1,
    random_entry(J1, I, Limit, Matrix).
*/

append_once(A,B,C) :-
	append(A,B,C),!.

append([],Y,Y).
append([A|X], Y, [A|Z]) :- append(X, Y, Z).

% Declarative random-function.

random(Limit, N, S, S1) :-
   N is (S mod Limit)+1,
   S1 is (125*S+1) mod 4096.
