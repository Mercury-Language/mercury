/*
  Test taken from the XSB testsuite.

  Under local evaluation it results in:
    318 variant call check/insert ops: 120 generators, 198 consumers.
    120 answer check/insert ops: 120 unique inserts, 0 redundant.
 
  If d/4 is declared semidet, the program compiles with a warning that
  the determinism declaration of d/1 could be tighter, but when the
  program executes, it produces the right answer.

  If d/4 is declared nondet, The program compiles without warnings,
  but produces no answer when it executes.
*/

:- module oota.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list.

:- import_module std_util, int, list.

main -->
	{ solutions(d, Solns) },
	io__write_string("D = "),
	io__write(Solns),
	io__write_string("\n").

:- pred d(int::out) is nondet.

d(D) :-
	GC = 10, I = 11, J = 9,
	d(GC, I, J, D).

% :- pred d(int::in, int::in, int::in, int::out) is nondet.
:- pred d(int::in, int::in, int::in, int::out) is semidet.
:- pragma minimal_model(d/4).

d(GC, I, J, D) :-
	( I = 0, J = 0 ->
	    D = 0
	; I = 0 ->
	    J1 = J - 1,
	    d(GC, 0, J1, D1),
	    D = D1 + GC
	; J = 0 ->
	    I1 = I - 1,
	    d(GC, I1, 0, D1),
	    D = D1 + GC
	;
	    I1 = I - 1,
	    J1 = J - 1,
	    ms(I, J, MS),
	    d(GC, I1,J1, D1), D_MS = D1 + MS,
	    d(GC, I1, J, D2), D_G1 = D2 + GC,
	    d(GC, I, J1, D3), D_G2 = D3 + GC,
	    min([D_MS, D_G1, D_G2], D)
	).

:- pred min(list(int)::in, int::out) is semidet.

min(S, M) :-
	list__sort(S, [M|_]).

:- pred ms(int::in, int::in, int::out) is semidet.

ms(0,0,0).
ms(0,1,0).
ms(0,2,0).
ms(0,3,0).
ms(0,4,0).
ms(0,5,0).
ms(0,6,0).
ms(0,7,0).
ms(0,8,0).
ms(0,9,0).
ms(1,0,0).
ms(1,1,1).
ms(1,2,0).
ms(1,3,0).
ms(1,4,0).
ms(1,5,1).
ms(1,6,1).
ms(1,7,1).
ms(1,8,1).
ms(1,9,0).
ms(2,0,0).
ms(2,1,1).
ms(2,2,0).
ms(2,3,0).
ms(2,4,0).
ms(2,5,1).
ms(2,6,1).
ms(2,7,1).
ms(2,8,1).
ms(2,9,0).
ms(3,0,0).
ms(3,1,0).
ms(3,2,1).
ms(3,3,1).
ms(3,4,1).
ms(3,5,0).
ms(3,6,0).
ms(3,7,1).
ms(3,8,1).
ms(3,9,1).
ms(4,0,0).
ms(4,1,0).
ms(4,2,1).
ms(4,3,1).
ms(4,4,1).
ms(4,5,0).
ms(4,6,0).
ms(4,7,1).
ms(4,8,1).
ms(4,9,1).
ms(5,0,0).
ms(5,1,0).
ms(5,2,1).
ms(5,3,1).
ms(5,4,1).
ms(5,5,0).
ms(5,6,0).
ms(5,7,1).
ms(5,8,1).
ms(5,9,1).
ms(6,0,0).
ms(6,1,1).
ms(6,2,1).
ms(6,3,1).
ms(6,4,1).
ms(6,5,1).
ms(6,6,1).
ms(6,7,0).
ms(6,8,1).
ms(6,9,1).
ms(7,0,0).
ms(7,1,1).
ms(7,2,1).
ms(7,3,1).
ms(7,4,1).
ms(7,5,1).
ms(7,6,1).
ms(7,7,0).
ms(7,8,1).
ms(7,9,1).
ms(8,0,0).
ms(8,1,1).
ms(8,2,1).
ms(8,3,1).
ms(8,4,1).
ms(8,5,1).
ms(8,6,1).
ms(8,7,0).
ms(8,8,1).
ms(8,9,1).
ms(9,0,0).
ms(9,1,1).
ms(9,2,1).
ms(9,3,1).
ms(9,4,1).
ms(9,5,1).
ms(9,6,1).
ms(9,7,1).
ms(9,8,0).
ms(9,9,1).
ms(10,0,0).
ms(10,1,1).
ms(10,2,1).
ms(10,3,1).
ms(10,4,1).
ms(10,5,1).
ms(10,6,1).
ms(10,7,1).
ms(10,8,0).
ms(10,9,1).
ms(11,0,0).
ms(11,1,1).
ms(11,2,0).
ms(11,3,0).
ms(11,4,0).
ms(11,5,1).
ms(11,6,1).
ms(11,7,1).
ms(11,8,1).
ms(11,9,0).
