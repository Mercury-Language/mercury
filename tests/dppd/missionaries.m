/*
The "missionaries" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   A program for the missionaries and cannibals problem without using
   built-ins, but with negation.
   
  The benchmark program
*/  

:- module missionaries.

:- interface.

:- pred missionaries is semidet.

:- implementation.

:- import_module list, run, missionaries_impl.

missionaries :- 
	missionaries_query(s(s(s(zero))),s(s(s(zero))), Res),
	use(Res).

/*
  The partial deduction query
  
 :- search(X,Y,west,[state(X,Y,west)],Res).

  The run-time queries
  
 :- search(s(s(s(zero))),s(s(s(zero))),west,
        [state(s(s(s(zero))),s(s(s(zero))),west)],Res).

  Example solution
  
   The best solution so far, using the ECCE partial deduction system runs
   almost 2 times faster than the original. Mixtus (0.3.3) and Paddy
   (Eclipse 3.5.1) did not terminate on this example. The following
   solution is not optimal (but is a bit smaller). Its relative execution
   time is 0.69.

search__1(zero,zero,X1,[state(zero,zero,west)|X1]).
search__1(X1,X2,X3,X4) :-
    move_boat_east_conj__2(X1,X2,X5,X6),
    not(loop__3(X5,X6,east,X1,X2,west,X3)),
    search__4(X5,X6,X1,X2,X3,X4).

move_boat_east_conj__2(s(s(X1)),X2,X1,X2) :- safe__11(X1,X2).
move_boat_east_conj__2(s(X1),X2,X1,X2) :- safe__11(X1,X2).
move_boat_east_conj__2(s(X1),s(X2),X1,X2) :- safe__11(X1,X2).
move_boat_east_conj__2(X1,s(X2),X1,X2) :- safe__11(X1,X2).
move_boat_east_conj__2(X1,s(s(X2)),X1,X2) :- safe__11(X1,X2).

loop__3(X1,X2,X3,X4,X5,X6,[state(X1,X2,X3)|X7]).
loop__3(X1,X2,X3,X4,X5,X6,[X7,X8|X9]) :- mymember__1zero(X1,X2,X3,X8,X9).

search__4(zero,zero,X1,X2,X3,[state(zero,zero,east),state(X1,X2,west)|X3]).
search__4(X1,X2,X3,X4,X5,X6) :-
    move_boat_west_conj__5(X1,X2,X7,X8),
    not(loop__3(X7,X8,west,X1,X2,east,[state(X3,X4,west)|X5])),
    search__1(X7,X8,[state(X1,X2,east),state(X3,X4,west)|X5],X6).

move_boat_west_conj__5(X1,X2,s(s(X1)),X2) :- safe__6(s(X1),X2).
move_boat_west_conj__5(X1,X2,s(X1),X2) :- safe__6(X1,X2).
move_boat_west_conj__5(X1,X2,s(X1),s(X2)) :- safe__6(X1,s(X2)).
move_boat_west_conj__5(X1,X2,X1,s(X2)) :-  safe__7(X1,X2).
move_boat_west_conj__5(X1,X2,X1,s(s(X2))) :- safe__7(X1,s(X2)).

safe__6(s(s(zero)),X1) :- not(gt__8(X1)).
safe__6(X1,s(X1)) :- not(ge__9(s(X1))).

safe__7(s(s(s(zero))),X1) :- not(gt__8(s(X1))).
safe__7(zero,X1) :- not(gt__8(s(X1))).
safe__7(s(X1),X1) :- not(ge__9(s(X1))).

gt__8(s(s(s(s(X1))))).

ge__9(s(s(s(zero)))).
ge__9(s(s(s(s(X1))))).

mymember__1zero(X1,X2,X3,state(X1,X2,X3),X4).
mymember__1zero(X1,X2,X3,X4,[X5|X6]) :- mymember__1zero(X1,X2,X3,X5,X6).

safe__11(s(s(s(zero))),X1) :- not(gt__8(X1)).
safe__11(zero,X1) :- not(gt__8(X1)).
safe__11(X1,X1) :- not(ge__12(X1)), not(ge__9(X1)).

ge__12(zero).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
*/
