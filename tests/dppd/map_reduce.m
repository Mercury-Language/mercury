/*
The "map.reduce" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   Specialising the higher-order map/3 (using call and =..) for the
   higher-order reduce/4 in turn applied to add/3. The benchmark program
   uses built-ins but no negations. The benchmark illustrates that
   partial deduction can be used to make declarative higher-order
   programming in Prolog/LP efficient.
   
  The benchmark program
*/ 

:- module map_reduce.

:- interface.

:- pred map_reduce is semidet.

:- implementation.

:- import_module list, run, map_impl.

map_reduce :-
	% XXX the commented-out line below results in a mode error,
	%     due to the Mercury compiler's lack of support for partially
	%     instantiated data structures.  Therefore it has been
	%     replaced with the line below it.  (The commented-out code here
	%     is also reproduced in a separate test case in tests/dppd/bug.m.)
	/* map_reduce_add([[1,2],[1,2,3]],[_L1,_L2]), */
	map_reduce_add([[1,2],[1,2,3]],Res0), Res0 = [_L1,_L2],
	map_reduce_add([[],[1,2],[5,6,7],[8,9,10]],Res1),
	use(Res1),
	map_reduce_add([[],[1,2],[5,6,7],[],[8,9,10],[11,12],
                                [13,14],[15],[16]],Res2),
	use(Res2).


/*
  The partial deduction query
  
 :- map(reduce_add,L,R).

  The run-time queries
  
 :- map(reduce_add,[[1,2],[1,2,3]],[L1,L2]).
 :- map(reduce_add,[[],[1,2],[5,6,7],[8,9,10]],Res).
 :- map(reduce_add,[[],[1,2],[5,6,7],[],[8,9,10],[11,12],
                                [13,14],[15],[16]],Res).

  Example solution
  
   With the ECCE partial deduction system one can obtain the following
   program (which runs more than 12 times faster than the original):

 map__1([],[]).
 map__1([X1|X2],[X3|X4]) :-
     reduce__3(X1,X3),
     map__1(X2,X4).

 reduce__3([],0).
 reduce__3([X1|X2],X3) :-
     reduce__3(X2,X4),
     X3 is '+'(X1,X4).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
*/
