/*
The "map.rev" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   Specialising the higher-order map/3 (using call and =..) for the
   reverse program. The benchmark program uses built-ins but no
   negations. The benchmark illustrates that partial deduction can be
   used to make declarative higher-order programming in Prolog/LP
   efficient.
   
  The benchmark program
*/

:- module map_rev.

:- interface.

:- pred map_rev is semidet.

:- implementation.

:- import_module list, map_impl, run.

map_rev :-
	map_rev([[a,b],[c,d,e]],[L1,L2]),
	use(L1), use(L2),
 	map_rev([[],[a,b],[c,d,e],[f,g,h,i]],Res1),
	use(Res1),
 	map_rev([[],[a,b],[c,d,e],[],[f,g,h],[i,j],[k,l],[m],[n]],Res2),
	use(Res2).

/*
  The partial deduction query
  
 :- map(rev,L,R).

  The run-time queries
  
 :- map(rev,[[a,b],[c,d,e]],[L1,L2]).
 :- map(rev,[[],[a,b],[c,d,e],[f,g,h,i]],Res).
 :- map(rev,[[],[a,b],[c,d,e],[],[f,g,h],[i,j],[k,l],[m],[n]],Res).

  Example solution
  
   With the ECCE partial deduction system one can obtain the following
   program (which runs almost 10 times faster than the original):

 map__1([],[]).
 map__1([X1|X2],[X3|X4]) :-
      rev__2(X1,X3),
      map__1(X2,X4).

 rev__2([],[]).
 rev__2([X1|X2],X3) :-
      rev__3(X2,X1,[],X3).

 rev__3([],X1,X2,[X1|X2]).
 rev__3([X1|X2],X3,X4,X5) :-
      rev__3(X2,X1,[X3|X4],X5).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
*/
