/*
The "regexp.r1" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   A program testing whether a string matches a regular expression (using
   difference lists). Much more naive (and smaller) than the program used
   by Mogensen/Bondorf for Logimix ! The regular expression for this
   benchmark is (a+b)*aab. This benchmark contains no built-in's nor
   negations.
   
  The benchmark program
*/ 

:- module regexp_r1.

:- interface.

:- pred regexp_r1 is semidet.

:- implementation.

:- import_module char, list, regexp.

regexp_r1 :-
	generate1([a,a,a,a,a,a,b,b,a,a,a,b]),
 	generate1([a,a,a,a,a,a,b,b,a,b]),
	generate1([a,b,a,b,a,b,a,b,a,b,a]).

 	% generate1([X, Y, Z, V]). this currently isn't well moded.

/*
  The partial deduction query
  
 :- generate(cat(star(or(char(a),char(b))), cat(char(a),cat(char(a),char(b)))),
 S, []).

  The run-time queries
  
 :- generate(cat(star(or(char(a),char(b))), cat(char(a),cat(char(a),char(b)))),
                [a,a,a,a,a,a,b,b,a,a,a,b],[]).
 :- generate(cat(star(or(char(a),char(b))), cat(char(a),cat(char(a),char(b)))),
                [a,a,a,a,a,a,b,b,a,b],[]).
 :- generate(cat(star(or(char(a),char(b))), cat(char(a),cat(char(a),char(b)))),
                [a,b,a,b,a,b,a,b,a,b,a],[]).
 :- generate(cat(star(or(char(a),char(b))), cat(char(a),cat(char(a),char(b)))),
                [X,Y,Z,V],[]).

  Example solution
  
   The following can be obtained by the ECCE partial deduction system .
   Although it runs considerably faster than the original program (5
   times actually) it does not correspond to a deterministic automaton
   yet.

 generate__1(X1) :- generate__2(X1).
 generate__2([a,a,b]).
 generate__2([a|X1]) :- generate__2(X1).
 generate__2([b|X1]) :- generate__2(X1).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be

*/
