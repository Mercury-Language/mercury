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

:- module regexp.

:- interface.

:- import_module char, list.

:- type regexp
	--->	empty
	;	char(char)
	;	or(regexp, regexp)
	;	cat(regexp, regexp)
	;	star(regexp).

:- pred generate1(list(char)). 
:- mode generate1(in) is semidet.

:- pred generate2(list(char)::in) is semidet.
:- pred generate3(list(char)::in) is semidet.

:- implementation.

generate1(S) :-
 	generate(cat(star(or(char(a),char(b))), 
			cat(char(a),cat(char(a),char(b)))), S, []).

generate2(S) :-
	generate(star(cat(or(char(a),char(b)),cat(or(char(c),char(d)),
		cat(or(char(e),char(f)),or(char(g),char(h)))))), S, []).

generate3(S) :-
 	generate(star(cat(or(char(a),char(b)),cat(or(char(a),char(b)),
		cat(or(char(a),char(b)),cat(or(char(a),char(b)),
		cat(or(char(a),char(b)),or(char(a),char(b)))))))),S,[]).


:- pred generate(regexp, list(char), list(char)) is nondet.
:- mode generate(in, in, out) is nondet.


generate(empty,T,T).

generate(char(X),[X|T],T).

generate(or(X,_),H,T) :- generate(X,H,T).
generate(or(_,Y),H,T) :- generate(Y,H,T).

generate(cat(X,Y),H,T) :- generate(X,H,T1), generate(Y,T1,T).

generate(star(_),T,T).
generate(star(X),H,T) :- generate(X,H,T1), generate(star(X),T1,T).

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
