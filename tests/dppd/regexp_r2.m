/*

The "regexp.r2" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   A program testing whether a string matches a regular expression (using
   difference lists). Much more naive (and smaller) than the program used
   by Mogensen/Bondorf for Logimix ! The regular expression for this
   benchmark is ((a+b)(c+d)(e+f)(g+h))*. This benchmark contains no
   built-in's nor negations.
   
  The benchmark program

*/

:- module regexp_r2.

:- interface.

:- pred regexp_r2 is semidet.

:- implementation.

:- import_module list, char, regexp.

regexp_r2 :-
	generate2([a,d,e,h, b,c,f,g]),

	% not well-moded 
	% generate2([b,d,X,g, a,c,Y,h]),

 	generate2([a,d,e,h, b,z,f,g, a,d,e,h, b,c,f,g, a,d,e,h, b,c,f,g]).
 /* 

  The partial deduction query
  
 :- generate(star(cat(or(char(a),char(b)),cat(or(char(c),char(d)),
        cat(or(char(e),char(f)),or(char(g),char(h)))))), S,[]).

  The run-time queries
  
 :- generate(star(cat(or(char(a),char(b)),cat(or(char(c),char(d)),
                cat(or(char(e),char(f)),or(char(g),char(h)))))),
                [a,d,e,h, b,c,f,g],[]).
 :- generate(star(cat(or(char(a),char(b)),cat(or(char(c),char(d)),
                cat(or(char(e),char(f)),or(char(g),char(h)))))),
                [b,d,X,g, a,c,Y,h],[]).
 :- generate(star(cat(or(char(a),char(b)),cat(or(char(c),char(d)),
                cat(or(char(e),char(f)),or(char(g),char(h)))))),
                [a,d,e,h, b,z,f,g, a,d,e,h, b,c,f,g, a,d,e,h, b,c,f,g],[]).

  Example solution
  
   The following can be obtained by the ECCE partial deduction system .
   It runs considerably faster than the original program (2.5 times
   actually) and correspond to a deterministic automaton.

 generate__1([]).
 generate__1(X1) :- generate_conj__2(X1).

 generate_conj__2([a|X1]) :- generate_conj__3(X1).
 generate_conj__2([b|X1]) :- generate_conj__3(X1).

 generate_conj__3([c|X1]) :- generate_conj__4(X1).
 generate_conj__3([d|X1]) :- generate_conj__4(X1).

 generate_conj__4([e|X1]) :- generate_conj__5(X1).
 generate_conj__4([f|X1]) :- generate_conj__5(X1).

 generate_conj__5([g|X1]) :- generate__1(X1).
 generate_conj__5([h|X1]) :- generate__1(X1).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
*/
