/*
The "regexp.r3" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   A program testing whether a string matches a regular expression (using
   difference lists). Much more naive (and smaller) than the program used
   by Mogensen/Bondorf for Logimix ! The regular expression for this
   benchmark is ((a+b)(a+b)(a+b)(a+b)(a+b)(a+b))*. This benchmark
   contains no built-in's nor negations.
   
  The benchmark program
*/ 

:- module regexp_r3.

:- interface.

:- pred regexp_r3 is semidet.

:- implementation.

:- import_module list, char, regexp.

regexp_r3 :-
	generate3([a,b,a,b,a,b, a,a,a,b,a,b]),
	% not well-moded.
	%generate3([a,X,a,b,a,b, a,a,a,Y,b,a]),
	generate3([a,b,a,b,a,a, a,b]),
	generate3([a,b,a,b,a,a, b,b,b,a,b,a, a,b,b,a,a,b]).

/*
  The partial deduction query
  
 :- generate(star(cat(or(char(a),char(b)),cat(or(char(a),char(b)),
        cat(or(char(a),char(b)),cat(or(char(a),char(b)),
        cat(or(char(a),char(b)),or(char(a),char(b)))))))),S,[]).

  The run-time queries
  
 :- generate(star(cat(or(char(a),char(b)),cat(or(char(a),char(b)),
                cat(or(char(a),char(b)),cat(or(char(a),char(b)),
                cat(or(char(a),char(b)),or(char(a),char(b)))))))),
                [a,b,a,b,a,b, a,a,a,b,a,b],[]).
 :- generate(star(cat(or(char(a),char(b)),cat(or(char(a),char(b)),
                cat(or(char(a),char(b)),cat(or(char(a),char(b)),
                cat(or(char(a),char(b)),or(char(a),char(b)))))))),
                [a,X,a,b,a,b, a,a,a,Y,b,a],[]).
 :- generate(star(cat(or(char(a),char(b)),cat(or(char(a),char(b)),
                cat(or(char(a),char(b)),cat(or(char(a),char(b)),
                cat(or(char(a),char(b)),or(char(a),char(b)))))))),
                [a,b,a,b,a,a, a,b],[]).
 :- generate(star(cat(or(char(a),char(b)),cat(or(char(a),char(b)),
                cat(or(char(a),char(b)),cat(or(char(a),char(b)),
                cat(or(char(a),char(b)),or(char(a),char(b)))))))),
                [a,b,a,b,a,a, b,b,b,a,b,a, a,b,b,a,a,b],[]).

  Example solution
  
   The following can be obtained by the ECCE partial deduction system .
   It runs considerably faster than the original program (more than 3
   times actually) and correspond to a deterministic automaton.

 generate__1([]).
 generate__1(X1) :- generate_conj__2(X1).

 generate_conj__2([a|X1]) :- generate_conj__3(X1).
 generate_conj__2([b|X1]) :- generate_conj__3(X1).

 generate_conj__3([a|X1]) :- generate_conj__4(X1).
 generate_conj__3([b|X1]) :- generate_conj__4(X1).

 generate_conj__4([a|X1]) :- generate_conj__5(X1).
 generate_conj__4([b|X1]) :- generate_conj__5(X1).

 generate_conj__5([a|X1]) :- generate_conj__6(X1).
 generate_conj__5([b|X1]) :- generate_conj__6(X1).

 generate_conj__6([a|X1]) :- generate_conj__7(X1).
 generate_conj__6([b|X1]) :- generate_conj__7(X1).

 generate_conj__7([a|X1]) :- generate__1(X1).
 generate_conj__7([b|X1]) :- generate__1(X1).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
*/
