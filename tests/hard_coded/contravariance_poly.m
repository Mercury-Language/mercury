:- module contravariance_poly.
:- interface.
:- import_module io.
 
:- pred main(io__state::di, io__state::uo) is det.
 
:- implementation.
:- import_module list, bool.
 
main -->
	( { q(p) } ->
		print("yes"), nl
	;
		print("no"), nl
	).
 
:- inst nonempty(I) ---> [I | list(I)].
:- inst list(I) ---> [I | list(I)] ; [].

:- type intlist == list(int).
:- inst bit == bound(0 ; 1).
 
:- pred p(intlist, intlist).
:- mode p((list(I) >> nonempty(I)), (free >> nonempty(I))) is semidet.
 
p([X], [X]).
p([X,Y|Zs], [Y,X|Zs]).
 
:- pred q(pred(intlist, intlist)).
:- mode q(pred((nonempty(bit) >> nonempty(bit)), (free >> list(bit)))
		is semidet) is semidet.
 
q(P) :- P([1], L), L \= [].
