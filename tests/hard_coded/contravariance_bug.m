:- module contravariance_bug.
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
 
:- type intlist == list(int).
:- inst nonempty ---> [ground|list].
:- inst list ---> [ground|list] ; [].
 
:- pred p(intlist, intlist).
:- mode p((list -> nonempty), (free -> nonempty)) is semidet.
 
p([X], [X]).
p([X,Y|Zs], [Y,X|Zs]).
 
:- pred q(pred(intlist, intlist)).
:- mode q(pred((nonempty -> nonempty), (free -> list)) is semidet) is semidet.
 
q(P) :- P([1], L), L \= [].
