
:- module match_append_impl.

:- interface.

:- import_module list, char.

:- pred match_aab(list(char)::in) is semidet.

:- implementation.

match_aab(S) :- match([a,a,b], S).

:- pred match(list(char)::in, list(char)::in) is semidet. 

match(P,S) :- app(S1,_,S),app(_,P,S1).

:- pred app(list(char), list(char), list(char)).
:- mode app(in, in, out) is det.
:- mode app(in, out, in) is semidet.
:- mode app(out, out, in) is multi.
:- mode app(out, in, in) is nondet.	% actually semidet.

app([],L,L).
app([H|L1],L2,[H|L3]) :- app(L1,L2,L3).

