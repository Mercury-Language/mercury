:- module foreign_type_2.

:- interface.

:- type foreign.

:- func unwrap_foreign(foreign) = int is semidet.

:- implementation.

:- pragma foreign_type(il, foreign, "class [mscorlib]System.Object").

:- type foreign
	---> foreign(int).

% There are no foreign clauses for this function, so the use
% of the foreign/1 constructor is an error.
unwrap_foreign(foreign(Int)) = Int.

