:- module foreign_type_visibility.

:- implementation.

:- type foreign.

:- interface.

:- pragma foreign_type(il, foreign, "class [mscorlib]System.Object").

:- type foreign2.

:- pragma foreign_type(il, foreign2, "class [mscorlib]System.Object").

:- type foreign3
	---> foreign3(c_pointer).

:- implementation.

:- pragma foreign_type(c, foreign2, "void *").

:- pragma foreign_type(c, foreign3, "void *").

