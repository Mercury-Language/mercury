:- module foreign_type_spec.

:- interface.

:- import_module foreign_type_spec__foreign_type.

:- type ft2
	--->	f(foreign).

:- module foreign_type_spec__foreign_type.

:- interface.

:- type foreign.
:- pragma foreign_type(il, foreign, "class [mscorlib]System.Object").

:- end_module foreign_type_spec__foreign_type.
