:- module nested_mod_type_bug.
:- interface.

	:- module nested.
	:- interface.
	:- type blah == int.
	:- end_module nested.

:- type foo(T).

:- implementation.

:- type foo(T) ---> f(T).
