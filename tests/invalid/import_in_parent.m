:- module import_in_parent.

:- interface.

:- import_module bool.

:- type foo.

:- implementation.

	:- module import_in_parent__sub.

	:- interface.

	:- pred foo(bool::in) is semidet.

	:- implementation.

	foo(X) :- bool__foo(X).

	:- end_module import_in_parent__sub.

:- end_module import_in_parent.
