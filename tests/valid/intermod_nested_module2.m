:- module intermod_nested_module2.

:- interface.

:- type foo.

:- module sub_module.

:- interface.

:- import_module int.

:- pred bar(int, foo).
:- mode bar(in, out) is det.

:- end_module sub_module.

:- implementation.

:- type foo ---> foo(int).

:- module sub_module.

:- implementation.

bar(X, foo(Y)) :-
        Y = X + 1.

:- end_module sub_module.

