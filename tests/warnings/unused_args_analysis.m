:- module unused_args_analysis.

:- interface.

:- pred p(int::in, int::out) is det.

:- implementation.

:- import_module unused_args_analysis2.

p(X, Y) :- p2(X, Y).
