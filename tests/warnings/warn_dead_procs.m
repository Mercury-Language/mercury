:- module warn_dead_procs.
:- interface.
:- type expr ---> div(expr, expr).
:- implementation.

:- type expr2 ---> div2(expr2, expr2).

:- pred foo is det.
foo.

:- pred bar(int).
:- mode bar(in) is semidet.
:- mode bar(out) is det.
bar(42).

baz.

:- promise ((X `with_type` int) = (Y `with_type` int) <=> Y = X).
