:- module dcg_test.

:- pred q(int::in, int::out) is semidet.
:- pred r(int::in, int::out) is semidet.
:- pred s(int::in, int::out) is semidet.
:- pred q is semidet.
:- pred r is semidet.
:- pred s is semidet.

:- pred p(int::in, int::out) is nondet.

p --> ( q -> r ; s ).
p --> ( { q } -> r ; s ).
p --> ( q -> { r } ; s ).
p --> ( { q } -> { r } ; s ).
p --> ( q -> r ; { s } ).
p --> ( { q } -> r ; { s } ).
p --> ( q -> { r } ; { s } ).
p --> ( { q } -> { r } ; { s } ).
p --> { ( q -> r ; s ) }.

