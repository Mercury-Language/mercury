:- module dcg_test.

:- pred q(int::in, int::out) is semidet.
:- pred r(int::in, int::out) is semidet.
:- pred s(int::in, int::out) is semidet.
:- pred q is semidet.
:- pred r is semidet.
:- pred s is semidet.
:- external(q/2).
:- external(r/2).
:- external(s/2).
:- external(q/0).
:- external(r/0).
:- external(s/0).

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

