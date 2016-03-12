%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module dcg_test.

:- interface.

:- pred q(int::in, int::out) is semidet.
:- pred r(int::in, int::out) is semidet.
:- pred s(int::in, int::out) is semidet.
:- pred q is semidet.
:- pred r is semidet.
:- pred s is semidet.

:- implementation.

:- pragma external_pred(q/2).
:- pragma external_pred(r/2).
:- pragma external_pred(s/2).
:- pragma external_pred(q/0).
:- pragma external_pred(r/0).
:- pragma external_pred(s/0).

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
