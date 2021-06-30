%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module undef_type.
:- interface.

:- type t1
    --->   a
    ;      b
    ;      c
    ;      d(undef1).

:- inst x == bound(a ; b ; c).

:- implementation.

:- pred p.
p.

:- pred q(undef2).
q(_).

:- pred r(io.state).
r(_).
