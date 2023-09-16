%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module det_switch.

:- interface.

:- type enum
    --->    a
    ;       b
    ;       c.

:- pred p(enum, int).
:- mode p(in, out) is det.
:- mode p(out, in) is semidet.
:- mode p(out, out) is multi.

:- implementation.

p(a, 1).
p(b, 2).
p(c, 3).

:- pred q(enum, int).
:- mode q(in, out) is semidet.

q(a, 1).
q(b, 2).
