:- module det_switch.

:- type enum ---> a ; b ; c.

:- pred p(enum, int) is det.
:- mode p(in, out) is det.
:- mode p(out, in) is semidet.
:- mode p(out, out) is multi.

p(a, 1).
p(b, 2).
p(c, 3).

:- pred q(enum, int) is det.
:- mode q(in, out) is semidet.

q(a, 1).
q(b, 2).

