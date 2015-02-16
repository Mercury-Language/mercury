%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module pl4_5_3a.

:- interface.

:- type node
    --->    a
    ;       b
    ;       c.

:- pred p(node).
:- mode p(in) is semidet.   % DIAGNOSED BY COMPILER
:- mode p(out) is multi.

:- implementation.

p(b).
p(a) :-
    p(_X).
