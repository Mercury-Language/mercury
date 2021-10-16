%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module circ_inst5.
:- interface.

:- inst c == c(ground).
:- inst c(I) == i(c(I)).
:- inst i(I) == I.

:- type foo
    --->    foo.

:- func f(foo) = int.
:- mode f(in(c)) = out is det.

% Test that the error message
% - does not module qualify g2, g3, g4 and g5, since they are local only, but
% - does module qualify dead, since it also exists in builtin.m.
:- inst dead == g2.
:- inst g2 == g3.
:- inst g3 == g4.
:- inst g4 == g5.
:- inst g5 == circ_inst5.dead.

% Test that we report the circularity as involving only f2 and f3, and not f1,
% even though f1 has an infinite expansion as well.
:- inst f1 == f2.
:- inst f2 == f3.
:- inst f3 == f2.

:- implementation.

f(foo) = 1.
