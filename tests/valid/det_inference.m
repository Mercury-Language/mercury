%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module det_inference.

:- interface.

:- pred p1 is det.

:- implementation.

:- pred p2.
:- mode p2.

:- pred p3.
:- mode p3.

:- pred p4.
:- mode p4.

:- pred p5.
:- mode p5.

p1 :- p2.
p2 :- p3.
p3 :- p4.
p4 :- p5.
p5 :- true.
p5 :- true.
