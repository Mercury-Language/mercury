:- module det_inference.

:- pred p1 is det.
:- pred p2.
:- pred p3.
:- pred p4.
:- pred p5.

p1 :- p2.
p2 :- p3.
p3 :- p4.
p4 :- p5.
p5 :- true.
p5 :- true.

