% Test for circular equivalence types.
:- module circ_type.

:- interface.

:- type circ == circ.

:- type circ1 == circ2.

:- type circ2 == circ1.
