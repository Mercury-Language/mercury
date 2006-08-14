:- module bad_mutable.

:- interface.

:- mutable(in_interface, int, 0, ground, [untrailed, thread_safe]).

:- implementation.

:- type list(T) ---> [] ; [ T | list(T) ].

:- mutable(not_a_type, no_type, 0, ground, [untrailed]).

:- mutable(not_an_inst, int, 0, special_ground, [untrailed]).

:- mutable(bad_attribute, int, 0, ground, [untrailed, bad_attrib]).

:- mutable(poly_type, list(T), [], ground, [untrailed]).

:- mutable(conflicting_trail, int, 0, ground, [untrailed, trailed]).

:- mutable(multiple_foreign, int, 0, ground,
	[untrailed, foreign_name("C", "one"), foreign_name("C", "two")]).
