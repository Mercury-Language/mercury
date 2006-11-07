:- module tc_map_lookup_2.
:- interface.

:- type t ---> t.

:- implementation.

:- import_module term_to_xml.

:- typeclass tc(T) <= xmlable(T) where [
	func f(T) = string
].

