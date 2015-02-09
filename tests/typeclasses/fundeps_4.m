:- module fundeps_4.
:- interface.

:- import_module assoc_list.
:- import_module io.
:- import_module string.

:- pred main(io::di, io::uo) is det.

:- type propagator_info
	--->	some [P, V] propagator_info(P) => propagator_info(P, V).

:- typeclass propagator_info(P, V) <= ((P -> V), variable_info(V)) where [
	func propagator_name(P) = string,
	func propagator_vars(P) = assoc_list(string, V)
].

:- typeclass variable_info(V) where [
	func domain_desc(V) = string
].

:- func i = propagator_info.

:- implementation.

:- import_module list.
:- import_module pair.

:- type no_propagator_info
	--->	no_propagator_info.

:- instance propagator_info(no_propagator_info, no_propagator_info) where [
	(propagator_name(_) = "<<no info>>"),
	(propagator_vars(_) = ["V1" - no_propagator_info])
].

:- instance variable_info(no_propagator_info) where [
	(domain_desc(_) = "<<no info>>")
].

i = 'new propagator_info'(no_propagator_info).

main(!IO) :-
	write_propagator_info(i, !IO),
	io.nl(!IO).

:- pred write_propagator_info(propagator_info::in, io::di, io::uo) is det.

write_propagator_info(propagator_info(P), !IO) :-
	io.write_strings([propagator_name(P), ": "], !IO),
	list.foldl(write_variable_info, propagator_vars(P), !IO).

:- pred write_variable_info(pair(string, V), io, io) <= variable_info(V).
:- mode write_variable_info(in, di, uo) is det.

write_variable_info(N - V, !IO) :-
	io.write_string(N, !IO),
	io.write_string(" - ", !IO),
	io.write_string(domain_desc(V), !IO),
	io.write_string("; ", !IO).

