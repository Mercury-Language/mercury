%-----------------------------------------------------------------%
%
%  Proof tree generator
%
%	This is an example of the result of a program transformation
%	that builds a proof tree at the	same time as calculating the
%	correct results.
%
%	The proof tree uses the ground representation provided by
%	term.m in the Mercury standard library.  This is the simplest
%	way to achieve this in Mercury because it avoids dealing with
%	issues such as writing an explicit type and instantiation for
%	the proof tree.  Unfortunately,	this means that many of
%	Mercury's advantages with strong typing are lost.
%
%  21 January, 1998
%  Mark Brown

:- module ground_dd.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module int, list, term, term_io, varset.

:- type proof 
	--->	node(term, proof)
%	;	leaf(pred(proof))	% implicit representation
	;	new_types(proof)	% currently a NOP
	;	conj(list(proof))
	;	disj(int, proof)
	;	if_then(proof, proof)
	;	else(disproof, proof)
	;	not(disproof)
	;	assumed.


:- type disproof
	--->	failed.

:- type node
	--->	n_isort(list(int), list(int))
	;	n_insert(int, list(int), list(int))
	;	n_sum_list(list(int), int).
:- type node(T)
	--->	n_append(list(T), list(T), list(T))
	;	n_length(list(T), int).
:- type node(T, S)
	--->	n_map(proc_id, list(T), list(S))
	;	n_foldl(proc_id, list(T), S, S).


:- type proc_id == string.


:- pred append_w(list(T), list(T), list(T), proof).
:- mode append_w(in, in, out, out) is det.
:- mode append_w(out, out, in, out) is multi.

append_w([], Bs, Bs, node(Node, assumed)) :-
	type_to_term(n_append([], Bs, Bs), Node).
append_w([A|As], Bs, [A|Cs], node(Node, Proof)) :-
	append_w(As, Bs, Cs, Proof),
	type_to_term(n_append([A|As], Bs, [A|Cs]), Node).


:- pred length_w(list(T), int, proof).
:- mode length_w(in, out, out) is det.

length_w([], 0, node(N, assumed)) :-
	type_to_term(n_length([], 0), N).
length_w([A|As], N+1, node(Node, Proof)) :-
	length_w(As, N, Proof),
	type_to_term(n_length([A|As], N+1), Node).


:- pred isort_w(list(int), list(int), proof).
:- mode isort_w(in, out, out) is det.

isort_w([], [], node(Node, assumed)) :-
	type_to_term(n_isort([], []), Node).
isort_w([A|As], Ss, node(Node, conj([Proof_1, Proof_2]))) :-
	isort_w(As, Ss0, Proof_1),
	insert_w(A, Ss0, Ss, Proof_2),
	type_to_term(n_isort([A|As], Ss), Node).


:- pred insert_w(int, list(int), list(int), proof).
:- mode insert_w(in, in, out, out) is det.

insert_w(N, [], [N], node(Node, assumed)) :-
	type_to_term(n_insert(N, [], [N]), Node).
insert_w(N, [A|As], Ss, node(Node, Proof)) :-
	( N =< A ->
		Ss = [N,A|As],
		Proof = if_then(assumed, assumed)
	;
		insert_w(N, As, Ss0, Proof_1),
		Ss = [A|Ss0],
		Proof = else(failed, Proof_1)
	),
	type_to_term(n_insert(N, [A|As], Ss), Node).
	

%------------------------------------------------------------

:- pred write_proof(int, proof, io__state, io__state).
:- mode write_proof(in, in, di, uo) is det.

write_proof(L, node(N, P)) -->
	indent(L),
	{ varset__init(V) },
	write_term(V, N),
	nl,
	write_proof(L+1, P).
% write_proof(L, leaf(Closure)) -->
%	{ Closure(P) },
%	write_proof(L, P).
write_proof(L, new_types(P)) -->
	write_proof(L, P).
write_proof(L, conj(Ps)) -->
	foldl(write_proof(L), Ps).
write_proof(L, disj(_, P)) -->
	write_proof(L, P).
write_proof(L, if_then(P_1, P_2)) -->
	write_proof(L, P_1),
	write_proof(L, P_2).
write_proof(L, else(_, P_2)) -->
	write_proof(L, P_2).
write_proof(_, not(_)) -->
	[].
write_proof(_, assumed) -->
	[].


:- pred indent(int, io__state, io__state).
:- mode indent(in, di, uo) is det.

indent(L) -->
	( { L > 0 } ->
		write_char('\t'),
		indent(L-1)
	;
		[]
	).


main -->
	{ append_w([a,b,c], [d,e], As, P_1) },
	write_proof(0, P_1),

	{ length_w(As, _, P_2) },
	write_proof(0, P_2),

	{ isort_w([4,2,5,3,1], _, P_3) },
	write_proof(0, P_3).

