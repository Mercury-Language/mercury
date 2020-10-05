%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
%  Proof tree generator
%
%       This is an example of the result of a program transformation
%       that builds a proof tree at the same time as calculating the
%       correct results.
%
%       The proof tree uses the ground representation provided by
%       term.m in the Mercury standard library.  This is the simplest
%       way to achieve this in Mercury because it avoids dealing with
%       issues such as writing an explicit type and instantiation for
%       the proof tree.  Unfortunately, this means that many of
%       Mercury's advantages with strong typing are lost.
%
%  21 January, 1998
%  Mark Brown
%

:- module ground_dd.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module term.
:- import_module term_conversion.
:- import_module term_io.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type proof
        --->    node(term, proof)
%       ;       leaf(pred(proof))   % implicit representation
        ;       new_types(proof)    % currently a NOP
        ;       conj(list(proof))
        ;       disj(int, proof)
        ;       if_then(proof, proof)
        ;       else(disproof, proof)
        ;       not(disproof)
        ;       assumed.

:- type disproof
        --->    failed.

:- type node
        --->    n_isort(list(int), list(int))
        ;       n_insert(int, list(int), list(int))
        ;       n_sum_list(list(int), int).

:- type node(T)
        --->    n_append(list(T), list(T), list(T))
        ;       n_length(list(T), int).

:- type node(T, S)
        --->    n_map(proc_id, list(T), list(S))
        ;       n_foldl(proc_id, list(T), S, S).

:- type proc_id == string.

%---------------------------------------------------------------------------%

main(!IO) :-
    append_w([a, b, c], [d, e], As, P_1),
    write_proof(0, P_1, !IO),

    length_w(As, _, P_2),
    write_proof(0, P_2, !IO),

    isort_w([4, 2, 5, 3, 1], _, P_3),
    write_proof(0, P_3, !IO).

%---------------------------------------------------------------------------%

:- pred append_w(list(T), list(T), list(T), proof).
:- mode append_w(in, in, out, out) is det.
:- mode append_w(out, out, in, out) is multi.

append_w([], Bs, Bs, node(Node, assumed)) :-
    type_to_term(n_append([], Bs, Bs), Node).
append_w([A | As], Bs, [A | Cs], node(Node, Proof)) :-
    append_w(As, Bs, Cs, Proof),
    type_to_term(n_append([A | As], Bs, [A | Cs]), Node).

:- pred length_w(list(T), int, proof).
:- mode length_w(in, out, out) is det.

length_w([], 0, node(N, assumed)) :-
    type_to_term(n_length([] : list(int), 0), N).
length_w([A | As], N + 1, node(Node, Proof)) :-
    length_w(As, N, Proof),
    type_to_term(n_length([A | As], N + 1), Node).

:- pred isort_w(list(int), list(int), proof).
:- mode isort_w(in, out, out) is det.

isort_w([], [], node(Node, assumed)) :-
    type_to_term(n_isort([], []), Node).
isort_w([A | As], Ss, node(Node, conj([Proof_1, Proof_2]))) :-
    isort_w(As, Ss0, Proof_1),
    insert_w(A, Ss0, Ss, Proof_2),
    type_to_term(n_isort([A | As], Ss), Node).

:- pred insert_w(int, list(int), list(int), proof).
:- mode insert_w(in, in, out, out) is det.

insert_w(N, [], [N], node(Node, assumed)) :-
    type_to_term(n_insert(N, [], [N]), Node).
insert_w(N, [A | As], Ss, node(Node, Proof)) :-
    ( if N =< A then
        Ss = [N, A | As],
        Proof = if_then(assumed, assumed)
    else
        insert_w(N, As, Ss0, Proof_1),
        Ss = [A | Ss0],
        Proof = else(failed, Proof_1)
    ),
    type_to_term(n_insert(N, [A | As], Ss), Node).

%------------------------------------------------------------

:- pred write_proof(int::in, proof::in, io::di, io::uo) is det.

write_proof(L, node(N, P), !IO) :-
    indent(L, !IO),
    varset.init(V),
    write_term(V, N, !IO),
    nl(!IO),
    write_proof(L + 1, P, !IO).
% write_proof(L, leaf(Closure), !IO) :-
%   Closure(P),
%   write_proof(L, P, !IO).
write_proof(L, new_types(P), !IO) :-
    write_proof(L, P, !IO).
write_proof(L, conj(Ps), !IO) :-
    foldl(write_proof(L), Ps, !IO).
write_proof(L, disj(_, P), !IO) :-
    write_proof(L, P, !IO).
write_proof(L, if_then(P_1, P_2), !IO) :-
    write_proof(L, P_1, !IO),
    write_proof(L, P_2, !IO).
write_proof(L, else(_, P_2), !IO) :-
    write_proof(L, P_2, !IO).
write_proof(_, not(_), !IO).
write_proof(_, assumed, !IO).

:- pred indent(int::in, io::di, io::uo) is det.

indent(L, !IO) :-
    ( if L > 0 then
        io.write_char('\t', !IO),
        indent(L - 1, !IO)
    else
        true
    ).
