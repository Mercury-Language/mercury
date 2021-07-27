%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unbound_tvar_in_lambda.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module int.
:- import_module std_util.
:- import_module require.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("--- Start Proofs ---\n\n", !IO),
    Pred =
        ( pred(P1::out) is nondet :-
            append_w([1, 2, 3, 4], [5, 6, 7], _, P1)
        ),
    io.write(Pred, !IO),
    io.write_string("--- End Proofs ---\n\n", !IO).

%---------------------------------------------------------------------------%

:- type node(S, T)
    --->    append(list(S), list(S), list(S)).

:- type proof(N)
    --->    node(N, proof(N))
    ;       assumed.

:- type proof(S, T) == proof(node(S, T)).

% Simple polymorphic examples.

:- pred append_w(list(S), list(S), list(S), proof(S, T)).
:- mode append_w(in, in, out, out) is det.
:- mode append_w(out, out, in, out) is multi.

append_w([], Bs, Bs, node(append([], Bs, Bs), assumed)).
append_w([A | As], Bs, [A | Cs], node(append([A | As], Bs, [A | Cs]), Proof)) :-
    append_w(As, Bs, Cs, Proof).
