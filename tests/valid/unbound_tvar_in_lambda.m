:- module unbound_tvar_in_lambda.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module list, int, std_util, require.

%--------------------------------------------------------%

:- type node(S, T)
        --->    append(list(S), list(S), list(S)).

:- type proof(N)
        --->    node(N, proof(N))
        ;       assumed.

:- type proof(S, T) == proof(node(S, T)).

%--------------------------------------------------------%

% Simple polymorphic examples

:- pred append_w(list(S), list(S), list(S), proof(S, T)).
:- mode append_w(in, in, out, out) is det.
:- mode append_w(out, out, in, out) is multi.

append_w([], Bs, Bs, node(append([], Bs, Bs), assumed)).
append_w([A|As], Bs, [A|Cs], node(append([A|As], Bs, [A|Cs]), Proof)) :-
        append_w(As, Bs, Cs, Proof).


%--------------------------------------------------------%

main -->
        write_string("--- Start Proofs ---\n\n"),
        { Pred = (pred(P1::out) is nondet :-
                        append_w([1,2,3,4], [5,6,7], _, P1)) },
	io__write(Pred),
        write_string("--- End Proofs ---\n\n").

