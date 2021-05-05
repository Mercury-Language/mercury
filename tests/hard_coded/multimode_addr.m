%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Take addresses of multimoded predicates and functions.

:- module multimode_addr.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("TEST 1\n", !IO),
    Rev = my_foldl(cons, 1 .. 5, []),
    io.write(Rev, !IO),
    io.nl(!IO),

    io.write_string("\nTEST 2\n", !IO),
    !:IO = my_foldl(echo, ["a", "b", "c"], !.IO),

    io.write_string("\nTEST 3\n", !IO),
    ( if RevB = my_foldl(maybe_cons, 1 .. 5, []) then
        io.write(RevB, !IO),
        io.nl(!IO)
    else
        io.write_string("found a multiple of three\n", !IO)
    ).

:- func my_foldl(func(L, A) = A, list(L), A) = A.
:- mode my_foldl(in(func(in, in) = out is semidet), in, in) = out is semidet.
:- mode my_foldl(in(func(in, di) = uo is det), in, di) = uo is det.
:- mode my_foldl(in(func(in, in) = out is det), in, in) = out is det.

my_foldl(F, L, A0) = A :-
    list.foldl(f2p(F), L, A0, A).

% Changing the type signature causes polymorphism.m to produce a lambda goal
% which is more than just a plain_call.  It creates type_infos before the
% actual call.
:- func my_foldl_b(func(int, A) = A, list(int), A) = A.
:- mode my_foldl_b(in(func(in, in) = out is semidet), in, in) = out is semidet.
:- mode my_foldl_b(in(func(in, di) = uo is det), in, di) = uo is det.
:- mode my_foldl_b(in(func(in, in) = out is det), in, in) = out is det.

my_foldl_b(F, L, A0) = A :-
    list.foldl(f2p(F), L, A0, A).

% Some reordering.
:- func my_foldl_silly(func(L, A) = A, list(L), A) = A.
:- mode my_foldl_silly(in(func(in, in) = out is semidet), in, in) = out
    is semidet.
:- mode my_foldl_silly(in(func(in, di) = uo is det), in, di) = uo is det.
:- mode my_foldl_silly(in(func(in, in) = out is det), in, in) = out is det.

my_foldl_silly(F, L, A0) = A :-
    list.foldl(P, L, A0, A),
    P = f2p(F1),
    F1 = F.

:- pred f2p(func(L, A) = A, L, A, A).
:- mode f2p(in(func(in, di) = uo is det), in, di, uo) is det.
:- mode f2p(in(func(in, in) = out is det), in, in, out) is det.
:- mode f2p(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- pragma promise_equivalent_clauses(pred(f2p/4)).

f2p(F::in(func(in, di) = uo is det), L::in, A0::di, A::uo) :-
    trace [io(!IO)] io.write_string("f2p: det, unique\n", !IO),
    F(L, A0) = A.

f2p(F::in(func(in, in) = out is det), L::in, A0::in, A::out) :-
    trace [io(!IO)] io.write_string("f2p: det, not unique\n", !IO),
    F(L, A0) = A.

f2p(F::in(func(in, in) = out is semidet), L::in, A0::in, A::out) :-
    trace [io(!IO)] io.write_string("f2p: semidet\n", !IO),
    F(L, A0) = A.

:- func echo(string::in, io::di) = (io::uo) is det.

echo(S, IO0) = IO :-
    io.write_string(S, IO0, IO1),
    io.nl(IO1, IO).

:- func maybe_cons(int::in, list(int)::in) = (list(int)::out) is semidet.

maybe_cons(X, Xs) = [X | Xs] :-
    mod(X, 3) \= 0.
