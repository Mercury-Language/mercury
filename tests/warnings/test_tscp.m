%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Test the operation of type_spec_constrained_preds pragmas.
% We keep the module name short to make the type_spec pragmas
% that the compiler outputs as informational messages fit on one line.
%---------------------------------------------------------------------------%

:- module test_tscp.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module term.

main(!IO) :-
    test(41, 42, 43, 44, N),
    io.write_int(N, !IO),
    io.nl(!IO).

:- typeclass tc1(E, F, G, H) <= (tc2(E, int, H), tc4(float, G)) where [].
:- typeclass tc2(K, L, M) <= (tc3(K, L), tc3(L, M)) where [].
:- typeclass tc3(P, Q) where [].
:- typeclass tc4(S, T) where [].

:- instance tc1(int, int, int, int) where [].
:- instance tc2(int, int, int) where [].
:- instance tc3(int, int) where [].
:- instance tc4(float, int) where [].

:- pragma type_spec_constrained_preds(
    [tc1(X1, X2, X3, X4)],
    apply_to_superclasses,
    [subst([X2 = char, X3 = bool]),
    subst([X3 = int, X4 = var(Y)])]).

:- pred test(A::in, B::in, C::in, D::in, D::out) is det <= tc1(A, B, C, D).

test(_A, _B, _C, !D).
