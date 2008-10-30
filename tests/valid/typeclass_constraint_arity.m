% Check that we support type class constraints for type classes with arity up
% to 10.  Low-level C grades can support higher arities but high-level C grades
% are currently limited.

:- module typeclass_constraint_arity.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- typeclass tc(A, B, C, D, E, F, G, H, I, J, K)
    <= (
        tc6(A, B, C, D, E, F),
        tc7(A, B, C, D, E, F, G),
        tc8(A, B, C, D, E, F, G, H),
        tc9(A, B, C, D, E, F, G, H, I),
        tc10(A, B, C, D, E, F, G, H, I, J)
        % tc11(A, B, C, D, E, F, G, H, I, J, K)
    ) where [].

:- typeclass tc6(A, B, C, D, E, F) where [].
:- typeclass tc7(A, B, C, D, E, F, G) where [].
:- typeclass tc8(A, B, C, D, E, F, G, H) where [].
:- typeclass tc9(A, B, C, D, E, F, G, H, I) where [].
:- typeclass tc10(A, B, C, D, E, F, G, H, I, J) where [].
% :- typeclass tc11(A, B, C, D, E, F, G, H, I, J, K) where [].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

main(!IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
