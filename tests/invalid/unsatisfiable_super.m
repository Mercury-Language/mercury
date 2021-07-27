%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unsatisfiable_super.
:- interface.
:- import_module io.

:- typeclass foo(A, B) where [ func f(A) = B ].
:- typeclass bar(B) <= foo(int, B) where [].

:- pred test(B::in, io::di, io::uo) is det <= bar(B).

:- implementation.

:- instance foo(int, int) where [ (f(N) = N) ].

test(X, !IO) :-
    io.write_int(f(X), !IO).
