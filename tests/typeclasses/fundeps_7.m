%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module fundeps_7.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- typeclass a(X, A) <= (A -> X) where [
].

:- typeclass b(B, X) <= a(X, B) where [
    % X is determined by the functional dependency on a/2, which we
    % should inherit.
    func b(B) = int
].

:- instance a(int, int) where [
].

:- instance b(int, int) where [
    (b(N) = N)
].

    % X is determined by the functional dependency on a/2, which
    % should be inherited by b/2.
    %
:- type foo
    --->    some [B, X] foo(B) => b(B, X).

main(!IO)  :-
    ( if b(1) = b(2) then
        write_string("yes\n", !IO)
    else
        write_string("no\n", !IO)
    ).

