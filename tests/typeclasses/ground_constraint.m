%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ground_constraint.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.

main(!IO) :-
    S1 = f(0),
    io.write_string(S1, !IO),
    io.nl(!IO),

    p(0, S2),
    io.write_string(S2, !IO),
    io.nl(!IO),

    q([0], S3),
    io.write_string(S3, !IO),
    io.nl(!IO).

:- typeclass foo(T) where [
    func s(T) = string
].

:- instance foo(int) where [
    (s(_) = "bar")
].

:- func f(int) = string <= foo(int).

f(N) = s(N).

:- pred p(int::in, string::out) is det <= foo(int).

p(N, s(N)).

:- instance foo(list(T)) <= foo(T) where [
    (s(_) = "baz")
].

:- pred q(list(int)::in, string::out) is det <= foo(list(int)).

q(Ns, s(Ns)).
