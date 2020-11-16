%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module multi_parameter_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.

main(!IO) :-
    p(42, 'a', !IO).

:- pred p(T1::in, T2::in, io::di, io::uo) is det <= c2(T1, T2).

p(X, _, !IO) :-
    m1(X, Y),
    io.write_int(Y, !IO),
    io.nl(!IO).

:- typeclass c1(T) where [
    pred m1(T, int),
    mode m1(in, out) is det
].

:- typeclass c2(T1, T2) <= c1(T1) where [
].

:- instance c1(int) where [
    m1(X, X)
].

:- instance c2(int, char) where [
].
