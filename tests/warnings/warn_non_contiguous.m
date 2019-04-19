%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a test of the compiler's ability to properly diagnose non-contiguous
% clauses for a predicate.
%
%---------------------------------------------------------------------------%

:- module warn_non_contiguous.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    test(1, !IO),
    test(3, !IO),
    test(5, !IO),
    test(7, !IO),
    test(9, !IO),
    test(11, !IO),
    test(13, !IO).

:- pred test(int::in, io::di, io::uo) is det.

test(N, !IO) :-
    ( if p(N, PM) then
        io.format("p(%d) = %d\n", [i(N), i(PM)], !IO)
    else
        io.format("p(%d) failed\n", [i(N)], !IO)
    ),
    ( if q(N, QM) then
        io.format("q(%d) = %d\n", [i(N), i(QM)], !IO)
    else
        io.format("q(%d) failed\n", [i(N)], !IO)
    ).

:- pred p(int::in, int::out) is semidet.

q(10, 11).
p(1, 2).
p(2, 3).
p(3, 4).
q(11, 12).

:- pred q(int::in, int::out) is semidet.

p(0, 1).
q(12, 13).
q(13, 14).
