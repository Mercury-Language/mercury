%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a test of the compiler's ability to properly diagnose non-contiguous
% clauses for a predicate, when some of those clauses are actually
% foreign_procs.
%
% It is also a regression test for a bug that existed in versions of the
% compiler prior to the addition of this test case in august 2009.
% The bug was that the code for adding new foreign_procs did not handle
% q's clauses properly. q's foreign_proc overrode the second existing Mercury
% clause for q, but not the first. The resulting two-clause q then had a
% disjunction, and as result it had a determinism error.
%
% The match against the exact text of the output of the compiler
% guards against the reappearance of this bug.
%
%---------------------------------------------------------------------------%

:- module warn_non_contiguous_foreign.
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

:- pragma foreign_proc("C",
    p(N::in, M::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (N < 10) {
        M = N + 1;
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

q(11, 12).

:- pred q(int::in, int::out) is semidet.

p(0, 1).

:- pragma foreign_proc("C",
    q(N::in, M::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (N < 5) {
        M = N + 1;
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").
