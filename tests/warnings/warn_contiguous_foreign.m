%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a test of the compiler's ability to properly AVOID reporting
% non-contiguous clauses for a predicate when the totality of all clauses
% and foreign_procs of a predicate are contiguous, but the applicable ones
% are not contiguous.
%
%---------------------------------------------------------------------------%

:- module warn_contiguous_foreign.
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
    p(N, M),
    io.format("p(%d) = %d\n", [i(N), i(M)], !IO).

:- pred p(int::in, int::out) is det.

p(N, M) :-
    M = N + 1.

:- pragma foreign_proc("Java",
    p(N::in, M::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    M = N + 1;
").

:- pragma foreign_proc("C#",
    p(N::in, M::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    M = N + 1;
").

:- pragma foreign_proc("C",
    p(N::in, M::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    M = N + 1;
").

:- pragma foreign_proc("Erlang",
    p(N::in, M::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    M = N + 1
").
