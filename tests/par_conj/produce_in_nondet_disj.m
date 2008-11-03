% vim: ft=mercury ts=4 sw=4 et

% This is a regression test. Before 30 Oct 2008, this test case generated a
% runtime abort: "Assertion `Future->MR_fut_value == Value' failed.".
% The reason was the the compiler inserted a signal operation after each
% unification binding C, and the failure of the first few invocations of
% the test predicate caused backtracking that lead to more than one of these
% signal operations to be executed with different values.
%
% In my thesis, I called this problem "stability": a parallel consumer of a
% shared variable should see that the value of that shared variable only when
% that value is *stable*, i.e. when the producer has committed to that value.
% That requires delaying the signal operation from when C is bound (the
% unifications) to when C becomes stable (the end of the
% promise_equivalent_solutions scope).

:- module produce_in_nondet_disj.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
    p(A, B),
    (
        promise_equivalent_solutions [C] (
            (
                (
                    C = 0
                ;
                    A = 1,
                    C = 10
                ;
                    B = 2,
                    C = 20
                ;
                    C = 30      % We commit to this value of C.
                ),
                test(A, B, C)
            ;
                C = 40
            )
        )
    &
        q(C, D)
    ),
    io.write(D, !IO),
    io.nl(!IO).

:- pred p(int::out, int::out) is det.
:- pragma no_inline(p/2).

p(1, 2).

:- pred test(int::in, int::in, int::in) is semidet.
:- pragma no_inline(test/3).

test(A, B, C) :-
    C > 0,
    not C = A * 10,
    not C = B * 10.

:- pred q(int::in, int::out) is det.
:- pragma no_inline(q/2).

q(C, D) :-
    D = C + 1.
