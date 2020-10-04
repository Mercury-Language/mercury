%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test should be compiled with --no-inlining. If inlining is enabled,
% the tests 42=43 and 43=43 will be evaluated at compile time, masking any
% bugs in the selection of which modes of p and q to call.
%

:- module cc_and_non_cc_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module std_util.

main(!IO) :-
    % test multi

    % call in single-solution context
    p(X),
    print(X, !IO),
    nl(!IO),

    % call in all-solutions context
    ( if p(43) then
        print("yes\n", !IO)
    else
        print("no\n", !IO)
    ),

    % test nondet

    % call in single-solution context
    ( if q(X) then
        print(X, !IO),
        nl(!IO)
    else
        print("no\n", !IO)
    ),
    % call in all-solutions context
    ( if q(43) then
        print("yes\n", !IO)
    else
        print("no\n", !IO)
    ).

:- pred p(int).
:- mode p(out) is multi.
:- mode p(out) is cc_multi.
p(42).
p(43).

:- pred q(int).
:- mode q(out) is cc_nondet.
:- mode q(out) is nondet.
q(42) :- semidet_succeed.
q(43) :- semidet_succeed.
