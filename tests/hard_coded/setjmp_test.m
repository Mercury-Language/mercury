%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test:
% the compiler used to generate code for this in the hlc.gc grade
% which had undefined behaviour according to ANSI/ISO C,
% and which would in practice fail on some systems
% (e.g. alpha*-dec-osf3.2 with egcs-1.1.2).
%

:- module setjmp_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module std_util.

main(!IO) :-
    ( if p(X) then
        print("X = ", !IO),
        print(X, !IO),
        nl(!IO)
    else
        print("no", !IO)
    ).

:- pred p(int).
:- mode p(out) is nondet.
:- pragma inline(p/1).

p(1) :- semidet_fail.
p(2) :- semidet_succeed.
p(3) :- semidet_fail.
