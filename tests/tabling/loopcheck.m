%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test.

% rotd-1999-10-29 got a software error when compiling an earlier version test.

:- module loopcheck.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- pragma require_feature_set([memo]).

main(!IO) :-
    loop(10),
    io.write_string("Hello, world\n", !IO).

:- pragma loop_check(loop/1).
:- pred loop(int::in) is det.

loop(X) :-
    loop(X).
