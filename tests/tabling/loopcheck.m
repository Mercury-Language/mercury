% Regression test.

% rotd-1999-10-29 got a software error when compiling this test.

:- module loopcheck.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main --> { loop(10) }, io__write_string("Hello, world\n").

:- pragma loop_check(loop/1).
:- pred loop(int::in) is erroneous.
loop(X) :- loop(X).
