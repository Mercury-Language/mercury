% This is a regression test. In some versions of the compiler,
% mode analysis notices that `1 = 2' can't succeed, and replaces the
% if-then-else with `(not(1 = 2), true)', but det analysis doesn't
% notice that 1 = 2 cannot succeed, and so reports a determinism error.

:- module unreachable.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

main --> ( { 1 = 2 } -> { true } ; { true } ).
