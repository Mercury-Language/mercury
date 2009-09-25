% vim: ts=4 sw=4 et ft=mercury
%
% This is a regression test for Mantis bug 93. Versions of the compiler before
% 22 Sep 2009 used to throw "Unexpected: do_unravel_unification:
% from_ground_term not conj" on this code. The reason is that the
% unification introduced for the first argument of the call to log_tf
% has a ground term above the size % threshold specified in Mercury.options
% on the right hand side, but unraveling it yields not a conjunction of
% construct unifications, but a single unification with an rhs_lambda_goal
% right hand side.

:- module from_ground_term_bug.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string.

main(!IO):-
    log_tf(((func) = "s3nav Server version: " ++ "\n" ++ "\n" ++ "\n"), !IO),
    io.write_string("ok\n", !IO).

:- pred log_tf(((func) = string)::in, io::di, io::uo) is det.

log_tf(_, !IO).
