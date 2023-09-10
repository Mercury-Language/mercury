%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Mercury ROTD 2/8/2000 reported a type error when typechecking the clauses
% for foo/2 from dcg_bug_helper_1.opt. The problem was that the DCG arguments
% introduced for the DCG pred expressions clashed with the DCG arguments for
% the enclosing clauses. The solution was to add variable numbers to clauses
% in `.opt' files.
%
% This test was originally called intermod_dcg_bug.
%

:- module dcg_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module dcg_bug_helper_1.

main(!IO) :-
    foo(!IO).
