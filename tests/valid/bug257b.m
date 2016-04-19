%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Don't emit an erroneous warning about the variable that is the subject
% of a require_complete_switch scope not occurring in the sub-goal if
% the sub-goal in question is not a switch and the variable in question
% does not occur in the non-local set of the sub-goal.
% (See also tests/warnings/bug257b.m.)
%
% This test is now disabled, since we now generate an error when a
% require_complete_switch scope is wrapped around a goal that is not
% a switch on the named variable.

:- module bug257b.
:- interface.

:- import_module io.

:- type xyz
    --->    x
    ;       y
    ;       z.

:- pred oops(xyz::in, int::out, io::di, io::uo) is det.

:- implementation.

oops(_G, N, !IO) :-
    require_complete_switch [Gee] (
        Gee = 123,
        io.write(Gee, !IO),
        N = 3
    ).
