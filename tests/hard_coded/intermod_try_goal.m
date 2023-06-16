%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_try_goal.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module intermod_try_goal_helper_1.
:- import_module exception.

main(!IO) :-
    catcher(thrower, !IO),
    io.write_string("done.\n", !IO).

:- pred thrower(io::di, io::uo) is det.

thrower(!IO) :-
    ( if semidet_true then
        throw("catch me")
    else
        true
    ).
