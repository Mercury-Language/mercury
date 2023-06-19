%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module finalise_parent.finalise_parent_helper_1.

:- interface.

:- type foo
    --->    foo.

:- implementation.

:- finalise child_final/2.

:- pred child_final(io::di, io::uo) is det.

child_final(!IO) :-
    io.write_string("This is child_final/2...\n", !IO).
