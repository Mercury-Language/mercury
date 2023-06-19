%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module initialise_parent.initialise_parent_helper_1.

:- interface.

:- type foo
    --->    foo.

:- implementation.

:- initialise child_init/2.

:- pred child_init(io::di, io::uo) is det.

child_init(!IO) :-
    io.write_string("This is child_init/2...\n", !IO).
