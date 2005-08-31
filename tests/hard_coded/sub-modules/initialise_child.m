:- module initialise_parent.initialise_child.

:- interface.

:- type foo ---> foo.

:- implementation.

:- initialise child_init.

:- pred child_init(io::di, io::uo) is det.

child_init(!IO) :- io.write_string("This is child_init/2...\n", !IO).
