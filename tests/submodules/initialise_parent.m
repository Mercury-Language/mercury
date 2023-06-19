%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is to test that `:- intialise' declarations are not
% written out to private interfaces.

:- module initialise_parent.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- include_module initialise_parent_helper_1.

:- implementation.

:- initialise parent_init/2.

:- pred parent_init(io::di, io::uo) is det.

main(!IO) :-
    io.write_string("This is main/2...\n", !IO).

parent_init(!IO) :-
    io.write_string("This is parent_init/2...\n", !IO).
