%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is to test that `:- finalise' declarations are not
% written out to private interfaces.

:- module finalise_parent.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- include_module finalise_parent_helper_1.

:- implementation.

:- finalise parent_final/2.

:- pred parent_final(io::di, io::uo) is det.

main(!IO) :-
    io.write_string("This is main/2...\n", !IO).

parent_final(!IO) :-
    io.write_string("This is parent_final/2...\n", !IO).
