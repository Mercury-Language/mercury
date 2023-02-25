% This source file is hereby placed in the public domain.  -fjh (the author).

:- module mercury_main.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

% Import the module which defines the Mercury interface to the
% C function c_main().
:- import_module c_main_int.

% main just invokes c_main
main(!IO) :-
    io.write_string("In Mercury main, about to call c_main...\n", !IO),
    c_main(!IO),
    io.write_string("Back in Mercury main.\n", !IO).
