% This source file is hereby placed in the public domain.  -fjh (the author).

:- module mercury_main.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

% Import the module which defines the Mercury interface to the
% C++ function cpp_main().
:- import_module cpp_main_int.

% main just invokes cpp_main
main(!IO) :-
    io.write_string("In Mercury main, about to call cpp_main...\n", !IO),
    cpp_main(!IO),
    io.write_string("Back in Mercury main.\n", !IO).
