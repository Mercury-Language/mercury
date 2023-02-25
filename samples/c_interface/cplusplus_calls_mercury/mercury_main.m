% This source file is hereby placed in the public domain.  -fjh (the author).

:- module mercury_main.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

    % Nothing from mercury_lib is used in mercury_main.
    % The import is needed to make sure mmake includes mercury_lib in the
    % executable.
:- import_module mercury_lib.

% Import the module which defines the Mercury interface to the C++ function
% cpp_main().
:- import_module cpp_main_int.

% main just invokes cpp_main
main(!IO) :-
    io.write_string("In Mercury main, about to call cpp_main...\n", !IO),
    cpp_main(!IO),
    io.write_string("Back in Mercury main.\n", !IO).
