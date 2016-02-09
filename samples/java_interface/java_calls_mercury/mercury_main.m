% This source file is hereby placed in the public domain.  -fjh (the author).

:- module mercury_main.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

    % Nothing from mercury_lib is used in mercury_main.
    % The import is needed to make sure mmake includes
    % mercury_lib in the executable.
:- import_module mercury_lib.

    % import the module which defines the Mercury interface to the
    % Java method java_main().
:- import_module java_main_int.

% main just invokes java_main
main(!IO) :-
    io.write_string("In Mercury main, about to call java_main...\n", !IO),
    java_main(!IO),
    io.write_string("Back in Mercury main.\n", !IO).
