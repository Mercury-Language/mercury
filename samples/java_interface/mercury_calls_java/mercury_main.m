% This source file is hereby placed in the public domain.

:- module mercury_main.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

% import the module which defines the Mercury interface to the
% Java method JavaMain.java_main().
:- import_module java_main_int.

% main just invokes java_main
main(!IO) :-
	io.write_string("In Mercury main, about to call java_main...\n", !IO),
	java_main(!IO),
	io.write_string("Back in Mercury main.\n", !IO).
