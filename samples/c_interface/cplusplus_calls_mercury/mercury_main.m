% This source file is hereby placed in the public domain.  -fjh (the author).

:- module mercury_main.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

	% Nothing from mercury_lib is used in mercury_main.
	% The import is needed to make sure mmake includes
	% mercury_lib in the executable.
:- import_module mercury_lib.

% import the module which defines the Mercury interface to the
% C++ function cpp_main().
:- import_module cpp_main_int.

% main just invokes cpp_main
main -->
	io__write_string("In Mercury main, about to call cpp_main...\n"),
	cpp_main,
	io__write_string("Back in Mercury main.\n").
