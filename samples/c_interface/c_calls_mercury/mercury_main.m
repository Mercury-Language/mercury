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
% C function c_main().
:- import_module c_main_int.

% main just invokes c_main
main -->
	io__write_string("In Mercury main, about to call c_main...\n"),
	c_main,
	io__write_string("Back in Mercury main.\n").
