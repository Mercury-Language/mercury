% This source file is hereby placed in the public domain.  -fjh (the author).

:- module mercury_main.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

% import the module which defines the Mercury interface to the
% Fortran procedure `FORTRAN_MAIN'.
:- import_module fortran_main_int.

% main just invokes fortran_main
main -->
	io__write_string("In Mercury main, about to call fortran_main...\n"),
	fortran_main,
	io__write_string("Back in Mercury main.\n").
