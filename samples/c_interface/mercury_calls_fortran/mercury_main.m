% This source file is hereby placed in the public domain.  -fjh (the author).

:- module mercury_main.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

% Import the module which defines the Mercury interface to the
% Fortran procedure `FORTRAN_MAIN'.
:- import_module fortran_main_int.

% main just invokes fortran_main
main(!IO) :-
    io.write_string("In Mercury main, about to call fortran_main...\n", !IO),
    fortran_main(!IO),
    io.write_string("Back in Mercury main.\n", !IO).
