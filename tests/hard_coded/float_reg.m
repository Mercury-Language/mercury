%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test:
%
% Test case for creation of float constants.
%
% The Mercury compiler of 21 December 1996 failed to compile this on
% SPARC platforms, because the + in the float name was not converted
% correctly. This leads to syntax errors in the generated C code,
% eg
% flo.c:22: syntax error before `+'
% flo.c:23: `mercury_float_const_2pt88e' undeclared (first use this function)
%
% Author: trd
%

:- module float_reg.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module io.
:- import_module float.

main(!IO) :-
    io.write_float(2.88e32, !IO),
    io.write_string("\n", !IO),
    io.write_float(1.0e32, !IO),
    io.write_string("\n", !IO),
    io.write_float(1.0e10, !IO),
    io.write_string("\n", !IO).

