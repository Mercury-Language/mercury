% This is a regression test; Mercury 0.5 and earlier had
% a bug in which floating point division expressions involving
% whole numbers were computed using integer division rather
% than floating point division.

:- module float_rounding_bug.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module float.

main --> io__write_float(1.0/2.0), io__write_char('\n').
