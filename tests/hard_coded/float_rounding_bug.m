%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test; Mercury 0.5 and earlier had a bug in which
% floating point division expressions involving whole numbers were computed
% using integer division rather than floating point division.
%

:- module float_rounding_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module float.

main(!IO) :-
    io.write_float(1.0/2.0, !IO),
    io.write_char('\n', !IO).
