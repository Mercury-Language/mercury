%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case exposes a bug with intermodule optimization
% in rotd-2005-11-02 and before. (This is a cut-down version
% of the original test case provided by Peter Hawkins.)
%

:- module intermod_poly_mode.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module intermod_poly_mode_helper_1.

main(!IO) :-
    New = new(561),
    io.write_line(New, !IO).
