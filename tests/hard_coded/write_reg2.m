%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test:
%
% This test ensures that tuple types are written out correctly.
%
% The Mercury compiler of 12 Dec 2000 failed to correctly run this test.
%
% Author: trd
%

:- module write_reg2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module pair.
:- import_module univ.

main(!IO) :-
    io.write_line(univ((1 - 2)), !IO),
    io.write_line(univ({1, 2, 3}), !IO).
