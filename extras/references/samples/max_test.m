%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998 University of Melbourne.
% Copyright (C) 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: max_test.m.
% Authors: pets (Peter Schachte)
% Purpose: Test the max_of module.
%
%-----------------------------------------------------------------------------%

:- module max_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module max_of.

:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    ( if max_of(square_mod_29, Max) then
        io.format("The biggest small square mod 29 is %d\n", [i(Max)], !IO)
    else
        io.write_string("square_mod_29 failed!\n", !IO)
    ).

:- pred square_mod_29(int::out) is nondet.

square_mod_29((I * I) mod 29) :-
    between(I, 1, 100).

:- pred between(int::out, int::in, int::in) is nondet.

between(I, Low, High) :-
    Low =< High,
    (
        I = Low
    ;
        between(I, Low + 1, High)
    ).

%-----------------------------------------------------------------------------%
:- end_module max_test.
%-----------------------------------------------------------------------------%
