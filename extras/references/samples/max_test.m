%-----------------------------------------------------------------------------%
% Copyright (C) 1998 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File      : max_test.m
% Authors   : pets (Peter Schachte)
% Purpose   : test the max_of module
%

:- module max_test.
:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module max_of.
:- import_module int.

main -->
	( { max_of(square_mod_29, Max) } ->
		write_string("The biggest small square mod 29 is "),
		write_int(Max),
		write_string("\n")
	;
		write_string("square_mod_29 failed!\n")
	).

:- pred square_mod_29(int).
:- mode square_mod_29(out) is nondet.

square_mod_29((I*I) mod 29) :-
	between(I, 1, 100).


:- pred between(int, int, int).
:- mode between(out, in, in) is nondet.

between(I, Low, High) :-
	Low =< High,
	(
		I = Low
	;
		between(I, Low+1, High)
	).

