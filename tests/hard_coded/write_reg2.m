% Regression test:
% 
% This test ensures that tuple types are written out correctly.
%
% The Mercury compiler of 12 Dec 2000 failed to correctly
% run this test.
% 
% Author: trd

:- module write_reg2.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module pair, univ.

main -->
	io__write(univ((1 - 2))),
	io__nl,
	io__write(univ({1,2,3})),
	io__nl.
