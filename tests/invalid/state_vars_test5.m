%------------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et wm=0 tw=0
%
% This is a regression test. The versions of the compiler prior to 6 March 2004
% used to get an exception inside the compiler code (in svar_unifiers in
% make_hlds.m). This test checks that we get a meaningful error instead
% of a compiler abort.
%
%------------------------------------------------------------------------------%

:- module state_vars_test5.

:- interface.

:- import_module int.

:- pred p(int::out) is det.

:- implementation.

p(!:X).
