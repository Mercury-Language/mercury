%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case exposes a bug in the interaction of solver types and
% submodules in rotd-2005-09-06 and before. The problem was that the submodules
% were reading in the solver type declarations from the .int0 files
% and adding the foreign_procs for the representation and intialisation
% functions.
% (The test case is from Peter Hawkins.)

:- module ts.
:- interface.

:- include_module ts_helper_1.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module exception.
:- import_module int.

:- solver type st where representation is int.

:- pred init_int(st::oa) is erroneous.

init_int(_A) :-
    throw("stop").

main(!IO) :-
    print("hello\n", !IO).
