% This test case exposes a bug in the interaction of solver types and
% sub-modules in rotd-2005-09-06 and before.  The problem was that the
% sub-modules were reading in the solver type declarations from the .int0 files
% and adding the foreign_procs for the representation and intialisation
% functions.
% (The test case is from Peter Hawkins.)
:- module ts.
:- interface.
:- include_module tsub.
:- import_module io.
:- pred main(io.state::di, io.state::uo) is det.
:- implementation.

:- import_module int, exception.
:- solver type st where representation is int,
    initialisation is init_int.

:- pred init_int(st::oa) is erroneous.
init_int(_A) :- throw("stop").

main(!IO) :- print("hello\n", !IO).
