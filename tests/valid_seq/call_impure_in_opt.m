%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The compiler of 23/8/1999 had a bug which caused a mode error
% if the clauses in a `.opt' file contained a call to an impure predicate.
% The problem was that mode analysis was not reordering the head unifications
% with the impure call, because the head unifications had been expanded
% into separate unifications twice - once when reading the `.m' file
% and once when reading the `.opt' file.
%
% The fix was to avoid putting goals which call impure predicates
% into `.opt' files.
%
% This test was originally called intermod_impure.
%

:- module call_impure_in_opt.

:- interface.

:- import_module io.

:- impure pred main(io::di, io::uo) is det.

:- implementation.

:- import_module call_impure_in_opt_helper_1.

main(!IO) :-
    impure intermod_impure(Int),
    io.write_int(Int, !IO),
    io.nl(!IO).
