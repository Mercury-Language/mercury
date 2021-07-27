%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test.  The compiler incorrectly computed the instmap delta of
% generic calls with no solutions.

% On this test case, the compiler would abort:
%
% mmc -C --local-constraint-propagation instmap_generic_failure.m
% Uncaught Mercury exception:
% Software Error: instmap.m: Unexpected:
%   merge_instmapping_delta_2: error merging var 8
%
% The instmap for the then branch (below) is something like
% `reachable([var(New)])' and the instmap for the else branch was
% `reachable([])', so it couldn't merge them.  After the fix, the instmap for
% the else branch is `unreachable' so merging succeeds.

:- module instmap_generic_failure.
:- interface.

:- import_module list.

:- pred int_to_ascii(pred(int, int), int, list(int), list(int)).
:- mode int_to_ascii(in(pred(in, out) is failure), in, in, out) is semidet.

:- implementation.

:- import_module int.

int_to_ascii(ConvertNonAscii, U, Old, New) :-
    ( if U < 128 then
        New = [U | Old]
    else
        ConvertNonAscii(U, S),  % failure
        New = [S | Old]
    ).
