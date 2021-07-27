%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% When compiled with:
%
%   $ mmc --errorcheck-only zinc2mer_lib.m
%
% this module generated the following assertion failure in rotd-2007-10-23:
%
%   Software Error: error_util.m: Unexpected: do_write_error_spec: \
%   MaybeActual isn't no
%
% This bug was originally reported by Nick Nethercote. The code here
% is a cut-down version of his code.
%
% This test is disabled, because automatic initialization of solver variables
% is no longer supported.
%
%---------------------------------------------------------------------------%

:- module zinc2mer_lib.
:- interface.

:- import_module list.

:- solver type fdvar.

:- type constraint_expression
    --->    v(fdvar).

:- pred new_fdvar(fdvar::oa) is det.

:- impure pred min_domsize_min_split(list(fdvar)::ia,
    list(constraint_expression)::oa) is det.

:- implementation.

:- import_module require.

min_domsize_min_split([], _) :- error("empty list").
min_domsize_min_split([V | _Vs], BranchConstraints) :-
    impure BranchConstraints = min_split_var(V).

:- impure func min_split_var(fdvar::ia) = (list(constraint_expression)::ia)
    is det.

min_split_var(V) = BranchConstraints :-
    impure get_min_max(V, Min, Max),
    ( if Min = Max then
        error("")
    else
        BranchConstraints = []
    ).

:- impure pred get_min_max(fdvar::ia, int::out, int::out) is det.

get_min_max(_, 3, 7) :-
    impure impure_true.

:- solver type fdvar
    where   representation  is int,
            initialisation  is new_fdvar.

new_fdvar(V) :-
    promise_pure (
        impure V = 'representation to any fdvar/0'(10)
    ).
