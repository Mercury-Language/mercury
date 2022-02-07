%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug544.

:- interface.

:- type t1.
:- type t2.

:- pred s(int::in, t2::out) is cc_nondet.

:- implementation.
:- import_module int.
:- import_module list.

:- type t1
    --->    f(string, int).

:- type t2
    --->    g(t1, t1)
    ;       i.

s(X, Y) :-
    % The bug happened because, after the construction of Result
    % was transformed by simplification into a reference to a ground term,
    %
    % - add_trail_ops.m wrapped a disjunction around this scope,
    %   in which the first disjunct defines Result, while the second
    %   disjunct, which could not succeed, did not, but
    %
    % - ml_disj_gen.m used ml_gen_goal_as_branch_block to generate code
    %   for each disjunct, and that predicate threw away any changes made
    %   to the const_var_map during each disjunct.
    %
    % This meant that when the code generator reached the construction of Y,
    % which was marked as being constructed statically by a pass before
    % add_trail_ops.m, it could not find Result in the const_var_map.
    % The map lookup failure caused a compiler crash.
    some [Z] (
        (Z = 1 ; Z = 2),
        X = Z * Z,
        Result = f("hello", 0)
    ),
    Y = g(Result, Result).
