%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case is a regression test to check that we don't issue
% a spurious warning about infinite recursion in recursive procedures
% such as label/1 whose inputs initial inst contains `any'.
%
%---------------------------------------------------------------------------%

% This module is an example of how to write a finite domain solver.

:- module solv.
:- interface.

:- import_module io.
:- import_module list.

:- solver type fd_var.

    % initialize an unconstrained fd_var
:- pred init_any(fd_var).
:- mode init_any(free >> any) is det.

    % unify an fd_var with an int
:- pred fd_var == int.
:- mode in == out is det.
:- mode (any >> ground) == in is semidet.

    % constrain an fd_var to be greater than the specified int
:- pred fd_var > int.
:- mode in(any) > in is semidet.

    % constrain an fd_var to be less than the specified int
:- pred fd_var < int.
:- mode in(any) < in is semidet.

    % Given a list of constrained fd_vars, nondeterminstically
    % find bindings for the variables that meet those constraints.
    % The output list here will be the same as the input list,
    % but with ground values for all the variables.
:- pred labeling(list(fd_var), list(fd_var)).
:- mode labeling(in(list_skel(any)), out) is nondet.

    % Given a list of constrained fd_vars,
    % print out all possible bindings for the variables
    % that meet those constraints.  The order in which
    % the solutions will be printed is unspecified.
:- pred print_labeling(list(fd_var), io, io).
:- mode print_labeling(in(list_skel(any)), di, uo) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module solutions.

:- solver type fd_var
    where   representation is c_pointer,
            ground         is ground,
            any            is ground.

print_labeling(Vars, !IO) :-
    Labeling0 =
        ( impure pred(Labels::out) is nondet :-
            labeling(Vars, Labels)
        ),
    Labeling =
        ( pred(Labels::out) is nondet :-
            promise_pure ( impure Labeling0(Labels) )
        ),
    unsorted_aggregate(Labeling, print_solution, !IO).

:- pred print_solution(list(fd_var)::in, io::di, io::uo) is det.

print_solution(Vars, !IO) :-
    io.print("Here's a solution: ", !IO),
    io.write_list(Vars, ", ", print_var, !IO),
    io.nl(!IO).

:- pred print_var(fd_var::in, io::di, io::uo) is det.

print_var(Var, !IO) :-
    Var == Val, % convert ground fd_var to int
    io.write_int(Val, !IO).

labeling([], []).
labeling([V | Vs0], [V | Vs]) :-
    label(V),
    labeling(Vs0, Vs).

:- pred label(fd_var).
:- mode label(any >> ground) is nondet.
:- pragma promise_pure(label/1).

label(V) :-
    impure solver_min_domain(V, Min),
    impure solver_max_domain(V, Max),
    ( if Min = Max then
        promise_ground(V)
    else
        (
            V == Min
        ;
            V > Min,
            label(V)
        )
    ).

:- pred promise_ground(fd_var).
:- mode promise_ground(any >> ground) is det.

:- pragma foreign_proc("C",
    promise_ground(X :: any >> ground),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /* assert(X->min == X->max); */
").

:- impure pred solver_min_domain(fd_var, int).
:-        mode solver_min_domain(in(any), out) is det.

:- impure pred solver_max_domain(fd_var, int).
:-        mode solver_max_domain(in(any), out) is det.

%---------------------------------------------------------------------------%

% Implementing the following is left as an exercise for the reader...
:- pragma external_pred(init_any/1).
:- pragma external_pred(solver_min_domain/2).
:- pragma external_pred(solver_max_domain/2).
:- pragma external_pred((==)/2).
:- pragma external_pred((>)/2).
:- pragma external_pred((<)/2).

%---------------------------------------------------------------------------%
