%-----------------------------------------------------------------------------%

% This test case is a regression test to check that we don't
% issue a spurious warning about infinite recursion in recursive
% procedures such as label/1 whose inputs initial inst contains `any'.

%-----------------------------------------------------------------------------%

% This module is an example of how to write a finite domain solver.

:- module solv.
:- interface.
:- import_module io, list, int.

% :- type fd_var.	% For Mercury versions < rotd-25-07-03
:- solver type fd_var.  % For Mercury versions > rotd-25-07-03

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
:- pred print_labeling(list(fd_var), io__state, io__state).
:- mode print_labeling(in(list_skel(any)), di, uo) is cc_multi.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util.

:- solver type fd_var
	---> fd_var(c_pointer).

print_labeling(Vars) -->
	unsorted_aggregate(labeling(Vars), print_solution).

:- pred print_solution(list(fd_var), io__state, io__state).
:- mode print_solution(in, di, uo) is det.

print_solution(Vars) -->
	io__print("Here's a solution: "),
	io__write_list(Vars, ", ", print_var),
	io__nl.

:- pred print_var(fd_var, io__state, io__state).
:- mode print_var(in, di, uo) is det.
print_var(Var) -->
	{ Var == Val }, % convert ground fd_var to int
	io__write_int(Val).

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
	(if Min = Max then
		promise_ground(V)
	else
		( V == Min
		; V > Min,
		  label(V)
		)
	).

:- pred promise_ground(fd_var).
:- mode promise_ground(any >> ground) is det.

:- pragma foreign_proc("C", promise_ground(X :: any >> ground),
	[promise_pure, will_not_call_mercury, thread_safe],
	"/* assert(X->min == X->max); */").

:- impure pred solver_min_domain(fd_var, int).
:-        mode solver_min_domain(in(any), out) is det.

:- impure pred solver_max_domain(fd_var, int).
:-        mode solver_max_domain(in(any), out) is det.

%-----------------------------------------------------------------------------%

% Implementing the following is left as an exercise for the reader...
:- external(init_any/1).
:- external(solver_min_domain/2).
:- external(solver_max_domain/2).
:- external((==)/2).
:- external((>)/2).
:- external((<)/2).

%-----------------------------------------------------------------------------%
