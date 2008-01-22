% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
% rotd-2007-11-27 emits the following error message for this program:
%
% equiv_solns_ia.m:051: Error: the `promise_equivalent_solutions' goal lists
% equiv_solns_ia.m:051:   some extra variables: V_3, V_4 and V_5.
% Uncaught Mercury exception:
% Software Error: pd_util.m: Unexpected: rerun_det_analysis: determinism errors
%
% Compile with: -C -O0 --inlining --local-constraint-propagation
%
% The problem was caused by the fact that inst any non-locals in a promise
% equivalent_solutions scope must be listed in the scope head but optimizations
% such as inlining can cause the inst of these non-locals to become ground.
% Since only variables that become further bound/constrained should be listed
% in the scope head this means that determinism analysis reruns after
% inlining reported extra variables in the scope head.
% 
:- module equiv_solns_ia.
:- interface.

:- import_module list.

:- typeclass flatzinc_solver(Solver, Var) <= (Solver -> Var) where [

    pred solve_for_vars(Solver::in, solve_spec(Var)::ia,
        solver_annotations(Var)::ia, list(Var)::ia) is nondet
].

:- type solve_spec(Var)
    --->    satisfy
    ;       minimize(Var)
    ;       maximize(Var).

:- type solver_annotations(Var) == list(solver_annotation(Var)).

:- type solver_annotation(Var)
    --->    solver_annotation(list(Var)).

:- type env(Var) == list({int, Var}).

:- implementation.
:- import_module exception.

:- pred interpret(Solver::in) is det <= flatzinc_solver(Solver, Var).

interpret(Solver) :-
    interpret_2(Solver, satisfy, [], []).

:- pragma inline(interpret_2/4).
:- pred interpret_2(Solver::in, solve_spec(Var)::ia,
    solver_annotations(Var)::ia,
    list(Var)::ia) is det <= flatzinc_solver(Solver, Var).

interpret_2(Solver, SolveSpec, SolverAnns, AllVars) :-
    promise_pure
    ( if
        promise_equivalent_solutions [SolveSpec, SolverAnns, AllVars] (
            solve_for_vars(Solver, SolveSpec, SolverAnns, AllVars)
        )
      then
        true
      else
        throw("")
    ).
