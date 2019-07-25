%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Regression test for bug #214.
% The error message produced for this program is:
%
% bug.m:026: In predicate `solve_mip_cg'/2:
% bug.m:026:   type error: unsatisfied typeclass constraint:
% bug.m:026:     `bug.linear_expr(int, V_15, int)'
% bug.m:026:   The constraint is due to:
%
% But no constraints are listed.
% The actual problem here is that the type of the 4th argument of the
% closure mip_search is incorrect.
% (XXX as of rotd-2016-06-29, the compiler doesn't print the last
% line of the error message -- ideally it would produce something that
% would allow the user to localise the cause of the unsatisfied constraint.)
%
% Compile with: mmc --allow-stubs --no-warn-stubs --make bug

:- module bug214.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module array2d.
:- import_module float.
:- import_module int.
:- import_module list.

%---------------------------------------------------------------------------%

main(!IO).

:- pred solve_mip_cg(ss::mdi, ss::muo) is semidet.

solve_mip_cg(!SS) :-
    invoke(create_colgen_dw_solver, {CgSolver, _K, OrigVars}, !SS),

    % The next two lines are incorrect.
    OrigVarsL = condense(lists(OrigVars)),
    list.map((func(E) = v(E)), OrigVarsL) = OrigVarsV,

    % They should actually be replaced by ...
    % OrigVarsV = condense(lists(OrigVars)),

    new_assertion_ref(0, NodeCounter, !SS),
    Search = mip_search(CgSolver, most_frac, std_split, OrigVarsV, NodeCounter),
    Check = mip_check_cutoff(CgSolver, integer_granularity),
    branch_and_bound(Search, Check, _Optimal, !SS).

:- pred create_colgen_dw_solver
    : propagate({colgen_dw_solver, int, array2d(int)})
    `with_inst` propagate_semidet.

%---------------------------------------------------------------------------%

:- type ss ---> ss.

:- type solve(T)            == pred(T, ss, ss).
:- inst solve_det           == (pred(out, mdi, muo) is det).
:- inst solve_semidet       == (pred(out, mdi, muo) is semidet).
:- inst solve_nondet        == (pred(out, mdi, muo) is nondet).

:- type solve0 == pred(ss, ss).
:- inst solve0_nondet == (pred(mdi, muo) is nondet).

:- type ps ---> ps.

:- type propagate(T)        == pred(T, ps, ps).
:- inst propagate_det       == (pred(out, di, uo) is det).
:- inst propagate_semidet   == (pred(out, di, uo) is semidet).

:- type propagate0           == pred(ps, ps).
:- inst propagate0_det       == (pred(di, uo) is det).
:- inst propagate0_semidet   == (pred(di, uo) is semidet).

%---------------------------------------------------------------------------%

:- pred invoke(propagate(T)) : solve(T).
:- mode invoke(in(propagate_det)) `with_inst` solve_semidet.
:- mode invoke(in(propagate_semidet)) `with_inst` solve_semidet.

%---------------------------------------------------------------------------%

:- type assertion_ref(T) ---> assertion_ref(T).

:- pred new_assertion_ref(T::in, assertion_ref(T)::out, ss::mdi, ss::uo) is det.

:- typeclass linear_expr(Expr, Coeff, Var) <= ((Expr -> Var), (Expr -> Coeff))
where [
    func v(Var) = Expr
].

:- typeclass lp_solver(S) where [].

:- type colgen_dw_solver ---> colgen_dw_solver.

:- instance lp_solver(colgen_dw_solver) where [].

:- type col_num == int.

%---------------------------------------------------------------------------%

:- pred branch_and_bound(bb_search(T)::in(bb_search),
    bb_check(T)::in(bb_check), T::out, ss::mdi, ss::muo) is semidet.

:- type bb_search(T) == solve(T).
:- inst bb_search == solve_nondet.

:- type bb_check(T) == (pred(T, ps, ps)).
:- inst bb_check == (pred(in, di, uo) is semidet).

:- func integer_granularity = float.

:- type lp_nodes_ref == assertion_ref(int).

:- type lp_split(S) == (pred(lp_nodes_ref, S, col_num, ss, ss)).
:- inst lp_split    == (pred(in, in, in, mdi, muo) is nondet).

:- type lp_choose_var(S)    == (pred(S, list(col_num), col_num, ss, ss)).
:- inst lp_choose_var       == (pred(in, in, out, mdi, muo) is semidet).

:- type ip_solution
    --->    ip_solution(
                ip_objective    :: float,
                ip_var_values   :: list(float)
            ).

:- pred mip_search(S::in, lp_choose_var(S)::in(lp_choose_var),
    lp_split(S)::in(lp_split), list(col_num)::in, assertion_ref(int)::in)
    : bb_search(ip_solution) `with_inst` bb_search <= lp_solver(S).

:- pred mip_check_cutoff(S::in, float::in, ip_solution::in)
    : propagate0 `with_inst` propagate0_semidet <= lp_solver(S).

:- pred std_split(assertion_ref(int)::in, S::in, col_num::in)
    : solve0 `with_inst` solve0_nondet <= lp_solver(S).

:- pred most_frac(S::in, list(col_num)::in)
    : solve(col_num) `with_inst` solve_semidet <= lp_solver(S).

%---------------------------------------------------------------------------%
