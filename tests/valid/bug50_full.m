%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is the original version of the test case for Mantis bug 50.
% The file bug50.m contains a minimized version of this test case,
% as well as a description of the bug itself and its fix.
%
% This file is here so that we can ensure that the fix works for the
% full test case, not just the cut-down version.
%
%---------------------------------------------------------------------------%

:- module bug50_full.
:- interface.

:- import_module map.
:- import_module maybe.
:- import_module list.

:- type maybe_lower_bound(T)
    --->    no_lb
    ;       lb(T).

:- type maybe_upper_bound(T)
    --->    no_ub
    ;       ub(T).

:- type solver_annotations(Var) == list(solver_annotation(Var)).

:- type solver_annotation(Var)
    --->    solver_annotation(string, list(Var)).

:- typeclass flatzinc_solver(Solver, Var) <= (Solver -> Var) where [
    pred new_float_var(Solver::in,
        maybe_lower_bound(float)::in,
        maybe_upper_bound(float)::in,
        solver_annotations(Var)::ia, Var::oa) is det
].

:- type colgen_dw_solver
    --->    colgen_dw_solver.

:- type colgen_dw_var
    --->    colgen_dw_var.

:- type flatzinc_colgen_solver 
    --->    some [Solver, Var]
            flatzinc_colgen_solver(
                fcb_colgen_dw_solver :: colgen_dw_solver,
                fcb_sp_solvers       :: bt_ref(map(int,
                                            {Solver,
                                             maybe(int),
                                             map(flatzinc_colgen_var, Var),
                                             map(int, flatzinc_colgen_var)
                                             })),
                fcb_colgen_var_map   :: bt_ref(map(flatzinc_colgen_var,
                                            colgen_dw_var))
            ) => ( flatzinc_solver(Solver, Var) ).               

:- type flatzinc_colgen_var
    --->    colgen_var(int)
    ;       colgen_master_var(int)
    ;       sp_var(int, int, flatzinc_type).

:- type flatzinc_type
    --->    scalar(flatzinc_scalar_type)
    ;       array(int, flatzinc_scalar_type).

:- type flatzinc_scalar_type
    --->    flatzinc_bool
    ;       flatzinc_float(maybe_lower_bound(float), maybe_upper_bound(float)).

:- pred fcb_new_float_var(flatzinc_colgen_solver::in,
    maybe_lower_bound(float)::in,
    maybe_upper_bound(float)::in,
    solver_annotations(flatzinc_colgen_var)::ia,
    flatzinc_colgen_var::oa) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.

:- typeclass linear_solver(Solver, Var) <=  ( (Solver -> Var) ) where [
    pred new_var(Solver::in, Var::oa) is det
].

:- type var_ann
    --->    var_ann_colgen_master_var
    ;       var_ann_colgen_sp_var(int, string).

:- instance linear_solver(colgen_dw_solver, colgen_dw_var) where [
    (new_var(_, _) :- throw(""))
].

%---------------------------------------------------------------------------%

:- type sp_solver_instance 
    --->    some[Solver, Var]
            sp_solver_instance(
                spsi_solver   :: Solver,
                spsi_var_map  :: map(flatzinc_colgen_var, Var)
            ) => flatzinc_solver(Solver, Var).

%---------------------------------------------------------------------------%

fcb_new_float_var(FCBackend, MaybeLo, MaybeHi, Anns, FCVar) :-
    promise_pure (
        ( if fcb_extract_var_ann(Anns, VarAnn) then
            (
                VarAnn = var_ann_colgen_master_var,
                promise_pure (
                    ColgenSolver = FCBackend ^ fcb_colgen_dw_solver,
                    new_var(ColgenSolver, Var),
                    semipure Vars0 = get(FCBackend ^ fcb_colgen_var_map),
                    VarID = 1,
                    FCVar = colgen_master_var(VarID),
                    det_insert(FCVar, Var, Vars0, Vars),
                    impure set(FCBackend ^ fcb_colgen_var_map, Vars)
                )
            ;
                VarAnn = var_ann_colgen_sp_var(SPID, SPSolverName),
                FCBackend = flatzinc_colgen_solver(_, SPSolversRef, _),
                FZN_Type = scalar(flatzinc_float(MaybeLo, MaybeHi)),
                VarCreator = new_float_var_wrapper(MaybeLo, MaybeHi),
                fcb_new_var(SPSolversRef, SPID, SPSolverName,
                    FZN_Type, Anns, VarCreator, FCVar)
            )
        else
            throw("")
        )
    ).

%---------------------------------------------------------------------------%

:- pred fcb_extract_var_ann(solver_annotations(flatzinc_colgen_var)::ia,
    var_ann::oa) is semidet.

fcb_extract_var_ann([_A | _Anns], VarAnn) :-
    VarAnn = var_ann_colgen_master_var.

%---------------------------------------------------------------------------%

:- pred new_float_var_wrapper(
    maybe_lower_bound(float)::in, maybe_upper_bound(float)::in,
    Solver::in, solver_annotations(Var)::ia, Var::oa) is det
    <= flatzinc_solver(Solver, Var).

new_float_var_wrapper(MaybeLo, MaybeHi, Solver, Anns, Var) :-
    new_float_var(Solver, MaybeLo, MaybeHi, Anns, Var).

:- pred fcb_new_var(
    bt_ref(map(int,
        {Solver, maybe(int), map(flatzinc_colgen_var, Var),
        map(int, flatzinc_colgen_var)}))::in,
    int::in, string::in, flatzinc_type::in,
    solver_annotations(flatzinc_colgen_var)::ia, 
    pred(Solver, solver_annotations(Var), Var)::in(pred(in, ia, oa) is det),
    flatzinc_colgen_var::oa) is det <= flatzinc_solver(Solver, Var).

fcb_new_var(_SPSolversRef, SPID, _SPSolverName, FznType, _Anns, _NewVarPred,
        FCVar) :-
    VarID = 0,
    FCVar = sp_var(SPID, VarID, FznType).

%---------------------------------------------------------------------------%

:- pred fcb_anns_to_sp_anns(Solver::in, map(flatzinc_colgen_var, Var)::in,
   solver_annotations(flatzinc_colgen_var)::ia, solver_annotations(Var)::oa)
   is det <= flatzinc_solver(Solver, Var).

fcb_anns_to_sp_anns(_, _, _, []).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- interface.

:- type bt_ref(T).

:- impure func new(T::in(I =< any)) = (bt_ref(T)::out(I =< any)) is det.

:- impure pred set(bt_ref(T)::in(I =< any), T::in(I =< any)) is det.

:- semipure func get(bt_ref(T)::in(I =< any)) = (T::out(I =< any)) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("C", bt_ref(T), "MR_Word *", [can_pass_as_mercury_type]).

:- pragma foreign_proc("C",
    new(X::in(I =< any)) = (R::out(I =< any)),
    [will_not_call_mercury, will_not_modify_trail],
"
    R = MR_GC_NEW(MR_Word);
    *R = X;
").

:- pragma foreign_proc("C",
    set(R::in(I =< any), X::in(I =< any)),
    [will_not_call_mercury],
"
    /*MR_trail_current_value(R);*/
    *R = X;
").

:- pragma foreign_proc("C",
    get(R::in(I =< any)) = (X::out(I =< any)),
    [will_not_call_mercury, promise_semipure, will_not_modify_trail],
"
    X = *R;
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
