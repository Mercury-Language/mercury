%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This program causes rotd-2008-03-03 to abort in debug grades with the
% following uncaught Mercury exception from find_typeinfos_for_tvars in
% continuation_info.m:
%
%   can't find rval for type_info var V_<nn>
%
% when compiling the "crash" predicate.
%
% To reproduce compile with:
%
%   $ mmc -C --grade asm_fast.gc.debug 
%
% (This test case is derived from g12/zinc/compiler/flatzinc_colgen_solver.m
% in r4693 of G12.)
%
%---------------------------------------------------------------------------%
% 
% The compiler crashed when compiling the call to crash_location (pred_id 2)
% in the body of crash (pred_id 0).
% 
% The crash came when the compiler is trying to record the type of
% SpSolversRep_9 in the RTTI. It was trying to record this because
% SpSolversRep_9 is a named variable. Even though it is not live after
% the call, the user may wish to inspect its value on the call stack
% when he/she stops the program execution somewhere iside the call tree
% of the call. This is why the delay_death pass of liveness.m prolongs
% the life of SpSolversRep_9 beyond the call.
% 
% SpSolversRep_9's type is tree234(int, Solver_1), so the compiler needs
% to know where the type_info for the type represented by type variable
% Solver_1 is.
% 
% The rtti varmaps records it as being in slot 1 of the typeclass_info
% variable V_20, which is indeed its location.
% 
% The problem was that V_20 is the post-death set of the call, and since
% calls are atomic goals, the variables in the post-death set are deleted
% from the code generator state BEFORE we generate code for the call.
% (This is done to avoid the storing them on the stack across the call.)
% 
% There were two reasons why V_20 died in the post-death set of the call,
% and not into the post-death set of the switch.
% 
% First, FCBackend is nonlocal to the switch, and thus it dies in the
% post-death set of the switch. However, while V_20 is needed to describe
% the type of the ARGUMENTS of FCBackend, it is not needed to describe
% the type of FCBackend itself, since that type is just flatzinc_colgen_solver.
% Therefore the initial_deadness predicate in liveness.m does NOT believe
% that V_20 is needed outside the switch.
% 
% Second, the delay_death pass of liveness.m COULD have delayed the death
% of V_20, moving it from the post-death set of the call to the post-death
% set of the switch, but it didn't, because V_20 did not have a name.
% The reason it didn't have a name was that V_20 was created by cse_detection.m
% when it hoisted desconstructions of FCBackend out of both arms of the switch.
% Even though polymorphism.m gave names to the field variables inside FCBackend
% containing the typeclass info in both arms of the switch, cse_detection put
% an UNNAMED variable into the deconstruction of FCBackend that it hoisted
% out of the switch. (This is why removing the deconstruction of FCBackend
% in the var_ann_ok arm used to eliminate the crash.) This unnamed variable
% (V_20) was later unified with the original named variable (which was
% TypeClassInfo_for_flatzinc_solver_14). The typeclass info needed to describe
% the type of SpSolversRep_9 was thus available in both
% 
% - a named variable (TypeClassInfo_for_flatzinc_solver_14) that was local
%   to the switch arm, and
% - an unnamed variable (V_20) that was visible outside (before) the switch,
% 
% but it was not recorded as being in a NAMED variable that was visible OUTSIDE
% the switch. Without the second condition being fulfilled, the delay_death
% pass COULD not delay the death of V_20, and without the first, it CHOSE
% not to delay it.
%
% Making delay_death delay the death of all variables, even the unnamed ones,
% would be a bad idea, since there are a LOT of them. Making it delay the death
% of unnamed variables only if they contain typeinfos or typeclass_infos
% would have a smaller impact on the size and speed of the generate code,
% but would probably confuse debugger users being presented with those
% unnamed variables.
% 
% The better fix is to have cse_detection copy the names (if any) of the
% field variables containing type_infos and typeclass_infos from the original
% deconstruction unification in the first switch arm it processes
% to the new field variables it creates for the hoisted-out deconstruction.
% Since polymorphism always names typeinfo and typeclass info variables,
% with this fix, if delay_death preserves the value of a named variable
% past a call, it will also preserve the values of the typeinfo and typeclass
% info variables needed to make sense of it.
%
%---------------------------------------------------------------------------%

:- module bug50.
:- interface.

:- import_module map.
:- import_module list.

:- type solver_annotation(Var)
    --->    solver_annotation(string, list(Var)).

:- typeclass flatzinc_solver(Solver, Var) <= (Solver -> Var) where [
    pred new_float_var(Solver::in,
        solver_annotation(Var)::ia, Var::oa) is det
].

:- type dummy
    --->    dummy.

:- type flatzinc_colgen_solver 
    --->    some [Solver, Var]
            flatzinc_colgen_solver(
                fcb_colgen_dw_solver :: dummy,
                fcb_sp_solvers       :: map(int, Solver)
            ) => ( flatzinc_solver(Solver, Var) ).               

:- type flatzinc_colgen_var
    --->    colgen_var(int)
    ;       colgen_master_var
    ;       sp_var(int).

:- type var_ann
    --->    var_ann_ok
    ;       var_ann_buggy.

:- pred crash(flatzinc_colgen_solver::in, var_ann::ia,
    flatzinc_colgen_var::oa) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.

%---------------------------------------------------------------------------%

:- type sp_solver_instance 
    --->    some [Solver, Var]
            sp_solver_instance(
                spsi_solver   :: Solver,
                spsi_var_map  :: map(flatzinc_colgen_var, Var)
            ) => flatzinc_solver(Solver, Var).

%---------------------------------------------------------------------------%

crash(FCBackend, VarAnn, FCVar) :-
    (
        VarAnn = var_ann_ok,
        % Commenting out the assignment to _Dummy makes the compiler abort
        % go away.
        FCBackend = flatzinc_colgen_solver(_Dummy, _SPSolversRef),
        % _Dummy = _Y,
        FCVar = colgen_master_var
    ;
        VarAnn = var_ann_buggy,
        FCBackend = flatzinc_colgen_solver(_Dummy, SPSolversRef),
        VarCreator = new_float_var_wrapper,
        crash_location(SPSolversRef, VarCreator, FCVar)
    ).

%---------------------------------------------------------------------------%

:- pred new_float_var_wrapper(
    Solver::in, solver_annotation(Var)::ia, Var::oa) is det
    <= flatzinc_solver(Solver, Var).

:- pragma external_pred(new_float_var_wrapper/3).

:- pred crash_location(map(int, Solver)::in,
    pred(Solver, solver_annotation(Var), Var)::in(pred(in, ia, oa) is det),
    flatzinc_colgen_var::oa) is det <= flatzinc_solver(Solver, Var).

:- pragma external_pred(crash_location/3).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
