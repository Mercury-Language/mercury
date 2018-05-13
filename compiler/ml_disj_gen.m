%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_disj_gen.m.
% Authors: fjh, zs.
%
% Generate MLDS code for disjunctions.
%
%---------------------------------------------------------------------------%
%
% Code for empty disjunctions (`fail')
%
%
%   model_semi goal:
%       <succeeded = fail>
%   ===>
%       succeeded = MR_FALSE;
%
%   model_non goal:
%       <fail && CONT()>
%   ===>
%       /* fall through */
%
%---------------------------------------------------------------------------%
%
% Code for non-empty disjunctions
%
%
% model_det disj:
%
%   model_det Goal:
%       <do (Goal ; Goals)>
%   ===>
%       <do Goal>
%       /* <Goals> will never be reached */
%
%   model_semi Goal:
%       <do (Goal ; Goals)>
%   ===>
%       MR_bool succeeded;
%
%       <succeeded = Goal>;
%       if (!succeeded) {
%           <do Goals>;
%       }
%
% model_semi disj:
%
%   model_det Goal:
%       <succeeded = (Goal ; Goals)>
%   ===>
%       MR_bool succeeded;
%
%       <do Goal>
%       succeeded = MR_TRUE
%       /* <Goals> will never be reached */
%
%   model_semi Goal:
%       <succeeded = (Goal ; Goals)>
%   ===>
%       MR_bool succeeded;
%
%       <succeeded = Goal>;
%       if (!succeeded) {
%           <succeeded = Goals>;
%       }
%
% model_non disj:
%
%   model_det Goal:
%       <(Goal ; Goals) && SUCCEED()>
%   ===>
%       <Goal>
%       SUCCEED();
%       <Goals && SUCCEED()>
%
%   model_semi Goal:
%       <(Goal ; Goals) && SUCCEED()>
%   ===>
%       MR_bool succeeded;
%
%       <succeeded = Goal>
%       if (succeeded) SUCCEED();
%       <Goals && SUCCEED()>
%
%   model_non Goal:
%       <(Goal ; Goals) && SUCCEED()>
%   ===>
%       <Goal && SUCCEED()>
%       <Goals && SUCCEED()>
%
% The implementation of lookup disjunctions is modelled on the code in
% disj_gen.m that does the same thing for the LLDS backend. Any changes here
% may need to be reflected there as well.
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_disj_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ml_backend.mlds.
:- import_module ml_backend.ml_gen_info.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

:- pred ml_gen_disj(list(hlds_goal)::in, hlds_goal_info::in, code_model::in,
    prog_context::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_module.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module maybe.
:- import_module require.

ml_gen_disj(Disjuncts, GoalInfo, CodeModel, Context, Stmts, !Info) :-
    % Note that we place each arm of the disjunction into a block. This is so
    % that we can avoid having to figure out how to merge their declarations
    % with the declarations of the other disjuncts.
    (
        Disjuncts = [],
        % Handle empty disjunctions (a.ka. `fail').
        ml_gen_failure(CodeModel, Context, Stmts, !Info)
    ;
        Disjuncts = [FirstDisjunct | LaterDisjuncts],
        (
            LaterDisjuncts = [],
            unexpected($pred, "single disjunct")
        ;
            LaterDisjuncts = [_ | _],
            (
                CodeModel = model_non,
                ( if
                    ml_gen_info_get_target(!.Info, Target),
                    allow_lookup_disj(Target) = yes,

                    ml_gen_info_get_module_info(!.Info, ModuleInfo),
                    module_info_get_globals(ModuleInfo, Globals),
                    globals.lookup_bool_option(Globals, static_ground_cells,
                        StaticGroundCells),
                    StaticGroundCells = yes,

                    DisjNonLocals = goal_info_get_nonlocals(GoalInfo),
                    all_disjuncts_are_conj_of_unify(DisjNonLocals, Disjuncts)
                then
                    % Since the MLDS backend implements trailing by a
                    % HLDS-to-HLDS transform (which is in add_trail_ops.m),
                    % if we get here, then trailing is not enabled, and we do
                    % not have to worry about resetting the trail at the
                    % starts of all non-first disjuncts.
                    NonLocals = goal_info_get_nonlocals(GoalInfo),
                    OutVars = set_of_var.to_sorted_list(NonLocals),
                    list.map_foldl(ml_generate_constants_for_arm(OutVars),
                        Disjuncts, Solns, !Info),
                    ml_gen_lookup_disj(OutVars, Solns, Context, Stmts, !Info)
                else
                    ml_gen_ordinary_model_non_disj(FirstDisjunct,
                        LaterDisjuncts, Context, Stmts, !Info)
                )
            ;
                ( CodeModel = model_det
                ; CodeModel = model_semi
                ),
                ml_gen_ordinary_model_det_semi_disj(FirstDisjunct,
                    LaterDisjuncts, CodeModel, Context, Stmts, !Info)
            )
        )
    ).

    % Disable generation of lookup disjunctions on some backends.
    % ml_generate_constants_for_arm expects the mark_static_terms pass to have
    % been run, which is not true when static_ground_cells is disabled.
    %
:- func allow_lookup_disj(mlds_target_lang) = bool.

allow_lookup_disj(ml_target_c) = yes.
allow_lookup_disj(ml_target_csharp) = yes.
allow_lookup_disj(ml_target_java) = yes.

%---------------------------------------------------------------------------%

:- pred ml_gen_ordinary_model_det_semi_disj(hlds_goal::in, list(hlds_goal)::in,
    code_model::in, prog_context::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_ordinary_model_det_semi_disj(FirstDisjunct, LaterDisjuncts, CodeModel,
        Context, Stmts, !Info) :-
    (
        LaterDisjuncts = [],
        ml_gen_goal_as_branch_block(CodeModel, FirstDisjunct, Stmt, !Info),
        Stmts = [Stmt]
    ;
        LaterDisjuncts = [FirstLaterDisjunct | LaterLaterDisjuncts],
        % model_det/model_semi disj:
        %
        %   model_det goal:
        %       <Goal ; Goals>
        %   ===>
        %       <Goal>
        %       /* <Goals> will never be reached */
        %
        %   model_semi goal:
        %       <Goal ; Goals>
        %   ===>
        %   {
        %       MR_bool succeeded;
        %
        %       <succeeded = Goal>;
        %       if (!succeeded) {
        %           <Goals>;
        %       }
        %   }

        FirstDisjunct = hlds_goal(_, FirstGoalInfo),
        FirstCodeModel = goal_info_get_code_model(FirstGoalInfo),
        (
            FirstCodeModel = model_det,
            ml_gen_goal_as_branch_block(model_det, FirstDisjunct, Stmt, !Info),
            Stmts = [Stmt]
        ;
            FirstCodeModel = model_semi,
            ml_gen_goal_as_branch_block(model_semi, FirstDisjunct, FirstStmt,
                !Info),
            ml_gen_test_success(Succeeded, !Info),
            ml_gen_ordinary_model_det_semi_disj(FirstLaterDisjunct,
                LaterLaterDisjuncts, CodeModel, Context, LaterStmts, !Info),
            LaterStmt = ml_gen_block([], [], LaterStmts, Context),
            IfStmt = ml_stmt_if_then_else(ml_unop(logical_not, Succeeded),
                LaterStmt, no, Context),
            Stmts = [FirstStmt, IfStmt]
        ;
            FirstCodeModel = model_non,
            % simplify.m should get wrap commits around these.
            unexpected($pred,
                "model_non disjunct in model_det or model_semi disjunction")
        )
    ).

%---------------------------------------------------------------------------%

:- pred ml_gen_ordinary_model_non_disj(hlds_goal::in, list(hlds_goal)::in,
    prog_context::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_ordinary_model_non_disj(FirstDisjunct, LaterDisjuncts, Context,
        Stmts, !Info) :-
    (
        LaterDisjuncts = [],
        ml_gen_goal_as_branch_block(model_non, FirstDisjunct, Stmt, !Info),
        Stmts = [Stmt]
    ;
        LaterDisjuncts = [FirstLaterDisjunct | LaterLaterDisjuncts],
        % model_non disj:
        %
        %       <(Goal ; Goals) && SUCCEED()>
        %   ===>
        %       <Goal && SUCCEED()>
        %       <Goals && SUCCEED()>

        ml_gen_goal_as_branch_block(model_non, FirstDisjunct, FirstStmt,
            !Info),
        ml_gen_ordinary_model_non_disj(FirstLaterDisjunct, LaterLaterDisjuncts,
            Context, LaterStmts, !Info),
        Stmts = [FirstStmt | LaterStmts]
    ).

%---------------------------------------------------------------------------%

:- pred ml_gen_lookup_disj(list(prog_var)::in, list(list(mlds_rval))::in,
    prog_context::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_lookup_disj(OutVars, Solns, Context, Stmts, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_gen_info_get_target(!.Info, Target),

    ml_gen_info_get_var_types(!.Info, VarTypes),
    lookup_var_types(VarTypes, OutVars, FieldTypes),
    MLDS_FieldTypes =
        list.map(mercury_type_to_mlds_type(ModuleInfo), FieldTypes),

    ml_gen_info_get_global_data(!.Info, GlobalData0),
    ml_gen_static_vector_type(MLDS_ModuleName, Context, Target,
        MLDS_FieldTypes, StructTypeNum, StructType, FieldIds,
        GlobalData0, GlobalData1),

    ml_construct_disjunction_vector(ModuleInfo, StructType, Solns,
        RowInitializers),
    list.length(RowInitializers, NumRows),

    ml_gen_static_vector_defn(MLDS_ModuleName, StructTypeNum, RowInitializers,
        VectorCommon, GlobalData1, GlobalData),
    ml_gen_info_set_global_data(GlobalData, !Info),

    ml_gen_info_new_aux_var_name(mcav_slot, SlotVar, !Info),
    SlotVarType = mlds_native_int_type,
    % We never need to trace ints.
    SlotVarGCStmt = gc_no_stmt,
    SlotVarDefn = ml_gen_mlds_var_decl(SlotVar, SlotVarType,
        SlotVarGCStmt, Context),
    SlotVarLval = ml_local_var(SlotVar, SlotVarType),
    SlotVarRval = ml_lval(SlotVarLval),

    ml_generate_field_assigns(OutVars, MLDS_FieldTypes, FieldIds,
        VectorCommon, StructType, SlotVarRval, Context, LookupStmts, !Info),
    ml_gen_call_current_success_cont(Context, CallContStmt, !Info),

    InitSlotVarStmt = ml_stmt_atomic(
        assign(SlotVarLval, ml_const(mlconst_int(0))),
        Context),

    IncrSlotVarStmt = ml_stmt_atomic(
        assign(SlotVarLval,
            ml_binop(int_add(int_type_int), SlotVarRval,
                ml_const(mlconst_int(1)))),
        Context),

    LoopBodyStmt = ml_stmt_block([], [],
        LookupStmts ++ [CallContStmt, IncrSlotVarStmt], Context),

    LoopCond = ml_binop(int_lt(int_type_int), SlotVarRval,
        ml_const(mlconst_int(NumRows))),
    LoopStmt = ml_stmt_while(loop_at_least_once, LoopCond, LoopBodyStmt,
        [SlotVar], Context),

    Stmt = ml_stmt_block([SlotVarDefn], [],
        [InitSlotVarStmt, LoopStmt], Context),
    Stmts = [Stmt].

:- pred ml_construct_disjunction_vector(module_info::in, mlds_type::in,
    list(list(mlds_rval))::in, list(mlds_initializer)::out) is det.

ml_construct_disjunction_vector(_ModuleInfo, _StructType, [], []).
ml_construct_disjunction_vector(ModuleInfo, StructType,
        [Soln | Solns], [RowInitializer | RowInitializers]) :-
    FieldInitializers = list.map(wrap_init_obj, Soln),
    RowInitializer = init_struct(StructType, FieldInitializers),
    ml_construct_disjunction_vector(ModuleInfo, StructType,
        Solns, RowInitializers).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_disj_gen.
%---------------------------------------------------------------------------%
