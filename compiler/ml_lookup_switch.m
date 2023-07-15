%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% Copyright (C) 2014-2018, 2020-2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_lookup_switch.m
% Author: zs.
%
% This module implements lookup switches for the MLDS backend.
% Much of its structure is modelled after the structure of lookup_switch.m,
% which does the same thing for the LLDS backend. Most of the documentation
% you may need is in that module.
%
% Any changes here may need to be reflected in lookup_switch.m as well.
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_lookup_switch.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.switch_util.
:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module unit.

%---------------------------------------------------------------------------%

:- type ml_lookup_switch_info
    --->    ml_lookup_switch_info(
                % The map from the case_id of each switch arm to the
                % values of the variables in that switch arm.
                mllsi_cases             ::  case_consts(case_id, mlds_rval,
                                                unit),

                % The output variables, which become (some of) the fields
                % in each row of a lookup table.
                mllsi_out_variables     ::  list(prog_var),

                % The types of the fields holding output variables.
                mllsi_out_types         ::  list(mlds_type)
            ).

    % Is the given list of cases implementable as a lookup switch?
    %
:- pred ml_is_lookup_switch(prog_var::in, list(tagged_case)::in,
    hlds_goal_info::in, code_model::in, maybe(ml_lookup_switch_info)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % Given a case_id->V map, create the corresponding cons_id->V map.
    % Given an entry caseid1->values1 in the case_id->V map, if the case
    % with case_id caseid1 has main and other cons_ids consid1a, consid1b
    % and consid1c, the cons_id->V map will have the corresponding entries
    % consid1a->values1, consid1b->values1 and consid1c->values1.
    %
:- pred ml_case_id_soln_consts_to_tag_soln_consts(
    pred(cons_tag, T)::in(pred(in, out) is det), list(tagged_case)::in,
    map(case_id, V)::in, map(T, V)::out) is det.

%---------------------------------------------------------------------------%

    % Generate MLDS code for the lookup switch.
    %
:- pred ml_gen_int_max_32_lookup_switch(prog_var::in,
    list(tagged_case)::in, ml_lookup_switch_info::in,
    code_model::in, prog_context::in, int::in, int::in,
    need_bit_vec_check::in, need_range_check::in, mlds_stmt::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%
% These types and predicates are exported because they are useful in the
% implementation of string lookup switches.
%

:- pred ml_gen_several_soln_lookup_code(prog_context::in,
    mlds_rval::in, list(prog_var)::in, list(mlds_type)::in,
    mlds_type::in, mlds_type::in, mlds_field_id::in, mlds_field_id::in,
    list(mlds_field_id)::in, list(mlds_field_id)::in,
    mlds_vector_common::in, mlds_vector_common::in, need_bit_vec_check::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

:- type ml_several_soln_lookup_vars
    --->    ml_several_soln_lookup_vars(
                msslv_num_later_solns_var       :: mlds_local_var_name_type,
                msslv_later_slot_var            :: mlds_local_var_name_type,
                msslv_limit_var                 :: mlds_local_var_name_type,
                msslv_limit_assign_statement    :: mlds_stmt,
                msslv_incr_later_slot_statement :: mlds_stmt,
                msslv_denfs                     :: list(mlds_local_var_defn)
            ).

:- pred make_several_soln_lookup_vars(prog_context::in,
    ml_several_soln_lookup_vars::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%

:- func ml_construct_later_soln_row(mlds_type, list(mlds_rval)) =
    mlds_initializer.

:- func ml_default_value_for_type(mlds_type) = mlds_rval.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module uint.

%---------------------------------------------------------------------------%

ml_is_lookup_switch(SwitchVar, TaggedCases, GoalInfo, CodeModel,
        MaybeLookupSwitchInfo, !Info) :-
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    % SwitchVar must be nonlocal to the switch, since it must be bound
    % *before* the switch.
    set_of_var.delete(SwitchVar, NonLocals, OtherNonLocals),
    set_of_var.to_sorted_list(OtherNonLocals, OutVars),
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    ( if
        OptTuple ^ ot_use_static_ground_cells = use_static_ground_cells,
        ml_generate_constants_for_lookup_switch(CodeModel, OutVars,
            OtherNonLocals, TaggedCases, map.init, CaseSolnMap, !Info)
    then
        % While the LLDS backend has to worry about implementing trailing
        % for model_non lookup switches, we do not. The MLDS backend implements
        % trailing by a HLDS-to-HLDS transform (which is in add_trail_ops.m),
        % so we can get here only if trailing is not enabled, since otherwise
        % the calls or foreign_procs inserted into all non-first disjuncts
        % would cause ml_generate_constants_for_lookup_switch to fail.
        ( if project_all_to_one_solution(CaseSolnMap, CaseValuePairMap) then
            CaseConsts = all_one_soln(CaseValuePairMap)
        else
            CaseConsts = some_several_solns(CaseSolnMap, unit)
        ),
        ml_gen_info_get_var_table(!.Info, VarTable),
        lookup_var_entries(VarTable, OutVars, OutVarEntries),
        FieldTypes =
            list.map(var_table_entry_to_mlds_type(ModuleInfo), OutVarEntries),
        LookupSwitchInfo =
            ml_lookup_switch_info(CaseConsts, OutVars, FieldTypes),
        MaybeLookupSwitchInfo = yes(LookupSwitchInfo)
    else
        % We keep the original !.Info.
        MaybeLookupSwitchInfo = no
    ).

:- pred ml_generate_constants_for_lookup_switch(code_model::in,
    list(prog_var)::in, set_of_progvar::in, list(tagged_case)::in,
    map(case_id, soln_consts(mlds_rval))::in,
    map(case_id, soln_consts(mlds_rval))::out,
    ml_gen_info::in, ml_gen_info::out) is semidet.

ml_generate_constants_for_lookup_switch(_CodeModel, _OutVars, _ArmNonLocals,
        [], !IndexMap, !Info).
ml_generate_constants_for_lookup_switch(CodeModel, OutVars, ArmNonLocals,
        [TaggedCase | TaggedCases], !CaseIdMap, !Info) :-
    TaggedCase = tagged_case(_TaggedMainConsId, _TaggedOtherConsIds,
        CaseId, Goal),
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    ( if GoalExpr = disj(Disjuncts) then
        (
            Disjuncts = []
        ;
            Disjuncts = [FirstDisjunct | LaterDisjuncts],
            goal_is_conj_of_unify(ArmNonLocals, FirstDisjunct),
            all_disjuncts_are_conj_of_unify(ArmNonLocals, LaterDisjuncts),
            ml_generate_constants_for_arm(OutVars, FirstDisjunct, FirstSoln,
                !Info),
            ml_generate_constants_for_arms(OutVars, LaterDisjuncts, LaterSolns,
                !Info),
            SolnConsts = several_solns(FirstSoln, LaterSolns),
            map.det_insert(CaseId, SolnConsts, !CaseIdMap)
        )
    else
        goal_is_conj_of_unify(ArmNonLocals, Goal),
        ml_generate_constants_for_arm(OutVars, Goal, Soln, !Info),
        SolnConsts = one_soln(Soln),
        map.det_insert(CaseId, SolnConsts, !CaseIdMap)
    ),
    ml_generate_constants_for_lookup_switch(CodeModel,
        OutVars, ArmNonLocals, TaggedCases, !CaseIdMap, !Info).

%---------------------------------------------------------------------------%

ml_case_id_soln_consts_to_tag_soln_consts(GetTag, TaggedCases, CaseIdMap,
        TagMap) :-
    ml_case_id_soln_consts_to_tag_soln_consts_loop(GetTag, TaggedCases,
        CaseIdMap, DepletedCaseIdMap, map.init, TagMap),
    expect(map.is_empty(DepletedCaseIdMap), $pred,
        "DepletedCaseIdMap not empty").

:- pred ml_case_id_soln_consts_to_tag_soln_consts_loop(
    pred(cons_tag, Key)::in(pred(in, out) is det), list(tagged_case)::in,
    map(case_id, V)::in, map(case_id, V)::out,
    map(Key, V)::in, map(Key, V)::out) is det.

ml_case_id_soln_consts_to_tag_soln_consts_loop(_GetTag, [],
        !CaseIdMap, !TagMap).
ml_case_id_soln_consts_to_tag_soln_consts_loop(GetTag,
        [TaggedCase | TaggedCases], !CaseIdMap, !TagMap) :-
    TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds,
        CaseId, _Goal),
    map.det_remove(CaseId, SolnConsts, !CaseIdMap),
    ml_record_lookup_for_tagged_cons_id(GetTag, SolnConsts,
        TaggedMainConsId, !TagMap),
    list.foldl(ml_record_lookup_for_tagged_cons_id(GetTag, SolnConsts),
        TaggedOtherConsIds, !TagMap),
    ml_case_id_soln_consts_to_tag_soln_consts_loop(GetTag, TaggedCases,
        !CaseIdMap, !TagMap).

:- pred ml_record_lookup_for_tagged_cons_id(
    pred(cons_tag, T)::in(pred(in, out) is det), V::in, tagged_cons_id::in,
    map(T, V)::in, map(T, V)::out) is det.

ml_record_lookup_for_tagged_cons_id(GetTag, SolnConsts, TaggedConsId,
        !IndexMap) :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    GetTag(ConsTag, Index),
    map.det_insert(Index, SolnConsts, !IndexMap).

%---------------------------------------------------------------------------%

ml_gen_int_max_32_lookup_switch(SwitchVar, TaggedCases, LookupSwitchInfo,
        CodeModel, Context, StartVal, EndVal, NeedBitVecCheck, NeedRangeCheck,
        Stmt, !Info) :-
    LookupSwitchInfo =
        ml_lookup_switch_info(CaseIdConstMap, OutVars, FieldTypes),
    ml_gen_var_direct(!.Info, SwitchVar, SwitchVarLval),
    SwitchVarRval = ml_lval(SwitchVarLval),
    ( if StartVal = 0 then
        IndexRval = SwitchVarRval
    else
        StartRval = ml_const(mlconst_int(StartVal)),
        IndexRval = ml_binop(int_sub(int_type_int), SwitchVarRval, StartRval)
    ),
    (
        CaseIdConstMap = all_one_soln(CaseIdValueMap),
        ml_case_id_soln_consts_to_tag_soln_consts(get_int_tag, TaggedCases,
            CaseIdValueMap, IntValueMap),
        map.to_assoc_list(IntValueMap, IntValues),
        ml_gen_simple_atomic_lookup_switch(IndexRval, OutVars, FieldTypes,
            IntValues, CodeModel, Context, StartVal, EndVal,
            NeedBitVecCheck, NeedRangeCheck, Stmt, !Info)
    ;
        CaseIdConstMap = some_several_solns(CaseIdSolnMap, _Unit),
        expect(unify(CodeModel, model_non), $pred, "CodeModel != model_non"),
        ml_case_id_soln_consts_to_tag_soln_consts(get_int_tag, TaggedCases,
            CaseIdSolnMap, IntSolnMap),
        map.to_assoc_list(IntSolnMap, IntSolns),
        ml_gen_several_soln_atomic_lookup_switch(IndexRval, OutVars,
            FieldTypes, IntSolns, Context, StartVal, EndVal,
            NeedBitVecCheck, NeedRangeCheck, Stmt, !Info)
    ).

%---------------------------------------------------------------------------%

:- pred ml_gen_simple_atomic_lookup_switch(mlds_rval::in, list(prog_var)::in,
    list(mlds_type)::in, assoc_list(int, list(mlds_rval))::in, code_model::in,
    prog_context::in, int::in, int::in,
    need_bit_vec_check::in, need_range_check::in, mlds_stmt::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_simple_atomic_lookup_switch(IndexRval, OutVars, OutTypes, CaseValues,
        CodeModel, Context, StartVal, EndVal, NeedBitVecCheck, NeedRangeCheck,
        Stmt, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_gen_info_get_target(!.Info, Target),

    (
        OutTypes = [],
        % There are no output parameters, so there is nothing to look up.
        % Generating a structure with no fields would cause problems for
        % Visual C, which cannot handle such structures.
        %
        % This should happen only for model_semi switches. If it happens for
        % a model_det switch, that switch is effectively a no-op.
        LookupStmts = []
    ;
        OutTypes = [_ | _],
        ml_gen_info_get_global_data(!.Info, GlobalData0),
        ml_gen_static_vector_type(MLDS_ModuleName, Context, Target,
            OutTypes, StructTypeNum, StructType, FieldIds,
            GlobalData0, GlobalData1),

        ml_construct_simple_switch_vector(ModuleInfo, StructType,
            OutTypes, StartVal, CaseValues, RowInitializers),

        ml_gen_static_vector_defn(MLDS_ModuleName, StructTypeNum,
            RowInitializers, VectorCommon, GlobalData1, GlobalData),
        ml_gen_info_set_global_data(GlobalData, !Info),

        ml_generate_field_assigns(OutVars, OutTypes, FieldIds, VectorCommon,
            StructType, IndexRval, Context, LookupStmts, !Info)
    ),

    (
        CodeModel = model_det,
        expect(unify(NeedRangeCheck, dont_need_range_check), $pred,
            "model_det need_range_check"),
        (
            NeedBitVecCheck = need_bit_vec_check,
            unexpected($pred, "model_det need_bit_vec_check")
        ;
            ( NeedBitVecCheck = dont_need_bit_vec_check_no_gaps
            ; NeedBitVecCheck = dont_need_bit_vec_check_with_gaps
            )
        ),
        Stmt = ml_stmt_block([], [], LookupStmts, Context)
    ;
        CodeModel = model_semi,
        ml_gen_set_success(ml_const(mlconst_true), Context, SetSuccessTrueStmt,
            !Info),
        LookupSucceedStmt = ml_stmt_block([], [],
            LookupStmts ++ [SetSuccessTrueStmt], Context),
        (
            NeedRangeCheck = dont_need_range_check,
            (
                ( NeedBitVecCheck = dont_need_bit_vec_check_no_gaps
                ; NeedBitVecCheck = dont_need_bit_vec_check_with_gaps
                ),
                Stmt = LookupSucceedStmt
            ;
                NeedBitVecCheck = need_bit_vec_check,
                ml_generate_bitvec_test(MLDS_ModuleName, Context, IndexRval,
                    CaseValues, StartVal, EndVal, BitVecCheckCond, !Info),

                ml_gen_set_success(ml_const(mlconst_false), Context,
                    SetSuccessFalseStmt, !Info),

                Stmt = ml_stmt_if_then_else(BitVecCheckCond,
                    LookupSucceedStmt, yes(SetSuccessFalseStmt), Context)
            )
        ;
            NeedRangeCheck = need_range_check,
            Difference = EndVal - StartVal,
            RangeCheckCond = ml_binop(unsigned_le, IndexRval,
                ml_const(mlconst_int(Difference))),
            ml_gen_set_success(ml_const(mlconst_false), Context,
                SetSuccessFalseStmt, !Info),
            (
                ( NeedBitVecCheck = dont_need_bit_vec_check_no_gaps
                ; NeedBitVecCheck = dont_need_bit_vec_check_with_gaps
                ),
                RangeCheckSuccessStmt = LookupSucceedStmt
            ;
                NeedBitVecCheck = need_bit_vec_check,
                ml_generate_bitvec_test(MLDS_ModuleName, Context, IndexRval,
                    CaseValues, StartVal, EndVal, BitVecCheckCond, !Info),

                RangeCheckSuccessStmt = ml_stmt_if_then_else(BitVecCheckCond,
                    LookupSucceedStmt, yes(SetSuccessFalseStmt), Context)
            ),

            % We want to execute the bit vector test only if the range check
            % succeeded, since otherwise the bit vector test will probably
            % access the bit vector outside its bounds.
            Stmt = ml_stmt_if_then_else(RangeCheckCond,
                RangeCheckSuccessStmt, yes(SetSuccessFalseStmt), Context)
        )
    ;
        CodeModel = model_non,
        % If all the switch arms have exactly one solution, then the switch
        % as a whole cannot be model_non.
        unexpected($pred, "model_non")
    ).

%---------------------------------------------------------------------------%

:- pred ml_gen_several_soln_atomic_lookup_switch(mlds_rval::in,
    list(prog_var)::in, list(mlds_type)::in,
    assoc_list(int, soln_consts(mlds_rval))::in, prog_context::in,
    int::in, int::in, need_bit_vec_check::in, need_range_check::in,
    mlds_stmt::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_several_soln_atomic_lookup_switch(IndexRval, OutVars, OutTypes,
        CaseSolns, Context, StartVal, EndVal, NeedBitVecCheck, NeedRangeCheck,
        Stmt, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_gen_info_get_target(!.Info, Target),

    MLDS_IntType = mlds_builtin_type_int(int_type_int),
    FirstSolnFieldTypes = [MLDS_IntType, MLDS_IntType | OutTypes],

    ml_gen_info_get_global_data(!.Info, GlobalData0),
    ml_gen_static_vector_type(MLDS_ModuleName, Context, Target,
        FirstSolnFieldTypes, FirstSolnStructTypeNum, FirstSolnStructType,
        FirstSolnTableFieldIds, GlobalData0, GlobalData1),
    ml_gen_static_vector_type(MLDS_ModuleName, Context, Target,
        OutTypes, LaterSolnStructTypeNum, LaterSolnStructType,
        LaterSolnOutFieldIds, GlobalData1, GlobalData2),
    (
        ( FirstSolnTableFieldIds = []
        ; FirstSolnTableFieldIds = [_]
        ),
        unexpected($pred, "not enough field_ids")
    ;
        FirstSolnTableFieldIds =
            [NumLaterSolnsFieldId, FirstLaterSolnRowFieldId
            | FirstSolnOutFieldIds]
    ),
    ml_construct_model_non_switch_vector(ModuleInfo, StartVal, EndVal,
        0, CaseSolns, FirstSolnStructType, LaterSolnStructType, OutTypes,
        [], RevFirstSolnRowInitializers,
        cord.init, LaterSolnRowInitializersCord, no, HadDummyRows),
    list.reverse(RevFirstSolnRowInitializers, FirstSolnRowInitializers),
    LaterSolnRowInitializers = cord.list(LaterSolnRowInitializersCord),
    ml_gen_static_vector_defn(MLDS_ModuleName, FirstSolnStructTypeNum,
        FirstSolnRowInitializers, FirstSolnVectorCommon,
        GlobalData2, GlobalData3),
    ml_gen_static_vector_defn(MLDS_ModuleName, LaterSolnStructTypeNum,
        LaterSolnRowInitializers, LaterSolnVectorCommon,
        GlobalData3, GlobalData),
    ml_gen_info_set_global_data(GlobalData, !Info),

    (
        NeedBitVecCheck = dont_need_bit_vec_check_no_gaps,
        expect(unify(HadDummyRows, no), $pred,
            "bad dont_need_bit_vec_check_no_gaps")
    ;
        NeedBitVecCheck = dont_need_bit_vec_check_with_gaps,
        expect(unify(HadDummyRows, yes), $pred,
            "bad dont_need_bit_vec_check_with_gaps")
    ;
        NeedBitVecCheck = need_bit_vec_check,
        expect(unify(HadDummyRows, yes), $pred, "bad need_bit_vec_check")
    ),

    ml_gen_several_soln_lookup_code(Context, IndexRval,
        OutVars, OutTypes, FirstSolnStructType, LaterSolnStructType,
        NumLaterSolnsFieldId, FirstLaterSolnRowFieldId,
        FirstSolnOutFieldIds, LaterSolnOutFieldIds,
        FirstSolnVectorCommon, LaterSolnVectorCommon, NeedBitVecCheck,
        MatchDefns, InRangeStmts, !Info),
    InRangeStmt = ml_stmt_block(MatchDefns, [], InRangeStmts, Context),

    (
        NeedRangeCheck = dont_need_range_check,
        Stmt = InRangeStmt
    ;
        NeedRangeCheck = need_range_check,
        Difference = EndVal - StartVal,
        RangeCheckCond = ml_binop(unsigned_le, IndexRval,
            ml_const(mlconst_int(Difference))),
        Stmt = ml_stmt_if_then_else(RangeCheckCond, InRangeStmt, no, Context)
    ).

%---------------------------------------------------------------------------%

ml_gen_several_soln_lookup_code(Context, SlotVarRval,
        OutVars, OutTypes, FirstSolnStructType, LaterSolnStructType,
        NumLaterSolnsFieldId, FirstLaterSolnRowFieldId,
        FirstSolnOutFieldIds, LaterSolnOutFieldIds,
        FirstSolnVectorCommon, LaterSolnVectorCommon, NeedBitVecCheck,
        MatchDefns, Stmts, !Info) :-
    make_several_soln_lookup_vars(Context, SeveralSolnLookupVars, !Info),
    SeveralSolnLookupVars = ml_several_soln_lookup_vars(
        NumLaterSolnsVarNameType, LaterSlotVarNameType, LimitVarNameType,
        LimitAssignStmt, IncrLaterSlotVarStmt, MatchDefns),

    NumLaterSolnsVarNameType =
        mlds_local_var_name_type(NumLaterSolnsVarName, NumLaterSolnsType),
    NumLaterSolnsVarLval =
        ml_local_var(NumLaterSolnsVarName, NumLaterSolnsType),
    LaterSlotVarNameType =
        mlds_local_var_name_type(LaterSlotVarName, LaterSlotType),
    LaterSlotVarLval =
        ml_local_var(LaterSlotVarName, LaterSlotType),
    LimitVarNameType =
        mlds_local_var_name_type(LimitVarName, LimitType),
    LimitVarLval =
        ml_local_var(LimitVarName, LimitType),

    NumLaterSolnsVarRval = ml_lval(NumLaterSolnsVarLval),
    LaterSlotVarRval = ml_lval(LaterSlotVarLval),
    LimitVarRval = ml_lval(LimitVarLval),
    MLDS_IntType = mlds_builtin_type_int(int_type_int),

    ml_generate_field_assign(NumLaterSolnsVarLval, MLDS_IntType,
        NumLaterSolnsFieldId, FirstSolnVectorCommon, FirstSolnStructType,
        SlotVarRval, Context, NumLaterSolnsAssignStmt, !Info),
    ml_generate_field_assign(LaterSlotVarLval, MLDS_IntType,
        FirstLaterSolnRowFieldId, FirstSolnVectorCommon, FirstSolnStructType,
        SlotVarRval, Context, LaterSlotVarAssignStmt, !Info),
    ml_generate_field_assigns(OutVars, OutTypes, FirstSolnOutFieldIds,
        FirstSolnVectorCommon, FirstSolnStructType,
        SlotVarRval, Context, FirstSolnLookupStmts, !Info),
    ml_generate_field_assigns(OutVars, OutTypes, LaterSolnOutFieldIds,
        LaterSolnVectorCommon, LaterSolnStructType,
        LaterSlotVarRval, Context, LaterSolnLookupStmts, !Info),

    ml_gen_call_current_success_cont(Context, CallContStmt, !Info),
    FirstLookupSucceedStmt = ml_stmt_block([], [],
        FirstSolnLookupStmts ++ [CallContStmt], Context),

    LaterLookupSucceedStmt = ml_stmt_block([], [],
        LaterSolnLookupStmts ++ [CallContStmt, IncrLaterSlotVarStmt], Context),

    MoreSolnsLoopCond = ml_binop(int_lt(int_type_int),
        LaterSlotVarRval, LimitVarRval),
    MoreSolnsLoopStmt = ml_stmt_while(may_loop_zero_times, MoreSolnsLoopCond,
        LaterLookupSucceedStmt, [LaterSlotVarName], Context),

    OneOrMoreSolnsStmts = [FirstLookupSucceedStmt, LaterSlotVarAssignStmt,
        LimitAssignStmt, MoreSolnsLoopStmt],
    (
        ( NeedBitVecCheck = dont_need_bit_vec_check_no_gaps
        ; NeedBitVecCheck = dont_need_bit_vec_check_with_gaps
        ),
        Stmts = [NumLaterSolnsAssignStmt | OneOrMoreSolnsStmts]
    ;
        NeedBitVecCheck = need_bit_vec_check,
        OneOrMoreSolnsBlockStmt =
            ml_stmt_block([], [], OneOrMoreSolnsStmts, Context),

        AnySolnsCond = ml_binop(int_ge(int_type_int),
            NumLaterSolnsVarRval, ml_const(mlconst_int(0))),
        ZeroOrMoreSolnsStmt = ml_stmt_if_then_else(AnySolnsCond,
            OneOrMoreSolnsBlockStmt, no, Context),

        Stmts = [NumLaterSolnsAssignStmt, ZeroOrMoreSolnsStmt]
    ).

make_several_soln_lookup_vars(Context, SeveralSolnLookupVars, !Info) :-
    ml_gen_info_new_aux_var_name(mcav_num_later_solns, NumLaterSolnsVarName,
        !Info),
    % We never need to trace ints.
    IntType = mlds_builtin_type_int(int_type_int),
    NumLaterSolnsVarDefn = ml_gen_mlds_var_decl(NumLaterSolnsVarName, IntType,
        gc_no_stmt, Context),
    NumLaterSolnsVarNameType =
        mlds_local_var_name_type(NumLaterSolnsVarName, IntType),
    NumLaterSolnsVarLval = ml_local_var(NumLaterSolnsVarName, IntType),

    ml_gen_info_new_aux_var_name(mcav_later_slot, LaterSlotVarName, !Info),
    % We never need to trace ints.
    LaterSlotVarDefn = ml_gen_mlds_var_decl(LaterSlotVarName, IntType,
        gc_no_stmt, Context),
    LaterSlotVarNameType = mlds_local_var_name_type(LaterSlotVarName, IntType),
    LaterSlotVarLval = ml_local_var(LaterSlotVarName, IntType),

    ml_gen_info_new_aux_var_name(mcav_limit, LimitVarName, !Info),
    % We never need to trace ints.
    LimitVarDefn = ml_gen_mlds_var_decl(LimitVarName, IntType,
        gc_no_stmt, Context),
    LimitVarNameType = mlds_local_var_name_type(LimitVarName, IntType),
    LimitVarLval = ml_local_var(LimitVarName, IntType),

    Defns = [NumLaterSolnsVarDefn, LaterSlotVarDefn, LimitVarDefn],

    LaterSlotVarRval = ml_lval(LaterSlotVarLval),
    NumLaterSolnsVarRval = ml_lval(NumLaterSolnsVarLval),
    LimitAssign = assign(LimitVarLval,
        ml_binop(int_add(int_type_int),
            LaterSlotVarRval, NumLaterSolnsVarRval)),
    LimitAssignStmt = ml_stmt_atomic(LimitAssign, Context),
    IncrLaterSlotVar = assign(LaterSlotVarLval,
        ml_binop(int_add(int_type_int), LaterSlotVarRval,
            ml_const(mlconst_int(1)))),
    IncrLaterSlotVarStmt = ml_stmt_atomic(IncrLaterSlotVar, Context),

    SeveralSolnLookupVars = ml_several_soln_lookup_vars(
        NumLaterSolnsVarNameType, LaterSlotVarNameType, LimitVarNameType,
        LimitAssignStmt, IncrLaterSlotVarStmt, Defns).

%---------------------------------------------------------------------------%

    % The bitvector is an array of words (where we use the first 32 bits
    % of each word). Each bit represents a tag value for the (range checked)
    % input to the lookup switch. The bit is `1' iff we have a case for that
    % tag value.
    %
:- pred ml_generate_bitvec_test(mlds_module_name::in, prog_context::in,
    mlds_rval::in, assoc_list(int, T)::in, int::in, int::in,
    mlds_rval::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_bitvec_test(MLDS_ModuleName, Context, IndexRval, CaseVals,
        Start, _End, CheckRval, !Info) :-
    ml_gen_info_get_globals(!.Info, Globals),
    get_word_bits(Globals, WordBits, Log2WordBits),
    ml_gen_info_get_global_data(!.Info, GlobalData0),
    ml_generate_bit_vec(MLDS_ModuleName, Context, CaseVals, Start, WordBits,
        BitVecArgRvals, BitVecRval, GlobalData0, GlobalData),

    % Optimize the single-word case: if all the cases fit into a single word,
    % then the word to use is always that word, and the index specifies which
    % bit; we don't need the array.
    ( if BitVecArgRvals = [SingleWordRval] then
        % Do not save GlobalData back into !Info.
        WordRval = SingleWordRval,
        BitNumRval = IndexRval
    else
        % Otherwise, the high bits of the index specify which word in the array
        % to use and the low bits specify which bit in that word.
        ml_gen_info_set_global_data(GlobalData, !Info),

        % This is the same as
        % WordNumRval = ml_binop(int_div, IndexRval,
        %   ml_const(mlconst_int(WordBits)))
        % except that it can generate more efficient code.
        WordNumRval =
            ml_binop(unchecked_right_shift(int_type_int, shift_by_int),
                IndexRval, ml_const(mlconst_int(Log2WordBits))),

        ArrayElemType = array_elem_scalar(scalar_elem_int),
        WordRval = ml_binop(array_index(ArrayElemType),
            BitVecRval, WordNumRval),

        % This is the same as
        % BitNumRval = ml_binop(int_mod, IndexRval,
        %   ml_const(mlconst_int(WordBits)))
        % except that it can generate more efficient code.
        BitNumRval = ml_binop(bitwise_and(int_type_int), IndexRval,
            ml_const(mlconst_int(WordBits - 1)))
    ),
    CheckRval = ml_binop(bitwise_and(int_type_int), WordRval,
        ml_binop(unchecked_left_shift(int_type_int, shift_by_int),
            ml_const(mlconst_int(1)), BitNumRval)).

    % We generate the bitvector by iterating through the cases marking the bit
    % for each case. We represent the bitvector here as a map from the word
    % number in the vector to the bits for that word.
    %
:- pred ml_generate_bit_vec(mlds_module_name::in, prog_context::in,
    assoc_list(int, T)::in, int::in, int::in,
    list(mlds_rval)::out, mlds_rval::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_generate_bit_vec(MLDS_ModuleName, Context, CaseVals, Start, WordBits,
        WordRvals, BitVecRval, !GlobalData) :-
    map.init(BitMap0),
    ml_generate_bit_vec_2(CaseVals, Start, WordBits, BitMap0, BitMap),
    map.to_assoc_list(BitMap, WordVals),
    ml_generate_bit_vec_initializers(WordVals, 0, WordRvals, WordInitializers),
    Initializer = init_array(WordInitializers),

    ConstType = mlds_array_type(mlds_builtin_type_int(int_type_int)),
    ml_gen_static_scalar_const_value(MLDS_ModuleName, mgcv_bit_vector,
        ConstType, Initializer, Context, BitVecRval, !GlobalData).

:- pred ml_generate_bit_vec_2(assoc_list(int, T)::in, int::in, int::in,
    map(int, uint)::in, map(int, uint)::out) is det.

ml_generate_bit_vec_2([], _, _, !BitMap).
ml_generate_bit_vec_2([Tag - _ | Rest], Start, WordBits, !BitMap) :-
    Val = Tag - Start,
    WordNum = Val // WordBits,
    Offset = Val mod WordBits,
    ( if map.search(!.BitMap, WordNum, X0) then
        X1 = X0 \/ (1u << Offset),
        map.det_update(WordNum, X1, !BitMap)
    else
        X1 = (1u << Offset),
        map.det_insert(WordNum, X1, !BitMap)
    ),
    ml_generate_bit_vec_2(Rest, Start, WordBits, !BitMap).

:- pred ml_generate_bit_vec_initializers(list(pair(int, uint))::in, int::in,
    list(mlds_rval)::out, list(mlds_initializer)::out) is det.

ml_generate_bit_vec_initializers([], _, [], []).
ml_generate_bit_vec_initializers(All @ [WordNum - Bits | Rest], Count,
        [Rval | Rvals], [Initializer | Initializers]) :-
    ( if Count < WordNum then
        WordVal = 0u,
        Remainder = All
    else
        WordVal = Bits,
        Remainder = Rest
    ),
    Rval = ml_const(mlconst_uint(WordVal)),
    Initializer = init_obj(Rval),
    ml_generate_bit_vec_initializers(Remainder, Count + 1,
        Rvals, Initializers).

%---------------------------------------------------------------------------%

:- pred ml_construct_simple_switch_vector(module_info::in,
    mlds_type::in, list(mlds_type)::in, int::in,
    assoc_list(int, list(mlds_rval))::in, list(mlds_initializer)::out) is det.

ml_construct_simple_switch_vector(_, _, _, _, [], []).
ml_construct_simple_switch_vector(ModuleInfo, StructType, FieldTypes,
        CurIndex, [Pair | Pairs], [RowInitializer | RowInitializers]) :-
    Pair = Index - Rvals,
    ( if CurIndex < Index then
        FieldRvals = list.map(ml_default_value_for_type, FieldTypes),
        RemainingPairs = [Pair | Pairs]
    else
        FieldRvals = Rvals,
        RemainingPairs = Pairs
    ),
    FieldInitializers = list.map(wrap_init_obj, FieldRvals),
    RowInitializer = init_struct(StructType, FieldInitializers),
    ml_construct_simple_switch_vector(ModuleInfo, StructType, FieldTypes,
        CurIndex + 1, RemainingPairs, RowInitializers).

:- pred ml_construct_model_non_switch_vector(module_info::in,
    int::in, int::in, int::in, assoc_list(int, soln_consts(mlds_rval))::in,
    mlds_type::in, mlds_type::in, list(mlds_type)::in,
    list(mlds_initializer)::in, list(mlds_initializer)::out,
    cord(mlds_initializer)::in, cord(mlds_initializer)::out,
    bool::in, bool::out) is det.

ml_construct_model_non_switch_vector(ModuleInfo, CurIndex, EndVal,
        !.NextLaterSolnRow, [],
        FirstSolnStructType, LaterSolnStructType, FieldTypes,
        !RevFirstSolnRowInitializers, !LaterSolnRowInitializersCord,
        !HadDummyRows) :-
    ( if CurIndex > EndVal then
        true
    else
        make_dummy_first_soln_row(FirstSolnStructType, FieldTypes,
            !RevFirstSolnRowInitializers),
        !:HadDummyRows = yes,
        ml_construct_model_non_switch_vector(ModuleInfo, CurIndex + 1, EndVal,
            !.NextLaterSolnRow, [],
            FirstSolnStructType, LaterSolnStructType, FieldTypes,
            !RevFirstSolnRowInitializers, !LaterSolnRowInitializersCord,
            !HadDummyRows)
    ).
ml_construct_model_non_switch_vector(ModuleInfo, CurIndex, EndVal,
        !.NextLaterSolnRow, [Pair | Pairs],
        FirstSolnStructType, LaterSolnStructType, FieldTypes,
        !RevFirstSolnRowInitializers, !LaterSolnRowInitializersCord,
        !HadDummyRows) :-
    Pair = Index - Soln,
    ( if CurIndex < Index then
        make_dummy_first_soln_row(FirstSolnStructType, FieldTypes,
            !RevFirstSolnRowInitializers),
        !:HadDummyRows = yes,
        NextPairs = [Pair | Pairs]
    else
        (
            Soln = one_soln(FieldRvals),
            FieldInitializers = list.map(wrap_init_obj, FieldRvals),
            NumLaterSolnsInitializer = gen_init_int(0),
            FirstLaterSlotInitializer = gen_init_int(-1),
            FirstSolnFieldInitializers =
                [NumLaterSolnsInitializer, FirstLaterSlotInitializer
                    | FieldInitializers],
            FirstSolnRowInitializer =
                init_struct(FirstSolnStructType, FirstSolnFieldInitializers),
            !:RevFirstSolnRowInitializers =
                [FirstSolnRowInitializer | !.RevFirstSolnRowInitializers]
        ;
            Soln = several_solns(FirstSolnRvals, LaterSolns),
            FieldInitializers = list.map(wrap_init_obj, FirstSolnRvals),
            list.length(LaterSolns, NumLaterSolns),
            NumLaterSolnsInitializer = gen_init_int(NumLaterSolns),
            FirstLaterSlotInitializer = gen_init_int(!.NextLaterSolnRow),
            FirstSolnFieldInitializers =
                [NumLaterSolnsInitializer, FirstLaterSlotInitializer
                    | FieldInitializers],
            FirstSolnRowInitializer =
                init_struct(FirstSolnStructType, FirstSolnFieldInitializers),
            !:RevFirstSolnRowInitializers =
                [FirstSolnRowInitializer | !.RevFirstSolnRowInitializers],

            LaterSolnRowInitializers = list.map(
                ml_construct_later_soln_row(LaterSolnStructType),
                LaterSolns),
            !:LaterSolnRowInitializersCord = !.LaterSolnRowInitializersCord ++
                from_list(LaterSolnRowInitializers),
            !:NextLaterSolnRow = !.NextLaterSolnRow + NumLaterSolns
        ),
        NextPairs = Pairs
    ),
    ml_construct_model_non_switch_vector(ModuleInfo, CurIndex + 1, EndVal,
        !.NextLaterSolnRow, NextPairs,
        FirstSolnStructType, LaterSolnStructType, FieldTypes,
        !RevFirstSolnRowInitializers, !LaterSolnRowInitializersCord,
        !HadDummyRows).

ml_construct_later_soln_row(StructType, Rvals) = RowInitializer :-
    FieldInitializers = list.map(wrap_init_obj, Rvals),
    RowInitializer = init_struct(StructType, FieldInitializers).

%---------------------------------------------------------------------------%

:- pred make_dummy_first_soln_row(mlds_type::in, list(mlds_type)::in,
    list(mlds_initializer)::in, list(mlds_initializer)::out) is det.

make_dummy_first_soln_row(FirstSolnStructType, FieldTypes,
        !RevFirstSolnRowInitializers) :-
    FieldRvals = list.map(ml_default_value_for_type, FieldTypes),
    FieldInitializers = list.map(wrap_init_obj, FieldRvals),
    NumLaterSolnsInitializer = gen_init_int(-1),
    FirstLaterSlotInitializer = gen_init_int(-1),
    FirstSolnFieldInitializers =
        [NumLaterSolnsInitializer, FirstLaterSlotInitializer
            | FieldInitializers],
    FirstSolnRowInitializer =
        init_struct(FirstSolnStructType, FirstSolnFieldInitializers),
    !:RevFirstSolnRowInitializers =
        [FirstSolnRowInitializer | !.RevFirstSolnRowInitializers].

ml_default_value_for_type(MLDS_Type) = DefaultRval :-
    (
        MLDS_Type = mlds_builtin_type_int(IntType),
        (
            IntType = int_type_int,
            DefaultRval = ml_const(mlconst_int(0))
        ;
            IntType = int_type_int8,
            DefaultRval = ml_const(mlconst_int8(0i8))
        ;
            IntType = int_type_int16,
            DefaultRval = ml_const(mlconst_int16(0i16))
        ;
            IntType = int_type_int32,
            DefaultRval = ml_const(mlconst_int32(0i32))
        ;
            IntType = int_type_int64,
            DefaultRval = ml_const(mlconst_int64(0i64))
        ;
            IntType = int_type_uint,
            DefaultRval = ml_const(mlconst_uint(0u))
        ;
            IntType = int_type_uint8,
            DefaultRval = ml_const(mlconst_uint8(0u8))
        ;
            IntType = int_type_uint16,
            DefaultRval = ml_const(mlconst_uint16(0u16))
        ;
            IntType = int_type_uint32,
            DefaultRval = ml_const(mlconst_uint32(0u32))
        ;
            IntType = int_type_uint64,
            DefaultRval = ml_const(mlconst_uint64(0u64))
        )
    ;
        MLDS_Type = mlds_builtin_type_float,
        DefaultRval = ml_const(mlconst_float(0.0))
    ;
        MLDS_Type = mlds_builtin_type_string,
        DefaultRval = ml_cast(MLDS_Type, ml_const(mlconst_int(0)))
    ;
        MLDS_Type = mlds_builtin_type_char,
        DefaultRval = ml_const(mlconst_char(0))
    ;
        MLDS_Type = mlds_native_bool_type,
        DefaultRval = ml_const(mlconst_false)
    ;
        ( MLDS_Type = mercury_nb_type(_, _)
        ; MLDS_Type = mlds_mercury_array_type(_)
        ; MLDS_Type = mlds_foreign_type(_)
        ; MLDS_Type = mlds_class_type(_)
        ; MLDS_Type = mlds_enum_class_type(_)
        ; MLDS_Type = mlds_env_type(_)
        ; MLDS_Type = mlds_struct_type(_)
        ; MLDS_Type = mlds_array_type(_)
        ; MLDS_Type = mlds_mostly_generic_array_type(_)
        ; MLDS_Type = mlds_func_type(_)
        ; MLDS_Type = mlds_generic_type
        ; MLDS_Type = mlds_type_info_type
        ; MLDS_Type = mlds_pseudo_type_info_type
        ),
        DefaultRval = ml_cast(MLDS_Type, ml_const(mlconst_int(0)))
    ;
        ( MLDS_Type = mlds_cont_type(_)
        ; MLDS_Type = mlds_commit_type
        ; MLDS_Type = mlds_ptr_type(_)
        ; MLDS_Type = mlds_generic_env_ptr_type
        ; MLDS_Type = mlds_rtti_type(_)
        ; MLDS_Type = mlds_tabling_type(_)
        ; MLDS_Type = mlds_unknown_type
        ),
        unexpected($pred, "unexpected MLDS_Type")
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_lookup_switch.
%---------------------------------------------------------------------------%
