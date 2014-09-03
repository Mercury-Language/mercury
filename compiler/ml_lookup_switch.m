%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
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
%-----------------------------------------------------------------------------%

:- module ml_backend.ml_lookup_switch.
:- interface.

:- import_module backend_libs.switch_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module list.
:- import_module unit.

%-----------------------------------------------------------------------------%

:- type ml_lookup_switch_info(Key)
    --->    ml_lookup_switch_info(
                % The map from the switched-on value to the values of the
                % variables in each solution.
                mllsi_cases             ::  case_consts(Key, mlds_rval, unit),

                % The output variables, which become (some of) the fields
                % in each row of a lookup table.
                mllsi_out_variables     ::  list(prog_var),

                % The types of the fields holding output variables.
                mllsi_out_types         ::  list(mlds_type)
            ).

    % Is the given list of cases implementable as a lookup switch?
    %
:- pred ml_is_lookup_switch(pred(cons_tag, Key)::in(pred(in, out) is det),
    prog_var::in, list(tagged_case)::in, set_of_progvar::in, code_model::in,
    ml_lookup_switch_info(Key)::out,
    ml_gen_info::in, ml_gen_info::out) is semidet.

%-----------------------------------------------------------------------------%

    % Generate MLDS code for the lookup switch.
    %
:- pred ml_gen_lookup_switch(prog_var::in, ml_lookup_switch_info(int)::in,
    code_model::in, prog_context::in, int::in, int::in,
    need_bit_vec_check::in, need_range_check::in,
    statement::out, ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%
%
% These types and predicates are exported because they are useful in the
% implementation of string lookup switches.
%

:- type ml_several_soln_lookup_vars
    --->    ml_several_soln_lookup_vars(
                msslv_num_later_solns_var       :: mlds_lval,
                msslv_later_slot_var            :: mlds_lval,
                msslv_limit_var                 :: mlds_lval,
                msslv_limit_assign_statement    :: statement,
                msslv_incr_later_slot_statement :: statement,
                msslv_denfs                     :: list(mlds_defn)
            ).

:- pred make_several_soln_lookup_vars(mlds_context::in,
    ml_several_soln_lookup_vars::out,
    ml_gen_info::in, ml_gen_info::out) is det.

:- func ml_construct_later_soln_row(mlds_type, list(mlds_rval)) =
    mlds_initializer.

:- func ml_default_value_for_type(mlds_type) = mlds_rval.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_module.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.ml_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.

%-----------------------------------------------------------------------------%

ml_is_lookup_switch(GetTag, SwitchVar, TaggedCases, NonLocals, CodeModel,
        LookupSwitchInfo, !Info) :-
    set_of_var.remove(SwitchVar, NonLocals, OtherNonLocals),
    set_of_var.to_sorted_list(OtherNonLocals, OutVars),
    % While the LLDS backend has to worry about about implementing trailing
    % for model_non lookup switches, we do not. The MLDS backend implements
    % trailing by a HLDS-to-HLDS transform (which is in add_trail_ops.m),
    % so we can get here only if trailing is not enabled, since otherwise
    % the calls or foreign_procs inserted into all non-first disjuncts
    % would cause ml_generate_constants_for_lookup_switch to fail.
    ml_generate_constants_for_lookup_switch(GetTag, CodeModel,
        OutVars, OtherNonLocals, TaggedCases, map.init, CaseSolnMap, !Info),
    map.to_assoc_list(CaseSolnMap, CaseSolns),
    ( project_all_to_one_solution(CaseSolns, CaseValuePairs) ->
        CaseConsts = all_one_soln(CaseValuePairs)
    ;
        CaseConsts = some_several_solns(CaseSolns, unit)
    ),
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ml_gen_info_get_var_types(!.Info, VarTypes),
    lookup_var_types(VarTypes, OutVars, OutTypes),
    FieldTypes =
        list.map(mercury_type_to_mlds_type(ModuleInfo), OutTypes),
    LookupSwitchInfo = ml_lookup_switch_info(CaseConsts, OutVars, FieldTypes).

:- pred ml_generate_constants_for_lookup_switch(
    pred(cons_tag, T)::in(pred(in, out) is det),
    code_model::in, list(prog_var)::in, set_of_progvar::in,
    list(tagged_case)::in,
    map(T, soln_consts(mlds_rval))::in,
    map(T, soln_consts(mlds_rval))::out,
    ml_gen_info::in, ml_gen_info::out) is semidet.

ml_generate_constants_for_lookup_switch(_GetTag, _CodeModel,
        _OutVars, _ArmNonLocals, [], !IndexMap, !Info).
ml_generate_constants_for_lookup_switch(GetTag, CodeModel,
        OutVars, ArmNonLocals, [TaggedCase | TaggedCases], !IndexMap, !Info) :-
    TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds, _, Goal),
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    ( GoalExpr = disj(Disjuncts) ->
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
            ml_record_lookup_for_tagged_cons_id(GetTag, SolnConsts,
                TaggedMainConsId, !IndexMap),
            list.foldl(
                ml_record_lookup_for_tagged_cons_id(GetTag, SolnConsts),
                TaggedOtherConsIds, !IndexMap)
        )
    ;
        goal_is_conj_of_unify(ArmNonLocals, Goal),
        ml_generate_constants_for_arm(OutVars, Goal, Soln, !Info),
        SolnConsts = one_soln(Soln),
        ml_record_lookup_for_tagged_cons_id(GetTag, SolnConsts,
            TaggedMainConsId, !IndexMap),
        list.foldl(ml_record_lookup_for_tagged_cons_id(GetTag, SolnConsts),
            TaggedOtherConsIds, !IndexMap)
    ),
    ml_generate_constants_for_lookup_switch(GetTag, CodeModel,
        OutVars, ArmNonLocals, TaggedCases, !IndexMap, !Info).

:- pred ml_record_lookup_for_tagged_cons_id(
    pred(cons_tag, T)::in(pred(in, out) is det),
    soln_consts(mlds_rval)::in, tagged_cons_id::in,
    map(T, soln_consts(mlds_rval))::in,
    map(T, soln_consts(mlds_rval))::out) is det.

ml_record_lookup_for_tagged_cons_id(GetTag, SolnConsts, TaggedConsId,
        !IndexMap) :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    GetTag(ConsTag, Index),
    map.det_insert(Index, SolnConsts, !IndexMap).

%-----------------------------------------------------------------------------%

ml_gen_lookup_switch(SwitchVar, LookupSwitchInfo, CodeModel, Context,
        StartVal, EndVal, NeedBitVecCheck, NeedRangeCheck, Statement, !Info) :-
    LookupSwitchInfo = ml_lookup_switch_info(CaseConsts, OutVars, FieldTypes),

    ml_gen_var(!.Info, SwitchVar, SwitchVarLval),
    SwitchVarRval = ml_lval(SwitchVarLval),
    ( StartVal = 0 ->
        IndexRval = SwitchVarRval
    ;
        StartRval = ml_const(mlconst_int(StartVal)),
        IndexRval = ml_binop(int_sub, SwitchVarRval, StartRval)
    ),
    (
        CaseConsts = all_one_soln(CaseValuePairs),
        ml_gen_simple_lookup_switch(IndexRval, OutVars, FieldTypes,
            CaseValuePairs, CodeModel, Context, StartVal, EndVal,
            NeedBitVecCheck, NeedRangeCheck, Statement, !Info)
    ;
        CaseConsts = some_several_solns(CaseSolns, _Unit),
        expect(unify(CodeModel, model_non), $module, $pred,
            "CodeModel != model_non"),
        ml_gen_several_soln_lookup_switch(IndexRval, OutVars, FieldTypes,
            CaseSolns, Context, StartVal, EndVal,
            NeedBitVecCheck, NeedRangeCheck, Statement, !Info)
    ).

%-----------------------------------------------------------------------------%

:- pred ml_gen_simple_lookup_switch(mlds_rval::in, list(prog_var)::in,
    list(mlds_type)::in, assoc_list(int, list(mlds_rval))::in, code_model::in,
    prog_context::in, int::in, int::in,
    need_bit_vec_check::in, need_range_check::in,
    statement::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_simple_lookup_switch(IndexRval, OutVars, OutTypes, CaseValues,
        CodeModel, Context, StartVal, EndVal, NeedBitVecCheck, NeedRangeCheck,
        Statement, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    MLDS_Context = mlds_make_context(Context),
    ml_gen_info_get_target(!.Info, Target),

    (
        OutTypes = [],
        % There are no output parameters, so there is nothing to look up.
        % Generating a structure with no fields would cause problems for
        % Visual C, which cannot handle such structures.
        %
        % This should happen only for model_semi switches. If it happens for
        % a model_det switch, that switch is effectively a no-op.
        LookupStatements = []
    ;
        OutTypes = [_ | _],
        ml_gen_info_get_global_data(!.Info, GlobalData0),
        ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
            OutTypes, StructTypeNum, StructType, FieldIds,
            GlobalData0, GlobalData1),

        ml_construct_simple_switch_vector(ModuleInfo, StructType,
            OutTypes, StartVal, CaseValues, RowInitializers),

        ml_gen_static_vector_defn(MLDS_ModuleName, StructTypeNum,
            RowInitializers, VectorCommon, GlobalData1, GlobalData),
        ml_gen_info_set_global_data(GlobalData, !Info),

        ml_generate_field_assigns(OutVars, OutTypes, FieldIds, VectorCommon,
            StructType, IndexRval, MLDS_Context, LookupStatements, !Info)
    ),

    (
        CodeModel = model_det,
        expect(unify(NeedRangeCheck, dont_need_range_check), $module, $pred,
            "model_det need_range_check"),
        expect(unify(NeedBitVecCheck, dont_need_bit_vec_check), $module, $pred,
            "model_det need_bit_vec_check"),
        Stmt = ml_stmt_block([], LookupStatements),
        Statement = statement(Stmt, MLDS_Context)
    ;
        CodeModel = model_semi,
        ml_gen_set_success(!.Info, ml_const(mlconst_true), Context,
            SetSuccessTrueStatement),
        LookupSucceedStmt = ml_stmt_block([],
            LookupStatements ++ [SetSuccessTrueStatement]),
        LookupSucceedStatement = statement(LookupSucceedStmt, MLDS_Context),
        (
            NeedRangeCheck = dont_need_range_check,
            (
                NeedBitVecCheck = dont_need_bit_vec_check,
                Statement = LookupSucceedStatement
            ;
                NeedBitVecCheck = need_bit_vec_check,
                ml_generate_bitvec_test(MLDS_ModuleName, Context, IndexRval,
                    CaseValues, StartVal, EndVal, BitVecCheckCond, !Info),

                ml_gen_set_success(!.Info, ml_const(mlconst_false), Context,
                    SetSuccessFalseStatement),

                Stmt = ml_stmt_if_then_else(BitVecCheckCond,
                    LookupSucceedStatement, yes(SetSuccessFalseStatement)),
                Statement = statement(Stmt, MLDS_Context)
            )
        ;
            NeedRangeCheck = need_range_check,
            Difference = EndVal - StartVal,
            RangeCheckCond = ml_binop(unsigned_le, IndexRval,
                ml_const(mlconst_int(Difference))),
            ml_gen_set_success(!.Info, ml_const(mlconst_false), Context,
                SetSuccessFalseStatement),
            (
                NeedBitVecCheck = dont_need_bit_vec_check,
                RangeCheckSuccessStatement = LookupSucceedStatement
            ;
                NeedBitVecCheck = need_bit_vec_check,
                ml_generate_bitvec_test(MLDS_ModuleName, Context, IndexRval,
                    CaseValues, StartVal, EndVal, BitVecCheckCond, !Info),

                RangeCheckSuccessStmt = ml_stmt_if_then_else(BitVecCheckCond,
                    LookupSucceedStatement, yes(SetSuccessFalseStatement)),
                RangeCheckSuccessStatement =
                    statement(RangeCheckSuccessStmt, MLDS_Context)
            ),

            % We want to execute the bit vector test only if the range check
            % succeeded, since otherwise the bit vector test will probably
            % access the bit vector outside its bounds.
            Stmt = ml_stmt_if_then_else(RangeCheckCond,
                RangeCheckSuccessStatement, yes(SetSuccessFalseStatement)),
            Statement = statement(Stmt, MLDS_Context)
        )
    ;
        CodeModel = model_non,
        % If all the switch arms have exactly one solution, then the switch
        % as a whole cannot be model_non.
        unexpected($module, $pred, "model_non")
    ).

%-----------------------------------------------------------------------------%

:- pred ml_gen_several_soln_lookup_switch(mlds_rval::in, list(prog_var)::in,
    list(mlds_type)::in, assoc_list(int, soln_consts(mlds_rval))::in,
    prog_context::in, int::in, int::in,
    need_bit_vec_check::in, need_range_check::in,
    statement::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_several_soln_lookup_switch(IndexRval, OutVars, OutTypes,
        CaseSolns, Context, StartVal, EndVal, NeedBitVecCheck, NeedRangeCheck,
        Statement, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    MLDS_Context = mlds_make_context(Context),
    ml_gen_info_get_target(!.Info, Target),

    make_several_soln_lookup_vars(MLDS_Context, SeveralSolnLookupVars, !Info),
    SeveralSolnLookupVars = ml_several_soln_lookup_vars(NumLaterSolnsVarLval,
        LaterSlotVarLval, LimitVarLval,
        LimitAssignStatement, IncrLaterSlotVarStatement, Defns),

    NumLaterSolnsVarRval = ml_lval(NumLaterSolnsVarLval),
    LaterSlotVarRval = ml_lval(LaterSlotVarLval),
    LimitVarRval = ml_lval(LimitVarLval),

    MLDS_IntType = mlds_native_int_type,
    FirstSolnFieldTypes = [MLDS_IntType, MLDS_IntType | OutTypes],

    ml_gen_call_current_success_cont(Context, CallContStatement, !Info),

    ml_gen_info_get_global_data(!.Info, GlobalData0),
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        FirstSolnFieldTypes, FirstSolnStructTypeNum, FirstSolnStructType,
        FirstSolnTableFieldIds, GlobalData0, GlobalData1),
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        OutTypes, LaterSolnStructTypeNum, LaterSolnStructType,
        LaterSolnFieldIds, GlobalData1, GlobalData2),
    (
        ( FirstSolnTableFieldIds = []
        ; FirstSolnTableFieldIds = [_]
        ),
        unexpected($module, $pred, "not enough field_ids")
    ;
        FirstSolnTableFieldIds =
            [NumLaterSolnsFieldId, FirstLaterRowFieldId | FirstSolnFieldIds]
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

    ml_generate_field_assign(NumLaterSolnsVarLval, MLDS_IntType,
        NumLaterSolnsFieldId,
        FirstSolnVectorCommon, FirstSolnStructType, IndexRval,
        MLDS_Context, NumLaterSolnsLookupStatement, !Info),
    ml_generate_field_assign(LaterSlotVarLval, MLDS_IntType,
        FirstLaterRowFieldId,
        FirstSolnVectorCommon, FirstSolnStructType, IndexRval,
        MLDS_Context, LaterSlotVarLookupStatement, !Info),
    ml_generate_field_assigns(OutVars, OutTypes, FirstSolnFieldIds,
        FirstSolnVectorCommon, FirstSolnStructType, IndexRval,
        MLDS_Context, FirstSolnLookupStatements, !Info),
    ml_generate_field_assigns(OutVars, OutTypes, LaterSolnFieldIds,
        LaterSolnVectorCommon, LaterSolnStructType, LaterSlotVarRval,
        MLDS_Context, LaterSolnLookupStatements, !Info),

    FirstLookupSucceedStmt = ml_stmt_block([],
        FirstSolnLookupStatements ++ [CallContStatement]),
    FirstLookupSucceedStatement =
        statement(FirstLookupSucceedStmt, MLDS_Context),

    LaterLookupSucceedStmt = ml_stmt_block([],
        LaterSolnLookupStatements ++
        [CallContStatement, IncrLaterSlotVarStatement]),
    LaterLookupSucceedStatement =
        statement(LaterLookupSucceedStmt, MLDS_Context),

    MoreSolnsLoopCond = ml_binop(int_lt, LaterSlotVarRval, LimitVarRval),
    MoreSolnsLoopStmt = ml_stmt_while(may_loop_zero_times, MoreSolnsLoopCond,
        LaterLookupSucceedStatement),
    MoreSolnsLoopStatement = statement(MoreSolnsLoopStmt, MLDS_Context),

    OneOrMoreSolnsStatements = [FirstLookupSucceedStatement,
        LaterSlotVarLookupStatement, LimitAssignStatement,
        MoreSolnsLoopStatement],

    (
        NeedBitVecCheck = dont_need_bit_vec_check,
        expect(unify(HadDummyRows, no), $module, $pred,
            "bad dont_need_bit_vec_check"),
        InRangeStmt = ml_stmt_block(Defns,
            [NumLaterSolnsLookupStatement | OneOrMoreSolnsStatements]),
        InRangeStatement = statement(InRangeStmt, MLDS_Context)
    ;
        NeedBitVecCheck = need_bit_vec_check,
        expect(unify(HadDummyRows, yes), $module, $pred,
            "bad need_bit_vec_check"),

        OneOrMoreSolnsBlockStmt = ml_stmt_block([], OneOrMoreSolnsStatements),
        OneOrMoreSolnsBlockStatement =
            statement(OneOrMoreSolnsBlockStmt, MLDS_Context),

        AnySolnsCond = ml_binop(int_ge,
            NumLaterSolnsVarRval, ml_const(mlconst_int(0))),
        ZeroOrMoreSolnsStmt = ml_stmt_if_then_else(AnySolnsCond,
            OneOrMoreSolnsBlockStatement, no),
        ZeroOrMoreSolnsStatement =
            statement(ZeroOrMoreSolnsStmt, MLDS_Context),

        InRangeStmt = ml_stmt_block(Defns,
            [NumLaterSolnsLookupStatement, ZeroOrMoreSolnsStatement]),
        InRangeStatement = statement(InRangeStmt, MLDS_Context)
    ),
    (
        NeedRangeCheck = dont_need_range_check,
        Statement = InRangeStatement
    ;
        NeedRangeCheck = need_range_check,
        Difference = EndVal - StartVal,

        RangeCheckCond = ml_binop(unsigned_le, IndexRval,
            ml_const(mlconst_int(Difference))),

        Stmt = ml_stmt_if_then_else(RangeCheckCond, InRangeStatement, no),
        Statement = statement(Stmt, MLDS_Context)
    ).

make_several_soln_lookup_vars(MLDS_Context, SeveralSolnLookupVars, !Info) :-
    ml_gen_info_new_aux_var_name("num_later_solns", NumLaterSolnsVar, !Info),
    % We never need to trace ints.
    NumLaterSolnsVarDefn = ml_gen_mlds_var_decl(
        mlds_data_var(NumLaterSolnsVar), mlds_native_int_type, gc_no_stmt,
        MLDS_Context),
    ml_gen_var_lval(!.Info, NumLaterSolnsVar, mlds_native_int_type,
        NumLaterSolnsVarLval),

    ml_gen_info_new_aux_var_name("later_slot", LaterSlotVar, !Info),
    % We never need to trace ints.
    LaterSlotVarDefn = ml_gen_mlds_var_decl(mlds_data_var(LaterSlotVar),
        mlds_native_int_type, gc_no_stmt, MLDS_Context),
    ml_gen_var_lval(!.Info, LaterSlotVar, mlds_native_int_type,
        LaterSlotVarLval),

    ml_gen_info_new_aux_var_name("limit", LimitVar, !Info),
    % We never need to trace ints.
    LimitVarDefn = ml_gen_mlds_var_decl(mlds_data_var(LimitVar),
        mlds_native_int_type, gc_no_stmt, MLDS_Context),
    ml_gen_var_lval(!.Info, LimitVar, mlds_native_int_type, LimitVarLval),

    Defns = [NumLaterSolnsVarDefn, LaterSlotVarDefn, LimitVarDefn],

    LaterSlotVarRval = ml_lval(LaterSlotVarLval),
    NumLaterSolnsVarRval = ml_lval(NumLaterSolnsVarLval),
    LimitAssignStmt = ml_stmt_atomic(assign(LimitVarLval,
        ml_binop(int_add, LaterSlotVarRval, NumLaterSolnsVarRval))),
    LimitAssignStatement = statement(LimitAssignStmt, MLDS_Context),
    IncrLaterSlotVarStmt = ml_stmt_atomic(assign(LaterSlotVarLval,
        ml_binop(int_add, LaterSlotVarRval, ml_const(mlconst_int(1))))),
    IncrLaterSlotVarStatement = statement(IncrLaterSlotVarStmt, MLDS_Context),

    SeveralSolnLookupVars = ml_several_soln_lookup_vars(NumLaterSolnsVarLval,
        LaterSlotVarLval, LimitVarLval,
        LimitAssignStatement, IncrLaterSlotVarStatement, Defns).

%-----------------------------------------------------------------------------%

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
    ( BitVecArgRvals = [SingleWordRval] ->
        % Do not save GlobalData back into !Info.
        WordRval = SingleWordRval,
        BitNumRval = IndexRval
    ;
        % Otherwise, the high bits of the index specify which word in the array
        % to use and the low bits specify which bit in that word.
        ml_gen_info_set_global_data(GlobalData, !Info),

        % This is the same as
        % WordNumRval = ml_binop(int_div, IndexRval,
        %   ml_const(mlconst_int(WordBits)))
        % except that it can generate more efficient code.
        WordNumRval = ml_binop(unchecked_right_shift, IndexRval,
            ml_const(mlconst_int(Log2WordBits))),

        ArrayElemType = array_elem_scalar(scalar_elem_int),
        WordRval = ml_binop(array_index(ArrayElemType),
            BitVecRval, WordNumRval),

        % This is the same as
        % BitNumRval = ml_binop(int_mod, IndexRval,
        %   ml_const(mlconst_int(WordBits)))
        % except that it can generate more efficient code.
        BitNumRval = ml_binop(bitwise_and, IndexRval,
            ml_const(mlconst_int(WordBits - 1)))
    ),
    CheckRval = ml_binop(bitwise_and, WordRval,
        ml_binop(unchecked_left_shift, ml_const(mlconst_int(1)), BitNumRval)).

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

    ConstType = mlds_array_type(mlds_native_int_type),
    ml_gen_static_scalar_const_value(MLDS_ModuleName, "bit_vector", ConstType,
        Initializer, Context, BitVecRval, !GlobalData).

:- pred ml_generate_bit_vec_2(assoc_list(int, T)::in, int::in, int::in,
    map(int, int)::in, map(int, int)::out) is det.

ml_generate_bit_vec_2([], _, _, !BitMap).
ml_generate_bit_vec_2([Tag - _ | Rest], Start, WordBits, !BitMap) :-
    Val = Tag - Start,
    WordNum = Val // WordBits,
    Offset = Val mod WordBits,
    ( map.search(!.BitMap, WordNum, X0) ->
        X1 = X0 \/ (1 << Offset)
    ;
        X1 = (1 << Offset)
    ),
    map.set(WordNum, X1, !BitMap),
    ml_generate_bit_vec_2(Rest, Start, WordBits, !BitMap).

:- pred ml_generate_bit_vec_initializers(list(pair(int))::in, int::in,
    list(mlds_rval)::out, list(mlds_initializer)::out) is det.

ml_generate_bit_vec_initializers([], _, [], []).
ml_generate_bit_vec_initializers(All @ [WordNum - Bits | Rest], Count,
        [Rval | Rvals], [Initializer | Initializers]) :-
    ( Count < WordNum ->
        WordVal = 0,
        Remainder = All
    ;
        WordVal = Bits,
        Remainder = Rest
    ),
    Rval = ml_const(mlconst_int(WordVal)),
    Initializer = init_obj(Rval),
    ml_generate_bit_vec_initializers(Remainder, Count + 1,
        Rvals, Initializers).

%-----------------------------------------------------------------------------%

:- pred ml_construct_simple_switch_vector(module_info::in,
    mlds_type::in, list(mlds_type)::in, int::in,
    assoc_list(int, list(mlds_rval))::in, list(mlds_initializer)::out) is det.

ml_construct_simple_switch_vector(_, _, _, _, [], []).
ml_construct_simple_switch_vector(ModuleInfo, StructType, FieldTypes,
        CurIndex, [Pair | Pairs], [RowInitializer | RowInitializers]) :-
    Pair = Index - Rvals,
    ( CurIndex < Index ->
        FieldRvals = list.map(ml_default_value_for_type, FieldTypes),
        RemainingPairs = [Pair | Pairs]
    ;
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
    ( CurIndex > EndVal ->
        true
    ;
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
    ( CurIndex < Index ->
        make_dummy_first_soln_row(FirstSolnStructType, FieldTypes,
            !RevFirstSolnRowInitializers),
        !:HadDummyRows = yes,
        NextPairs = [Pair | Pairs]
    ;
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

%-----------------------------------------------------------------------------%

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
        MLDS_Type = mlds_native_int_type,
        DefaultRval = ml_const(mlconst_int(0))
    ;
        MLDS_Type = mlds_native_char_type,
        DefaultRval = ml_const(mlconst_char(0))
    ;
        MLDS_Type = mlds_native_bool_type,
        DefaultRval = ml_const(mlconst_false)
    ;
        MLDS_Type = mlds_native_float_type,
        DefaultRval = ml_const(mlconst_float(0.0))
    ;
        ( MLDS_Type = mercury_type(_, _, _)
        ; MLDS_Type = mlds_mercury_array_type(_)
        ; MLDS_Type = mlds_foreign_type(_)
        ; MLDS_Type = mlds_class_type(_, _, _)
        ; MLDS_Type = mlds_array_type(_)
        ; MLDS_Type = mlds_mostly_generic_array_type(_)
        ; MLDS_Type = mlds_func_type(_)
        ; MLDS_Type = mlds_generic_type
        ; MLDS_Type = mlds_type_info_type
        ; MLDS_Type = mlds_pseudo_type_info_type
        ),
        DefaultRval = ml_unop(cast(MLDS_Type), ml_const(mlconst_int(0)))
    ;
        ( MLDS_Type = mlds_cont_type(_)
        ; MLDS_Type = mlds_commit_type
        ; MLDS_Type = mlds_ptr_type(_)
        ; MLDS_Type = mlds_generic_env_ptr_type
        ; MLDS_Type = mlds_rtti_type(_)
        ; MLDS_Type = mlds_tabling_type(_)
        ; MLDS_Type = mlds_unknown_type
        ),
        unexpected($module, $pred, "unexpected MLDS_Type")
    ).

%-----------------------------------------------------------------------------%
:- end_module ml_backend.ml_lookup_switch.
%-----------------------------------------------------------------------------%
