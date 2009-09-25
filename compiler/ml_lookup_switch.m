%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ml_lookup_switch.m
% Author: zs.
%
%-----------------------------------------------------------------------------%

:- module ml_backend.ml_lookup_switch.
:- interface.

:- import_module backend_libs.switch_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

    % Generate MLDS code for a lookup switch, if possible.
    %
:- pred ml_gen_lookup_switch(prog_var::in, list(tagged_case)::in,
    set(prog_var)::in, code_model::in, prog_context::in,
    int::in, int::in, need_bit_vec_check::in, need_range_check::in,
    list(statement)::out, ml_gen_info::in, ml_gen_info::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_global_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module svmap.

%-----------------------------------------------------------------------------%

ml_gen_lookup_switch(SwitchVar, TaggedCases, NonLocals, CodeModel, Context,
        StartVal, EndVal, NeedBitVecCheck, NeedRangeCheck, Statements,
        !Info) :-
    set.remove(NonLocals, SwitchVar, OtherNonLocals),
    set.to_sorted_list(OtherNonLocals, OutVars),
    ml_generate_constants_for_lookup_switch(CodeModel, OtherNonLocals, OutVars,
        TaggedCases, map.init, CaseSolnMap, no, GoalsMayModifyTrail, !Info),

    % XXX We should implement the case GoalsMayModifyTrail = yes.
    GoalsMayModifyTrail = no,

    ml_gen_var(!.Info, SwitchVar, SwitchVarLval),
    SwitchVarRval = ml_lval(SwitchVarLval),
    ( StartVal = 0 ->
        IndexRval = SwitchVarRval
    ;
        StartRval = ml_const(mlconst_int(StartVal)),
        IndexRval = ml_binop(int_sub, SwitchVarRval, StartRval)
    ),

    map.to_assoc_list(CaseSolnMap, CaseSolns),
    ( project_all_to_one_solution(CaseSolns, [], RevCaseValuePairs) ->
        list.reverse(RevCaseValuePairs, CaseValuePairs),
        ml_gen_simple_lookup_switch(IndexRval, OutVars, CaseValuePairs,
            CodeModel, Context, StartVal, EndVal,
            NeedBitVecCheck, NeedRangeCheck, Statement, !Info),
        Statements = [Statement]
    ;
        % XXX We should implement this case.
        fail
    ).

%-----------------------------------------------------------------------------%

:- pred ml_gen_simple_lookup_switch(mlds_rval::in, list(prog_var)::in,
    assoc_list(int, list(mlds_rval))::in, code_model::in, prog_context::in,
    int::in, int::in, need_bit_vec_check::in, need_range_check::in,
    statement::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_simple_lookup_switch(IndexRval, OutVars, CaseValues, CodeModel, Context,
        StartVal, EndVal, NeedBitVecCheck, NeedRangeCheck, Statement, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    MLDS_Context = mlds_make_context(Context),
    ml_gen_info_get_target(!.Info, Target),

    ml_gen_info_get_var_types(!.Info, VarTypes),
    list.map(map.lookup(VarTypes), OutVars, FieldTypes),
    MLDS_FieldTypes =
        list.map(mercury_type_to_mlds_type(ModuleInfo), FieldTypes),

    ml_gen_info_get_global_data(!.Info, GlobalData0),
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        MLDS_FieldTypes, StructTypeNum, StructType, FieldIds,
        GlobalData0, GlobalData1),

    ml_construct_simple_vector(ModuleInfo, Context, StructType,
        MLDS_FieldTypes, StartVal, CaseValues, RowInitializers),

    ml_gen_static_vector_defn(MLDS_ModuleName, StructTypeNum, RowInitializers,
        VectorCommon, GlobalData1, GlobalData),
    ml_gen_info_set_global_data(GlobalData, !Info),

    ml_generate_offset_assigns(OutVars, MLDS_FieldTypes, FieldIds,
        VectorCommon, StructType, IndexRval, MLDS_Context, LookupStatements,
        !Info),

    (
        CodeModel = model_det,
        expect(unify(NeedRangeCheck, dont_need_range_check), this_file,
            "ml_gen_simple_lookup_switch: model_det need_range_check"),
        expect(unify(NeedBitVecCheck, dont_need_bit_vec_check), this_file,
            "ml_gen_simple_lookup_switch: model_det need_bit_vec_check"),
        Stmt = ml_stmt_block([], LookupStatements),
        Statement = statement(Stmt, MLDS_Context)
    ;
        CodeModel = model_semi,
        ml_gen_set_success(!.Info, ml_const(mlconst_true), Context,
            SetSuccessTrueStatement),
        LookupStmt = ml_stmt_block([],
            LookupStatements ++ [SetSuccessTrueStatement]),
        LookupStatement = statement(LookupStmt, MLDS_Context),
        (
            NeedRangeCheck = need_range_check,
            Difference = EndVal - StartVal,
            RangeCheckRval = ml_binop(unsigned_le, IndexRval,
                ml_const(mlconst_int(Difference))),
            ml_gen_set_success(!.Info, ml_const(mlconst_false), Context,
                SetSuccessFalseStatement),
            (
                NeedBitVecCheck = dont_need_bit_vec_check,
                RangeCheckSuccessStatement = LookupStatement
            ;
                NeedBitVecCheck = need_bit_vec_check,
                ml_generate_bitvec_test(MLDS_ModuleName, Context, IndexRval,
                    CaseValues, StartVal, EndVal, BitVecCheckRval, !Info),

                RangeCheckSuccessStmt = ml_stmt_if_then_else(BitVecCheckRval,
                    LookupStatement, yes(SetSuccessFalseStatement)),
                RangeCheckSuccessStatement =
                    statement(RangeCheckSuccessStmt, MLDS_Context)
            ),

            % We want to execute the bit vector test only if the range check
            % succeeded, since otherwise the bit vector test will probably
            % access the bit vector outside its bounds.
            Stmt = ml_stmt_if_then_else(RangeCheckRval,
                RangeCheckSuccessStatement, yes(SetSuccessFalseStatement)),
            Statement = statement(Stmt, MLDS_Context)
        ;
            NeedRangeCheck = dont_need_range_check,
            (
                NeedBitVecCheck = dont_need_bit_vec_check,
                Statement = LookupStatement
            ;
                NeedBitVecCheck = need_bit_vec_check,
                ml_generate_bitvec_test(MLDS_ModuleName, Context, IndexRval,
                    CaseValues, StartVal, EndVal, BitVecCheckRval, !Info),

                ml_gen_set_success(!.Info, ml_const(mlconst_false), Context,
                    SetSuccessFalseStatement),

                Stmt = ml_stmt_if_then_else(BitVecCheckRval,
                    LookupStatement, yes(SetSuccessFalseStatement)),
                Statement = statement(Stmt, MLDS_Context)
            )
        )
    ;
        CodeModel = model_non,
        % If all the switch arms have exactly one solution, then the switch
        % as a whole cannot be model_non.
        unexpected(this_file, "ml_gen_simple_lookup_switch: model_non")
    ).

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
    Word = Val // WordBits,
    Offset = Val mod WordBits,
    ( map.search(!.BitMap, Word, X0) ->
        X1 = X0 \/ (1 << Offset)
    ;
        X1 = (1 << Offset)
    ),
    svmap.set(Word, X1, !BitMap),
    ml_generate_bit_vec_2(Rest, Start, WordBits, !BitMap).

:- pred ml_generate_bit_vec_initializers(list(pair(int))::in, int::in,
    list(mlds_rval)::out, list(mlds_initializer)::out) is det.

ml_generate_bit_vec_initializers([], _, [], []).
ml_generate_bit_vec_initializers([Word - Bits | Rest], Count,
        [Rval | Rvals], [Initializer | Initializers]) :-
    ( Count < Word ->
        WordVal = 0,
        Remainder = [Word - Bits | Rest]
    ;
        WordVal = Bits,
        Remainder = Rest
    ),
    Rval = ml_const(mlconst_int(WordVal)),
    Initializer = init_obj(Rval),
    Count1 = Count + 1,
    ml_generate_bit_vec_initializers(Remainder, Count1, Rvals, Initializers).

%-----------------------------------------------------------------------------%

:- pred ml_construct_simple_vector(module_info::in, prog_context::in,
    mlds_type::in, list(mlds_type)::in, int::in,
    assoc_list(int, list(mlds_rval))::in, list(mlds_initializer)::out) is det.

ml_construct_simple_vector(_, _, _, _, _, [], []).
ml_construct_simple_vector(ModuleInfo, Context, StructType, FieldTypes,
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
    ml_construct_simple_vector(ModuleInfo, Context, StructType, FieldTypes,
        CurIndex + 1, RemainingPairs, RowInitializers).

:- func wrap_init_obj(mlds_rval) = mlds_initializer.

wrap_init_obj(Rval) = init_obj(Rval).

:- func ml_default_value_for_type(mlds_type) = mlds_rval.

ml_default_value_for_type(MLDS_Type) = DefaultRval :-
    (
        MLDS_Type = mlds_native_int_type,
        DefaultRval = ml_const(mlconst_int(0))
    ;
        MLDS_Type = mlds_native_bool_type,
        DefaultRval = ml_const(mlconst_false)
    ;
        MLDS_Type = mlds_native_float_type,
        DefaultRval = ml_const(mlconst_float(0.0))
    ;
        ( MLDS_Type = mlds_native_char_type
        ; MLDS_Type = mercury_type(_, _, _)
        ; MLDS_Type = mlds_mercury_array_type(_)
        ; MLDS_Type = mlds_foreign_type(_)
        ; MLDS_Type = mlds_class_type(_, _, _)
        ; MLDS_Type = mlds_array_type(_)
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
        unexpected(this_file,
            "ml_default_value_for_type: unexpected MLDS_Type")
    ).

%-----------------------------------------------------------------------------%

:- pred ml_generate_offset_assigns(list(prog_var)::in, list(mlds_type)::in,
    list(mlds_field_id)::in, mlds_vector_common::in, mlds_type::in,
    mlds_rval::in, mlds_context::in, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_offset_assigns(OutVars, FieldTypes, FieldIds, VectorCommon,
        StructType, IndexRval, Context, Statements, !Info) :-
    (
        OutVars = [],
        FieldTypes = [],
        FieldIds = []
    ->
        Statements = []
    ;
        OutVars = [HeadOutVar | TailOutVars],
        FieldTypes = [HeadFieldType | TailFieldTypes],
        FieldIds = [HeadFieldId | TailFieldIds]
    ->
        ml_generate_offset_assigns(TailOutVars, TailFieldTypes, TailFieldIds,
            VectorCommon, StructType, IndexRval, Context, TailStatements,
            !Info),

        ml_gen_var(!.Info, HeadOutVar, HeadOutVarLval),
        BaseRval = ml_vector_common_row(VectorCommon, IndexRval),
        HeadFieldLval = ml_field(yes(0), BaseRval, HeadFieldId,
            HeadFieldType, StructType),
        HeadAtomicStmt = assign(HeadOutVarLval, ml_lval(HeadFieldLval)),
        HeadStmt = ml_stmt_atomic(HeadAtomicStmt),
        HeadStatement = statement(HeadStmt, Context),

        Statements = [HeadStatement | TailStatements]
    ;
        unexpected(this_file, "ml_generate_offset_assigns: mismatched lists")
    ).

%-----------------------------------------------------------------------------%

:- pred ml_generate_constants_for_lookup_switch(code_model::in,
    set(prog_var)::in, list(prog_var)::in, list(tagged_case)::in,
    map(int, soln_consts(mlds_rval))::in,
    map(int, soln_consts(mlds_rval))::out,
    bool::in, bool::out, ml_gen_info::in, ml_gen_info::out) is semidet.

ml_generate_constants_for_lookup_switch(_CodeModel, _NonLocals, _OutVars,
        [], !IndexMap, !GoalsMayModifyTrail, !Info).
ml_generate_constants_for_lookup_switch(CodeModel, NonLocals, OutVars,
        [TaggedCase | TaggedCases], !IndexMap, !GoalsMayModifyTrail, !Info) :-
    TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds, _, Goal),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    ( GoalExpr = disj(Disjuncts) ->
        bool.or(goal_may_modify_trail(GoalInfo), !GoalsMayModifyTrail),
        ml_all_disjuncts_are_conj_of_unify(NonLocals, Disjuncts),
        ml_generate_constants_for_disjuncts(CodeModel, OutVars, Disjuncts,
            Solns, !Info),
        SolnConsts = several_solns(Solns)
    ;
        ml_goal_is_conj_of_unify(NonLocals, Goal),
        ml_generate_constants_for_arm(CodeModel, OutVars, Goal, Soln, !Info),
        SolnConsts = one_soln(Soln)
    ),
    ml_record_lookup_for_tagged_cons_id(SolnConsts,
        TaggedMainConsId, !IndexMap),
    list.foldl(ml_record_lookup_for_tagged_cons_id(SolnConsts),
        TaggedOtherConsIds, !IndexMap),
    ml_generate_constants_for_lookup_switch(CodeModel, NonLocals, OutVars,
        TaggedCases, !IndexMap, !GoalsMayModifyTrail, !Info).

:- pred ml_record_lookup_for_tagged_cons_id(soln_consts(mlds_rval)::in,
    tagged_cons_id::in,
    map(int, soln_consts(mlds_rval))::in,
    map(int, soln_consts(mlds_rval))::out) is det.

ml_record_lookup_for_tagged_cons_id(SolnConsts, TaggedConsId, !IndexMap) :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    ( ConsTag = int_tag(Index) ->
        svmap.det_insert(Index, SolnConsts, !IndexMap)
    ;
        unexpected(this_file,
            "ml_record_lookup_for_tagged_cons_id: not int_tag")
    ).

:- pred ml_generate_constants_for_disjuncts(code_model::in, list(prog_var)::in,
    list(hlds_goal)::in, list(list(mlds_rval))::out,
    ml_gen_info::in, ml_gen_info::out) is semidet.

ml_generate_constants_for_disjuncts(_CodeModel, _Vars, [], [], !Info).
ml_generate_constants_for_disjuncts(CodeModel, Vars,
        [Goal | Goals], [Soln | Solns], !Info) :-
    ml_generate_constants_for_arm(CodeModel, Vars, Goal, Soln, !Info),
    ml_generate_constants_for_disjuncts(CodeModel, Vars, Goals, Solns, !Info).

:- pred ml_generate_constants_for_arm(code_model::in, list(prog_var)::in,
    hlds_goal::in, list(mlds_rval)::out,
    ml_gen_info::in, ml_gen_info::out) is semidet.

ml_generate_constants_for_arm(CodeModel, Vars, Goal, Soln, !Info) :-
    ml_gen_info_get_const_var_map(!.Info, InitConstVarMap),
    ml_gen_goal(CodeModel, Goal, _Decls, _Statements, !Info),
    ml_gen_info_get_const_var_map(!.Info, FinalConstVarMap),
    ml_gen_info_set_const_var_map(InitConstVarMap, !Info),
    list.map(search_ground_rval(FinalConstVarMap), Vars, Soln).

:- pred search_ground_rval(ml_ground_term_map::in, prog_var::in,
    mlds_rval::out) is semidet.

search_ground_rval(FinalConstVarMap, Var, Rval) :-
    map.search(FinalConstVarMap, Var, GroundTerm),
    GroundTerm = ml_ground_term(Rval, _, _).

%-----------------------------------------------------------------------------%

    % Is the input goal a conjunction of unifications? A
    % from_ground_term_construct scope counts as a unification.
    %
    % Unlike goal_is_conj_of_unify in lookup_util, we also insist that
    % (a) the unification be a static construction, and (b) the only nonlocal
    % variables being constructed be in the provided set.
    %
:- pred ml_goal_is_conj_of_unify(set(prog_var)::in, hlds_goal::in) is semidet.

ml_goal_is_conj_of_unify(AllowedVars, Goal) :-
    Goal = hlds_goal(_GoalExpr, GoalInfo),
    CodeModel = goal_info_get_code_model(GoalInfo),
    CodeModel = model_det,
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    goal_to_conj_list(Goal, Conj),
    ml_only_constant_goals(NonLocals, AllowedVars, Conj).

    % Run ml_goal_is_conj_of_unify on each goal in the list.
    %
:- pred ml_all_disjuncts_are_conj_of_unify(set(prog_var)::in,
    list(hlds_goal)::in) is semidet.

ml_all_disjuncts_are_conj_of_unify(_AllowedVars, []).
ml_all_disjuncts_are_conj_of_unify(AllowedVars, [Disjunct | Disjuncts]) :-
    ml_goal_is_conj_of_unify(AllowedVars, Disjunct),
    ml_all_disjuncts_are_conj_of_unify(AllowedVars, Disjuncts).

:- pred ml_only_constant_goals(set(prog_var)::in, set(prog_var)::in,
    list(hlds_goal)::in) is semidet.

ml_only_constant_goals(_NonLocals, AllowedVars0, []) :-
    set.empty(AllowedVars0).
ml_only_constant_goals(NonLocals, AllowedVars0, [Goal | Goals]) :-
    Goal = hlds_goal(GoalExpr, _),
    % We could allow calls as well. Some procedures have an output inst
    % that fixes the value of the output variable, which is thus a constant.
    % However, calls to such procedures should have been inlined by now.
    (
        GoalExpr = unify(_, _, _, Unify, _),
        Unify = construct(Var, _, _, _, construct_statically, _, _)
    ;
        GoalExpr = scope(Reason, _),
        Reason = from_ground_term(Var, from_ground_term_construct)
    ),
    ( set.member(Var, NonLocals) ->
        set.remove(AllowedVars0, Var, AllowedVars)
    ;
        % The conjunction is allowed to bind local variables, since these
        % may later be used to construct variables in AllowedVars0.
        AllowedVars = AllowedVars0
    ),
    ml_only_constant_goals(NonLocals, AllowedVars, Goals).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "ml_lookup_switch.m".

%-----------------------------------------------------------------------------%
