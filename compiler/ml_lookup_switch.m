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
    statement::out, ml_gen_info::in, ml_gen_info::out) is semidet.

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
:- import_module ml_backend.ml_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module svmap.

%-----------------------------------------------------------------------------%

ml_gen_lookup_switch(SwitchVar, TaggedCases, NonLocals, CodeModel, Context,
        StartVal, EndVal, NeedBitVecCheck, NeedRangeCheck, Statement, !Info) :-
    set.remove(NonLocals, SwitchVar, OtherNonLocals),
    set.to_sorted_list(OtherNonLocals, OutVars),
    ml_generate_constants_for_lookup_switch(CodeModel, OutVars, OtherNonLocals,
        TaggedCases, map.init, CaseSolnMap, !Info),
    % While the LLDS backend has to worry about about implementing trailing
    % for model_non lookup switches, we do not. The MLDS backend implements
    % trailing by a HLDS-to-HLDS transform (which is in add_trail_ops.m),
    % so we can get here only if trailing is not enabled, since otherwise
    % the calls or foreign_procs inserted into all non-first disjuncts
    % would have caused ml_generate_constants_for_lookup_switch to fail.

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
            NeedBitVecCheck, NeedRangeCheck, Statement, !Info)
    ;
        expect(unify(CodeModel, model_non), this_file,
            "ml_gen_lookup_switch: CodeModel != model_non"),
        ml_gen_model_non_lookup_switch(IndexRval, OutVars, CaseSolns,
            Context, StartVal, EndVal, NeedBitVecCheck, NeedRangeCheck,
            Statement, !Info)
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

    ml_construct_simple_switch_vector(ModuleInfo, StructType,
        MLDS_FieldTypes, StartVal, CaseValues, RowInitializers),

    ml_gen_static_vector_defn(MLDS_ModuleName, StructTypeNum, RowInitializers,
        VectorCommon, GlobalData1, GlobalData),
    ml_gen_info_set_global_data(GlobalData, !Info),

    ml_generate_field_assigns(OutVars, MLDS_FieldTypes, FieldIds,
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
        unexpected(this_file, "ml_gen_simple_lookup_switch: model_non")
    ).

%-----------------------------------------------------------------------------%

:- pred ml_gen_model_non_lookup_switch(mlds_rval::in, list(prog_var)::in,
    assoc_list(int, soln_consts(mlds_rval))::in, prog_context::in,
    int::in, int::in, need_bit_vec_check::in, need_range_check::in,
    statement::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_model_non_lookup_switch(IndexRval, OutVars, CaseSolns, Context,
        StartVal, EndVal, NeedBitVecCheck, NeedRangeCheck, Statement,
        !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    MLDS_Context = mlds_make_context(Context),
    ml_gen_info_get_target(!.Info, Target),

    MLDS_IntType = mlds_native_int_type,

    ml_gen_info_new_aux_var_name("num_later_solns", NumLaterSolnsVar, !Info),
    % We never need to trace ints.
    NumLaterSolnsVarDefn = ml_gen_mlds_var_decl(mlds_data_var(NumLaterSolnsVar),
        MLDS_IntType, gc_no_stmt, MLDS_Context),
    ml_gen_var_lval(!.Info, NumLaterSolnsVar, MLDS_IntType,
        NumLaterSolnsVarLval),
    NumLaterSolnsVarRval = ml_lval(NumLaterSolnsVarLval),

    ml_gen_info_new_aux_var_name("later_slot", LaterSlotVar, !Info),
    % We never need to trace ints.
    LaterSlotVarDefn = ml_gen_mlds_var_decl(mlds_data_var(LaterSlotVar),
        MLDS_IntType, gc_no_stmt, MLDS_Context),
    ml_gen_var_lval(!.Info, LaterSlotVar, MLDS_IntType, LaterSlotVarLval),
    LaterSlotVarRval = ml_lval(LaterSlotVarLval),

    ml_gen_info_new_aux_var_name("limit", LimitVar, !Info),
    % We never need to trace ints.
    LimitVarDefn = ml_gen_mlds_var_decl(mlds_data_var(LimitVar),
        MLDS_IntType, gc_no_stmt, MLDS_Context),
    ml_gen_var_lval(!.Info, LimitVar, MLDS_IntType, LimitVarLval),
    LimitVarRval = ml_lval(LimitVarLval),

    ml_gen_info_get_var_types(!.Info, VarTypes),
    list.map(map.lookup(VarTypes), OutVars, FieldTypes),
    MLDS_FieldTypes =
        list.map(mercury_type_to_mlds_type(ModuleInfo), FieldTypes),
    FirstSolnFieldTypes = [MLDS_IntType, MLDS_IntType | MLDS_FieldTypes],
    LaterSolnFieldTypes = MLDS_FieldTypes,

    ml_gen_call_current_success_cont(Context, CallContStatement, !Info),

    ml_gen_info_get_global_data(!.Info, GlobalData0),
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        FirstSolnFieldTypes, FirstSolnStructTypeNum, FirstSolnStructType,
        FirstSolnTableFieldIds, GlobalData0, GlobalData1),
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        LaterSolnFieldTypes, LaterSolnStructTypeNum, LaterSolnStructType,
        LaterSolnFieldIds, GlobalData1, GlobalData2),
    (
        ( FirstSolnTableFieldIds = []
        ; FirstSolnTableFieldIds = [_]
        ),
        unexpected(this_file,
            "ml_gen_model_non_lookup_switch: not enough field_ids")
    ;
        FirstSolnTableFieldIds =
            [NumLaterSolnsFieldId, FirstLaterRowFieldId | FirstSolnFieldIds]
    ),

    ml_construct_model_non_switch_vector(ModuleInfo, StartVal, EndVal,
        0, CaseSolns,
        FirstSolnStructType, LaterSolnStructType, LaterSolnFieldTypes,
        FirstSolnRowInitializers, LaterSolnRowInitializers,
        no, HadDummyRows),

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
    % We must use LaterSolnFieldTypes here, since we handle the assignments
    % from the first two fields separately.
    ml_generate_field_assigns(OutVars, LaterSolnFieldTypes, FirstSolnFieldIds,
        FirstSolnVectorCommon, FirstSolnStructType, IndexRval,
        MLDS_Context, FirstSolnLookupStatements, !Info),
    ml_generate_field_assigns(OutVars, LaterSolnFieldTypes, LaterSolnFieldIds,
        LaterSolnVectorCommon, LaterSolnStructType, LaterSlotVarRval,
        MLDS_Context, LaterSolnLookupStatements, !Info),

    LimitAssignStmt = ml_stmt_atomic(assign(LimitVarLval, 
        ml_binop(int_add, LaterSlotVarRval, NumLaterSolnsVarRval))),
    LimitAssignStatement = statement(LimitAssignStmt, MLDS_Context),

    IncrLaterSlotVarStmt = ml_stmt_atomic(assign(LaterSlotVarLval,
        ml_binop(int_add, LaterSlotVarRval, ml_const(mlconst_int(1))))),
    IncrLaterSlotVarStatement = statement(IncrLaterSlotVarStmt, MLDS_Context),

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
        expect(unify(HadDummyRows, no), this_file,
            "ml_gen_model_non_lookup_switch: bad dont_need_bit_vec_check"),
        InRangeStmt = ml_stmt_block(
            [NumLaterSolnsVarDefn, LaterSlotVarDefn, LimitVarDefn],
            [NumLaterSolnsLookupStatement | OneOrMoreSolnsStatements]),
        InRangeStatement = statement(InRangeStmt, MLDS_Context)
    ;
        NeedBitVecCheck = need_bit_vec_check,
        expect(unify(HadDummyRows, yes), this_file,
            "ml_gen_model_non_lookup_switch: bad need_bit_vec_check"),

        OneOrMoreSolnsBlockStmt = ml_stmt_block([], OneOrMoreSolnsStatements),
        OneOrMoreSolnsBlockStatement =
            statement(OneOrMoreSolnsBlockStmt, MLDS_Context),

        AnySolnsCond = ml_binop(int_ge,
            NumLaterSolnsVarRval, ml_const(mlconst_int(0))),
        ZeroOrMoreSolnsStmt = ml_stmt_if_then_else(AnySolnsCond,
            OneOrMoreSolnsBlockStatement, no),
        ZeroOrMoreSolnsStatement =
            statement(ZeroOrMoreSolnsStmt, MLDS_Context),

        InRangeStmt = ml_stmt_block(
            [NumLaterSolnsVarDefn, LaterSlotVarDefn, LimitVarDefn],
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
    list(mlds_initializer)::out, list(mlds_initializer)::out,
    bool::in, bool::out) is det.

ml_construct_model_non_switch_vector(ModuleInfo, CurIndex, EndVal,
        NextLaterSolnRow, [],
        FirstSolnStructType, LaterSolnStructType, FieldTypes,
        FirstSolnRowInitializers, LaterSolnRowInitializers, !HadDummyRows) :-
    ( CurIndex > EndVal ->
        FirstSolnRowInitializers = [],
        LaterSolnRowInitializers = []
    ;
        !:HadDummyRows = yes,
        FieldRvals = list.map(ml_default_value_for_type, FieldTypes),
        FieldInitializers = list.map(wrap_init_obj, FieldRvals),
        NumLaterSolnsInitializer = gen_init_int(-1),
        FirstLaterSlotInitializer = gen_init_int(-1),
        FirstSolnFieldInitializers =
            [NumLaterSolnsInitializer, FirstLaterSlotInitializer
                | FieldInitializers],
        FirstSolnRowInitializer =
            init_struct(FirstSolnStructType, FirstSolnFieldInitializers),
        ml_construct_model_non_switch_vector(ModuleInfo, CurIndex + 1, EndVal,
            NextLaterSolnRow, [],
            FirstSolnStructType, LaterSolnStructType, FieldTypes,
            FirstSolnRowInitializersTail, LaterSolnRowInitializers,
            !HadDummyRows),
        FirstSolnRowInitializers =
            [FirstSolnRowInitializer | FirstSolnRowInitializersTail]
    ).
ml_construct_model_non_switch_vector(ModuleInfo, CurIndex, EndVal,
        NextLaterSolnRow, [Pair | Pairs],
        FirstSolnStructType, LaterSolnStructType, FieldTypes,
        FirstSolnRowInitializers, LaterSolnRowInitializers, !HadDummyRows) :-
    Pair = Index - Soln,
    ( CurIndex < Index ->
        !:HadDummyRows = yes,
        FieldRvals = list.map(ml_default_value_for_type, FieldTypes),
        FieldInitializers = list.map(wrap_init_obj, FieldRvals),
        NumLaterSolnsInitializer = gen_init_int(-1),
        FirstLaterSlotInitializer = gen_init_int(-1),
        FirstSolnFieldInitializers =
            [NumLaterSolnsInitializer, FirstLaterSlotInitializer
                | FieldInitializers],
        FirstSolnRowInitializer =
            init_struct(FirstSolnStructType, FirstSolnFieldInitializers),
        ml_construct_model_non_switch_vector(ModuleInfo, CurIndex + 1, EndVal,
            NextLaterSolnRow, [Pair | Pairs],
            FirstSolnStructType, LaterSolnStructType, FieldTypes,
            FirstSolnRowInitializersTail, LaterSolnRowInitializers,
            !HadDummyRows),
        FirstSolnRowInitializers =
            [FirstSolnRowInitializer | FirstSolnRowInitializersTail]
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
            ml_construct_model_non_switch_vector(ModuleInfo,
                CurIndex + 1, EndVal, NextLaterSolnRow, Pairs,
                FirstSolnStructType, LaterSolnStructType, FieldTypes,
                FirstSolnRowInitializersTail, LaterSolnRowInitializers,
                !HadDummyRows),
            FirstSolnRowInitializers =
                [FirstSolnRowInitializer | FirstSolnRowInitializersTail]
        ;
            Soln = several_solns(FirstSolnRvals, LaterSolns),
            FieldInitializers = list.map(wrap_init_obj, FirstSolnRvals),
            list.length(LaterSolns, NumLaterSolns),
            NumLaterSolnsInitializer = gen_init_int(NumLaterSolns),
            FirstLaterSlotInitializer = gen_init_int(NextLaterSolnRow),
            FirstSolnFieldInitializers =
                [NumLaterSolnsInitializer, FirstLaterSlotInitializer
                    | FieldInitializers],
            HeadLaterSolnRowInitializers = list.map(
                ml_construct_later_soln_row(LaterSolnStructType),
                LaterSolns),
            FirstSolnRowInitializer =
                init_struct(FirstSolnStructType, FirstSolnFieldInitializers),
            ml_construct_model_non_switch_vector(ModuleInfo,
                CurIndex + 1, EndVal, NextLaterSolnRow + NumLaterSolns, Pairs,
                FirstSolnStructType, LaterSolnStructType, FieldTypes,
                FirstSolnRowInitializersTail, LaterSolnRowInitializersTail,
                !HadDummyRows),
            FirstSolnRowInitializers =
                [FirstSolnRowInitializer | FirstSolnRowInitializersTail],
            LaterSolnRowInitializers =
                HeadLaterSolnRowInitializers ++ LaterSolnRowInitializersTail
        )
    ).

:- func ml_construct_later_soln_row(mlds_type, list(mlds_rval)) =
    mlds_initializer.

ml_construct_later_soln_row(StructType, Rvals) = RowInitializer :-
    FieldInitializers = list.map(wrap_init_obj, Rvals),
    RowInitializer = init_struct(StructType, FieldInitializers).

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

:- pred ml_generate_constants_for_lookup_switch(code_model::in,
    list(prog_var)::in, set(prog_var)::in, list(tagged_case)::in,
    map(int, soln_consts(mlds_rval))::in,
    map(int, soln_consts(mlds_rval))::out,
    ml_gen_info::in, ml_gen_info::out) is semidet.

ml_generate_constants_for_lookup_switch(_CodeModel, _OutVars, _ArmNonLocals,
        [], !IndexMap, !Info).
ml_generate_constants_for_lookup_switch(CodeModel, OutVars, ArmNonLocals,
        [TaggedCase | TaggedCases], !IndexMap, !Info) :-
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
            ml_record_lookup_for_tagged_cons_id(SolnConsts,
                TaggedMainConsId, !IndexMap),
            list.foldl(ml_record_lookup_for_tagged_cons_id(SolnConsts),
                TaggedOtherConsIds, !IndexMap)
        )
    ;
        goal_is_conj_of_unify(ArmNonLocals, Goal),
        ml_generate_constants_for_arm(OutVars, Goal, Soln, !Info),
        SolnConsts = one_soln(Soln),
        ml_record_lookup_for_tagged_cons_id(SolnConsts,
            TaggedMainConsId, !IndexMap),
        list.foldl(ml_record_lookup_for_tagged_cons_id(SolnConsts),
            TaggedOtherConsIds, !IndexMap)
    ),
    ml_generate_constants_for_lookup_switch(CodeModel, OutVars, ArmNonLocals,
        TaggedCases, !IndexMap, !Info).

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

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "ml_lookup_switch.m".

%-----------------------------------------------------------------------------%
