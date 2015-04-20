%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ml_string_switch.m.
% Authors: fjh, zs.
%
% For switches on strings, we can generate
%
% - a trie;
% - a hash table using open addressing to resolve hash conflicts; or
% - a sorted table for binary search.
%
% The hash table has a higher startup cost than binary search, but should use
% fewer comparisons, so it is preferred for bigger tables. The trie approach
% does not need any startup code and examines each character just once,
% so it does the least work, but it does do a hard-to-predict jump
% for each character in the trie (which usually *won't* be all the characters
% in the string being switched on).
%
% All of these techniques are implemented both for switches in which
% each arm executes code (we call these the "jump" versions, since the task
% of the indexing method is to decide which piece of code to jump to),
% and for switches in which each arm looks up the data to return in tables
% (we call these the "lookup" versions). The lookup versions themselves
% come in two distinct flavours: those in which each arm has at most
% one solution, and those in which some arms gave more than one soluion.
%
% WARNING: the code here is quite similar to the code in string_switch.m.
% Any changes here may require similar changes there and vice versa.
%
%-----------------------------------------------------------------------------%

:- module ml_backend.ml_string_switch.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.ml_lookup_switch.
:- import_module ml_backend.mlds.
:- import_module parse_tree.prog_data.

:- import_module list.

:- pred ml_generate_string_trie_jump_switch(mlds_rval::in,
    list(tagged_case)::in, code_model::in, can_fail::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

:- pred ml_generate_string_trie_lookup_switch(mlds_rval::in,
    list(tagged_case)::in, ml_lookup_switch_info::in,
    code_model::in, can_fail::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

:- pred ml_generate_string_hash_jump_switch(mlds_rval::in,
    list(tagged_case)::in, code_model::in, can_fail::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

:- pred ml_generate_string_hash_lookup_switch(mlds_rval::in,
    list(tagged_case)::in, ml_lookup_switch_info::in,
    code_model::in, can_fail::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

:- pred ml_generate_string_binary_jump_switch(mlds_rval::in,
    list(tagged_case)::in, code_model::in, can_fail::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

:- pred ml_generate_string_binary_lookup_switch(mlds_rval::in,
    list(tagged_case)::in, ml_lookup_switch_info::in,
    code_model::in, can_fail::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.string_encoding.
:- import_module backend_libs.switch_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.ml_simplify_switch.
:- import_module ml_backend.ml_target_util.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.builtin_lib_types.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module unit.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% The implementation of jump tries.
%

ml_generate_string_trie_jump_switch(VarRval, TaggedCases, CodeModel, CanFail,
        Context, Defns, Statements, !Info) :-
    gen_tagged_case_codes_for_string_switch(CodeModel, TaggedCases,
        map.init, CodeMap, !Info),
    MLDS_Context = mlds_make_context(Context),
    create_nested_switch_trie(TaggedCases, MLDS_Context, VarRval, MaxCaseNum,
        CaseNumVarLval, CaseNumVarDefn,
        InitCaseNumVarStatement, GetCaseNumSwitchStatement, !Info),

    map.to_assoc_list(CodeMap, CodeCases),
    generate_trie_arms(CodeCases, [], RevCaseNumSwitchArms),
    list.reverse(RevCaseNumSwitchArms, CaseNumSwitchArms),
    ml_gen_maybe_switch_failure(CodeModel, CanFail, Context, FailStatements,
        !Info),
    (
        FailStatements = [],
        (
            CodeModel = model_det,
            % For model_det switches, the default must be unreachable.
            CaseNumDefault = default_is_unreachable
        ;
            CodeModel = model_semi,
            % For model_semi switches, the default should contain at least
            % an assignment of FALSE to the "succeeded" flag.
            unexpected($module, $pred, "failure does not assign to succeeded")
        ;
            CodeModel = model_non,
            FailStmt = ml_stmt_block([], []),
            FailStatement = statement(FailStmt, MLDS_Context),
            CaseNumDefault = default_case(FailStatement)
        )
    ;
        (
            FailStatements = [FailStatement]
        ;
            FailStatements = [_, _ | _],
            FailStmt = ml_stmt_block([], FailStatements),
            FailStatement = statement(FailStmt, MLDS_Context)
        ),
        CaseNumDefault = default_case(FailStatement)
    ),
    CaseNumSwitchRange = mlds_switch_range(0, MaxCaseNum),
    CaseSwitchStmt = ml_stmt_switch(mlds_native_int_type,
        ml_lval(CaseNumVarLval), CaseNumSwitchRange, CaseNumSwitchArms,
        CaseNumDefault),
    CaseSwitchStatement = statement(CaseSwitchStmt, MLDS_Context),

    Stmt = ml_stmt_block([CaseNumVarDefn],
        [InitCaseNumVarStatement, GetCaseNumSwitchStatement,
        CaseSwitchStatement]),
    Statements = [statement(Stmt, MLDS_Context)],
    Defns = [].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% The implementation of lookup tries.
%

ml_generate_string_trie_lookup_switch(VarRval, TaggedCases, LookupSwitchInfo,
        CodeModel, CanFail, Context, Defns, Statements, !Info) :-
    MLDS_Context = mlds_make_context(Context),
    create_nested_switch_trie(TaggedCases, MLDS_Context, VarRval, MaxCaseNum,
        CaseNumVarLval, CaseNumVarDefn,
        InitCaseNumVarStatement, GetCaseNumSwitchStatement, !Info),
    LookupSwitchInfo = ml_lookup_switch_info(CaseIdConsts, OutVars, OutTypes),
    (
        CaseIdConsts = all_one_soln(CaseIdValueMap),
        map.to_assoc_list(CaseIdValueMap, CaseIdValues),
        ml_generate_string_trie_simple_lookup_switch(MaxCaseNum,
            CaseNumVarLval, CaseNumVarDefn, InitCaseNumVarStatement,
            GetCaseNumSwitchStatement, CaseIdValues, OutVars, OutTypes,
            CodeModel, CanFail, Context, Defns, Statements, !Info)
    ;
        CaseIdConsts = some_several_solns(CaseIdSolnMap, _Unit),
        expect(unify(CodeModel, model_non), $module, $pred,
            "CodeModel != model_non"),
        map.to_assoc_list(CaseIdSolnMap, CaseIdSolns),
        ml_generate_string_trie_several_soln_lookup_switch(MaxCaseNum,
            CaseNumVarLval, CaseNumVarDefn, InitCaseNumVarStatement,
            GetCaseNumSwitchStatement, CaseIdSolns, OutVars, OutTypes,
            CanFail, Context, Defns, Statements, !Info)
    ).

%-----------------------------------------------------------------------------%

:- pred ml_generate_string_trie_simple_lookup_switch(int::in,
    mlds_lval::in, mlds_defn::in, statement::in, statement::in,
    assoc_list(case_id, list(mlds_rval))::in,
    list(prog_var)::in, list(mlds_type)::in,
    code_model::in, can_fail::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_string_trie_simple_lookup_switch(MaxCaseNum,
        CaseNumVarLval, CaseNumVarDefn, InitCaseNumVarStatement,
        GetCaseNumSwitchStatement, CaseIdValues, OutVars, OutTypes,
        CodeModel, CanFail, Context, Defns, Statements, !Info) :-
    MLDS_Context = mlds_make_context(Context),
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_gen_info_get_target(!.Info, Target),

    (
        OutTypes = [_ | _],
        ml_gen_info_get_global_data(!.Info, GlobalData0),
        ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
            OutTypes, StructTypeNum, StructType, OutFieldIds,
            GlobalData0, GlobalData1),
        ml_gen_string_trie_simple_lookup_slots(StructType, CaseIdValues,
            0, AfterLastCaseNum, [], RevRowInitializers),
        expect(unify(MaxCaseNum + 1, AfterLastCaseNum), $module, $pred,
            "MaxCaseNum + 1 != AfterLastCaseNum"),
        list.reverse(RevRowInitializers, RowInitializers),
        ml_gen_static_vector_defn(MLDS_ModuleName, StructTypeNum,
            RowInitializers, VectorCommon, GlobalData1, GlobalData),
        ml_gen_info_set_global_data(GlobalData, !Info),

        ml_generate_field_assigns(OutVars, OutTypes, OutFieldIds, VectorCommon,
            StructType, ml_lval(CaseNumVarLval), MLDS_Context,
            FieldAssignStatements, !Info)
    ;
        OutTypes = [],
        FieldAssignStatements = []
    ),

    FoundMatchComment = "we found a match; look up the results",
    FoundMatchCommentStatement = statement(
        ml_stmt_atomic(comment(FoundMatchComment)), MLDS_Context),
    CommentedFieldAssignStatements =
        [FoundMatchCommentStatement | FieldAssignStatements],
    (
        CodeModel = model_det,
        LookupStatements = CommentedFieldAssignStatements
    ;
        CodeModel = model_semi,
        ml_gen_set_success(!.Info, ml_const(mlconst_true), Context,
            SetSuccessTrueStatement),
        LookupStatements = CommentedFieldAssignStatements ++
            [SetSuccessTrueStatement]
    ;
        CodeModel = model_non,
        unexpected($module, $pred, "model_non")
    ),
    LookupStmt = ml_stmt_block([], LookupStatements),
    LookupStatement = statement(LookupStmt, MLDS_Context),

    ml_gen_maybe_switch_failure(CodeModel, CanFail, Context, FailStatements,
        !Info),
    (
        FailStatements = [],
        ResultStatement = LookupStatement
    ;
        (
            FailStatements = [FailStatement]
        ;
            FailStatements = [_, _ | _],
            FailStmt = ml_stmt_block([], FailStatements),
            FailStatement = statement(FailStmt, MLDS_Context)
        ),
        IsCaseNumNegCond = ml_binop(int_lt, ml_lval(CaseNumVarLval),
            ml_const(mlconst_int(0))),
        ResultStmt = ml_stmt_if_then_else(IsCaseNumNegCond, FailStatement,
            yes(LookupStatement)),
        ResultStatement = statement(ResultStmt, MLDS_Context)
    ),

    Stmt = ml_stmt_block([CaseNumVarDefn],
        [InitCaseNumVarStatement, GetCaseNumSwitchStatement, ResultStatement]),
    Statements = [statement(Stmt, MLDS_Context)],
    Defns = [].

:- pred ml_gen_string_trie_simple_lookup_slots(mlds_type::in,
    assoc_list(case_id, list(mlds_rval))::in, int::in, int::out,
    list(mlds_initializer)::in, list(mlds_initializer)::out) is det.

ml_gen_string_trie_simple_lookup_slots(_StructType, [],
        !CurCaseNum, !RevRowInitializers).
ml_gen_string_trie_simple_lookup_slots(StructType,
        [CaseIdValue | CaseIdValues], !CurCaseNum, !RevRowInitializers) :-
    CaseIdValue = CaseId - OutRvals,
    CaseId = case_id(CaseIdNum),
    expect(unify(CaseIdNum, !.CurCaseNum), $module, $pred,
        "CaseIdNum != !.CurCaseNum"),
    OutInitializers = list.map(wrap_init_obj, OutRvals),
    RowInitializer = init_struct(StructType, OutInitializers),
    !:RevRowInitializers = [RowInitializer | !.RevRowInitializers],
    !:CurCaseNum = !.CurCaseNum + 1,
    ml_gen_string_trie_simple_lookup_slots(StructType, CaseIdValues,
        !CurCaseNum, !RevRowInitializers).

%-----------------------------------------------------------------------------%

:- pred ml_generate_string_trie_several_soln_lookup_switch(int::in,
    mlds_lval::in, mlds_defn::in, statement::in, statement::in,
    assoc_list(case_id, soln_consts(mlds_rval))::in,
    list(prog_var)::in, list(mlds_type)::in, can_fail::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_string_trie_several_soln_lookup_switch(MaxCaseNum,
        CaseNumVarLval, CaseNumVarDefn, InitCaseNumVarStatement,
        GetCaseNumSwitchStatement, CaseIdSolns, OutVars, OutTypes,
        CanFail, Context, Defns, Statements, !Info) :-
    MLDS_Context = mlds_make_context(Context),
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_gen_info_get_target(!.Info, Target),

    ml_gen_info_get_global_data(!.Info, GlobalData0),
    MLDS_IntType = mlds_native_int_type,
    FirstSolnFieldTypes = [MLDS_IntType, MLDS_IntType | OutTypes],
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        FirstSolnFieldTypes, FirstSolnStructTypeNum, FirstSolnStructType,
        FirstSolnFieldIds, GlobalData0, GlobalData1),
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        OutTypes, LaterSolnStructTypeNum, LaterSolnStructType,
        LaterSolnOutFieldIds, GlobalData1, GlobalData2),
    (
        FirstSolnFieldIds =
            [NumLaterSolnsFieldIdPrime, FirstLaterSolnRowFieldIdPrime |
            FirstSolnOutFieldIdsPrime]
    ->
        NumLaterSolnsFieldId = NumLaterSolnsFieldIdPrime,
        FirstLaterSolnRowFieldId = FirstLaterSolnRowFieldIdPrime,
        FirstSolnOutFieldIds = FirstSolnOutFieldIdsPrime
    ;
        unexpected($module, $pred, "bad FieldIds")
    ),

    ml_gen_string_trie_several_soln_lookup_slots(
        FirstSolnStructType, LaterSolnStructType, CaseIdSolns,
        0, AfterLastCaseNum, 0, [], RevFirstSolnRowInitializers,
        cord.init, LaterSolnRowInitializerCord),
    expect(unify(MaxCaseNum + 1, AfterLastCaseNum), $module, $pred,
        "MaxCaseNum + 1 != AfterLastCaseNum"),
    list.reverse(RevFirstSolnRowInitializers, FirstSolnRowInitializers),
    LaterSolnRowInitializers = cord.list(LaterSolnRowInitializerCord),
    ml_gen_static_vector_defn(MLDS_ModuleName, FirstSolnStructTypeNum,
        FirstSolnRowInitializers, FirstSolnVectorCommon,
        GlobalData2, GlobalData3),
    ml_gen_static_vector_defn(MLDS_ModuleName, LaterSolnStructTypeNum,
        LaterSolnRowInitializers, LaterSolnVectorCommon,
        GlobalData3, GlobalData),
    ml_gen_info_set_global_data(GlobalData, !Info),

    ml_gen_several_soln_lookup_code(Context, MLDS_Context,
        ml_lval(CaseNumVarLval), OutVars, OutTypes,
        FirstSolnStructType, LaterSolnStructType,
        NumLaterSolnsFieldId, FirstLaterSolnRowFieldId,
        FirstSolnOutFieldIds, LaterSolnOutFieldIds,
        FirstSolnVectorCommon, LaterSolnVectorCommon, dont_need_bit_vec_check,
        MatchDefns, SuccessStatements, !Info),
    SuccessBlockStmt = ml_stmt_block(MatchDefns, SuccessStatements),
    SuccessBlockStatement = statement(SuccessBlockStmt, MLDS_Context),
    (
        CanFail = can_fail,
        IsCaseNumNonNegCond = ml_binop(int_ge, ml_lval(CaseNumVarLval),
            ml_const(mlconst_int(0))),
        ResultStmt = ml_stmt_if_then_else(IsCaseNumNonNegCond,
            SuccessBlockStatement, no),
        ResultStatement = statement(ResultStmt, MLDS_Context)
    ;
        CanFail = cannot_fail,
        ResultStatement = SuccessBlockStatement
    ),
    Stmt = ml_stmt_block([CaseNumVarDefn],
        [InitCaseNumVarStatement, GetCaseNumSwitchStatement, ResultStatement]),
    Statements = [statement(Stmt, MLDS_Context)],
    Defns = [].

:- pred ml_gen_string_trie_several_soln_lookup_slots(
    mlds_type::in, mlds_type::in,
    assoc_list(case_id, soln_consts(mlds_rval))::in, int::in, int::out,
    int::in,
    list(mlds_initializer)::in, list(mlds_initializer)::out,
    cord(mlds_initializer)::in, cord(mlds_initializer)::out) is det.

ml_gen_string_trie_several_soln_lookup_slots(
        _FirstSolnStructType, _LaterSolnStructType, [], !CurCaseNum,
        _CurLaterSolnIndex,
        !RevFirstSolnRowInitializers, !LaterSolnRowInitializersCord).
ml_gen_string_trie_several_soln_lookup_slots(
        FirstSolnStructType, LaterSolnStructType,
        [CaseIdSolns | CaseIdsSolns], !CurCaseNum, !.CurLaterSolnIndex,
        !RevFirstSolnRowInitializers, !LaterSolnRowInitializersCord) :-
    CaseIdSolns = CaseId - Solns,
    CaseId = case_id(CaseIdNum),
    expect(unify(CaseIdNum, !.CurCaseNum), $module, $pred,
        "CaseIdNum != !.CurCaseNum"),
    (
        Solns = one_soln(FirstSolnRvals),
        NumLaterSolnsRval = ml_const(mlconst_int(0)),
        FirstLaterSlotRval = ml_const(mlconst_int(-1))
    ;
        Solns = several_solns(FirstSolnRvals, LaterSolns),
        list.length(LaterSolns, NumLaterSolns),
        NumLaterSolnsRval = ml_const(mlconst_int(NumLaterSolns)),
        FirstLaterSlotRval = ml_const(mlconst_int(!.CurLaterSolnIndex)),
        LaterSolnRowInitializers = list.map(
            ml_construct_later_soln_row(LaterSolnStructType),
            LaterSolns),
        !:LaterSolnRowInitializersCord = !.LaterSolnRowInitializersCord ++
            cord.from_list(LaterSolnRowInitializers),
        !:CurLaterSolnIndex = !.CurLaterSolnIndex + NumLaterSolns
    ),
    FirstSolnRowRvals =
        [NumLaterSolnsRval, FirstLaterSlotRval | FirstSolnRvals],
    FirstSolnRowInitializer = init_struct(FirstSolnStructType,
        list.map(wrap_init_obj, FirstSolnRowRvals)),
    !:RevFirstSolnRowInitializers =
        [FirstSolnRowInitializer | !.RevFirstSolnRowInitializers],
    !:CurCaseNum = !.CurCaseNum + 1,
    ml_gen_string_trie_several_soln_lookup_slots(
        FirstSolnStructType, LaterSolnStructType,
        CaseIdsSolns, !CurCaseNum, !.CurLaterSolnIndex,
        !RevFirstSolnRowInitializers, !LaterSolnRowInitializersCord).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Code useful for both the jump and lookup versions of tries.
%

:- type trie_node
    --->    trie_leaf(
                leaf_matched        :: list(int),
                % The already matched code units, in reverse order.

                leaf_unmatched      :: list(int),
                % The not-yet matched code units, in forward order.
                % Invariant: applying from_code_unit_list to
                % list.reverse(leaf_matched) ++ leaf_unmatched
                % should yield the original string.

                case_id
                % The case_id of the switch arm.
            )
    ;       trie_choice(
                choice_matched      :: list(int),
                % The already matched code units, in reverse order.

                choice_next_level   :: map(int, trie_node),
                % Maps the next code unit to the trie node reachable
                % through it.

                choice_end          :: maybe(case_id)
                % The case number of the switch arm whose string ends here,
                % if there is one.
            ).

:- pred create_nested_switch_trie(list(tagged_case)::in, mlds_context::in,
    mlds_rval::in, int::out, mlds_lval::out, mlds_defn::out,
    statement::out, statement::out, ml_gen_info::in, ml_gen_info::out) is det.

create_nested_switch_trie(TaggedCases, MLDS_Context, VarRval, MaxCaseNum,
        CaseNumVarLval, CaseNumVarDefn,
        InitCaseNumVarStatement, GetCaseNumSwitchStatement, !Info) :-
    ml_gen_info_get_target(!.Info, Target),
    Encoding = target_string_encoding(Target),
    create_trie(Encoding, TaggedCases, MaxCaseNum, TopTrieNode),
    ml_gen_trie_case_num_var_and_init(MLDS_Context, CaseNumVarLval,
        CaseNumVarDefn, InitCaseNumVarStatement, !Info),
    convert_trie_to_nested_switches(Encoding, VarRval, CaseNumVarLval,
        MLDS_Context, 0, TopTrieNode, GetCaseNumSwitchStatement).

%-----------------------------------------------------------------------------%

:- pred create_trie(string_encoding::in, list(tagged_case)::in, int::out,
    trie_node::out) is det.

create_trie(Encoding, TaggedCases, MaxCaseNum, TopTrieNode) :-
    build_str_case_id_assoc_list(TaggedCases, -1, MaxCaseNum, [], StrsCaseIds),
    % The order of StrsCaseIds does not matter; we will build the same trie
    % regardless of the order.
    (
        StrsCaseIds = [],
        TopTrieNode = trie_choice([], map.init, no)
    ;
        StrsCaseIds = [HeadStrCaseId | TailStrCaseIds],
        HeadStrCaseId = HeadStr - HeadCaseId,
        to_code_unit_list(Encoding, HeadStr, HeadStrCodeUnits),
        TopTrieNode1 = trie_leaf([], HeadStrCodeUnits, HeadCaseId),
        insert_cases_into_trie(Encoding, TailStrCaseIds, TopTrieNode1,
            TopTrieNode)
    ).

:- pred insert_cases_into_trie(string_encoding::in,
    assoc_list(string, case_id)::in, trie_node::in, trie_node::out) is det.

insert_cases_into_trie(_Encoding, [], !TrieNode).
insert_cases_into_trie(Encoding, [Case | Cases], !TrieNode) :-
    Case = Str - CaseId,
    to_code_unit_list(Encoding, Str, StrCodeUnits),
    insert_case_into_trie_node([], StrCodeUnits, CaseId, !TrieNode),
    insert_cases_into_trie(Encoding, Cases, !TrieNode).

:- pred insert_case_into_trie_node(list(int)::in, list(int)::in, case_id::in,
    trie_node::in, trie_node::out) is det.

insert_case_into_trie_node(InsertMatched, InsertNotYetMatched, InsertCaseId,
        TrieNode0, TrieNode) :-
    (
        TrieNode0 = trie_leaf(LeafMatched, LeafNotYetMatched, LeafCaseId),
        expect(unify(LeafMatched, InsertMatched), $module, $pred,
            "LeafMatched didn't"),
        (
            LeafNotYetMatched = [],
            ChoiceMap0 = map.init,
            MaybeEnd0 = yes(LeafCaseId)
        ;
            LeafNotYetMatched = [LeafFirstCodeUnit | LeafLaterCodeUnits],
            NewLeaf = trie_leaf([LeafFirstCodeUnit | LeafMatched],
                LeafLaterCodeUnits, LeafCaseId),
            ChoiceMap0 = map.singleton(LeafFirstCodeUnit, NewLeaf),
            MaybeEnd0 = no
        )
    ;
        TrieNode0 = trie_choice(ChoiceMatched, ChoiceMap0, MaybeEnd0),
        expect(unify(ChoiceMatched, InsertMatched), $module, $pred,
            "ChoiceMatched didn't")
    ),
    insert_case_into_trie_choice(InsertMatched, InsertNotYetMatched,
        InsertCaseId, ChoiceMap0, ChoiceMap, MaybeEnd0, MaybeEnd),
    TrieNode = trie_choice(InsertMatched, ChoiceMap, MaybeEnd).

:- pred insert_case_into_trie_choice(list(int)::in, list(int)::in, case_id::in,
    map(int, trie_node)::in, map(int, trie_node)::out,
    maybe(case_id)::in, maybe(case_id)::out) is det.

insert_case_into_trie_choice(InsertMatched, InsertNotYetMatched, InsertCaseId,
        ChoiceMap0, ChoiceMap, MaybeEnd0, MaybeEnd) :-
    (
        InsertNotYetMatched = [],
        ChoiceMap = ChoiceMap0,
        (
            MaybeEnd0 = no,
            MaybeEnd = yes(InsertCaseId)
        ;
            MaybeEnd0 = yes(_),
            % You can't have more than one occurrence of a string
            % as a cons_id in a switch.
            unexpected($module, $pred, "two strings end at same trie node")
        )
    ;
        InsertNotYetMatched = [InsertFirstCodeUnit | InsertLaterCodeUnits],
        MaybeEnd = MaybeEnd0,
        ( if map.search(ChoiceMap0, InsertFirstCodeUnit, SubTrieNode0) then
            insert_case_into_trie_node([InsertFirstCodeUnit | InsertMatched],
                InsertLaterCodeUnits, InsertCaseId, SubTrieNode0, SubTrieNode),
            map.det_update(InsertFirstCodeUnit, SubTrieNode,
                ChoiceMap0, ChoiceMap)
        else
            SubTrieNode = trie_leaf([InsertFirstCodeUnit | InsertMatched],
                InsertLaterCodeUnits, InsertCaseId),
            map.det_insert(InsertFirstCodeUnit, SubTrieNode,
                ChoiceMap0, ChoiceMap)
        )
    ).

    % Generate the following local variable declaration:
    %   int         case_num = -1;
    %
:- pred ml_gen_trie_case_num_var_and_init(mlds_context::in,
    mlds_lval::out, mlds_defn::out, statement::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_trie_case_num_var_and_init(MLDS_Context, CaseNumVarLval, CaseNumVarDefn,
        InitStatement, !Info) :-
    ml_gen_info_new_aux_var_name("case_num", CaseNumVar, !Info),
    CaseNumVarType = mlds_native_int_type,
    % We never need to trace ints.
    CaseNumVarDefn = ml_gen_mlds_var_decl(mlds_data_var(CaseNumVar),
        CaseNumVarType, gc_no_stmt, MLDS_Context),
    ml_gen_var_lval(!.Info, CaseNumVar, CaseNumVarType, CaseNumVarLval),

    InitAssign = assign(CaseNumVarLval, ml_const(mlconst_int(-1))),
    InitStmt = ml_stmt_atomic(InitAssign),
    InitStatement = statement(InitStmt, MLDS_Context).

%-----------------------------------------------------------------------------%

:- pred convert_trie_to_nested_switches(string_encoding::in, mlds_rval::in,
    mlds_lval::in, mlds_context::in, int::in, trie_node::in, statement::out)
    is det.

convert_trie_to_nested_switches(Encoding, VarRval, CaseNumVarLval, Context,
        NumMatched, TrieNode, Statement) :-
    (
        TrieNode = trie_leaf(RevMatchedCodeUnits, NotYetMatchedCodeUnits,
            CaseId),
        CaseId = case_id(CaseNum),

        CaseNumRval = ml_const(mlconst_int(CaseNum)),
        SetCaseNumVarAssign = assign(CaseNumVarLval, CaseNumRval),
        SetCaseNumVarStmt = ml_stmt_atomic(SetCaseNumVarAssign),
        SetCaseNumVarStatement = statement(SetCaseNumVarStmt, Context),

        AllCodeUnits =
            list.reverse(RevMatchedCodeUnits) ++ NotYetMatchedCodeUnits,
        list.length(RevMatchedCodeUnits, NumRevMatchedCodeUnits),
        expect(unify(NumRevMatchedCodeUnits, NumMatched), $module, $pred,
            "NumRevMatchedCodeUnits != NumMatched"),
        ( if from_code_unit_list(Encoding, AllCodeUnits, String) then
            StringRval = ml_const(mlconst_string(String))
        else
            unexpected($module, $pred,
                "code units cannot be turned back into string")
        ),
        CondRval = ml_binop(offset_str_eq(NumMatched), VarRval, StringRval),
        Stmt = ml_stmt_if_then_else(CondRval, SetCaseNumVarStatement, no)
    ;
        TrieNode = trie_choice(RevMatchedCodeUnits, ChoiceMap, MaybeEnd),
        CurCodeUnitRval = ml_binop(string_unsafe_index_code_unit,
            VarRval, ml_const(mlconst_int(NumMatched))),
        map.to_assoc_list(ChoiceMap, ChoicePairs),
        ( if
            ChoicePairs = [OneChoicePair],
            MaybeEnd = no
        then
            OneChoicePair = OneCodeUnit - OneSubTrieNode,
            OneCodeUnitConst = ml_const(mlconst_int(OneCodeUnit)),
            FirstCond = ml_binop(eq, CurCodeUnitRval, OneCodeUnitConst),
            chase_one_cond_trie_nodes(Encoding, VarRval, CaseNumVarLval,
                Context, NumMatched + 1, OneSubTrieNode, FirstCond, AllCond,
                ThenStatement),
            Stmt = ml_stmt_if_then_else(AllCond, ThenStatement, no)
        else
            convert_trie_choices_to_nested_switches(Encoding, VarRval,
                CaseNumVarLval, Context, NumMatched + 1, ChoicePairs,
                cord.init, SwitchArmsCord0),
            SwitchArms0 = cord.list(SwitchArmsCord0),
            (
                MaybeEnd = no,
                SwitchArms = SwitchArms0
            ;
                MaybeEnd = yes(EndCaseId),
                EndCaseId = case_id(EndCaseNum),

                EndCaseNumRval = ml_const(mlconst_int(EndCaseNum)),
                EndSetCaseNumVarAssign = assign(CaseNumVarLval, EndCaseNumRval),
                EndSetCaseNumVarStmt = ml_stmt_atomic(EndSetCaseNumVarAssign),
                EndSetCaseNumVarStatement =
                    statement(EndSetCaseNumVarStmt, Context),
                NullCodeUnit = 0,    % Match the terminating NUL character.
                NullMatchCond =
                    match_value(ml_const(mlconst_int(NullCodeUnit))),
                EndSwitchArm = mlds_switch_case(NullMatchCond, [],
                    EndSetCaseNumVarStatement),
                SwitchArms = [EndSwitchArm | SwitchArms0]
            ),
            list.length(RevMatchedCodeUnits, NumRevMatchedCodeUnits),
            expect(unify(NumRevMatchedCodeUnits, NumMatched), $module, $pred,
                "NumRevMatchedCodeUnits != NumMatched"),
            SwitchCodeUnitRval = CurCodeUnitRval,
            % Could we set this to a known range? If we could,
            % would it be useful?
            SwitchRange = mlds_switch_range_unknown,
            Stmt = ml_stmt_switch(mlds_native_int_type, SwitchCodeUnitRval,
                SwitchRange, SwitchArms, default_do_nothing)
        )
    ),
    Statement = statement(Stmt, Context).

:- pred convert_trie_choices_to_nested_switches(string_encoding::in,
    mlds_rval::in, mlds_lval::in, mlds_context::in, int::in,
    assoc_list(int, trie_node)::in,
    cord(mlds_switch_case)::in, cord(mlds_switch_case)::out) is det.

convert_trie_choices_to_nested_switches(_, _, _, _, _, [], !SwitchArmsCord).
convert_trie_choices_to_nested_switches(Encoding, VarRval, CaseNumVarLval,
        Context, NumMatched, [ChoicePair | ChoicePairs], !SwitchArmsCord) :-
    ChoicePair = CodeUnit - SubTrieNode,
    convert_trie_to_nested_switches(Encoding, VarRval, CaseNumVarLval, Context,
        NumMatched, SubTrieNode, SwitchArmStatement),
    MatchCond = match_value(ml_const(mlconst_int(CodeUnit))),
    SwitchArm = mlds_switch_case(MatchCond, [], SwitchArmStatement),
    !:SwitchArmsCord = cord.snoc(!.SwitchArmsCord, SwitchArm),
    convert_trie_choices_to_nested_switches(Encoding, VarRval, CaseNumVarLval,
        Context, NumMatched, ChoicePairs, !SwitchArmsCord).

:- pred generate_trie_arms(assoc_list(case_id, statement)::in,
    list(mlds_switch_case)::in, list(mlds_switch_case)::out) is det.

generate_trie_arms([], !RevSwitchCases).
generate_trie_arms([CasePair | CasePairs], !RevSwitchCases) :-
    CasePair = CaseId - CaseStatement,
    CaseId = case_id(CaseNum),
    MatchCond = match_value(ml_const(mlconst_int(CaseNum))),
    Case = mlds_switch_case(MatchCond, [], CaseStatement),
    !:RevSwitchCases = [Case | !.RevSwitchCases],
    generate_trie_arms(CasePairs, !RevSwitchCases).

:- pred chase_one_cond_trie_nodes(string_encoding::in, mlds_rval::in,
    mlds_lval::in, mlds_context::in, int::in, trie_node::in,
    mlds_rval::in, mlds_rval::out, statement::out) is det.

chase_one_cond_trie_nodes(Encoding, VarRval, CaseNumVarLval, Context,
        NumMatched, TrieNode, RevCond0, RevCond, ThenStatement) :-
    ( if
        TrieNode = trie_choice(RevMatchedCodeUnits, ChoiceMap, MaybeEnd),
        map.to_assoc_list(ChoiceMap, ChoicePairs),
        ChoicePairs = [OneChoicePair],
        MaybeEnd = no
    then
        expect(unify(list.length(RevMatchedCodeUnits), NumMatched),
            $module, $pred, "length(RevMatchedCodeUnits) != NumMatched"),
        OneChoicePair = OneCodeUnit - OneSubTrieNode,
        CurCodeUnitRval = ml_binop(string_unsafe_index_code_unit,
            VarRval, ml_const(mlconst_int(NumMatched))),
        OneCodeUnitConst = ml_const(mlconst_int(OneCodeUnit)),
        CurCond = ml_binop(eq, CurCodeUnitRval, OneCodeUnitConst),
        RevCond1 = ml_binop(logical_and, RevCond0, CurCond),
        chase_one_cond_trie_nodes(Encoding, VarRval, CaseNumVarLval, Context,
            NumMatched + 1, OneSubTrieNode, RevCond1, RevCond, ThenStatement)
    else
        RevCond = RevCond0,
        convert_trie_to_nested_switches(Encoding, VarRval, CaseNumVarLval,
            Context, NumMatched, TrieNode, ThenStatement)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% The implementation of jump hash tables.
%

ml_generate_string_hash_jump_switch(VarRval, TaggedCases, CodeModel, CanFail,
        Context, Defns, Statements, !Info) :-
    MLDS_Context = mlds_make_context(Context),
    gen_tagged_case_codes_for_string_switch(CodeModel, TaggedCases,
        map.init, CodeMap, !Info),
    build_str_case_id_assoc_list(TaggedCases, -1, _MaxCaseNum,
        [], RevStrsCaseIds),
    list.reverse(RevStrsCaseIds, StrsCaseIds),

    % Compute the hash table.
    construct_string_hash_cases(StrsCaseIds, allow_doubling,
        TableSize, HashSlotMap, HashOp, NumCollisions),
    HashMask = TableSize - 1,

    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_gen_info_get_target(!.Info, Target),

    MLDS_StringType = mercury_type_to_mlds_type(ModuleInfo, string_type),
    MLDS_IntType = mlds_native_int_type,

    ( NumCollisions = 0 ->
        MLDS_ArgTypes = [MLDS_StringType],
        LoopPresent = no
    ;
        MLDS_ArgTypes = [MLDS_StringType, MLDS_IntType],
        LoopPresent = yes
    ),

    ml_gen_string_hash_switch_search_vars(CodeModel, CanFail, LoopPresent,
        Context, MLDS_Context, VarRval, HashSearchInfo, !Info),
    HashSearchInfo = ml_hash_search_info(_CodeModel, _LoopPresent,
        _Context, _VarRval, SlotVarLval, _StringVarLval,
        _MaybeStopLoopLval, _FailStatements, Defns),

    ml_gen_info_get_global_data(!.Info, GlobalData0),
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        MLDS_ArgTypes, StructTypeNum, StructType, FieldIds,
        GlobalData0, GlobalData1),
    ( NumCollisions = 0 ->
        ( FieldIds = [StringFieldIdPrime] ->
            StringFieldId = StringFieldIdPrime,
            MaybeNextSlotFieldId = no
        ;
            unexpected($module, $pred, "bad FieldIds")
        )
    ;
        ( FieldIds = [StringFieldIdPrime, NextSlotFieldIdPrime] ->
            StringFieldId = StringFieldIdPrime,
            MaybeNextSlotFieldId = yes(NextSlotFieldIdPrime)
        ;
            unexpected($module, $pred, "bad FieldIds")
        )
    ),
    % Generate the rows of the hash table.
    ml_gen_string_hash_jump_slots(0, TableSize, HashSlotMap, StructType,
        MaybeNextSlotFieldId, [], RevRowInitializers, map.init, RevMap),
    list.reverse(RevRowInitializers, RowInitializers),
    % Generate the hash table.
    ml_gen_static_vector_defn(MLDS_ModuleName, StructTypeNum, RowInitializers,
        VectorCommon, GlobalData1, GlobalData),
    ml_gen_info_set_global_data(GlobalData, !Info),

    % Generate code which does the hash table lookup.
    map.to_assoc_list(RevMap, RevList),
    generate_string_jump_switch_arms(CodeMap, RevList, [], SlotsCases0),
    list.sort(SlotsCases0, SlotsCases),
    SlotVarType = mlds_native_int_type,

    SwitchStmt0 = ml_stmt_switch(SlotVarType, ml_lval(SlotVarLval),
        mlds_switch_range(0, TableSize - 1), SlotsCases,
        default_is_unreachable),
    ml_simplify_switch(SwitchStmt0, MLDS_Context, SwitchStatement, !Info),

    FoundMatchComment = "we found a match; dispatch to the corresponding code",
    FoundMatchStatements = [
        statement(ml_stmt_atomic(comment(FoundMatchComment)), MLDS_Context),
        SwitchStatement
    ],

    InitialComment = "hashed string jump switch",
    ml_gen_string_hash_switch_search(MLDS_Context, InitialComment,
        HashSearchInfo, HashOp, VectorCommon, StructType,
        StringFieldId, MaybeNextSlotFieldId, HashMask,
        [], FoundMatchStatements, Statements, !Info).

%-----------------------------------------------------------------------------%

:- pred build_str_case_id_assoc_list(list(tagged_case)::in, int::in, int::out,
    assoc_list(string, case_id)::in, assoc_list(string, case_id)::out) is det.

build_str_case_id_assoc_list([], !MaxCaseNum, !RevStrsCaseIds).
build_str_case_id_assoc_list([TaggedCase | TaggedCases],
        !MaxCaseNum, !RevStrsCaseIds) :-
    TaggedCase = tagged_case(MainTaggedConsId, OtherTaggedConsIds, CaseId, _),
    CaseId = case_id(CaseNum),
    int.max(CaseNum, !MaxCaseNum),
    add_to_strs_case_ids(CaseId, MainTaggedConsId, !RevStrsCaseIds),
    list.foldl(add_to_strs_case_ids(CaseId),
        OtherTaggedConsIds, !RevStrsCaseIds),
    build_str_case_id_assoc_list(TaggedCases, !MaxCaseNum, !RevStrsCaseIds).

:- pred add_to_strs_case_ids(case_id::in, tagged_cons_id::in,
    assoc_list(string, case_id)::in, assoc_list(string, case_id)::out) is det.

add_to_strs_case_ids(CaseId, TaggedConsId, !RevStrsCaseIds) :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    ( ConsTag = string_tag(String) ->
        !:RevStrsCaseIds = [String - CaseId | !.RevStrsCaseIds]
    ;
        unexpected($module, $pred, "non-string tag")
    ).

%-----------------------------------------------------------------------------%

:- pred gen_tagged_case_codes_for_string_switch(code_model::in,
    list(tagged_case)::in,
    map(case_id, statement)::in, map(case_id, statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_tagged_case_codes_for_string_switch(_CodeModel, [],
        !CodeMap, !Info).
gen_tagged_case_codes_for_string_switch(CodeModel, [TaggedCase | TaggedCases],
        !CodeMap, !Info) :-
    gen_tagged_case_code_for_string_switch(CodeModel, TaggedCase,
        !CodeMap, !Info),
    gen_tagged_case_codes_for_string_switch(CodeModel, TaggedCases,
        !CodeMap, !Info).

:- pred gen_tagged_case_code_for_string_switch_dummy(code_model::in,
    tagged_case::in, case_id::out,
    map(case_id, statement)::in, map(case_id, statement)::out,
    ml_gen_info::in, ml_gen_info::out, unit::in, unit::out) is det.

gen_tagged_case_code_for_string_switch_dummy(CodeModel, TaggedCase, CaseId,
        !CodeMap, !Info, !Dummy) :-
    TaggedCase = tagged_case(_, _, CaseId, _),
    gen_tagged_case_code_for_string_switch(CodeModel, TaggedCase,
        !CodeMap, !Info).

:- pred gen_tagged_case_code_for_string_switch(code_model::in, tagged_case::in,
    map(case_id, statement)::in, map(case_id, statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_tagged_case_code_for_string_switch(CodeModel, TaggedCase,
        !CodeMap, !Info) :-
    TaggedCase = tagged_case(MainTaggedConsId, OtherTaggedConsIds,
        CaseId, Goal),
    ml_gen_goal_as_branch_block(CodeModel, Goal, GoalStatement, !Info),
    MainString = gen_string_switch_case_comment(MainTaggedConsId),
    OtherStrings = list.map(gen_string_switch_case_comment,
        OtherTaggedConsIds),
    Strings = string.join_list(", ", [MainString | OtherStrings]),
    % Note that the order of the strings in the comment will in general
    % not match the order of the hash slots for which the case applies.
    % In other words, if e.g. OtherTaggedConsIds has two elements and
    % CaseStatement has the C code "case Slot1: case Slot2: case Slot3:"
    % generated in front of it, Slot1 can be the slot of any of
    % MainTaggedConsId and the two OtherTaggedConsIds; it will be the slot
    % of MainTaggedConsId only by accident.
    CommentString = "case " ++ Strings,
    Goal = hlds_goal(_GoalExpr, GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    MLDS_Context = mlds_make_context(Context),
    Comment = statement(ml_stmt_atomic(comment(CommentString)),
        MLDS_Context),
    CaseStatement = statement(ml_stmt_block([], [Comment, GoalStatement]),
        MLDS_Context),
    map.det_insert(CaseId, CaseStatement, !CodeMap).

:- func gen_string_switch_case_comment(tagged_cons_id) = string.

gen_string_switch_case_comment(TaggedConsId) = String :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    ( ConsTag = string_tag(ConsString) ->
        String = """" ++ ConsString ++ """"
    ;
        unexpected($module, $pred, "non-string tag")
    ).

%-----------------------------------------------------------------------------%

    % A list of all the hash slots that all have the same code. The list is
    % stored in reversed form.
    %
:- type hash_slots
    --->    hash_slots(int, list(int)).

    % Maps case numbers (each of which identifies the code of one switch arm)
    % to the hash slots that share that code.
    %
:- type hash_slot_rev_map == map(case_id, hash_slots).

:- pred ml_gen_string_hash_jump_slots(int::in, int::in,
    map(int, string_hash_slot(case_id))::in,
    mlds_type::in, maybe(mlds_field_id)::in,
    list(mlds_initializer)::in, list(mlds_initializer)::out,
    hash_slot_rev_map::in, hash_slot_rev_map::out) is det.

ml_gen_string_hash_jump_slots(Slot, TableSize, HashSlotMap, StructType,
        MaybeNextSlotId, !RevRowInitializers, !RevMap) :-
    ( Slot = TableSize ->
        true
    ;
        ml_gen_string_hash_jump_slot(Slot, HashSlotMap,
            StructType, MaybeNextSlotId, RowInitializer, !RevMap),
        !:RevRowInitializers = [RowInitializer | !.RevRowInitializers],
        ml_gen_string_hash_jump_slots(Slot + 1, TableSize, HashSlotMap,
            StructType, MaybeNextSlotId, !RevRowInitializers, !RevMap)
    ).

:- pred ml_gen_string_hash_jump_slot(int::in,
    map(int, string_hash_slot(case_id))::in,
    mlds_type::in, maybe(mlds_field_id)::in, mlds_initializer::out,
    hash_slot_rev_map::in, hash_slot_rev_map::out) is det.

ml_gen_string_hash_jump_slot(Slot, HashSlotMap, StructType,
        MaybeNextSlotId, RowInitializer, !RevMap) :-
    ( map.search(HashSlotMap, Slot, HashSlotMapEntry) ->
        HashSlotMapEntry = string_hash_slot(String, Next, CaseId),
        StringRval = ml_const(mlconst_string(String)),
        NextSlotRval = ml_const(mlconst_int(Next)),
        ( map.search(!.RevMap, CaseId, OldEntry) ->
            OldEntry = hash_slots(OldFirstSlot, OldLaterSlots),
            NewEntry = hash_slots(OldFirstSlot, [Slot | OldLaterSlots]),
            map.det_update(CaseId, NewEntry, !RevMap)
        ;
            NewEntry = hash_slots(Slot, []),
            map.det_insert(CaseId, NewEntry, !RevMap)
        )
    ;
        StringRval = ml_const(mlconst_null(ml_string_type)),
        NextSlotRval = ml_const(mlconst_int(-2))
    ),
    (
        MaybeNextSlotId = yes(_),
        RowInitializer = init_struct(StructType,
            [init_obj(StringRval), init_obj(NextSlotRval)])
    ;
        MaybeNextSlotId = no,
        RowInitializer = init_struct(StructType,
            [init_obj(StringRval)])
    ).

:- pred generate_string_jump_switch_arms(map(case_id, statement)::in,
    assoc_list(case_id, hash_slots)::in,
    list(mlds_switch_case)::in, list(mlds_switch_case)::out) is det.

generate_string_jump_switch_arms(_, [], !Cases).
generate_string_jump_switch_arms(CodeMap, [Entry | Entries], !Cases) :-
    Entry = CaseId - HashSlots,
    HashSlots = hash_slots(FirstHashSlot, RevLaterHashSlots),
    list.reverse(RevLaterHashSlots, LaterHashSlots),
    FirstMatchCond = make_hash_match(FirstHashSlot),
    LaterMatchConds = list.map(make_hash_match, LaterHashSlots),
    map.lookup(CodeMap, CaseId, CaseStatement),
    Case = mlds_switch_case(FirstMatchCond, LaterMatchConds, CaseStatement),
    !:Cases = [Case | !.Cases],
    generate_string_jump_switch_arms(CodeMap, Entries, !Cases).

:- func make_hash_match(int) = mlds_case_match_cond.

make_hash_match(Slot) = match_value(ml_const(mlconst_int(Slot))).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% The implementation of lookup hash tables.
%

ml_generate_string_hash_lookup_switch(VarRval, TaggedCases, LookupSwitchInfo,
        CodeModel, CanFail, Context, Defns, Statements, !Info) :-
    LookupSwitchInfo = ml_lookup_switch_info(CaseIdConsts, OutVars, OutTypes),
    (
        CaseIdConsts = all_one_soln(CaseIdValueMap),
        ml_case_id_soln_consts_to_tag_soln_consts(get_string_tag, TaggedCases,
            CaseIdValueMap, StrValueMap),
        map.to_assoc_list(StrValueMap, StrValues),
        ml_generate_string_hash_simple_lookup_switch(VarRval,
            StrValues, CodeModel, CanFail, OutVars, OutTypes, Context,
            Defns, Statements, !Info)
    ;
        CaseIdConsts = some_several_solns(CaseIdSolnMap, _Unit),
        expect(unify(CodeModel, model_non), $module, $pred,
            "CodeModel != model_non"),
        ml_case_id_soln_consts_to_tag_soln_consts(get_string_tag, TaggedCases,
            CaseIdSolnMap, StrSolnMap),
        map.to_assoc_list(StrSolnMap, StrSolns),
        ml_generate_string_hash_several_soln_lookup_switch(VarRval,
            StrSolns, CodeModel, CanFail, OutVars, OutTypes, Context,
            Defns, Statements, !Info)
    ).

%-----------------------------------------------------------------------------%

:- pred ml_generate_string_hash_simple_lookup_switch(mlds_rval::in,
    assoc_list(string, list(mlds_rval))::in, code_model::in, can_fail::in,
    list(prog_var)::in, list(mlds_type)::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_string_hash_simple_lookup_switch(VarRval, CaseValues,
        CodeModel, CanFail, OutVars, OutTypes, Context,
        Defns, Statements, !Info) :-
    MLDS_Context = mlds_make_context(Context),

    % Compute the hash table.
    construct_string_hash_cases(CaseValues, allow_doubling,
        TableSize, HashSlotMap, HashOp, NumCollisions),
    HashMask = TableSize - 1,

    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_gen_info_get_target(!.Info, Target),

    MLDS_StringType = mercury_type_to_mlds_type(ModuleInfo, string_type),
    MLDS_IntType = mlds_native_int_type,

    ( NumCollisions = 0 ->
        MLDS_ArgTypes = [MLDS_StringType | OutTypes],
        LoopPresent = no
    ;
        MLDS_ArgTypes = [MLDS_StringType, MLDS_IntType | OutTypes],
        LoopPresent = yes
    ),

    ml_gen_string_hash_switch_search_vars(CodeModel, CanFail, LoopPresent,
        Context, MLDS_Context, VarRval, HashSearchInfo, !Info),
    HashSearchInfo = ml_hash_search_info(_CodeModel, _LoopPresent,
        _Context, _VarRval, SlotVarLval, _StringVarLval, _MaybeStopLoopLval,
        _FailStatements, Defns),
    SlotVarRval = ml_lval(SlotVarLval),

    ml_gen_info_get_global_data(!.Info, GlobalData0),
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        MLDS_ArgTypes, StructTypeNum, StructType, FieldIds,
        GlobalData0, GlobalData1),
    ( NumCollisions = 0 ->
        ( FieldIds = [StringFieldIdPrime | OutFieldIdsPrime] ->
            StringFieldId = StringFieldIdPrime,
            OutFieldIds = OutFieldIdsPrime,
            MaybeNextSlotFieldId = no
        ;
            unexpected($module, $pred, "bad FieldIds")
        )
    ;
        (
            FieldIds =
                [StringFieldIdPrime, NextSlotFieldIdPrime | OutFieldIdsPrime]
        ->
            StringFieldId = StringFieldIdPrime,
            OutFieldIds = OutFieldIdsPrime,
            MaybeNextSlotFieldId = yes(NextSlotFieldIdPrime)
        ;
            unexpected($module, $pred, "bad FieldIds")
        )
    ),
    % Generate the rows of the hash table.
    DummyOutRvals = list.map(ml_default_value_for_type, OutTypes),
    DummyOutInitializers = list.map(wrap_init_obj, DummyOutRvals),
    ml_gen_string_hash_simple_lookup_slots(0, TableSize, StructType,
        HashSlotMap, MaybeNextSlotFieldId, DummyOutInitializers,
        [], RevRowInitializers),
    list.reverse(RevRowInitializers, RowInitializers),
    % Generate the hash table.
    ml_gen_static_vector_defn(MLDS_ModuleName, StructTypeNum, RowInitializers,
        VectorCommon, GlobalData1, GlobalData),
    ml_gen_info_set_global_data(GlobalData, !Info),

    ml_generate_field_assigns(OutVars, OutTypes, OutFieldIds, VectorCommon,
        StructType, SlotVarRval, MLDS_Context, LookupStatements, !Info),

    FoundMatchComment = "we found a match; look up the results",
    FoundMatchCommentStatement = statement(
        ml_stmt_atomic(comment(FoundMatchComment)), MLDS_Context),

    (
        CodeModel = model_det,
        MatchStatements = [FoundMatchCommentStatement | LookupStatements]
    ;
        CodeModel = model_semi,
        ml_gen_set_success(!.Info, ml_const(mlconst_true), Context,
            SetSuccessTrueStatement),
        MatchStatements = [FoundMatchCommentStatement | LookupStatements] ++
            [SetSuccessTrueStatement]
    ;
        CodeModel = model_non,
        unexpected($module, $pred, "model_non")
    ),

    InitialComment = "hashed string simple lookup switch",
    ml_gen_string_hash_switch_search(MLDS_Context, InitialComment,
        HashSearchInfo, HashOp, VectorCommon, StructType,
        StringFieldId, MaybeNextSlotFieldId, HashMask,
        [], MatchStatements, Statements, !Info).

:- pred ml_gen_string_hash_simple_lookup_slots(int::in, int::in, mlds_type::in,
    map(int, string_hash_slot(list(mlds_rval)))::in, maybe(mlds_field_id)::in,
    list(mlds_initializer)::in,
    list(mlds_initializer)::in, list(mlds_initializer)::out) is det.

ml_gen_string_hash_simple_lookup_slots(Slot, TableSize, StructType,
        HashSlotMap, MaybeNextSlotId, DummyOutInitializers,
        !RevRowInitializers) :-
    ( Slot = TableSize ->
        true
    ;
        ml_gen_string_hash_simple_lookup_slot(Slot, StructType, HashSlotMap,
            MaybeNextSlotId, DummyOutInitializers, RowInitializer),
        !:RevRowInitializers = [RowInitializer | !.RevRowInitializers],
        ml_gen_string_hash_simple_lookup_slots(Slot + 1, TableSize,
            StructType, HashSlotMap, MaybeNextSlotId,
            DummyOutInitializers, !RevRowInitializers)
    ).

:- pred ml_gen_string_hash_simple_lookup_slot(int::in, mlds_type::in,
    map(int, string_hash_slot(list(mlds_rval)))::in,
    maybe(mlds_field_id)::in,
    list(mlds_initializer)::in, mlds_initializer::out) is det.

ml_gen_string_hash_simple_lookup_slot(Slot, StructType, HashSlotMap,
        MaybeNextSlotId, DummyOutInitializers, RowInitializer) :-
    ( map.search(HashSlotMap, Slot, HashSlotMapEntry) ->
        HashSlotMapEntry = string_hash_slot(String, Next, OutRvals),
        StringRval = ml_const(mlconst_string(String)),
        NextSlotRval = ml_const(mlconst_int(Next)),
        OutInitializers = list.map(wrap_init_obj, OutRvals)
    ;
        StringRval = ml_const(mlconst_null(ml_string_type)),
        NextSlotRval = ml_const(mlconst_int(-2)),
        OutInitializers = DummyOutInitializers
    ),
    (
        MaybeNextSlotId = yes(_),
        RowInitializer = init_struct(StructType,
            [init_obj(StringRval), init_obj(NextSlotRval) | OutInitializers])
    ;
        MaybeNextSlotId = no,
        RowInitializer = init_struct(StructType,
            [init_obj(StringRval) | OutInitializers])
    ).

%-----------------------------------------------------------------------------%

:- pred ml_generate_string_hash_several_soln_lookup_switch(mlds_rval::in,
    assoc_list(string, soln_consts(mlds_rval))::in,
    code_model::in, can_fail::in,
    list(prog_var)::in, list(mlds_type)::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_string_hash_several_soln_lookup_switch(VarRval, CaseSolns,
        CodeModel, CanFail, OutVars, OutTypes, Context,
        Defns, Statements, !Info) :-
    MLDS_Context = mlds_make_context(Context),

    % Compute the hash table.
    construct_string_hash_cases(CaseSolns, allow_doubling,
        TableSize, HashSlotMap, HashOp, NumCollisions),
    HashMask = TableSize - 1,

    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_gen_info_get_target(!.Info, Target),

    MLDS_StringType = mercury_type_to_mlds_type(ModuleInfo, string_type),
    MLDS_IntType = mlds_native_int_type,

    ( NumCollisions = 0 ->
        FirstSolnFieldTypes = [MLDS_StringType,
            MLDS_IntType, MLDS_IntType | OutTypes],
        LoopPresent = no
    ;
        FirstSolnFieldTypes = [MLDS_StringType, MLDS_IntType,
            MLDS_IntType, MLDS_IntType | OutTypes],
        LoopPresent = yes
    ),

    ml_gen_string_hash_switch_search_vars(CodeModel, CanFail, LoopPresent,
        Context, MLDS_Context, VarRval, HashSearchInfo, !Info),
    HashSearchInfo = ml_hash_search_info(_CodeModel, _LoopPresent,
        _Context, _VarRval, SlotVarLval, _StringVarLval, _MaybeStopLoopLval,
        _FailStatements, Defns),

    ml_gen_info_get_global_data(!.Info, GlobalData0),
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        FirstSolnFieldTypes, FirstSolnStructTypeNum, FirstSolnStructType,
        FirstSolnFieldIds, GlobalData0, GlobalData1),
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        OutTypes, LaterSolnStructTypeNum, LaterSolnStructType,
        LaterSolnOutFieldIds, GlobalData1, GlobalData2),
    ( NumCollisions = 0 ->
        (
            FirstSolnFieldIds = [StringFieldIdPrime,
                NumLaterSolnsFieldIdPrime, FirstLaterSolnRowFieldIdPrime
                | FirstSolnOutFieldIdsPrime]
        ->
            StringFieldId = StringFieldIdPrime,
            NumLaterSolnsFieldId = NumLaterSolnsFieldIdPrime,
            FirstLaterSolnRowFieldId = FirstLaterSolnRowFieldIdPrime,
            FirstSolnOutFieldIds = FirstSolnOutFieldIdsPrime,
            MaybeNextSlotFieldId = no
        ;
            unexpected($module, $pred, "bad FieldIds")
        )
    ;
        (
            FirstSolnFieldIds =
                [StringFieldIdPrime, NextSlotFieldIdPrime,
                NumLaterSolnsFieldIdPrime, FirstLaterSolnRowFieldIdPrime
                | FirstSolnOutFieldIdsPrime]
        ->
            StringFieldId = StringFieldIdPrime,
            NumLaterSolnsFieldId = NumLaterSolnsFieldIdPrime,
            FirstLaterSolnRowFieldId = FirstLaterSolnRowFieldIdPrime,
            FirstSolnOutFieldIds = FirstSolnOutFieldIdsPrime,
            MaybeNextSlotFieldId = yes(NextSlotFieldIdPrime)
        ;
            unexpected($module, $pred, "bad FieldIds")
        )
    ),

    % Generate the rows of the first soln hash table.
    DummyOutRvals = list.map(ml_default_value_for_type, OutTypes),
    DummyOutInitializers = list.map(wrap_init_obj, DummyOutRvals),
    ml_gen_string_hash_several_soln_lookup_slots(0, TableSize, HashSlotMap,
        FirstSolnStructType, LaterSolnStructType,
        MaybeNextSlotFieldId, DummyOutInitializers,
        [], RevFirstSolnRowInitializers,
        cord.init, LaterSolnRowInitializersCord, 0),
    list.reverse(RevFirstSolnRowInitializers, FirstSolnRowInitializers),
    LaterSolnRowInitializers = cord.list(LaterSolnRowInitializersCord),
    % Generate the hash table.
    ml_gen_static_vector_defn(MLDS_ModuleName, FirstSolnStructTypeNum,
        FirstSolnRowInitializers, FirstSolnVectorCommon,
        GlobalData2, GlobalData3),
    ml_gen_static_vector_defn(MLDS_ModuleName, LaterSolnStructTypeNum,
        LaterSolnRowInitializers, LaterSolnVectorCommon,
        GlobalData3, GlobalData),
    ml_gen_info_set_global_data(GlobalData, !Info),

    ml_gen_several_soln_lookup_code(Context, MLDS_Context,
        ml_lval(SlotVarLval), OutVars, OutTypes,
        FirstSolnStructType, LaterSolnStructType,
        NumLaterSolnsFieldId, FirstLaterSolnRowFieldId,
        FirstSolnOutFieldIds, LaterSolnOutFieldIds,
        FirstSolnVectorCommon, LaterSolnVectorCommon, dont_need_bit_vec_check,
        MatchDefns, SuccessStatements, !Info),

    InitialComment = "hashed string several_soln lookup switch",
    ml_gen_string_hash_switch_search(MLDS_Context, InitialComment,
        HashSearchInfo, HashOp, FirstSolnVectorCommon,
        FirstSolnStructType, StringFieldId, MaybeNextSlotFieldId, HashMask,
        MatchDefns, SuccessStatements, Statements, !Info).

:- pred ml_gen_string_hash_several_soln_lookup_slots(int::in, int::in,
    map(int, string_hash_slot(soln_consts(mlds_rval)))::in,
    mlds_type::in, mlds_type::in,
    maybe(mlds_field_id)::in, list(mlds_initializer)::in,
    list(mlds_initializer)::in, list(mlds_initializer)::out,
    cord(mlds_initializer)::in, cord(mlds_initializer)::out, int::in) is det.

ml_gen_string_hash_several_soln_lookup_slots(Slot, TableSize,
        HashSlotMap, FirstSolnStructType, LaterSolnStructType,
        MaybeNextSlotId, DummyOutInitializers,
        !RevFirstSolnRowInitializers, !LaterSolnRowInitializersCord,
        !.CurLaterSolnIndex) :-
    ( Slot = TableSize ->
        true
    ;
        ml_gen_string_hash_several_soln_lookup_slot(Slot, HashSlotMap,
            FirstSolnStructType, LaterSolnStructType, MaybeNextSlotId,
            DummyOutInitializers, FirstSolnsRowInitializer,
            !LaterSolnRowInitializersCord, !CurLaterSolnIndex),
        !:RevFirstSolnRowInitializers =
            [FirstSolnsRowInitializer | !.RevFirstSolnRowInitializers],
        ml_gen_string_hash_several_soln_lookup_slots(Slot + 1, TableSize,
            HashSlotMap, FirstSolnStructType, LaterSolnStructType,
            MaybeNextSlotId,
            DummyOutInitializers, !RevFirstSolnRowInitializers,
            !LaterSolnRowInitializersCord, !.CurLaterSolnIndex)
    ).

:- pred ml_gen_string_hash_several_soln_lookup_slot(int::in,
    map(int, string_hash_slot(soln_consts(mlds_rval)))::in,
    mlds_type::in, mlds_type::in, maybe(mlds_field_id)::in,
    list(mlds_initializer)::in, mlds_initializer::out,
    cord(mlds_initializer)::in, cord(mlds_initializer)::out,
    int::in, int::out) is det.

ml_gen_string_hash_several_soln_lookup_slot(Slot, HashSlotMap,
        FirstSolnStructType, LaterSolnStructType, MaybeNextSlotId,
        DummyOutInitializers, FirstSolnsRowInitializer,
        !LaterSolnRowInitializersCord, !CurLaterSolnIndex) :-
    ( map.search(HashSlotMap, Slot, HashSlotMapEntry) ->
        HashSlotMapEntry = string_hash_slot(String, Next, Solns),
        StringRval = ml_const(mlconst_string(String)),
        NextSlotRval = ml_const(mlconst_int(Next)),
        (
            Solns = one_soln(FirstSolnRvals),
            NumLaterSolnsRval = ml_const(mlconst_int(0)),
            FirstLaterSlotRval = ml_const(mlconst_int(-1)),
            FirstSolnOutInitializers = list.map(wrap_init_obj, FirstSolnRvals)
        ;
            Solns = several_solns(FirstSolnRvals, LaterSolns),
            list.length(LaterSolns, NumLaterSolns),
            NumLaterSolnsRval = ml_const(mlconst_int(NumLaterSolns)),
            FirstLaterSlotRval = ml_const(mlconst_int(!.CurLaterSolnIndex)),
            FirstSolnOutInitializers = list.map(wrap_init_obj, FirstSolnRvals),
            LaterSolnRowInitializers = list.map(
                ml_construct_later_soln_row(LaterSolnStructType),
                LaterSolns),
            !:LaterSolnRowInitializersCord = !.LaterSolnRowInitializersCord ++
                from_list(LaterSolnRowInitializers),
            !:CurLaterSolnIndex = !.CurLaterSolnIndex + NumLaterSolns
        )
    ;
        StringRval = ml_const(mlconst_null(ml_string_type)),
        NextSlotRval = ml_const(mlconst_int(-2)),
        NumLaterSolnsRval = ml_const(mlconst_int(-1)),
        FirstLaterSlotRval = ml_const(mlconst_int(-1)),
        FirstSolnOutInitializers = DummyOutInitializers
    ),
    (
        MaybeNextSlotId = yes(_),
        FirstSolnsRowInitializer = init_struct(FirstSolnStructType,
            [init_obj(StringRval), init_obj(NextSlotRval),
            init_obj(NumLaterSolnsRval), init_obj(FirstLaterSlotRval)
            | FirstSolnOutInitializers])
    ;
        MaybeNextSlotId = no,
        FirstSolnsRowInitializer = init_struct(FirstSolnStructType,
            [init_obj(StringRval),
            init_obj(NumLaterSolnsRval), init_obj(FirstLaterSlotRval)
            | FirstSolnOutInitializers])
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Code useful for both jump and lookup hash string switches.
%

:- type ml_hash_search_info
    --->    ml_hash_search_info(
                mhsi_code_model                 :: code_model,
                mhsi_loop_present               :: bool,
                mhsi_context                    :: prog_context,
                mhsi_switch_var                 :: mlds_rval,
                mhsi_slot_var                   :: mlds_lval,
                mhsi_string_var                 :: mlds_lval,
                mhsi_stop_loop_var              :: maybe(mlds_lval),
                mhsi_fail_statements            :: list(statement),
                mhsi_defns                      :: list(mlds_defn)
            ).

:- pred ml_gen_string_hash_switch_search_vars(code_model::in, can_fail::in,
    bool::in, prog_context::in, mlds_context::in, mlds_rval::in,
    ml_hash_search_info::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_string_hash_switch_search_vars(CodeModel, CanFail, LoopPresent,
        Context, MLDS_Context, VarRval, HashSearchInfo, !Info) :-
    % Generate the following local variable declarations:
    %   int         slot;
    %   MR_String   str;

    ml_gen_info_new_aux_var_name("slot", SlotVar, !Info),
    SlotVarType = mlds_native_int_type,
    % We never need to trace ints.
    SlotVarDefn = ml_gen_mlds_var_decl(mlds_data_var(SlotVar), SlotVarType,
        gc_no_stmt, MLDS_Context),
    ml_gen_var_lval(!.Info, SlotVar, SlotVarType, SlotVarLval),

    ml_gen_info_new_aux_var_name("str", StringVar, !Info),
    StringVarType = ml_string_type,
    % StringVar always points to an element of the string_table array.
    % All those elements are static constants; they can never point into
    % the heap. So GC never needs to trace StringVar.
    StringVarDefn = ml_gen_mlds_var_decl(mlds_data_var(StringVar),
        StringVarType, gc_no_stmt, MLDS_Context),
    ml_gen_var_lval(!.Info, StringVar, StringVarType, StringVarLval),

    AlwaysDefns = [SlotVarDefn, StringVarDefn],
    ml_should_use_stop_loop(MLDS_Context, LoopPresent,
        MaybeStopLoopLval, StopLoopVarDefns, !Info),
    Defns = AlwaysDefns ++ StopLoopVarDefns,

    % Generate the code for when the lookup fails.
    ml_gen_maybe_switch_failure(CodeModel, CanFail, Context, FailStatements,
        !Info),

    HashSearchInfo = ml_hash_search_info(CodeModel, LoopPresent, Context,
        VarRval, SlotVarLval, StringVarLval, MaybeStopLoopLval,
        FailStatements, Defns).

:- pred ml_gen_string_hash_switch_search(mlds_context::in, string::in,
    ml_hash_search_info::in, unary_op::in,
    mlds_vector_common::in, mlds_type::in,
    mlds_field_id::in, maybe(mlds_field_id)::in, int::in,
    list(mlds_defn)::in, list(statement)::in, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_string_hash_switch_search(MLDS_Context, InitialComment,
        HashSearchInfo, HashOp, VectorCommon, StructType,
        StringFieldId, MaybeNextSlotFieldId, HashMask,
        MatchDefns, MatchStatements, Statements, !Info) :-
    HashSearchInfo = ml_hash_search_info(CodeModel, LoopPresent,
        Context, VarRval, SlotVarLval, StringVarLval,
        MaybeStopLoopVarLval, FailStatements, _Defns),
    SlotVarRval = ml_lval(SlotVarLval),
    StringVarRval = ml_lval(StringVarLval),
    SlotVarType = mlds_native_int_type,
    StringVarType = ml_string_type,

    ml_wrap_loop_break(CodeModel, LoopPresent,
        MLDS_Context, MaybeStopLoopVarLval,
        MatchDefns, MatchStatements, FailStatements,
        SetupForFailStatements, SuccessStatement, AfterStatements, !Info),

    InitialCommentStatement =
        statement(ml_stmt_atomic(comment(InitialComment)), MLDS_Context),
    PrepareForMatchStatements = [
        statement(ml_stmt_atomic(comment(
            "compute the hash value of the input string")), MLDS_Context),
        statement(
            ml_stmt_atomic(assign(SlotVarLval,
                ml_binop(bitwise_and,
                    ml_unop(std_unop(HashOp), VarRval),
                    ml_const(mlconst_int(HashMask))))),
            MLDS_Context)
        ],
    FoundMatchCond =
        ml_binop(logical_and,
            ml_binop(ne, StringVarRval, ml_const(mlconst_null(StringVarType))),
            ml_binop(str_eq, StringVarRval, VarRval)
        ),
    LookForMatchPrepareStatements = [
        statement(ml_stmt_atomic(comment(
            "lookup the string for this hash slot")), MLDS_Context),
        statement(
            ml_stmt_atomic(assign(StringVarLval,
                ml_lval(ml_field(yes(0),
                    ml_vector_common_row(VectorCommon, SlotVarRval),
                    StringFieldId, StringVarType, StructType)))),
            MLDS_Context),
        statement(ml_stmt_atomic(comment("did we find a match?")),
            MLDS_Context)
    ],
    SlotTest = ml_binop(int_ge, SlotVarRval, ml_const(mlconst_int(0))),
    (
        MaybeStopLoopVarLval = no,
        InitStopLoopVarStatements = [],
        InitSuccessStatements = [],
        LoopTest = SlotTest
    ;
        MaybeStopLoopVarLval = yes(StopLoopVarLval),
        InitStopLoopVarStatement = statement(ml_stmt_atomic(
            assign(StopLoopVarLval, ml_const(mlconst_int(0)))),
            MLDS_Context),
        InitStopLoopVarStatements = [InitStopLoopVarStatement],
        (
            CodeModel = model_det,
            % If the switch is model_det, the value of `succeeded'
            % is irrelevant, as it will not be consulted.
            InitSuccessStatements = []
        ;
            CodeModel = model_semi,
            ml_gen_set_success(!.Info, ml_const(mlconst_false), Context,
                InitSuccessStatement),
            InitSuccessStatements = [InitSuccessStatement]
        ;
            CodeModel = model_non,
            % If the switch is model_non, we get to the end of search code
            % (which may or my not be a loop) for both matches and non-matches,
            % though in the case of matches we get there only after invoking
            % the success continuation for each solution.
            InitSuccessStatements = []
        ),
        StopLoopTest = ml_binop(eq,
            ml_lval(StopLoopVarLval), ml_const(mlconst_int(0))),
        LoopTest = ml_binop(logical_and, StopLoopTest, SlotTest)
    ),
    (
        MaybeNextSlotFieldId = yes(NextSlotFieldId),
        NoMatchStatement = statement(ml_stmt_block([], [
            statement(ml_stmt_atomic(comment(
                "no match yet, so get next slot in hash chain")),
                MLDS_Context),
            statement(
                ml_stmt_atomic(assign(SlotVarLval,
                    ml_lval(ml_field(yes(0),
                        ml_vector_common_row(VectorCommon, SlotVarRval),
                        NextSlotFieldId, SlotVarType, StructType)))),
                MLDS_Context)
            ]), MLDS_Context),
        LookForMatchStatement = statement(
            ml_stmt_if_then_else(FoundMatchCond, SuccessStatement,
                yes(NoMatchStatement)),
            MLDS_Context),
        LoopBody = statement(ml_stmt_block([],
            LookForMatchPrepareStatements ++ [LookForMatchStatement]),
            MLDS_Context),
        LoopStatements = [
            statement(ml_stmt_atomic(comment("hash chain loop")),
                MLDS_Context),
            statement(ml_stmt_while(loop_at_least_once, LoopTest, LoopBody),
                MLDS_Context)
        ],
        SearchStatements = PrepareForMatchStatements ++
            InitStopLoopVarStatements ++ InitSuccessStatements ++
            LoopStatements,
        Statements = [InitialCommentStatement | SetupForFailStatements] ++
            SearchStatements ++ AfterStatements
    ;
        MaybeNextSlotFieldId = no,
        NoLoopCommentStatement = statement(ml_stmt_atomic(
            comment("no collisions; no hash chain loop")), MLDS_Context),
        LookForMatchStatement = statement(
            ml_stmt_if_then_else(FoundMatchCond, SuccessStatement, no),
            MLDS_Context),
        SearchStatements = PrepareForMatchStatements ++
            InitSuccessStatements ++
            [NoLoopCommentStatement | LookForMatchPrepareStatements] ++
            [LookForMatchStatement],
        Statements = [InitialCommentStatement | SearchStatements] ++
            AfterStatements
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% The implementation of jump binary search tables.
%

ml_generate_string_binary_jump_switch(VarRval, Cases, CodeModel, CanFail,
        Context, Defns, Statements, !Info) :-
    MLDS_Context = mlds_make_context(Context),
    ml_gen_string_binary_switch_search_vars(CodeModel, CanFail,
        Context, MLDS_Context, VarRval, BinarySearchInfo, !Info),
    BinarySearchInfo = ml_binary_search_info(_CodeModel,
        _VarRval, _LoVarLval, _HiVarLval, MidVarLval, _ResultVarLval,
        _MaybeStopLoopVarLval, _FailStatements, Defns),

    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_gen_info_get_target(!.Info, Target),

    MLDS_StringType = mercury_type_to_mlds_type(ModuleInfo, string_type),
    MLDS_CaseNumType = mlds_native_int_type,
    MLDS_ArgTypes = [MLDS_StringType, MLDS_CaseNumType],

    % Generate the binary search table.
    ml_gen_info_get_global_data(!.Info, GlobalData0),
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        MLDS_ArgTypes, StructTypeNum, StructType, FieldIds,
        GlobalData0, GlobalData1),
    ml_gen_info_set_global_data(GlobalData1, !Info),
    ( FieldIds = [StringFieldIdPrime, CaseNumFieldIdPrime] ->
        StringFieldId = StringFieldIdPrime,
        CaseNumFieldId = CaseNumFieldIdPrime
    ;
        unexpected($module, $pred, "bad FieldIds")
    ),
    map.init(CaseLabelMap0),
    switch_util.string_binary_cases(Cases,
        gen_tagged_case_code_for_string_switch_dummy(CodeModel),
        CaseLabelMap0, CaseLabelMap, !Info, unit, _, SortedTable),
    ml_gen_string_binary_jump_initializers(SortedTable, StructType,
        [], RevRowInitializers, 0, TableSize),
    list.reverse(RevRowInitializers, RowInitializers),
    % We need to get the globaldata out of !Info again, since the generation
    % of code for the switch arms can generate global data.
    ml_gen_info_get_global_data(!.Info, GlobalData2),
    ml_gen_static_vector_defn(MLDS_ModuleName, StructTypeNum, RowInitializers,
        VectorCommon, GlobalData2, GlobalData),
    ml_gen_info_set_global_data(GlobalData, !Info),

    % Generate the switch that uses the final value of MidVar (the one that,
    % when used as an index into the binary search table, matches the current
    % value of the switched-on variable) to select the piece of code to
    % execute.
    map.to_assoc_list(CaseLabelMap, CaseLabelList),
    ml_gen_string_binary_jump_switch_arms(CaseLabelList, [], SwitchCases0),
    list.sort(SwitchCases0, SwitchCases),
    SwitchStmt0 = ml_stmt_switch(mlds_native_int_type,
        ml_lval(ml_field(yes(0),
            ml_vector_common_row(VectorCommon, ml_lval(MidVarLval)),
        CaseNumFieldId, MLDS_StringType, StructType)),
        mlds_switch_range(0, TableSize - 1), SwitchCases,
        default_is_unreachable),
    ml_simplify_switch(SwitchStmt0, MLDS_Context, SwitchStatement, !Info),

    % Generate the code that searches the table.
    InitialComment = "binary string jump switch",
    ml_gen_string_binary_switch_search(MLDS_Context, InitialComment,
        BinarySearchInfo, VectorCommon, TableSize, StructType, StringFieldId,
        [], [SwitchStatement], Statements, !Info).

:- pred ml_gen_string_binary_jump_initializers(assoc_list(string, case_id)::in,
    mlds_type::in,
    list(mlds_initializer)::in, list(mlds_initializer)::out,
    int::in, int::out) is det.

ml_gen_string_binary_jump_initializers([],
        _StructType, !RevRowInitializers, !CurIndex).
ml_gen_string_binary_jump_initializers([Str - CaseId | StrCaseIds],
        StructType, !RevRowInitializers, !CurIndex) :-
    CaseId = case_id(CaseNum),
    StrRval = ml_const(mlconst_string(Str)),
    CaseNumRval = ml_const(mlconst_int(CaseNum)),
    RowInitializer = init_struct(StructType,
        [init_obj(StrRval), init_obj(CaseNumRval)]),
    !:RevRowInitializers = [RowInitializer | !.RevRowInitializers],
    !:CurIndex = !.CurIndex + 1,
    ml_gen_string_binary_jump_initializers(StrCaseIds,
        StructType, !RevRowInitializers, !CurIndex).

:- pred ml_gen_string_binary_jump_switch_arms(
    assoc_list(case_id, statement)::in,
    list(mlds_switch_case)::in, list(mlds_switch_case)::out) is det.

ml_gen_string_binary_jump_switch_arms([], !SwitchCases).
ml_gen_string_binary_jump_switch_arms([CaseIdStmt | CaseIdsStmts],
        !SwitchCases) :-
    CaseIdStmt = CaseId - Statement,
    CaseId = case_id(CaseNum),
    MatchCond = match_value(ml_const(mlconst_int(CaseNum))),
    SwitchCase = mlds_switch_case(MatchCond, [], Statement),
    !:SwitchCases = [SwitchCase | !.SwitchCases],
    ml_gen_string_binary_jump_switch_arms(CaseIdsStmts, !SwitchCases).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% The implementation of lookup binary search tables.
%

ml_generate_string_binary_lookup_switch(VarRval, TaggedCases, LookupSwitchInfo,
        CodeModel, CanFail, Context, Defns, Statements, !Info) :-
    MLDS_Context = mlds_make_context(Context),
    LookupSwitchInfo = ml_lookup_switch_info(CaseIdConsts, OutVars, OutTypes),
    (
        CaseIdConsts = all_one_soln(CaseIdValueMap),
        ml_case_id_soln_consts_to_tag_soln_consts(get_string_tag, TaggedCases,
            CaseIdValueMap, StrValueMap),
        map.to_assoc_list(StrValueMap, StrValues),
        ml_generate_string_binary_simple_lookup_switch(VarRval,
            StrValues, CodeModel, CanFail, OutVars, OutTypes,
            Context, MLDS_Context, Defns, Statements, !Info)
    ;
        CaseIdConsts = some_several_solns(CaseIdSolnMap, _Unit),
        expect(unify(CodeModel, model_non), $module, $pred,
            "CodeModel != model_non"),
        ml_case_id_soln_consts_to_tag_soln_consts(get_string_tag, TaggedCases,
            CaseIdSolnMap, StrSolnMap),
        map.to_assoc_list(StrSolnMap, StrSolns),
        ml_generate_string_binary_several_soln_lookup_switch(VarRval,
            StrSolns, CodeModel, CanFail, OutVars, OutTypes,
            Context, MLDS_Context, Defns, Statements, !Info)
    ).

%-----------------------------------------------------------------------------%

:- pred ml_generate_string_binary_simple_lookup_switch(mlds_rval::in,
    assoc_list(string, list(mlds_rval))::in, code_model::in, can_fail::in,
    list(prog_var)::in, list(mlds_type)::in,
    prog_context::in, mlds_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_string_binary_simple_lookup_switch(VarRval, CaseValues0,
        CodeModel, CanFail, OutVars, OutTypes, Context, MLDS_Context,
        Defns, Statements, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_gen_info_get_target(!.Info, Target),

    MLDS_StringType = mercury_type_to_mlds_type(ModuleInfo, string_type),
    MLDS_ArgTypes = [MLDS_StringType | OutTypes],

    % Generate the binary search table.
    list.sort(CaseValues0, CaseValues),
    ml_gen_info_get_global_data(!.Info, GlobalData0),
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        MLDS_ArgTypes, StructTypeNum, StructType, FieldIds,
        GlobalData0, GlobalData1),
    ( FieldIds = [StringFieldIdPrime | OutFieldIdsPrime] ->
        StringFieldId = StringFieldIdPrime,
        OutFieldIds = OutFieldIdsPrime
    ;
        unexpected($module, $pred, "bad FieldIds")
    ),
    ml_gen_string_binary_simple_lookup_initializers(CaseValues, StructType,
        [], RevRowInitializers, 0, TableSize),
    list.reverse(RevRowInitializers, RowInitializers),
    ml_gen_static_vector_defn(MLDS_ModuleName, StructTypeNum, RowInitializers,
        VectorCommon, GlobalData1, GlobalData),
    ml_gen_info_set_global_data(GlobalData, !Info),

    ml_gen_string_binary_switch_search_vars(CodeModel, CanFail,
        Context, MLDS_Context, VarRval, BinarySearchInfo, !Info),
    BinarySearchInfo = ml_binary_search_info(_CodeModel, _VarRval,
        _LoVarLval, _HiVarLval, MidVarLval, _ResultVarLval, _MaybeStopLoopLval,
        _FailStatements, Defns),
    MidVarRval = ml_lval(MidVarLval),

    ml_generate_field_assigns(OutVars, OutTypes, OutFieldIds,
        VectorCommon, StructType, MidVarRval, MLDS_Context,
        GetArgStatements, !Info),
    (
        CodeModel = model_det,
        MatchStatements = GetArgStatements
    ;
        CodeModel = model_semi,
        ml_gen_set_success(!.Info, ml_const(mlconst_true), Context,
            SetSuccessTrueStatement),
        MatchStatements = GetArgStatements ++ [SetSuccessTrueStatement]
    ;
        CodeModel = model_non,
        unexpected($module, $pred, "model_non")
    ),

    % Generate the code that searches the table.
    InitialComment = "binary string simple lookup switch",
    ml_gen_string_binary_switch_search(MLDS_Context, InitialComment,
        BinarySearchInfo, VectorCommon, TableSize, StructType, StringFieldId,
        [], MatchStatements, Statements, !Info).

:- pred ml_gen_string_binary_simple_lookup_initializers(
    assoc_list(string, list(mlds_rval))::in, mlds_type::in,
    list(mlds_initializer)::in, list(mlds_initializer)::out,
    int::in, int::out) is det.

ml_gen_string_binary_simple_lookup_initializers([],
        _StructType, !RevRowInitializers, !CurIndex).
ml_gen_string_binary_simple_lookup_initializers([Str - Rvals | StrRvals],
        StructType, !RevRowInitializers, !CurIndex) :-
    StrRval = ml_const(mlconst_string(Str)),
    RowInitializer = init_struct(StructType,
        [init_obj(StrRval) | list.map(wrap_init_obj, Rvals)]),
    !:RevRowInitializers = [RowInitializer | !.RevRowInitializers],
    !:CurIndex = !.CurIndex + 1,
    ml_gen_string_binary_simple_lookup_initializers(StrRvals,
        StructType, !RevRowInitializers, !CurIndex).

%-----------------------------------------------------------------------------%

:- pred ml_generate_string_binary_several_soln_lookup_switch(mlds_rval::in,
    assoc_list(string, soln_consts(mlds_rval))::in,
    code_model::in, can_fail::in,
    list(prog_var)::in, list(mlds_type)::in,
    prog_context::in, mlds_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_string_binary_several_soln_lookup_switch(VarRval, CaseSolns0,
        CodeModel, CanFail, OutVars, OutTypes, Context, MLDS_Context,
        Defns, Statements, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_gen_info_get_target(!.Info, Target),
    MLDS_StringType = mercury_type_to_mlds_type(ModuleInfo, string_type),
    MLDS_IntType = mlds_native_int_type,
    FirstSolnFieldTypes =
        [MLDS_StringType, MLDS_IntType, MLDS_IntType | OutTypes],
    LaterSolnFieldTypes = OutTypes,

    % Generate the binary search table.
    list.sort(CaseSolns0, CaseSolns),
    ml_gen_info_get_global_data(!.Info, GlobalData0),
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        FirstSolnFieldTypes, FirstSolnStructTypeNum, FirstSolnStructType,
        FirstSolnFieldIds, GlobalData0, GlobalData1),
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        LaterSolnFieldTypes, LaterSolnStructTypeNum, LaterSolnStructType,
        LaterSolnOutFieldIds, GlobalData1, GlobalData2),
    (
        FirstSolnFieldIds = [StringFieldIdPrime, NumLaterSolnsFieldIdPrime,
            FirstLaterSolnRowFieldIdPrime | FirstSolnOutFieldIdsPrime]
    ->
        StringFieldId = StringFieldIdPrime,
        NumLaterSolnsFieldId = NumLaterSolnsFieldIdPrime,
        FirstLaterSolnRowFieldId = FirstLaterSolnRowFieldIdPrime,
        FirstSolnOutFieldIds = FirstSolnOutFieldIdsPrime
    ;
        unexpected($module, $pred, "bad FieldIds")
    ),
    ml_gen_string_binary_several_lookup_initializers(CaseSolns,
        FirstSolnStructType, LaterSolnStructType,
        [], RevFirstSolnRowInitializers,
        cord.init, LaterSolnRowInitializersCord,
        0, FirstSolnTableSize, 0),
    list.reverse(RevFirstSolnRowInitializers, FirstSolnRowInitializers),
    LaterSolnRowInitializers = cord.list(LaterSolnRowInitializersCord),
    ml_gen_static_vector_defn(MLDS_ModuleName, FirstSolnStructTypeNum,
        FirstSolnRowInitializers, FirstSolnVectorCommon,
        GlobalData2, GlobalData3),
    ml_gen_static_vector_defn(MLDS_ModuleName, LaterSolnStructTypeNum,
        LaterSolnRowInitializers, LaterSolnVectorCommon,
        GlobalData3, GlobalData),
    ml_gen_info_set_global_data(GlobalData, !Info),

    ml_gen_string_binary_switch_search_vars(CodeModel, CanFail,
        Context, MLDS_Context, VarRval, BinarySearchInfo, !Info),
    BinarySearchInfo = ml_binary_search_info(_CodeModel, _VarRval,
        _LoVarLval, _HiVarLval, MidVarLval, _ResultVarLval, _MaybeStopLoopLval,
        _FailStatements, Defns),

    ml_gen_several_soln_lookup_code(Context, MLDS_Context,
        ml_lval(MidVarLval), OutVars, OutTypes,
        FirstSolnStructType, LaterSolnStructType,
        NumLaterSolnsFieldId, FirstLaterSolnRowFieldId,
        FirstSolnOutFieldIds, LaterSolnOutFieldIds,
        FirstSolnVectorCommon, LaterSolnVectorCommon, dont_need_bit_vec_check,
        MatchDefns, SuccessStatements, !Info),

    % Generate the code that searches the table.
    InitialComment = "binary string several soln lookup switch",
    ml_gen_string_binary_switch_search(MLDS_Context, InitialComment,
        BinarySearchInfo, FirstSolnVectorCommon,FirstSolnTableSize,
        FirstSolnStructType, StringFieldId,
        MatchDefns, SuccessStatements, Statements, !Info).

:- pred ml_gen_string_binary_several_lookup_initializers(
    assoc_list(string, soln_consts(mlds_rval))::in,
    mlds_type::in, mlds_type::in,
    list(mlds_initializer)::in, list(mlds_initializer)::out,
    cord(mlds_initializer)::in, cord(mlds_initializer)::out,
    int::in, int::out, int::in) is det.

ml_gen_string_binary_several_lookup_initializers([],
        _FirstSolnStructType, _LaterSolnStructType,
        !RevFirstSolnRowInitializers, !LaterSolnRowInitializersCord,
        !CurFirstSolnIndex, _CurLaterSolnIndex).
ml_gen_string_binary_several_lookup_initializers([Str - Solns | StrSolns],
        FirstSolnStructType, LaterSolnStructType,
        !RevFirstSolnRowInitializers, !LaterSolnRowInitializersCord,
        !CurFirstSolnIndex, !.CurLaterSolnIndex) :-
    StrRval = ml_const(mlconst_string(Str)),
    (
        Solns = one_soln(FirstSolnRvals),
        NumLaterSolnsInitializer = gen_init_int(0),
        FirstLaterSlotInitializer = gen_init_int(-1),
        FirstSolnRowInitializer = init_struct(FirstSolnStructType,
            [init_obj(StrRval),
                NumLaterSolnsInitializer, FirstLaterSlotInitializer
                | list.map(wrap_init_obj, FirstSolnRvals)])
    ;
        Solns = several_solns(FirstSolnRvals, LaterSolns),
        list.length(LaterSolns, NumLaterSolns),
        NumLaterSolnsInitializer = gen_init_int(NumLaterSolns),
        FirstLaterSlotInitializer = gen_init_int(!.CurLaterSolnIndex),
        FirstSolnRowInitializer = init_struct(FirstSolnStructType,
            [init_obj(StrRval),
                NumLaterSolnsInitializer, FirstLaterSlotInitializer
                | list.map(wrap_init_obj, FirstSolnRvals)]),
        LaterSolnRowInitializers = list.map(
            ml_construct_later_soln_row(LaterSolnStructType),
            LaterSolns),
        !:LaterSolnRowInitializersCord = !.LaterSolnRowInitializersCord ++
            from_list(LaterSolnRowInitializers),
        !:CurLaterSolnIndex = !.CurLaterSolnIndex + NumLaterSolns
    ),
    !:RevFirstSolnRowInitializers =
        [FirstSolnRowInitializer | !.RevFirstSolnRowInitializers],
    !:CurFirstSolnIndex = !.CurFirstSolnIndex + 1,
    ml_gen_string_binary_several_lookup_initializers(StrSolns,
        FirstSolnStructType, LaterSolnStructType,
        !RevFirstSolnRowInitializers, !LaterSolnRowInitializersCord,
        !CurFirstSolnIndex, !.CurLaterSolnIndex).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Code useful for both jump and lookup binary string switches.
%

:- type ml_binary_search_info
    --->    ml_binary_search_info(
                mbsi_code_model             :: code_model,
                mbsi_switch_var             :: mlds_rval,
                mbsi_lo_var                 :: mlds_lval,
                mbsi_hi_var                 :: mlds_lval,
                mbsi_mid_var                :: mlds_lval,
                mbsi_result_var             :: mlds_lval,
                mbsi_stop_loop_var          :: maybe(mlds_lval),
                mbsi_fail_statements        :: list(statement),
                mbsi_defns                  :: list(mlds_defn)
            ).

:- pred ml_gen_string_binary_switch_search_vars(code_model::in, can_fail::in,
    prog_context::in, mlds_context::in, mlds_rval::in,
    ml_binary_search_info::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_string_binary_switch_search_vars(CodeModel, CanFail,
        Context, MLDS_Context, VarRval, BinarySearchInfo, !Info) :-
    % Generate the following local variable declarations:
    %   int         lo;
    %   int         hi;
    %   int         mid;
    %   MR_String   str;

    IndexType = mlds_native_int_type,
    ResultType = mlds_native_int_type,
    % We never need to trace ints.
    IndexGCStatement = gc_no_stmt,
    ResultGCStatement = gc_no_stmt,

    ml_gen_info_new_aux_var_name("lo", LoVar, !Info),
    LoVarDefn = ml_gen_mlds_var_decl(mlds_data_var(LoVar), IndexType,
        IndexGCStatement, MLDS_Context),
    ml_gen_var_lval(!.Info, LoVar, IndexType, LoVarLval),

    ml_gen_info_new_aux_var_name("hi", HiVar, !Info),
    HiVarDefn = ml_gen_mlds_var_decl(mlds_data_var(HiVar), IndexType,
        IndexGCStatement, MLDS_Context),
    ml_gen_var_lval(!.Info, HiVar, IndexType, HiVarLval),

    ml_gen_info_new_aux_var_name("mid", MidVar, !Info),
    MidVarDefn = ml_gen_mlds_var_decl(mlds_data_var(MidVar), IndexType,
        IndexGCStatement, MLDS_Context),
    ml_gen_var_lval(!.Info, MidVar, IndexType, MidVarLval),

    ml_gen_info_new_aux_var_name("result", ResultVar, !Info),
    ResultVarDefn = ml_gen_mlds_var_decl(mlds_data_var(ResultVar), ResultType,
        ResultGCStatement, MLDS_Context),
    ml_gen_var_lval(!.Info, ResultVar, ResultType, ResultVarLval),

    AlwaysDefns = [LoVarDefn, HiVarDefn, MidVarDefn, ResultVarDefn],
    ml_should_use_stop_loop(MLDS_Context, yes,
        MaybeStopLoopLval, StopLoopVarDefns, !Info),
    Defns = AlwaysDefns ++ StopLoopVarDefns,

    % Generate the code for when the lookup fails.
    ml_gen_maybe_switch_failure(CodeModel, CanFail, Context, FailStatements,
        !Info),

    BinarySearchInfo = ml_binary_search_info(CodeModel, VarRval,
        LoVarLval, HiVarLval, MidVarLval, ResultVarLval, MaybeStopLoopLval,
        FailStatements, Defns).

:- pred ml_gen_string_binary_switch_search(mlds_context::in, string::in,
    ml_binary_search_info::in, mlds_vector_common::in, int::in, mlds_type::in,
    mlds_field_id::in, list(mlds_defn)::in, list(statement)::in,
    list(statement)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_string_binary_switch_search(MLDS_Context, InitialComment,
        BinarySearchInfo, VectorCommon, TableSize, StructType, StringFieldId,
        MatchDefns, MatchStatement, Statements, !Info) :-
    BinarySearchInfo = ml_binary_search_info(CodeModel, VarRval,
        LoVarLval, HiVarLval, MidVarLval, ResultVarLval, MaybeStopLoopVarLval,
        FailStatements, _Defns),
    LoVarRval = ml_lval(LoVarLval),
    HiVarRval = ml_lval(HiVarLval),
    MidVarRval = ml_lval(MidVarLval),
    ResultVarRval = ml_lval(ResultVarLval),

    ml_wrap_loop_break(CodeModel, yes, MLDS_Context, MaybeStopLoopVarLval,
        MatchDefns, MatchStatement, FailStatements,
        SetupForFailStatements, SuccessStatement, AfterStatements, !Info),

    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    MLDS_StringType = mercury_type_to_mlds_type(ModuleInfo, string_type),

    InitLoVarStatement = statement(ml_stmt_atomic(
        assign(LoVarLval, ml_const(mlconst_int(0)))),
        MLDS_Context),
    InitHiVarStatement = statement(ml_stmt_atomic(
        assign(HiVarLval, ml_const(mlconst_int(TableSize - 1)))),
        MLDS_Context),
    CrossingTest = ml_binop(int_le, LoVarRval, HiVarRval),

    AssignMidVarStatement = statement(ml_stmt_atomic(
        assign(MidVarLval,
            ml_binop(int_div,
                ml_binop(int_add, LoVarRval, HiVarRval),
                ml_const(mlconst_int(2))))),
        MLDS_Context),
    AssignResultVarStatement = statement(ml_stmt_atomic(
        assign(ResultVarLval,
            ml_binop(str_cmp,
                VarRval,
                ml_lval(ml_field(yes(0),
                    ml_vector_common_row(VectorCommon, MidVarRval),
                StringFieldId, MLDS_StringType, StructType))))),
        MLDS_Context),
    ResultTest = ml_binop(eq, ResultVarRval, ml_const(mlconst_int(0))),
    UpdateLoOrHiVarStatement = statement(
        ml_stmt_if_then_else(
            ml_binop(int_lt, ResultVarRval, ml_const(mlconst_int(0))),
            statement(ml_stmt_atomic(
                assign(HiVarLval,
                    ml_binop(int_sub, MidVarRval, ml_const(mlconst_int(1))))),
                MLDS_Context),
            yes(statement(ml_stmt_atomic(
                assign(LoVarLval,
                    ml_binop(int_add, MidVarRval, ml_const(mlconst_int(1))))),
                MLDS_Context))),
        MLDS_Context),

    (
        MaybeStopLoopVarLval = no,
        LoopBodyStatements = [
            AssignMidVarStatement,
            AssignResultVarStatement,
            statement(ml_stmt_if_then_else(ResultTest,
                SuccessStatement, yes(UpdateLoOrHiVarStatement)),
                MLDS_Context)
        ],
        SearchStatements = [
            InitLoVarStatement,
            InitHiVarStatement,
            statement(ml_stmt_while(loop_at_least_once,
                CrossingTest,
                statement(ml_stmt_block([], LoopBodyStatements),
                    MLDS_Context)),
                MLDS_Context)
        ]
    ;
        MaybeStopLoopVarLval = yes(StopLoopVarLval),
        InitStopLoopVarStatement = statement(ml_stmt_atomic(
            assign(StopLoopVarLval, ml_const(mlconst_int(0)))),
            MLDS_Context),
        StopLoopTest = ml_binop(eq,
            ml_lval(StopLoopVarLval), ml_const(mlconst_int(0))),
        LoopBodyStatements = [
            AssignMidVarStatement,
            AssignResultVarStatement,
            % SuccessStatement should set StopLoopVarLval to 1.
            statement(ml_stmt_if_then_else(ResultTest,
                SuccessStatement, yes(UpdateLoOrHiVarStatement)),
                MLDS_Context)
        ],
        SearchStatements = [
            InitLoVarStatement,
            InitHiVarStatement,
            InitStopLoopVarStatement,
            statement(ml_stmt_while(loop_at_least_once,
                ml_binop(logical_and, StopLoopTest, CrossingTest),
                statement(ml_stmt_block([], LoopBodyStatements),
                    MLDS_Context)),
                MLDS_Context)
        ]
    ),

    InitialCommentStatement =
        statement(ml_stmt_atomic(comment(InitialComment)), MLDS_Context),
    Statements = [InitialCommentStatement | SetupForFailStatements] ++
        SearchStatements ++ AfterStatements.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Code useful for more than one kind of string switch.
%

:- pred ml_should_use_stop_loop(mlds_context::in, bool::in,
    maybe(mlds_lval)::out, list(mlds_defn)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_should_use_stop_loop(MLDS_Context, LoopPresent,
        MaybeStopLoopLval, StopLoopLvalDefns, !Info) :-
    (
        LoopPresent = no,
        UseStopLoop = no
    ;
        LoopPresent = yes,
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        module_info_get_globals(ModuleInfo, Globals),
        SupportsGoto = globals_target_supports_goto(Globals),
        globals.lookup_string_option(Globals, experiment, Experiment),
        (
            SupportsGoto = yes,
            ( Experiment = "use_stop_loop" ->
                UseStopLoop = yes
            ;
                UseStopLoop = no
            )
        ;
            SupportsGoto = no,
            UseStopLoop = yes
        )
    ),
    (
        UseStopLoop = no,
        MaybeStopLoopLval = no,
        StopLoopLvalDefns = []
    ;
        UseStopLoop = yes,
        % On targets that do not support gotos or break, after we have
        % handled a match, we set the stop loop flag, which will cause the
        % next test of the loop condition to fail.

        StopLoopType = mlds_native_int_type,
        % We never need to trace ints.
        StopLoopGCStatement = gc_no_stmt,

        ml_gen_info_new_aux_var_name("stop_loop", StopLoopVar, !Info),
        StopLoopVarDefn = ml_gen_mlds_var_decl(mlds_data_var(StopLoopVar),
            StopLoopType, StopLoopGCStatement, MLDS_Context),
        ml_gen_var_lval(!.Info, StopLoopVar, StopLoopType, StopLoopVarLval),
        MaybeStopLoopLval = yes(StopLoopVarLval),
        StopLoopLvalDefns = [StopLoopVarDefn]
    ).

:- pred ml_gen_maybe_switch_failure(code_model::in, can_fail::in,
    prog_context::in, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_maybe_switch_failure(CodeModel, CanFail, Context, FailStatements,
        !Info) :-
    % We used to include comments in FailStatements. However, that would
    % complicate the task of ml_wrap_loop_break, which needs to decide
    % whether FailStatements actually contains executable code.
    (
        CanFail = cannot_fail,
        % This can happen if the initial inst of the switched-on variable
        % shows that we know a finite set of strings that the variable can be
        % bound to.
        FailStatements = []
    ;
        CanFail = can_fail,
        ml_gen_failure(CodeModel, Context, FailStatements, !Info)
    ).

    % ml_wrap_loop_break(CodeModel, LoopPresent,
    %   MLDS_Context, MaybeStopLoopVarLval, MatchDefns, MatchStatements,
    %   SetupForFailStatements, BodyStatement, AfterStatements, !Info)
    %
    % MatchStatements should be the statements that we execute once we find
    % a match, and OnlyFailAfterStatements should be the statements that we
    % want to execute after the search loop if the loop did NOT find a match.
    %
    % This predicate wraps up MatchStatements with both MatchDefns and with
    % other following code that causes execution to exit the loop
    % after a match, and returns the resulting code as BodyStatement.
    %
    % We also return SetupForFailStatements and AfterStatements.
    % SetupForFailStatements will be code to put before the loop, to set up
    % for possible failure to find a match.
    %
    % AfterStatements will be code to put after the loop. It will contain
    % OnlyFailAfterStatements, wrapped up in a test if necessary, as well as
    % any code needed to enable BodyStatement to break out of the loop
    % on a match.
    %
:- pred ml_wrap_loop_break(code_model::in, bool::in, mlds_context::in,
    maybe(mlds_lval)::in, list(mlds_defn)::in, list(statement)::in,
    list(statement)::in,
    list(statement)::out, statement::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_wrap_loop_break(CodeModel, LoopPresent, MLDS_Context, MaybeStopLoopVarLval,
        MatchDefns, MatchStatements, FailStatements,
        SetupForFailStatements, BodyStatement, AfterStatements, !Info) :-
    (
        CodeModel = model_det,
        SetupForFailStatements = [],
        expect(unify(FailStatements, []), $module, $pred,
            "model_det, but FailStatements is not empty"),
        OnlyFailAfterStatements = []
    ;
        CodeModel = model_semi,
        (
            MaybeStopLoopVarLval = no,
            SetupForFailStatements = [],
            OnlyFailAfterStatements = FailStatements
        ;
            MaybeStopLoopVarLval = yes(_),
            % It is more efficient to set up the default value of the succeeded
            % variable (FALSE) with an unconditional assignment than it is
            % to set up its actual value with a conditional assignment.
            SetupForFailStatements = FailStatements,
            OnlyFailAfterStatements = []
        )
    ;
        CodeModel = model_non,
        SetupForFailStatements = [],
        expect(unify(FailStatements, []), $module, $pred,
            "model_non, but FailStatements is not empty"),
        OnlyFailAfterStatements = []
    ),
    (
        MaybeStopLoopVarLval = no,
        (
            LoopPresent = no,
            OnlyFailAfterStatements = []
        ->
            BodyStatement =
                statement(ml_stmt_block(MatchDefns, MatchStatements),
                    MLDS_Context),
            AfterStatements = []
        ;
            ml_gen_info_get_module_info(!.Info, ModuleInfo),
            module_info_get_globals(ModuleInfo, Globals),
            SupportsBreakContinue =
                globals_target_supports_break_and_continue(Globals),
            globals.lookup_string_option(Globals, experiment, Experiment),
            (
                SupportsBreakContinue = yes,
                OnlyFailAfterStatements = [],
                Experiment \= "use_end_label"
            ->
                BreakCommentStatement = statement(ml_stmt_atomic(
                    comment("break out of search loop")), MLDS_Context),
                BreakStatement =
                    statement(ml_stmt_goto(goto_break), MLDS_Context),
                BodyStatement =
                    statement(ml_stmt_block(MatchDefns,
                        MatchStatements ++
                            [BreakCommentStatement, BreakStatement]),
                        MLDS_Context),
                AfterStatements = []
            ;
                ml_gen_new_label(EndLabel, !Info),
                GotoCommentStatement = statement(ml_stmt_atomic(
                    comment("jump out of search loop")), MLDS_Context),
                GotoEndStatement =
                    statement(ml_stmt_goto(goto_label(EndLabel)),
                        MLDS_Context),
                BodyStatement =
                    statement(ml_stmt_block(MatchDefns,
                        MatchStatements ++
                            [GotoCommentStatement, GotoEndStatement]),
                        MLDS_Context),
                EndLabelStatement =
                    statement(ml_stmt_label(EndLabel), MLDS_Context),
                AfterStatements =
                    OnlyFailAfterStatements ++ [EndLabelStatement]
            )
        )
    ;
        MaybeStopLoopVarLval = yes(StopLoopVarLval),
        (
            LoopPresent = no,
            OnlyFailAfterStatements = []
        ->
            BodyStatement =
                statement(ml_stmt_block(MatchDefns, MatchStatements),
                    MLDS_Context)
        ;
            SetStopLoopStatement =
                statement(ml_stmt_atomic(
                    assign(StopLoopVarLval, ml_const(mlconst_int(1)))),
                    MLDS_Context),
            BodyStatement =
                statement(ml_stmt_block(MatchDefns,
                    MatchStatements ++ [SetStopLoopStatement]),
                    MLDS_Context)
        ),
        (
            OnlyFailAfterStatements = [],
            AfterStatements = []
        ;
            (
                OnlyFailAfterStatements = [OnlyFailAfterStatement]
            ;
                OnlyFailAfterStatements = [_, _ | _],
                OnlyFailAfterStatement = statement(
                    ml_stmt_block([], OnlyFailAfterStatements),
                    MLDS_Context)
            ),
            SuccessTest = ml_binop(eq,
                ml_lval(StopLoopVarLval),
                ml_const(mlconst_int(0))),
            AfterStatement =
                statement(
                    ml_stmt_if_then_else(SuccessTest,
                        OnlyFailAfterStatement, no),
                    MLDS_Context),
            AfterStatements = [AfterStatement]
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module ml_backend.ml_string_switch.
%-----------------------------------------------------------------------------%
