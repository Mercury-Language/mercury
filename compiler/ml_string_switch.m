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
% For switches on strings, we can generate either
% - a hash table using open addressing to resolve hash conflicts, or
% - a sorted table for binary search.
%
% The hash table has a higher startup cost, but should use fewer comparisons,
% so it is preferred for bigger tables.
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

:- pred ml_generate_string_hash_jump_switch(list(tagged_case)::in,
    prog_var::in, code_model::in, can_fail::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

:- pred ml_generate_string_hash_lookup_switch(prog_var::in,
    ml_lookup_switch_info(string)::in,
    code_model::in, can_fail::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

:- pred ml_generate_string_binary_jump_switch(list(tagged_case)::in,
    prog_var::in, code_model::in, can_fail::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

:- pred ml_generate_string_binary_lookup_switch(prog_var::in,
    ml_lookup_switch_info(string)::in,
    code_model::in, can_fail::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.switch_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module libs.globals.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.ml_simplify_switch.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.builtin_lib_types.

:- import_module assoc_list.
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

ml_generate_string_hash_jump_switch(Cases, Var, CodeModel, CanFail, Context,
        Defns, Statements, !Info) :-
    ml_gen_string_hash_switch_search_vars(Context, Var, HashSearchInfo, !Info),
    HashSearchInfo = ml_hash_search_info(MLDS_Context, _VarRval,
        SlotVarLval, _StringVarLval, Defns),

    ml_gen_new_label(EndLabel, !Info),
    GotoEndStatement =
        statement(ml_stmt_goto(goto_label(EndLabel)), MLDS_Context),

    gen_tagged_case_codes_for_string_switch(CodeModel, Cases, StrsCaseNums,
        map.init, CodeMap, !Info),

    % Compute the hash table.
    construct_string_hash_cases(StrsCaseNums, allow_doubling,
        TableSize, HashSlotMap, HashOp, NumCollisions),
    HashMask = TableSize - 1,

    % Generate the code for when the hash lookup fails.
    ml_gen_maybe_switch_failure(CodeModel, CanFail, Context, MLDS_Context,
        FailStatements, !Info),

    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_gen_info_get_target(!.Info, Target),

    MLDS_StringType = mercury_type_to_mlds_type(ModuleInfo, string_type),
    MLDS_IntType = mlds_native_int_type,

    ( NumCollisions = 0 ->
        MLDS_ArgTypes = [MLDS_StringType]
    ;
        MLDS_ArgTypes = [MLDS_StringType, MLDS_IntType]
    ),

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
    FoundMatchStatement = statement(
        ml_stmt_block([], [
            statement(ml_stmt_atomic(comment(FoundMatchComment)),
                MLDS_Context),
            SwitchStatement,
            GotoEndStatement
        ]),
        MLDS_Context),

    InitialComment = "hashed string jump switch",
    ml_gen_string_hash_switch_search(InitialComment, HashSearchInfo, HashOp,
        VectorCommon, StructType, StringFieldId, MaybeNextSlotFieldId,
        HashMask, FoundMatchStatement, HashLookupStatements),

    EndLabelStatement = statement(ml_stmt_label(EndLabel), MLDS_Context),
    EndComment =
        statement(ml_stmt_atomic(comment("end of hashed string switch")),
            MLDS_Context),

    % Collect all the generated code fragments together.
    Statements = HashLookupStatements ++ FailStatements ++
        [EndLabelStatement, EndComment].

%-----------------------------------------------------------------------------%

:- pred gen_tagged_case_codes_for_string_switch(code_model::in,
    list(tagged_case)::in, assoc_list(string, int)::out,
    map(int, statement)::in, map(int, statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_tagged_case_codes_for_string_switch(_CodeModel, [], [],
        !CodeMap, !Info).
gen_tagged_case_codes_for_string_switch(CodeModel, [TaggedCase | TaggedCases],
        !:StrsCaseNums, !CodeMap, !Info) :-
    gen_tagged_case_code_for_string_switch(CodeModel,
        TaggedCase, CaseNum, !CodeMap, !Info),
    gen_tagged_case_codes_for_string_switch(CodeModel,
        TaggedCases, !:StrsCaseNums, !CodeMap, !Info),
    TaggedCase = tagged_case(MainTaggedConsId, OtherTaggedConsIds, _, _),
    add_to_strs_casenums(CaseNum, MainTaggedConsId, !StrsCaseNums),
    list.foldl(add_to_strs_casenums(CaseNum), OtherTaggedConsIds,
        !StrsCaseNums).

:- pred add_to_strs_casenums(int::in, tagged_cons_id::in,
    assoc_list(string, int)::in, assoc_list(string, int)::out) is det.

add_to_strs_casenums(CaseNum, TaggedConsId, !StrsCaseNums) :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    ( ConsTag = string_tag(String) ->
        !:StrsCaseNums = [String - CaseNum | !.StrsCaseNums]
    ;
        unexpected($module, $pred, "non-string tag")
    ).

:- pred gen_tagged_case_code_for_string_switch_dummy(code_model::in,
    tagged_case::in, int::out,
    map(int, statement)::in, map(int, statement)::out,
    ml_gen_info::in, ml_gen_info::out, unit::in, unit::out) is det.

gen_tagged_case_code_for_string_switch_dummy(CodeModel, TaggedCase, CaseNum,
        !CodeMap, !Info, !Dummy) :-
    gen_tagged_case_code_for_string_switch(CodeModel, TaggedCase, CaseNum,
        !CodeMap, !Info).

:- pred gen_tagged_case_code_for_string_switch(code_model::in, tagged_case::in,
    int::out, map(int, statement)::in, map(int, statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_tagged_case_code_for_string_switch(CodeModel, TaggedCase, CaseNum,
        !CodeMap, !Info) :-
    TaggedCase = tagged_case(MainTaggedConsId, OtherTaggedConsIds,
        CaseNum, Goal),
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
    map.det_insert(CaseNum, CaseStatement, !CodeMap).

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
:- type hash_slot_rev_map == map(int, hash_slots).

:- pred ml_gen_string_hash_jump_slots(int::in, int::in,
    map(int, string_hash_slot(int))::in,
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
    map(int, string_hash_slot(int))::in,
    mlds_type::in, maybe(mlds_field_id)::in, mlds_initializer::out,
    hash_slot_rev_map::in, hash_slot_rev_map::out) is det.

ml_gen_string_hash_jump_slot(Slot, HashSlotMap, StructType,
        MaybeNextSlotId, RowInitializer, !RevMap) :-
    ( map.search(HashSlotMap, Slot, HashSlotMapEntry) ->
        HashSlotMapEntry = string_hash_slot(String, Next, CaseNum),
        StringRval = ml_const(mlconst_string(String)),
        NextSlotRval = ml_const(mlconst_int(Next)),
        ( map.search(!.RevMap, CaseNum, OldEntry) ->
            OldEntry = hash_slots(OldFirstSlot, OldLaterSlots),
            NewEntry = hash_slots(OldFirstSlot, [Slot | OldLaterSlots]),
            map.det_update(CaseNum, NewEntry, !RevMap)
        ;
            NewEntry = hash_slots(Slot, []),
            map.det_insert(CaseNum, NewEntry, !RevMap)
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

:- pred generate_string_jump_switch_arms(map(int, statement)::in,
    assoc_list(int, hash_slots)::in,
    list(mlds_switch_case)::in, list(mlds_switch_case)::out) is det.

generate_string_jump_switch_arms(_, [], !Cases).
generate_string_jump_switch_arms(CodeMap, [Entry | Entries], !Cases) :-
    Entry = CaseNum - HashSlots,
    HashSlots = hash_slots(FirstHashSlot, RevLaterHashSlots),
    list.reverse(RevLaterHashSlots, LaterHashSlots),
    FirstMatchCond = make_hash_match(FirstHashSlot),
    LaterMatchConds = list.map(make_hash_match, LaterHashSlots),
    map.lookup(CodeMap, CaseNum, CaseStatement),
    Case = mlds_switch_case(FirstMatchCond, LaterMatchConds, CaseStatement),
    !:Cases = [Case | !.Cases],
    generate_string_jump_switch_arms(CodeMap, Entries, !Cases).

:- func make_hash_match(int) = mlds_case_match_cond.

make_hash_match(Slot) = match_value(ml_const(mlconst_int(Slot))).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

ml_generate_string_hash_lookup_switch(Var, LookupSwitchInfo, CodeModel,
        CanFail, Context, Defns, Statements, !Info) :-
    ml_gen_string_hash_switch_search_vars(Context, Var, HashSearchInfo, !Info),
    Defns = HashSearchInfo ^ mhsi_defns,
    MLDS_Context = HashSearchInfo ^ mhsi_mlds_context,

    % Generate the code for when the lookup fails.
    ml_gen_maybe_switch_failure(CodeModel, CanFail, Context, MLDS_Context,
        FailStatements, !Info),

    LookupSwitchInfo = ml_lookup_switch_info(CaseConsts, OutVars, OutTypes),
    (
        CaseConsts = all_one_soln(CaseValuePairs),
        ml_generate_string_hash_simple_lookup_switch(CodeModel, CaseValuePairs,
            OutVars, OutTypes, Context, HashSearchInfo,
            FailStatements, Statements, !Info)
    ;
        CaseConsts = some_several_solns(CaseSolns, _Unit),
        expect(unify(CodeModel, model_non), $module, $pred,
            "CodeModel != model_non"),
        ml_generate_string_hash_several_soln_lookup_switch(CaseSolns,
            OutVars, OutTypes, Context, HashSearchInfo,
            FailStatements, Statements, !Info)
    ).

%-----------------------------------------------------------------------------%

:- pred ml_generate_string_hash_simple_lookup_switch(code_model::in,
    assoc_list(string, list(mlds_rval))::in,
    list(prog_var)::in, list(mlds_type)::in,
    prog_context::in, ml_hash_search_info::in,
    list(statement)::in, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_string_hash_simple_lookup_switch(CodeModel, CaseValuePairs,
        OutVars, OutTypes, Context, HashSearchInfo,
        FailStatements, Statements, !Info) :-
    HashSearchInfo = ml_hash_search_info(MLDS_Context, _VarRval,
        SlotVarLval, _StringVarLval, _Defns),
    SlotVarRval = ml_lval(SlotVarLval),

    ml_gen_new_label(EndLabel, !Info),
    GotoEndStatement =
        statement(ml_stmt_goto(goto_label(EndLabel)), MLDS_Context),

    % Compute the hash table.
    construct_string_hash_cases(CaseValuePairs, allow_doubling,
        TableSize, HashSlotMap, HashOp, NumCollisions),
    HashMask = TableSize - 1,

    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_gen_info_get_target(!.Info, Target),

    MLDS_StringType = mercury_type_to_mlds_type(ModuleInfo, string_type),
    MLDS_IntType = mlds_native_int_type,

    ( NumCollisions = 0 ->
        MLDS_ArgTypes = [MLDS_StringType | OutTypes]
    ;
        MLDS_ArgTypes = [MLDS_StringType, MLDS_IntType | OutTypes]
    ),

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
        FoundMatchStatement = statement(
            ml_stmt_block([],
                [FoundMatchCommentStatement | LookupStatements] ++
                [GotoEndStatement]
            ),
            MLDS_Context)
    ;
        CodeModel = model_semi,
        ml_gen_set_success(!.Info, ml_const(mlconst_true), Context,
            SetSuccessTrueStatement),
        FoundMatchStatement = statement(
            ml_stmt_block([],
                [FoundMatchCommentStatement | LookupStatements] ++
                [SetSuccessTrueStatement, GotoEndStatement]
            ),
            MLDS_Context)
    ;
        CodeModel = model_non,
        unexpected($module, $pred, "model_non")
    ),

    InitialComment = "hashed string simple lookup switch",
    ml_gen_string_hash_switch_search(InitialComment, HashSearchInfo, HashOp,
        VectorCommon, StructType, StringFieldId, MaybeNextSlotFieldId,
        HashMask, FoundMatchStatement, HashLookupStatements),

    EndLabelStatement = statement(ml_stmt_label(EndLabel), MLDS_Context),
    EndComment =
        statement(ml_stmt_atomic(comment("end of hashed string switch")),
            MLDS_Context),

    % Collect all the generated code fragments together.
    Statements = HashLookupStatements ++ FailStatements ++
        [EndLabelStatement, EndComment].

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

:- pred ml_generate_string_hash_several_soln_lookup_switch(
    assoc_list(string, soln_consts(mlds_rval))::in,
    list(prog_var)::in, list(mlds_type)::in,
    prog_context::in, ml_hash_search_info::in,
    list(statement)::in, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_string_hash_several_soln_lookup_switch(CaseSolns,
        OutVars, OutTypes, Context, HashSearchInfo,
        FailStatements, Statements, !Info) :-
    HashSearchInfo = ml_hash_search_info(MLDS_Context, _VarRval,
        SlotVarLval, _StringVarLval, _Defns),
    SlotVarRval = ml_lval(SlotVarLval),

    make_several_soln_lookup_vars(MLDS_Context, SeveralSolnLookupVars, !Info),
    SeveralSolnLookupVars = ml_several_soln_lookup_vars(NumLaterSolnsVarLval,
        LaterSlotVarLval, LimitVarLval,
        LimitAssignStatement, IncrLaterSlotVarStatement, Defns),
    LaterSlotVarRval = ml_lval(LaterSlotVarLval),
    LimitVarRval = ml_lval(LimitVarLval),

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
            MLDS_IntType, MLDS_IntType | OutTypes]
    ;
        FirstSolnFieldTypes = [MLDS_StringType, MLDS_IntType,
            MLDS_IntType, MLDS_IntType | OutTypes]
    ),

    ml_gen_info_get_global_data(!.Info, GlobalData0),
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        FirstSolnFieldTypes, FirstSolnStructTypeNum, FirstSolnStructType,
        FirstSolnFieldIds, GlobalData0, GlobalData1),
    ml_gen_static_vector_type(MLDS_ModuleName, MLDS_Context, Target,
        OutTypes, LaterSolnStructTypeNum, LaterSolnStructType,
        LaterSolnFieldIds, GlobalData1, GlobalData2),
    ( NumCollisions = 0 ->
        (
            FirstSolnFieldIds = [StringFieldIdPrime,
                NumLaterSolnsFieldIdPrime, FirstLaterSolnRowFieldIdPrime
                | OutFieldIdsPrime]
        ->
            StringFieldId = StringFieldIdPrime,
            NumLaterSolnsFieldId = NumLaterSolnsFieldIdPrime,
            FirstLaterSolnRowFieldId = FirstLaterSolnRowFieldIdPrime,
            OutFieldIds = OutFieldIdsPrime,
            MaybeNextSlotFieldId = no
        ;
            unexpected($module, $pred, "bad FieldIds")
        )
    ;
        (
            FirstSolnFieldIds =
                [StringFieldIdPrime, NextSlotFieldIdPrime,
                NumLaterSolnsFieldIdPrime, FirstLaterSolnRowFieldIdPrime
                | OutFieldIdsPrime]
        ->
            StringFieldId = StringFieldIdPrime,
            NumLaterSolnsFieldId = NumLaterSolnsFieldIdPrime,
            FirstLaterSolnRowFieldId = FirstLaterSolnRowFieldIdPrime,
            OutFieldIds = OutFieldIdsPrime,
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

    ml_generate_field_assign(NumLaterSolnsVarLval, MLDS_IntType,
        NumLaterSolnsFieldId, FirstSolnVectorCommon, FirstSolnStructType,
        SlotVarRval, MLDS_Context, NumLaterSolnsAssignStatement, !Info),
    ml_generate_field_assign(LaterSlotVarLval, MLDS_IntType,
        FirstLaterSolnRowFieldId, FirstSolnVectorCommon, FirstSolnStructType,
        SlotVarRval, MLDS_Context, LaterSlotVarAssignStatement, !Info),
    ml_generate_field_assigns(OutVars, OutTypes, OutFieldIds,
        FirstSolnVectorCommon, FirstSolnStructType,
        SlotVarRval, MLDS_Context, FirstSolnLookupStatements, !Info),
    ml_generate_field_assigns(OutVars, OutTypes, LaterSolnFieldIds,
        LaterSolnVectorCommon, LaterSolnStructType,
        LaterSlotVarRval, MLDS_Context, LaterSolnLookupStatements, !Info),

    ml_gen_call_current_success_cont(Context, CallContStatement, !Info),
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

    ml_gen_new_label(FailLabel, !Info),
    GotoFailStatement =
        statement(ml_stmt_goto(goto_label(FailLabel)), MLDS_Context),
    SuccessStmt = ml_stmt_block(Defns, [
        NumLaterSolnsAssignStatement, FirstLookupSucceedStatement,
        LaterSlotVarAssignStatement, LimitAssignStatement,
        MoreSolnsLoopStatement, GotoFailStatement
    ]),
    SuccessStatement = statement(SuccessStmt, MLDS_Context),

    InitialComment = "hashed string several_soln lookup switch",
    ml_gen_string_hash_switch_search(InitialComment, HashSearchInfo, HashOp,
        FirstSolnVectorCommon, FirstSolnStructType, StringFieldId,
        MaybeNextSlotFieldId, HashMask,
        SuccessStatement, LookupStatements),
    FailLabelStatement = statement(ml_stmt_label(FailLabel), MLDS_Context),
    Statements = LookupStatements ++ [FailLabelStatement | FailStatements].

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
                mhsi_mlds_context               :: mlds_context,
                mhsi_switch_var                 :: mlds_rval,
                mhsi_slot_var                   :: mlds_lval,
                mhsi_string_var                 :: mlds_lval,
                mhsi_defns                      :: list(mlds_defn)
            ).

:- pred ml_gen_string_hash_switch_search_vars(prog_context::in, prog_var::in,
    ml_hash_search_info::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_string_hash_switch_search_vars(Context, Var, HashSearchInfo, !Info) :-
    MLDS_Context = mlds_make_context(Context),
    ml_gen_var(!.Info, Var, VarLval),
    VarRval = ml_lval(VarLval),

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

    Defns = [SlotVarDefn, StringVarDefn],
    HashSearchInfo = ml_hash_search_info(MLDS_Context, VarRval,
        SlotVarLval, StringVarLval, Defns).

:- pred ml_gen_string_hash_switch_search(string::in, ml_hash_search_info::in,
    unary_op::in, mlds_vector_common::in, mlds_type::in,
    mlds_field_id::in, maybe(mlds_field_id)::in, int::in,
    statement::in, list(statement)::out) is det.

ml_gen_string_hash_switch_search(InitialComment, HashSearchInfo, HashOp,
        VectorCommon, StructType, StringFieldId, MaybeNextSlotFieldId,
        HashMask, FoundMatchStatement, HashLookupStatements) :-
    HashSearchInfo = ml_hash_search_info(MLDS_Context, VarRval,
        SlotVarLval, StringVarLval, _Defns),
    SlotVarRval = ml_lval(SlotVarLval),
    StringVarRval = ml_lval(StringVarLval),
    SlotVarType = mlds_native_int_type,
    StringVarType = ml_string_type,

    PrepareForMatchStatements = [
        statement(ml_stmt_atomic(comment(InitialComment)), MLDS_Context),
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
            ml_binop(ne, StringVarRval,
                ml_const(mlconst_null(StringVarType))),
            ml_binop(str_eq, StringVarRval, VarRval)
        ),
    LookForMatchStatements = [
        statement(ml_stmt_atomic(comment(
            "lookup the string for this hash slot")), MLDS_Context),
        statement(
            ml_stmt_atomic(assign(StringVarLval,
                ml_lval(ml_field(yes(0),
                    ml_vector_common_row(VectorCommon, SlotVarRval),
                    StringFieldId, StringVarType, StructType)))),
            MLDS_Context),
        statement(ml_stmt_atomic(comment("did we find a match?")),
            MLDS_Context),
        statement(ml_stmt_if_then_else(FoundMatchCond,
            FoundMatchStatement, no),
            MLDS_Context)
    ],
    (
        MaybeNextSlotFieldId = yes(NextSlotFieldId),
        NoMatchStatements = [
            statement(ml_stmt_atomic(comment(
                "no match yet, so get next slot in hash chain")),
                MLDS_Context),
            statement(
                ml_stmt_atomic(assign(SlotVarLval,
                    ml_lval(ml_field(yes(0),
                        ml_vector_common_row(VectorCommon, SlotVarRval),
                        NextSlotFieldId, SlotVarType, StructType)))),
                MLDS_Context)
        ],

        LoopBody = statement(ml_stmt_block([],
            LookForMatchStatements ++ NoMatchStatements), MLDS_Context),

        LoopStatements = [
            statement(ml_stmt_atomic(comment("hash chain loop")),
                MLDS_Context),
            statement(
                ml_stmt_while(loop_at_least_once,
                    ml_binop(int_ge, SlotVarRval, ml_const(mlconst_int(0))),
                    LoopBody),
                MLDS_Context)
            ],

        HashLookupStatements =
            PrepareForMatchStatements ++ LoopStatements
    ;
        MaybeNextSlotFieldId = no,
        NoLoopStatements = [
            statement(ml_stmt_atomic(
                comment("no collisions; no hash chain loop")), MLDS_Context)
            ],

        HashLookupStatements =
            PrepareForMatchStatements ++ LookForMatchStatements ++
            NoLoopStatements
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

ml_generate_string_binary_jump_switch(Cases, Var, CodeModel, CanFail, Context,
        Defns, Statements, !Info) :-
    ml_gen_string_binary_switch_search_vars(Context, Var, BinarySearchInfo,
        !Info),
    BinarySearchInfo = ml_binary_search_info(MLDS_Context, _VarRval,
        _LoVarLval, _HiVarLval, MidVarLval, _ResultVarLval, Defns),

    % Generate the code for when the lookup fails.
    ml_gen_maybe_switch_failure(CodeModel, CanFail, Context, MLDS_Context,
        FailStatements, !Info),

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

    ml_gen_new_label(EndLabel, !Info),
    GotoEndStatement =
        statement(ml_stmt_goto(goto_label(EndLabel)), MLDS_Context),
    SuccessStatement =
        statement(ml_stmt_block([], [SwitchStatement, GotoEndStatement]),
            MLDS_Context),

    % Generate the code that searches the table.
    ml_gen_string_binary_switch_search(BinarySearchInfo, VectorCommon,
        TableSize, StructType, StringFieldId,
        SuccessStatement, LookupStatements, !.Info),
    InitialComment = "binary string jump switch",
    CommentStatement = statement(ml_stmt_atomic(comment(InitialComment)),
        MLDS_Context),
    EndLabelStatement = statement(ml_stmt_label(EndLabel), MLDS_Context),
    Statements = [CommentStatement | LookupStatements] ++
        FailStatements ++ [EndLabelStatement].

:- pred ml_gen_string_binary_jump_initializers(assoc_list(string, int)::in,
    mlds_type::in,
    list(mlds_initializer)::in, list(mlds_initializer)::out,
    int::in, int::out) is det.

ml_gen_string_binary_jump_initializers([],
        _StructType, !RevRowInitializers, !CurIndex).
ml_gen_string_binary_jump_initializers([Str - CaseNum | StrCaseNums],
        StructType, !RevRowInitializers, !CurIndex) :-
    StrRval = ml_const(mlconst_string(Str)),
    CaseNumRval = ml_const(mlconst_int(CaseNum)),
    RowInitializer = init_struct(StructType,
        [init_obj(StrRval), init_obj(CaseNumRval)]),
    !:RevRowInitializers = [RowInitializer | !.RevRowInitializers],
    !:CurIndex = !.CurIndex + 1,
    ml_gen_string_binary_jump_initializers(StrCaseNums,
        StructType, !RevRowInitializers, !CurIndex).

:- pred ml_gen_string_binary_jump_switch_arms(assoc_list(int, statement)::in,
    list(mlds_switch_case)::in, list(mlds_switch_case)::out) is det.

ml_gen_string_binary_jump_switch_arms([], !SwitchCases).
ml_gen_string_binary_jump_switch_arms([CaseNumStmt | CaseNumsStmts],
        !SwitchCases) :-
    CaseNumStmt = CaseNum - Statement,
    MatchCond = match_value(ml_const(mlconst_int(CaseNum))),
    SwitchCase = mlds_switch_case(MatchCond, [], Statement),
    !:SwitchCases = [SwitchCase | !.SwitchCases],
    ml_gen_string_binary_jump_switch_arms(CaseNumsStmts, !SwitchCases).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

ml_generate_string_binary_lookup_switch(Var, LookupSwitchInfo, CodeModel,
        CanFail, Context, Defns, Statements, !Info) :-
    ml_gen_string_binary_switch_search_vars(Context, Var, BinarySearchInfo,
        !Info),
    Defns = BinarySearchInfo ^ mbsi_defns,
    MLDS_Context = BinarySearchInfo ^ mbsi_mlds_context,

    % Generate the code for when the lookup fails.
    ml_gen_maybe_switch_failure(CodeModel, CanFail, Context, MLDS_Context,
        FailStatements, !Info),

    LookupSwitchInfo = ml_lookup_switch_info(CaseConsts, OutVars, OutTypes),
    (
        CaseConsts = all_one_soln(CaseValuePairs),
        ml_generate_string_binary_simple_lookup_switch(CodeModel,
            CaseValuePairs, OutVars, OutTypes, Context,
            BinarySearchInfo, FailStatements, Statements, !Info)
    ;
        CaseConsts = some_several_solns(CaseSolns, _Unit),
        expect(unify(CodeModel, model_non), $module, $pred,
            "CodeModel != model_non"),
        ml_generate_string_binary_several_soln_lookup_switch(CaseSolns,
            OutVars, OutTypes, Context, BinarySearchInfo,
            FailStatements, Statements, !Info)
    ).

%-----------------------------------------------------------------------------%

:- pred ml_generate_string_binary_simple_lookup_switch(code_model::in,
    assoc_list(string, list(mlds_rval))::in,
    list(prog_var)::in, list(mlds_type)::in,
    prog_context::in, ml_binary_search_info::in,
    list(statement)::in, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_string_binary_simple_lookup_switch(CodeModel, CaseValuePairs0,
        OutVars, OutTypes, Context, BinarySearchInfo,
        FailStatements, Statements, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_gen_info_get_target(!.Info, Target),
    MLDS_Context = BinarySearchInfo ^ mbsi_mlds_context,

    MLDS_StringType = mercury_type_to_mlds_type(ModuleInfo, string_type),
    MLDS_ArgTypes = [MLDS_StringType | OutTypes],

    % Generate the binary search table.
    list.sort(CaseValuePairs0, CaseValuePairs),
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
    ml_gen_string_binary_simple_lookup_initializers(CaseValuePairs, StructType,
        [], RevRowInitializers, 0, TableSize),
    list.reverse(RevRowInitializers, RowInitializers),
    ml_gen_static_vector_defn(MLDS_ModuleName, StructTypeNum, RowInitializers,
        VectorCommon, GlobalData1, GlobalData),
    ml_gen_info_set_global_data(GlobalData, !Info),

    MidVarLval = BinarySearchInfo ^ mbsi_mid_var,
    ml_generate_field_assigns(OutVars, OutTypes, OutFieldIds,
        VectorCommon, StructType, ml_lval(MidVarLval), MLDS_Context,
        GetArgStatements, !Info),

    ml_gen_new_label(EndLabel, !Info),
    GotoEndStatement =
        statement(ml_stmt_goto(goto_label(EndLabel)), MLDS_Context),
    (
        CodeModel = model_det,
        SuccessStatement = statement(
            ml_stmt_block([], GetArgStatements ++ [GotoEndStatement]),
            MLDS_Context)
    ;
        CodeModel = model_semi,
        ml_gen_set_success(!.Info, ml_const(mlconst_true), Context,
            SetSuccessTrueStatement),
        SuccessStatement = statement(
            ml_stmt_block([],
                GetArgStatements ++
                [SetSuccessTrueStatement, GotoEndStatement]),
            MLDS_Context)
    ;
        CodeModel = model_non,
        unexpected($module, $pred, "model_non")
    ),

    % Generate the code that searches the table.
    ml_gen_string_binary_switch_search(BinarySearchInfo, VectorCommon,
        TableSize, StructType, StringFieldId,
        SuccessStatement, LookupStatements, !.Info),
    InitialComment = "binary string simple lookup switch",
    CommentStatement = statement(ml_stmt_atomic(comment(InitialComment)),
        MLDS_Context),
    EndLabelStatement = statement(ml_stmt_label(EndLabel), MLDS_Context),
    Statements = [CommentStatement | LookupStatements] ++
        FailStatements ++ [EndLabelStatement].

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

:- pred ml_generate_string_binary_several_soln_lookup_switch(
    assoc_list(string, soln_consts(mlds_rval))::in,
    list(prog_var)::in, list(mlds_type)::in,
    prog_context::in, ml_binary_search_info::in,
    list(statement)::in, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_string_binary_several_soln_lookup_switch(CaseSolns0,
        OutVars, OutTypes, Context, BinarySearchInfo,
        FailStatements, Statements, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_gen_info_get_target(!.Info, Target),
    MLDS_Context = BinarySearchInfo ^ mbsi_mlds_context,

    make_several_soln_lookup_vars(MLDS_Context, SeveralSolnLookupVars, !Info),
    SeveralSolnLookupVars = ml_several_soln_lookup_vars(NumLaterSolnsVarLval,
        LaterSlotVarLval, LimitVarLval,
        LimitAssignStatement, IncrLaterSlotVarStatement, Defns),
    LaterSlotVarRval = ml_lval(LaterSlotVarLval),
    LimitVarRval = ml_lval(LimitVarLval),

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
        LaterSolnFieldIds, GlobalData1, GlobalData2),
    (
        FirstSolnFieldIds = [StringFieldIdPrime, NumLaterSolnsFieldIdPrime,
            FirstLaterRowFieldIdPrime | FirstSolnArgIdsPrime]
    ->
        StringFieldId = StringFieldIdPrime,
        NumLaterSolnsFieldId = NumLaterSolnsFieldIdPrime,
        FirstLaterRowFieldId = FirstLaterRowFieldIdPrime,
        FirstSolnArgIds = FirstSolnArgIdsPrime
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

    MidVarLval = BinarySearchInfo ^ mbsi_mid_var,
    MidVarRval = ml_lval(MidVarLval),
    ml_generate_field_assign(NumLaterSolnsVarLval, MLDS_IntType,
        NumLaterSolnsFieldId,
        FirstSolnVectorCommon, FirstSolnStructType, MidVarRval,
        MLDS_Context, NumLaterSolnsAssignStatement, !Info),
    ml_generate_field_assign(LaterSlotVarLval, MLDS_IntType,
        FirstLaterRowFieldId,
        FirstSolnVectorCommon, FirstSolnStructType, MidVarRval,
        MLDS_Context, LaterSlotVarAssignStatement, !Info),
    ml_generate_field_assigns(OutVars, OutTypes, FirstSolnArgIds,
        FirstSolnVectorCommon, FirstSolnStructType, MidVarRval,
        MLDS_Context, FirstSolnLookupStatements, !Info),
    ml_generate_field_assigns(OutVars, OutTypes, LaterSolnFieldIds,
        LaterSolnVectorCommon, LaterSolnStructType, LaterSlotVarRval,
        MLDS_Context, LaterSolnLookupStatements, !Info),

    ml_gen_call_current_success_cont(Context, CallContStatement, !Info),
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

    ml_gen_new_label(FailLabel, !Info),
    GotoFailStatement =
        statement(ml_stmt_goto(goto_label(FailLabel)), MLDS_Context),
    SuccessStmt = ml_stmt_block(Defns, [
        NumLaterSolnsAssignStatement, FirstLookupSucceedStatement,
        LaterSlotVarAssignStatement, LimitAssignStatement,
        MoreSolnsLoopStatement, GotoFailStatement
    ]),
    SuccessStatement = statement(SuccessStmt, MLDS_Context),

    % Generate the code that searches the table.
    ml_gen_string_binary_switch_search(BinarySearchInfo, FirstSolnVectorCommon,
        FirstSolnTableSize, FirstSolnStructType, StringFieldId,
        SuccessStatement, LookupStatements, !.Info),
    InitialComment = "binary string several soln lookup switch",
    CommentStatement = statement(ml_stmt_atomic(comment(InitialComment)),
        MLDS_Context),
    FailLabelStatement = statement(ml_stmt_label(FailLabel), MLDS_Context),
    Statements = [CommentStatement | LookupStatements] ++
        [FailLabelStatement | FailStatements].

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
                mbsi_mlds_context           :: mlds_context,
                mbsi_switch_var             :: mlds_rval,
                mbsi_lo_var                 :: mlds_lval,
                mbsi_hi_var                 :: mlds_lval,
                mbsi_mid_var                :: mlds_lval,
                mbsi_result_var             :: mlds_lval,
                mbsi_defns                  :: list(mlds_defn)
            ).

:- pred ml_gen_string_binary_switch_search_vars(prog_context::in, prog_var::in,
    ml_binary_search_info::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_string_binary_switch_search_vars(Context, Var, BinarySearchInfo,
        !Info) :-
    MLDS_Context = mlds_make_context(Context),
    ml_gen_var(!.Info, Var, VarLval),
    VarRval = ml_lval(VarLval),

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

    Defns = [LoVarDefn, HiVarDefn, MidVarDefn, ResultVarDefn],
    BinarySearchInfo = ml_binary_search_info(MLDS_Context, VarRval,
        LoVarLval, HiVarLval, MidVarLval, ResultVarLval, Defns).

:- pred ml_gen_string_binary_switch_search(ml_binary_search_info::in,
    mlds_vector_common::in, int::in, mlds_type::in, mlds_field_id::in,
    statement::in, list(statement)::out, ml_gen_info::in) is det.

ml_gen_string_binary_switch_search(BinarySearchInfo,
        VectorCommon, TableSize, StructType, StringFieldId,
        SuccessStatement, Statements, Info) :-
    BinarySearchInfo = ml_binary_search_info(MLDS_Context, VarRval,
        LoVarLval, HiVarLval, MidVarLval, ResultVarLval, _Defns),
    ml_gen_info_get_module_info(Info, ModuleInfo),
    MLDS_StringType = mercury_type_to_mlds_type(ModuleInfo, string_type),

    LoopBodyStatements = [
        statement(ml_stmt_atomic(
            assign(MidVarLval,
                ml_binop(int_div,
                    ml_binop(int_add, ml_lval(LoVarLval), ml_lval(HiVarLval)),
                    ml_const(mlconst_int(2))))),
            MLDS_Context),
        statement(ml_stmt_atomic(
            assign(ResultVarLval,
                ml_binop(str_cmp,
                    VarRval,
                    ml_lval(ml_field(yes(0),
                        ml_vector_common_row(VectorCommon,
                            ml_lval(MidVarLval)),
                    StringFieldId, MLDS_StringType, StructType))))),
            MLDS_Context),
        statement(ml_stmt_if_then_else(
            ml_binop(eq,
                ml_lval(ResultVarLval),
                ml_const(mlconst_int(0))),
            SuccessStatement,
            yes(statement(
                ml_stmt_if_then_else(
                    ml_binop(int_lt,
                        ml_lval(ResultVarLval),
                        ml_const(mlconst_int(0))),
                    statement(ml_stmt_atomic(
                        assign(HiVarLval,
                            ml_binop(int_sub,
                                ml_lval(MidVarLval),
                                ml_const(mlconst_int(1))))),
                        MLDS_Context),
                    yes(statement(ml_stmt_atomic(
                        assign(LoVarLval,
                            ml_binop(int_add,
                                ml_lval(MidVarLval),
                                ml_const(mlconst_int(1))))),
                        MLDS_Context))),
                MLDS_Context))),
            MLDS_Context)
    ],
    Statements = [
        statement(ml_stmt_atomic(
            assign(LoVarLval, ml_const(mlconst_int(0)))),
            MLDS_Context),
        statement(ml_stmt_atomic(
            assign(HiVarLval, ml_const(mlconst_int(TableSize - 1)))),
            MLDS_Context),
        statement(ml_stmt_while(may_loop_zero_times,
            ml_binop(int_le,
                ml_lval(LoVarLval),
                ml_lval(HiVarLval)),
            statement(ml_stmt_block([], LoopBodyStatements),
                MLDS_Context)),
            MLDS_Context)
        ].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Code useful for all kinds of string switches.
%

:- pred ml_gen_maybe_switch_failure(code_model::in, can_fail::in,
    prog_context::in, mlds_context::in, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_maybe_switch_failure(CodeModel, CanFail, Context, MLDS_Context,
        FailStatements, !Info) :-
    (
        CanFail = cannot_fail,
        % This can happen if the initial inst of the switched-on variable
        % shows that we know a finite set of strings that the variable can be
        % bound to.
        FailComment =
            statement(ml_stmt_atomic(comment("switch cannot fail")),
                MLDS_Context),
        FailStatements = [FailComment]
    ;
        CanFail = can_fail,
        FailComment = statement(ml_stmt_atomic(comment("no match, so fail")),
            MLDS_Context),
        ml_gen_failure(CodeModel, Context, FailStatements0, !Info),
        FailStatements = [FailComment | FailStatements0]
    ).

%-----------------------------------------------------------------------------%
:- end_module ml_backend.ml_string_switch.
%-----------------------------------------------------------------------------%
