%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ml_string_switch.m.
% Author: fjh (adapted from string_switch.m)
%
% For switches on strings, we generate a hash table using open addressing
% to resolve hash conflicts.
%
% WARNING: the code here is quite similar to the code in string_switch.m.
% Any changes here may require similar changes there and vice versa.
%
%-----------------------------------------------------------------------------%

:- module ml_backend.ml_string_switch.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.mlds.
:- import_module parse_tree.prog_data.

:- import_module list.

:- pred ml_generate_string_switch(list(tagged_case)::in, prog_var::in,
    code_model::in, can_fail::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.switch_util.
:- import_module hlds.hlds_data.
:- import_module libs.compiler_util.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_simplify_switch.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module svmap.
:- import_module unit.

%-----------------------------------------------------------------------------%

ml_generate_string_switch(Cases, Var, CodeModel, _CanFail, Context,
        Decls, Statements, !Info) :-
    MLDS_Context = mlds_make_context(Context),
    % Compute the value we're going to switch on.

    ml_gen_var(!.Info, Var, VarLval),
    VarRval = ml_lval(VarLval),

    % Generate the following local variable declarations:
    %   int slot_N;
    %   MR_String str_M;

    ml_gen_info_new_aux_var_name("slot", SlotVarName, !Info),
    SlotVarType = mlds_native_int_type,
    % We never need to trace ints.
    SlotVarGCStatement = gc_no_stmt,
    SlotVarDefn = ml_gen_mlds_var_decl(mlds_data_var(SlotVarName), SlotVarType,
        SlotVarGCStatement, MLDS_Context),
    ml_gen_var_lval(!.Info, SlotVarName, SlotVarType, SlotVarLval),

    ml_gen_info_new_aux_var_name("str", StringVarName, !Info),
    StringVarType = ml_string_type,
    % This variable always points to an element of the string_table array,
    % which are all static constants; it can never point into the heap.
    % So the GC never needs to trace it
    StringVarGCStatement = gc_no_stmt,
    StringVarDefn = ml_gen_mlds_var_decl(mlds_data_var(StringVarName),
        StringVarType, StringVarGCStatement, MLDS_Context),
    ml_gen_var_lval(!.Info, StringVarName, StringVarType, StringVarLval),

    % Generate new labels.
    ml_gen_new_label(EndLabel, !Info),
    GotoEndStatement =
        statement(ml_stmt_goto(goto_label(EndLabel)), MLDS_Context),

    % Determine how big to make the hash table. Currently we round the number
    % of cases up to the nearest power of two, and then double it. This should
    % hopefully ensure that we don't get too many hash collisions.
    list.length(Cases, NumCases),
    int.log2(NumCases, LogNumCases),
    int.pow(2, LogNumCases, RoundedNumCases),
    TableSize = 2 * RoundedNumCases,
    HashMask = TableSize - 1,

    % Compute the hash table.
    string_hash_cases(Cases, HashMask,
        gen_tagged_case_code_for_string_switch(CodeModel),
        map.init, CodeMap, unit, _, !Info, HashValsMap),
    map.to_assoc_list(HashValsMap, HashValsList),
    calc_string_hash_slots(HashValsList, HashValsMap, HashSlotsMap),

    % Generate the code for when the hash lookup fails.
    (
        CodeModel = model_det,
        % This can happen if the initial inst of the switched-on variable
        % shows that we know a finite set of strings that the variable can be
        % bound to.
        FailComment =
            statement(ml_stmt_atomic(comment("switch cannot fail")),
                MLDS_Context),
        FailStatements = []
    ;
        ( CodeModel = model_semi
        ; CodeModel = model_non
        ),
        FailComment = statement(ml_stmt_atomic(comment("no match, so fail")),
            MLDS_Context),
        ml_gen_failure(CodeModel, Context, FailStatements, !Info)
    ),

    % Generate the code etc. for the hash table.
    ml_gen_string_hash_slots(0, TableSize, HashSlotsMap,
        Strings, NextSlots, map.init, RevMap),
    map.to_assoc_list(RevMap, RevList),
    generate_string_switch_arms(CodeMap, RevList, [], SlotsCases0),
    list.sort(SlotsCases0, SlotsCases),

    % Generate the following local constant declarations:
    %   static const int next_slots_table_N = { <NextSlots> };
    %   static const MR_String string_table_M[] = { <Strings> };

    some [!GlobalData] (
        ml_gen_info_get_global_data(!.Info, !:GlobalData),

        NextSlotsType = mlds_array_type(SlotVarType),
        ml_gen_static_const_defn("next_slots_table", NextSlotsType,
            acc_private, init_array(NextSlots), Context, NextSlotsName,
            !GlobalData),
        ml_gen_var_lval(!.Info, NextSlotsName, NextSlotsType, NextSlotsLval),

        StringTableType = mlds_array_type(StringVarType),
        ml_gen_static_const_defn("string_table", StringTableType,
            acc_private, init_array(Strings), Context, StringTableName,
            !GlobalData),
        ml_gen_var_lval(!.Info, StringTableName, StringTableType,
            StringTableLval),

        ml_gen_info_set_global_data(!.GlobalData, !Info)
    ),

    % Generate code which does the hash table lookup.
    SwitchStmt0 = ml_stmt_switch(SlotVarType, ml_lval(SlotVarLval),
        mlds_switch_range(0, TableSize - 1), SlotsCases,
        default_is_unreachable),
    ml_simplify_switch(SwitchStmt0, MLDS_Context, SwitchStatement, !Info),

    FoundMatchCond =
        ml_binop(logical_and,
            ml_binop(ne,
                ml_lval(StringVarLval),
                ml_const(mlconst_null(StringVarType))),
            ml_binop(str_eq,
                ml_lval(StringVarLval),
                VarRval)
        ),
    FoundMatchCode = statement(
        ml_stmt_block([], [
            statement(ml_stmt_atomic(comment("we found a match")),
                MLDS_Context),
            statement(ml_stmt_atomic(
                    comment("dispatch to the corresponding code")),
                MLDS_Context),
            SwitchStatement,
            GotoEndStatement
        ]),
        MLDS_Context),
    LoopBody = ml_gen_block([], [
        statement(ml_stmt_atomic(comment(
            "lookup the string for this hash slot")), MLDS_Context),
        statement(
            ml_stmt_atomic(assign(StringVarLval,
                ml_binop(array_index(elem_type_string),
                    ml_lval(StringTableLval),
                    ml_lval(SlotVarLval)))),
            MLDS_Context),
        statement(ml_stmt_atomic(comment("did we find a match?")),
            MLDS_Context),
        statement(ml_stmt_if_then_else(FoundMatchCond, FoundMatchCode, no),
            MLDS_Context),
        statement(ml_stmt_atomic(comment(
            "no match yet, so get next slot in hash chain")), MLDS_Context),
        statement(
            ml_stmt_atomic(assign(SlotVarLval,
                ml_binop(array_index(elem_type_int),
                    ml_lval(NextSlotsLval),
                    ml_lval(SlotVarLval)))),
            MLDS_Context)
        ],
        Context),
    HashLookupStatements = [
        statement(ml_stmt_atomic(comment("hashed string switch")),
            MLDS_Context),
        statement(ml_stmt_atomic(comment(
            "compute the hash value of the input string")), MLDS_Context),
        statement(
            ml_stmt_atomic(assign(SlotVarLval,
                ml_binop(bitwise_and,
                    ml_unop(std_unop(hash_string), VarRval),
                    ml_const(mlconst_int(HashMask))))),
            MLDS_Context),
        statement(ml_stmt_atomic(comment("hash chain loop")), MLDS_Context),
        statement(
            ml_stmt_while(
                ml_binop(int_ge,
                    ml_lval(SlotVarLval),
                    ml_const(mlconst_int(0))),
                LoopBody,
                yes), % This is a do...while loop.
            MLDS_Context)
        ],
    EndLabelStatement = statement(ml_stmt_label(EndLabel), MLDS_Context),
    EndComment =
        statement(ml_stmt_atomic(comment("end of hashed string switch")),
            MLDS_Context),

    % Collect all the generated variable/constant declarations
    % and code fragments together.
    Decls = [SlotVarDefn, StringVarDefn],
    Statements =
        HashLookupStatements ++
        [FailComment | FailStatements] ++
        [EndLabelStatement, EndComment].

%-----------------------------------------------------------------------------%

:- pred gen_tagged_case_code_for_string_switch(code_model::in,
    tagged_case::in, int::out,
    map(int, statement)::in, map(int, statement)::out, unit::in, unit::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_tagged_case_code_for_string_switch(CodeModel, TaggedCase, CaseNum,
        !CodeMap, !Unit, !Info) :-
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
    svmap.det_insert(CaseNum, CaseStatement, !CodeMap).

:- func gen_string_switch_case_comment(tagged_cons_id) = string.

gen_string_switch_case_comment(TaggedConsId) = String :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    ( ConsTag = string_tag(ConsString) ->
        String = """" ++ ConsString ++ """"
    ;
        unexpected(this_file, "gen_string_switch_case_comment: non-string tag")
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

:- pred ml_gen_string_hash_slots(int::in, int::in,
    map(int, string_hash_slot(int))::in,
    list(mlds_initializer)::out, list(mlds_initializer)::out,
    hash_slot_rev_map::in, hash_slot_rev_map::out) is det.

ml_gen_string_hash_slots(Slot, TableSize, HashSlotMap,
        StringInits, NextSlotInits, !RevMap) :-
    ( Slot = TableSize ->
        StringInits = [],
        NextSlotInits = []
    ;
        ml_gen_string_hash_slot(Slot, HashSlotMap,
            StringInit, NextSlotInit, !RevMap),
        ml_gen_string_hash_slots(Slot + 1, TableSize, HashSlotMap,
            LaterStringInits, LaterNextSlotInits, !RevMap),
        StringInits = [StringInit | LaterStringInits],
        NextSlotInits = [NextSlotInit | LaterNextSlotInits]
    ).

:- pred ml_gen_string_hash_slot(int::in,
    map(int, string_hash_slot(int))::in,
    mlds_initializer::out, mlds_initializer::out,
    hash_slot_rev_map::in, hash_slot_rev_map::out) is det.

ml_gen_string_hash_slot(Slot, HashSlotMap, StringInit, NextSlotInit,
        !RevMap) :-
    ( map.search(HashSlotMap, Slot, string_hash_slot(Next, String, CaseNum)) ->
        StringRval = ml_const(mlconst_string(String)),
        NextSlotRval = ml_const(mlconst_int(Next)),
        ( map.search(!.RevMap, CaseNum, OldEntry) ->
            OldEntry = hash_slots(OldFirstSlot, OldLaterSlots),
            NewEntry = hash_slots(OldFirstSlot, [Slot | OldLaterSlots]),
            svmap.det_update(CaseNum, NewEntry, !RevMap)
        ;
            NewEntry = hash_slots(Slot, []),
            svmap.det_insert(CaseNum, NewEntry, !RevMap)
        )
    ;
        StringRval = ml_const(mlconst_null(ml_string_type)),
        NextSlotRval = ml_const(mlconst_int(-2))
    ),
    StringInit = init_obj(StringRval),
    NextSlotInit = init_obj(NextSlotRval).

:- pred generate_string_switch_arms(map(int, statement)::in,
    assoc_list(int, hash_slots)::in,
    list(mlds_switch_case)::in, list(mlds_switch_case)::out) is det.

generate_string_switch_arms(_, [], !Cases).
generate_string_switch_arms(CodeMap, [Entry | Entries], !Cases) :-
    Entry = CaseNum - HashSlots,
    HashSlots = hash_slots(FirstHashSlot, RevLaterHashSlots),
    list.reverse(RevLaterHashSlots, LaterHashSlots),
    FirstMatchCond = make_hash_match(FirstHashSlot),
    LaterMatchConds = list.map(make_hash_match, LaterHashSlots),
    map.lookup(CodeMap, CaseNum, CaseStatement),
    Case = mlds_switch_case(FirstMatchCond, LaterMatchConds, CaseStatement),
    !:Cases = [Case | !.Cases],
    generate_string_switch_arms(CodeMap, Entries, !Cases).

:- func make_hash_match(int) = mlds_case_match_cond.

make_hash_match(Slot) = match_value(ml_const(mlconst_int(Slot))).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "ml_string_switch.m".

%-----------------------------------------------------------------------------%
