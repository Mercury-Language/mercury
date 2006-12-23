%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
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

:- import_module backend_libs.switch_util.
:- import_module hlds.code_model.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.mlds.
:- import_module parse_tree.prog_data.

:- pred generate(cases_list::in, prog_var::in, code_model::in, can_fail::in,
    prog_context::in, mlds_defns::out, statements::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module hlds.hlds_data.
:- import_module libs.compiler_util.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_simplify_switch.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

generate(Cases, Var, CodeModel, _CanFail, Context, Decls, Statements, !Info) :-
    MLDS_Context = mlds_make_context(Context),
    % Compute the value we're going to switch on.

    ml_gen_var(!.Info, Var, VarLval),
    VarRval = lval(VarLval),

    % Generate the following local variable declarations:
    %   int slot;
    %   MR_String str;

    ml_gen_info_new_cond_var(SlotVarSeq, !Info),
    SlotVarName = mlds_var_name(
        string.format("slot_%d", [i(SlotVarSeq)]), no),
    SlotVarType = mlds_native_int_type,
    SlotVarGCStatement = gc_no_stmt, % never need to trace ints
    SlotVarDefn = ml_gen_mlds_var_decl(var(SlotVarName), SlotVarType,
        SlotVarGCStatement, MLDS_Context),
    ml_gen_var_lval(!.Info, SlotVarName, SlotVarType, SlotVarLval),

    ml_gen_info_new_cond_var(StringVarSeq, !Info),
    StringVarName = mlds_var_name(
        string.format("str_%d", [i(StringVarSeq)]), no),
    StringVarType = ml_string_type,
    % This variable always points to an element of the string_table array,
    % which are all static constants; it can never point into the heap.
    % So the GC never needs to trace it
    StringVarGCStatement = gc_no_stmt,
    StringVarDefn = ml_gen_mlds_var_decl(var(StringVarName),
        StringVarType, StringVarGCStatement, MLDS_Context),
    ml_gen_var_lval(!.Info, StringVarName, StringVarType, StringVarLval),

    % Generate new labels.
    ml_gen_new_label(EndLabel, !Info),
    GotoEndStatement = statement(goto(label(EndLabel)),
        MLDS_Context),

    % Determine how big to make the hash table. Currently we round the number
    % of cases up to the nearest power of two, and then double it. This should
    % hopefully ensure that we don't get too many hash collisions.
    %
    list.length(Cases, NumCases),
    int.log2(NumCases, LogNumCases),
    int.pow(2, LogNumCases, RoundedNumCases),
    TableSize = 2 * RoundedNumCases,
    HashMask = TableSize - 1,

    % Compute the hash table.
    switch_util.string_hash_cases(Cases, HashMask, HashValsMap),
    map.to_assoc_list(HashValsMap, HashValsList),
    switch_util.calc_hash_slots(HashValsList, HashValsMap, HashSlotsMap),

    % Generate the code for when the hash lookup fails.
    ml_gen_failure(CodeModel, Context, FailStatements, !Info),

    % Generate the code etc. for the hash table.
    gen_hash_slots(0, TableSize, HashSlotsMap, CodeModel,
        Context, Strings, NextSlots, SlotsCases, !Info),

    % Generate the following local constant declarations:
    %   static const int next_slots_table = { <NextSlots> };
    %   static const MR_String string_table[] = { <Strings> };

    ml_gen_info_new_const(NextSlotsSeq, !Info),
    ml_format_static_const_name(!.Info, "next_slots_table", NextSlotsSeq,
        NextSlotsName),
    NextSlotsType = mlds_array_type(SlotVarType),
    NextSlotsDefn = ml_gen_static_const_defn(NextSlotsName,
        NextSlotsType, local, init_array(NextSlots), Context),
    ml_gen_var_lval(!.Info, NextSlotsName, NextSlotsType, NextSlotsLval),

    ml_gen_info_new_const(StringTableSeq, !Info),
    ml_format_static_const_name(!.Info, "string_table", StringTableSeq,
        StringTableName),
    StringTableType = mlds_array_type(StringVarType),
    StringTableDefn = ml_gen_static_const_defn(StringTableName,
        StringTableType, local, init_array(Strings), Context),
    ml_gen_var_lval(!.Info, StringTableName, StringTableType,
        StringTableLval),

    % Generate code which does the hash table lookup.
    SwitchStmt0 = switch(SlotVarType, lval(SlotVarLval),
        range(0, TableSize - 1), SlotsCases, default_is_unreachable),
    ml_simplify_switch(SwitchStmt0, MLDS_Context, SwitchStatement, !Info),

    FoundMatchCond =
        binop(logical_and,
            binop(ne,
                lval(StringVarLval),
                const(mlconst_null(StringVarType))),
            binop(str_eq,
                lval(StringVarLval),
                VarRval)
        ),
    FoundMatchCode = statement(
        block([], [
            statement(atomic(comment("we found a match")), MLDS_Context),
            statement(atomic(comment(
                "dispatch to the corresponding code")), MLDS_Context),
            SwitchStatement,
            GotoEndStatement
        ]),
        MLDS_Context),
    LoopBody = ml_gen_block([], [
        statement(atomic(comment(
            "lookup the string for this hash slot")), MLDS_Context),
        statement(
            atomic(assign(StringVarLval,
                binop(array_index(elem_type_string),
                    lval(StringTableLval),
                    lval(SlotVarLval)))),
            MLDS_Context),
        statement(atomic(comment("did we find a match?")), MLDS_Context),
        statement(
            if_then_else(FoundMatchCond, FoundMatchCode, no), MLDS_Context),
        statement(atomic(comment(
            "no match yet, so get next slot in hash chain")), MLDS_Context),
        statement(
            atomic(assign(SlotVarLval,
                binop(array_index(elem_type_int),
                    lval(NextSlotsLval),
                    lval(SlotVarLval)))),
            MLDS_Context)
        ],
        Context),
    HashLookupStatements = [
        statement(atomic(comment("hashed string switch")),
            MLDS_Context),
        statement(atomic(comment(
            "compute the hash value of the input string")), MLDS_Context),
        statement(
            atomic(assign(SlotVarLval, binop(bitwise_and,
                unop(std_unop(hash_string), VarRval),
                const(mlconst_int(HashMask))))),
            MLDS_Context),
        statement(atomic(comment("hash chain loop")), MLDS_Context),
        statement(
            while(binop(int_ge, lval(SlotVarLval), const(mlconst_int(0))),
                LoopBody,
                yes), % this is a do...while loop
            MLDS_Context)
        ],
    FailComment =
        statement(atomic(comment("no match, so fail")),
            MLDS_Context),
    EndLabelStatement = statement(label(EndLabel), MLDS_Context),
    EndComment =
        statement(atomic(comment("end of hashed string switch")),
            MLDS_Context),

    % Collect all the generated variable/constant declarations
    % and code fragments together.
    Decls = [NextSlotsDefn, StringTableDefn, SlotVarDefn, StringVarDefn],
    Statements = HashLookupStatements ++ [FailComment | FailStatements] ++
        [EndLabelStatement, EndComment].

%-----------------------------------------------------------------------------%

:- pred gen_hash_slots(int::in, int::in,
    map(int, hash_slot)::in, code_model::in, prog_context::in,
    list(mlds_initializer)::out, list(mlds_initializer)::out,
    list(mlds_switch_case)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_hash_slots(Slot, TableSize, HashSlotMap, CodeModel, Context, Strings,
        NextSlots, MLDS_Cases, !Info) :-
    ( Slot = TableSize ->
        Strings = [],
        NextSlots = [],
        MLDS_Cases = []
    ;
        MLDS_Context = mlds_make_context(Context),
        gen_hash_slot(Slot, HashSlotMap, CodeModel, MLDS_Context, String,
            NextSlot, SlotCases, !Info),
        gen_hash_slots(Slot + 1, TableSize, HashSlotMap, CodeModel, Context,
            Strings0, NextSlots0, MLDS_Cases0, !Info),
        Strings = [String | Strings0],
        NextSlots = [NextSlot | NextSlots0],
        MLDS_Cases = SlotCases ++ MLDS_Cases0
    ).

:- pred gen_hash_slot(int::in, map(int, hash_slot)::in,
    code_model::in, mlds_context::in, mlds_initializer::out,
    mlds_initializer::out, list(mlds_switch_case)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_hash_slot(Slot, HashSlotMap, CodeModel, MLDS_Context,
        init_obj(StringRval), init_obj(NextSlotRval), MLDS_Cases, !Info) :-
    ( map.search(HashSlotMap, Slot, hash_slot(Case, Next)) ->
        NextSlotRval = const(mlconst_int(Next)),
        Case = extended_case(_, ConsTag, _, Goal),
        ( ConsTag = string_tag(String0) ->
            String = String0
        ;
            unexpected(this_file, "gen_hash_slots: string expected")
        ),
        StringRval = const(mlconst_string(String)),
        ml_gen_goal(CodeModel, Goal, GoalStatement, !Info),

        string.append_list(["case """, String, """"], CommentString),
        Comment = statement(atomic(comment(CommentString)),
            MLDS_Context),
        CaseStatement = statement(block([], [Comment, GoalStatement]),
            MLDS_Context),
        MLDS_Cases = [[match_value(const(mlconst_int(Slot)))] - CaseStatement]
    ;
        StringRval = const(mlconst_null(ml_string_type)),
        NextSlotRval = const(mlconst_int(-2)),
        MLDS_Cases = []
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "ml_string_switch.m".

%-----------------------------------------------------------------------------%
