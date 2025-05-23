%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2013-2020, 2022-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_switch_gen.m
% Authors: fjh, zs.
%
% This module determines how we should generate code for a switch, primarily
% by deciding what sort of indexing, if any, we should use.
% NOTE The code here is quite similar to the code in switch_gen.m,
% which does the same thing for the LLDS back-end. Any changes here
% probably also require similar changes there.
%
% The following describes the different forms of indexing that we can use.
% Note that currently some of these are not implemented; these are marked
% NYI (for "not yet implemented").
%
% 1 For switches on atomic data types (int, char, enums), we can use
%   three smart indexing strategies.
%
%   a)  If all the alternative goals for a switch on an atomic data type
%       contain only construction unifications of constants, then we
%       generate a dense lookup table (an array) in which we look up
%       the values of the output variables.
%       Implemented by ml_lookup_switch.m
%
%   b)  If the cases are not sparse, and the target supports computed
%       gotos, we should use a computed_goto, unless the target supports
%       switch statements and the `--prefer-switch' option is set.
%       NYI.
%
%   c)  If the target supports switch statements, we generate an MLDS
%       switch statement.
%       Implemented by this module.
%
% 2 For switches on strings, we can use seven smart indexing strategies,
%   six of which are the possible combinations of two possible implementation
%   strategies of each of two aspects on the switch.
%
%   a)  One basic implementation strategy is the use of a trie. We convert
%       the string being switched on into a list of code units (NOT code
%       points), and examine each code unit in sequence, narrowing the
%       range of possible matching switch arms at each step until only
%       one is left, at which point we compare the remaining characters.
%
%   b)  The second basic implementation strategy is the use of a hash table
%       with open addressing. Since the contents of the hash table is fixed,
%       the open addressing can select buckets that are not the home bucket
%       of any string in the table. And if we know that no two strings in
%       the table share the same home address, we can dispense with open
%       addressing altogether.
%
%       This strategy requires the target to support either computed gotos
%       or int switches. We prefer computed gotos, in which case table entries
%       contain the address to jump to, but we can also use it switches,
%       in which case entries contain an integer to give to a switch.
%       We don't use this strategy if the target supports string switch
%       statements and the `--prefer-switch' option is set.
%
%   c)  The third basic implementation approach is the use of binary search.
%       We generate a table containing all the strings in the switch cases in
%       order, and search it using binary search. Again, we don't use this
%       strategy if the target supports string switch statements and the
%       `--prefer-switch' option is set.
%
%   d)  If the target supports string switches, we generate an MLDS switch
%       statement directly on the string variable.
%
%   The second aspect is whether we use a lookup table. If all the switch arms
%   contain only construction unifications of constants, we can store the
%   values to be returned in a table for approach (a), or in extensions of
%   either the hash table (for approach (b)) or the binary search table
%   (for approach (c)).
%
%   All these indexing strategies are implemented by ml_string_switch.m.
%
% 3 For switches on discriminated union types, we generate code that does
%   indexing first on the primary tag, and then on the secondary tag (if
%   the primary tag is shared between several function symbols). The
%   indexing code for switches on both primary and secondary tags can be
%   in the form of a try-me-else chain, a try chain, a dense jump table
%   or a binary search.
%   At the moment, ml_tag_switch implements only try-me-else chains;
%   the other indexing forms are NYI.
%
% 4 For switches on floats, we could generate code that does binary search.
%   However, this is not yet implemented, as switches on floats can be
%   expected to be rare.
%
% If we cannot apply any of the above smart indexing strategies, or if the
% --smart-indexing option was disabled, then this module just generates
% a chain of if-then-elses.
%
% TODO:
% - implement the things marked NYI above
% - optimize if-then-else chain switches so that the recursive case
%   comes first (see switch_gen.m).
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_switch_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % Generate MLDS code for a switch.
    %
:- pred ml_gen_switch(prog_var::in, can_fail::in, code_model::in,
    hlds_goal_info::in, prog_context::in, list(case)::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % Generate an appropriate default for a switch.
    % (Exported to ml_tag_switch.m.)
    %
:- pred ml_switch_generate_default(can_fail::in, code_model::in,
    prog_context::in, mlds_switch_default::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.lookup_switch_util.
:- import_module backend_libs.switch_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_lookup_switch.
:- import_module ml_backend.ml_simplify_switch.
:- import_module ml_backend.ml_string_switch.
:- import_module ml_backend.ml_tag_switch.
:- import_module ml_backend.ml_target_util.
:- import_module ml_backend.ml_unify_gen_test.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

ml_gen_switch(SwitchVar, CanFail, CodeModel, GoalInfo, Context, Cases,
        Defns, Stmts, !Info) :-
    ml_gen_info_get_var_table(!.Info, VarTable),
    lookup_var_entry(VarTable, SwitchVar, SwitchVarEntry),
    SwitchVarType = SwitchVarEntry ^ vte_type,
    % Lookup the representation of the constructors for the tag tests.
    % Note that you cannot have a switch on a variable whose type's main
    % type constructor is not known.
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    tag_cases(ModuleInfo, SwitchVarType, Cases, TaggedCases,
        MaybeIntSwitchInfo),

    % Start every disjunct with EntryPackedWordMap to prevent later
    % switch arms from trying to use map entries added by earlier switch arms.
    ml_gen_info_get_packed_word_map(!.Info, EntryPackedWordMap),

    find_switch_category(ModuleInfo, SwitchVarType, SwitchCategory),
    (
        ( SwitchCategory = ite_chain_switch
        ; SwitchCategory = float_switch
        ),
        ml_switch_generate_if_then_else_chain(SwitchVar, TaggedCases,
            CodeModel, CanFail, EntryPackedWordMap, Context, Stmts, !Info),
        Defns = []
    ;
        SwitchCategory = int_max_32_switch,
        ml_gen_smart_int_max_32_switch(SwitchVar, SwitchVarType,
            TaggedCases, MaybeIntSwitchInfo, CanFail, CodeModel, GoalInfo,
            EntryPackedWordMap, Context, Stmts, !Info),
        Defns = []
    ;
        SwitchCategory = int_64_switch,
        module_info_get_globals(ModuleInfo, Globals),
        Int64SwitchSupported = globals_target_supports_int64_switch(Globals),
        (
            Int64SwitchSupported = yes,
            ml_switch_generate_mlds_switch(SwitchVar, TaggedCases,
                CodeModel, CanFail, EntryPackedWordMap, Context, Stmts, !Info)
        ;
            Int64SwitchSupported = no,
            ml_switch_generate_if_then_else_chain(SwitchVar, TaggedCases,
                CodeModel, CanFail, EntryPackedWordMap, Context, Stmts, !Info)
        ),
        Defns = []
    ;
        SwitchCategory = string_switch,
        ml_gen_string_switch(SwitchVar, SwitchVarEntry, TaggedCases,
            CanFail, CodeModel, GoalInfo, Context,
            EntryPackedWordMap, Defns, Stmts, !Info)
    ;
        SwitchCategory = tag_switch,
        num_cons_ids_in_tagged_cases(TaggedCases, NumConsIds, NumArms),
        module_info_get_globals(ModuleInfo, Globals),
        globals.get_opt_tuple(Globals, OptTuple),
        TagSize = OptTuple ^ ot_tag_switch_size,
        ( if
            NumConsIds >= TagSize,
            NumArms > 1,
            ml_generate_tag_switch_if_possible(SwitchVar, SwitchVarEntry,
                TaggedCases, CodeModel, CanFail,
                Context, EntryPackedWordMap, StmtsPrime, !Info)
        then
            Stmts = StmtsPrime
        else
            ml_switch_generate_if_then_else_chain(SwitchVar, TaggedCases,
                CodeModel, CanFail, EntryPackedWordMap, Context, Stmts, !Info)
        ),
        Defns = []
    ),
    % Start the code *after* the whole switch with EntryPackedWordMap as well,
    % to prevent that code from trying to use map entries added by a switch arm
    % that may not have been taken.
    ml_gen_info_set_packed_word_map(EntryPackedWordMap, !Info).

:- pred ml_gen_smart_int_max_32_switch(prog_var::in, mer_type::in,
    list(tagged_case)::in, maybe_int_switch_info::in,
    can_fail::in, code_model::in, hlds_goal_info::in,
    packed_word_map::in, prog_context::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_smart_int_max_32_switch(SwitchVar, SwitchVarType, TaggedCases,
        MaybeIntSwitchInfo, CanFail, CodeModel, GoalInfo,
        EntryPackedWordMap, Context, Stmts, !Info) :-
    num_cons_ids_in_tagged_cases(TaggedCases, NumConsIds, NumArms),
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    ml_gen_info_get_high_level_data(!.Info, HighLevelData),
    ( if
        HighLevelData = no,
        % XXX It should be possible to implement lookup switches
        % for Java and C# (HighLevelData = yes) as well.
        MaybeIntSwitchInfo = int_switch(IntSwitchInfo),
        IntSwitchInfo = int_switch_info(LowerLimit, UpperLimit, NumValues),
        globals.get_opt_tuple(Globals, OptTuple),
        OptTuple ^ ot_use_static_ground_cells = use_static_ground_cells,
        LookupSize = OptTuple ^ ot_lookup_switch_size,
        NumConsIds >= LookupSize,
        NumArms > 1,
        ReqDensity = OptTuple ^ ot_lookup_switch_req_density,
        filter_out_failing_cases_if_needed(CodeModel,
            TaggedCases, FilteredTaggedCases, CanFail, FilteredCanFail),
        find_int_lookup_switch_params(ModuleInfo, SwitchVarType,
            FilteredCanFail, LowerLimit, UpperLimit, NumValues,
            ReqDensity, NeedBitVecCheck, NeedRangeCheck, FirstVal, LastVal),
        ml_is_lookup_switch(SwitchVar, FilteredTaggedCases, GoalInfo,
            CodeModel, !.Info, MaybeLookupSwitchInfo),
        MaybeLookupSwitchInfo = yes(LookupSwitchInfo)
    then
        ml_gen_int_max_32_lookup_switch(SwitchVar, TaggedCases,
            LookupSwitchInfo, CodeModel, Context, FirstVal, LastVal,
            NeedBitVecCheck, NeedRangeCheck, LookupStmt, !:Info),
        Stmts = [LookupStmt]
    else
        % All MLDS targets (as of 2023 april) support switches on integers
        % whose size is at most 32 bits.
        ml_switch_generate_mlds_switch(SwitchVar, TaggedCases,
            CodeModel, CanFail, EntryPackedWordMap, Context, Stmts, !Info)
    ).

:- pred ml_gen_string_switch(prog_var::in, var_table_entry::in,
    list(tagged_case)::in, can_fail::in, code_model::in, hlds_goal_info::in,
    prog_context::in, packed_word_map::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_string_switch(SwitchVar, SwitchVarEntry, TaggedCases,
        CanFail, CodeModel, GoalInfo, Context,
        EntryPackedWordMap, Defns, Stmts, !Info) :-
    filter_out_failing_cases_if_needed(CodeModel,
        TaggedCases, FilteredTaggedCases, CanFail, FilteredCanFail),
    num_cons_ids_in_tagged_cases(FilteredTaggedCases, NumConsIds, NumArms),
    % If we don't have at least two arms, an if-then-else chain
    % (which will contain at most one if-then-else) will be faster than
    % any other method for implementing the "switch".
    ( if NumArms < 2 then
        ml_switch_generate_if_then_else_chain(SwitchVar, FilteredTaggedCases,
            CodeModel, FilteredCanFail, EntryPackedWordMap, Context,
            Stmts, !Info),
        Defns = []
    else
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        module_info_get_globals(ModuleInfo, Globals),
        globals.get_target(Globals, Target),
        (
            ( Target = target_java
            ; Target = target_csharp
            ),
            % We always take this path for these two target languages, because
            % - both support switches on strings, and
            % - both represent strings using UTF-16, which (currently)
            %   cannot be handled by any of the Mercury compiler's
            %   smart string switch implementations,
            % We therefore leave any implementation of smart indexing
            % to the Java/C# compilers.
            ml_switch_generate_mlds_switch(SwitchVar, FilteredTaggedCases,
                CodeModel, FilteredCanFail, EntryPackedWordMap, Context,
                Stmts, !Info),
            Defns = []
        ;
            Target = target_c,
            ml_gen_smart_string_switch(Globals, SwitchVar, SwitchVarEntry,
                FilteredTaggedCases, CodeModel, FilteredCanFail, GoalInfo,
                Context, NumConsIds, EntryPackedWordMap, Defns, Stmts, !Info)
        )
    ).

:- pred ml_gen_smart_string_switch(globals::in,
    prog_var::in, var_table_entry::in, list(tagged_case)::in,
    code_model::in, can_fail::in, hlds_goal_info::in,
    prog_context::in, int::in, packed_word_map::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_smart_string_switch(Globals, SwitchVar, SwitchVarEntry,
        FilteredTaggedCases, CodeModel, FilteredCanFail, GoalInfo,
        Context, NumConsIds, EntryPackedWordMap, Defns, Stmts, !Info) :-
    ml_gen_var(!.Info, SwitchVar, SwitchVarEntry, SwitchVarLval),
    SwitchVarRval = ml_lval(SwitchVarLval),
    % We can use a trie, a hash switch or binary switch, and we prefer
    % them in that order.
    %
    % We can implement all three methods (tries, hash tables and binary
    % searches) using either computed gotos or int switches.
    ml_is_lookup_switch(SwitchVar, FilteredTaggedCases, GoalInfo,
        CodeModel, !.Info, MaybeLookupSwitchInfo),
    globals.get_opt_tuple(Globals, OptTuple),
    globals.get_target(Globals, Target),
    ( if
        % For now, string trie switches have been implemented only for C,
        % and the code assumes that the host and the target use the same
        % string encoding. This effectively requires that both the host and
        % the target compile to C.
        internal_string_encoding = utf8,    % host is C
        Target = target_c,                  % target is C
        StringTrieSwitchSize = OptTuple ^ ot_string_trie_switch_size,
        NumConsIds >= StringTrieSwitchSize
    then
        (
            MaybeLookupSwitchInfo = yes(LookupSwitchInfo),
            ml_generate_string_trie_lookup_switch(SwitchVarRval,
                FilteredTaggedCases, LookupSwitchInfo, CodeModel,
                FilteredCanFail, Context, Stmts, !:Info)
        ;
            MaybeLookupSwitchInfo = no,
            ml_generate_string_trie_jump_switch(SwitchVarRval,
                FilteredTaggedCases, CodeModel, FilteredCanFail,
                EntryPackedWordMap, Context, Stmts, !Info)
        ),
        Defns = []
    else if
        StringHashSwitchSize = OptTuple ^ ot_string_hash_switch_size,
        NumConsIds >= StringHashSwitchSize
    then
        (
            MaybeLookupSwitchInfo = yes(LookupSwitchInfo),
            ml_generate_string_hash_lookup_switch(SwitchVarRval,
                FilteredTaggedCases, LookupSwitchInfo, CodeModel,
                FilteredCanFail, Context, Defns, Stmts, !:Info)
        ;
            MaybeLookupSwitchInfo = no,
            ml_generate_string_hash_jump_switch(SwitchVarRval,
                FilteredTaggedCases, CodeModel, FilteredCanFail,
                EntryPackedWordMap, Context, Defns, Stmts, !Info)
        )
    else if
        StringBinarySwitchSize = OptTuple ^ ot_string_binary_switch_size,
        NumConsIds >= StringBinarySwitchSize
    then
        (
            MaybeLookupSwitchInfo = yes(LookupSwitchInfo),
            ml_generate_string_binary_lookup_switch(SwitchVarRval,
                FilteredTaggedCases, LookupSwitchInfo, CodeModel,
                FilteredCanFail, Context, Defns, Stmts, !:Info)
        ;
            MaybeLookupSwitchInfo = no,
            ml_generate_string_binary_jump_switch(SwitchVarRval,
                FilteredTaggedCases, CodeModel, FilteredCanFail,
                EntryPackedWordMap, Context, Defns, Stmts, !Info)
        )
    else
        ml_switch_generate_if_then_else_chain(SwitchVar, FilteredTaggedCases,
            CodeModel, FilteredCanFail, EntryPackedWordMap, Context, Stmts,
            !Info),
        Defns = []
    ).

%---------------------------------------------------------------------------%

    % Look up the representation (tag) for the cons_id in each case.
    % Also look up the priority of each tag test.
    %
:- pred mark_tag_test_cost(tagged_case::in, pair(int, tagged_case)::out)
    is det.

mark_tag_test_cost(TaggedCase, Cost - TaggedCase) :-
    TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds,
        _CaseNum, _Goal),
    estimate_cons_id_tag_test_cost(TaggedMainConsId, 0, MainCost),
    list.foldl(estimate_cons_id_tag_test_cost, TaggedOtherConsIds,
        MainCost, Cost).

:- pred estimate_cons_id_tag_test_cost(tagged_cons_id::in,
    int::in, int::out) is det.

estimate_cons_id_tag_test_cost(TaggedConsId, !CaseCost) :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    ConsIdCost = estimate_switch_tag_test_cost(ConsTag),
    !:CaseCost = !.CaseCost + ConsIdCost.

%---------------------------------------------------------------------------%

    % Generate a chain of if-then-elses to test each case in turn.
    %
:- pred ml_switch_generate_if_then_else_chain(prog_var::in,
    list(tagged_case)::in, code_model::in, can_fail::in, packed_word_map::in,
    prog_context::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_switch_generate_if_then_else_chain(Var, TaggedCases0,
        CodeModel, CanFail, EntryPackedWordMap, Context, Stmts, !Info) :-
    % Associate each tagged case with the estimated cost of its tag tests.
    list.map(mark_tag_test_cost, TaggedCases0, CostTaggedCases0),

    % Sort the cases according to the priority of their tag tests.
    list.sort(CostTaggedCases0, CostTaggedCases),
    assoc_list.values(CostTaggedCases, TaggedCases),
    (
        TaggedCases = [],
        unexpected($pred, "empty switch")
    ;
        TaggedCases = [FirstTaggedCase | LaterTaggedCases],
        ml_switch_generate_if_then_else_chain_ites(Var,
            FirstTaggedCase, LaterTaggedCases, CodeModel, CanFail,
            EntryPackedWordMap, Context, Stmts,
            [], ReachableConstVarMaps, !Info),
        ml_gen_record_consensus_const_var_map(ReachableConstVarMaps, !Info)
    ).

:- pred ml_switch_generate_if_then_else_chain_ites(prog_var::in,
    tagged_case::in, list(tagged_case)::in, code_model::in, can_fail::in,
    packed_word_map::in, prog_context::in, list(mlds_stmt)::out,
    list(ml_ground_term_map)::in, list(ml_ground_term_map)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_switch_generate_if_then_else_chain_ites(Var, TaggedCase, TaggedCases,
        CodeModel, CanFail, EntryPackedWordMap, Context, [Stmt],
        !ReachableConstVarMaps, !Info) :-
    ml_gen_info_set_packed_word_map(EntryPackedWordMap, !Info),
    TaggedCase = tagged_case(_, _, _, Goal),
    (
        TaggedCases = [],
        (
            CanFail = cannot_fail,
            % If execution has got to the code we are generating here, then
            % previously-executed tests must have ruled out all the cons_ids
            % that Var could be bound to, other than the one in TaggedCase.
            ml_gen_goal_as_branch_block(CodeModel, Goal, Stmt,
                !ReachableConstVarMaps, !Info)
        ;
            CanFail = can_fail,
            % We handle this case as if we still had later cases, cases
            % representing the cons_ids that the switch does not cover.

            % Generate the test for checking whether we are in the first
            % tagged case.
            ml_switch_generate_if_then_else_cond(!.Info, Var, TaggedCase,
                Cond),

            % Generate code for the first tagged case.
            ml_gen_goal_as_branch_block(CodeModel, Goal, GoalBlock,
                !ReachableConstVarMaps, !Info),

            % Generate code for the non-covered tagged cases.
            ml_gen_failure(CodeModel, Context, FailStmts, !Info),
            FailBlock = ml_gen_block([], [], FailStmts, Context),

            % Put the codes for the first and non-covered tagged cases
            % together.
            Stmt = ml_stmt_if_then_else(Cond, GoalBlock, yes(FailBlock),
                Context)
        )
    ;
        TaggedCases = [LaterTaggedCase | LaterTaggedCases],

        % Generate the test for checking whether we are in the first
        % tagged case.
        ml_switch_generate_if_then_else_cond(!.Info, Var, TaggedCase, Cond),

        % Generate code for the first tagged case.
        ml_gen_goal_as_branch_block(CodeModel, Goal, GoalBlock,
            !ReachableConstVarMaps, !Info),

        % Generate code for the later tagged cases.
        ml_switch_generate_if_then_else_chain_ites(Var,
            LaterTaggedCase, LaterTaggedCases, CodeModel, CanFail,
            EntryPackedWordMap, Context, LaterStmts,
            !ReachableConstVarMaps, !Info),
        LaterBlock = ml_gen_block([], [], LaterStmts, Context),

        % Put the codes for the first and later tagged cases together.
        Stmt = ml_stmt_if_then_else(Cond, GoalBlock, yes(LaterBlock), Context)
    ).

:- pred ml_switch_generate_if_then_else_cond(ml_gen_info::in, prog_var::in,
    tagged_case::in, mlds_rval::out) is det.

ml_switch_generate_if_then_else_cond(Info, Var, TaggedCase, CondRval) :-
    TaggedCase = tagged_case(MainTaggedConsId, OtherTaggedConsIds, _, _),
    ml_generate_test_var_has_one_tagged_cons_id(Info, Var,
        MainTaggedConsId, OtherTaggedConsIds, CondRval).

%---------------------------------------------------------------------------%

    % Generate an MLDS switch. This is used for "direct-mapped" switches,
    % where we map a Mercury switch directly to a switch in the target
    % language. (But see the post-processing done by ml_simplify_switch.)
    %
:- pred ml_switch_generate_mlds_switch(prog_var::in, list(tagged_case)::in,
    code_model::in, can_fail::in, packed_word_map::in, prog_context::in,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_switch_generate_mlds_switch(Var, Cases, CodeModel, CanFail,
        EntryPackedWordMap, Context, Stmts, !Info) :-
    ml_gen_info_get_var_table(!.Info, VarTable),
    lookup_var_entry(VarTable, Var, VarEntry),
    Type = VarEntry ^ vte_type,
    ml_gen_mlds_type(!.Info, Type, MLDS_Type),
    ml_gen_var(!.Info, Var, VarEntry, Lval),
    Rval = ml_lval(Lval),
    ml_switch_gen_range(!.Info, MLDS_Type, Range),
    ml_switch_generate_mlds_cases(Type, MLDS_Type, CodeModel,
        EntryPackedWordMap, Cases, MLDS_Cases,
        [], ReachableConstVarMaps, !Info),
    ml_gen_record_consensus_const_var_map(ReachableConstVarMaps, !Info),
    ml_switch_generate_default(CanFail, CodeModel, Context, Default, !Info),
    SwitchStmt0 = ml_stmt_switch(MLDS_Type, Rval, Range, MLDS_Cases, Default,
        Context),
    ml_simplify_switch(SwitchStmt0, SwitchStmt, !Info),
    Stmts = [SwitchStmt].

:- pred ml_switch_gen_range(ml_gen_info::in, mlds_type::in,
    mlds_switch_range::out) is det.

ml_switch_gen_range(Info, MLDS_Type, Range) :-
    ml_gen_info_get_module_info(Info, ModuleInfo),
    ( if
        MLDS_Type = mercury_nb_type(Type, TypeCategory),
        type_range(ModuleInfo, coerce(TypeCategory), Type,
            MinRange, MaxRange, _NumValuesInRange)
    then
        Range = mlds_switch_range(MinRange, MaxRange)
    else
        Range = mlds_switch_range_unknown
    ).

:- pred ml_switch_generate_mlds_cases(mer_type::in, mlds_type::in,
    code_model::in, packed_word_map::in,
    list(tagged_case)::in, list(mlds_switch_case)::out,
    list(ml_ground_term_map)::in, list(ml_ground_term_map)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_switch_generate_mlds_cases(_, _, _, _, [], [],
        !ReachableConstVarMaps, !Info).
ml_switch_generate_mlds_cases(MerType, MLDS_Type, CodeModel,
        EntryPackedWordMap, [TaggedCase | TaggedCases],
        [MLDS_Case | MLDS_Cases], !ReachableConstVarMaps, !Info) :-
    ml_switch_generate_mlds_case(MerType, MLDS_Type, CodeModel,
        EntryPackedWordMap, TaggedCase, MLDS_Case,
        !ReachableConstVarMaps, !Info),
    ml_switch_generate_mlds_cases(MerType, MLDS_Type, CodeModel,
        EntryPackedWordMap, TaggedCases, MLDS_Cases,
        !ReachableConstVarMaps, !Info).

:- pred ml_switch_generate_mlds_case(mer_type::in, mlds_type::in,
    code_model::in, packed_word_map::in,
    tagged_case::in, mlds_switch_case::out,
    list(ml_ground_term_map)::in, list(ml_ground_term_map)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_switch_generate_mlds_case(MerType, MLDS_Type, CodeModel, EntryPackedWordMap,
        TaggedCase, MLDS_Case, !ReachableConstVarMaps, !Info) :-
    ml_gen_info_set_packed_word_map(EntryPackedWordMap, !Info),
    TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds, _, Goal),
    ml_tagged_cons_id_to_match_cond(MerType, MLDS_Type, TaggedMainConsId,
        MainCond),
    list.map(ml_tagged_cons_id_to_match_cond(MerType, MLDS_Type),
        TaggedOtherConsIds, OtherConds),
    ml_gen_goal_as_branch_block(CodeModel, Goal, Stmt,
        !ReachableConstVarMaps, !Info),
    MLDS_Case = mlds_switch_case(MainCond, OtherConds, Stmt).

:- pred ml_tagged_cons_id_to_match_cond(mer_type::in, mlds_type::in,
    tagged_cons_id::in, mlds_case_match_cond::out) is det.

ml_tagged_cons_id_to_match_cond(MerType, MLDS_Type, TaggedConsId, MatchCond) :-
    TaggedConsId = tagged_cons_id(_ConsId, Tag),
    (
        Tag = int_tag(IntTag),
        Rval = ml_int_tag_to_rval_const(IntTag, MerType, MLDS_Type)
    ;
        Tag = string_tag(String),
        Rval = ml_const(mlconst_string(String))
    ;
        Tag = foreign_tag(ForeignLang, ForeignTag),
        Rval = ml_const(mlconst_foreign(ForeignLang, ForeignTag, MLDS_Type))
    ;
        ( Tag = dummy_tag
        ; Tag = float_tag(_)
        ; Tag = type_ctor_info_tag(_, _, _)
        ; Tag = base_typeclass_info_tag(_, _, _)
        ; Tag = type_info_const_tag(_)
        ; Tag = typeclass_info_const_tag(_)
        ; Tag = ground_term_const_tag(_, _)
        ; Tag = tabling_info_tag(_, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ; Tag = table_io_entry_tag(_, _)
        ; Tag = no_tag
        ; Tag = direct_arg_tag(_)
        ; Tag = remote_args_tag(_)
        ; Tag = shared_local_tag_no_args(_, _, _)
        ; Tag = local_args_tag(_)
        ; Tag = closure_tag(_, _)
        ),
        unexpected($pred, "invalid tag type")
    ),
    MatchCond = match_value(Rval).

ml_switch_generate_default(CanFail, CodeModel, Context, Default, !Info) :-
    (
        CanFail = can_fail,
        ml_gen_failure(CodeModel, Context, FailStmts, !Info),
        ( if is_empty(FailStmts) then
            Default = default_do_nothing
        else
            Fail = ml_gen_block([], [], FailStmts, Context),
            Default = default_case(Fail)
        )
    ;
        CanFail = cannot_fail,
        Default = default_is_unreachable
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_switch_gen.
%---------------------------------------------------------------------------%
