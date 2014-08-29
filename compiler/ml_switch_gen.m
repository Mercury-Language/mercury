%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
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
% 2 For switches on strings, we can use five smart indexing strategies,
%   four of which are the possible combinations of two possible implementation
%   strategies of each of two aspects on the switch.
%
%   a)  One basic implementation strategy is the use of a hash table with
%       open addressing. Since the contents of the hash table is fixed,
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
%   b)  The second basic implementation approach is the use of binary search.
%       We generate a table containing all the strings in the switch cases in
%       order, and search it using binary search. Again, we don't use this
%       strategy if the target supports string switch statements and the
%       `--prefer-switch' option is set.
%
%   c)  If the target supports string switches, we generate an MLDS switch
%       statement directly on the string variable.
%
%   The second aspect is whether we use a lookup table. If all the switch arms
%   contain only construction unifications of constants, and we are using
%   approach (a) or (b), we extend can each row in either the hash table or the
%   binary search table with extra columns containing the values of the output
%   variables.
%
%   The use of a lookup table and the use of binary searches is NYI.
%   All the other indexing strategies are implemented by ml_string_switch.m.
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
%   However, this is not yet implemented.
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
%-----------------------------------------------------------------------------%

:- module ml_backend.ml_switch_gen.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Generate MLDS code for a switch.
    %
:- pred ml_gen_switch(prog_var::in, can_fail::in, list(case)::in,
    code_model::in, prog_context::in, hlds_goal_info::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % Generate an appropriate default for a switch.
    %
:- pred ml_switch_generate_default(can_fail::in, code_model::in,
    prog_context::in, mlds_switch_default::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.foreign.
:- import_module backend_libs.switch_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_lookup_switch.
:- import_module ml_backend.ml_simplify_switch.
:- import_module ml_backend.ml_string_switch.
:- import_module ml_backend.ml_tag_switch.
:- import_module ml_backend.ml_target_util.
:- import_module ml_backend.ml_unify_gen.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.

%-----------------------------------------------------------------------------%

ml_gen_switch(SwitchVar, CanFail, Cases, CodeModel, Context, GoalInfo,
        Decls, Statements, !Info) :-
    % Lookup the representation of the constructors for the tag tests.
    % Note that you cannot have a switch on a variable whose type's main
    % type constructor is not known.
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ml_variable_type(!.Info, SwitchVar, SwitchVarType),
    type_to_ctor_det(SwitchVarType, SwitchVarTypeCtor),
    tag_cases(ModuleInfo, SwitchVarType, Cases, TaggedCases,
        MaybeIntSwitchInfo),

    % Figure out what kind of switch this is.
    TypeCtorCategory = classify_type(ModuleInfo, SwitchVarType),
    SwitchCategory = type_ctor_cat_to_switch_cat(TypeCtorCategory),
    ml_gen_info_get_globals(!.Info, Globals),
    globals.lookup_bool_option(Globals, smart_indexing, Indexing),
    (
        (
            Indexing = no
        ;
            % Check for a switch on a type whose representation uses
            % reserved addresses.
            % The search will fail for builtin types.
            module_info_get_type_table(ModuleInfo, TypeTable),
            search_type_ctor_defn(TypeTable, SwitchVarTypeCtor,
                SwitchVarTypeDefn),
            hlds_data.get_type_defn_body(SwitchVarTypeDefn, SwitchVarTypeBody),
            SwitchVarTypeBody ^ du_type_reserved_addr = uses_reserved_address
        ;
            is_smart_indexing_disabled_category(Globals, SwitchCategory)
        )
    ->
        % XXX In some cases, we could generate better code if we first checked
        % for and handled the reserved addresses, and then used one of the
        % smart indexing schemes for the other cases.
        ml_switch_generate_if_then_else_chain(TaggedCases, SwitchVar,
            CodeModel, CanFail, Context, Statements, !Info),
        Decls = []
    ;
        (
            SwitchCategory = atomic_switch,
            num_cons_ids_in_tagged_cases(TaggedCases, NumConsIds, NumArms),
            (
                ml_gen_info_get_high_level_data(!.Info, no),
                MaybeIntSwitchInfo = int_switch(LowerLimit, UpperLimit,
                    NumValues),
                globals.lookup_bool_option(Globals, static_ground_cells, yes),
                globals.lookup_int_option(Globals, lookup_switch_size,
                    LookupSize),
                NumConsIds >= LookupSize,
                NumArms > 1,
                globals.lookup_int_option(Globals, lookup_switch_req_density,
                    ReqDensity),
                filter_out_failing_cases_if_needed(CodeModel,
                    TaggedCases, FilteredTaggedCases,
                    CanFail, FilteredCanFail),
                find_int_lookup_switch_params(ModuleInfo, SwitchVarType,
                    FilteredCanFail, LowerLimit, UpperLimit, NumValues,
                    ReqDensity, NeedBitVecCheck, NeedRangeCheck,
                    FirstVal, LastVal),
                NonLocals = goal_info_get_nonlocals(GoalInfo),
                ml_is_lookup_switch(get_int_tag, SwitchVar,
                    FilteredTaggedCases, NonLocals, CodeModel,
                    LookupSwitchInfo, !Info)
            ->
                ml_gen_lookup_switch(SwitchVar, LookupSwitchInfo,
                    CodeModel, Context, FirstVal, LastVal,
                    NeedBitVecCheck, NeedRangeCheck, LookupStatement, !Info),
                Statements = [LookupStatement]
            ;
                globals_target_supports_int_switch(Globals) = yes
            ->
                ml_switch_generate_mlds_switch(TaggedCases, SwitchVar,
                    CodeModel, CanFail, Context, Statements, !Info)
            ;
                ml_switch_generate_if_then_else_chain(TaggedCases,
                    SwitchVar, CodeModel, CanFail, Context, Statements, !Info)
            ),
            Decls = []
        ;
            SwitchCategory = string_switch,
            filter_out_failing_cases_if_needed(CodeModel,
                TaggedCases, FilteredTaggedCases, CanFail, FilteredCanFail),
            num_cons_ids_in_tagged_cases(FilteredTaggedCases,
                NumConsIds, NumArms),
            ( NumArms > 1 ->
                globals.lookup_int_option(Globals, string_hash_switch_size,
                    StringHashSwitchSize),
                globals.lookup_int_option(Globals, string_binary_switch_size,
                    StringBinarySwitchSize),
                globals.lookup_bool_option(Globals, prefer_switch,
                    PreferSwitch),
                (
                    globals_target_supports_string_switch(Globals) = yes,
                    % Even if we could use a hash or binary switch,
                    % we may prefer to do a direct-mapped string switch.
                    PreferSwitch = yes
                ->
                    ml_switch_generate_mlds_switch(FilteredTaggedCases,
                        SwitchVar, CodeModel, FilteredCanFail, Context,
                        Statements, !Info),
                    Decls = []
                ;
                    NumConsIds >= StringHashSwitchSize,
                    % We can implement string hash switches using either
                    % computed gotos or int switches.
                    (
                        globals_target_supports_computed_goto(Globals) = yes
                    ;
                        globals_target_supports_int_switch(Globals) = yes
                    )
                ->
                    (
                        ml_gen_info_get_high_level_data(!.Info, no),
                        globals.lookup_bool_option(Globals,
                            static_ground_cells, yes),
                        NonLocals = goal_info_get_nonlocals(GoalInfo),
                        ml_is_lookup_switch(get_string_tag, SwitchVar,
                            FilteredTaggedCases, NonLocals, CodeModel,
                            LookupSwitchInfo, !Info)
                    ->
                        ml_generate_string_hash_lookup_switch(SwitchVar,
                            LookupSwitchInfo, CodeModel, FilteredCanFail,
                            Context, Decls, Statements, !Info)
                    ;
                        ml_generate_string_hash_jump_switch(
                            FilteredTaggedCases, SwitchVar, CodeModel,
                            FilteredCanFail, Context, Decls, Statements, !Info)
                    )
                ;
                    NumConsIds >= StringBinarySwitchSize,
                    % We can implement string binary switches using either
                    % computed gotos or int switches.
                    (
                        globals_target_supports_computed_goto(Globals) = yes
                    ;
                        globals_target_supports_int_switch(Globals) = yes
                    )
                ->
                    (
                        ml_gen_info_get_high_level_data(!.Info, no),
                        globals.lookup_bool_option(Globals,
                            static_ground_cells, yes),
                        NonLocals = goal_info_get_nonlocals(GoalInfo),
                        ml_is_lookup_switch(get_string_tag, SwitchVar,
                            FilteredTaggedCases, NonLocals, CodeModel,
                            LookupSwitchInfo, !Info)
                    ->
                        ml_generate_string_binary_lookup_switch(SwitchVar,
                            LookupSwitchInfo, CodeModel, FilteredCanFail,
                            Context, Decls, Statements, !Info)
                    ;
                        ml_generate_string_binary_jump_switch(
                            FilteredTaggedCases, SwitchVar, CodeModel,
                            FilteredCanFail, Context, Decls, Statements, !Info)
                    )
                ;
                    ml_switch_generate_if_then_else_chain(FilteredTaggedCases,
                        SwitchVar, CodeModel, FilteredCanFail, Context,
                        Statements, !Info),
                    Decls = []
                )
            ;
                ml_switch_generate_if_then_else_chain(FilteredTaggedCases,
                    SwitchVar, CodeModel, FilteredCanFail, Context,
                    Statements, !Info),
                Decls = []
            )
        ;
            SwitchCategory = tag_switch,
            num_cons_ids_in_tagged_cases(TaggedCases, NumConsIds, NumArms),
            globals.lookup_int_option(Globals, tag_switch_size, TagSize),
            (
                NumConsIds >= TagSize,
                NumArms > 1,
                globals_target_supports_int_switch(Globals) = yes
            ->
                ml_generate_tag_switch(TaggedCases, SwitchVar, CodeModel,
                    CanFail, Context, Statements, !Info)
            ;
                ml_switch_generate_if_then_else_chain(TaggedCases,
                    SwitchVar, CodeModel, CanFail, Context, Statements, !Info)
            ),
            Decls = []
        ;
            SwitchCategory = float_switch,
            ml_switch_generate_if_then_else_chain(TaggedCases,
                SwitchVar, CodeModel, CanFail, Context, Statements, !Info),
            Decls = []
        )
    ).

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

    % Generate a chain of if-then-elses to test each case in turn.
    %
:- pred ml_switch_generate_if_then_else_chain(list(tagged_case)::in,
    prog_var::in, code_model::in, can_fail::in, prog_context::in,
    list(statement)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_switch_generate_if_then_else_chain(TaggedCases0, Var,
        CodeModel, CanFail, Context, Statements, !Info) :-
    % Associate each tagged case with the estimated cost of its tag tests.
    list.map(mark_tag_test_cost, TaggedCases0, CostTaggedCases0),

    % Sort the cases according to the priority of their tag tests.
    list.sort(CostTaggedCases0, CostTaggedCases),
    assoc_list.values(CostTaggedCases, TaggedCases),

    (
        TaggedCases = [],
        unexpected($module, $pred, "empty switch")
    ;
        TaggedCases = [FirstTaggedCase | LaterTaggedCases],
        ml_switch_generate_if_then_else_chain_ites(FirstTaggedCase,
            LaterTaggedCases, Var, CodeModel, CanFail, Context, Statements,
            !Info)
    ).

:- pred ml_switch_generate_if_then_else_chain_ites(tagged_case::in,
    list(tagged_case)::in, prog_var::in, code_model::in, can_fail::in,
    prog_context::in, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_switch_generate_if_then_else_chain_ites(TaggedCase, TaggedCases, Var,
        CodeModel, CanFail, Context, [Statement], !Info) :-
    TaggedCase = tagged_case(_, _, _, Goal),
    (
        TaggedCases = [],
        (
            CanFail = cannot_fail,
            % We do not need to test whether we are in the first tagged case;
            % previous tests have implied that we must be, by eliminating all
            % other cons_ids that Var could be bound to.
            ml_gen_goal_as_branch_block(CodeModel, Goal, Statement, !Info)
        ;
            CanFail = can_fail,
            % We handle this case as if we still had later cases, cases
            % representing the cons_ids that the switch does not cover.

            % Generate the test for checking whether we are in the first
            % tagged case.
            ml_switch_generate_if_then_else_cond(TaggedCase, Var, Cond, !Info),

            % Generate code for the first tagged case.
            ml_gen_goal_as_branch_block(CodeModel, Goal, GoalBlock, !Info),

            % Generate code for the non-covered tagged cases.
            ml_gen_failure(CodeModel, Context, FailStatements, !Info),
            FailBlock = ml_gen_block([], FailStatements, Context),

            % Put the codes for the first and non-covered tagged cases
            % together.
            Stmt = ml_stmt_if_then_else(Cond, GoalBlock, yes(FailBlock)),
            Statement = statement(Stmt, mlds_make_context(Context))
        )
    ;
        TaggedCases = [LaterTaggedCase | LaterTaggedCases],

        % Generate the test for checking whether we are in the first
        % tagged case.
        ml_switch_generate_if_then_else_cond(TaggedCase, Var, Cond, !Info),

        % Generate code for the first tagged case.
        ml_gen_goal_as_branch_block(CodeModel, Goal, GoalBlock, !Info),

        % Generate code for the later tagged cases.
        ml_switch_generate_if_then_else_chain_ites(LaterTaggedCase,
            LaterTaggedCases, Var, CodeModel, CanFail, Context,
            LaterStatements, !Info),
        LaterBlock = ml_gen_block([], LaterStatements, Context),

        % Put the codes for the first and later tagged cases together.
        Stmt = ml_stmt_if_then_else(Cond, GoalBlock, yes(LaterBlock)),
        Statement = statement(Stmt, mlds_make_context(Context))
    ).

:- pred ml_switch_generate_if_then_else_cond(tagged_case::in, prog_var::in,
    mlds_rval::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_switch_generate_if_then_else_cond(TaggedCase, Var, CondRval, !Info) :-
    TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds, _, _),
    ml_gen_known_tag_test(Var,
        TaggedMainConsId, MainTagTestRval, !Info),
    list.map_foldl(ml_gen_known_tag_test(Var),
        TaggedOtherConsIds, OtherTagTestRval, !Info),
    chain_ors(MainTagTestRval, OtherTagTestRval, CondRval).

    % chain_ors(FirstExpr, LaterExprs, Expr):
    %
    % Expr is true iff any one of FirstExpr and LaterExprs is true.
    %
:- pred chain_ors(mlds_rval::in, list(mlds_rval)::in, mlds_rval::out)
    is det.

chain_ors(FirstExpr, LaterExprs, Expr) :-
    (
        LaterExprs = [],
        Expr = FirstExpr
    ;
        LaterExprs = [SecondExpr | OtherExprs],
        FirstSecondExpr = ml_binop(logical_or, FirstExpr, SecondExpr),
        chain_ors(FirstSecondExpr, OtherExprs, Expr)
    ).

%-----------------------------------------------------------------------------%

    % Generate an MLDS switch. This is used for "direct-mapped" switches,
    % where we map a Mercury switch directly to a switch in the target
    % language. (But see the post-processing done by ml_simplify_switch.)
    %
:- pred ml_switch_generate_mlds_switch(list(tagged_case)::in,
    prog_var::in, code_model::in, can_fail::in, prog_context::in,
    list(statement)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_switch_generate_mlds_switch(Cases, Var, CodeModel, CanFail, Context,
        Statements, !Info) :-
    ml_variable_type(!.Info, Var, Type),
    ml_gen_type(!.Info, Type, MLDS_Type),
    ml_gen_var(!.Info, Var, Lval),
    Rval = ml_lval(Lval),
    ml_switch_gen_range(!.Info, MLDS_Type, Range),
    ml_switch_generate_mlds_cases(MLDS_Type, Cases, CodeModel, MLDS_Cases,
        !Info),
    ml_switch_generate_default(CanFail, CodeModel, Context, Default, !Info),
    SwitchStmt0 = ml_stmt_switch(MLDS_Type, Rval, Range, MLDS_Cases, Default),
    MLDS_Context = mlds_make_context(Context),
    ml_simplify_switch(SwitchStmt0, MLDS_Context, SwitchStatement, !Info),
    Statements = [SwitchStatement].

:- pred ml_switch_gen_range(ml_gen_info::in, mlds_type::in,
    mlds_switch_range::out) is det.

ml_switch_gen_range(Info, MLDS_Type, Range) :-
    (
        ml_gen_info_get_module_info(Info, ModuleInfo),
        ExportedType = to_exported_type(ModuleInfo, Type),
        MLDS_Type = mercury_type(Type, TypeCategory, ExportedType),
        switch_util.type_range(ModuleInfo, TypeCategory, Type,
            MinRange, MaxRange, _NumValuesInRange)
    ->
        Range = mlds_switch_range(MinRange, MaxRange)
    ;
        Range = mlds_switch_range_unknown
    ).

:- pred ml_switch_generate_mlds_cases(mlds_type::in, list(tagged_case)::in,
    code_model::in, list(mlds_switch_case)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_switch_generate_mlds_cases(_, [], _, [], !Info).
ml_switch_generate_mlds_cases(MLDS_Type, [TaggedCase | TaggedCases], CodeModel,
        [MLDS_Case | MLDS_Cases], !Info) :-
    ml_switch_generate_mlds_case(MLDS_Type, TaggedCase, CodeModel,
        MLDS_Case, !Info),
    ml_switch_generate_mlds_cases(MLDS_Type, TaggedCases, CodeModel,
        MLDS_Cases, !Info).

:- pred ml_switch_generate_mlds_case(mlds_type::in, tagged_case::in,
    code_model::in, mlds_switch_case::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_switch_generate_mlds_case(MLDS_Type, TaggedCase, CodeModel, MLDS_Case,
        !Info) :-
    TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds, _, Goal),
    ml_tagged_cons_id_to_match_cond(MLDS_Type, TaggedMainConsId, MainCond),
    list.map(ml_tagged_cons_id_to_match_cond(MLDS_Type), TaggedOtherConsIds,
        OtherConds),
    ml_gen_goal_as_branch_block(CodeModel, Goal, Statement, !Info),
    MLDS_Case = mlds_switch_case(MainCond, OtherConds, Statement).

:- pred ml_tagged_cons_id_to_match_cond(mlds_type::in, tagged_cons_id::in,
    mlds_case_match_cond::out) is det.

ml_tagged_cons_id_to_match_cond(MLDS_Type, TaggedConsId, MatchCond) :-
    TaggedConsId = tagged_cons_id(ConsId, Tag),
    (
        Tag = int_tag(Int),
        ( ConsId = int_const(_) ->
            Rval = ml_const(mlconst_int(Int))
        ; ConsId = char_const(_) ->
            Rval = ml_const(mlconst_char(Int))
        ;
            Rval = ml_const(mlconst_enum(Int, MLDS_Type))
        )
    ;
        Tag = string_tag(String),
        Rval = ml_const(mlconst_string(String))
    ;
        Tag = foreign_tag(ForeignLang, ForeignTag),
        Rval = ml_const(mlconst_foreign(ForeignLang, ForeignTag, MLDS_Type))
    ;
        ( Tag = float_tag(_)
        ; Tag = closure_tag(_, _, _)
        ; Tag = type_ctor_info_tag(_, _, _)
        ; Tag = base_typeclass_info_tag(_, _, _)
        ; Tag = type_info_const_tag(_)
        ; Tag = typeclass_info_const_tag(_)
        ; Tag = ground_term_const_tag(_, _)
        ; Tag = tabling_info_tag(_, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ; Tag = table_io_entry_tag(_, _)
        ; Tag = single_functor_tag
        ; Tag = unshared_tag(_)
        ; Tag = direct_arg_tag(_)
        ; Tag = shared_remote_tag(_, _)
        ; Tag = shared_local_tag(_, _)
        ; Tag = no_tag
        ; Tag = reserved_address_tag(_)
        ; Tag = shared_with_reserved_addresses_tag(_, _)
        ),
        unexpected($module, $pred, "invalid tag type")
    ),
    MatchCond = match_value(Rval).

    % Generate an appropriate default for a switch.
    %
ml_switch_generate_default(CanFail, CodeModel, Context, Default, !Info) :-
    (
        CanFail = can_fail,
        ml_gen_failure(CodeModel, Context, FailStatements, !Info),
        ( is_empty(FailStatements) ->
            Default = default_do_nothing
        ;
            Fail = ml_gen_block([], FailStatements, Context),
            Default = default_case(Fail)
        )
    ;
        CanFail = cannot_fail,
        Default = default_is_unreachable
    ).

%-----------------------------------------------------------------------------%
:- end_module ml_backend.ml_switch_gen.
%-----------------------------------------------------------------------------%
