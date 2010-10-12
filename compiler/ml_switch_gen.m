%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ml_switch_gen.m
% Author: fjh (adapted from switch_gen.m)
%
% This module handles the generation of code for switches for the MLDS
% back-end. Switches are disjunctions that do not require backtracking.
% They are detected in switch_detection.m.  This is the module that determines
% what sort of indexing to use for each switch and then actually generates the
% code.  The code here is quite similar to the code in switch_gen.m, which
% does the same thing for the LLDS back-end.
%
% The following describes the different forms of indexing that we could use.
% Note that currently most of these are not implemented!
% The ones that are not are marked NYI (for "not yet implemented").
%
% 1 For switches on atomic data types (int, char, enums), there are
%   several possibilities.
%
%   a)  If all the alternative goals for a switch on an atomic data type
%       contain only construction unifications of constants, then we should
%       generate a dense lookup table (an array) in which we can look up
%       the values of the output variables.
%   b)  If the cases are not sparse, and the target supports computed
%       gotos, we should use a computed_goto, unless the target supports
%       switch statements and the `--prefer-switch' option is set. (NYI)
%   c)  If the target supports switch statements,
%       we generate an MLDS switch statement.
%
% 2 For switches on strings, there are several possibilities.
%
%   a)  If the target supports indirect gotos, we should look up the address
%       to jump to in a hash table (e.g. using open addressing to resolve
%       hash collisions), and then jump to it using an indirect goto,
%       unless the target supports string switch statements and the
%       `--prefer-switch' option is set. (NYI)
%   b)  If the target supports string switches,
%       we generate an MLDS switch statement.
%
% 3 For switches on discriminated union types, we generate code that does
%   indexing first on the primary tag, and then on the secondary tag (if
%   the primary tag is shared between several function symbols). The
%   indexing code for switches on both primary and secondary tags can be
%   in the form of a try-me-else chain, a try chain, a dense jump table
%   or a binary search. (NYI)
%
% For all other cases (or if the --smart-indexing option was disabled),
% we just generate a chain of if-then-elses.
%
% TODO:
%   - implement the things marked NYI above
%   - optimize switches so that the recursive case comes first
%     (see switch_gen.m).
%
%-----------------------------------------------------------------------------%

:- module ml_backend.ml_switch_gen.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module libs.globals.
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

    % Succeed iff the target supports the specified construct.
    %
:- pred target_supports_int_switch(globals::in) is semidet.
:- pred target_supports_string_switch(globals::in) is semidet.
:- pred target_supports_goto(globals::in) is semidet.
:- pred target_supports_computed_goto(globals::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.foreign.
:- import_module backend_libs.switch_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module libs.compiler_util.
:- import_module libs.options.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_lookup_switch.
:- import_module ml_backend.ml_simplify_switch.
:- import_module ml_backend.ml_string_switch.
:- import_module ml_backend.ml_tag_switch.
:- import_module ml_backend.ml_unify_gen.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.

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
            SwitchCategory = string_switch,
            num_cons_ids_in_tagged_cases(TaggedCases, NumConsIds, NumArms),
            globals.lookup_int_option(Globals, string_switch_size, StringSize),
            (
                NumConsIds >= StringSize,
                NumArms > 1,
                % We can implement string hash switches using either
                % computed gotos or int switches.
                (
                    target_supports_computed_goto(Globals)
                ;
                    target_supports_int_switch(Globals)
                ),
                % XXX Currently string hash switches always use gotos
                % (to break out of the hash chain loop).
                % We should change that, so that we can use string hash
                % switches for the Java back-end too.
                target_supports_goto(Globals),
                % OK, we could use a string hash switch. But should we?
                % We may prefer to do a direct-mapped string switch.
                \+ (
                    target_supports_string_switch(Globals),
                    globals.lookup_bool_option(Globals, prefer_switch, yes)
                )
            ->
                ml_generate_string_switch(TaggedCases, SwitchVar,
                    CodeModel, CanFail, Context, Decls, Statements, !Info)
            ;
                target_supports_string_switch(Globals)
            ->
                ml_switch_generate_mlds_switch(TaggedCases, SwitchVar,
                    CodeModel, CanFail, Context, Statements, !Info),
                Decls = []
            ;
                ml_switch_generate_if_then_else_chain(TaggedCases,
                    SwitchVar, CodeModel, CanFail, Context, Statements, !Info),
                Decls = []
            )
        ;
            SwitchCategory = tag_switch,
            num_cons_ids_in_tagged_cases(TaggedCases, NumConsIds, NumArms),
            globals.lookup_int_option(Globals, tag_switch_size, TagSize),
            (
                NumConsIds >= TagSize,
                NumArms > 1,
                target_supports_int_switch(Globals)
            ->
                ml_generate_tag_switch(TaggedCases, SwitchVar, CodeModel,
                    CanFail, Context, Statements, !Info)
            ;
                ml_switch_generate_if_then_else_chain(TaggedCases,
                    SwitchVar, CodeModel, CanFail, Context, Statements, !Info)
            ),
            Decls = []
        ;
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
                find_lookup_switch_params(ModuleInfo, SwitchVarType, CodeModel,
                    CanFail, TaggedCases, FilteredTaggedCases,
                    LowerLimit, UpperLimit, NumValues, ReqDensity,
                    NeedBitVecCheck, NeedRangeCheck, FirstVal, LastVal),
                NonLocals = goal_info_get_nonlocals(GoalInfo),
                ml_gen_lookup_switch(SwitchVar, FilteredTaggedCases,
                    NonLocals, CodeModel, Context, FirstVal, LastVal,
                    NeedBitVecCheck, NeedRangeCheck, LookupStatement, !Info)
            ->
                Statements = [LookupStatement]
            ;
                target_supports_computed_goto(Globals)
            ->
                ml_switch_generate_mlds_switch(TaggedCases, SwitchVar,
                    CodeModel, CanFail, Context, Statements, !Info)
            ;
                ml_switch_generate_if_then_else_chain(TaggedCases,
                    SwitchVar, CodeModel, CanFail, Context, Statements, !Info)
            ),
            Decls = []
        ;
            SwitchCategory = other_switch,
            (
                % Try using a "direct-mapped" switch. This also handles dense
                % (computed goto) switches -- for those, we first generate a
                % direct-mapped switch, and then convert it into a computed
                % goto switch in ml_simplify_switch.
                target_supports_switch(SwitchCategory, Globals)
            ->
                ml_switch_generate_mlds_switch(TaggedCases, SwitchVar,
                    CodeModel, CanFail, Context, Statements, !Info)
            ;
                ml_switch_generate_if_then_else_chain(TaggedCases,
                    SwitchVar, CodeModel, CanFail, Context, Statements, !Info)
            ),
            Decls = []
        )
    ).

%-----------------------------------------------------------------------------%

:- pred target_supports_switch(switch_category::in, globals::in) is semidet.

target_supports_switch(SwitchCategory, Globals) :-
    (
        SwitchCategory = atomic_switch,
        target_supports_int_switch(Globals)
    ;
        SwitchCategory = string_switch,
        target_supports_string_switch(Globals)
    ).

target_supports_int_switch(Globals) :-
    globals.get_target(Globals, Target),
    target_supports_int_switch_2(Target) = yes.

target_supports_string_switch(Globals) :-
    globals.get_target(Globals, Target),
    target_supports_string_switch_2(Target) = yes.

target_supports_goto(Globals) :-
    globals.get_target(Globals, Target),
    target_supports_goto_2(Target) = yes.

target_supports_computed_goto(Globals) :-
    globals.get_target(Globals, Target),
    target_supports_computed_goto_2(Target) = yes.

:- func target_supports_int_switch_2(compilation_target) = bool.
:- func target_supports_string_switch_2(compilation_target) = bool.
:- func target_supports_goto_2(compilation_target) = bool.
:- func target_supports_computed_goto_2(compilation_target) = bool.

target_supports_int_switch_2(target_c) = yes.
target_supports_int_switch_2(target_asm) = yes.
target_supports_int_switch_2(target_il) = no.
target_supports_int_switch_2(target_csharp) = yes.
target_supports_int_switch_2(target_java) = yes.
target_supports_int_switch_2(target_x86_64) =
    unexpected(this_file, "target x86_64 with --high-level code").
target_supports_int_switch_2(target_erlang) =
    unexpected(this_file, "target erlang").

target_supports_string_switch_2(target_c) = no.
target_supports_string_switch_2(target_asm) = no.
target_supports_string_switch_2(target_il) = no.
target_supports_string_switch_2(target_csharp) = yes.
target_supports_string_switch_2(target_java) = no.
target_supports_string_switch_2(target_x86_64) =
    unexpected(this_file, "target x86_64 with --high-level code").
target_supports_string_switch_2(target_erlang) =
    unexpected(this_file, "target erlang").

target_supports_computed_goto_2(target_c) = yes.
target_supports_computed_goto_2(target_asm) = no.
    % XXX for asm, it should be `yes', but currently
    % computed gotos are not yet implemented in gcc.m.
target_supports_computed_goto_2(target_il) = yes.
target_supports_computed_goto_2(target_csharp) = yes.
target_supports_computed_goto_2(target_java) = no.
% target_supports_computed_goto_2(c_sharp) = no.
target_supports_computed_goto_2(target_x86_64) =
    unexpected(this_file, "target x86_64 with --high-level code").
target_supports_computed_goto_2(target_erlang) =
    unexpected(this_file, "target erlang").

target_supports_goto_2(target_c) = yes.
target_supports_goto_2(target_asm) = yes.
target_supports_goto_2(target_il) = yes.
target_supports_goto_2(target_csharp) = yes.
target_supports_goto_2(target_java) = no.
target_supports_goto_2(target_x86_64) =
    unexpected(this_file, "target x86_64 with --high-level code").
target_supports_goto_2(target_erlang) =
    unexpected(this_file, "target erlang").

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
        unexpected(this_file,
            "ml_switch_generate_if_then_else_chain: empty switch")
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
    % language.
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
        ; Tag = tabling_info_tag(_, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ; Tag = table_io_decl_tag(_, _)
        ; Tag = single_functor_tag
        ; Tag = unshared_tag(_)
        ; Tag = shared_remote_tag(_, _)
        ; Tag = shared_local_tag(_, _)
        ; Tag = no_tag
        ; Tag = reserved_address_tag(_)
        ; Tag = shared_with_reserved_addresses_tag(_, _)
        ),
        unexpected(this_file,
            "ml_tagged_cons_id_to_match_cond: invalid tag type")
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

:- func this_file = string.

this_file = "ml_switch_gen.m".

%-----------------------------------------------------------------------------%
