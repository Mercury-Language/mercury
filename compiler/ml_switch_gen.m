%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
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
%       contain only construction unifications of constants, then we
%       should generate a dense lookup table (an array) for each output
%       variable of the switch, rather than computed goto, so that
%       executing the switch becomes a matter of doing an array index for
%       each output variable. (NYI)
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
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.mlds.
:- import_module parse_tree.prog_data.

:- import_module list.

    % Generate MLDS code for a switch.
    %
:- pred ml_gen_switch(prog_var::in, can_fail::in, list(case)::in,
    code_model::in, prog_context::in,
    mlds_defns::out, statements::out,
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

:- implementation.

:- import_module backend_libs.foreign.
:- import_module backend_libs.switch_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module libs.compiler_util.
:- import_module libs.options.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_simplify_switch.
:- import_module ml_backend.ml_string_switch.
:- import_module ml_backend.ml_tag_switch.
:- import_module ml_backend.ml_unify_gen.

:- import_module bool.
:- import_module int.
:- import_module libs.tree.
:- import_module map.
:- import_module std_util.
:- import_module string.

%-----------------------------------------------------------------------------%

ml_gen_switch(CaseVar, CanFail, Cases, CodeModel, Context, Decls, Statements,
        !Info) :-
    % Lookup the representation of the constructors for the tag tests
    % and their corresponding priorities.
    ml_switch_lookup_tags(!.Info, Cases, CaseVar, TaggedCases0),

    % Sort the cases according to the priority of their tag tests.
    list.sort_and_remove_dups(TaggedCases0, TaggedCases),

    % Figure out what kind of switch this is.
    SwitchCategory = determine_category(!.Info, CaseVar),
    ml_gen_info_get_globals(!.Info, Globals),
    globals.lookup_bool_option(Globals, smart_indexing, Indexing),
    (
        % Check for a switch on a type whose representation
        % uses reserved addresses.
        list.member(Case, TaggedCases),
        Case = case(_Priority, Tag, _ConsId, _Goal),
        (
            Tag = reserved_address(_)
        ;
            Tag = shared_with_reserved_addresses(_, _)
        )
    ->
        % XXX This may be inefficient in some cases.
        ml_switch_generate_if_else_chain(TaggedCases, CaseVar, CodeModel,
            CanFail, Context, Decls, Statements, !Info)
    ;
% XXX Lookup switches are NYI
% When we do get around to implementing them,
% they should probably be handled in ml_simplify_switch rather than here.
%       Indexing = yes,
%       SwitchCategory = atomic_switch,
%       % Note that if/when the MLDS back-end supports execution
%       % tracing, we would also need to check that tracing is not
%       % enabled.
%       list.length(TaggedCases, NumCases),
%       globals.lookup_int_option(Globals, lookup_switch_size,
%           LookupSize),
%       NumCases >= LookupSize,
%       globals.lookup_int_option(Globals, lookup_switch_req_density,
%           ReqDensity),
%       lookup_switch.is_lookup_switch(CaseVar, TaggedCases, GoalInfo,
%           CanFail, ReqDensity,
%           CodeModel, FirstVal, LastVal, NeedRangeCheck,
%           NeedBitVecCheck, OutVars, CaseVals, !Info)
%   ->
%       MaybeEnd = MaybeEndPrime,
%       ml_lookup_switch.generate(CaseVar, OutVars, CaseVals,
%           FirstVal, LastVal, NeedRangeCheck, NeedBitVecCheck,
%           Decls, Statements, !Info)
%   ;
        % Try using a string hash switch.
        Indexing = yes,
        SwitchCategory = string_switch,
        list.length(TaggedCases, NumCases),
        globals.lookup_int_option(Globals, string_switch_size, StringSize),
        NumCases >= StringSize,
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
        ml_string_switch.generate(TaggedCases, CaseVar, CodeModel,
            CanFail, Context, Decls, Statements, !Info)
    ;
        % Try using a tag switch.
        Indexing = yes,
        SwitchCategory = tag_switch,
        list.length(TaggedCases, NumCases),
        globals.lookup_int_option(Globals, tag_switch_size, TagSize),
        NumCases >= TagSize,
        target_supports_int_switch(Globals)
    ->
        ml_tag_switch.generate(TaggedCases, CaseVar, CodeModel,
            CanFail, Context, Decls, Statements, !Info)
    ;
        % Try using a "direct-mapped" switch. This also handles dense
        % (computed goto) switches -- for those, we first generate a
        % direct-mapped switch, and then convert it into a computed goto switch
        % in ml_simplify_switch.
        %
        Indexing = yes,
        (
            target_supports_switch(SwitchCategory, Globals)
        ;
            SwitchCategory = atomic_switch,
            target_supports_computed_goto(Globals)
        )
    ->
        ml_switch_generate_mlds_switch(TaggedCases, CaseVar, CodeModel,
            CanFail, Context, Decls, Statements, !Info)
    ;
        % The fallback method: if all else fails, generate an if-then-else
        % chain which tests each of the cases in turn.
        ml_switch_generate_if_else_chain(TaggedCases, CaseVar, CodeModel,
            CanFail, Context, Decls, Statements, !Info)
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

target_supports_int_switch_2(c) = yes.
target_supports_int_switch_2(asm) = yes.    % asm means via gnu back-end
target_supports_int_switch_2(il) = no.
target_supports_int_switch_2(java) = yes.
% target_supports_int_switch_2(c_sharp) = yes.

target_supports_string_switch_2(c) = no.
target_supports_string_switch_2(asm) = no.  % asm means via gnu back-end
target_supports_string_switch_2(il) = no.
target_supports_string_switch_2(java) = no.
% target_supports_string_switch_2(c_sharp) = yes.

target_supports_computed_goto_2(c) = yes.
target_supports_computed_goto_2(asm) = no.  % asm means via gnu back-end
    % XXX for asm, it should be `yes', but currently
    % computed gotos are not yet implemented in gcc.m.
target_supports_computed_goto_2(il) = yes.
target_supports_computed_goto_2(java) = no.
% target_supports_computed_goto_2(c_sharp) = no.

target_supports_goto_2(c) = yes.
target_supports_goto_2(asm) = yes.          % asm means via gnu back-end
target_supports_goto_2(il) = yes.
target_supports_goto_2(java) = no.
% target_supports_goto_2(c_sharp) = yes.

%-----------------------------------------------------------------------------%

    % We categorize switches according to whether the value being switched on
    % is an atomic type, a string, or something more complicated.
    %
:- func determine_category(ml_gen_info, prog_var) = switch_category.

determine_category(Info, CaseVar) = SwitchCategory :-
    ml_variable_type(Info, CaseVar, Type),
    ml_gen_info_get_module_info(Info, ModuleInfo),
    type_util.classify_type(ModuleInfo, Type) = TypeCategory,
    SwitchCategory = switch_util.type_cat_to_switch_cat(TypeCategory).

%-----------------------------------------------------------------------------%

    % Look up the representation (tag) for the cons_id in each case.
    % Also look up the priority of each tag test.
    %
:- pred ml_switch_lookup_tags(ml_gen_info::in, list(case)::in, prog_var::in,
    cases_list::out) is det.

ml_switch_lookup_tags(_Info, [], _, []).
ml_switch_lookup_tags(Info, [Case | Cases], Var, [TaggedCase | TaggedCases]) :-
    Case = case(ConsId, Goal),
    ml_variable_type(Info, Var, Type),
    ml_cons_id_to_tag(Info, ConsId, Type, Tag),
    Priority = switch_util.switch_priority(Tag),
    TaggedCase = case(Priority, Tag, ConsId, Goal),
    ml_switch_lookup_tags(Info, Cases, Var, TaggedCases).

%-----------------------------------------------------------------------------%

    % Generate a chain of if-then-elses to test each case in turn.
    %
:- pred ml_switch_generate_if_else_chain(list(extended_case)::in, prog_var::in,
    code_model::in, can_fail::in, prog_context::in,
    mlds_defns::out, statements::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_switch_generate_if_else_chain([], _Var, CodeModel, CanFail, Context,
        [], Statements, !Info) :-
    (
        CanFail = can_fail,
        ml_gen_failure(CodeModel, Context, Statements, !Info)
    ;
        CanFail = cannot_fail,
        unexpected(this_file, "switch failure")
    ).
ml_switch_generate_if_else_chain([Case | Cases], Var, CodeModel, CanFail,
        Context, Decls, Statements, !Info) :-
    Case = case(_, _Tag, ConsId, Goal),
    (
        Cases = [],
        CanFail = cannot_fail
    ->
        ml_gen_goal(CodeModel, Goal, Decls, Statements, !Info)
    ;
        ml_gen_tag_test(Var, ConsId, TagTestDecls, TagTestStatements,
            TagTestExpression, !Info),
        ml_gen_goal(CodeModel, Goal, GoalStatement, !Info),
        ml_switch_generate_if_else_chain(Cases, Var, CodeModel,
            CanFail, Context, RestDecls, RestStatements, !Info),
        Rest = ml_gen_block(RestDecls, RestStatements, Context),
        IfStmt = if_then_else(TagTestExpression, GoalStatement, yes(Rest)),
        IfStatement = statement(IfStmt, mlds_make_context(Context)),
        Decls = TagTestDecls,
        Statements = TagTestStatements ++ [IfStatement]
    ).

%-----------------------------------------------------------------------------%

    % Generate an MLDS switch. This is used for "direct-mapped" switches,
    % where we map a Mercury switch directly to a switch in the target
    % language.
    %
:- pred ml_switch_generate_mlds_switch(list(extended_case)::in, prog_var::in,
    code_model::in, can_fail::in, prog_context::in, mlds_defns::out,
    statements::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_switch_generate_mlds_switch(Cases, Var, CodeModel, CanFail, Context,
        Decls, Statements, !Info) :-
    ml_variable_type(!.Info, Var, Type),
    ml_gen_type(!.Info, Type, MLDS_Type),
    ml_gen_var(!.Info, Var, Lval),
    Rval = lval(Lval),
    ml_switch_gen_range(!.Info, MLDS_Type, Range),
    ml_switch_generate_mlds_cases(Cases, CodeModel, MLDS_Cases, !Info),
    ml_switch_generate_default(CanFail, CodeModel, Context, Default, !Info),
    SwitchStmt0 = switch(MLDS_Type, Rval, Range, MLDS_Cases, Default),
    MLDS_Context = mlds_make_context(Context),
    ml_simplify_switch(SwitchStmt0, MLDS_Context, SwitchStatement, !Info),
    Decls = [],
    Statements = [SwitchStatement].

:- pred ml_switch_gen_range(ml_gen_info::in, mlds_type::in,
    mlds_switch_range::out) is det.

ml_switch_gen_range(Info, MLDS_Type, Range) :-
    (
        ml_gen_info_get_module_info(Info, ModuleInfo),
        ExportedType = to_exported_type(ModuleInfo, Type),
        MLDS_Type = mercury_type(Type, TypeCategory, ExportedType),
        switch_util.type_range(TypeCategory, Type, ModuleInfo,
            MinRange, MaxRange)
    ->
        Range = range(MinRange, MaxRange)
    ;
        Range = range_unknown
    ).

:- pred ml_switch_generate_mlds_cases(list(extended_case)::in,
    code_model::in, list(mlds_switch_case)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_switch_generate_mlds_cases([], _, [], !Info).
ml_switch_generate_mlds_cases([Case | Cases], CodeModel,
        [MLDS_Case | MLDS_Cases], !Info) :-
    ml_switch_generate_mlds_case(Case, CodeModel, MLDS_Case, !Info),
    ml_switch_generate_mlds_cases(Cases, CodeModel, MLDS_Cases, !Info).

:- pred ml_switch_generate_mlds_case(extended_case::in, code_model::in,
    mlds_switch_case::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_switch_generate_mlds_case(Case, CodeModel, MLDS_Case, !Info) :-
    Case = case(_Priority, Tag, _ConsId, Goal),
    ( Tag = int_constant(Int) ->
        Rval = const(int_const(Int))
    ; Tag = string_constant(String) ->
        Rval = const(string_const(String))
    ;
        unexpected(this_file, "ml_switch_gen.m: invalid tag type")
    ),
    ml_gen_goal(CodeModel, Goal, Statement, !Info),
    MLDS_Case = [match_value(Rval)] - Statement.

    % Generate an appropriate default for a switch.
    %
ml_switch_generate_default(CanFail, CodeModel, Context, Default, !Info) :-
    (
        CanFail = can_fail,
        ml_gen_failure(CodeModel, Context, FailStatements, !Info),
        (
            FailStatements = [],
            Default = default_do_nothing
        ;
            FailStatements = [_ | _],
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
