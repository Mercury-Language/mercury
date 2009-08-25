%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001, 2003-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ml_simplify_switch.m.
% Main author: fjh.
%
% This module, which is invoked by the various parts of the MLDS code generator
% that generate switches, converts MLDS switches into computed gotos
% or if-then-else chains.
%
% We should eventually also handle lookup switches and binary search switches
% here too.
%
% The choice of which exactly which simplifications will get
% performed depends on the target (e.g. whether it understands
% switches) and the --prefer-switch option.
%
%-----------------------------------------------------------------------------%

:- module ml_backend.ml_simplify_switch.
:- interface.

:- import_module ml_backend.mlds.
:- import_module ml_backend.ml_code_util.

:- pred ml_simplify_switch(mlds_stmt::in, mlds_context::in,
    statement::out, ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.switch_util.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ml_backend.ml_switch_gen.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.

%-----------------------------------------------------------------------------%

ml_simplify_switch(Stmt0, MLDS_Context, Statement, !Info) :-
    ml_gen_info_get_globals(!.Info, Globals),
    (
        % Convert dense int switches into computed gotos,
        % unless the target prefers switches.

        % Is this an int switch?
        Stmt0 = ml_stmt_switch(Type, Rval, Range, Cases, Default),
        is_integral_type(Type) = yes,

        % Does the target want us to convert dense int switches
        % into computed gotos?
        target_supports_computed_goto(Globals),
        \+ (
            target_supports_int_switch(Globals),
            globals.lookup_bool_option(Globals, prefer_switch, yes)
        ),

        % Is the switch big enough?
        list.length(Cases, NumCases),
        globals.lookup_int_option(Globals, dense_switch_size, DenseSize),
        NumCases >= DenseSize,

        % ... and dense enough?
        globals.lookup_int_option(Globals, dense_switch_req_density,
            ReqDensity),
        is_dense_switch(Cases, ReqDensity)
    ->
        maybe_eliminate_default(Range, Cases, Default, ReqDensity,
            FirstVal, LastVal, NeedRangeCheck),
        generate_dense_switch(Cases, Default, FirstVal, LastVal,
            NeedRangeCheck, Type, Rval, MLDS_Context,
            Decls, Statements, !Info),
        Stmt = ml_stmt_block(Decls, Statements),
        Statement = statement(Stmt, MLDS_Context)
    ;
        % Convert the remaining (sparse) int switches into if-then-else chains,
        % unless the target prefers switches.

        Stmt0 = ml_stmt_switch(Type, Rval, _Range, Cases, Default),
        is_integral_type(Type) = yes,
        \+ (
            target_supports_int_switch(Globals),
            globals.lookup_bool_option(Globals, prefer_switch, yes)
        )
    ->
        Statement = ml_switch_to_if_else_chain(Cases, Default, Rval,
            MLDS_Context)
    ;
        % Optimize away trivial switches (these can occur e.g. with
        % --tags none, where the tag test always has only one reachable case)

        Stmt0 = ml_stmt_switch(_Type, _Rval, _Range, Cases, Default),
        Cases = [SingleCase],
        Default = default_is_unreachable
    ->
        SingleCase = mlds_switch_case(_FirstCond, _LaterConds, CaseStatement),
        Statement = CaseStatement
    ;
        Stmt = Stmt0,
        Statement = statement(Stmt, MLDS_Context)
    ).

:- func is_integral_type(mlds_type) = bool.

is_integral_type(MLDSType) = IsIntegral :-
    (
        ( MLDSType = mlds_native_int_type
        ; MLDSType = mlds_native_char_type
        ),
        IsIntegral = yes
    ;
        ( MLDSType = mlds_mercury_array_type(_)
        ; MLDSType = mlds_cont_type(_)
        ; MLDSType = mlds_commit_type
        ; MLDSType = mlds_native_bool_type
        ; MLDSType = mlds_native_float_type
        ; MLDSType = mlds_foreign_type(_)
        ; MLDSType = mlds_class_type(_, _, _)
        ; MLDSType = mlds_ptr_type(_)
        ; MLDSType = mlds_func_type(_)
        ; MLDSType = mlds_type_info_type
        ; MLDSType = mlds_generic_type
        ; MLDSType = mlds_generic_env_ptr_type
        ; MLDSType = mlds_array_type(_)
        ; MLDSType = mlds_pseudo_type_info_type
        ; MLDSType = mlds_rtti_type(_)
        ; MLDSType = mlds_tabling_type(_)
        ; MLDSType = mlds_unknown_type
        ),
        IsIntegral = no
    ;
        MLDSType = mercury_type(_, CtorCat, _),
        (
            ( CtorCat = ctor_cat_builtin(cat_builtin_int)
            ; CtorCat = ctor_cat_builtin(cat_builtin_char)
            ; CtorCat = ctor_cat_enum(cat_enum_mercury)
            ),
            IsIntegral = yes
        ;
            ( CtorCat = ctor_cat_builtin(cat_builtin_string)
            ; CtorCat = ctor_cat_builtin(cat_builtin_float)
            ; CtorCat = ctor_cat_higher_order
            ; CtorCat = ctor_cat_tuple
            ; CtorCat = ctor_cat_builtin_dummy
            ; CtorCat = ctor_cat_variable
            ; CtorCat = ctor_cat_void
            ; CtorCat = ctor_cat_system(_)
            ; CtorCat = ctor_cat_user(cat_user_notag)
            ; CtorCat = ctor_cat_user(cat_user_general)
            ; CtorCat = ctor_cat_user(cat_user_direct_dummy)
            ),
            IsIntegral = no
        ;
            CtorCat = ctor_cat_enum(cat_enum_foreign),
            % XXX We can switch on foreign enumerations in C, but this may
            % not be the case for the other target languages.
            IsIntegral = no
        )
    ).

:- pred is_dense_switch(list(mlds_switch_case)::in, int::in) is semidet.

is_dense_switch(Cases, ReqDensity) :-
    % Need at least two cases
    NumCases = list.length(Cases),
    NumCases > 2,

    % The switch needs to be dense enough.
    find_min_and_max_in_cases(Cases, FirstCaseVal, LastCaseVal),
    CasesRange = LastCaseVal - FirstCaseVal + 1,
    Density = switch_density(NumCases, CasesRange),
    Density > ReqDensity.

    % For switches with a default, we normally need to check that
    % the variable is in range before we index into the jump table.
    % However, if the range of the type is sufficiently small,
    % we can make the jump table large enough to hold all
    % of the values for the type.
    %
:- pred maybe_eliminate_default(mlds_switch_range::in,
    list(mlds_switch_case)::in, mlds_switch_default::in, int::in,
    int::out, int::out, bool::out) is det.

maybe_eliminate_default(Range, Cases, Default, ReqDensity,
        FirstVal, LastVal, NeedRangeCheck) :-
    (
        Default \= default_is_unreachable,
        Range = mlds_switch_range(Min, Max),
        TypeRange = Max - Min + 1,
        NumCases = list.length(Cases),
        NoDefaultDensity = switch_density(NumCases, TypeRange),
        NoDefaultDensity > ReqDensity
    ->
        NeedRangeCheck = no,
        FirstVal = Min,
        LastVal = Max
    ;
        (
            Default = default_is_unreachable,
            NeedRangeCheck = no
        ;
            ( Default = default_do_nothing
            ; Default = default_case(_)
            ),
            NeedRangeCheck = yes
        ),
        find_min_and_max_in_cases(Cases, FirstCaseVal, LastCaseVal),
        FirstVal = FirstCaseVal,
        LastVal = LastCaseVal
    ).

%-----------------------------------------------------------------------------%

    % Find the highest and lowest case values in a list of cases.
    %
:- pred find_min_and_max_in_cases(list(mlds_switch_case)::in,
    int::out, int::out) is det.

find_min_and_max_in_cases(Cases, Min, Max) :-
    list.foldl2(find_min_and_max_in_case, Cases,
        int.max_int, Min, int.min_int, Max).

:- pred find_min_and_max_in_case(mlds_switch_case::in,
    int::in, int::out, int::in, int::out) is det.

find_min_and_max_in_case(Case, !Min, !Max) :-
    Case = mlds_switch_case(FirstCond, LaterConds, _CaseStatement),
    find_min_and_max_in_case_cond(FirstCond, !Min, !Max),
    list.foldl2(find_min_and_max_in_case_cond, LaterConds, !Min, !Max).

:- pred find_min_and_max_in_case_cond(mlds_case_match_cond::in,
    int::in, int::out, int::in, int::out) is det.

find_min_and_max_in_case_cond(match_value(Rval), !Min, !Max) :-
    (
        Rval = ml_const(mlconst_int(Val))
    ->
        int.min(Val, !Min),
        int.max(Val, !Max)
    ;
        unexpected(this_file, "find_min_and_max_in_case_cond: non-int case")
    ).
find_min_and_max_in_case_cond(match_range(MinRval, MaxRval),
        !Min, !Max) :-
    (
        MinRval = ml_const(mlconst_int(RvalMin)),
        MaxRval = ml_const(mlconst_int(RvalMax))
    ->
        int.min(RvalMin, !Min),
        int.max(RvalMax, !Max)
    ;
        unexpected(this_file, "find_min_and_max_in_case_cond: non-int case")
    ).

%-----------------------------------------------------------------------------%

    % Generate code for a switch using a dense jump table.
    %
:- pred generate_dense_switch(list(mlds_switch_case)::in,
    mlds_switch_default::in, int::in, int::in, bool::in,
    mlds_type::in, mlds_rval::in, mlds_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

generate_dense_switch(Cases, Default, FirstVal, LastVal, NeedRangeCheck,
        _Type, Rval, MLDS_Context, Decls, Statements, !Info) :-
    % If the case values start at some number other than 0,
    % then subtract that number to give us a zero-based index.
    ( FirstVal = 0 ->
        Index = Rval
    ;
        Index = ml_binop(int_sub, Rval, ml_const(mlconst_int(FirstVal)))
    ),

    % Now generate the jump table.
    ml_gen_new_label(EndLabel, !Info),
    map.init(CaseLabelsMap0),
    generate_cases(Cases, EndLabel, CaseLabelsMap0,
        CaseLabelsMap, CasesDecls, CasesCode, !Info),
    ml_gen_new_label(DefaultLabel, !Info),
    CaseLabels = get_case_labels(FirstVal, LastVal,
        CaseLabelsMap, DefaultLabel),
    DefaultLabelStatement = statement(ml_stmt_label(DefaultLabel),
        MLDS_Context),
    (
        Default = default_is_unreachable,
        % We still need the label, in case we inserted references to it
        % into (unreachable) slots in the jump table.
        DefaultStatements = [DefaultLabelStatement]
    ;
        Default = default_do_nothing,
        DefaultStatements = [DefaultLabelStatement]
    ;
        Default = default_case(DefaultCase),
        DefaultStatements = [DefaultLabelStatement, DefaultCase]
    ),

    StartComment = statement(
        ml_stmt_atomic(comment("switch (using dense jump table)")),
        MLDS_Context),
    DoJump = statement(ml_stmt_computed_goto(Index, CaseLabels), MLDS_Context),
    EndLabelStatement = statement(ml_stmt_label(EndLabel), MLDS_Context),
    EndComment = statement(ml_stmt_atomic(comment("End of dense switch")),
        MLDS_Context),

    % We may need to check that the value of the variable lies within the
    % appropriate range.
    (
        NeedRangeCheck = yes,
        Difference = LastVal - FirstVal,
        InRange = ml_binop(unsigned_le,
            Index,
            ml_const(mlconst_int(Difference))),
        Else = yes(statement(ml_stmt_block([], DefaultStatements),
            MLDS_Context)),
        SwitchBody = statement(ml_stmt_block([], [DoJump | CasesCode]),
            MLDS_Context),
        DoSwitch = statement(ml_stmt_if_then_else(InRange, SwitchBody, Else),
            MLDS_Context),
        Statements = [StartComment, DoSwitch, EndLabelStatement, EndComment]
    ;
        NeedRangeCheck = no,
        Statements =
            [StartComment, DoJump | CasesCode] ++
            DefaultStatements ++
            [EndLabelStatement, EndComment]
    ),
    Decls = CasesDecls.

:- pred generate_cases(list(mlds_switch_case)::in, mlds_label::in,
    case_labels_map::in, case_labels_map::out,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

generate_cases([], _EndLabel, !CaseLabelsMap, [], [], !Info).
generate_cases([Case | Cases], EndLabel, !CaseLabelsMap, Decls, Statements,
        !Info) :-
    generate_case(Case, EndLabel, !CaseLabelsMap,
        CaseDecls, CaseStatements, !Info),
    generate_cases(Cases, EndLabel, !CaseLabelsMap,
        CasesDecls, CasesStatements, !Info),
    Decls = CaseDecls ++ CasesDecls,
    Statements = CaseStatements ++ CasesStatements.

    % This converts an MLDS switch case into code for a dense switch case,
    % by adding a label at the front and a `goto <EndLabel>' at the end.
    % It also inserts the label for this case into the CaseLabelsMap.
    %
:- pred generate_case(mlds_switch_case::in, mlds_label::in,
    case_labels_map::in, case_labels_map::out,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

generate_case(Case, EndLabel, !CaseLabelsMap, Decls, Statements, !Info) :-
    Case = mlds_switch_case(FirstCond, LaterConds, CaseStatement),
    ml_gen_new_label(ThisLabel, !Info),
    insert_case_into_map(ThisLabel, FirstCond, !CaseLabelsMap),
    list.foldl(insert_case_into_map(ThisLabel), LaterConds, !CaseLabelsMap),
    CaseStatement = statement(_, MLDS_Context),
    LabelComment = statement(ml_stmt_atomic(comment("case of dense switch")),
        MLDS_Context),
    LabelCode = statement(ml_stmt_label(ThisLabel), MLDS_Context),
    JumpComment = statement(
        ml_stmt_atomic(comment("branch to end of dense switch")),
        MLDS_Context),
    JumpCode = statement(ml_stmt_goto(goto_label(EndLabel)), MLDS_Context),
    Decls = [],
    Statements = [LabelComment, LabelCode, CaseStatement,
        JumpComment, JumpCode].

%-----------------------------------------------------------------------------%
%
% We build up a map which records which label should be used for
% each case value.

:- type case_labels_map == map(int, mlds_label).

:- pred insert_case_into_map(mlds_label::in, mlds_case_match_cond::in,
    case_labels_map::in, case_labels_map::out) is det.

insert_case_into_map(ThisLabel, Cond, !CaseLabelsMap) :-
    (
        Cond = match_value(Rval),
        ( Rval = ml_const(mlconst_int(Val)) ->
            map.det_insert(!.CaseLabelsMap, Val, ThisLabel, !:CaseLabelsMap)
        ;
            unexpected(this_file, "insert_case_into_map: non-int case")
        )
    ;
        Cond = match_range(MinRval, MaxRval),
        (
            MinRval = ml_const(mlconst_int(Min)),
            MaxRval = ml_const(mlconst_int(Max))
        ->
            insert_range_into_map(Min, Max, ThisLabel, !CaseLabelsMap)
        ;
            unexpected(this_file, "insert_case_into_map: non-int case")
        )
    ).

:- pred insert_range_into_map(int::in, int::in, mlds_label::in,
    case_labels_map::in, case_labels_map::out) is det.

insert_range_into_map(Min, Max, ThisLabel, !CaseLabelsMap) :-
    ( Min > Max ->
        true
    ;
        map.det_insert(!.CaseLabelsMap, Min, ThisLabel, !:CaseLabelsMap),
        insert_range_into_map(Min + 1, Max, ThisLabel, !CaseLabelsMap)
    ).

%-----------------------------------------------------------------------------%

    % Given the starting and ending case values, the mapping from case values
    % to labels, and the default label to use for case values which aren't in
    % the map, this function returns the list of labels to use for the case
    % values.
    %
:- func get_case_labels(int, int, map(int, mlds_label), mlds_label)
    = list(mlds_label).

get_case_labels(ThisVal, LastVal, CaseLabelsMap, DefaultLabel) = CaseLabels :-
    ( ThisVal > LastVal ->
        CaseLabels = []
    ;
        ( map.search(CaseLabelsMap, ThisVal, CaseLabel0) ->
            CaseLabel = CaseLabel0
        ;
            CaseLabel = DefaultLabel
        ),
        CaseLabels1 = get_case_labels(ThisVal + 1, LastVal,
            CaseLabelsMap, DefaultLabel),
        CaseLabels = [CaseLabel | CaseLabels1]
    ).

%-----------------------------------------------------------------------------%

    % Convert an int switch to a chain of if-then-elses that test each case
    % in turn.
    %
:- func ml_switch_to_if_else_chain(list(mlds_switch_case), mlds_switch_default,
    mlds_rval, mlds_context) = statement.

ml_switch_to_if_else_chain([], Default, _Rval, MLDS_Context) = Statement :-
    (
        Default = default_do_nothing,
        Statement = statement(ml_stmt_block([], []), MLDS_Context)
    ;
        Default = default_is_unreachable,
        Statement = statement(ml_stmt_block([], []), MLDS_Context)
    ;
        Default = default_case(Statement)
    ).
ml_switch_to_if_else_chain([Case | Cases], Default, SwitchRval, MLDS_Context) =
        Statement :-
    Case = mlds_switch_case(FirstMatchCond, LaterMatchConds, CaseStatement),
    (
        Cases = [],
        Default = default_is_unreachable
    ->
        Statement = CaseStatement
    ;
        AllMatchConds = [FirstMatchCond | LaterMatchConds],
        CaseMatchedRval = ml_gen_case_match_conds(AllMatchConds, SwitchRval),
        RestStatement = ml_switch_to_if_else_chain(Cases, Default, SwitchRval,
            MLDS_Context),
        IfStmt = ml_stmt_if_then_else(CaseMatchedRval, CaseStatement,
            yes(RestStatement)),
        Statement = statement(IfStmt, MLDS_Context)
    ).

    % Generate an rval which will be true iff any of the specified list of
    % case conditions matches the specified rval (which must have integral
    % type).
    %
:- func ml_gen_case_match_conds(list(mlds_case_match_cond), mlds_rval)
    = mlds_rval.

ml_gen_case_match_conds([], _) = ml_const(mlconst_false).
ml_gen_case_match_conds([Cond], SwitchRval) =
    ml_gen_case_match_cond(Cond, SwitchRval).
ml_gen_case_match_conds([Cond1, Cond2 | Conds], SwitchRval) =
    ml_binop(logical_or,
        ml_gen_case_match_cond(Cond1, SwitchRval),
        ml_gen_case_match_conds([Cond2 | Conds], SwitchRval)).

    % Generate an rval which will be true iff the specified case condition
    % matches the specified rval (which must have integral type).
    %
:- func ml_gen_case_match_cond(mlds_case_match_cond, mlds_rval) = mlds_rval.

ml_gen_case_match_cond(match_value(CaseRval), SwitchRval) =
    ml_binop(eq, CaseRval, SwitchRval).
ml_gen_case_match_cond(match_range(MinRval, MaxRval), SwitchRval) =
    ml_binop(logical_and,
        ml_binop(int_gt, SwitchRval, MinRval),
        ml_binop(int_le, SwitchRval, MaxRval)).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "ml_simplify_switch.m".

%-----------------------------------------------------------------------------%
