%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2001, 2003-2011 The University of Melbourne.
% Copyright (C) 2015-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
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
% The choice of which exactly which simplifications will get performed
% depends on the target (e.g. whether it understands switches) and the
% --prefer-switch option.
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_simplify_switch.
:- interface.

:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.

:- pred ml_simplify_switch(mlds_stmt::in, mlds_stmt::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.switch_util.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_target_util.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.

%---------------------------------------------------------------------------%

ml_simplify_switch(Stmt0, Stmt, !Info) :-
    ml_gen_info_get_globals(!.Info, Globals),
    ( if
        % Convert dense int switches into computed gotos,
        % unless the target prefers switches.

        % Is this an int switch?
        Stmt0 = ml_stmt_switch(Type, Rval, Range, Cases, Default, Context),
        is_integral_type(Type, IntType),

        % Does the target want us to convert dense int switches
        % into computed gotos?
        globals_target_supports_computed_goto(Globals) = yes,
        not (
            globals_target_supports_int_type_switch(Globals, IntType) = yes,
            globals.lookup_bool_option(Globals, prefer_switch, yes)
        ),

        % Is the switch big enough?
        list.length(Cases, NumCases),
        globals.get_opt_tuple(Globals, OptTuple),
        DenseSize = OptTuple ^ ot_dense_switch_size,
        NumCases >= DenseSize,

        % ... and dense enough?
        ReqDensity = OptTuple ^ ot_dense_switch_req_density,
        is_dense_switch(Cases, ReqDensity)
    then
        maybe_eliminate_default(Range, Cases, Default, ReqDensity,
            FirstVal, LastVal, NeedRangeCheck),
        generate_dense_switch(Cases, Default, FirstVal, LastVal,
            NeedRangeCheck, Type, Rval, Context, Stmts, !Info),
        Stmt = ml_stmt_block([], [], Stmts, Context)
    else if
        % Convert the remaining (sparse) int switches into if-then-else chains,
        % unless the target prefers switches.

        Stmt0 = ml_stmt_switch(Type, Rval, _Range, Cases, Default, Context),
        is_integral_type(Type, IntType),
        not (
            globals_target_supports_int_type_switch(Globals, IntType) = yes,
            globals.lookup_bool_option(Globals, prefer_switch, yes)
        )
    then
        Stmt = ml_switch_to_if_else_chain(Cases, Default, Rval, Context)
    else if
        % Optimize away trivial switches (these can occur e.g. with
        % --tags none, where the tag test always has only one reachable case)

        Stmt0 = ml_stmt_switch(_Type, _Rval, _Range, Cases, Default, _Context),
        Cases = [SingleCase],
        Default = default_is_unreachable
    then
        SingleCase = mlds_switch_case(_FirstCond, _LaterConds, CaseStmt),
        Stmt = CaseStmt
    else
        Stmt = Stmt0
    ).

:- pred is_integral_type(mlds_type::in, int_type::out) is semidet.

is_integral_type(MLDSType, IntType) :-
    require_complete_switch [MLDSType]
    (
        MLDSType = mlds_builtin_type_int(IntType)
    ;
        MLDSType = mlds_builtin_type_char,
        IntType = int_type_int
    ;
        ( MLDSType = mlds_builtin_type_float
        ; MLDSType = mlds_builtin_type_string
        ),
        fail
    ;
        ( MLDSType = mlds_mercury_array_type(_)
        ; MLDSType = mlds_cont_type(_)
        ; MLDSType = mlds_commit_type
        ; MLDSType = mlds_native_bool_type
        ; MLDSType = mlds_builtin_type_float
        ; MLDSType = mlds_foreign_type(_)
        ; MLDSType = mlds_class_type(_)
        ; MLDSType = mlds_ptr_type(_)
        ; MLDSType = mlds_func_type(_)
        ; MLDSType = mlds_type_info_type
        ; MLDSType = mlds_generic_type
        ; MLDSType = mlds_generic_env_ptr_type
        ; MLDSType = mlds_array_type(_)
        ; MLDSType = mlds_mostly_generic_array_type(_)
        ; MLDSType = mlds_pseudo_type_info_type
        ; MLDSType = mlds_rtti_type(_)
        ; MLDSType = mlds_tabling_type(_)
        ; MLDSType = mlds_unknown_type
        ),
        fail
    ;
        MLDSType = mercury_nb_type(_, CtorCat),
        require_complete_switch [CtorCat]
        (
            CtorCat = ctor_cat_enum(cat_enum_mercury),
            IntType = int_type_int
        ;
            ( CtorCat = ctor_cat_higher_order
            ; CtorCat = ctor_cat_tuple
            ; CtorCat = ctor_cat_builtin_dummy
            ; CtorCat = ctor_cat_variable
            ; CtorCat = ctor_cat_void
            ; CtorCat = ctor_cat_system(_)
            ; CtorCat = ctor_cat_user(cat_user_general)
            ; CtorCat = ctor_cat_user(cat_user_notag)
            ; CtorCat = ctor_cat_user(cat_user_abstract_notag)
            ; CtorCat = ctor_cat_user(cat_user_direct_dummy)
            ; CtorCat = ctor_cat_user(cat_user_abstract_dummy)
            ),
            fail
        ;
            CtorCat = ctor_cat_enum(cat_enum_foreign),
            % XXX We can switch on foreign enumerations in C, but this may
            % not be the case for the other target languages.
            fail
        ;
            CtorCat = ctor_cat_builtin(_),
            unexpected($pred, "mercury_nb_type but ctor_cat_builtin")
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
    int::out, int::out, need_range_check::out) is det.

maybe_eliminate_default(Range, Cases, Default, ReqDensity,
        FirstVal, LastVal, NeedRangeCheck) :-
    ( if
        Default \= default_is_unreachable,
        Range = mlds_switch_range(Min, Max),
        TypeRange = Max - Min + 1,
        NumCases = list.length(Cases),
        NoDefaultDensity = switch_density(NumCases, TypeRange),
        NoDefaultDensity > ReqDensity
    then
        NeedRangeCheck = dont_need_range_check,
        FirstVal = Min,
        LastVal = Max
    else
        (
            Default = default_is_unreachable,
            NeedRangeCheck = dont_need_range_check
        ;
            ( Default = default_do_nothing
            ; Default = default_case(_)
            ),
            NeedRangeCheck = need_range_check
        ),
        find_min_and_max_in_cases(Cases, FirstCaseVal, LastCaseVal),
        FirstVal = FirstCaseVal,
        LastVal = LastCaseVal
    ).

%---------------------------------------------------------------------------%

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
    Case = mlds_switch_case(FirstCond, LaterConds, _CaseStmt),
    find_min_and_max_in_case_cond(FirstCond, !Min, !Max),
    list.foldl2(find_min_and_max_in_case_cond, LaterConds, !Min, !Max).

:- pred find_min_and_max_in_case_cond(mlds_case_match_cond::in,
    int::in, int::out, int::in, int::out) is det.

find_min_and_max_in_case_cond(match_value(Rval), !Min, !Max) :-
    ( if
        Rval = ml_const(mlconst_int(Val))
    then
        int.min(Val, !Min),
        int.max(Val, !Max)
    else
        unexpected($pred, "non-int case")
    ).
find_min_and_max_in_case_cond(match_range(MinRval, MaxRval), !Min, !Max) :-
    ( if
        MinRval = ml_const(mlconst_int(RvalMin)),
        MaxRval = ml_const(mlconst_int(RvalMax))
    then
        int.min(RvalMin, !Min),
        int.max(RvalMax, !Max)
    else
        unexpected($pred, "non-int case")
    ).

%---------------------------------------------------------------------------%

    % Generate code for a switch using a dense jump table.
    %
:- pred generate_dense_switch(list(mlds_switch_case)::in,
    mlds_switch_default::in, int::in, int::in, need_range_check::in,
    mlds_type::in, mlds_rval::in, prog_context::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

generate_dense_switch(Cases, Default, FirstVal, LastVal, NeedRangeCheck,
        _Type, Rval, Context, Stmts, !Info) :-
    % If the case values start at some number other than 0,
    % then subtract that number to give us a zero-based index.
    ( if FirstVal = 0 then
        Index = Rval
    else
        Index = ml_binop(int_sub(int_type_int), Rval,
            ml_const(mlconst_int(FirstVal)))
    ),

    % Now generate the jump table.
    ml_gen_new_label(EndLabel, !Info),
    map.init(CaseLabelsMap0),
    generate_cases(Cases, EndLabel, CaseLabelsMap0, CaseLabelsMap,
        CasesCode, !Info),
    ml_gen_new_label(DefaultLabel, !Info),
    CaseLabels = get_case_labels(FirstVal, LastVal,
        CaseLabelsMap, DefaultLabel),
    DefaultLabelStmt = ml_stmt_label(DefaultLabel, Context),
    (
        Default = default_is_unreachable,
        % We still need the label, in case we inserted references to it
        % into (unreachable) slots in the jump table.
        DefaultStmts = [DefaultLabelStmt]
    ;
        Default = default_do_nothing,
        DefaultStmts = [DefaultLabelStmt]
    ;
        Default = default_case(DefaultCase),
        DefaultStmts = [DefaultLabelStmt, DefaultCase]
    ),

    StartComment = ml_stmt_atomic(comment("switch (using dense jump table)"),
        Context),
    DoJump = ml_stmt_computed_goto(Index, CaseLabels, Context),
    EndLabelStmt = ml_stmt_label(EndLabel, Context),
    EndComment = ml_stmt_atomic(comment("End of dense switch"), Context),

    % We may need to check that the value of the variable lies within the
    % appropriate range.
    (
        NeedRangeCheck = need_range_check,
        Difference = LastVal - FirstVal,
        InRange = ml_binop(unsigned_le,
            Index, ml_const(mlconst_int(Difference))),
        Else = yes(ml_stmt_block([], [], DefaultStmts, Context)),
        SwitchBody = ml_stmt_block([], [], [DoJump | CasesCode], Context),
        DoSwitch = ml_stmt_if_then_else(InRange, SwitchBody, Else, Context),
        Stmts = [StartComment, DoSwitch, EndLabelStmt, EndComment]
    ;
        NeedRangeCheck = dont_need_range_check,
        Stmts =
            [StartComment, DoJump | CasesCode] ++
            DefaultStmts ++
            [EndLabelStmt, EndComment]
    ).

:- pred generate_cases(list(mlds_switch_case)::in, mlds_label::in,
    case_labels_map::in, case_labels_map::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

generate_cases([], _EndLabel, !CaseLabelsMap, [], !Info).
generate_cases([Case | Cases], EndLabel, !CaseLabelsMap, Stmts, !Info) :-
    generate_case(Case, EndLabel, !CaseLabelsMap, CaseStmts, !Info),
    generate_cases(Cases, EndLabel, !CaseLabelsMap, CasesStmts, !Info),
    Stmts = CaseStmts ++ CasesStmts.

    % This converts an MLDS switch case into code for a dense switch case,
    % by adding a label at the front and a `goto <EndLabel>' at the end.
    % It also inserts the label for this case into the CaseLabelsMap.
    %
:- pred generate_case(mlds_switch_case::in, mlds_label::in,
    case_labels_map::in, case_labels_map::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

generate_case(Case, EndLabel, !CaseLabelsMap, Stmts, !Info) :-
    Case = mlds_switch_case(FirstCond, LaterConds, CaseStmt),
    ml_gen_new_label(ThisLabel, !Info),
    insert_case_into_map(ThisLabel, FirstCond, !CaseLabelsMap),
    list.foldl(insert_case_into_map(ThisLabel), LaterConds, !CaseLabelsMap),
    Context = get_mlds_stmt_context(CaseStmt),
    LabelComment = ml_stmt_atomic(comment("case of dense switch"), Context),
    LabelCode = ml_stmt_label(ThisLabel, Context),
    JumpComment = ml_stmt_atomic(comment("branch to end of dense switch"),
        Context),
    JumpCode = ml_stmt_goto(goto_label(EndLabel), Context),
    Stmts = [LabelComment, LabelCode, CaseStmt, JumpComment, JumpCode].

%---------------------------------------------------------------------------%
%
% We build up a map which records which label should be used for
% each case value.
%

:- type case_labels_map == map(int, mlds_label).

:- pred insert_case_into_map(mlds_label::in, mlds_case_match_cond::in,
    case_labels_map::in, case_labels_map::out) is det.

insert_case_into_map(ThisLabel, Cond, !CaseLabelsMap) :-
    (
        Cond = match_value(Rval),
        ( if Rval = ml_const(mlconst_int(Val)) then
            map.det_insert(Val, ThisLabel, !CaseLabelsMap)
        else
            unexpected($pred, "non-int case")
        )
    ;
        Cond = match_range(MinRval, MaxRval),
        ( if
            MinRval = ml_const(mlconst_int(Min)),
            MaxRval = ml_const(mlconst_int(Max))
        then
            insert_range_into_map(Min, Max, ThisLabel, !CaseLabelsMap)
        else
            unexpected($pred, "non-int case")
        )
    ).

:- pred insert_range_into_map(int::in, int::in, mlds_label::in,
    case_labels_map::in, case_labels_map::out) is det.

insert_range_into_map(Min, Max, ThisLabel, !CaseLabelsMap) :-
    ( if Min > Max then
        true
    else
        map.det_insert(Min, ThisLabel, !CaseLabelsMap),
        insert_range_into_map(Min + 1, Max, ThisLabel, !CaseLabelsMap)
    ).

%---------------------------------------------------------------------------%

    % Given the starting and ending case values, the mapping from case values
    % to labels, and the default label to use for case values which aren't in
    % the map, this function returns the list of labels to use for the case
    % values.
    %
:- func get_case_labels(int, int, map(int, mlds_label), mlds_label)
    = list(mlds_label).

get_case_labels(ThisVal, LastVal, CaseLabelsMap, DefaultLabel) = CaseLabels :-
    ( if ThisVal > LastVal then
        CaseLabels = []
    else
        ( if map.search(CaseLabelsMap, ThisVal, CaseLabel0) then
            CaseLabel = CaseLabel0
        else
            CaseLabel = DefaultLabel
        ),
        CaseLabels1 = get_case_labels(ThisVal + 1, LastVal,
            CaseLabelsMap, DefaultLabel),
        CaseLabels = [CaseLabel | CaseLabels1]
    ).

%---------------------------------------------------------------------------%

    % Convert an int switch to a chain of if-then-elses that test each case
    % in turn.
    %
:- func ml_switch_to_if_else_chain(list(mlds_switch_case), mlds_switch_default,
    mlds_rval, prog_context) = mlds_stmt.

ml_switch_to_if_else_chain([], Default, _Rval, Context) = Stmt :-
    (
        Default = default_do_nothing,
        Stmt = ml_stmt_block([], [], [], Context)
    ;
        Default = default_is_unreachable,
        Stmt = ml_stmt_block([], [], [], Context)
    ;
        Default = default_case(Stmt)
    ).
ml_switch_to_if_else_chain([Case | Cases], Default, SwitchRval, Context) =
        Stmt :-
    Case = mlds_switch_case(FirstMatchCond, LaterMatchConds, CaseStmt),
    ( if
        Cases = [],
        Default = default_is_unreachable
    then
        Stmt = CaseStmt
    else
        AllMatchConds = [FirstMatchCond | LaterMatchConds],
        CaseMatchedRval = ml_gen_case_match_conds(AllMatchConds, SwitchRval),
        RestStmt = ml_switch_to_if_else_chain(Cases, Default, SwitchRval,
            Context),
        Stmt = ml_stmt_if_then_else(CaseMatchedRval, CaseStmt, yes(RestStmt),
            Context)
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
    ml_binop(eq(int_type_int), CaseRval, SwitchRval).
ml_gen_case_match_cond(match_range(MinRval, MaxRval), SwitchRval) =
    ml_binop(logical_and,
        ml_binop(int_gt(int_type_int), SwitchRval, MinRval),
        ml_binop(int_le(int_type_int), SwitchRval, MaxRval)).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_simplify_switch.
%---------------------------------------------------------------------------%
