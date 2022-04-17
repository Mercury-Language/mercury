%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2003-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_tag_switch.m.
% Author: fjh.
%
% Generate switches based on primary and secondary tags, for the MLDS
% back-end.
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_tag_switch.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module list.

%---------------------------------------------------------------------------%

    % Generate efficient indexing code for tag based switches,
    % if this can be done without generating duplicates of
    % auxiliary MLDS functions, or environment structures that
    % have more than one copy of a field.
    %
:- pred ml_generate_tag_switch_if_possible(prog_var::in, var_table_entry::in,
    code_model::in, can_fail::in, prog_context::in, packed_word_map::in,
    list(tagged_case)::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.rtti.
:- import_module backend_libs.switch_util.
:- import_module hlds.hlds_data.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_simplify_switch.
:- import_module ml_backend.ml_switch_gen.
:- import_module ml_backend.ml_unify_gen_util.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module uint8.

:- type code_map == map(case_id, maybe_code).

:- type maybe_code
    --->    immediate(mlds_stmt)
    ;       generate(packed_word_map, hlds_goal).

%---------------------------------------------------------------------------%

ml_generate_tag_switch_if_possible(Var, VarEntry, CodeModel, CanFail, Context,
        EntryPackedArgsMap, TaggedCases, Stmts, !Info) :-
    % Group the cases based on primary tag value, find out how many
    % constructors share each primary tag value, and sort the cases so that
    % the most frequently occurring primary tag values come first.
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    get_ptag_counts(ModuleInfo, VarEntry ^ vte_type, MaxPrimary, PtagCountMap),
    group_cases_by_ptag(TaggedCases,
        gen_tagged_case_code(CodeModel, EntryPackedArgsMap),
        map.init, CodeMap, [], ReachableConstVarMaps0,
        may_use_tag_switch, MayUseTagSwitch, !Info,
        _CaseIdPtagsMap, PtagCaseMap),
    % Proceed only if we can do so safely.
    MayUseTagSwitch = may_use_tag_switch,
    ml_generate_tag_switch(Var, VarEntry, CodeModel, CanFail, Context,
        MaxPrimary, CodeMap, PtagCountMap, ReachableConstVarMaps0,
        PtagCaseMap, Stmts, !Info).

:- pred ml_generate_tag_switch(prog_var::in, var_table_entry::in,
    code_model::in, can_fail::in, prog_context::in, uint8::in,
    code_map::in, ptag_count_map::in, list(ml_ground_term_map)::in,
    ptag_case_map(case_id)::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_tag_switch(Var, VarEntry, CodeModel, CanFail, Context, MaxPrimary,
        CodeMap, PtagCountMap, ReachableConstVarMaps0,
        PtagCaseMap, Stmts, !Info) :-
    % Generate the rval for the primary tag.
    ml_gen_var(!.Info, Var, VarEntry, VarLval),
    VarRval = ml_lval(VarLval),
    PtagRval = ml_unop(tag, VarRval),

    order_ptags_by_count(PtagCountMap, PtagCaseMap, PtagCaseList),
    % The code generation scheme that we use below can duplicate the code of a
    % case if the representations of the cons_ids of that case use more than
    % one primary tag. (We generate one copy of the code for each such primary
    % tag.)
    %
    % If generating code for a target language that does not allow the code of
    % a case to be duplicated, we could adopt a code generation scheme
    % like the following pseudocode:
    %
    % index = primary_tag(Var) * max_secondary_tag_value
    % if primary_tag(Var) indicates that Var has secondary_tag
    %   index += secondary_tag(Var)
    % switch on index using the direct mapped scheme
    %
    % For such targets, you would want to employ this code generation scheme,
    % which is not (yet) implemented, if any value in CaseIdPtagsMap is a set
    % with more than one element. One could test for that condition with the
    % (currently unused) find_any_split_cases predicate.

    % Generate the switch on the primary tag.
    gen_ptag_cases(Var, VarEntry, CanFail, CodeModel, Context,
        CodeMap, PtagCountMap, PtagCaseList, PtagCases0,
        ReachableConstVarMaps0, ReachableConstVarMaps, !Info),
    % We compute ReachableConstVarMaps above in two steps, because
    % - gen_tagged_case_code, as invoked by group_cases_by_ptag, generates code
    %   for *some* cases, and
    % - gen_ptag_cases generates code for the remaining cases.
    ml_gen_record_consensus_const_var_map(ReachableConstVarMaps, !Info),

    % Package up the results into a switch statement.
    Range = mlds_switch_range(0, uint8.cast_to_int(MaxPrimary)),
    list.sort(PtagCases0, PtagCases),
    ml_switch_generate_default(CanFail, CodeModel, Context, Default, !Info),
    SwitchStmt0 = ml_stmt_switch(mlds_builtin_type_int(int_type_int),
        PtagRval, Range, PtagCases, Default, Context),
    ml_simplify_switch(SwitchStmt0, SwitchStmt, !Info),
    Stmts = [SwitchStmt].

:- type may_use_tag_switch
    --->    may_not_use_tag_switch
    ;       may_use_tag_switch.

:- pred gen_tagged_case_code(code_model::in, packed_word_map::in,
    tagged_case::in, case_id::out, code_map::in, code_map::out,
    list(ml_ground_term_map)::in, list(ml_ground_term_map)::out,
    may_use_tag_switch::in, may_use_tag_switch::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_tagged_case_code(CodeModel, EntryPackedArgsMap, TaggedCase, CaseId,
        !CodeMap, !ReachableConstVarMaps, !MayUseTagSwitch, Info0, Info) :-
    ml_gen_info_set_packed_word_map(EntryPackedArgsMap, Info0, Info1),
    TaggedCase = tagged_case(_MainTaggedConsId, OtherTaggedConsIds,
        CaseId, Goal),
    ml_gen_goal_as_branch_block(CodeModel, Goal, Stmt,
        !ReachableConstVarMaps, Info1, Info2),
    (
        OtherTaggedConsIds = [],
        MaybeCode = immediate(Stmt),
        Info = Info2
    ;
        OtherTaggedConsIds = [_ | _],
        % Testing whether this case is for two or more cons_ids
        % is conservative.
        % XXX When generating code for a language (such as C)
        % in which two or more values of the switched-on variable
        % can share the same code, the condition we should test for
        % is whether the tags of [MainTaggedConsId | OtherTaggedConsIds]
        % indicate whether this is the case for them. If it is,
        % then returning MaybeCode = immediate(Stmt) and Info = Info2
        % should be fine.
        acc_dup_properties_of_stmt(Stmt,
            does_not_contain_label, HasLabel,
            will_not_gen_aux_pred, HasAuxPred,
            has_no_local_vars, HasLocalVars),
        ( if
            HasAuxPred = will_gen_aux_pred,
            HasLocalVars = has_local_vars
        then
            % Unlike labels and aux functions (see below), the names
            % of MLDS variables do not include a sequence number.
            % Therefore whether we return Stmt to be used duplicated as is,
            % or letting it be generated several times from scratch,
            % the resulting MLDS code will contain duplicate definitions
            % of the local variables indicated by has_local_vars,
            % and when ml_elim_nested moves all local variables *from all
            % the duplicated copies of the block that these came from*
            % to a *single* environment structure, the affected fields
            % of the environment structure will be doubly defined,
            % and the target language compiler will rightly report an error.
            % The simplest way to avoid this is use this setting of
            % !:MayUseTagSwitch to tell ml_generate_tag_switch_if_possible
            % to fail, letting ml_switch_gen.m fall back to an if-then-else
            % chain for the switch. Slow but working target language code
            % beats "fast" but non-compilable target language code :-(
            % Given how long the problem we are guarding against here
            % has lurked in this code without being detected, the speed
            % of the code we generate in such rare cases are extremely
            % unlikely to matter in practice.
            !:MayUseTagSwitch = may_not_use_tag_switch,
            % Since we are forcing ml_generate_tag_switch_if_possible
            % to fail, the MaybeCode and Info values we return won't be used.
            MaybeCode = generate(EntryPackedArgsMap, Goal),
            Info = Info1
        else
            ( if
                ( HasLabel = does_contain_label
                ; HasAuxPred = will_gen_aux_pred
                )
            then
                % Stmt contains either the definition of either a label,
                % or an auxiliary function. We therefore cannot include
                % two copies of Stmt in the code we generate for the switch,
                % because that would leave the label or the aux function
                % multiply defined, but if we create MLDS code for Goal
                % in every branch, things will be fine, because each time
                % we generate code for Goal, any labels and aux functions
                % will have different sequence numbers included in their names.
                MaybeCode = generate(EntryPackedArgsMap, Goal),
                Info = Info1
            else
                MaybeCode = immediate(Stmt),
                Info = Info2
            )
        )
    ),
    map.det_insert(CaseId, MaybeCode, !CodeMap).

%---------------------------------------------------------------------------%

:- type stmt_contains_label
    --->    does_not_contain_label
    ;       does_contain_label.

:- type stmt_will_gen_aux_pred
    --->    will_not_gen_aux_pred
    ;       will_gen_aux_pred.

:- type stmt_has_local_vars
    --->    has_no_local_vars
    ;       has_local_vars.

    % Find out, for the given MLDS statement,
    %
    % - whether it contains any labels;
    % - whether it contains any constructs that ml_elim_nested.m will turn into
    %   an auxiliary structure using an environment, and
    % - whether it defines any local variables.
    %
:- pred acc_dup_properties_of_stmt(mlds_stmt::in,
    stmt_contains_label::in, stmt_contains_label::out,
    stmt_will_gen_aux_pred::in, stmt_will_gen_aux_pred::out,
    stmt_has_local_vars::in, stmt_has_local_vars::out) is det.

acc_dup_properties_of_stmt(Stmt, !HasLabel, !HasAuxPred, !HasLocalVars) :-
    (
        Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, BlockStmts, _Ctxt),
        (
            LocalVarDefns = []
        ;
            LocalVarDefns = [_ | _],
            !:HasLocalVars = has_local_vars
        ),
        (
            FuncDefns = []
        ;
            FuncDefns = [_ | _],
            !:HasAuxPred = will_gen_aux_pred,
            list.foldl3(acc_dup_properties_of_func, FuncDefns,
                !HasLabel, !HasAuxPred, !HasLocalVars)
        ),
        list.foldl3(acc_dup_properties_of_stmt, BlockStmts,
            !HasLabel, !HasAuxPred, !HasLocalVars)
    ;
        Stmt = ml_stmt_while(_Kind, _Rval, BodyStmt, _LoopLocalVars, _Ctxt),
        % _LoopLocalVars does not *create* any new local vars.
        acc_dup_properties_of_stmt(BodyStmt,
            !HasLabel, !HasAuxPred, !HasLocalVars)
    ;
        Stmt = ml_stmt_if_then_else(_Cond, ThenStmt, MaybeElseStmt, _Ctxt),
        acc_dup_properties_of_stmt(ThenStmt,
            !HasLabel, !HasAuxPred, !HasLocalVars),
        (
            MaybeElseStmt = no
        ;
            MaybeElseStmt = yes(ElseStmt),
            acc_dup_properties_of_stmt(ElseStmt,
                !HasLabel, !HasAuxPred, !HasLocalVars)
        )
    ;
        Stmt = ml_stmt_switch(_Type, _Val, _Range, Cases, Default, _Ctxt),
        list.foldl3(acc_dup_properties_of_case, Cases,
            !HasLabel, !HasAuxPred, !HasLocalVars),
        (
            ( Default = default_is_unreachable
            ; Default = default_do_nothing
            )
        ;
            Default = default_case(DefaultStmt),
            acc_dup_properties_of_stmt(DefaultStmt,
                !HasLabel, !HasAuxPred, !HasLocalVars)
        )
    ;
        Stmt = ml_stmt_try_commit(_Ref, BodyStmt, HandlerStmt, _Ctxt),
        !:HasAuxPred = will_gen_aux_pred,
        acc_dup_properties_of_stmt(BodyStmt,
            !HasLabel, !HasAuxPred, !HasLocalVars),
        acc_dup_properties_of_stmt(HandlerStmt,
            !HasLabel, !HasAuxPred, !HasLocalVars)
    ;
        Stmt = ml_stmt_do_commit(_Ref, _Ctxt),
        !:HasAuxPred = will_gen_aux_pred
    ;
        Stmt = ml_stmt_label(_Label, _Ctxt),
        !:HasLabel = does_contain_label
    ;
        ( Stmt = ml_stmt_goto(_, _Ctxt)
        ; Stmt = ml_stmt_computed_goto(_Rval, _Labels, _Ctxt)
        ; Stmt = ml_stmt_call(_Sig, _Func, _Args, _RetLvals, _TailCall, _Ctxt)
        ; Stmt = ml_stmt_return(_Rvals, _Ctxt)
        ; Stmt = ml_stmt_atomic(_AtomicStmt, _Ctxt)
        )
    ).

:- pred acc_dup_properties_of_case(mlds_switch_case::in,
    stmt_contains_label::in, stmt_contains_label::out,
    stmt_will_gen_aux_pred::in, stmt_will_gen_aux_pred::out,
    stmt_has_local_vars::in, stmt_has_local_vars::out) is det.

acc_dup_properties_of_case(Case, !HasLabel, !HasAuxPred, !HasLocalVars) :-
    Case = mlds_switch_case(_HeadMatchCond, _TailMatchConds, Stmt),
    acc_dup_properties_of_stmt(Stmt, !HasLabel, !HasAuxPred, !HasLocalVars).

:- pred acc_dup_properties_of_func(mlds_function_defn::in,
    stmt_contains_label::in, stmt_contains_label::out,
    stmt_will_gen_aux_pred::in, stmt_will_gen_aux_pred::out,
    stmt_has_local_vars::in, stmt_has_local_vars::out) is det.

acc_dup_properties_of_func(FuncDefn, !HasLabel, !HasAuxPred, !HasLocalVars) :-
    FuncDefn = mlds_function_defn(_Name, _Ctxt, _Flags, _OrigPredProcId,
        _Params, FuncBody, _EnvVars, _MaybeTailRec),
    (
        FuncBody = body_external
    ;
        FuncBody = body_defined_here(Stmt),
        acc_dup_properties_of_stmt(Stmt, !HasLabel, !HasAuxPred, !HasLocalVars)
    ).

%---------------------------------------------------------------------------%

:- type is_a_case_split_between_ptags
    --->    no_case_is_split_between_ptags
    ;       some_case_is_split_between_ptags.

:- pred find_any_split_cases(case_id_ptags_map::in,
    is_a_case_split_between_ptags::out) is det.
:- pragma consider_used(pred(find_any_split_cases/2)).

find_any_split_cases(CaseIdPtagsMap, IsAnyCaseSplit) :-
    map.foldl(find_any_split_cases_2, CaseIdPtagsMap,
        no_case_is_split_between_ptags, IsAnyCaseSplit).

:- pred find_any_split_cases_2(case_id::in, set(ptag)::in,
    is_a_case_split_between_ptags::in, is_a_case_split_between_ptags::out)
    is det.

find_any_split_cases_2(_CaseId, Ptags, !IsAnyCaseSplit) :-
    ( if set.is_singleton(Ptags, _OnlyPtag) then
        true
    else
        !:IsAnyCaseSplit = some_case_is_split_between_ptags
    ).

%---------------------------------------------------------------------------%

:- pred gen_ptag_cases(prog_var::in, var_table_entry::in, can_fail::in,
    code_model::in, prog_context::in, code_map::in, ptag_count_map::in,
    ptag_case_group_list(case_id)::in, list(mlds_switch_case)::out,
    list(ml_ground_term_map)::in, list(ml_ground_term_map)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_ptag_cases(_, _, _, _, _, _, _, [], [], !ReachableConstVarMaps, !Info).
gen_ptag_cases(Var, VarEntry, CanFail, CodeModel, Context, CodeMap,
        PtagCountMap, [PtagCase | PtagsCases], [MLDS_Case | MLDS_Cases],
        !ReachableConstVarMaps, !Info) :-
    gen_ptag_case(Var, VarEntry, CanFail, CodeModel, Context, CodeMap,
        PtagCountMap, PtagCase, MLDS_Case, !ReachableConstVarMaps, !Info),
    gen_ptag_cases(Var, VarEntry, CanFail, CodeModel, Context, CodeMap,
        PtagCountMap, PtagsCases, MLDS_Cases, !ReachableConstVarMaps, !Info).

:- pred gen_ptag_case(prog_var::in, var_table_entry::in, can_fail::in,
    code_model::in, prog_context::in, code_map::in, ptag_count_map::in,
    ptag_case_group_entry(case_id)::in, mlds_switch_case::out,
    list(ml_ground_term_map)::in, list(ml_ground_term_map)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_ptag_case(Var, VarEntry, CanFail, CodeModel, Context, CodeMap,
        PtagCountMap, PtagCase, MLDS_Case, !ReachableConstVarMaps, !Info) :-
    PtagCase = ptag_case_group_entry(MainPtag, OtherPtags,
        ptag_case(SecTagLocn, GoalMap)),
    map.lookup(PtagCountMap, MainPtag, CountInfo),
    CountInfo = SecTagLocn1 - MaxSecondary,
    expect(unify(SecTagLocn, SecTagLocn1), $pred,
        "secondary tag locations differ"),
    map.to_assoc_list(GoalMap, GoalList),
    (
        ( SecTagLocn = sectag_none
        ; SecTagLocn = sectag_none_direct_arg
        ),
        % There is no secondary tag, so there is no switch on it.
        (
            GoalList = [],
            unexpected($pred, "no goal for non-shared tag")
        ;
            GoalList = [_Stag - CaseId],
            lookup_code_map(CodeMap, CaseId, CodeModel, Stmt,
                !ReachableConstVarMaps, !Info)
        ;
            GoalList = [_, _ | _],
            unexpected($pred, "more than one goal for non-shared tag")
        )
    ;
        ( SecTagLocn = sectag_local_rest_of_word
        ; SecTagLocn = sectag_local_bits(_, _)
        ; SecTagLocn = sectag_remote_word
        ; SecTagLocn = sectag_remote_bits(_, _)
        ),
        expect(unify(OtherPtags, []), $pred, ">1 ptag with secondary tag"),
        (
            CanFail = cannot_fail,
            CaseCanFail = cannot_fail
        ;
            CanFail = can_fail,
            ( if
                list.length(GoalList, GoalCount),
                FullGoalCount = MaxSecondary + 1,
                FullGoalCount = GoalCount
            then
                CaseCanFail = cannot_fail
            else
                CaseCanFail = can_fail
            )
        ),
        group_stag_cases(GoalList, GroupedGoalList),
        ( if
            GroupedGoalList = [CaseId - _Stags],
            CaseCanFail = cannot_fail
        then
            % There is only one possible matching goal, so we don't need
            % to switch on it. This can happen if the other functor symbols
            % that share this primary tag are ruled out by the initial inst
            % of the switched-on variable.
            lookup_code_map(CodeMap, CaseId, CodeModel, Stmt,
                !ReachableConstVarMaps, !Info)
        else
            gen_stag_switch(Var, VarEntry, CodeModel, CaseCanFail, Context,
                CodeMap, MainPtag, SecTagLocn, GroupedGoalList, Stmt,
                !ReachableConstVarMaps, !Info)
        )
    ),
    MainPtagMatch = make_ptag_match(MainPtag),
    OtherPtagMatches = list.map(make_ptag_match, OtherPtags),
    MLDS_Case = mlds_switch_case(MainPtagMatch, OtherPtagMatches, Stmt).

:- func make_ptag_match(ptag) = mlds_case_match_cond.

make_ptag_match(Ptag) = Cond :-
    Ptag = ptag(PtagUint8),
    Cond = match_value(ml_const(mlconst_int(uint8.cast_to_int(PtagUint8)))).

:- pred lookup_code_map(code_map::in, case_id::in, code_model::in,
    mlds_stmt::out,
    list(ml_ground_term_map)::in, list(ml_ground_term_map)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

lookup_code_map(CodeMap, CaseId, CodeModel, Stmt,
        !ReachableConstVarMaps, !Info) :-
    map.lookup(CodeMap, CaseId, MaybeCode),
    (
        MaybeCode = immediate(Stmt)
    ;
        MaybeCode = generate(EntryPackedArgsMap, Goal),
        ml_gen_info_set_packed_word_map(EntryPackedArgsMap, !Info),
        ml_gen_goal_as_branch_block(CodeModel, Goal, Stmt,
            !ReachableConstVarMaps, !Info)
    ).

%---------------------------------------------------------------------------%

    % A list of secondary tag values that all have the same code.
    % The list is stored in reversed form.
    %
:- type stags
    --->    stags(int, list(int)).

    % Maps case numbers (each of which identifies the code of one switch arm)
    % to the secondary tags that share that code.
    %
:- type stag_rev_map == map(case_id, stags).

:- pred group_stag_cases(stag_goal_list(case_id)::in,
    assoc_list(case_id, stags)::out) is det.

group_stag_cases(Goals, GroupedGoals) :-
    build_stag_rev_map(Goals, map.init, RevMap),
    map.to_assoc_list(RevMap, GroupedGoals).

:- pred build_stag_rev_map(stag_goal_list(case_id)::in,
    stag_rev_map::in, stag_rev_map::out) is det.

build_stag_rev_map([], !RevMap).
build_stag_rev_map([Entry | Entries], !RevMap) :-
    Entry = Stag - CaseId,
    ( if map.search(!.RevMap, CaseId, OldEntry) then
        OldEntry = stags(OldFirstStag, OldLaterStags),
        NewEntry = stags(OldFirstStag, [Stag | OldLaterStags]),
        map.det_update(CaseId, NewEntry, !RevMap)
    else
        NewEntry = stags(Stag, []),
        map.det_insert(CaseId, NewEntry, !RevMap)
    ),
    build_stag_rev_map(Entries, !RevMap).

%---------------------------------------------------------------------------%

:- pred gen_stag_switch(prog_var::in, var_table_entry::in, code_model::in,
    can_fail::in, prog_context::in, code_map::in, ptag::in, sectag_locn::in,
    assoc_list(case_id, stags)::in, mlds_stmt::out,
    list(ml_ground_term_map)::in, list(ml_ground_term_map)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_stag_switch(Var, VarEntry, CodeModel, CanFail, Context, CodeMap,
        Ptag, StagLocn, Cases, Stmt, !ReachableConstVarMaps, !Info) :-
    % Generate the rval for the secondary tag.
    VarType = VarEntry ^ vte_type,
    ml_gen_var(!.Info, Var, VarEntry, VarLval),
    VarRval = ml_lval(VarLval),
    (
        StagLocn = sectag_local_rest_of_word,
        StagRval = ml_unop(unmkbody, VarRval)
    ;
        StagLocn = sectag_local_bits(_, Mask),
        StagRval = ml_binop(bitwise_and(int_type_uint),
            ml_unop(unmkbody, VarRval), ml_const(mlconst_uint(Mask)))
    ;
        StagLocn = sectag_remote_word,
        ml_gen_secondary_tag_rval(!.Info, VarType, VarRval,
            Ptag, StagRval)
    ;
        StagLocn = sectag_remote_bits(_, Mask),
        ml_gen_secondary_tag_rval(!.Info, VarType, VarRval,
            Ptag, StagWordRval),
        StagRval = ml_binop(bitwise_and(int_type_uint),
            StagWordRval, ml_const(mlconst_uint(Mask)))
    ;
        ( StagLocn = sectag_none
        ; StagLocn = sectag_none_direct_arg
        ),
        unexpected($pred, "no stag")
    ),

    % Generate the switch on the secondary tag.
    gen_stag_cases(Cases, CodeMap, CodeModel, StagCases0,
        !ReachableConstVarMaps, !Info),
    list.sort(StagCases0, StagCases),
    ml_switch_generate_default(CanFail, CodeModel, Context, Default, !Info),

    % Package up the results into a switch statement.
    Range = mlds_switch_range_unknown, % XXX could do better
    SwitchStmt = ml_stmt_switch(mlds_builtin_type_int(int_type_int),
        StagRval, Range, StagCases, Default, Context),
    ml_simplify_switch(SwitchStmt, Stmt, !Info).

%---------------------------------------------------------------------------%

:- pred gen_stag_cases(assoc_list(case_id, stags)::in,
    code_map::in, code_model::in, list(mlds_switch_case)::out,
    list(ml_ground_term_map)::in, list(ml_ground_term_map)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_stag_cases([], _, _, [], !ReachableConstVarMaps, !Info).
gen_stag_cases([Group | Groups], CodeMap, CodeModel, [Case | Cases],
        !ReachableConstVarMaps, !Info) :-
    gen_stag_case(Group, CodeMap, CodeModel, Case,
        !ReachableConstVarMaps, !Info),
    gen_stag_cases(Groups, CodeMap, CodeModel, Cases,
        !ReachableConstVarMaps, !Info).

:- pred gen_stag_case(pair(case_id, stags)::in,
    code_map::in, code_model::in, mlds_switch_case::out,
    list(ml_ground_term_map)::in, list(ml_ground_term_map)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_stag_case(Group, CodeMap, CodeModel, MLDS_Case,
        !ReachableConstVarMaps, !Info) :-
    Group = CaseId - Stags,
    Stags = stags(FirstStag, RevLaterStags),
    list.reverse(RevLaterStags, LaterStags),
    FirstMatch = make_match_value(FirstStag),
    LaterMatches = list.map(make_match_value, LaterStags),
    lookup_code_map(CodeMap, CaseId, CodeModel, Stmt,
        !ReachableConstVarMaps, !Info),
    MLDS_Case = mlds_switch_case(FirstMatch, LaterMatches, Stmt).

:- func make_match_value(int) = mlds_case_match_cond.

make_match_value(Stag) = match_value(ml_const(mlconst_int(Stag))).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_tag_switch.
%---------------------------------------------------------------------------%
