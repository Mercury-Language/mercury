%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2003-2012 The University of Melbourne.
% Copyright (C) 2013, 2015, 2017-2018, 2021-2022, 2024 The Mercury team.
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
:- import_module backend_libs.switch_util.
:- import_module hlds.hlds_data.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_simplify_switch.
:- import_module ml_backend.ml_switch_gen.
:- import_module ml_backend.ml_unify_gen_util.

:- import_module assoc_list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module set.
:- import_module uint.
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
    % the groups covering the most function symbols come first.
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    get_ptag_counts(ModuleInfo, VarEntry ^ vte_type, MaxPrimary, PtagCountMap),
    group_cases_by_ptag(PtagCountMap, TaggedCases,
        gen_tagged_case_code(CodeModel, EntryPackedArgsMap),
        map.init, CodeMap, [], ReachableConstVarMaps0,
        may_use_tag_switch, MayUseTagSwitch, !Info,
        _CaseIdPtagsMap, _PtagCaseMap, PtagCaseGroups0),
    order_ptag_groups_by_count(PtagCaseGroups0, PtagCaseGroups),
    % Proceed only if we can do so safely.
    MayUseTagSwitch = may_use_tag_switch,
    ml_generate_tag_switch(Var, VarEntry, CodeModel, CanFail, Context,
        MaxPrimary, CodeMap, ReachableConstVarMaps0, PtagCaseGroups, Stmts,
        !Info).

:- pred ml_generate_tag_switch(prog_var::in, var_table_entry::in,
    code_model::in, can_fail::in, prog_context::in, uint8::in,
    code_map::in, list(ml_ground_term_map)::in,
    list(ptag_case_group(case_id))::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_tag_switch(Var, VarEntry, CodeModel, CanFail, Context, MaxPrimary,
        CodeMap, ReachableConstVarMaps0, PtagCaseGroups, Stmts, !Info) :-
    % Generate the rval for the primary tag.
    ml_gen_var(!.Info, Var, VarEntry, VarLval),
    VarRval = ml_lval(VarLval),
    PtagRval = ml_unop(tag, VarRval),

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
    % ZZZ
    % XXX SWITCH If the length of SectagCases0 is small enough (2, or maybe 3)
    % and there is no default case needed, then consider generating code
    % that does one (or maybe two) if-then-elses, with the conditions
    % being bitmap checks. The details are explained below after the next
    % occurrence of XXX SWITCH, but their application here would be simpler,
    % due to the max ptag being just 7.
    gen_ptag_cases(Var, VarEntry, CanFail, CodeModel, Context, CodeMap,
        PtagCaseGroups, MLDS_Cases0,
        ReachableConstVarMaps0, ReachableConstVarMaps, !Info),
    % We compute ReachableConstVarMaps above in two steps, because
    % - gen_tagged_case_code, as invoked by group_cases_by_ptag, generates code
    %   for *some* cases, and
    % - gen_ptag_cases generates code for the remaining cases.
    ml_gen_record_consensus_const_var_map(ReachableConstVarMaps, !Info),

    % Package up the results into a switch statement.
    Range = mlds_switch_range(0, uint8.cast_to_int(MaxPrimary)),
    list.sort(MLDS_Cases0, MLDS_Cases),
    ml_switch_generate_default(CanFail, CodeModel, Context, Default, !Info),
    SwitchStmt0 = ml_stmt_switch(mlds_builtin_type_int(int_type_int),
        PtagRval, Range, MLDS_Cases, Default, Context),
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
            % The simplest way to avoid this is to use this setting of
            % !:MayUseTagSwitch to tell ml_generate_tag_switch_if_possible
            % to fail, letting ml_switch_gen.m fall back to an if-then-else
            % chain for the switch. Slow but working target language code
            % beats "fast" but non-compilable target language code :-(
            % Given how long the problem we are guarding against here
            % has lurked in this code without being detected, the speed
            % of the code we generate in such rare cases is extremely
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

    % This type, as its name implies, is an extension of the mlds_switch_case
    % type, the addition being the maybe(case_id) field. Its semantics is that
    % if it contains "yes(CaseId)", then the case contains the implementation
    % of the given arm of the switch we are implementing, and this MLDS case
    % can be merged with any other MLDS case that implements the same CaseId.
    % If this field contains "no", then the case contains code for more than
    % one case_id of the original switch, and this MLDS case cannot be
    % merged with any other.
    %
    % It is an invariant that if two mlds_switch_case_ids both contain
    % "yes(CaseId)" for the same CaseId, then their statements will be
    % identical. We guarantee this by setting this field to yes(CaseId) only
    % when getting the mlds_stmt out of the CodeMap by looking up CaseId in it.
    % Two lookups of the same key in the same map must yield the same result.
:- type mlds_switch_case_id
    --->    mlds_switch_case_id(
                mlds_case_match_cond,
                list(mlds_case_match_cond),
                maybe(case_id),
                mlds_stmt
            ).

:- pred gen_ptag_cases(prog_var::in, var_table_entry::in, can_fail::in,
    code_model::in, prog_context::in, code_map::in,
    list(ptag_case_group(case_id))::in, list(mlds_switch_case)::out,
    list(ml_ground_term_map)::in, list(ml_ground_term_map)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_ptag_cases(_, _, _, _, _, _, [], [], !ReachableConstVarMaps, !Info).
gen_ptag_cases(Var, VarEntry, CanFail, CodeModel, Context, CodeMap,
        [PtagCase | PtagsCases], [MLDS_Case | MLDS_Cases],
        !ReachableConstVarMaps, !Info) :-
    gen_ptag_case(Var, VarEntry, CanFail, CodeModel, Context, CodeMap,
        PtagCase, MLDS_Case, !ReachableConstVarMaps, !Info),
    gen_ptag_cases(Var, VarEntry, CanFail, CodeModel, Context, CodeMap,
        PtagsCases, MLDS_Cases, !ReachableConstVarMaps, !Info).

:- pred gen_ptag_case(prog_var::in, var_table_entry::in, can_fail::in,
    code_model::in, prog_context::in, code_map::in,
    ptag_case_group(case_id)::in, mlds_switch_case::out,
    list(ml_ground_term_map)::in, list(ml_ground_term_map)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_ptag_case(Var, VarEntry, CanFail, CodeModel, Context, CodeMap,
        PtagGroup, MLDS_Case, !ReachableConstVarMaps, !Info) :-
    (
        PtagGroup = one_or_more_whole_ptags(WholeInfo),
        WholeInfo = whole_ptags_info(MainPtag, OtherPtags, _NF, CaseId),
        % There is no secondary tag, so there is no switch on it.
        lookup_code_map(CodeMap, CaseId, CodeModel, Stmt,
            !ReachableConstVarMaps, !Info),
        MainPtagMatch = make_ptag_match(MainPtag),
        OtherPtagMatches = list.map(make_ptag_match, OtherPtags)
    ;
        PtagGroup = one_shared_ptag(SharedInfo),
        SharedInfo = shared_ptag_info(Ptag, SharedSectagLocn, MaxSectag, _NF,
            SectagToGoalMap, CaseIdToSectagsMap),
        MainPtagMatch = make_ptag_match(Ptag),
        OtherPtagMatches = [],
        (
            CanFail = cannot_fail,
            CaseCanFail = cannot_fail
        ;
            CanFail = can_fail,
            map.count(SectagToGoalMap, NumSectagsWithGoals),
            % The +1 is to account for the fact that secondary tags go from
            % 0 to MaxSecondary, both inclusive.
            ( if uint.cast_from_int(NumSectagsWithGoals) = MaxSectag + 1u then
                CaseCanFail = cannot_fail
            else
                CaseCanFail = can_fail
            )
        ),
        map.to_sorted_assoc_list(CaseIdToSectagsMap, CaseIdToSectagsAL),
        gen_stag_switch(Var, VarEntry, CodeModel, CaseCanFail, Context,
            CodeMap, Ptag, SharedSectagLocn, CaseIdToSectagsAL, Stmt,
            !ReachableConstVarMaps, !Info)
    ),
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

:- pred gen_stag_switch(prog_var::in, var_table_entry::in, code_model::in,
    can_fail::in, prog_context::in, code_map::in, ptag::in,
    shared_sectag_locn::in, assoc_list(case_id, one_or_more(uint))::in,
    mlds_stmt::out,
    list(ml_ground_term_map)::in, list(ml_ground_term_map)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_stag_switch(Var, VarEntry, CodeModel, CanFail, Context, CodeMap,
        Ptag, SharedSectagLocn, Cases, Stmt, !ReachableConstVarMaps, !Info) :-
    % Generate the rval for the secondary tag.
    VarType = VarEntry ^ vte_type,
    ml_gen_var(!.Info, Var, VarEntry, VarLval),
    VarRval = ml_lval(VarLval),
    (
        SharedSectagLocn = sectag_local_rest_of_word,
        SectagRval = ml_unop(unmkbody, VarRval)
    ;
        SharedSectagLocn = sectag_local_bits(_, Mask),
        SectagRval = ml_binop(bitwise_and(int_type_uint),
            ml_unop(unmkbody, VarRval), ml_const(mlconst_uint(Mask)))
    ;
        SharedSectagLocn = sectag_remote_word,
        ml_gen_secondary_tag_rval(!.Info, VarType, VarRval,
            Ptag, SectagRval)
    ;
        SharedSectagLocn = sectag_remote_bits(_, Mask),
        ml_gen_secondary_tag_rval(!.Info, VarType, VarRval,
            Ptag, SectagWordRval),
        SectagRval = ml_binop(bitwise_and(int_type_uint),
            SectagWordRval, ml_const(mlconst_uint(Mask)))
    ),

    % Generate the switch on the secondary tag.
    % XXX SWITCH If the length of SectagCases0 is small enough (2, or maybe 3)
    % and there is no default case needed, then consider generating code
    % that does one (or maybe two) if-then-elses, with the conditions
    % being bitmap checks. We would construct a bitmap at compile time
    % from the sectags of a case (by setting the 1 << Sectag for every
    % Sectag in the case), and then test at runtime whether 1 << ActualSectag
    % has its bit set in the resulting bitmap. We would need either
    % ceil(MaxSectag/64) or ceil(MaxSectag/32) words for the bitmap,
    % depending on the word size of the target machine. (Technically,
    % we could use uint64s even on 32 bit machines, but looking for a set bit
    % in 2N 32 bit words will be almost certainly faster than looking in
    % N 64 bit words, when each half of those 64 bit words have to be tested
    % individually.)
    %
    % If there are only two cases, we could use the trick of testing for
    % "is the actual sectag in the set covered by the first case?" by testing
    % for "is the actual sectag in the set covered by the second case?",
    % and negating the result. This may be faster if it replaces a test
    % in more than one word with a test in just one word.
    %
    % NOTE While target language compilers could convert an MLDS switch into
    % an if-then-else chain using bitmap tests, they *cannot* use the above
    % trick, because unlike us, they are not allowed to assume that the value
    % of the actual secondary tag at runtime is in the range 0 .. MaxSectag;
    % in fact, they don't even know the value of MaxSectag.
    gen_stag_cases(Cases, CodeMap, CodeModel, SectagCases0,
        !ReachableConstVarMaps, !Info),
    list.sort(SectagCases0, SectagCases),
    ml_switch_generate_default(CanFail, CodeModel, Context, Default, !Info),

    % Package up the results into a switch statement.
    Range = mlds_switch_range_unknown, % XXX could do better
    SwitchStmt = ml_stmt_switch(mlds_builtin_type_int(int_type_int),
        SectagRval, Range, SectagCases, Default, Context),
    ml_simplify_switch(SwitchStmt, Stmt, !Info).

%---------------------------------------------------------------------------%

:- pred gen_stag_cases(assoc_list(case_id, one_or_more(uint))::in,
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

:- pred gen_stag_case(pair(case_id, one_or_more(uint))::in,
    code_map::in, code_model::in, mlds_switch_case::out,
    list(ml_ground_term_map)::in, list(ml_ground_term_map)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_stag_case(Group, CodeMap, CodeModel, MLDS_Case,
        !ReachableConstVarMaps, !Info) :-
    Group = CaseId - one_or_more(FirstStag, LaterStags),
    FirstMatch = make_match_value(FirstStag),
    LaterMatches = list.map(make_match_value, LaterStags),
    lookup_code_map(CodeMap, CaseId, CodeModel, Stmt,
        !ReachableConstVarMaps, !Info),
    MLDS_Case = mlds_switch_case(FirstMatch, LaterMatches, Stmt).

:- func make_match_value(uint) = mlds_case_match_cond.

make_match_value(Sectag) = Match :-
    Match = match_value(ml_const(mlconst_int(uint.cast_to_int(Sectag)))).

%---------------------------------------------------------------------------%

    % ZZZ move this commeent to switch_util.m
    % The job of merge_any_arms_for_same_case_id is to generate target language
    % code that should be more easily optimizable by the target language
    % compiler. Without it, we can generate MLDS code that looks like this:
    %
    %   switch (MR_tag((MR_Word) HeadVar__3_3)) {
    %     default: /*NOTREACHED*/ MR_assert(0);
    %     case (MR_Integer) 0:
    %       *HeadVar__1_1 = (MR_Integer) 0;
    %       break;
    %     case (MR_Integer) 1:
    %     case (MR_Integer) 2:
    %     case (MR_Integer) 3:
    %     case (MR_Integer) 4:
    %     case (MR_Integer) 5:
    %     case (MR_Integer) 6:
    %       *HeadVar__1_1 = (MR_Integer) 1;
    %       break;
    %     case (MR_Integer) 7:
    %       *HeadVar__1_1 = (MR_Integer) 1;
    %       break;
    %   }
    %
    % (This is actual C code generated by mmc for part of the compare predicate
    % of the expr type in tests/benchmarks/deriv.m.)
    %
    % Note that we have three separate arms of this switch that all do the same
    % thing. This is not coincidence: they all implement the body of the same
    % switch arm in the original HLDS. However, this switch arm is the active
    % switch arm for nine of the type's ten cons_ids, including the six that
    % are mapped to the unshared primary tags 1 through 6, and all three
    % that are mapped to the shared primary tag 7. We generate code for the
    % shared primary tag separately from the unshared ptags, since we may need
    % to generate different code for each value of the remote secondary tag,
    % but in this case, there is no such need. The same issue can arise
    % with primary tag 0 which can be shared between several different cons_ids
    % distinguished by a local secondary tag, though that is not an issue
    % in this example.
    %
    % This predicate merges together MLDS cases that all implement the same arm
    % of the original switch. For the code that previously resulted in the MLDS
    % code above, we now generate this:
    %
    %  switch (MR_tag((MR_Word) HeadVar__3_3)) {
    %     default: /*NOTREACHED*/ MR_assert(0);
    %     case (MR_Integer) 0:
    %       *HeadVar__1_1 = (MR_Integer) 0;
    %       break;
    %     case (MR_Integer) 1:
    %     case (MR_Integer) 2:
    %     case (MR_Integer) 3:
    %     case (MR_Integer) 4:
    %     case (MR_Integer) 5:
    %     case (MR_Integer) 6:
    %     case (MR_Integer) 7:
    %       *HeadVar__1_1 = (MR_Integer) 1;
    %       break;
    %   }
    %
    % Note that the order in which we return the cases does not matter,
    % since our caller will sort the case list we return.
    %

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_tag_switch.
%---------------------------------------------------------------------------%
