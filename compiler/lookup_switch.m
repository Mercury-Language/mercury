%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: lookup_switch.m.
% Authors: conway, zs.
% 
% For switches on atomic types in which the cases contain only the
% construction of constants, generate code which just assigns the values of
% the output variables by indexing into an array of values for the output
% variables.
%
% For switches that can fail, the generated code does a range check on the
% index, and then does a lookup in a bit-vector to see if there is a value for
% the appropriate case. If there is, then it does a lookup (using the field
% macro) in the array of results. The array is padded with "0"s for cases that
% are not covered. This is fine, since we do the lookup after we check the
% bit-vector for the appropriate case.
%
% The current implementation works out whether or not it can do a lookup
% switch by generating code for each case and looking to see that no code got
% generated (i.e. only the code generation state got modified) and that the
% output variables of the switch are all constants. This is potentially quite
% inefficient because it does the work of generating code for the cases and
% then may throw it away if a subsequent case generates actual code, or non
% constant outputs.
%
% A potential improvement would be to make a single array for each switch,
% since putting the values produced for each tag value side-by-side in memory
% will tend to lead to fewer cache misses.
%
% The number of bits per word is taken from the bits_per_word option which
% uses a flag in the mc script with a value from configuration. This is used
% when generating bit-vectors.
% 
%-----------------------------------------------------------------------------%

:- module ll_backend.lookup_switch.
:- interface.

:- import_module backend_libs.switch_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.
:- import_module parse_tree.prog_data.

%-----------------------------------------------------------------------------%

:- type lookup_switch_info.

    % Decide whether we can generate code for this switch using a lookup table.
    % The cases_list must be sorted on the index values.
    %
:- pred is_lookup_switch(prog_var::in, cases_list::in,
    hlds_goal_info::in, can_fail::in, int::in, abs_store_map::in,
    branch_end::in, branch_end::out, code_model::in, lookup_switch_info::out,
    code_info::in, code_info::out) is semidet.

    % Generate code for the switch that the lookup_switch_info came from.
    %
:- pred generate_lookup_switch(prog_var::in, abs_store_map::in, branch_end::in,
    lookup_switch_info::in, code_tree::out, code_info::in, code_info::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_data.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.tree.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.dense_switch.
:- import_module ll_backend.global_data.
:- import_module ll_backend.lookup_util.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type case_consts
    --->    all_one_soln(
                assoc_list(int, list(rval))
            )
    ;       some_several_solns(
                assoc_list(int, soln_consts),
                set(prog_var),          % The resume vars.
                bool                    % The Boolean "or" of the result
                                        % of invoking should_emit_trail_ops
                                        % on the goal_infos of the
                                        % disjunctions.
            ).

:- type soln_consts
    --->    one_soln(list(rval))
    ;       several_solns(list(list(rval))).

:- type need_range_check
    --->    need_range_check
    ;       dont_need_range_check.

:- type need_bit_vec_check
    --->    need_bit_vec_check
    ;       dont_need_bit_vec_check.

:- type lookup_switch_info
    --->    lookup_switch_info(
                lsi_first               :: int,
                lsi_last                :: int,
                                        % The first and last values of the
                                        % switched-on rval covered by the
                                        % switch.
                lsi_cases               :: case_consts,
                                        % The map from the switched-on value
                                        % to the values of the variables
                                        % in each solution.
                lsi_variables           :: list(prog_var),
                                        % The output variables.
                lsi_field_types         :: list(llds_type),
                                        % The types of the fields in the C
                                        % structure we generate for each case.
                lsi_need_range_check    :: need_range_check,
                lsi_need_bit_vec_check  :: need_bit_vec_check,
                                        % Do we need a range check and/or a
                                        % bit vector check on the switched-on
                                        % variable?
                lsi_liveness            :: set(prog_var)
            ).

%-----------------------------------------------------------------------------%

    % Most of this predicate is taken from dense_switch.m.
    %
is_lookup_switch(CaseVar, TaggedCases0, GoalInfo, SwitchCanFail0, ReqDensity,
        StoreMap, !MaybeEnd, CodeModel, LookupSwitchInfo, !CI) :-
    % We need the code_info structure to generate code for the cases to
    % get the constants (if they exist). We can't throw it away at the
    % end because we may have allocated some new static ground terms.

    % Since lookup switches rely on static ground terms to work efficiently,
    % there is no point in using a lookup switch if static ground terms are
    % not enabled. Well, actually, it is possible that they might be a win in
    % some circumstances, but it would take a pretty complex heuristic to get
    % it right, so, lets just use a simple one - no static ground terms,
    % no lookup switch.
    code_info.get_globals(!.CI, Globals),
    globals.lookup_bool_option(Globals, static_ground_terms, yes),

    goal_info_get_code_model(GoalInfo, CodeModel),
    (
        ( CodeModel = model_non
        ; CodeModel = model_semi
        ),
        % We build up the list in reverse because that is linear and uses
        % constant stack space, whereas building it up in the right order
        % would either use linear stack space or require a quadratic algorithm.
        % This doesn't matter if TaggedCases0 is short, but does matter if it
        % contains thousands of elements.
        filter_out_failing_cases(TaggedCases0, [], RevTaggedCases,
            SwitchCanFail0, SwitchCanFail),
        list.reverse(RevTaggedCases, TaggedCases)
    ;
        CodeModel = model_det,
        TaggedCases = TaggedCases0,
        SwitchCanFail = SwitchCanFail0
    ),

    % We want to generate a lookup switch for any switch that is dense enough
    % - we don't care how many cases it has. A memory lookup tends to be
    % cheaper than a branch.
    list.length(TaggedCases, NumCases),
    TaggedCases = [FirstCase | _],
    FirstCase = extended_case(_, int_tag(FirstCaseVal), _, _),
    list.index1_det(TaggedCases, NumCases, LastCase),
    LastCase = extended_case(_, int_tag(LastCaseVal), _, _),
    Span = LastCaseVal - FirstCaseVal,
    Range = Span + 1,
    Density = switch_density(NumCases, Range),
    Density > ReqDensity,

    % If there are going to be no gaps in the lookup table then we won't need
    % a bitvector test to see if this switch has a value for this case.
    ( NumCases = Range ->
        NeedBitVecCheck0 = dont_need_bit_vec_check
    ;
        NeedBitVecCheck0 = need_bit_vec_check
    ),
    (
        SwitchCanFail = can_fail,
        % For can_fail switches, we normally need to check that the variable
        % is in range before we index into the jump table. However, if the
        % range of the type is sufficiently small, we can make the jump table
        % large enough to hold all of the values for the type, but then we
        % will need to do the bitvector test.
        Type = code_info.variable_type(!.CI, CaseVar),
        code_info.get_module_info(!.CI, ModuleInfo),
        classify_type(ModuleInfo, Type) = TypeCategory,
        (
            dense_switch.type_range(!.CI, TypeCategory, Type, TypeRange),
            DetDensity = switch_density(NumCases, TypeRange),
            DetDensity > ReqDensity
        ->
            NeedRangeCheck = dont_need_range_check,
            NeedBitVecCheck = need_bit_vec_check,
            FirstVal = 0,
            LastVal = TypeRange - 1
        ;
            NeedRangeCheck = need_range_check,
            NeedBitVecCheck = NeedBitVecCheck0,
            FirstVal = FirstCaseVal,
            LastVal = LastCaseVal
        )
    ;
        SwitchCanFail = cannot_fail,
        NeedRangeCheck = dont_need_range_check,
        NeedBitVecCheck = NeedBitVecCheck0,
        FirstVal = FirstCaseVal,
        LastVal = LastCaseVal
    ),
    figure_out_output_vars(!.CI, GoalInfo, OutVars),
    code_info.remember_position(!.CI, CurPos),
    generate_constants_for_lookup_switch(TaggedCases, OutVars, StoreMap,
        CaseSolns, !MaybeEnd, MaybeLiveness, set.init, ResumeVars,
        no, GoalTrailOps, !CI),
    code_info.reset_to_position(CurPos, !CI),
    (
        MaybeLiveness = yes(Liveness)
    ;
        MaybeLiveness = no,
        unexpected(this_file, "is_lookup_switch: no liveness!")
    ),
    VarTypes = get_var_types(!.CI),
    list.map(map.lookup(VarTypes), OutVars, OutTypes),
    ( project_all_to_one_solution(CaseSolns, [], RevCaseValuePairs) ->
        list.reverse(RevCaseValuePairs, CaseValuePairs),
        CaseConsts = all_one_soln(CaseValuePairs),
        assoc_list.values(CaseValuePairs, CaseValues)
    ;
        CaseConsts = some_several_solns(CaseSolns, ResumeVars, GoalTrailOps),
        % This generates CaseValues in reverse order of index, but given that
        % we only use CaseValues to find out the right LLDSTypes, this is OK.
        project_solns_to_rval_lists(CaseSolns, [], CaseValues)
    ),
    code_info.get_globals(!.CI, Globals),
    globals.lookup_bool_option(Globals, unboxed_float, UnboxFloat),
    find_general_llds_types(UnboxFloat, OutTypes, CaseValues, LLDSTypes),
    LookupSwitchInfo = lookup_switch_info(FirstVal, LastVal, CaseConsts,
        OutVars, LLDSTypes, NeedRangeCheck, NeedBitVecCheck, Liveness).

:- pred project_all_to_one_solution(assoc_list(int, soln_consts)::in,
    assoc_list(int, list(rval))::in, assoc_list(int, list(rval))::out)
    is semidet.

project_all_to_one_solution([], !RevCaseValuePairs).
project_all_to_one_solution([Case - Solns | CaseSolns], !RevCaseValuePairs) :-
    Solns = one_soln(Values),
    !:RevCaseValuePairs = [Case - Values | !.RevCaseValuePairs],
    project_all_to_one_solution(CaseSolns, !RevCaseValuePairs).

:- pred project_solns_to_rval_lists(assoc_list(int, soln_consts)::in,
    list(list(rval))::in, list(list(rval))::out) is det.

project_solns_to_rval_lists([], !RvalsList).
project_solns_to_rval_lists([Case | Cases], !RvalsList) :-
    Case = _Index - Soln,
    (
        Soln = one_soln(Rvals),
        !:RvalsList = [Rvals | !.RvalsList]
    ;
        Soln = several_solns(SolnRvalsList),
        !:RvalsList = SolnRvalsList ++ !.RvalsList
    ),
    project_solns_to_rval_lists(Cases, !RvalsList).

%---------------------------------------------------------------------------%

:- pred filter_out_failing_cases(cases_list::in,
    cases_list::in, cases_list::out, can_fail::in, can_fail::out) is det.

filter_out_failing_cases([], !RevTaggedCases, !SwitchCanFail).
filter_out_failing_cases([Case | Cases], !RevTaggedCases, !SwitchCanFail) :-
    Case = extended_case(_, _, _, Goal),
    Goal = hlds_goal(GoalExpr, _),
    ( GoalExpr = disj([]) ->
        !:SwitchCanFail = can_fail
    ;
        !:RevTaggedCases = [Case | !.RevTaggedCases]
    ),
    filter_out_failing_cases(Cases, !RevTaggedCases, !SwitchCanFail).

%---------------------------------------------------------------------------%

:- pred generate_constants_for_lookup_switch(cases_list::in,
    list(prog_var)::in, abs_store_map::in, assoc_list(int, soln_consts)::out,
    branch_end::in, branch_end::out, maybe(set(prog_var))::out,
    set(prog_var)::in, set(prog_var)::out, bool::in, bool::out,
    code_info::in, code_info::out) is semidet.

generate_constants_for_lookup_switch([], _Vars, _StoreMap, [], !MaybeEnd, no,
        !ResumeVars, !GoalTrailOps, !CI).
generate_constants_for_lookup_switch([Case | Cases], Vars, StoreMap,
        [CaseVal | Rest], !MaybeEnd, MaybeLiveness, !ResumeVars,
        !GoalTrailOps, !CI) :-
    Case = extended_case(_, int_tag(CaseTag), _, Goal),
    Goal = hlds_goal(GoalExpr, GoalInfo),

    % Goals with these features need special treatment in generate_goal.
    goal_info_get_features(GoalInfo, Features),
    not set.member(feature_call_table_gen, Features),
    not set.member(feature_save_deep_excp_vars, Features),

    ( GoalExpr = disj(Disjuncts) ->
        bool.or(goal_may_modify_trail(GoalInfo), !GoalTrailOps),
        (
            Disjuncts = [],
            % Cases like this should have been filtered out by
            % filter_out_failing_cases above.
            unexpected(this_file, "generate_constants: disj([])")
        ;
            Disjuncts = [FirstDisjunct | _],
            FirstDisjunct = hlds_goal(_, FirstDisjunctGoalInfo),
            goal_info_get_resume_point(FirstDisjunctGoalInfo, ThisResumePoint),
            (
                ThisResumePoint = resume_point(ThisResumeVars, _),
                set.union(ThisResumeVars, !ResumeVars)
            ;
                ThisResumePoint = no_resume_point
            )
        ),
        all_disjuncts_are_conj_of_unify(Disjuncts),

        % We execute the pre- and post-goal update for the disjunction.
        % The pre- and post-goal updates for the disjuncts themselves are
        % done as part of the call to generate_goal in
        % generate_constants_for_disjuncts in lookup_util.m.
        code_info.pre_goal_update(GoalInfo, no, !CI),
        code_info.get_instmap(!.CI, InstMap),
        generate_constants_for_disjuncts(Disjuncts, Vars, StoreMap, Solns,
            !MaybeEnd, MaybeLiveness, !CI),
        code_info.set_instmap(InstMap, !CI),
        code_info.post_goal_update(GoalInfo, !CI),
        CaseVal = CaseTag - several_solns(Solns)
    ;
        goal_is_conj_of_unify(Goal),
        % The pre- and post-goal updates for the goals themselves
        % are done as part of the call to generate_goal in
        % generate_constants_for_disjuncts in lookup_util.m.
        generate_constants_for_arm(Goal, Vars, StoreMap, Soln,
            !MaybeEnd, Liveness, !CI),
        MaybeLiveness = yes(Liveness),
        CaseVal = CaseTag - one_soln(Soln)
    ),
    generate_constants_for_lookup_switch(Cases, Vars, StoreMap, Rest,
        !MaybeEnd, _, !ResumeVars, !GoalTrailOps, !CI).

%---------------------------------------------------------------------------%

generate_lookup_switch(Var, StoreMap, MaybeEnd0, LookupSwitchInfo, Code,
        !CI) :-
    LookupSwitchInfo = lookup_switch_info(StartVal, EndVal, CaseConsts,
        OutVars, LLDSTypes, NeedRangeCheck, NeedBitVecCheck, Liveness),

    % Evaluate the variable which we are going to be switching on.
    code_info.produce_variable(Var, VarCode, Rval, !CI),

    % If the case values start at some number other than 0,
    % then subtract that number to give us a zero-based index.
    ( StartVal = 0 ->
        IndexRval = Rval
    ;
        IndexRval = binop(int_sub, Rval, const(llconst_int(StartVal)))
    ),

    % If the switch is not locally deterministic, we may need to check that
    % the value of the variable lies within the appropriate range.
    (
        NeedRangeCheck = need_range_check,
        Difference = EndVal - StartVal,
        CmpRval = binop(unsigned_le, IndexRval,
            const(llconst_int(Difference))),
        code_info.fail_if_rval_is_false(CmpRval, RangeCheckCode, !CI)
    ;
        NeedRangeCheck = dont_need_range_check,
        RangeCheckCode = empty
    ),

    (
        CaseConsts = all_one_soln(CaseValues),
        Comment = node([llds_instr(comment("simple lookup switch"), "")]),
        generate_simple_lookup_switch(IndexRval, StoreMap, MaybeEnd0,
            StartVal, EndVal, CaseValues, OutVars, LLDSTypes,
            NeedBitVecCheck, Liveness, RestCode, !CI)
    ;
        CaseConsts = some_several_solns(CaseSolns, ResumeVars, GoalTrailOps),
        get_emit_trail_ops(!.CI, EmitTrailOps),
        bool.and(EmitTrailOps, GoalTrailOps, AddTrailOps),
        Comment = node([
            llds_instr(comment("several soln lookup switch"), "")
        ]),
        generate_several_soln_lookup_switch(IndexRval, StoreMap, MaybeEnd0,
            StartVal, EndVal, CaseSolns, ResumeVars, AddTrailOps, OutVars,
            LLDSTypes, NeedBitVecCheck, Liveness, RestCode, !CI)
    ),
    Code = tree_list([Comment, VarCode, RangeCheckCode, RestCode]).

:- pred generate_simple_lookup_switch(rval::in, abs_store_map::in,
    branch_end::in, int::in, int::in, assoc_list(int, list(rval))::in,
    list(prog_var)::in, list(llds_type)::in, need_bit_vec_check::in,
    set(prog_var)::in, code_tree::out, code_info::in, code_info::out) is det.

generate_simple_lookup_switch(IndexRval, StoreMap, MaybeEnd0, StartVal, EndVal,
        CaseValues, OutVars, LLDSTypes, NeedBitVecCheck, Liveness, Code,
        !CI) :-
    (
        NeedBitVecCheck = need_bit_vec_check,
        generate_bitvec_test(IndexRval, CaseValues, StartVal, EndVal,
            CheckBitVecCode, !CI)
    ;
        NeedBitVecCheck = dont_need_bit_vec_check,
        CheckBitVecCode = empty
    ),

    % Now generate the static cells into which we do the lookups of the values
    % of the output variables, if there are any.
    %
    % Note that invoking generate_simple_terms when OutVars = [] would lead to
    % a compiler abort, since we cannot create C structures with zero fields.
    (
        OutVars = [],
        BaseRegInitCode = empty,
        MaybeBaseReg = no
    ;
        OutVars = [_ | _],
        % Since we release BaseReg only after the call to generate_branch_end,
        % we must make sure that generate_branch_end won't want to overwrite
        % BaseReg.
        code_info.acquire_reg_not_in_storemap(StoreMap, BaseReg, !CI),
        MaybeBaseReg = yes(BaseReg),
        generate_simple_terms(IndexRval, OutVars, LLDSTypes, CaseValues,
            StartVal, BaseReg, BaseRegInitCode, !CI)
    ),

    % We keep track of what variables are supposed to be live at the end
    % of cases. We have to do this explicitly because generating a `fail' slot
    % last would yield the wrong liveness.
    code_info.set_forward_live_vars(Liveness, !CI),
    code_info.generate_branch_end(StoreMap, MaybeEnd0, _MaybeEnd,
        BranchEndCode, !CI),
    (
        MaybeBaseReg = no
    ;
        MaybeBaseReg = yes(FinalBaseReg),
        code_info.release_reg(FinalBaseReg, !CI)
    ),
    Code = tree_list([CheckBitVecCode, BaseRegInitCode, BranchEndCode]).

    % Add an expression to the expression cache in the code_info structure
    % for each of the output variables of the lookup switch. This is done by
    % creating a `create' term for the array, and caching an expression
    % for the variable to get the IndexRval'th field of that term.
    %
:- pred generate_simple_terms(rval::in, list(prog_var)::in,
    list(llds_type)::in, assoc_list(int, list(rval))::in, int::in,
    lval::in, code_tree::out, code_info::in, code_info::out) is det.

generate_simple_terms(IndexRval, OutVars, OutTypes, CaseVals, Start, BaseReg,
        Code, !CI) :-
    list.length(OutVars, NumOutVars),
    construct_simple_vector(Start, OutTypes, CaseVals, VectorRvals),
    code_info.add_vector_static_cell(OutTypes, VectorRvals, VectorAddr, !CI),

    VectorAddrRval = const(llconst_data_addr(VectorAddr, no)),
    % IndexRval has already had Start subtracted from it.
    ( NumOutVars = 1 ->
        BaseRval = IndexRval
    ;
        BaseRval = binop(int_mul, IndexRval, const(llconst_int(NumOutVars)))
    ),
    Code = node([
        llds_instr(
            assign(BaseReg, mem_addr(heap_ref(VectorAddrRval, 0, BaseRval))),
            "Compute base address for this case")
    ]),
    generate_offset_assigns(OutVars, 0, BaseReg, !CI).

:- pred construct_simple_vector(int::in, list(llds_type)::in,
    assoc_list(int, list(rval))::in, list(list(rval))::out) is det.

construct_simple_vector(_, _, [], []).
construct_simple_vector(CurIndex, LLDSTypes, [Index - Rvals | Rest],
        [Row | Rows]) :-
    ( CurIndex < Index ->
        % If this argument (array element) is a place-holder and
        % will never be referenced, just fill it in with a dummy entry.
        Row = list.map(default_value_for_type, LLDSTypes),
        Remainder = [Index - Rvals | Rest]
    ;
        Row = Rvals,
        Remainder = Rest
    ),
    construct_simple_vector(CurIndex + 1, LLDSTypes, Remainder, Rows).

%-----------------------------------------------------------------------------%

:- pred generate_several_soln_lookup_switch(rval::in, abs_store_map::in,
    branch_end::in, int::in, int::in, assoc_list(int, soln_consts)::in,
    set(prog_var)::in, bool::in, list(prog_var)::in, list(llds_type)::in,
    need_bit_vec_check::in, set(prog_var)::in, code_tree::out,
    code_info::in, code_info::out) is det.

generate_several_soln_lookup_switch(IndexRval, StoreMap, MaybeEnd0,
        StartVal, EndVal, CaseSolns, ResumeVars, AddTrailOps, OutVars,
        LLDSTypes, NeedBitVecCheck, Liveness, Code, !CI) :-
    (
        OutVars = [],
        % If there are no output variables, then how can the individual
        % solutions differ from each other?
        unexpected(this_file,
            "generate_several_soln_lookup_switch: no OutVars")
    ;
        OutVars = [_ | _]
    ),

    % Now generate the static cells into which we do the lookups of the values
    % of the output variables, if there are any.
    %
    % We put a dummy row at the start.
    list.length(LLDSTypes, NumLLDSTypes),
    InitRowNumber = 1,
    DummyLaterSolnRow = list.map(default_value_for_type, LLDSTypes),
    construct_several_soln_vector(StartVal, EndVal, InitRowNumber,
        LLDSTypes, NumLLDSTypes, CaseSolns, MainRows,
        [DummyLaterSolnRow], RevLaterSolnArray,
        0, FailCaseCount, 0, OneSolnCaseCount, 0, SeveralSolnCaseCount),
    (
        (
            NeedBitVecCheck = need_bit_vec_check
        <=>
            FailCaseCount > 0
        )
    ->
        true
    ;
        unexpected(this_file,
            "generate_several_soln_lookup_switch: bad FailCaseCount")
    ),

    list.reverse(RevLaterSolnArray, LaterSolnArray),
    MainRowTypes = [integer, integer | LLDSTypes],
    list.length(MainRowTypes, MainRowWidth),
    code_info.add_vector_static_cell(MainRowTypes, MainRows, MainVectorAddr,
        !CI),
    MainVectorAddrRval = const(llconst_data_addr(MainVectorAddr, no)),
    code_info.add_vector_static_cell(LLDSTypes, LaterSolnArray,
        LaterVectorAddr, !CI),
    LaterVectorAddrRval = const(llconst_data_addr(LaterVectorAddr, no)),

    % Since we release BaseReg only after the calls to generate_branch_end,
    % we must make sure that generate_branch_end won't want to overwrite
    % BaseReg.
    %
    % We release BaseReg in each arm of generate_code_for_each_kind below.
    % We cannot release it at the bottom of this predicate, because in the
    % kind_several_solns arm of generate_code_for_each_kind the generation
    % of the resume point will clobber the set of acquired registers.
    %
    % We cannot release the stack slots anywhere, since they will be needed
    % after backtracking.
    code_info.acquire_reg_not_in_storemap(StoreMap, BaseReg, !CI),
    code_info.acquire_temp_slot(lookup_switch_cur, CurSlot, !CI),
    code_info.acquire_temp_slot(lookup_switch_max, MaxSlot, !CI),
    % IndexRval has already had Start subtracted from it.
    BaseRval = binop(int_mul, IndexRval, const(llconst_int(MainRowWidth))),
    BaseRegInitCode = node([
        llds_instr(
            assign(BaseReg,
                mem_addr(heap_ref(MainVectorAddrRval, 0, BaseRval))),
            "Compute base address for this case")
    ]),

    list.sort([FailCaseCount - kind_zero_solns,
        OneSolnCaseCount - kind_one_soln,
        SeveralSolnCaseCount - kind_several_solns], AscendingSortedKinds),
    list.reverse(AscendingSortedKinds, DescendingSortedKinds),

    code_info.get_next_label(EndLabel, !CI),
    code_info.remember_position(!.CI, BranchStart),
    generate_code_for_each_kind(DescendingSortedKinds, BaseReg,
        CurSlot, MaxSlot, LaterVectorAddrRval, EndLabel, BranchStart,
        ResumeVars, AddTrailOps, OutVars, StoreMap, MaybeEnd0, Liveness,
        KindsCode, !CI),

    code_info.set_resume_point_to_unknown(!CI),
    EndLabelCode = node([
        llds_instr(label(EndLabel), "end of several_soln lookup switch")
    ]),
    Code = tree_list([BaseRegInitCode, KindsCode, EndLabelCode]).

:- type case_kind
    --->    kind_zero_solns
    ;       kind_one_soln
    ;       kind_several_solns.

:- func case_kind_to_string(case_kind) = string.

case_kind_to_string(kind_zero_solns) = "kind_zero_solns".
case_kind_to_string(kind_one_soln) = "kind_one_soln".
case_kind_to_string(kind_several_solns) = "kind_several_solns".

:- pred generate_code_for_each_kind(assoc_list(int, case_kind)::in,
    lval::in, lval::in, lval::in, rval::in, label::in, position_info::in,
    set(prog_var)::in, bool::in, list(prog_var)::in, abs_store_map::in,
    branch_end::in, set(prog_var)::in, code_tree::out,
    code_info::in, code_info::out) is det.

generate_code_for_each_kind([], _, _, _, _, _, _, _, _, _, _, _, _, _, !CI) :-
    unexpected(this_file, "generate_code_for_each_kind: no kinds").
generate_code_for_each_kind([_ - Kind | Kinds], BaseReg, CurSlot, MaxSlot,
        LaterVectorAddrRval, EndLabel, BranchStart, ResumeVars, AddTrailOps,
        OutVars, StoreMap, MaybeEnd0, Liveness, Code, !CI) :-
    (
        Kind = kind_zero_solns,
        TestOp = int_ge,
        code_info.reset_to_position(BranchStart, !CI),
        code_info.release_reg(BaseReg, !CI),
        code_info.generate_failure(KindCode, !CI)
    ;
        Kind = kind_one_soln,
        TestOp = ne,
        code_info.reset_to_position(BranchStart, !CI),
        generate_offset_assigns(OutVars, 2, BaseReg, !CI),
        set_liveness_and_end_branch(StoreMap, MaybeEnd0, Liveness,
            BranchEndCode, !CI),
        code_info.release_reg(BaseReg, !CI),
        GotoEndCode = node([
            llds_instr(goto(code_label(EndLabel)),
                "goto end of switch from one_soln")
        ]),
        KindCode = tree_list([BranchEndCode, GotoEndCode])
    ;
        Kind = kind_several_solns,
        TestOp = int_le,
        code_info.get_globals(!.CI, Globals),
        code_info.reset_to_position(BranchStart, !CI),

        % The code below is modelled on the code in disj_gen, but is
        % specialized for the situation here.

        code_info.produce_vars(ResumeVars, ResumeMap, FlushCode, !CI),
        SaveSlotsCode = node([
            llds_instr(assign(CurSlot,
                lval(field(yes(0), lval(BaseReg), const(llconst_int(0))))),
                "Setup current slot in the later solution array"),
            llds_instr(assign(MaxSlot,
                lval(field(yes(0), lval(BaseReg), const(llconst_int(1))))),
                "Setup maximum slot in the later solution array")
        ]),
        code_info.maybe_save_ticket(AddTrailOps, SaveTicketCode,
            MaybeTicketSlot, !CI),
        globals.lookup_bool_option(Globals, reclaim_heap_on_nondet_failure,
            ReclaimHeap),
        code_info.maybe_save_hp(ReclaimHeap, SaveHpCode, MaybeHpSlot, !CI),
        code_info.prepare_for_disj_hijack(model_non, HijackInfo,
            PrepareHijackCode, !CI),

        code_info.remember_position(!.CI, DisjEntry),

        % Generate code for the non-last disjunct.

        code_info.make_resume_point(ResumeVars, resume_locs_stack_only,
            ResumeMap, ResumePoint, !CI),
        code_info.effect_resume_point(ResumePoint, model_non, UpdateRedoipCode,
            !CI),
        generate_offset_assigns(OutVars, 2, BaseReg, !CI),
        code_info.flush_resume_vars_to_stack(FirstFlushResumeVarsCode, !CI),

        % Forget the variables that are needed only at the resumption point at
        % the start of the next disjunct, so that we don't generate exceptions
        % when their storage is clobbered by the movement of the live variables
        % to the places indicated in the store map.
        code_info.pop_resume_point(!CI),
        code_info.pickup_zombies(FirstZombies, !CI),
        code_info.make_vars_forward_dead(FirstZombies, !CI),

        set_liveness_and_end_branch(StoreMap, MaybeEnd0, Liveness,
            FirstBranchEndCode, !CI),
        code_info.release_reg(BaseReg, !CI),

        GotoEndCode = node([
            llds_instr(goto(code_label(EndLabel)),
                "goto end of switch from several_soln")
        ]),

        code_info.reset_to_position(DisjEntry, !CI),
        code_info.generate_resume_point(ResumePoint, ResumePointCode, !CI),

        code_info.maybe_reset_ticket(MaybeTicketSlot, reset_reason_undo,
            RestoreTicketCode),
        code_info.maybe_restore_hp(MaybeHpSlot, RestoreHpCode),

        code_info.acquire_reg_not_in_storemap(StoreMap, LaterBaseReg, !CI),
        code_info.get_next_label(UndoLabel, !CI),
        code_info.get_next_label(AfterUndoLabel, !CI),
        list.length(OutVars, NumOutVars),
        TestMoreSolnsCode = node([
            llds_instr(assign(LaterBaseReg, lval(CurSlot)),
                "Init later base register"),
            llds_instr(if_val(binop(int_ge, lval(LaterBaseReg), lval(MaxSlot)),
                code_label(UndoLabel)),
                "Jump to undo hijack code if there are no more solutions"),
            llds_instr(assign(CurSlot,
                binop(int_add, lval(CurSlot), const(llconst_int(NumOutVars)))),
                "Update current slot in the later solution array"),
            llds_instr(goto(code_label(AfterUndoLabel)),
                "Jump around undo hijack code"),
            llds_instr(label(UndoLabel),
                "Undo hijack code")
        ]),
        code_info.undo_disj_hijack(HijackInfo, UndoHijackCode, !CI),
        AfterUndoLabelCode = node([
            llds_instr(label(AfterUndoLabel),
                "Return later answer code"),
            llds_instr(assign(LaterBaseReg,
                mem_addr(heap_ref(LaterVectorAddrRval, 0,
                    lval(LaterBaseReg)))),
                "Compute base address in later array for this solution")
        ]),

        % We need to call effect_resume_point in order to push ResumePoint
        % onto the failure continuation stack, so pop_resume_point can pop
        % it off. However, since the redoip already points there, we don't need
        % to execute _LaterUpdateRedoipCode.
        code_info.effect_resume_point(ResumePoint, model_non,
            _LaterUpdateRedoipCode, !CI),

        generate_offset_assigns(OutVars, 0, LaterBaseReg, !CI),
        code_info.flush_resume_vars_to_stack(LaterFlushResumeVarsCode, !CI),

        % Forget the variables that are needed only at the resumption point at
        % the start of the next disjunct, so that we don't generate exceptions
        % when their storage is clobbered by the movement of the live variables
        % to the places indicated in the store map.
        code_info.pop_resume_point(!CI),
        code_info.pickup_zombies(LaterZombies, !CI),
        code_info.make_vars_forward_dead(LaterZombies, !CI),

        set_liveness_and_end_branch(StoreMap, MaybeEnd0, Liveness,
            LaterBranchEndCode, !CI),

        KindCode = tree_list([FlushCode, SaveSlotsCode,
            SaveTicketCode, SaveHpCode, PrepareHijackCode,
            UpdateRedoipCode, FirstFlushResumeVarsCode, FirstBranchEndCode,
            GotoEndCode, ResumePointCode, RestoreTicketCode, RestoreHpCode,
            TestMoreSolnsCode, UndoHijackCode, AfterUndoLabelCode,
            LaterFlushResumeVarsCode, LaterBranchEndCode, GotoEndCode])
    ),
    (
        Kinds = [],
        Code = KindCode
    ;
        Kinds = [_ - NextKind | _],
        code_info.get_next_label(NextKindLabel, !CI),
        TestRval = binop(TestOp,
            lval(field(yes(0), lval(BaseReg), const(llconst_int(0)))),
            const(llconst_int(0))),
        TestCode = node([
            llds_instr(if_val(TestRval, code_label(NextKindLabel)),
                "skip to next kind in several_soln lookup switch"),
            llds_instr(comment("This kind is " ++ case_kind_to_string(Kind)),
                "")
        ]),
        generate_code_for_each_kind(Kinds, BaseReg, CurSlot, MaxSlot,
            LaterVectorAddrRval, EndLabel, BranchStart, ResumeVars,
            AddTrailOps, OutVars, StoreMap, MaybeEnd0, Liveness,
            LaterKindsCode, !CI),
        NextKindLabelCode = node([
            llds_instr(label(NextKindLabel),
                "next kind in several_soln lookup switch"),
            llds_instr(comment("Next kind is "
                ++ case_kind_to_string(NextKind)),
                "")
        ]),
        Code = tree_list([TestCode, KindCode, NextKindLabelCode,
            LaterKindsCode])
    ).

    % Note that we specify --optimise-constructor-last-call for this module
    % in order to make this predicate tail recursive.
    %
:- pred construct_several_soln_vector(int::in, int::in, int::in,
    list(llds_type)::in, int::in, assoc_list(int, soln_consts)::in,
    list(list(rval))::out,
    list(list(rval))::in, list(list(rval))::out,
    int::in, int::out, int::in, int::out, int::in, int::out) is det.

construct_several_soln_vector(CurIndex, EndVal, !.LaterNextRow, LLDSTypes,
        NumLLDSTypes, [], MainRows, !RevLaterSolnArray,
        !FailCaseCount, !OneSolnCaseCount, !SeveralSolnCaseCount) :-
    ( CurIndex > EndVal ->
        MainRows = []
    ;
        construct_fail_row(LLDSTypes, MainRow, !FailCaseCount),
        construct_several_soln_vector(CurIndex + 1, EndVal, !.LaterNextRow,
            LLDSTypes, NumLLDSTypes, [], MoreMainRows, !RevLaterSolnArray,
            !FailCaseCount, !OneSolnCaseCount, !SeveralSolnCaseCount),
        MainRows = [MainRow | MoreMainRows]
    ).
construct_several_soln_vector(CurIndex, EndVal, !.LaterNextRow, LLDSTypes,
        NumLLDSTypes, [Index - Soln | Rest], [MainRow | MainRows],
        !RevLaterSolnArray,
        !FailCaseCount, !OneSolnCaseCount, !SeveralSolnCaseCount) :-
    ( CurIndex < Index ->
        construct_fail_row(LLDSTypes, MainRow, !FailCaseCount),
        Remainder = [Index - Soln | Rest]
    ;
        (
            Soln = one_soln(Rvals),
            !:OneSolnCaseCount = !.OneSolnCaseCount + 1,
            % The first 0 means there is exactly one solution for this case;
            % the second 0 is a dummy that won't be referenced.
            ControlRvals = [const(llconst_int(0)), const(llconst_int(0))],
            MainRow = ControlRvals ++ Rvals
        ;
            Soln = several_solns([]),
            unexpected(this_file, "construct_several_soln_vector: several = 0")
        ;
            Soln = several_solns([FirstSoln | LaterSolns]),
            !:SeveralSolnCaseCount = !.SeveralSolnCaseCount + 1,
            list.length(LaterSolns, NumLaterSolns),
            FirstRowOffset = !.LaterNextRow * NumLLDSTypes,
            LastRowOffset = (!.LaterNextRow + NumLaterSolns - 1)
                * NumLLDSTypes,
            ControlRvals = [const(llconst_int(FirstRowOffset)),
                const(llconst_int(LastRowOffset))],
            MainRow = ControlRvals ++ FirstSoln,
            list.reverse(LaterSolns, RevLaterSolns),
            !:RevLaterSolnArray = RevLaterSolns ++ !.RevLaterSolnArray,
            !:LaterNextRow = !.LaterNextRow + NumLaterSolns
        ),
        Remainder = Rest
    ),
    construct_several_soln_vector(CurIndex + 1, EndVal, !.LaterNextRow,
        LLDSTypes, NumLLDSTypes, Remainder, MainRows, !RevLaterSolnArray,
        !FailCaseCount, !OneSolnCaseCount, !SeveralSolnCaseCount).

:- pred construct_fail_row(list(llds_type)::in, list(rval)::out,
    int::in, int::out) is det.

construct_fail_row(LLDSTypes, MainRow, !FailCaseCount) :-
    % The -1 means no solutions for this case; the 0 is a dummy that
    % won't be referenced.
    ControlRvals = [const(llconst_int(-1)), const(llconst_int(0))],

    % Since this argument (array element) is a place-holder and will never be
    % referenced, just fill it in with a dummy entry.
    VarRvals = list.map(default_value_for_type, LLDSTypes),

    MainRow = ControlRvals ++ VarRvals,
    !:FailCaseCount = !.FailCaseCount + 1.

%-----------------------------------------------------------------------------%

    % The bitvector is an array of words (where we use the first 32 bits
    % of each word). Each bit represents a tag value for the (range checked)
    % input to the lookup switch. The bit is `1' iff we have a case for that
    % tag value.
    %
:- pred generate_bitvec_test(rval::in, assoc_list(int, T)::in,
    int::in, int::in, code_tree::out, code_info::in, code_info::out) is det.

generate_bitvec_test(IndexRval, CaseVals, Start, _End, CheckCode, !CI) :-
    get_word_bits(!.CI, WordBits, Log2WordBits),
    generate_bit_vec(CaseVals, Start, WordBits, BitVecArgs, BitVecRval, !CI),

    % Optimize the single-word case: if all the cases fit into a single word,
    % then the word to use is always that word, and the index specifies which
    % bit. Otherwise, the high bits of the index specify which word to use
    % and the low bits specify which bit.
    ( BitVecArgs = [SingleWord] ->
        Word = SingleWord,
        BitNum = IndexRval
    ;
        % This is the same as
        % WordNum = binop(int_div, IndexRval, const(llconst_int(WordBits)))
        % except that it can generate more efficient code.
        WordNum = binop(unchecked_right_shift, IndexRval,
            const(llconst_int(Log2WordBits))),

        Word = lval(field(yes(0), BitVecRval, WordNum)),

        % This is the same as
        % BitNum = binop(int_mod, IndexRval, const(llconst_int(WordBits)))
        % except that it can generate more efficient code.
        BitNum = binop(bitwise_and, IndexRval,
            const(llconst_int(WordBits - 1)))
    ),
    HasBit = binop(bitwise_and,
        binop(unchecked_left_shift, const(llconst_int(1)), BitNum), Word),
    code_info.fail_if_rval_is_false(HasBit, CheckCode, !CI).

    % Prevent cross-compilation errors by making sure that the bitvector
    % uses a number of bits that will fit both on this machine (so that
    % we can correctly generate it), and on the target machine (so that
    % it can be executed correctly). Also make sure that the number of bits
    % that we use is a power of 2, so that we implement division as
    % right-shift (see above).
    %
:- pred get_word_bits(code_info::in, int::out, int::out) is det.

get_word_bits(CI, WordBits, Log2WordBits) :-
    int.bits_per_int(HostWordBits),
    code_info.get_globals(CI, Globals),
    globals.lookup_int_option(Globals, bits_per_word, TargetWordBits),
    int.min(HostWordBits, TargetWordBits, WordBits0),
    % round down to the nearest power of 2
    Log2WordBits = log2_rounded_down(WordBits0),
    int.pow(2, Log2WordBits, WordBits).

:- func log2_rounded_down(int) = int.

log2_rounded_down(X) = Log :-
    int.log2(X + 1, Log + 1).  % int.log2 rounds up

    % We generate the bitvector by iterating through the cases marking the bit
    % for each case. (We represent the bitvector here as a map from the word
    % number in the vector to the bits for that word.
    %
:- pred generate_bit_vec(assoc_list(int, T)::in, int::in, int::in,
    list(rval)::out, rval::out, code_info::in, code_info::out) is det.

generate_bit_vec(CaseVals, Start, WordBits, Args, BitVec, !CI) :-
    map.init(Empty),
    generate_bit_vec_2(CaseVals, Start, WordBits, Empty, BitMap),
    map.to_assoc_list(BitMap, WordVals),
    generate_bit_vec_args(WordVals, 0, Args),
    add_scalar_static_cell_natural_types(Args, DataAddr, !CI),
    BitVec = const(llconst_data_addr(DataAddr, no)).

:- pred generate_bit_vec_2(assoc_list(int, T)::in, int::in, int::in,
    map(int, int)::in, map(int, int)::out) is det.

generate_bit_vec_2([], _, _, Bits, Bits).
generate_bit_vec_2([Tag - _ | Rest], Start, WordBits, Bits0, Bits) :-
    Val = Tag - Start,
    Word = Val // WordBits,
    Offset = Val mod WordBits,
    ( map.search(Bits0, Word, X0) ->
        X1 = X0 \/ (1 << Offset)
    ;
        X1 = (1 << Offset)
    ),
    map.set(Bits0, Word, X1, Bits1),
    generate_bit_vec_2(Rest, Start, WordBits, Bits1, Bits).

:- pred generate_bit_vec_args(list(pair(int))::in, int::in,
    list(rval)::out) is det.

generate_bit_vec_args([], _, []).
generate_bit_vec_args([Word - Bits | Rest], Count, [Rval | Rvals]) :-
    ( Count < Word ->
        WordVal = 0,
        Remainder = [Word - Bits | Rest]
    ;
        WordVal = Bits,
        Remainder = Rest
    ),
    Rval = const(llconst_int(WordVal)),
    Count1 = Count + 1,
    generate_bit_vec_args(Remainder, Count1, Rvals).

%-----------------------------------------------------------------------------%

:- func default_value_for_type(llds_type) = rval.

default_value_for_type(bool) = const(llconst_int(0)).
default_value_for_type(int_least8) = const(llconst_int(0)).
default_value_for_type(uint_least8) = const(llconst_int(0)).
default_value_for_type(int_least16) = const(llconst_int(0)).
default_value_for_type(uint_least16) = const(llconst_int(0)).
default_value_for_type(int_least32) = const(llconst_int(0)).
default_value_for_type(uint_least32) = const(llconst_int(0)).
default_value_for_type(integer) = const(llconst_int(0)).
default_value_for_type(unsigned) = const(llconst_int(0)).
default_value_for_type(float) = const(llconst_float(0.0)).
default_value_for_type(string) = const(llconst_string("")).
default_value_for_type(data_ptr) = const(llconst_int(0)).
default_value_for_type(code_ptr) = const(llconst_int(0)).
default_value_for_type(word) = const(llconst_int(0)).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "lookup_switch.m".

%-----------------------------------------------------------------------------%
:- end_module lookup_switch.
%-----------------------------------------------------------------------------%
