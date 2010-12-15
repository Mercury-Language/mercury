%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2010 The University of Melbourne.
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
% the appropriate case. If there is, then it does a lookup (using the MR_field
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
% The number of bits per word is taken from the bits_per_word option which
% uses a flag in the mmc script with a value from configuration. This is used
% when generating bit-vectors.
%
% The implementation of lookup switches is further documented in the paper
% Dealing with large predicates: exo-compilation in the WAM and in Mercury,
% by Bart Demoen, Phuong-Lan Nguyen, Vi'tor Santos Costa and Zoltan Somogyi.
%
% The module ml_lookup_switch.m implements lookup switches for the MLDS
% backend. Any changes here may need to be reflected there as well.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.lookup_switch.
:- interface.

:- import_module backend_libs.switch_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.
:- import_module set.

%-----------------------------------------------------------------------------%

:- type lookup_switch_info(Key)
    --->    lookup_switch_info(
                % The map from the switched-on value to the values of the
                % variables in each solution.
                lsi_cases               :: case_consts(Key, rval),

                % The output variables.
                lsi_variables           :: list(prog_var),

                % The types of the fields in the C structure we generate
                % for each case.
                lsi_field_types         :: list(llds_type),

                lsi_liveness            :: set(prog_var)
            ).

:- pred record_lookup_for_tagged_cons_id_int(soln_consts(rval)::in,
    tagged_cons_id::in,
    map(int, soln_consts(rval))::in, map(int, soln_consts(rval))::out) is det.

:- pred record_lookup_for_tagged_cons_id_string(soln_consts(rval)::in,
    tagged_cons_id::in,
    map(string, soln_consts(rval))::in, map(string, soln_consts(rval))::out)
    is det.

:- type record_switch_lookup(Key) ==
    pred(soln_consts(rval), tagged_cons_id,
    map(Key, soln_consts(rval)), map(Key, soln_consts(rval))).
:- inst record_switch_lookup ==
    (pred(in, in, in, out) is det).

    % Decide whether we can generate code for this switch using a lookup table.
    %
:- pred is_lookup_switch(record_switch_lookup(Key)::in(record_switch_lookup),
    list(tagged_case)::in, hlds_goal_info::in, abs_store_map::in,
    branch_end::in, branch_end::out, lookup_switch_info(Key)::out,
    code_info::in, code_info::out) is semidet.

    % Generate code for the switch that the lookup_switch_info came from.
    %
:- pred generate_int_lookup_switch(rval::in, lookup_switch_info(int)::in,
    label::in, abs_store_map::in, int::in, int::in,
    need_bit_vec_check::in, need_range_check::in,
    branch_end::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out) is det.

:- type case_kind
    --->    kind_zero_solns
    ;       kind_one_soln
    ;       kind_several_solns.

    % generate_code_for_all_kinds(Kinds, NumPrevColumns, OutVars, ResumeVars,
    %   EndLabel, StoreMap, Liveness, AddTrailOps,
    %   BaseReg, LaterVectorAddrRval, Code, !CI):
    %
    % Generate code for the kinds of solution cardinalities listed in Kinds.
    %
    % - For kind_zero_solns, generate code that performs failure.
    % - For kind_one_solution, generate code that looks up the main table
    %   at row BaseReg and then goes to EndLabel.
    % - For kind_several_solns, generate code that looks up the main table
    %   at row BaseReg, sets up a resume point that stores ResumeVars,
    %   succeeds by going to EndLabel. On backtracking, the generated code
    %   will keep returning rows from the later solution table until there are
    %   no more later solutions associated with row BaseReg.
    %
    % The definition of EndLabel is up to the caller.
    %
    % For this predicate, the main table's columns form three groups.
    %
    % - The first group of NumPrevColumns columns are ignored by this
    %   predicate.
    %   - For int switches, there will be no previous columns.
    %   - For binary string switches, there is one containing the string.
    %   - For hash string switches, there are two, containing the string,
    %     and the number of the next slot in the open addressing sequence.
    %
    % - The second group contains two columns, which contain respectively
    %   the offsets of the first and last later solutions in the later
    %   solutions table. Each of these offsets is the offset of the first
    %   column of the selected row. An offset of zero indicates there is
    %   no later solution, which means that the first row of the later solution
    %   table can never be referred to, and must therefore be a dummy.
    %
    % - The third group consists of the values of OutVars.
    %
    % Therefore each row of the main table has NumPrevColumns + 2 + |OutVars|
    % columns, while the later solutions table has |OutVars| columns.
    %
    % This predicate assumes that the caller has already set BaseReg to point
    % to the start of the relevant row in the main solutions table.
    % LaterVectorAddrRval should be the address of the start of the later
    % solutions table.
    %
:- pred generate_code_for_all_kinds(list(case_kind)::in, int::in,
    list(prog_var)::in, set(prog_var)::in, label::in, abs_store_map::in,
    set(prog_var)::in, add_trail_ops::in, lval::in, rval::in,
    branch_end::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out) is det.

:- func default_value_for_type(llds_type) = rval.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.goal_form.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.dense_switch.
:- import_module ll_backend.global_data.
:- import_module ll_backend.lookup_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module svmap.

%-----------------------------------------------------------------------------%

is_lookup_switch(RecordLookupForTaggedConsId, TaggedCases, GoalInfo, StoreMap,
        !MaybeEnd, LookupSwitchInfo, !CI) :-
    % Most of this predicate is taken from dense_switch.m.

    % We need the code_info structure to generate code for the cases to
    % get the constants (if they exist). We can't throw it away at the
    % end because we may have allocated some new static ground terms.

    figure_out_output_vars(!.CI, GoalInfo, OutVars),
    set.list_to_set(OutVars, ArmNonLocals),
    remember_position(!.CI, CurPos),
    generate_constants_for_lookup_switch(RecordLookupForTaggedConsId,
        TaggedCases, OutVars, ArmNonLocals, StoreMap, Liveness,
        map.init, CaseSolnMap, !MaybeEnd, set.init, ResumeVars,
        no, GoalsMayModifyTrail, !CI),
    map.to_assoc_list(CaseSolnMap, CaseSolns),
    reset_to_position(CurPos, !CI),
    VarTypes = get_var_types(!.CI),
    list.map(map.lookup(VarTypes), OutVars, OutTypes),
    ( project_all_to_one_solution(CaseSolns, CaseValuePairs) ->
        CaseConsts = all_one_soln(CaseValuePairs),
        assoc_list.values(CaseValuePairs, CaseValues)
    ;
        CaseConsts = some_several_solns(CaseSolns, ResumeVars,
            GoalsMayModifyTrail),
        % This generates CaseValues in reverse order of index, but given that
        % we only use CaseValues to find out the right OutLLDSTypes,
        % this is OK.
        project_solns_to_rval_lists(CaseSolns, [], CaseValues)
    ),
    get_exprn_opts(!.CI, ExprnOpts),
    UnboxFloats = get_unboxed_floats(ExprnOpts),
    find_general_llds_types(UnboxFloats, OutTypes, CaseValues, OutLLDSTypes),
    LookupSwitchInfo = lookup_switch_info(CaseConsts, OutVars, OutLLDSTypes,
        Liveness).

%---------------------------------------------------------------------------%

:- pred generate_constants_for_lookup_switch(
    record_switch_lookup(Key)::in(record_switch_lookup),
    list(tagged_case)::in, list(prog_var)::in, set(prog_var)::in,
    abs_store_map::in, set(prog_var)::out,
    map(Key, soln_consts(rval))::in, map(Key, soln_consts(rval))::out,
    branch_end::in, branch_end::out, set(prog_var)::in, set(prog_var)::out,
    bool::in, bool::out, code_info::in, code_info::out) is semidet.

generate_constants_for_lookup_switch(_RecordLookupForTaggedConsId,
        [], _Vars, _ArmNonLocals, _StoreMap, set.init, !IndexMap,
        !MaybeEnd, !ResumeVars, !GoalsMayModifyTrail, !CI).
generate_constants_for_lookup_switch(RecordLookupForTaggedConsId,
        [TaggedCase | TaggedCases], Vars, ArmNonLocals, StoreMap, Liveness,
        !IndexMap, !MaybeEnd, !ResumeVars, !GoalsMayModifyTrail, !CI) :-
    TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds, _, Goal),
    Goal = hlds_goal(GoalExpr, GoalInfo),

    % Goals with these features need special treatment in generate_goal.
    Features = goal_info_get_features(GoalInfo),
    not set.member(feature_call_table_gen, Features),
    not set.member(feature_save_deep_excp_vars, Features),

    ( GoalExpr = disj(Disjuncts) ->
        (
            Disjuncts = [],
            % Cases like this should have been filtered out by
            % filter_out_failing_cases.
            unexpected(this_file, "generate_constants: disj([])")
        ;
            Disjuncts = [FirstDisjunct | LaterDisjuncts],
            goal_is_conj_of_unify(ArmNonLocals, FirstDisjunct),
            all_disjuncts_are_conj_of_unify(ArmNonLocals, LaterDisjuncts),

            bool.or(goal_may_modify_trail(GoalInfo), !GoalsMayModifyTrail),

            FirstDisjunct = hlds_goal(_, FirstDisjunctGoalInfo),
            goal_info_get_resume_point(FirstDisjunctGoalInfo, ThisResumePoint),
            (
                ThisResumePoint = resume_point(ThisResumeVars, _),
                set.union(ThisResumeVars, !ResumeVars)
            ;
                ThisResumePoint = no_resume_point
            ),

            % We execute the pre- and post-goal update for the disjunction.
            % The pre- and post-goal updates for the disjuncts themselves are
            % done as part of the call to generate_goal in
            % generate_constants_for_disjuncts in lookup_util.m.
            pre_goal_update(GoalInfo, has_subgoals, !CI),
            get_instmap(!.CI, InstMap),
            generate_constants_for_disjunct(FirstDisjunct, Vars, StoreMap,
                FirstSoln, !MaybeEnd, Liveness, !CI),
            generate_constants_for_disjuncts(LaterDisjuncts, Vars, StoreMap,
                LaterSolns, !MaybeEnd, !CI),
            set_instmap(InstMap, !CI),
            post_goal_update(GoalInfo, !CI),
            SolnConsts = several_solns(FirstSoln, LaterSolns)
        )
    ;
        goal_is_conj_of_unify(ArmNonLocals, Goal),
        % The pre- and post-goal updates for the goals themselves
        % are done as part of the call to generate_goal in
        % generate_constants_for_disjuncts in lookup_util.m.
        generate_constants_for_arm(Goal, Vars, StoreMap, Soln,
            !MaybeEnd, Liveness, !CI),
        SolnConsts = one_soln(Soln)
    ),
    RecordLookupForTaggedConsId(SolnConsts, TaggedMainConsId, !IndexMap),
    record_lookup_for_tagged_cons_ids(RecordLookupForTaggedConsId, SolnConsts,
        TaggedOtherConsIds, !IndexMap),
    generate_constants_for_lookup_switch(RecordLookupForTaggedConsId,
        TaggedCases, Vars, ArmNonLocals, StoreMap, _LivenessRest,
        !IndexMap, !MaybeEnd, !ResumeVars, !GoalsMayModifyTrail, !CI).

:- pred record_lookup_for_tagged_cons_ids(
    record_switch_lookup(Key)::in(record_switch_lookup),
    soln_consts(rval)::in, list(tagged_cons_id)::in,
    map(Key, soln_consts(rval))::in, map(Key, soln_consts(rval))::out) is det.

record_lookup_for_tagged_cons_ids(_RecordLookupForTaggedConsId, _SolnConsts,
        [], !IndexMap).
record_lookup_for_tagged_cons_ids(RecordLookupForTaggedConsId, SolnConsts,
        [TaggedConsId | TaggedConsIds], !IndexMap) :-
    RecordLookupForTaggedConsId(SolnConsts, TaggedConsId, !IndexMap),
    record_lookup_for_tagged_cons_ids(RecordLookupForTaggedConsId, SolnConsts,
        TaggedConsIds, !IndexMap).

record_lookup_for_tagged_cons_id_int(SolnConsts, TaggedConsId, !IndexMap) :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    ( ConsTag = int_tag(Index) ->
        svmap.det_insert(Index, SolnConsts, !IndexMap)
    ;
        unexpected(this_file, "record_lookup_for_tagged_cons_id: not int_tag")
    ).

record_lookup_for_tagged_cons_id_string(SolnConsts, TaggedConsId, !IndexMap) :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    ( ConsTag = string_tag(Index) ->
        svmap.det_insert(Index, SolnConsts, !IndexMap)
    ;
        unexpected(this_file, "record_lookup_for_tagged_cons_id: not int_tag")
    ).

%---------------------------------------------------------------------------%

generate_int_lookup_switch(VarRval, LookupSwitchInfo, EndLabel, StoreMap,
        StartVal, EndVal, NeedBitVecCheck, NeedRangeCheck, !MaybeEnd,
        Code, !CI) :-
    LookupSwitchInfo = lookup_switch_info(CaseConsts, OutVars, OutTypes,
        Liveness),

    % If the case values start at some number other than 0,
    % then subtract that number to give us a zero-based index.
    ( StartVal = 0 ->
        IndexRval = VarRval
    ;
        IndexRval = binop(int_sub, VarRval, const(llconst_int(StartVal)))
    ),

    % If the switch is not locally deterministic, we may need to check that
    % the value of the variable lies within the appropriate range.
    (
        NeedRangeCheck = need_range_check,
        Difference = EndVal - StartVal,
        CmpRval = binop(unsigned_le, IndexRval,
            const(llconst_int(Difference))),
        fail_if_rval_is_false(CmpRval, RangeCheckCode, !CI)
    ;
        NeedRangeCheck = dont_need_range_check,
        RangeCheckCode = empty
    ),

    (
        CaseConsts = all_one_soln(CaseValues),
        Comment = singleton(
            llds_instr(comment("simple lookup switch"), "")
        ),
        generate_simple_int_lookup_switch(IndexRval, StoreMap,
            StartVal, EndVal, CaseValues, OutVars, OutTypes,
            NeedBitVecCheck, Liveness, RestCode, !CI)
    ;
        CaseConsts = some_several_solns(CaseSolns, ResumeVars,
            GoalsMayModifyTrail),
        (
            GoalsMayModifyTrail = yes,
            get_emit_trail_ops(!.CI, EmitTrailOps),
            AddTrailOps = EmitTrailOps
        ;
            GoalsMayModifyTrail = no,
            AddTrailOps = do_not_add_trail_ops
        ),
        Comment = singleton(
            llds_instr(comment("several soln lookup switch"), "")
        ),
        generate_several_soln_int_lookup_switch(IndexRval, EndLabel, StoreMap,
            StartVal, EndVal, CaseSolns, ResumeVars, AddTrailOps, OutVars,
            OutTypes, NeedBitVecCheck, Liveness, !MaybeEnd, RestCode, !CI)
    ),
    Code = Comment ++ RangeCheckCode ++ RestCode.

:- pred generate_simple_int_lookup_switch(rval::in, abs_store_map::in,
    int::in, int::in, assoc_list(int, list(rval))::in,
    list(prog_var)::in, list(llds_type)::in, need_bit_vec_check::in,
    set(prog_var)::in, llds_code::out, code_info::in, code_info::out) is det.

generate_simple_int_lookup_switch(IndexRval, StoreMap, StartVal, EndVal,
        CaseValues, OutVars, OutTypes, NeedBitVecCheck, Liveness, Code, !CI) :-
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
    % This can happen for semidet switches.
    (
        OutVars = [],
        BaseRegInitCode = empty,
        MaybeBaseReg = no
    ;
        OutVars = [_ | _],
        % Since we release BaseReg only after the call to generate_branch_end,
        % we must make sure that generate_branch_end won't want to overwrite
        % BaseReg.
        acquire_reg_not_in_storemap(StoreMap, BaseReg, !CI),
        MaybeBaseReg = yes(BaseReg),

        % Generate the static lookup table for this switch.
        list.length(OutVars, NumOutVars),
        construct_simple_int_lookup_vector(CaseValues, StartVal, OutTypes,
            [], RevVectorRvals),
        list.reverse(RevVectorRvals, VectorRvals),
        add_vector_static_cell(OutTypes, VectorRvals, VectorAddr, !CI),
        VectorAddrRval = const(llconst_data_addr(VectorAddr, no)),

        % Generate code to look up each of the variables in OutVars
        % in its slot in the table row IndexRval (which will be row
        % VarRval - StartVal). Most of the change is done by
        % generate_offset_assigns associating each var with the relevant field
        % in !CI.
        ( NumOutVars = 1 ->
            BaseRval = IndexRval
        ;
            BaseRval = binop(int_mul,
                IndexRval, const(llconst_int(NumOutVars)))
        ),
        BaseRegInitCode = singleton(
            llds_instr(
                assign(BaseReg,
                    mem_addr(heap_ref(VectorAddrRval, 0, BaseRval))),
                "Compute base address for this case")
        ),
        generate_offset_assigns(OutVars, 0, BaseReg, !CI)
    ),

    % We keep track of what variables are supposed to be live at the end
    % of cases. We have to do this explicitly because generating a `fail' slot
    % last would yield the wrong liveness.
    set_liveness_and_end_branch(StoreMap, Liveness, no, _MaybeEnd,
        BranchEndCode, !CI),
    (
        MaybeBaseReg = no
    ;
        MaybeBaseReg = yes(FinalBaseReg),
        release_reg(FinalBaseReg, !CI)
    ),
    Code = CheckBitVecCode ++ BaseRegInitCode ++ BranchEndCode.

%-----------------------------------------------------------------------------%

:- pred construct_simple_int_lookup_vector(assoc_list(int, list(rval))::in,
    int::in, list(llds_type)::in,
    list(list(rval))::in, list(list(rval))::out) is det.

construct_simple_int_lookup_vector([], _, _, !RevRows).
construct_simple_int_lookup_vector([Index - Rvals | Rest], CurIndex, OutTypes,
        !RevRows) :-
    ( CurIndex < Index ->
        % If this argument (array element) is a place-holder and
        % will never be referenced, just fill it in with a dummy entry.
        Row = list.map(default_value_for_type, OutTypes),
        Remainder = [Index - Rvals | Rest]
    ;
        Row = Rvals,
        Remainder = Rest
    ),
    !:RevRows = [Row | !.RevRows],
    construct_simple_int_lookup_vector( Remainder, CurIndex + 1, OutTypes,
        !RevRows).

%-----------------------------------------------------------------------------%

:- pred generate_several_soln_int_lookup_switch(rval::in, label::in,
    abs_store_map::in, int::in, int::in,
    assoc_list(int, soln_consts(rval))::in, set(prog_var)::in,
    add_trail_ops::in, list(prog_var)::in, list(llds_type)::in,
    need_bit_vec_check::in, set(prog_var)::in,
    branch_end::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out) is det.

generate_several_soln_int_lookup_switch(IndexRval, EndLabel, StoreMap,
        StartVal, EndVal, CaseSolns, ResumeVars, AddTrailOps,
        OutVars, OutTypes, NeedBitVecCheck, Liveness, !MaybeEnd, Code, !CI) :-
    % If there are no output variables, then how can the individual solutions
    % differ from each other?
    expect(negate(unify(OutVars, [])), this_file,
        "generate_several_soln_int_lookup_switch: no OutVars"),

    % Now generate the static cells into which we do the lookups of the values
    % of the output variables, if there are any.
    %
    % We put a dummy row at the start of the later solns table, so that
    % a zero in the "later solns start row" column of the main table can mean
    % "no later solutions".
    list.length(OutTypes, NumOutTypes),
    InitLaterSolnRowNumber = 1,
    DummyLaterSolnRow = list.map(default_value_for_type, OutTypes),
    LaterSolnArrayCord0 = singleton(DummyLaterSolnRow),
    construct_several_soln_int_lookup_vector(StartVal, EndVal,
        OutTypes, NumOutTypes, CaseSolns, MainRows,
        InitLaterSolnRowNumber, LaterSolnArrayCord0, LaterSolnArrayCord,
        0, FailCaseCount, 0, OneSolnCaseCount, 0, SeveralSolnCaseCount),
    LaterSolnArray = cord.list(LaterSolnArrayCord),
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
            "generate_several_soln_int_lookup_switch: bad FailCaseCount")
    ),

    MainRowTypes = [lt_integer, lt_integer | OutTypes],
    list.length(MainRowTypes, MainNumColumns),
    add_vector_static_cell(MainRowTypes, MainRows, MainVectorAddr, !CI),
    MainVectorAddrRval = const(llconst_data_addr(MainVectorAddr, no)),
    add_vector_static_cell(OutTypes, LaterSolnArray, LaterVectorAddr, !CI),
    LaterVectorAddrRval = const(llconst_data_addr(LaterVectorAddr, no)),

    list.sort([FailCaseCount - kind_zero_solns,
        OneSolnCaseCount - kind_one_soln,
        SeveralSolnCaseCount - kind_several_solns], AscendingSortedCountKinds),
    list.reverse(AscendingSortedCountKinds, DescendingSortedCountKinds),
    assoc_list.values(DescendingSortedCountKinds, DescendingSortedKinds),

    % Since we release BaseReg only after the calls to generate_branch_end,
    % we must make sure that generate_branch_end won't want to overwrite
    % BaseReg.
    acquire_reg_not_in_storemap(StoreMap, BaseReg, !CI),
    % IndexRval has already had Start subtracted from it.
    BaseRegInitCode = singleton(
        llds_instr(
            assign(BaseReg,
                mem_addr(heap_ref(MainVectorAddrRval, 0,
                    binop(int_mul,
                        IndexRval,
                        const(llconst_int(MainNumColumns)))))),
            "Compute base address for this case")
    ),

    generate_code_for_all_kinds(DescendingSortedKinds, 0, OutVars, ResumeVars,
        EndLabel, StoreMap, Liveness, AddTrailOps,
        BaseReg, LaterVectorAddrRval, !MaybeEnd, KindsCode, !CI),
    EndLabelCode = singleton(
        llds_instr(label(EndLabel),
            "end of int several soln lookup switch")
    ),
    Code = BaseRegInitCode ++ KindsCode ++ EndLabelCode.

generate_code_for_all_kinds(Kinds, NumPrevColumns, OutVars, ResumeVars,
        EndLabel, StoreMap, Liveness, AddTrailOps,
        BaseReg, LaterVectorAddrRval, !MaybeEnd, Code, !CI) :-
    % We release BaseReg in each arm of generate_code_for_each_kind below.
    % We cannot release it at the bottom of this predicate, because in the
    % kind_several_solns arm of generate_code_for_each_kind the generation
    % of the resume point will clobber the set of acquired registers.
    %
    % We cannot release the stack slots anywhere, since they will be needed
    % after backtracking to later alternatives of any model_non switch arm.
    acquire_temp_slot(slot_lookup_switch_cur, non_persistent_temp_slot,
        CurSlot, !CI),
    acquire_temp_slot(slot_lookup_switch_max, non_persistent_temp_slot,
        MaxSlot, !CI),

    remember_position(!.CI, BranchStart),
    generate_code_for_each_kind(Kinds, NumPrevColumns,OutVars, ResumeVars,
        BranchStart, EndLabel, StoreMap, Liveness, AddTrailOps, BaseReg,
        CurSlot, MaxSlot, LaterVectorAddrRval, !MaybeEnd, Code, !CI),
    set_resume_point_to_unknown(!CI).

:- func case_kind_to_string(case_kind) = string.

case_kind_to_string(kind_zero_solns) = "kind_zero_solns".
case_kind_to_string(kind_one_soln) = "kind_one_soln".
case_kind_to_string(kind_several_solns) = "kind_several_solns".

:- pred generate_code_for_each_kind(list(case_kind)::in, int::in,
    list(prog_var)::in, set(prog_var)::in, position_info::in,
    label::in, abs_store_map::in, set(prog_var)::in, add_trail_ops::in,
    lval::in, lval::in, lval::in, rval::in,
    branch_end::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out) is det.

generate_code_for_each_kind([], _, _, _, _, _, _, _, _, _, _, _, _,
        !MaybeEnd, _, !CI) :-
    unexpected(this_file, "generate_code_for_each_kind: no kinds").
generate_code_for_each_kind([Kind | Kinds], NumPrevColumns,
        OutVars, ResumeVars, BranchStart, EndLabel, StoreMap, Liveness,
        AddTrailOps, BaseReg, CurSlot, MaxSlot, LaterVectorAddrRval,
        !MaybeEnd, Code, !CI) :-
    (
        Kind = kind_zero_solns,
        TestOp = int_ge,
        reset_to_position(BranchStart, !CI),
        release_reg(BaseReg, !CI),
        generate_failure(KindCode, !CI)
    ;
        Kind = kind_one_soln,
        TestOp = ne,
        reset_to_position(BranchStart, !CI),
        generate_offset_assigns(OutVars, NumPrevColumns + 2, BaseReg, !CI),
        set_liveness_and_end_branch(StoreMap, Liveness, !MaybeEnd,
            BranchEndCode, !CI),
        release_reg(BaseReg, !CI),
        GotoEndCode = singleton(
            llds_instr(goto(code_label(EndLabel)),
                "goto end of switch from one_soln")
        ),
        KindCode = BranchEndCode ++ GotoEndCode
    ;
        Kind = kind_several_solns,
        TestOp = int_le,
        get_globals(!.CI, Globals),
        reset_to_position(BranchStart, !CI),

        % The code below is modelled on the code in disj_gen, but is
        % specialized for the situation here.

        produce_vars(ResumeVars, ResumeMap, FlushCode, !CI),
        MinOffsetColumnRval = const(llconst_int(NumPrevColumns)),
        MaxOffsetColumnRval = const(llconst_int(NumPrevColumns + 1)),
        SaveSlotsCode = from_list([
            llds_instr(assign(CurSlot,
                lval(field(yes(0), lval(BaseReg), MinOffsetColumnRval))),
                "Setup current slot in the later solution array"),
            llds_instr(assign(MaxSlot,
                lval(field(yes(0), lval(BaseReg), MaxOffsetColumnRval))),
                "Setup maximum slot in the later solution array")
        ]),
        maybe_save_ticket(AddTrailOps, SaveTicketCode, MaybeTicketSlot, !CI),
        globals.lookup_bool_option(Globals, reclaim_heap_on_nondet_failure,
            ReclaimHeap),
        maybe_save_hp(ReclaimHeap, SaveHpCode, MaybeHpSlot, !CI),
        prepare_for_disj_hijack(model_non, HijackInfo, PrepareHijackCode, !CI),

        remember_position(!.CI, DisjEntry),

        % Generate code for the non-last disjunct.

        make_resume_point(ResumeVars, resume_locs_stack_only,
            ResumeMap, ResumePoint, !CI),
        effect_resume_point(ResumePoint, model_non, UpdateRedoipCode, !CI),
        generate_offset_assigns(OutVars, NumPrevColumns + 2, BaseReg, !CI),
        flush_resume_vars_to_stack(FirstFlushResumeVarsCode, !CI),

        % Forget the variables that are needed only at the resumption point at
        % the start of the next disjunct, so that we don't generate exceptions
        % when their storage is clobbered by the movement of the live variables
        % to the places indicated in the store map.
        pop_resume_point(!CI),
        pickup_zombies(FirstZombies, !CI),
        make_vars_forward_dead(FirstZombies, !CI),

        set_liveness_and_end_branch(StoreMap, Liveness, !MaybeEnd,
            FirstBranchEndCode, !CI),
        release_reg(BaseReg, !CI),

        GotoEndCode = singleton(
            llds_instr(goto(code_label(EndLabel)),
                "goto end of switch from several_soln")
        ),

        reset_to_position(DisjEntry, !CI),
        generate_resume_point(ResumePoint, ResumePointCode, !CI),

        maybe_reset_ticket(MaybeTicketSlot, reset_reason_undo,
            RestoreTicketCode),
        maybe_restore_hp(MaybeHpSlot, RestoreHpCode),

        acquire_reg_not_in_storemap(StoreMap, LaterBaseReg, !CI),
        get_next_label(UndoLabel, !CI),
        get_next_label(AfterUndoLabel, !CI),
        list.length(OutVars, NumOutVars),
        TestMoreSolnsCode = from_list([
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
        undo_disj_hijack(HijackInfo, UndoHijackCode, !CI),
        AfterUndoLabelCode = from_list([
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
        effect_resume_point(ResumePoint, model_non,
            _LaterUpdateRedoipCode, !CI),

        generate_offset_assigns(OutVars, 0, LaterBaseReg, !CI),
        flush_resume_vars_to_stack(LaterFlushResumeVarsCode, !CI),

        % Forget the variables that are needed only at the resumption point at
        % the start of the next disjunct, so that we don't generate exceptions
        % when their storage is clobbered by the movement of the live variables
        % to the places indicated in the store map.
        pop_resume_point(!CI),
        pickup_zombies(LaterZombies, !CI),
        make_vars_forward_dead(LaterZombies, !CI),

        set_liveness_and_end_branch(StoreMap, Liveness, !MaybeEnd,
            LaterBranchEndCode, !CI),

        KindCode = FlushCode ++ SaveSlotsCode ++
            SaveTicketCode ++ SaveHpCode ++ PrepareHijackCode ++
            UpdateRedoipCode ++ FirstFlushResumeVarsCode ++
            FirstBranchEndCode ++ GotoEndCode ++ ResumePointCode ++
            RestoreTicketCode ++ RestoreHpCode ++
            TestMoreSolnsCode ++ UndoHijackCode ++ AfterUndoLabelCode ++
            LaterFlushResumeVarsCode ++ LaterBranchEndCode ++ GotoEndCode
    ),
    (
        Kinds = [],
        Code = KindCode
    ;
        Kinds = [NextKind | _],
        get_next_label(NextKindLabel, !CI),
        TestRval = binop(TestOp,
            lval(field(yes(0), lval(BaseReg),
                const(llconst_int(NumPrevColumns)))),
            const(llconst_int(0))),
        TestCode = from_list([
            llds_instr(if_val(TestRval, code_label(NextKindLabel)),
                "skip to next kind in several_soln lookup switch"),
            llds_instr(comment("This kind is " ++ case_kind_to_string(Kind)),
                "")
        ]),
        NextKindLabelCode = from_list([
            llds_instr(label(NextKindLabel),
                "next kind in several_soln lookup switch"),
            llds_instr(comment("Next kind is "
                ++ case_kind_to_string(NextKind)),
                "")
        ]),
        generate_code_for_each_kind(Kinds, NumPrevColumns, OutVars, ResumeVars,
            BranchStart, EndLabel, StoreMap, Liveness, AddTrailOps,
            BaseReg, CurSlot, MaxSlot, LaterVectorAddrRval,
            !MaybeEnd, LaterKindsCode, !CI),
        Code = TestCode ++ KindCode ++ NextKindLabelCode ++ LaterKindsCode
    ).

    % Note that we specify --optimise-constructor-last-call for this module
    % in order to make this predicate tail recursive.
    %
:- pred construct_several_soln_int_lookup_vector(int::in, int::in,
    list(llds_type)::in, int::in, assoc_list(int, soln_consts(rval))::in,
    list(list(rval))::out,
    int::in, cord(list(rval))::in, cord(list(rval))::out,
    int::in, int::out, int::in, int::out, int::in, int::out) is det.

construct_several_soln_int_lookup_vector(CurIndex, EndVal,
        OutTypes, NumOutTypes, [], MainRows,
        !.LaterNextRow, !LaterSolnArray,
        !FailCaseCount, !OneSolnCaseCount, !SeveralSolnCaseCount) :-
    ( CurIndex > EndVal ->
        MainRows = []
    ;
        construct_fail_row(OutTypes, MainRow, !FailCaseCount),
        construct_several_soln_int_lookup_vector(CurIndex + 1, EndVal,
            OutTypes, NumOutTypes, [], MoreMainRows,
            !.LaterNextRow, !LaterSolnArray,
            !FailCaseCount, !OneSolnCaseCount, !SeveralSolnCaseCount),
        MainRows = [MainRow | MoreMainRows]
    ).
construct_several_soln_int_lookup_vector(CurIndex, EndVal,
        OutTypes, NumOutTypes, [Index - Soln | Rest], [MainRow | MainRows],
        !.LaterNextRow, !LaterSolnArray,
        !FailCaseCount, !OneSolnCaseCount, !SeveralSolnCaseCount) :-
    ( CurIndex < Index ->
        construct_fail_row(OutTypes, MainRow, !FailCaseCount),
        Remainder = [Index - Soln | Rest]
    ;
        (
            Soln = one_soln(OutRvals),
            !:OneSolnCaseCount = !.OneSolnCaseCount + 1,
            ZeroRval = const(llconst_int(0)),
            % The first 0 means there is exactly one solution for this case;
            % the second 0 is a dummy that won't be referenced.
            MainRow = [ZeroRval, ZeroRval | OutRvals]
        ;
            Soln = several_solns(FirstSolnRvals, LaterSolns),
            !:SeveralSolnCaseCount = !.SeveralSolnCaseCount + 1,
            list.length(LaterSolns, NumLaterSolns),
            FirstRowOffset = !.LaterNextRow * NumOutTypes,
            LastRowOffset = (!.LaterNextRow + NumLaterSolns - 1) * NumOutTypes,
            FirstRowRval = const(llconst_int(FirstRowOffset)),
            LastRowRval = const(llconst_int(LastRowOffset)),
            MainRow = [FirstRowRval, LastRowRval | FirstSolnRvals],
            !:LaterNextRow = !.LaterNextRow + NumLaterSolns,
            !:LaterSolnArray = !.LaterSolnArray ++ from_list(LaterSolns)
        ),
        Remainder = Rest
    ),
    construct_several_soln_int_lookup_vector(CurIndex + 1, EndVal,
        OutTypes, NumOutTypes, Remainder, MainRows,
        !.LaterNextRow, !LaterSolnArray,
        !FailCaseCount, !OneSolnCaseCount, !SeveralSolnCaseCount).

:- pred construct_fail_row(list(llds_type)::in, list(rval)::out,
    int::in, int::out) is det.

construct_fail_row(OutTypes, MainRow, !FailCaseCount) :-
    % The -1 means no solutions for this case; the 0 is a dummy that
    % won't be referenced.
    ControlRvals = [const(llconst_int(-1)), const(llconst_int(0))],

    % Since this argument (array element) is a place-holder and will never be
    % referenced, just fill it in with a dummy entry.
    VarRvals = list.map(default_value_for_type, OutTypes),

    MainRow = ControlRvals ++ VarRvals,
    !:FailCaseCount = !.FailCaseCount + 1.

%-----------------------------------------------------------------------------%

    % The bitvector is an array of words (where we use the first 32 bits
    % of each word). Each bit represents a tag value for the (range checked)
    % input to the lookup switch. The bit is `1' iff we have a case for that
    % tag value.
    %
:- pred generate_bitvec_test(rval::in, assoc_list(int, T)::in,
    int::in, int::in, llds_code::out, code_info::in, code_info::out) is det.

generate_bitvec_test(IndexRval, CaseVals, Start, _End, CheckCode, !CI) :-
    get_globals(!.CI, Globals),
    get_word_bits(Globals, WordBits, Log2WordBits),
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
    fail_if_rval_is_false(HasBit, CheckCode, !CI).

    % We generate the bitvector by iterating through the cases marking the bit
    % for each case. We represent the bitvector here as a map from the word
    % number in the vector to the bits for that word.
    %
:- pred generate_bit_vec(assoc_list(int, T)::in, int::in, int::in,
    list(rval)::out, rval::out, code_info::in, code_info::out) is det.

generate_bit_vec(CaseVals, Start, WordBits, Args, BitVec, !CI) :-
    map.init(BitMap0),
    generate_bit_vec_2(CaseVals, Start, WordBits, BitMap0, BitMap),
    map.to_assoc_list(BitMap, WordVals),
    generate_bit_vec_args(WordVals, 0, Args),
    add_scalar_static_cell_natural_types(Args, DataAddr, !CI),
    BitVec = const(llconst_data_addr(DataAddr, no)).

:- pred generate_bit_vec_2(assoc_list(int, T)::in, int::in, int::in,
    map(int, int)::in, map(int, int)::out) is det.

generate_bit_vec_2([], _, _, !BitMap).
generate_bit_vec_2([Tag - _ | Rest], Start, WordBits, !BitMap) :-
    Val = Tag - Start,
    Word = Val // WordBits,
    Offset = Val mod WordBits,
    ( map.search(!.BitMap, Word, X0) ->
        X1 = X0 \/ (1 << Offset)
    ;
        X1 = (1 << Offset)
    ),
    svmap.set(Word, X1, !BitMap),
    generate_bit_vec_2(Rest, Start, WordBits, !BitMap).

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

default_value_for_type(lt_bool) = const(llconst_int(0)).
default_value_for_type(lt_int_least8) = const(llconst_int(0)).
default_value_for_type(lt_uint_least8) = const(llconst_int(0)).
default_value_for_type(lt_int_least16) = const(llconst_int(0)).
default_value_for_type(lt_uint_least16) = const(llconst_int(0)).
default_value_for_type(lt_int_least32) = const(llconst_int(0)).
default_value_for_type(lt_uint_least32) = const(llconst_int(0)).
default_value_for_type(lt_integer) = const(llconst_int(0)).
default_value_for_type(lt_unsigned) = const(llconst_int(0)).
default_value_for_type(lt_float) = const(llconst_float(0.0)).
default_value_for_type(lt_string) = const(llconst_string("")).
default_value_for_type(lt_data_ptr) = const(llconst_int(0)).
default_value_for_type(lt_code_ptr) = const(llconst_int(0)).
default_value_for_type(lt_word) = const(llconst_int(0)).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "lookup_switch.m".

%-----------------------------------------------------------------------------%
:- end_module lookup_switch.
%-----------------------------------------------------------------------------%
