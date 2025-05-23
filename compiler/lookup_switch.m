%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2015, 2017-2018, 2020-2022, 2024-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
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
%---------------------------------------------------------------------------%

:- module ll_backend.lookup_switch.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.lookup_switch_util.
:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.llds.
:- import_module ll_backend.lookup_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.

%---------------------------------------------------------------------------%

    % NOTE The first two fields of a lookup_switch_info, when composed
    % together, specify a map from the value of a Key (such as ints or strings)
    % to the values of the output variables for that Key.
    %
    % Lookup_switch_infos have traditionally stored just this composed map.
    % This was because this was the only map we needed before the addition of 
    %
    % - multi-cons-id switch arms, and
    % - lookup trie switches.
 
    % The lookup tables we generate for det or semidet switches have
    % traditionally looked like this:
    %
    %       key1    A1  B1  C1
    %       key2    A2  B2  C2
    %       key3    A3  B3  C3
    %       key4    A4  B4  C4
    %       key5    A5  B5  C5
    %
    % where A, B and C are the three output variables, and e.g. B3
    % is the value of the B output variable in the switch arm where
    % the key is key3.
    %
    % This is the best you can do in switches where each case is for
    % a single key. It is even the best you can do in switches where
    % *most* cases are for a single key. However, if the #keys/#cases ratio
    % is *much* higher than 1, then another arrangement may be better.
    % For example, if key1, key4 and key5 belong to the same case
    % (which implies that e.g. A1=A4, A1=A5, B1=B4, etc), then the setup
    % with two tables may be better:
    %
    %       key1    caseL
    %       key2    caseM
    %       key3    caseN
    %       key4    caseL
    %       key5    caseL
    %
    %       caseL   A1  B1  C1
    %       caseM   A2  B2  C2
    %       caseN   A3  B3  C3
    %
    % This setup can take less space, and accesses to e.g. keys 1 and 4 will
    % both get their values for the output variables from the same row
    % in the second table. Both of these factors can improve the hit rate
    % of the data cache.
    %
    % On the other hand, the fact that the values of the output variables
    % are not next to the keys forces one extra cache miss.
    %
    % (The higher the #keys/#cases ratio, the more important the advantages
    % of the second setup can be expected to be relative to the first.)
    %
    % While binary search and hash table based implementations of lookup
    % switches benefit in this way from having the values of the output
    % variables next to the key value, trie switches on strings do not;
    % they do not even store each key in its entirety. Trie based lookup
    % switches have two parts: the trie search, which needs the key to case id
    % map, and the lookup part, which needs the case id to the values of
    % the output variables part. The reason why the lookup_switch_info
    % structure stores these two data structures is because
    %
    % - trie lookup switches want this info in this form, and
    % - while binary search and hash lookup switches want this info
    %   in another form (mapping keys to solutions directly),
    %   it is easier to compute that form from the one we use
    %   than vice versa.
    %
:- type lookup_switch_info(Key)
    --->    lookup_switch_info(
                % The map from the switched-on value to the corresponding
                % case_id.
                lsi_key_to_case_map     :: map(Key, case_id),

                % The map from each case_id to the values of the variables
                % in each solution in that case.
                lsi_cases               ::  case_consts(case_id, rval,
                                                case_consts_several_llds),

                % The output variables, which become (some of) the fields
                % in each row of a lookup table.
                lsi_out_variables       ::  list(prog_var),

                % The types of the fields holding output variables.
                lsi_out_types           ::  list(llds_type),

                lsi_end_branch_info     ::  end_branch_info,

                lsi_branch_end          ::  branch_end,
                lsi_code_info           ::  code_info,
                lsi_code_loc_dep        ::  code_loc_dep
            ).

    % Decide whether we can generate code for this switch using a lookup table.
    %
:- pred is_lookup_switch((func(cons_tag) = Key)::in, list(tagged_case)::in,
    hlds_goal_info::in, code_info::in, code_loc_dep::in,
    maybe(lookup_switch_info(Key))::out) is det.

%---------------------------------------------------------------------------%

    % Generate code for the switch that the lookup_switch_info came from.
    %
:- pred generate_int_lookup_switch(rval::in, lookup_switch_info(int)::in,
    label::in, int::in, int::in, need_bit_vec_check::in, need_range_check::in,
    branch_end::out, llds_code::out, code_info::out) is det.

%---------------------------------------------------------------------------%

    % We conceptualize lookup tables as two-dimensional arrays. However,
    % the LLDS has no mechanism either for describing 2D arrays, or for
    % accessing their elements. Instead, we have to flatten the 2D array
    % into a 1D array that contains the rows of the conceptual 2D table
    % one after the other, in the usual row-major form. We can then access
    % the item at a given 2D coordinate (say row R, column C) by
    %
    % - computing the offset of the start of the 2D row R in the 1D flattened
    %   array,
    % - adding this offset to the address of the start of the whole 1D array,
    %   giving the address of the embedded 1D array for row R, and then
    % - accessing the item at offset C in that row.
    %
    % Most of our callers want to give us (the id of the register that
    % at runtime will contain) the row number directly. They can do this
    % because they don't need to access anything in that row themselves.
    % On the other hand, string hash switches *do* want to access something
    % in that row, so they also need to know where that row of the 2D table
    % is stored in the 1D array. It therefore gives us (the id of the register
    % that at runtime will contain) the starting offset of the row.
:- type main_table_row_select
    --->    main_row_number_reg(rval, list(llds_type))
            % We select the row we want in the main table by specifying
            % the rval containing the number of the row, and the types
            % of the row's elements. The number of these types specifies
            % the number of columns in each row.
    ;       main_row_start_offset_reg(rval).
            % We select the row we want in the main table by specifying
            % the rval containing the offset of its first column in the
            % 1D array. The given register will therefore contain the
            % row number multiplied by the number of columns in each row.

:- pred acquire_and_setup_lookup_base_reg(data_id::in, end_branch_info::in,
    main_table_row_select::in, lval::out, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

%---------------------------------------------------------------------------%

    % generate_single_soln_table_lookup_code_no_vars(EndBranch, Code,
    %   !MaybeEnd, CLD):
    %
    % Generate the "lookup code" for lookup switch that has no output
    % variables. The returned Code will ensure that all live variables
    % (which could have been bound *before* the switch) get put where
    % EndBranch says they should be at the end of the switch.
    %
:- pred generate_single_soln_table_lookup_code_no_vars(end_branch_info::in,
    llds_code::out, branch_end::in, branch_end::out, code_loc_dep::in) is det.

    % generate_single_soln_table_lookup_code_some_vars(TableDataId,
    %     RowSelect, NumPrevColumns, OutVars, EndBranch, Code,
    %     !MaybeEnd, CI, !.CLD):
    %
    % Generate the lookup code for lookup switch that has one or more
    % output variables. The returned Code will ensure that all live variables,
    % including OutVars, which should be the output variables of the switch,
    % get put where EndBranch says they should be at the end of the switch.
    % We get the values of OutVars from a row in the table identified by
    % TableDataId. The row is identified by RowSelect. We ignore the first
    % NumPrevColumns columns in that row, and use the following columns
    % as the sources of the values of OutVars.
    %
:- pred generate_single_soln_table_lookup_code_some_vars(data_id::in,
    main_table_row_select::in, int::in, list(prog_var)::in,
    end_branch_info::in, llds_code::out,
    branch_end::in, branch_end::out, code_info::in, code_loc_dep::in) is det.

    % append_goto_end(EndLabel, Code0, Code):
    %
    % Append a "goto EndLabel" after Code0.
    %
:- pred append_goto_end(label::in, llds_code::in, llds_code::out) is det.

%---------------------------------------------------------------------------%

:- type case_kind
    --->    kind_zero_solns
    ;       kind_one_soln
    ;       kind_several_solns.

    % generate_multi_soln_table_lookup_code(Kinds, NumPrevColumns, OutVars,
    %   ResumeVars, EndLabel, StoreMap, Liveness, AddTrailOps,
    %   BaseRegLval, LaterSolnsTableAddrRval, Code, !MaybeEnd, !CI, !.CLD):
    %
    % Generate code for the kinds of solution cardinalities listed in Kinds.
    %
    % - For kind_zero_solns, generate code that performs failure.
    % - For kind_one_solution, generate code that looks up the main table
    %   at row BaseRegLval and then goes to EndLabel.
    % - For kind_several_solns, generate code that looks up the main table
    %   at row BaseRegLval, sets up a resume point that stores ResumeVars,
    %   succeeds by going to EndLabel. On backtracking, the generated code
    %   will keep returning rows from the later solution table until there are
    %   no more later solutions associated with row BaseRegLval.
    %
    % The definition of EndLabel is up to the caller.
    %
    % For this predicate, the main table's columns form three groups.
    %
    % - The first group of NumPrevColumns columns are ignored by this
    %   predicate.
    %
    %   - For int switches and for string trie switches, there will be
    %     zero previous columns.
    %
    %   - For binary string switches, there is one previous column, which
    %     contains the string.
    %
    %   - For hash string switches, there are one or two previous columns,
    %     the first column containing the string, and the second (if it is
    %     needed) containing the number of the next slot in the open
    %     addressing sequence.
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
    % This predicate assumes that the caller has already set BaseRegLval
    % to point to the start of the relevant row in the main solutions table.
    % LaterSolnsTableAddrRval should be the address of the start of the later
    % solutions table.
    %
:- pred generate_multi_soln_table_lookup_code(case_consts_several_llds::in,
    pair(int, case_kind)::in, list(pair(int, case_kind))::in, int::in,
    list(prog_var)::in, label::in, lval::in, rval::in,
    end_branch_info::in, llds_code::out, branch_end::in, branch_end::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

%---------------------------------------------------------------------------%

:- func default_value_for_type(llds_type) = rval.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.switch_util.
:- import_module hlds.code_model.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_markers.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.code_util.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.global_data.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

is_lookup_switch(GetTag, TaggedCases, GoalInfo, CI0, CLD0,
        MaybeLookupSwitchInfo) :-
    % Most of this predicate is taken from dense_switch.m.

    % We need the code_info structure to generate code for the cases to
    % get the constants (if they exist). We can't throw it away at the
    % end because we may have allocated some new static ground terms.

    goal_info_get_store_map(GoalInfo, StoreMap),
    remember_position(CLD0, BranchStart),
    figure_out_output_vars(CI0, CLD0, GoalInfo, OutVars),
    set_of_var.list_to_set(OutVars, ArmNonLocals),
    ( if
        MaybeEnd0 = no,
        generate_constants_for_lookup_switch(BranchStart, GetTag, TaggedCases,
            OutVars, ArmNonLocals, StoreMap, Liveness,
            map.init, KeyToCaseIdMap, map.init, CaseIdToSolnsMap,
            MaybeEnd0, MaybeEnd, set_of_var.init, ResumeVars, no,
            GoalsMayModifyTrail, CI0, CI),
        get_var_table(CI, VarTable),
        OutTypes = list.map(lookup_var_type_func(VarTable), OutVars),
        ( if project_all_to_one_solution(CaseIdToSolnsMap, CaseValuesMap) then
            CaseConsts = all_one_soln(CaseValuesMap)
        else
            CaseConsts = some_several_solns(CaseIdToSolnsMap,
                case_consts_several_llds(ResumeVars, GoalsMayModifyTrail))
        ),
        get_exprn_opts(CI0, ExprnOpts),
        UnboxFloats = get_unboxed_floats(ExprnOpts),
        UnboxInt64s = get_unboxed_int64s(ExprnOpts),
        map.to_assoc_list(CaseIdToSolnsMap, CaseIdsSolns),
        % This generates CaseValues in reverse order of index, but given that
        % we only use CaseValues to find the right OutLldsTypes, this is OK.
        project_solns_to_rval_lists(CaseIdsSolns, [], CaseValues),
        find_general_llds_types(UnboxFloats, UnboxInt64s, OutTypes, CaseValues,
            OutLldsTypes)
    then
        reset_to_position(BranchStart, CI, CLD),
        EndBranch = end_branch_info(StoreMap, Liveness),
        LookupSwitchInfo = lookup_switch_info(KeyToCaseIdMap, CaseConsts,
            OutVars, OutLldsTypes, EndBranch, MaybeEnd, CI, CLD),
        MaybeLookupSwitchInfo = yes(LookupSwitchInfo)
    else
        MaybeLookupSwitchInfo = no
    ).

%---------------------------------------------------------------------------%

:- pred generate_constants_for_lookup_switch(position_info::in,
    (func(cons_tag) = Key)::in, list(tagged_case)::in, list(prog_var)::in,
    set_of_progvar::in, abs_store_map::in, set_of_progvar::out,
    map(Key, case_id)::in, map(Key, case_id)::out,
    map(case_id, soln_consts(rval))::in, map(case_id, soln_consts(rval))::out,
    branch_end::in, branch_end::out, set_of_progvar::in, set_of_progvar::out,
    bool::in, bool::out, code_info::in, code_info::out) is semidet.

generate_constants_for_lookup_switch(_BranchStart, _GetTag,
        [], _Vars, _ArmNonLocals, _StoreMap, set_of_var.init,
        !KeyToCaseIdMap, !CaseIdToSolnsMap, !MaybeEnd, !ResumeVars,
        !GoalsMayModifyTrail, !CI).
generate_constants_for_lookup_switch(BranchStart, GetTag,
        [TaggedCase | TaggedCases], Vars, ArmNonLocals, StoreMap, Liveness,
        !KeyToCaseIdMap, !CaseIdToSolnsMap, !MaybeEnd, !ResumeVars,
        !GoalsMayModifyTrail, !CI) :-
    TaggedCase =
        tagged_case(TaggedMainConsId, TaggedOtherConsIds, CaseId, Goal),
    Goal = hlds_goal(GoalExpr, GoalInfo),

    % Goals with these features need special treatment in generate_goal.
    Features = goal_info_get_features(GoalInfo),
    not set.member(feature_call_table_gen, Features),
    not set.member(feature_save_deep_excp_vars, Features),

    ( if GoalExpr = disj(Disjuncts) then
        (
            Disjuncts = [],
            % Cases like this should have been filtered out by
            % filter_out_failing_cases.
            unexpected($pred, "disj([])")
        ;
            Disjuncts = [FirstDisjunct | LaterDisjuncts],
            goal_is_conj_of_unify(ArmNonLocals, FirstDisjunct),
            all_disjuncts_are_conj_of_unify(ArmNonLocals, LaterDisjuncts),

            bool.or(goal_may_modify_trail(GoalInfo), !GoalsMayModifyTrail),

            FirstDisjunct = hlds_goal(_, FirstDisjunctGoalInfo),
            goal_info_get_resume_point(FirstDisjunctGoalInfo, ThisResumePoint),
            (
                ThisResumePoint = resume_point(ThisResumeVars, _),
                set_of_var.union(ThisResumeVars, !ResumeVars)
            ;
                ThisResumePoint = no_resume_point
            ),

            % The pre- and post-goal updates for the disjuncts themselves are
            % done as part of the call to generate_goal in
            % generate_constants_for_disjuncts in lookup_util.m.
            some [!CLD] (
                reset_to_position(BranchStart, !.CI, !:CLD),
                pre_goal_update(GoalInfo, has_subgoals, !CLD),
                remember_position(!.CLD, GoalBranchStart)
            ),
            generate_constants_for_disjunct(GoalBranchStart, FirstDisjunct,
                Vars, StoreMap, FirstSoln, !MaybeEnd, Liveness, !CI),
            generate_constants_for_disjuncts(GoalBranchStart, LaterDisjuncts,
                Vars, StoreMap, LaterSolns, !MaybeEnd, !CI),
            % Don't apply the post goal update, since nothing would see
            % the result.
            SolnConsts = several_solns(FirstSoln, LaterSolns)
        )
    else
        goal_is_conj_of_unify(ArmNonLocals, Goal),
        % The pre- and post-goal updates for the goals themselves
        % are done as part of the call to generate_goal in
        % generate_constants_for_disjuncts in lookup_util.m.
        generate_constants_for_arm(BranchStart, Goal, Vars, StoreMap, Soln,
            !MaybeEnd, Liveness, !CI),
        SolnConsts = one_soln(Soln)
    ),
    map.det_insert(CaseId, SolnConsts, !CaseIdToSolnsMap),
    record_case_id_for_tagged_cons_id(GetTag, CaseId,
        TaggedMainConsId, !KeyToCaseIdMap),
    record_case_id_for_tagged_cons_ids(GetTag, CaseId,
        TaggedOtherConsIds, !KeyToCaseIdMap),
    generate_constants_for_lookup_switch(BranchStart, GetTag,
        TaggedCases, Vars, ArmNonLocals, StoreMap, _LivenessRest,
        !KeyToCaseIdMap, !CaseIdToSolnsMap, !MaybeEnd, !ResumeVars,
        !GoalsMayModifyTrail, !CI).

:- pred record_case_id_for_tagged_cons_ids((func(cons_tag) = Key)::in,
    case_id::in, list(tagged_cons_id)::in,
    map(Key, case_id)::in, map(Key, case_id)::out) is det.

record_case_id_for_tagged_cons_ids(_GetTag, _CaseId, [], !KeyToCaseIdMap).
record_case_id_for_tagged_cons_ids(GetTag, CaseId,
        [TaggedConsId | TaggedConsIds], !KeyToCaseIdMap) :-
    record_case_id_for_tagged_cons_id(GetTag, CaseId,
        TaggedConsId, !KeyToCaseIdMap),
    record_case_id_for_tagged_cons_ids(GetTag, CaseId,
        TaggedConsIds, !KeyToCaseIdMap).

:- pred record_case_id_for_tagged_cons_id((func(cons_tag) = Key)::in,
    case_id::in, tagged_cons_id::in,
    map(Key, case_id)::in, map(Key, case_id)::out) is det.

record_case_id_for_tagged_cons_id(GetTag, CaseId, TaggedConsId,
        !KeyToCaseIdMap) :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    Index = GetTag(ConsTag),
    map.det_insert(Index, CaseId, !KeyToCaseIdMap).

%---------------------------------------------------------------------------%

generate_int_lookup_switch(VarRval, LookupSwitchInfo, EndLabel,
        StartVal, EndVal, NeedBitVecCheck, NeedRangeCheck, !:MaybeEnd, Code,
        !:CI) :-
    LookupSwitchInfo = lookup_switch_info(KeyToCaseMap, CaseConsts,
        OutVars, OutTypes, EndBranch, !:MaybeEnd, !:CI, CLD0),

    % If the case values start at some number other than 0,
    % then subtract that number to give us a zero-based index.
    ( if StartVal = 0 then
        IndexRval = VarRval
    else
        IndexRval = binop(int_arith(int_type_int, ao_sub),
            VarRval, const(llconst_int(StartVal)))
    ),

    % If the switch is not locally deterministic, we may need to check that
    % the value of the variable lies within the appropriate range.
    (
        NeedRangeCheck = need_range_check,
        Difference = EndVal - StartVal,
        CmpRval = binop(int_as_uint_cmp(le),
            IndexRval, const(llconst_int(Difference))),
        fail_if_rval_is_false(CmpRval, RangeCheckCode, !CI, CLD0, CLD)
    ;
        NeedRangeCheck = do_not_need_range_check,
        RangeCheckCode = empty,
        CLD = CLD0
    ),

    (
        CaseConsts = all_one_soln(CaseIdToValuesMap),
        compose_maps(KeyToCaseMap, CaseIdToValuesMap, KeyToSolnsMap),
        map.to_assoc_list(KeyToSolnsMap, KeySolnsAL),
        generate_simple_int_lookup_switch(IndexRval, StartVal, EndVal,
            KeySolnsAL, OutVars, OutTypes, NeedBitVecCheck, EndBranch,
            RangeCheckCode, Code, !CI, CLD)
    ;
        CaseConsts = some_several_solns(CaseIdToValuesListMap,
            CaseConstsSeveralLlds),
        compose_maps(KeyToCaseMap, CaseIdToValuesListMap, KeyToSolnsListMap),
        map.to_assoc_list(KeyToSolnsListMap, KeySolnsListAL),
        generate_several_soln_int_lookup_switch(CaseConstsSeveralLlds,
            IndexRval, EndLabel, StartVal, EndVal, KeySolnsListAL,
            OutVars, OutTypes, NeedBitVecCheck,
            EndBranch, RangeCheckCode, Code, !MaybeEnd, !CI, CLD)
    ).

:- pred generate_simple_int_lookup_switch(rval::in,
    int::in, int::in, assoc_list(int, list(rval))::in,
    list(prog_var)::in, list(llds_type)::in, need_bit_vec_check::in,
    end_branch_info::in, llds_code::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_simple_int_lookup_switch(IndexRval, StartVal, EndVal, CaseValues,
        OutVars, OutTypes, NeedBitVecCheck, EndBranch,
        RangeCheckCode, Code, !CI, !.CLD) :-
    (
        NeedBitVecCheck = need_bit_vec_check,
        generate_bitvec_test(IndexRval, CaseValues, StartVal, EndVal,
            CheckBitVecCode, !CI, !CLD)
    ;
        ( NeedBitVecCheck = do_not_need_bit_vec_check_no_gaps
        ; NeedBitVecCheck = do_not_need_bit_vec_check_with_gaps
        ),
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
        % ZZZ _MaybeEnd other copies
        generate_single_soln_table_lookup_code_no_vars(EndBranch, LookupCode,
            no, _MaybeEnd, !.CLD)
    ;
        OutVars = [_ | _],

        % Generate the static lookup table for this switch.
        construct_simple_int_lookup_vector(CaseValues, StartVal, OutTypes,
            cord.init, TableRvalsCord),
        TableRvals = cord.list(TableRvalsCord),
        add_vector_static_cell(OutTypes, TableRvals, TableDataId, !CI),

        % Generate code to look up each of the variables in OutVars in its slot
        % in the table row IndexRval (which will be row VarRval - StartVal).
        % IndexRval has already had StartVal subtracted from it.
        RowSelect = main_row_number_reg(IndexRval, OutTypes),
        NumPrevColumns = 0,
        generate_single_soln_table_lookup_code_some_vars(TableDataId,
            RowSelect, NumPrevColumns, OutVars, EndBranch, LookupCode,
            no, _MaybeEnd, !.CI, !.CLD)
    ),

    CommentCode = cord.singleton(
        llds_instr(comment("int single soln lookup switch"), "")
    ),
    % Note: we do not need an end label.
    Code = CommentCode ++ RangeCheckCode ++ CheckBitVecCode ++ LookupCode.

%---------------------------------------------------------------------------%

:- pred construct_simple_int_lookup_vector(assoc_list(int, list(rval))::in,
    int::in, list(llds_type)::in,
    cord(list(rval))::in, cord(list(rval))::out) is det.

construct_simple_int_lookup_vector([], _, _, !RowCord).
construct_simple_int_lookup_vector([Index - Rvals | Rest0], CurIndex, OutTypes,
        !RowCord) :-
    ( if CurIndex < Index then
        % If this argument (array element) is a place-holder and
        % will never be referenced, just fill it in with a dummy entry.
        Row = list.map(default_value_for_type, OutTypes),
        Rest = [Index - Rvals | Rest0]
    else
        Row = Rvals,
        Rest = Rest0
    ),
    cord.snoc(Row, !RowCord),
    construct_simple_int_lookup_vector(Rest, CurIndex + 1, OutTypes,
        !RowCord).

%---------------------------------------------------------------------------%

:- pred generate_several_soln_int_lookup_switch(case_consts_several_llds::in,
    rval::in, label::in, int::in, int::in,
    assoc_list(int, soln_consts(rval))::in,
    list(prog_var)::in, list(llds_type)::in,
    need_bit_vec_check::in, end_branch_info::in,
    llds_code::in, llds_code::out, branch_end::in, branch_end::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_several_soln_int_lookup_switch(CaseConstsSeveralLlds, IndexRval,
        EndLabel, StartVal, EndVal, CaseSolns, OutVars, OutTypes,
        NeedBitVecCheck, EndBranch, RangeCheckCode, Code,
        !MaybeEnd, !CI, !.CLD) :-
    % If there are no output variables, then how can the individual solutions
    % differ from each other?
    expect_not(unify(OutVars, []), $pred, "no OutVars"),

    % Now generate the static cells into which we do the lookups of the values
    % of the output variables, if there are any.
    %
    % We put a dummy row at the start of the later solns table, so that
    % a zero in the "later solns start row" column of the main table can mean
    % "no later solutions".
    list.length(OutTypes, NumOutTypes),
    InitLaterSolnRowNumber = 1,
    DummyLaterSolnRow = list.map(default_value_for_type, OutTypes),
    LaterSolnsRowsCord0 = cord.singleton(DummyLaterSolnRow),
    construct_several_soln_int_lookup_vector(StartVal, EndVal,
        OutTypes, NumOutTypes, CaseSolns, MainRows,
        InitLaterSolnRowNumber, LaterSolnsRowsCord0, LaterSolnsRowsCord,
        0, FailCaseCount, 0, OneSolnCaseCount, 0, SeveralSolnCaseCount),
    LaterSolnsRows = cord.list(LaterSolnsRowsCord),
    ( if (NeedBitVecCheck = need_bit_vec_check <=> FailCaseCount > 0) then
        true
    else
        unexpected($pred, "bad FailCaseCount")
    ),

    MainRowTypes = [lt_int(int_type_int), lt_int(int_type_int) | OutTypes],
    add_vector_static_cell(MainRowTypes, MainRows, MainTableDataId, !CI),
    add_vector_static_cell(OutTypes, LaterSolnsRows, LaterSolnsTableId, !CI),
    LaterSolnsTableAddrRval = const(llconst_data_addr(LaterSolnsTableId)),

    % IndexRval has already had Start subtracted from it.
    MainRowSelect = main_row_number_reg(IndexRval, MainRowTypes),
    acquire_and_setup_lookup_base_reg(MainTableDataId, EndBranch,
        MainRowSelect, BaseRegLval, SetBaseRegCode, !CLD),

    NumPrevColumns = 0,
    generate_multi_soln_table_lookup_code(CaseConstsSeveralLlds,
        FailCaseCount - kind_zero_solns,
        [OneSolnCaseCount - kind_one_soln,
        SeveralSolnCaseCount - kind_several_solns],
        NumPrevColumns, OutVars, EndLabel, BaseRegLval,
        LaterSolnsTableAddrRval, EndBranch, KindsCode, !MaybeEnd, !CI, !.CLD),
    MainCode = RangeCheckCode ++ SetBaseRegCode ++ KindsCode,
    SwitchKindStr = "int multi soln lookup switch",
    add_switch_kind_comment_and_end_label(SwitchKindStr, EndLabel,
        MainCode, Code).

%---------------------------------------------------------------------------%

acquire_and_setup_lookup_base_reg(MainTableDataId, EndBranch, MainRowSelect,
        BaseRegLval, SetBaseRegCode, !CLD) :-
    % Since we release BaseRegLval only after the calls to generate_branch_end,
    % we must make sure that generate_branch_end won't want to overwrite
    % BaseRegLval.
    EndBranch = end_branch_info(StoreMap, _),
    acquire_reg_not_in_storemap(StoreMap, reg_r, BaseRegLval, !CLD),
    (
        MainRowSelect = main_row_number_reg(MainRowNumRval, MainRowTypes),
        list.length(MainRowTypes, MainNumColumns),
        ( if MainNumColumns = 1 then
            MainRowStartOffsetRval = MainRowNumRval
        else
            MainRowStartOffsetRval = binop(int_arith(int_type_int, ao_mul),
                MainRowNumRval, const(llconst_int(MainNumColumns)))
        )
    ;
        MainRowSelect = main_row_start_offset_reg(MainRowStartOffsetRval)
    ),
    MainTableAddrRval = const(llconst_data_addr(MainTableDataId)),
    SetBaseRegCode = singleton(
        llds_instr(
            assign(BaseRegLval,
                mem_addr(
                    heap_ref(MainTableAddrRval, yes(ptag(0u8)),
                        MainRowStartOffsetRval))),
            "set up base reg")
    ).

%---------------------------------------------------------------------------%

generate_single_soln_table_lookup_code_no_vars(EndBranch, Code,
        !MaybeEnd, CLD) :-
    set_liveness_and_end_branch(EndBranch, Code, !MaybeEnd, CLD).

generate_single_soln_table_lookup_code_some_vars(MainTableDataId,
        MainRowSelect, NumPrevColumns, OutVars, EndBranch, LookupCode,
        !MaybeEnd, CI, !.CLD) :-
    % The predicates generate_single_soln_table_lookup_code_some_vars and
    % generate_table_lookup_code_for_kind_one_soln do the same job
    % in slightly different circumstances. They both implement lookups
    % that return one solution, i.e. one value bound to each var in OutVars,
    % where OutVars is not []. generate_single_soln_table_lookup_code_some_vars
    % does so in a switch in which *all* arms are like that, while
    % generate_table_lookup_code_for_kind_one_soln does so in a switch
    % in which some arms return more than one solution.
    %
    % The differences between the two predicates come about because
    % with different kinds of switch arms in the terms of solution
    % cardinality, generate_table_lookup_code_for_kind_one_soln
    %
    % - does not have to set up SetBaseRegCode, that task having been
    %   done earlier,
    %
    % - it does have to start by setting the location-dependent part
    %   of the code generator state to the branch start, since that state
    %   could have been by other code, and
    %
    % - it always has to add a goto to the end of the switch, since
    %   it is not guaranteed that the end label will immediately follow
    %   the code it returns.
    acquire_and_setup_lookup_base_reg(MainTableDataId, EndBranch,
        MainRowSelect, BaseRegLval, SetBaseRegCode, !CLD),
    record_offset_assigns(OutVars, NumPrevColumns, BaseRegLval, CI, !CLD),
    % We keep track of what variables are supposed to be live at the end
    % of cases. We have to do this explicitly because generating a `fail'
    % slot last would yield the wrong liveness.
    set_liveness_and_end_branch(EndBranch, BranchEndCode, !MaybeEnd, !.CLD),
    LookupCode = SetBaseRegCode ++ BranchEndCode.

append_goto_end(EndLabel, Code0, Code) :-
    GotoEndCode = singleton(
        llds_instr(goto(code_label(EndLabel)), "go to end of switch")
    ),
    Code = Code0 ++ GotoEndCode.

%---------------------------------------------------------------------------%

generate_multi_soln_table_lookup_code(CaseConstsSeveralLlds,
        CountKind, CountKinds, NumPrevColumns, OutVars, EndLabel,
        BaseRegLval, LaterSolnsTableAddrRval, EndBranch, Code,
        !MaybeEnd, !CI, !.CLD) :-
    list.sort([CountKind | CountKinds], AscendingSortedCountKinds),
    list.reverse(AscendingSortedCountKinds, DescendingSortedCountKinds),
    assoc_list.values(DescendingSortedCountKinds, DescendingSortedKinds),
    list.det_head_tail(DescendingSortedKinds, HeadKind, TailKinds),

    % We release BaseRegLval in each arm of generate_code_for_each_kind below.
    % We cannot release it at the bottom of this predicate, because in the
    % kind_several_solns arm of generate_code_for_each_kind the generation
    % of the resume point will clobber the set of acquired registers.
    %
    % We cannot release the stack slots anywhere, since they will be needed
    % after backtracking to later alternatives of any model_non switch arm.
    acquire_temp_slot(slot_lookup_switch_cur, non_persistent_temp_slot,
        CurSlot, !CI, !CLD),
    acquire_temp_slot(slot_lookup_switch_max, non_persistent_temp_slot,
        MaxSlot, !CI, !CLD),

    remember_position(!.CLD, BranchStart),
    generate_table_lookup_code_for_each_kind(CaseConstsSeveralLlds,
        HeadKind, TailKinds, NumPrevColumns, OutVars, BranchStart, EndLabel,
        BaseRegLval, CurSlot, MaxSlot, LaterSolnsTableAddrRval,
        EndBranch, Code, !MaybeEnd, !CI).

:- pred generate_table_lookup_code_for_each_kind(case_consts_several_llds::in,
    case_kind::in, list(case_kind)::in, int::in, list(prog_var)::in,
    position_info::in, label::in, lval::in, lval::in, lval::in, rval::in,
    end_branch_info::in, llds_code::out,
    branch_end::in, branch_end::out, code_info::in, code_info::out) is det.

generate_table_lookup_code_for_each_kind(CaseConstsSeveralLlds, Kind, Kinds,
        NumPrevColumns, OutVars, BranchStart, EndLabel, BaseRegLval,
        CurSlot, MaxSlot, LaterSolnsTableAddrRval, EndBranch, Code,
        !MaybeEnd, !CI) :-
    (
        Kind = kind_zero_solns,
        SkipToNextKindTestOp = int_cmp(int_type_int, ge),
        some [!CLD] (
            reset_to_position(BranchStart, !.CI, !:CLD),
            release_reg(BaseRegLval, !CLD),
            generate_failure(KindCode, !.CI, !.CLD)
        )
    ;
        Kind = kind_one_soln,
        SkipToNextKindTestOp = int_cmp(int_type_int, ne),
        NumSeveralColumns = 2,
        generate_table_lookup_code_for_kind_one_soln(NumPrevColumns,
            NumSeveralColumns, OutVars, BranchStart, EndLabel, BaseRegLval,
            EndBranch, KindCode, !MaybeEnd, !.CI)
    ;
        Kind = kind_several_solns,
        generate_table_lookup_code_for_kind_several_solns(
            CaseConstsSeveralLlds, NumPrevColumns, OutVars, BranchStart,
            EndLabel, BaseRegLval, CurSlot, MaxSlot, LaterSolnsTableAddrRval,
            SkipToNextKindTestOp, EndBranch, KindCode, !MaybeEnd, !CI)
    ),
    (
        Kinds = [],
        Code = KindCode
    ;
        Kinds = [NextKind | LaterKinds],
        get_next_label(NextKindLabel, !CI),
        TestRval = binop(SkipToNextKindTestOp,
            lval(field(yes(ptag(0u8)), lval(BaseRegLval),
                const(llconst_int(NumPrevColumns)))),
            const(llconst_int(0))),
        KindComment = "This kind is " ++ case_kind_to_string(Kind),
        NextKindComment = "Next kind is " ++ case_kind_to_string(NextKind),
        TestCode = cord.from_list([
            llds_instr(if_val(TestRval, code_label(NextKindLabel)),
                "skip to next kind in several_soln lookup switch"),
            llds_instr(comment(KindComment), "")
        ]),
        NextKindLabelCode = cord.from_list([
            llds_instr(label(NextKindLabel),
                "next kind in several_soln lookup switch"),
            llds_instr(comment(NextKindComment), "")
        ]),
        generate_table_lookup_code_for_each_kind(CaseConstsSeveralLlds,
            NextKind, LaterKinds, NumPrevColumns, OutVars, BranchStart,
            EndLabel, BaseRegLval, CurSlot, MaxSlot, LaterSolnsTableAddrRval,
            EndBranch, LaterKindsCode, !MaybeEnd, !CI),
        Code = TestCode ++ KindCode ++ NextKindLabelCode ++ LaterKindsCode
    ).

:- func case_kind_to_string(case_kind) = string.

case_kind_to_string(kind_zero_solns) = "kind_zero_solns".
case_kind_to_string(kind_one_soln) = "kind_one_soln".
case_kind_to_string(kind_several_solns) = "kind_several_solns".

%---------------------------------------------------------------------------%

:- pred generate_table_lookup_code_for_kind_one_soln(int::in, int::in,
    list(prog_var)::in, position_info::in, label::in, lval::in,
    end_branch_info::in, llds_code::out,
    branch_end::in, branch_end::out, code_info::in) is det.

generate_table_lookup_code_for_kind_one_soln(NumPrevColumns, NumSeveralColumns,
        OutVars, BranchStart, EndLabel, BaseRegLval,
        EndBranch, KindCode, !MaybeEnd, CI) :-
    % See the comment in generate_single_soln_table_lookup_code_some_vars.
    some [!CLD] (
        reset_to_position(BranchStart, CI, !:CLD),
        record_offset_assigns(OutVars, NumPrevColumns + NumSeveralColumns,
            BaseRegLval, CI, !CLD),
        set_liveness_and_end_branch(EndBranch, BranchEndCode, !MaybeEnd, !.CLD)
    ),
    append_goto_end(EndLabel, BranchEndCode, KindCode).

:- pred generate_table_lookup_code_for_kind_several_solns(
    case_consts_several_llds::in, int::in, list(prog_var)::in,
    position_info::in, label::in, lval::in, lval::in, lval::in, rval::in,
    binary_op::out, end_branch_info::in, llds_code::out,
    branch_end::in, branch_end::out, code_info::in, code_info::out) is det.

generate_table_lookup_code_for_kind_several_solns(CaseConstsSeveralLlds,
        NumPrevColumns, OutVars, BranchStart, EndLabel, BaseRegLval,
        CurSlot, MaxSlot, LaterSolnsTableAddrRval, TestOp, EndBranch,
        KindCode, !MaybeEnd, !CI) :-
    TestOp = int_cmp(int_type_int, le),
    get_globals(!.CI, Globals),
    some [!CLD] (
        reset_to_position(BranchStart, !.CI, !:CLD),

        CaseConstsSeveralLlds =
            case_consts_several_llds(ResumeVars, GoalsMayModifyTrail),
        (
            GoalsMayModifyTrail = yes,
            get_emit_trail_ops(!.CI, AddTrailOps)
        ;
            GoalsMayModifyTrail = no,
            AddTrailOps = do_not_add_trail_ops
        ),

        % The code below is modelled on the code in disj_gen, but is
        % specialized for the situation here.

        produce_vars(set_of_var.to_sorted_list(ResumeVars), ResumeMap,
            FlushCode, !CLD),
        MinOffsetColumnRval = const(llconst_int(NumPrevColumns)),
        MaxOffsetColumnRval = const(llconst_int(NumPrevColumns + 1)),
        NumSeveralColumns = 2,
        SaveSlotsCode = cord.from_list([
            llds_instr(assign(CurSlot,
                lval(field(yes(ptag(0u8)), lval(BaseRegLval),
                    MinOffsetColumnRval))),
                "Setup current slot in the later solution array"),
            llds_instr(assign(MaxSlot,
                lval(field(yes(ptag(0u8)), lval(BaseRegLval),
                    MaxOffsetColumnRval))),
                "Setup maximum slot in the later solution array")
        ]),
        maybe_save_ticket(AddTrailOps, SaveTicketCode, MaybeTicketSlot,
            !CI, !CLD),
        globals.lookup_bool_option(Globals, reclaim_heap_on_nondet_failure,
            ReclaimHeap),
        maybe_save_hp(ReclaimHeap, SaveHpCode, MaybeHpSlot, !CI, !CLD),
        prepare_for_disj_hijack(model_non, HijackInfo, PrepareHijackCode,
            !CI, !CLD),

        remember_position(!.CLD, DisjEntry),

        % Generate code for the non-last disjunct.
        make_resume_point(ResumeVars, resume_locs_stack_only, ResumeMap,
            ResumePoint, !CI),
        effect_resume_point(ResumePoint, model_non, UpdateRedoipCode, !CLD),
        record_offset_assigns(OutVars, NumPrevColumns + NumSeveralColumns,
            BaseRegLval, !.CI, !CLD),
        flush_resume_vars_to_stack(FirstFlushResumeVarsCode, !CLD),

        % Forget the variables that are needed only at the resumption point
        % at the start of the next disjunct, so that we don't generate
        % exceptions when their storage is clobbered by the movement
        % of the live variables to the places indicated in the store map.
        pop_resume_point(!CLD),
        pickup_zombies(FirstZombies, !CLD),
        make_vars_forward_dead(FirstZombies, !CLD),

        set_liveness_and_end_branch(EndBranch, FirstBranchEndCode, !MaybeEnd,
            !.CLD)
    ),

    GotoEndCode = cord.singleton(
        llds_instr(goto(code_label(EndLabel)),
            "goto end of switch from several_soln")
    ),

    some [!CLD] (
        reset_to_position(DisjEntry, !.CI, !:CLD),
        generate_resume_point(ResumePoint, ResumePointCode, !CI, !CLD),

        maybe_reset_ticket(MaybeTicketSlot, reset_reason_undo,
            RestoreTicketCode),
        maybe_restore_hp(MaybeHpSlot, RestoreHpCode),

        EndBranch = end_branch_info(StoreMap, _),
        acquire_reg_not_in_storemap(StoreMap, reg_r, LaterBaseRegLval, !CLD),
        get_next_label(UndoLabel, !CI),
        get_next_label(AfterUndoLabel, !CI),
        list.length(OutVars, NumOutVars),
        TestMoreSolnsCode = cord.from_list([
            llds_instr(assign(LaterBaseRegLval, lval(CurSlot)),
                "Init later base register"),
            llds_instr(
                if_val(binop(int_cmp(int_type_int, ge),
                    lval(LaterBaseRegLval), lval(MaxSlot)),
                code_label(UndoLabel)),
                "Jump to undo hijack code if there are no more solutions"),
            llds_instr(assign(CurSlot,
                binop(int_arith(int_type_int, ao_add),
                    lval(CurSlot),
                    const(llconst_int(NumOutVars)))),
                "Update current slot in the later solution array"),
            llds_instr(goto(code_label(AfterUndoLabel)),
                "Jump around undo hijack code"),
            llds_instr(label(UndoLabel),
                "Undo hijack code")
        ]),
        undo_disj_hijack(HijackInfo, UndoHijackCode, !CLD),
        AfterUndoLabelCode = cord.from_list([
            llds_instr(label(AfterUndoLabel),
                "Return later answer code"),
            llds_instr(assign(LaterBaseRegLval,
                mem_addr(heap_ref(LaterSolnsTableAddrRval, yes(ptag(0u8)),
                    lval(LaterBaseRegLval)))),
                "Compute base address in later array for this solution")
        ]),

        % We need to call effect_resume_point in order to push ResumePoint
        % onto the failure continuation stack, so pop_resume_point can pop
        % it off. However, since the redoip already points there, we don't
        % need to execute _LaterUpdateRedoipCode.
        effect_resume_point(ResumePoint, model_non,
            _LaterUpdateRedoipCode, !CLD),

        record_offset_assigns(OutVars, 0, LaterBaseRegLval, !.CI, !CLD),
        flush_resume_vars_to_stack(LaterFlushResumeVarsCode, !CLD),

        % Forget the variables that are needed only at the resumption point
        % at the start of the next disjunct, so that we don't generate
        % exceptions when their storage is clobbered by the movement
        % of the live variables to the places indicated in the store map.
        pop_resume_point(!CLD),
        pickup_zombies(LaterZombies, !CLD),
        make_vars_forward_dead(LaterZombies, !CLD),

        set_liveness_and_end_branch(EndBranch, LaterBranchEndCode, !MaybeEnd,
            !.CLD)
    ),

    KindCode = FlushCode ++ SaveSlotsCode ++
        SaveTicketCode ++ SaveHpCode ++ PrepareHijackCode ++
        UpdateRedoipCode ++ FirstFlushResumeVarsCode ++
        FirstBranchEndCode ++ GotoEndCode ++ ResumePointCode ++
        RestoreTicketCode ++ RestoreHpCode ++
        TestMoreSolnsCode ++ UndoHijackCode ++ AfterUndoLabelCode ++
        LaterFlushResumeVarsCode ++ LaterBranchEndCode ++ GotoEndCode.

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
    ( if CurIndex > EndVal then
        MainRows = []
    else
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
    ( if CurIndex < Index then
        construct_fail_row(OutTypes, MainRow, !FailCaseCount),
        Remainder = [Index - Soln | Rest]
    else
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
            !:LaterSolnArray = !.LaterSolnArray ++ cord.from_list(LaterSolns)
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

%---------------------------------------------------------------------------%

    % The bitvector is an array of words (where we use the first 32 bits
    % of each word). Each bit represents a tag value for the (range checked)
    % input to the lookup switch. The bit is `1' iff we have a case for that
    % tag value.
    %
:- pred generate_bitvec_test(rval::in, assoc_list(int, T)::in,
    int::in, int::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_bitvec_test(IndexRval, CaseVals, Start, _End, CheckCode,
        !CI, !CLD) :-
    get_globals(!.CI, Globals),
    get_target_host_min_word_size(Globals, WordSize),
    ( WordSize = word_size_32, WordBits = 32, Log2WordBits = 5
    ; WordSize = word_size_64, WordBits = 64, Log2WordBits = 6
    ),
    generate_bit_vec(CaseVals, Start, WordBits, BitVecArgs, BitVecRval, !CI),

    % Optimize the single-word case: if all the cases fit into a single word,
    % then the word to use is always that word, and the index specifies which
    % bit. Otherwise, the high bits of the index specify which word to use
    % and the low bits specify which bit.
    ( if BitVecArgs = [SingleWord] then
        Word = SingleWord,
        BitNum = IndexRval
    else
        % This is the same as
        % WordNum = binop(int_div, IndexRval, const(llconst_int(WordBits)))
        % except that it can generate more efficient code.
        WordNum = binop(unchecked_right_shift(int_type_int, shift_by_int),
            IndexRval, const(llconst_int(Log2WordBits))),

        Word = lval(field(yes(ptag(0u8)), BitVecRval, WordNum)),

        % This is the same as
        % BitNum = binop(int_mod, IndexRval, const(llconst_int(WordBits)))
        % except that it can generate more efficient code.
        BitNum = binop(bitwise_and(int_type_int), IndexRval,
            const(llconst_int(WordBits - 1)))
    ),
    HasBit = binop(bitwise_and(int_type_int),
        binop(unchecked_left_shift(int_type_int, shift_by_int),
            const(llconst_int(1)), BitNum),
        Word),
    fail_if_rval_is_false(HasBit, CheckCode, !CI, !CLD).

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
    BitVec = const(llconst_data_addr(DataAddr)).

:- pred generate_bit_vec_2(assoc_list(int, T)::in, int::in, int::in,
    map(int, int)::in, map(int, int)::out) is det.

generate_bit_vec_2([], _, _, !BitMap).
generate_bit_vec_2([Tag - _ | Rest], Start, WordBits, !BitMap) :-
    Val = Tag - Start,
    Word = Val // WordBits,
    Offset = Val mod WordBits,
    ( if map.search(!.BitMap, Word, X0) then
        X1 = X0 \/ (1 << Offset)
    else
        X1 = (1 << Offset)
    ),
    map.set(Word, X1, !BitMap),
    generate_bit_vec_2(Rest, Start, WordBits, !BitMap).

:- pred generate_bit_vec_args(list(pair(int))::in, int::in,
    list(rval)::out) is det.

generate_bit_vec_args([], _, []).
generate_bit_vec_args([Word - Bits | Rest], Count, [Rval | Rvals]) :-
    ( if Count < Word then
        WordVal = 0,
        Remainder = [Word - Bits | Rest]
    else
        WordVal = Bits,
        Remainder = Rest
    ),
    Rval = const(llconst_int(WordVal)),
    Count1 = Count + 1,
    generate_bit_vec_args(Remainder, Count1, Rvals).

%---------------------------------------------------------------------------%

default_value_for_type(lt_bool) = const(llconst_int(0)).
default_value_for_type(lt_int_least(_)) = const(llconst_int(0)).
default_value_for_type(lt_int(int_type_int)) = const(llconst_int(0)).
default_value_for_type(lt_int(int_type_uint)) = const(llconst_uint(0u)).
default_value_for_type(lt_int(int_type_int8)) = const(llconst_int8(0i8)).
default_value_for_type(lt_int(int_type_uint8)) = const(llconst_uint8(0u8)).
default_value_for_type(lt_int(int_type_int16)) = const(llconst_int16(0i16)).
default_value_for_type(lt_int(int_type_uint16)) = const(llconst_uint16(0u16)).
default_value_for_type(lt_int(int_type_int32)) = const(llconst_int32(0i32)).
default_value_for_type(lt_int(int_type_uint32)) = const(llconst_uint32(0u32)).
default_value_for_type(lt_int(int_type_int64)) = const(llconst_int64(0i64)).
default_value_for_type(lt_int(int_type_uint64)) = const(llconst_uint64(0u64)).
default_value_for_type(lt_float) = const(llconst_float(0.0)).
default_value_for_type(lt_string) = const(llconst_string("")).
default_value_for_type(lt_data_ptr) = const(llconst_int(0)).
default_value_for_type(lt_code_ptr) = const(llconst_int(0)).
default_value_for_type(lt_word) = const(llconst_int(0)).

%---------------------------------------------------------------------------%
:- end_module ll_backend.lookup_switch.
%---------------------------------------------------------------------------%
