%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: lookup_switch.m.
% Author: conway.

% For switches on atomic types in which the cases contain only the
% construction of constants, generate code which just assigns the values of
% the output variables by indexing into an array of values for each output
% variable.
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
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.instmap.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.tree.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.dense_switch.
:- import_module ll_backend.exprn_aux.
:- import_module ll_backend.global_data.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module solutions.

:- type case_consts == assoc_list(int, list(rval)).

:- type lookup_switch_info
    --->    lookup_switch_info(
                lsi_first               :: int,
                lsi_last                :: int,
                                        % The first and last values of the
                                        % switched-on rval covered by the
                                        % switch.
                lsi_cases               :: case_consts,
                                        % The map from the switched-on value
                                        % to the the values of the variables.
                lsi_variables           :: list(prog_var),
                                        % The output variables.
                lsi_field_types         :: list(llds_type),
                                        % The types of the fields in the C
                                        % structure we generate for each case.
                lsi_need_range_check    :: can_fail,
                lsi_need_bit_vec_check  :: can_fail,
                                        % Do we need a range check and/or a
                                        % bit vector check on the switched-on
                                        % variable?
                lsi_liveness            :: maybe(set(prog_var))
            ).

%-----------------------------------------------------------------------------%

    % Most of this predicate is taken from dense_switch.m.
    %
is_lookup_switch(CaseVar, TaggedCases, GoalInfo, SwitchCanFail, ReqDensity,
        StoreMap, !MaybeEnd, CodeModel, LookupSwitchInfo, !CI) :-
    % We need the code_info structure to generate code for the cases to
    % get the constants (if they exist). We can't throw it away at the
    % end because we may have allocated some new static ground term labels.

    % Since lookup switches rely on static ground terms to work efficiently,
    % there is no point in using a lookup switch if static-ground-terms are
    % not enabled. Well, actually, it is possible that they might be a win in
    % some circumstances, but it would take a pretty complex heuristic to get
    % it right, so, lets just use a simple one - no static ground terms,
    % no lookup switch.
    code_info.get_globals(!.CI, Globals),
    globals.lookup_bool_option(Globals, static_ground_terms, yes),

    % We want to generate a lookup switch for any switch that is dense enough
    % - we don't care how many cases it has. A memory lookup tends to be
    % cheaper than a branch.
    list.length(TaggedCases, NumCases),
    TaggedCases = [FirstCase | _],
    FirstCase = case(_, int_constant(FirstCaseVal), _, _),
    list.index1_det(TaggedCases, NumCases, LastCase),
    LastCase = case(_, int_constant(LastCaseVal), _, _),
    Span = LastCaseVal - FirstCaseVal,
    Range = Span + 1,
    dense_switch.calc_density(NumCases, Range, Density),
    Density > ReqDensity,

    % If there are going to be no gaps in the lookup table then we won't need
    % a bitvector test to see if this switch has a value for this case.
    ( NumCases = Range ->
        NeedBitVecCheck0 = cannot_fail
    ;
        NeedBitVecCheck0 = can_fail
    ),
    (
        SwitchCanFail = can_fail,
        % For semidet switches, we normally need to check that the variable
        % is in range before we index into the jump table. However, if the
        % range of the type is sufficiently small, we can make the jump table
        % large enough to hold all of the values for the type, but then we
        % will need to do the bitvector test.
        Type = code_info.variable_type(!.CI, CaseVar),
        code_info.get_module_info(!.CI, ModuleInfo),
        classify_type(ModuleInfo, Type) = TypeCategory,
        (
            dense_switch.type_range(!.CI, TypeCategory, Type, TypeRange),
            dense_switch.calc_density(NumCases, TypeRange, DetDensity),
            DetDensity > ReqDensity
        ->
            NeedRangeCheck = cannot_fail,
            NeedBitVecCheck = can_fail,
            FirstVal = 0,
            LastVal = TypeRange - 1
        ;
            NeedRangeCheck = SwitchCanFail,
            NeedBitVecCheck = NeedBitVecCheck0,
            FirstVal = FirstCaseVal,
            LastVal = LastCaseVal
        )
    ;
        SwitchCanFail = cannot_fail,
        NeedRangeCheck = cannot_fail,
        NeedBitVecCheck = NeedBitVecCheck0,
        FirstVal = FirstCaseVal,
        LastVal = LastCaseVal
    ),
    figure_out_output_vars(!.CI, GoalInfo, OutVars),
    generate_constants(TaggedCases, OutVars, StoreMap, !MaybeEnd, CodeModel,
        CaseValuePairs, MaybeLiveness, !CI),
    VarTypes = get_var_types(!.CI),
    list.map(map.lookup(VarTypes), OutVars, OutTypes),
    assoc_list.values(CaseValuePairs, CaseValues),
    code_info.get_globals(!.CI, Globals),
    globals.lookup_bool_option(Globals, unboxed_float, UnboxFloat),
    find_general_llds_types(UnboxFloat, OutTypes, CaseValues, LLDSTypes),
    LookupSwitchInfo = lookup_switch_info(FirstVal, LastVal, CaseValuePairs,
        OutVars, LLDSTypes, NeedRangeCheck, NeedBitVecCheck, MaybeLiveness).

%---------------------------------------------------------------------------%

    % Figure out which variables are bound in the switch.
    % We do this by using the current instmap and the instmap delta in the
    % goal info to work out which variables are [further] bound by the switch.
    %
:- pred figure_out_output_vars(code_info::in, hlds_goal_info::in,
    list(prog_var)::out) is det.

figure_out_output_vars(CI, GoalInfo, OutVars) :-
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    ( instmap_delta_is_unreachable(InstMapDelta) ->
        OutVars = []
    ;
        code_info.get_instmap(CI, CurrentInstMap),
        code_info.get_module_info(CI, ModuleInfo),
        instmap_delta_changed_vars(InstMapDelta, ChangedVars),
        instmap.apply_instmap_delta(CurrentInstMap, InstMapDelta,
            InstMapAfter),
        Lambda = (pred(Var::out) is nondet :-
            % If a variable has a final inst, then it changed
            % instantiatedness during the switch.
            set.member(Var, ChangedVars),
            instmap.lookup_var(CurrentInstMap, Var, Initial),
            instmap.lookup_var(InstMapAfter, Var, Final),
            mode_is_output(ModuleInfo, (Initial -> Final))
        ),
        solutions.solutions(Lambda, OutVars)
    ).

%---------------------------------------------------------------------------%

    % To figure out if the outputs are constants, we generate code for
    % the cases, and check to see if each of the output vars is a constant,
    % and that no actual code was generated for the goal.
    %
:- pred generate_constants(cases_list::in, list(prog_var)::in,
    abs_store_map::in, branch_end::in, branch_end::out, code_model::in,
    case_consts::out, maybe(set(prog_var))::out,
    code_info::in, code_info::out) is semidet.

generate_constants([], _Vars, _StoreMap, !MaybeEnd, _CodeModel, [], no, !CI).
generate_constants([Case | Cases], Vars, StoreMap, !MaybeEnd, CodeModel,
        [CaseVal | Rest], yes(Liveness), !CI) :-
    Case = case(_, int_constant(CaseTag), _, Goal),
    code_info.remember_position(!.CI, BranchStart),
    code_gen.generate_goal(CodeModel, Goal, Code, !CI),
    tree.tree_of_lists_is_empty(Code),
    code_info.get_forward_live_vars(!.CI, Liveness),
    get_case_rvals(Vars, CaseRvals, !CI),
    CaseVal = CaseTag - CaseRvals,
    % EndCode code may contain instructions that place Vars in the locations
    % dictated by StoreMap, and thus does not have to be empty. (The array
    % lookup code will put those variables in those locations directly.)
    code_info.generate_branch_end(StoreMap, !MaybeEnd, _EndCode, !CI),
    code_info.reset_to_position(BranchStart, !CI),
    generate_constants(Cases, Vars, StoreMap, !MaybeEnd,
        CodeModel, Rest, _, !CI).

%---------------------------------------------------------------------------%

:- pred get_case_rvals(list(prog_var)::in, list(rval)::out,
    code_info::in, code_info::out) is semidet.

get_case_rvals([], [], !CI).
get_case_rvals([Var | Vars], [Rval | Rvals], !CI) :-
    code_info.produce_variable(Var, Code, Rval, !CI),
    tree.tree_of_lists_is_empty(Code),
    code_info.get_globals(!.CI, Globals),
    globals.get_options(Globals, Options),
    exprn_aux.init_exprn_opts(Options, ExprnOpts),
    rval_is_constant(Rval, ExprnOpts),
    get_case_rvals(Vars, Rvals, !CI).

%---------------------------------------------------------------------------%

    % rval_is_constant(Rval, ExprnOpts) is true iff Rval is a constant.
    % This depends on the options governing nonlocal gotos, asm labels enabled
    % and static ground terms, etc.
    %
:- pred rval_is_constant(rval::in, exprn_opts::in) is semidet.

rval_is_constant(const(Const), ExprnOpts) :-
    exprn_aux.const_is_constant(Const, ExprnOpts, yes).
rval_is_constant(unop(_, Exprn), ExprnOpts) :-
    rval_is_constant(Exprn, ExprnOpts).
rval_is_constant(binop(_, Exprn0, Exprn1), ExprnOpts) :-
    rval_is_constant(Exprn0, ExprnOpts),
    rval_is_constant(Exprn1, ExprnOpts).
rval_is_constant(mkword(_, Exprn0), ExprnOpts) :-
    rval_is_constant(Exprn0, ExprnOpts).

:- pred rvals_are_constant(list(maybe(rval))::in,
    exprn_opts::in) is semidet.

rvals_are_constant([], _).
rvals_are_constant([MRval | MRvals], ExprnOpts) :-
    MRval = yes(Rval),
    rval_is_constant(Rval, ExprnOpts),
    rvals_are_constant(MRvals, ExprnOpts).

%---------------------------------------------------------------------------%

generate_lookup_switch(Var, StoreMap, MaybeEnd0, LookupSwitchInfo, Code,
        !CI) :-
    LookupSwitchInfo = lookup_switch_info(StartVal, EndVal, CaseValues,
        OutVars, LLDSTypes, NeedRangeCheck, NeedBitVecCheck, MaybeLiveness),

    % Evaluate the variable which we are going to be switching on.
    code_info.produce_variable(Var, VarCode, Rval, !CI),

    % If the case values start at some number other than 0,
    % then subtract that number to give us a zero-based index.
    ( StartVal = 0 ->
        IndexRval = Rval
    ;
        IndexRval = binop(int_sub, Rval, const(int_const(StartVal)))
    ),

    % If the switch is not locally deterministic, we need to check that
    % the value of the variable lies within the appropriate range.
    (
        NeedRangeCheck = can_fail,
        Difference = EndVal - StartVal,
        CmpRval = binop(unsigned_le, IndexRval, const(int_const(Difference))),
        code_info.fail_if_rval_is_false(CmpRval, RangeCheckCode, !CI)
    ;
        NeedRangeCheck = cannot_fail,
        RangeCheckCode = empty
    ),
    (
        NeedBitVecCheck = can_fail,
        generate_bitvec_test(IndexRval, CaseValues, StartVal, EndVal,
            CheckBitVecCode, !CI)
    ;
        NeedBitVecCheck = cannot_fail,
        CheckBitVecCode = empty
    ),

    % Now generate the terms into which we do the lookups of the values of
    % the output variables, if there are any.
    % 
    % Note that invoking generate_terms when OutVars = [] would lead to
    % a compiler abort, since we cannot create C structures with zero fields.
    (
        OutVars = [],
        BaseRegCode = empty,
        MaybeBaseReg = no
    ;
        OutVars = [_ | _],
        % Since we release BaseReg only after the call to generate_branch_end,
        % we must make sure that generate_branch_end won't want to overwrite
        % BaseReg.
        code_info.acquire_reg_not_in_storemap(StoreMap, BaseReg, !CI),
        MaybeBaseReg = yes(BaseReg),
        generate_terms(IndexRval, OutVars, LLDSTypes, CaseValues, StartVal,
            BaseRegCode, BaseReg, !CI)
    ),

    % We keep track of what variables are supposed to be live at the end
    % of cases. We have to do this explicitly because generating a `fail' slot
    % last would yield the wrong liveness.
    (
        MaybeLiveness = yes(Liveness),
        code_info.set_forward_live_vars(Liveness, !CI)
    ;
        MaybeLiveness = no,
        unexpected(this_file, "generate_lookup_switch: no liveness!")
    ),
    code_info.generate_branch_end(StoreMap, MaybeEnd0, _MaybeEnd,
        BranchEndCode, !CI),
    (
        MaybeBaseReg = no
    ;
        MaybeBaseReg = yes(FinalBaseReg),
        code_info.release_reg(FinalBaseReg, !CI)
    ),
    Comment = node([comment("lookup switch") - ""]),
    Code = tree_list([Comment, VarCode, RangeCheckCode, CheckBitVecCode,
        BaseRegCode, BranchEndCode]).

%-----------------------------------------------------------------------------%

    % The bitvector is an array of words (where we use the first 32 bits
    % of each word). Each bit represents a tag value for the (range checked)
    % input to the lookup switch. The bit is `1' iff we have a case for that
    % tag value.
    %
:- pred generate_bitvec_test(rval::in, case_consts::in, int::in, int::in,
    code_tree::out, code_info::in, code_info::out) is det.

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
        % WordNum = binop(int_div, IndexRval, const(int_const(WordBits)))
        % except that it can generate more efficient code.
        WordNum = binop(unchecked_right_shift, IndexRval,
            const(int_const(Log2WordBits))),

        Word = lval(field(yes(0), BitVecRval, WordNum)),

        % This is the same as
        % BitNum = binop(int_mod, IndexRval, const(int_const(WordBits)))
        % except that it can generate more efficient code.
        BitNum = binop(bitwise_and, IndexRval, const(int_const(WordBits - 1)))
    ),
    HasBit = binop(bitwise_and,
        binop(unchecked_left_shift, const(int_const(1)), BitNum), Word),
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
:- pred generate_bit_vec(case_consts::in, int::in, int::in,
    list(rval)::out, rval::out, code_info::in, code_info::out) is det.

generate_bit_vec(CaseVals, Start, WordBits, Args, BitVec, !CI) :-
    map.init(Empty),
    generate_bit_vec_2(CaseVals, Start, WordBits, Empty, BitMap),
    map.to_assoc_list(BitMap, WordVals),
    generate_bit_vec_args(WordVals, 0, Args),
    add_scalar_static_cell_natural_types(Args, DataAddr, !CI),
    BitVec = const(data_addr_const(DataAddr, no)).

:- pred generate_bit_vec_2(case_consts::in, int::in, int::in,
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
    Rval = const(int_const(WordVal)),
    Count1 = Count + 1,
    generate_bit_vec_args(Remainder, Count1, Rvals).

%-----------------------------------------------------------------------------%

    % Add an expression to the expression cache in the code_info structure
    % for each of the output variables of the lookup switch. This is done by
    % creating a `create' term for the array, and caching an expression
    % for the variable to get the IndexRval'th field of that term.
    %
:- pred generate_terms(rval::in, list(prog_var)::in, list(llds_type)::in,
    case_consts::in, int::in, code_tree::out, lval::in,
    code_info::in, code_info::out) is det.

generate_terms(IndexRval, OutVars, OutTypes, CaseVals, Start, Code, BaseReg,
        !CI) :-
    list.length(OutVars, NumOutVars),
    construct_vector(Start, CaseVals, VectorRvals),
    code_info.add_vector_static_cell(OutTypes, VectorRvals, VectorAddr, !CI),

    VectorAddrRval = const(data_addr_const(VectorAddr, no)),
    % IndexRval has already had Start subtracted from it.
    ( NumOutVars = 1 ->
        BaseRval = IndexRval
    ;
        BaseRval = binop(int_mul, IndexRval, const(int_const(NumOutVars)))
    ),
    Code = node([
        assign(BaseReg, mem_addr(heap_ref(VectorAddrRval, 0, BaseRval)))
            - "Compute base address for this case"
    ]),
    generate_offset_assigns(OutVars, 0, BaseReg, !CI).

:- pred construct_vector(int::in, case_consts::in,
    list(maybe(list(rval)))::out) is det.

construct_vector(_, [], []).
construct_vector(CurIndex, [Index - Rvals | Rest], [MaybeRow | MaybeRows]) :-
    ( CurIndex < Index ->
        % If this argument (array element) is a place-holder and
        % will never be referenced, just fill it in with a dummy entry.
        MaybeRow = no,
        Remainder = [Index - Rvals | Rest]
    ;
        MaybeRow = yes(Rvals),
        Remainder = Rest
    ),
    construct_vector(CurIndex + 1, Remainder, MaybeRows).

:- pred generate_offset_assigns(list(prog_var)::in, int::in, lval::in,
    code_info::in, code_info::out) is det.

generate_offset_assigns([], _, _, !CI).
generate_offset_assigns([Var | Vars], Offset, BaseReg, !CI) :-
    LookupLval = field(yes(0), lval(BaseReg), const(int_const(Offset))),
    code_info.assign_lval_to_var(Var, LookupLval, Code, !CI),
    expect(tree.is_empty(Code), this_file,
        "generate_offset_assigns: nonempty code"),
    generate_offset_assigns(Vars, Offset + 1, BaseReg, !CI).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "lookup_switch.m".

%-----------------------------------------------------------------------------%
:- end_module lookup_switch.
%-----------------------------------------------------------------------------%
