%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% lookup_switch.m
% Author: conway.
%
% For switches on atomic types in which the cases contain only the construction
% of constants, generate code which just assigns the values of the output
% variables by indexing into an array of values for each output variable.
%
% For switches that can fail, the generated code does a range check on the
% index, and then does a lookup in a bit-vector to see if there is a value
% for the appropriate case. If there is, then it does a lookup (using the
% field macro) in the array of results. The array is padded with "0"s for
% cases that are not covered. This is fine, since we do the lookup after
% we check the bit-vector for the appropriate case.
%
% The current implementation works out whether or not it can do a lookup
% switch by generating code for each case and looking to see that no code
% got generated (i.e. only the code generation state got modified) and that
% the output variables of the switch are all constants. This is potentially
% quite inefficient because it does the work of generating code for the cases
% and then may throw it away if a subsequent case generates actual code, or
% non constant outputs.
%
% A potential improvement would be to make a single array for each switch,
% since putting the values produced for each tag value side-by-side in
% memory will tend to lead to fewer cache misses.
%
% The number of bits per word is taken from the bits_per_word option which
% uses a flag in the mc script with a value from configuration. This is
% used when generating bit-vectors.
%
%-----------------------------------------------------------------------------%

:- module ll_backend__lookup_switch.

:- interface.

:- import_module backend_libs__switch_util.
:- import_module hlds__code_model.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_llds.
:- import_module ll_backend__code_info.
:- import_module ll_backend__llds.
:- import_module parse_tree__prog_data.

:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.

:- type case_consts == list(pair(int, list(rval))).

:- type rval_map == map(prog_var, list(pair(int, rval))).

:- pred is_lookup_switch(prog_var::in, cases_list::in,
    hlds_goal_info::in, can_fail::in, int::in, abs_store_map::in,
    branch_end::in, branch_end::out, code_model::in, int::out, int::out,
    can_fail::out, can_fail::out, list(prog_var)::out, case_consts::out,
    maybe(set(prog_var))::out, code_info::in, code_info::out) is semidet.

    % Generate code for a switch using a lookup table.
    %
:- pred generate_lookup_switch(prog_var::in, list(prog_var)::in,
    case_consts::in, int::in, int::in, can_fail::in, can_fail::in,
    maybe(set(prog_var))::in, abs_store_map::in, branch_end::in,
    code_tree::out, code_info::in, code_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__builtin_ops.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__type_util.
:- import_module hlds__instmap.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module libs__tree.
:- import_module ll_backend__code_gen.
:- import_module ll_backend__dense_switch.
:- import_module ll_backend__exprn_aux.
:- import_module ll_backend__global_data.
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module require.

    % Most of this predicate is taken from dense_switch.m

is_lookup_switch(CaseVar, TaggedCases, GoalInfo, SwitchCanFail,
        ReqDensity, StoreMap, !MaybeEnd, CodeModel,
        FirstVal, LastVal, NeedRangeCheck, NeedBitVecTest, OutVars,
        CaseValues, MLiveness, !CI) :-
    % We need the code_info structure to generate code for the cases to
    % get the constants (if they exist). We can't throw it away at the
    % end because we may have allocated some new static ground term labels.

    % Since lookup switches rely on static ground terms to work efficiently,
    % there is no point in using a lookup switch if static-ground-terms are
    % not enabled. Well, actually, it is possible that they might be a win in
    % some circumstances, but it would take a pretty complex heuristic to get
    % it right, so, lets just use a simple one - no static ground terms,
    % no lookup switch.
    code_info__get_globals(!.CI, Globals),
    globals__lookup_bool_option(Globals, static_ground_terms, yes),

    % We want to generate a lookup switch for any switch that is dense enough
    % - we don't care how many cases it has. A memory lookup tends to be
    % cheaper than a branch.
    list__length(TaggedCases, NumCases),
    TaggedCases = [FirstCase | _],
    FirstCase = case(_, int_constant(FirstCaseVal), _, _),
    list__index1_det(TaggedCases, NumCases, LastCase),
    LastCase = case(_, int_constant(LastCaseVal), _, _),
    Span = LastCaseVal - FirstCaseVal,
    Range = Span + 1,
    dense_switch__calc_density(NumCases, Range, Density),
    Density > ReqDensity,

    % If there are going to be no gaps in the lookup table then we won't need
    % a bitvector test to see if this switch has a value for this case.
    ( NumCases = Range ->
        NeedBitVecTest0 = cannot_fail
    ;
        NeedBitVecTest0 = can_fail
    ),
    (
        SwitchCanFail = can_fail,
        % For semidet switches, we normally need to check that the variable
        % is in range before we index into the jump table. However, if the
        % range of the type is sufficiently small, we can make the jump table
        % large enough to hold all of the values for the type, but then we
        % will need to do the bitvector test.
        Type = code_info__variable_type(!.CI, CaseVar),
        code_info__get_module_info(!.CI, ModuleInfo),
        classify_type(ModuleInfo, Type) = TypeCategory,
        (
            dense_switch__type_range(!.CI, TypeCategory, Type, TypeRange),
            dense_switch__calc_density(NumCases, TypeRange, DetDensity),
            DetDensity > ReqDensity
        ->
            NeedRangeCheck = cannot_fail,
            NeedBitVecTest = can_fail,
            FirstVal = 0,
            LastVal = TypeRange - 1
        ;
            NeedRangeCheck = SwitchCanFail,
            NeedBitVecTest = NeedBitVecTest0,
            FirstVal = FirstCaseVal,
            LastVal = LastCaseVal
        )
    ;
        SwitchCanFail = cannot_fail,
        NeedRangeCheck = cannot_fail,
        NeedBitVecTest = NeedBitVecTest0,
        FirstVal = FirstCaseVal,
        LastVal = LastCaseVal
    ),
    figure_out_output_vars(!.CI, GoalInfo, OutVars),
    generate_constants(TaggedCases, OutVars, StoreMap, !MaybeEnd, CodeModel,
        CaseValues, MLiveness, !CI).

%---------------------------------------------------------------------------%

    % Figure out which variables are bound in the switch.
    % We do this by using the current instmap and the instmap delta in
    % the goal info to work out which variables are [further] bound by
    % the switch.

:- pred figure_out_output_vars(code_info::in, hlds_goal_info::in,
    list(prog_var)::out) is det.

figure_out_output_vars(CI, GoalInfo, OutVars) :-
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    ( instmap_delta_is_unreachable(InstMapDelta) ->
        OutVars = []
    ;
        code_info__get_instmap(CI, CurrentInstMap),
        code_info__get_module_info(CI, ModuleInfo),
        instmap_delta_changed_vars(InstMapDelta, ChangedVars),
        instmap__apply_instmap_delta(CurrentInstMap, InstMapDelta,
            InstMapAfter),
        Lambda = (pred(Var::out) is nondet :-
            % If a variable has a final inst, then it changed
            % instantiatedness during the switch.
            set__member(Var, ChangedVars),
            instmap__lookup_var(CurrentInstMap, Var, Initial),
            instmap__lookup_var(InstMapAfter, Var, Final),
            mode_is_output(ModuleInfo, (Initial -> Final))
        ),
        solutions(Lambda, OutVars)
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
    code_info__remember_position(!.CI, BranchStart),
    code_gen__generate_goal(CodeModel, Goal, Code, !CI),
    tree__tree_of_lists_is_empty(Code),
    code_info__get_forward_live_vars(!.CI, Liveness),
    get_case_rvals(Vars, CaseRvals, !CI),
    CaseVal = CaseTag - CaseRvals,
    % EndCode code may contain instructions that place Vars in the locations
    % dictated by StoreMap, and thus does not have to be empty. (The array
    % lookup code will put those variables in those locations directly.)
    code_info__generate_branch_end(StoreMap, !MaybeEnd, _EndCode, !CI),
    code_info__reset_to_position(BranchStart, !CI),
    generate_constants(Cases, Vars, StoreMap, !MaybeEnd,
        CodeModel, Rest, _, !CI).

%---------------------------------------------------------------------------%

:- pred get_case_rvals(list(prog_var)::in, list(rval)::out,
    code_info::in, code_info::out) is semidet.

get_case_rvals([], [], !CI).
get_case_rvals([Var | Vars], [Rval | Rvals], !CI) :-
    code_info__produce_variable(Var, Code, Rval, !CI),
    tree__tree_of_lists_is_empty(Code),
    code_info__get_globals(!.CI, Globals),
    globals__get_options(Globals, Options),
    exprn_aux__init_exprn_opts(Options, ExprnOpts),
    rval_is_constant(Rval, ExprnOpts),
    get_case_rvals(Vars, Rvals, !CI).

%---------------------------------------------------------------------------%

    % rval_is_constant(Rval, ExprnOpts) is true iff Rval is a constant.
    % This depends on the options governing nonlocal gotos, asm labels enabled
    % and static ground terms, etc.
    %
:- pred rval_is_constant(rval::in, exprn_opts::in) is semidet.

rval_is_constant(const(Const), ExprnOpts) :-
    exprn_aux__const_is_constant(Const, ExprnOpts, yes).
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

generate_lookup_switch(Var, OutVars, CaseValues, StartVal, EndVal,
        NeedRangeCheck, NeedBitVecCheck, MLiveness, StoreMap, MaybeEnd0,
        Code, !CI) :-

    % Evaluate the variable which we are going to be switching on.
    code_info__produce_variable(Var, VarCode, Rval, !CI),

    % If the case values start at some number other than 0,
    % then subtract that number to give us a zero-based index.
    ( StartVal = 0 ->
        Index = Rval
    ;
        Index = binop(-, Rval, const(int_const(StartVal)))
    ),

    % If the switch is not locally deterministic, we need to check that
    % the value of the variable lies within the appropriate range.
    (
        NeedRangeCheck = can_fail,
        Difference = EndVal - StartVal,
        code_info__fail_if_rval_is_false(
            binop(unsigned_le, Index,
                const(int_const(Difference))), RangeCheck, !CI)
    ;
        NeedRangeCheck = cannot_fail,
        RangeCheck = empty
    ),
    (
        NeedBitVecCheck = can_fail,
        generate_bitvec_test(Index, CaseValues, StartVal, EndVal, CheckBitVec,
            !CI)
    ;
        NeedBitVecCheck = cannot_fail,
        CheckBitVec = empty
    ),

    % Now generate the terms into which we do the lookups.
    generate_terms(Index, OutVars, CaseValues, StartVal, !CI),

    % We keep track of what variables are supposed to be live at the end
    % of cases. We have to do this explicitly because generating a `fail' slot
    % last would yield the wrong liveness.
    (
        MLiveness = yes(Liveness),
        code_info__set_forward_live_vars(Liveness, !CI)
    ;
        MLiveness = no,
        unexpected(this_file, "generate_lookup_switch: no liveness!")
    ),
    code_info__generate_branch_end(StoreMap, MaybeEnd0, _MaybeEnd, LookupCode,
        !CI),
    Comment = node([comment("lookup switch") - ""]),
    Code = tree_list([Comment, VarCode, RangeCheck, CheckBitVec, LookupCode]).

%------------------------------------------------------------------------------%

    % The bitvector is an array of words (where we use the first 32 bits
    % of each word). Each bit represents a tag value for the (range checked)
    % input to the lookup switch. The bit is `1' iff we have a case for that
    % tag value.
    %
:- pred generate_bitvec_test(rval::in, case_consts::in, int::in, int::in,
    code_tree::out, code_info::in, code_info::out) is det.

generate_bitvec_test(Index, CaseVals, Start, _End, CheckCode, !CI) :-
    get_word_bits(!.CI, WordBits, Log2WordBits),
    generate_bit_vec(CaseVals, Start, WordBits, BitVecArgs, BitVecRval, !CI),

    % Optimize the single-word case: if all the cases fit into a single word,
    % then the word to use is always that word, and the index specifies which
    % bit. Otherwise, the high bits of the index specify which word to use
    % and the low bits specify which bit.
    ( BitVecArgs = [SingleWord] ->
        Word = SingleWord,
        BitNum = Index
    ;
        % This is the same as
        % WordNum = binop(/, Index, const(int_const(WordBits)))
        % except that it can generate more efficient code.
        WordNum = binop(>>, Index, const(int_const(Log2WordBits))),

        Word = lval(field(yes(0), BitVecRval, WordNum)),

        % This is the same as
        % BitNum = binop(mod, Index, const(int_const(WordBits)))
        % except that it can generate more efficient code.
        BitNum = binop(&, Index, const(int_const(WordBits - 1)))
    ),
    HasBit = binop((&), binop((<<), const(int_const(1)), BitNum), Word),
    code_info__fail_if_rval_is_false(HasBit, CheckCode, !CI).

    % Prevent cross-compilation errors by making sure that the bitvector
    % uses a number of bits that will fit both on this machine (so that
    % we can correctly generate it), and on the target machine (so that
    % it can be executed correctly). Also make sure that the number of bits
    % that we use is a power of 2, so that we implement division as
    % right-shift (see above).
    %
:- pred get_word_bits(code_info::in, int::out, int::out) is det.

get_word_bits(CI, WordBits, Log2WordBits) :-
    int__bits_per_int(HostWordBits),
    code_info__get_globals(CI, Globals),
    globals__lookup_int_option(Globals, bits_per_word, TargetWordBits),
    int__min(HostWordBits, TargetWordBits, WordBits0),
    % round down to the nearest power of 2
    Log2WordBits = log2_rounded_down(WordBits0),
    int__pow(2, Log2WordBits, WordBits).

:- func log2_rounded_down(int) = int.

log2_rounded_down(X) = Log :-
    int__log2(X + 1, Log + 1).  % int__log2 rounds up

    % We generate the bitvector by iterating through the cases marking the bit
    % for each case. (We represent the bitvector here as a map from the word
    % number in the vector to the bits for that word.
    %
:- pred generate_bit_vec(case_consts::in, int::in, int::in,
    list(rval)::out, rval::out, code_info::in, code_info::out) is det.

generate_bit_vec(CaseVals, Start, WordBits, Args, BitVec, !CI) :-
    map__init(Empty),
    generate_bit_vec_2(CaseVals, Start, WordBits, Empty, BitMap),
    map__to_assoc_list(BitMap, WordVals),
    generate_bit_vec_args(WordVals, 0, Args),
    add_static_cell_natural_types(Args, DataAddr, !CI),
    BitVec = const(data_addr_const(DataAddr, no)).

:- pred generate_bit_vec_2(case_consts::in, int::in, int::in,
    map(int, int)::in, map(int, int)::out) is det.

generate_bit_vec_2([], _, _, Bits, Bits).
generate_bit_vec_2([Tag - _ | Rest], Start, WordBits, Bits0, Bits) :-
    Val = Tag - Start,
    Word = Val // WordBits,
    Offset = Val mod WordBits,
    ( map__search(Bits0, Word, X0) ->
        X1 = X0 \/ (1 << Offset)
    ;
        X1 = (1 << Offset)
    ),
    map__set(Bits0, Word, X1, Bits1),
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

%------------------------------------------------------------------------------%

    % Add an expression to the expression cache in the code_info structure
    % for each of the output variables of the lookup switch. This is done by
    % creating a `create' term for the array, and caching an expression
    % for the variable to get the Index'th field of that term.
    %
:- pred generate_terms(rval::in, list(prog_var)::in, case_consts::in, int::in,
    code_info::in, code_info::out) is det.

generate_terms(Index, OutVars, CaseVals, Start, !CI) :-
    map__init(Empty),
    rearrange_vals(OutVars, CaseVals, Start, Empty, ValMap),
    generate_terms_2(Index, OutVars, ValMap, !CI).

:- pred generate_terms_2(rval::in, list(prog_var)::in,
    rval_map::in, code_info::in, code_info::out) is det.

generate_terms_2(_Index, [], _Map, !CI).
generate_terms_2(Index, [Var | Vars], Map, !CI) :-
    map__lookup(Map, Var, Vals0),
    list__sort(Vals0, Vals),
    construct_args(Vals, 0, Args),
    code_info__add_static_cell_natural_types(Args, DataAddr, !CI),
    ArrayTerm = const(data_addr_const(DataAddr, no)),
    LookupLval = field(yes(0), ArrayTerm, Index),
    code_info__assign_lval_to_var(Var, LookupLval, Code, !CI),
    require(tree__is_empty(Code), "generate_terms_2: nonempty code"),
    generate_terms_2(Index, Vars, Map, !CI).

:- pred construct_args(list(pair(int, rval))::in, int::in, list(rval)::out)
    is det.

construct_args([], _, []).
construct_args([Index - Rval | Rest], Count0, [Arg | Args]) :-
    ( Count0 < Index ->
        % If this argument (array element) is a place-holder and
        % will never be referenced, just fill it in with a `0'.
        Arg = const(int_const(0)),
        Remainder = [Index - Rval | Rest]
    ;
        Arg = Rval,
        Remainder = Rest
    ),
    Count1 = Count0 + 1,
    construct_args(Remainder, Count1, Args).

%------------------------------------------------------------------------------%

    % For the purpose of constructing the terms, the case_consts structure
    % is a bit inconvenient, so we rearrange the data into a map from var
    % to list of tag-value pairs.
    %
:- pred rearrange_vals(list(prog_var)::in, case_consts::in, int::in,
    rval_map::in, rval_map::out) is det.

rearrange_vals(_Vars, [], _Start, Map, Map).
rearrange_vals(Vars, [Tag - Rvals | Rest], Start, Map0, Map) :-
    assoc_list__from_corresponding_lists(Vars, Rvals, Pairs),
    Index = Tag - Start,
    rearrange_vals_2(Pairs, Index, Map0, Map1),
    rearrange_vals(Vars, Rest, Start, Map1, Map).

:- pred rearrange_vals_2(list(pair(prog_var, rval))::in, int::in,
    rval_map::in, rval_map::out) is det.

rearrange_vals_2([], _, Map, Map).
rearrange_vals_2([Var - Rval | Rest], Tag, Map0, Map) :-
    ( map__search(Map0, Var, Vals0) ->
        Vals = [Tag - Rval | Vals0]
    ;
        Vals = [Tag - Rval]
    ),
    map__set(Map0, Var, Vals, Map1),
    rearrange_vals_2(Rest, Tag, Map1, Map).

%------------------------------------------------------------------------------%

:- func this_file = string.

this_file = "lookup_switch.m".

%------------------------------------------------------------------------------%
