%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% Copyright (C) 2015, 2021, 2023-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: inst_merge.m.
% Author: fjh.
%
% This module merges insts by effectively computing (or approximating)
% their least upper bound.
%
% When a variable that is live after a branched control structure
% has its inst set (or updated) in the various branches, we merge
% that variable's final insts in those branches into a single inst,
% which we then use as the variable's inst at the program point just after
% the branched control structure. This merged inst must correctly describe
% the instantiation state of the variable at that program point *regardless*
% of which branch execution took to get there.
%
%---------------------------------------------------------------------------%

:- module check_hlds.inst_merge.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % insts_merge(Type, HeadInst, TailInsts, MaybeMergedInst, !ModuleInfo):
    %
    % Given a list of one or more insts of a given variable that reflect
    % the inst of that variable at the ends of the branches of a branched
    % control structure such as a disjunction or if-then-else, return
    %
    % - either `yes(MergedInst)' where MergedInst is the final inst
    %   of that variable after the branched control structure as a whole,
    %
    % - or `no' if some of the insts are not compatible.
    %
:- pred insts_merge(mer_type::in, mer_inst::in, list(mer_inst)::in,
    maybe(mer_inst)::out, module_info::in, module_info::out) is det.

    % inst_merge(Type, InstA, InstB, InstAB, !ModuleInfo):
    %
    % Combine the insts found in different arms of a disjunction, switch, or
    % if-then-else. The information in InstAB is the minimum of the information
    % in InstA and InstB. Where InstA and InstB specify a binding (free or
    % bound), it must be the same in both.
    %
    % In the vast majority of cases, our caller knows the type of variable
    % whose insts InstA and InstB represent. When it does not, it should pass
    % the type returned by the no_type_available function. This can happen e.g.
    % when the type of a constructor argument is existentially typed.
    % (We could figure it out from the types of the variables that were used
    % to construct that term, but since that construction could have
    % taken place in another predicate, we can't count on having access
    % to that information.)
    %
:- pred inst_merge(mer_type::in, mer_inst::in, mer_inst::in, mer_inst::out,
    module_info::in, module_info::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_lookup.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_inst_mode.
:- import_module parse_tree.prog_type.

:- import_module require.
:- import_module set.

%---------------------------------------------------------------------------%

insts_merge(Type, HeadInst, TailInsts, MaybeMergedInst, !ModuleInfo) :-
    % We used to use a straightforward algorithm that, given a list of N insts,
    % merged the tail N-1 insts, and merged the result with the head inst.
    % While this is simple and efficient for small N, it has very bad
    % performance for large N. The reason is that its complexity can be N^2,
    % since in many cases each arm of the branched control structure binds
    % the variable to a different function symbol, and this means that the
    % merged inst evolves like this:
    %
    %   bound(f)
    %   bound(f; g)
    %   bound(f; g; h)
    %   bound(f; g; h; i)
    %
    % Our current algorithm uses a number of passes, each of which merges
    % groups of up to eight adjacent insts, thus dividing the number of insts
    % by eight. The overall complexity is thus closer to N log N than N^2.
    insts_merge_pass(Type, HeadInst, TailInsts,
        [], MergedInsts, merge_has_not_failed, Fail, !ModuleInfo),
    (
        Fail = merge_has_failed,
        MaybeMergedInst = no
    ;
        Fail = merge_has_not_failed,
        (
            MergedInsts = [],
            % insts_merge_pass can return MergedInsts = [], but only
            % together with Fail = merge_has_failed.
            unexpected($pred, "MergedInsts = []")
        ;
            MergedInsts = [MergedInst1 | MergedInsts2Plus],
            (
                MergedInsts2Plus = [],
                MaybeMergedInst = yes(MergedInst1)
            ;
                MergedInsts2Plus = [_ | _],
                insts_merge(Type, MergedInst1, MergedInsts2Plus,
                    MaybeMergedInst, !ModuleInfo)
            )
        )
    ).

:- type merge_fail
    --->    merge_has_not_failed
    ;       merge_has_failed.

:- pred insts_merge_pass(mer_type::in, mer_inst::in, list(mer_inst)::in,
    list(mer_inst)::in, list(mer_inst)::out,
    merge_fail::in, merge_fail::out, module_info::in, module_info::out) is det.

insts_merge_pass(Type, I1, Is2Plus, !MergedIs, !Fail, !ModuleInfo) :-
    (
        Is2Plus = [],
        !:MergedIs = [I1 | !.MergedIs]
    ;
        Is2Plus = [I2],
        ( if
            inst_merge(Type, I1, I2, I12, !ModuleInfo)
        then
            !:MergedIs = [I12 | !.MergedIs]
        else
            !:Fail = merge_has_failed
        )
    ;
        Is2Plus = [I2, I3],
        ( if
            inst_merge(Type, I1, I2, I12, !ModuleInfo),
            inst_merge(Type, I12, I3, I123, !ModuleInfo)
        then
            !:MergedIs = [I123 | !.MergedIs]
        else
            !:Fail = merge_has_failed
        )
    ;
        Is2Plus = [I2, I3, I4],
        ( if
            inst_merge(Type, I1, I2, I12, !ModuleInfo),
            inst_merge(Type, I3, I4, I34, !ModuleInfo),
            inst_merge(Type, I12, I34, I1234, !ModuleInfo)
        then
            !:MergedIs = [I1234 | !.MergedIs]
        else
            !:Fail = merge_has_failed
        )
    ;
        Is2Plus = [I2, I3, I4, I5],
        ( if
            inst_merge(Type, I1, I2, I12, !ModuleInfo),
            inst_merge(Type, I12, I3, I123, !ModuleInfo),
            inst_merge(Type, I4, I5, I45, !ModuleInfo),
            inst_merge(Type, I123, I45, I12345, !ModuleInfo)
        then
            !:MergedIs = [I12345 | !.MergedIs]
        else
            !:Fail = merge_has_failed
        )
    ;
        Is2Plus = [I2, I3, I4, I5, I6],
        ( if
            inst_merge(Type, I1, I2, I12, !ModuleInfo),
            inst_merge(Type, I12, I3, I123, !ModuleInfo),
            inst_merge(Type, I4, I5, I45, !ModuleInfo),
            inst_merge(Type, I45, I6, I456, !ModuleInfo),
            inst_merge(Type, I123, I456, I123456, !ModuleInfo)
        then
            !:MergedIs = [I123456 | !.MergedIs]
        else
            !:Fail = merge_has_failed
        )
    ;
        Is2Plus = [I2, I3, I4, I5, I6, I7],
        ( if
            inst_merge(Type, I1, I2, I12, !ModuleInfo),
            inst_merge(Type, I3, I4, I34, !ModuleInfo),
            inst_merge(Type, I12, I34, I1234, !ModuleInfo),
            inst_merge(Type, I5, I6, I56, !ModuleInfo),
            inst_merge(Type, I56, I7, I567, !ModuleInfo),
            inst_merge(Type, I1234, I567, I1234567, !ModuleInfo)
        then
            !:MergedIs = [I1234567 | !.MergedIs]
        else
            !:Fail = merge_has_failed
        )
    ;
        Is2Plus = [I2, I3, I4, I5, I6, I7, I8 | Is9Plus],
        ( if
            inst_merge(Type, I1, I2, I12, !ModuleInfo),
            inst_merge(Type, I3, I4, I34, !ModuleInfo),
            inst_merge(Type, I12, I34, I1234, !ModuleInfo),
            inst_merge(Type, I5, I6, I56, !ModuleInfo),
            inst_merge(Type, I7, I8, I78, !ModuleInfo),
            inst_merge(Type, I56, I78, I5678, !ModuleInfo),
            inst_merge(Type, I1234, I5678, I12345678, !ModuleInfo)
        then
            !:MergedIs = [I12345678 | !.MergedIs],
            (
                Is9Plus = []
            ;
                Is9Plus = [I9 | Is10Plus],
                insts_merge_pass(Type, I9, Is10Plus,
                    !MergedIs, !Fail, !ModuleInfo)
            )
        else
            !:Fail = merge_has_failed
        )
    ).

%---------------------------------------------------------------------------%

inst_merge(Type, InstA, InstB, InstAB, !ModuleInfo) :-
    % The merge_inst_table has two functions. One is to act as a cache,
    % in the expectation that just looking up InstAB would be quicker than
    % computing it. The other is to ensure termination for situations
    % in which one or both of InstA and InstB are recursive.
    %
    % In cases where both InstA and InstB are bound/3, the merge_inst_table
    % does not work as a cache: actually doing merging the insts is likely
    % to be faster (and maybe *much* faster) than looking them up
    % in the merge_inst_table. And in such cases, the table is not needed
    % for termination either. Since the skeleton of the bound_functor list
    % does not contain any inst_names, any recursion has to be in the list
    % elements, and will be caught and handled there.
    ( if
        InstA = bound(_, _, _),
        InstB = bound(_, _, _)
    then
        % The invocations of inst_expand in inst_merge_2 would do nothing
        % for bound insts, and after that, inst_merge_2 would call
        % inst_merge_3 anyway.
        inst_merge_3(Type, InstA, InstB, InstAB, !ModuleInfo)
    else
        % Check whether this pair of insts is already in the merge_insts table.
        module_info_get_inst_table(!.ModuleInfo, InstTable0),
        inst_table_get_merge_insts(InstTable0, MergeInstTable0),
        MergeInstInfo = merge_inst_info(InstA, InstB),
        MergeInstName = merge_inst(InstA, InstB),
        search_insert_unknown_merge_inst(MergeInstInfo,
            MaybeOldMaybeMergedInst, MergeInstTable0, MergeInstTable1),
        (
            MaybeOldMaybeMergedInst = yes(OldMaybeMergedInst),
            (
                OldMaybeMergedInst = inst_known(InstAB0)
            ;
                OldMaybeMergedInst = inst_unknown,
                InstAB0 = defined_inst(MergeInstName)
            )
        ;
            MaybeOldMaybeMergedInst = no,
            % We have inserted MergeInst into the table with value
            % `inst_unknown'.
            inst_table_set_merge_insts(MergeInstTable1,
                InstTable0, InstTable1),
            module_info_set_inst_table(InstTable1, !ModuleInfo),

            % Merge the insts.
            inst_merge_2(Type, InstA, InstB, InstAB0, !ModuleInfo),

            % Now update the value associated with ThisInstPair.
            module_info_get_inst_table(!.ModuleInfo, InstTable2),
            inst_table_get_merge_insts(InstTable2, MergeInstTable2),
            det_update_merge_inst(MergeInstInfo, inst_known(InstAB0),
                MergeInstTable2, MergeInstTable3),
            inst_table_set_merge_insts(MergeInstTable3,
                InstTable2, InstTable3),
            module_info_set_inst_table(InstTable3, !ModuleInfo)
        ),
        % Avoid expanding recursive insts.
        ( if inst_contains_inst_name(!.ModuleInfo, MergeInstName, InstAB0) then
            InstAB = defined_inst(MergeInstName)
        else
            InstAB = InstAB0
        )
    ).

:- pred inst_merge_2(mer_type::in, mer_inst::in, mer_inst::in, mer_inst::out,
    module_info::in, module_info::out) is semidet.

inst_merge_2(Type, InstA, InstB, InstAB, !ModuleInfo) :-
%   % XXX Would this test improve efficiency?
%   % What if we compared the addresses?
%   ( if InstA = InstB then
%       Inst = InstA,
%   else
    inst_expand(!.ModuleInfo, InstA, ExpandedInstA),
    inst_expand(!.ModuleInfo, InstB, ExpandedInstB),
    require_complete_switch [ExpandedInstA]
    (
        ExpandedInstA = not_reached,
        InstAB = ExpandedInstB
    ;
        ExpandedInstA = inst_var(_),
        % If ExpandedInstA = ExpandedInstB, we could succeed with
        % InstAB = ExpandedInstA, though that would amount to a de-facto
        % change in the language.
        fail
    ;
        ( ExpandedInstA = ground(_, _)
        ; ExpandedInstA = free
        ; ExpandedInstA = bound(_, _, _)
        ; ExpandedInstA = any(_, _)
        ; ExpandedInstA = constrained_inst_vars(_, _)
        ),
        require_complete_switch [ExpandedInstB]
        (
            ExpandedInstB = not_reached,
            InstAB = ExpandedInstA
        ;
            ExpandedInstB = inst_var(_),
            fail
        ;
            ( ExpandedInstB = ground(_, _)
            ; ExpandedInstB = free
            ; ExpandedInstB = bound(_, _, _)
            ; ExpandedInstB = any(_, _)
            ; ExpandedInstB = constrained_inst_vars(_, _)
            ),
            inst_merge_3(Type, ExpandedInstA, ExpandedInstB, InstAB,
                !ModuleInfo)
        )
    ).

:- inst inst_merge_3_input for mer_inst/0
    --->        ground(ground, ground)
    ;           free
    ;           bound(ground, ground, ground)
    ;           constrained_inst_vars(ground, ground)
    ;           any(ground, ground).

:- pred inst_merge_3(mer_type::in,
    mer_inst::in(inst_merge_3_input), mer_inst::in(inst_merge_3_input),
    mer_inst::out, module_info::in, module_info::out) is semidet.
:- pragma inline(pred(inst_merge_3/6)).

inst_merge_3(Type, InstA, InstB, InstAB, !ModuleInfo) :-
    require_complete_switch [InstA]
    (
        InstA = constrained_inst_vars(InstVarsA, SubInstA),
        require_complete_switch [InstB]
        (
            InstB = constrained_inst_vars(InstVarsB, SubInstB),
            inst_merge(Type, SubInstA, SubInstB, InstAB0, !ModuleInfo),
            set.intersect(InstVarsA, InstVarsB, InstVars),
            ( if set.is_non_empty(InstVars) then
                InstAB = constrained_inst_vars(InstVars, InstAB0)
                % We can keep the constrained_inst_vars here since
                % Inst0 = SubInstA `lub` SubInstB and the original constraint
                % on the InstVars, InstAB, must have been such that
                % SubInstA `lub` SubInstB =< InstAB.
            else
                InstAB = InstAB0
            )
        ;
            ( InstB = ground(_, _)
            ; InstB = free
            ; InstB = bound(_, _, _)
            ; InstB = any(_, _)
            ),
            inst_merge(Type, SubInstA, InstB, InstAB, !ModuleInfo)
        )
    ;
        ( InstA = ground(_, _)
        ; InstA = free
        ; InstA = bound(_, _, _)
        ; InstA = any(_, _)
        ),
        require_complete_switch [InstB]
        (
            InstB = constrained_inst_vars(_InstVarsB, SubInstB),
            % InstA \= constrained_inst_vars(_, _) is equivalent to
            % constrained_inst_vars(InstVarsA, InstA) where InstVarsA = empty.
            inst_merge(Type, InstA, SubInstB, InstAB, !ModuleInfo)
        ;
            ( InstB = ground(_, _)
            ; InstB = free
            ; InstB = bound(_, _, _)
            ; InstB = any(_, _)
            ),
            inst_merge_4(Type, InstA, InstB, InstAB, !ModuleInfo)
        )
    ).

:- inst inst_merge_4_input for mer_inst/0
    --->        ground(ground, ground)
    ;           free
    ;           bound(ground, ground, ground)
    ;           any(ground, ground).

:- pred inst_merge_4(mer_type::in,
    mer_inst::in(inst_merge_4_input), mer_inst::in(inst_merge_4_input),
    mer_inst::out, module_info::in, module_info::out) is semidet.
:- pragma inline(pred(inst_merge_4/6)).

inst_merge_4(Type, InstA, InstB, InstAB, !ModuleInfo) :-
    % We do not yet allow merging of `free' and `any', except in the case
    % where the any is `mostly_clobbered_any' or `clobbered_any', because
    % that would require inserting additional code to initialize the free var.
    %
    % We do NOT plan to allow merging of `free' and `ground' to produce `any',
    % because that would introduce `any' insts even for builtin types such as
    % `int' which can't support `any'. It would also make the mode system
    % too weak -- it might not be able to detect bugs as well as it can
    % currently.
    %
    % We process four kinds of insts: free, ground, any, and bound.
    % In both the top-level switch and in the nested switches,
    % we process them in this order.
    %
    % If we are merging free with ground or bound, regardless of
    % which is InstA and which is InstB, then two of the branches
    % of the branched control structure whose final insts we are merging
    % disagree about whether the variable whose insts we are processing
    % is bound by the whole control structure or not. In such cases,
    % we fail, to tell our ancestors this fact.
    require_complete_switch [InstA]
    (
        InstA = free,
        require_complete_switch [InstB]
        (
            InstB = free,
            InstAB = free
        ;
            InstB = any(Uniq, HOInstInfo),
            % We do not yet allow merge of any with free, except for
            % clobbered anys.
            ( Uniq = clobbered ; Uniq = mostly_clobbered ),
            InstAB = any(Uniq, HOInstInfo)
        ;
            ( InstB = ground(_, _)
            ; InstB = bound(_, _, _)
            ),
            fail
        )
    ;
        InstA = ground(UniqA, HOInstInfoA),
        require_complete_switch [InstB]
        (
            InstB = free,
            fail
        ;
            InstB = ground(UniqB, HOInstInfoB),
            merge_ho_inst_info(!.ModuleInfo, Type,
                HOInstInfoA, HOInstInfoB, HOInstInfo),
            merge_uniq(UniqA, UniqB, Uniq),
            InstAB = ground(Uniq, HOInstInfo)
        ;
            InstB = any(UniqB, HOInstInfoB),
            merge_ho_inst_info(!.ModuleInfo, Type,
                HOInstInfoA, HOInstInfoB, HOInstInfo),
            merge_uniq(UniqA, UniqB, Uniq),
            InstAB = any(Uniq, HOInstInfo)
        ;
            InstB = bound(UniqB, InstResultsB, BoundFunctorsB),
            inst_merge_bound_ground(Type, UniqB, InstResultsB, BoundFunctorsB,
                UniqA, InstAB, !ModuleInfo),
            not inst_contains_nondefault_func_mode(!.ModuleInfo, Type, InstB)
        )
    ;
        InstA = any(UniqA, HOInstInfoA),
        require_complete_switch [InstB]
        (
            InstB = free,
            % We do not yet allow merge of any with free, except for
            % clobbered anys.
            ( UniqA = clobbered ; UniqA = mostly_clobbered ),
            InstAB = any(UniqA, HOInstInfoA)
        ;
            InstB = ground(UniqB, HOInstInfoB),
            merge_ho_inst_info(!.ModuleInfo, Type,
                HOInstInfoA, HOInstInfoB, HOInstInfo),
            merge_uniq(UniqA, UniqB, Uniq),
            InstAB = any(Uniq, HOInstInfo)
        ;
            InstB = any(UniqB, HOInstInfoB),
            merge_ho_inst_info(!.ModuleInfo, Type,
                HOInstInfoA, HOInstInfoB, HOInstInfo),
            merge_uniq(UniqA, UniqB, Uniq),
            InstAB = any(Uniq, HOInstInfo)
        ;
            InstB = bound(UniqB, InstResultsB, BoundFunctorsB),
            merge_uniq_bound(!.ModuleInfo, UniqA, UniqB, BoundFunctorsB, Uniq),
            % We do not yet allow merge of any with free, except for
            % clobbered anys.
            ( if ( Uniq = clobbered ; Uniq = mostly_clobbered ) then
                true
            else
                % XXX We will lose any nondefault higher-order info in
                % BoundFunctorsB. We should at least check that there isn't any
                % such info, as the result may be treated as default.
                inst_results_bound_functor_list_is_ground_or_any(!.ModuleInfo,
                    InstResultsB, BoundFunctorsB)
            ),
            InstAB = any(Uniq, none_or_default_func)
        )
    ;
        InstA = bound(UniqA, InstResultsA, BoundFunctorsA),
        require_complete_switch [InstB]
        (
            InstB = free,
            fail
        ;
            InstB = ground(UniqB, _),
            inst_merge_bound_ground(Type, UniqA, InstResultsA, BoundFunctorsA,
                UniqB, InstAB, !ModuleInfo),
            not inst_contains_nondefault_func_mode(!.ModuleInfo, Type, InstA)
        ;
            InstB = any(UniqB, _),
            merge_uniq_bound(!.ModuleInfo, UniqB, UniqA, BoundFunctorsA, Uniq),
            % We do not yet allow merge of any with free, except
            % for clobbered anys.
            ( if ( Uniq = clobbered ; Uniq = mostly_clobbered ) then
                true
            else
                % XXX We will lose any nondefault higher-order info in
                % BoundFunctorsA. We should at least check that there isn't any
                % such info, as the result may be treated as default.
                inst_results_bound_functor_list_is_ground_or_any(!.ModuleInfo,
                    InstResultsA, BoundFunctorsA)
            ),
            InstAB = any(Uniq, none_or_default_func)
        ;
            InstB = bound(UniqB, _InstResultsB, BoundFunctorsB),
            merge_uniq(UniqA, UniqB, Uniq),
            bound_functor_list_merge(Type, BoundFunctorsA, BoundFunctorsB,
                BoundFunctorsAB, !ModuleInfo),
            % XXX A better approximation of InstResults is probably possible.
            InstAB = bound(Uniq, inst_test_no_results, BoundFunctorsAB)
        )
    ).

    % merge_uniq(A, B, C) succeeds if C is minimum of A and B in the ordering
    % clobbered < mostly_clobbered < shared < mostly_unique < unique.
    %
:- pred merge_uniq(uniqueness::in, uniqueness::in, uniqueness::out) is det.

merge_uniq(UniqA, UniqB, Merged) :-
    ( if unique_matches_initial(UniqA, UniqB) then       % A >= B
        Merged = UniqB
    else
        Merged = UniqA
    ).

:- pred merge_ho_inst_info(module_info::in, mer_type::in,
    ho_inst_info::in, ho_inst_info::in, ho_inst_info::out) is semidet.

merge_ho_inst_info(ModuleInfo, Type, HOInstInfoA, HOInstInfoB, HOInstInfo) :-
    ( if
        HOInstInfoA = higher_order(PredA),
        HOInstInfoB = higher_order(PredB)
    then
        % If they specify matching pred insts, but one is more precise
        % (specifies more info) than the other, then we want to choose
        % the least precise one.
        ( if pred_inst_matches(ModuleInfo, Type, PredA, PredB) then
            HOInstInfo = higher_order(PredB)
        else if pred_inst_matches(ModuleInfo, Type, PredB, PredA) then
            HOInstInfo = higher_order(PredA)
        else
            % If either is a function inst with non-default modes,
            % don't allow the higher-order information to be lost.
            pred_inst_matches_ground(ModuleInfo, Type, PredA),
            pred_inst_matches_ground(ModuleInfo, Type, PredB),
            HOInstInfo = none_or_default_func
        )
    else
        ho_inst_info_matches_ground(ModuleInfo, Type, HOInstInfoA),
        ho_inst_info_matches_ground(ModuleInfo, Type, HOInstInfoB),
        HOInstInfo = none_or_default_func
    ).

    % merge_uniq_bound(UniqA, UniqB, BoundFunctorsB, ModuleInfo, Uniq) succeeds
    % iff Uniq is the result of merging.
    %
:- pred merge_uniq_bound(module_info::in, uniqueness::in, uniqueness::in,
    list(bound_functor)::in, uniqueness::out) is det.

merge_uniq_bound(ModuleInfo, UniqA, UniqB, BoundFunctorsB, Uniq) :-
    merge_uniq(UniqA, UniqB, Uniq0),
    set.init(Expansions0),
    merge_bound_functor_list_uniq(ModuleInfo, BoundFunctorsB, Uniq0,
        Expansions0, _Expansions, Uniq).

:- pred merge_bound_functor_list_uniq(module_info::in, list(bound_functor)::in,
    uniqueness::in, set(inst_name)::in, set(inst_name)::out,
    uniqueness::out) is det.

merge_bound_functor_list_uniq(_, [], Uniq, !Expansions, Uniq).
merge_bound_functor_list_uniq(ModuleInfo, [BoundFunctor | BoundFunctors],
        Uniq0, !Expansions, Uniq) :-
    BoundFunctor = bound_functor(_ConsId, ArgInsts),
    merge_inst_list_uniq(ModuleInfo, ArgInsts, Uniq0, !Expansions, Uniq1),
    merge_bound_functor_list_uniq(ModuleInfo, BoundFunctors,
        Uniq1, !Expansions, Uniq).

:- pred merge_inst_list_uniq(module_info::in, list(mer_inst)::in,
    uniqueness::in, set(inst_name)::in, set(inst_name)::out, uniqueness::out)
    is det.

merge_inst_list_uniq(_, [], Uniq, !Expansions, Uniq).
merge_inst_list_uniq(ModuleInfo, [Inst | Insts], Uniq0, !Expansions, Uniq) :-
    merge_inst_uniq(ModuleInfo, Inst, Uniq0, !Expansions, Uniq1),
    merge_inst_list_uniq(ModuleInfo, Insts, Uniq1, !Expansions, Uniq).

:- pred merge_inst_uniq(module_info::in, mer_inst::in, uniqueness::in,
    set(inst_name)::in, set(inst_name)::out, uniqueness::out) is det.

merge_inst_uniq(ModuleInfo, InstA, UniqB, !Expansions, Uniq) :-
    (
        ( InstA = free
        ; InstA = not_reached
        ),
        Uniq = UniqB
    ;
        ( InstA = ground(UniqA, _)
        ; InstA = any(UniqA, _)
        ),
        merge_uniq(UniqA, UniqB, Uniq)
    ;
        InstA = bound(UniqA, _InstResultsA, BoundFunctorsA),
        merge_uniq(UniqA, UniqB, Uniq0),
        merge_bound_functor_list_uniq(ModuleInfo, BoundFunctorsA, Uniq0,
            !Expansions, Uniq)
    ;
        InstA = defined_inst(InstName),
        ( if set.member(InstName, !.Expansions) then
            Uniq = UniqB
        else
            set.insert(InstName, !Expansions),
            inst_lookup(ModuleInfo, InstName, Inst),
            merge_inst_uniq(ModuleInfo, Inst, UniqB, !Expansions, Uniq)
        )
    ;
        InstA = inst_var(_),
        unexpected($pred, "inst_var")
    ;
        InstA = constrained_inst_vars(_InstVars, SubInstA),
        merge_inst_uniq(ModuleInfo, SubInstA, UniqB, !Expansions, Uniq)
    ).

%---------------------------------------------------------------------------%

:- pred inst_merge_bound_ground(mer_type::in, uniqueness::in,
    inst_test_results::in, list(bound_functor)::in, uniqueness::in,
    mer_inst::out, module_info::in, module_info::out) is semidet.

inst_merge_bound_ground(Type, UniqA, InstResultsA, BoundFunctorsA, UniqB,
        Result, !ModuleInfo) :-
    ( if
        inst_results_bound_functor_list_is_ground(!.ModuleInfo, Type,
            InstResultsA, BoundFunctorsA)
    then
        merge_uniq_bound(!.ModuleInfo, UniqB, UniqA, BoundFunctorsA, Uniq),
        Result = ground(Uniq, none_or_default_func)
    else
        inst_results_bound_functor_list_is_ground_or_any(!.ModuleInfo,
            InstResultsA, BoundFunctorsA),
        % If we know the type, we can give a more accurate result than
        % just "any".
        ( if type_constructors(!.ModuleInfo, Type, Constructors) then
            type_to_ctor_det(Type, TypeCtor),
            constructors_to_bound_functors(!.ModuleInfo, UniqB, TypeCtor,
                Constructors, BoundFunctorsB0),
            list.sort_and_remove_dups(BoundFunctorsB0, BoundFunctorsB),
            InstResultsB = inst_test_results(
                inst_result_is_ground,
                inst_result_does_not_contain_any,
                inst_result_contains_inst_names_known(set.init),
                inst_result_contains_inst_vars_known(set.init),
                inst_result_contains_types_known(set.init),
                inst_result_type_ctor_propagated(TypeCtor)
            ),
            InstA = bound(UniqA, InstResultsA, BoundFunctorsA),
            InstB = bound(UniqB, InstResultsB, BoundFunctorsB),
            inst_merge_4(Type, InstA, InstB, Result, !ModuleInfo)
        else
            merge_uniq_bound(!.ModuleInfo, UniqB, UniqA, BoundFunctorsA, Uniq),
            Result = any(Uniq, none_or_default_func)
        )
    ).

%---------------------------------------------------------------------------%

:- pred inst_list_merge(list(mer_type)::in,
    list(mer_inst)::in, list(mer_inst)::in, list(mer_inst)::out,
    module_info::in, module_info::out) is semidet.

inst_list_merge(_, [], [], [], !ModuleInfo).
inst_list_merge([Type | Types], [ArgInstA | ArgInstsA], [ArgInstB | ArgInstsB],
        [ArgInstAB | ArgInstsAB], !ModuleInfo) :-
    inst_merge(Type, ArgInstA, ArgInstB, ArgInstAB, !ModuleInfo),
    inst_list_merge(Types, ArgInstsA, ArgInstsB, ArgInstsAB, !ModuleInfo).

    % bound_functor_list_merge(Type, BoundFunctorsA, BoundFunctorsB,
    %   BoundFunctors, !ModuleInfo):
    %
    % The two input lists BoundFunctorsA and BoundFunctorsB must already
    % be sorted. Here we perform a sorted merge operation, so that
    % the functors of the output list BoundFunctors are the union of the
    % functors of the input lists BoundFunctorsA and BoundFunctorsB.
    %
:- pred bound_functor_list_merge(mer_type::in,
    list(bound_functor)::in, list(bound_functor)::in, list(bound_functor)::out,
    module_info::in, module_info::out) is semidet.

bound_functor_list_merge(Type, BoundFunctorsA, BoundFunctorsB, BoundFunctorsAB,
        !ModuleInfo) :-
    (
        BoundFunctorsA = [],
        BoundFunctorsAB = BoundFunctorsB
    ;
        BoundFunctorsA = [_ | _],
        BoundFunctorsB = [],
        BoundFunctorsAB = BoundFunctorsA
    ;
        BoundFunctorsA = [BoundFunctorA | BoundFunctorsTailA],
        BoundFunctorsB = [BoundFunctorB | BoundFunctorsTailB],
        BoundFunctorA = bound_functor(ConsIdA, ArgInstsA),
        BoundFunctorB = bound_functor(ConsIdB, ArgInstsB),
        ( if equivalent_cons_ids(ConsIdA, ConsIdB) then
            get_cons_id_arg_types_for_bound_functor(!.ModuleInfo, Type,
                BoundFunctorA, Types),
            inst_list_merge(Types, ArgInstsA, ArgInstsB, ArgInstsAB,
                !ModuleInfo),
            BoundFunctor = bound_functor(ConsIdA, ArgInstsAB),
            bound_functor_list_merge(Type,
                BoundFunctorsTailA, BoundFunctorsTailB,
                BoundFunctorsABTail, !ModuleInfo),
            BoundFunctorsAB = [BoundFunctor | BoundFunctorsABTail]
        else if compare(<, ConsIdA, ConsIdB) then
            bound_functor_list_merge(Type, BoundFunctorsTailA, BoundFunctorsB,
                BoundFunctorsABTail, !ModuleInfo),
            BoundFunctorsAB = [BoundFunctorA | BoundFunctorsABTail]
        else
            bound_functor_list_merge(Type, BoundFunctorsA, BoundFunctorsTailB,
                BoundFunctorsABTail, !ModuleInfo),
            BoundFunctorsAB = [BoundFunctorB | BoundFunctorsABTail]
        )
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.inst_merge.
%---------------------------------------------------------------------------%
