%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
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

%---------------------------------------------------------------------------%

    % inst_merge(InstA, InstB, Type, InstC, !ModuleInfo):
    %
    % Combine the insts found in different arms of a disjunction, switch, or
    % if-then-else. The information in InstC is the minimum of the information
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
:- pred inst_merge(mer_inst::in, mer_inst::in, mer_type::in,
    mer_inst::out, module_info::in, module_info::out) is semidet.

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
:- import_module mdbcomp.
:- import_module parse_tree.prog_type.

:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module set.

%---------------------------------------------------------------------------%

inst_merge(InstA, InstB, Type, Inst, !ModuleInfo) :-
    % The merge_inst_table has two functions. One is to act as a cache,
    % in the expectation that just looking up Inst would be quicker than
    % computing it. The other is to ensure termination for situations
    % in which one or both of InstA and InstB are recursive.
    %
    % In cases where both InstA and InstB are bound/3, the merge_inst_table
    % does not work as a cache: actually doing merging the insts is likely
    % to be faster (and maybe *much* faster) than looking them up
    % in the merge_inst_table. And in such cases, the table is not needed
    % for termination either. Since the skeleton of the bound_inst list
    % does not contain any inst_names, any recursion has to be in the list
    % elements, and will be caught and handled there.
    ( if
        InstA = bound(_, _, _),
        InstB = bound(_, _, _)
    then
        inst_merge_2(InstA, InstB, Type, Inst, !ModuleInfo)
    else
        % Check whether this pair of insts is already in the merge_insts table.
        module_info_get_inst_table(!.ModuleInfo, InstTable0),
        inst_table_get_merge_insts(InstTable0, MergeInstTable0),
        MergeInstInfo = merge_inst_info(InstA, InstB),
        MergeInstName = merge_inst(InstA, InstB),
        search_insert_merge_inst(MergeInstInfo, MaybeMaybeMergedInst,
            MergeInstTable0, MergeInstTable1),
        (
            MaybeMaybeMergedInst = yes(MaybeMergedInst),
            (
                MaybeMergedInst = inst_known(Inst0)
            ;
                MaybeMergedInst = inst_unknown,
                Inst0 = defined_inst(MergeInstName)
            )
        ;
            MaybeMaybeMergedInst = no,
            % We have inserted MergeInst into the table with value
            % `inst_unknown'.
            inst_table_set_merge_insts(MergeInstTable1,
                InstTable0, InstTable1),
            module_info_set_inst_table(InstTable1, !ModuleInfo),

            % Merge the insts.
            inst_merge_2(InstA, InstB, Type, Inst0, !ModuleInfo),

            % Now update the value associated with ThisInstPair.
            module_info_get_inst_table(!.ModuleInfo, InstTable2),
            inst_table_get_merge_insts(InstTable2, MergeInstTable2),
            det_update_merge_inst(MergeInstInfo, inst_known(Inst0),
                MergeInstTable2, MergeInstTable3),
            inst_table_set_merge_insts(MergeInstTable3,
                InstTable2, InstTable3),
            module_info_set_inst_table(InstTable3, !ModuleInfo)
        ),
        % Avoid expanding recursive insts.
        ( if inst_contains_inst_name(!.ModuleInfo, MergeInstName, Inst0) then
            Inst = defined_inst(MergeInstName)
        else
            Inst = Inst0
        )
    ).

:- pred inst_merge_2(mer_inst::in, mer_inst::in, mer_type::in,
    mer_inst::out, module_info::in, module_info::out) is semidet.

inst_merge_2(InstA, InstB, Type, Inst, !ModuleInfo) :-
%   % XXX Would this test improve efficiency?
%   % What if we compared the addresses?
%   ( if InstA = InstB then
%       Inst = InstA,
%   else
    inst_expand(!.ModuleInfo, InstA, ExpandedInstA),
    inst_expand(!.ModuleInfo, InstB, ExpandedInstB),
    ( if ExpandedInstB = not_reached then
        Inst = ExpandedInstA
    else if ExpandedInstA = not_reached then
        Inst = ExpandedInstB
    else
        inst_merge_3(ExpandedInstA, ExpandedInstB, Type, Inst, !ModuleInfo)
    ).

:- pred inst_merge_3(mer_inst::in, mer_inst::in, mer_type::in,
    mer_inst::out, module_info::in, module_info::out) is semidet.

inst_merge_3(InstA, InstB, Type, Inst, !ModuleInfo) :-
    ( if InstA = constrained_inst_vars(InstVarsA, SubInstA) then
        ( if InstB = constrained_inst_vars(InstVarsB, SubInstB) then
            inst_merge(SubInstA, SubInstB, Type, Inst0, !ModuleInfo),
            set.intersect(InstVarsA, InstVarsB, InstVars),
            ( if set.is_non_empty(InstVars) then
                Inst = constrained_inst_vars(InstVars, Inst0)
                % We can keep the constrained_inst_vars here since
                % Inst0 = SubInstA `lub` SubInstB and the original constraint
                % on the InstVars, InstC, must have been such that
                % SubInstA `lub` SubInstB =< InstC.
            else
                Inst = Inst0
            )
        else
            inst_merge(SubInstA, InstB, Type, Inst, !ModuleInfo)
        )
    else if InstB = constrained_inst_vars(_InstVarsB, SubInstB) then
        % InstA \= constrained_inst_vars(_, _) is equivalent to
        % constrained_inst_vars(InstVarsA, InstA) where InstVarsA = empty.
        inst_merge(InstA, SubInstB, Type, Inst, !ModuleInfo)
    else
        inst_merge_4(InstA, InstB, Type, Inst, !ModuleInfo)
    ).

:- pred inst_merge_4(mer_inst::in, mer_inst::in, mer_type::in,
    mer_inst::out, module_info::in, module_info::out) is semidet.

inst_merge_4(InstA, InstB, Type, Inst, !ModuleInfo) :-
    % We do not yet allow merging of `free' and `any', except in the case
    % where the any is `mostly_clobbered_any' or `clobbered_any', because
    % that would require inserting additional code to initialize the free var.
    %
    % We do NOT plan to allow merging of `free' and `ground' to produce `any',
    % because that would introduce `any' insts even for builtin types such as
    % `int' which can't support `any'. It might also make the mode system
    % too weak -- it might not be able to detect bugs as well as it can
    % currently.

    (
        InstA = any(UniqA, HOInstInfoA),
        InstB = any(UniqB, HOInstInfoB),
        merge_ho_inst_info(HOInstInfoA, HOInstInfoB, HOInstInfo, !ModuleInfo),
        merge_uniq(UniqA, UniqB, Uniq),
        Inst = any(Uniq, HOInstInfo)
    ;
        InstA = any(Uniq, HOInstInfo),
        InstB = free,
        % We do not yet allow merge of any with free, except for
        % clobbered anys.
        ( Uniq = clobbered ; Uniq = mostly_clobbered ),
        Inst = any(Uniq, HOInstInfo)
    ;
        InstA = any(UniqA, _),
        InstB = bound(UniqB, InstResultsB, BoundInstsB),
        merge_uniq_bound(!.ModuleInfo, UniqA, UniqB, BoundInstsB, Uniq),
        % We do not yet allow merge of any with free, except for
        % clobbered anys.
        ( if ( Uniq = clobbered ; Uniq = mostly_clobbered ) then
            true
        else
            % XXX We will lose any nondefault higher-order info in
            % BoundInstsB. We should at least check that there isn't any
            % such info, as the result may be treated as default.
            inst_results_bound_inst_list_is_ground_or_any(!.ModuleInfo,
                InstResultsB, BoundInstsB)
        ),
        Inst = any(Uniq, none_or_default_func)
    ;
        InstA = any(UniqA, HOInstInfoA),
        InstB = ground(UniqB, HOInstInfoB),
        merge_ho_inst_info(HOInstInfoA, HOInstInfoB, HOInstInfo, !ModuleInfo),
        merge_uniq(UniqA, UniqB, Uniq),
        Inst = any(Uniq, HOInstInfo)
    ;
        InstA = any(UniqA, _),
        InstB = abstract_inst(_, _),
        merge_uniq(UniqA, shared, Uniq),
        % We do not yet allow merge of any with free, except for
        % clobbered anys.
        ( Uniq = clobbered ; Uniq = mostly_clobbered ),
        Inst = any(Uniq, none_or_default_func)
    ;
        InstA = free,
        InstB = any(Uniq, HOInstInfo),
        % We do not yet allow merge of any with free, except for
        % clobbered anys.
        ( Uniq = clobbered ; Uniq = mostly_clobbered ),
        Inst = any(Uniq, HOInstInfo)
    ;
        InstA = bound(UniqA, InstResultsA, BoundInstsA),
        InstB = any(UniqB, _),
        merge_uniq_bound(!.ModuleInfo, UniqB, UniqA, BoundInstsA, Uniq),
        % We do not yet allow merge of any with free, except
        % for clobbered anys.
        ( if ( Uniq = clobbered ; Uniq = mostly_clobbered ) then
            true
        else
            % XXX We will lose any nondefault higher-order info in
            % BoundInstsA. We should at least check that there isn't any
            % such info, as the result may be treated as default.
            inst_results_bound_inst_list_is_ground_or_any(!.ModuleInfo,
                InstResultsA, BoundInstsA)
        ),
        Inst = any(Uniq, none_or_default_func)
    ;
        InstA = ground(UniqA, HOInstInfoA),
        InstB = any(UniqB, HOInstInfoB),
        merge_ho_inst_info(HOInstInfoA, HOInstInfoB, HOInstInfo, !ModuleInfo),
        merge_uniq(UniqA, UniqB, Uniq),
        Inst = any(Uniq, HOInstInfo)
    ;
        InstA = abstract_inst(_, _),
        InstB = any(UniqB, _),
        merge_uniq(shared, UniqB, Uniq),
        % We do not yet allow merge of any with free, except for
        % clobbered anys.
        ( Uniq = clobbered ; Uniq = mostly_clobbered ),
        Inst = any(Uniq, none_or_default_func)
    ;
        InstA = free,
        InstB = free,
        Inst = free
    ;
        InstA = bound(UniqA, _InstResultsA, BoundInstsA),
        InstB = bound(UniqB, _InstResultsB, BoundInstsB),
        merge_uniq(UniqA, UniqB, Uniq),
        bound_inst_list_merge(BoundInstsA, BoundInstsB, Type, BoundInsts,
            !ModuleInfo),
        % XXX A better approximation of InstResults is probably possible.
        Inst = bound(Uniq, inst_test_no_results, BoundInsts)
    ;
        InstA = bound(UniqA, InstResultsA, BoundInstsA),
        InstB = ground(UniqB, _),
        inst_merge_bound_ground(UniqA, InstResultsA, BoundInstsA, UniqB,
            Type, Inst, !ModuleInfo),
        not inst_contains_nondefault_func_mode(!.ModuleInfo, InstA)
    ;
        InstA = ground(UniqA, _),
        InstB = bound(UniqB, InstResultsB, BoundInstsB),
        inst_merge_bound_ground(UniqB, InstResultsB, BoundInstsB, UniqA,
            Type, Inst, !ModuleInfo),
        not inst_contains_nondefault_func_mode(!.ModuleInfo, InstB)
    ;
        InstA = ground(UniqA, HOInstInfoA),
        InstB = ground(UniqB, HOInstInfoB),
        merge_ho_inst_info(HOInstInfoA, HOInstInfoB, HOInstInfo, !ModuleInfo),
        merge_uniq(UniqA, UniqB, Uniq),
        Inst = ground(Uniq, HOInstInfo)
    ;
        InstA = abstract_inst(Name, ArgsA),
        InstB = abstract_inst(Name, ArgsB),
        % We don't know the arguments types of an abstract inst.
        Types = list.duplicate(list.length(ArgsA), no_type_available),
        inst_list_merge(ArgsA, ArgsB, Types, Args, !ModuleInfo),
        Inst = abstract_inst(Name, Args)
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

:- pred merge_ho_inst_info(ho_inst_info::in, ho_inst_info::in,
    ho_inst_info::out, module_info::in, module_info::out) is semidet.

merge_ho_inst_info(HOInstInfoA, HOInstInfoB, HOInstInfo, !ModuleInfo) :-
    ( if
        HOInstInfoA = higher_order(PredA),
        HOInstInfoB = higher_order(PredB)
    then
        % If they specify matching pred insts, but one is more precise
        % (specifies more info) than the other, then we want to choose
        % the least precise one.
        ( if pred_inst_matches(!.ModuleInfo, PredA, PredB) then
            HOInstInfo = higher_order(PredB)
        else if pred_inst_matches(!.ModuleInfo, PredB, PredA) then
            HOInstInfo = higher_order(PredA)
        else
            % If either is a function inst with non-default modes,
            % don't allow the higher-order information to be lost.
            pred_inst_matches_ground(!.ModuleInfo, PredA),
            pred_inst_matches_ground(!.ModuleInfo, PredB),
            HOInstInfo = none_or_default_func
        )
    else
        ho_inst_info_matches_ground(!.ModuleInfo, HOInstInfoA),
        ho_inst_info_matches_ground(!.ModuleInfo, HOInstInfoB),
        HOInstInfo = none_or_default_func
    ).

    % merge_uniq_bound(UniqA, UniqB, BoundInstsB, ModuleInfo, Uniq) succeeds
    % iff Uniq is the result of merging.
    %
:- pred merge_uniq_bound(module_info::in, uniqueness::in, uniqueness::in,
    list(bound_inst)::in, uniqueness::out) is det.

merge_uniq_bound(ModuleInfo, UniqA, UniqB, BoundInstsB, Uniq) :-
    merge_uniq(UniqA, UniqB, Uniq0),
    set.init(Expansions0),
    merge_bound_inst_list_uniq(ModuleInfo, BoundInstsB, Uniq0,
        Expansions0, _Expansions, Uniq).

:- pred merge_bound_inst_list_uniq(module_info::in, list(bound_inst)::in,
    uniqueness::in, set(inst_name)::in, set(inst_name)::out,
    uniqueness::out) is det.

merge_bound_inst_list_uniq(_, [], Uniq, !Expansions, Uniq).
merge_bound_inst_list_uniq(ModuleInfo, [BoundInst | BoundInsts], Uniq0,
        !Expansions, Uniq) :-
    BoundInst = bound_functor(_ConsId, ArgInsts),
    merge_inst_list_uniq(ModuleInfo, ArgInsts, Uniq0, !Expansions, Uniq1),
    merge_bound_inst_list_uniq(ModuleInfo, BoundInsts, Uniq1,
        !Expansions, Uniq).

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
        ; InstA = free(_)
        ; InstA = not_reached
        ),
        Uniq = UniqB
    ;
        ( InstA = ground(UniqA, _)
        ; InstA = any(UniqA, _)
        ),
        merge_uniq(UniqA, UniqB, Uniq)
    ;
        InstA = abstract_inst(_, _),
        merge_uniq(shared, UniqB, Uniq)
    ;
        InstA = bound(UniqA, _InstResultsA, BoundInstsA),
        merge_uniq(UniqA, UniqB, Uniq0),
        merge_bound_inst_list_uniq(ModuleInfo, BoundInstsA, Uniq0,
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

:- pred inst_merge_bound_ground(uniqueness::in, inst_test_results::in,
    list(bound_inst)::in, uniqueness::in, mer_type::in, mer_inst::out,
    module_info::in, module_info::out) is semidet.

inst_merge_bound_ground(UniqA, InstResultsA, BoundInstsA, UniqB,
        Type, Result, !ModuleInfo) :-
    ( if
        inst_results_bound_inst_list_is_ground(!.ModuleInfo, InstResultsA,
            BoundInstsA)
    then
        merge_uniq_bound(!.ModuleInfo, UniqB, UniqA, BoundInstsA, Uniq),
        Result = ground(Uniq, none_or_default_func)
    else
        inst_results_bound_inst_list_is_ground_or_any(!.ModuleInfo,
            InstResultsA, BoundInstsA),
        % If we know the type, we can give a more accurate result than
        % just "any".
        ( if type_constructors(!.ModuleInfo, Type, Constructors) then
            type_to_ctor_det(Type, TypeCtor),
            constructors_to_bound_insts(!.ModuleInfo, UniqB, TypeCtor,
                Constructors, BoundInstsB0),
            list.sort_and_remove_dups(BoundInstsB0, BoundInstsB),
            InstResultsB = inst_test_results(
                inst_result_is_ground,
                inst_result_does_not_contain_any,
                inst_result_contains_inst_names_known(set.init),
                inst_result_contains_inst_vars_known(set.init),
                inst_result_contains_types_known(set.init),
                inst_result_type_ctor_propagated(TypeCtor)
            ),
            InstA = bound(UniqA, InstResultsA, BoundInstsA),
            InstB = bound(UniqB, InstResultsB, BoundInstsB),
            inst_merge_4(InstA, InstB, Type, Result, !ModuleInfo)
        else
            merge_uniq_bound(!.ModuleInfo, UniqB, UniqA, BoundInstsA, Uniq),
            Result = any(Uniq, none_or_default_func)
        )
    ).

%---------------------------------------------------------------------------%

:- pred inst_list_merge(list(mer_inst)::in, list(mer_inst)::in,
    list(mer_type)::in, list(mer_inst)::out,
    module_info::in, module_info::out) is semidet.

inst_list_merge([], [], _, [], !ModuleInfo).
inst_list_merge([ArgA | ArgsA], [ArgB | ArgsB], [Type | Types],
        [Arg | Args], !ModuleInfo) :-
    inst_merge(ArgA, ArgB, Type, Arg, !ModuleInfo),
    inst_list_merge(ArgsA, ArgsB, Types, Args, !ModuleInfo).

    % bound_inst_list_merge(BoundInstsA, BoundInstsB, BoundInsts, Type,
    %   !ModuleInfo):
    %
    % The two input lists BoundInstsA and BoundInstsB must already be sorted.
    % Here we perform a sorted merge operation,
    % so that the functors of the output list BoundInsts are the union
    % of the functors of the input lists BoundInstsA and BoundInstsB.
    %
:- pred bound_inst_list_merge(list(bound_inst)::in, list(bound_inst)::in,
    mer_type::in, list(bound_inst)::out,
    module_info::in, module_info::out) is semidet.

bound_inst_list_merge(BoundInstsA, BoundInstsB, Type, BoundInsts,
        !ModuleInfo) :-
    (
        BoundInstsA = [],
        BoundInsts = BoundInstsB
    ;
        BoundInstsA = [_ | _],
        BoundInstsB = [],
        BoundInsts = BoundInstsA
    ;
        BoundInstsA = [BoundInstA | BoundInstsTailA],
        BoundInstsB = [BoundInstB | BoundInstsTailB],
        BoundInstA = bound_functor(ConsIdA, ArgsA),
        BoundInstB = bound_functor(ConsIdB, ArgsB),
        ( if equivalent_cons_ids(ConsIdA, ConsIdB) then
            get_cons_id_arg_types(!.ModuleInfo, Type,
                ConsIdA, list.length(ArgsA), Types),
            inst_list_merge(ArgsA, ArgsB, Types, Args, !ModuleInfo),
            BoundInst = bound_functor(ConsIdA, Args),
            bound_inst_list_merge(BoundInstsTailA, BoundInstsTailB, Type,
                BoundInstsTail, !ModuleInfo),
            BoundInsts = [BoundInst | BoundInstsTail]
        else if compare(<, ConsIdA, ConsIdB) then
            bound_inst_list_merge(BoundInstsTailA, BoundInstsB, Type,
                BoundInstsTail, !ModuleInfo),
            BoundInsts = [BoundInstA | BoundInstsTail]
        else
            bound_inst_list_merge(BoundInstsA, BoundInstsTailB, Type,
                BoundInstsTail, !ModuleInfo),
            BoundInsts = [BoundInstB | BoundInstsTail]
        )
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.inst_merge.
%---------------------------------------------------------------------------%
