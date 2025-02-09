%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% Copyright (C) 2015, 2021, 2023-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: inst_abstract_unify.m.
% Author: fjh.
%
% This module does abstract unification of insts, which in effect
% does interpretation on terms using insts as the abstraction mechanism.
%
% A limitation is that we don't allow any unifications between functors
% and variables of mode `any'; the reason for that is that I have no
% idea what code we should generate for them. Currently `any' insts
% are only used for abstract types, so the type system should prevent
% any unification between functors and variables of mode `any'.
%
% Another limitation is that currently code generation assumes that insts
% `bound', `ground', and `any' are all represented the same way.
% That works fine for the CLP(R) interface but might not be ideal
% in the general case.
%
%---------------------------------------------------------------------------%

:- module check_hlds.inst_abstract_unify.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % Mode checking is like abstract interpretation. The predicates below
    % define the abstract unification operation which unifies two
    % instantiatednesses. If the unification would be illegal, then abstract
    % unification fails. If the unification would fail, then the abstract
    % unification will succeed, and the resulting instantiatedness will be
    % `not_reached'.

    % Compute the inst that results from abstractly unifying two variables.
    %
:- pred abstractly_unify_inst(mer_type::in, is_live::in, unify_is_real::in,
    mer_inst::in, mer_inst::in, mer_inst::out, determinism::out,
    module_info::in, module_info::out) is semidet.

    % Compute the inst that results from abstractly unifying
    % a variable with a functor.
    %
:- pred abstractly_unify_inst_functor(mer_type::in, is_live::in,
    unify_is_real::in, mer_inst::in,
    cons_id::in, list(mer_inst)::in, list(is_live)::in,
    mer_inst::out, determinism::out,
    module_info::in, module_info::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_lookup.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.modecheck_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_inst_mode.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_detism.
:- import_module parse_tree.prog_type.

:- import_module maybe.
:- import_module require.
:- import_module set.

%---------------------------------------------------------------------------%

abstractly_unify_inst(Type, Live, Real, InstA, InstB, Inst, Detism,
        !ModuleInfo) :-
    % We avoid infinite loops by checking whether the InstA-InstB
    % pair of insts is already in the unify_insts table.
    %
    % XXX For code that uses large facts, the deeply nested insts we unify
    % here means that searching the unify_insts table here, and updating it
    % (twice) in the else case below are *extremely* expensive. In one version
    % of Doug Auclair's training_cars example, the map search, insert and
    % update account for 116 out the 120 clock ticks spent in this predicate,
    % i.e. they account for almost 97% of its runtime.
    %
    % We reduce the expense of these operations in two ways.
    %
    % First, we make each instance of the operation cheaper, by combining
    % the lookup with one of the updates. We do this by having
    % search_insert_unify_inst use map.search_insert.
    %
    % Second, we avoid some instances of the operation altogether.
    %
    % If either inst is free, then just unifying the two insts is likely
    % to be faster (and maybe *much* faster) than looking them up
    % in the unify_inst_table. The other purpose of the unify_inst_table,
    % avoiding nontermination, is also moot in such cases.
    %
    % We could also avoid using the unify_inst_table if both insts are
    % bound/3 insts, as inst_merge below does, but even in stress test cases,
    % abstractly_unify_inst is (almost) never invoked on such inst pairs.
    ( if
        ( InstA = free
        ; InstB = free
        )
    then
        abstractly_unify_inst_2(Type, Live, Real, InstA, InstB,
            Inst, Detism, !ModuleInfo)
    else
        module_info_get_inst_table(!.ModuleInfo, InstTable0),
        inst_table_get_unify_insts(InstTable0, UnifyInstTable0),
        UnifyInstInfo = unify_inst_info(Live, Real, InstA, InstB),
        UnifyInstName = unify_inst(Live, Real, InstA, InstB),
        search_insert_unknown_unify_inst(UnifyInstInfo, MaybeOldMaybeInst,
            UnifyInstTable0, UnifyInstTable1),
        (
            MaybeOldMaybeInst = yes(OldMaybeInst),
            (
                OldMaybeInst = inst_det_known(Inst0, Detism)
            ;
                OldMaybeInst = inst_det_unknown,
                Inst0 = defined_inst(UnifyInstName),
                % It is ok to assume that the unification is deterministic
                % here, because the only time that this will happen is when
                % we get to the recursive case for a recursively defined inst.
                % If the unification as a whole is semidet, then this must be
                % because it is semidet somewhere else too.
                Detism = detism_det
            )
        ;
            MaybeOldMaybeInst = no,
            % We have inserted UnifyInst into the table with value
            % `inst_unknown'.
            inst_table_set_unify_insts(UnifyInstTable1,
                InstTable0, InstTable1),
            module_info_set_inst_table(InstTable1, !ModuleInfo),
            % Unify the insts.
            abstractly_unify_inst_2(Type, Live, Real, InstA, InstB,
                Inst0, Detism, !ModuleInfo),

            % Now update the value associated with ThisInstPair.
            module_info_get_inst_table(!.ModuleInfo, InstTable2),
            inst_table_get_unify_insts(InstTable2, UnifyInstTable2),
            det_update_unify_inst(UnifyInstInfo, inst_det_known(Inst0, Detism),
                UnifyInstTable2, UnifyInstTable),
            inst_table_set_unify_insts(UnifyInstTable, InstTable2, InstTable),
            module_info_set_inst_table(InstTable, !ModuleInfo)
        ),
        % Avoid expanding recursive insts.
        ( if inst_contains_inst_name(!.ModuleInfo, UnifyInstName, Inst0) then
            Inst = defined_inst(UnifyInstName)
        else
            Inst = Inst0
        )
    ).

:- pred abstractly_unify_inst_2(mer_type::in, is_live::in, unify_is_real::in,
    mer_inst::in, mer_inst::in, mer_inst::out, determinism::out,
    module_info::in, module_info::out) is semidet.

abstractly_unify_inst_2(Type, Live, Real, InstA, InstB, Inst, Detism,
        !ModuleInfo) :-
    inst_expand(!.ModuleInfo, InstA, ExpandedInstA),
    inst_expand(!.ModuleInfo, InstB, ExpandedInstB),
    abstractly_unify_inst_3(Type, Live, Real, ExpandedInstA, ExpandedInstB,
        Inst0, Detism, !ModuleInfo),
    % If this unification cannot possibly succeed, the correct inst
    % is not_reached.
    ( if determinism_components(Detism, _, at_most_zero) then
        Inst = not_reached
    else
        Inst = Inst0
    ).

    % Abstractly unify two expanded insts.
    % The is_live parameter is `is_live' iff *both* insts are live.
    % Given the two insts to be unified, this produces
    % a resulting inst and a determinism for the unification.
    %
    % XXX Could be extended to handle `any' insts better.
    %
:- pred abstractly_unify_inst_3(mer_type::in, is_live::in, unify_is_real::in,
    mer_inst::in(mer_inst_expanded), mer_inst::in(mer_inst_expanded),
    mer_inst::out, determinism::out,
    module_info::in, module_info::out) is semidet.

abstractly_unify_inst_3(Type, Live, Real, InstA, InstB, Inst, Detism,
        !ModuleInfo) :-
    require_complete_switch [InstA]
    (
        InstA = not_reached,
        Inst = not_reached,
        Detism = detism_det
    ;
        InstA = free,
        (
            Live = is_live,
            require_complete_switch [InstB]
            (
                InstB = not_reached,
                Inst = not_reached,
                Detism = detism_det
            ;
                InstB = free,
                fail
            ;
                InstB = bound(UniqB, InstResultsB, BoundFunctorsB),
                unify_uniq(is_live, Real, detism_det, unique, UniqB, Uniq),
                % Since both are live, we must disallow free-free unifications.
                inst_results_bound_functor_list_is_ground_or_any(!.ModuleInfo,
                    InstResultsB, BoundFunctorsB),
                % Since both are live, we must make the result shared
                % (unless it was already shared).
                ( if ( UniqB = unique ; UniqB = mostly_unique ) then
                    make_shared_bound_functors(Type,
                        BoundFunctorsB, BoundFunctors, !ModuleInfo)
                else
                    BoundFunctors = BoundFunctorsB
                ),
                Inst = bound(Uniq, InstResultsB, BoundFunctors),
                Detism = detism_det
            ;
                InstB = ground(UniqB, HOInstInfoB),
                unify_uniq(is_live, Real, detism_det, unique, UniqB, Uniq),
                Inst = ground(Uniq, HOInstInfoB),
                Detism = detism_det
            ;
                InstB = any(UniqB, HOInstInfo),
                unify_uniq(is_live, Real, detism_det, unique, UniqB, Uniq),
                Inst = any(Uniq, HOInstInfo),
                Detism = detism_det
            ;
                InstB = constrained_inst_vars(InstVarsB, SubInstB),
                abstractly_unify_constrained_inst_vars(Type, Live, Real,
                    InstVarsB, SubInstB, InstA, Inst, Detism, !ModuleInfo)
            ;
                InstB = inst_var(_),
                % XXX Failing here preserves the old behavior of this predicate
                % for these cases, but I am not convinced it is the right thing
                % to do.
                % Why are we not handling defined_inst by looking it up?
                % Why are we not handling free/1 similarly to free/0?
                % And why are we not aborting for inst_var?
                fail
            )
        ;
            Live = is_dead,
            Inst = InstB,
            Detism = detism_det
        )
    ;
        InstA = bound(UniqA, InstResultsA, BoundFunctorsA),
        require_complete_switch [InstB]
        (
            InstB = not_reached,
            Inst = not_reached,
            Detism = detism_det
        ;
            InstB = free,
            (
                Live = is_live,
                unify_uniq(Live, Real, detism_det, unique, UniqA, Uniq),
                % Since both are live, we must disallow free-free unifications.
                inst_results_bound_functor_list_is_ground_or_any(!.ModuleInfo,
                    InstResultsA, BoundFunctorsA),
                make_shared_bound_functors(Type, BoundFunctorsA, BoundFunctors,
                    !ModuleInfo)
            ;
                Live = is_dead,
                % Why the different argument order different to the call above?
                unify_uniq(Live, Real, detism_det, UniqA, unique, Uniq),
                BoundFunctors = BoundFunctorsA
            ),
            Inst = bound(Uniq, InstResultsA, BoundFunctors),
            Detism = detism_det
        ;
            InstB = bound(UniqB, _InstResultsB, BoundFunctorsB),
            abstractly_unify_bound_functor_list(Type, Live, Real,
                BoundFunctorsA, BoundFunctorsB, BoundFunctors,
                Detism, !ModuleInfo),
            unify_uniq(Live, Real, Detism, UniqA, UniqB, Uniq),
            % XXX A better approximation of InstResults is probably possible.
            Inst = bound(Uniq, inst_test_no_results, BoundFunctors)
        ;
            InstB = ground(UniqB, _),
            unify_uniq(Live, Real, detism_semi, UniqA, UniqB, Uniq),
            (
                InstResultsA = inst_test_results_fgtc,
                Inst = InstA,
                Detism1 = detism_semi
            ;
                InstResultsA = inst_test_results(GroundnessResultA,
                    _ContainsAny, _ContainsInstNames, _ContainsInstVars,
                    ContainsTypes, MaybeTypeCtorPropagated),
                (
                    GroundnessResultA = inst_result_is_ground,
                    Inst = InstA,
                    Detism1 = detism_semi
                ;
                    ( GroundnessResultA = inst_result_is_not_ground
                    ; GroundnessResultA = inst_result_groundness_unknown
                    ),
                    make_ground_bound_functor_list(Type, Live, Real, UniqB,
                        BoundFunctorsA, BoundFunctors, Detism1, !ModuleInfo),
                    InstResults = inst_test_results(inst_result_is_ground,
                        inst_result_does_not_contain_any,
                        inst_result_contains_inst_names_unknown,
                        inst_result_contains_inst_vars_unknown,
                        ContainsTypes, MaybeTypeCtorPropagated),
                    Inst = bound(Uniq, InstResults, BoundFunctors)
                )
            ;
                InstResultsA = inst_test_no_results,
                make_ground_bound_functor_list(Type, Live, Real, UniqB,
                    BoundFunctorsA, BoundFunctors, Detism1, !ModuleInfo),
                InstResults = inst_test_results(inst_result_is_ground,
                    inst_result_does_not_contain_any,
                    inst_result_contains_inst_names_unknown,
                    inst_result_contains_inst_vars_unknown,
                    inst_result_contains_types_unknown,
                    inst_result_no_type_ctor_propagated),
                Inst = bound(Uniq, InstResults, BoundFunctors)
            ),
            det_par_conjunction_detism(Detism1, detism_semi, Detism)
        ;
            InstB = any(UniqB, _),
            allow_unify_bound_any(Real),
            unify_uniq(Live, Real, detism_semi, UniqA, UniqB, Uniq),
            % XXX Should this is_live be Live?
            make_any_bound_functor_list(Type, is_live, Real, UniqB,
                BoundFunctorsA, BoundFunctors, Detism1, !ModuleInfo),
            % XXX A better approximation of InstResults is probably possible.
            Inst = bound(Uniq, inst_test_no_results, BoundFunctors),
            det_par_conjunction_detism(Detism1, detism_semi, Detism)
        ;
            InstB = constrained_inst_vars(InstVarsB, SubInstB),
            abstractly_unify_constrained_inst_vars(Type, Live, Real,
                InstVarsB, SubInstB, InstA, Inst, Detism, !ModuleInfo)
        ;
            InstB = inst_var(_),
            % XXX Failing here preserves the old behavior of this predicate
            % for these cases, but I am not convinced it is the right thing
            % to do.
            % Why are we not handling defined_inst by looking it up?
            % Why are we not handling free/1 similarly to free/0?
            % And why are we not aborting for inst_var?
            fail
        )
    ;
        InstA = ground(UniqA, HOInstInfoA),
        (
            HOInstInfoA = none_or_default_func,
            make_ground_inst(Type, Live, Real, UniqA, InstB, Inst, Detism,
                !ModuleInfo)
        ;
            HOInstInfoA = higher_order(_PredInstA),
            require_complete_switch [InstB]
            (
                InstB = not_reached,
                Inst = not_reached,
                Detism = detism_det
            ;
                InstB = free,
                (
                    Live = is_live,
                    unify_uniq(Live, Real, detism_det, unique, UniqA, Uniq)
                ;
                    Live = is_dead,
                    Uniq = UniqA
                ),
                Inst = ground(Uniq, HOInstInfoA),
                Detism = detism_det
            ;
                InstB = bound(UniqB, InstResultsB, BoundFunctorsB),
                % If Live = is_live, should we check `Real = fake_unify'?
                unify_uniq(Live, Real, detism_semi, UniqA, UniqB, Uniq),
                make_ground_bound_functor_list(Type, Live, Real, UniqA,
                    BoundFunctorsB, BoundFunctors, Detism1, !ModuleInfo),
                Inst = bound(Uniq, InstResultsB, BoundFunctors),
                det_par_conjunction_detism(Detism1, detism_semi, Detism)
            ;
                InstB = ground(UniqB, _HOInstInfoB),
                % It is an error to unify higher-order preds,
                % so if Real \= fake_unify, then we must fail.
                % XXX but this results in mode errors in unify procs
                % Real = fake_unify,
                % In theory we should choose take the union of the information
                % specified by PredInstA and _HOInstInfoB. However, since
                % our data representation provides no way of doing that, and
                % since this will only happen for fake_unifys, for which it
                % shouldn't make any difference, we just choose the information
                % specified by HOInstInfoA.
                unify_uniq(Live, Real, detism_semi, UniqA, UniqB, Uniq),
                Inst = ground(Uniq, HOInstInfoA),
                Detism = detism_semi
            ;
                InstB = any(UniqB, _),
                (
                    Live = is_live,
                    Real = fake_unify
                ;
                    Live = is_dead,
                    allow_unify_bound_any(Real)
                ),
                unify_uniq(Live, Real, detism_semi, UniqA, UniqB, Uniq),
                Inst = ground(Uniq, HOInstInfoA),
                Detism = detism_semi
            ;
                InstB = constrained_inst_vars(InstVarsB, SubInstB),
                abstractly_unify_constrained_inst_vars(Type, Live, Real,
                    InstVarsB, SubInstB, InstA, Inst, Detism, !ModuleInfo)
            ;
                InstB = inst_var(_),
                % XXX Failing here preserves the old behavior of this predicate
                % for these cases, but I am not convinced it is the right thing
                % to do.
                % Why are we not aborting for inst_var?
                fail
            )
        )
    ;
        InstA = any(UniqA, HOInstInfoA),
        (
            HOInstInfoA = none_or_default_func,
            make_any_inst(Type, Live, Real, UniqA, InstB, Inst, Detism,
                !ModuleInfo)
        ;
            HOInstInfoA = higher_order(_PredInstA),
            require_complete_switch [InstB]
            (
                InstB = not_reached,
                Inst = not_reached,
                Detism = detism_det
            ;
                InstB = free,
                (
                    Live = is_live,
                    unify_uniq(Live, Real, detism_det, unique, UniqA, Uniq)
                ;
                    Live = is_dead,
                    Uniq = UniqA
                ),
                Inst = any(Uniq, HOInstInfoA),
                Detism = detism_det
            ;
                InstB = bound(UniqB, _InstResultsB, BoundFunctorsB),
                % XXX If Live = is_live, should we test `Real = fake_unify'?
                unify_uniq(Live, Real, detism_semi, UniqA, UniqB, Uniq),
                make_any_bound_functor_list(Type, Live, Real, UniqA,
                    BoundFunctorsB, BoundFunctors, Detism1, !ModuleInfo),
                % XXX A better approximation of InstResults is probably
                % possible.
                Inst = bound(Uniq, inst_test_no_results, BoundFunctors),
                det_par_conjunction_detism(Detism1, detism_semi, Detism)
            ;
                InstB = ground(UniqB, _HOInstInfoB),
                % See comment for the ground(_, higher_order(_)), ground(_, _)
                % case.
                Real = fake_unify,
                unify_uniq(Live, Real, detism_semi, UniqA, UniqB, Uniq),
                Inst = ground(Uniq, HOInstInfoA),
                Detism = detism_semi
            ;
                InstB = any(UniqB, _HOInstInfoB),
                % See comment for the ground(_, higher_order(_)), ground(_, _)
                % case.
                (
                    Live = is_live,
                    Real = fake_unify
                ;
                    Live = is_dead
                ),
                unify_uniq(Live, Real, detism_semi, UniqA, UniqB, Uniq),
                Inst = any(Uniq, HOInstInfoA),
                Detism = detism_semi
            ;
                InstB = constrained_inst_vars(InstVarsB, SubInstB),
                abstractly_unify_constrained_inst_vars(Type, Live, Real,
                    InstVarsB, SubInstB, InstA, Inst, Detism, !ModuleInfo)
            ;
                InstB = inst_var(_),
                % XXX Failing here preserves the old behavior of this predicate
                % for these cases, but I am not convinced it is the right thing
                % to do.
                % Why are we not aborting for inst_var?
                fail
            )
        )
    ;
        InstA = constrained_inst_vars(InstVarsA, SubInstA),
        abstractly_unify_constrained_inst_vars(Type, Live, Real,
            InstVarsA, SubInstA, InstB, Inst, Detism, !ModuleInfo)
    ;
        InstA = inst_var(_),
        % XXX Failing here preserves the old behavior of this predicate
        % for these cases, but I am not convinced it is the right thing to do.
        % Why are we not aborting for inst_var?
        fail
    ).

% :- pred check_not_clobbered(uniqueness::in, unify_is_real::in) is det.
%
% check_not_clobbered(Uniq, Real) :-
%     % Sanity check.
%     ( if Real = real_unify, Uniq = clobbered then
%         unexpected($pred, "clobbered inst")
%     else if Real = real_unify, Uniq = mostly_clobbered then
%         unexpected($pred, "mostly_clobbered inst")
%     else
%         true
%     ).

%---------------------------------------------------------------------------%

    % Abstractly unify two inst lists.
    %
:- pred abstractly_unify_inst_list(list(mer_type)::in,
    is_live::in, unify_is_real::in, list(mer_inst)::in, list(mer_inst)::in,
    list(mer_inst)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

abstractly_unify_inst_list([], _, _, [], [], [], detism_det, !ModuleInfo).
abstractly_unify_inst_list([Type | Types], Live, Real,
        [InstA | InstsA], [InstB | InstsB], [Inst | Insts],
        Detism, !ModuleInfo) :-
    abstractly_unify_inst(Type, Live, Real, InstA, InstB, Inst,
        Detism1, !ModuleInfo),
    abstractly_unify_inst_list(Types, Live, Real, InstsA, InstsB, Insts,
        Detism2, !ModuleInfo),
    det_par_conjunction_detism(Detism1, Detism2, Detism).

%---------------------------------------------------------------------------%

abstractly_unify_inst_functor(Type, Live, Real, InstA0,
        ConsIdB, ArgInstsB, ArgLives, Inst, Detism, !ModuleInfo) :-
    inst_expand(!.ModuleInfo, InstA0, InstA),
    abstractly_unify_inst_functor_2(Type, Live, Real, InstA,
        ConsIdB, ArgInstsB, ArgLives, Inst, Detism, !ModuleInfo).

:- pred abstractly_unify_inst_functor_2(mer_type::in, is_live::in,
    unify_is_real::in, mer_inst::in(mer_inst_expanded),
    cons_id::in, list(mer_inst)::in, list(is_live)::in,
    mer_inst::out, determinism::out,
    module_info::in, module_info::out) is semidet.

abstractly_unify_inst_functor_2(Type, Live, Real, InstA,
        ConsIdB, ArgInstsB, ArgLives, Inst, Detism, !ModuleInfo) :-
    require_complete_switch [InstA]
    (
        InstA = not_reached,
        Inst = not_reached,
        Detism = detism_erroneous
    ;
        InstA = free,
        (
            Live = is_live,
            get_cons_id_arg_types_for_inst(!.ModuleInfo, Type,
                ConsIdB, ArgInstsB, ArgTypes),
            inst_list_is_ground_or_any_or_dead(!.ModuleInfo, ArgLives,
                ArgInstsB),
            maybe_make_shared_inst_list(ArgTypes, ArgInstsB, ArgLives,
                ArgInsts, !ModuleInfo)
        ;
            Live = is_dead,
            ArgInsts = ArgInstsB
        ),
        ( if ConsIdB = du_data_ctor(DuCtorB) then
            arg_insts_match_ho_inst_info_in_type(!.ModuleInfo, Type,
                DuCtorB, ArgInsts)
        else
            % There are no arg types we can check against.
            true
        ),
        % XXX A better approximation of InstResults is probably possible.
        Inst = bound(unique, inst_test_no_results,
            [bound_functor(ConsIdB, ArgInsts)]),
        Detism = detism_det
    ;
        InstA = any(Uniq, _),
        % We only allow `any' to unify with a functor if we know that
        % the type is not a solver type.
        % XXX ANY_AS_BOUND
        not type_is_solver_type(!.ModuleInfo, Type),
        get_cons_id_arg_types_for_inst(!.ModuleInfo, Type, ConsIdB, ArgInstsB,
            ArgTypes),
        (
            Live = is_live,
            make_any_inst_list_lives(Live, Real, Uniq, ArgTypes,
                ArgInstsB, ArgLives, ArgInsts, Detism, !ModuleInfo)
        ;
            Live = is_dead,
            make_any_inst_list(Live, Real, Uniq, ArgTypes,
                ArgInstsB, ArgInsts, Detism, !ModuleInfo)
        ),
        % XXX A better approximation of InstResults is probably possible.
        Inst = bound(Uniq, inst_test_no_results,
            [bound_functor(ConsIdB, ArgInsts)])
    ;
        InstA = bound(UniqA, _InstResultsA, BoundFunctorsA0),
        ( if type_to_ctor(Type, TypeCtor) then
            propagate_ho_inst_info_into_bound_insts(!.ModuleInfo, TypeCtor,
                BoundFunctorsA0, BoundFunctorsA)
        else
            BoundFunctorsA = BoundFunctorsA0
        ),
        (
            Live = is_live,
            abstractly_unify_bound_functor_list_lives(Type, Real,
                BoundFunctorsA, ConsIdB, ArgInstsB, ArgLives, BoundFunctors,
                Detism, !ModuleInfo)
        ;
            Live = is_dead,
            BoundFunctorsB = [bound_functor(ConsIdB, ArgInstsB)],
            abstractly_unify_bound_functor_list(Type, is_dead, Real,
                BoundFunctorsA, BoundFunctorsB, BoundFunctors,
                Detism, !ModuleInfo)
        ),
        % XXX A better approximation of InstResults is probably possible.
        Inst = bound(UniqA, inst_test_no_results, BoundFunctors)
    ;
        InstA = ground(UniqA, _),
        get_cons_id_arg_types_for_inst(!.ModuleInfo, Type,
            ConsIdB, ArgInstsB, ArgTypes),
        (
            Live = is_live,
            make_ground_inst_list_lives(Live, Real, UniqA, ArgTypes,
                ArgLives, ArgInstsB, ArgInsts0, Detism, !ModuleInfo)
        ;
            Live = is_dead,
            make_ground_inst_list(Live, Real, UniqA, ArgTypes,
                ArgInstsB, ArgInsts0, Detism, !ModuleInfo)
        ),
        ( if ConsIdB = du_data_ctor(DuCtorB) then
            propagate_ho_inst_info_into_arg_insts(!.ModuleInfo, Type, DuCtorB,
                ArgInsts0, ArgInsts)
        else
            ArgInsts = ArgInsts0
        ),
        % XXX A better approximation of InstResults is probably possible.
        Inst = bound(UniqA, inst_test_no_results,
            [bound_functor(ConsIdB, ArgInsts)])
    ;
        InstA = constrained_inst_vars(InstVars, SubInstA),
        abstractly_unify_inst_functor(Type, Live, Real, SubInstA,
            ConsIdB, ArgInstsB, ArgLives, Inst0, Detism, !ModuleInfo),
        ( if inst_matches_final(!.ModuleInfo, Type, Inst0, SubInstA) then
            % We can keep the constrained_inst_vars.
            Inst = constrained_inst_vars(InstVars, Inst0)
        else
            % The inst has become too instantiated so we must remove
            % the constrained_inst_var.
            % XXX This throws away the information that Inst is at least as
            % ground as InstVars and is a subtype of InstVars. I don't think
            % this is likely to be a problem in practice because:
            % a) I don't think it's likely to occur very often in typical uses
            %    of polymorphic modes (I suspect SubInstA will nearly always
            %    be `ground' or `any' in which case the only way
            %    inst_matches_final can fail is if Inst0 is clobbered
            %    -- it can't be less instantiated than SubInstA); and
            % b) Even if this information is retained, I can't see what sort
            %    of situations it would actually be useful for.
            Inst = Inst0
        )
    ;
        InstA = inst_var(_),
        % XXX Failing here preserves the old behavior of this predicate
        % for these cases, but I am not convinced it is the right thing to do.
        % Why are we not handling defined_inst by looking it up?
        % And why are we not aborting for inst_var?
        fail
    ).

:- pred maybe_make_shared_inst_list(list(mer_type)::in, list(mer_inst)::in,
    list(is_live)::in, list(mer_inst)::out,
    module_info::in, module_info::out) is det.

maybe_make_shared_inst_list(Types, Insts0, Lives, Insts, !ModuleInfo) :-
    ( if
        Types = [HeadType | TailTypes],
        Insts0 = [HeadInst0 | TailInsts0],
        Lives = [HeadLive | TailLives]
    then
        (
            HeadLive = is_live,
            make_shared_inst(HeadType, HeadInst0, HeadInst, !ModuleInfo)
        ;
            HeadLive = is_dead,
            HeadInst = HeadInst0
        ),
        maybe_make_shared_inst_list(TailTypes, TailInsts0, TailLives,
            TailInsts, !ModuleInfo),
        Insts = [HeadInst | TailInsts]
    else if
        Types = [],
        Insts0 = [],
        Lives = []
    then
        Insts = []
    else
        unexpected($pred, "list length mismatch")
    ).

%---------------------------------------------------------------------------%

    % This code performs abstract unification of two bound(...) insts.
    % The lists of bound_functor are guaranteed to be sorted. The algorithm
    % for abstractly unifying two lists of bound_functors is basically a sorted
    % merge operation. If the head elements of both lists specify the same
    % function symbol, we try to unify their argument insts. If all those
    % unifications succeed, we put the resulting bound_functor in the output;
    % if one doesn't, the whole thing fails. If a function symbol occurs
    % in only one of the two input lists, it is *not* added to the output list.
    %
    % One way of looking at this code is that it simulates mode and determinism
    % checking of the goal for the unification predicate for the type.
    %
:- pred abstractly_unify_bound_functor_list(mer_type::in,
    is_live::in, unify_is_real::in,
    list(bound_functor)::in, list(bound_functor)::in, list(bound_functor)::out,
    determinism::out, module_info::in, module_info::out) is semidet.

abstractly_unify_bound_functor_list(Type, Live, Real,
        BoundFunctorsA, BoundFunctorsB, BoundFunctors, Detism, !ModuleInfo) :-
    ( if ( BoundFunctorsA = [] ; BoundFunctorsB = [] ) then
        % This probably shouldn't happen. If we get here, it means that
        % a previous goal had determinism `failure' or `erroneous',
        % but we should have optimized away the rest of the conjunction
        % after that goal.

        BoundFunctors = [],
        Detism = detism_erroneous
    else
        abstractly_unify_bound_functor_list_2(Type, Live, Real,
            BoundFunctorsA, BoundFunctorsB, BoundFunctors,
            Detism0, !ModuleInfo),

        % If there are multiple alternatives for either of the inputs,
        % or the constructor of the single alternative for each input
        % doesn't match, then the unification can fail, so adjust the
        % determinism.
        ( if
            BoundFunctorsA = [bound_functor(ConsIdA, _)],
            BoundFunctorsB = [bound_functor(ConsIdB, _)],
            equivalent_cons_ids(ConsIdA, ConsIdB)
        then
            Detism = Detism0
        else
            determinism_components(Detism0, _, MaxSoln),
            determinism_components(Detism, can_fail, MaxSoln)
        )
    ).

:- pred abstractly_unify_bound_functor_list_2(mer_type::in,
    is_live::in, unify_is_real::in,
    list(bound_functor)::in, list(bound_functor)::in, list(bound_functor)::out,
    determinism::out, module_info::in, module_info::out) is semidet.

abstractly_unify_bound_functor_list_2(_, _, _, [], [], [], detism_erroneous,
        !ModuleInfo).
abstractly_unify_bound_functor_list_2(_, _, _, [], [_ | _], [], detism_failure,
        !ModuleInfo).
abstractly_unify_bound_functor_list_2(_, _, _, [_ | _], [], [], detism_failure,
        !ModuleInfo).
abstractly_unify_bound_functor_list_2(Type, Live, Real,
        [BoundFunctorA | BoundFunctorsA], [BoundFunctorB | BoundFunctorsB],
        BoundFunctors, Detism, !ModuleInfo) :-
    BoundFunctorA = bound_functor(ConsIdA, ArgInstsA),
    BoundFunctorB = bound_functor(ConsIdB, ArgInstsB),
    ( if equivalent_cons_ids(ConsIdA, ConsIdB) then
        get_cons_id_arg_types_for_inst(!.ModuleInfo, Type, ConsIdA, ArgInstsA,
            ArgTypes),
        abstractly_unify_inst_list(ArgTypes, Live, Real, ArgInstsA, ArgInstsB,
            ArgInsts, Detism1, !ModuleInfo),
        abstractly_unify_bound_functor_list_2(Type, Live, Real,
            BoundFunctorsA, BoundFunctorsB,
            BoundFunctorsTail, Detism2, !ModuleInfo),

        % If the unification of the two bound_functors is guaranteed to fail,
        % don't include the cons_id in the list.
        ( if determinism_components(Detism1, _, at_most_zero) then
            BoundFunctors = BoundFunctorsTail
        else
            BoundFunctors =
                [bound_functor(ConsIdA, ArgInsts) | BoundFunctorsTail]
        ),
        det_switch_detism(Detism1, Detism2, Detism)
    else
        ( if compare(<, ConsIdA, ConsIdB) then
            abstractly_unify_bound_functor_list_2(Type, Live, Real,
                BoundFunctorsA, [BoundFunctorB | BoundFunctorsB],
                BoundFunctors, Detism1, !ModuleInfo)
        else
            abstractly_unify_bound_functor_list_2(Type, Live, Real,
                [BoundFunctorA | BoundFunctorsA], BoundFunctorsB,
                BoundFunctors, Detism1, !ModuleInfo)
        ),
        det_switch_detism(Detism1, detism_failure, Detism)
    ).

:- pred abstractly_unify_bound_functor_list_lives(mer_type::in,
    unify_is_real::in, list(bound_functor)::in, cons_id::in,
    list(mer_inst)::in, list(is_live)::in,
    list(bound_functor)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

abstractly_unify_bound_functor_list_lives(_, _, [], _, _, _,
        [], detism_failure, !ModuleInfo).
abstractly_unify_bound_functor_list_lives(Type, Real,
        [BoundFunctorA | BoundFunctorsA], ConsIdB, ArgInstsB, LivesB,
        BoundFunctors, Detism, !ModuleInfo) :-
    BoundFunctorA = bound_functor(ConsIdA, ArgInstsA),
    ( if equivalent_cons_ids(ConsIdA, ConsIdB) then
        % XXX ArgTypes seems to be a invariant of this loop.
        get_cons_id_arg_types_for_inst(!.ModuleInfo, Type, ConsIdB, ArgInstsB,
            ArgTypes),
        abstractly_unify_inst_list_lives(ArgTypes, Real, ArgInstsA, ArgInstsB,
            LivesB, ArgInsts, Detism, !ModuleInfo),
        BoundFunctors = [bound_functor(ConsIdA, ArgInsts)]
    else
        abstractly_unify_bound_functor_list_lives(Type, Real, BoundFunctorsA,
            ConsIdB, ArgInstsB, LivesB, BoundFunctors, Detism, !ModuleInfo)
    ).

:- pred abstractly_unify_inst_list_lives(list(mer_type)::in, unify_is_real::in,
    list(mer_inst)::in, list(mer_inst)::in, list(is_live)::in,
    list(mer_inst)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

abstractly_unify_inst_list_lives([], _, [], [], [],
        [], detism_det, !ModuleInfo).
abstractly_unify_inst_list_lives([Type | Types], Real,
        [InstA | InstsA], [InstB | InstsB], [Live | Lives],
        [Inst | Insts], Detism, !ModuleInfo) :-
    abstractly_unify_inst(Type, Live, Real, InstA, InstB,
        Inst, Detism1, !ModuleInfo),
    abstractly_unify_inst_list_lives(Types, Real, InstsA, InstsB, Lives,
        Insts, Detism2, !ModuleInfo),
    det_par_conjunction_detism(Detism1, Detism2, Detism).

%---------------------------------------------------------------------------%

:- pred abstractly_unify_constrained_inst_vars(mer_type::in,
    is_live::in, unify_is_real::in,
    set(inst_var)::in, mer_inst::in, mer_inst::in, mer_inst::out,
    determinism::out, module_info::in, module_info::out) is semidet.

abstractly_unify_constrained_inst_vars(Type, Live, Real, InstVarsA, SubInstA,
        InstB, Inst, Detism, !ModuleInfo) :-
    abstractly_unify_inst(Type, Live, Real, SubInstA, InstB, Inst0,
        Detism, !ModuleInfo),
    ( if not inst_matches_final(!.ModuleInfo, Type, Inst0, SubInstA) then
        % The inst has become too instantiated, so the
        % constrained_inst_vars wrapper must be removed.
        Inst = Inst0
    else if Inst0 = constrained_inst_vars(InstVars0, SubInst0) then
        % Avoid nested constrained_inst_vars wrappers.
        Inst = constrained_inst_vars(set.union(InstVars0, InstVarsA), SubInst0)
    else
        % We can keep the constrained_inst_vars wrapper.
        Inst = constrained_inst_vars(InstVarsA, Inst0)
    ).

%---------------------------------------------------------------------------%

:- pred arg_insts_match_ho_inst_info_in_type(module_info::in, mer_type::in,
    du_ctor::in, list(mer_inst)::in) is semidet.

arg_insts_match_ho_inst_info_in_type(ModuleInfo, Type, DuCtor, ArgInsts) :-
    ( if
        type_to_ctor(Type, TypeCtor),
        get_cons_defn(ModuleInfo, TypeCtor, DuCtor, ConsDefn),
        ConsDefn = hlds_cons_defn(_, _, _, _,
            MaybeExistConstraints, ConsArgs, _),
        % Some builtin types have constructors with arguments that are not
        % reflected in the constructor definition, and which return an
        % empty list.
        ConsArgs = [_ | _],
        % XXX Handle existentially quantified constructors.
        MaybeExistConstraints = no_exist_constraints
    then
        arg_insts_match_ho_inst_info_in_type_loop(ModuleInfo,
            ConsArgs, ArgInsts)
    else
        true
    ).

:- pred arg_insts_match_ho_inst_info_in_type_loop(module_info::in,
    list(constructor_arg)::in, list(mer_inst)::in) is semidet.

arg_insts_match_ho_inst_info_in_type_loop(_, [], []).
arg_insts_match_ho_inst_info_in_type_loop(_, [], [_ | _]) :-
    unexpected($pred, "length mismatch").
arg_insts_match_ho_inst_info_in_type_loop(_, [_ | _], []) :-
    unexpected($pred, "length mismatch").
arg_insts_match_ho_inst_info_in_type_loop(ModuleInfo,
        [ConsArg | ConsArgs], [Inst | Insts]) :-
    ( if
        ( Inst = ground(_, InstHOInstInfo)
        ; Inst = any(_, InstHOInstInfo)
        ),
        ArgType = ConsArg ^ arg_type,
        ArgType = higher_order_type(_, _, TypeHOInstInfo, _),
        TypeHOInstInfo = higher_order(TypePredInst)
    then
        InstHOInstInfo = higher_order(InstPredInst),
        pred_inst_matches(ModuleInfo, ArgType, InstPredInst, TypePredInst)
    else
        true
    ),
    arg_insts_match_ho_inst_info_in_type_loop(ModuleInfo, ConsArgs, Insts).

%---------------------------------------------------------------------------%

:- pred propagate_ho_inst_info_into_bound_insts(module_info::in,
    type_ctor::in, list(bound_functor)::in, list(bound_functor)::out) is det.

propagate_ho_inst_info_into_bound_insts(_, _, [], []).
propagate_ho_inst_info_into_bound_insts(ModuleInfo, TypeCtor,
        [BoundFunctor0 | BoundFunctors0], [BoundFunctor | BoundFunctors]) :-
    BoundFunctor0 = bound_functor(ConsId, ArgInsts0),
    ( if
        ConsId = du_data_ctor(DuCtor),
        get_cons_defn(ModuleInfo, TypeCtor, DuCtor, ConsDefn),
        ConsDefn = hlds_cons_defn(_, _, _, _,
            MaybeExistConstraints, ConsArgTypes, _),
        % Some builtin types have constructors with arguments that are not
        % reflected in the constructor definition, and which return an
        % empty list.
        ConsArgTypes = [_ | _],
        % XXX Handle existentially quantified constructors.
        MaybeExistConstraints = no_exist_constraints
    then
        propagate_ho_inst_info_into_arg_insts_loop(ConsArgTypes,
            ArgInsts0, ArgInsts),
        BoundFunctor = bound_functor(ConsId, ArgInsts)
    else
        BoundFunctor = BoundFunctor0
    ),
    propagate_ho_inst_info_into_bound_insts(ModuleInfo, TypeCtor,
        BoundFunctors0, BoundFunctors).

%---------------------------------------------------------------------------%

:- pred propagate_ho_inst_info_into_arg_insts(module_info::in, mer_type::in,
    du_ctor::in, list(mer_inst)::in, list(mer_inst)::out) is det.

propagate_ho_inst_info_into_arg_insts(ModuleInfo, Type, DuCtor, !ArgInsts) :-
    ( if
        type_to_ctor(Type, TypeCtor),
        get_cons_defn(ModuleInfo, TypeCtor, DuCtor, ConsDefn),
        ConsDefn = hlds_cons_defn(_, _, _, _,
            MaybeExistConstraints, ConsArgTypes, _),
        % Some builtin types have constructors with arguments that are not
        % reflected in the constructor definition, and which return an
        % empty list.
        ConsArgTypes = [_ | _],
        % XXX Handle existentially quantified constructors.
        MaybeExistConstraints = no_exist_constraints
    then
        propagate_ho_inst_info_into_arg_insts_loop(ConsArgTypes, !ArgInsts)
    else
        true
    ).

:- pred propagate_ho_inst_info_into_arg_insts_loop(list(constructor_arg)::in,
    list(mer_inst)::in, list(mer_inst)::out) is det.

propagate_ho_inst_info_into_arg_insts_loop([], [], []).
propagate_ho_inst_info_into_arg_insts_loop([], [_ | _], _) :-
    unexpected($pred, "length mismatch").
propagate_ho_inst_info_into_arg_insts_loop([_ | _], [], _) :-
    unexpected($pred, "length mismatch").
propagate_ho_inst_info_into_arg_insts_loop([ConsArgType | ConsArgTypes],
        [Inst0 | Insts0], [Inst | Insts]) :-
    propagate_type_ho_inst_info_into_inst(ConsArgType ^ arg_type, Inst0, Inst),
    propagate_ho_inst_info_into_arg_insts_loop(ConsArgTypes, Insts0, Insts).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Unifying shared with either shared or unique gives shared.
    % Unifying unique with unique gives shared if live, unique if dead.
    % Unifying clobbered with anything gives clobbered, except that if live
    % then it is an internal error (a clobbered value should not be live,
    % right?), and except that unifying with clobbered is not allowed for
    % semidet unifications, unless they are "fake".
    %
    % The only way this predicate can abort is if a clobbered value is live.
    %
    % The only way this predicate can fail (indicating a unique mode error)
    % is if we are attempting to unify with a clobbered value, and this was
    % a "real" unification, not a "fake" one, and the determinism of the
    % unification is semidet. (See comment in prog_data.m for more info
    % on "real" vs "fake".) Note that if a unification or sub-unification
    % is det, then it is OK to unify with a clobbered value. This can occur
    % e.g. with unifications between free and clobbered, or with free and
    % bound(..., clobbered, ...). Such det unifications are OK because the
    % clobbered value will not be examined, instead all that will happen
    % is that a variable or a field of a variable will become bound to the
    % clobbered value; and since the final inst will also be clobbered,
    % the variable or field's value can never be examined later either.
    % Only semidet unifications would test the value of a clobbered variable,
    % so those are the only ones we need to disallow.
    %
:- pred unify_uniq(is_live::in, unify_is_real::in, determinism::in,
    uniqueness::in, uniqueness::in, uniqueness::out) is semidet.

unify_uniq(Live, Real, Detism, UniqA, UniqB, Uniq) :-
    require_complete_switch [UniqA]
    (
        UniqA = shared,
        require_complete_switch [UniqB]
        (
            ( UniqB = shared
            ; UniqB = unique
            ; UniqB = mostly_unique
            ),
            Uniq = shared
        ;
            UniqB = clobbered,
            allow_unify_with_clobbered(Live, Real, Detism),
            Uniq = clobbered
        ;
            UniqB = mostly_clobbered,
            allow_unify_with_clobbered(Live, Real, Detism),
            Uniq = mostly_clobbered
        )
    ;
        UniqA = unique,
        require_complete_switch [UniqB]
        (
            UniqB = shared,
            Uniq = shared
        ;
            UniqB = unique,
            (
                Live = is_live,
                Uniq = shared
            ;
                Live = is_dead,
                Uniq = unique
            )
        ;
            UniqB = mostly_unique,
            (
                Live = is_live,
                Uniq = shared
            ;
                Live = is_dead,
                % XXX This is a conservative approximation;
                % sometimes we should return unique, not mostly_unique.
                Uniq = mostly_unique
            )
        ;
            UniqB = clobbered,
            allow_unify_with_clobbered(Live, Real, Detism),
            Uniq = clobbered
        ;
            UniqB = mostly_clobbered,
            allow_unify_with_clobbered(Live, Real, Detism),
            Uniq = mostly_clobbered
        )
    ;
        UniqA = mostly_unique,
        require_complete_switch [UniqB]
        (
            UniqB = shared,
            Uniq = shared
        ;
            UniqB = unique,
            (
                Live = is_live,
                Uniq = shared
            ;
                Live = is_dead,
                % XXX This is a conservative approximation;
                % sometimes we should return unique, not mostly_unique.
                Uniq = mostly_unique
            )
        ;
            UniqB = mostly_unique,
            (
                Live = is_live,
                Uniq = shared
            ;
                Live = is_dead,
                Uniq = mostly_unique
            )
        ;
            UniqB = clobbered,
            allow_unify_with_clobbered(Live, Real, Detism),
            Uniq = clobbered
        ;
            UniqB = mostly_clobbered,
            allow_unify_with_clobbered(Live, Real, Detism),
            Uniq = mostly_clobbered
        )
    ;
        UniqA = clobbered,
        allow_unify_with_clobbered(Live, Real, Detism),
        Uniq = clobbered
    ;
        UniqA = mostly_clobbered,
        allow_unify_with_clobbered(Live, Real, Detism),
        ( if UniqB = clobbered then
            Uniq = clobbered
        else
            Uniq = mostly_clobbered
        )
    ).

:- pred allow_unify_with_clobbered(is_live::in, unify_is_real::in,
    determinism::in) is semidet.

allow_unify_with_clobbered(is_live, _, _) :-
    unexpected($pred, "clobbered value is is_live?").
allow_unify_with_clobbered(is_dead, fake_unify, _).
allow_unify_with_clobbered(is_dead, _, detism_det).

%---------------------------------------------------------------------------%

:- pred make_ground_inst_list_lives(is_live::in, unify_is_real::in,
    uniqueness::in, list(mer_type)::in, list(is_live)::in,
    list(mer_inst)::in, list(mer_inst)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

make_ground_inst_list_lives(_, _, _, _, [], [], [], detism_det, !ModuleInfo).
make_ground_inst_list_lives(Live, Real, Uniq, [ArgType | ArgTypes],
        [ArgLive | ArgLives], [ArgInst0 | ArgInsts0], [ArgInst | ArgInsts],
        Detism, !ModuleInfo) :-
    ( if Live = is_live, ArgLive = is_live then
        BothLive = is_live
    else
        BothLive = is_dead
    ),
    make_ground_inst(ArgType, BothLive, Real, Uniq, ArgInst0, ArgInst,
        Detism1, !ModuleInfo),
    make_ground_inst_list_lives(Live, Real, Uniq, ArgTypes, ArgLives,
        ArgInsts0, ArgInsts, Detism2, !ModuleInfo),
    det_par_conjunction_detism(Detism1, Detism2, Detism).

:- pred make_ground_inst_list(is_live::in, unify_is_real::in, uniqueness::in,
    list(mer_type)::in, list(mer_inst)::in, list(mer_inst)::out,
    determinism::out, module_info::in, module_info::out) is semidet.

make_ground_inst_list(_, _, _, [], [], [], detism_det, !ModuleInfo).
make_ground_inst_list(Live, Real, Uniq, [ArgType | ArgTypes],
        [ArgInst0 | ArgInsts0], [ArgInst | ArgInsts],
        Detism, !ModuleInfo) :-
    make_ground_inst(ArgType, Live, Real, Uniq, ArgInst0, ArgInst,
        Detism1, !ModuleInfo),
    make_ground_inst_list(Live, Real, Uniq, ArgTypes, ArgInsts0, ArgInsts,
        Detism2, !ModuleInfo),
    det_par_conjunction_detism(Detism1, Detism2, Detism).

    % Abstractly unify an inst with `ground' and calculate the new inst
    % and the determinism of the unification.
    %
:- pred make_ground_inst(mer_type::in, is_live::in, unify_is_real::in,
    uniqueness::in, mer_inst::in, mer_inst::out, determinism::out,
    module_info::in, module_info::out) is semidet.

make_ground_inst(Type, Live, Real, Uniq1, Inst0, Inst, Detism, !ModuleInfo) :-
    (
        Inst0 = not_reached,
        Inst = not_reached,
        Detism = detism_erroneous
    ;
        Inst0 = any(Uniq0, HOInstInfo),
        unify_uniq(Live, Real, detism_semi, Uniq0, Uniq1, Uniq),
        Inst = ground(Uniq, HOInstInfo),
        Detism = detism_semi
    ;
        Inst0 = free,
        unify_uniq(Live, Real, detism_det, unique, Uniq1, Uniq),
        Inst = ground(Uniq, none_or_default_func),
        Detism = detism_det
    ;
        Inst0 = bound(Uniq0, InstResults0, BoundFunctors0),
        unify_uniq(Live, Real, detism_semi, Uniq0, Uniq1, Uniq),
        make_ground_bound_functor_list(Type, Live, Real, Uniq1,
            BoundFunctors0, BoundFunctors, Detism1, !ModuleInfo),
        Inst = bound(Uniq, InstResults0, BoundFunctors),
        det_par_conjunction_detism(Detism1, detism_semi, Detism)
    ;
        Inst0 = ground(Uniq0, HOInstInfo),
        unify_uniq(Live, Real, detism_semi, Uniq0, Uniq1, Uniq),
        Inst = ground(Uniq, HOInstInfo),
        Detism = detism_semi
    ;
        Inst0 = inst_var(_),
        unexpected($pred, "free inst var")
    ;
        Inst0 = constrained_inst_vars(InstVars, SubInst0),
        abstractly_unify_constrained_inst_vars(Type, Live, Real,
            InstVars, SubInst0, ground(Uniq1, none_or_default_func),
            Inst, Detism, !ModuleInfo)
    ;
        Inst0 = defined_inst(InstName),
        % Check whether the inst name is already in the ground_inst table.
        module_info_get_inst_table(!.ModuleInfo, InstTable0),
        inst_table_get_ground_insts(InstTable0, GroundInstTable0),
        GroundInstInfo = ground_inst_info(InstName, Uniq1, Live, Real),
        GroundInstName = ground_inst(InstName, Uniq1, Live, Real),
        search_insert_unknown_ground_inst(GroundInstInfo, MaybeOldMaybeInst,
            GroundInstTable0, GroundInstTable1),
        (
            MaybeOldMaybeInst = yes(OldMaybeInst),
            (
                OldMaybeInst = inst_det_known(GroundInst, Detism)
            ;
                OldMaybeInst = inst_det_unknown,
                GroundInst = defined_inst(GroundInstName),
                Detism = detism_det
                % We can safely assume this is det, since if it were semidet,
                % we would have noticed this in the process of unfolding the
                % definition.
            )
        ;
            MaybeOldMaybeInst = no,
            % We have inserted GroundInstInfo into the table with value
            % `inst_unknown'.
            inst_table_set_ground_insts(GroundInstTable1,
                InstTable0, InstTable1),
            module_info_set_inst_table(InstTable1, !ModuleInfo),

            % Expand the inst name, and invoke ourself recursively on its
            % expansion.
            inst_lookup(!.ModuleInfo, InstName, SubInst0),
            inst_expand(!.ModuleInfo, SubInst0, SubInst1),
            make_ground_inst(Type, Live, Real, Uniq1, SubInst1, GroundInst,
                Detism, !ModuleInfo),

            % Now that we have determined the resulting Inst, store the
            % appropriate value `known(GroundInst, Detism)' in the ground_inst
            % table.
            module_info_get_inst_table(!.ModuleInfo, InstTable2),
            inst_table_get_ground_insts(InstTable2, GroundInstTable2),
            det_update_ground_inst(GroundInstInfo,
                inst_det_known(GroundInst, Detism),
                GroundInstTable2, GroundInstTable),
            inst_table_set_ground_insts(GroundInstTable,
                InstTable2, InstTable),
            module_info_set_inst_table(InstTable, !ModuleInfo)
        ),
        % Avoid expanding recursive insts.
        ( if
            inst_contains_inst_name(!.ModuleInfo, GroundInstName, GroundInst)
        then
            Inst = defined_inst(GroundInstName)
        else
            Inst = GroundInst
        )
    ).

:- pred make_ground_bound_functor_list(mer_type::in,
    is_live::in, unify_is_real::in,
    uniqueness::in, list(bound_functor)::in, list(bound_functor)::out,
    determinism::out, module_info::in, module_info::out) is semidet.

make_ground_bound_functor_list(_, _, _, _, [], [], detism_det, !ModuleInfo).
make_ground_bound_functor_list(Type, Live, Real, Uniq,
            [BoundFunctor0 | BoundFunctors0], [BoundFunctor | BoundFunctors],
            Detism, !ModuleInfo) :-
    BoundFunctor0 = bound_functor(ConsId, ArgInsts0),
    get_cons_id_arg_types_for_inst(!.ModuleInfo, Type, ConsId, ArgInsts0,
        ArgTypes),
    make_ground_inst_list(Live, Real, Uniq, ArgTypes, ArgInsts0, ArgInsts,
        Detism1, !ModuleInfo),
    BoundFunctor = bound_functor(ConsId, ArgInsts),
    make_ground_bound_functor_list(Type, Live, Real, Uniq,
        BoundFunctors0, BoundFunctors, Detism2, !ModuleInfo),
    det_par_conjunction_detism(Detism1, Detism2, Detism).

%---------------------------------------------------------------------------%

    % Abstractly unify an inst with `any' and calculate the new inst
    % and the determinism of the unification.
    %
:- pred make_any_inst(mer_type::in, is_live::in, unify_is_real::in,
    uniqueness::in, mer_inst::in, mer_inst::out, determinism::out,
    module_info::in, module_info::out) is semidet.

make_any_inst(Type, Live, Real, Uniq1, Inst0, Inst, Detism, !ModuleInfo) :-
    (
        Inst0 = not_reached,
        Inst = not_reached,
        Detism = detism_erroneous
    ;
        Inst0 = any(Uniq0, HOInstInfo),
        allow_unify_bound_any(Real),
        unify_uniq(Live, Real, detism_semi, Uniq0, Uniq1, Uniq),
        Inst = any(Uniq, HOInstInfo),
        Detism = detism_semi
    ;
        Inst0 = free,
        unify_uniq(Live, Real, detism_det, unique, Uniq1, Uniq),
        Inst = any(Uniq, none_or_default_func),
        Detism = detism_det
    ;
        Inst0 = bound(Uniq0, _InstResults0, BoundFunctors0),
        allow_unify_bound_any(Real),
        unify_uniq(Live, Real, detism_semi, Uniq0, Uniq1, Uniq),
        make_any_bound_functor_list(Type, Live, Real, Uniq1,
            BoundFunctors0, BoundFunctors, Detism1, !ModuleInfo),
        % XXX A better approximation of InstResults is probably possible.
        Inst = bound(Uniq, inst_test_no_results, BoundFunctors),
        det_par_conjunction_detism(Detism1, detism_semi, Detism)
    ;
        Inst0 = ground(Uniq0, PredInst),
        allow_unify_bound_any(Real),
        unify_uniq(Live, Real, detism_semi, Uniq0, Uniq1, Uniq),
        Inst = ground(Uniq, PredInst),
        Detism = detism_semi
    ;
        Inst0 = inst_var(_),
        unexpected($pred, "free inst var")
    ;
        Inst0 = constrained_inst_vars(InstVars, SubInst0),
        abstractly_unify_constrained_inst_vars(Type, Live, Real,
            InstVars, SubInst0, any(Uniq1, none_or_default_func),
            Inst, Detism, !ModuleInfo)
    ;
        Inst0 = defined_inst(InstName),
        % Check whether the inst name is already in the any_inst table.
        module_info_get_inst_table(!.ModuleInfo, InstTable0),
        inst_table_get_any_insts(InstTable0, AnyInstTable0),
        AnyInstInfo = any_inst_info(InstName, Uniq1, Live, Real),
        AnyInstName = any_inst(InstName, Uniq1, Live, Real),
        search_insert_unknown_any_inst(AnyInstInfo, MaybeOldMaybeInst,
            AnyInstTable0, AnyInstTable1),
        (
            MaybeOldMaybeInst = yes(OldMaybeInst),
            (
                OldMaybeInst = inst_det_known(AnyInst, Detism)
            ;
                OldMaybeInst = inst_det_unknown,
                AnyInst = defined_inst(AnyInstName),
                Detism = detism_det
                % We can safely assume this is det, since if it were semidet,
                % we would have noticed this in the process of unfolding the
                % definition.
            )
        ;
            MaybeOldMaybeInst = no,
            % We have inserted AnyInstKey into the table with value
            % `inst_unknown'.
            inst_table_set_any_insts(AnyInstTable1, InstTable0, InstTable1),
            module_info_set_inst_table(InstTable1, !ModuleInfo),

            % Expand the inst name, and invoke ourself recursively on its
            % expansion.
            inst_lookup(!.ModuleInfo, InstName, SubInst0),
            inst_expand(!.ModuleInfo, SubInst0, SubInst1),
            make_any_inst(Type, Live, Real, Uniq1, SubInst1, AnyInst, Detism,
                !ModuleInfo),

            % Now that we have determined the resulting Inst, store the
            % appropriate value `known(AnyInst, Detism)' in the any_inst table.
            module_info_get_inst_table(!.ModuleInfo, InstTable2),
            inst_table_get_any_insts(InstTable2, AnyInstTable2),
            det_update_any_inst(AnyInstInfo, inst_det_known(AnyInst, Detism),
                AnyInstTable2, AnyInstTable),
            inst_table_set_any_insts(AnyInstTable, InstTable2, InstTable),
            module_info_set_inst_table(InstTable, !ModuleInfo)
        ),
        % Avoid expanding recursive insts.
        ( if inst_contains_inst_name(!.ModuleInfo, AnyInstName, AnyInst) then
            Inst = defined_inst(AnyInstName)
        else
            Inst = AnyInst
        )
    ).

:- pred make_any_bound_functor_list(mer_type::in, is_live::in,
    unify_is_real::in, uniqueness::in, list(bound_functor)::in,
    list(bound_functor)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

make_any_bound_functor_list(_, _, _, _, [], [], detism_det, !ModuleInfo).
make_any_bound_functor_list(Type, Live, Real, Uniq, [BoundFunctor0 |
BoundFunctors0],
        [BoundFunctor | BoundFunctors], Detism, !ModuleInfo) :-
    BoundFunctor0 = bound_functor(ConsId, ArgInsts0),
    get_cons_id_arg_types_for_inst(!.ModuleInfo, Type, ConsId, ArgInsts0,
        ArgTypes),
    make_any_inst_list(Live, Real, Uniq, ArgTypes, ArgInsts0, ArgInsts,
        Detism1, !ModuleInfo),
    BoundFunctor = bound_functor(ConsId, ArgInsts),
    make_any_bound_functor_list(Type, Live, Real, Uniq, BoundFunctors0,
        BoundFunctors, Detism2, !ModuleInfo),
    det_par_conjunction_detism(Detism1, Detism2, Detism).

:- pred make_any_inst_list(is_live::in, unify_is_real::in, uniqueness::in,
    list(mer_type)::in, list(mer_inst)::in, list(mer_inst)::out,
    determinism::out, module_info::in, module_info::out) is semidet.

make_any_inst_list(_, _, _, [], [], [], detism_det, !ModuleInfo).
make_any_inst_list(Live, Real, Uniq, [Type | Types],
        [Inst0 | Insts0], [Inst | Insts], Detism, !ModuleInfo) :-
    make_any_inst(Type, Live, Real, Uniq, Inst0, Inst, Detism1, !ModuleInfo),
    make_any_inst_list(Live, Real, Uniq, Types, Insts0, Insts,
        Detism2, !ModuleInfo),
    det_par_conjunction_detism(Detism1, Detism2, Detism).

:- pred make_any_inst_list_lives(is_live::in, unify_is_real::in,
    uniqueness::in, list(mer_type)::in, list(mer_inst)::in, list(is_live)::in,
    list(mer_inst)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

make_any_inst_list_lives(_, _, _, [], [], _, [], detism_det, !ModuleInfo).
make_any_inst_list_lives(Live, Real, Uniq, [Type | Types], [Inst0 | Insts0],
        [ArgLive | ArgLives], [Inst | Insts], Detism,
        !ModuleInfo) :-
    ( if Live = is_live, ArgLive = is_live then
        BothLive = is_live
    else
        BothLive = is_dead
    ),
    make_any_inst(Type, BothLive, Real, Uniq, Inst0, Inst,
        Detism1, !ModuleInfo),
    make_any_inst_list_lives(Live, Real, Uniq, Types, Insts0, ArgLives,
        Insts, Detism2, !ModuleInfo),
    det_par_conjunction_detism(Detism1, Detism2, Detism).

%---------------------------------------------------------------------------%

    % Should we allow unifications between bound (or ground) insts
    % and `any' insts?
    % Previously we only allowed this for fake_unifies,
    % but now we allow it for real_unifies too.
    %
:- pred allow_unify_bound_any(unify_is_real::in) is det.

allow_unify_bound_any(_) :-
    true.

%---------------------------------------------------------------------------%
:- end_module check_hlds.inst_abstract_unify.
%---------------------------------------------------------------------------%
