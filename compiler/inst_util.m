%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: inst_util.m.
% Author: fjh.
%
% This module defines some utility routines for manipulating insts.
%
% The handling of `any' insts is not complete. (See also inst_match.m)
%
% The major limitation is that we don't allow `free' to be passed
% where `any' is expected. To handle that, modes.m would have to be
% changed to handle the implicit conversions from `free' to `any' at
%
%   (1) procedure calls (this is just an extension of implied modes)
%   (2) the end of branched goals
%   (3) the end of predicates.
%
% Since that is not yet done, we currently require the user to
% insert explicit calls to initialize constraint variables.
%
% Another limitation is that we don't allow any unifications between functors
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
%-----------------------------------------------------------------------------%

:- module check_hlds.inst_util.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % Mode checking is like abstract interpretation. The predicates below
    % define the abstract unification operation which unifies two
    % instantiatednesses. If the unification would be illegal, then abstract
    % unification fails. If the unification would fail, then the abstract
    % unification will succeed, and the resulting instantiatedness will be
    % `not_reached'.

    % Compute the inst that results from abstractly unifying two variables.
    %
:- pred abstractly_unify_inst(is_live::in, mer_inst::in, mer_inst::in,
    unify_is_real::in, mer_inst::out, determinism::out,
    module_info::in, module_info::out) is semidet.

    % Compute the inst that results from abstractly unifying
    % a variable with a functor.
    %
:- pred abstractly_unify_inst_functor(is_live::in, mer_inst::in,
    cons_id::in, list(mer_inst)::in, list(is_live)::in, unify_is_real::in,
    mer_type::in, mer_inst::out, determinism::out,
    module_info::in, module_info::out) is semidet.

%-----------------------------------------------------------------------------%

    % Given an inst, return a new inst which is the same as the original inst
    % but with all occurrences of `unique' replaced with `mostly_unique'.
    %
:- pred make_mostly_uniq_inst(mer_inst::in, mer_inst::out,
    module_info::in, module_info::out) is det.

    % Given a list of insts, return a new list of insts which is the same
    % as the original list of insts, but with all occurrences of `unique'
    % replaced with `shared'. It is an error if any part of the inst list
    % is free.
    %
:- pred make_shared_inst_list(list(mer_inst)::in, list(mer_inst)::out,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%

    % inst_merge(InstA, InstB, MaybeType, InstC, !ModuleInfo):
    %
    % Combine the insts found in different arms of a disjunction (or
    % if-then-else). The information in InstC is the minimum of the information
    % in InstA and InstB. Where InstA and InstB specify a binding (free or
    % bound), it must be the same in both.
    %
:- pred inst_merge(mer_inst::in, mer_inst::in, maybe(mer_type)::in,
    mer_inst::out, module_info::in, module_info::out) is semidet.

%-----------------------------------------------------------------------------%

    % inst_contains_nonstandard_func_mode(Inst, ModuleInfo) succeeds iff the
    % inst contains a higher-order function inst that does not match the
    % standard function mode `(in, ..., in) = out is det'.
    % E.g. this predicate fails for "func(in) = uo" because that matches the
    % standard func mode "func(in) = out", even though it isn't the same as
    % the standard func mode.
    %
:- pred inst_contains_nonstandard_func_mode(module_info::in, mer_inst::in)
    is semidet.

    % Succeed iff the inst is any or contains any.
    %
:- pred inst_contains_any(module_info::in, mer_inst::in) is semidet.

    % Succeed iff the given var's inst is any or contains any.
    %
:- pred var_inst_contains_any(module_info::in, instmap::in, prog_var::in)
    is semidet.

    % Succeed iff the first argument is a function pred_inst_info
    % whose mode does not match the standard func mode.
    %
:- pred pred_inst_info_is_nonstandard_func_mode(module_info::in,
    pred_inst_info::in) is semidet.

    % Succeed iff the first argument is a function ho_inst_info
    % whose mode does not match the standard func mode.
    %
:- pred ho_inst_info_is_nonstandard_func_mode(module_info::in,
    ho_inst_info::in) is semidet.

    % Return the standard mode for a function of the given arity.
    %
:- func pred_inst_info_standard_func_mode(arity) = pred_inst_info.

    % Return true if the given inst may restrict the set of function symbols
    % that may be successfully unified with the variable that has this inst.
    %
:- func inst_may_restrict_cons_ids(module_info, mer_inst) = bool.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.

%-----------------------------------------------------------------------------%

abstractly_unify_inst(Live, InstA, InstB, Real, Inst, Detism, !ModuleInfo) :-
    % Check whether this pair of insts is already in the unify_insts table.
    ThisInstPair = unify_inst(Live, InstA, InstB, Real),
    module_info_get_inst_table(!.ModuleInfo, InstTable0),
    inst_table_get_unify_insts(InstTable0, UnifyInsts0),
    % XXX For code that uses large facts, the deeply nested insts we unify
    % here means that searching UnifyInsts0 here, and updating it (twice)
    % in the else case below are *extremely* expensive. In one version of
    % Doug Auclair's training_cars example, the map search, insert and update
    % account for 116 out the 120 clock ticks spent in this predicate,
    % i.e. they account for almost 97% of its runtime.
    ( map.search(UnifyInsts0, ThisInstPair, Result) ->
        ( Result = inst_det_known(UnifyInst, UnifyDetism) ->
            Inst0 = UnifyInst,
            Detism = UnifyDetism
        ;
            Inst0 = defined_inst(ThisInstPair),
            % It is ok to assume that the unification is deterministic here,
            % because the only time that this will happen is when we get to the
            % recursive case for a recursively defined inst. If the unification
            % as a whole is semidet, then this must be because it is semidet
            % somewhere else too.
            Detism = detism_det
        ),
        Inst1 = Inst0
    ;
        % Insert ThisInstPair into the table with value `unknown'.
        map.det_insert(ThisInstPair, inst_det_unknown,
            UnifyInsts0, UnifyInsts1),
        inst_table_set_unify_insts(UnifyInsts1, InstTable0, InstTable1),
        module_info_set_inst_table(InstTable1, !ModuleInfo),
        % Unify the insts.
        inst_expand(!.ModuleInfo, InstA, ExpandedInstA),
        inst_expand(!.ModuleInfo, InstB, ExpandedInstB),
        abstractly_unify_inst_2(Live, ExpandedInstA, ExpandedInstB, Real,
            Inst0, Detism, !ModuleInfo),

        % If this unification cannot possibly succeed, the correct inst
        % is not_reached.
        ( determinism_components(Detism, _, at_most_zero) ->
            Inst1 = not_reached
        ;
            Inst1 = Inst0
        ),

        % Now update the value associated with ThisInstPair.
        module_info_get_inst_table(!.ModuleInfo, InstTable2),
        inst_table_get_unify_insts(InstTable2, UnifyInsts2),
        map.det_update(ThisInstPair, inst_det_known(Inst1, Detism),
            UnifyInsts2, UnifyInsts),
        inst_table_set_unify_insts(UnifyInsts, InstTable2, InstTable),
        module_info_set_inst_table(InstTable, !ModuleInfo)
    ),
    % Avoid expanding recursive insts.
    ( inst_contains_instname(Inst1, !.ModuleInfo, ThisInstPair) ->
        Inst = defined_inst(ThisInstPair)
    ;
        Inst = Inst1
    ).

:- pred abstractly_unify_inst_2(is_live::in, mer_inst::in, mer_inst::in,
    unify_is_real::in, mer_inst::out, determinism::out,
    module_info::in, module_info::out) is semidet.

abstractly_unify_inst_2(Live, InstA, InstB, Real, Inst, Detism,
        !ModuleInfo) :-
    abstractly_unify_inst_3(Live, InstA, InstB, Real, Inst,
        Detism, !ModuleInfo).

    % Abstractly unify two expanded insts.
    % The is_live parameter is `is_live' iff *both* insts are live.
    % Given the two insts to be unified, this produces
    % a resulting inst and a determinism for the unification.
    %
    % XXX Could be extended to handle `any' insts better.
    %
:- pred abstractly_unify_inst_3(is_live::in, mer_inst::in, mer_inst::in,
    unify_is_real::in, mer_inst::out, determinism::out,
    module_info::in, module_info::out) is semidet.

abstractly_unify_inst_3(Live, InstA, InstB, Real, Inst, Detism, !ModuleInfo) :-
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
                InstB = bound(UniqB, InstResultsB, BoundInstsB),
                unify_uniq(is_live, Real, detism_det, unique, UniqB, Uniq),
                % Since both are live, we must disallow free-free unifications.
                bound_inst_list_is_ground_or_any(BoundInstsB, !.ModuleInfo),
                % Since both are live, we must make the result shared
                % (unless it was already shared).
                ( ( UniqB = unique ; UniqB = mostly_unique ) ->
                    make_shared_bound_inst_list(BoundInstsB, BoundInsts,
                        !ModuleInfo)
                ;
                    BoundInsts = BoundInstsB
                ),
                Inst = bound(Uniq, InstResultsB, BoundInsts),
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
                abstractly_unify_constrained_inst_vars(Live, InstVarsB,
                    SubInstB, InstA, Real, Inst, Detism, !ModuleInfo)
            ;
                InstB = abstract_inst(_, _),
                fail
            ;
                ( InstB = defined_inst(_)
                ; InstB = free(_)
                ; InstB = inst_var(_)
                ),
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
        InstA = bound(UniqA, InstResultsA, BoundInstsA),
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
                bound_inst_list_is_ground_or_any(BoundInstsA, !.ModuleInfo),
                make_shared_bound_inst_list(BoundInstsA, BoundInsts,
                    !ModuleInfo)
            ;
                Live = is_dead,
                % Why the different argument order different to the call above?
                unify_uniq(Live, Real, detism_det, UniqA, unique, Uniq),
                BoundInsts = BoundInstsA
            ),
            Inst = bound(Uniq, InstResultsA, BoundInsts),
            Detism = detism_det
        ;
            InstB = bound(UniqB, _InstResultsB, BoundInstsB),
            abstractly_unify_bound_inst_list(Live, BoundInstsA, BoundInstsB,
                Real, BoundInsts, Detism, !ModuleInfo),
            unify_uniq(Live, Real, Detism, UniqA, UniqB, Uniq),
            % XXX A better approximation of InstResults is probably possible.
            Inst = bound(Uniq, inst_test_no_results, BoundInsts)
        ;
            InstB = ground(UniqB, _),
            unify_uniq(Live, Real, detism_semi, UniqA, UniqB, Uniq),
            (
                InstResultsA = inst_test_results_fgtc,
                Inst = InstA,
                Detism1 = detism_semi
            ;
                InstResultsA = inst_test_results(GroundnessResultA, _, _, _),
                (
                    GroundnessResultA = inst_result_is_ground,
                    Inst = InstA,
                    Detism1 = detism_semi
                ;
                    ( GroundnessResultA = inst_result_is_not_ground
                    ; GroundnessResultA = inst_result_groundness_unknown
                    ),
                    make_ground_bound_inst_list(BoundInstsA, Live, UniqB, Real,
                        BoundInsts, Detism1, !ModuleInfo),
                    Inst = bound(Uniq, InstResultsA, BoundInsts)
                )
            ;
                InstResultsA = inst_test_no_results,
                make_ground_bound_inst_list(BoundInstsA, Live, UniqB, Real,
                    BoundInsts, Detism1, !ModuleInfo),
                Inst = bound(Uniq, InstResultsA, BoundInsts)
            ),
            det_par_conjunction_detism(Detism1, detism_semi, Detism)
        ;
            InstB = any(UniqB, _),
            allow_unify_bound_any(Real),
            unify_uniq(Live, Real, detism_semi, UniqA, UniqB, Uniq),
            % XXX Should this is_live be Live?
            make_any_bound_inst_list(BoundInstsA, is_live, UniqB, Real,
                BoundInsts, Detism1, !ModuleInfo),
            % XXX A better approximation of InstResults is probably possible.
            Inst = bound(Uniq, inst_test_no_results, BoundInsts),
            det_par_conjunction_detism(Detism1, detism_semi, Detism)
        ;
            InstB = constrained_inst_vars(InstVarsB, SubInstB),
            abstractly_unify_constrained_inst_vars(Live, InstVarsB,
                SubInstB, InstA, Real, Inst, Detism, !ModuleInfo)
        ;
            InstB = abstract_inst(_N, _As),
            fail
            % Abstract insts are not supported.
            %
            % (
            %   Live = is_live,
            %   unify_uniq(is_live, Real, detism_semi, unique, UniqB, Uniq),
            %   bound_inst_list_is_ground(BoundInstsA, !.ModuleInfo),
            %   Inst = ground(shared),
            %   Detism = detism_semi
            % ;
            %   Live = is_dead,
            %   ( bound_inst_list_is_ground(BoundInstsA, !.ModuleInfo) ->
            %       Inst = bound(Uniq, BoundInstsA),
            %       Detism = semidet
            %   ; bound_inst_list_is_free(BoundInstsA, !.ModuleInfo) ->
            %       Inst = abstract_inst(N, As),
            %       Detism = det
            %   ;
            %       fail
            %   )
            % )
        ;
            ( InstB = defined_inst(_)
            ; InstB = free(_)
            ; InstB = inst_var(_)
            ),
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
            HOInstInfoA = none,
            make_ground_inst(InstB, Live, UniqA, Real, Inst, Detism,
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
                InstB = bound(UniqB, InstResultsB, BoundInstsB),
                % If Live = is_live, should we check `Real = fake_unify'?
                unify_uniq(Live, Real, detism_semi, UniqA, UniqB, Uniq),
                make_ground_bound_inst_list(BoundInstsB, Live, UniqA, Real,
                    BoundInsts, Detism1, !ModuleInfo),
                Inst = bound(Uniq, InstResultsB, BoundInsts),
                det_par_conjunction_detism(Detism1, detism_semi, Detism)
            ;
                InstB = ground(UniqB, _HOInstInfoB),
                % It is an error to unify higher-order preds,
                % so if Real \= fake_unify, then we must fail.
                Real = fake_unify,
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
                abstractly_unify_constrained_inst_vars(Live, InstVarsB,
                    SubInstB, InstA, Real, Inst, Detism, !ModuleInfo)
            ;
                InstB = abstract_inst(_N, _As),
                % Abstract insts are not supported.
                fail
            ;
                ( InstB = defined_inst(_)
                ; InstB = free(_)
                ; InstB = inst_var(_)
                ),
                % XXX Failing here preserves the old behavior of this predicate
                % for these cases, but I am not convinced it is the right thing
                % to do.
                % Why are we not handling defined_inst by looking it up?
                % Why are we not handling free/1 similarly to free/0?
                % And why are we not aborting for inst_var?
                fail
            )
        )
    ;
        InstA = any(UniqA, HOInstInfoA),
        (
            HOInstInfoA = none,
            make_any_inst(InstB, Live, UniqA, Real, Inst, Detism,
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
                InstB = bound(UniqB, _InstResultsB, BoundInstsB),
                % XXX If Live = is_live, should we test `Real = fake_unify'?
                unify_uniq(Live, Real, detism_semi, UniqA, UniqB, Uniq),
                make_any_bound_inst_list(BoundInstsB, Live, UniqA, Real,
                    BoundInsts, Detism1, !ModuleInfo),
                % XXX A better approximation of InstResults is probably
                % possible.
                Inst = bound(Uniq, inst_test_no_results, BoundInsts),
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
                abstractly_unify_constrained_inst_vars(Live, InstVarsB,
                    SubInstB, InstA, Real, Inst, Detism, !ModuleInfo)
            ;
                InstB = abstract_inst(_N, _As),
                % Abstract insts are not supported.
                fail
            ;
                ( InstB = defined_inst(_)
                ; InstB = free(_)
                ; InstB = inst_var(_)
                ),
                % XXX Failing here preserves the old behavior of this predicate
                % for these cases, but I am not convinced it is the right thing
                % to do.
                % Why are we not handling defined_inst by looking it up?
                % Why are we not handling free/1 similarly to free/0?
                % And why are we not aborting for inst_var?
                fail
            )
        )
    ;
        InstA = constrained_inst_vars(InstVarsA, SubInstA),
        abstractly_unify_constrained_inst_vars(Live, InstVarsA,
            SubInstA, InstB, Real, Inst, Detism, !ModuleInfo)
    ;
        InstA = abstract_inst(_N, _As),
        % Abstract insts are not supported.
        fail
%       (
%           Live = is_live,
%           (
%               InstB = bound(_Uniq, _BoundInstsB),
%               check_not_clobbered(Real, Uniq),
%               bound_inst_list_is_ground(BoundInstsB, !.ModuleInfo).
%               Inst = ground(shared, none),
%               Detism = detism_semi
%           ;
%               InstB = ground(_Uniq, none),
%               check_not_clobbered(Real, Uniq),
%               Inst = ground(shared, none),
%               Detism = detism_semi
%           ;
%               InstB = abstract_inst(_NameB, _ArgsB),
%               abstractly_unify_inst_list(ArgsA, ArgsB, is_live, Real,
%                   Args, Detism, !ModuleInfo),
%               Inst = abstract_inst(Name, Args)
%           )
%       ;
%           Live = is_dead,
%           (
%               InstB = bound(_, _BoundInstsB),
%               ( bound_inst_list_is_ground(BoundInstsB, ModuleInfo) ->
%                   Inst = bound(BoundInstsB),
%                   Detism = semidet
%               ; bound_inst_list_is_free(BoundInstsB, ModuleInfo) ->
%                   Inst = abstract_inst(N, As),
%                   Detism = det
%               ;
%                   fail
%               ).
%           ;
%               InstB = abstract_inst(_NameB, _ArgsB),
%               abstractly_unify_inst_list(ArgsA, ArgsB, is_dead, Real,
%                   Args, Detism, !ModuleInfo),
%               Inst = abstract_inst(Name, Args)
%           )
%       )
    ;
        ( InstA = defined_inst(_)
        ; InstA = free(_)
        ; InstA = inst_var(_)
        ),
        % XXX Failing here preserves the old behavior of this predicate
        % for these cases, but I am not convinced it is the right thing to do.
        % Why are we not handling defined_inst by looking it up?
        % Why are we not handling free/1 similarly to free/0?
        % And why are we not aborting for inst_var?
        fail
    ).

% :- pred check_not_clobbered(uniqueness::in, unify_is_real::in) is det.
%
% check_not_clobbered(Uniq, Real) :-
%     % Sanity check.
%     ( Real = real_unify, Uniq = clobbered ->
%         unexpected($module, $pred, "clobbered inst")
%     ; Real = real_unify, Uniq = mostly_clobbered ->
%         unexpected($module, $pred, "mostly_clobbered inst")
%     ;
%         true
%     ).

%-----------------------------------------------------------------------------%

    % Abstractly unify two inst lists.
    %
:- pred abstractly_unify_inst_list(list(mer_inst)::in, list(mer_inst)::in,
    is_live::in, unify_is_real::in, list(mer_inst)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

abstractly_unify_inst_list([], [], _, _, [], detism_det, !ModuleInfo).
abstractly_unify_inst_list([InstA | InstsA], [InstB | InstsB], Live, Real,
        [Inst | Insts], Detism, !ModuleInfo) :-
    abstractly_unify_inst(Live, InstA, InstB, Real, Inst,
        Detism1, !ModuleInfo),
    abstractly_unify_inst_list(InstsA, InstsB, Live, Real, Insts,
        Detism2, !ModuleInfo),
    det_par_conjunction_detism(Detism1, Detism2, Detism).

%-----------------------------------------------------------------------------%

abstractly_unify_inst_functor(Live, InstA0, ConsIdB, ArgInstsB, ArgLives,
        Real, Type, Inst, Detism, !ModuleInfo) :-
    inst_expand(!.ModuleInfo, InstA0, InstA),
    abstractly_unify_inst_functor_2(Live, InstA, ConsIdB, ArgInstsB, ArgLives,
        Real, Type, Inst, Detism, !ModuleInfo).

:- pred abstractly_unify_inst_functor_2(is_live::in, mer_inst::in,
    cons_id::in, list(mer_inst)::in, list(is_live)::in, unify_is_real::in,
    mer_type::in, mer_inst::out, determinism::out,
    module_info::in, module_info::out) is semidet.

abstractly_unify_inst_functor_2(Live, InstA, ConsIdB, ArgInstsB, ArgLives,
        Real, Type, Inst, Detism, !ModuleInfo) :-
    require_complete_switch [InstA]
    (
        InstA = not_reached,
        Inst = not_reached,
        Detism = detism_erroneous
    ;
        InstA = free,
        (
            Live = is_live,
            inst_list_is_ground_or_any_or_dead(ArgInstsB, ArgLives,
                !.ModuleInfo),
            maybe_make_shared_inst_list(ArgInstsB, ArgLives, ArgInsts,
                !ModuleInfo)
        ;
            Live = is_dead,
            ArgInsts = ArgInstsB
        ),
        % XXX A better approximation of InstResults is probably possible.
        Inst = bound(unique, inst_test_no_results,
            [bound_functor(ConsIdB, ArgInsts)]),
        Detism = detism_det
    ;
        InstA = any(Uniq, _),
        % We only allow `any' to unify with a functor if we know that
        % the type is not a solver type.
        \+ type_is_solver_type(!.ModuleInfo, Type),
        (
            Live = is_live,
            make_any_inst_list_lives(ArgInstsB, Live, ArgLives, Uniq, Real,
                ArgInsts, Detism, !ModuleInfo)
        ;
            Live = is_dead,
            make_any_inst_list(ArgInstsB, Live, Uniq, Real,
                ArgInsts, Detism, !ModuleInfo)
        ),
        % XXX A better approximation of InstResults is probably possible.
        Inst = bound(Uniq, inst_test_no_results,
            [bound_functor(ConsIdB, ArgInsts)])
    ;
        InstA = bound(UniqA, _InstResultsA, BoundInstsA),
        (
            Live = is_live,
            abstractly_unify_bound_inst_list_lives(BoundInstsA, ConsIdB,
                ArgInstsB, ArgLives, Real, BoundInsts, Detism, !ModuleInfo)
        ;
            Live = is_dead,
            BoundInstsB = [bound_functor(ConsIdB, ArgInstsB)],
            abstractly_unify_bound_inst_list(is_dead, BoundInstsA, BoundInstsB,
                Real, BoundInsts, Detism, !ModuleInfo)
        ),
        % XXX A better approximation of InstResults is probably possible.
        Inst = bound(UniqA, inst_test_no_results, BoundInsts)
    ;
        InstA = ground(UniqA, _),
        (
            Live = is_live,
            make_ground_inst_list_lives(ArgInstsB, Live, ArgLives, UniqA, Real,
                ArgInsts, Detism, !ModuleInfo)
        ;
            Live = is_dead,
            make_ground_inst_list(ArgInstsB, Live, UniqA, Real,
                ArgInsts, Detism, !ModuleInfo)
        ),
        % XXX A better approximation of InstResults is probably possible.
        Inst = bound(UniqA, inst_test_no_results,
            [bound_functor(ConsIdB, ArgInsts)])
    ;
        InstA = constrained_inst_vars(InstVars, SubInstA),
        abstractly_unify_inst_functor(Live, SubInstA, ConsIdB, ArgInstsB,
            ArgLives, Real, Type, Inst0, Detism, !ModuleInfo),
        ( inst_matches_final(Inst0, SubInstA, !.ModuleInfo) ->
            % We can keep the constrained_inst_vars.
            Inst = constrained_inst_vars(InstVars, Inst0)
        ;
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
        InstA = abstract_inst(_, _),
        fail
    ;
        ( InstA = defined_inst(_)
        ; InstA = free(_)
        ; InstA = inst_var(_)
        ),
        % XXX Failing here preserves the old behavior of this predicate
        % for these cases, but I am not convinced it is the right thing to do.
        % Why are we not handling defined_inst by looking it up?
        % Why are we not handling free/1 similarly to free/0?
        % And why are we not aborting for inst_var?
        fail
    ).

%-----------------------------------------------------------------------------%

    % This code performs abstract unification of two bound(...) insts.
    % The lists of bound_inst are guaranteed to be sorted.
    % Abstract unification of two bound(...) insts proceeds
    % like a sorted merge operation. If two elements have the
    % same functor name, they are inserted in the output list,
    % assuming their argument inst list can be abstractly unified.
    % (If it can't, the whole thing fails). If a functor name
    % occurs in only one of the two input lists, it is not inserted
    % in the output list.
    %
    % One way of looking at this code is that it simulates mode
    % and determinism checking of the goal for the unification
    % predicate for the type.
    %
:- pred abstractly_unify_bound_inst_list(is_live::in,
    list(bound_inst)::in, list(bound_inst)::in, unify_is_real::in,
    list(bound_inst)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

abstractly_unify_bound_inst_list(Live, BoundInstsA, BoundInstsB, Real,
        BoundInsts, Detism, !ModuleInfo) :-
    ( ( BoundInstsA = [] ; BoundInstsB = [] ) ->
        % This probably shouldn't happen. If we get here, it means that
        % a previous goal had determinism `failure' or `erroneous',
        % but we should have optimized away the rest of the conjunction
        % after that goal.

        BoundInsts = [],
        Detism = detism_erroneous
    ;
        abstractly_unify_bound_inst_list_2(Live, BoundInstsA, BoundInstsB,
            Real, BoundInsts, Detism0, !ModuleInfo),

        % If there are multiple alternatives for either of the inputs,
        % or the constructor of the single alternative for each input
        % doesn't match, then the unification can fail, so adjust the
        % determinism.
        (
            BoundInstsA = [bound_functor(ConsIdA, _)],
            BoundInstsB = [bound_functor(ConsIdB, _)],
            equivalent_cons_ids(ConsIdA, ConsIdB)
        ->
            Detism = Detism0
        ;
            determinism_components(Detism0, _, MaxSoln),
            determinism_components(Detism, can_fail, MaxSoln)
        )
    ).

:- pred abstractly_unify_bound_inst_list_2(is_live::in, list(bound_inst)::in,
    list(bound_inst)::in, unify_is_real::in,
    list(bound_inst)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

abstractly_unify_bound_inst_list_2(_, [], [], _, [], detism_erroneous,
        !ModuleInfo).
abstractly_unify_bound_inst_list_2(_, [], [_ | _], _, [], detism_failure,
        !ModuleInfo).
abstractly_unify_bound_inst_list_2(_, [_ | _], [], _, [], detism_failure,
        !ModuleInfo).
abstractly_unify_bound_inst_list_2(Live,
        [BoundInstA | BoundInstsA], [BoundInstB | BoundInstsB], Real,
        BoundInsts, Detism, !ModuleInfo) :-
    BoundInstA = bound_functor(ConsIdA, ArgsA),
    BoundInstB = bound_functor(ConsIdB, ArgsB),
    ( equivalent_cons_ids(ConsIdA, ConsIdB) ->
        abstractly_unify_inst_list(ArgsA, ArgsB, Live,
            Real, Args, Detism1, !ModuleInfo),
        abstractly_unify_bound_inst_list_2(Live, BoundInstsA, BoundInstsB,
            Real, BoundInstsTail, Detism2, !ModuleInfo),

        % If the unification of the two cons_ids is guaranteed
        % not to succeed, don't include it in the list.
        ( determinism_components(Detism1, _, at_most_zero) ->
            BoundInsts = BoundInstsTail
        ;
            BoundInsts = [bound_functor(ConsIdA, Args) | BoundInstsTail]
        ),
        det_switch_detism(Detism1, Detism2, Detism)
    ;
        ( compare(<, ConsIdA, ConsIdB) ->
            abstractly_unify_bound_inst_list_2(Live,
                BoundInstsA, [BoundInstB | BoundInstsB], Real, BoundInsts,
                Detism1, !ModuleInfo)
        ;
            abstractly_unify_bound_inst_list_2(Live,
                [BoundInstA | BoundInstsA], BoundInstsB, Real, BoundInsts,
                Detism1, !ModuleInfo)
        ),
        det_switch_detism(Detism1, detism_failure, Detism)
    ).

:- pred abstractly_unify_bound_inst_list_lives(list(bound_inst)::in,
    cons_id::in, list(mer_inst)::in, list(is_live)::in,
    unify_is_real::in, list(bound_inst)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

abstractly_unify_bound_inst_list_lives([], _, _, _, _, [], detism_failure,
        !ModuleInfo).
abstractly_unify_bound_inst_list_lives([BoundInstA | BoundInstsA], ConsIdB,
        ArgsB, LivesB, Real, BoundInsts, Detism, !ModuleInfo) :-
    BoundInstA = bound_functor(ConsIdA, ArgsA),
    ( equivalent_cons_ids(ConsIdA, ConsIdB) ->
        abstractly_unify_inst_list_lives(ArgsA, ArgsB, LivesB, Real, Args,
            Detism, !ModuleInfo),
        BoundInsts = [bound_functor(ConsIdA, Args)]
    ;
        abstractly_unify_bound_inst_list_lives(BoundInstsA, ConsIdB, ArgsB,
            LivesB, Real, BoundInsts, Detism, !ModuleInfo)
    ).

:- pred abstractly_unify_inst_list_lives(list(mer_inst)::in,
    list(mer_inst)::in, list(is_live)::in, unify_is_real::in,
    list(mer_inst)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

abstractly_unify_inst_list_lives([], [], [], _, [], detism_det, !ModuleInfo).
abstractly_unify_inst_list_lives([InstA | InstsA], [InstB | InstsB],
        [Live | Lives], Real, [Inst | Insts], Detism, !ModuleInfo) :-
    abstractly_unify_inst(Live, InstA, InstB, Real, Inst,
        Detism1, !ModuleInfo),
    abstractly_unify_inst_list_lives(InstsA, InstsB, Lives, Real, Insts,
        Detism2, !ModuleInfo),
    det_par_conjunction_detism(Detism1, Detism2, Detism).

%-----------------------------------------------------------------------------%

:- pred abstractly_unify_constrained_inst_vars(is_live::in, set(inst_var)::in,
    mer_inst::in, mer_inst::in, unify_is_real::in, mer_inst::out,
    determinism::out, module_info::in, module_info::out) is semidet.

abstractly_unify_constrained_inst_vars(Live, InstVarsA, SubInstA, InstB,
        Real, Inst, Detism, !ModuleInfo) :-
    abstractly_unify_inst(Live, SubInstA, InstB, Real, Inst0, Detism,
        !ModuleInfo),
    ( \+ inst_matches_final(Inst0, SubInstA, !.ModuleInfo) ->
        % The inst has become too instantiated so the
        % constrained_inst_vars wrapper must be removed.
        Inst = Inst0
    ; Inst0 = constrained_inst_vars(InstVars0, SubInst0) ->
        % Avoid nested constrained_inst_vars wrappers.
        Inst = constrained_inst_vars(set.union(InstVars0, InstVarsA), SubInst0)
    ;
        % We can keep the constrained_inst_vars wrapper.
        Inst = constrained_inst_vars(InstVarsA, Inst0)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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
    % on "real" v.s. "fake".) Note that if a unification or sub-unification
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
        ( UniqB = clobbered ->
            Uniq = clobbered
        ;
            Uniq = mostly_clobbered
        )
    ).

:- pred allow_unify_with_clobbered(is_live::in, unify_is_real::in,
    determinism::in) is semidet.

allow_unify_with_clobbered(is_live, _, _) :-
    unexpected($module, $pred, "clobbered value is is_live?").
allow_unify_with_clobbered(is_dead, fake_unify, _).
allow_unify_with_clobbered(is_dead, _, detism_det).

%-----------------------------------------------------------------------------%

:- pred make_ground_inst_list_lives(list(mer_inst)::in, is_live::in,
    list(is_live)::in, uniqueness::in, unify_is_real::in,
    list(mer_inst)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

make_ground_inst_list_lives([], _, _, _, _, [], detism_det, !ModuleInfo).
make_ground_inst_list_lives([Inst0 | Insts0], Live, [ArgLive | ArgLives],
        Uniq, Real, [Inst | Insts], Detism, !ModuleInfo) :-
    ( Live = is_live, ArgLive = is_live ->
        BothLive = is_live
    ;
        BothLive = is_dead
    ),
    make_ground_inst(Inst0, BothLive, Uniq, Real, Inst, Detism1,
        !ModuleInfo),
    make_ground_inst_list_lives(Insts0, Live, ArgLives, Uniq, Real,
        Insts, Detism2, !ModuleInfo),
    det_par_conjunction_detism(Detism1, Detism2, Detism).

:- pred make_ground_inst_list(list(mer_inst)::in, is_live::in, uniqueness::in,
    unify_is_real::in, list(mer_inst)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

make_ground_inst_list([], _, _, _, [], detism_det, !ModuleInfo).
make_ground_inst_list([Inst0 | Insts0], Live, Uniq, Real, [Inst | Insts],
        Detism, !ModuleInfo) :-
    make_ground_inst(Inst0, Live, Uniq, Real, Inst, Detism1, !ModuleInfo),
    make_ground_inst_list(Insts0, Live, Uniq, Real, Insts, Detism2,
        !ModuleInfo),
    det_par_conjunction_detism(Detism1, Detism2, Detism).

    % Abstractly unify an inst with `ground' and calculate the new inst
    % and the determinism of the unification.
    %
:- pred make_ground_inst(mer_inst::in, is_live::in, uniqueness::in,
    unify_is_real::in, mer_inst::out, determinism::out,
    module_info::in, module_info::out) is semidet.

make_ground_inst(Inst0, Live, Uniq1, Real, Inst, Detism, !ModuleInfo) :-
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
        Inst = ground(Uniq, none),
        Detism = detism_det
    ;
        Inst0 = free(T),
        unify_uniq(Live, Real, detism_det, unique, Uniq1, Uniq),
        Inst = defined_inst(typed_ground(Uniq, T)),
        Detism = detism_det
    ;
        Inst0 = bound(Uniq0, InstResults0, BoundInsts0),
        unify_uniq(Live, Real, detism_semi, Uniq0, Uniq1, Uniq),
        make_ground_bound_inst_list(BoundInsts0, Live, Uniq1, Real,
            BoundInsts, Detism1, !ModuleInfo),
        Inst = bound(Uniq, InstResults0, BoundInsts),
        det_par_conjunction_detism(Detism1, detism_semi, Detism)
    ;
        Inst0 = ground(Uniq0, HOInstInfo),
        unify_uniq(Live, Real, detism_semi, Uniq0, Uniq1, Uniq),
        Inst = ground(Uniq, HOInstInfo),
        Detism = detism_semi
    ;
        Inst0 = inst_var(_),
        unexpected($module, $pred, "free inst var")
    ;
        Inst0 = constrained_inst_vars(InstVars, SubInst0),
        abstractly_unify_constrained_inst_vars(Live, InstVars,
            SubInst0, ground(Uniq1, none), Real, Inst, Detism, !ModuleInfo)
    ;
        Inst0 = abstract_inst(_, _),
        Inst = ground(shared, none),
        Detism = detism_semi
    ;
        Inst0 = defined_inst(InstName),
        % Check whether the inst name is already in the ground_inst table.
        module_info_get_inst_table(!.ModuleInfo, InstTable0),
        inst_table_get_ground_insts(InstTable0, GroundInsts0),
        GroundInstKey = ground_inst(InstName, Live, Uniq1, Real),
        ( map.search(GroundInsts0, GroundInstKey, Result) ->
            ( Result = inst_det_known(GroundInst0, Detism0) ->
                GroundInst = GroundInst0,
                Detism = Detism0
            ;
                GroundInst = defined_inst(GroundInstKey),
                Detism = detism_det
                % We can safely assume this is det, since if it were semidet,
                % we would have noticed this in the process of unfolding the
                % definition.
            )
        ;
            % Insert the inst name in the ground_inst table, with value
            % `unknown' for the moment.
            map.det_insert(GroundInstKey, inst_det_unknown,
                GroundInsts0, GroundInsts1),
            inst_table_set_ground_insts(GroundInsts1, InstTable0, InstTable1),
            module_info_set_inst_table(InstTable1, !ModuleInfo),

            % Expand the inst name, and invoke ourself recursively on its
            % expansion.
            inst_lookup(!.ModuleInfo, InstName, SubInst0),
            inst_expand(!.ModuleInfo, SubInst0, SubInst1),
            make_ground_inst(SubInst1, Live, Uniq1, Real, GroundInst, Detism,
                !ModuleInfo),

            % Now that we have determined the resulting Inst, store the
            % appropriate value `known(GroundInst, Detism)' in the ground_inst
            % table.
            module_info_get_inst_table(!.ModuleInfo, InstTable2),
            inst_table_get_ground_insts(InstTable2, GroundInsts2),
            map.det_update(GroundInstKey, inst_det_known(GroundInst, Detism),
                GroundInsts2, GroundInsts),
            inst_table_set_ground_insts(GroundInsts, InstTable2, InstTable),
            module_info_set_inst_table(InstTable, !ModuleInfo)
        ),
        % Avoid expanding recursive insts.
        ( inst_contains_instname(GroundInst, !.ModuleInfo, GroundInstKey) ->
            Inst = defined_inst(GroundInstKey)
        ;
            Inst = GroundInst
        )
    ).

:- pred make_ground_bound_inst_list(list(bound_inst)::in, is_live::in,
    uniqueness::in, unify_is_real::in,
    list(bound_inst)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

make_ground_bound_inst_list([], _, _, _, [], detism_det, !ModuleInfo).
make_ground_bound_inst_list([BoundInst0 | BoundInsts0], Live, Uniq, Real,
            [BoundInst | BoundInsts], Detism, !ModuleInfo) :-
    BoundInst0 = bound_functor(ConsId, ArgInsts0),
    make_ground_inst_list(ArgInsts0, Live, Uniq, Real, ArgInsts,
        Detism1, !ModuleInfo),
    BoundInst = bound_functor(ConsId, ArgInsts),
    make_ground_bound_inst_list(BoundInsts0, Live, Uniq, Real, BoundInsts,
        Detism2, !ModuleInfo),
    det_par_conjunction_detism(Detism1, Detism2, Detism).

%-----------------------------------------------------------------------------%

    % Abstractly unify an inst with `any' and calculate the new inst
    % and the determinism of the unification.
    %
:- pred make_any_inst(mer_inst::in, is_live::in, uniqueness::in,
    unify_is_real::in, mer_inst::out, determinism::out,
    module_info::in, module_info::out) is semidet.

make_any_inst(Inst0, Live, Uniq1, Real, Inst, Detism, !ModuleInfo) :-
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
        Inst = any(Uniq, none),
        Detism = detism_det
    ;
        Inst0 = free(T),
        % The following is a round-about way of doing this
        %   unify_uniq(Live, Real, detism_det, unique, Uniq0, Uniq),
        %   Any = typed_any(Uniq, T).
        % without the need for a `typed_any' inst.
        Any = typed_inst(T, unify_inst(Live, free, any(Uniq1, none), Real)),
        Inst = defined_inst(Any),
        Detism = detism_det
    ;
        Inst0 = bound(Uniq0, _InstResults0, BoundInsts0),
        allow_unify_bound_any(Real),
        unify_uniq(Live, Real, detism_semi, Uniq0, Uniq1, Uniq),
        make_any_bound_inst_list(BoundInsts0, Live, Uniq1, Real, BoundInsts,
            Detism1, !ModuleInfo),
        % XXX A better approximation of InstResults is probably possible.
        Inst = bound(Uniq, inst_test_no_results, BoundInsts),
        det_par_conjunction_detism(Detism1, detism_semi, Detism)
    ;
        Inst0 = ground(Uniq0, PredInst),
        allow_unify_bound_any(Real),
        unify_uniq(Live, Real, detism_semi, Uniq0, Uniq1, Uniq),
        Inst = ground(Uniq, PredInst),
        Detism = detism_semi
    ;
        Inst0 = inst_var(_),
        unexpected($module, $pred, "free inst var")
    ;
        Inst0 = constrained_inst_vars(InstVars, SubInst0),
        abstractly_unify_constrained_inst_vars(Live, InstVars,
            SubInst0, any(Uniq1, none), Real, Inst, Detism, !ModuleInfo)
    ;
        Inst0 = abstract_inst(_, _),
        Inst = any(shared, none),
        Detism = detism_semi
    ;
        Inst0 = defined_inst(InstName),
        % Check whether the inst name is already in the any_inst table.
        module_info_get_inst_table(!.ModuleInfo, InstTable0),
        inst_table_get_any_insts(InstTable0, AnyInsts0),
        AnyInstKey = any_inst(InstName, Live, Uniq1, Real),
        ( map.search(AnyInsts0, AnyInstKey, Result) ->
            ( Result = inst_det_known(AnyInst0, Detism0) ->
                AnyInst = AnyInst0,
                Detism = Detism0
            ;
                AnyInst = defined_inst(AnyInstKey),
                Detism = detism_det
                % We can safely assume this is det, since if it were semidet,
                % we would have noticed this in the process of unfolding the
                % definition.
            )
        ;
            % Insert the inst name in the any_inst table, with value `unknown'
            % for the moment.
            map.det_insert(AnyInstKey, inst_det_unknown, AnyInsts0, AnyInsts1),
            inst_table_set_any_insts(AnyInsts1, InstTable0, InstTable1),
            module_info_set_inst_table(InstTable1, !ModuleInfo),

            % Expand the inst name, and invoke ourself recursively on its
            % expansion.
            inst_lookup(!.ModuleInfo, InstName, SubInst0),
            inst_expand(!.ModuleInfo, SubInst0, SubInst1),
            make_any_inst(SubInst1, Live, Uniq1, Real, AnyInst, Detism,
                !ModuleInfo),

            % Now that we have determined the resulting Inst, store the
            % appropriate value `known(AnyInst, Detism)' in the any_inst table.
            module_info_get_inst_table(!.ModuleInfo, InstTable2),
            inst_table_get_any_insts(InstTable2, AnyInsts2),
            map.det_update(AnyInstKey, inst_det_known(AnyInst, Detism),
                AnyInsts2, AnyInsts),
            inst_table_set_any_insts(AnyInsts, InstTable2, InstTable),
            module_info_set_inst_table(InstTable, !ModuleInfo)
        ),
        % Avoid expanding recursive insts.
        ( inst_contains_instname(AnyInst, !.ModuleInfo, AnyInstKey) ->
            Inst = defined_inst(AnyInstKey)
        ;
            Inst = AnyInst
        )
    ).

:- pred make_any_bound_inst_list(list(bound_inst)::in, is_live::in,
    uniqueness::in, unify_is_real::in,
    list(bound_inst)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

make_any_bound_inst_list([], _, _, _, [], detism_det, !ModuleInfo).
make_any_bound_inst_list([Bound0 | Bounds0], Live, Uniq, Real,
        [Bound | Bounds], Detism, !ModuleInfo) :-
    Bound0 = bound_functor(ConsId, ArgInsts0),
    make_any_inst_list(ArgInsts0, Live, Uniq, Real,
        ArgInsts, Detism1, !ModuleInfo),
    Bound = bound_functor(ConsId, ArgInsts),
    make_any_bound_inst_list(Bounds0, Live, Uniq, Real, Bounds, Detism2,
        !ModuleInfo),
    det_par_conjunction_detism(Detism1, Detism2, Detism).

:- pred make_any_inst_list(list(mer_inst)::in, is_live::in, uniqueness::in,
    unify_is_real::in, list(mer_inst)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

make_any_inst_list([], _, _, _, [], detism_det, !ModuleInfo).
make_any_inst_list([Inst0 | Insts0], Live, Uniq, Real, [Inst | Insts], Detism,
        !ModuleInfo) :-
    make_any_inst(Inst0, Live, Uniq, Real, Inst, Detism1, !ModuleInfo),
    make_any_inst_list(Insts0, Live, Uniq, Real, Insts, Detism2, !ModuleInfo),
    det_par_conjunction_detism(Detism1, Detism2, Detism).

:- pred make_any_inst_list_lives(list(mer_inst)::in, is_live::in,
    list(is_live)::in, uniqueness::in, unify_is_real::in,
    list(mer_inst)::out, determinism::out,
    module_info::in, module_info::out) is semidet.

make_any_inst_list_lives([], _, _, _, _, [], detism_det, !ModuleInfo).
make_any_inst_list_lives([Inst0 | Insts0], Live, [ArgLive | ArgLives],
        Uniq, Real, [Inst | Insts], Detism, !ModuleInfo) :-
    ( Live = is_live, ArgLive = is_live ->
        BothLive = is_live
    ;
        BothLive = is_dead
    ),
    make_any_inst(Inst0, BothLive, Uniq, Real, Inst, Detism1, !ModuleInfo),
    make_any_inst_list_lives(Insts0, Live, ArgLives, Uniq, Real,
        Insts, Detism2, !ModuleInfo),
    det_par_conjunction_detism(Detism1, Detism2, Detism).

%-----------------------------------------------------------------------------%

:- pred maybe_make_shared_inst_list(list(mer_inst)::in, list(is_live)::in,
    list(mer_inst)::out, module_info::in, module_info::out) is det.

maybe_make_shared_inst_list([], [], [], !ModuleInfo).
maybe_make_shared_inst_list([Inst0 | Insts0], [Live | Lives],
        [Inst | Insts], !ModuleInfo) :-
    (
        Live = is_live,
        make_shared_inst(Inst0, Inst, !ModuleInfo)
    ;
        Live = is_dead,
        Inst = Inst0
    ),
    maybe_make_shared_inst_list(Insts0, Lives, Insts, !ModuleInfo).
maybe_make_shared_inst_list([], [_ | _], _, _, _) :-
    unexpected($module, $pred, "length mismatch").
maybe_make_shared_inst_list([_ | _], [], _, _, _) :-
    unexpected($module, $pred, "length mismatch").

make_shared_inst_list([], [], !ModuleInfo).
make_shared_inst_list([Inst0 | Insts0], [Inst | Insts], !ModuleInfo) :-
    make_shared_inst(Inst0, Inst, !ModuleInfo),
    make_shared_inst_list(Insts0, Insts, !ModuleInfo).

    % Make an inst shared; replace all occurrences of `unique' or
    % `mostly_unique' in the inst with `shared'.
    %
:- pred make_shared_inst(mer_inst::in, mer_inst::out,
    module_info::in, module_info::out) is det.

make_shared_inst(Inst0, Inst, !ModuleInfo) :-
    (
        Inst0 = not_reached,
        Inst = Inst0
    ;
        Inst0 = free,
        % The caller should ensure that this never happens.
        unexpected($module, $pred, "cannot make shared version of `free'")
    ;
        Inst0 = free(_),
        % The caller should ensure that this never happens.
        unexpected($module, $pred, "cannot make shared version of `free(T)'")
    ;
        Inst0 = any(Uniq0, HOInstInfo),
        make_shared(Uniq0, Uniq),
        Inst = any(Uniq, HOInstInfo)
    ;
        Inst0 = bound(Uniq0, InstResults0, BoundInsts0),
        % XXX This code has a performance problem.
        %
        % The problem is that e.g. in a list of length N, you will have
        % N variables for the skeletons whose insts contain an average of
        % N/2 occurences of `bound' each, so the complexity of running
        % make_shared_inst on all their insts is quadratic in N.
        %
        % One potential way to fix this would be to introduce a new function
        % symbol for insts, make_shared(mer_inst), which would have the meaning
        % of requiring any compiler component that finds it to run
        % make_shared_inst on its argument before using it. That would require
        % parameterizing make_shared_inst to say whether it is being used
        % in such a manner.
        %
        % Another similar fix would be to add an extra argument to bound/2
        % to say whether the insts in its last argument should implicitly be
        % made shared.
        %
        % If Uniq0 = shared, then all the other cells below it should also be
        % shared as well, which means we should be able to avoid the call to
        % make_shared_bound_inst_list below. However, for the kinds of goals
        % for which the call is a bottleneck, the goals resulting from the
        % construction of large ground terms, Uniq0 will in fact be `unique'.

        make_shared(Uniq0, Uniq),
        make_shared_bound_inst_list(BoundInsts0, BoundInsts, !ModuleInfo),
        Inst = bound(Uniq, InstResults0, BoundInsts)
    ;
        Inst0 = ground(Uniq0, PredInst),
        make_shared(Uniq0, Uniq),
        Inst = ground(Uniq, PredInst)
    ;
        Inst0 = inst_var(_),
        unexpected($module, $pred, "free inst var")
    ;
        Inst0 = constrained_inst_vars(InstVars, SubInst0),
        make_shared_inst(SubInst0, SubInst1, !ModuleInfo),
        ( \+ inst_matches_final(SubInst1, SubInst0, !.ModuleInfo) ->
            Inst = SubInst1
        ;
            Inst = constrained_inst_vars(InstVars, SubInst1)
        )
    ;
        Inst0 = abstract_inst(_, _),
        unexpected($module, $pred, "abstract_inst")
    ;
        Inst0 = defined_inst(InstName),
        % Check whether the inst name is already in the shared_inst table.
        module_info_get_inst_table(!.ModuleInfo, InstTable0),
        inst_table_get_shared_insts(InstTable0, SharedInsts0),
        ( map.search(SharedInsts0, InstName, Result) ->
            ( Result = inst_known(SharedInst0) ->
                SharedInst = SharedInst0
            ;
                SharedInst = defined_inst(InstName)
            )
        ;
            % Insert the inst name in the shared_inst table, with value
            % `unknown' for the moment.
            map.det_insert(InstName, inst_unknown, SharedInsts0, SharedInsts1),
            inst_table_set_shared_insts(SharedInsts1, InstTable0, InstTable1),
            module_info_set_inst_table(InstTable1, !ModuleInfo),

            % Expand the inst name, and invoke ourself recursively on its
            % expansion.
            inst_lookup(!.ModuleInfo, InstName, SubInst0),
            inst_expand(!.ModuleInfo, SubInst0, SubInst1),
            make_shared_inst(SubInst1, SharedInst, !ModuleInfo),

            % Now that we have determined the resulting Inst, store the
            % appropriate value `known(SharedInst)' in the shared_inst table.
            module_info_get_inst_table(!.ModuleInfo, InstTable2),
            inst_table_get_shared_insts(InstTable2, SharedInsts2),
            map.det_update(InstName, inst_known(SharedInst),
                SharedInsts2, SharedInsts),
            inst_table_set_shared_insts(SharedInsts, InstTable2, InstTable),
            module_info_set_inst_table(InstTable, !ModuleInfo)
        ),
        % Avoid expanding recursive insts.
        ( inst_contains_instname(SharedInst, !.ModuleInfo, InstName) ->
            Inst = defined_inst(InstName)
        ;
            Inst = SharedInst
        )
    ).

:- pred make_shared(uniqueness::in, uniqueness::out) is det.

make_shared(unique, shared).
make_shared(mostly_unique, shared).
make_shared(shared, shared).
make_shared(mostly_clobbered, mostly_clobbered).
make_shared(clobbered, clobbered).

:- pred make_shared_bound_inst_list(list(bound_inst)::in,
    list(bound_inst)::out, module_info::in, module_info::out) is det.

make_shared_bound_inst_list([], [], !ModuleInfo).
make_shared_bound_inst_list([Bound0 | Bounds0], [Bound | Bounds],
        !ModuleInfo) :-
    Bound0 = bound_functor(ConsId, ArgInsts0),
    make_shared_inst_list(ArgInsts0, ArgInsts, !ModuleInfo),
    Bound = bound_functor(ConsId, ArgInsts),
    make_shared_bound_inst_list(Bounds0, Bounds, !ModuleInfo).

%-----------------------------------------------------------------------------%

make_mostly_uniq_inst(Inst0, Inst, !ModuleInfo) :-
    (
        ( Inst0 = not_reached
        ; Inst0 = free
        ; Inst0 = free(_)
        ),
        Inst = Inst0
    ;
        Inst0 = any(Uniq0, HOInstInfo),
        make_mostly_uniq(Uniq0, Uniq),
        Inst = any(Uniq, HOInstInfo)
    ;
        Inst0 = bound(Uniq0, _InstResults0, BoundInsts0),
        % XXX could improve efficiency by avoiding recursion here
        make_mostly_uniq(Uniq0, Uniq),
        make_mostly_uniq_bound_inst_list(BoundInsts0, BoundInsts, !ModuleInfo),
        % XXX A better approximation of InstResults is probably possible.
        Inst = bound(Uniq, inst_test_no_results, BoundInsts)
    ;
        Inst0 = ground(Uniq0, PredInst),
        make_mostly_uniq(Uniq0, Uniq),
        Inst = ground(Uniq, PredInst)
    ;
        Inst0 = inst_var(_),
        unexpected($module, $pred, "free inst var")
    ;
        Inst0 = constrained_inst_vars(InstVars, SubInst0),
        make_mostly_uniq_inst(SubInst0, SubInst, !ModuleInfo),
        ( \+ inst_matches_final(SubInst, SubInst0, !.ModuleInfo) ->
            Inst = SubInst
        ;
            Inst = constrained_inst_vars(InstVars, SubInst)
        )
    ;
        Inst0 = abstract_inst(_, _),
        unexpected($module, $pred, "abstract_inst")
    ;
        Inst0 = defined_inst(InstName),
        % Check whether the inst name is already in the mostly_uniq_inst table.
        module_info_get_inst_table(!.ModuleInfo, InstTable0),
        inst_table_get_mostly_uniq_insts(InstTable0, NondetLiveInsts0),
        ( map.search(NondetLiveInsts0, InstName, Result) ->
            ( Result = inst_known(NondetLiveInst0) ->
                NondetLiveInst = NondetLiveInst0
            ;
                NondetLiveInst = defined_inst(InstName)
            )
        ;
            % Insert the inst name in the mostly_uniq_inst table,
            % with value `unknown' for the moment.
            map.det_insert(InstName, inst_unknown,
                NondetLiveInsts0, NondetLiveInsts1),
            inst_table_set_mostly_uniq_insts(NondetLiveInsts1,
                InstTable0, InstTable1),
            module_info_set_inst_table(InstTable1, !ModuleInfo),

            % Expand the inst name, and invoke ourself recursively on its
            % expansion.
            inst_lookup(!.ModuleInfo, InstName, SubInst0),
            inst_expand(!.ModuleInfo, SubInst0, SubInst1),
            make_mostly_uniq_inst(SubInst1, NondetLiveInst, !ModuleInfo),

            % Now that we have determined the resulting Inst, store the
            % appropriate value `known(NondetLiveInst)' in the
            % mostly_uniq_inst table.
            module_info_get_inst_table(!.ModuleInfo, InstTable2),
            inst_table_get_mostly_uniq_insts(InstTable2, NondetLiveInsts2),
            map.det_update(InstName, inst_known(NondetLiveInst),
                NondetLiveInsts2, NondetLiveInsts),
            inst_table_set_mostly_uniq_insts(NondetLiveInsts,
                InstTable2, InstTable),
            module_info_set_inst_table(InstTable, !ModuleInfo)
        ),
        % Avoid expanding recursive insts.
        ( inst_contains_instname(NondetLiveInst, !.ModuleInfo, InstName) ->
            Inst = defined_inst(InstName)
        ;
            Inst = NondetLiveInst
        )
    ).

:- pred make_mostly_uniq(uniqueness::in, uniqueness::out) is det.

make_mostly_uniq(unique, mostly_unique).
make_mostly_uniq(mostly_unique, mostly_unique).
make_mostly_uniq(shared, shared).
make_mostly_uniq(mostly_clobbered, mostly_clobbered).
make_mostly_uniq(clobbered, clobbered).

:- pred make_mostly_uniq_bound_inst_list(list(bound_inst)::in,
    list(bound_inst)::out, module_info::in, module_info::out) is det.

make_mostly_uniq_bound_inst_list([], [], !ModuleInfo).
make_mostly_uniq_bound_inst_list([Bound0 | Bounds0], [Bound | Bounds],
        !ModuleInfo) :-
    Bound0 = bound_functor(ConsId, ArgInsts0),
    make_mostly_uniq_inst_list(ArgInsts0, ArgInsts, !ModuleInfo),
    Bound = bound_functor(ConsId, ArgInsts),
    make_mostly_uniq_bound_inst_list(Bounds0, Bounds, !ModuleInfo).

:- pred make_mostly_uniq_inst_list(list(mer_inst)::in, list(mer_inst)::out,
    module_info::in, module_info::out) is det.

make_mostly_uniq_inst_list([], [], !ModuleInfo).
make_mostly_uniq_inst_list([Inst0 | Insts0], [Inst | Insts], !ModuleInfo) :-
    make_mostly_uniq_inst(Inst0, Inst, !ModuleInfo),
    make_mostly_uniq_inst_list(Insts0, Insts, !ModuleInfo).

%-----------------------------------------------------------------------------%

    % Should we allow unifications between bound (or ground) insts
    % and `any' insts?
    % Previously we only allowed this for fake_unifies,
    % but now we allow it for real_unifies too.
    %
:- pred allow_unify_bound_any(unify_is_real::in) is det.

allow_unify_bound_any(_) :-
    true.

%-----------------------------------------------------------------------------%

inst_merge(InstA, InstB, MaybeType, Inst, !ModuleInfo) :-
    % Check whether this pair of insts is already in the merge_insts table.
    module_info_get_inst_table(!.ModuleInfo, InstTable0),
    inst_table_get_merge_insts(InstTable0, MergeInstTable0),
    ThisInstPair = InstA - InstB,
    ( map.search(MergeInstTable0, ThisInstPair, Result) ->
        ( Result = inst_known(MergedInst) ->
            Inst0 = MergedInst
        ;
            Inst0 = defined_inst(merge_inst(InstA, InstB))
        )
    ;
        % Insert ThisInstPair into the table with value `unknown'.
        map.det_insert(ThisInstPair, inst_unknown,
            MergeInstTable0, MergeInstTable1),
        inst_table_set_merge_insts(MergeInstTable1, InstTable0, InstTable1),
        module_info_set_inst_table(InstTable1, !ModuleInfo),

        % Merge the insts.
        inst_merge_2(InstA, InstB, MaybeType, Inst0, !ModuleInfo),

        % Now update the value associated with ThisInstPair.
        module_info_get_inst_table(!.ModuleInfo, InstTable2),
        inst_table_get_merge_insts(InstTable2, MergeInstTable2),
        map.det_update(ThisInstPair, inst_known(Inst0),
            MergeInstTable2, MergeInstTable3),
        inst_table_set_merge_insts(MergeInstTable3, InstTable2, InstTable3),
        module_info_set_inst_table(InstTable3, !ModuleInfo)
    ),
    % Avoid expanding recursive insts.
    ( inst_contains_instname(Inst0, !.ModuleInfo, merge_inst(InstA, InstB)) ->
        Inst = defined_inst(merge_inst(InstA, InstB))
    ;
        Inst = Inst0
    ).

:- pred inst_merge_2(mer_inst::in, mer_inst::in, maybe(mer_type)::in,
    mer_inst::out, module_info::in, module_info::out) is semidet.

inst_merge_2(InstA, InstB, MaybeType, Inst, !ModuleInfo) :-
%   % XXX Would this test improve efficiency?s
%   % What if we compared the addresses?
%   ( InstA = InstB ->
%       Inst = InstA,
%   ;
    inst_expand(!.ModuleInfo, InstA, ExpandedInstA),
    inst_expand(!.ModuleInfo, InstB, ExpandedInstB),
    ( ExpandedInstB = not_reached ->
        Inst = ExpandedInstA
    ; ExpandedInstA = not_reached ->
        Inst = ExpandedInstB
    ;
        inst_merge_3(ExpandedInstA, ExpandedInstB, MaybeType, Inst,
            !ModuleInfo)
    ).

:- pred inst_merge_3(mer_inst::in, mer_inst::in, maybe(mer_type)::in,
    mer_inst::out, module_info::in, module_info::out) is semidet.

inst_merge_3(InstA, InstB, MaybeType, Inst, !ModuleInfo) :-
    ( InstA = constrained_inst_vars(InstVarsA, SubInstA) ->
        ( InstB = constrained_inst_vars(InstVarsB, SubInstB) ->
            inst_merge(SubInstA, SubInstB, MaybeType, Inst0, !ModuleInfo),
            set.intersect(InstVarsA, InstVarsB, InstVars),
            ( set.is_non_empty(InstVars) ->
                Inst = constrained_inst_vars(InstVars, Inst0)
                % We can keep the constrained_inst_vars here since
                % Inst0 = SubInstA `lub` SubInstB and the original constraint
                % on the InstVars, InstC, must have been such that
                % SubInstA `lub` SubInstB =< InstC.
            ;
                Inst = Inst0
            )
        ;
            inst_merge(SubInstA, InstB, MaybeType, Inst, !ModuleInfo)
        )
    ; InstB = constrained_inst_vars(_InstVarsB, SubInstB) ->
        % InstA \= constrained_inst_vars(_, _) is equivalent to
        % constrained_inst_vars(InstVarsA, InstA) where InstVarsA = empty.
        inst_merge(InstA, SubInstB, MaybeType, Inst, !ModuleInfo)
    ;
        inst_merge_4(InstA, InstB, MaybeType, Inst, !ModuleInfo)
    ).

:- pred inst_merge_4(mer_inst::in, mer_inst::in, maybe(mer_type)::in,
    mer_inst::out, module_info::in, module_info::out) is semidet.

inst_merge_4(InstA, InstB, MaybeType, Inst, !ModuleInfo) :-
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
        InstB = bound(UniqB, _InstResultsB, BoundInstsB),
        merge_uniq_bound(UniqA, UniqB, BoundInstsB, !.ModuleInfo, Uniq),
        % We do not yet allow merge of any with free, except for
        % clobbered anys.
        ( ( Uniq = clobbered ; Uniq = mostly_clobbered ) ->
            true
        ;
            bound_inst_list_is_ground_or_any(BoundInstsB, !.ModuleInfo)
        ),
        Inst = any(Uniq, none)
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
        Inst = any(Uniq, none)
    ;
        InstA = free,
        InstB = any(Uniq, HOInstInfo),
        % We do not yet allow merge of any with free, except for
        % clobbered anys.
        ( Uniq = clobbered ; Uniq = mostly_clobbered ),
        Inst = any(Uniq, HOInstInfo)
    ;
        InstA = bound(UniqA, _InstResultsA, BoundInstsA),
        InstB = any(UniqB, _),
        merge_uniq_bound(UniqB, UniqA, BoundInstsA, !.ModuleInfo, Uniq),
        % We do not yet allow merge of any with free, except
        % for clobbered anys.
        ( ( Uniq = clobbered ; Uniq = mostly_clobbered ) ->
            true
        ;
            bound_inst_list_is_ground_or_any(BoundInstsA, !.ModuleInfo)
        ),
        Inst = any(Uniq, none)
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
        Inst = any(Uniq, none)
    ;
        InstA = free,
        InstB = free,
        Inst = free
    ;
        InstA = bound(UniqA, _InstResultsA, BoundInstsA),
        InstB = bound(UniqB, _InstResultsB, BoundInstsB),
        merge_uniq(UniqA, UniqB, Uniq),
        bound_inst_list_merge(BoundInstsA, BoundInstsB, MaybeType, BoundInsts,
            !ModuleInfo),
        % XXX A better approximation of InstResults is probably possible.
        Inst = bound(Uniq, inst_test_no_results, BoundInsts)
    ;
        InstA = bound(UniqA, InstResultsA, BoundInstsA),
        InstB = ground(UniqB, _),
        inst_merge_bound_ground(UniqA, InstResultsA, BoundInstsA, UniqB,
            MaybeType, Inst, !ModuleInfo)
    ;
        InstA = ground(UniqA, _),
        InstB = bound(UniqB, InstResultsB, BoundInstsB),
        inst_merge_bound_ground(UniqB, InstResultsB, BoundInstsB, UniqA,
            MaybeType, Inst, !ModuleInfo)
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
        MaybeTypes = list.duplicate(list.length(ArgsA), no),
        inst_list_merge(ArgsA, ArgsB, MaybeTypes, Args, !ModuleInfo),
        Inst = abstract_inst(Name, Args)
    ).

    % merge_uniq(A, B, C) succeeds if C is minimum of A and B in the ordering
    % clobbered < mostly_clobbered < shared < mostly_unique < unique.
    %
:- pred merge_uniq(uniqueness::in, uniqueness::in, uniqueness::out) is det.

merge_uniq(UniqA, UniqB, Merged) :-
    ( unique_matches_initial(UniqA, UniqB) ->       % A >= B
        Merged = UniqB
    ;
        Merged = UniqA
    ).

:- pred merge_ho_inst_info(ho_inst_info::in, ho_inst_info::in,
    ho_inst_info::out, module_info::in, module_info::out) is semidet.

merge_ho_inst_info(HOInstInfoA, HOInstInfoB, HOInstInfo, !ModuleInfo) :-
    (
        HOInstInfoA = higher_order(PredA),
        HOInstInfoB = higher_order(PredB)
    ->
        % If they specify matching pred insts, but one is more precise
        % (specifies more info) than the other, then we want to choose
        % the least precise one.
        ( pred_inst_matches(PredA, PredB, !.ModuleInfo) ->
            HOInstInfo = higher_order(PredB)
        ; pred_inst_matches(PredB, PredA, !.ModuleInfo) ->
            HOInstInfo = higher_order(PredA)
        ;
            % If either is a function inst with non-standard modes,
            % don't allow the higher-order information to be lost.
            \+ pred_inst_info_is_nonstandard_func_mode(!.ModuleInfo, PredA),
            \+ pred_inst_info_is_nonstandard_func_mode(!.ModuleInfo, PredB),
            HOInstInfo = none
        )
    ;
        \+ ho_inst_info_is_nonstandard_func_mode(!.ModuleInfo, HOInstInfoA),
        \+ ho_inst_info_is_nonstandard_func_mode(!.ModuleInfo, HOInstInfoB),
        HOInstInfo = none
    ).

    % merge_uniq_bound(UniqA, UniqB, BoundInstsB, ModuleInfo, Uniq) succeeds
    % iff Uniq is the result of merging.
    %
:- pred merge_uniq_bound(uniqueness::in, uniqueness::in, list(bound_inst)::in,
    module_info::in, uniqueness::out) is det.

merge_uniq_bound(UniqA, UniqB, BoundInstsB, ModuleInfo, Uniq) :-
    merge_uniq(UniqA, UniqB, Uniq0),
    set.init(Expansions0),
    merge_bound_inst_list_uniq(BoundInstsB, Uniq0, ModuleInfo,
        Expansions0, _Expansions, Uniq).

:- pred merge_bound_inst_list_uniq(list(bound_inst)::in, uniqueness::in,
    module_info::in, set(inst_name)::in,
    set(inst_name)::out, uniqueness::out) is det.

merge_bound_inst_list_uniq([], Uniq, _, !Expansions, Uniq).
merge_bound_inst_list_uniq([BoundInst | BoundInsts], Uniq0, ModuleInfo,
        !Expansions, Uniq) :-
    BoundInst = bound_functor(_ConsId, ArgInsts),
    merge_inst_list_uniq(ArgInsts, Uniq0, ModuleInfo, !Expansions, Uniq1),
    merge_bound_inst_list_uniq(BoundInsts, Uniq1, ModuleInfo,
        !Expansions, Uniq).

:- pred merge_inst_list_uniq(list(mer_inst)::in, uniqueness::in,
    module_info::in, set(inst_name)::in, set(inst_name)::out, uniqueness::out)
    is det.

merge_inst_list_uniq([], Uniq, _, !Expansions, Uniq).
merge_inst_list_uniq([Inst | Insts], Uniq0, ModuleInfo, !Expansions, Uniq) :-
    merge_inst_uniq(Inst, Uniq0, ModuleInfo, !Expansions, Uniq1),
    merge_inst_list_uniq(Insts, Uniq1, ModuleInfo, !Expansions, Uniq).

:- pred merge_inst_uniq(mer_inst::in, uniqueness::in, module_info::in,
    set(inst_name)::in, set(inst_name)::out, uniqueness::out) is det.

merge_inst_uniq(InstA, UniqB, ModuleInfo, !Expansions, Uniq) :-
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
        merge_bound_inst_list_uniq(BoundInstsA, Uniq0, ModuleInfo,
            !Expansions, Uniq)
    ;
        InstA = defined_inst(InstName),
        ( set.member(InstName, !.Expansions) ->
            Uniq = UniqB
        ;
            set.insert(InstName, !Expansions),
            inst_lookup(ModuleInfo, InstName, Inst),
            merge_inst_uniq(Inst, UniqB, ModuleInfo, !Expansions, Uniq)
        )
    ;
        InstA = inst_var(_),
        unexpected($module, $pred, "inst_var")
    ;
        InstA = constrained_inst_vars(_InstVars, SubInstA),
        merge_inst_uniq(SubInstA, UniqB, ModuleInfo, !Expansions, Uniq)
    ).

%-----------------------------------------------------------------------------%

:- pred inst_merge_bound_ground(uniqueness::in, inst_test_results::in,
    list(bound_inst)::in, uniqueness::in, maybe(mer_type)::in, mer_inst::out,
    module_info::in, module_info::out) is semidet.

inst_merge_bound_ground(UniqA, InstResultsA, BoundInstsA, UniqB,
        MaybeType, Result, !ModuleInfo) :-
    ( bound_inst_list_is_ground(BoundInstsA, !.ModuleInfo) ->
        merge_uniq_bound(UniqB, UniqA, BoundInstsA, !.ModuleInfo, Uniq),
        Result = ground(Uniq, none)
    ;
        bound_inst_list_is_ground_or_any(BoundInstsA, !.ModuleInfo),
        % If we know the type, we can give a more accurate result than
        % just "any".
        (
            MaybeType = yes(Type),
            type_constructors(!.ModuleInfo, Type, Constructors),
            type_to_ctor_det(Type, TypeCtor),
            constructors_to_bound_insts(!.ModuleInfo, UniqB, TypeCtor,
                Constructors, BoundInstsB0),
            list.sort_and_remove_dups(BoundInstsB0, BoundInstsB),
            InstResultsB = inst_test_results(
                inst_result_is_ground,
                inst_result_does_not_contain_any,
                inst_result_contains_instnames_known(set.init),
                inst_result_contains_types_known(set.init)
            ),
            InstA = bound(UniqA, InstResultsA, BoundInstsA),
            InstB = bound(UniqB, InstResultsB, BoundInstsB),
            inst_merge_4(InstA, InstB, MaybeType, Result, !ModuleInfo)
        ;
            MaybeType = no,
            merge_uniq_bound(UniqB, UniqA, BoundInstsA, !.ModuleInfo, Uniq),
            Result = any(Uniq, none)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred inst_list_merge(list(mer_inst)::in, list(mer_inst)::in,
    list(maybe(mer_type))::in, list(mer_inst)::out,
    module_info::in, module_info::out) is semidet.

inst_list_merge([], [], _, [], !ModuleInfo).
inst_list_merge([ArgA | ArgsA], [ArgB | ArgsB], [MaybeType | MaybeTypes],
        [Arg | Args], !ModuleInfo) :-
    inst_merge(ArgA, ArgB, MaybeType, Arg, !ModuleInfo),
    inst_list_merge(ArgsA, ArgsB, MaybeTypes, Args, !ModuleInfo).

    % bound_inst_list_merge(BoundInstsA, BoundInstsB, BoundInsts, !ModuleInfo):
    %
    % The two input lists BoundInstsA and BoundInstsB must already be sorted.
    % Here we perform a sorted merge operation,
    % so that the functors of the output list BoundInsts are the union
    % of the functors of the input lists BoundInstsA and BoundInstsB.
    %
:- pred bound_inst_list_merge(list(bound_inst)::in, list(bound_inst)::in,
    maybe(mer_type)::in, list(bound_inst)::out,
    module_info::in, module_info::out) is semidet.

bound_inst_list_merge(BoundInstsA, BoundInstsB, MaybeType, BoundInsts,
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
        ( equivalent_cons_ids(ConsIdA, ConsIdB) ->
            maybe_get_cons_id_arg_types(!.ModuleInfo, MaybeType,
                ConsIdA, list.length(ArgsA), MaybeTypes),
            inst_list_merge(ArgsA, ArgsB, MaybeTypes, Args, !ModuleInfo),
            BoundInst = bound_functor(ConsIdA, Args),
            bound_inst_list_merge(BoundInstsTailA, BoundInstsTailB, MaybeType,
                BoundInstsTail, !ModuleInfo),
            BoundInsts = [BoundInst | BoundInstsTail]
        ; compare(<, ConsIdA, ConsIdB) ->
            bound_inst_list_merge(BoundInstsTailA, BoundInstsB, MaybeType,
                BoundInstsTail, !ModuleInfo),
            BoundInsts = [BoundInstA | BoundInstsTail]
        ;
            bound_inst_list_merge(BoundInstsA, BoundInstsTailB, MaybeType,
                BoundInstsTail, !ModuleInfo),
            BoundInsts = [BoundInstB | BoundInstsTail]
        )
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

inst_contains_nonstandard_func_mode(ModuleInfo, Inst) :-
    set.init(Expansions0),
    inst_contains_nonstandard_func_mode_2(ModuleInfo, Inst, Expansions0) = yes.

:- func inst_contains_nonstandard_func_mode_2(module_info, mer_inst,
    set(inst_name)) = bool.

inst_contains_nonstandard_func_mode_2(ModuleInfo, Inst, !.Expansions)
        = ContainsNonstd :-
    (
        Inst = ground(_, HOInstInfo),
        ( ho_inst_info_is_nonstandard_func_mode(ModuleInfo, HOInstInfo) ->
            ContainsNonstd = yes
        ;
            ContainsNonstd = no
        )
    ;
        Inst = bound(_, InstResults, BoundInsts),
        (
            InstResults = inst_test_results_fgtc,
            ContainsNonstd = no
        ;
            ( InstResults = inst_test_results(_, _, _, _)
            ; InstResults = inst_test_no_results
            ),
            ContainsNonstd = bound_inst_list_contains_nonstandard_func_mode(
                ModuleInfo, BoundInsts, !.Expansions)
        )
    ;
        Inst = inst_var(_),
        unexpected($module, $pred, "uninstantiated inst parameter")
    ;
        Inst = defined_inst(InstName),
        ( set.member(InstName, !.Expansions) ->
            ContainsNonstd = no
        ;
            set.insert(InstName, !Expansions),
            inst_lookup(ModuleInfo, InstName, SubInst),
            ContainsNonstd = inst_contains_nonstandard_func_mode_2(ModuleInfo,
                SubInst, !.Expansions)
        )
    ;
        Inst = constrained_inst_vars(_, SubInst),
        % ZZZ We used to fail for this case (the equivalent of returning `no').
        ContainsNonstd = inst_contains_nonstandard_func_mode_2(ModuleInfo,
            SubInst, !.Expansions)
    ;
        ( Inst = free
        ; Inst = free(_)
        ; Inst = not_reached
        ; Inst = abstract_inst(_, _)
        ),
        ContainsNonstd = no
    ;
        Inst = any(_, _),
        % XXX This code preserves the old behavior of the predicate that
        % preceded this function, but it is arguably incorrect, since
        % any/2 insts, like ground/2 insts, contain a ho_inst_info.
        ContainsNonstd = no
    ).

:- func inst_list_contains_nonstandard_func_mode(module_info, list(mer_inst),
    set(inst_name)) = bool.

inst_list_contains_nonstandard_func_mode(_ModuleInfo, [], _Expansions) = no.
inst_list_contains_nonstandard_func_mode(ModuleInfo, [Inst | Insts],
        Expansions) = ContainsNonstd :-
    HeadContainsNonstd = inst_contains_nonstandard_func_mode_2(ModuleInfo,
        Inst, Expansions),
    (
        HeadContainsNonstd = yes,
        ContainsNonstd = yes
    ;
        HeadContainsNonstd = no,
        ContainsNonstd = inst_list_contains_nonstandard_func_mode(ModuleInfo,
            Insts, Expansions)
    ).

:- func bound_inst_list_contains_nonstandard_func_mode(module_info,
    list(bound_inst), set(inst_name)) = bool.

bound_inst_list_contains_nonstandard_func_mode(_ModuleInfo, [], _Expansions)
        = no.
bound_inst_list_contains_nonstandard_func_mode(ModuleInfo,
        [BoundInst | BoundInsts], Expansions) = ContainsNonstd :-
    BoundInst = bound_functor(_ConsId, ArgInsts),
    HeadContainsNonstd = inst_list_contains_nonstandard_func_mode(ModuleInfo,
        ArgInsts, Expansions),
    (
        HeadContainsNonstd = yes,
        ContainsNonstd = yes
    ;
        HeadContainsNonstd = no,
        ContainsNonstd = bound_inst_list_contains_nonstandard_func_mode(
            ModuleInfo, BoundInsts, Expansions)
    ).

%-----------------------------------------------------------------------------%

pred_inst_info_is_nonstandard_func_mode(ModuleInfo, PredInstInfo) :-
    PredInstInfo = pred_inst_info(pf_function, ArgModes, _, _),
    Arity = list.length(ArgModes),
    \+ pred_inst_matches(PredInstInfo,
        pred_inst_info_standard_func_mode(Arity), ModuleInfo).

ho_inst_info_is_nonstandard_func_mode(ModuleInfo, HOInstInfo) :-
    HOInstInfo = higher_order(PredInstInfo),
    pred_inst_info_is_nonstandard_func_mode(ModuleInfo, PredInstInfo).

pred_inst_info_standard_func_mode(Arity) = PredInstInfo :-
    in_mode(InMode),
    out_mode(OutMode),
    ArgModes = list.duplicate(Arity - 1, InMode) ++ [OutMode],
    PredInstInfo = pred_inst_info(pf_function, ArgModes, arg_reg_types_unset,
        detism_det).

%-----------------------------------------------------------------------------%

inst_contains_any(ModuleInfo, Inst) :-
    set.init(Expansions),
    inst_contains_any_2(ModuleInfo, Inst, Expansions) = yes.

:- func inst_contains_any_2(module_info, mer_inst, set(inst_name)) = bool.

inst_contains_any_2(ModuleInfo, Inst, !.Expansions) = ContainsAny :-
    (
        Inst = any(_, _),
        ContainsAny = yes
    ;
        Inst = bound(_, InstResults, BoundInsts),
        (
            InstResults = inst_test_results_fgtc,
            ContainsAny = no
        ;
            InstResults = inst_test_results(_, AnyResults, _, _),
            (
                AnyResults = inst_result_does_not_contain_any,
                ContainsAny = no
            ;
                AnyResults = inst_result_does_contain_any,
                ContainsAny = yes
            ;
                AnyResults = inst_result_contains_any_unknown,
                ContainsAny = bound_inst_list_contains_any(ModuleInfo,
                    BoundInsts, !.Expansions)
            )
        ;
            InstResults = inst_test_no_results,
            ContainsAny = bound_inst_list_contains_any(ModuleInfo, BoundInsts,
                !.Expansions)
        )
    ;
        Inst = inst_var(_),
        unexpected($module, $pred, "uninstantiated inst parameter")
    ;
        Inst = defined_inst(InstName),
        ( set.member(InstName, !.Expansions) ->
            ContainsAny = no
        ;
            set.insert(InstName, !Expansions),
            inst_lookup(ModuleInfo, InstName, SubInst),
            ContainsAny =
                inst_contains_any_2(ModuleInfo, SubInst, !.Expansions)
        )
    ;
        Inst = constrained_inst_vars(_, SubInst),
        % ZZZ We used to fail for this case (the equivalent of returning `no').
        ContainsAny = inst_contains_any_2(ModuleInfo, SubInst, !.Expansions)
    ;
        ( Inst = free
        ; Inst = free(_)
        ; Inst = not_reached
        ; Inst = ground(_, _)
        ; Inst = abstract_inst(_, _)
        ),
        ContainsAny = no
    ).

:- func inst_list_contains_any(module_info, list(mer_inst), set(inst_name))
    = bool.

inst_list_contains_any(_ModuleInfo, [], _Expansions) = no.
inst_list_contains_any(ModuleInfo, [Inst | Insts], Expansions) = ContainsAny :-
    HeadContainsAny = inst_contains_any_2(ModuleInfo, Inst, Expansions),
    (
        HeadContainsAny = yes,
        ContainsAny = yes
    ;
        HeadContainsAny = no,
        ContainsAny = inst_list_contains_any(ModuleInfo, Insts, Expansions)
    ).

:- func bound_inst_list_contains_any(module_info, list(bound_inst),
    set(inst_name)) = bool.

bound_inst_list_contains_any(_ModuleInfo, [], _Expansions) = no.
bound_inst_list_contains_any(ModuleInfo, [BoundInst | BoundInsts],
        Expansions) = ContainsAny :-
    BoundInst = bound_functor(_ConsId, ArgInsts),
    HeadContainsAny =
        inst_list_contains_any(ModuleInfo, ArgInsts, Expansions),
    (
        HeadContainsAny = yes,
        ContainsAny = yes
    ;
        HeadContainsAny = no,
        ContainsAny = bound_inst_list_contains_any(ModuleInfo, BoundInsts,
            Expansions)
    ).

%-----------------------------------------------------------------------------%

var_inst_contains_any(ModuleInfo, Instmap, Var) :-
    instmap_lookup_var(Instmap, Var, Inst),
    inst_contains_any(ModuleInfo, Inst).

%-----------------------------------------------------------------------------%

inst_may_restrict_cons_ids(ModuleInfo, Inst) = MayRestrict :-
    (
        ( Inst = any(_, _)
        ; Inst = bound(_, _, _)
        ; Inst = inst_var(_)
        ; Inst = constrained_inst_vars(_, _)    % XXX is this right?
        ; Inst = abstract_inst(_, _)
        ),
        MayRestrict = yes
    ;
        ( Inst = free
        ; Inst = free(_)
        ; Inst = not_reached
        ; Inst = ground(_, _)
        ),
        MayRestrict = no
    ;
        Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NewInst),
        MayRestrict = inst_may_restrict_cons_ids(ModuleInfo, NewInst)
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.inst_util.
%-----------------------------------------------------------------------------%
