%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2000-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: inst_match.m.
% Author: fjh.
%
% This module defines some utility routines for comparing insts that are used
% by modes.m and det_analysis.m.
%
% We do allow `bound' and `ground' to match `any', based on the assumption
% that `bound' and `ground' are represented in the same way as `any', i.e.
% that we use the type system rather than the mode system to distinguish
% between different representations.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.inst_match.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

    % inst_matches_initial(ModuleInfo, Type, InstA, InstB):
    %
    % Succeed iff `InstA' specifies at least as much information as `InstB',
    % and in those parts where they specify the same information, `InstA'
    % is at least as instantiated as `InstB'. Thus, the call
    % inst_matches_initial(not_reached, ground, _) succeeds, since
    % not_reached contains more information than ground - but not vice versa.
    % Similarly, inst_matches_initial(bound(a), bound(a;b), _) should
    % succeed, but not vice versa.
    %
:- pred inst_matches_initial(module_info::in, mer_type::in,
    mer_inst::in, mer_inst::in) is semidet.

    % This version of inst_matches_initial builds up a substitution map
    % (inst_var_sub). For each inst_var which occurs in InstA there will be
    % a substitution to the corresponding inst in InstB.
    %
:- pred inst_matches_initial_sub(mer_type::in, mer_inst::in, mer_inst::in,
    module_info::in, module_info::out, inst_var_sub::in, inst_var_sub::out)
    is semidet.

    % This version of inst_matches_initial does not allow implied modes.
    % This makes it almost the same as inst_matches_final. The only difference
    % is in the way it handles constrained_inst_vars.
    %
:- pred inst_matches_initial_no_implied_modes(module_info::in, mer_type::in,
    mer_inst::in, mer_inst::in) is semidet.

    % A version of the above that also computes the inst_var_sub.
    %
:- pred inst_matches_initial_no_implied_modes_sub(mer_type::in,
    mer_inst::in, mer_inst::in,
    module_info::in, module_info::out,
    inst_var_sub::in, inst_var_sub::out) is semidet.

    % inst_matches_final(ModuleInfo, InstA, InstB):
    %
    % Succeed iff InstA is compatible with InstB, i.e. iff InstA will satisfy
    % the final inst requirement InstB. This is true if the information
    % specified by InstA is at least as great as that specified by InstB,
    % and where the information is the same and both insts specify a binding,
    % the binding must be identical.
    %
:- pred inst_matches_final(module_info::in, mer_inst::in, mer_inst::in)
    is semidet.

    % This version of inst_matches_final allows you to pass in the type of the
    % variables being compared. This allows it to be more precise (i.e. less
    % conservative) for cases such as inst_matches_final(ground(...),
    % bound(...), ...). This version is to be preferred when the type is
    % available.
    %
:- pred inst_matches_final_typed(module_info::in, mer_type::in,
    mer_inst::in, mer_inst::in) is semidet.

    % Normally ground matches bound(...) only if the latter is complete for the
    % type. However, the mode checker would reject some compiler-generated
    % predicates in the absence of mode checking. We work around the problem by
    % allowing ground to match incomplete bound insts when checking the final
    % insts of those generated predicates.
    %
:- type ground_matches_bound
    --->    ground_matches_bound_if_complete
    ;       ground_matches_bound_always.

:- pred inst_matches_final_gmb(module_info::in, ground_matches_bound::in,
    mer_type::in, mer_inst::in, mer_inst::in) is semidet.

    % The difference between inst_matches_initial and inst_matches_final is
    % that inst_matches_initial requires only something which is at least as
    % instantiated, whereas this predicate wants something which is an exact
    % match (or not reachable).
    %
    % Note that this predicate is not symmetric, because of the existence of
    % `not_reached' insts: not_reached matches_final with anything, but not
    % everything matches_final with not_reached - in fact only not_reached
    % matches_final with not_reached. It is also asymmetric with respect to
    % unique insts.
    %
    % It might be a good idea to fold inst_matches_initial and
    % inst_matches_final into a single predicate inst_matches(When, ...) where
    % When is either `initial' or `final'.
    %
    % inst_is_at_least_as_instantiated(InstA, InstB, Type, ModuleInfo)
    % succeeds iff InstA is at least as instantiated as InstB. This defines
    % a partial order which is the same as inst_matches_initial except that
    % uniqueness comparisons are reversed and we don't allow
    % inst_is_at_least_as_instantiated(any, any).
    %
:- pred inst_is_at_least_as_instantiated(module_info::in, mer_type::in,
    mer_inst::in, mer_inst::in) is semidet.

    % inst_matches_binding(InstA, InstB, Type, ModuleInfo):
    %
    % Succeed iff the binding of InstA is definitely exactly the same as
    % that of InstB. This is the same as inst_matches_final except that it
    % ignores uniqueness, and that `any' does not match itself. It is used
    % to check whether variables get bound in negated contexts.
    %
:- pred inst_matches_binding(module_info::in, mer_type::in,
    mer_inst::in, mer_inst::in) is semidet.

    % inst_matches_binding_allow_any_any is the same as
    % inst_matches_binding except that it also allows `any' to match `any'.
    %
:- pred inst_matches_binding_allow_any_any(module_info::in, mer_type::in,
    mer_inst::in, mer_inst::in) is semidet.

%-----------------------------------------------------------------------------%

    % unique_matches_initial(A, B) succeeds if A >= B in the ordering
    % clobbered < mostly_clobbered < shared < mostly_unique < unique
    %
:- pred unique_matches_initial(uniqueness::in, uniqueness::in) is semidet.

    % unique_matches_final(A, B) succeeds if A >= B in the ordering
    % clobbered < mostly_clobbered < shared < mostly_unique < unique
    %
:- pred unique_matches_final(uniqueness::in, uniqueness::in) is semidet.

%-----------------------------------------------------------------------------%

    % inst_contains_nondefault_func_mode(Inst, ModuleInfo) succeeds iff the
    % inst contains a higher-order function inst that does not match the
    % default function mode `(in, ..., in) = out is det'.
    % E.g. this predicate fails for "func(in) = uo" because that matches the
    % default func mode "func(in) = out", even though it isn't the same as
    % the default func mode.
    %
:- pred inst_contains_nondefault_func_mode(module_info::in, mer_inst::in)
    is semidet.

    % Succeed iff the second argument is not a function ho_inst_info
    % whose mode does not match the default func mode.
    %
:- pred ho_inst_info_matches_ground(module_info::in, ho_inst_info::in)
    is semidet.

    % Succeed iff the second argument is not a function pred_inst_info
    % whose mode does not match the default func mode.
    %
:- pred pred_inst_matches_ground(module_info::in, pred_inst_info::in)
    is semidet.

    % pred_inst_matches(ModuleInfo, PredInstA, PredInstB)
    %
    % Succeeds if PredInstA specifies a pred that can be used wherever and
    % whenever PredInstB could be used. This is true if they both have the
    % same PredOrFunc indicator and the same determinism, and if the arguments
    % match using pred_inst_argmodes_match.
    %
:- pred pred_inst_matches(module_info::in,
    pred_inst_info::in, pred_inst_info::in) is semidet.

%-----------------------------------------------------------------------------%

    % Nondeterministically produce all the inst_vars contained in the
    % specified list of modes.
    %
:- pred mode_list_contains_inst_var(list(mer_mode)::in, inst_var::out)
    is nondet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_abstract_unify.
:- import_module check_hlds.inst_lookup.
:- import_module check_hlds.inst_merge.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_mode.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module term.

%-----------------------------------------------------------------------------%

:- type inst_match_inputs
    --->    inst_match_inputs(
                mer_inst,
                mer_inst,
                maybe(mer_type)
            ).

:- type expansions == set_tree234(inst_match_inputs).

:- func expansion_init = expansions.
:- pragma inline(func(expansion_init/0)).

expansion_init = set_tree234.init.

:- pred expansion_insert_new(inst_match_inputs::in,
    expansions::in, expansions::out) is semidet.
:- pragma inline(pred(expansion_insert_new/3)).

expansion_insert_new(E, S0, S) :-
    set_tree234.insert_new(E, S0, S).

%-----------------------------------------------------------------------------%

    % The uniqueness_comparison type is used by the predicate
    % compare_uniqueness to determine what order should be used for
    % comparing two uniqueness annotations.

:- type uniqueness_comparison
    --->    uc_match
            % We are doing a "matches" comparison, e.g. at a predicate call
            % or the end of a procedure body.
    ;       uc_instantiated.
            % We are comparing two insts for how "instantiated" they are.
            % The uniqueness order here should be the reverse of the order
            % used for matching.

:- type inst_match_info
    --->    inst_match_info(
                imi_module_info             :: module_info,
                imi_expansions              :: expansions,
                imi_maybe_sub               :: maybe_inst_var_sub,
                imi_calculate_sub           :: calculate_sub,
                imi_uniqueness_comparison   :: uniqueness_comparison,
                imi_any_matches_any         :: any_matches_any,
                imi_ground_matches_bound    :: ground_matches_bound
            ).

:- type maybe_inst_var_sub
    --->    no_inst_var_sub
    ;       inst_var_sub(inst_var_sub).

    % The calculate_sub type determines how the inst var substitution
    % should be calculated.
:- type calculate_sub
    --->    cs_forward
            % Calculate in the (normal) forward direction
            % (used by inst_matches_initial).

    ;       cs_reverse
            % Calculate in the reverse direction. Used by the call
            % to inst_matches_final from pred_inst_argmodes_match
            % to ensure contravariance of the initial argument
            % insts of higher order pred insts.

    ;       cs_none.
            % Do not calculate inst var substitution.

:- type any_matches_any
    --->    any_does_not_match_any
    ;       any_does_match_any.

:- func init_inst_match_info(module_info, maybe_inst_var_sub, calculate_sub,
    uniqueness_comparison, any_matches_any, ground_matches_bound) =
    inst_match_info.

init_inst_match_info(ModuleInfo, MaybeSub, CalculateSub, Comparison,
        AnyMatchesAny, GroundMatchesBound) =
    inst_match_info(ModuleInfo, expansion_init, MaybeSub, CalculateSub,
        Comparison, AnyMatchesAny, GroundMatchesBound).

:- type inst_matches_pred ==
    pred(maybe(mer_type), mer_inst, mer_inst,
        inst_match_info, inst_match_info).
:- inst inst_matches_pred == (pred(in, in, in, in, out) is semidet).

:- pred swap_sub(
    pred(inst_match_info, inst_match_info)::in(pred(in, out) is semidet),
    inst_match_info::in, inst_match_info::out) is semidet.

swap_sub(P, !Info) :-
    CalculateSub = !.Info ^ imi_calculate_sub,
    !Info ^ imi_calculate_sub := swap_calculate_sub(CalculateSub),
    P(!Info),
    !Info ^ imi_calculate_sub := CalculateSub.

:- pred unswap(inst_matches_pred::in(inst_matches_pred),
    maybe(mer_type)::in, mer_inst::in, mer_inst::in,
    inst_match_info::in, inst_match_info::out) is semidet.

unswap(P, Type, InstA, InstB, !Info) :-
    % Swap the arguments *and* undo swap_sub.
    CalculateSub = !.Info ^ imi_calculate_sub,
    !Info ^ imi_calculate_sub := swap_calculate_sub(CalculateSub),
    P(Type, InstB, InstA, !Info),
    !Info ^ imi_calculate_sub := CalculateSub.

:- func swap_calculate_sub(calculate_sub) = calculate_sub.

swap_calculate_sub(cs_forward) = cs_reverse.
swap_calculate_sub(cs_reverse) = cs_forward.
swap_calculate_sub(cs_none) = cs_none.

%-----------------------------------------------------------------------------%

:- pred handle_inst_var_subs(
    inst_matches_pred::in(inst_matches_pred),
    inst_matches_pred::in(inst_matches_pred),
    maybe(mer_type)::in, mer_inst::in, mer_inst::in,
    inst_match_info::in, inst_match_info::out) is semidet.

handle_inst_var_subs(Recurse, Continue, Type, InstA, InstB, !Info) :-
    CalculateSub = !.Info ^ imi_calculate_sub,
    (
        CalculateSub = cs_forward,
        handle_inst_var_subs_2(Recurse, Continue, Type, InstA, InstB, !Info)
    ;
        CalculateSub = cs_reverse,
        % Calculate the inst var substitution with arguments swapped,
        % but swap back for inst matching.
        handle_inst_var_subs_2(unswap(Recurse), unswap(Continue),
            Type, InstB, InstA, !Info)
    ;
        CalculateSub = cs_none,
        Continue(Type, InstA, InstB, !Info)
    ).

:- pred handle_inst_var_subs_2(
    inst_matches_pred::in(inst_matches_pred),
    inst_matches_pred::in(inst_matches_pred),
    maybe(mer_type)::in, mer_inst::in, mer_inst::in,
    inst_match_info::in, inst_match_info::out) is semidet.

handle_inst_var_subs_2(Recurse, Continue, Type, InstA, InstB, !Info) :-
    ( if InstB = constrained_inst_vars(InstVarsB, SubInstB) then
        % Add the substitution InstVarsB => InstA `glb` SubInstB
        % (see get_subst_inst in dmo's thesis, page 78).
        %
        % We pass `Live = is_dead' because we want
        % abstractly_unify(unique, unique) = unique, not shared.
        ModuleInfo0 = !.Info ^ imi_module_info,
        abstractly_unify_inst(is_dead, InstA, SubInstB, fake_unify,
            UnifyInst, _Det, ModuleInfo0, ModuleInfo),
        !Info ^ imi_module_info := ModuleInfo,
        update_inst_var_sub(InstVarsB, UnifyInst, Type, !Info),

        % Check that InstA matches InstB after applying the substitution
        % to InstB.
        ( if UnifyInst = constrained_inst_vars(InstVarsB, UnifySubInst) then
            % Avoid infinite regress.
            Recurse(Type, InstA, UnifySubInst, !Info)
        else
            Recurse(Type, InstA, UnifyInst, !Info)
        )
    else if InstA = constrained_inst_vars(_InstVarsA, SubInstA) then
        Recurse(Type, SubInstA, InstB, !Info)
    else
        Continue(Type, InstA, InstB, !Info)
    ).

    % Update the inst_var_sub that is computed by inst_matches_initial.
    % The inst_var_sub records what inst should be substituted for each
    % inst_var that occurs in the called procedure's argument modes.
    %
:- pred update_inst_var_sub(set(inst_var)::in, mer_inst::in,
    maybe(mer_type)::in, inst_match_info::in, inst_match_info::out) is semidet.

update_inst_var_sub(InstVars, InstA, MaybeType, !Info) :-
    (
        !.Info ^ imi_maybe_sub = inst_var_sub(_),
        set.fold(update_inst_var_sub_2(InstA, MaybeType), InstVars, !Info)
    ;
        !.Info ^ imi_maybe_sub = no_inst_var_sub
    ).

:- pred update_inst_var_sub_2(mer_inst::in, maybe(mer_type)::in, inst_var::in,
    inst_match_info::in, inst_match_info::out) is semidet.

update_inst_var_sub_2(InstA, MaybeType, InstVar, !Info) :-
    (
        !.Info ^ imi_maybe_sub = inst_var_sub(InstVarSub0),
        ( if map.search(InstVarSub0, InstVar, InstB) then
            % If InstVar already has an inst associated with it, merge
            % the old and new insts. Fail if this merge is not possible.
            ModuleInfo0 = !.Info ^ imi_module_info,
            inst_merge(InstA, InstB, MaybeType, Inst, ModuleInfo0, ModuleInfo),
            !Info ^ imi_module_info := ModuleInfo,
            map.det_update(InstVar, Inst, InstVarSub0, InstVarSub),
            !Info ^ imi_maybe_sub := inst_var_sub(InstVarSub)
        else
            map.det_insert(InstVar, InstA, InstVarSub0, InstVarSub),
            !Info ^ imi_maybe_sub := inst_var_sub(InstVarSub)
        )
    ;
        !.Info ^ imi_maybe_sub = no_inst_var_sub,
        InstVarSub = map.singleton(InstVar, InstA),
        !Info ^ imi_maybe_sub := inst_var_sub(InstVarSub)
    ).

%-----------------------------------------------------------------------------%

inst_matches_initial(ModuleInfo, Type, InstA, InstB) :-
    inst_matches_initial_1(Type, InstA, InstB,
        ModuleInfo, _, no_inst_var_sub, _).

inst_matches_initial_sub(Type, InstA, InstB, !ModuleInfo, !Sub) :-
    inst_matches_initial_1(Type, InstA, InstB, !ModuleInfo,
        inst_var_sub(!.Sub), MaybeSub),
    (
        MaybeSub = inst_var_sub(!:Sub)
    ;
        MaybeSub = no_inst_var_sub,
        unexpected($pred, "missing inst_var_sub")
    ).

inst_matches_initial_no_implied_modes(ModuleInfo, Type, InstA, InstB) :-
    Info0 = init_inst_match_info(ModuleInfo, no_inst_var_sub, cs_forward,
        uc_match, any_does_match_any, ground_matches_bound_if_complete),
    inst_matches_final_mt(yes(Type), InstA, InstB, Info0, _).

inst_matches_initial_no_implied_modes_sub(Type, InstA, InstB,
        !ModuleInfo, !Sub) :-
    Info0 = init_inst_match_info(!.ModuleInfo, inst_var_sub(!.Sub), cs_forward,
        uc_match, any_does_match_any, ground_matches_bound_if_complete),
    inst_matches_final_mt(yes(Type), InstA, InstB, Info0, Info),
    !:ModuleInfo = Info ^ imi_module_info,
    inst_var_sub(!:Sub) = Info ^ imi_maybe_sub.

:- pred inst_matches_initial_1(mer_type::in, mer_inst::in, mer_inst::in,
    module_info::in, module_info::out,
    maybe_inst_var_sub::in, maybe_inst_var_sub::out) is semidet.

inst_matches_initial_1(Type, InstA, InstB, !ModuleInfo, !MaybeSub) :-
    Info0 = init_inst_match_info(!.ModuleInfo, !.MaybeSub, cs_forward,
        uc_match, any_does_match_any, ground_matches_bound_if_complete),
    inst_matches_initial_mt(yes(Type), InstA, InstB, Info0, Info),
    !:ModuleInfo = Info ^ imi_module_info,
    !:MaybeSub = Info ^ imi_maybe_sub.

:- pred inst_matches_initial_mt(maybe(mer_type)::in,
    mer_inst::in, mer_inst::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_initial_mt(MaybeType, InstA, InstB, !Info) :-
    ThisExpansion = inst_match_inputs(InstA, InstB, MaybeType),
    Expansions0 = !.Info ^ imi_expansions,
    ( if expansion_insert_new(ThisExpansion, Expansions0, Expansions) then
        !Info ^ imi_expansions := Expansions,
        inst_expand(!.Info ^ imi_module_info, InstA, ExpandedInstA),
        inst_expand(!.Info ^ imi_module_info, InstB, ExpandedInstB),
        handle_inst_var_subs(inst_matches_initial_mt, inst_matches_initial_4,
            MaybeType, ExpandedInstA, ExpandedInstB, !Info)
    else
        true
    ).

:- pred inst_matches_initial_4(maybe(mer_type)::in, mer_inst::in, mer_inst::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_initial_4(MaybeType, InstA, InstB, !Info) :-
    % To avoid infinite regress, we assume that inst_matches_initial is true
    % for any pairs of insts which occur in `Expansions'.
    %
    % XXX Maybe we could use the inst result field of bound/3 insts
    % in some places.
    (
        InstA = any(UniqA, HOInstInfoA),
        (
            InstB = any(UniqB, HOInstInfoB),
            !.Info ^ imi_any_matches_any = any_does_match_any,
            compare_uniqueness(!.Info ^ imi_uniqueness_comparison,
                UniqA, UniqB),
            ho_inst_info_matches_initial(MaybeType, HOInstInfoA, HOInstInfoB,
                !Info)
        ;
            InstB = free
        ;
            InstB = ground(_, _),
            maybe_any_to_bound(!.Info ^ imi_module_info, MaybeType, UniqA,
                HOInstInfoA, NextInstA),
            inst_matches_initial_mt(MaybeType, NextInstA, InstB, !Info)
        ;
            InstB = bound(_, _, _),
            maybe_any_to_bound(!.Info ^ imi_module_info, MaybeType, UniqA,
                HOInstInfoA, NextInstA),
            inst_matches_initial_mt(MaybeType, NextInstA, InstB, !Info)
        )
    ;
        InstA = free,
        InstB = free
    ;
        InstA = bound(UniqA, _InstResultsA, BoundInstsA),
        (
            InstB = any(UniqB, none_or_default_func),
            compare_uniqueness(!.Info ^ imi_uniqueness_comparison,
                UniqA, UniqB),
            compare_bound_inst_list_uniq(!.Info ^ imi_module_info,
                !.Info ^ imi_uniqueness_comparison, BoundInstsA, UniqB),
            inst_contains_nondefault_func_mode_1(InstA, no, !Info)
        ;
            InstB = free
        )
    ;
        InstA = bound(UniqA, InstResultsA, BoundInstsA),
        (
            InstB = bound(UniqB, _InstResultsB, BoundInstsB),
            ( if
                same_addr_insts(InstA, InstB),
                InstResultsA = inst_test_results_fgtc
            then
                true
            else
                compare_uniqueness(!.Info ^ imi_uniqueness_comparison,
                    UniqA, UniqB),
                bound_inst_list_matches_initial_mt(MaybeType,
                    BoundInstsA, BoundInstsB, !Info)
            )
        ;
            InstB = ground(UniqB, none_or_default_func),
            compare_uniqueness(!.Info ^ imi_uniqueness_comparison,
                UniqA, UniqB),
            inst_results_bound_inst_list_is_ground_mt(!.Info ^ imi_module_info,
                MaybeType, InstResultsA, BoundInstsA),
            compare_bound_inst_list_uniq(!.Info ^ imi_module_info,
                !.Info ^ imi_uniqueness_comparison, BoundInstsA, UniqB),
            inst_contains_nondefault_func_mode_1(InstA, no, !Info)
        ;
            InstB = abstract_inst(_,_),
            inst_results_bound_inst_list_is_ground_mt(!.Info ^ imi_module_info,
                no, InstResultsA, BoundInstsA),
            (
                UniqA = unique,
                bound_inst_list_is_unique(!.Info ^ imi_module_info,
                    BoundInstsA)
            ;
                UniqA = mostly_unique,
                bound_inst_list_is_mostly_unique(!.Info ^ imi_module_info,
                    BoundInstsA)
            ),
            inst_contains_nondefault_func_mode_1(InstA, no, !Info)
        )
    ;
        InstA = ground(UniqA, HOInstInfoA),
        (
            InstB = any(UniqB, HOInstInfoB),
            compare_uniqueness(!.Info ^ imi_uniqueness_comparison,
                UniqA, UniqB),
            ho_inst_info_matches_initial(MaybeType, HOInstInfoA, HOInstInfoB,
                !Info)
        ;
            InstB = free
        ;
            InstB = bound(UniqB, _InstResultsB, BoundInstsB),
            MaybeType = yes(Type),
            % We can only check this case properly if the type is known.
            compare_uniqueness(!.Info ^ imi_uniqueness_comparison,
                UniqA, UniqB),
            bound_inst_list_is_complete_for_type(!.Info ^ imi_module_info,
                set.init, Type, BoundInstsB),
            ground_matches_initial_bound_inst_list(UniqA, yes(Type),
                BoundInstsB, !Info)
        ;
            InstB = ground(UniqB, HOInstInfoB),
            compare_uniqueness(!.Info ^ imi_uniqueness_comparison,
                UniqA, UniqB),
            ho_inst_info_matches_initial(MaybeType, HOInstInfoA, HOInstInfoB,
                !Info)
        ;
            InstB = abstract_inst(_,_),
            HOInstInfoA = none_or_default_func,
            % I don't know what this should do.
            % Abstract insts aren't really supported.
            unexpected($pred,
                "inst_matches_initial(ground, abstract_inst) == ??")
        )
    ;
        InstA = abstract_inst(Name, ArgsA),
        (
            InstB = any(shared, none_or_default_func)
        ;
            InstB = free
        ;
            InstB = abstract_inst(Name, ArgsB),
            list.duplicate(length(ArgsA), no, MaybeTypes),
            % XXX how do we get the argument types for an abstract inst?
            inst_list_matches_initial_mt(MaybeTypes, ArgsA, ArgsB, !Info)
        )
    ;
        InstA = not_reached
    ).

%-----------------------------------------------------------------------------%

    % This predicate assumes that the check of
    % `bound_inst_list_is_complete_for_type' is done by the caller.
    %
:- pred ground_matches_initial_bound_inst_list(uniqueness::in,
    maybe(mer_type)::in, list(bound_inst)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

ground_matches_initial_bound_inst_list(_, _, [], !Info).
ground_matches_initial_bound_inst_list(Uniq, MaybeType,
        [BoundInst | BoundInsts], !Info) :-
    BoundInst = bound_functor(ConsId, Args),
    maybe_get_cons_id_arg_types(!.Info ^ imi_module_info, MaybeType, ConsId,
        list.length(Args), MaybeTypes),
    ground_matches_initial_inst_list(Uniq, MaybeTypes, Args, !Info),
    ground_matches_initial_bound_inst_list(Uniq, MaybeType, BoundInsts, !Info).

:- pred ground_matches_initial_inst_list(uniqueness::in,
    list(maybe(mer_type))::in, list(mer_inst)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

ground_matches_initial_inst_list(_, [], [], !Info).
ground_matches_initial_inst_list(Uniq,
        [MaybeType | MaybeTypes], [Inst | Insts], !Info) :-
    Ground = ground(Uniq, none_or_default_func),
    inst_matches_initial_mt(MaybeType, Ground, Inst, !Info),
    ground_matches_initial_inst_list(Uniq, MaybeTypes, Insts, !Info).

%-----------------------------------------------------------------------------%

    % A list(bound_inst) is ``complete'' for a given type iff it includes
    % each functor of the type and each argument of each functor is also
    % ``complete'' for its type.
    %
:- pred bound_inst_list_is_complete_for_type(module_info::in,
    set(inst_name)::in, mer_type::in, list(bound_inst)::in) is semidet.

bound_inst_list_is_complete_for_type(ModuleInfo, Expansions, Type,
        BoundInsts) :-
    % Is this a type for which cons_ids are recorded in the type_table?
    type_util.cons_id_arg_types(ModuleInfo, Type, _, _),

    % Is there a bound_inst for each cons_id in the type_table?
    % XXX This code has a potential performance problem. If the type has
    % N cons_ids, then this code can do N invocations of list.member,
    % each of which has O(N) complexity, for an overall complexity of O(N^2).
    % We should fix this by taking advantage of the fact that BoundInsts
    % should be sorted.
    all [ConsId, ArgTypes] (
        type_util.cons_id_arg_types(ModuleInfo, Type, ConsId, ArgTypes)
    =>
        (
            list.member(bound_functor(ConsId0, ArgInsts), BoundInsts),
            % Cons_ids returned from type_util.cons_id_arg_types
            % are not module-qualified, so we need to call
            % equivalent_cons_ids instead of just using `=/2'.
            equivalent_cons_ids(ConsId0, ConsId),
            list.map(inst_is_complete_for_type(ModuleInfo, Expansions),
                ArgTypes, ArgInsts)
        )
    ).

:- pred inst_is_complete_for_type(module_info::in, set(inst_name)::in,
    mer_type::in, mer_inst::in) is semidet.

inst_is_complete_for_type(ModuleInfo, Expansions, Type, Inst) :-
    require_complete_switch [Inst]
    (
        Inst = defined_inst(Name),
        ( if set.member(Name, Expansions) then
            true
        else
            inst_lookup(ModuleInfo, Name, ExpandedInst),
            inst_is_complete_for_type(ModuleInfo, set.insert(Expansions, Name),
                Type, ExpandedInst)
        )
    ;
        Inst = bound(_, _, BoundInsts),
        bound_inst_list_is_complete_for_type(ModuleInfo, Expansions,
            Type, BoundInsts)
    ;
        % XXX This switch was originally an if-then-else chain, with explicit
        % tests for defined_inst and bound, and the final else case being
        % simply "Inst \= not_reached". However, several of the Inst values
        % for which this succeeds look extremely dodgy. For example, I (zs)
        % am pretty much certain that constrained_inst_vars should recurse
        % on its sub-inst instead of succeeding.
        %
        % On the other hand, it is possible that some or all of the Inst values
        % for which we do the wrong thing here will never be given to us
        % by our callers.
        ( Inst = free
        ; Inst = free(_)
        ; Inst = any(_, _)
        ; Inst = ground(_, _)
        ; Inst = inst_var(_)
        ; Inst = constrained_inst_vars(_, _)
        ; Inst = abstract_inst(_, _)
        )
    ;
        Inst = not_reached,
        fail
    ).

    % Check that the first cons_id is lexically greater than the second,
    % after all module qualifiers have been removed.
    %
:- pred first_unqual_cons_id_is_greater(cons_id::in, cons_id::in) is semidet.

first_unqual_cons_id_is_greater(ConsIdA, ConsIdB) :-
    ( if
        ConsIdA = cons(QNameA, ArityA, _),
        ConsIdB = cons(QNameB, ArityB, _)
    then
        ( QNameA = unqualified(NameA)
        ; QNameA = qualified(_, NameA)
        ),
        ( QNameB = unqualified(NameB)
        ; QNameB = qualified(_, NameB)
        ),
        compare(O, NameA, NameB),
        (
            O = (>)
        ;
            O = (=),
            ArityA > ArityB
        )
    else
        compare((>), ConsIdA, ConsIdB)
    ).

%-----------------------------------------------------------------------------%

    % This predicate checks if two ho_inst_infos match_initial.
    % It does not check uniqueness.
    %
:- pred ho_inst_info_matches_initial(maybe(mer_type)::in,
    ho_inst_info::in, ho_inst_info::in,
    inst_match_info::in, inst_match_info::out) is semidet.

ho_inst_info_matches_initial(MaybeType, HOInstInfoA, HOInstInfoB, !Info) :-
    (
        HOInstInfoB = none_or_default_func,
        ho_inst_info_matches_ground_2(MaybeType, HOInstInfoA, !Info)
    ;
        HOInstInfoB = higher_order(PredInstB),
        (
            HOInstInfoA = none_or_default_func,
            PredInstB = pred_inst_info(pf_function, ArgModes, _, _Det),
            Arity = list.length(ArgModes),
            PredInstA = pred_inst_info_default_func_mode(Arity),
            pred_inst_matches_2(MaybeType, PredInstA, PredInstB, !Info)
        ;
            HOInstInfoA = higher_order(PredInstA),
            pred_inst_matches_2(MaybeType, PredInstA, PredInstB, !Info)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred compare_bound_inst_list_uniq(module_info::in,
    uniqueness_comparison::in,
    list(bound_inst)::in, uniqueness::in) is semidet.

compare_bound_inst_list_uniq(ModuleInfo, uc_match, BoundInsts, Uniq) :-
    bound_inst_list_matches_uniq(ModuleInfo, Uniq, BoundInsts).
compare_bound_inst_list_uniq(ModuleInfo, uc_instantiated, BoundInsts, Uniq) :-
    uniq_matches_bound_inst_list(ModuleInfo, Uniq, BoundInsts).

:- pred bound_inst_list_matches_uniq(module_info::in, uniqueness::in,
    list(bound_inst)::in) is semidet.

bound_inst_list_matches_uniq(ModuleInfo, Uniq, BoundInsts) :-
    ( if Uniq = unique then
        bound_inst_list_is_unique(ModuleInfo, BoundInsts)
    else if Uniq = mostly_unique then
        bound_inst_list_is_mostly_unique(ModuleInfo, BoundInsts)
    else
        true
    ).

:- pred uniq_matches_bound_inst_list(module_info::in, uniqueness::in,
    list(bound_inst)::in) is semidet.

uniq_matches_bound_inst_list(ModuleInfo, Uniq, BoundInsts) :-
    ( if Uniq = shared then
        bound_inst_list_is_not_partly_unique(ModuleInfo, BoundInsts)
    else if Uniq = mostly_unique then
        bound_inst_list_is_not_fully_unique(ModuleInfo, BoundInsts)
    else
        true
    ).

%-----------------------------------------------------------------------------%

    % Here we check that the functors in the first list are a subset of the
    % functors in the second list. (If a bound(...) inst only specifies the
    % insts for some of the constructors of its type, then it implicitly means
    % that all other constructors must have all their arguments `not_reached'.)
    % The code here makes use of the fact that the bound_inst lists are sorted.
    %
:- pred bound_inst_list_matches_initial_mt(maybe(mer_type)::in,
    list(bound_inst)::in, list(bound_inst)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

bound_inst_list_matches_initial_mt(_, [], _, !Info).
bound_inst_list_matches_initial_mt(MaybeType, [X | Xs], [Y | Ys], !Info) :-
    X = bound_functor(ConsIdX, ArgsX),
    Y = bound_functor(ConsIdY, ArgsY),
    ( if equivalent_cons_ids(ConsIdX, ConsIdY) then
        maybe_get_cons_id_arg_types(!.Info ^ imi_module_info, MaybeType,
            ConsIdX, list.length(ArgsX), MaybeTypes),
        inst_list_matches_initial_mt(MaybeTypes, ArgsX, ArgsY, !Info),
        bound_inst_list_matches_initial_mt(MaybeType, Xs, Ys, !Info)
    else
        first_unqual_cons_id_is_greater(ConsIdX, ConsIdY),
        % ConsIdY does not occur in [X | Xs].
        % Hence [X | Xs] implicitly specifies `not_reached' for the args
        % of ConsIdY, and hence automatically matches_initial Y. We just
        % need to check that [X | Xs] matches_initial Ys.
        bound_inst_list_matches_initial_mt(MaybeType, [X | Xs], Ys, !Info)
    ).

:- pred inst_list_matches_initial_mt(list(maybe(mer_type))::in,
    list(mer_inst)::in, list(mer_inst)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_list_matches_initial_mt([], [], [], !Info).
inst_list_matches_initial_mt([MaybeType | MaybeTypes], [X | Xs], [Y | Ys],
        !Info) :-
    inst_matches_initial_mt(MaybeType, X, Y, !Info),
    inst_list_matches_initial_mt(MaybeTypes, Xs, Ys, !Info).

%-----------------------------------------------------------------------------%

inst_matches_final(ModuleInfo, InstA, InstB) :-
    Info0 = init_inst_match_info(ModuleInfo, no_inst_var_sub, cs_none,
        uc_match, any_does_match_any, ground_matches_bound_if_complete),
    inst_matches_final_mt(no, InstA, InstB, Info0, _).

inst_matches_final_typed(ModuleInfo, Type, InstA, InstB) :-
    inst_matches_final_gmb(ModuleInfo, ground_matches_bound_if_complete,
        Type, InstA, InstB).

inst_matches_final_gmb(ModuleInfo, GroundMatchesBound, Type, InstA, InstB) :-
    Info0 = init_inst_match_info(ModuleInfo, no_inst_var_sub, cs_none,
        uc_match, any_does_match_any, GroundMatchesBound),
    inst_matches_final_mt(yes(Type), InstA, InstB, Info0, _).

:- pred inst_matches_final_mt(maybe(mer_type)::in, mer_inst::in, mer_inst::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_final_mt(MaybeType, InstA, InstB, !Info) :-
    ( if InstA = InstB then
        true
    else
        ThisExpansion = inst_match_inputs(InstA, InstB, MaybeType),
        Expansions0 = !.Info ^ imi_expansions,
        ( if expansion_insert_new(ThisExpansion, Expansions0, Expansions) then
            !Info ^ imi_expansions := Expansions,
            inst_expand(!.Info ^ imi_module_info, InstA, ExpandedInstA),
            inst_expand(!.Info ^ imi_module_info, InstB, ExpandedInstB),
            handle_inst_var_subs(inst_matches_final_mt, inst_matches_final_3,
                MaybeType, ExpandedInstA, ExpandedInstB, !Info)
        else
            true
        )
    ).

:- pred inst_matches_final_3(maybe(mer_type)::in, mer_inst::in, mer_inst::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_final_3(MaybeType, InstA, InstB, !Info) :-
    (
        InstA = any(UniqA, HOInstInfoA),
        (
            InstB = any(UniqB, HOInstInfoB),
            ho_inst_info_matches_final(MaybeType, HOInstInfoA, HOInstInfoB,
                !Info),
            unique_matches_final(UniqA, UniqB)
        ;
            InstB = ground(_, _),
            maybe_any_to_bound(!.Info ^ imi_module_info, MaybeType, UniqA,
                HOInstInfoA, NextInstA),
            inst_matches_final_mt(MaybeType, NextInstA, InstB, !Info)
        ;
            InstB = bound(_, _, _),
            maybe_any_to_bound(!.Info ^ imi_module_info, MaybeType, UniqA,
                HOInstInfoA, NextInstA),
            inst_matches_final_mt(MaybeType, NextInstA, InstB, !Info)
        )
    ;
        InstA = free,
        (
            InstB = any(Uniq, _),
            % We do not yet allow `free' to match `any',
            % unless the `any' is `clobbered_any' or `mostly_clobbered_any'.
            % Among other things, changing this would break compare_inst
            % in modecheck_call.m.
            ( Uniq = clobbered ; Uniq = mostly_clobbered )
        ;
            InstB = free
        )
    ;
        InstA = bound(UniqA, InstResultsA, BoundInstsA),
        (
            InstB = any(UniqB, none_or_default_func),
            unique_matches_final(UniqA, UniqB),
            bound_inst_list_matches_uniq(!.Info ^ imi_module_info, UniqB,
                BoundInstsA),
            % We do not yet allow `free' to match `any'.
            % Among other things, changing this would break compare_inst
            % in modecheck_call.m.
            inst_results_bound_inst_list_is_ground_or_any(
                !.Info ^ imi_module_info, InstResultsA, BoundInstsA),
            inst_contains_nondefault_func_mode_1(InstA, no, !Info)
        ;
            InstB = bound(UniqB, _InstResultsB, BoundInstsB),
            unique_matches_final(UniqA, UniqB),
            bound_inst_list_matches_final(MaybeType, BoundInstsA, BoundInstsB,
                !Info)
        ;
            InstB = ground(UniqB, none_or_default_func),
            unique_matches_final(UniqA, UniqB),
            inst_results_bound_inst_list_is_ground_mt(!.Info ^ imi_module_info,
                MaybeType, InstResultsA, BoundInstsA),
            bound_inst_list_matches_uniq(!.Info ^ imi_module_info, UniqB,
                BoundInstsA),
            inst_contains_nondefault_func_mode_1(InstA, no, !Info)
        )
    ;
        InstA = ground(UniqA, HOInstInfoA),
        (
            InstB = any(UniqB, HOInstInfoB),
            ho_inst_info_matches_final(MaybeType, HOInstInfoA, HOInstInfoB,
                !Info),
            unique_matches_final(UniqA, UniqB)
        ;
            InstB = bound(UniqB, InstResultsB, BoundInstsB),
            ho_inst_info_matches_ground_2(MaybeType, HOInstInfoA, !Info),
            unique_matches_final(UniqA, UniqB),
            ModuleInfo = !.Info ^ imi_module_info,
            inst_results_bound_inst_list_is_ground_mt(ModuleInfo, MaybeType,
                InstResultsB, BoundInstsB),
            uniq_matches_bound_inst_list(ModuleInfo, UniqA, BoundInstsB),
            inst_contains_nondefault_func_mode_1(InstB, no, !Info),
            (
                MaybeType = yes(Type),
                % We can only do this check if the type is known.
                bound_inst_list_is_complete_for_type(ModuleInfo, set.init,
                    Type, BoundInstsB)
            ;
                % XXX the check for bound_inst_list_is_complete_for_type
                % makes the mode checker too conservative in the absence
                % of alias tracking. Bypass the check if instructed.
                GroundMatchesBound = !.Info ^ imi_ground_matches_bound,
                GroundMatchesBound = ground_matches_bound_always
            )
        ;
            InstB = ground(UniqB, HOInstInfoB),
            ho_inst_info_matches_final(MaybeType, HOInstInfoA, HOInstInfoB,
                !Info),
            unique_matches_final(UniqA, UniqB)
        )
    ;
        InstA = abstract_inst(Name, ArgsA),
        (
            InstB = any(shared, none_or_default_func)
        ;
            InstB = abstract_inst(Name, ArgsB),
            list.duplicate(list.length(ArgsA), no, MaybeTypes),
            % XXX how do we get the argument types for an abstract inst?
            inst_list_matches_final(MaybeTypes, ArgsA, ArgsB, !Info)
        )
    ;
        InstA = not_reached
    ;
        InstA = constrained_inst_vars(InstVarsA, SubInstA),
        ( if InstB = constrained_inst_vars(InstVarsB, SubInstB) then
            % Constrained_inst_vars match_final only if InstVarsA contains
            % all the variables in InstVarsB.
            set.subset(InstVarsB, InstVarsA),
            inst_matches_final_mt(MaybeType, SubInstA, SubInstB, !Info)
        else
            inst_matches_final_mt(MaybeType, SubInstA, InstB, !Info)
        )
    ).

:- pred ho_inst_info_matches_final(maybe(mer_type)::in,
    ho_inst_info::in, ho_inst_info::in,
    inst_match_info::in, inst_match_info::out) is semidet.

ho_inst_info_matches_final(MaybeType, HOInstInfoA, HOInstInfoB, !Info) :-
    (
        HOInstInfoB = none_or_default_func,
        ho_inst_info_matches_ground_2(MaybeType, HOInstInfoA, !Info)
    ;
        HOInstInfoB = higher_order(PredInstB),
        (
            HOInstInfoA = none_or_default_func,
            PredInstB = pred_inst_info(pf_function, ArgModes, _, _Det),
            list.length(ArgModes, Arity),
            PredInstA = pred_inst_info_default_func_mode(Arity),
            pred_inst_matches_2(MaybeType, PredInstA, PredInstB, !Info)
        ;
            HOInstInfoA = higher_order(PredInstA),
            pred_inst_matches_2(MaybeType, PredInstA, PredInstB, !Info)
        )
    ).

:- pred inst_list_matches_final(list(maybe(mer_type))::in,
    list(mer_inst)::in, list(mer_inst)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_list_matches_final([], [], [], !Info).
inst_list_matches_final([MaybeType | MaybeTypes],
        [ArgA | ArgsA], [ArgB | ArgsB], !Info) :-
    inst_matches_final_mt(MaybeType, ArgA, ArgB, !Info),
    inst_list_matches_final(MaybeTypes, ArgsA, ArgsB, !Info).

    % Here we check that the functors in the first list are a subset of the
    % functors in the second list. (If a bound(...) inst only specifies
    % the insts for some of the constructors of its type, then it implicitly
    % means that all other constructors must have all their arguments
    % `not_reached'.) The code here makes use of the fact that the bound_inst
    % lists are sorted.
    %
:- pred bound_inst_list_matches_final(maybe(mer_type)::in,
    list(bound_inst)::in, list(bound_inst)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

bound_inst_list_matches_final(_, [], _, !Info).
bound_inst_list_matches_final(MaybeType, [X | Xs], [Y | Ys], !Info) :-
    X = bound_functor(ConsIdX, ArgInstsX),
    Y = bound_functor(ConsIdY, ArgInstsY),
    ( if equivalent_cons_ids(ConsIdX, ConsIdY) then
        maybe_get_cons_id_arg_types(!.Info ^ imi_module_info, MaybeType,
            ConsIdX, list.length(ArgInstsX), MaybeTypes),
        inst_list_matches_final(MaybeTypes, ArgInstsX, ArgInstsY, !Info),
        bound_inst_list_matches_final(MaybeType, Xs, Ys, !Info)
    else
        first_unqual_cons_id_is_greater(ConsIdX, ConsIdY),
        % ConsIdY does not occur in [X | Xs].
        % Hence [X | Xs] implicitly specifies `not_reached' for the args
        % of ConsIdY, and hence automatically matches_final Y. We just
        % need to check that [X | Xs] matches_final Ys.
        bound_inst_list_matches_final(MaybeType, [X | Xs], Ys, !Info)
    ).

inst_is_at_least_as_instantiated(ModuleInfo, Type, InstA, InstB) :-
    Info0 = init_inst_match_info(ModuleInfo, no_inst_var_sub, cs_none,
        uc_instantiated, any_does_not_match_any,
        ground_matches_bound_if_complete),
    inst_matches_initial_mt(yes(Type), InstA, InstB, Info0, _).

%-----------------------------------------------------------------------------%

inst_matches_binding(ModuleInfo, Type, InstA, InstB) :-
    Info0 = init_inst_match_info(ModuleInfo, no_inst_var_sub, cs_none,
        uc_match, any_does_not_match_any, ground_matches_bound_if_complete),
    inst_matches_binding_mt(yes(Type), InstA, InstB, Info0, _).

inst_matches_binding_allow_any_any(ModuleInfo, Type, InstA, InstB) :-
    Info0 = init_inst_match_info(ModuleInfo, no_inst_var_sub, cs_none,
        uc_match, any_does_match_any, ground_matches_bound_if_complete),
    inst_matches_binding_mt(yes(Type), InstA, InstB, Info0, _).

:- pred inst_matches_binding_mt(maybe(mer_type)::in,
    mer_inst::in, mer_inst::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_binding_mt(MaybeType, InstA, InstB, !Info) :-
    ThisExpansion = inst_match_inputs(InstA, InstB, MaybeType),
    Expansions0 = !.Info ^ imi_expansions,
    ( if expansion_insert_new(ThisExpansion, Expansions0, Expansions) then
        !Info ^ imi_expansions := Expansions,
        inst_expand_and_remove_constrained_inst_vars(!.Info ^ imi_module_info,
            InstA, ExpandedInstA),
        inst_expand_and_remove_constrained_inst_vars(!.Info ^ imi_module_info,
            InstB, ExpandedInstB),
        inst_matches_binding_3(MaybeType, ExpandedInstA, ExpandedInstB, !Info)
    else
        true
    ).

:- pred inst_matches_binding_3(maybe(mer_type)::in, mer_inst::in, mer_inst::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_binding_3(MaybeType, InstA, InstB, !Info) :-
    (
        InstA = free,
        InstB = free
    ;
        InstA = any(UniqA, HOInstInfoA),
        (
            InstB = any(UniqB, HOInstInfoB),
            % Note that `any' is *not* considered to match `any' unless
            % Info ^ any_matches_any = yes or the type is not a solver type
            % (and does not contain any solver types).
            AnyMatchesAny = !.Info ^ imi_any_matches_any,
            (
                AnyMatchesAny = any_does_match_any,
                ho_inst_info_matches_final(MaybeType, HOInstInfoA, HOInstInfoB,
                    !Info)
            ;
                AnyMatchesAny = any_does_not_match_any,
                maybe_any_to_bound(!.Info ^ imi_module_info, MaybeType, UniqA,
                    HOInstInfoA, NextInstA),
                maybe_any_to_bound(!.Info ^ imi_module_info, MaybeType, UniqB,
                    HOInstInfoB, NextInstB),
                inst_matches_binding_mt(MaybeType, NextInstA, NextInstB, !Info)
            )
        ;
            InstB = ground(_, _),
            maybe_any_to_bound(!.Info ^ imi_module_info, MaybeType, UniqA,
                HOInstInfoA, NextInstA),
            inst_matches_binding_mt(MaybeType, NextInstA, InstB, !Info)
        ;
            InstB = bound(_, _, _),
            maybe_any_to_bound(!.Info ^ imi_module_info, MaybeType, UniqA,
                HOInstInfoA, NextInstA),
            inst_matches_binding_mt(MaybeType, NextInstA, InstB, !Info)
        )
    ;
        InstA = ground(_, _),
        InstB = any(UniqB, HOInstInfoB),
        maybe_any_to_bound(!.Info ^ imi_module_info, MaybeType, UniqB,
            HOInstInfoB, NextInstB),
        inst_matches_binding_mt(MaybeType, InstA, NextInstB, !Info)
    ;
        InstA = bound(_UniqA, InstResultsA, BoundInstsA),
        (
            InstB = any(UniqB, HOInstInfoB),
            maybe_any_to_bound(!.Info ^ imi_module_info, MaybeType, UniqB,
                HOInstInfoB, NextInstB),
            inst_matches_binding_mt(MaybeType, InstA, NextInstB, !Info)
        ;
            InstB = bound(_UniqB, _InstResultB, BoundInstsB),
            bound_inst_list_matches_binding(MaybeType,
                BoundInstsA, BoundInstsB, !Info)
        ;
            InstB = ground(_UniqB, none_or_default_func),
            inst_results_bound_inst_list_is_ground_mt(!.Info ^ imi_module_info,
                MaybeType, InstResultsA, BoundInstsA),
            inst_contains_nondefault_func_mode_1(InstA, no, !Info)
        )
    ;
        InstA = ground(_UniqA, HOInstInfoA),
        (
            InstB = bound(_UniqB, InstResultsB, BoundInstsB),
            inst_results_bound_inst_list_is_ground_mt(!.Info ^ imi_module_info,
                MaybeType, InstResultsB, BoundInstsB),
            inst_contains_nondefault_func_mode_1(InstB, no, !Info),
            (
                MaybeType = yes(Type),
                % We can only do this check if the type is known.
                bound_inst_list_is_complete_for_type(!.Info ^ imi_module_info,
                    set.init, Type, BoundInstsB)
            ;
                MaybeType = no,
                fail
            )
        ;
            InstB = ground(_UniqB, HOInstInfoB),
            ho_inst_info_matches_binding(!.Info ^ imi_module_info,
                MaybeType, HOInstInfoA, HOInstInfoB)
        )
    ;
        InstA = abstract_inst(Name, ArgsA),
        InstB = abstract_inst(Name, ArgsB),
        list.duplicate(length(ArgsA), no, MaybeTypes),
        % XXX how do we get the argument types for an abstract inst?
        inst_list_matches_binding(MaybeTypes, ArgsA, ArgsB, !Info)
    ;
        InstA = not_reached
    ).

:- pred ho_inst_info_matches_binding(module_info::in, maybe(mer_type)::in,
    ho_inst_info::in, ho_inst_info::in) is semidet.

ho_inst_info_matches_binding(ModuleInfo, MaybeType,
        HOInstInfoA, HOInstInfoB) :-
    (
        HOInstInfoB = none_or_default_func,
        ho_inst_info_matches_ground_mt(ModuleInfo, HOInstInfoA, MaybeType)
    ;
        HOInstInfoB = higher_order(PredInstB),
        (
            HOInstInfoA = none_or_default_func,
            PredInstB = pred_inst_info(pf_function, ArgModes, _, _Det),
            Arity = list.length(ArgModes),
            PredInstA = pred_inst_info_default_func_mode(Arity),
            pred_inst_matches_mt(ModuleInfo, MaybeType, PredInstA, PredInstB)
        ;
            HOInstInfoA = higher_order(PredInstA),
            pred_inst_matches_mt(ModuleInfo, MaybeType, PredInstA, PredInstB)
        )
    ).

:- pred inst_list_matches_binding(list(maybe(mer_type))::in,
    list(mer_inst)::in, list(mer_inst)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_list_matches_binding([], [], [], !Info).
inst_list_matches_binding([MaybeType | MaybeTypes],
        [ArgA | ArgsA], [ArgB | ArgsB], !Info) :-
    inst_matches_binding_mt(MaybeType, ArgA, ArgB, !Info),
    inst_list_matches_binding(MaybeTypes, ArgsA, ArgsB, !Info).

    % Here we check that the functors in the first list are a subset of the
    % functors in the second list. (If a bound(...) inst only specifies
    % the insts for some of the constructors of its type, then it implicitly
    % means that all other constructors must have all their arguments
    % `not_reached'.) The code here makes use of the fact that the bound_inst
    % lists are sorted.
    %
:- pred bound_inst_list_matches_binding(maybe(mer_type)::in,
    list(bound_inst)::in, list(bound_inst)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

bound_inst_list_matches_binding(_, [], _, !Info).
bound_inst_list_matches_binding(MaybeType, [X | Xs], [Y | Ys], !Info) :-
    X = bound_functor(ConsIdX, ArgsX),
    Y = bound_functor(ConsIdY, ArgsY),
    ( if equivalent_cons_ids(ConsIdX, ConsIdY) then
        maybe_get_cons_id_arg_types(!.Info ^ imi_module_info, MaybeType,
            ConsIdX, list.length(ArgsX), MaybeTypes),
        inst_list_matches_binding(MaybeTypes, ArgsX, ArgsY, !Info),
        bound_inst_list_matches_binding(MaybeType, Xs, Ys, !Info)
    else
        first_unqual_cons_id_is_greater(ConsIdX, ConsIdY),
        % ConsIdX does not occur in [X | Xs].
        % Hence [X | Xs] implicitly specifies `not_reached' for the args
        % of ConsIdY, and hence automatically matches_binding Y. We just
        % need to check that [X | Xs] matches_binding Ys.
        bound_inst_list_matches_binding(MaybeType, [X | Xs], Ys, !Info)
    ).

%-----------------------------------------------------------------------------%

    % Determine what kind of uniqueness comparison we are doing and then do it.
    % If we are doing a "match" then call unique_matches_initial to do the
    % comparison. If we are comparing "instantiatedness" then the uniqueness
    % comparison is the reverse of when we are doing a match so call
    % unique_matches_initial with the arguments reversed.
    %
:- pred compare_uniqueness(uniqueness_comparison::in,
    uniqueness::in, uniqueness::in) is semidet.

compare_uniqueness(uc_match, InstA, InstB) :-
    unique_matches_initial(InstA, InstB).
compare_uniqueness(uc_instantiated, InstA, InstB) :-
    unique_matches_initial(InstB, InstA).

unique_matches_initial(unique, _).
unique_matches_initial(mostly_unique, mostly_unique).
unique_matches_initial(mostly_unique, shared).
unique_matches_initial(mostly_unique, mostly_clobbered).
unique_matches_initial(mostly_unique, clobbered).
unique_matches_initial(shared, shared).
unique_matches_initial(shared, mostly_clobbered).
unique_matches_initial(shared, clobbered).
unique_matches_initial(mostly_clobbered, mostly_clobbered).
unique_matches_initial(mostly_clobbered, clobbered).
unique_matches_initial(clobbered, clobbered).

unique_matches_final(A, B) :-
    unique_matches_initial(A, B).

%-----------------------------------------------------------------------------%

inst_contains_nondefault_func_mode(ModuleInfo, Inst) :-
    Info = init_inst_match_info(ModuleInfo, no_inst_var_sub, cs_none, uc_match,
        any_does_match_any, ground_matches_bound_if_complete),
    inst_contains_nondefault_func_mode_1(Inst, yes, Info, _).

:- pred inst_contains_nondefault_func_mode_1(mer_inst::in, bool::out,
    inst_match_info::in, inst_match_info::out) is det.

inst_contains_nondefault_func_mode_1(Inst, ContainsNonstd, !Info) :-
    inst_contains_nondefault_func_mode_2(Inst, set.init, ContainsNonstd,
        !Info).

:- pred inst_contains_nondefault_func_mode_2(mer_inst::in, set(inst_name)::in,
    bool::out, inst_match_info::in, inst_match_info::out) is det.

inst_contains_nondefault_func_mode_2(Inst, !.Expansions, ContainsNonstd,
        !Info) :-
    (
        Inst = ground(_, HOInstInfo),
        ( if ho_inst_info_matches_ground_2(no, HOInstInfo, !Info) then
            ContainsNonstd = no
        else
            ContainsNonstd = yes
        )
    ;
        Inst = bound(_, InstResults, BoundInsts),
        (
            InstResults = inst_test_results_fgtc,
            ContainsNonstd = no
        ;
            ( InstResults = inst_test_results(_, _, _, _, _, _)
            ; InstResults = inst_test_no_results
            ),
            bound_inst_list_contains_nondefault_func_mode(BoundInsts,
                !.Expansions, ContainsNonstd, !Info)
        )
    ;
        Inst = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        Inst = defined_inst(InstName),
        ( if set.member(InstName, !.Expansions) then
            ContainsNonstd = no
        else
            set.insert(InstName, !Expansions),
            inst_lookup(!.Info ^ imi_module_info, InstName, SubInst),
            inst_contains_nondefault_func_mode_2(SubInst, !.Expansions,
                ContainsNonstd, !Info)
        )
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_contains_nondefault_func_mode_2(SubInst, !.Expansions,
            ContainsNonstd, !Info)
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

:- pred inst_list_contains_nondefault_func_mode(list(mer_inst)::in,
    set(inst_name)::in, bool::out, inst_match_info::in, inst_match_info::out)
    is det.

inst_list_contains_nondefault_func_mode([], _Expansions, no, !Info).
inst_list_contains_nondefault_func_mode([Inst | Insts], Expansions,
        ContainsNonstd, !Info) :-
    inst_contains_nondefault_func_mode_2(Inst, Expansions, HeadContainsNonstd,
        !Info),
    (
        HeadContainsNonstd = yes,
        ContainsNonstd = yes
    ;
        HeadContainsNonstd = no,
        inst_list_contains_nondefault_func_mode(Insts, Expansions,
            ContainsNonstd, !Info)
    ).

:- pred bound_inst_list_contains_nondefault_func_mode(list(bound_inst)::in,
    set(inst_name)::in, bool::out, inst_match_info::in, inst_match_info::out)
    is det.

bound_inst_list_contains_nondefault_func_mode([], _Expansions, no, !Info).
bound_inst_list_contains_nondefault_func_mode([BoundInst | BoundInsts],
        Expansions, ContainsNonstd, !Info) :-
    BoundInst = bound_functor(_ConsId, ArgInsts),
    inst_list_contains_nondefault_func_mode(ArgInsts, Expansions,
        HeadContainsNonstd, !Info),
    (
        HeadContainsNonstd = yes,
        ContainsNonstd = yes
    ;
        HeadContainsNonstd = no,
        bound_inst_list_contains_nondefault_func_mode(BoundInsts, Expansions,
            ContainsNonstd, !Info)
    ).

%---------------------------------------------------------------------------%

ho_inst_info_matches_ground(ModuleInfo, HOInstInfo) :-
    ho_inst_info_matches_ground_mt(ModuleInfo, HOInstInfo, no).

:- pred ho_inst_info_matches_ground_mt(module_info::in, ho_inst_info::in,
    maybe(mer_type)::in) is semidet.

ho_inst_info_matches_ground_mt(ModuleInfo, HOInstInfo, MaybeType) :-
    Info = init_inst_match_info(ModuleInfo, no_inst_var_sub, cs_none, uc_match,
        any_does_match_any, ground_matches_bound_if_complete),
    ho_inst_info_matches_ground_2(MaybeType, HOInstInfo, Info, _).

:- pred ho_inst_info_matches_ground_2(maybe(mer_type)::in, ho_inst_info::in,
    inst_match_info::in, inst_match_info::out) is semidet.

ho_inst_info_matches_ground_2(MaybeType, HOInstInfo, !Info) :-
    (
        HOInstInfo = higher_order(PredInst),
        pred_inst_matches_ground_2(MaybeType, PredInst, !Info)
    ;
        HOInstInfo = none_or_default_func
    ).

pred_inst_matches_ground(ModuleInfo, PredInst) :-
    pred_inst_matches_ground_mt(ModuleInfo, no, PredInst).

:- pred pred_inst_matches_ground_mt(module_info::in, maybe(mer_type)::in,
    pred_inst_info::in) is semidet.

pred_inst_matches_ground_mt(ModuleInfo, MaybeType, PredInst) :-
    Info = init_inst_match_info(ModuleInfo, no_inst_var_sub, cs_none, uc_match,
        any_does_match_any, ground_matches_bound_if_complete),
    pred_inst_matches_ground_2(MaybeType, PredInst, Info, _).

:- pred pred_inst_matches_ground_2(maybe(mer_type)::in, pred_inst_info::in,
    inst_match_info::in, inst_match_info::out) is semidet.

pred_inst_matches_ground_2(MaybeType, PredInst, !Info) :-
    PredInst = pred_inst_info(PredOrFunc, ArgModes, _, _),
    (
        PredOrFunc = pf_predicate
    ;
        PredOrFunc = pf_function,
        Arity = list.length(ArgModes),
        DefaultFunc = pred_inst_info_default_func_mode(Arity),
        pred_inst_matches_2(MaybeType, PredInst, DefaultFunc, !Info)
    ).

%-----------------------------------------------------------------------------%

pred_inst_matches(ModuleInfo, PredInstA, PredInstB) :-
    pred_inst_matches_mt(ModuleInfo, no, PredInstA, PredInstB).

:- pred pred_inst_matches_mt(module_info::in, maybe(mer_type)::in,
    pred_inst_info::in, pred_inst_info::in) is semidet.

pred_inst_matches_mt(ModuleInfo, MaybeType, PredInstA, PredInstB) :-
    Info0 = init_inst_match_info(ModuleInfo, no_inst_var_sub, cs_none,
        uc_match, any_does_match_any, ground_matches_bound_if_complete),
    pred_inst_matches_2(MaybeType, PredInstA, PredInstB, Info0, _).

    % pred_inst_matches_2(MaybeType, PredInstA, PredInstB, !Info)
    %
    % Same as pred_inst_matches/3, except that it updates the inst_var_sub
    % in the inst_match_info, and that any inst pairs in !.Info ^ expansions
    % are assumed to match_final each other. (This avoids infinite loops
    % when calling inst_matches_final on higher-order recursive insts.)
    %
:- pred pred_inst_matches_2(maybe(mer_type)::in,
    pred_inst_info::in, pred_inst_info::in,
    inst_match_info::in, inst_match_info::out) is semidet.

pred_inst_matches_2(MaybeType, PredInstA, PredInstB, !Info) :-
    % In the float_regs.m pass a variable may take on pred insts which differ
    % only in the arg reg lists in different branches. They should be allowed
    % to match here.
    PredInstA = pred_inst_info(PredOrFunc, ModesA, _MaybeArgRegsA, Det),
    PredInstB = pred_inst_info(PredOrFunc, ModesB, _MaybeArgRegsB, Det),
    maybe_get_higher_order_arg_types(MaybeType, list.length(ModesA),
        MaybeTypes),
    pred_inst_argmodes_matches(MaybeTypes, ModesA, ModesB, !Info).

    % pred_inst_argmodes_matches(MaybeTypes, ModesA, ModesB, !Info):
    %
    % Succeeds if the initial insts of ModesB specify at least as much
    % information as, and the same binding as, the initial insts of ModesA;
    % and the final insts of ModesA specify at least as much information as,
    % and the same binding as, the final insts of ModesB. Any inst pairs
    % in Inst0 ^ expansions are assumed to match_final each other.
    %
    % (In other words, as far as subtyping goes it is contravariant in
    % the initial insts, and covariant in the final insts;
    % as far as binding goes, it is invariant for both.)
    %
:- pred pred_inst_argmodes_matches(list(maybe(mer_type))::in,
    list(mer_mode)::in, list(mer_mode)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

pred_inst_argmodes_matches([], [], [], !Info).
pred_inst_argmodes_matches([MaybeType | MaybeTypes],
        [ModeA | ModeAs], [ModeB | ModeBs], !Info) :-
    ModuleInfo = !.Info ^ imi_module_info,
    mode_get_insts(ModuleInfo, ModeA, InitialA, FinalA0),
    mode_get_insts(ModuleInfo, ModeB, InitialB, FinalB),
    % inst_matches_final_mt should probably just accept cs_reverse directly.
    swap_sub(inst_matches_final_mt(MaybeType, InitialB, InitialA), !Info),
    % Apply the substitution computed so far (it may be necessary for InitialA
    % as well).
    maybe_apply_substitution(!.Info, FinalA0, FinalA),
    inst_matches_final_mt(MaybeType, FinalA, FinalB, !Info),
    pred_inst_argmodes_matches(MaybeTypes, ModeAs, ModeBs, !Info).

:- pred maybe_apply_substitution(inst_match_info::in,
    mer_inst::in, mer_inst::out) is det.

maybe_apply_substitution(Info, Inst0, Inst) :-
    (
        Info ^ imi_maybe_sub = inst_var_sub(Subst),
        inst_apply_substitution(Subst, Inst0, Inst)
    ;
        Info ^ imi_maybe_sub = no_inst_var_sub,
        Inst = Inst0
    ).

%-----------------------------------------------------------------------------%

:- pred inst_name_contains_inst_var(inst_name::in, inst_var::out) is nondet.

inst_name_contains_inst_var(InstName, InstVar) :-
    require_complete_switch [InstName]
    (
        InstName = user_inst(_Name, ArgInsts),
        inst_list_contains_inst_var(ArgInsts, InstVar)
    ;
        InstName = merge_inst(InstA, InstB),
        ( inst_contains_inst_var(InstA, InstVar)
        ; inst_contains_inst_var(InstB, InstVar)
        )
    ;
        InstName = unify_inst(_Live, _Real, InstA, InstB),
        ( inst_contains_inst_var(InstA, InstVar)
        ; inst_contains_inst_var(InstB, InstVar)
        )
    ;
        InstName = ground_inst(SubInstName, _Live, _Uniq, _Real),
        inst_name_contains_inst_var(SubInstName, InstVar)
    ;
        InstName = any_inst(SubInstName, _Live, _Uniq, _Real),
        inst_name_contains_inst_var(SubInstName, InstVar)
    ;
        InstName = shared_inst(SubInstName),
        inst_name_contains_inst_var(SubInstName, InstVar)
    ;
        InstName = mostly_uniq_inst(SubInstName),
        inst_name_contains_inst_var(SubInstName, InstVar)
    ;
        InstName = typed_ground(_Uniq, _Type),
        fail
    ;
        InstName = typed_inst(_Type, SubInstName),
        inst_name_contains_inst_var(SubInstName, InstVar)
    ).

:- pred inst_contains_inst_var(mer_inst::in, inst_var::out) is nondet.

inst_contains_inst_var(Inst, InstVar) :-
    (
        Inst = inst_var(InstVar)
    ;
        Inst = defined_inst(InstName),
        inst_name_contains_inst_var(InstName, InstVar)
    ;
        Inst = bound(_Uniq, InstResults, BoundInsts),
        (
            InstResults = inst_test_results_fgtc,
            fail
        ;
            InstResults = inst_test_results(_, _, _, InstVarResult, _, _),
            (
                InstVarResult = inst_result_contains_inst_vars_known(InstVars),
                set.member(InstVar, InstVars),
                % Membership in InstVars means that BoundInsts *may* contain
                % InstVar, not that it *does*, so we have to check whether
                % it actually does.
                bound_inst_list_contains_inst_var(BoundInsts, InstVar)
            ;
                InstVarResult = inst_result_contains_inst_vars_unknown,
                bound_inst_list_contains_inst_var(BoundInsts, InstVar)
            )
        ;
            InstResults = inst_test_no_results,
            bound_inst_list_contains_inst_var(BoundInsts, InstVar)
        )
    ;
        Inst = ground(_Uniq, HOInstInfo),
        HOInstInfo = higher_order(pred_inst_info(_PredOrFunc, Modes, _, _Det)),
        mode_list_contains_inst_var(Modes, InstVar)
    ;
        Inst = abstract_inst(_Name, ArgInsts),
        inst_list_contains_inst_var(ArgInsts, InstVar)
    ).

:- pred bound_inst_list_contains_inst_var(list(bound_inst)::in, inst_var::out)
    is nondet.

bound_inst_list_contains_inst_var([BoundInst | BoundInsts], InstVar) :-
    BoundInst = bound_functor(_Functor, ArgInsts),
    (
        inst_list_contains_inst_var(ArgInsts, InstVar)
    ;
        bound_inst_list_contains_inst_var(BoundInsts, InstVar)
    ).

:- pred inst_list_contains_inst_var(list(mer_inst)::in, inst_var::out)
    is nondet.

inst_list_contains_inst_var([Inst | Insts], InstVar) :-
    (
        inst_contains_inst_var(Inst, InstVar)
    ;
        inst_list_contains_inst_var(Insts, InstVar)
    ).

mode_list_contains_inst_var([Mode | Modes], InstVar) :-
    (
        mode_contains_inst_var(Mode, InstVar)
    ;
        mode_list_contains_inst_var(Modes, InstVar)
    ).

:- pred mode_contains_inst_var(mer_mode::in, inst_var::out) is nondet.

mode_contains_inst_var(Mode, InstVar) :-
    (
        Mode = from_to_mode(Initial, Final),
        ( Inst = Initial ; Inst = Final )
    ;
        Mode = user_defined_mode(_Name, Insts),
        list.member(Inst, Insts)
    ),
    inst_contains_inst_var(Inst, InstVar).

%-----------------------------------------------------------------------------%

:- pred same_addr_insts(mer_inst::in, mer_inst::in) is semidet.

:- pragma foreign_proc("C",
    same_addr_insts(InstA::in, InstB::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((void *) InstA == (void *) InstB);
").

same_addr_insts(_, _) :-
    semidet_fail.

%-----------------------------------------------------------------------------%
:- end_module check_hlds.inst_match.
%-----------------------------------------------------------------------------%
