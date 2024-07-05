%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2000-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: inst_match.m.
% Author: fjh.
%
% This module defines some utility routines for comparing insts that are used
% by the mode analysis pass, and by the rest of the compiler.
%
% We do allow `bound' and `ground' to match `any', based on the assumption
% that `bound' and `ground' are represented in the same way as `any', i.e.
% that we use the type system rather than the mode system to distinguish
% between different representations.
%
%---------------------------------------------------------------------------%

:- module check_hlds.inst_match.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

%---------------------------------------------------------------------------%

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
    mer_inst::in, mer_inst::in, module_info::in, module_info::out,
    inst_var_sub::in, inst_var_sub::out) is semidet.

    % inst_is_at_least_as_instantiated(InstA, InstB, Type, ModuleInfo)
    % succeeds iff InstA is at least as instantiated as InstB. This defines
    % a partial order which is the same as inst_matches_initial except that
    % uniqueness comparisons are reversed and we don't allow
    % inst_is_at_least_as_instantiated(any, any).
    %
    % XXX It is not actually clear what "this predicate" refers to
    % in the next two paragraphs :-(
    %
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
:- pred inst_is_at_least_as_instantiated(module_info::in, mer_type::in,
    mer_inst::in, mer_inst::in) is semidet.

%---------------------------------------------------------------------------%

    % inst_matches_final(ModuleInfo, Type, InstA, InstB):
    %
    % Succeed iff InstA is compatible with InstB, i.e. iff InstA will satisfy
    % the final inst requirement InstB. This is true if the information
    % specified by InstA is at least as great as that specified by InstB,
    % and where the information is the same and both insts specify a binding,
    % the binding must be identical.
    %
:- pred inst_matches_final(module_info::in, mer_type::in,
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

%---------------------------------------------------------------------------%

    % inst_matches_binding(ModuleInfo, Type, InstA, InstB):
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

%---------------------------------------------------------------------------%

    % inst_contains_nondefault_func_mode(ModuleInfo, Type, Inst) succeeds
    % iff the inst contains a higher-order function inst that does not match
    % the default function mode `(in, ..., in) = out is det'.
    % E.g. this predicate fails for "func(in) = uo" because that matches the
    % default func mode "func(in) = out", even though it isn't the same as
    % the default func mode.
    %
:- pred inst_contains_nondefault_func_mode(module_info::in, mer_type::in,
    mer_inst::in) is semidet.

%---------------------------------------------------------------------------%

    % Succeed iff the second argument is not a function ho_inst_info
    % whose mode does not match the default func mode.
    %
:- pred ho_inst_info_matches_ground(module_info::in, mer_type::in,
    ho_inst_info::in) is semidet.

    % Succeed iff the second argument is not a function pred_inst_info
    % whose mode does not match the default func mode.
    %
:- pred pred_inst_matches_ground(module_info::in, mer_type::in,
    pred_inst_info::in) is semidet.

    % pred_inst_matches(ModuleInfo, Type, PredInstA, PredInstB)
    %
    % Succeeds if PredInstA specifies a pred that can be used wherever and
    % whenever PredInstB could be used. This is true if they both have the
    % same PredOrFunc indicator and the same determinism, and if the arguments
    % match using pred_inst_argmodes_match.
    %
:- pred pred_inst_matches(module_info::in, mer_type::in,
    pred_inst_info::in, pred_inst_info::in) is semidet.

%---------------------------------------------------------------------------%

    % unique_matches_initial(A, B) succeeds if A >= B in the ordering
    % clobbered < mostly_clobbered < shared < mostly_unique < unique.
    %
:- pred unique_matches_initial(uniqueness::in, uniqueness::in) is semidet.

    % unique_matches_final(A, B) succeeds if A >= B in the ordering
    % clobbered < mostly_clobbered < shared < mostly_unique < unique.
    %
:- pred unique_matches_final(uniqueness::in, uniqueness::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module term.

%---------------------------------------------------------------------------%

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
    Info0 = init_inst_match_info(ModuleInfo, no_inst_var_sub, uc_match,
        any_does_match_any, ground_matches_bound_if_complete),
    inst_matches_final_mt(cs_forward, Type, InstA, InstB, Info0, _).

inst_matches_initial_no_implied_modes_sub(Type, InstA, InstB,
        !ModuleInfo, !Sub) :-
    Info0 = init_inst_match_info(!.ModuleInfo, inst_var_sub(!.Sub), uc_match,
        any_does_match_any, ground_matches_bound_if_complete),
    inst_matches_final_mt(cs_forward, Type, InstA, InstB, Info0, Info),
    !:ModuleInfo = Info ^ imi_module_info,
    inst_var_sub(!:Sub) = Info ^ imi_maybe_sub.

inst_is_at_least_as_instantiated(ModuleInfo, Type, InstA, InstB) :-
    Info0 = init_inst_match_info(ModuleInfo, no_inst_var_sub, uc_instantiated,
        any_does_not_match_any, ground_matches_bound_if_complete),
    inst_matches_initial_mt(cs_none, Type, InstA, InstB, Info0, _).

%---------------------%

:- pred inst_matches_initial_1(mer_type::in, mer_inst::in, mer_inst::in,
    module_info::in, module_info::out,
    maybe_inst_var_sub::in, maybe_inst_var_sub::out) is semidet.

inst_matches_initial_1(Type, InstA, InstB, !ModuleInfo, !MaybeSub) :-
    Info0 = init_inst_match_info(!.ModuleInfo, !.MaybeSub, uc_match,
        any_does_match_any, ground_matches_bound_if_complete),
    inst_matches_initial_mt(cs_forward, Type, InstA, InstB, Info0, Info),
    !:ModuleInfo = Info ^ imi_module_info,
    !:MaybeSub = Info ^ imi_maybe_sub.

:- pred inst_matches_initial_mt(calculate_sub::in, mer_type::in,
    mer_inst::in, mer_inst::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_initial_mt(CalcSub, Type, InstA, InstB, !Info) :-
    ThisExpansion = inst_match_inputs(InstA, InstB, Type),
    Expansions0 = !.Info ^ imi_expansions,
    ( if expansion_insert_new(ThisExpansion, Expansions0, Expansions) then
        !Info ^ imi_expansions := Expansions,
        inst_expand(!.Info ^ imi_module_info, InstA, ExpandedInstA),
        inst_expand(!.Info ^ imi_module_info, InstB, ExpandedInstB),
        maybe_handle_inst_var_subs(CalcSub, Type, ExpandedInstA, ExpandedInstB,
            InstVarSubResult, !Info),
        require_complete_switch [InstVarSubResult]
        (
            InstVarSubResult = ivsr_recurse(RecurseInstA, RecurseInstB),
            inst_matches_initial_mt(cs_forward, Type,
                RecurseInstA, RecurseInstB, !Info)
        ;
            InstVarSubResult = ivsr_continue(ContCalcSub,
                ContinueInstA, ContinueInstB),
            inst_matches_initial_mt_2(ContCalcSub, Type,
                ContinueInstA, ContinueInstB, !Info)
        )
    else
        true
    ).

:- pred inst_matches_initial_mt_2(calculate_sub::in, mer_type::in,
    mer_inst::in(mer_inst_expanded), mer_inst::in(mer_inst_expanded),
    inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_initial_mt_2(CalcSub, Type, InstA, InstB, !Info) :-
    % To avoid infinite regress, we assume that inst_matches_initial is true
    % for any pairs of insts which occur in `Expansions'.
    %
    % XXX Maybe we could use the inst result field of bound/3 insts
    % in some places.
    require_complete_switch [InstA]
    (
        InstA = free,
        InstB = free
    ;
        InstA = any(UniqA, HOInstInfoA),
        require_complete_switch [InstB]
        (
            InstB = free
        ;
            InstB = ground(_, _),
            maybe_any_to_bound(!.Info ^ imi_module_info, Type, UniqA,
                HOInstInfoA, NextInstA),
            inst_matches_initial_mt(CalcSub, Type, NextInstA, InstB, !Info)
        ;
            InstB = bound(_, _, _),
            maybe_any_to_bound(!.Info ^ imi_module_info, Type, UniqA,
                HOInstInfoA, NextInstA),
            inst_matches_initial_mt(CalcSub, Type, NextInstA, InstB, !Info)
        ;
            InstB = any(UniqB, HOInstInfoB),
            !.Info ^ imi_any_matches_any = any_does_match_any,
            compare_uniqueness(!.Info ^ imi_uniqueness_comparison,
                UniqA, UniqB),
            ho_inst_info_matches_initial(CalcSub, Type,
                HOInstInfoA, HOInstInfoB, !Info)
        ;
            ( InstB = constrained_inst_vars(_, _)
            ; InstB = inst_var(_)
            ; InstB = not_reached
            ),
            % XXX The failures in these cases used to be implicit.
            % For some of these cases, failure may be the wrong thing to do.
            fail
        )
    ;
        InstA = bound(UniqA, InstResultsA, BoundInstsA),
        require_complete_switch [InstB]
        (
            InstB = free
        ;
            InstB = ground(UniqB, none_or_default_func),
            compare_uniqueness(!.Info ^ imi_uniqueness_comparison,
                UniqA, UniqB),
            inst_results_bound_inst_list_is_ground_mt(!.Info ^ imi_module_info,
                Type, InstResultsA, BoundInstsA),
            compare_bound_inst_list_uniq(!.Info ^ imi_module_info,
                !.Info ^ imi_uniqueness_comparison, BoundInstsA, UniqB),
            inst_contains_nondefault_func_mode_1(CalcSub, Type, InstA, no,
                !Info)
        ;
            InstB = bound(UniqB, _InstResultsB, BoundInstsB),
            ( if
                same_addr_insts(InstA, InstB),
                InstResultsA = inst_test_results_fgtc
            then
                true
            else
                compare_uniqueness(!.Info ^ imi_uniqueness_comparison,
                    UniqA, UniqB),
                bound_inst_list_matches_initial_mt(CalcSub, Type,
                    BoundInstsA, BoundInstsB, !Info)
            )
        ;
            InstB = any(UniqB, none_or_default_func),
            compare_uniqueness(!.Info ^ imi_uniqueness_comparison,
                UniqA, UniqB),
            compare_bound_inst_list_uniq(!.Info ^ imi_module_info,
                !.Info ^ imi_uniqueness_comparison, BoundInstsA, UniqB),
            inst_contains_nondefault_func_mode_1(CalcSub, Type, InstA, no,
                !Info)
        ;
            ( InstB = constrained_inst_vars(_, _)
            ; InstB = inst_var(_)
            ; InstB = not_reached
            ),
            % XXX The failures in these cases used to be implicit.
            % For some of these cases, failure may be the wrong thing to do.
            fail
        )
    ;
        InstA = ground(UniqA, HOInstInfoA),
        require_complete_switch [InstB]
        (
            InstB = free
        ;
            InstB = ground(UniqB, HOInstInfoB),
            compare_uniqueness(!.Info ^ imi_uniqueness_comparison,
                UniqA, UniqB),
            ho_inst_info_matches_initial(CalcSub, Type,
                HOInstInfoA, HOInstInfoB, !Info)
        ;
            InstB = bound(UniqB, _InstResultsB, BoundInstsB),
            ModuleInfo = !.Info ^ imi_module_info,
            % We can check this case properly only if the type is a du type.
            type_is_du_type(ModuleInfo, Type),
            compare_uniqueness(!.Info ^ imi_uniqueness_comparison,
                UniqA, UniqB),
            bound_inst_list_is_complete_for_type(ModuleInfo, set.init, Type,
                BoundInstsB),
            ground_matches_initial_bound_inst_list(CalcSub, UniqA, Type,
                BoundInstsB, !Info)
        ;
            InstB = any(UniqB, HOInstInfoB),
            compare_uniqueness(!.Info ^ imi_uniqueness_comparison,
                UniqA, UniqB),
            ho_inst_info_matches_initial(CalcSub, Type,
                HOInstInfoA, HOInstInfoB, !Info)
        ;
            ( InstB = constrained_inst_vars(_, _)
            ; InstB = inst_var(_)
            ; InstB = not_reached
            ),
            % XXX The failures in these cases used to be implicit.
            % For some of these cases, failure may be the wrong thing to do.
            fail
        )
    ;
        InstA = not_reached
    ;
        ( InstA = constrained_inst_vars(_, _)
        ; InstA = inst_var(_)
        ),
        % Our callers should have expanded out these insts, but some of them
        % do not do that. In particular, inst_is_at_least_as_instantiated
        % calls inst_matches_initial_mt with cs_none.
        % XXX If and when this is fixed, and all callers expand out these
        % insts, update the expected insts of InstA/InstB to reflect this.
        unexpected($pred, "unexpected InstA")
    ).

%---------------------%

    % This predicate assumes that the check of
    % `bound_inst_list_is_complete_for_type' is done by the caller.
    %
:- pred ground_matches_initial_bound_inst_list(calculate_sub::in,
    uniqueness::in, mer_type::in, list(bound_inst)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

ground_matches_initial_bound_inst_list(_, _, _, [], !Info).
ground_matches_initial_bound_inst_list(CalcSub, Uniq, Type,
        [BoundInst | BoundInsts], !Info) :-
    get_cons_id_arg_types_for_bound_inst(!.Info ^ imi_module_info, Type,
        BoundInst, ArgTypes),
    BoundInst = bound_functor(_ConsId, ArgInsts),
    ground_matches_initial_inst_list(CalcSub, Uniq, ArgTypes, ArgInsts, !Info),
    ground_matches_initial_bound_inst_list(CalcSub, Uniq, Type, BoundInsts,
        !Info).

:- pred ground_matches_initial_inst_list(calculate_sub::in, uniqueness::in,
    list(mer_type)::in, list(mer_inst)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

ground_matches_initial_inst_list(_, _, [], [], !Info).
ground_matches_initial_inst_list(CalcSub, Uniq,
        [Type | Types], [Inst | Insts], !Info) :-
    Ground = ground(Uniq, none_or_default_func),
    inst_matches_initial_mt(CalcSub, Type, Ground, Inst, !Info),
    ground_matches_initial_inst_list(CalcSub, Uniq, Types, Insts, !Info).

%---------------------%

    % This predicate checks if two ho_inst_infos match_initial.
    % It does not check uniqueness.
    %
:- pred ho_inst_info_matches_initial(calculate_sub::in, mer_type::in,
    ho_inst_info::in, ho_inst_info::in,
    inst_match_info::in, inst_match_info::out) is semidet.

ho_inst_info_matches_initial(CalcSub, Type, HOInstInfoA, HOInstInfoB, !Info) :-
    (
        HOInstInfoB = none_or_default_func,
        ho_inst_info_matches_ground_1(CalcSub, Type, HOInstInfoA, !Info)
    ;
        HOInstInfoB = higher_order(PredInstB),
        (
            HOInstInfoA = none_or_default_func,
            PredInstB = pred_inst_info(pf_function, ArgModes, _, _Det),
            Arity = list.length(ArgModes),
            PredInstA = pred_inst_info_default_func_mode(Arity)
        ;
            HOInstInfoA = higher_order(PredInstA)
        ),
        pred_inst_matches_1(CalcSub, Type, PredInstA, PredInstB, !Info)
    ).

%---------------------%

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

%---------------------%

    % Here we check that the functors in the first list are a subset of the
    % functors in the second list. (If a bound(...) inst only specifies the
    % insts for some of the constructors of its type, then it implicitly means
    % that all other constructors must have all their arguments `not_reached'.)
    % The code here makes use of the fact that the bound_inst lists are sorted.
    %
:- pred bound_inst_list_matches_initial_mt(calculate_sub::in, mer_type::in,
    list(bound_inst)::in, list(bound_inst)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

bound_inst_list_matches_initial_mt(_, _, [], _, !Info).
bound_inst_list_matches_initial_mt(CalcSub, Type,
        [BoundInstX | BoundInstXs], [BoundInstY | BoundInstYs], !Info) :-
    BoundInstX = bound_functor(ConsIdX, ArgInstsX),
    BoundInstY = bound_functor(ConsIdY, ArgInstsY),
    ( if equivalent_cons_ids(ConsIdX, ConsIdY) then
        get_cons_id_arg_types_for_bound_inst(!.Info ^ imi_module_info, Type,
            BoundInstX, Types),
        inst_list_matches_initial_mt(CalcSub, Types, ArgInstsX, ArgInstsY,
            !Info),
        bound_inst_list_matches_initial_mt(CalcSub, Type,
            BoundInstXs, BoundInstYs, !Info)
    else
        first_unqual_cons_id_is_greater(ConsIdX, ConsIdY),
        % ConsIdY does not occur in [BoundInstX | BoundInstXs].
        % Hence [BoundInstX | BoundInstXs] implicitly specifies `not_reached'
        % for the args of ConsIdY, and hence automatically matches_initial Y.
        % We just need to check that [BoundInstX | BoundInstXs]
        % matches_initial BoundInstsYs.
        bound_inst_list_matches_initial_mt(CalcSub, Type,
            [BoundInstX | BoundInstXs], BoundInstYs, !Info)
    ).

:- pred inst_list_matches_initial_mt(calculate_sub::in, list(mer_type)::in,
    list(mer_inst)::in, list(mer_inst)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_list_matches_initial_mt(_, [], [], [], !Info).
inst_list_matches_initial_mt(CalcSub, [Type | Types],
        [InstX | InstXs], [InstY | InstYs], !Info) :-
    inst_matches_initial_mt(CalcSub, Type, InstX, InstY, !Info),
    inst_list_matches_initial_mt(CalcSub, Types, InstXs, InstYs, !Info).

%---------------------------------------------------------------------------%

inst_matches_final(ModuleInfo, Type, InstA, InstB) :-
    Info0 = init_inst_match_info(ModuleInfo, no_inst_var_sub,
        uc_match, any_does_match_any, ground_matches_bound_if_complete),
    inst_matches_final_mt(cs_none, Type, InstA, InstB, Info0, _).

inst_matches_final_gmb(ModuleInfo, GroundMatchesBound, Type, InstA, InstB) :-
    Info0 = init_inst_match_info(ModuleInfo, no_inst_var_sub,
        uc_match, any_does_match_any, GroundMatchesBound),
    inst_matches_final_mt(cs_none, Type, InstA, InstB, Info0, _).

%---------------------%

:- pred inst_matches_final_mt(calculate_sub::in, mer_type::in,
    mer_inst::in, mer_inst::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_final_mt(CalcSub, Type, InstA, InstB, !Info) :-
    ( if InstA = InstB then
        true
    else
        ThisExpansion = inst_match_inputs(InstA, InstB, Type),
        Expansions0 = !.Info ^ imi_expansions,
        ( if expansion_insert_new(ThisExpansion, Expansions0, Expansions) then
            !Info ^ imi_expansions := Expansions,
            inst_expand(!.Info ^ imi_module_info, InstA, ExpandedInstA),
            inst_expand(!.Info ^ imi_module_info, InstB, ExpandedInstB),
            maybe_handle_inst_var_subs(CalcSub, Type,
                ExpandedInstA, ExpandedInstB, InstVarSubResult, !Info),
            require_complete_switch [InstVarSubResult]
            (
                InstVarSubResult = ivsr_recurse(RecurseInstA, RecurseInstB),
                inst_matches_final_mt(cs_forward, Type,
                    RecurseInstA, RecurseInstB, !Info)
            ;
                InstVarSubResult = ivsr_continue(ContCalcSub,
                    ContinueInstA, ContinueInstB),
                inst_matches_final_mt_2(ContCalcSub, Type,
                    ContinueInstA, ContinueInstB, !Info)
            )
        else
            true
        )
    ).

:- pred inst_matches_final_mt_2(calculate_sub::in, mer_type::in,
    mer_inst::in(mer_inst_expanded), mer_inst::in(mer_inst_expanded),
    inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_final_mt_2(CalcSub, Type, InstA, InstB, !Info) :-
    % NOTE Both InstA and InstB may be constrained_inst_vars/2.
    require_complete_switch [InstA]
    (
        InstA = free,
        require_complete_switch [InstB]
        (
            InstB = free
        ;
            InstB = any(Uniq, _),
            % We do not yet allow `free' to match `any',
            % unless the `any' is `clobbered_any' or `mostly_clobbered_any'.
            % Among other things, changing this would break compare_inst
            % in modecheck_call.m.
            ( Uniq = clobbered ; Uniq = mostly_clobbered )
        ;
            ( InstB = ground(_, _)
            ; InstB = bound(_, _, _)
            ; InstB = constrained_inst_vars(_, _)
            ; InstB = inst_var(_)
            ; InstB = not_reached
            ),
            % XXX The failures in these cases used to be implicit.
            % For some of these cases, failure may be the wrong thing to do.
            fail
        )
    ;
        InstA = ground(UniqA, HOInstInfoA),
        require_complete_switch [InstB]
        (
            InstB = ground(UniqB, HOInstInfoB),
            ho_inst_info_matches_final(CalcSub, Type,
                HOInstInfoA, HOInstInfoB, !Info),
            unique_matches_final(UniqA, UniqB)
        ;
            InstB = bound(UniqB, InstResultsB, BoundInstsB),
            ho_inst_info_matches_ground_1(CalcSub, Type, HOInstInfoA, !Info),
            unique_matches_final(UniqA, UniqB),
            ModuleInfo = !.Info ^ imi_module_info,
            inst_results_bound_inst_list_is_ground_mt(ModuleInfo, Type,
                InstResultsB, BoundInstsB),
            uniq_matches_bound_inst_list(ModuleInfo, UniqA, BoundInstsB),
            inst_contains_nondefault_func_mode_1(CalcSub, Type, InstB, no,
                !Info),
            (
                % This check can succeed only if the type is known.
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
            InstB = any(UniqB, HOInstInfoB),
            ho_inst_info_matches_final(CalcSub, Type,
                HOInstInfoA, HOInstInfoB, !Info),
            unique_matches_final(UniqA, UniqB)
        ;
            ( InstB = free
            ; InstB = constrained_inst_vars(_, _)
            ; InstB = inst_var(_)
            ; InstB = not_reached
            ),
            % XXX The failures in these cases used to be implicit.
            % For some of these cases, failure may be the wrong thing to do.
            fail
        )
    ;
        InstA = bound(UniqA, InstResultsA, BoundInstsA),
        require_complete_switch [InstB]
        (
            InstB = ground(UniqB, none_or_default_func),
            unique_matches_final(UniqA, UniqB),
            inst_results_bound_inst_list_is_ground_mt(!.Info ^ imi_module_info,
                Type, InstResultsA, BoundInstsA),
            bound_inst_list_matches_uniq(!.Info ^ imi_module_info, UniqB,
                BoundInstsA),
            inst_contains_nondefault_func_mode_1(CalcSub, Type, InstA, no,
                !Info)
        ;
            InstB = bound(UniqB, _InstResultsB, BoundInstsB),
            unique_matches_final(UniqA, UniqB),
            bound_inst_list_matches_final(CalcSub, Type,
                BoundInstsA, BoundInstsB, !Info)
        ;
            InstB = any(UniqB, none_or_default_func),
            unique_matches_final(UniqA, UniqB),
            bound_inst_list_matches_uniq(!.Info ^ imi_module_info, UniqB,
                BoundInstsA),
            % We do not yet allow `free' to match `any'.
            % Among other things, changing this would break compare_inst
            % in modecheck_call.m.
            inst_results_bound_inst_list_is_ground_or_any(
                !.Info ^ imi_module_info, InstResultsA, BoundInstsA),
            inst_contains_nondefault_func_mode_1(CalcSub, Type, InstA, no,
                !Info)
        ;
            ( InstB = free
            ; InstB = not_reached
            ; InstB = constrained_inst_vars(_, _)
            ; InstB = inst_var(_)
            ),
            % XXX The failures in these cases used to be implicit.
            % For some of these cases, failure may be the wrong thing to do.
            fail
        )
    ;
        InstA = any(UniqA, HOInstInfoA),
        require_complete_switch [InstB]
        (
            InstB = ground(_, _),
            maybe_any_to_bound(!.Info ^ imi_module_info, Type, UniqA,
                HOInstInfoA, NextInstA),
            inst_matches_final_mt(CalcSub, Type, NextInstA, InstB, !Info)
        ;
            InstB = bound(_, _, _),
            maybe_any_to_bound(!.Info ^ imi_module_info, Type, UniqA,
                HOInstInfoA, NextInstA),
            inst_matches_final_mt(CalcSub, Type, NextInstA, InstB, !Info)
        ;
            InstB = any(UniqB, HOInstInfoB),
            ho_inst_info_matches_final(CalcSub, Type,
                HOInstInfoA, HOInstInfoB, !Info),
            unique_matches_final(UniqA, UniqB)
        ;
            ( InstB = free
            ; InstB = constrained_inst_vars(_, _)
            ; InstB = inst_var(_)
            ; InstB = not_reached
            ),
            % XXX The failures in these cases used to be implicit.
            % For some of these cases, failure may be the wrong thing to do.
            fail
        )
    ;
        InstA = constrained_inst_vars(InstVarsA, SubInstA),
        ( if InstB = constrained_inst_vars(InstVarsB, SubInstB) then
            % Constrained_inst_vars match_final only if InstVarsA contains
            % all the variables in InstVarsB.
            set.subset(InstVarsB, InstVarsA),
            inst_matches_final_mt(CalcSub, Type, SubInstA, SubInstB, !Info)
        else
            inst_matches_final_mt(CalcSub, Type, SubInstA, InstB, !Info)
        )
    ;
        InstA = not_reached
    ;
        InstA = inst_var(_),
        % XXX This case used to be simply missing, so I (zs) have no idea
        % whether failing here is the right thing to do; I only know that
        % that is what this code has always done implicitly. This code here
        % only makes the failure explicit.
        fail
    ).

%---------------------%

:- pred ho_inst_info_matches_final(calculate_sub::in, mer_type::in,
    ho_inst_info::in, ho_inst_info::in,
    inst_match_info::in, inst_match_info::out) is semidet.

ho_inst_info_matches_final(CalcSub, Type, HOInstInfoA, HOInstInfoB, !Info) :-
    (
        HOInstInfoB = none_or_default_func,
        ho_inst_info_matches_ground_1(CalcSub, Type, HOInstInfoA, !Info)
    ;
        HOInstInfoB = higher_order(PredInstB),
        (
            HOInstInfoA = none_or_default_func,
            PredInstB = pred_inst_info(pf_function, ArgModes, _, _Det),
            list.length(ArgModes, Arity),
            PredInstA = pred_inst_info_default_func_mode(Arity),
            pred_inst_matches_1(CalcSub, Type, PredInstA, PredInstB, !Info)
        ;
            HOInstInfoA = higher_order(PredInstA),
            pred_inst_matches_1(CalcSub, Type, PredInstA, PredInstB, !Info)
        )
    ).

%---------------------%

    % Here we check that the functors in the first list are a subset of the
    % functors in the second list. (If a bound(...) inst only specifies
    % the insts for some of the constructors of its type, then it implicitly
    % means that all other constructors must have all their arguments
    % `not_reached'.) The code here makes use of the fact that the bound_inst
    % lists are sorted.
    %
:- pred bound_inst_list_matches_final(calculate_sub::in, mer_type::in,
    list(bound_inst)::in, list(bound_inst)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

bound_inst_list_matches_final(_, _, [], _, !Info).
bound_inst_list_matches_final(CalcSub, Type,
        [BoundInstX | BoundInstXs], [BoundInstY | BoundInstYs], !Info) :-
    BoundInstX = bound_functor(ConsIdX, ArgInstsX),
    BoundInstY = bound_functor(ConsIdY, ArgInstsY),
    ( if equivalent_cons_ids(ConsIdX, ConsIdY) then
        get_cons_id_arg_types_for_bound_inst(!.Info ^ imi_module_info, Type,
            BoundInstX, Types),
        inst_list_matches_final(CalcSub, Types, ArgInstsX, ArgInstsY, !Info),
        bound_inst_list_matches_final(CalcSub, Type,
            BoundInstXs, BoundInstYs, !Info)
    else
        first_unqual_cons_id_is_greater(ConsIdX, ConsIdY),
        % ConsIdY does not occur in [X | Xs].
        % Hence [X | Xs] implicitly specifies `not_reached' for the args
        % of ConsIdY, and hence automatically matches_final Y. We just
        % need to check that [X | Xs] matches_final Ys.
        bound_inst_list_matches_final(CalcSub, Type,
            [BoundInstX | BoundInstXs], BoundInstYs, !Info)
    ).

:- pred inst_list_matches_final(calculate_sub::in, list(mer_type)::in,
    list(mer_inst)::in, list(mer_inst)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_list_matches_final(_, [], [], [], !Info).
inst_list_matches_final(CalcSub, [Type | Types],
        [ArgInstA | ArgInstsA], [ArgInstB | ArgInstsB], !Info) :-
    inst_matches_final_mt(CalcSub, Type, ArgInstA, ArgInstB, !Info),
    inst_list_matches_final(CalcSub, Types, ArgInstsA, ArgInstsB, !Info).

%---------------------------------------------------------------------------%

inst_matches_binding(ModuleInfo, Type, InstA, InstB) :-
    Info0 = init_inst_match_info(ModuleInfo, no_inst_var_sub,
        uc_match, any_does_not_match_any, ground_matches_bound_if_complete),
    inst_matches_binding_mt(cs_none, Type, InstA, InstB, Info0, _).

inst_matches_binding_allow_any_any(ModuleInfo, Type, InstA, InstB) :-
    Info0 = init_inst_match_info(ModuleInfo, no_inst_var_sub,
        uc_match, any_does_match_any, ground_matches_bound_if_complete),
    inst_matches_binding_mt(cs_none, Type, InstA, InstB, Info0, _).

%---------------------%

:- pred inst_matches_binding_mt(calculate_sub::in, mer_type::in,
    mer_inst::in, mer_inst::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_binding_mt(CalcSub, Type, InstA, InstB, !Info) :-
    ThisExpansion = inst_match_inputs(InstA, InstB, Type),
    Expansions0 = !.Info ^ imi_expansions,
    ( if expansion_insert_new(ThisExpansion, Expansions0, Expansions) then
        !Info ^ imi_expansions := Expansions,
        inst_expand_and_remove_constrained_inst_vars(!.Info ^ imi_module_info,
            InstA, ExpandedInstA),
        inst_expand_and_remove_constrained_inst_vars(!.Info ^ imi_module_info,
            InstB, ExpandedInstB),
        inst_matches_binding_2(CalcSub, Type,
            ExpandedInstA, ExpandedInstB, !Info)
    else
        true
    ).

:- pred inst_matches_binding_2(calculate_sub::in, mer_type::in,
    mer_inst::in(mer_inst_expanded), mer_inst::in(mer_inst_expanded),
    inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_binding_2(CalcSub, Type, InstA, InstB, !Info) :-
    require_complete_switch [InstA]
    (
        InstA = free,
        InstB = free
    ;
        InstA = ground(_UniqA, HOInstInfoA),
        require_complete_switch [InstB]
        (
            InstB = ground(_UniqB, HOInstInfoB),
            ho_inst_info_matches_binding(!.Info ^ imi_module_info,
                Type, HOInstInfoA, HOInstInfoB)
        ;
            InstB = bound(_UniqB, InstResultsB, BoundInstsB),
            inst_results_bound_inst_list_is_ground_mt(!.Info ^ imi_module_info,
                Type, InstResultsB, BoundInstsB),
            inst_contains_nondefault_func_mode_1(CalcSub, Type, InstB, no,
                !Info),
            % We can only do this check if the type is known.
            bound_inst_list_is_complete_for_type(!.Info ^ imi_module_info,
                set.init, Type, BoundInstsB)
        ;
            InstB = any(UniqB, HOInstInfoB),
            maybe_any_to_bound(!.Info ^ imi_module_info, Type, UniqB,
                HOInstInfoB, NextInstB),
            inst_matches_binding_mt(CalcSub, Type, InstA, NextInstB, !Info)
        ;
            ( InstB = free
            ; InstB = not_reached
            ; InstB = constrained_inst_vars(_, _)
            ; InstB = inst_var(_)
            ),
            % XXX The failures in these cases used to be implicit.
            % For some of these cases, failure may be the wrong thing to do.
            fail
        )
    ;
        InstA = bound(_UniqA, InstResultsA, BoundInstsA),
        require_complete_switch [InstB]
        (
            InstB = any(UniqB, HOInstInfoB),
            maybe_any_to_bound(!.Info ^ imi_module_info, Type, UniqB,
                HOInstInfoB, NextInstB),
            inst_matches_binding_mt(CalcSub, Type, InstA, NextInstB, !Info)
        ;
            InstB = bound(_UniqB, _InstResultB, BoundInstsB),
            bound_inst_list_matches_binding(CalcSub, Type,
                BoundInstsA, BoundInstsB, !Info)
        ;
            InstB = ground(_UniqB, none_or_default_func),
            inst_results_bound_inst_list_is_ground_mt(!.Info ^ imi_module_info,
                Type, InstResultsA, BoundInstsA),
            inst_contains_nondefault_func_mode_1(CalcSub, Type, InstA, no,
                !Info)
        ;
            ( InstB = free
            ; InstB = not_reached
            ; InstB = constrained_inst_vars(_, _)
            ; InstB = inst_var(_)
            ),
            % XXX The failures in these cases used to be implicit.
            % For some of these cases, failure may be the wrong thing to do.
            fail
        )
    ;
        InstA = any(UniqA, HOInstInfoA),
        (
            InstB = ground(_, _),
            maybe_any_to_bound(!.Info ^ imi_module_info, Type, UniqA,
                HOInstInfoA, NextInstA),
            inst_matches_binding_mt(CalcSub, Type, NextInstA, InstB, !Info)
        ;
            InstB = bound(_, _, _),
            maybe_any_to_bound(!.Info ^ imi_module_info, Type, UniqA,
                HOInstInfoA, NextInstA),
            inst_matches_binding_mt(CalcSub, Type, NextInstA, InstB, !Info)
        ;
            InstB = any(UniqB, HOInstInfoB),
            % Note that `any' is *not* considered to match `any' unless
            % Info ^ any_matches_any = yes or the type is not a solver type
            % (and does not contain any solver types).
            AnyMatchesAny = !.Info ^ imi_any_matches_any,
            (
                AnyMatchesAny = any_does_match_any,
                ho_inst_info_matches_final(CalcSub, Type,
                    HOInstInfoA, HOInstInfoB, !Info)
            ;
                AnyMatchesAny = any_does_not_match_any,
                maybe_any_to_bound(!.Info ^ imi_module_info, Type, UniqA,
                    HOInstInfoA, NextInstA),
                maybe_any_to_bound(!.Info ^ imi_module_info, Type, UniqB,
                    HOInstInfoB, NextInstB),
                inst_matches_binding_mt(CalcSub, Type,
                    NextInstA, NextInstB, !Info)
            )
        ;
            ( InstB = free
            ; InstB = not_reached
            ; InstB = constrained_inst_vars(_, _)
            ; InstB = inst_var(_)
            ),
            % XXX The failures in these cases used to be implicit.
            % For some of these cases, failure may be the wrong thing to do.
            fail
        )
    ;
        InstA = not_reached
    ;
        ( InstA = constrained_inst_vars(_, _)
        ; InstA = inst_var(_)
        ),
        % XXX The failures in these cases used to be implicit.
        % For some of these cases, failure may be the wrong thing to do.
        fail
    ).

%---------------------%

:- pred ho_inst_info_matches_binding(module_info::in, mer_type::in,
    ho_inst_info::in, ho_inst_info::in) is semidet.

ho_inst_info_matches_binding(ModuleInfo, Type, HOInstInfoA, HOInstInfoB) :-
    (
        HOInstInfoB = none_or_default_func,
        ho_inst_info_matches_ground(ModuleInfo, Type, HOInstInfoA)
    ;
        HOInstInfoB = higher_order(PredInstB),
        (
            HOInstInfoA = none_or_default_func,
            PredInstB = pred_inst_info(pf_function, ArgModes, _, _Det),
            Arity = list.length(ArgModes),
            PredInstA = pred_inst_info_default_func_mode(Arity)
        ;
            HOInstInfoA = higher_order(PredInstA)
        ),
        pred_inst_matches(ModuleInfo, Type, PredInstA, PredInstB)
    ).

%---------------------%

    % Here we check that the functors in the first list are a subset of the
    % functors in the second list. (If a bound(...) inst only specifies
    % the insts for some of the constructors of its type, then it implicitly
    % means that all other constructors must have all their arguments
    % `not_reached'.) The code here makes use of the fact that the bound_inst
    % lists are sorted.
    %
:- pred bound_inst_list_matches_binding(calculate_sub::in, mer_type::in,
    list(bound_inst)::in, list(bound_inst)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

bound_inst_list_matches_binding(_, _, [], _, !Info).
bound_inst_list_matches_binding(CalcSub, Type,
        [BoundInstX | BoundInstXs], [BoundInstY | BoundInstYs], !Info) :-
    BoundInstX = bound_functor(ConsIdX, ArgInstsX),
    BoundInstY = bound_functor(ConsIdY, ArgInstsY),
    ( if equivalent_cons_ids(ConsIdX, ConsIdY) then
        get_cons_id_arg_types_for_bound_inst(!.Info ^ imi_module_info, Type,
            BoundInstX, Types),
        inst_list_matches_binding(CalcSub, Types, ArgInstsX, ArgInstsY, !Info),
        bound_inst_list_matches_binding(CalcSub, Type,
            BoundInstXs, BoundInstYs, !Info)
    else
        first_unqual_cons_id_is_greater(ConsIdX, ConsIdY),
        % ConsIdX does not occur in [X | Xs].
        % Hence [X | Xs] implicitly specifies `not_reached' for the args
        % of ConsIdY, and hence automatically matches_binding Y. We just
        % need to check that [X | Xs] matches_binding Ys.
        bound_inst_list_matches_binding(CalcSub, Type,
            [BoundInstX | BoundInstXs], BoundInstYs, !Info)
    ).

:- pred inst_list_matches_binding(calculate_sub::in, list(mer_type)::in,
    list(mer_inst)::in, list(mer_inst)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_list_matches_binding(_, [], [], [], !Info).
inst_list_matches_binding(CalcSub, [Type | Types],
        [ArgInstA | ArgInstsA], [ArgInstB | ArgInstsB], !Info) :-
    inst_matches_binding_mt(CalcSub, Type, ArgInstA, ArgInstB, !Info),
    inst_list_matches_binding(CalcSub, Types, ArgInstsA, ArgInstsB, !Info).

%---------------------------------------------------------------------------%

inst_contains_nondefault_func_mode(ModuleInfo, Type, Inst) :-
    Info = init_inst_match_info(ModuleInfo, no_inst_var_sub, uc_match,
        any_does_match_any, ground_matches_bound_if_complete),
    inst_contains_nondefault_func_mode_1(cs_none, Type, Inst, yes, Info, _).

:- pred inst_contains_nondefault_func_mode_1(calculate_sub::in, mer_type::in,
    mer_inst::in, bool::out,
    inst_match_info::in, inst_match_info::out) is det.

inst_contains_nondefault_func_mode_1(CalcSub, Type, Inst,
        ContainsNonstd, !Info) :-
    inst_contains_nondefault_func_mode_2(CalcSub, Type, Inst, set.init,
        ContainsNonstd, !Info).

%---------------------%

:- pred inst_contains_nondefault_func_mode_2(calculate_sub::in, mer_type::in,
    mer_inst::in, set(inst_name)::in, bool::out,
    inst_match_info::in, inst_match_info::out) is det.

inst_contains_nondefault_func_mode_2(CalcSub, Type, Inst, !.Expansions,
        ContainsNonstd, !Info) :-
    (
        ( Inst = free
        ; Inst = not_reached
        ),
        ContainsNonstd = no
    ;
        Inst = ground(_, HOInstInfo),
        ( if
            ho_inst_info_matches_ground_1(CalcSub, Type, HOInstInfo, !Info)
        then
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
            bound_inst_list_contains_nondefault_func_mode(CalcSub, Type,
                BoundInsts, !.Expansions, ContainsNonstd, !Info)
        )
    ;
        Inst = any(_, _),
        % XXX This code preserves the old behavior of the predicate that
        % preceded this function, but it is arguably incorrect, since
        % any/2 insts, like ground/2 insts, contain a ho_inst_info.
        ContainsNonstd = no
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_contains_nondefault_func_mode_2(CalcSub, Type, SubInst,
            !.Expansions, ContainsNonstd, !Info)
    ;
        Inst = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        Inst = defined_inst(InstName),
        ( if set.insert_new(InstName, !Expansions) then
            inst_lookup(!.Info ^ imi_module_info, InstName, SubInst),
            inst_contains_nondefault_func_mode_2(CalcSub, Type, SubInst,
                !.Expansions, ContainsNonstd, !Info)
        else
            ContainsNonstd = no
        )
    ).

%---------------------%

:- pred bound_inst_list_contains_nondefault_func_mode(calculate_sub::in,
    mer_type::in, list(bound_inst)::in, set(inst_name)::in, bool::out,
    inst_match_info::in, inst_match_info::out) is det.

bound_inst_list_contains_nondefault_func_mode(_, _, [], _Expansions,
        no, !Info).
bound_inst_list_contains_nondefault_func_mode(CalcSub, Type,
        [BoundInst | BoundInsts], Expansions, ContainsNonstd, !Info) :-
    BoundInst = bound_functor(_ConsId, ArgInsts),
    get_cons_id_arg_types_for_bound_inst(!.Info ^ imi_module_info, Type,
        BoundInst, ArgTypes),
    inst_list_contains_nondefault_func_mode(CalcSub, ArgTypes, ArgInsts,
        Expansions, HeadContainsNonstd, !Info),
    (
        HeadContainsNonstd = yes,
        ContainsNonstd = yes
    ;
        HeadContainsNonstd = no,
        bound_inst_list_contains_nondefault_func_mode(CalcSub, Type,
            BoundInsts, Expansions, ContainsNonstd, !Info)
    ).

:- pred inst_list_contains_nondefault_func_mode(calculate_sub::in,
    list(mer_type)::in, list(mer_inst)::in, set(inst_name)::in, bool::out,
    inst_match_info::in, inst_match_info::out) is det.

inst_list_contains_nondefault_func_mode(_, [], [], _Expansions, no, !Info).
inst_list_contains_nondefault_func_mode(_, [], [_ | _], _, _, !Info) :-
    unexpected($pred, "list length mismatch").
inst_list_contains_nondefault_func_mode(_, [_ | _], [], _, _, !Info) :-
    unexpected($pred, "list length mismatch").
inst_list_contains_nondefault_func_mode(CalcSub, [Type | Types], [Inst | Insts],
        Expansions, ContainsNonstd, !Info) :-
    inst_contains_nondefault_func_mode_2(CalcSub, Type, Inst,
        Expansions, HeadContainsNonstd, !Info),
    (
        HeadContainsNonstd = yes,
        ContainsNonstd = yes
    ;
        HeadContainsNonstd = no,
        inst_list_contains_nondefault_func_mode(CalcSub, Types, Insts,
            Expansions, ContainsNonstd, !Info)
    ).

%---------------------------------------------------------------------------%

ho_inst_info_matches_ground(ModuleInfo, Type, HOInstInfo) :-
    Info = init_inst_match_info(ModuleInfo, no_inst_var_sub, uc_match,
        any_does_match_any, ground_matches_bound_if_complete),
    ho_inst_info_matches_ground_1(cs_none, Type, HOInstInfo, Info, _).

:- pred ho_inst_info_matches_ground_1(calculate_sub::in, mer_type::in,
    ho_inst_info::in,
    inst_match_info::in, inst_match_info::out) is semidet.

ho_inst_info_matches_ground_1(CalcSub, Type, HOInstInfo, !Info) :-
    (
        HOInstInfo = higher_order(PredInst),
        pred_inst_matches_ground_1(CalcSub, Type, PredInst, !Info)
    ;
        HOInstInfo = none_or_default_func
    ).

%---------------------%

pred_inst_matches_ground(ModuleInfo, Type, PredInst) :-
    Info = init_inst_match_info(ModuleInfo, no_inst_var_sub, uc_match,
        any_does_match_any, ground_matches_bound_if_complete),
    pred_inst_matches_ground_1(cs_none, Type, PredInst, Info, _).

:- pred pred_inst_matches_ground_1(calculate_sub::in, mer_type::in,
    pred_inst_info::in, inst_match_info::in, inst_match_info::out) is semidet.

pred_inst_matches_ground_1(CalcSub, Type, PredInst, !Info) :-
    % NOTE CalcSub is set to cs_none by pred_inst_matches_ground above,
    % but NOT by ho_inst_info_matches_ground_1.
    PredInst = pred_inst_info(PredOrFunc, ArgModes, _, _),
    (
        PredOrFunc = pf_predicate
    ;
        PredOrFunc = pf_function,
        Arity = list.length(ArgModes),
        DefaultFunc = pred_inst_info_default_func_mode(Arity),
        pred_inst_matches_1(CalcSub, Type, PredInst, DefaultFunc, !Info)
    ).

%---------------------%

pred_inst_matches(ModuleInfo, Type, PredInstA, PredInstB) :-
    Info0 = init_inst_match_info(ModuleInfo, no_inst_var_sub,
        uc_match, any_does_match_any, ground_matches_bound_if_complete),
    pred_inst_matches_1(cs_none, Type, PredInstA, PredInstB, Info0, _).

    % pred_inst_matches_1(Type, PredInstA, PredInstB, !Info)
    %
    % Same as pred_inst_matches/4, except that it updates the inst_var_sub
    % in the inst_match_info, and that any inst pairs in !.Info ^ expansions
    % are assumed to match_final each other. (This avoids infinite loops
    % when calling inst_matches_final on higher-order recursive insts.)
    %
:- pred pred_inst_matches_1(calculate_sub::in, mer_type::in,
    pred_inst_info::in, pred_inst_info::in,
    inst_match_info::in, inst_match_info::out) is semidet.

pred_inst_matches_1(CalcSub, Type, PredInstA, PredInstB, !Info) :-
    % In the float_regs.m pass a variable may take on pred insts which differ
    % only in the arg reg lists in different branches. They should be allowed
    % to match here.
    %
    % NOTE CalcSub is set to cs_none by pred_inst_matches above,
    % but NOT by ho_inst_info_matches_{initial,final}.
    PredInstA = pred_inst_info(PredOrFunc, ModesA, _MaybeArgRegsA, Det),
    PredInstB = pred_inst_info(PredOrFunc, ModesB, _MaybeArgRegsB, Det),
    get_higher_order_arg_types(Type, list.length(ModesA), Types),
    pred_inst_argmodes_matches(CalcSub, Types, ModesA, ModesB, !Info).

    % pred_inst_argmodes_matches(Types, ModesA, ModesB, !Info):
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
:- pred pred_inst_argmodes_matches(calculate_sub::in, list(mer_type)::in,
    list(mer_mode)::in, list(mer_mode)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

pred_inst_argmodes_matches(_, [], [], [], !Info).
pred_inst_argmodes_matches(CalcSub, [Type | Types],
        [ModeA | ModeAs], [ModeB | ModeBs], !Info) :-
    ModuleInfo = !.Info ^ imi_module_info,
    mode_get_insts(ModuleInfo, ModeA, InitialA, FinalA0),
    mode_get_insts(ModuleInfo, ModeB, InitialB, FinalB),
    InitialCalcSub = swap_calculate_sub(CalcSub),
    inst_matches_final_mt(InitialCalcSub, Type, InitialB, InitialA, !Info),
    % Apply the substitution computed so far (it may be necessary for InitialA
    % as well).
    maybe_apply_substitution(!.Info, FinalA0, FinalA),
    inst_matches_final_mt(CalcSub, Type, FinalA, FinalB, !Info),
    pred_inst_argmodes_matches(CalcSub, Types, ModeAs, ModeBs, !Info).

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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type inst_match_inputs
    --->    inst_match_inputs(
                mer_inst,
                mer_inst,
                mer_type
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

%---------------------------------------------------------------------------%

:- type inst_match_info
    --->    inst_match_info(
                imi_module_info             :: module_info,
                imi_expansions              :: expansions,
                imi_maybe_sub               :: maybe_inst_var_sub,
                imi_uniqueness_comparison   :: uniqueness_comparison,
                imi_any_matches_any         :: any_matches_any,
                imi_ground_matches_bound    :: ground_matches_bound
            ).

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

:- type maybe_inst_var_sub
    --->    no_inst_var_sub
    ;       inst_var_sub(inst_var_sub).
            % The inst_var_sub records what inst should be substituted for each
            % inst_var that occurs in the called procedure's argument modes.

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

:- type calculate_sub_dir =< calculate_sub
    --->    cs_forward
    ;       cs_reverse.

:- type any_matches_any
    --->    any_does_not_match_any
    ;       any_does_match_any.

:- func init_inst_match_info(module_info, maybe_inst_var_sub,
    uniqueness_comparison, any_matches_any, ground_matches_bound) =
    inst_match_info.

init_inst_match_info(ModuleInfo, MaybeSub, UniqCmp,
        AnyMatchesAny, GroundMatchesBound) =
    inst_match_info(ModuleInfo, expansion_init, MaybeSub, UniqCmp,
        AnyMatchesAny, GroundMatchesBound).

%---------------------------------------------------------------------------%

:- func swap_calculate_sub(calculate_sub) = calculate_sub.

swap_calculate_sub(cs_forward) = cs_reverse.
swap_calculate_sub(cs_reverse) = cs_forward.
swap_calculate_sub(cs_none) = cs_none.

%---------------------------------------------------------------------------%

:- type inst_var_subs_result
    --->    ivsr_recurse(mer_inst, mer_inst) % CalcSub = cs_forward.
    ;       ivsr_continue(calculate_sub, mer_inst, mer_inst).

:- inst inst_var_subs_result for inst_var_subs_result/0
    --->    ivsr_recurse(ground, ground)
    ;       ivsr_continue(ground, mer_inst_expanded, mer_inst_expanded).
            % We construct ivsr_continue in two places: one place each in
            % maybe_handle_inst_var_subs and do_handle_inst_var_subs.
            % The latter guarantees that the two mer_insts will both be
            % mer_inst_expanded_nc, but the former only guarantees
            % mer_inst_expanded, and that only because that is the inst
            % of its input.
            %
            % Since both callers of maybe_handle_inst_var_subs have to be
            % prepared to handle constrained_inst_vars insts being returned
            % in ivsr_continue, we can guarantee only the looser inst.

:- pred maybe_handle_inst_var_subs(calculate_sub::in, mer_type::in,
    mer_inst::in(mer_inst_expanded), mer_inst::in(mer_inst_expanded),
    inst_var_subs_result::out(inst_var_subs_result),
    inst_match_info::in, inst_match_info::out) is semidet.

maybe_handle_inst_var_subs(CalcSub, Type, InstA, InstB, Result, !Info) :-
    (
        CalcSub = cs_forward,
        CalcSubDir = coerce(CalcSub),
        do_handle_inst_var_subs(CalcSubDir, Type, InstA, InstB, Result, !Info)
    ;
        CalcSub = cs_reverse,
        CalcSubDir = coerce(CalcSub),
        do_handle_inst_var_subs(CalcSubDir, Type, InstB, InstA, Result, !Info)
    ;
        CalcSub = cs_none,
        Result = ivsr_continue(cs_none, InstA, InstB)
    ).

:- pred do_handle_inst_var_subs(calculate_sub_dir::in, mer_type::in,
    mer_inst::in(mer_inst_expanded), mer_inst::in(mer_inst_expanded),
    inst_var_subs_result::out(inst_var_subs_result),
    inst_match_info::in, inst_match_info::out) is semidet.

do_handle_inst_var_subs(CalcSubDir, Type, InstA, InstB, Result, !Info) :-
    require_complete_switch [InstB]
    (
        InstB = constrained_inst_vars(InstVarsB, SubInstB),
        % Add the substitution InstVarsB => InstA `glb` SubInstB
        % (see get_subst_inst in dmo's thesis, page 78).
        %
        % We pass `Live = is_dead' because we want
        % abstractly_unify(unique, unique) = unique, not shared.
        ModuleInfo0 = !.Info ^ imi_module_info,
        abstractly_unify_inst(Type, is_dead, fake_unify, InstA, SubInstB,
            UnifyInst, _Det, ModuleInfo0, ModuleInfo),
        !Info ^ imi_module_info := ModuleInfo,
        update_inst_var_sub(InstVarsB, UnifyInst, Type, !Info),

        % Check that InstA matches InstB after applying the substitution
        % to InstB.
        ( if UnifyInst = constrained_inst_vars(InstVarsB, UnifySubInst) then
            % Avoid infinite regress.
            swap_insts_back(CalcSubDir, InstA, UnifySubInst, Inst1, Inst2)
        else
            swap_insts_back(CalcSubDir, InstA, UnifyInst, Inst1, Inst2)
        ),
        Result = ivsr_recurse(Inst1, Inst2)
    ;
        ( InstB = free
        ; InstB = ground(_, _)
        ; InstB = any(_, _)
        ; InstB = bound(_, _, _)
        ; InstB = not_reached
        ; InstB = inst_var(_)
        ),
        require_complete_switch [InstA]
        (
            InstA = constrained_inst_vars(_InstVarsA, SubInstA),
            % XXX The code for InstB being constrained_inst_vars unifies
            % SubInstB with InstA, and records the resulting UnifyInst against
            % InstVarsB. However, if it is InstA that is bound to
            % constrained_inst_vars/2, then here we *throw away* this wrapper;
            % we do not unify SubInstA with InstB, and therefore we cannot
            % record its results against _InstVarsA. This strikes me (zs)
            % as strange.
            %
            % NOTE: our *caller* *may* end up unifying SubInstA with InstB,
            % but it has no access to _InstVarsA, since we throw it away here.
            swap_insts_back(CalcSubDir, SubInstA, InstB, Inst1, Inst2),
            Result = ivsr_recurse(Inst1, Inst2)
        ;
            ( InstA = free
            ; InstA = ground(_, _)
            ; InstA = any(_, _)
            ; InstA = bound(_, _, _)
            ; InstA = not_reached
            ; InstA = inst_var(_)
            ),
            swap_insts_back(CalcSubDir, InstA, InstB, Inst1, Inst2),
            Result = ivsr_continue(cs_forward, Inst1, Inst2)
        )
    ).

:- pred swap_insts_back(calculate_sub_dir,
    mer_inst, mer_inst, mer_inst, mer_inst).
% This mode is not yet needed, but it should be needed later
% to express the inst of the ivsr_continue term constructed above.
% Note that this will kind of ivsr_continue will have to be separated
% from the ivsr_continue constructed by maybe_handle_inst_var_subs.
% :- mode swap_insts_back(in,
%     in(mer_inst_expanded_nc), in(mer_inst_expanded_nc),
%     out(mer_inst_expanded_nc), out(mer_inst_expanded_nc)) is det.
:- mode swap_insts_back(in,
    in(mer_inst_expanded), in(mer_inst_expanded),
    out(mer_inst_expanded), out(mer_inst_expanded)) is det.
:- mode swap_insts_back(in, in, in, out, out) is det.

swap_insts_back(CalcSubDir, InstA, InstB, Inst1, Inst2) :-
    (
        CalcSubDir = cs_forward,
        % handle_inst_var_subs did not swap InstA and InstB; no swap-back.
        Inst1 = InstA,
        Inst2 = InstB
    ;
        CalcSubDir = cs_reverse,
        % handle_inst_var_subs *did* swap InstA and InstB; swap them back.
        Inst1 = InstB,
        Inst2 = InstA
    ).

%---------------------------------------------------------------------------%

    % Update the inst_var_sub that is computed by inst_matches_initial.
    %
:- pred update_inst_var_sub(set(inst_var)::in, mer_inst::in, mer_type::in,
    inst_match_info::in, inst_match_info::out) is semidet.

update_inst_var_sub(InstVars, InstA, Type, !Info) :-
    (
        !.Info ^ imi_maybe_sub = inst_var_sub(_),
        set.fold(update_inst_var_sub_2(InstA, Type), InstVars, !Info)
    ;
        !.Info ^ imi_maybe_sub = no_inst_var_sub
    ).

:- pred update_inst_var_sub_2(mer_inst::in, mer_type::in, inst_var::in,
    inst_match_info::in, inst_match_info::out) is semidet.

update_inst_var_sub_2(InstA, Type, InstVar, !Info) :-
    (
        !.Info ^ imi_maybe_sub = inst_var_sub(InstVarSub0),
        ( if map.search(InstVarSub0, InstVar, InstB) then
            % If InstVar already has an inst associated with it, merge
            % the old and new insts. Fail if this merge is not possible.
            ModuleInfo0 = !.Info ^ imi_module_info,
            inst_merge(Type, InstA, InstB, InstAB, ModuleInfo0, ModuleInfo),
            !Info ^ imi_module_info := ModuleInfo,
            map.det_update(InstVar, InstAB, InstVarSub0, InstVarSub),
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

%---------------------------------------------------------------------------%

    % A list(bound_inst) is ``complete'' for a given type iff
    %
    % - it includes each functor of that type, and
    % - each argument of each functor is also ``complete'' for its type.
    %
:- pred bound_inst_list_is_complete_for_type(module_info::in,
    set(inst_name)::in, mer_type::in, list(bound_inst)::in) is semidet.

bound_inst_list_is_complete_for_type(ModuleInfo, Expansions, Type,
        BoundInsts) :-
    % Is this a type for which cons_ids are recorded in the type_table?
    type_is_du_type(ModuleInfo, Type),

    all_du_ctor_arg_types(ModuleInfo, Type, NamesAritiesArgTypes0),
    list.sort(NamesAritiesArgTypes0, NamesAritiesArgTypes1),
    bound_inst_list_is_complete_for_type_loop(ModuleInfo, Expansions,
        BoundInsts, NamesAritiesArgTypes1, NamesAritiesArgTypes),
    % Each and every NamesAritiesArgTypes left over specifies
    % a data in Type that BoundInsts did not cover.
    NamesAritiesArgTypes = [].

:- pred bound_inst_list_is_complete_for_type_loop(module_info::in,
    set(inst_name)::in, list(bound_inst)::in,
    list({string, arity, list(mer_type)})::in,
    list({string, arity, list(mer_type)})::out) is semidet.

bound_inst_list_is_complete_for_type_loop(_ModuleInfo, _Expansions,
        [], !NamesAritiesArgTypes).
bound_inst_list_is_complete_for_type_loop(ModuleInfo, Expansions,
        [BoundInst | BoundInsts], !NamesAritiesArgTypes) :-
    BoundInst = bound_functor(InstConsId, ArgInsts),
    InstConsId = du_data_ctor(InstDuCtor),
    InstDuCtor = du_ctor(InstSymName, InstArity, _InstTypeCtor),
    % We are assuming here that BoundInst is sorted on cons_ids.
    ( if
        !.NamesAritiesArgTypes = [NameArityArgTypes | !:NamesAritiesArgTypes],
        NameArityArgTypes = {unqualify_name(InstSymName), InstArity, ArgTypes},
        list.map(inst_is_complete_for_type(ModuleInfo, Expansions),
            ArgTypes, ArgInsts)
    then
        bound_inst_list_is_complete_for_type_loop(ModuleInfo, Expansions,
            BoundInsts, !NamesAritiesArgTypes)
    else
        fail
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
        ; Inst = any(_, _)
        ; Inst = ground(_, _)
        ; Inst = inst_var(_)
        ; Inst = constrained_inst_vars(_, _)
        )
    ;
        Inst = not_reached,
        fail
    ).

%---------------------------------------------------------------------------%

    % Check that the first cons_id is lexically greater than the second,
    % after all module qualifiers have been removed.
    %
:- pred first_unqual_cons_id_is_greater(cons_id::in, cons_id::in) is semidet.

first_unqual_cons_id_is_greater(ConsIdA, ConsIdB) :-
    ( if
        ConsIdA = du_data_ctor(du_ctor(QNameA, ArityA, _)),
        ConsIdB = du_data_ctor(du_ctor(QNameB, ArityB, _))
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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

:- pred same_addr_insts(mer_inst::in, mer_inst::in) is semidet.

:- pragma foreign_proc("C",
    same_addr_insts(InstA::in, InstB::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((void *) InstA == (void *) InstB);
").

same_addr_insts(_, _) :-
    semidet_fail.

%---------------------------------------------------------------------------%
:- end_module check_hlds.inst_match.
%---------------------------------------------------------------------------%
