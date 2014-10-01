%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2000-2012 The University of Melbourne.
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
% rafe: XXX The following comment needs revising in the light of
% the new solver types design.
%
% The handling of `any' insts is not complete. (See also inst_util.m).
% It would be nice to allow `free' to match `any', but right now we only allow
% a few special cases of that. The reason is that although the mode analysis
% would be pretty straight-forward, generating the correct code is quite a bit
% trickier. modes.m would have to be changed to handle the implicit conversions
% from `free'/`bound'/`ground' to `any' at
%
%   (1) procedure calls (this is just an extension of implied modes)
%       currently we support only the easy cases of this
%   (2) the end of branched goals
%   (3) the end of predicates.
%
% Since that is not yet done, we currently require the user to insert explicit
% calls to initialize constraint variables.
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

    % inst_expand(ModuleInfo, Inst0, Inst) checks if the top-level part
    % of the inst is a defined inst, and if so replaces it with the definition.
    %
:- pred inst_expand(module_info::in, mer_inst::in, mer_inst::out) is det.

    % inst_expand_and_remove_constrained_inst_vars is the same as inst_expand
    % except that it also removes constrained_inst_vars from the top level,
    % replacing them with the constraining inst.
    %
:- pred inst_expand_and_remove_constrained_inst_vars(module_info::in,
    mer_inst::in, mer_inst::out) is det.

%-----------------------------------------------------------------------------%

    % inst_matches_initial(InstA, InstB, Type, ModuleInfo):
    %
    % Succeed iff `InstA' specifies at least as much information as `InstB',
    % and in those parts where they specify the same information, `InstA'
    % is at least as instantiated as `InstB'. Thus, the call
    % inst_matches_initial(not_reached, ground, _) succeeds, since
    % not_reached contains more information than ground - but not vice versa.
    % Similarly, inst_matches_initial(bound(a), bound(a;b), _) should
    % succeed, but not vice versa.
    %
:- pred inst_matches_initial(mer_inst::in, mer_inst::in, mer_type::in,
    module_info::in) is semidet.

    % This version of inst_matches_initial builds up a substitution map
    % (inst_var_sub). For each inst_var which occurs in InstA there will be
    % a substitution to the corresponding inst in InstB.
    %
:- pred inst_matches_initial_sub(mer_inst::in, mer_inst::in, mer_type::in,
    module_info::in, module_info::out, inst_var_sub::in, inst_var_sub::out)
    is semidet.

    % This version of inst_matches_initial does not allow implied modes.
    % This makes it almost the same as inst_matches_final. The only difference
    % is in the way it handles constrained_inst_vars.
    %
:- pred inst_matches_initial_no_implied_modes(mer_inst::in, mer_inst::in,
    mer_type::in, module_info::in) is semidet.

    % A version of the above that also computes the inst_var_sub.
    %
:- pred inst_matches_initial_no_implied_modes_sub(mer_inst::in, mer_inst::in,
    mer_type::in, module_info::in, module_info::out,
    inst_var_sub::in, inst_var_sub::out) is semidet.

    % inst_matches_final(InstA, InstB, ModuleInfo):
    %
    % Succeed iff InstA is compatible with InstB, i.e. iff InstA will satisfy
    % the final inst requirement InstB. This is true if the information
    % specified by InstA is at least as great as that specified by InstB,
    % and where the information is the same and both insts specify a binding,
    % the binding must be identical.
    %
:- pred inst_matches_final(mer_inst::in, mer_inst::in, module_info::in)
    is semidet.

    % This version of inst_matches_final allows you to pass in the type of the
    % variables being compared. This allows it to be more precise (i.e. less
    % conservative) for cases such as inst_matches_final(ground(...),
    % bound(...), ...). This version is to be preferred when the type is
    % available.
    %
:- pred inst_matches_final_typed(mer_inst::in, mer_inst::in, mer_type::in,
    module_info::in) is semidet.

    % Normally ground matches bound(...) only if the latter is complete for the
    % type. However, the mode checker would reject some compiler-generated
    % predicates in the absence of mode checking. We work around the problem by
    % allowing ground to match incomplete bound insts when checking the final
    % insts of those generated predicates.
    %
:- type ground_matches_bound
    --->    ground_matches_bound_if_complete
    ;       ground_matches_bound_always.

:- pred inst_matches_final_gmb(mer_inst::in, mer_inst::in, mer_type::in,
    module_info::in, ground_matches_bound::in) is semidet.

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
:- pred inst_is_at_least_as_instantiated(mer_inst::in, mer_inst::in,
    mer_type::in, module_info::in) is semidet.

    % unique_matches_initial(A, B) succeeds if A >= B in the ordering
    % clobbered < mostly_clobbered < shared < mostly_unique < unique
    %
:- pred unique_matches_initial(uniqueness::in, uniqueness::in) is semidet.

    % unique_matches_final(A, B) succeeds if A >= B in the ordering
    % clobbered < mostly_clobbered < shared < mostly_unique < unique
    %
:- pred unique_matches_final(uniqueness::in, uniqueness::in) is semidet.

    % inst_matches_binding(InstA, InstB, Type, ModuleInfo):
    %
    % Succeed iff the binding of InstA is definitely exactly the same as
    % that of InstB. This is the same as inst_matches_final except that it
    % ignores uniqueness, and that `any' does not match itself. It is used
    % to check whether variables get bound in negated contexts.
    %
:- pred inst_matches_binding(mer_inst::in, mer_inst::in, mer_type::in,
    module_info::in) is semidet.

    % inst_matches_binding_allow_any_any is the same as
    % inst_matches_binding except that it also allows `any' to match `any'.
    %
:- pred inst_matches_binding_allow_any_any(mer_inst::in, mer_inst::in,
    mer_type::in, module_info::in) is semidet.

%-----------------------------------------------------------------------------%

    % pred_inst_matches(PredInstA, PredInstB, ModuleInfo)
    %
    % Succeeds if PredInstA specifies a pred that can be used wherever and
    % whenever PredInstB could be used. This is true if they both have the
    % same PredOrFunc indicator and the same determinism, and if the arguments
    % match using pred_inst_argmodes_match.
    %
:- pred pred_inst_matches(pred_inst_info::in, pred_inst_info::in,
    module_info::in) is semidet.

%-----------------------------------------------------------------------------%
%
% Predicates to test various properties of insts.
%
% NOTE: `not_reached' insts are considered to satisfy all of these predicates
% except inst_is_clobbered.
%

    % Succeed if the inst is fully ground (i.e. contains only `ground',
    % `bound', and `not_reached' insts, with no `free' or `any' insts).
    % This predicate succeeds for non-standard function insts so some care
    % needs to be taken since these insts may not be replaced by a less
    % precise inst that uses the higher-order mode information.
    %
:- pred inst_is_ground(module_info::in, mer_inst::in) is semidet.

    % Succeed if the inst is not partly free (i.e. contains only `any',
    % `ground', `bound', and `not_reached' insts, with no `free' insts).
    % This predicate succeeds for non-standard function insts so some care
    % needs to be taken since these insts may not be replaced by a less
    % precise inst that uses the higher-order mode information.
    %
:- pred inst_is_ground_or_any(module_info::in, mer_inst::in) is semidet.

    % Succeed if the inst is `mostly_unique' or `unique'.
    %
    % XXX The documentation on the code used to say: " inst_is_mostly_unique
    % succeeds iff the inst passed is unique, mostly_unique, or free.
    % Abstract insts are not considered unique.". The part about free is
    % dubious.
    %
:- pred inst_is_mostly_unique(module_info::in, mer_inst::in) is semidet.

    % Succeed if the inst is `unique'.
    %
    % XXX The documentation on the code used to say: "inst_is_unique succeeds
    % iff the inst passed is unique or free. Abstract insts are not considered
    % unique.". The part about free is dubious.
    %
:- pred inst_is_unique(module_info::in, mer_inst::in) is semidet.

    % Succeed if the inst is not `mostly_unique' or `unique'.
    %
:- pred inst_is_not_partly_unique(module_info::in, mer_inst::in) is semidet.

    % Succeed if the inst is not `unique'.
    %
:- pred inst_is_not_fully_unique(module_info::in, mer_inst::in) is semidet.

    % inst_is_clobbered succeeds iff the inst passed is `clobbered'
    % or `mostly_clobbered' or if it is a user-defined inst which
    % is defined as one of those.
    %
:- pred inst_is_clobbered(module_info::in, mer_inst::in) is semidet.

:- pred inst_list_is_ground(list(mer_inst)::in, module_info::in) is semidet.

:- pred inst_list_is_ground_or_any(list(mer_inst)::in, module_info::in)
    is semidet.

:- pred inst_list_is_unique(list(mer_inst)::in, module_info::in) is semidet.

:- pred inst_list_is_mostly_unique(list(mer_inst)::in, module_info::in)
    is semidet.

:- pred inst_list_is_not_partly_unique(list(mer_inst)::in, module_info::in)
    is semidet.

:- pred inst_list_is_not_fully_unique(list(mer_inst)::in, module_info::in)
    is semidet.

:- pred bound_inst_list_is_ground(list(bound_inst)::in, module_info::in)
    is semidet.

:- pred bound_inst_list_is_ground_or_any(list(bound_inst)::in,
    module_info::in) is semidet.

:- pred bound_inst_list_is_unique(list(bound_inst)::in, module_info::in)
    is semidet.

:- pred bound_inst_list_is_mostly_unique(list(bound_inst)::in, module_info::in)
    is semidet.

:- pred bound_inst_list_is_not_partly_unique(list(bound_inst)::in,
    module_info::in) is semidet.

:- pred bound_inst_list_is_not_fully_unique(list(bound_inst)::in,
    module_info::in) is semidet.

    % inst_is_free succeeds iff the inst passed is `free'
    % or is a user-defined inst which is defined as `free'.
    % Abstract insts must not be free.
    %
:- pred inst_is_free(module_info::in, mer_inst::in) is semidet.

:- pred inst_is_any(module_info::in, mer_inst::in) is semidet.

:- pred inst_list_is_free(list(mer_inst)::in, module_info::in) is semidet.

:- pred bound_inst_list_is_free(list(bound_inst)::in, module_info::in)
    is semidet.

    % inst_is_bound succeeds iff the inst passed is not `free'
    % or is a user-defined inst which is not defined as `free'.
    % Abstract insts must be bound.
    %
:- pred inst_is_bound(module_info::in, mer_inst::in) is semidet.

:- pred inst_is_bound_to_functors(module_info::in, mer_inst::in,
    list(bound_inst)::out) is semidet.

%-----------------------------------------------------------------------------%

    % Succeed iff the specified inst contains (directly or indirectly) the
    % specified inst_name.
    %
:- pred inst_contains_instname(mer_inst::in, module_info::in, inst_name::in)
    is semidet.

    % Nondeterministically produce all the inst_vars contained in the
    % specified list of modes.
    %
:- pred mode_list_contains_inst_var(list(mer_mode)::in, module_info::in,
    inst_var::out) is nondet.

    % Given a list of insts, and a corresponding list of livenesses, return
    % true iff for every element in the list of insts, either the elemement is
    % ground or the corresponding element in the liveness list is dead.
    %
:- pred inst_list_is_ground_or_dead(list(mer_inst)::in, list(is_live)::in,
    module_info::in) is semidet.

    % Given a list of insts, and a corresponding list of livenesses, return
    % true iff for every element in the list of insts, either the elemement is
    % ground or any, or the corresponding element in the liveness list is
    % dead.
    %
:- pred inst_list_is_ground_or_any_or_dead(list(mer_inst)::in,
    list(is_live)::in, module_info::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
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
:- pragma inline(expansion_init/0).

expansion_init = set_tree234.init.

:- pred expansion_insert_new(inst_match_inputs::in,
    expansions::in, expansions::out) is semidet.
:- pragma inline(expansion_insert_new/3).

expansion_insert_new(E, S0, S) :-
    set_tree234.insert_new(E, S0, S).

%-----------------------------------------------------------------------------%

inst_expand(ModuleInfo, !Inst) :-
    ( !.Inst = defined_inst(InstName) ->
        inst_lookup(ModuleInfo, InstName, !:Inst),
        inst_expand(ModuleInfo, !Inst)
    ;
        true
    ).

inst_expand_and_remove_constrained_inst_vars(ModuleInfo, !Inst) :-
    ( !.Inst = defined_inst(InstName) ->
        inst_lookup(ModuleInfo, InstName, !:Inst),
        inst_expand(ModuleInfo, !Inst)
    ; !.Inst = constrained_inst_vars(_, !:Inst) ->
        inst_expand(ModuleInfo, !Inst)
    ;
        true
    ).

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
                imi_maybe_sub               :: maybe(inst_var_sub),
                imi_calculate_sub           :: calculate_sub,
                imi_uniqueness_comparison   :: uniqueness_comparison,
                imi_any_matches_any         :: bool,
                imi_ground_matches_bound    :: ground_matches_bound
            ).

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

:- func init_inst_match_info(module_info, maybe(inst_var_sub),
    calculate_sub, uniqueness_comparison, bool, ground_matches_bound) =
    inst_match_info.

init_inst_match_info(ModuleInfo, MaybeSub, CalculateSub, Comparison,
        AnyMatchesAny, GroundMatchesBound) =
    inst_match_info(ModuleInfo, expansion_init, MaybeSub, CalculateSub,
        Comparison, AnyMatchesAny, GroundMatchesBound).

:- type inst_matches_pred ==
    pred(mer_inst, mer_inst, maybe(mer_type),
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
    mer_inst::in, mer_inst::in, maybe(mer_type)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

unswap(P, InstA, InstB, Type, !Info) :-
    % Swap the arguments *and* undo swap_sub.
    CalculateSub = !.Info ^ imi_calculate_sub,
    !Info ^ imi_calculate_sub := swap_calculate_sub(CalculateSub),
    P(InstB, InstA, Type, !Info),
    !Info ^ imi_calculate_sub := CalculateSub.

:- func swap_calculate_sub(calculate_sub) = calculate_sub.

swap_calculate_sub(cs_forward) = cs_reverse.
swap_calculate_sub(cs_reverse) = cs_forward.
swap_calculate_sub(cs_none) = cs_none.

%-----------------------------------------------------------------------------%

:- pred handle_inst_var_subs(
    inst_matches_pred::in(inst_matches_pred),
    inst_matches_pred::in(inst_matches_pred),
    mer_inst::in, mer_inst::in, maybe(mer_type)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

handle_inst_var_subs(Recurse, Continue, InstA, InstB, Type, !Info) :-
    CalculateSub = !.Info ^ imi_calculate_sub,
    (
        CalculateSub = cs_forward,
        handle_inst_var_subs_2(Recurse, Continue, InstA, InstB,
            Type, !Info)
    ;
        CalculateSub = cs_reverse,
        % Calculate the inst var substitution with arguments swapped,
        % but swap back for inst matching.
        handle_inst_var_subs_2(unswap(Recurse), unswap(Continue),
            InstB, InstA, Type, !Info)
    ;
        CalculateSub = cs_none,
        Continue(InstA, InstB, Type, !Info)
    ).

:- pred handle_inst_var_subs_2(
    inst_matches_pred::in(inst_matches_pred),
    inst_matches_pred::in(inst_matches_pred),
    mer_inst::in, mer_inst::in, maybe(mer_type)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

handle_inst_var_subs_2(Recurse, Continue, InstA, InstB, Type, !Info) :-
    ( InstB = constrained_inst_vars(InstVarsB, SubInstB) ->
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
        ( UnifyInst = constrained_inst_vars(InstVarsB, UnifySubInst) ->
            % Avoid infinite regress.
            Recurse(InstA, UnifySubInst, Type, !Info)
        ;
            Recurse(InstA, UnifyInst, Type, !Info)
        )
    ; InstA = constrained_inst_vars(_InstVarsA, SubInstA) ->
        Recurse(SubInstA, InstB, Type, !Info)
    ;
        Continue(InstA, InstB, Type, !Info)
    ).

    % Update the inst_var_sub that is computed by inst_matches_initial.
    % The inst_var_sub records what inst should be substituted for each
    % inst_var that occurs in the called procedure's argument modes.
    %
:- pred update_inst_var_sub(set(inst_var)::in, mer_inst::in,
    maybe(mer_type)::in, inst_match_info::in, inst_match_info::out) is semidet.

update_inst_var_sub(InstVars, InstA, MaybeType, !Info) :-
    (
        !.Info ^ imi_maybe_sub = yes(_),
        set.fold(update_inst_var_sub_2(InstA, MaybeType), InstVars, !Info)
    ;
        !.Info ^ imi_maybe_sub = no
    ).

:- pred update_inst_var_sub_2(mer_inst::in, maybe(mer_type)::in, inst_var::in,
    inst_match_info::in, inst_match_info::out) is semidet.

update_inst_var_sub_2(InstA, MaybeType, InstVar, !Info) :-
    (
        !.Info ^ imi_maybe_sub = yes(InstVarSub0),
        ( map.search(InstVarSub0, InstVar, InstB) ->
            % If InstVar already has an inst associated with it, merge
            % the old inst and the new inst. Fail if this merge is not
            % possible.
            ModuleInfo0 = !.Info ^ imi_module_info,
            inst_merge(InstA, InstB, MaybeType, Inst,
                ModuleInfo0, ModuleInfo),
            !Info ^ imi_module_info := ModuleInfo,
            map.det_update(InstVar, Inst, InstVarSub0, InstVarSub),
            !Info ^ imi_maybe_sub := yes(InstVarSub)
        ;
            map.det_insert(InstVar, InstA, InstVarSub0, InstVarSub),
            !Info ^ imi_maybe_sub := yes(InstVarSub)
        )
    ;
        !.Info ^ imi_maybe_sub = no,
        InstVarSub = map.singleton(InstVar, InstA),
        !Info ^ imi_maybe_sub := yes(InstVarSub)
    ).

%-----------------------------------------------------------------------------%

inst_matches_initial(InstA, InstB, Type, ModuleInfo) :-
    inst_matches_initial_1(InstA, InstB, Type, ModuleInfo, _, no, _).

inst_matches_initial_sub(InstA, InstB, Type, !ModuleInfo, !Sub) :-
    inst_matches_initial_1(InstA, InstB, Type, !ModuleInfo,
        yes(!.Sub), MaybeSub),
    (
        MaybeSub = yes(!:Sub)
    ;
        MaybeSub = no,
        unexpected($module, $pred, "missing inst_var_sub")
    ).

inst_matches_initial_no_implied_modes(InstA, InstB, Type, ModuleInfo) :-
    Info0 = init_inst_match_info(ModuleInfo, no, cs_forward, uc_match, yes,
        ground_matches_bound_if_complete),
    inst_matches_final_mt(InstA, InstB, yes(Type), Info0, _).

inst_matches_initial_no_implied_modes_sub(InstA, InstB, Type, !ModuleInfo,
        !Sub) :-
    Info0 = init_inst_match_info(!.ModuleInfo, yes(!.Sub), cs_forward,
        uc_match, yes, ground_matches_bound_if_complete),
    inst_matches_final_mt(InstA, InstB, yes(Type), Info0, Info),
    !:ModuleInfo = Info ^ imi_module_info,
    yes(!:Sub) = Info ^ imi_maybe_sub.

:- pred inst_matches_initial_1(mer_inst::in, mer_inst::in, mer_type::in,
    module_info::in, module_info::out,
    maybe(inst_var_sub)::in, maybe(inst_var_sub)::out) is semidet.

inst_matches_initial_1(InstA, InstB, Type, !ModuleInfo, !MaybeSub) :-
    Info0 = init_inst_match_info(!.ModuleInfo, !.MaybeSub, cs_forward,
        uc_match, yes, ground_matches_bound_if_complete),
    inst_matches_initial_mt(InstA, InstB, yes(Type), Info0, Info),
    !:ModuleInfo = Info ^ imi_module_info,
    !:MaybeSub = Info ^ imi_maybe_sub.

:- pred inst_matches_initial_mt(mer_inst::in, mer_inst::in,
    maybe(mer_type)::in, inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_initial_mt(InstA, InstB, MaybeType, !Info) :-
    ThisExpansion = inst_match_inputs(InstA, InstB, MaybeType),
    Expansions0 = !.Info ^ imi_expansions,
    ( expansion_insert_new(ThisExpansion, Expansions0, Expansions) ->
        !Info ^ imi_expansions := Expansions,
        inst_expand(!.Info ^ imi_module_info, InstA, ExpandedInstA),
        inst_expand(!.Info ^ imi_module_info, InstB, ExpandedInstB),
        handle_inst_var_subs(inst_matches_initial_mt, inst_matches_initial_4,
            ExpandedInstA, ExpandedInstB, MaybeType, !Info)
    ;
        true
    ).

:- pred inst_matches_initial_4(mer_inst::in, mer_inst::in, maybe(mer_type)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_initial_4(InstA, InstB, MaybeType, !Info) :-
    % To avoid infinite regress, we assume that inst_matches_initial is true
    % for any pairs of insts which occur in `Expansions'.
    %
    % XXX Maybe we could use the inst result field of bound/3 insts
    % in some places.
    (
        InstA = any(UniqA, HOInstInfoA),
        InstB = any(UniqB, HOInstInfoB),
        !.Info ^ imi_any_matches_any = yes,
        compare_uniqueness(!.Info ^ imi_uniqueness_comparison, UniqA, UniqB),
        ho_inst_info_matches_initial(HOInstInfoA, HOInstInfoB, MaybeType,
            !Info)
    ;
        InstA = any(_, _),
        InstB = free
    ;
        InstA = any(UniqA, HOInstInfoA),
        InstB = ground(_, _),
        maybe_any_to_bound(MaybeType, !.Info ^ imi_module_info, UniqA,
            HOInstInfoA, NextInstA),
        inst_matches_initial_mt(NextInstA, InstB, MaybeType, !Info)
    ;
        InstA = any(UniqA, HOInstInfoA),
        InstB = bound(_, _, _),
        maybe_any_to_bound(MaybeType, !.Info ^ imi_module_info, UniqA,
            HOInstInfoA, NextInstA),
        inst_matches_initial_mt(NextInstA, InstB, MaybeType, !Info)
    ;
        InstA = free,
        InstB = free
    ;
        InstA = bound(UniqA, _InstResultsA, BoundInstsA),
        InstB = any(UniqB, none),
        compare_uniqueness(!.Info ^ imi_uniqueness_comparison, UniqA, UniqB),
        compare_bound_inst_list_uniq(!.Info ^ imi_uniqueness_comparison,
            BoundInstsA, UniqB, !.Info ^ imi_module_info)
    ;
        InstA = bound(_, _, _),
        InstB = free
    ;
        InstA = bound(UniqA, InstResultsA, BoundInstsA),
        InstB = bound(UniqB, _InstResultsB, BoundInstsB),
        (
            same_addr_insts(InstA, InstB),
            InstResultsA = inst_test_results_fgtc
        ->
            true
        ;
            compare_uniqueness(!.Info ^ imi_uniqueness_comparison,
                UniqA, UniqB),
            bound_inst_list_matches_initial_mt(BoundInstsA, BoundInstsB,
                MaybeType, !Info)
        )
    ;
        InstA = bound(UniqA, InstResultsA, BoundInstsA),
        InstB = ground(UniqB, none),
        compare_uniqueness(!.Info ^ imi_uniqueness_comparison, UniqA, UniqB),
        inst_results_bound_inst_list_is_ground_mt(InstResultsA, BoundInstsA,
            MaybeType, !.Info ^ imi_module_info),
        compare_bound_inst_list_uniq(!.Info ^ imi_uniqueness_comparison,
            BoundInstsA, UniqB, !.Info ^ imi_module_info)
    ;
        InstA = bound(Uniq, InstResultsA, BoundInstsA),
        InstB = abstract_inst(_,_),
        Uniq = unique,
        inst_results_bound_inst_list_is_ground_mt(InstResultsA, BoundInstsA,
            no, !.Info ^ imi_module_info),
        bound_inst_list_is_unique(BoundInstsA, !.Info ^ imi_module_info)
    ;
        InstA = bound(Uniq, InstResultsA, BoundInstsA),
        InstB = abstract_inst(_,_),
        Uniq = mostly_unique,
        inst_results_bound_inst_list_is_ground_mt(InstResultsA, BoundInstsA,
            no, !.Info ^ imi_module_info),
        bound_inst_list_is_mostly_unique(BoundInstsA, !.Info ^ imi_module_info)
    ;
        InstA = ground(UniqA, HOInstInfoA),
        InstB = any(UniqB, HOInstInfoB),
        compare_uniqueness(!.Info ^ imi_uniqueness_comparison, UniqA, UniqB),
        ho_inst_info_matches_initial(HOInstInfoA, HOInstInfoB, MaybeType,
            !Info)
    ;
        InstA = ground(_Uniq, _PredInst),
        InstB = free
    ;
        InstA = ground(UniqA, _GII_A),
        InstB = bound(UniqB, _InstResultsB, BoundInstsB),
        MaybeType = yes(Type),
        % We can only check this case properly if the type is known.
        compare_uniqueness(!.Info ^ imi_uniqueness_comparison, UniqA, UniqB),
        bound_inst_list_is_complete_for_type(set.init,
            !.Info ^ imi_module_info, BoundInstsB, Type),
        ground_matches_initial_bound_inst_list(UniqA, BoundInstsB, yes(Type),
            !Info)
    ;
        InstA = ground(UniqA, HOInstInfoA),
        InstB = ground(UniqB, HOInstInfoB),
        compare_uniqueness(!.Info ^ imi_uniqueness_comparison, UniqA, UniqB),
        ho_inst_info_matches_initial(HOInstInfoA, HOInstInfoB, MaybeType,
            !Info)
    ;
        InstA = ground(_UniqA, none),
        InstB = abstract_inst(_,_),
        % I don't know what this should do.
        % Abstract insts aren't really supported.
        unexpected($module, $pred,
            "inst_matches_initial(ground, abstract_inst) == ??")
    ;
        InstA = abstract_inst(_,_),
        InstB = any(shared, none)
    ;
        InstA = abstract_inst(_,_),
        InstB = free
    ;
        InstA = abstract_inst(Name, ArgsA),
        InstB = abstract_inst(Name, ArgsB),
        list.duplicate(length(ArgsA), no, MaybeTypes),
        % XXX how do we get the argument types for an abstract inst?
        inst_list_matches_initial_mt(ArgsA, ArgsB, MaybeTypes, !Info)
    ;
        InstA = not_reached
    ).

%-----------------------------------------------------------------------------%

    % This predicate assumes that the check of
    % `bound_inst_list_is_complete_for_type' is done by the caller.
    %
:- pred ground_matches_initial_bound_inst_list(uniqueness::in,
    list(bound_inst)::in, maybe(mer_type)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

ground_matches_initial_bound_inst_list(_, [], _, !Info).
ground_matches_initial_bound_inst_list(Uniq, [BoundInst | BoundInsts],
        MaybeType, !Info) :-
    BoundInst = bound_functor(ConsId, Args),
    maybe_get_cons_id_arg_types(!.Info ^ imi_module_info, MaybeType, ConsId,
        list.length(Args), MaybeTypes),
    ground_matches_initial_inst_list(Uniq, Args, MaybeTypes, !Info),
    ground_matches_initial_bound_inst_list(Uniq, BoundInsts, MaybeType, !Info).

:- pred ground_matches_initial_inst_list(uniqueness::in, list(mer_inst)::in,
    list(maybe(mer_type))::in, inst_match_info::in, inst_match_info::out)
    is semidet.

ground_matches_initial_inst_list(_, [], [], !Info).
ground_matches_initial_inst_list(Uniq, [Inst | Insts],
        [MaybeType | MaybeTypes], !Info) :-
    inst_matches_initial_mt(ground(Uniq, none), Inst, MaybeType, !Info),
    ground_matches_initial_inst_list(Uniq, Insts, MaybeTypes, !Info).

%-----------------------------------------------------------------------------%

    % A list(bound_inst) is ``complete'' for a given type iff it includes
    % each functor of the type and each argument of each functor is also
    % ``complete'' for its type.
    %
:- pred bound_inst_list_is_complete_for_type(set(inst_name)::in,
    module_info::in, list(bound_inst)::in, mer_type::in) is semidet.

bound_inst_list_is_complete_for_type(Expansions, ModuleInfo, BoundInsts,
        Type) :-
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
            list.map(inst_is_complete_for_type(Expansions, ModuleInfo),
                ArgInsts, ArgTypes)
        )
    ).

:- pred inst_is_complete_for_type(set(inst_name)::in, module_info::in,
    mer_inst::in, mer_type::in) is semidet.

inst_is_complete_for_type(Expansions, ModuleInfo, Inst, Type) :-
    % XXX This should be a switch on Inst.
    ( Inst = defined_inst(Name) ->
        ( set.member(Name, Expansions) ->
            true
        ;
            inst_lookup(ModuleInfo, Name, ExpandedInst),
            inst_is_complete_for_type(set.insert(Expansions, Name),
                ModuleInfo, ExpandedInst, Type)
        )
    ; Inst = bound(_, _, BoundInsts) ->
        bound_inst_list_is_complete_for_type(Expansions, ModuleInfo,
            BoundInsts, Type)
    ;
        Inst \= not_reached
    ).

    % Check that the first cons_id is lexically greater than the
    % second, after all module qualifiers have been removed.
    %
:- pred greater_than_disregard_module_qual(cons_id::in, cons_id::in)
    is semidet.

greater_than_disregard_module_qual(ConsIdA, ConsIdB) :-
    (
        ConsIdA = cons(QNameA, ArityA, _),
        ConsIdB = cons(QNameB, ArityB, _)
    ->
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
    ;
        compare((>), ConsIdA, ConsIdB)
    ).

%-----------------------------------------------------------------------------%

    % This predicate checks if two ho_inst_infos match_initial.
    % It does not check uniqueness.
    %
:- pred ho_inst_info_matches_initial(ho_inst_info::in, ho_inst_info::in,
    maybe(mer_type)::in, inst_match_info::in, inst_match_info::out) is semidet.

ho_inst_info_matches_initial(HOInstInfoA, HOInstInfoB, MaybeType, !Info) :-
    (
        HOInstInfoB = none,
        \+ ho_inst_info_is_nonstandard_func_mode(!.Info ^ imi_module_info,
            HOInstInfoA)
    ;
        HOInstInfoA = none,
        HOInstInfoB = higher_order(PredInstB),
        PredInstB = pred_inst_info(pf_function, ArgModes, _, _Det),
        Arity = list.length(ArgModes),
        PredInstA = pred_inst_info_standard_func_mode(Arity),
        pred_inst_matches_2(PredInstA, PredInstB, MaybeType, !Info)
    ;
        HOInstInfoA = higher_order(PredInstA),
        HOInstInfoB = higher_order(PredInstB),
        pred_inst_matches_2(PredInstA, PredInstB, MaybeType, !Info)
    ).

pred_inst_matches(PredInstA, PredInstB, ModuleInfo) :-
    pred_inst_matches_mt(PredInstA, PredInstB, no, ModuleInfo).

:- pred pred_inst_matches_mt(pred_inst_info::in, pred_inst_info::in,
    maybe(mer_type)::in, module_info::in) is semidet.

pred_inst_matches_mt(PredInstA, PredInstB, MaybeType, ModuleInfo) :-
    Info0 = init_inst_match_info(ModuleInfo, no, cs_none, uc_match, yes,
        ground_matches_bound_if_complete),
    pred_inst_matches_2(PredInstA, PredInstB, MaybeType, Info0, _).

    % pred_inst_matches_2(PredInstA, PredInstB, !Info)
    %
    % Same as pred_inst_matches/3, except that it updates the inst_var_sub
    % in the inst_match_info, and that any inst pairs in !.Info ^ expansions
    % are assumed to match_final each other. (This avoids infinite loops
    % when calling inst_matches_final on higher-order recursive insts.)
    %
:- pred pred_inst_matches_2(pred_inst_info::in, pred_inst_info::in,
    maybe(mer_type)::in, inst_match_info::in, inst_match_info::out) is semidet.

pred_inst_matches_2(PredInstA, PredInstB, MaybeType, !Info) :-
    % In the float_regs.m pass a variable may take on pred insts which differ
    % only in the arg reg lists in different branches. They should be allowed
    % to match here.
    PredInstA = pred_inst_info(PredOrFunc, ModesA, _MaybeArgRegsA, Det),
    PredInstB = pred_inst_info(PredOrFunc, ModesB, _MaybeArgRegsB, Det),
    maybe_get_higher_order_arg_types(MaybeType, length(ModesA), MaybeTypes),
    pred_inst_argmodes_matches(ModesA, ModesB, MaybeTypes, !Info).

    % pred_inst_argmodes_matches(ModesA, ModesB, MaybeTypes, !Info):
    %
    % succeeds if the initial insts of ModesB specify at least as much
    % information as, and the same binding as, the initial insts of ModesA;
    % and the final insts of ModesA specify at least as much information as,
    % and the same binding as, the final insts of ModesB. Any inst pairs
    % in Inst0 ^ expansions are assumed to match_final each other.
    %
    % (In other words, as far as subtyping goes it is contravariant in
    % the initial insts, and covariant in the final insts;
    % as far as binding goes, it is invariant for both.)
    %
:- pred pred_inst_argmodes_matches(list(mer_mode)::in, list(mer_mode)::in,
    list(maybe(mer_type))::in, inst_match_info::in, inst_match_info::out)
    is semidet.

pred_inst_argmodes_matches([], [], [], !Info).
pred_inst_argmodes_matches([ModeA | ModeAs], [ModeB | ModeBs],
        [MaybeType | MaybeTypes], !Info) :-
    ModuleInfo = !.Info ^ imi_module_info,
    mode_get_insts(ModuleInfo, ModeA, InitialA, FinalA0),
    mode_get_insts(ModuleInfo, ModeB, InitialB, FinalB),
    % inst_matches_final_mt should probably just accept cs_reverse directly.
    swap_sub(inst_matches_final_mt(InitialB, InitialA, MaybeType), !Info),
    % Apply the substitution computed so far (it may be necessary for InitialA
    % as well).
    maybe_apply_substitution(!.Info, FinalA0, FinalA),
    inst_matches_final_mt(FinalA, FinalB, MaybeType, !Info),
    pred_inst_argmodes_matches(ModeAs, ModeBs, MaybeTypes, !Info).

:- pred maybe_apply_substitution(inst_match_info::in,
    mer_inst::in, mer_inst::out) is det.

maybe_apply_substitution(Info, Inst0, Inst) :-
    (
        Info ^ imi_maybe_sub = yes(Subst),
        inst_apply_substitution(Subst, Inst0, Inst)
    ;
        Info ^ imi_maybe_sub = no,
        Inst = Inst0
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

:- pred compare_bound_inst_list_uniq(uniqueness_comparison::in,
    list(bound_inst)::in, uniqueness::in, module_info::in) is semidet.

compare_bound_inst_list_uniq(uc_match, BoundInsts, Uniq, ModuleInfo) :-
    bound_inst_list_matches_uniq(BoundInsts, Uniq, ModuleInfo).
compare_bound_inst_list_uniq(uc_instantiated, BoundInsts, Uniq, ModuleInfo) :-
    uniq_matches_bound_inst_list(Uniq, BoundInsts, ModuleInfo).

:- pred bound_inst_list_matches_uniq(list(bound_inst)::in, uniqueness::in,
    module_info::in) is semidet.

bound_inst_list_matches_uniq(BoundInsts, Uniq, ModuleInfo) :-
    ( Uniq = unique ->
        bound_inst_list_is_unique(BoundInsts, ModuleInfo)
    ; Uniq = mostly_unique ->
        bound_inst_list_is_mostly_unique(BoundInsts, ModuleInfo)
    ;
        true
    ).

:- pred uniq_matches_bound_inst_list(uniqueness::in, list(bound_inst)::in,
    module_info::in) is semidet.

uniq_matches_bound_inst_list(Uniq, BoundInsts, ModuleInfo) :-
    ( Uniq = shared ->
        bound_inst_list_is_not_partly_unique(BoundInsts, ModuleInfo)
    ; Uniq = mostly_unique ->
        bound_inst_list_is_not_fully_unique(BoundInsts, ModuleInfo)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

    % Here we check that the functors in the first list are a subset of the
    % functors in the second list. (If a bound(...) inst only specifies the
    % insts for some of the constructors of its type, then it implicitly means
    % that all other constructors must have all their arguments `not_reached'.)
    % The code here makes use of the fact that the bound_inst lists are sorted.
    %
:- pred bound_inst_list_matches_initial_mt(list(bound_inst)::in,
    list(bound_inst)::in, maybe(mer_type)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

bound_inst_list_matches_initial_mt([], _, _, !Info).
bound_inst_list_matches_initial_mt([X | Xs], [Y | Ys], MaybeType, !Info) :-
    X = bound_functor(ConsIdX, ArgsX),
    Y = bound_functor(ConsIdY, ArgsY),
    ( equivalent_cons_ids(ConsIdX, ConsIdY) ->
        maybe_get_cons_id_arg_types(!.Info ^ imi_module_info, MaybeType,
            ConsIdX, list.length(ArgsX), MaybeTypes),
        inst_list_matches_initial_mt(ArgsX, ArgsY, MaybeTypes, !Info),
        bound_inst_list_matches_initial_mt(Xs, Ys, MaybeType, !Info)
    ;
        greater_than_disregard_module_qual(ConsIdX, ConsIdY),
        % ConsIdY does not occur in [X | Xs].
        % Hence [X | Xs] implicitly specifies `not_reached' for the args
        % of ConsIdY, and hence automatically matches_initial Y. We just
        % need to check that [X | Xs] matches_initial Ys.
        bound_inst_list_matches_initial_mt([X | Xs], Ys, MaybeType, !Info)
    ).

:- pred inst_list_matches_initial_mt(list(mer_inst)::in, list(mer_inst)::in,
    list(maybe(mer_type))::in, inst_match_info::in, inst_match_info::out)
    is semidet.

inst_list_matches_initial_mt([], [], [], !Info).
inst_list_matches_initial_mt([X | Xs], [Y | Ys], [MaybeType | MaybeTypes],
        !Info) :-
    inst_matches_initial_mt(X, Y, MaybeType, !Info),
    inst_list_matches_initial_mt(Xs, Ys, MaybeTypes, !Info).

%-----------------------------------------------------------------------------%

inst_matches_final(InstA, InstB, ModuleInfo) :-
    Info0 = init_inst_match_info(ModuleInfo, no, cs_none, uc_match, yes,
        ground_matches_bound_if_complete),
    inst_matches_final_mt(InstA, InstB, no, Info0, _).

inst_matches_final_typed(InstA, InstB, Type, ModuleInfo) :-
    inst_matches_final_gmb(InstA, InstB, Type, ModuleInfo,
        ground_matches_bound_if_complete).

inst_matches_final_gmb(InstA, InstB, Type, ModuleInfo, GroundMatchesBound) :-
    Info0 = init_inst_match_info(ModuleInfo, no, cs_none, uc_match, yes,
        GroundMatchesBound),
    inst_matches_final_mt(InstA, InstB, yes(Type), Info0, _).

:- pred inst_matches_final_mt(mer_inst::in, mer_inst::in, maybe(mer_type)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_final_mt(InstA, InstB, MaybeType, !Info) :-
    ( InstA = InstB ->
        true
    ;
        ThisExpansion = inst_match_inputs(InstA, InstB, MaybeType),
        Expansions0 = !.Info ^ imi_expansions,
        ( expansion_insert_new(ThisExpansion, Expansions0, Expansions) ->
            !Info ^ imi_expansions := Expansions,
            inst_expand(!.Info ^ imi_module_info, InstA, ExpandedInstA),
            inst_expand(!.Info ^ imi_module_info, InstB, ExpandedInstB),
            handle_inst_var_subs(inst_matches_final_mt, inst_matches_final_3,
                ExpandedInstA, ExpandedInstB, MaybeType, !Info)
        ;
            true
        )
    ).

:- pred inst_matches_final_3(mer_inst::in, mer_inst::in, maybe(mer_type)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_final_3(InstA, InstB, MaybeType, !Info) :-
    (
        InstA = any(UniqA, HOInstInfoA),
        InstB = any(UniqB, HOInstInfoB),
        ho_inst_info_matches_final(HOInstInfoA, HOInstInfoB, MaybeType, !Info),
        unique_matches_final(UniqA, UniqB)
    ;
        InstA = any(UniqA, HOInstInfoA),
        InstB = ground(_, _)@InstB,
        maybe_any_to_bound(MaybeType, !.Info ^ imi_module_info, UniqA,
            HOInstInfoA, NextInstA),
        inst_matches_final_mt(NextInstA, InstB, MaybeType, !Info)
    ;
        InstA = any(UniqA, HOInstInfoA),
        InstB = bound(_, _, _),
        maybe_any_to_bound(MaybeType, !.Info ^ imi_module_info, UniqA,
            HOInstInfoA, NextInstA),
        inst_matches_final_mt(NextInstA, InstB, MaybeType, !Info)
    ;
        InstA = free,
        InstB = any(Uniq, _),
        % We do not yet allow `free' to match `any',
        % unless the `any' is `clobbered_any' or `mostly_clobbered_any'.
        % Among other things, changing this would break compare_inst
        % in modecheck_call.m.
        ( Uniq = clobbered ; Uniq = mostly_clobbered )
    ;
        InstA = free,
        InstB = free
    ;
        InstA = bound(UniqA, InstResultsA, BoundInstsA),
        InstB = any(UniqB, none),
        unique_matches_final(UniqA, UniqB),
        bound_inst_list_matches_uniq(BoundInstsA, UniqB,
            !.Info ^ imi_module_info),
        % We do not yet allow `free' to match `any'.
        % Among other things, changing this would break compare_inst
        % in modecheck_call.m.
        inst_results_bound_inst_list_is_ground_or_any(InstResultsA,
            BoundInstsA, !.Info ^ imi_module_info)
    ;
        InstA = bound(UniqA, _InstResultsA, BoundInstsA),
        InstB = bound(UniqB, _InstResultsB, BoundInstsB),
        unique_matches_final(UniqA, UniqB),
        bound_inst_list_matches_final(BoundInstsA, BoundInstsB, MaybeType,
            !Info)
    ;
        InstA = bound(UniqA, InstResultsA, BoundInstsA),
        InstB = ground(UniqB, none),
        unique_matches_final(UniqA, UniqB),
        inst_results_bound_inst_list_is_ground_mt(InstResultsA, BoundInstsA,
            MaybeType, !.Info ^ imi_module_info),
        bound_inst_list_matches_uniq(BoundInstsA, UniqB,
            !.Info ^ imi_module_info)
    ;
        InstA = ground(UniqA, HOInstInfoA),
        InstB = any(UniqB, HOInstInfoB),
        ho_inst_info_matches_final(HOInstInfoA, HOInstInfoB, MaybeType, !Info),
        unique_matches_final(UniqA, UniqB)
    ;
        InstA = ground(UniqA, HOInstInfoA),
        InstB = bound(UniqB, InstResultsB, BoundInstsB),
        ModuleInfo = !.Info ^ imi_module_info,
        \+ ho_inst_info_is_nonstandard_func_mode(ModuleInfo, HOInstInfoA),
        unique_matches_final(UniqA, UniqB),
        inst_results_bound_inst_list_is_ground_mt(InstResultsB, BoundInstsB,
            MaybeType, ModuleInfo),
        uniq_matches_bound_inst_list(UniqA, BoundInstsB, ModuleInfo),
        (
            MaybeType = yes(Type),
            % We can only do this check if the type is known.
            bound_inst_list_is_complete_for_type(set.init, ModuleInfo,
                BoundInstsB, Type)
        ;
            % XXX the check for bound_inst_list_is_complete_for_type makes the
            % mode checker too conservative in the absence of alias tracking.
            % Bypass the check if instructed.
            GroundMatchesBound = !.Info ^ imi_ground_matches_bound,
            GroundMatchesBound = ground_matches_bound_always
        )
    ;
        InstA = ground(UniqA, HOInstInfoA),
        InstB = ground(UniqB, HOInstInfoB),
        ho_inst_info_matches_final(HOInstInfoA, HOInstInfoB, MaybeType, !Info),
        unique_matches_final(UniqA, UniqB)
    ;
        InstA = abstract_inst(_, _),
        InstB = any(shared, none)
    ;
        InstA = abstract_inst(Name, ArgsA),
        InstB = abstract_inst(Name, ArgsB),
        list.duplicate(length(ArgsA), no, MaybeTypes),
        % XXX how do we get the argument types for an abstract inst?
        inst_list_matches_final(ArgsA, ArgsB, MaybeTypes, !Info)
    ;
        InstA = not_reached
    ;
        InstA = constrained_inst_vars(InstVarsA, SubInstA),
        ( InstB = constrained_inst_vars(InstVarsB, SubInstB) ->
            % Constrained_inst_vars match_final only if InstVarsA contains
            % all the variables in InstVarsB.
            set.subset(InstVarsB, InstVarsA),
            inst_matches_final_mt(SubInstA, SubInstB, MaybeType, !Info)
        ;
            inst_matches_final_mt(SubInstA, InstB, MaybeType, !Info)
        )
    ).

:- pred ho_inst_info_matches_final(ho_inst_info::in, ho_inst_info::in,
    maybe(mer_type)::in, inst_match_info::in, inst_match_info::out) is semidet.

ho_inst_info_matches_final(HOInstInfoA, HOInstInfoB, MaybeType, !Info) :-
    (
        HOInstInfoB = none,
        \+ ho_inst_info_is_nonstandard_func_mode(!.Info ^ imi_module_info,
            HOInstInfoA)
    ;
        HOInstInfoA = none,
        HOInstInfoB = higher_order(PredInstB),
        PredInstB = pred_inst_info(pf_function, ArgModes, _, _Det),
        Arity = list.length(ArgModes),
        PredInstA = pred_inst_info_standard_func_mode(Arity),
        pred_inst_matches_2(PredInstA, PredInstB, MaybeType, !Info)
    ;
        HOInstInfoA = higher_order(PredInstA),
        HOInstInfoB = higher_order(PredInstB),
        pred_inst_matches_2(PredInstA, PredInstB, MaybeType, !Info)
    ).

:- pred inst_list_matches_final(list(mer_inst)::in, list(mer_inst)::in,
    list(maybe(mer_type))::in, inst_match_info::in, inst_match_info::out)
    is semidet.

inst_list_matches_final([], [], [], !Info).
inst_list_matches_final([ArgA | ArgsA], [ArgB | ArgsB],
        [MaybeType | MaybeTypes], !Info) :-
    inst_matches_final_mt(ArgA, ArgB, MaybeType, !Info),
    inst_list_matches_final(ArgsA, ArgsB, MaybeTypes, !Info).

    % Here we check that the functors in the first list are a subset of the
    % functors in the second list. (If a bound(...) inst only specifies
    % the insts for some of the constructors of its type, then it implicitly
    % means that all other constructors must have all their arguments
    % `not_reached'.) The code here makes use of the fact that the bound_inst
    % lists are sorted.
    %
:- pred bound_inst_list_matches_final(list(bound_inst)::in,
    list(bound_inst)::in, maybe(mer_type)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

bound_inst_list_matches_final([], _, _, !Info).
bound_inst_list_matches_final([X | Xs], [Y | Ys], MaybeType, !Info) :-
    X = bound_functor(ConsIdX, ArgsX),
    Y = bound_functor(ConsIdY, ArgsY),
    ( equivalent_cons_ids(ConsIdX, ConsIdY) ->
        maybe_get_cons_id_arg_types(!.Info ^ imi_module_info, MaybeType,
            ConsIdX, list.length(ArgsX), MaybeTypes),
        inst_list_matches_final(ArgsX, ArgsY, MaybeTypes, !Info),
        bound_inst_list_matches_final(Xs, Ys, MaybeType, !Info)
    ;
        greater_than_disregard_module_qual(ConsIdX, ConsIdY),
        % ConsIdY does not occur in [X | Xs].
        % Hence [X | Xs] implicitly specifies `not_reached' for the args
        % of ConsIdY, and hence automatically matches_final Y. We just
        % need to check that [X | Xs] matches_final Ys.
        bound_inst_list_matches_final([X | Xs], Ys, MaybeType, !Info)
    ).

inst_is_at_least_as_instantiated(InstA, InstB, Type, ModuleInfo) :-
    Info0 = init_inst_match_info(ModuleInfo, no, cs_none, uc_instantiated, no,
        ground_matches_bound_if_complete),
    inst_matches_initial_mt(InstA, InstB, yes(Type), Info0, _).

inst_matches_binding(InstA, InstB, Type, ModuleInfo) :-
    Info0 = init_inst_match_info(ModuleInfo, no, cs_none, uc_match, no,
        ground_matches_bound_if_complete),
    inst_matches_binding_mt(InstA, InstB, yes(Type), Info0, _).

inst_matches_binding_allow_any_any(InstA, InstB, Type, ModuleInfo) :-
    Info0 = init_inst_match_info(ModuleInfo, no, cs_none, uc_match, yes,
        ground_matches_bound_if_complete),
    inst_matches_binding_mt(InstA, InstB, yes(Type), Info0, _).

:- pred inst_matches_binding_mt(mer_inst::in, mer_inst::in,
    maybe(mer_type)::in, inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_binding_mt(InstA, InstB, MaybeType, !Info) :-
    ThisExpansion = inst_match_inputs(InstA, InstB, MaybeType),
    Expansions0 = !.Info ^ imi_expansions,
    ( expansion_insert_new(ThisExpansion, Expansions0, Expansions) ->
        !Info ^ imi_expansions := Expansions,
        inst_expand_and_remove_constrained_inst_vars(!.Info ^ imi_module_info,
            InstA, ExpandedInstA),
        inst_expand_and_remove_constrained_inst_vars(!.Info ^ imi_module_info,
            InstB, ExpandedInstB),
        inst_matches_binding_3(ExpandedInstA, ExpandedInstB, MaybeType, !Info)
    ;
        true
    ).

:- pred inst_matches_binding_3(mer_inst::in, mer_inst::in, maybe(mer_type)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

inst_matches_binding_3(InstA, InstB, MaybeType, !Info) :-
    (
        InstA = free,
        InstB = free
    ;
        InstA = any(UniqA, HOInstInfoA),
        InstB = any(UniqB, HOInstInfoB),
        % Note that `any' is *not* considered to match `any' unless
        % Info ^ any_matches_any = yes or the type is not a solver type
        % (and does not contain any solver types).
        AnyMatchesAny = !.Info ^ imi_any_matches_any,
        (
            AnyMatchesAny = yes,
            ho_inst_info_matches_final(HOInstInfoA, HOInstInfoB, MaybeType,
                !Info)
        ;
            AnyMatchesAny = no,
            maybe_any_to_bound(MaybeType, !.Info ^ imi_module_info, UniqA,
                HOInstInfoA, NextInstA),
            maybe_any_to_bound(MaybeType, !.Info ^ imi_module_info, UniqB,
                HOInstInfoB, NextInstB),
            inst_matches_binding_mt(NextInstA, NextInstB, MaybeType, !Info)
        )
    ;
        InstA = any(UniqA, HOInstInfoA),
        InstB = ground(_, _),
        maybe_any_to_bound(MaybeType, !.Info ^ imi_module_info, UniqA,
            HOInstInfoA, NextInstA),
        inst_matches_binding_mt(NextInstA, InstB, MaybeType, !Info)
    ;
        InstA = any(UniqA, HOInstInfoA),
        InstB = bound(_, _, _),
        maybe_any_to_bound(MaybeType, !.Info ^ imi_module_info, UniqA,
            HOInstInfoA, NextInstA),
        inst_matches_binding_mt(NextInstA, InstB, MaybeType, !Info)
    ;
        InstA = ground(_, _),
        InstB = any(UniqB, HOInstInfoB),
        maybe_any_to_bound(MaybeType, !.Info ^ imi_module_info, UniqB,
            HOInstInfoB, NextInstB),
        inst_matches_binding_mt(InstA, NextInstB, MaybeType, !Info)
    ;
        InstA = bound(_, _, _),
        InstB = any(UniqB, HOInstInfoB),
        maybe_any_to_bound(MaybeType, !.Info ^ imi_module_info, UniqB,
            HOInstInfoB, NextInstB),
        inst_matches_binding_mt(InstA, NextInstB, MaybeType, !Info)
    ;
        InstA = bound(_UniqA, _InstResultA, BoundInstsA),
        InstB = bound(_UniqB, _InstResultB, BoundInstsB),
        bound_inst_list_matches_binding(BoundInstsA, BoundInstsB, MaybeType,
            !Info)
    ;
        InstA = bound(_UniqA, InstResultsA, BoundInstsA),
        InstB = ground(_UniqB, none),
        inst_results_bound_inst_list_is_ground_mt(InstResultsA, BoundInstsA,
            MaybeType, !.Info ^ imi_module_info)
    ;
        InstA = ground(_UniqA, _),
        InstB = bound(_UniqB, InstResultsB, BoundInstsB),
        inst_results_bound_inst_list_is_ground_mt(InstResultsB, BoundInstsB,
            MaybeType, !.Info ^ imi_module_info),
        (
            MaybeType = yes(Type),
            % We can only do this check if the type is known.
            bound_inst_list_is_complete_for_type(set.init,
                !.Info ^ imi_module_info, BoundInstsB, Type)
        ;
            MaybeType = no,
            fail
        )
    ;
        InstA = ground(_UniqA, HOInstInfoA),
        InstB = ground(_UniqB, HOInstInfoB),
        ho_inst_info_matches_binding(HOInstInfoA, HOInstInfoB, MaybeType,
            !.Info ^ imi_module_info)
    ;
        InstA = abstract_inst(Name, ArgsA),
        InstB = abstract_inst(Name, ArgsB),
        list.duplicate(length(ArgsA), no, MaybeTypes),
        % XXX how do we get the argument types for an abstract inst?
        inst_list_matches_binding(ArgsA, ArgsB, MaybeTypes, !Info)
    ;
        InstA = not_reached
    ).

:- pred ho_inst_info_matches_binding(ho_inst_info::in, ho_inst_info::in,
    maybe(mer_type)::in, module_info::in) is semidet.

ho_inst_info_matches_binding(HOInstInfoA, HOInstInfoB, MaybeType,
        ModuleInfo) :-
    (
        HOInstInfoB = none
    ;
        HOInstInfoA = none,
        HOInstInfoB = higher_order(PredInstB),
        PredInstB = pred_inst_info(pf_function, ArgModes, _, _Det),
        Arity = list.length(ArgModes),
        PredInstA = pred_inst_info_standard_func_mode(Arity),
        pred_inst_matches_mt(PredInstA, PredInstB, MaybeType, ModuleInfo)
    ;
        HOInstInfoA = higher_order(PredInstA),
        HOInstInfoB = higher_order(PredInstB),
        pred_inst_matches_mt(PredInstA, PredInstB, MaybeType, ModuleInfo)
    ).

:- pred inst_list_matches_binding(list(mer_inst)::in, list(mer_inst)::in,
    list(maybe(mer_type))::in, inst_match_info::in, inst_match_info::out)
    is semidet.

inst_list_matches_binding([], [], [], !Info).
inst_list_matches_binding([ArgA | ArgsA], [ArgB | ArgsB],
        [MaybeType | MaybeTypes], !Info) :-
    inst_matches_binding_mt(ArgA, ArgB, MaybeType, !Info),
    inst_list_matches_binding(ArgsA, ArgsB, MaybeTypes, !Info).

    % Here we check that the functors in the first list are a subset of the
    % functors in the second list. (If a bound(...) inst only specifies
    % the insts for some of the constructors of its type, then it implicitly
    % means that all other constructors must have all their arguments
    % `not_reached'.) The code here makes use of the fact that the bound_inst
    % lists are sorted.
    %
:- pred bound_inst_list_matches_binding(list(bound_inst)::in,
    list(bound_inst)::in, maybe(mer_type)::in,
    inst_match_info::in, inst_match_info::out) is semidet.

bound_inst_list_matches_binding([], _, _, !Info).
bound_inst_list_matches_binding([X | Xs], [Y | Ys], MaybeType, !Info) :-
    X = bound_functor(ConsIdX, ArgsX),
    Y = bound_functor(ConsIdY, ArgsY),
    ( equivalent_cons_ids(ConsIdX, ConsIdY) ->
        maybe_get_cons_id_arg_types(!.Info ^ imi_module_info, MaybeType,
            ConsIdX, list.length(ArgsX), MaybeTypes),
        inst_list_matches_binding(ArgsX, ArgsY, MaybeTypes, !Info),
        bound_inst_list_matches_binding(Xs, Ys, MaybeType, !Info)
    ;
        greater_than_disregard_module_qual(ConsIdX, ConsIdY),
        % ConsIdX does not occur in [X | Xs].
        % Hence [X | Xs] implicitly specifies `not_reached' for the args
        % of ConsIdY, and hence automatically matches_binding Y. We just
        % need to check that [X | Xs] matches_binding Ys.
        bound_inst_list_matches_binding([X | Xs], Ys, MaybeType, !Info)
    ).

%-----------------------------------------------------------------------------%

inst_is_clobbered(ModuleInfo, Inst) :-
    require_complete_switch [Inst]
    (
        ( Inst = free
        ; Inst = free(_)
        ; Inst = not_reached
        ; Inst = abstract_inst(_, _)    % XXX is this right?
        ),
        fail
    ;
        ( Inst = any(mostly_clobbered, _)
        ; Inst = any(clobbered, _)
        ; Inst = ground(clobbered, _)
        ; Inst = ground(mostly_clobbered, _)
        ; Inst = bound(clobbered, _, _)
        ; Inst = bound(mostly_clobbered, _, _)
        )
    ;
        Inst = inst_var(_),
        unexpected($module, $pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_clobbered(ModuleInfo, SubInst)
    ;
        Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NextInst),
        inst_is_clobbered(ModuleInfo, NextInst)
    ).

inst_is_free(ModuleInfo, Inst) :-
    require_complete_switch [Inst]
    (
        ( Inst = free
        ; Inst = free(_)
        )
    ;
        ( Inst = ground(_, _)
        ; Inst = bound(_, _, _)
        ; Inst = any(_, _)
        ; Inst = not_reached
        ; Inst = abstract_inst(_, _)    % XXX is this right?
        ),
        fail
    ;
        Inst = inst_var(_),
        unexpected($module, $pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_free(ModuleInfo, SubInst)
    ;
        Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NextInst),
        inst_is_free(ModuleInfo, NextInst)
    ).

inst_is_any(ModuleInfo, Inst) :-
    require_complete_switch [Inst]
    (
        Inst = any(_, _)
    ;
        ( Inst = free
        ; Inst = free(_)
        ; Inst = ground(_, _)
        ; Inst = bound(_, _, _)
        ; Inst = not_reached
        ; Inst = abstract_inst(_, _)    % XXX is this right?
        ),
        fail
    ;
        Inst = inst_var(_),
        unexpected($module, $pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_any(ModuleInfo, SubInst)
    ;
        Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NextInst),
        inst_is_any(ModuleInfo, NextInst)
    ).

inst_is_bound(ModuleInfo, Inst) :-
    require_complete_switch [Inst]
    (
        ( Inst = ground(_, _)
        ; Inst = bound(_, _, _)
        ; Inst = any(_, _)
        ; Inst = abstract_inst(_, _)    % XXX is this right?
        ; Inst = not_reached
        )
    ;
        ( Inst = free
        ; Inst = free(_)
        ),
        fail
    ;
        Inst = inst_var(_),
        unexpected($module, $pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_bound(ModuleInfo, SubInst)
    ;
        Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NextInst),
        inst_is_bound(ModuleInfo, NextInst)
    ).

inst_is_bound_to_functors(ModuleInfo, Inst, Functors) :-
    % inst_is_bound_to_functors succeeds iff the inst passed is
    % `bound(_Uniq, Functors)' or is a user-defined inst which expands to
    % `bound(_Uniq, Functors)'.
    %
    require_complete_switch [Inst]
    (
        Inst = bound(_Uniq, _InstResult, Functors)
    ;
        Inst = inst_var(_),
        unexpected($module, $pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_bound_to_functors(ModuleInfo, SubInst, Functors)
    ;
        Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NextInst),
        inst_is_bound_to_functors(ModuleInfo, NextInst, Functors)
    ;
        ( Inst = free
        ; Inst = free(_)
        ; Inst = any(_, _)
        ; Inst = ground(_, _)
        ; Inst = abstract_inst(_, _)
        ; Inst = not_reached
        ),
        fail
    ).

%-----------------------------------------------------------------------------%

inst_is_ground(ModuleInfo, Inst) :-
    % inst_is_ground succeeds iff the inst passed is `ground' or the
    % equivalent. Abstract insts are not considered ground.
    %
    promise_pure (
        semipure lookup_inst_is_ground(Inst, Found, OldIsGround),
        (
            Found = yes,
            trace [compiletime(flag("inst-is-ground-perf")), io(!IO)] (
                io.write_string("inst_is_ground hit\n", !IO)
            ),
            % Succeed if OldIsGround = yes, fail if OldIsGround = no.
            OldIsGround = yes
        ;
            Found = no,
            trace [compiletime(flag("inst-is-ground-perf")), io(!IO)] (
                io.write_string("inst_is_ground miss\n", !IO)
            ),
            ( inst_is_ground_mt(ModuleInfo, no, Inst) ->
                impure record_inst_is_ground(Inst, yes)
                % Succeed.
            ;
                impure record_inst_is_ground(Inst, no),
                fail
            )
        )
    ).

%-----------------------------------------------------------------------------%
%
% The expansion of terms by the superhomogeneous transformation generates code
% that looks like this:
%
%   V1 = [],
%   V2 = e1,
%   V3 = [V2 | V1],
%   V4 = e2,
%   V5 = [V3 | V4]
%
% The insts on those unifications will contain insts from earlier unifications.
% For example, the inst on the unification building V5 will give V5 an inst
% that contains the insts of V3 and V4.
%
% If there are N elements in a list, testing the insts of the N variables
% representing the N cons cells in the list would ordinarily take O(N^2) steps.
% Since N could be very large, this is disastrous.
%
% We avoid quadratic performance by caching the results of recent calls
% to inst_is_ground for insts that are susceptible to this problem.
% This way, the test on the inst of e.g. V5 will find the results of the tests
% on the insts of V3 and V4 already available. This reduces the overall
% complexity of testing the insts of those N variables to O(n).
%
% The downsides of this cache include the costs of the lookups, and
% the fact that it keeps the cached insts alive.
%
% Note that we do not need to record the ModuleInfo argument of inst_is_ground,
% since it is needed only to interpret insts that need access to the mode
% tables. If we get a result for an inst with one ModuleInfo, we should get
% the exact same result with any later ModuleInfo. The conservative nature
% of the Boehm collector means that an inst address recorded in the cache
% will always point to the original inst; the address cannot be reused until
% the cache entry is itself reused.

:- pragma foreign_decl("C",
"
typedef struct {
    MR_Word     iig_inst_addr;
    MR_Word     iig_is_ground;
} InstIsGroundCacheEntry;

#define INST_IS_GROUND_CACHE_SIZE 1307

/*
** Every entry should be implicitly initialized to zeros. Since zero is
** not a valid address for an inst, uninitialized entries cannot be mistaken
** for filled-in entries.
*/

static  InstIsGroundCacheEntry
                inst_is_ground_cache[INST_IS_GROUND_CACHE_SIZE];
").

    % Look up Inst in the cache. If it is there, return Found = yes
    % and set MayOccur. Otherwise, return Found = no.
    %
:- semipure pred lookup_inst_is_ground(mer_inst::in,
    bool::out, bool::out) is det.

:- pragma foreign_proc("C",
    lookup_inst_is_ground(Inst::in, Found::out, IsGround::out),
    [will_not_call_mercury, promise_semipure],
"
    MR_Unsigned hash;

    hash = (MR_Unsigned) Inst;
    hash = hash >> MR_LOW_TAG_BITS;
    hash = hash % INST_IS_GROUND_CACHE_SIZE;

    if (inst_is_ground_cache[hash].iig_inst_addr == Inst) {
        Found = MR_BOOL_YES;
        IsGround = inst_is_ground_cache[hash].iig_is_ground;
    } else {
        Found = MR_BOOL_NO;
    }
").

lookup_inst_is_ground(_, no, no) :-
    semipure semipure_true.

    % Record the result for Inst in the cache.
    %
:- impure pred record_inst_is_ground(mer_inst::in, bool::in) is det.

:- pragma foreign_proc("C",
    record_inst_is_ground(Inst::in, IsGround::in),
    [will_not_call_mercury],
"
    MR_Unsigned hash;

    hash = (MR_Unsigned) Inst;
    hash = hash >> MR_LOW_TAG_BITS;
    hash = hash % INST_IS_GROUND_CACHE_SIZE;
    /* We overwrite any existing entry in the slot. */
    inst_is_ground_cache[hash].iig_inst_addr = Inst;
    inst_is_ground_cache[hash].iig_is_ground = IsGround;
").

record_inst_is_ground(_, _) :-
    impure impure_true.

%-----------------------------------------------------------------------------%

:- pred inst_is_ground_mt(module_info::in, maybe(mer_type)::in, mer_inst::in)
    is semidet.

inst_is_ground_mt(ModuleInfo, MaybeType, Inst) :-
    Expansions0 = set_tree234.init,
    inst_is_ground_mt_1(ModuleInfo, MaybeType, Inst, Expansions0, _Expansions).

    % The third arg is the set of insts which have already been expanded;
    % we use this to avoid going into an infinite loop.
    %
:- pred inst_is_ground_mt_1(module_info::in, maybe(mer_type)::in, mer_inst::in,
    set_tree234(mer_inst)::in, set_tree234(mer_inst)::out) is semidet.

inst_is_ground_mt_1(ModuleInfo, MaybeType, Inst, !Expansions) :-
    % XXX This special casing of any/2 was introduced in version 1.65
    % of this file. The log message for that version gives a reason why
    % this special casing is required, but I (zs) don't believe it,
    % at least not without more explanation.
    ( Inst = any(_, _) ->
        ( set_tree234.contains(!.Expansions, Inst) ->
            true
        ;
            inst_is_ground_mt_2(ModuleInfo, MaybeType, Inst, !Expansions)
        )
    ;
        % ZZZ make this work on Inst's *address*.
        ( set_tree234.insert_new(Inst, !Expansions) ->
            % Inst was not yet in Expansions, but we have now inserted it.
            inst_is_ground_mt_2(ModuleInfo, MaybeType, Inst, !Expansions)
        ;
            % Inst was already in !.Expansions.
            true
        )
    ).

:- pred inst_is_ground_mt_2(module_info::in, maybe(mer_type)::in, mer_inst::in,
    set_tree234(mer_inst)::in, set_tree234(mer_inst)::out) is semidet.

inst_is_ground_mt_2(ModuleInfo, MaybeType, Inst, !Expansions) :-
    require_complete_switch [Inst]
    (
        ( Inst = free
        ; Inst = free(_)
        ),
        fail
    ;
        ( Inst = not_reached
        ; Inst = ground(_, _)
        )
    ;
        Inst = bound(_, InstResults, BoundInsts),
        inst_results_bound_inst_list_is_ground_mt_2(InstResults, BoundInsts,
            MaybeType, ModuleInfo, !Expansions)
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_ground_mt_1(ModuleInfo, MaybeType, SubInst, !Expansions)
    ;
        Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NextInst),
        inst_is_ground_mt_1(ModuleInfo, MaybeType, NextInst, !Expansions)
    ;
        Inst = any(Uniq, HOInstInfo),
        maybe_any_to_bound(MaybeType, ModuleInfo, Uniq, HOInstInfo, NextInst),
        inst_is_ground_mt_1(ModuleInfo, MaybeType, NextInst, !Expansions)
    ;
        Inst = inst_var(_),
        unexpected($module, $pred, "uninstantiated inst parameter")
    ;
        Inst = abstract_inst(_, _),
        % XXX I (zs) am not sure this is the right thing to do here.
        % The original code of this predicate simply did not consider
        % this kind of Inst.
        fail
    ).

inst_is_ground_or_any(ModuleInfo, Inst) :-
    set.init(Expansions0),
    inst_is_ground_or_any_2(ModuleInfo, Inst, Expansions0, _Expansions).

    % The third arg is the set of insts which have already been expanded;
    % we use this to avoid going into an infinite loop.
    %
:- pred inst_is_ground_or_any_2(module_info::in, mer_inst::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_is_ground_or_any_2(ModuleInfo, Inst, !Expansions) :-
    require_complete_switch [Inst]
    (
        ( Inst = ground(_, _)
        ; Inst = any(_, _)
        ; Inst = not_reached
        )
    ;
        Inst = bound(_, InstResults, BoundInsts),
        inst_results_bound_inst_list_is_ground_or_any_2(InstResults,
            BoundInsts, ModuleInfo, !Expansions)
    ;
        ( Inst = free
        ; Inst = free(_)
        ; Inst = abstract_inst(_, _)   % XXX is this right?
        ),
        fail
    ;
        Inst = inst_var(_),
        unexpected($module, $pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_ground_or_any_2(ModuleInfo, SubInst, !Expansions)
    ;
        Inst = defined_inst(InstName),
        ( set.insert_new(Inst, !Expansions) ->
            inst_lookup(ModuleInfo, InstName, NextInst),
            inst_is_ground_or_any_2(ModuleInfo, NextInst, !Expansions)
        ;
            true
        )
    ).

inst_is_unique(ModuleInfo, Inst) :-
    set.init(Expansions0),
    inst_is_unique_2(ModuleInfo, Inst, Expansions0, _Expansions).

    % The third arg is the set of insts which have already been expanded;
    % we use this to avoid going into an infinite loop.
    %
:- pred inst_is_unique_2(module_info::in, mer_inst::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_is_unique_2(ModuleInfo, Inst, !Expansions) :-
    (
        ( Inst = ground(unique, _)
        ; Inst = any(unique, _)
        ; Inst = not_reached
        ; Inst = free               % XXX I don't think this is right [zs].
        )
    ;
        ( Inst = ground(shared, _)
        ; Inst = bound(shared, _, _)
        ; Inst = any(shared, _)
        ),
        fail
    ;
        Inst = bound(unique, InstResults, BoundInsts),
        (
            InstResults = inst_test_results_fgtc,
            fail
        ;
            ( InstResults = inst_test_no_results
            ; InstResults = inst_test_results(_, _, _, _)
            ),
            bound_inst_list_is_unique_2(BoundInsts, ModuleInfo, !Expansions)
        )
    ;
        Inst = inst_var(_),
        unexpected($module, $pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_unique_2(ModuleInfo, SubInst, !Expansions)
    ;
        Inst = defined_inst(InstName),
        ( set.insert_new(Inst, !Expansions) ->
            inst_lookup(ModuleInfo, InstName, NextInst),
            inst_is_unique_2(ModuleInfo, NextInst, !Expansions)
        ;
            true
        )
    ).

inst_is_mostly_unique(ModuleInfo, Inst) :-
    set.init(Expansions0),
    inst_is_mostly_unique_2(ModuleInfo, Inst, Expansions0, _Expansions).

    % The third arg is the set of insts which have already been expanded;
    % we use this to avoid going into an infinite loop.
    %
:- pred inst_is_mostly_unique_2(module_info::in, mer_inst::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_is_mostly_unique_2(ModuleInfo, Inst, !Expansions) :-
    require_complete_switch [Inst]
    (
        ( Inst = not_reached
        ; Inst = free
        ; Inst = free(_)
        ; Inst = ground(unique, _)
        ; Inst = ground(mostly_unique, _)
        ; Inst = any(unique, _)
        ; Inst = any(mostly_unique, _)
        )
    ;
        Inst = bound(unique, InstResults, BoundInsts),
        (
            InstResults = inst_test_results_fgtc,
            fail
        ;
            ( InstResults = inst_test_no_results
            ; InstResults = inst_test_results(_, _, _, _)
            ),
            bound_inst_list_is_mostly_unique_2(BoundInsts, ModuleInfo,
                !Expansions)
        )
    ;
        Inst = inst_var(_),
        unexpected($module, $pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_mostly_unique_2(ModuleInfo, SubInst, !Expansions)
    ;
        Inst = defined_inst(InstName),
        ( set.insert_new(Inst, !Expansions) ->
            inst_lookup(ModuleInfo, InstName, NextInst),
            inst_is_mostly_unique_2(ModuleInfo, NextInst, !Expansions)
        ;
            true
        )
    ;
        Inst = abstract_inst(_, _),
        % XXX I (zs) am not sure this is the right thing to do here.
        % The original code of this predicate simply did not consider
        % this kind of Inst.
        fail
    ).

    % inst_is_not_partly_unique succeeds iff the inst passed is not unique
    % or mostly_unique, i.e. if it is shared free. It fails for abstract insts.
    %
inst_is_not_partly_unique(ModuleInfo, Inst) :-
    set.init(Expansions0),
    inst_is_not_partly_unique_2(ModuleInfo, Inst, Expansions0, _Expansions).

    % The third arg is the set of insts which have already been expanded;
    % we use this to avoid going into an infinite loop.
    %
:- pred inst_is_not_partly_unique_2(module_info::in, mer_inst::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_is_not_partly_unique_2(ModuleInfo, Inst, !Expansions) :-
    require_complete_switch [Inst]
    (
        ( Inst = not_reached
        ; Inst = free
        ; Inst = free(_)
        ; Inst = any(shared, _)
        ; Inst = ground(shared, _)
        )
    ;
        Inst = bound(shared, InstResult, BoundInsts),
        (
            InstResult = inst_test_results_fgtc
        ;
            ( InstResult = inst_test_no_results
            ; InstResult = inst_test_results(_, _, _, _)
            ),
            bound_inst_list_is_not_partly_unique_2(BoundInsts, ModuleInfo,
                !Expansions)
        )
    ;
        Inst = inst_var(_),
        unexpected($module, $pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_not_partly_unique_2(ModuleInfo, SubInst, !Expansions)
    ;
        Inst = defined_inst(InstName),
        ( set.insert_new(Inst, !Expansions) ->
            inst_lookup(ModuleInfo, InstName, NextInst),
            inst_is_not_partly_unique_2(ModuleInfo, NextInst, !Expansions)
        ;
            true
        )
    ;
        Inst = abstract_inst(_, _),
        % XXX I (zs) am not sure this is the right thing to do here.
        % The original code of this predicate simply did not consider
        % this kind of Inst.
        fail
    ).

    % inst_is_not_fully_unique succeeds iff the inst passed is not unique,
    % i.e. if it is mostly_unique, shared, or free. It fails for abstract
    % insts.
    %
inst_is_not_fully_unique(ModuleInfo, Inst) :-
    set.init(Expansions0),
    inst_is_not_fully_unique_2(ModuleInfo, Inst, Expansions0, _Expansions).

    % The third arg is the set of insts which have already been expanded - we
    % use this to avoid going into an infinite loop.
    %
:- pred inst_is_not_fully_unique_2(module_info::in, mer_inst::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_is_not_fully_unique_2(ModuleInfo, Inst, !Expansions) :-
    require_complete_switch [Inst]
    (
        ( Inst = not_reached
        ; Inst = free
        ; Inst = free(_)
        ; Inst = ground(shared, _)
        ; Inst = ground(mostly_unique, _)
        ; Inst = any(shared, _)
        ; Inst = any(mostly_unique, _)
        )
    ;
        Inst = bound(Uniq, InstResult, BoundInsts),
        ( Uniq = shared
        ; Uniq = mostly_unique
        ),
        (
            InstResult = inst_test_results_fgtc
        ;
            ( InstResult = inst_test_no_results
            ; InstResult = inst_test_results(_, _, _, _)
            ),
            bound_inst_list_is_not_fully_unique_2(BoundInsts, ModuleInfo,
                !Expansions)
        )
    ;
        Inst = inst_var(_),
        unexpected($module, $pred, "uninstantiated inst parameter")
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_is_not_fully_unique_2(ModuleInfo, SubInst, !Expansions)
    ;
        Inst = defined_inst(InstName),
        ( set.insert_new(Inst, !Expansions) ->
            inst_lookup(ModuleInfo, InstName, NextInst),
            inst_is_not_fully_unique_2(ModuleInfo, NextInst, !Expansions)
        ;
            true
        )
    ;
        Inst = abstract_inst(_, _),
        % XXX I (zs) am not sure this is the right thing to do here.
        % The original code of this predicate simply did not consider
        % this kind of Inst.
        fail
    ).

%-----------------------------------------------------------------------------%

bound_inst_list_is_ground(BoundInsts, ModuleInfo) :-
    bound_inst_list_is_ground_mt(BoundInsts, no, ModuleInfo).

:- pred inst_results_bound_inst_list_is_ground_mt(inst_test_results::in,
    list(bound_inst)::in, maybe(mer_type)::in, module_info::in) is semidet.

inst_results_bound_inst_list_is_ground_mt(InstResults, BoundInsts,
        MaybeType, ModuleInfo) :-
    require_complete_switch [InstResults]
    (
        InstResults = inst_test_results_fgtc
    ;
        InstResults = inst_test_results(GroundnessResult, _, _, _),
        require_complete_switch [GroundnessResult]
        (
            GroundnessResult = inst_result_is_ground
        ;
            GroundnessResult = inst_result_is_not_ground,
            fail
        ;
            GroundnessResult = inst_result_groundness_unknown,
            bound_inst_list_is_ground_mt(BoundInsts, MaybeType, ModuleInfo)
        )
    ;
        InstResults = inst_test_no_results,
        bound_inst_list_is_ground_mt(BoundInsts, MaybeType, ModuleInfo)
    ).

:- pred bound_inst_list_is_ground_mt(list(bound_inst)::in, maybe(mer_type)::in,
    module_info::in) is semidet.

bound_inst_list_is_ground_mt([], _, _).
bound_inst_list_is_ground_mt([BoundInst | BoundInsts], MaybeType,
        ModuleInfo) :-
    BoundInst = bound_functor(Name, Args),
    maybe_get_cons_id_arg_types(ModuleInfo, MaybeType, Name,
        list.length(Args), MaybeTypes),
    inst_list_is_ground_mt(Args, MaybeTypes, ModuleInfo),
    bound_inst_list_is_ground_mt(BoundInsts, MaybeType, ModuleInfo).

:- pred inst_results_bound_inst_list_is_ground_or_any(inst_test_results::in,
    list(bound_inst)::in, module_info::in) is semidet.

inst_results_bound_inst_list_is_ground_or_any(InstResults, BoundInsts,
        ModuleInfo) :-
    require_complete_switch [InstResults]
    (
        InstResults = inst_test_results_fgtc
    ;
        InstResults = inst_test_results(GroundnessResult, _, _, _),
        require_complete_switch [GroundnessResult]
        (
            GroundnessResult = inst_result_is_ground
        ;
            GroundnessResult = inst_result_is_not_ground,
            fail
        ;
            GroundnessResult = inst_result_groundness_unknown,
            bound_inst_list_is_ground_or_any(BoundInsts, ModuleInfo)
        )
    ;
        InstResults = inst_test_no_results,
        bound_inst_list_is_ground_or_any(BoundInsts, ModuleInfo)
    ).

bound_inst_list_is_ground_or_any([], _).
bound_inst_list_is_ground_or_any([BoundInst | BoundInsts], ModuleInfo) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_ground_or_any(Args, ModuleInfo),
    bound_inst_list_is_ground_or_any(BoundInsts, ModuleInfo).

bound_inst_list_is_unique([], _).
bound_inst_list_is_unique([BoundInst | BoundInsts], ModuleInfo) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_unique(Args, ModuleInfo),
    bound_inst_list_is_unique(BoundInsts, ModuleInfo).

bound_inst_list_is_mostly_unique([], _).
bound_inst_list_is_mostly_unique([BoundInst | BoundInsts], ModuleInfo) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_mostly_unique(Args, ModuleInfo),
    bound_inst_list_is_mostly_unique(BoundInsts, ModuleInfo).

bound_inst_list_is_not_partly_unique([], _).
bound_inst_list_is_not_partly_unique([BoundInst | BoundInsts], ModuleInfo) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_not_partly_unique(Args, ModuleInfo),
    bound_inst_list_is_not_partly_unique(BoundInsts, ModuleInfo).

bound_inst_list_is_not_fully_unique([], _).
bound_inst_list_is_not_fully_unique([BoundInst | BoundInsts], ModuleInfo) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_not_fully_unique(Args, ModuleInfo),
    bound_inst_list_is_not_fully_unique(BoundInsts, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred inst_results_bound_inst_list_is_ground_mt_2(inst_test_results::in,
    list(bound_inst)::in, maybe(mer_type)::in, module_info::in,
    set_tree234(mer_inst)::in, set_tree234(mer_inst)::out) is semidet.

inst_results_bound_inst_list_is_ground_mt_2(InstResults, BoundInsts,
        MaybeType, ModuleInfo, !Expansions) :-
    require_complete_switch [InstResults]
    (
        InstResults = inst_test_results_fgtc
    ;
        InstResults = inst_test_results(GroundnessResult, _, _, _),
        require_complete_switch [GroundnessResult]
        (
            GroundnessResult = inst_result_is_ground
        ;
            GroundnessResult = inst_result_is_not_ground,
            fail
        ;
            GroundnessResult = inst_result_groundness_unknown,
            bound_inst_list_is_ground_mt_2(BoundInsts, MaybeType, ModuleInfo,
                !Expansions)
        )
    ;
        InstResults = inst_test_no_results,
        bound_inst_list_is_ground_mt_2(BoundInsts, MaybeType, ModuleInfo,
            !Expansions)
    ).

:- pred bound_inst_list_is_ground_mt_2(list(bound_inst)::in,
    maybe(mer_type)::in, module_info::in,
    set_tree234(mer_inst)::in, set_tree234(mer_inst)::out) is semidet.

bound_inst_list_is_ground_mt_2([], _, _, !Expansions).
bound_inst_list_is_ground_mt_2([BoundInst | BoundInsts], MaybeType, ModuleInfo,
        !Expansions) :-
    BoundInst = bound_functor(Name, Args),
    maybe_get_cons_id_arg_types(ModuleInfo, MaybeType, Name,
        list.length(Args), MaybeTypes),
    inst_list_is_ground_mt_2(Args, MaybeTypes, ModuleInfo, !Expansions),
    bound_inst_list_is_ground_mt_2(BoundInsts, MaybeType, ModuleInfo,
        !Expansions).

:- pred inst_results_bound_inst_list_is_ground_or_any_2(inst_test_results::in,
    list(bound_inst)::in, module_info::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_results_bound_inst_list_is_ground_or_any_2(InstResults, BoundInsts,
        ModuleInfo, !Expansions) :-
    require_complete_switch [InstResults]
    (
        InstResults = inst_test_results_fgtc
    ;
        InstResults = inst_test_results(GroundnessResult, _, _, _),
        require_complete_switch [GroundnessResult]
        (
            GroundnessResult = inst_result_is_ground
        ;
            GroundnessResult = inst_result_is_not_ground,
            fail
        ;
            GroundnessResult = inst_result_groundness_unknown,
            bound_inst_list_is_ground_or_any_2(BoundInsts, ModuleInfo,
                !Expansions)
        )
    ;
        InstResults = inst_test_no_results,
        bound_inst_list_is_ground_or_any_2(BoundInsts, ModuleInfo, !Expansions)
    ).

:- pred bound_inst_list_is_ground_or_any_2(list(bound_inst)::in,
    module_info::in, set(mer_inst)::in, set(mer_inst)::out) is semidet.

bound_inst_list_is_ground_or_any_2([], _, !Expansions).
bound_inst_list_is_ground_or_any_2([BoundInst | BoundInsts], ModuleInfo,
        !Expansions) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_ground_or_any_2(Args, ModuleInfo, !Expansions),
    bound_inst_list_is_ground_or_any_2(BoundInsts, ModuleInfo, !Expansions).

:- pred bound_inst_list_is_unique_2(list(bound_inst)::in, module_info::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

bound_inst_list_is_unique_2([], _, !Expansions).
bound_inst_list_is_unique_2([BoundInst | BoundInsts], ModuleInfo,
        !Expansions) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_unique_2(Args, ModuleInfo, !Expansions),
    bound_inst_list_is_unique_2(BoundInsts, ModuleInfo, !Expansions).

:- pred bound_inst_list_is_mostly_unique_2(list(bound_inst)::in,
    module_info::in, set(mer_inst)::in, set(mer_inst)::out) is semidet.

bound_inst_list_is_mostly_unique_2([], _, !Expansions).
bound_inst_list_is_mostly_unique_2([BoundInst | BoundInsts], ModuleInfo,
        !Expansions) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_mostly_unique_2(Args, ModuleInfo, !Expansions),
    bound_inst_list_is_mostly_unique_2(BoundInsts, ModuleInfo, !Expansions).

:- pred bound_inst_list_is_not_partly_unique_2(list(bound_inst)::in,
    module_info::in, set(mer_inst)::in, set(mer_inst)::out) is semidet.

bound_inst_list_is_not_partly_unique_2([], _, !Expansions).
bound_inst_list_is_not_partly_unique_2([BoundInst | BoundInsts], ModuleInfo,
        !Expansions) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_not_partly_unique_2(Args, ModuleInfo, !Expansions),
    bound_inst_list_is_not_partly_unique_2(BoundInsts, ModuleInfo,
        !Expansions).

:- pred bound_inst_list_is_not_fully_unique_2(list(bound_inst)::in,
    module_info::in, set(mer_inst)::in, set(mer_inst)::out) is semidet.

bound_inst_list_is_not_fully_unique_2([], _, !Expansions).
bound_inst_list_is_not_fully_unique_2([BoundInst | BoundInsts], ModuleInfo,
        !Expansions) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_not_fully_unique_2(Args, ModuleInfo, !Expansions),
    bound_inst_list_is_not_fully_unique_2(BoundInsts, ModuleInfo,
        !Expansions).

%-----------------------------------------------------------------------------%

:- pred inst_list_is_ground_mt(list(mer_inst)::in, list(maybe(mer_type))::in,
    module_info::in) is semidet.

inst_list_is_ground_mt([], [], _).
inst_list_is_ground_mt([Inst | Insts], [MaybeType | MaybeTypes], ModuleInfo) :-
    inst_is_ground_mt(ModuleInfo, MaybeType, Inst),
    inst_list_is_ground_mt(Insts, MaybeTypes, ModuleInfo).

inst_list_is_ground([], _).
inst_list_is_ground([Inst | Insts], ModuleInfo) :-
    inst_is_ground(ModuleInfo, Inst),
    inst_list_is_ground(Insts, ModuleInfo).

inst_list_is_ground_or_any([], _).
inst_list_is_ground_or_any([Inst | Insts], ModuleInfo) :-
    inst_is_ground_or_any(ModuleInfo, Inst),
    inst_list_is_ground_or_any(Insts, ModuleInfo).

inst_list_is_unique([], _).
inst_list_is_unique([Inst | Insts], ModuleInfo) :-
    inst_is_unique(ModuleInfo, Inst),
    inst_list_is_unique(Insts, ModuleInfo).

inst_list_is_mostly_unique([], _).
inst_list_is_mostly_unique([Inst | Insts], ModuleInfo) :-
    inst_is_mostly_unique(ModuleInfo, Inst),
    inst_list_is_mostly_unique(Insts, ModuleInfo).

inst_list_is_not_partly_unique([], _).
inst_list_is_not_partly_unique([Inst | Insts], ModuleInfo) :-
    inst_is_not_partly_unique(ModuleInfo, Inst),
    inst_list_is_not_partly_unique(Insts, ModuleInfo).

inst_list_is_not_fully_unique([], _).
inst_list_is_not_fully_unique([Inst | Insts], ModuleInfo) :-
    inst_is_not_fully_unique(ModuleInfo, Inst),
    inst_list_is_not_fully_unique(Insts, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred inst_list_is_ground_mt_2(list(mer_inst)::in, list(maybe(mer_type))::in,
    module_info::in, set_tree234(mer_inst)::in, set_tree234(mer_inst)::out)
    is semidet.

inst_list_is_ground_mt_2([], [], _, !Expansions).
inst_list_is_ground_mt_2([], [_ | _], _, !Expansions) :-
    unexpected($module, $pred, "length mismatch").
inst_list_is_ground_mt_2([_ | _], [], _, !Expansions) :-
    unexpected($module, $pred, "length mismatch").
inst_list_is_ground_mt_2([Inst | Insts], [MaybeType | MaybeTypes], ModuleInfo,
        !Expansions) :-
    inst_is_ground_mt_1(ModuleInfo, MaybeType, Inst, !Expansions),
    inst_list_is_ground_mt_2(Insts, MaybeTypes, ModuleInfo, !Expansions).

:- pred inst_list_is_ground_or_any_2(list(mer_inst)::in, module_info::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_list_is_ground_or_any_2([], _, !Expansions).
inst_list_is_ground_or_any_2([Inst | Insts], ModuleInfo, !Expansions) :-
    inst_is_ground_or_any_2(ModuleInfo, Inst, !Expansions),
    inst_list_is_ground_or_any_2(Insts, ModuleInfo, !Expansions).

:- pred inst_list_is_unique_2(list(mer_inst)::in, module_info::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_list_is_unique_2([], _, !Expansions).
inst_list_is_unique_2([Inst | Insts], ModuleInfo, !Expansions) :-
    inst_is_unique_2(ModuleInfo, Inst, !Expansions),
    inst_list_is_unique_2(Insts, ModuleInfo, !Expansions).

:- pred inst_list_is_mostly_unique_2(list(mer_inst)::in, module_info::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_list_is_mostly_unique_2([], _, !Expansions).
inst_list_is_mostly_unique_2([Inst | Insts], ModuleInfo, !Expansions) :-
    inst_is_mostly_unique_2(ModuleInfo, Inst, !Expansions),
    inst_list_is_mostly_unique_2(Insts, ModuleInfo, !Expansions).

:- pred inst_list_is_not_partly_unique_2(list(mer_inst)::in, module_info::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_list_is_not_partly_unique_2([], _, !Expansions).
inst_list_is_not_partly_unique_2([Inst | Insts], ModuleInfo, !Expansions) :-
    inst_is_not_partly_unique_2(ModuleInfo, Inst, !Expansions),
    inst_list_is_not_partly_unique_2(Insts, ModuleInfo, !Expansions).

:- pred inst_list_is_not_fully_unique_2(list(mer_inst)::in, module_info::in,
    set(mer_inst)::in, set(mer_inst)::out) is semidet.

inst_list_is_not_fully_unique_2([], _, !Expansions).
inst_list_is_not_fully_unique_2([Inst | Insts], ModuleInfo, !Expansions) :-
    inst_is_not_fully_unique_2(ModuleInfo, Inst, !Expansions),
    inst_list_is_not_fully_unique_2(Insts, ModuleInfo, !Expansions).

%-----------------------------------------------------------------------------%

bound_inst_list_is_free([], _).
bound_inst_list_is_free([BoundInst | BoundInsts], ModuleInfo) :-
    BoundInst = bound_functor(_Name, Args),
    inst_list_is_free(Args, ModuleInfo),
    bound_inst_list_is_free(BoundInsts, ModuleInfo).

inst_list_is_free([], _).
inst_list_is_free([Inst | Insts], ModuleInfo) :-
    inst_is_free(ModuleInfo, Inst),
    inst_list_is_free(Insts, ModuleInfo).

%-----------------------------------------------------------------------------%

inst_list_is_ground_or_dead([], [], _).
inst_list_is_ground_or_dead([Inst | Insts], [Live | Lives], ModuleInfo) :-
    (
        Live = is_live,
        inst_is_ground(ModuleInfo, Inst)
    ;
        Live = is_dead
    ),
    inst_list_is_ground_or_dead(Insts, Lives, ModuleInfo).

inst_list_is_ground_or_any_or_dead([], [], _).
inst_list_is_ground_or_any_or_dead([Inst | Insts], [Live | Lives],
        ModuleInfo) :-
    (
        Live = is_live,
        inst_is_ground_or_any(ModuleInfo, Inst)
    ;
        Live = is_dead
    ),
    inst_list_is_ground_or_any_or_dead(Insts, Lives, ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

inst_contains_instname(Inst, ModuleInfo, InstName) :-
    set.init(Expansions0),
    inst_contains_instname_2(Inst, ModuleInfo, InstName, yes,
        Expansions0, _Expansions).

:- type inst_names == set(inst_name).

:- pred inst_contains_instname_2(mer_inst::in, module_info::in, inst_name::in,
    bool::out, inst_names::in, inst_names::out) is det.

inst_contains_instname_2(Inst, ModuleInfo, InstName, Contains, !Expansions) :-
    (
        ( Inst = abstract_inst(_, _)
        ; Inst = any(_, _)
        ; Inst = free
        ; Inst = free(_)
        ; Inst = ground(_, _)
        ; Inst = inst_var(_)
        ; Inst = not_reached
        ),
        Contains = no
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_contains_instname_2(SubInst, ModuleInfo, InstName, Contains,
            !Expansions)
    ;
        Inst = defined_inst(ThisInstName),
        ( InstName = ThisInstName ->
            Contains = yes
        ;
            ( set.insert_new(ThisInstName, !Expansions) ->
                inst_lookup(ModuleInfo, ThisInstName, ThisInst),
                set.insert(ThisInstName, !Expansions),
                inst_contains_instname_2(ThisInst, ModuleInfo, InstName,
                    Contains, !Expansions)
            ;
                Contains = no
            )
        )
    ;
        Inst = bound(_Uniq, InstResults, ArgInsts),
        % XXX This code has a performance problem.
        %
        % The problem is that e.g. in a list of length N, you will have N
        % variables for the skeletons whose insts contain an average of N/2
        % occurences of `bound' each, so the complexity of running
        % inst_contains_instname_2 on all their insts is quadratic in N.
        %
        % One solution to this would be to add an extra argument to bound/2
        % that gives the set of included inst_names, or simply asserts that
        % this set is empty. This field can be set at the time of the
        % construction of the inst, avoiding quadratic behavior in
        % inst_contains_instname_2. The complexity of constructing may be
        % quadratic in N, of course.
        %
        % We could try to solve this performance problem with a cache
        % of the results of recent invocations of inst_contains_instname.

        (
            InstResults = inst_test_results_fgtc,
            Contains = no
        ;
            InstResults = inst_test_results(_, _, InstNamesResult, _),
            (
                InstNamesResult =
                    inst_result_contains_instnames_known(InstNameSet),
                ( set.contains(InstNameSet, InstName) ->
                    % The Inst may contain InstName, and probably does,
                    % but verify it.
                    bound_inst_list_contains_instname(ArgInsts, ModuleInfo,
                        InstName, Contains, !Expansions)
                ;
                    Contains = no
                )
            ;
                InstNamesResult = inst_result_contains_instnames_unknown,
                bound_inst_list_contains_instname(ArgInsts, ModuleInfo,
                    InstName, Contains, !Expansions)
            )
        ;
            InstResults = inst_test_no_results,
            bound_inst_list_contains_instname(ArgInsts, ModuleInfo,
                InstName, Contains, !Expansions)
        )
    ).

:- pred bound_inst_list_contains_instname(list(bound_inst)::in,
    module_info::in, inst_name::in, bool::out,
    inst_names::in, inst_names::out) is det.

bound_inst_list_contains_instname([], _ModuleInfo, _InstName, no, !Expansions).
bound_inst_list_contains_instname([BoundInst | BoundInsts], ModuleInfo,
        InstName, Contains, !Expansions) :-
    BoundInst = bound_functor(_Functor, ArgInsts),
    inst_list_contains_instname(ArgInsts, ModuleInfo, InstName, Contains1,
        !Expansions),
    (
        Contains1 = yes,
        Contains = yes
    ;
        Contains1 = no,
        bound_inst_list_contains_instname(BoundInsts, ModuleInfo,
            InstName, Contains, !Expansions)
    ).

:- pred inst_list_contains_instname(list(mer_inst)::in, module_info::in,
    inst_name::in, bool::out, inst_names::in, inst_names::out) is det.

inst_list_contains_instname([], _ModuleInfo, _InstName, no, !Expansions).
inst_list_contains_instname([Inst | Insts], ModuleInfo, InstName, Contains,
        !Expansions) :-
    inst_contains_instname_2(Inst, ModuleInfo, InstName, Contains1,
        !Expansions),
    (
        Contains1 = yes,
        Contains = yes
    ;
        Contains1 = no,
        inst_list_contains_instname(Insts, ModuleInfo, InstName, Contains,
            !Expansions)
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
        InstName = unify_inst(_Live, InstA, InstB, _Real),
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
            ( InstResults = inst_test_no_results
            ; InstResults = inst_test_results(_, _, _, _)
            ),
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

mode_list_contains_inst_var(Modes, _ModuleInfo, InstVar) :-
    mode_list_contains_inst_var(Modes, InstVar).

:- pred mode_list_contains_inst_var(list(mer_mode)::in, inst_var::out)
    is nondet.

mode_list_contains_inst_var([Mode | Modes], InstVar) :-
    (
        mode_contains_inst_var(Mode, InstVar)
    ;
        mode_list_contains_inst_var(Modes, InstVar)
    ).

:- pred mode_contains_inst_var(mer_mode::in, inst_var::out) is nondet.

mode_contains_inst_var(Mode, InstVar) :-
    (
        Mode = (Initial -> Final),
        ( Inst = Initial ; Inst = Final )
    ;
        Mode = user_defined_mode(_Name, Insts),
        list.member(Inst, Insts)
    ),
    inst_contains_inst_var(Inst, InstVar).

%-----------------------------------------------------------------------------%

    % For a non-solver type t (i.e. any type declared without using the
    % `solver' keyword), the inst `any' should be considered to be equivalent
    % to a bound inst i where i contains all the functors of the type t and
    % each argument has inst `any'.
    %
    % Note that pred and func types are considered solver types, since
    % higher-order terms that contain non-local solver variables are
    % themselves not ground -- they only become ground when all non-locals do.
    %
:- pred maybe_any_to_bound(maybe(mer_type)::in, module_info::in,
    uniqueness::in, ho_inst_info::in, mer_inst::out) is semidet.

maybe_any_to_bound(yes(Type), ModuleInfo, Uniq, none, Inst) :-
    \+ type_is_solver_type(ModuleInfo, Type),
    ( type_constructors(ModuleInfo, Type, Constructors) ->
        type_to_ctor_det(Type, TypeCtor),
        constructors_to_bound_any_insts(ModuleInfo, Uniq, TypeCtor,
            Constructors, BoundInsts0),
        list.sort_and_remove_dups(BoundInsts0, BoundInsts),
        % If all the constructors are constant, then Inst will be ground
        % and will not contain any.
        InstResult = inst_test_results(
            inst_result_groundness_unknown,
            inst_result_contains_any_unknown,
            inst_result_contains_instnames_known(set.init),
            inst_result_contains_types_known(set.init)
        ),
        Inst = bound(Uniq, InstResult, BoundInsts)
    ; type_may_contain_solver_type(ModuleInfo, Type) ->
        % For a type for which constructors are not available (e.g. an
        % abstract type) and which may contain solver types, we fail, meaning
        % that we will use `any' for this type.
        fail
    ;
        Inst = ground(Uniq, none)
    ).

:- pred type_may_contain_solver_type(module_info::in, mer_type::in) is semidet.

type_may_contain_solver_type(ModuleInfo, Type) :-
    TypeCtorCat = classify_type(ModuleInfo, Type),
    type_may_contain_solver_type_2(TypeCtorCat) = yes.

:- func type_may_contain_solver_type_2(type_ctor_category) = bool.

type_may_contain_solver_type_2(CtorCat) = MayContainSolverType :-
    (
        ( CtorCat = ctor_cat_builtin(_)
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_user(cat_user_direct_dummy)
        ),
        MayContainSolverType = no
    ;
        ( CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_user(cat_user_notag)
        ; CtorCat = ctor_cat_user(cat_user_general)
        ),
        MayContainSolverType = yes
    ).

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
