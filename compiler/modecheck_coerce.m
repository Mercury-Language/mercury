%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2021, 2024-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: modecheck_coerce.m.
% Main author: wangp.
%
% This file contains the code to modecheck a coerce expression.
%
%---------------------------------------------------------------------------%

:- module check_hlds.modecheck_coerce.
:- interface.

:- import_module check_hlds.mode_info.
:- import_module check_hlds.modecheck_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

:- pred modecheck_coerce(list(prog_var)::in, list(prog_var)::out,
    list(mer_mode)::in, list(mer_mode)::out, determinism::out,
    extra_goals::out, mode_info::in, mode_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_abstract_unify.
:- import_module check_hlds.inst_lookup.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.type_util.
:- import_module hlds.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.var_table_hlds.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

:- type modecheck_coerce_result
    --->    coerce_mode_ok(
                list(prog_var),
                list(mer_mode),
                extra_goals
            )
    ;       coerce_mode_error.

:- type rev_term_path == list(coerce_error_term_path_step).

:- type maybe_coerce_error(T) == maybe1(T, list(coerce_error)).

%---------------------------------------------------------------------------%

modecheck_coerce(Args0, Args, Modes0, Modes, Det, ExtraGoals, !ModeInfo) :-
    ( if Args0 = [X, Y] then
        mode_info_get_instmap(!.ModeInfo, InstMap),
        ( if instmap_is_reachable(InstMap) then
            mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
            mode_info_get_var_table(!.ModeInfo, VarTable),
            lookup_var_type(VarTable, X, TypeX),
            lookup_var_type(VarTable, Y, TypeY),
            instmap_lookup_var(InstMap, X, InstX),
            instmap_lookup_var(InstMap, Y, InstY),
            ( if
                inst_is_ground(ModuleInfo0, TypeX, InstX),
                not inst_is_clobbered(ModuleInfo0, InstX)
            then
                modecheck_coerce_vars(ModuleInfo0, X, Y, TypeX, TypeY,
                    InstX, InstY, Result, !ModeInfo),
                (
                    Result = coerce_mode_ok(Args, Modes, ExtraGoals),
                    Det = detism_det
                ;
                    Result = coerce_mode_error,
                    Args = [X, Y],
                    Modes = Modes0,
                    Det = detism_erroneous,
                    ExtraGoals = no_extra_goals
                )
            else
                WaitingVars = set_of_var.make_singleton(X),
                Reason = input_inst_not_ground(InstX),
                Error = coerce_error([], TypeX, TypeY, Reason),
                ModeError = mode_error_coerce_error([Error]),
                mode_info_error(WaitingVars, ModeError, !ModeInfo),
                Args = [X, Y],
                Modes = Modes0,
                Det = detism_erroneous,
                ExtraGoals = no_extra_goals
            )
        else
            Args = Args0,
            Modes = Modes0,
            Det = detism_erroneous,
            ExtraGoals = no_extra_goals
        )
    else
        unexpected($pred, "bad coerce")
    ).

:- pred modecheck_coerce_vars(module_info::in, prog_var::in, prog_var::in,
    mer_type::in, mer_type::in, mer_inst::in, mer_inst::in,
    modecheck_coerce_result::out, mode_info::in, mode_info::out) is det.

modecheck_coerce_vars(ModuleInfo0, X, Y, TypeX, TypeY, InstX, InstY, Result,
        !ModeInfo) :-
    mode_info_get_pred_id(!.ModeInfo, PredId),
    module_info_pred_info(ModuleInfo0, PredId, PredInfo),
    pred_info_get_typevarset(PredInfo, TVarSet),

    mode_info_var_is_live(!.ModeInfo, X, LiveX),
    mode_info_var_is_live(!.ModeInfo, Y, LiveY),
    ( if LiveX = is_live, LiveY = is_live then
        BothLive = is_live
    else
        BothLive = is_dead
    ),

    ExistQTVars = [],
    RevTermPath0 = [],
    set.init(ExpandedInsts0),
    modecheck_coerce_make_inst(ModuleInfo0, TVarSet, LiveX, ExistQTVars,
        RevTermPath0, TypeX, TypeY, ExpandedInsts0, InstX, MaybeFinalInstY),
    (
        MaybeFinalInstY = ok1(FinalInstY),
        ( if
            abstractly_unify_inst(TypeX, BothLive, real_unify, InstX,
                ground_inst, UnifyInstX, _Det, ModuleInfo0, ModuleInfo1)
        then
            ModuleInfo = ModuleInfo1,
            FinalInstX = UnifyInstX
        else
            unexpected($pred, "abstractly_unify_inst failed")
        ),
        mode_info_set_module_info(ModuleInfo, !ModeInfo),
        modecheck_set_var_inst(X, FinalInstX, no, !ModeInfo),
        ModeX = from_to_mode(InstX, FinalInstX),
        ( if inst_is_free(ModuleInfo, InstY) then
            % Y is free so bind the coercion result to Y.
            modecheck_set_var_inst(Y, FinalInstY, no, !ModeInfo),
            ModeY = from_to_mode(InstY, FinalInstY),
            Result = coerce_mode_ok([X, Y], [ModeX, ModeY], no_extra_goals)
        else
            % Y is bound so bind the coercion result to a fresh variable
            % YPrime, then unify Y = YPrime.
            modecheck_info_create_fresh_var(TypeY, YPrime, !ModeInfo),
            create_var_var_unification(Y, YPrime, TypeY, !.ModeInfo,
                ExtraGoal),
            ExtraGoals = extra_goals([], [ExtraGoal]),
            modecheck_set_var_inst(YPrime, FinalInstY, no, !ModeInfo),
            ModeYPrime = from_to_mode(free_inst, FinalInstY),
            Result = coerce_mode_ok([X, YPrime], [ModeX, ModeYPrime],
                ExtraGoals)
        )
    ;
        MaybeFinalInstY = error1(Errors),
        ModeError = mode_error_coerce_error(Errors),
        set_of_var.init(WaitingVars),
        mode_info_error(WaitingVars, ModeError, !ModeInfo),
        Result = coerce_mode_error
    ).

:- pred modecheck_info_create_fresh_var(mer_type::in, prog_var::out,
    mode_info::in, mode_info::out) is det.

modecheck_info_create_fresh_var(VarType, Var, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_info_get_var_table(!.ModeInfo, VarTable0),
    create_fresh_var(ModuleInfo, VarType, Var, VarTable0, VarTable),
    mode_info_set_var_table(VarTable, !ModeInfo).

%---------------------------------------------------------------------------%

:- type expanded_insts == set(pair(inst_name, mer_type)).

    % Try to produce the resulting inst of a coercion from TypeX to TypeY.
    % InstX is the initial inst of the input term.
    % If the coercion is mode correct, then Result is bound to 'ok(InstY)'
    % where InstY is the final inst of the result term.
    % If there is a mode error, Result is bound to 'coerce_error(Error)'.
    %
    % ConsExistQTVars are the existentially quantified type variables
    % from the constructor that we are currently following.
    %
:- pred modecheck_coerce_make_inst(module_info::in, tvarset::in, is_live::in,
    existq_tvars::in, rev_term_path::in, mer_type::in, mer_type::in,
    expanded_insts::in, mer_inst::in, maybe_coerce_error(mer_inst)::out)
    is det.

modecheck_coerce_make_inst(ModuleInfo, TVarSet, LiveX, ConsExistQTVars,
        RevTermPath0, TypeX, TypeY, ExpandedInsts0, InstX, Result) :-
    (
        InstX = free,
        unexpected($pred, "free inst")
    ;
        InstX = any(_, _),
        unexpected($pred, "any inst")
    ;
        InstX = bound(_UniqX, _InstResultsX, _BoundFunctorsX),
        modecheck_coerce_from_bound_make_bound_functor(ModuleInfo, TVarSet,
            LiveX, ConsExistQTVars, RevTermPath0, TypeX, TypeY, ExpandedInsts0,
            InstX, Result)
    ;
        InstX = ground(UniqX, HOInstInfoX),
        (
            HOInstInfoX = none_or_default_func,
            modecheck_coerce_from_ground_make_inst(ModuleInfo, TVarSet, LiveX,
                UniqX, ConsExistQTVars, RevTermPath0, TypeX, TypeY,
                InstX, Result)
        ;
            HOInstInfoX = higher_order(_PredInstInfoX),
            UniqY = uniqueness_for_coerce_result(LiveX, UniqX),
            % Coerce cannot change the calling convention.
            InstY = ground(UniqY, HOInstInfoX),
            Result = ok1(InstY)
        )
    ;
        InstX = not_reached,
        InstY = not_reached,
        Result = ok1(InstY)
    ;
        InstX = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        InstX = constrained_inst_vars(InstVars, SubInstX),
        % The input term of TypeX is approximated by a ground inst SubInstX.
        % After conversion, the result term of TypeY must be approximated by
        % a ground inst SubInstY.
        modecheck_coerce_make_inst(ModuleInfo, TVarSet, LiveX, ConsExistQTVars,
            RevTermPath0, TypeX, TypeY, ExpandedInsts0, SubInstX, SubResult),
        (
            SubResult = ok1(SubInstY),
            InstY = constrained_inst_vars(InstVars, SubInstY),
            Result = ok1(InstY)
        ;
            SubResult = error1(Errors),
            Result = error1(Errors)
        )
    ;
        InstX = defined_inst(InstNameX),
        % Avoid infinite loops. We track inst-type pairs because a user-defined
        % inst can be used with different types.
        ( if
            set.insert_new(InstNameX - TypeX, ExpandedInsts0, ExpandedInsts1)
        then
            inst_lookup(ModuleInfo, InstNameX, InstX1),
            modecheck_coerce_make_inst(ModuleInfo, TVarSet, LiveX,
                ConsExistQTVars, RevTermPath0, TypeX, TypeY, ExpandedInsts1,
                InstX1, Result)
        else
            % If we would enter an infinite loop, return the inst unexpanded.
            % A recursive check, by definition, cannot find any errors that the
            % nonrecursive part of the algorithm would not find.
            %
            % Ensure that we are definitely dealing with user-defined inst.
            % This check might not be strictly necessary.
            ( if is_user_inst(InstNameX) then
                % If TypeX =< TypeY then an inst valid for TypeX must be
                % valid for TypeY.
                ( if is_subtype(ModuleInfo, TVarSet, TypeX, TypeY) then
                    InstY = defined_inst(InstNameX),
                    Result = ok1(InstY)
                else
                    list.reverse(RevTermPath0, TermPath),
                    Reason = has_inst_expect_upcast(InstX),
                    Error = coerce_error(TermPath, TypeX, TypeY, Reason),
                    Result = error1([Error])
                )
            else
                unexpected($pred, "not user-defined inst")
            )
        )
    ).

:- func uniqueness_for_coerce_result(is_live, uniqueness) = uniqueness.

uniqueness_for_coerce_result(LiveX, UniqX) = UniqY :-
    (
        UniqX = shared,
        UniqY = shared
    ;
        ( UniqX = unique
        ; UniqX = mostly_unique
        ),
        (
            LiveX = is_live,
            UniqY = shared
        ;
            LiveX = is_dead,
            UniqY = UniqX
        )
    ;
        ( UniqX = clobbered
        ; UniqX = mostly_clobbered
        ),
        % We know that the input must not be clobbered.
        unexpected($pred, "clobbered")
    ).

:- pred is_user_inst(inst_name::in) is semidet.

is_user_inst(InstName) :-
    require_complete_switch [InstName]
    (
        InstName = user_inst(_, _)
    ;
        InstName = typed_inst(_, InstName1),
        is_user_inst(InstName1)
    ;
        ( InstName = unify_inst(_, _, _, _)
        ; InstName = merge_inst(_, _)
        ; InstName = ground_inst(_, _, _, _)
        ; InstName = any_inst(_, _, _, _)
        ; InstName = shared_inst(_)
        ; InstName = mostly_uniq_inst(_)
        ; InstName = typed_ground(_, _)
        ),
        sorry($pred, "unexpected compiler generated inst_name")
    ).

%---------------------------------------------------------------------------%

    % "from_bound" refers to the fact that the input term has a `bound(...)'
    % inst. This predicate tries to create the `bound(...)' result inst,
    % if the conversion is valid.
    %
:- pred modecheck_coerce_from_bound_make_bound_functor(module_info::in,
    tvarset::in, is_live::in, existq_tvars::in, rev_term_path::in,
    mer_type::in, mer_type::in, expanded_insts::in,
    mer_inst::in(mer_inst_is_bound), maybe_coerce_error(mer_inst)::out) is det.

modecheck_coerce_from_bound_make_bound_functor(ModuleInfo, TVarSet, LiveX,
        ConsExistQTVars, RevTermPath0, TypeX, TypeY, ExpandedInsts0,
        InstX, Result) :-
    InstX = bound(UniqX, _InstResultsX, BoundFunctorsX),
    modecheck_coerce_from_bound_make_bound_functors(ModuleInfo, TVarSet, LiveX,
        ConsExistQTVars, RevTermPath0, TypeX, TypeY, ExpandedInsts0,
        BoundFunctorsX, BoundFunctorsY, BadConsIdErrors, [], DeeperErrors),
    (
        BadConsIdErrors = [],
        (
            DeeperErrors = [],
            UniqY = uniqueness_for_coerce_result(LiveX, UniqX),
            % XXX A better approximation of InstResults is probably possible.
            InstResults = inst_test_results(
                inst_result_is_ground,
                inst_result_does_not_contain_any,
                inst_result_contains_inst_names_unknown,
                inst_result_contains_inst_vars_unknown,
                inst_result_contains_types_unknown,
                inst_result_no_type_ctor_propagated
            ),
            InstY = bound(UniqY, InstResults, BoundFunctorsY),
            Result = ok1(InstY)
        ;
            DeeperErrors = [_ | _],
            Result = error1(DeeperErrors)
        )
    ;
        BadConsIdErrors = [HeadBadConsIdError | TailBadConsIdErrors],
        % We report both
        % - the invalid functors in BoundFunctorsX (ConsIdError), and
        % - any errors found deeper in the inst tree (DeeperErrors).
        list.reverse(RevTermPath0, TermPath),
        Reason = cons_id_errors(InstX,
            HeadBadConsIdError, TailBadConsIdErrors),
        ConsIdError = coerce_error(TermPath, TypeX, TypeY, Reason),
        Result = error1([ConsIdError | DeeperErrors])
    ).

:- pred modecheck_coerce_from_bound_make_bound_functors(module_info::in,
    tvarset::in, is_live::in, existq_tvars::in, rev_term_path::in,
    mer_type::in, mer_type::in, expanded_insts::in,
    list(bound_functor)::in, list(bound_functor)::out,
    list(bound_functor_cons_id_error)::out,
    list(coerce_error)::in, list(coerce_error)::out) is det.

modecheck_coerce_from_bound_make_bound_functors(ModuleInfo, TVarSet, LiveX,
        ConsExistQTVars, RevTermPath0, TypeX, TypeY, ExpandedInsts0,
        BoundFunctorsX, BoundFunctorsY, BadConsIdErrors, !Errors) :-
    (
        BoundFunctorsX = [],
        BoundFunctorsY = [],
        BadConsIdErrors = []
    ;
        BoundFunctorsX = [HeadBoundFunctorX | TailBoundFunctorsX],
        modecheck_coerce_from_bound_make_bound_functor(ModuleInfo, TVarSet,
            LiveX, ConsExistQTVars, RevTermPath0, TypeX, TypeY, ExpandedInsts0,
            HeadBoundFunctorX, MaybeHeadBoundFunctorY, !Errors),
        % Check remaining functors in this `bound()' inst so that we can
        % report multiple invalid functors together.
        modecheck_coerce_from_bound_make_bound_functors(ModuleInfo, TVarSet,
            LiveX, ConsExistQTVars, RevTermPath0, TypeX, TypeY, ExpandedInsts0,
            TailBoundFunctorsX, TailBoundFunctorsY, TailBadConsIdErrors,
            !Errors),
        (
            MaybeHeadBoundFunctorY = bfoe_ok(HeadBoundFunctorY),
            BoundFunctorsY = [HeadBoundFunctorY | TailBoundFunctorsY],
            BadConsIdErrors = TailBadConsIdErrors
        ;
            MaybeHeadBoundFunctorY = bfoe_cons_id_error(HeadBadConsIdError),
            BoundFunctorsY = TailBoundFunctorsY,
            BadConsIdErrors = [HeadBadConsIdError | TailBadConsIdErrors]
        ;
            MaybeHeadBoundFunctorY = bfoe_other_error,
            BoundFunctorsY = TailBoundFunctorsY,
            BadConsIdErrors = TailBadConsIdErrors
        )
    ).

:- type bound_functor_or_error
    --->    bfoe_ok(bound_functor)
    ;       bfoe_cons_id_error(bound_functor_cons_id_error)
    ;       bfoe_other_error.    % error kept separately

:- pred modecheck_coerce_from_bound_make_bound_functor(module_info::in,
    tvarset::in, is_live::in, existq_tvars::in, rev_term_path::in,
    mer_type::in, mer_type::in, expanded_insts::in,
    bound_functor::in, bound_functor_or_error::out,
    list(coerce_error)::in, list(coerce_error)::out) is det.

modecheck_coerce_from_bound_make_bound_functor(ModuleInfo, TVarSet, LiveX,
        ConsExistQTVars, RevTermPath0, TypeX, TypeY, ExpandedInsts0,
        BoundFunctorX, MaybeBoundFunctorY, !Errors) :-
    BoundFunctorX = bound_functor(ConsIdX, ArgInstsX),
    (
        ( ConsIdX = du_data_ctor(_)
        ; ConsIdX = tuple_cons(_)
        ; ConsIdX = some_int_const(_)
        ; ConsIdX = float_const(_)
        ; ConsIdX = char_const(_)
        ; ConsIdX = string_const(_)
        ),
        ( if existq_tvars_contains(ConsExistQTVars, TypeX) then
            % TypeX is an existential type. We can assume that TypeX = TypeY,
            % so BoundFunctorX is acceptable for the result term as well.
            % The only thing we do is to remove uniqueness in the result inst
            % if the input remains live (might not be strictly necessary).
            copy_bound_functor_for_coerce_result(LiveX,
                BoundFunctorX, BoundFunctorY),
            MaybeBoundFunctorY = bfoe_ok(BoundFunctorY)
        else
            modecheck_coerce_from_bound_make_bound_functor_not_existq(
                ModuleInfo, TVarSet, LiveX, RevTermPath0, TypeX, TypeY,
                ExpandedInsts0, ConsIdX, ArgInstsX, MaybeBoundFunctorY,
                !Errors)
        )
    ;
        ( ConsIdX = closure_cons(_)
        ; ConsIdX = impl_defined_const(_)
        ; ConsIdX = type_ctor_info_const(_, _, _)
        ; ConsIdX = base_typeclass_info_const(_, _, _, _)
        ; ConsIdX = type_info_cell_constructor(_)
        ; ConsIdX = type_info_const(_)
        ; ConsIdX = typeclass_info_cell_constructor
        ; ConsIdX = typeclass_info_const(_)
        ; ConsIdX = ground_term_const(_, _)
        ; ConsIdX = tabling_info_const(_)
        ; ConsIdX = table_io_entry_desc(_)
        ; ConsIdX = deep_profiling_proc_layout(_)
        ),
        unexpected($pred, "unsupported cons_id")
    ).

:- inst supported_cons_id for cons_id/0
    --->    du_data_ctor(ground)
    ;       tuple_cons(ground)
    ;       some_int_const(ground)
    ;       float_const(ground)
    ;       char_const(ground)
    ;       string_const(ground).

:- pred modecheck_coerce_from_bound_make_bound_functor_not_existq(
    module_info::in, tvarset::in, is_live::in, rev_term_path::in,
    mer_type::in, mer_type::in, expanded_insts::in,
    cons_id::in(supported_cons_id), list(mer_inst)::in,
    bound_functor_or_error::out,
    list(coerce_error)::in, list(coerce_error)::out) is det.

modecheck_coerce_from_bound_make_bound_functor_not_existq(ModuleInfo, TVarSet,
        LiveX, RevTermPath0, TypeX, TypeY, ExpandedInsts0,
        ConsIdX, ArgInstsX0, MaybeBoundFunctorY, !Errors) :-
    % The user may have provided an inst that does not make sense for the type.
    % The compiler *should* check for that elsewhere, in types_into_modes.m,
    % but neither wangp nor I (zs) are confident that it guarantees catching
    % all such errors (mostly because it may not be *invoked* on every inst
    % that should be checked).
    (
        (
            ConsIdX = du_data_ctor(DuCtorX),
            get_bound_functor_cons_and_arg_types(ModuleInfo, TypeX, TypeY,
                DuCtorX, DuCtorY, GetConsArgsResult),
            ConsIdY = du_data_ctor(DuCtorY)
        ;
            ConsIdX = tuple_cons(ConsArity),
            get_tuple_cons_arg_types(TypeX, TypeY, ConsArity,
                GetConsArgsResult),
            ConsIdY = tuple_cons(ConsArity)
        ),
        (
            GetConsArgsResult = cons_args(ArgTypesX, ArgTypesY, Arity,
                ConsExistQTVars, NumExtraArgs),
            % Separate out insts for type_infos and type_class_infos from
            % actual constructor arguments.
            list.det_split_list(NumExtraArgs, ArgInstsX0,
                ExtraArgsInsts, ArgInstsX),
            list.length(ArgInstsX, NumArgInstsX),
            ( if NumArgInstsX = Arity then
                modecheck_coerce_from_bound_make_bound_functor_arg_insts(
                    ModuleInfo, TVarSet, LiveX, RevTermPath0, ExpandedInsts0,
                    ConsIdX, ConsExistQTVars, 1, ArgTypesX, ArgTypesY,
                    ArgInstsX, OkArgInstsY, ErrorsY),
                (
                    ErrorsY = [],
                    % Add back the insts for type_infos and type_class_infos
                    % that were separated out.
                    BoundFunctorY = bound_functor(ConsIdY,
                        ExtraArgsInsts ++ OkArgInstsY),
                    MaybeBoundFunctorY = bfoe_ok(BoundFunctorY)
                ;
                    ErrorsY = [_ | _],
                    !:Errors = ErrorsY ++ !.Errors,
                    MaybeBoundFunctorY = bfoe_other_error
                )
            else
                ConsIdError = bad_cons_id_input_inst_arity(ConsIdX,
                    NumArgInstsX, Arity),
                MaybeBoundFunctorY = bfoe_cons_id_error(ConsIdError)
            )
        ;
            GetConsArgsResult = bad_cons_id_for_input_type,
            MaybeBoundFunctorY = bfoe_cons_id_error(bad_cons_id_input(ConsIdX))
        ;
            GetConsArgsResult = bad_cons_id_for_result_type,
            MaybeBoundFunctorY =
                bfoe_cons_id_error(bad_cons_id_result(ConsIdY))
        )
    ;
        ( ConsIdX = some_int_const(_)
        ; ConsIdX = float_const(_)
        ; ConsIdX = char_const(_)
        ; ConsIdX = string_const(_)
        ),
        ( if cons_id_matches_builtin_type(ConsIdX, TypeX) then
            expect(unify(TypeX, TypeY), $pred,
                "coercion between different builtin types"),
            expect(unify(ArgInstsX0, []), $pred,
                "bound functor literal has arguments"),
            BoundFunctorY = bound_functor(ConsIdX, ArgInstsX0),
            MaybeBoundFunctorY = bfoe_ok(BoundFunctorY)
        else
            MaybeBoundFunctorY = bfoe_cons_id_error(bad_cons_id_input(ConsIdX))
        )
    ).

:- type get_arg_types_result
    --->    cons_args(
                arg_types_x         :: list(mer_type),
                arg_types_y         :: list(mer_type),
                arity               :: int,
                cons_existq_tvars   :: existq_tvars,
                num_extra_args      :: int
            )
    ;       bad_cons_id_for_input_type
    ;       bad_cons_id_for_result_type.

:- pred get_bound_functor_cons_and_arg_types(module_info::in,
    mer_type::in, mer_type::in, du_ctor::in, du_ctor::out,
    get_arg_types_result::out) is det.

get_bound_functor_cons_and_arg_types(ModuleInfo, TypeX, TypeY,
        DuCtorX, DuCtorY, Result) :-
    type_to_ctor_det(TypeY, TypeCtorY),
    make_du_ctor_for_type_ctor(TypeCtorY, DuCtorX, DuCtorY),
    (
        TypeX = defined_type(_, _, _),
        ( if
            % This fails if the input type does not actually have the
            % functor given in a `bound' inst.
            get_ctor_arg_types_do_subst(ModuleInfo, TypeX, DuCtorX, ArgTypesX)
        then
            ( if
                TypeY = defined_type(_, _, _),
                % This fails if the result type does not have a constructor
                % matching that of the input type.
                get_ctor_arg_types_do_subst(ModuleInfo, TypeY, DuCtorY,
                    ArgTypesY)
            then
                ( if
                    list.length(ArgTypesX, Arity),
                    list.length(ArgTypesY, Arity)
                then
                    get_ctor_existq_tvars_det(ModuleInfo, TypeX, DuCtorX,
                        ConsExistQTVars, NumExtraArgs),
                    Result = cons_args(ArgTypesX, ArgTypesY, Arity,
                        ConsExistQTVars, NumExtraArgs)
                else
                    unexpected($pred, "arg types length mismatch")
                )
            else
                Result = bad_cons_id_for_result_type
            )
        else
            Result = bad_cons_id_for_input_type
        )
    ;
        TypeX = tuple_type(_, _),
        ( if DuCtorX = du_ctor(unqualified("{}"), Arity, _) then
            get_tuple_cons_arg_types(TypeX, TypeY, Arity, Result)
        else
            Result = bad_cons_id_for_input_type
        )
    ;
        TypeX = builtin_type(BuiltinType),
        expect(unify(TypeX, TypeY), $pred,
            "coercion between different builtin types"),
        (
            BuiltinType = builtin_type_char,
            DuCtorX = du_ctor(_SymName, Arity, _),
            ( if Arity = 0 then
                ArgTypesX = [],
                ArgTypesY = [],
                ConsExistQTVars = [],
                NumExtraArgs = 0,
                Result = cons_args(ArgTypesX, ArgTypesY, Arity,
                    ConsExistQTVars, NumExtraArgs)
            else
                Result = bad_cons_id_for_input_type
            )
        ;
            ( BuiltinType = builtin_type_int(_)
            ; BuiltinType = builtin_type_float
            ; BuiltinType = builtin_type_string
            ),
            Result = bad_cons_id_for_input_type
        )
    ;
        TypeX = kinded_type(TypeX1, Kind),
        ( if TypeY = kinded_type(TypeY1, Kind) then
            get_bound_functor_cons_and_arg_types(ModuleInfo, TypeX1, TypeY1,
                DuCtorX, _DuCtorY, Result)
        else
            sorry($pred, "kinded type mismatch")
        )
    ;
        ( TypeX = type_variable(_, _)
        ; TypeX = higher_order_type(_, _, _, _)
        ; TypeX = apply_n_type(_, _, _)
        ),
        Result = bad_cons_id_for_input_type
    ).

:- pred get_tuple_cons_arg_types(mer_type::in, mer_type::in, int::in,
    get_arg_types_result::out) is det.

get_tuple_cons_arg_types(TypeX, TypeY, Arity, Result) :-
    ( if
        TypeX = tuple_type(ArgTypesX, _),
        list.length(ArgTypesX, Arity)
    then
        ( if
            TypeY = tuple_type(ArgTypesY, _),
            list.length(ArgTypesY, Arity)
        then
            ConsExistQTVars = [],
            NumExtraArgs = 0,
            Result = cons_args(ArgTypesX, ArgTypesY, Arity,
                ConsExistQTVars, NumExtraArgs)
        else
            unexpected($pred, "tuple type mismatch")
        )
    else
        Result = bad_cons_id_for_input_type
    ).

:- pred modecheck_coerce_from_bound_make_bound_functor_arg_insts(
    module_info::in, tvarset::in, is_live::in, rev_term_path::in,
    expanded_insts::in, cons_id::in, existq_tvars::in,
    int::in, list(mer_type)::in, list(mer_type)::in,
    list(mer_inst)::in, list(mer_inst)::out, list(coerce_error)::out) is det.

modecheck_coerce_from_bound_make_bound_functor_arg_insts(ModuleInfo, TVarSet,
        LiveX, RevTermPath0, ExpandedInsts0, ConsIdX, ConsExistQTVars,
        CurArgNum, ArgTypesX, ArgTypesY, ArgInstsX, OkArgInstsY, Errors) :-
    ( if
        ArgTypesX = [],
        ArgTypesY = [],
        ArgInstsX = []
    then
        OkArgInstsY = [],
        Errors = []
    else if
        ArgTypesX = [HeadArgTypeX | TailArgTypesX],
        ArgTypesY = [HeadArgTypeY | TailArgTypesY],
        ArgInstsX = [HeadArgInstX | TailArgInstsX]
    then
        Step = coerce_error_term_path_step(ConsIdX, CurArgNum),
        RevTermPath1 = [Step | RevTermPath0],
        modecheck_coerce_make_inst(ModuleInfo, TVarSet, LiveX, ConsExistQTVars,
            RevTermPath1, HeadArgTypeX, HeadArgTypeY, ExpandedInsts0,
            HeadArgInstX, MaybeHeadArgInstY),
        modecheck_coerce_from_bound_make_bound_functor_arg_insts(ModuleInfo,
            TVarSet, LiveX, RevTermPath0, ExpandedInsts0,
            ConsIdX, ConsExistQTVars, CurArgNum + 1,
            TailArgTypesX, TailArgTypesY,
            TailArgInstsX, OkTailArgInstsY, TailErrors),
        (
            MaybeHeadArgInstY = ok1(HeadArgInstY),
            OkArgInstsY = [HeadArgInstY | OkTailArgInstsY],
            Errors = TailErrors
        ;
            MaybeHeadArgInstY = error1(HeadErrors),
            OkArgInstsY = OkTailArgInstsY,
            Errors = HeadErrors ++ TailErrors
        )
    else
        unexpected($pred, "length mismatch")
    ).

:- pred cons_id_matches_builtin_type(cons_id::in, mer_type::in) is semidet.

cons_id_matches_builtin_type(ConsId, Type) :-
    (
        ConsId = some_int_const(IC),
        Type = builtin_type(builtin_type_int(type_of_int_const(IC)))
    ;
        ConsId = float_const(_),
        Type = float_type
    ;
        ConsId = char_const(_),
        Type = char_type
    ;
        ConsId = string_const(_),
        Type = string_type
    ).

%---------------------------------------------------------------------------%

    % "from_ground" refers to the fact that the input term has the inst
    % `ground'. This predicate tries to create the result inst, if the
    % conversion is valid.
    %
:- pred modecheck_coerce_from_ground_make_inst(module_info::in, tvarset::in,
    is_live::in, uniqueness::in, existq_tvars::in, rev_term_path::in,
    mer_type::in, mer_type::in, mer_inst::in,
    maybe_coerce_error(mer_inst)::out) is det.

modecheck_coerce_from_ground_make_inst(ModuleInfo, TVarSet, LiveX, UniqX,
        ExistQTVars, RevTermPath0, TypeX, TypeY, InstX, MaybeInstY) :-
    ( if
        ( TypeX = TypeY
        ; existq_tvars_contains(ExistQTVars, TypeX)
        )
    then
        UniqY = uniqueness_for_coerce_result(LiveX, UniqX),
        InstY = ground(UniqY, none_or_default_func),
        MaybeInstY = ok1(InstY)
    else if is_subtype(ModuleInfo, TVarSet, TypeX, TypeY) then
        set.init(SeenTypes0),
        modecheck_coerce_from_ground_make_inst_for_subtype(ModuleInfo, TVarSet,
            LiveX, UniqX, SeenTypes0, TypeX, TypeY, InstY),
        MaybeInstY = ok1(InstY)
    else
        list.reverse(RevTermPath0, TermPath),
        Reason = has_inst_expect_upcast(InstX),
        Error = coerce_error(TermPath, TypeX, TypeY, Reason),
        MaybeInstY = error1([Error])
    ).

    % Precondition: TypeX =< TypeY.
    %
:- pred modecheck_coerce_from_ground_make_inst_for_subtype(module_info::in,
    tvarset::in, is_live::in, uniqueness::in, set(mer_type)::in,
    mer_type::in, mer_type::in, mer_inst::out) is det.

modecheck_coerce_from_ground_make_inst_for_subtype(ModuleInfo, TVarSet,
        LiveX, UniqX, SeenTypes0, TypeX, TypeY, InstY) :-
    % We can preserve more information by creating a `bound' inst from the
    % constructors in TypeX. This is only worth doing if the types differ,
    % and if it would not lead to an infinite loop.
    ( if
        TypeX \= TypeY,
        set.insert_new(TypeX, SeenTypes0, SeenTypes1),
        type_constructors(ModuleInfo, TypeX, CtorsX)
    then
        modecheck_coerce_from_ground_make_bound_inst(ModuleInfo, TVarSet,
            LiveX, UniqX, SeenTypes1, TypeX, TypeY, CtorsX, InstY)
    else
        UniqY = uniqueness_for_coerce_result(LiveX, UniqX),
        InstY = ground(UniqY, none_or_default_func)
    ).

:- pred modecheck_coerce_from_ground_make_bound_inst(module_info::in,
    tvarset::in, is_live::in, uniqueness::in, set(mer_type)::in,
    mer_type::in, mer_type::in, list(constructor)::in, mer_inst::out) is det.

modecheck_coerce_from_ground_make_bound_inst(ModuleInfo, TVarSet,
        LiveX, UniqX, SeenTypes, TypeX, TypeY, CtorsX, InstY) :-
    list.map(
        modecheck_coerce_from_ground_make_bound_functor(ModuleInfo,
            TVarSet, LiveX, UniqX, SeenTypes, TypeX, TypeY),
        CtorsX, BoundFunctorsY),
    UniqY = uniqueness_for_coerce_result(LiveX, UniqX),
    % XXX A better approximation of InstResults is probably possible.
    InstResults = inst_test_results(
        inst_result_is_ground,
        inst_result_does_not_contain_any,
        inst_result_contains_inst_names_unknown,
        inst_result_contains_inst_vars_unknown,
        inst_result_contains_types_unknown,
        inst_result_no_type_ctor_propagated
    ),
    InstY = bound(UniqY, InstResults, BoundFunctorsY).

:- pred modecheck_coerce_from_ground_make_bound_functor(module_info::in,
    tvarset::in, is_live::in, uniqueness::in, set(mer_type)::in,
    mer_type::in, mer_type::in, constructor::in, bound_functor::out) is det.

modecheck_coerce_from_ground_make_bound_functor(ModuleInfo, TVarSet,
        LiveX, UniqX, SeenTypes, _TypeX, TypeY, CtorX, BoundFunctorY) :-
    CtorX = ctor(_OrdinalX, _MaybeExistConstraintsX, CtorNameX, CtorArgsX,
        CtorArity, _ContextX),

    % Make the corresponding cons_id for TypeCtorY.
    type_to_ctor_det(TypeY, TypeCtorY),
    ModuleNameY = type_ctor_module(TypeCtorY),
    maybe_change_module_qualifier(ModuleNameY, CtorNameX, CtorNameY),
    DuCtorY = du_ctor(CtorNameY, CtorArity, TypeCtorY),

    % Get the argument types for the constructors of both types,
    % with type arguments substituted in.
    % type_constructors already substituted type args into CtorArgsX.
    get_ctor_arg_types(CtorArgsX, ArgTypesX),
    ( if
        get_ctor_arg_types_do_subst(ModuleInfo, TypeY, DuCtorY, ArgTypesY0)
    then
        ArgTypesY = ArgTypesY0
    else
        % The constructor should exist in TypeY since TypeX =< TypeY.
        unexpected($pred, "missing constructor for result type")
    ),

    % Make the argument insts for the bound functor.
    % Since TypeX =< TypeY, it must be that case each type in ArgTypesX is a
    % subtype of the corresponding type in ArgTypesY.
    list.map_corresponding(
        modecheck_coerce_from_ground_make_inst_for_subtype(ModuleInfo, TVarSet,
            LiveX, UniqX, SeenTypes),
        ArgTypesX, ArgTypesY, ArgInstsY),

    ConsIdY = du_data_ctor(DuCtorY),
    BoundFunctorY = bound_functor(ConsIdY, ArgInstsY).

%---------------------------------------------------------------------------%

:- pred make_du_ctor_for_type_ctor(type_ctor::in,
    du_ctor::in, du_ctor::out) is det.

make_du_ctor_for_type_ctor(TypeCtor, DuCtor0, DuCtor) :-
    % TypeCtor0 should not be cons_id_dummy_type_ctor after post-typecheck.
    DuCtor0 = du_ctor(SymName0, Arity, _TypeCtor0),
    ModuleName = type_ctor_module(TypeCtor),
    maybe_change_module_qualifier(ModuleName, SymName0, SymName),
    DuCtor = du_ctor(SymName, Arity, TypeCtor).

:- pred maybe_change_module_qualifier(sym_name::in, sym_name::in,
    sym_name::out) is det.

maybe_change_module_qualifier(ModuleName, SymName0, SymName) :-
    (
        SymName0 = unqualified(_),
        SymName = SymName0
    ;
        SymName0 = qualified(_ModuleName0, Name),
        SymName = qualified(ModuleName, Name)
    ).

%---------------------------------------------------------------------------%

:- pred get_ctor_arg_types_do_subst(module_info::in, mer_type::in,
    du_ctor::in, list(mer_type)::out) is semidet.

get_ctor_arg_types_do_subst(ModuleInfo, Type, DuCtor, CtorArgTypes) :-
    ( if Type = tuple_type(TypeArgs, _Kind) then
        % Assume DuCtor is correct.
        CtorArgTypes = TypeArgs
    else
        type_to_ctor_and_args(Type, TypeCtor, TypeArgs),
        module_info_get_cons_table(ModuleInfo, ConsTable),
        search_cons_table_of_type_ctor(ConsTable, TypeCtor, DuCtor, ConsDefn),
        require_det (
            ConsDefn = hlds_cons_defn(_TypeCtor, _TVarSet, TypeParams,
                _KindMap, _MaybeExistConstraints, CtorArgs, _Context),
            get_ctor_arg_types(CtorArgs, CtorArgTypes0),
            (
                TypeParams = [],
                CtorArgTypes = CtorArgTypes0
            ;
                TypeParams = [_ | _],
                map.from_corresponding_lists(TypeParams, TypeArgs, Subst),
                apply_subst_to_type_list(Subst, CtorArgTypes0, CtorArgTypes)
            )
        )
    ).

:- pred get_ctor_existq_tvars_det(module_info::in, mer_type::in, du_ctor::in,
    existq_tvars::out, int::out) is det.

get_ctor_existq_tvars_det(ModuleInfo, Type, DuCtor,
        ConsExistQTVars, NumExtraArgs) :-
    type_to_ctor_det(Type, TypeCtor),
    module_info_get_cons_table(ModuleInfo, ConsTable),
    lookup_cons_table_of_type_ctor(ConsTable, TypeCtor, DuCtor, ConsDefn),
    ConsDefn = hlds_cons_defn(_TypeCtor, _TVarSet, _TypeParams, _KindMap,
        MaybeExistConstraints, _CtorArgs, _Context),
    (
        MaybeExistConstraints = no_exist_constraints,
        ConsExistQTVars = [],
        NumExtraArgs = 0
    ;
        MaybeExistConstraints = exist_constraints(ConsExistConstraints),
        ConsExistConstraints = cons_exist_constraints(ConsExistQTVars,
            _Constraints, UnconstrainedTVars, ConstrainedTVars),
        % Count the extra arguments added by
        % polymorphism_process_existq_unify_functor.
        list.length(UnconstrainedTVars, NumTypeInfoArgs),
        list.length(ConstrainedTVars, NumTypeClassInfoArgs),
        NumExtraArgs = NumTypeInfoArgs + NumTypeClassInfoArgs,
        list.length(ConsExistQTVars, NumExistQTVars),
        expect(unify(NumExtraArgs, NumExistQTVars), $pred,
            "NumExtraArgs != NumExistQTVars")
    ).

:- pred existq_tvars_contains(existq_tvars::in, mer_type::in) is semidet.

existq_tvars_contains(ExistQTVars, Type) :-
    Type = type_variable(TVar, _),
    list.contains(ExistQTVars, TVar).

%---------------------------------------------------------------------------%

:- pred copy_bound_functor_for_coerce_result(is_live::in,
    bound_functor::in, bound_functor::out) is det.

copy_bound_functor_for_coerce_result(LiveX, BoundFunctorX, BoundFunctorY) :-
    BoundFunctorX = bound_functor(ConsId, ArgInstsX),
    list.map(copy_inst_for_coerce_result(LiveX), ArgInstsX, ArgInstsY),
    BoundFunctorY = bound_functor(ConsId, ArgInstsY).

:- pred copy_inst_for_coerce_result(is_live::in, mer_inst::in, mer_inst::out)
    is det.

copy_inst_for_coerce_result(LiveX, InstX, InstY) :-
    (
        InstX = ground(UniqX, HOInstInfo),
        UniqY = uniqueness_for_coerce_result(LiveX, UniqX),
        InstY = ground(UniqY, HOInstInfo)
    ;
        InstX = bound(UniqX, InstResults, BoundFunctorsX),
        UniqY = uniqueness_for_coerce_result(LiveX, UniqX),
        list.map(copy_bound_functor_for_coerce_result(LiveX),
            BoundFunctorsX, BoundFunctorsY),
        InstY = bound(UniqY, InstResults, BoundFunctorsY)
    ;
        InstX = defined_inst(InstName),
        InstY = defined_inst(InstName)
    ;
        InstX = constrained_inst_vars(InstVars, SubInstX),
        copy_inst_for_coerce_result(LiveX, SubInstX, SubInstY),
        InstY = constrained_inst_vars(InstVars, SubInstY)
    ;
        InstX = not_reached,
        InstY = not_reached
    ;
        InstX = free,
        unexpected($pred, "free inst")
    ;
        InstX = any(_, _),
        unexpected($pred, "any inst")
    ;
        InstX = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ).

%---------------------------------------------------------------------------%

:- type types_comparison
    --->    compare_equal
    ;       compare_equal_lt.

:- pred is_subtype(module_info::in, tvarset::in,
    mer_type::in, mer_type::in) is semidet.

is_subtype(ModuleInfo, TVarSet, TypeA, TypeB) :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    types_compare_as_given_mc(TypeTable, TVarSet, compare_equal_lt,
        TypeA, TypeB).

    % Succeed if TypeA is the same as TypeB.
    % If Comparison is compare_equal_lt, then also succeed if TypeA =< TypeB
    % by subtype definitions.
    %
    % Note: the code this predicate and its call tree almost duplicate
    % code in typecheck_coerce.m, but since they are not part of the
    % type checker, they cannot and do not maintain type assignments.
    % The _mc suffix distinguishes them from their typecheck_coerce.m
    % near-equivalents.
    %
    % In typecheck_coerce.m, the reason for allowing the Comparison parameter
    % to be compare_equal is that types_compare_as_given (no _mc) can modify
    % the type assignment to enforce equality. The code here does not do that,
    % which means that calling types_compare_as_given_mc with compare_equal
    % is equivalent to invoking "TypeA = TypeB".
    %
    % The only advantage of handling compare_equal is reducing the textual
    % difference between the code in typecheck_coerce.m and the code here.
    % However, this is enough, because even though "TypeA = TypeB" would be
    % faster than calling this predicate with compare_equal, this does not
    % matter, due to the rarity of coercion.
    %
:- pred types_compare_as_given_mc(type_table::in, tvarset::in,
    types_comparison::in, mer_type::in, mer_type::in) is semidet.

types_compare_as_given_mc(TypeTable, TVarSet, Comparison, TypeA, TypeB) :-
    ( if
        ( TypeA = type_variable(_, _)
        ; TypeB = type_variable(_, _)
        )
    then
        % Unlike typecheck.types_compare_as_given which unifies two type
        % variables, here we simply test for equality.
        TypeA = TypeB
    else
        types_compare_as_given_nonvar_mc(TypeTable, TVarSet, Comparison,
            TypeA, TypeB)
    ).

:- pred types_compare_as_given_nonvar_mc(type_table::in, tvarset::in,
    types_comparison::in, mer_type::in, mer_type::in) is semidet.

types_compare_as_given_nonvar_mc(TypeTable, TVarSet, Comparison,
        TypeA, TypeB) :-
    require_complete_switch [TypeA]
    (
        TypeA = builtin_type(BuiltinType),
        TypeB = builtin_type(BuiltinType)
    ;
        TypeA = type_variable(_, _),
        TypeB = type_variable(_, _),
        unexpected($pred, "type_variable")
    ;
        TypeA = defined_type(_, _, _),
        type_to_ctor_and_args(TypeA, TypeCtorA, ArgTypesA),
        type_to_ctor_and_args(TypeB, TypeCtorB, ArgTypesB),
        ( if TypeCtorA = TypeCtorB then
            corresponding_types_compare_as_given_mc(TypeTable, TVarSet,
                Comparison, ArgTypesA, ArgTypesB)
        else
            Comparison = compare_equal_lt,
            get_supertype(TypeTable, TVarSet, TypeCtorA, ArgTypesA,
                SuperTypeA),
            types_compare_as_given_mc(TypeTable, TVarSet, Comparison,
                SuperTypeA, TypeB)
        )
    ;
        TypeA = tuple_type(ArgTypesA, Kind),
        TypeB = tuple_type(ArgTypesB, Kind),
        corresponding_types_compare_as_given_mc(TypeTable, TVarSet, Comparison,
            ArgTypesA, ArgTypesB)
    ;
        TypeA = higher_order_type(PredOrFunc, ArgTypesA, _HOInstInfoA, Purity),
        TypeB = higher_order_type(PredOrFunc, ArgTypesB, _HOInstInfoB, Purity),
        % We do not allow subtyping in higher order argument types.
        corresponding_types_compare_as_given_mc(TypeTable, TVarSet,
            compare_equal, ArgTypesA, ArgTypesB)
    ;
        TypeA = apply_n_type(_, _, _),
        sorry($pred, "apply_n_type")
    ;
        TypeA = kinded_type(TypeA1, Kind),
        TypeB = kinded_type(TypeB1, Kind),
        types_compare_as_given_mc(TypeTable, TVarSet, Comparison,
            TypeA1, TypeB1)
    ).

:- pred corresponding_types_compare_as_given_mc(type_table::in, tvarset::in,
    types_comparison::in, list(mer_type)::in, list(mer_type)::in) is semidet.

corresponding_types_compare_as_given_mc(_TypeTable, _TVarSet, _Comparison,
        [], []).
corresponding_types_compare_as_given_mc(TypeTable, TVarSet, Comparison,
        [TypeA | TypesA], [TypeB | TypesB]) :-
    types_compare_as_given_mc(TypeTable, TVarSet, Comparison, TypeA, TypeB),
    corresponding_types_compare_as_given_mc(TypeTable, TVarSet, Comparison,
        TypesA, TypesB).

%---------------------------------------------------------------------------%
:- end_module check_hlds.modecheck_coerce.
%---------------------------------------------------------------------------%
