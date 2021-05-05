%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2021 The Mercury team.
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

:- import_module check_hlds.inst_test.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.modecheck_unify.
:- import_module check_hlds.type_util.
:- import_module hlds.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.set_of_var.

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

:- inst cons for cons_id/0
    --->    cons(ground, ground, ground).

%---------------------------------------------------------------------------%

modecheck_coerce(Args0, Args, Modes0, Modes, Det, ExtraGoals, !ModeInfo) :-
    ( if Args0 = [X, Y] then
        mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
        mode_info_get_instmap(!.ModeInfo, InstMap),
        ( if instmap_is_reachable(InstMap) then
            instmap_lookup_var(InstMap, X, InstX),
            instmap_lookup_var(InstMap, Y, InstY),
            ( if
                inst_is_ground(ModuleInfo0, InstX),
                not inst_is_clobbered(ModuleInfo0, InstX)
            then
                modecheck_coerce_vars(ModuleInfo0, X, Y, InstX, InstY, Res,
                    !ModeInfo),
                (
                    Res = coerce_mode_ok(Args, Modes, ExtraGoals),
                    Det = detism_det
                ;
                    Res = coerce_mode_error,
                    Args = [X, Y],
                    Modes = Modes0,
                    Det = detism_erroneous,
                    ExtraGoals = no_extra_goals
                )
            else
                WaitingVars = set_of_var.make_singleton(X),
                ModeError = mode_error_coerce_input_not_ground(X, InstX),
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
    mer_inst::in, mer_inst::in, modecheck_coerce_result::out,
    mode_info::in, mode_info::out) is det.

modecheck_coerce_vars(ModuleInfo0, X, Y, InstX, InstY, Res, !ModeInfo) :-
    mode_info_get_pred_id(!.ModeInfo, PredId),
    module_info_pred_info(ModuleInfo0, PredId, PredInfo),
    pred_info_get_typevarset(PredInfo, TVarSet),

    mode_info_get_var_types(!.ModeInfo, VarTypes),
    lookup_var_type(VarTypes, X, TypeX),
    lookup_var_type(VarTypes, Y, TypeY),

    mode_info_var_is_live(!.ModeInfo, X, LiveX),
    mode_info_var_is_live(!.ModeInfo, Y, LiveY),
    ( if LiveX = is_live, LiveY = is_live then
        BothLive = is_live
    else
        BothLive = is_dead
    ),

    set.init(ExpandedInsts0),
    modecheck_coerce_make_inst(ModuleInfo0, TVarSet, LiveX, TypeX, TypeY,
        ExpandedInsts0, InstX, MaybeFinalInstY),
    (
        MaybeFinalInstY = yes(FinalInstY),
        ( if
            abstractly_unify_inst(BothLive, InstX, ground_inst, real_unify,
                UnifyInstX, _Det, ModuleInfo0, ModuleInfo1)
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
            Res = coerce_mode_ok([X, Y], [ModeX, ModeY], no_extra_goals)
        else
            % Y is bound so bind the coercion result to a fresh variable
            % YPrime, then unify Y = YPrime.
            create_fresh_var(TypeY, YPrime, !ModeInfo),
            create_var_var_unification(Y, YPrime, TypeY, !.ModeInfo,
                ExtraGoal),
            ExtraGoals = extra_goals([], [ExtraGoal]),
            modecheck_set_var_inst(YPrime, FinalInstY, no, !ModeInfo),
            ModeYPrime = from_to_mode(free_inst, FinalInstY),
            Res = coerce_mode_ok([X, YPrime], [ModeX, ModeYPrime], ExtraGoals)
        )
    ;
        MaybeFinalInstY = no,
        ModeError = mode_error_coerce_ground_invalid(InstX, TypeY),
        set_of_var.init(WaitingVars),
        mode_info_error(WaitingVars, ModeError, !ModeInfo),
        Res = coerce_mode_error
    ).

:- pred create_fresh_var(mer_type::in, prog_var::out,
    mode_info::in, mode_info::out) is det.

create_fresh_var(VarType, Var, !ModeInfo) :-
    mode_info_get_var_types(!.ModeInfo, VarTypes0),
    mode_info_get_varset(!.ModeInfo, VarSet0),
    varset.new_var(Var, VarSet0, VarSet),
    add_var_type(Var, VarType, VarTypes0, VarTypes),
    mode_info_set_varset(VarSet, !ModeInfo),
    mode_info_set_var_types(VarTypes, !ModeInfo).

%---------------------------------------------------------------------------%

:- type expanded_insts == set(pair(inst_name, mer_type)).

    % Try to produce the resulting inst of a coercion from TypeX to TypeY.
    % InstX is the initial inst of the input term.
    % If the coercion is mode correct, then Res is bound to 'yes(InstY)' where
    % InstY is the final inst of the result term.
    % If there is a mode error, Res is bound to 'no'.
    %
:- pred modecheck_coerce_make_inst(module_info::in, tvarset::in, is_live::in,
    mer_type::in, mer_type::in, expanded_insts::in,
    mer_inst::in, maybe(mer_inst)::out) is det.

modecheck_coerce_make_inst(ModuleInfo, TVarSet, LiveX, TypeX, TypeY,
        ExpandedInsts0, InstX, Res) :-
    (
        ( InstX = free
        ; InstX = free(_)
        ),
        unexpected($pred, "free inst")
    ;
        InstX = any(_, _),
        unexpected($pred, "any inst")
    ;
        InstX = bound(UniqX, _InstResultsX, FunctorsX),
        modecheck_coerce_from_bound_make_bound_inst(ModuleInfo, TVarSet, LiveX,
            UniqX, TypeX, TypeY, ExpandedInsts0, FunctorsX, Res)
    ;
        InstX = ground(UniqX, HOInstInfoX),
        (
            HOInstInfoX = none_or_default_func,
            modecheck_coerce_from_ground_make_inst(ModuleInfo, TVarSet,
                LiveX, UniqX, TypeX, TypeY, Res)
        ;
            HOInstInfoX = higher_order(_PredInstInfoX),
            UniqY = uniqueness_for_coerce_result(LiveX, UniqX),
            % Coerce cannot change the calling convention.
            InstY = ground(UniqY, HOInstInfoX),
            Res = yes(InstY)
        )
    ;
        InstX = not_reached,
        InstY = not_reached,
        Res = yes(InstY)
    ;
        InstX = inst_var(_),
        unexpected($pred, "uninstantiated inst parameter")
    ;
        InstX = constrained_inst_vars(InstVars, SubInstX),
        % The input term of TypeX is approximated by a ground inst SubInstX.
        % After conversion, the result term of TypeY must be approximated by
        % a ground inst SubInstY.
        modecheck_coerce_make_inst(ModuleInfo, TVarSet, LiveX, TypeX, TypeY,
            ExpandedInsts0, SubInstX, SubRes),
        (
            SubRes = yes(SubInstY),
            InstY = constrained_inst_vars(InstVars, SubInstY),
            Res = yes(InstY)
        ;
            SubRes = no,
            Res = no
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
                TypeX, TypeY, ExpandedInsts1, InstX1, Res)
        else
            % If we would enter an infinite loop, return the inst unexpanded.
            % A recursive check, by definition, cannot find any errors that the
            % nonrecursive part of the algorithm would not find.
            %
            % Just in case, ensure that we are definitely dealing with
            % user-defined inst; and TypeX =< TypeY so an inst valid for
            % TypeX must be valid for TypeY.
            % The sanity checks might not be strictly necessary.
            ( if
                is_user_inst(InstNameX),
                check_is_subtype(ModuleInfo, TVarSet, TypeX, TypeY)
            then
                InstY = defined_inst(InstNameX),
                Res = yes(InstY)
            else
                Res = no
            )
        )
    ;
        InstX = abstract_inst(_, _),
        unexpected($pred, "abstract inst")
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
:- pred modecheck_coerce_from_bound_make_bound_inst(module_info::in,
    tvarset::in, is_live::in, uniqueness::in, mer_type::in, mer_type::in,
    expanded_insts::in, list(bound_inst)::in, maybe(mer_inst)::out) is det.

modecheck_coerce_from_bound_make_bound_inst(ModuleInfo, TVarSet, LiveX, UniqX,
        TypeX, TypeY, ExpandedInsts0, FunctorsX, Res) :-
    ( if
        modecheck_coerce_from_bound_make_bound_functors(ModuleInfo, TVarSet,
            LiveX, TypeX, TypeY, ExpandedInsts0, FunctorsX, FunctorsY)
    then
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
        InstY = bound(UniqY, InstResults, FunctorsY),
        Res = yes(InstY)
    else
        Res = no
    ).

    % XXX try to report which functors cause the coercion to be invalid
    %
:- pred modecheck_coerce_from_bound_make_bound_functors(module_info::in,
    tvarset::in, is_live::in, mer_type::in, mer_type::in, expanded_insts::in,
    list(bound_inst)::in, list(bound_inst)::out) is semidet.

modecheck_coerce_from_bound_make_bound_functors(ModuleInfo, TVarSet, LiveX,
        TypeX, TypeY, ExpandedInsts0, FunctorsX, FunctorsY) :-
    (
        FunctorsX = [],
        FunctorsY = []
    ;
        FunctorsX = [HeadFunctorX | TailFunctorsX],
        modecheck_coerce_from_bound_make_bound_functor(ModuleInfo, TVarSet,
            LiveX, TypeX, TypeY, ExpandedInsts0,
            HeadFunctorX, yes(HeadFunctorY)),
        modecheck_coerce_from_bound_make_bound_functors(ModuleInfo, TVarSet,
            LiveX, TypeX, TypeY, ExpandedInsts0,
            TailFunctorsX, TailFunctorsY),
        FunctorsY = [HeadFunctorY | TailFunctorsY]
    ).

:- pred modecheck_coerce_from_bound_make_bound_functor(module_info::in,
    tvarset::in, is_live::in, mer_type::in, mer_type::in, expanded_insts::in,
    bound_inst::in, maybe(bound_inst)::out) is det.

modecheck_coerce_from_bound_make_bound_functor(ModuleInfo, TVarSet, LiveX,
        TypeX, TypeY, ExpandedInsts0, FunctorX, Res) :-
    FunctorX = bound_functor(ConsIdX, ArgInstsX),
    % The user may have provided an inst that does not make sense for the type.
    % The compiler does not check for that elsewhere (probably it should)
    % so we try to be careful about that here.
    (
        ConsIdX = cons(_, _, _),
        ( if
            get_bound_functor_cons_and_arg_types(ModuleInfo, TypeX, TypeY,
                ConsIdX, ConsIdY, ArgTypesX, ArgTypesY, Arity),
            list.length(ArgInstsX, Arity)
        then
            ( if
                modecheck_coerce_from_bound_make_bound_functor_arg_insts(
                    ModuleInfo, TVarSet, LiveX, ExpandedInsts0,
                    ArgTypesX, ArgTypesY, ArgInstsX, ArgInstsY)
            then
                FunctorY = bound_functor(ConsIdY, ArgInstsY),
                Res = yes(FunctorY)
            else
                Res = no
            )
        else
            Res = no
        )
    ;
        ConsIdX = tuple_cons(_),
        % XXX post-typecheck does not replace occurences of
        % cons(unqualified("{}"), ...) with tuple_cons yet.
        sorry($pred, "tuple_cons")
    ;
        ( ConsIdX = int_const(_)
        ; ConsIdX = uint_const(_)
        ; ConsIdX = int8_const(_)
        ; ConsIdX = uint8_const(_)
        ; ConsIdX = int16_const(_)
        ; ConsIdX = uint16_const(_)
        ; ConsIdX = int32_const(_)
        ; ConsIdX = uint32_const(_)
        ; ConsIdX = int64_const(_)
        ; ConsIdX = uint64_const(_)
        ; ConsIdX = float_const(_)
        ; ConsIdX = char_const(_)
        ; ConsIdX = string_const(_)
        ),
        ( if
            cons_id_matches_builtin_type(ConsIdX, TypeX),
            TypeX = TypeY,
            ArgInstsX = []
        then
            FunctorY = FunctorX,
            Res = yes(FunctorY)
        else
            Res = no
        )
    ;
        ( ConsIdX = closure_cons(_, _)
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

:- pred get_bound_functor_cons_and_arg_types(module_info::in,
    mer_type::in, mer_type::in, cons_id::in(cons), cons_id::out(cons),
    list(mer_type)::out, list(mer_type)::out, int::out) is semidet.

get_bound_functor_cons_and_arg_types(ModuleInfo, TypeX, TypeY,
        ConsIdX, ConsIdY, ArgTypesX, ArgTypesY, Arity) :-
    type_to_ctor(TypeY, TypeCtorY),
    make_cons_id_for_type_ctor(TypeCtorY, ConsIdX, ConsIdY),
    require_complete_switch [TypeX]
    (
        TypeX = defined_type(_, _, _),
        TypeY = defined_type(_, _, _),
        % This fails if the input type does not actually have the
        % functor given in a `bound' inst.
        get_ctor_arg_types_do_subst(ModuleInfo, TypeX, ConsIdX,
            ArgTypesX),
        % This fails if the result type does not have a constructor
        % matching that of the input type.
        get_ctor_arg_types_do_subst(ModuleInfo, TypeY, ConsIdY,
            ArgTypesY),
        list.length(ArgTypesX, Arity),
        list.length(ArgTypesY, Arity)
    ;
        TypeX = tuple_type(ArgTypesX, _),
        TypeY = tuple_type(ArgTypesY, _),
        ConsIdX = cons(unqualified("{}"), Arity, _),
        list.length(ArgTypesX, Arity),
        list.length(ArgTypesY, Arity)
    ;
        TypeX = builtin_type(BuiltinType),
        TypeY = builtin_type(BuiltinType),
        % `cons' is used for char.
        BuiltinType = builtin_type_char,
        ConsIdX = cons(_SymName, Arity, _),
        Arity = 0,
        ArgTypesX = [],
        ArgTypesY = []
    ;
        TypeX = kinded_type(TypeX1, Kind),
        TypeY = kinded_type(TypeY1, Kind),
        get_bound_functor_cons_and_arg_types(ModuleInfo, TypeX1, TypeY1,
            ConsIdX, ConsIdY, ArgTypesX, ArgTypesY, Arity)
    ;
        ( TypeX = type_variable(_, _)
        ; TypeX = higher_order_type(_, _, _, _, _)
        ; TypeX = apply_n_type(_, _, _)
        ),
        fail
    ).

    % XXX try to report which arguments cause the coercion to be invalid
    %
:- pred modecheck_coerce_from_bound_make_bound_functor_arg_insts(
    module_info::in, tvarset::in, is_live::in, expanded_insts::in,
    list(mer_type)::in, list(mer_type)::in,
    list(mer_inst)::in, list(mer_inst)::out) is semidet.

modecheck_coerce_from_bound_make_bound_functor_arg_insts(ModuleInfo, TVarSet,
        LiveX, ExpandedInsts0, ArgTypesX, ArgTypesY, ArgInstsX, ArgInstsY) :-
    ( if
        ArgTypesX = [],
        ArgTypesY = [],
        ArgInstsX = []
    then
        ArgInstsY = []
    else if
        ArgTypesX = [HeadArgTypeX | TailArgTypesX],
        ArgTypesY = [HeadArgTypeY | TailArgTypesY],
        ArgInstsX = [HeadArgInstX | TailArgInstsX]
    then
        modecheck_coerce_make_inst(ModuleInfo, TVarSet, LiveX,
            HeadArgTypeX, HeadArgTypeY, ExpandedInsts0,
            HeadArgInstX, yes(HeadArgInstY)),
        modecheck_coerce_from_bound_make_bound_functor_arg_insts(ModuleInfo,
            TVarSet, LiveX, ExpandedInsts0,
            TailArgTypesX, TailArgTypesY,
            TailArgInstsX, TailArgInstsY),
        ArgInstsY = [HeadArgInstY | TailArgInstsY]
    else
        unexpected($pred, "length mismatch")
    ).

:- pred cons_id_matches_builtin_type(cons_id::in, mer_type::in) is semidet.

cons_id_matches_builtin_type(ConsId, Type) :-
    ( ConsId = int_const(_), Type = int_type
    ; ConsId = uint_const(_), Type = uint_type
    ; ConsId = int8_const(_), Type = int8_type
    ; ConsId = uint8_const(_), Type = uint8_type
    ; ConsId = int16_const(_), Type = int16_type
    ; ConsId = uint16_const(_), Type = uint16_type
    ; ConsId = int32_const(_), Type = int32_type
    ; ConsId = uint32_const(_), Type = uint32_type
    ; ConsId = int64_const(_), Type = int64_type
    ; ConsId = uint64_const(_), Type = uint64_type
    ; ConsId = float_const(_), Type = float_type
    ; ConsId = char_const(_), Type = char_type
    ; ConsId = string_const(_), Type = string_type
    ).

%---------------------------------------------------------------------------%

    % "from_ground" refers to the fact that the input term has the inst
    % `ground'. This predicate tries to create the result inst, if the
    % conversion is valid.
    %
:- pred modecheck_coerce_from_ground_make_inst(module_info::in, tvarset::in,
    is_live::in, uniqueness::in, mer_type::in, mer_type::in,
    maybe(mer_inst)::out) is det.

modecheck_coerce_from_ground_make_inst(ModuleInfo, TVarSet, LiveX, UniqX,
        TypeX, TypeY, MaybeInstY) :-
    ( if TypeX = TypeY then
        % This should be a common case.
        UniqY = uniqueness_for_coerce_result(LiveX, UniqX),
        InstY = ground(UniqY, none_or_default_func),
        MaybeInstY = yes(InstY)
    else if check_is_subtype(ModuleInfo, TVarSet, TypeX, TypeY) then
        set.init(SeenTypes0),
        modecheck_coerce_from_ground_make_inst_for_subtype(ModuleInfo, TVarSet,
            LiveX, UniqX, SeenTypes0, TypeX, TypeY, InstY),
        MaybeInstY = yes(InstY)
    else
        MaybeInstY = no
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
        set.insert_new(TypeX, SeenTypes0, SeenTypes1)
    then
        modecheck_coerce_from_ground_make_bound_inst(ModuleInfo, TVarSet,
            LiveX, UniqX, SeenTypes1, TypeX, TypeY, InstY)
    else
        UniqY = uniqueness_for_coerce_result(LiveX, UniqX),
        InstY = ground(UniqY, none_or_default_func)
    ).

:- pred modecheck_coerce_from_ground_make_bound_inst(module_info::in,
    tvarset::in, is_live::in, uniqueness::in, set(mer_type)::in,
    mer_type::in, mer_type::in, mer_inst::out) is det.

modecheck_coerce_from_ground_make_bound_inst(ModuleInfo, TVarSet,
        LiveX, UniqX, SeenTypes, TypeX, TypeY, InstY) :-
    ( if type_constructors(ModuleInfo, TypeX, CtorsX) then
        list.map(
            modecheck_coerce_from_ground_make_bound_functor(ModuleInfo,
                TVarSet, LiveX, UniqX, SeenTypes, TypeX, TypeY),
            CtorsX, FunctorsY),
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
        InstY = bound(UniqY, InstResults, FunctorsY)
    else
        unexpected($pred, "missing constructors")
    ).

:- pred modecheck_coerce_from_ground_make_bound_functor(module_info::in,
    tvarset::in, is_live::in, uniqueness::in, set(mer_type)::in,
    mer_type::in, mer_type::in, constructor::in, bound_inst::out) is det.

modecheck_coerce_from_ground_make_bound_functor(ModuleInfo, TVarSet,
        LiveX, UniqX, SeenTypes, _TypeX, TypeY, CtorX, FunctorY) :-
    CtorX = ctor(_OrdinalX, _MaybeExistConstraintsX, CtorNameX, CtorArgsX,
        CtorArity, _ContextX),

    % Make the corresponding cons_id for TypeCtorY.
    type_to_ctor_det(TypeY, TypeCtorY),
    ModuleNameY = type_ctor_module(TypeCtorY),
    maybe_change_module_qualifier(ModuleNameY, CtorNameX, CtorNameY),
    ConsIdY = cons(CtorNameY, CtorArity, TypeCtorY),

    % Get the argument types for the constructors of both types,
    % with type arguments substituted in.
    % type_constructors already substituted type args into CtorArgsX.
    get_ctor_arg_types(CtorArgsX, ArgTypesX),
    ( if
        get_ctor_arg_types_do_subst(ModuleInfo, TypeY, ConsIdY, ArgTypesY0)
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

    FunctorY = bound_functor(ConsIdY, ArgInstsY).

%---------------------------------------------------------------------------%

:- pred make_cons_id_for_type_ctor(type_ctor::in, cons_id::in(cons),
    cons_id::out(cons)) is det.

make_cons_id_for_type_ctor(TypeCtor, ConsId0, ConsId) :-
    ConsId0 = cons(SymName0, Arity, TypeCtor0),
    ModuleName = type_ctor_module(TypeCtor),
    maybe_change_module_qualifier(ModuleName, SymName0, SymName),
    ( if TypeCtor0 = cons_id_dummy_type_ctor then
        % Keep the dummy type_ctor from the original cons_id.
        % XXX TypeCtor0 should not be a dummy after post-typecheck.
        ConsId = cons(SymName, Arity, TypeCtor0)
    else
        ConsId = cons(SymName, Arity, TypeCtor)
    ).

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

:- pred get_ctor_arg_types_do_subst(module_info::in, mer_type::in, cons_id::in,
    list(mer_type)::out) is semidet.

get_ctor_arg_types_do_subst(ModuleInfo, Type, ConsId, CtorArgTypes) :-
    type_to_ctor_and_args(Type, TypeCtor, TypeArgs),
    module_info_get_cons_table(ModuleInfo, ConsTable),
    search_cons_table_of_type_ctor(ConsTable, TypeCtor, ConsId, ConsDefn),
    ConsDefn = hlds_cons_defn(_TypeCtor, _TVarSet, TypeParams, _KindMap,
        _MaybeExistConstraints, CtorArgs, _Context),
    get_ctor_arg_types(CtorArgs, CtorArgTypes0),
    (
        TypeParams = [],
        CtorArgTypes = CtorArgTypes0
    ;
        TypeParams = [_ | _],
        map.from_corresponding_lists(TypeParams, TypeArgs, Subst),
        apply_subst_to_type_list(Subst, CtorArgTypes0, CtorArgTypes)
    ).

%---------------------------------------------------------------------------%

:- type types_comparison
    --->    compare_equal
    ;       compare_equal_lt.

:- pred check_is_subtype(module_info::in, tvarset::in,
    mer_type::in, mer_type::in) is semidet.

check_is_subtype(ModuleInfo, TVarSet, TypeA, TypeB) :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    compare_types(TypeTable, TVarSet, compare_equal_lt, TypeA, TypeB).

    % Succeed if TypeA unifies with TypeB.
    % If Comparison is compare_equal_lt, then also succeed if TypeA =< TypeB
    % by subtype definitions.
    %
    % Note: this code is almost identical to compare_types in typecheck.m
    % but since it is not part of the type checker, does not maintain
    % type assignments.
    %
:- pred compare_types(type_table::in, tvarset::in, types_comparison::in,
    mer_type::in, mer_type::in) is semidet.

compare_types(TypeTable, TVarSet, Comparison, TypeA, TypeB) :-
    ( if
        ( TypeA = type_variable(_, _)
        ; TypeB = type_variable(_, _)
        )
    then
        % Unlike typecheck.compare_types which unifies two type variables,
        % here we simply test for equality.
        TypeA = TypeB
    else
        compare_types_nonvar(TypeTable, TVarSet, Comparison, TypeA, TypeB)
    ).

:- pred compare_types_nonvar(type_table::in, tvarset::in, types_comparison::in,
    mer_type::in, mer_type::in) is semidet.

compare_types_nonvar(TypeTable, TVarSet, Comparison, TypeA, TypeB) :-
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
        type_to_ctor_and_args(TypeA, TypeCtorA, ArgsA),
        type_to_ctor_and_args(TypeB, TypeCtorB, ArgsB),
        ( if TypeCtorA = TypeCtorB then
            compare_types_corresponding(TypeTable, TVarSet, Comparison,
                ArgsA, ArgsB)
        else
            Comparison = compare_equal_lt,
            get_supertype(TypeTable, TVarSet, TypeCtorA, ArgsA, SuperTypeA),
            compare_types(TypeTable, TVarSet, Comparison, SuperTypeA, TypeB)
        )
    ;
        TypeA = tuple_type(ArgsA, Kind),
        TypeB = tuple_type(ArgsB, Kind),
        compare_types_corresponding(TypeTable, TVarSet, Comparison,
            ArgsA, ArgsB)
    ;
        TypeA = higher_order_type(PredOrFunc, ArgsA, _HOInstInfoA,
            Purity, EvalMethod),
        TypeB = higher_order_type(PredOrFunc, ArgsB, _HOInstInfoB,
            Purity, EvalMethod),
        % We do not allow subtyping in higher order argument types.
        compare_types_corresponding(TypeTable, TVarSet, compare_equal,
            ArgsA, ArgsB)
    ;
        TypeA = apply_n_type(_, _, _),
        sorry($pred, "apply_n_type")
    ;
        TypeA = kinded_type(TypeA1, Kind),
        TypeB = kinded_type(TypeB1, Kind),
        compare_types(TypeTable, TVarSet, Comparison, TypeA1, TypeB1)
    ).

:- pred compare_types_corresponding(type_table::in, tvarset::in,
    types_comparison::in, list(mer_type)::in, list(mer_type)::in) is semidet.

compare_types_corresponding(_TypeTable, _TVarSet, _Comparison,
        [], []).
compare_types_corresponding(TypeTable, TVarSet, Comparison,
        [TypeA | TypesA], [TypeB | TypesB]) :-
    compare_types(TypeTable, TVarSet, Comparison, TypeA, TypeB),
    compare_types_corresponding(TypeTable, TVarSet, Comparison,
        TypesA, TypesB).

%---------------------------------------------------------------------------%
:- end_module check_hlds.modecheck_coerce.
%---------------------------------------------------------------------------%
