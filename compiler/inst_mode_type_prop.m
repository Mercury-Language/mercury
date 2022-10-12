%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: inst_mode_type_prop.m.
% Main author: fjh.
%
% This code of this module propagates type information into
% both insts and modes.
%
%---------------------------------------------------------------------------%

:- module check_hlds.inst_mode_type_prop.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%
%
% Propagating type information into insts.
%

    % Given a type and an inst, generate a new inst that includes
    % the information provided by the type.
    %
    % There are three sorts of information added:
    %
    % 1 Module qualifiers.
    % 2 The set of constructors in the type.
    % 3 For higher-order function types (but not higher-order predicate types),
    %   the higher-order inst, i.e. the argument modes and the determinism.
    %
    % Currently #2 is not yet implemented, due to unsolved
    % efficiency problems. (See the XXX's below.)
    %
    % There are two versions, an "eager" one and a "lazy" one. In general,
    % eager expansion is to be preferred, because the expansion is done
    % just once, whereas with lazy expansion the work will be done N times.
    % However, for recursive insts we must use lazy expansion (otherwise
    % we would get infinite regress). Also, usually many of the imported
    % procedures will not be called, so for the insts in imported mode
    % declarations N is often zero.
    %
:- pred propagate_unchecked_type_into_inst(module_info::in,
    mer_type::in, mer_inst::in, mer_inst::out) is det.

    % This predicate has the same job as propagate_type_into_inst,
    % restricted to cases where the input is 'bound(Uniq, Tests, BoundInsts)'.
    % Each bound_inst in BoundInsts contains a cons_id. This predicate records,
    % for each of those cons_ids, that the cons_id belongs to the given type,
    % *if* in fact that cons_id is one of the function symbols of that type.
    %
    % NOTE: If insts were required to belong to just one explicitly specified
    % type, as they should be, this predicate would not be necessary.
    %
    % This predicate is exported for use by inst_user.m.
    %
:- pred propagate_unchecked_type_into_bound_inst(module_info::in,
    mer_type::in, mer_inst::in(mer_inst_is_bound), mer_inst::out) is det.

%---------------------------------------------------------------------------%
%
% Propagating type information into modes.
%

:- type tprop_context
    --->    tprop_arg_list_slot(tprop_args, int)
            % The inst is in the argument list specified by the first argument
            % at the position given by the second argument.
            % (The first argument is argument #1.)
    ;       tprop_inst_name_expansion(sym_name_arity, tprop_context).
            % The inst is the expansion of the named user inst, which
            % occurs in the given context.

    % Values of this type specify the origin of a list of insts.
    %
    % These usually come from a list of modes, as each mode specifies
    % an initial and a final inst. For input modes, the initial and final
    % insts may be identical or at least similar, which means that if
    % a final inst applies a user-defined inst name to a value of a type
    % that has different type constructor from the one that the inst name
    % is declared to be for, that error is quite likely to be present
    % in the mode's initial inst as well.
    %
    % We "handle" the problem by simply not including in the error message
    % any reference to whether the inst being complained about came from
    % the initial or the final inst. If the error is present in both insts,
    % then we will generate one error for each inst, but these will be
    % identical in every respect, so sort_error_specs in error_util.m
    % will keep only one out of every pair of duplicate messages.
    %
    % The argument lists can be divided into two categories.
    % The first two, ta_pred and ta_lambda, stand on their own, in that
    % the insts in their lists are not part of a bigger inst. The last three
    % describe inst lists that *are* part of a bigger inst, whose context
    % is given by their last arguments.
    %
    % The pred_or_func flag and arity arguments of ta_lambda and ta_ho_inst
    % are needed to decide whether an inst refers to the inst of an ordinary
    % function argument, or the inst of the function result argument.
    % For arguments of predicates, cons_ids and tuples, the distinction
    % is moot.
    %
:- type tprop_args
    --->    ta_pred(pred_info)
            % The inst is in the argument list of the given predicate or
            % function.
    ;       ta_lambda(pred_or_func, int, prog_context)
            % The inst is in the argument list of a lambda expression
            % whose nature (predicate or function, arity, and source location)
            % are specified by the arguments.
    ;       ta_bound_inst(cons_id, tprop_context)
            % The inst is in the argument list of a bound_inst for
            % the data constructor, which itself occurs at the given context.
    ;       ta_tuple_inst(tprop_context)
            % The inst is in the argument list of a bound_inst for a tuple,
            % which itself occurs at the given context.
    ;       ta_ho_inst(pred_or_func, int, tprop_context).
            % The inst is in the argument list of a higher order inst,
            % which has the given arity, and which occurs at the given context.

%---------------------%

    % Given corresponding lists of types and modes, produce a new list
    % of modes which includes the information provided by the
    % corresponding types.
    %
:- pred propagate_checked_types_into_modes(module_info::in, tprop_args::in,
    list(mer_type)::in, list(mer_mode)::in, list(mer_mode)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Given a type and a mode, produce a new mode that includes the
    % information provided by the type.
    %
:- pred propagate_checked_type_into_mode(module_info::in,
    tprop_context::in, mer_type::in, mer_mode::in, mer_mode::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_lookup.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_inst_mode.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module unit.

%---------------------------------------------------------------------------%

    % Given bindings for type variables, and corresponding lists of types
    % and insts, produce a new list of insts which includes the information
    % provided by the substituted versions of the corresponding types.
    %
:- pred propagate_subst_types_into_insts(module_info::in, Args::in, int::in,
    tsubst::in, list(mer_type)::in, list(mer_inst)::in, list(mer_inst)::out,
    Errors::in, Errors::out) is det <= tprop_record(Context, Args, Errors).
:- pragma type_spec(pred(propagate_subst_types_into_insts/9),
    (Context = tprop_context, Args = tprop_args, Errors = tprop_errors)).
:- pragma type_spec(pred(propagate_subst_types_into_insts/9),
    (Context = unit, Args = unit, Errors = unit)).

propagate_subst_types_into_insts(_, _, _, _, [], [], [], !Errors).
propagate_subst_types_into_insts(_, _, _, _, [], [_ | _], [], !Errors) :-
    unexpected($pred, "length mismatch").
propagate_subst_types_into_insts(_, _, _, _, [_ | _], [], [], !Errors) :-
    unexpected($pred, "length mismatch").
propagate_subst_types_into_insts(ModuleInfo, Args, ArgNum, Subst,
        [Type | Types], [Inst0 | Insts0], [Inst | Insts], !Errors) :-
    args_slot_to_context(Args, ArgNum, Context),
    propagate_subst_type_into_inst(ModuleInfo, Context, Subst, Type,
        Inst0, Inst, !Errors),
    propagate_subst_types_into_insts(ModuleInfo, Args, ArgNum + 1, Subst,
        Types, Insts0, Insts, !Errors).

%---------------------%

:- pred propagate_subst_type_into_inst(module_info::in, Context::in,
    tsubst::in, mer_type::in, mer_inst::in, mer_inst::out,
    Errors::in, Errors::out) is det <= tprop_record(Context, Args, Errors).
:- pragma type_spec(pred(propagate_subst_type_into_inst/8),
    (Context = tprop_context, Args = tprop_args, Errors = tprop_errors)).
:- pragma type_spec(pred(propagate_subst_type_into_inst/8),
    (Context = unit, Args = unit, Errors = unit)).

propagate_subst_type_into_inst(ModuleInfo, Context, Subst, Type0, Inst0, Inst,
        !Errors) :-
    % Optimize common case.
    ( if map.is_empty(Subst) then
        Type = Type0
    else
        apply_subst_to_type(Subst, Type0, Type)
    ),
    propagate_type_into_inst(ModuleInfo, Context, Type, Inst0, Inst, !Errors).

%---------------------%

    % Given bindings for type variables, and corresponding lists of types
    % and insts, produce a new list of insts which includes the information
    % provided by the corresponding types.
    %
:- pred propagate_types_into_insts(module_info::in, Args::in, int::in,
    list(mer_type)::in, list(mer_inst)::in, list(mer_inst)::out,
    Errors::in, Errors::out) is det <= tprop_record(Context, Args, Errors).
:- pragma type_spec(pred(propagate_types_into_insts/8),
    (Context = tprop_context, Args = tprop_args, Errors = tprop_errors)).
:- pragma type_spec(pred(propagate_types_into_insts/8),
    (Context = unit, Args = unit, Errors = unit)).

propagate_types_into_insts(_, _, _, [], [], [], !Errors).
propagate_types_into_insts(_, _, _, [], [_ | _], [], !Errors) :-
    unexpected($pred, "length mismatch").
propagate_types_into_insts(_, _, _, [_ | _], [], [], !Errors) :-
    unexpected($pred, "length mismatch").
propagate_types_into_insts(ModuleInfo, Args, ArgNum,
        [Type | Types], [Inst0 | Insts0], [Inst | Insts], !Errors) :-
    args_slot_to_context(Args, ArgNum, Context),
    propagate_type_into_inst(ModuleInfo, Context,
        Type, Inst0, Inst, !Errors),
    propagate_types_into_insts(ModuleInfo, Args, ArgNum + 1,
        Types, Insts0, Insts, !Errors).

propagate_unchecked_type_into_inst(ModuleInfo, Type, Inst0, Inst) :-
    propagate_type_into_inst(ModuleInfo, unit, Type, Inst0, Inst, unit, _).

:- pred propagate_type_into_inst(module_info::in, Context::in,
    mer_type::in, mer_inst::in, mer_inst::out,
    Errors::in, Errors::out) is det <= tprop_record(Context, Args, Errors).
:- pragma type_spec(pred(propagate_type_into_inst/7),
    (Context = tprop_context, Args = tprop_args, Errors = tprop_errors)).
:- pragma type_spec(pred(propagate_type_into_inst/7),
    (Context = unit, Args = unit, Errors = unit)).

propagate_type_into_inst(ModuleInfo, Context, Type, Inst0, Inst, !Errors) :-
    ( if semidet_fail then
        % XXX We ought to expand things eagerly here, using this code.
        % However, that causes efficiency problems, so for the moment
        % we always do propagation lazily.
        ( if type_constructors(ModuleInfo, Type, Constructors) then
            propagate_type_into_inst_eagerly(ModuleInfo, Context,
                Type, Constructors, Inst0, Inst, !Errors)
        else
            Inst = Inst0
        )
    else
        propagate_type_into_inst_lazily(ModuleInfo, Context,
            Type, Inst0, Inst, !Errors)
    ).

%---------------------%

:- pred propagate_type_into_inst_eagerly(module_info::in, Context::in,
    mer_type::in, list(constructor)::in, mer_inst::in, mer_inst::out,
    Errors::in, Errors::out) is det <= tprop_record(Context, Args, Errors).
:- pragma type_spec(pred(propagate_type_into_inst_eagerly/8),
    (Context = tprop_context, Args = tprop_args, Errors = tprop_errors)).
:- pragma type_spec(pred(propagate_type_into_inst_eagerly/8),
    (Context = unit, Args = unit, Errors = unit)).

propagate_type_into_inst_eagerly(ModuleInfo, Context, Type, Constructors,
        Inst0, Inst, !Errors) :-
    (
        Inst0 = free,
        % Inst = free(Type)
        Inst = free             % XXX temporary hack
    ;
        Inst0 = free(_),
        unexpected($pred, "type info already present")
    ;
        Inst0 = ground(Uniq, HOInstInfo0),
        (
            HOInstInfo0 = none_or_default_func,
            ( if
                type_is_higher_order_details(Type, _, pf_function, _, ArgTypes)
            then
                default_higher_order_func_inst(ModuleInfo, Context, ArgTypes,
                    HigherOrderInstInfo, !Errors),
                Inst = ground(Uniq, higher_order(HigherOrderInstInfo))
            else
                type_to_ctor_det(Type, TypeCtor),
                constructors_to_bound_insts(ModuleInfo, Uniq, TypeCtor,
                    Constructors, BoundInsts0),
                list.sort_and_remove_dups(BoundInsts0, BoundInsts),
                InstResults = inst_test_results(
                    inst_result_is_ground,
                    inst_result_does_not_contain_any,
                    inst_result_contains_inst_names_known(set.init),
                    inst_result_contains_inst_vars_known(set.init),
                    inst_result_contains_types_known(set.init),
                    inst_result_type_ctor_propagated(TypeCtor)
                ),
                Inst = bound(Uniq, InstResults, BoundInsts)
            )
        ;
            HOInstInfo0 = higher_order(PredInstInfo0),
            PredInstInfo0 =
                pred_inst_info(PredOrFunc, Modes0, MaybeArgRegs, Detism),
            ( if
                type_is_higher_order_details(Type, _, PredOrFunc, _, ArgTypes),
                list.same_length(ArgTypes, Modes0)
            then
                list.length(ArgTypes, NumArgTypes),
                step_into_ho_inst(PredOrFunc, NumArgTypes, Context, Args),
                propagate_types_into_modes(ModuleInfo, Args, 1,
                    ArgTypes, Modes0, Modes, !Errors)
            else
                % The inst is not a valid inst for the type, so leave it alone.
                % This can only happen if the user has made a mistake. A mode
                % error should hopefully be reported if anything tries to match
                % with the inst.
                Modes = Modes0
            ),
            PredInstInfo =
                pred_inst_info(PredOrFunc, Modes, MaybeArgRegs, Detism),
            Inst = ground(Uniq, higher_order(PredInstInfo))
        )
    ;
        Inst0 = any(Uniq, HOInstInfo0),
        (
            HOInstInfo0 = none_or_default_func,
            ( if
                type_is_higher_order_details(Type, _, pf_function, _, ArgTypes)
            then
                default_higher_order_func_inst(ModuleInfo, Context, ArgTypes,
                    PredInstInfo, !Errors),
                Inst = any(Uniq, higher_order(PredInstInfo))
            else
                type_to_ctor_det(Type, TypeCtor),
                constructors_to_bound_any_insts(ModuleInfo, Uniq, TypeCtor,
                    Constructors, BoundInsts0),
                list.sort_and_remove_dups(BoundInsts0, BoundInsts),
                % Normally, Inst is not ground, and contains any.
                % But if all the Ctors are constants, it is ground,
                % and does not contain any.
                InstResults = inst_test_results(
                    inst_result_groundness_unknown,
                    inst_result_contains_any_unknown,
                    inst_result_contains_inst_names_known(set.init),
                    inst_result_contains_inst_vars_known(set.init),
                    inst_result_contains_types_known(set.init),
                    inst_result_type_ctor_propagated(TypeCtor)
                ),
                Inst = bound(Uniq, InstResults, BoundInsts)
            )
        ;
            HOInstInfo0 = higher_order(PredInstInfo0),
            PredInstInfo0 =
                pred_inst_info(PredOrFunc, Modes0, MaybeArgRegs, Detism),
            ( if
                type_is_higher_order_details(Type, _, PredOrFunc, _, ArgTypes),
                list.same_length(ArgTypes, Modes0)
            then
                list.length(ArgTypes, NumArgTypes),
                step_into_ho_inst(PredOrFunc, NumArgTypes, Context, Args),
                propagate_types_into_modes(ModuleInfo, Args, 1,
                    ArgTypes, Modes0, Modes, !Errors)
            else
                % The inst is not a valid inst for the type, so leave it alone.
                % This can only happen if the user has made a mistake. A mode
                % error should hopefully be reported if anything tries to match
                % with the inst.
                Modes = Modes0
            ),
            PredInstInfo =
                pred_inst_info(PredOrFunc, Modes, MaybeArgRegs, Detism),
            Inst = any(Uniq, higher_order(PredInstInfo))
        )
    ;
        Inst0 = bound(_Uniq, _InstResult, _BoundInsts0),
        propagate_type_into_bound_inst(ModuleInfo, Context,
            Type, Inst0, Inst, !Errors)
    ;
        Inst0 = not_reached,
        Inst = Inst0
    ;
        Inst0 = inst_var(_),
        Inst = Inst0
    ;
        Inst0 = constrained_inst_vars(InstVars, SubInst0),
        propagate_type_into_inst_eagerly(ModuleInfo, Context,
            Type, Constructors, SubInst0, SubInst, !Errors),
        Inst = constrained_inst_vars(InstVars, SubInst)
    ;
        Inst0 = abstract_inst(_Name, _Args),
        Inst = Inst0                        % XXX loses info
    ;
        Inst0 = defined_inst(InstName),
        check_for_bad_use_of_user_inst(ModuleInfo, InstName, Type, Context,
            MaybeUserInstInfo, !Errors),
        maybe_check_user_inst_args(ModuleInfo, Type, Context,
            MaybeUserInstInfo, !Errors),
        inst_lookup(ModuleInfo, InstName, NamedInst),
        propagate_type_into_inst_eagerly(ModuleInfo, Context,
            Type, Constructors, NamedInst, Inst, !Errors)
    ).

:- pred propagate_type_into_inst_lazily(module_info::in, Context::in,
    mer_type::in, mer_inst::in, mer_inst::out,
    Errors::in, Errors::out) is det <= tprop_record(Context, Args, Errors).
:- pragma type_spec(pred(propagate_type_into_inst_lazily/7),
    (Context = tprop_context, Args = tprop_args, Errors = tprop_errors)).
:- pragma type_spec(pred(propagate_type_into_inst_lazily/7),
    (Context = unit, Args = unit, Errors = unit)).

propagate_type_into_inst_lazily(ModuleInfo, Context, Type, Inst0, Inst,
        !Errors) :-
    (
        Inst0 = free,
        % Inst = free(Type0)
        Inst = free             % XXX temporary hack
    ;
        Inst0 = free(_),
        unexpected($pred, "typeinfo already present")
    ;
        Inst0 = ground(Uniq, HOInstInfo0),
        (
            HOInstInfo0 = none_or_default_func,
            ( if
                type_is_higher_order_details(Type, _, pf_function, _, ArgTypes)
            then
                default_higher_order_func_inst(ModuleInfo, Context, ArgTypes,
                    HOInstInfo, !Errors),
                Inst = ground(Uniq, higher_order(HOInstInfo))
            else
                % XXX The information added by this is not yet used, so
                % it is disabled since it unnecessarily complicates the insts.
                %
                % Inst = defined_inst(typed_ground(Uniq, Type))
                Inst = ground(Uniq, none_or_default_func)
            )
        ;
            HOInstInfo0 =  higher_order(PredInstInfo0),
            PredInstInfo0 =
                pred_inst_info(PredOrFunc, Modes0, MaybeArgRegs, Detism),
            ( if
                type_is_higher_order_details(Type, _, PredOrFunc, _, ArgTypes),
                list.same_length(ArgTypes, Modes0)
            then
                list.length(ArgTypes, NumArgTypes),
                step_into_ho_inst(PredOrFunc, NumArgTypes, Context, Args),
                propagate_types_into_modes(ModuleInfo, Args, 1,
                    ArgTypes, Modes0, Modes, !Errors)
            else
                % The inst is not a valid inst for the type, so leave it alone.
                % This can only happen if the user has made a mistake.
                % A mode error should hopefully be reported if anything
                % tries to match with the inst.
                Modes = Modes0
            ),
            PredInstInfo =
                pred_inst_info(PredOrFunc, Modes, MaybeArgRegs, Detism),
            Inst = ground(Uniq, higher_order(PredInstInfo))
        )
    ;
        Inst0 = any(Uniq, HOInstInfo0),
        (
            HOInstInfo0 = none_or_default_func,
            ( if
                type_is_higher_order_details(Type, _, pf_function, _, ArgTypes)
            then
                default_higher_order_func_inst(ModuleInfo, Context, ArgTypes,
                    HOInstInfo, !Errors),
                Inst = any(Uniq, higher_order(HOInstInfo))
            else
                Inst = any(Uniq, none_or_default_func)
            )
        ;
            HOInstInfo0 = higher_order(PredInstInfo0),
            PredInstInfo0 =
                pred_inst_info(PredOrFunc, Modes0, MaybeArgRegs, Detism),
            ( if
                type_is_higher_order_details(Type, _, PredOrFunc, _, ArgTypes),
                list.same_length(ArgTypes, Modes0)
            then
                list.length(ArgTypes, NumArgTypes),
                step_into_ho_inst(PredOrFunc, NumArgTypes, Context, Args),
                propagate_types_into_modes(ModuleInfo, Args, 1,
                    ArgTypes, Modes0, Modes, !Errors)
            else
                % The inst is not a valid inst for the type, so leave it alone.
                % This can only happen if the user has made a mistake.
                % A mode error should hopefully be reported if anything
                % tries to match with the inst.
                Modes = Modes0
            ),
            PredInstInfo =
                pred_inst_info(PredOrFunc, Modes, MaybeArgRegs, Detism),
            Inst = any(Uniq, higher_order(PredInstInfo))
        )
    ;
        Inst0 = bound(_Uniq, _InstResult, _BoundInsts0),
        propagate_type_into_bound_inst(ModuleInfo, Context,
            Type, Inst0, Inst, !Errors)
    ;
        Inst0 = not_reached,
        Inst = Inst0
    ;
        Inst0 = inst_var(_),
        Inst = Inst0
    ;
        Inst0 = constrained_inst_vars(InstVars, SubInst0),
        propagate_type_into_inst_lazily(ModuleInfo, Context,
            Type, SubInst0, SubInst, !Errors),
        Inst = constrained_inst_vars(InstVars, SubInst)
    ;
        Inst0 = abstract_inst(_Name, _Args),
        Inst = Inst0                        % XXX loses info
    ;
        Inst0 = defined_inst(InstName0),
        check_for_bad_use_of_user_inst(ModuleInfo, InstName0, Type, Context,
            MaybeUserInstInfo, !Errors),
        ( if InstName0 = typed_inst(_, _) then
            % If this happens, it means that we have already lazily propagated
            % type info into this inst. We want to avoid creating insts of
            % the form typed_inst(_, typed_inst(...)), because that would be
            % unnecessary, and could cause efficiency problems or perhaps
            % even infinite loops.
            Inst = Inst0
        else
            InstName = typed_inst(Type, InstName0),
            Inst = defined_inst(InstName),
            maybe_check_user_inst_args(ModuleInfo, Type, Context,
                MaybeUserInstInfo, !Errors)
        )
    ).

:- pred maybe_check_user_inst_args(module_info::in, mer_type::in,
    Context::in, maybe(user_inst_info)::in,
    Errors::in, Errors::out) is det <= tprop_record(Context, Args, Errors).
:- pragma type_spec(pred(maybe_check_user_inst_args/6),
    (Context = tprop_context, Args = tprop_args, Errors = tprop_errors)).
:- pragma type_spec(pred(maybe_check_user_inst_args/6),
    (Context = unit, Args = unit, Errors = unit)).

maybe_check_user_inst_args(ModuleInfo, Type, Context, MaybeUserInstInfo,
        !Errors) :-
    (
        MaybeUserInstInfo = no
    ;
        MaybeUserInstInfo = yes(UserInstInfo),
        UserInstInfo = user_inst_info(SymNameArity, InstArgs, InstDefn),
        InstDefn = hlds_inst_defn(_VarSet, Params, InstBody,
            _IFTC, _Context, _Status),
        InstBody = eqv_inst(Inst0),
        inst_substitute_arg_list(Params, InstArgs, Inst0, Inst1),
        ( if
            user_inst_expansion_to_context(Context, SymNameArity, SubContext)
        then
            propagate_type_into_inst(ModuleInfo, SubContext,
                Type, Inst1, _Inst, !Errors)
        else
            true
        )
    ).

%---------------------%

    % If the user does not explicitly specify a higher-order inst
    % for a higher-order function type, it defaults to
    % `func(in, in, ..., in) = out is det',
    % i.e. all args input, return value output, and det.
    % This applies recursively to the arguments and return
    % value too.
    %
:- pred default_higher_order_func_inst(module_info::in, Context::in,
    list(mer_type)::in, pred_inst_info::out,
    Errors::in, Errors::out) is det <= tprop_record(Context, Args, Errors).
:- pragma type_spec(pred(default_higher_order_func_inst/6),
    (Context = tprop_context, Args = tprop_args, Errors = tprop_errors)).
:- pragma type_spec(pred(default_higher_order_func_inst/6),
    (Context = unit, Args = unit, Errors = unit)).

default_higher_order_func_inst(ModuleInfo, Context, PredArgTypes, PredInstInfo,
        !Errors) :-
    Ground = ground(shared, none_or_default_func),
    In = from_to_mode(Ground, Ground),
    Out = from_to_mode(free, Ground),
    list.length(PredArgTypes, NumPredArgs),
    NumFuncArgs = NumPredArgs - 1,
    list.duplicate(NumFuncArgs, In, FuncArgModes),
    FuncRetMode = Out,
    PredArgModes0 = FuncArgModes ++ [FuncRetMode],
    step_into_ho_inst(pf_function, NumPredArgs, Context, Args),
    propagate_types_into_modes(ModuleInfo, Args, 1, PredArgTypes,
        PredArgModes0, PredArgModes, !Errors),
    PredInstInfo = pred_inst_info(pf_function, PredArgModes,
        arg_reg_types_unset, detism_det).

%---------------------------------------------------------------------------%

propagate_unchecked_type_into_bound_inst(ModuleInfo, Type, Inst0, Inst) :-
    propagate_type_into_bound_inst(ModuleInfo, unit, Type, Inst0, Inst,
        unit, _).

:- pred propagate_type_into_bound_inst(module_info::in, Context::in,
    mer_type::in, mer_inst::in(mer_inst_is_bound), mer_inst::out,
    Errors::in, Errors::out) is det <= tprop_record(Context, Args, Errors).
:- pragma type_spec(pred(propagate_type_into_bound_inst/7),
    (Context = tprop_context, Args = tprop_args, Errors = tprop_errors)).
:- pragma type_spec(pred(propagate_type_into_bound_inst/7),
    (Context = unit, Args = unit, Errors = unit)).

propagate_type_into_bound_inst(ModuleInfo, Context, Type, Inst0, Inst,
        !Errors) :-
    Inst0 = bound(Uniq, InstResults0, BoundInsts0),
    % Test for the most frequent kinds of type_ctors first.
    % XXX Replace with a switch on Type.
    (
        Type = builtin_type(BuiltinType),
        (
            BuiltinType = builtin_type_char,
            % There is no need to sort BoundInsts; if BoundInsts0 is sorted,
            % which it should be, then BoundInsts will be sorted too.
            list.map(propagate_char_type, BoundInsts0, BoundInsts),
            % Tuples don't have a *conventional* type_ctor.
            % XXX Tuples?
            PropagatedResult = inst_result_no_type_ctor_propagated,
            construct_new_bound_inst(Uniq, InstResults0, PropagatedResult,
                BoundInsts, Inst)
        ;
            ( BuiltinType = builtin_type_int(_)
            ; BuiltinType = builtin_type_float
            ; BuiltinType = builtin_type_string
            ),
            % Builtin types other than char have no info to propagate
            % into an inst.
            Inst = Inst0
        )
    ;
        Type = tuple_type(ArgTypes, _Kind),
        % There is no need to sort BoundInsts; if BoundInsts0 is sorted,
        % which it should be, then BoundInsts will be sorted too.
        list.map_foldl(
            propagate_types_into_tuple(ModuleInfo, Context, ArgTypes),
            BoundInsts0, BoundInsts, !Errors),
        % Tuples don't have a *conventional* type_ctor.
        PropagatedResult = inst_result_no_type_ctor_propagated,
        construct_new_bound_inst(Uniq, InstResults0, PropagatedResult,
            BoundInsts, Inst)
    ;
        ( Type = type_variable(_, _)
        ; Type = higher_order_type(_, _, _, _, _)
        ; Type = apply_n_type(_, _, _)
        ),
        % Type variables have no info to propagate into an inst.
        % Higher order types may have some, but we have not traditionally
        % propagated them.
        Inst = Inst0
    ;
        Type = defined_type(SymName, ArgTypes, _Kind),
        ( if
            SymName = qualified(TypeModule, _),
            module_info_get_type_table(ModuleInfo, TypeTable),
            TypeCtor = type_ctor(SymName, list.length(ArgTypes)),
            search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
            hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
            hlds_data.get_type_defn_body(TypeDefn, TypeBody),
            TypeBody = hlds_du_type(TypeBodyDu)
        then
            ( if
                InstResults0 = inst_test_results(_, _, _, _, _,
                    PropagatedResult0),
                PropagatedResult0 =
                    inst_result_type_ctor_propagated(PropagatedTypeCtor0),
                PropagatedTypeCtor0 = TypeCtor,
                TypeParams = []
            then
                % The job has already been done.
                Inst = Inst0
            else
                map.from_corresponding_lists(TypeParams, ArgTypes, ArgSubst),
                OoMConstructors = TypeBodyDu ^ du_type_ctors,
                Constructors = one_or_more_to_list(OoMConstructors),
                propagate_subst_type_ctor_into_bound_insts(ModuleInfo, Context,
                    ArgSubst, TypeCtor, TypeModule, Constructors,
                    BoundInsts0, BoundInsts1, !Errors),
                list.sort(BoundInsts1, BoundInsts),
                PropagatedResult = inst_result_type_ctor_propagated(TypeCtor),
                construct_new_bound_inst(Uniq, InstResults0, PropagatedResult,
                    BoundInsts, Inst)
            )
        else
            % Type variables have no info to propagate into an inst.
            % Higher order types may have some, but we have not traditionally
            % propagated them.
            Inst = Inst0
        )
    ;
        Type = kinded_type(KindedType, _Kind),
        propagate_type_into_bound_inst(ModuleInfo, Context, KindedType,
            Inst0, Inst, !Errors)
    ).

:- pred construct_new_bound_inst(uniqueness::in, inst_test_results::in,
    inst_result_type_ctor_propagated::in, list(bound_inst)::in,
    mer_inst::out) is det.

construct_new_bound_inst(Uniq, InstResults0, PropagatedResult, BoundInsts,
        Inst) :-
    (
        BoundInsts = [],
        Inst = not_reached
    ;
        BoundInsts = [_ | _],
        (
            InstResults0 = inst_test_results_fgtc,
            InstResults = InstResults0
        ;
            InstResults0 = inst_test_no_results,
            InstResults = inst_test_results(inst_result_groundness_unknown,
                inst_result_contains_any_unknown,
                inst_result_contains_inst_names_unknown,
                inst_result_contains_inst_vars_unknown,
                inst_result_contains_types_unknown, PropagatedResult)
        ;
            InstResults0 = inst_test_results(GroundNessResult0,
                ContainsAnyResult, _, _, _, _),
            % XXX I (zs) don't understand the predicate
            % propagate_type_ctor_into_bound_inst
            % well enough to figure out under what circumstances we could
            % keep the parts of InstResult0 we are clobbering here.
            InstResults = inst_test_results(GroundNessResult0,
                ContainsAnyResult, inst_result_contains_inst_names_unknown,
                inst_result_contains_inst_vars_unknown,
                inst_result_contains_types_unknown, PropagatedResult)
        ),
        % We shouldn't need to sort BoundInsts. The cons_ids in the
        % bound_insts in the list should have been either all typed
        % or all non-typed. If they were all typed, then pushing the
        % type_ctor into them should not have modified them. If they
        % were all non-typed, then pushing the same type_ctor into them all
        % should not have changed their order.
        Inst = bound(Uniq, InstResults, BoundInsts)
    ).

:- pred propagate_types_into_tuple(module_info::in, Context::in,
    list(mer_type)::in, bound_inst::in, bound_inst::out,
    Errors::in, Errors::out) is det <= tprop_record(Context, Args, Errors).
:- pragma type_spec(pred(propagate_types_into_tuple/7),
    (Context = tprop_context, Args = tprop_args, Errors = tprop_errors)).
:- pragma type_spec(pred(propagate_types_into_tuple/7),
    (Context = unit, Args = unit, Errors = unit)).

propagate_types_into_tuple(ModuleInfo, Context, TupleArgTypes,
        BoundInst0, BoundInst, !Errors) :-
    BoundInst0 = bound_functor(Functor, ArgInsts0),
    ( if
        ( Functor = tuple_cons(_)
        ; Functor = cons(unqualified("{}"), _, _)
        ),
        list.length(ArgInsts0, ArgInstsLen),
        list.length(TupleArgTypes, TupleArgTypesLen),
        ArgInstsLen = TupleArgTypesLen
    then
        step_into_tuple_inst(Context, Args),
        propagate_types_into_insts(ModuleInfo, Args, 1,
            TupleArgTypes, ArgInsts0, ArgInsts, !Errors)
    else
        % The bound_inst's arity does not match the tuple's arity, so leave it
        % alone. This can only happen in a user defined bound_inst.
        % A mode error should be reported if anything tries to match with
        % the inst.
        ArgInsts = ArgInsts0
    ),
    BoundInst = bound_functor(Functor, ArgInsts).

:- pred propagate_char_type(bound_inst::in, bound_inst::out) is det.

propagate_char_type(BoundInst0, BoundInst) :-
    BoundInst0 = bound_functor(Functor0, ArgInsts0),
    ( if
        Functor0 = cons(unqualified(Name), 0, _),
        string.to_char_list(Name, NameChars),
        NameChars = [NameChar],
        ArgInsts0 = []
    then
        Functor = char_const(NameChar),
        BoundInst = bound_functor(Functor, [])
    else
        % The bound_inst is not a valid one for chars, so leave it alone.
        % This can only happen in a user defined bound_inst.
        % A mode error should be reported if anything tries to match with
        % the inst.
        BoundInst = BoundInst0
    ).

:- pred propagate_subst_type_ctor_into_bound_insts(module_info::in,
    Context::in, tsubst::in, type_ctor::in, module_name::in,
    list(constructor)::in, list(bound_inst)::in, list(bound_inst)::out,
    Errors::in, Errors::out) is det <= tprop_record(Context, Args, Errors).
:- pragma type_spec(pred(propagate_subst_type_ctor_into_bound_insts/10),
    (Context = tprop_context, Args = tprop_args, Errors = tprop_errors)).
:- pragma type_spec(pred(propagate_subst_type_ctor_into_bound_insts/10),
    (Context = unit, Args = unit, Errors = unit)).

propagate_subst_type_ctor_into_bound_insts(_, _, _, _, _, _, [], [], !Errors).
propagate_subst_type_ctor_into_bound_insts(ModuleInfo, Context, Subst,
        TypeCtor, TypeModule, Constructors,
        [BoundInst0 | BoundInsts0], [BoundInst | BoundInsts], !Errors) :-
    BoundInst0 = bound_functor(ConsId0, ArgInsts0),
    ( if ConsId0 = cons(unqualified(Name), ConsArity, _ConsTypeCtor) then
        % _ConsTypeCtor should be either TypeCtor or cons_id_dummy_type_ctor.
        ConsId = cons(qualified(TypeModule, Name), ConsArity, TypeCtor)
    else
        ConsId = ConsId0
    ),
    ( if
        ConsId = cons(ConsName, Arity, _),
        find_first_matching_constructor(ConsName, Arity, Constructors,
            MatchingConstructor)
    then
        MatchingConstructor = ctor(_Ordinal, _MaybeExistConstraints,
            _Name, CtorArgs, _Arity, _Ctxt),
        get_constructor_arg_types(CtorArgs, ArgTypes),
        step_into_bound_inst(ConsId, Context, Args),
        propagate_subst_types_into_insts(ModuleInfo, Args, 1, Subst,
            ArgTypes, ArgInsts0, ArgInsts, !Errors),
        BoundInst = bound_functor(ConsId, ArgInsts)
    else
        % The cons_id is not a valid constructor for the type,
        % so leave it alone. This can only happen in a user defined
        % bound_inst. A mode error should be reported if anything
        % tries to match with the inst.
        BoundInst = bound_functor(ConsId, ArgInsts0)
    ),
    propagate_subst_type_ctor_into_bound_insts(ModuleInfo, Context, Subst,
        TypeCtor, TypeModule, Constructors, BoundInsts0, BoundInsts, !Errors).

    % Find the first constructor in the egiven list of constructors
    % that match the given functor name and arity. Since the constructors
    % should all come from the same type definition, there should be
    % at most one matching constructor in the list anyway.
    %
:- pred find_first_matching_constructor(sym_name::in, arity::in,
    list(constructor)::in, constructor::out) is semidet.

find_first_matching_constructor(_ConsName, _Arity, [], _MatchingCtor) :-
    fail.
find_first_matching_constructor(ConsName, Arity, [Ctor | Ctors],
        MatchingCtor) :-
    ( if Ctor = ctor(_, _, ConsName, _, Arity, _) then
        MatchingCtor = Ctor
    else
        find_first_matching_constructor(ConsName, Arity, Ctors, MatchingCtor)
    ).

:- pred get_constructor_arg_types(list(constructor_arg)::in,
    list(mer_type)::out) is det.

get_constructor_arg_types([], []).
get_constructor_arg_types([Arg | Args], [ArgType | ArgTypes]) :-
    ArgType = Arg ^ arg_type,
    get_constructor_arg_types(Args, ArgTypes).

%---------------------------------------------------------------------------%

propagate_checked_types_into_modes(ModuleInfo, Args,
        Types, Modes0, Modes, !Specs) :-
    Errors0 = tprop_errors(!.Specs),
    propagate_types_into_modes(ModuleInfo, Args, 1,
        Types, Modes0, Modes, Errors0, Errors),
    Errors = tprop_errors(!:Specs).

propagate_checked_type_into_mode(ModuleInfo, Context,
        Type, Mode0, Mode, !Specs) :-
    Errors0 = tprop_errors(!.Specs),
    propagate_type_into_mode(ModuleInfo, Context,
        Type, Mode0, Mode, Errors0, Errors),
    Errors = tprop_errors(!:Specs).

%---------------------%

    % Given corresponding lists of types and modes, produce a new list
    % of modes which includes the information provided by the
    % corresponding types.
    %
:- pred propagate_types_into_modes(module_info::in, Args::in, int::in,
    list(mer_type)::in, list(mer_mode)::in, list(mer_mode)::out,
    Errors::in, Errors::out) is det <= tprop_record(Context, Args, Errors).
:- pragma type_spec(pred(propagate_types_into_modes/8),
    (Context = tprop_context, Args = tprop_args, Errors = tprop_errors)).
:- pragma type_spec(pred(propagate_types_into_modes/8),
    (Context = unit, Args = unit, Errors = unit)).

propagate_types_into_modes(_, _, _, [], [], [], !Errors).
propagate_types_into_modes(_, _, _, [], [_ | _], [], !Errors) :-
    unexpected($pred, "length mismatch").
propagate_types_into_modes(_, _, _, [_ | _], [], [], !Errors) :-
    unexpected($pred, "length mismatch").
propagate_types_into_modes(ModuleInfo, Args, ArgNum, [Type | Types],
        [Mode0 | Modes0], [Mode | Modes], !Errors) :-
    args_slot_to_context(Args, ArgNum, Context),
    propagate_type_into_mode(ModuleInfo, Context, Type, Mode0, Mode, !Errors),
    propagate_types_into_modes(ModuleInfo, Args, ArgNum + 1,
        Types, Modes0, Modes, !Errors).

    % Given a type and a mode, produce a new mode that includes the
    % information provided by the type.
    %
:- pred propagate_type_into_mode(module_info::in, Context::in,
    mer_type::in, mer_mode::in, mer_mode::out,
    Errors::in, Errors::out) is det <= tprop_record(Context, Args, Errors).
:- pragma type_spec(pred(propagate_type_into_mode/7),
    (Context = tprop_context, Args = tprop_args, Errors = tprop_errors)).
:- pragma type_spec(pred(propagate_type_into_mode/7),
    (Context = unit, Args = unit, Errors = unit)).

propagate_type_into_mode(ModuleInfo, Context, Type, Mode0, Mode, !Errors) :-
    mode_get_insts(ModuleInfo, Mode0, InitialInst0, FinalInst0),
    propagate_type_into_inst_lazily(ModuleInfo, Context, Type,
        InitialInst0, InitialInst, !Errors),
    propagate_type_into_inst_lazily(ModuleInfo, Context, Type,
        FinalInst0, FinalInst, !Errors),
    Mode = from_to_mode(InitialInst, FinalInst).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% Some of the exported predicates of this module want to generate error
% messages for applications of inst_names to type constructors differ
% from the "for type_ctor" annotation on that inst_name, but some do not.
% To avoid excessive performance impact on the latter, we make the checking
% optional through a typeclass with two instances, one of which prepares for
% and then does the checking, while the other replaces all those operations
% with no-ops.

:- typeclass tprop_record(Context, Args, Errors)
    <= ((Context -> Args, Errors), (Args -> Context, Errors))
where [
        % Return a representation of the argument list inside
        % the bound_inst of the given cons_id at the given context.
        %
    pred step_into_bound_inst(cons_id::in, Context::in, Args::out) is det,

        % Return a representation of the argument list inside
        % the tuple inst at the given context.
        %
    pred step_into_tuple_inst(Context::in, Args::out) is det,

        % Return a representation of the argument list inside
        % the higher order inst of the given pred_or_func and arity
        % at the given context.
        %
    pred step_into_ho_inst(pred_or_func::in, int::in,
        Context::in, Args::out) is det,

        % Return a representation of the N'th argument of the given
        % argument list.
        %
    pred args_slot_to_context(Args::in, int::in, Context::out) is det,

        % Record that we are stepping inside the expansion of the
        % user inst specified by the given sym_name_arity. Fail if
        % we were *already* inside its expansion, in order to avoid
        % infinite recursion in the presence of recursive user insts.
        %
    pred user_inst_expansion_to_context(Context::in, sym_name_arity::in,
        Context::out) is semidet,

        % check_for_bad_use_of_user_inst(ModuleInfo, InstName, Type,
        %   Context, MaybeUserInstInfo, !Errors):
        %
        % Check whether InstName is an user inst applies to zero or more
        % argument insts, and if so, return information about it in
        % MaybeUserInstInfo, for further checks the user inst's body.
        % Then at check whether using it at Context to describe a value
        % whose type is Type violates a "for type_ctor" annotation on InstName,
        % and if it does, add an error message for this violation to !Errors.
        %
    pred check_for_bad_use_of_user_inst(module_info::in, inst_name::in,
        mer_type::in, Context::in, maybe(user_inst_info)::out,
        Errors::in, Errors::out) is det
].

    % Values of this type hold the information needed to check
    % for applications of insts to values of inappropriate types
    % *inside* the application of a user inst name.
:- type user_inst_info
    --->    user_inst_info(
                sym_name_arity,
                % The name and arity of the user inst.

                list(mer_inst),
                % The arguments of that user inst.

                hlds_inst_defn
                % The definition of the user inst.
            ).

%---------------------%

    % This instance defines all operations as no-ops.
:- instance tprop_record(unit, unit, unit) where [
    (step_into_bound_inst(_, _, unit)),
    (step_into_tuple_inst(_, unit)),
    (step_into_ho_inst(_, _, _, unit)),
    (args_slot_to_context(_, _, unit)),
    (user_inst_expansion_to_context(_, _, _) :- fail),
    (check_for_bad_use_of_user_inst(_, _, _, _, no, Errors, Errors))
].

    % This instance does all the work.
    %
    % The third type here, tprop_errors, is just list(error_spec).
    % We need the separate type because each type in an instance
    % may contain only a single type constructor, and list(error_spec)
    % contains two.
:- instance tprop_record(tprop_context, tprop_args, tprop_errors) where [
    pred(step_into_bound_inst/3) is do_step_into_bound_inst,
    pred(step_into_tuple_inst/2) is do_step_into_tuple_inst,
    pred(step_into_ho_inst/4) is do_step_into_ho_inst,
    pred(args_slot_to_context/3) is do_args_slot_to_context,
    pred(user_inst_expansion_to_context/3)
        is do_user_inst_expansion_to_context,
    pred(check_for_bad_use_of_user_inst/7)
        is do_check_for_bad_use_of_user_inst
].

:- type tprop_errors
    --->    tprop_errors(list(error_spec)).

%---------------------%

:- pred do_step_into_bound_inst(cons_id::in,
    tprop_context::in, tprop_args::out) is det.

do_step_into_bound_inst(ConsId, Context, Args) :-
    Args = ta_bound_inst(ConsId, Context).

:- pred do_step_into_tuple_inst(tprop_context::in, tprop_args::out)
    is det.

do_step_into_tuple_inst(Context, Args) :-
    Args = ta_tuple_inst(Context).

:- pred do_step_into_ho_inst(pred_or_func::in, int::in,
    tprop_context::in, tprop_args::out) is det.

do_step_into_ho_inst(PredOrFunc, NumArgs, Context, Args) :-
    Args = ta_ho_inst(PredOrFunc, NumArgs, Context).

:- pred do_args_slot_to_context(tprop_args::in, int::in,
    tprop_context::out) is det.

do_args_slot_to_context(Args, ArgNum, Context) :-
    Context = tprop_arg_list_slot(Args, ArgNum).

:- pred do_user_inst_expansion_to_context(tprop_context::in,
    sym_name_arity::in, tprop_context::out) is semidet.

do_user_inst_expansion_to_context(Context, InstNameArity, SubContext) :-
    Inside = are_we_already_inside_user_inst_expansion(Context, InstNameArity),
    (
        Inside = inside_user_inst,
        fail
    ;
        Inside = not_inside_user_inst,
        SubContext = tprop_inst_name_expansion(InstNameArity, Context)
    ).

:- type maybe_inside_user_inst
    --->    not_inside_user_inst
    ;       inside_user_inst.

:- func are_we_already_inside_user_inst_expansion(tprop_context,
    sym_name_arity) = maybe_inside_user_inst.

are_we_already_inside_user_inst_expansion(Context, SymNameArity) = Inside :-
    (
        Context = tprop_arg_list_slot(Args, _ArgNum),
        (
            ( Args = ta_pred(_)
            ; Args = ta_lambda(_, _, _)
            ),
            Inside = not_inside_user_inst
        ;
            ( Args = ta_bound_inst(_, OuterContext)
            ; Args = ta_tuple_inst(OuterContext)
            ; Args = ta_ho_inst(_, _, OuterContext)
            ),
            Inside = are_we_already_inside_user_inst_expansion(OuterContext,
                SymNameArity)
        )
    ;
        Context = tprop_inst_name_expansion(InstNameArity, OuterContext),
        ( if SymNameArity = InstNameArity then
            Inside = inside_user_inst
        else
            Inside = are_we_already_inside_user_inst_expansion(OuterContext,
                SymNameArity)
        )
    ).

%---------------------%

:- pred do_check_for_bad_use_of_user_inst(module_info::in, inst_name::in,
    mer_type::in, tprop_context::in, maybe(user_inst_info)::out,
    tprop_errors::in, tprop_errors::out) is det.

do_check_for_bad_use_of_user_inst(ModuleInfo, InstName, Type,
        TPropContext, MaybeUserInstInfo, !Errors) :-
    (
        ( InstName = unify_inst(_, _, _, _)
        ; InstName = merge_inst(_, _)
        ; InstName = ground_inst(_, _, _, _)
        ; InstName = any_inst(_, _, _, _)
        ; InstName = shared_inst(_)
        ; InstName = mostly_uniq_inst(_)
        ; InstName = typed_ground(_, _)
        ; InstName = typed_inst(_, _)
        ),
        MaybeUserInstInfo = no
    ;
        InstName = user_inst(SymName, ArgInsts),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_user_insts(InstTable, UserInstTable),
        list.length(ArgInsts, Arity),
        InstCtor = inst_ctor(SymName, Arity),
        ( if map.search(UserInstTable, InstCtor, InstDefn) then
            SymNameArity = sym_name_arity(SymName, Arity),
            UserInstInfo = user_inst_info(SymNameArity, ArgInsts, InstDefn),
            ( if 
                MaybeInstForTypeCtor = InstDefn ^ inst_for_type,
                MaybeInstForTypeCtor =
                    iftc_applicable_declared(InstForTypeCtor),
                type_to_ctor(Type, TypeCtor),
                TypeCtor \= InstForTypeCtor
            then
                do_record_bad_use_of_user_inst(InstCtor, InstForTypeCtor,
                    TypeCtor, TPropContext, !Errors),
                % We have found an error in this application of InstName.
                % Since we return a meaningful MaybeUserInstInfo here,
                % our caller will also check whether InstName's argument insts
                % are similarly applied to values of inappropriate types.
                % The question is: would users find this extra error report
                % useful, or would they find it a distraction? We won't know
                % unless we try it. If it turns out to be a distraction,
                % we can change this to MaybeUserInstInfo = no.
                MaybeUserInstInfo = yes(UserInstInfo)
            else
                MaybeUserInstInfo = yes(UserInstInfo)
            )
        else
            MaybeUserInstInfo = no
        )
    ).

:- pred do_record_bad_use_of_user_inst(inst_ctor::in,
    type_ctor::in, type_ctor::in, tprop_context::in,
    tprop_errors::in, tprop_errors::out) is det.

do_record_bad_use_of_user_inst(InstCtor, ForTypeCtor, TypeCtor,
        TPropContext, !Errors) :-
    !.Errors = tprop_errors(Specs0),
    tprop_context_to_pieces(TPropContext, Context, TPropContextPieces),
    Pieces = TPropContextPieces ++
        [words("error: the user defined inst"),
        nl_indent_delta(1),
        qual_inst_ctor(InstCtor),
        nl_indent_delta(-1),
        words("is declared to be applicable only to values of type"),
        nl_indent_delta(1),
        qual_type_ctor(ForTypeCtor), suffix(","),
        nl_indent_delta(-1),
        words("but here it is applied to values of type"),
        nl_indent_delta(1),
        qual_type_ctor(TypeCtor), suffix("."),
        nl_indent_delta(-1)],
    Spec = simplest_spec($pred, severity_error, phase_inst_check,
        Context, Pieces),
    Specs = [Spec | Specs0],
    !:Errors = tprop_errors(Specs).

:- pred tprop_context_to_pieces(tprop_context::in,
    prog_context::out, list(format_piece)::out) is det.

tprop_context_to_pieces(TPropContext, Context, Pieces) :-
    (
        TPropContext = tprop_arg_list_slot(Args, ArgNum),
        (
            (
                Args = ta_pred(PredInfo),
                pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
                pred_info_get_orig_arity(PredInfo, NumArgs),
                pred_info_get_context(PredInfo, Context),
                PredPieces = describe_one_pred_info_name(
                    should_not_module_qualify, PredInfo)
            ;
                Args = ta_lambda(PredOrFunc, NumArgs, Context),
                PredPieces = [words("the lambda expression")]
            ),
            ArgDescPieces = pred_or_func_arg_desc(PredOrFunc, NumArgs, ArgNum),
            Pieces = [words("In")] ++ ArgDescPieces ++ [words("of")] ++
                PredPieces ++ [suffix(":"), nl]
        ;
            (
                Args = ta_bound_inst(ConsId, OuterTPropContext),
                InnerPieces =
                    [words("in the"), nth_fixed(ArgNum), words("argument of"),
                    unqual_cons_id_and_maybe_arity(ConsId), suffix(":"), nl]
            ;
                Args = ta_tuple_inst(OuterTPropContext),
                InnerPieces =
                    [words("in the"), nth_fixed(ArgNum),
                    words("argument of tuple:"), nl]
            ;
                Args = ta_ho_inst(PredOrFunc, NumArgs, OuterTPropContext),
                ArgDescPieces =
                    pred_or_func_arg_desc(PredOrFunc, NumArgs, ArgNum),
                InnerPieces =
                    [words("in")] ++ ArgDescPieces ++
                    [words("of the higher order inst:"), nl]
            ),
            tprop_context_to_pieces(OuterTPropContext, Context, OuterPieces),
            Pieces = OuterPieces ++ InnerPieces
        )
    ;
        TPropContext =
            tprop_inst_name_expansion(InstNameArity, OuterTPropContext),
        tprop_context_to_pieces(OuterTPropContext, Context, OuterPieces),
        InnerPieces =
            [words("in the expansion of the user-defined inst"),
            qual_sym_name_arity(InstNameArity), suffix(":"), nl],
        Pieces = OuterPieces ++ InnerPieces
    ).

:- func pred_or_func_arg_desc(pred_or_func, int, int)
    = list(format_piece).

pred_or_func_arg_desc(PredOrFunc, NumArgs, ArgNum) = Pieces :-
    ( if
        PredOrFunc = pf_function,
        ArgNum = NumArgs
    then
        Pieces = [words("the result argument")]
    else
        Pieces = [words("the"), nth_fixed(ArgNum), words("argument")]
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.inst_mode_type_prop.
%---------------------------------------------------------------------------%
