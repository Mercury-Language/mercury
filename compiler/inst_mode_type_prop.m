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
:- import_module parse_tree.
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
    % XXX inst_lookup
    % redundant traversals
    %
:- pred propagate_type_into_inst(module_info::in, mer_type::in,
    mer_inst::in, mer_inst::out) is det.

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
:- pred propagate_type_into_bound_inst(module_info::in, mer_type::in,
    mer_inst::in(mer_inst_is_bound), mer_inst::out) is det.

%---------------------------------------------------------------------------%
%
% Propagating type information into modes.
%

    % Given corresponding lists of types and modes, produce a new list
    % of modes which includes the information provided by the
    % corresponding types.
    %
:- pred propagate_types_into_modes(module_info::in, list(mer_type)::in,
    list(mer_mode)::in, list(mer_mode)::out) is det.

    % Given a type and a mode, produce a new mode that includes the
    % information provided by the type.
    %
:- pred propagate_type_into_mode(module_info::in, mer_type::in,
    mer_mode::in, mer_mode::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_lookup.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
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

%---------------------------------------------------------------------------%

    % Given bindings for type variables, and corresponding lists of types
    % and insts, produce a new list of insts which includes the information
    % provided by the corresponding types.
    %
:- pred propagate_subst_types_into_insts(module_info::in, tsubst::in,
    list(mer_type)::in, list(mer_inst)::in, list(mer_inst)::out) is det.

propagate_subst_types_into_insts(_, _, [], [], []).
propagate_subst_types_into_insts(_, _, [], [_ | _], []) :-
    unexpected($pred, "length mismatch").
propagate_subst_types_into_insts(_, _, [_ | _], [], []) :-
    unexpected($pred, "length mismatch").
propagate_subst_types_into_insts(ModuleInfo, Subst, [Type | Types],
        [Inst0 | Insts0], [Inst | Insts]) :-
    propagate_subst_type_into_inst(ModuleInfo, Subst, Type, Inst0, Inst),
    propagate_subst_types_into_insts(ModuleInfo, Subst, Types, Insts0, Insts).

%---------------------%

:- pred propagate_subst_type_into_inst(module_info::in, tsubst::in,
    mer_type::in, mer_inst::in, mer_inst::out) is det.

propagate_subst_type_into_inst(ModuleInfo, Subst, Type0, Inst0, Inst) :-
    apply_type_subst(Type0, Subst, Type),
    ( if semidet_fail then
        % XXX We ought to expand things eagerly here, using this code.
        % However, that causes efficiency problems, so for the moment
        % we always do propagation lazily.
        ( if type_constructors(ModuleInfo, Type, Constructors) then
            propagate_type_into_inst_eagerly(ModuleInfo, Type, Constructors,
                Inst0, Inst)
        else
            Inst = Inst0
        )
    else
        propagate_type_into_inst_lazily(ModuleInfo, Type, Inst0, Inst)
    ).

propagate_type_into_inst(ModuleInfo, Type, Inst0, Inst) :-
    ( if semidet_fail then
        % XXX We ought to expand things eagerly here, using this code.
        % However, that causes efficiency problems, so for the moment
        % we always do propagation lazily.
        ( if type_constructors(ModuleInfo, Type, Constructors) then
            propagate_type_into_inst_eagerly(ModuleInfo, Type, Constructors,
                Inst0, Inst)
        else
            Inst = Inst0
        )
    else
        propagate_type_into_inst_lazily(ModuleInfo, Type, Inst0, Inst)
    ).

%---------------------%

:- pred propagate_type_into_inst_eagerly(module_info::in, mer_type::in,
    list(constructor)::in, mer_inst::in, mer_inst::out) is det.

propagate_type_into_inst_eagerly(ModuleInfo, Type, Constructors, Inst0, Inst) :-
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
                default_higher_order_func_inst(ModuleInfo, ArgTypes,
                    HigherOrderInstInfo),
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
                propagate_types_into_modes(ModuleInfo, ArgTypes, Modes0, Modes)
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
                default_higher_order_func_inst(ModuleInfo, ArgTypes,
                    PredInstInfo),
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
                propagate_types_into_modes(ModuleInfo, ArgTypes, Modes0, Modes)
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
        propagate_type_into_bound_inst(ModuleInfo, Type, Inst0, Inst)
    ;
        Inst0 = not_reached,
        Inst = Inst0
    ;
        Inst0 = inst_var(_),
        Inst = Inst0
    ;
        Inst0 = constrained_inst_vars(InstVars, SubInst0),
        propagate_type_into_inst_eagerly(ModuleInfo, Type, Constructors,
            SubInst0, SubInst),
        Inst = constrained_inst_vars(InstVars, SubInst)
    ;
        Inst0 = abstract_inst(_Name, _Args),
        Inst = Inst0                        % XXX loses info
    ;
        Inst0 = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NamedInst),
        propagate_type_into_inst_eagerly(ModuleInfo, Type, Constructors,
            NamedInst, Inst)
    ).

:- pred propagate_type_into_inst_lazily(module_info::in, mer_type::in,
    mer_inst::in, mer_inst::out) is det.

propagate_type_into_inst_lazily(ModuleInfo, Type, Inst0, Inst) :-
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
                default_higher_order_func_inst(ModuleInfo, ArgTypes,
                    HOInstInfo),
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
                propagate_types_into_modes(ModuleInfo, ArgTypes, Modes0, Modes)
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
                default_higher_order_func_inst(ModuleInfo, ArgTypes,
                    HOInstInfo),
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
                propagate_types_into_modes(ModuleInfo, ArgTypes, Modes0, Modes)
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
        propagate_type_into_bound_inst(ModuleInfo, Type, Inst0, Inst)
    ;
        Inst0 = not_reached,
        Inst = Inst0
    ;
        Inst0 = inst_var(_),
        Inst = Inst0
    ;
        Inst0 = constrained_inst_vars(InstVars, SubInst0),
        propagate_type_into_inst_lazily(ModuleInfo, Type, SubInst0, SubInst),
        Inst = constrained_inst_vars(InstVars, SubInst)
    ;
        Inst0 = abstract_inst(_Name, _Args),
        Inst = Inst0                        % XXX loses info
    ;
        Inst0 = defined_inst(InstName0),
        ( if InstName0 = typed_inst(_, _) then
            % If this happens, it means that we have already lazily propagated
            % type info into this inst. We want to avoid creating insts of
            % the form typed_inst(_, typed_inst(...)), because that would be
            % unnecessary, and could cause efficiency problems or perhaps
            % even infinite loops.
            Inst = Inst0
        else
            InstName = typed_inst(Type, InstName0),
            Inst = defined_inst(InstName)
        )
    ).

:- pred apply_type_subst(mer_type::in, tsubst::in, mer_type::out) is det.

apply_type_subst(Type0, Subst, Type) :-
    % Optimize common case.
    ( if map.is_empty(Subst) then
        Type = Type0
    else
        apply_subst_to_type(Subst, Type0, Type)
    ).

    % If the user does not explicitly specify a higher-order inst
    % for a higher-order function type, it defaults to
    % `func(in, in, ..., in) = out is det',
    % i.e. all args input, return value output, and det.
    % This applies recursively to the arguments and return
    % value too.
    %
:- pred default_higher_order_func_inst(module_info::in, list(mer_type)::in,
    pred_inst_info::out) is det.

default_higher_order_func_inst(ModuleInfo, PredArgTypes, PredInstInfo) :-
    Ground = ground(shared, none_or_default_func),
    In = from_to_mode(Ground, Ground),
    Out = from_to_mode(free, Ground),
    list.length(PredArgTypes, NumPredArgs),
    NumFuncArgs = NumPredArgs - 1,
    list.duplicate(NumFuncArgs, In, FuncArgModes),
    FuncRetMode = Out,
    list.append(FuncArgModes, [FuncRetMode], PredArgModes0),
    propagate_types_into_modes(ModuleInfo, PredArgTypes,
        PredArgModes0, PredArgModes),
    PredInstInfo = pred_inst_info(pf_function, PredArgModes,
        arg_reg_types_unset, detism_det).

%---------------------------------------------------------------------------%

propagate_type_into_bound_inst(ModuleInfo, Type, Inst0, Inst) :-
    Inst0 = bound(Uniq, InstResults0, BoundInsts0),
    % Test for the most frequent kinds of type_ctors first.
    % XXX Replace with a switch on Type.
    ( if
        type_to_ctor_and_args(Type, TypeCtor, TypeArgs),
        TypeCtor = type_ctor(qualified(TypeModule, _), _),
        module_info_get_type_table(ModuleInfo, TypeTable),
        search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        TypeBody = hlds_du_type(TypeBodyDu),
        OoMConstructors = TypeBodyDu ^ du_type_ctors
    then
        ( if
            InstResults0 = inst_test_results(_, _, _, _, _, PropagatedResult0),
            PropagatedResult0 =
                inst_result_type_ctor_propagated(PropagatedTypeCtor0),
            PropagatedTypeCtor0 = TypeCtor,
            TypeParams = []
        then
            % The job has already been done.
            Inst = Inst0
        else
            map.from_corresponding_lists(TypeParams, TypeArgs, ArgSubst),
            Constructors = one_or_more_to_list(OoMConstructors),
            propagate_subst_type_ctor_into_bound_insts(ModuleInfo,
                ArgSubst, TypeCtor, TypeModule, Constructors,
                BoundInsts0, BoundInsts1),
            list.sort(BoundInsts1, BoundInsts),
            PropagatedResult = inst_result_type_ctor_propagated(TypeCtor),
            construct_new_bound_inst(Uniq, InstResults0, PropagatedResult,
                BoundInsts, Inst)
        )
    else if
        type_is_tuple(Type, TupleArgTypes)
    then
        % There is no need to sort BoundInsts; if BoundInsts0 is sorted,
        % which it should be, then BoundInsts will be sorted too.
        list.map(propagate_types_into_tuple(ModuleInfo, TupleArgTypes),
            BoundInsts0, BoundInsts),
        % Tuples don't have a *conventional* type_ctor.
        PropagatedResult = inst_result_no_type_ctor_propagated,
        construct_new_bound_inst(Uniq, InstResults0, PropagatedResult,
            BoundInsts, Inst)
    else if
        Type = builtin_type(builtin_type_char)
    then
        % There is no need to sort BoundInsts; if BoundInsts0 is sorted,
        % which it should be, then BoundInsts will be sorted too.
        list.map(propagate_char_type, BoundInsts0, BoundInsts),
        % Tuples don't have a *conventional* type_ctor.
        PropagatedResult = inst_result_no_type_ctor_propagated,
        construct_new_bound_inst(Uniq, InstResults0, PropagatedResult,
            BoundInsts, Inst)
    else
        % Type variables, and builtin types other than char,
        % don't need processing.
        Inst = Inst0
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

:- pred propagate_types_into_tuple(module_info::in, list(mer_type)::in,
    bound_inst::in, bound_inst::out) is det.

propagate_types_into_tuple(ModuleInfo, TupleArgTypes, BoundInst0, BoundInst) :-
    BoundInst0 = bound_functor(Functor, ArgInsts0),
    ( if
        Functor = tuple_cons(_),
        list.length(ArgInsts0, ArgInstsLen),
        list.length(TupleArgTypes, TupleArgTypesLen),
        ArgInstsLen = TupleArgTypesLen
    then
        map.init(Subst),
        propagate_subst_types_into_insts(ModuleInfo, Subst, TupleArgTypes,
            ArgInsts0, ArgInsts)
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
    tsubst::in, type_ctor::in, module_name::in, list(constructor)::in,
    list(bound_inst)::in, list(bound_inst)::out) is det.

propagate_subst_type_ctor_into_bound_insts(_, _, _, _, _, [], []).
propagate_subst_type_ctor_into_bound_insts(ModuleInfo, Subst,
        TypeCtor, TypeModule, Constructors,
        [BoundInst0 | BoundInsts0], [BoundInst | BoundInsts]) :-
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
            _Name, Args, _Arity, _Ctxt),
        get_constructor_arg_types(Args, ArgTypes),
        propagate_subst_types_into_insts(ModuleInfo, Subst, ArgTypes,
            ArgInsts0, ArgInsts),
        BoundInst = bound_functor(ConsId, ArgInsts)
    else
        % The cons_id is not a valid constructor for the type,
        % so leave it alone. This can only happen in a user defined
        % bound_inst. A mode error should be reported if anything
        % tries to match with the inst.
        BoundInst = bound_functor(ConsId, ArgInsts0)
    ),
    propagate_subst_type_ctor_into_bound_insts(ModuleInfo, Subst,
        TypeCtor, TypeModule, Constructors, BoundInsts0, BoundInsts).

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

propagate_types_into_modes(_, [], [], []).
propagate_types_into_modes(_, [], [_ | _], []) :-
    unexpected($pred, "length mismatch").
propagate_types_into_modes(_, [_ | _], [], []) :-
    unexpected($pred, "length mismatch").
propagate_types_into_modes(ModuleInfo, [Type | Types],
        [Mode0 | Modes0], [Mode | Modes]) :-
    propagate_type_into_mode(ModuleInfo, Type, Mode0, Mode),
    propagate_types_into_modes(ModuleInfo, Types, Modes0, Modes).

propagate_type_into_mode(ModuleInfo, Type, Mode0, Mode) :-
    mode_get_insts(ModuleInfo, Mode0, InitialInst0, FinalInst0),
    propagate_type_into_inst_lazily(ModuleInfo, Type,
        InitialInst0, InitialInst),
    propagate_type_into_inst_lazily(ModuleInfo, Type,
        FinalInst0, FinalInst),
    Mode = from_to_mode(InitialInst, FinalInst).

%---------------------------------------------------------------------------%
:- end_module check_hlds.inst_mode_type_prop.
%---------------------------------------------------------------------------%
