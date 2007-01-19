%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: typecheck_info.m.
% Main author: fjh.
%
% This module defines the typecheck_info and type_assign types, plus some
% useful predicates that work with those types.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.typecheck_info.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module map.

%-----------------------------------------------------------------------------%
%
% The typecheck_info data structure.
%

:- type typecheck_info
    --->    typecheck_info(
                module_info     :: module_info,

                call_id         :: call_id,
                                % The call_id of the pred being called (if
                                % any).

                arg_num         :: int,
                                % The argument number within a pred call.

                pred_id         :: pred_id,
                                % The pred we're checking.

                import_status   :: import_status,
                                % Import status of the pred being checked.

                pred_markers    :: pred_markers,
                                % Markers of the pred being checked

                is_field_access_function :: bool,
                                % Is the pred we're checking a field access
                                % function? If so, there should only be
                                % a field access function application
                                % in the body, not predicate or function calls
                                % or constructor applications.

                context         :: prog_context,
                                % The context of the goal we're checking.

                unify_context   :: unify_context,
                                % The original source of the unification
                                % we're checking.

                varset          :: prog_varset,
                                % Variable names

                type_assign_set :: type_assign_set,
                                % This is the main piece of information
                                % that we are computing and which gets updated
                                % as we go along.

                found_errors    :: list(error_spec),
                                % The list of errors found so far (if any).

                overloaded_symbols :: overloaded_symbol_map,
                                % The symbols used by the current predicate
                                % that have more than one accessible
                                % definition, mapped to the unsorted list of
                                % the locations that refer to them.

                warned_about_overloading :: bool
                                % Have we already warned about highly
                                % ambiguous overloading?
            ).

:- type overloaded_symbol_map == map(overloaded_symbol, list(prog_context)).

:- type overloaded_symbol
    --->    overloaded_pred(
                simple_call_id,
                list(pred_id)
            )
    ;       overloaded_func(
                cons_id,
                list(cons_type_info_source)
            ).

%-----------------------------------------------------------------------------%
%
% typecheck_info initialisation and finalisation.
%

:- pred typecheck_info_init(module_info::in, pred_id::in,
    bool::in, tvarset::in, prog_varset::in, vartypes::in,
    head_type_params::in, hlds_constraints::in, import_status::in,
    pred_markers::in, list(error_spec)::in, typecheck_info::out) is det.

    % typecheck_info_get_final_info(Info, OldHeadTypeParams, OldExistQVars,
    %   OldExplicitVarTypes, NewTypeVarSet, New* ..., TypeRenaming,
    %   ExistTypeRenaming):
    %
    % Extracts the final inferred types from Info.
    %
    % OldHeadTypeParams should be the type variables from the head of the
    % predicate.
    % OldExistQVars should be the declared existentially quantified
    % type variables (if any).
    % OldExplicitVarTypes is the vartypes map containing the explicit
    % type qualifications.
    % New* is the newly inferred types, in NewTypeVarSet.
    % TypeRenaming is a map to rename things from the old TypeVarSet
    % to the NewTypeVarSet.
    % ExistTypeRenaming is a map (which should be applied *before*
    % applying TypeRenaming) to rename existential type variables
    % in OldExistQVars.
    %
:- pred typecheck_info_get_final_info(typecheck_info::in, list(tvar)::in,
    existq_tvars::in, vartypes::in, tvarset::out, existq_tvars::out,
    vartypes::out, prog_constraints::out,
    constraint_proof_map::out, constraint_map::out,
    tvar_renaming::out, tvar_renaming::out) is det.

%-----------------------------------------------------------------------------%
%
% Basic access predicates for typecheck_info.
%

:- pred typecheck_info_get_module_info(typecheck_info::in, module_info::out)
    is det.
:- pred typecheck_info_get_called_predid(typecheck_info::in, call_id::out)
    is det.
:- pred typecheck_info_get_arg_num(typecheck_info::in, int::out) is det.
:- pred typecheck_info_get_predid(typecheck_info::in, pred_id::out) is det.
:- pred typecheck_info_get_context(typecheck_info::in,
    prog_context::out) is det.
:- pred typecheck_info_get_unify_context(typecheck_info::in,
    unify_context::out) is det.
:- pred typecheck_info_get_varset(typecheck_info::in, prog_varset::out) is det.
:- pred typecheck_info_get_type_assign_set(typecheck_info::in,
    type_assign_set::out) is det.
:- pred typecheck_info_get_errors(typecheck_info::in, list(error_spec)::out)
    is det.
:- pred typecheck_info_get_warned_about_overloading(typecheck_info::in,
    bool::out) is det.
:- pred typecheck_info_get_overloaded_symbols(typecheck_info::in,
    overloaded_symbol_map::out) is det.
:- pred typecheck_info_get_pred_import_status(typecheck_info::in,
    import_status::out) is det.

:- pred typecheck_info_set_called_predid(call_id::in,
    typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_arg_num(int::in,
    typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_context(prog_context::in,
    typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_unify_context(unify_context::in,
    typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_type_assign_set(type_assign_set::in,
    typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_warned_about_overloading(bool::in,
    typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_overloaded_symbols(overloaded_symbol_map::in,
    typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_pred_import_status(import_status::in,
    typecheck_info::in, typecheck_info::out) is det.

%-----------------------------------------------------------------------------%
%
% Utility predicates for typecheck_info.
%

:- pred typecheck_info_get_module_name(typecheck_info::in, module_name::out)
    is det.
:- pred typecheck_info_get_preds(typecheck_info::in, predicate_table::out)
    is det.
:- pred typecheck_info_get_types(typecheck_info::in, type_table::out) is det.
:- pred typecheck_info_get_ctors(typecheck_info::in, cons_table::out) is det.
:- pred typecheck_info_get_pred_markers(typecheck_info::in, pred_markers::out)
    is det.

:- pred typecheck_info_add_overloaded_symbol(overloaded_symbol::in,
    prog_context::in, typecheck_info::in, typecheck_info::out) is det.

:- pred typecheck_info_add_error(error_spec::in,
    typecheck_info::in, typecheck_info::out) is det.

%-----------------------------------------------------------------------------%
%
% The type_assign and type_assign_set data structures.
%

:- type type_assign_set ==  list(type_assign).

:- type type_assign
    --->    type_assign(
                var_types           :: vartypes,
                type_varset         :: tvarset,
                head_type_params    :: head_type_params,
                                    % Universally quantified type variables.
                type_bindings       :: tsubst,
                                    % Type bindings.
                class_constraints   :: hlds_constraints,
                                    % The set of class constraints
                                    % collected so far.
                constraint_proofs   :: constraint_proof_map,
                                    % For each constraint found to be
                                    % redundant, why is it so?
                constraint_map      :: constraint_map
                                    % Maps constraint identifiers to the
                                    % actual constraints.
            ).

%-----------------------------------------------------------------------------%
%
% Access predicates for type_assign.
%

:- pred type_assign_get_var_types(type_assign::in,
    vartypes::out) is det.
:- pred type_assign_get_typevarset(type_assign::in,
    tvarset::out) is det.
:- pred type_assign_get_head_type_params(type_assign::in,
    head_type_params::out) is det.
:- pred type_assign_get_type_bindings(type_assign::in,
    tsubst::out) is det.
:- pred type_assign_get_typeclass_constraints(type_assign::in,
    hlds_constraints::out) is det.
:- pred type_assign_get_constraint_proofs(type_assign::in,
    constraint_proof_map::out) is det.
:- pred type_assign_get_constraint_map(type_assign::in,
    constraint_map::out) is det.

:- pred type_assign_set_var_types(vartypes::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_typevarset(tvarset::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_head_type_params(head_type_params::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_type_bindings(tsubst::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_typeclass_constraints(hlds_constraints::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_constraint_proofs(constraint_proof_map::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_constraint_map(constraint_map::in,
    type_assign::in, type_assign::out) is det.

%-----------------------------------------------------------------------------%

:- type args_type_assign_set == list(args_type_assign).

:- type args_type_assign
    --->    args(
                caller_arg_assign   :: type_assign,
                                    % Type assignment.
                callee_arg_types    :: list(mer_type),
                                    % Types of callee args, renamed apart.
                callee_constraints  :: hlds_constraints
                                    % Constraints from callee, renamed apart.
            ).

:- func get_caller_arg_assign(args_type_assign) = type_assign.
:- func get_callee_arg_types(args_type_assign) = list(mer_type).
:- func get_callee_constraints(args_type_assign) = hlds_constraints.

    % XXX document me
    %
:- func convert_args_type_assign_set(args_type_assign_set) = type_assign_set.

    % Same as convert_args_type_assign_set, but aborts when the args are
    % non-empty.
    %
:- func convert_args_type_assign_set_check_empty_args(args_type_assign_set) =
    type_assign_set.

%-----------------------------------------------------------------------------%

:- type cons_type_info
    --->    cons_type_info(
                cti_varset          :: tvarset,
                                    % Type variables.

                cti_exit_tvars      :: existq_tvars,
                                    % Existentially quantified type vars.

                cti_result_type     :: mer_type,
                                    % Constructor type.

                cti_arg_types       :: list(mer_type),
                                    % Types of the arguments.

                cti_constraints     :: hlds_constraints,
                                    % Constraints introduced by this
                                    % constructor (e.g. if it is actually
                                    % a function, or if it is an existentially
                                    % quantified data constructor).

                cti_source          :: cons_type_info_source
            ).

:- type cons_type_info_source
    --->    source_type(type_ctor)
    ;       source_builtin_type(string)
    ;       source_get_field_access(type_ctor)
    ;       source_set_field_access(type_ctor)
    ;       source_apply(string)
    ;       source_pred(pred_id).

:- func project_cons_type_info_source(cons_type_info) = cons_type_info_source.

%-----------------------------------------------------------------------------%

:- pred write_type_assign_set(type_assign_set::in, prog_varset::in,
    io::di, io::uo) is det.

:- func type_assign_set_to_pieces(type_assign_set, maybe(int), prog_varset)
    = list(format_component).

:- pred write_args_type_assign_set(args_type_assign_set::in, prog_varset::in,
    io::di, io::uo) is det.

:- func args_type_assign_set_to_pieces(args_type_assign_set, maybe(int),
    prog_varset) = list(format_component).

%-----------------------------------------------------------------------------%

    % Used for debugging typechecking.
    %
:- pred type_checkpoint(string::in, typecheck_info::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module libs.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module int.
:- import_module string.
:- import_module svmap.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

typecheck_info_init(ModuleInfo, PredId, IsFieldAccessFunction,
        TypeVarSet, VarSet, VarTypes, HeadTypeParams,
        Constraints, Status, Markers, Errors, Info) :-
    CallPredId =
        plain_call_id(simple_call_id(pf_predicate, unqualified(""), 0)),
    term.context_init(Context),
    map.init(TypeBindings),
    map.init(Proofs),
    map.init(ConstraintMap),
    WarnedAboutOverloading = no,
    map.init(OverloadedSymbols),
    Info = typecheck_info(ModuleInfo, CallPredId, 0, PredId, Status, Markers,
        IsFieldAccessFunction, Context,
        unify_context(umc_explicit, []), VarSet,
        [type_assign(VarTypes, TypeVarSet, HeadTypeParams,
            TypeBindings, Constraints, Proofs, ConstraintMap)],
        Errors, OverloadedSymbols, WarnedAboutOverloading
    ).

typecheck_info_get_final_info(Info, OldHeadTypeParams, OldExistQVars,
        OldExplicitVarTypes, NewTypeVarSet, NewHeadTypeParams,
        NewVarTypes, NewTypeConstraints, NewConstraintProofs,
        NewConstraintMap, TSubst, ExistTypeRenaming) :-
    typecheck_info_get_type_assign_set(Info, TypeAssignSet),
    (
        TypeAssignSet = [TypeAssign | _],
        type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
        type_assign_get_typevarset(TypeAssign, OldTypeVarSet),
        type_assign_get_var_types(TypeAssign, VarTypes0),
        type_assign_get_type_bindings(TypeAssign, TypeBindings),
        type_assign_get_typeclass_constraints(TypeAssign, HLDSTypeConstraints),
        type_assign_get_constraint_proofs(TypeAssign, ConstraintProofs0),
        type_assign_get_constraint_map(TypeAssign, ConstraintMap0),

        map.keys(VarTypes0, Vars),
        expand_types(Vars, TypeBindings, VarTypes0, VarTypes),
        apply_rec_subst_to_constraint_proofs(TypeBindings,
            ConstraintProofs0, ConstraintProofs),
        apply_rec_subst_to_constraint_map(TypeBindings,
            ConstraintMap0, ConstraintMap1),

        % When inferring the typeclass constraints, the universal constraints
        % here may be assumed (if this is the last pass) but will not have been
        % eliminated during context reduction, hence they will not yet be
        % in the constraint map. Since they may be required, put them in now.
        %
        % Additionally, existential constraints are assumed so don't need to be
        % eliminated during context reduction, so they need to be put in the
        % constraint map now.

        HLDSTypeConstraints = constraints(HLDSUnivConstraints,
            HLDSExistConstraints, _, _),
        list.foldl(update_constraint_map, HLDSUnivConstraints,
            ConstraintMap1, ConstraintMap2),
        list.foldl(update_constraint_map, HLDSExistConstraints,
            ConstraintMap2, ConstraintMap),

        % Figure out how we should rename the existential types
        % in the type declaration (if any).

        get_existq_tvar_renaming(OldHeadTypeParams, OldExistQVars,
            TypeBindings, ExistTypeRenaming),

        % We used to just use the OldTypeVarSet that we got from the type
        % assignment.
        %
        % However, that caused serious efficiency problems, because the
        % typevarsets get bigger and bigger with each inference step. Instead,
        % we now construct a new typevarset NewTypeVarSet which contains
        % only the variables we want, and we rename the type variables so that
        % they fit into this new typevarset.

        % First, find the set (sorted list) of type variables that we need.
        % This must include any type variables in the inferred types, the
        % explicit type qualifications, and any existentially typed variables
        % that will remain in the declaration.
        %
        % There may also be some type variables in the HeadTypeParams
        % which do not occur in the type of any variable (e.g. this can happen
        % in the case of code containing type errors). We'd better keep those,
        % too, to avoid errors when we apply the TSubst to the HeadTypeParams.
        % (XXX should we do the same for TypeConstraints and ConstraintProofs
        % too?)

        map.values(VarTypes, Types),
        type_vars_list(Types, TypeVars0),
        map.values(OldExplicitVarTypes, ExplicitTypes),
        type_vars_list(ExplicitTypes, ExplicitTypeVars0),
        map.keys(ExistTypeRenaming, ExistQVarsToBeRenamed),
        list.delete_elems(OldExistQVars, ExistQVarsToBeRenamed,
            ExistQVarsToRemain),
        list.condense([ExistQVarsToRemain, HeadTypeParams,
            TypeVars0, ExplicitTypeVars0], TypeVars1),
        list.sort_and_remove_dups(TypeVars1, TypeVars),

        % Next, create a new typevarset with the same number of variables.
        varset.squash(OldTypeVarSet, TypeVars, NewTypeVarSet, TSubst),

        % Finally, rename the types and type class constraints to use
        % the new typevarset type variables.
        apply_variable_renaming_to_type_list(TSubst, Types, NewTypes),
        map.from_corresponding_lists(Vars, NewTypes, NewVarTypes),
        map.apply_to_list(HeadTypeParams, TSubst, NewHeadTypeParams),
        retrieve_prog_constraints(HLDSTypeConstraints, TypeConstraints),
        apply_variable_renaming_to_prog_constraints(TSubst,
            TypeConstraints, NewTypeConstraints),
        apply_variable_renaming_to_constraint_proofs(TSubst,
            ConstraintProofs, NewConstraintProofs),
        apply_variable_renaming_to_constraint_map(TSubst,
            ConstraintMap, NewConstraintMap)
    ;
        TypeAssignSet = [],
        unexpected(this_file, "internal error in typecheck_info_get_vartypes")
    ).

    % Fully expand the types of the variables by applying the type bindings.
    %
    % The number of variables can be huge here (hundred of thousands for
    % Doug Auclair's training_cars program). The code below prevents stack
    % overflows in grades that do not permit tail recursion.
    %
:- pred expand_types(list(prog_var)::in, tsubst::in,
    vartypes::in, vartypes::out) is det.

expand_types(Vars, TypeSubst, !VarTypes) :-
    expand_types_2(Vars, TypeSubst, 1000, LeftOverVars, !VarTypes),
    (
        LeftOverVars = []
    ;
        LeftOverVars = [_ | _],
        expand_types(LeftOverVars, TypeSubst, !VarTypes)
    ).

:- pred expand_types_2(list(prog_var)::in, tsubst::in, int::in,
    list(prog_var)::out, vartypes::in, vartypes::out) is det.

expand_types_2([], _, _, [], !VarTypes).
expand_types_2([Var | Vars], TypeSubst, VarsToDo, LeftOverVars, !VarTypes) :-
    ( VarsToDo < 0 ->
        LeftOverVars = [Var | Vars]
    ;
        map.lookup(!.VarTypes, Var, Type0),
        apply_rec_subst_to_type(TypeSubst, Type0, Type),
        map.det_update(!.VarTypes, Var, Type, !:VarTypes),
        expand_types_2(Vars, TypeSubst, VarsToDo - 1, LeftOverVars, !VarTypes)
    ).

    % We rename any existentially quantified type variables which get mapped
    % to other type variables, unless they are mapped to universally quantified
    % type variables from the head of the predicate.
    %
:- pred get_existq_tvar_renaming(list(tvar)::in, existq_tvars::in, tsubst::in,
    tvar_renaming::out) is det.

get_existq_tvar_renaming(OldHeadTypeParams, ExistQVars, TypeBindings,
        ExistTypeRenaming) :-
    list.foldl(get_existq_tvar_renaming_2(OldHeadTypeParams, TypeBindings),
        ExistQVars, map.init, ExistTypeRenaming).

:- pred get_existq_tvar_renaming_2(existq_tvars::in, tsubst::in,
    tvar::in, tvar_renaming::in, tvar_renaming::out) is det.

get_existq_tvar_renaming_2(OldHeadTypeParams, TypeBindings, TVar, !Renaming) :-
    (
        tvar_maps_to_tvar(TypeBindings, TVar, NewTVar),
        NewTVar \= TVar,
        \+ list.member(NewTVar, OldHeadTypeParams)
    ->
        svmap.det_insert(TVar, NewTVar, !Renaming)
    ;
        true
    ).

:- pred tvar_maps_to_tvar(tsubst::in, tvar::in, tvar::out) is semidet.

tvar_maps_to_tvar(TypeBindings, TVar0, TVar) :-
    ( map.search(TypeBindings, TVar0, Type) ->
        Type = type_variable(TVar1, _),
        tvar_maps_to_tvar(TypeBindings, TVar1, TVar)
    ;
        TVar = TVar0
    ).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_errors(list(error_spec)::in,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_info_get_module_info(Info, Info ^ module_info).
typecheck_info_get_called_predid(Info, Info ^ call_id).
typecheck_info_get_arg_num(Info, Info ^ arg_num).
typecheck_info_get_predid(Info, Info ^ pred_id).
typecheck_info_get_context(Info, Info ^ context).
typecheck_info_get_unify_context(Info, Info ^ unify_context).
typecheck_info_get_varset(Info, Info ^ varset).
typecheck_info_get_type_assign_set(Info, Info ^ type_assign_set).
typecheck_info_get_errors(Info, Info ^ found_errors).
typecheck_info_get_warned_about_overloading(Info,
        Info ^ warned_about_overloading).
typecheck_info_get_overloaded_symbols(Info, Info ^ overloaded_symbols).
typecheck_info_get_pred_import_status(Info, Info ^ import_status).

typecheck_info_set_called_predid(PredCallId, Info,
        Info ^ call_id := PredCallId).
typecheck_info_set_arg_num(ArgNum, Info, Info ^ arg_num := ArgNum).
typecheck_info_set_context(Context, Info, Info ^ context := Context).
typecheck_info_set_unify_context(UnifyContext, Info,
        Info ^ unify_context := UnifyContext).
typecheck_info_set_type_assign_set(TypeAssignSet, Info,
        Info ^ type_assign_set := TypeAssignSet).
typecheck_info_set_errors(Errors, Info, Info ^ found_errors := Errors).
typecheck_info_set_warned_about_overloading(Warned, Info,
        Info ^ warned_about_overloading := Warned).
typecheck_info_set_overloaded_symbols(Symbols, Info,
        Info ^ overloaded_symbols := Symbols).
typecheck_info_set_pred_import_status(Status, Info,
        Info ^ import_status := Status).

%-----------------------------------------------------------------------------%

typecheck_info_get_module_name(Info, Name) :-
    module_info_get_name(Info ^ module_info, Name).
typecheck_info_get_preds(Info, Preds) :-
    module_info_get_predicate_table(Info ^ module_info, Preds).
typecheck_info_get_types(Info, Types) :-
    module_info_get_type_table(Info ^ module_info, Types).
typecheck_info_get_ctors(Info, Ctors) :-
    module_info_get_cons_table(Info ^ module_info, Ctors).

typecheck_info_get_pred_markers(Info, PredMarkers) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
    typecheck_info_get_predid(Info, PredId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_markers(PredInfo, PredMarkers).

typecheck_info_add_overloaded_symbol(Symbol, Context, !Info) :-
    typecheck_info_get_overloaded_symbols(!.Info, SymbolMap0),
    ( map.search(SymbolMap0, Symbol, OldContexts) ->
        Contexts = [Context | OldContexts],
        map.det_update(SymbolMap0, Symbol, Contexts, SymbolMap)
    ;
        Contexts = [Context],
        map.det_insert(SymbolMap0, Symbol, Contexts, SymbolMap)
    ),
    typecheck_info_set_overloaded_symbols(SymbolMap, !Info).

typecheck_info_add_error(Error, !Info) :-
    typecheck_info_get_errors(!.Info, Errors0),
    Errors = [Error | Errors0],
    typecheck_info_set_errors(Errors, !Info).

%-----------------------------------------------------------------------------%

type_assign_get_var_types(TA, TA ^ var_types).
type_assign_get_typevarset(TA, TA ^ type_varset).
type_assign_get_head_type_params(TA, TA ^ head_type_params).
type_assign_get_type_bindings(TA, TA ^ type_bindings).
type_assign_get_typeclass_constraints(TA, TA ^ class_constraints).
type_assign_get_constraint_proofs(TA, TA ^ constraint_proofs).
type_assign_get_constraint_map(TA, TA ^ constraint_map).

type_assign_set_var_types(X, TA, TA ^ var_types := X).
type_assign_set_typevarset(X, TA, TA ^ type_varset := X).
type_assign_set_head_type_params(X, TA, TA ^ head_type_params := X).
type_assign_set_type_bindings(X, TA, TA ^ type_bindings := X).
type_assign_set_typeclass_constraints(X, TA, TA ^ class_constraints := X).
type_assign_set_constraint_proofs(X, TA, TA ^ constraint_proofs := X).
type_assign_set_constraint_map(X, TA, TA ^ constraint_map := X).

%-----------------------------------------------------------------------------%

convert_args_type_assign_set([]) = [].
convert_args_type_assign_set([ArgsTypeAssign | ArgsTypeAssigns]) =
    [convert_args_type_assign(ArgsTypeAssign) |
    convert_args_type_assign_set(ArgsTypeAssigns)].

convert_args_type_assign_set_check_empty_args([]) = [].
convert_args_type_assign_set_check_empty_args([ArgTypeAssign | ArgTypeAssigns])
        = Result :-
    ArgTypeAssign = args(_, Args, _),
    (
        Args = [],
        Result =
            [convert_args_type_assign(ArgTypeAssign) |
            convert_args_type_assign_set_check_empty_args(ArgTypeAssigns)]
    ;
        Args = [_ | _],
        % This should never happen, since the arguments should all have been
        % processed at this point.
        unexpected(this_file, "convert_nonempty_args_type_assign_set")
    ).

:- func convert_args_type_assign(args_type_assign) = type_assign.

convert_args_type_assign(args(TypeAssign0, _, Constraints0)) = TypeAssign :-
    type_assign_get_typeclass_constraints(TypeAssign0, OldConstraints),
    type_assign_get_type_bindings(TypeAssign0, Bindings),
    apply_rec_subst_to_constraints(Bindings, Constraints0, Constraints),
    merge_hlds_constraints(Constraints, OldConstraints, NewConstraints),
    type_assign_set_typeclass_constraints(NewConstraints,
        TypeAssign0, TypeAssign).

get_caller_arg_assign(ArgsTypeAssign) = ArgsTypeAssign ^ caller_arg_assign.
get_callee_arg_types(ArgsTypeAssign) = ArgsTypeAssign ^ callee_arg_types.
get_callee_constraints(ArgsTypeAssign) = ArgsTypeAssign ^ callee_constraints.

project_cons_type_info_source(CTI) = CTI ^ cti_source.

%-----------------------------------------------------------------------------%

:- func varnums = bool.

varnums = yes.

:- func inc_maybe_seq(maybe(int)) = maybe(int).

inc_maybe_seq(no) = no.
inc_maybe_seq(yes(N)) = yes(N + 1).

write_type_assign_set([], _, !IO).
write_type_assign_set([TypeAssign | TypeAssigns], VarSet, !IO) :-
    io.write_string("\t", !IO),
    write_type_assign(TypeAssign, VarSet, !IO),
    io.write_string("\n", !IO),
    write_type_assign_set(TypeAssigns, VarSet, !IO).

type_assign_set_to_pieces([], _, _) = [].
type_assign_set_to_pieces([TypeAssign | TypeAssigns], MaybeSeq, VarSet) =
    type_assign_to_pieces(TypeAssign, MaybeSeq, VarSet) ++
    type_assign_set_to_pieces(TypeAssigns, inc_maybe_seq(MaybeSeq), VarSet).

write_args_type_assign_set([], _, !IO).
write_args_type_assign_set([ArgTypeAssign | ArgTypeAssigns], VarSet, !IO) :-
    % XXX Why does this simply pick the TypeAssign part of the ArgTypeAssign,
    % instead of invoking convert_args_type_assign?
    ArgTypeAssign = args(TypeAssign, _ArgTypes, _Cnstrs),
    io.write_string("\t", !IO),
    write_type_assign(TypeAssign, VarSet, !IO),
    io.write_string("\n", !IO),
    write_args_type_assign_set(ArgTypeAssigns, VarSet, !IO).

args_type_assign_set_to_pieces([], _, _) = [].
args_type_assign_set_to_pieces([ArgTypeAssign | ArgTypeAssigns], MaybeSeq,
        VarSet) = Pieces :-
    % XXX Why does this simply pick the TypeAssign part of the ArgTypeAssign,
    % instead of invoking convert_args_type_assign?
    ArgTypeAssign = args(TypeAssign, _ArgTypes, _Cnstrs),
    Pieces = type_assign_to_pieces(TypeAssign, MaybeSeq, VarSet) ++
        args_type_assign_set_to_pieces(ArgTypeAssigns, inc_maybe_seq(MaybeSeq),
            VarSet).

:- pred write_type_assign(type_assign::in, prog_varset::in, io::di, io::uo)
    is det.

write_type_assign(TypeAssign, VarSet, !IO) :-
    type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
    type_assign_get_var_types(TypeAssign, VarTypes),
    type_assign_get_typeclass_constraints(TypeAssign, Constraints),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    type_assign_get_typevarset(TypeAssign, TypeVarSet),
    map.keys(VarTypes, Vars),
    (
        HeadTypeParams = []
    ;
        HeadTypeParams = [_ | _],
        io.write_string("some [", !IO),
        mercury_output_vars(TypeVarSet, varnums, HeadTypeParams, !IO),
        io.write_string("]\n\t", !IO)
    ),
    write_type_assign_types(Vars, VarSet, VarTypes, TypeBindings, TypeVarSet,
        no, !IO),
    write_type_assign_hlds_constraints(Constraints, TypeBindings, TypeVarSet,
        !IO),
    io.write_string("\n", !IO).

:- func type_assign_to_pieces(type_assign, maybe(int), prog_varset)
    = list(format_component).

type_assign_to_pieces(TypeAssign, MaybeSeq, VarSet) = Pieces :-
    (
        MaybeSeq = yes(N),
        SeqPieces0 = [words("Type assignment"), int_fixed(N), suffix(":"), nl],
        ( N > 1 ->
            SeqPieces = [blank_line | SeqPieces0]
        ;
            SeqPieces = SeqPieces0
        )
    ;
        MaybeSeq = no,
        SeqPieces = []
    ),
    type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
    type_assign_get_var_types(TypeAssign, VarTypes),
    type_assign_get_typeclass_constraints(TypeAssign, Constraints),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    type_assign_get_typevarset(TypeAssign, TypeVarSet),
    map.keys(VarTypes, Vars),
    (
        HeadTypeParams = [],
        HeadPieces = []
    ;
        HeadTypeParams = [_ | _],
        VarsStr = mercury_vars_to_string(TypeVarSet, varnums, HeadTypeParams), 
        HeadPieces = [words("some [" ++ VarsStr ++ "]"), nl]
    ),
    TypePieces = type_assign_types_to_pieces(Vars, VarSet, VarTypes,
        TypeBindings, TypeVarSet, no),
    ConstraintPieces = type_assign_hlds_constraints_to_pieces(Constraints,
        TypeBindings, TypeVarSet),
    Pieces = SeqPieces ++ HeadPieces ++ TypePieces ++ ConstraintPieces ++ [nl].

:- pred write_type_assign_types(list(prog_var)::in, prog_varset::in,
    vartypes::in, tsubst::in, tvarset::in, bool::in, io::di, io::uo) is det.

write_type_assign_types([], _, _, _, _, FoundOne, !IO) :-
    (
        FoundOne = no,
        io.write_string("(No variables were assigned a type)", !IO)
    ;
        FoundOne = yes
    ).
write_type_assign_types([Var | Vars], VarSet, VarTypes, TypeBindings,
        TypeVarSet, FoundOne, !IO) :-
    ( map.search(VarTypes, Var, Type) ->
        (
            FoundOne = yes,
            io.write_string("\n\t", !IO)
        ;
            FoundOne = no
        ),
        mercury_output_var(VarSet, varnums, Var, !IO),
        io.write_string(": ", !IO),
        write_type_with_bindings(Type, TypeVarSet, TypeBindings, !IO),
        write_type_assign_types(Vars, VarSet, VarTypes, TypeBindings,
            TypeVarSet, yes, !IO)
    ;
        write_type_assign_types(Vars, VarSet, VarTypes, TypeBindings,
            TypeVarSet, FoundOne, !IO)
    ).

:- func type_assign_types_to_pieces(list(prog_var), prog_varset,
    vartypes, tsubst, tvarset, bool) = list(format_component).

type_assign_types_to_pieces([], _, _, _, _, FoundOne) = Pieces :-
    (
        FoundOne = no,
        Pieces = [words("(No variables were assigned a type)")]
    ;
        FoundOne = yes,
        Pieces = []
    ).
type_assign_types_to_pieces([Var | Vars], VarSet, VarTypes, TypeBindings,
        TypeVarSet, FoundOne) = Pieces :-
    ( map.search(VarTypes, Var, Type) ->
        (
            FoundOne = yes,
            PrefixPieces = [nl]
        ;
            FoundOne = no,
            PrefixPieces = []
        ),
        VarStr = mercury_var_to_string(VarSet, varnums, Var),
        TypeStr = type_with_bindings_to_string(Type, TypeVarSet, TypeBindings),
        AssignPieces = [fixed(VarStr), suffix(":"), words(TypeStr)],
        TailPieces = type_assign_types_to_pieces(Vars, VarSet, VarTypes,
            TypeBindings, TypeVarSet, yes),
        Pieces = PrefixPieces ++ AssignPieces ++ TailPieces
    ;
        Pieces = type_assign_types_to_pieces(Vars, VarSet, VarTypes,
            TypeBindings, TypeVarSet, FoundOne)
    ).

:- pred write_type_assign_hlds_constraints(hlds_constraints::in,
    tsubst::in, tvarset::in, io::di, io::uo) is det.

write_type_assign_hlds_constraints(Constraints, TypeBindings, TypeVarSet,
        !IO) :-
    Constraints = constraints(ConstraintsToProve, AssumedConstraints, _, _),
    write_type_assign_constraints("&", AssumedConstraints,
        TypeBindings, TypeVarSet, no, !IO),
    write_type_assign_constraints("<=", ConstraintsToProve,
        TypeBindings, TypeVarSet, no, !IO).

:- func type_assign_hlds_constraints_to_pieces(hlds_constraints,
    tsubst, tvarset) = list(format_component).

type_assign_hlds_constraints_to_pieces(Constraints, TypeBindings, TypeVarSet)
        = Pieces1 ++ Pieces2 :-
    Constraints = constraints(ConstraintsToProve, AssumedConstraints, _, _),
    PiecesList1 = type_assign_constraints_to_pieces_list("&",
        AssumedConstraints, TypeBindings, TypeVarSet, no),
    PiecesList2 = type_assign_constraints_to_pieces_list("<=",
        ConstraintsToProve, TypeBindings, TypeVarSet, no),
    Pieces1 = component_list_to_line_pieces(PiecesList1, []),
    Pieces2 = component_list_to_line_pieces(PiecesList2, []).

:- pred write_type_assign_constraints(string::in, list(hlds_constraint)::in,
    tsubst::in, tvarset::in, bool::in, io::di, io::uo) is det.

write_type_assign_constraints(_, [], _, _, _, !IO).
write_type_assign_constraints(Operator, [Constraint | Constraints],
        TypeBindings, TypeVarSet, FoundOne, !IO) :-
    (
        FoundOne = no,
        io.write_strings(["\n\t", Operator, " "], !IO)
    ;
        FoundOne = yes,
        io.write_string(",\n\t   ", !IO)
    ),
    apply_rec_subst_to_constraint(TypeBindings, Constraint, BoundConstraint),
    retrieve_prog_constraint(BoundConstraint, ProgConstraint),
    mercury_output_constraint(TypeVarSet, varnums, ProgConstraint, !IO),
    write_type_assign_constraints(Operator, Constraints, TypeBindings,
        TypeVarSet, yes, !IO).

:- func type_assign_constraints_to_pieces_list(string, list(hlds_constraint),
    tsubst, tvarset, bool) = list(list(format_component)).

type_assign_constraints_to_pieces_list(_, [], _, _, _) = [].
type_assign_constraints_to_pieces_list(Operator, [Constraint | Constraints],
        TypeBindings, TypeVarSet, FoundOne) = [ThisPieces] ++ TailPieceLists :-
    (
        FoundOne = no,
        Prefix = Operator ++ " "
    ;
        FoundOne = yes,
        Prefix = "   "
    ),
    apply_rec_subst_to_constraint(TypeBindings, Constraint, BoundConstraint),
    retrieve_prog_constraint(BoundConstraint, ProgConstraint),
    ThisPieces = [fixed(Prefix ++
        mercury_constraint_to_string(TypeVarSet, ProgConstraint))],
    TailPieceLists = type_assign_constraints_to_pieces_list(Operator,
        Constraints, TypeBindings, TypeVarSet, yes).

    % write_type_with_bindings writes out a type after applying the
    % type bindings.
    %
:- pred write_type_with_bindings(mer_type::in, tvarset::in, tsubst::in,
    io::di, io::uo) is det.

write_type_with_bindings(Type0, TypeVarSet, TypeBindings, !IO) :-
    apply_rec_subst_to_type(TypeBindings, Type0, Type1),
    strip_builtin_qualifiers_from_type(Type1, Type),
    mercury_output_type(TypeVarSet, no, Type, !IO).

:- func type_with_bindings_to_string(mer_type, tvarset, tsubst) = string.

type_with_bindings_to_string(Type0, TypeVarSet, TypeBindings) = Str :-
    apply_rec_subst_to_type(TypeBindings, Type0, Type1),
    strip_builtin_qualifiers_from_type(Type1, Type),
    Str = mercury_type_to_string(TypeVarSet, no, Type).

%-----------------------------------------------------------------------------%

type_checkpoint(Msg, Info, !IO) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_types, DoCheckPoint),
    (
        DoCheckPoint = yes,
        do_type_checkpoint(Msg, Info, !IO)
    ;
        DoCheckPoint = no
    ).

:- pred do_type_checkpoint(string::in, typecheck_info::in, io::di, io::uo)
    is det.

do_type_checkpoint(Msg, T0, !IO) :-
    io.write_string("At ", !IO),
    io.write_string(Msg, !IO),
    io.write_string(": ", !IO),
    globals.io_lookup_bool_option(detailed_statistics, Statistics, !IO),
    maybe_report_stats(Statistics, !IO),
    io.write_string("\n", !IO),
    typecheck_info_get_type_assign_set(T0, TypeAssignSet),
    (
        Statistics = yes,
        TypeAssignSet = [TypeAssign | _]
    ->
        type_assign_get_var_types(TypeAssign, VarTypes),
        checkpoint_tree_stats("\t`var -> type' map", VarTypes, !IO),
        type_assign_get_type_bindings(TypeAssign, TypeBindings),
        checkpoint_tree_stats("\t`type var -> type' map", TypeBindings, !IO)
    ;
        true
    ),
    typecheck_info_get_varset(T0, VarSet),
    write_type_assign_set(TypeAssignSet, VarSet, !IO).

:- pred checkpoint_tree_stats(string::in, map(_K, _V)::in, io::di, io::uo)
    is det.

checkpoint_tree_stats(Description, Tree, !IO) :-
    map.count(Tree, Count),
    io.write_string(Description, !IO),
    io.write_string(": count = ", !IO),
    io.write_int(Count, !IO),
    io.write_string("\n", !IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "typecheck_info.m".

%-----------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_info.
%-----------------------------------------------------------------------------%
