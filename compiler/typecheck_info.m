%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
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

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.

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

                found_error     :: bool,
                                % Did we find any type errors?

                warned_about_overloading :: bool
                                % Have we already warned about highly
                                % ambiguous overloading?
            ).

%-----------------------------------------------------------------------------%
%
% typecheck_info initialisation and finalisation.
%

:- pred typecheck_info_init(module_info::in, pred_id::in,
    bool::in, tvarset::in, prog_varset::in, vartypes::in,
    head_type_params::in, hlds_constraints::in, import_status::in,
    pred_markers::in, typecheck_info::out) is det.

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
:- pred typecheck_info_get_found_error(typecheck_info::in, bool::out) is det.
:- pred typecheck_info_get_warned_about_overloading(typecheck_info::in,
    bool::out) is det.
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
:- pred typecheck_info_set_found_error(bool::in,
    typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_warned_about_overloading(bool::in,
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

%-----------------------------------------------------------------------------%
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

:- pred write_type_assign_set(type_assign_set::in, prog_varset::in,
    io::di, io::uo) is det.

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

:- pred write_args_type_assign_set(args_type_assign_set::in, prog_varset::in,
    io::di, io::uo) is det.

:- pred convert_nonempty_args_type_assign_set(args_type_assign_set::in,
    type_assign_set::out) is det.

    % Same as convert_nonempty_args_type_assign_set, but does not abort
    % when the args are empty.
    %
:- pred convert_args_type_assign_set(args_type_assign_set::in,
    type_assign_set::out) is det.

%-----------------------------------------------------------------------------%

:- type cons_type_info
    --->    cons_type_info(
                tvarset,            % Type variables.
                existq_tvars,       % Existentially quantified type vars.
                mer_type,           % Constructor type.
                list(mer_type),     % Types of the arguments.
                hlds_constraints    % Constraints introduced by this
                                    % constructor (e.g. if it is actually
                                    % a function, or if it is an existentially
                                    % quantified data constructor).
            ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module libs.compiler_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.

:- import_module map.
:- import_module pair.
:- import_module svmap.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

typecheck_info_init(ModuleInfo, PredId, IsFieldAccessFunction,
        TypeVarSet, VarSet, VarTypes, HeadTypeParams,
        Constraints, Status, Markers, Info) :-
    CallPredId = call(predicate - unqualified("") / 0),
    term.context_init(Context),
    map.init(TypeBindings),
    map.init(Proofs),
    map.init(ConstraintMap),
    FoundTypeError = no,
    WarnedAboutOverloading = no,
    Info = typecheck_info(
        ModuleInfo, CallPredId, 0, PredId, Status, Markers,
        IsFieldAccessFunction, Context,
        unify_context(explicit, []), VarSet,
        [type_assign(VarTypes, TypeVarSet, HeadTypeParams,
            TypeBindings, Constraints, Proofs, ConstraintMap)],
        FoundTypeError, WarnedAboutOverloading
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

        %
        % When inferring the typeclass constraints, the universal
        % constraints here may be assumed (if this is the last pass)
        % but will not have been eliminated during context reduction,
        % hence they will not yet be in the constraint map.  Since
        % they may be required, put them in now.
        %
        % Additionally, existential constraints are assumed so don't
        % need to be eliminated during context reduction, so they
        % need to be put in the constraint map now.
        %
        HLDSTypeConstraints = constraints(HLDSUnivConstraints,
            HLDSExistConstraints, _, _),
        list.foldl(update_constraint_map, HLDSUnivConstraints,
            ConstraintMap1, ConstraintMap2),
        list.foldl(update_constraint_map, HLDSExistConstraints,
            ConstraintMap2, ConstraintMap),

        %
        % Figure out how we should rename the existential types
        % in the type declaration (if any).
        %
        get_existq_tvar_renaming(OldHeadTypeParams, OldExistQVars,
            TypeBindings, ExistTypeRenaming),

        %
        % We used to just use the OldTypeVarSet that we got
        % from the type assignment.
        %
        % However, that caused serious efficiency problems,
        % because the typevarsets get bigger and bigger with each
        % inference step.  Instead, we now construct a new
        % typevarset NewTypeVarSet which contains only the
        % variables we want, and we rename the type variables
        % so that they fit into this new typevarset.
        %

        %
        % First, find the set (sorted list) of type variables
        % that we need.  This must include any type variables
        % in the inferred types, the explicit type qualifications,
        % and any existentially typed variables that will remain
        % in the declaration.
        %
        % There may also be some type variables in the HeadTypeParams
        % which do not occur in the type of any variable (e.g. this
        % can happen in the case of code containing type errors).
        % We'd better keep those, too, to avoid errors
        % when we apply the TSubst to the HeadTypeParams.
        % (XXX should we do the same for TypeConstraints and
        % ConstraintProofs too?)
        %
        map.values(VarTypes, Types),
        prog_type.vars_list(Types, TypeVars0),
        map.values(OldExplicitVarTypes, ExplicitTypes),
        prog_type.vars_list(ExplicitTypes, ExplicitTypeVars0),
        map.keys(ExistTypeRenaming, ExistQVarsToBeRenamed),
        list.delete_elems(OldExistQVars, ExistQVarsToBeRenamed,
            ExistQVarsToRemain),
        list.condense([ExistQVarsToRemain, HeadTypeParams,
            TypeVars0, ExplicitTypeVars0], TypeVars1),
        list.sort_and_remove_dups(TypeVars1, TypeVars),
        %
        % Next, create a new typevarset with the same number of
        % variables.
        %
        varset.squash(OldTypeVarSet, TypeVars, NewTypeVarSet, TSubst),
        %
        % Finally, rename the types and type class constraints
        % to use the new typevarset type variables.
        %
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
:- pred expand_types(list(prog_var)::in, tsubst::in,
    vartypes::in, vartypes::out) is det.

expand_types([], _, !VarTypes).
expand_types([Var | Vars], TypeSubst, !VarTypes) :-
    map.lookup(!.VarTypes, Var, Type0),
    apply_rec_subst_to_type(TypeSubst, Type0, Type),
    map.det_update(!.VarTypes, Var, Type, !:VarTypes),
    expand_types(Vars, TypeSubst, !VarTypes).

    % We rename any existentially quantified type variables which
    % get mapped to other type variables, unless they are mapped to
    % universally quantified type variables from the head of the predicate.
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
        Type = variable(TVar1, _),
        tvar_maps_to_tvar(TypeBindings, TVar1, TVar)
    ;
        TVar = TVar0
    ).

%-----------------------------------------------------------------------------%

typecheck_info_get_module_info(Info, Info ^ module_info).
typecheck_info_get_called_predid(Info, Info ^ call_id).
typecheck_info_get_arg_num(Info, Info ^ arg_num).
typecheck_info_get_predid(Info, Info ^ pred_id).
typecheck_info_get_context(Info, Info ^ context).
typecheck_info_get_unify_context(Info, Info ^ unify_context).
typecheck_info_get_varset(Info, Info ^ varset).
typecheck_info_get_type_assign_set(Info, Info ^ type_assign_set).
typecheck_info_get_found_error(Info, Info ^ found_error).
typecheck_info_get_warned_about_overloading(Info,
        Info ^ warned_about_overloading).
typecheck_info_get_pred_import_status(Info, Info ^ import_status).

typecheck_info_set_called_predid(PredCallId, Info,
        Info ^ call_id := PredCallId).
typecheck_info_set_arg_num(ArgNum, Info, Info ^ arg_num := ArgNum).
typecheck_info_set_context(Context, Info, Info ^ context := Context).
typecheck_info_set_unify_context(UnifyContext, Info,
        Info ^ unify_context := UnifyContext).
typecheck_info_set_type_assign_set(TypeAssignSet, Info,
        Info ^ type_assign_set := TypeAssignSet).
typecheck_info_set_found_error(FoundError, Info,
        Info ^ found_error := FoundError).
typecheck_info_set_warned_about_overloading(Warned, Info,
        Info ^ warned_about_overloading := Warned).
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

%-----------------------------------------------------------------------------%
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

:- func varnums = bool.

varnums = yes.

write_type_assign_set([], _) --> [].
write_type_assign_set([TypeAssign | TypeAssigns], VarSet) -->
    io.write_string("\t"),
    write_type_assign(TypeAssign, VarSet),
    io.write_string("\n"),
    write_type_assign_set(TypeAssigns, VarSet).

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
        mercury_output_vars(HeadTypeParams, TypeVarSet, varnums, !IO),
        io.write_string("]\n\t", !IO)
    ),
    write_type_assign_types(Vars, VarSet, VarTypes, TypeBindings, TypeVarSet,
        no, !IO),
    write_type_assign_constraints(Constraints, TypeBindings, TypeVarSet, !IO),
    io.write_string("\n", !IO).

:- pred write_type_assign_types(list(prog_var)::in, prog_varset::in,
    vartypes::in, tsubst::in, tvarset::in, bool::in,
    io::di, io::uo) is det.

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
        mercury_output_var(Var, VarSet, varnums, !IO),
        io.write_string(": ", !IO),
        write_type_with_bindings(Type, TypeVarSet, TypeBindings, !IO),
        write_type_assign_types(Vars, VarSet, VarTypes, TypeBindings,
            TypeVarSet, yes, !IO)
    ;
        write_type_assign_types(Vars, VarSet, VarTypes, TypeBindings,
            TypeVarSet, FoundOne, !IO)
    ).

:- pred write_type_assign_constraints(hlds_constraints::in,
    tsubst::in, tvarset::in, io::di, io::uo) is det.

write_type_assign_constraints(Constraints, TypeBindings, TypeVarSet, !IO) :-
    Constraints = constraints(ConstraintsToProve, AssumedConstraints, _, _),
    write_type_assign_constraints("&", AssumedConstraints,
        TypeBindings, TypeVarSet, no, !IO),
    write_type_assign_constraints("<=", ConstraintsToProve,
        TypeBindings, TypeVarSet, no, !IO).

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
    apply_rec_subst_to_constraint(TypeBindings, Constraint,
        BoundConstraint),
    retrieve_prog_constraint(BoundConstraint, ProgConstraint),
    mercury_output_constraint(TypeVarSet, varnums, ProgConstraint,
        !IO),
    write_type_assign_constraints(Operator, Constraints, TypeBindings,
        TypeVarSet, yes, !IO).

    % write_type_with_bindings writes out a type after applying the
    % type bindings.
    %
:- pred write_type_with_bindings(mer_type::in, tvarset::in, tsubst::in,
    io::di, io::uo) is det.

write_type_with_bindings(Type0, TypeVarSet, TypeBindings, !IO) :-
    apply_rec_subst_to_type(TypeBindings, Type0, Type1),
    strip_builtin_qualifiers_from_type(Type1, Type),
    mercury_output_type(TypeVarSet, no, Type, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

write_args_type_assign_set([], _, !IO).
write_args_type_assign_set([ArgTypeAssign | ArgTypeAssigns], VarSet, !IO) :-
    ArgTypeAssign = args(TypeAssign, _ArgTypes, _Cnstrs),
    io.write_string("\t", !IO),
    write_type_assign(TypeAssign, VarSet, !IO),
    io.write_string("\n", !IO),
    write_args_type_assign_set(ArgTypeAssigns, VarSet, !IO).

convert_nonempty_args_type_assign_set([], []).
convert_nonempty_args_type_assign_set([ArgTypeAssign | ArgTypeAssigns],
        [TypeAssign | TypeAssigns]) :-
    ArgTypeAssign = args(_, Args, _),
    (
        Args = [],
        convert_args_type_assign(ArgTypeAssign, TypeAssign)
    ;
        Args = [_ | _],
        % this should never happen, since the arguments should
        % all have been processed at this point
        unexpected(this_file, "convert_nonempty_args_type_assign_set")
    ),
    convert_nonempty_args_type_assign_set(ArgTypeAssigns, TypeAssigns).

convert_args_type_assign_set([], []).
convert_args_type_assign_set([X | Xs], [Y | Ys]) :-
    convert_args_type_assign(X, Y),
    convert_args_type_assign_set(Xs, Ys).

:- pred convert_args_type_assign(args_type_assign::in, type_assign::out)
    is det.

convert_args_type_assign(args(TypeAssign0, _, Constraints0), TypeAssign) :-
    type_assign_get_typeclass_constraints(TypeAssign0, OldConstraints),
    type_assign_get_type_bindings(TypeAssign0, Bindings),
    apply_rec_subst_to_constraints(Bindings, Constraints0, Constraints),
    merge_hlds_constraints(Constraints, OldConstraints, NewConstraints),
    type_assign_set_typeclass_constraints(NewConstraints,
        TypeAssign0, TypeAssign).

get_caller_arg_assign(ArgsTypeAssign) = ArgsTypeAssign ^ caller_arg_assign.
get_callee_arg_types(ArgsTypeAssign) = ArgsTypeAssign ^ callee_arg_types.
get_callee_constraints(ArgsTypeAssign) = ArgsTypeAssign ^ callee_constraints.

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "typecheck_info.m".

%-----------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_info.
%-----------------------------------------------------------------------------%
