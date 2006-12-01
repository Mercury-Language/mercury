%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: hlds_rtti.m.
% Main authors: Mark Brown.
%
% This module defines the part of the HLDS that keeps track of information
% relating to RTTI.
%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_rtti.
:- interface.

:- import_module hlds.hlds_pred.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module set.

%-----------------------------------------------------------------------------%

:- type prog_var_name == string.

    % The rtti_proc_label type holds all the information about a procedure
    % that we need to compute the entry label for that procedure
    % in the target language (the llds.code_addr or mlds.code_addr).

:- type rtti_proc_label
    --->    rtti_proc_label(
                pred_or_func            ::  pred_or_func,
                this_module             ::  module_name,
                proc_module             ::  module_name,
                proc_name               ::  string,
                proc_arity              ::  arity,
                proc_arg_types          ::  list(mer_type),
                pred_id                 ::  pred_id,
                proc_id                 ::  proc_id,
                proc_headvars           ::  assoc_list(prog_var,
                                                prog_var_name),
                proc_arg_modes          ::  list(arg_mode),
                proc_interface_detism   ::  determinism,

                % The following booleans hold values computed from the
                % pred_info, using procedures
                %   pred_info_is_imported/1,
                %   pred_info_is_pseudo_imported/1,
                %   pred_info_get_origin/1
                % respectively.
                % We store booleans here, rather than storing the
                % pred_info, to avoid retaining a reference to the
                % parts of the pred_info that we aren't interested in,
                % so that those parts can be garbage collected.
                % We use booleans rather than an import_status
                % so that we can continue to use the above-mentioned
                % abstract interfaces rather than hard-coding tests
                % on the import_status.

                pred_is_imported        ::  bool,
                pred_is_pseudo_imported ::  bool,
                pred_info_origin        ::  pred_origin,

                % The following boolean holds a value computed from the
                % proc_info, using procedure_is_exported/2

                proc_is_exported        ::  bool,

                % The following bool is true if the procedure was
                % imported, either because the containing predicate
                % was imported, or because it was pseudoimported
                % and the procedure is an in-in unify procedure.

                proc_is_imported        ::  bool
            ).

%-----------------------------------------------------------------------------%
%
% Types and predicates to store information about RTTI.
%

    % Describes the class constraints on an instance method implementation.
    % This information is used by polymorphism.m to ensure that the
    % type_info and typeclass_info arguments are added in the order in
    % which they will be passed in by do_call_class_method.
    %
:- type instance_method_constraints
    --->    instance_method_constraints(
                class_id,
                list(mer_type),         % The types in the head of the
                                        % instance declaration.
                list(prog_constraint),  % The universal constraints
                                        % on the instance declaration.
                prog_constraints        % The contraints on the method's
                                        % type declaration in the
                                        % `:- typeclass' declaration.
            ).

    %  A type_info_locn specifies how to access a type_info.
    %
:- type type_info_locn
    --->    type_info(prog_var)
                % It is a normal type_info, i.e. the type
                % is not constrained.

    ;       typeclass_info(prog_var, int).
                % The type_info is packed inside a
                % typeclass_info. If the int is N, it is
                % the Nth type_info inside the typeclass_info,
                % but there may be several superclass pointers
                % before the block of type_infos, so it won't
                % be the Nth word of the typeclass_info.
                %
                % To find the type_info inside the
                % typeclass_info, use the predicate
                % type_info_from_typeclass_info from Mercury
                % code; from C code use the macro
                % MR_typeclass_info_superclass_info.

    % type_info_locn_var(TypeInfoLocn, Var):
    %
    % Var is the variable corresponding to the TypeInfoLocn. Note
    % that this does *not* mean that Var is a type_info; it may be
    % a typeclass_info in which the type_info is nested.
    %
:- pred type_info_locn_var(type_info_locn::in, prog_var::out) is det.

:- pred type_info_locn_set_var(prog_var::in,
    type_info_locn::in, type_info_locn::out) is det.

    % This type describes the contents of a prog_var.
    %
:- type rtti_var_info
    --->    type_info_var(mer_type)
            % The variable holds a type_info for the given type.

    ;       typeclass_info_var(prog_constraint)
            % The variable holds a typeclass_info for the given
            % constraint.

    ;       non_rtti_var.
            % The variable does not directly hold any run time
            % type information.

    % This records information about how type_infos and typeclass_infos
    % were introduced in the polymorphism transformation.
    %
:- type rtti_varmaps.

    % Returns an empty rtti_varmaps structure.
    %
:- pred rtti_varmaps_init(rtti_varmaps::out) is det.

    % Succeeds iff the rtti_varmaps contain no information about any
    % type variables.
    %
:- pred rtti_varmaps_no_tvars(rtti_varmaps::in) is semidet.

    % Find the location of a type_info.
    %
:- pred rtti_lookup_type_info_locn(rtti_varmaps::in, tvar::in,
    type_info_locn::out) is det.

    % Find the location of a type_info, if it is known.
    %
:- pred rtti_search_type_info_locn(rtti_varmaps::in, tvar::in,
    type_info_locn::out) is semidet.

    % Find the prog_var which contains the typeclass_info for a given
    % constraint and which can be reused.
    %
:- pred rtti_lookup_typeclass_info_var(rtti_varmaps::in, prog_constraint::in,
    prog_var::out) is det.

    % Find the prog_var which contains the typeclass_info for a given
    % constraint and which can be reused, if it is known.
    %
:- pred rtti_search_typeclass_info_var(rtti_varmaps::in, prog_constraint::in,
    prog_var::out) is semidet.

    % Find what RTTI, if any, is stored in a prog_var.
    %
:- pred rtti_varmaps_var_info(rtti_varmaps::in, prog_var::in,
    rtti_var_info::out) is det.

    % Insert the location of a type_info.  Abort if such information
    % already exists.
    %
:- pred rtti_det_insert_type_info_locn(tvar::in, type_info_locn::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % Set the location of a type_info, overwriting any previous
    % information.
    %
:- pred rtti_set_type_info_locn(tvar::in, type_info_locn::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % Insert the prog_var which contains the typeclass_info for a
    % given constraint.  Abort if such information already exists.
    %
:- pred rtti_det_insert_typeclass_info_var(prog_constraint::in, prog_var::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % Set the prog_var which contains the typeclass_info for a given
    % constraint, overwriting any previous information.
    %
:- pred rtti_set_typeclass_info_var(prog_constraint::in, prog_var::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % Make the given typeclass_info var available for reuse in later
    % goals.  Abort if we know nothing about this variable.
    %
:- pred rtti_reuse_typeclass_info_var(prog_var::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % For a prog_var which holds a type_info, set the type that the
    % type_info is for.  Abort if such information already exists.
    %
:- pred rtti_det_insert_type_info_type(prog_var::in, mer_type::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % For a prog_var which holds a type_info, set the type that the
    % type_info is for, overwriting any previous information.
    %
:- pred rtti_set_type_info_type(prog_var::in, mer_type::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % rtti_var_info_duplicate(Var, NewVar, !RttiVarMaps)
    %
    % Duplicate the rtti_var_info we have about Var for NewVar.
    %
:- pred rtti_var_info_duplicate(prog_var::in, prog_var::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % Returns all of the tvars that we have information about in the
    % rtti_varmaps structure.
    %
:- pred rtti_varmaps_tvars(rtti_varmaps::in, list(tvar)::out) is det.

    % Returns all of the types that we have information about in the
    % rtti_varmaps structure, including those types which appear in the
    % arguments of constraints.
    %
:- pred rtti_varmaps_types(rtti_varmaps::in, list(mer_type)::out) is det.

    % Returns all of the prog_constraints which have typeclass_infos
    % stored in a prog_var we can reuse.
    %
:- pred rtti_varmaps_reusable_constraints(rtti_varmaps::in,
    list(prog_constraint)::out) is det.

    % Returns all of the prog_vars which are known to contain a type_info
    % or typeclass_info.
    %
:-  pred rtti_varmaps_rtti_prog_vars(rtti_varmaps::in, list(prog_var)::out)
    is det.

    % apply_substitutions_to_rtti_varmaps(TRenaming, TSubst, Subst,
    %   !RttiVarMaps)
    %
    % Apply substitutions to the rtti_varmaps data.  TRenaming is applied
    % to all types first, then TSubst is applied to all types.  Subst
    % is applied to all prog_vars.
    %
:- pred apply_substitutions_to_rtti_varmaps(tvar_renaming::in, tsubst::in,
    map(prog_var, prog_var)::in, rtti_varmaps::in, rtti_varmaps::out)
    is det.

    % rtti_varmaps_transform_types(Pred, !RttiVarMaps)
    %
    % Apply the transformation predicate to every type appearing in the
    % rtti_varmaps structure, including those in the arguments of constraints.
    %
:- pred rtti_varmaps_transform_types(
    pred(mer_type, mer_type)::in(pred(in, out) is det),
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % rtti_varmaps_overlay(A, B, C)
    %
    % Merge the information in rtti_varmaps A and B to produce C.  Where
    % information conflicts, use the information in B rather than A.
    %
:- pred rtti_varmaps_overlay(rtti_varmaps::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % For a set of variables V, find all the type variables in the types
    % of the variables in V, and return set of typeinfo variables for
    % those type variables. (find all typeinfos for variables in V).
    %
    % This set of typeinfos is often needed in liveness computation
    % for accurate garbage collection - live variables need to have
    % their typeinfos stay live too.
    %
:- pred get_typeinfo_vars(set(prog_var)::in, vartypes::in, rtti_varmaps::in,
    set(prog_var)::out) is det.

:- pred maybe_complete_with_typeinfo_vars(set(prog_var)::in,
    bool::in, vartypes::in, rtti_varmaps::in, set(prog_var)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module solutions.
:- import_module svmap.
:- import_module term.

%-----------------------------------------------------------------------------%

type_info_locn_var(type_info(Var), Var).
type_info_locn_var(typeclass_info(Var, _), Var).

type_info_locn_set_var(Var, type_info(_), type_info(Var)).
type_info_locn_set_var(Var, typeclass_info(_, Num), typeclass_info(Var, Num)).

:- type rtti_varmaps
    --->    rtti_varmaps(
                tci_varmap          :: typeclass_info_varmap,
                ti_varmap           :: type_info_varmap,
                ti_type_map         :: type_info_type_map,
                tci_constraint_map  :: typeclass_info_constraint_map
            ).

    % A typeclass_info_varmap is a map which for each type class constraint
    % records which variable contains the typeclass_info for that
    % constraint.  The constraints covered by this map are those which
    % are passed in as head arguments and those which are produced as
    % existential constraints from calls or deconstructions.  These are
    % the constraints for which it is safe to reuse the variable associated
    % with the constraint.
    %
:- type typeclass_info_varmap == map(prog_constraint, prog_var).

    % A type_info_varmap is a map which for each type variable
    % records where the type_info for that type variable is stored.
    %
    % XXX this doesn't record the information that we want.  For a
    % constraint such as foo(list(T)) we can't properly record the
    % location of the type_info for T, since it does not occupy a slot
    % in the typeclass_info directly, but is inside the type_info for
    % list(T).
    %
:- type type_info_varmap == map(tvar, type_info_locn).

    % Every program variable which holds a type_info is a key in this
    % map.  The value associated with a given key is the type that the
    % type_info is for.
    %
:- type type_info_type_map == map(prog_var, mer_type).

    % Every program variable which holds a typeclass_info is a key in this
    % map.  The value associated with a given key is the prog_constraint
    % that the typeclass_info is for.
    %
:- type typeclass_info_constraint_map == map(prog_var, prog_constraint).

rtti_varmaps_init(rtti_varmaps(TCIMap, TIMap, TypeMap, ConstraintMap)) :-
    map.init(TCIMap),
    map.init(TIMap),
    map.init(TypeMap),
    map.init(ConstraintMap).

rtti_varmaps_no_tvars(VarMaps) :-
    map.is_empty(VarMaps ^ ti_varmap).

rtti_lookup_type_info_locn(VarMaps, TVar, Locn) :-
    map.lookup(VarMaps ^ ti_varmap, TVar, Locn).

rtti_search_type_info_locn(VarMaps, TVar, Locn) :-
    map.search(VarMaps ^ ti_varmap, TVar, Locn).

rtti_lookup_typeclass_info_var(VarMaps, Constraint, ProgVar) :-
    map.lookup(VarMaps ^ tci_varmap, Constraint, ProgVar).

rtti_search_typeclass_info_var(VarMaps, Constraint, ProgVar) :-
    map.search(VarMaps ^ tci_varmap, Constraint, ProgVar).

rtti_varmaps_var_info(VarMaps, Var, VarInfo) :-
    ( map.search(VarMaps ^ ti_type_map, Var, Type) ->
        VarInfo = type_info_var(Type)
    ; map.search(VarMaps ^ tci_constraint_map, Var, Constraint) ->
        VarInfo = typeclass_info_var(Constraint)
    ;
        VarInfo = non_rtti_var
    ).

rtti_det_insert_type_info_locn(TVar, Locn, !VarMaps) :-
    Map0 = !.VarMaps ^ ti_varmap,
    map.det_insert(Map0, TVar, Locn, Map),
    !:VarMaps = !.VarMaps ^ ti_varmap := Map,
    maybe_check_type_info_var(Locn, TVar, !VarMaps).

rtti_set_type_info_locn(TVar, Locn, !VarMaps) :-
    Map0 = !.VarMaps ^ ti_varmap,
    map.set(Map0, TVar, Locn, Map),
    !:VarMaps = !.VarMaps ^ ti_varmap := Map,
    maybe_check_type_info_var(Locn, TVar, !VarMaps).

:- pred maybe_check_type_info_var(type_info_locn::in, tvar::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

maybe_check_type_info_var(type_info(Var), TVar, !VarMaps) :-
    ( map.search(!.VarMaps ^ ti_type_map, Var, Type) ->
        ( Type = type_variable(TVar, _) ->
            true
        ;
            unexpected(this_file, "inconsistent info in rtti_varmaps")
        )
    ;
        unexpected(this_file, "missing info in rtti_varmaps")
    ).
maybe_check_type_info_var(typeclass_info(_, _), _, !VarMaps).

rtti_det_insert_typeclass_info_var(Constraint, ProgVar, !VarMaps) :-
    Map0 = !.VarMaps ^ tci_constraint_map,
    map.det_insert(Map0, ProgVar, Constraint, Map),
    !:VarMaps = !.VarMaps ^ tci_constraint_map := Map.

rtti_set_typeclass_info_var(Constraint, ProgVar, !VarMaps) :-
    Map0 = !.VarMaps ^ tci_constraint_map,
    map.set(Map0, ProgVar, Constraint, Map),
    !:VarMaps = !.VarMaps ^ tci_constraint_map := Map.

rtti_reuse_typeclass_info_var(ProgVar, !VarMaps) :-
    ( map.search(!.VarMaps ^ tci_constraint_map, ProgVar, Constraint) ->
        Map0 = !.VarMaps ^ tci_varmap,
        map.set(Map0, Constraint, ProgVar, Map),
        !:VarMaps = !.VarMaps ^ tci_varmap := Map
    ;
        unexpected(this_file,
            "rtti_reuse_typeclass_info_var: variable not known")
    ).

rtti_det_insert_type_info_type(ProgVar, Type, !VarMaps) :-
    Map0 = !.VarMaps ^ ti_type_map,
    map.det_insert(Map0, ProgVar, Type, Map),
    !:VarMaps = !.VarMaps ^ ti_type_map := Map.

rtti_set_type_info_type(ProgVar, Type, !VarMaps) :-
    Map0 = !.VarMaps ^ ti_type_map,
    map.set(Map0, ProgVar, Type, Map),
    !:VarMaps = !.VarMaps ^ ti_type_map := Map.

rtti_var_info_duplicate(Var, NewVar, !VarMaps) :-
    rtti_varmaps_var_info(!.VarMaps, Var, VarInfo),
    (
        VarInfo = type_info_var(Type),
        rtti_det_insert_type_info_type(NewVar, Type, !VarMaps)
    ;
        VarInfo = typeclass_info_var(Constraint),
        rtti_det_insert_typeclass_info_var(Constraint, NewVar, !VarMaps)
    ;
        VarInfo = non_rtti_var
    ).

rtti_varmaps_tvars(VarMaps, TVars) :-
    map.keys(VarMaps ^ ti_varmap, TVars).

rtti_varmaps_types(VarMaps, Types) :-
    solutions.solutions(rtti_varmaps_is_known_type(VarMaps), Types).

:- pred rtti_varmaps_is_known_type(rtti_varmaps::in, mer_type::out) is nondet.

rtti_varmaps_is_known_type(VarMaps, Type) :-
    map.values(VarMaps ^ ti_type_map, Types),
    list.member(Type, Types).
rtti_varmaps_is_known_type(VarMaps, Type) :-
    map.values(VarMaps ^ tci_constraint_map, Constraints),
    list.member(constraint(_, Types), Constraints),
    list.member(Type, Types).

rtti_varmaps_reusable_constraints(VarMaps, Constraints) :-
    map.keys(VarMaps ^ tci_varmap, Constraints).

rtti_varmaps_rtti_prog_vars(VarMaps, Vars) :-
    map.keys(VarMaps ^ ti_type_map, TIVars),
    map.keys(VarMaps ^ tci_constraint_map, TCIVars),
    list.append(TIVars, TCIVars, Vars).

apply_substitutions_to_rtti_varmaps(TRenaming, TSubst, Subst, !RttiVarMaps) :-
    (
        % Optimize the simple case.
        map.is_empty(Subst),
        map.is_empty(TSubst),
        map.is_empty(TRenaming)
    ->
        true
    ;
        !.RttiVarMaps = rtti_varmaps(TCIMap0, TIMap0, TypeMap0,
            ConstraintMap0),
        map.foldl(apply_substs_to_tci_map(TRenaming, TSubst, Subst),
            TCIMap0, map.init, TCIMap),
        map.foldl(apply_substs_to_ti_map(TRenaming, TSubst, Subst),
            TIMap0, map.init, TIMap),
        map.foldl(apply_substs_to_type_map(TRenaming, TSubst, Subst),
            TypeMap0, map.init, TypeMap),
        map.foldl(apply_substs_to_constraint_map(TRenaming, TSubst, Subst),
            ConstraintMap0, map.init, ConstraintMap),
        !:RttiVarMaps = rtti_varmaps(TCIMap, TIMap, TypeMap, ConstraintMap)
    ).

:- pred apply_subst_to_prog_var(map(prog_var, prog_var)::in,
    prog_var::in, prog_var::out) is det.

apply_subst_to_prog_var(Subst, Var0, Var) :-
    ( map.search(Subst, Var0, Var1) ->
        Var = Var1
    ;
        Var = Var0
    ).

:- pred apply_substs_to_tci_map(tvar_renaming::in, tsubst::in,
    map(prog_var, prog_var)::in, prog_constraint::in, prog_var::in,
    typeclass_info_varmap::in, typeclass_info_varmap::out) is det.

apply_substs_to_tci_map(TRenaming, TSubst, Subst, Constraint0, Var0, !Map) :-
    apply_variable_renaming_to_prog_constraint(TRenaming, Constraint0,
        Constraint1),
    apply_rec_subst_to_prog_constraint(TSubst, Constraint1, Constraint),
    apply_subst_to_prog_var(Subst, Var0, Var),
    svmap.set(Constraint, Var, !Map).

    % Update a map entry from tvar to type_info_locn, using the type renaming
    % and substitution to rename tvars and a variable substitution to rename
    % vars. The type renaming is applied before the type substitution.
    %
    % If tvar maps to a another type variable, we keep the new variable, if
    % it maps to a type, we remove it from the map.
    %
:- pred apply_substs_to_ti_map(tvar_renaming::in, tsubst::in,
    map(prog_var, prog_var)::in, tvar::in, type_info_locn::in,
    type_info_varmap::in, type_info_varmap::out) is det.

apply_substs_to_ti_map(TRenaming, TSubst, Subst, TVar, Locn, !Map) :-
    type_info_locn_var(Locn, Var),
    apply_subst_to_prog_var(Subst, Var, NewVar),
    type_info_locn_set_var(NewVar, Locn, NewLocn),
    apply_variable_renaming_to_tvar(TRenaming, TVar, NewTVar1),
    % We don't use the correct kinds here, but that doesn't matter because
    % the resulting kind will be thrown away anyway.
    apply_rec_subst_to_tvar(map.init, TSubst, NewTVar1, NewType),
    (
        % If the tvar is still a variable, insert it into the map with the
        % new var.
        NewType = type_variable(NewTVar, _)
    ->
        % Don't abort if two old type variables map to the same new type
        % variable.
        svmap.set(NewTVar, NewLocn, !Map)
    ;
        true
    ).

:- pred apply_substs_to_type_map(tvar_renaming::in, tsubst::in,
    map(prog_var, prog_var)::in, prog_var::in, mer_type::in,
    type_info_type_map::in, type_info_type_map::out) is det.

apply_substs_to_type_map(TRenaming, TSubst, Subst, Var0, Type0, !Map) :-
    apply_variable_renaming_to_type(TRenaming, Type0, Type1),
    apply_rec_subst_to_type(TSubst, Type1, Type),
    apply_subst_to_prog_var(Subst, Var0, Var),
    ( map.search(!.Map, Var, ExistingType) ->
        ( Type = ExistingType ->
            true
        ;
            unexpected(this_file, "inconsistent type_infos")
        )
    ;
        svmap.det_insert(Var, Type, !Map)
    ).

:- pred apply_substs_to_constraint_map(tvar_renaming::in, tsubst::in,
    map(prog_var, prog_var)::in, prog_var::in, prog_constraint::in,
    typeclass_info_constraint_map::in, typeclass_info_constraint_map::out)
    is det.

apply_substs_to_constraint_map(TRenaming, TSubst, Subst, Var0, Constraint0,
        !Map) :-
    apply_variable_renaming_to_prog_constraint(TRenaming, Constraint0,
        Constraint1),
    apply_rec_subst_to_prog_constraint(TSubst, Constraint1, Constraint),
    apply_subst_to_prog_var(Subst, Var0, Var),
    ( map.search(!.Map, Var, ExistingConstraint) ->
        ( Constraint = ExistingConstraint ->
            true
        ;
            unexpected(this_file, "inconsistent typeclass_infos")
        )
    ;
        svmap.det_insert(Var, Constraint, !Map)
    ).

rtti_varmaps_transform_types(Pred, !RttiVarMaps) :-
    TciMap0 = !.RttiVarMaps ^ tci_varmap,
    TypeMap0 = !.RttiVarMaps ^ ti_type_map,
    ConstraintMap0 = !.RttiVarMaps ^ tci_constraint_map,
    map.foldl(apply_constraint_key_transformation(Pred), TciMap0,
        map.init, TciMap),
    Pred2 = (pred(_::in, V::in, W::out) is det :-
            Pred(V, W)
    ),
    map.map_values(Pred2, TypeMap0, TypeMap),
    map.map_values(apply_constraint_value_transformation(Pred),
        ConstraintMap0, ConstraintMap),
    !:RttiVarMaps = !.RttiVarMaps ^ tci_varmap := TciMap,
    !:RttiVarMaps = !.RttiVarMaps ^ ti_type_map := TypeMap,
    !:RttiVarMaps = !.RttiVarMaps ^ tci_constraint_map := ConstraintMap.

:- pred apply_constraint_key_transformation(
    pred(mer_type, mer_type)::in(pred(in, out) is det),
    prog_constraint::in, prog_var::in,
    typeclass_info_varmap::in, typeclass_info_varmap::out) is det.

apply_constraint_key_transformation(Pred, Constraint0, Var, !Map) :-
    Constraint0 = constraint(Name, Args0),
    list.map(Pred, Args0, Args),
    Constraint = constraint(Name, Args),
    svmap.set(Constraint, Var, !Map).

:- pred apply_constraint_value_transformation(
    pred(mer_type, mer_type)::in(pred(in, out) is det),
    prog_var::in, prog_constraint::in, prog_constraint::out) is det.

apply_constraint_value_transformation(Pred, _, Constraint0, Constraint) :-
    Constraint0 = constraint(Name, Args0),
    list.map(Pred, Args0, Args),
    Constraint = constraint(Name, Args).

rtti_varmaps_overlay(VarMapsA, VarMapsB, VarMaps) :-
    VarMapsA = rtti_varmaps(TCImapA, TImapA, TypeMapA, ConstraintMapA),
    VarMapsB = rtti_varmaps(TCImapB, TImapB, TypeMapB, ConstraintMapB),

        % Prefer VarMapsB for this information.
        %
    map.overlay(TCImapA, TCImapB, TCImap),
    map.overlay(TImapA, TImapB, TImap),

        % On the other hand, we insist that this information is consistent.
        %
    map.old_merge(TypeMapA, TypeMapB, TypeMap),
    map.old_merge(ConstraintMapA, ConstraintMapB, ConstraintMap),

    VarMaps = rtti_varmaps(TCImap, TImap, TypeMap, ConstraintMap).

%-----------------------------------------------------------------------------%

get_typeinfo_vars(Vars, VarTypes, RttiVarMaps, TypeInfoVars) :-
    TVarMap = RttiVarMaps ^ ti_varmap,
    set.to_sorted_list(Vars, VarList),
    get_typeinfo_vars_2(VarList, VarTypes, TVarMap, TypeInfoVarList),
    set.list_to_set(TypeInfoVarList, TypeInfoVars).

    % Auxiliary predicate - traverses variables and builds a list of
    % variables that store typeinfos for these variables.
    %
:- pred get_typeinfo_vars_2(list(prog_var)::in,
    vartypes::in, type_info_varmap::in, list(prog_var)::out) is det.

get_typeinfo_vars_2([], _, _, []).
get_typeinfo_vars_2([Var | Vars], VarTypes, TVarMap, TypeInfoVars) :-
    ( map.search(VarTypes, Var, Type) ->
        type_vars(Type, TypeVars),
        (
            TypeVars = [],
            % Optimize common case,
            get_typeinfo_vars_2(Vars, VarTypes, TVarMap, TypeInfoVars)
        ;
            TypeVars = [_ | _],
            % XXX It's possible there are some complications with
            % higher order pred types here -- if so, maybe
            % treat them specially.

            % The type_info is either stored in a variable, or in a
            % typeclass_info.  Either get the type_info variable or
            % the typeclass_info variable.
            LookupVar = (pred(TVar::in, TVarVar::out) is det :-
                map.lookup(TVarMap, TVar, Locn),
                type_info_locn_var(Locn, TVarVar)
            ),
            list.map(LookupVar, TypeVars, TypeInfoVarsHead),

            get_typeinfo_vars_2(Vars, VarTypes, TVarMap, TypeInfoVarsTail),
            TypeInfoVars = TypeInfoVarsHead ++ TypeInfoVarsTail
        )
    ;
        unexpected(this_file, "get_typeinfo_vars_2: var not found in typemap")
    ).

maybe_complete_with_typeinfo_vars(Vars0, TypeInfoLiveness, VarTypes,
        RttiVarMaps, Vars) :-
    (
        TypeInfoLiveness = yes,
        get_typeinfo_vars(Vars0, VarTypes, RttiVarMaps, TypeInfoVars),
        set.union(Vars0, TypeInfoVars, Vars)
    ;
        TypeInfoLiveness = no,
        Vars = Vars0
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "hlds_rtti.m".

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_rtti.
%-----------------------------------------------------------------------------%
