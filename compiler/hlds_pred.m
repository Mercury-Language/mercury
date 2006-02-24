%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: hlds_pred.m.
% Main authors: fjh, conway.

% This module defines the part of the HLDS that deals with predicates
% and procedures.

%-----------------------------------------------------------------------------%

:- module hlds__hlds_pred.
:- interface.

:- import_module check_hlds.mode_constraint_robdd.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.mode_errors.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.inst_graph.
:- import_module hlds.special_pred.
:- import_module hlds.instmap.
:- import_module libs.globals.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.term_constr_main.
:- import_module transform_hlds.term_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.make_hlds.
:- import_module libs.compiler_util.
:- import_module libs.options.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.

% Standard library modules.
:- import_module int.
:- import_module string.
:- import_module svmap.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- interface.

    % A proc_id is the name of a mode within a particular predicate -
    % not to be confused with a mode_id, which is the name of a
    % user-defined mode.

:- type pred_id.
:- type proc_id.
:- type pred_proc_id    --->    proc(pred_id, proc_id).

    % Predicate and procedure ids are abstract data types. One important
    % advantage of this arrangement is to make it harder to accidentally
    % use an integer in their place. However, you can convert between
    % integers and pred_ids/proc_ids with the following predicates and
    % functions.
    %
:- func shroud_pred_id(pred_id) = shrouded_pred_id.
:- func shroud_proc_id(proc_id) = shrouded_proc_id.
:- func shroud_pred_proc_id(pred_proc_id) = shrouded_pred_proc_id.

:- func unshroud_pred_id(shrouded_pred_id) = pred_id.
:- func unshroud_proc_id(shrouded_proc_id) = proc_id.
:- func unshroud_pred_proc_id(shrouded_pred_proc_id) = pred_proc_id.

:- pred pred_id_to_int(pred_id, int).
:- mode pred_id_to_int(in, out) is det.
:- mode pred_id_to_int(out, in) is det.
:- func pred_id_to_int(pred_id) = int.

:- pred proc_id_to_int(proc_id, int).
:- mode proc_id_to_int(in, out) is det.
:- mode proc_id_to_int(out, in) is det.
:- func proc_id_to_int(proc_id) = int.

    % Return the id of the first predicate in a module, and of the first
    % procedure in a predicate.
    %
:- func initial_pred_id = pred_id.
:- func initial_proc_id = proc_id.

    % Return an invalid predicate or procedure id. These are intended to be
    % used to initialize the relevant fields in in call(...) goals before
    % we do type- and mode-checks, or when those check find that there was
    % no predicate matching the call.
    %
:- func invalid_pred_id = pred_id.
:- func invalid_proc_id = proc_id.

:- pred next_pred_id(pred_id::in, pred_id::out) is det.

    % For semidet complicated unifications with mode (in, in), these are
    % defined to have the same proc_id (0). This returns that proc_id.
    %
:- pred in_in_unification_proc_id(proc_id::out) is det.

:- type pred_info.
:- type proc_info.

:- type proc_table  ==  map(proc_id, proc_info).

:- pred next_mode_id(proc_table::in, proc_id::out) is det.

:- type call_id
    --->    call(simple_call_id)
    ;       generic_call(generic_call_id).

:- type generic_call_id
    --->    higher_order(purity, pred_or_func, arity)
    ;       class_method(class_id, simple_call_id)
    ;       cast(cast_type).

:- type pred_proc_list  ==  list(pred_proc_id).

:- type prog_var_name == string.

    % The rtti_proc_label type holds all the information about a procedure
    % that we need to compute the entry label for that procedure
    % in the target language (the llds__code_addr or mlds__code_addr).

:- type rtti_proc_label --->
    rtti_proc_label(
        pred_or_func            ::  pred_or_func,
        this_module             ::  module_name,
        proc_module             ::  module_name,
        proc_name               ::  string,
        proc_arity              ::  arity,
        proc_arg_types          ::  list(mer_type),
        pred_id                 ::  pred_id,
        proc_id                 ::  proc_id,
        proc_headvars           ::  assoc_list(prog_var, prog_var_name),
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
                list(mer_type),             % The types in the head of the
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

%-----------------------------------------------------------------------------%

:- implementation.

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
    map__init(TCIMap),
    map__init(TIMap),
    map__init(TypeMap),
    map__init(ConstraintMap).

rtti_varmaps_no_tvars(VarMaps) :-
    map__is_empty(VarMaps ^ ti_varmap).

rtti_lookup_type_info_locn(VarMaps, TVar, Locn) :-
    map__lookup(VarMaps ^ ti_varmap, TVar, Locn).

rtti_search_type_info_locn(VarMaps, TVar, Locn) :-
    map__search(VarMaps ^ ti_varmap, TVar, Locn).

rtti_lookup_typeclass_info_var(VarMaps, Constraint, ProgVar) :-
    map__lookup(VarMaps ^ tci_varmap, Constraint, ProgVar).

rtti_search_typeclass_info_var(VarMaps, Constraint, ProgVar) :-
    map__search(VarMaps ^ tci_varmap, Constraint, ProgVar).

rtti_varmaps_var_info(VarMaps, Var, VarInfo) :-
    ( map__search(VarMaps ^ ti_type_map, Var, Type) ->
        VarInfo = type_info_var(Type)
    ; map__search(VarMaps ^ tci_constraint_map, Var, Constraint) ->
        VarInfo = typeclass_info_var(Constraint)
    ;
        VarInfo = non_rtti_var
    ).

rtti_det_insert_type_info_locn(TVar, Locn, !VarMaps) :-
    Map0 = !.VarMaps ^ ti_varmap,
    map__det_insert(Map0, TVar, Locn, Map),
    !:VarMaps = !.VarMaps ^ ti_varmap := Map,
    maybe_check_type_info_var(Locn, TVar, !VarMaps).

rtti_set_type_info_locn(TVar, Locn, !VarMaps) :-
    Map0 = !.VarMaps ^ ti_varmap,
    map__set(Map0, TVar, Locn, Map),
    !:VarMaps = !.VarMaps ^ ti_varmap := Map,
    maybe_check_type_info_var(Locn, TVar, !VarMaps).

:- pred maybe_check_type_info_var(type_info_locn::in, tvar::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

maybe_check_type_info_var(type_info(Var), TVar, !VarMaps) :-
    ( map__search(!.VarMaps ^ ti_type_map, Var, Type) ->
        ( Type = variable(TVar, _) ->
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
    map__det_insert(Map0, ProgVar, Constraint, Map),
    !:VarMaps = !.VarMaps ^ tci_constraint_map := Map.

rtti_set_typeclass_info_var(Constraint, ProgVar, !VarMaps) :-
    Map0 = !.VarMaps ^ tci_constraint_map,
    map__set(Map0, ProgVar, Constraint, Map),
    !:VarMaps = !.VarMaps ^ tci_constraint_map := Map.

rtti_reuse_typeclass_info_var(ProgVar, !VarMaps) :-
    ( map__search(!.VarMaps ^ tci_constraint_map, ProgVar, Constraint) ->
        Map0 = !.VarMaps ^ tci_varmap,
        map__set(Map0, Constraint, ProgVar, Map),
        !:VarMaps = !.VarMaps ^ tci_varmap := Map
    ;
        unexpected(this_file,
            "rtti_reuse_typeclass_info_var: variable not known")
    ).

rtti_det_insert_type_info_type(ProgVar, Type, !VarMaps) :-
    Map0 = !.VarMaps ^ ti_type_map,
    map__det_insert(Map0, ProgVar, Type, Map),
    !:VarMaps = !.VarMaps ^ ti_type_map := Map.

rtti_set_type_info_type(ProgVar, Type, !VarMaps) :-
    Map0 = !.VarMaps ^ ti_type_map,
    map__set(Map0, ProgVar, Type, Map),
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
    map__keys(VarMaps ^ ti_varmap, TVars).

rtti_varmaps_types(VarMaps, Types) :-
    solutions(rtti_varmaps_is_known_type(VarMaps), Types).

:- pred rtti_varmaps_is_known_type(rtti_varmaps::in, mer_type::out) is nondet.

rtti_varmaps_is_known_type(VarMaps, Type) :-
    map__values(VarMaps ^ ti_type_map, Types),
    list__member(Type, Types).
rtti_varmaps_is_known_type(VarMaps, Type) :-
    map__values(VarMaps ^ tci_constraint_map, Constraints),
    list__member(constraint(_, Types), Constraints),
    list__member(Type, Types).

rtti_varmaps_reusable_constraints(VarMaps, Constraints) :-
    map__keys(VarMaps ^ tci_varmap, Constraints).

rtti_varmaps_rtti_prog_vars(VarMaps, Vars) :-
    map__keys(VarMaps ^ ti_type_map, TIVars),
    map__keys(VarMaps ^ tci_constraint_map, TCIVars),
    list__append(TIVars, TCIVars, Vars).

apply_substitutions_to_rtti_varmaps(TRenaming, TSubst, Subst, !RttiVarMaps) :-
    (
        % Optimize the simple case.
        map__is_empty(Subst),
        map__is_empty(TSubst),
        map__is_empty(TRenaming)
    ->
        true
    ;
        !.RttiVarMaps = rtti_varmaps(TCIMap0, TIMap0, TypeMap0,
            ConstraintMap0),
        map__foldl(apply_substs_to_tci_map(TRenaming, TSubst, Subst),
            TCIMap0, map__init, TCIMap),
        map__foldl(apply_substs_to_ti_map(TRenaming, TSubst, Subst),
            TIMap0, map__init, TIMap),
        map__foldl(apply_substs_to_type_map(TRenaming, TSubst, Subst),
            TypeMap0, map__init, TypeMap),
        map__foldl(apply_substs_to_constraint_map(TRenaming, TSubst, Subst),
            ConstraintMap0, map__init, ConstraintMap),
        !:RttiVarMaps = rtti_varmaps(TCIMap, TIMap, TypeMap, ConstraintMap)
    ).

:- pred apply_subst_to_prog_var(map(prog_var, prog_var)::in,
    prog_var::in, prog_var::out) is det.

apply_subst_to_prog_var(Subst, Var0, Var) :-
    ( map__search(Subst, Var0, Var1) ->
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
    svmap__set(Constraint, Var, !Map).

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
    apply_rec_subst_to_tvar(map__init, TSubst, NewTVar1, NewType),
    (
        % If the tvar is still a variable, insert it into the map with the
        % new var.
        NewType = variable(NewTVar, _)
    ->
        % Don't abort if two old type variables map to the same new type
        % variable.
        svmap__set(NewTVar, NewLocn, !Map)
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
    ( map__search(!.Map, Var, ExistingType) ->
        ( Type = ExistingType ->
            true
        ;
            unexpected(this_file, "inconsistent type_infos")
        )
    ;
        svmap__det_insert(Var, Type, !Map)
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
    ( map__search(!.Map, Var, ExistingConstraint) ->
        ( Constraint = ExistingConstraint ->
            true
        ;
            unexpected(this_file, "inconsistent typeclass_infos")
        )
    ;
        svmap__det_insert(Var, Constraint, !Map)
    ).

rtti_varmaps_transform_types(Pred, !RttiVarMaps) :-
    TciMap0 = !.RttiVarMaps ^ tci_varmap,
    TypeMap0 = !.RttiVarMaps ^ ti_type_map,
    ConstraintMap0 = !.RttiVarMaps ^ tci_constraint_map,
    map__foldl(apply_constraint_key_transformation(Pred), TciMap0,
    map__init, TciMap),
    Pred2 = (pred(_::in, V::in, W::out) is det :-
            Pred(V, W)
    ),
    map__map_values(Pred2, TypeMap0, TypeMap),
    map__map_values(apply_constraint_value_transformation(Pred),
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
    list__map(Pred, Args0, Args),
    Constraint = constraint(Name, Args),
    svmap__set(Constraint, Var, !Map).

:- pred apply_constraint_value_transformation(
    pred(mer_type, mer_type)::in(pred(in, out) is det),
    prog_var::in, prog_constraint::in, prog_constraint::out) is det.

apply_constraint_value_transformation(Pred, _, Constraint0, Constraint) :-
    Constraint0 = constraint(Name, Args0),
    list__map(Pred, Args0, Args),
    Constraint = constraint(Name, Args).

rtti_varmaps_overlay(VarMapsA, VarMapsB, VarMaps) :-
    VarMapsA = rtti_varmaps(TCImapA, TImapA, TypeMapA, ConstraintMapA),
    VarMapsB = rtti_varmaps(TCImapB, TImapB, TypeMapB, ConstraintMapB),

        % Prefer VarMapsB for this information.
        %
    map__overlay(TCImapA, TCImapB, TCImap),
    map__overlay(TImapA, TImapB, TImap),

        % On the other hand, we insist that this information is consistent.
        %
    map__merge(TypeMapA, TypeMapB, TypeMap),
    map__merge(ConstraintMapA, ConstraintMapB, ConstraintMap),

    VarMaps = rtti_varmaps(TCImap, TImap, TypeMap, ConstraintMap).

%-----------------------------------------------------------------------------%

:- interface.

    % The clauses_info structure contains the clauses for a predicate
    % after conversion from the item_list by make_hlds.m.
    % Typechecking is performed on the clauses info, then the clauses
    % are copied to create the proc_info for each procedure.
    % After mode analysis the clauses and the procedure goals are not
    % guaranteed to be the same, and the clauses are only kept so that
    % the optimized goal can be compared with the original in HLDS dumps.
:- type clauses_info --->
    clauses_info(
        varset                  :: prog_varset,
                                % variable names
        explicit_vartypes       :: vartypes,
                                % variable types from explicit qualifications
        tvar_name_map           :: tvar_name_map,
                                % map from variable name to type variable
                                % for the type variables occurring in the
                                % argument types. This is used to process
                                % explicit type qualifications.
        vartypes                :: vartypes,
                                % variable types inferred by typecheck.m.
        headvars                :: list(prog_var),
                                % head vars
        clauses_rep             :: clauses_rep,
                                % the following field is computed by
                                % polymorphism.m
        clauses_rtti_varmaps    :: rtti_varmaps,
        have_foreign_clauses    :: bool
                                % do we have foreign language clauses?
    ).

:- pred clauses_info_init(int::in, clauses_info::out) is det.

:- pred clauses_info_init_for_assertion(prog_vars::in, clauses_info::out)
    is det.

:- type clauses_rep.

    % Returns yes iff the given clauses_rep represents the empty list of
    % clauses.
    %
:- func clause_list_is_empty(clauses_rep) = bool.

    % Adds the given clause to the end of the clause list.
    %
:- pred add_clause(clause::in, clauses_rep::in, clauses_rep::out) is det.

    % Get the list of clauses in the given clauses_rep in whatever order
    % happens to be efficient.
    %
:- pred get_clause_list_any_order(clauses_rep::in, list(clause)::out) is det.

    % Get the list of clauses in the given clauses_rep in program order.
    %
:- pred get_clause_list(clauses_rep::in, list(clause)::out) is det.

    % Set the list of clauses to the one given.
    %
:- pred set_clause_list(list(clause)::in, clauses_rep::out) is det.

:- pred clauses_info_varset(clauses_info::in, prog_varset::out) is det.

    % This partial map holds the types specified by any explicit
    % type qualifiers in the clauses.
    %
:- pred clauses_info_explicit_vartypes(clauses_info::in, vartypes::out) is det.

    % This map contains the types of all the variables, as inferred
    % by typecheck.m.
    %
:- pred clauses_info_vartypes(clauses_info::in, vartypes::out) is det.

:- pred clauses_info_rtti_varmaps(clauses_info::in, rtti_varmaps::out) is det.

:- pred clauses_info_headvars(clauses_info::in, list(prog_var)::out) is det.

:- pred clauses_info_clauses_rep(clauses_info::in, clauses_rep::out) is det.

    % Return the list of clauses in program order.
    %
:- pred clauses_info_clauses_only(clauses_info::in, list(clause)::out) is det.

    % Return the list of clauses in program order, and if necessary update
    % the cache of this info in the clauses_info.
    %
:- pred clauses_info_clauses(list(clause)::out,
    clauses_info::in, clauses_info::out) is det.

:- pred clauses_info_set_headvars(list(prog_var)::in,
    clauses_info::in, clauses_info::out) is det.

:- pred clauses_info_set_clauses(list(clause)::in,
    clauses_info::in, clauses_info::out) is det.

:- pred clauses_info_set_clauses_rep(clauses_rep::in,
    clauses_info::in, clauses_info::out) is det.

:- pred clauses_info_set_varset(prog_varset::in,
    clauses_info::in, clauses_info::out) is det.

    % This partial map holds the types specified by any explicit
    % type qualifiers in the clauses.
    %
:- pred clauses_info_set_explicit_vartypes(vartypes::in,
    clauses_info::in, clauses_info::out) is det.

    % This map contains the types of all the variables, as inferred
    % by typecheck.m.
    %
:- pred clauses_info_set_vartypes(vartypes::in,
    clauses_info::in, clauses_info::out) is det.

:- pred clauses_info_set_rtti_varmaps(rtti_varmaps::in,
    clauses_info::in, clauses_info::out) is det.

:- type clause --->
    clause(
        applicable_procs    :: list(proc_id),
                            % modes for which this clause applies (empty list
                            % means it applies to all modes)
        clause_body         :: hlds_goal,
        clause_lang         :: implementation_language,
        clause_context      :: prog_context
    ).

%-----------------------------------------------------------------------------%

:- implementation.

clauses_info_init(Arity, ClausesInfo) :-
    map__init(VarTypes),
    map__init(TVarNameMap),
    varset__init(VarSet0),
    make_n_fresh_vars("HeadVar__", Arity, HeadVars, VarSet0, VarSet),
    rtti_varmaps_init(RttiVarMaps),
    HasForeignClauses = no,
    set_clause_list([], ClausesRep),
    ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap, VarTypes,
        HeadVars, ClausesRep, RttiVarMaps, HasForeignClauses).

clauses_info_init_for_assertion(HeadVars, ClausesInfo) :-
    map__init(VarTypes),
    map__init(TVarNameMap),
    varset__init(VarSet),
    rtti_varmaps_init(RttiVarMaps),
    HasForeignClauses = no,
    set_clause_list([], ClausesRep),
    ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap, VarTypes,
        HeadVars, ClausesRep, RttiVarMaps, HasForeignClauses).

clauses_info_varset(CI, CI ^ varset).
clauses_info_explicit_vartypes(CI, CI ^ explicit_vartypes).
clauses_info_vartypes(CI, CI ^ vartypes).
clauses_info_headvars(CI, CI ^ headvars).
clauses_info_clauses_rep(CI, CI ^ clauses_rep).
clauses_info_rtti_varmaps(CI, CI ^ clauses_rtti_varmaps).

clauses_info_set_varset(X, CI, CI ^ varset := X).
clauses_info_set_explicit_vartypes(X, CI, CI ^ explicit_vartypes := X).
clauses_info_set_vartypes(X, CI, CI ^ vartypes := X).
clauses_info_set_headvars(X, CI, CI ^ headvars := X).
clauses_info_set_clauses(X, CI, CI ^ clauses_rep := forw(X)).
clauses_info_set_clauses_rep(X, CI, CI ^ clauses_rep := X).
clauses_info_set_rtti_varmaps(X, CI, CI ^ clauses_rtti_varmaps := X).

:- type clauses_rep
    --->    rev(list(clause))
    ;       forw(list(clause))
    ;       both(
                rev :: list(clause),
                forw :: list(clause)
            ).

clause_list_is_empty(ClausesRep) = IsEmpty :-
    (
        ClausesRep = rev(List)
    ;
        ClausesRep = forw(List)
    ;
        ClausesRep = both(List, _)
    ),
    (
        List = [],
        IsEmpty = yes
    ;
        List = [_ | _],
        IsEmpty = no
    ).

get_clause_list_any_order(ClausesRep, Clauses) :-
    (
        ClausesRep = rev(Clauses)
    ;
        ClausesRep = forw(Clauses)
    ;
        ClausesRep = both(_, Clauses)
    ).

get_clause_list(ClausesRep, Clauses) :-
    (
        ClausesRep = rev(RevClauses),
        list__reverse(RevClauses, Clauses)
    ;
        ClausesRep = forw(Clauses)
    ;
        ClausesRep = both(_, Clauses)
    ).

set_clause_list(Clauses, forw(Clauses)).

clauses_info_clauses_only(CI, Clauses) :-
    ClausesRep = CI ^ clauses_rep,
    get_clause_list(ClausesRep, Clauses).

clauses_info_clauses(Clauses, !CI) :-
    ClausesRep = !.CI ^ clauses_rep,
    (
        ClausesRep = rev(RevClauses),
        list__reverse(RevClauses, Clauses),
        !:CI = !.CI ^ clauses_rep := both(RevClauses, Clauses)
    ;
        ClausesRep = forw(Clauses)
    ;
        ClausesRep = both(_, Clauses)
    ).

add_clause(Clause, !ClausesRep) :-
    % We keep the clause list in reverse order, to make it possible
    % to add other clauses without quadratic behavior.
    (
        !.ClausesRep = rev(RevClauses0),
        RevClauses = [Clause | RevClauses0],
        !:ClausesRep = rev(RevClauses)
    ;
        !.ClausesRep = forw(Clauses0),
        list__reverse(Clauses0, RevClauses0),
        RevClauses = [Clause | RevClauses0],
        !:ClausesRep = rev(RevClauses)
    ;
        !.ClausesRep = both(RevClauses0, _),
        RevClauses = [Clause | RevClauses0],
        !:ClausesRep = rev(RevClauses)
    ).

%-----------------------------------------------------------------------------%

:- interface.

:- type implementation_language
    --->    mercury
    ;       foreign_language(foreign_language).

    % The type of goals that have been given for a pred.

:- type goal_type
    --->    pragmas         % pragma foreign_proc(...)
    ;       clauses
    ;       clauses_and_pragmas % both clauses and pragmas
    ;       promise(promise_type)
    ;       none.

    % Note: `liveness' and `liveness_info' record liveness in the sense
    % used by code generation.  This is *not* the same thing as the notion
    % of liveness used by mode analysis!  See compiler/notes/glossary.html.

:- type liveness_info   ==  set(prog_var).  % The live variables

:- type liveness
    --->    live
    ;       dead.

:- type arg_info   
    --->    arg_info(
                arg_loc,    % stored location
                arg_mode    % mode of top functor
            ).

    % The `arg_mode' specifies the mode of the top-level functor
    % (excluding `no_tag' functors, since those have no representation).
    % It is used by the code generators for determining how to
    % pass the argument.
    %
    % For the LLDS back-end, top_in arguments are passed in registers,
    % and top_out values are returned in registers; top_unused
    % values are not passed at all, but they are treated as if
    % they were top_out for the purpose of assigning arguments
    % to registers.  (So e.g. if a det procedure has three arguments
    % with arg_modes top_out, top_unused, and top_out respectively,
    % the last argument will be returned in register r3, not r2.)
    %
    % For the MLDS back-end, top_in values are passed as arguments;
    % top_out values are normally passed by reference, except that
    %   - if the procedure is model_nondet, and the --nondet-copy-out
    %     option is set, top_out values are passed by value to
    %     the continuation function
    %   - if the procedure is model_det or model_semi,
    %     and the --det-copy-out option is set,
    %     top_out arguments in the HLDS are mapped to (multiple)
    %     return values in the MLDS
    %   - if the HLDS function return value for a det function has
    %     mode `top_out', it is mapped to an MLDS return value.
    % top_unused arguments are not passed at all.
    %
:- type arg_mode
    --->    top_in
    ;       top_out
    ;       top_unused.

:- type arg_loc     ==  int.

    % The type `import_status' describes whether an entity (a predicate,
    % type, inst, or mode) is local to the current module, exported from
    % the current module, or imported from some other module.
    % Only predicates can have status pseudo_exported or pseudo_imported.
    % Only types can have status abstract_exported or abstract_imported.
    %
:- type import_status
    --->    external(import_status)
                % Declared `:- external'. This means that the implementation
                % for this procedure will be provided by some external source,
                % rather than via Mercury clauses (including `pragma
                % foreign_code' clauses). It can be through the use of another
                % language, or it could be through some other method we haven't
                % thought of yet.
    ;       imported(import_locn)
                % Defined in the interface of some other module.
    ;       opt_imported
                % Defined in the optimization interface of another module.
    ;       abstract_imported
                % Describes a type with only an abstract declaration imported,
                % maybe with the body of the type imported from a .opt file.
    ;       pseudo_imported
                % This is used for entities that are defined in the interface
                % of some other module but for which we may generate some code
                % in this module - in particular, this is used for unification
                % predicates (see comments in unify_proc.m).
    ;       exported
                % Defined in the interface of this module.
    ;       opt_exported
                % A local item for which the import-status has been changed
                % due to its presence in the .opt files
                % (intermod__adjust_pred_import_status).
    ;       abstract_exported
                % Describes a type with only an abstract declaration exported.
    ;       pseudo_exported
                % The converse of pseudo_imported; this means that only the
                % (in, in) mode of a unification is exported.
    ;       exported_to_submodules
                % Defined in the implementation of this module, and thus in
                % a sense local, but the module contains sub-modules, so the
                % entity needs to be exported to those sub-modules.
    ;       local.
                % Defined in the implementation of this module, and the module
                % does not contain any sub-modules.

    % Returns yes if the status indicates that the item was
    % in any way exported -- that is, if it could be used
    % by any other module, or by sub-modules of this module.
    %
:- pred status_is_exported(import_status::in, bool::out) is det.

    % Returns yes if the status indicates that the item was
    % exported to importing modules (not just to sub-modules).
    %
:- pred status_is_exported_to_non_submodules(import_status::in, bool::out)
    is det.

    % Returns yes if the status indicates that the item was
    % in any way imported -- that is, if it was defined in
    % some other module, or in a sub-module of this module.
    % This is the opposite of status_defined_in_this_module.
    %
:- pred status_is_imported(import_status::in, bool::out) is det.

    % Returns yes if the status indicates that the item was
    % defined in this module.  This is the opposite of
    % status_is_imported.
    %
:- pred status_defined_in_this_module(import_status::in, bool::out) is det.

    % Are calls from a predicate with the given import_status always fully
    % qualified. For calls occurring in `.opt' files this will return
    % `is_fully_qualified', otherwise `may_be_partially_qualified'.
    %
:- func calls_are_fully_qualified(pred_markers) = is_fully_qualified.

    % Predicates can be marked with various boolean flags, called "markers".

    % An abstract set of markers.
:- type pred_markers.

:- type marker
    --->    stub        % The predicate has no clauses. typecheck.m will
                        % generate a body for the predicate which just throws
                        % an exception. This marker is used to tell purity
                        % analysis and determinism analysis not to issue
                        % warnings for these predicates.

    ;       infer_type  % Requests type inference for the predicate.
                        % These markers are inserted by make_hlds
                        % for undeclared predicates.

    ;       infer_modes % Requests mode inference for the predicate.
                        % These markers are inserted by make_hlds
                        % for undeclared predicates.

    ;       obsolete    % Requests warnings if this predicate is used.
                        % Used for pragma(obsolete).


    ;       user_marked_inline
                        % The user requests that this be predicate should
                        % be inlined, even if it exceeds the usual size limits.
                        % Used for pragma(inline).
                        % Mutually exclusive with user_marked_no_inline.

    ;       user_marked_no_inline
                        % The user requests that this be predicate should
                        % not be inlined. Used for pragma(no_inline).
                        % Mutually exclusive with user_marked_inline.

    ;       heuristic_inline     
                        % The compiler (meaning probably inlining.m) requests
                        % that this predicate be inlined. Does not override
                        % user_marked_no_inline.

    ;       class_method
                        % Requests that this predicate be transformed into
                        % the appropriate call to a class method.

    ;       class_instance_method
                        % This predicate was automatically generated for the
                        % implementation of a class method for an instance.

    ;       named_class_instance_method
                        % This predicate was automatically generated for the
                        % implementation of a class method for an instance,
                        % and the instance was defined using the named syntax
                        % (e.g. "pred(...) is ...") rather than the clause
                        % syntax. (For such predicates, we output slightly
                        % different error messages.)

    ;       is_impure   % Requests that no transformation that would be
                        % inappropriate for impure code be performed on calls
                        % to this predicate. This includes reordering calls
                        % to it relative to other goals (in both conjunctions
                        % and disjunctions), and removing redundant calls
                        % to it.

    ;       is_semipure  % Requests that no transformation that would be
                        % inappropriate for semipure code be performed on
                        % calls to this predicate. This includes removing
                        % redundant calls to it on different sides of an
                        % impure goal.
    ;       promised_pure
                        % Requests that calls to this predicate be transformed
                        % as usual, despite any impure or semipure markers
                        % present.
    ;       promised_semipure
                        % Requests that calls to this predicate be treated as
                        % semipure, despite any impure calls in the body.

    % The terminates and does_not_terminate pragmas are kept as markers
    % to ensure that conflicting declarations are not made by the user.
    % Otherwise, the information could be added to the ProcInfos directly.

    ;       terminates  % The user guarantees that this predicate will
                        % terminate for all (finite?) input.
    ;       does_not_terminate
                        % States that this predicate does not terminate.
                        % This is useful for pragma foreign_code, which the
                        % compiler assumes to be terminating.
    ;       check_termination
                        % The user requires the compiler to guarantee
                        % the termination of this predicate. If the compiler
                        % cannot guarantee termination then it must give an
                        % error message.

    ;       calls_are_fully_qualified
                        % All calls in this predicate are fully qualified.
                        % This occurs for predicates read from `.opt' files
                        % and compiler-generated predicates.
    ;       mode_check_clauses.
                        % Each clause of the predicate should be modechecked
                        % separately. Used for predicates defined by lots of
                        % clauses (usually facts) for which the compiler's
                        % quadratic behavior during mode checking (in
                        % inst_match.bound_inst_list_contains_instname and
                        % instmap__merge) would be unacceptable.

    % An abstract set of attributes.
:- type pred_attributes.

:- type attribute
    --->    custom(mer_type).
                        % A custom attribute, indended to be associated
                        % with this predicate in the underlying
                        % implementation.

:- type pred_transformation
    --->    higher_order_specialization(
                int % Sequence number among the higher order
                    % specializations of the original predicate.
            )
    ;       higher_order_type_specialization(
                int % The procedure number of the original procedure.
            )
    ;       type_specialization(
                assoc_list(int, mer_type)
                    % The substitution from type variables (represented by
                    % the integers) to types (represented by the terms).
            )
    ;       unused_argument_elimination(
                list(int) % The list of eliminated argument numbers.
            )
    ;       accumulator(
                list(int)
                    % The list of the numbers of the variables in the original
                    % predicate interface that have been converted to
                    % accumulators.
            )
    ;       loop_invariant(
                int % The procedure number of the original procedure.
            )
    ;       tuple(
                int % The procedure number of the original procedure.
            )
    ;       untuple(
                int % The procedure number of the original procedure.
            )
    ;       return_via_ptr(
                proc_id,
                    % The id of the procedure this predicate is derived from.
                list(int)
                    % The arguments in these positions are returned via
                    % pointer.
            )
    ;       table_generator
    ;       dnf(
                int % This predicate was originally part of a predicate
                    % transformed into disjunctive normal form; this integer
                    % gives the part number.
            ).

:- type pred_creation
    --->    deforestation
                % I/O tabling will create a new predicate if the predicate
                % to be I/O tabled must not be inlined.
    ;       io_tabling.

:- type pred_origin
    --->    special_pred(special_pred)
                % If the predicate is a unify, compare, index or initialisation
                % predicate, specify which one, and for which type constructor.
    ;       instance_method(instance_method_constraints)
                % If this predicate is a class method implementation, record
                % extra information about the class context to allow
                % polymorphism.m to correctly set up the extra type_info
                % and typeclass_info arguments.
    ;       transformed(pred_transformation, pred_origin, pred_id)
                % The predicate is a transformed version of another predicate,
                % whose origin and identity are given by the second and third
                % arguments.
    ;       created(pred_creation)
                % The predicate was created by the compiler, and there is no
                % information available on where it came from.
    ;       assertion(string, int)
                % The predicate represents an assertion.
    ;       lambda(string, int, int)
                % The predicate is a higher-order manifest constant.
                % The arguments specify its location in the source, as a
                % filename/line number pair, and a sequence number used to
                % distinguish multiple lambdas on the same line.
    ;       user(sym_name).
                % The predicate is a normal user-written predicate;
                % the string is its name.

    % pred_info_init(ModuleName, SymName, Arity, PredOrFunc, Context,
    %   Origin, Status, GoalType, Markers, ArgTypes, TypeVarSet,
    %   ExistQVars, ClassContext, ClassProofs, ClassConstraintMap,
    %   User, ClausesInfo, PredInfo)
    %
    % Return a pred_info whose fields are filled in from the information
    % (direct and indirect) in the arguments, and from defaults.
    %
:- pred pred_info_init(module_name::in, sym_name::in, arity::in,
    pred_or_func::in, prog_context::in, pred_origin::in, import_status::in,
    goal_type::in, pred_markers::in, list(mer_type)::in, tvarset::in,
    existq_tvars::in, prog_constraints::in, constraint_proof_map::in,
    constraint_map::in, clauses_info::in, pred_info::out) is det.

    % pred_info_create(ModuleName, SymName, PredOrFunc, Context, Origin,
    %   Status, Markers, TypeVarSet, ExistQVars, ArgTypes,
    %   ClassContext, Assertions, User, ProcInfo, ProcId, PredInfo)
    %
    % Return a pred_info whose fields are filled in from the information
    % (direct and indirect) in the arguments, and from defaults. The given
    % proc_info becomes the only procedure of the predicate (currently)
    % and its proc_id is returned as the second last argument.
    %
:- pred pred_info_create(module_name::in, sym_name::in, pred_or_func::in,
    prog_context::in, pred_origin::in, import_status::in, pred_markers::in,
    list(mer_type)::in, tvarset::in, existq_tvars::in, prog_constraints::in,
    set(assert_id)::in, proc_info::in, proc_id::out, pred_info::out) is det.

    % define_new_pred(Origin, Goal, CallGoal, Args, ExtraArgs,
    %   InstMap, PredName, TVarSet, VarTypes, ClassContext,
    %   TVarMap, TCVarMap, VarSet, Markers, IsAddressTaken,
    %   !ModuleInfo, PredProcId)
    %
    % Create a new predicate for the given goal, returning a goal to
    % call the created predicate. ExtraArgs is the list of extra
    % type_infos and typeclass_infos required by typeinfo liveness
    % which were added to the front of the argument list.
    %
:- pred define_new_pred(pred_origin::in,
    hlds_goal::in, hlds_goal::out, list(prog_var)::in, list(prog_var)::out,
    instmap::in, string::in, tvarset::in, vartypes::in,
    prog_constraints::in, rtti_varmaps::in, prog_varset::in,
    inst_varset::in, pred_markers::in, is_address_taken::in,
    module_info::in, module_info::out, pred_proc_id::out) is det.

    % Various predicates for accessing the information stored in the
    % pred_id and pred_info data structures.
    %
:- type head_type_params == list(tvar).

:- func pred_info_module(pred_info) =  module_name.
:- func pred_info_name(pred_info) = string.

    % Pred_info_orig_arity returns the arity of the predicate
    % *not* counting inserted type_info arguments for polymorphic preds.
    %
:- func pred_info_orig_arity(pred_info) = arity.

    % N-ary functions are converted into N+1-ary predicates.
    % (Clauses are converted in make_hlds, but calls to functions
    % cannot be converted until after type-checking, once we have
    % resolved overloading. So we do that during mode analysis.)
    % The `is_pred_or_func' field of the pred_info records whether
    % a pred_info is really for a predicate or whether it is for
    % what was originally a function.
    %
:- func pred_info_is_pred_or_func(pred_info) = pred_or_func.

:- pred pred_info_context(pred_info::in, prog_context::out) is det.
:- pred pred_info_get_origin(pred_info::in, pred_origin::out) is det.
:- pred pred_info_import_status(pred_info::in, import_status::out) is det.
:- pred pred_info_get_goal_type(pred_info::in, goal_type::out) is det.
:- pred pred_info_get_markers(pred_info::in, pred_markers::out) is det.
:- pred pred_info_get_attributes(pred_info::in, pred_attributes::out) is det.
:- pred pred_info_arg_types(pred_info::in, list(mer_type)::out) is det.
:- pred pred_info_typevarset(pred_info::in, tvarset::out) is det.
:- pred pred_info_tvar_kinds(pred_info::in, tvar_kind_map::out) is det.
:- pred pred_info_get_exist_quant_tvars(pred_info::in, existq_tvars::out)
    is det.
:- pred pred_info_get_existq_tvar_binding(pred_info::in, tsubst::out) is det.
:- pred pred_info_get_head_type_params(pred_info::in, head_type_params::out)
    is det.
:- pred pred_info_get_class_context(pred_info::in, prog_constraints::out)
    is det.
:- pred pred_info_get_constraint_proofs(pred_info::in,
    constraint_proof_map::out) is det.
:- pred pred_info_get_constraint_map(pred_info::in,
    constraint_map::out) is det.
:- pred pred_info_get_unproven_body_constraints(pred_info::in,
    list(prog_constraint)::out) is det.
:- pred pred_info_get_assertions(pred_info::in, set(assert_id)::out) is det.
:- pred pred_info_clauses_info(pred_info::in, clauses_info::out) is det.
:- pred pred_info_procedures(pred_info::in, proc_table::out) is det.

    % Setting the name of a pred_info after its creation won't remove its name
    % from the indexes under its old name or insert into the indexes under its
    % new name. If is therefore safe to do this only after all the passes that
    % look up predicates by name.
    %
:- pred pred_info_set_name(string::in,
    pred_info::in, pred_info::out) is det.

:- pred pred_info_set_origin(pred_origin::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_import_status(import_status::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_goal_type(goal_type::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_markers(pred_markers::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_attributes(pred_attributes::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_typevarset(tvarset::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_tvar_kinds(tvar_kind_map::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_existq_tvar_binding(tsubst::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_head_type_params(head_type_params::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_class_context(prog_constraints::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_constraint_proofs(constraint_proof_map::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_constraint_map(constraint_map::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_unproven_body_constraints(list(prog_constraint)::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_assertions(set(assert_id)::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_clauses_info(clauses_info::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_procedures(proc_table::in,
    pred_info::in, pred_info::out) is det.

:- func inst_graph_info(pred_info) = inst_graph_info.
:- func 'inst_graph_info :='(pred_info, inst_graph_info) = pred_info.

    % Mode information for the arguments of a procedure.
    % The first map gives the instantiation state on entry of the
    % node corresponding to the prog_var.  The second map gives
    % the instantation state on exit.
:- type arg_modes_map == pair(map(prog_var, bool)).

:- func modes(pred_info) = list(arg_modes_map).
:- func 'modes :='(pred_info, list(arg_modes_map)) = pred_info.

    % Return a list of all the proc_ids for the valid modes
    % of this predicate.  This does not include candidate modes
    % that were generated during mode inference but which mode
    % inference found were not valid modes.
:- func pred_info_procids(pred_info) = list(proc_id).

    % Return a list of the proc_ids for all the modes
    % of this predicate, including invalid modes.
:- func pred_info_all_procids(pred_info) = list(proc_id).

    % Return a list of the proc_ids for all the valid modes
    % of this predicate that are not imported.
:- func pred_info_non_imported_procids(pred_info) = list(proc_id).

    % Return a list of the proc_ids for all the modes
    % of this predicate that are not imported
    % (including invalid modes).
:- func pred_info_all_non_imported_procids(pred_info) = list(proc_id).

    % Return a list of the proc_ids for all the valid modes
    % of this predicate that are exported.
:- func pred_info_exported_procids(pred_info) = list(proc_id).

    % Remove a procedure from the pred_info.
:- pred pred_info_remove_procid(proc_id::in, pred_info::in, pred_info::out)
    is det.

:- pred pred_info_arg_types(pred_info::in, tvarset::out, existq_tvars::out,
    list(mer_type)::out) is det.

:- pred pred_info_set_arg_types(tvarset::in, existq_tvars::in,
    list(mer_type)::in, pred_info::in, pred_info::out) is det.

:- pred pred_info_get_univ_quant_tvars(pred_info::in, existq_tvars::out)
    is det.

:- pred pred_info_proc_info(pred_info::in, proc_id::in, proc_info::out) is det.

:- pred pred_info_set_proc_info(proc_id::in, proc_info::in,
    pred_info::in, pred_info::out) is det.

:- pred pred_info_is_imported(pred_info::in) is semidet.

:- pred pred_info_is_pseudo_imported(pred_info::in) is semidet.

    % pred_info_is_exported does *not* include predicates which are
    % exported_to_submodules or pseudo_exported
:- pred pred_info_is_exported(pred_info::in) is semidet.

:- pred pred_info_is_opt_exported(pred_info::in) is semidet.

:- pred pred_info_is_exported_to_submodules(pred_info::in) is semidet.

:- pred pred_info_is_pseudo_exported(pred_info::in) is semidet.

    % procedure_is_exported includes all modes of exported or
    % exported_to_submodules predicates, plus the in-in mode
    % for pseudo_exported unification predicates.
    %
:- pred procedure_is_exported(module_info::in, pred_info::in, proc_id::in)
    is semidet.

    % Set the import_status of the predicate to `imported'.
    % This is used for `:- external(foo/2).' declarations.
    %
:- pred pred_info_mark_as_external(pred_info::in, pred_info::out) is det.

    % Do we have a clause goal type?
    % (this means either "clauses" or "clauses_and_pragmas")
    %
:- pred pred_info_clause_goal_type(pred_info::in) is semidet.

    % Do we have a pragma goal type?
    % (this means either "pragmas" or "clauses_and_pragmas")
    %
:- pred pred_info_pragma_goal_type(pred_info::in) is semidet.

:- pred pred_info_update_goal_type(goal_type::in,
    pred_info::in, pred_info::out) is det.

    % Succeeds if there was a `:- pragma inline(...)' declaration
    % for this predicate. Note that the compiler may decide
    % to inline a predicate even if there was no pragma inline(...)
    % declaration for that predicate.
    %
:- pred pred_info_requested_inlining(pred_info::in) is semidet.

    % Succeeds if there was a `:- pragma no_inline(...)' declaration
    % for this predicate.
    %
:- pred pred_info_requested_no_inlining(pred_info::in) is semidet.

:- pred pred_info_get_purity(pred_info::in, purity::out) is det.

:- pred pred_info_get_promised_purity(pred_info::in, purity::out) is det.

:- pred pred_info_infer_modes(pred_info::in) is semidet.

:- pred purity_to_markers(purity::in, pred_markers::out) is det.

:- pred terminates_to_markers(terminates::in, pred_markers::out) is det.

:- pred pred_info_get_call_id(pred_info::in, simple_call_id::out) is det.

    % Create an empty set of markers.
    %
:- pred init_markers(pred_markers::out) is det.

    % Check if a particular is in the set.
    %
:- pred check_marker(pred_markers::in, marker::in) is semidet.

    % Add a marker to the set.
    %
:- pred add_marker(marker::in, pred_markers::in, pred_markers::out) is det.

    % Remove a marker from the set.
    %
:- pred remove_marker(marker::in, pred_markers::in, pred_markers::out) is det.

    % Convert the set to a list.
    %
:- pred markers_to_marker_list(pred_markers::in, list(marker)::out) is det.

:- pred marker_list_to_markers(list(marker)::in, pred_markers::out) is det.

    % Create an empty set of attributes.
    %
:- pred init_attributes(pred_attributes::out) is det.

    % Check if a particular is in the set.
    %
:- pred check_attribute(pred_attributes::in, attribute::in) is semidet.

    % Add a attribute to the set.
    %
:- pred add_attribute(attribute::in, pred_attributes::in, pred_attributes::out)
    is det.

    % Remove a attribute from the set.
    %
:- pred remove_attribute(attribute::in,
    pred_attributes::in, pred_attributes::out) is det.

    % Convert the set to a list.
    %
:- pred attributes_to_attribute_list(pred_attributes::in,
    list(attribute)::out) is det.

:- pred attribute_list_to_attributes(list(attribute)::in,
    pred_attributes::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- type pred_id     ==  int.
:- type proc_id     ==  int.

shroud_pred_id(PredId) = shrouded_pred_id(PredId).
shroud_proc_id(ProcId) = shrouded_proc_id(ProcId).
shroud_pred_proc_id(proc(PredId, ProcId)) =
    shrouded_pred_proc_id(PredId, ProcId).

unshroud_pred_id(shrouded_pred_id(PredId)) = PredId.
unshroud_proc_id(shrouded_proc_id(ProcId)) = ProcId.
unshroud_pred_proc_id(shrouded_pred_proc_id(PredId, ProcId)) =
    proc(PredId, ProcId).

pred_id_to_int(PredId, PredId).
pred_id_to_int(PredId) = PredId.

proc_id_to_int(ProcId, ProcId).
proc_id_to_int(ProcId) = ProcId.

initial_pred_id = 0.
initial_proc_id = 0.

invalid_pred_id = -1.
invalid_proc_id = -1.

next_pred_id(PredId, NextPredId) :-
    NextPredId = PredId + 1.

in_in_unification_proc_id(0).

    % We could store the next available ModeId rather than recomputing
    % it on demand, but it is probably more efficient this way.
    %
next_mode_id(Procs, ModeId) :-
    map__to_assoc_list(Procs, List),
    list__length(List, ModeInt),
    proc_id_to_int(ModeId, ModeInt).

status_is_exported(imported(_),             no).
status_is_exported(external(_),             no).
status_is_exported(abstract_imported,       no).
status_is_exported(pseudo_imported,         no).
status_is_exported(opt_imported,            no).
status_is_exported(exported,                yes).
status_is_exported(opt_exported,            yes).
status_is_exported(abstract_exported,       yes).
status_is_exported(pseudo_exported,         yes).
status_is_exported(exported_to_submodules,  yes).
status_is_exported(local,                   no).

status_is_exported_to_non_submodules(Status, Result) :-
    (
        status_is_exported(Status, yes),
        Status \= exported_to_submodules
    ->
        Result = yes
    ;
        Result = no
    ).

status_is_imported(Status, Imported) :-
    status_defined_in_this_module(Status, InThisModule),
    bool__not(InThisModule, Imported).

status_defined_in_this_module(imported(_),              no).
status_defined_in_this_module(external(_),              no).
status_defined_in_this_module(abstract_imported,        no).
status_defined_in_this_module(pseudo_imported,          no).
status_defined_in_this_module(opt_imported,             no).
status_defined_in_this_module(exported,                 yes).
status_defined_in_this_module(opt_exported,             yes).
status_defined_in_this_module(abstract_exported,        yes).
status_defined_in_this_module(pseudo_exported,          yes).
status_defined_in_this_module(exported_to_submodules,   yes).
status_defined_in_this_module(local,                    yes).

calls_are_fully_qualified(Markers) =
    ( check_marker(Markers, calls_are_fully_qualified) ->
        is_fully_qualified
    ;
        may_be_partially_qualified
    ).

%-----------------------------------------------------------------------------%

    % The information specific to a predicate, as opposed to a procedure.
    % (Functions count as predicates.)
    %
    % Note that it is an invariant that any type_info-related
    % variables in the arguments of a predicate must precede any
    % polymorphically-typed arguments whose type depends on the
    % values of those type_info-related variables;
    % accurate GC for the MLDS back-end relies on this.
:- type pred_info --->
    pred_info(
        module_name         :: module_name,
                            % Module in which pred occurs.

        name                :: string,
                            % Predicate name.

        orig_arity          :: arity,
                            % The arity of the pred (*not* counting any
                            % inserted type_info arguments)

        is_pred_or_func     :: pred_or_func,
                            % Whether this "predicate" was really
                            % a predicate or a function.

        context             :: prog_context,
                            % The location (line #) of the :- pred decl.

        pred_origin         :: pred_origin,
                            % Where did the predicate come from.

        import_status       :: import_status,

        goal_type           :: goal_type,
                            % Whether the goals seen so far, if any, for this
                            % predicate are clauses or foreign_code(...)
                            % pragmas.

        markers             :: pred_markers,
                            % Various boolean flags.

        attributes          :: pred_attributes,
                            % Various attributes.

        arg_types           :: list(mer_type),
                            % Argument types.

        decl_typevarset     :: tvarset,
                            % Names of type vars in the predicate's type decl.

        typevarset          :: tvarset,
                            % Names of type vars in the predicate's type decl
                            % or in the variable type assignments.

        tvar_kinds          :: tvar_kind_map,
                            % Kinds of the type vars.

        exist_quant_tvars   :: existq_tvars,
                            % The set of existentially quantified type
                            % variables in the predicate's type decl.

        existq_tvar_binding :: tsubst,
                            % The statically known bindings of existentially
                            % quantified type variables inside this predicate.
                            % This field is set at the end of the polymorphism
                            % stage.

        head_type_params    :: head_type_params,
                            % The set of type variables which the body of the
                            % predicate can't bind, and whose type_infos are
                            % produced elsewhere. This includes universally
                            % quantified head types (the type_infos are passed
                            % in) plus existentially quantified types in preds
                            % called from the body (the type_infos are returned
                            % from the called preds). Computed during type
                            % checking.

        class_context       :: prog_constraints,
                            % The class constraints on the type variables
                            % in the predicate's type declaration.

        constraint_proofs   :: constraint_proof_map,
                            % Explanations of how redundant constraints were
                            % eliminated. These are needed by polymorphism.m
                            % to work out where to get the typeclass_infos
                            % from. Computed during type checking.

        constraint_map      :: constraint_map,
                            % Maps constraint identifiers to the actual
                            % constraints. Computed during type checking.

        unproven_body_constraints :: list(prog_constraint),
                            % Unproven class constraints on type variables
                            % in the predicate's body, if any (if this remains
                            % non-empty after type checking has finished,
                            % post_typecheck.m will report a type error).

        inst_graph_info     :: inst_graph_info,
                            % The predicate's inst graph, for constraint
                            % based mode analysis.

        modes               :: list(arg_modes_map),
                            % Mode information extracted from constraint
                            % based mode analysis.

        assertions          :: set(assert_id),
                            % List of assertions which mention this predicate.

        clauses_info        :: clauses_info,

        procedures          :: proc_table
    ).

pred_info_init(ModuleName, SymName, Arity, PredOrFunc, Context, Origin,
        Status, GoalType, Markers, ArgTypes, TypeVarSet, ExistQVars,
        ClassContext, ClassProofs, ClassConstraintMap,
        ClausesInfo, PredInfo) :-
    unqualify_name(SymName, PredName),
    sym_name_get_module_name(SymName, ModuleName, PredModuleName),
    prog_type__vars_list(ArgTypes, TVars),
    list__delete_elems(TVars, ExistQVars, HeadTypeParams),
    Attributes = [],
    % XXX kind inference:
    % we assume all tvars have kind `star'.
    map__init(Kinds),
    map__init(ExistQVarBindings),
    UnprovenBodyConstraints = [],
    set__init(Assertions),
    map__init(Procs),
    PredInfo = pred_info(PredModuleName, PredName, Arity, PredOrFunc,
        Context, Origin, Status, GoalType, Markers, Attributes,
        ArgTypes, TypeVarSet, TypeVarSet, Kinds, ExistQVars, ExistQVarBindings,
        HeadTypeParams, ClassContext, ClassProofs, ClassConstraintMap,
        UnprovenBodyConstraints, inst_graph_info_init, [],
        Assertions, ClausesInfo, Procs).

pred_info_create(ModuleName, SymName, PredOrFunc, Context, Origin, Status,
        Markers, ArgTypes, TypeVarSet, ExistQVars, ClassContext,
        Assertions, ProcInfo, ProcId, PredInfo) :-
    list__length(ArgTypes, Arity),
    proc_info_varset(ProcInfo, VarSet),
    proc_info_vartypes(ProcInfo, VarTypes),
    proc_info_headvars(ProcInfo, HeadVars),
    unqualify_name(SymName, PredName),
    Attributes = [],
    map__init(ClassProofs),
    map__init(ClassConstraintMap),
    prog_type__vars_list(ArgTypes, TVars),
    list__delete_elems(TVars, ExistQVars, HeadTypeParams),
    % XXX kind inference:
    % we assume all tvars have kind `star'.
    map__init(Kinds),
    map__init(ExistQVarBindings),
    UnprovenBodyConstraints = [],

    % The empty list of clauses is a little white lie.
    Clauses = forw([]),
    map__init(TVarNameMap),
    proc_info_rtti_varmaps(ProcInfo, RttiVarMaps),
    HasForeignClauses = no,
    ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap, VarTypes,
        HeadVars, Clauses, RttiVarMaps, HasForeignClauses),

    map__init(Procs0),
    next_mode_id(Procs0, ProcId),
    map__det_insert(Procs0, ProcId, ProcInfo, Procs),

    PredInfo = pred_info(ModuleName, PredName, Arity, PredOrFunc,
        Context, Origin, Status, clauses, Markers, Attributes,
        ArgTypes, TypeVarSet, TypeVarSet, Kinds, ExistQVars, ExistQVarBindings,
        HeadTypeParams, ClassContext, ClassProofs, ClassConstraintMap,
        UnprovenBodyConstraints, inst_graph_info_init, [], Assertions,
        ClausesInfo, Procs).

define_new_pred(Origin, Goal0, Goal, ArgVars0, ExtraTypeInfos,
        InstMap0, PredName, TVarSet, VarTypes0, ClassContext,
        RttiVarMaps, VarSet0, InstVarSet, Markers,
        IsAddressTaken, ModuleInfo0, ModuleInfo, PredProcId) :-
    Goal0 = _GoalExpr - GoalInfo,
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap),

    % XXX The set of existentially quantified type variables
    % here might not be correct.
    ExistQVars = [],

    % If interface typeinfo liveness is set, all type_infos for the
    % arguments need to be passed in, not just the ones that are used.
    % Similarly if the address of a procedure of this predicate is taken,
    % so that we can copy the closure.
    module_info_get_globals(ModuleInfo0, Globals),
    ExportStatus = local,
    non_special_interface_should_use_typeinfo_liveness(ExportStatus,
        IsAddressTaken, Globals, TypeInfoLiveness),
    (
        TypeInfoLiveness = yes,
        goal_info_get_nonlocals(GoalInfo, NonLocals),
        goal_util__extra_nonlocal_typeinfos(RttiVarMaps, VarTypes0,
            ExistQVars, NonLocals, ExtraTypeInfos0),
        set__delete_list(ExtraTypeInfos0, ArgVars0, ExtraTypeInfos1),
        set__to_sorted_list(ExtraTypeInfos1, ExtraTypeInfos),
        list__append(ExtraTypeInfos, ArgVars0, ArgVars)
    ;
        TypeInfoLiveness = no,
        ArgVars = ArgVars0,
        ExtraTypeInfos = []
    ),

    goal_info_get_context(GoalInfo, Context),
    goal_info_get_determinism(GoalInfo, Detism),
    compute_arg_types_modes(ArgVars, VarTypes0, InstMap0, InstMap,
        ArgTypes, ArgModes),

    module_info_get_name(ModuleInfo0, ModuleName),
    SymName = qualified(ModuleName, PredName),

        % Remove unneeded variables from the vartypes and varset.
    goal_util__goal_vars(Goal0, GoalVars0),
    set__insert_list(GoalVars0, ArgVars, GoalVars),
    map__select(VarTypes0, GoalVars, VarTypes),
    varset__select(VarSet0, GoalVars, VarSet),

        % Approximate the termination information for the new procedure.
    ( goal_cannot_loop(ModuleInfo0, Goal0) ->
        TermInfo = yes(cannot_loop(unit))
    ;
        TermInfo = no
    ),

    MaybeDeclaredDetism = no,
    proc_info_create(Context, VarSet, VarTypes, ArgVars, InstVarSet,
        ArgModes, MaybeDeclaredDetism, Detism, Goal0,
        RttiVarMaps, IsAddressTaken, ProcInfo0),
    proc_info_set_maybe_termination_info(TermInfo, ProcInfo0, ProcInfo),

    set__init(Assertions),

    pred_info_create(ModuleName, SymName, predicate, Context, Origin,
        ExportStatus, Markers, ArgTypes, TVarSet, ExistQVars,
        ClassContext, Assertions, ProcInfo, ProcId, PredInfo),

    module_info_get_predicate_table(ModuleInfo0, PredTable0),
    predicate_table_insert(PredInfo, PredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, ModuleInfo0, ModuleInfo),

    GoalExpr = call(PredId, ProcId, ArgVars, not_builtin, no, SymName),
    Goal = GoalExpr - GoalInfo,
    PredProcId = proc(PredId, ProcId).

:- pred compute_arg_types_modes(list(prog_var)::in, vartypes::in,
    instmap::in, instmap::in, list(mer_type)::out, list(mer_mode)::out) is det.

compute_arg_types_modes([], _, _, _, [], []).
compute_arg_types_modes([Var | Vars], VarTypes, InstMap0, InstMap,
        [Type | Types], [Mode | Modes]) :-
    map__lookup(VarTypes, Var, Type),
    instmap__lookup_var(InstMap0, Var, Inst0),
    instmap__lookup_var(InstMap, Var, Inst),
    Mode = (Inst0 -> Inst),
    compute_arg_types_modes(Vars, VarTypes, InstMap0, InstMap,
        Types, Modes).

%-----------------------------------------------------------------------------%

% The trivial access predicates.

pred_info_module(PI) = PI ^ module_name.
pred_info_name(PI) = PI ^ name.
pred_info_orig_arity(PI) = PI ^ orig_arity.

pred_info_is_pred_or_func(PI) = PI ^ is_pred_or_func.
pred_info_context(PI, PI ^ context).
pred_info_get_origin(PI, PI ^ pred_origin).
pred_info_import_status(PI, PI ^ import_status).
pred_info_get_goal_type(PI, PI ^ goal_type).
pred_info_get_markers(PI, PI ^ markers).
pred_info_get_attributes(PI, PI ^ attributes).
pred_info_arg_types(PI, PI ^ arg_types).
pred_info_typevarset(PI, PI ^ typevarset).
pred_info_tvar_kinds(PI, PI ^ tvar_kinds).
pred_info_get_exist_quant_tvars(PI, PI ^ exist_quant_tvars).
pred_info_get_existq_tvar_binding(PI, PI ^ existq_tvar_binding).
pred_info_get_head_type_params(PI, PI ^ head_type_params).
pred_info_get_class_context(PI, PI ^ class_context).
pred_info_get_constraint_proofs(PI, PI ^ constraint_proofs).
pred_info_get_constraint_map(PI, PI ^ constraint_map).
pred_info_get_unproven_body_constraints(PI, PI ^ unproven_body_constraints).
pred_info_get_assertions(PI, PI ^ assertions).
pred_info_clauses_info(PI, PI ^ clauses_info).
pred_info_procedures(PI, PI ^ procedures).

pred_info_set_name(X, PI, PI ^ name := X).
pred_info_set_origin(X, PI, PI ^ pred_origin := X).
pred_info_set_import_status(X, PI, PI ^ import_status := X).
pred_info_set_goal_type(X, PI, PI ^ goal_type := X).
pred_info_set_markers(X, PI, PI ^ markers := X).
pred_info_set_attributes(X, PI, PI ^ attributes := X).
pred_info_set_typevarset(X, PI, PI ^ typevarset := X).
pred_info_set_tvar_kinds(X, PI, PI ^ tvar_kinds := X).
pred_info_set_existq_tvar_binding(X, PI, PI ^ existq_tvar_binding := X).
pred_info_set_head_type_params(X, PI, PI ^ head_type_params := X).
pred_info_set_class_context(X, PI, PI ^ class_context := X).
pred_info_set_constraint_proofs(X, PI, PI ^ constraint_proofs := X).
pred_info_set_constraint_map(X, PI, PI ^ constraint_map := X).
pred_info_set_unproven_body_constraints(X, PI,
    PI ^ unproven_body_constraints := X).
pred_info_set_assertions(X, PI, PI ^ assertions := X).
pred_info_set_clauses_info(X, PI, PI ^ clauses_info := X).
pred_info_set_procedures(X, PI, PI ^ procedures := X).

%-----------------------------------------------------------------------------%

% The non-trivial access predicates.

pred_info_all_procids(PredInfo) = ProcIds :-
    ProcTable = PredInfo ^ procedures,
    map__keys(ProcTable, ProcIds).

pred_info_procids(PredInfo) = ValidProcIds :-
    AllProcIds = pred_info_all_procids(PredInfo),
    ProcTable = PredInfo ^ procedures,
    IsValid = (pred(ProcId::in) is semidet :-
        ProcInfo = map__lookup(ProcTable, ProcId),
        proc_info_is_valid_mode(ProcInfo)),
    list__filter(IsValid, AllProcIds, ValidProcIds).

pred_info_non_imported_procids(PredInfo) = ProcIds :-
    pred_info_import_status(PredInfo, ImportStatus),
    ( ImportStatus = imported(_) ->
        ProcIds = []
    ; ImportStatus = external(_) ->
        ProcIds = []
    ; ImportStatus = pseudo_imported ->
        ProcIds0 = pred_info_procids(PredInfo),
        % for pseduo_imported preds, procid 0 is imported
        list__delete_all(ProcIds0, 0, ProcIds)
    ;
        ProcIds = pred_info_procids(PredInfo)
    ).

pred_info_all_non_imported_procids(PredInfo) = ProcIds :-
    pred_info_import_status(PredInfo, ImportStatus),
    ( ImportStatus = imported(_) ->
        ProcIds = []
    ; ImportStatus = external(_) ->
        ProcIds = []
    ; ImportStatus = pseudo_imported ->
        ProcIds0 = pred_info_procids(PredInfo),
        % for pseduo_imported preds, procid 0 is imported
        list__delete_all(ProcIds0, 0, ProcIds)
    ;
        ProcIds = pred_info_procids(PredInfo)
    ).

pred_info_exported_procids(PredInfo) = ProcIds :-
    pred_info_import_status(PredInfo, ImportStatus),
    (
        ( ImportStatus = exported
        ; ImportStatus = opt_exported
        ; ImportStatus = exported_to_submodules
        )
    ->
        ProcIds = pred_info_procids(PredInfo)
    ;
        ImportStatus = pseudo_exported
    ->
        ProcIds = [0]
    ;
        ProcIds = []
    ).

pred_info_remove_procid(ProcId, !PredInfo) :-
    pred_info_procedures(!.PredInfo, Procs0),
    map__delete(Procs0, ProcId, Procs),
    pred_info_set_procedures(Procs, !PredInfo).

pred_info_arg_types(PredInfo, PredInfo ^ decl_typevarset,
        PredInfo ^ exist_quant_tvars, PredInfo ^ arg_types).

pred_info_set_arg_types(TypeVarSet, ExistQVars, ArgTypes, !PredInfo) :-
	!:PredInfo = !.PredInfo ^ decl_typevarset := TypeVarSet,
	!:PredInfo = !.PredInfo ^ exist_quant_tvars := ExistQVars,
	!:PredInfo = !.PredInfo ^ arg_types := ArgTypes.

pred_info_proc_info(PredInfo, ProcId, ProcInfo) :-
    ProcInfo = map__lookup(PredInfo ^ procedures, ProcId).

pred_info_set_proc_info(ProcId, ProcInfo, PredInfo0, PredInfo) :-
    PredInfo = PredInfo0 ^ procedures :=
        map__set(PredInfo0 ^ procedures, ProcId, ProcInfo).

pred_info_is_imported(PredInfo) :-
    pred_info_import_status(PredInfo, Status),
    ( Status = imported(_)
    ; Status = external(_)
    ).

pred_info_is_pseudo_imported(PredInfo) :-
    pred_info_import_status(PredInfo, ImportStatus),
    ImportStatus = pseudo_imported.

pred_info_is_exported(PredInfo) :-
    pred_info_import_status(PredInfo, ImportStatus),
    ImportStatus = exported.

pred_info_is_opt_exported(PredInfo) :-
    pred_info_import_status(PredInfo, ImportStatus),
    ImportStatus = opt_exported.

pred_info_is_exported_to_submodules(PredInfo) :-
    pred_info_import_status(PredInfo, ImportStatus),
    ImportStatus = exported_to_submodules.

pred_info_is_pseudo_exported(PredInfo) :-
    pred_info_import_status(PredInfo, ImportStatus),
    ImportStatus = pseudo_exported.

procedure_is_exported(ModuleInfo, PredInfo, ProcId) :-
    (
        pred_info_is_exported(PredInfo)
    ;
        pred_info_is_opt_exported(PredInfo)
    ;
        pred_info_is_exported_to_submodules(PredInfo)
    ;
        pred_info_is_pseudo_exported(PredInfo),
        in_in_unification_proc_id(ProcId)
    ;
        pred_info_import_status(PredInfo, ImportStatus),
        ImportStatus = external(ExternalImportStatus),
        status_is_exported(ExternalImportStatus, yes)
    ;
        pred_info_get_origin(PredInfo, special_pred(SpecialPred)),
        SpecialPred = SpecialId - TypeCtor,
        module_info_get_type_table(ModuleInfo, TypeTable),
        % If the search fails, then TypeCtor must be a builtin type
        % constructor, such as the tuple constructor.
        map__search(TypeTable, TypeCtor, TypeDefn),
        get_type_defn_in_exported_eqv(TypeDefn, yes),
        (
            SpecialId = spec_pred_unify,
            % The other proc_ids are module-specific.
            in_in_unification_proc_id(ProcId)
        ;
            SpecialId = spec_pred_compare
            % The declared modes are all global, and we don't
            % generate any modes for compare preds dynamically.
        ;
            SpecialId = spec_pred_index,
            % The index predicate is never called from anywhere
            % except the compare predicate.
            fail
        )
    ).

pred_info_mark_as_external(PredInfo0, PredInfo) :-
    PredInfo = PredInfo0 ^ import_status :=
        external(PredInfo0 ^ import_status).

pred_info_clause_goal_type(PredInfo) :-
    clause_goal_type(PredInfo ^ goal_type).

pred_info_pragma_goal_type(PredInfo) :-
    pragma_goal_type(PredInfo ^ goal_type).

:- pred clause_goal_type(goal_type::in) is semidet.

clause_goal_type(clauses).
clause_goal_type(clauses_and_pragmas).

:- pred pragma_goal_type(goal_type::in) is semidet.

pragma_goal_type(pragmas).
pragma_goal_type(clauses_and_pragmas).

pred_info_update_goal_type(GoalType1, !PredInfo) :-
    pred_info_get_goal_type(!.PredInfo, GoalType0),
    (
        GoalType0 = none,
        GoalType = GoalType1
    ;
        GoalType0 = pragmas,
        ( clause_goal_type(GoalType1) ->
            GoalType = clauses_and_pragmas
        ;
            GoalType = pragmas
        )
    ;
        GoalType0 = clauses,
        ( pragma_goal_type(GoalType1) ->
            GoalType = clauses_and_pragmas
        ;
            GoalType = clauses
        )
    ;
        GoalType0 = clauses_and_pragmas,
        GoalType = GoalType0
    ;
        GoalType0 = promise(_),
        unexpected(this_file, "pred_info_update_goal_type")
    ),
    pred_info_set_goal_type(GoalType, !PredInfo).

pred_info_requested_inlining(PredInfo0) :-
    pred_info_get_markers(PredInfo0, Markers),
    (
        check_marker(Markers, user_marked_inline)
    ;
        check_marker(Markers, heuristic_inline)
    ).

pred_info_requested_no_inlining(PredInfo0) :-
    pred_info_get_markers(PredInfo0, Markers),
    check_marker(Markers, user_marked_no_inline).

pred_info_get_purity(PredInfo0, Purity) :-
    pred_info_get_markers(PredInfo0, Markers),
    ( check_marker(Markers, is_impure) ->
        Purity = purity_impure
    ; check_marker(Markers, is_semipure) ->
        Purity = purity_semipure
    ;
        Purity = purity_pure
    ).

pred_info_get_promised_purity(PredInfo0, PromisedPurity) :-
    pred_info_get_markers(PredInfo0, Markers),
    ( check_marker(Markers, promised_pure) ->
        PromisedPurity = purity_pure
    ; check_marker(Markers, promised_semipure) ->
        PromisedPurity = purity_semipure
    ;
        PromisedPurity = purity_impure
    ).

pred_info_infer_modes(PredInfo) :-
    pred_info_get_markers(PredInfo, Markers),
    check_marker(Markers, infer_modes).

purity_to_markers(purity_pure, []).
purity_to_markers(purity_semipure, [is_semipure]).
purity_to_markers(purity_impure, [is_impure]).

terminates_to_markers(terminates, [terminates]).
terminates_to_markers(does_not_terminate, [does_not_terminate]).
terminates_to_markers(depends_on_mercury_calls, []).

pred_info_get_univ_quant_tvars(PredInfo, UnivQVars) :-
    pred_info_arg_types(PredInfo, ArgTypes),
    prog_type__vars_list(ArgTypes, ArgTypeVars0),
    list__sort_and_remove_dups(ArgTypeVars0, ArgTypeVars),
    pred_info_get_exist_quant_tvars(PredInfo, ExistQVars),
    list__delete_elems(ArgTypeVars, ExistQVars, UnivQVars).

%-----------------------------------------------------------------------------%

pred_info_get_call_id(PredInfo, PredOrFunc - qualified(Module, Name)/Arity) :-
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo).

%-----------------------------------------------------------------------------%

:- type pred_markers == list(marker).

init_markers([]).

check_marker(Markers, Marker) :-
    list__member(Marker, Markers).

add_marker(Marker, Markers, [Marker | Markers]).

remove_marker(Marker, Markers0, Markers) :-
    list__delete_all(Markers0, Marker, Markers).

markers_to_marker_list(Markers, Markers).

marker_list_to_markers(Markers, Markers).

:- type pred_attributes == list(attribute).

init_attributes([]).

check_attribute(Attributes, Attribute) :-
    list__member(Attribute, Attributes).

add_attribute(Attribute, Attributes, [Attribute | Attributes]).

remove_attribute(Attribute, Attributes0, Attributes) :-
    list__delete_all(Attributes0, Attribute, Attributes).

attributes_to_attribute_list(Attributes, Attributes).

attribute_list_to_attributes(Attributes, Attributes).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Various predicates for accessing the proc_info data structure.

:- interface.

:- type is_address_taken
    --->    address_is_taken
    ;       address_is_not_taken.

:- type deep_profile_role
    --->    inner_proc(
                outer_proc  :: pred_proc_id
            )
    ;       outer_proc(
                inner_proc  :: pred_proc_id
            ).

:- type deep_recursion_info
    --->    deep_recursion_info(
                role        :: deep_profile_role,
                visible_scc :: list(visible_scc_data)
                            % If the procedure is not tail recursive, this list
                            % is empty. Otherwise, it contains outer-inner
                            % pairs of procedures in the visible SCC,
                            % including this procedure and its copy.
            ).

:- type visible_scc_data
    --->    visible_scc_data(
                vis_outer_proc  :: pred_proc_id,
                vis_inner_proc  :: pred_proc_id,
                rec_call_sites  :: list(int)
                                % A list of all the call site numbers that
                                % correspond to tail calls. (Call sites are
                                % numbered depth-first, left-to-right,
                                % from zero.)
            ).

:- type call_site_static_data           % defines MR_CallSiteStatic
    --->    normal_call(
                normal_callee           :: rtti_proc_label,
                normal_type_subst       :: string,
                normal_file_name        :: string,
                normal_line_number      :: int,
                normal_goal_path        :: goal_path
            )
    ;       special_call(
                special_file_name       :: string,
                special_line_number     :: int,
                special_goal_path       :: goal_path
            )
    ;       higher_order_call(
                higher_order_file_name  :: string,
                ho_line_number          :: int,
                ho_goal_path            :: goal_path
            )
    ;       method_call(
                method_file_name        :: string,
                method_line_number      :: int,
                method_goal_path        :: goal_path
            )
    ;       callback(
                callback_file_name      :: string,
                callback_line_number    :: int,
                callback_goal_path      :: goal_path
            ).

:- type hlds_proc_static
    --->    hlds_proc_static(   % defines part of MR_ProcStatic
                proc_static_file_name   :: string,
                proc_static_line_number :: int,
                proc_is_in_interface    :: bool,
                call_site_statics       :: list(call_site_static_data)
            ).

    % The hlds_deep_excp_vars gives the variables that hold the values returned
    % by the call port code, which are needed to let exception.throw perform
    % the work we need to do at the excp port.
:- type hlds_deep_excp_vars
    --->    hlds_deep_excp_vars(
                top_csd                 :: prog_var,
                middle_csd              :: prog_var,
                old_outermost           :: maybe(prog_var)
                                        % Needed only with the save/restore
                                        % approach, not the activation counting
                                        % approach.
            ).

:- type hlds_deep_layout
    --->    hlds_deep_layout(
                deep_layout_static      :: hlds_proc_static,
                deep_layout_excp        :: hlds_deep_excp_vars
            ).

:- type deep_profile_proc_info
    --->    deep_profile_proc_info(
                deep_rec        :: maybe(deep_recursion_info),
                deep_layout     :: maybe(hlds_deep_layout)
                                % The first field is set during the first,
                                % tail recursion part of the deep profiling
                                % transformation, if that is enabled.
                                % The deep_layout field is set during the
                                % second part; it will be bound to `no'
                                % before and during the first part, and
                                % to `yes' after the second. The contents
                                % of this field govern what will go into
                                % MR_ProcStatic structures.
            ).

:- type table_arg_info
    --->    table_arg_info(
                headvar         :: prog_var,
                slot_num        :: int,
                arg_type        :: mer_type
            ).

    % This type is analogous to llds:layout_locn, but it refers to slots in
    % the extended answer blocks used by I/O action tabling for declarative
    % debugging, not to lvals.
:- type table_locn
    --->    direct(int)
    ;       indirect(int, int).

:- type table_trie_step
    --->    table_trie_step_int
    ;       table_trie_step_char
    ;       table_trie_step_string
    ;       table_trie_step_float
    ;       table_trie_step_dummy
    ;       table_trie_step_enum(int)
                % The int gives the number of alternatives in the enum type,
                % and thus the size of the corresponding trie node.
    ;       table_trie_step_user(mer_type)
    ;       table_trie_step_user_fast_loose(mer_type)
    ;       table_trie_step_poly
    ;       table_trie_step_poly_fast_loose
    ;       table_trie_step_typeinfo
    ;       table_trie_step_typeclassinfo
    ;       table_trie_step_promise_implied.

:- type table_arg_infos
    --->    table_arg_infos(
                list(table_arg_info),
                map(tvar, table_locn)
            ).

:- type proc_table_info

        % The information we need to display an I/O action to the user.
        %
        % The table_arg_type_infos correspond one to one to the
        % elements of the block saved for an I/O action. The first
        % element will be the pointer to the proc_layout of the
        % action's procedure.
    --->    table_io_decl_info(
                table_arg_infos
            )

        % The information we need to interpret the data structures
        % created by tabling for a procedure, except the information
        % (such as determinism) that is already available from
        % proc_layout structures.
        %
        % The table_arg_type_infos list first all the input arguments,
        % then all the output arguments.
    ;       table_gen_info(
                num_inputs      :: int,
                num_outputs     :: int,
                input_steps     :: list(table_trie_step),
                gen_arg_infos   :: table_arg_infos
            ).

:- type untuple_proc_info
    --->    untuple_proc_info(
                map(prog_var, prog_vars)
            ).

:- pred proc_info_init(prog_context::in, arity::in, list(mer_type)::in,
    maybe(list(mer_mode))::in, list(mer_mode)::in, maybe(list(is_live))::in,
    maybe(determinism)::in, is_address_taken::in, proc_info::out) is det.

:- pred proc_info_set(prog_context::in, prog_varset::in, vartypes::in,
    list(prog_var)::in, inst_varset::in, list(mer_mode)::in,
    maybe(list(is_live))::in, maybe(determinism)::in, determinism::in,
    hlds_goal::in, bool::in, rtti_varmaps::in,
    maybe(arg_size_info)::in, maybe(termination_info)::in,
    termination2_info::in, is_address_taken::in, stack_slots::in,
    maybe(list(arg_info))::in, liveness_info::in, proc_info::out) is det.

:- pred proc_info_create(prog_context::in, prog_varset::in, vartypes::in,
    list(prog_var)::in, inst_varset::in, list(mer_mode)::in,
    determinism::in, hlds_goal::in, rtti_varmaps::in,
    is_address_taken::in, proc_info::out) is det.

:- pred proc_info_create(prog_context::in, prog_varset::in, vartypes::in,
    list(prog_var)::in, inst_varset::in, list(mer_mode)::in,
    maybe(determinism)::in, determinism::in, hlds_goal::in,
    rtti_varmaps::in, is_address_taken::in, proc_info::out) is det.

:- pred proc_info_set_body(prog_varset::in, vartypes::in,
    list(prog_var)::in, hlds_goal::in, rtti_varmaps::in,
    proc_info::in, proc_info::out) is det.

    % Predicates to get fields of proc_infos.

:- pred proc_info_context(proc_info::in, prog_context::out) is det.
:- pred proc_info_varset(proc_info::in, prog_varset::out) is det.
:- pred proc_info_vartypes(proc_info::in, vartypes::out) is det.
:- pred proc_info_headvars(proc_info::in, list(prog_var)::out) is det.
:- pred proc_info_inst_varset(proc_info::in, inst_varset::out) is det.
:- pred proc_info_maybe_declared_argmodes(proc_info::in,
    maybe(list(mer_mode))::out) is det.
:- pred proc_info_argmodes(proc_info::in, list(mer_mode)::out) is det.
:- pred proc_info_maybe_arglives(proc_info::in,
    maybe(list(is_live))::out) is det.
:- pred proc_info_declared_determinism(proc_info::in,
    maybe(determinism)::out) is det.
:- pred proc_info_inferred_determinism(proc_info::in, determinism::out) is det.
:- pred proc_info_goal(proc_info::in, hlds_goal::out) is det.
:- pred proc_info_can_process(proc_info::in, bool::out) is det.
:- pred proc_info_rtti_varmaps(proc_info::in, rtti_varmaps::out) is det.
:- pred proc_info_eval_method(proc_info::in, eval_method::out) is det.
:- pred proc_info_get_maybe_arg_size_info(proc_info::in,
    maybe(arg_size_info)::out) is det.
:- pred proc_info_get_maybe_termination_info(proc_info::in,
    maybe(termination_info)::out) is det.
:- pred proc_info_is_address_taken(proc_info::in,
    is_address_taken::out) is det.
:- pred proc_info_stack_slots(proc_info::in, stack_slots::out) is det.
:- pred proc_info_maybe_arg_info(proc_info::in,
    maybe(list(arg_info))::out) is det.
:- pred proc_info_liveness_info(proc_info::in, liveness_info::out) is det.
:- pred proc_info_get_need_maxfr_slot(proc_info::in, bool::out) is det.
:- pred proc_info_get_call_table_tip(proc_info::in,
    maybe(prog_var)::out) is det.
:- pred proc_info_get_maybe_proc_table_info(proc_info::in,
    maybe(proc_table_info)::out) is det.
:- pred proc_info_get_maybe_deep_profile_info(proc_info::in,
    maybe(deep_profile_proc_info)::out) is det.
:- pred proc_info_get_maybe_untuple_info(proc_info::in,
    maybe(untuple_proc_info)::out) is det.

    % Predicates to set fields of proc_infos.

:- pred proc_info_set_varset(prog_varset::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_vartypes(vartypes::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_headvars(list(prog_var)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_inst_varset(inst_varset::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_declared_argmodes(maybe(list(mer_mode))::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_argmodes(list(mer_mode)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_arglives(maybe(list(is_live))::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_inferred_determinism(determinism::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_goal(hlds_goal::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_can_process(bool::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_rtti_varmaps(rtti_varmaps::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_eval_method(eval_method::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_arg_size_info(maybe(arg_size_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_termination_info(maybe(termination_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_address_taken(is_address_taken::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_stack_slots(stack_slots::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_arg_info(list(arg_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_liveness_info(liveness_info::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_need_maxfr_slot(bool::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_call_table_tip(maybe(prog_var)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_proc_table_info(maybe(proc_table_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_deep_profile_info(
    maybe(deep_profile_proc_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_untuple_info(
    maybe(untuple_proc_info)::in,
    proc_info::in, proc_info::out) is det.

:- pred proc_info_get_termination2_info(proc_info::in,
    termination2_info::out) is det.

:- pred proc_info_set_termination2_info(termination2_info::in,
    proc_info::in, proc_info::out) is det.

:- pred proc_info_get_structure_sharing(proc_info::in,
    maybe(structure_sharing_domain)::out) is det.

:- pred proc_info_set_structure_sharing(structure_sharing_domain::in, 
    proc_info::in, proc_info::out) is det.

:- pred proc_info_head_modes_constraint(proc_info::in, mode_constraint::out)
    is det.

:- pred proc_info_set_head_modes_constraint(mode_constraint::in,
    proc_info::in, proc_info::out) is det.

    % See also proc_info_interface_code_model in code_model.m.
:- pred proc_info_interface_determinism(proc_info::in, determinism::out)
    is det.

    % proc_info_never_succeeds(ProcInfo, Result):
    % return Result = yes if the procedure is known to never succeed
    % according to the declared determinism.
    %
:- pred proc_info_never_succeeds(proc_info::in, bool::out) is det.

:- pred proc_info_declared_argmodes(proc_info::in, list(mer_mode)::out) is det.
:- pred proc_info_arglives(proc_info::in, module_info::in,
    list(is_live)::out) is det.
:- pred proc_info_arg_info(proc_info::in, list(arg_info)::out) is det.
:- pred proc_info_get_initial_instmap(proc_info::in, module_info::in,
    instmap::out) is det.

    % For a set of variables V, find all the type variables in the types
    % of the variables in V, and return set of typeinfo variables for
    % those type variables. (find all typeinfos for variables in V).
    %
    % This set of typeinfos is often needed in liveness computation
    % for accurate garbage collection - live variables need to have
    % their typeinfos stay live too.
    %
:- pred proc_info_get_typeinfo_vars(set(prog_var)::in, vartypes::in,
    rtti_varmaps::in, set(prog_var)::out) is det.

:- pred proc_info_maybe_complete_with_typeinfo_vars(set(prog_var)::in,
    bool::in, vartypes::in, rtti_varmaps::in, set(prog_var)::out) is det.

:- pred proc_info_ensure_unique_names(proc_info::in, proc_info::out) is det.

    % Create a new variable of the given type to the procedure.
    %
:- pred proc_info_create_var_from_type(mer_type::in, maybe(string)::in,
    prog_var::out, proc_info::in, proc_info::out) is det.

    % Create a new variable for each element of the list of types.
    %
:- pred proc_info_create_vars_from_types(list(mer_type)::in,
    list(prog_var)::out, proc_info::in, proc_info::out) is det.

    % Given a procedure, return a list of all its headvars which are
    % (further) instantiated by the procedure.
    %
:- pred proc_info_instantiated_head_vars(module_info::in, proc_info::in,
    list(prog_var)::out) is det.

    % Given a procedure, return a list of all its headvars which are
    % not (further) instantiated by the procedure.
    %
:- pred proc_info_uninstantiated_head_vars(module_info::in, proc_info::in,
    list(prog_var)::out) is det.

    % Return true if the interface of the given procedure must include
    % typeinfos for all the type variables in the types of the arguments.
    %
:- pred proc_interface_should_use_typeinfo_liveness(pred_info::in, proc_id::in,
    globals::in, bool::out) is det.

    % Return true if the interface of a procedure in a non-special
    % predicate with the given characteristics (import/export/local
    % status, address taken status) must include typeinfos for
    % all the type variables in the types of the arguments.
    % Note that only a few predicates in the builtin modules are special
    % in this sense, and that compiler-generated predicates are never
    % special.
    %
:- pred non_special_interface_should_use_typeinfo_liveness(import_status::in,
    is_address_taken::in, globals::in, bool::out) is det.

    % Return true if the body of a procedure from the given predicate
    % must keep a typeinfo variable alive during the lifetime of all
    % variables whose type includes the corresponding type variable.
    % Note that body typeinfo liveness implies interface typeinfo liveness,
    % but not vice versa.
    %
:- pred body_should_use_typeinfo_liveness(pred_info::in, globals::in,
    bool::out) is det.

    % Return true if the body of a procedure in a non-special predicate
    % must keep a typeinfo variable alive during the lifetime of all
    % variables whose type includes the corresponding type variable.
    %
:- pred non_special_body_should_use_typeinfo_liveness(globals::in,
    bool::out) is det.

    % If the procedure has a input/output pair of io__state arguments,
    % return the positions of those arguments in the argument list.
    % The positions are given as argument numbers, with the first argument
    % in proc_info_headvars being position 1, and so on. The first output
    % argument gives the position of the input state, the second the
    % position of the output state.
    %
    % Note that the automatically constructed unify, index and compare
    % procedures for the io:state type are not counted as having io:state
    % args, since they do not fall into the scheme of one input and one
    % output arg. Since they should never be called, this should not matter.
    %
:- pred proc_info_has_io_state_pair(module_info::in, proc_info::in,
    int::out, int::out) is semidet.

:- pred proc_info_has_io_state_pair_from_details(module_info::in,
    list(prog_var)::in, list(mer_mode)::in, vartypes::in,
    int::out, int::out) is semidet.

    % Given a procedure table and the id of a procedure in that table,
    % return a procedure id to be attached to a clone of that procedure.
    % (The task of creating the clone proc_info and inserting into the
    % procedure table is the task of the caller.)
    %
:- pred clone_proc_id(proc_table::in, proc_id::in, proc_id::out) is det.

    % When mode inference is enabled, we record for each inferred
    % mode whether it is valid or not by keeping a list of error
    % messages in the proc_info.  The mode is valid iff this list
    % is empty.
    %
:- func mode_errors(proc_info) = list(mode_error_info).
:- func 'mode_errors :='(proc_info, list(mode_error_info)) = proc_info.
:- pred proc_info_is_valid_mode(proc_info::in) is semidet.

    % Make sure that all headvars are named. This can be useful e.g.
    % becasue the debugger ignores unnamed variables.
    %
:- pred ensure_all_headvars_are_named(proc_info::in, proc_info::out) is det.

    % Test whether the variable is of a dummy type, based on the vartypes.
    %
:- pred var_is_of_dummy_type(module_info::in, vartypes::in, prog_var::in)
    is semidet.

:- implementation.

:- import_module check_hlds.mode_errors.
:- import_module mdbcomp.program_representation.

:- type proc_info --->
    proc_info(
        proc_context                :: prog_context,
                                    % The context of the `:- mode' decl
                                    % (or the context of the first clause,
                                    % if there was no mode declaration).
        prog_varset                 :: prog_varset,
        var_types                   :: vartypes,
        head_vars                   :: list(prog_var),
        inst_varset                 :: inst_varset,
        maybe_declared_head_modes   :: maybe(list(mer_mode)),
                                    % The declared modes of arguments.
        actual_head_modes           :: list(mer_mode),
        maybe_head_modes_constraint :: maybe(mode_constraint),
        head_var_caller_liveness    :: maybe(list(is_live)),
                                    % Liveness (in the mode analysis sense)
                                    % of the arguments in the caller; says
                                    % whether each argument may be used
                                    % after the call.
        declared_detism             :: maybe(determinism),
                                    % The _declared_ determinism of the
                                    % procedure, or `no' if there was no
                                    % detism decl.
        inferred_detism             :: determinism,
        body                        :: hlds_goal,
        can_process                 :: bool,
                                    % No if we must not process this procedure
                                    % yet (used to delay mode checking etc.
                                    % for complicated modes of unification
                                    % procs until the end of the unique_modes
                                    % pass.)
        mode_errors                 :: list(mode_error_info),
        proc_rtti_varmaps           :: rtti_varmaps,
                                    % Information about type_infos and
                                    % typeclass_infos.
        eval_method                 :: eval_method,
                                    % How should the proc be evaluated.

        proc_sub_info               :: proc_sub_info
    ).

:- type proc_sub_info --->
    proc_sub_info(
        maybe_arg_sizes             :: maybe(arg_size_info),
                                    % Information about the relative sizes
                                    % of the input and output args of the
                                    % procedure. Set by termination
                                    % analysis.

        maybe_termination           :: maybe(termination_info),
                                    % The termination properties of the
                                    % procedure. Set by termination
                                    % analysis.
        termination2                :: termination2_info,
                                    % Termination properties and argument
                                    % size constraints for the procedure.
                                    % Set by termination2 analysis.

        is_address_taken            :: is_address_taken,
                                    % Is the address of this procedure
                                    % taken? If yes, we will need to use
                                    % typeinfo liveness for them, so that
                                    % deep_copy and accurate gc have the
                                    % RTTI they need for copying closures.
                                    %
                                    % Note that any non-local procedure
                                    % must be considered as having its
                                    % address taken, since it is possible
                                    % that some other module may do so.

        stack_slots                 :: stack_slots,
                                    % Allocation of variables to stack slots.

        arg_pass_info               :: maybe(list(arg_info)),
                                    % The calling convention of each arg:
                                    % information computed by arg_info.m
                                    % (based on the modes etc.)
                                    % and used by code generation
                                    % to determine how each argument
                                    % should be passed.

        initial_liveness            :: liveness_info,
                                    % The initial liveness, for code
                                    % generation.

        need_maxfr_slot             :: bool,
                                    % True iff tracing is enabled, this
                                    % is a procedure that lives on the det
                                    % stack, and the code of this procedure
                                    % may create a frame on the det stack.
                                    % (Only in these circumstances do we
                                    % need to reserve a stack slot to hold
                                    % the value of maxfr at the call, for
                                    % use in implementing retry.)
                                    %
                                    % This slot is used only with the LLDS
                                    % backend XXX. Its value is set during
                                    % the live_vars pass; it is invalid
                                    % before then.

        call_table_tip              :: maybe(prog_var),
                                    % If the procedure's evaluation method
                                    % is memo, loopcheck or minimal, this
                                    % slot identifies the variable that
                                    % holds the tip of the call table.
                                    % Otherwise, this field will be set to
                                    % `no'.
                                    %
                                    % Tabled procedures record, in the
                                    % data structure identified by this
                                    % variable, that the call is active.
                                    % When performing a retry across
                                    % such a procedure, we must reset
                                    % the state of the call; if we don't,
                                    % the retried call will find the
                                    % active call and report an infinite
                                    % loop error.
                                    %
                                    % Such resetting of course requires
                                    % the debugger to know whether the
                                    % procedure has reached the call table
                                    % tip yet. Therefore when binding this
                                    % variable, the code generator of the
                                    % relevant backend must record this
                                    % fact in a place accessible to the
                                    % debugger, if debugging is enabled.

        maybe_table_info            :: maybe(proc_table_info),
                                    % If set, it means that procedure
                                    % has been subject to a tabling
                                    % transformation, either I/O tabling
                                    % or the regular kind. In the former
                                    % case, the argument will contain all
                                    % the information we need to display
                                    % I/O actions involving this procedure;
                                    % in the latter case, it will contain
                                    % all the information we need to display
                                    % the call tables, answer tables and
                                    % answer blocks of the procedure.
                                    % XXX For now, the compiler fully
                                    % supports only procedures whose
                                    % arguments are all either ints, floats
                                    % or strings. However, this is still
                                    % sufficient for debugging most
                                    % problems in the tabling system.

        maybe_deep_profile_proc_info :: maybe(deep_profile_proc_info),

        maybe_untuple_info          :: maybe(untuple_proc_info), 
                                    % If set, it means this procedure was
                                    % created from another procedure by the
                                    % untupling transformation. This slot
                                    % records which of the procedure's
                                    % arguments were derived from which
                                    % arguments in the original procedure.
                                    
        maybe_structure_sharing     :: maybe(structure_sharing_domain)
                                    % Structure sharing information as obtained
                                    % by the structure sharing analysis.
    ).

    % Some parts of the procedure aren't known yet. We initialize
    % them to any old garbage which we will later throw away.

    % Inferred determinism gets initialized to `erroneous'.
    % This is what `det_analysis.m' wants. det_analysis.m
    % will later provide the correct inferred determinism for it.

proc_info_init(MContext, Arity, Types, DeclaredModes, Modes, MaybeArgLives,
        MaybeDet, IsAddressTaken, NewProc) :-
    make_n_fresh_vars("HeadVar__", Arity, HeadVars, varset__init, BodyVarSet),
    varset__init(InstVarSet),
    map__from_corresponding_lists(HeadVars, Types, BodyTypes),
    ModeErrors = [],
    InferredDet = erroneous,
    map__init(StackSlots),
    set__init(InitialLiveness),
    ArgInfo = no,
    goal_info_init(GoalInfo),
    ClauseBody = conj(plain_conj, []) - GoalInfo,
    CanProcess = yes,
    rtti_varmaps_init(RttiVarMaps),
    Term2Info = term_constr_main__term2_info_init,
    NewProc = proc_info(MContext, BodyVarSet, BodyTypes, HeadVars, InstVarSet,
        DeclaredModes, Modes, no, MaybeArgLives, MaybeDet, InferredDet,
        ClauseBody, CanProcess, ModeErrors, RttiVarMaps, eval_normal,
        proc_sub_info(no, no, Term2Info, IsAddressTaken, StackSlots,
        ArgInfo, InitialLiveness, no, no, no, no, no, no)).

proc_info_set(Context, BodyVarSet, BodyTypes, HeadVars, InstVarSet, HeadModes,
        HeadLives, DeclaredDetism, InferredDetism, Goal, CanProcess,
        RttiVarMaps, ArgSizes, Termination, Termination2,
        IsAddressTaken, StackSlots, ArgInfo, Liveness, ProcInfo) :-
    ModeErrors = [],
    ProcSubInfo = proc_sub_info(ArgSizes, Termination, Termination2,
        IsAddressTaken, StackSlots, ArgInfo, Liveness, no, no, no, no, no, no),
    ProcInfo = proc_info(Context, BodyVarSet, BodyTypes, HeadVars,
        InstVarSet, no, HeadModes, no, HeadLives,
        DeclaredDetism, InferredDetism, Goal, CanProcess, ModeErrors,
        RttiVarMaps, eval_normal, ProcSubInfo).

proc_info_create(Context, VarSet, VarTypes, HeadVars, InstVarSet,
        HeadModes, Detism, Goal, RttiVarMaps, IsAddressTaken,
        ProcInfo) :-
    proc_info_create(Context, VarSet, VarTypes, HeadVars, InstVarSet,
        HeadModes, yes(Detism), Detism, Goal, RttiVarMaps,
        IsAddressTaken, ProcInfo).

proc_info_create(Context, VarSet, VarTypes, HeadVars, InstVarSet, HeadModes,
        MaybeDeclaredDetism, Detism, Goal, RttiVarMaps, IsAddressTaken,
        ProcInfo) :-
    map__init(StackSlots),
    set__init(Liveness),
    MaybeHeadLives = no,
    ModeErrors = [],
    Term2Info = term_constr_main__term2_info_init,
    ProcSubInfo = proc_sub_info(no, no, Term2Info, IsAddressTaken,
        StackSlots, no, Liveness, no, no, no, no, no, no),
    ProcInfo = proc_info(Context, VarSet, VarTypes, HeadVars,
        InstVarSet, no, HeadModes, no, MaybeHeadLives,
        MaybeDeclaredDetism, Detism, Goal, yes, ModeErrors,
        RttiVarMaps, eval_normal, ProcSubInfo).

proc_info_set_body(VarSet, VarTypes, HeadVars, Goal, RttiVarMaps,
        !ProcInfo) :-
    !:ProcInfo = !.ProcInfo ^ prog_varset := VarSet,
    !:ProcInfo = !.ProcInfo ^ var_types := VarTypes,
    !:ProcInfo = !.ProcInfo ^ head_vars := HeadVars,
    !:ProcInfo = !.ProcInfo ^ body := Goal,
    !:ProcInfo = !.ProcInfo ^ proc_rtti_varmaps := RttiVarMaps.

proc_info_context(PI, PI ^ proc_context).
proc_info_varset(PI, PI ^ prog_varset).
proc_info_vartypes(PI, PI ^ var_types).
proc_info_headvars(PI, PI ^ head_vars).
proc_info_inst_varset(PI, PI ^ inst_varset).
proc_info_maybe_declared_argmodes(PI, PI ^ maybe_declared_head_modes).
proc_info_argmodes(PI, PI ^ actual_head_modes).
proc_info_maybe_arglives(PI, PI ^ head_var_caller_liveness).
proc_info_declared_determinism(PI, PI ^ declared_detism).
proc_info_inferred_determinism(PI, PI ^ inferred_detism).
proc_info_goal(PI, PI ^ body).
proc_info_can_process(PI, PI ^ can_process).
proc_info_rtti_varmaps(PI, PI ^ proc_rtti_varmaps).
proc_info_eval_method(PI, PI ^ eval_method).
proc_info_get_maybe_arg_size_info(PI, PI ^ proc_sub_info ^ maybe_arg_sizes).
proc_info_get_maybe_termination_info(PI,
    PI ^ proc_sub_info ^ maybe_termination).
proc_info_is_address_taken(PI, PI ^ proc_sub_info ^ is_address_taken).
proc_info_stack_slots(PI, PI ^ proc_sub_info ^ stack_slots).
proc_info_maybe_arg_info(PI, PI ^ proc_sub_info ^ arg_pass_info).
proc_info_liveness_info(PI, PI ^ proc_sub_info ^ initial_liveness).
proc_info_get_need_maxfr_slot(PI, PI ^ proc_sub_info ^ need_maxfr_slot).
proc_info_get_call_table_tip(PI, PI ^ proc_sub_info ^ call_table_tip).
proc_info_get_maybe_proc_table_info(PI, PI ^ proc_sub_info ^ maybe_table_info).
proc_info_get_maybe_deep_profile_info(PI,
    PI ^ proc_sub_info ^ maybe_deep_profile_proc_info).
proc_info_get_maybe_untuple_info(PI,
    PI ^ proc_sub_info ^ maybe_untuple_info).

proc_info_set_varset(VS, PI, PI ^ prog_varset := VS).
proc_info_set_vartypes(VT, PI, PI ^ var_types := VT).
proc_info_set_headvars(HV, PI, PI ^ head_vars := HV).
proc_info_set_inst_varset(IV, PI, PI ^ inst_varset := IV).
proc_info_set_maybe_declared_argmodes(AM, PI,
    PI ^ maybe_declared_head_modes := AM).
proc_info_set_argmodes(AM, PI, PI ^ actual_head_modes := AM).
proc_info_set_maybe_arglives(CL, PI, PI ^ head_var_caller_liveness := CL).
proc_info_set_inferred_determinism(ID, PI, PI ^ inferred_detism := ID).
proc_info_set_goal(G, PI, PI ^ body := G).
proc_info_set_can_process(CP, PI, PI ^ can_process := CP).
proc_info_set_rtti_varmaps(RI, PI, PI ^ proc_rtti_varmaps := RI).
proc_info_set_eval_method(EM, PI, PI ^ eval_method := EM).
proc_info_set_maybe_arg_size_info(MAS, PI,
    PI ^ proc_sub_info ^ maybe_arg_sizes := MAS).
proc_info_set_maybe_termination_info(MT, PI,
    PI ^ proc_sub_info ^ maybe_termination := MT).
proc_info_set_address_taken(AT, PI,
    PI ^ proc_sub_info ^ is_address_taken := AT).
proc_info_set_stack_slots(SS, PI, PI ^ proc_sub_info ^ stack_slots := SS).
proc_info_set_arg_info(AP, PI, PI ^ proc_sub_info ^ arg_pass_info := yes(AP)).
proc_info_set_liveness_info(IL, PI,
    PI ^ proc_sub_info ^ initial_liveness := IL).
proc_info_set_need_maxfr_slot(NMS, PI,
    PI ^ proc_sub_info ^ need_maxfr_slot := NMS).
proc_info_set_call_table_tip(CTT, PI,
    PI ^ proc_sub_info ^ call_table_tip := CTT).
proc_info_set_maybe_proc_table_info(MTI, PI,
    PI ^ proc_sub_info ^ maybe_table_info := MTI).
proc_info_set_maybe_deep_profile_info(DPI, PI,
    PI ^ proc_sub_info ^ maybe_deep_profile_proc_info := DPI).
proc_info_set_maybe_untuple_info(MUI, PI,
    PI ^ proc_sub_info ^ maybe_untuple_info := MUI).

proc_info_head_modes_constraint(ProcInfo, HeadModesConstraint) :-
    MaybeHeadModesConstraint = ProcInfo ^ maybe_head_modes_constraint,
    (
        MaybeHeadModesConstraint = yes(HeadModesConstraint)
    ;
        MaybeHeadModesConstraint = no,
        unexpected(this_file, "proc_info_head_modes_constraint: no constraint")
    ).

proc_info_set_head_modes_constraint(HMC, ProcInfo,
    ProcInfo ^ maybe_head_modes_constraint := yes(HMC)).

proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap) :-
    proc_info_headvars(ProcInfo, HeadVars),
    proc_info_argmodes(ProcInfo, ArgModes),
    mode_list_get_initial_insts(ModuleInfo, ArgModes, InitialInsts),
    assoc_list__from_corresponding_lists(HeadVars, InitialInsts, InstAL),
    instmap__from_assoc_list(InstAL, InstMap).

proc_info_declared_argmodes(ProcInfo, ArgModes) :-
    proc_info_maybe_declared_argmodes(ProcInfo, MaybeArgModes),
    (
        MaybeArgModes = yes(ArgModes1),
        ArgModes = ArgModes1
    ;
        MaybeArgModes = no,
        proc_info_argmodes(ProcInfo, ArgModes)
    ).

proc_info_interface_determinism(ProcInfo, Determinism) :-
    proc_info_declared_determinism(ProcInfo, MaybeDeterminism),
    (
        MaybeDeterminism = no,
        proc_info_inferred_determinism(ProcInfo, Determinism)
    ;
        MaybeDeterminism = yes(Determinism)
    ).

    % Return Result = yes if the called predicate is known to never succeed.
    %
proc_info_never_succeeds(ProcInfo, Result) :-
    proc_info_declared_determinism(ProcInfo, DeclaredDeterminism),
    (
        DeclaredDeterminism = no,
        Result = no
    ;
        DeclaredDeterminism = yes(Determinism),
        determinism_components(Determinism, _, HowMany),
        ( HowMany = at_most_zero ->
            Result = yes
        ;
            Result = no
        )
    ).

proc_info_arglives(ProcInfo, ModuleInfo, ArgLives) :-
    proc_info_maybe_arglives(ProcInfo, MaybeArgLives),
    (
        MaybeArgLives = yes(ArgLives0),
        ArgLives = ArgLives0
    ;
        MaybeArgLives = no,
        proc_info_argmodes(ProcInfo, Modes),
        get_arg_lives(ModuleInfo, Modes, ArgLives)
    ).

proc_info_is_valid_mode(ProcInfo) :-
    ProcInfo ^ mode_errors = [].

proc_info_arg_info(ProcInfo, ArgInfo) :-
    proc_info_maybe_arg_info(ProcInfo, MaybeArgInfo0),
    (
        MaybeArgInfo0 = yes(ArgInfo)
    ;
        MaybeArgInfo0 = no,
        unexpected(this_file, "proc_info_arg_info: arg_pass_info not set")
    ).

proc_info_get_termination2_info(ProcInfo, Termination2Info) :-
    Termination2Info = ProcInfo ^ proc_sub_info ^ termination2.

proc_info_set_termination2_info(Termination2Info, !ProcInfo) :-
    !:ProcInfo = !.ProcInfo ^ proc_sub_info ^ termination2 :=
        Termination2Info.

proc_info_get_structure_sharing(ProcInfo, MaybeSharing) :-
    MaybeSharing = ProcInfo ^ proc_sub_info ^ maybe_structure_sharing.

proc_info_set_structure_sharing(Sharing, !ProcInfo) :- 
    !:ProcInfo = !.ProcInfo ^ proc_sub_info ^ maybe_structure_sharing :=
        yes(Sharing).

proc_info_get_typeinfo_vars(Vars, VarTypes, RttiVarMaps, TypeInfoVars) :-
    TVarMap = RttiVarMaps ^ ti_varmap,
    set__to_sorted_list(Vars, VarList),
    proc_info_get_typeinfo_vars_2(VarList, VarTypes, TVarMap, TypeInfoVarList),
    set__list_to_set(TypeInfoVarList, TypeInfoVars).

    % auxiliary predicate - traverses variables and builds a list of
    % variables that store typeinfos for these variables.
:- pred proc_info_get_typeinfo_vars_2(list(prog_var)::in,
    vartypes::in, type_info_varmap::in, list(prog_var)::out) is det.

proc_info_get_typeinfo_vars_2([], _, _, []).
proc_info_get_typeinfo_vars_2([Var | Vars], VarTypes, TVarMap, TypeInfoVars) :-
    ( map__search(VarTypes, Var, Type) ->
        prog_type__vars(Type, TypeVars),
        (
            % Optimize common case
            TypeVars = []
        ->
            proc_info_get_typeinfo_vars_2(Vars, VarTypes, TVarMap,
                TypeInfoVars)
        ;
            % XXX It's possible there are some complications with
            % higher order pred types here -- if so, maybe
            % treat them specially.

                % The type_info is either stored in a variable,
                % or in a typeclass_info. Either get the
                % type_info variable or the typeclass_info
                % variable
            LookupVar = (pred(TVar::in, TVarVar::out) is det :-
                    map__lookup(TVarMap, TVar, Locn),
                    type_info_locn_var(Locn, TVarVar)
                ),
            list__map(LookupVar, TypeVars, TypeInfoVars0),

            proc_info_get_typeinfo_vars_2(Vars, VarTypes, TVarMap,
                TypeInfoVars1),
            list__append(TypeInfoVars0, TypeInfoVars1, TypeInfoVars)
        )
    ;
        unexpected(this_file,
            "proc_info_get_typeinfo_vars_2: var not found in typemap")
    ).

proc_info_maybe_complete_with_typeinfo_vars(Vars0, TypeInfoLiveness,
        VarTypes, RttiVarMaps, Vars) :-
    (
        TypeInfoLiveness = yes,
        proc_info_get_typeinfo_vars(Vars0, VarTypes, RttiVarMaps,
            TypeInfoVars),
        set__union(Vars0, TypeInfoVars, Vars)
    ;
        TypeInfoLiveness = no,
        Vars = Vars0
    ).

proc_info_ensure_unique_names(!ProcInfo) :-
    proc_info_vartypes(!.ProcInfo, VarTypes),
    map__keys(VarTypes, AllVars),
    proc_info_varset(!.ProcInfo, VarSet0),
    varset__ensure_unique_names(AllVars, "p", VarSet0, VarSet),
    proc_info_set_varset(VarSet, !ProcInfo).

proc_info_create_var_from_type(Type, MaybeName, NewVar, !ProcInfo) :-
    proc_info_varset(!.ProcInfo, VarSet0),
    proc_info_vartypes(!.ProcInfo, VarTypes0),
    varset__new_maybe_named_var(VarSet0, MaybeName, NewVar, VarSet),
    map__det_insert(VarTypes0, NewVar, Type, VarTypes),
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo).

proc_info_create_vars_from_types(Types, NewVars, !ProcInfo) :-
    list__length(Types, NumVars),
    proc_info_varset(!.ProcInfo, VarSet0),
    proc_info_vartypes(!.ProcInfo, VarTypes0),
    varset__new_vars(VarSet0, NumVars, NewVars, VarSet),
    map__det_insert_from_corresponding_lists(VarTypes0, NewVars,
        Types, VarTypes),
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo).

proc_info_instantiated_head_vars(ModuleInfo, ProcInfo, ChangedInstHeadVars) :-
    proc_info_headvars(ProcInfo, HeadVars),
    proc_info_argmodes(ProcInfo, ArgModes),
    proc_info_vartypes(ProcInfo, VarTypes),
    assoc_list__from_corresponding_lists(HeadVars, ArgModes, HeadVarModes),
    IsInstChanged = (pred(VarMode::in, Var::out) is semidet :-
        VarMode = Var - Mode,
        map__lookup(VarTypes, Var, Type),
        mode_get_insts(ModuleInfo, Mode, Inst1, Inst2),
        \+ inst_matches_binding(Inst1, Inst2, Type, ModuleInfo)
    ),
    list__filter_map(IsInstChanged, HeadVarModes, ChangedInstHeadVars).

proc_info_uninstantiated_head_vars(ModuleInfo, ProcInfo,
        UnchangedInstHeadVars) :-
    proc_info_headvars(ProcInfo, HeadVars),
    proc_info_argmodes(ProcInfo, ArgModes),
    proc_info_vartypes(ProcInfo, VarTypes),
    assoc_list__from_corresponding_lists(HeadVars, ArgModes, HeadVarModes),
    IsInstUnchanged = (pred(VarMode::in, Var::out) is semidet :-
        VarMode = Var - Mode,
        map__lookup(VarTypes, Var, Type),
        mode_get_insts(ModuleInfo, Mode, Inst1, Inst2),
        inst_matches_binding(Inst1, Inst2, Type, ModuleInfo)
    ),
    list__filter_map(IsInstUnchanged, HeadVarModes, UnchangedInstHeadVars).

proc_interface_should_use_typeinfo_liveness(PredInfo, ProcId, Globals,
        InterfaceTypeInfoLiveness) :-
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    ( no_type_info_builtin(PredModule, PredName, PredArity) ->
        InterfaceTypeInfoLiveness = no
    ;
        pred_info_import_status(PredInfo, Status),
        pred_info_procedures(PredInfo, ProcTable),
        map__lookup(ProcTable, ProcId, ProcInfo),
        proc_info_is_address_taken(ProcInfo, IsAddressTaken),
        non_special_interface_should_use_typeinfo_liveness(Status,
            IsAddressTaken, Globals, InterfaceTypeInfoLiveness)
    ).

non_special_interface_should_use_typeinfo_liveness(Status, IsAddressTaken,
        Globals, InterfaceTypeInfoLiveness) :-
    (
        (
            IsAddressTaken = address_is_taken
        ;
            % If the predicate is exported, its address may have
            % been taken elsewhere. If it is imported, then it
            % follows that it must be exported somewhere.
            Status \= local
        ;
            % If term size profiling (of either form) is enabled,
            % then we may need to access the typeinfo of any
            % variable bound to a heap cell argument. The only way
            % to ensure that this is possible is to preserve the
            % ability to access the typeinfo of any variable.
            globals__lookup_bool_option(Globals,
                record_term_sizes_as_words, yes)
        ;
            globals__lookup_bool_option(Globals,
                record_term_sizes_as_cells, yes)
        ;
            non_special_body_should_use_typeinfo_liveness(Globals,
                yes)
        )
    ->
        InterfaceTypeInfoLiveness = yes
    ;
        InterfaceTypeInfoLiveness = no
    ).

body_should_use_typeinfo_liveness(PredInfo, Globals, BodyTypeInfoLiveness) :-
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    ( no_type_info_builtin(PredModule, PredName, PredArity) ->
        BodyTypeInfoLiveness = no
    ;
        non_special_body_should_use_typeinfo_liveness(Globals,
            BodyTypeInfoLiveness)
    ).

non_special_body_should_use_typeinfo_liveness(Globals, BodyTypeInfoLiveness) :-
    globals__lookup_bool_option(Globals, body_typeinfo_liveness,
        BodyTypeInfoLiveness).

proc_info_has_io_state_pair(ModuleInfo, ProcInfo, InArgNum, OutArgNum) :-
    proc_info_headvars(ProcInfo, HeadVars),
    proc_info_argmodes(ProcInfo, ArgModes),
    proc_info_vartypes(ProcInfo, VarTypes),
    proc_info_has_io_state_pair_from_details(ModuleInfo, HeadVars,
        ArgModes, VarTypes, InArgNum, OutArgNum).

proc_info_has_io_state_pair_from_details(ModuleInfo, HeadVars, ArgModes,
        VarTypes, InArgNum, OutArgNum) :-
    assoc_list__from_corresponding_lists(HeadVars, ArgModes, HeadVarsModes),
    proc_info_has_io_state_pair_2(HeadVarsModes, ModuleInfo, VarTypes,
        1, no, MaybeIn, no, MaybeOut),
    (
        MaybeIn = yes(In),
        MaybeOut = yes(Out)
    ->
        InArgNum = In,
        OutArgNum = Out
    ;
        fail
    ).

:- pred proc_info_has_io_state_pair_2(assoc_list(prog_var, mer_mode)::in,
    module_info::in, vartypes::in, int::in,
    maybe(int)::in, maybe(int)::out, maybe(int)::in, maybe(int)::out)
    is semidet.

proc_info_has_io_state_pair_2([], _, _, _, !MaybeIn, !MaybeOut).
proc_info_has_io_state_pair_2([Var - Mode | VarModes], ModuleInfo, VarTypes,
        ArgNum, !MaybeIn, !MaybeOut) :-
    (
        map__lookup(VarTypes, Var, VarType),
        type_is_io_state(VarType)
    ->
        ( mode_is_fully_input(ModuleInfo, Mode) ->
            (
                !.MaybeIn = no,
                !:MaybeIn = yes(ArgNum)
            ;
                !.MaybeIn = yes(_),
                % Procedures with two input arguments of type io__state
                % (e.g. the automatically generated unification or comparison
                % procedure for the io__state type) do not fall into the
                % one input/one output pattern we are looking for.
                fail
            )
        ; mode_is_fully_output(ModuleInfo, Mode) ->
            (
                !.MaybeOut = no,
                !:MaybeOut = yes(ArgNum)
            ;
                !.MaybeOut = yes(_),
                % Procedures with two output arguments of type io__state
                % do not fall into the one input/one output pattern we are
                % looking for.
                fail
            )
        ;
            fail
        )
    ;
        true
    ),
    proc_info_has_io_state_pair_2(VarModes, ModuleInfo, VarTypes,
        ArgNum + 1, !MaybeIn, !MaybeOut).

clone_proc_id(ProcTable, _ProcId, CloneProcId) :-
    find_lowest_unused_proc_id(ProcTable, CloneProcId).

:- pred find_lowest_unused_proc_id(proc_table::in, proc_id::out) is det.

find_lowest_unused_proc_id(ProcTable, CloneProcId) :-
    find_lowest_unused_proc_id_2(0, ProcTable, CloneProcId).

:- pred find_lowest_unused_proc_id_2(proc_id::in, proc_table::in, proc_id::out)
    is det.

find_lowest_unused_proc_id_2(TrialProcId, ProcTable, CloneProcId) :-
    ( map__search(ProcTable, TrialProcId, _) ->
        find_lowest_unused_proc_id_2(TrialProcId + 1, ProcTable, CloneProcId)
    ;
        CloneProcId = TrialProcId
    ).

ensure_all_headvars_are_named(!ProcInfo) :-
    proc_info_headvars(!.ProcInfo, HeadVars),
    proc_info_varset(!.ProcInfo, VarSet0),
    ensure_all_headvars_are_named_2(HeadVars, 1, VarSet0, VarSet),
    proc_info_set_varset(VarSet, !ProcInfo).

:- pred ensure_all_headvars_are_named_2(list(prog_var)::in, int::in,
    prog_varset::in, prog_varset::out) is det.

ensure_all_headvars_are_named_2([], _, !VarSet).
ensure_all_headvars_are_named_2([Var | Vars], SeqNum, !VarSet) :-
    ( varset__search_name(!.VarSet, Var, _Name) ->
        true
    ;
        Name = "HeadVar__" ++ int_to_string(SeqNum),
        varset__name_var(!.VarSet, Var, Name, !:VarSet)
    ),
    ensure_all_headvars_are_named_2(Vars, SeqNum + 1, !VarSet).

var_is_of_dummy_type(ModuleInfo, VarTypes, Var) :-
    map__lookup(VarTypes, Var, Type),
    is_dummy_argument_type(ModuleInfo, Type).

%-----------------------------------------------------------------------------%

    % Predicates to deal with record syntax.

:- interface.

    % field_extraction_function_args(Args, InputTermArg).
    % Work out which arguments of a field access correspond to the
    % field being extracted/set, and which are the container arguments.
    %
:- pred field_extraction_function_args(list(prog_var)::in, prog_var::out)
    is det.

    % field_update_function_args(Args, InputTermArg, FieldArg).
    %
:- pred field_update_function_args(list(prog_var)::in, prog_var::out,
    prog_var::out) is det.

    % field_access_function_name(AccessType, FieldName, FuncName).
    %
    % From the access type and the name of the field,
    % construct a function name.
    %
:- pred field_access_function_name(field_access_type::in, ctor_field_name::in,
    sym_name::out) is det.

    % is_field_access_function_name(ModuleInfo, FuncName, Arity,
    %   AccessType, FieldName).
    %
    % Inverse of the above.
    %
:- pred is_field_access_function_name(module_info::in, sym_name::in,
    arity::out, field_access_type::out, ctor_field_name::out) is semidet.

:- pred pred_info_is_field_access_function(module_info::in, pred_info::in)
    is semidet.

:- implementation.

field_extraction_function_args(Args, TermInputArg) :-
    ( Args = [TermInputArg0] ->
        TermInputArg = TermInputArg0
    ;
        unexpected(this_file, "field_extraction_function_args")
    ).

field_update_function_args(Args, TermInputArg, FieldArg) :-
    ( Args = [TermInputArg0, FieldArg0] ->
        FieldArg = FieldArg0,
        TermInputArg = TermInputArg0
    ;
        unexpected(this_file, "field_update_function_args")
    ).

field_access_function_name(get, FieldName, FieldName).
field_access_function_name(set, FieldName, FuncName) :-
    add_sym_name_suffix(FieldName, " :=", FuncName).

is_field_access_function_name(ModuleInfo, FuncName, Arity,
        AccessType, FieldName) :-
    ( remove_sym_name_suffix(FuncName, " :=", FieldName0) ->
        Arity = 2,
        AccessType = set,
        FieldName = FieldName0
    ;
        Arity = 1,
        AccessType = get,
        FieldName = FuncName
    ),
    module_info_get_ctor_field_table(ModuleInfo, CtorFieldTable),
    map__contains(CtorFieldTable, FieldName).

pred_info_is_field_access_function(ModuleInfo, PredInfo) :-
    pred_info_is_pred_or_func(PredInfo) = function,
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    adjust_func_arity(function, FuncArity, PredArity),
    is_field_access_function_name(ModuleInfo, qualified(Module, Name),
        FuncArity, _, _).

%-----------------------------------------------------------------------------%

    % Predicates to deal with builtins.

:- interface.

    % is_unify_or_compare_pred(PredInfo) succeeds iff the PredInfo is for a
    % compiler generated instance of a type-specific special_pred (i.e. one
    % of the unify, compare, or index predicates generated as a type-specific
    % instance of unify/2, index/2, or compare/3).
    %
:- pred is_unify_or_compare_pred(pred_info::in) is semidet.

    % Is the argument the pred_info for a builtin that can be generated inline?
    %
:- pred pred_info_is_builtin(pred_info::in) is semidet.

    % builtin_state(ModuleInfo, CallerPredId, PredId, ProcId, BuiltinState)
    %
    % Is the given procedure a builtin that should be generated inline
    % in the given caller?
    %
:- func builtin_state(module_info, pred_id, pred_id, proc_id) = builtin_state.

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.special_pred.

pred_info_is_builtin(PredInfo) :-
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    ProcId = initial_proc_id,
    is_inline_builtin(ModuleName, PredName, ProcId, Arity).

builtin_state(ModuleInfo, CallerPredId, PredId, ProcId) = BuiltinState :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals__lookup_bool_option(Globals, allow_inlining, AllowInlining),
    globals__lookup_bool_option(Globals, inline_builtins, InlineBuiltins),
    (
        % The automatically generated "recursive" call in the
        % goal for each builtin must be generated inline, or
        % we would generate an infinite loop.
        (
            AllowInlining = yes,
            InlineBuiltins = yes
        ;
            CallerPredId = PredId
        ),
        is_inline_builtin(ModuleName, PredName, ProcId, Arity)
    ->
        BuiltinState = inline_builtin
    ;
        BuiltinState = not_builtin
    ).

:- pred is_inline_builtin(module_name::in, string::in, proc_id::in, arity::in)
    is semidet.

is_inline_builtin(ModuleName, PredName, ProcId, Arity) :-
    Arity =< 3,
    prog_varset_init(VarSet),
    varset__new_vars(VarSet, Arity, Args, _),
    builtin_ops__translate_builtin(ModuleName, PredName, ProcId, Args, _).

:- pred prog_varset_init(prog_varset::out) is det.

prog_varset_init(VarSet) :- varset__init(VarSet).

is_unify_or_compare_pred(PredInfo) :-
    pred_info_get_origin(PredInfo, special_pred(_)). % XXX bug

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

    % Check if the given evaluation method is allowed with
    % the given determinism.
    %
:- func valid_determinism_for_eval_method(eval_method, determinism) = bool.

    % Return true if the given evaluation method requires a
    % stratification check.
    %
:- func eval_method_needs_stratification(eval_method) = bool.

    % Return true if the given evaluation method uses a per-procedure
    % tabling pointer. If so, the back-end must generate a declaration
    % for the variable to hold the table.
    %
:- func eval_method_has_per_proc_tabling_pointer(eval_method) = bool.

    % Return true if the given evaluation method requires the body
    % of the procedure using it to be transformed by table_gen.m.
    %
:- func eval_method_requires_tabling_transform(eval_method) = bool.

    % Return true if the given evaluation method requires the arguments
    % of the procedure using it to be ground.
    %
:- func eval_method_requires_ground_args(eval_method) = bool.

    % Return true if the given evaluation method requires the arguments
    % of the procedure using it to be non-unique.
    %
:- func eval_method_destroys_uniqueness(eval_method) = bool.

    % Return the change a given evaluation method can do to a given
    % determinism.
    %
:- func eval_method_change_determinism(eval_method, determinism) = determinism.

:- implementation.

valid_determinism_for_eval_method(eval_normal, _) = yes.
valid_determinism_for_eval_method(eval_loop_check, Detism) = Valid :-
    determinism_components(Detism, _, MaxSoln),
    ( MaxSoln = at_most_zero ->
        Valid = no
    ;
        Valid = yes
    ).
valid_determinism_for_eval_method(eval_memo(_), Detism) = Valid :-
    determinism_components(Detism, _, MaxSoln),
    ( MaxSoln = at_most_zero ->
        Valid = no
    ;
        Valid = yes
    ).
valid_determinism_for_eval_method(eval_table_io(_, _), _) = _ :-
    unexpected(this_file,
        "valid_determinism_for_eval_method called after tabling phase").
valid_determinism_for_eval_method(eval_minimal(_), Detism) = Valid :-
    % Determinism analysis isn't yet smart enough to know whether
    % a cannot_fail execution path is guaranteed not to go through
    % a call to a predicate that is mutually recursive with this one,
    % which (if this predicate is minimal model) is the only way that
    % the predicate can be properly cannot_fail. The problem is that in
    % in general, the mutually recursive predicate may be in another
    % module.
    determinism_components(Detism, CanFail, _),
    ( CanFail = can_fail ->
        Valid = yes
    ;
        Valid = no
    ).

eval_method_needs_stratification(eval_normal) = no.
eval_method_needs_stratification(eval_loop_check) = no.
eval_method_needs_stratification(eval_table_io(_, _)) = no.
eval_method_needs_stratification(eval_memo(_)) = no.
eval_method_needs_stratification(eval_minimal(_)) = yes.

eval_method_has_per_proc_tabling_pointer(eval_normal) = no.
eval_method_has_per_proc_tabling_pointer(eval_loop_check) = yes.
eval_method_has_per_proc_tabling_pointer(eval_table_io(_, _)) = no.
eval_method_has_per_proc_tabling_pointer(eval_memo(_)) = yes.
eval_method_has_per_proc_tabling_pointer(eval_minimal(_)) = yes.

eval_method_requires_tabling_transform(eval_normal) = no.
eval_method_requires_tabling_transform(eval_loop_check) = yes.
eval_method_requires_tabling_transform(eval_table_io(_, _)) = yes.
eval_method_requires_tabling_transform(eval_memo(_)) = yes.
eval_method_requires_tabling_transform(eval_minimal(_)) = yes.

eval_method_requires_ground_args(eval_normal) = no.
eval_method_requires_ground_args(eval_loop_check) = yes.
eval_method_requires_ground_args(eval_table_io(_, _)) = yes.
eval_method_requires_ground_args(eval_memo(_)) = yes.
eval_method_requires_ground_args(eval_minimal(_)) = yes.

eval_method_destroys_uniqueness(eval_normal) = no.
eval_method_destroys_uniqueness(eval_loop_check) = yes.
eval_method_destroys_uniqueness(eval_table_io(_, _)) = no.
eval_method_destroys_uniqueness(eval_memo(_)) = yes.
eval_method_destroys_uniqueness(eval_minimal(_)) = yes.

eval_method_change_determinism(eval_normal, Detism) = Detism.
eval_method_change_determinism(eval_loop_check, Detism) = Detism.
eval_method_change_determinism(eval_table_io(_, _), Detism) = Detism.
eval_method_change_determinism(eval_memo(_), Detism) = Detism.
eval_method_change_determinism(eval_minimal(_), Detism0) = Detism :-
    det_conjunction_detism(semidet, Detism0, Detism).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "hlds_pred.m".

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_pred.
%-----------------------------------------------------------------------------%
