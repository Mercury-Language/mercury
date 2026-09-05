%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2007, 2009-2012 The University of Melbourne.
% Copyright (C) 2014-2016, 2018, 2021-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_rtti.m.
% Main authors: Mark Brown.
%
% This module defines the part of the HLDS that keeps track of information
% relating to RTTI.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_rtti.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.pred_name.
:- import_module hlds.pred_proc_id.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module array.
:- import_module assoc_list.
:- import_module bool.
:- import_module list.

%---------------------------------------------------------------------------%

:- type prog_var_name == string.

    % The rtti_proc_label type holds all the information about a procedure
    % that we need to compute the entry label for that procedure
    % in the target language (the llds.code_addr or mlds.code_addr).

:- type rtti_proc_label
    --->    rtti_proc_label(
                rpl_pred_or_func            ::  pred_or_func,
                rpl_this_module             ::  module_name,
                rpl_proc_module             ::  module_name,
                rpl_proc_name               ::  string,
                rpl_proc_arity              ::  pred_form_arity,
                rpl_proc_arg_types          ::  list(mer_type),
                rpl_pred_id                 ::  pred_id,
                rpl_proc_id                 ::  proc_id,
                rpl_proc_headvars           ::  assoc_list(prog_var,
                                                prog_var_name),
                rpl_proc_top_modes          ::  list(top_functor_mode),
                rpl_proc_interface_detism   ::  determinism,

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

                rpl_pred_is_imported        ::  bool,
                rpl_pred_is_pseudo_imported ::  bool,
                rpl_pred_info_origin        ::  pred_origin,

                % The following boolean holds a value computed from the
                % proc_info, using procedure_is_exported/2

                rpl_proc_is_exported        ::  bool,

                % The following bool is true if the procedure was
                % imported, either because the containing predicate
                % was imported, or because it was pseudoimported
                % and the procedure is an in-in unify procedure.

                rpl_proc_is_imported        ::  bool
            ).

    % Construct an rtti_proc_label for a given procedure.
    %
:- func make_rtti_proc_label(module_info, pred_id, proc_id) = rtti_proc_label.

    % The inverse of make_rtti_proc_label.
    %
:- pred proc_label_pred_proc_id(rtti_proc_label::in,
    pred_id::out, proc_id::out) is det.

%---------------------------------------------------------------------------%
%
% Types and predicates to store information about RTTI.
%

    %  A type_info_locn specifies how to access a type_info.
    %
:- type type_info_locn
    --->    type_info(prog_var)
            % It is a normal type_info, i.e. the type is not constrained.

    ;       typeclass_info(prog_var, int).
            % The type_info is packed inside a typeclass_info. If the int is N,
            % it is the Nth type_info inside the typeclass_info, but there
            % may be several superclass pointers before the block of
            % type_infos, so it won't be the Nth word of the typeclass_info.
            %
            % To find the type_info inside the typeclass_info, use the
            % predicate type_info_from_typeclass_info from Mercury code;
            % from C code use the macro MR_typeclass_info_superclass_info.

    % type_info_locn_var(TypeInfoLocn, Var):
    %
    % Var is the variable corresponding to the TypeInfoLocn. Note that
    % this does *not* mean that Var is a type_info; it may be a typeclass_info
    % in which the type_info is nested.
    %
:- pred type_info_locn_var(type_info_locn::in, prog_var::out) is det.

:- pred type_info_locn_set_var(prog_var::in,
    type_info_locn::in, type_info_locn::out) is det.

%---------------------%

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

%---------------------------------------------------------------------------%
%
% Initialize rtti_varmaps.
%

    % This records information about how type_infos and typeclass_infos
    % were introduced in the polymorphism transformation.
    %
:- type rtti_varmaps.

    % Return an empty rtti_varmaps structure.
    %
:- pred rtti_varmaps_init(rtti_varmaps::out) is det.

%---------------------------------------------------------------------------%
%
% Update rtti_varmaps.
%

    % Insert the location of a type_info. Abort if such information
    % already exists.
    %
:- pred rtti_det_insert_type_info_locn(tvar::in, type_info_locn::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % Set the location of a type_info, overwriting any previous information.
    %
:- pred rtti_set_type_info_locn(tvar::in, type_info_locn::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

%---------------------%

    % For a prog_var which holds a type_info, set the type that the type_info
    % is for. Abort if such information already exists.
    %
:- pred rtti_det_insert_type_info_type(prog_var::in, mer_type::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % For a prog_var which holds a type_info, set the type that the type_info
    % is for, overwriting any previous information.
    %
:- pred rtti_set_type_info_type(prog_var::in, mer_type::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

%---------------------%

    % Insert the prog_var which contains the typeclass_info for a
    % given constraint. Abort if such information already exists.
    %
:- pred rtti_det_insert_typeclass_info_var(prog_constraint::in, prog_var::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % Set the prog_var which contains the typeclass_info for a given
    % constraint, overwriting any previous information.
    %
:- pred rtti_set_typeclass_info_var(prog_constraint::in, prog_var::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % Make the given typeclass_info var available for reuse in later goals.
    % Abort if we know nothing about this variable.
    %
:- pred rtti_reuse_typeclass_info_var(prog_var::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

%---------------------%

    % rtti_var_info_duplicate(Var, NewVar, !RttiVarMaps)
    %
    % Duplicate the rtti_var_info we have about Var for NewVar.
    %
:- pred rtti_var_info_duplicate(prog_var::in, prog_var::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % rtti_var_info_duplicate_replace(Var, NewVar, !RttiVarMaps)
    %
    % Duplicate the rtti_var_info we have about Var for NewVar.
    % Replace old information about Var which already exists.
    %
:- pred rtti_var_info_duplicate_replace(prog_var::in, prog_var::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

%---------------------%

    % Given an array in which the entry for a variable's integer form is true
    % iff the variable is actually used in a procedure body, restrict the
    % rtti_varmaps for that procedure to the variables needed.
    %
:- pred restrict_rtti_varmaps(array(bool)::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

%---------------------%

    % apply_renamings_and_subst_to_rtti_varmaps(TRenaming, TSubst, Renaming,
    %   !RttiVarMaps):
    %
    % Apply substitutions to the rtti_varmaps data. First apply TRenaming
    % to all types, then apply TSubst to all types. Apply Renaming to all
    % prog_vars.
    %
:- pred apply_renamings_and_subst_to_rtti_varmaps(tvar_renaming::in,
    tsubst::in, prog_var_renaming::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % rtti_varmaps_transform_types(Pred, !RttiVarMaps):
    %
    % Apply the transformation predicate to every type appearing in the
    % rtti_varmaps structure, including those in the arguments of constraints.
    %
:- pred rtti_varmaps_transform_types(
    pred(mer_type, mer_type)::in(pred(in, out) is det),
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % rtti_varmaps_overlay(A, B, C):
    %
    % Merge the information in rtti_varmaps A and B to produce C.
    % Where information conflicts, use the information in B rather than A.
    %
:- pred rtti_varmaps_overlay(rtti_varmaps::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

%---------------------------------------------------------------------------%
%
% Searches in rtti_varmaps.
%

    % Find the location of a type_info, if it is known.
    %
:- pred rtti_search_type_info_locn(rtti_varmaps::in, tvar::in,
    type_info_locn::out) is semidet.

    % Find the location of a type_info.
    %
:- pred rtti_lookup_type_info_locn(rtti_varmaps::in, tvar::in,
    type_info_locn::out) is det.

    % Find the prog_var which contains the typeclass_info for a given
    % constraint and which can be reused, if it is known.
    %
:- pred rtti_search_typeclass_info_var(rtti_varmaps::in, prog_constraint::in,
    prog_var::out) is semidet.

    % Find the prog_var which contains the typeclass_info for a given
    % constraint and which can be reused.
    %
:- pred rtti_lookup_typeclass_info_var(rtti_varmaps::in, prog_constraint::in,
    prog_var::out) is det.

%---------------------%

    % Find what RTTI, if any, is stored in a prog_var.
    %
:- pred rtti_varmaps_var_info(rtti_varmaps::in, prog_var::in,
    rtti_var_info::out) is det.

%---------------------------------------------------------------------------%
%
% Get global info from rtti_varmaps.
%

    % Returns all of the tvars that we have information about in the
    % rtti_varmaps structure.
    %
:- pred rtti_varmaps_tvars(rtti_varmaps::in, list(tvar)::out) is det.

    % Succeeds iff the rtti_varmaps contain no information about any
    % type variables.
    %
:- pred rtti_varmaps_no_tvars(rtti_varmaps::in) is semidet.

    % Returns all of the prog_vars which are known to contain a type_info
    % or typeclass_info.
    %
:-  pred rtti_varmaps_rtti_prog_vars(rtti_varmaps::in, list(prog_var)::out)
    is det.

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

%---------------------%

    % For a set of variables V, find all the type variables in the types
    % of the variables in V, and return set of typeinfo variables for
    % those type variables. (find all typeinfos for variables in V).
    %
    % This set of typeinfos is often needed in liveness computation
    % for accurate garbage collection - live variables need to have
    % their typeinfos stay live too.
    %
:- pred get_typeinfo_vars(var_table::in, rtti_varmaps::in,
    set_of_progvar::in, set_of_progvar::out) is det.

:- pred maybe_complete_with_typeinfo_vars(var_table::in, rtti_varmaps::in,
    bool::in, set_of_progvar::in, set_of_progvar::out) is det.

%---------------------%

:- type are_typeclass_records_complete
    --->    typeclass_records_are_not_complete
    ;       typeclass_records_are_complete.

:- pred check_whether_typeclass_records_are_complete(rtti_varmaps::in,
    are_typeclass_records_complete::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_proc.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.mode_top_functor.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_type_subst.

:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set_tree234.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

make_rtti_proc_label(ModuleInfo, PredId, ProcId) = ProcLabel :-
    module_info_get_name(ModuleInfo, ThisModule),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredFormArity = pred_info_pred_form_arity(PredInfo),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    proc_info_get_var_table(ProcInfo, ProcVarTable),
    proc_info_get_headvars(ProcInfo, ProcHeadVars),
    proc_info_get_argmodes(ProcInfo, ProcModes),
    proc_info_interface_determinism(ProcInfo, ProcDetism),
    modes_to_top_functor_modes(ModuleInfo, ProcModes, ArgTypes, ProcTopModes),
    PredIsImported =
        (if pred_info_is_imported(PredInfo) then yes else no),
    PredIsPseudoImp =
        (if pred_info_is_pseudo_imported(PredInfo) then yes else no),
    ProcIsExported =
        (if procedure_is_exported(ModuleInfo, PredInfo, ProcId)
            then yes else no),
    pred_info_get_origin(PredInfo, Origin),
    ProcHeadVarsWithNames = list.map(
        ( func(Var) = Var - Name :-
            Name = var_table_entry_name(ProcVarTable, Var)
        ), ProcHeadVars),
    ( if
        (
            PredIsImported = yes
        ;
            PredIsPseudoImp = yes,
            in_in_unification_proc_id(ProcId)
        )
    then
        ProcIsImported = yes
    else
        ProcIsImported = no
    ),
    ProcLabel = rtti_proc_label(PredOrFunc, ThisModule, PredModule,
        PredName, PredFormArity, ArgTypes, PredId, ProcId,
        ProcHeadVarsWithNames, ProcTopModes, ProcDetism,
        PredIsImported, PredIsPseudoImp, Origin,
        ProcIsExported, ProcIsImported).

proc_label_pred_proc_id(RttiProcLabel, PredId, ProcId) :-
    PredId = RttiProcLabel ^ rpl_pred_id,
    ProcId = RttiProcLabel ^ rpl_proc_id.

%---------------------------------------------------------------------------%

type_info_locn_var(type_info(Var), Var).
type_info_locn_var(typeclass_info(Var, _), Var).

type_info_locn_set_var(Var, type_info(_), type_info(Var)).
type_info_locn_set_var(Var, typeclass_info(_, Num), typeclass_info(Var, Num)).

%---------------------------------------------------------------------------%

:- type rtti_varmaps
    --->    rtti_varmaps(
                rv_tv_to_ti_locn_map        :: tvar_to_ti_locn_map,
                rv_ti_var_to_type_map       :: ti_var_to_type_map,
                rv_tci_constr_to_var_map    :: tci_constraint_to_var_map,
                rv_tci_var_to_constr_map    :: tci_var_to_constraint_map
            ).

    % A tci_constraint_to_var_map records, for each type class constraint,
    % which variable contains the typeclass_info for that constraint.
    % XXX This is a LIMITATION: on different branches, the same constraint
    % may need to be mapped to different variables.
    %
    % The constraints covered by this map are
    % - those which are passed in as head arguments, and
    % - those which are produced as existential constraints
    %   from calls or deconstructions.
    % These are constraints for which it is safe to reuse the variable
    % associated with the constraint.
    %
:- type tci_constraint_to_var_map == map(prog_constraint, prog_var).

    % A tvar_to_ti_locn_map is a map which, for each type variable,
    % records where the type_info for that type variable is stored.
    %
    % XXX This doesn't record the information that we want. For a constraint
    % such as foo(list(T)) we can't properly record the location of the
    % type_info for T, since it does not occupy a slot in the typeclass_info
    % directly, but is inside the type_info for list(T).
    %
    % XXX Even the information that is recorded has the wrong key. Consider
    % a conjunction between:
    %
    % - a call that returns an existentially typed result, and
    % - a goal that uses that existentially typed variable.
    %
    % Let us say that the conjunction looks like
    %
    %   gen_result(X_1, TypeInfo_for_T_2), use_result(TypeInfo_for_T_2, X_1).
    %
    % This conjunction can be duplicated, e.g. by switch detection (it could be
    % in a switch arm that is guarded by a disjunction that handles different
    % values of the switched-on variable differently) or by tabling.
    % (This is what happens in Mantis bug #154.)
    %
    % In such cases, the renamed-apart duplicated goal would be something like
    %
    %   gen_result(X_3, TypeInfo_for_T_4), use_result(TypeInfo_for_T_4, X_3).
    %
    % Yet the rtti_var_map for the procedure would say that X_3 is of the same
    % type as X_1, which means that the compiler would think that X_1 and X_3
    % use the same type_info variable to represent their types. This would be
    % TypeInfo_for_T_2, even though it won't exist on the execution branch
    % containing the copied version of the goal.
    %
    % I (zs) can see two possible fixes.
    %
    % - First, we could have tvar_to_ti_locn_map map each tvar to one_or_more
    %   type_info_locns, exactly one of which would be available on every
    %   execution path. It would be the responsibility of other parts of the
    %   compiler to pick the right one.
    %
    % - Second, instead of mapping prog_vars to types, and the tvars in those
    %   types to the prog_vars holding their type_infos, we could map each
    %   prog_var directly to a {tvar -> typeinfo progvar} map.
    %
:- type tvar_to_ti_locn_map == map(tvar, type_info_locn).

    % Every program variable which holds a type_info is a key in this map.
    % The value associated with a given key is the type that the type_info
    % is for.
    %
:- type ti_var_to_type_map == map(prog_var, mer_type).

    % Every program variable which holds a typeclass_info is a key in this map.
    % The value associated with a given key is the prog_constraint that
    % the typeclass_info is for.
    %
:- type tci_var_to_constraint_map == map(prog_var, prog_constraint).

%---------------------------------------------------------------------------%

rtti_varmaps_init(RttiVarMaps) :-
    map.init(TVarToLocnMap),
    map.init(TIVarToTypeMap),
    map.init(ConstraintToVarMap),
    map.init(VarToConstraintMap),
    RttiVarMaps = rtti_varmaps(TVarToLocnMap, TIVarToTypeMap,
        ConstraintToVarMap, VarToConstraintMap).

%---------------------------------------------------------------------------%

rtti_det_insert_type_info_locn(TVar, Locn, !RttiVarMaps) :-
    TVarToLocnMap0 = !.RttiVarMaps ^ rv_tv_to_ti_locn_map,
    map.det_insert(TVar, Locn, TVarToLocnMap0, TVarToLocnMap),
    !RttiVarMaps ^ rv_tv_to_ti_locn_map := TVarToLocnMap,
    maybe_check_type_info_var(Locn, TVar, !RttiVarMaps).

rtti_set_type_info_locn(TVar, Locn, !RttiVarMaps) :-
    TVarToLocnMap0 = !.RttiVarMaps ^ rv_tv_to_ti_locn_map,
    map.set(TVar, Locn, TVarToLocnMap0, TVarToLocnMap),
    !RttiVarMaps ^ rv_tv_to_ti_locn_map := TVarToLocnMap,
    maybe_check_type_info_var(Locn, TVar, !RttiVarMaps).

:- pred maybe_check_type_info_var(type_info_locn::in, tvar::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

maybe_check_type_info_var(Locn, TVar, RttiVarMaps, RttiVarMaps) :-
    (
        Locn = type_info(Var),
        % We do an unneeded return of RttiVarMaps to ensure that
        % calls to this predicate, and therefore this sanity check,
        % do not get optimized away.
        map.lookup(RttiVarMaps ^ rv_ti_var_to_type_map, Var, Type),
        ( if Type = type_variable(TVar, _) then
            true
        else
            unexpected($pred, "inconsistent info in rtti_varmaps")
        )
    ;
        Locn = typeclass_info(_, _)
    ).

%---------------------%

rtti_det_insert_type_info_type(ProgVar, Type, !RttiVarMaps) :-
    TIVarToTypeMap0 = !.RttiVarMaps ^ rv_ti_var_to_type_map,
    map.det_insert(ProgVar, Type, TIVarToTypeMap0, TIVarToTypeMap),
    !RttiVarMaps ^ rv_ti_var_to_type_map := TIVarToTypeMap.

rtti_set_type_info_type(ProgVar, Type, !RttiVarMaps) :-
    TIVarToTypeMap0 = !.RttiVarMaps ^ rv_ti_var_to_type_map,
    map.set(ProgVar, Type, TIVarToTypeMap0, TIVarToTypeMap),
    !RttiVarMaps ^ rv_ti_var_to_type_map := TIVarToTypeMap.

%---------------------%

rtti_det_insert_typeclass_info_var(Constraint, ProgVar, !RttiVarMaps) :-
    VarToConstraintMap0 = !.RttiVarMaps ^ rv_tci_var_to_constr_map,
    map.det_insert(ProgVar, Constraint,
        VarToConstraintMap0, VarToConstraintMap),
    !RttiVarMaps ^ rv_tci_var_to_constr_map := VarToConstraintMap.

rtti_set_typeclass_info_var(Constraint, ProgVar, !RttiVarMaps) :-
    VarToConstraintMap0 = !.RttiVarMaps ^ rv_tci_var_to_constr_map,
    % XXX This should call one_or_more_map.add.
    map.set(ProgVar, Constraint, VarToConstraintMap0, VarToConstraintMap),
    !RttiVarMaps ^ rv_tci_var_to_constr_map := VarToConstraintMap.

rtti_reuse_typeclass_info_var(ProgVar, !RttiVarMaps) :-
    map.lookup(!.RttiVarMaps ^ rv_tci_var_to_constr_map, ProgVar, Constraint),
    ConstraintToVarMap0 = !.RttiVarMaps ^ rv_tci_constr_to_var_map,
    % XXX This should call one_or_more_map.add.
    map.set(Constraint, ProgVar, ConstraintToVarMap0, ConstraintToVarMap),
    !RttiVarMaps ^ rv_tci_constr_to_var_map := ConstraintToVarMap.

%---------------------------------------------------------------------------%

rtti_var_info_duplicate(Var, NewVar, !RttiVarMaps) :-
    rtti_varmaps_var_info(!.RttiVarMaps, Var, VarInfo),
    (
        VarInfo = type_info_var(Type),
        rtti_det_insert_type_info_type(NewVar, Type, !RttiVarMaps)
    ;
        VarInfo = typeclass_info_var(Constraint),
        rtti_det_insert_typeclass_info_var(Constraint, NewVar, !RttiVarMaps)
    ;
        VarInfo = non_rtti_var
    ).

rtti_var_info_duplicate_replace(Var, NewVar, !RttiVarMaps) :-
    rtti_varmaps_var_info(!.RttiVarMaps, Var, VarInfo),
    (
        VarInfo = type_info_var(Type),
        rtti_set_type_info_type(NewVar, Type, !RttiVarMaps)
    ;
        VarInfo = typeclass_info_var(Constraint),
        rtti_set_typeclass_info_var(Constraint, NewVar, !RttiVarMaps)
    ;
        VarInfo = non_rtti_var
    ).

%---------------------------------------------------------------------------%

restrict_rtti_varmaps(VarUses, !RttiVarMaps) :-
    % This code makes the assumption that if a type_ctor_info, type_info,
    % base_typeclass_info or typeclass_info variable is not needed, then
    % any code that refers to the constraints reachable from those variables
    % has also been removed from the procedure. (This would happen by being
    % moved to a procedure of its own by expand_lambdas.m.)
    !.RttiVarMaps = rtti_varmaps(TVarToLocnMap0, TIVarToTypeMap0,
        ConstraintToVarMap0, VarToConstraintMap0),

    map.to_assoc_list(TVarToLocnMap0, TVarToLocnList0),
    filter_tvar_to_ti_locn_map(TVarToLocnList0, VarUses,
        [], RevTVarToLocnList),
    map.from_rev_sorted_assoc_list(RevTVarToLocnList, TVarToLocnMap),

    map.to_assoc_list(TIVarToTypeMap0, TIVarToTypeList0),
    filter_ti_var_to_type_map(TIVarToTypeList0, VarUses,
        [], RevTIVarToTypeList),
    map.from_rev_sorted_assoc_list(RevTIVarToTypeList, TIVarToTypeMap),

    map.to_assoc_list(VarToConstraintMap0, ConstraintList0),
    filter_constraint_map(ConstraintList0, VarUses, [], RevConstraintList,
        ConstraintToVarMap0, ConstraintToVarMap),
    map.from_rev_sorted_assoc_list(RevConstraintList, VarToConstraintMap),

    !:RttiVarMaps = rtti_varmaps(TVarToLocnMap, TIVarToTypeMap,
        ConstraintToVarMap, VarToConstraintMap).

:- pred filter_tvar_to_ti_locn_map(assoc_list(tvar, type_info_locn)::in,
    array(bool)::in,
    assoc_list(tvar, type_info_locn)::in,
    assoc_list(tvar, type_info_locn)::out) is det.

filter_tvar_to_ti_locn_map([], _VarUses, !RevTVarLocns).
filter_tvar_to_ti_locn_map([TVarLocn | TVarLocns], VarUses, !RevTVarLocns) :-
    TVarLocn = _TVar - Locn,
    ( Locn = type_info(Var)
    ; Locn = typeclass_info(Var, _)
    ),
    VarNum = var_to_int(Var),
    array.unsafe_lookup(VarUses, VarNum, Used),
    (
        Used = yes,
        !:RevTVarLocns = [TVarLocn | !.RevTVarLocns]
    ;
        Used = no
    ),
    filter_tvar_to_ti_locn_map(TVarLocns, VarUses, !RevTVarLocns).

:- pred filter_ti_var_to_type_map(assoc_list(prog_var, mer_type)::in,
    array(bool)::in,
    assoc_list(prog_var, mer_type)::in, assoc_list(prog_var, mer_type)::out)
    is det.

filter_ti_var_to_type_map([], _VarUses, !RevVarTypes).
filter_ti_var_to_type_map([VarType | VarTypes], VarUses, !RevVarTypes) :-
    VarType = Var - _Type,
    VarNum = var_to_int(Var),
    array.unsafe_lookup(VarUses, VarNum, Used),
    (
        Used = yes,
        !:RevVarTypes = [VarType | !.RevVarTypes]
    ;
        Used = no
    ),
    filter_ti_var_to_type_map(VarTypes, VarUses, !RevVarTypes).

:- pred filter_constraint_map(assoc_list(prog_var, prog_constraint)::in,
    array(bool)::in,
    assoc_list(prog_var, prog_constraint)::in,
    assoc_list(prog_var, prog_constraint)::out,
    tci_constraint_to_var_map::in, tci_constraint_to_var_map::out) is det.

filter_constraint_map([], _VarUses, !RevVarConstraints, !ConstraintToVarMap).
filter_constraint_map([VarConstraint | VarConstraints], VarUses,
        !RevVarConstraints, !ConstraintToVarMap) :-
    VarConstraint = Var - Constraint,
    VarNum = var_to_int(Var),
    array.unsafe_lookup(VarUses, VarNum, Used),
    (
        Used = yes,
        !:RevVarConstraints = [VarConstraint | !.RevVarConstraints]
    ;
        Used = no,
        map.delete(Constraint, !ConstraintToVarMap)
    ),
    filter_constraint_map(VarConstraints, VarUses,
        !RevVarConstraints, !ConstraintToVarMap).

%---------------------------------------------------------------------------%

apply_renamings_and_subst_to_rtti_varmaps(TRenaming, TSubst, Subst,
        !RttiVarMaps) :-
    ( if
        % Optimize the simple case.
        map.is_empty(Subst),
        map.is_empty(TSubst),
        map.is_empty(TRenaming)
    then
        true
    else
        !.RttiVarMaps = rtti_varmaps(TVarToLocnMap0, TIVarToTypeMap0,
            ConstraintToVarMap0, VarToConstraintMap0),
        map.foldl(apply_substs_to_tci_map(TRenaming, TSubst, Subst),
            ConstraintToVarMap0, map.init, ConstraintToVarMap),
        map.foldl(apply_substs_to_ti_map(TRenaming, TSubst, Subst),
            TVarToLocnMap0, map.init, TVarToLocnMap),
        map.foldl(apply_substs_to_type_map(TRenaming, TSubst, Subst),
            TIVarToTypeMap0, map.init, TIVarToTypeMap),
        map.foldl(apply_substs_to_constraint_map(TRenaming, TSubst, Subst),
            VarToConstraintMap0, map.init, VarToConstraintMap),
        !:RttiVarMaps = rtti_varmaps(TVarToLocnMap, TIVarToTypeMap,
            ConstraintToVarMap, VarToConstraintMap)
    ).

:- pred apply_subst_to_prog_var(prog_var_renaming::in,
    prog_var::in, prog_var::out) is det.

apply_subst_to_prog_var(Subst, Var0, Var) :-
    ( if map.search(Subst, Var0, Var1) then
        Var = Var1
    else
        Var = Var0
    ).

:- pred apply_substs_to_tci_map(tvar_renaming::in, tsubst::in,
    prog_var_renaming::in, prog_constraint::in, prog_var::in,
    tci_constraint_to_var_map::in, tci_constraint_to_var_map::out) is det.

apply_substs_to_tci_map(TRenaming, TSubst, Subst, Constraint0, Var0,
        !ConstraintToVarMap) :-
    apply_renaming_to_prog_constraint(TRenaming, Constraint0,
        Constraint1),
    apply_rec_subst_to_prog_constraint(TSubst, Constraint1, Constraint),
    apply_subst_to_prog_var(Subst, Var0, Var),
    % XXX This should call one_or_more_map.add.
    map.set(Constraint, Var, !ConstraintToVarMap).

    % Update a map entry from tvar to type_info_locn, using the type renaming
    % and substitution to rename tvars and a variable substitution to rename
    % vars. The type renaming is applied before the type substitution.
    %
    % If tvar maps to another type variable, we keep the new variable.
    % If it maps to a type, we remove it from the map.
    %
:- pred apply_substs_to_ti_map(tvar_renaming::in, tsubst::in,
    prog_var_renaming::in, tvar::in, type_info_locn::in,
    tvar_to_ti_locn_map::in, tvar_to_ti_locn_map::out) is det.

apply_substs_to_ti_map(TRenaming, TSubst, Subst, TVar, Locn, !Map) :-
    type_info_locn_var(Locn, Var),
    apply_subst_to_prog_var(Subst, Var, NewVar),
    type_info_locn_set_var(NewVar, Locn, NewLocn),
    apply_renaming_to_tvar(TRenaming, TVar, NewTVar1),
    % We don't use the correct kinds here, but that doesn't matter because
    % the resulting kind will be thrown away anyway.
    apply_rec_subst_to_tvar(map.init, TSubst, NewTVar1, NewType),
    (
        % If the tvar is still a variable, insert it into the map with the
        % new var.
        NewType = type_variable(NewTVar, _),
        % Don't abort if two old type variables map to the same new type
        % variable.
        map.set(NewTVar, NewLocn, !Map)
    ;
        ( NewType = builtin_type(_)
        ; NewType = defined_type(_, _, _)
        ; NewType = tuple_type(_, _)
        ; NewType = higher_order_type(_, _, _, _)
        ; NewType = kinded_type(_, _)
        )
    ).

:- pred apply_substs_to_type_map(tvar_renaming::in, tsubst::in,
    prog_var_renaming::in, prog_var::in, mer_type::in,
    ti_var_to_type_map::in, ti_var_to_type_map::out) is det.

apply_substs_to_type_map(TRenaming, TSubst, Subst, Var0, Type0, !Map) :-
    apply_renaming_to_type(TRenaming, Type0, Type1),
    apply_rec_subst_to_type(TSubst, Type1, Type),
    apply_subst_to_prog_var(Subst, Var0, Var),
    ( if map.search(!.Map, Var, ExistingType) then
        ( if Type = ExistingType then
            true
        else
            unexpected($pred,
                string.format("inconsistent type_infos: "
                    ++ " Type: %s ExistingType: %s",
                    [s(string(Type)), s(string(ExistingType))]))
        )
    else
        map.det_insert(Var, Type, !Map)
    ).

:- pred apply_substs_to_constraint_map(tvar_renaming::in, tsubst::in,
    prog_var_renaming::in, prog_var::in, prog_constraint::in,
    tci_var_to_constraint_map::in, tci_var_to_constraint_map::out) is det.

apply_substs_to_constraint_map(TRenaming, TSubst, Subst, Var0, Constraint0,
        !VarToConstraintMap) :-
    apply_renaming_to_prog_constraint(TRenaming, Constraint0, Constraint1),
    apply_rec_subst_to_prog_constraint(TSubst, Constraint1, Constraint),
    apply_subst_to_prog_var(Subst, Var0, Var),
    ( if map.search(!.VarToConstraintMap, Var, ExistingConstraint) then
        ( if Constraint = ExistingConstraint then
            true
        else
            unexpected($pred, "inconsistent typeclass_infos")
        )
    else
        map.det_insert(Var, Constraint, !VarToConstraintMap)
    ).

%---------------------------------------------------------------------------%

rtti_varmaps_transform_types(Pred, !RttiVarMaps) :-
    !.RttiVarMaps = rtti_varmaps(TVarToLocnMap0, TIVarToTypeMap0,
        ConstraintToVarMap0, VarToConstraintMap0),
    map.map_values_only(Pred, TIVarToTypeMap0, TIVarToTypeMap),
    map.foldl(apply_constraint_key_transformation(Pred), ConstraintToVarMap0,
        map.init, ConstraintToVarMap),
    map.map_values(apply_constraint_value_transformation(Pred),
        VarToConstraintMap0, VarToConstraintMap),
    !:RttiVarMaps = rtti_varmaps(TVarToLocnMap0, TIVarToTypeMap,
        ConstraintToVarMap, VarToConstraintMap).

:- pred apply_constraint_key_transformation(
    pred(mer_type, mer_type)::in(pred(in, out) is det),
    prog_constraint::in, prog_var::in,
    tci_constraint_to_var_map::in, tci_constraint_to_var_map::out) is det.

apply_constraint_key_transformation(Pred, Constraint0, Var,
        !ConstraintToVarMap) :-
    Constraint0 = constraint(Name, ArgTypes0),
    list.map(Pred, ArgTypes0, ArgTypes),
    Constraint = constraint(Name, ArgTypes),
    % XXX This should call one_or_more_map.add.
    map.set(Constraint, Var, !ConstraintToVarMap).

:- pred apply_constraint_value_transformation(
    pred(mer_type, mer_type)::in(pred(in, out) is det),
    prog_var::in, prog_constraint::in, prog_constraint::out) is det.

apply_constraint_value_transformation(Pred, _, Constraint0, Constraint) :-
    Constraint0 = constraint(Name, ArgTypes0),
    list.map(Pred, ArgTypes0, ArgTypes),
    Constraint = constraint(Name, ArgTypes).

%---------------------------------------------------------------------------%

rtti_varmaps_overlay(RttiVarMapsA, RttiVarMapsB, RttiVarMaps) :-
    RttiVarMapsA = rtti_varmaps(TVarToLocnMapA, TIVarToTypeMapA,
        ConstraintToVarMapA, VarToConstraintMapA),
    RttiVarMapsB = rtti_varmaps(TVarToLocnMapB, TIVarToTypeMapB,
        ConstraintToVarMapB, VarToConstraintMapB),

    % Prefer VarMapsB for this information.
    map.overlay(ConstraintToVarMapA, ConstraintToVarMapB, ConstraintToVarMap),
    map.overlay(TVarToLocnMapA, TVarToLocnMapB, TVarToLocnMap),

    % On the other hand, we insist that this information is consistent.
    map.old_merge(TIVarToTypeMapA, TIVarToTypeMapB, TIVarToTypeMap),
    map.old_merge(VarToConstraintMapA, VarToConstraintMapB,
        VarToConstraintMap),

    RttiVarMaps = rtti_varmaps(TVarToLocnMap, TIVarToTypeMap,
        ConstraintToVarMap, VarToConstraintMap).

%---------------------------------------------------------------------------%

rtti_search_type_info_locn(RttiVarMaps, TVar, Locn) :-
    map.search(RttiVarMaps ^ rv_tv_to_ti_locn_map, TVar, Locn).

rtti_lookup_type_info_locn(RttiVarMaps, TVar, Locn) :-
    map.lookup(RttiVarMaps ^ rv_tv_to_ti_locn_map, TVar, Locn).

rtti_search_typeclass_info_var(RttiVarMaps, Constraint, ProgVar) :-
    map.search(RttiVarMaps ^ rv_tci_constr_to_var_map, Constraint, ProgVar).

rtti_lookup_typeclass_info_var(RttiVarMaps, Constraint, ProgVar) :-
    map.lookup(RttiVarMaps ^ rv_tci_constr_to_var_map, Constraint, ProgVar).

rtti_varmaps_var_info(RttiVarMaps, Var, VarInfo) :-
    ( if
        map.search(RttiVarMaps ^ rv_ti_var_to_type_map, Var, Type)
    then
        VarInfo = type_info_var(Type)
    else if
        map.search(RttiVarMaps ^ rv_tci_var_to_constr_map, Var, Constraint)
    then
        VarInfo = typeclass_info_var(Constraint)
    else
        VarInfo = non_rtti_var
    ).

%---------------------------------------------------------------------------%

rtti_varmaps_tvars(RttiVarMaps, TVars) :-
    map.keys(RttiVarMaps ^ rv_tv_to_ti_locn_map, TVars).

rtti_varmaps_no_tvars(RttiVarMaps) :-
    map.is_empty(RttiVarMaps ^ rv_tv_to_ti_locn_map).

rtti_varmaps_rtti_prog_vars(RttiVarMaps, Vars) :-
    map.keys(RttiVarMaps ^ rv_ti_var_to_type_map, TIVars),
    map.keys(RttiVarMaps ^ rv_tci_var_to_constr_map, TCIVars),
    list.append(TIVars, TCIVars, Vars).

rtti_varmaps_types(RttiVarMaps, Types) :-
    TIVarToTypeMap = RttiVarMaps ^ rv_ti_var_to_type_map,
    VarToConstraintMap = RttiVarMaps ^ rv_tci_var_to_constr_map,
    TypeSet0 = set_tree234.init,
    map.foldl_values(set_tree234.insert, TIVarToTypeMap,
        TypeSet0, TypeSet1),
    map.foldl_values(accumulate_types_in_prog_constraint, VarToConstraintMap,
        TypeSet1, TypeSet),
    Types = set_tree234.to_sorted_list(TypeSet).

:- pred accumulate_types_in_prog_constraint(prog_constraint::in,
    set_tree234(mer_type)::in, set_tree234(mer_type)::out) is det.

accumulate_types_in_prog_constraint(Constraint, !TypeSet) :-
    Constraint = constraint(_, ArgTypes),
    set_tree234.insert_list(ArgTypes, !TypeSet).

rtti_varmaps_reusable_constraints(RttiVarMaps, Constraints) :-
    map.keys(RttiVarMaps ^ rv_tci_constr_to_var_map, Constraints).

%---------------------%

get_typeinfo_vars(VarTable, RttiVarMaps, Vars, TypeInfoVars) :-
    TVarToLocnMap = RttiVarMaps ^ rv_tv_to_ti_locn_map,
    VarList = set_of_var.to_sorted_list(Vars),
    get_typeinfo_vars_acc(VarTable, TVarToLocnMap, VarList,
        set_of_var.init, TypeInfoVars).

    % Auxiliary predicate - traverses variables and builds a list of
    % variables that store typeinfos for these variables.
    %
:- pred get_typeinfo_vars_acc(var_table::in, tvar_to_ti_locn_map::in,
    list(prog_var)::in, set_of_progvar::in, set_of_progvar::out) is det.

get_typeinfo_vars_acc(_, _, [], !TypeInfoVars).
get_typeinfo_vars_acc(VarTable, TVarToLocnMap, [Var | Vars], !TypeInfoVars) :-
    lookup_var_type(VarTable, Var, Type),
    type_vars_in_type(Type, TypeVars),
    (
        TypeVars = []
        % Optimize common case,
    ;
        TypeVars = [_ | _],
        % XXX It is possible there are some complications with higher order
        % pred types here -- if so, maybe treat them specially.
        % The type_info is either stored in a variable, or in a
        % typeclass_info. Either get the type_info variable or
        % the typeclass_info variable.
        LookupVar =
            ( pred(TVar::in, TVarVar::out) is det :-
                map.lookup(TVarToLocnMap, TVar, Locn),
                type_info_locn_var(Locn, TVarVar)
            ),
        list.map(LookupVar, TypeVars, TypeInfoVarsHead),
        set_of_var.insert_list(TypeInfoVarsHead, !TypeInfoVars)
    ),
    get_typeinfo_vars_acc(VarTable, TVarToLocnMap, Vars, !TypeInfoVars).

%---------------------%

maybe_complete_with_typeinfo_vars(VarTable, RttiVarMaps, TypeInfoLiveness,
        Vars0, Vars) :-
    (
        TypeInfoLiveness = yes,
        get_typeinfo_vars(VarTable, RttiVarMaps, Vars0, TypeInfoVars),
        set_of_var.union(Vars0, TypeInfoVars, Vars)
    ;
        TypeInfoLiveness = no,
        Vars = Vars0
    ).

%---------------------------------------------------------------------------%

check_whether_typeclass_records_are_complete(RttiVarMaps, MaybeComplete) :-
    RttiVarMaps = rtti_varmaps(_, _,
        ConstraintToVarMap0, VarToConstraintMap0),
    map.foldl(delete_forward_edge, ConstraintToVarMap0,
        VarToConstraintMap0, VarToConstraintMap),
    ( if map.is_empty(VarToConstraintMap) then
        MaybeComplete = typeclass_records_are_complete
    else
        trace [io(!IO)] (
            map.to_sorted_assoc_list(VarToConstraintMap0, VarToConstraintAL0),
            map.to_sorted_assoc_list(VarToConstraintMap, VarToConstraintAL),
            io.stderr_stream(StdErr, !IO),
            io.write_string(StdErr, "\nVarToConstraintAL0:\n", !IO),
            list.foldl(io.write_line(StdErr), VarToConstraintAL0, !IO),
            io.nl(StdErr, !IO),
            io.write_string(StdErr, "\nVarToConstraintAL:\n", !IO),
            list.foldl(io.write_line(StdErr), VarToConstraintAL, !IO),
            io.nl(StdErr, !IO)
        ),
        MaybeComplete = typeclass_records_are_not_complete
    ).

:- pred delete_forward_edge(prog_constraint::in, prog_var::in,
    tci_var_to_constraint_map::in, tci_var_to_constraint_map::out) is det.

delete_forward_edge(Constraint, Var, !VarToConstraintMap) :-
    % Var not occurring in !.VarToConstraintMap, and ...
    map.det_remove(Var, VarConstraint, !VarToConstraintMap),
    % ... the matching constraint not being Constraint, would both be bugs.
    expect(unify(Constraint, VarConstraint), $pred,
        "Constraint != VarConstraint").

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_rtti.
%---------------------------------------------------------------------------%
