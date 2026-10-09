%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2014-2015, 2021-2022, 2025-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: polymorphism_info.m.
% Main authors: fjh and zs (when this code was in polymorphism.m).
%
% This module defines the poly_info structure used by polymorphism.m.
%
%---------------------------------------------------------------------------%

:- module check_hlds.polymorphism_info.
:- interface.

:- import_module hlds.
:- import_module hlds.const_struct.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_proc.
:- import_module hlds.hlds_rtti.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type const_or_var_arg
    --->    cova_const(const_struct_arg)
    ;       cova_var(prog_var).

:- type var_and_maybe_csa
    --->    var_and_maybe_csa(
                prog_var,
                maybe(const_struct_arg)
            ).

:- func var_and_maybe_csa_to_var(var_and_maybe_csa) = prog_var.

%---------------------%

    % These two types together describe a two-stage map from a type
    % to the type_info describing that type. The two stages specify
    %
    % - the type_ctor of the type, and
    % - its argument types.
    %
    % The type_info will in general be a heap cell consisting of
    %
    % - a pointer to the statically allocated type_ctor_info for
    %   the type_ctor (which will contain the arity, amongs other things), and
    % - the type_info for each argument type.
    %
    % (The in-general part is there because for zero-arity type_ctors,
    % we do not allocate a heap cell.)
    %
    % The result of the two-stage map will be the program variable
    % holding the type_info, and, if the type contains no type variables,
    % then also a reference to the constant form of the type_info.
    %
:- type type_to_type_info_map ==
    map(type_ctor, type_ctor_to_type_info_map).
:- type type_ctor_to_type_info_map ==
    map(list(mer_type), var_and_maybe_csa).

%---------------------%

    % These three types together describe a three-stage map from a typeclass
    % constraint to the to the typeclass_info describing that type.
    % The first two stages specify
    %
    % - the class_id of the typeclass constraint, and
    % - its argument types.
    %
    % The third stage is needed because the general form of a typeclass_info
    % does NOT consist of just
    %
    % - a pointer to the statically allocated base_typeclass_info for
    %   the typeclass, and
    % - the type_info for each type,
    %
    % but also has two kinds of differences.
    %
    % The first difference is that a typeclass_info  also contains references
    % to other typeclass_infos (both from the context of the relevant
    % instance declaration, and for the applicable superclasses).
    %
    % Part of the second difference is that a typeclass_info contains
    % type_infos not for the argument types of the class, but for the types
    % occurring in the instance declaration, which may be different.
    % The rest of the second difference is that the place of these type_infos
    % in the typeclass_info depends on whether the type variables that
    % they represent are constrained or not.
    %
    % This is why the third stage map specifies exactly the second-and-later
    % arguments of the typeclass_info_cell_constructor in a typeclass info cell
    % (the base_typeclass_info arg is always the first). In general, some
    % of these arguments will be type_infos, and some will be typeclass_infos.
    % Each argument may be a constant, or a variable.
    %
    % The result of the three-stage map will be the program variable
    % holding the typeclass_info, and, if all the arguments are constants,
    % then also a reference to the constant form of the typeclass_info.
    %
    % XXX I (zs) am not sure whether the second stage of the three-stage map
    % is actually required (the first and third stages should suffice),
    % though I am pretty sure that including them made the initial
    % implementation of the relevant code much easier to debug.
    %
    % On the other hand, it may be that the key to the second stage
    % (the list of the class name's argument types) uniquely determines
    % the key to the third stage, making the third stage redundant.
    % (The code that computes the list of const_or_var_args is so long
    % that answering that question is not at all easy.)
    %
    % XXX We should consider replacing the key type of the tci_args_map
    % with a structure containing not one but FOUR lists of const_or_var_args.
    % At the moment, make_typeclass_info_from_proof_instance computes
    % the one list like this:
    %
    %   ArgVarsMCAs = ArgUnconstrainedTypeInfoVarsMCAs ++
    %       ArgTypeClassInfoVarsMCAs ++
    %       ArgSuperClassVarsMCAs ++ ArgTypeInfoVarsMCAs,
    %
    % Instead of ArgVarsMCAs, we could store its four components.
    % This should make it easier to debug any future changeover
    % to the design documented in runtime/mercury_typeclass_info.h.
    %
:- type class_to_typeclass_info_map ==
    map(class_id, class_id_to_typeclass_info_map).
:- type class_id_to_typeclass_info_map ==
    map(list(mer_type), tci_args_map).
:- type tci_args_map ==
    map(list(const_or_var_arg), var_and_maybe_csa).

%---------------------%

:- type int_const_map == map(int, prog_var).

%---------------------%

    % If the value that can be a constant structure argument is currently
    % available in a variable, give the id of that variable.
    %
:- type const_struct_var_map == map(const_struct_arg, prog_var).

%---------------------------------------------------------------------------%

:- type poly_info.

    % This predicate initializes a poly_info from a pred_info and clauses_info.
    % (See also create_poly_info.)
    %
:- pred init_poly_info(module_info::in, pred_info::in, clauses_info::in,
    poly_info::out) is det.

    % Extract some fields from a pred_info and proc_info, and use them
    % to create a poly_info, for use by the polymorphism transformation.
    %
:- pred create_poly_info(module_info::in, pred_info::in, proc_info::in,
    poly_info::out) is det.

    % Update the fields in a pred_info and proc_info with
    % the values in a poly_info.
    %
:- pred poly_info_extract(poly_info::in, list(diag_spec)::out,
    pred_info::in, pred_info::out, proc_info::in, proc_info::out,
    module_info::out) is det.

%---------------------------------------------------------------------------%

:- type maybe_must_requantify
    --->    no_must_requantify
    ;       must_requantify.

:- pred poly_info_get_module_info(poly_info::in,
    module_info::out) is det.
:- pred poly_info_get_var_table(poly_info::in,
    var_table::out) is det.
:- pred poly_info_get_rtti_varmaps(poly_info::in,
    rtti_varmaps::out) is det.
:- pred poly_info_get_typevarset(poly_info::in,
    tvarset::out) is det.
:- pred poly_info_get_tvar_kind_map(poly_info::in,
    tvar_kind_map::out) is det.
:- pred poly_info_get_proof_map(poly_info::in,
    constraint_proof_map::out) is det.
:- pred poly_info_get_constraint_map(poly_info::in,
    constraint_map::out) is det.
:- pred poly_info_get_type_to_type_info_map(poly_info::in,
    type_to_type_info_map::out) is det.
:- pred poly_info_get_class_to_typeclass_info_map(poly_info::in,
    class_to_typeclass_info_map::out) is det.
:- pred poly_info_get_int_const_map(poly_info::in,
    int_const_map::out) is det.
:- pred poly_info_get_const_struct_var_map(poly_info::in,
    const_struct_var_map::out) is det.
:- pred poly_info_get_num_reuses(poly_info::in,
    int::out) is det.
:- pred poly_info_get_const_struct_db(poly_info::in,
    const_struct_db::out) is det.
:- pred poly_info_get_defined_where(poly_info::in,
    defined_where::out) is det.
:- pred poly_info_get_must_requantify(poly_info::in,
    maybe_must_requantify::out) is det.
:- pred poly_info_get_errors(poly_info::in,
    list(diag_spec)::out) is det.

:- pred poly_info_set_var_table(var_table::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_var_table_rtti(var_table::in, rtti_varmaps::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_rtti_varmaps(rtti_varmaps::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_typevarset(tvarset::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_proof_map(constraint_proof_map::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_type_to_type_info_map(type_to_type_info_map::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_class_to_typeclass_info_map(
    class_to_typeclass_info_map::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_int_const_map(int_const_map::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_const_struct_var_map(const_struct_var_map::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_num_reuses(int::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_const_struct_db(const_struct_db::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_must_requantify(
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_errors(list(diag_spec)::in,
    poly_info::in, poly_info::out) is det.

%---------------------------------------------------------------------------%

:- type cache_maps_snapshot
    --->    cache_maps_snapshot(
                cm_snapshot_num                 :: int,
                cm_type_to_type_info_map        :: type_to_type_info_map,
                cm_class_to_typeclass_info_map  :: class_to_typeclass_info_map,
                cm_int_const_map                :: int_const_map,
                cm_const_struct_var_map         :: const_struct_var_map
            ).

:- pred get_cache_maps_snapshot(string::in, cache_maps_snapshot::out,
    poly_info::in, poly_info::out) is det.

:- pred set_cache_maps_snapshot(string::in, cache_maps_snapshot::in,
    poly_info::in, poly_info::out) is det.

:- pred empty_cache_maps(poly_info::in, poly_info::out) is det.

%---------------------------------------------------------------------------%

:- type var_maps_snapshot
    --->    var_maps_snapshot(
                vm_snapshot_num                 :: int,
                vm_var_table                    :: var_table,
                vm_rtti_varmaps                 :: rtti_varmaps,
                vm_cache_maps                   :: cache_maps_snapshot
            ).

:- pred get_var_maps_snapshot(string::in, var_maps_snapshot::out,
    poly_info::in, poly_info::out) is det.

:- pred set_var_maps_snapshot(string::in, var_maps_snapshot::in,
    poly_info::in, poly_info::out) is det.

%---------------------------------------------------------------------------%

:- pred get_poly_const(prog_context::in, int::in,
    prog_var::out, list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

:- pred all_are_const_struct_args(list(var_and_maybe_csa)::in,
    list(const_struct_arg)::out) is semidet.

:- pred get_inst_of_const_struct_arg(const_struct_db::in, const_struct_arg::in,
    mer_inst::out) is det.

%---------------------------------------------------------------------------%

:- type maybe_selected_pred
    --->    is_not_selected_pred
    ;       is_selected_pred.

:- pred poly_info_get_selected_pred(maybe_selected_pred::out,
    io::di, io::uo) is det.
:- pred poly_info_set_selected_pred(maybe_selected_pred::in,
    io::di, io::uo) is det.
:- pred poly_info_get_indent_level(int::out, io::di, io::uo) is det.
:- pred poly_info_set_indent_level(int::in, io::di, io::uo) is det.

:- pred poly_info_get_debug_stream(poly_info::in, io.text_output_stream::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Predicates that can help debug the polymorphism transformation.
%

:- pred write_cache_maps(io.text_output_stream::in, poly_info::in,
    string::in, io::di, io::uo) is det.

:- func var_and_maybe_csa_to_string(var_table, var_and_maybe_csa) = string.

:- func const_or_var_arg_to_string(tvarset, var_table, const_or_var_arg)
    = string.

:- func const_struct_arg_to_string(tvarset, const_struct_arg) = string.

:- func trace_constraint_to_string(tvarset, prog_constraint) = string.

:- func trace_type_to_string(tvarset, mer_type) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.make_goal.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module deconstruct.
:- import_module int.
:- import_module pretty_printer.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%

var_and_maybe_csa_to_var(var_and_maybe_csa(V, _)) = V.

%---------------------------------------------------------------------------%

:- type poly_info
    --->    poly_info(
                poly_module_info            :: module_info,

                poly_var_table              :: var_table,
                poly_rtti_varmaps           :: rtti_varmaps,

                poly_typevarset             :: tvarset,
                poly_tvar_kind_map          :: tvar_kind_map,

                % Specifies why each constraint that was eliminated from the
                % pred was able to be eliminated (this allows us to efficiently
                % construct the dictionary).
                % Note that the rtti_varmaps is separate from the
                % constraint_proof_map, since the second is the information
                % calculated during typechecking, while the first is
                % the information calculated in the polymorphism pass.
                poly_proof_map              :: constraint_proof_map,

                % Specifies the constraints at each location in the goal.
                poly_constraint_map         :: constraint_map,

                % The next four maps hold information about what
                % type_ctor_infos, type_infos, base_typeclass_infos,
                % typeclass_infos and ints are guaranteed to be available
                % (i.e. created by previous code on all execution paths)
                % at the current point in the code, so they can be reused.
                % The fifth field counts the number of times that one of these
                % variables has in fact been reused.
                %
                % The type_infos and typeclass_infos are in the first two maps.
                % The type_ctor_infos and base_typeclass_infos are in the
                % fourth map. The integers are in the third map.
                % The fourth map also caches typeclass_infos for instance ids.
                poly_type_to_type_info_map  :: type_to_type_info_map,
                poly_class_to_typeclass_info_map:: class_to_typeclass_info_map,
                poly_int_const_map          :: int_const_map,
                poly_const_struct_var_map   :: const_struct_var_map,
                poly_num_reuses             :: int,

                poly_snapshot_num           :: int,

                % The database of constant structures of the module.
                % If a type_info or typeclass_info we construct is a constant
                % term, we allocate it in this database.
                poly_const_struct_db        :: const_struct_db,

                poly_defined_where          :: defined_where,

                poly_must_requantify        :: maybe_must_requantify,

                % The list of errors we have discovered during the polymorphism
                % pass.
                poly_errors                 :: list(diag_spec)
            ).

%---------------------------------------------------------------------------%

init_poly_info(ModuleInfo, PredInfo, ClausesInfo, PolyInfo) :-
    clauses_info_get_var_table(ClausesInfo, VarTable),
    pred_info_get_typevarset(PredInfo, TypeVarSet),
    pred_info_get_tvar_kind_map(PredInfo, TypeVarKinds),
    pred_info_get_constraint_proof_map(PredInfo, ProofMap),
    pred_info_get_constraint_map(PredInfo, ConstraintMap),
    rtti_varmaps_init(RttiVarMaps),
    map.init(TypeInfoVarMap),
    map.init(TypeClassInfoMap),
    map.init(IntConstMap),
    map.init(ConstStructVarMap),
    NumReuses = 0,
    SnapshotNum = 0,
    module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
    pred_info_get_status(PredInfo, PredStatus),
    pred_status_defined_in_this_module(PredStatus) = InThisModule,
    ( InThisModule = yes, DefinedWhere = defined_in_this_module
    ; InThisModule = no,  DefinedWhere = defined_in_other_module
    ),
    Requant = no_must_requantify,
    Specs = [],
    PolyInfo = poly_info(ModuleInfo, VarTable, RttiVarMaps,
        TypeVarSet, TypeVarKinds, ProofMap, ConstraintMap,
        TypeInfoVarMap, TypeClassInfoMap, IntConstMap, ConstStructVarMap,
        NumReuses, SnapshotNum, ConstStructDb, DefinedWhere, Requant, Specs).

create_poly_info(ModuleInfo, PredInfo, ProcInfo, PolyInfo) :-
    pred_info_get_typevarset(PredInfo, TypeVarSet),
    pred_info_get_tvar_kind_map(PredInfo, TypeVarKinds),
    pred_info_get_constraint_proof_map(PredInfo, ProofMap),
    pred_info_get_constraint_map(PredInfo, ConstraintMap),
    proc_info_get_var_table(ProcInfo, VarTable),
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    map.init(TypeInfoVarMap),
    map.init(TypeClassInfoMap),
    map.init(IntConstMap),
    map.init(ConstStructVarMap),
    NumReuses = 0,
    SnapshotNum = 0,
    module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
    pred_info_get_status(PredInfo, PredStatus),
    pred_status_defined_in_this_module(PredStatus) = InThisModule,
    ( InThisModule = yes, DefinedWhere = defined_in_this_module
    ; InThisModule = no,  DefinedWhere = defined_in_other_module
    ),
    Requant = no_must_requantify,
    Specs = [],
    PolyInfo = poly_info(ModuleInfo, VarTable, RttiVarMaps,
        TypeVarSet, TypeVarKinds, ProofMap, ConstraintMap,
        TypeInfoVarMap, TypeClassInfoMap, IntConstMap, ConstStructVarMap,
        NumReuses, SnapshotNum, ConstStructDb, DefinedWhere, Requant, Specs).

poly_info_extract(Info, Specs, !PredInfo, !ProcInfo, !:ModuleInfo) :-
    Info = poly_info(!:ModuleInfo, VarTable, RttiVarMaps,
        TypeVarSet, TypeVarKinds, _ProofMap, _ConstraintMap,
        _TypeInfoVarMap, _TypeClassInfoMap, _IntConstMap, _ConstStructVarMap,
        _NumReuses, _SnapshotNum, ConstStructDb, _DefinedWhere,
        _Requant, Specs),

    module_info_set_const_struct_db(ConstStructDb, !ModuleInfo),

    % Set the new values of the fields in proc_info and pred_info.
    proc_info_set_var_table(VarTable, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),
    pred_info_set_typevarset(TypeVarSet, !PredInfo),
    pred_info_set_tvar_kind_map(TypeVarKinds, !PredInfo).

%---------------------------------------------------------------------------%

:- pragma inline(pred(poly_info_get_module_info/2)).
:- pragma inline(pred(poly_info_get_var_table/2)).
:- pragma inline(pred(poly_info_get_rtti_varmaps/2)).
:- pragma inline(pred(poly_info_get_typevarset/2)).
:- pragma inline(pred(poly_info_get_tvar_kind_map/2)).
:- pragma inline(pred(poly_info_get_proof_map/2)).
:- pragma inline(pred(poly_info_get_constraint_map/2)).
:- pragma inline(pred(poly_info_get_type_to_type_info_map/2)).
:- pragma inline(pred(poly_info_get_class_to_typeclass_info_map/2)).
:- pragma inline(pred(poly_info_get_const_struct_var_map/2)).
:- pragma inline(pred(poly_info_get_int_const_map/2)).
:- pragma inline(pred(poly_info_get_num_reuses/2)).
:- pragma inline(pred(poly_info_get_const_struct_db/2)).
:- pragma inline(pred(poly_info_get_errors/2)).

poly_info_get_module_info(PI, X) :-
    X = PI ^ poly_module_info.
poly_info_get_var_table(PI, X) :-
    X = PI ^ poly_var_table.
poly_info_get_rtti_varmaps(PI, X) :-
    X = PI ^ poly_rtti_varmaps.
poly_info_get_typevarset(PI, X) :-
    X = PI ^ poly_typevarset.
poly_info_get_tvar_kind_map(PI, X) :-
    X = PI ^ poly_tvar_kind_map.
poly_info_get_proof_map(PI, X) :-
    X = PI ^ poly_proof_map.
poly_info_get_constraint_map(PI, X) :-
    X = PI ^ poly_constraint_map.
poly_info_get_type_to_type_info_map(PI, X) :-
    X = PI ^ poly_type_to_type_info_map.
poly_info_get_class_to_typeclass_info_map(PI, X) :-
    X = PI ^ poly_class_to_typeclass_info_map.
poly_info_get_int_const_map(PI, X) :-
    X = PI ^ poly_int_const_map.
poly_info_get_const_struct_var_map(PI, X) :-
    X = PI ^ poly_const_struct_var_map.
poly_info_get_num_reuses(PI, X) :-
    X = PI ^ poly_num_reuses.
poly_info_get_const_struct_db(PI, X) :-
    X = PI ^ poly_const_struct_db.
poly_info_get_defined_where(PI, X) :-
    X = PI ^ poly_defined_where.
poly_info_get_must_requantify(PI, X) :-
    X = PI ^ poly_must_requantify.
poly_info_get_errors(PI, X) :-
    X = PI ^ poly_errors.

:- pragma inline(pred(poly_info_set_var_table/3)).
:- pragma inline(pred(poly_info_set_var_table_rtti/4)).
:- pragma inline(pred(poly_info_set_rtti_varmaps/3)).
:- pragma inline(pred(poly_info_set_typevarset/3)).
:- pragma inline(pred(poly_info_set_proof_map/3)).
:- pragma inline(pred(poly_info_set_type_to_type_info_map/3)).
:- pragma inline(pred(poly_info_set_class_to_typeclass_info_map/3)).
:- pragma inline(pred(poly_info_set_int_const_map/3)).
:- pragma inline(pred(poly_info_set_const_struct_var_map/3)).
:- pragma inline(pred(poly_info_set_num_reuses/3)).
:- pragma inline(pred(poly_info_set_const_struct_db/3)).
:- pragma inline(pred(poly_info_set_errors/3)).

poly_info_set_var_table(X, !PI) :-
    !PI ^ poly_var_table := X.
poly_info_set_var_table_rtti(X, Y, !PI) :-
    !:PI = ((!.PI
        ^ poly_var_table := X)
        ^ poly_rtti_varmaps := Y).
poly_info_set_rtti_varmaps(X, !PI) :-
    ( if private_builtin.pointer_equal(X, !.PI ^ poly_rtti_varmaps) then
        true
    else
        !PI ^ poly_rtti_varmaps := X
    ).
poly_info_set_typevarset(X, !PI) :-
    !PI ^ poly_typevarset := X.
poly_info_set_proof_map(X, !PI) :-
    ( if private_builtin.pointer_equal(X, !.PI ^ poly_proof_map) then
        true
    else
        !PI ^ poly_proof_map := X
    ).
poly_info_set_type_to_type_info_map(X, !PI) :-
    ( if
        private_builtin.pointer_equal(X, !.PI ^ poly_type_to_type_info_map)
    then
        true
    else
        !PI ^ poly_type_to_type_info_map := X
    ).
poly_info_set_class_to_typeclass_info_map(X, !PI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.PI ^ poly_class_to_typeclass_info_map)
    then
        true
    else
        !PI ^ poly_class_to_typeclass_info_map := X
    ).
poly_info_set_int_const_map(X, !PI) :-
    ( if private_builtin.pointer_equal(X, !.PI ^ poly_int_const_map) then
        true
    else
        !PI ^ poly_int_const_map := X
    ).
poly_info_set_const_struct_var_map(X, !PI) :-
    ( if
        private_builtin.pointer_equal(X, !.PI ^ poly_const_struct_var_map)
    then
        true
    else
        !PI ^ poly_const_struct_var_map := X
    ).
poly_info_set_num_reuses(X, !PI) :-
    ( if X = !.PI ^ poly_num_reuses then
        true
    else
        !PI ^ poly_num_reuses := X
    ).
poly_info_set_const_struct_db(X, !PI) :-
    ( if private_builtin.pointer_equal(X, !.PI ^ poly_const_struct_db) then
        true
    else
        !PI ^ poly_const_struct_db := X
    ).
poly_info_set_must_requantify(!PI) :-
    !PI ^ poly_must_requantify := must_requantify.
poly_info_set_errors(X, !PI) :-
    !PI ^ poly_errors := X.

%  i      read      same      diff   same%
%  0   6245285         0   1560789   0.000% varset
%  1   6662703         0         0          vartypes
%  2         0      1110    129008   0.853% varset, vartypes
% 17         0    131468   1961967   6.280% varset, vartypes, rtti_varmaps
%  3   3052707         4    245972   0.002% typevarset
%  4   1578929         0         0          tvar_kind_map
%  5   8959328   2116731    988195  68.173% rtti_varmaps
%  6     14812      3980      4058  49.515% proof_map
%  7   3030093         0         0          constraint_map
%  8    811687    776589    288951  72.882% type_to_type_info_map
%  9    385071    863384      6104  99.298% class_to_typeclass_info_map
% 10    385706    863310      8464  99.029% int_const_map
% 11    253310    331092     41528  88.855% num_reuses
% 12   2559364     25821     15631  62.291% const_struct_db
% 13    583633    780835    286464  73.160% const_struct_var_map
% 14         0         0         0          pred_info
% 15   3933469         0         0          module_info
% 16         0    431065     87104  83.190% cache_maps_snapshot

% :- pragma foreign_decl("C", local,
% "
% #define MR_NUM_INFO_STATS    18
% unsigned long MR_stats_read[MR_NUM_INFO_STATS];
% unsigned long MR_stats_same[MR_NUM_INFO_STATS];
% unsigned long MR_stats_diff[MR_NUM_INFO_STATS];
% ").
%
% :- pred gather_poly_info_read_stats(int::in,
%     poly_info::in, poly_info::out) is det.
%
% :- pragma foreign_proc("C",
%     gather_poly_info_read_stats(N::in, PI0::in, PI::out),
%     [will_not_call_mercury, promise_pure],
% "
%     ++MR_stats_read[N];
%     PI = PI0;
% ").
%
% :- pred gather_poly_info_write_stats(int::in, T::in, T::in,
%     poly_info::in, poly_info::out) is det.
%
% :- pragma foreign_proc("C",
%     gather_poly_info_write_stats(N::in, Old::in, New::in,
%         PI0::in, PI::out),
%     [will_not_call_mercury, promise_pure],
% "
%     if (((MR_Unsigned) Old) == ((MR_Unsigned) New)) {
%         ++MR_stats_same[N];
%     } else {
%         ++MR_stats_diff[N];
%     }
%
%     PI = PI0;
% ").
%
% :- pred gather_poly_info_write_stats_2(int::in, T::in, T::in, U::in, U::in,
%     poly_info::in, poly_info::out) is det.
%
% :- pragma foreign_proc("C",
%     gather_poly_info_write_stats_2(N::in, OldA::in, NewA::in,
%         OldB::in, NewB::in, PI0::in, PI::out),
%     [will_not_call_mercury, promise_pure],
% "
%     if ((((MR_Unsigned) OldA) == ((MR_Unsigned) NewA)) &&
%         (((MR_Unsigned) OldB) == ((MR_Unsigned) NewB)))
%     {
%         ++MR_stats_same[N];
%     } else {
%         ++MR_stats_diff[N];
%     }
%
%     PI = PI0;
% ").
%
% :- pred gather_poly_info_write_stats_3(int::in, T::in, T::in, U::in, U::in,
%     V::in, V::in, poly_info::in, poly_info::out) is det.
%
% :- pragma foreign_proc("C",
%     gather_poly_info_write_stats_3(N::in, OldA::in, NewA::in,
%         OldB::in, NewB::in, OldC::in, NewC::in, PI0::in, PI::out),
%     [will_not_call_mercury, promise_pure],
% "
%     if ((((MR_Unsigned) OldA) == ((MR_Unsigned) NewA)) &&
%         (((MR_Unsigned) OldB) == ((MR_Unsigned) NewB)) &&
%         (((MR_Unsigned) OldC) == ((MR_Unsigned) NewC)))
%     {
%         ++MR_stats_same[N];
%     } else {
%         ++MR_stats_diff[N];
%     }
%
%     PI = PI0;
% ").
%
% :- interface.
% :- import_module io.
% :- pred write_poly_info_stats(io::di, io::uo) is det.
% :- implementation.
%
% :- pragma foreign_proc("C",
%     write_poly_info_stats(IO0::di, IO::uo),
%     [will_not_call_mercury, promise_pure],
% "
%     FILE *fp;
%
%     fp = fopen(""/tmp/POLY_INFO_STATS"", ""a"");
%     if (fp != NULL) {
%         int i;
%         for (i = 0; i < MR_NUM_INFO_STATS; i++) {
%             fprintf(fp, ""stat_rsd %d %lu %lu %lu\\n"",
%                 i, MR_stats_read[i], MR_stats_same[i], MR_stats_diff[i]);
%         }
%     }
%
%     IO = IO0;
% ").

%---------------------------------------------------------------------------%

get_cache_maps_snapshot(Name, CacheMaps, !Info) :-
    poly_info_get_type_to_type_info_map(!.Info, TypeInfoVarMap),
    poly_info_get_class_to_typeclass_info_map(!.Info, TypeClassInfoMap),
    poly_info_get_int_const_map(!.Info, IntConstMap),
    poly_info_get_const_struct_var_map(!.Info, ConstStructVarMap),

    SnapshotNum = !.Info ^ poly_snapshot_num,
    CacheMaps = cache_maps_snapshot(SnapshotNum, TypeInfoVarMap,
        TypeClassInfoMap, IntConstMap, ConstStructVarMap),
    !Info ^ poly_snapshot_num := SnapshotNum + 1,

    trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
        get_selected_pred(SelectedPred, !IO),
        get_indent_level(Level, !IO),
        ( if
            SelectedPred = is_selected_pred,
            Name \= ""
        then
            poly_info_get_debug_stream(!.Info, Stream, !IO),
            IndentStr = string.duplicate_char(' ', Level * 4),
            poly_info_get_var_table(!.Info, VarTable),
            var_table_count(VarTable, NumVars),
            io.format(Stream, "%sget_cache_maps_snapshot %d %s\n",
                [s(IndentStr), i(SnapshotNum), s(Name)], !IO),
            io.format(Stream, "%snum_allocated vars: %d\n\n",
                [s(IndentStr), i(NumVars)], !IO)
        else
            true
        )
    ).

set_cache_maps_snapshot(Name, CacheMaps, !Info) :-
    CacheMaps = cache_maps_snapshot(SnapshotNum, TypeInfoVarMap,
        TypeClassInfoMap, IntConstMap, ConstStructVarMap),
    ( if
        private_builtin.pointer_equal(TypeInfoVarMap,
            !.Info ^ poly_type_to_type_info_map),
        private_builtin.pointer_equal(TypeClassInfoMap,
            !.Info ^ poly_class_to_typeclass_info_map),
        private_builtin.pointer_equal(IntConstMap,
            !.Info ^ poly_int_const_map),
        private_builtin.pointer_equal(ConstStructVarMap,
            !.Info ^ poly_const_struct_var_map)
    then
        true
    else
        !:Info = ((((!.Info
            ^ poly_type_to_type_info_map := TypeInfoVarMap)
            ^ poly_class_to_typeclass_info_map := TypeClassInfoMap)
            ^ poly_int_const_map := IntConstMap)
            ^ poly_const_struct_var_map := ConstStructVarMap)
    ),

    trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
        get_selected_pred(SelectedPred, !IO),
        get_indent_level(Level, !IO),
        ( if
            SelectedPred = is_selected_pred,
            Name \= ""
        then
            poly_info_get_debug_stream(!.Info, Stream, !IO),
            IndentStr = string.duplicate_char(' ', Level * 4),
            poly_info_get_var_table(!.Info, VarTable),
            var_table_count(VarTable, NumVars),

            io.format(Stream, "%sset_cache_maps_snapshot %d %s\n",
                [s(IndentStr), i(SnapshotNum), s(Name)], !IO),
            io.format(Stream, "%snum_allocated vars: %d\n\n",
                [s(IndentStr), i(NumVars)], !IO),
            write_cache_maps(Stream, !.Info, IndentStr, !IO),
            io.nl(Stream, !IO)
        else
            true
        )
    ).

empty_cache_maps(!Info) :-
    poly_info_set_type_to_type_info_map(map.init, !Info),
    poly_info_set_class_to_typeclass_info_map(map.init, !Info),
    poly_info_set_int_const_map(map.init, !Info),
    poly_info_set_const_struct_var_map(map.init, !Info).

%---------------------------------------------------------------------------%

get_var_maps_snapshot(Name, VarMaps, !Info) :-
    SnapshotNum = !.Info ^ poly_snapshot_num,
    poly_info_get_var_table(!.Info, VarTable),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps),

    trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
        get_selected_pred(SelectedPred, !IO),
        get_indent_level(Level, !IO),
        (
            SelectedPred = is_not_selected_pred
        ;
            SelectedPred = is_selected_pred,
            poly_info_get_debug_stream(!.Info, Stream, !IO),
            IndentStr = string.duplicate_char(' ', Level * 4),
            var_table_count(VarTable, NumVars),
            io.format(Stream, "%sget_var_maps_snapshot %d %s\n",
                [s(IndentStr), i(SnapshotNum), s(Name)], !IO),
            io.format(Stream, "%snum_allocated vars: %d\n\n",
                [s(IndentStr), i(NumVars)], !IO)
        )
    ),

    get_cache_maps_snapshot("", CacheMaps, !Info),
    VarMaps = var_maps_snapshot(SnapshotNum, VarTable, RttiVarMaps, CacheMaps).

set_var_maps_snapshot(Name, VarMaps, !Info) :-
    VarMaps = var_maps_snapshot(SnapshotNum, VarTable, RttiVarMaps, CacheMaps),

    trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
        get_selected_pred(SelectedPred, !IO),
        get_indent_level(Level, !IO),
        (
            SelectedPred = is_not_selected_pred
        ;
            SelectedPred = is_selected_pred,
            poly_info_get_debug_stream(!.Info, Stream, !IO),
            IndentStr = string.duplicate_char(' ', Level * 4),
            io.format(Stream, "%sset_var_maps_snapshot %d %s\n",
                [s(IndentStr), i(SnapshotNum), s(Name)], !IO),
            write_cache_maps(Stream, !.Info, IndentStr, !IO),
            io.nl(Stream, !IO)
        )
    ),

    poly_info_set_var_table_rtti(VarTable, RttiVarMaps, !Info),
    set_cache_maps_snapshot("", CacheMaps, !Info).

%---------------------------------------------------------------------------%

get_poly_const(Context, IntConst, IntVar, Goals, !Info) :-
    poly_info_get_int_const_map(!.Info, IntConstMap0),
    ( if map.search(IntConstMap0, IntConst, IntVarPrime) then
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 1, !Info),
        IntVar = IntVarPrime,
        Goals = []
    else
        Name = "PolyConst" ++ string.int_to_string(IntConst),
        poly_info_get_var_table(!.Info, VarTable0),
        make_int_const_construction_alloc(Context, IntConst, Name,
            Goal, IntVar, VarTable0, VarTable),
        poly_info_set_var_table(VarTable, !Info),
        map.det_insert(IntConst, IntVar, IntConstMap0, IntConstMap),
        poly_info_set_int_const_map(IntConstMap, !Info),
        Goals = [Goal]
    ).

all_are_const_struct_args([], []).
all_are_const_struct_args([VarMCA | VarsMCAs], [ConstArg | ConstArgs]) :-
    VarMCA = var_and_maybe_csa(_Var, MCA),
    MCA = yes(ConstArg),
    all_are_const_struct_args(VarsMCAs, ConstArgs).

get_inst_of_const_struct_arg(ConstStructDb, ConstArg, Inst) :-
    (
        ConstArg = csa_constant(ConsId, _),
        Inst = bound(shared, inst_test_results_fgtc,
            [bound_functor(ConsId, [])])
    ;
        ConstArg = csa_const_struct(StructNum),
        lookup_const_struct_num(ConstStructDb, StructNum, Struct),
        Struct = const_struct(_, _, _, Inst, _)
    ).

%---------------------------------------------------------------------------%

:- mutable(selected_pred, maybe_selected_pred, is_not_selected_pred, ground,
    [untrailed, attach_to_io_state]).
:- mutable(indent_level, int, 0, ground,
    [untrailed, attach_to_io_state]).

poly_info_get_selected_pred(Selected, !IO) :-
    get_selected_pred(Selected, !IO).

poly_info_set_selected_pred(Selected, !IO) :-
    set_selected_pred(Selected, !IO).

poly_info_get_indent_level(Level, !IO) :-
    get_indent_level(Level, !IO).

poly_info_set_indent_level(Level, !IO) :-
    set_indent_level(Level, !IO).

poly_info_get_debug_stream(PolyInfo, Stream, !IO) :-
    poly_info_get_module_info(PolyInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    module_info_get_name(ModuleInfo, ModuleName),
    get_debug_output_stream(Globals, ModuleName, Stream, !IO).

%---------------------------------------------------------------------------%

write_cache_maps(Stream, Info, IndentStr, !IO) :-
    write_type_to_type_info_map(Stream, Info, IndentStr, !IO),
    write_class_to_typeclass_info_map(Stream, Info, IndentStr, !IO),
    write_constr_struct_var_map(Stream, Info, IndentStr, !IO).

%---------------------------------------------------------------------------%

:- pred write_type_to_type_info_map(io.text_output_stream::in,
    poly_info::in, string::in, io::di, io::uo) is det.

write_type_to_type_info_map(Stream, Info, IndentStr, !IO) :-
    poly_info_get_type_to_type_info_map(Info, TypeInfoVarMap),
    poly_info_get_typevarset(Info, TVarSet),
    poly_info_get_var_table(Info, VarTable),
    io.format(Stream, "%stype_to_type_info_map\n", [s(IndentStr)], !IO),
    NextIndentStr = IndentStr ++ "    ",
    map.foldl(
        write_type_ctor_to_type_info_map(Stream, TVarSet, VarTable,
            NextIndentStr),
        TypeInfoVarMap, !IO).

:- pred write_type_ctor_to_type_info_map(io.text_output_stream::in,
    tvarset::in, var_table::in, string::in, type_ctor::in,
    type_ctor_to_type_info_map::in, io::di, io::uo) is det.

write_type_ctor_to_type_info_map(Stream, TVarSet, VarTable, IndentStr,
        TypeCtor, TypeInfoVarMapEntry, !IO) :-
    TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
    TypeCtorName = unqualify_name(TypeCtorSymName),
    io.format(Stream, "%s%s/%d:\n",
        [s(IndentStr), s(TypeCtorName), i(TypeCtorArity)], !IO),
    NextIndentStr = IndentStr ++ "    ",
    map.foldl(
        write_type_ctor_to_type_info_map_entry(Stream, TVarSet, VarTable,
            NextIndentStr),
        TypeInfoVarMapEntry, !IO).

:- pred write_type_ctor_to_type_info_map_entry(io.text_output_stream::in,
    tvarset::in, var_table::in, string::in,
    list(mer_type)::in, var_and_maybe_csa::in, io::di, io::uo) is det.

write_type_ctor_to_type_info_map_entry(Stream, TVarSet, VarTable, IndentStr,
        Types, VarMaybeCSA, !IO) :-
    TypeStrs = list.map(trace_type_to_string(TVarSet), Types),
    TypesStr = string.join_list(", ", TypeStrs),
    VarCSAStr = var_and_maybe_csa_to_string(VarTable, VarMaybeCSA),
    io.format(Stream, "%s[%s] -> %s\n",
        [s(IndentStr), s(TypesStr), s(VarCSAStr)], !IO).

%---------------------------------------------------------------------------%

:- pred write_class_to_typeclass_info_map(io.text_output_stream::in,
    poly_info::in, string::in, io::di, io::uo) is det.

write_class_to_typeclass_info_map(Stream, Info, IndentStr, !IO) :-
    poly_info_get_class_to_typeclass_info_map(Info, TypeClassInfoMap),
    poly_info_get_typevarset(Info, TVarSet),
    poly_info_get_var_table(Info, VarTable),
    io.format(Stream, "%sclass_to_typeclass_info_map\n", [s(IndentStr)], !IO),
    NextIndentStr = IndentStr ++ "    ",
    map.foldl(
        write_typeclass_info_top_map_entry(Stream, TVarSet, VarTable,
            NextIndentStr),
        TypeClassInfoMap, !IO).

:- pred write_typeclass_info_top_map_entry(io.text_output_stream::in,
    tvarset::in, var_table::in, string::in,
    class_id::in, class_id_to_typeclass_info_map::in, io::di, io::uo) is det.

write_typeclass_info_top_map_entry(Stream, TVarSet, VarTable, IndentStr,
        ClassId, TypeClassInfoClassMap, !IO) :-
    ClassId = class_id(ClassSymName, ClassArity),
    ClassName = unqualify_name(ClassSymName),
    io.format(Stream, "%sclass id %s/%d\n",
        [s(IndentStr), s(ClassName), i(ClassArity)], !IO),
    NextIndentStr = IndentStr ++ "    ",
    map.foldl(
        write_typeclass_info_args(Stream, TVarSet, VarTable,
            NextIndentStr),
        TypeClassInfoClassMap, !IO).

:- pred write_typeclass_info_args(io.text_output_stream::in,
    tvarset::in, var_table::in, string::in,
    list(mer_type)::in, tci_args_map::in, io::di, io::uo) is det.

write_typeclass_info_args(Stream, TVarSet, VarTable, IndentStr,
        Types, CVAMap, !IO) :-
    TypeStrs = list.map(trace_type_to_string(TVarSet), Types),
    TypesStr = string.join_list(", ", TypeStrs),
    io.format(Stream, "%stypes [%s]:\n", [s(IndentStr), s(TypesStr)], !IO),
    io.format(Stream, "%s:\n", [s(IndentStr)], !IO),
    NextIndentStr = IndentStr ++ "    ",
    map.foldl(
        write_typeclass_info_cva_map_entry(Stream, TVarSet, VarTable,
            NextIndentStr),
        CVAMap, !IO).

:- pred write_typeclass_info_cva_map_entry(io.text_output_stream::in,
    tvarset::in, var_table::in, string::in,
    list(const_or_var_arg)::in, var_and_maybe_csa::in,
    io::di, io::uo) is det.

write_typeclass_info_cva_map_entry(Stream, TVarSet, VarTable, IndentStr,
        ConstOrVarArgs, VarMaybeCSA, !IO) :-
    COVAStrs = list.map(const_or_var_arg_to_string(TVarSet, VarTable),
        ConstOrVarArgs),
    COVAsStr = string.join_list(", ", COVAStrs),
    VarMaybeCSAStr = var_and_maybe_csa_to_string(VarTable, VarMaybeCSA),
    io.format(Stream, "%s%s ->\n%s    %s\n",
        [s(IndentStr), s(COVAsStr), s(IndentStr), s(VarMaybeCSAStr)], !IO).

%---------------------------------------------------------------------------%

:- pred write_constr_struct_var_map(io.text_output_stream::in,
    poly_info::in, string::in, io::di, io::uo) is det.

write_constr_struct_var_map(Stream, Info, IndentStr, !IO) :-
    poly_info_get_const_struct_var_map(Info, ConstStructVarMap),
    poly_info_get_typevarset(Info, TVarSet),
    poly_info_get_var_table(Info, VarTable),
    io.format(Stream, "%sconst_struct_var_map\n", [s(IndentStr)], !IO),
    NextIndentStr = IndentStr ++ "    ",
    map.foldl(
        write_constr_struct_var_map_entry(Stream, TVarSet, VarTable,
            NextIndentStr),
        ConstStructVarMap, !IO).

:- pred write_constr_struct_var_map_entry(io.text_output_stream::in,
    tvarset::in, var_table::in, string::in, const_struct_arg::in, prog_var::in,
    io::di, io::uo) is det.

write_constr_struct_var_map_entry(Stream, TVarSet, VarTable, IndentStr,
        CSA, Var, !IO) :-
    CSAStr = const_struct_arg_to_string(TVarSet, CSA),
    VarStr = mercury_var_to_string(VarTable, print_name_and_num, Var),
    io.format(Stream, "%s%s ->\n%s    %s\n",
        [s(IndentStr), s(CSAStr), s(IndentStr), s(VarStr)], !IO).

%---------------------------------------------------------------------------%

var_and_maybe_csa_to_string(VarTable, var_and_maybe_csa(Var, MaybeCSA))
        = Str :-
    VarStr = mercury_var_to_string(VarTable, print_name_and_num, Var),
    string.format("%s - %s", [s(VarStr), s(string(MaybeCSA))], Str).

const_or_var_arg_to_string(TVarSet, VarTable, ConstOrVarArg) = Str :-
    (
        ConstOrVarArg = cova_const(ConstStructArg),
        Str = const_struct_arg_to_string(TVarSet, ConstStructArg)
    ;
        ConstOrVarArg = cova_var(Var),
        Str = mercury_var_to_string(VarTable, print_name_and_num, Var)
    ).

const_struct_arg_to_string(TVarSet, ConstStructArg) = Str :-
    (
        ConstStructArg = csa_const_struct(N),
        string.format("struct #%d", [i(N)], Str)
    ;
        ConstStructArg = csa_constant(ConsId, Type),
        ConsIdStr = unqual_cons_id_and_arity_to_string(ConsId),
        TypeStr = trace_type_to_string(TVarSet, Type),
        string.format("constant(%s %s)", [s(ConsIdStr), s(TypeStr)], Str)
    ).

%---------------------------------------------------------------------------%

trace_constraint_to_string(TVarSet, Constraint0) = Str :-
    strip_module_names_from_constraint(strip_all_module_names,
        set_default_func, Constraint0, Constraint),
    Constraint = constraint(ClassSymName, ArgTypes),
    ClassName = unqualify_name(ClassSymName),
    ArgTypeStrs = list.map(trace_type_to_string(TVarSet), ArgTypes),
    ArgTypesStr = string.join_list(", ", ArgTypeStrs),
    string.format("%s(%s)", [s(ClassName), s(ArgTypesStr)], Str).

trace_type_to_string(TVarSet, Type0) = Str :-
    strip_module_names_from_type(strip_all_module_names, set_default_func,
        Type0, Type),
    Str = mercury_type_to_string(TVarSet, print_name_and_num, Type).

%---------------------------------------------------------------------------%

:- pred format_for_trace(string::in, T::in, string::out,
    io::di, io::uo) is det.
:- pragma consider_used(pred(format_for_trace/5)).

format_for_trace(IndentStr, Item, ItemDocStr, !IO) :-
    get_default_formatter_map(FormatterMap, !IO),
    MaxLen = 78 - string.count_code_points(IndentStr),
    Params = pp_params(MaxLen, 99999, linear(99999)),
    ItemDoc = pretty_printer.format(Item),
    doc_to_string(canonicalize, FormatterMap, Params, ItemDoc, ItemDocStr0),
    add_prefix_to_every_line(IndentStr, ItemDocStr0, ItemDocStr).

%---------------------------------------------------------------------------%
:- end_module check_hlds.polymorphism_info.
%---------------------------------------------------------------------------%
