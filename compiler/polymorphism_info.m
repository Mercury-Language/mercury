%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012, 2014 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
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
:- import_module hlds.hlds_rtti.
:- import_module hlds.vartypes.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

:- type const_or_var_arg
    --->    cova_const(const_struct_arg)
    ;       cova_var(prog_var).

:- type type_info_var_map ==
    map(type_ctor,
        map(list(mer_type), pair(prog_var, maybe(const_struct_arg)))).

:- type typeclass_info_map_entry
    --->    typeclass_info_map_entry(
                % The cons_id representing the base_typeclass_info.
                cons_id,

                % Maps the arguments of the typeclass_info_cell_constructor
                % after the base_typeclass_info to the variable that holds the
                % typeclass_info for that cell.
                map(list(const_or_var_arg),
                    pair(prog_var, maybe(const_struct_arg)))
            ).

:- type typeclass_info_map ==
    map(class_name, map(list(mer_type), typeclass_info_map_entry)).

:- type int_const_map == map(int, prog_var).

    % If the value that can be a constant structure argument is currently
    % available in a variable, give the id of that variable.
    %
:- type const_struct_var_map == map(const_struct_arg, prog_var).

%---------------------------------------------------------------------------%

:- type poly_info.

    % Init_poly_info initializes a poly_info from a pred_info and clauses_info.
    % (See also create_poly_info.)
    %
:- pred init_poly_info(module_info::in, pred_info::in, clauses_info::in,
    poly_info::out) is det.

    % Extract some fields from a pred_info and proc_info and use them to
    % create a poly_info, for use by the polymorphism transformation.
    %
:- pred create_poly_info(module_info::in, pred_info::in,
    proc_info::in, poly_info::out) is det.

    % Update the fields in a pred_info and proc_info with
    % the values in a poly_info.
    %
:- pred poly_info_extract(poly_info::in, list(error_spec)::out,
    pred_info::in, pred_info::out, proc_info::in, proc_info::out,
    module_info::out) is det.

%---------------------------------------------------------------------------%

:- pred poly_info_get_module_info(poly_info::in,
    module_info::out) is det.
:- pred poly_info_get_varset(poly_info::in,
    prog_varset::out) is det.
:- pred poly_info_get_var_types(poly_info::in,
    vartypes::out) is det.
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
:- pred poly_info_get_type_info_var_map(poly_info::in,
    type_info_var_map::out) is det.
:- pred poly_info_get_typeclass_info_map(poly_info::in,
    typeclass_info_map::out) is det.
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
:- pred poly_info_get_errors(poly_info::in,
    list(error_spec)::out) is det.

:- pred poly_info_set_varset(prog_varset::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_varset_types(prog_varset::in, vartypes::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_varset_types_rtti(prog_varset::in, vartypes::in,
    rtti_varmaps::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_rtti_varmaps(rtti_varmaps::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_typevarset(tvarset::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_proof_map(constraint_proof_map::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_type_info_var_map(type_info_var_map::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_typeclass_info_map(typeclass_info_map::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_int_const_map(int_const_map::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_const_struct_var_map(const_struct_var_map::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_num_reuses(int::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_const_struct_db(const_struct_db::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_errors(list(error_spec)::in,
    poly_info::in, poly_info::out) is det.

%---------------------------------------------------------------------------%

:- type cache_maps
    --->    cache_maps(
                cm_snapshot_num             :: int,
                cm_type_info_var_map        :: type_info_var_map,
                cm_typeclass_info_map       :: typeclass_info_map,
                cm_int_const_map            :: int_const_map,
                cm_const_struct_var_map     :: const_struct_var_map
            ).

:- pred get_cache_maps_snapshot(string::in, cache_maps::out,
    poly_info::in, poly_info::out) is det.

:- pred set_cache_maps_snapshot(string::in, cache_maps::in,
    poly_info::in, poly_info::out) is det.

:- pred empty_cache_maps(poly_info::in, poly_info::out) is det.

%---------------------------------------------------------------------------%

:- type var_maps
    --->    var_maps(
                vm_snapshot_num             :: int,
                vm_varset                   :: prog_varset,
                vm_vartypes                 :: vartypes,
                vm_rtti_varmaps             :: rtti_varmaps,
                vm_cache_maps               :: cache_maps
            ).

:- pred get_var_maps_snapshot(string::in, var_maps::out,
    poly_info::in, poly_info::out) is det.

:- pred set_var_maps_snapshot(string::in, var_maps::in,
    poly_info::in, poly_info::out) is det.

%---------------------------------------------------------------------------%

:- pred get_poly_const(int::in, prog_var::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

:- pred all_are_const_struct_args(
    assoc_list(prog_var, maybe(const_struct_arg))::in,
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
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.make_goal.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.

:- import_module bool.
:- import_module int.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type poly_info
    --->    poly_info(
                poly_module_info            :: module_info,

                poly_varset                 :: prog_varset,
                poly_vartypes               :: vartypes,
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
                poly_type_info_var_map      :: type_info_var_map,
                poly_typeclass_info_map     :: typeclass_info_map,
                poly_int_const_map          :: int_const_map,
                poly_const_struct_var_map   :: const_struct_var_map,
                poly_num_reuses             :: int,

                poly_snapshot_num           :: int,

                % The database of constant structures of the module.
                % If a type_info or typeclass_info we construct is a constant
                % term, we allocate it in this database.
                poly_const_struct_db        :: const_struct_db,

                poly_defined_where          :: defined_where,

                % The list of errors we have discovered during the polymorphism
                % pass.
                poly_errors                 :: list(error_spec)
            ).

%---------------------------------------------------------------------------%

init_poly_info(ModuleInfo, PredInfo, ClausesInfo, PolyInfo) :-
    clauses_info_get_varset(ClausesInfo, VarSet),
    clauses_info_get_vartypes(ClausesInfo, VarTypes),
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
    Specs = [],
    PolyInfo = poly_info(ModuleInfo, VarSet, VarTypes, RttiVarMaps,
        TypeVarSet, TypeVarKinds, ProofMap, ConstraintMap,
        TypeInfoVarMap, TypeClassInfoMap, IntConstMap, ConstStructVarMap,
        NumReuses, SnapshotNum, ConstStructDb, DefinedWhere, Specs).

create_poly_info(ModuleInfo, PredInfo, ProcInfo, PolyInfo) :-
    pred_info_get_typevarset(PredInfo, TypeVarSet),
    pred_info_get_tvar_kind_map(PredInfo, TypeVarKinds),
    pred_info_get_constraint_proof_map(PredInfo, ProofMap),
    pred_info_get_constraint_map(PredInfo, ConstraintMap),
    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
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
    Specs = [],
    PolyInfo = poly_info(ModuleInfo, VarSet, VarTypes, RttiVarMaps,
        TypeVarSet, TypeVarKinds, ProofMap, ConstraintMap,
        TypeInfoVarMap, TypeClassInfoMap, IntConstMap, ConstStructVarMap,
        NumReuses, SnapshotNum, ConstStructDb, DefinedWhere, Specs).

poly_info_extract(Info, Specs, !PredInfo, !ProcInfo, !:ModuleInfo) :-
    Info = poly_info(!:ModuleInfo, VarSet, VarTypes, RttiVarMaps,
        TypeVarSet, TypeVarKinds, _ProofMap, _ConstraintMap,
        _TypeInfoVarMap, _TypeClassInfoMap, _IntConstMap, _ConstStructVarMap,
        _NumReuses, _SnapshotNum, ConstStructDb, _DefinedWhere, Specs),

    module_info_set_const_struct_db(ConstStructDb, !ModuleInfo),

    % Set the new values of the fields in proc_info and pred_info.
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),
    pred_info_set_typevarset(TypeVarSet, !PredInfo),
    pred_info_set_tvar_kind_map(TypeVarKinds, !PredInfo).

%---------------------------------------------------------------------------%

:- pragma inline(pred(poly_info_get_module_info/2)).
:- pragma inline(pred(poly_info_get_varset/2)).
:- pragma inline(pred(poly_info_get_var_types/2)).
:- pragma inline(pred(poly_info_get_rtti_varmaps/2)).
:- pragma inline(pred(poly_info_get_typevarset/2)).
:- pragma inline(pred(poly_info_get_tvar_kind_map/2)).
:- pragma inline(pred(poly_info_get_proof_map/2)).
:- pragma inline(pred(poly_info_get_constraint_map/2)).
:- pragma inline(pred(poly_info_get_type_info_var_map/2)).
:- pragma inline(pred(poly_info_get_typeclass_info_map/2)).
:- pragma inline(pred(poly_info_get_const_struct_var_map/2)).
:- pragma inline(pred(poly_info_get_int_const_map/2)).
:- pragma inline(pred(poly_info_get_num_reuses/2)).
:- pragma inline(pred(poly_info_get_const_struct_db/2)).
:- pragma inline(pred(poly_info_get_errors/2)).

poly_info_get_module_info(!.PI, X) :-
    X = !.PI ^ poly_module_info.
poly_info_get_varset(!.PI, X) :-
    X = !.PI ^ poly_varset.
poly_info_get_var_types(!.PI, X) :-
    X = !.PI ^ poly_vartypes.
poly_info_get_rtti_varmaps(!.PI, X) :-
    X = !.PI ^ poly_rtti_varmaps.
poly_info_get_typevarset(!.PI, X) :-
    X = !.PI ^ poly_typevarset.
poly_info_get_tvar_kind_map(!.PI, X) :-
    X = !.PI ^ poly_tvar_kind_map.
poly_info_get_proof_map(!.PI, X) :-
    X = !.PI ^ poly_proof_map.
poly_info_get_constraint_map(!.PI, X) :-
    X = !.PI ^ poly_constraint_map.
poly_info_get_type_info_var_map(!.PI, X) :-
    X = !.PI ^ poly_type_info_var_map.
poly_info_get_typeclass_info_map(!.PI, X) :-
    X = !.PI ^ poly_typeclass_info_map.
poly_info_get_int_const_map(!.PI, X) :-
    X = !.PI ^ poly_int_const_map.
poly_info_get_const_struct_var_map(!.PI, X) :-
    X = !.PI ^ poly_const_struct_var_map.
poly_info_get_num_reuses(!.PI, X) :-
    X = !.PI ^ poly_num_reuses.
poly_info_get_const_struct_db(!.PI, X) :-
    X = !.PI ^ poly_const_struct_db.
poly_info_get_defined_where(!.PI, X) :-
    X = !.PI ^ poly_defined_where.
poly_info_get_errors(!.PI, X) :-
    X = !.PI ^ poly_errors.

:- pragma inline(pred(poly_info_set_varset/3)).
:- pragma inline(pred(poly_info_set_varset_types/4)).
:- pragma inline(pred(poly_info_set_varset_types_rtti/5)).
:- pragma inline(pred(poly_info_set_rtti_varmaps/3)).
:- pragma inline(pred(poly_info_set_typevarset/3)).
:- pragma inline(pred(poly_info_set_proof_map/3)).
:- pragma inline(pred(poly_info_set_type_info_var_map/3)).
:- pragma inline(pred(poly_info_set_typeclass_info_map/3)).
:- pragma inline(pred(poly_info_set_int_const_map/3)).
:- pragma inline(pred(poly_info_set_const_struct_var_map/3)).
:- pragma inline(pred(poly_info_set_num_reuses/3)).
:- pragma inline(pred(poly_info_set_const_struct_db/3)).
:- pragma inline(pred(poly_info_set_errors/3)).

poly_info_set_varset(X, !PI) :-
    !PI ^ poly_varset := X.
poly_info_set_varset_types(X, Y, !PI) :-
    !:PI = ((!.PI
        ^ poly_varset := X)
        ^ poly_vartypes := Y).
poly_info_set_varset_types_rtti(X, Y, Z, !PI) :-
    !:PI = (((!.PI
        ^ poly_varset := X)
        ^ poly_vartypes := Y)
        ^ poly_rtti_varmaps := Z).
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
poly_info_set_type_info_var_map(X, !PI) :-
    ( if private_builtin.pointer_equal(X, !.PI ^ poly_type_info_var_map) then
        true
    else
        !PI ^ poly_type_info_var_map := X
    ).
poly_info_set_typeclass_info_map(X, !PI) :-
    ( if private_builtin.pointer_equal(X, !.PI ^ poly_typeclass_info_map) then
        true
    else
        !PI ^ poly_typeclass_info_map := X
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
%  8    811687    776589    288951  72.882% type_info_var_map
%  9    385071    863384      6104  99.298% typeclass_info_map
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
    poly_info_get_type_info_var_map(!.Info, TypeInfoVarMap),
    poly_info_get_typeclass_info_map(!.Info, TypeClassInfoMap),
    poly_info_get_int_const_map(!.Info, IntConstMap),
    poly_info_get_const_struct_var_map(!.Info, ConstStructVarMap),

    SnapshotNum = !.Info ^ poly_snapshot_num,
    CacheMaps = cache_maps(SnapshotNum, TypeInfoVarMap, TypeClassInfoMap,
        IntConstMap, ConstStructVarMap),
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
            poly_info_get_varset(!.Info, VarSet),
            NumVars = varset.num_allocated(VarSet),
            io.format(Stream, "%sget_cache_maps_snapshot %d %s\n",
                [s(IndentStr), i(SnapshotNum), s(Name)], !IO),
            io.format(Stream, "%snum_allocated vars: %d\n\n",
                [s(IndentStr), i(NumVars)], !IO)
        else
            true
        )
    ).

set_cache_maps_snapshot(Name, CacheMaps, !Info) :-
    CacheMaps = cache_maps(SnapshotNum, TypeInfoVarMap, TypeClassInfoMap,
        IntConstMap, ConstStructVarMap),
    ( if
        private_builtin.pointer_equal(TypeInfoVarMap,
            !.Info ^ poly_type_info_var_map),
        private_builtin.pointer_equal(TypeClassInfoMap,
            !.Info ^ poly_typeclass_info_map),
        private_builtin.pointer_equal(IntConstMap,
            !.Info ^ poly_int_const_map),
        private_builtin.pointer_equal(ConstStructVarMap,
            !.Info ^ poly_const_struct_var_map)
    then
        true
    else
        !:Info = ((((!.Info
            ^ poly_type_info_var_map := TypeInfoVarMap)
            ^ poly_typeclass_info_map := TypeClassInfoMap)
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
            poly_info_get_varset(!.Info, VarSet),
            NumVars = varset.num_allocated(VarSet),

            io.format(Stream, "%sset_cache_maps_snapshot %d %s\n",
                [s(IndentStr), i(SnapshotNum), s(Name)], !IO),
            io.format(Stream, "%snum_allocated vars: %d\n\n",
                [s(IndentStr), i(NumVars)], !IO),

            io.format(Stream, "%stype_info_var_map ", [s(IndentStr)], !IO),
            io.write_line(Stream, CacheMaps ^ cm_type_info_var_map, !IO),
            io.format(Stream, "%stypeclass_info_map ",
                [s(IndentStr)], !IO),
            io.write_line(Stream, CacheMaps ^ cm_typeclass_info_map, !IO),
            io.format(Stream, "%sstruct_var_map ", [s(IndentStr)], !IO),
            io.write_line(Stream,
                CacheMaps ^ cm_const_struct_var_map, !IO),
            io.nl(Stream, !IO)
        else
            true
        )
    ).

empty_cache_maps(!Info) :-
    poly_info_set_type_info_var_map(map.init, !Info),
    poly_info_set_typeclass_info_map(map.init, !Info),
    poly_info_set_int_const_map(map.init, !Info),
    poly_info_set_const_struct_var_map(map.init, !Info).

%---------------------------------------------------------------------------%

get_var_maps_snapshot(Name, VarMaps, !Info) :-
    SnapshotNum = !.Info ^ poly_snapshot_num,
    poly_info_get_varset(!.Info, VarSet),
    poly_info_get_var_types(!.Info, VarTypes),
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
            NumVars = varset.num_allocated(VarSet),
            io.format(Stream, "%sget_var_maps_snapshot %d %s\n",
                [s(IndentStr), i(SnapshotNum), s(Name)], !IO),
            io.format(Stream, "%snum_allocated vars: %d\n\n",
                [s(IndentStr), i(NumVars)], !IO)
        )
    ),

    get_cache_maps_snapshot("", CacheMaps, !Info),
    VarMaps = var_maps(SnapshotNum, VarSet, VarTypes, RttiVarMaps, CacheMaps).

set_var_maps_snapshot(Name, VarMaps, !Info) :-
    VarMaps = var_maps(SnapshotNum, VarSet, VarTypes, RttiVarMaps, CacheMaps),

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

            io.format(Stream, "%stype_info_var_map ",
                [s(IndentStr)], !IO),
            io.write_line(Stream, CacheMaps ^ cm_type_info_var_map, !IO),
            io.format(Stream, "%stypeclass_info_map ",
                [s(IndentStr)], !IO),
            io.write_line(Stream, CacheMaps ^ cm_typeclass_info_map, !IO),
            io.format(Stream, "%sstruct_var_map ", [s(IndentStr)], !IO),
            io.write_line(Stream,
                CacheMaps ^ cm_const_struct_var_map, !IO),
            io.nl(Stream, !IO)
        )
    ),

    poly_info_set_varset_types_rtti(VarSet, VarTypes, RttiVarMaps, !Info),
    set_cache_maps_snapshot("", CacheMaps, !Info).

%---------------------------------------------------------------------------%

get_poly_const(IntConst, IntVar, Goals, !Info) :-
    poly_info_get_varset(!.Info, VarSet0),
    poly_info_get_var_types(!.Info, VarTypes0),
    poly_info_get_int_const_map(!.Info, IntConstMap0),
    ( if map.search(IntConstMap0, IntConst, IntVarPrime) then
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 1, !Info),
        IntVar = IntVarPrime,
        Goals = []
    else
        make_int_const_construction_alloc(IntConst,
            yes("PolyConst" ++ string.int_to_string(IntConst)),
            Goal, IntVar, VarSet0, VarSet, VarTypes0, VarTypes),
        map.det_insert(IntConst, IntVar, IntConstMap0, IntConstMap),
        poly_info_set_int_const_map(IntConstMap, !Info),
        poly_info_set_varset_types(VarSet, VarTypes, !Info),
        Goals = [Goal]
    ).

all_are_const_struct_args([], []).
all_are_const_struct_args([VarMCA | VarsMCAs], [ConstArg | ConstArgs]) :-
    VarMCA = _Var - MCA,
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
:- end_module check_hlds.polymorphism_info.
%---------------------------------------------------------------------------%
