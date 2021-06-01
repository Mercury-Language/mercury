%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2012, 2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: const_struct.m.
% Author: zs
%
% This module defines the part of the HLDS that stores constant data structures
% separate from any procedure, for use on backends that support them.
%
%-----------------------------------------------------------------------------%

:- module hlds.const_struct.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module map.

:- type const_struct
    --->    const_struct(
                % The constant structured term: the data constructor,
                % and its arguments.
                cs_cons_id          :: cons_id,
                cs_args             :: list(const_struct_arg),

                % The type and inst of the term.
                cs_term_type        :: mer_type,
                cs_term_inst        :: mer_inst,

                % Was the code that led to the creation of this const_struct
                % in the current module? If not, then unused_imports.m
                % should *not* consider this const_struct as representing
                % uses of the modules that are referenced in it.
                cs_defined_where    :: defined_where
            ).

:- type defined_where
    --->    defined_in_this_module
    ;       defined_in_other_module.

:- type const_struct_arg
    --->    csa_const_struct(int)
            % The argument is a reference to another constant structure.
            % The argument gives the id of that structure.

    ;       csa_constant(cons_id, mer_type).
            % The argument is an arity-zero cons_id. The second argument
            % gives its type.

:- type const_struct_db.

:- type const_instance_id
    --->    ciid(
                % The instance number. This field is first because tests on it
                % are cheap.
                int,

                % The constraint.
                prog_constraint,

                % The already seen constraints.
                list(prog_constraint)
            ).

:- type const_instance_map == map(const_instance_id, int).

    % Initialize the database.
    %
:- pred const_struct_db_init(globals::in, const_struct_db::out) is det.

    % Look up a constant structure in the database. If it is not there, add it.
    %
:- pred lookup_insert_const_struct(const_struct::in, int::out,
    const_struct_db::in, const_struct_db::out) is det.

    % Look up the number of a constant structure in the database.
    % If it is not there, abort.
    %
:- pred lookup_const_struct_num(const_struct_db::in, int::in,
    const_struct::out) is det.

    % Check whether the given constant instance already has a constant
    % structure.
    %
:- pred search_for_constant_instance(const_struct_db::in,
    const_instance_id::in, int::out) is semidet.

    % Record that the given constant instance has a constant structure
    % identified by the given integer.
    %
:- pred insert_constant_instance(const_instance_id::in, int::in,
    const_struct_db::in, const_struct_db::out) is det.

    % Mark the constant structure with the given number as logically deleted.
    %
:- pred delete_const_struct(int::in,
    const_struct_db::in, const_struct_db::out) is det.

    % Get the list of constant structures to generate.
    % If the assoc list contains N elements, the keys are guaranteed to be
    % the integers 0 .. N-1 in ascending order.
    %
:- pred const_struct_db_get_structs(const_struct_db::in,
    assoc_list(int, const_struct)::out) is det.

    % Return whether the generation of separate constant structures is enabled
    % for (a) structures created by polymorphism, and (b) for structures
    % created in from_ground_term_construct scopes. If it is not, then
    % lookup_insert_const_struct should not be called from polymorphism.m
    % and simplify.m respectively.
    %
:- pred const_struct_db_get_poly_enabled(const_struct_db::in,
    maybe_enable_const_struct_poly::out) is det.
:- pred const_struct_db_get_ground_term_enabled(const_struct_db::in,
    maybe_enable_const_struct_user::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.

const_struct_db_init(Globals, Db) :-
    globals.get_opt_tuple(Globals, OptTuple),
    PolyEnabled = OptTuple ^ ot_enable_const_struct_poly,
    GroundTermEnabled = OptTuple ^ ot_enable_const_struct_user,
    Db = const_struct_db(PolyEnabled, GroundTermEnabled, 0,
        map.init, map.init, map.init, map.init).

lookup_insert_const_struct(ConstStruct, ConstNum, !Db) :-
    const_struct_db_get_poly_enabled(!.Db, Enabled),
    (
        Enabled = do_not_enable_const_struct_poly,
        unexpected($pred, "not enabled")
    ;
        Enabled = enable_const_struct_poly,
        ConstStruct = const_struct(ConsId, Args, Type, Inst, DefinedWhere),
        ( if ConsId = cons(SymName, _, _) then
            Name = unqualify_name(SymName),
            ConsProxyStruct = cons_proxy_struct(Name, Args, ConsId,
                Type, Inst),
            const_struct_db_get_next_num(!.Db, NextConstNum),
            const_struct_db_get_cons_proxy_map(!.Db, ConsProxyMap0),
            map.search_insert(ConsProxyStruct, NextConstNum, MaybeOldConstNum,
                ConsProxyMap0, ConsProxyMap),
            (
                MaybeOldConstNum = yes(ConstNum),
                % ConsProxyMap should be the same as ConsProxyMap0.
                maybe_update_defined_where(DefinedWhere, ConstNum, !Db)
            ;
                MaybeOldConstNum = no,
                ConstNum = NextConstNum,
                const_struct_db_get_num_map(!.Db, NumMap0),
                map.det_insert(ConstNum, ConstStruct, NumMap0, NumMap),

                const_struct_db_set_next_num(NextConstNum + 1, !Db),
                const_struct_db_set_cons_proxy_map(ConsProxyMap, !Db),
                const_struct_db_set_num_map(NumMap, !Db)
            )
        else
            NonConsProxyStruct = noncons_proxy_struct(ConsId, Args,
                Type, Inst),
            const_struct_db_get_next_num(!.Db, NextConstNum),
            const_struct_db_get_noncons_proxy_map(!.Db, NonConsProxyMap0),
            map.search_insert(NonConsProxyStruct, NextConstNum,
                MaybeOldConstNum, NonConsProxyMap0, NonConsProxyMap),
            (
                MaybeOldConstNum = yes(ConstNum),
                % NonConsProxyMap should be the same as NonConsProxyMap0.
                maybe_update_defined_where(DefinedWhere, ConstNum, !Db)
            ;
                MaybeOldConstNum = no,
                ConstNum = NextConstNum,
                const_struct_db_get_num_map(!.Db, NumMap0),
                map.det_insert(ConstNum, ConstStruct, NumMap0, NumMap),

                const_struct_db_set_next_num(NextConstNum + 1, !Db),
                const_struct_db_set_noncons_proxy_map(NonConsProxyMap, !Db),
                const_struct_db_set_num_map(NumMap, !Db)
            )
        )
    ).

:- pred maybe_update_defined_where(defined_where::in, int::in,
    const_struct_db::in, const_struct_db::out) is det.

maybe_update_defined_where(DefinedWhere, ConstNum, !Db) :-
    const_struct_db_get_num_map(!.Db, NumMap0),
    map.lookup(NumMap0, ConstNum, ConstStruct0),
    DefinedWhere0 = ConstStruct0 ^ cs_defined_where,
    % If the new definition says that this constant is derived from code
    % in this module, then set the constant accordingly, unless its old
    % setting already says the same.
    ( if
        DefinedWhere = defined_in_this_module,
        DefinedWhere0 = defined_in_other_module
    then
        ConstStruct = ConstStruct0 ^ cs_defined_where
            := defined_in_this_module,
        map.det_update(ConstNum, ConstStruct, NumMap0, NumMap),
        const_struct_db_set_num_map(NumMap, !Db)
    else
        % There is nothing to update.
        true
    ).

lookup_const_struct_num(Db, ConstNum, ConstStruct) :-
    const_struct_db_get_num_map(Db, NumMap),
    map.lookup(NumMap, ConstNum, ConstStruct).

search_for_constant_instance(Db, InstanceId, ConstNum) :-
    const_struct_db_get_instance_map(Db, InstanceMap),
    map.search(InstanceMap, InstanceId, ConstNum).

insert_constant_instance(InstanceId, ConstNum, !Db) :-
    const_struct_db_get_instance_map(!.Db, InstanceMap0),
    map.det_insert(InstanceId, ConstNum, InstanceMap0, InstanceMap),
    const_struct_db_set_instance_map(InstanceMap, !Db).

delete_const_struct(ConstNum, !Db) :-
    const_struct_db_get_num_map(!.Db, NumMap0),
    map.det_remove(ConstNum, ConstStruct, NumMap0, NumMap),
    const_struct_db_set_num_map(NumMap, !Db),

    ConstStruct = const_struct(ConsId, Args, Type, Inst, _DefinedWhere),
    ( if ConsId = cons(SymName, _, _) then
        Name = unqualify_name(SymName),
        ConsProxyStruct = cons_proxy_struct(Name, Args, ConsId, Type, Inst),
        const_struct_db_get_cons_proxy_map(!.Db, ConsProxyMap0),
        map.det_remove(ConsProxyStruct, _ConstNum,
            ConsProxyMap0, ConsProxyMap),
        const_struct_db_set_cons_proxy_map(ConsProxyMap, !Db)
    else
        NonConsProxyStruct = noncons_proxy_struct(ConsId, Args, Type, Inst),
        const_struct_db_get_noncons_proxy_map(!.Db, NonConsProxyMap0),
        map.det_remove(NonConsProxyStruct, _ConstNum,
            NonConsProxyMap0, NonConsProxyMap),
        const_struct_db_set_noncons_proxy_map(NonConsProxyMap, !Db)
    ).

const_struct_db_get_structs(Db, Structs) :-
    const_struct_db_get_num_map(Db, NumMap),
    map.to_assoc_list(NumMap, Structs).

%-----------------------------------------------------------------------------%

    % Values of this type contain the same information as values of type
    % const_struct, but in a form that should allow significantly faster
    % comparisons, because it copies to the front the data item most useful
    % for comparisons, the name of the function symbol. If two proxy structs
    % match on the first two fields, they are almost certain to match
    % on the other three as well.
    %
:- type cons_proxy_struct
    --->    cons_proxy_struct(
                cps_name        :: string,
                cps_args        :: list(const_struct_arg),
                cps_cons_id     :: cons_id,
                cps_term_type   :: mer_type,
                cps_term_inst   :: mer_inst
            ).

:- type noncons_proxy_struct
    --->    noncons_proxy_struct(
                % All the fields of a const_struct, except defined_where.
                ncps_cons_id    :: cons_id,
                ncps_args       :: list(const_struct_arg),
                ncps_term_type  :: mer_type,
                ncps_term_inst  :: mer_inst
            ).

:- type const_struct_db
    --->    const_struct_db(
                csdb_poly_enabled           :: maybe_enable_const_struct_poly,
                csdb_ground_term_enabled    :: maybe_enable_const_struct_user,
                csdb_next_num               :: int,
                csdb_cons_proxy_map         :: map(cons_proxy_struct, int),
                csdb_noncons_struct_map     :: map(noncons_proxy_struct, int),
                csdb_num_map                :: map(int, const_struct),
                csdb_instance_map           :: const_instance_map
            ).

:- pred const_struct_db_get_next_num(const_struct_db::in, int::out) is det.
:- pred const_struct_db_get_cons_proxy_map(const_struct_db::in,
    map(cons_proxy_struct, int)::out) is det.
:- pred const_struct_db_get_noncons_proxy_map(const_struct_db::in,
    map(noncons_proxy_struct, int)::out) is det.
:- pred const_struct_db_get_num_map(const_struct_db::in,
    map(int, const_struct)::out) is det.
:- pred const_struct_db_get_instance_map(const_struct_db::in,
    const_instance_map::out) is det.

const_struct_db_get_poly_enabled(Db, X) :-
    X = Db ^ csdb_poly_enabled.
const_struct_db_get_ground_term_enabled(Db, X) :-
    X = Db ^ csdb_ground_term_enabled.
const_struct_db_get_next_num(Db, X) :-
    X = Db ^ csdb_next_num.
const_struct_db_get_cons_proxy_map(Db, X) :-
    X = Db ^ csdb_cons_proxy_map.
const_struct_db_get_noncons_proxy_map(Db, X) :-
    X = Db ^ csdb_noncons_struct_map.
const_struct_db_get_num_map(Db, X) :-
    X = Db ^ csdb_num_map.
const_struct_db_get_instance_map(Db, X) :-
    X = Db ^ csdb_instance_map.

:- pred const_struct_db_set_next_num(int::in,
    const_struct_db::in, const_struct_db::out) is det.
:- pred const_struct_db_set_cons_proxy_map(map(cons_proxy_struct, int)::in,
    const_struct_db::in, const_struct_db::out) is det.
:- pred const_struct_db_set_noncons_proxy_map(
    map(noncons_proxy_struct, int)::in,
    const_struct_db::in, const_struct_db::out) is det.
:- pred const_struct_db_set_num_map(map(int, const_struct)::in,
    const_struct_db::in, const_struct_db::out) is det.
:- pred const_struct_db_set_instance_map(const_instance_map::in,
    const_struct_db::in, const_struct_db::out) is det.

const_struct_db_set_next_num(Num, !Db) :-
    !Db ^ csdb_next_num := Num.
const_struct_db_set_cons_proxy_map(ConsProxyMap, !Db) :-
    !Db ^ csdb_cons_proxy_map := ConsProxyMap.
const_struct_db_set_noncons_proxy_map(NonConsProxyMap, !Db) :-
    !Db ^ csdb_noncons_struct_map := NonConsProxyMap.
const_struct_db_set_num_map(NumMap, !Db) :-
    !Db ^ csdb_num_map := NumMap.
const_struct_db_set_instance_map(InstanceMap, !Db) :-
    !Db ^ csdb_instance_map := InstanceMap.

%-----------------------------------------------------------------------------%
:- end_module hlds.const_struct.
%-----------------------------------------------------------------------------%
