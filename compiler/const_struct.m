%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2012 The University of Melbourne.
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

:- import_module libs.globals.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.

:- type const_struct
    --->    const_struct(cons_id, list(const_struct_arg), mer_type, mer_inst).

:- type const_struct_arg
    --->    csa_const_struct(int)
    ;       csa_constant(cons_id, mer_type).

:- type const_struct_db.

:- type const_instance_id
    --->    ciid(
                % The instance number. This is first because tests on it
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

    % Return whether the generation of separate constant structures is enabled.
    % If it is not, the lookup_insert_const_struct and
    % const_struct_db_get_structs predicates should never be called.
    %
:- pred const_struct_db_get_enabled(const_struct_db::in, bool::out) is det.

    % Look up a constant structure in the database. If it is not there,
    % add it.
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

    % Get the list of constant structures to generate.
    % If the assoc list contains N elements, the keys are guaranteed to be
    % the integers 0 .. N-1 in ascending order.
    %
:- pred const_struct_db_get_structs(const_struct_db::in,
    assoc_list(int, const_struct)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.

:- import_module int.
:- import_module pair.
:- import_module require.

const_struct_db_init(Globals, Db) :-
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
        (
            HighLevelData = no,
            globals.get_tags_method(Globals, Tags),
            (
                Tags = tags_low,
                globals.lookup_bool_option(Globals, enable_const_struct,
                    Enabled)
            ;
                ( Tags = tags_high
                ; Tags = tags_none
                ),
                Enabled = no
            )
        ;
            HighLevelData = yes,
            Enabled = no
        )
    ;
        ( Target = target_il
        ; Target = target_csharp
        ; Target = target_java
        ; Target = target_asm
        ; Target = target_x86_64
        ; Target = target_erlang
        ),
        Enabled = no
    ),
    Db = const_struct_db(Enabled, 0, map.init, map.init, [], map.init).

lookup_insert_const_struct(ConstStruct, ConstNum, !Db) :-
    const_struct_db_get_struct_map(!.Db, StructMap0),
    ( map.search(StructMap0, ConstStruct, ConstNumPrime) ->
        ConstNum = ConstNumPrime
    ;
        % Since we expect many searches to be successful if enabled,
        % we don't test the enabled flag on every search. We just test
        % it on insertions. Without successful insertions, searches
        % cannot succeed, so this is enough.
        const_struct_db_get_enabled(!.Db, Enabled),
        (
            Enabled = no,
            unexpected($module, $pred, "not enabled")
        ;
            Enabled = yes,
            const_struct_db_get_next_num(!.Db, ConstNum),
            const_struct_db_set_next_num(ConstNum + 1, !Db),

            map.det_insert(ConstStruct, ConstNum, StructMap0, StructMap),
            const_struct_db_set_struct_map(StructMap, !Db),

            const_struct_db_get_num_map(!.Db, NumMap0),
            map.det_insert(ConstNum, ConstStruct, NumMap0, NumMap),
            const_struct_db_set_num_map(NumMap, !Db),

            const_struct_db_get_rev_structs(!.Db, RevStructs0),
            RevStructs = [ConstNum - ConstStruct | RevStructs0],
            const_struct_db_set_rev_structs(RevStructs, !Db)
        )
    ).

lookup_const_struct_num(Db, ConstNum, ConstStruct) :-
    const_struct_db_get_num_map(Db, NumMap),
    map.lookup(NumMap, ConstNum, ConstStruct).

const_struct_db_get_structs(Db, Structs) :-
    const_struct_db_get_rev_structs(Db, RevStructs),
    list.reverse(RevStructs, Structs).

search_for_constant_instance(Db, InstanceId, ConstNum) :-
    const_struct_db_get_instance_map(Db, InstanceMap),
    map.search(InstanceMap, InstanceId, ConstNum).

insert_constant_instance(InstanceId, ConstNum, !Db) :-
    const_struct_db_get_instance_map(!.Db, InstanceMap0),
    map.det_insert(InstanceId, ConstNum, InstanceMap0, InstanceMap),
    const_struct_db_set_instance_map(InstanceMap, !Db).

%-----------------------------------------------------------------------------%

:- type const_struct_db
    --->    const_struct_db(
                csdb_enabled        :: bool,
                csdb_next_num       :: int,
                csdb_struct_map     :: map(const_struct, int),
                csdb_num_map        :: map(int, const_struct),
                csdb_rev_structs    :: assoc_list(int, const_struct),
                csdb_instance_map   :: const_instance_map
            ).

:- pred const_struct_db_get_next_num(const_struct_db::in, int::out) is det.
:- pred const_struct_db_get_struct_map(const_struct_db::in,
    map(const_struct, int)::out) is det.
:- pred const_struct_db_get_num_map(const_struct_db::in,
    map(int, const_struct)::out) is det.
:- pred const_struct_db_get_rev_structs(const_struct_db::in,
    assoc_list(int, const_struct)::out) is det.
:- pred const_struct_db_get_instance_map(const_struct_db::in,
    const_instance_map::out) is det.

const_struct_db_get_enabled(Db, Db ^ csdb_enabled).
const_struct_db_get_next_num(Db, Db ^ csdb_next_num).
const_struct_db_get_struct_map(Db, Db ^ csdb_struct_map).
const_struct_db_get_num_map(Db, Db ^ csdb_num_map).
const_struct_db_get_rev_structs(Db, Db ^ csdb_rev_structs).
const_struct_db_get_instance_map(Db, Db ^ csdb_instance_map).

:- pred const_struct_db_set_next_num(int::in,
    const_struct_db::in, const_struct_db::out) is det.
:- pred const_struct_db_set_struct_map(map(const_struct, int)::in,
    const_struct_db::in, const_struct_db::out) is det.
:- pred const_struct_db_set_num_map(map(int, const_struct)::in,
    const_struct_db::in, const_struct_db::out) is det.
:- pred const_struct_db_set_rev_structs(assoc_list(int, const_struct)::in,
    const_struct_db::in, const_struct_db::out) is det.
:- pred const_struct_db_set_instance_map(const_instance_map::in,
    const_struct_db::in, const_struct_db::out) is det.

const_struct_db_set_next_num(Num, !Db) :-
    !Db ^ csdb_next_num := Num.
const_struct_db_set_struct_map(StructMap, !Db) :-
    !Db ^ csdb_struct_map := StructMap.
const_struct_db_set_num_map(NumMap, !Db) :-
    !Db ^ csdb_num_map := NumMap.
const_struct_db_set_rev_structs(RevStructs, !Db) :-
    !Db ^ csdb_rev_structs := RevStructs.
const_struct_db_set_instance_map(InstanceMap, !Db) :-
    !Db ^ csdb_instance_map := InstanceMap.

%-----------------------------------------------------------------------------%
:- end_module hlds.const_struct.
%-----------------------------------------------------------------------------%
