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
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.

:- type const_struct
    --->    const_struct(
                % The constant structured term: the data constructor,
                % and its arguments.
                cs_cons_id      :: cons_id,
                cs_args         :: list(const_struct_arg),

                % The type and inst of the term.
                cs_term_type    :: mer_type,
                cs_term_inst    :: mer_inst
            ).

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
    bool::out) is det.
:- pred const_struct_db_get_ground_term_enabled(const_struct_db::in,
    bool::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.

const_struct_db_init(Globals, Db) :-
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        globals.get_tags_method(Globals, Tags),
        (
            Tags = tags_low,
            can_enable_const_struct(Globals, PolyEnabled, GroundTermEnabled)
        ;
            Tags = tags_none,
            PolyEnabled = no,
            GroundTermEnabled = no
        )
    ;
        Target = target_java,
        can_enable_const_struct(Globals, PolyEnabled, _GroundTermEnabled),
        GroundTermEnabled = no
    ;
        ( Target = target_csharp
        ; Target = target_erlang
        ),
        PolyEnabled = no,
        GroundTermEnabled = no
    ),
    Db = const_struct_db(PolyEnabled, GroundTermEnabled, 0,
        map.init, map.init, map.init, map.init).

    % Test if constant structures are enabled for polymorphism structures
    % and from ground_term_contexts. The latter is only enabled if tracing
    % does not require procedure bodies to be preserved. The caller
    % (const_struct_db_init/2) must also check if the compilation grade
    % supports constant structures.
    %
:- pred can_enable_const_struct(globals::in, bool::out, bool::out) is det.

can_enable_const_struct(Globals, PolyEnabled, GroundTermEnabled) :-
    globals.lookup_bool_option(Globals, enable_const_struct,
        OptionEnabled),
    PolyEnabled = OptionEnabled,

    globals.get_trace_level(Globals, TraceLevel),
    globals.get_trace_suppress(Globals, TraceSuppress),
    Bodies = trace_needs_proc_body_reps(TraceLevel, TraceSuppress),
    (
        Bodies = no,
        GroundTermEnabled = OptionEnabled
    ;
        Bodies = yes,
        % We generate representations of procedure bodies for the
        % declarative debugger and for the profiler. When traverse_primitives
        % in browser/declarative_tree.m looks for the Nth argument of
        % variable X and X is built with a unification such as
        % X = ground_term_const(...), it crashes. It should be taught not
        % to do that, but in the meantime, we prevent the situation from
        % arising in the first place. (We never look for the original
        % sources of type infos and typeclass infos, so we can use constant
        % structures for them.)
        GroundTermEnabled = no
    ).

lookup_insert_const_struct(ConstStruct, ConstNum, !Db) :-
    const_struct_db_get_poly_enabled(!.Db, Enabled),
    (
        Enabled = no,
        unexpected($pred, "not enabled")
    ;
        Enabled = yes,
        ConstStruct = const_struct(ConsId, Args, Type, Inst),
        ( if ConsId = cons(SymName, _, _) then
            Name = unqualify_name(SymName),
            ConsProxyStruct = cons_proxy_struct(Name, Args, ConsId,
                Type, Inst),
            const_struct_db_get_next_num(!.Db, NextConstNum),
            const_struct_db_get_cons_proxy_map(!.Db, ConsMap0),
            map.search_insert(ConsProxyStruct, NextConstNum, MaybeOldConstNum,
                ConsMap0, ConsMap),
            (
                MaybeOldConstNum = yes(ConstNum)
                % ConsMap should be the same as ConsMap0.
            ;
                MaybeOldConstNum = no,
                ConstNum = NextConstNum,
                const_struct_db_get_num_map(!.Db, NumMap0),
                map.det_insert(ConstNum, ConstStruct, NumMap0, NumMap),

                const_struct_db_set_next_num(NextConstNum + 1, !Db),
                const_struct_db_set_cons_proxy_map(ConsMap, !Db),
                const_struct_db_set_num_map(NumMap, !Db)
            )
        else
            const_struct_db_get_next_num(!.Db, NextConstNum),
            const_struct_db_get_other_struct_map(!.Db, OtherMap0),
            map.search_insert(ConstStruct, NextConstNum, MaybeOldConstNum,
                OtherMap0, OtherMap),
            (
                MaybeOldConstNum = yes(ConstNum)
                % OtherMap should be the same as OtherMap0.
            ;
                MaybeOldConstNum = no,
                ConstNum = NextConstNum,
                const_struct_db_get_num_map(!.Db, NumMap0),
                map.det_insert(ConstNum, ConstStruct, NumMap0, NumMap),

                const_struct_db_set_next_num(NextConstNum + 1, !Db),
                const_struct_db_set_other_struct_map(OtherMap, !Db),
                const_struct_db_set_num_map(NumMap, !Db)
            )
        )
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

    ConstStruct = const_struct(ConsId, Args, Type, Inst),
    ( if ConsId = cons(SymName, _, _) then
        Name = unqualify_name(SymName),
        ConsProxyStruct = cons_proxy_struct(Name, Args, ConsId, Type, Inst),
        const_struct_db_get_cons_proxy_map(!.Db, ConsMap0),
        map.det_remove(ConsProxyStruct, _ConstNum, ConsMap0, ConsMap),
        const_struct_db_set_cons_proxy_map(ConsMap, !Db)
    else
        const_struct_db_get_other_struct_map(!.Db, OtherMap0),
        map.det_remove(ConstStruct, _ConstNum, OtherMap0, OtherMap),
        const_struct_db_set_other_struct_map(OtherMap, !Db)
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

:- type const_struct_db
    --->    const_struct_db(
                csdb_poly_enabled           :: bool,
                csdb_ground_term_enabled    :: bool,
                csdb_next_num               :: int,
                csdb_cons_proxy_map         :: map(cons_proxy_struct, int),
                csdb_other_struct_map       :: map(const_struct, int),
                csdb_num_map                :: map(int, const_struct),
                csdb_instance_map           :: const_instance_map
            ).

:- pred const_struct_db_get_next_num(const_struct_db::in, int::out) is det.
:- pred const_struct_db_get_cons_proxy_map(const_struct_db::in,
    map(cons_proxy_struct, int)::out) is det.
:- pred const_struct_db_get_other_struct_map(const_struct_db::in,
    map(const_struct, int)::out) is det.
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
const_struct_db_get_other_struct_map(Db, X) :-
    X = Db ^ csdb_other_struct_map.
const_struct_db_get_num_map(Db, X) :-
    X = Db ^ csdb_num_map.
const_struct_db_get_instance_map(Db, X) :-
    X = Db ^ csdb_instance_map.

:- pred const_struct_db_set_next_num(int::in,
    const_struct_db::in, const_struct_db::out) is det.
:- pred const_struct_db_set_cons_proxy_map(map(cons_proxy_struct, int)::in,
    const_struct_db::in, const_struct_db::out) is det.
:- pred const_struct_db_set_other_struct_map(map(const_struct, int)::in,
    const_struct_db::in, const_struct_db::out) is det.
:- pred const_struct_db_set_num_map(map(int, const_struct)::in,
    const_struct_db::in, const_struct_db::out) is det.
:- pred const_struct_db_set_instance_map(const_instance_map::in,
    const_struct_db::in, const_struct_db::out) is det.

const_struct_db_set_next_num(Num, !Db) :-
    !Db ^ csdb_next_num := Num.
const_struct_db_set_cons_proxy_map(ConsMap, !Db) :-
    !Db ^ csdb_cons_proxy_map := ConsMap.
const_struct_db_set_other_struct_map(OtherMap, !Db) :-
    !Db ^ csdb_other_struct_map := OtherMap.
const_struct_db_set_num_map(NumMap, !Db) :-
    !Db ^ csdb_num_map := NumMap.
const_struct_db_set_instance_map(InstanceMap, !Db) :-
    !Db ^ csdb_instance_map := InstanceMap.

%-----------------------------------------------------------------------------%
:- end_module hlds.const_struct.
%-----------------------------------------------------------------------------%
