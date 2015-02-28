%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This module preprocesses user-defined insts in the inst table,
% recording the results of the possible tests on those insts in the values
% expansions of the user-named insts themselves. It is better to do this
% once and for all than potentially many, many, many times during mode
% analysis.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.inst_user.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

:- pred pretest_user_inst_table(module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_data.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.              % ZZZ
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.mercury_to_mercury. % ZZZ

:- import_module assoc_list.
:- import_module io.                            % ZZZ
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module set.
:- import_module require.

:- type maybe_user_inst
    --->    user_inst_being_processed
    ;       processed_user_inst(hlds_inst_defn).

:- type maybe_inst_defns_map == map(inst_id, maybe_user_inst).

pretest_user_inst_table(!ModuleInfo) :-
    module_info_get_inst_table(!.ModuleInfo, InstTable0),
    inst_table_get_user_insts(InstTable0, UserInstTable0),
    map.to_sorted_assoc_list(UserInstTable0, UserInstDefns0),
%   trace [io(!IO)] (
%       io.write_string("BEFORE PRETEST\n", !IO),
%       list.foldl(output_user_inst_pair, UserInstDefns0, !IO)
%   ),
    pretest_user_inst_defns(UserInstDefns0, [], UserInstTable0,
        map.init, MaybeInstDefnsMap),
    map.to_sorted_assoc_list(MaybeInstDefnsMap, MaybeInstDefns),
    record_user_inst_results(MaybeInstDefns, UserInstDefns),
%   trace [io(!IO)] (
%       io.write_string("AFTER PRETEST\n", !IO),
%       list.foldl(output_user_inst_pair, UserInstDefns, !IO)
%   ),
    map.from_sorted_assoc_list(UserInstDefns, UserInstTable),
    inst_table_set_user_insts(UserInstTable, InstTable0, InstTable),
    module_info_set_inst_table(InstTable, !ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred pretest_user_inst_defns(assoc_list(inst_id, hlds_inst_defn)::in,
    assoc_list(inst_id, hlds_inst_defn)::in, map(inst_id, hlds_inst_defn)::in,
    map(inst_id, maybe_user_inst)::in, map(inst_id, maybe_user_inst)::out)
    is det.

pretest_user_inst_defns([], DelayedInstDefnPairs, UserInstTable0,
        !MaybeInstDefnsMap) :-
    (
        DelayedInstDefnPairs = []
    ;
        DelayedInstDefnPairs = [_ | _],
        pretest_user_inst_defns(DelayedInstDefnPairs, [], UserInstTable0,
            !MaybeInstDefnsMap)
    ).
pretest_user_inst_defns([InstDefnPair | InstDefnPairs], !.DelayedInstDefnPairs,
        UserInstTable0, !MaybeInstDefnsMap) :-
    InstDefnPair = InstId - InstDefn,
    ( if map.search(!.MaybeInstDefnsMap, InstId, MaybeUserInst) then
        (
            MaybeUserInst = user_inst_being_processed,
            !:DelayedInstDefnPairs = [InstDefnPair | !.DelayedInstDefnPairs]
        ;
            MaybeUserInst = processed_user_inst(_)
        )
    else
        pretest_user_inst_defn(InstId, InstDefn, UserInstTable0,
            !MaybeInstDefnsMap)
    ),
    pretest_user_inst_defns(InstDefnPairs, !.DelayedInstDefnPairs,
        UserInstTable0, !MaybeInstDefnsMap).

:- pred pretest_user_inst_defn(inst_id::in, hlds_inst_defn::in,
    map(inst_id, hlds_inst_defn)::in,
    map(inst_id, maybe_user_inst)::in, map(inst_id, maybe_user_inst)::out)
    is det.

pretest_user_inst_defn(InstId, InstDefn0, UserInstTable0,
        !MaybeInstDefnsMap) :-
    InstDefn0 = hlds_inst_defn(InstVarSet, InstParams, InstBody0,
        MaybeMatchingTypeCtors, Context, Status),
    (
        InstBody0 = abstract_inst,
        map.det_insert(InstId, processed_user_inst(InstDefn0),
            !MaybeInstDefnsMap)
    ;
        InstBody0 = eqv_inst(Inst0),
        (
            InstParams = [_ | _],
            map.det_insert(InstId, processed_user_inst(InstDefn0),
                !MaybeInstDefnsMap)
        ;
            InstParams = [],
            map.det_insert(InstId, user_inst_being_processed,
                !MaybeInstDefnsMap),
            pretest_inst(Inst0, Inst, UserInstTable0, _, _, _, _,
                !MaybeInstDefnsMap),
            InstBody = eqv_inst(Inst),
            % ZZZ MaybeMatchingTypeCtors
            InstDefn = hlds_inst_defn(InstVarSet, InstParams, InstBody,
                MaybeMatchingTypeCtors, Context, Status),
            map.det_update(InstId, processed_user_inst(InstDefn),
                !MaybeInstDefnsMap)
        )
    ).

:- pred pretest_inst(mer_inst::in, mer_inst::out,
    map(inst_id, hlds_inst_defn)::in,
    inst_result_groundness::out, inst_result_contains_any::out,
    inst_result_contains_instnames::out, inst_result_contains_types::out,
    map(inst_id, maybe_user_inst)::in, map(inst_id, maybe_user_inst)::out)
    is det.

pretest_inst(Inst0, Inst, UserInstTable0, Groundness, ContainsAny,
        ContainsInstNames, ContainsTypes, !MaybeInstDefnsMap) :-
    (
        Inst0 = free,
        Groundness = inst_result_is_not_ground,
        ContainsAny = inst_result_does_not_contain_any,
        ContainsInstNames = inst_result_contains_instnames_known(set.init),
        ContainsTypes = inst_result_contains_types_known(set.init),
        Inst = Inst0
    ;
        Inst0 = free(Type),
        Groundness = inst_result_is_not_ground,
        ContainsAny = inst_result_does_not_contain_any,
        ContainsInstNames = inst_result_contains_instnames_known(set.init),
        ( if type_to_ctor(Type, TypeCtor) then
            set.singleton_set(TypeCtor, TypeCtors),
            ContainsTypes = inst_result_contains_types_known(TypeCtors)
        else
            ContainsTypes = inst_result_contains_types_unknown
        ),
        Inst = Inst0
    ;
        Inst0 = ground(_Uniq, _HOInstInfo),
        Groundness = inst_result_is_ground,
        ContainsAny = inst_result_does_not_contain_any,
        ContainsInstNames = inst_result_contains_instnames_known(set.init),
        ContainsTypes = inst_result_contains_types_known(set.init),
        Inst = Inst0
    ;
        Inst0 = any(_Uniq, _HOInstInfo),
        Groundness = inst_result_is_not_ground,
        ContainsAny = inst_result_does_contain_any,
        ContainsInstNames = inst_result_contains_instnames_known(set.init),
        ContainsTypes = inst_result_contains_types_known(set.init),
        Inst = Inst0
    ;
        Inst0 = not_reached,
        % These should be generated only by the compiler.
        unexpected($module, $pred, "Inst0 = not_reached")
    ;
        Inst0 = inst_var(_InstVar),
        % We should invoke pretest_inst only for inst definitions that
        % do NOT have parameters.
        unexpected($module, $pred, "Inst0 = inst_var")
    ;
        Inst0 = constrained_inst_vars(InstVars, SubInst0),
        pretest_inst(SubInst0, SubInst, UserInstTable0,
            Groundness, ContainsAny, ContainsInstNames, ContainsTypes,
            !MaybeInstDefnsMap),
        Inst = constrained_inst_vars(InstVars, SubInst)
    ;
        Inst0 = defined_inst(_InstName),
        % ZZZ
        Groundness = inst_result_groundness_unknown,
        ContainsAny = inst_result_contains_any_unknown,
        ContainsInstNames = inst_result_contains_instnames_unknown,
        ContainsTypes = inst_result_contains_types_unknown,
        Inst = Inst0
    ;
        Inst0 = abstract_inst(_SymName, _SubInsts),
        % These aren't supported.
        unexpected($module, $pred, "Inst0 = abstract_inst")
    ;
        Inst0 = bound(Uniq, _TestResults0, BoundInsts0),
        pretest_bound_insts(BoundInsts0, BoundInsts, UserInstTable0,
            inst_result_is_ground, Groundness,
            inst_result_does_not_contain_any, ContainsAny,
            inst_result_contains_instnames_known(set.init), ContainsInstNames,
            inst_result_contains_types_known(set.init), ContainsTypes,
            !MaybeInstDefnsMap),
        TestResults = inst_test_results(Groundness,
            ContainsAny, ContainsInstNames, ContainsTypes),
        Inst = bound(Uniq, TestResults, BoundInsts)
    ).

:- pred pretest_bound_insts(list(bound_inst)::in, list(bound_inst)::out,
    map(inst_id, hlds_inst_defn)::in,
    inst_result_groundness::in, inst_result_groundness::out,
    inst_result_contains_any::in, inst_result_contains_any::out,
    inst_result_contains_instnames::in, inst_result_contains_instnames::out,
    inst_result_contains_types::in, inst_result_contains_types::out,
    map(inst_id, maybe_user_inst)::in, map(inst_id, maybe_user_inst)::out)
    is det.

pretest_bound_insts([], [], _UserInstTable0,
        !Groundness, !ContainsAny, !ContainsInstNames, !ContainsTypes,
        !MaybeInstDefnsMap).
pretest_bound_insts([BoundInst0 | BoundInsts0], [BoundInst | BoundInsts],
        UserInstTable0,
        !Groundness, !ContainsAny, !ContainsInstNames, !ContainsTypes,
        !MaybeInstDefnsMap) :-
    BoundInst0 = bound_functor(ConsId, ArgInsts0),
    pretest_bound_inst_args(ArgInsts0, ArgInsts, UserInstTable0,
        !Groundness, !ContainsAny, !ContainsInstNames, !ContainsTypes,
        !MaybeInstDefnsMap),
    BoundInst = bound_functor(ConsId, ArgInsts),
    pretest_bound_insts(BoundInsts0, BoundInsts, UserInstTable0,
        !Groundness, !ContainsAny, !ContainsInstNames, !ContainsTypes,
        !MaybeInstDefnsMap).

:- pred pretest_bound_inst_args(list(mer_inst)::in, list(mer_inst)::out,
    map(inst_id, hlds_inst_defn)::in,
    inst_result_groundness::in, inst_result_groundness::out,
    inst_result_contains_any::in, inst_result_contains_any::out,
    inst_result_contains_instnames::in, inst_result_contains_instnames::out,
    inst_result_contains_types::in, inst_result_contains_types::out,
    map(inst_id, maybe_user_inst)::in, map(inst_id, maybe_user_inst)::out)
    is det.

pretest_bound_inst_args([], [], _UserInstTable0,
        !Groundness, !ContainsAny, !ContainsInstNames, !ContainsTypes,
        !MaybeInstDefnsMap).
pretest_bound_inst_args([ArgInst0 | ArgInsts0], [ArgInst | ArgInsts],
        UserInstTable0,
        !Groundness, !ContainsAny, !ContainsInstNames, !ContainsTypes,
        !MaybeInstDefnsMap) :-
    pretest_inst(ArgInst0, ArgInst, UserInstTable0,
        ArgGroundness, ArgContainsAny, ArgContainsInstNames, ArgContainsTypes,
        !MaybeInstDefnsMap),
    combine_groundness_results(ArgGroundness, !Groundness),
    combine_contains_any_results(ArgContainsAny, !ContainsAny),
    combine_contains_instnames_results(ArgContainsInstNames,
        !ContainsInstNames),
    combine_contains_types_results(ArgContainsTypes, !ContainsTypes),
    pretest_bound_inst_args(ArgInsts0, ArgInsts, UserInstTable0,
        !Groundness, !ContainsAny, !ContainsInstNames, !ContainsTypes,
        !MaybeInstDefnsMap).

:- pred combine_groundness_results(inst_result_groundness::in,
    inst_result_groundness::in, inst_result_groundness::out) is det.

combine_groundness_results(GroundnessA, GroundnessB, Groundness) :-
    (
        GroundnessA = inst_result_is_not_ground,
        Groundness = inst_result_is_not_ground
    ;
        GroundnessA = inst_result_is_ground,
        Groundness = GroundnessB
    ;
        GroundnessA = inst_result_groundness_unknown,
        (
            GroundnessB = inst_result_is_not_ground,
            Groundness = inst_result_is_not_ground
        ;
            ( GroundnessB = inst_result_is_ground
            ; GroundnessB = inst_result_groundness_unknown
            ),
            Groundness = inst_result_groundness_unknown
        )
    ).

:- pred combine_contains_any_results(inst_result_contains_any::in,
    inst_result_contains_any::in, inst_result_contains_any::out) is det.

combine_contains_any_results(ContainsAnyA, ContainsAnyB, ContainsAny) :-
    (
        ContainsAnyA = inst_result_does_contain_any,
        ContainsAny = inst_result_does_contain_any
    ;
        ContainsAnyA = inst_result_does_not_contain_any,
        ContainsAny = ContainsAnyB
    ;
        ContainsAnyA = inst_result_contains_any_unknown,
        (
            ContainsAnyB = inst_result_does_contain_any,
            ContainsAny = inst_result_does_contain_any
        ;
            ( ContainsAnyB = inst_result_does_not_contain_any
            ; ContainsAnyB = inst_result_contains_any_unknown
            ),
            ContainsAny = inst_result_contains_any_unknown
        )
    ).

:- pred combine_contains_instnames_results(inst_result_contains_instnames::in,
    inst_result_contains_instnames::in,
    inst_result_contains_instnames::out) is det.

combine_contains_instnames_results(ContainsInstNamesA, ContainsInstNamesB,
        ContainsInstNames) :-
    (
        ContainsInstNamesA = inst_result_contains_instnames_unknown,
        ContainsInstNames = inst_result_contains_instnames_unknown
    ;
        ContainsInstNamesA = inst_result_contains_instnames_known(InstNamesA),
        (
            ContainsInstNamesB = inst_result_contains_instnames_unknown,
            ContainsInstNames = inst_result_contains_instnames_unknown
        ;
            ContainsInstNamesB =
                inst_result_contains_instnames_known(InstNamesB),
            set.union(InstNamesA, InstNamesB, InstNames),
            ContainsInstNames =
                inst_result_contains_instnames_known(InstNames)
        )
    ).

:- pred combine_contains_types_results(inst_result_contains_types::in,
    inst_result_contains_types::in, inst_result_contains_types::out) is det.

combine_contains_types_results(ContainsTypesA, ContainsTypesB,
        ContainsTypes) :-
    (
        ContainsTypesA = inst_result_contains_types_unknown,
        ContainsTypes = inst_result_contains_types_unknown
    ;
        ContainsTypesA = inst_result_contains_types_known(TypesA),
        (
            ContainsTypesB = inst_result_contains_types_unknown,
            ContainsTypes = inst_result_contains_types_unknown
        ;
            ContainsTypesB = inst_result_contains_types_known(TypesB),
            set.union(TypesA, TypesB, Types),
            ContainsTypes = inst_result_contains_types_known(Types)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred record_user_inst_results(assoc_list(inst_id, maybe_user_inst)::in,
    assoc_list(inst_id, hlds_inst_defn)::out) is det.

record_user_inst_results([], []).
record_user_inst_results([MaybeInstPair | MaybeInstPairs],
        [InstDefnPair | InstDefnPairs]) :-
    MaybeInstPair = InstId - MaybeUserInst,
    (
        MaybeUserInst = user_inst_being_processed,
        unexpected($module, $pred, "MaybeUserInst = user_inst_being_processed")
    ;
        MaybeUserInst = processed_user_inst(UserInstDefn)
    ),
    InstDefnPair = InstId - UserInstDefn,
    record_user_inst_results(MaybeInstPairs, InstDefnPairs).

%-----------------------------------------------------------------------------%

:- pred output_user_inst_pair(pair(inst_id, hlds_inst_defn)::in,
    io::di, io::uo) is det.

output_user_inst_pair(InstId - InstDefn, !IO) :-
    InstId = inst_id(SymName, Arity),
    io.write_string(sym_name_to_string(SymName), !IO),
    io.write_string("/", !IO),
    io.write_int(Arity, !IO),
    io.write_string(" -> ", !IO),
    InstDefn = hlds_inst_defn(_VarSet, _Params, InstBody,
        _MaybeMatchingTypeCtors, _Context, _Status),
    io.write(InstBody, !IO),
    io.nl(!IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.inst_user.
%-----------------------------------------------------------------------------%
