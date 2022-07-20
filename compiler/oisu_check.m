%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: oisu_check.m.
% Main author: zs.
%
% This module checks whether the oisu (order independent state update) pragmas
% (if any) that are present in the module being compiled satisfy the
% requirements on them.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.oisu_check.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.

    % XXX document me
    %
:- pred check_oisu_pragmas_for_module(assoc_list(type_ctor, oisu_preds)::in,
    module_info::in, module_info::out, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_test.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_name.
:- import_module hlds.status.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

check_oisu_pragmas_for_module(OISUPairs, !ModuleInfo, Specs) :-
    map.init(KindMap0),
    list.foldl(add_type_ctor_to_kind_map, OISUPairs, KindMap0, KindMap),
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    map.to_assoc_list(PredIdTable0, PredIdsInfos0),
    assoc_list.keys(OISUPairs, OISUTypeCtors),
    list.map_foldl2(
        check_local_oisu_pred(!.ModuleInfo, KindMap, OISUTypeCtors),
        PredIdsInfos0, PredIdsInfos, set.init, OISUProcs, [], Specs),
    map.from_assoc_list(PredIdsInfos, PredIdTable),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo),
    module_info_set_oisu_procs(OISUProcs, !ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred add_type_ctor_to_kind_map(pair(type_ctor, oisu_preds)::in,
    oisu_kind_map::in, oisu_kind_map::out) is det.

add_type_ctor_to_kind_map(TypeCtor - OISUPreds, !KindMap) :-
    OISUPreds = oisu_preds(CreatorPreds, MutatorPreds, DestructorPreds),
    list.foldl(add_pred_to_kind_map(TypeCtor, oisu_creator),
        CreatorPreds, !KindMap),
    list.foldl(add_pred_to_kind_map(TypeCtor, oisu_mutator),
        MutatorPreds, !KindMap),
    list.foldl(add_pred_to_kind_map(TypeCtor, oisu_destructor),
        DestructorPreds, !KindMap).

:- type oisu_pred_kind
    --->    oisu_creator
    ;       oisu_mutator
    ;       oisu_destructor.

:- type oisu_kind_map == map(pred_id, list(oisu_pred_kind_for)).

:- pred add_pred_to_kind_map(type_ctor::in, oisu_pred_kind::in,
    pred_id::in, oisu_kind_map::in, oisu_kind_map::out) is det.

add_pred_to_kind_map(TypeCtor, Kind, PredId, !KindMap) :-
    (
        Kind = oisu_creator,
        KindFor = oisu_creator_for(TypeCtor)
    ;
        Kind = oisu_mutator,
        KindFor = oisu_mutator_for(TypeCtor)
    ;
        Kind = oisu_destructor,
        KindFor = oisu_destructor_for(TypeCtor)
    ),
    ( if map.search(!.KindMap, PredId, OldEntries) then
        Entries = [KindFor | OldEntries],
        map.det_update(PredId, Entries, !KindMap)
    else
        Entries = [KindFor],
        map.det_insert(PredId, Entries, !KindMap)
    ).

%-----------------------------------------------------------------------------%

:- pred check_local_oisu_pred(module_info::in, oisu_kind_map::in,
    list(type_ctor)::in,
    pair(pred_id, pred_info)::in, pair(pred_id, pred_info)::out,
    set(pred_proc_id)::in, set(pred_proc_id)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_local_oisu_pred(ModuleInfo, KindMap, OISUTypeCtors, Pair0, Pair,
        !OISUProcs, !Specs) :-
    Pair0 = PredId - PredInfo0,
    pred_info_get_status(PredInfo0, Status0),
    ( if Status0 = pred_status(status_external(StatusPrime)) then
        Status = pred_status(StatusPrime)
    else
        Status = Status0
    ),
    IsDefnInModule = pred_status_defined_in_this_module(Status),
    (
        IsDefnInModule = no,
        Pair = Pair0
    ;
        IsDefnInModule = yes,
        ( if map.search(KindMap, PredId, KindFors) then
            pred_info_get_proc_table(PredInfo0, ProcTable0),
            map.to_assoc_list(ProcTable0, Procs0),
            (
                Procs0 = [],
                unexpected($pred, "no procedure for local predicate")
            ;
                Procs0 = [ProcId - ProcInfo0],
                pred_info_get_arg_types(PredInfo0, ArgTypes),
                proc_info_get_argmodes(ProcInfo0, ArgModes),
                assoc_list.from_corresponding_lists(ArgTypes, ArgModes,
                    ArgTypesModes),
                check_arg_oisu_types(ModuleInfo, PredInfo0, KindFors,
                    OISUTypeCtors, 1, [], ArgTypesModes, !Specs),
                proc_info_set_oisu_kind_fors(KindFors, ProcInfo0, ProcInfo),
                Procs = [ProcId - ProcInfo],
                map.from_assoc_list(Procs, ProcTable),
                pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
                Pair = PredId - PredInfo,
                set.insert(proc(PredId, ProcId), !OISUProcs)
            ;
                Procs0 = [_, _ | _],
                PredDesc = describe_one_pred_info_name(
                    should_not_module_qualify, PredInfo0),
                ProcsPieces = PredDesc ++ [words("is mentioned"),
                    words("in a"), pragma_decl("oisu"), words("declaration,"),
                    words("so it should have exactly one procedure."), nl],
                pred_info_get_context(PredInfo0, Context),
                ProcsSpec = simplest_spec($pred, severity_error,
                    phase_oisu_check, Context, ProcsPieces),
                !:Specs = [ProcsSpec | !.Specs],
                Pair = Pair0
            )
        else
            pred_info_get_origin(PredInfo0, Origin),
            ( if Origin = origin_special_pred(_, _) then
                true
            else
                pred_info_get_arg_types(PredInfo0, ArgTypes),
                check_args_have_no_oisu_types(PredInfo0, OISUTypeCtors,
                    ArgTypes, !Specs)
            ),
            Pair = Pair0
        )
    ).

%-----------------------------------------------------------------------------%

:- pred check_arg_oisu_types(module_info::in, pred_info::in,
    list(oisu_pred_kind_for)::in, list(type_ctor)::in, int::in,
    list(type_ctor)::in, assoc_list(mer_type, mer_mode)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_arg_oisu_types(ModuleInfo, PredInfo, KindFors, OISUTypeCtors, ArgNum,
        !.HandledOISUTypeCtors, [TypeMode | TypesModes], !Specs) :-
    ( if
        TypeMode = Type - Mode,
        type_to_ctor_and_args(Type, TypeCtor, ArgTypes),
        list.member(TypeCtor, OISUTypeCtors)
    then
        (
            ArgTypes = []
        ;
            ArgTypes = [_ | _],
            unexpected($pred, "ArgTypes != []")
        ),
        ( if find_kind_for_oisu_type(KindFors, TypeCtor, ThisKind) then
            ( if list.member(TypeCtor, !.HandledOISUTypeCtors) then
                DupPredDesc = describe_one_pred_info_name(
                    should_not_module_qualify, PredInfo),
                DupPieces = [words("The"), nth_fixed(ArgNum),
                    words("argument of")] ++ DupPredDesc ++
                    [words("handles a previous handled OISU type."), nl],
                pred_info_get_context(PredInfo, DupContext),
                DupSpec = simplest_spec($pred, severity_error,
                    phase_oisu_check, DupContext, DupPieces),
                !:Specs = [DupSpec | !.Specs],
                RestArgNum = ArgNum + 1,
                RestTypesModes = TypesModes
            else
                !:HandledOISUTypeCtors = [TypeCtor | !.HandledOISUTypeCtors],
                (
                    ThisKind = oisu_creator,
                    ( if mode_is_output(ModuleInfo, Mode) then
                        true
                    else
                        PredDesc = describe_one_pred_info_name(
                            should_not_module_qualify, PredInfo),
                        Pieces = [words("The"), nth_fixed(ArgNum),
                            words("argument of")] ++ PredDesc ++
                            [words("should be a creator of its OISU type,"),
                            words("but its mode is not output."), nl],
                        pred_info_get_context(PredInfo, Context),
                        Spec = simplest_spec($pred, severity_error,
                            phase_oisu_check, Context, Pieces),
                        !:Specs = [Spec | !.Specs]
                    ),
                    RestArgNum = ArgNum + 1,
                    RestTypesModes = TypesModes
                ;
                    ThisKind = oisu_mutator,
                    ( if
                        TypesModes = [NextTypeMode | TailTypesModes],
                        NextTypeMode = NextType - NextMode,
                        NextType = Type
                    then
                        ( if mode_is_input(ModuleInfo, Mode) then
                            true
                        else
                            InPredDesc = describe_one_pred_info_name(
                                should_not_module_qualify, PredInfo),
                            InPieces = [words("The"), nth_fixed(ArgNum),
                                words("argument of")] ++ InPredDesc ++
                                [words("should be the input of the mutator"),
                                words("of its OISU type,"),
                                words("but its mode is not input."), nl],
                            pred_info_get_context(PredInfo, InContext),
                            InSpec = simplest_spec($pred, severity_error,
                                phase_oisu_check, InContext, InPieces),
                            !:Specs = [InSpec | !.Specs]
                        ),
                        ( if mode_is_output(ModuleInfo, NextMode) then
                            true
                        else
                            OutPredDesc = describe_one_pred_info_name(
                                should_not_module_qualify, PredInfo),
                            OutPieces = [words("The"), nth_fixed(ArgNum + 1),
                                words("argument of")] ++ OutPredDesc ++
                                [words("should be the output of the mutator"),
                                words("of its OISU type,"),
                                words("but its mode is not output."), nl],
                            pred_info_get_context(PredInfo, OutContext),
                            OutSpec = simplest_spec($pred, severity_error,
                                phase_oisu_check, OutContext, OutPieces),
                            !:Specs = [OutSpec | !.Specs]
                        ),
                        RestArgNum = ArgNum + 2,
                        RestTypesModes = TailTypesModes
                    else
                        PredDesc = describe_one_pred_info_name(
                            should_not_module_qualify, PredInfo),
                        Pieces = [words("Since the"), nth_fixed(ArgNum),
                            words("argument of")] ++ PredDesc ++
                            [words("is a mutator of its OISU type,"),
                            words("it should be followed by"),
                            words("another argument of the same type."), nl],
                        pred_info_get_context(PredInfo, Context),
                        Spec = simplest_spec($pred, severity_error,
                            phase_oisu_check, Context, Pieces),
                        !:Specs = [Spec | !.Specs],
                        RestArgNum = ArgNum + 1,
                        RestTypesModes = TypesModes
                    )
                ;
                    ThisKind = oisu_destructor,
                    ( if mode_is_input(ModuleInfo, Mode) then
                        true
                    else
                        PredDesc = describe_one_pred_info_name(
                            should_not_module_qualify, PredInfo),
                        Pieces = [words("The"), nth_fixed(ArgNum),
                            words("argument of")] ++ PredDesc ++
                            [words("should be a destructor of its OISU type,"),
                            words("but its mode is not input."), nl],
                        pred_info_get_context(PredInfo, Context),
                        Spec = simplest_spec($pred, severity_error,
                            phase_oisu_check, Context, Pieces),
                        !:Specs = [Spec | !.Specs]
                    ),
                    RestArgNum = ArgNum + 1,
                    RestTypesModes = TypesModes
                )
            ),
            check_arg_oisu_types(ModuleInfo, PredInfo, KindFors, OISUTypeCtors,
                RestArgNum, !.HandledOISUTypeCtors, RestTypesModes, !Specs)
        else
            PredDesc = describe_one_pred_info_name(should_not_module_qualify,
                PredInfo),
            Pieces = [words("The"), nth_fixed(ArgNum), words("argument of")] ++
                PredDesc ++ [words("is an OISU type"),
                words("but it is not listed in that type's OISU pragma."), nl],
            pred_info_get_context(PredInfo, Context),
            Spec = simplest_spec($pred, severity_error, phase_oisu_check,
                Context, Pieces),
            !:Specs = [Spec | !.Specs],
            check_arg_oisu_types(ModuleInfo, PredInfo, KindFors, OISUTypeCtors,
                ArgNum + 1, !.HandledOISUTypeCtors, TypesModes, !Specs)
        )
    else
        check_arg_oisu_types(ModuleInfo, PredInfo, KindFors, OISUTypeCtors,
            ArgNum + 1, !.HandledOISUTypeCtors, TypesModes, !Specs)
    ).
check_arg_oisu_types(_ModuleInfo, PredInfo, KindFors, _OISUTypeCtors,
        _ArgNum, !.HandledOISUTypeCtors, [], !Specs) :-
    find_unhandled_oisu_kind_fors(KindFors, !.HandledOISUTypeCtors,
        UnhandledKindFors),
    (
        UnhandledKindFors = []
    ;
        UnhandledKindFors = [HeadUnhandledKindFor | TailUnhandledKindFors],
        describe_unhandled_kind_fors(HeadUnhandledKindFor,
            TailUnhandledKindFors, UnhandledPieces),
        PredDesc = describe_one_pred_info_name(should_not_module_qualify,
            PredInfo),
        Pieces = PredDesc ++ [words("is declared to handle"),
            words("the following OISU types, but it does not:"),
            nl_indent_delta(1)] ++ UnhandledPieces,
        pred_info_get_context(PredInfo, Context),
        Spec = simplest_spec($pred, severity_error, phase_oisu_check,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred find_kind_for_oisu_type(list(oisu_pred_kind_for)::in, type_ctor::in,
    oisu_pred_kind::out) is semidet.

find_kind_for_oisu_type([KindFor | KindFors], TypeCtor, Kind) :-
    ( if
        (
            KindFor = oisu_creator_for(TypeCtor),
            KindPrime = oisu_creator
        ;
            KindFor = oisu_mutator_for(TypeCtor),
            KindPrime = oisu_mutator
        ;
            KindFor = oisu_destructor_for(TypeCtor),
            KindPrime = oisu_destructor
        )
    then
        Kind = KindPrime
    else
        find_kind_for_oisu_type(KindFors, TypeCtor, Kind)
    ).

:- pred find_unhandled_oisu_kind_fors(list(oisu_pred_kind_for)::in,
    list(type_ctor)::in, list(oisu_pred_kind_for)::out) is det.

find_unhandled_oisu_kind_fors([], _, []).
find_unhandled_oisu_kind_fors([KindFor | KindFors], HandledOISUTypeCtors,
        UnhandledKindFors) :-
    find_unhandled_oisu_kind_fors(KindFors, HandledOISUTypeCtors,
        UnhandledKindForsTail),
    ( KindFor = oisu_creator_for(TypeCtor)
    ; KindFor = oisu_mutator_for(TypeCtor)
    ; KindFor = oisu_destructor_for(TypeCtor)
    ),
    ( if list.member(TypeCtor, HandledOISUTypeCtors) then
        UnhandledKindFors = UnhandledKindForsTail
    else
        UnhandledKindFors = [KindFor | UnhandledKindForsTail]
    ).

:- pred describe_unhandled_kind_fors(
    oisu_pred_kind_for::in, list(oisu_pred_kind_for)::in,
    list(format_component)::out) is det.

describe_unhandled_kind_fors(HeadKindFor, TailKindFors, Pieces) :-
    ( HeadKindFor = oisu_creator_for(HeadTypeCtor), HeadKind = "creator"
    ; HeadKindFor = oisu_mutator_for(HeadTypeCtor), HeadKind = "mutator"
    ; HeadKindFor = oisu_destructor_for(HeadTypeCtor), HeadKind = "destructor"
    ),
    HeadTypeCtor = type_ctor(HeadTypeSymName, HeadTypeArity),
    HeadPieces0 =
        [unqual_sym_name_arity(
            sym_name_arity(HeadTypeSymName, HeadTypeArity)),
        fixed("(as " ++ HeadKind ++ ")")],
    (
        TailKindFors = [],
        HeadPieces = HeadPieces0 ++ [suffix("."), nl],
        Pieces = HeadPieces
    ;
        TailKindFors = [HeadTailKindFor | TailTailKindFors],
        HeadPieces = HeadPieces0 ++ [suffix(","), nl],
        describe_unhandled_kind_fors(HeadTailKindFor, TailTailKindFors,
            TailPieces),
        Pieces = HeadPieces ++ TailPieces
    ).

%-----------------------------------------------------------------------------%

:- pred check_args_have_no_oisu_types(pred_info::in, list(type_ctor)::in,
    list(mer_type)::in, list(error_spec)::in, list(error_spec)::out) is det.

check_args_have_no_oisu_types(_PredInfo, _OISUTypeCtors, [], !Specs).
check_args_have_no_oisu_types(PredInfo, OISUTypeCtors, [Type | Types],
        !Specs) :-
    ( if
        type_to_ctor_and_args(Type, TypeCtor, ArgTypes),
        ArgTypes = [],
        list.member(TypeCtor, OISUTypeCtors)
    then
        PredDesc = describe_one_pred_info_name(should_not_module_qualify,
            PredInfo),
        TypeCtor = type_ctor(TypeName, TypeArity),
        ProcsPieces = PredDesc ++ [words("is not mentioned"),
            words("in the"), pragma_decl("oisu"), words("declaration"),
            words("as a predicate that handles values of the type"),
            unqual_sym_name_arity(sym_name_arity(TypeName, TypeArity)),
            suffix("."), nl],
        pred_info_get_context(PredInfo, Context),
        ProcsSpec = simplest_spec($pred, severity_error, phase_oisu_check,
            Context, ProcsPieces),
        !:Specs = [ProcsSpec | !.Specs]
    else
        true
    ),
    check_args_have_no_oisu_types(PredInfo, OISUTypeCtors, Types, !Specs).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.oisu_check.
%-----------------------------------------------------------------------------%
