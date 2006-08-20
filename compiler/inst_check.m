%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: inst_check.m.
% Main author: maclarty.
%
% This module exports a predicate that checks that each user defined inst is
% consistant with at least one type in scope.
%
% TODO:
%   If we find an inst that is not consistent with any of the types in scope,
%   except for one function symbol with a different arity, then we
%   should include this information in the warning message.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.inst_check.
:- interface.

:- import_module io.

:- import_module hlds.hlds_module.

    % This predicate issues a warning for each user defined bound insts
    % that is not consistant with at least one type in scope.
    %
:- pred check_insts_have_matching_types(module_info::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module string.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

check_insts_have_matching_types(Module, !IO) :-
    module_info_get_inst_table(Module, InstTable),
    inst_table_get_user_insts(InstTable, UserInstTable),
    user_inst_table_get_inst_defns(UserInstTable, InstDefs),
    module_info_get_type_table(Module, TypeTable),
    AllTypeDefs = map.values(TypeTable),
    list.filter(type_is_user_visible, AllTypeDefs, UserVisibleTypeDefs),
    InstIdDefPairs = map.to_assoc_list(InstDefs),
    list.filter(inst_is_defined_in_current_module, InstIdDefPairs,
        InstIdDefPairsForCurrentModule),
    FunctorsToTypeDefs = index_types_by_unqualified_functors(
        UserVisibleTypeDefs),
    list.foldl_corresponding(check_inst(FunctorsToTypeDefs),
        assoc_list.keys(InstIdDefPairsForCurrentModule),
        assoc_list.values(InstIdDefPairsForCurrentModule), !IO).

    % Returns yes if a type definition with the given import status
    % is user visible in the current module.
    %
:- func status_implies_type_defn_is_user_visible(import_status) = bool.

status_implies_type_defn_is_user_visible(status_imported(_)) =             yes.
status_implies_type_defn_is_user_visible(status_external(_)) =             no.
status_implies_type_defn_is_user_visible(status_abstract_imported) =       no.
status_implies_type_defn_is_user_visible(status_pseudo_imported) =         no.
status_implies_type_defn_is_user_visible(status_opt_imported) =            no.
status_implies_type_defn_is_user_visible(status_exported) =                yes.
status_implies_type_defn_is_user_visible(status_opt_exported) =            yes.
status_implies_type_defn_is_user_visible(status_abstract_exported) =       yes.
status_implies_type_defn_is_user_visible(status_pseudo_exported) =         yes.
status_implies_type_defn_is_user_visible(status_exported_to_submodules) =  yes.
status_implies_type_defn_is_user_visible(status_local) =                   yes.

:- pred inst_is_defined_in_current_module(pair(inst_id, hlds_inst_defn)::in)
    is semidet.

inst_is_defined_in_current_module(_ - InstDef) :-
    ImportStatus = InstDef ^ inst_status,
    status_defined_in_this_module(ImportStatus) = yes.

:- pred type_is_user_visible(hlds_type_defn::in) is semidet.

type_is_user_visible(TypeDef) :-
    get_type_defn_status(TypeDef, ImportStatus),
    status_implies_type_defn_is_user_visible(ImportStatus) = yes.

:- type functors_to_types == multi_map(sym_name_and_arity, hlds_type_defn).

:- type bound_inst_functor
    --->    name_and_arity(sym_name, arity)
    ;       string_constant
    ;       int_constant
    ;       float_constant.

:- type type_defn_or_builtin
    --->    type_def(hlds_type_defn)
    ;       builtin_type(builtin_type)
    ;       tuple(arity).

:- pred check_inst(functors_to_types::in, inst_id::in, hlds_inst_defn::in,
    io::di, io::uo) is det.

check_inst(FunctorsToTypes, InstId, InstDef, !IO) :-
    InstBody = InstDef ^ inst_body,
    (
        InstBody = eqv_inst(Inst),
        (
            Inst = bound(_, BoundInsts),
            (
                list.map(bound_inst_to_functor, BoundInsts, Functors)
            ->
                list.map(find_types_for_functor(FunctorsToTypes),
                    Functors, MatchingTypeLists),
                maybe_issue_inst_check_warning(InstId, InstDef,
                    MatchingTypeLists, !IO)
            ;
                true
            )
        ;
            ( Inst = any(_)
            ; Inst = free
            ; Inst = free(_)
            ; Inst = ground(_, _)
            ; Inst = not_reached
            ; Inst = inst_var(_)
            ; Inst = constrained_inst_vars(_, _)
            ; Inst = defined_inst(_)
            ; Inst = abstract_inst(_, _)
            )
        )
    ;
        InstBody = abstract_inst
    ).

:- pred maybe_issue_inst_check_warning(inst_id::in, hlds_inst_defn::in,
    list(list(type_defn_or_builtin))::in, io::di, io::uo) is det.

maybe_issue_inst_check_warning(InstId, InstDef, MatchingTypeLists,
        !IO) :-
    ( if
        MatchingTypeLists = [MatchingTypeList | _],
        not (some [Type] (
            list.member(Type, MatchingTypeList),
            all [TypeList] (
                list.member(TypeList, MatchingTypeLists)
            =>
                list.member(Type, TypeList)
            )
        ))
    then
        Context = InstDef ^ inst_context,
        InstId = inst_id(InstName, InstArity),
        Warning = [
            words("Warning: inst "),
            sym_name_and_arity(InstName / InstArity),
            words("does not match any of the types in scope.")
        ],
        report_warning(Context, 0, Warning, !IO)
    else
        true
    ).

:- pred find_types_for_functor(functors_to_types::in, bound_inst_functor::in,
    list(type_defn_or_builtin)::out) is det.

find_types_for_functor(FunctorsToTypes, Functor, Types) :-
    (
        Functor = name_and_arity(Name, Arity),
        ( multi_map.search(FunctorsToTypes, strip_qualifiers(Name) / Arity,
                TypeDefs) ->
            TypesExceptChar = list.map(func(TypeDef) = type_def(TypeDef),
                TypeDefs)
        ;
            TypesExceptChar = []
        ),
        (
            %
            % Zero arity functors with length 1 could match the
            % character builtin type.
            %
            Name = unqualified(NameStr),
            string.length(NameStr) = 1
        ->
            TypesExceptTuple = [builtin_type(character) | TypesExceptChar]
        ;
            TypesExceptTuple = TypesExceptChar
        ),
        (
            %
            % The inst could match a tuple type, which won't be explicitly
            % declared.
            %
            type_ctor_is_tuple(type_ctor(Name, Arity))
        ->
            Types = [tuple(Arity) | TypesExceptTuple]
        ;
            Types = TypesExceptTuple
        )
    ;
        Functor = int_constant,
        Types = [builtin_type(int)]
    ;
        Functor = float_constant,
        Types = [builtin_type(float)]
    ;
        Functor = string_constant,
        Types = [builtin_type(string)]
    ).

:- pred bound_inst_to_functor(bound_inst::in, bound_inst_functor::out)
    is semidet.

bound_inst_to_functor(bound_functor(ConsId, _), Functor) :-
    get_functor_if_must_check_for_type(ConsId, yes(Functor)).

    % Return the functor for the given cons_id if we should look for
    % matching types for the cons_id.
    % We don't bother checking for types for certain cons_ids such as
    % predicate signatures and cons_ids that are only used internally.
    %
:- pred get_functor_if_must_check_for_type(cons_id::in,
    maybe(bound_inst_functor)::out) is det.

get_functor_if_must_check_for_type(ConsId, MaybeFunctor) :-
    (
        ConsId = cons(Name, Arity),
        MaybeFunctor = yes(name_and_arity(Name, Arity))
    ;
        ConsId = int_const(_),
        MaybeFunctor = yes(int_constant)
    ;
        ConsId = string_const(_),
        MaybeFunctor = yes(string_constant)
    ;
        ConsId = float_const(_),
        MaybeFunctor = yes(float_constant)
    ;
        ( ConsId = pred_const(_, __)
        ; ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = tabling_info_const(_)
        ; ConsId = deep_profiling_proc_layout(_)
        ; ConsId = table_io_decl(_)
        ),
        MaybeFunctor = no
    ).

:- func index_types_by_unqualified_functors(list(hlds_type_defn)) =
    functors_to_types.

index_types_by_unqualified_functors([]) = multi_map.init.
index_types_by_unqualified_functors([TypeDef | TypeDefs]) =
        FunctorsToTypeDefs :-
    FunctorsToTypeDefs0 = index_types_by_unqualified_functors(TypeDefs),
    Functors = get_du_functors_for_type_def(TypeDef),
    UnqualifiedFunctors = list.map(
        ( func(Name / Arity) = strip_qualifiers(Name) / Arity ), Functors),
    FunctorsToTypeDefs = list.foldl(multi_map_set(TypeDef),
        UnqualifiedFunctors, FunctorsToTypeDefs0).

:- func strip_qualifiers(sym_name) = sym_name.

strip_qualifiers(unqualified(Name)) = unqualified(Name).
strip_qualifiers(qualified(_, Name)) = unqualified(Name).

:- func get_du_functors_for_type_def(hlds_type_defn) =
    list(sym_name_and_arity).

get_du_functors_for_type_def(TypeDef) = Functors :-
    get_type_defn_body(TypeDef, TypeDefBody),
    (
        TypeDefBody = hlds_du_type(Constructors, _, _, _, _, _),
        Functors = list.map(constructor_to_sym_name_and_arity, Constructors)
    ;
        ( TypeDefBody = hlds_eqv_type(_)
        ; TypeDefBody = hlds_foreign_type(_)
        ; TypeDefBody = hlds_solver_type(_, _)
        ; TypeDefBody = hlds_abstract_type(_)
        ),
        Functors = []
    ).

:- func constructor_to_sym_name_and_arity(constructor) = sym_name_and_arity.

constructor_to_sym_name_and_arity(ctor(_, _, Name, Args)) =
    Name / list.length(Args).

    % multi_map_set is the same as multi_map.set, except that the arguments are
    % in an order convenient for use in index_types_by_unqualified_functors.
    %
:- func multi_map_set(V, K, multi_map(K, V)) = multi_map(K, V).

multi_map_set(Value, Key, Map) = multi_map.set(Map, Key, Value).

:- end_module check_hlds.inst_check.
