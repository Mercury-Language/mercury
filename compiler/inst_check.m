%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2009, 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: inst_check.m.
% Main author: maclarty.
%
% This module exports a predicate that checks that each user defined inst is
% consistent with at least one type in scope.
%
% TODO:
%   If we find an inst that is not consistent with any of the types in scope,
%   except for one function symbol with a different arity, then we
%   should include this information in the warning message.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.inst_check.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module list.

    % This predicate issues a warning for each user defined bound inst
    % that is not consistent with at least one type in scope.
    %
:- pred check_insts_have_matching_types(module_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module string.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

%-----------------------------------------------------------------------------%

check_insts_have_matching_types(Module, !Specs) :-
    module_info_get_inst_table(Module, InstTable),
    inst_table_get_user_insts(InstTable, UserInstTable),
    user_inst_table_get_inst_defns(UserInstTable, InstDefs),
    module_info_get_type_table(Module, TypeTable),
    get_all_type_ctor_defns(TypeTable, TypeCtorsDefns),
    AllTypeDefs = assoc_list.values(TypeCtorsDefns),
    list.filter(type_is_user_visible(section_implementation), AllTypeDefs,
        UserVisibleTypeDefs),
    InstIdDefPairs = map.to_assoc_list(InstDefs),
    list.filter(inst_is_defined_in_current_module, InstIdDefPairs,
        InstIdDefPairsForCurrentModule),
    FunctorsToTypeDefs = index_types_by_unqualified_functors(
        UserVisibleTypeDefs),
    list.foldl(check_inst(FunctorsToTypeDefs), InstIdDefPairsForCurrentModule,
        !Specs).

    % Returns yes if a type definition with the given import status
    % is user visible in a section of the current module.
    %
:- func status_implies_type_defn_is_user_visible(section, import_status)
    = bool.

status_implies_type_defn_is_user_visible(Section, Status) = Visible :-
    (
        ( Status = status_imported(_)
        ; Status = status_exported
        ),
        Visible = yes
    ;
        ( Status = status_external(_)
        ; Status = status_abstract_imported
        ; Status = status_pseudo_imported
        ; Status = status_opt_imported
        ),
        Visible = no
    ;
        ( Status = status_opt_exported
        ; Status = status_abstract_exported
        ; Status = status_pseudo_exported
        ; Status = status_exported_to_submodules
        ; Status = status_local
        ),
        (
            Section = section_interface,
            Visible = no
        ;
            Section = section_implementation,
            Visible = yes
        )
    ).

:- pred inst_is_defined_in_current_module(pair(inst_id, hlds_inst_defn)::in)
    is semidet.

inst_is_defined_in_current_module(_ - InstDef) :-
    ImportStatus = InstDef ^ inst_status,
    status_defined_in_this_module(ImportStatus) = yes.

:- pred type_is_user_visible(section::in, hlds_type_defn::in) is semidet.

type_is_user_visible(Section, TypeDef) :-
    get_type_defn_status(TypeDef, ImportStatus),
    status_implies_type_defn_is_user_visible(Section, ImportStatus) = yes.

:- type functors_to_types == multi_map(sym_name_and_arity, hlds_type_defn).

:- type bound_inst_functor
    --->    bif_name_and_arity(sym_name, arity)
    ;       bif_tuple(arity)
    ;       bif_int_constant
    ;       bif_float_constant
    ;       bif_char_constant
    ;       bif_string_constant.

:- type type_defn_or_builtin
    --->    type_def(hlds_type_defn)
    ;       type_builtin(builtin_type)
    ;       type_tuple(arity).

:- pred check_inst(functors_to_types::in, pair(inst_id, hlds_inst_defn)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_inst(FunctorsToTypes, InstId - InstDef, !Specs) :-
    InstBody = InstDef ^ inst_body,
    (
        InstBody = eqv_inst(Inst),
        (
            Inst = bound(_, _, BoundInsts),
            (
                list.map(bound_inst_to_functor, BoundInsts, Functors)
            ->
                list.map(find_types_for_functor(FunctorsToTypes),
                    Functors, MatchingTypeLists),
                maybe_issue_inst_check_warning(InstId, InstDef,
                    MatchingTypeLists, !Specs)
            ;
                true
            )
        ;
            ( Inst = any(_, _)
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
    list(list(type_defn_or_builtin))::in,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_issue_inst_check_warning(InstId, InstDef, MatchingTypeLists, !Specs) :-
    InstImportStatus = InstDef ^ inst_status,
    InstIsExported = status_is_exported_to_non_submodules(InstImportStatus),
    (
        MatchingTypeLists = [MatchingTypeList | _],
        not (some [Type] (
            % Check at least one type matched all the functors of the inst.
            list.member(Type, MatchingTypeList),
            type_matched_all_functors(Type, MatchingTypeLists),

            % If the inst is exported then that type must be concrete outside
            % of this module.
            (
                InstIsExported = yes,
                (
                    Type = type_def(TypeDefn),
                    type_is_user_visible(section_interface, TypeDefn)
                ;
                    Type = type_builtin(_)
                ;
                    Type = type_tuple(_)
                )
            ;
                InstIsExported = no
            )
        ))
    ->
        Context = InstDef ^ inst_context,
        InstId = inst_id(InstName, InstArity),
        Pieces = [words("Warning: inst "),
            sym_name_and_arity(InstName / InstArity),
            words("does not match any of the types in scope.")],
        Spec = error_spec(severity_warning, phase_inst_check,
            [simple_msg(Context, [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ;
        true
    ).

:- pred type_matched_all_functors(type_defn_or_builtin::in,
    list(list(type_defn_or_builtin))::in) is semidet.

type_matched_all_functors(Type, MatchingTypeLists) :-
    all [TypeList] (
        list.member(TypeList, MatchingTypeLists)
    =>
        list.member(Type, TypeList)
    ).

:- pred find_types_for_functor(functors_to_types::in, bound_inst_functor::in,
    list(type_defn_or_builtin)::out) is det.

find_types_for_functor(FunctorsToTypes, Functor, Types) :-
    (
        Functor = bif_name_and_arity(Name, Arity),
        (
            multi_map.search(FunctorsToTypes, strip_qualifiers(Name) / Arity,
                TypeDefs)
        ->
            TypesExceptChar = list.map(func(TypeDef) = type_def(TypeDef),
                TypeDefs)
        ;
            TypesExceptChar = []
        ),
        (
            % Zero arity functors with length 1 could match the builtin
            % character type.
            Name = unqualified(NameStr),
            string.count_codepoints(NameStr) = 1
        ->
            TypesExceptTuple = [type_builtin(builtin_type_char)
                | TypesExceptChar]
        ;
            TypesExceptTuple = TypesExceptChar
        ),
        (
            % The inst could match a tuple type, which won't be explicitly
            % declared.
            type_ctor_is_tuple(type_ctor(Name, Arity))
        ->
            Types = [type_tuple(Arity) | TypesExceptTuple]
        ;
            Types = TypesExceptTuple
        )
    ;
        Functor = bif_tuple(Arity),
        Types = [type_tuple(Arity)]
    ;
        Functor = bif_int_constant,
        Types = [type_builtin(builtin_type_int)]
    ;
        Functor = bif_float_constant,
        Types = [type_builtin(builtin_type_float)]
    ;
        Functor = bif_char_constant,
        Types = [type_builtin(builtin_type_char)]
    ;
        Functor = bif_string_constant,
        Types = [type_builtin(builtin_type_string)]
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
        ConsId = cons(Name, Arity, _),
        MaybeFunctor = yes(bif_name_and_arity(Name, Arity))
    ;
        ConsId = tuple_cons(Arity),
        MaybeFunctor = yes(bif_tuple(Arity))
    ;
        ConsId = int_const(_),
        MaybeFunctor = yes(bif_int_constant)
    ;
        ConsId = float_const(_),
        MaybeFunctor = yes(bif_float_constant)
    ;
        ConsId = char_const(_),
        MaybeFunctor = yes(bif_char_constant)
    ;
        ConsId = string_const(_),
        MaybeFunctor = yes(bif_string_constant)
    ;
        ( ConsId = closure_cons(_, _)
        ; ConsId = impl_defined_const(_)
        ; ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = type_info_const(_)
        ; ConsId = typeclass_info_const(_)
        ; ConsId = ground_term_const(_, _)
        ; ConsId = tabling_info_const(_)
        ; ConsId = deep_profiling_proc_layout(_)
        ; ConsId = table_io_entry_desc(_)
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
        TypeDefBody = hlds_du_type(Constructors, _, _, _, _, _, _, _, _),
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

constructor_to_sym_name_and_arity(ctor(_, _, Name, Args, _)) =
    Name / list.length(Args).

    % multi_map_set is the same as multi_map.set, except that the arguments are
    % in an order convenient for use in index_types_by_unqualified_functors.
    %
:- func multi_map_set(V, K, multi_map(K, V)) = multi_map(K, V).

multi_map_set(Value, Key, Map) = multi_map.set(Map, Key, Value).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.inst_check.
%-----------------------------------------------------------------------------%
