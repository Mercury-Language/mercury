%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2009, 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: inst_check.m.
% Original author: maclarty.
% Rewritten by zs.
%
% This module exports a predicate that checks that each user defined inst is
% consistent with at least one type in scope.
%
% TODO
% The code in this module checks only that the cons_ids in the sequence of
% bound_inst at the *top level* match the function symbols of a type.
% It does not check whether any bound_insts that may appear among the
% arguments of the cons_ids in those bound_insts match the function symbols
% of the applicable argument types. For example, given the types
%
% :- type f
%   --->    f1(g)
%   ;       f2.
%
% :- type g
%   --->    g1
%   ;       g2
%
% the code in this module will accept
%
%   bound_functor(f1,
%       [bound(...,
%           [bound_functor(h1, [])])
%       ])
%
% as a valid body for an inst definition, even though h1 is *not* among
% the function symbols of type g.
%
%---------------------------------------------------------------------------%

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
:- pred check_insts_have_matching_types(module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_inst_mode.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.      % undesirable dependency
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

check_insts_have_matching_types(!ModuleInfo, !Specs) :-
    module_info_get_inst_table(!.ModuleInfo, InstTable0),
    inst_table_get_user_insts(InstTable0, UserInstTable0),
    map.to_sorted_assoc_list(UserInstTable0, InstCtorDefnPairs0),
    module_info_get_type_table(!.ModuleInfo, TypeTable),
    get_all_type_ctor_defns(TypeTable, TypeCtorsDefns),
    index_visible_types_by_unqualified_functors(TypeCtorsDefns,
        multi_map.init, FunctorsToTypeDefns),
    check_inst_defns_have_matching_types(TypeTable, FunctorsToTypeDefns,
        InstCtorDefnPairs0, InstCtorDefnPairs, !Specs),
    map.from_sorted_assoc_list(InstCtorDefnPairs, UserInstTable),
    inst_table_set_user_insts(UserInstTable, InstTable0, InstTable),
    module_info_set_inst_table(InstTable, !ModuleInfo).

%---------------------------------------------------------------------------%

:- type functor_name_and_arity
    --->    functor_name_and_arity(string, int).

:- type type_ctor_and_defn
    --->    type_ctor_and_defn(type_ctor, hlds_type_defn).

:- type functors_to_types_map ==
    multi_map(functor_name_and_arity, type_ctor_and_defn).

:- pred index_visible_types_by_unqualified_functors(
    assoc_list(type_ctor, hlds_type_defn)::in,
    functors_to_types_map::in, functors_to_types_map::out) is det.

index_visible_types_by_unqualified_functors([], !FunctorsToTypesMap).
index_visible_types_by_unqualified_functors([TypeCtorDefn | TypeCtorDefns],
        !FunctorsToTypesMap) :-
    TypeCtorDefn = TypeCtor - TypeDefn,
    ( if type_is_user_visible(ms_implementation, TypeDefn) then
        TypeCtorAndDefn = type_ctor_and_defn(TypeCtor, TypeDefn),
        get_du_functors_for_type_def(TypeDefn, Functors),
        list.foldl(multi_map.reverse_set(TypeCtorAndDefn), Functors,
            !FunctorsToTypesMap)
    else
        true
    ),
    index_visible_types_by_unqualified_functors(TypeCtorDefns,
        !FunctorsToTypesMap).

%---------------------%

:- pred type_is_user_visible(module_section::in, hlds_type_defn::in)
    is semidet.

type_is_user_visible(Section, TypeDefn) :-
    get_type_defn_status(TypeDefn, TypeStatus),
    status_implies_type_defn_is_user_visible(Section, TypeStatus) = yes.

    % Returns yes if a type definition with the given import status
    % is user visible in a section of the current module.
    %
:- func status_implies_type_defn_is_user_visible(module_section,
    type_status) = bool.

status_implies_type_defn_is_user_visible(Section, TypeStatus) = Visible :-
    TypeStatus = type_status(Status),
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
            Section = ms_interface,
            Visible = no
        ;
            Section = ms_implementation,
            Visible = yes
        )
    ).

%---------------------%

:- pred get_du_functors_for_type_def(hlds_type_defn::in,
    list(functor_name_and_arity)::out) is det.

get_du_functors_for_type_def(TypeDefn, Functors) :-
    get_type_defn_body(TypeDefn, TypeDefnBody),
    (
        TypeDefnBody = hlds_du_type(type_body_du(Constructors, _, _, _, _)),
        list.map(constructor_to_functor_name_and_arity,
            one_or_more_to_list(Constructors), Functors)
    ;
        ( TypeDefnBody = hlds_eqv_type(_)
        ; TypeDefnBody = hlds_foreign_type(_)
        ; TypeDefnBody = hlds_solver_type(_)
        ; TypeDefnBody = hlds_abstract_type(_)
        ),
        Functors = []
    ).

:- pred constructor_to_functor_name_and_arity(constructor::in,
    functor_name_and_arity::out) is det.

constructor_to_functor_name_and_arity(Ctor, FunctorNameAndArity) :-
    Ctor = ctor(_, _, SymName, _ArgTypes, Arity, _),
    FunctorNameAndArity =
        functor_name_and_arity(unqualify_name(SymName), Arity).

%---------------------------------------------------------------------------%

:- pred check_inst_defns_have_matching_types(type_table::in,
    functors_to_types_map::in,
    assoc_list(inst_ctor, hlds_inst_defn)::in,
    assoc_list(inst_ctor, hlds_inst_defn)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_inst_defns_have_matching_types(_TypeTable, _FunctorsToTypeDefns,
        [], [], !Specs).
check_inst_defns_have_matching_types(TypeTable, FunctorsToTypeDefns,
        [InstCtorDefnPair0 | InstCtorDefnPairs0],
        [InstCtorDefnPair | InstCtorDefnPairs], !Specs) :-
    InstCtorDefnPair0 = InstCtor - InstDefn0,
    check_inst_defn_has_matching_type(TypeTable, FunctorsToTypeDefns,
        InstCtor, InstDefn0, InstDefn, !Specs),
    InstCtorDefnPair = InstCtor - InstDefn,
    check_inst_defns_have_matching_types(TypeTable, FunctorsToTypeDefns,
        InstCtorDefnPairs0, InstCtorDefnPairs, !Specs).

:- pred check_inst_defn_has_matching_type(type_table::in,
    functors_to_types_map::in, inst_ctor::in,
    hlds_inst_defn::in, hlds_inst_defn::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_inst_defn_has_matching_type(TypeTable, FunctorsToTypesMap, InstCtor,
        InstDefn0, InstDefn, !Specs) :-
    InstDefn0 = hlds_inst_defn(InstVarSet, InstParams, InstBody,
        IFTC0, Context, Status),
    InstBody = eqv_inst(Inst),
    (
        Inst = bound(_, _, BoundInsts),
        (
            IFTC0 = iftc_applicable_declared(ForTypeCtor0),
            ForTypeCtor0 = type_ctor(ForTypeCtorName, ForTypeCtorArity),
            % We bind ForTypeCtor to a standardized form of ForTypeCtor0.
            ( if
                sym_name_for_builtin_type(ForTypeCtorName, "int"),
                ForTypeCtorArity = 0
            then
                ForTypeCtor = int_type_ctor,
                MaybeForTypeKind = yes(ftk_int(int_type_int))
            else if
                sym_name_for_builtin_type(ForTypeCtorName, "uint"),
                ForTypeCtorArity = 0
            then
                ForTypeCtor = uint_type_ctor,
                MaybeForTypeKind = yes(ftk_int(int_type_uint))
            else if
                sym_name_for_builtin_type(ForTypeCtorName, "int8"),
                ForTypeCtorArity = 0
            then
                ForTypeCtor = int8_type_ctor,
                MaybeForTypeKind = yes(ftk_int(int_type_int8))
            else if
                sym_name_for_builtin_type(ForTypeCtorName, "uint8"),
                ForTypeCtorArity = 0
            then
                ForTypeCtor = uint8_type_ctor,
                MaybeForTypeKind = yes(ftk_int(int_type_uint8))
            else if
                sym_name_for_builtin_type(ForTypeCtorName, "int16"),
                ForTypeCtorArity = 0
            then
                ForTypeCtor = int16_type_ctor,
                MaybeForTypeKind = yes(ftk_int(int_type_int16))
            else if
                sym_name_for_builtin_type(ForTypeCtorName, "uint16"),
                ForTypeCtorArity = 0
            then
                ForTypeCtor = uint16_type_ctor,
                MaybeForTypeKind = yes(ftk_int(int_type_uint16))
            else if
                sym_name_for_builtin_type(ForTypeCtorName, "int32"),
                ForTypeCtorArity = 0
            then
                ForTypeCtor = int32_type_ctor,
                MaybeForTypeKind = yes(ftk_int(int_type_int32))
            else if
                sym_name_for_builtin_type(ForTypeCtorName, "uint32"),
                ForTypeCtorArity = 0
            then
                ForTypeCtor = uint32_type_ctor,
                MaybeForTypeKind = yes(ftk_int(int_type_uint32))
            else if
                sym_name_for_builtin_type(ForTypeCtorName, "int64"),
                ForTypeCtorArity = 0
            then
                ForTypeCtor = int64_type_ctor,
                MaybeForTypeKind = yes(ftk_int(int_type_int64))
            else if
                sym_name_for_builtin_type(ForTypeCtorName, "uint64"),
                ForTypeCtorArity = 0
            then
                ForTypeCtor = uint64_type_ctor,
                MaybeForTypeKind = yes(ftk_int(int_type_uint64))
            else if
                sym_name_for_builtin_type(ForTypeCtorName, "float"),
                ForTypeCtorArity = 0
            then
                ForTypeCtor = float_type_ctor,
                MaybeForTypeKind = yes(ftk_float)
            else if
                ( ForTypeCtorName = unqualified(CName)
                ; ForTypeCtorName = qualified(unqualified("char"), CName)
                ),
                ( CName = "char"
                ; CName = "character"
                ),
                ForTypeCtorArity = 0
            then
                ForTypeCtor = char_type_ctor,
                MaybeForTypeKind = yes(ftk_char)
            else if
                sym_name_for_builtin_type(ForTypeCtorName, "string"),
                ForTypeCtorArity = 0
            then
                ForTypeCtor = string_type_ctor,
                MaybeForTypeKind = yes(ftk_string)
            else
                ForTypeCtor = ForTypeCtor0,
                ( if
                    search_type_ctor_defn(TypeTable, ForTypeCtor0,
                        ForTypeDefn)
                then
                    ForTypeKind0 = ftk_user(ForTypeCtor, ForTypeDefn),
                    MaybeForTypeKind = yes(ForTypeKind0)
                else
                    MaybeForTypeKind = no
                )
            ),
            (
                MaybeForTypeKind = no,
                maybe_issue_no_such_type_error(InstCtor, InstDefn0,
                    ForTypeCtor, !Specs),
                IFTC = iftc_not_applicable
            ;
                MaybeForTypeKind = yes(ForTypeKind),
                check_for_type_bound_insts(ForTypeKind, BoundInsts,
                    cord.init, MismatchesCord),
                Mismatches = cord.list(MismatchesCord),
                maybe_issue_type_match_error(InstCtor, InstDefn0,
                    ForTypeKind, IFTC, Mismatches, MatchSpecs),
                !:Specs = MatchSpecs ++ !.Specs
            ),
            InstDefn = hlds_inst_defn(InstVarSet, InstParams, InstBody,
                IFTC, Context, Status)
        ;
            IFTC0 = iftc_applicable_not_known,
            get_possible_types_for_bound_insts(FunctorsToTypesMap,
                BoundInsts, all_typeable_functors, TypeableFunctors,
                [], PossibleTypeSets),
            (
                TypeableFunctors = some_untypeable_functors,
                InstDefn = InstDefn0
            ;
                TypeableFunctors = all_typeable_functors,
                PossibleTypesSet = set.intersect_list(PossibleTypeSets),
                PossibleTypes = set.to_sorted_list(PossibleTypesSet),
                maybe_issue_no_matching_types_warning(InstCtor, InstDefn0,
                    BoundInsts, PossibleTypes, PossibleTypeSets, !Specs),
                list.map(type_defn_or_builtin_to_type_ctor, PossibleTypes,
                    PossibleTypeCtors),
                IFTC = iftc_applicable_known(PossibleTypeCtors),
                InstDefn = hlds_inst_defn(InstVarSet, InstParams, InstBody,
                    IFTC, Context, Status)
            )
        ;
            IFTC0 = iftc_applicable_known(_),
            % We haven't yet had a chance to set IFTC0 to this value.
            unexpected($pred, "bound, IFTC0 = iftc_applicable_known")
        ;
            IFTC0 = iftc_applicable_error,
            % We haven't yet had a chance to set IFTC0 to this value.
            unexpected($pred, "bound, IFTC0 = iftc_applicable_error")
        ;
            IFTC0 = iftc_not_applicable,
            % A "for type" annotation definitely is applicable to this
            % inst definition.
            unexpected($pred, "bound, IFTC0 = iftc_not_applicable")
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
        ),
        expect(unify(IFTC0, iftc_not_applicable), $pred,
            "not bound, IFTC0 != iftc_not_applicable"),
        InstDefn = InstDefn0
    ).

:- pred sym_name_for_builtin_type(sym_name::in, string::in) is semidet.

sym_name_for_builtin_type(SymName, TypeName) :-
    ( SymName = unqualified(TypeName)
    ; SymName = qualified(unqualified(TypeName), TypeName)
    ).

:- pred type_defn_or_builtin_to_type_ctor(type_defn_or_builtin::in,
    type_ctor::out) is det.

type_defn_or_builtin_to_type_ctor(TypeDefnOrBuiltin, TypeCtor) :-
    (
        TypeDefnOrBuiltin = type_user(type_ctor_and_defn(TypeCtor, _))
    ;
        TypeDefnOrBuiltin = type_builtin(BuiltinType),
        (
            BuiltinType = builtin_type_int(int_type_int),
            TypeCtor = type_ctor(unqualified("int"), 0)
        ;
            BuiltinType = builtin_type_int(int_type_uint),
            TypeCtor = type_ctor(unqualified("uint"), 0)
        ;
            BuiltinType = builtin_type_int(int_type_int8),
            TypeCtor = type_ctor(unqualified("int8"), 0)
        ;
            BuiltinType = builtin_type_int(int_type_uint8),
            TypeCtor = type_ctor(unqualified("uint8"), 0)
        ;
            BuiltinType = builtin_type_int(int_type_int16),
            TypeCtor = type_ctor(unqualified("int16"), 0)
        ;
            BuiltinType = builtin_type_int(int_type_uint16),
            TypeCtor = type_ctor(unqualified("uint16"), 0)
        ;
            BuiltinType = builtin_type_int(int_type_int32),
            TypeCtor = type_ctor(unqualified("int32"), 0)
        ;
            BuiltinType = builtin_type_int(int_type_uint32),
            TypeCtor = type_ctor(unqualified("uint32"), 0)
        ;
            BuiltinType = builtin_type_int(int_type_int64),
            TypeCtor = type_ctor(unqualified("int64"), 0)
        ;
            BuiltinType = builtin_type_int(int_type_uint64),
            TypeCtor = type_ctor(unqualified("uint64"), 0)
        ;
            BuiltinType = builtin_type_float,
            TypeCtor = type_ctor(unqualified("float"), 0)
        ;
            BuiltinType = builtin_type_char,
            TypeCtor = type_ctor(unqualified("char"), 0)
        ;
            BuiltinType = builtin_type_string,
            TypeCtor = type_ctor(unqualified("string"), 0)
        )
    ;
        TypeDefnOrBuiltin = type_tuple(Arity),
        TypeCtor = type_ctor(unqualified("{}"), Arity)
    ).

%---------------------------------------------------------------------------%

:- type for_type_kind
    --->    ftk_user(type_ctor, hlds_type_defn)
    ;       ftk_int(int_type)
    ;       ftk_float
    ;       ftk_char
    ;       ftk_string.

:- type cons_mismatch
    --->    cons_mismatch(
                bad_cons_id                     :: format_component,
                possible_near_miss_cons_ids     :: list(format_component)
            ).

:- pred check_for_type_bound_insts(for_type_kind::in,
    list(bound_inst)::in,
    cord(cons_mismatch)::in, cord(cons_mismatch)::out) is det.

check_for_type_bound_insts(_ForTypeKind, [], !RevMismatches).
check_for_type_bound_insts(ForTypeKind, [BoundInst | BoundInsts],
        !Mismatches) :-
    BoundInst = bound_functor(ConsId, _),
    (
        ConsId = cons(ConsSymName, ConsArity, ConsIdTypeCtor),
        (
            ForTypeKind = ftk_user(TypeCtor, TypeDefn),
            get_type_defn_body(TypeDefn, TypeDefnBody),
            (
                TypeDefnBody = hlds_du_type(TypeBodyDu),
                TypeBodyDu = type_body_du(OoMConstructors, _, _, _, _),
                Constructors = one_or_more_to_list(OoMConstructors),
                (
                    ConsSymName = unqualified(ConsName),
                    find_ctors_with_given_name(ConsName, Constructors,
                        CtorArities),
                    check_arity_and_maybe_report_near_misses(ConsIdTypeCtor,
                        ConsId, unqualified(ConsName),
                        ConsArity, CtorArities, !Mismatches)
                ;
                    ConsSymName = qualified(ConsModuleName, ConsName),
                    find_ctors_with_given_name(ConsName, Constructors,
                        CtorArities),
                    TypeCtor = type_ctor(TypeCtorSymName, _),
                    ( if
                        TypeCtorSymName = qualified(TypeCtorModuleName, _),
                        partial_sym_name_matches_full(ConsModuleName,
                            TypeCtorModuleName)
                    then
                        check_arity_and_maybe_report_near_misses(
                            ConsIdTypeCtor, ConsId, unqualified(ConsName),
                            ConsArity, CtorArities, !Mismatches)
                    else
                        (
                            TypeCtorSymName = qualified(TypeCtorModuleName, _),
                            MissConsSymName =
                                qualified(TypeCtorModuleName, ConsName)
                        ;
                            TypeCtorSymName = unqualified(_),
                            MissConsSymName = unqualified(ConsName)
                        ),
                        report_near_misses(ConsIdTypeCtor, ConsId,
                            MissConsSymName, CtorArities, !Mismatches)
                    )
                )
            ;
                ( TypeDefnBody = hlds_eqv_type(_)
                ; TypeDefnBody = hlds_foreign_type(_)
                ; TypeDefnBody = hlds_solver_type(_)
                ; TypeDefnBody = hlds_abstract_type(_)
                ),
                !:Mismatches = cord.snoc(!.Mismatches, simple_miss(ConsId))
            )
        ;
            ForTypeKind = ftk_char,
            ( if
                ConsSymName = unqualified(ConsName),
                string.count_codepoints(ConsName) = 1
            then
                true
            else
                !:Mismatches = cord.snoc(!.Mismatches, simple_miss(ConsId))
            )
        ;
            ( ForTypeKind = ftk_int(_)
            ; ForTypeKind = ftk_float
            ; ForTypeKind = ftk_string
            ),
            !:Mismatches = cord.snoc(!.Mismatches, simple_miss(ConsId))
        )
    ;
        ConsId = some_int_const(IntConst),
        ExpType = type_of_int_const(IntConst),
        ( if ForTypeKind = ftk_int(ExpType) then
            true
        else
            !:Mismatches = cord.snoc(!.Mismatches, simple_miss(ConsId))
        )
    ;
        ConsId = float_const(_),
        (
            ForTypeKind = ftk_float
        ;
            ( ForTypeKind = ftk_user(_, _)
            ; ForTypeKind = ftk_int(_)
            ; ForTypeKind = ftk_char
            ; ForTypeKind = ftk_string
            ),
            !:Mismatches = cord.snoc(!.Mismatches, simple_miss(ConsId))
        )
    ;
        ConsId = char_const(_),
        (
            ForTypeKind = ftk_char
        ;
            ( ForTypeKind = ftk_user(_, _)
            ; ForTypeKind = ftk_int(_)
            ; ForTypeKind = ftk_float
            ; ForTypeKind = ftk_string
            ),
            !:Mismatches = cord.snoc(!.Mismatches, simple_miss(ConsId))
        )
    ;
        ConsId = string_const(_),
        (
            ForTypeKind = ftk_string
        ;
            ( ForTypeKind = ftk_user(_, _)
            ; ForTypeKind = ftk_int(_)
            ; ForTypeKind = ftk_float
            ; ForTypeKind = ftk_char
            ),
            !:Mismatches = cord.snoc(!.Mismatches, simple_miss(ConsId))
        )
    ;
        ( ConsId = tuple_cons(_)
        ; ConsId = closure_cons(_, _)
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
        !:Mismatches = cord.snoc(!.Mismatches, simple_miss(ConsId))
    ),
    check_for_type_bound_insts(ForTypeKind, BoundInsts, !Mismatches).

:- pred find_ctors_with_given_name(string::in, list(constructor)::in,
    list(arity)::out) is det.

find_ctors_with_given_name(_ConsName, [], []).
find_ctors_with_given_name(ConsName, [Constructor | Constructors], Arities) :-
    find_ctors_with_given_name(ConsName, Constructors, AritiesTail),
    Constructor = ctor(_, _, CtorSymName, _, CtorArity, _),
    ( if unqualify_name(CtorSymName) = ConsName then
        Arities = [CtorArity | AritiesTail]
    else
        Arities = AritiesTail
    ).

:- func simple_miss(cons_id) = cons_mismatch.

simple_miss(ConsId) =
    cons_mismatch(qual_cons_id_and_maybe_arity(ConsId), []).

:- pred check_arity_and_maybe_report_near_misses(type_ctor::in, cons_id::in,
    sym_name::in, arity::in, list(arity)::in,
    cord(cons_mismatch)::in, cord(cons_mismatch)::out) is det.

check_arity_and_maybe_report_near_misses(TypeCtor, ConsId, SymName,
        ConsArity, CtorArities, !Mismatches) :-
    ( if list.member(ConsArity, CtorArities) then
        true
    else
        report_near_misses(TypeCtor, ConsId, SymName, CtorArities, !Mismatches)
    ).

:- pred report_near_misses(type_ctor::in, cons_id::in, sym_name::in,
    list(arity)::in,
    cord(cons_mismatch)::in, cord(cons_mismatch)::out) is det.

report_near_misses(TypeCtor, ConsId, SymName, CtorArities, !Mismatches) :-
    NearMisses =
        list.map(make_cons_id_component(TypeCtor, SymName), CtorArities),
    Mismatch = cons_mismatch(qual_cons_id_and_maybe_arity(ConsId), NearMisses),
    !:Mismatches = cord.snoc(!.Mismatches, Mismatch).

:- func make_cons_id_component(type_ctor, sym_name, arity) = format_component.

make_cons_id_component(TypeCtor, SymName, Arity) =
    qual_cons_id_and_maybe_arity(cons(SymName, Arity, TypeCtor)).

%---------------------------------------------------------------------------%

:- type typeable_functors
    --->    some_untypeable_functors
    ;       all_typeable_functors.

:- type type_defn_or_builtin
    --->    type_user(type_ctor_and_defn)
    ;       type_builtin(builtin_type)
    ;       type_tuple(arity).

:- pred get_possible_types_for_bound_insts(functors_to_types_map::in,
    list(bound_inst)::in, typeable_functors::in, typeable_functors::out,
    list(set(type_defn_or_builtin))::in, list(set(type_defn_or_builtin))::out)
    is det.

get_possible_types_for_bound_insts(_FunctorsToTypesMap, [],
        !TypeableFunctors, !PossibleTypeSets).
get_possible_types_for_bound_insts(FunctorsToTypesMap,
        [BoundInst | BoundInsts], !TypeableFunctors, !PossibleTypeSets) :-
    get_possible_types_for_bound_inst(FunctorsToTypesMap, BoundInst,
        MaybePossibleTypes),
    (
        MaybePossibleTypes = no,
        !:TypeableFunctors = some_untypeable_functors
    ;
        MaybePossibleTypes = yes(PossibleTypes),
        PossibleTypeSet = set.list_to_set(PossibleTypes),
        !:PossibleTypeSets = [PossibleTypeSet | !.PossibleTypeSets]
    ),
    get_possible_types_for_bound_insts(FunctorsToTypesMap,
        BoundInsts, !TypeableFunctors, !PossibleTypeSets).

    % Return the types that match the cons_id in the given bound inst.
    % We don't bother checking for types for certain cons_ids such as
    % predicate signatures and cons_ids that are only used internally.
    %
:- pred get_possible_types_for_bound_inst(functors_to_types_map::in,
    bound_inst::in, maybe(list(type_defn_or_builtin))::out) is det.

get_possible_types_for_bound_inst(FunctorsToTypesMap, BoundInst, MaybeTypes) :-
    BoundInst = bound_functor(ConsId, _),
    (
        ConsId = cons(SymName, Arity, _),
        Name = unqualify_name(SymName),
        FunctorNameAndArity = functor_name_and_arity(Name, Arity),
        ( if
            multi_map.search(FunctorsToTypesMap, FunctorNameAndArity,
                TypeCtorDefns)
        then
            find_matching_user_types(SymName, TypeCtorDefns, UserTypes)
        else
            UserTypes = []
        ),
        % Zero arity functors with length 1 could match the builtin
        % character type.
        ( if string.count_codepoints(Name) = 1 then
            UserCharTypes = [type_builtin(builtin_type_char) | UserTypes]
        else
            UserCharTypes = UserTypes
        ),
        % The inst could match a tuple type, which won't be explicitly
        % declared.
        ( if type_ctor_is_tuple(type_ctor(SymName, Arity)) then
            Types = [type_tuple(Arity) | UserCharTypes]
        else
            Types = UserCharTypes
        ),
        MaybeTypes = yes(Types)
    ;
        ConsId = tuple_cons(Arity),
        MaybeTypes = yes([type_tuple(Arity)])
    ;
        ConsId = some_int_const(IntConst),
        IntType = type_of_int_const(IntConst),
        MaybeTypes = yes([type_builtin(builtin_type_int(IntType))])
    ;
        ConsId = float_const(_),
        MaybeTypes = yes([type_builtin(builtin_type_float)])
    ;
        ConsId = char_const(_),
        MaybeTypes = yes([type_builtin(builtin_type_char)])
    ;
        ConsId = string_const(_),
        MaybeTypes = yes([type_builtin(builtin_type_string)])
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
        MaybeTypes = no
    ).

:- pred find_matching_user_types(sym_name::in, list(type_ctor_and_defn)::in,
    list(type_defn_or_builtin)::out) is det.

find_matching_user_types(_FunctorSymName, [], []).
find_matching_user_types(FunctorSymName,
        [TypeCtorAndDefn | TypeCtorAndDefns], MatchingUserTypes) :-
    find_matching_user_types(FunctorSymName, TypeCtorAndDefns,
        MatchingUserTypes0),
    TypeCtorAndDefn = type_ctor_and_defn(TypeCtor, _TypeDefn),
    TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
    (
        TypeCtorSymName = unqualified(_),
        unexpected($pred, "TypeCtorSymName is unqualified")
    ;
        TypeCtorSymName = qualified(TypeCtorModuleName, _)
    ),
    (
        FunctorSymName = unqualified(_),
        MatchingUserTypes = [type_user(TypeCtorAndDefn) | MatchingUserTypes0]
    ;
        FunctorSymName = qualified(FunctorModuleName, _),
        ( if
            partial_sym_name_matches_full(FunctorModuleName,
                TypeCtorModuleName)
        then
            MatchingUserTypes = [type_user(TypeCtorAndDefn) |
                MatchingUserTypes0]
        else
            MatchingUserTypes = MatchingUserTypes0
        )
    ).

%---------------------------------------------------------------------------%

:- pred maybe_issue_no_such_type_error(inst_ctor::in, hlds_inst_defn::in,
    type_ctor::in, list(error_spec)::in, list(error_spec)::out) is det.

maybe_issue_no_such_type_error(InstCtor, InstDefn, TypeCtor, !Specs) :-
    InstStatus = InstDefn ^ inst_status,
    InstDefinedInThisModule = inst_status_defined_in_this_module(InstStatus),
    (
        InstDefinedInThisModule = no
    ;
        InstDefinedInThisModule = yes,
        Context = InstDefn ^ inst_context,
        InstCtor = inst_ctor(InstName, InstArity),
        TypeCtor = type_ctor(TypeCtorName, TypeCtorArity),
        Pieces = [words("Error: inst"),
            unqual_sym_name_arity(sym_name_arity(InstName, InstArity)),
            words("is specified to be for"),
            qual_sym_name_arity(
                sym_name_arity(TypeCtorName, TypeCtorArity)),
            suffix(","),
            words("but that type constructor is not visible here."), nl],
        Spec = simplest_spec($pred, severity_error, phase_inst_check,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred maybe_issue_type_match_error(inst_ctor::in, hlds_inst_defn::in,
    for_type_kind::in, inst_for_type_ctor::out, list(cons_mismatch)::in,
    list(error_spec)::out) is det.

maybe_issue_type_match_error(InstCtor, InstDefn, ForTypeKind, IFTC, Mismatches,
        !:Specs) :-
    !:Specs = [],
    InstCtor = inst_ctor(InstSymName, InstArity),
    ShortInstSymName = unqualified(unqualify_name(InstSymName)),
    Context = InstDefn ^ inst_context,
    InstStatus = InstDefn ^ inst_status,
    InstDefinedInThisModule = inst_status_defined_in_this_module(InstStatus),
    (
        (
            ForTypeKind = ftk_int(int_type_int),
            ForTypeCtor = int_type_ctor
        ;
            ForTypeKind = ftk_int(int_type_uint),
            ForTypeCtor = uint_type_ctor
        ;
            ForTypeKind = ftk_int(int_type_int8),
            ForTypeCtor = int8_type_ctor
        ;
            ForTypeKind = ftk_int(int_type_uint8),
            ForTypeCtor = uint8_type_ctor
        ;
            ForTypeKind = ftk_int(int_type_int16),
            ForTypeCtor = int16_type_ctor
        ;
            ForTypeKind = ftk_int(int_type_uint16),
            ForTypeCtor = uint16_type_ctor
        ;
            ForTypeKind = ftk_int(int_type_int32),
            ForTypeCtor = int32_type_ctor
        ;
            ForTypeKind = ftk_int(int_type_uint32),
            ForTypeCtor = uint32_type_ctor
        ;
            ForTypeKind = ftk_int(int_type_int64),
            ForTypeCtor = int64_type_ctor
        ;
            ForTypeKind = ftk_int(int_type_uint64),
            ForTypeCtor = uint64_type_ctor
        ;
            ForTypeKind = ftk_float,
            ForTypeCtor = float_type_ctor
        ;
            ForTypeKind = ftk_char,
            ForTypeCtor = char_type_ctor
        ;
            ForTypeKind = ftk_string,
            ForTypeCtor = string_type_ctor
        ),
        ForTypeCtor = type_ctor(TypeCtorName, TypeCtorArity)
    ;
        ForTypeKind = ftk_user(ForTypeCtor, ForTypeDefn),
        ForTypeCtor = type_ctor(TypeCtorName, TypeCtorArity),
        InstIsExported =
            inst_status_is_exported_to_non_submodules(InstStatus),
        ( if
            InstIsExported = yes,
            not type_is_user_visible(ms_interface, ForTypeDefn)
        then
            VisPieces = [words("Error: inst"),
                unqual_sym_name_arity(
                    sym_name_arity(ShortInstSymName, InstArity)),
                words("is exported, but the type it is for,"),
                qual_sym_name_arity(
                    sym_name_arity(TypeCtorName, TypeCtorArity)),
                suffix(","), words("is not visible outside this module."), nl],
            VisSpec = simplest_spec($pred, severity_error, phase_inst_check,
                Context, VisPieces),
            !:Specs = [VisSpec | !.Specs]
        else
            true
        )
    ),

    (
        Mismatches = []
    ;
        Mismatches = [_ | MismatchesTail],
        cons_id_strs_and_near_misses(Mismatches, MismatchConsIdComponents,
            NearMisses),
        FuncSymbolPhrase = choose_number(Mismatches,
            "function symbol", "function symbols"),
        IsAreNotPhrase = choose_number(Mismatches,
            "is not a function symbol", "are not function symbols"),
        MismatchConsIdPieces =
            component_list_to_pieces("and", MismatchConsIdComponents),
        MismatchPieces = [words("Error: inst"),
            unqual_sym_name_arity(
                sym_name_arity(ShortInstSymName, InstArity)),
            words("is declared to be for type"),
            qual_sym_name_arity(
                sym_name_arity(TypeCtorName, TypeCtorArity)),
            suffix(","), words("but its top level"),
            words(FuncSymbolPhrase)] ++ MismatchConsIdPieces ++
            [words(IsAreNotPhrase), words("of that type."), nl],
        (
            NearMisses = [],
            NearMissPieces = []
        ;
            NearMisses = [_ | _],
            (
                MismatchesTail = [],
                NearMissPieces = list.condense(
                    list.map(project_if_alone, NearMisses))
            ;
                MismatchesTail = [_ | _],
                NearMissPieces = list.condense(
                    list.map(project_if_several, NearMisses))
            )
        ),
        MismatchSpec = simplest_spec($pred, severity_error, phase_inst_check,
            Context, MismatchPieces ++ NearMissPieces),
        !:Specs = [MismatchSpec | !.Specs]
    ),
    (
        !.Specs = [],
        IFTC = iftc_applicable_declared(ForTypeCtor)
    ;
        !.Specs = [_ | _],
        IFTC = iftc_applicable_error,
        (
            InstDefinedInThisModule = no,
            !:Specs = []
        ;
            InstDefinedInThisModule = yes
        )
    ).

:- type near_miss_cons_mismatch
    --->    near_miss_cons_mismatch(
                if_only_one_mismatch    :: list(format_component),
                if_several_mismatches   :: list(format_component)
            ).

:- pred cons_id_strs_and_near_misses(list(cons_mismatch)::in,
    list(format_component)::out, list(near_miss_cons_mismatch)::out) is det.

cons_id_strs_and_near_misses([], [], []).
cons_id_strs_and_near_misses([Mismatch | Mismatches],
        [ConsIdComponent | ConsIdComponents], NearMissMismatches) :-
    cons_id_strs_and_near_misses(Mismatches, ConsIdComponents,
        NearMissMismatchesTail),
    Mismatch = cons_mismatch(ConsIdComponent, MaybeNearMisses),
    (
        MaybeNearMisses = [],
        NearMissMismatches = NearMissMismatchesTail
    ;
        MaybeNearMisses = [_FirstNearMiss | _LaterNearMisses],
        IfAlone = [words("Maybe you meant") |
            component_list_to_pieces("or", MaybeNearMisses)] ++
            [suffix("."), nl],
        IfSeveral = [words("For"), ConsIdComponent, suffix(","),
            lower_case_next_if_not_first | IfAlone],
        NearMissMismatch = near_miss_cons_mismatch(IfAlone, IfSeveral),
        NearMissMismatches = [NearMissMismatch | NearMissMismatchesTail]
    ).

:- func project_if_alone(near_miss_cons_mismatch) = list(format_component).
:- func project_if_several(near_miss_cons_mismatch) = list(format_component).

project_if_alone(near_miss_cons_mismatch(IfAlone, _)) = IfAlone.
project_if_several(near_miss_cons_mismatch(_, IfSeveral)) = IfSeveral.

%---------------------------------------------------------------------------%

:- pred maybe_issue_no_matching_types_warning(
    inst_ctor::in, hlds_inst_defn::in,
    list(bound_inst)::in, list(type_defn_or_builtin)::in,
    list(set(type_defn_or_builtin))::in,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_issue_no_matching_types_warning(InstCtor, InstDefn, BoundInsts,
        PossibleTypes, PossibleTypeSets, !Specs) :-
    InstStatus = InstDefn ^ inst_status,
    DefinedInThisModule = inst_status_defined_in_this_module(InstStatus),
    (
        DefinedInThisModule = no
    ;
        DefinedInThisModule = yes,
        (
            PossibleTypes = [],
            Context = InstDefn ^ inst_context,
            InstCtor = inst_ctor(InstName, InstArity),
            NoMatchPieces = [words("Warning: inst"),
                unqual_sym_name_arity(sym_name_arity(InstName, InstArity)),
                words("does not match any of the types in scope."), nl],

            AllPossibleTypesSet = set.union_list(PossibleTypeSets),
            set.to_sorted_list(AllPossibleTypesSet, AllPossibleTypes),
            list.map(diagnose_mismatches_from_type(BoundInsts),
                AllPossibleTypes, MismatchesFromPossibleTypes),
            list.sort(MismatchesFromPossibleTypes,
                SortedMismatchesFromPossibleTypes),
            create_mismatch_pieces(SortedMismatchesFromPossibleTypes,
                MismatchPieces),

            Pieces = NoMatchPieces ++ MismatchPieces,
            Spec = simplest_spec($pred, severity_warning, phase_inst_check,
                Context, Pieces),
            !:Specs = [Spec | !.Specs]
        ;
            PossibleTypes = [_ | _],
            InstIsExported =
                inst_status_is_exported_to_non_submodules(InstStatus),
            % If the inst is exported, then it must match a type
            % that is concrete outside of this module.
            ( if
                (
                    InstIsExported = no
                ;
                    InstIsExported = yes,
                    some [Type] (
                        list.member(Type, PossibleTypes),
                        (
                            Type = type_user(TypeCtorAndDefn),
                            TypeCtorAndDefn = type_ctor_and_defn(_, TypeDefn),
                            type_is_user_visible(ms_interface, TypeDefn)
                        ;
                            Type = type_builtin(_)
                        ;
                            Type = type_tuple(_)
                        )
                    )
                )
            then
                true
            else
                Context = InstDefn ^ inst_context,
                InstCtor = inst_ctor(InstName, InstArity),
                (
                    PossibleTypes = [OnePossibleType],
                    OnePossibleTypeStr =
                        type_defn_or_builtin_to_string(OnePossibleType),
                    Pieces = [words("Warning: inst"),
                        unqual_sym_name_arity(
                            sym_name_arity(InstName, InstArity)),
                        words("is exported, but the one type it matches"),
                        prefix("("), words(OnePossibleTypeStr), suffix(")"),
                        words("is not visible from outside this module.")]
                ;
                    PossibleTypes = [_, _ | _],
                    PossibleTypeStrs = list.map(type_defn_or_builtin_to_string,
                        PossibleTypes),
                    PossibleTypesStr =
                        string.join_list(", ", PossibleTypeStrs),
                    Pieces = [words("Warning: inst"),
                        unqual_sym_name_arity(
                            sym_name_arity(InstName, InstArity)),
                        words("is exported, but none of the types it matches"),
                        prefix("("), words(PossibleTypesStr), suffix(")"),
                        words("are visible from outside this module.")]
                ),
                Spec = simplest_spec($pred, severity_warning, phase_inst_check,
                    Context, Pieces),
                !:Specs = [Spec | !.Specs]
            )
        )
    ).

%---------------------------------------------------------------------------%

:- type mismatch_from_type
    --->    mismatch_from_type(
                mft_num_mismatches      :: int,
                mft_type                :: type_defn_or_builtin,
                mft_pieces              :: list(format_component)
            ).

:- pred diagnose_mismatches_from_type(list(bound_inst)::in,
    type_defn_or_builtin::in, mismatch_from_type::out) is det.

diagnose_mismatches_from_type(BoundInsts, TypeDefnOrBuiltin,
        MismatchFromType) :-
    (
        TypeDefnOrBuiltin = type_user(TypeCtorAndDefn),
        TypeCtorAndDefn = type_ctor_and_defn(_TypeCtor, TypeDefn),
        get_type_defn_body(TypeDefn, TypeDefnBody),
        (
            TypeDefnBody = hlds_du_type(TypeBodyDu),
            TypeBodyDu = type_body_du(Constructors, _, _, _, _),
            find_mismatches_from_user(one_or_more_to_list(Constructors), 1,
                BoundInsts, 0, NumMismatches, cord.init, MismatchPiecesCord)
        ;
            ( TypeDefnBody = hlds_eqv_type(_)
            ; TypeDefnBody = hlds_foreign_type(_)
            ; TypeDefnBody = hlds_solver_type(_)
            ; TypeDefnBody = hlds_abstract_type(_)
            ),
            unexpected($pred, "non-du TypeDefnBody")
        )
    ;
        TypeDefnOrBuiltin = type_builtin(BuiltinType),
        find_mismatches_from_builtin(BuiltinType, 1, BoundInsts,
            0, NumMismatches, cord.init, MismatchPiecesCord)
    ;
        TypeDefnOrBuiltin = type_tuple(TupleArity),
        find_mismatches_from_tuple(TupleArity, 1, BoundInsts,
            0, NumMismatches, cord.init, MismatchPiecesCord)
    ),
    MismatchPieces = cord.list(MismatchPiecesCord),
    MismatchFromType = mismatch_from_type(NumMismatches, TypeDefnOrBuiltin,
        MismatchPieces).

%---------------------%

:- pred find_mismatches_from_user(list(constructor)::in, int::in,
    list(bound_inst)::in, int::in, int::out,
    cord(format_component)::in, cord(format_component)::out) is det.

find_mismatches_from_user(_Ctors, _CurNum,
        [], !NumMismatches, !PiecesCord).
find_mismatches_from_user(Ctors, CurNum,
        [BoundInst | BoundInsts], !NumMismatches, !PiecesCord) :-
    BoundInst = bound_functor(ConsId, _SubInsts),
    ( if
        ConsId = cons(SymName, Arity, _)
    then
        FunctorName = unqualify_name(SymName),
        ( if
            some_ctor_matches_exactly(Ctors, FunctorName, Arity)
        then
            true
        else
            find_matching_name_wrong_arities(Ctors, FunctorName, Arity,
                set.init, ExpectedArities),
            ( if set.is_empty(ExpectedArities) then
                record_mismatch(CurNum, BoundInst, !NumMismatches, !PiecesCord)
            else
                record_arity_mismatch(CurNum, FunctorName, Arity,
                    ExpectedArities, !NumMismatches, !PiecesCord)
            )
        )
    else
        record_mismatch(CurNum, BoundInst, !NumMismatches, !PiecesCord)
    ),
    find_mismatches_from_user(Ctors, CurNum + 1,
        BoundInsts, !NumMismatches, !PiecesCord).

:- pred some_ctor_matches_exactly(list(constructor)::in, string::in, int::in)
    is semidet.

some_ctor_matches_exactly([], _FunctorName, _FunctorArity) :-
    fail.
some_ctor_matches_exactly([Ctor | Ctors], FunctorName, FunctorArity) :-
    Ctor = ctor(_Ordinal, _MaybeExistConstraints, ConsName, _ConsArgs,
        ConsArity, _Context),
    ( if
        unqualify_name(ConsName) = FunctorName,
        ConsArity = FunctorArity
    then
        true
    else
        some_ctor_matches_exactly(Ctors, FunctorName, FunctorArity)
    ).

:- pred find_matching_name_wrong_arities(list(constructor)::in,
    string::in, int::in, set(int)::in, set(int)::out) is det.

find_matching_name_wrong_arities([], _FunctorName, _FunctorArity,
        !ExpectedArities).
find_matching_name_wrong_arities([Ctor | Ctors], FunctorName, FunctorArity,
        !ExpectedArities) :-
    Ctor = ctor(_Ordinal, _MaybeExistConstraints, ConsName, _ConsArgs,
        ConsArity, _Context),
    ( if
        unqualify_name(ConsName) = FunctorName,
        ConsArity \= FunctorArity
    then
        set.insert(ConsArity, !ExpectedArities)
    else
        true
    ),
    find_matching_name_wrong_arities(Ctors, FunctorName, FunctorArity,
        !ExpectedArities).

%---------------------%

:- pred find_mismatches_from_builtin(builtin_type::in, int::in,
    list(bound_inst)::in, int::in, int::out,
    cord(format_component)::in, cord(format_component)::out) is det.

find_mismatches_from_builtin(_ExpectedBuiltinType, _CurNum,
        [], !NumMismatches, !PiecesCord).
find_mismatches_from_builtin(ExpectedBuiltinType, CurNum,
        [BoundInst | BoundInsts], !NumMismatches, !PiecesCord) :-
    BoundInst = bound_functor(ConsId, _SubInsts),
    (
        ExpectedBuiltinType = builtin_type_int(IntType),
        ( if
            ConsId = some_int_const(IntConst),
            type_of_int_const(IntConst) = IntType
        then
            true
        else
            record_mismatch(CurNum, BoundInst, !NumMismatches, !PiecesCord)
        )
    ;
        ExpectedBuiltinType = builtin_type_float,
        ( if ConsId = float_const(_) then
            true
        else
            record_mismatch(CurNum, BoundInst, !NumMismatches, !PiecesCord)
        )
    ;
        ExpectedBuiltinType = builtin_type_char,
        ( if ConsId = char_const(_) then
            true
        else if
            ConsId = cons(SymName, ConsArity, _),
            string.count_codepoints(unqualify_name(SymName)) = 1,
            ConsArity = 0
        then
            true
        else
            record_mismatch(CurNum, BoundInst, !NumMismatches, !PiecesCord)
        )
    ;
        ExpectedBuiltinType = builtin_type_string,
        ( if ConsId = string_const(_) then
            true
        else
            record_mismatch(CurNum, BoundInst, !NumMismatches, !PiecesCord)
        )
    ),
    find_mismatches_from_builtin(ExpectedBuiltinType, CurNum + 1,
        BoundInsts, !NumMismatches, !PiecesCord).

%---------------------%

:- pred find_mismatches_from_tuple(int::in, int::in, list(bound_inst)::in,
    int::in, int::out,
    cord(format_component)::in, cord(format_component)::out) is det.

find_mismatches_from_tuple(_ExpectedArity, _CurNum,
        [], !NumMismatches, !PiecesCord).
find_mismatches_from_tuple(ExpectedArity, CurNum,
        [BoundInst | BoundInsts], !NumMismatches, !PiecesCord) :-
    BoundInst = bound_functor(ConsId, _SubInsts),
    ( if ConsId = tuple_cons(ActualArity) then
        ( if ActualArity = ExpectedArity then
            true
        else
            record_mismatch(CurNum, BoundInst, !NumMismatches, !PiecesCord)
        )
    else
        record_mismatch(CurNum, BoundInst, !NumMismatches, !PiecesCord)
    ),
    find_mismatches_from_tuple(ExpectedArity, CurNum + 1,
        BoundInsts, !NumMismatches, !PiecesCord).

%---------------------%

:- pred record_arity_mismatch(int::in, string::in, int::in, set(int)::in,
    int::in, int::out,
    cord(format_component)::in, cord(format_component)::out) is det.

record_arity_mismatch(CurNum, FunctorName, ActualArity, ExpectedAritiesSet,
        !NumMismatches, !PiecesCord) :-
    !:NumMismatches = !.NumMismatches + 1,
    string.format("In bound functor #%d:", [i(CurNum)], InFunctorStr),
    list.map(string.int_to_string, ExpectedArities, ExpectedArityStrs),
    ExpectedArityOrStr = string.join_list("or", ExpectedArityStrs),
    string.format("function symbol %s has arity %d,",
        [s(FunctorName), i(ActualArity)], ActualStr),
    string.format("expected arity was %s.",
        [s(ExpectedArityOrStr)], ExpectedStr),
    set.to_sorted_list(ExpectedAritiesSet, ExpectedArities),
    Pieces = [words(InFunctorStr), nl, words(ActualStr), nl,
        words(ExpectedStr), nl],
    !:PiecesCord = !.PiecesCord ++ cord.from_list(Pieces).

:- pred record_mismatch(int::in, bound_inst::in, int::in, int::out,
    cord(format_component)::in, cord(format_component)::out) is det.

record_mismatch(CurNum, BoundInst, !NumMismatches, !PiecesCord) :-
    !:NumMismatches = !.NumMismatches + 1,
    BoundInst = bound_functor(ConsId, SubInsts),
    ConsIdStr = mercury_cons_id_to_string(output_mercury,
        does_not_need_brackets, ConsId),
    string.format("In bound functor #%d:", [i(CurNum)], InFunctorStr),
    string.format("function symbol is %s/%d.",
        [s(ConsIdStr), i(list.length(SubInsts))], ActualStr),
    Pieces = [words(InFunctorStr), nl, words(ActualStr), nl],
    !:PiecesCord = !.PiecesCord ++ cord.from_list(Pieces).

%---------------------------------------------------------------------------%

:- pred create_mismatch_pieces(list(mismatch_from_type)::in,
    list(format_component)::out) is det.

create_mismatch_pieces([], []).
create_mismatch_pieces([FirstMismatch | LaterMismatches], Pieces) :-
    FirstMismatch = mismatch_from_type(FirstNumMismatches, _, _),
    take_while_same_num_mismatches(FirstNumMismatches,
        LaterMismatches, TakenLaterMismatches),
    (
        TakenLaterMismatches = [],
        create_pieces_for_one_mismatch(FirstMismatch, Pieces)
    ;
        TakenLaterMismatches = [_ | _],
        RelevantMismatches = [FirstMismatch | TakenLaterMismatches],
        list.length(RelevantMismatches, NumRelevantMismatches),
        HeadPieces = [words("There are"), int_fixed(NumRelevantMismatches),
            words("equally close matches."), nl],
        create_pieces_for_all_mismatches(RelevantMismatches, 1, TailPieces),
        Pieces = HeadPieces ++ TailPieces
    ).

:- pred take_while_same_num_mismatches(int::in,
    list(mismatch_from_type)::in, list(mismatch_from_type)::out) is det.

take_while_same_num_mismatches(_Num, [], []).
take_while_same_num_mismatches(Num, [Mismatch | Mismatches], Taken) :-
    Mismatch = mismatch_from_type(NumMismatches, _, _),
    ( if Num = NumMismatches then
        take_while_same_num_mismatches(Num, Mismatches, TakenTail),
        Taken = [Mismatch | TakenTail]
    else
        Taken = []
    ).

:- pred create_pieces_for_one_mismatch(mismatch_from_type::in,
    list(format_component)::out) is det.

create_pieces_for_one_mismatch(Mismatch, Pieces) :-
    Mismatch = mismatch_from_type(_, TypeDefnOrBuiltin, BoundInstPieces),
    Pieces = [words("The closest match is"),
        fixed(type_defn_or_builtin_to_string(TypeDefnOrBuiltin)), suffix(","),
        words("for which the top level mismatches are the following."), nl]
        ++ BoundInstPieces.

:- pred create_pieces_for_all_mismatches(list(mismatch_from_type)::in, int::in,
    list(format_component)::out) is det.

create_pieces_for_all_mismatches([], _Cur, []).
create_pieces_for_all_mismatches([Mismatch | Mismatches], Cur, Pieces) :-
    create_pieces_for_all_mismatches(Mismatches, Cur + 1, TailPieces),
    Mismatch = mismatch_from_type(_, TypeDefnOrBuiltin, BoundInstPieces),
    Pieces = [words("The"), nth_fixed(Cur), words("match is"),
        fixed(type_defn_or_builtin_to_string(TypeDefnOrBuiltin)), suffix(","),
        words("for which the top level mismatches are the following."), nl]
        ++ BoundInstPieces ++ TailPieces.

:- func type_defn_or_builtin_to_string(type_defn_or_builtin) = string.

type_defn_or_builtin_to_string(TypeDefnOrBuiltin) = Str :-
    (
        TypeDefnOrBuiltin = type_user(type_ctor_and_defn(TypeCtor, _)),
        Str = type_ctor_to_string(TypeCtor)
    ;
        TypeDefnOrBuiltin = type_builtin(BuiltinType),
        (
            BuiltinType = builtin_type_int(int_type_int),
            Str = "int"
        ;
            BuiltinType = builtin_type_int(int_type_uint),
            Str = "uint"
        ;
            BuiltinType = builtin_type_int(int_type_int8),
            Str = "int8"
        ;
            BuiltinType = builtin_type_int(int_type_uint8),
            Str = "uint8"
        ;
            BuiltinType = builtin_type_int(int_type_int16),
            Str = "int16"
        ;
            BuiltinType = builtin_type_int(int_type_uint16),
            Str = "uint16"
        ;
            BuiltinType = builtin_type_int(int_type_int32),
            Str = "int32"
        ;
            BuiltinType = builtin_type_int(int_type_uint32),
            Str = "uint32"
        ;
            BuiltinType = builtin_type_int(int_type_int64),
            Str = "int64"
        ;
            BuiltinType = builtin_type_int(int_type_uint64),
            Str = "uint64"
        ;
            BuiltinType = builtin_type_float,
            Str = "float"
        ;
            BuiltinType = builtin_type_char,
            Str = "char"
        ;
            BuiltinType = builtin_type_string,
            Str = "string"
        )
    ;
        TypeDefnOrBuiltin = type_tuple(Arity),
        Str = string.format("{}/%d", [i(Arity)])
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.inst_check.
%---------------------------------------------------------------------------%
