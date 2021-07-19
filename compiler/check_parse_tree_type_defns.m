%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% Copyright (C) 2019-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: check_parse_tree_type_defns.m.
% Main author: zs.
%
% This module checks whether the type definition and foreign enum items
% in a module represent a valid set of type definitions.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.check_parse_tree_type_defns.
:- interface.

:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.

%-----------------------------------------------------------------------------%

:- type maybe_insist_on_defn
    --->    do_not_insist_on_defn
    ;       do_insist_on_defn.

    % create_type_ctor_checked_map(InsistOnDefn,
    %   IntTypeDefnMap, ImpTypeDefnMap, ImpForeignEnumMap,
    %   TypeCtorCheckedMap, !Specs):
    %
    % Given the type definitions in both the interface and implementation
    % sections of a module, and foreign enum definitions in the implementation
    % section, check for each type constructor whether all the definitions
    % of that type constructor are consistent with one another.
    %
    % If yes, then include a representation of that consistent set
    % of declarations for that type constructor in TypeCtorCheckedMap.
    %
    % If not, generate one or more error messages.
    %
    % If InsistOnDefn is do_insist_on_defn, then also generate an error
    % if a TypeCtor is not actually defined. If our input comes from
    % a source file InsistOnDefn *should* be do_insist_on_defn, since
    % declaring but not defining a type is an error. (With the very rare
    % exception of a few library types whose definitions are in the
    % runtime system, outside the view of the Mercury compiler.)
    % However, InsistOnDefn should be do_not_insist_on_defn if the
    % input comes from interface files whose creation may have deleted
    % any such originally-present definitions.
    %
    % In addition, generate error messages for any duplicate field names
    % on arguments of du types.
    %
:- pred create_type_ctor_checked_map(maybe_insist_on_defn::in,
    type_ctor_defn_map::in, type_ctor_defn_map::in,
    type_ctor_foreign_enum_map::in, type_ctor_checked_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%
% The representation of a checked-to-be-consistent set of type and
% foreign enum definitions for every type constructor defined in a module.
%

:- type type_ctor_checked_map == map(type_ctor, type_ctor_checked_defn).

    % A type is either a solver type, or not.
:- type type_ctor_checked_defn
    --->    checked_defn_solver(solver_type_defn)
    ;       checked_defn_std(std_type_defn).

:- type solver_type_defn
    --->    solver_type_abstract(
                abstract_solver_type_status,

                % The abstract definition. It may be in either section;
                % the status specifies the section.
                item_type_defn_info_abstract
            )
    ;       solver_type_full(
                % The abstract definition in the interface section,
                % if one exists.
                maybe(item_type_defn_info_abstract),

                % The full solver type definition, which must in the
                % implementation section.
                item_type_defn_info_solver
            ).

:- type abstract_solver_type_status
    --->    abstract_solver_type_exported
            % The type name is exported. The abstract definition
            % is in the interface section.
    ;       abstract_solver_type_private.
            % The type name is not exported. The abstract definition
            % is in the implementation section.

:- type std_type_defn
    --->    std_mer_type_eqv(
                std_eqv_type_status,

                % The equivalence type definition.
                item_type_defn_info_eqv
            )
    ;       std_mer_type_du_subtype(
                std_du_type_status,

                % The discriminated union type definition, which is a subtype.
                item_type_defn_info_du
            )
    ;       std_mer_type_du_all_plain_constants(
                std_du_type_status,

                % The discriminated union type definition (not a subtype),
                % which represents either a direct dummy type or an enum.
                item_type_defn_info_du,

                % The first functor name in the type, and any later functor
                % names. If there are no later functor names, then the type
                % is a direct dummy type, and must satisfy the requirements
                % of non_sub_du_type_is_dummy; if there are, then the type
                % is an enum type, and must satisfy the requirements of
                % non_sub_du_type_is_enum. (Function symbols that do not meet
                % the relevant requirements may be constants, but we
                % don't consider them *plain* constants.)
                string,
                list(string),

                % For each of our target foreign languages, this field
                % specifies whether we have either a foreign language
                % definition for this type, or a foreign enum definition.
                %
                % While the Mercury representation uses small integers
                % allocated consecutively from 0 to represent function symbols,
                % this is not true even for foreign enum definitions,
                % much less foreign type definitions.
                c_j_cs_maybe_defn_or_enum
            )
    ;       std_mer_type_du_not_all_plain_constants(
                std_du_type_status,

                % The discriminated union type definition (not a subtype),
                % which represents a type *other* than a direct dummy type or
                % an enum.
                item_type_defn_info_du,

                % For each of our target foreign languages, this field
                % specifies whether we have a foreign language type definition
                % for this type.
                c_j_cs_maybe_defn
            )
    ;       std_mer_type_abstract(
                std_abs_type_status,

                % The abstract declaration of the type (not a subtype).
                item_type_defn_info_abstract,

                % For each of our target foreign languages, this field
                % specifies whether we have a foreign language type definition
                % for this type.
                c_j_cs_maybe_defn
            ).

:- type maybe_only_constants
    --->    not_only_plain_constants
    ;       only_plain_constants(
                % The names of the constants, in the order of declaration.
                opc_head_name       :: string,
                opc_tail_names      :: list(string)
            ).

:- type std_du_type_status
    --->    std_du_type_mer_ft_exported
            % Both the Mercury and any foreign type definitions are exported.
            % Any foreign enum definitions are private, as they have to be.
            % This status is not applicable to equivalence types or subtypes,
            % since they may not have foreign type definitions.
    ;       std_du_type_mer_exported
            % The Mercury definition is exported. Any foreign type definitions
            % and/or foreign enum definitions are private.
    ;       std_du_type_abstract_exported
            % Only the type name is exported. The Mercury definition and
            % any foreign type definitions and/or foreign enum definitions
            % are private.
    ;       std_du_type_all_private.
            % Everything about the type is private.

:- type std_eqv_type_status
    --->    std_eqv_type_mer_exported
            % The Mercury definition is exported.
    ;       std_eqv_type_abstract_exported
            % Only the type name is exported. The Mercury definition
            % is private.
    ;       std_eqv_type_all_private.
            % Everything about the type is private.

:- type std_abs_type_status
    --->    std_abs_type_ft_exported
            % The type has foreign type definitions that are exported.
            % Any foreign enum definitions are private, as they have to be.
    ;       std_abs_type_abstract_exported
            % Only the type name is exported. Any foreign type definitions
            % and/or foreign enum definitions are private.
    ;       std_abs_type_all_private.
            % Everything about the type is private.

:- type c_j_cs_maybe_defn_or_enum ==
    c_java_csharp(maybe(foreign_type_or_enum)).

:- type foreign_type_or_enum
    --->    foreign_type_or_enum_type(item_type_defn_info_foreign)
    ;       foreign_type_or_enum_enum(checked_foreign_enum).

    % Part of checking a foreign enum definition is checking whether
    % the correspondence it describes between the Mercury functors
    % of the type on the one hand and their foreign language counterparts
    % on the other hand is a bijection. If yes, then the second argument
    % of the checked_foreign_enum we construct gives the foreign language
    % counterpart of each Mercury function symbol in the type in the order
    % in which the Mercury function symbols are defined.
    %
    % For example, given
    %
    %   :- type t ---> m1 ; m2 ; m3.
    %
    % and a foreign enum definition that gives the correspondence correctly
    % but in a different order, such as
    %
    %   :- pragma foreign_enum("C", t/0, [m2 - "f2", m3 - "f3", m1 - "f1"]).
    %
    % the second argument will contain the (nonempty) list "f1", "f2", "f3".
    %
:- type checked_foreign_enum
    --->    checked_foreign_enum(item_foreign_enum_info, one_or_more(string)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_foreign_enum.
:- import_module parse_tree.prog_type.

:- import_module bimap.
:- import_module int.
:- import_module require.
:- import_module pair.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

create_type_ctor_checked_map(InsistOnDefn, IntTypeDefnMap, ImpTypeDefnMap,
        ImpForeignEnumMap, CheckedMap, !Specs) :-
    map.keys_as_set(IntTypeDefnMap, IntDefnTypeCtors),
    map.keys_as_set(ImpTypeDefnMap, ImpDefnTypeCtors),
    % Foreign_enum items are not allowed in interface sections.
    map.keys_as_set(ImpForeignEnumMap, ImpEnumTypeCtors),
    % This union operation depends on the type_ctors in all four maps
    % being qualified exactly the same way. We could require the type_ctor keys
    % to be all fully qualified or all fully unqualified; we chose the former.
    TypeCtors = set.to_sorted_list(
        set.union_list([
            IntDefnTypeCtors, ImpDefnTypeCtors, ImpEnumTypeCtors
        ])),
    list.foldl2(
        check_type_ctor_defns(InsistOnDefn, IntTypeDefnMap, ImpTypeDefnMap,
            ImpForeignEnumMap),
        TypeCtors, map.init, CheckedMap, !Specs),

    map.foldl(add_type_ctor_to_field_name_map, CheckedMap,
        map.init, FieldNameMap),
    map.foldl(report_any_duplicate_field_names, FieldNameMap, !Specs).

:- pred check_type_ctor_defns(maybe_insist_on_defn::in,
    type_ctor_defn_map::in, type_ctor_defn_map::in,
    type_ctor_foreign_enum_map::in, type_ctor::in,
    type_ctor_checked_map::in, type_ctor_checked_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_type_ctor_defns(InsistOnDefn,
        IntTypeDefnMap, ImpTypeDefnMap, ImpForeignEnumMap,
        TypeCtor, !TypeCtorCheckedMap, !Specs) :-
    % A given type constructor may have more than one definition in a module.
    % These definitions may be consistent with one another (such as a du
    % definition in Mercury and a foreign type definition in C), or
    % inconsistent with one another (such as an equivalence definition in
    % Mercury and a foreign_enum definition in Java).
    %
    % We start with the full set of definitions for a type constructor
    % (both type definitions and foreign enum definitions), which may contain
    % inconsistencies. We want to get to a set of definitions which are
    % not just consistent but have been *checked* to be consistent.
    %
    % The overall strategy of this predicate is to go from many possible
    % definitions of a type, which may be inconsistent with one another,
    % to smaller and smaller sets of definitions with fewer and fewer kinds
    % of inconsistencies being possible, until *no* kind of inconsistency
    % is left. At each step, we generate an error message for each definition
    % we discard.
    %
    % Note that an original inconsistent set of definitions may have
    % more than one consistent set of definitions within it. Our algorithm
    % has to pick one. Our approach is to keep definitions that are more
    % likely to be intended (e.g. people rarely define solver types by
    % accident) than others which are more likely to be mistakes (for example,
    % cut-and-paste errors may field foreign type definitions that specify
    % the wrong type name or the wrong foreign language).

    % The first stage is to look for and report inconsistencies that manifest
    % themselves as violations of the "at most one definition of TypeCtor
    % of a given kind in a given module section" rule.
    check_any_type_ctor_defns_for_duplicates(IntTypeDefnMap, TypeCtor,
        IntMaybeDefn, !Specs),
    check_any_type_ctor_defns_for_duplicates(ImpTypeDefnMap, TypeCtor,
        ImpMaybeDefn, !Specs),
    check_any_type_ctor_enums_for_duplicates(ImpForeignEnumMap, TypeCtor,
        ImpMaybeEnumCJCs, ImpLeftOverEnumsCJCs, !Specs),

    % Get the contexts of each different definition in case we later
    % need to generate error messages for them. This is not very efficient
    % in terms of runtime, but it keeps the later code sane.
    ( if map.search(ImpForeignEnumMap, TypeCtor, ImpEnumsCJCsPrime) then
        ImpEnumsCJCs = ImpEnumsCJCsPrime
    else
        ImpEnumsCJCs = c_java_csharp([], [], [])
    ),
    ImpEnumsCJCs = c_java_csharp(ImpEnumsC, ImpEnumsJava, ImpEnumsCsharp),
    ImpEnums = ImpEnumsC ++ ImpEnumsJava ++ ImpEnumsCsharp,

    IntMaybeDefn = type_ctor_maybe_defn(
        IntAbstractSolverMaybeDefn, IntSolverMaybeDefn,
        IntAbstractStdMaybeDefn, IntEqvMaybeDefn, IntDuMaybeDefn,
        IntMaybeDefnCJCs),
    ImpMaybeDefn = type_ctor_maybe_defn(
        ImpAbstractSolverMaybeDefn, ImpSolverMaybeDefn,
        ImpAbstractStdMaybeDefn, ImpEqvMaybeDefn, ImpDuMaybeDefn,
        ImpMaybeDefnCJCs),
    IntMaybeDefnCJCs = c_java_csharp(IntMaybeDefnC, IntMaybeDefnJava,
        IntMaybeDefnCsharp),
    ImpMaybeDefnCJCs = c_java_csharp(ImpMaybeDefnC, ImpMaybeDefnJava,
        ImpMaybeDefnCsharp),

    IntContextAbstractSolver = get_maybe_context(IntAbstractSolverMaybeDefn),
    % IntContextSolver = get_maybe_context(IntSolverMaybeDefn),
    IntContextAbstractStd = get_maybe_context(IntAbstractStdMaybeDefn),
    IntContextEqv = get_maybe_context(IntEqvMaybeDefn),
    IntContextDu = get_maybe_context(IntDuMaybeDefn),
    ImpContextAbstractSolver = get_maybe_context(ImpAbstractSolverMaybeDefn),
    % ImpContextSolver = get_maybe_context(ImpSolverMaybeDefn),
    ImpContextAbstractStd = get_maybe_context(ImpAbstractStdMaybeDefn),
    ImpContextEqv = get_maybe_context(ImpEqvMaybeDefn),
    ImpContextDu = get_maybe_context(ImpDuMaybeDefn),

    IntContextC      = get_maybe_context(IntMaybeDefnC),
    IntContextJava   = get_maybe_context(IntMaybeDefnJava),
    IntContextCsharp = get_maybe_context(IntMaybeDefnCsharp),
    ImpContextC      = get_maybe_context(ImpMaybeDefnC),
    ImpContextJava   = get_maybe_context(ImpMaybeDefnJava),
    ImpContextCsharp = get_maybe_context(ImpMaybeDefnCsharp),

    % Now we have at most one definition of each kind in each section,
    % and no definition in a section in which that kind is not allowed.
    % The general strategy forward is:
    %
    %   - first we look for a solver type definition
    %   - then we look for an equivalence type definition
    %   - then we look for a du type definition
    %   - then we look for a foreign type definition
    %   - then we look for an abstract solver type declaration
    %   - then we look for an abstract nonsolver type declaration
    %
    % If we find a definition of a given kind, then we generate an error
    % message for every other kind of definition that occurs later in
    % that list that is not compatible with the one we found. (There won't be
    % any definitions of a kind that is earlier in the list; if there were,
    % we wouldn't have gone on to test for the later kinds.)
    %
    % In each case, we look first in the interface, and then in the
    % implementation. Solver types are the one exception, because finding
    % a nonabstract definition in the interface section is an error.

    report_any_nonabstract_solver_type_in_int(TypeCtor, IntSolverMaybeDefn,
        !Specs),
    ( if
        % Is there a solver type definition (not declaration) in the
        % implementation? There can't be a valid one in the interface.
        ImpSolverMaybeDefn = yes(ImpSolverDefn)
    then
        report_any_redundant_abstract_type_in_imp(TypeCtor, "implementation",
            ImpAbstractSolverMaybeDefn, !Specs),
        % Print error messages for every Std definition.
        list.foldl(
            report_any_incompatible_type_definition(TypeCtor,
                ImpSolverDefn ^ td_context, "solver type", "implementation"),
            [IntContextAbstractStd, IntContextEqv, IntContextDu,
                IntContextC, IntContextJava, IntContextCsharp,
            ImpContextAbstractStd, ImpContextEqv, ImpContextDu,
                ImpContextC, ImpContextJava, ImpContextCsharp],
            !Specs),
        list.foldl(
            report_incompatible_foreign_enum(TypeCtor,
                ImpSolverDefn ^ td_context, "solver type", "implementation"),
            ImpEnums, !Specs),

        CheckedSolverDefn = solver_type_full(IntAbstractSolverMaybeDefn,
            ImpSolverDefn),
        CheckedDefn = checked_defn_solver(CheckedSolverDefn),
        map.det_insert(TypeCtor, CheckedDefn, !TypeCtorCheckedMap)
    else if
        % TypeCtor is NOT defined as a solver type.
        % Is there an equivalence type definition ...
        ( if
            % ... in the interface?
            IntEqvMaybeDefn = yes(IntEqvDefn)
        then
            EqvDefn = IntEqvDefn,
            EqvWhere = "interface",
            Status = std_eqv_type_mer_exported
        else if
            % ... in the implementation?
            ImpEqvMaybeDefn = yes(ImpEqvDefn)
        then
            EqvDefn = ImpEqvDefn,
            EqvWhere = "implementation",
            (
                IntAbstractStdMaybeDefn = yes(_),
                Status = std_eqv_type_abstract_exported
            ;
                IntAbstractStdMaybeDefn = no,
                Status = std_eqv_type_all_private
            )
        else
            fail
        )
    then
        report_any_redundant_abstract_type_in_imp(TypeCtor, EqvWhere,
            ImpAbstractStdMaybeDefn, !Specs),
        list.foldl(
            report_any_incompatible_type_definition(TypeCtor,
                EqvDefn ^ td_context, "equivalence type", EqvWhere),
            [IntContextAbstractSolver, IntContextDu,
                IntContextC, IntContextJava, IntContextCsharp,
            ImpContextAbstractSolver, ImpContextDu,
                ImpContextC, ImpContextJava, ImpContextCsharp],
            !Specs),
        list.foldl(
            report_incompatible_foreign_enum(TypeCtor,
                EqvDefn ^ td_context, "equivalence type", EqvWhere),
            ImpEnums, !Specs),
        CheckedStdDefn = std_mer_type_eqv(Status, EqvDefn),
        CheckedDefn = checked_defn_std(CheckedStdDefn),
        map.det_insert(TypeCtor, CheckedDefn, !TypeCtorCheckedMap)
    else if
        % TypeCtor is NOT defined as a solver or equivalence type.
        % Is there a du type definition ...
        ( if
            % ... in the interface?
            IntDuMaybeDefn = yes(IntDuDefn)
        then
            DuDefn = IntDuDefn,
            DuSection = ms_interface,
            DuWhere = "interface"
        else if
            % ... in the implementation?
            ImpDuMaybeDefn = yes(ImpDuDefn)
        then
            DuDefn = ImpDuDefn,
            DuSection = ms_implementation,
            DuWhere = "implementation"
        else
            fail
        )
    then
        report_any_redundant_abstract_type_in_imp(TypeCtor, DuWhere,
            ImpAbstractStdMaybeDefn, !Specs),
        list.foldl(
            report_any_incompatible_type_definition(TypeCtor,
                DuDefn ^ td_context, "discriminated union type", DuWhere),
            [IntContextAbstractSolver, ImpContextAbstractSolver],
            !Specs),

        % The status of TypeCtor depends not just on which section
        % the du definition was in and on the presence or absence
        % of an abstact declaration in the interface, but also on
        % which section any foreign language definitions are found in.
        % (All foreign language definitions must be in the same section;
        % if they occur in both sections, we pick one, and report the
        % definitions in the other section as errors.)
        decide_du_foreign_type_section(TypeCtor, DuDefn, DuSection,
            IntAbstractStdMaybeDefn,
            IntMaybeDefnCJCs, ImpMaybeDefnCJCs,
            Status, ChosenMaybeDefnCJCs, !Specs),

        % XXX TYPE_REPN As part of switching over to this new system,
        % we will have to disable users' ability to specify MaybeDirectArgs
        % in source code.
        DetailsDu = DuDefn ^ td_ctor_defn,
        DetailsDu = type_details_du(MaybeSuperType, OoMCtors, _MaybeCanonical,
            _MaybeDirectArgs),
        OoMCtors = one_or_more(HeadCtor, TailCtors),
        (
            MaybeSuperType = not_a_subtype,
            ( if
                ctor_is_constant(HeadCtor, HeadName0),
                ctors_are_all_constants(TailCtors, TailNames0)
            then
                (
                    TailNames0 = [],
                    ( if non_sub_du_type_is_dummy(DetailsDu) then
                        MaybeOnlyConstants =
                            only_plain_constants(HeadName0, [])
                    else
                        MaybeOnlyConstants = not_only_plain_constants
                    )
                ;
                    TailNames0 = [_ | _],
                    ( if non_sub_du_type_is_enum(DetailsDu, _NumFunctors) then
                        MaybeOnlyConstants =
                            only_plain_constants(HeadName0, TailNames0)
                    else
                        MaybeOnlyConstants = not_only_plain_constants
                    )
                )
            else
                MaybeOnlyConstants = not_only_plain_constants
            ),
            (
                MaybeOnlyConstants = not_only_plain_constants,
                list.foldl(
                    non_enum_du_report_any_foreign_enum(TypeCtor, DuDefn),
                    ImpEnums, !Specs),
                CheckedStdDefn =
                    std_mer_type_du_not_all_plain_constants(Status, DuDefn,
                        ChosenMaybeDefnCJCs)
            ;
                MaybeOnlyConstants = only_plain_constants(HeadName, TailNames),
                decide_du_repn_foreign_only_constants(TypeCtor,
                    [HeadName | TailNames], ChosenMaybeDefnCJCs,
                    ImpMaybeEnumCJCs, ImpLeftOverEnumsCJCs,
                    MaybeDefnOrEnumCJCs, !Specs),
                CheckedStdDefn = std_mer_type_du_all_plain_constants(Status,
                    DuDefn, HeadName, TailNames, MaybeDefnOrEnumCJCs)
            )
        ;
            MaybeSuperType = subtype_of(_),
            % A subtype's representation depends on its base type, not only on
            % its own constructors.
            list.foldl(non_enum_du_report_any_foreign_enum(TypeCtor, DuDefn),
                ImpEnums, !Specs),
            CheckedStdDefn = std_mer_type_du_subtype(Status, DuDefn)
        ),

        CheckedDefn = checked_defn_std(CheckedStdDefn),
        map.det_insert(TypeCtor, CheckedDefn, !TypeCtorCheckedMap)
    else if
        % TypeCtor is NOT defined as a solver, equivalence or du type.
        % Is there a foreign type definition in the interface or
        % in the implementation?
        ( IntMaybeDefnC = yes(_)
        ; IntMaybeDefnJava = yes(_)
        ; IntMaybeDefnCsharp = yes(_)
        ; ImpMaybeDefnC = yes(_)
        ; ImpMaybeDefnJava = yes(_)
        ; ImpMaybeDefnCsharp = yes(_)
        )
    then
        IntForeignContexts = get_maybe_type_defn_contexts([
            IntMaybeDefnC, IntMaybeDefnJava, IntMaybeDefnCsharp]),
        list.sort(IntForeignContexts, SortedIntForeignContexts),
        (
            SortedIntForeignContexts = [FirstForeignContext | _],
            ForeignWhere = "interface"
        ;
            SortedIntForeignContexts = [],
            ImpForeignContexts = get_maybe_type_defn_contexts([
                ImpMaybeDefnC, ImpMaybeDefnJava, ImpMaybeDefnCsharp]),
            list.sort(ImpForeignContexts, SortedImpForeignContexts),
            FirstForeignContext = list.det_head(SortedImpForeignContexts),
            ForeignWhere = "implementation"
        ),
        list.foldl(
            report_any_incompatible_type_definition(TypeCtor,
                FirstForeignContext, "foreign type", ForeignWhere),
            [IntContextAbstractSolver, ImpContextAbstractSolver],
            !Specs),
        list.foldl(
            report_incompatible_foreign_enum(TypeCtor,
                FirstForeignContext, "foreign type", ForeignWhere),
            ImpEnums, !Specs),

        decide_only_foreign_type_section(TypeCtor,
            IntAbstractStdMaybeDefn, ImpAbstractStdMaybeDefn,
            IntMaybeDefnCJCs, ImpMaybeDefnCJCs,
            Status, ChosenAbstractStdDefn, ChosenMaybeDefnCJCs, !Specs),

        CheckedStdDefn = std_mer_type_abstract(Status,
            ChosenAbstractStdDefn, ChosenMaybeDefnCJCs),
        CheckedDefn = checked_defn_std(CheckedStdDefn),
        map.det_insert(TypeCtor, CheckedDefn, !TypeCtorCheckedMap)
    else if
        % TypeCtor is NOT defined as a solver, equivalence, du or foreign type.
        % Does it have a declaration as a solver type ...
        ( if
            % ... in the interface?
            IntAbstractSolverMaybeDefn = yes(IntAbstractSolverDefn)
        then
            AbstractSolverDefn = IntAbstractSolverDefn,
            AbstractSolverWhere = "interface",
            Status = abstract_solver_type_exported
        else if
            % ... in the implementation?
            ImpAbstractSolverMaybeDefn = yes(ImpAbstractSolverDefn)
        then
            AbstractSolverDefn = ImpAbstractSolverDefn,
            AbstractSolverWhere = "implementation",
            Status = abstract_solver_type_private
        else
            fail
        )
    then
        maybe_report_declared_but_undefined_type(InsistOnDefn, TypeCtor,
            AbstractSolverDefn, !Specs),
        list.foldl(
            report_any_incompatible_type_definition(TypeCtor,
                AbstractSolverDefn ^ td_context, "solver type",
                AbstractSolverWhere),
            [IntContextAbstractStd, ImpContextAbstractStd],
            !Specs),
        list.foldl(
            report_incompatible_foreign_enum(TypeCtor,
                AbstractSolverDefn ^ td_context, "solver type",
                AbstractSolverWhere),
            ImpEnums, !Specs),
        % XXX Should we report any foreign enums using this code?
        % It would say that TypeCtor is not defined, while the code
        % above says it is a solver type.
        % list.foldl(
        %   report_any_foreign_enum_for_undefined_type(TypeCtor,
        %       "undefined"),
        %   [ImpMaybeEnumC, ImpMaybeEnumJava, ImpMaybeEnumCsharp],
        %   !Specs),
        CheckedSolverDefn = solver_type_abstract(Status, AbstractSolverDefn),
        CheckedDefn = checked_defn_solver(CheckedSolverDefn),
        map.det_insert(TypeCtor, CheckedDefn, !TypeCtorCheckedMap)
    else if
        % TypeCtor is NOT defined as a solver, equivalence, du or foreign type,
        % and it is NOT declared as a solver type.
        % Does it have a declaration as a non-solver type ...
        ( if
            % ... in the interface?
            IntAbstractStdMaybeDefn = yes(IntAbstractStdDefn)
        then
            AbstractStdDefn = IntAbstractStdDefn,
            Status = std_abs_type_abstract_exported
        else if
            % ... in the implementation?
            ImpAbstractStdMaybeDefn = yes(ImpAbstractStdDefn)
        then
            AbstractStdDefn = ImpAbstractStdDefn,
            Status = std_abs_type_all_private
        else
            fail
        )
    then
        maybe_report_declared_but_undefined_type(InsistOnDefn, TypeCtor,
            AbstractStdDefn, !Specs),
        list.foldl(
            report_foreign_enum_for_undefined_type(TypeCtor, "undefined"),
            ImpEnums, !Specs),
        MaybeEnumCJCs = c_java_csharp(no, no, no),
        CheckedStdDefn = std_mer_type_abstract(Status,
            AbstractStdDefn, MaybeEnumCJCs),
        CheckedDefn = checked_defn_std(CheckedStdDefn),
        map.det_insert(TypeCtor, CheckedDefn, !TypeCtorCheckedMap)
    else
        % TypeCtor is NOT defined as a solver, equivalence, du or foreign type,
        % and it is NOT declared either as a solver or as a nonsolver type.
        % This means it has no type definition whatsoever. The only way
        % we can get here is if it has any foreign enum definitions.
        list.foldl(
            report_foreign_enum_for_undefined_type(TypeCtor, "undeclared"),
            ImpEnums, !Specs)
        % There is no actual definition to add to !TypeCtorCheckedMap.
    ).

:- func get_maybe_context(maybe(item_type_defn_info_general(T)))
    = maybe(prog_context).

get_maybe_context(no) = no.
get_maybe_context(yes(TypeDefnInfo)) = yes(TypeDefnInfo ^ td_context).

:- pred decide_du_foreign_type_section(type_ctor::in,
    item_type_defn_info_du::in, module_section::in,
    maybe(item_type_defn_info_abstract)::in,
    c_j_cs_maybe_defn::in, c_j_cs_maybe_defn::in,
    std_du_type_status::out, c_j_cs_maybe_defn::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_du_foreign_type_section(TypeCtor, DuDefn, DuSection,
        IntAbstractStdMaybeDefn, IntMaybeDefnCJCs, ImpMaybeDefnCJCs,
        Status, ChosenMaybeDefnCJCs, !Specs) :-
    IntMaybeDefnCJCs = c_java_csharp(IntMaybeDefnC, IntMaybeDefnJava,
        IntMaybeDefnCsharp),
    ImpMaybeDefnCJCs = c_java_csharp(ImpMaybeDefnC, ImpMaybeDefnJava,
        ImpMaybeDefnCsharp),
    (
        DuSection = ms_interface,
        IntContexts = get_maybe_type_defn_contexts([IntMaybeDefnC,
            IntMaybeDefnJava, IntMaybeDefnCsharp]),
        (
            IntContexts = [FirstIntContext | _],
            Status = std_du_type_mer_ft_exported,
            list.foldl(
                foreign_int_report_any_foreign_defn_in_imp(TypeCtor,
                    FirstIntContext),
                [ImpMaybeDefnC, ImpMaybeDefnJava, ImpMaybeDefnCsharp],
                !Specs),
            ChosenMaybeDefnCJCs = IntMaybeDefnCJCs
        ;
            IntContexts = [],
            Status = std_du_type_mer_exported,
            ChosenMaybeDefnCJCs = ImpMaybeDefnCJCs
        )
    ;
        DuSection = ms_implementation,
        (
            IntAbstractStdMaybeDefn = yes(_),
            Status = std_du_type_abstract_exported
        ;
            IntAbstractStdMaybeDefn = no,
            Status = std_du_type_all_private
        ),
        list.foldl(
            du_imp_report_any_foreign_defn_in_int(TypeCtor, DuDefn),
            [IntMaybeDefnC, IntMaybeDefnJava, IntMaybeDefnCsharp],
            !Specs),
        ChosenMaybeDefnCJCs = ImpMaybeDefnCJCs
    ).

:- pred decide_only_foreign_type_section(type_ctor::in,
    maybe(item_type_defn_info_abstract)::in,
        maybe(item_type_defn_info_abstract)::in,
    c_j_cs_maybe_defn::in, c_j_cs_maybe_defn::in,
    std_abs_type_status::out,
    item_type_defn_info_abstract::out, c_j_cs_maybe_defn::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_only_foreign_type_section(TypeCtor,
        IntAbstractStdMaybeDefn, ImpAbstractStdMaybeDefn,
        IntMaybeDefnCJCs, ImpMaybeDefnCJCs,
        Status, AbstractStdDefn, ChosenMaybeDefnCJCs, !Specs) :-
    IntMaybeDefnCJCs = c_java_csharp(IntMaybeDefnC, IntMaybeDefnJava,
        IntMaybeDefnCsharp),
    ImpMaybeDefnCJCs = c_java_csharp(ImpMaybeDefnC, ImpMaybeDefnJava,
        ImpMaybeDefnCsharp),
    (
        IntAbstractStdMaybeDefn = yes(IntAbstractStdDefn),
        AbstractStdDefn = IntAbstractStdDefn,
        IntContexts = get_maybe_type_defn_contexts([IntMaybeDefnC,
            IntMaybeDefnJava, IntMaybeDefnCsharp]),
        list.sort(IntContexts, SortedIntContexts),
        (
            SortedIntContexts = [FirstIntContext | _],
            Status = std_abs_type_ft_exported,
            list.foldl(
                foreign_int_report_any_foreign_defn_in_imp(TypeCtor,
                    FirstIntContext),
                [ImpMaybeDefnC, ImpMaybeDefnJava, ImpMaybeDefnCsharp],
                !Specs),
            ChosenMaybeDefnCJCs = IntMaybeDefnCJCs
        ;
            SortedIntContexts = [],
            Status = std_abs_type_abstract_exported,
            ChosenMaybeDefnCJCs = ImpMaybeDefnCJCs
        )
    ;
        IntAbstractStdMaybeDefn = no,
        Status = std_abs_type_all_private,
        (
            ImpAbstractStdMaybeDefn = yes(ImpAbstractStdDefn),
            AbstractStdDefn = ImpAbstractStdDefn,
            list.foldl(
                du_imp_report_any_foreign_defn_in_int(TypeCtor,
                    ImpAbstractStdDefn),
                [IntMaybeDefnC, IntMaybeDefnJava, IntMaybeDefnCsharp],
                !Specs)
        ;
            ImpAbstractStdMaybeDefn = no,
            list.foldl(
                report_any_foreign_type_without_declaration(TypeCtor),
                [IntMaybeDefnC, IntMaybeDefnJava, IntMaybeDefnCsharp,
                ImpMaybeDefnC, ImpMaybeDefnJava, ImpMaybeDefnCsharp],
                !Specs),

            Defns = get_maybe_type_defns([
                IntMaybeDefnC, IntMaybeDefnJava, IntMaybeDefnCsharp,
                ImpMaybeDefnC, ImpMaybeDefnJava, ImpMaybeDefnCsharp]),

            % Defns cannot be empty because our caller calls us only if
            % at least one of the foreign language definitions is yes(_).
            % This *also* means that we will generate at least one
            % error message above.
            FirstDefn = list.det_head(Defns),
            % Pretend that there *was* an abstract declaration of the type,
            % since it is at least 99% likely that the programmer's response
            % to the above error message will be to add one. Manufacturing
            % this item out of thin air will suppress any error messages
            % that would be inappropriate in the presence of this repair,
            % but they won't lead to any problems, since the presence of
            % the error means that we won't actually do anything with the
            % typr_ctor_checked_defn we are constructing.
            AbstractStdDefn = FirstDefn ^ td_ctor_defn := abstract_type_general
        ),
        ChosenMaybeDefnCJCs = ImpMaybeDefnCJCs
    ).

:- pred decide_du_repn_foreign_only_constants(type_ctor::in,
    list(string)::in, c_j_cs_maybe_defn::in,
    c_j_cs_maybe_enum::in, c_j_cs_enums::in,
    c_j_cs_maybe_defn_or_enum::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_du_repn_foreign_only_constants(TypeCtor, CtorNames,
        MaybeDefnCJCs, MaybeEnumCJCs, LeftOverEnumsCJCs,
        MaybeDefnOrEnumCJCs, !Specs) :-
    % If TypeCtor has more than one enum definition for a given foreign
    % language, we pick on to return in MaybeEnumCJCs, but we return
    % all the others as well in LeftOverEnumsCJCs so that our caller
    % can generate error messages for them where required.
    set_tree234.list_to_set(CtorNames, CtorNamesSet),
    MaybeDefnCJCs = c_java_csharp(MaybeDefnC, MaybeDefnJava, MaybeDefnCsharp),
    MaybeEnumCJCs = c_java_csharp(MaybeEnumC, MaybeEnumJava, MaybeEnumCsharp),
    LeftOverEnumsCJCs = c_java_csharp(LeftOverEnumsC, LeftOverEnumsJava,
        LeftOverEnumsCsharp),

    decide_du_repn_foreign_only_constants_lang(TypeCtor,
        CtorNames, CtorNamesSet, MaybeDefnC,
        MaybeEnumC, LeftOverEnumsC, MaybeDefnOrEnumC, !Specs),
    decide_du_repn_foreign_only_constants_lang(TypeCtor,
        CtorNames, CtorNamesSet, MaybeDefnJava,
        MaybeEnumJava, LeftOverEnumsJava, MaybeDefnOrEnumJava, !Specs),
    decide_du_repn_foreign_only_constants_lang(TypeCtor,
        CtorNames, CtorNamesSet, MaybeDefnCsharp,
        MaybeEnumCsharp, LeftOverEnumsCsharp, MaybeDefnOrEnumCsharp, !Specs),

    MaybeDefnOrEnumCJCs = c_java_csharp(MaybeDefnOrEnumC, MaybeDefnOrEnumJava,
        MaybeDefnOrEnumCsharp).

:- pred decide_du_repn_foreign_only_constants_lang(type_ctor::in,
    list(string)::in, set_tree234(string)::in,
    maybe(item_type_defn_info_foreign)::in,
    maybe(item_foreign_enum_info)::in, list(item_foreign_enum_info)::in,
    maybe(foreign_type_or_enum)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_du_repn_foreign_only_constants_lang(TypeCtor, CtorNames, CtorNamesSet,
        MaybeDefn, MaybeEnum, LeftOverEnums, MaybeDefnOrEnum, !Specs) :-
    (
        MaybeEnum = no,
        expect(unify(LeftOverEnums, []), $pred,
            "MaybeEnum = no but LeftOverEnums != []"),
        MaybeCheckedForeignEnum = no
    ;
        MaybeEnum = yes(Enum),
        build_mercury_foreign_enum_map(TypeCtor, CtorNames, CtorNamesSet,
            Enum, HeadFETuples),
        list.map(
            build_mercury_foreign_enum_map(TypeCtor, CtorNames, CtorNamesSet),
            LeftOverEnums, TailFETuples),
        AllFETuples = [HeadFETuples | TailFETuples],
        pick_first_error_free_enum_if_any(AllFETuples,
            MaybeCheckedForeignEnum),
        pick_up_all_errors(AllFETuples, !Specs)
    ),
    (
        MaybeDefn = yes(Defn),
        MaybeDefnOrEnum = yes(foreign_type_or_enum_type(Defn))
    ;
        MaybeDefn = no,
        (
            MaybeCheckedForeignEnum = no,
            MaybeDefnOrEnum = no
        ;
            MaybeCheckedForeignEnum = yes(CheckedForeignEnum),
            MaybeDefnOrEnum =
                yes(foreign_type_or_enum_enum(CheckedForeignEnum))
        )
    ).

:- pred pick_first_error_free_enum_if_any(
    list({checked_foreign_enum, list(error_spec)})::in,
    maybe(checked_foreign_enum)::out) is det.

pick_first_error_free_enum_if_any([], no).
pick_first_error_free_enum_if_any([HeadFETuple | TailFETuples],
        MaybeCheckedForeignEnum) :-
    HeadFETuple = {HeadCheckedForeignEnum, HeadSpecs},
    (
        HeadSpecs = [],
        MaybeCheckedForeignEnum = yes(HeadCheckedForeignEnum)
    ;
        HeadSpecs = [_ | _],
        pick_first_error_free_enum_if_any(TailFETuples,
            MaybeCheckedForeignEnum)
    ).

:- pred pick_up_all_errors(
    list({checked_foreign_enum, list(error_spec)})::in,
    list(error_spec)::in, list(error_spec)::out) is det.

pick_up_all_errors([], !Specs).
pick_up_all_errors([HeadFETuple | TailFETuples], !Specs) :-
    HeadFETuple = {_HeadCheckedForeignEnum, HeadSpecs},
    !:Specs = HeadSpecs ++ !.Specs,
    pick_up_all_errors(TailFETuples, !Specs).

:- pred build_mercury_foreign_enum_map(type_ctor::in,
    list(string)::in, set_tree234(string)::in,
    item_foreign_enum_info::in,
    {checked_foreign_enum, list(error_spec)}::out) is det.

build_mercury_foreign_enum_map(TypeCtor, CtorNames, CtorNamesSet,
        ForeignEnum, {CheckedForeignEnum, Specs}) :-
    ForeignEnum = item_foreign_enum_info(_Lang, _TypeCtor, MercuryForeignOoM,
        Context, _SeqNum),
    MercuryForeignAL = one_or_more_to_list(MercuryForeignOoM),
    ContextPieces = [words("In"), pragma_decl("foreign_export_enum"),
        words("declaration for type"), unqual_type_ctor(TypeCtor),
        suffix(":"), nl],

    TypeCtor = type_ctor(TypeCtorSymName, _),
    det_sym_name_get_module_name(TypeCtorSymName, TypeCtorModuleName),

    build_ctor_name_to_foreign_name_map(for_foreign_enum,
        Context, ContextPieces, TypeCtorModuleName, CtorNamesSet,
        MercuryForeignAL, MercuryForeignBiMap, [], Specs),
    (
        Specs = [],
        bimap.apply_forward_map_to_list(MercuryForeignBiMap, CtorNames,
            ForeignNames)
    ;
        Specs = [_ | _],
        make_up_dummy_foreign_names(CtorNames, 1, ForeignNames)
    ),
    (
        ForeignNames = [],
        % There should be exactly one ForeignName for every CtorName.
        unexpected($pred, "enum type with no constructors")
    ;
        ForeignNames = [HeadForeignName | TailForeignNames],
        ForeignNameOoM = one_or_more(HeadForeignName, TailForeignNames)
    ),
    CheckedForeignEnum = checked_foreign_enum(ForeignEnum, ForeignNameOoM).

:- pred make_up_dummy_foreign_names(list(T)::in, int::in, list(string)::out)
    is det.

make_up_dummy_foreign_names([], _, []).
make_up_dummy_foreign_names([_Ctor | Ctors], CtorNum,
        [ForeignName | ForeignNames]) :-
    ForeignName = string.format("dummy_foreign_name_%d", [i(CtorNum)]),
    make_up_dummy_foreign_names(Ctors, CtorNum + 1, ForeignNames).

:- pred non_enum_du_report_any_foreign_enum(type_ctor::in,
    item_type_defn_info_du::in, item_foreign_enum_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

non_enum_du_report_any_foreign_enum(TypeCtor, DuDefn, Enum, !Specs) :-
    EnumPieces = [words("Error: the Mercury definition of"),
        unqual_type_ctor(TypeCtor), words("is not an enumeration type,"),
        words("so there must not be any"),
        pragma_decl("foreign_enum"), words("declarations for it."), nl],
    DuPieces = [words("That Mercury definition is here."), nl],
    Spec = error_spec($pred, severity_warning, phase_term_to_parse_tree,
        [simplest_msg(Enum ^ fe_context, EnumPieces),
        simplest_msg(DuDefn ^ td_context, DuPieces)]),
    !:Specs = [Spec | !.Specs].

:- pred du_imp_report_any_foreign_defn_in_int(type_ctor::in,
    item_type_defn_info_general(T)::in, maybe(item_type_defn_info_foreign)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

du_imp_report_any_foreign_defn_in_int(TypeCtor, DuDefn, MaybeForeignDefn,
        !Specs) :-
    (
        MaybeForeignDefn = no
    ;
        MaybeForeignDefn = yes(ForeignDefn),
        ForeignPieces = [words("Error: since the Mercury definition of"),
            unqual_type_ctor(TypeCtor),
            words("is in the implementation section,"),
            words("any foreign type definitions for it"),
            words("must be in the implementation section as well."), nl],
        DuPieces = [words("That Mercury definition is here."), nl],
        Spec = error_spec($pred, severity_warning, phase_term_to_parse_tree,
            [simplest_msg(ForeignDefn ^ td_context, ForeignPieces),
            simplest_msg(DuDefn ^ td_context, DuPieces)]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred report_any_foreign_type_without_declaration(type_ctor::in,
    maybe(item_type_defn_info_foreign)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_any_foreign_type_without_declaration(TypeCtor, MaybeForeignDefn,
        !Specs) :-
    (
        MaybeForeignDefn = no
    ;
        MaybeForeignDefn = yes(ForeignDefn),
        Pieces = [words("Error: a"),
            pragma_decl("foreign_type"), words("declaration for"),
            unqual_type_ctor(TypeCtor), words("without either"),
            words("a Mercury definition or a Mercury declaration for"),
            unqual_type_ctor(TypeCtor), suffix("."), nl],
        Spec = simplest_spec($pred, severity_warning, phase_term_to_parse_tree,
            ForeignDefn ^ td_context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred foreign_int_report_any_foreign_defn_in_imp(type_ctor::in,
    prog_context::in, maybe(item_type_defn_info_foreign)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

foreign_int_report_any_foreign_defn_in_imp(TypeCtor, IntForeignContext,
        MaybeImpForeignDefn, !Specs) :-
    (
        MaybeImpForeignDefn = no
    ;
        MaybeImpForeignDefn = yes(ImpForeignDefn),
        ImpPieces = [words("Error: since some foreign language definition"),
            words("of"), unqual_type_ctor(TypeCtor),
            words("is in the interface section,"),
            words("all other foreign language definitions for it"),
            words("must be in the interface section as well."), nl],
        IntPieces = [words("That foreign definition in the interface"),
            words("is here."), nl],
        Spec = error_spec($pred, severity_warning, phase_term_to_parse_tree,
            [simplest_msg(ImpForeignDefn ^ td_context, ImpPieces),
            simplest_msg(IntForeignContext, IntPieces)]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred report_any_nonabstract_solver_type_in_int(type_ctor::in,
    maybe(item_type_defn_info_solver)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_any_nonabstract_solver_type_in_int(TypeCtor, MaybeDefn, !Specs) :-
    (
        MaybeDefn = no
    ;
        MaybeDefn = yes(Defn),
        Pieces = [words("Error: a solver type such as"),
            unqual_type_ctor(TypeCtor), words("may be defined"),
            words("(as opposed to declared)"),
            words("only in the implementation section."), nl],
        Spec = simplest_spec($pred, severity_warning, phase_term_to_parse_tree,
            Defn ^ td_context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred report_any_redundant_abstract_type_in_imp(type_ctor::in, string::in,
    maybe(item_type_defn_info_abstract)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_any_redundant_abstract_type_in_imp(TypeCtor, Section,
        MaybeImpAbstractDefn, !Specs) :-
    (
        MaybeImpAbstractDefn = no
    ;
        MaybeImpAbstractDefn = yes(ImpAbstractDefn),
        Pieces = [words("Error: this declaration of"),
            unqual_type_ctor(TypeCtor), words("is redundant,"),
            words("since the type has a definition in the"),
            words(Section), words("section."), nl],
        Spec = simplest_spec($pred, severity_warning, phase_term_to_parse_tree,
            ImpAbstractDefn ^ td_context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred report_any_incompatible_type_definition(type_ctor::in,
    prog_context::in, string::in, string::in,
    maybe(prog_context)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_any_incompatible_type_definition(TypeCtor, UsedContext, Kind, Section,
        MaybeDefnContext, !Specs) :-
    (
        MaybeDefnContext = no
    ;
        MaybeDefnContext = yes(DefnContext),
        MainPieces = [words("Error: this definition of"),
            unqual_type_ctor(TypeCtor), words("is incompatible"),
            words("with the"), words(Kind), words("definition"),
            words("in the"), words(Section), words("section."), nl],
        UsedPieces = [words("That definition is here."), nl],
        Spec = error_spec($pred, severity_warning, phase_term_to_parse_tree,
            [simplest_msg(DefnContext, MainPieces),
            simplest_msg(UsedContext, UsedPieces)]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred report_incompatible_foreign_enum(type_ctor::in, prog_context::in,
    string::in, string::in, item_foreign_enum_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_incompatible_foreign_enum(TypeCtor, UsedContext, Kind, Section, Enum,
        !Specs) :-
    MainPieces = [words("Error: this"), pragma_decl("foreign_enum"),
        words("declaration for"), unqual_type_ctor(TypeCtor),
        words("is incompatible with the"), words(Kind),
        words("definition in the"), words(Section), words("section."), nl],
    UsedPieces = [words("That definition is here."), nl],
    Spec = error_spec($pred, severity_warning, phase_term_to_parse_tree,
        [simplest_msg(Enum ^ fe_context, MainPieces),
        simplest_msg(UsedContext, UsedPieces)]),
    !:Specs = [Spec | !.Specs].

:- pred report_foreign_enum_for_undefined_type(type_ctor::in, string::in,
    item_foreign_enum_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_foreign_enum_for_undefined_type(TypeCtor, UndefOrUndecl, Enum,
        !Specs) :-
    Pieces = [words("Error:"), pragma_decl("foreign_enum"),
        words("declaration for the"), words(UndefOrUndecl),
        words("type"), unqual_type_ctor(TypeCtor), suffix("."), nl],
    Spec = simplest_spec($pred, severity_warning, phase_term_to_parse_tree,
        Enum ^ fe_context, Pieces),
    !:Specs = [Spec | !.Specs].

:- pred maybe_report_declared_but_undefined_type(maybe_insist_on_defn::in,
    type_ctor::in, item_type_defn_info_abstract::in,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_report_declared_but_undefined_type(InsistOnDefn, TypeCtor, AbsTypeDefn,
        !Specs) :-
    TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
    det_sym_name_get_module_name(TypeCtorSymName, TypeCtorModuleName),
    BuiltinTypeCtors = builtin_type_ctors_with_no_hlds_type_defn,
    ( if
        InsistOnDefn = do_insist_on_defn,
        not any_mercury_builtin_module(TypeCtorModuleName),

        % Several of the type defineds in type_desc do not have
        % Mercury definitions.
        not TypeCtorModuleName = unqualified("type_desc"),
        not list.member(TypeCtor, BuiltinTypeCtors)
    then
        Pieces = [words("Error: the type"), unqual_type_ctor(TypeCtor),
            words("has this declaration, but it has no definition."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            AbsTypeDefn ^ td_context, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        % Do not report undefined types in builtin modules, since
        % their implementation (such as c_pointer's) may be handwritten.
        true
    ).

    % TypeDefnMap should contain all the type definitions in a given section
    % of a module.
    %
    % For a given type constructor in that module section, there should be
    % at most one definition of each kind (e.g. there must not be two
    % definitions as a du type), though there may be two or more definitions
    % of different kids (e.g. a du definition, a C definition and a Java
    % definition).
    %
:- pred check_any_type_ctor_defns_for_duplicates(type_ctor_defn_map::in,
    type_ctor::in, type_ctor_maybe_defn::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_any_type_ctor_defns_for_duplicates(TypeDefnMap, TypeCtor,
        MaybeDefn, !Specs) :-
    ( if map.search(TypeDefnMap, TypeCtor, AllDefns) then
        AllDefns = type_ctor_all_defns(AbstractSolverDefns, SolverDefns,
            AbstractNonSolverDefns, EqvDefns, DuDefns, ForeignDefns),
        at_most_one_type_defn("abstract solver type", TypeCtor,
            AbstractSolverDefns, AbstractSolverMaybeDefn, !Specs),
        at_most_one_type_defn("solver type", TypeCtor,
            SolverDefns, SolverMaybeDefn, !Specs),
        at_most_one_type_defn("abstract type", TypeCtor,
            AbstractNonSolverDefns, AbstractNonSolverMaybeDefn, !Specs),
        at_most_one_type_defn("equivalence type", TypeCtor,
            EqvDefns, EqvMaybeDefn, !Specs),
        at_most_one_type_defn("discriminated union type", TypeCtor,
            DuDefns, DuMaybeDefn, !Specs),
        at_most_one_foreign_type_for_all_langs(TypeCtor,
            ForeignDefns, ForeignMaybeDefn, !Specs),
        MaybeDefn = type_ctor_maybe_defn(
            AbstractSolverMaybeDefn, SolverMaybeDefn,
            AbstractNonSolverMaybeDefn, EqvMaybeDefn, DuMaybeDefn,
            ForeignMaybeDefn)
    else
        MaybeDefn = type_ctor_maybe_defn(no, no, no, no, no,
            c_java_csharp(no, no, no))
    ).

:- pred at_most_one_type_defn(string::in, type_ctor::in,
    list(item_type_defn_info_general(T))::in,
        maybe(item_type_defn_info_general(T))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

at_most_one_type_defn(Kind, TypeCtor, TypeDefns, MaybeTypeDefn, !Specs) :-
    (
        TypeDefns = [],
        MaybeTypeDefn = no
    ;
        TypeDefns = [TypeDefn],
        MaybeTypeDefn = yes(TypeDefn)
    ;
        TypeDefns = [TypeDefn, _ | _],
        MaybeTypeDefn = yes(TypeDefn),
        list.foldl(accumulate_type_defn_contexts, TypeDefns,
            set.init, Contexts),
        ( if set.remove_least(LeastContext, Contexts, OtherContexts) then
            set.foldl(
                report_duplicate_type_defn(Kind, TypeCtor, LeastContext),
                OtherContexts, !Specs)
        else
            unexpected($pred, "nonempty set doesn't have least element")
        )
    ).

:- pred at_most_one_foreign_type_for_all_langs(type_ctor::in,
    c_j_cs_defns::in, c_j_cs_maybe_defn::out,
    list(error_spec)::in, list(error_spec)::out) is det.

at_most_one_foreign_type_for_all_langs(TypeCtor, DefnsCJCs, MaybeDefnCJCs,
        !Specs) :-
    DefnsCJCs = c_java_csharp(DefnsC, DefnsJava, DefnsCsharp),
    at_most_one_foreign_type_for_lang(TypeCtor, lang_c,
        DefnsC, MaybeDefnC, !Specs),
    at_most_one_foreign_type_for_lang(TypeCtor, lang_java,
        DefnsJava, MaybeDefnJava, !Specs),
    at_most_one_foreign_type_for_lang(TypeCtor, lang_csharp,
        DefnsCsharp, MaybeDefnCsharp, !Specs),
    MaybeDefnCJCs = c_java_csharp(MaybeDefnC, MaybeDefnJava, MaybeDefnCsharp).

:- pred check_any_type_ctor_enums_for_duplicates(
    type_ctor_foreign_enum_map::in, type_ctor::in,
    c_j_cs_maybe_enum::out, c_j_cs_enums::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_any_type_ctor_enums_for_duplicates(ForeignEnumMap, TypeCtor,
        MaybeEnumCJCs, LeftOverEnumCJCse, !Specs) :-
    ( if map.search(ForeignEnumMap, TypeCtor, AllEnums) then
        at_most_one_foreign_enum_for_all_langs(TypeCtor,
            AllEnums, MaybeEnumCJCs, LeftOverEnumCJCse, !Specs)
    else
        MaybeEnumCJCs = c_java_csharp(no, no, no),
        LeftOverEnumCJCse = c_java_csharp([], [], [])
    ).

:- pred at_most_one_foreign_enum_for_all_langs(type_ctor::in,
    c_j_cs_enums::in, c_j_cs_maybe_enum::out, c_j_cs_enums::out,
    list(error_spec)::in, list(error_spec)::out) is det.

at_most_one_foreign_enum_for_all_langs(TypeCtor, AllEnumsCJCs,
        MaybeEnumCJCs, LeftOverEnumsCJCs, !Specs) :-
    AllEnumsCJCs = c_java_csharp(EnumsC, EnumsJava, EnumsCsharp),
    at_most_one_foreign_enum_for_lang(TypeCtor, lang_c,
        EnumsC, MaybeEnumC, LeftOverEnumsC, !Specs),
    at_most_one_foreign_enum_for_lang(TypeCtor, lang_java,
        EnumsJava, MaybeEnumJava, LeftOverEnumsJava, !Specs),
    at_most_one_foreign_enum_for_lang(TypeCtor, lang_csharp,
        EnumsCsharp, MaybeEnumCsharp, LeftOverEnumsCsharp, !Specs),
    MaybeEnumCJCs = c_java_csharp(MaybeEnumC, MaybeEnumJava, MaybeEnumCsharp),
    LeftOverEnumsCJCs = c_java_csharp(LeftOverEnumsC,
        LeftOverEnumsJava, LeftOverEnumsCsharp).

:- pred report_duplicate_type_defn(string::in, type_ctor::in,
    prog_context::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_duplicate_type_defn(Kind, TypeCtor, LeastContext, Context, !Specs) :-
    MainPieces = [words("Error: duplicate"), words(Kind),
        words("definition for"), unqual_type_ctor(TypeCtor), suffix("."), nl],
    LeastPieces = [words("The original definition is here."), nl],
    Spec = error_spec($pred, severity_error, phase_term_to_parse_tree,
        [simplest_msg(Context, MainPieces),
        simplest_msg(LeastContext, LeastPieces)]),
    !:Specs = [Spec | !.Specs].

:- pred at_most_one_foreign_type_for_lang(type_ctor::in, foreign_language::in,
    list(item_type_defn_info_foreign)::in,
    maybe(item_type_defn_info_foreign)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

at_most_one_foreign_type_for_lang(TypeCtor, Lang, TypeDefns,
        MaybeTypeDefn, !Specs) :-
    (
        TypeDefns = [],
        MaybeTypeDefn = no
    ;
        TypeDefns = [TypeDefn],
        MaybeTypeDefn = yes(TypeDefn)
    ;
        TypeDefns = [TypeDefn, _ | _],
        MaybeTypeDefn = yes(TypeDefn),
        list.foldl(accumulate_type_defn_contexts, TypeDefns,
            set.init, Contexts),
        ( if set.remove_least(LeastContext, Contexts, OtherContexts) then
            set.foldl(
                report_duplicate_foreign_defn("type", TypeCtor, Lang,
                    LeastContext),
                OtherContexts, !Specs)
        else
            unexpected($pred, "nonempty set doesn't have least element")
        )
    ).

:- pred at_most_one_foreign_enum_for_lang(type_ctor::in, foreign_language::in,
    list(item_foreign_enum_info)::in,
    maybe(item_foreign_enum_info)::out, list(item_foreign_enum_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

at_most_one_foreign_enum_for_lang(TypeCtor, Lang, ForeignEnums,
        MaybeForeignEnum, LeftOverForeignEnums, !Specs) :-
    list.sort(compare_foreign_enum_infos_by_context,
        ForeignEnums, SortedForeignEnums),
    (
        SortedForeignEnums = [],
        MaybeForeignEnum = no,
        LeftOverForeignEnums = []
    ;
        SortedForeignEnums = [ForeignEnum],
        MaybeForeignEnum = yes(ForeignEnum),
        LeftOverForeignEnums = []
    ;
        SortedForeignEnums = [ForeignEnum | LeftOverForeignEnums],
        LeftOverForeignEnums = [_ | _],
        MaybeForeignEnum = yes(ForeignEnum),

        LeastContext = ForeignEnum ^ fe_context,
        LeftOverContexts = list.map(project_foreign_enum_context,
            LeftOverForeignEnums),
        list.foldl(
            report_duplicate_foreign_defn("enum", TypeCtor, Lang,
                LeastContext),
            LeftOverContexts, !Specs)
    ).

:- pred compare_foreign_enum_infos_by_context(
    item_foreign_enum_info::in, item_foreign_enum_info::in,
    comparison_result::out) is det.

compare_foreign_enum_infos_by_context(ForeignEnumA, ForeignEnumB, Cmp) :-
    ContextA = ForeignEnumA ^ fe_context,
    ContextB = ForeignEnumB ^ fe_context,
    compare(Cmp, ContextA, ContextB).

:- func project_foreign_enum_context(item_foreign_enum_info) = prog_context.

project_foreign_enum_context(ForeignEnum) = ForeignEnum ^ fe_context.

:- pred accumulate_type_defn_contexts(item_type_defn_info_general(T)::in,
    set(prog_context)::in, set(prog_context)::out) is det.

accumulate_type_defn_contexts(TypeDefn, !Contexts) :-
    set.insert(get_type_defn_info_context(TypeDefn), !Contexts).

:- func get_type_defn_info_context(item_type_defn_info_general(T))
    = prog_context.

get_type_defn_info_context(TypeDefn) = TypeDefn ^ td_context.

:- pred report_duplicate_foreign_defn(string::in,
    type_ctor::in, foreign_language::in, prog_context::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_duplicate_foreign_defn(TypeOrEnum, TypeCtor, Lang,
        LeastContext, Context, !Specs) :-
    MainPieces = [words("Error: duplicate foreign"), fixed(TypeOrEnum),
        words("definition in"), fixed(foreign_language_string(Lang)),
        words("for"), unqual_type_ctor(TypeCtor), suffix("."), nl],
    LeastPieces = [words("The original definition is here."), nl],
    Spec = error_spec($pred, severity_error, phase_term_to_parse_tree,
        [simplest_msg(Context, MainPieces),
        simplest_msg(LeastContext, LeastPieces)]),
    !:Specs = [Spec | !.Specs].

:- func get_maybe_type_defns(list(maybe(item_type_defn_info_general(T))))
    = list(item_type_defn_info_general(T)).

get_maybe_type_defns([]) = [].
get_maybe_type_defns([MaybeTypeDefn | MaybeTypeDefns]) = Defns :-
    TailDefns = get_maybe_type_defns(MaybeTypeDefns),
    (
        MaybeTypeDefn = no,
        Defns = TailDefns
    ;
        MaybeTypeDefn = yes(TypeDefn),
        Defns = [TypeDefn | TailDefns]
    ).

:- func get_maybe_type_defn_contexts(list(maybe(item_type_defn_info_foreign)))
    = list(prog_context).

get_maybe_type_defn_contexts([]) = [].
get_maybe_type_defn_contexts([MaybeTypeDefn | MaybeTypeDefns]) = Contexts :-
    TailContexts = get_maybe_type_defn_contexts(MaybeTypeDefns),
    (
        MaybeTypeDefn = no,
        Contexts = TailContexts
    ;
        MaybeTypeDefn = yes(TypeDefn),
        Contexts = [TypeDefn ^ td_context | TailContexts]
    ).

%-----------------------------------------------------------------------------%

    % This type maps a field name to the locations where it occurs.
    %
:- type field_name_map ==
    map(field_name_of_type_ctor, one_or_more(field_name_locn)).

:- type field_name_of_type_ctor
    --->    field_name_of_type_ctor(
                string,
                type_ctor
            ).

    % The info we have about each location where a field name occurs:
    %
    % - the context where it occurs, and
    % - the data constructor in which it occurs.
    %
    % The context is first to make sorting easier. This is relevant
    % because we want to consider the textually first occurrence of
    % a field name to be the valid occurrence, and the later ones
    % as the duplicates.
    %
:- type field_name_locn
    --->    field_name_locn(prog_context, string).

:- pred add_type_ctor_to_field_name_map(
    type_ctor::in, type_ctor_checked_defn::in,
    field_name_map::in, field_name_map::out) is det.

add_type_ctor_to_field_name_map(TypeCtor, CheckedDefn, !FieldNameMap) :-
    (
        CheckedDefn = checked_defn_solver(_)
    ;
        CheckedDefn = checked_defn_std(CheckedStdDefn),
        (
            ( CheckedStdDefn = std_mer_type_eqv(_, _)
            ; CheckedStdDefn = std_mer_type_abstract(_, _, _)
            ; CheckedStdDefn = std_mer_type_du_all_plain_constants(_, _,
                _, _, _)
            )
        ;
            (
                CheckedStdDefn =
                    std_mer_type_du_not_all_plain_constants(_Status,
                        DuDefn, _MaybeDefnCJCs)
            ;
                CheckedStdDefn = std_mer_type_du_subtype(_Status, DuDefn)
            ),
            DetailsDu = DuDefn ^ td_ctor_defn,
            DetailsDu = type_details_du(_MaybeSuperType, OoMCtors,
                _MaybeCanonical, _MaybeDirectArgs),
            OoMCtors = one_or_more(HeadCtor, TailCtors),
            list.foldl(add_data_ctor_to_field_name_map(TypeCtor),
                [HeadCtor | TailCtors], !FieldNameMap)
        )
    ).

:- pred add_data_ctor_to_field_name_map(type_ctor::in, constructor::in,
    field_name_map::in, field_name_map::out) is det.

add_data_ctor_to_field_name_map(TypeCtor, Ctor, !FieldNameMap) :-
    Ctor = ctor(_Ordinal, _MaybeExist, CtorSymName, CtorArgs,
        _NumArgs, _CtorContext),
    CtorName = unqualify_name(CtorSymName),
    list.foldl(add_data_ctor_arg_to_field_name_map(TypeCtor, CtorName),
        CtorArgs, !FieldNameMap).

:- pred add_data_ctor_arg_to_field_name_map(type_ctor::in, string::in,
    constructor_arg::in, field_name_map::in, field_name_map::out) is det.

add_data_ctor_arg_to_field_name_map(TypeCtor, CtorName, CtorArg,
        !FieldNameMap) :-
    CtorArg = ctor_arg(MaybeCtorFieldName, _Type, _ArgContext),
    (
        MaybeCtorFieldName = no
    ;
        MaybeCtorFieldName = yes(CtorFieldName),
        CtorFieldName = ctor_field_name(FieldSymName, FieldNameContext),
        FieldName = unqualify_name(FieldSymName),
        FieldNameTypeCtor = field_name_of_type_ctor(FieldName, TypeCtor),
        FNLocn = field_name_locn(FieldNameContext, CtorName),
        ( if map.search(!.FieldNameMap, FieldNameTypeCtor, OoMFNLocns0) then
            OoMFNLocns0 = one_or_more(HeadFNLocn, TailFNLocns),
            OoMFNLocns = one_or_more(FNLocn, [HeadFNLocn | TailFNLocns]),
            map.det_update(FieldNameTypeCtor, OoMFNLocns, !FieldNameMap)
        else
            OoMFNLocns = one_or_more(FNLocn, []),
            map.det_insert(FieldNameTypeCtor, OoMFNLocns, !FieldNameMap)
        )
    ).

:- pred report_any_duplicate_field_names(field_name_of_type_ctor::in,
    one_or_more(field_name_locn)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_any_duplicate_field_names(FieldNameTypeCtor, OoMFNLocns, !Specs) :-
    FNLocns = one_or_more_to_list(OoMFNLocns),
    list.sort(FNLocns, SortedFNLocns),
    (
        SortedFNLocns = [],
        unexpected($pred, "SortedFNLocns = []")
    ;
        SortedFNLocns = [_]
        % The expected case; FieldName is defined exactly once.
    ;
        SortedFNLocns = [HeadFNLocn | TailFNLocns],
        TailFNLocns = [_ | _],
        % The case we are looking for; FieldName is defined *more* than once
        % in the same type.
        list.foldl(report_duplicate_field_name(FieldNameTypeCtor, HeadFNLocn),
            TailFNLocns, !Specs)
    ).

:- pred report_duplicate_field_name(field_name_of_type_ctor::in,
    field_name_locn::in, field_name_locn::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_duplicate_field_name(FieldNameTypeCtor, FirstFNLocn, FNLocn, !Specs) :-
    FieldNameTypeCtor = field_name_of_type_ctor(FieldName, TypeCtor),
    FirstFNLocn = field_name_locn(FirstContext, FirstCtorName),
    FNLocn = field_name_locn(Context, CtorName),
    InitPieces = [words("Error: duplicate occurrence of the field name"),
        quote(FieldName)],
    ( if CtorName = FirstCtorName then
        MainPieces = InitPieces ++ [words("in the function symbol"),
            quote(CtorName), suffix("."), nl]
    else
        MainPieces = InitPieces ++ [words("in the definition of"),
            unqual_type_ctor(TypeCtor), suffix("."), nl]
    ),
    FirstPieces = [words("The first occurrence of this field name"),
        words("is here."), nl],
    Spec = error_spec($pred, severity_warning, phase_term_to_parse_tree,
        [simplest_msg(Context, MainPieces),
        simplest_msg(FirstContext, FirstPieces)]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%
:- end_module parse_tree.check_parse_tree_type_defns.
%-----------------------------------------------------------------------------%
