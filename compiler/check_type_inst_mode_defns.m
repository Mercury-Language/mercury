%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% Copyright (C) 2019-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: check_type_inst_mode_defns.m.
% Main author: zs.
%
% This module checks whether the type definition and foreign enum items
% in a module represent a valid set of type definitions.
%
%---------------------------------------------------------------------------%

:- module parse_tree.check_type_inst_mode_defns.
:- interface.

:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

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

:- pred create_inst_ctor_checked_map(maybe_insist_on_defn::in,
    inst_ctor_defn_map::in, inst_ctor_defn_map::in,
    inst_ctor_checked_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred create_mode_ctor_checked_map(maybe_insist_on_defn::in,
    mode_ctor_defn_map::in, mode_ctor_defn_map::in,
    mode_ctor_checked_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.item_util.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_foreign_enum.
:- import_module parse_tree.prog_type.

:- import_module bimap.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module term.

%---------------------------------------------------------------------------%

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
    % accident) than others, which are more likely to be mistakes. For example,
    % cut-and-paste errors may yield foreign type definitions that specify
    % the wrong type name or the wrong foreign language.

    % The first stage is to look for and report inconsistencies that manifest
    % themselves as violations of the "at most one definition of TypeCtor
    % of a given kind in a given module section" rule.
    check_any_type_ctor_defns_for_duplicates(IntTypeDefnMap, TypeCtor,
        IntMaybeDefn, !Specs),
    check_any_type_ctor_defns_for_duplicates(ImpTypeDefnMap, TypeCtor,
        ImpMaybeDefn, !Specs),
    check_any_type_ctor_enums_for_duplicates(ImpForeignEnumMap, TypeCtor,
        ImpMaybeEnumCJCs, ImpLeftOverEnumsCJCs, !Specs),

    ( if map.search(ImpForeignEnumMap, TypeCtor, ImpEnumsCJCs) then
        ImpEnumsCJCs = c_java_csharp(ImpEnumsC, ImpEnumsJava, ImpEnumsCsharp),
        ImpEnums = ImpEnumsC ++ ImpEnumsJava ++ ImpEnumsCsharp
    else
        ImpEnums = []
    ),

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

    % Get the contexts of each different definition in case we later
    % need to generate error messages for them. This is not very efficient
    % in terms of runtime, but it keeps the later code sane.
    % XXX CLEANUP It should be possible to pass values of type
    % maybe(item_type_defn_info_general(T)) to any of the predicates
    % that currently take a maybe(prog_context), and get *them* to call
    % get_maybe_context as needed.
    IntContextAbstractSolver = get_maybe_context(IntAbstractSolverMaybeDefn),
    % IntContextSolver       = get_maybe_context(IntSolverMaybeDefn),
    IntContextAbstractStd    = get_maybe_context(IntAbstractStdMaybeDefn),
    IntContextEqv            = get_maybe_context(IntEqvMaybeDefn),
    IntContextDu             = get_maybe_context(IntDuMaybeDefn),
    ImpContextAbstractSolver = get_maybe_context(ImpAbstractSolverMaybeDefn),
    % ImpContextSolver       = get_maybe_context(ImpSolverMaybeDefn),
    ImpContextAbstractStd    = get_maybe_context(ImpAbstractStdMaybeDefn),
    ImpContextEqv            = get_maybe_context(ImpEqvMaybeDefn),
    ImpContextDu             = get_maybe_context(ImpDuMaybeDefn),
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

    % Note that having both an abstract and a nonabstract definition
    % in the interface section is ok. We sometimes put an abstract declaration
    % in one interface section, which we publicize, and a non-abstract
    % definition in another interface section, which we don't publicize.

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
        SrcDefns = src_defns_solver(
            map_maybe(wrap_abstract_type_defn, IntAbstractSolverMaybeDefn),
            yes(wrap_solver_type_defn(ImpSolverDefn))),
        CheckedDefn = checked_defn_solver(CheckedSolverDefn, SrcDefns),
        map.det_insert(TypeCtor, CheckedDefn, !TypeCtorCheckedMap)
    else if
        % TypeCtor is NOT defined as a solver type.
        % Is there an equivalence type definition ...
        ( if
            % ... in the interface?
            IntEqvMaybeDefn = yes(IntEqvDefn)
        then
            % XXX CLEANUP Include ImpEqvMaybeDefn
            % in report_any_incompatible_type_definition
            EqvDefn = IntEqvDefn,
            EqvWhere = "interface",
            Status = std_eqv_type_mer_exported,
            SrcDefns = src_defns_std([wrap_eqv_type_defn(IntEqvDefn)], [], [])
        else if
            % ... in the implementation?
            ImpEqvMaybeDefn = yes(ImpEqvDefn)
        then
            EqvDefn = ImpEqvDefn,
            EqvWhere = "implementation",
            (
                IntAbstractStdMaybeDefn = yes(AbstractStdDefn),
                Status = std_eqv_type_abstract_exported,
                SrcDefnsInt = [wrap_abstract_type_defn(AbstractStdDefn)]
            ;
                IntAbstractStdMaybeDefn = no,
                Status = std_eqv_type_all_private,
                SrcDefnsInt = []
            ),
            SrcDefnsImp = [wrap_eqv_type_defn(ImpEqvDefn)],
            SrcDefns = src_defns_std(SrcDefnsInt, SrcDefnsImp, [])
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
        CheckedDefn = checked_defn_std(CheckedStdDefn, SrcDefns),
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

        % XXX TYPE_REPN As part of switching over to this new system,
        % we will have to disable users' ability to specify MaybeDirectArgs
        % in source code.
        DetailsDu = DuDefn ^ td_ctor_defn,
        DetailsDu = type_details_du(MaybeSubType, OoMCtors, _MaybeCanonical,
            _MaybeDirectArgs),
        OoMCtors = one_or_more(HeadCtor, TailCtors),

        (
            MaybeSubType = not_a_subtype,
            % The status of TypeCtor depends not just on which section
            % the du definition was in and on the presence or absence
            % of an abstact declaration in the interface, but also on
            % which section any foreign language definitions are found in.
            %
            % All foreign language definitions must be in the same section.
            % If they occur in both sections, we pick one, and report the
            % definitions in the other section as errors.
            decide_du_foreign_type_section(TypeCtor, DuDefn, DuSection,
                IntAbstractStdMaybeDefn, IntMaybeDefnCJCs, ImpMaybeDefnCJCs,
                Status, ChosenSectionCJCs, ChosenMaybeDefnCJCs,
                SrcDefnsDuInt, SrcDefnsDuImp, !Specs),
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
                        ChosenMaybeDefnCJCs),
                ChosenMaybeDefnCJCs = c_java_csharp(ChosenMaybeDefnC,
                    ChosenMaybeDefnJava, ChosenMaybeDefnCsharp),
                SrcForeignDefns = get_maybe_type_defns([ChosenMaybeDefnC,
                    ChosenMaybeDefnJava, ChosenMaybeDefnCsharp]),
                SrcForeignEnums = []
            ;
                MaybeOnlyConstants = only_plain_constants(HeadName, TailNames),
                decide_du_repn_foreign_only_constants(TypeCtor,
                    [HeadName | TailNames], ChosenMaybeDefnCJCs,
                    ImpMaybeEnumCJCs, ImpLeftOverEnumsCJCs,
                    MaybeDefnOrEnumCJCs, SrcForeignDefns, SrcForeignEnums,
                    !Specs),
                CheckedStdDefn = std_mer_type_du_all_plain_constants(Status,
                    DuDefn, HeadName, TailNames, MaybeDefnOrEnumCJCs)
            ),
            (
                ChosenSectionCJCs = ms_interface,
                SrcDefnsInt = SrcDefnsDuInt ++
                    list.map(wrap_foreign_type_defn, SrcForeignDefns),
                SrcDefnsImp = SrcDefnsDuImp
            ;
                ChosenSectionCJCs = ms_implementation,
                SrcDefnsInt = SrcDefnsDuInt,
                SrcDefnsImp = SrcDefnsDuImp ++
                    list.map(wrap_foreign_type_defn, SrcForeignDefns)
            ),
            SrcDefns = src_defns_std(SrcDefnsInt, SrcDefnsImp, SrcForeignEnums)
        ;
            MaybeSubType = subtype_of(_),
            % A subtype's representation is controlled entirely by the
            % representation of its base type. It cannot have its own
            % representation specifications for its constructors.
            ForeignDefns = get_maybe_type_defns([IntMaybeDefnC, ImpMaybeDefnC,
                IntMaybeDefnJava, ImpMaybeDefnJava,
                IntMaybeDefnCsharp, ImpMaybeDefnCsharp]),
            list.foldl(subtype_du_report_any_foreign_type(TypeCtor, DuDefn),
                ForeignDefns, !Specs),
            list.foldl(subtype_du_report_any_foreign_enum(TypeCtor, DuDefn),
                ImpEnums, !Specs),

            decide_subtype_status(TypeCtor, DuDefn, DuSection,
                IntAbstractStdMaybeDefn, Status, SrcDefnsInt, SrcDefnsImp),
            CheckedStdDefn = std_mer_type_du_subtype(Status, DuDefn),
            SrcDefns = src_defns_std(SrcDefnsInt, SrcDefnsImp, [])
        ),

        CheckedDefn = checked_defn_std(CheckedStdDefn, SrcDefns),
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
            Status, ChosenAbstractStdDefn, ChosenMaybeDefnCJCs, SrcDefns,
            !Specs),

        CheckedStdDefn = std_mer_type_abstract(Status,
            ChosenAbstractStdDefn, ChosenMaybeDefnCJCs),
        CheckedDefn = checked_defn_std(CheckedStdDefn, SrcDefns),
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
            Status = abstract_solver_type_exported,
            SrcDefnsInt = yes(wrap_abstract_type_defn(IntAbstractSolverDefn)),
            SrcDefnsImp = no
        else if
            % ... in the implementation?
            ImpAbstractSolverMaybeDefn = yes(ImpAbstractSolverDefn)
        then
            AbstractSolverDefn = ImpAbstractSolverDefn,
            AbstractSolverWhere = "implementation",
            Status = abstract_solver_type_private,
            SrcDefnsInt = no,
            SrcDefnsImp = yes(wrap_abstract_type_defn(ImpAbstractSolverDefn))
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
        SrcDefns = src_defns_solver(SrcDefnsInt, SrcDefnsImp),
        CheckedDefn = checked_defn_solver(CheckedSolverDefn, SrcDefns),
        map.det_insert(TypeCtor, CheckedDefn, !TypeCtorCheckedMap)
    else if
        % TypeCtor is NOT defined as a solver, equivalence, du or foreign type,
        % and it is NOT declared as a solver type.
        % Does it have a declaration as a non-solver type ...
        ( if
            % ... in the interface?
            IntAbstractStdMaybeDefn = yes(IntAbstractStdDefn)
        then
            % If an abstract exported du type is a dummy type, notag type
            % or an enum type, then the compiler will put into the
            % automatically generated .int/.int2 file for its module
            % both a plain abstract type_defn item in the interface section
            % and *another* type_defn item in the implementation section
            % that is also abstract but nevertheless carries more information,
            % such as abstract_type_fits_in_n_bits(N).
            (
                ImpAbstractStdMaybeDefn = yes(ImpAbstractStdDefn),
                AbstractStdDefn = ImpAbstractStdDefn,
                SrcDefnsInt = [wrap_abstract_type_defn(ImpAbstractStdDefn)]
            ;
                ImpAbstractStdMaybeDefn = no,
                AbstractStdDefn = IntAbstractStdDefn,
                SrcDefnsInt = [wrap_abstract_type_defn(IntAbstractStdDefn)]
            ),
            Status = std_abs_type_abstract_exported,
            SrcDefnsImp = []
        else if
            % ... in the implementation?
            ImpAbstractStdMaybeDefn = yes(ImpAbstractStdDefn)
        then
            AbstractStdDefn = ImpAbstractStdDefn,
            Status = std_abs_type_all_private,
            SrcDefnsInt = [],
            SrcDefnsImp = [wrap_abstract_type_defn(ImpAbstractStdDefn)]
        else
            fail
        )
    then
        maybe_report_declared_but_undefined_type(InsistOnDefn, TypeCtor,
            AbstractStdDefn, !Specs),
        list.foldl(
            report_foreign_enum_for_undefined_type(TypeCtor, "undefined"),
            ImpEnums, !Specs),
        MaybeCJCs = c_java_csharp(no, no, no),
        CheckedStdDefn = std_mer_type_abstract(Status,
            AbstractStdDefn, MaybeCJCs),
        SrcDefns = src_defns_std(SrcDefnsInt, SrcDefnsImp, []),
        CheckedDefn = checked_defn_std(CheckedStdDefn, SrcDefns),
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
    std_du_type_status::out, module_section::out, c_j_cs_maybe_defn::out,
    list(item_type_defn_info)::out, list(item_type_defn_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_du_foreign_type_section(TypeCtor, DuDefn, DuSection,
        IntAbstractStdMaybeDefn, IntMaybeDefnCJCs, ImpMaybeDefnCJCs,
        Status, ChosenSectionCJCs, ChosenMaybeDefnCJCs,
        SrcDefnsDuInt, SrcDefnsDuImp, !Specs) :-
    IntMaybeDefnCJCs = c_java_csharp(IntMaybeDefnC, IntMaybeDefnJava,
        IntMaybeDefnCsharp),
    ImpMaybeDefnCJCs = c_java_csharp(ImpMaybeDefnC, ImpMaybeDefnJava,
        ImpMaybeDefnCsharp),
    IntDefnsCJCs = get_maybe_type_defns([IntMaybeDefnC, IntMaybeDefnJava,
        IntMaybeDefnCsharp]),
    ImpDefnsCJCs = get_maybe_type_defns([ImpMaybeDefnC, ImpMaybeDefnJava,
        ImpMaybeDefnCsharp]),
    (
        DuSection = ms_interface,
        (
            IntDefnsCJCs = [_ | _],
            IntCJCsContexts = list.map(get_type_defn_context, IntDefnsCJCs),
            list.sort(IntCJCsContexts, SortedIntCJCsContexts),
            FirstIntContext = list.det_head(SortedIntCJCsContexts),
            list.foldl(
                foreign_int_report_any_foreign_defn_in_imp(TypeCtor,
                    FirstIntContext),
                ImpDefnsCJCs, !Specs),
            Status = std_du_type_mer_ft_exported,
            ChosenSectionCJCs = ms_interface,
            ChosenMaybeDefnCJCs = IntMaybeDefnCJCs,
            SrcDefnsDuInt = [wrap_du_type_defn(DuDefn)],
            SrcDefnsDuImp = []
        ;
            IntDefnsCJCs = [],
            Status = std_du_type_mer_exported,
            ChosenSectionCJCs = ms_implementation,
            ChosenMaybeDefnCJCs = ImpMaybeDefnCJCs,
            SrcDefnsDuInt = [wrap_du_type_defn(DuDefn)],
            SrcDefnsDuImp = list.map(wrap_foreign_type_defn, ImpDefnsCJCs)
        )
    ;
        DuSection = ms_implementation,
        list.foldl(
            du_imp_report_any_foreign_defn_in_int(TypeCtor, DuDefn),
            IntDefnsCJCs, !Specs),
        (
            IntAbstractStdMaybeDefn = yes(IntAbstractStdDefn),
            Status = std_du_type_abstract_exported,
            SrcDefnsDuInt = [wrap_abstract_type_defn(IntAbstractStdDefn)]
        ;
            IntAbstractStdMaybeDefn = no,
            Status = std_du_type_all_private,
            SrcDefnsDuInt = []
        ),
        ChosenSectionCJCs = ms_implementation,
        ChosenMaybeDefnCJCs = ImpMaybeDefnCJCs,
        SrcDefnsDuImp = [wrap_du_type_defn(DuDefn)]
    ).

:- pred decide_subtype_status(type_ctor::in,
    item_type_defn_info_du::in, module_section::in,
    maybe(item_type_defn_info_abstract)::in, std_du_subtype_status::out,
    list(item_type_defn_info)::out, list(item_type_defn_info)::out) is det.

decide_subtype_status(_TypeCtor, DuDefn, DuSection, IntAbstractStdMaybeDefn,
        Status, SrcDefnsInt, SrcDefnsImp) :-
    (
        DuSection = ms_interface,
        Status = std_sub_type_mer_exported,
        SrcDefnsInt = [wrap_du_type_defn(DuDefn)],
        SrcDefnsImp = []
    ;
        DuSection = ms_implementation,
        (
            IntAbstractStdMaybeDefn = yes(IntAbstractStdDefn),
            Status = std_sub_type_abstract_exported,
            SrcDefnsInt = [wrap_abstract_type_defn(IntAbstractStdDefn)]
        ;
            IntAbstractStdMaybeDefn = no,
            Status = std_sub_type_all_private,
            SrcDefnsInt = []
        ),
        SrcDefnsImp = [wrap_du_type_defn(DuDefn)]
    ).

:- pred decide_only_foreign_type_section(type_ctor::in,
    maybe(item_type_defn_info_abstract)::in,
    maybe(item_type_defn_info_abstract)::in,
    c_j_cs_maybe_defn::in, c_j_cs_maybe_defn::in,
    std_abs_type_status::out,
    item_type_defn_info_abstract::out, c_j_cs_maybe_defn::out,
    src_defns_std::out, list(error_spec)::in, list(error_spec)::out) is det.

decide_only_foreign_type_section(TypeCtor,
        IntAbstractStdMaybeDefn, ImpAbstractStdMaybeDefn,
        IntMaybeDefnCJCs, ImpMaybeDefnCJCs,
        Status, AbstractStdDefn, ChosenMaybeDefnCJCs, SrcDefns, !Specs) :-
    IntMaybeDefnCJCs = c_java_csharp(IntMaybeDefnC, IntMaybeDefnJava,
        IntMaybeDefnCsharp),
    ImpMaybeDefnCJCs = c_java_csharp(ImpMaybeDefnC, ImpMaybeDefnJava,
        ImpMaybeDefnCsharp),
    IntDefnsCJCs = get_maybe_type_defns([IntMaybeDefnC, IntMaybeDefnJava,
        IntMaybeDefnCsharp]),
    ImpDefnsCJCs = get_maybe_type_defns([ImpMaybeDefnC, ImpMaybeDefnJava,
        ImpMaybeDefnCsharp]),
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
                ImpDefnsCJCs, !Specs),
            ChosenMaybeDefnCJCs = IntMaybeDefnCJCs,
            SrcDefnsInt = [wrap_abstract_type_defn(IntAbstractStdDefn) |
                list.map(wrap_foreign_type_defn, IntDefnsCJCs)],
            SrcDefnsImp = []
        ;
            SortedIntContexts = [],
            Status = std_abs_type_abstract_exported,
            ChosenMaybeDefnCJCs = ImpMaybeDefnCJCs,
            SrcDefnsInt = [wrap_abstract_type_defn(IntAbstractStdDefn)],
            SrcDefnsImp = list.map(wrap_foreign_type_defn, ImpDefnsCJCs)
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
                IntDefnsCJCs, !Specs)
        ;
            ImpAbstractStdMaybeDefn = no,
            IntImpDefnsCJCs = IntDefnsCJCs ++ ImpDefnsCJCs,
            list.foldl(
                report_any_foreign_type_without_declaration(TypeCtor),
                IntImpDefnsCJCs, !Specs),
            % IntImpDefnsCJCs cannot be empty because our caller calls us
            % only if at least one of the foreign language definitions
            % is yes(_). This *also* means that we will generate at least one
            % error message above.
            FirstDefn = list.det_head(IntImpDefnsCJCs),
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
        ChosenMaybeDefnCJCs = ImpMaybeDefnCJCs,
        SrcDefnsInt = [],
        SrcDefnsImp = [wrap_abstract_type_defn(AbstractStdDefn) |
            list.map(wrap_foreign_type_defn, ImpDefnsCJCs)]
    ),
    SrcDefns = src_defns_std(SrcDefnsInt, SrcDefnsImp, []).

:- pred decide_du_repn_foreign_only_constants(type_ctor::in,
    list(string)::in, c_j_cs_maybe_defn::in,
    c_j_cs_maybe_enum::in, c_j_cs_enums::in,
    c_j_cs_maybe_defn_or_enum::out,
    list(item_type_defn_info_foreign)::out, list(item_foreign_enum_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_du_repn_foreign_only_constants(TypeCtor, CtorNames,
        MaybeDefnCJCs, MaybeEnumCJCs, LeftOverEnumsCJCs,
        MaybeDefnOrEnumCJCs, SrcForeignDefns, SrcForeignEnums, !Specs) :-
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
        MaybeEnumC, LeftOverEnumsC, MaybeDefnOrEnumC,
        SrcForeignDefnsC, SrcForeignEnumsC, !Specs),
    decide_du_repn_foreign_only_constants_lang(TypeCtor,
        CtorNames, CtorNamesSet, MaybeDefnJava,
        MaybeEnumJava, LeftOverEnumsJava, MaybeDefnOrEnumJava,
        SrcForeignDefnsJava, SrcForeignEnumsJava, !Specs),
    decide_du_repn_foreign_only_constants_lang(TypeCtor,
        CtorNames, CtorNamesSet, MaybeDefnCsharp,
        MaybeEnumCsharp, LeftOverEnumsCsharp, MaybeDefnOrEnumCsharp,
        SrcForeignDefnsCsharp, SrcForeignEnumsCsharp, !Specs),

    SrcForeignDefns =
        SrcForeignDefnsC ++ SrcForeignDefnsJava ++ SrcForeignDefnsCsharp,
    SrcForeignEnums =
        SrcForeignEnumsC ++ SrcForeignEnumsJava ++ SrcForeignEnumsCsharp,
    MaybeDefnOrEnumCJCs = c_java_csharp(MaybeDefnOrEnumC, MaybeDefnOrEnumJava,
        MaybeDefnOrEnumCsharp).

:- pred decide_du_repn_foreign_only_constants_lang(type_ctor::in,
    list(string)::in, set_tree234(string)::in,
    maybe(item_type_defn_info_foreign)::in,
    maybe(item_foreign_enum_info)::in, list(item_foreign_enum_info)::in,
    maybe(foreign_type_or_enum)::out,
    list(item_type_defn_info_foreign)::out,
    list(item_foreign_enum_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_du_repn_foreign_only_constants_lang(TypeCtor, CtorNames, CtorNamesSet,
        MaybeDefn, MaybeEnum, LeftOverEnums, MaybeDefnOrEnum,
        SrcForeignDefns, SrcForeignEnums, !Specs) :-
    (
        MaybeEnum = no,
        expect(unify(LeftOverEnums, []), $pred,
            "MaybeEnum = no but LeftOverEnums != []"),
        MaybeCheckedForeignEnum = no
    ;
        MaybeEnum = yes(Enum),
        build_mercury_foreign_enum_map(TypeCtor, CtorNames, CtorNamesSet,
            Enum, HeadMaybeCFE),
        list.map(
            build_mercury_foreign_enum_map(TypeCtor, CtorNames, CtorNamesSet),
            LeftOverEnums, TailMaybeCFEs),
        pick_first_error_free_enum_if_any([HeadMaybeCFE | TailMaybeCFEs],
            MaybeCheckedForeignEnum, CFESpecs),
        !:Specs = CFESpecs ++ !.Specs
    ),
    (
        MaybeDefn = yes(Defn),
        MaybeDefnOrEnum = yes(foreign_type_or_enum_type(Defn)),
        SrcForeignDefns = [Defn],
        SrcForeignEnums = []
    ;
        MaybeDefn = no,
        (
            MaybeCheckedForeignEnum = no,
            MaybeDefnOrEnum = no,
            SrcForeignDefns = [],
            SrcForeignEnums = []
        ;
            MaybeCheckedForeignEnum = yes(CheckedForeignEnum),
            MaybeDefnOrEnum =
                yes(foreign_type_or_enum_enum(CheckedForeignEnum)),
            CheckedForeignEnum = checked_foreign_enum(EnumInfo, _),
            SrcForeignDefns = [],
            SrcForeignEnums = [EnumInfo]
        )
    ).

:- pred pick_first_error_free_enum_if_any(
    list(maybe1(checked_foreign_enum))::in,
    maybe(checked_foreign_enum)::out, list(error_spec)::out) is det.

pick_first_error_free_enum_if_any([], no, []).
pick_first_error_free_enum_if_any([HeadMaybeCFE | TailMaybeCFEs],
        MaybeCFE, Specs) :-
    pick_first_error_free_enum_if_any(TailMaybeCFEs,
        TailMaybeCFE, TailSpecs),
    (
        HeadMaybeCFE = ok1(CFE),
        MaybeCFE = yes(CFE),
        Specs = TailSpecs
    ;
        HeadMaybeCFE = error1(HeadSpecs),
        MaybeCFE = TailMaybeCFE,
        Specs = HeadSpecs ++ TailSpecs
    ).

:- pred build_mercury_foreign_enum_map(type_ctor::in,
    list(string)::in, set_tree234(string)::in,
    item_foreign_enum_info::in,
    maybe1(checked_foreign_enum)::out) is det.

build_mercury_foreign_enum_map(TypeCtor, CtorNames, CtorNamesSet,
        ForeignEnum, MaybeCheckedForeignEnum) :-
    ForeignEnum = item_foreign_enum_info(_Lang, _TypeCtor, MercuryForeignOoM,
        Context, _SeqNum),
    MercuryForeignAL = one_or_more_to_list(MercuryForeignOoM),
    ContextPieces = [words("In"), pragma_decl("foreign_enum"),
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
            ForeignNames),
        (
            ForeignNames = [],
            % There should be exactly one ForeignName for every CtorName.
            unexpected($pred, "enum type with no constructors")
        ;
            ForeignNames = [HeadForeignName | TailForeignNames],
            ForeignNameOoM = one_or_more(HeadForeignName, TailForeignNames)
        ),
        CheckedForeignEnum = checked_foreign_enum(ForeignEnum, ForeignNameOoM),
        MaybeCheckedForeignEnum = ok1(CheckedForeignEnum)
    ;
        Specs = [_ | _],
        MaybeCheckedForeignEnum = error1(Specs)
    ).

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

:- pred subtype_du_report_any_foreign_type(type_ctor::in,
    item_type_defn_info_du::in, item_type_defn_info_foreign::in,
    list(error_spec)::in, list(error_spec)::out) is det.

subtype_du_report_any_foreign_type(TypeCtor, SubTypeDefn, Foreign, !Specs) :-
    ForeignPieces = [words("Error:"),
        unqual_type_ctor(TypeCtor), words("is defined to be a subtype,"),
        words("so there must not be any"),
        pragma_decl("foreign_type"), words("declarations for it."), nl],
    SubTypePieces = [words("That subtype definition is here."), nl],
    Spec = error_spec($pred, severity_warning, phase_term_to_parse_tree,
        [simplest_msg(Foreign ^ td_context, ForeignPieces),
        simplest_msg(SubTypeDefn ^ td_context, SubTypePieces)]),
    !:Specs = [Spec | !.Specs].

:- pred subtype_du_report_any_foreign_enum(type_ctor::in,
    item_type_defn_info_du::in, item_foreign_enum_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

subtype_du_report_any_foreign_enum(TypeCtor, SubTypeDefn, Enum, !Specs) :-
    EnumPieces = [words("Error:"),
        unqual_type_ctor(TypeCtor), words("is defined to be a subtype,"),
        words("so there must not be any"),
        pragma_decl("foreign_enum"), words("declarations for it."), nl],
    SubTypePieces = [words("That subtype definition is here."), nl],
    Spec = error_spec($pred, severity_warning, phase_term_to_parse_tree,
        [simplest_msg(Enum ^ fe_context, EnumPieces),
        simplest_msg(SubTypeDefn ^ td_context, SubTypePieces)]),
    !:Specs = [Spec | !.Specs].

:- pred du_imp_report_any_foreign_defn_in_int(type_ctor::in,
    item_type_defn_info_general(T)::in, item_type_defn_info_foreign::in,
    list(error_spec)::in, list(error_spec)::out) is det.

du_imp_report_any_foreign_defn_in_int(TypeCtor, DuDefn, ForeignDefn, !Specs) :-
    ForeignPieces = [words("Error: since the Mercury definition of"),
        unqual_type_ctor(TypeCtor),
        words("is in the implementation section,"),
        words("any foreign type definitions for it"),
        words("must be in the implementation section as well."), nl],
    DuPieces = [words("That Mercury definition is here."), nl],
    Spec = error_spec($pred, severity_warning, phase_term_to_parse_tree,
        [simplest_msg(ForeignDefn ^ td_context, ForeignPieces),
        simplest_msg(DuDefn ^ td_context, DuPieces)]),
    !:Specs = [Spec | !.Specs].

:- pred report_any_foreign_type_without_declaration(type_ctor::in,
    item_type_defn_info_foreign::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_any_foreign_type_without_declaration(TypeCtor, ForeignDefn, !Specs) :-
    Pieces = [words("Error: a"),
        pragma_decl("foreign_type"), words("declaration for"),
        unqual_type_ctor(TypeCtor), words("without either"),
        words("a Mercury definition or a Mercury declaration for"),
        unqual_type_ctor(TypeCtor), suffix("."), nl],
    Spec = simplest_spec($pred, severity_warning, phase_term_to_parse_tree,
        ForeignDefn ^ td_context, Pieces),
    !:Specs = [Spec | !.Specs].

:- pred foreign_int_report_any_foreign_defn_in_imp(type_ctor::in,
    prog_context::in, item_type_defn_info_foreign::in,
    list(error_spec)::in, list(error_spec)::out) is det.

foreign_int_report_any_foreign_defn_in_imp(TypeCtor, IntForeignContext,
        ImpForeignDefn, !Specs) :-
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
    !:Specs = [Spec | !.Specs].

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
    % XXX CLEANUP Should take item_type_defn_info_general(T1)
    prog_context::in, string::in, string::in,
    % XXX CLEANUP Should take maybe(item_type_defn_info_general(T2))
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
        % Do not report undefined types in builtin modules, since
        % their implementation (such as c_pointer's) may be handwritten.
        not any_mercury_builtin_module(TypeCtorModuleName),
        % Several of the types defined in type_desc.m do not have
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
        TypeDefns = [_, _ | _],
        CompareTypeDefnsByContext =
            ( pred(TDA::in, TDB::in, Cmp::out) is det :-
                compare(Cmp, TDA ^ td_context, TDB ^ td_context)
            ),
        list.sort(CompareTypeDefnsByContext, TypeDefns, SortedTypeDefns),
        list.det_head_tail(SortedTypeDefns, HeadTypeDefn, TailTypeDefns),
        MaybeTypeDefn = yes(HeadTypeDefn),
        list.foldl(
            report_duplicate_type_defn(Kind, TypeCtor, HeadTypeDefn),
            TailTypeDefns, !Specs)
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
    item_type_defn_info_general(T1)::in, item_type_defn_info_general(T2)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_duplicate_type_defn(Kind, TypeCtor, OrigTypeDefn, TypeDefn, !Specs) :-
    MainPieces = [words("Error: duplicate"), words(Kind),
        words("definition for"), unqual_type_ctor(TypeCtor), suffix("."), nl],
    LeastPieces = [words("The original definition is here."), nl],
    Spec = error_spec($pred, severity_error, phase_term_to_parse_tree,
        [simplest_msg(TypeDefn ^ td_context, MainPieces),
        simplest_msg(OrigTypeDefn ^ td_context, LeastPieces)]),
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

:- func get_type_defn_context(item_type_defn_info_general(T)) = prog_context.

get_type_defn_context(TypeDefn) = TypeDefn ^ td_context.

%---------------------------------------------------------------------------%

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
        CheckedDefn = checked_defn_solver(_, _)
    ;
        CheckedDefn = checked_defn_std(CheckedStdDefn, _),
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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

create_inst_ctor_checked_map(InsistOnDefn, IntInstDefnMap, ImpInstDefnMap,
        CheckedMap, !Specs) :-
    map.keys_as_set(IntInstDefnMap, IntDefnInstCtors),
    map.keys_as_set(ImpInstDefnMap, ImpDefnInstCtors),
    % This union operation depends on the type_ctors in all four maps
    % being qualified exactly the same way. We could require the type_ctor keys
    % to be all fully qualified or all fully unqualified; we chose the former.
    InstCtors =
        set.to_sorted_list(set.union(IntDefnInstCtors, ImpDefnInstCtors)),
    list.foldl2(
        check_inst_ctor_defns(InsistOnDefn, IntInstDefnMap, ImpInstDefnMap),
        InstCtors, map.init, CheckedMap, !Specs).

:- pred check_inst_ctor_defns(maybe_insist_on_defn::in,
    inst_ctor_defn_map::in, inst_ctor_defn_map::in, inst_ctor::in,
    inst_ctor_checked_map::in, inst_ctor_checked_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_inst_ctor_defns(InsistOnDefn, IntInstDefnMap, ImpInstDefnMap, InstCtor,
        !CheckedMap, !Specs) :-
    check_any_inst_ctor_defns_for_duplicates(IntInstDefnMap, InstCtor,
        IntMaybeAbstractDefn, IntMaybeEqvDefn, !Specs),
    check_any_inst_ctor_defns_for_duplicates(ImpInstDefnMap, InstCtor,
        ImpMaybeAbstractDefn, ImpMaybeEqvDefn, !Specs),
    ( if
        % Does InstCtor have a non-abstract definition ...
        ( if
            % ... in the interface?
            IntMaybeEqvDefn = yes(IntEqvDefn)
        then
            EqvDefn = IntEqvDefn,
            EqvWhere = "interface",
            Status = std_inst_exported,
            SrcDefnsInt = yes(wrap_eqv_inst_defn(IntEqvDefn)),
            SrcDefns = src_defns_inst(SrcDefnsInt, no)
        else if
            % ... in the implementation?
            ImpMaybeEqvDefn = yes(ImpEqvDefn)
        then
            EqvDefn = ImpEqvDefn,
            EqvWhere = "implementation",
            (
                IntMaybeAbstractDefn = yes(IntAbstractDefn),
                Status = std_inst_abstract_exported,
                SrcDefnsInt = yes(wrap_abstract_inst_defn(IntAbstractDefn))
            ;
                IntMaybeAbstractDefn = no,
                Status = std_inst_all_private,
                SrcDefnsInt = no
            ),
            SrcDefnsImp = yes(wrap_eqv_inst_defn(ImpEqvDefn)),
            SrcDefns = src_defns_inst(SrcDefnsInt, SrcDefnsImp)
        else
            fail
        )
    then
        % Having an IntAbstractDefn, as well an IntEqvDefn, is ok.
        % We sometimes put an abstract declaration in one interface section,
        % which we publicize, and a non-abstract definition in another
        % interface section, which we don't publicize.
        report_any_redundant_abstract_inst_in_imp(InstCtor,
            "definition", EqvWhere, ImpMaybeAbstractDefn, !Specs),
        StdDefn = std_inst_defn(Status, wrap_eqv_inst_defn(EqvDefn)),
        CheckedDefn = checked_defn_inst(StdDefn, SrcDefns),
        map.det_insert(InstCtor, CheckedDefn, !CheckedMap)
    else
        % We get called only on InstCtors for which we have
        % at least one definition. Since we don't have any equivalence
        % definitions, we must have at least one abstract declaration.
        (
            IntMaybeAbstractDefn = yes(IntAbstractDefn),
            report_any_redundant_abstract_inst_in_imp(InstCtor,
                "declaration", "interface", ImpMaybeAbstractDefn, !Specs),
            (
                InsistOnDefn = do_not_insist_on_defn,
                Status = std_inst_abstract_exported,
                StdDefn = std_inst_defn(Status,
                    wrap_abstract_inst_defn(IntAbstractDefn)),
                IntDefn = wrap_abstract_inst_defn(IntAbstractDefn),
                SrcDefns = src_defns_inst(yes(IntDefn), no),
                CheckedDefn = checked_defn_inst(StdDefn, SrcDefns),
                map.det_insert(InstCtor, CheckedDefn, !CheckedMap)
            ;
                InsistOnDefn = do_insist_on_defn,
                report_declared_but_undefined_inst(InstCtor, IntAbstractDefn,
                    !Specs)
            )
        ;
            IntMaybeAbstractDefn = no,
            (
                ImpMaybeAbstractDefn = yes(ImpAbstractDefn),
                (
                    InsistOnDefn = do_not_insist_on_defn,
                    Status = std_inst_all_private,
                    StdDefn = std_inst_defn(Status,
                        wrap_abstract_inst_defn(ImpAbstractDefn)),
                    ImpDefn = wrap_abstract_inst_defn(ImpAbstractDefn),
                    SrcDefns = src_defns_inst(no, yes(ImpDefn)),
                    CheckedDefn = checked_defn_inst(StdDefn, SrcDefns),
                    map.det_insert(InstCtor, CheckedDefn, !CheckedMap)
                ;
                    InsistOnDefn = do_insist_on_defn,
                    report_declared_but_undefined_inst(InstCtor,
                        ImpAbstractDefn, !Specs)
                )
            ;
                ImpMaybeAbstractDefn = no,
                unexpected($pred, "no defns at all")
            )
        )
    ).

:- pred check_any_inst_ctor_defns_for_duplicates(inst_ctor_defn_map::in,
    inst_ctor::in, maybe(item_inst_defn_info_abstract)::out,
    maybe(item_inst_defn_info_eqv)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_any_inst_ctor_defns_for_duplicates(InstDefnMap, InstCtor,
        AbstractMaybeDefn, EqvMaybeDefn, !Specs) :-
    ( if map.search(InstDefnMap, InstCtor, AllDefns) then
        AllDefns = inst_ctor_all_defns(AbstractDefns, EqvDefns),
        at_most_one_inst_defn("abstract inst", InstCtor,
            AbstractDefns, AbstractMaybeDefn, !Specs),
        at_most_one_inst_defn("inst", InstCtor,
            EqvDefns, EqvMaybeDefn, !Specs)
    else
        AbstractMaybeDefn = no,
        EqvMaybeDefn = no
    ).

:- pred at_most_one_inst_defn(string::in, inst_ctor::in,
    list(item_inst_defn_info_general(T))::in,
    maybe(item_inst_defn_info_general(T))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

at_most_one_inst_defn(Kind, InstCtor, InstDefns, MaybeInstDefn, !Specs) :-
    (
        InstDefns = [],
        MaybeInstDefn = no
    ;
        InstDefns = [InstDefn],
        MaybeInstDefn = yes(InstDefn)
    ;
        InstDefns = [_, _ | _],
        CompareInstDefnsByContext =
            ( pred(IDA::in, IDB::in, Cmp::out) is det :-
                compare(Cmp, IDA ^ id_context, IDB ^ id_context)
            ),
        list.sort(CompareInstDefnsByContext, InstDefns, SortedInstDefns),
        list.det_head_tail(SortedInstDefns, HeadInstDefn, TailInstDefns),
        MaybeInstDefn = yes(HeadInstDefn),
        list.foldl(
            report_duplicate_inst_defn(Kind, InstCtor, HeadInstDefn),
            TailInstDefns, !Specs)
    ).

:- pred report_duplicate_inst_defn(string::in, inst_ctor::in,
    item_inst_defn_info_general(T1)::in, item_inst_defn_info_general(T2)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_duplicate_inst_defn(Kind, InstCtor, OrigInstDefn, InstDefn, !Specs) :-
    MainPieces = [words("Error: duplicate"), words(Kind),
        words("definition for"), unqual_inst_ctor(InstCtor), suffix("."), nl],
    LeastPieces = [words("The original definition is here."), nl],
    Spec = error_spec($pred, severity_error, phase_term_to_parse_tree,
        [simplest_msg(InstDefn ^ id_context, MainPieces),
        simplest_msg(OrigInstDefn ^ id_context, LeastPieces)]),
    !:Specs = [Spec | !.Specs].

:- pred report_any_redundant_abstract_inst_in_imp(inst_ctor::in,
    string::in, string::in,
    maybe(item_inst_defn_info_abstract)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_any_redundant_abstract_inst_in_imp(TypeCtor, DeclOrDefn, Section,
        MaybeImpAbstractDefn, !Specs) :-
    (
        MaybeImpAbstractDefn = no
    ;
        MaybeImpAbstractDefn = yes(ImpAbstractDefn),
        Pieces = [words("Error: this declaration of"),
            unqual_inst_ctor(TypeCtor), words("is redundant,"),
            words("since the inst has a"), words(DeclOrDefn),
            words("in the"), words(Section), words("section."), nl],
        Spec = simplest_spec($pred, severity_warning, phase_term_to_parse_tree,
            ImpAbstractDefn ^ id_context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred report_any_incompatible_inst_definition(inst_ctor::in,
    item_inst_defn_info_general(T1)::in, string::in, string::in,
    maybe(item_inst_defn_info_general(T2))::in,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma consider_used(pred(report_any_incompatible_inst_definition/7)).

report_any_incompatible_inst_definition(InstCtor, OrigInstDefn, Kind, Section,
        MaybeInstDefn, !Specs) :-
    (
        MaybeInstDefn = no
    ;
        MaybeInstDefn = yes(InstDefn),
        MainPieces = [words("Error: this definition of"),
            unqual_inst_ctor(InstCtor), words("is incompatible"),
            words("with the"), words(Kind), words("definition"),
            words("in the"), words(Section), words("section."), nl],
        UsedPieces = [words("That definition is here."), nl],
        Spec = error_spec($pred, severity_warning, phase_term_to_parse_tree,
            [simplest_msg(InstDefn ^ id_context, MainPieces),
            simplest_msg(OrigInstDefn ^ id_context, UsedPieces)]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred report_declared_but_undefined_inst(inst_ctor::in,
    item_inst_defn_info_abstract::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_declared_but_undefined_inst(InstCtor, AbsInstDefn, !Specs) :-
    Pieces = [words("Error: the inst"), unqual_inst_ctor(InstCtor),
        words("has this declaration, but it has no definition."), nl],
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        AbsInstDefn ^ id_context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

create_mode_ctor_checked_map(InsistOnDefn, IntModeDefnMap, ImpModeDefnMap,
        CheckedMap, !Specs) :-
    map.keys_as_set(IntModeDefnMap, IntDefnModeCtors),
    map.keys_as_set(ImpModeDefnMap, ImpDefnModeCtors),
    % This union operation depends on the type_ctors in all four maps
    % being qualified exactly the same way. We could require the type_ctor keys
    % to be all fully qualified or all fully unqualified; we chose the former.
    ModeCtors =
        set.to_sorted_list(set.union(IntDefnModeCtors, ImpDefnModeCtors)),
    list.foldl2(
        check_mode_ctor_defns(InsistOnDefn, IntModeDefnMap, ImpModeDefnMap),
        ModeCtors, map.init, CheckedMap, !Specs).

:- pred check_mode_ctor_defns(maybe_insist_on_defn::in,
    mode_ctor_defn_map::in, mode_ctor_defn_map::in, mode_ctor::in,
    mode_ctor_checked_map::in, mode_ctor_checked_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_mode_ctor_defns(InsistOnDefn, IntModeDefnMap, ImpModeDefnMap, ModeCtor,
        !CheckedMap, !Specs) :-
    check_any_mode_ctor_defns_for_duplicates(IntModeDefnMap, ModeCtor,
        IntMaybeAbstractDefn, IntMaybeEqvDefn, !Specs),
    check_any_mode_ctor_defns_for_duplicates(ImpModeDefnMap, ModeCtor,
        ImpMaybeAbstractDefn, ImpMaybeEqvDefn, !Specs),
    ( if
        % Does ModeCtor have a non-abstract definition ...
        ( if
            % ... in the interface?
            IntMaybeEqvDefn = yes(IntEqvDefn)
        then
            EqvDefn = IntEqvDefn,
            EqvWhere = "interface",
            Status = std_mode_exported,
            SrcDefnsInt = yes(wrap_eqv_mode_defn(IntEqvDefn)),
            SrcDefns = src_defns_mode(SrcDefnsInt, no)
        else if
            % ... in the implementation?
            ImpMaybeEqvDefn = yes(ImpEqvDefn)
        then
            EqvDefn = ImpEqvDefn,
            EqvWhere = "implementation",
            (
                IntMaybeAbstractDefn = yes(IntAbstractDefn),
                Status = std_mode_abstract_exported,
                SrcDefnsInt = yes(wrap_abstract_mode_defn(IntAbstractDefn))
            ;
                IntMaybeAbstractDefn = no,
                Status = std_mode_all_private,
                SrcDefnsInt = no
            ),
            SrcDefnsImp = yes(wrap_eqv_mode_defn(ImpEqvDefn)),
            SrcDefns = src_defns_mode(SrcDefnsInt, SrcDefnsImp)
        else
            fail
        )
    then
        % Having an IntAbstractDefn, as well an IntEqvDefn, is ok.
        % We sometimes put an abstract declaration in one interface section,
        % which we publicize, and a non-abstract definition in another
        % interface section, which we don't publicize.
        report_any_redundant_abstract_mode_in_imp(ModeCtor,
            "definition", EqvWhere, ImpMaybeAbstractDefn, !Specs),
        StdDefn = std_mode_defn(Status, wrap_eqv_mode_defn(EqvDefn)),
        CheckedDefn = checked_defn_mode(StdDefn, SrcDefns),
        map.det_insert(ModeCtor, CheckedDefn, !CheckedMap)
    else
        % We get called only on ModeCtors for which we have
        % at least one definition. Since we don't have any equivalence
        % definitions, we must have at least one abstract declaration.
        (
            IntMaybeAbstractDefn = yes(IntAbstractDefn),
            report_any_redundant_abstract_mode_in_imp(ModeCtor,
                "declaration", "interface", ImpMaybeAbstractDefn, !Specs),
            (
                InsistOnDefn = do_not_insist_on_defn,
                Status = std_mode_abstract_exported,
                StdDefn = std_mode_defn(Status,
                    wrap_abstract_mode_defn(IntAbstractDefn)),
                IntDefn = wrap_abstract_mode_defn(IntAbstractDefn),
                SrcDefns = src_defns_mode(yes(IntDefn), no),
                CheckedDefn = checked_defn_mode(StdDefn, SrcDefns),
                map.det_insert(ModeCtor, CheckedDefn, !CheckedMap)
            ;
                InsistOnDefn = do_insist_on_defn,
                report_declared_but_undefined_mode(ModeCtor, IntAbstractDefn,
                    !Specs)
            )
        ;
            IntMaybeAbstractDefn = no,
            (
                ImpMaybeAbstractDefn = yes(ImpAbstractDefn),
                (
                    InsistOnDefn = do_not_insist_on_defn,
                    Status = std_mode_all_private,
                    StdDefn = std_mode_defn(Status,
                        wrap_abstract_mode_defn(ImpAbstractDefn)),
                    ImpDefn = wrap_abstract_mode_defn(ImpAbstractDefn),
                    SrcDefns = src_defns_mode(no, yes(ImpDefn)),
                    CheckedDefn = checked_defn_mode(StdDefn, SrcDefns),
                    map.det_insert(ModeCtor, CheckedDefn, !CheckedMap)
                ;
                    InsistOnDefn = do_insist_on_defn,
                    report_declared_but_undefined_mode(ModeCtor,
                        ImpAbstractDefn, !Specs)
                )
            ;
                ImpMaybeAbstractDefn = no,
                unexpected($pred, "no defns at all")
            )
        )
    ).

:- pred check_any_mode_ctor_defns_for_duplicates(mode_ctor_defn_map::in,
    mode_ctor::in, maybe(item_mode_defn_info_abstract)::out,
    maybe(item_mode_defn_info_eqv)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_any_mode_ctor_defns_for_duplicates(ModeDefnMap, ModeCtor,
        AbstractMaybeDefn, EqvMaybeDefn, !Specs) :-
    ( if map.search(ModeDefnMap, ModeCtor, AllDefns) then
        AllDefns = mode_ctor_all_defns(AbstractDefns, EqvDefns),
        at_most_one_mode_defn("abstract mode", ModeCtor,
            AbstractDefns, AbstractMaybeDefn, !Specs),
        at_most_one_mode_defn("mode", ModeCtor,
            EqvDefns, EqvMaybeDefn, !Specs)
    else
        AbstractMaybeDefn = no,
        EqvMaybeDefn = no
    ).

:- pred at_most_one_mode_defn(string::in, mode_ctor::in,
    list(item_mode_defn_info_general(T))::in,
    maybe(item_mode_defn_info_general(T))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

at_most_one_mode_defn(Kind, ModeCtor, ModeDefns, MaybeModeDefn, !Specs) :-
    (
        ModeDefns = [],
        MaybeModeDefn = no
    ;
        ModeDefns = [ModeDefn],
        MaybeModeDefn = yes(ModeDefn)
    ;
        ModeDefns = [_, _ | _],
        CompareModeDefnsByContext =
            ( pred(IDA::in, IDB::in, Cmp::out) is det :-
                compare(Cmp, IDA ^ md_context, IDB ^ md_context)
            ),
        list.sort(CompareModeDefnsByContext, ModeDefns, SortedModeDefns),
        list.det_head_tail(SortedModeDefns, HeadModeDefn, TailModeDefns),
        MaybeModeDefn = yes(HeadModeDefn),
        list.foldl(
            report_duplicate_mode_defn(Kind, ModeCtor, HeadModeDefn),
            TailModeDefns, !Specs)
    ).

:- pred report_duplicate_mode_defn(string::in, mode_ctor::in,
    item_mode_defn_info_general(T1)::in, item_mode_defn_info_general(T2)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_duplicate_mode_defn(Kind, ModeCtor, OrigModeDefn, ModeDefn, !Specs) :-
    MainPieces = [words("Error: duplicate"), words(Kind),
        words("definition for"), unqual_mode_ctor(ModeCtor), suffix("."), nl],
    LeastPieces = [words("The original definition is here."), nl],
    Spec = error_spec($pred, severity_error, phase_term_to_parse_tree,
        [simplest_msg(ModeDefn ^ md_context, MainPieces),
        simplest_msg(OrigModeDefn ^ md_context, LeastPieces)]),
    !:Specs = [Spec | !.Specs].

:- pred report_any_redundant_abstract_mode_in_imp(mode_ctor::in,
    string::in, string::in,
    maybe(item_mode_defn_info_abstract)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_any_redundant_abstract_mode_in_imp(TypeCtor, DeclOrDefn, Section,
        MaybeImpAbstractDefn, !Specs) :-
    (
        MaybeImpAbstractDefn = no
    ;
        MaybeImpAbstractDefn = yes(ImpAbstractDefn),
        Pieces = [words("Error: this declaration of"),
            unqual_mode_ctor(TypeCtor), words("is redundant,"),
            words("since the mode has a"), words(DeclOrDefn),
            words("in the"), words(Section), words("section."), nl],
        Spec = simplest_spec($pred, severity_warning, phase_term_to_parse_tree,
            ImpAbstractDefn ^ md_context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred report_any_incompatible_mode_definition(mode_ctor::in,
    item_mode_defn_info_general(T1)::in, string::in, string::in,
    maybe(item_mode_defn_info_general(T2))::in,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma consider_used(pred(report_any_incompatible_mode_definition/7)).

report_any_incompatible_mode_definition(ModeCtor, OrigModeDefn, Kind, Section,
        MaybeModeDefn, !Specs) :-
    (
        MaybeModeDefn = no
    ;
        MaybeModeDefn = yes(ModeDefn),
        MainPieces = [words("Error: this definition of"),
            unqual_mode_ctor(ModeCtor), words("is incompatible"),
            words("with the"), words(Kind), words("definition"),
            words("in the"), words(Section), words("section."), nl],
        UsedPieces = [words("That definition is here."), nl],
        Spec = error_spec($pred, severity_warning, phase_term_to_parse_tree,
            [simplest_msg(ModeDefn ^ md_context, MainPieces),
            simplest_msg(OrigModeDefn ^ md_context, UsedPieces)]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred report_declared_but_undefined_mode(mode_ctor::in,
    item_mode_defn_info_abstract::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_declared_but_undefined_mode(ModeCtor, AbsModeDefn, !Specs) :-
    Pieces = [words("Error: the mode"), unqual_mode_ctor(ModeCtor),
        words("has this declaration, but it has no definition."), nl],
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        AbsModeDefn ^ md_context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%
:- end_module parse_tree.check_type_inst_mode_defns.
%---------------------------------------------------------------------------%
