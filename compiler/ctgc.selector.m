%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006,2008-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ctgc.selector.m.
% Main author: nancy.
%
% Definition of predicates and functions for the manipulation of selectors.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.selector.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % An exception of this type is thrown a procedure would need to know the
    % type of a existentially typed node to proceed.
    %
:- type encounter_existential_subtype
    --->    encounter_existential_subtype.

:- type normalization
    --->    need_normalization
    ;       already_normalized.

    % Create a selector as either the top selector, a term selector,
    % or a type selector.
    %
:- func top_selector = selector.
:- func selector_init(cons_id, int) = selector.
:- func selector_init_from_list(list(mer_type)) = selector.

    % Perform a termshift operation.
    %
:- pred selector_termshift(selector::in, selector::in, selector::out) is det.

    % selector_subsumed_by(ModuleInfo, Normalization,
    %   Selector1, Selector2, Type, Extension).
    %
    % Succeeds iff Selector1 is subsumed by Selector2. This means that
    % Selector2 is more general than Selector1, hence, there exists an
    % Extension, such that Selector2.Extension = Selector1.
    %
    % The type specifies the type of the term to which the selectors refer.
    %
:- pred selector_subsumed_by(module_info::in, normalization::in,
    selector::in, selector::in, mer_type::in, selector::out) is semidet.

    % Using the type information of the variable to which the given selector
    % belongs, normalize that selector.
    % S2 is the normalized form of S1 iff:
    %   * type of the node selected by S2 = type of the node selected by S1.
    %   * a path of selectors can be constructed which leads from S2 to S1.
    %
:- pred normalize_selector_with_type_information(module_info::in, mer_type::in,
    selector::in, selector::out) is det.

    % As above but fail if the subtype would be an existential type instead of
    % aborting.
    %
:- pred type_of_node(module_info::in, mer_type::in, selector::in,
    mer_type::out) is semidet.

    % Abbreviate a selector to the type of the node it selects.
    %
:- pred selector_apply_widening(module_info::in, mer_type::in,
    selector::in, selector::out) is det.

    % Reset memoisation tables used by this module.
    %
:- pred reset_tables(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module exception.
:- import_module map.
:- import_module pair.
:- import_module queue.
:- import_module require.
:- import_module set.
:- import_module solutions.

%-----------------------------------------------------------------------------%

top_selector = [].

selector_init(ConsId, Index) = [TermSel] :-
    (
        ( ConsId = cons(_, _, _)
        ; ConsId = tuple_cons(_)
        ),
        TermSel = termsel(ConsId, Index)
    ;
        ( ConsId = closure_cons(_, _)
        ; ConsId = int_const(_)
        ; ConsId = uint_const(_)
        ; ConsId = int8_const(_)
        ; ConsId = uint8_const(_)
        ; ConsId = int16_const(_)
        ; ConsId = uint16_const(_)
        ; ConsId = int32_const(_)
        ; ConsId = uint32_const(_)
        ; ConsId = float_const(_)
        ; ConsId = char_const(_)
        ; ConsId = string_const(_)
        ; ConsId = impl_defined_const(_)
        ; ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = type_info_const(_)
        ; ConsId = typeclass_info_const(_)
        ; ConsId = ground_term_const(_, _)
        ; ConsId = tabling_info_const(_)
        ; ConsId = table_io_entry_desc(_)
        ; ConsId = deep_profiling_proc_layout(_)
        ),
        unexpected($module, $pred, "cannot handle cons_id")
    ).

selector_init_from_list(Types)
    = list.map((func(T) = typesel(T)), Types).

selector_termshift(S1, S2, S) :-
    list.append(S1, S2, S).

selector_subsumed_by(ModuleInfo, Normalization, S1, S2, MainType, Extension) :-
    % First make sure that both selectors are in a normalized form.
    (
        Normalization = already_normalized,
        NormS1 = S1,
        NormS2 = S2
    ;
        Normalization = need_normalization,
        normalize_selector_with_type_information(ModuleInfo, MainType,
            S1, NormS1),
        normalize_selector_with_type_information(ModuleInfo, MainType,
            S2, NormS2)
    ),
    (
        only_term_selectors(NormS1),
        only_term_selectors(NormS2)
    ->
        % Easy case.
        term_selector_subsumed_by(NormS1, NormS2, Extension)
    ;
        selector_subsumed_by_2(ModuleInfo, NormS1, NormS2, MainType, Extension)
    ).

:- pred only_term_selectors(selector::in) is semidet.

only_term_selectors([]).
only_term_selectors([H | T]) :-
    H = termsel(_, _),
    only_term_selectors(T).

    % Both selectors must only contain term selectors.
    %
:- pred term_selector_subsumed_by(selector::in, selector::in, selector::out)
    is semidet.

term_selector_subsumed_by(S1, S2, Extension) :-
    list.append(S2, Extension, S1).

    % The general case of selector_subsumed_by, where either selector may
    % contain type selectors.
    %
:- pred selector_subsumed_by_2(module_info::in, selector::in, selector::in,
    mer_type::in, selector::out) is semidet.

selector_subsumed_by_2(ModuleInfo, A, B, Type, Extension) :-
    (
        B = [],
        Extension = A
    ;
        A = [AH | AT],
        B = [BH | BT],
        (
            AH = termsel(ConsIdA, IndexA),
            BH = termsel(ConsIdB, IndexB)
        ->
            ConsIdA = ConsIdB,
            IndexA = IndexB,
            (
                Type = type_variable(_, _),
                AT = BT
            ->
                % XXX Avoid an assertion failure when trying to index arguments
                % of a type variable.  This is probably a hack.
                Extension = []
            ;
                % If both selectors begin with term selectors, clearly
                % they must agree on the node to select for the selectors
                % to be comparable.
                SubType = det_select_subtype(ModuleInfo, Type, ConsIdA,
                    IndexA),
                selector_subsumed_by_2(ModuleInfo, AT, BT, SubType, Extension)
            )
        ;
            % If one selector has a term selector at the current position but
            % the other has a type selector, we select the node dictated by the
            % term selector.  We also verify that the type of that node
            % contains within it a node selectable by the type selector.
            AH = termsel(ConsIdA, IndexA),
            BH = typesel(SubTypeB),
            SubTypeA = det_select_subtype(ModuleInfo, Type, ConsIdA, IndexA),
            ( SubTypeA = SubTypeB ->
                % Both selectors agree on the subtype to select.
                selector_subsumed_by_2(ModuleInfo, AT, BT, SubTypeA, Extension)
            ;
                type_contains_subtype(ModuleInfo, SubTypeA, SubTypeB),
                selector_subsumed_by_2(ModuleInfo, AT, B, SubTypeA, Extension)
            )
        ;
            % Symmetric with the previous case.
            AH = typesel(SubTypeA),
            BH = termsel(ConsIdB, IndexB),
            SubTypeB = det_select_subtype(ModuleInfo, Type, ConsIdB, IndexB),
            ( SubTypeA = SubTypeB ->
                % Both selectors agree on the subtype to select.
                selector_subsumed_by_2(ModuleInfo, AT, BT, SubTypeB, Extension)
            ;
                type_contains_subtype(ModuleInfo, SubTypeB, SubTypeA),
                selector_subsumed_by_2(ModuleInfo, A, BT, SubTypeB, Extension)
            )
        ;
            AH = typesel(SubTypeA),
            BH = typesel(SubTypeB),
            (
                SubTypeA = SubTypeB
            ->
                % Both selectors begin with type selectors and agree on the
                % subtype to select.
                selector_subsumed_by_2(ModuleInfo, AT, BT, SubTypeB, Extension)
            ;
                % Assume we select node according to the B selector, then check
                % that the rest of B subsumes A.
                type_contains_subtype(ModuleInfo, SubTypeB, SubTypeA),
                selector_subsumed_by_2(ModuleInfo, A, BT, SubTypeB, Extension0)
            ->
                % Don't succeed for something like:
                %   A   = [typesel(foo)],
                %   B   = [typesel(bar)],
                %   Ext = [typesel(foo)]
                % i.e.
                %   [typesel(bar), typesel(foo)] = [typesel(bar)]
                Extension0 \= A,
                Extension = Extension0
            ;
                % Assume we select node according to the A selector, then check
                % that B subsumes the rest of A.
                type_contains_subtype(ModuleInfo, SubTypeA, SubTypeB),
                selector_subsumed_by_2(ModuleInfo, AT, B, SubTypeA, Extension)
            )
        )
    ).

    % type_contains_subtype(ModuleInfo, FromType, ToType).
    %
    % Succeed iff starting from FromType we can reach a node ToType.
    %
    % XXX I didn't think about type variables when writing this.
    %
:- pred type_contains_subtype(module_info::in, mer_type::in, mer_type::in)
    is semidet.

type_contains_subtype(ModuleInfo, FromType, ToType) :-
    ( FromType = ToType ->
        true
    ;
        type_contains_subtype_1(ModuleInfo, FromType, ToType, Contains),
        Contains = yes
    ).

:- pred type_contains_subtype_1(module_info::in, mer_type::in, mer_type::in,
    bool::out) is det.

    % We assume that type definitions for a module don't change for the
    % duration of the analysis.
    %
:- pragma memo(type_contains_subtype_1/4,
    [allow_reset, specified([promise_implied, value, value, output])]).

type_contains_subtype_1(ModuleInfo, FromType, ToType, Contains) :-
    queue.put(FromType, queue.init, Queue0),
    type_contains_subtype_2(ModuleInfo, ToType, Queue0, _Queue,
        set.init, _SeenTypes, Contains).

    % We perform a breadth-first search, keeping track of the types that
    % already seen, to avoid some really bad performance when performing
    % structure sharing analysis on some modules.
    %
:- pred type_contains_subtype_2(module_info::in, mer_type::in,
    queue(mer_type)::in, queue(mer_type)::out,
    set(mer_type)::in, set(mer_type)::out, bool::out) is det.

type_contains_subtype_2(ModuleInfo, ToType, !Queue, !SeenTypes, Contains) :-
    ( queue.get(FromType, !Queue) ->
        ( set.contains(!.SeenTypes, FromType) ->
            type_contains_subtype_2(ModuleInfo, ToType, !Queue, !SeenTypes,
                Contains)
        ;
            set.insert(FromType, !SeenTypes),
            type_arg_types(ModuleInfo, FromType, ArgTypes),
            ( list.member(ToType, ArgTypes) ->
                Contains = yes
            ;
                queue.put_list(ArgTypes, !Queue),
                type_contains_subtype_2(ModuleInfo, ToType, !Queue, !SeenTypes,
                    Contains)
            )
        )
    ;
        Contains = no
    ).

:- pred type_arg_types(module_info::in, mer_type::in, list(mer_type)::out)
    is det.

:- pragma memo(type_arg_types/3,
    [allow_reset, specified([promise_implied, value, output])]).

type_arg_types(ModuleInfo, Type, ArgTypes) :-
    solutions(
        (pred(ConsIdArgTypes::out) is nondet :-
            cons_id_arg_types(ModuleInfo, Type, _ConsId, ConsIdArgTypes)
        ), ArgTypesLists),
    list.condense(ArgTypesLists, ArgTypes).

type_of_node(ModuleInfo, StartType, Selector, SubType) :-
    (
        Selector = [UnitSelector | RestSelector],
        (
            UnitSelector = termsel(ConsId, Index),
            select_subtype(ModuleInfo, StartType, ConsId, Index, SubType0)
        ;
            UnitSelector = typesel(SubType0)
        ),
        type_of_node(ModuleInfo, SubType0, RestSelector, SubType)
    ;
        Selector = [],
        SubType = StartType
    ).

    % select_subtype(ModuleInfo, Type, ConsID, Position) = SubType.
    % Determine the type of the type node selected from the type tree Type,
    % selecting the specific constructor (ConsId), at position Position.
    % Position counts starting from 1.
    %
:- func det_select_subtype(module_info, mer_type, cons_id, int) = mer_type.

det_select_subtype(ModuleInfo, Type, ConsID, Position) = SubType :-
    ( select_subtype(ModuleInfo, Type, ConsID, Position, SubType0) ->
        SubType = SubType0
    ;
        throw(encounter_existential_subtype)
    ).

:- pred select_subtype(module_info::in, mer_type::in, cons_id::in, int::in,
    mer_type::out) is semidet.

select_subtype(ModuleInfo, Type, ConsID, Position, SubType) :-
    (
        get_cons_id_non_existential_arg_types(ModuleInfo, Type, ConsID,
            ArgTypes)
    ->
        SubType = list.det_index1(ArgTypes, Position)
    ;
        get_existq_cons_defn(ModuleInfo, Type, ConsID, CtorDefn)
    ->
        CtorDefn = ctor_defn(_TVarSet, ExistQVars, _KindMap, _Constraints,
            ArgTypes, _RetType),
        SubType = list.det_index1(ArgTypes, Position),
        not (
            SubType = type_variable(TVar, _),
            list.member(TVar, ExistQVars)
        )
    ;
        unexpected($module, $pred,
            "type is both existential and non-existential")
    ).

:- pragma memo(normalize_selector_with_type_information/4,
    [allow_reset, specified([promise_implied, value, value, output])]).

normalize_selector_with_type_information(ModuleInfo, Type, !Selector) :-
    ( is_introduced_type_info_type(Type) ->
        true
    ;
        branch_map_init(BranchMap0),
        branch_map_insert(Type, top_selector, BranchMap0, BranchMap1),
        do_normalize_selector(ModuleInfo, Type, BranchMap1, top_selector,
            !Selector)
    ).

:- pred do_normalize_selector(module_info::in, mer_type::in,
    branch_map::in, selector::in, selector::in, selector::out) is det.

do_normalize_selector(ModuleInfo, VarType, BranchMap0,
        SelectorAcc0, !Selector) :-
    (
        !.Selector = [UnitSelector | SelRest],
        CtorCat = classify_type(ModuleInfo, VarType),
        ( CtorCat = ctor_cat_user(CatUser) ->
            (
                CatUser = cat_user_general
            ;
                CatUser = cat_user_notag
            ;
                CatUser = cat_user_direct_dummy,
                % We should not be producing selectors for dummy types.
                unexpected($module, $pred, "cat_user_direct_dummy")
            ),

            % If it is either a term-selector of a non-existentially typed
            % functor or is a type-selector, construct the branch map and
            % proceed with normalization. If it is a term-selector of an
            % existentially typed functor, then normalization stops.
            (
                (
                    UnitSelector = termsel(ConsId, Index),
                    get_cons_id_non_existential_arg_types(ModuleInfo,
                        VarType, ConsId, ArgTypes),
                    ( list.index1(ArgTypes, Index, SubType) ->
                        CType = SubType
                    ;
                        unexpected($module, $pred,
                            "accessing nonexistent index")
                    )
                ;
                    UnitSelector = typesel(CType)
                )
            ->
                !:Selector = SelRest,
                ( branch_map_search(BranchMap0, CType, BranchSelector) ->
                    do_normalize_selector(ModuleInfo, CType, BranchMap0,
                        BranchSelector, !Selector)
                ;
                    selector_termshift(SelectorAcc0, [UnitSelector],
                        SelectorAcc1),
                    branch_map_insert(CType, SelectorAcc1,
                        BranchMap0, BranchMap1),
                    do_normalize_selector(ModuleInfo, CType, BranchMap1,
                        SelectorAcc1, !Selector)
                )
            ;
                % Existentially typed functor.
                % XXX awaiting confirmation on this from Nancy but it seems
                % right to me --pw
                append(SelectorAcc0, [UnitSelector], !:Selector)
            )
        ;
            % If it is not a user type, SelRest is empty anyhow, and
            % normalization stops.
            % Resulting selector = accumulator.sel0
            append(SelectorAcc0, !Selector)
        )
    ;
        !.Selector = [],
        !:Selector = SelectorAcc0
    ).

selector_apply_widening(ModuleInfo, MainType, Selector0, Selector) :-
    (
        Selector0 = [],
        Selector = []
    ;
        Selector0 = [_ | _],
        ( type_of_node(ModuleInfo, MainType, Selector0, SubType) ->
            Selector = [typesel(SubType)]
        ;
            % The node is existentially typed.  Let's just leave the selector
            % as-is.
            Selector = Selector0
        )
    ).

%-----------------------------------------------------------------------------%
%
% BRANCH_MAP : copy/pasted from wimvh/bta_reduce.m
%

:- type branch_map == assoc_list(mer_type, selector).

:- pred branch_map_init(branch_map::out) is det.

branch_map_init([]).

:- pred branch_map_insert(mer_type::in, selector::in,
    branch_map::in, branch_map::out) is det.

branch_map_insert(Type, Sel, TypeSels, [Type - Sel | TypeSels]).

:- pred branch_map_search(branch_map::in, mer_type::in, selector::out)
    is semidet.

branch_map_search([Type - Sel | TypeSels], KeyType, ValueSel):-
    map.init(Empty),
    % The two types are considered equal if they unify under an
    % empty substitution.
    (
        type_unify(Type, KeyType, [], Empty, Subst),
        map.is_empty(Subst)
    ->
        ValueSel = Sel
    ;
        branch_map_search(TypeSels, KeyType, ValueSel)
    ).

%-----------------------------------------------------------------------------%

reset_tables(!IO) :-
    table_reset_for_type_contains_subtype_1_4(!IO),
    table_reset_for_type_arg_types_3(!IO),
    table_reset_for_normalize_selector_with_type_information_4(!IO).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.selector.
%-----------------------------------------------------------------------------%
