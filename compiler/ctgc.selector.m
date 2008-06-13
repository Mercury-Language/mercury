%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006, 2008 The University of Melbourne.
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

:- import_module hlds.hlds_module.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

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

    % subsumed_by(ModuleInfo, Selector1, Selector2, Type, Extension).
    %
    % Succeeds iff Selector1 is subsumed by Selector2. This means that
    % Selector2 is more general than Selector1, hence, there exists an
    % Extension, such that Selector2.Extension = Selector1.
    %
    % The type specifies the type of the term to which the selectors refer.
    %
:- pred subsumed_by(module_info::in, normalization::in,
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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module map.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

top_selector = [].

selector_init(Cons, Index) = [TermSel] :-
    (
        Cons = cons(_, _),
        TermSel = termsel(Cons, Index)
    ;
        ( Cons = int_const(_)
        ; Cons = string_const(_)
        ; Cons = float_const(_)
        ; Cons = implementation_defined_const(_)
        ; Cons = pred_const(_, _)
        ; Cons = type_ctor_info_const(_, _, _)
        ; Cons = base_typeclass_info_const(_, _, _, _)
        ; Cons = type_info_cell_constructor(_)
        ; Cons = typeclass_info_cell_constructor
        ; Cons = tabling_info_const(_)
        ; Cons = deep_profiling_proc_layout(_)
        ; Cons = table_io_decl(_)
        ),
        unexpected(this_file, "selector_init: cannot handle cons_id")
    ).

selector_init_from_list(Types)
    = list.map((func(T) = typesel(T)), Types).

selector_termshift(S1, S2, S) :-
    list.append(S1, S2, S).

subsumed_by(ModuleInfo, Normalization, S1, S2, MainType, Extension):-
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
        selector_subsumed_by(NormS1, NormS2, Extension)
    ;
        subsumed_by_2(ModuleInfo, NormS1, NormS2, MainType, Extension)
    ).

:- pred only_term_selectors(selector::in) is semidet.

only_term_selectors([]).
only_term_selectors([H | T]) :-
    H = termsel(_, _),
    only_term_selectors(T).

    % Both selectors must only contain term selectors.
    %
:- pred selector_subsumed_by(selector::in, selector::in, selector::out)
    is semidet.

selector_subsumed_by(S1, S2, Extension):-
    list.append(S2, Extension, S1).

    % The general case of subsumed_by, where either selector may contain type
    % selectors.
    %
:- pred subsumed_by_2(module_info::in, selector::in, selector::in,
    mer_type::in, selector::out) is semidet.

subsumed_by_2(ModuleInfo, A, B, Type, Extension) :-
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
            % If both selectors begin with term selectors, clearly they must
            % agree on the node to select for the selectors to be comparable.
            ConsIdA = ConsIdB,
            IndexA = IndexB,
            SubType = det_select_subtype(ModuleInfo, Type, ConsIdA, IndexA),
            subsumed_by_2(ModuleInfo, AT, BT, SubType, Extension)
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
                subsumed_by_2(ModuleInfo, AT, BT, SubTypeA, Extension)
            ;
                type_contains_subtype(ModuleInfo, SubTypeA, SubTypeB),
                subsumed_by_2(ModuleInfo, AT, B, SubTypeA, Extension)
            )
        ;
            % Symmetric with the previous case.
            AH = typesel(SubTypeA),
            BH = termsel(ConsIdB, IndexB),
            SubTypeB = det_select_subtype(ModuleInfo, Type, ConsIdB, IndexB),
            ( SubTypeA = SubTypeB ->
                % Both selectors agree on the subtype to select.
                subsumed_by_2(ModuleInfo, AT, BT, SubTypeB, Extension)
            ;
                type_contains_subtype(ModuleInfo, SubTypeB, SubTypeA),
                subsumed_by_2(ModuleInfo, A, BT, SubTypeB, Extension)
            )
        ;
            AH = typesel(SubTypeA),
            BH = typesel(SubTypeB),
            (
                SubTypeA = SubTypeB
            ->
                % Both selectors begin with type selectors and agree on the
                % subtype to select.
                subsumed_by_2(ModuleInfo, AT, BT, SubTypeB, Extension)
            ;
                % Assume we select node according to the B selector, then check
                % that the rest of B subsumes A.
                type_contains_subtype(ModuleInfo, SubTypeB, SubTypeA),
                subsumed_by_2(ModuleInfo, A, BT, SubTypeB, Extension0)
            ->
                Extension = Extension0
            ;
                % Assume we select node according to the A selector, then check
                % that B subsumes the rest of A.
                type_contains_subtype(ModuleInfo, SubTypeA, SubTypeB),
                subsumed_by_2(ModuleInfo, AT, B, SubTypeA, Extension)
            )
        )
    ).

    % type_contains_subtype(ModuleInfo, FromType, ToType).
    %
    % Succeed iff starting from FromType we can reach a node ToType.
    %
:- pred type_contains_subtype(module_info::in, mer_type::in, mer_type::in)
    is semidet.

type_contains_subtype(ModuleInfo, FromType, ToType) :-
    type_contains_subtype_2(ModuleInfo, FromType, ToType, []).

:- pred type_contains_subtype_2(module_info::in, mer_type::in, mer_type::in,
    list(mer_type)::in) is semidet.

type_contains_subtype_2(ModuleInfo, FromType, ToType, SeenTypes0) :-
    (
        FromType = ToType
    ;
        SeenTypes = [FromType | SeenTypes0],
        cons_id_arg_types(ModuleInfo, FromType, _ConsId, ArgTypes),
        list.member(ArgType, ArgTypes),
        not list.member(ArgType, SeenTypes),
        type_contains_subtype_2(ModuleInfo, ArgType, ToType, SeenTypes)
    ).

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

    % det_select_subtype(ModuleInfo, Type, ConsID, Position) = SubType.
    % Determine the type of the type node selected from the type tree Type,
    % selecting the specific constructor (ConsId), at position Position.
    % Position counts starting from 1.
    %
:- func det_select_subtype(module_info, mer_type, cons_id, int) = mer_type.

det_select_subtype(ModuleInfo, Type, ConsID, Position) = SubType :-
    ( select_subtype(ModuleInfo, Type, ConsID, Position, SubType0) ->
        SubType = SubType0
    ;
        unexpected(this_file, "select_subtype: existential subtype")
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
        unexpected(this_file,
            "select_subtype: type is both existential and non-existential")
    ).

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
                unexpected(this_file,
                    "do_normalize_selector: cat_user_direct_dummy")
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
                        unexpected(this_file,
                            "do_normalize_selector: " ++
                            "accessing non-existing index.")
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
            % The node is existentially typed. Try for the type of the node's
            % parent instead.
            list.det_split_last(Selector0, ParentSelector, _),
            selector_apply_widening(ModuleInfo, MainType, ParentSelector,
                Selector)
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

:- func this_file = string.

this_file = "ctgc.selector.m".

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
