%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
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

    % Create a selector as either the top selector, a term selector,
    % or a type selector.
    %
:- func top_selector = selector.
:- func selector_init(cons_id, int) = selector.
:- func selector_init_from_list(list(mer_type)) = selector.

    % Perform a termshift operation.
    %
:- pred selector_termshift(selector::in, selector::in, selector::out) is det.

    % subsumed_by(ModuleInfo, Selector0, Selector1, Type, Extension).
    %
    % Returns true if Selector0 is subsumed by Selector1. This means that
    % Selector1 is more general than Selector0, hence, there exists an
    % extension Ext, such that Selector1.Extension = Selector0.
    %
    % The type specifies the type of the term to which the selectors refer.
    %
:- pred subsumed_by(module_info::in, selector::in, selector::in,
    mer_type::in, selector::out) is semidet.

    % Using the type information of the variable to which the given selector
    % belongs, normalize that selector.
    % S2 is the normalized form of S1 iff:
    %   * type of the node selected by S2 = type of the node selected by S1.
    %   * a path of selectors can be constructed which leads from S2 to S1.
    %
:- pred normalize_selector_with_type_information(module_info::in, mer_type::in,
    selector::in, selector::out) is det.

    % SubType = type_of_node(ModuleInfo, StartType, Selector).
    % Determines the type SubType of the node obtained by traversing the type
    % tree of StartType using the path Selector.
    %
:- func type_of_node(module_info, mer_type, selector) = mer_type.

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
:- import_module parse_tree.prog_type_subst.

:- import_module assoc_list.
:- import_module map.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

top_selector = [].

selector_init(Cons, Index) = [termsel(Cons, Index)].
selector_init_from_list(Types)
    = list.map((func(T) = typesel(T)), Types).

selector_termshift(S1, S2, S) :-
    list.append(S1, S2, S).

subsumed_by(ModuleInfo, S1, S2, MainType, Extension):-
    % First make sure that both selectors are in a normalized form.
    normalize_selector_with_type_information(ModuleInfo, MainType, S1, NormS1),
    normalize_selector_with_type_information(ModuleInfo, MainType, S2, NormS2),
    subsumed_by_2(ModuleInfo, NormS1, NormS2, MainType, Extension).

:- pred subsumed_by_2(module_info::in, selector::in, selector::in,
    mer_type::in, selector::out) is semidet.

subsumed_by_2(ModuleInfo, S1, S2, MainType, Extension):-
    (
        split_upto_type_selector(S2, S2_part1, TS, S2_part2),
        TS = typesel(SubType)
    ->
        % step 1: S2_part1.Rest = S1, hence S2_part1 should be more general
        % than S1.
        subsumed_by_2(ModuleInfo, S1, S2_part1, MainType, Rest),

        % step 2: S2_part1.TS.Remainder = S1, hence S2_part1.TS should be more
        % general than S1.
        % i.e.
        % Check the type-selector part: starting from the type selected by
        % S2_part1, does the remainder of Rest (the extension of S2_part1
        % such that S2_part1.Rest = S1) lead through a node with type
        % "SubType".
        %
        type_on_path(ModuleInfo, type_of_node(ModuleInfo, MainType, S2_part1),
            SubType, Rest, Remainder),

        % step 3:
        % % S2_part1.TS.S1_part2 should be more general than S1.
        subsumed_by_2(ModuleInfo, Remainder, S2_part2, SubType, Extension)
    ;
        % If the second selector S2 has no type-selectors, we have the
        % simple case where S1 can be more general than S2 if there exists
        % a path "Extension" such that S1.Extension = S2
        selector_subsumed_by(S1, S2, Extension)
    ).

:- pred selector_subsumed_by(selector::in, selector::in, selector::out)
    is semidet.

selector_subsumed_by(S1, S2, Extension):-
    list.append(S2, Extension, S1).

type_of_node(ModuleInfo, StartType, Selector) = SubType :-
    (
        Selector = [UnitSelector | RestSelector],
        (
            UnitSelector = termsel(ConsId, Index),
            SubType0 = select_subtype(ModuleInfo, StartType, ConsId, Index)
        ;
            UnitSelector = typesel(SubType0)
        ),
        SubType = type_of_node(ModuleInfo, SubType0, RestSelector)
    ;
        Selector = [],
        SubType = StartType
    ).

    % SubType = select_subtype(ModuleInfo, Type, ConsID, Position, SubType).
    % Determine the type of the type node selected from the type tree Type,
    % selecting the specific constructor (ConsId), at position Position.
    % Position counts starting from 1.
    %
:- func select_subtype(module_info, mer_type, cons_id, int) = mer_type.

select_subtype(ModuleInfo, Type, ConsID, Choice) = SubType :-
    (
        get_cons_id_non_existential_arg_types(ModuleInfo, Type, ConsID,
            ArgTypes)
    ->
        ( list.index1(ArgTypes, Choice, SubType0) ->
            SubType = SubType0
        ;
            unexpected(this_file, "get_type_of_node: selection failed.")
        )
    ;
        unexpected(this_file, "get_type_of_node: existential type.")
    ).

    % split_upto_type_selector(Sin, S1, TS, S2).
    %
    % This predicate succeeds if there exists a typeselector TS, such that Sin
    % is equivalent to append(S1, [TS | S2]) and S1 contains no other type
    % selector. It fails otherwise.
    %
:- pred split_upto_type_selector(selector::in, selector::out,
    unit_selector::out, selector::out) is semidet.

split_upto_type_selector(Sin, S1, TS, S2):-
    list.takewhile(is_term_selector, Sin, S1, Remainder),
    Remainder = [TS | S2].

:- pred is_term_selector(unit_selector::in) is semidet.

is_term_selector(termsel(_, _)).

    % type_on_path(ModuleInfo, FromType, ToType, Path, Remainder).
    %
    % This predicate verifies that the path Path starting from FromType
    % encounters at least one type node with the type ToType.  Remainder is the
    % remainder of the Path after stripping it to the last encounter of a node
    % with "ToType".
    %
    % XXX Changed w.r.t. original implementation!
    %
:- pred type_on_path(module_info::in, mer_type::in, mer_type::in,
    selector::in, selector::out) is semidet.

type_on_path(ModuleInfo, FromType, ToType, Path, RemainderPath) :-
    %
    % In checking this, at least one step of the Path must be done. Indeed, if
    % FromType = ToType, than RemainderPath would be equal to Path, which would
    % contradict the actual meaning of a type selector: a type-selector is a
    % shortcut notation for any non-zero (!) selector that selects a node of
    % the type described by the type-selector.
    %
    type_on_path_2(first, ModuleInfo, FromType, ToType, Path, RemainderPath).

    % In checking whether a type is encountered on a given selector-path
    % we check whether the type of a selector is encountered _after_ the first
    % unit-step on that selector-path. This means that we need to make
    % a difference when looking at the first unit-step or any of the subsequent
    % steps.
    %
    % To make the difference during the verification process, the type 'step'
    % is used.
    %
:- type step
    --->    first
    ;       subsequent.

:- pred type_on_path_2(step::in, module_info::in, mer_type::in, mer_type::in,
    selector::in, selector::out) is semidet.

type_on_path_2(Step, ModuleInfo, FromType, ToType, Path, RemainderPath) :-
    (
        FromType = ToType,
        Step = subsequent
    ->
        RemainderPath = Path
    ;
        Path = [UnitSelector | Rest],
        (
            UnitSelector = typesel(SubType),
            ( SubType = ToType ->
                (
                    % Check if the same type occurs anywhere further on the
                    % path.
                    type_on_path_2(first, ModuleInfo, ToType, ToType,
                        Rest, RemainderPath0)
                ->
                    RemainderPath = RemainderPath0
                ;
                    RemainderPath = Rest
                )
            ;
                type_on_path_2(subsequent, ModuleInfo, SubType, ToType,
                    Rest, RemainderPath)
            )
        ;
            UnitSelector = termsel(ConsId, Index),
            SubType = select_subtype(ModuleInfo, FromType, ConsId, Index),
            ( SubType = ToType ->
                (
                    % Check if the same type occurs anywhere further on the
                    % path.
                    type_on_path_2(first, ModuleInfo, ToType, ToType,
                        Rest, RemainderPath0)
                ->
                    RemainderPath = RemainderPath0
                ;
                    RemainderPath = Rest
                )
            ;
                type_on_path_2(subsequent, ModuleInfo, SubType, ToType,
                    Rest, RemainderPath)
            )
        )
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
        Class = classify_type(ModuleInfo, VarType),
        ( Class = type_cat_user_ctor ->
            % If it is either a term-selector of a non existentially typed
            % functor or a type-selector, construct the branch map and proceed
            % with normalization. If it is a term-selector of an existentially
            % typed functor, than normalization stops.
            (
                (
                    UnitSelector = termsel(ConsId, Index),
                    get_cons_id_non_existential_arg_types(ModuleInfo,
                    VarType, ConsId, ArgTypes),
                    ( list.index1(ArgTypes, Index, SubType) ->
                        CType = SubType
                    ;
                        unexpected(this_file, "normalize_wti: " ++
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
                append(SelectorAcc0, !Selector)
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

selector_apply_widening(ModuleInfo, MainType, !Selector) :-
    (
        !.Selector = []
    ;
        !.Selector = [_ | _],
        UnitSelector = typesel(type_of_node(ModuleInfo, MainType, !.Selector)),
        !:Selector = [UnitSelector]
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
