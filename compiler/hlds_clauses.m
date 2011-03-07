%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: hlds_clauses.m.
% Main authors: fjh, conway.
%
% This module defines the part of the HLDS that deals with clauses.
%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_clauses.
:- interface.

:- import_module hlds.hlds_args.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.error_util.
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module term.

%-----------------------------------------------------------------------------%

    % The clauses_info structure contains the clauses for a predicate
    % after conversion from the item_list by make_hlds.m.
    % Typechecking is performed on the clauses info, then the clauses
    % are copied to create the proc_info for each procedure.
    % After mode analysis the clauses and the procedure goals are not
    % guaranteed to be the same, and the clauses are only kept so that
    % the optimized goal can be compared with the original in HLDS dumps.
    %
:- type clauses_info
    --->    clauses_info(
                % The varset describing the clauses.
                cli_varset                  :: prog_varset,

                % Variable types from explicit qualifications.
                cli_explicit_vartypes       :: vartypes,

                % Map from variable name to type variable for the type
                % variables occurring in the argument types. This is used
                % to process explicit qualifications.
                cli_tvar_name_map           :: tvar_name_map,

                % Variable types inferred by typecheck.m.
                cli_vartypes                :: vartypes,

                % The head variables.
                cli_headvars                :: proc_arg_vector(prog_var),

                % The clauses themselves (some may be pragma foreign_procs).
                cli_rep                     :: clauses_rep,

                % Information about where the clauses came fro.
                cli_item_numbers            :: clause_item_numbers,

                % This field is computed by polymorphism.m.
                cli_rtti_varmaps            :: rtti_varmaps,

                % Do we have foreign language clauses?
                cli_have_foreign_clauses    :: bool
        ).

:- pred clauses_info_init(pred_or_func::in, int::in, clause_item_numbers::in,
    clauses_info::out) is det.

:- pred clauses_info_init_for_assertion(prog_vars::in, clauses_info::out)
    is det.

:- type clauses_rep.

:- func init_clauses_rep = clauses_rep.

    % Returns yes iff the given clauses_rep represents the empty list of
    % clauses.
    %
:- func clause_list_is_empty(clauses_rep) = bool.

    % Returns the number of clauses in the clauses list.
    %
:- func num_clauses_in_clauses_rep(clauses_rep) = int.

    % Adds the given clause to the end of the clause list.
    %
:- pred add_clause(clause::in, clauses_rep::in, clauses_rep::out) is det.

    % Get the list of clauses in the given clauses_rep in whatever order
    % happens to be efficient.
    %
:- pred get_clause_list_any_order(clauses_rep::in, list(clause)::out) is det.

    % Get the list of clauses in the given clauses_rep in program order.
    %
:- pred get_clause_list(clauses_rep::in, list(clause)::out) is det.

    % Set the list of clauses to the one given.
    %
:- pred set_clause_list(list(clause)::in, clauses_rep::out) is det.

:- pred clauses_info_get_varset(clauses_info::in, prog_varset::out) is det.

    % This partial map holds the types specified by any explicit
    % type qualifiers in the clauses.
    %
:- pred clauses_info_get_explicit_vartypes(clauses_info::in, vartypes::out)
    is det.

    % This map contains the types of all the variables, as inferred
    % by typecheck.m.
    %
:- pred clauses_info_get_vartypes(clauses_info::in, vartypes::out) is det.

:- pred clauses_info_get_rtti_varmaps(clauses_info::in, rtti_varmaps::out)
    is det.

:- pred clauses_info_get_headvars(clauses_info::in,
    proc_arg_vector(prog_var)::out) is det.

    % Return the headvars as a list rather than as a proc_arg_vector.
    % New code should avoid using this and should instead be written to
    % work with the arg_vector structure directly.
    %
:- pred clauses_info_get_headvar_list(clauses_info::in, list(prog_var)::out)
    is det.

:- pred clauses_info_get_clauses_rep(clauses_info::in, clauses_rep::out,
    clause_item_numbers::out) is det.

    % Return the list of clauses in program order, and if necessary update
    % the cache of this info in the clauses_info.
    %
:- pred clauses_info_clauses(list(clause)::out, clause_item_numbers::out,
    clauses_info::in, clauses_info::out) is det.

:- pred clauses_info_set_headvars(proc_arg_vector(prog_var)::in,
    clauses_info::in, clauses_info::out) is det.

:- pred clauses_info_set_clauses_rep(clauses_rep::in, clause_item_numbers::in,
    clauses_info::in, clauses_info::out) is det.

:- pred clauses_info_set_varset(prog_varset::in,
    clauses_info::in, clauses_info::out) is det.

    % This partial map holds the types specified by any explicit
    % type qualifiers in the clauses.
    %
:- pred clauses_info_set_explicit_vartypes(vartypes::in,
    clauses_info::in, clauses_info::out) is det.

    % This map contains the types of all the variables, as inferred
    % by typecheck.m.
    %
:- pred clauses_info_set_vartypes(vartypes::in,
    clauses_info::in, clauses_info::out) is det.

:- pred clauses_info_set_rtti_varmaps(rtti_varmaps::in,
    clauses_info::in, clauses_info::out) is det.

:- type clause
    --->    clause(
                % Modes for which this clause applies.
                clause_applicable_procs     :: clause_applicable_modes,
                clause_body                 :: hlds_goal,
                clause_lang                 :: implementation_language,
                clause_context              :: prog_context,
                clause_statevar_warnings    :: list(error_spec)
            ).

:- func clause_body(clause) = hlds_goal.

:- type clause_applicable_modes
    --->    all_modes
            % This clause is applicable to all modes of the predicate.

    ;       selected_modes(list(proc_id)).
            % This clause or foreign_proc is applicable only to this given
            % list of modes.
            %
            % The list should always be sorted, and should never be empty.
            %
            % The list *may* be the same as the list of all the modes of the
            % predicate. If it is, this indicates that the clause came from
            % a mode-specific clause or foreign_proc, contexts that would
            % normally imply that the clause is applicable only to one selected
            % mode, but that we don't know what that mode is, perhaps because
            % of an error in the predicate's definition, such as a
            % mode-specific clause for a nonexistent mode.
            %
            % For such erroneous clauses and foreign_procs, this is the only
            % way to get them to be typechecked (at least for now).

%-----------------------------------------------------------------------------%

    % We want to know whether the clauses of each predicate (which may include
    % pragma foreign_procs) are contiguous in the source code or not.
    %
    % To this end, we record the item numbers of all the clauses of the

:- type clause_item_numbers.

:- type clause_item_number_region
    --->    clause_item_number_region(
                cnr_lower_item_number   ::  int,
                cnr_upper_item_number   ::  int,
                cnr_lower_item_context  ::  term.context,
                cnr_upper_item_context  ::  term.context
            ).

:- type clause_item_number_types
    --->    only_clauses
    ;       clauses_and_foreign_procs.

:- pred clauses_are_non_contiguous(clause_item_numbers::in,
    clause_item_number_types::in,
    clause_item_number_region::out, clause_item_number_region::out,
    list(clause_item_number_region)::out) is semidet.

:- type clause_item_number_type
    --->    item_is_clause
    ;       item_is_foreign_proc.

:- pred add_clause_item_number(maybe(int)::in, term.context::in,
    clause_item_number_type::in,
    clause_item_numbers::in, clause_item_numbers::out) is det.

:- func init_clause_item_numbers_user = clause_item_numbers.
:- func init_clause_item_numbers_comp_gen = clause_item_numbers.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module map.
:- import_module require.
:- import_module term.
:- import_module varset.

:- type clause_item_numbers
    --->    user_clauses(
                % This field records the locations of the Mercury language
                % clauses only.
                list(clause_item_number_region),

                % This field records the locations of both the Mercury language
                % clauses and the foreign language foreign_procs.
                list(clause_item_number_region)
            )
    ;       comp_gen_clauses.

init_clause_item_numbers_user = user_clauses([], []).
init_clause_item_numbers_comp_gen = comp_gen_clauses.

clauses_are_non_contiguous(ClauseItemNumbers, Type, FirstRegion, SecondRegion,
        LaterRegions) :-
    ClauseItemNumbers = user_clauses(MercuryRegions, BothRegions),
    (
        Type = only_clauses,
        MercuryRegions = [FirstRegion, SecondRegion | LaterRegions]
    ;
        Type = clauses_and_foreign_procs,
        BothRegions = [FirstRegion, SecondRegion | LaterRegions]
    ).

add_clause_item_number(MaybeItemNumber, Context, Type, !ClauseItemNumbers) :-
    (
        MaybeItemNumber = no,
        (
            !.ClauseItemNumbers = user_clauses(_MercuryRegions, _BothRegions)
            % This can happen for predicates defined in foreign languages
            % through pragma import. The ordinary declaration of the
            % predicate initializes !.ClauseItemNumbers to user_clauses,
            % and the first clue we have that the predicate actually has
            % no user clauses is the pragma import, whose processing
            % will yield a call to add_clause_item_number that ends up
            % here.
            %
            % We could insist on _MercuryRegions and _BothRegions being [],
            % but that would cause a compiler abort if a predicate had
            % some clauses and/or foreign_procs followed by a pragma import.
            % Such situations should be caught and reported by our ancestors.
        ;
            !.ClauseItemNumbers = comp_gen_clauses
        )
    ;
        MaybeItemNumber = yes(ItemNumber),
        (
            !.ClauseItemNumbers = user_clauses(MercuryRegions0, BothRegions0),
            (
                Type = item_is_clause,
                add_clause_item_number_regions(ItemNumber, Context,
                    MercuryRegions0, MercuryRegions)
            ;
                Type = item_is_foreign_proc,
                MercuryRegions = MercuryRegions0
            ),
            add_clause_item_number_regions(ItemNumber, Context,
                BothRegions0, BothRegions),
            !:ClauseItemNumbers = user_clauses(MercuryRegions, BothRegions)
        ;
            !.ClauseItemNumbers = comp_gen_clauses
            % Do not record the locations of any clauses that shouldn't be
            % there in the first place, since any error messages about such
            % clauses being out of order would be misleading (the error isn't
            % their non-contiguity, but their very existence).
        )
    ).

:- pred add_clause_item_number_regions(int::in, term.context::in,
    list(clause_item_number_region)::in, list(clause_item_number_region)::out)
    is det.

add_clause_item_number_regions(ItemNum, Context, !Regions) :-
    (
        !.Regions = [],
        NewRegion = clause_item_number_region(ItemNum, ItemNum,
            Context, Context),
        !:Regions = [NewRegion]
    ;
        !.Regions = [FirstRegion0 | LaterRegions0],
        FirstRegion0 = clause_item_number_region(
            LowerNum0, UpperNum0, LowerContext0, UpperContext0),
        ( ItemNum < LowerNum0 - 1 ->
            NewRegion = clause_item_number_region(ItemNum, ItemNum,
                Context, Context),
            !:Regions = [NewRegion, FirstRegion0 | LaterRegions0]
        ; ItemNum = LowerNum0 - 1 ->
            FirstRegion = clause_item_number_region(ItemNum, UpperNum0,
                Context, UpperContext0),
            !:Regions = [FirstRegion | LaterRegions0]
        ; ItemNum =< UpperNum0 ->
            unexpected(this_file,
                "add_clause_item_number: duplicate item number")
        ; ItemNum = UpperNum0 + 1 ->
            FirstRegion = clause_item_number_region(LowerNum0, ItemNum,
                LowerContext0, Context),
            !:Regions = [FirstRegion | LaterRegions0]
        ;
            add_clause_item_number_regions(ItemNum, Context,
                LaterRegions0, LaterRegions1),
            % See if need to merge FirstRegion0 with the first region
            % of LaterRegions1.
            (
                LaterRegions1 = [],
                unexpected(this_file,
                    "add_clause_item_number: insertion yields empty list")
            ;
                LaterRegions1 = [FirstLaterRegion1 | LaterLaterRegions1],
                FirstLaterRegion1 = clause_item_number_region(
                    LowerNum1, UpperNum1, _LowerContext1, UpperContext1),
                ( UpperNum0 + 1 = LowerNum1 ->
                    FirstRegion =
                        clause_item_number_region(LowerNum0, UpperNum1,
                            LowerContext0, UpperContext1),
                    !:Regions = [FirstRegion | LaterLaterRegions1]
                ;
                    !:Regions = [FirstRegion0, FirstLaterRegion1
                        | LaterLaterRegions1]
                )
            )
        )
    ).

%-----------------------------------------------------------------------------%

clauses_info_init(PredOrFunc, Arity, ItemNumbers, ClausesInfo) :-
    map.init(VarTypes),
    map.init(TVarNameMap),
    varset.init(VarSet0),
    make_n_fresh_vars("HeadVar__", Arity, HeadVars, VarSet0, VarSet),
    HeadVarVec = proc_arg_vector_init(PredOrFunc, HeadVars),
    rtti_varmaps_init(RttiVarMaps),
    HasForeignClauses = no,
    set_clause_list([], ClausesRep),
    ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap, VarTypes,
        HeadVarVec, ClausesRep, ItemNumbers, RttiVarMaps, HasForeignClauses).

clauses_info_init_for_assertion(HeadVars, ClausesInfo) :-
    varset.init(VarSet),
    map.init(VarTypes),
    map.init(TVarNameMap),
    % Procedures introduced for assertions are always predicates, never
    % functions.
    HeadVarVec = proc_arg_vector_init(pf_predicate, HeadVars),
    set_clause_list([], ClausesRep),
    rtti_varmaps_init(RttiVarMaps),
    HasForeignClauses = no,
    ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap, VarTypes,
        HeadVarVec, ClausesRep, init_clause_item_numbers_comp_gen,
        RttiVarMaps, HasForeignClauses).

clauses_info_get_varset(CI, CI ^ cli_varset).
clauses_info_get_explicit_vartypes(CI, CI ^ cli_explicit_vartypes).
clauses_info_get_vartypes(CI, CI ^ cli_vartypes).
clauses_info_get_headvars(CI, CI ^ cli_headvars).
clauses_info_get_headvar_list(CI, List) :-
    List = proc_arg_vector_to_list(CI ^ cli_headvars).
clauses_info_get_clauses_rep(CI, CI ^ cli_rep, CI ^ cli_item_numbers).
clauses_info_get_rtti_varmaps(CI, CI ^ cli_rtti_varmaps).

clauses_info_set_varset(X, !CI) :-
    !CI ^ cli_varset := X.
clauses_info_set_explicit_vartypes(X, !CI) :-
    !CI ^ cli_explicit_vartypes := X.
clauses_info_set_vartypes(X, !CI) :-
    !CI ^ cli_vartypes := X.
clauses_info_set_headvars(X, !CI) :-
    !CI ^ cli_headvars := X.
clauses_info_set_clauses_rep(X, Y, !CI) :-
    !CI ^ cli_rep := X,
    !CI ^ cli_item_numbers := Y.
clauses_info_set_rtti_varmaps(X, !CI) :-
    !CI ^ cli_rtti_varmaps := X.

    % In each of the alternatives below, the num field gives the number of
    % clauses. In the forw_list and both_forw fields, the clauses are in
    % program order. In the rev_list and both_rev fields, the clauses are in
    % reverse program order. It is an invariant that
    %
    %   list.reverse(Rep ^ both_rev, Rep ^ both_forw)
    %
    % holds.
:- type clauses_rep
    --->    cr_rev(
                rev_num     :: int,
                rev_list    :: list(clause)
            )
    ;       cr_forw(
                forw_num    :: int,
                forw_list   :: list(clause)
            )
    ;       cr_both(
                both_num    :: int,
                both_rev    :: list(clause),
                both_forw   :: list(clause)
            ).

init_clauses_rep = cr_forw(0, []).

clause_list_is_empty(ClausesRep) = IsEmpty :-
    (
        ClausesRep = cr_rev(_, List)
    ;
        ClausesRep = cr_forw(_, List)
    ;
        ClausesRep = cr_both(_, List, _)
    ),
    (
        List = [],
        IsEmpty = yes
    ;
        List = [_ | _],
        IsEmpty = no
    ).

num_clauses_in_clauses_rep(ClausesRep) = NumClauses :-
    (
        ClausesRep = cr_rev(NumClauses, _)
    ;
        ClausesRep = cr_forw(NumClauses, _)
    ;
        ClausesRep = cr_both(NumClauses, _, _)
    ).

get_clause_list_any_order(ClausesRep, Clauses) :-
    (
        ClausesRep = cr_rev(_, Clauses)
    ;
        ClausesRep = cr_forw(_, Clauses)
    ;
        ClausesRep = cr_both(_, _, Clauses)
    ).

get_clause_list(ClausesRep, Clauses) :-
    (
        ClausesRep = cr_rev(_, RevClauses),
        list.reverse(RevClauses, Clauses)
    ;
        ClausesRep = cr_forw(_, Clauses)
    ;
        ClausesRep = cr_both(_, _, Clauses)
    ).

set_clause_list(Clauses, cr_forw(list.length(Clauses), Clauses)).

clauses_info_clauses(Clauses, ItemNumbers, !CI) :-
    ClausesRep = !.CI ^ cli_rep,
    ItemNumbers = !.CI ^ cli_item_numbers,
    (
        ClausesRep = cr_rev(NumClauses, RevClauses),
        list.reverse(RevClauses, Clauses),
        !CI ^ cli_rep := cr_both(NumClauses, RevClauses, Clauses)
    ;
        ClausesRep = cr_forw(_, Clauses)
    ;
        ClausesRep = cr_both(_, _, Clauses)
    ).

add_clause(Clause, !ClausesRep) :-
    % We keep the clause list in reverse order, to make it possible
    % to add other clauses without quadratic behavior.
    (
        !.ClausesRep = cr_rev(NumClauses0, RevClauses0),
        NumClauses = NumClauses0 + 1,
        RevClauses = [Clause | RevClauses0],
        !:ClausesRep = cr_rev(NumClauses, RevClauses)
    ;
        !.ClausesRep = cr_forw(NumClauses0, Clauses0),
        NumClauses = NumClauses0 + 1,
        list.reverse(Clauses0, RevClauses0),
        RevClauses = [Clause | RevClauses0],
        !:ClausesRep = cr_rev(NumClauses, RevClauses)
    ;
        !.ClausesRep = cr_both(NumClauses0, RevClauses0, _),
        NumClauses = NumClauses0 + 1,
        RevClauses = [Clause | RevClauses0],
        !:ClausesRep = cr_rev(NumClauses, RevClauses)
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "hlds_clauses.m".

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_clauses.
%-----------------------------------------------------------------------------%
