%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2007, 2009-2012 The University of Melbourne.
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
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.vartypes.

:- import_module bool.
:- import_module list.
:- import_module term.

%-----------------------------------------------------------------------------%

:- type maybe_foreign_lang_clauses
    --->    no_foreign_lang_clauses
    ;       some_foreign_lang_clauses.

:- type maybe_clause_syntax_errors
    --->    no_clause_syntax_errors
    ;       some_clause_syntax_errors.

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

                % Map from variable name to type variable for the type
                % variables occurring in the argument types. This is used
                % to process explicit qualifications.
                cli_tvar_name_map           :: tvar_name_map,

                % This partial map holds the types specified by any explicit
                % type qualifiers in the clauses.
                cli_explicit_vartypes       :: vartypes,

                % This map contains the types of all the variables, as inferred
                % by typecheck.m.
                cli_vartypes                :: vartypes,

                % The head variables.
                cli_headvars                :: proc_arg_vector(prog_var),

                % The clauses themselves (some may be pragma foreign_procs).
                cli_rep                     :: clauses_rep,

                % Information about where the clauses came fro.
                cli_item_numbers            :: clause_item_numbers,

                % This field is computed by polymorphism.m.
                cli_rtti_varmaps            :: rtti_varmaps,

                % Does this predicate/function have foreign language clauses?
                cli_have_foreign_clauses    :: maybe_foreign_lang_clauses,

                % Did this predicate/function have clauses with syntax errors
                % in their bodies (so we could know, despite the error, that
                % the clause was for them)?
                cli_had_syntax_errors       :: maybe_clause_syntax_errors
        ).

:- pred clauses_info_init(pred_or_func::in, pred_form_arity::in,
    clause_item_numbers::in, clauses_info::out) is det.

:- pred clauses_info_init_for_assertion(prog_vars::in, clauses_info::out)
    is det.

:- pred clauses_info_get_varset(clauses_info::in, prog_varset::out) is det.
:- pred clauses_info_get_tvar_name_map(clauses_info::in, tvar_name_map::out)
    is det.
:- pred clauses_info_get_explicit_vartypes(clauses_info::in, vartypes::out)
    is det.
:- pred clauses_info_get_vartypes(clauses_info::in, vartypes::out) is det.
:- pred clauses_info_get_headvars(clauses_info::in,
    proc_arg_vector(prog_var)::out) is det.
:- pred clauses_info_get_clauses_rep(clauses_info::in, clauses_rep::out,
    clause_item_numbers::out) is det.
:- pred clauses_info_get_rtti_varmaps(clauses_info::in, rtti_varmaps::out)
    is det.
:- pred clauses_info_get_have_foreign_clauses(clauses_info::in,
    maybe_foreign_lang_clauses::out) is det.
:- pred clauses_info_get_had_syntax_errors(clauses_info::in,
    maybe_clause_syntax_errors::out) is det.

:- pred clauses_info_set_varset(prog_varset::in,
    clauses_info::in, clauses_info::out) is det.
:- pred clauses_info_set_tvar_name_map(tvar_name_map::in,
    clauses_info::in, clauses_info::out) is det.
:- pred clauses_info_set_explicit_vartypes(vartypes::in,
    clauses_info::in, clauses_info::out) is det.
:- pred clauses_info_set_vartypes(vartypes::in,
    clauses_info::in, clauses_info::out) is det.
:- pred clauses_info_set_headvars(proc_arg_vector(prog_var)::in,
    clauses_info::in, clauses_info::out) is det.
:- pred clauses_info_set_clauses_rep(clauses_rep::in, clause_item_numbers::in,
    clauses_info::in, clauses_info::out) is det.
:- pred clauses_info_set_rtti_varmaps(rtti_varmaps::in,
    clauses_info::in, clauses_info::out) is det.
:- pred clauses_info_set_have_foreign_clauses(maybe_foreign_lang_clauses::in,
    clauses_info::in, clauses_info::out) is det.
:- pred clauses_info_set_had_syntax_errors(maybe_clause_syntax_errors::in,
    clauses_info::in, clauses_info::out) is det.

    % Return the headvars as a list rather than as a proc_arg_vector.
    % New code should avoid using this, and should instead be written to
    % work with the arg_vector structure directly.
    %
:- pred clauses_info_get_headvar_list(clauses_info::in, list(prog_var)::out)
    is det.

%-----------------------------------------------------------------------------%

:- type clauses_rep.

:- func init_clauses_rep = clauses_rep.

    % Returns yes iff the given clauses_rep represents the empty list of
    % clauses.
    %
:- func clause_list_is_empty(clauses_rep) = bool.

    % Returns the number of clauses in the clauses list.
    %
:- func num_clauses_in_clauses_rep(clauses_rep) = int.

    % Get the list of clauses in the given clauses_rep in program order.
    %
    % There are three variants of this predicate. The reason why a simple
    % getter predicate is not good enough is the combination of these
    % circumstances.
    %
    % - We need to know the order of the clauses in the code.
    % - When we add a new clause, we need to add it at the end.
    %   This is best done by either keeping the clauses in reversed order
    %   (which is what we used to do) or keeping them in a cord (which is
    %   what we do now). Using a plain list in forward order would make
    %   require O(N^2) operations to add N clauses to the list.
    % - When users want to get the clause list, they want it in forward order.
    %   With either the reversed list or cord representations, this requires
    %   a representation change: re-reversing the list, or flattening the cord.
    %   In both cases, the cost of this is O(N) for N clauses.
    % - If the compiler generates a sequence of requests to get the clause
    %   list, we would perform this representation change over and over again.
    %
    % The first variant, get_clause_list, avoids the need for this repetition
    % by storing the result of the representation change back in the
    % clause_rep. Flattening an already-flat cord is an O(1) operation,
    % so repeatedly calling get_clause_list on the same clause_rep
    % is not a performance problem.
    %
    % The second variant, get_clause_list_for_replacement, is for use
    % in situations where the clause list is about to be replaced,
    % usually by a modified version of itself. In such cases, the cost
    % of future operations on *this* version of the clauses_rep is moot.
    %
    % The third variant is for places in the compiler that neither replace
    % the clause list nor update the clauses_rep, or its containing
    % clause_info. This is less than ideal from a performance viewpoint,
    % but it is ok for experimental features whose performance doesn't (yet)
    % matter. The name get_clause_list_maybe_repeated is there to remind
    % programmers who call it about the performance problem with repeated
    % representation changes, to act as incentive to switch to one of the
    % previous two versions.
    %
:- pred get_clause_list(list(clause)::out,
    clauses_rep::in, clauses_rep::out) is det.
:- pred get_clause_list_for_replacement(clauses_rep::in, list(clause)::out)
    is det.
:- pred get_clause_list_maybe_repeated(clauses_rep::in, list(clause)::out)
    is det.

:- pred get_first_clause(clauses_rep::in, clause::out) is semidet.

    % Return the list of clauses in program order, and if necessary update
    % the cache of this info in the clauses_info.
    %
:- pred clauses_info_clauses(list(clause)::out, clause_item_numbers::out,
    clauses_info::in, clauses_info::out) is det.

    % Set the list of clauses to the one given.
    %
:- pred set_clause_list(list(clause)::in, clauses_rep::out) is det.

    % Adds the given clause to the end of the clause list.
    %
:- pred add_clause(clause::in, clauses_rep::in, clauses_rep::out) is det.

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

    ;       selected_modes(list(proc_id))
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

    ;       unify_in_in_modes
    ;       unify_non_in_in_modes.
            % Given two terms such as
            %
            % X = f(XA, XB, XC, XD, XE)
            % Y = f(YA, YB, YC, YD, YE)
            %
            % where the B, C and D arguments are packed into the same word,
            % testing their equality in bulk by testing the equality of
            % the two words is obviously faster than extracting XB, XC, and XD
            % from one word, extracting YB, YC, and YD from the other word,
            % and comparing them pairwise. This is why we generate code to do
            % bulk comparisons in the automatically generated unify predicates
            % when possible.
            %
            % However, while bulk comparisons are a win for <in,in>
            % unifications, they work only for unifications in which
            % all of the bulk-compared arguments are ground, because they
            % have no means to e.g. copy the value of YC to the XC field in X
            % if the XC field started out as free.
            %
            % Therefore whenever we generate code for a unify predicate
            % that does one or more bulk comparisons, we mark that clause
            % as unify_in_in_modes (i.e. being valid only for unifications
            % in which both inputs are initially ground), and we also generate
            % another clause free of bulk comparisons, and mark it with
            % unify_non_in_in_modes, i.e. to be used for all other
            % unifications.
            %
            % If the unify predicate of a type_ctor *can* use bulk comparisons,
            % then we generate two clauses for it, one unify_in_in_modes
            % and one unify_non_in_in_modes. If it *cannot* use bulk
            % comparisons, we generate just one all_modes clause for it.

%-----------------------------------------------------------------------------%

    % We want to know whether the clauses of each predicate (which may include
    % pragma foreign_procs) are contiguous in the source code or not.
    %
    % To this end, we record the item numbers of
    %
    % - all the clauses of the predicate, and
    % - all the clauses and foreign_procs of the predicate.
    %
    % We store each set of numbers as a sorted list of item number regions,
    % with every item number between the lower and upper item numbers in
    % a region belonging to the predicate. Besides making it trivial to see
    % whether a predicate's clauses (or clauses and foreign_procs) are
    % contiguous or not, this compression also allows us to handle predicates
    % with large numbers of clauses in a small amount of memory,
    %
:- type clause_item_numbers.

:- type clause_item_number_region
    --->    clause_item_number_region(
                cnr_lower_item_number   ::  int,
                cnr_upper_item_number   ::  int,
                cnr_lower_item_context  ::  term.context,
                cnr_upper_item_context  ::  term.context
            ).

:- func init_clause_item_numbers_user = clause_item_numbers.
:- func init_clause_item_numbers_comp_gen = clause_item_numbers.

:- type clause_item_number_types
    --->    only_clauses
    ;       clauses_and_foreign_procs.

:- pred clause_item_number_regions(clause_item_numbers::in,
    clause_item_number_types::in, list(clause_item_number_region)::out) is det.

:- pred clauses_are_non_contiguous(clause_item_numbers::in,
    clause_item_number_types::in,
    clause_item_number_region::out, clause_item_number_region::out,
    list(clause_item_number_region)::out) is semidet.

:- type clause_item_number_type
    --->    item_is_clause
    ;       item_is_foreign_proc.

:- pred add_clause_item_number(item_seq_num::in, term.context::in,
    clause_item_number_type::in,
    clause_item_numbers::in, clause_item_numbers::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_util.

:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module require.
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

%-----------------------------------------------------------------------------%

clauses_info_init(PredOrFunc, PredFormArity, ItemNumbers, ClausesInfo) :-
    varset.init(VarSet0),
    PredFormArity = pred_form_arity(PredFormArityInt),
    make_n_fresh_vars("HeadVar__", PredFormArityInt, HeadVars,
        VarSet0, VarSet),
    init_vartypes(VarTypes),
    map.init(TVarNameMap),
    HeadVarVec = proc_arg_vector_init(PredOrFunc, HeadVars),
    set_clause_list([], ClausesRep),
    rtti_varmaps_init(RttiVarMaps),
    ClausesInfo = clauses_info(VarSet, TVarNameMap, VarTypes, VarTypes,
        HeadVarVec, ClausesRep, ItemNumbers, RttiVarMaps,
        no_foreign_lang_clauses, no_clause_syntax_errors).

clauses_info_init_for_assertion(HeadVars, ClausesInfo) :-
    varset.init(VarSet),
    init_vartypes(VarTypes),
    map.init(TVarNameMap),
    % Procedures introduced for assertions are always predicates, never
    % functions.
    HeadVarVec = proc_arg_vector_init(pf_predicate, HeadVars),
    set_clause_list([], ClausesRep),
    ItemNumbers = init_clause_item_numbers_comp_gen,
    rtti_varmaps_init(RttiVarMaps),
    ClausesInfo = clauses_info(VarSet, TVarNameMap, VarTypes, VarTypes,
        HeadVarVec, ClausesRep, ItemNumbers, RttiVarMaps,
        no_foreign_lang_clauses, no_clause_syntax_errors).

clauses_info_get_varset(CI, CI ^ cli_varset).
clauses_info_get_tvar_name_map(CI, CI ^ cli_tvar_name_map).
clauses_info_get_explicit_vartypes(CI, CI ^ cli_explicit_vartypes).
clauses_info_get_vartypes(CI, CI ^ cli_vartypes).
clauses_info_get_headvars(CI, CI ^ cli_headvars).
clauses_info_get_clauses_rep(CI, CI ^ cli_rep, CI ^ cli_item_numbers).
clauses_info_get_rtti_varmaps(CI, CI ^ cli_rtti_varmaps).
clauses_info_get_have_foreign_clauses(CI, CI ^ cli_have_foreign_clauses).
clauses_info_get_had_syntax_errors(CI, CI ^ cli_had_syntax_errors).

clauses_info_set_varset(X, !CI) :-
    !CI ^ cli_varset := X.
clauses_info_set_tvar_name_map(X, !CI) :-
    !CI ^ cli_tvar_name_map := X.
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
clauses_info_set_have_foreign_clauses(X, !CI) :-
    !CI ^ cli_have_foreign_clauses := X.
clauses_info_set_had_syntax_errors(X, !CI) :-
    !CI ^ cli_had_syntax_errors := X.

%-----------------------------------------------------------------------------%

clauses_info_get_headvar_list(CI, HeadVarList) :-
    clauses_info_get_headvars(CI, HeadVars),
    HeadVarList = proc_arg_vector_to_list(HeadVars).

:- type clauses_rep
    --->    clauses_rep(
                cr_num_clauses      :: int,
                cr_clauses_cord     :: cord(clause)
            ).

init_clauses_rep = clauses_rep(0, cord.init).

clause_list_is_empty(ClausesRep) = IsEmpty :-
    ClausesRep = clauses_rep(_, ClausesCord),
    ( if cord.is_empty(ClausesCord) then
        IsEmpty = yes
    else
        IsEmpty = no
    ).

num_clauses_in_clauses_rep(ClausesRep) = NumClauses :-
    ClausesRep = clauses_rep(NumClauses, _).

get_clause_list(Clauses, ClausesRep0, ClausesRep) :-
    ClausesRep0 = clauses_rep(NumClauses, ClausesCord0),
    Clauses = cord.list(ClausesCord0),
    ClausesCord = cord.from_list(Clauses),
    ClausesRep = clauses_rep(NumClauses, ClausesCord).

get_clause_list_for_replacement(ClausesRep, Clauses) :-
    ClausesRep = clauses_rep(_NumClauses, ClausesCord),
    Clauses = cord.list(ClausesCord).

get_clause_list_maybe_repeated(ClausesRep, Clauses) :-
    ClausesRep = clauses_rep(_NumClauses, ClausesCord),
    Clauses = cord.list(ClausesCord).

get_first_clause(ClausesRep, FirstClause) :-
    ClausesRep = clauses_rep(_NumClauses, ClausesCord),
    cord.get_first(ClausesCord, FirstClause).

clauses_info_clauses(Clauses, ItemNumbers, !CI) :-
    ItemNumbers = !.CI ^ cli_item_numbers,
    ClausesRep0 = !.CI ^ cli_rep,
    get_clause_list(Clauses, ClausesRep0, ClausesRep),
    !CI ^ cli_rep := ClausesRep.

set_clause_list(Clauses, ClausesRep) :-
    ClausesRep = clauses_rep(list.length(Clauses), cord.from_list(Clauses)).

add_clause(Clause, !ClausesRep) :-
    !.ClausesRep = clauses_rep(NumClauses0, ClausesCord0),
    NumClauses = NumClauses0 + 1,
    ClausesCord = cord.snoc(ClausesCord0, Clause),
    !:ClausesRep = clauses_rep(NumClauses, ClausesCord).

%-----------------------------------------------------------------------------%

init_clause_item_numbers_user = user_clauses([], []).
init_clause_item_numbers_comp_gen = comp_gen_clauses.

clause_item_number_regions(ClauseItemNumbers, Type, Regions) :-
    (
        ClauseItemNumbers = comp_gen_clauses,
        Regions = []
    ;
        ClauseItemNumbers = user_clauses(MercuryRegions, BothRegions),
        (
            Type = only_clauses,
            Regions = MercuryRegions
        ;
            Type = clauses_and_foreign_procs,
            Regions = BothRegions
        )
    ).

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

add_clause_item_number(SeqNum, Context, Type, !ClauseItemNumbers) :-
    (
        SeqNum = item_no_seq_num,
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
        SeqNum = item_seq_num(ItemNumber),
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
        ( if ItemNum < LowerNum0 - 1 then
            NewRegion = clause_item_number_region(ItemNum, ItemNum,
                Context, Context),
            !:Regions = [NewRegion, FirstRegion0 | LaterRegions0]
        else if ItemNum = LowerNum0 - 1 then
            FirstRegion = clause_item_number_region(ItemNum, UpperNum0,
                Context, UpperContext0),
            !:Regions = [FirstRegion | LaterRegions0]
        else if ItemNum =< UpperNum0 then
            unexpected($pred, "duplicate item number")
        else if ItemNum = UpperNum0 + 1 then
            FirstRegion1 = clause_item_number_region(LowerNum0, ItemNum,
                LowerContext0, Context),
            maybe_merge_clause_item_number_regions(FirstRegion1, LaterRegions0,
                !:Regions)
        else
            add_clause_item_number_regions(ItemNum, Context,
                LaterRegions0, LaterRegions1),
            maybe_merge_clause_item_number_regions(FirstRegion0, LaterRegions1,
                !:Regions)
        )
    ).

    % Merge Region0 with the first region of Regions12 if need be.
    %
:- pred maybe_merge_clause_item_number_regions(
    clause_item_number_region::in, list(clause_item_number_region)::in,
    list(clause_item_number_region)::out) is det.

maybe_merge_clause_item_number_regions(Region0, Regions12, Regions) :-
    (
        Regions12 = [],
        Regions = [Region0]
    ;
        Regions12 = [Region1 | Regions2],
        Region0 = clause_item_number_region(
            LowerNum0, UpperNum0, LowerContext0, _UpperContext0),
        Region1 = clause_item_number_region(
            LowerNum1, UpperNum1, _LowerContext1, UpperContext1),
        ( if UpperNum0 + 1 = LowerNum1 then
            Region01 = clause_item_number_region(LowerNum0, UpperNum1,
                LowerContext0, UpperContext1),
            Regions = [Region01 | Regions2]
        else
            Regions = [Region0, Region1 | Regions2]
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_clauses.
%-----------------------------------------------------------------------------%
