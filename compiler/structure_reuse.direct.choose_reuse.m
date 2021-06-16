%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: structure_reuse.direct.choose_reuse.m.
% Main authors: nancy.
%
% Given a dead cell table listing the deconstructions that may leave garbage
% (dead cells), we compute the concrete assignments of which constructions can
% profit from these dead cells. Obviously, we want to find those assignments
% which result in the 'best' form of memory reuse possible for the given goals.
%
% Hence, the assignment problem is translated into a mapping problem (inspired
% from Debray's paper: "On copy avoidance in single assignment languages", and
% restricted to reuse of dead cells by at most one new cell).
%
% When assigning constructions to dead deconstructions, a table is first
% computed. For each dead cell, a value is computed that reflects the gain
% a reuse might bring, and the list of constructions involved with reusing it.
% The cell with highest value is selected first, the according constructions
% are annotated, and the table is recomputed. This process is repeated until
% no reusable dead deconstructions are left.
%
% The value of a dead cell (a specific deconstruction) is computed taking
% into account the call graph which can be simplified to take only into account
% construction-unifications, conjunctions, and disjunctions.
% The source of the graph is the deconstruction, the leaves are
% either constructions, or empty. The branches are either conjunctions
% or disjunctions.
% The value of the dead cell is then computed as follows:
%   - value of a conjunction = maximum of the values of each of the
%       conjunct branches.
%       Intuitively: if a dead deconstruction is followed by
%       two constructions which might reuse the dead cell: pick
%       the one which allows the most potential gain.
%   - value of a disjunction = average of the value of each of the
%       disjunct branches.
%       Intuitively: if a dead deconstruction is followed by
%       a disjunction with 2 disjuncts. If reuse is only possible
%       in one of the branches, allowing this reuse means that
%       a priori reuse will occur in only 50% of the cases.
%       The value of the disjunct should take this into account.
%       Without precise notion of which branches are executed
%       more often, taking the simple average of the values is
%       a good approximation.
%   - value of a construction = a value that takes into account
%       the cost of constructing a new cell and compares it
%       to the cost of updating a dead cell. If the arities
%       between the dead and new cell differ, a penalty cost
%       is added (approximated as the gain one would have had if
%       the unusable words would have been reused too).
%       Weights are used to estimate all of these costs and are
%       hard-coded. I don't think there is any need in making
%       these values an option.
%
% Once the table is computed, the cell with highest value is selected.
% To cut the decision between different dead cells with the same
% value, we select the dead cell that has the least number of
% opportunities to be reused.
%
% e.g.
%   X can be reused by 5 different constructions,
%       but reaches its highest value for a construction C1
%       (value 10).
%   Y can be reused by only one construction, also C1 (value 10).
%
% First selecting X (and reusing it with construction C1) would
% jeopardize the reuse of Y and leaves us with only one cell reused.
% If, on the contrary, one would select Y first, chances are that
% after recomputing the table, X can still be reused by other
% constructions, hence possibly 2 cells reused.
% Even if Y would be of smaller value, selecting Y first would still
% be more interesting. Hence, instead of selecting the cell
% with highest value, we select the cell with highest
% value/degree ratio, degree being the number of constructions at which
% the cell could potentially be reused.
%
% Note that cells being deconstructed in the different branches of a
% disjunction can now also be reused after the disjunction.
%
% e.g.:
%   (
%       ..., X => f(... ), ...      % X dies
%   ;
%       ..., X => g(... ), ...      % X dies
%   ),
%   Y <= f(... ), ...           % Y can reuse X
%
% In this example, it is allowed to reuse X for Y. And it will also be
% discovered by the analysis.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_reuse.direct.choose_reuse.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.
:- import_module transform_hlds.ctgc.structure_reuse.domain.

%---------------------------------------------------------------------------%

:- pred determine_reuse(module_info::in, proc_info::in, dead_cell_table::in,
    hlds_goal::in, hlds_goal::out, reuse_as::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.vartypes.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module float.
:- import_module int.
:- import_module maybe.
:- import_module multi_map.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

determine_reuse(ModuleInfo, ProcInfo, DeadCellTable, !Goal, ReuseAs) :-
    % Check for local reuse:
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_reuse_strategy(Globals, Strategy),
    BackGroundInfo = background_info_init(Strategy, ModuleInfo, ProcInfo),
    choose_reuse_in_goal(BackGroundInfo, DeadCellTable, RemainingDeadCellTable,
        !Goal, reuse_as_init, ReuseAs),

    globals.lookup_bool_option(Globals, structure_reuse_free_cells, FreeCells),
    (
        FreeCells = yes,
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        check_for_cell_caching(VeryVerbose, RemainingDeadCellTable, !Goal)
    ;
        FreeCells = no
    ).

%---------------------------------------------------------------------------%

    % A type to collect all the background information needed to process
    % each individual goal.
    %
:- type background_info
    --->    background(
                back_strategy       :: reuse_strategy,
                back_module_info    :: module_info,
                back_proc_info      :: proc_info,
                back_vartypes       :: vartypes
            ).

:- func background_info_init(reuse_strategy, module_info, proc_info) =
    background_info.

background_info_init(Strategy, ModuleInfo, ProcInfo) = Background :-
    proc_info_get_vartypes(ProcInfo, VarTypes),
    Background = background(Strategy, ModuleInfo, ProcInfo, VarTypes).

%---------------------------------------------------------------------------%
% Some types and predicates for the administration of the deconstructions,
% constructions and the 'matches' we want to derive from them.
%

% XXX With the --use-atomic-cells option, the compiler generates code
% that uses GC_MALLOC_ATOMIC to allocate memory for heap cells that contain
% no pointers to GCable memory. If we later reuse such a cell and put a pointer
% to GCable memory into it, the Boehm collector will not see that pointer,
% which may lead to the heap cell being pointed to being reclaimed prematurely,
% a bug that will probably be very hard to find.
%
% To avoid this situation, we should
%
% (1) extend deconstruction_spec with a field of type may_use_atomic_alloc,
%     indicating whether the potentially reused cell may be atomic or not, and
% (2) ensure that we reuse atomically-created cells only for constructions
%     in which all arguments can be put into atomic cells.
%
% These will require applying type_may_use_atomic_alloc to the arguments of
% both the reused deconstruction unifications and the reusing construction
% unifications.
%
% However, a fix to this problem can wait until structure reuse starts to be
% used in earnest. Until then, Nancy can simply avoid turning on
% --use-atomic-cells.

    % Details of a deconstruction yielding garbage.
    %
:- type deconstruction_spec
    --->    decon(
                decon_var       :: dead_var,
                decon_pp        :: program_point,
                decon_cons_id   :: cons_id,
                decon_args      :: prog_vars,
                decon_conds     :: reuse_as
            ).

    % Details of a construction possibly reusing some specific garbage cells
    % generated at a deconstruction.
    %
:- type construction_spec
    --->    con(
                con_pp      :: program_point,
                con_reuse   :: reuse_type
            ).

    % The reuse-type is a basic identification of whether the cons-ids involved
    % in the reuse are the same, what the arities of the old and new cells are,
    % and which arguments do not need to be updated.
    %
:- type reuse_type
    --->    reuse_type(
                same_cons       :: bool,

                % States whether the corresponding argument in the list of
                % arguments of the reused cons needs to be updated when
                % reused or not.
                % Note that list.length(reuse_fields) is the arity of the
                % reused term.
                reuse_fields    :: list(needs_update),

                % A metric measuring the value of the reuse. A high value
                % should represent a 'good' reuse (yielding possibly good
                % results on the general memory behaviour of the procedure)
                % compared to a reuse with a lower value.
                reuse_value     :: float
            ).

        % A match is a description of a list of deconstructions and a list of
        % constructions. The deconstructions and constructions can all be coded
        % into reuses, as they are such that at run-time at most one
        % deconstruction yielding the dead cell will occur on the same
        % execution path as a construction that
        % can reuse that cell.
        % This means that all the deconstructions can be coded as
        % deconstructions yielding dead cell, and all the constructions can be
        % coded as constructions reusing the cell that becomes available
        % through one of the deconstructions.
        %
:- type match
    --->    match(
                decon_specs     :: list(deconstruction_spec),
                con_specs       :: list(construction_spec),
                match_value     :: float,
                match_degree    :: int
            ).

:- type match_table == multi_map(dead_var, match).

    % Initialise a deconstruction_spec.
    %
:- func deconstruction_spec_init(dead_var, program_point, cons_id,
    prog_vars, reuse_as) = deconstruction_spec.

deconstruction_spec_init(Var, PP, ConsId, Args, Cond)
    = decon(Var, PP, ConsId, Args, Cond).

    % Pre-condition: the set of variables to which the list of deconstructions
    % relate (the dead vars) should be a singleton set. In other words,
    % all of the deconstructions in a match relate to one and the same
    % dying variable.
    %
:- func match_init(list(deconstruction_spec)) = match.

match_init(DS) = match(DS, [], 0.0, 0).

    % Verify that a match is still 'empty', i.e. has no constructions that can
    % reuse the dead cell available from the deconstructions listed in the
    % match.
    %
:- pred match_has_no_construction_candidates(match::in) is semidet.
:- pragma consider_used(pred(match_has_no_construction_candidates/1)).

match_has_no_construction_candidates(match(_, [], _, _)).

    % Determine the variable whose term is involved in the reuse if the
    % match would be implemented.
    %
:- func match_get_dead_var(match) = dead_var.

match_get_dead_var(Match) = Var :-
    GetVar = (func(D) = D ^ decon_var),
    DeadVars0 = list.map(GetVar, Match ^ decon_specs),
    DeadVars  = list.remove_dups(DeadVars0),
    (
        DeadVars = [Var | Rest],
        (
            Rest = []
        ;
            Rest = [_ | _],
            unexpected($pred, "too many dead vars")
        )
    ;
        DeadVars = [],
        unexpected($pred, "empty list of vars")
    ).

    % Get the list of cons_ids that the dead variable may have when it
    % will be reused.
    %
:- func match_get_dead_cons_ids(match) = list(cons_id).

match_get_dead_cons_ids(Match) = ConsIds :-
    GetConsId = (func(D) = D ^ decon_cons_id),
    ConsIds = list.map(GetConsId, Match ^ decon_specs).

    % Determine the reuse condition of the match.
    %
:- func match_get_condition(background_info, match) = reuse_as.

match_get_condition(Background, Match) = Condition :-
    GetCond = (func(D) = D ^ decon_conds),
    Conditions = list.map(GetCond, Match ^ decon_specs),
    (
        Conditions = [First | Rest],
        list.foldl(
            reuse_as_least_upper_bound(Background ^ back_module_info,
                Background ^ back_proc_info),
            Rest, First, Condition)
    ;
        Conditions = [],
        unexpected($pred, "no reuse conditions")
    ).

    % Add a construction as a potential place for reusing the garbage
    % produced by any of the deconstructions listed in the match.
    % This changes the value of the match.
    %
:- pred match_add_construction(construction_spec::in,
    match::in, match::out) is det.

match_add_construction(ConSpec, Match0, Match) :-
    Match0 = match(DeconSpecs0, ConSpecs0, Value0, Degree0),
    ConSpecs = [ConSpec | ConSpecs0],
    Degree = Degree0 + 1,
    FDegree0 = float(Degree0),
    FDegree = float(Degree),
    Value = (Value0 * FDegree0 + ConSpec ^ con_reuse ^ reuse_value) / FDegree,
    Match = match(DeconSpecs0, ConSpecs, Value, Degree).

%---------------------------------------------------------------------------%
%
% Manipulating the values of matches...
%

:- func highest_match_degree_ratio(match_table) = match.

highest_match_degree_ratio(MatchTable) = Match :-
    multi_map.values(MatchTable, Matches),
    list.sort(reverse_compare_matches_value_degree, Matches, Sorted),
    (
        Sorted = [Match | _]
    ;
        Sorted = [],
        unexpected($pred, "empty multi_map")
    ).

:- pred compare_matches_value_degree(match::in, match::in,
    comparison_result::out) is det.

compare_matches_value_degree(MatchA, MatchB, Result) :-
    VA = match_value_degree(MatchA),
    VB = match_value_degree(MatchB),
    compare(Result, VA, VB).

:- pred reverse_compare_matches_value_degree(match::in, match::in,
    comparison_result::out) is det.

reverse_compare_matches_value_degree(MatchA, MatchB, Result) :-
    compare_matches_value_degree(MatchB, MatchA, Result).

:- func match_value_degree(match) = float.

match_value_degree(Match) =
    ( if Match ^ match_value \= 0.0 then
        Match ^ match_value / float(Match ^ match_degree)
    else
        0.0
    ).

:- pred compare_matches_value(match::in, match::in,
        comparison_result::out) is det.

compare_matches_value(Match1, Match2, Result) :-
    V1 = Match1 ^ match_value,
    V2 = Match2 ^ match_value,
    compare(Result, V1, V2).

:- pred reverse_compare_matches_value(match::in, match::in,
    comparison_result::out) is det.

reverse_compare_matches_value(Match1, Match2, Result) :-
    compare_matches_value(Match2, Match1, Result).

:- pred match_allows_reuse(match::in) is semidet.

match_allows_reuse(Match) :-
    Constructions = Match ^ con_specs,
    Value = Match ^ match_value,
    Constructions = [_|_],
    Value > 0.0.

:- pred highest_match_in_list(list(match)::in, match::in, match::out) is det.

highest_match_in_list(Matches, Match0, Match) :-
    list.sort(reverse_compare_matches_value, [Match0 | Matches], Sorted),
    (
        Sorted = [Match | _]
    ;
        Sorted = [],
        unexpected($pred, "empty list of matches")
    ).

    % Given a list of matches concerning the same (list of) deconstruction,
    % compute the average reuse value of that deconstruction. This means
    % merging all the constructions together into one list, and using the
    % average value of the reuses of each of the matches. The final degree
    % of the match is set to the sum of all degrees.
    %
:- pred average_match(list(match)::in, match::out) is det.

average_match(List, AverageMatch) :-
    (
        List = [First | Rest],
        list.length(List, Length),
        P = (pred(M::in, !.Acc::in, !:Acc::out) is det :-
            DeconSpecs = !.Acc ^ decon_specs,
            ConSpecs = append(!.Acc ^ con_specs, M ^ con_specs),
            Val = !.Acc ^ match_value + M ^ match_value,
            Deg = !.Acc ^ match_degree + M ^ match_degree,
            !:Acc = match(DeconSpecs, ConSpecs, Val, Deg)
        ),
        list.foldl(P, Rest, First, Match0),
        AverageMatch = (Match0 ^ match_value :=
            (Match0 ^ match_value / float(Length)))
    ;
        List = [],
        unexpected($pred, "empty list")
    ).

%---------------------------------------------------------------------------%
%
% Process a single goal:
%
%   * determine a match table
%   * find the best match
%   * annotate the goal with the reuse described by that match
%   * and reprocess the goal until no matches are found.

:- pred choose_reuse_in_goal(background_info::in, dead_cell_table::in,
    dead_cell_table::out, hlds_goal::in, hlds_goal::out, reuse_as::in,
    reuse_as::out) is det.

choose_reuse_in_goal(Background, !DeadCellTable, !Goal, !ReuseAs) :-
    ModuleInfo = Background ^ back_module_info,
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),

    % Compute a match table.
    compute_match_table(Background, !.DeadCellTable, !.Goal, MatchTable),

    % As long as the match table is not empty, pick out the match with the
    % highest value, annotate the goal accordingly, and repeat the procedure.
    % If the match table is empty, the work is finished.

    ( if multi_map.is_empty(MatchTable) then
        true
    else
        % Select the deconstructions-constructions with highest value.

        Match = highest_match_degree_ratio(MatchTable),
        (
            Match ^ con_specs = []
        ;
            Match ^ con_specs = [_ | _],

            % Maybe dump all the matches recorded in the table, highlight the
            % match with the highest value.
            trace [io(!IO)] (
                (
                    VeryVerbose = no
                ;
                    VeryVerbose = yes,
                    io.stderr_stream(StdErr, !IO),
                    io.write_string(StdErr, "% Reuse results: \n", !IO),
                    dump_match_table(StdErr, MatchTable, Match, !IO)
                )
            ),

            OldGoal = !.Goal,
            OldReuseAs = !.ReuseAs,

            % Realise the reuses by explicitly annotating the procedure goal.
            annotate_reuses_in_goal(Background, Match, !Goal),

            % Remove the deconstructions from the available map of dead cells.
            remove_deconstructions_from_dead_cell_table(Match, !DeadCellTable),

            % Add the conditions involved in the reuses to the existing
            % conditions.
            ProcInfo   = Background ^ back_proc_info,
            reuse_as_least_upper_bound(ModuleInfo, ProcInfo,
                match_get_condition(Background, Match), !ReuseAs),

            % If there would be too many reuse conditions on this procedure
            % by taking the reuse opportunity, just drop it.
            globals.lookup_int_option(Globals, structure_reuse_max_conditions,
                MaxConditions),
            ( if reuse_as_count_conditions(!.ReuseAs) > MaxConditions then
                !:Goal = OldGoal,
                !:ReuseAs = OldReuseAs
            else
                true
            ),

            % Process the goal for further reuse-matches.
            disable_warning [suspicious_recursion] (
                choose_reuse_in_goal(Background, !DeadCellTable,
                    !Goal, !ReuseAs)
            )
        )
    ).

:- pred remove_deconstructions_from_dead_cell_table(match::in,
    dead_cell_table::in, dead_cell_table::out) is det.

remove_deconstructions_from_dead_cell_table(Match, !DeadCellTable) :-
    DeconSpecs = Match ^ decon_specs,
    list.foldl(remove_deconstruction_from_dead_cell_table, DeconSpecs,
        !DeadCellTable).

:- pred remove_deconstruction_from_dead_cell_table(deconstruction_spec::in,
    dead_cell_table::in, dead_cell_table::out) is det.

remove_deconstruction_from_dead_cell_table(DeconSpec, !DeadCellTable) :-
    dead_cell_table_remove(DeconSpec ^ decon_pp, !DeadCellTable).

%---------------------------------------------------------------------------%
%
% Compute the match table for a given goal
%
% The table is computed by traversing the whole goal. For each
% deconstruction encountered that is also listed in the dead_cell_table,
% compute a match.
%

:- pred compute_match_table(background_info::in, dead_cell_table::in,
    hlds_goal::in, match_table::out) is det.

compute_match_table(Background, DeadCellTable, Goal, MatchTable) :-
    ContinuationGoals = [],
    compute_match_table_with_continuation(Background, DeadCellTable,
        Goal, ContinuationGoals, multi_map.init, MatchTable).

:- pred compute_match_table_goal_list(background_info::in, dead_cell_table::in,
    hlds_goals::in, match_table::in, match_table::out) is det.

compute_match_table_goal_list(Background, DeadCellTable, Goals, !Table) :-
    (
        Goals = []
    ;
        Goals = [CurrentGoal | Cont],
        compute_match_table_with_continuation(Background, DeadCellTable,
            CurrentGoal, Cont, !Table)
    ).

:- pred compute_match_table_with_continuation(background_info::in,
    dead_cell_table::in, hlds_goal::in, hlds_goals::in,
    match_table::in, match_table::out) is det.

compute_match_table_with_continuation(Background, DeadCellTable,
        CurrentGoal, Cont, !Table) :-
    CurrentGoal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = unify(_, _, _, Unification, _),
        ( if Unification = deconstruct(Var, ConsId, Args, _, _, _) then

            ProgramPoint = program_point_init(GoalInfo),
            ( if
                Condition = dead_cell_table_search(ProgramPoint, DeadCellTable)
            then
                ReuseAs = reuse_as_init_with_one_condition(Condition),
                DeconstructionSpec = deconstruction_spec_init(Var,
                    ProgramPoint, ConsId, Args, ReuseAs),
                Match0 = match_init([DeconstructionSpec]),
                find_best_match_in_conjunction(Background, Cont,
                    Match0, Match),
                multi_map.set(Var, Match, !Table)
            else
                true
            )
        else
            true
        ),
        compute_match_table_goal_list(Background, DeadCellTable, Cont, !Table)
    ;
        GoalExpr = plain_call(_, _, _, _, _, _),
        compute_match_table_goal_list(Background, DeadCellTable,
            Cont, !Table)
    ;
        GoalExpr = generic_call( _, _, _, _, _),
        compute_match_table_goal_list(Background, DeadCellTable,
            Cont, !Table)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
        compute_match_table_goal_list(Background, DeadCellTable,
            Cont, !Table)
    ;
        GoalExpr = conj(_, Goals),
        list.append(Goals, Cont, NewCont),
        compute_match_table_goal_list(Background, DeadCellTable,
            NewCont, !Table)
    ;
        GoalExpr = disj(Goals),
        compute_match_table_in_disjunction(Background, DeadCellTable, Goals,
            Cont, !Table),
        compute_match_table_goal_list(Background, DeadCellTable, Cont, !Table)
    ;
        GoalExpr = switch(_, _, Cases),
        Goals = list.map((func(C) = C ^ case_goal), Cases),
        compute_match_table_in_disjunction(Background, DeadCellTable,
            Goals, Cont, !Table),
        compute_match_table_goal_list(Background, DeadCellTable, Cont, !Table)
    ;
        GoalExpr = negation(Goal),
        % If Goal contains deconstructions, they should not be reused within
        % Cont.
        compute_match_table_with_continuation(Background, DeadCellTable,
            Goal, [], !Table),
        compute_match_table_goal_list(Background, DeadCellTable, Cont,
            !Table)
    ;
        GoalExpr = scope(_, Goal),
        % XXX We should special-case the handling of from_ground_term_construct
        % scopes. Since they construct ground terms statically, there is no
        % uniqueness we can exploit,
        compute_match_table_with_continuation(Background, DeadCellTable,
            Goal, Cont, !Table)
    ;
        GoalExpr = if_then_else(_, CondGoal, ThenGoal, ElseGoal),
        multi_map.init(Table0),
        compute_match_table_with_continuation(Background, DeadCellTable,
            CondGoal, [ThenGoal], Table0, TableThen),
        compute_match_table_with_continuation(Background, DeadCellTable,
            ElseGoal, [], Table0, TableElse),
        multi_map.merge(TableThen, !Table),
        multi_map.merge(TableElse, !Table),
        process_possible_common_dead_vars(Background, Cont,
            [TableThen, TableElse], CommonDeadVarsTables),
        list.foldl(multi_map.merge, CommonDeadVarsTables, !Table),
        compute_match_table_goal_list(Background, DeadCellTable, Cont,
            !Table)
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

:- pred compute_match_table_in_disjs(background_info::in, dead_cell_table::in,
    hlds_goals::in, list(match_table)::out) is det.

compute_match_table_in_disjs(Background, DeadCellTable, Branches, Tables) :-
    list.map(compute_match_table(Background, DeadCellTable),
        Branches, Tables).

:- pred compute_match_table_in_disjunction(background_info::in,
    dead_cell_table::in, hlds_goals::in, hlds_goals::in,
    match_table::in, match_table::out) is det.

compute_match_table_in_disjunction(Background, DeadCellTable, DisjGoals, Cont,
        !Table) :-
    % Compute a match table for each of the branches of the disjunction.
    % Each of these tables will contain information about local reuses
    % w.r.t. the disjunction, i.e. a data structure is reused within the
    % same branch in which it dies.
    compute_match_table_in_disjs(Background, DeadCellTable, DisjGoals,
        DisjTables),
    list.foldl(multi_map.merge, DisjTables, !Table),

    % It is possible that each of the branches of the disjunctions
    % deconstructs the same (non local) dead variable. In such a case, we
    % need to check if that dead variable can be reused outside of the
    % disjunction.
    process_possible_common_dead_vars(Background, Cont, DisjTables,
        CommonDeadVarsDisjTables),
    list.foldl(multi_map.merge, CommonDeadVarsDisjTables, !Table).

:- pred process_possible_common_dead_vars(background_info::in, hlds_goals::in,
    list(match_table)::in, list(match_table)::out) is det.

process_possible_common_dead_vars(Background, Cont, DisjTables,
        ExtraTables) :-
    CommonDeadVars = common_vars(DisjTables),
    (
        CommonDeadVars = [_ | _],
        list.filter_map(process_common_var(Background, Cont, DisjTables),
            CommonDeadVars, ExtraTables)
    ;
        CommonDeadVars = [],
        ExtraTables = []
    ).

:- func common_vars(list(match_table)) = dead_vars.

common_vars(Tables) = CommonVars :-
    (
        Tables = [First | RestTables],
        CommonVars = list.foldl(common_var_with_list, RestTables,
            map.keys(First))
    ;
        Tables = [],
        CommonVars = []
    ).

:- func common_var_with_list(match_table, prog_vars) = dead_vars.

common_var_with_list(Table, List0) = List :-
    map.keys(Table, Keys),
    Set = set.intersect(list_to_set(List0), list_to_set(Keys)),
    List = set.to_sorted_list(Set).

:- pred process_common_var(background_info::in, hlds_goals::in,
    list(match_table)::in, dead_var::in, match_table::out) is semidet.

process_common_var(Background, Cont, DisjTables, CommonDeadVar, Table) :-
    Match0 = match_init(deconstruction_specs(CommonDeadVar, DisjTables)),
    find_best_match_in_conjunction(Background, Cont, Match0, Match),
    match_allows_reuse(Match), % can fail
    multi_map.init(Table0),
    multi_map.det_insert(CommonDeadVar, Match, Table0, Table).

:- func deconstruction_specs(prog_var, list(match_table)) =
    list(deconstruction_spec).

deconstruction_specs(DeadVar, Tables) = DeconstructionSpecs :-
    list.foldl(deconstruction_specs_2(DeadVar), Tables, [],
        DeconstructionSpecs).

:- pred deconstruction_specs_2(prog_var::in, match_table::in,
    list(deconstruction_spec)::in, list(deconstruction_spec)::out) is det.

deconstruction_specs_2(DeadVar, Table, !DeconstructionSpecs) :-
    multi_map.lookup(Table, DeadVar, Matches),
    NewSpecs = list.condense(list.map(match_get_decon_specs, Matches)),
    append(NewSpecs, !DeconstructionSpecs).

:- func match_get_decon_specs(match) = list(deconstruction_spec).

match_get_decon_specs(Match) = Match ^ decon_specs.

%---------------------------------------------------------------------------%
%
% Find construction unifications for dead cells, compute the values of the
% matches.
%

    % Compute the value of a dead cell with respect to its possible reuses in
    % a conjunction of goals. If reuse is possible, add the specification of
    % the construction where it can be reused to the list of constructions
    % recorded in the match.
    %
    % In a conjunction, a dead cell can only be reused in at most one of its
    % direct children. This means that for each child a new value is computed.
    % At the end of a conjunction, we immediately choose the reuse with the
    % highest value.
    %
    % XXX This may not be such a good idea, as the notion of "degree" is used
    % to decide between reuses with the same value later on, once the full
    % match_table is computed.
    %
    % XXX What is the thing with the degrees here?
    %
:- pred find_best_match_in_conjunction(background_info::in, hlds_goals::in,
    match::in, match::out) is det.

find_best_match_in_conjunction(Background, Goals, !Match) :-
    Match0 = !.Match,
    list.map(find_match_in_goal(Background, Match0), Goals, ExclusiveMatches),
    Degree = count_candidates(ExclusiveMatches),
    highest_match_in_list(ExclusiveMatches, !Match),
    !Match ^ match_degree := Degree.

    % Compute the matches for a dead cell in the context of a disjunction. For
    % each branch, a different match may be found.  At the end, these matches
    % are merged together into one single match, taking the average of match
    % values to be the value of the final match.  Each construction involved in
    % the reuses is counted as a possibility for reuse, hence is reflected in
    % the degree of the final match description.
    %
:- pred find_match_in_disjunction(background_info::in, hlds_goals::in,
    match::in, match::out) is det.

find_match_in_disjunction(Background, Branches, !Match) :-
    (
        Branches = []
    ;
        Branches = [_ | _],
        list.map(find_match_in_goal(Background, !.Match), Branches,
            BranchMatches),
        average_match(BranchMatches, !:Match)
    ).

:- pred find_match_in_goal(background_info::in, match::in, hlds_goal::in,
    match::out) is det.

find_match_in_goal(Background, Match0, Goal, Match) :-
    find_match_in_goal_2(Background, Goal, Match0, Match).

:- pred find_match_in_goal_2(background_info::in, hlds_goal::in,
    match::in, match::out) is det.

find_match_in_goal_2(Background, Goal, !Match) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = unify(_, _, _, Unification, _),
        (
            Unification = construct(Var, Cons, Args, _, _, _, _),
            lookup_var_type(Background ^ back_vartypes, Var, VarType),
            ( if
                top_cell_may_be_reusable(Background ^ back_module_info,
                    VarType),

                % Is the construction still looking for reuse-possibilities...
                empty_reuse_description(goal_info_get_reuse(GoalInfo))
            then
                % Is it possible for the construction to reuse the dead cell
                % specified by the match?
                verify_match(Background, Var, Cons, Args,
                    program_point_init(GoalInfo), !Match)
            else
                true
            )
        ;
            ( Unification = deconstruct(_, _, _, _, _, _)
            ; Unification = assign(_, _)
            ; Unification = simple_test(_, _)
            )
        ;
            Unification = complicated_unify(_, _, _),
            unexpected($pred, "complicated unify")
        )
    ;
        GoalExpr = plain_call(_, _, _, _, _, _)
    ;
        GoalExpr = generic_call( _, _, _, _, _)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        GoalExpr = conj(_, Goals),
        find_best_match_in_conjunction(Background, Goals, !Match)
    ;
        GoalExpr = disj(Goals),
        find_match_in_disjunction(Background, Goals, !Match)
    ;
        GoalExpr = switch(_, _, Cases),
        Goals = list.map((func(C) = C ^ case_goal), Cases),
        find_match_in_disjunction(Background, Goals, !Match)
    ;
        GoalExpr = if_then_else(_, CondGoal, ThenGoal, ElseGoal),
        Match0 = !.Match,
        find_best_match_in_conjunction(Background, [CondGoal, ThenGoal],
            Match0, MatchThen),
        find_match_in_goal_2(Background, ElseGoal, Match0, MatchElse),
        average_match([MatchThen, MatchElse], !:Match)
    ;
        GoalExpr = negation(_)
    ;
        GoalExpr = scope(_, ScopeGoal),
        % XXX We should special-case the handling of from_ground_term_construct
        % scopes. Since they construct ground terms statically, there is no
        % uniqueness we can exploit,
        find_match_in_goal_2(Background, ScopeGoal, !Match)
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- func count_candidates(list(match)) = int.

count_candidates(Matches) = list.foldl(add_degree, Matches, 0).

:- func add_degree(match, int) = int.

add_degree(Match, Degree0) = Degree0 + Match ^ match_degree.

:- pred empty_reuse_description(reuse_description::in) is semidet.

empty_reuse_description(no_reuse_info).

%---------------------------------------------------------------------------%
%
% Verify the value of a match for a given construction
%

% The value is computed using the following rule:
%
% Gain = (Alfa + Gamma) * ArityNewCell + Beta
%       - Gamma * (ArityNewCell - UptoDateFields)
%       - ( SameCons? Beta; 0)
%       - Alfa * (ArityOldCell - ArityNewCell)
%
% where
% * Alfa: cost of allocating one single memory cell on the heap;
% * Gamma: cost of setting the value of one single memory cell on the heap;
% * Beta: cost of setting the value of the cons_id field;

:- func alfa_value = int is det.

alfa_value = 5.

:- func gamma_value = int is det.

gamma_value = 1.

:- func beta_value = int is det.

beta_value = 1.

:- pred verify_match(background_info::in, prog_var::in, cons_id::in,
    prog_vars::in, program_point::in, match::in, match::out) is det.

verify_match(Background, NewVar, NewCons, NewArgs, PP, !Match) :-
    DeconSpecs = !.Match ^ decon_specs,
    ( if
        % The construction must be compatible with *all* deconstruction specs.
        % Otherwise we may try to reuse a cell which is only compatible through
        % one code path but not another.
        list.map(compute_reuse_type(Background, NewVar, NewCons, NewArgs),
            DeconSpecs, ReuseTypes),
        ReuseType = glb_reuse_types(ReuseTypes) % Can Fail.
    then
        ConSpec = con(PP, ReuseType),
        match_add_construction(ConSpec, !Match)
    else
        true
    ).

    % compute_reuse_type(Background, NewVar, NewCons, NewArgs,
    %   DeconstructionSpecification) = Cost (represented as a reuse_type).
    %
    % Compute a description (including its cost) of reusing the
    % specified deconstruction for the construction of the new var (NewVar),
    % with cons_id NewCons, and arguments NewArgs.
    %
    % The predicate fails if the construction is incompatible with the
    % deconstructed dead data structure.
    %
:- pred compute_reuse_type(background_info::in, prog_var::in, cons_id::in,
    prog_vars::in, deconstruction_spec::in, reuse_type::out) is semidet.

compute_reuse_type(Background, _NewVar, NewCons, NewCellArgs, DeconSpec,
        ReuseType) :-
    DeconSpec = decon(_DeadVar, _, DeadCons, DeadCellArgs, _),

    ModuleInfo = Background ^ back_module_info,

    ( if NewCons = DeadCons then
        SameCons = yes
    else
        SameCons = no,
        % XXX All the reuse code was written before packed and double word
        % arguments were introduced. For now only allow reuse of cells with
        % packed fields when the dead variable and the new variable have the
        % same constructor.
        cons_has_normal_fields(ModuleInfo, NewCons),
        cons_has_normal_fields(ModuleInfo, DeadCons)
    ),

    NewNumArgs = list.length(NewCellArgs),
    DeadNumArgs = list.length(DeadCellArgs),

    % Cells with arity zero can not reuse heap cells.
    NewNumArgs \= 0,

    % Include the space needed for secondary tags.
    has_secondary_tag(ModuleInfo, NewCons, SecTag),
    has_secondary_tag(ModuleInfo, DeadCons, DeadSecTag),
    NewArity = NewNumArgs + (if SecTag = yes then 1 else 0),
    DeadArity = DeadNumArgs + (if DeadSecTag = yes then 1 else 0),

    % The new cell must not be bigger than the dead cell.
    NewArity =< DeadArity,

    % Verify whether the cons_ids and arities match the reuse constraint
    % specified by the user.
    Constraint = Background ^ back_strategy,
    DiffArity = DeadArity - NewArity,
    (
        Constraint = within_n_cells_difference(N),
        DiffArity =< N
    ;
        Constraint = same_cons_id,
        SameCons = yes
    ),

    % Upon success of all the previous checks, determine the number of
    % fields that do not require an update if the construction unification
    % would reuse the deconstructed cell.
    %
    ReuseFields = already_correct_fields(SecTag, NewCellArgs,
        DeadSecTag, DeadCellArgs),
    UpToDateFields = list.length(
        list.delete_all(ReuseFields, needs_update)),

    % Finally, compute the value of this reuse-configuration.
    (if SameCons = yes then SameConsV = 0 else SameConsV = 1),
    Weight = ( (alfa_value + gamma_value) * NewArity + beta_value
        - gamma_value * (NewArity - UpToDateFields)
        - beta_value * SameConsV
        - alfa_value * DiffArity ),
    Weight > 0,
    ReuseType = reuse_type(SameCons, ReuseFields, float(Weight)).

:- pred cons_has_normal_fields(module_info::in, cons_id::in) is semidet.

cons_has_normal_fields(ModuleInfo, ConsId) :-
    require_complete_switch [ConsId]
    (
        ConsId = cons(_, _, _),
        get_cons_repn_defn_det(ModuleInfo, ConsId, ConsRepnDefn),
        ConsArgRepns = ConsRepnDefn ^ cr_args,
        all [ArgRepn] (
            list.member(ArgRepn, ConsArgRepns)
        =>
            ArgRepn = ctor_arg_repn(_, _, apw_full(_, _), _)
        )
    ;
        ConsId = tuple_cons(_)
    ;
        ConsId = ground_term_const(_, _RepnConsId)
        % We could test whether _RepnConsId is normal as we test cons/3
        % cons_ids above, but we should not do so, because ground term
        % constants are NOT candidates for reuse.
    ;
        ( ConsId = type_info_const(_)
        ; ConsId = typeclass_info_const(_)
        ),
        % XXX Before the require_complete_switch scope was added,
        % this predicate silently failed for these two cons_ids.
        % To me (zs), this shows that this module treats these cons_ids
        % as not having normal fields more by accident than by design.
        fail
    ;
        ( ConsId = closure_cons(_, _)
        ; ConsId = some_int_const(_)
        ; ConsId = float_const(_)
        ; ConsId = char_const(_)
        ; ConsId = string_const(_)
        ; ConsId = impl_defined_const(_)
        ; ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = tabling_info_const(_)
        ; ConsId = table_io_entry_desc(_)
        ; ConsId = deep_profiling_proc_layout(_)
        ),
        unexpected($pred, "unusual cons_id")
    ).

:- func glb_reuse_types(list(reuse_type)) = reuse_type is semidet.

glb_reuse_types([First|Rest]) =
    list.foldl(glb_reuse_types_2, Rest, First).

:- func glb_reuse_types_2(reuse_type, reuse_type) = reuse_type.

glb_reuse_types_2(R1, R2) = R :-
    R1 = reuse_type(SameCons1, Fields1, V1),
    R2 = reuse_type(SameCons2, Fields2, V2),
    R = reuse_type(SameCons1 `and` SameCons2, Fields1 `ands` Fields2,
        (V1 + V2) / 2.00 ).

:- func ands(list(needs_update), list(needs_update)) = list(needs_update).

ands(L1, L2) = L :-
    ( if length(L1) =< length(L2) then
        L1b = L1,
        L2b = take_upto(length(L1), L2)
    else
        L1b = take_upto(length(L2), L1),
        L2b = L2
    ),
    L = list.map_corresponding(needs_update_and, L1b, L2b).

:- func needs_update_and(needs_update, needs_update) = needs_update.

needs_update_and(needs_update, needs_update) = needs_update.
needs_update_and(needs_update, does_not_need_update) = needs_update.
needs_update_and(does_not_need_update, needs_update) = needs_update.
needs_update_and(does_not_need_update, does_not_need_update) =
    does_not_need_update.

%---------------------------------------------------------------------------%

    % has_secondary_tag(ModuleInfo, ConsId, HasSecTag) returns `yes' iff
    % ConsId requires a remote secondary tag to distinguish between
    % the various functors of its type.
    %
:- pred has_secondary_tag(module_info::in, cons_id::in, bool::out) is det.

has_secondary_tag(ModuleInfo, ConsId, SecondaryTag) :-
    ( if
        get_cons_repn_defn(ModuleInfo, ConsId, ConsRepn),
        ConsTag = ConsRepn ^ cr_tag,
        get_maybe_secondary_tag(ConsTag) = yes(_)
    then
        SecondaryTag = yes
    else
        SecondaryTag = no
    ).

    % already_correct_fields(ExplicitSecTagC, VarsC, ExplicitSecTagR, VarsR)
    % takes a list of variables, VarsC, which are the arguments for the cell to
    % be constructed and the list of variables, VarsR, which are the arguments
    % for the cell to be reused and returns a list of 'needs_update' values.
    % Each occurrence of 'does_not_need_update' indicates that the argument at
    % the corresponding position in the list of arguments already has the
    % correct value stored in it.  To do this correctly we
    % need to know whether each cell has a secondary tag field.
    %
:- func already_correct_fields(bool, prog_vars, bool, prog_vars) =
    list(needs_update).

already_correct_fields(ExplicitSecTagC, CurrentCellVars, ExplicitSecTagR,
        ReuseCellVars) = ReuseFields :-
    NeedsNoUpdate = already_correct_fields_2(ExplicitSecTagC, CurrentCellVars,
        ExplicitSecTagR, ReuseCellVars),
    LengthC = list.length(CurrentCellVars),
    LengthB = list.length(NeedsNoUpdate),
    NeedsUpdate = list.duplicate(LengthC - LengthB, needs_update),
    ReuseFields = NeedsNoUpdate ++ NeedsUpdate.

:- func already_correct_fields_2(bool, prog_vars, bool, prog_vars)
    = list(needs_update).

already_correct_fields_2(yes, CurrentCellVars, yes, ReuseCellVars)
    = equals(CurrentCellVars, ReuseCellVars).
already_correct_fields_2(yes, CurrentCellVars, no, ReuseCellVars)
    = [needs_update | equals(CurrentCellVars, drop_one(ReuseCellVars))].
already_correct_fields_2(no, CurrentCellVars, yes, ReuseCellVars)
    = [needs_update | equals(drop_one(CurrentCellVars), ReuseCellVars)].
already_correct_fields_2(no, CurrentCellVars, no, ReuseCellVars)
    = equals(CurrentCellVars, ReuseCellVars).

    % equals(ListA, ListB) produces a list of 'needs_update' that indicates
    % whether the corresponding elements from ListA and ListB are equal.  If
    % ListA and ListB are of different lengths, the resulting list is the
    % length of the shorter of the two.
    %
:- func equals(list(T), list(T)) = list(needs_update).

equals([], []) = [].
equals([], [_|_]) = [].
equals([_|_], []) = [].
equals([X | Xs], [Y | Ys]) = [NeedsUpdate | equals(Xs, Ys)] :-
    ( if X = Y then
        NeedsUpdate = does_not_need_update
    else
        NeedsUpdate = needs_update
    ).

:- func drop_one(list(T)) = list(T).

drop_one([]) = [].
drop_one([_ | Xs]) = Xs.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
    % Once a match is selected (hence a set of deconstructions and matching
    % constructions), annotate all the involved unifications in the goal.
    %
:- pred annotate_reuses_in_goal(background_info::in, match::in, hlds_goal::in,
    hlds_goal::out) is det.

annotate_reuses_in_goal(Background, Match, !Goal) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = unify(_, _, _, Unification, _),
        GoalExpr = GoalExpr0,
        annotate_reuse_for_unification(Background, Match, Unification,
            GoalInfo0, GoalInfo)
    ;
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        GoalExpr = GoalExpr0,
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = generic_call( _, _, _, _, _),
        GoalExpr = GoalExpr0,
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        GoalExpr = GoalExpr0,
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = conj(A, Goals0),
        list.map(annotate_reuses_in_goal(Background, Match), Goals0, Goals),
        GoalExpr = conj(A, Goals),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = disj(Goals0),
        list.map(annotate_reuses_in_goal(Background, Match), Goals0, Goals),
        GoalExpr = disj(Goals),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = switch(A, B, Cases0),
        list.map(annotate_reuses_in_case(Background, Match), Cases0, Cases),
        GoalExpr = switch(A, B, Cases),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = negation(_),
        GoalExpr = GoalExpr0,
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = scope(A, ScopeGoal0),
        % XXX We should special-case the handling of from_ground_term_construct
        % scopes. Since they construct ground terms statically, there is no
        % uniqueness we can exploit,
        annotate_reuses_in_goal(Background, Match, ScopeGoal0, ScopeGoal),
        GoalExpr = scope(A, ScopeGoal),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = if_then_else(A, CondGoal0, ThenGoal0, ElseGoal0),
        annotate_reuses_in_goal(Background, Match, CondGoal0, CondGoal),
        annotate_reuses_in_goal(Background, Match, ThenGoal0, ThenGoal),
        annotate_reuses_in_goal(Background, Match, ElseGoal0, ElseGoal),
        GoalExpr = if_then_else(A, CondGoal, ThenGoal, ElseGoal),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand.")
    ),
    !:Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred annotate_reuses_in_case(background_info::in, match::in,
    case::in, case::out) is det.

annotate_reuses_in_case(Background, Match, !Case) :-
    !.Case = case(MainConsId, OtherConsIds, Goal0),
    annotate_reuses_in_goal(Background, Match, Goal0, Goal),
    !:Case = case(MainConsId, OtherConsIds, Goal).

:- pred annotate_reuse_for_unification(background_info::in, match::in,
    unification::in, hlds_goal_info::in, hlds_goal_info::out) is det.

annotate_reuse_for_unification(Background, Match, Unification, !GoalInfo) :-
    CurrentProgramPoint = program_point_init(!.GoalInfo),
    (
        Unification = deconstruct(_, _, _, _, _, _),
        ( if
            match_find_deconstruction_spec(Match, CurrentProgramPoint,
                _DeconSpec)
        then
            goal_info_set_reuse(potential_reuse(cell_died), !GoalInfo)
        else
            true
        )
    ;
        Unification = construct(_, _, _, _, _, _, _),
        ( if
            match_find_construction_spec(Match, CurrentProgramPoint, ConSpec)
        then
            DeadVar = match_get_dead_var(Match),
            DeadConsIds = match_get_dead_cons_ids(Match),
            ReuseAs = match_get_condition(Background, Match),
            ReuseFields = ConSpec ^ con_reuse ^ reuse_fields,

            ( if reuse_as_conditional_reuses(ReuseAs) then
                Kind = conditional_reuse
            else if reuse_as_all_unconditional_reuses(ReuseAs) then
                Kind = unconditional_reuse
            else
                % reuse_as_no_reuses(ReuseAs)
                unexpected($pred, "no reuse conditions")
            ),
            CellReused = cell_reused(DeadVar, Kind, DeadConsIds,
                ReuseFields),
            (
                Kind = conditional_reuse,
                KindReuse = potential_reuse(CellReused)
            ;
                % When the reuse is unconditional, we can safely annotate
                % that the unification is always a reuse unification.
                Kind = unconditional_reuse,
                KindReuse = reuse(CellReused)
            ),
            goal_info_set_reuse(KindReuse, !GoalInfo)
        else
            true
        )
    ;
        Unification = assign(_, _)
    ;
        Unification = simple_test(_, _)
    ;
        Unification = complicated_unify(_, _, _),
        unexpected($pred, "complicated_unify")
    ).

:- pred match_find_deconstruction_spec(match::in, program_point::in,
    deconstruction_spec::out) is semidet.

match_find_deconstruction_spec(Match, ProgramPoint, DeconstructionSpec) :-
    list.filter(deconstruction_spec_with_program_point(ProgramPoint),
        Match ^ decon_specs, [DeconstructionSpec]).

:- pred match_find_construction_spec(match::in, program_point::in,
    construction_spec::out) is semidet.

match_find_construction_spec(Match, ProgramPoint, ConstructionSpec) :-
    list.filter(construction_spec_with_program_point(ProgramPoint),
        Match ^ con_specs, [ConstructionSpec]).

:- pred deconstruction_spec_with_program_point(program_point::in,
    deconstruction_spec::in) is semidet.

deconstruction_spec_with_program_point(DeconstructionSpec ^ decon_pp,
    DeconstructionSpec).

:- pred construction_spec_with_program_point(program_point::in,
    construction_spec::in) is semidet.

construction_spec_with_program_point(ConstructionSpec ^ con_pp,
    ConstructionSpec).

%---------------------------------------------------------------------------%
%
% Predicates to print intermediate results as stored in a match_table.
%

:- func line_length = int.

line_length = 79.

:- pred dump_line(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

dump_line(Stream, Msg, !IO) :-
    Prefix = "%---",
    Start = string.append(Prefix, Msg),
    Remainder = line_length - string.count_codepoints(Start) - 1,
    Line = Start ++ string.duplicate_char('-', Remainder),
    io.write_string(Stream, Line, !IO),
    io.write_string(Stream, "%\n", !IO).

:- pred dump_match_table(io.text_output_stream::in, match_table::in, match::in,
    io::di, io::uo) is det.

dump_match_table(Stream, MatchTable, HighestMatch, !IO) :-
    dump_line(Stream, "reuse table", !IO),
    io.write_string(Stream, "%\t|\tvar\t|\tvalue\t|\tdegree\n", !IO),
    dump_match(Stream, "%-sel- ", HighestMatch, !IO),
    dump_full_table(Stream, MatchTable, !IO),
    dump_line(Stream, "", !IO).

:- pred dump_match(io.text_output_stream::in, string::in, match::in,
    io::di, io::uo) is det.

dump_match(Stream, Prefix, Match, !IO) :-
    MatchVarInt = term.var_to_int(match_get_dead_var(Match)),
    io.format(Stream, "%s\t|\t%d\t|\t", [s(Prefix), i(MatchVarInt)], !IO),
    Val = Match ^ match_value,
    ( if Val = 0.0 then
        io.write_string(Stream, "-", !IO)
    else
        io.format(Stream, "%.2f", [f(Val)], !IO)
    ),
    Degree = Match ^ match_degree,
    io.format(Stream, "\t|\t%d\t", [i(Degree)], !IO),
    dump_match_details(Stream, Match, !IO).

:- pred dump_match_details(io.text_output_stream::in, match::in,
    io::di, io::uo) is det.

dump_match_details(Stream, Match, !IO) :-
    Conds = list.map((func(DeconSpec) = DeconSpec ^ decon_conds),
        Match ^ decon_specs),
    ( if all_true(reuse_as_all_unconditional_reuses, Conds) then
        CondsString = "A"
    else
        CondsString = "C"
    ),
    D = list.length(Match ^ decon_specs),
    C = list.length(Match ^ con_specs),
    io.format(Stream, "d: %d, c: %d, Co: %s\n",
        [i(D), i(C), s(CondsString)], !IO).

:- pred dump_full_table(io.text_output_stream::in, match_table::in,
    io::di, io::uo) is det.

dump_full_table(Stream, MatchTable, !IO) :-
    ( if multi_map.is_empty(MatchTable) then
        dump_line(Stream, "empty match table", !IO)
    else
        dump_line(Stream, "full table (start)", !IO),
        multi_map.values(MatchTable, Matches),
        list.foldl(dump_match(Stream, "%-----"), Matches, !IO),
        dump_line(Stream, "full table (end)", !IO)
    ).

%---------------------------------------------------------------------------%

    % After determining all local reuses of dead datastructures (a data
    % structure becomes dead and is reused in one and the same procedure), we
    % determine the 'global reuses': deconstructions that yield dead data
    % structures, without imposing any reuse constraints are annotated so that
    % these cells can be cached whenever the user specifies that option.
    %
    % XXX cell caching is not actually implemented, but we use the same
    % information to free dead cells that have no reuse opportunity
    %
:- pred check_for_cell_caching(bool::in, dead_cell_table::in,
    hlds_goal::in, hlds_goal::out) is det.

check_for_cell_caching(VeryVerbose, DeadCellTable0, !Goal) :-
    dead_cell_table_remove_conditionals(DeadCellTable0, DeadCellTable),
    ( if dead_cell_table_is_empty(DeadCellTable) then
        trace [io(!IO)] (
            io.stderr_stream(StdErr, !IO),
            maybe_write_string(StdErr, VeryVerbose,
                "% No cells to be cached/freed.\n", !IO)
        )
    else
        trace [io(!IO)] (
            io.stderr_stream(StdErr, !IO),
            maybe_write_string(StdErr, VeryVerbose,
                "% Marking cacheable/freeable cells.\n", !IO)
        ),
        check_for_cell_caching_2(DeadCellTable, !Goal)
    ).

:- pred check_for_cell_caching_2(dead_cell_table::in,
    hlds_goal::in, hlds_goal::out) is det.

check_for_cell_caching_2(DeadCellTable, !Goal) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = unify(A, B, C, Unification0, D),
        check_for_cell_caching_in_unification(DeadCellTable,
            Unification0, Unification, GoalInfo0, GoalInfo),
        GoalExpr = unify(A, B, C, Unification, D)
    ;
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        GoalExpr = GoalExpr0,
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = generic_call( _, _, _, _, _),
        GoalExpr = GoalExpr0,
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        GoalExpr = GoalExpr0,
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = conj(A, Goals0),
        list.map(check_for_cell_caching_2(DeadCellTable), Goals0, Goals),
        GoalExpr = conj(A, Goals),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = disj(Goals0),
        list.map(check_for_cell_caching_2(DeadCellTable), Goals0, Goals),
        GoalExpr = disj(Goals),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = switch(A, B, Cases0),
        list.map(check_for_cell_caching_in_case(DeadCellTable), Cases0, Cases),
        GoalExpr = switch(A, B, Cases),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = negation(_),
        GoalExpr = GoalExpr0,
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = scope(A, ScopeGoal0),
        % XXX We should special-case the handling of from_ground_term_construct
        % scopes. Since they construct ground terms statically, there is no
        % uniqueness we can exploit,
        check_for_cell_caching_2(DeadCellTable, ScopeGoal0, ScopeGoal),
        GoalExpr = scope(A, ScopeGoal),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = if_then_else(A, CondGoal0, ThenGoal0, ElseGoal0),
        check_for_cell_caching_2(DeadCellTable, CondGoal0, CondGoal),
        check_for_cell_caching_2(DeadCellTable, ThenGoal0, ThenGoal),
        check_for_cell_caching_2(DeadCellTable, ElseGoal0, ElseGoal),
        GoalExpr = if_then_else(A, CondGoal, ThenGoal, ElseGoal),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ),
    !:Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred check_for_cell_caching_in_case(dead_cell_table::in,
    case::in, case::out) is det.

check_for_cell_caching_in_case(DeadCellTable, !Case) :-
    !.Case = case(MainConsId, OtherConsIds, Goal0),
    check_for_cell_caching_2(DeadCellTable, Goal0, Goal),
    !:Case = case(MainConsId, OtherConsIds, Goal).

:- pred check_for_cell_caching_in_unification(dead_cell_table::in,
    unification::in, unification::out,
    hlds_goal_info::in, hlds_goal_info::out) is det.

check_for_cell_caching_in_unification(DeadCellTable, !Unification,
        !GoalInfo) :-
    ( if
        !.Unification = deconstruct(Var, ConsId, Args, ArgModes, CanFail, _),
        Condition = dead_cell_table_search(program_point_init(!.GoalInfo),
            DeadCellTable),
        not reuse_condition_is_conditional(Condition)
    then
        !:Unification = deconstruct(Var, ConsId, Args, ArgModes, CanFail,
            can_cgc),
        % XXX Why potential_reuse and not simply "reuse"?
        ReuseInfo = potential_reuse(cell_died),
        goal_info_set_reuse(ReuseInfo, !GoalInfo)
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.direct.choose_reuse.
%---------------------------------------------------------------------------%
