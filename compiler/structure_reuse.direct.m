%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: structure_reuse.direct.m.
% Main authors: nancy.
%
% This module efined procedure and type related to the dectection of so called
% direct reuses within the CTGC system.  A "direct reuse" is a combination of
% the location of a deconstruction unification (where a datastructure may
% become garbage under certain conditions) and a set of locations of
% construction unifications where the garbage datastructure can be reused
% locally. 
%
% Direct reuse analysis requires two steps: 
%   - Detecting where datastructures may become garbage.
%   - Finding where these garbage datastructures can be reused.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_reuse.direct.
:- interface.

:- import_module hlds.hlds_module.
:- import_module transform_hlds.ctgc.structure_reuse.domain.
:- import_module transform_hlds.ctgc.structure_sharing.domain.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred direct_reuse_pass(sharing_as_table::in, module_info::in,
    module_info::out, reuse_as_table::in, reuse_as_table::out, 
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation. 

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.passes_aux.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_out.
:- import_module transform_hlds.ctgc.structure_reuse.direct.choose_reuse.
:- import_module transform_hlds.ctgc.structure_reuse.direct.detect_garbage.
:- import_module transform_hlds.ctgc.util.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module svmap.
:- import_module term.

:- include_module transform_hlds.ctgc.structure_reuse.direct.detect_garbage.
:- include_module transform_hlds.ctgc.structure_reuse.direct.choose_reuse.

%-----------------------------------------------------------------------------%

    % The strategy for determining the reuse possibilities, i.e., either
    % reuse is only allowed between terms that have exactly the same cons_id, 
    % or reuse is also allowed between terms that have different cons_id, yet
    % where the difference in arity is not bigger than a given threshold. 
    %
:- type reuse_strategy
    --->    same_cons_id
    ;       within_n_cells_difference(int).

    % Determine the strategy that was set by the user. 
    %
:- pred get_strategy(reuse_strategy::out, module_info::in, module_info::out, 
    io::di, io::uo) is det.

get_strategy(Strategy, !ModuleInfo, !IO):- 
    io_lookup_string_option(structure_reuse_constraint, ConstraintStr, !IO),
    ( 
        ConstraintStr = "same_cons_id"  
    ->
        Strategy = same_cons_id
    ; 
        ConstraintStr = "within_n_cells_difference"  
    ->
        io_lookup_int_option(structure_reuse_constraint_arg, NCells, !IO),
        Strategy = within_n_cells_difference(NCells)
    ;
        Strategy = same_cons_id,
        Pieces = [words("error: Invalid argument to "), 
            words("`--structure-reuse-constraint.'")],
        write_error_pieces_plain(Pieces, !IO),
        module_info_incr_errors(!ModuleInfo)
    ).

direct_reuse_pass(SharingTable, !ModuleInfo, !ReuseTable, !IO):- 
    % Determine the reuse strategy: 
    get_strategy(Strategy, !ModuleInfo, !IO), 

    % Gather the pred-ids of the preds that need to be analysed.
    module_info_predids(!.ModuleInfo, AllPredIds), 
    list.filter(pred_requires_analysis(!.ModuleInfo), AllPredIds, 
        ToBeAnalysedPredIds), 

    % Analyse and annotate each of the predicates. 
    list.foldl3(direct_reuse_process_pred(Strategy, SharingTable),
        ToBeAnalysedPredIds, !ModuleInfo, !ReuseTable, !IO).

:- pred direct_reuse_process_pred(reuse_strategy::in, sharing_as_table::in,
    pred_id::in, module_info::in, module_info::out, reuse_as_table::in,
    reuse_as_table::out, io::di, io::uo) is det.

direct_reuse_process_pred(Strategy, SharingTable, PredId, !ModuleInfo, 
        !ReuseTable, !IO):-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0), 
    list.foldl3(direct_reuse_process_proc(Strategy, SharingTable, PredId), 
        pred_info_non_imported_procids(PredInfo0), !ModuleInfo, 
        !ReuseTable, !IO).

:- pred direct_reuse_process_proc(reuse_strategy::in, sharing_as_table::in, 
    pred_id::in, proc_id::in, module_info::in, module_info::out,
    reuse_as_table::in, reuse_as_table::out, io::di, io::uo) is det.

direct_reuse_process_proc(Strategy, SharingTable, PredId, ProcId, 
        !ModuleInfo, !ReuseTable, !IO) :- 
    module_info_preds(!.ModuleInfo, Preds0), 
    map.lookup(Preds0, PredId, Pred0), 
    pred_info_get_procedures(Pred0, Procs0), 
    map.lookup(Procs0, ProcId, Proc0), 

    direct_reuse_process_procedure(Strategy, SharingTable, PredId, ProcId, 
        !.ModuleInfo, Pred0, Proc0, Proc, ReuseAs, !IO), 
    reuse_as_table_set(proc(PredId, ProcId), ReuseAs, !ReuseTable),

    map.det_update(Procs0, ProcId, Proc, Procs),
    pred_info_set_procedures(Procs, Pred0, Pred),
    map.det_update(Preds0, PredId, Pred, Preds),
    module_info_set_preds(Preds, !ModuleInfo).

    % Process one individual procedure. 
    %
:- pred direct_reuse_process_procedure(reuse_strategy::in, 
    sharing_as_table::in, pred_id::in, proc_id::in, module_info::in, 
    pred_info::in, proc_info::in, proc_info::out, reuse_as::out, 
    io::di, io::uo) is det.

direct_reuse_process_procedure(Strategy, SharingTable, PredId, ProcId,
        ModuleInfo, PredInfo, !ProcInfo, ReuseAs, !IO):- 
    io_lookup_bool_option(very_verbose, VeryVerbose, !IO),

    write_proc_progress_message("% Direct reuse analysis of ",
        PredId, ProcId, ModuleInfo, !IO),

    proc_info_get_goal(!.ProcInfo, Goal0),

    % Determine the deconstructions in which data may potentially become
    % garbage.
    %
    determine_dead_deconstructions(ModuleInfo, PredInfo, !.ProcInfo, 
        SharingTable, Goal0, DeadCellTable),
    dead_cell_table_maybe_dump(VeryVerbose, DeadCellTable, !IO),

    % Determine how the detected dead datastructures can be reused. 
    % This annotates the goal with potential reuses.
    %
    determine_reuse(Strategy, ModuleInfo, !.ProcInfo, DeadCellTable,
        Goal0, Goal, ReuseAs, !IO),

    proc_info_set_goal(Goal, !ProcInfo), 
    maybe_write_string(VeryVerbose, "% reuse analysis done.\n", !IO).


%-----------------------------------------------------------------------------%
% We use the type dead_cell_table to collect all deconstructions that possibly
% leave garbage behind.
%
%
    % To record the place at which a data structure possible becomes garbage, 
    % we use the notion of a program point. A program point is unique using
    % its goal_path. The context of the goal is used for debugging traces.
    %
:- type program_point 
    --->    pp(
                pp_context  ::  term.context,
                pp_path     ::  goal_path
            ).

    % A dead_cell_table maps program points onto reuse conditions. 
    %
:- type dead_cell_table == map(program_point, reuse_condition).
    

    % Compute the program point of a given goal. 
    %
:- func program_point_init(hlds_goal_info) = program_point.

    % Dump the information contained in a program point.
    %
:- pred dump_program_point(program_point::in, io::di, io::uo) is det.

    % Initialise a dead_cell_table. 
    %
:- func dead_cell_table_init = dead_cell_table. 

    % Check whether the table is empty. 
    %
:- pred dead_cell_table_is_empty(dead_cell_table::in) is semidet.

    % Succeeds if the given program point is listed in the table. Return
    % the associated reuse_condition. 
    %
:- func dead_cell_table_search(program_point, dead_cell_table) 
    = reuse_condition is semidet.

    % Add a program point and its associated reuse_condition to the table.
    %
:- pred dead_cell_table_set(program_point::in, reuse_condition::in, 
    dead_cell_table::in, dead_cell_table::out) is det.

    % Remove a program point from the table. 
    %
:- pred dead_cell_table_remove(program_point::in, 
    dead_cell_table::in, dead_cell_table::out) is det.

    % Remove all program points from the table for which the reuse_conditions
    % are "conditional". 
    %
:- pred dead_cell_table_remove_conditionals(dead_cell_table::in, 
    dead_cell_table::out) is det. 

    % Dump the contents of the table. 
    %
:- pred dead_cell_table_maybe_dump(bool::in, dead_cell_table::in, 
    io::di, io::uo) is det.

program_point_init(Info) = PP :- 
    goal_info_get_context(Info, Context), 
    goal_info_get_goal_path(Info, GoalPath), 
    PP = pp(Context, GoalPath). 

dump_program_point(pp(Context, GoalPath), !IO):- 
    % context
    prog_out.write_context(Context, !IO), 
    io.write_string("--", !IO),
    % goal path
    list.foldl(dump_goal_path_step, GoalPath, !IO).

:- pred dump_goal_path_step(goal_path_step::in, io::di, io::uo) is det.

dump_goal_path_step(conj(N)) -->
    io.write_char('c'),
    io.write_int(N).
dump_goal_path_step(disj(N)) -->
    io.write_char('d'),
    io.write_int(N).
dump_goal_path_step(switch(N, _)) -->
    io.write_char('s'),
    io.write_int(N).
dump_goal_path_step(ite_cond) -->
    io.write_char('c').
dump_goal_path_step(ite_then) -->
    io.write_char('t').
dump_goal_path_step(ite_else) -->
    io.write_char('e').
dump_goal_path_step(neg) -->
    io.write_char('n').
dump_goal_path_step(scope(_)) -->
    io.write_char('q').
dump_goal_path_step(first) -->
    io.write_char('f').
dump_goal_path_step(later) -->
    io.write_char('l').

dead_cell_table_init = map.init.

dead_cell_table_is_empty(Table) :-
    map.is_empty(Table).

dead_cell_table_search(PP, Table) = Table ^ elem(PP). 

dead_cell_table_set(PP, RC, !Table) :- 
    svmap.set(PP, RC, !Table). 

dead_cell_table_remove(PP, !Table) :- 
    svmap.det_remove(PP, _, !Table). 

dead_cell_table_remove_conditionals(!Table) :- 
    map.foldl(dead_cell_table_add_unconditional, !.Table, 
        dead_cell_table_init, !:Table). 

:- pred dead_cell_table_add_unconditional(program_point::in, 
    reuse_condition::in, dead_cell_table::in, dead_cell_table::out) is det.

dead_cell_table_add_unconditional(PP, C, !Table) :- 
    (
        reuse_condition_is_conditional(C)
    ->  
        true
    ; 
        dead_cell_table_set(PP, C, !Table)
    ).

dead_cell_table_maybe_dump(MaybeDump, Table, !IO) :- 
    (
        MaybeDump = no
    ;
        MaybeDump = yes,
        io.write_string("\t\t|--------|\n", !IO),
        map.foldl(dead_cell_entry_dump, Table, !IO),
        io.write_string("\t\t|--------|\n", !IO)
    ).

:- pred dead_cell_entry_dump(program_point::in, reuse_condition::in, 
    io::di, io::uo) is det.

dead_cell_entry_dump(PP, Cond, !IO) :- 
    (
        reuse_condition_is_conditional(Cond)
    -> 
        io.write_string("\t\t|  cond  |\t", !IO)
    ;
        io.write_string("\t\t| always |\t", !IO)
    ), 
    dump_program_point(PP, !IO), 
    io.write_string("\n", !IO). 

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "structure_sharing.direct.m".

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.direct.
%-----------------------------------------------------------------------------%
