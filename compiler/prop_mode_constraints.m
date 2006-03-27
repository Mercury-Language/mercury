%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: prop_mode_constraints.m.
% Main author: richardf.

% XXX This module essentially serves as interface between the
% pre-existing mode_constraints module and the new
% abstract_mode_constraints and build_mode_constraints modules.
% Ultimately its contents should probably be moved into those
% modules respectively.

%-----------------------------------------------------------------------------%

:- module check_hlds.prop_mode_constraints.
:- interface.

:- import_module check_hlds.abstract_mode_constraints.
:- import_module check_hlds.build_mode_constraints.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.
:- import_module list.
:- import_module map.

%-----------------------------------------------------------------------------%

    % Storing constraints by predicate.
    %
:- type pred_constraints_map == map(pred_id, mode_constraints).

%-----------------------------------------------------------------------------%

    % process_scc(SCC, !ModuleInfo, !Varset, !VarMap, !PredConstraintsMap),
    %
    % For each predicate in SCC:
    %   Adds producer/consumer constraints to PredConstraintsMap,
    %   Adds goal path annotations to its clauses in ModuleInfo,
    %   Adds any constraint variables it requires to Varset and VarMap
    %
:- pred process_scc(list(pred_id)::in, module_info::in, module_info::out,
    mc_var_info::in, mc_var_info::out, pred_constraints_map::in,
    pred_constraints_map::out) is det.

    % Checks whether a predicate has been imported according to the
    % status_is_imported pred in the hlds_pred module.
    %
:- pred module_info_pred_status_is_imported(module_info::in, pred_id::in)
    is semidet.

    % Writes in human readable form to the current output stream the
    % information in the pred_constraints_map, indicating which
    % predicate each set of constraints applies to.
    %
:- pred pretty_print_pred_constraints_map(module_info::in, mc_varset::in,
    pred_constraints_map::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.goal_path.
:- import_module check_hlds.mcsolver.
:- import_module check_hlds.mode_constraint_robdd.
:- import_module check_hlds.mode_ordering.
:- import_module check_hlds.mode_util.
:- import_module hlds.hhf.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.inst_graph.
:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mode_robdd.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.modules.
:- import_module transform_hlds.dependency_graph.

:- import_module assoc_list.
:- import_module bimap.
:- import_module bool.
:- import_module int.
:- import_module multi_map.
:- import_module robdd.
:- import_module set.
:- import_module sparse_bitset.
:- import_module string.
:- import_module svmap.
:- import_module term.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%

process_scc(SCC0, !ModuleInfo, !VarInfo, !Constraints) :-
    % Process only predicates from this module
    list.filter(module_info_pred_status_is_imported(!.ModuleInfo),
        SCC0, _, SCC),

    % Prepare the solver variables for the home path of
    % each predicate of the SCC - needed for calls to
    % predicates in the same SCC that do not have mode
    % declarations.
    add_mc_vars_for_scc_heads(!.ModuleInfo, SCC, !VarInfo),

    % Now go through the SCC and add the constraint
    % variables and then constraints predicate by predicate
    list.foldl3(process_pred, SCC, !ModuleInfo, !VarInfo, !Constraints).

    % process_pred(PredId, !ModuleInfo, !Varset, !VarMap, !Constraints)
    %
    % Performs a number of tasks for predicate PredId:
    %   1) Fills out the goal_path information in the
    %      ModuleInfo structure
    %   2) Adds producer/consumer constraint variables for program
    %      variables corresponding to any location at which they are
    %      nonlocal to Varset and VarMap. (Elsewhere is is clear as to
    %      whether they are produced or consumed.)
    %   3) Adds mode declaration constraints to Constraints
    %   4) Adds goal constraints to Constraints
    %
    % NOTE: it relies on the head variables for any predicate
    % without mode declarations that is called by this one (PredId)
    % to have the constraint variables corresponding to the empty
    % goal path (ie the whole body of the predicate) to already be
    % in VarMap (and therefore Varset).
    %
:- pred process_pred(pred_id::in, module_info::in, module_info::out,
    mc_var_info::in, mc_var_info::out, pred_constraints_map::in,
    pred_constraints_map::out) is det.

process_pred(PredId, !ModuleInfo, !VarInfo, !Constraints) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    process_pred(!.ModuleInfo, PredId, PredInfo0, PredInfo, !VarInfo,
        !Constraints),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

    % The working part of process_pred/8 - just with the PredInfo
    % unpacked from the ModuleInfo.
    %
:- pred process_pred(module_info::in, pred_id::in, pred_info::in,
    pred_info::out, mc_var_info::in, mc_var_info::out,
    pred_constraints_map::in, pred_constraints_map::out)
    is det.

process_pred(ModuleInfo, PredId, !PredInfo, !VarInfo, !Constraints) :-
    fill_goal_path_slots_in_clauses(ModuleInfo, GoalPathOptimisation,
        !PredInfo),

    %
    % If mode inference requested, just add constraints for the clause body,
    % otherwise, process the predicate for each of the procedures.
    %
    ( pred_info_infer_modes(!.PredInfo) ->
        add_clauses_constraints(ModuleInfo, PredId, !.PredInfo, !VarInfo,
            abstract_mode_constraints.init, BodyConstraints),
        svmap.set(PredId, BodyConstraints, !Constraints)
    ;
        process_mode_declared_pred(ModuleInfo, PredId, !.PredInfo,
            !VarInfo, !Constraints)
    ),

    % XXX Currently the constraints simply say that if a
    % variable is bound at a disjunct it is bound at the
    % disjunction by making the relevant variables
    % equivalent. Setting GoalPathOptimisation to yes will
    % cause the disjucts to be given the same path as the
    % disjunction, so that the relevant constraint variables
    % will not need to be constrained equivalent - they will
    % be the same variable. It will do the same for other
    % path types with similar equivalence constraints -
    % refer to the goal_path module for a more detailed
    % description.
    GoalPathOptimisation = no.

    % process_mode_declared_pred(ModuleInfo, PredId, PredInfo, !VarInfo,
    %   !PredConstraintsMap)
    %
    % Uses the clauses and mode declarations in PredInfo (which should
    % be for predicate PredId taken from ModuleInfo) to create
    % producer/consumer constraints for program variables in predicate
    % PredId and stores them in PredConstraintsMap. VarInfo is updated
    % with any constraint variables used.
    %
:- pred process_mode_declared_pred(module_info::in, pred_id::in,
    pred_info::in, mc_var_info::in, mc_var_info::out,
    pred_constraints_map::in, pred_constraints_map::out)
    is det.

process_mode_declared_pred(ModuleInfo, PredId, PredInfo, !VarInfo,
    !PredConstraintsMap) :-

    ProcIds = pred_info_all_procids(PredInfo),

    add_clauses_constraints(ModuleInfo, PredId, PredInfo, !VarInfo,
        abstract_mode_constraints.init, BodyConstr),

    % Store procedure specific constraints in the constraints structure
    list.map(pred_info_proc_info(PredInfo), ProcIds, ProcInfos),
    list.foldl2_corresponding(
        process_mode_declared_proc(ModuleInfo, PredId),
        ProcIds, ProcInfos, !VarInfo, BodyConstr, FullConstraints),

    svmap.set(PredId, FullConstraints, !PredConstraintsMap).

    % process_mode_declared_proc(ModuleInfo, PredId, ProcId,
    %   ProcInfo, !VarInfo, !PredConstraints)
    %
    % Adds constraints based on the mode declaration in
    % ProcInfo to PredConstraints structure, associating them
    % specifically with ProcId. Relies on the constraint variables
    % associated with the head variables of PredId at the empty goal
    % path being stored in VarInfo.
    %
:- pred process_mode_declared_proc(module_info::in,
    pred_id::in, proc_id::in, proc_info::in, mc_var_info::in,
    mc_var_info::out, mode_constraints::in, mode_constraints::out) is det.

process_mode_declared_proc(ModuleInfo, PredId, ProcId, ProcInfo, !VarInfo,
    !PredConstraints) :-

    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_headvars(ProcInfo, Args),

    add_mode_decl_constraints(ModuleInfo, PredId, ProcId, ArgModes, Args,
        !VarInfo, !PredConstraints).

module_info_pred_status_is_imported(ModuleInfo, PredId) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),

    % The following used because pred_info_is_imported/2 is not
    % as comprehensive as status_is_imported/2.
    pred_info_get_import_status(PredInfo, Status),
    status_is_imported(Status, yes).

%----------------------------------------------------------------------------%

    % Put the constraints to the current output stream in human
    % readable format. It titles each pred's constraints with a
    % module qualification based on the default filename for the
    % module followed by the predicate's name.
    %
pretty_print_pred_constraints_map(
    ModuleInfo, ConstraintVarset, PredConstraintsMap, !IO) :-
    ConstrainedPreds = map.keys(PredConstraintsMap),
    list.foldl(
        pretty_print_pred_constraints(ModuleInfo, ConstraintVarset,
            PredConstraintsMap),
        ConstrainedPreds, !IO).

    % Puts the constraints for the specified predicate from the
    % pred_constraints_map to the current output stream in human
    % readable format.
    %
:- pred pretty_print_pred_constraints(module_info::in, mc_varset::in,
    pred_constraints_map::in, pred_id::in, io::di, io::uo) is det.

pretty_print_pred_constraints(ModuleInfo, ConstraintVarset,
        PredConstraintsMap, PredId, !IO) :-
    % Start with a blank line.
    write_error_pieces_plain([fixed("")], !IO),

    hlds_module.module_info_pred_info(ModuleInfo, PredId, PredInfo),
    write_error_pieces_plain([words("Constraints for")] ++
        describe_one_pred_info_name(should_module_qualify, PredInfo) ++
        [suffix(":")], !IO),

    map.lookup(PredConstraintsMap, PredId, PredConstraints),
    FormulaeAndAnnotations = pred_constraints_to_formulae_and_annotations(
        PredConstraints),
    dump_constraints_and_annotations(ConstraintVarset, FormulaeAndAnnotations,
        !IO),
    list.foldl(pretty_print_proc_constraints(ModuleInfo, ConstraintVarset,
        PredConstraints, PredId), pred_info_all_procids(PredInfo), !IO).

    % Puts the constraints specific to the procedure indicated from
    % the pred_p_c_constraints to the current output stream in human
    % readable format.
    %
:- pred pretty_print_proc_constraints(module_info::in, mc_varset::in,
    pred_p_c_constraints::in, pred_id::in, proc_id::in, io::di, io::uo) is det.

pretty_print_proc_constraints(ModuleInfo, ConstraintVarset, PredConstraints,
        PredId, ProcId, !IO) :-
    % Start with a blank line.
    write_error_pieces_plain([fixed("")], !IO),

    write_error_pieces_plain(describe_one_proc_name(ModuleInfo,
        should_module_qualify, proc(PredId, ProcId)) ++ [suffix(":")], !IO),
    FormulaeAndAnnotations =
        proc_constraints_to_formulae_and_annotations(ProcId, PredConstraints),
    dump_constraints_and_annotations(ConstraintVarset, FormulaeAndAnnotations,
        !IO).

%----------------------------------------------------------------------------%
:- end_module prop_mode_constraints.
%----------------------------------------------------------------------------%
