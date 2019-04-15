%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2008, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mode_constraint_robdd.m.
% Main author: dmo.
%
% This module provides an abstraction layer on top of the ROBDD library
% modules. It provides for the possibility of storing the constraints in a more
% convenient structure (but less efficient), in addition to the ROBDD.
% This might be desirable for viewing the constraints in a human-readable
% form or for outputting them to the SICStus clpb solver.
%
% Whether this extra information is stored is controlled by the `debug/0'
% predicate.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.mode_constraint_robdd.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mode_robdd.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module io.
:- import_module map.
:- import_module robdd.
:- import_module set.
:- import_module stack.
:- import_module term.

:- type mc_type.

:- type mode_constraint == mode_robdd(mc_type).
:- type mode_constraint_var == var(mc_type).
:- type mode_constraint_vars == vars(mc_type).
:- type mode_constraint_info.
:- type threshold.

:- func init_mode_constraint_info(bool) = mode_constraint_info.
:- func mci_set_pred_id(mode_constraint_info, pred_id) = mode_constraint_info.

:- type rep_var
    --->    in(prog_var)
    ;       out(prog_var)
    ;       prog_var `at` goal_id.

    % Lookup a var in the mode_constraint_info. If the var is not found,
    % insert it.
    %
:- pred mode_constraint_var(rep_var::in, mode_constraint_var::out,
    mode_constraint_info::in, mode_constraint_info::out) is det.

:- pred mode_constraint_var(pred_id::in, rep_var::in, mode_constraint_var::out,
    mode_constraint_info::in, mode_constraint_info::out) is det.

    % Functional version of the above. If the var is not found, abort.
    %
:- func mode_constraint_var(mode_constraint_info, rep_var) =
    mode_constraint_var.

:- pred enter_lambda_goal(goal_id::in, mode_constraint_info::in,
    mode_constraint_info::out) is det.

:- pred leave_lambda_goal(mode_constraint_info::in, mode_constraint_info::out)
    is det.

    % lambda_path extends the idea of the goal_id to allow describing the
    % location of a goal within nested lambda goals.
:- type lambda_path == stack(goal_id).

    % Describes a var, its pred-id and lambda-nesting level.
    % XXX think up a better name for this.
:- type prog_var_and_level.

:- pred get_prog_var_level(mode_constraint_info::in, prog_var::in,
    prog_var_and_level::out) is det.

:- pred set_level_from_var(prog_var_and_level::in,
    mode_constraint_info::in, mode_constraint_info::out) is det.

    % Return the current max var for later use by restrict_threshold.
    %
:- pred save_threshold(mode_constraint_info::in, threshold::out) is det.

:- func restrict_threshold(threshold, mode_constraint) = mode_constraint.

:- func restrict_filter(pred(rep_var), mode_constraint_info,
    mode_constraint) = mode_constraint.
:- mode restrict_filter(pred(in) is semidet, in, in) = out is det.

:- pred save_min_var_for_pred(pred_id::in,
    mode_constraint_info::in, mode_constraint_info::out) is det.

:- pred save_max_var_for_pred(pred_id::in, mode_constraint_info::in,
    mode_constraint_info::out) is det.

:- pred get_interesting_vars_for_pred(mode_constraint_info::in, pred_id::in,
    set(mode_constraint_var)::out) is det.

    % Set the input_nodes field of the mode_constraint_info and make sure
    % the zero_var is constrained to be zero in the mode_constraint.
    %
:- pred set_input_nodes(mode_constraint::in, mode_constraint::out,
    mode_constraint_info::in, mode_constraint_info::out) is det.

:- pred set_simple_mode_constraints(mode_constraint_info::in,
    mode_constraint_info::out) is det.

:- pred unset_simple_mode_constraints(mode_constraint_info::in,
    mode_constraint_info::out) is det.

:- pred using_simple_mode_constraints(mode_constraint_info::in) is semidet.

:- pred get_forward_goal_path_map(mode_constraint_info::in,
    goal_forward_path_map::out) is det.

:- pred get_forward_goal_path_map_for_pred(mode_constraint_info::in,
    pred_id::in, goal_forward_path_map::out) is det.

:- pred add_forward_goal_path_map(pred_id::in, goal_forward_path_map::in,
    mode_constraint_info::in, mode_constraint_info::out) is det.

% Remove the comments here and on the definition if you want to debug
% the mode constraint system.
%
% :- pred dump_mode_constraints(module_info::in, pred_info::in, inst_graph::in,
%   mode_constraint::in, mode_constraint_info::in, io::di, io::uo) is det.
%
% :- pred dump_constraints(module_info::in, prog_varset::in,
%   mode_constraint::in, io::di, io::uo) is det.

:- pred robdd_to_dot(mode_constraint::in, prog_varset::in,
    mode_constraint_info::in, string::in, io::di, io::uo) is det.

% A prodvars_map maps each subgoal to the set of variables produced
% by that subgoal.

:- type prodvars_map == map(lambda_path, set_of_progvar).

:- func atomic_prodvars_map(mode_constraint, mode_constraint_info) =
    prodvars_map.

:- implementation.

% :- import_module mode_robdd.tfeir.
:- import_module mode_robdd.tfeirn.
% :- import_module mode_robdd.check.

:- import_module bimap.
:- import_module list.
:- import_module require.
:- import_module solutions.
:- import_module sparse_bitset.
:- import_module std_util.
:- import_module varset.

:- type mc_type
    --->    mc_type.

:- type mode_constraint_info
    --->    mode_constraint_info(
                mci_varset              :: varset(mc_type),
                mci_varmap              :: mode_constraint_varmap,
                mci_pred_id             :: pred_id,
                mci_lambda_path         :: lambda_path,
                mci_min_vars            :: map(pred_id, mode_constraint_var),
                mci_max_vars            :: map(pred_id, mode_constraint_var),
                mci_input_nodes         :: set_of_progvar,

                % A var that is always zero.
                mci_zero_var            :: robdd_var,

                % Are we using the simplified constraint model.
                mci_simple_constraints  :: bool,

                mci_goal_path_map       :: map(pred_id, goal_forward_path_map)
            ).

:- type threshold
    --->    threshold(mode_constraint_var).

init_mode_constraint_info(Simple) = MCI :-
    VarSet0 = varset.init,
    varset.new_var(ZeroVar, VarSet0, VarSet),
    PredId = hlds_pred.initial_pred_id,
    MCI = mode_constraint_info(VarSet, bimap.init, PredId, stack.init,
        map.init, map.init, set_of_var.init, ZeroVar, Simple, map.init).

mci_set_pred_id(MCI, PredId) = MCI ^ mci_pred_id := PredId.

:- type robdd_var == var(mc_type).

:- type mode_constraint_varmap == bimap(varmap_key, robdd_var).

    % Key for looking up robdd_vars.
    % `pred_id' is the predicate the variable belongs to.
    % `lambda_path' describes the location of the lambda_goal
    % we are referring to.
:- type varmap_key
    --->    key(
                rep_var,
                pred_id,
                lambda_path
            ).

mode_constraint_var(RepVar0, RobddVar, !MCI) :-
    mode_constraint_var(!.MCI ^ mci_pred_id, RepVar0, RobddVar, !MCI).

mode_constraint_var(PredId, RepVar0, RobddVar, !MCI) :-
    (
        RepVar0 = ProgVar `at` _,
        set_of_var.contains(!.MCI ^ mci_input_nodes, ProgVar)
    ->
        % This RepVar must be false since the corresponding input var
        % is true.  We can just return the zero var.
        RobddVar = !.MCI ^ mci_zero_var
    ;
        RepVar = RepVar0,
        LambdaId = !.MCI ^ mci_lambda_path,
        Key = key(RepVar, PredId, LambdaId),
        ( bimap.search(!.MCI ^ mci_varmap, Key, RobddVar0) ->
            RobddVar = RobddVar0
        ;
            varset.new_var(RobddVar, !.MCI ^ mci_varset, NewVarSet),
            bimap.set(Key, RobddVar, !.MCI ^ mci_varmap, NewVarMap),
            !MCI ^ mci_varset := NewVarSet,
            !MCI ^ mci_varmap := NewVarMap
        )
    ).

mode_constraint_var(MCI, RepVar) = Var :-
    Key = key(RepVar, MCI ^ mci_pred_id, MCI ^ mci_lambda_path),
    bimap.lookup(MCI ^ mci_varmap, Key, Var).

enter_lambda_goal(GoalId, !MCI) :-
    LambdaPath0 = !.MCI ^ mci_lambda_path,
    !MCI ^ mci_lambda_path := stack.push(LambdaPath0, GoalId).

leave_lambda_goal(!MCI) :-
    LambdaPath0 = !.MCI ^ mci_lambda_path,
    stack.det_pop(_GoalPath, LambdaPath0, LambdaPath),
    !MCI ^ mci_lambda_path := LambdaPath.

:- type prog_var_and_level
    --->    prog_var_and_level(
                prog_var,
                pred_id,
                lambda_path
            ).

get_prog_var_level(MCI, Var, prog_var_and_level(Var, PredId, LambdaPath)) :-
    PredId = MCI ^ mci_pred_id,
    LambdaPath = MCI ^ mci_lambda_path.

set_level_from_var(prog_var_and_level(_Var, PredId, LambdaPath), !MCI) :-
    !MCI ^ mci_pred_id := PredId,
    !MCI ^ mci_lambda_path := LambdaPath.

save_threshold(MCI, threshold(varset.max_var(VarSet))) :-
    VarSet = MCI ^ mci_varset.

restrict_threshold(threshold(Threshold), Constraint) =
    restrict_threshold(Threshold, ensure_normalised(Constraint)).

restrict_filter(P0, MCI, M) = restrict_filter(P, ensure_normalised(M)) :-
    P = (pred(MCV::in) is semidet :-
        bimap.reverse_lookup(MCI ^ mci_varmap, key(RV, PredId, _), MCV),
        (
            PredId \= MCI ^ mci_pred_id
        ;
            P0(RV)
        )
    ).

save_min_var_for_pred(PredId, !MCI) :-
    save_threshold(!.MCI, threshold(Threshold)),
    MinVars0 = !.MCI ^ mci_min_vars,
    map.set(PredId, Threshold, MinVars0, MinVars),
    !MCI ^ mci_min_vars := MinVars.

save_max_var_for_pred(PredId, !MCI) :-
    save_threshold(!.MCI, threshold(Threshold)),
    MaxVars0 = !.MCI ^ mci_max_vars,
    map.set(PredId, Threshold, MaxVars0, MaxVars),
    !MCI ^ mci_max_vars := MaxVars.

get_interesting_vars_for_pred(MCI, PredId, Vars) :-
    MinVars = MCI ^ mci_min_vars,
    MaxVars = MCI ^ mci_max_vars,
    VarSet = MCI ^ mci_varset,
    Vars = ( set.sorted_list_to_set `compose`
        list.filter((pred(V::in) is semidet :-
            compare(<, map.lookup(MinVars, PredId), V),
            \+ compare(<, map.lookup(MaxVars, PredId), V)
        )) `compose` varset.vars
    )(VarSet).

set_input_nodes(Constraint0, Constraint, !MCI) :-
    VarMap = !.MCI ^ mci_varmap,
    LambdaPath = !.MCI ^ mci_lambda_path,
    PredId = !.MCI ^ mci_pred_id,
    bimap.ordinates(VarMap, Keys),
    Constraint1 = ensure_normalised(Constraint0),
    solutions.solutions(
        (pred(ProgVar::out) is nondet :-
            list.member(Key, Keys),
            Key = key(in(ProgVar), PredId, LambdaPath),
            bimap.lookup(VarMap, Key, RobddVar),
            var_entailed(Constraint1, RobddVar)
        ), InputNodes),
    !MCI ^ mci_input_nodes := set_of_var.sorted_list_to_set(InputNodes),
    Constraint = Constraint0 ^ not_var(!.MCI ^ mci_zero_var).

set_simple_mode_constraints(!MCI) :-
    !MCI ^ mci_simple_constraints := yes.

unset_simple_mode_constraints(!MCI) :-
    !MCI ^ mci_simple_constraints := no.

using_simple_mode_constraints(MCI) :-
    MCI ^ mci_simple_constraints = yes.

get_forward_goal_path_map(MCI, ForwardGoalPathMap) :-
    map.lookup(MCI ^ mci_goal_path_map, MCI ^ mci_pred_id, ForwardGoalPathMap).

get_forward_goal_path_map_for_pred(MCI, PredId, ForwardGoalPathMap) :-
    map.lookup(MCI ^ mci_goal_path_map, PredId, ForwardGoalPathMap).

add_forward_goal_path_map(PredId, ForwardGoalPathMap, !MCI) :-
    ForwardGoalPathMapMap0 = !.MCI ^ mci_goal_path_map,
    map.det_insert(PredId, ForwardGoalPathMap,
        ForwardGoalPathMapMap0, ForwardGoalPathMapMap),
    !MCI ^ mci_goal_path_map := ForwardGoalPathMapMap.

% dump_mode_constraints(_ModuleInfo, _PredInfo, _InstGraph, ROBDD, MCI) -->
%   { AL = (list.sort `compose`
%       assoc_list.reverse_members `compose`
%       bimap.to_assoc_list)(MCI ^ mci_varmap) },
%   list.foldl((pred((MCV - key(RV, _, _))::in, di, uo) is det -->
%       print(MCV), write_string("\t"), print(RV), nl), AL),
%
%   nl,
%   flush_output,
%
%   print_robdd(ROBDD),
%
%   nl,
%   flush_output.
%
% dump_constraints(_ModuleInfo, _VarSet, ROBDD) -->
%   { robdd.size(ROBDD, Nodes, Depth) },
%   io.format("Nodes: %d \tDepth: %d\n", [i(Nodes), i(Depth)]),
%   flush_output.

:- pred dump_mode_constraint_var(prog_varset::in, rep_var::in,
    io::di, io::uo) is det.

dump_mode_constraint_var(VarSet, in(V), !IO) :-
    varset.lookup_name(VarSet, V, Name),
    io.write_string(Name, !IO),
    io.write_string("_in", !IO).
dump_mode_constraint_var(VarSet, out(V), !IO) :-
    varset.lookup_name(VarSet, V, Name),
    io.write_string(Name, !IO),
    io.write_string("_out", !IO).
dump_mode_constraint_var(VarSet, V `at` Id, !IO) :-
    varset.lookup_name(VarSet, V, Name),
    io.write_string(Name, !IO),
    io.write_char('_', !IO),
    Id = goal_id(IdNum),
    io.write_int(IdNum, !IO).

robdd_to_dot(Constraint, ProgVarSet, MCI, FileName, !IO) :-
    VarMap = MCI ^ mci_varmap,
    P = (pred(RobddVar::in, di, uo) is det -->
        { bimap.reverse_lookup(VarMap, key(RepVar, PredId, LambdaId),
            RobddVar) },
        dump_mode_constraint_var(ProgVarSet, RepVar),
        io.write_string(" "),
        { pred_id_to_int(PredId, PredIdNum) },
        io.write_int(PredIdNum),
        io.write_string(" "),
        io.write_int(stack.depth(LambdaId)),
        io.write_string(" ("),
        io.write_int(term.var_to_int(RobddVar)),
        io.write_string(")")
    ),
    robdd_to_dot(Constraint ^ robdd, P, FileName, !IO).

%-----------------------------------------------------------------------------%

atomic_prodvars_map(Constraint, MCI) = ProdVarsMap :-
    ( some_vars(VarsEntailed) = vars_entailed(ensure_normalised(Constraint)) ->
        list.foldl(
            (pred(MCVar::in, PVM0::in, PVM::out) is det :-
                (
                    bimap.reverse_lookup(MCI ^ mci_varmap, Key, MCVar),
                    Key = key(RepVar, PredId, LambdaId0),
                    PredId = MCI ^ mci_pred_id,
                    RepVar = ProgVar `at` GoalId,
                    stack.push(GoalId, LambdaId0, LambdaId)
                ->
                    ( map.search(PVM0, LambdaId, Vs0) ->
                        set_of_var.insert(ProgVar, Vs0, Vs),
                        map.det_update(LambdaId, Vs, PVM0, PVM)
                    ;
                        set_of_var.make_singleton(ProgVar, Vs),
                        map.det_insert(LambdaId, Vs, PVM0, PVM)
                    )
                ;
                    PVM = PVM0
                )
            ), sparse_bitset.to_sorted_list(VarsEntailed),
            map.init, ProdVarsMap)
    ;
        unexpected($pred, "zero constraint")
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.mode_constraint_robdd.
%-----------------------------------------------------------------------------%
