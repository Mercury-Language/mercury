%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2007 The University of Melbourne.
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

:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.
:- import_module mode_robdd.

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
:- func 'pred_id :='(mode_constraint_info, pred_id) = mode_constraint_info.

:- type rep_var
    --->    in(prog_var)
    ;   out(prog_var)
    ;   prog_var `at` goal_path.

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

:- pred enter_lambda_goal(goal_path::in, mode_constraint_info::in,
    mode_constraint_info::out) is det.

:- pred leave_lambda_goal(mode_constraint_info::in, mode_constraint_info::out)
    is det.

    % lambda_path extends the idea of the goal_path to allow describing the
    % location of a goal within nested lambda goals.
:- type lambda_path == stack(goal_path).

    % Describes a var, its pred-id and lambda-nesting level.
    % XXX think up a better name for this.
:- type prog_var_and_level.

:- pred get_prog_var_level(prog_var::in, prog_var_and_level::out,
    mode_constraint_info::in, mode_constraint_info::out) is det.

:- pred set_level_from_var(prog_var_and_level::in,
    mode_constraint_info::in, mode_constraint_info::out) is det.

    % Return the current max var for later use by restrict_threshold.
:- pred save_threshold(threshold::out, mode_constraint_info::in,
    mode_constraint_info::out) is det.

:- func restrict_threshold(threshold, mode_constraint) = mode_constraint.

:- func restrict_filter(pred(rep_var), mode_constraint_info,
    mode_constraint) = mode_constraint.
:- mode restrict_filter(pred(in) is semidet, in, in) = out is det.

:- pred save_min_var_for_pred(pred_id::in, mode_constraint_info::in,
    mode_constraint_info::out) is det.

:- pred save_max_var_for_pred(pred_id::in, mode_constraint_info::in,
    mode_constraint_info::out) is det.

:- pred get_interesting_vars_for_pred(pred_id::in,
    set(mode_constraint_var)::out, mode_constraint_info::in,
    mode_constraint_info::out) is det.

    % Set the input_nodes field of the mode_constraint_info and make sure
    % the zero_var is constrained to be zero in the mode_constraint.
:- pred set_input_nodes(mode_constraint::in, mode_constraint::out,
    mode_constraint_info::in, mode_constraint_info::out) is det.

:- pred set_simple_mode_constraints(mode_constraint_info::in,
    mode_constraint_info::out) is det.

:- pred unset_simple_mode_constraints(mode_constraint_info::in,
    mode_constraint_info::out) is det.

:- pred using_simple_mode_constraints(mode_constraint_info::in,
    mode_constraint_info::out) is semidet.

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

:- type prodvars_map == map(lambda_path, set(prog_var)).

:- func atomic_prodvars_map(mode_constraint, mode_constraint_info) =
    prodvars_map.

:- implementation.

:- import_module hlds.hlds_goal.
% :- import_module mode_robdd.tfeir.
:- import_module mode_robdd.tfeirn.
% :- import_module mode_robdd.check.

:- import_module bimap.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module robdd.
:- import_module solutions.
:- import_module sparse_bitset.
:- import_module stack.
:- import_module std_util.
:- import_module term.
:- import_module varset.

:- type mc_type ---> mc_type.

:- type mode_constraint_info --->
    mode_constraint_info(
        varset      :: varset(mc_type),
        varmap      :: mode_constraint_varmap,
        pred_id     :: pred_id,
        lambda_path :: lambda_path,
        min_vars    :: map(pred_id, mode_constraint_var),
        max_vars    :: map(pred_id, mode_constraint_var),
        input_nodes :: sparse_bitset(prog_var),
        zero_var    :: robdd_var,
                % A var that is always zero.
        simple_constraints :: bool
                % Are we using the simplified constraint model.
    ).

:- type threshold ---> threshold(mode_constraint_var).

init_mode_constraint_info(Simple) = Info :-
    VarSet0 = varset.init,
    varset.new_var(VarSet0, ZeroVar, VarSet),
    PredId = hlds_pred.initial_pred_id,
    Info = mode_constraint_info(VarSet, bimap.init, PredId, stack.init,
        map.init, map.init, sparse_bitset.init, ZeroVar, Simple).

:- type robdd_var == var(mc_type).

:- type mode_constraint_varmap == bimap(varmap_key, robdd_var).

    % Key for looking up robdd_vars.
    % `pred_id' is the predicate the variable belongs to.
    % `lambda_path' describes the location of the lambda_goal
    % we are referring to.
:- type varmap_key ---> key(rep_var, pred_id, lambda_path).

mode_constraint_var(RepVar0, RobddVar, Info0, Info) :-
    mode_constraint_var(Info0 ^ pred_id, RepVar0, RobddVar, Info0, Info).

mode_constraint_var(PredId, RepVar0, RobddVar, Info0, Info) :-
    (
        RepVar0 = ProgVar `at` _,
        Info0 ^ input_nodes `contains` ProgVar
    ->
        % This RepVar must be false since the corresponding input var
        % is true.  We can just return the zero var.
        RobddVar = Info0 ^ zero_var,
        Info = Info0
    ;
        RepVar = RepVar0,
        LambdaPath = Info0 ^ lambda_path,
        Key = key(RepVar, PredId, LambdaPath),
        ( bimap.search(Info0 ^ varmap, Key, RobddVar0) ->
            RobddVar = RobddVar0,
            Info = Info0
        ;
            varset.new_var(Info0 ^ varset, RobddVar, NewVarSet),
            bimap.set(Info0 ^ varmap, Key, RobddVar, NewVarMap),
            Info = (Info0 ^ varset := NewVarSet)
                ^ varmap := NewVarMap
        )
    ).

mode_constraint_var(Info, RepVar) = bimap.lookup(Info ^ varmap, Key) :-
    Key = key(RepVar, Info ^ pred_id, Info ^ lambda_path).

enter_lambda_goal(GoalPath) -->
    LambdaPath0 =^ lambda_path,
    ^ lambda_path := stack.push(LambdaPath0, GoalPath).

leave_lambda_goal -->
    LambdaPath0 =^ lambda_path,
    { stack.pop_det(LambdaPath0, _GoalPath, LambdaPath) },
    ^ lambda_path := LambdaPath.

:- type prog_var_and_level
    --->    prog_var_and_level(
            prog_var,
            pred_id,
            lambda_path
        ).

get_prog_var_level(Var, prog_var_and_level(Var, PredId, LambdaPath)) -->
    PredId =^ pred_id,
    LambdaPath =^ lambda_path.

set_level_from_var(prog_var_and_level(_Var, PredId, LambdaPath)) -->
    ^ pred_id := PredId,
    ^ lambda_path := LambdaPath.

save_threshold(threshold(varset.max_var(VarSet))) -->
    VarSet =^ varset.

restrict_threshold(threshold(Threshold), Constraint) =
    restrict_threshold(Threshold, ensure_normalised(Constraint)).

restrict_filter(P0, Info, M) = restrict_filter(P, ensure_normalised(M)) :-
    P = (pred(MCV::in) is semidet :-
        bimap.reverse_lookup(Info ^ varmap, key(RV, PredId, _), MCV),
        ( PredId \= Info ^ pred_id ; P0(RV) )
    ).

save_min_var_for_pred(PredId) -->
    save_threshold(threshold(Threshold)),
    MinVars0 =^ min_vars,
    { map.set(MinVars0, PredId, Threshold, MinVars) },
    ^ min_vars := MinVars.

save_max_var_for_pred(PredId) -->
    save_threshold(threshold(Threshold)),
    MaxVars0 =^ max_vars,
    { map.set(MaxVars0, PredId, Threshold, MaxVars) },
    ^ max_vars := MaxVars.

get_interesting_vars_for_pred(PredId, Vars) -->
    MinVars =^ min_vars,
    MaxVars =^ max_vars,
    VarSet =^ varset,
    { Vars = ( set.sorted_list_to_set `compose`
        list.filter((pred(V::in) is semidet :-
        compare(<, map.lookup(MinVars, PredId), V),
        \+ compare(<, map.lookup(MaxVars, PredId), V))) `compose`
        varset.vars
    )(VarSet) }.

set_input_nodes(Constraint0, Constraint, Info0, Info) :-
    VarMap = Info0 ^ varmap,
    LambdaPath = Info0 ^ lambda_path,
    PredId = Info0 ^ pred_id,
    bimap.ordinates(VarMap, Keys),
    Constraint1 = ensure_normalised(Constraint0),
    solutions.solutions((pred(ProgVar::out) is nondet :-
            list.member(Key, Keys),
            Key = key(in(ProgVar), PredId, LambdaPath),
            bimap.lookup(VarMap, Key, RobddVar),
            var_entailed(Constraint1, RobddVar)
        ), InputNodes),
    Info = Info0 ^ input_nodes := sorted_list_to_set(InputNodes),
    Constraint = Constraint0 ^ not_var(Info ^ zero_var).

set_simple_mode_constraints -->
    ^ simple_constraints := yes.

unset_simple_mode_constraints -->
    ^ simple_constraints := no.

using_simple_mode_constraints -->
    yes =^ simple_constraints.

% dump_mode_constraints(_ModuleInfo, _PredInfo, _InstGraph, ROBDD, Info) -->
%   { AL = (list.sort `compose`
%       assoc_list.reverse_members `compose`
%       bimap.to_assoc_list)(Info ^ varmap) },
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

dump_mode_constraint_var(VarSet, in(V)) -->
    { varset.lookup_name(VarSet, V, Name) },
    io.write_string(Name),
    io.write_string("_in").
dump_mode_constraint_var(VarSet, out(V)) -->
    { varset.lookup_name(VarSet, V, Name) },
    io.write_string(Name),
    io.write_string("_out").
dump_mode_constraint_var(VarSet, V `at` Path0) -->
    { varset.lookup_name(VarSet, V, Name) },
    io.write_string(Name),
    io.write_char('_'),
    { list.reverse(Path0, Path) },
    list.foldl(dump_goal_path_step, Path).

:- pred dump_goal_path_step(goal_path_step::in, io::di, io::uo) is det.

dump_goal_path_step(step_conj(N)) -->
    io.write_char('c'),
    io.write_int(N).
dump_goal_path_step(step_disj(N)) -->
    io.write_char('d'),
    io.write_int(N).
dump_goal_path_step(step_switch(N, _)) -->
    io.write_char('s'),
    io.write_int(N).
dump_goal_path_step(step_ite_cond) -->
    io.write_char('c').
dump_goal_path_step(step_ite_then) -->
    io.write_char('t').
dump_goal_path_step(step_ite_else) -->
    io.write_char('e').
dump_goal_path_step(step_neg) -->
    io.write_char('n').
dump_goal_path_step(step_scope(_)) -->
    io.write_char('q').
dump_goal_path_step(step_first) -->
    io.write_char('f').
dump_goal_path_step(step_later) -->
    io.write_char('l').

robdd_to_dot(Constraint, ProgVarSet, Info, FileName) -->
    robdd_to_dot(Constraint ^ robdd, P, FileName),
    { VarMap = Info ^ varmap },
    { P = (pred(RobddVar::in, di, uo) is det -->
        { bimap.reverse_lookup(VarMap, key(RepVar, PredId, LambdaPath),
            RobddVar) },
        dump_mode_constraint_var(ProgVarSet, RepVar),
        io.write_string(" "),
        { pred_id_to_int(PredId, PredIdNum) },
        io.write_int(PredIdNum),
        io.write_string(" "),
        io.write_int(stack.depth(LambdaPath)),
        io.write_string(" ("),
        io.write_int(term.var_to_int(RobddVar)),
        io.write_string(")")
    )}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

atomic_prodvars_map(Constraint, MCInfo) =
    (
        some_vars(VarsEntailed) =
            vars_entailed(ensure_normalised(Constraint))
    ->
        list.foldl(
            (func(MCVar, PVM) =
                (
                    bimap.reverse_lookup(MCInfo ^ varmap, Key, MCVar),
                    Key = key(RepVar, PredId, LambdaPath0),
                    PredId = MCInfo ^ pred_id,
                    RepVar = ProgVar `at` GoalPath,
                    LambdaPath = stack.push(LambdaPath0, GoalPath)
                ->
                    ( Vs = map.search(PVM, LambdaPath) ->
                    map.det_update(PVM, LambdaPath,
                        Vs `insert` ProgVar)
                    ;
                    map.det_insert(PVM, LambdaPath,
                        make_singleton_set(ProgVar))
                    )
                ;
                    PVM
                )
            ), to_sorted_list(VarsEntailed), map.init)
    ;
        func_error("atomic_prodvars_map: zero constraint")
    ).
