%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: unused_args_base_ops.m.
%
% This module contains the main data structure we use to compute
% the set of unused arguments in each procedure, and all the basic
% operations on that data structure.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.unused_args_base_ops.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module map.
:- import_module set.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The types we use to track which arguments may possibly be unused,
% and the operations on them.
%

    % A collection of the local_var_usage_map structures of each procedure.
:- type global_var_usage_map == map(pred_proc_id, local_var_usage_map).

    % Values of this type map variables in a procedure that are
    % not yet known to be used to their aliases. When we find out that
    % either the variable, or one of its aliases, is used, we delete
    % the variable from the map. The absence of the variable from the map
    % implies that the variable is used.
    %
    % XXX Document exactly what set of variables ever get put into this map.
    % Is it just the variables representing the input args of the procedure
    % we are analyzing, or do other variables, such as those aliases,
    % get put in here as well?
:- type local_var_usage_map == map(prog_var, required_by).

    % For each variable Var that is not yet definitely known to be used,
    % we record information about the set of variables whose computation
    % requires the value of Var. We do this because if any of those
    % requiring vars is used, then Var is used as well, since it will be used
    % to compute them.
    %
    % We record this set of variables in two parts: a set of local variables,
    % and a set of procedure argument variables. Note that the second set
    % *may* refer to the arguments of the procedure in which Var occurs.
    %
    % The init_global_var_usage_map pass adds entries to each of these sets
    % as required. After that pass is finished, the rest of our algorithm,
    % implemented by record_required_vars_as_used_to_fixpoint, can and will
    % delete a variable's whole entry from its procedure's local_var_usage_map
    % when it decides that the variable is definely USED. On the other hand,
    % our algorithm can never decide that a variable is definitely UNUSED
    % until record_required_vars_as_used_to_fixpoint is finished. And by then,
    % only the absence or presence of a required_by entry for a variable in
    % the local_var_usage_map matters; the contents of any required_by entry
    % are no longer relevant. This is why we never actually delete
    % any elements from these two sets.
:- type required_by
    --->    required_by(
                % The set of requiring local variables.
                set(prog_var),

                % The set of requiring procedure argument variables.
                set(arg_var_in_proc)
            ).

    % We identify a specific argument of a procedure by storing ...
:- type arg_var_in_proc
    --->    arg_var_in_proc(
                % ... the identity of the procedure, and ...
                pred_proc_id,

                % ... the identity of the variable that represents that
                % argument in the list of head variables of that procedure
                % (as returned by proc_info_get_headvars). This means that
                % this prog_var is NOT in the varset of the procedure
                % whose whole BODY GOAL we are analyzing, but in the varset
                % of the procedure that is the CALLEE of the call we are
                % processing.
                %
                % Simon chose this representation over an argument number.
                % Both require a translation from the caller to the callee's
                % context: the prog_var representation requires it when an
                % arg_var_in_proc structure is created, while the argument
                % number representation requires it when they are used.
                % The prog_var representation looks simpler, but it is also
                % more error-prone, because the compiler cannot help detect
                % confusing a local variable for a variable in another
                % procedure, or vice versa.
                prog_var
            ).

%---------------------------------------------------------------------------%

    % Add requirements of local vars.
    %
:- pred local_var_is_required_by_local_vars(prog_var::in, list(prog_var)::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.
:- pred local_vars_are_required_by_local_var(list(prog_var)::in, prog_var::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

    % Add requirements of procedure arguments.
    %
:- pred local_var_is_required_by_proc_arg(prog_var::in, arg_var_in_proc::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.
:- pred local_vars_are_required_by_proc_arg(list(prog_var)::in,
    arg_var_in_proc::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

%---------------------------------------------------------------------------%

    % Record that variables are used.
    %
:- pred record_vars_as_used(list(prog_var)::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.
:- pred record_var_as_used(prog_var::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

%---------------------------------------------------------------------------%

    % Succeed if and only if the given argument variable of the
    % given procedure is *definitely* used, and our record of it is not just
    % "it is used *if* some of these *other* variables are used".
    %
:- pred proc_arg_var_is_used(global_var_usage_map::in, pred_proc_id::in,
    prog_var::in) is semidet.

    % Succeed if and only if the given local variable is *definitely* used,
    % and our record of it not just "it is used *if* these *other* variables
    % are used".
    %
:- pred local_var_is_used(local_var_usage_map::in, prog_var::in) is semidet.

%---------------------------------------------------------------------------%

    % This predicate can help debug the code of this module.
    %
:- pred write_global_var_usage_map(io.text_output_stream::in, module_info::in,
    global_var_usage_map::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred remove_specified_positions(list(int)::in,
    list(T)::in, list(T)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.var_table.

:- import_module int.
:- import_module pair.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

local_var_is_required_by_local_vars(LocalVar, NewRequiringVars,
        !LocalVarUsageMap) :-
    ( if map.search(!.LocalVarUsageMap, LocalVar, RequiredBy0) then
        RequiredBy0 = required_by(RequiringLocalVars0, RequiringProcArgs),
        set.insert_list(NewRequiringVars,
            RequiringLocalVars0, RequiringLocalVars),
        RequiredBy = required_by(RequiringLocalVars, RequiringProcArgs),
        map.det_update(LocalVar, RequiredBy, !LocalVarUsageMap)
    else
        true
    ).

local_vars_are_required_by_local_var([], _, !LocalVarUsageMap).
local_vars_are_required_by_local_var([LocalVar | LocalVars], RequiringVar,
        !LocalVarUsageMap) :-
    ( if map.search(!.LocalVarUsageMap, LocalVar, RequiredBy0) then
        RequiredBy0 = required_by(RequiringLocalVars0, RequiringProcArgs),
        set.insert(RequiringVar, RequiringLocalVars0, RequiringLocalVars),
        RequiredBy = required_by(RequiringLocalVars, RequiringProcArgs),
        map.det_update(LocalVar, RequiredBy, !LocalVarUsageMap)
    else
        true
    ),
    local_vars_are_required_by_local_var(LocalVars, RequiringVar,
        !LocalVarUsageMap).

%---------------------------------------------------------------------------%

local_var_is_required_by_proc_arg(LocalVar, ArgVarInProc, !LocalVarUsageMap) :-
    ( if map.search(!.LocalVarUsageMap, LocalVar, RequiredBy0) then
        RequiredBy0 = required_by(RequiringLocalVars, RequiringProcArgs0),
        set.insert(ArgVarInProc, RequiringProcArgs0, RequiringProcArgs),
        RequiredBy = required_by(RequiringLocalVars, RequiringProcArgs),
        map.det_update(LocalVar, RequiredBy, !LocalVarUsageMap)
    else
        true
    ).

local_vars_are_required_by_proc_arg([], _ArgVarInProc, !LocalVarUsageMap).
local_vars_are_required_by_proc_arg([LocalVar | LocalVars], ArgVarInProc,
        !LocalVarUsageMap) :-
    local_var_is_required_by_proc_arg(LocalVar, ArgVarInProc,
        !LocalVarUsageMap),
    local_vars_are_required_by_proc_arg(LocalVars, ArgVarInProc,
        !LocalVarUsageMap).

%---------------------------------------------------------------------------%

record_vars_as_used(Vars, !LocalVarUsageMap) :-
    map.delete_list(Vars, !LocalVarUsageMap).

record_var_as_used(Var, !LocalVarUsageMap) :-
    map.delete(Var, !LocalVarUsageMap).

%---------------------------------------------------------------------------%

proc_arg_var_is_used(GlobalVarUsageMap, PredProcId, Var) :-
    % Note that GlobalVarUsageMap will have required_by variable entries
    % for local procedures that mention non-local procedures, which
    % do *not* occur in GlobalVarUsageMap as keys. This is why calling
    % map.lookup on GlobalVarUsageMap would not work.
    not (
        map.search(GlobalVarUsageMap, PredProcId, LocalVarUsageMap),
        map.contains(LocalVarUsageMap, Var)
    ).

local_var_is_used(LocalVarUsageMap, Var) :-
    not map.contains(LocalVarUsageMap, Var).

%---------------------------------------------------------------------------%

write_global_var_usage_map(Stream, ModuleInfo, GlobalVarUsageMap, !IO) :-
    map.foldl(write_local_var_usage_map(Stream, ModuleInfo),
        GlobalVarUsageMap, !IO).

:- pred write_local_var_usage_map(io.text_output_stream::in, module_info::in,
    pred_proc_id::in, local_var_usage_map::in, io::di, io::uo) is det.

write_local_var_usage_map(Stream, ModuleInfo, PredProcId,
        LocalVarUsageMap, !IO) :-
    PredProcIdStr = pred_proc_id_to_dev_string(ModuleInfo, PredProcId),
    io.format(Stream, "\n%s:\n", [s(PredProcIdStr)], !IO),
    map.to_assoc_list(LocalVarUsageMap, LocalVarUsages),
    module_info_proc_info(ModuleInfo, PredProcId, ProcInfo),
    proc_info_get_var_table(ProcInfo, VarTable),
    list.foldl2(
        write_var_requiring_vars(Stream, ModuleInfo, VarTable), LocalVarUsages,
        [], RevNotRequiredVars, !IO),
    list.reverse(RevNotRequiredVars, NotRequiredVars),
    (
        NotRequiredVars = []
    ;
        NotRequiredVars = [_ | _],
        NotRequiredVarsStr = mercury_vars_to_string(VarTable,
            print_name_and_num, NotRequiredVars),
        % NotRequiredVars lists the variables that have both
        % - an empty set of requiring vars, and
        % - an empty set of requiring proc arguments.
        % init_global_var_usage_map_entry_for_proc initializes the local var
        % usage map entry of every variable in the procedure's var table
        % to this value. If the traversal of the procedure body neither
        % added any new elements to either set, nor deleted the variable's
        % entry, then the variable is not mentioned at all in the body.
        % This can happen if an earlier pass, such as simplification,
        % deleted the last reference to the variable from the body, but
        % left the variable in the var table.
        io.format(Stream, "  not required but present vars: %s\n",
            [s(NotRequiredVarsStr)], !IO)
    ),
    io.nl(Stream, !IO).

:- pred write_var_requiring_vars(io.text_output_stream::in, module_info::in,
    var_table::in, pair(prog_var, required_by)::in,
    list(prog_var)::in, list(prog_var)::out, io::di, io::uo) is det.

write_var_requiring_vars(Stream, ModuleInfo, VarTable, Var - RequiringVars,
        !RevNotRequiredVars, !IO) :-
    RequiringVars = required_by(LocalVarSet, ArgVarInProcsSet),
    set.to_sorted_list(LocalVarSet, LocalVars),
    set.to_sorted_list(ArgVarInProcsSet, ArgVarsInProcs),
    ( if LocalVars = [], ArgVarsInProcs = [] then
        !:RevNotRequiredVars = [Var | !.RevNotRequiredVars]
    else
        VarStr = mercury_var_to_string(VarTable, print_name_and_num, Var),
        io.format(Stream, "  requiring vars of %s:\n", [s(VarStr)], !IO),
        (
            LocalVars = []
        ;
            LocalVars = [_ | _],
            LocalVarsStr = mercury_vars_to_string(VarTable,
                print_name_and_num, LocalVars),
            io.format(Stream, "    variables: %s\n", [s(LocalVarsStr)], !IO)
        ),
        (
            ArgVarsInProcs = []
        ;
            ArgVarsInProcs = [_ | _],
            io.write_string(Stream, "    procedure arguments:\n", !IO),
            list.foldl(write_arg_var_in_proc(Stream, ModuleInfo),
                ArgVarsInProcs, !IO)
        )
    ).

:- pred write_arg_var_in_proc(io.text_output_stream::in, module_info::in,
    arg_var_in_proc::in, io::di, io::uo) is det.

write_arg_var_in_proc(Stream, ModuleInfo, ArgVarInProc, !IO) :-
    ArgVarInProc = arg_var_in_proc(PredProcId, Var),
    PredProcIdStr = pred_proc_id_to_dev_string(ModuleInfo, PredProcId),
    module_info_proc_info(ModuleInfo, PredProcId, ProcInfo),
    proc_info_get_var_table(ProcInfo, VarTable),
    VarStr = mercury_var_to_string(VarTable, print_name_and_num, Var),
    io.format(Stream, "      %s, %s\n", [s(PredProcIdStr), s(VarStr)], !IO).

%---------------------------------------------------------------------------%

remove_specified_positions(ArgNumsToRemove, !List) :-
    remove_specified_positions_loop(ArgNumsToRemove, 1, !List).

:- pred remove_specified_positions_loop(list(int)::in, int::in,
    list(T)::in, list(T)::out) is det.

remove_specified_positions_loop(_ArgNumsToRemove, _ArgNum,
        List0 @ [], List0).
remove_specified_positions_loop(ArgNumsToRemove, ArgNum,
        List0 @ [Head0 | Tail0], List) :-
    (
        ArgNumsToRemove = [],
        List = List0
    ;
        ArgNumsToRemove = [_ | _],
        remove_specified_positions_loop(ArgNumsToRemove, ArgNum + 1,
            Tail0, Tail),
        ( if list.member(ArgNum, ArgNumsToRemove) then
            List = Tail
        else
            List = [Head0 | Tail]
        )
    ).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.unused_args_base_ops.
%---------------------------------------------------------------------------%
