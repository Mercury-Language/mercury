%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: unused_args.m.
% Main author: stayl.
%
% Detects and removes unused input arguments in procedures, especially
% type_infos.
%
% To enable the warnings use `--warn-unused-args'.
% To enable the optimisation use `--optimize-unused-args'.
%
% An argument is considered used if it (or any of its aliases) are
%   - used in a call to a predicate external to the current module
%   - used in a higher-order call
%   - used to instantiate an output variable
%   - involved in a simple test, switch or a semidet deconstruction
%   - used as an argument to another predicate in this module which is used.
%
% When using typeinfo liveness, the following variables are also
% considered used:
%   - a type-info (or part of a type-info) of a type parameter of the
%     type of a variable that is used (for example, if a variable
%     of type list(T) is used, then TypeInfo_for_T is used)
%
% The first step is to determine which arguments of which predicates are used
% locally to their predicate. For each unused argument, a set of other
% arguments that it depends on is built up.
%
% The next step is to iterate over the this map, checking for each unused
% argument whether any of the arguments it depends on has become used in the
% last iteration. Iterations are repeated until a fixpoint is reached.
%
% Warnings are then output. The warning message indicates which arguments are
% used in none of the modes of a predicate.
%
% The predicates are then fixed up. Unused variables and unifications are
% removed.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.unused_args.
:- interface.

:- import_module analysis.
:- import_module hlds.hlds_module.
:- import_module parse_tree.error_util.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred unused_args_process_module(module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Instances used by mmc_analysis.m
%

:- type unused_args_func_info.
:- type unused_args_call.
:- type unused_args_answer.

:- instance analysis(unused_args_func_info, unused_args_call,
    unused_args_answer).

:- instance partial_order(unused_args_func_info, unused_args_call).
:- instance call_pattern(unused_args_func_info, unused_args_call).
:- instance to_term(unused_args_call).

:- instance partial_order(unused_args_func_info, unused_args_answer).
:- instance answer_pattern(unused_args_func_info, unused_args_answer).
:- instance to_term(unused_args_answer).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.file_names.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.mmc_analysis.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % Arguments are stored as their variable id, not their index
    % in the argument vector.
:- type arg_var_in_proc
    --->    arg_var_in_proc(
                pred_proc_id,
                prog_var
            ).

    % Information about the dependencies of a variable that is not known to be
    % used.
    %
:- type usage_info
    --->    unused(set(prog_var), set(arg_var_in_proc)).

    % Contains dependency information for the variables in a procedure
    % that are not yet known to be used.
:- type var_dep == map(prog_var, usage_info).

    % A collection of variable usages for each procedure.
:- type var_usage == map(pred_proc_id, var_dep).

:- type warning_info
    --->    warning_info(prog_context, string, int, list(int)).
            % context, pred name, arity, list of args to warn

%-----------------------------------------------------------------------------%
%
% Types and instances used by mmc_analysis.m
%

:- type unused_args_func_info
    --->    unused_args_func_info(arity).

:- type unused_args_call
    --->    unused_args_call.

:- type unused_args_answer
    --->    unused_args(
                % The list of unused arguments is in sorted order.
                args    :: list(int)
            ).

:- func get_unused_args(unused_args_answer) = list(int).

get_unused_args(UnusedArgs) = UnusedArgs ^ args.

:- instance analysis(unused_args_func_info, unused_args_call,
    unused_args_answer) where
[
    analysis_name(_, _) = analysis_name,
    analysis_version_number(_, _) = 3,
    preferred_fixpoint_type(_, _) = least_fixpoint,
    bottom(unused_args_func_info(Arity), _) = unused_args(1 .. Arity),
    top(_, _) = unused_args([]),
    (get_func_info(ModuleInfo, ModuleName, FuncId, _, _, FuncInfo) :-
        func_id_to_ppid(ModuleInfo, ModuleName, FuncId, proc(PredId, _)),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        Arity = pred_info_orig_arity(PredInfo),
        FuncInfo = unused_args_func_info(Arity)
    )
].

:- func analysis_name = string.

analysis_name = "unused_args".

:- instance call_pattern(unused_args_func_info, unused_args_call) where [].
:- instance partial_order(unused_args_func_info, unused_args_call) where [
    ( more_precise_than(_, _, _) :-
        semidet_fail
    ),
    equivalent(_, Call, Call)
].

:- instance to_term(unused_args_call) where [
    ( to_term(unused_args_call) = Term :-
        Term = term.functor(atom("any"), [], context_init)
    ),
    ( from_term(Term, unused_args_call) :-
        Term = term.functor(atom("any"), [], _)
    )
].

:- instance answer_pattern(unused_args_func_info, unused_args_answer) where [].
:- instance partial_order(unused_args_func_info, unused_args_answer) where [
    (more_precise_than(_, Answer1, Answer2) :-
        Answer1 = unused_args(Args1),
        Answer2 = unused_args(Args2),
        set.subset(sorted_list_to_set(Args2), sorted_list_to_set(Args1))
    ),
    equivalent(_, Args, Args)
].

:- instance to_term(unused_args_answer) where [
    ( to_term(unused_args(Args)) = Term :-
        type_to_term(Args, Term)
    ),
    ( from_term(Term, unused_args(Args)) :-
        term_to_type(Term, Args)
    )
].

%-----------------------------------------------------------------------------%

unused_args_process_module(!ModuleInfo, !Specs, !IO) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    init_var_usage(VarUsage0, PredProcs, ProcCallInfo0, !ModuleInfo),
    % maybe_write_string(VeryVerbose, "% Finished initialisation.\n", !IO),

    unused_args_pass(0, !.ModuleInfo, PredProcs, VarUsage0, VarUsage),
    % maybe_write_string(VeryVerbose, "% Finished analysis.\n", !IO),

    map.init(UnusedArgInfo0),
    get_unused_arg_info(!.ModuleInfo, PredProcs, VarUsage,
        UnusedArgInfo0, UnusedArgInfo),

    map.keys(UnusedArgInfo, PredProcsToFix),
    globals.lookup_bool_option(Globals, make_optimization_interface, MakeOpt),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    % Only write unused argument analysis pragmas to `.opt' files for
    % `--intermodule-optimization', not `--intermodule-analysis'.
    (
        MakeOpt = yes,
        IntermodAnalysis = no
    ->
        module_info_get_name(!.ModuleInfo, ModuleName),
        module_name_to_file_name(Globals, ModuleName, ".opt.tmp",
            do_not_create_dirs, OptFileName, !IO),
        io.open_append(OptFileName, OptFileRes, !IO),
        (
            OptFileRes = ok(OptFile),
            MaybeOptFile = yes(OptFile)
        ;
            OptFileRes = error(IOError),
            io.error_message(IOError, IOErrorMessage),
            io.write_strings(["Cannot open `", OptFileName, "' for output: ",
                IOErrorMessage], !IO),
            io.set_exit_status(1, !IO),
            MaybeOptFile = no
        )
    ;
        MaybeOptFile = no
    ),
    globals.lookup_bool_option(Globals, warn_unused_args, DoWarn),
    (
        ( DoWarn = yes
        ; MakeOpt = yes
        )
    ->
        set.init(WarnedPredIds0),
        output_warnings_and_pragmas(!.ModuleInfo, UnusedArgInfo,
            MaybeOptFile, DoWarn, PredProcsToFix, WarnedPredIds0, !Specs, !IO)
    ;
        true
    ),
    (
        MaybeOptFile = yes(OptFile2),
        io.close_output(OptFile2, !IO)
    ;
        MaybeOptFile = no
    ),
    globals.lookup_bool_option(Globals, make_analysis_registry,
        MakeAnalysisRegistry),
    (
        MakeAnalysisRegistry = yes,
        module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
        module_info_get_valid_predids(PredIds, !ModuleInfo),
        list.foldl(
            maybe_record_intermod_unused_args(!.ModuleInfo, UnusedArgInfo),
            PredIds, AnalysisInfo0, AnalysisInfo1),
        list.foldl(record_intermod_dependencies(!.ModuleInfo),
            PredProcs, AnalysisInfo1, AnalysisInfo),
        module_info_set_analysis_info(AnalysisInfo, !ModuleInfo)
    ;
        MakeAnalysisRegistry = no
    ),
    globals.lookup_bool_option(Globals, optimize_unused_args, DoFixup),
    (
        DoFixup = yes,
        list.foldl2(unused_args_create_new_pred(UnusedArgInfo), PredProcsToFix,
            ProcCallInfo0, ProcCallInfo, !ModuleInfo),
        % maybe_write_string(VeryVerbose, "% Finished new preds.\n",
        %   !IO),
        unused_args_fixup_module(VarUsage, PredProcs, ProcCallInfo,
            !ModuleInfo, VeryVerbose, !IO),
        % maybe_write_string(VeryVerbose, "% Fixed up goals.\n", !IO),
        ( map.is_empty(ProcCallInfo) ->
            true
        ;
            % The dependencies have changed, so the dependency graph is now
            % invalid.
            module_info_clobber_dependency_info(!ModuleInfo)
        )
    ;
        DoFixup = no
    ).

%-----------------------------------------------------------------------------%
%
% Initialisation section
%

    % Set initial status of all args of local procs by examining the
    % module_info. PredProcList is the list of procedures to do the fixpoint
    % iteration over.
    %
:- pred init_var_usage(var_usage::out, pred_proc_list::out,
    proc_call_info::out, module_info::in, module_info::out) is det.

init_var_usage(VarUsage, PredProcList, ProcCallInfo, !ModuleInfo) :-
    map.init(ProcCallInfo0),
    map.init(VarUsage0),
    module_info_get_valid_predids(PredIds, !ModuleInfo),
    setup_local_var_usage(PredIds, VarUsage0, VarUsage, [], PredProcList,
        ProcCallInfo0, ProcCallInfo, !ModuleInfo).

    % Setup args for the whole module.
    %
:- pred setup_local_var_usage(list(pred_id)::in,
    var_usage::in, var_usage::out, pred_proc_list::in, pred_proc_list::out,
    proc_call_info::in, proc_call_info::out,
    module_info::in, module_info::out) is det.

setup_local_var_usage([], !VarUsage, !PredProcs, !OptProcs, !ModuleInfo).
setup_local_var_usage([PredId | PredIds], !VarUsage, !PredProcList, !OptProcs,
        !ModuleInfo) :-
    maybe_setup_pred_args(PredId, !VarUsage, !PredProcList, !OptProcs,
        !ModuleInfo),
    setup_local_var_usage(PredIds, !VarUsage, !PredProcList, !OptProcs,
        !ModuleInfo).

    % Setup args for the given predicate if required.
    %
:- pred maybe_setup_pred_args(pred_id::in, var_usage::in, var_usage::out,
    pred_proc_list::in, pred_proc_list::out,
    proc_call_info::in, proc_call_info::out,
    module_info::in, module_info::out) is det.

maybe_setup_pred_args(PredId, !VarUsage, !PredProcList, !OptProcs,
        !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    (
        % The builtins use all their arguments. We also want to treat stub
        % procedures (those which originally had no clauses) as if they use
        % all of their arguments, to avoid spurious warnings in their callers.
        (
            pred_info_is_builtin(PredInfo)
        ;
            pred_info_get_markers(PredInfo, Markers),
            check_marker(Markers, marker_stub)
        )
    ->
        true
    ;
        ProcIds = pred_info_procids(PredInfo),
        setup_pred_args(PredId, ProcIds, !VarUsage, !PredProcList, !OptProcs,
            !ModuleInfo)
    ).

    % Setup args for each mode of a predicate.
    %
:- pred setup_pred_args(pred_id::in, list(proc_id)::in,
    var_usage::in, var_usage::out, pred_proc_list::in, pred_proc_list::out,
    proc_call_info::in, proc_call_info::out,
    module_info::in, module_info::out) is det.

setup_pred_args(_, [], !VarUsage, !PredProcs, !OptProcs, !ModuleInfo).
setup_pred_args(PredId, [ProcId | ProcIds], !VarUsage,
        !PredProcs, !OptProcs, !ModuleInfo) :-
    setup_proc_args(PredId, ProcId, !VarUsage, !PredProcs,
        !OptProcs, !ModuleInfo),
    setup_pred_args(PredId, ProcIds, !VarUsage, !PredProcs,
        !OptProcs, !ModuleInfo).

    % Setup args for the procedure.
    %
:- pred setup_proc_args(pred_id::in, proc_id::in,
    var_usage::in, var_usage::out, pred_proc_list::in, pred_proc_list::out,
    proc_call_info::in, proc_call_info::out,
    module_info::in, module_info::out) is det.

setup_proc_args(PredId, ProcId, !VarUsage, !PredProcs, !OptProcs,
        !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
        PredInfo, ProcInfo),
    module_info_get_globals(!.ModuleInfo, Globals),
    some [!VarDep] (
        map.init(!:VarDep),
        globals.lookup_bool_option(Globals, intermodule_analysis, Intermod),
        (
            % Don't use the intermodule analysis info when we have the clauses
            % (opt_imported preds) since we may be able to do better with the
            % information in this module.
            Intermod = yes,
            pred_info_is_imported_not_external(PredInfo),
            not is_unify_or_compare_pred(PredInfo)
        ->
            PredModule = pred_info_module(PredInfo),
            PredArity = pred_info_orig_arity(PredInfo),
            FuncInfo = unused_args_func_info(PredArity),
            module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
            module_name_func_id_from_pred_info(PredInfo, ProcId, ModuleId,
                FuncId),
            lookup_best_result(AnalysisInfo0, ModuleId, FuncId,
                FuncInfo, unused_args_call, MaybeBestResult),
            (
                MaybeBestResult = yes(analysis_result(_, BestAnswer, _)),
                BestAnswer = unused_args(UnusedArgs),
                (
                    UnusedArgs = [_ | _],
                    proc_info_get_headvars(ProcInfo, HeadVars),
                    list.map(list.det_index1(HeadVars), UnusedArgs,
                        UnusedVars),
                    initialise_vardep(UnusedVars, !.VarDep, VarDep),
                    PredProcId = proc(PredId, ProcId),
                    map.set(PredProcId, VarDep, !VarUsage),
                    globals.lookup_bool_option(Globals,
                        optimize_unused_args, OptimizeUnusedArgs),
                    (
                        OptimizeUnusedArgs = yes,
                        make_imported_unused_args_pred_info(PredProcId,
                            UnusedArgs, !OptProcs, !ModuleInfo)
                    ;
                        OptimizeUnusedArgs = no
                    )
                ;
                    UnusedArgs = []
                ),
                AnalysisInfo = AnalysisInfo0
            ;
                MaybeBestResult = no,
                record_request(analysis_name, PredModule, FuncId,
                    unused_args_call, AnalysisInfo0, AnalysisInfo)
            ),
            module_info_set_analysis_info(AnalysisInfo, !ModuleInfo)
        ;
            (
                pred_info_is_imported(PredInfo)
            ;
                pred_info_is_pseudo_imported(PredInfo),
                hlds_pred.in_in_unification_proc_id(ProcId)
            ;
                % Unused argument optimization and tabling are
                % not compatible with each other.
                proc_info_get_eval_method(ProcInfo, EvalMethod),
                EvalMethod \= eval_normal
            )
        ->
            true
        ;
            proc_info_get_vartypes(ProcInfo, VarTypes),
            vartypes_vars(VarTypes, Vars),
            initialise_vardep(Vars, !VarDep),
            setup_output_args(!.ModuleInfo, ProcInfo, !VarDep),

            proc_interface_should_use_typeinfo_liveness(PredInfo, ProcId,
                Globals, TypeInfoLiveness),
            (
                TypeInfoLiveness = yes,
                proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
                setup_typeinfo_deps(Vars, VarTypes, proc(PredId, ProcId),
                    RttiVarMaps, !VarDep)
            ;
                TypeInfoLiveness = no
            ),

            proc_info_get_goal(ProcInfo, Goal),
            Info = unused_args_info(!.ModuleInfo, VarTypes),
            unused_args_traverse_goal(Info, Goal, !VarDep),
            map.set(proc(PredId, ProcId), !.VarDep, !VarUsage),

            !:PredProcs = [proc(PredId, ProcId) | !.PredProcs]
        )
    ).

:- pred initialise_vardep(list(prog_var)::in, var_dep::in, var_dep::out)
    is det.

initialise_vardep([], !VarDep).
initialise_vardep([Var | Vars], !VarDep) :-
    set.init(VDep),
    set.init(Args),
    map.set(Var, unused(VDep, Args), !VarDep),
    initialise_vardep(Vars, !VarDep).

%-----------------------------------------------------------------------------%
%
% Predicates for manipulating the var_usage and var_dep structures
%

    % For each variable, ensure the typeinfos describing the type parameters
    % of the type of the variable depend on the head variable. For example,
    % if HeadVar1 has type list(T), then TypeInfo_for_T is used if HeadVar1
    % is used.
    %
:- pred setup_typeinfo_deps(list(prog_var)::in, vartypes::in,
    pred_proc_id::in, rtti_varmaps::in, var_dep::in, var_dep::out) is det.

setup_typeinfo_deps([], _, _, _, !VarDep).
setup_typeinfo_deps([Var | Vars], VarTypeMap, PredProcId, RttiVarMaps,
        !VarDep) :-
    setup_typeinfo_dep(Var, VarTypeMap, PredProcId, RttiVarMaps, !VarDep),
    setup_typeinfo_deps(Vars, VarTypeMap, PredProcId, RttiVarMaps, !VarDep).

:- pred setup_typeinfo_dep(prog_var::in, vartypes::in,
    pred_proc_id::in, rtti_varmaps::in, var_dep::in, var_dep::out) is det.

setup_typeinfo_dep(Var, VarTypeMap, PredProcId, RttiVarMaps, !VarDep) :-
    lookup_var_type(VarTypeMap, Var, Type),
    type_vars(Type, TVars),
    list.map(tvar_to_type_info_var(RttiVarMaps), TVars, TypeInfoVars),
    list.foldl(add_rev_arg_dep(Var, PredProcId), TypeInfoVars, !VarDep).

:- pred tvar_to_type_info_var(rtti_varmaps::in, tvar::in, prog_var::out)
    is det.

tvar_to_type_info_var(RttiVarMaps, TVar, TypeInfoVar) :-
    rtti_lookup_type_info_locn(RttiVarMaps, TVar, Locn),
    type_info_locn_var(Locn, TypeInfoVar).

    % Get output arguments for a procedure given the headvars and the
    % argument modes, and set them as used.
    %
:- pred setup_output_args(module_info::in, proc_info::in,
    var_dep::in, var_dep::out) is det.

setup_output_args(ModuleInfo, ProcInfo, !VarDep) :-
    proc_info_instantiated_head_vars(ModuleInfo, ProcInfo,
        ChangedInstHeadVars),
    list.foldl(set_var_used, ChangedInstHeadVars, !VarDep).

    % Searches for the dependencies of a variable, succeeds if the variable
    % is definitely used.
    %
:- pred var_is_used(pred_proc_id::in, prog_var::in, var_usage::in) is semidet.

var_is_used(PredProc, Var, VarUsage) :-
    \+ (
        map.search(VarUsage, PredProc, UsageInfos),
        map.contains(UsageInfos, Var)
    ).

:- pred local_var_is_used(var_dep::in, prog_var::in) is semidet.

local_var_is_used(VarDep, Var) :-
    \+ map.contains(VarDep, Var).

    % Add a list of aliases for a variable.
    %
:- pred add_aliases(prog_var::in, list(prog_var)::in,
    var_dep::in, var_dep::out) is det.

add_aliases(Var, Aliases, !VarDep) :-
    ( map.search(!.VarDep, Var, VarInf0) ->
        VarInf0 = unused(VarDep0, ArgDep),
        set.insert_list(Aliases, VarDep0, VarDep),
        VarInf = unused(VarDep, ArgDep),
        map.det_update(Var, VarInf, !VarDep)
    ;
        true
    ).

:- pred set_list_vars_used(list(prog_var)::in, var_dep::in, var_dep::out)
    is det.

set_list_vars_used(Vars, !VarDep) :-
    map.delete_list(Vars, !VarDep).

:- pred set_var_used(prog_var::in, var_dep::in, var_dep::out) is det.

set_var_used(Var, !VarDep) :-
    map.delete(Var, !VarDep).

:- pred lookup_local_var(var_dep::in, prog_var::in, usage_info::out)
    is semidet.

lookup_local_var(VarDep, Var, UsageInfo) :-
    map.search(VarDep, Var, UsageInfo).

%-----------------------------------------------------------------------------%
%
% Traversal of goal structure, building up dependencies for all variables
%

:- type unused_args_info
    --->    unused_args_info(
                unarg_module_info   :: module_info,
                unarg_vartypes      :: vartypes
            ).

:- pred unused_args_traverse_goal(unused_args_info::in, hlds_goal::in,
    var_dep::in, var_dep::out) is det.

unused_args_traverse_goal(Info, Goal, !VarDep) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = conj(_ConjType, Goals),
        unused_args_traverse_goals(Info, Goals, !VarDep)
    ;
        GoalExpr = disj(Goals),
        unused_args_traverse_goals(Info, Goals, !VarDep)
    ;
        GoalExpr = switch(Var, _, Cases),
        set_var_used(Var, !VarDep),
        list_case_to_list_goal(Cases, Goals),
        unused_args_traverse_goals(Info, Goals, !VarDep)
    ;
        GoalExpr = plain_call(PredId, ProcId, Args, _, _, _),
        module_info_pred_proc_info(Info ^ unarg_module_info, PredId, ProcId,
            _PredInfo, ProcInfo),
        proc_info_get_headvars(ProcInfo, HeadVars),
        add_pred_call_arg_dep(proc(PredId, ProcId), Args, HeadVars, !VarDep)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        unused_args_traverse_goal(Info, Cond, !VarDep),
        unused_args_traverse_goal(Info, Then, !VarDep),
        unused_args_traverse_goal(Info, Else, !VarDep)
    ;
        GoalExpr = negation(SubGoal),
        unused_args_traverse_goal(Info, SubGoal, !VarDep)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( Reason = from_ground_term(_TermVar, from_ground_term_construct) ->
            % What we do here is what we would do for a construction
            % unification that binds TermVar to a constant, i.e. nothing.
            true
        ;
            % XXX We could treat from_ground_term_deconstruct specially
            % as well.
            unused_args_traverse_goal(Info, SubGoal, !VarDep)
        )
    ;
        GoalExpr = generic_call(GenericCall, Args, _, _, _),
        goal_util.generic_call_vars(GenericCall, CallArgs),
        set_list_vars_used(CallArgs, !VarDep),
        set_list_vars_used(Args, !VarDep)
    ;
        GoalExpr = call_foreign_proc(_, _, _, Args, ExtraArgs, _, _),
        % Only arguments with names can be used in the foreign code.  The code
        % in here should be kept in sync with the treatment of foreign_procs
        % in unused_args_fixup_goal_expr: any variable considered unused here
        % should be renamed apart in unused_args_fixup_goal_expr.
        ArgIsUsed = (pred(Arg::in, Var::out) is semidet :-
            Arg = foreign_arg(Var, MaybeNameAndMode, _, _),
            MaybeNameAndMode = yes(_)
        ),
        list.filter_map(ArgIsUsed, Args ++ ExtraArgs, UsedVars),
        set_list_vars_used(UsedVars, !VarDep)
    ;
        GoalExpr = unify(LHS, RHS, _, Unify, _),
        (
            Unify = simple_test(Var1, Var2),
            set_var_used(Var1, !VarDep),
            set_var_used(Var2, !VarDep)
        ;
            Unify = assign(Target, Source),
            ( local_var_is_used(!.VarDep, Target) ->
                % If Target is used to instantiate an output argument,
                % Source is used.
                set_var_used(Source, !VarDep)
            ;
                add_aliases(Source, [Target], !VarDep)
            )
        ;
            Unify = deconstruct(CellVar, _, Args, Modes, CanFail, _),
            expect(unify(CellVar, LHS), $module, $pred, "LHS != CellVar"),
            partition_deconstruct_args(Info, Args, Modes,
                InputVars, OutputVars),
            % The deconstructed variable is used if any of the variables that
            % the deconstruction binds are used.
            add_aliases(CellVar, OutputVars, !VarDep),
            % Treat a deconstruction that further instantiates its left arg
            % as a partial construction.
            add_construction_aliases(CellVar, InputVars, !VarDep),
            (
                CanFail = can_fail,
                % A deconstruction that can_fail uses its left arg.
                set_var_used(CellVar, !VarDep)
            ;
                CanFail = cannot_fail
            )
        ;
            Unify = construct(CellVar, _, Args, _, _, _, _),
            expect(unify(CellVar, LHS), $module, $pred, "LHS != CellVar"),
            ( local_var_is_used(!.VarDep, CellVar) ->
                set_list_vars_used(Args, !VarDep)
            ;
                add_construction_aliases(CellVar, Args, !VarDep)
            )
        ;
            Unify = complicated_unify(_, _, _),
            % These should be transformed into calls by polymorphism.m.
            % This is here to cover the case where unused arguments is called
            % with --error-check-only and polymorphism has not been run.
            (
                RHS = rhs_var(RHSVar),
                set_var_used(RHSVar, !VarDep),
                set_var_used(LHS, !VarDep)
            ;
                ( RHS = rhs_functor(_, _, _)
                ; RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _, _)
                ),
                unexpected($module, $pred,
                    "complicated unifications should only be var-var")
            )
        )
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($module, $pred, "shorthand")
    ).

    % Add PredProc - HeadVar as an alias for the same element of Args.
    %
:- pred add_pred_call_arg_dep(pred_proc_id::in, list(prog_var)::in,
    list(prog_var)::in, var_dep::in, var_dep::out) is det.

add_pred_call_arg_dep(PredProc, LocalArguments, HeadVarIds, !VarDep) :-
    (
        LocalArguments = [Arg | Args],
        HeadVarIds = [HeadVar | HeadVars]
    ->
        add_arg_dep(Arg, PredProc, HeadVar, !VarDep),
        add_pred_call_arg_dep(PredProc, Args, HeadVars, !VarDep)
    ;
        LocalArguments = [],
        HeadVarIds = []
    ->
        true
    ;
        unexpected($module, $pred, "invalid call")
    ).

:- pred add_arg_dep(prog_var::in, pred_proc_id::in, prog_var::in,
    var_dep::in, var_dep::out) is det.

add_arg_dep(Var, PredProc, Arg, !VarDep) :-
    ( lookup_local_var(!.VarDep, Var, VarUsage0) ->
        VarUsage0 = unused(VarDep, ArgDep0),
        set.insert(arg_var_in_proc(PredProc, Arg), ArgDep0, ArgDep),
        VarUsage = unused(VarDep, ArgDep),
        map.det_update(Var, VarUsage, !VarDep)
    ;
        true
    ).

:- pred add_rev_arg_dep(prog_var::in, pred_proc_id::in, prog_var::in,
    var_dep::in, var_dep::out) is det.

add_rev_arg_dep(Var, PredProcId, Arg, !VarDep) :-
    add_arg_dep(Arg, PredProcId, Var, !VarDep).

    % Partition the arguments to a deconstruction into inputs
    % and outputs.
    %
:- pred partition_deconstruct_args(unused_args_info::in, list(prog_var)::in,
    list(uni_mode)::in, list(prog_var)::out, list(prog_var)::out) is det.

partition_deconstruct_args(Info, ArgVars, ArgModes, InputVars, OutputVars) :-
    (
        ArgVars = [Var | Vars],
        ArgModes = [Mode | Modes]
    ->
        partition_deconstruct_args(Info, Vars, Modes, InputVars1, OutputVars1),
        Mode = ((InitialInst1 - InitialInst2) -> (FinalInst1 - FinalInst2)),

        lookup_var_type(Info ^ unarg_vartypes, Var, Type),

        % If the inst of the argument of the LHS is changed,
        % the argument is input.
        (
            inst_matches_binding(InitialInst1, FinalInst1,
                Type, Info ^ unarg_module_info)
        ->
            InputVars = InputVars1
        ;
            InputVars = [Var | InputVars1]
        ),

        % If the inst of the argument of the RHS is changed,
        % the argument is output.
        (
            inst_matches_binding(InitialInst2, FinalInst2, Type,
                Info ^ unarg_module_info)
        ->
            OutputVars = OutputVars1
        ;
            OutputVars = [Var | OutputVars1]
        )
    ;
        ArgVars = [],
        ArgModes = []
    ->
        InputVars = [],
        OutputVars = []
    ;
        unexpected($module, $pred, "mismatched lists")
    ).

    % Add Alias as an alias for all of Vars.
    %
:- pred add_construction_aliases(prog_var::in, list(prog_var)::in,
    var_dep::in, var_dep::out) is det.

add_construction_aliases(_, [], !VarDep).
add_construction_aliases(Alias, [Var | Vars], !VarDep) :-
    ( lookup_local_var(!.VarDep, Var, VarInfo0) ->
        VarInfo0 = unused(VarDep0, ArgDep),
        set.insert(Alias, VarDep0, VarDep),
        VarInfo = unused(VarDep, ArgDep),
        map.set(Var, VarInfo, !VarDep)
    ;
        true
    ),
    add_construction_aliases(Alias, Vars, !VarDep).

:- pred list_case_to_list_goal(list(case)::in, list(hlds_goal)::out) is det.

list_case_to_list_goal([], []).
list_case_to_list_goal([case(_, _, Goal) | Cases], [Goal | Goals]) :-
    list_case_to_list_goal(Cases, Goals).

:- pred unused_args_traverse_goals(unused_args_info::in, list(hlds_goal)::in,
    var_dep::in, var_dep::out) is det.

unused_args_traverse_goals(_, [], !VarDep).
unused_args_traverse_goals(Info, [Goal | Goals], !VarDep) :-
    unused_args_traverse_goal(Info, Goal, !VarDep),
    unused_args_traverse_goals(Info, Goals, !VarDep).

%-----------------------------------------------------------------------------%
%
% Analysis section - do the fixpoint iteration
%

    % Do a full iteration, check if anything changed, if so, repeat.
    %
:- pred unused_args_pass(int::in, module_info::in, pred_proc_list::in,
    var_usage::in, var_usage::out) is det.

unused_args_pass(PassNum, ModuleInfo, LocalPredProcs, !VarUsage) :-
    unused_args_single_pass(LocalPredProcs, no, Changed, !VarUsage),
    (
        Changed = yes,
        trace [compile_time(flag("unused_args_var_usage")), io(!IO)] (
            io.format("\nVARIABLE USAGE MAP AFTER PASS %d\n", [i(PassNum)],
                !IO),
            write_var_usage_map(ModuleInfo, !.VarUsage, !IO)
        ),
        unused_args_pass(PassNum + 1, ModuleInfo, LocalPredProcs, !VarUsage)
    ;
        Changed = no
    ).

    % Check over all the procedures in a module.
    %
:- pred unused_args_single_pass(pred_proc_list::in, bool::in, bool::out,
    var_usage::in, var_usage::out) is det.

unused_args_single_pass([], !Changed, !VarUsage).
unused_args_single_pass([PredProc | PredProcs], !Changed, !VarUsage) :-
    unused_args_check_proc(PredProc, !Changed, !VarUsage),
    unused_args_single_pass(PredProcs, !Changed, !VarUsage).

    % Check a single procedure.
    %
:- pred unused_args_check_proc(pred_proc_id::in, bool::in, bool::out,
    var_usage::in, var_usage::out) is det.

unused_args_check_proc(PredProcId, !Changed, !VarUsage) :-
    map.lookup(!.VarUsage, PredProcId, LocalUsages0),
    map.keys(LocalUsages0, Vars),
    unused_args_check_all_vars(!.VarUsage, Vars, no, LocalChanged,
        LocalUsages0, LocalUsages),
    (
        LocalChanged = yes,
        map.det_update(PredProcId, LocalUsages, !VarUsage),
        !:Changed = yes
    ;
        LocalChanged = no
    ).

    % Check each var of a procedure in turn.
    %
:- pred unused_args_check_all_vars(var_usage::in, list(prog_var)::in,
    bool::in, bool::out, var_dep::in, var_dep::out) is det.

unused_args_check_all_vars(_, [], !Changed, !LocalVars).
unused_args_check_all_vars(VarUsage, [Var | Vars], !Changed, !LocalVars) :-
    ( lookup_local_var(!.LocalVars, Var, Usage) ->
        Usage = unused(VarDep0, ArgDep0),
        (
            (
                % Check whether any arguments that the current variable
                % depends on are used.
                some [Argument] (
                    set.member(Argument, ArgDep0),
                    Argument = arg_var_in_proc(PredProc, ArgVar),
                    var_is_used(PredProc, ArgVar, VarUsage)
                )
            ;
                % Check whether any variables that the current variable
                % depends on are used.
                some [X] (
                    set.member(X, VarDep0),
                    local_var_is_used(!.LocalVars, X)
                )
            )
        ->
            % Set the current variable to used. Note that we update the same
            % data structure (!LocalVars) as we test in the condition above.
            % This is OK because we use a fixpoint iteration to add variables
            % satisfying the above condition to !LocalVars until we can't add
            % any more.
            set_var_used(Var, !LocalVars),
            !:Changed = yes
        ;
            true
        )
    ;
        true
    ),
    unused_args_check_all_vars(VarUsage, Vars, !Changed, !LocalVars).

:- pred get_unused_arg_info(module_info::in, pred_proc_list::in, var_usage::in,
    unused_arg_info::in, unused_arg_info::out) is det.

get_unused_arg_info(_, [], _, !UnusedArgInfo).
get_unused_arg_info(ModuleInfo, [PredProc | PredProcs], VarUsage,
        !UnusedArgInfo) :-
    PredProc = proc(PredId, ProcId),
    map.lookup(VarUsage, PredProc, LocalVarUsage),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),
    get_unused_arg_nos(LocalVarUsage, HeadVars, 1, UnusedArgs),
    map.det_insert(PredProc, UnusedArgs, !UnusedArgInfo),
    get_unused_arg_info(ModuleInfo, PredProcs, VarUsage, !UnusedArgInfo).

%-----------------------------------------------------------------------------%
%
% Fix up the module
%

    % Information about predicates which have new predicates created for the
    % optimized version.
    %
:- type proc_call_info == map(pred_proc_id, new_proc_info).

    % New pred_id, proc_id, name, and the indices in the argument
    % vector of the arguments that have been removed.
:- type new_proc_info
    --->    call_info(pred_id, proc_id, sym_name, list(int)).

    % Create a new predicate for each procedure which has unused arguments.
    % There are two reasons why we can't throw away the old procedure for
    % non-exported predicates. One is higher-order terms - we can't remove
    % arguments from them without changing their type, so they need the old
    % calling interface. The other is that the next proc_id for a predicate is
    % chosen based on the length of the list of proc_ids.
    %
:- pred unused_args_create_new_pred(unused_arg_info::in, pred_proc_id::in,
    proc_call_info::in, proc_call_info::out,
    module_info::in, module_info::out) is det.

unused_args_create_new_pred(UnusedArgInfo, proc(PredId, ProcId), !ProcCallInfo,
        !ModuleInfo) :-
    map.lookup(UnusedArgInfo, proc(PredId, ProcId), UnusedArgs),
    module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
        OrigPredInfo, OrigProcInfo),
    PredModule = pred_info_module(OrigPredInfo),

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, intermodule_analysis, Intermod),
    (
        Intermod = yes,
        module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),

        module_name_func_id_from_pred_info(OrigPredInfo, ProcId, ModuleId,
            FuncId),
        analysis.lookup_results(AnalysisInfo0, ModuleId, FuncId,
            IntermodResultsTriples : list(analysis_result(unused_args_call,
                unused_args_answer))),
        IntermodOldAnswers = list.map((func(R) = R ^ ar_answer),
            IntermodResultsTriples),

        PredArity = pred_info_orig_arity(OrigPredInfo),
        FuncInfo = unused_args_func_info(PredArity),
        Answer = unused_args(UnusedArgs),

        FilterUnused = (pred(VersionAnswer::in) is semidet :-
            VersionAnswer \= Answer,
            VersionAnswer \= unused_args([]),
            more_precise_than(FuncInfo, Answer, VersionAnswer)
        ),
        IntermodOldArgLists = list.map(get_unused_args,
            list.filter(FilterUnused, IntermodOldAnswers))
    ;
        Intermod = no,
        IntermodResultsTriples = [],
        IntermodOldArgLists = []
    ),

    (
        UnusedArgs = []
    ;
        UnusedArgs = [_ | _],
        pred_info_get_import_status(OrigPredInfo, Status0),
        (
            Status0 = status_opt_imported,
            IntermodResultsTriples = [_ | _],
            IntermodOldArgLists = []
        ->
            % If this predicate is from a .opt file, and no more arguments
            % have been removed than in the original module, then leave the
            % import status as opt_imported so that dead_proc_elim will remove
            % it if no other optimization is performed on it.
            Status = status_opt_imported
        ;
            status_is_exported(Status0) = yes
        ->
            % This specialized version of the predicate will be declared
            % in the analysis file for this module so it must be exported.
            Status = Status0
        ;
            Status = status_local
        ),
        make_new_pred_info(!.ModuleInfo, UnusedArgs, Status,
            proc(PredId, ProcId), OrigPredInfo, NewPredInfo0),
        NewPredName = pred_info_name(NewPredInfo0),
        pred_info_get_procedures(NewPredInfo0, NewProcs0),

        % Assign the old procedure to a new predicate, which will be fixed up
        % in unused_args_fixup_module.
        map.set(ProcId, OrigProcInfo, NewProcs0, NewProcs),
        pred_info_set_procedures(NewProcs, NewPredInfo0, NewPredInfo),

        % Add the new proc to the pred table.
        module_info_get_predicate_table(!.ModuleInfo, PredTable0),
        predicate_table_insert(NewPredInfo, NewPredId, PredTable0, PredTable),
        module_info_set_predicate_table(PredTable, !ModuleInfo),

        % Add the new proc to the proc_call_info map.
        PredSymName = qualified(PredModule, NewPredName),
        map.det_insert(proc(PredId, ProcId),
            call_info(NewPredId, ProcId, PredSymName, UnusedArgs),
            !ProcCallInfo),

        % Add a forwarding predicate with the original interface.
        create_call_goal(UnusedArgs, NewPredId, ProcId, PredModule,
            NewPredName, OrigProcInfo, ForwardingProcInfo),
        module_info_set_pred_proc_info(PredId, ProcId, OrigPredInfo,
            ForwardingProcInfo, !ModuleInfo),

        % Add forwarding predicates for results produced in previous
        % compilations.
        % XXX this only works "once" due to the analysis framework now
        % discarding all but the best answer.  If we compile this module
        % again without changing anything else, we won't remember to
        % produce the same forwarding predicates.  If some callers refer
        % to those forwarding predicates then linking will fail.
        list.foldl(
            make_intermod_proc(PredId, NewPredId, ProcId, NewPredName,
                OrigPredInfo, OrigProcInfo, UnusedArgs),
            IntermodOldArgLists, !ModuleInfo)
    ).

:- pred make_intermod_proc(pred_id::in, pred_id::in, proc_id::in, string::in,
    pred_info::in, proc_info::in, list(int)::in, list(int)::in,
    module_info::in, module_info::out) is det.

make_intermod_proc(PredId, NewPredId, ProcId, NewPredName,
        OrigPredInfo, OrigProcInfo, UnusedArgs, UnusedArgs2, !ModuleInfo) :-
    % Add an exported predicate with the number of removed arguments promised
    % in the analysis file which just calls the new predicate.
    make_new_pred_info(!.ModuleInfo, UnusedArgs2, status_exported,
        proc(PredId, ProcId), OrigPredInfo, ExtraPredInfo0),
    PredModule = pred_info_module(OrigPredInfo),
    create_call_goal(UnusedArgs, NewPredId, ProcId,
        PredModule, NewPredName, OrigProcInfo, ExtraProc0),
    proc_info_get_headvars(OrigProcInfo, HeadVars0),
    remove_listof_elements(1, UnusedArgs2, HeadVars0, IntermodHeadVars),
    proc_info_set_headvars(IntermodHeadVars, ExtraProc0, ExtraProc1),
    proc_info_get_argmodes(OrigProcInfo, ArgModes0),
    remove_listof_elements(1, UnusedArgs2, ArgModes0, IntermodArgModes),
    proc_info_set_argmodes(IntermodArgModes, ExtraProc1, ExtraProc),
    pred_info_get_procedures(ExtraPredInfo0, ExtraProcs0),
    map.set(ProcId, ExtraProc, ExtraProcs0, ExtraProcs),
    pred_info_set_procedures(ExtraProcs, ExtraPredInfo0, ExtraPredInfo),
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(ExtraPredInfo, _, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !ModuleInfo).

:- pred make_new_pred_info(module_info::in, list(int)::in, import_status::in,
    pred_proc_id::in, pred_info::in, pred_info::out) is det.

make_new_pred_info(_ModuleInfo, UnusedArgs, Status, proc(PredId, ProcId),
        !PredInfo) :-
    PredModule = pred_info_module(!.PredInfo),
    Name0 = pred_info_name(!.PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(!.PredInfo),
    pred_info_get_arg_types(!.PredInfo, Tvars, ExistQVars, ArgTypes0),
    pred_info_get_origin(!.PredInfo, OrigOrigin),
    % Create a unique new pred name using the old proc_id.
    (
        string.prefix(Name0, "__"),
        \+ string.prefix(Name0, "__LambdaGoal__")
    ->
        (
            % Fix up special pred names.
            OrigOrigin = origin_special_pred(_SpecialId - TypeCtor)
        ->
            type_ctor_module_name_arity(TypeCtor, TypeModule, TypeName,
                TypeArity),
            string.int_to_string(TypeArity, TypeArityStr),
            TypeModuleString = sym_name_to_string_sep(TypeModule, "__"),
            string.append_list([Name0, "_", TypeModuleString, "__", TypeName,
                "_", TypeArityStr], Name1)
        ;
            % The special predicate has already been specialised.
            Name1 = Name0
        )
    ;
        Name1 = Name0
    ),
    make_pred_name(PredModule, "UnusedArgs", yes(PredOrFunc),
        Name1, newpred_unused_args(UnusedArgs), Name2),
    % The mode number is included because we want to avoid the creation of
    % more than one predicate with the same name if more than one mode of
    % a predicate is specialized. Since the names of e.g. deep profiling
    % proc_static structures are derived from the names of predicates,
    % duplicate predicate names lead to duplicate global variable names
    % and hence to link errors.
    proc_id_to_int(ProcId, ProcInt),
    add_sym_name_suffix(Name2, "_" ++ int_to_string(ProcInt), Name),
    Arity = pred_info_orig_arity(!.PredInfo),
    pred_info_get_typevarset(!.PredInfo, TypeVars),
    remove_listof_elements(1, UnusedArgs, ArgTypes0, ArgTypes),
    pred_info_get_context(!.PredInfo, Context),
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo),
    pred_info_get_markers(!.PredInfo, Markers),
    pred_info_get_goal_type(!.PredInfo, GoalType),
    pred_info_get_class_context(!.PredInfo, ClassContext),
    pred_info_get_var_name_remap(!.PredInfo, VarNameRemap),

    % Since this pred_info isn't built until after the polymorphism
    % transformation is complete, we just use dummy maps for the class
    % constraints.
    map.init(EmptyProofs),
    map.init(EmptyConstraintMap),
    Transform = transform_unused_argument_elimination(UnusedArgs),
    Origin = origin_transformed(Transform, OrigOrigin, PredId),
    pred_info_init(PredModule, Name, Arity, PredOrFunc, Context, Origin,
        Status, GoalType, Markers, ArgTypes, Tvars, ExistQVars,
        ClassContext, EmptyProofs, EmptyConstraintMap, ClausesInfo,
        VarNameRemap, !:PredInfo),
    pred_info_set_typevarset(TypeVars, !PredInfo).

    % Replace the goal in the procedure with one to call the given
    % pred_id and proc_id.
    %
:- pred create_call_goal(list(int)::in, pred_id::in, proc_id::in,
    module_name::in, string::in, proc_info::in, proc_info::out) is det.

create_call_goal(UnusedArgs, NewPredId, NewProcId, PredModule, PredName,
        !OldProc) :-
    proc_info_get_headvars(!.OldProc, HeadVars),
    proc_info_get_goal(!.OldProc, Goal0),
    Goal0 = hlds_goal(_GoalExpr, GoalInfo0),

    % We must use the interface determinism for determining the determinism
    % of the version of the goal with its arguments removed, not the actual
    % determinism of the body is it may be more lax, which will lead to code
    % generation problems.
    proc_info_interface_determinism(!.OldProc, Determinism),
    goal_info_set_determinism(Determinism, GoalInfo0, GoalInfo1),

    proc_info_get_vartypes(!.OldProc, VarTypes0),
    set.list_to_set(HeadVars, NonLocals),
    lookup_var_types(VarTypes0, HeadVars, VarTypeList),
    vartypes_from_corresponding_lists(HeadVars, VarTypeList, VarTypes1),
    % The varset should probably be fixed up, but it shouldn't make
    % too much difference.
    proc_info_get_varset(!.OldProc, VarSet0),
    proc_info_get_rtti_varmaps(!.OldProc, RttiVarMaps0),
    remove_listof_elements(1, UnusedArgs, HeadVars, NewHeadVars),
    GoalExpr = plain_call(NewPredId, NewProcId, NewHeadVars,
        not_builtin, no, qualified(PredModule, PredName)),
    Goal1 = hlds_goal(GoalExpr, GoalInfo1),
    implicitly_quantify_goal_general(ordinary_nonlocals_no_lambda,
        set_to_bitset(NonLocals), _, Goal1, Goal, VarSet0, VarSet,
        VarTypes1, VarTypes, RttiVarMaps0, RttiVarMaps),
    proc_info_set_goal(Goal, !OldProc),
    proc_info_set_varset(VarSet, !OldProc),
    proc_info_set_vartypes(VarTypes, !OldProc),
    proc_info_set_rtti_varmaps(RttiVarMaps, !OldProc).

    % Create a pred_info for an imported pred with a pragma unused_args
    % in the .opt file.
    %
:- pred make_imported_unused_args_pred_info(pred_proc_id::in, list(int)::in,
    proc_call_info::in, proc_call_info::out,
    module_info::in, module_info::out) is det.

make_imported_unused_args_pred_info(OptProc, UnusedArgs, !ProcCallInfo,
        !ModuleInfo) :-
    OptProc = proc(PredId, ProcId),
    module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
        PredInfo0, ProcInfo0),
    make_new_pred_info(!.ModuleInfo, UnusedArgs,
        status_imported(import_locn_interface), OptProc,
        PredInfo0, NewPredInfo0),
    pred_info_get_procedures(NewPredInfo0, NewProcs0),

    % Assign the old procedure to a new predicate.
    proc_info_get_headvars(ProcInfo0, HeadVars0),
    remove_listof_elements(1, UnusedArgs, HeadVars0, HeadVars),
    proc_info_set_headvars(HeadVars, ProcInfo0, ProcInfo1),
    proc_info_get_argmodes(ProcInfo1, ArgModes0),
    remove_listof_elements(1, UnusedArgs, ArgModes0, ArgModes),
    proc_info_set_argmodes(ArgModes, ProcInfo1, ProcInfo),
    map.set(ProcId, ProcInfo, NewProcs0, NewProcs),
    pred_info_set_procedures(NewProcs, NewPredInfo0, NewPredInfo),

    % Add the new proc to the pred table.
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(NewPredInfo, NewPredId, PredTable0, PredTable1),
    module_info_set_predicate_table(PredTable1, !ModuleInfo),
    PredModule = pred_info_module(NewPredInfo),
    PredName = pred_info_name(NewPredInfo),
    PredSymName = qualified(PredModule, PredName),
    % Add the new proc to the proc_call_info map.
    map.det_insert(proc(PredId, ProcId),
        call_info(NewPredId, ProcId, PredSymName, UnusedArgs), !ProcCallInfo).

:- pred remove_listof_elements(int::in, list(int)::in,
    list(T)::in, list(T)::out) is det.

remove_listof_elements(ArgNo, ElemsToRemove, !List) :-
    (
        ElemsToRemove = []
    ;
        ElemsToRemove = [_ | _],
        (
            !.List = [Head | Tail],
            NextArg = ArgNo + 1,
            remove_listof_elements(NextArg, ElemsToRemove, Tail, NewTail),
            ( list.member(ArgNo, ElemsToRemove) ->
                !:List = NewTail
            ;
                !:List = [Head | NewTail]
            )
        ;
            !.List = []
        )
    ).

:- pred get_unused_arg_nos(var_dep::in, list(prog_var)::in, int::in,
    list(int)::out) is det.

get_unused_arg_nos(_, [], _, []).
get_unused_arg_nos(LocalVars, [HeadVar | HeadVars], ArgNo, UnusedArgs) :-
    get_unused_arg_nos(LocalVars, HeadVars, NextArg, UnusedArgsTail),
    NextArg = ArgNo + 1,
    ( map.contains(LocalVars, HeadVar) ->
        UnusedArgs = [ArgNo | UnusedArgsTail]
    ;
        UnusedArgs = UnusedArgsTail
    ).

    % Note - we should probably remove unused variables from the type map.
    %
:- pred unused_args_fixup_module(var_usage::in, pred_proc_list::in,
    proc_call_info::in, module_info::in, module_info::out, bool::in,
    io::di, io::uo) is det.

unused_args_fixup_module(VarUsage, PredProcs, ProcCallInfo, !ModuleInfo,
        VeryVerbose, !IO) :-
    list.foldl2(unused_args_fixup_proc(VeryVerbose, VarUsage, ProcCallInfo),
        PredProcs, !ModuleInfo, !IO).

    % Note - we should probably remove unused variables from the type map.
    %
:- pred unused_args_fixup_proc(bool::in, var_usage::in, proc_call_info::in,
    pred_proc_id::in, module_info::in, module_info::out, io::di, io::uo)
    is det.

unused_args_fixup_proc(VeryVerbose, VarUsage, ProcCallInfo, PredProc,
        !ModuleInfo, !IO) :-
    (
        VeryVerbose = yes,
        PredProc = proc(PredId, ProcId),
        io.write_string("% Fixing up `", !IO),
        Name = predicate_name(!.ModuleInfo, PredId),
        Arity = predicate_arity(!.ModuleInfo, PredId),
        proc_id_to_int(ProcId, ProcInt),
        io.write_string(Name, !IO),
        io.write_string("/", !IO),
        io.write_int(Arity, !IO),
        io.write_string("' in mode ", !IO),
        io.write_int(ProcInt, !IO),
        io.write_char('\n', !IO)
    ;
        VeryVerbose = no
    ),
    do_unused_args_fixup_proc(VarUsage, PredProc, ProcCallInfo, !ModuleInfo).

:- pred do_unused_args_fixup_proc(var_usage::in, pred_proc_id::in,
    proc_call_info::in, module_info::in, module_info::out) is det.

do_unused_args_fixup_proc(VarUsage, proc(OldPredId, OldProcId), ProcCallInfo,
        ModuleInfo0, ModuleInfo) :-
    (
        % Work out which proc we should be fixing up.
        map.search(ProcCallInfo, proc(OldPredId, OldProcId),
            call_info(NewPredId, NewProcId, _, UnusedArgs0))
    ->
        UnusedArgs = UnusedArgs0,
        PredId = NewPredId,
        ProcId = NewProcId
    ;
        UnusedArgs = [],
        PredId = OldPredId,
        ProcId = OldProcId
    ),
    map.lookup(VarUsage, proc(OldPredId, OldProcId), UsageInfos),
    map.keys(UsageInfos, UnusedVars),
    module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
        PredInfo0, ProcInfo0),
    proc_info_get_vartypes(ProcInfo0, VarTypes0),
    module_info_get_preds(ModuleInfo0, Preds0),
    pred_info_get_procedures(PredInfo0, Procs0),

    proc_info_get_headvars(ProcInfo0, HeadVars0),
    proc_info_get_argmodes(ProcInfo0, ArgModes0),
    proc_info_get_varset(ProcInfo0, VarSet0),
    proc_info_get_goal(ProcInfo0, Goal0),
    remove_listof_elements(1, UnusedArgs, HeadVars0, HeadVars),
    remove_listof_elements(1, UnusedArgs, ArgModes0, ArgModes),

    some [!ProcInfo, !Goal] (
        !:ProcInfo = ProcInfo0,
        !:Goal = Goal0,

        proc_info_set_headvars(HeadVars, !ProcInfo),
        proc_info_set_argmodes(ArgModes, !ProcInfo),

        % Remove unused vars from goal.
        FixupInfo0 = fixup_info(ModuleInfo0, ProcCallInfo, UnusedVars,
            VarSet0, VarTypes0),
        unused_args_fixup_goal(!Goal, FixupInfo0, FixupInfo, Changed),
        FixupInfo = fixup_info(_, _, _, VarSet1, VarTypes1),
        (
            Changed = yes,
            % If anything has changed, rerun quantification.
            NonLocals = set_of_var.list_to_set(HeadVars),
            proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
            implicitly_quantify_goal_general(ordinary_nonlocals_no_lambda,
                NonLocals, _, !Goal, VarSet1, VarSet, VarTypes1, VarTypes,
                RttiVarMaps0, RttiVarMaps),
            proc_info_set_goal(!.Goal, !ProcInfo),
            proc_info_set_varset(VarSet, !ProcInfo),
            proc_info_set_vartypes(VarTypes, !ProcInfo),
            proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo)
        ;
            Changed = no
        ),
        ProcInfo = !.ProcInfo
    ),

    map.set(ProcId, ProcInfo, Procs0, Procs),
    pred_info_set_procedures(Procs, PredInfo0, PredInfo),
    map.set(PredId, PredInfo, Preds0, Preds),
    module_info_set_preds(Preds, ModuleInfo0, ModuleInfo).

:- type fixup_info
    --->    fixup_info(
                fixup_module_info       :: module_info,
                fixup_proc_call_info    :: proc_call_info,
                fixup_unused_vars       :: list(prog_var),
                fixup_varset            :: prog_varset,
                fixup_vartypes          :: vartypes
            ).

    % This is the important bit of the transformation.
    %
:- pred unused_args_fixup_goal(hlds_goal::in, hlds_goal::out,
    fixup_info::in, fixup_info::out, bool::out) is det.

unused_args_fixup_goal(Goal0, Goal, !Info, Changed) :-
    unused_args_fixup_goal_expr(Goal0, Goal1, !Info, Changed),
    Goal1 = hlds_goal(GoalExpr1, GoalInfo1),
    (
        Changed = yes,
        UnusedVars = !.Info ^ fixup_unused_vars,
        unused_args_fixup_goal_info(UnusedVars, GoalInfo1, GoalInfo)
    ;
        Changed = no,
        GoalInfo = GoalInfo1
    ),
    Goal = hlds_goal(GoalExpr1, GoalInfo).

:- pred unused_args_fixup_goal_expr(hlds_goal::in, hlds_goal::out,
    fixup_info::in, fixup_info::out, bool::out) is det.

unused_args_fixup_goal_expr(Goal0, Goal, !Info, Changed) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        unused_args_fixup_conjuncts(Goals0, Goals, !Info, no, Changed),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Goals0),
        unused_args_fixup_disjuncts(Goals0, Goals, !Info, no, Changed),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(NegGoal0),
        unused_args_fixup_goal(NegGoal0, NegGoal, !Info, Changed),
        GoalExpr = negation(NegGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        unused_args_fixup_cases(Cases0, Cases, !Info, no, Changed),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        unused_args_fixup_goal(Cond0, Cond, !Info, Changed1),
        unused_args_fixup_goal(Then0, Then, !Info, Changed2),
        unused_args_fixup_goal(Else0, Else, !Info, Changed3),
        bool.or_list([Changed1, Changed2, Changed3], Changed),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( Reason = from_ground_term(TermVar, from_ground_term_construct) ->
            UnusedVars = !.Info ^ fixup_unused_vars,
            ( list.member(TermVar, UnusedVars) ->
                Goal = true_goal,
                % We don't change the set of unneeded variables.
                Changed = no
            ;
                Goal = Goal0,
                Changed = no
            )
        ;
            unused_args_fixup_goal(SubGoal0, SubGoal, !Info, Changed),
            GoalExpr = scope(Reason, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = plain_call(PredId, ProcId, ArgVars0, Builtin,
            UnifyC, _Name),
        ProcCallInfo = !.Info ^ fixup_proc_call_info,
        ( map.search(ProcCallInfo, proc(PredId, ProcId), CallInfo) ->
            CallInfo = call_info(NewPredId, NewProcId, NewName, UnusedArgs),
            Changed = yes,
            remove_listof_elements(1, UnusedArgs, ArgVars0, ArgVars),
            GoalExpr = plain_call(NewPredId, NewProcId, ArgVars, Builtin,
                UnifyC, NewName),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            Changed = no,
            Goal = hlds_goal(GoalExpr0, GoalInfo0)
        )
    ;
        GoalExpr0 = unify(_Var, _RHS, _Mode, Unify, _Context),
        ModuleInfo = !.Info ^ fixup_module_info,
        UnusedVars = !.Info ^ fixup_unused_vars,
        ( need_unify(ModuleInfo, UnusedVars, Unify, ChangedPrime) ->
            GoalExpr = GoalExpr0,
            Changed = ChangedPrime
        ;
            GoalExpr = true_goal_expr,
            Changed = yes
        ),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = generic_call(_, _, _, _, _),
        Goal = hlds_goal(GoalExpr0, GoalInfo0),
        Changed = no
    ;
        GoalExpr0 = call_foreign_proc(Attributes, PredId, ProcId,
            Args0, ExtraArgs0, MaybeTraceRuntimeCond, Impl),
        % The code in here should be kept in sync with the treatment of
        % foreign_procs in traverse_goal.
        Changed0 = no,
        map.init(Subst0),
        list.map_foldl3(rename_apart_unused_foreign_arg,
            Args0, Args, Subst0, Subst1, !Info, Changed0, ArgsChanged),
        list.map_foldl3(rename_apart_unused_foreign_arg,
            ExtraArgs0, ExtraArgs, Subst1, Subst, !Info, ArgsChanged, Changed),
        GoalExpr = call_foreign_proc(Attributes, PredId, ProcId,
            Args, ExtraArgs, MaybeTraceRuntimeCond, Impl),
        rename_vars_in_goal_info(need_not_rename, Subst, GoalInfo0, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($module, $pred, "shorthand")
    ).

:- pred rename_apart_unused_foreign_arg(foreign_arg::in, foreign_arg::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out,
    fixup_info::in, fixup_info::out, bool::in, bool::out) is det.

rename_apart_unused_foreign_arg(Arg0, Arg, !Subst, !Info, !Changed) :-
    Arg0 = foreign_arg(OldVar, MaybeName, OrigType, BoxPolicy),
    (
        MaybeName = yes(_),
        Arg = Arg0
    ;
        MaybeName = no,
        VarSet0 = !.Info ^ fixup_varset,
        VarTypes0 = !.Info ^ fixup_vartypes,
        ( varset.search_name(VarSet0, OldVar, Name) ->
            varset.new_named_var(Name, NewVar, VarSet0, VarSet)
        ;
            varset.new_var(NewVar, VarSet0, VarSet)
        ),
        lookup_var_type(VarTypes0, OldVar, Type),
        add_var_type(NewVar, Type, VarTypes0, VarTypes),
        !Info ^ fixup_varset := VarSet,
        !Info ^ fixup_vartypes := VarTypes,

        % It is possible for an unnamed input argument to occur more than once
        % in the list of foreign_args.
        map.set(OldVar, NewVar, !Subst),
        Arg = foreign_arg(NewVar, MaybeName, OrigType, BoxPolicy),
        !:Changed = yes
    ).

    % Remove useless unifications from a list of conjuncts.
    %
:- pred unused_args_fixup_conjuncts(hlds_goals::in, hlds_goals::out,
    fixup_info::in, fixup_info::out, bool::in, bool::out) is det.

unused_args_fixup_conjuncts([], [], !Info, !Changed).
unused_args_fixup_conjuncts([Goal0 | Goals0], Goals, !Info, !Changed) :-
    unused_args_fixup_goal(Goal0, Goal, !Info, LocalChanged),
    (
        LocalChanged = yes,
        !:Changed = yes
    ;
        LocalChanged = no
    ),
    % Replacing a goal with true signals that it is no longer needed.
    ( Goal = hlds_goal(true_goal_expr, _) ->
        Goals = Goals1
    ;
        Goals = [Goal | Goals1]
    ),
    unused_args_fixup_conjuncts(Goals0, Goals1, !Info, !Changed).

    % We can't remove unused goals from the list of disjuncts as we do
    % for conjuncts, since that would change the determinism of the goal.
    %
:- pred unused_args_fixup_disjuncts(hlds_goals::in, hlds_goals::out,
    fixup_info::in, fixup_info::out, bool::in, bool::out) is det.

unused_args_fixup_disjuncts([], [], !Info, !Changed).
unused_args_fixup_disjuncts([Goal0 | Goals0], [Goal | Goals],
        !Info, !Changed) :-
    unused_args_fixup_goal(Goal0, Goal, !Info, LocalChanged),
    (
        LocalChanged = yes,
        !:Changed = yes
    ;
        LocalChanged = no
    ),
    unused_args_fixup_disjuncts(Goals0, Goals, !Info, !Changed).

:- pred unused_args_fixup_cases(list(case)::in, list(case)::out,
    fixup_info::in, fixup_info::out, bool::in, bool::out) is det.

unused_args_fixup_cases([], [], !Info, !Changed).
unused_args_fixup_cases([Case0 | Cases0], [Case | Cases], !Info, !Changed) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    unused_args_fixup_goal(Goal0, Goal, !Info, LocalChanged),
    Case = case(MainConsId, OtherConsIds, Goal),
    (
        LocalChanged = yes,
        !:Changed = yes
    ;
        LocalChanged = no
    ),
    unused_args_fixup_cases(Cases0, Cases, !Info, !Changed).

    % Fail if the unification is no longer needed.
    %
:- pred need_unify(module_info::in, list(prog_var)::in, unification::in,
    bool::out) is semidet.

need_unify(ModuleInfo, UnusedVars, Unify, Changed) :-
    (
        Unify = simple_test(_, _),
        % A simple test doesn't have any unused vars to fixup.
        Changed = no
    ;
        % Target unused => we don't need the assignment
        % Source unused => Target unused
        Unify = assign(Target, _Source),
        \+ list.member(Target, UnusedVars),
        Changed = no
    ;
        % LVar unused => we don't need the unification
        Unify = construct(LVar, _, _, _, _, _, _),
        \+ list.member(LVar, UnusedVars),
        Changed = no
    ;
        Unify = deconstruct(LVar, _, ArgVars, ArgModes, CanFail, _CanCGC),
        \+ list.member(LVar, UnusedVars),
        (
            % Are any of the args unused? If so, we need to to fix up the
            % goal_info.
            CanFail = cannot_fail,
            check_deconstruct_args(ModuleInfo, UnusedVars, ArgVars, ArgModes,
                no, Changed)
        ;
            CanFail = can_fail,
            Changed = no
        )
    ;
        % These should have been transformed into calls by polymorphism.m.
        Unify = complicated_unify(_, _, _),
        unexpected($module, $pred, "complicated unify")
    ).

    % Check if any of the arguments of a deconstruction are unused, if
    % so Changed will be yes and quantification will be rerun. Fails if
    % none of the arguments are used. Arguments which further instantiate
    % the deconstructed variable are ignored in this.
    %
:- pred check_deconstruct_args(module_info::in, list(prog_var)::in,
    list(prog_var)::in, list(uni_mode)::in, bool::in, bool::out) is semidet.

check_deconstruct_args(ModuleInfo, UnusedVars, Args, Modes, !.SomeUsed,
        Changed) :-
    (
        Args = [ArgVar | ArgVars],
        Modes = [ArgMode | ArgModes]
    ->
        (
            ArgMode = ((Inst1 - Inst2) -> _),
            mode_is_output(ModuleInfo, (Inst1 -> Inst2)),
            list.member(ArgVar, UnusedVars)
        ->
            check_deconstruct_args(ModuleInfo, UnusedVars, ArgVars, ArgModes,
                !.SomeUsed, _),
            Changed = yes
        ;
            !:SomeUsed = yes,
            check_deconstruct_args(ModuleInfo, UnusedVars, ArgVars, ArgModes,
                !.SomeUsed, Changed)
        )
    ;
        Args = [],
        Modes = []
    ->
        !.SomeUsed = yes,
        Changed = no
    ;
        unexpected($module, $pred, "mismatched lists")
    ).

    % Remove unused vars from the instmap_delta, quantification fixes up
    % the rest.
    %
:- pred unused_args_fixup_goal_info(list(prog_var)::in, hlds_goal_info::in,
    hlds_goal_info::out) is det.

unused_args_fixup_goal_info(UnusedVars, !GoalInfo) :-
    InstMap0 = goal_info_get_instmap_delta(!.GoalInfo),
    instmap_delta_delete_vars(UnusedVars, InstMap0, InstMap),
    goal_info_set_instmap_delta(InstMap, !GoalInfo).

%-----------------------------------------------------------------------------%

    % Except for type_infos, all args that are unused in one mode of a
    % predicate should be unused in all of the modes of a predicate, so we
    % only need to put out one warning for each predicate.
    %
:- pred output_warnings_and_pragmas(module_info::in, unused_arg_info::in,
    maybe(io.output_stream)::in, bool::in, pred_proc_list::in,
    set(pred_id)::in, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

output_warnings_and_pragmas(_, _, _, _, [], _, !Specs, !IO).
output_warnings_and_pragmas(ModuleInfo, UnusedArgInfo, WriteOptPragmas, DoWarn,
        [proc(PredId, ProcId) | PredProcIds], !.WarnedPredIds, !Specs, !IO) :-
    ( map.search(UnusedArgInfo, proc(PredId, ProcId), UnusedArgs) ->
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        (
            Name = pred_info_name(PredInfo),
            \+ pred_info_is_imported(PredInfo),
            \+ pred_info_get_import_status(PredInfo, status_opt_imported),

            % Don't warn about builtins that have unused arguments.
            \+ pred_info_is_builtin(PredInfo),
            \+ is_unify_or_compare_pred(PredInfo),

            % Don't warn about stubs for procedures with no clauses --
            % in that case, we *expect* none of the arguments to be used.
            pred_info_get_markers(PredInfo, Markers),
            \+ check_marker(Markers, marker_stub),

            % Don't warn about lambda expressions not using arguments.
            % (The warning message for these doesn't contain context,
            % so it's useless).
            \+ string.sub_string_search(Name, "__LambdaGoal__", _),

            % Don't warn for a specialized version.
            \+ (
                string.sub_string_search(Name, "__ho", Position),
                string.length(Name, Length),
                IdLen = Length - Position - 4,
                string.right(Name, IdLen, Id),
                string.to_int(Id, _)
            ),
            module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
            TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
            \+ set.member(PredId, TypeSpecForcePreds),

            % Don't warn for a loop-invariant hoisting-generated procedure.
            pred_info_get_origin(PredInfo, Origin),
            \+ (
                Origin = origin_transformed(transform_loop_invariant(_), _, _)
            ),

            % XXX We don't currently generate pragmas for the automatically
            % generated class instance methods because the compiler aborts
            % when trying to read them back in from the `.opt' files.
            \+ check_marker(Markers, marker_class_instance_method),
            \+ check_marker(Markers, marker_named_class_instance_method)
        ->
            write_unused_args_to_opt_file(WriteOptPragmas,
                PredInfo, ProcId, UnusedArgs, !IO),
            maybe_warn_unused_args(DoWarn, ModuleInfo, PredInfo,
                PredId, ProcId, UnusedArgs, !WarnedPredIds, !Specs)
        ;
            true
        )
    ;
        true
    ),
    output_warnings_and_pragmas(ModuleInfo, UnusedArgInfo, WriteOptPragmas,
        DoWarn, PredProcIds, !.WarnedPredIds, !Specs, !IO).

:- pred write_unused_args_to_opt_file(maybe(io.output_stream)::in,
    pred_info::in, proc_id::in, list(int)::in, io::di, io::uo) is det.

write_unused_args_to_opt_file(no, _, _, _, !IO).
write_unused_args_to_opt_file(yes(OptStream), PredInfo, ProcId, UnusedArgs,
        !IO) :-
    (
        ( pred_info_is_exported(PredInfo)
        ; pred_info_is_opt_exported(PredInfo)
        ; pred_info_is_exported_to_submodules(PredInfo)
        ),
        UnusedArgs = [_ | _]
    ->
        Module = pred_info_module(PredInfo),
        Name = pred_info_name(PredInfo),
        Arity = pred_info_orig_arity(PredInfo),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        io.set_output_stream(OptStream, OldOutput, !IO),
        proc_id_to_int(ProcId, ModeNum),
        PredSymName = qualified(Module, Name),
        PredNameArityPFMn = pred_name_arity_pf_mn(PredSymName, Arity,
            PredOrFunc, ModeNum),
        UnusedArgsInfo = pragma_info_unused_args(PredNameArityPFMn,
            UnusedArgs),
        mercury_output_pragma_unused_args(UnusedArgsInfo, !IO),
        io.set_output_stream(OldOutput, _, !IO)
    ;
        true
    ).

:- pred maybe_warn_unused_args(bool::in, module_info::in, pred_info::in,
    pred_id::in, proc_id::in, list(int)::in,
    set(pred_id)::in, set(pred_id)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_warn_unused_args(no, _, _, _, _, _, !WarnedPredIds, !Specs).
maybe_warn_unused_args(yes, ModuleInfo, PredInfo, PredId, ProcId,
        UnusedArgs0, !WarnedPredIds, !Specs) :-
    ( set.member(PredId, !.WarnedPredIds) ->
        true
    ;
        set.insert(PredId, !WarnedPredIds),
        pred_info_get_procedures(PredInfo, Procs),
        map.lookup(Procs, ProcId, Proc),
        proc_info_get_headvars(Proc, HeadVars),
        list.length(HeadVars, NumHeadVars),

        % Strip off the extra type_info arguments
        % inserted at the front by polymorphism.m
        NumToDrop = NumHeadVars - pred_info_orig_arity(PredInfo),
        adjust_unused_args(NumToDrop, UnusedArgs0, UnusedArgs),
        (
            UnusedArgs = [_ | _],
            Spec = report_unused_args(ModuleInfo, PredInfo, UnusedArgs),
            !:Specs = [Spec | !.Specs]
        ;
            UnusedArgs = []
        )
    ).

    % Adjust warning message for the presence of type_infos.
    %
:- pred adjust_unused_args(int::in, list(int)::in, list(int)::out) is det.

adjust_unused_args(_, [], []).
adjust_unused_args(NumToDrop, [UnusedArgNo | UnusedArgNos0], AdjUnusedArgs) :-
    NewArg = UnusedArgNo - NumToDrop,
    ( NewArg < 1 ->
        AdjUnusedArgs = AdjUnusedArgs1
    ;
        AdjUnusedArgs = [NewArg | AdjUnusedArgs1]
    ),
    adjust_unused_args(NumToDrop, UnusedArgNos0, AdjUnusedArgs1).

    % Warn about unused arguments in a predicate. Only arguments unused
    % in every mode of a predicate are warned about. The warning is
    % suppressed for type_infos.
    %
:- func report_unused_args(module_info, pred_info, list(int)) = error_spec.

report_unused_args(_ModuleInfo, PredInfo, UnusedArgs) = Spec :-
    list.length(UnusedArgs, NumArgs),
    pred_info_get_context(PredInfo, Context),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    Pieces1 = [words("In"), fixed(pred_or_func_to_full_str(PredOrFunc)),
        sym_name_and_arity(qualified(ModuleName, PredName) / Arity),
        suffix(":"), nl, words("warning:")],
    ( NumArgs = 1 ->
        Pieces2 = [words("argument") | format_arg_list(UnusedArgs)] ++
            [words("is unused."), nl]
    ;
        Pieces2 = [words("arguments") | format_arg_list(UnusedArgs)] ++
            [words("are unused."), nl]
    ),
    Msg = simple_msg(Context, [always(Pieces1 ++ Pieces2)]),
    Spec = error_spec(severity_warning, phase_code_gen, [Msg]).

:- func format_arg_list(list(int)) = list(format_component).

format_arg_list([]) = unexpected($module, $pred, "empty list").
format_arg_list([Arg | Rest]) = Pieces :-
    ArgStr = int_to_string(Arg),
    (
        Rest = [],
        Pieces = [fixed(ArgStr)]
    ;
        Rest = [Head | Tail],
        Pieces = [fixed(ArgStr) | format_arg_list_2(Head, Tail)]
    ).

:- func format_arg_list_2(int, list(int)) = list(format_component).

format_arg_list_2(First, List) = Pieces :-
    FirstStr = int_to_string(First),
    (
        List = [Second | Rest],
        Pieces = [suffix(","), fixed(FirstStr) |
            format_arg_list_2(Second, Rest)]
    ;
        List = [],
        Pieces = [fixed("and"), fixed(FirstStr)]
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_record_intermod_unused_args(module_info::in, unused_arg_info::in,
    pred_id::in, analysis_info::in, analysis_info::out) is det.

maybe_record_intermod_unused_args(ModuleInfo, UnusedArgInfo, PredId,
        !AnalysisInfo) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    list.foldl(
        maybe_record_intermod_unused_args_2(ModuleInfo, UnusedArgInfo,
            PredId, PredInfo),
        ProcIds, !AnalysisInfo).

:- pred maybe_record_intermod_unused_args_2(module_info::in,
    unused_arg_info::in, pred_id::in, pred_info::in, proc_id::in,
    analysis_info::in, analysis_info::out) is det.

maybe_record_intermod_unused_args_2(ModuleInfo, UnusedArgInfo,
        PredId, PredInfo, ProcId, !AnalysisInfo) :-
    (
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        not is_unify_or_compare_pred(PredInfo)
    ->
        PPId = proc(PredId, ProcId),
        ( map.search(UnusedArgInfo, PPId, UnusedArgs) ->
            Answer = unused_args(UnusedArgs)
        ;
            Answer = unused_args([])
        ),
        module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
        record_result(ModuleName, FuncId, unused_args_call, Answer, optimal,
            !AnalysisInfo)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

    % If a procedure in this module calls a procedure from another module,
    % then we assume that this module depends on the analysis results of that
    % other procedure.
    %
    % This way of constructing the intermodule dependency graph is easier than
    % actually keeping track of which external analysis results we have used
    % in order to reach analysis results for this module.
    % It works because (1) we only have one type of call pattern so we don't
    % need to know which call patterns are used, and (2) we only record the
    % entire module as a dependency, so we don't have to know which exported
    % procedure is calling (directly or indirectly) which imported procedure.
    %
:- pred record_intermod_dependencies(module_info::in, pred_proc_id::in,
    analysis_info::in, analysis_info::out) is det.

record_intermod_dependencies(ModuleInfo, CallerPredProcId, !AnalysisInfo) :-
    module_info_pred_proc_info(ModuleInfo, CallerPredProcId,
        _CallerPredInfo, CallerProcInfo),
    proc_info_get_goal(CallerProcInfo, Goal),
    pred_proc_ids_from_goal(Goal, CalleePredProcIds),
    list.foldl(record_intermod_dependencies_2(ModuleInfo),
        CalleePredProcIds, !AnalysisInfo).

:- pred record_intermod_dependencies_2(module_info::in, pred_proc_id::in,
    analysis_info::in, analysis_info::out) is det.

record_intermod_dependencies_2(ModuleInfo, CalleePredProcId, !AnalysisInfo) :-
    CalleePredProcId = proc(CalleePredId, _),
    module_info_pred_info(ModuleInfo, CalleePredId, CalleePredInfo),
    (
        pred_info_is_imported_not_external(CalleePredInfo),
        not is_unify_or_compare_pred(CalleePredInfo)
    ->
        module_name_func_id(ModuleInfo, CalleePredProcId, CalleeModule,
            CalleeFuncId),
        Call = unused_args_call,
        Answer = _ : unused_args_answer,
        get_func_info(ModuleInfo, CalleeModule, CalleeFuncId, Call, Answer,
            FuncInfo),
        record_dependency(CalleeModule, CalleeFuncId, FuncInfo, Call, Answer,
            !AnalysisInfo)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred write_var_usage_map(module_info::in, var_usage::in, io::di, io::uo)
    is det.

write_var_usage_map(ModuleInfo, VarUsageMap, !IO) :-
    map.to_assoc_list(VarUsageMap, VarUsageList),
    list.foldl(write_var_usage(ModuleInfo), VarUsageList, !IO).

:- pred write_var_usage(module_info::in, pair(pred_proc_id, var_dep)::in,
    io::di, io::uo) is det.

write_var_usage(ModuleInfo, PredProcId - VarDepMap, !IO) :-
    PredProcIdStr = pred_proc_id_to_string(ModuleInfo, PredProcId),
    io.format("\n%s:\n", [s(PredProcIdStr)], !IO),
    map.to_assoc_list(VarDepMap, VarDepList),
    module_info_proc_info(ModuleInfo, PredProcId, ProcInfo),
    proc_info_get_varset(ProcInfo, VarSet),
    list.foldl2(write_usage_info(ModuleInfo, VarSet), VarDepList,
        [], RevNoDependVars, !IO),
    list.reverse(RevNoDependVars, NoDependVars),
    (
        NoDependVars = []
    ;
        NoDependVars = [_ | _],
        NoDependVarsStr = mercury_vars_to_string(VarSet, yes, NoDependVars),
        io.format("nodepend vars: %s\n", [s(NoDependVarsStr)], !IO)
    ).

:- pred write_usage_info(module_info::in, prog_varset::in,
    pair(prog_var, usage_info)::in, list(prog_var)::in, list(prog_var)::out,
    io::di, io::uo) is det.

write_usage_info(ModuleInfo, VarSet, Var - UsageInfo, !RevNoDependVars, !IO) :-
    UsageInfo = unused(Vars, Args),
    set.to_sorted_list(Vars, VarList),
    set.to_sorted_list(Args, ArgList),
    ( VarList = [], ArgList = [] ->
        !:RevNoDependVars = [Var | !.RevNoDependVars]
    ;
        VarStr = mercury_var_to_string(VarSet, yes, Var),
        io.format("dependencies of %s:\n", [s(VarStr)], !IO),
        (
            VarList = []
        ;
            VarList = [_ | _],
            VarListStr = mercury_vars_to_string(VarSet, yes, VarList),
            io.format("on variables: %s\n", [s(VarListStr)], !IO)
        ),
        (
            ArgList = []
        ;
            ArgList = [_ | _],
            io.write_string("on arguments:\n", !IO),
            list.foldl(write_arg_var_in_proc(ModuleInfo), ArgList, !IO)
        )
    ).

:- pred write_arg_var_in_proc(module_info::in, arg_var_in_proc::in,
    io::di, io::uo) is det.

write_arg_var_in_proc(ModuleInfo, ArgVarInProc, !IO) :-
    ArgVarInProc = arg_var_in_proc(PredProcId, Var),
    PredProcIdStr = pred_proc_id_to_string(ModuleInfo, PredProcId),
    module_info_proc_info(ModuleInfo, PredProcId, ProcInfo),
    proc_info_get_varset(ProcInfo, VarSet),
    VarStr = mercury_var_to_string(VarSet, yes, Var),
    io.format("%s: %s\n", [s(PredProcIdStr), s(VarStr)], !IO).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.unused_args.
%-----------------------------------------------------------------------------%
