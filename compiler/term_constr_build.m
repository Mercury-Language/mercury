%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2003, 2005-2012 The University of Melbourne.
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: term_constr_build.m.
% Main author: juliensf.
% (partially based on code written by vjteag)
%
% This module is responsible for building the abstract representation (AR)
% used by the constraint termination analyser.
% (The AR is defined in term_constr_data.m).
%
% TODO:
% Make the abstract representations more independent of the HLDS.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.term_constr_build.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_module.
:- import_module transform_hlds.term_constr_errors.
:- import_module transform_hlds.term_norm.

:- import_module bool.
:- import_module list.

%-----------------------------------------------------------------------------%

    % This structure holds the values of options used to control the build
    % pass.
    %
:- type term_build_options.

    % term_build_options_init(Norm, PropFailure, ArgSizeOnly):
    %
    % Initialise the `build_options' structure.
    % `Norm' is the norm we are using.
    % `PropFailure' is `yes' if we are propagating failure constraints
    % and no otherwise.
    % `ArgSizeOnly' is `yes' if the `--arg-size-analysis-only' option
    % is enabled and `no' otherwise.
    %
:- func term_build_options_init(functor_info, bool, bool) = term_build_options.

    % Builds the abstract representation of an SCC.
    %
:- pred term_constr_build_abstract_scc(term_build_options::in,
    scc_with_entry_points::in, list(term2_error)::out,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_test.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.lp_rational.
:- import_module libs.polyhedron.
:- import_module libs.rat.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.term_constr_data.
:- import_module transform_hlds.term_constr_main_types.
:- import_module transform_hlds.term_constr_util.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%
%
% Build pass options.
%

:- type term_build_options
    --->    term_build_options(
                % Which norm we are using.
                tbo_functor_info    :: functor_info,

                % Whether propagating failure constraints is enabled.
                tbo_failure_constrs :: bool,

                % Whether `--term2-arg-size-only' is enabled.
                tbo_arg_size_only   :: bool
            ).

term_build_options_init(Norm, Failure, ArgSizeOnly) =
    term_build_options(Norm, Failure, ArgSizeOnly).

%-----------------------------------------------------------------------------%

% This information is accumulated while building the abstract
% representation of a SCC. After we have finished we write it to the
% module_info. We cannot do this while we are building each individual
% procedure because we will not have all the information we need until
% we have finished processing the entire SCC.

:- type term_scc_info
    --->    term_scc_info(
                tsi_scc_ppid        :: pred_proc_id,
                tsi_proc            :: abstract_proc,
                tsi_size_var_map    :: size_var_map,
                tsi_intermod        :: intermod_status,
                tsi_accum_errors    :: list(term2_error),
                tsi_non_zero_heads  :: list(size_var)
            ).

%-----------------------------------------------------------------------------%

term_constr_build_abstract_scc(Options, SCCWithEntryPoints, Errors,
        !ModuleInfo) :-
    SCCWithEntryPoints = scc_with_entry_points(SCC,
        SCCProcsCalledFromHigherSCCs, ExportedSCCProcs),
    set.union(SCCProcsCalledFromHigherSCCs, ExportedSCCProcs, EntryProcs),

    set.foldl2(
        term_constr_build_abstract_proc(!.ModuleInfo, Options,
            SCC, EntryProcs),
        SCC, varset.init, SizeVarset, [], AbstractSCC),
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    RecordInfo =
        ( pred(Info::in, !.Errors::in, !:Errors::out,
                !.PredIdTable::in, !:PredIdTable::out) is det :-
            Info = term_scc_info(proc(PredId, ProcId), AR0, VarMap, Status,
                ProcErrors, HeadSizeVars),

            % Record the proper size_varset. Each procedure has a copy.
            % XXX It would be nicer to store one copy per SCC.
            %
            % NOTE: although each procedure in the a SCC shares the same
            % size_varset, they should all have separate size_var_maps.

            AR = AR0 ^ ap_size_varset := SizeVarset,
            map.lookup(!.PredIdTable, PredId, PredInfo0),
            pred_info_get_proc_table(PredInfo0, ProcTable0),
            map.lookup(ProcTable0, ProcId, ProcInfo0),
            some [!Term2Info] (
                proc_info_get_termination2_info(ProcInfo0, !:Term2Info),
                term2_info_set_intermod_status(yes(Status), !Term2Info),
                term2_info_set_abstract_rep(yes(AR), !Term2Info),
                term2_info_set_size_var_map(VarMap, !Term2Info),
                term2_info_set_head_vars(HeadSizeVars, !Term2Info),

                % If the remainder of the analysis is going to depend upon
                % higher order constructs, then set up the information
                % accordingly.
                ( if analysis_depends_on_ho(AR) then
                    term2_info_set_success_constrs(yes(polyhedron.universe),
                        !Term2Info),
                    HorderErrors = list.map(
                        ( func(ho_call(Context)) =
                            term2_error(Context, horder_call)
                        ), AR ^ ap_ho_calls),
                    !:Errors = HorderErrors ++ !.Errors
                else
                    true
                ),
                proc_info_set_termination2_info(!.Term2Info,
                    ProcInfo0, ProcInfo)
            ),
            map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
            pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
            map.det_update(PredId, PredInfo, !PredIdTable),
            !:Errors = ProcErrors ++ !.Errors
        ),
    list.foldl2(RecordInfo, AbstractSCC, [], Errors,
        PredIdTable0, PredIdTable),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo).

:- pred term_constr_build_abstract_proc(module_info::in,
    term_build_options::in, scc::in, set(pred_proc_id)::in, pred_proc_id::in,
    size_varset::in, size_varset::out,
    list(term_scc_info)::in, list(term_scc_info)::out) is det.

term_constr_build_abstract_proc(ModuleInfo, Options, SCC, EntryProcs, PPId,
        !SizeVarset, !AbstractInfo) :-
    trace [io(!DebugIO), compiletime(flag("term_constr_build"))] (
        PPIdStr = pred_proc_id_to_string(ModuleInfo, PPId),
        get_debug_output_stream(ModuleInfo, DebugStream, !DebugIO),
        io.format(DebugStream,
            "Building procedure: %s\n", [s(PPIdStr)], !DebugIO),
        io.flush_output(DebugStream, !DebugIO)
    ),

    module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, ProcInfo),
    pred_info_get_context(PredInfo, Context),
    proc_info_get_var_table(ModuleInfo, ProcInfo, VarTable),
    proc_info_get_headvars(ProcInfo, HeadProgVars),
    proc_info_get_argmodes(ProcInfo, ArgModes0),
    proc_info_get_goal(ProcInfo, Goal0),
    % The pretest code we add for compiler-generated unification and comparison
    % predicates uses type casts. It uses them in a way that is guaranteed
    % to terminate, but our analysis is not (yet) able to find this out for
    % itself. We therefore analyse only the non-pretest parts of such goals.
    Goal = maybe_strip_equality_pretest(Goal0),

    % Allocate one size_var for each real var. in the procedure.
    % Work out which variables have zero size.
    allocate_sizevars(HeadProgVars, Goal, SizeVarMap, !SizeVarset),
    Zeros = find_zero_size_vars(ModuleInfo, VarTable, SizeVarMap),
    Info0 = init_traversal_info(ModuleInfo, Options ^ tbo_functor_info, PPId,
        Context, VarTable, Zeros, SizeVarMap, SCC,
        Options ^ tbo_failure_constrs, Options ^ tbo_arg_size_only),

    % Traverse the HLDS and construct the abstract version of this procedure.
    build_abstract_goal(Goal, AbstractBody0, Info0, Info),
    IntermodStatus = Info ^ tti_intermod_status,
    HeadSizeVars   = prog_vars_to_size_vars(SizeVarMap, HeadProgVars),
    AbstractBody   = simplify_abstract_rep(AbstractBody0),

    % Work out which arguments can be used in termination proofs.
    % An argument may be used if (a) it is input and (b) it has non-zero size.
    ChooseArg =
        ( func(Var, Mode) = UseArg :-
            lookup_var_type(VarTable, Var, Type),
            ( if
                not zero_size_type(ModuleInfo, Type),
                mode_is_input(ModuleInfo, Mode)
            then
                UseArg = yes
            else
                UseArg = no
            )
        ),
    Inputs = list.map_corresponding(ChooseArg, HeadProgVars, ArgModes0),

    % The size_varset for this procedure is set to rubbish here.
    % When we complete building this SCC we will set it to the correct value.
    IsEntryPoint = ( if set.member(PPId, EntryProcs) then yes else no),
    AbstractProc = abstract_proc(real(PPId), Context,
        HeadSizeVars, Inputs, AbstractBody, SizeVarMap, !.SizeVarset, Zeros,
        IsEntryPoint, Info ^ tti_recursion, Info ^ tti_maxcalls,
        Info ^ tti_ho_info),

    ThisProcInfo = term_scc_info(PPId, AbstractProc, SizeVarMap,
        IntermodStatus, Info ^ tti_errors, HeadSizeVars),

    list.cons(ThisProcInfo, !AbstractInfo),

    trace [io(!DebugIO), compiletime(flag("term_constr_build"))] (
        get_debug_output_stream(ModuleInfo, DebugStream, !DebugIO),
        io.write_string(DebugStream, "Abstract proc is:\n", !DebugIO),
        dump_abstract_proc(DebugStream, ModuleInfo, 0, AbstractProc, !DebugIO),
        io.nl(DebugStream, !DebugIO)
    ).

%-----------------------------------------------------------------------------%
%
% Predicates for traversing HLDS goals and collecting constraints from them.
%

% While traversing the HLDS we accumulate the following information:
%
% * The type of recursion present in each procedure.
%
% * If the procedure may be involved in intermodule mutual recursion.
%
% * The number of calls in each procedure (We can use this information
%   to short-circuit edge labelling in pass 2).
%
% * Any calls that are made from the SCC being processed to lower SCCs
%   that do not terminate.

:- type tti_traversal_info
    --->    tti_traversal_info(
                % Do we only want to run IR analysis?
                % The `--term2-arg-size-analysis-only' option.
                tti_arg_analysis_only           :: bool,

                % If no then do not bother looking for failure constraints.
                % The `--no-term2-propagate-failure-constraints' options.
                tti_find_fail_constrs           :: bool,

                % What type of recursion is present in the procedure,
                % i.e. `none', `direct', `mutual'.
                tti_recursion                   :: recursion_type,

                % Record whether this procedure is potentially involved
                % in mutual recursion across module boundaries.
                tti_intermod_status             :: intermod_status,

                % Errors encountered while building the AR.
                tti_errors                      :: list(term2_error),

                % The HLDS.
                tti_module_info                 :: module_info,

                % The norm we are using.
                tti_norm                        :: functor_info,

                % The procedure we are currently processing.
                tti_ppid                        :: pred_proc_id,

                % The context of the current procedure.
                tti_context                     :: term.context,

                % Types for all prog_vars in the current procedure.
                tti_var_table                   :: var_table,

                % size_vars in the current procedure that are known
                % to have zero size.
                tti_zeros                       :: set(size_var),

                % Map from prog_vars to size_vars.
                tti_size_var_map                :: size_var_map,

                % The procedures in the SCC of the call graph
                % we are current traversing.
                tti_scc                         :: set(pred_proc_id),

                % The number of calls in the procedure.
                tti_maxcalls                    :: int,

                % Information about any higher-order calls a procedure makes.
                % XXX Currently unused.
                tti_ho_info                     :: list(abstract_ho_call)
        ).

:- func init_traversal_info(module_info, functor_info, pred_proc_id,
    term.context, var_table, zero_vars, size_var_map, scc, bool, bool)
    = tti_traversal_info.

init_traversal_info(ModuleInfo, Norm, PPId, Context, Types, Zeros,
        VarMap, SCC, FailConstrs, ArgSizeOnly)
    = tti_traversal_info(ArgSizeOnly, FailConstrs, none,
        not_mutually_recursive, [], ModuleInfo, Norm, PPId, Context,
        Types, Zeros, VarMap, SCC, 0, []).

:- pred info_increment_maxcalls(tti_traversal_info::in,
    tti_traversal_info::out) is det.

info_increment_maxcalls(!Info) :-
    !Info ^ tti_maxcalls := !.Info ^ tti_maxcalls + 1.

:- pred tti_info_add_error(term2_error::in,
    tti_traversal_info::in, tti_traversal_info::out) is det.

tti_info_add_error(Error, !Info) :-
    !Info ^ tti_errors := [Error | !.Info ^ tti_errors].

:- pred info_update_recursion(recursion_type::in,
    tti_traversal_info::in, tti_traversal_info::out) is det.

info_update_recursion(RecType, !Info) :-
    UpdatedRecType = combine_recursion_types(!.Info ^ tti_recursion, RecType),
    !Info ^ tti_recursion := UpdatedRecType.

:- pred info_update_ho_info(context::in,
    tti_traversal_info::in, tti_traversal_info::out) is det.

info_update_ho_info(Context, !Info) :-
    !Info ^ tti_ho_info := [ho_call(Context) | !.Info ^ tti_ho_info].

%-----------------------------------------------------------------------------%
%
% Predicates for abstracting goals.
%

% When constructing the abstract representation of the program
% this attaches the local variables to the abstract goal.
% (See comments about local variables in term_constr_data.m for more details.)

:- pred build_abstract_goal(hlds_goal::in, abstract_goal::out,
    tti_traversal_info::in, tti_traversal_info::out) is det.

build_abstract_goal(Goal, AbstractGoal, !Info) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    build_abstract_goal_2(GoalExpr, GoalInfo, AbstractGoal0, !Info),
    partition_vars(Goal, Locals0, NonLocals0),
    SizeVarMap = !.Info ^ tti_size_var_map,
    Locals = prog_vars_to_size_vars(SizeVarMap, Locals0),
    NonLocals = prog_vars_to_size_vars(SizeVarMap, NonLocals0),
    AbstractGoal = update_local_and_nonlocal_vars(AbstractGoal0,
        Locals, NonLocals).

:- pred build_abstract_goal_2(hlds_goal_expr::in, hlds_goal_info::in,
    abstract_goal::out, tti_traversal_info::in, tti_traversal_info::out)
    is det.

build_abstract_goal_2(GoalExpr, GoalInfo, AbstractGoal, !Info) :-
    (
        GoalExpr = conj(_, Goals),
        % For the purposes of termination analysis there is no
        % distinction between parallel conjunctions and normal ones.
        build_abstract_conj(Goals, AbstractGoal, !Info)
    ;
        GoalExpr = disj(Goals),
        build_abstract_disj(non_switch(Goals), AbstractGoal, !Info)
    ;
        GoalExpr = switch(SwitchVar, _, Cases),
        build_abstract_disj(switch(SwitchVar, Cases), AbstractGoal, !Info)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),

        % Reduce the if-then goals to an abstract conjunction.
        build_abstract_conj([Cond, Then], AbstractSuccessGoal, !Info),

        % Work out a failure constraint for the Cond and then abstract the else
        % branch. We won't bother do any other simplifications here as the AR
        % simplification pass will sort all of this out.
        CondFail = find_failure_constraint_for_goal(!.Info, Cond),

        % XXX FIXME - the local/non-local variable sets end up
        % being incorrect here.
        build_abstract_goal(Else, AbstractElse, !Info),
        AbstractFailureGoal = term_conj([CondFail, AbstractElse], [], []),
        AbstractDisjuncts = [AbstractSuccessGoal, AbstractFailureGoal],
        AbstractGoal = term_disj(AbstractDisjuncts, 2, [], [])
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(TermVar, from_ground_term_construct)
        then
            build_abstract_from_ground_term_goal(TermVar, SubGoal,
                AbstractGoal, !Info)
        else
            build_abstract_goal(SubGoal, AbstractGoal, !Info)
        )
    ;
        GoalExpr = plain_call(CallPredId, CallProcId, CallArgs, _, _, _),
        CallSizeArgs = prog_vars_to_size_vars(!.Info ^ tti_size_var_map,
            CallArgs),
        build_abstract_call(proc(CallPredId, CallProcId), CallSizeArgs,
            GoalInfo, AbstractGoal, !Info)
    ;
        GoalExpr = unify(_, _, _, Unification, _),
        build_abstract_unification(Unification, AbstractGoal, !Info)
    ;
        GoalExpr = negation(SubGoal),
        % Event though a negated goal cannot have any output we still need
        % to check it for calls to non-terminating procedures.
        build_abstract_goal(SubGoal, _, !Info),

        % Find a failure constraint for the goal if
        % `--term2-propagate-failure-constraints' is enabled,
        % otherwise just use the constraint that all non-zero input vars
        % should be non-negative.
        AbstractGoal = find_failure_constraint_for_goal(!.Info, SubGoal)
    ;
        GoalExpr = call_foreign_proc(Attrs, PredId, ProcId, Args, ExtraArgs,
            _, _),
        % XXX Eventually we should provide some facility for specifying the
        % arg_size constraints for foreign_procs.

        % Create non-negativity constraints for each non-zero argument
        % in the foreign proc.
        ForeignArgToVar = (func(ForeignArg) = ForeignArg ^ arg_var),
        ProgVars = list.map(ForeignArgToVar, Args ++ ExtraArgs),
        SizeVars = prog_vars_to_size_vars(!.Info ^ tti_size_var_map, ProgVars),
        Constraints = make_arg_constraints(SizeVars, !.Info ^ tti_zeros),
        ( if
            (
                get_terminates(Attrs) = proc_terminates
            ;
                get_terminates(Attrs) = depends_on_mercury_calls,
                get_may_call_mercury(Attrs) = proc_will_not_call_mercury
            )
        then
            true
        else
            Context = goal_info_get_context(GoalInfo),
            Error = term2_error(Context,
                foreign_proc_called(proc(PredId, ProcId))),
            tti_info_add_error(Error, !Info)
        ),
        Polyhedron = polyhedron.from_constraints(Constraints),
        AbstractGoal = term_primitive(Polyhedron, [], [])
    ;
        GoalExpr = generic_call(_, _, _, _, _),
        % XXX At the moment all higher-order calls are eventually treated
        % as an error. We do not record them as a normal type of error
        % because this is going to change. To approximate their effect here
        % just assume that any non-zero output variables from the HO call
        % are unbounded in size.
        %
        Context = goal_info_get_context(GoalInfo),
        AbstractGoal = term_primitive(polyhedron.universe, [], []),
        info_update_ho_info(Context, !Info)
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

%-----------------------------------------------------------------------------%
%
% Additional predicates for abstracting (parallel) conjunctions.
%

:- pred build_abstract_conj(hlds_goals::in, abstract_goal::out,
    tti_traversal_info::in, tti_traversal_info::out) is det.

build_abstract_conj(Conjuncts, AbstractGoal, !Info) :-
    list.map_foldl(build_abstract_goal,Conjuncts, AbstractGoals0, !Info),
    AbstractGoals = simplify_conjuncts(AbstractGoals0),
    AbstractGoal = term_conj(AbstractGoals, [], []).

%-----------------------------------------------------------------------------%
%
% Additional predicates for abstracting calls.
%

:- pred build_abstract_call(pred_proc_id::in, size_vars::in,
    hlds_goal_info::in, abstract_goal::out,
    tti_traversal_info::in, tti_traversal_info::out) is det.

build_abstract_call(CalleePPId, CallerArgs, GoalInfo, AbstractGoal, !Info) :-
    Context = goal_info_get_context(GoalInfo),
    ( if set.member(CalleePPId, !.Info ^ tti_scc) then
        build_recursive_call(CalleePPId, CallerArgs, Context, AbstractGoal,
            !Info)
    else
        build_non_recursive_call(CalleePPId, CallerArgs, Context, AbstractGoal,
            !Info)
    ).

    % If the call is potentially recursive, we construct an abstract call
    % to represent it - see term_constr_data.m for details.
    %
:- pred build_recursive_call(pred_proc_id::in, size_vars::in, prog_context::in,
    abstract_goal::out,
    tti_traversal_info::in, tti_traversal_info::out) is det.

build_recursive_call(CalleePPId, CallerArgs, Context, AbstractGoal, !Info) :-
    CallerPPId = !.Info ^ tti_ppid,
    CallerZeros = !.Info ^ tti_zeros,
    ( if CallerPPId = CalleePPId then
        info_update_recursion(direct_only, !Info)
    else
        info_update_recursion(mutual_only, !Info)
    ),
    CallerArgConstrs = make_arg_constraints(CallerArgs, CallerZeros),
    CallerArgPoly = polyhedron.from_constraints(CallerArgConstrs),
    info_increment_maxcalls(!Info),
    AbstractGoal = term_call(real(CalleePPId), Context, CallerArgs,
        CallerZeros, [], [], CallerArgPoly).

    % For non-recursive calls look up the argument size constraints for the
    % callee procedure and build an abstract primitive goal to store them.
    %
    % If we are running termination analysis, as opposed to just the IR
    % analysis then we also need to check that the termination status of the
    % callee procedure.
    %
:- pred build_non_recursive_call(pred_proc_id::in, size_vars::in,
    prog_context::in, abstract_goal::out,
    tti_traversal_info::in, tti_traversal_info::out) is det.

build_non_recursive_call(CalleePPId, CallerArgs, Context, AbstractGoal,
        !Info) :-
    ModuleInfo = !.Info ^ tti_module_info,
    CallerPPId = !.Info ^ tti_ppid,
    ZeroVars = !.Info ^ tti_zeros,
    module_info_pred_proc_info(ModuleInfo, CalleePPId, _, CalleeProcInfo),

    % Check the termination status of the callee procedure if we are running a
    % full analysis - ignore it if we are only running the IR analysis.
    proc_info_get_termination2_info(CalleeProcInfo, CalleeTerm2Info),
    ArgAnalysisOnly = !.Info ^ tti_arg_analysis_only,
    (
        ArgAnalysisOnly = no,
        MaybeTermStatus = term2_info_get_term_status(CalleeTerm2Info),
        (
            MaybeTermStatus = yes(TermStatus),
            (
                TermStatus = can_loop(_),
                Error = term2_error(Context,
                    can_loop_proc_called(CallerPPId, CalleePPId)),
                tti_info_add_error(Error, !Info)
            ;
                TermStatus = cannot_loop(_)
            )
        ;
            MaybeTermStatus = no,
            unexpected($pred, "callee procedure has no termination status.")
        )
    ;
        ArgAnalysisOnly = yes
    ),

    % Check the arg_size_info for the procedure being called.
    ArgSizeInfo = term2_info_get_success_constrs(CalleeTerm2Info),
    (
        ArgSizeInfo = no,
        unexpected($pred, "no argument size info for callee proc")
    ;
        ArgSizeInfo = yes(SizeInfo),
        ArgSizeConstrs0 = polyhedron.non_false_constraints(SizeInfo),
        (
            ArgSizeConstrs0 = [],
            Constraints = []
        ;
            ArgSizeConstrs0 = [_ | _],
            CalleeHeadVars = term2_info_get_head_vars(CalleeTerm2Info),
            SubstMap = create_var_substitution(CallerArgs, CalleeHeadVars),
            Constraints0 = lp_rational.substitute_vars(SubstMap,
                ArgSizeConstrs0),
            Constraints = lp_rational.set_vars_to_zero(ZeroVars, Constraints0)
        )
    ),
    Polyhedron = polyhedron.from_constraints(Constraints),
    AbstractGoal = term_primitive(Polyhedron, [], []).

%-----------------------------------------------------------------------------%
%
% Additional predicates for abstracting switches and disjunctions.
%

% NOTE: for switches and disjunctions we add the variables that
% are local to the entire switch/disjunction to the list of variables
% that are local to each case/disjunct. The reason for this is that
% the projection operation distributes over the convex hull operation
% and it is more efficient to eliminate the variables from each branch
% *before* taking the convex hull. This is because the transformation
% matrix used by the convex hull operation (see polyhedron.m) will
% usually be much larger for the entire disjunction than the matrix used
% for each case/disjunct.

:- type disj_info
    --->    switch(prog_var, list(case))
    ;       non_switch(hlds_goals).

:- pred build_abstract_disj(disj_info::in, abstract_goal::out,
    tti_traversal_info::in, tti_traversal_info::out) is det.

build_abstract_disj(Type, AbstractGoal, !Info) :-
    (
        Type = non_switch(Goals),
        build_abstract_disj_acc(Goals, [], AbstractGoals, !Info)
    ;
        Type = switch(SwitchVar, Cases),
        build_abstract_switch_acc(SwitchVar, Cases, [], AbstractGoals, !Info)
    ),
    (
        AbstractGoals = [],
        AbstractGoal = term_primitive(polyhedron.universe, [], [])
    ;
        AbstractGoals = [Goal],
        AbstractGoal = Goal
    ;
        AbstractGoals = [_, _ | _],
        DisjSize = list.length(AbstractGoals),
        AbstractGoal = term_disj(AbstractGoals, DisjSize, [], [])
    ).

:- pred build_abstract_disj_acc(hlds_goals::in, abstract_goals::in,
    abstract_goals::out,
    tti_traversal_info::in, tti_traversal_info::out) is det.

build_abstract_disj_acc([], !AbstractGoals, !Info).
build_abstract_disj_acc([Goal | Goals], !AbstractGoals, !Info) :-
    build_abstract_goal(Goal, AbstractGoal, !Info),
    list.cons(AbstractGoal, !AbstractGoals),
    build_abstract_disj_acc(Goals, !AbstractGoals, !Info).

    % With switches we need to consider the constraints on the variable
    % being switched on as well as those from the body of each case.
    %
    % For each case, we check if there is a deconstruction unification
    % involving the switch variable. If there is no such unification then
    % the constraint for the case will not include a constraint on the size
    % of the switched-on var. In that case we add an appropriate constraint.
    %
    % We add the extra constraint by creating a new primitive abstract
    % goal and conjoining that to the rest.
    %
:- pred build_abstract_switch_acc(prog_var::in, list(case)::in,
    abstract_goals::in, abstract_goals::out,
    tti_traversal_info::in, tti_traversal_info::out) is det.

build_abstract_switch_acc(_, [], !AbstractGoals, !Info).
build_abstract_switch_acc(SwitchProgVar, [Case | Cases], !AbstractGoals,
        !Info) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    build_abstract_goal(Goal, AbstractGoal0, !Info),

    % We now need to check that constraints on the switch var are included.
    % They will *not* have been included if the case did not contain a
    % unification deconstructing that variable (which it can't contain if the
    % switch arm is for several cons_ids). They are of course in the HLDS,
    % just not stored in a way we can derive them from the goal in the normal
    % fashion unless there is actually a deconstruction unification present.
    %
    % XXX Why do we ignore OtherConsIds when it is not []?

    ( if
        OtherConsIds = [],
        detect_switch_var(Goal, SwitchProgVar, MainConsId)
    then
        AbstractGoal = AbstractGoal0
    else
        VarTable = !.Info ^ tti_var_table,
        SizeVarMap = !.Info ^ tti_size_var_map,
        lookup_var_type(VarTable, SwitchProgVar, SwitchVarType),
        SwitchSizeVar = prog_var_to_size_var(SizeVarMap, SwitchProgVar),
        type_to_ctor_det(SwitchVarType, TypeCtor),
        ModuleInfo = !.Info ^ tti_module_info,
        Norm = !.Info ^ tti_norm,
        Zeros = !.Info ^ tti_zeros,
        Size = functor_lower_bound(ModuleInfo, Norm, TypeCtor, MainConsId),
        ( if set.member(SwitchSizeVar, Zeros) then
            ExtraConstr = []
        else
            SwitchVarConst = rat(Size),
            ( if Size = 0 then
                SwitchVarConstr =
                    make_var_const_eq_constraint(SwitchSizeVar,
                        SwitchVarConst)
            else
                SwitchVarConstr =
                    make_var_const_gte_constraint(SwitchSizeVar,
                        SwitchVarConst)
            ),
            ExtraConstr = [SwitchVarConstr]
        ),
        ExtraPoly = polyhedron.from_constraints(ExtraConstr),
        ExtraGoal = term_primitive(ExtraPoly, [], []),
        AbstractGoal = term_conj([ExtraGoal, AbstractGoal0], [], [])
    ),
    list.cons(AbstractGoal, !AbstractGoals),
    build_abstract_switch_acc(SwitchProgVar, Cases, !AbstractGoals, !Info).

:- pred detect_switch_var(hlds_goal::in, prog_var::in, cons_id::in) is semidet.

detect_switch_var(hlds_goal(unify(_, _, _, Kind, _), _), SwitchVar, ConsId) :-
    (
        Kind = deconstruct(SwitchVar, ConsId, _, _, _, _)
    ;
        Kind = complicated_unify(_, _, _),
        unexpected($pred, "complicated_unify")
    ;
        ( Kind = construct(_, _, _, _, _, _, _)
        ; Kind = assign(_, _)
        ; Kind = simple_test(_, _)
        ),
        fail
    ).
detect_switch_var(hlds_goal(shorthand(_), _), _, _) :-
    unexpected($pred, "shorthand").

%-----------------------------------------------------------------------------%
%
% Additional predicates for abstracting from_ground_term scopes,
% which act like giant construction unifications.
%

:- pred build_abstract_from_ground_term_goal(prog_var::in, hlds_goal::in,
    abstract_goal::out, tti_traversal_info::in, tti_traversal_info::out)
    is det.

build_abstract_from_ground_term_goal(TermVar, SubGoal, AbstractGoal, !Info) :-
    SubGoal = hlds_goal(SubGoalExpr, _SubGoalInfo),
    ( if SubGoalExpr = conj(plain_conj, Conjuncts) then
        SizeVarMap = !.Info ^ tti_size_var_map,
        Zeros = !.Info ^ tti_zeros,
        TermSizeVar = prog_var_to_size_var(SizeVarMap, TermVar),
        ( if set.member(TermSizeVar, Zeros) then
            Constraints = []
        else
            ModuleInfo = !.Info ^ tti_module_info,
            Norm = !.Info ^ tti_norm,
            VarTable = !.Info ^ tti_var_table,
            abstract_from_ground_term_conjuncts(ModuleInfo, Norm, VarTable,
                Conjuncts, map.init, SizeMap),
            map.lookup(SizeMap, TermVar, KnownTermVarSize),
            Terms = [TermSizeVar - one],
            Constraint =
                construct_constraint(Terms, lp_eq, rat(KnownTermVarSize)),
            Constraints = [Constraint]
        ),
        AbstractGoal = build_goal_from_unify(Constraints)
    else
        unexpected($pred, "not conj")
    ).

:- pred abstract_from_ground_term_conjuncts(module_info::in, functor_info::in,
    var_table::in, list(hlds_goal)::in,
    map(prog_var, int)::in, map(prog_var, int)::out) is det.

abstract_from_ground_term_conjuncts(_ModuleInfo, _Norm, _VarTable, [],
        !SizeMap).
abstract_from_ground_term_conjuncts(ModuleInfo, Norm, VarTable, [Goal | Goals],
        !SizeMap) :-
    abstract_from_ground_term_conjunct(ModuleInfo, Norm, VarTable, Goal,
        !SizeMap),
    abstract_from_ground_term_conjuncts(ModuleInfo, Norm, VarTable, Goals,
        !SizeMap).

:- pred abstract_from_ground_term_conjunct(module_info::in, functor_info::in,
    var_table::in, hlds_goal::in,
    map(prog_var, int)::in, map(prog_var, int)::out) is det.

abstract_from_ground_term_conjunct(ModuleInfo, Norm, VarTable, Goal,
        !SizeMap) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    ( if
        GoalExpr = unify(_, _, _, Unify, _),
        Unify = construct(Var, ConsId, ArgVars, Modes, _, _, _)
    then
        strip_typeinfos_from_args_and_modes(VarTable, ArgVars, FixedArgVars,
            Modes, FixedModes),
        lookup_var_type(VarTable, Var, Type),
        type_to_ctor_det(Type, TypeCtor),
        functor_norm(ModuleInfo, Norm, TypeCtor, ConsId, ConsIdSize,
            FixedArgVars, CountedVars, FixedModes, _),

        % Note that any vars that are in ArgVars but not in CountedVars
        % will be left in !:SizeMap, which is a performance problem (but not
        % correctness problem) for the later goals in the conjunction.
        list.map_foldl(map.det_remove, CountedVars, ArgSizes, !SizeMap),
        accumulate_sum(ArgSizes, 0, TotalArgSize),
        Size = ConsIdSize + TotalArgSize,
        map.det_insert(Var, Size, !SizeMap)
    else
        unexpected($pred, "malformed conjunct")
    ).

:- pred accumulate_sum(list(int)::in, int::in, int::out) is det.

accumulate_sum([], !TotalSize).
accumulate_sum([Size | Sizes], !TotalSize) :-
    !:TotalSize = !.TotalSize + Size,
    accumulate_sum(Sizes, !TotalSize).

%-----------------------------------------------------------------------------%
%
% Additional predicates for abstracting unifications.
%

:- pred build_abstract_unification(unification::in, abstract_goal::out,
    tti_traversal_info::in, tti_traversal_info::out) is det.

build_abstract_unification(Unification, AbstractGoal, !Info) :-
    (
        Unification = construct(Var, ConsId, ArgVars, Modes, _, _, _),
        build_abstract_decon_or_con_unify(Var, ConsId, ArgVars, Modes,
            Constraints, !Info),
        AbstractGoal = build_goal_from_unify(Constraints)
    ;
        Unification = deconstruct(Var, ConsId, ArgVars, Modes, _, _),
        build_abstract_decon_or_con_unify(Var, ConsId, ArgVars, Modes,
            Constraints, !Info),
        AbstractGoal = build_goal_from_unify(Constraints)
    ;
        Unification = assign(LVar, RVar),
        build_abstract_simple_or_assign_unify(LVar, RVar, Constraints, !Info),
        AbstractGoal = build_goal_from_unify(Constraints)
    ;
        Unification = simple_test(LVar, RVar),
        build_abstract_simple_or_assign_unify(LVar, RVar, Constraints, !Info),
        AbstractGoal = build_goal_from_unify(Constraints)
    ;
        Unification = complicated_unify(_, _, _),
        unexpected($pred, "complicated_unify")
    ).

    % Used for deconstruction and construction unifications, i.e. for
    % unifications of the form: X = f(U, V, W). If the norm counts the
    % first and second arguments, then the constraint returned is |X| -
    % |U| - |V| = |f|. (|X| is the size_var corresponding to X).
    %
:- pred build_abstract_decon_or_con_unify(prog_var::in, cons_id::in,
    prog_vars::in, list(unify_mode)::in, constraints::out,
    tti_traversal_info::in, tti_traversal_info::out) is det.

build_abstract_decon_or_con_unify(Var, ConsId, ArgVars, Modes, Constraints,
        !Info) :-
    VarTable = !.Info ^ tti_var_table,
    lookup_var_type(VarTable, Var, Type),
    ( if
        % The only valid higher-order unifications are assignments.
        % For the purposes of the IR analysis, we can ignore them.
        % We can also ignore unifications that build constant terms.
        % XXX Should we process constant terms that are NOT typeinfos
        % or typeclass infos? We have no test cases (yet) that need that.
        ( type_is_higher_order(Type)
        ; cons_id_is_const_struct(ConsId, _)
        )
    then
        Constraints = []
    else
        % We need to strip out any typeinfo related variables before
        % measuring the size of the term; otherwise functor_norm will
        % raise a software error if we are using the `num-data-elems'
        % norm and the term has existential typeclass constraints.

        strip_typeinfos_from_args_and_modes(VarTable, ArgVars, FixedArgVars,
            Modes, FixedModes),
        ModuleInfo = !.Info ^ tti_module_info,
        Norm = !.Info ^ tti_norm,
        type_to_ctor_det(Type, TypeCtor),
        functor_norm(ModuleInfo, Norm, TypeCtor, ConsId, Constant,
            FixedArgVars, CountedVars, FixedModes, _),

        % The constraint from this unification is:
        %
        %      |Var| = Constant + sum(CountedVars)
        %
        % |Var| is just the size_var corresponding to Var. The value of
        % `Constant' will depend upon the norm being used.

        SizeVarMap = !.Info ^ tti_size_var_map,
        Zeros = !.Info ^ tti_zeros,

        SizeVar = prog_var_to_size_var(SizeVarMap, Var),
        ( if set.member(SizeVar, Zeros) then
            FirstTerms = []
        else
            FirstTerms = [SizeVar - one]
        ),
        list.foldl(accumulate_nonzero_arg_coeffs(SizeVarMap, Zeros, -one),
            CountedVars, FirstTerms, Terms),
        Constraint = construct_constraint(Terms, lp_eq, rat(Constant)),
        ( if is_false(Constraint) then
            unexpected($pred, "false constraint from unification")
        else
            SizeVars0 = prog_vars_to_size_vars(SizeVarMap, ArgVars),
            SizeVars1 = [SizeVar | SizeVars0],
            SizeVars  = list.filter(isnt(is_zero_size_var(Zeros)), SizeVars1)
        ),
        NonNegConstraints = list.map(make_nonneg_constr, SizeVars),
        Constraints = [Constraint | NonNegConstraints]
    ).

:- pred accumulate_nonzero_arg_coeffs(size_var_map::in, set(size_var)::in,
    lp_coefficient::in, prog_var::in, lp_terms::in, lp_terms::out) is det.

accumulate_nonzero_arg_coeffs(SizeVarMap, Zeros, Coeff, Var, !Terms) :-
    SizeVar = prog_var_to_size_var(SizeVarMap, Var),
    ( if set.member(SizeVar, Zeros) then
        true
    else
        !:Terms = [SizeVar - Coeff | !.Terms]
    ).

:- pred strip_typeinfos_from_args_and_modes(var_table::in,
    list(prog_var)::in, list(prog_var)::out,
    list(unify_mode)::in, list(unify_mode)::out) is det.

strip_typeinfos_from_args_and_modes(VarTable, !Args, !Modes) :-
    ( if strip_typeinfos_from_args_and_modes_2(VarTable, !Args, !Modes) then
        true
    else
        unexpected($pred, "unequal length lists")
    ).

:- pred strip_typeinfos_from_args_and_modes_2(var_table::in,
    list(prog_var)::in, list(prog_var)::out,
    list(unify_mode)::in, list(unify_mode)::out) is semidet.

strip_typeinfos_from_args_and_modes_2(_, [], [], [], []).
strip_typeinfos_from_args_and_modes_2(VarTable, [Arg | !.Args], !:Args,
        [Mode | !.Modes], !:Modes) :-
    strip_typeinfos_from_args_and_modes_2(VarTable, !Args, !Modes),
    lookup_var_type(VarTable, Arg, Type),
    ( if is_introduced_type_info_type(Type) then
        true
    else
        list.cons(Arg, !Args),
        list.cons(Mode, !Modes)
    ).

    % Assignment and simple_test unifications of the form X = Y
    % are abstracted as |X| - |Y| = 0.
    %
:- pred build_abstract_simple_or_assign_unify(prog_var::in, prog_var::in,
    constraints::out, tti_traversal_info::in, tti_traversal_info::out) is det.

build_abstract_simple_or_assign_unify(LeftProgVar, RightProgVar, Constraints,
        !Info) :-
    SizeVarMap = !.Info ^ tti_size_var_map,
    Zeros = !.Info ^ tti_zeros,
    LeftSizeVar = prog_var_to_size_var(SizeVarMap, LeftProgVar),
    RightSizeVar = prog_var_to_size_var(SizeVarMap, RightProgVar),
    ( if
        set.member(LeftSizeVar, Zeros),
        set.member(RightSizeVar, Zeros)
    then
        Constraints = []    % `true' constraint.
    else if
        ( set.member(LeftSizeVar, Zeros)
        ; set.member(RightSizeVar, Zeros)
        )
    then
        unexpected($pred, "zero unified with non-zero")
    else
        % Create non-negativity constraints.
        NonNegConstrs = list.map(make_nonneg_constr,
            [LeftSizeVar, RightSizeVar]),
        Terms = [LeftSizeVar - one, RightSizeVar - (-one)],
        AssignConstr = construct_constraint(Terms, lp_eq, zero),
        % XXX I don't think this call to simplify helps anymore.
        Constraints = simplify_constraints([AssignConstr | NonNegConstrs])
    ).

    % Check that the abstraction of a unification has not resulted
    % in the false constraint.
    %
:- func build_goal_from_unify(constraints) = abstract_goal.

build_goal_from_unify(Constraints) = term_primitive(Polyhedron, [], []) :-
    Polyhedron = polyhedron.from_constraints(Constraints),
    ( if polyhedron.is_empty(Polyhedron) then
        unexpected($pred, "empty polyhedron from unification")
    else
        true
    ).

%-----------------------------------------------------------------------------%

    % Partition the variables of a goal into a set of local variables
    % and a set of non-local variables.
    %
:- pred partition_vars(hlds_goal::in, prog_vars::out, prog_vars::out) is det.

partition_vars(hlds_goal(GoalExpr, GoalInfo), Locals, NonLocals) :-
    NonLocals0 = goal_info_get_nonlocals(GoalInfo),
    QuantVars = free_goal_vars(hlds_goal(GoalExpr, GoalInfo)),
    Locals = set_of_var.to_sorted_list(
        set_of_var.difference(QuantVars, NonLocals0)),
    NonLocals = set_of_var.to_sorted_list(NonLocals0).

%-----------------------------------------------------------------------------%
%
% Procedures for manipulating sets of size_vars.
%

    % Create the size_vars corresponding to the given prog_vars.
    % Also create map from the prog_vars to the size_vars.
    %
    % As termination analysis is (currently) carried out before unused
    % argument analysis it is possible that some variables in the head
    % of a procedure may not occur in the body (this typically occurs
    % with typeinfos).
    %
:- pred allocate_sizevars(prog_vars::in, hlds_goal::in, size_var_map::out,
    size_varset::in, size_varset::out) is det.

allocate_sizevars(HeadProgVars, Goal, SizeVarMap, !SizeVarset) :-
    fill_var_to_sizevar_map(Goal, !SizeVarset, SizeVarMap0),
    possibly_fix_sizevar_map(HeadProgVars, !SizeVarset,
        SizeVarMap0, SizeVarMap).

:- pred fill_var_to_sizevar_map(hlds_goal::in,
    size_varset::in, size_varset::out, size_var_map::out) is det.

fill_var_to_sizevar_map(Goal, !SizeVarset, SizeVarMap) :-
    ProgVarsInGoal = free_goal_vars(Goal),
    ProgVars = set_of_var.to_sorted_list(ProgVarsInGoal),
    make_size_var_map_alloc_from(ProgVars, !SizeVarset, SizeVarMap).

    % Fix the map in case some variables that are present only
    % in the head of a procedure were missed.
    %
:- pred possibly_fix_sizevar_map(prog_vars::in, size_varset::in,
    size_varset::out, size_var_map::in, size_var_map::out) is det.

possibly_fix_sizevar_map([], !SizeVarset, !SizeVarMap).
possibly_fix_sizevar_map([ProgVar | ProgVars], !SizeVarset, !SizeVarMap) :-
    ( if map.search(!.SizeVarMap, ProgVar, _) then
        possibly_fix_sizevar_map(ProgVars, !SizeVarset, !SizeVarMap)
    else
        varset.new_var(SizeVar, !SizeVarset),
        map.set(ProgVar, SizeVar, !SizeVarMap),
        possibly_fix_sizevar_map(ProgVars, !SizeVarset, !SizeVarMap)
    ).

%-----------------------------------------------------------------------------%
%
% Failure constraints.
%

% The idea here is that if a goal can fail, the fact that it fails
% may tell us information about the size of any input arguments.
%
% For unifications, both deconstructions and simple tests can fail but
% since the latter involves only zero size types it does not tell us
% anything useful. For a deconstruction unification that can fail we
% know that the variable must be bound to one of the other type
% constructors so we can use this to try and place a lower bound on the
% size of the variable.
%
% For calls we can associate a failure constraint with each procedure in
% the program. In contexts where the call fails we can just look up the
% failure constraint.
%
% In the worst case we just assume that the size of the (non-zero)
% inputs is unbounded.
%
% TODO Better failure constraints for goals other than unifications.

:-  func find_failure_constraint_for_goal(tti_traversal_info, hlds_goal)
    = abstract_goal.

find_failure_constraint_for_goal(Info, Goal) = AbstractGoal :-
    ( if
        Info ^ tti_find_fail_constrs = yes,
        find_failure_constraint_for_goal_2(Info, Goal, AbstractGoal0)
    then
        AbstractGoal = AbstractGoal0
    else
        NonLocalProgVars0 = goal_info_get_nonlocals(Goal ^ hg_info),
        NonLocalProgVars = set_of_var.to_sorted_list(NonLocalProgVars0),
        NonLocalSizeVars = prog_vars_to_size_vars(Info ^ tti_size_var_map,
            NonLocalProgVars),
        Constraints = make_arg_constraints(NonLocalSizeVars,
            Info ^ tti_zeros),
        FailPoly = polyhedron.from_constraints(Constraints),
        AbstractGoal = term_primitive(FailPoly, [], [])
    ).

:- pred find_failure_constraint_for_goal_2(tti_traversal_info::in,
    hlds_goal::in, abstract_goal::out) is semidet.

find_failure_constraint_for_goal_2(Info, Goal, AbstractGoal) :-
    % XXX We could factor out a lot of the code used for
    % substitutions below as the same code is used elsewhere.

    Goal = hlds_goal(GoalExpr, _),
    (
        GoalExpr = plain_call(PredId, ProcId, CallArgs, _, _, _),
        SizeVarMap = Info ^ tti_size_var_map,
        CallSizeArgs0 = prog_vars_to_size_vars(SizeVarMap, CallArgs),
        Zeros = Info ^ tti_zeros,
        CallSizeArgs = list.filter(isnt(is_zero_size_var(Zeros)),
            CallSizeArgs0),
        ModuleInfo = Info ^ tti_module_info,
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
        proc_info_get_termination2_info(ProcInfo, Term2Info),
        MaybeFailureConstrs = term2_info_get_failure_constrs(Term2Info),
        (
            MaybeFailureConstrs = no,
            FailureConstraints = []
        ;
            MaybeFailureConstrs = yes(CalleeFailurePolyhedron),
            CalleeFailureConstraints =
                polyhedron.non_false_constraints(CalleeFailurePolyhedron),
            (
                CalleeFailureConstraints = [],
                FailureConstraints = []
            ;
                CalleeFailureConstraints = [_ | _],
                CalleeHeadVars = term2_info_get_head_vars(Term2Info),
                SubstMap =
                    create_var_substitution(CallSizeArgs, CalleeHeadVars),
                FailureConstraints =
                    substitute_size_vars(CalleeFailureConstraints, SubstMap)
            )
        ),
        FailurePolyhedron = polyhedron.from_constraints(FailureConstraints),
        AbstractGoal = term_primitive(FailurePolyhedron, [], [])
    ;
        % Given a deconstruction unification and assuming that it has failed,
        % find a bound on the size of the variable being deconstructed.

        GoalExpr = unify(_, _, _, Unification, _),
        Unification = deconstruct(Var, ConsId, _, _, can_fail, _),
        lookup_var_type(Info ^ tti_var_table, Var, Type),
        type_to_ctor_det(Type, TypeCtor),
        ModuleInfo = Info ^ tti_module_info,
        type_util.type_constructors(ModuleInfo, Type, Constructors0),
        ( if ConsId = cons(ConsName, ConsArity, ConsTypeCtor) then
            expect(unify(TypeCtor, ConsTypeCtor), $pred,
                "mismatched type_ctors"),
            FindComplement =
                ( pred(Ctor::in) is semidet :-
                    Ctor = ctor(_, _, SymName, _Args, Arity, _),
                    not (
                        SymName = ConsName,
                        Arity   = ConsArity
                    )
                ),
            list.filter(FindComplement, Constructors0, Constructors)
        else
            unexpected($pred, "non cons cons_id.")
        ),
        SizeVarMap = Info ^ tti_size_var_map,
        SizeVar = prog_var_to_size_var(SizeVarMap, Var),
        Norm = Info ^ tti_norm,
        bounds_on_var(Norm, ModuleInfo, TypeCtor, SizeVar, Constructors,
            Polyhedron),
        AbstractGoal = term_primitive(Polyhedron, [], [])
    ).

    % Given a variable, its type and a list of constructors to which
    % it could be bound, return a polyhedron representing the bounds
    % on the size of that variable.
    %
:- pred bounds_on_var(functor_info::in, module_info::in, type_ctor::in,
    size_var::in, list(constructor)::in, polyhedron::out) is det.

bounds_on_var(Norm, ModuleInfo, TypeCtor, Var, Constructors, Polyhedron) :-
    CtorSizes = list.map(lower_bound(Norm, ModuleInfo, TypeCtor),
        Constructors),

    % Split constructors into those that have zero size and
    % those that have non-zero size.
    list.filter((pred(V::in) is semidet :- V = 0), CtorSizes,
        ZeroSizeCtors, NonZeroSizeCtors),
    (
        NonZeroSizeCtors = [],
        (
            ZeroSizeCtors = [],
            unexpected($pred, "no other constructors for type")
        ;
            ZeroSizeCtors = [_ | _],
            Constraints = [construct_constraint([Var - one], lp_eq, zero)]
        )
    ;
        NonZeroSizeCtors = [C | Cs],
        upper_bound_constraints(Norm, ModuleInfo, Var, TypeCtor,
            Constructors, UpperBoundConstr),
        (
            ZeroSizeCtors = [],
            LowerBound = list.foldl(int.min, Cs, C),
            LowerBoundConstr =
                [construct_constraint([Var - one], lp_gt_eq, rat(LowerBound))]
        ;
            ZeroSizeCtors = [_ | _],
            LowerBoundConstr =
                [construct_constraint([Var - one], lp_gt_eq, zero)]
        ),
        Constraints = LowerBoundConstr ++ UpperBoundConstr
    ),
    Polyhedron = polyhedron.from_constraints(Constraints).

:- func lower_bound(functor_info, module_info, type_ctor, constructor) = int.

lower_bound(Norm, ModuleInfo, TypeCtor, Constructor) = LowerBound :-
    Constructor = ctor(_, _, SymName, _Args, Arity, _),
    ConsId = cons(SymName, Arity, TypeCtor),
    LowerBound = functor_lower_bound(ModuleInfo, Norm, TypeCtor, ConsId).

    % Given a variable, its type and a set of constructors to which it
    % could be bound, return a constraint that specifies an upper bound
    % on the size of the variable. An empty list means that there is no
    % upper bound.
    %
:- pred upper_bound_constraints(functor_info::in, module_info::in,
    size_var::in, type_ctor::in, list(constructor)::in, constraints::out)
    is det.

upper_bound_constraints(Norm, ModuleInfo, Var, TypeCtor, Ctors, Constraints) :-
    % If all the arguments of a functor are zero sized then we can give
    % an upper bound on its size. If we have a set of such functors
    % then the upper bound is the maximum of the individual upper bounds.
    %
    % XXX We could extend this to include functors can only have a
    % finite size but I'm not sure that it's worth it.

    FindUpperBound = (pred(Ctor::in, !.B::in, !:B::out) is semidet :-
        Ctor = ctor(_, _, SymName, Args, Arity, _),
        all [Arg] (
            list.member(Arg, Args)
        =>
            zero_size_type(ModuleInfo, Arg ^ arg_type)
        ),
        ConsId = cons(SymName, Arity, TypeCtor),
        Bound = functor_lower_bound(ModuleInfo, Norm, TypeCtor, ConsId),
        ( if Bound > !.B then !:B = Bound else true )
    ),
    ( if list.foldl(FindUpperBound, Ctors, 0, Bound0) then
        ( if Bound0 = 0 then
            unexpected($pred, "zero upper bound")
        else
            Constraints =
                [construct_constraint([Var - one], lp_lt_eq, rat(Bound0))]
        )
    else
        Constraints = []
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.term_constr_build.
%-----------------------------------------------------------------------------%
