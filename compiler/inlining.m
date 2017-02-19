%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: inlining.m.
% Main author: conway.
%
% This module inlines
%
% * (--inline-simple and --inline-simple-threshold N)
%   procedures whose size is below the given threshold,
%   PLUS
%   procedures that are flat (i.e. contain no branched structures)
%   and are composed of inline builtins (eg arithmetic),
%   and whose size is less than three times the given threshold
%   (XXX shouldn't hard-code 3)
%
% * (--inline-compound-threshold N)
%   procedures where the product of the number of calls to them
%   and their size is below a given threshold.
%
% * (--inline-single-use)
%   procedures which are called only once
%
% * procedures which have a `:- pragma inline(name/arity).'
%
% It will not inline procedures which have a `:- pragma no_inline(name/arity).'
%
% If inlining a procedure takes the total number of variables over a given
% threshold (from a command-line option), then the procedure is not inlined
% - note that this means that some calls to a procedure may inlined while
% others are not.
%
% It builds the call-graph (if necessary) works from the bottom of the
% call-graph towards the top, first performing inlining on a procedure,
% then deciding if calls to it (higher in the call-graph) should be inlined.
% SCCs get flattened and processed in the order returned by
% hlds_dependency_info_get_dependency_ordering.
%
% There are a couple of classes of procedure that we clearly want to inline
% because doing so *reduces* the size of the generated code:
%
% - access predicates that get or set one or more fields of a structure
%   Inlining these is almost always a win because the infrastructure for the
%   call to the procedure is almost always larger than the code to do the
%   access. In the case of `get' accessors, the call usually becomes a single
%   `field' expression to get the relevant field of the structure. In the case
%   of `set' accessors, it is a bit more complicated since the code to copy
%   the fields can be quite big if there are lots of fields. However, in the
%   frequent case where several `set' accessors get called one after the other,
%   inlining them all enables the code generator to avoid creating all the
%   intermediate structures, which is usually a significant win.
%
% - arithmetic predicates where as above, the cost of the call will often
%   outweigh the cost of the arithmetic.
%
% - det or semidet foreign_proc code, where the foreign code is often
%   very small; inlining avoids a call and allows the target language
%   compiler to do a better job of optimizing it.
%
% The threshold on the size of simple goals (which covers both of the first
% two cases above), is to prevent the inlining of large goals such as those
% that construct big terms where the duplication is usually inappropriate
% (for example in nrev).
%
% The threshold on the number of variables in a procedure is to prevent the
% problem of inlining lots of calls and having a resulting procedure with so
% many variables that the back end of the compiler gets bogged down (for
% example in the pseudoknot benchmark).
%
% Due to the way in which we generate code for model_non pragma_foreign_code,
% procedures whose body is such a pragma_foreign_code must NOT be inlined.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.inlining.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.vartypes.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module map.

%-----------------------------------------------------------------------------%

:- pred inlining(module_info::in, module_info::out) is det.

    % This heuristic is used for both local and intermodule inlining.
    %
:- pred is_simple_clause_list(list(clause)::in, int::in) is semidet.

:- pred is_simple_goal(hlds_goal::in, int::in) is semidet.

    % do_inline_call(UnivQVars, Args, CalledPredInfo, CalledProcInfo,
    %   !VarSet, !VarTypes, !TVarSet, !RttiVarMaps):
    %
    % Given the universally quantified type variables in the caller's type,
    % the arguments to the call, the pred_info and proc_info for the called
    % goal and various information about the variables and types in the
    % procedure currently being analysed, rename the goal for the called
    % procedure so that it can be inlined.
    %
:- pred do_inline_call(list(tvar)::in, list(prog_var)::in,
    pred_info::in, proc_info::in, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, tvarset::in, tvarset::out,
    rtti_varmaps::in, rtti_varmaps::out, hlds_goal::out) is det.

    % get_type_substitution(CalleeArgTypes, CallerArgTypes,
    %   ExternalTypeParams, CalleeExistQTVars, TypeSubn):
    %
    % Work out a type substitution to map the callee's argument types
    % into the caller's.
    %
:- pred get_type_substitution(list(mer_type)::in, list(mer_type)::in,
    external_type_params::in, list(tvar)::in, map(tvar, mer_type)::out) is det.

    % rename_goal(CalledProcHeadVars, CallArgs,
    %   CallerVarSet0, CalleeVarSet, CallerVarSet,
    %   CallerVarTypes0, CalleeVarTypes, CallerVarTypes,
    %   VarRenaming, CalledGoal, RenamedGoal).
    %
:- pred rename_goal(list(prog_var)::in, list(prog_var)::in,
    prog_varset::in, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::in, vartypes::out,
    map(prog_var, prog_var)::out, hlds_goal::in, hlds_goal::out) is det.

    % can_inline_proc(PredId, ProcId, BuiltinState,
    %   InlinePromisedPure, CallingPredMarkers, ModuleInfo):
    %
    % Determine whether a predicate can be inlined.
    %
:- pred can_inline_proc(module_info::in, pred_id::in, proc_id::in,
    builtin_state::in, bool::in, pred_markers::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.det_analysis.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.purity.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.complexity.
:- import_module transform_hlds.dead_proc_elim.

:- import_module int.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % This structure holds option values, extracted from the globals.
    %
:- type inline_params
    --->    params(
                simple                  :: bool,
                single_use              :: bool,
                call_cost               :: int,
                compound_size_threshold :: int,
                simple_goal_threshold   :: int,
                var_threshold           :: int,
                highlevel_code          :: bool,
                any_tracing             :: bool
                                        % Is any procedure being traced
                                        % in the module?
            ).

inlining(!ModuleInfo) :-
    % Package up all the inlining options
    % - whether to inline simple conj's of builtins
    % - whether to inline predicates that are only called once
    % - the threshold for determining whether to inline more complicated goals
    % - the threshold for determining whether to inline the simple conj's
    % - the upper limit on the number of variables we want in procedures;
    %   if inlining a procedure would cause the number of variables to exceed
    %   this threshold then we don't inline it.
    % - whether we're in an MLDS grade

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, inline_simple, Simple),
    globals.lookup_bool_option(Globals, inline_single_use, SingleUse),
    globals.lookup_int_option(Globals, inline_call_cost, CallCost),
    globals.lookup_int_option(Globals, inline_compound_threshold,
        CompoundThreshold),
    globals.lookup_int_option(Globals, inline_simple_threshold,
        SimpleThreshold),
    globals.lookup_int_option(Globals, inline_vars_threshold, VarThreshold),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    globals.get_trace_level(Globals, TraceLevel),
    AnyTracing = bool.not(given_trace_level_is_none(TraceLevel)),
    Params = params(Simple, SingleUse, CallCost, CompoundThreshold,
        SimpleThreshold, VarThreshold, HighLevelCode, AnyTracing),

    % Get the usage counts for predicates (but only if needed, i.e. only if
    % --inline-single-use or --inline-compound-threshold has been specified).
    ( if
        ( SingleUse = yes
        ; CompoundThreshold > 0
        )
    then
        dead_proc_analyze(!.ModuleInfo, NeededMap)
    else
        map.init(NeededMap)
    ),

    % Build the call graph and extract the topological sort.
    % NOTE: the topological sort returns a list of SCCs. Clearly, we want to
    % process the SCCs bottom to top (which is the order that they are
    % returned), but it is not easy to guess the best way to flatten each SCC
    % to achieve the best result. The current implementation just uses the
    % ordering of the list returned by the topological sort. A more
    % sophisticated approach would be to break the cycle so that
    % the procedure(s) that are called by higher SCCs are processed last,
    % but we do not implement that yet.

    module_info_ensure_dependency_info(!ModuleInfo),
    module_info_dependency_info(!.ModuleInfo, DepInfo),
    PredProcs = dependency_info_get_condensed_bottom_up_sccs(DepInfo),
    set.init(InlinedProcs0),
    do_inlining(PredProcs, NeededMap, Params, InlinedProcs0, !ModuleInfo),

    % The dependency graph is now out of date and needs to be rebuilt.
    module_info_clobber_dependency_info(!ModuleInfo).

:- pred do_inlining(list(pred_proc_id)::in, needed_map::in,
    inline_params::in, set(pred_proc_id)::in,
    module_info::in, module_info::out) is det.

do_inlining([], _Needed, _Params, _Inlined, !Module).
do_inlining([PPId | PPIds], Needed, Params, !.Inlined, !Module) :-
    inline_in_proc(PPId, !.Inlined, Params, !Module),
    mark_predproc(PPId, Needed, Params, !.Module, !Inlined),
    do_inlining(PPIds, Needed, Params, !.Inlined, !Module).

    % This predicate effectively adds implicit `pragma inline' directives
    % for procedures that match its heuristic.
    %
:- pred mark_predproc(pred_proc_id::in, needed_map::in,
    inline_params::in, module_info::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

mark_predproc(PredProcId, NeededMap, Params, ModuleInfo, !InlinedProcs) :-
    ( if
        Simple = Params ^ simple,
        SingleUse = Params ^ single_use,
        CallCost = Params ^ call_cost,
        CompoundThreshold = Params ^ compound_size_threshold,
        SimpleThreshold = Params ^ simple_goal_threshold,
        PredProcId = proc(PredId, ProcId),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_proc_table(PredInfo, Procs),
        map.lookup(Procs, ProcId, ProcInfo),
        proc_info_get_goal(ProcInfo, CalledGoal),
        Entity = entity_proc(PredId, ProcId),

        % The heuristic represented by the following code could be improved.
        (
            Simple = yes,
            is_simple_goal(CalledGoal, SimpleThreshold)
        ;
            CompoundThreshold > 0,
            map.search(NeededMap, Entity, Needed),
            Needed = maybe_eliminable(NumUses),
            goal_size(CalledGoal, Size),
            % The size increase due to inlining at a call site is not Size,
            % but the difference between Size and the size of the call.
            % CallCost is the user-provided approximation of the size of the
            % call.
            (Size - CallCost) * NumUses =< CompoundThreshold
        ;
            SingleUse = yes,
            map.search(NeededMap, Entity, Needed),
            Needed = maybe_eliminable(NumUses),
            NumUses = 1
        ),
        % Don't inline recursive predicates (unless explicitly requested).
        not goal_calls(CalledGoal, PredProcId)
    then
        mark_proc_as_inlined(PredProcId, ModuleInfo, !InlinedProcs)
    else
        true
    ).

is_simple_clause_list(Clauses, SimpleThreshold) :-
    clause_list_size(Clauses, Size),
    (
        Size < SimpleThreshold
    ;
        Clauses = [Clause],
        Goal = Clause ^ clause_body,
        Size < SimpleThreshold * 3,

        % For flat goals, we are more likely to be able to optimize stuff away,
        % so we use a higher threshold.
        % XXX This should be a separate option, we shouldn't hardcode
        % the number `3' (which is just a guess).

        is_flat_simple_goal(Goal)
    ).

is_simple_goal(CalledGoal, SimpleThreshold) :-
    goal_size(CalledGoal, Size),
    (
        Size < SimpleThreshold
    ;
        % For flat goals, we are more likely to be able to optimize stuff away,
        % so we use a higher threshold.
        % XXX this should be a separate option, we shouldn't hardcode
        % the number `3' (which is just a guess).

        Size < SimpleThreshold * 3,
        is_flat_simple_goal(CalledGoal)
    ).

:- pred is_flat_simple_goal(hlds_goal::in) is semidet.

is_flat_simple_goal(hlds_goal(GoalExpr, _)) :-
    (
        GoalExpr = conj(plain_conj, Goals),
        is_flat_simple_goal_list(Goals)
    ;
        GoalExpr = negation(Goal),
        is_flat_simple_goal(Goal)
    ;
        GoalExpr = scope(Reason, Goal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % These scopes are flat and simple by construction.
            true
        else
            is_flat_simple_goal(Goal)
        )
    ;
        GoalExpr = plain_call(_, _, _, inline_builtin, _, _)
    ;
        GoalExpr = unify(_, _, _, _, _)
    ).

:- pred is_flat_simple_goal_list(hlds_goals::in) is semidet.

is_flat_simple_goal_list([]).
is_flat_simple_goal_list([Goal | Goals]) :-
    is_flat_simple_goal(Goal),
    is_flat_simple_goal_list(Goals).

:- pred mark_proc_as_inlined(pred_proc_id::in, module_info::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

mark_proc_as_inlined(proc(PredId, ProcId), ModuleInfo, !InlinedProcs) :-
    set.insert(proc(PredId, ProcId), !InlinedProcs),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ( if pred_info_requested_inlining(PredInfo) then
        true
    else
        trace [io(!IO)] (
            write_proc_progress_message("% Inlining ", PredId, ProcId,
                ModuleInfo, !IO)
        )
    ).

%-----------------------------------------------------------------------------%

    % inline_info contains the information that is changed as a result
    % of inlining. It is threaded through the inlining process, and when
    % finished, contains the updated information associated with the new
    % goal.
    %
    % It also stores some necessary information that is not updated.
    %
:- type inline_info
    --->    inline_info(
                i_var_threshold     :: int,
                                    % variable threshold for inlining

                i_highlevel_code    :: bool,
                                    % highlevel_code option

                i_exec_trace        :: bool,
                                    % is executing tracing enabled

                i_inlined_procs     :: set(pred_proc_id),

                i_module_info       :: module_info,

                i_univ_caller_tvars :: list(tvar),
                                    % Universally quantified type vars
                                    % occurring in the argument types
                                    % for this predicate (the caller,
                                    % not the callee). These are the
                                    % ones that must not be bound.

                i_pred_markers      :: pred_markers,
                                    % Markers for the current predicate.

                % All the following fields are updated as a result of inlining.

                i_prog_varset       :: prog_varset,
                i_vartypes          :: vartypes,
                i_tvarset           :: tvarset,

                i_rtti_varmaps      :: rtti_varmaps,
                                    % information about locations of
                                    % type_infos and typeclass_infos

                i_done_any_inlining :: bool,
                                    % Did we do any inlining in the proc?

                i_inlined_parallel  :: bool,
                                    % Did  we inline any procs for which
                                    % proc_info_get_has_parallel_conj returns
                                    % `has_parallel_conj'?

                i_need_requant      :: bool,
                                    % Does the goal need to be requantified?

                i_changed_detism    :: bool,
                                    % Did we change the determinism
                                    % of any subgoal?

                i_changed_purity    :: bool
                                    % Did we change the purity of
                                    % any subgoal.
            ).

:- pred inline_in_proc(pred_proc_id::in, set(pred_proc_id)::in,
    inline_params::in, module_info::in, module_info::out) is det.

inline_in_proc(PredProcId, InlinedProcs, Params, !ModuleInfo) :-
    VarThresh = Params ^ var_threshold,
    HighLevelCode = Params ^ highlevel_code,
    AnyTracing = Params ^ any_tracing,

    PredProcId = proc(PredId, ProcId),

    some [!PredInfo, !ProcInfo] (
        module_info_get_preds(!.ModuleInfo, PredTable0),
        map.lookup(PredTable0, PredId, !:PredInfo),
        pred_info_get_proc_table(!.PredInfo, ProcTable0),
        map.lookup(ProcTable0, ProcId, !:ProcInfo),

        pred_info_get_univ_quant_tvars(!.PredInfo, UnivQTVars),
        pred_info_get_typevarset(!.PredInfo, TypeVarSet0),
        pred_info_get_markers(!.PredInfo, Markers0),

        proc_info_get_goal(!.ProcInfo, Goal0),
        proc_info_get_varset(!.ProcInfo, VarSet0),
        proc_info_get_vartypes(!.ProcInfo, VarTypes0),
        proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),

        DidInlining0 = no,
        InlinedParallel0 = no,
        Requantify0 = no,
        DetChanged0 = no,
        PurityChanged0 = no,

        InlineInfo0 = inline_info(VarThresh, HighLevelCode, AnyTracing,
            InlinedProcs, !.ModuleInfo, UnivQTVars, Markers0,
            VarSet0, VarTypes0, TypeVarSet0, RttiVarMaps0,
            DidInlining0, InlinedParallel0, Requantify0, DetChanged0,
            PurityChanged0),

        inlining_in_goal(Goal0, Goal, InlineInfo0, InlineInfo),

        InlineInfo = inline_info(_, _, _, _, _, _, Markers, VarSet, VarTypes,
            TypeVarSet, RttiVarMaps, DidInlining, InlinedParallel, Requantify,
            DetChanged, PurityChanged),

        pred_info_set_markers(Markers, !PredInfo),
        pred_info_set_typevarset(TypeVarSet, !PredInfo),

        proc_info_set_varset(VarSet, !ProcInfo),
        proc_info_set_vartypes(VarTypes, !ProcInfo),
        proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),
        proc_info_set_goal(Goal, !ProcInfo),

        (
            InlinedParallel = yes,
            proc_info_set_has_parallel_conj(has_parallel_conj, !ProcInfo)
        ;
            InlinedParallel = no
        ),

        (
            Requantify = yes,
            requantify_proc_general(ordinary_nonlocals_no_lambda, !ProcInfo)
        ;
            Requantify = no
        ),

        (
            DidInlining = yes,
            recompute_instmap_delta_proc(recompute_atomic_instmap_deltas,
                !ProcInfo, !ModuleInfo)
        ;
            DidInlining = no
        ),

        map.det_update(ProcId, !.ProcInfo, ProcTable0, ProcTable),
        pred_info_set_proc_table(ProcTable, !PredInfo),

        (
            PurityChanged = yes,
            repuritycheck_proc(!.ModuleInfo, PredProcId, !PredInfo)
        ;
            PurityChanged = no
        ),

        map.det_update(PredId, !.PredInfo, PredTable0, PredTable),
        module_info_set_preds(PredTable, !ModuleInfo),

        % If the determinism of some sub-goals has changed, then we re-run
        % determinism analysis, because propagating the determinism information
        % through the procedure may lead to more efficient code.
        (
            DetChanged = yes,
            det_infer_proc_ignore_msgs(PredId, ProcId, !ModuleInfo)
        ;
            DetChanged = no
        )
    ).

%-----------------------------------------------------------------------------%

:- pred inlining_in_goal(hlds_goal::in, hlds_goal::out,
    inline_info::in, inline_info::out) is det.

inlining_in_goal(Goal0, Goal, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = plain_call(PredId, ProcId, ArgVars, Builtin, Context, Sym),
        inlining_in_call(PredId, ProcId, ArgVars, Builtin,
            Context, Sym, GoalExpr, GoalInfo0, GoalInfo, !Info)
    ;
        ( GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ),
        GoalExpr = GoalExpr0,
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            inlining_in_conj(Goals0, Goals, !Info)
        ;
            ConjType = parallel_conj,
            inlining_in_par_conj(Goals0, Goals, !Info)
        ),
        GoalExpr = conj(ConjType, Goals),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = disj(Goals0),
        inlining_in_goals(Goals0, Goals, !Info),
        GoalExpr = disj(Goals),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = switch(Var, Det, Cases0),
        inlining_in_cases(Cases0, Cases, !Info),
        GoalExpr = switch(Var, Det, Cases),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        inlining_in_goal(Cond0, Cond, !Info),
        inlining_in_goal(Then0, Then, !Info),
        inlining_in_goal(Else0, Else, !Info),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = negation(SubGoal0),
        inlining_in_goal(SubGoal0, SubGoal, !Info),
        GoalExpr = negation(SubGoal),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % The scope has no calls to inline.
            GoalExpr = GoalExpr0,
            GoalInfo = GoalInfo0
        else
            inlining_in_goal(SubGoal0, SubGoal, !Info),
            GoalExpr = scope(Reason, SubGoal),
            GoalInfo = GoalInfo0
        )
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($module, $pred, "shorthand")
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred inlining_in_call(pred_id::in, proc_id::in,
    list(prog_var)::in, builtin_state::in, maybe(call_unify_context)::in,
    sym_name::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    inline_info::in, inline_info::out) is det.

inlining_in_call(PredId, ProcId, ArgVars, Builtin,
        Context, Sym, GoalExpr, GoalInfo0, GoalInfo, !Info) :-
    !.Info = inline_info(VarThresh, HighLevelCode, AnyTracing,
        InlinedProcs, ModuleInfo, ExternalTypeParams, Markers,
        VarSet0, VarTypes0, TypeVarSet0, RttiVarMaps0, _DidInlining0,
        InlinedParallel0, Requantify0, DetChanged0, PurityChanged0),

    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
    % Should we inline this call?
    ( if
        should_inline_proc(PredId, ProcId, Builtin, HighLevelCode,
            AnyTracing, InlinedProcs, Markers, ModuleInfo, UserReq),
        (
            UserReq = yes
        ;
            UserReq = no,
            % Okay, but will we exceed the number-of-variables threshold?
            varset.vars(VarSet0, ListOfVars),
            list.length(ListOfVars, ThisMany),

            % We need to find out how many variables the Callee has.
            proc_info_get_varset(ProcInfo, CalleeVarSet),
            varset.vars(CalleeVarSet, CalleeListOfVars),
            list.length(CalleeListOfVars, CalleeThisMany),
            TotalVars = ThisMany + CalleeThisMany,
            TotalVars =< VarThresh
        ),
        % XXX Work around bug #142.
        not may_encounter_bug_142(ProcInfo, ArgVars)
    then
        do_inline_call(ExternalTypeParams, ArgVars, PredInfo, ProcInfo,
            VarSet0, VarSet, VarTypes0, VarTypes, TypeVarSet0, TypeVarSet,
            RttiVarMaps0, RttiVarMaps, hlds_goal(GoalExpr, GoalInfo)),

        % If some of the output variables are not used in the calling
        % procedure, requantify the procedure.
        NonLocals = goal_info_get_nonlocals(GoalInfo0),
        ( if set_of_var.list_to_set(ArgVars) = NonLocals then
            Requantify = Requantify0
        else
            Requantify = yes
        ),

        ( if
            goal_info_get_purity(GoalInfo0) = goal_info_get_purity(GoalInfo)
        then
            PurityChanged = PurityChanged0
        else
            PurityChanged = yes
        ),

        % If the inferred determinism of the called goal differs from the
        % declared determinism, flag that we should re-run determinism analysis
        % on this proc.
        Determinism0 = goal_info_get_determinism(GoalInfo0),
        Determinism = goal_info_get_determinism(GoalInfo),
        ( if Determinism0 = Determinism then
            DetChanged = DetChanged0
        else
            DetChanged = yes
        ),

        proc_info_get_has_parallel_conj(ProcInfo, HasParallelConj),
        (
            HasParallelConj = has_parallel_conj,
            InlinedParallel = yes
        ;
            HasParallelConj = has_no_parallel_conj,
            InlinedParallel = InlinedParallel0
        ),

        DidInlining = yes,

        !:Info = inline_info(VarThresh, HighLevelCode, AnyTracing,
            InlinedProcs, ModuleInfo, ExternalTypeParams, Markers,
            VarSet, VarTypes, TypeVarSet, RttiVarMaps, DidInlining,
            InlinedParallel, Requantify, DetChanged, PurityChanged)
    else
        GoalExpr = plain_call(PredId, ProcId, ArgVars, Builtin, Context, Sym),
        GoalInfo = GoalInfo0
    ).

:- pred may_encounter_bug_142(proc_info::in, list(prog_var)::in) is semidet.

may_encounter_bug_142(CalleeProcInfo, ArgVars) :-
    proc_info_get_rtti_varmaps(CalleeProcInfo, RttiVarMaps),
    proc_info_get_headvars(CalleeProcInfo, HeadVars),
    multi_map.from_corresponding_lists(ArgVars, HeadVars, MultiMap),
    some [ArgVar] (
        list.member(ArgVar, ArgVars),
        multi_map.lookup(MultiMap, ArgVar, HeadVarsForArgVar),
        HeadVarsForArgVar = [_ | _],
        tci_vars_different_constraints(RttiVarMaps, HeadVarsForArgVar)
    ).

:- pred tci_vars_different_constraints(rtti_varmaps::in, list(prog_var)::in)
    is semidet.

tci_vars_different_constraints(RttiVarMaps, [VarA, VarB | Vars]) :-
    (
        rtti_varmaps_var_info(RttiVarMaps, VarA, VarInfoA),
        rtti_varmaps_var_info(RttiVarMaps, VarB, VarInfoB),
        VarInfoA = typeclass_info_var(ConstraintA),
        VarInfoB = typeclass_info_var(ConstraintB),
        ConstraintA \= ConstraintB
    ;
        tci_vars_different_constraints(RttiVarMaps, [VarB | Vars])
    ).

%-----------------------------------------------------------------------------%

do_inline_call(ExternalTypeParams, ArgVars, PredInfo, ProcInfo,
        VarSet0, VarSet, VarTypes0, VarTypes, TypeVarSet0, TypeVarSet,
        RttiVarMaps0, RttiVarMaps, Goal) :-

    proc_info_get_goal(ProcInfo, CalledGoal),

    % Look up the rest of the info for the called procedure.

    pred_info_get_typevarset(PredInfo, CalleeTypeVarSet),
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_vartypes(ProcInfo, CalleeVarTypes0),
    proc_info_get_varset(ProcInfo, CalleeVarSet),
    proc_info_get_rtti_varmaps(ProcInfo, CalleeRttiVarMaps0),

    % Substitute the appropriate types into the type mapping of the called
    % procedure. For example, if we call `:- pred foo(T)' with an argument
    % of type `int', then we need to replace all occurrences of type `T'
    % with type `int' when we inline it. Conversely, in the case of
    % existentially typed preds, we may need to bind type variables in the
    % caller. For example, if we call `:- pred some [T] foo(T)', and the
    % definition of `foo' binds `T' to `int', then we need to replace all
    % occurrences of type `T' with type `int' in the caller.

    % First, rename apart the type variables in the callee. (We can almost
    % throw away the new typevarset, since we are about to substitute away
    % any new type variables, but any unbound type variables in the callee
    % will not be substituted away)

    tvarset_merge_renaming(TypeVarSet0, CalleeTypeVarSet, TypeVarSet,
        TypeRenaming),
    apply_variable_renaming_to_vartypes(TypeRenaming,
        CalleeVarTypes0, CalleeVarTypes1),

    % Next, compute the type substitution and then apply it.

    % Note: there's no need to update the type_info locations maps,
    % either for the caller or callee, since for any type vars in the
    % callee which get bound to type vars in the caller, the type_info
    % location will be given by the entry in the caller's
    % type_info locations map (and vice versa).  It doesn't matter if the
    % final type_info locations map contains some entries
    % for type variables which have been substituted away,
    % because those entries simply won't be used.

    lookup_var_types(CalleeVarTypes1, HeadVars, HeadTypes),
    lookup_var_types(VarTypes0, ArgVars, ArgTypes),

    pred_info_get_exist_quant_tvars(PredInfo, CalleeExistQVars),
    get_type_substitution(HeadTypes, ArgTypes, ExternalTypeParams,
        CalleeExistQVars, TypeSubn),

    % Handle the common case of non-existentially typed preds specially,
    % since we can do things more efficiently in that case
    (
        CalleeExistQVars = [],
        % Update types in callee only.
        apply_rec_subst_to_vartypes(TypeSubn, CalleeVarTypes1, CalleeVarTypes),
        VarTypes1 = VarTypes0
    ;
        CalleeExistQVars = [_ | _],
        % Update types in callee.
        apply_rec_subst_to_vartypes(TypeSubn, CalleeVarTypes1, CalleeVarTypes),
        % Update types in caller.
        apply_rec_subst_to_vartypes(TypeSubn, VarTypes0, VarTypes1)
    ),

    % Now rename apart the variables in the called goal.
    rename_goal(HeadVars, ArgVars, VarSet0, CalleeVarSet, VarSet, VarTypes1,
        CalleeVarTypes, VarTypes, Subn, CalledGoal, Goal),

    apply_substitutions_to_rtti_varmaps(TypeRenaming, TypeSubn, Subn,
        CalleeRttiVarMaps0, CalleeRttiVarMaps1),

    % Prefer the type_info_locn from the caller.
    % The type_infos or typeclass_infos passed to the callee may
    % have been produced by extracting type_infos or typeclass_infos
    % from typeclass_infos in the caller, so they won't necessarily
    % be the same.
    rtti_varmaps_overlay(CalleeRttiVarMaps1, RttiVarMaps0, RttiVarMaps).

get_type_substitution(HeadTypes, ArgTypes,
        ExternalTypeParams, CalleeExistQVars, TypeSubn) :-
    (
        CalleeExistQVars = [],
        ( if type_list_subsumes(HeadTypes, ArgTypes, TypeSubn0) then
            TypeSubn = TypeSubn0
        else
            % The head types should always be unifiable with the actual
            % argument types, otherwise it is a type error that should have
            % been detected by typechecking. But polymorphism.m introduces
            % type-incorrect code -- e.g. compare(Res, EnumA, EnumB) gets
            % converted into builtin_compare_int(Res, EnumA, EnumB), which
            % is a type error since it assumes that an enumeration is an int.
            % In those cases, we don't need to worry about the type
            % substitution. (Perhaps it would be better if polymorphism
            % introduced calls to unsafe_type_cast/2 for such cases.)
            map.init(TypeSubn)
        )
    ;
        CalleeExistQVars = [_ | _],
        % For calls to existentially type preds, we may need to bind
        % type variables in the caller, not just those in the callee.
        ( if
            map.init(TypeSubn0),
            type_unify_list(HeadTypes, ArgTypes, ExternalTypeParams,
                TypeSubn0, TypeSubn1)
        then
            TypeSubn = TypeSubn1
        else
            unexpected($module, $pred, "type unification failed")
        )
    ).

rename_goal(HeadVars, ArgVars, VarSet0, CalleeVarSet, VarSet, VarTypes1,
        CalleeVarTypes, VarTypes, Renaming, CalledGoal, Goal) :-
    map.from_corresponding_lists(HeadVars, ArgVars, Renaming0),
    varset.vars(CalleeVarSet, CalleeListOfVars),
    clone_variables(CalleeListOfVars, CalleeVarSet, CalleeVarTypes,
        VarSet0, VarSet, VarTypes1, VarTypes, Renaming0, Renaming),
    must_rename_vars_in_goal(Renaming, CalledGoal, Goal).

%-----------------------------------------------------------------------------%

    % inlining_in_goals is used for both disjunctions and
    % parallel conjunctions.
    %
:- pred inlining_in_goals(list(hlds_goal)::in, list(hlds_goal)::out,
    inline_info::in, inline_info::out) is det.

inlining_in_goals([], [], !Info).
inlining_in_goals([Goal0 | Goals0], [Goal | Goals], !Info) :-
    inlining_in_goal(Goal0, Goal, !Info),
    inlining_in_goals(Goals0, Goals, !Info).

%-----------------------------------------------------------------------------%

:- pred inlining_in_cases(list(case)::in, list(case)::out,
    inline_info::in, inline_info::out) is det.

inlining_in_cases([], [], !Info).
inlining_in_cases([Case0 | Cases0], [Case | Cases], !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    inlining_in_goal(Goal0, Goal, !Info),
    Case = case(MainConsId, OtherConsIds, Goal),
    inlining_in_cases(Cases0, Cases, !Info).

%-----------------------------------------------------------------------------%

:- pred inlining_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    inline_info::in, inline_info::out) is det.

inlining_in_conj([], [], !Info).
inlining_in_conj([Goal0 | Goals0], Goals, !Info) :-
    % Since a single goal may become a conjunction,
    % we flatten the conjunction as we go.
    inlining_in_goal(Goal0, Goal1, !Info),
    goal_to_conj_list(Goal1, Goal1List),
    inlining_in_conj(Goals0, Goals1, !Info),
    list.append(Goal1List, Goals1, Goals).

:- pred inlining_in_par_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    inline_info::in, inline_info::out) is det.

inlining_in_par_conj([], [], !Info).
inlining_in_par_conj([Goal0 | Goals0], Goals, !Info) :-
    % Since a single goal may become a parallel conjunction,
    % we flatten the conjunction as we go.
    inlining_in_goal(Goal0, Goal1, !Info),
    goal_to_par_conj_list(Goal1, Goal1List),
    inlining_in_par_conj(Goals0, Goals1, !Info),
    list.append(Goal1List, Goals1, Goals).

%-----------------------------------------------------------------------------%

    % Check to see if we should inline a call.
    %
    % Fails if the called predicate cannot be inlined, e.g. because it is
    % a builtin, we don't have code for it, it uses nondet pragma c_code, etc.
    %
    % It succeeds if the called procedure is inlinable, and in addition
    % either there was a `pragma inline' for this procedure, or the procedure
    % was marked by mark_predproc as having met its heuristic.
    %
:- pred should_inline_proc(pred_id::in, proc_id::in,
    builtin_state::in, bool::in, bool::in, set(pred_proc_id)::in,
    pred_markers::in, module_info::in, bool::out) is semidet.

should_inline_proc(PredId, ProcId, BuiltinState, HighLevelCode,
        _Tracing, InlinedProcs, CallingPredMarkers, ModuleInfo, UserReq) :-
    InlinePromisedPure = yes,
    can_inline_proc_2(ModuleInfo, PredId, ProcId, BuiltinState,
        HighLevelCode, InlinePromisedPure, CallingPredMarkers),
    % OK, we could inline it - but should we?  Apply our heuristic.
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    ( if
        check_marker(Markers, marker_user_marked_inline)
    then
        UserReq = yes
    else if
        ( check_marker(Markers, marker_heuristic_inline)
        ; set.member(proc(PredId, ProcId), InlinedProcs)
        )
    then
        UserReq = no
    else
        fail
    ).

can_inline_proc(ModuleInfo, PredId, ProcId, BuiltinState, InlinePromisedPure,
        CallingPredMarkers) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    can_inline_proc_2(ModuleInfo, PredId, ProcId, BuiltinState, HighLevelCode,
        InlinePromisedPure, CallingPredMarkers).

:- pred can_inline_proc_2(module_info::in, pred_id::in, proc_id::in,
    builtin_state::in, bool::in, bool::in, pred_markers::in) is semidet.

can_inline_proc_2(ModuleInfo, PredId, ProcId, BuiltinState, HighLevelCode,
        InlinePromisedPure, _CallingPredMarkers) :-
    % Don't inline builtins, the code generator will handle them.
    BuiltinState = not_builtin,
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),

    % Don't try to inline imported predicates, since we don't
    % have the code for them.
    not pred_info_is_imported(PredInfo),

    % This next line catches the case of locally defined unification predicates
    % for imported types.
    not (
        pred_info_is_pseudo_imported(PredInfo),
        hlds_pred.in_in_unification_proc_id(ProcId)
    ),

    % Only try to inline procedures which are evaluated using normal
    % evaluation. Currently we can't inline procs evaluated using any of the
    % other methods because the code generator for the methods can only handle
    % whole procedures not code fragments.
    proc_info_get_eval_method(ProcInfo, eval_normal),

    % Don't inline anything we have been specifically requested not to inline.
    not pred_info_requested_no_inlining(PredInfo),

    % Don't inline any procedure whose complexity we are trying to determine,
    % since the complexity transformation can't transform *part* of a
    % procedure.
    module_info_get_maybe_complexity_proc_map(ModuleInfo,
        MaybeComplexityProcMap),
    (
        MaybeComplexityProcMap = no
    ;
        MaybeComplexityProcMap = yes(_ - ComplexityProcMap),
        IsInComplexityMap = is_in_complexity_proc_map(
            ComplexityProcMap, ModuleInfo, PredId, ProcId),
        IsInComplexityMap = no
    ),

    proc_info_get_goal(ProcInfo, CalledGoal),
    CalledGoal = hlds_goal(CalledGoalExpr, _),
    ( if
        CalledGoalExpr = call_foreign_proc(ForeignAttributes, _, _, _, _, _, _)
    then
        % Only inline a foreign_proc if it is appropriate for the target
        % language.
        module_info_get_globals(ModuleInfo, Globals),
        globals.get_target(Globals, Target),
        (
            ForeignLanguage = get_foreign_language(ForeignAttributes)
        =>
            ok_to_inline_language(ForeignLanguage, Target)
        ),

        % Don't inline a foreign_proc if it is has been marked with the
        % attribute that requests the code not be duplicated.
        (
            MaybeMayDuplicate = get_may_duplicate(ForeignAttributes)
        =>
            (
                MaybeMayDuplicate = no
            ;
                MaybeMayDuplicate = yes(proc_may_duplicate)
            )
        ),

        % For the LLDS back-end, under no circumstances inline model_non
        % foreign_procs. The resulting code would not work properly.
        not (
            HighLevelCode = no,
            proc_info_interface_determinism(ProcInfo, Detism),
            ( Detism = detism_non
            ; Detism = detism_multi
            )
        )
    else
        true
    ),

    (
        InlinePromisedPure = yes
    ;
        % For some optimizations (such as deforestation) we don't want to
        % inline predicates which are promised pure because the extra impurity
        % propagated through the goal will defeat any attempts at optimization.
        InlinePromisedPure = no,
        pred_info_get_promised_purity(PredInfo, purity_impure)
    ).

    % Succeed iff it is appropriate to inline `pragma foreign_proc'
    % in the specified language for the given compilation_target.
    % Generally that will only be the case if the target directly
    % supports inline code in that language.
    %
:- pred ok_to_inline_language(foreign_language::in, compilation_target::in)
    is semidet.

ok_to_inline_language(lang_c, target_c).
ok_to_inline_language(lang_erlang, target_erlang).
ok_to_inline_language(lang_java, target_java).
ok_to_inline_language(lang_csharp, target_csharp).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.inlining.
%-----------------------------------------------------------------------------%
