%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
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
%---------------------------------------------------------------------------%

:- module transform_hlds.inlining.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

:- pred inline_in_module(module_info::in, module_info::out) is det.

    % This heuristic is used for both local and intermodule inlining.
    % XXX No, it isn't; it is not used in this module.
    % The reason why I (zs) haven't moved it to intermod.m is that
    % making this module and intermod.m use separate tests for what is
    % inlineable is *not* the proper fix.
    %
:- pred is_simple_clause_list(list(clause)::in, int::in) is semidet.

:- pred is_simple_goal(hlds_goal::in, int::in) is semidet.

    % do_inline_call(ModuleInfo, UnivQVars, Args,
    %   CalledPredInfo, CalledProcInfo, !TVarSet, !VarTable, !RttiVarMaps,
    %   Goal):
    %
    % Given the universally quantified type variables in the caller's type,
    % the arguments to the call, the pred_info and proc_info for the called
    % goal and various information about the variables and types in the
    % procedure currently being analysed, rename the goal for the called
    % procedure so that it can be inlined.
    %
:- pred do_inline_call(module_info::in, list(tvar)::in, list(prog_var)::in,
    pred_info::in, proc_info::in, tvarset::in, tvarset::out,
    var_table::in, var_table::out, rtti_varmaps::in, rtti_varmaps::out,
    hlds_goal::out) is det.

    % rename_goal(CalledProcHeadVars, CallArgs,
    %   CallerVarTypes0, CalleeVarTypes, CallerVarTypes,
    %   VarRenaming, CalledGoal, RenamedGoal).
    %
:- pred rename_goal(list(prog_var)::in, list(prog_var)::in,
    var_table::in, var_table::in, var_table::out,
    map(prog_var, prog_var)::out, hlds_goal::in, hlds_goal::out) is det.

:- type may_inline_purity_promised_pred
    --->    may_not_inline_purity_promised_pred
    ;       may_inline_purity_promised_pred.

    % can_inline_proc(ModuleInfo, PredId, ProcId, BuiltinState,
    %   InlinePromisedPure):
    %
    % Determine whether a call to the given predicate can be inlined.
    %
:- pred can_inline_proc(module_info::in, pred_id::in, proc_id::in,
    builtin_state::in, may_inline_purity_promised_pred::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.det_analysis.
:- import_module check_hlds.purity.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.mark_tail_calls.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.complexity.
:- import_module transform_hlds.dead_proc_elim.

:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

    % This structure holds the paramaters that direct the details of the
    % inlining process. Most (but not all) of these fields hold the
    % values of compiler invocation options.
    %
:- type inline_params
    --->    inline_params(
                ip_simple                       :: maybe_inline_simple,
                ip_single_use                   :: maybe_inline_single_use,
                ip_linear_tail_rec              ::
                                            maybe_inline_linear_tail_rec_sccs,

                ip_highlevel_code               :: bool,

                ip_linear_tail_rec_max_extra    :: int,
                ip_call_cost                    :: int,
                ip_compound_size_threshold      :: int,
                ip_simple_goal_threshold        :: int,
                ip_var_threshold                :: int,

                ip_needed_map                   :: needed_map
            ).

%---------------------------------------------------------------------------%

    % inline_info contains
    %
    % - the information we need as we process a goal for inlining, and
    % - the information that is changed as a result of inlining.
    %
    % It is threaded through all the code that does inlining in procedure
    % bodies, updated when necessary. When the process is done, each of the
    % updateable fields should be acted upon, either by putting the updated
    % value of the field back where it came from, or by performing the action
    % that a flag calls for if it is set.
    %
:- type inline_info
    --->    inline_info(
            % The static fields.
                i_module_info           :: module_info,

                % Variable threshold for inlining.
                i_var_threshold         :: int,

                % Highlevel_code option.
                i_highlevel_code        :: bool,

                % Universally quantified type vars occurring in the argument
                % types for this predicate (the caller, not the callee).
                % These are the ones that must not be bound.
                i_univ_caller_tvars     :: list(tvar),

                % Markers for the current predicate.
%               i_pred_markers          :: pred_markers,

                % The set of procedures in the current SCC, tail calls
                % to which should be inlined.
                i_should_inline_tail_calls :: set(pred_proc_id),

            % The fields we can update between different inlining passes
            % on a procedure body.

                i_should_inline_procs   :: set(pred_proc_id),

            % The fields we can update while doing inlining in a goal.

                i_tvarset               :: tvarset,
                i_var_table             :: var_table,

                % Information about locations of type_infos and
                % typeclass_infos.
                i_rtti_varmaps          :: rtti_varmaps,

                % Did we do any inlining in the proc?
                i_done_any_inlining     :: have_we_inlined,

                % Did we inline any procs for which
                % proc_info_get_has_parallel_conj returns `has_parallel_conj'?
                i_inlined_parallel      :: have_we_inlined_parallel_conj,

                % Did we change the determinism of any subgoal?
                i_changed_detism        :: have_we_changed_detism,

                % Did we change the purity of any subgoal?
                i_changed_purity        :: have_we_changed_purity
            ).

:- type have_we_inlined
    --->    we_have_not_inlined
    ;       we_have_inlined.

:- type have_we_inlined_parallel_conj
    --->    we_have_not_inlined_parallel_conj
    ;       we_have_inlined_parallel_conj.

:- type have_we_changed_detism
    --->    have_not_changed_detism
    ;       may_have_changed_detism.

:- type have_we_changed_purity
    --->    have_not_changed_purity
    ;       have_changed_purity.

%---------------------------------------------------------------------------%

inline_in_module(!ModuleInfo) :-
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
    globals.get_opt_tuple(Globals, OptTuple),
    Simple = OptTuple ^ ot_inline_simple,
    SingleUse = OptTuple ^ ot_inline_single_use,
    LinearTailRec = OptTuple ^ ot_inline_linear_tail_rec_sccs,
    LinearTailRecMaxExtra =
        OptTuple ^ ot_inline_linear_tail_rec_sccs_max_extra,
    CallCost = OptTuple ^ ot_inline_call_cost,
    CompoundThreshold = OptTuple ^ ot_inline_compound_threshold,
    SimpleThreshold = OptTuple ^ ot_inline_simple_threshold,
    VarThreshold = OptTuple ^ ot_inline_vars_threshold,
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),

    % Get the usage counts for predicates (but only if needed, i.e. only if
    % --inline-single-use or --inline-compound-threshold has been specified).
    ( if
        ( SingleUse = inline_single_use
        ; CompoundThreshold > 0
        )
    then
        dead_proc_analyze(!.ModuleInfo, NeededMap)
    else
        map.init(NeededMap)
    ),
    Params = inline_params(Simple, SingleUse, LinearTailRec, HighLevelCode,
        LinearTailRecMaxExtra, CallCost,
        CompoundThreshold, SimpleThreshold, VarThreshold, NeededMap),

    % Build the call graph and extract the list of SCCs. We process
    % SCCs bottom up, so that if a caller wants to inline a callee
    % in a lower SCC, it gets the *already optimized* version of the callee.
    % If LinearTailRec = no, we don't try to do anything special about
    % calls where the callee is in the *same* SCC as the caller.

    (
        LinearTailRec = do_not_inline_linear_tail_rec_sccs,
        module_info_ensure_dependency_info(!ModuleInfo, DepInfo)
    ;
        LinearTailRec = inline_linear_tail_rec_sccs,
        % For this, we need *accurate* information about SCCs.
        % I (zs) am not 100% certain that every pass before this one
        % that invalidates any existing dependency info also clobbers
        % that dependency info.
        module_info_rebuild_dependency_info(!ModuleInfo, DepInfo),
        mark_self_and_mutual_tail_rec_calls_in_module(DepInfo, !ModuleInfo)
    ),

    get_bottom_up_sccs_with_entry_points(!.ModuleInfo, DepInfo,
        BottomUpSCCsEntryPoints),
    set.init(ShouldInlineProcs0),
    inline_in_sccs(Params, BottomUpSCCsEntryPoints, ShouldInlineProcs0,
        !ModuleInfo),

    % The dependency graph is now out of date and needs to be rebuilt.
    module_info_clobber_dependency_info(!ModuleInfo).

:- pred inline_in_sccs(inline_params::in, list(scc_with_entry_points)::in,
    set(pred_proc_id)::in,
    module_info::in, module_info::out) is det.

inline_in_sccs(_Params, [], _ShouldInlineProcs, !ModuleInfo).
inline_in_sccs(Params, [SCCEntryPoints | SCCsEntryPoints],
        !.ShouldInlineProcs, !ModuleInfo) :-
    inline_in_scc(Params, SCCEntryPoints, !ShouldInlineProcs, !ModuleInfo),
    inline_in_sccs(Params, SCCsEntryPoints, !.ShouldInlineProcs, !ModuleInfo).

:- pred inline_in_scc(inline_params::in, scc_with_entry_points::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out,
    module_info::in, module_info::out) is det.

inline_in_scc(Params, SCCEntryPoints, !ShouldInlineProcs, !ModuleInfo) :-
    SCCEntryPoints =
        scc_with_entry_points(SCC, _CalledFromHigherSCCs, _Exported),
    SCCProcs = set.to_sorted_list(SCC),
    (
        SCCProcs = [],
        unexpected($pred, "empty SCC")
    ;
        SCCProcs = [SCCProc],
        inline_in_proc_if_allowed(Params, !.ShouldInlineProcs,
            set.init, SCCProc, !ModuleInfo),
        maybe_mark_proc_to_be_inlined(Params, !.ModuleInfo, SCCProc,
            !ShouldInlineProcs)
    ;
        SCCProcs = [_, _ | _],
        LinearTailRec = Params ^ ip_linear_tail_rec,
        (
            LinearTailRec = do_not_inline_linear_tail_rec_sccs,
            inline_in_simple_non_singleton_scc(Params, SCCProcs,
                !ShouldInlineProcs, !ModuleInfo)
        ;
            LinearTailRec = inline_linear_tail_rec_sccs,
            inline_in_maybe_linear_tail_rec_scc(Params,
                SCCEntryPoints, SCCProcs, !ShouldInlineProcs, !ModuleInfo)
        )
    ).

:- pred inline_in_maybe_linear_tail_rec_scc(inline_params::in,
    scc_with_entry_points::in, list(pred_proc_id)::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out,
    module_info::in, module_info::out) is det.

inline_in_maybe_linear_tail_rec_scc(Params, SCCEntryPoints, SCCProcs,
        !ShouldInlineProcs, !ModuleInfo) :-
    SCCEntryPoints = scc_with_entry_points(SCC,
        CalledFromHigherSCCs, Exported),
    TSCCDepInfo =
        build_proc_dependency_graph(!.ModuleInfo, SCC, only_tail_calls),
    get_bottom_up_sccs_with_entry_points(!.ModuleInfo, TSCCDepInfo,
        TSCCsEntries),
    % Read the comments on the following code assuming that
    % LinearTailRecMaxExtra is zero, until you get to the part that explains
    % what happens if it is not.
    LinearTailRecMaxExtra = Params ^ ip_linear_tail_rec_max_extra,
    ( if
        % If there is only one TSSC, which means that every procedure
        % in the SCC is reachable from every other procedure via
        % *tail* recursive calls, ...
        TSCCsEntries = [_],
        % ... and the number of tail recursive calls in the SCC is equal
        % to the number of procedures in the SCC, ...
        TSCCArcs = dependency_info_get_arcs(TSCCDepInfo),
        list.length(TSCCArcs, NumTSCCArcs),
        list.length(SCCProcs, NumSCCProcs),
        NumTSCCArcs =< NumSCCProcs + LinearTailRecMaxExtra
    then
        % ... then every procedure in the SCC is called from exactly
        % one call site in some other procedure in the SCC.
        %
        % The optimization we apply to such SCCs is the following.
        %
        % Suppose the SCC consists of procedures A, B, C and D,
        % with the tail calls among them being A->B->C->D->A,
        % with A and B being entry points.
        %
        % For each entry point procedure of the SCC, such as A:
        %
        %   Inline the single tail recursive call site in its body.
        %   In this case, this replaces the tail recursive call to B
        %   with the body of B, which includes a single tail recursive
        %   call to C.
        %
        %   We keep doing the same thing until the only tail recursive
        %   call remaining is to the procedure we started with,
        %   in this case A.
        %
        % This makes the code of each entry point self-tail-recursive.
        %
        % This algorithm leaves recursive calls in the SCC that are not
        % tail calls unchanged. Each such call will consume a stack frame.
        %
        % This transformation may leave the procedures in the SCC
        % that are *not* entry points in the SCC unused, after all
        % calls to them have been inlined. They will be deleted by
        % dead procedure elimination in the usual course of events.
        %
        % Any increase in the value of LinearTailRecMaxExtra allows the
        % SCC to contain one more mutually-tail-recursive call site that
        % we will inline. For example, having LinearTailRecMaxExtra = 1
        % would allow the example SCC above to contain either two A->B calls,
        % or two B->C calls, or two C->D calls, or two D->A calls.
        %
        % Suppose we are doing inlining inside A. With two D->A calls,
        % there will be no code size increase, since recurive calls to A
        % won't be inlined. With two C->D calls, there will be two copies
        % of the body of D. With two B->C calls, there will be two copies
        % of the bodies of both C and D, since the call to D will be inlined
        % in both copies of C. With two A->B calls, there will be two copies
        % of B, C and D. If the body of A itself is small, this may mean
        % that the total size of the code after inlining may be close to
        % double what it would be without the extra tail call permitted
        % by LinearTailRecMaxExtra = 1.
        %
        % In general, the size of the resulting code may grow almost as
        % fast as 2^LinearTailRecMaxExtra.
        %
        % (Since a TSCC with N procedures must have at least N tail recursive
        % calls, negative values of LinearTailRecMaxExtra *will* cause the
        % test above to fail, so execution will never get here.)
        %
        set.union(CalledFromHigherSCCs, Exported, EntryPoints),
        list.foldl(
            inline_in_linear_tail_rec_proc(Params, !.ShouldInlineProcs,
                SCC, EntryPoints),
            SCCProcs, !ModuleInfo)
    else
        inline_in_simple_non_singleton_scc(Params, SCCProcs,
            !ShouldInlineProcs, !ModuleInfo)
    ).

:- pred inline_in_linear_tail_rec_proc(inline_params::in,
    set(pred_proc_id)::in, set(pred_proc_id)::in, set(pred_proc_id)::in,
    pred_proc_id::in, module_info::in, module_info::out) is det.

inline_in_linear_tail_rec_proc(Params, ShouldInlineProcs, SCC, EntryPoints,
        PredProcId, !ModuleInfo) :-
    ( if set.member(PredProcId, EntryPoints) then
        % We should inline every tail recursive call in the body of
        % procedure PredProcId, except the one that calls PredProcId itself.
        set.delete(PredProcId, SCC, ShouldInlineTailProcs)
    else
        set.init(ShouldInlineTailProcs)
    ),
    inline_in_proc_if_allowed(Params, ShouldInlineProcs, ShouldInlineTailProcs,
        PredProcId, !ModuleInfo).

:- pred inline_in_simple_non_singleton_scc(inline_params::in,
    list(pred_proc_id)::in, set(pred_proc_id)::in, set(pred_proc_id)::out,
    module_info::in, module_info::out) is det.

inline_in_simple_non_singleton_scc(Params, SCCProcs, !ShouldInlineProcs,
        !ModuleInfo) :-
    % We decide whether to inline *any* of the SCC's procedures *before*
    % we process any of them, so we can apply the results of the decision
    % to *all* of them.
    %
    % This mostly means inlining two kinds of SCC members in other SCC members:
    %
    % - procedures whose definition is trivial, such as a call to a higher
    %   order predicate or function such as list.map or list.foldl, specifying
    %   a closure involving another member of the SCC as the higher order
    %   value; and
    %
    % - procedures that are only called from one call site.
    %
    list.filter(
        should_proc_be_inlined(Params, !.ModuleInfo),
        SCCProcs, ShouldInlineSCCProcs),
    list.foldl(
        mark_proc_to_be_inlined(!.ModuleInfo),
        ShouldInlineSCCProcs, !ShouldInlineProcs),
    list.foldl(
        inline_in_proc_if_allowed(Params, !.ShouldInlineProcs, set.init),
        SCCProcs, !ModuleInfo).

    % This predicate effectively adds implicit `pragma inline' directives
    % for procedures that match its heuristic.
    %
:- pred maybe_mark_proc_to_be_inlined(inline_params::in, module_info::in,
    pred_proc_id::in, set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

maybe_mark_proc_to_be_inlined(Params, ModuleInfo, PredProcId,
        !ShouldInlineProcs) :-
    ( if should_proc_be_inlined(Params, ModuleInfo, PredProcId) then
        mark_proc_to_be_inlined(ModuleInfo, PredProcId, !ShouldInlineProcs)
    else
        true
    ).

:- pred mark_proc_to_be_inlined(module_info::in, pred_proc_id::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

mark_proc_to_be_inlined(ModuleInfo, PredProcId, !ShouldInlineProcs) :-
    set.insert(PredProcId, !ShouldInlineProcs),
    trace [io(!IO)] (
        write_proc_progress_message(ModuleInfo, "Inlining", PredProcId, !IO)
    ).

:- pred should_proc_be_inlined(inline_params::in, module_info::in,
    pred_proc_id::in) is semidet.

should_proc_be_inlined(Params, ModuleInfo, PredProcId) :-
    module_info_pred_proc_info(ModuleInfo, PredProcId, PredInfo, ProcInfo),
    proc_info_get_goal(ProcInfo, CalledGoal),
    PredProcId = proc(PredId, ProcId),
    Entity = entity_proc(PredId, ProcId),

    % The heuristic represented by the following code could be improved.
    (
        Params ^ ip_simple = inline_simple,
        SimpleThreshold = Params ^ ip_simple_goal_threshold,
        is_simple_goal(CalledGoal, SimpleThreshold)
    ;
        NeededMap = Params ^ ip_needed_map,
        map.search(NeededMap, Entity, Needed),
        Needed = maybe_eliminable(NumUses),
        goal_size(CalledGoal, Size),
        % The size increase due to inlining at a call site is not Size,
        % but the difference between Size and the size of the call.
        % CallCost is the user-provided approximation of the size of the call.
        CallCost = Params ^ ip_call_cost,
        CompoundThreshold = Params ^ ip_compound_size_threshold,
        CompoundThreshold > 0,
        (Size - CallCost) * NumUses =< CompoundThreshold
    ;
        Params ^ ip_single_use = inline_single_use,
        NeededMap = Params ^ ip_needed_map,
        map.search(NeededMap, Entity, Needed),
        Needed = maybe_eliminable(NumUses),
        NumUses = 1
    ),
    % Don't inline directly recursive predicates unless explicitly requested.
    not goal_calls(CalledGoal, PredProcId),

    pred_info_get_origin(PredInfo, Origin),
    origin_involves_daio(Origin, does_not_involve_daio).

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

%---------------------------------------------------------------------------%

:- pred inline_in_proc_if_allowed(inline_params::in,
    set(pred_proc_id)::in, set(pred_proc_id)::in, pred_proc_id::in,
    module_info::in, module_info::out) is det.

inline_in_proc_if_allowed(Params, ShouldInlineProcs, ShouldInlineTailProcs,
        PredProcId, !ModuleInfo) :-
    PredProcId = proc(PredId, _ProcId),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    pred_info_get_origin(PredInfo, Origin),
    origin_involves_daio(Origin, InvolvesDAIO),
    (
        InvolvesDAIO = does_not_involve_daio,
        inline_in_proc(Params, ShouldInlineProcs, ShouldInlineTailProcs,
            PredProcId, !ModuleInfo)
    ;
        InvolvesDAIO = does_involve_daio
    ).

:- pred inline_in_proc(inline_params::in,
    set(pred_proc_id)::in, set(pred_proc_id)::in, pred_proc_id::in,
    module_info::in, module_info::out) is det.

inline_in_proc(Params, ShouldInlineProcs, ShouldInlineTailProcs, PredProcId,
        !ModuleInfo) :-
    some [!PredInfo, !ProcInfo] (
        VarThresh = Params ^ ip_var_threshold,
        HighLevelCode = Params ^ ip_highlevel_code,

        PredProcId = proc(PredId, ProcId),

        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
        pred_info_proc_info(!.PredInfo, ProcId, !:ProcInfo),

        pred_info_get_univ_quant_tvars(!.PredInfo, UnivQTVars),
        pred_info_get_typevarset(!.PredInfo, TypeVarSet0),

        proc_info_get_goal(!.ProcInfo, Goal0),
        proc_info_get_var_table(!.ModuleInfo, !.ProcInfo, VarTypes0),
        proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),

        InlineInfo0 = inline_info(!.ModuleInfo, VarThresh, HighLevelCode,
            UnivQTVars, ShouldInlineTailProcs, ShouldInlineProcs,
            TypeVarSet0, VarTypes0, RttiVarMaps0,
            we_have_not_inlined, we_have_not_inlined_parallel_conj,
            have_not_changed_detism, have_not_changed_purity),

        inlining_in_goal(Goal0, Goal, InlineInfo0, InlineInfo),

        InlineInfo = inline_info(_, _, _, _, _, _,
            TypeVarSet, VarTypes, RttiVarMaps,
            DidInlining, InlinedParallel, DetChanged, PurityChanged),

        pred_info_set_typevarset(TypeVarSet, !PredInfo),

        proc_info_set_var_table(VarTypes, !ProcInfo),
        proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),
        proc_info_set_goal(Goal, !ProcInfo),

        (
            InlinedParallel = we_have_inlined_parallel_conj,
            proc_info_set_has_parallel_conj(has_parallel_conj, !ProcInfo)
        ;
            InlinedParallel = we_have_not_inlined_parallel_conj
        ),

        (
            DidInlining = we_have_inlined,
            % We want to requantify the procedure body if we did any inlining.
            % If the body of an inlined call did not use some of the
            % call's input arg vars, and this was the only use of the
            % corresponding caller variables, this will tell the simplification
            % pass we invoke before code generation that the goal(s) that
            % generate those caller variables can be optimized away.
            requantify_proc_general(ordinary_nonlocals_no_lambda, !ProcInfo),
            recompute_instmap_delta_proc(recompute_atomic_instmap_deltas,
                !ProcInfo, !ModuleInfo)
        ;
            DidInlining = we_have_not_inlined
        ),

        pred_info_set_proc_info(ProcId, !.ProcInfo, !PredInfo),

        (
            PurityChanged = have_changed_purity,
            repuritycheck_proc(!.ModuleInfo, PredProcId, !PredInfo)
        ;
            PurityChanged = have_not_changed_purity
        ),

        module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo),

        % If the determinism of some subgoals has changed, then we rerun
        % determinism analysis, because propagating the determinism information
        % through the procedure may lead to more efficient code.
        (
            DetChanged = may_have_changed_detism,
            det_infer_proc_ignore_msgs(PredId, ProcId, !ModuleInfo)
        ;
            DetChanged = have_not_changed_detism
        )
    ).

%---------------------------------------------------------------------------%

:- pred inlining_in_goal(hlds_goal::in, hlds_goal::out,
    inline_info::in, inline_info::out) is det.

inlining_in_goal(Goal0, Goal, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        inlining_in_call(GoalExpr0, GoalInfo0, Goal, !Info)
    ;
        ( GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ),
        Goal = Goal0
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
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Goals0),
        inlining_in_disjuncts(Goals0, Goals, !Info),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, Det, Cases0),
        inlining_in_cases(Cases0, Cases, !Info),
        GoalExpr = switch(Var, Det, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        inlining_in_goal(Cond0, Cond, !Info),
        inlining_in_goal(Then0, Then, !Info),
        inlining_in_goal(Else0, Else, !Info),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        inlining_in_goal(SubGoal0, SubGoal, !Info),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % The scope has no calls to inline.
            Goal = Goal0
        else
            inlining_in_goal(SubGoal0, SubGoal, !Info),
            GoalExpr = scope(Reason, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

%---------------------------------------------------------------------------%

:- pred inlining_in_disjuncts(list(hlds_goal)::in, list(hlds_goal)::out,
    inline_info::in, inline_info::out) is det.

inlining_in_disjuncts([], [], !Info).
inlining_in_disjuncts([Goal0 | Goals0], [Goal | Goals], !Info) :-
    inlining_in_goal(Goal0, Goal, !Info),
    inlining_in_disjuncts(Goals0, Goals, !Info).

:- pred inlining_in_cases(list(case)::in, list(case)::out,
    inline_info::in, inline_info::out) is det.

inlining_in_cases([], [], !Info).
inlining_in_cases([Case0 | Cases0], [Case | Cases], !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    inlining_in_goal(Goal0, Goal, !Info),
    Case = case(MainConsId, OtherConsIds, Goal),
    inlining_in_cases(Cases0, Cases, !Info).

%---------------------------------------------------------------------------%

:- pred inlining_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    inline_info::in, inline_info::out) is det.

inlining_in_conj([], [], !Info).
inlining_in_conj([HeadGoal0 | TailGoals0], Goals, !Info) :-
    inlining_in_goal(HeadGoal0, HeadGoal, !Info),
    inlining_in_conj(TailGoals0, TailGoals, !Info),
    % Since a single goal may become a conjunction,
    % we flatten the conjunction as we go.
    goal_to_conj_list(HeadGoal, HeadGoalList),
    Goals = HeadGoalList ++ TailGoals.

:- pred inlining_in_par_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    inline_info::in, inline_info::out) is det.

inlining_in_par_conj([], [], !Info).
inlining_in_par_conj([HeadGoal0 | TailGoals0], Goals, !Info) :-
    inlining_in_goal(HeadGoal0, HeadGoal, !Info),
    inlining_in_par_conj(TailGoals0, TailGoals, !Info),
    % Since a single goal may become a parallel conjunction,
    % we flatten the conjunction as we go.
    % Note that this is *much* less likely to happen for parallel conjunctions
    % than for sequential ones.
    goal_to_par_conj_list(HeadGoal, HeadGoalList),
    Goals = HeadGoalList ++ TailGoals.

%---------------------------------------------------------------------------%

:- pred inlining_in_call(hlds_goal_expr::in(goal_expr_plain_call),
    hlds_goal_info::in, hlds_goal::out,
    inline_info::in, inline_info::out) is det.

inlining_in_call(GoalExpr0, GoalInfo0, Goal, !Info) :-
    !.Info = inline_info(ModuleInfo, VarThresh, HighLevelCode,
        ExternalTypeParams, ShouldInlineTailProcs, ShouldInlineProcs,
        TypeVarSet0, VarTable0, RttiVarMaps0, _DidInlining0,
        InlinedParallel0, DetChanged0, PurityChanged0),
    GoalExpr0 =
        plain_call(PredId, ProcId, ArgVars, _Builtin, _Context, _SymName),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
    % Should we inline this call?
    should_inline_at_call_site(!.Info, GoalExpr0, GoalInfo0, ShouldInline),
    ( if
        ShouldInline = should_inline(TailRec, UserReq),
        (
            UserReq = user_req
        ;
            UserReq = not_user_req,
            % Okay, but will we exceed the number-of-variables threshold?
            var_table_count(VarTable0, NumVarsInVarTable),

            % We need to find out how many variables the Callee has.
            proc_info_get_var_table(ModuleInfo, ProcInfo, CalleeVarTable),
            var_table_count(CalleeVarTable, NumVarsInCallee),
            TotalNumVars = NumVarsInVarTable + NumVarsInCallee,
            TotalNumVars =< VarThresh
        ),
        % XXX Work around bug #142.
        not may_encounter_bug_142(ProcInfo, ArgVars)
    then
        do_inline_call(ModuleInfo, ExternalTypeParams, ArgVars,
            PredInfo, ProcInfo, TypeVarSet0, TypeVarSet, VarTable0, VarTable,
            RttiVarMaps0, RttiVarMaps, Goal1),

        DidInlining = we_have_inlined,

        proc_info_get_has_parallel_conj(ProcInfo, HasParallelConj),
        (
            HasParallelConj = has_parallel_conj,
            InlinedParallel = we_have_inlined_parallel_conj
        ;
            HasParallelConj = has_no_parallel_conj,
            InlinedParallel = InlinedParallel0
        ),

        Goal1 = hlds_goal(_, GoalInfo1),
        % If the determinism of the call is the same as the determinism
        % of the callee (which it should be, unless something has changed
        % since determinism analysis) *and* all the argument variables are
        % used outside the call, then there is no need to rerun determinism
        % analysis. We *do* have to rerun it if we have not met one of the
        % above preconditions.
        Determinism0 = goal_info_get_determinism(GoalInfo0),
        Determinism1 = goal_info_get_determinism(GoalInfo1),
        ArgVarSet = set_of_var.list_to_set(ArgVars),
        NonLocals = goal_info_get_nonlocals(GoalInfo0),
        ( if
            Determinism0 = Determinism1,
            set_of_var.subset(ArgVarSet, NonLocals)
        then
            DetChanged = DetChanged0
        else
            DetChanged = may_have_changed_detism
        ),

        Purity0 = goal_info_get_purity(GoalInfo0),
        Purity1 = goal_info_get_purity(GoalInfo1),
        ( if Purity0 = Purity1 then
            PurityChanged = PurityChanged0
        else
            PurityChanged = have_changed_purity
        ),

        !:Info = inline_info(ModuleInfo, VarThresh, HighLevelCode,
            ExternalTypeParams, ShouldInlineTailProcs, ShouldInlineProcs,
            TypeVarSet, VarTable, RttiVarMaps, DidInlining,
            InlinedParallel, DetChanged, PurityChanged),

        (
            TailRec = not_tail_rec,
            Goal = Goal1
        ;
            TailRec = tail_rec,
            inlining_in_goal(Goal1, Goal, !Info)
        )
    else
        Goal = hlds_goal(GoalExpr0, GoalInfo0)
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

%---------------------------------------------------------------------------%

do_inline_call(ModuleInfo, ExternalTypeParams, ArgVars, PredInfo, ProcInfo,
        TypeVarSet0, TypeVarSet, VarTable0, VarTable,
        RttiVarMaps0, RttiVarMaps, Goal) :-
    proc_info_get_goal(ProcInfo, CalledGoal),

    % Look up the rest of the info for the called procedure.

    pred_info_get_typevarset(PredInfo, CalleeTypeVarSet),
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_var_table(ModuleInfo, ProcInfo, CalleeVarTable0),
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
    % will not be substituted away.)

    tvarset_merge_renaming(TypeVarSet0, CalleeTypeVarSet, TypeVarSet,
        TypeRenaming),
    apply_variable_renaming_to_var_table(TypeRenaming,
        CalleeVarTable0, CalleeVarTable1),

    % Next, compute the type substitution and then apply it.

    % Note: there is no need to update the type_info locations maps,
    % either for the caller or callee, since for any type vars in the
    % callee which get bound to type vars in the caller, the type_info
    % location will be given by the entry in the caller's type_info
    % locations map (and vice versa).  It doesn't matter if the final
    % type_info locations map contains some entries for type variables
    % which have been substituted away, because those entries simply
    % won't be used.

    lookup_var_types(CalleeVarTable1, HeadVars, HeadTypes),
    lookup_var_types(VarTable0, ArgVars, ArgTypes),

    pred_info_get_exist_quant_tvars(PredInfo, CalleeExistQVars),
    compute_caller_callee_type_substitution(HeadTypes, ArgTypes,
        ExternalTypeParams, CalleeExistQVars, TypeSubn),

    % Update types in the callee.
    apply_rec_subst_to_var_table(TypeSubn, CalleeVarTable1, CalleeVarTable),
    % Handle the common case of non-existentially typed preds specially,
    % since we can do things more efficiently in that case.
    (
        CalleeExistQVars = [],
        VarTable1 = VarTable0
    ;
        CalleeExistQVars = [_ | _],
        % Update types in the caller.
        apply_rec_subst_to_var_table(TypeSubn, VarTable0, VarTable1)
    ),

    % Now rename apart the variables in the called goal.
    rename_goal(HeadVars, ArgVars, VarTable1, CalleeVarTable, VarTable,
        Subn, CalledGoal, Goal),

    apply_substitutions_to_rtti_varmaps(TypeRenaming, TypeSubn, Subn,
        CalleeRttiVarMaps0, CalleeRttiVarMaps1),

    % Prefer the type_info_locn from the caller.
    % The type_infos or typeclass_infos passed to the callee may have been
    % produced by extracting type_infos or typeclass_infos from
    % typeclass_infos in the caller, so they won't necessarily be the same.
    rtti_varmaps_overlay(CalleeRttiVarMaps1, RttiVarMaps0, RttiVarMaps).

rename_goal(HeadVars, ArgVars, VarTable0, CalleeVarTable, VarTable,
        Renaming, CalledGoal, Goal) :-
    map.from_corresponding_lists(HeadVars, ArgVars, Renaming0),
    var_table_vars(CalleeVarTable, CalleeListOfVars),
    clone_variables_var_table(CalleeListOfVars, CalleeVarTable,
        VarTable0, VarTable, Renaming0, Renaming),
    must_rename_vars_in_goal(Renaming, CalledGoal, Goal).

%---------------------------------------------------------------------------%

:- type maybe_user_req
    --->    not_user_req
    ;       user_req.

:- type maybe_tail_rec
    --->    not_tail_rec
    ;       tail_rec.

:- type maybe_should_inline
    --->    should_not_inline
    ;       should_inline(maybe_tail_rec, maybe_user_req).

    % Check to see if we should inline the callee at a call site.
    %
    % Returns should_not_inline if the called predicate cannot be inlined,
    % e.g. because it is a builtin, we don't have code for it, etc,
    % or if the callee is simply not in the set of procedures
    % that we have earlier decided we should inline.
    %
    % Returns should_inline(TailRec, UserReq) if the called procedure
    % is inlinable, and we have earlier decided that it *should* be inlined.
    %
:- pred should_inline_at_call_site(inline_info::in,
    hlds_goal_expr::in(goal_expr_plain_call), hlds_goal_info::in,
    maybe_should_inline::out) is det.

should_inline_at_call_site(Info, GoalExpr0, GoalInfo0, ShouldInline) :-
    Info = inline_info(ModuleInfo, _VarThresh, HighLevelCode,
        _ExternalTypeParams, ShouldInlineTailProcs, ShouldInlineProcs,
        _TypeVarSet, _VarTypes, _RttiVarMaps, _DidInlining,
        _InlinedParallel, _DetChanged, _PurityChanged),
    GoalExpr0 =
        plain_call(PredId, ProcId, _ArgVars, Builtin, _Context, _SymName),
    PredProcId = proc(PredId, ProcId),
    ( if
        set.member(PredProcId, ShouldInlineTailProcs),
        goal_info_has_feature(GoalInfo0, feature_self_or_mutual_tail_rec_call)
    then
        TailRec = tail_rec
    else
        TailRec = not_tail_rec
    ),
    ( if
        can_inline_proc_2(ModuleInfo, PredId, ProcId, Builtin,
            HighLevelCode, may_inline_purity_promised_pred)
    then
        % OK, we could inline it - but should we? Apply our heuristics.
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_markers(PredInfo, Markers),
        ( if
            check_marker(Markers, marker_user_marked_inline)
        then
            UserReq = user_req
        else
            UserReq = not_user_req
        ),
        ( if
            ( TailRec = tail_rec
            ; UserReq = user_req
            ; check_marker(Markers, marker_heuristic_inline)
            ; set.member(PredProcId, ShouldInlineProcs)
            )
        then
            ShouldInline = should_inline(TailRec, UserReq)
        else
            ShouldInline = should_not_inline
        )
    else
        ShouldInline = should_not_inline
    ).

can_inline_proc(ModuleInfo, PredId, ProcId, BuiltinState,
        MayInlinePromisedPure) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    can_inline_proc_2(ModuleInfo, PredId, ProcId, BuiltinState, HighLevelCode,
        MayInlinePromisedPure).

:- pred can_inline_proc_2(module_info::in, pred_id::in, proc_id::in,
    builtin_state::in, bool::in, may_inline_purity_promised_pred::in)
    is semidet.

can_inline_proc_2(ModuleInfo, PredId, ProcId, BuiltinState, HighLevelCode,
        MayInlinePurityPromisedPred) :-
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
        % XXX We should not have any model_non foreign_procs anymore.
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
        MayInlinePurityPromisedPred = may_inline_purity_promised_pred
    ;
        % For some optimizations (such as deforestation) we don't want to
        % inline predicates which are promised pure because the extra impurity
        % propagated through the goal will defeat any attempts at optimization.
        %
        % XXX For such procedures, we could wrap a purity promise scope
        % promising the same purity around the procedure body.
        MayInlinePurityPromisedPred = may_not_inline_purity_promised_pred,
        pred_info_get_promised_purity(PredInfo, MaybePromisedPurity),
        MaybePromisedPurity = no
    ).

    % Succeed iff it is appropriate to inline `pragma foreign_proc'
    % in the specified language for the given compilation_target.
    % Generally that will only be the case if the target directly
    % supports inline code in that language.
    %
:- pred ok_to_inline_language(foreign_language::in, compilation_target::in)
    is semidet.

ok_to_inline_language(lang_c, target_c).
ok_to_inline_language(lang_java, target_java).
ok_to_inline_language(lang_csharp, target_csharp).

%---------------------------------------------------------------------------%
%
% The direct_arg_in_out transformation (daio for short) creates code
% with insts that are good enough to be accepted by the code generators,
% but too fragile to be processed by transformations such as inlining.
% Invoking inlining on a predicate created by the daio transformation
% was responsible for Mantis bug #542.
%

:- type maybe_involves_daio
    --->    does_not_involve_daio
    ;       does_involve_daio.

:- pred origin_involves_daio(pred_origin::in,
    maybe_involves_daio::out) is det.

origin_involves_daio(Origin, InvolvesDAIO) :-
    (
        ( Origin = origin_special_pred(_SpecialPredId, _TypeCtor)
        ; Origin = origin_instance_method(_SymName, _Constraints)
        ; Origin = origin_class_method(_ClassId, _PFSymNameArity)
        ; Origin = origin_created(_Creation)
        ; Origin = origin_assertion(_, _)
        ; Origin = origin_lambda(_FileNam, _LineNum, _Seq)
        ; Origin = origin_solver_type(_SymName, _Arity, _PredKind)
        ; Origin = origin_tabling(_PFSymNameArity, _PredKind)
        ; Origin = origin_mutable(_ModuleName, _MutableName, _PredKind)
        ; Origin = origin_initialise
        ; Origin = origin_finalise
        ; Origin = origin_user(_SymName)
        ),
        InvolvesDAIO = does_not_involve_daio
    ;
        Origin = origin_transformed(Transform, SubOrigin, _PredId),
        ( if
            ( origin_transformation_involves_daio(Transform, does_involve_daio)
            ; origin_involves_daio(SubOrigin, does_involve_daio)
            )
        then
            InvolvesDAIO = does_involve_daio
        else
            InvolvesDAIO = does_not_involve_daio
        )
    ).

:- pred origin_transformation_involves_daio(pred_transformation::in,
    maybe_involves_daio::out) is det.

origin_transformation_involves_daio(Transform, InvolvesDAIO) :-
    (
        ( Transform = transform_higher_order_specialization(_)
        ; Transform = transform_higher_order_type_specialization(_)
        ; Transform = transform_type_specialization(_)
        ; Transform = transform_unused_argument_elimination(_)
        ; Transform = transform_accumulator(_)
        ; Transform = transform_loop_invariant(_)
        ; Transform = transform_tuple(_)
        ; Transform = transform_untuple(_)
        ; Transform = transform_dependent_parallel_conjunction
        ; Transform = transform_parallel_loop_control
        ; Transform = transform_return_via_ptr(_, _)
        ; Transform = transform_table_generator
        ; Transform = transform_stm_expansion
        ; Transform = transform_dnf(_)
        ; Transform = transform_structure_reuse
        ; Transform = transform_source_to_source_debug
        ),
        InvolvesDAIO = does_not_involve_daio
    ;
        Transform = transform_direct_arg_in_out,
        InvolvesDAIO = does_involve_daio
    ).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.inlining.
%---------------------------------------------------------------------------%
