%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: tupling.m.
%
% Author: wangp.
%
% This module takes the HLDS and performs a tupling transformation on the
% locally-defined procedures.  That is, instead of passing all of the
% procedure's arguments separately, it will try to bundle some of them up and
% pass them together as a tuple.
%
% The idea is that some arguments passed to a procedure may not be needed
% immediately: between the start of the procedure and the first use of a
% given argument there may be a flush point, such as a call to another
% procedure.  At these points all values residing in registers that will be
% needed later in the procedure will need to be flushed to the stack, to be
% restored later.  In some cases, it may be beneficial to refer to some
% arguments indirectly through a cell variable.  Flushing the (address of
% the) cell variable to the stack is enough to save all the field variables
% of the cell.  The downside is that accessing a field variable requires
% going through a cell variable (the cost of which may be amortised if
% multiple field variables are needed in the same interval).
%
% Another potentially good reason to pass arguments in a tuple is if many
% procedures will be passing the same arguments to each other, e.g. as is
% often the case in mutually-recursive procedures.
%
% This implementation works as follows:
%
% 1. The module is divided up into its constituent SCCs.  We work our way
% through each SCC, starting from the bottommost SCC in the call graph.
%
% 2. For each SCC, we take guesses at a good tupling scheme for the
% procedures in the SCC and count the average number of loads and stores
% between the registers and the stack for each given scheme.
%
% 3. If the best tupling scheme gives us an average number of loads/stores
% that compares favourably against the original (untupled) scheme, we go
% ahead and make the transformed versions of the procedures in the SCC and
% add them to the HLDS.
%
% 4. After all the SCCs have been processed, we update all calls to the
% original procedures to call their transformed versions instead.
%
% Step 2 in more detail:
%
% This implementation uses the names of input formal parameters to guess
% which values are common between the procedures in an SCC (for SCCs with
% more than one procedure).  i.e. if a variable name occurs as an input
% argument to more than one procedure in the SCC, those variables
% corresponding that name are candidates for tupling.  In the interest of
% speeding up compilation times, the implementation only tries to tuple
% contiguous runs of the candidate variables.
% e.g. if the candidates are [A,B,C,D] these combinations would be tested in
% turn, assuming a minimum run length of 3: {A,B,C,D}, {A,B,C}, {B,C,D}
%
% To count the average number of loads and stores in a procedure we traverse
% the procedure's body, remembering which values are available in registers
% and the stack.  When we reach a branch point, we use the relative
% frequencies that each branch was taken in a sample run to weight the costs
% incurred in each branch.  The relative frequency data is gathered from the
% trace count summary file that must be provided by the user.
%
% Ideas for further work:
%
% - Smarter / more aggressive choosing of arguments to tuple
% - Inter-SCC analysis
% - Inter-module optimisation
% - Proper weighting of calls to procedures from within and without the SCC
%
% This transformation is similar in spirit to the transformation in
% stack_opt.m.  It also shares much code with it.
%
% XXX: we need to check that mprof can demangle the names of the transformed
% procedures correctly
%
%-----------------------------------------------------------------------------%

:- module transform_hlds__tupling.

:- interface.

:- import_module hlds.hlds_module.

:- import_module io.

:- pred tuple_arguments(module_info::in, module_info::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.interval.
:- import_module check_hlds.goal_path.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.arg_info.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.
:- import_module ll_backend.call_gen.
:- import_module ll_backend.liveness.
:- import_module ll_backend.live_vars.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.trace_counts.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module transform_hlds.dependency_graph.
:- use_module mdbcomp.program_representation.

:- import_module assoc_list.
:- import_module bool.
:- import_module counter.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module multi_map.
:- import_module relation.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module svmap.
:- import_module svset.
:- import_module svvarset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%
%
% Top-level
%

tuple_arguments(!ModuleInfo, !IO) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals__lookup_string_option(Globals,
        tuple_trace_counts_file, TraceCountsFile),
    ( TraceCountsFile = "" ->
        report_warning("Warning: --tuple requires " ++
            "--tuple-trace-counts-file to work.\n", !IO)
    ;
        read_trace_counts_source(no, try_single_first, TraceCountsFile,
            Result, !IO),
        (
            Result = list_ok(_, TraceCounts),
            tuple_arguments_2(!ModuleInfo, TraceCounts, !IO)
        ;
            Result = list_error_message(Message),
            warn_trace_counts_error(TraceCountsFile, Message, !IO)
        )
    ).

:- pred tuple_arguments_2(module_info::in, module_info::out, trace_counts::in,
    io::di, io::uo) is det.

tuple_arguments_2(!ModuleInfo, TraceCounts0, !IO) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    % We use the same cost options as for the stack optimisation.
    globals__lookup_int_option(Globals,
        optimize_saved_vars_cell_cv_load_cost, CellVarLoadCost),
    globals__lookup_int_option(Globals,
        optimize_saved_vars_cell_cv_store_cost, CellVarStoreCost),
    globals__lookup_int_option(Globals,
        optimize_saved_vars_cell_fv_load_cost, FieldVarLoadCost),
    globals__lookup_int_option(Globals,
        optimize_saved_vars_cell_fv_store_cost, FieldVarStoreCost),
    globals__lookup_int_option(Globals, tuple_costs_ratio, CostsRatio),
    globals__lookup_int_option(Globals, tuple_min_args, MinArgsToTuple),
    % These are the costs for untupled variables.  We just assume it is
    % the lesser of the cell and field variable costs (usually the field
    % variable costs should be smaller).
    NormalVarStoreCost = min(CellVarStoreCost, FieldVarStoreCost),
    NormalVarLoadCost = min(CellVarLoadCost, FieldVarLoadCost),
    TuningParams = tuning_params(
        NormalVarLoadCost, NormalVarStoreCost,
        CellVarLoadCost, CellVarStoreCost,
        FieldVarLoadCost, FieldVarStoreCost,
        CostsRatio, MinArgsToTuple),

    module_info_get_name(!.ModuleInfo, ModuleName),
    restrict_trace_counts_to_module(ModuleName, TraceCounts0, TraceCounts),

    module_info_ensure_dependency_info(!ModuleInfo),
    module_info_dependency_info(!.ModuleInfo, DepInfo),
    hlds_dependency_info_get_dependency_graph(DepInfo, DepGraph),
    hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs),

    % Add transformed versions of procedures that we think would be
    % beneficial.
    list__foldl4(maybe_tuple_scc(TraceCounts, TuningParams, DepGraph),
        SCCs, !ModuleInfo, counter__init(0), _,
        map__init, TransformMap, !IO),

    % Update the callers of the original procedures to call their
    % transformed versions instead.  Do the same for the transformed
    % procedures themselves.
    list__foldl(fix_calls_in_procs(TransformMap), SCCs, !ModuleInfo),
    fix_calls_in_transformed_procs(TransformMap, !ModuleInfo).

:- pred warn_trace_counts_error(string::in, string::in, io::di, io::uo) is det.

warn_trace_counts_error(TraceCountsFile, Reason, !IO) :-
    string__format(
        "Warning: unable to read trace count summary from %s (%s)\n",
        [s(TraceCountsFile), s(Reason)], Message),
    report_warning(Message, !IO).

%-----------------------------------------------------------------------------%

    % This predicate can be used in place of maybe_tuple_scc to evaluate
    % and transform each procedure of an SCC individually. This is to mimic
    % the behaviour from an earlier version of this file. It's currently
    % unused but might be useful for debugging.
    %
:- pred maybe_tuple_scc_individual_procs(trace_counts::in, tuning_params::in,
    dependency_graph::in, list(pred_proc_id)::in,
    module_info::in, module_info::out, counter::in, counter::out,
    transform_map::in, transform_map::out, io::di, io::uo) is det.

maybe_tuple_scc_individual_procs(_TraceCounts, _TuningParams, _DepGraph,
        [], !ModuleInfo, !Counter, !TransformMap, !IO).
maybe_tuple_scc_individual_procs(TraceCounts, TuningParams, DepGraph,
        [Proc | Procs], !ModuleInfo, !Counter, !TransformMap, !IO) :-
    maybe_tuple_scc(TraceCounts, TuningParams, DepGraph,
        [Proc], !ModuleInfo, !Counter, !TransformMap, !IO),
    maybe_tuple_scc_individual_procs(TraceCounts, TuningParams, DepGraph,
        Procs, !ModuleInfo, !Counter, !TransformMap, !IO).

:- pred maybe_tuple_scc(trace_counts::in, tuning_params::in,
    dependency_graph::in, list(pred_proc_id)::in(bound([ground | ground])),
    module_info::in, module_info::out, counter::in, counter::out,
    transform_map::in, transform_map::out, io::di, io::uo) is det.

maybe_tuple_scc(TraceCounts, TuningParams, DepGraph, SCC,
        !ModuleInfo, !Counter, !TransformMap, !IO) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals__lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = yes,
        io.write_string("% Considering tupling in ", !IO),
        list__foldl((pred(PredProcId::in, IO0::di, IO::uo) is det :-
            PredProcId = proc(PredId, ProcId),
            hlds_out__write_pred_proc_id(!.ModuleInfo,
                PredId, ProcId, IO0, IO)),
            SCC, !IO),
        io.write_string("\n", !IO)
    ;
        VeryVerbose = no
    ),
    ( scc_has_local_callers(SCC, DepGraph) ->
        ( SCC = [SingleProc] ->
            candidate_headvars_of_proc(!.ModuleInfo, SingleProc,
                CandidateHeadVars)
        ;
            common_candidate_headvars_of_procs(!.ModuleInfo, SCC,
                CandidateHeadVars)
        ),
        MinArgsToTuple = TuningParams ^ min_args_to_tuple,
        ( list__length(CandidateHeadVars) < MinArgsToTuple ->
            (
                VeryVerbose = yes,
                io__write_string("% Too few candidate headvars\n", !IO)
            ;
                VeryVerbose = no
            )
        ;
            maybe_tuple_scc_2(TraceCounts, TuningParams,
                SCC, CandidateHeadVars, !ModuleInfo,
                !Counter, !TransformMap, !IO, VeryVerbose)
        )
    ;
        % No need to work on this SCC if there are no callers to it
        % within this module.
        %
        % XXX: If part of the SCC is exported then we might want
        % to look at it, for intermodule tupling.
        (
            VeryVerbose = yes,
            io__write_string("% SCC has no local callers\n", !IO)
        ;
            VeryVerbose = no
        )
    ).

:- pred scc_has_local_callers(list(pred_proc_id)::in, dependency_graph::in)
    is semidet.

scc_has_local_callers(CalleeProcs, DepGraph) :-
    some [CalleeProc] (
        CalleeProc `list.member` CalleeProcs,
        proc_has_local_callers(CalleeProc, DepGraph)
    ).

:- pred proc_has_local_callers(pred_proc_id::in, dependency_graph::in)
    is semidet.

proc_has_local_callers(CalleeProc, DepGraph) :-
    relation__lookup_element(DepGraph, CalleeProc, CalleeKey),
    relation__lookup_to(DepGraph, CalleeKey, CallingKeys),
    not set__empty(CallingKeys).

%-----------------------------------------------------------------------------%

:- pred maybe_tuple_scc_2(trace_counts::in, tuning_params::in,
    list(pred_proc_id)::in, candidate_headvars::in,
    module_info::in, module_info::out, counter::in, counter::out,
    transform_map::in, transform_map::out, io::di, io::uo, bool::in) is det.

maybe_tuple_scc_2(TraceCounts, TuningParams, PredProcIds, CandidateHeadVars,
        !ModuleInfo, !Counter, !TransformMap, !IO, VeryVerbose) :-
    list__foldl2(prepare_proc_for_counting, PredProcIds, !ModuleInfo, !IO),
    % Count the average number of loads/stores without any transformation.
    count_load_stores_for_scc(TraceCounts, TuningParams, !.ModuleInfo,
        map__init, PredProcIds, CostsWithoutTupling),
    (
        VeryVerbose = yes,
        CostsWithoutTupling = costs(LoadsWoTupling, StoresWoTupling),
        io.format("%% SCC costs without tupling = {%g, %g}\n",
            [f(LoadsWoTupling), f(StoresWoTupling)], !IO)
    ;
        VeryVerbose = no
    ),
    ( CostsWithoutTupling = costs(0.0, 0.0) ->
        % Don't bother continuing.
        true
    ;
        maybe_tuple_scc_3(TraceCounts, TuningParams, PredProcIds,
            CandidateHeadVars, CostsWithoutTupling,
            !ModuleInfo, !Counter, !TransformMap, !IO, VeryVerbose)
    ).

:- pred maybe_tuple_scc_3(trace_counts::in, tuning_params::in,
    list(pred_proc_id)::in, candidate_headvars::in, costs::in,
    module_info::in, module_info::out, counter::in, counter::out,
    transform_map::in, transform_map::out, io::di, io::uo, bool::in) is det.

maybe_tuple_scc_3(TraceCounts, TuningParams, PredProcIds, CandidateHeadVars,
        CostsWithoutTupling,
        !ModuleInfo, !Counter, !TransformMap, !IO, VeryVerbose) :-
    find_best_tupling_scheme(TraceCounts, TuningParams, !.ModuleInfo,
        PredProcIds, CandidateHeadVars, MaybeBestScheme),
    (
        MaybeBestScheme = no
    ;
        MaybeBestScheme = yes(CostsWithTupling-TuplingScheme),
        CostsWithTupling = costs(LoadsWithTupling, StoresWithTupling),
        (
            VeryVerbose = yes,
            io.format("%% SCC costs with tupling = {%g, %g}\n",
                [f(LoadsWithTupling), f(StoresWithTupling)], !IO)
        ;
            VeryVerbose = no
        ),
        (
            should_use_tupling_scheme(TuningParams,
                CostsWithoutTupling, CostsWithTupling)
        ->
            (
                VeryVerbose = yes,
                io.print("% Proceeding with tupling\n", !IO)
            ;
                VeryVerbose = no
            ),
            add_transformed_procs(TuplingScheme,
                !ModuleInfo, !Counter, !TransformMap)
        ;
            true
        )
    ).

:- pred should_use_tupling_scheme(tuning_params::in, costs::in, costs::in)
    is semidet.

should_use_tupling_scheme(TuningParams,
        costs(LoadsWithoutTupling, StoresWithoutTupling),
        costs(LoadsWithTupling, StoresWithTupling)) :-
    CostsRatio = float(TuningParams ^ costs_ratio),
    TotalWithoutTupling = LoadsWithoutTupling + StoresWithoutTupling,
    TotalWithTupling = LoadsWithTupling + StoresWithTupling,
    ( TotalWithTupling = 0.0 ->
        TotalWithoutTupling > 0.0
    ;
        (TotalWithoutTupling * 100.0 / TotalWithTupling) >= CostsRatio
    ).

%-----------------------------------------------------------------------------%

:- type candidate_headvars == assoc_list(string, candidate_headvar_origins).

:- type candidate_headvar_origins == map(pred_proc_id, prog_var).

    % The "candidate headvars" of a procedure are the input arguments of
    % a procedure that we are considering to pass to the procedure as a
    % tuple.
    %
    % The "common" candidate headvars of an SCC (of more than one
    % procedure) are the input arguments that, when passed as a tuple, we
    % hope can be reused in calls to other procedures in the same SCC.
    % The heuristic used to find candidates is to look for input arguments
    % which have the same name in more than one procedure in the SCC.
    %
    % For each candidate, the name is put in an association list along
    % with a mappping to the actual variable within each procedure (if
    % that procedure has an input variable of the given name).  The order
    % of the elements in the association list is important later on,
    % since we only try tupling contiguous runs of the candidate variables.
    %
:- pred candidate_headvars_of_proc(module_info::in, pred_proc_id::in,
    candidate_headvars::out) is det.

candidate_headvars_of_proc(ModuleInfo, PredProcId @ proc(PredId, ProcId),
        CandidateHeadVars) :-
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_varset(ProcInfo, VarSet),
    proc_info_vartypes(ProcInfo, VarTypes),
    proc_info_headvars(ProcInfo, HeadVars),
    proc_info_argmodes(ProcInfo, ArgModes),
    CandidateHeadVars = list__filter_map_corresponding(
        candidate_headvars_of_proc_2(PredProcId, VarSet, VarTypes, ModuleInfo),
        HeadVars, ArgModes).

:- func candidate_headvars_of_proc_2(pred_proc_id, prog_varset, vartypes,
    module_info, prog_var, mer_mode)
    = pair(string, candidate_headvar_origins) is semidet.

candidate_headvars_of_proc_2(PredProcId, VarSet, VarTypes, ModuleInfo,
        HeadVar, ArgMode) = (Name - Origins) :-
    % We only tuple input arguments.
    mode_is_input(ModuleInfo, ArgMode),
    % Don't touch introduced typeinfo arguments.
    map__lookup(VarTypes, HeadVar, Type),
    not is_introduced_type_info_type(Type),
    varset__search_name(VarSet, HeadVar, Name),
    map__det_insert(map__init, PredProcId, HeadVar, Origins).

:- pred common_candidate_headvars_of_procs(module_info::in,
    list(pred_proc_id)::in, candidate_headvars::out) is det.

common_candidate_headvars_of_procs(ModuleInfo, PredProcIds,
        CandidateHeadVars) :-
    list__map(candidate_headvars_of_proc(ModuleInfo),
        PredProcIds, ListsOfCandidates),
    list__condense(ListsOfCandidates, FlatListOfCandidates),
    multi_map__from_flat_assoc_list(FlatListOfCandidates, CandidatesMultiMap),
    map__foldl(common_candidate_headvars_of_procs_2, CandidatesMultiMap,
        [], CandidateHeadVars).

:- pred common_candidate_headvars_of_procs_2(
    string::in, list(candidate_headvar_origins)::in,
    candidate_headvars::in, candidate_headvars::out) is det.

common_candidate_headvars_of_procs_2(HeadVarName, ListOfOrigins,
        CandidateHeadVars0, CandidateHeadVars) :-
    % Only include this variable in the list of candidates if there are two
    % or more procedures in the SCC with head variables having the same name.
    ( ListOfOrigins = [_, _ | _] ->
        list__foldl(map__merge, ListOfOrigins, map__init, Origins),
        CandidateHeadVars = CandidateHeadVars0 ++ [HeadVarName - Origins]
    ;
        CandidateHeadVars = CandidateHeadVars0
    ).

%-----------------------------------------------------------------------------%

    % This is a mapping from the id of a procedure to the proposed
    % tupling that would be performed on the procedure's input arguments.
    %
:- type tupling_scheme == map(pred_proc_id, tupling_proposal).

:- type tupling_proposal
    --->    no_tupling
    ;       tupling(
                cell_var            :: prog_var,
                field_vars          :: prog_vars,
                field_var_arg_pos   :: assoc_list(prog_var, int)
            ).

:- pred find_best_tupling_scheme(trace_counts::in, tuning_params::in,
    module_info::in, list(pred_proc_id)::in, candidate_headvars::in,
    maybe(pair(costs, tupling_scheme))::out) is det.

find_best_tupling_scheme(TraceCounts, TuningParams, ModuleInfo,
        PredProcIds, CandidateHeadVars, MaybeBestScheme) :-
    MinArgsToTuple = TuningParams ^ min_args_to_tuple,
    fold_over_list_runs(
        find_best_tupling_scheme_2(TraceCounts, TuningParams,
            ModuleInfo, PredProcIds),
        CandidateHeadVars, MinArgsToTuple,
        no, MaybeBestScheme).

:- pred find_best_tupling_scheme_2(trace_counts::in, tuning_params::in,
    module_info::in, list(pred_proc_id)::in, candidate_headvars::in,
    maybe(pair(costs, tupling_scheme))::in,
    maybe(pair(costs, tupling_scheme))::out) is det.

find_best_tupling_scheme_2(TraceCounts, TuningParams, ModuleInfo,
        PredProcIds, CandidateHeadVars,
        MaybeBestScheme0, MaybeBestScheme) :-
    MinArgsToTuple = TuningParams ^ min_args_to_tuple,
    list__map(
        make_tupling_proposal(ModuleInfo, CandidateHeadVars, MinArgsToTuple),
        PredProcIds, TuplingProposals),
    map__from_corresponding_lists(PredProcIds, TuplingProposals,
        TuplingScheme),
    count_load_stores_for_scc(TraceCounts, TuningParams, ModuleInfo,
        TuplingScheme, PredProcIds, Costs),
    (
        (
            MaybeBestScheme0 = no
        ;
            MaybeBestScheme0 = yes(PrevCosts - _),
            less_total_cost(Costs, PrevCosts)
        )
    ->
        MaybeBestScheme = yes(Costs - TuplingScheme)
    ;
        MaybeBestScheme = MaybeBestScheme0
    ).

:- pred make_tupling_proposal(module_info::in, candidate_headvars::in,
    int::in, pred_proc_id::in, tupling_proposal::out) is det.

make_tupling_proposal(ModuleInfo, CandidateHeadVars, MinArgsToTuple,
        PredProcId @ proc(PredId, ProcId), TuplingProposal) :-
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_varset(ProcInfo, VarSet),
    proc_info_headvars(ProcInfo, HeadVars),
    FieldVarArgPos = list__filter_map(
        (func(_ - Annotation) = (Var - Pos) is semidet :-
            map__search(Annotation, PredProcId, Var),
            list__nth_member_search(HeadVars, Var, Pos)),
        CandidateHeadVars),
    ( list__length(FieldVarArgPos) < MinArgsToTuple ->
        TuplingProposal = no_tupling
    ;
        % We need a new variable to act as the cell variable while
        % counting loads/stores for a proposed tupling, but we don't
        % add that variable to the varset permanently.
        varset__new_named_var(VarSet, "DummyCellVar", DummyCellVar, _),
        FieldVars = assoc_list__keys(FieldVarArgPos),
        TuplingProposal = tupling(DummyCellVar, FieldVars, FieldVarArgPos)
    ).

:- pred less_total_cost(costs::in, costs::in) is semidet.

less_total_cost(costs(LoadsA, StoresA), costs(LoadsB, StoresB)) :-
    TotalA = LoadsA + StoresA,
    TotalB = LoadsB + StoresB,
    TotalA < TotalB.

%-----------------------------------------------------------------------------%

    % fold_over_list_runs(Pred, List, MinRunLength, !Acc):
    %
    % Call Pred for each consecutive run of elements in List of a length
    % greater or equal to MinRunLength, threading an accumulator through it.
    %
:- pred fold_over_list_runs(pred(list(L), A, A)::in(pred(in, in, out) is det),
    list(L)::in, int::in, A::in, A::out) is det.

fold_over_list_runs(_, [], _, !Acc).
fold_over_list_runs(Pred, List @ [_ | Tail], MinLength, !Acc) :-
    fold_over_list_runs_2(Pred, List, MinLength, !Acc),
    fold_over_list_runs(Pred, Tail, MinLength, !Acc).

:- pred fold_over_list_runs_2(
    pred(list(L), A, A)::in(pred(in, in, out) is det),
    list(L)::in, int::in, A::in, A::out) is det.

fold_over_list_runs_2(Pred, List, Length, !Acc) :-
    ( list__take(Length, List, Front) ->
        Pred(Front, !Acc),
        fold_over_list_runs_2(Pred, List, Length+1, !Acc)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
%
% Transforming procedures.
%

:- pred add_transformed_procs(tupling_scheme::in, module_info::in,
    module_info::out, counter::in, counter::out, transform_map::in,
    transform_map::out) is det.

add_transformed_procs(TuplingScheme, !ModuleInfo, !Counter, !TransformMap) :-
    map__foldl3(add_transformed_proc, TuplingScheme,
        !ModuleInfo, !Counter, !TransformMap).

:- pred add_transformed_proc(pred_proc_id::in, tupling_proposal::in,
    module_info::in, module_info::out, counter::in, counter::out,
    transform_map::in, transform_map::out) is det.

add_transformed_proc(PredProcId, tupling(_, FieldVars, _),
        !ModuleInfo, !Counter, !TransformMap) :-
    PredProcId = proc(PredId, ProcId),
    some [!ProcInfo] (
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            PredInfo, !:ProcInfo),

        % Build up information about intervals and which variables
        % are needed in each interval.
        build_interval_info(!.ModuleInfo, !.ProcInfo, IntervalInfo),

        % Create the cell variable.
        list__length(FieldVars, TupleArity),
        proc_info_vartypes(!.ProcInfo, VarTypes),
        list__map(map__lookup(VarTypes), FieldVars, TupleArgTypes),
        construct_type(unqualified("{}") - TupleArity, TupleArgTypes,
            TupleConsType),
        proc_info_create_var_from_type(TupleConsType,
            yes("TuplingCellVar"), CellVar, !ProcInfo),

        % Get the argument positions of the parameters to be tupled.
        proc_info_headvars(!.ProcInfo, HeadVars),
        list__map(nth_member_lookup(HeadVars), FieldVars, ArgsToTuple),

        % Build an insertion map of where the deconstruction
        % unifications are needed.
        build_insert_map(CellVar, FieldVars, IntervalInfo, InsertMap),

        % Make a transformed version of the procedure and add it to
        % the module.
        make_transformed_proc(CellVar, FieldVars, InsertMap, !ProcInfo),
        recompute_instmap_delta_proc(yes, !ProcInfo, !ModuleInfo),
        counter__allocate(Num, !Counter),
        create_aux_pred(PredId, ProcId, PredInfo, !.ProcInfo, Num,
            AuxPredProcId, CallAux, !ModuleInfo),

        % Add an entry to the transform map for the new procedure.
        TransformedProc = transformed_proc(AuxPredProcId, TupleConsType,
            ArgsToTuple, CallAux),
        svmap__det_insert(PredProcId, TransformedProc, !TransformMap)
    ).

add_transformed_proc(_, no_tupling, !ModuleInfo, !TransformMap, !Counter).

%-----------------------------------------------------------------------------%

:- pred make_transformed_proc(prog_var::in, prog_vars::in, insert_map::in,
    proc_info::in, proc_info::out) is det.

make_transformed_proc(CellVar, FieldVarsList, InsertMap, !ProcInfo) :-
    % Modify the procedure's formal parameters.
    proc_info_headvars(!.ProcInfo, HeadVars0),
    proc_info_argmodes(!.ProcInfo, ArgModes0),
    HeadVarsAndModes = list__filter_map_corresponding(
        (func(Var, Mode) = (Var - Mode) is semidet :-
            not Var `list.member` FieldVarsList),
        HeadVars0, ArgModes0),
    assoc_list__keys_and_values(HeadVarsAndModes, HeadVars, ArgModes),
    proc_info_set_headvars(HeadVars ++ [CellVar], !ProcInfo),
    proc_info_set_argmodes(ArgModes ++ [in_mode], !ProcInfo),

    % Insert the necessary deconstruction unifications.
    proc_info_goal(!.ProcInfo, Goal0),
    proc_info_vartypes(!.ProcInfo, VarTypes0),
    proc_info_varset(!.ProcInfo, VarSet0),
    % XXX: I haven't checked if adding this feature has any effect.
    MaybeGoalFeature = yes(tuple_opt),
    record_decisions_in_goal(Goal0, Goal1, VarSet0, VarSet1,
        VarTypes0, VarTypes1, map__init, RenameMapA, InsertMap,
        MaybeGoalFeature),

    % In some cases some of the field variables need to be available at
    % the very beginning of the procedure.  The required deconstructions
    % for those variables won't show up in the insert map.  To handle this
    % we just to insert a deconstruction unification at the start of the
    % procedure and let a simplification pass remove it later if not required.
    %
    % We could make build_insert_map add such required unifications to the
    % insert map, but record_decisions_in_goal would need to be modified
    % as well.
    %
    deconstruct_tuple(CellVar, FieldVarsList, ProcStartDeconstruct),
    ProcStartInsert = insert_spec(ProcStartDeconstruct,
        set__from_list(FieldVarsList)),
    insert_proc_start_deconstruction(Goal1, Goal2,
        VarSet1, VarSet, VarTypes1, VarTypes,
        RenameMapB, ProcStartInsert),
    rename_vars_in_goal(RenameMapB, Goal2, Goal3),

    map__merge(RenameMapA, RenameMapB, RenameMap),
    apply_headvar_correction(set__from_list(HeadVars), RenameMap, Goal3, Goal),
    proc_info_set_goal(Goal, !ProcInfo),
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    requantify_proc(!ProcInfo).

:- pred insert_proc_start_deconstruction(hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    rename_map::out, insert_spec::in) is det.

insert_proc_start_deconstruction(Goal0, Goal, !VarSet, !VarTypes,
        VarRename, Insert) :-
    % The tuple_opt feature is not used for this goal as we do want
    % other transformations to remove it if possible.
    make_inserted_goal(!VarSet, !VarTypes, map__init, VarRename,
        Insert, no, InsertGoal),
    Goal0 = _ - GoalInfo,
    conj_list_to_goal([InsertGoal, Goal0], GoalInfo, Goal).

%-----------------------------------------------------------------------------%

    % This predicate makes a new version of the given procedure in a module.
    % Amongst other things the new procedure is given a new pred_id and
    % proc_id, a new name and a new goal.
    %
    % CallAux is an output variable, which is unified with a goal that
    % can be used as a template for constructing calls to the newly
    % created procedure.
    %
    % See also create_aux_pred in loop_inv.m.
    %
:- pred create_aux_pred(pred_id::in, proc_id::in, pred_info::in,
    proc_info::in, int::in, pred_proc_id::out, hlds_goal::out,
    module_info::in, module_info::out) is det.

create_aux_pred(PredId, ProcId, PredInfo, ProcInfo, Counter,
        AuxPredProcId, CallAux, ModuleInfo0, ModuleInfo) :-
    module_info_get_name(ModuleInfo0, ModuleName),

    proc_info_headvars(ProcInfo, AuxHeadVars),
    proc_info_goal(ProcInfo, Goal @ (_GoalExpr - GoalInfo)),
    proc_info_get_initial_instmap(ProcInfo, ModuleInfo0,
        InitialAuxInstMap),
    pred_info_typevarset(PredInfo, TVarSet),
    proc_info_vartypes(ProcInfo, VarTypes),
    pred_info_get_class_context(PredInfo, ClassContext),
    proc_info_rtti_varmaps(ProcInfo, RttiVarMaps),
    proc_info_varset(ProcInfo, VarSet),
    proc_info_inst_varset(ProcInfo, InstVarSet),
    pred_info_get_markers(PredInfo, Markers),
    pred_info_get_aditi_owner(PredInfo, Owner),
    pred_info_get_origin(PredInfo, OrigOrigin),

    PredName = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    goal_info_get_context(GoalInfo, Context),
    term__context_line(Context, Line),
    proc_id_to_int(ProcId, ProcNo),
    AuxNamePrefix = string__format("tupling_%d", [i(ProcNo)]),
    make_pred_name_with_context(ModuleName, AuxNamePrefix,
        PredOrFunc, PredName, Line, Counter, AuxPredSymName),
    (
        AuxPredSymName = unqualified(AuxPredName)
    ;
        AuxPredSymName = qualified(_ModuleSpecifier, AuxPredName)
    ),

    Origin = transformed(tuple(ProcNo), OrigOrigin, PredId),
    hlds_pred__define_new_pred(
        Origin,                 % in
        Goal,                   % in
        CallAux,                % out
        AuxHeadVars,            % in
        _ExtraArgs,             % out
        InitialAuxInstMap,      % in
        AuxPredName,            % in
        TVarSet,                % in
        VarTypes,               % in
        ClassContext,           % in
        RttiVarMaps,            % in
        VarSet,                 % in
        InstVarSet,             % in
        Markers,                % in
        Owner,                  % in
        address_is_not_taken,   % in
        ModuleInfo0,
        ModuleInfo,
        AuxPredProcId
    ).

%-----------------------------------------------------------------------------%
%
% Counting loads and stores between the stack and registers.
%

:- type count_info
    --->    count_info(
                count_info_pred_proc_id :: pred_proc_id,
                                        % Which procedure is being counted.
                count_info_proc         :: proc_info,
                count_info_module       :: module_info,
                count_info_proc_counts  :: proc_trace_counts,
                count_info_params       :: tuning_params,
                tupling_scheme          :: tupling_scheme
            ).

:- type tuning_params
    --->    tuning_params(
                normal_var_load_cost    :: int,
                normal_var_store_cost   :: int,
                cell_var_load_cost      :: int,
                cell_var_store_cost     :: int,
                field_var_load_cost     :: int,
                field_var_store_cost    :: int,
                costs_ratio             :: int,
                min_args_to_tuple       :: int
            ).

:- type count_state
    --->    count_state(
                reg_vars                :: set(prog_var),
                stack_vars              :: set(prog_var),
                load_costs              :: float,
                store_costs             :: float
            ).

:- type costs
    --->    costs(
                avg_loads               :: float,
                avg_stores              :: float
            ).

:- func get_tupling_proposal(count_info, pred_proc_id) = tupling_proposal
    is det.

get_tupling_proposal(CountInfo, PredProcId) = TuplingProposal :-
    ( map__search(CountInfo ^ tupling_scheme, PredProcId, Probe) ->
        TuplingProposal = Probe
    ;
        TuplingProposal = no_tupling
    ).

:- func get_own_tupling_proposal(count_info) = tupling_proposal is det.

get_own_tupling_proposal(CountInfo) =
    get_tupling_proposal(CountInfo, CountInfo ^ count_info_pred_proc_id).

%-----------------------------------------------------------------------------%

    % Collect all the information for a procedure that is required for
    % the count_load_stores_in_proc predicate to work.
    %
:- pred prepare_proc_for_counting(pred_proc_id::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

prepare_proc_for_counting(PredProcId, !ModuleInfo, !IO) :-
    PredProcId = proc(PredId, ProcId),
    some [!ProcInfo] (
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            PredInfo, !:ProcInfo),
        pred_info_arg_types(PredInfo, ArgTypes),
        generate_proc_arg_info(ArgTypes, !.ModuleInfo, !ProcInfo),

        detect_liveness_proc(PredId, ProcId, !.ModuleInfo, !ProcInfo, !IO),
        initial_liveness(!.ProcInfo, PredId, !.ModuleInfo, Liveness0),
        module_info_get_globals(!.ModuleInfo, Globals),
        body_should_use_typeinfo_liveness(PredInfo, Globals,
            TypeInfoLiveness),
        globals__lookup_bool_option(Globals,
            opt_no_return_calls, OptNoReturnCalls),
        AllocData = alloc_data(!.ModuleInfo, !.ProcInfo,
            TypeInfoLiveness, OptNoReturnCalls),
        goal_path__fill_slots(!.ModuleInfo, !ProcInfo),
        proc_info_goal(!.ProcInfo, Goal0),
        OptTupleAlloc0 = opt_tuple_alloc,
        set__init(FailVars),
        set__init(NondetLiveness0),
        build_live_sets_in_goal(Goal0, Goal, FailVars, AllocData,
            OptTupleAlloc0, _OptTupleAlloc, Liveness0, _Liveness,
            NondetLiveness0, _NondetLiveness),
        proc_info_set_goal(Goal, !ProcInfo),

        module_info_set_pred_proc_info(PredId, ProcId,
            PredInfo, !.ProcInfo, !ModuleInfo)
    ).

%-----------------------------------------------------------------------------%

% The opt_tuple_alloc structure is constructed by live_vars.m.  As far as I can
% tell we don't need such a thing for this module so we just define some stubs.

:- type opt_tuple_alloc
    --->    opt_tuple_alloc.

:- instance stack_alloc_info(opt_tuple_alloc) where [
    pred(at_call_site/4) is opt_at_call_site,
    pred(at_resume_site/4) is opt_at_resume_site,
    pred(at_par_conj/4) is opt_at_par_conj
].

:- pred opt_at_call_site(need_across_call::in, hlds_goal_info::in,
    opt_tuple_alloc::in, opt_tuple_alloc::out) is det.

opt_at_call_site(_NeedAtCall, _GoalInfo, StackAlloc, StackAlloc).

:- pred opt_at_resume_site(need_in_resume::in, hlds_goal_info::in,
    opt_tuple_alloc::in, opt_tuple_alloc::out) is det.

opt_at_resume_site(_NeedAtResume, _GoalInfo, StackAlloc, StackAlloc).

:- pred opt_at_par_conj(need_in_par_conj::in, hlds_goal_info::in,
    opt_tuple_alloc::in, opt_tuple_alloc::out) is det.

opt_at_par_conj(_NeedParConj, _GoalInfo, StackAlloc, StackAlloc).

%-----------------------------------------------------------------------------%

:- pred count_load_stores_for_scc(trace_counts::in, tuning_params::in,
    module_info::in, tupling_scheme::in, list(pred_proc_id)::in, costs::out)
    is det.

count_load_stores_for_scc(TraceCounts, TuningParams, ModuleInfo,
        TuplingScheme, PredProcIds, costs(Loads, Stores)) :-
    list__foldl2(count_load_stores_for_scc_2(TraceCounts,
            TuningParams, ModuleInfo, TuplingScheme),
        PredProcIds, 0.0, Loads, 0.0, Stores).

:- pred count_load_stores_for_scc_2(trace_counts::in, tuning_params::in,
    module_info::in, tupling_scheme::in, pred_proc_id::in,
    float::in, float::out, float::in, float::out) is det.

count_load_stores_for_scc_2(TraceCounts, TuningParams, ModuleInfo,
        TuplingScheme, PredProcId, !Loads, !Stores) :-
    PredProcId = proc(PredId, ProcId),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
        PredInfo, ProcInfo),
    % XXX: Different declaring vs defining modules not handled.
    ProcLabel = proc(pred_info_module(PredInfo),
            pred_info_is_pred_or_func(PredInfo),
            pred_info_module(PredInfo),
            pred_info_name(PredInfo),
            pred_info_orig_arity(PredInfo),
            proc_id_to_int(ProcId)),
    pred_info_context(PredInfo, Context),
    Context = context(FileName, _),
    ProcLabelAndFile = proc_label_and_filename(ProcLabel, FileName),
    ( get_proc_counts(TraceCounts, ProcLabelAndFile, yes(ProcCounts)) ->
        count_load_stores_in_proc(count_info(PredProcId, ProcInfo,
            ModuleInfo, ProcCounts, TuningParams, TuplingScheme),
            ProcLoads, ProcStores),
        % XXX: There is a problem somewhere causing CALL and EXIT
        % events not to show up for some procedures in trace count files.
        % The weighting of the procedure's costs is disabled.
        % However, if working, it still wouldn't be ideal as we don't
        % know how many of the calls to the procedure came from within
        % or without the SCC.
        % get_proc_calls(ProcCounts, Weight),
        Weight = 1,
        !:Loads = !.Loads + float(Weight) * ProcLoads,
        !:Stores = !.Stores + float(Weight) * ProcStores
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred count_load_stores_in_proc(count_info::in, float::out, float::out)
    is det.

count_load_stores_in_proc(CountInfo, Loads, Stores) :-
    proc(PredId, _) = CountInfo ^ count_info_pred_proc_id,
    ProcInfo = CountInfo ^ count_info_proc,
    ModuleInfo = CountInfo ^ count_info_module,
    initial_liveness(ProcInfo, PredId, ModuleInfo, InitialLiveness),
    CountState0 = count_state(InitialLiveness, set__init, 0.0, 0.0),
    proc_info_goal(ProcInfo, Goal),
    count_load_stores_in_goal(Goal, CountInfo, CountState0, CountState1),
    arg_info__partition_proc_args(ProcInfo, ModuleInfo, _, OutputArgs, _),
    cls_require_in_regs(CountInfo, set__to_sorted_list(OutputArgs),
        CountState1, CountState),
    CountState = count_state(_, _, Loads, Stores).

%-----------------------------------------------------------------------------%

    % This code is based on interval__build_interval_info_in_goal.

:- pred count_load_stores_in_goal(hlds_goal::in, count_info::in,
    count_state::in, count_state::out) is det.

count_load_stores_in_goal(Goal - GoalInfo, CountInfo, !CountState) :-
    Goal = foreign_proc(_Attributes, PredId, ProcId, Args, ExtraArgs,
        _PragmaCode),
    ModuleInfo = CountInfo ^ count_info_module,
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
        _PredInfo, ProcInfo),
    ArgVars = list__map(foreign_arg_var, Args),
    ExtraVars = list__map(foreign_arg_var, ExtraArgs),
    CallingProcInfo = CountInfo ^ count_info_proc,
    proc_info_vartypes(CallingProcInfo, VarTypes),
    arg_info__partition_proc_call_args(ProcInfo, VarTypes,
        ModuleInfo, ArgVars, InputArgVarSet, OutputArgVarSet, _),
    set__to_sorted_list(InputArgVarSet, InputArgVars),
    list__append(InputArgVars, ExtraVars, InputVars),
    (
        goal_info_maybe_get_maybe_need_across_call(GoalInfo,
            MaybeNeedAcrossCall),
        MaybeNeedAcrossCall = yes(_)
    ->
        count_load_stores_for_call(CountInfo, InputVars, OutputArgVarSet,
            MaybeNeedAcrossCall, GoalInfo, !CountState)
    ;
        cls_require_in_regs(CountInfo, InputVars, !CountState),
        cls_clobber_regs(OutputArgVarSet, !CountState)
    ).

count_load_stores_in_goal(Goal - GoalInfo, CountInfo, !CountState) :-
    Goal = generic_call(GenericCall, ArgVars, ArgModes, _Detism),
    ProcInfo = CountInfo ^ count_info_proc,
    ModuleInfo = CountInfo ^ count_info_module,
    goal_info_get_maybe_need_across_call(GoalInfo, MaybeNeedAcrossCall),
    proc_info_vartypes(ProcInfo, VarTypes),
    list__map(map__lookup(VarTypes), ArgVars, ArgTypes),
    arg_info__compute_in_and_out_vars(ModuleInfo, ArgVars,
        ArgModes, ArgTypes, InputArgs, OutputArgs),

    % Casts are generated inline.
    ( GenericCall = cast(_) ->
        cls_require_in_regs(CountInfo, InputArgs, !CountState),
        cls_put_in_regs(OutputArgs, !CountState)
    ;
        module_info_get_globals(ModuleInfo, Globals),
        call_gen__generic_call_info(Globals, GenericCall,
            length(InputArgs), _, GenericVarsArgInfos, _, _),
        assoc_list__keys(GenericVarsArgInfos, GenericVars),
        list__append(GenericVars, InputArgs, Inputs),
        set__list_to_set(OutputArgs, Outputs),
        count_load_stores_for_call(CountInfo, Inputs, Outputs,
            MaybeNeedAcrossCall, GoalInfo, !CountState)
    ).

count_load_stores_in_goal(Goal - GoalInfo, CountInfo, !CountState) :-
    Goal = call(PredId, ProcId, _, Builtin, _, _),
    (
        Builtin = not_builtin,
        TuplingProposal = get_tupling_proposal(CountInfo,
            proc(PredId, ProcId)),
        TuplingProposal = tupling(_, _, _)
    ->
        count_load_stores_in_call_to_tupled(Goal - GoalInfo,
            CountInfo, TuplingProposal, !CountState)
    ;
        count_load_stores_in_call_to_not_tupled(Goal - GoalInfo,
            CountInfo, !CountState)
    ).

count_load_stores_in_goal(Goal - _GoalInfo, CountInfo, !CountState) :-
    Goal = unify(_, _, _, Unification, _),
    (
        Unification = construct(CellVar, _ConsId, ArgVars, _ArgModes,
            _HowToConstruct, _, _),
        cls_require_in_regs(CountInfo, ArgVars, !CountState),
        cls_put_in_regs([CellVar], !CountState)
    ;
        Unification = deconstruct(CellVar, _ConsId, ArgVars, _ArgModes, _, _),
        cls_put_in_regs_via_deconstruct(CountInfo, CellVar, ArgVars,
            !CountState)
    ;
        Unification = assign(ToVar, FromVar),
        cls_require_in_reg(CountInfo, FromVar, !CountState),
        cls_put_in_regs([ToVar], !CountState)
    ;
        Unification = simple_test(Var1, Var2),
        cls_require_in_regs(CountInfo, [Var1, Var2], !CountState)
    ;
        Unification = complicated_unify(_, _, _),
        unexpected(this_file, "count_load_stores_in_goal: complicated_unify")
    ).

count_load_stores_in_goal(scope(_Reason, Goal) - _GoalInfo, CountInfo,
        !CountState) :-
    count_load_stores_in_goal(Goal, CountInfo, !CountState).

count_load_stores_in_goal(conj(Goals) - _GoalInfo, CountInfo, !CountState) :-
    count_load_stores_in_conj(Goals, CountInfo, !CountState).

count_load_stores_in_goal(par_conj(_) - _, _, !_) :-
    sorry(this_file, "tupling with parallel conjunctions").

count_load_stores_in_goal(disj(Goals) - _GoalInfo, CountInfo, !CountState) :-
    count_load_stores_in_disj(Goals, CountInfo, !CountState).

count_load_stores_in_goal(switch(_Var, _Det, Cases) - _GoalInfo, CountInfo,
        !CountState) :-
    count_load_stores_in_cases(Cases, CountInfo, !CountState).

count_load_stores_in_goal(not(Goal) - _GoalInfo, CountInfo, !CountState) :-
    goal_info_get_resume_point(snd(Goal), ResumePoint),
    (
        ResumePoint = resume_point(LiveVars, _ResumeLocs),
        cls_require_flushed(CountInfo, LiveVars, !CountState)
    ;
        ResumePoint = no_resume_point,
        unexpected(this_file,
            "count_load_stores_in_goal: no_resume_point for not")
    ),
    count_load_stores_in_goal(Goal, CountInfo, !CountState).

count_load_stores_in_goal(if_then_else(_, Cond, Then, Else) - _GoalInfo,
        CountInfo, !CountState) :-
    goal_info_get_resume_point(snd(Cond), ResumePoint),
    (
        ResumePoint = resume_point(LiveVars, _ResumeLocs),
        cls_require_flushed(CountInfo, LiveVars, !CountState),
        count_load_stores_in_goal(Cond, CountInfo, !CountState),

        reset_count_state_counts(!.CountState, ResetCountInfo),
        count_load_stores_in_goal(Then, CountInfo,
            ResetCountInfo, ThenCountInfo),
        count_load_stores_in_goal(Else, CountInfo,
            ResetCountInfo, ElseCountInfo),

        ProcCounts = CountInfo ^ count_info_proc_counts,
        goal_info_get_goal_path(snd(Then), ThenGoalPath),
        goal_info_get_goal_path(snd(Else), ElseGoalPath),
        get_ite_relative_frequencies(ProcCounts,
            ThenGoalPath, ElseGoalPath,
            ThenRelFreq, ElseRelFreq),

        add_branch_costs(ThenCountInfo, ThenRelFreq, !CountState),
        add_branch_costs(ElseCountInfo, ElseRelFreq, !CountState)
    ;
        ResumePoint = no_resume_point,
        unexpected(this_file,
            "count_load_stores_in_goal: no_resume_point for if_then_else")
    ).

count_load_stores_in_goal(shorthand(_) - _, _, !_) :-
    unexpected(this_file,
        "count_load_stores_in_goal: unexpected shorthand").

%-----------------------------------------------------------------------------%

:- inst call_goal_expr
    ==  bound(call(ground, ground, ground, ground, ground, ground)).
:- mode in_call_goal
    ==  in(pair(call_goal_expr, ground)).

:- pred count_load_stores_in_call_to_tupled(hlds_goal::in_call_goal,
    count_info::in,
    tupling_proposal::in(bound(tupling(ground, ground, ground))),
    count_state::in, count_state::out) is det.

count_load_stores_in_call_to_tupled(Goal - GoalInfo, CountInfo,
        CalleeTuplingProposal, !CountState) :-
    Goal = call(CalleePredId, CalleeProcId, ArgVars, _, _, _),
    CalleeTuplingProposal = tupling(CellVar, FieldVars, FieldVarArgPos),
    ModuleInfo = CountInfo ^ count_info_module,
    module_info_pred_proc_info(ModuleInfo, CalleePredId, CalleeProcId,
        _, CalleeProcInfo),
    CallingProcInfo = CountInfo ^ count_info_proc,
    proc_info_vartypes(CallingProcInfo, VarTypes),
    arg_info__partition_proc_call_args(CalleeProcInfo, VarTypes,
        ModuleInfo, ArgVars, InputArgs0, Outputs, _),
    (
        % If the caller is a tupled procedure, and every field variable
        % of the tuple appears as an input argument to the callee AND
        % every such argument is in a position matching the field variable's
        % position in the tupling proposal, then the cell var of the caller
        % can be reused as the call var for the callee.
        %
        % TODO: If we kept track of the aliases of field variables,
        % then they could be checked also.
        get_own_tupling_proposal(CountInfo) = tupling(_, _, _),
        all [Var] Var `list.member` FieldVars => (
            Var `set.member` InputArgs0,
            assoc_list__search(FieldVarArgPos, Var, Pos),
            list__nth_member_search(ArgVars, Var, Pos)
        )
    ->
        % In this case, the cell var is not being used to access field
        % variables, so it should not incur the cell var cost.
        cls_require_normal_var_in_reg(CountInfo, CellVar, !CountState),
        set__delete_list(InputArgs0, FieldVars, InputArgs)
    ;
        % The cell var cannot be used for the callee, so we must add
        % the cost of constructing a new tuple.
        TuplingParams = CountInfo ^ count_info_params,
        CellVarStoreCost = float(TuplingParams ^ cell_var_store_cost),
        !:CountState = (!.CountState ^ store_costs :=
            (!.CountState ^ store_costs + CellVarStoreCost)),
        InputArgs = InputArgs0
    ),
    set__to_sorted_list(InputArgs, Inputs),
    goal_info_get_maybe_need_across_call(GoalInfo, MaybeNeedAcrossCall),
    count_load_stores_for_call(CountInfo, Inputs, Outputs,
        MaybeNeedAcrossCall, GoalInfo, !CountState).

:- pred count_load_stores_in_call_to_not_tupled(hlds_goal::in_call_goal,
    count_info::in, count_state::in, count_state::out) is det.

count_load_stores_in_call_to_not_tupled(Goal - GoalInfo, CountInfo,
        !CountState) :-
    Goal = call(PredId, ProcId, ArgVars, Builtin, _, _),
    ModuleInfo = CountInfo ^ count_info_module,
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
        _PredInfo, CalleeProcInfo),
    ProcInfo = CountInfo ^ count_info_proc,
    proc_info_vartypes(ProcInfo, VarTypes),
    arg_info__partition_proc_call_args(CalleeProcInfo, VarTypes,
        ModuleInfo, ArgVars, InputArgs, Outputs, _),
    set__to_sorted_list(InputArgs, Inputs),
    ( Builtin = inline_builtin ->
        cls_require_in_regs(CountInfo, Inputs, !CountState),
        cls_put_in_regs(set__to_sorted_list(Outputs), !CountState)
    ;
        goal_info_get_maybe_need_across_call(GoalInfo,
            MaybeNeedAcrossCall),
        count_load_stores_for_call(CountInfo, Inputs, Outputs,
            MaybeNeedAcrossCall, GoalInfo, !CountState)
    ).

:- pred count_load_stores_for_call(count_info::in, prog_vars::in,
    set(prog_var)::in, maybe(need_across_call)::in,
    hlds_goal_info::in, count_state::in, count_state::out) is det.

count_load_stores_for_call(CountInfo, Inputs, Outputs, MaybeNeedAcrossCall,
        _GoalInfo, !CountState) :-
    cls_require_in_regs(CountInfo, Inputs, !CountState),
    (
        MaybeNeedAcrossCall = yes(NeedAcrossCall),
        NeedAcrossCall = need_across_call(ForwardVars,
            ResumeVars, NondetLiveVars),
        AllVars = set__union_list([ForwardVars, ResumeVars, NondetLiveVars]),
        cls_require_flushed(CountInfo, AllVars, !CountState),
        cls_clobber_regs(Outputs, !CountState)
    ;
        MaybeNeedAcrossCall = no,
        unexpected(this_file,
            "count_load_stores_for_call: no need across call")
    ).

%-----------------------------------------------------------------------------%

:- pred count_load_stores_in_conj(hlds_goals::in, count_info::in,
    count_state::in, count_state::out) is det.

count_load_stores_in_conj([], _CountInfo, !CountState).
count_load_stores_in_conj([Goal | Goals], CountInfo, !CountState) :-
    count_load_stores_in_goal(Goal, CountInfo, !CountState),
    count_load_stores_in_conj(Goals, CountInfo, !CountState).

:- pred count_load_stores_in_disj(hlds_goals::in, count_info::in,
    count_state::in, count_state::out) is det.

count_load_stores_in_disj([], _CountInfo, !CountState).
count_load_stores_in_disj([Goal | Goals], CountInfo, !CountState) :-
    GoalInfo = snd(Goal),
    goal_info_get_resume_point(GoalInfo, ResumePoint),
    (
        ResumePoint = resume_point(LiveVars, _ResumeLocs),
        cls_require_flushed(CountInfo, LiveVars, !CountState)
    ;
        ResumePoint = no_resume_point
    ),
    reset_count_state_counts(!.CountState, BranchCountState0),
    count_load_stores_in_goal(Goal, CountInfo,
        BranchCountState0, BranchCountState),
    ProcCounts = CountInfo ^ count_info_proc_counts,
    goal_info_get_goal_path(GoalInfo, GoalPath),
    get_disjunct_relative_frequency(ProcCounts, GoalPath, RelFreq),
    add_branch_costs(BranchCountState, RelFreq, !CountState),
    count_load_stores_in_disj(Goals, CountInfo, !CountState).

:- pred count_load_stores_in_cases(list(case)::in, count_info::in,
    count_state::in, count_state::out) is det.

count_load_stores_in_cases([], _CountInfo, !CountState).
count_load_stores_in_cases([Case | Cases], CountInfo, !CountState) :-
    Case = case(_ConsId, Goal),
    GoalInfo = snd(Goal),
    goal_info_get_resume_point(GoalInfo, ResumePoint),
    (
        ResumePoint = resume_point(LiveVars, _ResumeLocs),
        cls_require_flushed(CountInfo, LiveVars, !CountState)
    ;
        ResumePoint = no_resume_point
    ),
    reset_count_state_counts(!.CountState, BranchCountState0),
    count_load_stores_in_goal(Goal, CountInfo, BranchCountState0,
        BranchCountState),
    ProcCounts = CountInfo ^ count_info_proc_counts,
    goal_info_get_goal_path(GoalInfo, GoalPath),
    get_case_relative_frequency(ProcCounts, GoalPath, RelFreq),
    add_branch_costs(BranchCountState, RelFreq, !CountState),
    count_load_stores_in_cases(Cases, CountInfo, !CountState).

%-----------------------------------------------------------------------------%

    % Make the values of the given variables available in registers.
    %
:- pred cls_require_in_regs(count_info::in, prog_vars::in, count_state::in,
    count_state::out) is det.

cls_require_in_regs(CountInfo, Vars, !CountState) :-
    list__foldl(cls_require_in_reg(CountInfo), Vars, !CountState).

:- pred cls_require_in_reg(count_info::in, prog_var::in, count_state::in,
    count_state::out) is det.

cls_require_in_reg(CountInfo, Var, !CountState) :-
    (
        TuplingProposal = get_own_tupling_proposal(CountInfo),
        TuplingProposal = tupling(_, FieldVars, _),
        Var `list.member` FieldVars
    ->
        cls_require_field_var_in_reg(CountInfo, TuplingProposal,
            Var, !CountState)
    ;
        cls_require_normal_var_in_reg(CountInfo, Var, !CountState)
    ).

:- pred cls_require_normal_var_in_reg(count_info::in, prog_var::in,
    count_state::in, count_state::out) is det.

cls_require_normal_var_in_reg(CountInfo, Var, !CountState) :-
    TuningParams = CountInfo ^ count_info_params,
    NormalLoadCost = TuningParams ^ normal_var_load_cost,
    cls_require_var_in_reg_with_cost(NormalLoadCost, Var, !CountState).

:- pred cls_require_field_var_in_reg(count_info::in,
    tupling_proposal::in(bound(tupling(ground, ground, ground))),
    prog_var::in, count_state::in, count_state::out) is det.

cls_require_field_var_in_reg(CountInfo, TuplingProposal, FieldVar,
        CountState0, CountState) :-
    CountState0 = count_state(RegVars0, StackVars, Loads0, Stores),
    ( FieldVar `set.member` RegVars0 ->
        CountState = CountState0
    ;
        TuplingProposal = tupling(CellVar, _, _),
        TuningParams = CountInfo ^ count_info_params,
        CvLoadCost = float(TuningParams ^ cell_var_load_cost),
        FvLoadCost = float(TuningParams ^ field_var_load_cost),
        ( CellVar `set.member` RegVars0 ->
            RegVars = RegVars0 `insert` FieldVar,
            Loads = Loads0 + FvLoadCost
        ;
            RegVars = RegVars0 `insert_list` [CellVar, FieldVar],
            Loads = Loads0 + CvLoadCost + FvLoadCost
        ),
        CountState = count_state(RegVars, StackVars, Loads, Stores)
    ).

:- pred cls_require_var_in_reg_with_cost(int::in, prog_var::in,
    count_state::in, count_state::out) is det.

cls_require_var_in_reg_with_cost(LoadCost, Var, CountState0, CountState) :-
    CountState0 = count_state(RegVars0, StackVars, Loads0, Stores),
    ( Var `set.member` RegVars0 ->
        CountState = CountState0
    ;
        RegVars = RegVars0 `insert` Var,
        Loads = Loads0 + float(LoadCost),
        CountState = count_state(RegVars, StackVars, Loads, Stores)
    ).

    % Put the values of the given variables into registers.
    %
:- pred cls_put_in_regs(prog_vars::in, count_state::in, count_state::out)
    is det.

cls_put_in_regs(Vars, State0, State) :-
    RegVars0 = (State0 ^ reg_vars),
    State = (State0 ^ reg_vars := RegVars0 `insert_list` Vars).

:- pred cls_put_in_regs_via_deconstruct(count_info::in, prog_var::in,
    prog_vars::in, count_state::in, count_state::out) is det.

cls_put_in_regs_via_deconstruct(CountInfo,
        DeconstructCellVar, DeconstructFieldVars, !State) :-
    TuningParams = CountInfo ^ count_info_params,
    CvLoadCost = TuningParams ^ cell_var_load_cost,
    FvLoadCost = TuningParams ^ field_var_load_cost,
    TuplingProposal = get_own_tupling_proposal(CountInfo),
    (
        TuplingProposal = no_tupling,
        cls_require_var_in_reg_with_cost(CvLoadCost,
            DeconstructCellVar, !State),
        list__foldl(cls_require_var_in_reg_with_cost(FvLoadCost),
            DeconstructFieldVars, !State)
    ;
        TuplingProposal = tupling(_, TupleFieldVars, _),
        VarsToLoad = set__difference(
            set__from_list(DeconstructFieldVars),
            set__from_list(TupleFieldVars)),
        ( set__non_empty(VarsToLoad) ->
            cls_require_var_in_reg_with_cost(CvLoadCost, DeconstructCellVar,
                !State),
            set__fold(cls_require_var_in_reg_with_cost(FvLoadCost), VarsToLoad,
                !State)
        ;
            % All the variables generated by this deconstruction can be
            % obtained from the proposed tupling, so the deconstruction
            % can be ignored. The costs of loading those variables from
            % the tuple will be counted as they come.
            true
        )
    ).

%-----------------------------------------------------------------------------%

    % Copy the given variables to the stack, if they have not been copied
    % previously.
    %
:- pred cls_require_flushed(count_info::in, set(prog_var)::in,
    count_state::in, count_state::out) is det.

cls_require_flushed(CountInfo, Vars, !CountState) :-
    TuplingProposal = get_own_tupling_proposal(CountInfo),
    TuningParams = CountInfo ^ count_info_params,
    set__fold(cls_require_flushed_2(TuplingProposal, TuningParams),
        Vars, !CountState).

:- pred cls_require_flushed_2(tupling_proposal::in, tuning_params::in,
    prog_var::in, count_state::in, count_state::out) is det.

cls_require_flushed_2(no_tupling, TuningParams, Var, !CountState) :-
    StoreCost = TuningParams ^ normal_var_store_cost,
    cls_require_flushed_with_cost(StoreCost, Var, !CountState).

cls_require_flushed_2(tupling(CellVar, FieldVars, _), TuningParams, Var,
        !CountState) :-
    ( list.member(Var, FieldVars) ->
        FvStoreCost = TuningParams ^ field_var_store_cost,
        cls_require_flushed_with_cost(FvStoreCost, CellVar, !CountState)
    ;
        StoreCost = TuningParams ^ normal_var_store_cost,
        cls_require_flushed_with_cost(StoreCost, Var, !CountState)
    ).

:- pred cls_require_flushed_with_cost(int::in, prog_var::in, count_state::in,
    count_state::out) is det.

cls_require_flushed_with_cost(StoreCost, Var,
        count_state(RegVars, StackVars0, Loads, Stores0),
        count_state(RegVars, StackVars, Loads, Stores)) :-
    ( set.member(Var, StackVars0) ->
        StackVars = StackVars0,
        Stores = Stores0
    ;
        StackVars = StackVars0 `insert` Var,
        Stores = Stores0 + float(StoreCost)
    ).

%-----------------------------------------------------------------------------%

    % Clear out the contents of the registers and replace them with the
    % values of the given variables.
    %
:- pred cls_clobber_regs(set(prog_var)::in, count_state::in, count_state::out)
    is det.

cls_clobber_regs(NewVars, CountState0, CountState0 ^ reg_vars := NewVars).

%-----------------------------------------------------------------------------%

:- pred reset_count_state_counts(count_state::in, count_state::out) is det.

reset_count_state_counts(!CountState) :-
    !:CountState = !.CountState ^ load_costs := 0.0,
    !:CountState = !.CountState ^ store_costs := 0.0.

:- pred add_branch_costs(count_state::in, float::in,
    count_state::in, count_state::out) is det.

add_branch_costs(BranchState, Weight, !CountState) :-
    BranchState = count_state(_, _, BranchLoads, BranchStores),
    !.CountState = count_state(_, _, Loads0, Stores0),
    !:CountState = !.CountState ^ load_costs
        := Loads0 + Weight * BranchLoads,
    !:CountState = !.CountState ^ store_costs
        := Stores0 + Weight * BranchStores.

%-----------------------------------------------------------------------------%
%
% Building information about intervals and insert maps.
%

:- pred build_interval_info(module_info::in, proc_info::in, interval_info::out)
    is det.

build_interval_info(ModuleInfo, ProcInfo, IntervalInfo) :-
    proc_info_goal(ProcInfo, Goal),
    proc_info_vartypes(ProcInfo, VarTypes),
    arg_info__partition_proc_args(ProcInfo, ModuleInfo,
        _InputArgs, OutputArgs, _UnusedArgs),
    Counter0 = counter__init(1),
    counter__allocate(CurInterval, Counter0, Counter),
    CurIntervalId = interval_id(CurInterval),
    EndMap = map__det_insert(map__init, CurIntervalId, proc_end),
    StartMap = map__init,
    SuccMap = map__det_insert(map__init, CurIntervalId, []),
    VarsMap = map__det_insert(map__init, CurIntervalId, OutputArgs),
    IntParams = interval_params(ModuleInfo, VarTypes, no),
    IntervalInfo0 = interval_info(IntParams, set__init,
        OutputArgs, map__init, map__init, map__init,
        CurIntervalId, Counter,
        set__make_singleton_set(CurIntervalId),
        map__init, set__init, StartMap, EndMap,
        SuccMap, VarsMap, map__init),
    build_interval_info_in_goal(Goal, IntervalInfo0, IntervalInfo, unit, _).

    % This is needed only to satisfy the interface of interval.m
    %
:- instance build_interval_info_acc(unit) where [
    pred(use_cell/8) is tupling__use_cell
].

:- pred use_cell(prog_var::in, list(prog_var)::in, cons_id::in, hlds_goal::in,
    interval_info::in, interval_info::out, unit::in, unit::out) is det.

use_cell(_CellVar, _FieldVarList, _ConsId, _Goal, !IntervalInfo, !Unit).

%-----------------------------------------------------------------------------%

    % This predicate uses the interval information built previously to
    % build an insertion map, i.e. a mapping from a left anchor to a
    % deconstruction unification that is to be inserted _after_ the
    % interval beginning with that left anchor.
    %
:- pred build_insert_map(prog_var::in, prog_vars::in, interval_info::in,
    insert_map::out) is det.

build_insert_map(CellVar, FieldVars, IntervalInfo, InsertMap) :-
    FieldVarsSet = set__from_list(FieldVars),
    map__foldl(build_insert_map_2(CellVar, FieldVars, FieldVarsSet),
        IntervalInfo ^ anchor_follow_map, map__init, InsertMap).

:- pred build_insert_map_2(prog_var::in, list(prog_var)::in, set(prog_var)::in,
    anchor::in, anchor_follow_info::in, insert_map::in, insert_map::out)
    is det.

build_insert_map_2(CellVar, FieldVars, FieldVarsSet, Anchor, FollowVars - _,
        !InsertMap) :-
    NeededFieldVars = FieldVarsSet `set__intersect` FollowVars,
    ( set__empty(NeededFieldVars) ->
        true
    ;
        deconstruct_tuple(CellVar, FieldVars, Goal),
        InsertSpec = insert_spec(Goal, NeededFieldVars),
        add_insert_spec(Anchor, InsertSpec, !InsertMap)
    ).

:- pred add_insert_spec(anchor::in, insert_spec::in, insert_map::in,
    insert_map::out) is det.

add_insert_spec(Anchor, InsertSpec, !InsertMap) :-
    ( map__search(!.InsertMap, Anchor, InsertSpecs0) ->
        combine_inserts(InsertSpec, InsertSpecs0, InsertSpecs),
        svmap__det_update(Anchor, InsertSpecs, !InsertMap)
    ;
        svmap__det_insert(Anchor, [InsertSpec], !InsertMap)
    ).

:- pred combine_inserts(insert_spec::in, list(insert_spec)::in,
    list(insert_spec)::out) is det.

combine_inserts(A, [], [A]).
combine_inserts(A, [B | Bs], [C | Cs]) :-
    (
        A = insert_spec(Goal, ASet),
        B = insert_spec(Goal, BSet)
    ->
        C = insert_spec(Goal, ASet `set__union` BSet),
        Cs = Bs
    ;
        C = B,
        combine_inserts(A, Bs, Cs)
    ).

%-----------------------------------------------------------------------------%
%
% Fixing calls to transformed procedures.
%

    % The transform_map structure records which procedures were
    % transformed into what procedures.
    %
:- type transform_map == map(pred_proc_id, transformed_proc).

:- type transformed_proc
    --->    transformed_proc(
                % The pred_proc_id of the transformed version of the procedure.
                transformed_pred_proc_id    :: pred_proc_id,

                % The type of the cell variable created by the transformation.
                % This will be a tuple type.
                tuple_cons_type         :: mer_type,

                % The argument positions of the original procedure
                % which were tupled.
                args_to_tuple           :: list(int),

                % A template for a call goal that is used to update calls
                % of the original procedure to the transformed procedure
                % instead. The arguments of the template need to be replaced
                % by the actual arguments.
                call_template           :: hlds_goal
            ).

:- pred fix_calls_in_procs(transform_map::in, list(pred_proc_id)::in,
    module_info::in, module_info::out) is det.

fix_calls_in_procs(TransformMap, PredProcIds, !ModuleInfo) :-
    list__foldl(fix_calls_in_proc(TransformMap), PredProcIds, !ModuleInfo).

:- pred fix_calls_in_transformed_procs(transform_map::in,
    module_info::in, module_info::out) is det.

fix_calls_in_transformed_procs(TransformMap, !ModuleInfo) :-
    map__foldl(fix_calls_in_transformed_procs_2(TransformMap), TransformMap,
        !ModuleInfo).

:- pred fix_calls_in_transformed_procs_2(transform_map::in, pred_proc_id::in,
    transformed_proc::in, module_info::in, module_info::out) is det.

fix_calls_in_transformed_procs_2(TransformMap,
        _, transformed_proc(PredProcId, _, _, _), !ModuleInfo) :-
    fix_calls_in_proc(TransformMap, PredProcId, !ModuleInfo).

:- pred fix_calls_in_proc(transform_map::in, pred_proc_id::in,
    module_info::in, module_info::out) is det.

fix_calls_in_proc(TransformMap, proc(PredId, ProcId), !ModuleInfo) :-
    some [!ProcInfo] (
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            PredInfo, !:ProcInfo),
        % XXX: Don't modify predicates that were created by type
        % specialisation. This is a last-minute workaround for some
        % linking problems that occurred when such predicates in the
        % library were made to call tupled procedures.
        pred_info_get_origin(PredInfo, Origin),
        ( Origin = transformed(type_specialization(_), _, _) ->
            true
        ;
            proc_info_goal(!.ProcInfo, Goal0),
            proc_info_vartypes(!.ProcInfo, VarTypes0),
            proc_info_varset(!.ProcInfo, VarSet0),
            fix_calls_in_goal(Goal0, Goal, VarSet0, VarSet,
                VarTypes0, VarTypes, TransformMap),
            proc_info_set_goal(Goal, !ProcInfo),
            proc_info_set_varset(VarSet, !ProcInfo),
            proc_info_set_vartypes(VarTypes, !ProcInfo),
            requantify_proc(!ProcInfo),
            recompute_instmap_delta_proc(yes, !ProcInfo, !ModuleInfo),
            module_info_set_pred_proc_info(PredId, ProcId,
                PredInfo, !.ProcInfo, !ModuleInfo)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred fix_calls_in_goal(hlds_goal::in, hlds_goal::out, prog_varset::in,
    prog_varset::out, vartypes::in, vartypes::out, transform_map::in)
    is det.

fix_calls_in_goal(Goal - GoalInfo, Goal - GoalInfo, !_, !_, _TransformMap) :-
    Goal = foreign_proc(_, _, _, _, _, _).

fix_calls_in_goal(Goal - GoalInfo, Goal - GoalInfo, !_, !_, _TransformMap) :-
    Goal = generic_call(_, _, _, _).

fix_calls_in_goal(Goal0 - GoalInfo0, Goal, !VarSet, !VarTypes, TransformMap) :-
    Goal0 = call(CalledPredId0, CalledProcId0, Args0, Builtin,
        _Context, _SymName),
    (
        Builtin = not_builtin,
        map__search(TransformMap, proc(CalledPredId0, CalledProcId0),
            TransformedProc),
        TransformedProc = transformed_proc(_, TupleConsType, ArgsToTuple,
            CallAux0 - CallAuxInfo)
    ->
        svvarset__new_named_var("TuplingCellVarForCall", CellVar, !VarSet),
        svmap__det_insert(CellVar, TupleConsType, !VarTypes),
        extract_tupled_args_from_list(Args0, ArgsToTuple,
            TupledArgs, UntupledArgs),
        construct_tuple(CellVar, TupledArgs, ConstructGoal),
        (
            NewArgs = UntupledArgs ++ [CellVar],
            CallAux = CallAux0 ^ call_args := NewArgs
        ->
            CallGoal = CallAux - CallAuxInfo
        ;
            unexpected(this_file, "fix_calls_in_goal: not a call template")
        ),
        conj_list_to_goal([ConstructGoal, CallGoal], GoalInfo0, Goal1),
        RequantifyVars = set__from_list([CellVar | Args0]),
        implicitly_quantify_goal(RequantifyVars, _, Goal1, Goal,
            !VarSet, !VarTypes)
    ;
        Goal = Goal0 - GoalInfo0
    ).

fix_calls_in_goal(Goal - GoalInfo, Goal - GoalInfo, !_, !_, _TransformMap) :-
    Goal = unify(_, _, _, _, _).

fix_calls_in_goal(not(Goal0) - GoalInfo, not(Goal) - GoalInfo,
        !VarSet, !VarTypes, TransformMap) :-
    fix_calls_in_goal(Goal0, Goal, !VarSet, !VarTypes, TransformMap).

fix_calls_in_goal(scope(Reason, Goal0) - GoalInfo,
        scope(Reason, Goal) - GoalInfo,
        !VarSet, !VarTypes, TransformMap) :-
    fix_calls_in_goal(Goal0, Goal, !VarSet, !VarTypes, TransformMap).

fix_calls_in_goal(conj(Goals0) - GoalInfo, conj(Goals) - GoalInfo,
        !VarSet, !VarTypes, TransformMap) :-
    fix_calls_in_conj(Goals0, Goals, !VarSet, !VarTypes, TransformMap).

fix_calls_in_goal(par_conj(Goals0) - GoalInfo, par_conj(Goals) - GoalInfo,
        !VarSet, !VarTypes, TransformMap) :-
    % XXX: I am not sure whether parallel conjunctions should be treated
    % with fix_calls_in_goal or fix_calls_in_goal_list.  At any rate,
    % this is untested.
    fix_calls_in_goal_list(Goals0, Goals, !VarSet, !VarTypes, TransformMap).

fix_calls_in_goal(disj(Goals0) - GoalInfo, disj(Goals) - GoalInfo,
        !VarSet, !VarTypes, TransformMap) :-
    fix_calls_in_goal_list(Goals0, Goals, !VarSet, !VarTypes, TransformMap).

fix_calls_in_goal(switch(Var, CanFail, Cases0) - GoalInfo,
        switch(Var, CanFail, Cases) - GoalInfo,
        !VarSet, !VarTypes, TransformMap) :-
    fix_calls_in_cases(Cases0, Cases, !VarSet, !VarTypes, TransformMap).

fix_calls_in_goal(if_then_else(Vars, Cond0, Then0, Else0) - GoalInfo,
        if_then_else(Vars, Cond, Then, Else) - GoalInfo,
        !VarSet, !VarTypes, TransformMap) :-
    fix_calls_in_goal(Cond0, Cond, !VarSet, !VarTypes, TransformMap),
    fix_calls_in_goal(Then0, Then, !VarSet, !VarTypes, TransformMap),
    fix_calls_in_goal(Else0, Else, !VarSet, !VarTypes, TransformMap).

fix_calls_in_goal(shorthand(_) - _, _, !VarSet, !VarTypes, _TransformMap) :-
    unexpected(this_file, "fix_calls_in_goal: unexpected shorthand").

%-----------------------------------------------------------------------------%

:- pred fix_calls_in_conj(hlds_goals::in, hlds_goals::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    transform_map::in) is det.

fix_calls_in_conj([], [], !VarSet, !VarTypes, _).
fix_calls_in_conj([Goal0 | Goals0], Goals, !VarSet, !VarTypes, TransformMap) :-
    fix_calls_in_goal(Goal0, Goal1, !VarSet, !VarTypes, TransformMap),
    fix_calls_in_conj(Goals0, Goals1, !VarSet, !VarTypes, TransformMap),
    ( Goal1 = conj(ConjGoals) - _ ->
        Goals = ConjGoals ++ Goals1
    ;
        Goals = [Goal1 | Goals1]
    ).

:- pred fix_calls_in_goal_list(hlds_goals::in, hlds_goals::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    transform_map::in) is det.

fix_calls_in_goal_list([], [], !VarSet, !VarTypes, _TransformMap).
fix_calls_in_goal_list([Goal0 | Goals0], [Goal | Goals], !VarSet, !VarTypes,
        TransformMap) :-
    fix_calls_in_goal(Goal0, Goal, !VarSet, !VarTypes, TransformMap),
    fix_calls_in_goal_list(Goals0, Goals, !VarSet, !VarTypes, TransformMap).

:- pred fix_calls_in_cases(list(case)::in, list(case)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    transform_map::in) is det.

fix_calls_in_cases([], [], !VarSet, !VarTypes, _TransformMap).
fix_calls_in_cases([Case0 | Cases0], [Case | Cases], !VarSet, !VarTypes,
        TransformMap) :-
    Case0 = case(Functor, Goal0),
    fix_calls_in_goal(Goal0, Goal, !VarSet, !VarTypes, TransformMap),
    Case = case(Functor, Goal),
    fix_calls_in_cases(Cases0, Cases, !VarSet, !VarTypes, TransformMap).

%-----------------------------------------------------------------------------%

    % extract_tupled_args_from_list(ArgList, Indices,
    %   Selected, NotSelected)
    %
    % Pick out the elements of ArgList by the indices given and put them
    % in the list Selected, in exactly the order that they are referenced
    % in Indices.  The list NotSelected is to contain all the elements
    % of ArgList which did not end up in Selected, in the order that they
    % appeared in ArgList.
    %
    % Note again that the ordering of Selected and NotSelected are
    % determined by different lists!
    %
:- pred extract_tupled_args_from_list(prog_vars::in, list(int)::in,
    prog_vars::out, prog_vars::out) is det.

extract_tupled_args_from_list(ArgList, Indices, Selected, NotSelected) :-
    list__map(list__index1_det(ArgList), Indices, Selected),
    extract_tupled_args_from_list_2(ArgList, 1, Indices, NotSelected).

:- pred extract_tupled_args_from_list_2(prog_vars::in, int::in, list(int)::in,
    prog_vars::out) is det.

extract_tupled_args_from_list_2([], _Num, _Indices, []).
extract_tupled_args_from_list_2([H | T], Num, Indices, NotSelected) :-
    ( list.member(Num, Indices) ->
        extract_tupled_args_from_list_2(T, Num+1, Indices, NotSelected)
    ;
        extract_tupled_args_from_list_2(T, Num+1, Indices, NotSelectedTail),
        NotSelected = [H | NotSelectedTail]
    ).

%-----------------------------------------------------------------------------%
%
% Trace count summaries.
%

:- type mdbcomp_goal_path_step
    == mdbcomp__program_representation__goal_path_step.
:- type mdbcomp_goal_path
    == mdbcomp__program_representation__goal_path.

:- pred get_proc_counts(trace_counts::in, proc_label_and_filename::in,
    maybe(proc_trace_counts)::out) is det.

get_proc_counts(TraceCounts, ProcLabelAndFile, MaybeProcCounts) :-
    ( map__search(TraceCounts, ProcLabelAndFile, ProcCounts) ->
        MaybeProcCounts = yes(ProcCounts)
    ;
        MaybeProcCounts = no
    ).

:- pred get_proc_calls(proc_trace_counts::in, int::out) is det.

get_proc_calls(ProcCounts, Count) :-
    map__lookup(ProcCounts, port_only(call), ContextCount),
    Count = ContextCount ^ exec_count.

:- pred get_path_only_count(proc_trace_counts::in, mdbcomp_goal_path::in,
    int::out) is det.

get_path_only_count(ProcCounts, GoalPath, Count) :-
    PathPort = path_only(GoalPath),
    ( map__search(ProcCounts, PathPort, ContextCount) ->
        Count = ContextCount ^ exec_count
    ;
        Count = 0
    ).

:- pred get_ite_relative_frequencies(proc_trace_counts::in,
    goal_path::in, goal_path::in, float::out, float::out) is det.

get_ite_relative_frequencies(ProcCounts, ThenGoalPath, ElseGoalPath,
        ThenRelFreq, ElseRelFreq) :-
    goal_path_to_mdbcomp_goal_path(ThenGoalPath, MdbThenGoalPath),
    goal_path_to_mdbcomp_goal_path(ElseGoalPath, MdbElseGoalPath),
    get_path_only_count(ProcCounts, MdbThenGoalPath, ThenCounts),
    get_path_only_count(ProcCounts, MdbElseGoalPath, ElseCounts),
    Total = ThenCounts + ElseCounts,
    ( Total > 0 ->
        ThenRelFreq = float(ThenCounts) / float(Total),
        ElseRelFreq = float(ElseCounts) / float(Total)
    ;
        ThenRelFreq = 0.5,
        ElseRelFreq = 0.5
    ).

:- pred get_disjunct_relative_frequency(proc_trace_counts::in, goal_path::in,
    float::out) is det.

get_disjunct_relative_frequency(ProcCounts, GoalPath, RelFreq) :-
    ( GoalPath  = [disj(Num) | GoalPathRest] ->
        goal_path_to_mdbcomp_goal_path(GoalPathRest, MdbGoalPathRest),
        get_path_only_count(ProcCounts,
            [mdbcomp__program_representation__disj(Num) |
                MdbGoalPathRest], DisjCount),
        get_path_only_count(ProcCounts,
            [mdbcomp__program_representation__disj(1) |
                MdbGoalPathRest], FirstDisjCount),
        ( FirstDisjCount = 0 ->
            RelFreq = 0.0
        ;
            RelFreq = float(DisjCount) / float(FirstDisjCount)
        )
    ;
        unexpected(this_file,
            "get_disjunct_relative_frequency/3 " ++
            "did not see disj(N) at head of goal path")
    ).

:- pred get_case_relative_frequency(proc_trace_counts::in, goal_path::in,
    float::out) is det.

get_case_relative_frequency(ProcCounts, GoalPath, RelFreq) :-
    goal_path_to_mdbcomp_goal_path(GoalPath, MdbGoalPath),
    get_path_only_count(ProcCounts, MdbGoalPath, CaseTotal),
    get_switch_total_count(ProcCounts, MdbGoalPath, SwitchTotal),
    ( SwitchTotal = 0 ->
        RelFreq = 0.0
    ;
        RelFreq = float(CaseTotal) / float(SwitchTotal)
    ).

:- pred get_switch_total_count(proc_trace_counts::in, mdbcomp_goal_path::in,
    int::out) is det.

get_switch_total_count(ProcCounts, MdbGoalPath, Total) :-
    map__foldl(get_switch_total_count_2(MdbGoalPath),
        ProcCounts, 0, Total).

:- pred get_switch_total_count_2(mdbcomp_goal_path::in, path_port::in,
    line_no_and_count::in, int::in, int::out) is det.

get_switch_total_count_2(SwitchGoalPath, PathPort, LineNoAndCount, 
        !TotalAcc) :-
    ( case_in_switch(SwitchGoalPath, PathPort) ->
        !:TotalAcc = !.TotalAcc + LineNoAndCount ^ exec_count
    ;
        true
    ).

:- pred case_in_switch(mdbcomp_goal_path::in, path_port::in) is semidet.

case_in_switch([mdbcomp.program_representation.switch(_) | Prefix],
    path_only([mdbcomp.program_representation.switch(_) | Prefix])).

%-----------------------------------------------------------------------------%

% XXX: This would not be necessary if there were not two definitions of
% goal_paths.  The only difference between them is that the mdbcomp version
% has switch/1 instead of switch/2.

:- pred goal_path_to_mdbcomp_goal_path(goal_path::in, mdbcomp_goal_path::out)
    is det.

goal_path_to_mdbcomp_goal_path(GoalPath, MdbGoalPath) :-
    list__map(goal_path_step_to_mdbcomp_goal_path_step,
        GoalPath, MdbGoalPath).

:- pred goal_path_step_to_mdbcomp_goal_path_step(goal_path_step::in,
    mdbcomp_goal_path_step::out) is det.

goal_path_step_to_mdbcomp_goal_path_step(
    conj(N), mdbcomp.program_representation.conj(N)).
goal_path_step_to_mdbcomp_goal_path_step(
    disj(N), mdbcomp.program_representation.disj(N)).
goal_path_step_to_mdbcomp_goal_path_step(
    switch(N, _), mdbcomp.program_representation.switch(N)).
goal_path_step_to_mdbcomp_goal_path_step(
    ite_cond, mdbcomp.program_representation.ite_cond).
goal_path_step_to_mdbcomp_goal_path_step(
    ite_then, mdbcomp.program_representation.ite_then).
goal_path_step_to_mdbcomp_goal_path_step(
    ite_else, mdbcomp.program_representation.ite_else).
goal_path_step_to_mdbcomp_goal_path_step(
    neg, mdbcomp.program_representation.neg).
goal_path_step_to_mdbcomp_goal_path_step(
    scope(cut), mdbcomp.program_representation.scope(
            mdbcomp.program_representation.cut)).
goal_path_step_to_mdbcomp_goal_path_step(
    scope(no_cut), mdbcomp.program_representation.scope(
            mdbcomp.program_representation.no_cut)).
goal_path_step_to_mdbcomp_goal_path_step(
    first, mdbcomp.program_representation.first).
goal_path_step_to_mdbcomp_goal_path_step(
    later, mdbcomp.program_representation.later).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "tupling.m".

%-----------------------------------------------------------------------------%
:- end_module tupling.
%-----------------------------------------------------------------------------%
