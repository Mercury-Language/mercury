%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2003-2008, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: term_pass2.m.
% Main author of original version: crs.
% Main author of this version: zs.
%
% This file contains the code that tries to prove that procedures terminate.
% For details, please refer to the papers mentioned in termination.m.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.term_pass2.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.term_util.

%-----------------------------------------------------------------------------%

    % NOTE: This code assumes that the SCC does not call any nonterminating
    % procedures. If it does then, that fact should have been detected
    % during pass 1.
    %
:- pred prove_termination_in_scc(module_info::in, pass_info::in,
    scc::in, int::in, termination_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_test.
:- import_module hlds.goal_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module transform_hlds.term_errors.
:- import_module transform_hlds.term_traversal.

:- import_module assoc_list.
:- import_module bag.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module unit.

%-----------------------------------------------------------------------------%

:- type fixpoint_dir
    --->    up
    ;       down.

:- type call_weight_info
    --->    call_weight_info(list(term_error), call_weight_graph).

    % The maximum non-infinite weight from proc to proc and which context
    % it occurs at.
    %
:- type call_weight_graph == map(pred_proc_id, call_weight_dst_map).
:- type call_weight_dst_map == map(pred_proc_id, pair(prog_context, int)).

:- type term_pass2_result
    --->    term_pass2_ok(
                call_weight_info,
                used_args
            )
    ;       term_pass2_error(
                list(term_error)
            ).

%-----------------------------------------------------------------------------%

prove_termination_in_scc(ModuleInfo, PassInfo, SCC, SingleArgs, Termination) :-
    set.to_sorted_list(SCC, SCCProcs),
    init_rec_input_suppliers(SCCProcs, ModuleInfo, InitRecSuppliers),
    prove_termination_in_scc_trial(ModuleInfo, PassInfo, down, SCCProcs,
        InitRecSuppliers, Termination0),
    (
        Termination0 = can_loop(Errors),
        ( if
            % On large SCCs, single arg analysis can require many iterations,
            % so we allow the user to limit the size of the SCCs we will try it
            % on.
            list.length(SCCProcs, ProcCount),
            ProcCount =< SingleArgs,

            % Don't try single arg analysis if it cannot cure the reason for
            % the failure of the main analysis.
            not (
                list.member(Error, Errors),
                Error = term_error(_, imported_pred)
            )
        then
            prove_termination_in_scc_single_arg(ModuleInfo, SCCProcs, PassInfo,
                SingleArgTerminates),
            (
                SingleArgTerminates = yes,
                Termination = cannot_loop(unit)
            ;
                SingleArgTerminates = no,
                Termination = Termination0
            )
        else
            Termination = Termination0
        )
    ;
        Termination0 = cannot_loop(unit),
        Termination = Termination0
    ).

    % Initialise the set of recursive input suppliers to be the set of all
    % input variables in all procedures of the SCC.
    %
:- pred init_rec_input_suppliers(list(pred_proc_id)::in, module_info::in,
    used_args::out) is det.

init_rec_input_suppliers([], _, InitMap) :-
    map.init(InitMap).
init_rec_input_suppliers([PPId | PPIds], ModuleInfo, RecSupplierMap) :-
    init_rec_input_suppliers(PPIds, ModuleInfo, RecSupplierMap0),
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    partition_call_args(ModuleInfo, ArgModes, HeadVars, InArgs, _OutVars),
    MapIsInput = (pred(HeadVar::in, Bool::out) is det :-
        ( if bag.contains(InArgs, HeadVar) then
            Bool = yes
        else
            Bool = no
        )
    ),
    list.map(MapIsInput, HeadVars, BoolList),
    map.det_insert(PPId, BoolList, RecSupplierMap0, RecSupplierMap).

%-----------------------------------------------------------------------------%

    % Perform single arg analysis on the SCC.
    %
    % We pick one procedure in the SCC (one of those with minimal arity).
    % We set the recursive input suppliers of this procedure to contain only
    % the first input argument, and the recursive input suppliers of the other
    % procedures to the empty set, and try a fixpoint iteration. If it works,
    % great, if not, try again with the next input arg of the selected
    % procedure, until we run out of input arguments of that procedure.
    %
    % While the fixpoint iteration in the main algorithm looks for the greatest
    % fixpoint, in which the recursive input supplier sets cannot increase, in
    % single arg analysis we are looking for a smallest fixpoint starting from
    % a given location, so we must make sure that the recursive input supplier
    % sets cannot decrease.
    %
:- pred prove_termination_in_scc_single_arg(module_info::in,
    list(pred_proc_id)::in, pass_info::in, bool::out) is det.

prove_termination_in_scc_single_arg(ModuleInfo, SCC, PassInfo, Terminates) :-
    (
        SCC = [FirstPPId | LaterPPIds],
        FirstArity = lookup_proc_arity(ModuleInfo, FirstPPId),
        find_min_arity_proc(ModuleInfo, LaterPPIds, FirstPPId, FirstArity,
            TrialPPId, RestSCC),
        prove_termination_in_scc_single_arg_2(ModuleInfo, PassInfo,
            TrialPPId, RestSCC, 1, Terminates)
    ;
        SCC = [],
        unexpected($pred, "empty SCC")
    ).

    % Find a procedure of minimum arity among the given list and the tentative
    % guess.
    %
:- pred find_min_arity_proc(module_info::in, list(pred_proc_id)::in,
    pred_proc_id::in, arity::in, pred_proc_id::out,
    list(pred_proc_id)::out) is det.

find_min_arity_proc(_, [], BestSofarPPId, _, BestSofarPPId, []).
find_min_arity_proc(ModuleInfo, [PPId | PPIds], BestSofarPPId, BestSofarArity,
        BestPPId, RestSCC) :-
    Arity = lookup_proc_arity(ModuleInfo, PPId),
    ( if Arity < BestSofarArity then
        find_min_arity_proc(ModuleInfo, PPIds, PPId, Arity, BestPPId,
            RestSCC0),
        RestSCC = [BestSofarPPId | RestSCC0]
    else
        find_min_arity_proc(ModuleInfo, PPIds, BestSofarPPId, BestSofarArity,
            BestPPId, RestSCC0),
        RestSCC = [PPId | RestSCC0]
    ).

    % Perform single arg analysis on the SCC.
    %
:- pred prove_termination_in_scc_single_arg_2(module_info::in, pass_info::in,
    pred_proc_id::in, list(pred_proc_id)::in, int::in, bool::out) is det.

prove_termination_in_scc_single_arg_2(ModuleInfo, PassInfo,
        TrialPPId, RestSCC, ArgNum0, Terminates) :-
    ( if
        init_rec_input_suppliers_single_arg(ModuleInfo, TrialPPId, RestSCC,
            ArgNum0, InitRecSuppliers)
    then
        prove_termination_in_scc_trial(ModuleInfo, PassInfo, up,
            [TrialPPId | RestSCC], InitRecSuppliers, Termination),
        (
            Termination = cannot_loop(unit),
            Terminates = yes
        ;
            Termination = can_loop(_),
            ArgNum1 = ArgNum0 + 1,
            prove_termination_in_scc_single_arg_2(ModuleInfo, PassInfo,
                TrialPPId, RestSCC, ArgNum1, Terminates)
        )
    else
        Terminates = no
    ).

:- pred init_rec_input_suppliers_single_arg(module_info::in, pred_proc_id::in,
    list(pred_proc_id)::in, int::in, used_args::out) is semidet.

init_rec_input_suppliers_single_arg(ModuleInfo, TrialPPId, RestSCC, ArgNum,
        RecSupplierMap) :-
    module_info_pred_proc_info(ModuleInfo, TrialPPId, _, ProcInfo),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    init_rec_input_suppliers_add_single_arg(ModuleInfo, ArgModes, ArgNum,
        TrialPPIdRecSuppliers),
    RecSupplierMap0 = map.singleton(TrialPPId, TrialPPIdRecSuppliers),
    init_rec_input_suppliers_single_arg_others(ModuleInfo, RestSCC,
        RecSupplierMap0, RecSupplierMap).

:- pred init_rec_input_suppliers_add_single_arg(module_info::in,
    list(mer_mode)::in, int::in, list(bool)::out) is semidet.

init_rec_input_suppliers_add_single_arg(ModuleInfo, [Mode | Modes], ArgNum,
        BoolList) :-
    ( if
        mode_is_input(ModuleInfo, Mode),
        ArgNum = 1
    then
        list.map(map_to_no, Modes, BoolListTail),
        BoolList = [yes | BoolListTail]
    else if
        ( if
            mode_is_output(ModuleInfo, Mode)
        then
            NextArgNum = ArgNum
        else if
            mode_is_input(ModuleInfo, Mode),
            ArgNum > 1
        then
            NextArgNum = ArgNum - 1
        else
            fail
        )
    then
        init_rec_input_suppliers_add_single_arg(ModuleInfo, Modes, NextArgNum,
            BoolListTail),
        BoolList = [no | BoolListTail]
    else
        fail
    ).

:- pred init_rec_input_suppliers_single_arg_others(module_info::in,
    list(pred_proc_id)::in, used_args::in, used_args::out) is det.

init_rec_input_suppliers_single_arg_others(_, [], !RecSupplierMap).
init_rec_input_suppliers_single_arg_others(ModuleInfo, [PPId | PPIds],
        !RecSupplierMap) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),
    list.map(map_to_no, HeadVars, BoolList),
    map.det_insert(PPId, BoolList, !RecSupplierMap),
    init_rec_input_suppliers_single_arg_others(ModuleInfo, PPIds,
        !RecSupplierMap).

:- func lookup_proc_arity(module_info, pred_proc_id) = arity.

lookup_proc_arity(ModuleInfo, PPId) = Arity :-
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),
    list.length(HeadVars, Arity).

%-----------------------------------------------------------------------------%

:- pred prove_termination_in_scc_trial(module_info::in, pass_info::in,
    fixpoint_dir::in, list(pred_proc_id)::in, used_args::in,
    termination_info::out) is det.

prove_termination_in_scc_trial(ModuleInfo, PassInfo, FixDir,
        SCC, InitRecSuppliers, Termination) :-
    prove_termination_in_scc_fixpoint(ModuleInfo, PassInfo, FixDir,
        SCC, InitRecSuppliers, Result),
    (
        Result = term_pass2_ok(CallInfo, _),
        CallInfo = call_weight_info(InfCalls, CallWeights),
        (
            InfCalls = [_ | _],
            PassInfo = pass_info(_, MaxErrors, _),
            list.take_upto(MaxErrors, InfCalls, ReportedInfCalls),
            Termination = can_loop(ReportedInfCalls)
        ;
            InfCalls = [],
            ( if
                zero_or_positive_weight_cycles(ModuleInfo, CallWeights,
                    Cycles),
                Cycles = [_ | _]
            then
                PassInfo = pass_info(_, MaxErrors, _),
                list.take_upto(MaxErrors, Cycles, ReportedCycles),
                Termination = can_loop(ReportedCycles)
            else
                Termination = cannot_loop(unit)
            )
        )
    ;
        Result = term_pass2_error(Errors),
        Termination = can_loop(Errors)
    ).

%-----------------------------------------------------------------------------%

:- pred prove_termination_in_scc_fixpoint(module_info::in,
    pass_info::in, fixpoint_dir::in, list(pred_proc_id)::in, used_args::in,
    term_pass2_result::out) is det.

prove_termination_in_scc_fixpoint(ModuleInfo, PassInfo, FixDir,
        SCC, RecSupplierMap0, Result) :-
    map.init(NewRecSupplierMap0),
    map.init(CallWeightGraph0),
    CallInfo0 = call_weight_info([], CallWeightGraph0),
    prove_termination_in_scc_pass(ModuleInfo, PassInfo, FixDir,
        SCC, RecSupplierMap0, NewRecSupplierMap0, CallInfo0, Result1),
    (
        Result1 = term_pass2_ok(_, RecSupplierMap1),
        ( if RecSupplierMap1 = RecSupplierMap0 then
            % We are at a fixed point, so further analysis
            % will not get any better results.
            Result = Result1
        else
            prove_termination_in_scc_fixpoint(ModuleInfo, PassInfo, FixDir,
                SCC, RecSupplierMap1, Result)
        )
    ;
        Result1 = term_pass2_error(_),
        Result = Result1
    ).

%-----------------------------------------------------------------------------%

    % Process a whole SCC, to determine the termination property of each
    % procedure in that SCC.
    %
:- pred prove_termination_in_scc_pass(module_info::in, pass_info::in,
    fixpoint_dir::in, list(pred_proc_id)::in, used_args::in, used_args::in,
    call_weight_info::in, term_pass2_result::out) is det.

prove_termination_in_scc_pass(_, _, _, [], _, NewRecSupplierMap, CallInfo,
        term_pass2_ok(CallInfo, NewRecSupplierMap)).
prove_termination_in_scc_pass(ModuleInfo, PassInfo, FixDir, [PPId | PPIds],
        RecSupplierMap, NewRecSupplierMap0, CallInfo0, Result) :-
    module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, ProcInfo),
    pred_info_get_context(PredInfo, Context),
    proc_info_get_goal(ProcInfo, Goal0),
    % The pretest code we add for compiler-generated unification and comparison
    % predicates uses type casts. It uses them in a way that is guaranteed
    % to terminate, but our analysis is not (yet) able to find this out for
    % itself. We therefore analyse only the non-pretest parts of such goals.
    Goal = maybe_strip_equality_pretest(Goal0),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    map.init(EmptyMap),
    PassInfo = pass_info(FunctorInfo, MaxErrors, MaxPaths),
    init_term_traversal_params(FunctorInfo, PPId, Context, VarTypes,
        EmptyMap, RecSupplierMap, MaxErrors, MaxPaths, Params),
    set.init(PathSet0),
    Info0 = term_traversal_ok(PathSet0, []),
    term_traverse_goal(ModuleInfo, Params, Goal, Info0, Info),
    (
        Info = term_traversal_ok(Paths, CanLoop),
        expect(unify(CanLoop, []), $pred,
            "can_loop detected in pass2 but not pass1"),
        set.to_sorted_list(Paths, PathList),
        upper_bound_active_vars(PathList, ActiveVars),
        map.lookup(RecSupplierMap, PPId, RecSuppliers0),
        proc_info_get_headvars(ProcInfo, Args),
        bag.init(EmptyBag),
        update_rec_input_suppliers(Args, ActiveVars, FixDir,
            RecSuppliers0, RecSuppliers,
            EmptyBag, RecSuppliers0Bag),
        map.det_insert(PPId, RecSuppliers,
            NewRecSupplierMap0, NewRecSupplierMap1),
        add_call_arcs(PathList, RecSuppliers0Bag, CallInfo0, CallInfo1),
        prove_termination_in_scc_pass(ModuleInfo, PassInfo, FixDir,
            PPIds, RecSupplierMap, NewRecSupplierMap1, CallInfo1, Result)
    ;
        Info = term_traversal_error(Errors, CanLoop),
        expect(unify(CanLoop, []), $pred,
            "can_loop detected in pass2 but not pass1"),
        Result = term_pass2_error(Errors)
    ).

%-----------------------------------------------------------------------------%

:- pred update_rec_input_suppliers(list(prog_var)::in, bag(prog_var)::in,
    fixpoint_dir::in, list(bool)::in, list(bool)::out,
    bag(prog_var)::in, bag(prog_var)::out) is det.

update_rec_input_suppliers([], _, _, [], [], !RecBag).
update_rec_input_suppliers([_ | _], _, _, [], [], _, _) :-
    unexpected($pred, "unmatched variables").
update_rec_input_suppliers([], _, _, [_ | _], [], _, _) :-
    unexpected($pred, "unmatched variables").
update_rec_input_suppliers([Arg | Args], ActiveVars, FixDir,
        [RecInputSupplier0 | RecInputSuppliers0],
        [RecInputSupplier | RecInputSuppliers], !RecBag) :-
    (
        RecInputSupplier0 = yes,
        bag.insert(Arg, !RecBag)
    ;
        RecInputSupplier0 = no
    ),
    (
        FixDir = down,
        % This guarantees that the set of rec input suppliers
        % can only decrease.
        ( if bag.contains(ActiveVars, Arg) then
            RecInputSupplier = RecInputSupplier0
        else
            RecInputSupplier = no
        )
    ;
        FixDir = up,
        % This guarantees that the set of rec input suppliers
        % can only increase.
        ( if bag.contains(ActiveVars, Arg) then
            RecInputSupplier = yes
        else
            RecInputSupplier = RecInputSupplier0
        )
    ),
    update_rec_input_suppliers(Args, ActiveVars, FixDir,
        RecInputSuppliers0, RecInputSuppliers, !RecBag).

%-----------------------------------------------------------------------------%

    % This adds the information from a stage 2 traversal to the graph.
    % The graph's nodes are the procedures in the current SCC. The graph's
    % edges represent calls from one procedure in the SCC to another.
    % The number attached to the edge from p to q shows the upper bound
    % on the difference between the size of the recursive input supplier
    % arguments in the call to q and the size of the recursive input supplier
    % arguments in the head of p. If there is no finite upper bound, then
    % we insert the details of the call into the list of "infinite" calls.
    %
:- pred add_call_arcs(list(term_path_info)::in, bag(prog_var)::in,
    call_weight_info::in, call_weight_info::out) is det.

add_call_arcs([], _RecInputSuppliers, !CallInfo).
add_call_arcs([Path | Paths], RecInputSuppliers, !CallInfo) :-
    Path = term_path_info(PPId, CallSite, GammaConst, GammaVars, ActiveVars),
    (
        CallSite = yes(CallPPIdPrime - ContextPrime),
        CallPPId = CallPPIdPrime,
        Context = ContextPrime
    ;
        CallSite = no,
        unexpected($pred, "no call site in path in stage 2")
    ),
    (
        GammaVars = []
    ;
        GammaVars = [_ | _],
        unexpected($pred, "gamma variables in path in stage 2")
    ),
    !.CallInfo = call_weight_info(InfCalls0, CallWeights0),
    ( if bag.is_subbag(ActiveVars, RecInputSuppliers) then
        ( if map.search(CallWeights0, PPId, NeighbourMap0) then
            ( if map.search(NeighbourMap0, CallPPId, OldEdgeInfo) then
                OldEdgeInfo = _OldContext - OldWeight,
                ( if OldWeight >= GammaConst then
                    EdgeInfo = OldEdgeInfo
                else
                    EdgeInfo = Context - GammaConst
                ),
                map.det_update(CallPPId, EdgeInfo, NeighbourMap0, NeighbourMap)
            else
                map.det_insert(CallPPId, Context - GammaConst,
                    NeighbourMap0, NeighbourMap)
            ),
            map.det_update(PPId, NeighbourMap, CallWeights0, CallWeights1)
        else
            NeighbourMap = map.singleton(CallPPId, Context - GammaConst),
            map.det_insert(PPId, NeighbourMap, CallWeights0, CallWeights1)
        ),
        !:CallInfo = call_weight_info(InfCalls0, CallWeights1)
    else
        InfCall = term_error(Context, inf_call(PPId, CallPPId)),
        InfCalls1 = [InfCall | InfCalls0],
        !:CallInfo = call_weight_info(InfCalls1, CallWeights0)
    ),
    add_call_arcs(Paths, RecInputSuppliers, !CallInfo).

%-----------------------------------------------------------------------------%

    % We use a simple depth first search to find and return the list of all
    % cycles in the call graph of the SCC where the change in the size of the
    % recursive input supplier arguments of the procedure that serves as the
    % start and end point of the circularity are not guaranteed to decrease.
    %
    % Finding one such cycle is enough for us to conclude that we cannot prove
    % termination of the procedures in the SCC; we collect all cycles because
    % it may be useful to print them out (if not all, then maybe a limited
    % set).
    %
:- pred zero_or_positive_weight_cycles(module_info::in, call_weight_graph::in,
    list(term_error)::out) is det.

zero_or_positive_weight_cycles(ModuleInfo, CallWeights, Cycles) :-
    map.keys(CallWeights, PPIds),
    zero_or_positive_weight_cycles_2(ModuleInfo, CallWeights, PPIds, Cycles).

:- pred zero_or_positive_weight_cycles_2(module_info::in,
    call_weight_graph::in, list(pred_proc_id)::in, list(term_error)::out)
    is det.

zero_or_positive_weight_cycles_2(_, _, [], []).
zero_or_positive_weight_cycles_2(ModuleInfo, CallWeights, [PPId | PPIds],
        Cycles) :-
    zero_or_positive_weight_cycles_from(ModuleInfo, CallWeights, PPId,
        Cycles1),
    zero_or_positive_weight_cycles_2(ModuleInfo, CallWeights, PPIds,
        Cycles2),
    Cycles = Cycles1 ++ Cycles2.

:- pred zero_or_positive_weight_cycles_from(module_info::in,
    call_weight_graph::in, pred_proc_id::in,list(term_error)::out) is det.

zero_or_positive_weight_cycles_from(ModuleInfo, CallWeights, PPId, Cycles) :-
    map.lookup(CallWeights, PPId, NeighboursMap),
    map.to_assoc_list(NeighboursMap, NeighboursList),
    PPId = proc(PredId, _ProcId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_context(PredInfo, Context),
    zero_or_positive_weight_cycles_from_neighbours(NeighboursList,
        PPId, Context, 0, [], CallWeights, Cycles).

:- pred zero_or_positive_weight_cycles_from_neighbours(assoc_list(pred_proc_id,
    pair(prog_context, int))::in, pred_proc_id::in, prog_context::in,
    int::in, assoc_list(pred_proc_id, prog_context)::in,
    call_weight_graph::in, list(term_error)::out) is det.

zero_or_positive_weight_cycles_from_neighbours([], _, _, _, _, _, []).
zero_or_positive_weight_cycles_from_neighbours([Neighbour | Neighbours],
        LookforPPId, Context, WeightSoFar, VisitedCalls, CallWeights,
        Cycles) :-
    zero_or_positive_weight_cycles_from_neighbour(Neighbour, LookforPPId,
        Context, WeightSoFar, VisitedCalls, CallWeights, Cycles1),
    zero_or_positive_weight_cycles_from_neighbours(Neighbours, LookforPPId,
        Context, WeightSoFar, VisitedCalls, CallWeights, Cycles2),
    list.append(Cycles1, Cycles2, Cycles).

:- pred zero_or_positive_weight_cycles_from_neighbour(pair(pred_proc_id,
    pair(prog_context, int))::in, pred_proc_id::in, prog_context::in,
    int::in, assoc_list(pred_proc_id, prog_context)::in,
    call_weight_graph::in, list(term_error)::out) is det.

zero_or_positive_weight_cycles_from_neighbour(CurPPId - (Context - EdgeWeight),
        LookforPPId, ProcContext, WeightSoFar0, VisitedCalls,
        CallWeights, Cycles) :-
    WeightSoFar1 = WeightSoFar0 + EdgeWeight,
    ( if
        CurPPId = LookforPPId
    then
        % We have a cycle on the looked for ppid.
        ( if WeightSoFar1 >= 0 then
            FinalVisitedCalls = [CurPPId - Context | VisitedCalls],
            list.reverse(FinalVisitedCalls, RevFinalVisitedCalls),
            CycleError = cycle(LookforPPId, RevFinalVisitedCalls),
            CycleErrorContext = term_error(ProcContext, CycleError),
            Cycles = [CycleErrorContext]
        else
            Cycles = []
        )
    else if
        assoc_list.keys(VisitedCalls, VisitedPPIds),
        list.member(CurPPId, VisitedPPIds)
    then
        % We have a cycle, but not on the looked for pred_proc_id. We ignore
        % it here; it will be picked up when we process that pred_proc_id.
        Cycles = []
    else
        % No cycle; try all possible edges from this node.
        NewVisitedCalls = [CurPPId - Context | VisitedCalls],
        map.lookup(CallWeights, CurPPId, NeighboursMap),
        map.to_assoc_list(NeighboursMap, NeighboursList),
        zero_or_positive_weight_cycles_from_neighbours(NeighboursList,
            LookforPPId, ProcContext, WeightSoFar1,
            NewVisitedCalls, CallWeights, Cycles)
    ).

%-----------------------------------------------------------------------------%

:- pred map_to_no(T::in, bool::out) is det.

map_to_no(_, no).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.term_pass2.
%-----------------------------------------------------------------------------%
