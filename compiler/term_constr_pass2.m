%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002, 2005-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: term_constr_pass2.m.
% Main author: juliensf.
%
% This module analyses a SCC of the call-graph and tries to prove that
% it terminates.
%
% XXX This version is just a place-holder. It attempts a very simple
% proof method which is essentially what the existing termination analyser
% does.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.term_constr_pass2.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.term_constr_main_types.

%-----------------------------------------------------------------------------%

    % This structure holds the values of options used to control pass 2.
    %
:- type pass2_options.

    % pass2_options_init(MaxMatrixSize).
    % Initialise the pass2_options structure. `MaxMatrixSize' specifies
    % the maximum number of constraints we allow a matrix to grow to
    % before we abort and try other approximations.
    %
:- func pass2_options_init(int) = pass2_options.

:- pred prove_termination_in_scc(pass2_options::in, scc::in,
    module_info::in, constr_termination_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module libs.
:- import_module libs.lp_rational.
:- import_module libs.polyhedron.
:- import_module libs.rat.
:- import_module parse_tree.
:- import_module parse_tree.prog_data_pragma.
:- import_module transform_hlds.term_constr_data.
:- import_module transform_hlds.term_constr_errors.
:- import_module transform_hlds.term_constr_util.

:- import_module assoc_list.
:- import_module bimap.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%
%
% Handle pass 2 options.
%

:- type pass2_options
    --->    pass2_options(
                max_matrix_size :: int
            ).

pass2_options_init(MaxSize) = pass2_options(MaxSize).

%-----------------------------------------------------------------------------%

:- type abstract_ppids == list(abstract_ppid).

    % Each edge in the call-graph represents a single call site.
    %
:- type edge
    --->    term_cg_edge(
                % The procedure that is making the call.
                tcge_caller         :: abstract_ppid,

                % The procedure being called.
                tcge_callee         :: abstract_ppid,

                % The size_vars that correspond to the variables in the head
                % of the procedure.
                tcge_head_args      :: size_vars,

                % The size_vars that correspond to the variables
                % in the procedure call.
                tcge_call_args      :: size_vars,

                % Variables in the procedure known to have zero size.
                tcge_zeros          :: set(size_var),

                % The constraints that occur between the head of the procedure
                % and the call.
                tcge_label          :: polyhedron
            ).

:- type edges == list(edge).

:- type cycle
    --->    term_cg_cycle(
                % A list of every procedure involved in this cycle.
                tcgc_nodes          :: list(abstract_ppid),

                % A list of edges involved in this cycle.
                % Note: The list is not ordered. This allows us to decide
                % (later) on where we want the cycle to start.
                tcgc_edges          :: list(edge)
            ).

:- type cycles == list(cycle).

    % A c_cycle, or collapsed cycle, is an elementary cycle from the call-graph
    % where we have picked a starting vertex and travelled around the cycle
    % conjoining all the labels (constraints) as we go.
    %
:- type cycle_set
    --->    term_cg_cycle_set(
                tcgcs_start     :: abstract_ppid,
                tcgcs_cycles    :: list(edge)
            ).

%-----------------------------------------------------------------------------%

prove_termination_in_scc(Options, SCC0, ModuleInfo, Result) :-
    ( if set.is_empty(SCC0) then
        Result = cannot_loop(term_reason_analysis)
    else
        AbstractSCC = get_abstract_scc(ModuleInfo, SCC0),
        ( if scc_contains_recursion(AbstractSCC) then
            % XXX Pass 1 should really set this up.
            PPIdSCC = set.map((func(A) = real(A)), SCC0),
            set.to_sorted_list(PPIdSCC, PPIds),

            SizeVarSet = size_varset_from_abstract_scc(AbstractSCC),
            Edges = label_edges_in_scc(AbstractSCC, ModuleInfo,
                Options ^ max_matrix_size),
            Cycles = find_elementary_cycles_in_scc(PPIds, Edges),
            CycleSets = partition_cycles(PPIds, Cycles),
            prove_termination(CycleSets, AbstractSCC, SizeVarSet, Result)
        else
            Result = cannot_loop(term_reason_analysis)
        )
    ).

%-----------------------------------------------------------------------------%
%
% Predicates for labelling edges.
%

% Work out what the constraints are between each procedure head and each
% call for every call in the SCC. This information is implicit in the
% AR, so we traverse the AR building up a list of labelled edges as
% we go - this is similar to the fixpoint calculation we performed in pass 1
% except that we can stop after we have examined the last call. This often
% means that we can avoid performing unnecessary convex hull operations.

:- func label_edges_in_scc(abstract_scc, module_info, int) = edges.

label_edges_in_scc(AbstractSCC, ModuleInfo, MaxMatrixSize) = Edges :-
    FindEdges =
        ( pred(Proc::in, !.Edges::in, !:Edges::out) is det :-
            find_edges_in_goal(Proc, AbstractSCC, ModuleInfo, MaxMatrixSize,
                Proc ^ ap_body, 1, _, polyhedron.universe, _, [],
                ProcEdges, yes, _),
            list.append(ProcEdges, !Edges)
        ),
    set.foldl(FindEdges, AbstractSCC, [], Edges).

    % The four accumulators here are for:
    % (1) the number of calls seen so far
    % (2) the constraints so far
    % (3) the edges found
    % (4) whether to abort or continue looking
    %
:- pred find_edges_in_goal(abstract_proc::in, abstract_scc::in,
    module_info::in, int::in, abstract_goal::in, int::in, int::out,
    polyhedron::in, polyhedron::out, edges::in, edges::out, bool::in,
    bool::out) is det.

find_edges_in_goal(Proc, AbstractSCC, ModuleInfo, MaxMatrixSize,
        Goal, !Calls, !Polyhedron, !Edges, !Continue) :-
    (
        Goal = term_disj(Goals, _, Locals, _),
        (
            !.Continue = yes,

            % XXX We may be able to prove termination in more cases if we pass
            % it !.Polyhedron instead of polyhedron.universe ... although
            % I don't think it is a major concern at the moment.
            find_edges_in_disj(Proc, AbstractSCC, ModuleInfo,
                MaxMatrixSize, polyhedron.universe, Goals, !Calls,
                [], DisjConstrs0, [], Edges1, !Continue),
            Edges2 = list.map(fix_edges(!.Polyhedron), Edges1),
            list.append(Edges2, !Edges),
            (
                !.Continue = yes,
                SizeVarSet = Proc ^ ap_size_varset,
                DisjConstrs = polyhedron.project_all(SizeVarSet, Locals,
                    DisjConstrs0),
                Constrs2 = list.foldl(
                    polyhedron.convex_union(SizeVarSet, yes(MaxMatrixSize)),
                    DisjConstrs, polyhedron.empty),
                polyhedron.intersection(Constrs2, !Polyhedron)
            ;
                !.Continue = no
            )
        ;
            !.Continue = no
        )
    ;
        Goal = term_conj(Goals, Locals, _),
        (
            !.Continue = yes,
            list.foldl4(
                find_edges_in_goal(Proc, AbstractSCC, ModuleInfo,
                    MaxMatrixSize),
                Goals, !Calls, !Polyhedron, !Edges, !Continue),
            (
                !.Continue = yes,
                polyhedron.project_polyhedron(Proc ^ ap_size_varset, Locals,
                    !Polyhedron)
            ;
                !.Continue = no
            )
        ;
            !.Continue = no
        )
    ;
        Goal = term_call(CallPPId0, _, CallVars, ZeroVars, _, _, _),
        % Having found a call, we now need to construct a label for that edge
        % and then continue looking for more edges.
        Edge = term_cg_edge(Proc ^ ap_ppid, CallPPId0,
            Proc ^ ap_head_vars, CallVars, Proc ^ ap_zeros, !.Polyhedron),
        list.cons(Edge, !Edges),

        % Update the call count and maybe stop processing
        % if that was the last call.
        !:Calls = !.Calls + 1,
        ( if !.Calls > Proc ^ ap_num_calls then
            !:Continue = no
        else
            true
        ),
        (
            !.Continue = no
        ;
            !.Continue = yes,
            CallPPId0 = real(CallPPId),
            module_info_pred_proc_info(ModuleInfo, CallPPId,  _, CallProcInfo),
            proc_info_get_termination2_info(CallProcInfo, CallTerm2Info),
            MaybeArgSizeInfo = term2_info_get_success_constrs(CallTerm2Info),
            (
                MaybeArgSizeInfo = no,
                unexpected($pred, "proc with no arg size info in pass 2")
            ;
                MaybeArgSizeInfo = yes(ArgSizePolyhedron0),
                ( if polyhedron.is_universe(ArgSizePolyhedron0) then
                    % If the polyhedron is universe, then there is no point
                    % in running the substitution.
                    true
                else
                    MaybeCallProc = term2_info_get_abstract_rep(CallTerm2Info),
                    (
                        MaybeCallProc = yes(CallProc0),
                        CallProc = CallProc0
                    ;
                        MaybeCallProc = no,
                        unexpected($pred,
                            "no abstract representation for proc")
                    ),
                    HeadVars = CallProc ^ ap_head_vars,
                    Subst = map.from_corresponding_lists(HeadVars, CallVars),
                    Eqns0 = non_false_constraints( ArgSizePolyhedron0),
                    Eqns1 = substitute_size_vars(Eqns0, Subst),
                    Eqns  = lp_rational.set_vars_to_zero(ZeroVars, Eqns1),
                    ArgSizePolyhedron = from_constraints(Eqns),
                    polyhedron.intersection(ArgSizePolyhedron, !Polyhedron)
                )
            )
        )
    ;
        Goal = term_primitive(Primitive, _, _),
        (
            !.Continue = yes,
            polyhedron.intersection(Primitive, !Polyhedron)
        ;
            !.Continue = no
        )
    ).

:- pred find_edges_in_disj(abstract_proc::in, abstract_scc::in,
    module_info::in, int::in, polyhedron::in, abstract_goals::in,
    int::in, int::out, polyhedra::in, polyhedra::out, edges::in, edges::out,
    bool::in, bool::out) is det.

find_edges_in_disj(_, _, _, _, _, [], !Calls, !DisjConstrs, !Edges, !Continue).
find_edges_in_disj(Proc, AbstractSCC, ModuleInfo, MaxMatrixSize, TopPoly,
        [Disj | Disjs], !Calls, !DisjConstrs, !Edges, !Continue) :-
    find_edges_in_goal(Proc, AbstractSCC, ModuleInfo, MaxMatrixSize, Disj,
        !Calls, TopPoly, Constrs, !Edges, !Continue),
    list.cons(Constrs, !DisjConstrs),

    % This is why it is important that after numbering the calls in the AR
    % we don't change anything around; otherwise this short-circuiting
    % will not work correctly.
    (
        !.Continue = yes,
        find_edges_in_disj(Proc, AbstractSCC, ModuleInfo,
            MaxMatrixSize, TopPoly, Disjs, !Calls, !DisjConstrs,
            !Edges, !Continue)
    ;
        !.Continue = no
    ).

:- func fix_edges(polyhedron, edge) = edge.

fix_edges(Poly, Edge0) = Edge :-
    Label0 = Edge0 ^ tcge_label,
    Label = polyhedron.intersection(Poly, Label0),
    Edge = Edge0 ^ tcge_label := Label.

%-----------------------------------------------------------------------------%
%
% Cycle detection.
%

% To find the elementary cycles of this SCC we perform a DFS of the call-graph.
% Since the call-graph is technically a pseudograph (i.e. it admits parallel
% edges and self-loops), we first of all strip out any self-loops
% to make things easier.

:- func find_elementary_cycles_in_scc(list(abstract_ppid), edges) = cycles.

find_elementary_cycles_in_scc(SCC, Edges0) = Cycles :-
    % Get any self-loops for each procedure.
    list.filter_map(direct_call, Edges0, Cycles0, Edges),

    % Find larger elementary cycles in what is left.
    Cycles1 = find_cycles(SCC, Edges),
    Cycles = Cycles0 ++ Cycles1.

    % Succeeds iff Edge is an edge that represents a directly recursive call
    % (a self-loop in the pseudograph)
    %
:- pred direct_call(edge::in, cycle::out) is semidet.

direct_call(Edge, Cycle) :-
    Edge ^ tcge_caller = Edge ^ tcge_callee,
    Cycle = term_cg_cycle([Edge ^ tcge_caller], [Edge]).

:- func find_cycles(list(abstract_ppid), edges) = cycles.

find_cycles(SCC, Edges) = Cycles :-
    EdgeMap = partition_edges(SCC, Edges),
    Cycles = search_for_cycles(SCC, EdgeMap).

    % Builds a map from `pred_proc_id' to a list of the edges that begin
    % with the `pred_proc_id.
    %
:- func partition_edges(list(abstract_ppid), edges)
    = map(abstract_ppid, edges).

partition_edges([], _) = map.init.
partition_edges([ProcId | SCC], Edges0) = Map :-
    Map0 = partition_edges(SCC, Edges0),
    Edges = list.filter(
        ( pred(Edge::in) is semidet :-
            ProcId = Edge ^ tcge_caller
        ), Edges0),
    map.det_insert(ProcId, Edges, Map0, Map).

:- func search_for_cycles(list(abstract_ppid), map(abstract_ppid, edges))
    = cycles.

search_for_cycles([], _) = [].
search_for_cycles([HeadPPId | TailPPId], Map0) = Cycles :-
    HeadCycles = search_for_cycles_2(HeadPPId, Map0),
    map.delete(HeadPPId, Map0, Map1),
    TailCycles = search_for_cycles(TailPPId, Map1),
    Cycles = HeadCycles ++ TailCycles.

:- func search_for_cycles_2(abstract_ppid, map(abstract_ppid, edges)) = cycles.

search_for_cycles_2(StartPPId, Map) = Cycles :-
    map.lookup(Map, StartPPId, InitialEdges),
    list.foldl(search_for_cycles_3(StartPPId, [], Map, []), InitialEdges,
        [], Cycles).

:- pred search_for_cycles_3(abstract_ppid::in, edges::in,
    map(abstract_ppid, edges)::in, list(abstract_ppid)::in, edge::in,
    cycles::in, cycles::out) is det.

search_for_cycles_3(Start, SoFar, Map, Visited, Edge, !Cycles) :-
    ( if Start = Edge ^ tcge_callee then
        Cycle = term_cg_cycle([Edge ^ tcge_caller | Visited], [Edge | SoFar]),
        list.cons(Cycle, !Cycles)
    else
        ( if map.search(Map, Edge ^ tcge_callee, MoreEdges0) then
            NotVisited =
                ( pred(E::in) is semidet :-
                    not list.member(E ^ tcge_caller, Visited)
                ),
            MoreEdges = list.filter(NotVisited, MoreEdges0),
            list.foldl(
                search_for_cycles_3(Start, [Edge | SoFar], Map,
                    [Edge ^ tcge_caller | Visited]),
                MoreEdges, !Cycles)
        else
            true
        )
    ).

%-----------------------------------------------------------------------------%
%
% Partitioning sets of cycles.
%

:- func partition_cycles(abstract_ppids, cycles) = list(cycle_set).

partition_cycles([], _) = [].
partition_cycles([Proc | Procs], Cycles0) = CycleSets :-
    list.filter(cycle_contains_proc(Proc), Cycles0, PCycles, Cycles1),
    CycleSets0 = partition_cycles(Procs, Cycles1),
    PEdges = collapse_cycles(Proc, PCycles),
    (
        PEdges = [],
        CycleSets = CycleSets0
    ;
        PEdges = [_ | _],
        CycleSets = [term_cg_cycle_set(Proc, PEdges) | CycleSets0]
    ).

:- func get_proc_from_abstract_scc(list(abstract_proc), abstract_ppid)
    = abstract_proc.

get_proc_from_abstract_scc([], _) = _ :-
    unexpected($pred, "cannot find proc").
get_proc_from_abstract_scc([Proc | Procs], PPId) =
    ( if Proc ^ ap_ppid = PPId then
        Proc
    else
        get_proc_from_abstract_scc(Procs, PPId)
    ).

%-----------------------------------------------------------------------------%
%
% Termination checking.
%

% This approach is very crude. It just checks that the sum of all
% the non-zero arguments is decreasing around all the elementary cycles.

:- pred prove_termination(list(cycle_set)::in, abstract_scc::in,
    size_varset::in, constr_termination_info::out) is det.

prove_termination(Cycles, AbstractSCC, SizeVarSet, Result) :-
    ( if total_sum_decrease(AbstractSCC, SizeVarSet, Cycles) then
        Result = cannot_loop(term_reason_analysis)
    else
        % NOTE: The context here will never be used, in any case
        % it is not clear what it should be.
        Error = term2_error(term.context_init, cond_not_satisfied),
        Result = can_loop([Error])
    ).

:- pred total_sum_decrease(abstract_scc::in, size_varset::in,
    list(cycle_set)::in) is semidet.

total_sum_decrease(_, _, []).
total_sum_decrease(AbstractSCC, SizeVarSet, [CycleSet | CycleSets]):-
    CycleSet = term_cg_cycle_set(Start, Loops),
    total_sum_decrease_2(AbstractSCC, SizeVarSet, Start, Loops),
    total_sum_decrease(AbstractSCC, SizeVarSet, CycleSets).

:- pred total_sum_decrease_2(abstract_scc::in, size_varset::in,
    abstract_ppid::in, list(edge)::in) is semidet.

total_sum_decrease_2(_, _, _, []).
total_sum_decrease_2(AbstractSCC, SizeVarSet, PPId, Loops @ [_ | _]) :-
    all [Loop] (
        list.member(Loop, Loops)
    =>
        strict_decrease_around_loop(AbstractSCC, SizeVarSet, PPId, Loop)
    ).

    % Succeeds iff there is strict decrease in the sum of *all*
    % the arguments around the given loop.
    %
:- pred strict_decrease_around_loop(abstract_scc::in, size_varset::in,
    abstract_ppid::in, edge::in) is semidet.

strict_decrease_around_loop(AbstractSCC, SizeVarSet, PPId, Loop) :-
    ( if
        ( PPId \= Loop ^ tcge_caller
        ; PPId \= Loop ^ tcge_callee
        )
    then
        unexpected($pred, "badly formed loop")
    else
        true
    ),
    IsActive =
        ( func(Var::in, Input::in) = (Var::out) is semidet :-
            Input = yes
        ),
    Proc = get_proc_from_abstract_scc(set.to_sorted_list(AbstractSCC), PPId),
    Inputs = Proc ^ ap_inputs,
    HeadArgs = list.filter_map_corresponding(IsActive, Loop ^ tcge_head_args,
        Inputs),
    CallArgs = list.filter_map_corresponding(IsActive, Loop ^ tcge_call_args,
        Inputs),
    Terms = make_coeffs(HeadArgs, -one) ++ make_coeffs(CallArgs, one),

    % NOTE: If you examine the condition it may contain fewer variables
    % than you expect. This is because if the same argument occurs in the head
    % and the call they will cancel each other out.
    Condition = construct_constraint(Terms, lp_lt_eq, -one),
    Label = polyhedron.non_false_constraints(Loop ^ tcge_label),
    entailed(SizeVarSet, Label, Condition).

:- pred cycle_contains_proc(abstract_ppid::in, cycle::in) is semidet.

cycle_contains_proc(PPId, term_cg_cycle(Nodes, _)) :- list.member(PPId, Nodes).

    % XXX Fix this name.
    %
:- func make_coeffs(size_vars, rat) = lp_terms.

make_coeffs(Vars, Coeff) = list.map((func(Var) = Var - Coeff), Vars).

%-----------------------------------------------------------------------------%

    % Collapse all the cycles so that they all start with the given
    % procedure and all the edge labels between are conjoined.
    %
:- func collapse_cycles(abstract_ppid, cycles) = edges.

collapse_cycles(Start, Cycles) = list.map(collapse_cycle(Start), Cycles).

:- func collapse_cycle(abstract_ppid, cycle) = edge.

collapse_cycle(StartPPId, Cycle) = CollapsedCycle :-
    Cycle = term_cg_cycle(_, Edges0),
    (
        Edges0 = [],
        unexpected($pred, "trying to collapse a cycle with no edges")
    ;
        Edges0 = [Edge],
        CollapsedCycle = Edge
    ;
        Edges0 = [_, _ | _],
        order_nodes(StartPPId, Edges0, Edges),
        (
            Edges = [StartEdge | Rest],
            StartEdge = term_cg_edge(_, _, HeadVars, CallVars0,
                Zeros0, Polyhedron0),
            collapse_cycle_2(Rest, Zeros0, Zeros, CallVars0, CallVars,
                Polyhedron0, Polyhedron),
            CollapsedCycle = term_cg_edge(StartPPId, StartPPId,
                HeadVars, CallVars, Zeros, Polyhedron)
        ;
            Edges = [],
            unexpected($pred, "error while collapsing cycles")
        )
    ).

:- pred collapse_cycle_2(edges::in, zero_vars::in, zero_vars::out,
    size_vars::in, size_vars::out, polyhedron::in, polyhedron::out) is det.

collapse_cycle_2([], !Zeros, !CallVars, !Polyhedron).
collapse_cycle_2([Edge | Edges], !Zeros, !CallVars, !Polyhedron) :-
    set.union(Edge ^ tcge_zeros, !Zeros),
    HeadVars = Edge ^ tcge_head_args,
    Subst0 = assoc_list.from_corresponding_lists(HeadVars, !.CallVars),
    bimap.set_from_assoc_list(Subst0, bimap.init, Subst),

    % We now need to substitute variables from the call to *this* predicate
    % for head variables in both the constraints from the body of the predicate
    % and also into the variables in the calls to the next predicate.
    %
    % While it might be easier to put equality constraints between
    % the caller's arguments and the callee's head arguments,
    % the substitution is in some ways more desirable as we can detect
    % some neutral arguments more directly.
    !:CallVars = list.map(subst_size_var(Subst), Edge ^ tcge_call_args),

    % These should be non-false, so throw an exception if they are not.
    Constraints0 = polyhedron.non_false_constraints(!.Polyhedron),
    Constraints1 = polyhedron.non_false_constraints(Edge ^ tcge_label),
    Constraints2 = list.map(subst_size_var_eqn(Subst), Constraints1),
    Constraints3 = Constraints0 ++ Constraints2,
    !:Polyhedron = polyhedron.from_constraints(Constraints3),
    collapse_cycle_2(Edges, !Zeros, !CallVars, !Polyhedron).

:- pred order_nodes(abstract_ppid::in, edges::in, edges::out) is det.

order_nodes(StartPPId, Edges0, [Edge | Edges]) :-
    EdgeMap = build_edge_map(Edges0),
    map.lookup(EdgeMap, StartPPId, Edge),
    order_nodes_2(StartPPId, Edge ^ tcge_callee, EdgeMap, Edges).

:- pred order_nodes_2(abstract_ppid::in, abstract_ppid::in,
    map(abstract_ppid, edge)::in, edges::out) is det.

order_nodes_2(StartPPId, CurrPPId, Map, Edges) :-
    ( if StartPPId = CurrPPId then
        Edges = []
    else
        map.lookup(Map, CurrPPId, Edge),
        order_nodes_2(StartPPId, Edge ^ tcge_callee, Map, Edges0),
        Edges = [Edge | Edges0]
    ).

:- func build_edge_map(edges) = map(abstract_ppid, edge).

build_edge_map([]) = map.init.
build_edge_map([Edge | Edges]) =
    map.det_insert(build_edge_map(Edges), Edge ^ tcge_caller, Edge).

:- func subst_size_var_eqn(bimap(size_var, size_var), constraint)
    = constraint.

subst_size_var_eqn(Map, Eqn0) = Eqn :-
    deconstruct_constraint(Eqn0, Coeffs0, Operator, Constant),
    Coeffs = list.map(subst_size_var_coeff(Map), Coeffs0),
    Eqn = construct_constraint(Coeffs, Operator, Constant).

:- func subst_size_var_coeff(bimap(size_var, size_var), lp_term) = lp_term.

subst_size_var_coeff(Map, Var0 - Coeff) = Var - Coeff :-
    Var = subst_size_var(Map, Var0).

:- func subst_size_var(bimap(size_var, size_var), size_var) = size_var.

subst_size_var(Map, Old) = (if bimap.search(Map, Old, New) then New else Old).

%-----------------------------------------------------------------------------%
%
% Predicates for printing out debugging traces.
%

:- pred write_cycles(io.text_output_stream::in, module_info::in,
    size_varset::in, cycles::in, io::di, io::uo) is det.
:- pragma consider_used(pred(write_cycles/6)).

write_cycles(_, _, _, [], !IO).
write_cycles(Stream, ModuleInfo, SizeVarSet, [Cycle | Cycles], !IO) :-
    io.write_string(Stream, "Cycle in SCC:\n", !IO),
    write_cycle(Stream, ModuleInfo, Cycle ^ tcgc_nodes, !IO),
    list.foldl(write_edge(Stream, ModuleInfo, SizeVarSet),
        Cycle ^ tcgc_edges, !IO),
    io.nl(Stream, !IO),
    write_cycles(Stream, ModuleInfo, SizeVarSet, Cycles, !IO).

:- pred write_cycle(io.text_output_stream::in, module_info::in,
    list(abstract_ppid)::in, io::di, io::uo) is det.

write_cycle(_, _, [], !IO).
write_cycle(Stream, ModuleInfo, [Proc | Procs], !IO) :-
    Proc = real(PredProcId),
    io.format(Stream, "\t- %s\n",
        [s(pred_proc_id_to_dev_string(ModuleInfo, PredProcId))], !IO),
    write_cycle(Stream, ModuleInfo, Procs, !IO).

:- pred write_edge(io.text_output_stream::in, module_info::in,
    size_varset::in, edge::in, io::di, io::uo) is det.

write_edge(Stream, ModuleInfo, SizeVarSet, Edge, !IO) :-
    Edge ^ tcge_caller = real(PredProcId),
    Edge ^ tcge_callee = real(CallPredProcId),
    io.format(Stream, "Edge is:\n\tHead: %s : ",
        [s(pred_proc_id_to_dev_string(ModuleInfo, PredProcId))], !IO),
    write_size_vars(Stream, SizeVarSet, Edge ^ tcge_head_args, !IO),
    io.write_string(Stream, " :- \n", !IO),
    io.write_string(Stream, "\tConstraints are:  \n", !IO),
    write_polyhedron(Stream, SizeVarSet, Edge ^ tcge_label, !IO),
    io.format(Stream, "\n\tCall is:  %s : ",
        [s(pred_proc_id_to_dev_string(ModuleInfo, CallPredProcId))], !IO),
    write_size_vars(Stream, SizeVarSet, Edge ^ tcge_call_args, !IO),
    io.write_string(Stream, " :- \n\n", !IO).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.term_constr_pass2.
%-----------------------------------------------------------------------------%
