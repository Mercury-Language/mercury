%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002, 2005-2006 The University of Melbourne.
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
% XXX This version is just a place-holder.  It attempts a very simple
% proof method which is essentially what the existing termination analyser 
% does.
% 
%-----------------------------------------------------------------------------%

:- module transform_hlds.term_constr_pass2.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.term_constr_main.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % This structure holds the values of options used to control pass 2.
    %
:- type pass2_options.

    % pass2_options_init(MaxMatrixSize).
    % Initialise the pass2_options structure.  `MaxMatrixSize' specifies
    % the maximum number of constraints we allow a matrix to grow to
    % before we abort and try other approximations. 
    % 
:- func pass2_options_init(int) = pass2_options.

:- pred prove_termination_in_scc(pass2_options::in, list(pred_proc_id)::in, 
    module_info::in, constr_termination_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_pred.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.lp_rational.
:- import_module libs.options.
:- import_module libs.polyhedron.
:- import_module libs.rat.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.term_constr_data.
:- import_module transform_hlds.term_constr_errors.
:- import_module transform_hlds.term_constr_util.

:- import_module assoc_list.
:- import_module bimap.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
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

:- type scc == list(abstract_ppid).

    % Each edge in the call-graph represents a single call site.
    %
:- type edge 
    --->    edge(
                head        :: abstract_ppid,
                            % The procedure that is making the call. 
                
                zeros       :: set(size_var),
                            % Variables in the procedure known to have
                            % zero size.
                
                head_args   :: size_vars,
                            % The size_vars that correspond to the 
                            % variables in the head of the procedure.
                
                label       :: polyhedron,
                            % The constraints that occur between the
                            % head of the procedure and the call.
                
                callee      :: abstract_ppid, 
                            % The callee procedure.
                
                call_args   :: size_vars 
                            % The size_vars that correspond to the
                            % variables in the procedure call.
            ).

:- type edges == list(edge).

:- type cycle 
    --->    cycle(
                nodes       :: list(abstract_ppid),
                            % A list of every procedure involved in 
                            % this cycle.
                
                edges       :: list(edge)
                            % A list of edges involved in this cycle.
                            % Note: It is not ordered.  This allows
                            % us to decide (later) on where we want
                            % the cycle to start.
            ). 

:- type cycles == list(cycle).

    % A c_cycle, or collapsed cycle, is an elmentary cycle from the
    % call-graph where we have picked a starting vertex and travelled
    % around the cycle conjoining all the labels (constraints) as we go.
    %
:- type cycle_set 
    --->    c_set(
                start    :: abstract_ppid,
                c_cycles :: list(edge)
            ).

%-----------------------------------------------------------------------------%

prove_termination_in_scc(_, [], _, cannot_loop(analysis), !IO). 
prove_termination_in_scc(Options, SCC0 @ [_|_], ModuleInfo, Result, !IO) :-
    AbstractSCC = get_abstract_scc(ModuleInfo, SCC0),
    % XXX Pass 1 should really set this up.
    SCC = list.map((func(A) = real(A)), SCC0),
    ( scc_contains_recursion(AbstractSCC) ->
        Varset = varset_from_abstract_scc(AbstractSCC),
        Edges  = label_edges_in_scc(AbstractSCC, ModuleInfo,
            Options ^ max_matrix_size),
        Cycles    = find_elementary_cycles_in_scc(SCC, Edges),
        CycleSets = partition_cycles(SCC, Cycles),
        prove_termination(CycleSets, AbstractSCC, Varset, Result)
    ;
        Result = cannot_loop(analysis)
    ).

%-----------------------------------------------------------------------------%
%
% Predicates for labelling edges.
%

% Work out what the constraints are between each procedure head and each
% call for every call in the SCC.  This information is implicit in the
% AR, so we traverse the AR building up a list of labelled edges as
% we go - this is similar to the fixpoint calculation we performed in pass 1
% except that we can stop after we have examined the last call.  This often
% means that we can avoid performing unnecessary convex hull operations.

:- func label_edges_in_scc(abstract_scc, module_info, int) = edges.

label_edges_in_scc(Procs, ModuleInfo, MaxMatrixSize) = Edges :-
    FindEdges = (pred(Proc::in, !.Edges::in, !:Edges::out) is det :-
        find_edges_in_goal(Proc, Procs, ModuleInfo, MaxMatrixSize,
            Proc ^ body, 1, _, polyhedron.universe, _, [],
            ProcEdges, yes, _),
        list.append(ProcEdges, !Edges)
    ),
    list.foldl(FindEdges, Procs, [], Edges).        

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
        AbstractGoal, !Calls, !Polyhedron, !Edges, !Continue) :- 
    AbstractGoal = term_disj(Goals, _, Locals, _),
    ( 
        !.Continue = yes,
        %
        % XXX We may be able to prove termination in more cases
        % if we pass in !.Polyhedron instead of
        % of polyhedron.universe ... although I don't think
        % it is a major concern at the moment.
        %   
        find_edges_in_disj(Proc, AbstractSCC, ModuleInfo,
            MaxMatrixSize, polyhedron.universe, Goals, !Calls,
            [], DisjConstrs0, [], Edges1, !Continue),
        Edges2 = list.map(fix_edges(!.Polyhedron), Edges1),
        list.append(Edges2, !Edges),
        (
            !.Continue = yes,
            Varset = Proc ^ varset,
            DisjConstrs = polyhedron.project_all(Varset, Locals,
                DisjConstrs0),
            Constrs2 = list.foldl(
                polyhedron.convex_union(Varset, 
                    yes(MaxMatrixSize)), DisjConstrs, 
                    polyhedron.empty),
            polyhedron.intersection(Constrs2, !Polyhedron)
        ;
            !.Continue = no
        )
    ;
        !.Continue = no
    ).

find_edges_in_goal(Proc, AbstractSCC, ModuleInfo, MaxMatrixSize,
        AbstractGoal, !Calls, !Polyhedron, !Edges, !Continue) :- 
    AbstractGoal = term_conj(Goals, Locals, _),
    ( 
        !.Continue = yes,   
        list.foldl4(find_edges_in_goal(Proc, AbstractSCC, ModuleInfo,
                MaxMatrixSize),
            Goals, !Calls, !Polyhedron, !Edges, !Continue), 
        (
            !.Continue = yes,
                polyhedron.project(Locals, Proc ^ varset, !Polyhedron)
        ;
            !.Continue = no
        )
    ;
        !.Continue = no
    ).

find_edges_in_goal(Proc, _AbstractSCC, ModuleInfo, _, 
        term_call(CallPPId0, _, CallVars, ZeroVars, _, _, _),
        !Calls, !Polyhedron, !Edges, !Continue) :- 
    %
    % Having found a call we now need to construct a label for that
    % edge and then continue looking for more edges.
    % 
    Edge = edge(Proc ^ ppid, Proc ^ zeros, Proc ^ head_vars, !.Polyhedron, 
        CallPPId0, CallVars),
    list.cons(Edge, !Edges),
    %   
    % Update the call count and maybe stop processing if that was
    % the last call.
    %
    !:Calls = !.Calls + 1,
    ( if    !.Calls > Proc ^ calls
      then  !:Continue = no
      else  true
    ),

    (
        !.Continue = no
    ;
        !.Continue = yes,
        CallPPId0 = real(CallPPId),
        module_info_pred_proc_info(ModuleInfo, CallPPId,  _,
            CallProcInfo),
        proc_info_get_termination2_info(CallProcInfo, CallTermInfo),
        MaybeArgSizeInfo = CallTermInfo ^ success_constrs,
        (   
            MaybeArgSizeInfo = no,
            unexpected(this_file, "Proc with no arg size info in pass 2.")
        ;
            MaybeArgSizeInfo = yes(ArgSizePolyhedron0),
            %
            % If the polyhedron is universe then
            % there's no point running the substitution. 
            %
            ( polyhedron.is_universe(ArgSizePolyhedron0) ->
                true  
            ;
                MaybeCallProc = CallTermInfo ^ abstract_rep, 
                ( if    MaybeCallProc = yes(CallProc0)
                  then  CallProc = CallProc0
                  else  unexpected(this_file, 
                    "No abstract representation for proc.")
                ),
                HeadVars = CallProc ^ head_vars,
                Subst = map.from_corresponding_lists(HeadVars, CallVars),
                Eqns0 = non_false_constraints( ArgSizePolyhedron0),
                Eqns1 = substitute_size_vars(Eqns0, Subst),
                Eqns  = lp_rational.set_vars_to_zero(ZeroVars, Eqns1),
                ArgSizePolyhedron = from_constraints(Eqns),
                polyhedron.intersection(ArgSizePolyhedron, !Polyhedron)
            )
        )
    ).

find_edges_in_goal(_, _, _, _, AbstractGoal, !Calls, !Polyhedron, !Edges,
        !Continue) :-
    AbstractGoal = term_primitive(Primitive, _, _),
    (
        !.Continue = yes,
        polyhedron.intersection(Primitive, !Polyhedron)
    ;
        !.Continue = no
    ).

:- pred find_edges_in_disj(abstract_proc::in, abstract_scc::in, module_info::in,
    int::in, polyhedron::in, abstract_goals::in, int::in, int::out,
    polyhedra::in, polyhedra::out, edges::in, edges::out, bool::in,
    bool::out) is det. 

find_edges_in_disj(_, _, _, _, _, [], !Calls, !DisjConstrs, !Edges, !Continue).
find_edges_in_disj(Proc, AbstractSCC, ModuleInfo, MaxMatrixSize, TopPoly,
        [Disj | Disjs], !Calls, !DisjConstrs, !Edges, !Continue) :- 
    find_edges_in_goal(Proc, AbstractSCC, ModuleInfo, MaxMatrixSize, Disj,
        !Calls, TopPoly, Constrs, !Edges, !Continue),
    list.cons(Constrs, !DisjConstrs),
    %
    % This is why it is important that after numbering the
    % calls in the AR we don't change anything around; otherwise
    % this short-circuiting will not work correctly.
    %
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
    Edge = Edge0 ^ label := polyhedron.intersection(Poly, Edge0 ^ label).

%-----------------------------------------------------------------------------%
%
% Cycle detection.
%

% To find the elementary cycles of this SCC we perform a DFS of the
% call-graph.  Since the call-graph is technically a pseudograph (ie. it
% admits parallel edges and self-loops), we first of all strip out any
% self-loops to make things easier.

:- func find_elementary_cycles_in_scc(list(abstract_ppid), edges) = cycles.

find_elementary_cycles_in_scc(SCC, Edges0) = Cycles :- 
    % 
    % Get any self-loops for each procedure. 
    %
    list.filter_map(direct_call, Edges0, Cycles0, Edges),
    %
    % Find larger elementary cycles in what is left.
    %
    Cycles1 = find_cycles(SCC, Edges),
    Cycles = Cycles0 ++ Cycles1.

    % Succeeds iff Edge is an edge that represents
    % a directly recursive call (a self-loop in 
    % the pseudograph)
    %
:- pred direct_call(edge::in, cycle::out) is semidet.

direct_call(Edge, Cycle) :-
    Edge ^ head = Edge ^ callee,
    Cycle = cycle([Edge ^ head], [Edge]).

:- func find_cycles(list(abstract_ppid), edges) = cycles.

find_cycles(SCC, Edges) = Cycles :- 
    EdgeMap = partition_edges(SCC, Edges),
    Cycles = search_for_cycles(SCC, EdgeMap). 

    % Builds a map from `pred_proc_id' to a list of the edges that begin
    % with the `pred_proc_id.
    %
:- func partition_edges(list(abstract_ppid), edges) = map(abstract_ppid, edges).

partition_edges([], _) = map.init.
partition_edges([ProcId | SCC], Edges0) = Map :- 
    Map0 = partition_edges(SCC, Edges0), 
    Edges = list.filter((pred(Edge::in) is semidet :- ProcId = Edge ^ head),
        Edges0), 
    Map = map.det_insert(Map0, ProcId, Edges).  

:- func search_for_cycles(list(abstract_ppid), map(abstract_ppid, edges)) 
    = cycles.

search_for_cycles([], _) = [].
search_for_cycles([Start | Rest], Map0) = Cycles :-
    Cycles0 = search_for_cycles_2(Start, Map0),
    Map = map.delete(Map0, Start),
    Cycles1 = search_for_cycles(Rest, Map),
    Cycles = Cycles0 ++ Cycles1.

:- func search_for_cycles_2(abstract_ppid, map(abstract_ppid, edges)) = cycles.

search_for_cycles_2(StartPPId, Map) = Cycles :-
    InitialEdges = Map ^ det_elem(StartPPId),
    list.foldl(search_for_cycles_3(StartPPId, [], Map, []), InitialEdges, 
        [], Cycles).

:- pred search_for_cycles_3(abstract_ppid::in, edges::in, 
    map(abstract_ppid, edges)::in, list(abstract_ppid)::in, edge::in, 
    cycles::in, cycles::out) is det.

search_for_cycles_3(Start, SoFar, Map, Visited, Edge, !Cycles) :-
    ( Start = Edge ^ callee ->
        Cycle = cycle([Edge ^ head | Visited], [Edge | SoFar]),
        list.cons(Cycle, !Cycles)
    ;
        ( MoreEdges0 = Map ^ elem(Edge ^ callee) ->
            NotVisited = (pred(E::in) is semidet :-
                not list.member(E ^ head, Visited)
            ),
            MoreEdges = list.filter(NotVisited, MoreEdges0),
            list.foldl(
                search_for_cycles_3(Start, [Edge | SoFar], Map,
                    [Edge ^ head | Visited]),
                MoreEdges, !Cycles)
        ;
            true
        )
    ).

%-----------------------------------------------------------------------------%
%
% Partitioning sets of cycles.
%

:- func partition_cycles(scc, cycles) = list(cycle_set).

partition_cycles([], _) = [].
partition_cycles([Proc | Procs], Cycles0) = CycleSets :-
    list.filter(cycle_contains_proc(Proc), Cycles0, PCycles, Cycles1),
    CycleSets0 = partition_cycles(Procs, Cycles1),
    PEdges = list.map(collapse_cycle(Proc), PCycles),   
    ( if    PEdges    = []
      then  CycleSets = CycleSets0
      else  CycleSets = [c_set(Proc, PEdges) | CycleSets0]
    ).

:- func get_proc_from_abstract_scc(list(abstract_proc), abstract_ppid)
    = abstract_proc.

get_proc_from_abstract_scc([], _) = _ :-
    unexpected(this_file, "Cannot find proc.").
get_proc_from_abstract_scc([ Proc | Procs ], PPId) = 
    ( Proc ^ ppid = PPId ->
        Proc
    ;
        get_proc_from_abstract_scc(Procs, PPId)
    ).

%-----------------------------------------------------------------------------%
% 
% Termination checking.
%

% This approach is very crude.  It just checks that the sum of all
% the non-zero arguments is decreasing around all the elementary cycles.  

:- pred prove_termination(list(cycle_set)::in, abstract_scc::in,
    size_varset::in, constr_termination_info::out) is det.

prove_termination(Cycles, AbstractSCC, Varset, Result) :-
    ( total_sum_decrease(AbstractSCC, Varset, Cycles) ->
        Result = cannot_loop(analysis)
    ; 
        % NOTE: the context here will never be used, in any
        % case it's not clear what it should be.
        Error = term.context_init - cond_not_satisfied,
        Result = can_loop([Error])
    ).

:- pred total_sum_decrease(abstract_scc::in, size_varset::in, 
    list(cycle_set)::in) is semidet.

total_sum_decrease(_, _, []).
total_sum_decrease(AbstractSCC, Varset, [c_set(Start, Loops) | Cycles]):-
    total_sum_decrease_2(AbstractSCC, Varset, Start, Loops),
    total_sum_decrease(AbstractSCC, Varset, Cycles).

:- pred total_sum_decrease_2(abstract_scc::in, size_varset::in,
    abstract_ppid::in, list(edge)::in) is semidet.

total_sum_decrease_2(_, _, _, []).
total_sum_decrease_2(AbstractSCC, Varset, PPId, Loops @ [_|_]) :-
    all [Loop] (
        list.member(Loop, Loops)
    => 
        strict_decrease_around_loop(AbstractSCC, Varset, PPId, Loop)
    ).

    % Succeeds iff there is strict decrease in the sum of *all*
    % the arguments around the given loop.
    %
:- pred strict_decrease_around_loop(abstract_scc::in, size_varset::in,
    abstract_ppid::in, edge::in) is semidet.

strict_decrease_around_loop(AbstractSCC, Varset, PPId, Loop) :-
    ( if    (PPId \= Loop ^ head ; PPId \= Loop ^ callee)
      then  unexpected(this_file, "Badly formed loop.")
      else  true
    ),
    IsActive = (func(Var::in, Input::in) = (Var::out) is semidet :- 
        Input = yes
    ),
    Proc = get_proc_from_abstract_scc(AbstractSCC, PPId),
    Inputs = Proc ^ inputs,
    HeadArgs = list.filter_map_corresponding(IsActive, Loop ^ head_args,
        Inputs),
    CallArgs = list.filter_map_corresponding(IsActive, Loop ^ call_args,
        Inputs),
    Terms = make_coeffs(HeadArgs, -one) ++ make_coeffs(CallArgs, one),
    %
    % NOTE: if you examine the condition it may contain less
    % variables than you expect.  This is because if the same
    % argument occurs in the head and the call they will cancel
    % each other out.
    %
    Condition = constraint(Terms, (=<), -one),
    Label = polyhedron.non_false_constraints(Loop ^ label),
    entailed(Varset, Label, Condition).

:- pred cycle_contains_proc(abstract_ppid::in, cycle::in) is semidet.
 
cycle_contains_proc(PPId, cycle(Nodes, _)) :- list.member(PPId, Nodes).
    
    % XXX Fix this name.
:- func make_coeffs(size_vars, rat) = lp_terms.
 
make_coeffs(Vars, Coeff) = list.map((func(Var) = Var - Coeff), Vars).

%-----------------------------------------------------------------------------%

    % Collapse all the cycles so that they all start with the given
    % procedure and all the edge labels between are conjoined.
    %
:- func collapse_cycles(abstract_ppid, cycles) = edges.

collapse_cycles(Start, Cycles) = list.map(collapse_cycle(Start), Cycles).

:- func collapse_cycle(abstract_ppid, cycle) = edge. 

collapse_cycle(_, cycle(_, [])) = _ :-
    unexpected(this_file, "Trying to collapse a cycle with no edges.").
collapse_cycle(_, cycle(_, [Edge])) = Edge. 
collapse_cycle(StartPPId, cycle(_, Edges0 @ [_,_|_])) = CollapsedCycle :-
    order_nodes(StartPPId, Edges0, Edges),
    ( if    Edges = [StartEdge0 | Rest0] 
      then  StartEdge = StartEdge0, Rest = Rest0
      else  unexpected(this_file, "Error while collapsing cycles.")
    ),
    StartEdge = edge(_, Zeros0, HeadVars, Polyhedron0, _, CallVars0),
    collapse_cycle_2(Rest, Zeros0, Zeros, CallVars0, CallVars, Polyhedron0, 
        Polyhedron),
    CollapsedCycle = edge(StartPPId, Zeros, HeadVars, Polyhedron, 
        StartPPId, CallVars).

:- pred collapse_cycle_2(edges::in, zero_vars::in, zero_vars::out, 
    size_vars::in, size_vars::out, polyhedron::in, polyhedron::out) is det.

collapse_cycle_2([], !Zeros, !CallVars, !Polyhedron). 
collapse_cycle_2([Edge | Edges], !Zeros, !CallVars, !Polyhedron) :-
    set.union(Edge ^ zeros, !Zeros),
    HeadVars = Edge ^ head_args,
    Subst0 = assoc_list.from_corresponding_lists(HeadVars, !.CallVars),
    bimap.set_from_assoc_list(Subst0, bimap.init, Subst),
    %
    % We now need to substitute variables from the call to *this* 
    % predicate for head variables in both the constraints from the 
    % body of the predicate and also into the variables in the 
    % calls to the next predicate.
    %
    % While it might be easier to put equality constraints
    % between the caller's arguments and the callee's head
    % arguments the substitution is in some ways more desirable
    % as we can detect some neutral arguments more directly.
    %
    !:CallVars = list.map(subst_size_var(Subst), Edge ^ call_args),
    %
    % These should be non-false, so throw an exception if they
    % are not.
    %
    Constraints0 = polyhedron.non_false_constraints(!.Polyhedron),
    Constraints1 = polyhedron.non_false_constraints(Edge ^ label),
    Constraints2 = list.map(subst_size_var_eqn(Subst), Constraints1),
    Constraints3 = Constraints0 ++ Constraints2,
    !:Polyhedron = polyhedron.from_constraints(Constraints3),
    collapse_cycle_2(Edges, !Zeros, !CallVars, !Polyhedron).    

:- pred order_nodes(abstract_ppid::in, edges::in, edges::out) is det.

order_nodes(StartPPId, Edges0, [Edge | Edges]) :-
    EdgeMap = build_edge_map(Edges0),
    Edge = EdgeMap ^ det_elem(StartPPId),
    order_nodes_2(StartPPId, Edge ^ callee, EdgeMap, Edges).

:- pred order_nodes_2(abstract_ppid::in, abstract_ppid::in, 
    map(abstract_ppid, edge)::in, edges::out) is det.

order_nodes_2(StartPPId, CurrPPId, Map, Edges) :-
    ( if    StartPPId = CurrPPId
      then  Edges = []
      else  
            Edge = Map ^ det_elem(CurrPPId),
            order_nodes_2(StartPPId, Edge ^ callee, Map, Edges0),
            Edges = [Edge | Edges0]
    ).

:- func build_edge_map(edges) = map(abstract_ppid, edge).

build_edge_map([]) = map.init.
build_edge_map([Edge | Edges]) = 
    map.det_insert(build_edge_map(Edges), Edge ^ head, Edge).

:- func subst_size_var_eqn(bimap(size_var, size_var), constraint)
    = constraint.

subst_size_var_eqn(Map, Eqn0) = Eqn :-
    constraint(Eqn0, Coeffs0, Operator, Constant),
    Coeffs = list.map(subst_size_var_coeff(Map), Coeffs0),
    Eqn = constraint(Coeffs, Operator, Constant).

:- func subst_size_var_coeff(bimap(size_var, size_var), lp_term) = lp_term. 

subst_size_var_coeff(Map, Var0 - Coeff) = Var - Coeff :-
    Var = subst_size_var(Map, Var0).

:- func subst_size_var(bimap(size_var, size_var), size_var) = size_var.

subst_size_var(Map, Old) = (if bimap.search(Map, Old, New) then New else Old).  

%-----------------------------------------------------------------------------%
%
% Predicates for printing out debugging traces.
%

:- pred write_cycles(cycles::in, module_info::in, size_varset::in, 
    io::di, io::uo) is det.

write_cycles([], _, _, !IO).
write_cycles([Cycle | Cycles], ModuleInfo, Varset, !IO) :- 
    io.write_string("Cycle in SCC:\n", !IO),
    write_cycle(Cycle ^ nodes, ModuleInfo, !IO), 
    io.write_list(Cycle ^ edges, "\n", write_edge(ModuleInfo, Varset), !IO),
    io.nl(!IO),
    write_cycles(Cycles, ModuleInfo, Varset, !IO).

:- pred write_cycle(list(abstract_ppid)::in, module_info::in, io::di, io::uo) 
    is det.

write_cycle([], _, !IO).
write_cycle([ Proc | Procs ], ModuleInfo, !IO) :- 
    io.write_string("\t- ", !IO),
    Proc = real(proc(PredId, ProcId)),
    hlds_out.write_pred_proc_id(ModuleInfo, PredId, ProcId, !IO),
    io.nl(!IO),
    write_cycle(Procs, ModuleInfo, !IO).

:- pred write_edge(module_info::in, size_varset::in, edge::in,
    io::di, io::uo) is det.

write_edge(ModuleInfo, Varset, Edge, !IO) :- 
    io.write_string("Edge is:\n\tHead: ", !IO),
    Edge ^ head = real(proc(PredId, ProcId)),
    hlds_out.write_pred_proc_id(ModuleInfo, PredId, ProcId, !IO),
    io.write_string(" : ", !IO),
    write_size_vars(Varset, Edge ^ head_args, !IO),
    io.write_string(" :- \n", !IO),
    io.write_string("\tConstraints are:  \n", !IO),
    write_polyhedron(Edge ^ label, Varset, !IO),
    io.write_string("\n\tCall is:  ", !IO),
    Edge ^ callee = real(proc(CallPredId, CallProcId)),
    hlds_out.write_pred_proc_id(ModuleInfo, CallPredId, CallProcId, !IO),
    io.write_string(" : ", !IO),
    write_size_vars(Varset, Edge ^ call_args, !IO),
    io.write_string(" :- \n", !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "term_constr_pass2.m".

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.term_constr_pass2.
%-----------------------------------------------------------------------------%
