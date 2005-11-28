%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002, 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: hhf.m.
% Author: dmo.

% Convert superhomogeneous form to hyperhomogeneous form and output an
% inst graph for the predicate based on this transformation.
%
% Hyperhomogeneous form and the transformation are documented in
% David Overton's PhD thesis.

%-----------------------------------------------------------------------------%

:- module hlds__hhf.
:- interface.

:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.
:- import_module hlds.inst_graph.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

:- pred process_pred(bool::in, pred_id::in, module_info::in,
    module_info::out, io::di, io::uo) is det.

:- pred process_clauses_info(bool::in, module_info::in,
    clauses_info::in, clauses_info::out, inst_graph::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

process_pred(Simple, PredId, !ModuleInfo, !IO) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    ( pred_info_is_imported(PredInfo0) ->
        % AAA
        % PredInfo2 = PredInfo0
        pred_info_clauses_info(PredInfo0, ClausesInfo),
        clauses_info_headvars(ClausesInfo, HeadVars),
        clauses_info_varset(ClausesInfo, VarSet),
        some [!IG] (
            !:IG = PredInfo0 ^ inst_graph_info,
            inst_graph__init(HeadVars, InstGraph),
            !:IG = !.IG ^ implementation_inst_graph := InstGraph,
            !:IG = !.IG ^ interface_inst_graph := InstGraph,
            !:IG = !.IG ^ interface_vars := HeadVars,
            !:IG = !.IG ^ interface_varset := VarSet,
            PredInfo2 = PredInfo0 ^ inst_graph_info := !.IG
        )
    ;
        write_pred_progress_message("% Calculating HHF and inst graph for ",
            PredId, !.ModuleInfo, !IO),

        pred_info_clauses_info(PredInfo0, ClausesInfo0),
        process_clauses_info(Simple, !.ModuleInfo, ClausesInfo0,
            ClausesInfo, ImplementationInstGraph),
        pred_info_set_clauses_info(ClausesInfo, PredInfo0, PredInfo1),
        some [!IG] (
            !:IG = PredInfo1 ^ inst_graph_info,
            !:IG = !.IG ^ implementation_inst_graph := ImplementationInstGraph,

            % AAA only for non-imported preds with no mode decls.
            clauses_info_headvars(ClausesInfo, HeadVars),
            clauses_info_varset(ClausesInfo, VarSet),
            !:IG = !.IG ^ interface_inst_graph := ImplementationInstGraph,
            solutions(
                (pred(V::out) is nondet :-
                    list__member(V0, HeadVars),
                    inst_graph__reachable(ImplementationInstGraph,
                    V0, V)
                ), InterfaceVars),
            !:IG = !.IG ^ interface_vars := InterfaceVars,
            !:IG = !.IG ^ interface_varset := VarSet,

            PredInfo2 = PredInfo1 ^ inst_graph_info := !.IG
        )
    ),

%   pred_info_get_markers(PredInfo2, Markers),
%   ( check_marker(Markers, infer_modes) ->
%       % No mode declarations.  If not imported, use implementation
%       % inst_graph.
%       % ...
%   ;
%       pred_info_clauses_info(PredInfo2, ClausesInfo2),
%       clauses_info_headvars(ClausesInfo2, HeadVars),
%       clauses_info_varset(ClausesInfo2, VarSet),
%       inst_graph__init(HeadVars, InterfaceInstGraph),
%       InstGraphInfo0 = ( (PredInfo2 ^ inst_graph_info)
%           ^ interface_inst_graph := InterfaceInstGraph )
%           ^ interface_varset := VarSet,
%       map__foldl(process_proc(ModuleInfo0, HeadVars),
%           Procedures, InstGraphInfo0, InstGraphInfo1),
% 
%       % Calculate interface vars.
%       solutions((pred(V::out) is nondet :-
%               list__member(V0, HeadVars),
%               inst_graph__reachable(InstGraph, V0, V)
%           ), InterfaceVars),
%       InstGraphInfo = InstGraphInfo1 ^ interface_vars :=
%           InterfaceVars,
% 
%       PredInfo = PredInfo2 ^ inst_graph_info := InstGraphInfo
%   ),

    PredInfo = PredInfo2, % AAA
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

process_clauses_info(Simple, ModuleInfo, !ClausesInfo, InstGraph) :-
    clauses_info_varset(!.ClausesInfo, VarSet0),
    clauses_info_vartypes(!.ClausesInfo, VarTypes0),
    inst_graph__init(VarTypes0 ^ keys, InstGraph0),
    Info0 = hhf_info(InstGraph0, VarSet0, VarTypes0),

    clauses_info_headvars(!.ClausesInfo, HeadVars),
    clauses_info_clauses(Clauses0, !ClausesInfo),

    (
    %   % For simple mode checking we do not give the inst_graph any
    %   % structure.
    %   Simple = yes,
    %   Clauses = Clauses0,
    %   Info1 = Info0
    %;
    %   Simple = no,
        list__map_foldl(process_clause(HeadVars),
            Clauses0, Clauses, Info0, Info1)
    ),

    clauses_info_set_clauses(Clauses, !ClausesInfo),

    complete_inst_graph(ModuleInfo, Info1, Info),
    % XXX Comment out the above line for incomplete, quick checking.
    % Info = Info1,

    Info = hhf_info(InstGraph1, VarSet, VarTypes),
    (
        Simple = yes,
        inst_graph__init(VarTypes ^ keys, InstGraph)
    ;
        Simple = no,
        InstGraph = InstGraph1
    ),

    % XXX do we need this (it slows things down a lot (i.e. uses 50%
    % of the runtime).
    % varset__vars(VarSet1, Vars1),
    % varset__ensure_unique_names(Vars1, "_", VarSet1, VarSet),

    clauses_info_set_varset(VarSet, !ClausesInfo),
    clauses_info_set_vartypes(VarTypes, !ClausesInfo).

:- type hhf_info
    --->    hhf_info(
                inst_graph  :: inst_graph,
                varset      :: prog_varset,
                vartypes    :: vartypes
            ).

:- pred process_clause(list(prog_var)::in, clause::in, clause::out,
    hhf_info::in, hhf_info::out) is det.

process_clause(_HeadVars, clause(ProcIds, Goal0, Lang, Context),
        clause(ProcIds, Goal, Lang, Context), !HI) :-
    Goal0 = _ - GoalInfo0,
    goal_info_get_nonlocals(GoalInfo0, NonLocals),

    process_goal(NonLocals, Goal0, Goal, !HI).
% XXX We probably need to requantify, but it stuffs up the inst_graph to do
% that.
%   VarSet1 = !.HI ^ varset,
%   VarTypes1 = !.HI ^ vartypes,
%   implicitly_quantify_clause_body(HeadVars, Goal1, VarSet1, VarTypes1,
%       Goal, VarSet, VarTypes, _Warnings),
%   !:HI = !.HI varset := VarSet,
%   !:HI = !.HI vartypes := VarTypes.

:- pred process_goal(set(prog_var)::in, hlds_goal::in, hlds_goal::out,
    hhf_info::in, hhf_info::out) is det.

process_goal(NonLocals, GoalExpr0 - GoalInfo, GoalExpr - GoalInfo, !HI) :-
    process_goal_expr(NonLocals, GoalInfo, GoalExpr0, GoalExpr, !HI).

:- pred goal_use_own_nonlocals(hlds_goal::in, hlds_goal::out,
    hhf_info::in, hhf_info::out) is det.

goal_use_own_nonlocals(GoalExpr0 - GoalInfo, GoalExpr - GoalInfo, !HI) :-
    goal_info_get_nonlocals(GoalInfo, NonLocals),
    process_goal_expr(NonLocals, GoalInfo, GoalExpr0, GoalExpr, !HI).

:- pred process_goal_expr(set(prog_var)::in, hlds_goal_info::in,
    hlds_goal_expr::in, hlds_goal_expr::out, hhf_info::in, hhf_info::out)
    is det.

process_goal_expr(NonLocals, GoalInfo, GoalExpr0, GoalExpr, !HI) :-
    (
        GoalExpr0 = unify(Var, RHS, Mode, Unif, Context),
        process_unify(RHS, NonLocals, GoalInfo, Var, Mode, Unif, Context,
            GoalExpr, !HI)
    ;
        GoalExpr0 = call(_, _, _, _, _, _),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = generic_call(_, _, _, _),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = foreign_proc(_, _, _, _, _, _),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = conj(Goals0),
        list__map_foldl(process_goal(NonLocals), Goals0, Goals1, !HI),
        flatten_conj(Goals1, Goals),
        GoalExpr = conj(Goals)
    ;
        GoalExpr0 = par_conj(Goals0),
        list__map_foldl(process_goal(NonLocals), Goals0, Goals, !HI),
        GoalExpr = par_conj(Goals)
    ;
        GoalExpr0 = disj(Goals0),
        list__map_foldl(goal_use_own_nonlocals, Goals0, Goals, !HI),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(_, _, _),
        unexpected(this_file, "hhf_goal_expr: found switch")
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        process_goal(NonLocals, SubGoal0, SubGoal, !HI),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = not(SubGoal0),
        process_goal(NonLocals, SubGoal0, SubGoal, !HI),
        GoalExpr = not(SubGoal)
    ;
        GoalExpr0 = if_then_else(Vs, Cond0, Then0, Else0),
        process_goal(NonLocals, Cond0, Cond, !HI),
        Then0 = ThenExpr0 - ThenInfo,
        goal_info_get_nonlocals(ThenInfo, ThenNonLocals),
        process_goal_expr(ThenNonLocals, ThenInfo, ThenExpr0, ThenExpr, !HI),
        Then = ThenExpr - ThenInfo,
        Else0 = ElseExpr0 - ElseInfo,
        goal_info_get_nonlocals(ElseInfo, ElseNonLocals),
        process_goal_expr(ElseNonLocals, ElseInfo, ElseExpr0, ElseExpr, !HI),
        Else = ElseExpr - ElseInfo,
        GoalExpr = if_then_else(Vs, Cond, Then, Else)
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file, "hhf_goal_expr: found shorthand")
    ).

:- pred process_unify(unify_rhs::in, set(prog_var)::in, hlds_goal_info::in,
    prog_var::in, unify_mode::in, unification::in, unify_context::in,
    hlds_goal_expr::out, hhf_info::in, hhf_info::out) is det.

process_unify(var(Y), _, _, X, Mode, Unif, Context, GoalExpr, !HI) :-
    GoalExpr = unify(X, var(Y), Mode, Unif, Context).
process_unify(lambda_goal(A,B,C,D,E,F,G,H,LambdaGoal0), NonLocals, _, X, Mode,
        Unif, Context, GoalExpr, !HI) :-
    process_goal(NonLocals, LambdaGoal0, LambdaGoal, !HI),
    GoalExpr = unify(X, lambda_goal(A,B,C,D,E,F,G,H,LambdaGoal), Mode,
        Unif, Context).
process_unify(functor(ConsId0, IsExistConstruct, ArgsA), NonLocals, GoalInfo0,
        X, Mode, Unif, Context, GoalExpr, !HI) :-
    TypeOfX = !.HI ^ vartypes ^ det_elem(X),
    qualify_cons_id(TypeOfX, ArgsA, ConsId0, _, ConsId),
    InstGraph0 = !.HI ^ inst_graph,
    map__lookup(InstGraph0, X, node(Functors0, MaybeParent)),
    ( map__search(Functors0, ConsId, ArgsB) ->
        make_unifications(ArgsA, ArgsB, GoalInfo0, Mode, Unif, Context,
            Unifications),
        Args = ArgsB
    ;
        add_unifications(ArgsA, NonLocals, GoalInfo0, Mode, Unif, Context,
            Args, Unifications, !HI),
        InstGraph1 = !.HI ^ inst_graph,
        map__det_insert(Functors0, ConsId, Args, Functors),
        map__det_update(InstGraph1, X, node(Functors, MaybeParent),
            InstGraph2),
        list__foldl(inst_graph__set_parent(X), Args, InstGraph2, InstGraph),
        !:HI = !.HI ^ inst_graph := InstGraph
    ),
    goal_info_get_nonlocals(GoalInfo0, GINonlocals0),
    GINonlocals = GINonlocals0 `set__union` list_to_set(Args),
    goal_info_set_nonlocals(GINonlocals, GoalInfo0, GoalInfo),
    UnifyGoal = unify(X, functor(ConsId, IsExistConstruct, Args),
        Mode, Unif, Context) - GoalInfo,
    GoalExpr = conj([UnifyGoal | Unifications]).

:- pred make_unifications(list(prog_var)::in, list(prog_var)::in,
    hlds_goal_info::in, unify_mode::in, unification::in, unify_context::in,
    hlds_goals::out) is det.

make_unifications([], [], _, _, _, _, []).
make_unifications([_ | _], [], _, _, _, _, _) :-
    unexpected(this_file, "hhf_make_unifications: length mismatch (1)").
make_unifications([], [_ | _], _, _, _, _, _) :-
    unexpected(this_file, "hhf_make_unifications: length mismatch (2)").
make_unifications([A | As], [B | Bs], GI0, M, U, C,
        [unify(A, var(B), M, U, C) - GI | Us]) :-
    goal_info_get_nonlocals(GI0, GINonlocals0),
    GINonlocals = GINonlocals0 `set__insert` A `set__insert` B,
    goal_info_set_nonlocals(GINonlocals, GI0, GI),
    make_unifications(As, Bs, GI0, M, U, C, Us).

:- pred add_unifications(list(prog_var)::in, set(prog_var)::in,
    hlds_goal_info::in, unify_mode::in, unification::in, unify_context::in,
    list(prog_var)::out, hlds_goals::out, hhf_info::in, hhf_info::out) is det.

add_unifications([], _, _, _, _, _, [], [], !HI).
add_unifications([A | As], NonLocals, GI0, M, U, C, [V | Vs], Goals, !HI) :-
    add_unifications(As, NonLocals, GI0, M, U, C, Vs, Goals0, !HI),
    InstGraph0 = !.HI ^ inst_graph,
    (
        ( 
            map__lookup(InstGraph0, A, Node),
            Node = node(_, parent(_))
        ;
            A `set__member` NonLocals
        )
    ->
        VarSet0 = !.HI ^ varset,
        VarTypes0 = !.HI ^ vartypes,
        varset__new_var(VarSet0, V, VarSet),
        map__lookup(VarTypes0, A, Type),
        map__det_insert(VarTypes0, V, Type, VarTypes),
        map__init(Empty),
        map__det_insert(InstGraph0, V, node(Empty, top_level), InstGraph),
        !:HI = !.HI ^ varset := VarSet,
        !:HI = !.HI ^ vartypes := VarTypes,
        !:HI = !.HI ^ inst_graph := InstGraph,
        goal_info_get_nonlocals(GI0, GINonlocals0),
        GINonlocals = GINonlocals0 `set__insert` V,
        goal_info_set_nonlocals(GINonlocals, GI0, GI),
        Goals = [unify(A, var(V), M, U, C) - GI | Goals0]
    ;
        V = A,
        Goals = Goals0
    ).

:- pred flatten_conj(hlds_goals::in, hlds_goals::out) is det.

flatten_conj([], []).
flatten_conj([Goal | Goals0], Goals) :-
    flatten_conj(Goals0, Goals1),
    ( Goal = conj(SubGoals) - _ ->
        list__append(SubGoals, Goals1, Goals)
    ;
        Goals = [Goal | Goals1]
    ).

:- pred complete_inst_graph(module_info::in, hhf_info::in, hhf_info::out)
    is det.

complete_inst_graph(ModuleInfo, !HI) :-
    InstGraph0 = !.HI ^ inst_graph,
    map__keys(InstGraph0, Vars),
    list__foldl(complete_inst_graph_node(ModuleInfo, Vars), Vars, !HI).

:- pred complete_inst_graph_node(module_info::in, list(prog_var)::in,
    prog_var::in, hhf_info::in, hhf_info::out) is det.

complete_inst_graph_node(ModuleInfo, BaseVars, Var, !HI) :-
    VarTypes0 = !.HI ^ vartypes,
    (
        map__search(VarTypes0, Var, Type),
        type_constructors(Type, ModuleInfo, Constructors),
        type_to_ctor_and_args(Type, TypeId, _)
    ->
        list__foldl(maybe_add_cons_id(Var, ModuleInfo, BaseVars, TypeId),
            Constructors, !HI)
    ;
        true
    ).

:- pred maybe_add_cons_id(prog_var::in, module_info::in, list(prog_var)::in,
    type_ctor::in, constructor::in, hhf_info::in, hhf_info::out) is det.

maybe_add_cons_id(Var, ModuleInfo, BaseVars, TypeId, Ctor, !HI) :-
    Ctor = ctor(_, _, Name, Args),
    ConsId = make_cons_id(Name, Args, TypeId),
    map__lookup(!.HI ^ inst_graph, Var, node(Functors0, MaybeParent)),
    ( map__contains(Functors0, ConsId) ->
        true
    ;
        list__map_foldl(add_cons_id(Var, ModuleInfo, BaseVars),
            Args, NewVars, !HI),
        map__det_insert(Functors0, ConsId, NewVars, Functors),
        !:HI = !.HI ^ inst_graph :=
            map__det_update(!.HI ^ inst_graph, Var,
                node(Functors, MaybeParent))
    ).

:- pred add_cons_id(prog_var::in, module_info::in, list(prog_var)::in,
    constructor_arg::in, prog_var::out, hhf_info::in, hhf_info::out)
    is det.

add_cons_id(Var, ModuleInfo, BaseVars, Arg, NewVar, !HI) :-
    Arg = _ - ArgType,
    !.HI = hhf_info(InstGraph0, VarSet0, VarTypes0),
    (
        find_var_with_type(Var, ArgType, InstGraph0, VarTypes0,
            BaseVars, NewVar0)
    ->
        NewVar = NewVar0
    ;
        varset__new_var(VarSet0, NewVar, VarSet),
        map__det_insert(VarTypes0, NewVar, ArgType, VarTypes),
        map__init(Empty),
        map__det_insert(InstGraph0, NewVar, node(Empty, parent(Var)),
            InstGraph),
        !:HI = hhf_info(InstGraph, VarSet, VarTypes),
        complete_inst_graph_node(ModuleInfo, BaseVars, NewVar, !HI)
    ).

:- pred find_var_with_type(prog_var::in, mer_type::in, inst_graph::in,
    vartypes::in, list(prog_var)::in, prog_var::out) is semidet.

find_var_with_type(Var0, Type, InstGraph, VarTypes, BaseVars, Var) :-
    (
        map__search(VarTypes, Var0, Type0),
        same_type(Type0, Type)
    ->
        Var = Var0
    ;
        map__lookup(InstGraph, Var0, node(_, parent(Var1))),
        \+ Var1 `list__member` BaseVars,
        find_var_with_type(Var1, Type, InstGraph, VarTypes, BaseVars, Var)
    ).

:- pred same_type(mer_type::in, mer_type::in) is semidet.

same_type(A0, B0) :-
    A = strip_kind_annotation(A0),
    B = strip_kind_annotation(B0),
    same_type_2(A, B).

:- pred same_type_2(mer_type::in, mer_type::in) is semidet.

same_type_2(variable(_, _), variable(_, _)).
same_type_2(defined(Name, ArgsA, _), defined(Name, ArgsB, _)) :-
    same_type_list(ArgsA, ArgsB).
same_type_2(builtin(BuiltinType), builtin(BuiltinType)).
same_type_2(higher_order(ArgsA, no, Purity, EvalMethod),
        higher_order(ArgsB, no, Purity, EvalMethod)) :-
    same_type_list(ArgsA, ArgsB).
same_type_2(higher_order(ArgsA, yes(RetA), Purity, EvalMethod),
        higher_order(ArgsB, yes(RetB), Purity, EvalMethod)) :-
    same_type_list(ArgsA, ArgsB),
    same_type(RetA, RetB).
same_type_2(tuple(ArgsA, _), tuple(ArgsB, _)) :-
    same_type_list(ArgsA, ArgsB).
same_type_2(apply_n(_, ArgsA, _), apply_n(_, ArgsB, _)) :-
    same_type_list(ArgsA, ArgsB).

:- pred same_type_list(list(mer_type)::in, list(mer_type)::in) is semidet.

same_type_list([], []).
same_type_list([A | As], [B | Bs]) :-
    same_type(A, B),
    same_type_list(As, Bs).

%------------------------------------------------------------------------%

%   % Add the information from the procedure's mode declaration
%   % to the inst_graph.
% :- pred process_proc(module_info::in, list(prog_var)::in, proc_id::in,
%   proc_info::in, inst_graph::out, prog_varset::out) is det.
% 
% process_proc(ModuleInfo, HeadVars, _ProcId, ProcInfo, Info0, Info) :-
%   proc_info_argmodes(ProcInfo, ArgModes),
% 
%   mode_list_get_initial_insts(ArgModes, ModuleInfo, InstsI),
%   assoc_list__from_corresponding_lists(HeadVars, InstsI, VarInstsI),
%   list__foldl(process_arg(ModuleInfo), VarInstsI, Info0, Info),
% 
%   mode_list_get_final_insts(ArgModes, ModuleInfo, InstsF),
%   assoc_list__from_corresponding_lists(HeadVars, InstsF, VarInstsF),
%   list__foldl(process_arg(ModuleInfo), VarInstsF, Info0, Info).
% 
% :- pred process_arg(module_info::in, pair(prog_var, inst)::in,
%       inst_graph_info::in, inst_graph_info::out) is det.
% 
% process_arg(ModuleInfo, Var - Inst, Info0, Info) :-
%   map__init(Seen0),
%   process_arg_inst(ModuleInfo, Var, Seen0, Inst, Info0, Info).
% 
% :- pred process_arg_inst(module_info::in, prog_var::in,
%       map(inst_name, prog_var)::in, inst::in, inst_graph_info::in,
%       inst_graph_info::out) is det.
% 
% process_arg_inst(ModuleInfo, Var, Seen0, Inst0, Info0, Info) :-
%   ( Inst0 = defined_inst(InstName) ->
%       map__det_insert(Seen0, InstName, Var, Seen),
%       inst_lookup(ModuleInfo, InstName, Inst),
%       process_arg_inst(Inst, ModuleInfo, Var, Seen, Info0, Info)
%   ; Inst0 = bound(_, BoundInsts) ->
%       list__foldl(process_bound_inst(ModuleInfo, Var, Seen0),
%           BoundInts, Info0, Info)
%   ;
%       Info = Info0
%   ).
% 
% :- pred process_bound_inst(module_info::in, prog_var::in,
%       map(inst_name, prog_var)::in, bound_inst::in,
%       inst_graph_info::in, inst_graph_info::out) is det.

%------------------------------------------------------------------------%

:- func this_file = string.

this_file = "hhf.m".

%------------------------------------------------------------------------%
:- end_module hhf.
%------------------------------------------------------------------------%
